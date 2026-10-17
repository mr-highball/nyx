{ nyx
  Copyright (c) 2020 mr-highball

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
}

unit nyx.editing.history;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.editing;

type
  { Invalid/stale editing transactions refuse before changing text or either
    stack. Ordinary physical observation may evict old entries within its budget. }
  ENyxTextHistory = class(Exception);

  { Detached immutable physical text/selection. Undefined selection is preserved
    as unavailable, never guessed. Text retains exact Unicode and line endings. }
  TNyxTextEditorFrame = record
  private
    FDefined: Boolean;
    FText: TNyxText;
    FSelection: TNyxTextSelection;
    FScalars: Integer;
    FBytes: Integer;
  public
    procedure Validate;
    function SameValue(const AOther: TNyxTextEditorFrame): Boolean;
    property Text: TNyxText read FText;
    property Selection: TNyxTextSelection read FSelection;
    property Scalars: Integer read FScalars;
    property RetainedBytes: Integer read FBytes;
  end;

  { Copied runtime retention policy. Both stacks count UTF-8 text plus 32 bytes
    per frame, excluding Current; this is accounting, not heap measurement.
    Defaults retain 128 entries/16 MiB; zero entries
    disables retention. No editor/widget/session or timing policy is retained. }
  TNyxTextHistoryLimits = record
  private
    FDefined: Boolean;
    FEntries: Integer;
    FBytes: Integer;
  public
    class function Defaults: TNyxTextHistoryLimits; static;
    function UpToEntries(AValue: Integer): TNyxTextHistoryLimits;
    function UpToBytes(AValue: Integer): TNyxTextHistoryLimits;
    procedure Validate;
    property MaximumEntries: Integer read FEntries;
    property MaximumBytes: Integer read FBytes;
  end;

  { A UI-thread-owned independent editing timeline. Observe admits actual physical
    edits; composition drafts update Current but commit one history entry at end.
    Selection-only observations preserve Redo. Replace requires an exact current
    frame and room for its immediate Undo/Redo states; it never silently loses
    that command. No platform default, control, application or document is owned.
    Hosts render Current and publish their ordinary draft notification separately. }
  INyxTextEditHistory = interface(IInterface)
    ['{F45DC5F0-1679-4905-BBB9-FF012CD6A6C1}']
    function Current: TNyxTextEditorFrame;
    function Composing: Boolean;
    function CanUndo: Boolean;
    function CanRedo: Boolean;
    function UndoCount: Integer;
    function RedoCount: Integer;
    function RetainedBytes: Integer;
    { Monotonic runtime identity includes selection/composition and explicit Reset.
      Captured async proposals cannot revive after an edit/Undo back to equal text. }
    function Generation: Integer;
    { Indicates eviction since Reset. Oversized physical edits remain real input:
      retain their new frame and clear any unretainable immediate history state. }
    function Truncated: Boolean;
    procedure Reset(const AFrame: TNyxTextEditorFrame);
    procedure Observe(const AFrame: TNyxTextEditorFrame; AComposing: Boolean = False);
    { Scalar replacement inside the exact current text. All validation/allocation
      precedes stack publication; composition, stale frames and budgets refuse.
      Equal text only changes selection and preserves history/Redo. }
    procedure Replace(const AExpected: TNyxTextEditorFrame; AStart: Integer;
      const AExpectedText, AInsertion: TNyxText);
    { Return False for an empty stack; composing refuses without ending the IME.
      Restored frames include the exact prior text/selection. }
    function Undo: Boolean;
    function Redo: Boolean;
    { Inspect the next target without changing stacks, so an owning source model
      can stage its own byte/membership admission before the synchronous command. }
    function UndoFrame(out AFrame: TNyxTextEditorFrame): Boolean;
    function RedoFrame(out AFrame: TNyxTextEditorFrame): Boolean;
  end;

function NyxTextEditorFrame(const AText: TNyxText;
  const ASelection: TNyxTextSelection): TNyxTextEditorFrame;
function NewNyxTextEditHistory(const AInitial: TNyxTextEditorFrame): INyxTextEditHistory; overload;
function NewNyxTextEditHistory(const AInitial: TNyxTextEditorFrame;
  const ALimits: TNyxTextHistoryLimits): INyxTextEditHistory; overload;

implementation

uses nyx.bytes;

type
  { Managed value frames contain copied text, never COM interfaces in records.
    Each staged array is detached before mutation on both compilers. }
  TTextFrames = array of TNyxTextEditorFrame;

  { Independent value timeline; arrays never escape. The composition base remains
    separate from live intermediate text, so cancellation back to it adds no entry. }
  TTextHistory = class(TInterfacedObject, INyxTextEditHistory)
  private
    FLimits: TNyxTextHistoryLimits;
    FCurrent: TNyxTextEditorFrame;
    FCompositionBase: TNyxTextEditorFrame;
    FComposing: Boolean;
    FUndo: TTextFrames;
    FRedo: TTextFrames;
    FTruncated: Boolean;
    FGeneration: Integer;
    procedure RequireGeneration;
    function Trim(var AUndo, ARedo: TTextFrames): Boolean;
    function Push(var AFrames: TTextFrames; const AFrame: TNyxTextEditorFrame): Boolean;
    procedure Commit(const ABefore, AAfter: TNyxTextEditorFrame);
  public
    function Current: TNyxTextEditorFrame;
    function Composing: Boolean;
    function CanUndo: Boolean;
    function CanRedo: Boolean;
    function UndoCount: Integer;
    function RedoCount: Integer;
    function RetainedBytes: Integer;
    function Generation: Integer;
    function Truncated: Boolean;
    procedure Reset(const AFrame: TNyxTextEditorFrame);
    procedure Observe(const AFrame: TNyxTextEditorFrame; AComposing: Boolean = False);
    procedure Replace(const AExpected: TNyxTextEditorFrame; AStart: Integer;
      const AExpectedText, AInsertion: TNyxText);
    function Undo: Boolean;
    function Redo: Boolean;
    function UndoFrame(out AFrame: TNyxTextEditorFrame): Boolean;
    function RedoFrame(out AFrame: TNyxTextEditorFrame): Boolean;
  end;

function NyxTextEditorFrame(const AText: TNyxText;
  const ASelection: TNyxTextSelection): TNyxTextEditorFrame;
var
  LBytes: Integer;
begin
  Result.FScalars := NyxTextScalarCount(AText);
  LBytes := NyxUTF8ByteCount(AText);

  if LBytes > High(Integer) - 32 then
  begin
    raise ENyxTextHistory.Create('Text frame exceeds its byte accounting range');
  end;
  Result.FText := AText;
  Result.FSelection := ASelection;
  Result.FBytes := LBytes + 32;
  Result.FDefined := True;
  Result.Validate;
end;

procedure TNyxTextEditorFrame.Validate;
begin

  if not FDefined or (FSelection.Defined and (FSelection.Finish > FScalars)) then
  begin
    raise ENyxTextHistory.Create('Text history needs a valid text/selection frame');
  end;
end;

function TNyxTextEditorFrame.SameValue(const AOther: TNyxTextEditorFrame): Boolean;
begin
  Validate;
  AOther.Validate;
  Result := (FText = AOther.FText) and FSelection.SameRange(AOther.FSelection);
end;

class function TNyxTextHistoryLimits.Defaults: TNyxTextHistoryLimits;
begin
  Result.FDefined := True;
  Result.FEntries := 128;
  Result.FBytes := 16 * 1024 * 1024;
end;

procedure TNyxTextHistoryLimits.Validate;
begin

  if not FDefined or (FEntries < 0) or (FEntries > 4096) or
    (FBytes < 1) or (FBytes > 256 * 1024 * 1024) then
  begin
    raise ENyxTextHistory.Create('Text history limits require 0..4096 entries and 1..268435456 bytes');
  end;
end;

function TNyxTextHistoryLimits.UpToEntries(AValue: Integer): TNyxTextHistoryLimits;
begin
  Result := Self;
  Result.FEntries := AValue;
  Result.Validate;
end;

function TNyxTextHistoryLimits.UpToBytes(AValue: Integer): TNyxTextHistoryLimits;
begin
  Result := Self;
  Result.FBytes := AValue;
  Result.Validate;
end;

function TTextHistory.Current: TNyxTextEditorFrame;
begin
  Result := FCurrent;
end;

function TTextHistory.Composing: Boolean;
begin
  Result := FComposing;
end;

function TTextHistory.CanUndo: Boolean;
begin
  Result := not FComposing and (Length(FUndo) > 0);
end;

function TTextHistory.CanRedo: Boolean;
begin
  Result := not FComposing and (Length(FRedo) > 0);
end;

function TTextHistory.UndoCount: Integer;
begin
  Result := Length(FUndo);
end;

function TTextHistory.RedoCount: Integer;
begin
  Result := Length(FRedo);
end;

function TTextHistory.RetainedBytes: Integer;
var
  LAt: Integer;
begin
  Result := 0;
  for LAt := 0 to High(FUndo) do
  begin
    Inc(Result, FUndo[LAt].RetainedBytes);
  end;
  for LAt := 0 to High(FRedo) do
  begin
    Inc(Result, FRedo[LAt].RetainedBytes);
  end;
end;

function TTextHistory.Truncated: Boolean;
begin
  Result := FTruncated;
end;

procedure TTextHistory.RequireGeneration;
begin

  if FGeneration = High(Integer) then
  begin
    raise ENyxTextHistory.Create('Text history generation exhausted');
  end;
end;

function TTextHistory.Generation: Integer;
begin
  Result := FGeneration;
end;

procedure TTextHistory.Reset(const AFrame: TNyxTextEditorFrame);
begin
  AFrame.Validate;
  RequireGeneration;
  Inc(FGeneration);
  FCurrent := AFrame;
  FCompositionBase := Default(TNyxTextEditorFrame);
  FComposing := False;
  FUndo := nil;
  FRedo := nil;
  FTruncated := False;
end;

function TTextHistory.Push(var AFrames: TTextFrames;
  const AFrame: TNyxTextEditorFrame): Boolean;
begin
  Result := (FLimits.MaximumEntries > 0) and
    (AFrame.RetainedBytes <= FLimits.MaximumBytes);

  if not Result then
  begin
    AFrames := nil;
    Exit;
  end;
  SetLength(AFrames, Length(AFrames) + 1);
  AFrames[High(AFrames)] := AFrame;
end;

function TTextHistory.Trim(var AUndo, ARedo: TTextFrames): Boolean;
var
  LBytes: Double;
  LCount: Integer;
  LAt: Integer;
  LUndoDrop: Integer;
  LRedoDrop: Integer;
begin
  Result := False;
  { pas2js has no Int64 arithmetic. Hard frame/entry limits keep this integer
    sum below 2^44, so IEEE Double represents every byte exactly on both targets. }
  LBytes := 0;
  LCount := Length(AUndo) + Length(ARedo);
  for LAt := 0 to High(AUndo) do
  begin
    LBytes := LBytes + AUndo[LAt].RetainedBytes;
  end;
  for LAt := 0 to High(ARedo) do
  begin
    LBytes := LBytes + ARedo[LAt].RetainedBytes;
  end;
  LUndoDrop := 0;
  LRedoDrop := 0;
  while (LCount > FLimits.MaximumEntries) or (LBytes > FLimits.MaximumBytes) do
  begin
    Result := True;

    if LUndoDrop < Length(AUndo) then
    begin
      LBytes := LBytes - AUndo[LUndoDrop].RetainedBytes;
      Inc(LUndoDrop);
    end
    else
    begin
      LBytes := LBytes - ARedo[LRedoDrop].RetainedBytes;
      Inc(LRedoDrop);
    end;
    Dec(LCount);
  end;
  { Drop farthest states in one copy per stack; repeated shifting would turn a
    large eviction into quadratic work. The nearest Undo/Redo remain at the end. }
  if LUndoDrop > 0 then
  begin
    AUndo := Copy(AUndo, LUndoDrop, Length(AUndo) - LUndoDrop);
  end;

  if LRedoDrop > 0 then
  begin
    ARedo := Copy(ARedo, LRedoDrop, Length(ARedo) - LRedoDrop);
  end;
end;

procedure TTextHistory.Commit(const ABefore, AAfter: TNyxTextEditorFrame);
var
  LUndo: TTextFrames;
  LRedo: TTextFrames;
  LTruncated: Boolean;
begin

  if ABefore.Text = AAfter.Text then
  begin
    FCurrent := AAfter;
    Exit;
  end;
  LUndo := Copy(FUndo);
  LRedo := nil;
  LTruncated := not Push(LUndo, ABefore);
  LTruncated := Trim(LUndo, LRedo) or LTruncated;
  FUndo := LUndo;
  FRedo := LRedo;
  FCurrent := AAfter;
  FTruncated := FTruncated or LTruncated;
end;

procedure TTextHistory.Observe(const AFrame: TNyxTextEditorFrame; AComposing: Boolean);
begin
  AFrame.Validate;
  RequireGeneration;

  if FCurrent.SameValue(AFrame) and (FComposing = AComposing) then
  begin
    Exit;
  end;

  if AComposing then
  begin

    if not FComposing then
    begin
      FCompositionBase := FCurrent;
    end;
    FCurrent := AFrame;
    FComposing := True;
    Inc(FGeneration);
    Exit;
  end;

  if FComposing then
  begin
    Commit(FCompositionBase, AFrame);
    FCompositionBase := Default(TNyxTextEditorFrame);
    FComposing := False;
  end
  else
  begin
    Commit(FCurrent, AFrame);
  end;
  Inc(FGeneration);
end;

{ Scalar-to-storage conversion rejects offsets outside exact immutable text.
  Both native UTF-8 and browser UTF-16 keep scalar boundaries indivisible. }
function StorageAt(const AText: TNyxText; AOffset: Integer): Integer;
var
  LAt: Integer;
  LScalar: Integer;
begin

  if AOffset < 0 then
  begin
    raise ENyxTextHistory.Create('Replacement offset cannot be negative');
  end;
  Result := 1;
  for LAt := 1 to AOffset do
  begin

    if not NyxNextScalar(AText, Result, LScalar) then
    begin
      raise ENyxTextHistory.Create('Replacement range is outside the current text');
    end;
  end;
end;

procedure TTextHistory.Replace(const AExpected: TNyxTextEditorFrame;
  AStart: Integer; const AExpectedText, AInsertion: TNyxText);
var
  LStart: Integer;
  LEnd: Integer;
  LExpectedScalars: Integer;
  LText: TNyxText;
  LAfter: TNyxTextEditorFrame;
  LCaret: Integer;
begin
  RequireGeneration;

  if FComposing or not FCurrent.SameValue(AExpected) then
  begin
    raise ENyxTextHistory.Create('Editing context changed or composition is active');
  end;
  LExpectedScalars := NyxTextScalarCount(AExpectedText);

  if (AStart < 0) or (AStart > FCurrent.Scalars) or
    (LExpectedScalars > FCurrent.Scalars - AStart) then
  begin
    raise ENyxTextHistory.Create('Replacement range is outside the current text');
  end;
  LStart := StorageAt(FCurrent.Text, AStart);
  LEnd := StorageAt(FCurrent.Text, AStart + LExpectedScalars);

  if Copy(FCurrent.Text, LStart, LEnd - LStart) <> AExpectedText then
  begin
    raise ENyxTextHistory.Create('Replacement expected text no longer matches');
  end;
  LCaret := AStart + NyxTextScalarCount(AInsertion);
  LText := Copy(FCurrent.Text, 1, LStart - 1) + AInsertion +
    Copy(FCurrent.Text, LEnd, MaxInt);
  LAfter := NyxTextEditorFrame(LText, NyxTextSelection(LText, LCaret, LCaret));

  if (LAfter.Text <> FCurrent.Text) and
    ((FLimits.MaximumEntries = 0) or
      (FCurrent.RetainedBytes > FLimits.MaximumBytes) or
      (LAfter.RetainedBytes > FLimits.MaximumBytes)) then
  begin
    raise ENyxTextHistory.Create('Replacement cannot retain its immediate Undo/Redo within these limits');
  end;
  Commit(FCurrent, LAfter);
  Inc(FGeneration);
end;

function TTextHistory.Undo: Boolean;
var
  LUndo: TTextFrames;
  LRedo: TTextFrames;
  LNext: TNyxTextEditorFrame;
  LTruncated: Boolean;
begin
  RequireGeneration;

  if FComposing then
  begin
    raise ENyxTextHistory.Create('The IME still owns this editing transaction');
  end;
  Result := CanUndo;

  if not Result then
  begin
    Exit;
  end;
  LNext := FUndo[High(FUndo)];
  LUndo := Copy(FUndo, 0, Length(FUndo) - 1);
  LRedo := Copy(FRedo);
  LTruncated := not Push(LRedo, FCurrent);
  LTruncated := Trim(LUndo, LRedo) or LTruncated;
  FUndo := LUndo;
  FRedo := LRedo;
  FCurrent := LNext;
  FTruncated := FTruncated or LTruncated;
  Inc(FGeneration);
end;

function TTextHistory.Redo: Boolean;
var
  LUndo: TTextFrames;
  LRedo: TTextFrames;
  LNext: TNyxTextEditorFrame;
  LTruncated: Boolean;
begin
  RequireGeneration;

  if FComposing then
  begin
    raise ENyxTextHistory.Create('The IME still owns this editing transaction');
  end;
  Result := CanRedo;

  if not Result then
  begin
    Exit;
  end;
  LNext := FRedo[High(FRedo)];
  LRedo := Copy(FRedo, 0, Length(FRedo) - 1);
  LUndo := Copy(FUndo);
  LTruncated := not Push(LUndo, FCurrent);
  LTruncated := Trim(LUndo, LRedo) or LTruncated;
  FUndo := LUndo;
  FRedo := LRedo;
  FCurrent := LNext;
  FTruncated := FTruncated or LTruncated;
  Inc(FGeneration);
end;

function TTextHistory.UndoFrame(out AFrame: TNyxTextEditorFrame): Boolean;
begin
  AFrame := Default(TNyxTextEditorFrame);
  Result := CanUndo;

  if Result then
  begin
    AFrame := FUndo[High(FUndo)];
  end;
end;

function TTextHistory.RedoFrame(out AFrame: TNyxTextEditorFrame): Boolean;
begin
  AFrame := Default(TNyxTextEditorFrame);
  Result := CanRedo;

  if Result then
  begin
    AFrame := FRedo[High(FRedo)];
  end;
end;

function NewNyxTextEditHistory(const AInitial: TNyxTextEditorFrame): INyxTextEditHistory;
begin
  Result := NewNyxTextEditHistory(AInitial, TNyxTextHistoryLimits.Defaults);
end;

function NewNyxTextEditHistory(const AInitial: TNyxTextEditorFrame;
  const ALimits: TNyxTextHistoryLimits): INyxTextEditHistory;
var
  LHistory: TTextHistory;
begin
  ALimits.Validate;
  AInitial.Validate;
  LHistory := TTextHistory.Create;
  Result := LHistory;
  LHistory.FLimits := ALimits;
  LHistory.Reset(AInitial);
end;

end.
