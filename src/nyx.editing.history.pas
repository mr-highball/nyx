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

uses SysUtils, nyx.text, nyx.editing, nyx.text.projection;

type
  { Invalid/stale editing transactions refuse before changing text or either
    stack. Ordinary physical observation may evict old entries within its budget. }
  ENyxTextHistory = class(Exception);

  { Detached immutable physical text/selection with exact authored bytes.
    Plain frames use the same text for both. Projected frames preserve different
    line endings without COM-interface fields in records. Selection counts
    physical scalars; unavailable selection stays unavailable. }
  TNyxTextEditorFrame = record
  private
    FDefined: Boolean;
    FText: TNyxText;
    FAuthoredText: TNyxText;
    FPresentation: TNyxTextLineEndings;
    FSelection: TNyxTextSelection;
    FScalars: Integer;
    FBytes: Integer;
  public
    procedure Validate;
    function SameValue(const AOther: TNyxTextEditorFrame): Boolean;
    { Reconstruct an immutable checked projection, borrowing no frame storage. }
    function Projection: INyxTextProjection;
    property Text: TNyxText read FText;
    property AuthoredText: TNyxText read FAuthoredText;
    property Presentation: TNyxTextLineEndings read FPresentation;
    property Selection: TNyxTextSelection read FSelection;
    property Scalars: Integer read FScalars;
    property RetainedBytes: Integer read FBytes;
  end;

  { Copied runtime retention policy. Both stacks count UTF-8 text plus 32 bytes
    per frame, excluding Current; this is accounting, not heap measurement.
    Distinct authored text is accounted in addition to physical text.
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

  { Optional implementation capability: projected frames, authored byte/policy
    equality and replacement/retention are preserved through every operation.
    The source-level history methods remain compatible; native identity below
    is versioned because returned frame records now carry authored state. }
  INyxProjectedTextHistory = interface(IInterface)
    ['{809BF35A-BA27-4CA7-A8E7-0C45E9EC8091}']
  end;

  { A UI-thread-owned independent editing timeline. Observe admits actual physical
    edits; composition drafts update Current but commit one history entry at end.
    Selection-only observations preserve Redo. Replace requires an exact current
    frame and room for its immediate Undo/Redo states; it never silently loses
    that command. No platform default, control, application or document is owned.
    Hosts render Current and publish their ordinary draft notification separately. }
  { Version-two native identity accompanies the expanded returned frame record.
    Rebuild native consumers together; never mix this layout with the old IID. }
  INyxTextEditHistory = interface(IInterface)
    ['{167F84E3-D535-4AD1-90A1-A8847BB84702}']
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
    { Scalar replacement inside the exact current physical text. Insertion is
      authored text, presented using the frame's explicit newline policy.
      Projected histories preserve untouched authored bytes. All validation/allocation
      precedes stack publication; composition, stale frames and budgets refuse.
      Equal authored/displayed text only changes selection and preserves Redo. }
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
{ Capture both complete immutable texts. Native raw presentation defaults to
  Preserve; a browser textarea owner explicitly supplies LF for new insertions. }
function NyxProjectedTextEditorFrame(const AProjection: INyxTextProjection;
  const ASelection: TNyxTextSelection;
  APresentation: TNyxTextLineEndings = ntePreserve): TNyxTextEditorFrame;
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
  TTextHistory = class(TInterfacedObject, INyxTextEditHistory, INyxProjectedTextHistory)
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
  Result.FAuthoredText := AText;
  Result.FPresentation := ntePreserve;
  Result.FSelection := ASelection;
  Result.FBytes := LBytes + 32;
  Result.FDefined := True;
  Result.Validate;
end;

function NyxProjectedTextEditorFrame(const AProjection: INyxTextProjection;
  const ASelection: TNyxTextSelection;
  APresentation: TNyxTextLineEndings): TNyxTextEditorFrame;
var
  LProjection: INyxTextProjection;
  LBytes: TNyxTextBytes;
begin

  if AProjection = nil then
  begin
    raise ENyxTextHistory.Create('Projected text frame requires complete text');
  end;
  LProjection := NewNyxTextProjection(AProjection.Authored, AProjection.Presented);
  NyxPresentText('', APresentation);
  Result.FText := LProjection.Presented;
  Result.FAuthoredText := LProjection.Authored;
  Result.FPresentation := APresentation;
  Result.FScalars := LProjection.PresentedScalars;
  Result.FSelection := ASelection;
  LBytes := NyxUTF8ByteCount(Result.FText);

  if Result.FAuthoredText <> Result.FText then
  begin
    LBytes := LBytes + NyxUTF8ByteCount(Result.FAuthoredText);
  end;

  if LBytes > High(Integer) - 32 then
  begin
    raise ENyxTextHistory.Create('Projected frame exceeds its byte accounting range');
  end;
  Result.FBytes := Integer(LBytes) + 32;
  Result.FDefined := True;
  Result.Validate;
end;

function TNyxTextEditorFrame.Projection: INyxTextProjection;
begin
  Validate;
  Result := NewNyxTextProjection(FAuthoredText, FText);
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
  Result := (FText = AOther.FText) and (FAuthoredText = AOther.FAuthoredText) and
    (FPresentation = AOther.FPresentation) and FSelection.SameRange(AOther.FSelection);
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

  if (ABefore.Text = AAfter.Text) and (ABefore.AuthoredText = AAfter.AuthoredText) and
    (ABefore.Presentation = AAfter.Presentation) then
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

procedure TTextHistory.Replace(const AExpected: TNyxTextEditorFrame;
  AStart: Integer; const AExpectedText, AInsertion: TNyxText);
var
  LExpectedScalars: Integer;
  LAfter: TNyxTextEditorFrame;
  LCaret: Integer;
  LInsertion: TNyxTextInsertion;
  LProjection: INyxTextProjection;
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
  try
    LInsertion := NyxTextInsertion(AInsertion).UsingLineEndings(FCurrent.Presentation);
    LProjection := FCurrent.Projection.ReplacePresented(AStart, AExpectedText, LInsertion);
  except
    on LException: ENyxTextProjection do
    begin
      raise ENyxTextHistory.Create(LException.Message);
    end;
  end;
  LCaret := AStart + NyxTextScalarCount(LInsertion.Presented);
  LAfter := NyxProjectedTextEditorFrame(LProjection,
    NyxTextSelection(LProjection.Presented, LCaret, LCaret), FCurrent.Presentation);

  if ((LAfter.Text <> FCurrent.Text) or (LAfter.AuthoredText <> FCurrent.AuthoredText)) and
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
