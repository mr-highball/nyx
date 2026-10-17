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

unit nyx.pascal.completion;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.pascal.sources, nyx.pascal.drafts;

type
  { Descriptors supplied by an assistance provider, never document/control kinds
    or permission to publish source. Unknown preserves an unresolved descriptor. }
  TNyxPascalCompletionKind = (pckUnknown, pckType, pckMethod, pckProperty,
    pckField, pckVariable, pckConstant, pckParameter, pckUnit, pckKeyword);

  { A ready page may have more matches. Unavailable/failed/cancelled results have
    no insertion rows; none of these states implies application compilation. }
  TNyxPascalCompletionStatus = (pcsReady, pcsUnavailable, pcsFailed, pcsCancelled);

  { Detached suggestion text. InsertText is Pascal text, not an executable
    callback. Details describe provider knowledge; no host path is required. }
  TNyxPascalCompletionItem = record
  private
    FName: TNyxText;
    FInsertText: TNyxText;
    FDetail: TNyxText;
    FKind: TNyxPascalCompletionKind;
  public
    property Name: TNyxText read FName;
    property InsertText: TNyxText read FInsertText;
    property Detail: TNyxText read FDetail;
    property Kind: TNyxPascalCompletionKind read FKind;
  end;

  { Frozen query over all exact source buffers and accepted membership. At/From/
    Skip/Take return independent values; no provider, session, compiler, mutable
    editor or filesystem authority is retained. Caret counts Unicode scalars.
    Includes retain an explicit owning unit, never a guessed filename context. }
  INyxPascalCompletionRequest = interface(IInterface)
    ['{DA4983AA-4D64-4E78-AC7D-9D091D512F32}']
    function Sources: INyxPascalSourceSnapshot;
    function Owner: TNyxPascalSourceRef;
    function FileReference: TNyxPascalSourceRef;
    function Caret: Integer;
    function Offset: Integer;
    function Limit: Integer;
    function At(const AFile: TNyxPascalSourceRef; ACaret: Integer): INyxPascalCompletionRequest;
    function From(const AOwner: TNyxPascalSourceRef): INyxPascalCompletionRequest;
    function Skip(AOffset: Integer): INyxPascalCompletionRequest;
    function Take(ALimit: Integer): INyxPascalCompletionRequest;
    { Compare exact membership, origin and all accepted/current texts. Caller
      revisions remain an additional host guard, never an inferred edit right. }
    function Matches(const ASources: INyxPascalSourceSnapshot): Boolean;
  end;

  { Immutable bounded response tied to one frozen query. Preview changes only
    a returned buffer snapshot after exact input comparison, retaining all other
    files and accepted comparison text. It never edits a session/history or
    admits/compiles source. NewCaret is the end of that proposed insertion. }
  INyxPascalCompletionResult = interface(IInterface)
    ['{DA4983AA-4D64-4E78-AC7D-9D091D512F33}']
    function Request: INyxPascalCompletionRequest;
    function Status: TNyxPascalCompletionStatus;
    function MessageText: TNyxText;
    function Count: Integer;
    function Total: Integer;
    function Item(AIndex: Integer): TNyxPascalCompletionItem;
    function ReplaceStart: Integer;
    function ExpectedText: TNyxText;
    function Preview(AIndex: Integer; const ACurrent: INyxPascalSourceSnapshot;
      out ANewCaret: Integer): INyxPascalSourceSnapshot;
  end;

  { Replaceable target/machine adapter. Synchronous calls belong on the host's
    worker/serialized service boundary; implementations must not touch UI.
    Providers return detached data and must not execute authored applications.
    A missing provider never prevents ordinary source browsing or design. }
  INyxPascalCompletionProvider = interface(IInterface)
    ['{DA4983AA-4D64-4E78-AC7D-9D091D512F34}']
    function Complete(const ARequest: INyxPascalCompletionRequest): INyxPascalCompletionResult;
  end;

{ Main/caret zero and a twenty-row first page are defaults; callers fluently
  choose exact file/owner/caret and pages up to fifty rows. Capture never parses
  an unfinished buffer or treats it as a compiler project. }
function NewNyxPascalCompletionRequest(
  const ASources: INyxPascalSourceSnapshot): INyxPascalCompletionRequest;
{ Validate bounded Unicode descriptors: name 256, insertion 4096 and detail
  2048 scalars. These are display/text values; kind is a closed Pascal enum. }
function NyxPascalCompletion(const AName, AInsertText, ADetail: TNyxText;
  AKind: TNyxPascalCompletionKind): TNyxPascalCompletionItem;
{ Freeze a provider page with an exact shared replacement range, including any
  identifier tail after the caret. Ready ranges must contain the queried caret
  and match its original buffer. Whole response text is bounded to 128 KiB UTF-8.
  Other statuses require zero entries and zero total. All failures are atomic. }
function NewNyxPascalCompletionResult(const ARequest: INyxPascalCompletionRequest;
  AStatus: TNyxPascalCompletionStatus; const AMessage: TNyxText;
  AReplaceStart: Integer; const AExpected: TNyxText;
  const AItems: array of TNyxPascalCompletionItem; ATotal: Integer): INyxPascalCompletionResult;
{ Freeze alternate query providers before dispatch/serialization; callers
  serialize mutable producers during this call. Returned getters are detached. }
function CaptureNyxPascalCompletionRequest(
  const ARequest: INyxPascalCompletionRequest): INyxPascalCompletionRequest;

implementation

uses SysUtils, nyx.editing, nyx.bytes, nyx.pascal.project;

type
  { Immutable query storage. Interfaces live in this managed class, never inside
    records, keeping FPC/pas2js ownership aligned without borrowed callbacks. }
  TCompletionRequest = class(TInterfacedObject, INyxPascalCompletionRequest)
  private
    FSources: INyxPascalSourceSnapshot;
    FOwner: TNyxPascalSourceRef;
    FFile: TNyxPascalSourceRef;
    FCaret: Integer;
    FOffset: Integer;
    FLimit: Integer;
    function CopyValue: TCompletionRequest;
  public
    function Sources: INyxPascalSourceSnapshot;
    function Owner: TNyxPascalSourceRef;
    function FileReference: TNyxPascalSourceRef;
    function Caret: Integer;
    function Offset: Integer;
    function Limit: Integer;
    function At(const AFile: TNyxPascalSourceRef; ACaret: Integer): INyxPascalCompletionRequest;
    function From(const AOwner: TNyxPascalSourceRef): INyxPascalCompletionRequest;
    function Skip(AOffset: Integer): INyxPascalCompletionRequest;
    function Take(ALimit: Integer): INyxPascalCompletionRequest;
    function Matches(const ASources: INyxPascalSourceSnapshot): Boolean;
  end;

  { Owned reply vector and captured query have no provider/editor back-reference.
    Preview stages a fresh snapshot; publishing remains the editor's operation. }
  TCompletionResult = class(TInterfacedObject, INyxPascalCompletionResult)
  private
    FRequest: INyxPascalCompletionRequest;
    FStatus: TNyxPascalCompletionStatus;
    FMessage: TNyxText;
    FStart: Integer;
    FExpected: TNyxText;
    FItems: array of TNyxPascalCompletionItem;
    FTotal: Integer;
  public
    function Request: INyxPascalCompletionRequest;
    function Status: TNyxPascalCompletionStatus;
    function MessageText: TNyxText;
    function Count: Integer;
    function Total: Integer;
    function Item(AIndex: Integer): TNyxPascalCompletionItem;
    function ReplaceStart: Integer;
    function ExpectedText: TNyxText;
    function Preview(AIndex: Integer; const ACurrent: INyxPascalSourceSnapshot;
      out ANewCaret: Integer): INyxPascalSourceSnapshot;
  end;

procedure CheckText(const AText: TNyxText; AMaximum: Integer);
var
  LPosition: Integer;
  LScalar: Integer;
  LCount: Integer;
begin
  LPosition := 1;
  LCount := 0;
  while LPosition <= Length(AText) do
  begin

    if not NyxNextScalar(AText, LPosition, LScalar) then
    begin
      raise ENyxPascalProject.Create('Completion text has malformed Unicode');
    end;
    Inc(LCount);

    if LCount > AMaximum then
    begin
      raise ENyxPascalProject.Create('Completion text exceeds its scalar budget');
    end;
  end;
end;

{ Convert only validated scalar boundaries. Native bytes and browser UTF-16
  positions stay private; a supplementary character is never split. }
function Position(const AText: TNyxText; AOffset: Integer): Integer;
var
  LCount: Integer;
  LScalar: Integer;
begin

  if AOffset < 0 then
  begin
    raise ENyxPascalProject.Create('A completion offset cannot be negative');
  end;
  Result := 1;
  for LCount := 1 to AOffset do
  begin

    if (Result > Length(AText)) or not NyxNextScalar(AText, Result, LScalar) then
    begin
      raise ENyxPascalProject.Create('Completion range is outside its captured buffer');
    end;
  end;
end;

function TCompletionRequest.CopyValue: TCompletionRequest;
begin
  Result := TCompletionRequest.Create;
  Result.FSources := FSources;
  Result.FOwner := FOwner;
  Result.FFile := FFile;
  Result.FCaret := FCaret;
  Result.FOffset := FOffset;
  Result.FLimit := FLimit;
end;

function TCompletionRequest.Sources: INyxPascalSourceSnapshot;
begin
  Result := FSources;
end;

function TCompletionRequest.Owner: TNyxPascalSourceRef;
begin
  Result := FOwner;
end;

function TCompletionRequest.FileReference: TNyxPascalSourceRef;
begin
  Result := FFile;
end;

function TCompletionRequest.Caret: Integer;
begin
  Result := FCaret;
end;

function TCompletionRequest.Offset: Integer;
begin
  Result := FOffset;
end;

function TCompletionRequest.Limit: Integer;
begin
  Result := FLimit;
end;

function TCompletionRequest.At(const AFile: TNyxPascalSourceRef;
  ACaret: Integer): INyxPascalCompletionRequest;
var
  LNext: TCompletionRequest;
begin
  AFile.Validate;
  Position(FSources.Text(AFile), ACaret);
  LNext := CopyValue;
  LNext.FFile := AFile;
  LNext.FCaret := ACaret;

  if AFile.Role <> npsInclude then
  begin
    LNext.FOwner := AFile;
  end;
  Result := LNext;
end;

function TCompletionRequest.From(const AOwner: TNyxPascalSourceRef): INyxPascalCompletionRequest;
var
  LNext: TCompletionRequest;
begin
  AOwner.Validate;

  if AOwner.Role = npsInclude then
  begin
    raise ENyxPascalProject.Create('A completion include needs a main/helper owning unit');
  end;
  FSources.Text(AOwner);
  LNext := CopyValue;
  LNext.FOwner := AOwner;
  Result := LNext;
end;

function TCompletionRequest.Skip(AOffset: Integer): INyxPascalCompletionRequest;
var
  LNext: TCompletionRequest;
begin

  if (AOffset < 0) or (AOffset > 4096) then
  begin
    raise ENyxPascalProject.Create('Completion paging offset must be between zero and 4096');
  end;
  LNext := CopyValue;
  LNext.FOffset := AOffset;
  Result := LNext;
end;

function TCompletionRequest.Take(ALimit: Integer): INyxPascalCompletionRequest;
var
  LNext: TCompletionRequest;
begin

  if (ALimit < 1) or (ALimit > 50) then
  begin
    raise ENyxPascalProject.Create('Completion pages need between one and fifty rows');
  end;
  LNext := CopyValue;
  LNext.FLimit := ALimit;
  Result := LNext;
end;

function TCompletionRequest.Matches(const ASources: INyxPascalSourceSnapshot): Boolean;
var
  LCurrent: INyxPascalSourceSnapshot;
  LIndex: Integer;
  LFile: TNyxPascalSourceRef;
begin
  Result := False;

  if ASources = nil then
  begin
    Exit;
  end;
  LCurrent := CaptureNyxPascalSources(ASources);

  if (LCurrent.Kind <> FSources.Kind) or
    (LCurrent.MainUnit.Name <> FSources.MainUnit.Name) or
    (LCurrent.Count <> FSources.Count) or
    (LCurrent.Limits.MaximumFiles <> FSources.Limits.MaximumFiles) or
    (LCurrent.Limits.MaximumBytes <> FSources.Limits.MaximumBytes) then
  begin
    Exit;
  end;
  for LIndex := 0 to FSources.Count - 1 do
  begin
    LFile := FSources.FileReference(LIndex);

    if not LCurrent.FileReference(LIndex).SameFile(LFile) or
      (LCurrent.Text(LFile) <> FSources.Text(LFile)) or
      (LCurrent.AcceptedText(LFile) <> FSources.AcceptedText(LFile)) then
    begin
      Exit;
    end;
  end;
  Result := True;
end;

function NewNyxPascalCompletionRequest(
  const ASources: INyxPascalSourceSnapshot): INyxPascalCompletionRequest;
var
  LRequest: TCompletionRequest;
begin
  LRequest := TCompletionRequest.Create;
  Result := LRequest;
  LRequest.FSources := CaptureNyxPascalSources(ASources);
  LRequest.FOwner := TNyxPascalSourceRef.Main;
  LRequest.FFile := TNyxPascalSourceRef.Main;
  LRequest.FLimit := 20;
end;

function CaptureNyxPascalCompletionRequest(
  const ARequest: INyxPascalCompletionRequest): INyxPascalCompletionRequest;
var
  LFile: TNyxPascalSourceRef;
  LOwner: TNyxPascalSourceRef;
begin

  if ARequest = nil then
  begin
    raise ENyxPascalProject.Create('A completion query requires captured inputs');
  end;
  LFile := ARequest.FileReference;
  LOwner := ARequest.Owner;

  if (LFile.Role <> npsInclude) and not LFile.SameFile(LOwner) then
  begin
    raise ENyxPascalProject.Create('A main/helper completion uses its own translation unit');
  end;
  Result := NewNyxPascalCompletionRequest(ARequest.Sources)
    .From(LOwner).At(LFile, ARequest.Caret)
    .Skip(ARequest.Offset).Take(ARequest.Limit);
end;

function NyxPascalCompletion(const AName, AInsertText, ADetail: TNyxText;
  AKind: TNyxPascalCompletionKind): TNyxPascalCompletionItem;
begin
  CheckText(AName, 256);
  CheckText(AInsertText, 4096);
  CheckText(ADetail, 2048);

  if (AName = '') or (Ord(AKind) < Ord(Low(TNyxPascalCompletionKind))) or
    (Ord(AKind) > Ord(High(TNyxPascalCompletionKind))) then
  begin
    raise ENyxPascalProject.Create('A completion needs a name and known kind');
  end;
  Result.FName := AName;
  Result.FInsertText := AInsertText;
  Result.FDetail := ADetail;
  Result.FKind := AKind;
end;

function TCompletionResult.Request: INyxPascalCompletionRequest;
begin
  Result := FRequest;
end;

function TCompletionResult.Status: TNyxPascalCompletionStatus;
begin
  Result := FStatus;
end;

function TCompletionResult.MessageText: TNyxText;
begin
  Result := FMessage;
end;

function TCompletionResult.Count: Integer;
begin
  Result := Length(FItems);
end;

function TCompletionResult.Total: Integer;
begin
  Result := FTotal;
end;

function TCompletionResult.Item(AIndex: Integer): TNyxPascalCompletionItem;
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ENyxPascalProject.Create('Completion row is outside its captured page');
  end;
  Result := FItems[AIndex];
end;

function TCompletionResult.ReplaceStart: Integer;
begin
  Result := FStart;
end;

function TCompletionResult.ExpectedText: TNyxText;
begin
  Result := FExpected;
end;

function TCompletionResult.Preview(AIndex: Integer; const ACurrent: INyxPascalSourceSnapshot;
  out ANewCaret: Integer): INyxPascalSourceSnapshot;
var
  LText: TNyxText;
  LStart: Integer;
  LFinish: Integer;
  LItem: TNyxPascalCompletionItem;
  LCurrent: INyxPascalSourceSnapshot;
begin
  ANewCaret := 0;

  if FStatus <> pcsReady then
  begin
    raise ENyxPascalProject.Create('Completion has no ready insertion page');
  end;
  LCurrent := CaptureNyxPascalSources(ACurrent);

  if not FRequest.Matches(LCurrent) then
  begin
    raise ENyxPascalProject.Create('Completion inputs changed; request a fresh page');
  end;
  LItem := Item(AIndex);
  LText := FRequest.Sources.Text(FRequest.FileReference);
  LStart := Position(LText, FStart);
  LFinish := Position(LText, FStart + NyxTextScalarCount(FExpected));
  LText := Copy(LText, 1, LStart - 1) + LItem.InsertText + Copy(LText, LFinish, MaxInt);
  Result := LCurrent.WithBuffer(FRequest.FileReference, LText);
  ANewCaret := FStart + NyxTextScalarCount(LItem.InsertText);
end;

function NewNyxPascalCompletionResult(const ARequest: INyxPascalCompletionRequest;
  AStatus: TNyxPascalCompletionStatus; const AMessage: TNyxText;
  AReplaceStart: Integer; const AExpected: TNyxText;
  const AItems: array of TNyxPascalCompletionItem; ATotal: Integer): INyxPascalCompletionResult;
var
  LResult: TCompletionResult;
  LText: TNyxText;
  LStart: Integer;
  LFinish: Integer;
  LIndex: Integer;
  LBytes: Integer;
begin
  LResult := TCompletionResult.Create;
  Result := LResult;
  LResult.FRequest := CaptureNyxPascalCompletionRequest(ARequest);
  CheckText(AMessage, 2048);
  CheckText(AExpected, 4096);

  if (Ord(AStatus) < Ord(Low(TNyxPascalCompletionStatus))) or
    (Ord(AStatus) > Ord(High(TNyxPascalCompletionStatus))) or
    (ATotal < Length(AItems)) or (Length(AItems) > LResult.FRequest.Limit) then
  begin
    raise ENyxPascalProject.Create('Invalid completion result state or page budget');
  end;

  if AStatus = pcsReady then
  begin

    if (Length(AItems) > 0) and
      (ATotal - Length(AItems) < LResult.FRequest.Offset) then
    begin
      raise ENyxPascalProject.Create('Completion page is outside its total match count');
    end;
    LText := LResult.FRequest.Sources.Text(LResult.FRequest.FileReference);
    LStart := Position(LText, AReplaceStart);
    LFinish := Position(LText, AReplaceStart + NyxTextScalarCount(AExpected));

    if (LResult.FRequest.Caret < AReplaceStart) or
      (LResult.FRequest.Caret > AReplaceStart + NyxTextScalarCount(AExpected)) or
      (Copy(LText, LStart, LFinish - LStart) <> AExpected) then
    begin
      raise ENyxPascalProject.Create('Completion replacement does not match its captured caret/buffer');
    end;
  end
  else if (Length(AItems) <> 0) or (ATotal <> 0) or (AExpected <> '') or (AReplaceStart <> 0) then
  begin
    raise ENyxPascalProject.Create('An unavailable completion cannot contain insertion rows');
  end;
  LResult.FStatus := AStatus;
  LResult.FMessage := AMessage;
  LResult.FStart := AReplaceStart;
  LResult.FExpected := AExpected;
  LResult.FTotal := ATotal;
  LBytes := NyxUTF8ByteCount(AMessage) + NyxUTF8ByteCount(AExpected);
  SetLength(LResult.FItems, Length(AItems));
  for LIndex := 0 to High(AItems) do
  begin
    LResult.FItems[LIndex] := NyxPascalCompletion(AItems[LIndex].Name,
      AItems[LIndex].InsertText, AItems[LIndex].Detail, AItems[LIndex].Kind);
    Inc(LBytes, NyxUTF8ByteCount(AItems[LIndex].Name) +
      NyxUTF8ByteCount(AItems[LIndex].InsertText) + NyxUTF8ByteCount(AItems[LIndex].Detail));

    if LBytes > 128 * 1024 then
    begin
      raise ENyxPascalProject.Create('Completion response exceeds 128 KiB UTF-8');
    end;
  end;
end;

end.
