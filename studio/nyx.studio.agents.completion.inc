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

{ Completion is inspection of acknowledged source, so unfinished drafts are
  valid inputs. Exact revision is mandatory for both origins; omitted snapshot
  deliberately means editing here, unlike the older accepted-source read modes. }
function TNyxAgentSession.CapturePascalCompletion(const AArguments: TNyxDataValue;
  out AContext: TNyxAgentPascalCompletionContext): INyxPascalCompletionRequest;
begin
  Result := CaptureCompletion(AArguments, AContext, False);
end;

function TNyxAgentSession.CaptureEditorCompletion(const AArguments: TNyxDataValue;
  out AContext: TNyxAgentPascalCompletionContext): INyxPascalCompletionRequest;
begin
  Result := CaptureCompletion(AArguments, AContext, True);
end;

function TNyxAgentSession.CaptureCompletion(const AArguments: TNyxDataValue;
  out AContext: TNyxAgentPascalCompletionContext;
  AOperator: Boolean): INyxPascalCompletionRequest;
var
  LKind: TNyxPascalSourceSnapshotKind;
  LFile: TNyxPascalSourceRef;
  LOwner: TNyxPascalSourceRef;
begin
  AContext := Default(TNyxAgentPascalCompletionContext);

  if not AOperator and (FPermission = apDisabled) then
  begin
    raise ENyxProjectConflict.Create('Agent access is disabled in Studio');
  end;
  NyxAgentFields(AArguments, '|mode|expectedRevision|snapshot|file|owner|caret|offset|limit|');

  if TextArgument(AArguments, 'mode') <> 'complete' then
  begin
    raise ENyxModel.Create('Completion capture requires the complete mode');
  end;
  RequireRevision(AArguments);
  LKind := pssEditing;

  if NyxAgentHas(AArguments, 'snapshot') then
  begin
    LKind := SourceReadKind(AArguments);
  end;
  RoutineContext(AArguments, LFile, LOwner);
  Result := CaptureNyxPascalCompletionRequest(
    NewNyxPascalCompletionRequest(SourceInspection(LKind))
      .At(LFile, AArguments.Field('caret').AsInteger).From(LOwner)
      .Skip(IntegerArgument(AArguments, 'offset', 0, 0, 4096))
      .Take(IntegerArgument(AArguments, 'limit', 20, 1, 50)));
  AContext.FSession := FSession.CommandContext;
  AContext.FRevision := FRevision;
  AContext.FPermissionGeneration := FPermissionGeneration;
  AContext.FOperator := AOperator;
end;

{ Bind the returned page to every query selector, not merely its source text.
  An alternate provider cannot substitute another caret, file, owner or page. }
function SameCompletionQuery(const ALeft,
  ARight: INyxPascalCompletionRequest): Boolean;
begin
  Result := (ALeft <> nil) and (ARight <> nil);

  if Result then
  begin
    Result := ALeft.FileReference.SameFile(ARight.FileReference) and
      ALeft.Owner.SameFile(ARight.Owner) and (ALeft.Caret = ARight.Caret) and
      (ALeft.Offset = ARight.Offset) and (ALeft.Limit = ARight.Limit) and
      ALeft.Matches(ARight.Sources);
  end;
end;

function TNyxAgentSession.FinishPascalCompletion(const AArguments: TNyxDataValue;
  const AContext: TNyxAgentPascalCompletionContext;
  const ARequest: INyxPascalCompletionRequest;
  const AResult: INyxPascalCompletionResult): TNyxDataValue;
var
  LDetached: INyxPascalCompletionResult;
begin
  Result := FinishCompletion(AArguments, AContext, ARequest, AResult, False, LDetached);
end;

function TNyxAgentSession.FinishEditorCompletion(const AArguments: TNyxDataValue;
  const AContext: TNyxAgentPascalCompletionContext;
  const ARequest: INyxPascalCompletionRequest;
  const AResult: INyxPascalCompletionResult): TNyxDataValue;
var
  LDetached: INyxPascalCompletionResult;
begin
  Result := FinishCompletion(AArguments, AContext, ARequest, AResult, True, LDetached);
end;

function TNyxAgentSession.FinishEditorCompletion(const AArguments: TNyxDataValue;
  const AContext: TNyxAgentPascalCompletionContext;
  const ARequest: INyxPascalCompletionRequest;
  const AResult: INyxPascalCompletionResult;
  out AFrozenResult: INyxPascalCompletionResult): TNyxDataValue;
begin
  Result := FinishCompletion(AArguments, AContext, ARequest, AResult, True, AFrozenResult);
end;

function TNyxAgentSession.FinishCompletion(const AArguments: TNyxDataValue;
  const AContext: TNyxAgentPascalCompletionContext;
  const ARequest: INyxPascalCompletionRequest;
  const AResult: INyxPascalCompletionResult; AOperator: Boolean;
  out AFrozenResult: INyxPascalCompletionResult): TNyxDataValue;
const
  CStatuses: array[TNyxPascalCompletionStatus] of TNyxText =
    ('ready', 'unavailable', 'failed', 'cancelled');
  CKinds: array[TNyxPascalCompletionKind] of TNyxText =
    ('unknown', 'type', 'method', 'property', 'field', 'variable', 'constant',
      'parameter', 'unit', 'keyword');
var
  LCurrent: INyxPascalCompletionRequest;
  LChecked: INyxPascalCompletionResult;
  LContext: TNyxAgentPascalCompletionContext;
  LItem: TNyxPascalCompletionItem;
  LRows: array of TNyxDataValue;
  LAt: Integer;
  LSnapshot: TNyxText;
begin
  AFrozenResult := nil;
  LCurrent := CaptureCompletion(AArguments, LContext, AOperator);

  if not FSession.MatchesCommandContext(AContext.FSession) or
    (AContext.FRevision <> FRevision) or
    (AContext.FOperator <> AOperator) or
    (not AOperator and (AContext.FPermissionGeneration <> FPermissionGeneration)) or
    not SameCompletionQuery(ARequest, LCurrent) then
  begin
    raise ENyxProjectConflict.Create('Completion context changed; request a fresh page');
  end;

  if AResult = nil then
  begin
    raise ENyxProjectConflict.Create('The completion provider returned no page');
  end;
  { Independently freeze alternate provider rows and optional project targets
    against the original query before serialization or private typed handoff. }
  LChecked := CaptureNyxPascalCompletionResult(AResult, ARequest);
  SetLength(LRows, LChecked.Count);
  for LAt := 0 to LChecked.Count - 1 do
  begin
    LItem := LChecked.Item(LAt);
    LRows[LAt] := NyxObject([
      NyxField('index', NyxData(ARequest.Offset + LAt)),
      NyxField('name', NyxData(LItem.Name)),
      NyxField('insert', NyxData(LItem.InsertText)),
      NyxField('detail', NyxData(LItem.Detail)),
      NyxField('kind', NyxData(CKinds[LItem.Kind]))]);
  end;
  LSnapshot := 'editing';

  if ARequest.Sources.Kind = pssAccepted then
  begin
    LSnapshot := 'accepted';
  end;
  Result := NyxObject([
    NyxField('revision', NyxData(FRevision)),
    NyxField('snapshot', NyxData(LSnapshot)),
    NyxField('file', SourceFileSelectionData(ARequest.FileReference)),
    NyxField('owner', SourceFileSelectionData(ARequest.Owner)),
    NyxField('caret', NyxData(ARequest.Caret)),
    NyxField('offset', NyxData(ARequest.Offset)),
    NyxField('limit', NyxData(ARequest.Limit)),
    NyxField('status', NyxData(CStatuses[LChecked.Status])),
    NyxField('message', NyxData(LChecked.MessageText)),
    NyxField('total', NyxData(LChecked.Total)),
    NyxField('nextOffset', NyxData(Min(ARequest.Offset + LChecked.Count, LChecked.Total))),
    NyxField('replacement', NyxObject([
      NyxField('offset', NyxData(LChecked.ReplaceStart)),
      NyxField('expected', NyxData(LChecked.ExpectedText))])),
    NyxField('items', NyxArray(LRows)),
    NyxField('inspectionOnly', NyxData(True)),
    NyxField('pendingDrafts', NyxData(FSession.SourceDraftsPending))]);
  BoundContext(Result);

  if AOperator then
  begin
    AFrozenResult := LChecked;
  end;
end;
