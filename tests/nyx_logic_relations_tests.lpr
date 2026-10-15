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
program nyx_logic_relations_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.logic, nyx.logic.collections, nyx.logic.relations,
  nyx.numbers.decimal, nyx.flow, nyx.flow.types, nyx.flow.declarations,
  nyx.scheduler, nyx.model, nyx.controls, nyx.codec, nyx.codegen, nyx.source,
  nyx.logic.editor, nyx.studio.agents, nyx.studio.projects, nyx.studio.session,
  nyx.test.logic.relations
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

type
  { Invocation-local cancellation oracle; no operation/UI references. The read
    limit is chosen beyond capture to exercise the subsequent LINQ lookup phase. }
  TCountingScope = class(TInterfacedObject, INyxCancellationScope)
  public
    Reads: Integer;
    Limit: Integer;
    procedure Cancel;
    function GetCancelled: Boolean;
  end;

var
  GChecks: Integer;

procedure TCountingScope.Cancel;
begin
  Limit := -1;
end;

function TCountingScope.GetCancelled: Boolean;
begin
  Inc(Reads);
  Result := Reads > Limit;
end;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure Equal(const AActual: TNyxDataValue; const AExpected: TNyxText);
begin
  Check(AActual.ToJSON = TNyxDataValue.ParseJSON(AExpected).ToJSON, 'Unexpected result: ' + AActual.ToJSON);
end;

function Unary(const AValue: TNyxDataValue): TNyxFlowValues;
begin
  Result := NyxFlowValues.WithValue(NyxLogicPort(nlpJSON), NyxFlowJSON(AValue));
end;

function Partners(const ALeft, ARight: TNyxDataValue): TNyxFlowValues;
begin
  Result := NyxFlowValues.WithValue(NyxLogicPort(nlpLeft), NyxFlowJSON(ALeft))
    .WithValue(NyxLogicPort(nlpRight), NyxFlowJSON(ARight));
end;

procedure Refuse(const AOperation: INyxLogicOperation; const AValues: TNyxFlowValues);
var
  LFailed: Boolean;
begin
  LFailed := False;
  try
    AOperation.Invoke(AValues, nil);
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'Complete invalid invocation must refuse');
end;

procedure Core;
const
  CKey: TNyxText = 'key 🐾' + #0;
var
  LInput: TNyxDataValue;
  LLeft: TNyxDataValue;
  LRight: TNyxDataValue;
  LResult: TNyxDataValue;
  LBefore: TNyxText;
  LRows: array of TNyxDataValue;
  LIndex: Integer;
  LMode: TNyxJSONJoinKind;
  LOperation: INyxLogicOperation;
  LScope: TCountingScope;
  LScopePort: INyxCancellationScope;
  LFailed: Boolean;
begin
  LInput := TNyxDataValue.ParseJSON('[{"k":1.0,"v":"first"},{"k":2,"v":"second"},{"k":1e0,"v":"third"}]');
  LBefore := LInput.ToJSON;
  LResult := NewNyxJSONGroup(NyxDataPath.Field('k'), njkDecimal).Group(LInput);
  Check((LResult.Count = 2) and (LResult.Item(0).Field('key').ToJSON = '1.0') and
    (LResult.Item(0).Field('count').AsInteger = 2), 'Groups use exact numeric equality and first raw key');
  Check((LResult.Item(0).Field('items').Item(1).Field('v').AsText = 'third') and
    (LResult.Item(1).Field('key').AsInteger = 2), 'Group order and member encounter order');
  Equal(NewNyxJSONDistinct(NyxDataPath.Field('k'), njkDecimal).Distinct(LInput),
    '[{"k":1.0,"v":"first"},{"k":2,"v":"second"}]');
  Check(LInput.ToJSON = LBefore, 'Grouping/distinct do not mutate caller rows');
  LInput := NyxArray([NyxNull, NyxData(TNyxText(#0)), NyxData(''), NyxNull, NyxData(TNyxText(#0))]);
  LResult := NewNyxJSONGroup(NyxDataPath, njkText, njnMatch).Group(LInput);
  Check((LResult.Count = 3) and (LResult.Item(0).Field('key').Kind = ndNull) and
    (LResult.Item(1).Field('key').AsText = TNyxText(#0)) and
    (LResult.Item(2).Field('key').AsText = ''), 'Explicit null, NUL text and empty text never collide');
  Refuse(NewNyxJSONGroup(NyxDataPath, njkText), Unary(LInput));
  Equal(NewNyxJSONDistinct(NyxDataPath, njkBoolean).Distinct(
    NyxArray([NyxData(False), NyxData(True), NyxData(False)])), '[false,true]');
  Refuse(NewNyxJSONDistinct(NyxDataPath, njkBoolean), Unary(NyxArray([NyxData('false')])));
  Refuse(NewNyxJSONDistinct(NyxDataPath, njkDecimal),
    Unary(NyxArray([NyxData(NyxDecimal('1e100'))])));
  Equal(NewNyxJSONGroup(NyxDataPath.Field('anything'), njkText).Group(NyxArray([])), '[]');
  Equal(NewNyxJSONDistinct(NyxDataPath, njkText).Distinct(NyxArray([])), '[]');
  Refuse(NewNyxJSONGroup(NyxDataPath.Field('absent'), njkText), Unary(NyxArray([NyxObject([])])));
  LInput := NyxArray([NyxObject([NyxField(CKey, NyxData(CKey))]), NyxObject([NyxField(CKey, NyxData(CKey))])]);
  LResult := NewNyxJSONDistinct(NyxDataPath.Field(CKey), njkText).Distinct(LInput);
  Check((LResult.Count = 1) and (LResult.Item(0).Field(CKey).AsText = CKey), 'Supplementary/NUL keys and values survive cached-record distinct');

  LLeft := TNyxDataValue.ParseJSON('[{"k":1,"id":"a"},{"k":1.0,"id":"b"},{"k":3,"id":"c"}]');
  LRight := TNyxDataValue.ParseJSON('[{"key":1e0,"id":"x"},{"key":2,"id":"y"}]');
  for LMode := Low(TNyxJSONJoinKind) to High(TNyxJSONJoinKind) do
  begin
    LResult := NewNyxJSONJoin(NyxDataPath.Field('k'), NyxDataPath.Field('key'), njkDecimal, LMode).Join(LLeft, LRight);
    case LMode of
      njjInner: Check(LResult.Count = 2, 'Inner emits both left duplicates');
      njjLeft: Check((LResult.Count = 3) and not LResult.Item(2).Field('hasRight').AsBoolean, 'Left retains unmatched left');
      njjRight: Check((LResult.Count = 3) and not LResult.Item(2).Field('hasLeft').AsBoolean and
        (LResult.Item(2).Field('right').Field('id').AsText = 'y'), 'Right retains side identities and right order');
      njjFull: Check((LResult.Count = 4) and
        (LResult.Item(3).Field('right').Field('id').AsText = 'y'), 'Full appends unmatched right');
    end;
    Check(LResult.Item(0).Field('left').Field('k').ToJSON = '1', 'Matched payload preserves original left spelling');
    Check(LResult.Item(0).Field('right').Field('key').ToJSON = '1e0', 'Matched payload preserves original right spelling');
  end;
  LResult := NewNyxJSONJoin(NyxDataPath, NyxDataPath, njkText, njjLeft, njnMatch)
    .Join(NyxArray([NyxNull]), NyxArray([]));
  Check(LResult.Item(0).Field('left').Kind = ndNull, 'Present null partner retains JSON null');
  Check(LResult.Item(0).Field('hasLeft').AsBoolean and
    not LResult.Item(0).Field('hasRight').AsBoolean, 'Partner absence is separate from a present null value');
  LResult := NewNyxJSONJoin(NyxDataPath, NyxDataPath, njkText, njjInner, njnMatch)
    .Join(NyxArray([NyxNull, NyxNull]), NyxArray([NyxNull, NyxNull]));
  Check(LResult.Count = 4, 'Explicit matching null keys emit all duplicate pairs');
  Refuse(NewNyxJSONJoin(NyxDataPath, NyxDataPath, njkText), Partners(NyxArray([]), NyxArray([NyxData(1)])));
  Refuse(NewNyxJSONJoin(NyxDataPath, NyxDataPath, njkText), Partners(NyxArray([NyxData(True)]), NyxArray([])));
  Refuse(NewNyxJSONJoin(NyxDataPath, NyxDataPath, njkText), Partners(NyxObject([]), NyxArray([])));

  Equal(NewNyxJSONAggregate(NyxDataPath, njaSum).Aggregate(TNyxDataValue.ParseJSON('[0.1,0.2,9007199254740993]')),
    '9007199254740993.3');
  Equal(NewNyxJSONAggregate(NyxDataPath, njaAverage).Aggregate(TNyxDataValue.ParseJSON('[0.1,0.2]')), '0.15');
  Equal(NewNyxJSONAggregate(NyxDataPath, njaMinimum).Aggregate(TNyxDataValue.ParseJSON('[1.00,1e0,2]')), '1.00');
  Equal(NewNyxJSONAggregate(NyxDataPath, njaMaximum).Aggregate(TNyxDataValue.ParseJSON('[2.00,2e0,1]')), '2.00');
  Equal(NewNyxJSONAggregate(NyxDataPath, njaCount).Aggregate(NyxArray([NyxNull, NyxObject([]), NyxData(False)])), '3');
  Equal(NewNyxJSONAggregate(NyxDataPath, njaSum).Aggregate(NyxArray([])), '0');
  Equal(NewNyxJSONAggregate(NyxDataPath, njaCount).Aggregate(NyxArray([])), '0');
  Equal(NewNyxJSONAggregate(NyxDataPath, njaAverage).Aggregate(NyxArray([])), 'null');
  Refuse(NewNyxJSONAggregate(NyxDataPath, njaMaximum, ndrExact, njaeError), Unary(NyxArray([])));
  Refuse(NewNyxJSONAggregate(NyxDataPath, njaSum), Unary(NyxArray([NyxNull])));
  Refuse(NewNyxJSONAggregate(NyxDataPath, njaAverage), Unary(TNyxDataValue.ParseJSON('[1,2,2]')));
  Equal(NewNyxJSONAggregate(NyxDataPath, njaAverage, ndrToEven)
    .Aggregate(TNyxDataValue.ParseJSON('[1,2,2]')), '1.6666666666666666666666666667');
  Equal(NewNyxJSONAggregate(NyxDataPath, njaAverage).Aggregate(TNyxDataValue.ParseJSON(
    '[79228162514264337593543950335,79228162514264337593543950335]')),
    '79228162514264337593543950335');
  Refuse(NewNyxJSONAggregate(NyxDataPath, njaSum), Unary(TNyxDataValue.ParseJSON(
    '[79228162514264337593543950335,1]')));
  Refuse(NewNyxJSONAggregate(NyxDataPath, njaMinimum), Unary(TNyxDataValue.ParseJSON('[1,"2"]')));
  Refuse(NewNyxJSONAggregate(NyxDataPath.Field('amount'), njaSum), Unary(NyxArray([NyxObject([])])));

  SetLength(LRows, 32);
  LInput := NyxObject([NyxField('id', NyxData(1)), NyxField('padding', NyxData(TNyxText(StringOfChar('x', 8192))))]);
  for LIndex := 0 to High(LRows) do
  begin
    LRows[LIndex] := LInput.Copy;
  end;
  LInput := NyxArray(LRows);
  LBefore := LInput.ToJSON;
  Refuse(NewNyxJSONJoin(NyxDataPath.Field('id'), NyxDataPath.Field('id'), njkDecimal), Partners(LInput, LInput));
  Check(LInput.ToJSON = LBefore, 'Cartesian output byte budget preserves both inputs');

  SetLength(LRows, 64);
  for LIndex := 0 to High(LRows) do
  begin
    LRows[LIndex] := NyxData(LIndex);
  end;
  LInput := NyxArray(LRows);
  for LIndex := 0 to 3 do
  begin
    case LIndex of
      0: LOperation := NewNyxJSONGroup(NyxDataPath, njkDecimal);
      1: LOperation := NewNyxJSONDistinct(NyxDataPath, njkDecimal);
      2: LOperation := NewNyxJSONJoin(NyxDataPath, NyxDataPath, njkDecimal);
      3: LOperation := NewNyxJSONAggregate(NyxDataPath, njaSum);
    end;
    Check(NyxLogicFromData(LOperation.Descriptor.Kind, LOperation.Configuration)
      .Descriptor.ToData.ToJSON = LOperation.Descriptor.ToData.ToJSON, 'Closed data factory retains exact schema');
    LScope := TCountingScope.Create;
    LScopePort := LScope;
    LScope.Limit := 80;

    if LIndex = 2 then
    begin
      LScope.Limit := 140;
    end;
    LFailed := False;
    try

      if LIndex = 2 then
      begin
        LOperation.Invoke(Partners(LInput, LInput), LScopePort);
      end
      else
      begin
        LOperation.Invoke(Unary(LInput), LScopePort);
      end;
    except
      on E: ENyxFlowCancelled do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (LScope.Reads > LScope.Limit), 'Cancellation occurs inside capture/lookup/numeric work');
  end;
end;

procedure Authoring;
var
  LDoc: TNyxDocument;
  LExpected: TNyxDocument;
  LParsed: TNyxDocument;
  LFormDoc: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LAgent: TNyxAgentSession;
  LPair: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LSource: TNyxText;
  LReply: TNyxDataValue;
  LArguments: TNyxDataValue;
  LDraft: TNyxLogicEditorDraft;
  LCopy: TNyxLogicEditorDraft;
  LCapture: TNyxLogicEditorCapture;
  LForm: INyxCard;
  LVersion: Integer;
  LFailed: Boolean;
  LIndex: Integer;
  LRun: INyxFlowResult;
  {$ifndef PAS2JS}LStream: TFileStream;{$endif}
begin
  LDoc := RelationDocument(ndrExact, njnReject);
  LExpected := nil;
  LParsed := nil;
  LFormDoc := nil;
  LWorkspace := nil;
  LAgent := nil;
  try
    LSource := TNyxCodegen.Generate(LDoc);
    LParsed := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LParsed) = TNyxCodec.Encode(LDoc), 'Whole crafted relational source admission');
    Check(LWorkspace.Render(LParsed) = LSource, 'No-op source stays exact');
    Check((Pos('NewNyxJSONJoin(NyxDataPath.Field(', LSource) > 0) and
      (Pos('njaSum, ndrExact, njaeNull', LSource) > 0), 'Crafted source uses typed paths and named enums');
    LRun := RelationGraph.Evaluate([
      NyxFlowInput(RelationEndpoint('orders', nlpText), NyxFlowText(RelationOrders)),
      NyxFlowInput(RelationEndpoint('customers', nlpText), NyxFlowText(RelationCustomers))]);
    Check(LRun.Value(RelationEndpoint('format-total', nlpText)).AsText = '9007199254740993.3',
      'Connected distinct/join/group/aggregate yields exact total');
    Check((LRun.Value(RelationEndpoint('join', nlpValue)).AsJSON.Count = 3) and
      (LRun.Value(RelationEndpoint('groups', nlpValue)).AsJSON.Item(0).Field('count').AsInteger = 2),
      'Connected relation outputs provide usable rows and first-region groups');

    LFormDoc := TNyxDocument.Create;
    LFormDoc.AddPage(NewNyxPage('form-home'));
    LForm := NewNyxLogicEditor('editor', LDoc, NyxFlowRef('Sales'));
    LFormDoc.Pages[0].Add(LForm);
    LForm.Node.Find('editor-operation').Configure.Value('json-join').Done;
    HandleNyxLogicEditor(LForm.Node.Find('editor-operation'), LFormDoc.Pages[0]);
    LForm.Node.Find('editor-path-part').Configure.Value('customer').Done;
    HandleNyxLogicEditor(LForm.Node.Find('editor-path-add'), LFormDoc.Pages[0]);
    LForm.Node.Find('editor-path-side').Configure.Value('Right key').Done;
    HandleNyxLogicEditor(LForm.Node.Find('editor-path-side'), LFormDoc.Pages[0]);
    LForm.Node.Find('editor-path-part').Configure.Value('id').Done;
    HandleNyxLogicEditor(LForm.Node.Find('editor-path-add'), LFormDoc.Pages[0]);
    LForm.Node.Find('editor-join-kind').Configure.Value('Full').Done;
    LForm.Node.Find('editor-null-keys').Configure.Value('Match null').Done;
    LForm.Node.Find('editor-step-name').Configure.Value('new-join').Done;
    Check(CaptureNyxLogicEditor(LForm.Node.Find('editor-add'), LFormDoc.Pages[0], LCapture),
      'Public form captures independent typed join paths');
    LDraft.Capture('editor', LFormDoc.Pages[0]);
    Check(LDraft.ToData.Field('version').AsInteger = 9, 'Relational fields/right path have a new draft version');
    LCopy := TNyxLogicEditorDraft.FromData(LDraft.ToData);
    Check(LCopy.ToData.ToJSON = LDraft.ToData.ToJSON, 'Draft keeps both paths and exact policy values');
    LForm.Node.Find('editor-path-side').Configure.Value('Left key').Done;
    HandleNyxLogicEditor(LForm.Node.Find('editor-path-side'), LFormDoc.Pages[0]);
    HandleNyxLogicEditor(LForm.Node.Find('editor-path-clear'), LFormDoc.Pages[0]);
    Check(LCopy.Restore('editor', LFormDoc.Pages[0]) and
      (LForm.Node.Find('editor-path-preview').Prop('text') = '["id"]'), 'Restore independent right preview after left mutation');

    LPair := NyxProjectPair(TNyxCodec.Encode(LDoc), LSource);
    LAgent := TNyxAgentSession.Create(LPair);
    LVersion := LAgent.Revision;
    LReply := LAgent.Call('nyx_logic', 'Relational qualification', NyxObject([
      NyxField('scope', NyxData('catalog')), NyxField('query', NyxData('join')),
      NyxField('limit', NyxData(1)), NyxField('expectedRevision', NyxData(LVersion))]));
    Check(LReply.Field('items').Count = 1, 'Intent discovery stays bounded');
    LArguments := NyxObject([NyxField('expectedRevision', NyxData(LVersion)),
      NyxField('operationId', NyxData('relational-policies')), NyxField('operations', NyxArray([
        TNyxDataValue.ParseJSON('{"op":"flow-step-set","flow":"Sales","step":"unique","kind":"json-distinct","configuration":{"path":["id"],"kind":0,"nulls":1}}'),
        TNyxDataValue.ParseJSON('{"op":"flow-step-set","flow":"Sales","step":"join","kind":"json-join","configuration":{"left":["customer"],"right":["id"],"kind":0,"join":0,"nulls":1}}'),
        TNyxDataValue.ParseJSON('{"op":"flow-step-set","flow":"Sales","step":"groups","kind":"json-group","configuration":{"path":["right","region"],"kind":0,"nulls":1}}'),
        TNyxDataValue.ParseJSON('{"op":"flow-step-set","flow":"Sales","step":"total","kind":"json-aggregate","configuration":{"path":["left","amount"],"aggregation":0,"rounding":1,"empty":0}}')]))]);
    LAgent.Call('nyx_transaction', 'Relational qualification', LArguments);
    Check(LAgent.Revision = LVersion + 1, 'Four semantic policies publish as one paired step');
    LAfter := LAgent.PreviewPair(LAgent.Revision, 'home');
    LAgent.Call('nyx_transaction', 'Relational qualification', LArguments);
    Check(LAgent.Revision = LVersion + 1, 'Retry is idempotent');
    LExpected := RelationDocument;
    Check(LAfter.Design = TNyxCodec.Encode(LExpected), 'Semantic result equals the expected whole practical application');
    LAgent.Call('nyx_history', 'Relational qualification', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)), NyxField('operationId', NyxData('relations-undo')),
      NyxField('direction', NyxData('undo'))]));
    Check(EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) = EncodeNyxProject(LPair), 'Exact paired semantic Undo');
    LAgent.Call('nyx_history', 'Relational qualification', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)), NyxField('operationId', NyxData('relations-redo')),
      NyxField('direction', NyxData('redo'))]));
    Check(EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) = EncodeNyxProject(LAfter), 'Exact paired semantic Redo');
    LVersion := LAgent.Revision;
    LFailed := False;
    try
      LAgent.Call('nyx_transaction', 'Relational qualification', NyxObject([
        NyxField('expectedRevision', NyxData(LVersion)), NyxField('operationId', NyxData('relations-malformed')),
        NyxField('operations', NyxArray([
          TNyxDataValue.ParseJSON('{"op":"flow-step-set","flow":"Sales","step":"join","kind":"json-join","configuration":{"left":["customer"],"right":["id"],"kind":0,"join":3,"nulls":1}}'),
          TNyxDataValue.ParseJSON('{"op":"flow-step-set","flow":"Sales","step":"total","kind":"json-aggregate","configuration":{"path":["left","amount"],"aggregation":0,"rounding":99,"empty":0}}')]))]));
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (LAgent.Revision = LVersion) and
      (EncodeNyxProject(LAgent.PreviewPair(LVersion, 'home')) = EncodeNyxProject(LAfter)),
      'Malformed rounding refuses the complete group');

    for LIndex := 0 to 1 do
    begin
      FreeAndNil(LWorkspace);
      FreeAndNil(LParsed);
      LSource := LAfter.Source;
      LParsed := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
      Check(TNyxCodec.Encode(LParsed) = LAfter.Design, 'Whole semantic-authored source exactly reconstructs its pair');
    end;
    {$ifndef PAS2JS}

    if ParamCount = 1 then
    begin
      ForceDirectories(ParamStr(1));
      LStream := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(1)) + 'nyx.generated.view.pas', fmCreate);
      try
        LStream.WriteBuffer(LSource[1], Length(LSource));
      finally
        LStream.Free;
      end;
    end;
    {$endif}
  finally
    LAgent.Free;
    LWorkspace.Free;
    LFormDoc.Free;
    LParsed.Free;
    LExpected.Free;
    LDoc.Free;
  end;
end;

begin
  try
    Core;
    Authoring;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-relations', 'passed');
    document.body.setAttribute('data-relations-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' relation/runtime/source/form/semantic checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-relations', 'failed');
      document.body.setAttribute('data-relations-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
