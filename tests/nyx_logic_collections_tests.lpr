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


program nyx_logic_collections_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.logic, nyx.logic.collections,
  nyx.logic.catalog, nyx.logic.editor, nyx.flow, nyx.flow.types, nyx.flow.editing,
  nyx.flow.declarations, nyx.scheduler, nyx.model, nyx.codec, nyx.codegen, nyx.source,
  nyx.controls, nyx.studio.session, nyx.studio.projects,
  nyx.test.logic.collections, nyx.test.logic.fixture
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

type
  { Counting cancellation proves checks inside work, not only Invoke's preflight.
    It owns no operation/UI and is used by one fixture invocation at a time. }
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
  Limit := 0;
end;

function TCountingScope.GetCancelled: Boolean;
begin
  Inc(Reads);
  Result := Reads > Limit;
end;

procedure Check(AValue: Boolean; const AMessage: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Collection Logic: ' + AMessage);
  end;
  Inc(GChecks);
end;

{$ifndef PAS2JS}
procedure WriteExact(const APath: String; const AText: TNyxText);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath, fmCreate);
  try

    if Length(AText) > 0 then
    begin
      LStream.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LStream.Free;
  end;
end;
{$endif}

procedure Run;
var
  LInput: TNyxDataValue;
  LResult: TNyxDataValue;
  LBefore: TNyxText;
  LFailure: Boolean;
  LOperation: INyxLogicOperation;
  LGraph: INyxFlowGraph;
  LRun: INyxFlowResult;
  LDoc: TNyxDocument;
  LParsed: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSource: TNyxText;
  LPair: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LSession: TNyxStudioSession;
  LFormDoc: TNyxDocument;
  LForm: INyxCard;
  LCapture: TNyxLogicEditorCapture;
  LDraft: TNyxLogicEditorDraft;
  LOldDraft: TNyxDataValue;
  LOldValues: array of TNyxDataField;
  LFields: TNyxDataValue;
  LLegacyContext: TNyxDataValue;
  LCatalog: TNyxDataValue;
  LScope: TCountingScope;
  LScopePort: INyxCancellationScope;
  LValues: TNyxFlowValues;
  LIndex: Integer;
begin
  LDoc := nil;
  LParsed := nil;
  LWorkspace := nil;
  LSession := nil;
  LFormDoc := nil;
  try
    LInput := TNyxDataValue.ParseJSON(CollectionLogicSource);
    LBefore := LInput.ToJSON;
    LResult := NewNyxJSONFilter(NyxDataPath.Field('active'), njcEqual, NyxData(True)).Filter(LInput);
    Check(LResult.Count = 3, 'typed Boolean filter');
    LResult := NewNyxJSONSort(NyxDataPath.Field('priority'), njkDecimal, njdDescending).Sort(LResult);
    Check(LResult.Item(0).Field('priority').ToJSON = '9007199254740993', 'sort exceeds IEEE exact integer range');
    LResult := NewNyxJSONSlice(0, 2).Slice(LResult);
    LResult := NewNyxJSONMap(NyxDataPath.Field('profile').Field('name')).Map(LResult);
    Check(LResult.ToJSON = NyxArray([NyxData('Ada'), NyxData('Grace')]).ToJSON,
      'direct filter/sort/page/map output: ' + LResult.ToJSON);
    Check(LInput.ToJSON = LBefore, 'direct calls preserve immutable source');
    LInput := TNyxDataValue.ParseJSON('[{"k":1.0,"id":"first"},{"k":1e0,"id":"second"},{"k":0,"id":"third"}]');
    LResult := NewNyxJSONSort(NyxDataPath.Field('k'), njkDecimal).Sort(LInput);
    Check((LResult.Item(1).Field('id').AsText = 'first') and
      (LResult.Item(2).Field('k').ToJSON = '1e0'), 'stable equivalent numeric keys preserve spelling and source ties');
    LResult := NewNyxJSONSort(NyxDataPath, njkText).Sort(
      NyxArray([NyxData('🐾'), NyxData('z'), NyxData('A'), NyxData('a' + TNyxText(#0))]));
    Check((LResult.Item(0).AsText = 'A') and (LResult.Item(3).AsText = TNyxText('🐾')),
      'portable ordinal sort follows Unicode scalars, not UTF-16 code units');
    Check(NewNyxJSONFilter(NyxDataPath, njcContains, NyxData('')).Filter(
      NyxArray([NyxData(''), NyxData('text')])).Count = 2, 'empty text search matches both targets consistently');
    Check(NewNyxJSONFilter(NyxDataPath, njcEqual, NyxNull).Filter(
      NyxArray([NyxNull, NyxData(False), NyxData('')])).Count = 1, 'null equality does not coerce false or empty text');
    Check(NewNyxJSONSlice(High(Integer), 2).Slice(LInput).Count = 0, 'large offset never overflows offset plus count');
    Check(NewNyxJSONSlice(0, 0).Slice(LInput).Count = 0, 'zero page is empty');
    LFailure := False;
    try
      NewNyxJSONSort(NyxDataPath, njkDecimal).Sort(NyxArray([NyxData(NyxDecimal('1e100'))]));
    except
      on E: Exception do
      begin
        LFailure := True;
      end;
    end;
    Check(LFailure, 'singleton exact-decimal overflow refuses before sorting');
    LFailure := False;
    try
      NewNyxJSONSort(NyxDataPath, njkText).Sort(NyxArray([NyxData(True)]));
    except
      on E: Exception do
      begin
        LFailure := True;
      end;
    end;
    Check(LFailure, 'singleton mismatched key family refuses');
    LFailure := False;
    try
      NewNyxJSONFilter(NyxDataPath.Field('missing'), njcEqual, NyxData(True)).Filter(LInput);
    except
      on E: Exception do
      begin
        LFailure := True;
      end;
    end;
    Check(LFailure, 'missing field refuses the complete result');
    LFailure := False;
    try
      NewNyxJSONSlice(0, 0).Slice(NyxNull);
    except
      on E: Exception do
      begin
        LFailure := True;
      end;
    end;
    Check(LFailure, 'empty page still requires array input');
    for LIndex := 0 to 3 do
    begin
      case LIndex of
        0: LOperation := NewNyxJSONFilter(NyxDataPath, njcGreater, NyxData(1));
        1: LOperation := NewNyxJSONSort(NyxDataPath, njkDecimal);
        2: LOperation := NewNyxJSONMap(NyxDataPath);
        3: LOperation := NewNyxJSONSlice(0, 10);
      end;
      Check(NyxLogicFromData(LOperation.Descriptor.Kind, LOperation.Configuration)
        .Configuration.ToJSON = LOperation.Configuration.ToJSON, 'closed operation configuration round trip');
      LScope := TCountingScope.Create;
      LScopePort := LScope;
      LScope.Limit := 12;
      LFailure := False;
      LValues := NyxFlowValues.WithValue(NyxLogicPort(nlpJSON), NyxFlowJSON(
        TNyxDataValue.ParseJSON('[3,2,1,4,5,6,7,8]')));
      try
        LOperation.Invoke(LValues, LScopePort);
      except
        on E: ENyxFlowCancelled do
        begin
          LFailure := True;
        end;
      end;
      Check(LFailure and (LScope.Reads > 12),
        'invocation cooperates with cancellation after capture inside transform work');
    end;
    LGraph := CollectionLogicGraph;
    LRun := LGraph.Evaluate([
      NyxFlowInput(LogicEndpoint('parse', nlpText), NyxFlowText(CollectionLogicSource)),
      NyxFlowInput(LogicEndpoint('summary', nlpLeft), NyxFlowText('Active people'))]);
    Check(LRun.Value(LogicEndpoint('compact', nlpText)).AsText = '[{"name":"Ada"},{"name":"Grace"}]',
      'connected graph executes all four operations before formatted output');
    LGraph := NyxFlowGraphFromData(LGraph.ToData);
    Check(LGraph.StepCount = 8, 'graph recovery resolves executable built-ins without creator authority');

    LDoc := CollectionLogicDocument;
    LSource := TNyxCodegen.Generate(LDoc);
    Check((Pos('NewNyxJSONFilter(', LSource) > 0) and (Pos('njkDecimal, njdDescending', LSource) > 0) and
      (Pos('NewNyxJSONMap(', LSource) > 0) and (Pos('NewNyxJSONSlice(0, 2)', LSource) > 0),
      'crafted source uses specialized typed constructors');
    LParsed := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LParsed) = TNyxCodec.Encode(LDoc), 'literal source rebuilds the whole exact document');
    Check(LWorkspace.Render(LParsed) = LSource, 'no-op source remains exact');
    LPair := NyxProjectPair(TNyxCodec.Encode(LDoc), LSource);
    LSession := TNyxStudioSession.Create(LPair);
    LSession.Select('title');
    LSession.SetProperty('text', 'Shape this data');
    LAfter := LSession.ProjectSnapshot;
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LPair), 'complete paired Undo');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAfter), 'complete paired Redo');

    LFormDoc := TNyxDocument.Create;
    LFormDoc.AddPage(NewNyxPage('form-home'));
    LForm := NewNyxLogicEditor('logic', LDoc, NyxFlowRef('Profile'));
    LFormDoc.Pages[0].Add(LForm);
    LForm.Node.Find('logic-operation').Configure.Value('json-sort').Done;
    HandleNyxLogicEditor(LForm.Node.Find('logic-operation'), LFormDoc.Pages[0]);
    Check((LForm.Node.Find('logic-creator-config').Prop('visible') = 'false') and
      (LForm.Node.Find('logic-key-kind').Prop('visible') = 'true'),
      'built-in configuration uses typed fields instead of extension JSON');
    LForm.Node.Find('logic-step-name').Configure.Value('sort-again').Done;
    LForm.Node.Find('logic-key-kind').Configure.Value('Exact decimal').Done;
    LForm.Node.Find('logic-direction').Configure.Value('Descending').Done;
    Check(CaptureNyxLogicEditor(LForm.Node.Find('logic-add'), LFormDoc.Pages[0], LCapture),
      'ordinary workbench captures a typed sort operation');
    Check(EditNyxFlows(LDoc.Flows, [LCapture.Change]).Definition(NyxFlowRef('Profile')).StepCount = 9,
      'workbench operation uses ordinary final-graph admission');
    LDraft.Capture('logic', LFormDoc.Pages[0]);
    Check(LDraft.ToData.Field('version').AsInteger = 2, 'new editor draft identifies its extra typed fields');
    LOldDraft := LDraft.ToData;
    LFields := LOldDraft.Field('values');
    SetLength(LOldValues, 17);
    for LIndex := 0 to 16 do
    begin
      LOldValues[LIndex] := NyxField(LFields.Key(LIndex), LFields.Field(LFields.Key(LIndex)));
    end;
    LLegacyContext := LOldDraft.Field('context');
    LCatalog := LLegacyContext.Field('catalog');
    LLegacyContext := NyxObject([NyxField('flow', LLegacyContext.Field('flow')),
      NyxField('baseline', LLegacyContext.Field('baseline')), NyxField('names', LLegacyContext.Field('names')),
      NyxField('catalog', NyxArray([LCatalog.Item(0), LCatalog.Item(1), LCatalog.Item(2), LCatalog.Item(3)])),
      NyxField('endpoints', LLegacyContext.Field('endpoints'))]);
    LOldDraft := NyxObject([NyxField('version', NyxData(1)), NyxField('context', LLegacyContext),
      NyxField('values', NyxObject(LOldValues)), NyxField('path', LOldDraft.Field('path')),
      NyxField('descriptor', LOldDraft.Field('descriptor'))]);
    LDraft := TNyxLogicEditorDraft.FromData(LOldDraft);
    Check(LDraft.ToData.ToJSON = LOldDraft.ToJSON, 'legacy draft retains its exact version/bytes');
    Check(LDraft.Restore('logic', LFormDoc.Pages[0]),
      'legacy fields restore across catalog growth without changing accepted graph guards');
    {$ifndef PAS2JS}

    if ParamCount > 0 then
    begin
      ForceDirectories(ParamStr(1));
      WriteExact(IncludeTrailingPathDelimiter(ParamStr(1)) + 'nyx.generated.view.pas', LSource);
      WriteExact(IncludeTrailingPathDelimiter(ParamStr(1)) + 'design.nyx', LPair.Design);
    end;
    {$endif}
  finally
    LFormDoc.Free;
    LSession.Free;
    LWorkspace.Free;
    LParsed.Free;
    LDoc.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-logic-collections', 'passed');
    document.body.setAttribute('data-logic-collections-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' collection Logic/source/history checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-logic-collections', 'failed');
      document.body.setAttribute('data-logic-collections-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
