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
program nyx_logic_authoring_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.model, nyx.codec, nyx.codegen,
  nyx.flow.types, nyx.flow, nyx.flow.declarations, nyx.flow.editing, nyx.scheduler,
  nyx.logic, nyx.logic.catalog, nyx.logic.editor, nyx.controls,
  nyx.studio.session, nyx.studio.projects, nyx.studio.edits, nyx.studio.agents,
  nyx.studio.presentation,
  nyx.test.logic.fixture, nyx.test.logic.document
  {$ifndef PAS2JS}, nyx.studio.mcp{$endif}
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

type
  { Retire the caller's const-interface variables from a creator getter. The
    library must own independent leases until every copied value is captured. }
  TRetiringOperation = class(TInterfacedObject, INyxLogicOperation)
  private
    FConfiguration: TNyxDataValue;
  public
    constructor Create;
    destructor Destroy; override;
    function GetDescriptor: TNyxLogicDescriptor;
    function GetConfiguration: TNyxDataValue;
    function Invoke(const AInputs: TNyxFlowValues;
      const ACancellation: INyxCancellationScope): TNyxFlowValues;
  end;

var
  GChecks: Integer;
  GCreator: INyxLogicOperation;
  GCreatorCatalog: INyxLogicCatalog;
  GDisposed: Integer;

constructor TRetiringOperation.Create;
begin
  inherited Create;
  FConfiguration := NyxData('Retained creator');
end;

destructor TRetiringOperation.Destroy;
begin
  Inc(GDisposed);
  inherited Destroy;
end;

function TRetiringOperation.GetDescriptor: TNyxLogicDescriptor;
begin
  GCreator := nil;
  GCreatorCatalog := nil;
  Result := NyxLogicDescriptor(NyxLogicKind('retiring-creator'), 'Retained creator',
    'Getter retires caller references', nlgData).Output(NyxFlowPort('value'), nftText, 'Owned text');
end;

function TRetiringOperation.GetConfiguration: TNyxDataValue;
begin
  Result := FConfiguration.Copy;
end;

function TRetiringOperation.Invoke(const AInputs: TNyxFlowValues;
  const ACancellation: INyxCancellationScope): TNyxFlowValues;
begin
  Result := NyxFlowValues;
  raise ENyxFlow.Create('Metadata qualification never executes creator code');
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function Endpoint(const AStep: TNyxText; APort: TNyxLogicPort): TNyxFlowEndpoint;
begin
  Result := NyxFlowEndpoint(NyxFlowStep(AStep), NyxLogicPort(APort));
end;

procedure Run;
var
  LDoc: TNyxDocument;
  LSession: TNyxStudioSession;
  LAgent: TNyxAgentSession;
  LFlows: INyxFlowDeclarations;
  LChanged: INyxFlowDeclarations;
  LCatalog: INyxLogicCatalog;
  LOtherCatalog: INyxLogicCatalog;
  LReference: TNyxFlowRef;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LResponse: TNyxDataValue;
  LArguments: TNyxDataValue;
  LPair: TNyxProjectPair;
  LResult: INyxFlowResult;
  LRefused: Boolean;
  LForm: INyxCard;
  LRoot: TNyxDocument;
  LCapture: TNyxLogicEditorCapture;
  LDraft: TNyxLogicEditorDraft;
  LPristineDraft: TNyxLogicEditorDraft;
  LRevision: Integer;
  LChange: TNyxFlowChange;
  LPresentation: TNyxStudioPresentation;
  {$ifndef PAS2JS}LFile: TFileStream; LSource: TNyxText;{$endif}
begin
  LDoc := LogicDocument;
  LSession := nil;
  LAgent := nil;
  LRoot := nil;
  try
    LReference := NyxFlowRef('Profile');
    GDisposed := 0;
    GCreator := TRetiringOperation.Create;
    LChange := NyxAddFlowStep(LReference, NyxFlowStep('retained'), GCreator);
    Check((GCreator = nil) and (GDisposed = 1) and
      (LChange.ToData.Field('configuration').AsText = 'Retained creator'),
      'typed edit capture retains a const creator input through retiring getters');
    GCreator := TRetiringOperation.Create;
    GCreatorCatalog := NewNyxLogicCatalog;
    LOtherCatalog := GCreatorCatalog.WithOperation(GCreator);
    Check((GCreatorCatalog = nil) and (GCreator = nil) and (GDisposed = 2) and
      (LOtherCatalog.Configuration(LOtherCatalog.Count - 1).AsText = 'Retained creator'),
      'discovery capture retains both receiver and creator through caller retirement');
    LDoc.Flows.Define(LReference, LogicFixture);
    LCatalog := NewNyxLogicCatalog;
    Check((LCatalog.Count = 24) and LCatalog.Matches(2, 'format json') and
      not LCatalog.Matches(2, 'format network'), 'Logic discovery uses intent/help and AND terms');
    LOtherCatalog := LCatalog.WithOperation(NewNyxLogicDeclaration(
      NyxLogicDescriptor(NyxLogicKind('creator-copy'), 'Copy data', 'Creator-owned intent', nlgData)
        .Input(NyxFlowPort('in'), nftJSON, 'Input').Output(NyxFlowPort('out'), nftJSON, 'Output')
        .Labelled('custom'), NyxObject([])));
    Check((LCatalog.Count = 24) and (LOtherCatalog.Count = 25) and
      LOtherCatalog.Matches(LOtherCatalog.Count - 1, 'custom creator'),
      'creator discovery is independent and searchable');
    LOtherCatalog := LCatalog.WithOperation(NewNyxTextJoin('Changed'));
    Check((LCatalog.Configuration(3).Field('separator').AsText = '') and
      (LOtherCatalog.Configuration(3).Field('separator').AsText = 'Changed'),
      'replacing creator defaults preserves the original catalog on both targets');
    LFlows := NewNyxFlowDeclarations;
    LFlows.Define(LReference, NewNyxFlowGraph
      .WithStep(NyxFlowStep('A'), NewNyxJSONParse)
      .WithStep(NyxFlowStep('B'), NewNyxJSONFormat)
      .Connect(Endpoint('A', nlpJSON), Endpoint('B', nlpJSON)).Build);
    LBefore := LFlows.ToData.ToJSON;
    LChanged := EditNyxFlows(LFlows, [
      NyxReplaceFlowStep(LReference, NyxFlowStep('A'), NewNyxJSONFormat),
      NyxReplaceFlowStep(LReference, NyxFlowStep('B'), NewNyxJSONParse),
      NyxConnectFlow(LReference, Endpoint('A', nlpText), Endpoint('B', nlpText)),
      NyxDisconnectFlow(LReference, Endpoint('A', nlpJSON), Endpoint('B', nlpJSON))]);
    Check(LFlows.ToData.ToJSON = LBefore, 'grouped graph staging never mutates accepted membership');
    LResult := LChanged.Definition(LReference).Build.Evaluate([
      NyxFlowInput(Endpoint('A', nlpJSON), NyxFlowJSON(
        TNyxDataValue.ParseJSON('{"exact":9007199254740993,"text":"Exact 🐾"}')))]);
    Check(LResult.Value(Endpoint('B', nlpJSON)).AsJSON.Field('exact').ToJSON =
      '9007199254740993', 'final graph admits repaired intermediate signatures and executes exact results');
    LRefused := False;
    try
      EditNyxFlows(LFlows, [NyxRemoveFlowStep(LReference, NyxFlowStep('A'))]);
    except
      on E: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LFlows.ToData.ToJSON = LBefore), 'connected step removal requires explicit detachment');
    LChanged := EditNyxFlows(LFlows, [NyxRemoveFlowStep(LReference, NyxFlowStep('A'), True)]);
    Check((LChanged.Definition(LReference).StepCount = 1) and
      (LChanged.Definition(LReference).WireCount = 0), 'explicit removal detaches only owned wires');
    LRefused := False;
    try
      EditNyxFlows(LFlows, [NyxConnectFlow(LReference, Endpoint('B', nlpText), Endpoint('A', nlpText))]);
    except
      on E: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LFlows.ToData.ToJSON = LBefore), 'whole cyclic candidate refuses without partial publication');
    LPair := NyxProjectPair(TNyxCodec.Encode(LDoc), TNyxCodegen.Generate(LDoc));
    LSession := TNyxStudioSession.Create(LPair);
    LBefore := EncodeNyxProject(LPair);
    LSession.ApplyPatch(NyxFlowPatch([
      NyxAddFlowStep(LReference, NyxFlowStep('extra'), NewNyxJSONFormat(njlCompact, 4)),
      NyxConnectFlow(LReference, Endpoint('parse', nlpJSON), Endpoint('extra', nlpJSON))]));
    LAfter := EncodeNyxProject(LSession.ProjectSnapshot);
    Check(LBefore <> LAfter, 'ordinary grouped Logic edit publishes paired source/design');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LBefore, 'one Undo restores exact grouped graph pair');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LAfter, 'one Redo restores exact grouped graph pair');
    LSession.SetSourceDraft(LSession.Source + #10 + '// unfinished');
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    LRefused := False;
    try
      LSession.ApplyPatch(NyxFlowPatch([NyxRemoveFlow(LReference)]));
    except
      on E: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (EncodeNyxProject(LSession.ProjectSnapshot) = LBefore),
      'pending source preserves complete pair/draft/history on grouped refusal');
    LSession.SetSourceDraft(LSession.Source);
    LRoot := TNyxDocument.Create;
    LRoot.AddPage(NewNyxPage('editor-home'));
    LForm := NewNyxLogicEditor('logic', LDoc, LReference, LOtherCatalog);
    LRoot.Pages[0].Add(LForm);
    LDraft.Clear;
    LPristineDraft.Clear;
    Check(LDraft.SameProposal(LPristineDraft), 'two absent proposals compare without reading absent data');
    LPristineDraft.Capture('logic', LRoot.Pages[0]);
    Check(not LDraft.SameProposal(LPristineDraft), 'an absent proposal differs from captured controls');
    LForm.Node.Find('logic-search').Configure.Value('JSON').Done;
    LForm.Node.Find('logic-group').Configure.Value('JSON').Done;
    LForm.Node.Find('logic-grouped').Configure.Value('Grouped').Done;
    LDraft.Capture('logic', LRoot.Pages[0]);
    Check(LDraft.SameProposal(LPristineDraft), 'discovery presentation alone is not an unfinished operation');
    LForm.Node.Find('logic-step-name').Configure.Value('extra').Done;
    LForm.Node.Find('logic-operation').Configure.Value('json-format').Done;
    LForm.Node.Find('logic-indent').Configure.Value('four?').Done;
    LDraft.Capture('logic', LRoot.Pages[0]);
    Check(not LDraft.SameProposal(LPristineDraft), 'malformed typed configuration remains an unfinished proposal');
    Check(LDraft.SameProposal(TNyxLogicEditorDraft.FromData(LDraft.ToData)),
      'proposal comparison retains exact meaning after its existing wire round trip');
    LPresentation := DefaultNyxStudioPresentation;
    LBefore := EncodeNyxStudioPresentation(LPresentation);
    Check(TNyxDataValue.ParseJSON(LBefore).Field('version').AsInteger = 16,
      'unused Logic retains exact preceding presentation version');
    LPresentation.LogicVisible := True;
    LPresentation.LogicReference := LReference;
    LPresentation.LogicDraft := LDraft;
    LBefore := EncodeNyxStudioPresentation(LPresentation);
    LPresentation := DecodeNyxStudioPresentation(LBefore);
    Check((EncodeNyxStudioPresentation(LPresentation) = LBefore) and
      (LPresentation.LogicDraft.ToData.ToJSON = LDraft.ToData.ToJSON),
      'project presentation retains exact unfinished Logic fields and baseline');
    LRefused := False;
    try
      CaptureNyxLogicEditor(LForm.Node.Find('logic-add'), LRoot.Pages[0], LCapture);
    except
      on E: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LForm.Node.Find('logic-indent').Prop('value') = 'four?'),
      'invalid typed input remains an unfinished editor proposal');
    LForm.Node.Find('logic-indent').Configure.Value('4').Done;
    Check(LDraft.Restore('logic', LRoot.Pages[0]) and
      (LForm.Node.Find('logic-indent').Prop('value') = 'four?'), 'exact form baseline restores unsubmitted text');
    Check(LForm.Node.Find('logic-group-' + TNyxText(IntToStr(Ord(nlgText)))).Prop('visible') = 'false',
      'complete restoration filters discovery even when raw filter controls already match');
    LForm.Node.Find('logic-search').Configure.Value('').Done;
    LForm.Node.Find('logic-group').Configure.Value('All groups').Done;
    LForm.Node.Find('logic-grouped').Configure.Value('Flat').Done;
    HandleNyxLogicEditor(LForm.Node.Find('logic-search'), LRoot.Pages[0]);
    Check(LForm.Node.Find('logic-group-' + TNyxText(IntToStr(Ord(nlgText)))).Prop('visible') = 'true',
      'ordinary filter input restores visible text operations');
    Check(LDraft.Restore('logic', LRoot.Pages[0], nlrmCurrentPresentation) and
      (LForm.Node.Find('logic-group-' + TNyxText(IntToStr(Ord(nlgText)))).Prop('visible') = 'false'),
      'current-presentation restoration still reapplies changed filter controls');
    LForm.Node.Find('logic-indent').Configure.Value('4').Done;
    Check(CaptureNyxLogicEditor(LForm.Node.Find('logic-add'), LRoot.Pages[0], LCapture),
      'public workbench captures a typed guarded operation');
    LChanged := EditNyxFlows(LDoc.Flows, [LCapture.Change]);
    Check(LChanged.Definition(LReference).StepCount = 6, 'captured workbench operation is functional graph data');
    LRefused := False;
    try
      EditNyxFlows(LChanged, [LCapture.Change]);
    except
      on E: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'stale editor baseline refuses even when identities remain');
    LForm.Node.Find('logic-step').Configure.Value('parse').Done;
    LRefused := False;
    try
      CaptureNyxLogicEditor(LForm.Node.Find('logic-remove-step'), LRoot.Pages[0], LCapture);
    except
      on E: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'workbench removal requires visible warning acknowledgement');
    LForm.Node.Find('logic-operation').Configure.Value('json-select').Done;
    LForm.Node.Find('logic-path-part').Configure.Value('literal.field').Done;
    HandleNyxLogicEditor(LForm.Node.Find('logic-path-add'), LRoot.Pages[0]);
    LForm.Node.Find('logic-path-kind').Configure.Value('Item').Done;
    LForm.Node.Find('logic-path-part').Configure.Value('2').Done;
    HandleNyxLogicEditor(LForm.Node.Find('logic-path-add'), LRoot.Pages[0]);
    CaptureNyxLogicEditor(LForm.Node.Find('logic-add'), LRoot.Pages[0], LCapture);
    LResponse := LCapture.Change.ToData.Field('configuration').Field('path');
    Check((LResponse.Count = 2) and (LResponse.Item(0).AsText = 'literal.field') and
      (LResponse.Item(1).AsInteger = 2),
      'typed field/item controls preserve structural paths without dotted parsing');
    LDoc.Flows.Define(NyxFlowRef('Creator'), NewNyxFlowGraph
      .WithStep(NyxFlowStep('one'), NewNyxLogicDeclaration(
        NyxLogicDescriptor(NyxLogicKind('creator-step'), 'Creator step', 'Variable schema', nlgData)
          .Output(NyxFlowPort('first'), nftJSON, 'First result'), NyxData(1)))
      .WithStep(NyxFlowStep('two'), NewNyxLogicDeclaration(
        NyxLogicDescriptor(NyxLogicKind('creator-step'), 'Creator step', 'Variable schema', nlgData)
          .Output(NyxFlowPort('second'), nftJSON, 'Second result'), NyxData(2)))
      .Build);
    LForm := NewNyxLogicEditor('creator', LDoc, NyxFlowRef('Creator'));
    LRoot.Pages[0].Add(LForm);
    LForm.Node.Find('creator-step').Configure.Value('two').Done;
    Check(HandleNyxLogicEditor(LForm.Node.Find('creator-load'), LRoot.Pages[0]),
      'imported creator configuration loads without runtime authority');
    CaptureNyxLogicEditor(LForm.Node.Find('creator-set'), LRoot.Pages[0], LCapture);
    Check(LCapture.Change.ToData.Field('descriptor').Field('ports').Item(0).Field('name').AsText =
      'second', 'loading a creator step retains its own configuration-dependent schema');
    LAgent := TNyxAgentSession.Create(LPair);
    LAgent.InheritPermission(apEdit);
    LRevision := LAgent.Revision;
    LResponse := LAgent.Call('nyx_logic', 'Logic review',
      NyxObject([NyxField('scope', NyxData('steps')), NyxField('flow', NyxData('Profile')),
        NyxField('limit', NyxData(2)), NyxField('expectedRevision', NyxData(LRevision))]));
    Check((LResponse.Field('items').Count = 2) and (LResponse.Field('total').AsInteger = 5) and
      (LAgent.Revision = LRevision), 'semantic neighborhood is paged and revision-pinned without mutation');
    LResponse := LAgent.Call('nyx_logic', 'Logic review',
      NyxObject([NyxField('scope', NyxData('ports')), NyxField('flow', NyxData('Profile')),
        NyxField('step', NyxData('parse')), NyxField('limit', NyxData(1))]));
    Check((LResponse.Field('items').Count = 1) and
      (LResponse.Field('items').Item(0).Field('type').AsText = 'Text'),
      'semantic ports expose named families without document payloads');
    LResponse := LAgent.Call('nyx_logic', 'Logic review',
      NyxObject([NyxField('scope', NyxData('configuration')), NyxField('flow', NyxData('Profile')),
        NyxField('step', NyxData('pretty')), NyxField('textLimit', NyxData(5))]));
    Check(LResponse.Field('truncated').AsBoolean and
      (Length(LResponse.Field('text').AsText) = 5), 'configuration inspection is a bounded scalar window');
    LArguments := NyxObject([NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('logic-group')),
      NyxField('operations', NyxArray([
        TNyxDataValue.ParseJSON('{"op":"flow-step-add","flow":"Profile","step":"extra","kind":"json-format","configuration":{"layout":0,"indent":2}}'),
        NyxConnectFlow(LReference, Endpoint('parse', nlpJSON), Endpoint('extra', nlpJSON)).ToData]))]);
    LResponse := LAgent.Call('nyx_transaction', 'Logic review', LArguments);
    Check(LAgent.Revision = LRevision + 1, 'semantic operation and connection publish as one revision');
    LAgent.Call('nyx_transaction', 'Logic review', LArguments);
    Check(LAgent.Revision = LRevision + 1, 'exact semantic retry does not duplicate steps or history');
    LResponse := LAgent.Call('nyx_history', 'Logic review', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)), NyxField('operationId', NyxData('logic-undo')),
      NyxField('direction', NyxData('undo'))]));
    Check(EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) = EncodeNyxProject(LPair),
      'semantic Undo restores exact accepted source and graph design');
    {$ifndef PAS2JS}
    Check(Pos('nyx_logic', NyxStudioMCPTools.ToJSON) > 0, 'MCP advertises the focused Logic query contract');
    {$endif}
    {$ifndef PAS2JS}

    if ParamCount = 1 then
    begin
      ForceDirectories(ParamStr(1));
      LSource := TNyxCodegen.Generate(LDoc);
      LFile := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(1)) + 'nyx.generated.view.pas', fmCreate);
      try
        LFile.WriteBuffer(LSource[1], Length(LSource));
      finally
        LFile.Free;
      end;
    end;
    {$endif}
  finally
    LRoot.Free;
    LAgent.Free;
    LSession.Free;
    LDoc.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-logic-authoring', 'passed');
    document.body.setAttribute('data-logic-authoring-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' Logic authoring checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-logic-authoring', 'failed');
      document.body.setAttribute('data-logic-authoring-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
