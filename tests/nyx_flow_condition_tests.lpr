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

program nyx_flow_condition_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.scheduler, nyx.flow.types, nyx.flow,
  nyx.logic, nyx.flow.declarations, nyx.model, nyx.controls, nyx.codec,
  nyx.codegen, nyx.source, nyx.flow.editing, nyx.studio.edits,
  nyx.studio.projects, nyx.studio.session, nyx.studio.agents,
  nyx.test.flow.conditionfixture
  {$ifdef PAS2JS}, Web{$endif};

type
  { Deterministic creator operation owns one Boolean configuration. The graph
    retains it; the raw test observer is used only while that graph remains live.
    Execute counts prove that inactive branches never invoke their operations. }
  TGate = class(TNyxLogicOperation)
  private
    FValue: Boolean;
  protected
    function Execute(const AInputs: TNyxFlowValues;
      const ACancellation: INyxCancellationScope): TNyxFlowValues; override;
  public
    Calls: Integer;
    constructor Create(AValue: Boolean);
  end;

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AMessage: String);
begin

  if not AValue then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

function Endpoint(const AStep, APort: TNyxText): TNyxFlowEndpoint;
begin
  Result := NyxFlowEndpoint(NyxFlowStep(AStep), NyxFlowPort(APort));
end;

constructor TGate.Create(AValue: Boolean);
begin
  inherited Create;
  FValue := AValue;
  FDescriptor := NyxLogicDescriptor(NyxLogicKind('qualification-predicate'),
    'Predicate fixture', 'Pure Boolean observation for conditional graph qualification',
    nlgFlow, nlaPure).Output(NyxFlowPort('condition'), nftBoolean, 'Predicate value');
  FConfiguration := NyxObject([NyxField('value', NyxData(AValue))]);
end;

function TGate.Execute(const AInputs: TNyxFlowValues;
  const ACancellation: INyxCancellationScope): TNyxFlowValues;
begin
  Inc(Calls);
  Result := NyxFlowValues.WithValue(NyxFlowPort('condition'), NyxFlowBoolean(FValue));
end;

function Evaluate(const AGraph: INyxFlowGraph; const AText: TNyxText): INyxFlowResult;
begin
  Result := AGraph.Evaluate([NyxFlowInput(Endpoint('parse', 'text'), NyxFlowText(AText))]);
end;

procedure Run;
var
  LBase: INyxFlowGraph;
  LGraph: INyxFlowGraph;
  LOther: INyxFlowGraph;
  LResult: INyxFlowResult;
  LConditions: INyxFlowConditions;
  LGate: TGate;
  LAlternative: TGate;
  LData: TNyxDataValue;
  LDefinition: INyxFlowDefinition;
  LBefore: TNyxText;
  LRejected: Boolean;
  LDoc: TNyxDocument;
  LParsed: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSource: TNyxText;
  LIndex: Integer;
  LBadConditions: array[0..3] of TNyxDataValue;
  LMiddle: TGate;
  LTail: TGate;
  LRegistry: INyxFlowDeclarations;
  LChanged: INyxFlowDeclarations;
  LSession: TNyxStudioSession;
  LAgent: TNyxAgentSession;
  LPair: TNyxProjectPair;
  LAfter: TNyxText;
  LArguments: TNyxDataValue;
  LReply: TNyxDataValue;
  LRevision: Integer;
begin
  LGate := TGate.Create(False);
  LAlternative := TGate.Create(True);
  LBase := NewNyxFlowGraph
    .WithStep(NyxFlowStep('gate'), LGate)
    .WithStep(NyxFlowStep('parse'), NewNyxJSONParse)
    .WithStep(NyxFlowStep('pretty'), NewNyxJSONFormat)
    .WithStep(NyxFlowStep('alternative'), LAlternative)
    .Connect(Endpoint('parse', 'json'), Endpoint('pretty', 'json')).Build;
  LBefore := LBase.ToData.ToJSON;
  LConditions := NyxFlowConditions(LBase)
    .When(NyxFlowStep('parse'), Endpoint('gate', 'condition'))
    .When(NyxFlowStep('alternative'), Endpoint('gate', 'condition'), False);
  LGraph := LConditions.Build;
  LResult := Evaluate(LGraph, '{invalid');
  Check((LGate.Calls = 1) and (LAlternative.Calls = 1),
    'the opposite branch executes once in deterministic dependency order');
  Check(LResult.Count = 2, 'inactive branches produce no fabricated output values');
  Check(NyxFlowOutputSkipped(LResult, Endpoint('parse', 'json')) and
    NyxFlowOutputSkipped(LResult, Endpoint('pretty', 'text')),
    'condition refusal skips the dependent parser/formatter chain');
  Check(not NyxFlowOutputSkipped(LResult, Endpoint('alternative', 'condition')),
    'the selected opposite branch retains ordinary complete output semantics');
  Check(LBase.ToData.ToJSON = LBefore, 'conditional derivation preserves its sealed original graph');
  Check(LGraph.ToData.Field('version').AsInteger = 2,
    'only graphs with conditions use the extended wire version');
  LRejected := False;
  try
    LResult.Value(Endpoint('parse', 'json'));
  except
    LRejected := True;
  end;
  Check(LRejected, 'direct reads of a skipped output remain explicit absence');
  LRejected := False;
  try
    NyxFlowOutputSkipped(LResult, Endpoint('parse', 'misspelled'));
  except
    LRejected := True;
  end;
  Check(LRejected, 'skip provenance cannot conceal an invalid output name');
  LRejected := False;
  try
    NyxFlowOutputSkipped(LResult, Endpoint('missing', 'json'));
  except
    LRejected := True;
  end;
  Check(LRejected, 'skip provenance cannot conceal an unknown source step');
  LConditions.Always(NyxFlowStep('parse')).Always(NyxFlowStep('alternative'));
  Check(LConditions.Build.ToData.ToJSON = LBefore,
    'removing every condition restores exact legacy graph bytes');
  Check(LGraph.ToData.Field('version').AsInteger = 2,
    'later builder edits cannot alter an already sealed derived graph');
  LOther := NyxFlowConditions(LGraph)
    .When(NyxFlowStep('parse'), Endpoint('gate', 'condition'), False).Build;
  LResult := Evaluate(LOther, '{"count":9007199254740993,"text":"Hello 🌙"}');
  Check(not NyxFlowOutputSkipped(LResult, Endpoint('pretty', 'text')) and
    (Pos('9007199254740993', LResult.Value(Endpoint('pretty', 'text')).AsText) > 0),
    'selected parse/format branch preserves exact numeric and supplementary text');
  Check(LOther.ToData.Field('conditions').Count = 2,
    'When deliberately replaces one step condition without duplicate entries');
  LRejected := False;
  try
    LResult := Evaluate(LOther, '{invalid');
  except
    LRejected := True;
  end;
  Check(LRejected, 'a selected malformed branch fails rather than becoming a skip');
  LRejected := False;
  try
    LOther := NyxFlowConditions(LBase)
      .When(NyxFlowStep('gate'), Endpoint('alternative', 'condition'))
      .When(NyxFlowStep('alternative'), Endpoint('gate', 'condition')).Build;
  except
    LRejected := True;
  end;
  Check(LRejected and (LBase.ToData.ToJSON = LBefore),
    'Boolean dependency cycles refuse without mutating the original graph');
  LRejected := False;
  try
    NyxFlowConditions(LBase).When(NyxFlowStep('alternative'), Endpoint('parse', 'json'));
  except
    LRejected := True;
  end;
  Check(LRejected, 'JSON values never become implicit Boolean conditions');
  LRejected := False;
  try
    NyxFlowConditions(LBase).When(NyxFlowStep('missing'), Endpoint('gate', 'condition'));
  except
    LRejected := True;
  end;
  Check(LRejected, 'condition targets must be existing exact step identities');
  LRegistry := NewNyxFlowDeclarations;
  LRegistry.Define(NyxFlowRef('Paths'), LGraph);
  LBefore := LRegistry.ToData.ToJSON;
  LChanged := EditNyxFlows(LRegistry, [NyxSetFlowCondition(NyxFlowRef('Paths'),
    NyxFlowStep('gate'), Endpoint('alternative', 'condition')),
    NyxClearFlowCondition(NyxFlowRef('Paths'), NyxFlowStep('alternative'))]);
  Check((LChanged.ToData.ToJSON <> LBefore) and (LRegistry.ToData.ToJSON = LBefore),
    'grouped dependency edits may repair an intermediate condition cycle atomically');
  LRejected := False;
  try
    LChanged := EditNyxFlows(LRegistry, [NyxSetFlowCondition(NyxFlowRef('Paths'),
      NyxFlowStep('gate'), Endpoint('alternative', 'condition'))]);
  except
    LRejected := True;
  end;
  Check(LRejected and (LRegistry.ToData.ToJSON = LBefore),
    'an unrepaired condition cycle leaves the original declaration exact');
  LRejected := False;
  try
    LChanged := EditNyxFlows(LRegistry, [NyxRemoveFlowStep(NyxFlowRef('Paths'), NyxFlowStep('gate'))]);
  except
    LRejected := True;
  end;
  Check(LRejected, 'ordinary step removal cannot silently break condition dependencies');
  LChanged := EditNyxFlows(LRegistry, [NyxRemoveFlowStep(NyxFlowRef('Paths'), NyxFlowStep('gate'), True)]);
  Check(LChanged.Definition(NyxFlowRef('Paths')).ToData.Field('version').AsInteger = 1,
    'explicit dependency detachment removes conditions and restores legacy encoding');

  LMiddle := TGate.Create(True);
  LTail := TGate.Create(True);
  LOther := NewNyxFlowGraph.WithStep(NyxFlowStep('gate'), TGate.Create(False))
    .WithStep(NyxFlowStep('middle'), LMiddle)
    .WithStep(NyxFlowStep('tail'), LTail).Build;
  LOther := NyxFlowConditions(LOther)
    .When(NyxFlowStep('middle'), Endpoint('gate', 'condition'))
    .When(NyxFlowStep('tail'), Endpoint('middle', 'condition'), False).Build;
  LResult := LOther.Evaluate([]);
  Check((LResult.Count = 1) and (LMiddle.Calls = 0) and (LTail.Calls = 0) and
    NyxFlowOutputSkipped(LResult, Endpoint('tail', 'condition')),
    'missing predicate provenance skips both choices without inventing False');
  LDefinition := NyxFlowDefinitionFromData(LGraph.ToData);
  Check(LDefinition.ToData.ToJSON = LGraph.ToData.ToJSON,
    'inert saved definitions retain exact conditions and declaration ordering');
  Check(NyxFlowSignatureGraph(LDefinition).ToData.ToJSON = LDefinition.ToData.ToJSON,
    'signature-only derivation preserves exact wire meaning without runtime authority');
  LBadConditions[0] := NyxArray([]);
  LBadConditions[1] := NyxArray([NyxObject([
    NyxField('step', NyxData('parse')),
    NyxField('source', LGraph.ToData.Field('conditions').Item(0).Field('source')),
    NyxField('expected', NyxData('true'))])]);
  LBadConditions[2] := NyxArray([LGraph.ToData.Field('conditions').Item(0),
    LGraph.ToData.Field('conditions').Item(0)]);
  LBadConditions[3] := NyxArray([NyxObject([
    NyxField('step', NyxData('parse')),
    NyxField('source', LGraph.ToData.Field('conditions').Item(0).Field('source')),
    NyxField('expected', NyxData(True)), NyxField('extra', NyxData(True))])]);
  for LIndex := 0 to High(LBadConditions) do
  begin
    LRejected := False;
    LData := LGraph.ToData;
    try
      LDefinition := NyxFlowDefinitionFromData(NyxObject([
        NyxField('version', NyxData(2)), NyxField('limits', LData.Field('limits')),
        NyxField('steps', LData.Field('steps')), NyxField('wires', LData.Field('wires')),
        NyxField('conditions', LBadConditions[LIndex])]));
    except
      LRejected := True;
    end;
    Check(LRejected, 'malformed persisted conditions refuse atomically at the declared boundary');
  end;

  LDoc := TNyxDocument.Create;
  LParsed := nil;
  LWorkspace := nil;
  LSession := nil;
  LAgent := nil;
  try
    LDoc.AddPage(NewNyxPage('home').Add(NewNyxLabel('title').WithText('Choose a path')));
    LDoc.Flows.Define(NyxFlowRef('Conditional report'), LGraph);
    LSource := TNyxCodegen.Generate(LDoc);
    Check((Pos('NyxFlowConditions(', LSource) > 0) and (Pos('.When(', LSource) > 0),
      'crafted source exposes conditional intent through typed fluent derivation');
    LParsed := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LParsed) = TNyxCodec.Encode(LDoc),
      'conditional source admission preserves the complete inert document');
    Check(LWorkspace.Render(LParsed) = LSource, 'conditional no-op regeneration preserves exact source');
    FreeAndNil(LParsed);
    FreeAndNil(LWorkspace);
    LRejected := False;
    try
      LParsed := TNyxSourceWorkspace.PrepareDraft(StringReplace(LSource,
        ', True)', ', ''true'')', []), LWorkspace);
    except
      LRejected := True;
    end;
    Check(LRejected, 'raw behavior strings cannot replace typed Boolean source conditions');
    FreeAndNil(LParsed);
    FreeAndNil(LWorkspace);
    LPair := NyxProjectPair(TNyxCodec.Encode(LDoc), LSource);
    LBefore := EncodeNyxProject(LPair);
    LSession := TNyxStudioSession.Create(LPair);
    LSession.ApplyPatch(NyxFlowPatch([NyxClearFlowCondition(NyxFlowRef('Conditional report'),
      NyxFlowStep('parse')).Expect(NyxFlowBaseline(LDoc.Flows, NyxFlowRef('Conditional report')))]));
    LAfter := EncodeNyxProject(LSession.ProjectSnapshot);
    Check(LAfter <> LBefore, 'ordinary typed condition editing publishes a complete paired source/design command');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LBefore, 'ordinary condition Undo restores exact accepted bytes');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LAfter, 'ordinary condition Redo restores the complete changed pair');
    LAgent := TNyxAgentSession.Create(LPair);
    LRevision := LAgent.Revision;
    LReply := LAgent.Call('nyx_logic', 'Conditional qualification', NyxObject([
      NyxField('scope', NyxData('conditions')), NyxField('flow', NyxData('Conditional report')),
      NyxField('offset', NyxData(1)), NyxField('limit', NyxData(1)),
      NyxField('expectedRevision', NyxData(LRevision))]));
    Check((LReply.Field('total').AsInteger = 2) and (LReply.Field('items').Count = 1) and
      not LReply.Field('items').Item(0).Field('expected').AsBoolean,
      'revision-pinned semantic condition paging returns only the requested row');
    LArguments := NyxObject([NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('choose-opposite-path')),
      NyxField('operations', NyxArray([NyxSetFlowCondition(NyxFlowRef('Conditional report'),
        NyxFlowStep('parse'), Endpoint('gate', 'condition'), False)
          .Expect(NyxFlowBaseline(LDoc.Flows, NyxFlowRef('Conditional report'))).ToData]))]);
    LAgent.Call('nyx_transaction', 'Conditional qualification', LArguments);
    Check(LAgent.Revision = LRevision + 1, 'semantic condition editing publishes one guarded revision');
    LAfter := EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home'));
    LAgent.Call('nyx_transaction', 'Conditional qualification', LArguments);
    Check(LAgent.Revision = LRevision + 1, 'exact conditional retry creates no duplicate history entry');
    LAgent.Call('nyx_history', 'Conditional qualification', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData('undo-path')), NyxField('direction', NyxData('undo'))]));
    Check(EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) = LBefore,
      'semantic Undo restores exact original source and design');
    LAgent.Call('nyx_history', 'Conditional qualification', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData('redo-path')), NyxField('direction', NyxData('redo'))]));
    Check(EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) = LAfter,
      'semantic Redo restores the complete conditional pair');
  finally
    LAgent.Free;
    LSession.Free;
    LWorkspace.Free;
    LParsed.Free;
    LDoc.Free;
  end;
end;

procedure QualifyFileSource;
var
  LDoc: TNyxDocument;
  LParsed: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSource: TNyxText;
  {$ifndef PAS2JS}LFile: TFileStream; LBytes: TNyxBytes;{$endif}
begin
  LDoc := ConditionalFileDocument;
  LParsed := nil;
  LWorkspace := nil;
  try
    LSource := TNyxCodegen.Generate(LDoc);
    Check((Pos('NyxFlowConditions(', LSource) > 0) and
      (Pos('NewNyxOpenFile(', LSource) > 0) and
      (Pos('SetNyxFlowRoutes(', LSource) > 0),
      'crafted file companion retains typed conditions, opening and ordinary saved event routes');
    LParsed := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LParsed) = TNyxCodec.Encode(LDoc),
      'complete saved conditional file route survives exact source admission');
    Check(LWorkspace.Render(LParsed) = LSource,
      'conditional file companion remains byte-stable under no-op source rendering');
    {$ifndef PAS2JS}

    if ParamCount = 1 then
    begin

      if DirectoryExists(ParamStr(1)) then
      begin
        raise Exception.Create('Conditional companion export requires a fresh owned directory');
      end;
      ForceDirectories(ParamStr(1));
      LBytes := NyxEncodeUTF8(LSource);
      LFile := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(1)) +
        'nyx.generated.view.pas', fmCreate);
      try
        LFile.WriteBuffer(LBytes[0], Length(LBytes));
      finally
        LFile.Free;
      end;
    end;
    {$endif}
  finally
    LWorkspace.Free;
    LParsed.Free;
    LDoc.Free;
  end;
end;

begin
  try
    Run;
    QualifyFileSource;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-flow-conditions', 'passed');
    document.body.setAttribute('data-flow-condition-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' conditional flow checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-flow-conditions', 'failed');
      document.body.setAttribute('data-flow-condition-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
