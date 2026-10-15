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


program nyx_flow_join_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.numbers.decimal, nyx.scheduler,
  nyx.flow, nyx.flow.types, nyx.flow.declarations, nyx.flow.editing,
  nyx.logic, nyx.logic.joins, nyx.logic.catalog, nyx.model, nyx.controls, nyx.codec, nyx.codegen,
  nyx.source, nyx.logic.editor, nyx.studio.agents, nyx.studio.projects,
  nyx.test.flow.joins
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function Recipe: TNyxFlowJoinOptions;
begin
  Result := NyxFlowJoinOptions(nftText).Input(NyxFlowPort('primary'))
    .Input(NyxFlowPort('fallback'));
end;

{ Build malformed boundary inputs structurally, independently of target JSON
  whitespace. No production admission path or original value is mutated. }
function Rewrite(const AValue: TNyxDataValue; const AName: TNyxText;
  const AReplacement: TNyxDataValue): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LItems: array of TNyxDataValue;
  LIndex: Integer;
begin
  Result := AValue.Copy;

  if AValue.Kind = ndObject then
  begin
    SetLength(LFields, AValue.Count);
    for LIndex := 0 to AValue.Count - 1 do
    begin
      LFields[LIndex] := NyxField(AValue.Key(LIndex),
        Rewrite(AValue.Field(AValue.Key(LIndex)), AName, AReplacement));

      if AValue.Key(LIndex) = AName then
      begin
        LFields[LIndex].Value := AReplacement.Copy;
      end;
    end;
    Result := NyxObject(LFields);
  end
  else if AValue.Kind = ndArray then
  begin
    SetLength(LItems, AValue.Count);
    for LIndex := 0 to AValue.Count - 1 do
    begin
      LItems[LIndex] := Rewrite(AValue.Item(LIndex), AName, AReplacement);
    end;
    Result := NyxArray(LItems);
  end;
end;

procedure Refuse(const AJoin: INyxFlowJoin; const AFrame: TNyxFlowValues);
var
  LFailed: Boolean;
begin
  LFailed := False;
  try
    AJoin.Invoke(AFrame, nil);
  except
    on E: ENyxFlow do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'Invalid direct join invocation must refuse');
end;

procedure Values;
const
  CName: TNyxText = 'branch 🌙';
var
  LType: TNyxFlowType;
  LValue: TNyxFlowValue;
  LSelected: TNyxFlowValue;
  LSource: TNyxFlowPortRef;
  LJoin: INyxFlowJoin;
  LOptions: TNyxFlowJoinOptions;
  LCopy: TNyxFlowJoinOptions;
  LFrame: TNyxFlowValues;
  LFailed: Boolean;
  LIndex: Integer;
begin
  for LType := Low(TNyxFlowType) to High(TNyxFlowType) do
  begin
    case LType of
      nftText: LValue := NyxFlowText('');
      nftBoolean: LValue := NyxFlowBoolean(False);
      nftInteger: LValue := NyxFlowInteger(0);
      nftNumber: LValue := NyxFlowNumber(NyxDecimal('9007199254740993.0100'));
      nftJSON: LValue := NyxFlowJSON(NyxNull);
      nftBytes: LValue := NyxFlowBytes([]);
      nftSignal: LValue := NyxFlowSignal;
    end;
    LJoin := NewNyxFlowJoin(NyxFlowJoinOptions(LType).Input(NyxFlowPort(CName)));
    Check(LJoin.TrySelect(NyxFlowValues.WithValue(NyxFlowPort(CName), LValue),
      LSelected, LSource) and (LSource.Name = CName) and
      (LSelected.ToData.ToJSON = LValue.ToData.ToJSON), 'Presence preserves exact payload family and value');
    Check(not LJoin.TrySelect(NyxFlowValues, LSelected, LSource) and not LSource.Defined,
      'Absent input skips without inventing false/zero/null/signal');
  end;
  LFrame := NyxFlowValues.WithValue(NyxFlowPort('fallback'), NyxFlowText('B'))
    .WithValue(NyxFlowPort('primary'), NyxFlowText(TNyxText('A 🌙') + #0));
  Refuse(NewNyxFlowJoin(Recipe), LFrame);
  Refuse(NewNyxFlowJoin(Recipe.WhenEmpty(nfjeError)), NyxFlowValues);
  Refuse(NewNyxFlowJoin(Recipe), NyxFlowValues);
  Refuse(NewNyxFlowJoin(Recipe), NyxFlowValues.WithValue(NyxFlowPort('unknown'), NyxFlowText('x')));
  Refuse(NewNyxFlowJoin(Recipe), NyxFlowValues.WithValue(NyxFlowPort('primary'), NyxFlowBoolean(False)));
  LJoin := NewNyxFlowJoin(Recipe.Select(nfjFirstAvailable));
  Check(LJoin.TrySelect(LFrame, LSelected, LSource) and (LSource.Name = 'primary') and
    (LSelected.AsText = TNyxText('A 🌙') + #0), 'First uses declared order, never frame insertion order');
  LJoin := NewNyxFlowJoin(Recipe.Select(nfjLastAvailable));
  Check(LJoin.TrySelect(LFrame, LSelected, LSource) and (LSource.Name = 'fallback') and
    (LSelected.AsText = 'B'), 'Last uses declared order');
  LOptions := Recipe;
  LCopy := LOptions.Input(NyxFlowPort('third'));
  Check((LOptions.Count = 2) and (LCopy.Count = 3), 'Fluent option vectors have independent ownership');
  Check(TNyxFlowJoinOptions.FromData(LCopy.ToData).ToData.ToJSON = LCopy.ToData.ToJSON,
    'Typed options persist exactly');
  for LIndex := 0 to 3 do
  begin
    LFailed := False;
    try
      case LIndex of
        0: LCopy := LOptions.Input(NyxFlowPort('primary'));
        1: LCopy := LOptions.Input(NyxFlowPort('value'));
        2: LCopy := LOptions.Input(NyxFlowPort('source'));
        3: LCopy := LOptions.Input(NyxFlowPort('bad' + #0));
      end;
    except
      on E: ENyxFlow do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (LOptions.Count = 2), 'Duplicate/reserved/control names refuse before changing recipe');
  end;
  LOptions := NyxFlowJoinOptions(nftText);
  for LIndex := 1 to 14 do
  begin
    LOptions := LOptions.Input(NyxFlowPort('input-' + TNyxText(IntToStr(LIndex))));
  end;
  LJoin := NewNyxFlowJoin(LOptions);
  Check(LJoin.Descriptor.PortCount = 16, 'Fourteen alternatives leave the declared two output ports');
  LFailed := False;
  try
    LCopy := LOptions.Input(NyxFlowPort('overflow'));
  except
    LFailed := True;
  end;
  Check(LFailed and (LOptions.Count = 14), 'Alternative budget refuses atomically');
end;

procedure Graphs;
var
  LGraph: INyxFlowGraph;
  LResult: INyxFlowResult;
  LRestored: INyxFlowGraph;
  LJSON: TNyxText;
  LFailed: Boolean;
  LRegistry: INyxFlowDeclarations;
  LChanged: INyxFlowDeclarations;
  LDescriptor: TNyxLogicDescriptor;
  LIndex: Integer;
  LScope: INyxCancellationScope;

  function Evaluate(APrimary, AFallback: Boolean; const AText: TNyxText): INyxFlowResult;
  begin
    Result := LGraph.Evaluate([
      NyxFlowInput(JoinEndpoint('primary-gate', 'enabled'), NyxFlowBoolean(APrimary)),
      NyxFlowInput(JoinEndpoint('fallback-gate', 'enabled'), NyxFlowBoolean(AFallback)),
      NyxFlowInput(JoinEndpoint('primary', 'text'), NyxFlowText(AText)),
      NyxFlowInput(JoinEndpoint('fallback', 'text'), NyxFlowText('{"selected":"fallback"}'))]);
  end;

begin
  LGraph := JoinGraph;
  LJSON := LGraph.ToData.ToJSON;
  Check(LGraph.ToData.Field('version').AsInteger = 3, 'Optional inputs require graph version three');
  Check(LGraph.ToData.Field('conditions').Count = 2, 'Both Boolean dependencies remain explicit');
  LRestored := NyxFlowGraphFromData(LGraph.ToData);
  Check(LRestored.ToData.ToJSON = LJSON, 'Complete optional graph round trip is exact');
  LResult := Evaluate(True, False, 'null');
  Check((LResult.Value(JoinEndpoint('format', 'text')).AsText = 'null') and
    (LResult.Value(JoinEndpoint('choice', 'source')).AsText = 'primary'),
    'Selected JSON null is present and executes its required downstream formatter');
  Check(NyxFlowOutputSkipped(LResult, JoinEndpoint('fallback', 'json')) and
    not NyxFlowOutputSkipped(LResult, JoinEndpoint('choice', 'value')),
    'Skipped optional producer does not skip the join');
  LResult := Evaluate(False, True, '{invalid unselected JSON');
  Check((LResult.Value(JoinEndpoint('choice', 'source')).AsText = 'fallback') and
    (LResult.Value(JoinEndpoint('format', 'text')).AsText = '{"selected":"fallback"}'),
    'Unselected invalid branch never executes; available fallback is selected');
  LResult := Evaluate(False, False, '{invalid');
  Check(NyxFlowOutputSkipped(LResult, JoinEndpoint('choice', 'value')) and
    NyxFlowOutputSkipped(LResult, JoinEndpoint('format', 'text')),
    'Empty skip propagates genuine provenance to required descendants');
  for LIndex := 0 to 2 do
  begin
    LFailed := False;
    try
      case LIndex of
        0: LResult := Evaluate(True, True, '{}');
        1: LResult := Evaluate(True, True, '{invalid selected JSON');
        2:
        begin
          LGraph := JoinGraph(nfjExactlyOne, nfjeError);
          LResult := Evaluate(False, False, '{}');
        end;
      end;
    except
      on E: ENyxFlow do
      begin
        LFailed := True;

        if LIndex in [0, 2] then
        begin
          Check(Pos('Logic step choice:', E.Message) = 1,
            'Join policy diagnostics identify the rejecting join, not its preceding producer');
        end;
      end;
    end;
    Check(LFailed, 'Ambiguity, selected branch failure and empty-error never publish a successful prefix');
  end;
  LGraph := JoinGraph(nfjFirstAvailable);
  LResult := Evaluate(True, True, '{"selected":"primary"}');
  Check(LResult.Value(JoinEndpoint('choice', 'source')).AsText = 'primary',
    'First priority is independent of fallback-first topological registration');
  LGraph := JoinGraph(nfjLastAvailable);
  LResult := Evaluate(True, True, '{}');
  Check(LResult.Value(JoinEndpoint('choice', 'source')).AsText = 'fallback',
    'Last priority is deterministic');
  LScope := NewNyxCancellationScope;
  LScope.Cancel;
  LFailed := False;
  try
    LResult := LGraph.Evaluate([], LScope);
  except
    on E: ENyxFlowCancelled do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'Cancellation is never treated as a missing branch');
  LFailed := False;
  try
    LResult := NewNyxFlowGraph.WithStep(NyxFlowStep('first'),
      NewNyxFlowJoin(Recipe.WhenEmpty(nfjeError))).Build.Evaluate([]);
  except
    on E: ENyxFlow do
    begin
      LFailed := Pos('Logic step first:', E.Message) = 1;
    end;
  end;
  Check(LFailed, 'Initial admission diagnostics identify the exact join before any operation enters');

  LGraph := NewNyxFlowGraph.WithStep(NyxFlowStep('parse'), NewNyxJSONParse).Build;
  Check(LGraph.ToData.Field('version').AsInteger = 1, 'Required-only graphs retain legacy version one');
  LDescriptor := NewNyxJSONParse.Descriptor;
  Check(LDescriptor.ToData.Field('ports').Item(0).Count = 4,
    'Required ports retain the exact four-field legacy shape');
  LJSON := LGraph.ToData.ToJSON;
  LRegistry := NewNyxFlowDeclarations;
  LRegistry.Define(NyxFlowRef('Editing'), LGraph);
  LChanged := EditNyxFlows(LRegistry, [NyxAddFlowStep(NyxFlowRef('Editing'),
    NyxFlowStep('join'), NewNyxFlowJoin(Recipe))]);
  Check((LChanged.Definition(NyxFlowRef('Editing')).ToData.Field('version').AsInteger = 3) and
    (LRegistry.Definition(NyxFlowRef('Editing')).ToData.ToJSON = LJSON),
    'Adding the first optional step upgrades the detached graph');
  LChanged := EditNyxFlows(LChanged, [NyxRemoveFlowStep(NyxFlowRef('Editing'), NyxFlowStep('join'))]);
  Check(LChanged.Definition(NyxFlowRef('Editing')).ToData.ToJSON = LJSON,
    'Removing the last optional step restores exact legacy graph bytes');
  LChanged := EditNyxFlows(LRegistry, [NyxReplaceFlowStep(NyxFlowRef('Editing'),
    NyxFlowStep('parse'), NewNyxFlowJoin(Recipe))]);
  Check(LChanged.Definition(NyxFlowRef('Editing')).ToData.Field('version').AsInteger = 3,
    'Replacement recomputes the graph version');
  LChanged := EditNyxFlows(LChanged, [NyxReplaceFlowStep(NyxFlowRef('Editing'),
    NyxFlowStep('parse'), NewNyxJSONParse)]);
  Check(LChanged.Definition(NyxFlowRef('Editing')).ToData.ToJSON = LJSON,
    'Required replacement restores exact legacy graph');
  for LIndex := 0 to 2 do
  begin
    LFailed := False;
    LJSON := JoinGraph.ToData.ToJSON;
    case LIndex of
      0: LJSON := Rewrite(JoinGraph.ToData, 'version', NyxData(2)).ToJSON;
      1: LJSON := Rewrite(JoinGraph.ToData, 'optional', NyxData(False)).ToJSON;
      2: LJSON := Rewrite(JoinGraph.ToData, 'optional', NyxData('true')).ToJSON;
    end;
    Check(LJSON <> JoinGraph.ToData.ToJSON, 'Malformed-wire oracle actually changes the input');
    try
      LRestored := NyxFlowGraphFromData(TNyxDataValue.ParseJSON(LJSON));
    except
      LFailed := True;
    end;
    Check(LFailed, 'Legacy versions and malformed optional flags cannot admit optional graph meaning');
  end;
end;

procedure Authoring;
var
  LDocument: TNyxDocument;
  LExpected: TNyxDocument;
  LParsed: TNyxDocument;
  LFormDocument: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LForm: INyxCard;
  LSource: TNyxText;
  LAgent: TNyxAgentSession;
  LBefore: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LArguments: TNyxDataValue;
  LReply: TNyxDataValue;
  LDraft: TNyxLogicEditorDraft;
  LCopy: TNyxLogicEditorDraft;
  LCapture: TNyxLogicEditorCapture;
  LRevision: Integer;
  {$ifndef PAS2JS}LStream: TFileStream;{$endif}

  procedure Field(const AName, AValue: TNyxText);
  begin
    LForm.Node.Find('editor-' + AName).Configure.Value(AValue).Done;
  end;

  procedure Command(const AName: TNyxText);
  begin
    Check(HandleNyxLogicEditor(LForm.Node.Find('editor-' + AName), LFormDocument.Pages[0]),
      'Public join configuration command is recognized');
  end;

begin
  LDocument := JoinDocument(nfjFirstAvailable, nfjeError);
  LExpected := nil;
  LParsed := nil;
  LFormDocument := nil;
  LWorkspace := nil;
  LAgent := nil;
  try
    LSource := TNyxCodegen.Generate(LDocument);
    Check((Pos('NewNyxFlowJoin(NyxFlowJoinOptions(nftJSON)', LSource) > 0) and
      (Pos('.Select(nfjFirstAvailable)', LSource) > 0) and
      (Pos('.WhenEmpty(nfjeError)', LSource) > 0), 'Crafted source uses typed join recipes and named policies');
    LParsed := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check((TNyxCodec.Encode(LParsed) = TNyxCodec.Encode(LDocument)) and
      (LWorkspace.Render(LParsed) = LSource), 'Whole join application source/no-op reconstruction is exact');
    LFormDocument := TNyxDocument.Create;
    LFormDocument.AddPage(NewNyxPage('form-home'));
    LForm := NewNyxLogicEditor('editor', LDocument, NyxFlowRef('Branches'));
    LFormDocument.Pages[0].Add(LForm);
    Field('operation', 'flow-join');
    Command('operation');
    Field('branch-name', 'third');
    Command('branch-add');
    Command('branch-up');
    Check(LForm.Node.Find('editor-branch-preview').Prop('text') =
      TNyxDataValue.ParseJSON('["primary","third","fallback"]').ToJSON,
      'Priority controls preserve declared branch order');
    Field('confirm', 'Confirm removal');
    Command('branch-remove');
    Check(LForm.Node.Find('editor-branch-preview').Prop('text') =
      TNyxDataValue.ParseJSON('["primary","fallback"]').ToJSON,
      'Confirmed removal removes only the selected alternative');
    Field('branch-type', NyxFlowTypeName(nftJSON));
    Field('branch-selection', 'Last available');
    Field('branch-empty', 'Report error');
    Field('branch-name', 'unfinished new input');
    Field('step-name', 'new-choice');
    Check(CaptureNyxLogicEditor(LForm.Node.Find('editor-add'), LFormDocument.Pages[0], LCapture),
      'Public form captures typed optional join operation');
    LDraft.Capture('editor', LFormDocument.Pages[0]);
    Check(LDraft.ToData.Field('version').AsInteger = 10, 'Join controls have a versioned unfinished draft');
    LCopy := TNyxLogicEditorDraft.FromData(LDraft.ToData);
    Check(LCopy.ToData.ToJSON = LDraft.ToData.ToJSON, 'Draft retains exact branch recipe and unfinished input');
    Field('branch-name', 'changed');
    Check(LCopy.Restore('editor', LFormDocument.Pages[0]) and
      (LForm.Node.Find('editor-branch-name').Prop('value') = 'unfinished new input'),
      'Draft restore keeps unfinished text independently of the selected branch');

    LBefore := NyxProjectPair(TNyxCodec.Encode(LDocument), LSource);
    LAgent := TNyxAgentSession.Create(LBefore);
    LRevision := LAgent.Revision;
    LReply := LAgent.Call('nyx_logic', 'Branch qualification', NyxObject([
      NyxField('scope', NyxData('ports')), NyxField('flow', NyxData('Branches')),
      NyxField('step', NyxData('choice')), NyxField('limit', NyxData(2)),
      NyxField('expectedRevision', NyxData(LRevision))]));
    Check((LReply.Field('items').Count = 2) and LReply.Field('items').Item(0).Field('optional').AsBoolean,
      'Bounded semantic query exposes optional input capability');
    LArguments := NyxObject([NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('join-policies')), NyxField('operations', NyxArray([
        TNyxDataValue.ParseJSON('{"op":"flow-step-set","flow":"Branches","step":"choice","kind":"flow-join","configuration":{"type":4,"selection":0,"empty":0,"inputs":["primary","fallback"]}}')]))]);
    LAgent.Call('nyx_transaction', 'Branch qualification', LArguments);
    LAfter := LAgent.PreviewPair(LAgent.Revision, 'home');
    LExpected := JoinDocument;
    Check((LAgent.Revision = LRevision + 1) and (LAfter.Design = TNyxCodec.Encode(LExpected)),
      'Expected-revision semantic policy edit publishes the exact practical application');
    LAgent.Call('nyx_transaction', 'Branch qualification', LArguments);
    Check(LAgent.Revision = LRevision + 1, 'Semantic retry remains idempotent');
    LAgent.Call('nyx_history', 'Branch qualification', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)), NyxField('operationId', NyxData('join-undo')),
      NyxField('direction', NyxData('undo'))]));
    Check(EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) = EncodeNyxProject(LBefore),
      'Semantic Undo restores exact source/design');
    LAgent.Call('nyx_history', 'Branch qualification', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)), NyxField('operationId', NyxData('join-redo')),
      NyxField('direction', NyxData('redo'))]));
    Check(EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) = EncodeNyxProject(LAfter),
      'Semantic Redo restores exact source/design');
    FreeAndNil(LWorkspace);
    FreeAndNil(LParsed);
    LParsed := TNyxSourceWorkspace.PrepareDraft(LAfter.Source, LWorkspace);
    Check(TNyxCodec.Encode(LParsed) = LAfter.Design, 'Complete semantic source compiles to its exact model');
    {$ifndef PAS2JS}

    if ParamCount = 1 then
    begin
      ForceDirectories(ParamStr(1));
      LSource := LAfter.Source;
      LStream := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(1)) +
        'nyx.generated.view.pas', fmCreate);
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
    LFormDocument.Free;
    LParsed.Free;
    LExpected.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Values;
    Graphs;
    Authoring;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-flow-join', 'passed');
    document.body.setAttribute('data-flow-join-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' typed branch join/source/editor/semantic checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-flow-join', 'failed');
      document.body.setAttribute('data-flow-join-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
