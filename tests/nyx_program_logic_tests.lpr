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

program nyx_program_logic_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.types, nyx.model, nyx.state,
  nyx.logic, nyx.logic.programs, nyx.logic.programs.editor, nyx.logic.editor,
  nyx.logic.catalog, nyx.controls, nyx.flow, nyx.flow.types, nyx.flow.declarations,
  nyx.programs, nyx.programs.types, nyx.programs.contract, nyx.programs.declarations,
  nyx.programs.editing, nyx.scheduler, nyx.codec, nyx.codegen, nyx.source,
  nyx.studio.projects, nyx.studio.session, nyx.studio.edits, nyx.test.programs.logic
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

type
  { Direct-call lifetime qualification. A caller-controlled cancellation read
    retires its operation alias and replaces the input variable. It owns no
    receiver; invocation must already own both before entering that callout. }
  TRetiringScope = class(TInterfacedObject, INyxCancellationScope)
  public
    procedure Cancel;
    function GetCancelled: Boolean;
  end;

var
  GChecks: Integer;
  GOperation: INyxProgramLogic;
  GInputs: TNyxFlowValues;

procedure TRetiringScope.Cancel;
begin
end;

function TRetiringScope.GetCancelled: Boolean;
begin
  GOperation := nil;
  GInputs := NyxFlowValues;
  Result := False;
end;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Program Logic: ' + AReason);
  end;
  Inc(GChecks);
end;

{ Independent visible-frame oracle; it reads public entries without depending
  on the private vector layout or manufacturing an invocation implementation. }
function FrameData(const AValues: TNyxFlowValues): TNyxDataValue;
var
  LItems: array of TNyxDataValue;
  LEntry: TNyxFlowEntry;
  LIndex: Integer;
begin
  SetLength(LItems, AValues.Count);
  for LIndex := 0 to High(LItems) do
  begin
    LEntry := AValues.Entry(LIndex);
    LItems[LIndex] := NyxObject([NyxField('port', NyxData(LEntry.Port.Name)),
      NyxField('value', LEntry.Value.ToData)]);
  end;
  Result := NyxArray(LItems);
end;

procedure CallerRetirement;
var
  LScope: INyxCancellationScope;
  LOutputs: TNyxFlowValues;
begin
  GOperation := NewNyxProgramLogic(NyxProgramRef('Greeting'),
    NewNyxProgramDefinition('begin Caption := ''Hello, '' + Name end.',
      GreetingProgramContract));
  GInputs := NyxFlowValues.WithValue(NyxFlowPort('Name'), NyxFlowText('Retained caller'));
  LScope := TRetiringScope.Create;
  LOutputs := GOperation.Invoke(GInputs, LScope);
  Check((GOperation = nil) and (GInputs.Count = 0) and
    (LOutputs.Value(NyxFlowPort('Caption')).AsText = 'Hello, Retained caller'),
    'direct invocation retains receiver, token and exact input before cancellation callouts');
end;

procedure Frames;
const
  CSource: TNyxText =
    'begin Caption := ''Hello, '' + Name; FlagOut := Flag; CountOut := Count; ' +
    'AmountOut := Amount; PayloadOut := Payload; BytesOut := BytesIn end.';
  CDecimals: array[0..6] of TNyxText =
    ('12.50e0', '1.00', '-0.000e2', '0.1', '1e100', '1e-100', '1.2345678901234567');
var
  LContract: TNyxProgramContract;
  LDefinition: INyxProgramDefinition;
  LOperation: INyxProgramLogic;
  LApproximate: INyxProgramLogic;
  LDeclaration: INyxLogicOperation;
  LInputs: TNyxFlowValues;
  LOutputs: TNyxFlowValues;
  LBytes: TNyxBytes;
  LJSON: TNyxDataValue;
  LText: TNyxText;
  LBefore: TNyxText;
  LFailed: Boolean;
  LIndex: Integer;
  LScope: INyxCancellationScope;
begin
  LContract := NyxProgramContract
    .Input(NyxProgramSymbol('Name'), NyxProgramType(npkText))
    .Output(NyxProgramSymbol('Caption'), NyxProgramType(npkText))
    .Input(NyxProgramSymbol('Flag'), NyxProgramType(npkBoolean))
    .Output(NyxProgramSymbol('FlagOut'), NyxProgramType(npkBoolean))
    .Input(NyxProgramSymbol('Count'), NyxProgramType(npkInteger))
    .Output(NyxProgramSymbol('CountOut'), NyxProgramType(npkInteger))
    .Input(NyxProgramSymbol('Amount'), NyxProgramType(npkNumber))
    .Output(NyxProgramSymbol('AmountOut'), NyxProgramType(npkNumber))
    .Input(NyxProgramSymbol('Payload'), NyxProgramType(npkJSON))
    .Output(NyxProgramSymbol('PayloadOut'), NyxProgramType(npkJSON))
    .Input(NyxProgramSymbol('BytesIn'), NyxProgramType(npkBytes))
    .Output(NyxProgramSymbol('BytesOut'), NyxProgramType(npkBytes));
  LDefinition := NewNyxProgramDefinition(CSource, LContract);
  LOperation := NewNyxProgramLogic(NyxProgramRef('Typed transform'), LDefinition);
  LText := TNyxText('Ada 🌙') + #0 + 'exact';
  LBytes := NyxEncodeUTF8(LText);
  LJSON := TNyxDataValue.ParseJSON('{"large":9007199254740993,"name":"Moon \uD83C\uDF19"}');
  LInputs := NyxFlowValues
    .WithValue(NyxFlowPort('Name'), NyxFlowText(LText))
    .WithValue(NyxFlowPort('Flag'), NyxFlowBoolean(True))
    .WithValue(NyxFlowPort('Count'), NyxFlowInteger(Low(Integer)))
    .WithValue(NyxFlowPort('Amount'), NyxFlowNumber(NyxDecimal('12.50e0')))
    .WithValue(NyxFlowPort('Payload'), NyxFlowJSON(LJSON))
    .WithValue(NyxFlowPort('BytesIn'), NyxFlowBytes(LBytes));
  LBefore := FrameData(LInputs).ToJSON;
  LOutputs := LOperation.Invoke(LInputs, nil);
  Check(LOutputs.Value(NyxFlowPort('Caption')).AsText = TNyxText('Hello, ') + LText,
    'text ports preserve supplementary Unicode and NUL');
  Check(LOutputs.Value(NyxFlowPort('FlagOut')).AsBoolean and
    (LOutputs.Value(NyxFlowPort('CountOut')).AsInteger = Low(Integer)),
    'Boolean and signed integer families remain specialized');
  Check(LOutputs.Value(NyxFlowPort('PayloadOut')).AsJSON.ToJSON = LJSON.ToJSON,
    'structured JSON keeps exact decimal metadata and supplementary text');
  Check(NyxDecodeUTF8(LOutputs.Value(NyxFlowPort('BytesOut')).AsBytes) = LText,
    'complete byte output remains independently owned');
  Check(FrameData(LInputs).ToJSON = LBefore, 'execution never changes its input frame');
  for LIndex := Low(CDecimals) to High(CDecimals) do
  begin
    LOutputs := LOperation.Invoke(LInputs.WithValue(NyxFlowPort('Amount'),
      NyxFlowNumber(NyxDecimal(CDecimals[LIndex]))), nil);
    Check(LOutputs.Value(NyxFlowPort('AmountOut')).ValueType = nftNumber,
      'equivalent finite decimal input survives the explicit Double bridge');
  end;
  LFailed := False;
  try
    LOperation.Invoke(LInputs.WithValue(NyxFlowPort('Amount'),
      NyxFlowNumber(NyxDecimal('9007199254740993'))), nil);
  except
    on E: ENyxFlow do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'default decimal policy rejects lossy projection beyond Double precision');
  LApproximate := LOperation.Numbers(npncApproximate);
  LOutputs := LApproximate.Invoke(LInputs.WithValue(NyxFlowPort('Amount'),
    NyxFlowNumber(NyxDecimal('9007199254740993'))), nil);
  Check((LOperation.NumberConversion = npncPreserveDecimal) and
    (LApproximate.NumberConversion = npncApproximate) and
    (LOutputs.Value(NyxFlowPort('AmountOut')).AsNumber.Text <> '9007199254740993'),
    'caller may explicitly select approximation without changing the original recipe');
  LDeclaration := NyxLogicFromData(NyxLogicKind(NyxProgramLogicKind), LOperation.Configuration);
  LFailed := False;
  try
    LDeclaration.Invoke(LInputs, nil);
  except
    on E: ENyxFlow do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LDeclaration.Configuration.ToJSON = LOperation.Configuration.ToJSON),
    'wire import retains exact intent without reconstructing executable authority');
  LScope := NewNyxCancellationScope;
  LScope.Cancel;
  LFailed := False;
  try
    LOperation.Invoke(LInputs, LScope);
  except
    on E: ENyxFlowCancelled do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (FrameData(LInputs).ToJSON = LBefore),
    'cancelled invocation exposes no successful output or input mutation');
  LFailed := False;
  try
    NewNyxProgramLogic(NyxProgramRef('Opaque'), NyxProgramContract.Input(
      NyxProgramSymbol('Item'), NyxProgramHandleType(NyxProgramSymbol('SceneItem'))));
  except
    on E: ENyxFlow do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'opaque scene ports require their actual owning adapter');
end;

{$ifndef PAS2JS}
procedure Save(const ADirectory, AName, AValue: TNyxText);
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  ForceDirectories(ADirectory);
  LBytes := NyxEncodeUTF8(AValue);
  LStream := TFileStream.Create(IncludeTrailingPathDelimiter(ADirectory) + AName, fmCreate);
  try

    if Length(LBytes) > 0 then
    begin
      LStream.WriteBuffer(LBytes[0], Length(LBytes));
    end;
  finally
    LStream.Free;
  end;
end;
{$endif}

procedure AuthoringForms;
var
  LDocument: TNyxDocument;
  LChild: INyxCard;
  LOther: INyxCard;
  LParent: INyxCard;
  LNewParent: INyxCard;
  LChildDraft: TNyxProgramLogicEditorDraft;
  LParentDraft: TNyxLogicEditorDraft;
  LSaved: TNyxDataValue;
  LBefore: TNyxText;
  LDefault: TNyxText;
  LFailed: Boolean;
  LCatalog: INyxLogicCatalog;
begin
  LDocument := ProgramLogicDocument;
  try
    LBefore := TNyxCodec.Encode(LDocument);
    LCatalog := NewNyxLogicCatalog(LDocument.Programs);
    Check(LCatalog.Count = NewNyxLogicCatalog.Count + 1,
      'document-aware discovery adds one inert typed program template');
    LChild := NewNyxProgramLogicEditor('program-form', LDocument.Programs);
    Check(CaptureNyxProgramLogicEditor('program-form', LChild.Node)
      .ProgramReference.Name = 'Greeting', 'public child form captures a typed named reference');
    LFailed := False;
    try
      LoadNyxProgramLogicEditor('program-form', LChild.Node, NewNyxProgramLogic(
        NyxProgramRef('Greeting'), NyxProgramContract.Output(
          NyxProgramSymbol('Caption'), NyxProgramType(npkText))));
    except
      on E: ENyxFlow do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (LChild.Node.Find('program-form-program').Prop('value') = 'Greeting'),
      'loading a mismatched signature refuses before replacing any selection');
    LChild.Node.Find('program-form-numbers').Configure.Value('future unfinished choice').Done;
    LChildDraft.Capture('program-form', LChild.Node, True);
    LSaved := LChildDraft.ToData;
    LChildDraft := TNyxProgramLogicEditorDraft.FromData(LSaved);
    LOther := NewNyxProgramLogicEditor('program-form', LDocument.Programs);
    Check(LChildDraft.Restore('program-form', LOther.Node) and
      (LOther.Node.Find('program-form-numbers').Prop('value') = 'future unfinished choice'),
      'child draft preserves invalid closed-choice presentation exactly');
    LFailed := False;
    try
      CaptureNyxProgramLogicEditor('program-form', LOther.Node);
    except
      on E: ENyxFlow do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (TNyxCodec.Encode(LDocument) = LBefore),
      'unfinished program configuration cannot mutate the design or become runtime authority');
    LParent := NewNyxLogicEditor('logic', LDocument, NyxFlowRef('Greeting flow'));
    LParent.Node.Find('logic-operation').Configure.Value(NyxProgramLogicKind).Done;
    Check(HandleNyxLogicEditor(LParent.Node.Find('logic-operation'), LParent.Node),
      'ordinary Logic form opens specialized program configuration');
    LParent.Node.Find('logic-step-name').Configure.Value('unfinished step').Done;
    LParent.Node.Find('logic-program-numbers').Configure.Value('future unfinished choice').Done;
    LParentDraft.Capture('logic', LParent.Node);
    LSaved := LParentDraft.ToData;
    Check(LSaved.Field('version').AsInteger = 12, 'owned program draft uses its optional wrapper');
    LParentDraft := TNyxLogicEditorDraft.FromData(LSaved);
    Check(LParentDraft.ToData.ToJSON = LSaved.ToJSON, 'complete compound draft round trip stays exact');
    LNewParent := NewNyxLogicEditor('logic', LDocument, NyxFlowRef('Greeting flow'));
    Check(LParentDraft.Restore('logic', LNewParent.Node) and
      (LNewParent.Node.Find('logic-step-name').Prop('value') = 'unfinished step') and
      (LNewParent.Node.Find('logic-program-numbers').Prop('value') = 'future unfinished choice'),
      'owning Logic draft restores its exact independent program choices');
    LDocument.Programs.Define(NyxProgramRef('Another greeting'),
      NewNyxProgramDefinition('begin Caption := Name end.', GreetingProgramContract));
    LNewParent := NewNyxLogicEditor('logic', LDocument, NyxFlowRef('Greeting flow'));
    LDefault := LNewParent.Node.Find('logic-step-name').Prop('value');
    Check(not LParentDraft.Restore('logic', LNewParent.Node) and
      (LNewParent.Node.Find('logic-step-name').Prop('value') = LDefault) and
      (LParentDraft.ToData.ToJSON = LSaved.ToJSON),
      'changed program context refuses before any parent field changes and retains the draft');
  finally
    LDocument.Free;
  end;
end;

procedure Documents;
var
  LDocument: TNyxDocument;
  LParsed: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSession: TNyxStudioSession;
  LSource: TNyxText;
  LBefore: TNyxText;
  LPair: TNyxText;
  LChanged: TNyxText;
  LRegistry: INyxProgramDeclarations;
  LResolver: INyxLogicResolver;
  LGraph: INyxFlowGraph;
  LInput: TNyxFlowInput;
  LResult: INyxFlowResult;
  LFailed: Boolean;
begin
  LDocument := ProgramLogicDocument;
  LParsed := nil;
  LWorkspace := nil;
  LSession := nil;
  try
    LBefore := TNyxCodec.Encode(LDocument);
    LSource := TNyxCodegen.Generate(LDocument);
    Check((Pos('NewNyxProgramLogic(NyxProgramRef(', LSource) > 0) and
      (Pos('NyxProgramContract', LSource) > 0) and
      (Pos('NewNyxLogicDeclaration(', LSource) = 0),
      'generated program steps use specialized typed constructors and vocabulary');
    LParsed := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LParsed) = LBefore,
      'literal admission reconstructs the entire routed program document');
    Check(LWorkspace.Render(LParsed) = LSource, 'no-op source retains exact authored bytes');
    FreeAndNil(LParsed);
    FreeAndNil(LWorkspace);
    LResolver := NewNyxProgramLogicResolver(LDocument.Programs);
    LRegistry := LDocument.Programs;
    LGraph := LDocument.Flows.Definition(NyxFlowRef('Greeting flow')).Build(LResolver);
    LInput.Endpoint := ProgramLogicEndpoint('Name');
    LInput.Value := NyxFlowText('Grace');
    LResult := LGraph.Evaluate([LInput]);
    Check(LResult.Value(ProgramLogicEndpoint('Caption')).AsText = 'Hello, Grace!',
      'named program executes inside the existing typed graph engine');
    LRegistry.Define(NyxProgramRef('Greeting'), NewNyxProgramDefinition(
      'begin Caption := ''A different greeting'' end.', GreetingProgramContract));
    LGraph := LDocument.Flows.Definition(NyxFlowRef('Greeting flow')).Build(LResolver);
    LResult := LGraph.Evaluate([LInput]);
    Check(LResult.Value(ProgramLogicEndpoint('Caption')).AsText = 'Hello, Grace!',
      'runtime resolver retains independent declaration membership');
    LSession := TNyxStudioSession.Create(NyxProjectPair(LBefore, LSource));
    LPair := EncodeNyxProject(LSession.ProjectSnapshot);
    LFailed := False;
    try
      LSession.ApplyPatch(NyxProgramPatch([NyxRemoveProgram(NyxProgramRef('Greeting'))]));
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (EncodeNyxProject(LSession.ProjectSnapshot) = LPair),
      'removing a referenced program refuses the complete paired publication');
    LSession.ApplyPatch(NyxProgramPatch([NyxDefineProgram(NyxProgramRef('Greeting'),
      NewNyxProgramDefinition('begin Caption := ''Welcome, '' + Name end.',
        GreetingProgramContract))]));
    LChanged := EncodeNyxProject(LSession.ProjectSnapshot);
    Check((LChanged <> LPair) and
      (LSession.Document.Flows.Definition(NyxFlowRef('Greeting flow')).StepCount = 1),
      'ordinary paired program editing retains its saved route and graph schema');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LPair,
      'one Undo restores the exact routed-program pair');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LChanged,
      'one Redo restores the exact changed program source/design pair');
    {$ifndef PAS2JS}

    if ParamCount = 1 then
    begin
      Save(ParamStr(1), 'nyx.generated.view.pas', LSource);
      Save(ParamStr(1), 'design.nyx', LBefore);
      Save(ParamStr(1), 'initial.nyxpair', EncodeNyxProject(NyxProjectPair(LBefore, LSource)));
    end;
    {$endif}
  finally
    LSession.Free;
    LWorkspace.Free;
    LParsed.Free;
    LDocument.Free;
  end;
end;

{ Explicit approximation is portable authoring data, not a runtime-only knob.
  Compare the complete reconstructed document and preserve its exact no-op source. }
procedure SourcePolicies;
var
  LDocument: TNyxDocument;
  LParsed: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSource: TNyxText;
begin
  LDocument := ProgramLogicDocument;
  LParsed := nil;
  LWorkspace := nil;
  try
    LDocument.Flows.Define(NyxFlowRef('Greeting flow'), NewNyxFlowGraph.WithStep(
      NyxFlowStep('greeting'), NewNyxProgramLogic(NyxProgramRef('Greeting'),
        GreetingProgramContract).Numbers(npncApproximate)).Build);
    LSource := TNyxCodegen.Generate(LDocument);
    Check(Pos('.Numbers(npncApproximate)', LSource) > 0,
      'explicit number policy generates through its specialized fluent contract');
    LParsed := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LParsed) = TNyxCodec.Encode(LDocument),
      'literal admission retains the complete explicit number-policy document');
    Check(LWorkspace.Render(LParsed) = LSource,
      'explicit number-policy source remains exact under no-op regeneration');
  finally
    LWorkspace.Free;
    LParsed.Free;
    LDocument.Free;
  end;
end;

begin
  try
    CallerRetirement;
    Frames;
    AuthoringForms;
    Documents;
    SourcePolicies;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-program-logic', 'passed');
    document.body.setAttribute('data-program-logic-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' named program Logic checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-program-logic', 'failed');
      document.body.setAttribute('data-program-logic-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
