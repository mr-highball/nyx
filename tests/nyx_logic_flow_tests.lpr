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
program nyx_logic_flow_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.flow.types, nyx.logic, nyx.flow,
  nyx.scheduler, nyx.test.logic.fixture
  {$ifdef PAS2JS}, Web{$endif};

type
  { Deliberately extensible implementation proves result-schema enforcement,
    cancellation between steps and independent captured configuration. }
  TProbe = class(TInterfacedObject, INyxLogicOperation)
  public
    Configuration: TNyxDataValue;
    Calls: Integer;
    Malformed: Boolean;
    CancelOnEntry: Boolean;
    function GetDescriptor: TNyxLogicDescriptor;
    function GetConfiguration: TNyxDataValue;
    function Invoke(const AInputs: TNyxFlowValues;
      const ACancellation: INyxCancellationScope): TNyxFlowValues;
  end;

var
  GChecks: Integer;

function TProbe.GetDescriptor: TNyxLogicDescriptor;
begin
  Result := NyxLogicDescriptor(NyxLogicKind('test-probe'), 'Test operation',
    'Qualification-only creator implementation.', nlgFlow)
    .Input(NyxFlowPort('start'), nftSignal, 'Explicit invocation signal')
    .Output(NyxFlowPort('result'), nftText, 'Owned output text');
end;

function TProbe.GetConfiguration: TNyxDataValue;
begin
  Result := Configuration;
end;

function TProbe.Invoke(const AInputs: TNyxFlowValues;
  const ACancellation: INyxCancellationScope): TNyxFlowValues;
begin
  Inc(Calls);

  if CancelOnEntry then
  begin
    ACancellation.Cancel;
  end;
  Result := NyxFlowValues;

  if Malformed then
  begin
    Result := Result.WithValue(NyxFlowPort('result'), NyxFlowInteger(42));
  end
  else
  begin
    Result := Result.WithValue(NyxFlowPort('result'), NyxFlowText('Signal received'));
  end;
end;

procedure Check(AValue: Boolean; const AMessage: TNyxText);
begin

  if not AValue then
  begin
    raise ENyxFlow.Create('Logic qualification: ' + AMessage);
  end;
  Inc(GChecks);
end;

procedure Run;
var
  LGraph: INyxFlowGraph;
  LCopy: INyxFlowGraph;
  LBuilder: INyxFlowBuilder;
  LResult: INyxFlowResult;
  LPrevious: INyxFlowResult;
  LFrame: TNyxFlowValues;
  LChanged: TNyxFlowValues;
  LData: TNyxDataValue;
  LText: TNyxText;
  LPretty: TNyxText;
  LCompact: TNyxText;
  LFailed: Boolean;
  LScope: INyxCancellationScope;
  LBytes: TNyxBytes;
  LByteCopy: TNyxBytes;
  LValue: TNyxFlowValue;
  LMetadata: TNyxLogicDescriptor;
  LProbe: TProbe;
  LOperation: INyxLogicOperation;
  LProbeGraph: INyxFlowGraph;
  LWire: TNyxText;
begin
  LText := '{"profile":{"name":"English demo","count":9007199254740993,' +
    '"small":1.2300e-40,"text":"Unicode 🐾 𝄞 漢字\u0000"},"extra":true}';
  LData := NewNyxJSONParse.Parse(LText);
  LPretty := NewNyxJSONFormat.Format(LData);
  LCompact := NewNyxJSONFormat(njlCompact).Format(LData);
  Check(Pos(#10 + '  "profile": {', LPretty) > 0, 'real JSON prettification');
  Check(Pos(#10, LCompact) = 0, 'compact output has no formatter whitespace');
  Check((Pos('9007199254740993', LPretty) > 0) and (Pos('1.2300e-40', LPretty) > 0),
    'number spellings never become Double');
  Check(TNyxDataValue.ParseJSON(LPretty).ToJSON = LData.ToJSON, 'pretty retains whole exact data');
  Check(TNyxDataValue.ParseJSON(LCompact).ToJSON = LData.ToJSON, 'compact retains whole exact data');
  Check(NewNyxJSONSelect(NyxDataPath.Field('profile').Field('text')).Select(LData).AsText =
    TNyxText('Unicode 🐾 𝄞 漢字') + TNyxText(#0), 'structural selection retains supplementary text and NUL');
  Check(NewNyxTextJoin(' / ').Join('Left 🐾', 'Right') = TNyxText('Left 🐾 / Right'), 'direct typed text operation');
  LMetadata := NewNyxJSONFormat.Descriptor;
  Check((LMetadata.Group = nlgJSON) and (LMetadata.LabelCount = 3) and
    (LMetadata.Description <> '') and (LMetadata.PortCount = 2), 'discoverable help/group/labels/ports');
  LFrame := NyxFlowValues.WithValue(NyxLogicPort(nlpText), NyxFlowText('old'));
  LChanged := LFrame.WithValue(NyxLogicPort(nlpText), NyxFlowText('new'));
  Check((LFrame.Value(NyxLogicPort(nlpText)).AsText = 'old') and
    (LChanged.Value(NyxLogicPort(nlpText)).AsText = 'new'), 'immutable input frame replacement');
  LBytes := NyxEncodeUTF8('Bytes 🐾' + TNyxText(#0));
  LValue := NyxFlowBytes(LBytes);
  LBytes[0] := 0;
  LByteCopy := LValue.AsBytes;
  Check(LByteCopy[0] = Ord('B'), 'byte inputs are detached');
  LByteCopy[0] := 0;
  Check(LValue.AsBytes[0] = Ord('B'), 'byte reads are detached');
  Check(TNyxFlowValue.FromData(LValue.ToData).ToData.ToJSON = LValue.ToData.ToJSON,
    'typed byte persistence');
  Check(NyxFlowNumber(NyxDecimal('9007199254740993')).AsNumber.Text = '9007199254740993',
    'exact numeric flow value');
  LFailed := False;
  try
    NyxFlowText('true').AsBoolean;
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'text is never a Boolean');
  LGraph := LogicFixture;
  Check((LGraph.StepCount = 5) and (LGraph.WireCount = 4) and LGraph.WorkerSafe, 'admitted executable DAG');
  LResult := LGraph.Evaluate([
    NyxFlowInput(LogicEndpoint('parse', nlpText), NyxFlowText(LText)),
    NyxFlowInput(LogicEndpoint('summary', nlpLeft), NyxFlowText('Profile'))]);
  Check(LResult.Count = 5, 'every step executes once despite fan-out');
  LPretty := LResult.Value(LogicEndpoint('pretty', nlpText)).AsText;
  Check(LResult.Value(LogicEndpoint('summary', nlpText)).AsText = 'Profile' + #10 + LPretty,
    'parse/select/pretty/join chain produces a real result');
  Check(TNyxDataValue.ParseJSON(LResult.Value(LogicEndpoint('compact', nlpText)).AsText).ToJSON =
    NewNyxJSONSelect(NyxDataPath.Field('profile')).Select(LData).ToJSON, 'fan-out compact branch agrees');
  LPrevious := LResult;
  LResult := LGraph.Evaluate([
    NyxFlowInput(LogicEndpoint('parse', nlpText), NyxFlowText('{"profile":{"name":"Another"}}')),
    NyxFlowInput(LogicEndpoint('summary', nlpLeft), NyxFlowText('Second'))]);
  Check(LPrevious.Value(LogicEndpoint('pretty', nlpText)).AsText = LPretty,
    'independent runs retain previous results');
  LCopy := NyxFlowGraphFromData(LGraph.ToData);
  Check(LCopy.ToData.ToJSON = LGraph.ToData.ToJSON, 'versioned graph/configuration exact round trip');
  Check(LCopy.Evaluate([
    NyxFlowInput(LogicEndpoint('parse', nlpText), NyxFlowText(LText)),
    NyxFlowInput(LogicEndpoint('summary', nlpLeft), NyxFlowText('Profile'))])
    .Value(LogicEndpoint('summary', nlpText)).AsText = 'Profile' + #10 + LPretty,
    'restored graph executes exact results');
  LFailed := False;
  try
    LGraph.Evaluate([]);
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'complete invocation admission rejects missing inputs');
  LFailed := False;
  try
    LGraph.Evaluate([
      NyxFlowInput(LogicEndpoint('parse', nlpText), NyxFlowJSON(LData)),
      NyxFlowInput(LogicEndpoint('summary', nlpLeft), NyxFlowText('Profile'))]);
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'no implicit JSON-to-text input conversion');
  LFailed := False;
  try
    LGraph.Evaluate([
      NyxFlowInput(LogicEndpoint('parse', nlpText), NyxFlowText('{broken')),
      NyxFlowInput(LogicEndpoint('summary', nlpLeft), NyxFlowText('Profile'))]);
  except
    on E: Exception do
    begin
      LFailed := Pos('parse', E.Message) > 0;
    end;
  end;
  Check(LFailed and (LPrevious.Value(LogicEndpoint('pretty', nlpText)).AsText = LPretty),
    'failed graph diagnoses its step and preserves prior output');
  LBuilder := NewNyxFlowGraph
    .WithStep(NyxFlowStep('a'), NewNyxTextJoin)
    .WithStep(NyxFlowStep('b'), NewNyxTextJoin);
  LCopy := LBuilder.Build;
  LFailed := False;
  try
    LBuilder.Connect(LogicEndpoint('a', nlpText), LogicEndpoint('b', nlpText));
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LBuilder.Build.WireCount = 0), 'direction refusal preserves builder');
  LBuilder.Connect(LogicEndpoint('a', nlpText), LogicEndpoint('b', nlpLeft));
  LFailed := False;
  try
    LBuilder.Connect(LogicEndpoint('a', nlpText), LogicEndpoint('b', nlpLeft));
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LBuilder.Build.WireCount = 1), 'cardinality refusal preserves accepted wire');
  LBuilder.Connect(LogicEndpoint('b', nlpText), LogicEndpoint('a', nlpLeft));
  LFailed := False;
  try
    LBuilder.Build;
  except
    on E: Exception do
    begin
      LFailed := Pos('cycle', E.Message) > 0;
    end;
  end;
  Check(LFailed and (LCopy.WireCount = 0), 'cycles refuse without changing sealed snapshots');
  LCopy := NyxFlowGraphFromData(NewNyxFlowGraph(
    TNyxFlowLimits.Defaults.ValueBytes(100).RunBytes(200))
    .WithStep(NyxFlowStep('join'), NewNyxTextJoin).Build.ToData);
  Check(LCopy.StepCount = 1, 'small custom limits restore without invalid intermediate defaults');
  LScope := NewNyxCancellationScope;
  LScope.Cancel;
  LFailed := False;
  try
    LGraph.Evaluate([], LScope);
  except
    on E: ENyxFlowCancelled do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'cancelled invocation never enters operations');
  LFailed := False;
  try
    LGraph.Evaluate([
      NyxFlowInput(LogicEndpoint('parse', nlpText), NyxFlowText(LText)),
      NyxFlowInput(LogicEndpoint('parse', nlpText), NyxFlowText(LText)),
      NyxFlowInput(LogicEndpoint('summary', nlpLeft), NyxFlowText('Profile'))]);
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'duplicate invocation endpoints refuse');
  LFailed := False;
  try
    NewNyxFlowGraph.WithStep(NyxFlowStep('parse'), NewNyxJSONParse)
      .WithStep(NyxFlowStep('join'), NewNyxTextJoin)
      .Connect(LogicEndpoint('parse', nlpJSON), LogicEndpoint('join', nlpLeft));
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'wire type mismatch requires explicit conversion');
  LProbe := TProbe.Create;
  LOperation := LProbe;
  LProbe.Configuration := NyxObject([NyxField('version', NyxData(1))]);
  LProbeGraph := NewNyxFlowGraph.WithStep(NyxFlowStep('probe'), LOperation).Build;
  LWire := LProbeGraph.ToData.ToJSON;
  LProbe.Configuration := NyxObject([NyxField('version', NyxData(2))]);
  Check(LProbeGraph.ToData.ToJSON = LWire, 'sealed graph captures configuration before creator mutation');
  LResult := LProbeGraph.Evaluate([NyxFlowInput(
    NyxFlowEndpoint(NyxFlowStep('probe'), NyxFlowPort('start')), NyxFlowSignal)]);
  Check(LResult.Value(NyxFlowEndpoint(NyxFlowStep('probe'), NyxFlowPort('result'))).AsText =
    'Signal received', 'signal token triggers an actual creator operation');
  LFailed := False;
  try
    LProbeGraph.Evaluate([NyxFlowInput(
      NyxFlowEndpoint(NyxFlowStep('probe'), NyxFlowPort('start')), NyxFlowBoolean(True))]);
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LProbe.Calls = 1), 'Boolean never fabricates an invocation signal');
  LProbe.Malformed := True;
  LFailed := False;
  try
    LProbeGraph.Evaluate([NyxFlowInput(
      NyxFlowEndpoint(NyxFlowStep('probe'), NyxFlowPort('start')), NyxFlowSignal)]);
  except
    on E: Exception do
    begin
      LFailed := Pos('probe', E.Message) > 0;
    end;
  end;
  Check(LFailed, 'alternative implementation cannot return mistyped output');
  LProbe.Malformed := False;
  LProbe.CancelOnEntry := True;
  LScope := NewNyxCancellationScope;
  LFailed := False;
  try
    LProbeGraph.Evaluate([NyxFlowInput(
      NyxFlowEndpoint(NyxFlowStep('probe'), NyxFlowPort('start')), NyxFlowSignal)], LScope);
  except
    on E: ENyxFlowCancelled do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'cancellation after operation entry exposes no successful result');
  LFailed := False;
  try
    NyxFlowGraphFromData(LProbeGraph.ToData);
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'creator kind requires explicit trusted wire resolver');
  LCopy := NewNyxFlowGraph(TNyxFlowLimits.Defaults.ValueBytes(64).RunBytes(256))
    .WithStep(NyxFlowStep('join'), NewNyxTextJoin).Build;
  LFailed := False;
  try
    LCopy.Evaluate([
      NyxFlowInput(LogicEndpoint('join', nlpLeft), NyxFlowText(
        TNyxText(StringOfChar('x', 100)))),
      NyxFlowInput(LogicEndpoint('join', nlpRight), NyxFlowText(''))]);
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'input byte budget refuses before operation execution');
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-logic-flow', 'passed');
    document.body.setAttribute('data-logic-flow-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' executable Logic graph checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-logic-flow', 'failed');
      document.body.setAttribute('data-logic-flow-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
