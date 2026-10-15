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



program nyx_programs_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.data, nyx.bytes, nyx.state,
  nyx.programs.types, nyx.programs.contract, nyx.programs, nyx.programs.execution, nyx.scheduler
  {$ifdef PAS2JS}, JS, Web{$endif};

type
  { Independent creator handle proves the interpreter contract never depends on
    one implementation class or a DOM/LCL object. Its type is immutable. }
  TFixtureHandle = class(TInterfacedObject, INyxProgramHandle)
    destructor Destroy; override;
    function GetValueType: TNyxProgramType;
  end;

  { Qualification-only frozen query values plus counters. Optional same-program
    reentry checks independent activation/journal ownership; no live app is read. }
  TFixtureQueries = class(TInterfacedObject, INyxProgramQueries)
  public
    Handle: INyxProgramHandle;
    ProgramValue: INyxProgram;
    Calls: Integer;
    WrongType: Boolean;
    ReturnNil: Boolean;
    Reenter: Boolean;
    Reentering: Boolean;
    InnerProposals: Integer;
    function Query(const ARoutine: TNyxProgramSymbolRef;
      const AArguments: INyxProgramArguments): INyxProgramValue;
  end;

  { Deterministic monotonic/decreasing clocks qualify quota contracts without
    asserting wall-time or platform scheduling performance from fixture values. }
  TFixtureClock = class(TInterfacedObject, INyxProgramClock)
  public
    Current: Double;
    Backwards: Boolean;
    function Milliseconds: Double;
  end;

  { Invocation-local cooperative cancellation counter; deliberately serial.
    Production worker consumers use the scheduler's synchronized scope. }
  TFixtureScope = class(TInterfacedObject, INyxCancellationScope)
  public
    Reads: Integer;
    AfterReads: Integer;
    Flag: Boolean;
    procedure Cancel;
    function GetCancelled: Boolean;
  end;

var
  GChecks: Integer;
  GHandlesRetired: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Pascal program: ' + AReason);
  end;
  Inc(GChecks);
end;

destructor TFixtureHandle.Destroy;
begin
  Inc(GHandlesRetired);
  inherited Destroy;
end;

function TFixtureHandle.GetValueType: TNyxProgramType;
begin
  Result := NyxProgramHandleType(NyxProgramSymbol('TCanvasItem'));
end;

procedure Contracts;
const
  CText: TNyxText = 'Hello 😀' + #0 + 'world';
var
  LContract: TNyxProgramContract;
  LCopy: TNyxProgramContract;
  LGrants: TNyxProgramGrants;
  LDerived: TNyxProgramGrants;
  LInputs: INyxProgramValues;
  LMore: INyxProgramValues;
  LHandle: INyxProgramHandle;
  LValue: INyxProgramValue;
  LBytes: TNyxBytes;
  LRead: TNyxBytes;
  LFailed: Boolean;
  LProposal: INyxProgramProposal;
begin
  Check(NyxProgramSymbol('Title').Same(NyxProgramSymbol('TITLE')), 'Pascal symbol comparison is case-insensitive');
  Check(not NyxProgramCapability('scene.read').Same(NyxProgramCapability('Scene.Read')),
    'Capability authority retains exact identity');
  LContract := NyxProgramContract.Input(NyxProgramSymbol('Source'), NyxProgramType(npkText))
    .Output(NyxProgramSymbol('Caption'), NyxProgramType(npkText));
  LCopy := LContract.WithRoutine(NyxProgramPropose(NyxProgramSymbol('SetText'),
    NyxProgramCapability(npcStateWrite)).Argument(NyxProgramSymbol('Text'), NyxProgramType(npkText)));
  Check((LContract.RoutineCount = 0) and (LCopy.RoutineCount = 1), 'Derived vocabularies remain independent');
  Check(TNyxProgramContract.FromData(LCopy.ToData).ToData.ToJSON = LCopy.ToData.ToJSON,
    'Declarations persist exact types without runtime authority');
  LGrants := NyxProgramGrants.Allow(NyxProgramCapability(npcStateRead));
  LDerived := LGrants.Allow(NyxProgramCapability(npcStateWrite));
  LDerived := LDerived.Allow(NyxProgramCapability(npcStateRead));
  Check((LGrants.Count = 1) and (LDerived.Count = 2) and
    LDerived.Contains(NyxProgramCapability(npcStateWrite)), 'Repeated grants retain every independently declared capability');
  LInputs := NyxProgramValues.WithValue(NyxProgramSymbol('Source'), NyxProgramValue(CText));
  LMore := LInputs.WithValue(NyxProgramSymbol('Extra'), NyxProgramValue(42));
  Check((LInputs.Count = 1) and (LMore.Count = 2) and
    (LInputs.Value(NyxProgramSymbol('source')).AsText = CText), 'Input frames retain exact supplementary text/NUL and independent membership');
  SetLength(LBytes, 2);
  LBytes[0] := 0;
  LBytes[1] := 255;
  LValue := NyxProgramValue(LBytes);
  LBytes[0] := 99;
  LRead := LValue.AsBytes;
  LRead[1] := 0;
  Check((LValue.AsBytes[0] = 0) and (LValue.AsBytes[1] = 255), 'Byte values detach both admission and getter storage');
  LHandle := TFixtureHandle.Create;
  LValue := NyxProgramValue(LHandle);
  Check((LValue.AsHandle = LHandle) and
    LValue.ValueType.Same(NyxProgramHandleType(NyxProgramSymbol('tcanvasitem'))),
    'Alternative handles retain exact typed managed capability');
  LProposal := NyxProgramProposal(LCopy.Routine(0),
    NyxProgramArguments([NyxProgramValue(CText)]), NyxProgramSpan(9, 4, 2, 3));
  Check((LProposal.Arguments.Item(0).AsText = CText) and (LProposal.Span.Start = 9),
    'Proposal receipts preserve detached typed arguments and exact source spans');
  LFailed := False;
  try
    LInputs := LInputs.WithValue(NyxProgramSymbol('SOURCE'), NyxProgramValue('overwrite'));
  except
    on E: ENyxProgram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LInputs.Count = 1), 'Duplicate binding refusal preserves the entire input frame');
  LFailed := False;
  try
    NyxProgramValue('scene.read').AsHandle;
  except
    on E: ENyxProgram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'Ordinary text cannot acquire an opaque host capability');
  Check(TNyxProgramLimits.FromData(NyxProgramLimits.Steps(12345).ToData)
    .ToData.ToJSON = NyxProgramLimits.Steps(12345).ToData.ToJSON, 'Caller-selected quotas round trip exactly');
end;


procedure Metadata;
var
  LContract: TNyxProgramContract;
  LDecoded: TNyxProgramContract;
  LRoutine: TNyxProgramRoutine;
  LLimits: TNyxProgramLimits;
  LData: TNyxDataValue;
  LParameter: TNyxDataValue;
  LReceipt: INyxProgramCompilation;
  LIndex: Integer;
  LFailed: Boolean;
begin
  LRoutine := NyxProgramQuery(NyxProgramSymbol('ReadPair'), NyxProgramType(npkInteger))
    .Argument(NyxProgramSymbol('First'), NyxProgramType(npkInteger))
    .Argument(NyxProgramSymbol('Second'), NyxProgramType(npkInteger));
  LContract := NyxProgramContract.Input(NyxProgramSymbol('Source'), NyxProgramType(npkText))
    .Output(NyxProgramSymbol('Caption'), NyxProgramType(npkText))
    .WithRoutine(LRoutine)
    .WithRoutine(NyxProgramQuery(NyxProgramSymbol('ReadCount'), NyxProgramType(npkInteger)));
  LDecoded := TNyxProgramContract.FromData(LContract.ToData,
    NyxProgramLimits.Symbols(2).Routines(2).Parameters(2));
  Check(LDecoded.ToData.ToJSON = LContract.ToData.ToJSON,
    'Bulk metadata admission retains ordered signatures and exact boundary types');
  for LIndex := 0 to 2 do
  begin
    LLimits := NyxProgramLimits;
    case LIndex of
      0: LLimits := LLimits.Symbols(1);
      1: LLimits := LLimits.Routines(1);
      2: LLimits := LLimits.Parameters(1);
    end;
    LFailed := False;
    try
      TNyxProgramContract.FromData(LContract.ToData, LLimits);
    except
      on E: ENyxProgram do
      begin
        LFailed := E.Diagnostic.Kind = npdLimit;
      end;
    end;
    Check(LFailed, 'Import refuses excess ports, routines and signature parameters under caller limits');
  end;
  LReceipt := InspectNyxProgram('not valid Pascal', LContract, NyxProgramLimits.Parameters(1));
  Check(not LReceipt.Succeeded and (LReceipt.Diagnostic(0).Kind = npdLimit),
    'Compiled admission refuses oversized caller metadata before parsing or copying its vocabulary');
  LData := NyxObject([NyxField('version', NyxData(NyxProgramLanguageVersion)),
    NyxField('source', NyxData('not valid Pascal')), NyxField('contract', LContract.ToData),
    NyxField('limits', NyxProgramLimits.Parameters(1).ToData)]);
  LFailed := False;
  try
    NyxProgramFromData(LData);
  except
    on E: ENyxProgram do
    begin
      LFailed := E.Diagnostic.Kind = npdLimit;
    end;
  end;
  Check(LFailed, 'Persisted program import admits its own limits before constructing host metadata');
  LData := NyxObject([NyxField('version', NyxData(1)),
    NyxField('ports', LContract.ToData.Field('ports')),
    NyxField('routines', NyxArray([NyxProgramQuery(NyxProgramSymbol('SOURCE'),
      NyxProgramType(npkInteger)).ToData]))]);
  LFailed := False;
  try
    TNyxProgramContract.FromData(LData);
  except
    on E: ENyxProgram do
    begin
      LFailed := E.Diagnostic.Kind = npdType;
    end;
  end;
  Check(LFailed, 'Bulk import keeps one case-insensitive namespace across ports and host routines');
  LParameter := NyxObject([NyxField('name', NyxData('FIRST')),
    NyxField('type', NyxProgramType(npkInteger).ToData)]);
  LData := NyxObject([NyxField('name', NyxData('ReadPair')), NyxField('kind', NyxData(0)),
    NyxField('result', NyxProgramType(npkInteger).ToData), NyxField('capability', NyxData('')),
    NyxField('parameters', NyxArray([LRoutine.ToData.Field('parameters').Item(0), LParameter]))]);
  LFailed := False;
  try
    TNyxProgramRoutine.FromData(LData);
  except
    on E: ENyxProgram do
    begin
      LFailed := E.Diagnostic.Kind = npdType;
    end;
  end;
  Check(LFailed, 'Bulk signature admission refuses differently cased duplicate parameter names');
end;

function IntegerProgram(const ASource: TNyxText): Integer;
var
  LProgram: INyxProgram;
begin
  LProgram := CompileNyxProgram(ASource, NyxProgramContract.Output(
    NyxProgramSymbol('Answer'), NyxProgramType(npkInteger)));
  Result := LProgram.Run(NyxProgramValues).Outputs.Value(NyxProgramSymbol('Answer')).AsInteger;
end;

procedure InvalidProgram(const ASource: TNyxText; AKind: TNyxProgramDiagnosticKind);
var
  LReceipt: INyxProgramCompilation;
begin
  LReceipt := InspectNyxProgram(ASource, NyxProgramContract.Output(
    NyxProgramSymbol('Answer'), NyxProgramType(npkInteger)), NyxProgramLimits);
  Check(not LReceipt.Succeeded and (LReceipt.CompiledProgram = nil) and
    (LReceipt.DiagnosticCount = 1) and (LReceipt.Diagnostic(0).Kind = AKind) and
    LReceipt.Diagnostic(0).Span.Defined, 'Whole-source typed refusal: ' + ASource);
end;

procedure RuntimeRefusal(const ASource: TNyxText; const ALimits: TNyxProgramLimits;
  AKind: TNyxProgramDiagnosticKind);
var
  LProgram: INyxProgram;
  LFailed: Boolean;
begin
  LProgram := CompileNyxProgram(ASource, NyxProgramContract.Output(
    NyxProgramSymbol('Answer'), NyxProgramType(npkInteger)), ALimits);
  LFailed := False;
  try
    LProgram.Run(NyxProgramValues);
  except
    on E: ENyxProgram do
    begin
      LFailed := (E.Diagnostic.Kind = AKind) and E.Diagnostic.Span.Defined;
    end;
  end;
  Check(LFailed, 'Runtime refusal retains no successful prefix: ' + ASource);
end;

procedure Language;
const
  CGreeting: TNyxText =
    'program Greeting;' + #10 +
    'var I, Total: Integer;' + #10 +
    'function Decorate(const Name: String): String;' + #10 +
    'var Prefix: String;' + #10 +
    '  function Suffix: String;' + #10 +
    '  begin Result := Prefix + Name end;' + #10 +
    'begin Prefix := ''[''; Result := Suffix() + '']'' end;' + #10 +
    'begin Total := 0; for I := 1 to 3 do Total := Total + I;' + #10 +
    '  Caption := Decorate(Source); Count := Total end.';
  CSource: TNyxText = 'Ada 😀' + #0 + 'Lovelace';
  CDiagnosticPrefix: TNyxText = '{😀}' + #13#10 + 'begin' + #10 + '  ';
var
  LContract: TNyxProgramContract;
  LProgram: INyxProgram;
  LResult: INyxProgramResult;
  LOther: INyxProgramResult;
  LInputs: INyxProgramValues;
  LReceipt: INyxProgramCompilation;
begin
  Check(IntegerProgram('begin Answer := 1 + 2 * 3 end.') = 7, 'Pascal arithmetic precedence');
  Check(IntegerProgram('var I, Total: Integer; begin Total := 0; I := 0; ' +
    'while I < 10 do begin I := I + 1; if I = 2 then Continue; if I = 6 then Break; ' +
    'Total := Total + I end; repeat I := I - 1; if I > 3 then Continue; ' +
    'Total := Total + I until I = 0; Answer := Total end.') = 19,
    'While/Repeat retain nested Break/Continue targets');
  Check(IntegerProgram('var I: Integer; begin Answer := 0; for I := 2147483647 to 2147483647 ' +
    'do Answer := Answer + 1; for I := -2147483648 downto -2147483648 do Answer := Answer + 1 end.') = 2,
    'For terminal iterations never overflow signed extrema');
  Check(IntegerProgram('function Factorial(const N: Integer): Integer; begin if N <= 1 then ' +
    'Exit(1); Result := N * Factorial(N - 1) end; begin Answer := Factorial(6) end.') = 720,
    'Named recursive functions and Exit(value) retain independent frames');
  Check(IntegerProgram('function IsOdd(const N: Integer): Boolean; forward; ' +
    'function IsEven(const N: Integer): Boolean; begin if N = 0 then Exit(True); ' +
    'Result := IsOdd(N - 1) end; ' +
    'function IsOdd(const N: Integer): Boolean; begin if N = 0 then Exit(False); ' +
    'Result := IsEven(N - 1) end; begin Answer := Ord(IsEven(8)) end.') = 1,
    'Exact forward declarations allow mutual recursion');
  Check(IntegerProgram('begin Answer := 0; if False and ((1 div 0) = 0) then Answer := 9; ' +
    'if True or ((1 div 0) = 0) then Answer := Answer + 1 end.') = 1,
    'Boolean operators short-circuit without skipping compile-time type checking');
  Check(IntegerProgram('begin Answer := -2147483648 mod -1 end.') = 0,
    'Integer modulo retains a valid result at the division overflow edge');
  LContract := NyxProgramContract.Input(NyxProgramSymbol('Source'), NyxProgramType(npkText))
    .Output(NyxProgramSymbol('Caption'), NyxProgramType(npkText))
    .Output(NyxProgramSymbol('Count'), NyxProgramType(npkInteger));
  LProgram := CompileNyxProgram(CGreeting, LContract);
  Check(LProgram.LanguageVersion = NyxProgramLanguageVersion, 'Compiled programs expose their exact grammar version');
  LInputs := NyxProgramValues.WithValue(NyxProgramSymbol('Source'), NyxProgramValue(CSource));
  LResult := LProgram.Run(LInputs);
  Check((LResult.Outputs.Value(NyxProgramSymbol('Caption')).AsText = '[' + CSource + ']') and
    (LResult.Outputs.Value(NyxProgramSymbol('Count')).AsInteger = 6),
    'Nested routines capture the correct lexical frame and exact typed event text');
  LOther := LProgram.Run(NyxProgramValues.WithValue(NyxProgramSymbol('Source'), NyxProgramValue('Grace')));
  Check((LOther.Outputs.Value(NyxProgramSymbol('Caption')).AsText = '[Grace]') and
    (LResult.Outputs.Value(NyxProgramSymbol('Caption')).AsText = '[' + CSource + ']'),
    'Repeated calls share code only; prior captured results stay unchanged');
  Check(NyxProgramFromData(LProgram.ToData).ToData.ToJSON = LProgram.ToData.ToJSON,
    'Exact language/version/contract/limits reconstruct without native evaluation');
  LProgram := CompileNyxProgram('begin Caption := Copy(Source, 2, 2) + Chr(0); Count := Length(Source) end.', LContract);
  LResult := LProgram.Run(NyxProgramValues.WithValue(NyxProgramSymbol('Source'), NyxProgramValue(TNyxText('A😀B'))));
  Check((LResult.Outputs.Value(NyxProgramSymbol('Caption')).AsText = TNyxText('😀B') + #0) and
    (LResult.Outputs.Value(NyxProgramSymbol('Count')).AsInteger = 3),
    'Length/Copy/Chr count scalars and retain supplementary/NUL values');
  LReceipt := InspectNyxProgram(CDiagnosticPrefix + 'Missing := 1; end.', LContract, NyxProgramLimits);
  Check(LReceipt.LanguageVersion = NyxProgramLanguageVersion, 'Diagnostic receipts expose the same grammar version');
  Check(not LReceipt.Succeeded and (LReceipt.Diagnostic(0).Kind = npdName) and
    (LReceipt.Diagnostic(0).Span.Start = NyxUTF8ByteCount(CDiagnosticPrefix)) and
    (LReceipt.Diagnostic(0).Span.Count = 7) and (LReceipt.Diagnostic(0).Span.Line = 3) and
    (LReceipt.Diagnostic(0).Span.Column = 3), 'UTF-8 byte spans and CRLF/scalar coordinates are exact on both targets');
  InvalidProgram('begin Answer := ''wrong type'' end.', npdType);
  InvalidProgram('begin if False then Answer := UnknownValue end.', npdName);
  InvalidProgram('begin if False then Answer := ''wrong type'' else Answer := 1 end.', npdType);
  InvalidProgram('begin ''end''.', npdName);
  InvalidProgram('{$mode objfpc} begin Answer := 1 end.', npdSyntax);
  InvalidProgram('begin Answer := 2147483648 end.', npdType);
  InvalidProgram('begin Answer := $FFFFFFFF end.', npdSyntax);
  InvalidProgram('var I: Integer; begin for I := 1 to 3 do I := I + 1; Answer := I end.', npdType);
  RuntimeRefusal('var Unassigned: Integer; begin Answer := Unassigned end.', NyxProgramLimits, npdRuntime);
  RuntimeRefusal('begin end.', NyxProgramLimits, npdRuntime);
  RuntimeRefusal('begin Answer := 2147483647 + 1 end.', NyxProgramLimits, npdRuntime);
  RuntimeRefusal('begin Answer := -(-2147483648) end.', NyxProgramLimits, npdRuntime);
  RuntimeRefusal('begin Answer := -2147483648 div -1 end.', NyxProgramLimits, npdRuntime);
  RuntimeRefusal('var I: Integer; function Change: Integer; begin I := 0; Result := 1 end; ' +
    'begin for I := 1 to 3 do Answer := Change() end.', NyxProgramLimits, npdRuntime);
  RuntimeRefusal('begin while True do begin end end.', NyxProgramLimits.Steps(50), npdLimit);
  RuntimeRefusal('begin Answer := 1 + 2 end.', NyxProgramLimits.StackValues(1), npdLimit);
  RuntimeRefusal('begin Answer := 1 end.', NyxProgramLimits.AllocationBytes(128), npdLimit);
  RuntimeRefusal('begin Answer := 1 end.', NyxProgramLimits.ResultBytes(32), npdLimit);
  RuntimeRefusal('var I: Integer; begin Answer := 0; for I := 1 to 100 do begin end end.',
    NyxProgramLimits.AllocationBytes(512), npdLimit);
end;


function FixtureGrants: TNyxProgramGrants;
begin
  Result := NyxProgramGrants.Allow(NyxProgramCapability(npcStateRead))
    .Allow(NyxProgramCapability(npcSceneRead)).Allow(NyxProgramCapability(npcSceneEdit));
end;

function TFixtureQueries.Query(const ARoutine: TNyxProgramSymbolRef;
  const AArguments: INyxProgramArguments): INyxProgramValue;
var
  LInner: INyxProgramResult;
begin
  Inc(Calls);

  if ReturnNil then
  begin
    Exit(nil);
  end;

  if ARoutine.Same(NyxProgramSymbol('Selected')) then
  begin
    Exit(NyxProgramValue(Handle));
  end;

  if Reenter and not Reentering then
  begin
    Reentering := True;
    try
      LInner := ProgramValue.Run(NyxProgramValues,
        NyxProgramRun.Grants(FixtureGrants).Queries(Self));
      InnerProposals := LInner.ProposalCount;
    finally
      Reentering := False;
      Reenter := False;
    end;
  end;

  if WrongType then
  begin
    Exit(NyxProgramValue('wrong family'));
  end;
  Result := NyxProgramValue(4);
end;

function TFixtureClock.Milliseconds: Double;
begin
  Result := Current;

  if Backwards then
  begin
    Current := Current - 1;
  end
  else
  begin
    Current := Current + 1;
  end;
end;

procedure TFixtureScope.Cancel;
begin
  Flag := True;
end;

function TFixtureScope.GetCancelled: Boolean;
begin
  Inc(Reads);
  Flag := Flag or ((AfterReads > 0) and (Reads >= AfterReads));
  Result := Flag;
end;

function HostContract: TNyxProgramContract;
var
  LItem: TNyxProgramType;
begin
  LItem := NyxProgramHandleType(NyxProgramSymbol('TCanvasItem'));
  Result := NyxProgramContract.Output(NyxProgramSymbol('Answer'), NyxProgramType(npkInteger))
    .WithRoutine(NyxProgramQuery(NyxProgramSymbol('Selected'), LItem,
      NyxProgramCapability(npcSceneRead)))
    .WithRoutine(NyxProgramQuery(NyxProgramSymbol('CountItems'), NyxProgramType(npkInteger),
      NyxProgramCapability(npcStateRead)))
    .WithRoutine(NyxProgramPropose(NyxProgramSymbol('Move'), NyxProgramCapability(npcSceneEdit))
      .Argument(NyxProgramSymbol('Item'), LItem)
      .Argument(NyxProgramSymbol('X'), NyxProgramType(npkNumber))
      .Argument(NyxProgramSymbol('Y'), NyxProgramType(npkNumber)));
end;

function RetainedHandleResult: INyxProgramResult;
var
  LProgram: INyxProgram;
  LQueries: TFixtureQueries;
  LLease: INyxProgramQueries;
begin
  LQueries := TFixtureQueries.Create;
  LLease := LQueries;
  LQueries.Handle := TFixtureHandle.Create;
  LProgram := CompileNyxProgram('var Item: TCanvasItem; begin Item := Selected(); ' +
    'Move(Item, 1, 2); Answer := 1 end.', HostContract);
  Result := LProgram.Run(NyxProgramValues, NyxProgramRun.Grants(FixtureGrants).Queries(LLease));
end;

procedure HostAndQuotas;
const
  CSource: TNyxText = 'var Item: TCanvasItem; X: Double; begin Item := Selected(); ' +
    'X := CountItems(); Move(Item, X, 2); Answer := 1 end.';
var
  LProgram: INyxProgram;
  LBad: INyxProgram;
  LQueries: TFixtureQueries;
  LLease: INyxProgramQueries;
  LOptions: INyxProgramRunOptions;
  LResult: INyxProgramResult;
  LPrevious: INyxProgramResult;
  LRetained: INyxProgramResult;
  LClock: TFixtureClock;
  LClockLease: INyxProgramClock;
  LScope: TFixtureScope;
  LScopeLease: INyxCancellationScope;
  LReceipt: INyxProgramCompilation;
  LFailed: Boolean;
  LBefore: Integer;
  LIndex: Integer;
begin
  LProgram := CompileNyxProgram(CSource, HostContract);
  Check((LProgram.CapabilityCount = 3) and
    LProgram.Capability(0).Same(NyxProgramCapability(npcSceneRead)) and
    LProgram.CapabilitySpan(0).Defined, 'Required capabilities are bounded typed source metadata');
  LQueries := TFixtureQueries.Create;
  LLease := LQueries;
  LQueries.Handle := TFixtureHandle.Create;
  LFailed := False;
  try
    LProgram.Run(NyxProgramValues, NyxProgramRun.Queries(LLease));
  except
    on E: ENyxProgram do
    begin
      LFailed := E.Diagnostic.Kind = npdCapability;
    end;
  end;
  Check(LFailed and (LQueries.Calls = 0), 'Missing grants refuse before invoking any host query');
  LOptions := NyxProgramRun.Grants(FixtureGrants).Queries(LLease);
  LResult := LProgram.Run(NyxProgramValues, LOptions);
  Check((LResult.ProposalCount = 1) and
    (LResult.Proposal(0).Arguments.Item(0).AsHandle = LQueries.Handle) and
    (LResult.Proposal(0).Arguments.Item(1).AsNumber = 4) and
    (LResult.Proposal(0).Arguments.Item(2).AsNumber = 2) and
    (LResult.Outputs.Value(NyxProgramSymbol('Answer')).AsInteger = 1),
    'Typed host handles, checked numeric widening and proposals produce useful detached results');
  Check((LQueries.Calls = 2) and LResult.Proposal(0).Routine.Capability.Same(
    NyxProgramCapability(npcSceneEdit)), 'Proposals never invoke a live host mutation');
  LPrevious := LResult;
  LBad := CompileNyxProgram('var Item: TCanvasItem; begin Item := Selected(); ' +
    'Move(Item, 1, 2); Answer := 1 div 0 end.', HostContract);
  LFailed := False;
  try
    LResult := LBad.Run(NyxProgramValues, LOptions);
  except
    on E: ENyxProgram do
    begin
      LFailed := E.Diagnostic.Kind = npdRuntime;
    end;
  end;
  Check(LFailed and (LResult = LPrevious) and (LResult.ProposalCount = 1),
    'A failure after staged proposals leaves the caller entire prior result untouched');
  LQueries.WrongType := True;
  LFailed := False;
  try
    LProgram.Run(NyxProgramValues, LOptions);
  except
    on E: ENyxProgram do
    begin
      LFailed := E.Diagnostic.Kind = npdType;
    end;
  end;
  Check(LFailed and (LPrevious.ProposalCount = 1), 'A wrong host result family refuses without publishing a partial journal');
  LQueries.WrongType := False;
  LQueries.ReturnNil := True;
  LFailed := False;
  try
    LProgram.Run(NyxProgramValues, LOptions);
  except
    on E: ENyxProgram do
    begin
      LFailed := E.Diagnostic.Kind = npdType;
    end;
  end;
  Check(LFailed, 'A missing host result is a typed refusal rather than an access violation');
  LQueries.ReturnNil := False;
  LQueries.ProgramValue := LProgram;
  LQueries.Calls := 0;
  LQueries.Reenter := True;
  LResult := LProgram.Run(NyxProgramValues, LOptions);
  Check((LQueries.Calls = 4) and (LQueries.InnerProposals = 1) and
    (LResult.ProposalCount = 1) and (LPrevious.ProposalCount = 1),
    'Reentrant calls share immutable code while retaining independent frames and journals');
  LQueries.ProgramValue := nil;
  LReceipt := InspectNyxProgram('begin Move(''not a handle'', 1, 2); Answer := 1 end.',
    HostContract, NyxProgramLimits);
  Check(not LReceipt.Succeeded and (LReceipt.Diagnostic(0).Kind = npdType),
    'Text cannot be passed to a typed opaque host parameter');

  LBefore := GHandlesRetired;
  LRetained := RetainedHandleResult;
  Check((LRetained <> nil) and (GHandlesRetired = LBefore),
    'A complete result retains its host handle after invocation/query retirement');
  LRetained := nil;
  Check(GHandlesRetired = LBefore + 1, 'Releasing the final result retires its owned handle without an interface cycle');

  LProgram := CompileNyxProgram('begin if False then Move(Selected(), 1, 2); Answer := 1 end.', HostContract);
  LFailed := False;
  try
    LProgram.Run(NyxProgramValues);
  except
    on E: ENyxProgram do
    begin
      LFailed := E.Diagnostic.Kind = npdCapability;
    end;
  end;
  Check(LFailed, 'Capability admission covers unreachable compiled branches');

  for LIndex := 0 to 2 do
  begin
    LProgram := CompileNyxProgram('begin while True do begin end end.',
      NyxProgramContract.Output(NyxProgramSymbol('Answer'), NyxProgramType(npkInteger)),
      NyxProgramLimits.Milliseconds(3).Steps(1000));
    LClock := TFixtureClock.Create;
    LClockLease := LClock;
    LClock.Backwards := LIndex = 1;
    LFailed := False;
    try

      if LIndex = 2 then
      begin
        LProgram.Run(NyxProgramValues);
      end
      else
      begin
        LProgram.Run(NyxProgramValues, NyxProgramRun.Clock(LClockLease));
      end;
    except
      on E: ENyxProgram do
      begin
        case LIndex of
          0: LFailed := E.Diagnostic.Kind = npdLimit;
          1: LFailed := E.Diagnostic.Kind = npdRuntime;
          2: LFailed := E.Diagnostic.Kind = npdCapability;
        end;
      end;
    end;
    Check(LFailed, 'Monotonic deadlines, backwards clocks and missing clocks have explicit outcomes');
  end;
  LProgram := CompileNyxProgram('begin while True do begin end end.', NyxProgramContract,
    NyxProgramLimits.Steps(10000));
  LScope := TFixtureScope.Create;
  LScopeLease := LScope;
  LScope.AfterReads := 3;
  LFailed := False;
  try
    LProgram.Run(NyxProgramValues, NyxProgramRun.Cancellation(LScopeLease));
  except
    on E: ENyxProgram do
    begin
      LFailed := E.Diagnostic.Kind = npdCancelled;
    end;
  end;
  Check(LFailed and (LScope.Reads = 3), 'Cancellation is checked during charged program work');
end;

begin
  GChecks := 0;
  GHandlesRetired := 0;
  try
    Contracts;
    Metadata;
    Language;
    HostAndQuotas;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-programs', 'passed');
    document.body.setAttribute('data-programs-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' portable Pascal-program checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-programs', 'failed');
      document.body.setAttribute('data-programs-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
