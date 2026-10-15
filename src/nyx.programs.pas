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



unit nyx.programs;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.data, nyx.programs.types, nyx.programs.contract;

const
  NyxProgramLanguageVersion = 1;

type
  { Immutable complete execution result. Outputs and proposals are detached from
    invocation frames; no accepted application/model state has been changed.
    The caller's owning adapter must admit the entire result at its captured
    revision/generation. Dropping or refusing a result performs no host effect. }
  INyxProgramResult = interface(IInterface)
    ['{9D69BC4B-085E-45F1-A9B0-000000000104}']
    function GetOutputs: INyxProgramValues;
    function GetProposalCount: Integer;
    function Proposal(AIndex: Integer): INyxProgramProposal;
    function GetSteps: Integer;
    function GetAllocatedBytes: Integer;
    property Outputs: INyxProgramValues read GetOutputs;
    property ProposalCount: Integer read GetProposalCount;
    property Steps: Integer read GetSteps;
    property AllocatedBytes: Integer read GetAllocatedBytes;
  end;

  { Managed immutable compiled Delphi-dialect behavior program. Compilation
    checks every body before execution. Each Run owns frames, operands, quotas,
    query services and its journal; no global/static variables are shared.
    Persisted declarations contain source/contract/limits only, never permission,
    executable native code, host objects or an application reference. }
  INyxProgram = interface(IInterface)
    ['{9D69BC4B-085E-45F1-A9B0-000000000105}']
    function GetSource: TNyxText;
    function GetLanguageVersion: Integer;
    function GetContract: TNyxProgramContract;
    function GetLimits: TNyxProgramLimits;
    function GetCapabilityCount: Integer;
    function GetMayPropose: Boolean;
    function Capability(AIndex: Integer): TNyxProgramCapabilityRef;
    function CapabilitySpan(AIndex: Integer): TNyxProgramSpan;
    function Run(const AInputs: INyxProgramValues): INyxProgramResult; overload;
    function Run(const AInputs: INyxProgramValues;
      const AOptions: INyxProgramRunOptions): INyxProgramResult; overload;
    function ToData: TNyxDataValue;
    property Source: TNyxText read GetSource;
    property LanguageVersion: Integer read GetLanguageVersion;
    property Contract: TNyxProgramContract read GetContract;
    property Limits: TNyxProgramLimits read GetLimits;
    property CapabilityCount: Integer read GetCapabilityCount;
    property MayPropose: Boolean read GetMayPropose;
  end;

  { Owned compilation receipt. Failure exposes the first exact diagnostic and
    no executable prefix. Source association belongs to the exact requested text;
    an unrelated editor revision must not use its offsets for navigation. }
  INyxProgramCompilation = interface(IInterface)
    ['{9D69BC4B-085E-45F1-A9B0-000000000106}']
    function GetSucceeded: Boolean;
    function GetLanguageVersion: Integer;
    function GetProgram: INyxProgram;
    function GetDiagnosticCount: Integer;
    function Diagnostic(AIndex: Integer): TNyxProgramDiagnostic;
    property Succeeded: Boolean read GetSucceeded;
    property LanguageVersion: Integer read GetLanguageVersion;
    property CompiledProgram: INyxProgram read GetProgram;
    property DiagnosticCount: Integer read GetDiagnosticCount;
  end;

function CompileNyxProgram(const ASource: TNyxText;
  const AContract: TNyxProgramContract): INyxProgram; overload;
function CompileNyxProgram(const ASource: TNyxText;
  const AContract: TNyxProgramContract; const ALimits: TNyxProgramLimits): INyxProgram; overload;
function InspectNyxProgram(const ASource: TNyxText;
  const AContract: TNyxProgramContract; const ALimits: TNyxProgramLimits): INyxProgramCompilation;
{ Strict version-one declaration import recompiles exact text against its typed
  vocabulary. No persisted field can enroll a query service or grant authority. }
function NyxProgramFromData(const AData: TNyxDataValue): INyxProgram;

implementation

uses SysUtils, Math, nyx.bytes, nyx.state, nyx.flow.types;

type
  { Private owned integer vector for bytecode fixups and source scalar maps. }
  TIntegers = array of Integer;
  { Private immutable runtime value vector; writes always replace whole values,
    never mutate JSON/byte payloads shared with an earlier frame or result. }
  TValues = array of INyxProgramValue;

  { Closed lexical tokens. Numeric literals retain their magnitude until unary
    parsing so signed Integer minimum is representable without an unsigned type. }
  TTokenKind = (ptEOF, ptIdentifier, ptText, ptInteger, ptNumber, ptSymbol);

  { Detached token with an exact UTF-8 span from the owned scalar source maps.
    Text is literal content or identifier/operator spelling, never authority. }
  TToken = record
    Kind: TTokenKind;
    Text: TNyxText;
    Number: Double;
    Span: TNyxProgramSpan;
  end;

  { Constructor-local lexer owns decoded scalar/storage/UTF-8/line maps. It
    validates all source Unicode before parsing and retains no runtime owner. }
  TLexer = class
  private
    FSource: TNyxText;
    FChars: TIntegers;
    FStorage: TIntegers;
    FBytes: TIntegers;
    FLines: TIntegers;
    FColumns: TIntegers;
    FPosition: Integer;
    FLimits: TNyxProgramLimits;
    function Peek(AOffset: Integer = 0): Integer;
    function Take: Integer;
    function Span(AFirst, ALast: Integer): TNyxProgramSpan;
    procedure Fail(AFirst: Integer; const AMessage: TNyxText);
    procedure Skip;
    function ReadText: TNyxText;
    function ReadNumber(out AInteger: Boolean): Double;
  public
    constructor Create(const ASource: TNyxText; const ALimits: TNyxProgramLimits);
    function Next: TToken;
  end;

  { Script-local symbol class owned by its compiled program. Initial is present only
    for literal constants; missing locals/results remain unassigned until written.
    Contract inputs and const parameters have ReadOnly set before execution. }
  TVariable = class
    Name: TNyxProgramSymbolRef;
    ValueType: TNyxProgramType;
    ReadOnly: Boolean;
    Initial: INyxProgramValue;
  end;

  { Immutable lexical routine metadata. Parent is the declaring routine index;
    frames resolve it through static links, never an escaping closure object.
    Forward signatures must match before their body/entry is admitted. }
  TRoutine = record
    Name: TNyxProgramSymbolRef;
    Parent: Integer;
    Entry: Integer;
    ResultType: TNyxProgramType;
    ResultSlot: Integer;
    Parameters: Integer;
    Variables: array of TVariable;
    Forwarded: Boolean;
    Declaration: TNyxProgramSpan;
  end;

  { Bytecode arithmetic vocabulary follows Pascal precedence and strict scalar
    types. AND/OR use separate short-circuit branches, not eager binary calls. }
  TBinary = (pbAdd, pbSubtract, pbMultiply, pbDivide, pbIntegerDivide, pbModulo,
    pbEqual, pbNotEqual, pbLess, pbAtMost, pbGreater, pbAtLeast);

  { Closed builtin functions. They cannot select a native routine from text,
    access a unit/global or bypass declared host signatures/capabilities. }
  TBuiltin = (pfLength, pfIntToStr, pfFloatToStr, pfCopy, pfTrim,
    pfAbs, pfMin, pfMax, pfOrd, pfChr);

  { Portable VM instructions. Scope/slot pairs refer to immutable lexical
    declarations; host calls refer only to the admitted contract's routine index.
    ForNext has a frozen end slot and advances without overflowing at the bound. }
  TOp = (poConstant, poRead, poWrite, poDrop, poWiden, poNegate, poNot, poBinary,
    poJump, poIfFalse, poIfTrue, poCall, poHost, poBuiltin, poReturn,
    poForLock, poForUnlock, poForNext);

  { One immutable instruction with exact source provenance. A/B/C/D/Target have
    opcode-specific checked meanings; only the compiler patches branch targets. }
  TInstruction = record
    Op: TOp;
    A: Integer;
    B: Integer;
    C: Integer;
    D: Integer;
    Target: Integer;
    Span: TNyxProgramSpan;
  end;

  { Shared immutable compiled owner. Constants, bytecode, lexical routines and
    capabilities are sealed before this object is returned as INyxProgram.
    Caller-local interface leases protect reentrant host/cancellation callbacks. }
  TCompiledProgram = class(TInterfacedObject, INyxProgram)
  private
    FSource: TNyxText;
    FContract: TNyxProgramContract;
    FLimits: TNyxProgramLimits;
    FCode: array of TInstruction;
    FConstants: TValues;
    FRoutines: array of TRoutine;
    FCapabilities: array of TNyxProgramCapabilityRef;
    FCapabilitySpans: array of TNyxProgramSpan;
    FNeedsQueries: Boolean;
    FMayPropose: Boolean;
  public
    constructor Create(const ASource: TNyxText;
      const AContract: TNyxProgramContract; const ALimits: TNyxProgramLimits);
    { Retire every privately owned symbol after complete or failed compilation. }
    destructor Destroy; override;
    function GetSource: TNyxText;
    function GetLanguageVersion: Integer;
    function GetContract: TNyxProgramContract;
    function GetLimits: TNyxProgramLimits;
    function GetCapabilityCount: Integer;
    function GetMayPropose: Boolean;
    function Capability(AIndex: Integer): TNyxProgramCapabilityRef;
    function CapabilitySpan(AIndex: Integer): TNyxProgramSpan;
    function Run(const AInputs: INyxProgramValues): INyxProgramResult; overload;
    function Run(const AInputs: INyxProgramValues;
      const AOptions: INyxProgramRunOptions): INyxProgramResult; overload;
    function ToData: TNyxDataValue;
  end;

  { Compiler-owned nested-loop fixups. Continue may be unresolved until Repeat's
    condition/For's step is emitted. Scope/Slot enforce readonly loop counters. }
  TLoop = record
    Breaks: TIntegers;
    Continues: TIntegers;
    Scope: Integer;
    Slot: Integer;
  end;

  { One parser/type-checker owns token state and fixups while borrowing only its
    unpublished destination. No parsing state survives into runtime execution. }
  TCompiler = class
  private
    FTarget: TCompiledProgram;
    FLexer: TLexer;
    FToken: TToken;
    FCurrent: Integer;
    FDepth: Integer;
    FSymbols: Integer;
    FCodeCount: Integer;
    FConstantCount: Integer;
    FLoops: array of TLoop;
    procedure Next;
    function IsToken(const AText: TNyxText): Boolean;
    function Accept(const AText: TNyxText): Boolean;
    procedure Expect(const AText: TNyxText);
    procedure Fail(AKind: TNyxProgramDiagnosticKind; const AMessage: TNyxText);
    procedure Enter;
    procedure Leave;
    function Emit(AOp: TOp; const ASpan: TNyxProgramSpan;
      AA: Integer = 0; AB: Integer = 0; AC: Integer = 0; AD: Integer = 0): Integer;
    function Constant(const AValue: INyxProgramValue; const ASpan: TNyxProgramSpan): Integer;
    function Variable(const AName: TNyxProgramSymbolRef; out AScope, ASlot: Integer): Boolean;
    function Routine(const AName: TNyxProgramSymbolRef): Integer;
    function Host(const AName: TNyxProgramSymbolRef): Integer;
    function Builtin(const AName: TNyxProgramSymbolRef; out AKind: TBuiltin): Boolean;
    function AddVariable(ARoutine: Integer; const AName: TNyxProgramSymbolRef;
      const AType: TNyxProgramType; AReadOnly: Boolean;
      const AInitial: INyxProgramValue): Integer;
    function ReadType: TNyxProgramType;
    function ReadName: TNyxProgramSymbolRef;
    function Literal(ANegative: Boolean = False): INyxProgramValue;
    function Primary: TNyxProgramType;
    function Unary: TNyxProgramType;
    function Expression(AMinimum: Integer = 0): TNyxProgramType;
    function Call(const AName: TNyxProgramSymbolRef;
      const ASpan: TNyxProgramSpan): TNyxProgramType;
    function BinaryType(AKind: TBinary;
      const ALeft, ARight: TNyxProgramType): TNyxProgramType;
    procedure Assignable(const ASource, ATarget: TNyxProgramType;
      const ASpan: TNyxProgramSpan);
    procedure Writable(AScope, ASlot: Integer);
    procedure Statements(const AEnd: TNyxText);
    procedure Statement;
    procedure Block(ARoutine: Integer);
    procedure DeclareRoutine;
    procedure Declarations;
    procedure PushLoop(AScope, ASlot: Integer);
    procedure EndLoop(ABreak, AContinue: Integer);
    procedure RequireCapability(const ARoutine: TNyxProgramRoutine;
      const ASpan: TNyxProgramSpan);
  public
    constructor Create(ATarget: TCompiledProgram);
    destructor Destroy; override;
    procedure Compile;
  end;

  { Invocation-owned lexical frame class. StaticParent locates captured declarations;
    return PC/operand base implement calls without borrowing another run's data. }
  TFrame = class
    Routine: Integer;
    StaticParent: Integer;
    ReturnPC: Integer;
    OperandBase: Integer;
    Values: TValues;
  end;

  { Active For guard belongs to its executing frame but may protect an outer
    lexical variable. Calls cannot mutate a captured active loop counter. Frame
    return and loop exit release the guard without creating reference cycles. }
  TForGuard = record
    Owner: Integer;
    Frame: Integer;
    Slot: Integer;
  end;

  { Detached completed result owner. Its vectors are never exposed mutably;
    getters clone data while retaining explicit managed opaque capabilities. }
  TProgramResult = class(TInterfacedObject, INyxProgramResult)
  private
    FOutputs: INyxProgramValues;
    FProposals: array of INyxProgramProposal;
    FSteps: Integer;
    FAllocation: Integer;
  public
    function GetOutputs: INyxProgramValues;
    function GetProposalCount: Integer;
    function Proposal(AIndex: Integer): INyxProgramProposal;
    function GetSteps: Integer;
    function GetAllocatedBytes: Integer;
  end;

  { Invocation-local bounded VM borrows its compiled owner under Run's managed
    lease. It owns frames, operand values, loop guards and the private proposal
    journal. Host queries receive detached values; no instruction commits edits. }
  TRunner = class
  private
    FProgram: TCompiledProgram;
    FOptions: INyxProgramRunOptions;
    FFrames: array of TFrame;
    FStack: TValues;
    FStackCount: Integer;
    FGuards: array of TForGuard;
    FProposals: array of INyxProgramProposal;
    FPC: Integer;
    FSteps: Integer;
    FAllocation: Integer;
    FResultBytes: Integer;
    FSpan: TNyxProgramSpan;
    FStarted: Double;
    FLastTime: Double;
    procedure Reject(AKind: TNyxProgramDiagnosticKind; const AMessage: TNyxText);
    procedure Tick;
    procedure CheckClock;
    procedure Charge(ABytes: TNyxTextBytes);
    procedure AdmitValue(const AValue: INyxProgramValue);
    procedure Push(const AValue: INyxProgramValue);
    function Pop: INyxProgramValue;
    function FrameFor(AScope: Integer): Integer;
    function Read(AScope, ASlot: Integer): INyxProgramValue;
    procedure Write(AScope, ASlot: Integer; const AValue: INyxProgramValue);
    procedure Call(ARoutine: Integer; const AArguments: INyxProgramArguments);
    procedure Return;
    function Arguments(ACount: Integer): INyxProgramArguments;
    function Binary(AKind: TBinary; const ALeft, ARight: INyxProgramValue): INyxProgramValue;
    function Builtin(AKind: TBuiltin; const AArguments: INyxProgramArguments): INyxProgramValue;
    function Finish: INyxProgramResult;
  public
    constructor Create(AProgram: TCompiledProgram; const AInputs: INyxProgramValues;
      const AOptions: INyxProgramRunOptions);
    { Retire every remaining frame on success, refusal or partial construction. }
    destructor Destroy; override;
    function Execute: INyxProgramResult;
  end;

  { Small owned compile receipt retaining either a complete program or one
    detached diagnostic. Unexpected implementation failures are not disguised
    as ordinary syntax/type refusal by InspectNyxProgram. }
  TCompilation = class(TInterfacedObject, INyxProgramCompilation)
  public
    ProgramValue: INyxProgram;
    Error: TNyxProgramDiagnostic;
    function GetSucceeded: Boolean;
    function GetLanguageVersion: Integer;
    function GetProgram: INyxProgram;
    function GetDiagnosticCount: Integer;
    function Diagnostic(AIndex: Integer): TNyxProgramDiagnostic;
  end;

{$i nyx.programs.lexer.inc}
{$i nyx.programs.compiler.inc}
{$i nyx.programs.runtime.inc}

constructor TCompiledProgram.Create(const ASource: TNyxText;
  const AContract: TNyxProgramContract; const ALimits: TNyxProgramLimits);
var
  LCompiler: TCompiler;
begin
  inherited Create;
  ALimits.Validate;
  AContract.ValidateLimits(ALimits);
  FSource := ASource;
  FContract := AContract.Copy;
  FLimits := ALimits;
  LCompiler := TCompiler.Create(Self);
  try
    LCompiler.Compile;
  finally
    LCompiler.Free;
  end;
end;

destructor TCompiledProgram.Destroy;
var
  LRoutine: Integer;
  LVariable: Integer;
begin
  for LRoutine := 0 to High(FRoutines) do
  begin
    for LVariable := 0 to High(FRoutines[LRoutine].Variables) do
    begin
      FRoutines[LRoutine].Variables[LVariable].Free;
    end;
  end;
  inherited Destroy;
end;

function TCompiledProgram.GetSource: TNyxText;
begin
  Result := FSource;
end;

function TCompiledProgram.GetLanguageVersion: Integer;
begin
  Result := NyxProgramLanguageVersion;
end;

function TCompiledProgram.GetContract: TNyxProgramContract;
begin
  Result := FContract.Copy;
end;

function TCompiledProgram.GetLimits: TNyxProgramLimits;
begin
  Result := FLimits;
end;

function TCompiledProgram.GetCapabilityCount: Integer;
begin
  Result := Length(FCapabilities);
end;

function TCompiledProgram.GetMayPropose: Boolean;
begin
  Result := FMayPropose;
end;

function TCompiledProgram.Capability(AIndex: Integer): TNyxProgramCapabilityRef;
begin

  if (AIndex < 0) or (AIndex >= Length(FCapabilities)) then
  begin
    raise ENyxProgram.Create(npdName, Default(TNyxProgramSpan), 'Capability index is out of range');
  end;
  Result := FCapabilities[AIndex];
end;

function TCompiledProgram.CapabilitySpan(AIndex: Integer): TNyxProgramSpan;
begin
  Capability(AIndex);
  Result := FCapabilitySpans[AIndex];
end;

function TCompiledProgram.Run(const AInputs: INyxProgramValues): INyxProgramResult;
begin
  Result := Run(AInputs, NyxProgramRun);
end;

function TCompiledProgram.Run(const AInputs: INyxProgramValues;
  const AOptions: INyxProgramRunOptions): INyxProgramResult;
var
  LLease: INyxProgram;
  LRunner: TRunner;
begin
  LLease := Self;
  LRunner := TRunner.Create(Self, AInputs, AOptions);
  try
    Result := LRunner.Execute;
  finally
    LRunner.Free;
  end;
end;

function TCompiledProgram.ToData: TNyxDataValue;
begin
  Result := NyxObject([NyxField('version', NyxData(NyxProgramLanguageVersion)), NyxField('source', NyxData(FSource)),
    NyxField('contract', FContract.ToData), NyxField('limits', FLimits.ToData)]);
end;

function CompileNyxProgram(const ASource: TNyxText;
  const AContract: TNyxProgramContract): INyxProgram;
begin
  Result := CompileNyxProgram(ASource, AContract, NyxProgramLimits);
end;

function CompileNyxProgram(const ASource: TNyxText;
  const AContract: TNyxProgramContract; const ALimits: TNyxProgramLimits): INyxProgram;
begin
  Result := TCompiledProgram.Create(ASource, AContract, ALimits);
end;

function NyxProgramFromData(const AData: TNyxDataValue): INyxProgram;
var
  LLimits: TNyxProgramLimits;
begin
  ValidateNyxFlowFields(AData, ['version', 'source', 'contract', 'limits']);

  if AData.Field('version').AsInteger <> NyxProgramLanguageVersion then
  begin
    raise ENyxProgram.Create(npdSyntax, Default(TNyxProgramSpan), 'Unsupported program declaration version');
  end;
  LLimits := TNyxProgramLimits.FromData(AData.Field('limits'));
  Result := CompileNyxProgram(AData.Field('source').AsText,
    TNyxProgramContract.FromData(AData.Field('contract'), LLimits), LLimits);
end;

function TCompilation.GetSucceeded: Boolean;
begin
  Result := ProgramValue <> nil;
end;

function TCompilation.GetLanguageVersion: Integer;
begin
  Result := NyxProgramLanguageVersion;
end;

function TCompilation.GetProgram: INyxProgram;
begin
  Result := ProgramValue;
end;

function TCompilation.GetDiagnosticCount: Integer;
begin
  Result := Ord(ProgramValue = nil);
end;

function TCompilation.Diagnostic(AIndex: Integer): TNyxProgramDiagnostic;
begin

  if (ProgramValue <> nil) or (AIndex <> 0) then
  begin
    raise ENyxProgram.Create(npdName, Default(TNyxProgramSpan), 'Compilation diagnostic index is out of range');
  end;
  Result := Error;
end;

function InspectNyxProgram(const ASource: TNyxText;
  const AContract: TNyxProgramContract; const ALimits: TNyxProgramLimits): INyxProgramCompilation;
var
  LResult: TCompilation;
begin
  LResult := TCompilation.Create;
  Result := LResult;
  try
    LResult.ProgramValue := CompileNyxProgram(ASource, AContract, ALimits);
  except
    on E: ENyxProgram do
    begin
      LResult.Error := E.Diagnostic;
    end;
  end;
end;

end.
