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


unit nyx.regex;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.regex.types, nyx.scheduler;

type
  { Managed immutable portable compiled pattern. Every search owns its cursor,
    captures, budgets and cancellation scope. Concurrent calls share code/data
    only, never lastIndex or a UI object. GroupCount includes complete group zero.
    Find honors Options.Extent; First/IsMatch intentionally inspect only the first
    match. Failed/cancelled/over-budget searches return no successful prefix. }
  INyxRegex = interface(IInterface)
    ['{D1411014-6120-4C84-A708-000000000080}']
    function GetOptions: TNyxRegexOptions;
    function GetGroupCount: Integer;
    function GroupName(AIndex: Integer): TNyxText;
    function CaptureIndex(const AReference: TNyxRegexCaptureRef): Integer;
    function Find(const AInput: TNyxText;
      const ACancellation: INyxCancellationScope = nil): TNyxRegexMatches;
    function First(const AInput: TNyxText;
      const ACancellation: INyxCancellationScope = nil): TNyxRegexMatch;
    function IsMatch(const AInput: TNyxText;
      const ACancellation: INyxCancellationScope = nil): Boolean;
    property Options: TNyxRegexOptions read GetOptions;
    property GroupCount: Integer read GetGroupCount;
  end;

{ Compile once into immutable Pascal instructions. Unsupported/malformed syntax
  and excessive resources refuse before returning an implementation. }
function NewNyxRegex(const AOptions: TNyxRegexOptions): INyxRegex;

implementation

uses SysUtils, Math, nyx.bytes, nyx.unicode, nyx.unicode.types;

type
  { Independently owned integer vector used for scalar input, capture boundaries,
    counters and program operands. Explicit copies separate backtracking states. }
  TIntegers = array of Integer;
  { Immutable exact group names, indexed like captures including group zero. }
  TNames = array of TNyxText;
  { Compile-time lexical flags captured by each instruction. Scoped flags never
    mutate shared runtime options or another invocation. }
  TMode = record
    Comparison: TNyxRegexCase;
    Multiline: Boolean;
    DotAll: Boolean;
    Expanded: Boolean;
  end;
  { Closed scalar-class primitives. Property escapes are declarative data;
    nested classes reference earlier compiler-owned classes without cycles. }
  TPartKind = (cpRange, cpDigit, cpSpace, cpWord, cpCategory, cpNested, cpAssigned);
  { Unicode category membership set, independent of locale/RTL character width. }
  TCategories = set of TNyxUnicodeCategory;
  { One union member, optionally complemented. Range endpoints are scalars;
    Category masks and nested class indices are validated during parsing. }
  TPart = record
    Kind: TPartKind;
    First: Integer;
    Last: Integer;
    Categories: TCategories;
    Negated: Boolean;
  end;
  { Closed nested-set combination, applied after union members in source order. }
  TSetOperator = (csoIntersection, csoSubtraction);
  { Immutable nested-set operand referencing a distinct earlier class. }
  TSetOperand = record
    Kind: TSetOperator;
    ClassIndex: Integer;
  end;
  { Immutable character set with union and explicit nested intersection/difference.
    Complements apply to scalar membership, never raw UTF-8/UTF-16 code units. }
  TCharacterClass = record
    Parts: array of TPart;
    Operators: array of TSetOperand;
    Negated: Boolean;
  end;
  { Private parse-tree choices. Flat child vectors avoid recursion proportional
    to literal/alternative count; only declared grouping increases nesting. }
  TNodeKind = (nkEmpty, nkLiteral, nkClass, nkAny, nkConcat, nkAlternate,
    nkCapture, nkRepeat, nkStart, nkEnd, nkWord, nkBackref, nkAssert, nkAtomic);
  { Compiler-owned node, holding indices/text rather than object references.
    Value names a scalar/class/capture, Min/Max a repetition; assertions carry
    negative/behind choices. No parse tree survives into an invocation. }
  TNode = record
    Kind: TNodeKind;
    Children: TIntegers;
    ChildCount: Integer;
    Value: Integer;
    Name: TNyxText;
    Minimum: Integer;
    Maximum: Integer;
    Greedy: Boolean;
    Negative: Boolean;
    Behind: Boolean;
    Mode: TMode;
  end;
  { Portable VM instructions. X is the next/body PC; Y is alternate/exit.
    Value is a scalar/class/capture/counter/atomic slot according to Op.
    Minimum/Maximum also carry fixed lookbehind width where applicable. }
  TOp = (roAccept, roChar, roClass, roAny, roSplit, roSaveStart, roSaveEnd,
    roRepeatInit, roRepeatCheck, roRepeatEnd, roStart, roEnd, roWord,
    roBackref, roAssert, roAtomicStart, roAtomicEnd,
    roRepeatChar, roRepeatClass, roRepeatAny);
  { Immutable program instruction and lexical policy. Only the compiler patches
    jump targets; runtime searches never mutate this vector. }
  TInstruction = record
    Op: TOp;
    X: Integer;
    Y: Integer;
    Value: Integer;
    Minimum: Integer;
    Maximum: Integer;
    Greedy: Boolean;
    Negative: Boolean;
    Behind: Boolean;
    Mode: TMode;
  end;

  { Shared compiled owner. A caller-local managed lease covers reentrant
    cancellation checks; no invocation or borrowed input is stored here. }
  TCompiledRegex = class(TInterfacedObject, INyxRegex)
  private
    FOptions: TNyxRegexOptions;
    FCode: array of TInstruction;
    FClasses: array of TCharacterClass;
    FNames: TNames;
    FEntry: Integer;
    FLoops: Integer;
    FAtomics: Integer;
  public
    constructor Create(const AOptions: TNyxRegexOptions);
    function GetOptions: TNyxRegexOptions;
    function GetGroupCount: Integer;
    function GroupName(AIndex: Integer): TNyxText;
    function CaptureIndex(const AReference: TNyxRegexCaptureRef): Integer;
    function Find(const AInput: TNyxText;
      const ACancellation: INyxCancellationScope): TNyxRegexMatches;
    function First(const AInput: TNyxText;
      const ACancellation: INyxCancellationScope): TNyxRegexMatch;
    function IsMatch(const AInput: TNyxText;
      const ACancellation: INyxCancellationScope): Boolean;
  end;

  { One constructor-local parser/compiler owns the decoded pattern and parse
    vectors. It borrows its destination only until Compile returns or refuses. }
  TCompiler = class
  private
    FTarget: TCompiledRegex;
    FPattern: TIntegers;
    FNodes: array of TNode;
    FNodeCount: Integer;
    FCodeCount: Integer;
    FPosition: Integer;
    FQuoted: Boolean;
    FNesting: Integer;
    FMode: TMode;
    function Peek: Integer;
    function Take: Integer;
    procedure Expect(AValue: Integer);
    procedure Space;
    procedure QuoteBoundary;
    procedure Enter;
    procedure Leave;
    procedure Fail(const AReason: TNyxText);
    function Node(AKind: TNodeKind): Integer;
    procedure Child(AParent, AChild: Integer);
    function Disjunction: Integer;
    function Alternative: Integer;
    function Term: Integer;
    function Atom: Integer;
    function Group: Integer;
    function Escape(AInClass: Boolean; out APart: TPart): Integer;
    function CharacterClass: Integer;
    function PropertyPart(ANegative: Boolean): TPart;
    function ReadName(AClose: Integer): TNyxText;
    function Decimal: Integer;
    function HexDigits(ACount: Integer): Integer;
    function Width(ANode: Integer): Integer;
    function Emit(AOp: TOp; ANode, ANext: Integer): Integer;
    function Code(ANode, ANext: Integer): Integer;
  public
    constructor Create(ATarget: TCompiledRegex);
    procedure Compile;
  end;

  { One private VM state owns its detached vectors. Capture slots contain
    start/end scalar offsets, counters contain repetition count/prior position,
    and marks contain atomic-stack boundaries. No state borrows another vector. }
  TState = record
    PC: Integer;
    Position: Integer;
    Captures: TIntegers;
    Counters: TIntegers;
    Marks: TIntegers;
  end;
  { One owned alternative. Primitive greedy repetitions store a compact scalar
    range rather than allocating one state per input character. Each returned
    candidate clones the frame; optional Group adjusts a repeated unit capture. }
  TAlternative = record
    Frame: TState;
    Ranged: Boolean;
    Minimum: Integer;
    NextPosition: Integer;
    Group: Integer;
  end;

  { Invocation-local runner borrows the compiled owner under a caller lease and
    owns decoded input/offset maps, budget counters and cancellation. Every
    alternate copies state; recursive assertions share the same COMPLETE budget. }
  TRunner = class
  private
    FRegex: TCompiledRegex;
    FScope: INyxCancellationScope;
    FText: TNyxText;
    FScalars: TIntegers;
    FOffsets: TIntegers;
    FBytes: array of TNyxTextBytes;
    FSteps: Integer;
    FStates: Integer;
    FResultBytes: Integer;
    procedure Tick;
    procedure Charge(ABytes: TNyxTextBytes);
    function Initial(AStart: Integer): TState;
    function Clone(const AState: TState): TState;
    function Equal(AA, AB: Integer; ACase: TNyxRegexCase): Boolean;
    function PartMatches(const APart: TPart; AScalar: Integer; const AMode: TMode): Boolean;
    function ClassMatches(AClass, AScalar: Integer; const AMode: TMode): Boolean;
    function Word(AScalar: Integer): Boolean;
    function LineBreak(AScalar: Integer): Boolean;
    function Run(const AInitial: TState; ARequiredEnd, ADepth: Integer;
      out AResult: TState): Boolean;
    function Snapshot(const AState: TState; AStart: Integer): TNyxRegexMatch;
  public
    constructor Create(ARegex: TCompiledRegex; const AInput: TNyxText;
      const ACancellation: INyxCancellationScope);
    function Find(AFirstOnly: Boolean): TNyxRegexMatches;
    function HasMatch: Boolean;
  end;

{$include nyx.regex.parser.inc}
{$include nyx.regex.vm.inc}

constructor TCompiledRegex.Create(const AOptions: TNyxRegexOptions);
var
  LCompiler: TCompiler;
begin
  inherited Create;
  AOptions.Validate;
  FOptions := AOptions;
  LCompiler := TCompiler.Create(Self);
  try
    LCompiler.Compile;
  finally
    LCompiler.Free;
  end;
end;

function TCompiledRegex.GetOptions: TNyxRegexOptions;
begin
  Result := FOptions;
end;

function TCompiledRegex.GetGroupCount: Integer;
begin
  Result := Length(FNames);
end;

function TCompiledRegex.GroupName(AIndex: Integer): TNyxText;
begin

  if (AIndex < 0) or (AIndex >= Length(FNames)) then
  begin
    raise ENyxRegex.Create('Capture group index is unavailable');
  end;
  Result := FNames[AIndex];
end;

function TCompiledRegex.CaptureIndex(const AReference: TNyxRegexCaptureRef): Integer;
begin

  if not AReference.Defined then
  begin
    raise ENyxRegex.Create('Capture selector is undefined');
  end;

  if not AReference.Named then
  begin
    Result := AReference.Index;
    GroupName(Result);
    Exit;
  end;
  for Result := 1 to High(FNames) do
  begin

    if FNames[Result] = AReference.Name then
    begin
      Exit;
    end;
  end;
  raise ENyxRegex.Create('Unknown named capture: ' + AReference.Name);
end;

function TCompiledRegex.Find(const AInput: TNyxText;
  const ACancellation: INyxCancellationScope): TNyxRegexMatches;
var
  LLease: INyxRegex;
  LRunner: TRunner;
begin
  LLease := Self;
  LRunner := TRunner.Create(Self, AInput, ACancellation);
  try
    Result := LRunner.Find(FOptions.Extent = nrsFirst);
  finally
    LRunner.Free;
  end;
end;

function TCompiledRegex.First(const AInput: TNyxText;
  const ACancellation: INyxCancellationScope): TNyxRegexMatch;
var
  LLease: INyxRegex;
  LRunner: TRunner;
  LMatches: TNyxRegexMatches;
begin
  Result := Default(TNyxRegexMatch);
  LLease := Self;
  LRunner := TRunner.Create(Self, AInput, ACancellation);
  try
    LMatches := LRunner.Find(True);

    if LMatches.Count > 0 then
    begin
      Result := LMatches.Item(0);
    end;
  finally
    LRunner.Free;
  end;
end;

function TCompiledRegex.IsMatch(const AInput: TNyxText;
  const ACancellation: INyxCancellationScope): Boolean;
var
  LLease: INyxRegex;
  LRunner: TRunner;
begin
  LLease := Self;
  LRunner := TRunner.Create(Self, AInput, ACancellation);
  try
    Result := LRunner.HasMatch;
  finally
    LRunner.Free;
  end;
end;

function NewNyxRegex(const AOptions: TNyxRegexOptions): INyxRegex;
begin
  Result := TCompiledRegex.Create(AOptions);
end;

end.
