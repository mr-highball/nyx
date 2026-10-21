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
unit nyx.pascal.types;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.scheduler, nyx.pascal.lexing, nyx.pascal.syntax,
  nyx.pascal.work;

type
  { Quotas refuse the complete type candidate; ordinary incomplete source
    instead produces a detached recoverable syntax tree and diagnostics. }
  ENyxPascalTypeLimit = class(ENyxPascalLexicalLimit);
  { Declared type construction, never resolved compiler identity. Bounds/defaults
    retain exact syntax for later constant binding rather than numeric conversion.
    Encoded strings retain both their authored base and opaque code-page operand;
    they grant no effective encoding, conversion or target-layout fact. }
  TNyxPascalTypeKind = (ptxMissing, ptxName, ptxQualified, ptxGeneric, ptxPointer,
    ptxArray, ptxSet, ptxFile, ptxString, ptxClassOf, ptxProcedure, ptxFunction,
    ptxReference, ptxDistinct, ptxParameterGroup, ptxParameterName, ptxUntyped,
    ptxBound, ptxSubrange, ptxEnumeration, ptxEnumMember, ptxUnsupported,
    ptxStructured, ptxEncodedString);
  { Typed child intent. Ordinals and links are local to one immutable arena;
    they confer neither declaration visibility nor an editing capability.
    A code-page child is a value expression, never a generic type argument. }
  TNyxPascalTypeRole = (ptrRoot, ptrBase, ptrMember, ptrArgument, ptrPointee,
    ptrElement, ptrDimension, ptrLength, ptrTarget, ptrParameter, ptrParameterName,
    ptrParameterType, ptrResult, ptrDefault, ptrLowerBound, ptrUpperBound, ptrEnumMember,
    ptrCodePage);
  { Source-compatible name for the shared declaration/type request contract.
    Existing qualified enum spellings and ordinals remain available below;
    Winapi retains intent without resolving a target-specific ABI alias. }
  TNyxPascalCallingConvention = nyx.pascal.syntax.TNyxPascalRoutineConvention;

const
  { Preserve this unit's exported source names while the single enum owner also
    serves routine declarations. These are compile-time values, never globals. }
  pccDefault = nyx.pascal.syntax.prcDefault;
  pccRegister = nyx.pascal.syntax.prcRegister;
  pccCdecl = nyx.pascal.syntax.prcCdecl;
  pccStdcall = nyx.pascal.syntax.prcStdcall;
  pccPascal = nyx.pascal.syntax.prcPascal;
  pccSafecall = nyx.pascal.syntax.prcSafecall;
  pccSyscall = nyx.pascal.syntax.prcSyscall;
  pccVectorcall = nyx.pascal.syntax.prcVectorcall;
  pccWinapi = nyx.pascal.syntax.prcWinapi;

type
  { Authored packing and procedural qualifiers, independent of inferred defaults. }
  TNyxPascalTypeFlag = (ptfPacked, ptfBitPacked, ptfOfObject, ptfNested, ptfVarargs,
    ptfInline, ptfSpecialize);
  { Value-owned flags. No interfaces/arrays occur in published records. }
  TNyxPascalTypeFlags = set of TNyxPascalTypeFlag;
  { Exact syntax node with parent/child/sibling ordinals (-1 absent). Grouped
    parameter names share their group's type child; tree ownership stays in its
    arena. Span counts scalars in Lexical, which may be an analysis projection. }
  TNyxPascalTypeNode = record
  private
    FKind: TNyxPascalTypeKind;
    FRole: TNyxPascalTypeRole;
    FName: TNyxText;
    FSpan: TNyxPascalSpan;
    FParent: Integer;
    FFirst: Integer;
    FLast: Integer;
    FNext: Integer;
    FMode: TNyxPascalSyntaxParameterMode;
    FConvention: TNyxPascalCallingConvention;
    FFlags: TNyxPascalTypeFlags;
    FDeclaration: Integer;
  public
    property Kind: TNyxPascalTypeKind read FKind;
    property Role: TNyxPascalTypeRole read FRole;
    property Name: TNyxText read FName;
    property Span: TNyxPascalSpan read FSpan;
    property Parent: Integer read FParent;
    property FirstChild: Integer read FFirst;
    property NextSibling: Integer read FNext;
    property ParameterMode: TNyxPascalSyntaxParameterMode read FMode;
    property CallingConvention: TNyxPascalCallingConvention read FConvention;
    property Flags: TNyxPascalTypeFlags read FFlags;
  end;
  { Recoverable type failures are distinct from lexical diagnostics and compiler
    admission. A directive needs captured translation, never guessed branch state. }
  TNyxPascalTypeIssue = (ptiExpectedType, ptiExpectedName, ptiExpectedDelimiter,
    ptiUnsupportedType, ptiTrailingSyntax, ptiContextRequired);
  { Detached scalar diagnostic, with stable English guidance and no source borrow. }
  TNyxPascalTypeDiagnostic = record
  private
    FIssue: TNyxPascalTypeIssue;
    FSpan: TNyxPascalSpan;
    FMessage: TNyxText;
  public
    property Issue: TNyxPascalTypeIssue read FIssue;
    property Span: TNyxPascalSpan read FSpan;
    property MessageText: TNyxText read FMessage;
  end;
  { Independent positive node/depth/diagnostic budgets. A zeroed record refuses;
    fluent values retain no interfaces and never modify their baseline. }
  TNyxPascalTypeOptions = record
  private
    FDefined: Boolean;
    FNodes: Integer;
    FDepth: Integer;
    FDiagnostics: Integer;
  public
    { Defaults: 4096 nodes, 128 nested type/group levels, 128 diagnostics. }
    class function Defaults: TNyxPascalTypeOptions; static;
    function UpToNodes(AValue: Integer): TNyxPascalTypeOptions;
    function UpToDepth(AValue: Integer): TNyxPascalTypeOptions;
    function UpToDiagnostics(AValue: Integer): TNyxPascalTypeOptions;
    procedure Validate;
    property MaximumNodes: Integer read FNodes;
    property MaximumDepth: Integer read FDepth;
    property MaximumDiagnostics: Integer read FDiagnostics;
  end;
  { Owns immutable syntax over one exact lexical window. Complete recognizes the
    supported type grammar only; opaque constant expressions are not evaluated,
    names/types are not bound, and no compilation/execution authority is granted.
    Returned records/checked ordinals borrow no editor, file, mutable provider or UI. }
  INyxPascalTypeSyntax = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570D01}']
    function Lexical: INyxPascalLexicalSnapshot;
    function Window: TNyxPascalSpan;
    function Root: Integer;
    function Complete: Boolean;
    function Count: Integer;
    function Node(AIndex: Integer): TNyxPascalTypeNode;
    { First/ordinal child of this role, -1 absent; no inherited/name lookup occurs. }
    function Child(AParent: Integer; ARole: TNyxPascalTypeRole;
      AOrdinal: Integer = 0): Integer;
    function DiagnosticCount: Integer;
    function Diagnostic(AIndex: Integer): TNyxPascalTypeDiagnostic;
  end;
  { Optional declared-source lease for structured leaves. Their bodies/members
    belong to this exact immutable declaration snapshot, never a copied parser
    or synthetic type name. DeclarationOf returns -1 on other nodes; Declarations
    is nil for standalone type syntax. There is no reverse reference or UI borrow.
    The corrected identity separates this two-slot capability from a translated
    projection; existing dependent units must be rebuilt together. }
  INyxPascalDeclaredTypeSyntax = interface(IInterface)
    ['{C5E05E21-14B3-49DF-8734-7DF63996E953}']
    function Declarations: INyxPascalSyntaxSnapshot;
    function DeclarationOf(ANode: Integer): Integer;
  end;
  { Source-free immutable type grammar. Nodes/spans and opaque metadata markers
    retain no lexical snapshot, declarations, world, parser or cancellation.
    This is unbound syntax only; Count is retained node storage, not authority. }
  INyxPascalTypePlan = interface(IInterface)
    ['{AD61C267-9FE8-4943-9CCB-1AF6824FA536}']
    function Window: TNyxPascalSpan;
    function Count: Integer;
  end;

{ Parse an exact scalar window without copying/relexing its text. Bounds cannot
  cut a significant token. Nested generic closers split >> and >= contextually;
  the original lexical tokens/text remain exact. Cancellation is borrowed only
  during construction and quotas never publish a truncated candidate. }
function NewNyxPascalTypeSyntax(const ALexical: INyxPascalLexicalSnapshot;
  const AWindow: TNyxPascalSpan; const AOptions: TNyxPascalTypeOptions;
  const ACancellation: INyxCancellationScope = nil): INyxPascalTypeSyntax;

{ Read a type window in its actual declared-source owner. Structured leaves use
  the owner's checked body/scope identity and existing grammar diagnostics. Names
  remain unbound; full compiler eligibility/layout is not inferred. The original
  standalone factory keeps refusing bodies without this explicit source context. }
function NewNyxPascalDeclaredTypeSyntax(const ADeclarations: INyxPascalSyntaxSnapshot;
  const AWindow: TNyxPascalSpan; const AOptions: TNyxPascalTypeOptions;
  const ACancellation: INyxCancellationScope = nil): INyxPascalTypeSyntax;

{ Capture a complete owned declaration-parser result without copying/reparsing its node
  arena. Nil means unsupported/unowned/incomplete syntax or a different window.
  Invalid owned ordinals and cancellation still refuse. The result owns no source. }
function CaptureNyxPascalTypePlan(const ASyntax: INyxPascalTypeSyntax;
  ADeclaration: Integer;
  const ACancellation: INyxCancellationScope = nil): INyxPascalTypePlan;

{ Rehost only after exact owned declaration, window, policy and structured-body
  identities match. Nil is a normal incompatibility miss, requiring fresh parsing.
  A hit returns a new view owning the current declaration/lexical snapshot; names
  remain unbound. Cancellation is polled outside any caller's memo lock. }
function ApplyNyxPascalTypePlan(const APlan: INyxPascalTypePlan;
  const ADeclarations: INyxPascalSyntaxSnapshot; ADeclaration: Integer;
  const AOptions: TNyxPascalTypeOptions;
  const ACancellation: INyxCancellationScope = nil): INyxPascalTypeSyntax;

implementation

type
  { Borrowed in-unit builder authority, valid only under its interface lease. }
  TTypeSyntax = class;
  { Alternate public type implementations cannot grant capture authority. }
  ITypeSyntaxOwner = interface(IInterface)
    ['{4835469E-09C0-4823-A593-B91B7C22E40D}']
    function TypeSyntax: TTypeSyntax;
  end;
  { Forward immutable plan owner, borrowed under the private interface below. }
  TTypePlan = class;
  { Sealed plan data authority; public Count/Window implementations confer none. }
  ITypePlanOwner = interface(IInterface)
    ['{4FB51AD6-F818-44E4-86C2-C9C4AE7F6045}']
    function Plan: TTypePlan;
  end;
  { Pure value arena shared only after the parser retires its construction state.
    Parallel vectors keep interfaces out of records. Identity markers contain no
    source or owner links; dependencies cover every structured leaf consulted. }
  TTypePlan = class(TInterfacedObject, INyxPascalTypePlan, ITypePlanOwner)
  public
    Nodes: array of TNyxPascalTypeNode;
    Diagnostics: array of TNyxPascalTypeDiagnostic;
    NodeCount: Integer;
    DiagnosticCount: Integer;
    RootNode: Integer;
    SourceWindow: TNyxPascalSpan;
    Policy: TNyxPascalTypeOptions;
    DeclarationIdentity: IInterface;
    DependencyNodes: array of Integer;
    DependencyDeclarations: array of IInterface;
    DependencyScopes: array of IInterface;
    function Window: TNyxPascalSpan;
    function Count: Integer;
    function Plan: TTypePlan;
  end;
  { A source-specific read-only view over pure grammar data. References point
    outward to its current declarations and plan; neither points back here. }
  TPlannedTypeSyntax = class(TInterfacedObject, INyxPascalTypeSyntax,
    INyxPascalDeclaredTypeSyntax)
  public
    DataLease: INyxPascalTypePlan;
    Data: TTypePlan;
    Source: INyxPascalSyntaxSnapshot;
    function Lexical: INyxPascalLexicalSnapshot;
    function Window: TNyxPascalSpan;
    function Root: Integer;
    function Complete: Boolean;
    function Count: Integer;
    function Node(AIndex: Integer): TNyxPascalTypeNode;
    function Child(AParent: Integer; ARole: TNyxPascalTypeRole;
      AOrdinal: Integer): Integer;
    function DiagnosticCount: Integer;
    function Diagnostic(AIndex: Integer): TNyxPascalTypeDiagnostic;
    function Declarations: INyxPascalSyntaxSnapshot;
    function DeclarationOf(ANode: Integer): Integer;
  end;
  { Builder owns an immutable lexical snapshot, local ordinal vectors and a
    temporary cancellation lease. Partial compound symbols are cursor state only;
    no source token is mutated or shared with another parser. }
  TTypeSyntax = class(TInterfacedObject, INyxPascalTypeSyntax,
    INyxPascalDeclaredTypeSyntax, ITypeSyntaxOwner)
  private
    FLexical: INyxPascalLexicalSnapshot;
    FDeclarations: INyxPascalSyntaxSnapshot;
    FWindow: TNyxPascalSpan;
    FOptions: TNyxPascalTypeOptions;
    FTokens: array of Integer;
    FNodes: array of TNyxPascalTypeNode;
    FDiagnostics: array of TNyxPascalTypeDiagnostic;
    FCount: Integer;
    FDiagnosticCount: Integer;
    FRoot: Integer;
    FAt: Integer;
    FSuffix: Boolean;
    FLastOffset: Integer;
    FDepth: Integer;
    FCancellation: INyxCancellationScope;
    procedure CheckCancellation;
    function AtEnd: Boolean;
    { One real token inspection; repeated lookahead remains separately admitted. }
    function ReadToken(AIndex: Integer): TNyxPascalToken;
    { Only the first > of an owned two-character closer may end a type window. }
    function PrefixClose(const AToken: TNyxPascalToken): Boolean;
    function Tok: TNyxPascalToken;
    function Sym(AValue: TNyxPascalSymbol): Boolean;
    function Word(AValue: TNyxPascalWord; ALookAhead: Integer = 0): Boolean;
    function NameToken: Boolean;
    function Key: TNyxText;
    function Offset: Integer;
    procedure Step;
    function CloseGeneric: Boolean;
    function Add(AKind: TNyxPascalTypeKind; AStart: Integer): Integer;
    procedure Finish(AIndex: Integer);
    procedure Link(AParent, AChild: Integer; ARole: TNyxPascalTypeRole);
    procedure Problem(AIssue: TNyxPascalTypeIssue; const AMessage: TNyxText);
    procedure Expect(AValue: TNyxPascalSymbol; const AMessage: TNyxText);
    function ParseType: Integer;
    function ParseName: Integer;
    { Keep an authored string base and one opaque code-page operand distinct.
      Existing bound grouping/quota rules apply; no numeric encoding is guessed. }
    function EncodedString(ABase: Integer): Integer;
    function ParseRoutine: Integer;
    function ParseStructure(AStart: Integer): Integer;
    procedure Parameters(AParent: Integer);
    function Bound(AStop: TNyxPascalSymbol; AComma, ARange: Boolean): Integer;
    function Convention(out AConvention: TNyxPascalCallingConvention): Boolean;
    procedure Build;
  public
    constructor Create(const ALexical: INyxPascalLexicalSnapshot;
      const AWindow: TNyxPascalSpan; const AOptions: TNyxPascalTypeOptions;
      const ACancellation: INyxCancellationScope;
      const ADeclarations: INyxPascalSyntaxSnapshot = nil);
    function Lexical: INyxPascalLexicalSnapshot;
    function Window: TNyxPascalSpan;
    function Root: Integer;
    function Complete: Boolean;
    function Count: Integer;
    function Node(AIndex: Integer): TNyxPascalTypeNode;
    function Child(AParent: Integer; ARole: TNyxPascalTypeRole;
      AOrdinal: Integer): Integer;
    function DiagnosticCount: Integer;
    function Diagnostic(AIndex: Integer): TNyxPascalTypeDiagnostic;
    function Declarations: INyxPascalSyntaxSnapshot;
    function DeclarationOf(ANode: Integer): Integer;
    function TypeSyntax: TTypeSyntax;
  end;

function Capacity(ACount, AMaximum: Integer): Integer;
begin
  Result := AMaximum;

  if 16 + ACount div 2 <= AMaximum - ACount then
  begin
    Result := ACount + 16 + ACount div 2;
  end;
end;

class function TNyxPascalTypeOptions.Defaults: TNyxPascalTypeOptions;
begin
  Result := Default(TNyxPascalTypeOptions);
  Result.FDefined := True;
  Result.FNodes := 4096;
  Result.FDepth := 128;
  Result.FDiagnostics := 128;
end;

procedure TNyxPascalTypeOptions.Validate;
begin

  if not FDefined or (FNodes < 1) or (FDepth < 1) or (FDiagnostics < 1) then
  begin
    raise ENyxPascalLexical.Create('Use explicit positive Pascal type syntax budgets');
  end;
end;

function TNyxPascalTypeOptions.UpToNodes(AValue: Integer): TNyxPascalTypeOptions;
begin
  Result := Self;
  Result.FNodes := AValue;
  Result.Validate;
end;

function TNyxPascalTypeOptions.UpToDepth(AValue: Integer): TNyxPascalTypeOptions;
begin
  Result := Self;
  Result.FDepth := AValue;
  Result.Validate;
end;

function TNyxPascalTypeOptions.UpToDiagnostics(AValue: Integer): TNyxPascalTypeOptions;
begin
  Result := Self;
  Result.FDiagnostics := AValue;
  Result.Validate;
end;

constructor TTypeSyntax.Create(const ALexical: INyxPascalLexicalSnapshot;
  const AWindow: TNyxPascalSpan; const AOptions: TNyxPascalTypeOptions;
  const ACancellation: INyxCancellationScope; const ADeclarations: INyxPascalSyntaxSnapshot);
begin
  inherited Create;
  AOptions.Validate;

  if (ALexical = nil) or (AWindow.Finish > ALexical.Scalars) then
  begin
    raise ENyxPascalLexical.Create('A type window needs an exact lexical source range');
  end;
  FLexical := ALexical;
  FDeclarations := ADeclarations;
  FWindow := AWindow;
  FOptions := AOptions;
  FCancellation := ACancellation;
  try
    CheckCancellation;
    Build;
    CheckCancellation;
    SetLength(FNodes, FCount);
    SetLength(FDiagnostics, FDiagnosticCount);
    FTokens := nil;
  finally
    FCancellation := nil;
  end;
end;

procedure TTypeSyntax.CheckCancellation;
begin

  if (FCancellation <> nil) and FCancellation.Cancelled then
  begin
    raise ENyxPascalAnalysisCancelled.Create('Pascal type analysis cancelled');
  end;
end;

function TTypeSyntax.AtEnd: Boolean;
begin
  Result := FAt >= Length(FTokens);
end;

function TTypeSyntax.ReadToken(AIndex: Integer): TNyxPascalToken;
begin
  NyxPascalAdmitAnalysis(FCancellation, papTypeToken);
  Result := FLexical.Token(AIndex);
end;

function TTypeSyntax.PrefixClose(const AToken: TNyxPascalToken): Boolean;
begin
  { A type can finish at the first > of an original >> or >= token: its
    neighbour belongs to the containing generic declaration/assignment. Borrow
    only this exact one-character prefix, retaining the original lexical owner
    and scalar position. All other significant-token cuts still refuse. }

  Result := AToken.Complete and (AToken.Kind = ptkSymbol) and
    (AToken.Symbol in [psoShiftRight, psoGreaterEqual]) and
    (AToken.Span.Start >= FWindow.Start) and (AToken.Span.Start + 1 = FWindow.Finish) and
    (AToken.Span.Length = 2);
end;

function TTypeSyntax.Tok: TNyxPascalToken;
begin

  if AtEnd then
  begin
    Result := ReadToken(FLexical.Count - 1);
  end
  else
  begin
    Result := ReadToken(FTokens[FAt]);
  end;
end;

function TTypeSyntax.Offset: Integer;
begin
  Result := FWindow.Finish;

  if not AtEnd then
  begin
    Result := Tok.Span.Start;

    if FSuffix then
    begin
      Inc(Result);
    end;
  end;
end;

function TTypeSyntax.Sym(AValue: TNyxPascalSymbol): Boolean;
var
  LToken: TNyxPascalToken;
begin
  Result := False;

  if AtEnd then
  begin
    Exit;
  end;
  LToken := Tok;

  if LToken.Kind <> ptkSymbol then Exit;

  if not FSuffix then
  begin

    if PrefixClose(LToken) then Exit(AValue = psoGreater);
    Result := LToken.Symbol = AValue;
  end
  else
  begin
    Result := ((LToken.Symbol = psoShiftRight) and (AValue = psoGreater)) or
      ((LToken.Symbol = psoGreaterEqual) and (AValue = psoEqual));
  end;
end;

function TTypeSyntax.Word(AValue: TNyxPascalWord; ALookAhead: Integer): Boolean;
var
  LAt: Integer;
  LToken: TNyxPascalToken;
begin
  LAt := FAt + ALookAhead;
  Result := False;

  if (LAt >= 0) and (LAt < Length(FTokens)) then
  begin
    LToken := ReadToken(FTokens[LAt]);
    Result := (LToken.Kind = ptkWord) and not LToken.Escaped and (LToken.Word = AValue);
  end;
end;

function TTypeSyntax.NameToken: Boolean;
begin
  Result := not AtEnd and (Tok.Kind = ptkWord) and
    ((Tok.Word = pkwNone) or Tok.Escaped);
end;

function TTypeSyntax.Key: TNyxText;
begin
  Result := '';

  if not AtEnd and (Tok.Kind = ptkWord) and not Tok.Escaped then
  begin
    Result := NyxPascalASCIIKey(FLexical.Identifier(FTokens[FAt]));
  end;
end;

procedure TTypeSyntax.Step;
begin
  CheckCancellation;

  if not AtEnd then
  begin
    FLastOffset := Tok.Span.Finish;

    if FLastOffset > FWindow.Finish then FLastOffset := FWindow.Finish;
    Inc(FAt);
    FSuffix := False;
  end;
end;

function TTypeSyntax.CloseGeneric: Boolean;
begin
  Result := True;

  if Sym(psoGreater) then
  begin
    Step;
  end
  else if not FSuffix and (Sym(psoShiftRight) or Sym(psoGreaterEqual)) then
  begin
    FSuffix := True;
    FLastOffset := Tok.Span.Start + 1;
  end
  else
  begin
    Result := False;
    Problem(ptiExpectedDelimiter, 'Finish this generic argument list with >');
  end;
end;

function TTypeSyntax.Add(AKind: TNyxPascalTypeKind; AStart: Integer): Integer;
begin
  NyxPascalAdmitAnalysis(FCancellation, papTypeNode);
  Result := FCount;

  if Result >= FOptions.MaximumNodes then
  begin
    raise ENyxPascalTypeLimit.Create('Pascal type nodes exceed their budget');
  end;

  if Result = Length(FNodes) then
  begin
    SetLength(FNodes, Capacity(Result, FOptions.MaximumNodes));
  end;
  FNodes[Result].FKind := AKind;
  FNodes[Result].FSpan := NyxPascalSpan(AStart, 0);
  FNodes[Result].FParent := -1;
  FNodes[Result].FFirst := -1;
  FNodes[Result].FLast := -1;
  FNodes[Result].FNext := -1;
  FNodes[Result].FDeclaration := -1;
  Inc(FCount);
end;

procedure TTypeSyntax.Finish(AIndex: Integer);
var
  LEnd: Integer;
begin
  LEnd := FLastOffset;

  if LEnd < FNodes[AIndex].Span.Start then
  begin
    LEnd := FNodes[AIndex].Span.Start;
  end;
  FNodes[AIndex].FSpan := NyxPascalSpan(FNodes[AIndex].Span.Start,
    LEnd - FNodes[AIndex].Span.Start);
end;

procedure TTypeSyntax.Link(AParent, AChild: Integer; ARole: TNyxPascalTypeRole);
begin

  if FNodes[AChild].Parent <> -1 then
  begin
    raise ENyxPascalLexical.Create('A type syntax node cannot acquire two parents');
  end;
  FNodes[AChild].FParent := AParent;
  FNodes[AChild].FRole := ARole;

  if FNodes[AParent].FFirst < 0 then
  begin
    FNodes[AParent].FFirst := AChild;
  end
  else
  begin
    FNodes[FNodes[AParent].FLast].FNext := AChild;
  end;
  FNodes[AParent].FLast := AChild;
end;

procedure TTypeSyntax.Problem(AIssue: TNyxPascalTypeIssue; const AMessage: TNyxText);
begin
  NyxPascalAdmitAnalysis(FCancellation, papDiagnosticRow);

  if FDiagnosticCount >= FOptions.MaximumDiagnostics then
  begin
    raise ENyxPascalTypeLimit.Create('Pascal type diagnostics exceed their budget');
  end;

  if FDiagnosticCount = Length(FDiagnostics) then
  begin
    SetLength(FDiagnostics, Capacity(FDiagnosticCount, FOptions.MaximumDiagnostics));
  end;
  FDiagnostics[FDiagnosticCount].FIssue := AIssue;
  FDiagnostics[FDiagnosticCount].FSpan := NyxPascalSpan(Offset, 0);
  FDiagnostics[FDiagnosticCount].FMessage := AMessage;
  Inc(FDiagnosticCount);
end;

procedure TTypeSyntax.Expect(AValue: TNyxPascalSymbol; const AMessage: TNyxText);
begin

  if Sym(AValue) then
  begin
    Step;
  end
  else
  begin
    Problem(ptiExpectedDelimiter, AMessage);
  end;
end;

function TTypeSyntax.ParseName: Integer;
var
  LPart: Integer;
  LCombined: Integer;
  LGeneric: Integer;
  LBefore: Integer;
  LCanSpecialize: Boolean;
begin
  Result := Add(ptxName, Offset);

  if not NameToken and not Word(pkwString) then
  begin
    FNodes[Result].FKind := ptxMissing;
    Problem(ptiExpectedName, 'Add a Pascal type name');
    Exit;
  end;
  FNodes[Result].FName := FLexical.Identifier(FTokens[FAt]);
  Step;
  Finish(Result);
  LCanSpecialize := True;
  while True do
  begin

    if Sym(psoLess) then
    begin

      if not LCanSpecialize then
      begin
        Problem(ptiUnsupportedType, 'A type segment cannot acquire two argument lists');
        Break;
      end;
      LGeneric := Add(ptxGeneric, FNodes[Result].Span.Start);
      Link(LGeneric, Result, ptrBase);
      Step;

      if Sym(psoGreater) or Sym(psoShiftRight) or AtEnd then
      begin
        Problem(ptiExpectedType, 'A generic argument list needs a type');
      end
      else
      begin
        repeat
          LBefore := FAt;
          LPart := ParseType;
          Link(LGeneric, LPart, ptrArgument);

          if not Sym(psoComma) then
          begin
            Break;
          end;
          Step;

          if (FAt = LBefore) or AtEnd then
          begin
            Problem(ptiExpectedType, 'Add the next generic argument type');
            Break;
          end;
        until False;
      end;
      CloseGeneric;
      Finish(LGeneric);
      Result := LGeneric;
      LCanSpecialize := False;
    end
    else if Sym(psoDot) then
    begin
      LCombined := Add(ptxQualified, FNodes[Result].Span.Start);
      Link(LCombined, Result, ptrBase);
      Step;
      LPart := Add(ptxName, Offset);

      if not NameToken then
      begin
        FNodes[LPart].FKind := ptxMissing;
        Problem(ptiExpectedName, 'A qualified type needs a member name');
        Link(LCombined, LPart, ptrMember);
        Finish(LCombined);
        Result := LCombined;
        Break;
      end;
      FNodes[LPart].FName := FLexical.Identifier(FTokens[FAt]);
      Step;
      Finish(LPart);
      Link(LCombined, LPart, ptrMember);
      Finish(LCombined);
      Result := LCombined;
      LCanSpecialize := True;
    end
    else
    begin
      Break;
    end;
  end;
end;

function TTypeSyntax.EncodedString(ABase: Integer): Integer;
begin
  Result := Add(ptxEncodedString, FNodes[ABase].Span.Start);
  Link(Result, ABase, ptrBase);
  Step;
  Link(Result, Bound(psoCloseParen, True, False), ptrCodePage);
  Expect(psoCloseParen, 'Finish the single string code-page expression with )');
  Finish(Result);
end;

function TTypeSyntax.Bound(AStop: TNyxPascalSymbol; AComma, ARange: Boolean): Integer;
var
  LStack: array of TNyxPascalSymbol;
  LDepth: Integer;
begin
  Result := Add(ptxBound, Offset);
  LDepth := 0;
  while not AtEnd do
  begin

    if (LDepth = 0) and (Sym(AStop) or Sym(psoSemicolon) or Sym(psoCloseParen) or
      (AComma and Sym(psoComma)) or (ARange and Sym(psoRange)) or
      Sym(psoGreater) or Sym(psoShiftRight) or Sym(psoGreaterEqual)) then
    begin
      Break;
    end;

    if Sym(psoOpenParen) or Sym(psoOpenBracket) then
    begin

      if LDepth >= FOptions.MaximumDepth then
      begin
        raise ENyxPascalTypeLimit.Create('Pascal type bound grouping exceeds its budget');
      end;
      SetLength(LStack, LDepth + 1);
      LStack[LDepth] := psoCloseParen;

      if Sym(psoOpenBracket) then
      begin
        LStack[LDepth] := psoCloseBracket;
      end;
      Inc(LDepth);
    end
    else if Sym(psoCloseParen) or Sym(psoCloseBracket) then
    begin

      if (LDepth = 0) or not Sym(LStack[LDepth - 1]) then
      begin
        Problem(ptiExpectedDelimiter, 'This bound has an unmatched delimiter');
        Break;
      end;
      Dec(LDepth);
    end;
    Step;
  end;

  if (FLastOffset < FNodes[Result].Span.Start) or
    (FLastOffset = FNodes[Result].Span.Start) then
  begin
    Problem(ptiExpectedType, 'Add an ordinal bound or constant expression');
  end;

  if LDepth <> 0 then
  begin
    Problem(ptiExpectedDelimiter, 'Finish this bound expression group');
  end;
  Finish(Result);
end;

function TTypeSyntax.Convention(out AConvention: TNyxPascalCallingConvention): Boolean;
begin
  AConvention := pccDefault;

  if Tok.Escaped then
  begin
    Exit(False);
  end;
  Result := NyxPascalCallingConventionNamed(Key, AConvention);
end;

procedure TTypeSyntax.Parameters(AParent: Integer);
var
  LGroup: Integer;
  LName: Integer;
  LType: Integer;
  LBefore: Integer;
  LMode: TNyxPascalSyntaxParameterMode;
begin
  Step;
  while not AtEnd and not Sym(psoCloseParen) do
  begin
    LBefore := FAt;
    LMode := pspmValue;
    LGroup := Add(ptxParameterGroup, Offset);
    case Tok.Word of
      pkwConst:
        begin
          LMode := pspmConst;
          Step;
        end;
      pkwConstref:
        begin
          LMode := pspmConstRef;
          Step;
        end;
      pkwVar:
        begin
          LMode := pspmVar;
          Step;
        end;
      pkwOut:
        begin
          LMode := pspmOut;
          Step;
        end;
    else
      begin
        { An ordinary value parameter has no prefix. }
      end;
    end;
    FNodes[LGroup].FMode := LMode;
    repeat
      LName := Add(ptxParameterName, Offset);

      if not NameToken then
      begin
        Problem(ptiExpectedName, 'A parameter group needs a name');
        Link(LGroup, LName, ptrParameterName);
        Break;
      end;
      FNodes[LName].FName := FLexical.Identifier(FTokens[FAt]);
      Step;
      Finish(LName);
      Link(LGroup, LName, ptrParameterName);

      if not Sym(psoComma) then
      begin
        Break;
      end;
      Step;
    until AtEnd;

    if Sym(psoColon) then
    begin
      Step;
      LType := ParseType;
    end
    else if LMode in [pspmConst, pspmVar, pspmOut] then
    begin
      LType := Add(ptxUntyped, Offset);
    end
    else
    begin
      LType := Add(ptxMissing, Offset);
      Problem(ptiExpectedType, 'A value parameter needs its declared type');
    end;
    Link(LGroup, LType, ptrParameterType);

    if Sym(psoEqual) then
    begin
      Step;
      Link(LGroup, Bound(psoCloseParen, False, False), ptrDefault);
    end;
    Finish(LGroup);
    Link(AParent, LGroup, ptrParameter);

    if not Sym(psoSemicolon) then
    begin
      Break;
    end;
    Step;

    if FAt = LBefore then
    begin
      Break;
    end;
  end;
  Expect(psoCloseParen, 'Finish this procedural parameter list with )');
end;

function TTypeSyntax.ParseRoutine: Integer;
var
  LFunction: Boolean;
  LConvention: TNyxPascalCallingConvention;
begin
  LFunction := Word(pkwFunction);
  Result := Add(ptxProcedure, Offset);

  if LFunction then
  begin
    FNodes[Result].FKind := ptxFunction;
  end;
  Step;

  if Sym(psoOpenParen) then
  begin
    Parameters(Result);
  end;

  if LFunction then
  begin
    Expect(psoColon, 'A function type needs a result colon');
    Link(Result, ParseType, ptrResult);
  end;
  while not AtEnd do
  begin

    if Word(pkwOf) then
    begin
      Step;

      if not Word(pkwObject) then
      begin
        Problem(ptiExpectedType, 'A method pointer needs of object');
        Break;
      end;
      Include(FNodes[Result].FFlags, ptfOfObject);
      Step;
    end
    else if Word(pkwIs) then
    begin
      Step;

      if Key <> 'nested' then
      begin
        Problem(ptiUnsupportedType, 'This procedural qualifier needs richer analysis');
        Break;
      end;
      Include(FNodes[Result].FFlags, ptfNested);
      Step;
    end
    else if Convention(LConvention) then
    begin

      if FNodes[Result].FConvention <> pccDefault then
      begin
        Problem(ptiUnsupportedType, 'A procedural type has conflicting calling conventions');
      end
      else
      begin
        FNodes[Result].FConvention := LConvention;
      end;
      Step;
    end
    else if Word(pkwVarargs) or Word(pkwInline) then
    begin

      if Word(pkwVarargs) then
      begin
        Include(FNodes[Result].FFlags, ptfVarargs);
      end
      else
      begin
        Include(FNodes[Result].FFlags, ptfInline);
      end;
      Step;
    end
    else if Sym(psoSemicolon) then
    begin
      { A convention follows its signature separator, not an arbitrary following
        declaration. The builder's bounded window remains the owning boundary. }
      Inc(FAt);

      if not AtEnd and (Convention(LConvention) or Word(pkwVarargs) or Word(pkwInline)) then
      begin
        FLastOffset := ReadToken(FTokens[FAt - 1]).Span.Finish;
      end
      else
      begin
        Dec(FAt);
        Break;
      end;
    end
    else
    begin
      Break;
    end;
  end;
  Finish(Result);
end;

function TTypeSyntax.ParseStructure(AStart: Integer): Integer;
var
  LStructures: INyxPascalStructuredSyntax;
  LAt: Integer;
  LDeclaration: TNyxPascalSyntaxDeclaration;
begin
  Result := Add(ptxStructured, AStart);

  if (FDeclarations = nil) or not Supports(FDeclarations, INyxPascalStructuredSyntax, LStructures) then
  begin
    Problem(ptiContextRequired, 'A structured type needs its exact declaration owner');
    Step;
    Exit;
  end;
  LAt := LStructures.TypeAt(AStart);

  if LAt < 0 then
  begin
    Problem(ptiContextRequired, 'This structured body is not registered in the declared source');
    Step;
    Exit;
  end;
  LDeclaration := FDeclarations.Declaration(LAt);

  if LDeclaration.TypeSpan.Finish > FWindow.Finish then
  begin
    Problem(ptiExpectedDelimiter, 'The type window cuts its owning structured body');
    Step;
    Exit;
  end;
  FNodes[Result].FDeclaration := LAt;

  if (LDeclaration.NestedScope < 0) or
    (LDeclaration.Flags * [psdfIncomplete, psdfConditional, psdfUnsupported, psdfForward] <> []) then
  begin
    Problem(ptiUnsupportedType, 'This structured declaration is incomplete or unsupported');
  end
  else if not FDeclarations.Scope(LDeclaration.NestedScope).Complete or
    FDeclarations.Scope(LDeclaration.NestedScope).UnsupportedSyntax then
  begin
    Problem(ptiUnsupportedType, 'This structured member scope is incomplete or unsupported');
  end;
  { The declaration parser owns body grammar and nesting budgets. Consume only
    its exact range; never scan for a convenient end or reparse copied text. }
  while not AtEnd and (Tok.Span.Start < LDeclaration.TypeSpan.Finish) do
  begin
    Step;
  end;
  Finish(Result);
end;

function TTypeSyntax.ParseType: Integer;
var
  LStart: Integer;
  LChild: Integer;
  LBase: Integer;
  LMember: Integer;
  LPacking: TNyxPascalTypeFlags;
  LSpecialize, LAnsiString: Boolean;
begin
  CheckCancellation;

  if FDepth >= FOptions.MaximumDepth then
  begin
    raise ENyxPascalTypeLimit.Create('Pascal type nesting exceeds its budget');
  end;
  Inc(FDepth);
  try
    LStart := Offset;
    LPacking := [];

    if Word(pkwPacked) or (Key = 'bitpacked') then
    begin
      Include(LPacking, ptfPacked);

      if Key = 'bitpacked' then
      begin
        Include(LPacking, ptfBitPacked);
      end;
      Step;
    end;

    if Word(pkwType) or Word(pkwReference) then
    begin
      Result := Add(ptxDistinct, LStart);

      if Word(pkwReference) then
      begin
        FNodes[Result].FKind := ptxReference;
        Step;

        if not Word(pkwTo) then
        begin
          Problem(ptiExpectedType, 'A function reference needs reference to');
        end
        else
        begin
          Step;
        end;

        if not Word(pkwFunction) and not Word(pkwProcedure) then
        begin
          Problem(ptiExpectedType, 'A function reference needs a procedural signature');
        end;
      end
      else
      begin
        Step;
      end;
      Link(Result, ParseType, ptrTarget);
    end
    else if Sym(psoCaret) then
    begin
      Result := Add(ptxPointer, LStart);
      Step;
      Link(Result, ParseType, ptrPointee);
    end
    else if Word(pkwRecord) or Word(pkwObject) or Word(pkwInterface) or Word(pkwDispinterface) then
    begin
      Result := ParseStructure(LStart);
    end
    else if Word(pkwClass) then
    begin

      if not Word(pkwOf, 1) then
      begin
        Result := ParseStructure(LStart);
      end
      else
      begin
        Result := Add(ptxClassOf, LStart);
        Step;
        Step;
        Link(Result, ParseType, ptrTarget);
      end;
    end
    else if Word(pkwArray) or Word(pkwSet) or Word(pkwFile) then
    begin
      Result := Add(ptxArray, LStart);

      if Word(pkwSet) then
      begin
        FNodes[Result].FKind := ptxSet;
      end
      else if Word(pkwFile) then
      begin
        FNodes[Result].FKind := ptxFile;
      end;
      Step;

      if Sym(psoOpenBracket) then
      begin

        if FNodes[Result].Kind <> ptxArray then
        begin
          Problem(ptiUnsupportedType, 'Only array types have dimension brackets');
        end;
        Step;
        repeat
          Link(Result, Bound(psoCloseBracket, True, False), ptrDimension);

          if not Sym(psoComma) then
          begin
            Break;
          end;
          Step;
        until AtEnd;
        Expect(psoCloseBracket, 'Finish the array dimensions with ]');
      end;

      if Word(pkwOf) then
      begin
        Step;

        if Word(pkwConst) and (FNodes[Result].Kind = ptxArray) then
        begin
          LChild := Add(ptxUntyped, Offset);
          FNodes[LChild].FName := 'const';
          Step;
          Finish(LChild);
          Link(Result, LChild, ptrElement);
        end
        else
        begin
          Link(Result, ParseType, ptrElement);
        end;
      end
      else if FNodes[Result].Kind <> ptxFile then
      begin
        Problem(ptiExpectedType, 'An array or set needs its element type after of');
      end;
    end
    else if Word(pkwProcedure) or Word(pkwFunction) then
    begin
      Result := ParseRoutine;
    end
    else if Word(pkwString) then
    begin
      Result := Add(ptxString, LStart);
      FNodes[Result].FName := FLexical.Identifier(FTokens[FAt]);
      Step;

      if Sym(psoOpenBracket) then
      begin
        Step;
        Link(Result, Bound(psoCloseBracket, False, False), ptrLength);
        Expect(psoCloseBracket, 'Finish the short-string length with ]');
      end
      else if Sym(psoOpenParen) then
      begin
        Finish(Result);
        Result := EncodedString(Result);
      end;
    end
    else if Sym(psoOpenParen) then
    begin
      Result := Add(ptxEnumeration, LStart);
      Step;
      repeat
        LMember := Add(ptxEnumMember, Offset);

        if not NameToken then
        begin
          Problem(ptiExpectedName, 'An enumeration needs a member name');
          Link(Result, LMember, ptrEnumMember);
          Break;
        end;
        FNodes[LMember].FName := FLexical.Identifier(FTokens[FAt]);
        Step;

        if Sym(psoAssign) or Sym(psoEqual) then
        begin
          Step;
          Link(LMember, Bound(psoCloseParen, True, False), ptrDefault);
        end;
        Finish(LMember);
        Link(Result, LMember, ptrEnumMember);

        if not Sym(psoComma) then
        begin
          Break;
        end;
        Step;
      until AtEnd;
      Expect(psoCloseParen, 'Finish this enumeration with )');
    end
    else if Word(pkwSpecialize) or NameToken then
    begin
      LSpecialize := Word(pkwSpecialize);
      LAnsiString := Key = 'ansistring';

      if LSpecialize then
      begin
        Step;
      end;
      Result := ParseName;

      if LAnsiString and (FNodes[Result].Kind = ptxName) and Sym(psoOpenParen) then
        Result := EncodedString(Result);

      if LSpecialize then
      begin
        FNodes[Result].FSpan := NyxPascalSpan(LStart, FLastOffset - LStart);
        Include(FNodes[Result].FFlags, ptfSpecialize);
      end;

      if Sym(psoRange) then
      begin
        LBase := Result;
        Result := Add(ptxSubrange, LStart);
        Link(Result, LBase, ptrLowerBound);
        Step;
        Link(Result, Bound(psoCloseBracket, True, False), ptrUpperBound);
      end;
    end
    else if not AtEnd and ((Tok.Kind in [ptkNumber, ptkString, ptkCharacter]) or
      Sym(psoPlus) or Sym(psoMinus)) then
    begin
      LBase := Bound(psoRange, True, True);
      Result := Add(ptxSubrange, LStart);
      Link(Result, LBase, ptrLowerBound);
      Expect(psoRange, 'An ordinal type expression needs a lower..upper range');
      Link(Result, Bound(psoCloseBracket, True, False), ptrUpperBound);
    end
    else
    begin
      Result := Add(ptxMissing, LStart);
      Problem(ptiExpectedType, 'Add a supported Pascal type expression');

      if not AtEnd and not Sym(psoComma) and not Sym(psoCloseParen) and
        not Sym(psoGreater) and not Sym(psoShiftRight) and not Sym(psoSemicolon) then
      begin
        Step;
      end;
    end;
    FNodes[Result].FFlags := FNodes[Result].FFlags + LPacking;
    Finish(Result);
  finally
    Dec(FDepth);
  end;
end;

procedure TTypeSyntax.Build;
var
  LAt: Integer;
  LCount: Integer;
  LToken: TNyxPascalToken;
begin
  LCount := 0;
  LAt := FLexical.TokenAt(FWindow.Start);
  { A small requested type window allocates only its own ordinal interval,
    rather than every subsequent token in a large owning unit. }
  SetLength(FTokens, FLexical.TokenAt(FWindow.Finish) - LAt + 1);
  while LAt < FLexical.Count do
  begin
    CheckCancellation;
    LToken := ReadToken(LAt);

    if (LToken.Kind = ptkEOF) or (LToken.Span.Start >= FWindow.Finish) then
    begin
      Break;
    end;

    if not (LToken.Kind in [ptkWhitespace, ptkComment]) or not LToken.Complete then
    begin

      if (LToken.Span.Start < FWindow.Start) or
        ((LToken.Span.Finish > FWindow.Finish) and not PrefixClose(LToken)) then
      begin
        raise ENyxPascalLexical.Create('A type window cannot cut a significant token');
      end;
      FTokens[LCount] := LAt;
      Inc(LCount);
    end;
    Inc(LAt);
  end;
  SetLength(FTokens, LCount);
  FLastOffset := FWindow.Start;

  for LAt := 0 to High(FTokens) do
  begin

    if ReadToken(FTokens[LAt]).Kind = ptkDirective then
    begin
      FAt := LAt;
      Problem(ptiContextRequired, 'Translate conditional/include directives before type analysis');
    end;

    if not ReadToken(FTokens[LAt]).Complete or
      (ReadToken(FTokens[LAt]).Kind = ptkInvalid) then
    begin
      FAt := LAt;
      Problem(ptiUnsupportedType, 'This type contains an incomplete or invalid lexical token');
    end;
  end;
  FAt := 0;
  FRoot := ParseType;

  if Sym(psoSemicolon) then
  begin
    Step;
  end;

  if not AtEnd then
  begin
    Problem(ptiTrailingSyntax, 'This type has additional unclassified syntax');
  end;
end;

function TTypeSyntax.Lexical: INyxPascalLexicalSnapshot;
begin
  Result := FLexical;
end;

function TTypeSyntax.Window: TNyxPascalSpan;
begin
  Result := FWindow;
end;

function TTypeSyntax.Root: Integer;
begin
  Result := FRoot;
end;

function TTypeSyntax.Complete: Boolean;
begin
  Result := FDiagnosticCount = 0;
end;

function TTypeSyntax.Count: Integer;
begin
  Result := FCount;
end;

function TTypeSyntax.Node(AIndex: Integer): TNyxPascalTypeNode;
begin

  if (AIndex < 0) or (AIndex >= FCount) then
  begin
    raise ENyxPascalLexical.Create('Type node index is outside its immutable syntax');
  end;
  Result := FNodes[AIndex];
end;

function TTypeSyntax.Child(AParent: Integer; ARole: TNyxPascalTypeRole;
  AOrdinal: Integer): Integer;
begin

  if AOrdinal < 0 then
  begin
    raise ENyxPascalLexical.Create('A type child ordinal cannot be negative');
  end;
  Result := Node(AParent).FirstChild;
  while Result >= 0 do
  begin

    if FNodes[Result].Role = ARole then
    begin

      if AOrdinal = 0 then
      begin
        Exit;
      end;
      Dec(AOrdinal);
    end;
    Result := FNodes[Result].NextSibling;
  end;
end;

function TTypeSyntax.DiagnosticCount: Integer;
begin
  Result := FDiagnosticCount;
end;

function TTypeSyntax.Diagnostic(AIndex: Integer): TNyxPascalTypeDiagnostic;
begin

  if (AIndex < 0) or (AIndex >= FDiagnosticCount) then
  begin
    raise ENyxPascalLexical.Create('Type diagnostic index is outside its immutable syntax');
  end;
  Result := FDiagnostics[AIndex];
end;

function NewNyxPascalTypeSyntax(const ALexical: INyxPascalLexicalSnapshot;
  const AWindow: TNyxPascalSpan; const AOptions: TNyxPascalTypeOptions;
  const ACancellation: INyxCancellationScope): INyxPascalTypeSyntax;
begin
  Result := TTypeSyntax.Create(ALexical, AWindow, AOptions, ACancellation);
end;

function TTypeSyntax.Declarations: INyxPascalSyntaxSnapshot;
begin
  Result := FDeclarations;
end;

function TTypeSyntax.DeclarationOf(ANode: Integer): Integer;
begin
  Node(ANode);
  Result := FNodes[ANode].FDeclaration;
end;

function NewNyxPascalDeclaredTypeSyntax(const ADeclarations: INyxPascalSyntaxSnapshot;
  const AWindow: TNyxPascalSpan; const AOptions: TNyxPascalTypeOptions;
  const ACancellation: INyxCancellationScope): INyxPascalTypeSyntax;
begin

  if ADeclarations = nil then
  begin
    raise ENyxPascalLexical.Create('Declared type syntax needs its exact source snapshot');
  end;
  Result := TTypeSyntax.Create(ADeclarations.Lexical, AWindow, AOptions, ACancellation, ADeclarations);
end;

function TTypeSyntax.TypeSyntax: TTypeSyntax;
begin
  Result := Self;
end;

function TTypePlan.Window: TNyxPascalSpan;
begin
  Result := SourceWindow;
end;

function TTypePlan.Count: Integer;
begin
  Result := NodeCount;
end;

function TTypePlan.Plan: TTypePlan;
begin
  Result := Self;
end;

procedure CheckPlanCancellation(const ACancellation: INyxCancellationScope);
begin

  if (ACancellation <> nil) and ACancellation.Cancelled then
  begin
    raise ENyxPascalAnalysisCancelled.Create('Pascal type-plan construction cancelled');
  end;
end;

function CaptureNyxPascalTypePlan(const ASyntax: INyxPascalTypeSyntax;
  ADeclaration: Integer; const ACancellation: INyxCancellationScope): INyxPascalTypePlan;
var
  LOwner: ITypeSyntaxOwner;
  LType: TTypeSyntax;
  LPlan: TTypePlan;
  LLease: INyxPascalTypePlan;
  LIdentity: IInterface;
  LDeclaration: TNyxPascalSyntaxDeclaration;
  LAt: Integer;
  LDependency: Integer;
  LScopeIdentity: IInterface;
begin
  CheckPlanCancellation(ACancellation);
  Result := nil;

  if not Supports(ASyntax, ITypeSyntaxOwner, LOwner) or not ASyntax.Complete then
  begin
    Exit;
  end;
  LType := LOwner.TypeSyntax;

  if LType.FDeclarations = nil then
  begin
    Exit;
  end;
  LDeclaration := LType.FDeclarations.Declaration(ADeclaration);
  LIdentity := NyxPascalDeclarationIdentity(LType.FDeclarations, ADeclaration);

  if (LIdentity = nil) or (LDeclaration.TypeSpan.Start <> LType.FWindow.Start) or
    (LDeclaration.TypeSpan.Length <> LType.FWindow.Length) then
  begin
    Exit;
  end;
  LPlan := TTypePlan.Create;
  LLease := LPlan;
  LPlan.Nodes := LType.FNodes;
  LPlan.Diagnostics := LType.FDiagnostics;
  LPlan.NodeCount := LType.FCount;
  LPlan.DiagnosticCount := LType.FDiagnosticCount;
  LPlan.RootNode := LType.FRoot;
  LPlan.SourceWindow := LType.FWindow;
  LPlan.Policy := LType.FOptions;
  LPlan.DeclarationIdentity := LIdentity;
  for LAt := 0 to LPlan.NodeCount - 1 do
  begin
    CheckPlanCancellation(ACancellation);
    NyxPascalAdmitAnalysis(ACancellation, papTypeProof);

    if LPlan.Nodes[LAt].FDeclaration < 0 then
    begin
      Continue;
    end;
    LDeclaration := LType.FDeclarations.Declaration(LPlan.Nodes[LAt].FDeclaration);

    if LDeclaration.NestedScope < 0 then
    begin
      Exit;
    end;
    LIdentity := NyxPascalDeclarationIdentity(LType.FDeclarations, LPlan.Nodes[LAt].FDeclaration);
    LScopeIdentity := NyxPascalScopeIdentity(LType.FDeclarations, LDeclaration.NestedScope);

    if (LIdentity = nil) or (LScopeIdentity = nil) then
    begin
      Exit;
    end;
    LDependency := Length(LPlan.DependencyNodes);
    SetLength(LPlan.DependencyNodes, LDependency + 1);
    SetLength(LPlan.DependencyDeclarations, LDependency + 1);
    SetLength(LPlan.DependencyScopes, LDependency + 1);
    LPlan.DependencyNodes[LDependency] := LAt;
    LPlan.DependencyDeclarations[LDependency] := LIdentity;
    LPlan.DependencyScopes[LDependency] := LScopeIdentity;
  end;
  CheckPlanCancellation(ACancellation);
  Result := LLease;
end;

function ApplyNyxPascalTypePlan(const APlan: INyxPascalTypePlan;
  const ADeclarations: INyxPascalSyntaxSnapshot; ADeclaration: Integer;
  const AOptions: TNyxPascalTypeOptions;
  const ACancellation: INyxCancellationScope): INyxPascalTypeSyntax;
var
  LOwner: ITypePlanOwner;
  LPlan: TTypePlan;
  LDeclaration: TNyxPascalSyntaxDeclaration;
  LStructures: INyxPascalStructuredSyntax;
  LAt: Integer;
  LNode: TNyxPascalTypeNode;
  LView: TPlannedTypeSyntax;
begin
  AOptions.Validate;
  CheckPlanCancellation(ACancellation);
  NyxPascalAdmitAnalysis(ACancellation, papTypeProof);
  Result := nil;

  if (ADeclarations = nil) or not Supports(APlan, ITypePlanOwner, LOwner) then
  begin
    Exit;
  end;
  LPlan := LOwner.Plan;
  LDeclaration := ADeclarations.Declaration(ADeclaration);

  if (NyxPascalDeclarationIdentity(ADeclarations, ADeclaration) <> LPlan.DeclarationIdentity) or
    (LDeclaration.TypeSpan.Start <> LPlan.SourceWindow.Start) or
    (LDeclaration.TypeSpan.Length <> LPlan.SourceWindow.Length) or
    (AOptions.MaximumNodes <> LPlan.Policy.MaximumNodes) or
    (AOptions.MaximumDepth <> LPlan.Policy.MaximumDepth) or
    (AOptions.MaximumDiagnostics <> LPlan.Policy.MaximumDiagnostics) then
  begin
    Exit;
  end;

  if (Length(LPlan.DependencyNodes) > 0) and
    not Supports(ADeclarations, INyxPascalStructuredSyntax, LStructures) then
  begin
    Exit;
  end;
  for LAt := 0 to High(LPlan.DependencyNodes) do
  begin
    CheckPlanCancellation(ACancellation);
    NyxPascalAdmitAnalysis(ACancellation, papTypeProof);
    LNode := LPlan.Nodes[LPlan.DependencyNodes[LAt]];

    if LStructures.TypeAt(LNode.Span.Start) <> LNode.FDeclaration then
    begin
      Exit;
    end;
    LDeclaration := ADeclarations.Declaration(LNode.FDeclaration);

    if (LDeclaration.NestedScope < 0) or
      (NyxPascalDeclarationIdentity(ADeclarations, LNode.FDeclaration) <>
        LPlan.DependencyDeclarations[LAt]) or
      (NyxPascalScopeIdentity(ADeclarations, LDeclaration.NestedScope) <>
        LPlan.DependencyScopes[LAt]) then
    begin
      Exit;
    end;
  end;
  CheckPlanCancellation(ACancellation);
  LView := TPlannedTypeSyntax.Create;
  Result := LView;
  LView.DataLease := APlan;
  LView.Data := LPlan;
  LView.Source := ADeclarations;
end;

function TPlannedTypeSyntax.Lexical: INyxPascalLexicalSnapshot;
begin
  Result := Source.Lexical;
end;

function TPlannedTypeSyntax.Window: TNyxPascalSpan;
begin
  Result := DataLease.Window;
end;

function TPlannedTypeSyntax.Root: Integer;
begin
  Result := Data.RootNode;
end;

function TPlannedTypeSyntax.Complete: Boolean;
begin
  Result := Data.DiagnosticCount = 0;
end;

function TPlannedTypeSyntax.Count: Integer;
begin
  Result := DataLease.Count;
end;

function TPlannedTypeSyntax.Node(AIndex: Integer): TNyxPascalTypeNode;
begin

  if (AIndex < 0) or (AIndex >= Data.NodeCount) then
  begin
    raise ENyxPascalLexical.Create('Type node index is outside its immutable plan');
  end;
  Result := Data.Nodes[AIndex];
end;

function TPlannedTypeSyntax.Child(AParent: Integer; ARole: TNyxPascalTypeRole;
  AOrdinal: Integer): Integer;
begin

  if AOrdinal < 0 then
  begin
    raise ENyxPascalLexical.Create('A type child ordinal cannot be negative');
  end;
  Result := Node(AParent).FirstChild;
  while Result >= 0 do
  begin

    if Node(Result).Role = ARole then
    begin

      if AOrdinal = 0 then
      begin
        Exit;
      end;
      Dec(AOrdinal);
    end;
    Result := Node(Result).NextSibling;
  end;
end;

function TPlannedTypeSyntax.DiagnosticCount: Integer;
begin
  Result := Data.DiagnosticCount;
end;

function TPlannedTypeSyntax.Diagnostic(AIndex: Integer): TNyxPascalTypeDiagnostic;
begin

  if (AIndex < 0) or (AIndex >= Data.DiagnosticCount) then
  begin
    raise ENyxPascalLexical.Create('Type diagnostic index is outside its immutable plan');
  end;
  Result := Data.Diagnostics[AIndex];
end;

function TPlannedTypeSyntax.Declarations: INyxPascalSyntaxSnapshot;
begin
  Result := Source;
end;

function TPlannedTypeSyntax.DeclarationOf(ANode: Integer): Integer;
begin
  Node(ANode);
  Result := Data.Nodes[ANode].FDeclaration;
end;

end.
