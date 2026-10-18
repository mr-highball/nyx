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
unit nyx.pascal.syntax;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.scheduler, nyx.pascal.lexing;

type
  { Scope/entry/diagnostic/depth quotas refuse the complete candidate rather than
    presenting a truncated structure as a usable language index. }
  ENyxPascalSyntaxLimit = class(ENyxPascalLexicalLimit);
  { An explicit physical-file starting context. Includes must receive their
    actual enclosing context; no filename or text heuristic grants that context. }
  TNyxPascalSyntaxFragment = (psfModule, psfDeclarations, psfTypes, psfMembers,
    psfInterfaceDeclarations);
  { Lexical containment, independent of resolved symbols or runtime ownership.
    Routine scopes include parameters/locals. Statement blocks retain boundaries;
    inline declarations and anonymous scopes explicitly require richer analysis. }
  TNyxPascalSyntaxScopeKind = (psskModule, psskMembers, psskRoutine, psskBlock);
  { Declared intent only. A typed variable/property preserves its type expression;
    a routine preserves its complete signature, result, parameters and owner name. }
  TNyxPascalSyntaxDeclarationKind = (psdkType, psdkConstant, psdkVariable,
    psdkField, psdkParameter, psdkProperty, psdkRoutine, psdkEnumValue, psdkResourceString,
    psdkLabel);
  { Closed operator family, independent of its authored symbolic/named spelling.
    None is an ordinary declaration; Unknown retains unsupported operator syntax.
    Arity, operand/result types and compiler eligibility remain separate facts;
    family equality never selects an overload or implies conversion equivalence. }
  TNyxPascalOperatorKind = (pokNone, pokUnknown, pokAdd, pokSubtract, pokMultiply,
    pokDivide, pokIntDivide, pokModulus, pokPower, pokEqual, pokNotEqual, pokLess,
    pokLessEqual, pokGreater, pokGreaterEqual, pokImplicit, pokExplicit, pokNot,
    pokAnd, pokOr, pokXor, pokShiftLeft, pokShiftRight, pokIn,
    pokSymmetricDifference, pokInc, pokDec, pokEnumerator, pokInitialize,
    pokFinalize, pokAddRef, pokCopy);
  { Surface type construction. Named expressions may still be generic/qualified;
    neither this form nor an ancestor expression claims semantic resolution. }
  TNyxPascalSyntaxTypeForm = (pstfNone, pstfNamed, pstfClass, pstfInterface,
    pstfRecord, pstfObject, pstfEnumeration, pstfArray, pstfSet, pstfPointer,
    pstfRoutine, pstfClassReference);
  { Authored access at this declaration. Strict and unit visibility remain
    distinct; effective inherited accessibility belongs to later binding. }
  TNyxPascalSyntaxVisibility = (psavUnspecified, psavInterface, psavImplementation,
    psavLocal, psavPrivate, psavStrictPrivate, psavProtected, psavStrictProtected,
    psavPublic, psavPublished, psavAutomated);
  { Pascal parameter passing syntax, copied into each grouped parameter entry. }
  TNyxPascalSyntaxParameterMode = (pspmValue, pspmConst, pspmVar, pspmOut, pspmConstRef);
  { Standard source hint intent. Messages remain exact source expressions;
    presence does not emit compiler diagnostics or change type identity. }
  TNyxPascalHintKind = (phkDeprecated, phkPlatform, phkExperimental, phkUnimplemented);
  { One value-owned hint attached to a declaration. Spans address this snapshot's
    lexical source; -1 ends its declaration's linked hint list. No decoded or
    executed message, editor reference or mutable backing storage is retained. }
  TNyxPascalSyntaxHint = record
  private
    FDeclaration: Integer;
    FKind: TNyxPascalHintKind;
    FSpan: TNyxPascalSpan;
    FMessage: TNyxPascalSpan;
    FNext: Integer;
  public
    property DeclarationIndex: Integer read FDeclaration;
    property Kind: TNyxPascalHintKind read FKind;
    property Span: TNyxPascalSpan read FSpan;
    property MessageSpan: TNyxPascalSpan read FMessage;
    property NextHint: Integer read FNext;
  end;
  { A record's variant selector. ParentArm is -1 at the record level; a named
    selector is a real field declaration, while an unnamed selector has -1.
    Type/label expressions remain source syntax, never evaluated constants or
    inferred layout. Ordinals belong only to this immutable snapshot. }
  TNyxPascalVariantSelector = record
  private
    FScope: Integer;
    FParent: Integer;
    FTag: Integer;
    FType: TNyxPascalSpan;
    FSpan: TNyxPascalSpan;
    FFirstArm: Integer;
    FLastArm: Integer;
    FComplete: Boolean;
  public
    property RecordScope: Integer read FScope;
    property ParentArm: Integer read FParent;
    property TagDeclaration: Integer read FTag;
    property TypeSpan: TNyxPascalSpan read FType;
    property Span: TNyxPascalSpan read FSpan;
    property FirstArm: Integer read FFirstArm;
    property Complete: Boolean read FComplete;
  end;
  { One alternative, retaining its labels/body range and selector relationship.
    Fields remain ordinary members of the enclosing record and carry this arm's
    ordinal. Nested selectors point back here; no active-arm or offset is guessed. }
  TNyxPascalVariantArm = record
  private
    FSelector: Integer;
    FSpan: TNyxPascalSpan;
    FFirstLabel: Integer;
    FLastLabel: Integer;
    FNext: Integer;
    FComplete: Boolean;
  public
    property SelectorIndex: Integer read FSelector;
    property Span: TNyxPascalSpan read FSpan;
    property FirstLabel: Integer read FFirstLabel;
    property NextArm: Integer read FNext;
    property Complete: Boolean read FComplete;
  end;
  { Exact unevaluated label/range expression, linked within one variant arm.
    Its span preserves signs, qualified names and numeric spelling without
    converting them to host numbers or selecting a runtime alternative. }
  TNyxPascalVariantLabel = record
  private
    FArm: Integer;
    FSpan: TNyxPascalSpan;
    FNext: Integer;
  public
    property ArmIndex: Integer read FArm;
    property Span: TNyxPascalSpan read FSpan;
    property NextLabel: Integer read FNext;
  end;
  { A declared statement target's lexical spelling. Decimal labels are kept as
    text, without a host-number conversion or a claim about compiler limits. }
  TNyxPascalStatementLabelKind = (pslkIdentifier, pslkDecimal);
  { One label in its owning module/routine, distinct from record-variant labels.
    DeclarationIndex retains its typed lexical symbol, so labels can shadow names
    and answer DECLARED without acquiring a value type. Spans belong to this syntax;
    SectionSpan includes LABEL and the shared list terminator when present.
    Complete describes list syntax only, never GOTO legality, target existence,
    cross-routine jumps, compiler dialect admission or control-flow effects. }
  TNyxPascalStatementLabel = record
  private
    FKind: TNyxPascalStatementLabelKind;
    FDeclaration: Integer;
    FName: TNyxText;
    FScope: Integer;
    FNameSpan: TNyxPascalSpan;
    FSectionSpan: TNyxPascalSpan;
    FComplete: Boolean;
    FConditional: Boolean;
  public
    property Kind: TNyxPascalStatementLabelKind read FKind;
    property DeclarationIndex: Integer read FDeclaration;
    property Name: TNyxText read FName;
    property Scope: Integer read FScope;
    property NameSpan: TNyxPascalSpan read FNameSpan;
    property SectionSpan: TNyxPascalSpan read FSectionSpan;
    property Complete: Boolean read FComplete;
    { Raw/unfinished conditional syntax confers no active-label authority. }
    property Conditional: Boolean read FConditional;
  end;
  { Explicit uncertainty and declaration modifiers. Unsupported/recovered scopes
    grant no successful type inference, compilation or source editing authority. }
  TNyxPascalSyntaxFlag = (psdfIncomplete, psdfConditional, psdfForward,
    psdfClassMember, psdfOverload, psdfUnsupported);
  { Value-owned flag set with no mutable storage or target-specific state. }
  TNyxPascalSyntaxFlags = set of TNyxPascalSyntaxFlag;
  { Authored platform/linker intent, distinct from Pascal member visibility.
    Public symbols do not turn subsequent unit declarations into class members.
    These flags describe source syntax, not linker/ABI admission or resolution
    of an alias, library expression or compiler intrinsic. RTL procedure intent
    does not supply the compiler's special parameter-matching rules. Non-returning
    intent preserves a body; it does not prove the body's control-flow effects. }
  TNyxPascalLinkageFlag = (plfPublic, plfExport, plfExternal, plfAlias,
    plfCVariable, plfCompilerHelper, plfIntrinsic, plfPlatformAttributes,
    plfConstantIntrinsic, plfRTLProcedure, plfNoReturn);
  { Independent value set; source ranges retain operands without mutable arrays,
    interfaces or fabricated external names. }
  TNyxPascalLinkageFlags = set of TNyxPascalLinkageFlag;
  { Declared intrinsic role, without evaluating its compiler-specific numeric
    operand. Constant annotations retain ordinary authored implementation bodies. }
  TNyxPascalIntrinsicKind = (pikRuntime, pikConstant);
  { Value-owned intrinsic source entry. Declaration/NextIntrinsic are local
    ordinals; spans belong to the same lexical snapshot and map through its
    translated origins. Complete proves only source delimiters/nonempty operand,
    never an integer value, compiler eligibility or actual constant folding. }
  TNyxPascalIntrinsicDeclaration = record
  private
    FDeclaration: Integer;
    FKind: TNyxPascalIntrinsicKind;
    FSpan: TNyxPascalSpan;
    FKey: TNyxPascalSpan;
    FOperand: TNyxPascalSpan;
    FNext: Integer;
    FComplete: Boolean;
  public
    property DeclarationIndex: Integer read FDeclaration;
    property Kind: TNyxPascalIntrinsicKind read FKind;
    property Span: TNyxPascalSpan read FSpan;
    property KeySpan: TNyxPascalSpan read FKey;
    property OperandSpan: TNyxPascalSpan read FOperand;
    property NextIntrinsic: Integer read FNext;
    property Complete: Boolean read FComplete;
  end;
  { Scoped structural failure/context requirement, separate from lexical issues.
    Consumers must inspect both diagnostic collections, never infer compilation. }
  TNyxPascalSyntaxIssue = (psiExpectedName, psiExpectedSeparator, psiUnclosedScope,
    psiUnsupportedSyntax, psiIncludeContextRequired, psiConditionalContextRequired,
    psiUnterminatedConditional, psiCompilerContextRequired);
  { Independent scalar diagnostic. English guidance is owned; the exact source
    slice remains on its immutable snapshot, never an editor/session borrow. }
  TNyxPascalSyntaxDiagnostic = record
  private
    FIssue: TNyxPascalSyntaxIssue;
    FSpan: TNyxPascalSpan;
    FMessage: TNyxText;
  public
    property Issue: TNyxPascalSyntaxIssue read FIssue;
    property Span: TNyxPascalSpan read FSpan;
    property MessageText: TNyxText read FMessage;
  end;
  { One immutable scope. Parent/owner/declaration links are snapshot-local
    ordinals, never persistent references or permissions. -1 means absent.
    Complete describes closed boundaries, never grammar or compiler acceptance;
    UnsupportedSyntax forbids inferring through a skipped declaration construct. }
  TNyxPascalSyntaxScope = record
  private
    FKind: TNyxPascalSyntaxScopeKind;
    FParent: Integer;
    FOwner: Integer;
    FFirst: Integer;
    FLast: Integer;
    FCount: Integer;
    FSpan: TNyxPascalSpan;
    FName: TNyxText;
    FComplete: Boolean;
    FUnsupported: Boolean;
  public
    property Kind: TNyxPascalSyntaxScopeKind read FKind;
    property Parent: Integer read FParent;
    property OwnerDeclaration: Integer read FOwner;
    property FirstDeclaration: Integer read FFirst;
    property DeclarationCount: Integer read FCount;
    property Span: TNyxPascalSpan read FSpan;
    property Name: TNyxText read FName;
    property Complete: Boolean read FComplete;
    property UnsupportedSyntax: Boolean read FUnsupported;
  end;
  { One physical declaration with exact source ranges. Grouped names have
    independent entries and a shared type/signature range. Linked ordinals allow
    bounded scope/name queries. Records retain no interfaces or mutable arrays. }
  TNyxPascalSyntaxDeclaration = record
  private
    FName: TNyxText;
    FOwnerName: TNyxText;
    FKind: TNyxPascalSyntaxDeclarationKind;
    FOperator: TNyxPascalOperatorKind;
    FForm: TNyxPascalSyntaxTypeForm;
    FVisibility: TNyxPascalSyntaxVisibility;
    FParameterMode: TNyxPascalSyntaxParameterMode;
    FFlags: TNyxPascalSyntaxFlags;
    FNameSpan: TNyxPascalSpan;
    FHeaderSpan: TNyxPascalSpan;
    FTypeSpan: TNyxPascalSpan;
    FAncestorSpan: TNyxPascalSpan;
    FGenericSpan: TNyxPascalSpan;
    FExpressionSpan: TNyxPascalSpan;
    FScope: Integer;
    FNestedScope: Integer;
    FNext: Integer;
    FNextNamed: Integer;
    FLastNamed: Integer;
    FVariantArm: Integer;
    FFirstHint: Integer;
    FLastHint: Integer;
    FAnonymous: Boolean;
    FLinkage: TNyxPascalLinkageFlags;
    FLinkageSpan: TNyxPascalSpan;
    FFirstIntrinsic: Integer;
    FLastIntrinsic: Integer;
  public
    property Name: TNyxText read FName;
    property OwnerName: TNyxText read FOwnerName;
    property Kind: TNyxPascalSyntaxDeclarationKind read FKind;
    { Operators remain routines for existing consumers, with their own typed
      lookup family. Name/NameSpan retain the exact authored operator token. }
    property OperatorKind: TNyxPascalOperatorKind read FOperator;
    { -1 means a fixed field or non-field declaration. Variant fields still
      inhabit the record's ordinary member scope, rather than hidden subscopes. }
    property VariantArm: Integer read FVariantArm;
    { -1 means no standard hint. Follow NextHint only in this snapshot's optional
      INyxPascalHintSyntax; each grouped declaration owns its own list. }
    property FirstHint: Integer read FFirstHint;
    { An unnamed structured type has ordinary owned type identity/member scope,
      an empty Name and its actual structured-type keyword as NameSpan. It never
      enters Named lookup; declarations sharing one expression share this type. }
    property Anonymous: Boolean read FAnonymous;
    property TypeForm: TNyxPascalSyntaxTypeForm read FForm;
    property Visibility: TNyxPascalSyntaxVisibility read FVisibility;
    property ParameterMode: TNyxPascalSyntaxParameterMode read FParameterMode;
    property Flags: TNyxPascalSyntaxFlags read FFlags;
    { LinkageSpan encloses the authored suffixes, including their operands and
      intervening routine modifiers; an empty span means no linkage suffix.
      Like other declaration spans, it belongs to this syntax snapshot. A
      translated snapshot's origin table supplies its actual physical ranges. }
    property Linkage: TNyxPascalLinkageFlags read FLinkage;
    property LinkageSpan: TNyxPascalSpan read FLinkageSpan;
    { -1 means no declared intrinsic. Follow checked local links in the optional
      INyxPascalIntrinsicSyntax; an entry's operand is never an evaluated value. }
    property FirstIntrinsic: Integer read FFirstIntrinsic;
    property NameSpan: TNyxPascalSpan read FNameSpan;
    property HeaderSpan: TNyxPascalSpan read FHeaderSpan;
    property TypeSpan: TNyxPascalSpan read FTypeSpan;
    property AncestorSpan: TNyxPascalSpan read FAncestorSpan;
    property GenericSpan: TNyxPascalSpan read FGenericSpan;
    property ExpressionSpan: TNyxPascalSpan read FExpressionSpan;
    property Scope: Integer read FScope;
    property NestedScope: Integer read FNestedScope;
    property Next: Integer read FNext;
    property NextNamed: Integer read FNextNamed;
  end;
  { A declared uses/requires/contains namespace. Optional file expressions remain
    authored text; this value confers no filesystem/import/loading capability. }
  TNyxPascalSyntaxImport = record
  private
    FName: TNyxText;
    FSpan: TNyxPascalSpan;
    FScope: Integer;
    FConditional: Boolean;
  public
    property Name: TNyxText read FName;
    property Span: TNyxPascalSpan read FSpan;
    property Scope: Integer read FScope;
    property Conditional: Boolean read FConditional;
  end;
  { Independent fluent limits and explicit include/fragment starting context.
    A lexer policy is copied, never inferred from the host compiler. }
  TNyxPascalSyntaxOptions = record
  private
    FDefined: Boolean;
    FFragment: TNyxPascalSyntaxFragment;
    FLexical: TNyxPascalLexicalOptions;
    FDeclarations: Integer;
    FScopes: Integer;
    FDepth: Integer;
    FDiagnostics: Integer;
  public
    { Explicit module/lexical defaults and positive independent quotas; zeroed
      options refuse. Setters return validated copies, never shared mutable state. }
    class function Defaults: TNyxPascalSyntaxOptions; static;
    { Select the actual enclosing fragment grammar; filenames grant no context. }
    function Fragment(AValue: TNyxPascalSyntaxFragment): TNyxPascalSyntaxOptions;
    { Copy the complete lexical policy, including Unicode/comment handling. }
    function Lexical(const AValue: TNyxPascalLexicalOptions): TNyxPascalSyntaxOptions;
    { Bounds declarations plus their variant-selector/arm/label, statement-label and hint entries
      across all scopes. Metadata cannot grow outside this aggregate quota. }
    function UpToDeclarations(AValue: Integer): TNyxPascalSyntaxOptions;
    { Includes the physical module/fragment scope and routine/member/body scopes. }
    function UpToScopes(AValue: Integer): TNyxPascalSyntaxOptions;
    { Bounds recursive grouping and declaration/body/conditional nesting. }
    function UpToDepth(AValue: Integer): TNyxPascalSyntaxOptions;
    { Excess recovery/context diagnostics refuse the whole candidate. }
    function UpToDiagnostics(AValue: Integer): TNyxPascalSyntaxOptions;
    { Raises ENyxPascalLexical for undefined, invalid-enum or nonpositive policies. }
    procedure Validate;
    property StartingContext: TNyxPascalSyntaxFragment read FFragment;
    property LexicalOptions: TNyxPascalLexicalOptions read FLexical;
    property MaximumDeclarations: Integer read FDeclarations;
    property MaximumScopes: Integer read FScopes;
    property MaximumDepth: Integer read FDepth;
    property MaximumDiagnostics: Integer read FDiagnostics;
  end;
  { Immutable recoverable declared structure over one lexical snapshot. Ordinary
    source positions are physical; translated adapters retain an explicit origin
    map for their analysis projection, which must never replace authored text.
    Types/ancestors/generic arguments remain exact syntax, not bound compiler
    types. Includes/conditions/unsupported constructs remain explicit diagnostics.
    There is no source execution, compiler provenance, editor or document borrow. }
  INyxPascalSyntaxSnapshot = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570B02}']
    function Lexical: INyxPascalLexicalSnapshot;
    { Ordinals are zero-based and checked. Returned records are detached values;
      nested/linked ordinals are meaningful only in this same immutable snapshot. }
    function ScopeCount: Integer;
    function Scope(AIndex: Integer): TNyxPascalSyntaxScope;
    { Deepest physical containment; an unfinished scope also contains EOF.
      This does not resolve conditional branches or imply visibility/binding. }
    function ScopeAt(AOffset: Integer): Integer;
    function DeclarationCount: Integer;
    function Declaration(AIndex: Integer): TNyxPascalSyntaxDeclaration;
    { First same-spelled entry in this exact scope, or -1. NextNamed retains
      overload/forward duplicates; this is declared lookup, not name binding. }
    function Named(AScope: Integer; const AName: TNyxText): Integer;
    function ImportCount: Integer;
    function ImportAt(AIndex: Integer): TNyxPascalSyntaxImport;
    function DiagnosticCount: Integer;
    function Diagnostic(AIndex: Integer): TNyxPascalSyntaxDiagnostic;
  end;
  { Construction-only work receipt retained as immutable values. Reused
    declarations still require copied/reindexed metadata; FilteredTokens counts
    the complete lexical filter and ParserTokenReads includes actual lookahead.
    These counters grant neither reduced aggregate charging nor binding reuse.
    An unchanged returned snapshot keeps its original construction receipt. }
  INyxPascalSyntaxWork = interface(IInterface)
    ['{9E5014FB-52E2-47E4-B3B2-B33CB5623459}']
    function ReusedDeclarations: Integer;
    function CopiedEntries: Integer;
    function FilteredTokens: Integer;
    function ParserTokenReads: Integer;
    function RestartToken: Integer;
  end;
  { Optional declaration-label metadata, with checked snapshot-local ordinals.
    Metadata and typed declarations consume the shared node quota. Lexical lookup
    retains labels for shadowing/DECLARED; value-type/member inference refuses.
    Retaining this capability retains only its immutable syntax. }
  INyxPascalStatementLabelSyntax = interface(IInterface)
    ['{3D6A9D7B-7F12-4E16-A8F7-6C3F65C715B4}']
    function StatementLabelCount: Integer;
    function StatementLabelAt(AIndex: Integer): TNyxPascalStatementLabel;
  end;

  { Optional typed operator index over the same immutable declaration arena.
    Ordinary Named never returns operators, even when an operator's named form
    matches a normal method. NextNamed follows this operator family's overloads. }
  INyxPascalOperatorSyntax = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570B03}']
    function NamedOperator(AScope: Integer; AKind: TNyxPascalOperatorKind): Integer;
  end;
  { Optional structural metadata over the same immutable declaration/source arena.
    All ordinals/getters are checked. Counts share declaration budgets; no layout,
    evaluated labels or runtime arm selection is supplied by these value records. }
  INyxPascalVariantSyntax = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570B04}']
    function SelectorCount: Integer;
    function SelectorAt(AIndex: Integer): TNyxPascalVariantSelector;
    function ArmCount: Integer;
    function ArmAt(AIndex: Integer): TNyxPascalVariantArm;
    function LabelCount: Integer;
    function LabelAt(AIndex: Integer): TNyxPascalVariantLabel;
  end;
  { Optional standard hint metadata. Each declaration's FirstHint/NextHint links
    stay snapshot-local; messages preserve original source and are never executed. }
  INyxPascalHintSyntax = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570B05}']
    function HintCount: Integer;
    function HintAt(AIndex: Integer): TNyxPascalSyntaxHint;
  end;
  { Optional immutable intrinsic entries over the same owned declaration arena.
    Queries own no editor/compiler and share the declaration metadata budget. }
  INyxPascalIntrinsicSyntax = interface(IInterface)
    ['{C2FB8E45-8349-4E77-A33B-A0BCA035B871}']
    { Count of explicit runtime/constant annotations, including incomplete entries. }
    function IntrinsicCount: Integer;
    { Return one checked value entry; invalid ordinals refuse. }
    function IntrinsicAt(AIndex: Integer): TNyxPascalIntrinsicDeclaration;
  end;
  { Exact structured-type source index over this immutable declaration arena.
    Both named and anonymous bodies keep ordinary type declarations/member scopes;
    TypeAt accepts a lexical scalar start (including authored packing), returns -1
    away from a body, and never creates a synthetic exported name or ABI layout. }
  INyxPascalStructuredSyntax = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570B06}']
    function TypeAt(AStart: Integer): Integer;
  end;

{ Pure analysis on copied text. Unknown/missing syntax retains preceding
  declarations and physical spans with explicit recovery issues. Quotas and
  cancellation refuse the complete candidate; no parser result admits source. }
function NewNyxPascalSyntaxSnapshot(const ASource: TNyxText): INyxPascalSyntaxSnapshot; overload;
function NewNyxPascalSyntaxSnapshot(const ASource: TNyxText;
  const AOptions: TNyxPascalSyntaxOptions;
  const ACancellation: INyxCancellationScope = nil): INyxPascalSyntaxSnapshot; overload;

{ Consume an immutable lexical snapshot under exactly matching lexical options.
  The parser retains that value owner, never reassembles/relexes its Source text.
  Alternate implementations must honor the lexical snapshot's immutable contract;
  source admission, compiler execution and incremental parsing are not implied. }
function NewNyxPascalSyntaxFromLexical(const ALexical: INyxPascalLexicalSnapshot;
  const AOptions: TNyxPascalSyntaxOptions;
  const ACancellation: INyxCancellationScope = nil): INyxPascalSyntaxSnapshot;

{ Continue an owned parser along exact sealed lexical append lineage and
  identical policies. Completed declarations may be restored before a safe
  checkpoint; provisional tails and diagnostics are parsed afresh. The complete
  token filter and metadata indexes still rebuild. Alternate/foreign histories
  or changed options refuse; absent safe checkpoints fall back to a whole parse.
  Cancellation/failure publishes no candidate and never mutates APrevious. }
function AppendNyxPascalSyntax(const APrevious: INyxPascalSyntaxSnapshot;
  const ALexical: INyxPascalLexicalSnapshot; const AOptions: TNyxPascalSyntaxOptions;
  const ACancellation: INyxCancellationScope = nil): INyxPascalSyntaxSnapshot;

implementation

uses nyx.text.index, nyx.pascal.lexing.append;

type
  { Source-independent ordinal vector. Temporary lists stage grouped declarations
    without putting interfaces or shared mutable arrays in published records. }
  TOrdinals = array of Integer;
  { Closed continuation positions in the root declaration grammar. A type
    section nested in root declarations must return to that caller's grammar;
    an explicit types fragment does not gain a declaration-section caller. }
  TParserResume = (prDeclarations, prTypes, prTypesFragment);
  { Value-only rollback point before a root declaration. Counts exclude the
    provisional suffix; Root retains pre-suffix flags and source identity.
    ReadThrough includes every parser lookahead dependency, independently of
    Position. No interface, mutable vector or prior snapshot is retained. }
  TParserCheckpoint = record
    Valid: Boolean;
    Resume: TParserResume;
    Position: Integer;
    ReadThrough: Integer;
    Depth: Integer;
    PartialEqual: Boolean;
    ActiveArm: Integer;
    Visibility: TNyxPascalSyntaxVisibility;
    Members: Boolean;
    Section: TNyxPascalWord;
    Root: TNyxPascalSyntaxScope;
    Scopes: Integer;
    Declarations: Integer;
    Imports: Integer;
    ParserDiagnostics: Integer;
    Selectors: Integer;
    Arms: Integer;
    Labels: Integer;
    StatementLabels: Integer;
    Hints: Integer;
    Intrinsics: Integer;
  end;
  { Forward owner for the private immutable construction seed. }
  TSyntaxSnapshot = class;
  { In-unit immutable parser authority. Snapshot is borrowed only while this
    interface lease is held; an appended parser retains no predecessor owner. }
  ISyntaxOwner = interface(IInterface)
    ['{04123994-82CD-41B5-8A95-BBEB2140880B}']
    function Snapshot: TSyntaxSnapshot;
  end;
  { Owned parser builder becomes the immutable snapshot after construction.
    Significant-token ordinals keep lossless trivia on Lexical. No raw source
    provider or cancellation borrow survives publication. }
  TSyntaxSnapshot = class(TInterfacedObject, INyxPascalSyntaxSnapshot,
    INyxPascalOperatorSyntax, INyxPascalVariantSyntax, INyxPascalHintSyntax,
    INyxPascalStructuredSyntax, INyxPascalIntrinsicSyntax, INyxPascalStatementLabelSyntax,
    INyxPascalSyntaxWork, ISyntaxOwner)
  private
    FLexical: INyxPascalLexicalSnapshot;
    FOptions: TNyxPascalSyntaxOptions;
    FCancellation: INyxCancellationScope;
    FTokens: TOrdinals;
    FConditional: array of Boolean;
    FScopes: array of TNyxPascalSyntaxScope;
    FDeclarations: array of TNyxPascalSyntaxDeclaration;
    FImports: array of TNyxPascalSyntaxImport;
    FDiagnostics: array of TNyxPascalSyntaxDiagnostic;
    FSelectors: array of TNyxPascalVariantSelector;
    FArms: array of TNyxPascalVariantArm;
    FLabels: array of TNyxPascalVariantLabel;
    FStatementLabels: array of TNyxPascalStatementLabel;
    FHints: array of TNyxPascalSyntaxHint;
    FIntrinsics: array of TNyxPascalIntrinsicDeclaration;
    FScopeCount: Integer;
    FDeclarationCount: Integer;
    FImportCount: Integer;
    FDiagnosticCount: Integer;
    FSelectorCount: Integer;
    FArmCount: Integer;
    FLabelCount: Integer;
    FStatementLabelCount: Integer;
    FHintCount: Integer;
    FIntrinsicCount: Integer;
    FActiveArm: Integer;
    FNames: TNyxTextIndex;
    { Owned source-position index, separate from exported/lexical names. }
    FStructures: TNyxTextIndex;
    FPosition: Integer;
    { A generic header may end in the physical >= token. Only its > belongs
      to the header; the residual equals sign stays for the alias declaration. }
    FPartialEqual: Boolean;
    FDepth: Integer;
    FCheckpoint: TParserCheckpoint;
    FFilterDiagnostics: Integer;
    FReadThrough: Integer;
    FParserReads: Integer;
    FReused: Integer;
    FCopied: Integer;
    FRestart: Integer;
    procedure CheckCancellation;
    function ReadToken(AIndex: Integer): TNyxPascalToken;
    procedure Remember(AResume: TParserResume; AVisibility: TNyxPascalSyntaxVisibility;
      AMembers: Boolean; ASection: TNyxPascalWord);
    procedure Restore(APrevious: TSyntaxSnapshot);
    procedure LinkDeclaration(AIndex: Integer);
    function Tok(AOffset: Integer = 0): TNyxPascalToken;
    function Word(AWord: TNyxPascalWord; AOffset: Integer = 0): Boolean;
    function Sym(ASymbol: TNyxPascalSymbol; AOffset: Integer = 0): Boolean;
    function EOF: Boolean;
    function Name(AOffset: Integer = 0): TNyxText;
    procedure Step;
    function EndOffset: Integer;
    function SpanFrom(AStart: Integer): TNyxPascalSpan;
    function Range(AStart, AEnd: Integer): TNyxPascalSpan;
    procedure Problem(AIssue: TNyxPascalSyntaxIssue; const AText: TNyxText;
      const ASpan: TNyxPascalSpan);
    procedure Unsupported(AScope: Integer; const AText: TNyxText;
      const ASpan: TNyxPascalSpan);
    procedure Enter;
    procedure Leave;
    { Metadata and declarations consume one common node quota. }
    procedure NodeBudget;
    function AddScope(AKind: TNyxPascalSyntaxScopeKind; AParent, AOwner: Integer;
      AStart: Integer; const AName: TNyxText): Integer;
    procedure FinishScope(AScope: Integer; AComplete: Boolean);
    function AddDeclaration(AScope: Integer; AKind: TNyxPascalSyntaxDeclarationKind;
      AVisibility: TNyxPascalSyntaxVisibility; AName: Integer;
      AOperator: TNyxPascalOperatorKind = pokNone; AAnonymous: Boolean = False): Integer;
    procedure Separator;
    procedure Groups(AOpen, AClose: TNyxPascalSymbol);
    procedure Angles;
    procedure Expression(AStop: TNyxPascalSymbol; AAlsoComma: Boolean = False;
      AHints: Boolean = False; AArmEnd: Boolean = False; AInitializerBoundary: Boolean = False;
      ARoutineBoundary: Boolean = False);
    { Closed routine clause lookahead; escaped/contextual type names keep their
      source role, and member PUBLIC remains a visibility section. }
    function RoutineClause(AScope: Integer = -1): Boolean;
    function HintWord(out AKind: TNyxPascalHintKind): Boolean;
    function AtHint(AExpressionStart: Integer): Boolean;
    procedure DeclarationHints(ADeclaration: Integer);
    procedure CloneDeclarationHints(AFrom, ATo: Integer);
    { Append one value entry after its source envelope has been scanned. Missing
      colon/value retains an incomplete declaration and explicit diagnostic. }
    procedure IntrinsicDeclaration(ADeclaration: Integer; AKind: TNyxPascalIntrinsicKind;
      const AKey, AOperand: TNyxPascalSpan; AEnvelope: Boolean);
    { Consume only declaration suffixes. Class-member visibility remains owned
      by Declarations; unknown platform attributes retain explicit uncertainty. }
    function DeclarationLinkage(ADeclaration: Integer; ARoutine: Boolean): Boolean;
    procedure VariantPart(AScope: Integer; AVisibility: TNyxPascalSyntaxVisibility);
    procedure VariantFields(AScope: Integer; AVisibility: TNyxPascalSyntaxVisibility);
    { Reuse declared-body grammar and ownership for named and inline types. }
    procedure StructuredBody(AScope, AOwner: Integer);
    procedure TypeExpression(AScope: Integer; AVisibility: TNyxPascalSyntaxVisibility;
      AStop: TNyxPascalSymbol; AArmEnd: Boolean = False);
    function Form: TNyxPascalSyntaxTypeForm;
    { Extend a procedural expression across only its supported trailing directive
      separators. Never consume an escaped/contextual name beginning a new type. }
    procedure RoutineTypeSuffix(AArmEnd: Boolean = False; AInitializerBoundary: Boolean = False);
    { Retain declaration labels with their typed lexical symbols. Malformed lists
      mark their owner unsupported; recovery leaves the next body/section intact. }
    procedure StatementLabels(AScope: Integer);
    procedure ParseType(AScope: Integer; AVisibility: TNyxPascalSyntaxVisibility);
    procedure TypeSection(AScope: Integer; AVisibility: TNyxPascalSyntaxVisibility);
    procedure Variables(AScope: Integer; AKind: TNyxPascalSyntaxDeclarationKind;
      AVisibility: TNyxPascalSyntaxVisibility; AClass: Boolean = False;
      AArmEnd: Boolean = False);
    procedure Constants(AScope: Integer; AKind: TNyxPascalSyntaxDeclarationKind;
      AVisibility: TNyxPascalSyntaxVisibility);
    procedure Parameters(AScope: Integer);
    procedure Routine(AScope: Integer; AVisibility: TNyxPascalSyntaxVisibility;
      AClass: Boolean = False);
    { A class property retains its ordinary property kind/type plus explicit
      class-member intent; accessors remain authored syntax, not executed code. }
    procedure PropertyDeclaration(AScope: Integer; AVisibility: TNyxPascalSyntaxVisibility;
      AClass: Boolean = False);
    procedure Imports(AScope: Integer);
    procedure Declarations(AScope: Integer; AVisibility: TNyxPascalSyntaxVisibility;
      AMembers: Boolean; ASection: TNyxPascalWord = pkwNone);
    function Block(AScope: Integer): Boolean;
    procedure BuildTokens;
    procedure Build(APrevious: TSyntaxSnapshot = nil; ARetained: Integer = -1);
    procedure ParseRoot;
    procedure FinishRoot;
  public
    constructor Create(const ASource: TNyxText; const AOptions: TNyxPascalSyntaxOptions;
      const ACancellation: INyxCancellationScope);
    constructor CreateLexical(const ALexical: INyxPascalLexicalSnapshot;
      const AOptions: TNyxPascalSyntaxOptions; const ACancellation: INyxCancellationScope;
      APrevious: TSyntaxSnapshot = nil; ARetained: Integer = -1);
    function Snapshot: TSyntaxSnapshot;
    function ReusedDeclarations: Integer;
    function CopiedEntries: Integer;
    function FilteredTokens: Integer;
    function ParserTokenReads: Integer;
    function RestartToken: Integer;
    destructor Destroy; override;
    function Lexical: INyxPascalLexicalSnapshot;
    function ScopeCount: Integer;
    function Scope(AIndex: Integer): TNyxPascalSyntaxScope;
    function ScopeAt(AOffset: Integer): Integer;
    function DeclarationCount: Integer;
    function Declaration(AIndex: Integer): TNyxPascalSyntaxDeclaration;
    function Named(AScope: Integer; const AName: TNyxText): Integer;
    function NamedOperator(AScope: Integer; AKind: TNyxPascalOperatorKind): Integer;
    function SelectorCount: Integer;
    function SelectorAt(AIndex: Integer): TNyxPascalVariantSelector;
    function ArmCount: Integer;
    function ArmAt(AIndex: Integer): TNyxPascalVariantArm;
    function LabelCount: Integer;
    function LabelAt(AIndex: Integer): TNyxPascalVariantLabel;
    function StatementLabelCount: Integer;
    function StatementLabelAt(AIndex: Integer): TNyxPascalStatementLabel;
    function HintCount: Integer;
    function HintAt(AIndex: Integer): TNyxPascalSyntaxHint;
    function IntrinsicCount: Integer;
    function IntrinsicAt(AIndex: Integer): TNyxPascalIntrinsicDeclaration;
    function TypeAt(AStart: Integer): Integer;
    function ImportCount: Integer;
    function ImportAt(AIndex: Integer): TNyxPascalSyntaxImport;
    function DiagnosticCount: Integer;
    function Diagnostic(AIndex: Integer): TNyxPascalSyntaxDiagnostic;
  end;

class function TNyxPascalSyntaxOptions.Defaults: TNyxPascalSyntaxOptions;
begin
  Result := Default(TNyxPascalSyntaxOptions);
  Result.FDefined := True;
  Result.FLexical := TNyxPascalLexicalOptions.Defaults;
  Result.FDeclarations := 16384;
  Result.FScopes := 8192;
  Result.FDepth := 128;
  Result.FDiagnostics := 1024;
end;

procedure TNyxPascalSyntaxOptions.Validate;
begin
  FLexical.Validate;

  if not FDefined or (FDeclarations < 1) or (FScopes < 1) or
    (FDepth < 1) or (FDiagnostics < 1) or
    (Ord(FFragment) < Ord(Low(TNyxPascalSyntaxFragment))) or
    (Ord(FFragment) > Ord(High(TNyxPascalSyntaxFragment))) then
  begin
    raise ENyxPascalLexical.Create('Use explicit positive Pascal syntax options');
  end;
end;

function TNyxPascalSyntaxOptions.Fragment(AValue: TNyxPascalSyntaxFragment): TNyxPascalSyntaxOptions;
begin
  Result := Self;
  Result.FFragment := AValue;
  Result.Validate;
end;

function TNyxPascalSyntaxOptions.Lexical(const AValue: TNyxPascalLexicalOptions): TNyxPascalSyntaxOptions;
begin
  Result := Self;
  Result.FLexical := AValue;
  Result.Validate;
end;

function TNyxPascalSyntaxOptions.UpToDeclarations(AValue: Integer): TNyxPascalSyntaxOptions;
begin
  Result := Self;
  Result.FDeclarations := AValue;
  Result.Validate;
end;

function TNyxPascalSyntaxOptions.UpToScopes(AValue: Integer): TNyxPascalSyntaxOptions;
begin
  Result := Self;
  Result.FScopes := AValue;
  Result.Validate;
end;

function TNyxPascalSyntaxOptions.UpToDepth(AValue: Integer): TNyxPascalSyntaxOptions;
begin
  Result := Self;
  Result.FDepth := AValue;
  Result.Validate;
end;

function TNyxPascalSyntaxOptions.UpToDiagnostics(AValue: Integer): TNyxPascalSyntaxOptions;
begin
  Result := Self;
  Result.FDiagnostics := AValue;
  Result.Validate;
end;

constructor TSyntaxSnapshot.Create(const ASource: TNyxText;
  const AOptions: TNyxPascalSyntaxOptions; const ACancellation: INyxCancellationScope);
begin
  inherited Create;
  AOptions.Validate;
  FOptions := AOptions;
  FCancellation := ACancellation;
  FNames := TNyxTextIndex.Create;
  FStructures := TNyxTextIndex.Create;
  FActiveArm := -1;
  try
    FLexical := NewNyxPascalLexicalSnapshot(ASource, AOptions.LexicalOptions, ACancellation);
    Build;
  finally
    FCancellation := nil;
  end;
end;

constructor TSyntaxSnapshot.CreateLexical(const ALexical: INyxPascalLexicalSnapshot;
  const AOptions: TNyxPascalSyntaxOptions; const ACancellation: INyxCancellationScope;
  APrevious: TSyntaxSnapshot; ARetained: Integer);
var
  LPolicy: TNyxPascalLexicalOptions;
  LExpected: TNyxPascalLexicalOptions;
begin
  inherited Create;
  AOptions.Validate;

  if ALexical = nil then
  begin
    raise ENyxPascalLexical.Create('Declared syntax needs its immutable lexical snapshot');
  end;
  LPolicy := ALexical.Options;
  LPolicy.Validate;
  LExpected := AOptions.LexicalOptions;

  if (LPolicy.MaximumSourceBytes <> LExpected.MaximumSourceBytes) or
    (LPolicy.MaximumTokens <> LExpected.MaximumTokens) or
    (LPolicy.MaximumDiagnostics <> LExpected.MaximumDiagnostics) or
    (LPolicy.MaximumCommentDepth <> LExpected.MaximumCommentDepth) or
    (LPolicy.AllowNestedComments <> LExpected.AllowNestedComments) or
    (LPolicy.AllowUnicodeIdentifiers <> LExpected.AllowUnicodeIdentifiers) or
    (ALexical.Count < 1) or (ALexical.Count > LExpected.MaximumTokens) or
    (ALexical.DiagnosticCount < 0) or (ALexical.DiagnosticCount > LExpected.MaximumDiagnostics) or
    (ALexical.Scalars < 0) or (ALexical.Scalars > LExpected.MaximumSourceBytes) or
    (ALexical.Token(ALexical.Count - 1).Kind <> ptkEOF) then
  begin
    raise ENyxPascalLexical.Create('Declared syntax lexical input does not match its fixed policy and bounds');
  end;
  FOptions := AOptions;
  FCancellation := ACancellation;
  FNames := TNyxTextIndex.Create;
  FStructures := TNyxTextIndex.Create;
  FActiveArm := -1;
  try
    FLexical := ALexical;
    Build(APrevious, ARetained);
  finally
    FCancellation := nil;
  end;
end;

destructor TSyntaxSnapshot.Destroy;
begin
  FStructures.Free;
  FNames.Free;
  inherited Destroy;
end;

procedure TSyntaxSnapshot.CheckCancellation;
begin

  if (FCancellation <> nil) and FCancellation.Cancelled then
  begin
    raise ENyxPascalAnalysisCancelled.Create('Declaration checkpoint construction cancelled');
  end;
end;

function TSyntaxSnapshot.ReadToken(AIndex: Integer): TNyxPascalToken;
begin

  if FParserReads = High(Integer) then
  begin
    raise ENyxPascalSyntaxLimit.Create('Parser token-read telemetry exceeds its exact counter domain');
  end;
  Inc(FParserReads);

  if AIndex > FReadThrough then
  begin
    FReadThrough := AIndex;
  end;
  Result := FLexical.Token(AIndex);
end;

procedure TSyntaxSnapshot.Remember(AResume: TParserResume;
  AVisibility: TNyxPascalSyntaxVisibility; AMembers: Boolean; ASection: TNyxPascalWord);
var
  LSafe: Integer;
begin
  { Keep only the latest point safe before append's two-token repair window.
    Actual continuation independently rechecks its retained-token authority.
    Lookahead reaching provisional EOF invalidates later rollback points. }
  LSafe := FLexical.Count - 3;

  if (FReadThrough >= LSafe) or (FTokens[FPosition] >= LSafe) then
  begin
    Exit;
  end;
  FCheckpoint.Valid := True;
  FCheckpoint.Resume := AResume;
  FCheckpoint.Position := FPosition;
  FCheckpoint.ReadThrough := FReadThrough;
  FCheckpoint.Depth := FDepth;
  FCheckpoint.PartialEqual := FPartialEqual;
  FCheckpoint.ActiveArm := FActiveArm;
  FCheckpoint.Visibility := AVisibility;
  FCheckpoint.Members := AMembers;
  FCheckpoint.Section := ASection;
  FCheckpoint.Root := FScopes[0];
  FCheckpoint.Scopes := FScopeCount;
  FCheckpoint.Declarations := FDeclarationCount;
  FCheckpoint.Imports := FImportCount;
  FCheckpoint.ParserDiagnostics := FDiagnosticCount - FFilterDiagnostics;
  FCheckpoint.Selectors := FSelectorCount;
  FCheckpoint.Arms := FArmCount;
  FCheckpoint.Labels := FLabelCount;
  FCheckpoint.StatementLabels := FStatementLabelCount;
  FCheckpoint.Hints := FHintCount;
  FCheckpoint.Intrinsics := FIntrinsicCount;
end;

procedure TSyntaxSnapshot.Restore(APrevious: TSyntaxSnapshot);
var
  LAt: Integer;
  LFilterCount: Integer;
begin
  FCheckpoint := APrevious.FCheckpoint;
  FScopeCount := FCheckpoint.Scopes;
  FDeclarationCount := FCheckpoint.Declarations;
  FImportCount := FCheckpoint.Imports;
  FSelectorCount := FCheckpoint.Selectors;
  FArmCount := FCheckpoint.Arms;
  FLabelCount := FCheckpoint.Labels;
  FStatementLabelCount := FCheckpoint.StatementLabels;
  FHintCount := FCheckpoint.Hints;
  FIntrinsicCount := FCheckpoint.Intrinsics;
  FPosition := FCheckpoint.Position;
  FPartialEqual := FCheckpoint.PartialEqual;
  FActiveArm := FCheckpoint.ActiveArm;
  FReadThrough := FCheckpoint.ReadThrough;
  FReused := FDeclarationCount;
  FRestart := FTokens[FPosition];
  FCopied := FScopeCount + FDeclarationCount + FImportCount + FSelectorCount +
    FArmCount + FLabelCount + FStatementLabelCount + FHintCount + FIntrinsicCount +
    FCheckpoint.ParserDiagnostics;
  SetLength(FScopes, FScopeCount);
  for LAt := 0 to FScopeCount - 1 do
  begin
    CheckCancellation;
    FScopes[LAt] := APrevious.FScopes[LAt];

    if LAt = 0 then
    begin
      FScopes[LAt] := FCheckpoint.Root;
    end;
    { Unclosed recovered scopes originally reach their lexical source end,
      even when recovery allowed later declarations to be inspected. }

    if not FScopes[LAt].Complete then
    begin
      FScopes[LAt].FSpan := NyxPascalSpan(FScopes[LAt].Span.Start,
        FLexical.Scalars - FScopes[LAt].Span.Start);
    end;
    FScopes[LAt].FFirst := -1;
    FScopes[LAt].FLast := -1;
    FScopes[LAt].FCount := 0;
  end;
  SetLength(FDeclarations, FDeclarationCount);
  for LAt := 0 to FDeclarationCount - 1 do
  begin
    CheckCancellation;
    FDeclarations[LAt] := APrevious.FDeclarations[LAt];
    LinkDeclaration(LAt);

    if (FDeclarations[LAt].Kind = psdkType) and
      (FDeclarations[LAt].TypeForm in [pstfClass, pstfInterface, pstfRecord, pstfObject]) then
    begin
      FStructures.AddFirst(TNyxText(IntToStr(FDeclarations[LAt].TypeSpan.Start)), LAt);
    end;
  end;
  SetLength(FImports, FImportCount);
  for LAt := 0 to FImportCount - 1 do
  begin
    CheckCancellation;
    FImports[LAt] := APrevious.FImports[LAt];
  end;
  SetLength(FSelectors, FSelectorCount);
  for LAt := 0 to FSelectorCount - 1 do
  begin
    CheckCancellation;
    FSelectors[LAt] := APrevious.FSelectors[LAt];
  end;
  SetLength(FArms, FArmCount);
  for LAt := 0 to FArmCount - 1 do
  begin
    CheckCancellation;
    FArms[LAt] := APrevious.FArms[LAt];
  end;
  SetLength(FLabels, FLabelCount);
  for LAt := 0 to FLabelCount - 1 do
  begin
    CheckCancellation;
    FLabels[LAt] := APrevious.FLabels[LAt];
  end;
  SetLength(FStatementLabels, FStatementLabelCount);
  for LAt := 0 to FStatementLabelCount - 1 do
  begin
    CheckCancellation;
    FStatementLabels[LAt] := APrevious.FStatementLabels[LAt];
  end;
  SetLength(FHints, FHintCount);
  for LAt := 0 to FHintCount - 1 do
  begin
    CheckCancellation;
    FHints[LAt] := APrevious.FHints[LAt];
  end;
  SetLength(FIntrinsics, FIntrinsicCount);
  for LAt := 0 to FIntrinsicCount - 1 do
  begin
    CheckCancellation;
    FIntrinsics[LAt] := APrevious.FIntrinsics[LAt];
  end;
  { Filter diagnostics belong to the new complete filter pass. Only parser
    diagnostics before the rollback point survive from the previous snapshot. }
  LFilterCount := FDiagnosticCount;

  if FCheckpoint.ParserDiagnostics > FOptions.MaximumDiagnostics - LFilterCount then
  begin
    raise ENyxPascalSyntaxLimit.Create('Restored diagnostics exceed the shared syntax budget');
  end;
  SetLength(FDiagnostics, LFilterCount + FCheckpoint.ParserDiagnostics);
  for LAt := 0 to FCheckpoint.ParserDiagnostics - 1 do
  begin
    CheckCancellation;
    FDiagnostics[LFilterCount + LAt] :=
      APrevious.FDiagnostics[APrevious.FFilterDiagnostics + LAt];
  end;
  FDiagnosticCount := Length(FDiagnostics);
end;

function TSyntaxSnapshot.Tok(AOffset: Integer): TNyxPascalToken;
var
  LAt: Integer;
begin
  LAt := FPosition + AOffset;

  if LAt < 0 then
  begin
    LAt := 0;
  end;

  if LAt >= Length(FTokens) then
  begin
    LAt := High(FTokens);
  end;
  Result := ReadToken(FTokens[LAt]);
end;

function TSyntaxSnapshot.Word(AWord: TNyxPascalWord; AOffset: Integer): Boolean;
begin
  Result := (Tok(AOffset).Kind = ptkWord) and (Tok(AOffset).Word = AWord);
end;

function TSyntaxSnapshot.Sym(ASymbol: TNyxPascalSymbol; AOffset: Integer): Boolean;
begin
  Result := (Tok(AOffset).Kind = ptkSymbol) and (Tok(AOffset).Symbol = ASymbol);

  if (AOffset = 0) and FPartialEqual then
  begin
    Result := ASymbol = psoEqual;
  end;
end;

function TSyntaxSnapshot.EOF: Boolean;
begin
  Result := Tok.Kind = ptkEOF;
end;

function TSyntaxSnapshot.Name(AOffset: Integer): TNyxText;
begin
  Result := '';

  if Tok(AOffset).Kind = ptkWord then
  begin
    Result := FLexical.Identifier(FTokens[FPosition + AOffset]);
  end;
end;

procedure TSyntaxSnapshot.Step;
begin

  if (FCancellation <> nil) and FCancellation.Cancelled then
  begin
    raise ENyxPascalAnalysisCancelled.Create('Pascal syntax analysis cancelled');
  end;

  if FPosition < High(FTokens) then
  begin
    FPartialEqual := False;
    Inc(FPosition);
  end;
end;

function TSyntaxSnapshot.EndOffset: Integer;
begin
  Result := 0;

  if FPartialEqual then
  begin
    Result := Tok.Span.Start + 1;
  end
  else if FPosition > 0 then
  begin
    Result := ReadToken(FTokens[FPosition - 1]).Span.Finish;
  end;
end;

function TSyntaxSnapshot.SpanFrom(AStart: Integer): TNyxPascalSpan;
begin
  Result := NyxPascalSpan(AStart, EndOffset - AStart);
end;

function TSyntaxSnapshot.Range(AStart, AEnd: Integer): TNyxPascalSpan;
begin
  Result := NyxPascalSpan(ReadToken(FTokens[AStart]).Span.Start, 0);

  if (AEnd = FPosition) and FPartialEqual then
  begin
    Result := NyxPascalSpan(Result.Start, EndOffset - Result.Start);
  end
  else if AEnd > AStart then
  begin
    Result := NyxPascalSpan(Result.Start,
      ReadToken(FTokens[AEnd - 1]).Span.Finish - Result.Start);
  end;
end;

{ Geometric growth remains within the admitted quota without integer overflow.
  Only the builder mutates vectors; unused capacity never enters public counts. }
function Capacity(ACount, AMaximum: Integer): Integer;
begin
  Result := AMaximum;

  if 16 + ACount div 2 <= AMaximum - ACount then
  begin
    Result := ACount + 16 + ACount div 2;
  end;
end;

procedure TSyntaxSnapshot.Problem(AIssue: TNyxPascalSyntaxIssue; const AText: TNyxText;
  const ASpan: TNyxPascalSpan);
var
  LAt: Integer;
begin
  LAt := FDiagnosticCount;

  if LAt >= FOptions.MaximumDiagnostics then
  begin
    raise ENyxPascalSyntaxLimit.Create('Pascal syntax diagnostics exceed their budget');
  end;

  if LAt = Length(FDiagnostics) then
  begin
    SetLength(FDiagnostics, Capacity(LAt, FOptions.MaximumDiagnostics));
  end;
  FDiagnostics[LAt].FIssue := AIssue;
  FDiagnostics[LAt].FSpan := ASpan;
  FDiagnostics[LAt].FMessage := AText;
  Inc(FDiagnosticCount);
end;

procedure TSyntaxSnapshot.Unsupported(AScope: Integer; const AText: TNyxText;
  const ASpan: TNyxPascalSpan);
var
  LScope: Integer;
begin
  Problem(psiUnsupportedSyntax, AText, ASpan);
  LScope := AScope;
  while LScope >= 0 do
  begin
    FScopes[LScope].FUnsupported := True;

    if FScopes[LScope].OwnerDeclaration >= 0 then
    begin
      Include(FDeclarations[FScopes[LScope].OwnerDeclaration].FFlags, psdfUnsupported);
      Break;
    end;
    LScope := FScopes[LScope].Parent;
  end;
end;

procedure TSyntaxSnapshot.Enter;
begin

  if FDepth >= FOptions.MaximumDepth then
  begin
    raise ENyxPascalSyntaxLimit.Create('Pascal syntax nesting exceeds its budget');
  end;
  Inc(FDepth);
end;

procedure TSyntaxSnapshot.Leave;
begin
  Dec(FDepth);
end;

procedure TSyntaxSnapshot.NodeBudget;
begin

  if FDeclarationCount + FSelectorCount + FArmCount + FLabelCount + FStatementLabelCount +
    FHintCount + FIntrinsicCount >=
    FOptions.MaximumDeclarations then
  begin
    raise ENyxPascalSyntaxLimit.Create('Declarations and structural metadata exceed their shared node budget');
  end;
end;

function TSyntaxSnapshot.AddScope(AKind: TNyxPascalSyntaxScopeKind;
  AParent, AOwner: Integer; AStart: Integer; const AName: TNyxText): Integer;
begin
  Result := FScopeCount;

  if Result >= FOptions.MaximumScopes then
  begin
    raise ENyxPascalSyntaxLimit.Create('Pascal syntax scopes exceed their budget');
  end;

  if Result = Length(FScopes) then
  begin
    SetLength(FScopes, Capacity(Result, FOptions.MaximumScopes));
  end;
  FScopes[Result].FKind := AKind;
  FScopes[Result].FParent := AParent;
  FScopes[Result].FOwner := AOwner;
  FScopes[Result].FFirst := -1;
  FScopes[Result].FLast := -1;
  FScopes[Result].FSpan := NyxPascalSpan(AStart, FLexical.Scalars - AStart);
  FScopes[Result].FName := AName;
  Inc(FScopeCount);
end;

procedure TSyntaxSnapshot.FinishScope(AScope: Integer; AComplete: Boolean);
begin
  FScopes[AScope].FComplete := AComplete;

  if AComplete then
  begin
    FScopes[AScope].FSpan := SpanFrom(FScopes[AScope].Span.Start);
  end;
end;

function TSyntaxSnapshot.AddDeclaration(AScope: Integer;
  AKind: TNyxPascalSyntaxDeclarationKind; AVisibility: TNyxPascalSyntaxVisibility;
  AName: Integer; AOperator: TNyxPascalOperatorKind; AAnonymous: Boolean): Integer;
var
  LToken: TNyxPascalToken;
begin
  NodeBudget;
  Result := FDeclarationCount;

  if Result >= FOptions.MaximumDeclarations then
  begin
    raise ENyxPascalSyntaxLimit.Create('Pascal declarations exceed their syntax budget');
  end;
  LToken := ReadToken(FTokens[AName]);

  if Result = Length(FDeclarations) then
  begin
    SetLength(FDeclarations, Capacity(Result, FOptions.MaximumDeclarations));
  end;

  if AAnonymous then
  begin
    FDeclarations[Result].FName := '';
  end
  else if (AOperator = pokNone) and not ((AKind = psdkLabel) and (LToken.Kind = ptkNumber)) then
  begin
    FDeclarations[Result].FName := FLexical.Identifier(FTokens[AName]);
  end
  else
  begin
    FDeclarations[Result].FName := FLexical.Text(FTokens[AName]);
  end;
  FDeclarations[Result].FKind := AKind;
  FDeclarations[Result].FOperator := AOperator;
  FDeclarations[Result].FVisibility := AVisibility;
  FDeclarations[Result].FNameSpan := LToken.Span;
  FDeclarations[Result].FScope := AScope;
  FDeclarations[Result].FNestedScope := -1;
  FDeclarations[Result].FNext := -1;
  FDeclarations[Result].FNextNamed := -1;
  FDeclarations[Result].FLastNamed := Result;
  FDeclarations[Result].FVariantArm := -1;
  FDeclarations[Result].FFirstHint := -1;
  FDeclarations[Result].FLastHint := -1;
  FDeclarations[Result].FFirstIntrinsic := -1;
  FDeclarations[Result].FLastIntrinsic := -1;
  FDeclarations[Result].FAnonymous := AAnonymous;

  if (AKind = psdkField) and (FActiveArm >= 0) and
    (FSelectors[FArms[FActiveArm].SelectorIndex].RecordScope = AScope) then
  begin
    FDeclarations[Result].FVariantArm := FActiveArm;
  end;
  Inc(FDeclarationCount);

  if FConditional[AName] then
  begin
    Include(FDeclarations[Result].FFlags, psdfConditional);
  end;

  LinkDeclaration(Result);
end;

procedure TSyntaxSnapshot.LinkDeclaration(AIndex: Integer);
var
  LScope: Integer;
  LKey: TNyxText;
  LFirst: Integer;
begin
  LScope := FDeclarations[AIndex].Scope;
  FDeclarations[AIndex].FNext := -1;
  FDeclarations[AIndex].FNextNamed := -1;
  FDeclarations[AIndex].FLastNamed := AIndex;

  if FScopes[LScope].FLast >= 0 then
  begin
    FDeclarations[FScopes[LScope].FLast].FNext := AIndex;
  end
  else
  begin
    FScopes[LScope].FFirst := AIndex;
  end;
  FScopes[LScope].FLast := AIndex;
  Inc(FScopes[LScope].FCount);

  if FDeclarations[AIndex].Anonymous then
  begin
    Exit;
  end;
  LKey := TNyxText(IntToStr(LScope)) + ':' + NyxPascalASCIIKey(FDeclarations[AIndex].Name);

  if FDeclarations[AIndex].OperatorKind <> pokNone then
  begin
    LKey := TNyxText(IntToStr(LScope)) + ':operator#' +
      TNyxText(IntToStr(Ord(FDeclarations[AIndex].OperatorKind)));
  end;
  LFirst := FNames.IndexOf(LKey);

  if LFirst < 0 then
  begin
    FNames.AddFirst(LKey, AIndex);
  end
  else
  begin
    FDeclarations[FDeclarations[LFirst].FLastNamed].FNextNamed := AIndex;
    FDeclarations[LFirst].FLastNamed := AIndex;
  end;
end;

procedure TSyntaxSnapshot.Separator;
begin

  if Sym(psoSemicolon) then
  begin
    Step;
  end
  else
  begin
    Problem(psiExpectedSeparator, 'Finish this declaration with a semicolon', Tok.Span);
  end;
end;

procedure TSyntaxSnapshot.Groups(AOpen, AClose: TNyxPascalSymbol);
begin
  Enter;
  try
    Step;
    while not EOF and not Sym(AClose) do
    begin

      if Sym(AOpen) then
      begin
        Groups(AOpen, AClose);
      end
      else if Sym(psoOpenParen) then
      begin
        Groups(psoOpenParen, psoCloseParen);
      end
      else if Sym(psoOpenBracket) then
      begin
        Groups(psoOpenBracket, psoCloseBracket);
      end
      else
      begin
        Step;
      end;
    end;

    if Sym(AClose) then
    begin
      Step;
    end
    else
    begin
      Problem(psiUnclosedScope, 'Finish this grouped Pascal expression', Tok.Span);
    end;
  finally
    Leave;
  end;
end;

procedure TSyntaxSnapshot.Angles;
var
  LDepth: Integer;
begin
  LDepth := 1;
  Step;
  while not EOF and (LDepth > 0) do
  begin

    if Sym(psoLess) then
    begin
      Inc(LDepth);

      if LDepth > FOptions.MaximumDepth then
      begin
        raise ENyxPascalSyntaxLimit.Create('Generic syntax nesting exceeds its budget');
      end;
    end
    else if Sym(psoGreater) then
    begin
      Dec(LDepth);
    end
    else if Sym(psoShiftRight) then
    begin
      Dec(LDepth, 2);
    end
    else if Sym(psoGreaterEqual) then
    begin
      Dec(LDepth);
      FPartialEqual := True;
      Break;
    end;
    Step;
  end;

  if LDepth <> 0 then
  begin
    Problem(psiUnclosedScope, 'Finish this generic parameter or argument list', Tok.Span);
  end;
end;

function TSyntaxSnapshot.HintWord(out AKind: TNyxPascalHintKind): Boolean;
var
  LKey: TNyxText;
begin
  Result := False;
  AKind := phkDeprecated;

  if (Tok.Kind <> ptkWord) or Tok.Escaped then
  begin
    Exit;
  end;
  LKey := NyxPascalASCIIKey(Name);

  if LKey = 'deprecated' then
  begin
    AKind := phkDeprecated;
  end
  else if LKey = 'platform' then
  begin
    AKind := phkPlatform;
  end
  else if LKey = 'experimental' then
  begin
    AKind := phkExperimental;
  end
  else if LKey = 'unimplemented' then
  begin
    AKind := phkUnimplemented;
  end
  else
  begin
    Exit;
  end;
  Result := True;
end;

function TSyntaxSnapshot.AtHint(AExpressionStart: Integer): Boolean;
var
  LKind: TNyxPascalHintKind;
  LPrevious: TNyxPascalToken;
begin
  Result := False;

  if (FPosition <= AExpressionStart) or not HintWord(LKind) then
  begin
    Exit;
  end;
  { Hint words are also legal identifiers. An expression operand, qualified
    member or escaped name must not be mistaken for a trailing declaration hint. }
  LPrevious := Tok(-1);
  Result := ((LPrevious.Kind in [ptkWord, ptkNumber, ptkString, ptkCharacter]) and
    not (LPrevious.Word in [pkwOf, pkwTo, pkwNot, pkwAnd, pkwOr, pkwXor,
      pkwDiv, pkwMod, pkwIn, pkwAs, pkwIs, pkwShl, pkwShr, pkwType, pkwPacked])) or
    (LPrevious.Symbol in [psoCloseParen, psoCloseBracket, psoGreater, psoShiftRight]);
end;

procedure TSyntaxSnapshot.DeclarationHints(ADeclaration: Integer);
var
  LKind: TNyxPascalHintKind;
  LAt: Integer;
  LStart: Integer;
  LMessage: Integer;
begin
  while HintWord(LKind) do
  begin
    NodeBudget;
    LAt := FHintCount;

    if LAt = Length(FHints) then
    begin
      SetLength(FHints, Capacity(LAt, FOptions.MaximumDeclarations));
    end;
    Inc(FHintCount);
    LStart := Tok.Span.Start;
    Step;
    LMessage := FPosition;
    while Tok.Kind in [ptkString, ptkCharacter] do
    begin
      Step;
    end;
    FHints[LAt].FDeclaration := ADeclaration;
    FHints[LAt].FKind := LKind;
    FHints[LAt].FMessage := Range(LMessage, FPosition);
    FHints[LAt].FSpan := SpanFrom(LStart);
    FHints[LAt].FNext := -1;

    if FDeclarations[ADeclaration].FFirstHint < 0 then
    begin
      FDeclarations[ADeclaration].FFirstHint := LAt;
    end
    else
    begin
      FHints[FDeclarations[ADeclaration].FLastHint].FNext := LAt;
    end;
    FDeclarations[ADeclaration].FLastHint := LAt;
  end;
end;

procedure TSyntaxSnapshot.CloneDeclarationHints(AFrom, ATo: Integer);
var
  LFrom: Integer;
  LAt: Integer;
begin
  LFrom := FDeclarations[AFrom].FirstHint;
  while LFrom >= 0 do
  begin
    NodeBudget;
    LAt := FHintCount;

    if LAt = Length(FHints) then
    begin
      SetLength(FHints, Capacity(LAt, FOptions.MaximumDeclarations));
    end;
    Inc(FHintCount);
    FHints[LAt] := FHints[LFrom];
    FHints[LAt].FDeclaration := ATo;
    FHints[LAt].FNext := -1;

    if FDeclarations[ATo].FFirstHint < 0 then
    begin
      FDeclarations[ATo].FFirstHint := LAt;
    end
    else
    begin
      FHints[FDeclarations[ATo].FLastHint].FNext := LAt;
    end;
    FDeclarations[ATo].FLastHint := LAt;
    LFrom := FHints[LFrom].NextHint;
  end;
end;

{ Modifier spellings shared by lookahead and the plain zero-operand path.
  Hints and platform/linker clauses use their own operand readers first. }
function RoutineModifier(AWord: TNyxPascalWord): Boolean;
begin
  Result := AWord in [pkwForward, pkwExternal, pkwAbstract, pkwOverload, pkwOverride,
    pkwVirtual, pkwDynamic, pkwInline, pkwReintroduce, pkwAssembler, pkwCdecl,
    pkwStdcall, pkwPascal, pkwSafecall, pkwRegister, pkwVarargs, pkwNostackframe,
    pkwStatic, pkwDeprecated, pkwPlatform, pkwExperimental, pkwFinal];
end;

{ Only contextual platform suffix keys enter this classification. Their values
  remain exact source syntax; no assembler symbol or intrinsic body is invented. }
function LinkageFlagNamed(const AName: TNyxText; out AFlag: TNyxPascalLinkageFlag): Boolean;
begin
  Result := True;

  if AName = 'public' then
  begin
    AFlag := plfPublic;
  end
  else if AName = 'export' then
  begin
    AFlag := plfExport;
  end
  else if AName = 'external' then
  begin
    AFlag := plfExternal;
  end
  else if AName = 'alias' then
  begin
    AFlag := plfAlias;
  end
  else if AName = 'cvar' then
  begin
    AFlag := plfCVariable;
  end
  else if AName = 'compilerproc' then
  begin
    AFlag := plfCompilerHelper;
  end
  else if AName = 'rtlproc' then
  begin
    AFlag := plfRTLProcedure;
  end
  else if AName = 'noreturn' then
  begin
    AFlag := plfNoReturn;
  end
  else if AName = 'internproc' then
  begin
    AFlag := plfIntrinsic;
  end
  else if AName = 'internconst' then
  begin
    AFlag := plfConstantIntrinsic;
  end
  else
  begin
    Result := False;
  end;
end;

function TSyntaxSnapshot.RoutineClause(AScope: Integer): Boolean;
var
  LFlag: TNyxPascalLinkageFlag;
  LHint: TNyxPascalHintKind;
begin
  Result := False;

  if (Tok.Kind <> ptkWord) or Tok.Escaped then
  begin
    Exit;
  end;
  Result := RoutineModifier(Tok.Word) or HintWord(LHint);

  if LinkageFlagNamed(NyxPascalASCIIKey(Name), LFlag) then
  begin
    Result := True;

    if (AScope >= 0) and (LFlag = plfPublic) then
    begin

      if FScopes[AScope].Kind = psskMembers then
      begin
        Result := False;
      end;
    end;
  end;
end;

procedure TSyntaxSnapshot.IntrinsicDeclaration(ADeclaration: Integer;
  AKind: TNyxPascalIntrinsicKind; const AKey, AOperand: TNyxPascalSpan; AEnvelope: Boolean);
var
  LAt: Integer;
begin
  NodeBudget;
  LAt := FIntrinsicCount;

  if LAt = Length(FIntrinsics) then
  begin
    SetLength(FIntrinsics, Capacity(LAt, FOptions.MaximumDeclarations));
  end;
  Inc(FIntrinsicCount);
  FIntrinsics[LAt].FDeclaration := ADeclaration;
  FIntrinsics[LAt].FKind := AKind;
  FIntrinsics[LAt].FKey := AKey;
  FIntrinsics[LAt].FOperand := AOperand;
  FIntrinsics[LAt].FSpan := SpanFrom(AKey.Start);
  FIntrinsics[LAt].FNext := -1;
  FIntrinsics[LAt].FComplete := AEnvelope and (AOperand.Length > 0);

  if FDeclarations[ADeclaration].FFirstIntrinsic < 0 then
  begin
    FDeclarations[ADeclaration].FFirstIntrinsic := LAt;
  end
  else
  begin
    FIntrinsics[FDeclarations[ADeclaration].FLastIntrinsic].FNext := LAt;
  end;
  FDeclarations[ADeclaration].FLastIntrinsic := LAt;

  if not FIntrinsics[LAt].Complete then
  begin
    Include(FDeclarations[ADeclaration].FFlags, psdfIncomplete);
    Problem(psiExpectedSeparator, 'An intrinsic attribute needs a colon and operand', AKey);
  end;
end;

function TSyntaxSnapshot.DeclarationLinkage(ADeclaration: Integer; ARoutine: Boolean): Boolean;
var
  LStart: Integer;
  LFlag: TNyxPascalLinkageFlag;
  LBracket: Boolean;
  LIntrinsic: Boolean;
  LKind: TNyxPascalIntrinsicKind;
  LKey: TNyxPascalSpan;
  LOperand: Integer;
  LColon: Boolean;
begin
  Result := False;
  LStart := Tok.Span.Start;
  LFlag := plfPlatformAttributes;
  LBracket := ARoutine and Sym(psoOpenBracket);

  if not LBracket and ((Tok.Kind <> ptkWord) or Tok.Escaped or
    not LinkageFlagNamed(NyxPascalASCIIKey(Name), LFlag)) then
  begin
    Exit;
  end;
  { A PUBLIC after a class method belongs to the next visibility section. }

  if not LBracket and (LFlag = plfPublic) and
    (FScopes[FDeclarations[ADeclaration].Scope].Kind = psskMembers) then
  begin
    Exit;
  end;

  if not ARoutine and
    (LFlag in [plfCompilerHelper, plfIntrinsic, plfConstantIntrinsic,
      plfRTLProcedure, plfNoReturn]) then
  begin
    Exit;
  end;
  Result := True;

  if LBracket then
  begin
    Include(FDeclarations[ADeclaration].FLinkage, plfPlatformAttributes);
    Step;
    while not EOF and not Sym(psoCloseBracket) and not Sym(psoSemicolon) and
      not Word(pkwBegin) and not Word(pkwEnd) and not Word(pkwImplementation) do
    begin
      LIntrinsic := False;
      { An unknown neighboring key must not inherit the preceding key's
        operand rules while its unsupported source remains preserved. }
      LFlag := plfPlatformAttributes;
      LKey := Tok.Span;

      if (Tok.Kind = ptkWord) and not Tok.Escaped and
        LinkageFlagNamed(NyxPascalASCIIKey(Name), LFlag) then
      begin
        Include(FDeclarations[ADeclaration].FLinkage, LFlag);
        LIntrinsic := LFlag in [plfIntrinsic, plfConstantIntrinsic];
      end
      else
      begin
        Unsupported(FDeclarations[ADeclaration].Scope,
          'This platform declaration attribute needs explicit meaning', Tok.Span);
        Include(FDeclarations[ADeclaration].FFlags, psdfUnsupported);
      end;
      Step;
      LColon := False;

      if (LFlag in [plfRTLProcedure, plfNoReturn]) and
        not (Sym(psoComma) or Sym(psoCloseBracket)) then
      begin
        Unsupported(FDeclarations[ADeclaration].Scope,
          'This routine declaration marker has no operand', Tok.Span);
        Include(FDeclarations[ADeclaration].FFlags, psdfUnsupported);
      end;

      if LIntrinsic and Sym(psoColon) then
      begin
        LColon := True;
        Step;
      end;
      LOperand := FPosition;
      while not EOF and not Sym(psoComma) and not Sym(psoCloseBracket) and
        not Sym(psoSemicolon) and not Word(pkwBegin) and not Word(pkwEnd) and
        not Word(pkwImplementation) do
      begin

        if Sym(psoOpenParen) then
        begin
          Groups(psoOpenParen, psoCloseParen);
        end
        else if Sym(psoOpenBracket) then
        begin
          Groups(psoOpenBracket, psoCloseBracket);
        end
        else
        begin
          Step;
        end;
      end;

      if LIntrinsic then
      begin
        LKind := pikRuntime;

        if LFlag = plfConstantIntrinsic then
        begin
          LKind := pikConstant;
        end;
        IntrinsicDeclaration(ADeclaration, LKind, LKey, Range(LOperand, FPosition),
          LColon and (Sym(psoComma) or Sym(psoCloseBracket)));
      end;

      if not Sym(psoComma) then
      begin
        Break;
      end;
      Step;
    end;

    if Sym(psoCloseBracket) then
    begin
      Step;
    end
    else
    begin
      Include(FDeclarations[ADeclaration].FFlags, psdfIncomplete);
      Problem(psiUnclosedScope, 'Finish this platform declaration attribute', Tok.Span);
    end;
  end
  else
  begin
    Include(FDeclarations[ADeclaration].FLinkage, LFlag);
    LKey := Tok.Span;
    Step;
    LColon := False;

    if (LFlag in [plfRTLProcedure, plfNoReturn]) and not Sym(psoSemicolon) and
      not (ARoutine and RoutineClause(FDeclarations[ADeclaration].Scope)) then
    begin
      Unsupported(FDeclarations[ADeclaration].Scope,
        'This routine declaration marker has no operand', Tok.Span);
      Include(FDeclarations[ADeclaration].FFlags, psdfUnsupported);
    end;

    if (LFlag in [plfIntrinsic, plfConstantIntrinsic]) and Sym(psoColon) then
    begin
      LColon := True;
      Step;
    end;
    LOperand := FPosition;

    if not (ARoutine and
      (LFlag in [plfRTLProcedure, plfNoReturn, plfCompilerHelper]) and
      RoutineClause(FDeclarations[ADeclaration].Scope)) then
    begin
      Expression(psoSemicolon, False, False, False, False, ARoutine);
    end;

    if LFlag in [plfIntrinsic, plfConstantIntrinsic] then
    begin
      LKind := pikRuntime;

      if LFlag = plfConstantIntrinsic then
      begin
        LKind := pikConstant;
      end;
      IntrinsicDeclaration(ADeclaration, LKind, LKey, Range(LOperand, FPosition),
        LColon and (Sym(psoSemicolon) or (ARoutine and RoutineClause(FDeclarations[ADeclaration].Scope))));
    end;
  end;

  if not Sym(psoSemicolon) and not (ARoutine and RoutineClause(FDeclarations[ADeclaration].Scope)) then
  begin
    Include(FDeclarations[ADeclaration].FFlags, psdfIncomplete);
  end;

  if Sym(psoSemicolon) or not (ARoutine and RoutineClause(FDeclarations[ADeclaration].Scope)) then
  begin
    Separator;
  end;

  if FDeclarations[ADeclaration].FLinkageSpan.Length > 0 then
  begin
    LStart := FDeclarations[ADeclaration].FLinkageSpan.Start;
  end;
  FDeclarations[ADeclaration].FLinkageSpan := SpanFrom(LStart);
end;

procedure TSyntaxSnapshot.Expression(AStop: TNyxPascalSymbol; AAlsoComma: Boolean;
  AHints: Boolean; AArmEnd: Boolean; AInitializerBoundary: Boolean; ARoutineBoundary: Boolean);
var
  LStart: Integer;
begin
  LStart := FPosition;
  while not EOF and not Sym(AStop) and not (AAlsoComma and Sym(psoComma)) and
    not (AInitializerBoundary and (Sym(psoEqual) or Sym(psoAssign))) and
    not (AArmEnd and Sym(psoCloseParen)) and
    not (AArmEnd and Sym(psoSemicolon)) and
    not Word(pkwEnd) and not Word(pkwImplementation) and not Word(pkwBegin) do
  begin

    if ARoutineBoundary and (FPosition > LStart) and not Sym(psoDot, -1) and
      not Sym(psoCaret, -1) and not Sym(psoColon, -1) and
      not Word(pkwOf, -1) and not Word(pkwTo, -1) and RoutineClause then
    begin
      Break;
    end;

    if AHints and AtHint(LStart) then
    begin
      Break;
    end;

    if not Tok.Complete then
    begin
      { Lexical recovery already identifies the uncertain token. Stop its value
        here so a following declaration is not swallowed as unfinished content. }
      Step;
      Break;
    end;

    if ARoutineBoundary and Sym(psoLess) then
    begin
      Angles;
    end
    else if Sym(psoOpenParen) then
    begin
      Groups(psoOpenParen, psoCloseParen);
    end
    else if Sym(psoOpenBracket) or Sym(psoAlternateOpenBracket) then
    begin

      if Sym(psoAlternateOpenBracket) then
      begin
        Groups(psoAlternateOpenBracket, psoAlternateCloseBracket);
      end
      else
      begin
        Groups(psoOpenBracket, psoCloseBracket);
      end;
    end
    else
    begin
      Step;
    end;
  end;
end;

function TSyntaxSnapshot.Form: TNyxPascalSyntaxTypeForm;
begin
  Result := pstfNamed;
  case Tok.Word of
    pkwClass:
      begin
        Result := pstfClass;

        if Word(pkwOf, 1) then
        begin
          Result := pstfClassReference;
        end;
      end;
    pkwInterface, pkwDispinterface:
      begin
        Result := pstfInterface;
      end;
    pkwRecord:
      begin
        Result := pstfRecord;
      end;
    pkwObject:
      begin
        Result := pstfObject;
      end;
    pkwArray:
      begin
        Result := pstfArray;
      end;
    pkwSet:
      begin
        Result := pstfSet;
      end;
    pkwProcedure, pkwFunction, pkwReference:
      begin
        Result := pstfRoutine;
      end;
  else
    begin
      { Other words remain an exact named type expression. }
    end;
  end;

  if Sym(psoCaret) then
  begin
    Result := pstfPointer;
  end
  else if Sym(psoOpenParen) then
  begin
    Result := pstfEnumeration;
  end;
end;

procedure TSyntaxSnapshot.RoutineTypeSuffix(AArmEnd: Boolean; AInitializerBoundary: Boolean);

  function Modifier(AOffset: Integer): Boolean;
  begin
    Result := (Tok(AOffset).Kind = ptkWord) and not Tok(AOffset).Escaped and
      ((Tok(AOffset).Word in [pkwCdecl, pkwStdcall, pkwPascal, pkwSafecall,
        pkwRegister, pkwVarargs, pkwInline]) or
        (NyxPascalASCIIKey(Name(AOffset)) = 'syscall') or
        (NyxPascalASCIIKey(Name(AOffset)) = 'vectorcall'));
  end;

begin
  while Sym(psoSemicolon) and Modifier(1) and
    (AInitializerBoundary or not Sym(psoEqual, 2)) and not Sym(psoLess, 2) and
    not Sym(psoColon, 2) and not Sym(psoComma, 2) do
  begin
    { Keep the separator inside the type's range. The structured type parser
      owns qualifier validation/conflicts; this scanner owns declaration bounds. }
    Step;
    Expression(psoSemicolon, False, True, AArmEnd, AInitializerBoundary);
  end;
end;

procedure TSyntaxSnapshot.StatementLabels(AScope: Integer);
var
  LStart: Integer;
  LFirst: Integer;
  LAt: Integer;
  LEntry: Integer;
  LDeclaration: Integer;
  LComplete: Boolean;
  LKind: TNyxPascalStatementLabelKind;
  LName: TNyxText;

  function LabelName: Boolean;
  var
    LText: TNyxText;
    LScalar: Integer;
  begin
    { Contextual words may be identifiers. Declaration/body boundaries are never
      consumed as recovery labels; complete compiler keyword admission is separate. }
    Result := (Tok.Kind = ptkWord) and not (Tok.Word in [pkwBegin, pkwEnd,
      pkwImplementation, pkwInterface, pkwInitialization, pkwFinalization,
      pkwVar, pkwThreadvar, pkwConst, pkwType, pkwResourcestring, pkwLabel,
      pkwProcedure, pkwFunction, pkwConstructor, pkwDestructor, pkwClass,
      pkwUses, pkwUnit, pkwProgram, pkwLibrary, pkwPackage, pkwAsm]);

    if Result then
    begin
      LKind := pslkIdentifier;
      LName := Name;
      Exit;
    end;
    Result := (Tok.Kind = ptkNumber) and Tok.Complete;

    if not Result then
    begin
      Exit;
    end;
    LText := FLexical.Text(FTokens[FPosition]);
    for LScalar := 1 to Length(LText) do
    begin

      if not (LText[LScalar] in ['0'..'9']) then
      begin
        Exit(False);
      end;
    end;
    LKind := pslkDecimal;
    LName := LText;
  end;

begin
  LStart := Tok.Span.Start;
  LFirst := FStatementLabelCount;
  LComplete := True;
  Step;
  repeat

    if not LabelName then
    begin
      Unsupported(AScope, 'A label list needs an identifier or decimal label', Tok.Span);
      LComplete := False;
      Break;
    end;
    LDeclaration := AddDeclaration(AScope, psdkLabel, psavLocal, FPosition);
    NodeBudget;
    LEntry := FStatementLabelCount;

    if LEntry = Length(FStatementLabels) then
    begin
      SetLength(FStatementLabels, Capacity(LEntry, FOptions.MaximumDeclarations));
    end;
    FStatementLabels[LEntry].FKind := LKind;
    FStatementLabels[LEntry].FDeclaration := LDeclaration;
    FStatementLabels[LEntry].FName := LName;
    FStatementLabels[LEntry].FScope := AScope;
    FStatementLabels[LEntry].FNameSpan := Tok.Span;
    FStatementLabels[LEntry].FConditional := FConditional[FPosition];
    Inc(FStatementLabelCount);
    Step;

    if not Sym(psoComma) then
    begin
      Break;
    end;
    Step;
  until False;

  if Sym(psoSemicolon) then
  begin
    Step;
  end
  else
  begin
    Unsupported(AScope, 'Finish this label list with a semicolon', Tok.Span);
    LComplete := False;
  end;
  for LAt := LFirst to FStatementLabelCount - 1 do
  begin
    FStatementLabels[LAt].FSectionSpan := SpanFrom(LStart);
    FStatementLabels[LAt].FComplete := LComplete;
    LDeclaration := FStatementLabels[LAt].DeclarationIndex;
    FDeclarations[LDeclaration].FHeaderSpan := FStatementLabels[LAt].SectionSpan;

    if not LComplete then
    begin
      Include(FDeclarations[LDeclaration].FFlags, psdfIncomplete);
    end;
  end;
end;

procedure TSyntaxSnapshot.StructuredBody(AScope, AOwner: Integer);
var
  LMemberScope: Integer;
  LAncestor: Integer;
  LParentArm: Integer;
begin
  Enter;
  LParentArm := FActiveArm;
  FActiveArm := -1;
  try
    Step;

    if Word(pkwSealed) or Word(pkwAbstract) then
    begin
      Step;
    end;

    if Sym(psoSemicolon) then
    begin
      Include(FDeclarations[AOwner].FFlags, psdfForward);
      Exit;
    end;

    if Sym(psoOpenParen) then
    begin
      LAncestor := FPosition;
      Groups(psoOpenParen, psoCloseParen);
      FDeclarations[AOwner].FAncestorSpan := Range(LAncestor, FPosition);
    end;
    LMemberScope := AddScope(psskMembers, AScope, AOwner, Tok.Span.Start,
      FDeclarations[AOwner].Name);
    FDeclarations[AOwner].FNestedScope := LMemberScope;

    if (FDeclarations[AOwner].TypeForm = pstfClass) and Sym(psoSemicolon) then
    begin
      { Delphi's empty descendant spelling class(TAncestor) has no end. }
      FScopes[LMemberScope].FComplete := True;
      FScopes[LMemberScope].FSpan := NyxPascalSpan(Tok.Span.Start, 0);
    end
    else
    begin
      Declarations(LMemberScope, psavUnspecified, True);

      if Word(pkwEnd) then
      begin
        Step;
        FinishScope(LMemberScope, True);
      end
      else
      begin
        Include(FDeclarations[AOwner].FFlags, psdfIncomplete);
        Problem(psiUnclosedScope, 'Finish this structured type with end', Tok.Span);
        FinishScope(LMemberScope, False);
      end;
    end;
  finally
    FActiveArm := LParentArm;
    Leave;
  end;
end;

procedure TSyntaxSnapshot.TypeExpression(AScope: Integer;
  AVisibility: TNyxPascalSyntaxVisibility; AStop: TNyxPascalSymbol; AArmEnd: Boolean);
var
  LStart: Integer;
  LBodyStart: Integer;
  LOwner: Integer;
begin
  LStart := FPosition;
  while not EOF and not Sym(AStop) and not Sym(psoSemicolon) and
    not Sym(psoEqual) and not Sym(psoAssign) and
    not (AArmEnd and Sym(psoCloseParen)) and not Word(pkwEnd) and
    not Word(pkwImplementation) and not Word(pkwBegin) do
  begin

    if AtHint(LStart) or ((FPosition > LStart) and
      (Tok.Word in [pkwType, pkwVar, pkwThreadvar, pkwConst, pkwResourcestring,
        pkwInitialization, pkwFinalization])) then
    begin
      Break;
    end;
    LBodyStart := FPosition;

    if Word(pkwPacked) and (Word(pkwRecord, 1) or Word(pkwObject, 1)) then
    begin
      Step;
    end;

    { A method pointer's of object is a qualifier, not an anonymous body. }

    if Word(pkwRecord) or (Word(pkwObject) and not Word(pkwOf, -1)) or
      Word(pkwInterface) or Word(pkwDispinterface) or
      (Word(pkwClass) and not Word(pkwOf, 1)) then
    begin
      LOwner := AddDeclaration(AScope, psdkType, AVisibility, FPosition, pokNone, True);
      FDeclarations[LOwner].FForm := Form;
      StructuredBody(AScope, LOwner);
      FDeclarations[LOwner].FTypeSpan := Range(LBodyStart, FPosition);
      FDeclarations[LOwner].FHeaderSpan := FDeclarations[LOwner].TypeSpan;
      FStructures.AddFirst(TNyxText(IntToStr(FDeclarations[LOwner].TypeSpan.Start)), LOwner);
    end
    else if Sym(psoOpenParen) then
    begin
      Groups(psoOpenParen, psoCloseParen);
    end
    else if Sym(psoOpenBracket) then
    begin
      Groups(psoOpenBracket, psoCloseBracket);
    end
    else if Sym(psoLess) then
    begin
      Angles;
    end
    else
    begin
      Step;
    end;
  end;
end;

procedure TSyntaxSnapshot.ParseType(AScope: Integer; AVisibility: TNyxPascalSyntaxVisibility);
var
  LEntry: Integer;
  LStart: Integer;
  LTypeStart: Integer;
  LGenericStart: Integer;
  LEnum: Integer;
  LForm: TNyxPascalSyntaxTypeForm;
begin
  Enter;
  try
    LStart := Tok.Span.Start;
    LEntry := AddDeclaration(AScope, psdkType, AVisibility, FPosition);
    Step;

    if Sym(psoLess) then
    begin
      LGenericStart := FPosition;
      Angles;
      FDeclarations[LEntry].FGenericSpan := Range(LGenericStart, FPosition);
    end;

    if not Sym(psoEqual) then
    begin
      Include(FDeclarations[LEntry].FFlags, psdfIncomplete);
      Problem(psiExpectedSeparator, 'Add an equals sign and type expression', Tok.Span);
      Separator;
      Exit;
    end;
    Step;
    LTypeStart := FPosition;

    if EOF or Sym(psoSemicolon) then
    begin
      Include(FDeclarations[LEntry].FFlags, psdfIncomplete);
      Problem(psiExpectedName, 'Add a type expression after the equals sign', Tok.Span);
      FDeclarations[LEntry].FTypeSpan := Range(LTypeStart, FPosition);
      Separator;
      FDeclarations[LEntry].FHeaderSpan := SpanFrom(LStart);
      Exit;
    end;

    if Word(pkwPacked) then
    begin
      Step;
    end;
    LForm := Form;
    FDeclarations[LEntry].FForm := LForm;

    if LForm in [pstfClass, pstfInterface, pstfRecord, pstfObject] then
    begin
      StructuredBody(AScope, LEntry);
    end
    else if LForm = pstfEnumeration then
    begin
      Step;
      while not EOF and not Sym(psoCloseParen) do
      begin

        if Tok.Kind = ptkWord then
        begin
          LEnum := AddDeclaration(AScope, psdkEnumValue, AVisibility, FPosition);
          Step;
          FDeclarations[LEnum].FTypeSpan := FDeclarations[LEntry].NameSpan;

          if Sym(psoEqual) or Sym(psoAssign) then
          begin
            Step;
            Expression(psoCloseParen, True);
          end;
        end
        else
        begin
          Problem(psiExpectedName, 'An enum entry needs an identifier', Tok.Span);
          Step;
        end;

        if Sym(psoComma) then
        begin
          Step;
        end
        else if not Sym(psoCloseParen) then
        begin
          Break;
        end;
      end;

      if Sym(psoCloseParen) then
      begin
        Step;
      end
      else
      begin
        Include(FDeclarations[LEntry].FFlags, psdfIncomplete);
        Problem(psiUnclosedScope, 'Finish this enumeration', Tok.Span);
      end;
    end
    else
    begin
      TypeExpression(AScope, AVisibility, psoSemicolon);

      if LForm = pstfRoutine then
      begin
        RoutineTypeSuffix;
      end;
    end;
    FDeclarations[LEntry].FTypeSpan := Range(LTypeStart, FPosition);

    if LForm in [pstfClass, pstfInterface, pstfRecord, pstfObject] then
    begin
      FStructures.AddFirst(TNyxText(IntToStr(FDeclarations[LEntry].TypeSpan.Start)), LEntry);
    end;
    DeclarationHints(LEntry);
    Separator;
    FDeclarations[LEntry].FHeaderSpan := SpanFrom(LStart);
  finally
    Leave;
  end;
end;

function SectionBoundary(AWord: TNyxPascalWord): Boolean;
begin
  Result := AWord in [pkwType, pkwVar, pkwThreadvar, pkwConst, pkwResourcestring,
    pkwProcedure, pkwFunction, pkwOperator, pkwConstructor, pkwDestructor, pkwImplementation,
    pkwInterface, pkwBegin, pkwEnd, pkwInitialization, pkwFinalization,
    pkwPrivate, pkwProtected, pkwPublic, pkwPublished, pkwStrict, pkwAutomated,
    pkwClass, pkwProperty, pkwGeneric];
end;

procedure TSyntaxSnapshot.TypeSection(AScope: Integer; AVisibility: TNyxPascalSyntaxVisibility);
var
  LBefore: Integer;
begin
  while not EOF do
  begin

    if Word(pkwGeneric) then
    begin
      Step;
    end
    else if SectionBoundary(Tok.Word) then
    begin
      Break;
    end;

    if Tok.Kind <> ptkWord then
    begin
      Break;
    end;
    LBefore := FPosition;

    if AScope = 0 then
    begin

      if FOptions.StartingContext = psfTypes then
      begin
        Remember(prTypesFragment, AVisibility, False, pkwNone);
      end
      else
      begin
        Remember(prTypes, AVisibility, FOptions.StartingContext = psfMembers, pkwNone);
      end;
    end;
    ParseType(AScope, AVisibility);

    if FPosition = LBefore then
    begin
      Step;
    end;
  end;
end;

procedure TSyntaxSnapshot.VariantFields(AScope: Integer; AVisibility: TNyxPascalSyntaxVisibility);
var
  LBefore: Integer;
begin
  while not EOF and not Sym(psoCloseParen) and not Word(pkwEnd) do
  begin
    LBefore := FPosition;

    if Word(pkwCase) then
    begin
      VariantPart(AScope, AVisibility);
    end
    else if Sym(psoSemicolon) then
    begin
      Step;
    end
    else if Tok.Kind = ptkWord then
    begin
      Variables(AScope, psdkField, AVisibility, False, True);
    end
    else
    begin
      Unsupported(AScope, 'A variant arm needs fields or a nested selector', Tok.Span);
      Step;
    end;

    if FPosition = LBefore then
    begin
      Step;
    end;
  end;
end;

procedure TSyntaxSnapshot.VariantPart(AScope: Integer; AVisibility: TNyxPascalSyntaxVisibility);
var
  LSelector: Integer;
  LArm: Integer;
  LLabel: Integer;
  LStart: Integer;
  LType: Integer;
  LLabelStart: Integer;
  LParent: Integer;
  LTag: Integer;
begin
  Enter;
  try
    LParent := FActiveArm;
    LStart := Tok.Span.Start;
    NodeBudget;
    LSelector := FSelectorCount;

    if LSelector = Length(FSelectors) then
    begin
      SetLength(FSelectors, Capacity(LSelector, FOptions.MaximumDeclarations));
    end;
    Inc(FSelectorCount);
    FSelectors[LSelector].FScope := AScope;
    FSelectors[LSelector].FParent := LParent;
    FSelectors[LSelector].FTag := -1;
    FSelectors[LSelector].FFirstArm := -1;
    FSelectors[LSelector].FLastArm := -1;
    Step;

    if (Tok.Kind = ptkWord) and Sym(psoColon, 1) then
    begin
      LTag := AddDeclaration(AScope, psdkField, AVisibility, FPosition);
      FSelectors[LSelector].FTag := LTag;
      Step;
      Step;
    end;
    LType := FPosition;
    while not EOF and not Word(pkwOf) and not Word(pkwEnd) and not Sym(psoCloseParen) do
    begin
      Step;
    end;
    FSelectors[LSelector].FType := Range(LType, FPosition);

    if FSelectors[LSelector].FTag >= 0 then
    begin
      LTag := FSelectors[LSelector].FTag;
      FDeclarations[LTag].FTypeSpan := FSelectors[LSelector].TypeSpan;
      FDeclarations[LTag].FHeaderSpan := NyxPascalSpan(FDeclarations[LTag].NameSpan.Start,
        EndOffset - FDeclarations[LTag].NameSpan.Start);
      FDeclarations[LTag].FForm := pstfNamed;
    end;

    if not Word(pkwOf) or (FSelectors[LSelector].TypeSpan.Length = 0) then
    begin
      Unsupported(AScope, 'A variant selector needs its type and of keyword', Tok.Span);
      FSelectors[LSelector].FSpan := SpanFrom(LStart);
      Exit;
    end;
    Step;
    while not EOF and not Word(pkwEnd) and not Sym(psoCloseParen) do
    begin
      NodeBudget;
      LArm := FArmCount;

      if LArm = Length(FArms) then
      begin
        SetLength(FArms, Capacity(LArm, FOptions.MaximumDeclarations));
      end;
      Inc(FArmCount);
      FArms[LArm].FSelector := LSelector;
      FArms[LArm].FFirstLabel := -1;
      FArms[LArm].FLastLabel := -1;
      FArms[LArm].FNext := -1;

      if FSelectors[LSelector].FFirstArm < 0 then
      begin
        FSelectors[LSelector].FFirstArm := LArm;
      end
      else
      begin
        FArms[FSelectors[LSelector].FLastArm].FNext := LArm;
      end;
      FSelectors[LSelector].FLastArm := LArm;
      LLabelStart := Tok.Span.Start;
      repeat
        LType := FPosition;
        Expression(psoColon, True, False, True);

        if LType = FPosition then
        begin
          Unsupported(AScope, 'A variant alternative needs a label expression', Tok.Span);
          FArms[LArm].FSpan := SpanFrom(LLabelStart);
          FSelectors[LSelector].FSpan := SpanFrom(LStart);
          Exit;
        end;
        NodeBudget;
        LLabel := FLabelCount;

        if LLabel = Length(FLabels) then
        begin
          SetLength(FLabels, Capacity(LLabel, FOptions.MaximumDeclarations));
        end;
        Inc(FLabelCount);
        FLabels[LLabel].FArm := LArm;
        FLabels[LLabel].FSpan := Range(LType, FPosition);
        FLabels[LLabel].FNext := -1;

        if FArms[LArm].FFirstLabel < 0 then
        begin
          FArms[LArm].FFirstLabel := LLabel;
        end
        else
        begin
          FLabels[FArms[LArm].FLastLabel].FNext := LLabel;
        end;
        FArms[LArm].FLastLabel := LLabel;

        if not Sym(psoComma) then
        begin
          Break;
        end;
        Step;
      until EOF;

      if not Sym(psoColon) then
      begin
        Unsupported(AScope, 'Finish variant labels with a colon', Tok.Span);
        FArms[LArm].FSpan := SpanFrom(LLabelStart);
        FSelectors[LSelector].FSpan := SpanFrom(LStart);
        Exit;
      end;
      Step;

      if not Sym(psoOpenParen) then
      begin
        Unsupported(AScope, 'A variant arm needs parenthesized fields', Tok.Span);
        FArms[LArm].FSpan := SpanFrom(LLabelStart);
        FSelectors[LSelector].FSpan := SpanFrom(LStart);
        Exit;
      end;
      Step;
      FActiveArm := LArm;
      try
        VariantFields(AScope, AVisibility);
      finally
        FActiveArm := LParent;
      end;

      if not Sym(psoCloseParen) then
      begin
        Unsupported(AScope, 'Finish this variant arm with )', Tok.Span);
        FArms[LArm].FSpan := SpanFrom(LLabelStart);
        FSelectors[LSelector].FSpan := SpanFrom(LStart);
        Exit;
      end;
      Step;
      FArms[LArm].FComplete := True;
      FArms[LArm].FSpan := SpanFrom(LLabelStart);

      if Sym(psoSemicolon) then
      begin
        Step;
      end;
    end;
    FSelectors[LSelector].FComplete := FSelectors[LSelector].FirstArm >= 0;
    FSelectors[LSelector].FSpan := SpanFrom(LStart);

    if not FSelectors[LSelector].Complete then
    begin
      Unsupported(AScope, 'A variant selector needs at least one alternative', Tok.Span);
    end;
  finally
    FActiveArm := LParent;
    Leave;
  end;
end;

procedure TSyntaxSnapshot.Variables(AScope: Integer; AKind: TNyxPascalSyntaxDeclarationKind;
  AVisibility: TNyxPascalSyntaxVisibility; AClass: Boolean; AArmEnd: Boolean);
var
  LEntries: TOrdinals;
  LEntry: Integer;
  LStart: Integer;
  LType: Integer;
  LExpression: Integer;
  LAt: Integer;
  LTypeSpan: TNyxPascalSpan;
  LExpressionSpan: TNyxPascalSpan;
  LForm: TNyxPascalSyntaxTypeForm;
  LInitialized: Boolean;
begin
  LStart := Tok.Span.Start;
  LEntries := nil;
  repeat

    if Tok.Kind <> ptkWord then
    begin
      Problem(psiExpectedName, 'A variable or field needs an identifier', Tok.Span);
      Break;
    end;
    LEntry := AddDeclaration(AScope, AKind, AVisibility, FPosition);
    SetLength(LEntries, Length(LEntries) + 1);
    LEntries[High(LEntries)] := LEntry;

    if AClass then
    begin
      Include(FDeclarations[LEntry].FFlags, psdfClassMember);
    end;
    Step;

    if not Sym(psoComma) then
    begin
      Break;
    end;
    Step;
  until EOF;
  LTypeSpan := NyxPascalSpan(Tok.Span.Start, 0);
  LExpressionSpan := LTypeSpan;
  LForm := pstfNone;
  LInitialized := False;

  if Sym(psoColon) then
  begin
    Step;
    LType := FPosition;
    LForm := Form;

    if Word(pkwProcedure) or Word(pkwFunction) or Word(pkwReference) then
    begin
      { Parameter defaults stay inside their groups. Only an outer initializer
        ends the type, including after a calling-convention separator. }
      Expression(psoSemicolon, False, True, AArmEnd, True);
      RoutineTypeSuffix(AArmEnd, True);
    end
    else
    begin
      TypeExpression(AScope, AVisibility, psoSemicolon, AArmEnd);
    end;
    LTypeSpan := Range(LType, FPosition);
  end
  else if not Sym(psoAssign) then
  begin
    Problem(psiExpectedSeparator, 'A variable needs a declared type or explicit initializer', Tok.Span);
  end;

  { Native Pascal admits hints between the declared type and its initializer.
    Consume them before checking '='; retain one ordered owner until any trailing
    hints have also been captured and the group can receive independent links. }

  if Length(LEntries) > 0 then
  begin
    DeclarationHints(LEntries[0]);
  end;

  if Sym(psoEqual) or Sym(psoAssign) then
  begin
    LInitialized := True;
    Step;
    LExpression := FPosition;
    Expression(psoSemicolon, False, True, AArmEnd);
    LExpressionSpan := Range(LExpression, FPosition);

    if LExpressionSpan.Length = 0 then
    begin
      Problem(psiExpectedName, 'Add a value after the variable initializer', Tok.Span);
    end;
  end;
  for LAt := 0 to High(LEntries) do
  begin
    LEntry := LEntries[LAt];
    FDeclarations[LEntry].FTypeSpan := LTypeSpan;
    FDeclarations[LEntry].FExpressionSpan := LExpressionSpan;
    FDeclarations[LEntry].FForm := LForm;

    if ((LTypeSpan.Length = 0) and (LExpressionSpan.Length = 0)) or
      (LInitialized and (LExpressionSpan.Length = 0)) then
    begin
      Include(FDeclarations[LEntry].FFlags, psdfIncomplete);
    end;
  end;

  if Length(LEntries) > 0 then
  begin
    DeclarationHints(LEntries[0]);
    for LAt := 1 to High(LEntries) do
    begin
      CloneDeclarationHints(LEntries[0], LEntries[LAt]);
    end;
  end;

  if not (AArmEnd and Sym(psoCloseParen)) and
    not ((AKind = psdkField) and Word(pkwEnd)) then
  begin
    Separator;
  end;

  if (Length(LEntries) > 0) and (AKind <> psdkField) then
  begin
    while DeclarationLinkage(LEntries[0], False) do
    begin
      { One authored group owns the same suffix for each independent entry. }
    end;
    for LAt := 1 to High(LEntries) do
    begin
      FDeclarations[LEntries[LAt]].FLinkage := FDeclarations[LEntries[0]].FLinkage;
      FDeclarations[LEntries[LAt]].FLinkageSpan := FDeclarations[LEntries[0]].FLinkageSpan;

      if psdfIncomplete in FDeclarations[LEntries[0]].Flags then
      begin
        Include(FDeclarations[LEntries[LAt]].FFlags, psdfIncomplete);
      end;
    end;
  end;
  for LAt := 0 to High(LEntries) do
  begin
    FDeclarations[LEntries[LAt]].FHeaderSpan := SpanFrom(LStart);
  end;
end;

procedure TSyntaxSnapshot.Constants(AScope: Integer;
  AKind: TNyxPascalSyntaxDeclarationKind; AVisibility: TNyxPascalSyntaxVisibility);
var
  LEntry: Integer;
  LStart: Integer;
  LType: Integer;
  LExpression: Integer;
begin
  LStart := Tok.Span.Start;
  LEntry := AddDeclaration(AScope, AKind, AVisibility, FPosition);
  Step;

  if Sym(psoColon) then
  begin
    Step;
    LType := FPosition;
    TypeExpression(AScope, AVisibility, psoEqual);
    FDeclarations[LEntry].FTypeSpan := Range(LType, FPosition);
  end;

  if Sym(psoEqual) then
  begin
    Step;
    LExpression := FPosition;
    Expression(psoSemicolon, False, True);
    FDeclarations[LEntry].FExpressionSpan := Range(LExpression, FPosition);
  end
  else
  begin
    Include(FDeclarations[LEntry].FFlags, psdfIncomplete);
    Problem(psiExpectedSeparator, 'A constant needs its value expression', Tok.Span);
  end;
  DeclarationHints(LEntry);
  Separator;
  while DeclarationLinkage(LEntry, False) do
  begin
    { Typed constants may also carry native linkage without changing visibility. }
  end;
  FDeclarations[LEntry].FHeaderSpan := SpanFrom(LStart);
end;

procedure TSyntaxSnapshot.Parameters(AScope: Integer);
var
  LEntries: TOrdinals;
  LEntry: Integer;
  LType: Integer;
  LMode: TNyxPascalSyntaxParameterMode;
  LAt: Integer;
begin
  Step;
  while not EOF and not Sym(psoCloseParen) do
  begin
    LMode := pspmValue;
    case Tok.Word of
      pkwConst:
        begin
          LMode := pspmConst;
        end;
      pkwVar:
        begin
          LMode := pspmVar;
        end;
      pkwOut:
        begin
          LMode := pspmOut;
        end;
      pkwConstref:
        begin
          LMode := pspmConstRef;
        end;
    else
      begin
        { Ordinary parameter names retain value passing. }
      end;
    end;

    if LMode <> pspmValue then
    begin
      Step;
    end;
    LEntries := nil;
    repeat

      if Tok.Kind <> ptkWord then
      begin
        Problem(psiExpectedName, 'A parameter needs an identifier', Tok.Span);
        Break;
      end;
      LEntry := AddDeclaration(AScope, psdkParameter, psavLocal, FPosition);
      FDeclarations[LEntry].FParameterMode := LMode;
      SetLength(LEntries, Length(LEntries) + 1);
      LEntries[High(LEntries)] := LEntry;
      Step;

      if not Sym(psoComma) then
      begin
        Break;
      end;
      Step;
    until EOF;

    if Sym(psoColon) then
    begin
      Step;
      LType := FPosition;
      while not EOF and not Sym(psoSemicolon) and not Sym(psoCloseParen) and not Sym(psoEqual) do
      begin

        if Sym(psoOpenParen) then
        begin
          Groups(psoOpenParen, psoCloseParen);
        end
        else if Sym(psoOpenBracket) then
        begin
          Groups(psoOpenBracket, psoCloseBracket);
        end
        else if Sym(psoLess) then
        begin
          Angles;
        end
        else
        begin
          Step;
        end;
      end;
      for LAt := 0 to High(LEntries) do
      begin
        FDeclarations[LEntries[LAt]].FTypeSpan := Range(LType, FPosition);
      end;
    end;

    if Sym(psoEqual) then
    begin
      Step;
      while not EOF and not Sym(psoSemicolon) and not Sym(psoCloseParen) do
      begin

        if Sym(psoOpenParen) then
        begin
          Groups(psoOpenParen, psoCloseParen);
        end
        else
        begin
          Step;
        end;
      end;
    end;

    if not Sym(psoSemicolon) then
    begin
      Break;
    end;
    Step;
  end;

  if Sym(psoCloseParen) then
  begin
    Step;
  end
  else
  begin
    Problem(psiUnclosedScope, 'Finish this parameter list', Tok.Span);
  end;
end;

{ Read an authored operator token, without converting its spelling into a fake
  identifier or inferring arity/eligibility. Named aliases share a query family. }
function OperatorToken(const AToken: TNyxPascalToken; const AText: TNyxText): TNyxPascalOperatorKind;
const
  CNames: array[TNyxPascalOperatorKind] of TNyxText =
    ('', '', 'add', 'subtract', 'multiply', 'divide', 'intdivide', 'modulus', '',
      'equal', 'notequal', 'lessthan', 'lessthanorequal', 'greaterthan',
      'greaterthanorequal', 'implicit', 'explicit', 'not', 'and', 'or', 'xor',
      'shl', 'shr', 'in', '', 'inc', 'dec', 'enumerator', 'initialize',
      'finalize', 'addref', 'copy');
var
  LKind: TNyxPascalOperatorKind;
  LKey: TNyxText;
begin
  Result := pokUnknown;

  if AToken.Escaped then
  begin
    Exit;
  end;

  if AToken.Kind = ptkSymbol then
  begin
    case AToken.Symbol of
      psoPlus:
        begin
          Result := pokAdd;
        end;
      psoMinus:
        begin
          Result := pokSubtract;
        end;
      psoStar:
        begin
          Result := pokMultiply;
        end;
      psoSlash:
        begin
          Result := pokDivide;
        end;
      psoPower:
        begin
          Result := pokPower;
        end;
      psoEqual:
        begin
          Result := pokEqual;
        end;
      psoNotEqual:
        begin
          Result := pokNotEqual;
        end;
      psoLess:
        begin
          Result := pokLess;
        end;
      psoLessEqual:
        begin
          Result := pokLessEqual;
        end;
      psoGreater:
        begin
          Result := pokGreater;
        end;
      psoGreaterEqual:
        begin
          Result := pokGreaterEqual;
        end;
      psoAssign:
        begin
          Result := pokImplicit;
        end;
      psoSymmetricDifference:
        begin
          Result := pokSymmetricDifference;
        end;
    else
      begin
        { Unsupported punctuation remains explicitly unknown. }
      end;
    end;
    Exit;
  end;

  if AToken.Kind <> ptkWord then
  begin
    Exit;
  end;
  LKey := NyxPascalASCIIKey(AText);
  for LKind := pokAdd to High(TNyxPascalOperatorKind) do
  begin

    if (CNames[LKind] <> '') and (CNames[LKind] = LKey) then
    begin
      Exit(LKind);
    end;
  end;

  if LKey = 'div' then
  begin
    Result := pokIntDivide;
  end
  else if LKey = 'mod' then
  begin
    Result := pokModulus;
  end
  else if LKey = 'positive' then
  begin
    Result := pokAdd;
  end
  else if LKey = 'negative' then
  begin
    Result := pokSubtract;
  end
  else if LKey = 'logicalnot' then
  begin
    Result := pokNot;
  end
  else if (LKey = 'logicaland') or (LKey = 'bitwiseand') then
  begin
    Result := pokAnd;
  end
  else if (LKey = 'logicalor') or (LKey = 'bitwiseor') then
  begin
    Result := pokOr;
  end
  else if (LKey = 'logicalxor') or (LKey = 'bitwisexor') then
  begin
    Result := pokXor;
  end
  else if LKey = 'leftshift' then
  begin
    Result := pokShiftLeft;
  end
  else if LKey = 'rightshift' then
  begin
    Result := pokShiftRight;
  end;
end;

procedure TSyntaxSnapshot.Routine(AScope: Integer;
  AVisibility: TNyxPascalSyntaxVisibility; AClass: Boolean);
var
  LEntry: Integer;
  LScope: Integer;
  LStart: Integer;
  LName: Integer;
  LFinalName: Integer;
  LType: Integer;
  LGeneric: Integer;
  LHasBody: Boolean;
  LIsFunction: Boolean;
  LIsOperator: Boolean;
  LOperator: TNyxPascalOperatorKind;
  LResultEntry: Integer;
  LFullName: TNyxText;
  LOwner: TNyxText;
  LGenericSpan: TNyxPascalSpan;
  LHint: TNyxPascalHintKind;
begin
  Enter;
  try
    LStart := Tok.Span.Start;
    LIsFunction := Word(pkwFunction);
    LIsOperator := Word(pkwOperator);
    LOperator := pokNone;
    LResultEntry := -1;
    Step;

    if (Tok.Kind <> ptkWord) and
      not (LIsOperator and (OperatorToken(Tok, FLexical.Text(FTokens[FPosition])) <> pokUnknown)) then
    begin

      if LIsOperator then
      begin
        Unsupported(AScope, 'This operator needs a supported token or named spelling', Tok.Span);
      end
      else
      begin
        Problem(psiExpectedName, 'A routine needs an identifier', Tok.Span);
      end;
      Expression(psoSemicolon);
      Separator;
      Exit;
    end;
    LName := FPosition;
    LFinalName := LName;
    LGenericSpan := NyxPascalSpan(Tok.Span.Start, 0);
    LFullName := Name;

    if LIsOperator and (Tok.Kind <> ptkWord) then
    begin
      LFullName := FLexical.Text(FTokens[FPosition]);
    end;
    Step;
    LOwner := '';

    if Sym(psoLess) then
    begin
      LGeneric := FPosition;
      Angles;
      LGenericSpan := Range(LGeneric, FPosition);
    end;
    while Sym(psoDot) and ((Tok(1).Kind = ptkWord) or LIsOperator) do
    begin
      Step;
      LOwner := LFullName;
      LFullName := LFullName + '.' + FLexical.Text(FTokens[FPosition]);
      LFinalName := FPosition;
      Step;

      if Sym(psoLess) then
      begin
        LGeneric := FPosition;
        Angles;
        LGenericSpan := Range(LGeneric, FPosition);
      end;
    end;

    if LIsOperator then
    begin
      LOperator := OperatorToken(ReadToken(FTokens[LFinalName]), FLexical.Text(FTokens[LFinalName]));

      if LOperator = pokUnknown then
      begin
        Unsupported(AScope, 'This operator spelling needs an explicit supported identity',
          ReadToken(FTokens[LFinalName]).Span);
      end;
    end;
    LEntry := AddDeclaration(AScope, psdkRoutine, AVisibility, LFinalName, LOperator);
    FDeclarations[LEntry].FGenericSpan := LGenericSpan;
    FDeclarations[LEntry].FOwnerName := LOwner;

    if not LIsOperator then
    begin
      FDeclarations[LEntry].FNameSpan := Range(LName, FPosition);
    end;

    if LIsOperator then
    begin
      Include(FDeclarations[LEntry].FFlags, psdfOverload);

      if LOperator = pokUnknown then
      begin
        Include(FDeclarations[LEntry].FFlags, psdfUnsupported);
      end;
    end;

    if AClass then
    begin
      Include(FDeclarations[LEntry].FFlags, psdfClassMember);
    end;
    LScope := AddScope(psskRoutine, AScope, LEntry, LStart, LFullName);
    FDeclarations[LEntry].FNestedScope := LScope;

    if Sym(psoOpenParen) then
    begin
      Parameters(LScope);
    end;

    if LIsOperator and (Tok.Kind = ptkWord) and Sym(psoColon, 1) then
    begin
      LResultEntry := AddDeclaration(LScope, psdkVariable, psavLocal, FPosition);
      Step;
    end;

    if (LIsFunction or LIsOperator) and Sym(psoColon) then
    begin
      Step;
      LType := FPosition;
      Expression(psoSemicolon, False, True, False, False, True);
      FDeclarations[LEntry].FTypeSpan := Range(LType, FPosition);

      if LResultEntry >= 0 then
      begin
        FDeclarations[LResultEntry].FTypeSpan := FDeclarations[LEntry].FTypeSpan;
      end;
    end;
    DeclarationHints(LEntry);

    if Sym(psoSemicolon) or (not RoutineClause(AScope) and not Sym(psoOpenBracket)) then
    begin

      if not Sym(psoSemicolon) then
      begin
        Include(FDeclarations[LEntry].FFlags, psdfIncomplete);
      end;
      Separator;
    end;
    LHasBody := not (AVisibility in [psavInterface]) and
      (FScopes[AScope].Kind <> psskMembers);
    while not EOF do
    begin

      if DeclarationLinkage(LEntry, True) then
      begin

        if (plfExternal in FDeclarations[LEntry].Linkage) or
          (plfIntrinsic in FDeclarations[LEntry].Linkage) then
        begin
          LHasBody := False;
          Include(FDeclarations[LEntry].FFlags, psdfForward);
        end;
        Continue;
      end;

      if not RoutineModifier(Tok.Word) and not HintWord(LHint) then
      begin
        Break;
      end;

      if HintWord(LHint) then
      begin
        DeclarationHints(LEntry);
        Separator;
        Continue;
      end;

      if Word(pkwForward) or Word(pkwExternal) or Word(pkwAbstract) then
      begin
        LHasBody := False;
        Include(FDeclarations[LEntry].FFlags, psdfForward);
      end;

      if Word(pkwOverload) then
      begin
        Include(FDeclarations[LEntry].FFlags, psdfOverload);
      end;
      Step;

      if Sym(psoSemicolon) then
      begin
        Step;
      end
      else if not RoutineClause(AScope) and not Sym(psoOpenBracket) then
      begin
        Include(FDeclarations[LEntry].FFlags, psdfIncomplete);
        Separator;
      end;
    end;
    FDeclarations[LEntry].FHeaderSpan := SpanFrom(LStart);

    if LHasBody then
    begin
      Declarations(LScope, psavLocal, False);

      if Word(pkwBegin) or Word(pkwAsm) then
      begin

        if not Block(LScope) then
        begin
          Include(FDeclarations[LEntry].FFlags, psdfIncomplete);
        end;
        Separator;
      end
      else
      begin
        Include(FDeclarations[LEntry].FFlags, psdfIncomplete);
        Problem(psiUnclosedScope, 'Finish this routine implementation', Tok.Span);
      end;
    end;
    FinishScope(LScope, not (psdfIncomplete in FDeclarations[LEntry].Flags));
  finally
    Leave;
  end;
end;

procedure TSyntaxSnapshot.PropertyDeclaration(AScope: Integer;
  AVisibility: TNyxPascalSyntaxVisibility; AClass: Boolean);
var
  LStart: Integer;
  LEntry: Integer;
  LType: Integer;
begin
  LStart := Tok.Span.Start;
  Step;

  if Tok.Kind <> ptkWord then
  begin
    Problem(psiExpectedName, 'A property needs an identifier', Tok.Span);
    Expression(psoSemicolon);
    Separator;
    Exit;
  end;
  LEntry := AddDeclaration(AScope, psdkProperty, AVisibility, FPosition);

  if AClass then
  begin
    Include(FDeclarations[LEntry].FFlags, psdfClassMember);
  end;
  Step;

  if Sym(psoOpenBracket) then
  begin
    Groups(psoOpenBracket, psoCloseBracket);
  end;

  if Sym(psoColon) then
  begin
    Step;
    LType := FPosition;
    while not EOF and not Sym(psoSemicolon) and
      not (Tok.Word in [pkwRead, pkwWrite, pkwStored, pkwDefault, pkwNodefault,
        pkwImplements, pkwIndex, pkwDispid]) and not Word(pkwEnd) do
    begin

      if AtHint(LType) then
      begin
        Break;
      end;

      if Sym(psoLess) then
      begin
        Angles;
      end
      else
      begin
        Step;
      end;
    end;
    FDeclarations[LEntry].FTypeSpan := Range(LType, FPosition);
    FDeclarations[LEntry].FForm := pstfNamed;
  end;
  DeclarationHints(LEntry);
  Expression(psoSemicolon, False, True);
  DeclarationHints(LEntry);
  Separator;

  if Word(pkwDefault) or Word(pkwNodefault) then
  begin
    Step;
    Separator;
  end;
  FDeclarations[LEntry].FHeaderSpan := SpanFrom(LStart);
end;

procedure TSyntaxSnapshot.Imports(AScope: Integer);
var
  LName: TNyxText;
  LStart: Integer;
  LEntry: Integer;
begin
  Step;
  while not EOF and (Tok.Kind = ptkWord) do
  begin
    LStart := FPosition;
    LName := Name;
    Step;
    while Sym(psoDot) and (Tok(1).Kind = ptkWord) do
    begin
      Step;
      LName := LName + '.' + Name;
      Step;
    end;
    LEntry := FImportCount;

    if LEntry >= FOptions.MaximumDeclarations then
    begin
      raise ENyxPascalSyntaxLimit.Create('Pascal import syntax exceeds its budget');
    end;

    if LEntry = Length(FImports) then
    begin
      SetLength(FImports, Capacity(LEntry, FOptions.MaximumDeclarations));
    end;
    FImports[LEntry].FName := LName;
    FImports[LEntry].FSpan := Range(LStart, FPosition);
    FImports[LEntry].FScope := AScope;
    FImports[LEntry].FConditional := FConditional[LStart];
    Inc(FImportCount);

    if Word(pkwIn) then
    begin
      Step;
      Expression(psoSemicolon, True);
    end;

    if not Sym(psoComma) then
    begin
      Break;
    end;
    Step;
  end;
  Separator;
end;

procedure TSyntaxSnapshot.Declarations(AScope: Integer;
  AVisibility: TNyxPascalSyntaxVisibility; AMembers: Boolean; ASection: TNyxPascalWord);
var
  LSection: TNyxPascalWord;
  LBefore: Integer;
  LClass: Boolean;
  LGeneric: Boolean;
  LStrict: Boolean;
begin
  Enter;
  try
    LSection := ASection;
    while not EOF and not Word(pkwEnd) and not Word(pkwBegin) and not Word(pkwAsm) do
    begin
      LBefore := FPosition;

      if AScope = 0 then
      begin
        Remember(prDeclarations, AVisibility, AMembers, LSection);
      end;

      if Word(pkwInterface) then
      begin
        AVisibility := psavInterface;
        LSection := pkwNone;
        Step;
      end
      else if Word(pkwImplementation) then
      begin

        if AScope <> 0 then
        begin
          Break;
        end;
        AVisibility := psavImplementation;
        LSection := pkwNone;
        Step;
      end
      else if Tok.Word in [pkwUses, pkwRequires, pkwContains] then
      begin
        Imports(AScope);
      end
      else if Word(pkwType) then
      begin
        Step;
        TypeSection(AScope, AVisibility);
        LSection := pkwNone;
      end
      else if Tok.Word in [pkwVar, pkwThreadvar, pkwConst, pkwResourcestring] then
      begin
        LSection := Tok.Word;
        Step;
      end
      else if Word(pkwLabel) and not AMembers then
      begin
        StatementLabels(AScope);
        LSection := pkwNone;
      end
      else if AMembers and (Tok.Word in [pkwPrivate, pkwProtected, pkwPublic, pkwPublished,
        pkwAutomated, pkwStrict]) then
      begin
        LStrict := Word(pkwStrict);

        if LStrict then
        begin
          Step;
        end;
        case Tok.Word of
          pkwPrivate:
            begin
              AVisibility := psavPrivate;

              if LStrict then
              begin
                AVisibility := psavStrictPrivate;
              end;
            end;
          pkwProtected:
            begin
              AVisibility := psavProtected;

              if LStrict then
              begin
                AVisibility := psavStrictProtected;
              end;
            end;
          pkwPublic:
            begin
              AVisibility := psavPublic;
            end;
          pkwPublished:
            begin
              AVisibility := psavPublished;
            end;
          pkwAutomated:
            begin
              AVisibility := psavAutomated;
            end;
        else
          begin
            Problem(psiExpectedName, 'Strict visibility needs private or protected', Tok.Span);
          end;
        end;
        Step;
        LSection := pkwNone;
      end
      else if Tok.Word in [pkwPrivate, pkwProtected, pkwPublic, pkwPublished,
        pkwAutomated, pkwStrict] then
      begin
        Unsupported(AScope, 'Visibility sections require an enclosing member scope', Tok.Span);
        Step;
        LSection := pkwNone;
      end
      else if Word(pkwProperty) then
      begin
        PropertyDeclaration(AScope, AVisibility);
        LSection := pkwNone;
      end
      else if Tok.Word in [pkwProcedure, pkwFunction, pkwOperator, pkwConstructor, pkwDestructor,
        pkwClass, pkwGeneric] then
      begin
        { FPC's generic class methods use both prefixes, in that order. Consume
          each once; repeated/reversed prefixes retain the unsupported path. }
        LGeneric := Word(pkwGeneric);

        if LGeneric then
        begin
          Step;
        end;
        LClass := Word(pkwClass);

        if LClass then
        begin
          Step;
        end;

        if Word(pkwVar) and LClass and not LGeneric and AMembers then
        begin
          Step;
          Variables(AScope, psdkField, AVisibility, True);
          { The whole section owns this modifier, not just its first name group.
            Visibility, another section or a routine ends the section normally. }
          LSection := pkwClass;
          Continue;
        end
        else if Word(pkwProperty) and LClass and not LGeneric and AMembers then
        begin
          PropertyDeclaration(AScope, AVisibility, True);
        end
        else if Tok.Word in [pkwProcedure, pkwFunction, pkwOperator, pkwConstructor, pkwDestructor] then
        begin
          Routine(AScope, AVisibility, LClass);
        end
        else
        begin
          Unsupported(AScope, 'This class/generic declaration needs richer context', Tok.Span);
          Expression(psoSemicolon);
          Separator;
        end;
        LSection := pkwNone;
      end
      else if Sym(psoOpenBracket) then
      begin
        Groups(psoOpenBracket, psoCloseBracket);
      end
      else if AMembers and Word(pkwCase) then
      begin

        if (FScopes[AScope].OwnerDeclaration >= 0) and
          (FDeclarations[FScopes[AScope].OwnerDeclaration].TypeForm = pstfRecord) then
        begin
          VariantPart(AScope, AVisibility);
        end
        else
        begin
          Unsupported(AScope, 'Variant parts require their enclosing record context', Tok.Span);
          Expression(psoSemicolon);
          Separator;
        end;
      end
      else if Tok.Kind = ptkWord then
      begin
        case LSection of
          pkwClass:
            begin
              Variables(AScope, psdkField, AVisibility, True);
            end;
          pkwVar, pkwThreadvar:
            begin
              Variables(AScope, psdkVariable, AVisibility);
            end;
          pkwConst:
            begin
              Constants(AScope, psdkConstant, AVisibility);
            end;
          pkwResourcestring:
            begin
              Constants(AScope, psdkResourceString, AVisibility);
            end;
        else

          if AMembers then
          begin
            Variables(AScope, psdkField, AVisibility);
          end
          else
          begin
            Unsupported(AScope, 'This declaration is not yet structurally classified', Tok.Span);
            Expression(psoSemicolon);
            Separator;
          end;
        end;
      end
      else
      begin
        Unsupported(AScope, 'Unrecognized declaration syntax is retained', Tok.Span);
        Step;
      end;

      if FPosition = LBefore then
      begin
        Step;
      end;
    end;
  finally
    Leave;
  end;
end;

function TSyntaxSnapshot.Block(AScope: Integer): Boolean;
var
  LScope: Integer;
  LStart: Integer;
  LDepth: Integer;
  LRepeat: Integer;
begin
  Enter;
  try
    LStart := Tok.Span.Start;
    LScope := AddScope(psskBlock, AScope, -1, LStart, '');
    Step;
    LDepth := 1;
    LRepeat := 0;
    while not EOF and (LDepth > 0) do
    begin

      if Tok.Word in [pkwBegin, pkwTry, pkwCase, pkwAsm] then
      begin
        Inc(LDepth);

        if LDepth > FOptions.MaximumDepth then
        begin
          raise ENyxPascalSyntaxLimit.Create('Pascal body nesting exceeds its budget');
        end;
      end
      else if Word(pkwRepeat) then
      begin
        Inc(LRepeat);
      end
      else if Word(pkwUntil) then
      begin

        if LRepeat > 0 then
        begin
          Dec(LRepeat);
        end;
      end
      else if Word(pkwEnd) then
      begin
        Dec(LDepth);
      end
      else if Tok.Word in [pkwProcedure, pkwFunction] then
      begin
        Unsupported(LScope, 'Anonymous routine scopes need expression-context analysis', Tok.Span);
      end
      else if Tok.Word in [pkwVar, pkwConst, pkwType] then
      begin
        Unsupported(LScope, 'Inline declarations need statement-scope analysis', Tok.Span);
      end;
      Step;
    end;
    FinishScope(LScope, (LDepth = 0) and (LRepeat = 0));
    Result := FScopes[LScope].Complete;

    if not FScopes[LScope].Complete then
    begin
      Problem(psiUnclosedScope, 'Finish this Pascal body', Tok.Span);
    end;
  finally
    Leave;
  end;
end;

function DirectiveWord(const AText: TNyxText): TNyxText;
var
  LAt: Integer;
  LStart: Integer;
begin
  LAt := 1;
  while (LAt <= Length(AText)) and (AText[LAt] <> '$') do
  begin
    Inc(LAt);
  end;
  Inc(LAt);
  LStart := LAt;
  while (LAt <= Length(AText)) and (AText[LAt] in ['A'..'Z', 'a'..'z']) do
  begin
    Inc(LAt);
  end;
  Result := NyxPascalASCIIKey(Copy(AText, LStart, LAt - LStart));

  if (Result = 'i') and (LAt <= Length(AText)) and (AText[LAt] in ['+', '-']) then
  begin
    Result := 'iochecks';
  end;
end;

procedure TSyntaxSnapshot.BuildTokens;
var
  LAt: Integer;
  LCount: Integer;
  LDepth: Integer;
  LDirective: TNyxText;
  LToken: TNyxPascalToken;
  LConditionalReported: Boolean;
  LIncludeReported: Boolean;
begin
  SetLength(FTokens, FLexical.Count);
  SetLength(FConditional, FLexical.Count);
  LCount := 0;
  LDepth := 0;
  LConditionalReported := False;
  LIncludeReported := False;
  for LAt := 0 to FLexical.Count - 1 do
  begin

    if (LAt mod 1024 = 0) and (FCancellation <> nil) and FCancellation.Cancelled then
    begin
      raise ENyxPascalAnalysisCancelled.Create('Pascal syntax analysis cancelled');
    end;
    LToken := FLexical.Token(LAt);

    if LToken.Kind = ptkDirective then
    begin
      LDirective := DirectiveWord(FLexical.Text(LAt));

      if (LDirective = 'ifdef') or (LDirective = 'ifndef') or (LDirective = 'if') or
        (LDirective = 'ifopt') then
      begin
        Inc(LDepth);

        if LDepth > FOptions.MaximumDepth then
        begin
          raise ENyxPascalSyntaxLimit.Create('Pascal conditional syntax exceeds its budget');
        end;

        if not LConditionalReported then
        begin
          Problem(psiConditionalContextRequired,
            'Conditional declarations require an explicit compiler context', LToken.Span);
          LConditionalReported := True;
        end;
      end
      else if (LDirective = 'endif') or (LDirective = 'ifend') then
      begin

        if LDepth > 0 then
        begin
          Dec(LDepth);
        end
        else
        begin
          Problem(psiUnterminatedConditional, 'Unmatched conditional terminator', LToken.Span);
        end;
      end
      else if ((LDirective = 'include') or (LDirective = 'i')) and not LIncludeReported then
      begin
        Problem(psiIncludeContextRequired,
          'Captured include expansion is required before binding this unit', LToken.Span);
        LIncludeReported := True;
      end;
    end
    else if not (LToken.Kind in [ptkWhitespace, ptkComment]) then
    begin
      FTokens[LCount] := LAt;
      FConditional[LCount] := LDepth > 0;
      Inc(LCount);
    end;
  end;

  if LDepth > 0 then
  begin
    Problem(psiUnterminatedConditional, 'Finish this conditional compilation block',
      FLexical.Token(FLexical.Count - 1).Span);
  end;
  SetLength(FTokens, LCount);
  SetLength(FConditional, LCount);
end;

procedure TSyntaxSnapshot.Build(APrevious: TSyntaxSnapshot; ARetained: Integer);
var
  LResume: TParserCheckpoint;
begin
  BuildTokens;
  FFilterDiagnostics := FDiagnosticCount;
  FReadThrough := -1;

  if (APrevious <> nil) and APrevious.FCheckpoint.Valid and
    (APrevious.FCheckpoint.ReadThrough < ARetained) then
  begin
    Restore(APrevious);
    LResume := FCheckpoint;
    case LResume.Resume of
      prDeclarations:
        begin
          FDepth := LResume.Depth - 1;
          Declarations(0, LResume.Visibility, LResume.Members, LResume.Section);
        end;
      prTypes:
        begin
          FDepth := LResume.Depth;
          TypeSection(0, LResume.Visibility);
          FDepth := LResume.Depth - 1;
          Declarations(0, LResume.Visibility, LResume.Members);
        end;
      prTypesFragment:
        begin
          FDepth := LResume.Depth;
          TypeSection(0, LResume.Visibility);
        end;
    end;
  end
  else
  begin
    ParseRoot;
  end;
  FinishRoot;
end;

procedure TSyntaxSnapshot.ParseRoot;
var
  LModule: TNyxText;
  LModuleScope: Integer;
begin

  if FOptions.StartingContext = psfMembers then
  begin
    LModuleScope := AddScope(psskMembers, -1, -1, 0, '');
  end
  else
  begin
    LModuleScope := AddScope(psskModule, -1, -1, 0, '');
  end;

  if (FOptions.StartingContext = psfModule) and
    (Tok.Word in [pkwUnit, pkwProgram, pkwLibrary, pkwPackage]) then
  begin
    Step;
    LModule := Name;
    Step;
    while Sym(psoDot) and (Tok(1).Kind = ptkWord) do
    begin
      Step;
      LModule := LModule + '.' + Name;
      Step;
    end;
    FScopes[LModuleScope].FName := LModule;

    if Sym(psoOpenParen) then
    begin
      Groups(psoOpenParen, psoCloseParen);
    end;
    Separator;
  end;

  if FOptions.StartingContext = psfTypes then
  begin
    TypeSection(0, psavInterface);
  end
  else if FOptions.StartingContext = psfInterfaceDeclarations then
  begin
    Declarations(0, psavInterface, False);
  end
  else
  begin

    if FOptions.StartingContext = psfMembers then
    begin
      Declarations(0, psavUnspecified, True);
    end
    else
    begin
      Declarations(0, psavImplementation, False);
    end;
  end;
end;

procedure TSyntaxSnapshot.FinishRoot;
var
  LStart: Integer;
  LBodyClosed: Boolean;
begin
  LBodyClosed := False;

  if Word(pkwBegin) or Word(pkwAsm) then
  begin
    LBodyClosed := Block(0);
  end
  else if Word(pkwInitialization) or Word(pkwFinalization) then
  begin
    { Initialization/finalization statements remain opaque until statement/expression
      scopes are modeled; they must not become fake unit declarations. }
    LStart := Tok.Span.Start;
    Unsupported(0, 'Initialization/finalization bodies need statement analysis',
      NyxPascalSpan(LStart, FLexical.Scalars - LStart));
  end;
  { A physical fragment closes at EOF. A complete module needs its final end-dot
    boundary; skipped initialization statements cannot grant closed structure. }

  if FOptions.StartingContext <> psfModule then
  begin
    FScopes[0].FComplete := EOF;
  end
  else
  begin
    FScopes[0].FComplete :=
      (Word(pkwEnd) and Sym(psoDot, 1) and (Tok(2).Kind = ptkEOF)) or
      (LBodyClosed and Sym(psoDot) and (Tok(1).Kind = ptkEOF));
  end;
end;

function TSyntaxSnapshot.Lexical: INyxPascalLexicalSnapshot;
begin
  Result := FLexical;
end;

function TSyntaxSnapshot.ScopeCount: Integer;
begin
  Result := FScopeCount;
end;

function TSyntaxSnapshot.Scope(AIndex: Integer): TNyxPascalSyntaxScope;
begin

  if (AIndex < 0) or (AIndex >= FScopeCount) then
  begin
    raise ENyxPascalLexical.Create('Pascal scope index is outside its syntax snapshot');
  end;
  Result := FScopes[AIndex];
end;

function TSyntaxSnapshot.ScopeAt(AOffset: Integer): Integer;
var
  LAt: Integer;
  LDepth: Integer;
  LBestDepth: Integer;
  LParent: Integer;
begin

  if (AOffset < 0) or (AOffset > FLexical.Scalars) then
  begin
    raise ENyxPascalLexical.Create('Pascal scope position is outside its syntax snapshot');
  end;
  Result := 0;
  LBestDepth := 0;
  for LAt := 1 to FScopeCount - 1 do
  begin

    if (AOffset >= FScopes[LAt].Span.Start) and
      ((AOffset < FScopes[LAt].Span.Finish) or
        ((AOffset = FLexical.Scalars) and not FScopes[LAt].Complete)) then
    begin
      LDepth := 0;
      LParent := LAt;
      while LParent >= 0 do
      begin
        Inc(LDepth);
        LParent := FScopes[LParent].Parent;
      end;

      if LDepth > LBestDepth then
      begin
        Result := LAt;
        LBestDepth := LDepth;
      end;
    end;
  end;
end;

function TSyntaxSnapshot.DeclarationCount: Integer;
begin
  Result := FDeclarationCount;
end;

function TSyntaxSnapshot.Declaration(AIndex: Integer): TNyxPascalSyntaxDeclaration;
begin

  if (AIndex < 0) or (AIndex >= FDeclarationCount) then
  begin
    raise ENyxPascalLexical.Create('Pascal declaration index is outside its syntax snapshot');
  end;
  Result := FDeclarations[AIndex];
end;

function TSyntaxSnapshot.Named(AScope: Integer; const AName: TNyxText): Integer;
begin
  Scope(AScope);
  Result := FNames.IndexOf(TNyxText(IntToStr(AScope)) + ':' + NyxPascalASCIIKey(AName));

  if (Result >= 0) and (FDeclarations[Result].OperatorKind <> pokNone) then
  begin
    Result := -1;
  end;
end;

function TSyntaxSnapshot.NamedOperator(AScope: Integer; AKind: TNyxPascalOperatorKind): Integer;
begin
  Scope(AScope);

  if (Ord(AKind) <= Ord(pokUnknown)) or (Ord(AKind) > Ord(High(TNyxPascalOperatorKind))) then
  begin
    raise ENyxPascalLexical.Create('Select one supported typed operator family');
  end;
  Result := FNames.IndexOf(TNyxText(IntToStr(AScope)) + ':operator#' + TNyxText(IntToStr(Ord(AKind))));
end;

function TSyntaxSnapshot.SelectorCount: Integer;
begin
  Result := FSelectorCount;
end;

function TSyntaxSnapshot.SelectorAt(AIndex: Integer): TNyxPascalVariantSelector;
begin

  if (AIndex < 0) or (AIndex >= FSelectorCount) then
  begin
    raise ENyxPascalLexical.Create('Variant selector index is outside its syntax snapshot');
  end;
  Result := FSelectors[AIndex];
end;

function TSyntaxSnapshot.ArmCount: Integer;
begin
  Result := FArmCount;
end;

function TSyntaxSnapshot.ArmAt(AIndex: Integer): TNyxPascalVariantArm;
begin

  if (AIndex < 0) or (AIndex >= FArmCount) then
  begin
    raise ENyxPascalLexical.Create('Variant arm index is outside its syntax snapshot');
  end;
  Result := FArms[AIndex];
end;

function TSyntaxSnapshot.LabelCount: Integer;
begin
  Result := FLabelCount;
end;

function TSyntaxSnapshot.LabelAt(AIndex: Integer): TNyxPascalVariantLabel;
begin

  if (AIndex < 0) or (AIndex >= FLabelCount) then
  begin
    raise ENyxPascalLexical.Create('Variant label index is outside its syntax snapshot');
  end;
  Result := FLabels[AIndex];
end;

function TSyntaxSnapshot.StatementLabelCount: Integer;
begin
  Result := FStatementLabelCount;
end;

function TSyntaxSnapshot.StatementLabelAt(AIndex: Integer): TNyxPascalStatementLabel;
begin

  if (AIndex < 0) or (AIndex >= FStatementLabelCount) then
  begin
    raise ENyxPascalLexical.Create('Statement label index is outside its syntax snapshot');
  end;
  Result := FStatementLabels[AIndex];
end;

function TSyntaxSnapshot.HintCount: Integer;
begin
  Result := FHintCount;
end;

function TSyntaxSnapshot.HintAt(AIndex: Integer): TNyxPascalSyntaxHint;
begin

  if (AIndex < 0) or (AIndex >= FHintCount) then
  begin
    raise ENyxPascalLexical.Create('Declaration hint index is outside its syntax snapshot');
  end;
  Result := FHints[AIndex];
end;

function TSyntaxSnapshot.IntrinsicCount: Integer;
begin
  Result := FIntrinsicCount;
end;

function TSyntaxSnapshot.IntrinsicAt(AIndex: Integer): TNyxPascalIntrinsicDeclaration;
begin

  if (AIndex < 0) or (AIndex >= FIntrinsicCount) then
  begin
    raise ENyxPascalLexical.Create('Intrinsic entry index is outside its syntax snapshot');
  end;
  Result := FIntrinsics[AIndex];
end;

function TSyntaxSnapshot.TypeAt(AStart: Integer): Integer;
begin

  if (AStart < 0) or (AStart > FLexical.Scalars) then
  begin
    raise ENyxPascalLexical.Create('Structured type position is outside its syntax snapshot');
  end;
  Result := FStructures.IndexOf(TNyxText(IntToStr(AStart)));
end;

function TSyntaxSnapshot.ImportCount: Integer;
begin
  Result := FImportCount;
end;

function TSyntaxSnapshot.ImportAt(AIndex: Integer): TNyxPascalSyntaxImport;
begin

  if (AIndex < 0) or (AIndex >= FImportCount) then
  begin
    raise ENyxPascalLexical.Create('Pascal import index is outside its syntax snapshot');
  end;
  Result := FImports[AIndex];
end;

function TSyntaxSnapshot.DiagnosticCount: Integer;
begin
  Result := FDiagnosticCount;
end;

function TSyntaxSnapshot.Diagnostic(AIndex: Integer): TNyxPascalSyntaxDiagnostic;
begin

  if (AIndex < 0) or (AIndex >= FDiagnosticCount) then
  begin
    raise ENyxPascalLexical.Create('Pascal syntax diagnostic index is outside its snapshot');
  end;
  Result := FDiagnostics[AIndex];
end;

function NewNyxPascalSyntaxSnapshot(const ASource: TNyxText): INyxPascalSyntaxSnapshot;
begin
  Result := NewNyxPascalSyntaxSnapshot(ASource, TNyxPascalSyntaxOptions.Defaults);
end;

function NewNyxPascalSyntaxSnapshot(const ASource: TNyxText;
  const AOptions: TNyxPascalSyntaxOptions;
  const ACancellation: INyxCancellationScope): INyxPascalSyntaxSnapshot;
begin
  Result := TSyntaxSnapshot.Create(ASource, AOptions, ACancellation);
end;

function NewNyxPascalSyntaxFromLexical(const ALexical: INyxPascalLexicalSnapshot;
  const AOptions: TNyxPascalSyntaxOptions;
  const ACancellation: INyxCancellationScope): INyxPascalSyntaxSnapshot;
begin
  Result := TSyntaxSnapshot.CreateLexical(ALexical, AOptions, ACancellation);
end;

function SamePolicy(const ALeft, ARight: TNyxPascalSyntaxOptions): Boolean;
var
  LLeft: TNyxPascalLexicalOptions;
  LRight: TNyxPascalLexicalOptions;
begin
  LLeft := ALeft.LexicalOptions;
  LRight := ARight.LexicalOptions;
  Result := (ALeft.StartingContext = ARight.StartingContext) and
    (ALeft.MaximumDeclarations = ARight.MaximumDeclarations) and
    (ALeft.MaximumScopes = ARight.MaximumScopes) and
    (ALeft.MaximumDepth = ARight.MaximumDepth) and
    (ALeft.MaximumDiagnostics = ARight.MaximumDiagnostics) and
    (LLeft.MaximumSourceBytes = LRight.MaximumSourceBytes) and
    (LLeft.MaximumTokens = LRight.MaximumTokens) and
    (LLeft.MaximumDiagnostics = LRight.MaximumDiagnostics) and
    (LLeft.MaximumCommentDepth = LRight.MaximumCommentDepth) and
    (LLeft.AllowNestedComments = LRight.AllowNestedComments) and
    (LLeft.AllowUnicodeIdentifiers = LRight.AllowUnicodeIdentifiers);
end;

function AppendNyxPascalSyntax(const APrevious: INyxPascalSyntaxSnapshot;
  const ALexical: INyxPascalLexicalSnapshot; const AOptions: TNyxPascalSyntaxOptions;
  const ACancellation: INyxCancellationScope): INyxPascalSyntaxSnapshot;
var
  LPrevious: ISyntaxOwner;
  LRetained: Integer;
begin

  if (ACancellation <> nil) and ACancellation.Cancelled then
  begin
    raise ENyxPascalAnalysisCancelled.Create('Syntax continuation cancelled');
  end;
  AOptions.Validate;

  if not Supports(APrevious, ISyntaxOwner, LPrevious) or (ALexical = nil) then
  begin
    raise ENyxPascalLexical.Create('Syntax continuation needs its owned predecessor and lexical source');
  end;
  LRetained := NyxPascalRetainedLexicalTokens(APrevious.Lexical, ALexical);

  if (LRetained < 0) or not SamePolicy(LPrevious.Snapshot.FOptions, AOptions) then
  begin
    raise ENyxPascalLexical.Create('Syntax continuation needs exact lexical lineage and identical options');
  end;

  if ALexical = APrevious.Lexical then
  begin
    Exit(APrevious);
  end;
  Result := TSyntaxSnapshot.CreateLexical(ALexical, AOptions, ACancellation,
    LPrevious.Snapshot, LRetained);
end;

function TSyntaxSnapshot.Snapshot: TSyntaxSnapshot;
begin
  Result := Self;
end;

function TSyntaxSnapshot.ReusedDeclarations: Integer;
begin
  Result := FReused;
end;

function TSyntaxSnapshot.CopiedEntries: Integer;
begin
  Result := FCopied;
end;

function TSyntaxSnapshot.FilteredTokens: Integer;
begin
  Result := FLexical.Count;
end;

function TSyntaxSnapshot.ParserTokenReads: Integer;
begin
  Result := FParserReads;
end;

function TSyntaxSnapshot.RestartToken: Integer;
begin
  Result := FRestart;
end;

end.
