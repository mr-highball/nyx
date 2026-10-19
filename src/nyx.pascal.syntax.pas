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

uses SysUtils, nyx.text, nyx.scheduler, nyx.pascal.lexing, nyx.pascal.conventions;

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
    inline declarations and anonymous scopes explicitly require richer analysis.
    Unit lifecycle scopes retain distinct executable regions, never declarations
    or inferred runtime ordering/effects. Existing scope ordinals stay stable. }
  TNyxPascalSyntaxScopeKind = (psskModule, psskMembers, psskRoutine, psskBlock,
    psskInitialization, psskFinalization, psskPropertyParameters);
  { Declared intent only. A typed variable/property preserves its type expression;
    a routine preserves its complete signature, result, parameters and owner name. }
  TNyxPascalSyntaxDeclarationKind = (psdkType, psdkConstant, psdkVariable,
    psdkField, psdkParameter, psdkProperty, psdkRoutine, psdkEnumValue, psdkResourceString,
    psdkLabel);
  { The authored routine keyword, independent of its identifier or result type.
    None belongs to non-routines; constructors/destructors must not acquire a
    function result merely because their implementation has an instance Self. }
  TNyxPascalRoutineKind = (prkNone, prkProcedure, prkFunction, prkConstructor,
    prkDestructor, prkOperator);
  { Authored property accessors, independent of getter/setter implementation or
    compiler admission. An inherited property redeclaration may omit both. }
  TNyxPascalPropertyAccess = (pspaRead, pspaWrite);
  { Value-owned accessor capabilities; no callable or interface is retained. }
  TNyxPascalPropertyAccesses = set of TNyxPascalPropertyAccess;
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
  { Declaration-facing alias for the shared authored-request value. Distinct
    source names below avoid shadowing the compiler-context policy enum. The
    original procedural-type RTTI names/ordinals remain owned by conventions. }
  TNyxPascalRoutineConvention = nyx.pascal.conventions.TNyxPascalCallingConvention;

const
  { Compile-time names for declaration callers. There is one enum value owner;
    these names introduce no runtime state or compiler default. }
  prcDefault = nyx.pascal.conventions.pccDefault;
  prcRegister = nyx.pascal.conventions.pccRegister;
  prcCdecl = nyx.pascal.conventions.pccCdecl;
  prcStdcall = nyx.pascal.conventions.pccStdcall;
  prcPascal = nyx.pascal.conventions.pccPascal;
  prcSafecall = nyx.pascal.conventions.pccSafecall;
  prcSyscall = nyx.pascal.conventions.pccSyscall;
  prcVectorcall = nyx.pascal.conventions.pccVectorcall;
  prcWinapi = nyx.pascal.conventions.pccWinapi;

type
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
  { Explicit uncertainty and declaration modifiers. ThreadLocal retains authored
    storage duration independently of ClassMember, not an inferred TLS address
    or synchronization guarantee. Static preserves the declared absence of an
    implicit receiver; an implementation need not repeat that suffix.
    Unsupported/recovered scopes grant no
    successful type inference, compilation or source editing authority. }
  TNyxPascalSyntaxFlag = (psdfIncomplete, psdfConditional, psdfForward,
    psdfClassMember, psdfOverload, psdfUnsupported, psdfThreadLocal, psdfStatic);
  { Value-owned flag set with no mutable storage or target-specific state. }
  TNyxPascalSyntaxFlags = set of TNyxPascalSyntaxFlag;
  { Authored platform/linker intent, distinct from Pascal member visibility.
    Public symbols do not turn subsequent unit declarations into class members.
    These flags describe source syntax, not linker/ABI admission or resolution
    of an alias, library expression or compiler intrinsic. RTL procedure intent
    does not supply the compiler's special parameter-matching rules. Non-returning
    intent preserves a body; it does not prove the body's control-flow effects.
    I/O-check intent preserves caller-side checking requests without resolving
    the caller's policy, compiler eligibility or an actual error outcome. }
  TNyxPascalLinkageFlag = (plfPublic, plfExport, plfExternal, plfAlias,
    plfCVariable, plfCompilerHelper, plfIntrinsic, plfPlatformAttributes,
    plfConstantIntrinsic, plfRTLProcedure, plfNoReturn, plfIOCheck);
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
    FRoutineKind: TNyxPascalRoutineKind;
    FPropertyParameters: TNyxPascalSpan;
    FPropertyAccess: TNyxPascalPropertyAccesses;
    FDefaultProperty: Boolean;
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
    FConvention: TNyxPascalRoutineConvention;
    FConventionSpan: TNyxPascalSpan;
  public
    property Name: TNyxText read FName;
    property OwnerName: TNyxText read FOwnerName;
    property Kind: TNyxPascalSyntaxDeclarationKind read FKind;
    property RoutineKind: TNyxPascalRoutineKind read FRoutineKind;
    { Exact bracketed declaration, not a default value/index directive. Its
      NestedScope contains ordered psdkParameter declarations, including groups. }
    property PropertyParameters: TNyxPascalSpan read FPropertyParameters;
    property PropertyAccess: TNyxPascalPropertyAccesses read FPropertyAccess;
    { Only the separate '; default;' member directive selects unnamed indexing. }
    property DefaultProperty: Boolean read FDefaultProperty;
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
    { First authored routine request and its exact keyword. Default/empty means
      absent, not an effective default. Conflicting requests retain Unsupported;
      consumers must respect declaration/scope uncertainty before inference. }
    property CallingConvention: TNyxPascalRoutineConvention read FConvention;
    property CallingConventionSpan: TNyxPascalSpan read FConventionSpan;
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
  { Construction-only work receipt retained as immutable values. Changed rows
    detach from checkpoint metadata; CopiedEntries includes those rows and
    diagnostic copies. FilteredTokens counts
    the complete lexical domain and ParserTokenReads includes actual lookahead.
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
  { Actual filter construction, separate from the logical domain above. Retained
    lexical tokens require no reread; complete significant-row blocks are shared,
    partial rows copied. Directories and diagnostics remain independent work.
    These receipts grant no authority or reduced aggregate charging. }
  INyxPascalSyntaxFilterWork = interface(IInterface)
    ['{AA2EFED7-6FBD-41E4-93B2-22D8ED2C7BC2}']
    function FilteredTokenReads: Integer;
    function ReusedFilterTokens: Integer;
    function SharedFilterRows: Integer;
    function CopiedFilterRows: Integer;
    function FilterDirectoryEntries: Integer;
  end;
  { Actual metadata/index construction. Retained values come from sealed
    checkpoint roots; copied values detach before mutation. Created values and
    allocated nodes (text tree, ordinal pages and compound roots) remain real
    work, independently of logical quotas.
    Diagnostic copying is separate. Counters grant no reuse/charging authority. }
  INyxPascalSyntaxMetadataWork = interface(IInterface)
    ['{C084FD4E-13F9-4E25-B9D8-E55D12961977}']
    function RetainedMetadataValues: Integer;
    function CopiedMetadataValues: Integer;
    function CreatedMetadataValues: Integer;
    function AllocatedMetadataNodes: Integer;
    function CopiedDiagnosticRows: Integer;
    function MetadataTreeHeight: Integer;
  end;
  { Immutable construction-access receipt. An unpublished parser alone may
    reuse at most eight exact typed ordinal/textual lookups, including absence.
    Ordinal misses use direct page lookup without a text key. Root writes
    refresh their typed key; COW stamps independently gate mutation.
    Publication drops all cached leases. Later/concurrent snapshot reads change
    none of these counters and retain zero cache entries. These values grant
    no reuse authority, discounted work, compiler meaning or latency guarantee. }
  INyxPascalSyntaxMetadataLookupWork = interface(IInterface)
    ['{0E819411-4909-4447-B541-28AEFB824C72}']
    function MetadataLookups: Integer;
    function MetadataLookupHits: Integer;
    function MetadataLookupTreeReads: Integer;
    function MetadataLookupPeak: Integer;
    function RetainedMetadataLookups: Integer;
  end;
  { Detached construction receipt for typed ordinal rows. Page nodes have four
    fixed slots; true text indexes retain their own balanced tree. Counts grant
    neither reuse authority nor compiler/latency meaning. Published reads change
    none of them, and the syntax retains no constructor/execution borrow. }
  INyxPascalSyntaxOrdinalMetadataWork = interface(IInterface)
    ['{ED7A4903-C142-44F6-9E47-AD3191F0B572}']
    function OrdinalMetadataLookups: Integer;
    function OrdinalMetadataPageReads: Integer;
    function AllocatedOrdinalPages: Integer;
    function CapturedMetadataRoots: Integer;
    function OrdinalMetadataDepth: Integer;
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
{ Closed source-spelling lookup shared by declaration and type readers. Callers
  separately enforce lexical kind/escaping/context. Unknown names return false
  with Default, never a guessed platform alias or compiler default. }
function NyxPascalCallingConventionNamed(const AName: TNyxText;
  out AConvention: TNyxPascalRoutineConvention): Boolean;

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
  checkpoint; provisional tails and diagnostics are parsed afresh. Filtering
  shares completed rows; metadata/index roots retain exact checkpoint identity.
  Alternate/foreign histories
  or changed options refuse; absent safe checkpoints fall back to a whole parse.
  Cancellation/failure publishes no candidate and never mutates APrevious. }
function AppendNyxPascalSyntax(const APrevious: INyxPascalSyntaxSnapshot;
  const ALexical: INyxPascalLexicalSnapshot; const AOptions: TNyxPascalSyntaxOptions;
  const ACancellation: INyxCancellationScope = nil): INyxPascalSyntaxSnapshot;

{ Opaque identity for an owned immutable metadata value, nil on an alternate
  snapshot. Owned ordinals are checked. A detached row receives a new identity;
  equal text/records confer none. Tokens retain no source, row or constructor
  and grant no visibility, editing, binding or arbitrary-source reuse authority. }
function NyxPascalDeclarationIdentity(const ASource: INyxPascalSyntaxSnapshot;
  AIndex: Integer): IInterface;
{ The same source-free proof for a scope row. Complete structured-body plans
  validate this separately from their declaring row; alternate owners yield nil. }
function NyxPascalScopeIdentity(const ASource: INyxPascalSyntaxSnapshot;
  AIndex: Integer): IInterface;

implementation

uses nyx.pascal.lexing.append, nyx.pascal.work;

const
  { Bounded private partial copying; the directory stays flat across appends. }
  CFilterBlockRows = 256;
  { One bounded probe/update operation covers at most these exact key slots.
    Entries exist only in the unpublished metadata constructor. }
  CMetadataReadSlots = 8;
  { Two ordinal bits select one of four fixed slots. A page clone copies at
    most four interface leases as one admitted metadata-node operation. }
  CMetadataPageSlots = 4;

type
  { Source-independent ordinal vector. Temporary lists stage grouped declarations
    without putting interfaces or shared mutable arrays in published records. }
  TOrdinals = array of Integer;
  { Closed namespaces separate row identities from authored source names. }
  TMetadataKind = (mkScope, mkDeclaration, mkImport, mkSelector, mkArm,
    mkVariantLabel, mkStatementLabel, mkHint, mkIntrinsic, mkName, mkStructure);
  { Borrow is usable only under its tree/constructor lease. Detach returns a
    private value; the ownership stamp contains no owner or history link. }
  IMetadataValue = interface(IInterface)
    function Borrow: TObject;
    function Stamp: IInterface;
    function Detach(const AStamp: IInterface): IMetadataValue;
  end;
  { Published values never edit. A later constructor must detach before acquiring
    its own write rights; ownership markers contain no parser/source reference. }
  TMetadataValue = class(TInterfacedObject, IMetadataValue)
  public
    OwnerStamp: IInterface;
    function Borrow: TObject;
    function Stamp: IInterface;
    function Detach(const AStamp: IInterface): IMetadataValue; virtual; abstract;
  end;
  { Open scope ends adapt at public read time without changing the retained row.
    Identity is a source-free opaque marker; detaching any row renews it. }
  TScopeValue = class(TMetadataValue)
  public
    Identity: IInterface;
    Value: TNyxPascalSyntaxScope;
    constructor Create;
    function Detach(const AStamp: IInterface): IMetadataValue; override;
  end;
  { A declaration detaches before later scope/overload writes. Its opaque marker
    renews on every detach, conservatively invalidating dependent type plans. }
  TDeclarationValue = class(TMetadataValue)
  public
    Identity: IInterface;
    Value: TNyxPascalSyntaxDeclaration;
    constructor Create;
    function Detach(const AStamp: IInterface): IMetadataValue; override;
  end;
  { Import entries retain exact local source identity, never a source-world. }
  TImportValue = class(TMetadataValue)
  public
    Value: TNyxPascalSyntaxImport;
    function Detach(const AStamp: IInterface): IMetadataValue; override;
  end;
  { Variant selector membership remains snapshot-local across checkpoint forks. }
  TSelectorValue = class(TMetadataValue)
  public
    Value: TNyxPascalVariantSelector;
    function Detach(const AStamp: IInterface): IMetadataValue; override;
  end;
  { Arm boundaries and label links detach before a constructing parser update. }
  TArmValue = class(TMetadataValue)
  public
    Value: TNyxPascalVariantArm;
    function Detach(const AStamp: IInterface): IMetadataValue; override;
  end;
  { Variant labels retain exact source spans and local next-label ordinals. }
  TVariantLabelValue = class(TMetadataValue)
  public
    Value: TNyxPascalVariantLabel;
    function Detach(const AStamp: IInterface): IMetadataValue; override;
  end;
  { Statement-label text is value-owned, with no binding/world reference. }
  TStatementLabelValue = class(TMetadataValue)
  public
    Value: TNyxPascalStatementLabel;
    function Detach(const AStamp: IInterface): IMetadataValue; override;
  end;
  { Hint source/next links survive independently of a suffix parse. }
  THintValue = class(TMetadataValue)
  public
    Value: TNyxPascalSyntaxHint;
    function Detach(const AStamp: IInterface): IMetadataValue; override;
  end;
  { Intrinsic annotations preserve authored syntax, never evaluated facts. }
  TIntrinsicValue = class(TMetadataValue)
  public
    Value: TNyxPascalIntrinsicDeclaration;
    function Detach(const AStamp: IInterface): IMetadataValue; override;
  end;
  { Name/structure indexes contain scalar ordinals, never mutable row objects. }
  TOrdinalValue = class(TMetadataValue)
  public
    Value: Integer;
    function Detach(const AStamp: IInterface): IMetadataValue; override;
  end;
  { Immutable AVL node. Children/value point outward; no parent, parser,
    snapshot or preceding root survives. Exact target-local text ordering never
    determines public declaration order, which remains in explicit ordinals. }
  IMetadataNode = interface(IInterface)
    function Key: TNyxText;
    function Value: IMetadataValue;
    function Left: IMetadataNode;
    function Right: IMetadataNode;
    function Height: Integer;
    function Count: Integer;
  end;
  { Fields initialize once through the private balanced-tree constructor. }
  TMetadataNode = class(TInterfacedObject, IMetadataNode)
  public
    KeyValue: TNyxText;
    Payload: IMetadataValue;
    LeftValue: IMetadataNode;
    RightValue: IMetadataNode;
    HeightValue: Integer;
    CountValue: Integer;
    function Key: TNyxText;
    function Value: IMetadataValue;
    function Left: IMetadataNode;
    function Right: IMetadataNode;
    function Height: Integer;
    function Count: Integer;
  end;
  { Persistent ordinal page. Level zero has four value slots; higher levels
    have four child slots. No source/parent/history or mutable directory escapes.
    The level selects two bits of a checked nonnegative snapshot-local ordinal. }
  IMetadataRowNode = interface(IInterface)
    function Level: Integer;
    function Value(AIndex: Integer): IMetadataValue;
    function Child(AIndex: Integer): IMetadataRowNode;
    function Count: Integer;
  end;
  { Initialized once before publication. Dynamic class arrays hold interface
    leases safely on both targets; a record/static array never holds them.
    Only the array matching LevelValue is allocated, always exactly four slots. }
  TMetadataRowNode = class(TInterfacedObject, IMetadataRowNode)
  public
    LevelValue: Integer;
    Values: array of IMetadataValue;
    Children: array of IMetadataRowNode;
    CountValue: Integer;
    function Level: Integer;
    function Value(AIndex: Integer): IMetadataValue;
    function Child(AIndex: Integer): IMetadataRowNode;
    function Count: Integer;
  end;
  { One immutable checkpoint contains the text root and all nine typed row
    roots. The bounded directory owns no preceding checkpoint/parser or stamp;
    payload COW stamps independently prevent later/sibling mutation. }
  IMetadataRoot = interface(IInterface)
    function Names: IMetadataNode;
    function Rows(AKind: TMetadataKind): IMetadataRowNode;
    function Count: Integer;
  end;
  { Filled once by Capture, including its checked total value count. Root
    directory references are copied explicitly under one bounded node admission. }
  TMetadataRoot = class(TInterfacedObject, IMetadataRoot)
  public
    NameRoot: IMetadataNode;
    RowRoots: array of IMetadataRowNode;
    CountValue: Integer;
    function Names: IMetadataNode;
    function Rows(AKind: TMetadataKind): IMetadataRowNode;
    function Count: Integer;
  end;
  { A parser's private text tree and typed ordinal directories. Bounded lookup
    entries live only during construction; ordinal access never encodes a string
    or searches that tree. Capture seals both root families and rotates the COW
    stamp. Later writes detach their value/path. Published readers mutate no
    cache/counter and retain no execution lease or source/owner chain. }
  TMetadataMap = class
  private
    FRoot: IMetadataNode;
    FRows: array of IMetadataRowNode;
    FStamp: IInterface;
    FCopied: Integer;
    FCreated: Integer;
    FNodes: Integer;
    { Construction-only outward admission lease, cleared before publication. }
    FExecution: INyxCancellationScope;
    FConstructing: Boolean;
    FReadKeys: array of TNyxText;
    FReadValues: array of IMetadataValue;
    { Negative ordinal tags identify text keys; nonnegative entries contain
      an exact row family/index. Both share the same original eight-slot cap. }
    FReadKinds: array of TMetadataKind;
    FReadOrdinals: array of Integer;
    FReadCount: Integer;
    FReadNext: Integer;
    FReadLookups: Integer;
    FReadHits: Integer;
    FReadTreeVisits: Integer;
    FReadPeak: Integer;
    FOrdinalLookups, FOrdinalReads, FOrdinalNodes, FCaptures: Integer;
    procedure RequireConstruction;
    function TryReadCache(const AKey: TNyxText; out AValue: IMetadataValue): Boolean;
    procedure RememberRead(const AKey: TNyxText; const AValue: IMetadataValue);
    procedure RememberRow(AKind: TMetadataKind; AIndex: Integer; const AValue: IMetadataValue);
    function FindRow(AKind: TMetadataKind; AIndex: Integer): IMetadataValue;
    procedure StoreRow(AKind: TMetadataKind; AIndex: Integer; const AValue: IMetadataValue);
    function NewRowNode(const APrevious: IMetadataRowNode; ALevel, ASlot: Integer;
      const AValue: IMetadataValue; const AChild: IMetadataRowNode): IMetadataRowNode;
    function InsertRow(const APrevious: IMetadataRowNode; ALevel, AIndex: Integer;
      const AValue: IMetadataValue): IMetadataRowNode;
    function NewNode(const AKey: TNyxText; const AValue: IMetadataValue;
      const ALeft, ARight: IMetadataNode): IMetadataNode;
    function Balance(const AKey: TNyxText; const AValue: IMetadataValue;
      const ALeft, ARight: IMetadataNode): IMetadataNode;
    function InsertFirst(const ANode: IMetadataNode; const AKey: TNyxText;
      AOrdinal: Integer; AReadLookup: Boolean; out AValue: IMetadataValue;
      out AInserted: Boolean): IMetadataNode;
  public
    constructor Create(const ARoot: IMetadataRoot;
      const AExecution: INyxCancellationScope);
    procedure RetireExecution;
    function Capture: IMetadataRoot;
    function Find(const AKey: TNyxText): IMetadataValue;
    function Edit(AKind: TMetadataKind; AIndex: Integer): TObject;
    function Row(AKind: TMetadataKind; AIndex: Integer): TObject;
    function Index(AKind: TMetadataKind; const AKey: TNyxText): Integer;
    { One bounded lookup/insertion, returning True only for an existing first
      ordinal. The candidate path stays private until admitted root application. }
    function FindOrAddFirst(AKind: TMetadataKind; const AKey: TNyxText;
      AValue: Integer; out AFirst: Integer): Boolean;
    procedure AddFirst(AKind: TMetadataKind; const AKey: TNyxText; AValue: Integer);
    function Height: Integer;
    function OrdinalDepth: Integer;
    property Copied: Integer read FCopied;
    property Created: Integer read FCreated;
    property AllocatedNodes: Integer read FNodes;
    property ReadLookups: Integer read FReadLookups;
    property ReadHits: Integer read FReadHits;
    property ReadTreeVisits: Integer read FReadTreeVisits;
    property ReadPeak: Integer read FReadPeak;
    property RetainedReads: Integer read FReadCount;
    property OrdinalLookups: Integer read FOrdinalLookups;
    property OrdinalReads: Integer read FOrdinalReads;
    property OrdinalNodes: Integer read FOrdinalNodes;
    property Captures: Integer read FCaptures;
  end;
  { Significant lexical ordinal and its raw conditional participation. No source,
    interface or mutable collection is retained in a row. }
  TFilterRow = record
    Token: Integer;
    Conditional: Boolean;
  end;
  { Immutable row block with no predecessor, source or parser back reference. }
  IFilterBlock = interface(IInterface)
    function Row(AIndex: Integer): TFilterRow;
  end;
  { Only an unpublished filter may fill this block. Sharing a complete block
    never grants the new builder permission to overwrite it. }
  TFilterBlock = class(TInterfacedObject, IFilterBlock)
  public
    Rows: array of TFilterRow;
    function Row(AIndex: Integer): TFilterRow;
  end;
  { Owns a flat immutable-block directory. The partial final block is detached
    during continuation; Seal retires the sole construction-only write borrow.
    Rows and directory counts remain bounded by the lexical token quota. }
  TFilteredTokens = class
  private
    FBlocks: array of IFilterBlock;
    FWriting: TFilterBlock;
    FCount: Integer;
    FShared: Integer;
    FCopied: Integer;
    function Token(AIndex: Integer): Integer;
    function Row(AIndex: Integer): TFilterRow;
  public
    constructor Create(APrevious: TFilteredTokens; ACount: Integer;
      const ACancellation: INyxCancellationScope);
    procedure Add(AToken: Integer; AConditional: Boolean);
    procedure Seal;
    function IsConditional(AIndex: Integer): Boolean;
    function DirectoryEntries: Integer;
    property Count: Integer read FCount;
    property Shared: Integer read FShared;
    property Copied: Integer read FCopied;
    property Tokens[AIndex: Integer]: Integer read Token; default;
  end;
  { Value-only filter state immediately before lexical tail repair. Diagnostics
    excludes the provisional EOF diagnostic; the parser's own suffix is separate.
    SignificantCount addresses immutable rows, never an earlier parser owner. }
  TFilterCheckpoint = record
    Lexical: Integer;
    SignificantCount: Integer;
    Depth: Integer;
    ConditionalReported: Boolean;
    IncludeReported: Boolean;
    Diagnostics: Integer;
  end;
  { Closed continuation positions in the root declaration grammar. A type
    section nested in root declarations must return to that caller's grammar;
    an explicit types fragment does not gain a declaration-section caller. }
  TParserResume = (prDeclarations, prTypes, prTypesFragment);
  { Declaration-section intent, distinct from lexical words. Class threadvars
    retain both type-member access and per-thread storage across every name
    group; ordinary threadvars never acquire a class owner. Checkpoints copy
    this closed value rather than reconstructing it from the previous token. }
  TDeclarationSection = (pdsNone, pdsVariables, pdsThreadVariables,
    pdsConstants, pdsResources, pdsClassVariables, pdsClassThreadVariables);
  { Value-only rollback point before a root declaration. Counts exclude the
    provisional suffix; its companion metadata root retains exact prior rows.
    ReadThrough includes every parser lookahead dependency, independently of
    Position. Unit/implementation authority resumes with the same declaration
    boundary. No interface, mutable vector or prior snapshot is retained. }
  TParserCheckpoint = record
    Valid: Boolean;
    Resume: TParserResume;
    Position: Integer;
    ReadThrough: Integer;
    Depth: Integer;
    PartialEqual: Boolean;
    ModuleUnit: Boolean;
    ImplementationSeen: Boolean;
    ActiveArm: Integer;
    Visibility: TNyxPascalSyntaxVisibility;
    Members: Boolean;
    Section: TDeclarationSection;
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
  { Construction-only delimiter frame for a unit executable region. Groups have
    no declaration scope; compound statements retain lexical block boundaries.
    Values own no interfaces, source buffers or mutable snapshot references. }
  TExecutableFrame = record
    OpenWord: TNyxPascalWord;
    CloseSymbol: TNyxPascalSymbol;
    Scope: Integer;
    ParentScope: Integer;
    HandlerSeen: Boolean;
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
    INyxPascalSyntaxWork, INyxPascalSyntaxFilterWork,
    INyxPascalSyntaxMetadataWork, INyxPascalSyntaxMetadataLookupWork,
    INyxPascalSyntaxOrdinalMetadataWork, ISyntaxOwner)
  private
    FLexical: INyxPascalLexicalSnapshot;
    FOptions: TNyxPascalSyntaxOptions;
    FCancellation: INyxCancellationScope;
    FTokens: TFilteredTokens;
    FFilterCheckpoint: TFilterCheckpoint;
    FFilteredReads: Integer;
    FFilterRetained: Integer;
    FMetadata: TMetadataMap;
    FCheckpointMetadata: IMetadataRoot;
    FRetainedMetadata: Integer;
    FDiagnostics: array of TNyxPascalSyntaxDiagnostic;
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
    FPosition: Integer;
    { A generic header may end in the physical >= token. Only its > belongs
      to the header; the residual equals sign stays for the alias declaration. }
    FPartialEqual: Boolean;
    FModuleUnit: Boolean;
    FImplementationSeen: Boolean;
    FDepth: Integer;
    FCheckpoint: TParserCheckpoint;
    FFilterDiagnostics: Integer;
    FReadThrough: Integer;
    FParserReads: Integer;
    FReused: Integer;
    FCopied: Integer;
    FRestart: Integer;
    { Read accessors borrow under FMetadata. Only construction calls Edit:
      each changed row detaches before a caller receives mutable field access. }
    function Frame(AIndex: Integer): TScopeValue;
    function EditFrame(AIndex: Integer): TScopeValue;
    function Entry(AIndex: Integer): TDeclarationValue;
    function EditEntry(AIndex: Integer): TDeclarationValue;
    function ImportRow(AIndex: Integer): TImportValue;
    function EditImportRow(AIndex: Integer): TImportValue;
    function SelectorRow(AIndex: Integer): TSelectorValue;
    function EditSelectorRow(AIndex: Integer): TSelectorValue;
    function ArmRow(AIndex: Integer): TArmValue;
    function EditArmRow(AIndex: Integer): TArmValue;
    function VariantLabelRow(AIndex: Integer): TVariantLabelValue;
    function EditVariantLabelRow(AIndex: Integer): TVariantLabelValue;
    function StatementLabelRow(AIndex: Integer): TStatementLabelValue;
    function EditStatementLabelRow(AIndex: Integer): TStatementLabelValue;
    function HintRow(AIndex: Integer): THintValue;
    function EditHintRow(AIndex: Integer): THintValue;
    function IntrinsicRow(AIndex: Integer): TIntrinsicValue;
    function EditIntrinsicRow(AIndex: Integer): TIntrinsicValue;
    procedure CheckCancellation;
    function ReadToken(AIndex: Integer): TNyxPascalToken;
    procedure Remember(AResume: TParserResume; AVisibility: TNyxPascalSyntaxVisibility;
      AMembers: Boolean; ASection: TDeclarationSection);
    procedure Restore(APrevious: TSyntaxSnapshot);
    procedure LinkDeclaration(AIndex: Integer);
    function Tok(AOffset: Integer = 0): TNyxPascalToken;
    function Word(AWord: TNyxPascalWord; AOffset: Integer = 0): Boolean;
    { Packing prefixes are contextual words; escaped identifiers stay names. }
    function Packing: Boolean;
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
    function RoutineConvention(out AConvention: TNyxPascalRoutineConvention): Boolean;
    procedure SetRoutineConvention(ADeclaration: Integer;
      AConvention: TNyxPascalRoutineConvention; const ASpan: TNyxPascalSpan);
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
      AArmEnd: Boolean = False; AThreadLocal: Boolean = False);
    procedure Constants(AScope: Integer; AKind: TNyxPascalSyntaxDeclarationKind;
      AVisibility: TNyxPascalSyntaxVisibility);
    function Parameters(AScope: Integer; AClose: TNyxPascalSymbol = psoCloseParen): Boolean;
    procedure Routine(AScope: Integer; AVisibility: TNyxPascalSyntaxVisibility;
      AClass: Boolean = False);
    { A class property retains its ordinary property kind/type plus explicit
      class-member intent; accessors remain authored syntax, not executed code. }
    procedure PropertyDeclaration(AScope: Integer; AVisibility: TNyxPascalSyntaxVisibility;
      AClass: Boolean = False);
    procedure Imports(AScope: Integer);
    procedure Declarations(AScope: Integer; AVisibility: TNyxPascalSyntaxVisibility;
      AMembers: Boolean; ASection: TDeclarationSection = pdsNone);
    function Block(AScope: Integer): Boolean;
    procedure BuildTokens(APrevious: TSyntaxSnapshot; ARetained: Integer);
    procedure Build(APrevious: TSyntaxSnapshot = nil; ARetained: Integer = -1);
    procedure ParseRoot;
    procedure FinishRoot;
    function ModuleSection(AKind: TNyxPascalSyntaxScopeKind): Boolean;
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
    function FilteredTokenReads: Integer;
    function ReusedFilterTokens: Integer;
    function SharedFilterRows: Integer;
    function CopiedFilterRows: Integer;
    function FilterDirectoryEntries: Integer;
    function RetainedMetadataValues: Integer;
    function CopiedMetadataValues: Integer;
    function CreatedMetadataValues: Integer;
    function AllocatedMetadataNodes: Integer;
    function CopiedDiagnosticRows: Integer;
    function MetadataTreeHeight: Integer;
    function MetadataLookups: Integer;
    function MetadataLookupHits: Integer;
    function MetadataLookupTreeReads: Integer;
    function MetadataLookupPeak: Integer;
    function RetainedMetadataLookups: Integer;
    function OrdinalMetadataLookups: Integer;
    function OrdinalMetadataPageReads: Integer;
    function AllocatedOrdinalPages: Integer;
    function CapturedMetadataRoots: Integer;
    function OrdinalMetadataDepth: Integer;
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

function TSyntaxSnapshot.Frame(AIndex: Integer): TScopeValue;
begin
  Result := TScopeValue(FMetadata.Row(mkScope, AIndex));
end;

function TSyntaxSnapshot.EditFrame(AIndex: Integer): TScopeValue;
begin
  CheckCancellation;
  Result := TScopeValue(FMetadata.Edit(mkScope, AIndex));
end;

function TSyntaxSnapshot.Entry(AIndex: Integer): TDeclarationValue;
begin
  Result := TDeclarationValue(FMetadata.Row(mkDeclaration, AIndex));
end;

function TSyntaxSnapshot.EditEntry(AIndex: Integer): TDeclarationValue;
begin
  CheckCancellation;
  Result := TDeclarationValue(FMetadata.Edit(mkDeclaration, AIndex));
end;

function TSyntaxSnapshot.ImportRow(AIndex: Integer): TImportValue;
begin
  Result := TImportValue(FMetadata.Row(mkImport, AIndex));
end;

function TSyntaxSnapshot.EditImportRow(AIndex: Integer): TImportValue;
begin
  CheckCancellation;
  Result := TImportValue(FMetadata.Edit(mkImport, AIndex));
end;

function TSyntaxSnapshot.SelectorRow(AIndex: Integer): TSelectorValue;
begin
  Result := TSelectorValue(FMetadata.Row(mkSelector, AIndex));
end;

function TSyntaxSnapshot.EditSelectorRow(AIndex: Integer): TSelectorValue;
begin
  CheckCancellation;
  Result := TSelectorValue(FMetadata.Edit(mkSelector, AIndex));
end;

function TSyntaxSnapshot.ArmRow(AIndex: Integer): TArmValue;
begin
  Result := TArmValue(FMetadata.Row(mkArm, AIndex));
end;

function TSyntaxSnapshot.EditArmRow(AIndex: Integer): TArmValue;
begin
  CheckCancellation;
  Result := TArmValue(FMetadata.Edit(mkArm, AIndex));
end;

function TSyntaxSnapshot.VariantLabelRow(AIndex: Integer): TVariantLabelValue;
begin
  Result := TVariantLabelValue(FMetadata.Row(mkVariantLabel, AIndex));
end;

function TSyntaxSnapshot.EditVariantLabelRow(AIndex: Integer): TVariantLabelValue;
begin
  CheckCancellation;
  Result := TVariantLabelValue(FMetadata.Edit(mkVariantLabel, AIndex));
end;

function TSyntaxSnapshot.StatementLabelRow(AIndex: Integer): TStatementLabelValue;
begin
  Result := TStatementLabelValue(FMetadata.Row(mkStatementLabel, AIndex));
end;

function TSyntaxSnapshot.EditStatementLabelRow(AIndex: Integer): TStatementLabelValue;
begin
  CheckCancellation;
  Result := TStatementLabelValue(FMetadata.Edit(mkStatementLabel, AIndex));
end;

function TSyntaxSnapshot.HintRow(AIndex: Integer): THintValue;
begin
  Result := THintValue(FMetadata.Row(mkHint, AIndex));
end;

function TSyntaxSnapshot.EditHintRow(AIndex: Integer): THintValue;
begin
  CheckCancellation;
  Result := THintValue(FMetadata.Edit(mkHint, AIndex));
end;

function TSyntaxSnapshot.IntrinsicRow(AIndex: Integer): TIntrinsicValue;
begin
  Result := TIntrinsicValue(FMetadata.Row(mkIntrinsic, AIndex));
end;

function TSyntaxSnapshot.EditIntrinsicRow(AIndex: Integer): TIntrinsicValue;
begin
  CheckCancellation;
  Result := TIntrinsicValue(FMetadata.Edit(mkIntrinsic, AIndex));
end;


function TMetadataValue.Borrow: TObject;
begin
  Result := Self;
end;

function TMetadataValue.Stamp: IInterface;
begin
  Result := OwnerStamp;
end;

constructor TScopeValue.Create;
begin
  inherited Create;
  Identity := TInterfacedObject.Create;
end;

constructor TDeclarationValue.Create;
begin
  inherited Create;
  Identity := TInterfacedObject.Create;
end;

function TScopeValue.Detach(const AStamp: IInterface): IMetadataValue;
var
  LCopy: TScopeValue;
begin
  LCopy := TScopeValue.Create;
  Result := LCopy;
  LCopy.OwnerStamp := AStamp;
  LCopy.Value := Value;
end;

function TDeclarationValue.Detach(const AStamp: IInterface): IMetadataValue;
var
  LCopy: TDeclarationValue;
begin
  LCopy := TDeclarationValue.Create;
  Result := LCopy;
  LCopy.OwnerStamp := AStamp;
  LCopy.Value := Value;
end;

function TImportValue.Detach(const AStamp: IInterface): IMetadataValue;
var
  LCopy: TImportValue;
begin
  LCopy := TImportValue.Create;
  Result := LCopy;
  LCopy.OwnerStamp := AStamp;
  LCopy.Value := Value;
end;

function TSelectorValue.Detach(const AStamp: IInterface): IMetadataValue;
var
  LCopy: TSelectorValue;
begin
  LCopy := TSelectorValue.Create;
  Result := LCopy;
  LCopy.OwnerStamp := AStamp;
  LCopy.Value := Value;
end;

function TArmValue.Detach(const AStamp: IInterface): IMetadataValue;
var
  LCopy: TArmValue;
begin
  LCopy := TArmValue.Create;
  Result := LCopy;
  LCopy.OwnerStamp := AStamp;
  LCopy.Value := Value;
end;

function TVariantLabelValue.Detach(const AStamp: IInterface): IMetadataValue;
var
  LCopy: TVariantLabelValue;
begin
  LCopy := TVariantLabelValue.Create;
  Result := LCopy;
  LCopy.OwnerStamp := AStamp;
  LCopy.Value := Value;
end;

function TStatementLabelValue.Detach(const AStamp: IInterface): IMetadataValue;
var
  LCopy: TStatementLabelValue;
begin
  LCopy := TStatementLabelValue.Create;
  Result := LCopy;
  LCopy.OwnerStamp := AStamp;
  LCopy.Value := Value;
end;

function THintValue.Detach(const AStamp: IInterface): IMetadataValue;
var
  LCopy: THintValue;
begin
  LCopy := THintValue.Create;
  Result := LCopy;
  LCopy.OwnerStamp := AStamp;
  LCopy.Value := Value;
end;

function TIntrinsicValue.Detach(const AStamp: IInterface): IMetadataValue;
var
  LCopy: TIntrinsicValue;
begin
  LCopy := TIntrinsicValue.Create;
  Result := LCopy;
  LCopy.OwnerStamp := AStamp;
  LCopy.Value := Value;
end;

function TOrdinalValue.Detach(const AStamp: IInterface): IMetadataValue;
var
  LCopy: TOrdinalValue;
begin
  LCopy := TOrdinalValue.Create;
  Result := LCopy;
  LCopy.OwnerStamp := AStamp;
  LCopy.Value := Value;
end;

function TMetadataNode.Key: TNyxText;
begin
  Result := KeyValue;
end;

function TMetadataNode.Value: IMetadataValue;
begin
  Result := Payload;
end;

function TMetadataNode.Left: IMetadataNode;
begin
  Result := LeftValue;
end;

function TMetadataNode.Right: IMetadataNode;
begin
  Result := RightValue;
end;

function TMetadataNode.Height: Integer;
begin
  Result := HeightValue;
end;

function TMetadataNode.Count: Integer;
begin
  Result := CountValue;
end;

function NodeHeight(const ANode: IMetadataNode): Integer;
begin
  Result := 0;

  if ANode <> nil then
  begin
    Result := ANode.Height;
  end;
end;

function NodeCount(const ANode: IMetadataNode): Integer;
begin
  Result := 0;

  if ANode <> nil then
  begin
    Result := ANode.Count;
  end;
end;

procedure RequireOrdinalKind(AKind: TMetadataKind);
begin

  if not (AKind in [mkScope..mkIntrinsic]) then
  begin
    raise ENyxPascalLexical.Create('A typed metadata row needs an ordinal family');
  end;
end;

function TMetadataRowNode.Level: Integer;
begin
  Result := LevelValue;
end;

function TMetadataRowNode.Value(AIndex: Integer): IMetadataValue;
begin

  if (LevelValue <> 0) or (AIndex < 0) or (AIndex >= Length(Values)) then
  begin
    raise ENyxPascalLexical.Create('Metadata value slot is outside its ordinal leaf');
  end;
  Result := Values[AIndex];
end;

function TMetadataRowNode.Child(AIndex: Integer): IMetadataRowNode;
begin

  if (LevelValue = 0) or (AIndex < 0) or (AIndex >= Length(Children)) then
  begin
    raise ENyxPascalLexical.Create('Metadata child slot is outside its ordinal branch');
  end;
  Result := Children[AIndex];
end;

function TMetadataRowNode.Count: Integer;
begin
  Result := CountValue;
end;

function TMetadataRoot.Names: IMetadataNode;
begin
  Result := NameRoot;
end;

function TMetadataRoot.Rows(AKind: TMetadataKind): IMetadataRowNode;
begin
  RequireOrdinalKind(AKind);
  Result := RowRoots[Ord(AKind)];
end;

function TMetadataRoot.Count: Integer;
begin
  Result := CountValue;
end;

constructor TMetadataMap.Create(const ARoot: IMetadataRoot;
  const AExecution: INyxCancellationScope);
var
  LAt: Integer;
begin
  inherited Create;
  FExecution := AExecution;
  NyxPascalAdmitAnalysis(FExecution, papMetadataValue);
  SetLength(FRows, Ord(mkIntrinsic) + 1);

  if ARoot <> nil then
  begin
    { This closed directory contains nine row roots plus the text root. It is
      one bounded node copy, independent of the number of retained rows. }
    NyxPascalAdmitAnalysis(FExecution, papMetadataNode);
    FRoot := ARoot.Names;
    for LAt := 0 to High(FRows) do
    begin
      FRows[LAt] := ARoot.Rows(TMetadataKind(LAt));
    end;
  end;
  FStamp := TInterfacedObject.Create;
  SetLength(FReadKeys, CMetadataReadSlots);
  SetLength(FReadValues, CMetadataReadSlots);
  SetLength(FReadKinds, CMetadataReadSlots);
  SetLength(FReadOrdinals, CMetadataReadSlots);
  FConstructing := True;
end;

procedure TMetadataMap.RetireExecution;
begin
  FConstructing := False;
  SetLength(FReadKeys, 0);
  SetLength(FReadValues, 0);
  SetLength(FReadKinds, 0);
  SetLength(FReadOrdinals, 0);
  FReadCount := 0;
  FReadNext := 0;
  FExecution := nil;
end;

procedure TMetadataMap.RequireConstruction;
begin

  if not FConstructing then
  begin
    raise ENyxPascalLexical.Create('Published metadata cannot acquire construction rights');
  end;
end;

{ Only construction calls this helper. The exact counter refuses overflow;
  published reads never advance access receipts or touch lookup slots. }
procedure IncrementMetadataAccess(var ACount: Integer);
begin

  if ACount = High(Integer) then
  begin
    raise ENyxPascalSyntaxLimit.Create('Metadata access exceeds its exact counter domain');
  end;
  Inc(ACount);
end;

procedure TMetadataMap.RememberRead(const AKey: TNyxText; const AValue: IMetadataValue);
var
  LAt: Integer;
begin
  RequireConstruction;
  { The caller already admitted its bounded lookup or root-application step.
    Updating a changed key also replaces a previously cached absence. Other
    cached values remain exact because a root insertion changes that key only. }
  for LAt := 0 to FReadCount - 1 do
  begin

    if (FReadOrdinals[LAt] < 0) and (FReadKeys[LAt] = AKey) then
    begin
      FReadValues[LAt] := AValue;
      Exit;
    end;
  end;
  FReadKeys[FReadNext] := AKey;
  FReadOrdinals[FReadNext] := -1;
  FReadValues[FReadNext] := AValue;
  FReadNext := (FReadNext + 1) mod CMetadataReadSlots;

  if FReadCount < CMetadataReadSlots then
  begin
    Inc(FReadCount);
    FReadPeak := FReadCount;
  end;
end;

procedure TMetadataMap.RememberRow(AKind: TMetadataKind; AIndex: Integer;
  const AValue: IMetadataValue);
var
  LAt: Integer;
begin
  RequireConstruction;
  RequireOrdinalKind(AKind);

  if AIndex < 0 then
  begin
    raise ENyxPascalLexical.Create('An ordinal lookup entry needs a nonnegative index');
  end;
  { Preserve the existing eight-slot construction optimization with typed keys.
    This is not an additional cache or lease; text and ordinal entries evict
    one another under the same bounded probe/root-application admission. }
  for LAt := 0 to FReadCount - 1 do
  begin

    if (FReadOrdinals[LAt] = AIndex) and (FReadKinds[LAt] = AKind) then
    begin
      FReadValues[LAt] := AValue;
      Exit;
    end;
  end;
  FReadKeys[FReadNext] := '';
  FReadKinds[FReadNext] := AKind;
  FReadOrdinals[FReadNext] := AIndex;
  FReadValues[FReadNext] := AValue;
  FReadNext := (FReadNext + 1) mod CMetadataReadSlots;

  if FReadCount < CMetadataReadSlots then
  begin
    Inc(FReadCount);
    FReadPeak := FReadCount;
  end;
end;

function TMetadataMap.Capture: IMetadataRoot;
var
  LRoot: TMetadataRoot;
  LAt, LCount: Integer;
begin
  RequireConstruction;
  NyxPascalAdmitAnalysis(FExecution, papMetadataValue);
  NyxPascalAdmitAnalysis(FExecution, papMetadataNode);
  IncrementMetadataAccess(FNodes);
  IncrementMetadataAccess(FCaptures);
  LRoot := TMetadataRoot.Create;
  Result := LRoot;
  LRoot.NameRoot := FRoot;
  LRoot.CountValue := NodeCount(FRoot);
  SetLength(LRoot.RowRoots, Length(FRows));
  for LAt := 0 to High(FRows) do
  begin
    LRoot.RowRoots[LAt] := FRows[LAt];
    LCount := 0;

    if FRows[LAt] <> nil then
    begin
      LCount := FRows[LAt].Count;
    end;

    if LCount > High(Integer) - LRoot.CountValue then
    begin
      raise ENyxPascalSyntaxLimit.Create('Metadata roots exceed their exact count domain');
    end;
    Inc(LRoot.CountValue, LCount);
  end;
  FStamp := TInterfacedObject.Create;
end;

function TMetadataMap.Height: Integer;
begin
  Result := NodeHeight(FRoot);

  if OrdinalDepth > Result then
  begin
    Result := OrdinalDepth;
  end;
end;

function TMetadataMap.OrdinalDepth: Integer;
var
  LAt: Integer;
begin
  Result := 0;
  for LAt := 0 to High(FRows) do
  begin

    if (FRows[LAt] <> nil) and (FRows[LAt].Level + 1 > Result) then
    begin
      Result := FRows[LAt].Level + 1;
    end;
  end;
end;

function TMetadataMap.NewRowNode(const APrevious: IMetadataRowNode;
  ALevel, ASlot: Integer; const AValue: IMetadataValue;
  const AChild: IMetadataRowNode): IMetadataRowNode;
var
  LNode: TMetadataRowNode;
  LAt, LCount: Integer;
begin
  { One page operation has exactly four slots. Admission precedes allocation,
    every copied reference and count calculation; no public getter defers it. }
  NyxPascalAdmitAnalysis(FExecution, papMetadataNode);
  IncrementMetadataAccess(FNodes);
  IncrementMetadataAccess(FOrdinalNodes);
  LNode := TMetadataRowNode.Create;
  Result := LNode;
  LNode.LevelValue := ALevel;

  if ALevel = 0 then
  begin
    SetLength(LNode.Values, CMetadataPageSlots);
    for LAt := 0 to CMetadataPageSlots - 1 do
    begin

      if APrevious <> nil then
      begin
        LNode.Values[LAt] := APrevious.Value(LAt);
      end;
    end;
    LNode.Values[ASlot] := AValue;
    for LAt := 0 to CMetadataPageSlots - 1 do
    begin

      if LNode.Values[LAt] <> nil then
      begin
        Inc(LNode.CountValue);
      end;
    end;
  end
  else
  begin
    SetLength(LNode.Children, CMetadataPageSlots);
    for LAt := 0 to CMetadataPageSlots - 1 do
    begin

      if APrevious <> nil then
      begin
        LNode.Children[LAt] := APrevious.Child(LAt);
      end;
    end;
    LNode.Children[ASlot] := AChild;
    for LAt := 0 to CMetadataPageSlots - 1 do
    begin
      LCount := 0;

      if LNode.Children[LAt] <> nil then
      begin
        LCount := LNode.Children[LAt].Count;
      end;

      if LCount > High(Integer) - LNode.CountValue then
      begin
        raise ENyxPascalSyntaxLimit.Create('Ordinal page exceeds its exact count domain');
      end;
      Inc(LNode.CountValue, LCount);
    end;
  end;
end;

function TMetadataMap.InsertRow(const APrevious: IMetadataRowNode;
  ALevel, AIndex: Integer; const AValue: IMetadataValue): IMetadataRowNode;
var
  LSlot: Integer;
  LChild: IMetadataRowNode;
begin
  NyxPascalAdmitAnalysis(FExecution, papMetadataNode);
  LSlot := (AIndex shr (ALevel * 2)) and (CMetadataPageSlots - 1);

  if ALevel = 0 then
  begin
    Exit(NewRowNode(APrevious, 0, LSlot, AValue, nil));
  end;
  LChild := nil;

  if APrevious <> nil then
  begin
    LChild := APrevious.Child(LSlot);
  end;
  LChild := InsertRow(LChild, ALevel - 1, AIndex, AValue);
  Result := NewRowNode(APrevious, ALevel, LSlot, nil, LChild);
end;

procedure TMetadataMap.StoreRow(AKind: TMetadataKind; AIndex: Integer;
  const AValue: IMetadataValue);
var
  LRoot: IMetadataRowNode;
  LLevel: Integer;
begin
  RequireConstruction;
  RequireOrdinalKind(AKind);

  if AIndex < 0 then
  begin
    raise ENyxPascalLexical.Create('An ordinal metadata write needs a nonnegative index');
  end;
  LRoot := FRows[Ord(AKind)];
  LLevel := 0;

  if LRoot <> nil then
  begin
    LLevel := LRoot.Level;
  end;
  { Level fifteen selects bits 30..31, covering every nonnegative Integer.
    No shift by 32 or overflowing capacity multiplication is ever evaluated. }
  while LLevel < 15 do
  begin

    if AIndex shr ((LLevel + 1) * 2) = 0 then
    begin
      Break;
    end;
    Inc(LLevel);

    if LRoot <> nil then
    begin
      LRoot := NewRowNode(nil, LLevel, 0, nil, LRoot);
    end;
  end;
  LRoot := InsertRow(LRoot, LLevel, AIndex, AValue);
  NyxPascalAdmitAnalysis(FExecution, papMetadataValue);
  FRows[Ord(AKind)] := LRoot;
  RememberRow(AKind, AIndex, AValue);
end;

function TMetadataMap.FindRow(AKind: TMetadataKind; AIndex: Integer): IMetadataValue;
var
  LNode: IMetadataRowNode;
  LFirst: Boolean;
  LAt: Integer;
begin
  RequireOrdinalKind(AKind);
  Result := nil;

  if AIndex < 0 then
  begin
    Exit;
  end;

  if FConstructing then
  begin
    NyxPascalAdmitAnalysis(FExecution, papMetadataValue);
    IncrementMetadataAccess(FReadLookups);
    IncrementMetadataAccess(FOrdinalLookups);
    for LAt := 0 to FReadCount - 1 do
    begin

      if (FReadOrdinals[LAt] = AIndex) and (FReadKinds[LAt] = AKind) then
      begin
        IncrementMetadataAccess(FReadHits);
        Exit(FReadValues[LAt]);
      end;
    end;
  end;
  LNode := FRows[Ord(AKind)];
  LFirst := True;
  while LNode <> nil do
  begin

    if FConstructing then
    begin
      NyxPascalAdmitAnalysis(FExecution, papMetadataNode);
      IncrementMetadataAccess(FReadTreeVisits);
      IncrementMetadataAccess(FOrdinalReads);
    end;

    if LFirst then
    begin

      if LNode.Level < 15 then
      begin

        if AIndex shr ((LNode.Level + 1) * 2) <> 0 then
        begin
          Break;
        end;
      end;
    end;
    LFirst := False;

    if LNode.Level = 0 then
    begin
      Result := LNode.Value(AIndex and (CMetadataPageSlots - 1));
      Break;
    end;
    LNode := LNode.Child((AIndex shr (LNode.Level * 2)) and (CMetadataPageSlots - 1));
  end;

  if FConstructing then
  begin
    RememberRow(AKind, AIndex, Result);
  end;
end;

function TMetadataMap.NewNode(const AKey: TNyxText; const AValue: IMetadataValue;
  const ALeft, ARight: IMetadataNode): IMetadataNode;
var
  LNode: TMetadataNode;
  LLeftCount: Integer;
  LRightCount: Integer;
begin
  NyxPascalAdmitAnalysis(FExecution, papMetadataNode);
  LLeftCount := NodeCount(ALeft);
  LRightCount := NodeCount(ARight);

  if (FNodes = High(Integer)) or (LLeftCount > High(Integer) - 1 - LRightCount) then
  begin
    raise ENyxPascalSyntaxLimit.Create('Metadata construction exceeds its exact counter domain');
  end;
  LNode := TMetadataNode.Create;
  Result := LNode;
  LNode.KeyValue := AKey;
  LNode.Payload := AValue;
  LNode.LeftValue := ALeft;
  LNode.RightValue := ARight;
  LNode.HeightValue := NodeHeight(ALeft);

  if NodeHeight(ARight) > LNode.HeightValue then
  begin
    LNode.HeightValue := NodeHeight(ARight);
  end;
  Inc(LNode.HeightValue);
  LNode.CountValue := 1 + LLeftCount + LRightCount;
  Inc(FNodes);
end;

function TMetadataMap.Balance(const AKey: TNyxText; const AValue: IMetadataValue;
  const ALeft, ARight: IMetadataNode): IMetadataNode;
var
  LMiddle: IMetadataNode;
begin

  if NodeHeight(ALeft) > NodeHeight(ARight) + 1 then
  begin

    if NodeHeight(ALeft.Left) >= NodeHeight(ALeft.Right) then
    begin
      Exit(NewNode(ALeft.Key, ALeft.Value, ALeft.Left,
        NewNode(AKey, AValue, ALeft.Right, ARight)));
    end;
    LMiddle := ALeft.Right;
    Exit(NewNode(LMiddle.Key, LMiddle.Value,
      NewNode(ALeft.Key, ALeft.Value, ALeft.Left, LMiddle.Left),
      NewNode(AKey, AValue, LMiddle.Right, ARight)));
  end;

  if NodeHeight(ARight) > NodeHeight(ALeft) + 1 then
  begin

    if NodeHeight(ARight.Right) >= NodeHeight(ARight.Left) then
    begin
      Exit(NewNode(ARight.Key, ARight.Value,
        NewNode(AKey, AValue, ALeft, ARight.Left), ARight.Right));
    end;
    LMiddle := ARight.Left;
    Exit(NewNode(LMiddle.Key, LMiddle.Value,
      NewNode(AKey, AValue, ALeft, LMiddle.Left),
      NewNode(ARight.Key, ARight.Value, LMiddle.Right, ARight.Right)));
  end;
  Result := NewNode(AKey, AValue, ALeft, ARight);
end;

function TMetadataMap.InsertFirst(const ANode: IMetadataNode; const AKey: TNyxText;
  AOrdinal: Integer; AReadLookup: Boolean; out AValue: IMetadataValue;
  out AInserted: Boolean): IMetadataNode;
var
  LChild: IMetadataNode;
  LCreated: TOrdinalValue;
begin
  { Search and persistent insertion follow one path. A cached absence still
    needs that insertion walk, but does not become a second lookup receipt.
    Every visited/prepared node remains admitted, including the empty leaf. }
  NyxPascalAdmitAnalysis(FExecution, papMetadataNode);

  if ANode = nil then
  begin
    NyxPascalAdmitAnalysis(FExecution, papMetadataValue);
    LCreated := TOrdinalValue.Create;
    AValue := LCreated;
    LCreated.OwnerStamp := FStamp;
    LCreated.Value := AOrdinal;
    AInserted := True;
    Exit(NewNode(AKey, AValue, nil, nil));
  end;

  if AReadLookup then
  begin
    IncrementMetadataAccess(FReadTreeVisits);
  end;

  if AKey < ANode.Key then
  begin
    LChild := InsertFirst(ANode.Left, AKey, AOrdinal, AReadLookup, AValue, AInserted);

    if not AInserted then
    begin
      Exit(ANode);
    end;
    Result := Balance(ANode.Key, ANode.Value, LChild, ANode.Right);
  end
  else if AKey > ANode.Key then
  begin
    LChild := InsertFirst(ANode.Right, AKey, AOrdinal, AReadLookup, AValue, AInserted);

    if not AInserted then
    begin
      Exit(ANode);
    end;
    Result := Balance(ANode.Key, ANode.Value, ANode.Left, LChild);
  end
  else
  begin
    AValue := ANode.Value;
    AInserted := False;
    Result := ANode;
  end;
end;

function TMetadataMap.TryReadCache(const AKey: TNyxText;
  out AValue: IMetadataValue): Boolean;
var
  LAt: Integer;
begin
  Result := False;
  AValue := nil;

  if FConstructing then
  begin
    { One bounded probe includes at most eight exact comparisons and one slot
      update. Tree misses still admit every visited node independently. }
    NyxPascalAdmitAnalysis(FExecution, papMetadataValue);
    IncrementMetadataAccess(FReadLookups);
    for LAt := 0 to FReadCount - 1 do
    begin

      if (FReadOrdinals[LAt] < 0) and (FReadKeys[LAt] = AKey) then
      begin
        IncrementMetadataAccess(FReadHits);
        AValue := FReadValues[LAt];
        Exit(True);
      end;
    end;
  end;
end;

function TMetadataMap.Find(const AKey: TNyxText): IMetadataValue;
var
  LNode: IMetadataNode;
begin

  if TryReadCache(AKey, Result) then
  begin
    Exit;
  end;
  LNode := FRoot;
  while LNode <> nil do
  begin

    if FConstructing then
    begin
      NyxPascalAdmitAnalysis(FExecution, papMetadataNode);
      IncrementMetadataAccess(FReadTreeVisits);
    end;

    if AKey < LNode.Key then
    begin
      LNode := LNode.Left;
    end
    else if AKey > LNode.Key then
    begin
      LNode := LNode.Right;
    end
    else
    begin
      Result := LNode.Value;
      Break;
    end;
  end;

  if FConstructing then
  begin
    RememberRead(AKey, Result);
  end;
end;

function MetadataKey(AKind: TMetadataKind; const AKey: TNyxText): TNyxText;
begin
  Result := TNyxText(IntToStr(Ord(AKind))) + ':' + AKey;
end;

function TMetadataMap.Row(AKind: TMetadataKind; AIndex: Integer): TObject;
var
  LValue: IMetadataValue;
begin
  LValue := FindRow(AKind, AIndex);

  if LValue = nil then
  begin
    raise ENyxPascalLexical.Create('Metadata row is outside its immutable ordinal pages');
  end;
  Result := LValue.Borrow;
end;

function TMetadataMap.Edit(AKind: TMetadataKind; AIndex: Integer): TObject;
var
  LValue: IMetadataValue;
  LCreated: TMetadataValue;
begin
  RequireConstruction;

  if AIndex < 0 then
  begin
    raise ENyxPascalLexical.Create('A metadata row needs a nonnegative source ordinal');
  end;
  LValue := FindRow(AKind, AIndex);

  if LValue = nil then
  begin
    NyxPascalAdmitAnalysis(FExecution, papMetadataValue);
    case AKind of
      mkScope:
        begin
          LCreated := TScopeValue.Create;
        end;
      mkDeclaration:
        begin
          LCreated := TDeclarationValue.Create;
        end;
      mkImport:
        begin
          LCreated := TImportValue.Create;
        end;
      mkSelector:
        begin
          LCreated := TSelectorValue.Create;
        end;
      mkArm:
        begin
          LCreated := TArmValue.Create;
        end;
      mkVariantLabel:
        begin
          LCreated := TVariantLabelValue.Create;
        end;
      mkStatementLabel:
        begin
          LCreated := TStatementLabelValue.Create;
        end;
      mkHint:
        begin
          LCreated := THintValue.Create;
        end;
      mkIntrinsic:
        begin
          LCreated := TIntrinsicValue.Create;
        end;
    else
      raise ENyxPascalLexical.Create('Scalar indexes cannot acquire mutable row rights');
    end;
    LValue := LCreated;
    LCreated.OwnerStamp := FStamp;
    StoreRow(AKind, AIndex, LValue);
    Inc(FCreated);
  end
  else if LValue.Stamp <> FStamp then
  begin
    NyxPascalAdmitAnalysis(FExecution, papMetadataValue);
    LValue := LValue.Detach(FStamp);
    StoreRow(AKind, AIndex, LValue);
    Inc(FCopied);
  end;
  Result := LValue.Borrow;
end;

function TMetadataMap.Index(AKind: TMetadataKind; const AKey: TNyxText): Integer;
var
  LValue: IMetadataValue;
begin
  LValue := Find(MetadataKey(AKind, AKey));
  Result := -1;

  if LValue <> nil then
  begin
    Result := TOrdinalValue(LValue.Borrow).Value;
  end;
end;

function TMetadataMap.FindOrAddFirst(AKind: TMetadataKind; const AKey: TNyxText;
  AValue: Integer; out AFirst: Integer): Boolean;
var
  LKey: TNyxText;
  LLease: IMetadataValue;
  LCandidate: IMetadataNode;
  LCached, LInserted: Boolean;
begin
  RequireConstruction;
  AFirst := -1;
  LKey := MetadataKey(AKind, AKey);
  LCached := TryReadCache(LKey, LLease);

  if LCached and (LLease <> nil) then
  begin
    AFirst := TOrdinalValue(LLease.Borrow).Value;
    Exit(True);
  end;
  LCandidate := InsertFirst(FRoot, LKey, AValue, not LCached, LLease, LInserted);
  Result := not LInserted;

  if LInserted then
  begin
    { Candidate values/nodes are owned locally. Admit the bounded root/cache
      update before publication; refusal cannot alter the old root or a sibling. }
    NyxPascalAdmitAnalysis(FExecution, papMetadataValue);
    FRoot := LCandidate;
    Inc(FCreated);
  end
  else
  begin
    AFirst := TOrdinalValue(LLease.Borrow).Value;
  end;
  RememberRead(LKey, LLease);
end;

procedure TMetadataMap.AddFirst(AKind: TMetadataKind; const AKey: TNyxText; AValue: Integer);
var
  LFirst: Integer;
begin
  FindOrAddFirst(AKind, AKey, AValue, LFirst);
end;

function TFilterBlock.Row(AIndex: Integer): TFilterRow;
begin
  Result := Rows[AIndex];
end;

constructor TFilteredTokens.Create(APrevious: TFilteredTokens; ACount: Integer;
  const ACancellation: INyxCancellationScope);
var
  LAt: Integer;
  LRow: TFilterRow;
begin
  inherited Create;

  if (ACount < 0) or ((ACount > 0) and
    ((APrevious = nil) or (ACount > APrevious.Count))) then
  begin
    raise ENyxPascalLexical.Create('Filter continuation exceeds its retained rows');
  end;
  FShared := ACount div CFilterBlockRows * CFilterBlockRows;
  SetLength(FBlocks, FShared div CFilterBlockRows);
  for LAt := 0 to High(FBlocks) do
  begin

    if (ACancellation <> nil) and ACancellation.Cancelled then
    begin
      raise ENyxPascalAnalysisCancelled.Create('Filter directory continuation cancelled');
    end;
    NyxPascalAdmitAnalysis(ACancellation, papFilterRow);
    FBlocks[LAt] := APrevious.FBlocks[LAt];
  end;
  FCount := FShared;
  for LAt := FShared to ACount - 1 do
  begin

    if (LAt mod 128 = 0) and (ACancellation <> nil) and ACancellation.Cancelled then
    begin
      raise ENyxPascalAnalysisCancelled.Create('Filter partial-row continuation cancelled');
    end;
    NyxPascalAdmitAnalysis(ACancellation, papFilterRow);
    LRow := APrevious.Row(LAt);
    Add(LRow.Token, LRow.Conditional);
    Inc(FCopied);
  end;
end;

function TFilteredTokens.Row(AIndex: Integer): TFilterRow;
begin

  if (AIndex < 0) or (AIndex >= FCount) then
  begin
    raise ENyxPascalLexical.Create('Significant token is outside its immutable filter');
  end;
  Result := FBlocks[AIndex div CFilterBlockRows].Row(AIndex mod CFilterBlockRows);
end;

function TFilteredTokens.Token(AIndex: Integer): Integer;
begin
  Result := Row(AIndex).Token;
end;

function TFilteredTokens.IsConditional(AIndex: Integer): Boolean;
begin
  Result := Row(AIndex).Conditional;
end;

function TFilteredTokens.DirectoryEntries: Integer;
begin
  Result := Length(FBlocks);
end;

procedure TFilteredTokens.Add(AToken: Integer; AConditional: Boolean);
var
  LBlock: TFilterBlock;
  LLease: IFilterBlock;
  LAt: Integer;
begin

  if FCount mod CFilterBlockRows = 0 then
  begin
    LBlock := TFilterBlock.Create;
    LLease := LBlock;
    SetLength(LBlock.Rows, CFilterBlockRows);
    LAt := Length(FBlocks);
    SetLength(FBlocks, LAt + 1);
    FBlocks[LAt] := LLease;
    FWriting := LBlock;
  end;
  FWriting.Rows[FCount mod CFilterBlockRows].Token := AToken;
  FWriting.Rows[FCount mod CFilterBlockRows].Conditional := AConditional;
  Inc(FCount);
end;

procedure TFilteredTokens.Seal;
begin
  FWriting := nil;
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
  FMetadata := TMetadataMap.Create(nil, ACancellation);
  FActiveArm := -1;
  try
    FLexical := NewNyxPascalLexicalSnapshot(ASource, AOptions.LexicalOptions, ACancellation);
    Build;
  finally

    if FMetadata <> nil then
    begin
      FMetadata.RetireExecution;
    end;
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
  FMetadata := TMetadataMap.Create(nil, ACancellation);
  FActiveArm := -1;
  try
    FLexical := ALexical;
    Build(APrevious, ARetained);
  finally

    if FMetadata <> nil then
    begin
      FMetadata.RetireExecution;
    end;
    FCancellation := nil;
  end;
end;

destructor TSyntaxSnapshot.Destroy;
begin
  FTokens.Free;
  FMetadata.Free;
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
  NyxPascalAdmitAnalysis(FCancellation, papSyntaxToken);

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
  AVisibility: TNyxPascalSyntaxVisibility; AMembers: Boolean; ASection: TDeclarationSection);
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
  FCheckpoint.ModuleUnit := FModuleUnit;
  FCheckpoint.ImplementationSeen := FImplementationSeen;
  FCheckpoint.ActiveArm := FActiveArm;
  FCheckpoint.Visibility := AVisibility;
  FCheckpoint.Members := AMembers;
  FCheckpoint.Section := ASection;
  FCheckpointMetadata := FMetadata.Capture;
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
  FModuleUnit := FCheckpoint.ModuleUnit;
  FImplementationSeen := FCheckpoint.ImplementationSeen;
  FActiveArm := FCheckpoint.ActiveArm;
  FReadThrough := FCheckpoint.ReadThrough;
  FReused := FDeclarationCount;
  FRestart := FTokens[FPosition];
  FreeAndNil(FMetadata);
  FCheckpointMetadata := APrevious.FCheckpointMetadata;
  FMetadata := TMetadataMap.Create(FCheckpointMetadata, FCancellation);
  FRetainedMetadata := 0;

  if FCheckpointMetadata <> nil then
  begin
    FRetainedMetadata := FCheckpointMetadata.Count;
  end;
  EditFrame(0).Value.FSpan := NyxPascalSpan(Frame(0).Value.Span.Start,
    FLexical.Scalars - Frame(0).Value.Span.Start);
  { Filter diagnostics belong to the current exact filter. Only parser
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
    NyxPascalAdmitAnalysis(FCancellation, papDiagnosticRow);
    FDiagnostics[LFilterCount + LAt] :=
      APrevious.FDiagnostics[APrevious.FFilterDiagnostics + LAt];
    Inc(FCopied);
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

  if LAt >= FTokens.Count then
  begin
    LAt := FTokens.Count - 1;
  end;
  Result := ReadToken(FTokens[LAt]);
end;

function TSyntaxSnapshot.Word(AWord: TNyxPascalWord; AOffset: Integer): Boolean;
var
  LToken: TNyxPascalToken;
begin
  { Kind and payload belong to one immutable token at this unchanged offset.
    One real read retains admission/frontier tracking; no shared cursor cache
    or skipped read charge is introduced. }
  LToken := Tok(AOffset);
  Result := (LToken.Kind = ptkWord) and (LToken.Word = AWord);
end;

function TSyntaxSnapshot.Packing: Boolean;
var
  LToken: TNyxPascalToken;
begin
  LToken := Tok;
  Result := False;

  if (LToken.Kind = ptkWord) and not LToken.Escaped then
  begin
    Result := (LToken.Word = pkwPacked) or
      ((LToken.Span.Length = 9) and
        (NyxPascalASCIIKey(FLexical.Identifier(FTokens[FPosition])) = 'bitpacked'));
  end;
end;

function TSyntaxSnapshot.Sym(ASymbol: TNyxPascalSymbol; AOffset: Integer): Boolean;
var
  LToken: TNyxPascalToken;
begin
  LToken := Tok(AOffset);
  Result := (LToken.Kind = ptkSymbol) and (LToken.Symbol = ASymbol);

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

  if FPosition < FTokens.Count - 1 then
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
  NyxPascalAdmitAnalysis(FCancellation, papDiagnosticRow);
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
    EditFrame(LScope).Value.FUnsupported := True;

    if Frame(LScope).Value.OwnerDeclaration >= 0 then
    begin
      Include(EditEntry(Frame(LScope).Value.OwnerDeclaration).Value.FFlags, psdfUnsupported);
      Break;
    end;
    LScope := Frame(LScope).Value.Parent;
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
var
  LFrame: TScopeValue;
  LSpan: TNyxPascalSpan;
begin
  Result := FScopeCount;

  if Result >= FOptions.MaximumScopes then
  begin
    raise ENyxPascalSyntaxLimit.Create('Pascal syntax scopes exceed their budget');
  end;
  LSpan := NyxPascalSpan(AStart, FLexical.Scalars - AStart);
  { One owner-checked borrow covers this fixed initialization. No metadata call
    or checkpoint capture intervenes before the last assignment. Later edits
    still reacquire the row and independently check its current COW stamp. }
  LFrame := EditFrame(Result);
  LFrame.Value.FKind := AKind;
  LFrame.Value.FParent := AParent;
  LFrame.Value.FOwner := AOwner;
  LFrame.Value.FFirst := -1;
  LFrame.Value.FLast := -1;
  LFrame.Value.FSpan := LSpan;
  LFrame.Value.FName := AName;
  Inc(FScopeCount);
end;

procedure TSyntaxSnapshot.FinishScope(AScope: Integer; AComplete: Boolean);
var
  LFrame: TScopeValue;
begin
  LFrame := EditFrame(AScope);
  LFrame.Value.FComplete := AComplete;

  if AComplete then
  begin
    { SpanFrom reads lexical coordinates only; it neither captures metadata nor
      changes the stamp protecting this bounded scope update. }
    LFrame.Value.FSpan := SpanFrom(LFrame.Value.Span.Start);
  end;
end;

function TSyntaxSnapshot.AddDeclaration(AScope: Integer;
  AKind: TNyxPascalSyntaxDeclarationKind; AVisibility: TNyxPascalSyntaxVisibility;
  AName: Integer; AOperator: TNyxPascalOperatorKind; AAnonymous: Boolean): Integer;
var
  LToken: TNyxPascalToken;
  LName: TNyxText;
  LEntry: TDeclarationValue;
begin
  NodeBudget;
  Result := FDeclarationCount;

  if Result >= FOptions.MaximumDeclarations then
  begin
    raise ENyxPascalSyntaxLimit.Create('Pascal declarations exceed their syntax budget');
  end;
  LToken := ReadToken(FTokens[AName]);

  if AAnonymous then
  begin
    LName := '';
  end
  else if (AOperator = pokNone) and not ((AKind = psdkLabel) and (LToken.Kind = ptkNumber)) then
  begin
    LName := FLexical.Identifier(FTokens[AName]);
  end
  else
  begin
    LName := FLexical.Text(FTokens[AName]);
  end;
  { Initialize the new declaration through one checked mutable row. This fixed
    group performs no metadata lookup/capture/callback and retains no borrow in
    the published value. Conditional/variant/link updates reacquire afterward. }
  LEntry := EditEntry(Result);
  LEntry.Value.FName := LName;
  LEntry.Value.FKind := AKind;
  LEntry.Value.FRoutineKind := prkNone;
  LEntry.Value.FOperator := AOperator;
  LEntry.Value.FVisibility := AVisibility;
  LEntry.Value.FNameSpan := LToken.Span;
  LEntry.Value.FScope := AScope;
  LEntry.Value.FNestedScope := -1;
  LEntry.Value.FNext := -1;
  LEntry.Value.FNextNamed := -1;
  LEntry.Value.FLastNamed := Result;
  LEntry.Value.FVariantArm := -1;
  LEntry.Value.FFirstHint := -1;
  LEntry.Value.FLastHint := -1;
  LEntry.Value.FFirstIntrinsic := -1;
  LEntry.Value.FLastIntrinsic := -1;
  LEntry.Value.FConvention := prcDefault;
  LEntry.Value.FConventionSpan := NyxPascalSpan(0, 0);
  LEntry.Value.FAnonymous := AAnonymous;

  if (AKind = psdkField) and (FActiveArm >= 0) and
    (SelectorRow(ArmRow(FActiveArm).Value.SelectorIndex).Value.RecordScope = AScope) then
  begin
    EditEntry(Result).Value.FVariantArm := FActiveArm;
  end;
  Inc(FDeclarationCount);

  if FTokens.IsConditional(AName) then
  begin
    Include(EditEntry(Result).Value.FFlags, psdfConditional);
  end;

  LinkDeclaration(Result);
end;

procedure TSyntaxSnapshot.LinkDeclaration(AIndex: Integer);
var
  LScope: Integer;
  LKey: TNyxText;
  LFirst: Integer;
  LLast: Integer;
  LName: TNyxText;
  LAnonymous: Boolean;
  LOperator: TNyxPascalOperatorKind;
  LDeclaration: TDeclarationValue;
  LFrame: TScopeValue;
begin
  { Capture immutable fields before any edit. Subsequent writes below affect
    links only; none can change these scalar/name values or rotate a stamp. }
  LDeclaration := Entry(AIndex);
  LScope := LDeclaration.Value.Scope;
  LName := LDeclaration.Value.Name;
  LAnonymous := LDeclaration.Value.Anonymous;
  LOperator := LDeclaration.Value.OperatorKind;
  LDeclaration := EditEntry(AIndex);
  LDeclaration.Value.FNext := -1;
  LDeclaration.Value.FNextNamed := -1;
  LDeclaration.Value.FLastNamed := AIndex;
  LLast := Frame(LScope).Value.FLast;

  if LLast >= 0 then
  begin
    EditEntry(LLast).Value.FNext := AIndex;
  end;
  { Reacquire only after the other row's update. This fixed scope-link group
    performs no further metadata call or checkpoint capture while borrowed. }
  LFrame := EditFrame(LScope);

  if LLast < 0 then
  begin
    LFrame.Value.FFirst := AIndex;
  end;
  LFrame.Value.FLast := AIndex;
  Inc(LFrame.Value.FCount);

  if LAnonymous then
  begin
    Exit;
  end;
  LKey := TNyxText(IntToStr(LScope)) + ':' + NyxPascalASCIIKey(LName);

  if LOperator <> pokNone then
  begin
    LKey := TNyxText(IntToStr(LScope)) + ':operator#' +
      TNyxText(IntToStr(Ord(LOperator)));
  end;

  if FMetadata.FindOrAddFirst(mkName, LKey, AIndex, LFirst) then
  begin
    EditEntry(Entry(LFirst).Value.FLastNamed).Value.FNextNamed := AIndex;
    EditEntry(LFirst).Value.FLastNamed := AIndex;
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
  LToken: TNyxPascalToken;
begin
  Result := False;
  AKind := phkDeprecated;
  LToken := Tok;

  if (LToken.Kind <> ptkWord) or LToken.Escaped then
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


    Inc(FHintCount);
    LStart := Tok.Span.Start;
    Step;
    LMessage := FPosition;
    while Tok.Kind in [ptkString, ptkCharacter] do
    begin
      Step;
    end;
    EditHintRow(LAt).Value.FDeclaration := ADeclaration;
    EditHintRow(LAt).Value.FKind := LKind;
    EditHintRow(LAt).Value.FMessage := Range(LMessage, FPosition);
    EditHintRow(LAt).Value.FSpan := SpanFrom(LStart);
    EditHintRow(LAt).Value.FNext := -1;

    if Entry(ADeclaration).Value.FFirstHint < 0 then
    begin
      EditEntry(ADeclaration).Value.FFirstHint := LAt;
    end
    else
    begin
      EditHintRow(Entry(ADeclaration).Value.FLastHint).Value.FNext := LAt;
    end;
    EditEntry(ADeclaration).Value.FLastHint := LAt;
  end;
end;

procedure TSyntaxSnapshot.CloneDeclarationHints(AFrom, ATo: Integer);
var
  LFrom: Integer;
  LAt: Integer;
begin
  LFrom := Entry(AFrom).Value.FirstHint;
  while LFrom >= 0 do
  begin
    NodeBudget;
    LAt := FHintCount;


    Inc(FHintCount);
    EditHintRow(LAt).Value := HintRow(LFrom).Value;
    EditHintRow(LAt).Value.FDeclaration := ATo;
    EditHintRow(LAt).Value.FNext := -1;

    if Entry(ATo).Value.FFirstHint < 0 then
    begin
      EditEntry(ATo).Value.FFirstHint := LAt;
    end
    else
    begin
      EditHintRow(Entry(ATo).Value.FLastHint).Value.FNext := LAt;
    end;
    EditEntry(ATo).Value.FLastHint := LAt;
    LFrom := HintRow(LFrom).Value.NextHint;
  end;
end;

function NyxPascalCallingConventionNamed(const AName: TNyxText;
  out AConvention: TNyxPascalRoutineConvention): Boolean;
const
  CNames: array[TNyxPascalRoutineConvention] of TNyxText =
    ('', 'register', 'cdecl', 'stdcall', 'pascal', 'safecall', 'syscall', 'vectorcall', 'winapi');
var
  LKey: TNyxText;
  LValue: TNyxPascalRoutineConvention;
begin
  LKey := NyxPascalASCIIKey(AName);
  for LValue := Succ(Low(TNyxPascalRoutineConvention)) to High(TNyxPascalRoutineConvention) do
  begin

    if LKey = CNames[LValue] then
    begin
      AConvention := LValue;
      Exit(True);
    end;
  end;
  AConvention := prcDefault;
  Result := False;
end;

function TSyntaxSnapshot.RoutineConvention(out AConvention: TNyxPascalRoutineConvention): Boolean;
var
  LToken: TNyxPascalToken;
begin
  AConvention := prcDefault;
  LToken := Tok;
  Result := (LToken.Kind = ptkWord) and not LToken.Escaped and
    NyxPascalCallingConventionNamed(Name, AConvention);
end;

procedure TSyntaxSnapshot.SetRoutineConvention(ADeclaration: Integer;
  AConvention: TNyxPascalRoutineConvention; const ASpan: TNyxPascalSpan);
begin

  if Entry(ADeclaration).Value.CallingConvention <> prcDefault then
  begin
    Unsupported(Entry(ADeclaration).Value.Scope, 'This routine has conflicting calling conventions', ASpan);
    Include(EditEntry(ADeclaration).Value.FFlags, psdfUnsupported);
    Exit;
  end;
  EditEntry(ADeclaration).Value.FConvention := AConvention;
  EditEntry(ADeclaration).Value.FConventionSpan := ASpan;
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
  else if AName = 'iocheck' then
  begin
    AFlag := plfIOCheck;
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
  LToken: TNyxPascalToken;
  LConvention: TNyxPascalRoutineConvention;
begin
  Result := False;
  LToken := Tok;

  if (LToken.Kind <> ptkWord) or LToken.Escaped then
  begin
    Exit;
  end;
  Result := RoutineModifier(LToken.Word) or HintWord(LHint) or RoutineConvention(LConvention);

  if LinkageFlagNamed(NyxPascalASCIIKey(Name), LFlag) then
  begin
    Result := True;

    if (AScope >= 0) and (LFlag = plfPublic) then
    begin

      if Frame(AScope).Value.Kind = psskMembers then
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


  Inc(FIntrinsicCount);
  EditIntrinsicRow(LAt).Value.FDeclaration := ADeclaration;
  EditIntrinsicRow(LAt).Value.FKind := AKind;
  EditIntrinsicRow(LAt).Value.FKey := AKey;
  EditIntrinsicRow(LAt).Value.FOperand := AOperand;
  EditIntrinsicRow(LAt).Value.FSpan := SpanFrom(AKey.Start);
  EditIntrinsicRow(LAt).Value.FNext := -1;
  EditIntrinsicRow(LAt).Value.FComplete := AEnvelope and (AOperand.Length > 0);

  if Entry(ADeclaration).Value.FFirstIntrinsic < 0 then
  begin
    EditEntry(ADeclaration).Value.FFirstIntrinsic := LAt;
  end
  else
  begin
    EditIntrinsicRow(Entry(ADeclaration).Value.FLastIntrinsic).Value.FNext := LAt;
  end;
  EditEntry(ADeclaration).Value.FLastIntrinsic := LAt;

  if not IntrinsicRow(LAt).Value.Complete then
  begin
    Include(EditEntry(ADeclaration).Value.FFlags, psdfIncomplete);
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
  LIsConvention: Boolean;
  LConvention: TNyxPascalRoutineConvention;
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
    (Frame(Entry(ADeclaration).Value.Scope).Value.Kind = psskMembers) then
  begin
    Exit;
  end;

  if not ARoutine and
    (LFlag in [plfCompilerHelper, plfIntrinsic, plfConstantIntrinsic,
      plfRTLProcedure, plfNoReturn, plfIOCheck]) then
  begin
    Exit;
  end;
  Result := True;

  if LBracket then
  begin
    Include(EditEntry(ADeclaration).Value.FLinkage, plfPlatformAttributes);
    Step;
    while not EOF and not Sym(psoCloseBracket) and not Sym(psoSemicolon) and
      not Word(pkwBegin) and not Word(pkwEnd) and not Word(pkwImplementation) do
    begin
      LIntrinsic := False;
      { An unknown neighboring key must not inherit the preceding key's
        operand rules while its unsupported source remains preserved. }
      LFlag := plfPlatformAttributes;
      LKey := Tok.Span;
      LIsConvention := RoutineConvention(LConvention);

      if LIsConvention then
      begin
        SetRoutineConvention(ADeclaration, LConvention, LKey);
      end
      else if (Tok.Kind = ptkWord) and not Tok.Escaped and
        LinkageFlagNamed(NyxPascalASCIIKey(Name), LFlag) then
      begin
        Include(EditEntry(ADeclaration).Value.FLinkage, LFlag);
        LIntrinsic := LFlag in [plfIntrinsic, plfConstantIntrinsic];
      end
      else
      begin
        Unsupported(Entry(ADeclaration).Value.Scope,
          'This platform declaration attribute needs explicit meaning', Tok.Span);
        Include(EditEntry(ADeclaration).Value.FFlags, psdfUnsupported);
      end;
      Step;
      LColon := False;

      if (LIsConvention or (LFlag in [plfRTLProcedure, plfNoReturn, plfIOCheck])) and
        not (Sym(psoComma) or Sym(psoCloseBracket)) then
      begin
        Unsupported(Entry(ADeclaration).Value.Scope,
          'This routine declaration marker has no operand', Tok.Span);
        Include(EditEntry(ADeclaration).Value.FFlags, psdfUnsupported);
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
      Include(EditEntry(ADeclaration).Value.FFlags, psdfIncomplete);
      Problem(psiUnclosedScope, 'Finish this platform declaration attribute', Tok.Span);
    end;
  end
  else
  begin
    Include(EditEntry(ADeclaration).Value.FLinkage, LFlag);
    LKey := Tok.Span;
    Step;
    LColon := False;

    if (LFlag in [plfRTLProcedure, plfNoReturn, plfIOCheck]) and not Sym(psoSemicolon) and
      not (ARoutine and RoutineClause(Entry(ADeclaration).Value.Scope)) then
    begin
      Unsupported(Entry(ADeclaration).Value.Scope,
        'This routine declaration marker has no operand', Tok.Span);
      Include(EditEntry(ADeclaration).Value.FFlags, psdfUnsupported);
    end;

    if (LFlag in [plfIntrinsic, plfConstantIntrinsic]) and Sym(psoColon) then
    begin
      LColon := True;
      Step;
    end;
    LOperand := FPosition;

    if not (ARoutine and
      (LFlag in [plfRTLProcedure, plfNoReturn, plfIOCheck, plfCompilerHelper]) and
      RoutineClause(Entry(ADeclaration).Value.Scope)) then
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
        LColon and (Sym(psoSemicolon) or (ARoutine and RoutineClause(Entry(ADeclaration).Value.Scope))));
    end;
  end;

  if not Sym(psoSemicolon) and not (ARoutine and RoutineClause(Entry(ADeclaration).Value.Scope)) then
  begin
    Include(EditEntry(ADeclaration).Value.FFlags, psdfIncomplete);
  end;

  if Sym(psoSemicolon) or not (ARoutine and RoutineClause(Entry(ADeclaration).Value.Scope)) then
  begin
    Separator;
  end;

  if Entry(ADeclaration).Value.FLinkageSpan.Length > 0 then
  begin
    LStart := Entry(ADeclaration).Value.FLinkageSpan.Start;
  end;
  EditEntry(ADeclaration).Value.FLinkageSpan := SpanFrom(LStart);
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
  var
    LToken: TNyxPascalToken;
    LConvention: TNyxPascalRoutineConvention;
  begin
    LToken := Tok(AOffset);
    Result := (LToken.Kind = ptkWord) and not LToken.Escaped and
      ((LToken.Word in [pkwVarargs, pkwInline]) or
        NyxPascalCallingConventionNamed(Name(AOffset), LConvention));
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
    LToken: TNyxPascalToken;
  begin
    { Contextual words may be identifiers. Declaration/body boundaries are never
      consumed as recovery labels; complete compiler keyword admission is separate. }
    LToken := Tok;
    Result := (LToken.Kind = ptkWord) and not (LToken.Word in [pkwBegin, pkwEnd,
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
    Result := (LToken.Kind = ptkNumber) and LToken.Complete;

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


    EditStatementLabelRow(LEntry).Value.FKind := LKind;
    EditStatementLabelRow(LEntry).Value.FDeclaration := LDeclaration;
    EditStatementLabelRow(LEntry).Value.FName := LName;
    EditStatementLabelRow(LEntry).Value.FScope := AScope;
    EditStatementLabelRow(LEntry).Value.FNameSpan := Tok.Span;
    EditStatementLabelRow(LEntry).Value.FConditional := FTokens.IsConditional(FPosition);
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
    EditStatementLabelRow(LAt).Value.FSectionSpan := SpanFrom(LStart);
    EditStatementLabelRow(LAt).Value.FComplete := LComplete;
    LDeclaration := StatementLabelRow(LAt).Value.DeclarationIndex;
    EditEntry(LDeclaration).Value.FHeaderSpan := StatementLabelRow(LAt).Value.SectionSpan;

    if not LComplete then
    begin
      Include(EditEntry(LDeclaration).Value.FFlags, psdfIncomplete);
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
      Include(EditEntry(AOwner).Value.FFlags, psdfForward);
      Exit;
    end;

    if Sym(psoOpenParen) then
    begin
      LAncestor := FPosition;
      Groups(psoOpenParen, psoCloseParen);
      EditEntry(AOwner).Value.FAncestorSpan := Range(LAncestor, FPosition);
    end;
    LMemberScope := AddScope(psskMembers, AScope, AOwner, Tok.Span.Start,
      Entry(AOwner).Value.Name);
    EditEntry(AOwner).Value.FNestedScope := LMemberScope;

    if (Entry(AOwner).Value.TypeForm = pstfClass) and Sym(psoSemicolon) then
    begin
      { Delphi's empty descendant spelling class(TAncestor) has no end. }
      EditFrame(LMemberScope).Value.FComplete := True;
      EditFrame(LMemberScope).Value.FSpan := NyxPascalSpan(Tok.Span.Start, 0);
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
        Include(EditEntry(AOwner).Value.FFlags, psdfIncomplete);
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

    if Packing and (Word(pkwRecord, 1) or Word(pkwObject, 1)) then
    begin
      Step;
    end;

    { A method pointer's of object is a qualifier, not an anonymous body. }

    if Word(pkwRecord) or (Word(pkwObject) and not Word(pkwOf, -1)) or
      Word(pkwInterface) or Word(pkwDispinterface) or
      (Word(pkwClass) and not Word(pkwOf, 1)) then
    begin
      LOwner := AddDeclaration(AScope, psdkType, AVisibility, FPosition, pokNone, True);
      EditEntry(LOwner).Value.FForm := Form;
      StructuredBody(AScope, LOwner);
      EditEntry(LOwner).Value.FTypeSpan := Range(LBodyStart, FPosition);
      EditEntry(LOwner).Value.FHeaderSpan := Entry(LOwner).Value.TypeSpan;
      FMetadata.AddFirst(mkStructure, TNyxText(IntToStr(Entry(LOwner).Value.TypeSpan.Start)), LOwner);
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
      EditEntry(LEntry).Value.FGenericSpan := Range(LGenericStart, FPosition);
    end;

    if not Sym(psoEqual) then
    begin
      Include(EditEntry(LEntry).Value.FFlags, psdfIncomplete);
      Problem(psiExpectedSeparator, 'Add an equals sign and type expression', Tok.Span);
      Separator;
      Exit;
    end;
    Step;
    LTypeStart := FPosition;

    if EOF or Sym(psoSemicolon) then
    begin
      Include(EditEntry(LEntry).Value.FFlags, psdfIncomplete);
      Problem(psiExpectedName, 'Add a type expression after the equals sign', Tok.Span);
      EditEntry(LEntry).Value.FTypeSpan := Range(LTypeStart, FPosition);
      Separator;
      EditEntry(LEntry).Value.FHeaderSpan := SpanFrom(LStart);
      Exit;
    end;

    if Packing then
    begin
      Step;
    end;
    LForm := Form;
    EditEntry(LEntry).Value.FForm := LForm;

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
          EditEntry(LEnum).Value.FTypeSpan := Entry(LEntry).Value.NameSpan;

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
        Include(EditEntry(LEntry).Value.FFlags, psdfIncomplete);
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
    EditEntry(LEntry).Value.FTypeSpan := Range(LTypeStart, FPosition);

    if LForm in [pstfClass, pstfInterface, pstfRecord, pstfObject] then
    begin
      FMetadata.AddFirst(mkStructure, TNyxText(IntToStr(Entry(LEntry).Value.TypeSpan.Start)), LEntry);
    end;
    DeclarationHints(LEntry);
    Separator;
    EditEntry(LEntry).Value.FHeaderSpan := SpanFrom(LStart);
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
        Remember(prTypesFragment, AVisibility, False, pdsNone);
      end
      else
      begin
        Remember(prTypes, AVisibility, FOptions.StartingContext = psfMembers, pdsNone);
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


    Inc(FSelectorCount);
    EditSelectorRow(LSelector).Value.FScope := AScope;
    EditSelectorRow(LSelector).Value.FParent := LParent;
    EditSelectorRow(LSelector).Value.FTag := -1;
    EditSelectorRow(LSelector).Value.FFirstArm := -1;
    EditSelectorRow(LSelector).Value.FLastArm := -1;
    Step;

    if (Tok.Kind = ptkWord) and Sym(psoColon, 1) then
    begin
      LTag := AddDeclaration(AScope, psdkField, AVisibility, FPosition);
      EditSelectorRow(LSelector).Value.FTag := LTag;
      Step;
      Step;
    end;
    LType := FPosition;
    while not EOF and not Word(pkwOf) and not Word(pkwEnd) and not Sym(psoCloseParen) do
    begin
      Step;
    end;
    EditSelectorRow(LSelector).Value.FType := Range(LType, FPosition);

    if SelectorRow(LSelector).Value.FTag >= 0 then
    begin
      LTag := SelectorRow(LSelector).Value.FTag;
      EditEntry(LTag).Value.FTypeSpan := SelectorRow(LSelector).Value.TypeSpan;
      EditEntry(LTag).Value.FHeaderSpan := NyxPascalSpan(Entry(LTag).Value.NameSpan.Start,
        EndOffset - Entry(LTag).Value.NameSpan.Start);
      EditEntry(LTag).Value.FForm := pstfNamed;
    end;

    if not Word(pkwOf) or (SelectorRow(LSelector).Value.TypeSpan.Length = 0) then
    begin
      Unsupported(AScope, 'A variant selector needs its type and of keyword', Tok.Span);
      EditSelectorRow(LSelector).Value.FSpan := SpanFrom(LStart);
      Exit;
    end;
    Step;
    while not EOF and not Word(pkwEnd) and not Sym(psoCloseParen) do
    begin
      NodeBudget;
      LArm := FArmCount;


      Inc(FArmCount);
      EditArmRow(LArm).Value.FSelector := LSelector;
      EditArmRow(LArm).Value.FFirstLabel := -1;
      EditArmRow(LArm).Value.FLastLabel := -1;
      EditArmRow(LArm).Value.FNext := -1;

      if SelectorRow(LSelector).Value.FFirstArm < 0 then
      begin
        EditSelectorRow(LSelector).Value.FFirstArm := LArm;
      end
      else
      begin
        EditArmRow(SelectorRow(LSelector).Value.FLastArm).Value.FNext := LArm;
      end;
      EditSelectorRow(LSelector).Value.FLastArm := LArm;
      LLabelStart := Tok.Span.Start;
      repeat
        LType := FPosition;
        Expression(psoColon, True, False, True);

        if LType = FPosition then
        begin
          Unsupported(AScope, 'A variant alternative needs a label expression', Tok.Span);
          EditArmRow(LArm).Value.FSpan := SpanFrom(LLabelStart);
          EditSelectorRow(LSelector).Value.FSpan := SpanFrom(LStart);
          Exit;
        end;
        NodeBudget;
        LLabel := FLabelCount;


        Inc(FLabelCount);
        EditVariantLabelRow(LLabel).Value.FArm := LArm;
        EditVariantLabelRow(LLabel).Value.FSpan := Range(LType, FPosition);
        EditVariantLabelRow(LLabel).Value.FNext := -1;

        if ArmRow(LArm).Value.FFirstLabel < 0 then
        begin
          EditArmRow(LArm).Value.FFirstLabel := LLabel;
        end
        else
        begin
          EditVariantLabelRow(ArmRow(LArm).Value.FLastLabel).Value.FNext := LLabel;
        end;
        EditArmRow(LArm).Value.FLastLabel := LLabel;

        if not Sym(psoComma) then
        begin
          Break;
        end;
        Step;
      until EOF;

      if not Sym(psoColon) then
      begin
        Unsupported(AScope, 'Finish variant labels with a colon', Tok.Span);
        EditArmRow(LArm).Value.FSpan := SpanFrom(LLabelStart);
        EditSelectorRow(LSelector).Value.FSpan := SpanFrom(LStart);
        Exit;
      end;
      Step;

      if not Sym(psoOpenParen) then
      begin
        Unsupported(AScope, 'A variant arm needs parenthesized fields', Tok.Span);
        EditArmRow(LArm).Value.FSpan := SpanFrom(LLabelStart);
        EditSelectorRow(LSelector).Value.FSpan := SpanFrom(LStart);
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
        EditArmRow(LArm).Value.FSpan := SpanFrom(LLabelStart);
        EditSelectorRow(LSelector).Value.FSpan := SpanFrom(LStart);
        Exit;
      end;
      Step;
      EditArmRow(LArm).Value.FComplete := True;
      EditArmRow(LArm).Value.FSpan := SpanFrom(LLabelStart);

      if Sym(psoSemicolon) then
      begin
        Step;
      end;
    end;
    EditSelectorRow(LSelector).Value.FComplete := SelectorRow(LSelector).Value.FirstArm >= 0;
    EditSelectorRow(LSelector).Value.FSpan := SpanFrom(LStart);

    if not SelectorRow(LSelector).Value.Complete then
    begin
      Unsupported(AScope, 'A variant selector needs at least one alternative', Tok.Span);
    end;
  finally
    FActiveArm := LParent;
    Leave;
  end;
end;

procedure TSyntaxSnapshot.Variables(AScope: Integer; AKind: TNyxPascalSyntaxDeclarationKind;
  AVisibility: TNyxPascalSyntaxVisibility; AClass: Boolean; AArmEnd: Boolean; AThreadLocal: Boolean);
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
      Include(EditEntry(LEntry).Value.FFlags, psdfClassMember);
    end;

    if AThreadLocal then
    begin
      Include(EditEntry(LEntry).Value.FFlags, psdfThreadLocal);
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
    EditEntry(LEntry).Value.FTypeSpan := LTypeSpan;
    EditEntry(LEntry).Value.FExpressionSpan := LExpressionSpan;
    EditEntry(LEntry).Value.FForm := LForm;

    if ((LTypeSpan.Length = 0) and (LExpressionSpan.Length = 0)) or
      (LInitialized and (LExpressionSpan.Length = 0)) then
    begin
      Include(EditEntry(LEntry).Value.FFlags, psdfIncomplete);
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
      EditEntry(LEntries[LAt]).Value.FLinkage := Entry(LEntries[0]).Value.FLinkage;
      EditEntry(LEntries[LAt]).Value.FLinkageSpan := Entry(LEntries[0]).Value.FLinkageSpan;

      if psdfIncomplete in Entry(LEntries[0]).Value.Flags then
      begin
        Include(EditEntry(LEntries[LAt]).Value.FFlags, psdfIncomplete);
      end;
    end;
  end;
  for LAt := 0 to High(LEntries) do
  begin
    EditEntry(LEntries[LAt]).Value.FHeaderSpan := SpanFrom(LStart);
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
    EditEntry(LEntry).Value.FTypeSpan := Range(LType, FPosition);
  end;

  if Sym(psoEqual) then
  begin
    Step;
    LExpression := FPosition;
    Expression(psoSemicolon, False, True);
    EditEntry(LEntry).Value.FExpressionSpan := Range(LExpression, FPosition);
  end
  else
  begin
    Include(EditEntry(LEntry).Value.FFlags, psdfIncomplete);
    Problem(psiExpectedSeparator, 'A constant needs its value expression', Tok.Span);
  end;
  DeclarationHints(LEntry);
  Separator;
  while DeclarationLinkage(LEntry, False) do
  begin
    { Typed constants may also carry native linkage without changing visibility. }
  end;
  EditEntry(LEntry).Value.FHeaderSpan := SpanFrom(LStart);
end;

function TSyntaxSnapshot.Parameters(AScope: Integer; AClose: TNyxPascalSymbol): Boolean;
var
  LEntries: TOrdinals;
  LEntry: Integer;
  LType: Integer;
  LMode: TNyxPascalSyntaxParameterMode;
  LAt: Integer;
begin
  Result := False;
  Step;
  while not EOF and not Sym(AClose) do
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
      EditEntry(LEntry).Value.FParameterMode := LMode;
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
      while not EOF and not Sym(psoSemicolon) and not Sym(AClose) and not Sym(psoEqual) do
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
        EditEntry(LEntries[LAt]).Value.FTypeSpan := Range(LType, FPosition);
      end;
    end;

    if Sym(psoEqual) then
    begin
      Step;
      while not EOF and not Sym(psoSemicolon) and not Sym(AClose) do
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

  if Sym(AClose) then
  begin
    Step;
    Result := True;
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
  LRoutineKind: TNyxPascalRoutineKind;
  LOperator: TNyxPascalOperatorKind;
  LResultEntry: Integer;
  LFullName: TNyxText;
  LOwner: TNyxText;
  LGenericSpan: TNyxPascalSpan;
  LHint: TNyxPascalHintKind;
  LConvention: TNyxPascalRoutineConvention;
  LIsConvention: Boolean;
begin
  Enter;
  try
    LStart := Tok.Span.Start;
    LIsFunction := Word(pkwFunction);
    LIsOperator := Word(pkwOperator);
    LRoutineKind := prkProcedure;

    if LIsFunction then LRoutineKind := prkFunction;

    if LIsOperator then LRoutineKind := prkOperator;

    if Word(pkwConstructor) then LRoutineKind := prkConstructor;

    if Word(pkwDestructor) then LRoutineKind := prkDestructor;
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
      { A qualifier's arguments belong to that owner, not to the routine's own
        parameter declaration. Only angles after the final name occupy this
        role; the complete qualified spelling remains in NameSpan/HeaderSpan. }
      LGenericSpan := NyxPascalSpan(Tok.Span.Start, 0);
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
    EditEntry(LEntry).Value.FGenericSpan := LGenericSpan;
    EditEntry(LEntry).Value.FOwnerName := LOwner;
    EditEntry(LEntry).Value.FRoutineKind := LRoutineKind;

    if not LIsOperator then
    begin
      EditEntry(LEntry).Value.FNameSpan := Range(LName, FPosition);
    end;

    if LIsOperator then
    begin
      Include(EditEntry(LEntry).Value.FFlags, psdfOverload);

      if LOperator = pokUnknown then
      begin
        Include(EditEntry(LEntry).Value.FFlags, psdfUnsupported);
      end;
    end;

    if AClass then
    begin
      Include(EditEntry(LEntry).Value.FFlags, psdfClassMember);
    end;
    LScope := AddScope(psskRoutine, AScope, LEntry, LStart, LFullName);
    EditEntry(LEntry).Value.FNestedScope := LScope;

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
      EditEntry(LEntry).Value.FTypeSpan := Range(LType, FPosition);

      if LResultEntry >= 0 then
      begin
        EditEntry(LResultEntry).Value.FTypeSpan := Entry(LEntry).Value.FTypeSpan;
      end;
    end;
    DeclarationHints(LEntry);

    if Sym(psoSemicolon) or (not RoutineClause(AScope) and not Sym(psoOpenBracket)) then
    begin

      if not Sym(psoSemicolon) then
      begin
        Include(EditEntry(LEntry).Value.FFlags, psdfIncomplete);
      end;
      Separator;
    end;
    LHasBody := not (AVisibility in [psavInterface]) and
      (Frame(AScope).Value.Kind <> psskMembers);
    while not EOF do
    begin

      if DeclarationLinkage(LEntry, True) then
      begin

        if (plfExternal in Entry(LEntry).Value.Linkage) or
          (plfIntrinsic in Entry(LEntry).Value.Linkage) then
        begin
          LHasBody := False;
          Include(EditEntry(LEntry).Value.FFlags, psdfForward);
        end;
        Continue;
      end;

      LIsConvention := RoutineConvention(LConvention);

      if not RoutineModifier(Tok.Word) and not HintWord(LHint) and not LIsConvention then
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
        Include(EditEntry(LEntry).Value.FFlags, psdfForward);
      end;

      if Word(pkwOverload) then
      begin
        Include(EditEntry(LEntry).Value.FFlags, psdfOverload);
      end;

      if Word(pkwStatic) then
      begin
        Include(EditEntry(LEntry).Value.FFlags, psdfStatic);
      end;

      if LIsConvention then
      begin
        SetRoutineConvention(LEntry, LConvention, Tok.Span);
      end;
      Step;

      if Sym(psoSemicolon) then
      begin
        Step;
      end
      else if not RoutineClause(AScope) and not Sym(psoOpenBracket) then
      begin
        Include(EditEntry(LEntry).Value.FFlags, psdfIncomplete);
        Separator;
      end;
    end;
    EditEntry(LEntry).Value.FHeaderSpan := SpanFrom(LStart);

    if LHasBody then
    begin
      Declarations(LScope, psavLocal, False);

      if Word(pkwBegin) or Word(pkwAsm) then
      begin

        if not Block(LScope) then
        begin
          Include(EditEntry(LEntry).Value.FFlags, psdfIncomplete);
        end;
        Separator;
      end
      else
      begin
        Include(EditEntry(LEntry).Value.FFlags, psdfIncomplete);
        Problem(psiUnclosedScope, 'Finish this routine implementation', Tok.Span);
      end;
    end;
    FinishScope(LScope, not (psdfIncomplete in Entry(LEntry).Value.Flags));
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
  LParameters: Integer;
  LScope: Integer;
  LAccessor: TNyxPascalPropertyAccess;
  LClosed: Boolean;
  LClauseStart: Integer;
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
    Include(EditEntry(LEntry).Value.FFlags, psdfClassMember);
  end;
  Step;

  if Sym(psoOpenBracket) then
  begin
    LParameters := FPosition;
    LScope := AddScope(psskPropertyParameters, AScope, LEntry, Tok.Span.Start, Entry(LEntry).Value.Name);
    EditEntry(LEntry).Value.FNestedScope := LScope;
    LClosed := Parameters(LScope, psoCloseBracket);
    EditEntry(LEntry).Value.FPropertyParameters := Range(LParameters, FPosition);
    FinishScope(LScope, LClosed);

    if not LClosed then Include(EditEntry(LEntry).Value.FFlags, psdfIncomplete);
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
    EditEntry(LEntry).Value.FTypeSpan := Range(LType, FPosition);
    EditEntry(LEntry).Value.FForm := pstfNamed;
  end;
  DeclarationHints(LEntry);
  LClauseStart := FPosition;
  while not EOF and not Sym(psoSemicolon) and not Word(pkwEnd) and
    not Word(pkwImplementation) and not Word(pkwBegin) do
  begin

    if AtHint(LClauseStart) then Break;

    if not Tok.Complete then
    begin
      Include(EditEntry(LEntry).Value.FFlags, psdfIncomplete);
      Step;
      Break;
    end;

    if Word(pkwRead) or Word(pkwWrite) then
    begin
      LAccessor := pspaRead;

      if Word(pkwWrite) then LAccessor := pspaWrite;
      Include(EditEntry(LEntry).Value.FPropertyAccess, LAccessor);
      Step;

      if Tok.Kind <> ptkWord then
      begin
        Include(EditEntry(LEntry).Value.FFlags, psdfIncomplete);
        Problem(psiExpectedName, 'A property accessor needs its declared name', Tok.Span);
        Break;
      end;
      Step;
      while Sym(psoDot) and (Tok(1).Kind = ptkWord) do
      begin
        Step;
        Step;
      end;
    end
    else if Sym(psoOpenParen) then Groups(psoOpenParen, psoCloseParen)
    else if Sym(psoOpenBracket) then Groups(psoOpenBracket, psoCloseBracket)
    else if Sym(psoAlternateOpenBracket) then Groups(psoAlternateOpenBracket, psoAlternateCloseBracket)
    else Step;
  end;
  DeclarationHints(LEntry);
  Separator;

  if Word(pkwDefault) or Word(pkwNodefault) then
  begin
    EditEntry(LEntry).Value.FDefaultProperty := Word(pkwDefault);
    Step;
    Separator;
  end;
  EditEntry(LEntry).Value.FHeaderSpan := SpanFrom(LStart);
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


    EditImportRow(LEntry).Value.FName := LName;
    EditImportRow(LEntry).Value.FSpan := Range(LStart, FPosition);
    EditImportRow(LEntry).Value.FScope := AScope;
    EditImportRow(LEntry).Value.FConditional := FTokens.IsConditional(LStart);
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
  AVisibility: TNyxPascalSyntaxVisibility; AMembers: Boolean; ASection: TDeclarationSection);
var
  LSection: TDeclarationSection;
  LBefore: Integer;
  LClass: Boolean;
  LGeneric: Boolean;
  LStrict: Boolean;
begin
  Enter;
  try
    LSection := ASection;
    while not EOF and not Word(pkwEnd) and not Word(pkwBegin) and not Word(pkwAsm) and
      not Word(pkwInitialization) and not Word(pkwFinalization) do
    begin
      LBefore := FPosition;

      if AScope = 0 then
      begin
        Remember(prDeclarations, AVisibility, AMembers, LSection);
      end;

      if Word(pkwInterface) then
      begin
        AVisibility := psavInterface;
        LSection := pdsNone;
        Step;
      end
      else if Word(pkwImplementation) then
      begin

        if AScope <> 0 then
        begin
          Break;
        end;
        AVisibility := psavImplementation;
        FImplementationSeen := True;
        LSection := pdsNone;
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
        LSection := pdsNone;
      end
      else if Tok.Word in [pkwVar, pkwThreadvar, pkwConst, pkwResourcestring] then
      begin

        if Word(pkwVar) then LSection := pdsVariables
        else if Word(pkwThreadvar) then LSection := pdsThreadVariables
        else if Word(pkwConst) then LSection := pdsConstants
        else LSection := pdsResources;
        Step;
      end
      else if Word(pkwLabel) and not AMembers then
      begin
        StatementLabels(AScope);
        LSection := pdsNone;
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
        LSection := pdsNone;
      end
      else if Tok.Word in [pkwPrivate, pkwProtected, pkwPublic, pkwPublished,
        pkwAutomated, pkwStrict] then
      begin
        Unsupported(AScope, 'Visibility sections require an enclosing member scope', Tok.Span);
        Step;
        LSection := pdsNone;
      end
      else if Word(pkwProperty) then
      begin
        PropertyDeclaration(AScope, AVisibility);
        LSection := pdsNone;
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

        if (Word(pkwVar) or Word(pkwThreadvar)) and LClass and not LGeneric and AMembers then
        begin
          LSection := pdsClassVariables;

          if Word(pkwThreadvar) then LSection := pdsClassThreadVariables;
          Step;
          Variables(AScope, psdkField, AVisibility, True, False,
            LSection = pdsClassThreadVariables);
          { The whole section owns this modifier, not just its first name group.
            Visibility, another section or a routine ends the section normally. }
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
        LSection := pdsNone;
      end
      else if Sym(psoOpenBracket) then
      begin
        Groups(psoOpenBracket, psoCloseBracket);
      end
      else if AMembers and Word(pkwCase) then
      begin

        if (Frame(AScope).Value.OwnerDeclaration >= 0) and
          (Entry(Frame(AScope).Value.OwnerDeclaration).Value.TypeForm = pstfRecord) then
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
          pdsClassVariables, pdsClassThreadVariables:
            begin
              Variables(AScope, psdkField, AVisibility, True, False,
                LSection = pdsClassThreadVariables);
            end;
          pdsVariables, pdsThreadVariables:
            begin
              Variables(AScope, psdkVariable, AVisibility, False, False,
                LSection = pdsThreadVariables);
            end;
          pdsConstants:
            begin
              Constants(AScope, psdkConstant, AVisibility);
            end;
          pdsResources:
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
  LToken: TNyxPascalToken;
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
      { All dispatch tests inspect the same cursor before Step. Keep one local
        value instead of rereading it for each alternative. EOF retains its
        final lookahead after a closing END; Step retains cursor/cancellation
        behavior. Checkpoint read frontiers keep their ordinary boundaries. }
      LToken := Tok;
      case LToken.Word of
        pkwBegin, pkwTry, pkwCase, pkwAsm:
          begin
            Inc(LDepth);

            if LDepth > FOptions.MaximumDepth then
            begin
              raise ENyxPascalSyntaxLimit.Create('Pascal body nesting exceeds its budget');
            end;
          end;
        pkwRepeat, pkwUntil, pkwEnd:
          begin
            { These alternatives previously used Word rather than the raw word
              field. Preserve its kind/escape guards independently of the other
              branches, including opaque assembly and escaped identifiers. }

            if (LToken.Kind = ptkWord) and not LToken.Escaped then
            begin
              case LToken.Word of
                pkwRepeat: Inc(LRepeat);
                pkwUntil:
                  begin

                    if LRepeat > 0 then
                    begin
                      Dec(LRepeat);
                    end;
                  end;
                pkwEnd: Dec(LDepth);
              else
                begin
                  { The enclosing dispatch admits only these three words. }
                end;
              end;
            end;
          end;
        pkwProcedure, pkwFunction:
          begin
            Unsupported(LScope, 'Anonymous routine scopes need expression-context analysis', LToken.Span);
          end;
        pkwVar, pkwConst, pkwType:
          begin
            Unsupported(LScope, 'Inline declarations need statement-scope analysis', LToken.Span);
          end;
      else
        begin
          { Ordinary expression tokens leave structural body depth unchanged. }
        end;
      end;
      Step;
    end;
    FinishScope(LScope, (LDepth = 0) and (LRepeat = 0));
    Result := Frame(LScope).Value.Complete;

    if not Frame(LScope).Value.Complete then
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

procedure TSyntaxSnapshot.BuildTokens(APrevious: TSyntaxSnapshot; ARetained: Integer);
var
  LAt: Integer;
  LFirst: Integer;
  LSafe: Integer;
  LPrevious: TFilteredTokens;
  LRetainedRows: Integer;
  LDepth: Integer;
  LDirective: TNyxText;
  LToken: TNyxPascalToken;
  LConditionalReported: Boolean;
  LIncludeReported: Boolean;
begin
  LFirst := 0;
  LSafe := FLexical.Count - 3;

  if LSafe < 0 then
  begin
    LSafe := 0;
  end;
  LPrevious := nil;
  LRetainedRows := 0;
  LDepth := 0;
  LConditionalReported := False;
  LIncludeReported := False;

  if (APrevious <> nil) and
    (APrevious.FFilterCheckpoint.Lexical <= ARetained) and
    (APrevious.FFilterCheckpoint.Lexical <= LSafe) then
  begin
    LFirst := APrevious.FFilterCheckpoint.Lexical;
    LPrevious := APrevious.FTokens;
    LRetainedRows := APrevious.FFilterCheckpoint.SignificantCount;
    LDepth := APrevious.FFilterCheckpoint.Depth;
    LConditionalReported := APrevious.FFilterCheckpoint.ConditionalReported;
    LIncludeReported := APrevious.FFilterCheckpoint.IncludeReported;
    FDiagnosticCount := APrevious.FFilterCheckpoint.Diagnostics;
    SetLength(FDiagnostics, FDiagnosticCount);
    for LAt := 0 to FDiagnosticCount - 1 do
    begin
      CheckCancellation;
      NyxPascalAdmitAnalysis(FCancellation, papDiagnosticRow);
      FDiagnostics[LAt] := APrevious.FDiagnostics[LAt];
      Inc(FCopied);
    end;
  end;
  FTokens := TFilteredTokens.Create(LPrevious, LRetainedRows, FCancellation);
  FFilterRetained := LFirst;
  for LAt := LFirst to FLexical.Count - 1 do
  begin

    if ((LAt - LFirst) mod 1024 = 0) and (FCancellation <> nil) and FCancellation.Cancelled then
    begin
      raise ENyxPascalAnalysisCancelled.Create('Pascal syntax analysis cancelled');
    end;

    if LAt = LSafe then
    begin
      FFilterCheckpoint.Lexical := LAt;
      FFilterCheckpoint.SignificantCount := FTokens.Count;
      FFilterCheckpoint.Depth := LDepth;
      FFilterCheckpoint.ConditionalReported := LConditionalReported;
      FFilterCheckpoint.IncludeReported := LIncludeReported;
      FFilterCheckpoint.Diagnostics := FDiagnosticCount;
    end;
    NyxPascalAdmitAnalysis(FCancellation, papFilterToken);
    LToken := FLexical.Token(LAt);
    Inc(FFilteredReads);

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
      FTokens.Add(LAt, LDepth > 0);
    end;
  end;

  if LDepth > 0 then
  begin
    Problem(psiUnterminatedConditional, 'Finish this conditional compilation block',
      LToken.Span);
  end;
  FTokens.Seal;
end;

procedure TSyntaxSnapshot.Build(APrevious: TSyntaxSnapshot; ARetained: Integer);
var
  LResume: TParserCheckpoint;
begin
  BuildTokens(APrevious, ARetained);
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
    FModuleUnit := Word(pkwUnit);
    Step;
    LModule := Name;
    Step;
    while Sym(psoDot) and (Tok(1).Kind = ptkWord) do
    begin
      Step;
      LModule := LModule + '.' + Name;
      Step;
    end;
    EditFrame(LModuleScope).Value.FName := LModule;

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

{ Structural executable ownership shares the ordinary scope/metadata and token
  admission owners. It does not evaluate expressions or admit compiler-specific
  inline/anonymous declarations. Unlike a counter-only scan, the bounded stack
  preserves delimiter order, separate lifecycle regions and exact group failures. }
function TSyntaxSnapshot.ModuleSection(AKind: TNyxPascalSyntaxScopeKind): Boolean;
var
  LFrames: array of TExecutableFrame;
  LCount: Integer;
  LSection: Integer;
  LScope: Integer;
  LToken: TNyxPascalToken;
  LTop: TExecutableFrame;
  LAt: Integer;

  procedure Push(AWord: TNyxPascalWord; AClose: TNyxPascalSymbol);
  var
    LCapacity: Integer;
    LNewScope: Integer;
  begin

    if LCount >= FOptions.MaximumDepth then
    begin
      raise ENyxPascalSyntaxLimit.Create('Pascal module body nesting exceeds its budget');
    end;

    if LCount = Length(LFrames) then
    begin
      LCapacity := Length(LFrames);

      if LCapacity = 0 then LCapacity := 1
      else if LCapacity > FOptions.MaximumDepth div 2 then LCapacity := FOptions.MaximumDepth
      else LCapacity := LCapacity * 2;
      SetLength(LFrames, LCapacity);
    end;
    LNewScope := -1;

    if AWord <> pkwNone then
    begin
      LNewScope := AddScope(psskBlock, LScope, -1, LToken.Span.Start, '');
    end;
    LFrames[LCount].OpenWord := AWord;
    LFrames[LCount].CloseSymbol := AClose;
    LFrames[LCount].Scope := LNewScope;
    LFrames[LCount].ParentScope := LScope;
    LFrames[LCount].HandlerSeen := False;
    Inc(LCount);

    if LNewScope >= 0 then
    begin
      LScope := LNewScope;
    end;
    Step;
  end;

  procedure Pop;
  begin
    Dec(LCount);

    if LFrames[LCount].Scope >= 0 then
    begin
      FinishScope(LFrames[LCount].Scope, True);
    end;
    LScope := LFrames[LCount].ParentScope;
  end;

begin
  Enter;
  try
    Result := False;
    LCount := 0;
    LSection := AddScope(AKind, 0, -1, Tok.Span.Start, '');
    LScope := LSection;
    Step;
    while not EOF do
    begin
      LToken := Tok;

      if (LCount > 0) and (LFrames[LCount - 1].OpenWord = pkwAsm) then
      begin
        { Assembly text is opaque to Pascal statement keywords. Only its END
          closes this frame; strings and escaped words cannot impersonate it. }
        Step;

        if (LToken.Kind = ptkWord) and not LToken.Escaped and (LToken.Word = pkwEnd) then
        begin
          Pop;
        end;
        Continue;
      end;

      if LToken.Kind = ptkSymbol then
      begin
        case LToken.Symbol of
          psoOpenParen: begin Push(pkwNone, psoCloseParen); Continue; end;
          psoOpenBracket: begin Push(pkwNone, psoCloseBracket); Continue; end;
          psoAlternateOpenBracket: begin Push(pkwNone, psoAlternateCloseBracket); Continue; end;
          psoCloseParen, psoCloseBracket, psoAlternateCloseBracket:
            begin

              if (LCount = 0) or (LFrames[LCount - 1].CloseSymbol <> LToken.Symbol) then
              begin
                Problem(psiUnclosedScope, 'This closing group does not match its module body opener', LToken.Span);
                Break;
              end;
              Step;
              Pop;
              Continue;
            end;
        else
          begin
            { Ordinary expression punctuation leaves the delimiter stack intact. }
          end;
        end;
      end;

      if (LToken.Kind = ptkWord) and not LToken.Escaped then
      begin

        if LToken.Word in [pkwInitialization, pkwFinalization] then
        begin

          if (LCount = 0) and (AKind = psskInitialization) and (LToken.Word = pkwFinalization) then
          begin
            Result := True;
          end
          else
          begin
            Unsupported(LScope, 'A lifecycle section cannot repeat or occur inside another executable construct', LToken.Span);
          end;
          Break;
        end;
        case LToken.Word of
          pkwBegin, pkwCase, pkwTry, pkwRepeat, pkwAsm:
            begin
              Push(LToken.Word, psoNone);
              Continue;
            end;
          pkwEnd, pkwUntil:
            begin

              if (LCount = 0) and (LToken.Word = pkwEnd) then
              begin
                Result := True;
                Break;
              end;

              if LCount = 0 then
              begin
                Problem(psiUnclosedScope, 'UNTIL needs its enclosing REPEAT', LToken.Span);
                Break;
              end;
              LTop := LFrames[LCount - 1];

              if (LTop.CloseSymbol <> psoNone) or
                ((LTop.OpenWord = pkwRepeat) <> (LToken.Word = pkwUntil)) then
              begin
                Problem(psiUnclosedScope, 'This executable closer does not match its module body opener', LToken.Span);
                Break;
              end;

              if (LTop.OpenWord = pkwTry) and not LTop.HandlerSeen then
              begin
                Unsupported(LScope, 'TRY needs its EXCEPT or FINALLY handler', LToken.Span);
              end;
              Step;
              Pop;

              if LToken.Word = pkwUntil then
              begin

                if EOF or Word(pkwEnd) or Word(pkwInitialization) or Word(pkwFinalization) or
                  Sym(psoSemicolon) then
                begin
                  Problem(psiExpectedName, 'UNTIL needs its condition expression', Tok.Span);
                end;
              end;
              Continue;
            end;
          pkwExcept, pkwFinally:
            begin

              if (LCount = 0) or (LFrames[LCount - 1].OpenWord <> pkwTry) or
                LFrames[LCount - 1].HandlerSeen then
              begin
                Unsupported(LScope, 'An exception handler needs its own unmatched TRY', LToken.Span);
              end
              else
              begin
                LFrames[LCount - 1].HandlerSeen := True;
              end;
            end;
          pkwProcedure, pkwFunction, pkwVar, pkwConst, pkwType, pkwLabel, pkwThreadvar,
          pkwResourcestring, pkwInterface, pkwImplementation:
            begin
              Unsupported(LScope, 'Declarations inside executable regions need statement/expression context', LToken.Span);
            end;
        else
          begin
            { References and expression words do not declare module symbols. }
          end;
        end;
      end;
      Step;
    end;
    for LAt := 0 to LCount - 1 do
    begin

      if LFrames[LAt].Scope >= 0 then
      begin
        FinishScope(LFrames[LAt].Scope, False);
      end;
    end;
    FinishScope(LSection, Result);

    if not Result then
    begin
      Problem(psiUnclosedScope, 'Finish this module executable section and its nested constructs', Tok.Span);
    end;
  finally
    Leave;
  end;
end;

procedure TSyntaxSnapshot.FinishRoot;
var
  LBodyClosed: Boolean;
  LSectionClosed: Boolean;
begin
  LBodyClosed := False;

  if FModuleUnit and FImplementationSeen and
    (Word(pkwBegin) or Word(pkwInitialization) or Word(pkwFinalization)) then
  begin
    LSectionClosed := True;

    if not Word(pkwFinalization) then
    begin
      LSectionClosed := ModuleSection(psskInitialization);
    end;

    if LSectionClosed and Word(pkwFinalization) then
    begin
      ModuleSection(psskFinalization);
    end;
  end
  else if Word(pkwBegin) or Word(pkwAsm) then
  begin
    LBodyClosed := Block(0);
  end
  else if Word(pkwInitialization) or Word(pkwFinalization) then
  begin
    Unsupported(0, 'Lifecycle sections require an explicit unit implementation context', Tok.Span);
  end;
  { A physical fragment closes at EOF. A complete module needs its final end-dot
    boundary. Structural closure grants no statement meaning or compiler admission. }

  if FOptions.StartingContext <> psfModule then
  begin
    EditFrame(0).Value.FComplete := EOF;
  end
  else
  begin
    EditFrame(0).Value.FComplete :=
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
  Result := Frame(AIndex).Value;

  if not Result.Complete then
  begin
    Result.FSpan := NyxPascalSpan(Result.Span.Start, FLexical.Scalars - Result.Span.Start);
  end;
end;

function TSyntaxSnapshot.ScopeAt(AOffset: Integer): Integer;
var
  LAt: Integer;
  LDepth: Integer;
  LBestDepth: Integer;
  LParent: Integer;
  LScope: TNyxPascalSyntaxScope;
begin

  if (AOffset < 0) or (AOffset > FLexical.Scalars) then
  begin
    raise ENyxPascalLexical.Create('Pascal scope position is outside its syntax snapshot');
  end;
  Result := 0;
  LBestDepth := 0;
  for LAt := 1 to FScopeCount - 1 do
  begin
    LScope := Scope(LAt);

    if (AOffset >= LScope.Span.Start) and
      ((AOffset < LScope.Span.Finish) or
        ((AOffset = FLexical.Scalars) and not LScope.Complete)) then
    begin
      LDepth := 0;
      LParent := LAt;
      while LParent >= 0 do
      begin
        Inc(LDepth);
        LParent := Frame(LParent).Value.Parent;
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
  Result := Entry(AIndex).Value;
end;

function TSyntaxSnapshot.Named(AScope: Integer; const AName: TNyxText): Integer;
begin
  Scope(AScope);
  Result := FMetadata.Index(mkName, TNyxText(IntToStr(AScope)) + ':' + NyxPascalASCIIKey(AName));

  if (Result >= 0) and (Entry(Result).Value.OperatorKind <> pokNone) then
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
  Result := FMetadata.Index(mkName, TNyxText(IntToStr(AScope)) + ':operator#' + TNyxText(IntToStr(Ord(AKind))));
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
  Result := SelectorRow(AIndex).Value;
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
  Result := ArmRow(AIndex).Value;
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
  Result := VariantLabelRow(AIndex).Value;
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
  Result := StatementLabelRow(AIndex).Value;
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
  Result := HintRow(AIndex).Value;
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
  Result := IntrinsicRow(AIndex).Value;
end;

function TSyntaxSnapshot.TypeAt(AStart: Integer): Integer;
begin

  if (AStart < 0) or (AStart > FLexical.Scalars) then
  begin
    raise ENyxPascalLexical.Create('Structured type position is outside its syntax snapshot');
  end;
  Result := FMetadata.Index(mkStructure, TNyxText(IntToStr(AStart)));
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
  Result := ImportRow(AIndex).Value;
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
  Result := FCopied + FMetadata.Copied;
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

function TSyntaxSnapshot.FilteredTokenReads: Integer;
begin
  Result := FFilteredReads;
end;

function TSyntaxSnapshot.ReusedFilterTokens: Integer;
begin
  Result := FFilterRetained;
end;

function TSyntaxSnapshot.SharedFilterRows: Integer;
begin
  Result := FTokens.Shared;
end;

function TSyntaxSnapshot.CopiedFilterRows: Integer;
begin
  Result := FTokens.Copied;
end;

function TSyntaxSnapshot.FilterDirectoryEntries: Integer;
begin
  Result := FTokens.DirectoryEntries;
end;

function TSyntaxSnapshot.RetainedMetadataValues: Integer;
begin
  Result := FRetainedMetadata;
end;

function TSyntaxSnapshot.CopiedMetadataValues: Integer;
begin
  Result := FMetadata.Copied;
end;

function TSyntaxSnapshot.CreatedMetadataValues: Integer;
begin
  Result := FMetadata.Created;
end;

function TSyntaxSnapshot.AllocatedMetadataNodes: Integer;
begin
  Result := FMetadata.AllocatedNodes;
end;

function TSyntaxSnapshot.CopiedDiagnosticRows: Integer;
begin
  Result := FCopied;
end;

function TSyntaxSnapshot.MetadataTreeHeight: Integer;
begin
  Result := FMetadata.Height;
end;

function TSyntaxSnapshot.MetadataLookups: Integer;
begin
  Result := FMetadata.ReadLookups;
end;

function TSyntaxSnapshot.MetadataLookupHits: Integer;
begin
  Result := FMetadata.ReadHits;
end;

function TSyntaxSnapshot.MetadataLookupTreeReads: Integer;
begin
  Result := FMetadata.ReadTreeVisits;
end;

function TSyntaxSnapshot.MetadataLookupPeak: Integer;
begin
  Result := FMetadata.ReadPeak;
end;

function TSyntaxSnapshot.RetainedMetadataLookups: Integer;
begin
  Result := FMetadata.RetainedReads;
end;

function TSyntaxSnapshot.OrdinalMetadataLookups: Integer;
begin
  Result := FMetadata.OrdinalLookups;
end;

function TSyntaxSnapshot.OrdinalMetadataPageReads: Integer;
begin
  Result := FMetadata.OrdinalReads;
end;

function TSyntaxSnapshot.AllocatedOrdinalPages: Integer;
begin
  Result := FMetadata.OrdinalNodes;
end;

function TSyntaxSnapshot.CapturedMetadataRoots: Integer;
begin
  Result := FMetadata.Captures;
end;

function TSyntaxSnapshot.OrdinalMetadataDepth: Integer;
begin
  Result := FMetadata.OrdinalDepth;
end;

function NyxPascalDeclarationIdentity(const ASource: INyxPascalSyntaxSnapshot;
  AIndex: Integer): IInterface;
var
  LOwner: ISyntaxOwner;
begin
  Result := nil;

  if Supports(ASource, ISyntaxOwner, LOwner) then
  begin
    ASource.Declaration(AIndex);
    Result := LOwner.Snapshot.Entry(AIndex).Identity;
  end;
end;

function NyxPascalScopeIdentity(const ASource: INyxPascalSyntaxSnapshot;
  AIndex: Integer): IInterface;
var
  LOwner: ISyntaxOwner;
begin
  Result := nil;

  if Supports(ASource, ISyntaxOwner, LOwner) then
  begin
    ASource.Scope(AIndex);
    Result := LOwner.Snapshot.Frame(AIndex).Identity;
  end;
end;

end.
