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
unit nyx.pascal.binding;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.scheduler, nyx.pascal.lexing, nyx.pascal.context,
  nyx.pascal.syntax, nyx.pascal.types, nyx.pascal.syntax.translated,
  nyx.pascal.requirements, nyx.pascal.values, nyx.pascal.reals, nyx.pascal.ieee;

type
  { Invalid snapshot/site references are caller failures. Semantic missing or
    unsupported source meaning is returned as a typed resolution, never executed. }
  ENyxPascalBinding = class(EArgumentException);
  { Query budgets refuse the entire response, independently of missing names.
    No partial symbol list is presented as a complete semantic search. }
  ENyxPascalBindingLimit = class(ENyxPascalBinding);
  { Definite disagreement between captured module/compiler evidence. This is
    an unavailable project context, not an internal resolver failure or proof
    that incomplete snapshots otherwise establish a coherent target/ABI. }
  ENyxPascalModuleContext = class(ENyxPascalBinding)
  private
    FRelatedModule: TNyxText;
  public
    constructor Create(const ARelatedModule: TNyxText);
    property RelatedModule: TNyxText read FRelatedModule;
  end;
  { Bound establishes supported declaration/name meaning, not successful compiler
    admission. Overloads retain alternatives without selecting a call signature. }
  TNyxPascalBindingStatus = (pbsBound, pbsOverloads, pbsMissing, pbsUnresolved,
    pbsInaccessible, pbsAmbiguous);
  { Explicit missing context/meaning. Compiler/SDK gaps never silently fall through
    to an earlier import or an invented member/builtin definition. }
  TNyxPascalBindingIssue = (pbiNone, pbiMissingName, pbiMissingModule,
    pbiVisibility, pbiUnsupportedScope, pbiIncompleteSource, pbiUnsupportedType,
    pbiGenericContext, pbiMissingType, pbiRecursiveType, pbiCompilerContext,
    pbiWrongMemberKind, pbiAmbiguousName, pbiOverloadContext,
    pbiGenericArity, pbiGenericConstraint);
  { Type definitions and constructed shapes remain distinct. Alias and distinct
    wrappers preserve declared identity; Target retains the underlying shape.
    Qualified compatibility queries do not change this nominal identity or
    infer numeric bounds/defaults, layout or general assignment legality. Encoded
    strings preserve their opaque source operand and qualified ANSI Target;
    existing plain-string folding/conversion rules do not acquire this family. }
  TNyxPascalSemanticTypeKind = (pstDeclared, pstAlias, pstDistinct, pstPointer,
    pstArray, pstSet, pstFile, pstString, pstClassOf, pstProcedure, pstFunction,
    pstReference, pstEnumeration, pstSubrange, pstBuiltin, pstUntyped, pstFormal,
    pstEncodedString);
  { Authored formal requirements grant only their declared type contract. They
    do not identify an actual argument, layout or constructor invocation. }
  TNyxPascalGenericRequirement = (pgrClass, pgrRecord, pgrConstructor);
  { Independent source requests; an empty set means unconstrained, not TObject. }
  TNyxPascalGenericRequirements = set of TNyxPascalGenericRequirement;
  { Language intrinsic names only. This never supplies hardcoded SDK members or
    hides a later uncaptured import. Sizes/ABI remain in captured compiler context. }
  TNyxPascalBuiltin = (pbbNone, pbbInteger, pbbByte, pbbWord, pbbSmallInt,
    pbbShortInt, pbbLongInt, pbbLongWord, pbbInt64, pbbQWord, pbbNativeInt,
    pbbNativeUInt, pbbBoolean, pbbByteBool, pbbWordBool, pbbLongBool,
    pbbChar, pbbAnsiChar, pbbWideChar, pbbSingle, pbbDouble, pbbExtended,
    pbbReal, pbbCurrency, pbbPointer, pbbAnsiString, pbbUTF8String,
    pbbUnicodeString, pbbWideString, pbbShortString, pbbComp);
  { Declaration-only callers may retain symbolic intrinsic classification.
    Compiler-unit policy instead resolves the implicit import chain, refusing a
    missing unit exactly like a missing explicit import. No SDK stub is created. }
  TNyxPascalImplicitScopePolicy = (pispDeclarationNames, pispCompilerUnits);
  { Symbolic, imported-System or compiler-root identity. Bootstrap evidence
    precedes authored aliases; equal spelling/category never establishes identity. }
  TNyxPascalIntrinsicOrigin = (pioUnqualified, pioSystem, pioSystemBootstrap);
  { Numeric storage meaning, independent of a declared type's nominal identity.
    Currency is scaled signed Int64 (four decimal places); Comp stores an
    integral signed Int64 value through the compiler's floating-type family.
    Neither is an IEEE binary floating format. No alignment/calling ABI follows. }
  TNyxPascalFloatingFormat = (pffUnknown, pffBinary32, pffBinary64,
    pffX87Extended80, pffCurrency64, pffComp64);
  { Literal value families are distinct from their expression types. A string
    constant can have an anonymous array type yet specialize a generic as a
    managed string; neither fact is an assignment-conversion permission. }
  TNyxPascalLiteralKind = (plkInteger, plkBoolean, plkCharacter, plkString, plkReal);
  { Explicit receiver intent; resolving a type name never grants instance access
    to an instance field through a class reference. }
  TNyxPascalMemberAccess = (pmaInstance, pmaType);
  { Open Pascal name reference. Dotted segments are identifiers, not filesystem
    paths or source edits; authored case is retained, comparison is ASCII-insensitive. }
  TNyxPascalBindingName = record
  private
    FName: TNyxText;
  public
    property Name: TNyxText read FName;
  end;
  { Positive immutable world/query limits. Defaults: 512 modules, 64 recursion
    levels, 100000 steps and 50 candidates. Exceeding a limit never truncates. }
  TNyxPascalBindingOptions = record
  private
    FDefined: Boolean;
    FModules: Integer;
    FDepth: Integer;
    FSteps: Integer;
    FCandidates: Integer;
    FImplicitScopes: TNyxPascalImplicitScopePolicy;
  public
    { Low-level declaration inspection retains symbolic-name compatibility.
      Owned completion defaults opt into the compiler-unit policy explicitly. }
    class function Defaults: TNyxPascalBindingOptions; static;
    function UpToModules(AValue: Integer): TNyxPascalBindingOptions;
    function UpToDepth(AValue: Integer): TNyxPascalBindingOptions;
    function UpToSteps(AValue: Integer): TNyxPascalBindingOptions;
    function UpToCandidates(AValue: Integer): TNyxPascalBindingOptions;
    { Select explicit compiler-scope lookup rather than declaration-only names.
      The owned completion engine selects compiler units in its defaults. }
    function WithImplicitScopes(AValue: TNyxPascalImplicitScopePolicy): TNyxPascalBindingOptions;
    procedure Validate;
    property MaximumModules: Integer read FModules;
    property MaximumDepth: Integer read FDepth;
    property MaximumSteps: Integer read FSteps;
    property MaximumCandidates: Integer read FCandidates;
    property ImplicitScopes: TNyxPascalImplicitScopePolicy read FImplicitScopes;
  end;
  { Forward world contract. Results/sites/symbols retain this immutable owner;
    the world retains only source views, so there is no reference-counting cycle. }
  INyxPascalSemanticWorld = interface;
  { Forward site contract, tied to the exact world that created its ordinals. }
  INyxPascalBindingSite = interface;
  { Forward symbol contract, preserving exact world and physical definition. }
  INyxPascalBoundSymbol = interface;
  { Forward semantic shape contract; child references point only outward to data. }
  INyxPascalSemanticType = interface;
  { Forward response contract; no response is cached back into its source world. }
  INyxPascalResolution = interface;
  { Forward immutable substitution frame. Frames and arguments point outward;
    definitions/worlds never retain a frame that refers back to them. }
  INyxPascalGenericEnvironment = interface;

  { Typed arguments for one exact generic declaration, with an optional outer
    lexical frame for nested types. Owner is an unspecialized declaration handle.
    Arguments can be concrete specializations or source-owned formal contracts.
    No constraints are assumed satisfied: unsupported constraint headers refuse
    before a frame is published. Arguments are semantic types, not source strings. }
  INyxPascalGenericEnvironment = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570E06}']
    function Owner: INyxPascalBoundSymbol;
    function Parent: INyxPascalGenericEnvironment;
    function Count: Integer;
    function Name(AIndex: Integer): TNyxText;
    function Argument(AIndex: Integer): INyxPascalSemanticType;
  end;
  { Optional symbol capability retaining concrete receiver substitutions across
    separate member and type queries. The original declaration/origin is unchanged.
    A nil frame means the declaration has no concrete generic context. }
  INyxPascalSubstitutedSymbol = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570E07}']
    function Environment: INyxPascalGenericEnvironment;
  end;
  { Bounded member-page request. Prefix is empty or an ASCII identifier prefix;
    ordering is case-folded name then declaration order within an overload family.
    Offset counts visible candidate rows, not hidden/private declarations. }
  TNyxPascalMemberQuery = record
  private
    FDefined: Boolean;
    FPrefix: TNyxText;
    FOffset: Integer;
    FCount: Integer;
    FAccess: TNyxPascalMemberAccess;
    FCountAll: Boolean;
  public
    class function Defaults: TNyxPascalMemberQuery; static;
    function Matching(const AValue: TNyxText): TNyxPascalMemberQuery;
    function Skip(AValue: Integer): TNyxPascalMemberQuery;
    function Take(AValue: Integer): TNyxPascalMemberQuery;
    function ForAccess(AValue: TNyxPascalMemberAccess): TNyxPascalMemberQuery;
    { Opt in to counting all matching visible rows within the same work budget.
      Responses remain bounded; failure while counting publishes no page. }
    function CountAll(AValue: Boolean = True): TNyxPascalMemberQuery;
    procedure Validate;
    property Prefix: TNyxText read FPrefix;
    property Offset: Integer read FOffset;
    property Limit: Integer read FCount;
    property Access: TNyxPascalMemberAccess read FAccess;
    property IncludesTotal: Boolean read FCountAll;
  end;

  { Checked caller position in one exact immutable world. Caret is in the declared
    analysis projection; use the view's origin map for physical editing/navigation. }
  INyxPascalBindingSite = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570E01}']
    function World: INyxPascalSemanticWorld;
    function ModuleIndex: Integer;
    function ScopeIndex: Integer;
    function Caret: Integer;
  end;
  { One bound declaration, with exact immutable-world ownership. The physical
    origin remains the original included file/occurrence, not a generated buffer. }
  INyxPascalBoundSymbol = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570E02}']
    function World: INyxPascalSemanticWorld;
    function ModuleIndex: Integer;
    function DeclarationIndex: Integer;
    function Declaration: TNyxPascalSyntaxDeclaration;
    function Origin: TNyxPascalSyntaxOrigin;
  end;
  { Bound names within a supported type shape. Target means alias/distinct/
    pointer/array/set/file/class-reference/reference target, or function result.
    Definition is absent for anonymous shapes/intrinsics. Opaque bounds/defaults
    remain on Syntax; this is not compiler type checking or inferred layout.
    Parameter entries retain names/modes and their bound types without interface
    fields in records. Worlds never retain the resulting semantic type graph. }
  INyxPascalSemanticType = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570E03}']
    function World: INyxPascalSemanticWorld;
    function Kind: TNyxPascalSemanticTypeKind;
    function Builtin: TNyxPascalBuiltin;
    function Definition: INyxPascalBoundSymbol;
    function Target: INyxPascalSemanticType;
    function Syntax: INyxPascalTypeSyntax;
    function SyntaxNode: Integer;
    function ParameterCount: Integer;
    function ParameterName(AIndex: Integer): TNyxText;
    function ParameterMode(AIndex: Integer): TNyxPascalSyntaxParameterMode;
    function ParameterType(AIndex: Integer): INyxPascalSemanticType;
  end;
  { Optional intrinsic provenance, independent of inferred ABI/layout. Getters
    require a builtin semantic type and retain its exact captured source context.
    World ownership points outward only; neither world nor context owns results. }
  INyxPascalIntrinsicIdentity = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570E0B}']
    function IntrinsicOrigin: TNyxPascalIntrinsicOrigin;
    function IntrinsicReference: TNyxPascalSizeReference;
    function IntrinsicContext: INyxPascalCompilerContext;
  end;
  { Optional qualified compiler-root representation on a builtin leaf. Alias
    and distinct wrappers retain their own identity and expose this leaf through
    Target; sharing a format does not grant equality, conversion or call legality.
    Absent capability means unqualified representation, not an absent type.
    Values are copied, immutable and independent of later source FPU selection. }
  INyxPascalFloatingIdentity = interface(IInterface)
    ['{2BA32237-BD90-4A7E-A481-E4A38FB9046A}']
    function FloatingFormat: TNyxPascalFloatingFormat;
    function StorageBytes: Integer;
  end;
  { Optional source-owned literal evidence. Span is in ModuleIndex's analysis
    projection; its world retains the exact physical/include/macro origins.
    Code units are bytes for narrow constants and UTF-16 units for wide ones,
    preserving NUL and individual surrogates without a host-string conversion.
    Value is exact integer/Boolean/character ordinal, otherwise Unknown.
    SpecializationType is a qualified generic argument type, never a promise
    that a routine receiving this literal returns its value. Nil means unknown.
    Results retain no query budget, mutable source or execution callback. }
  INyxPascalLiteralType = interface(IInterface)
    ['{A4B85D43-01E9-4F15-A018-B88103632951}']
    function LiteralKind: TNyxPascalLiteralKind;
    function ModuleIndex: Integer;
    function Span: TNyxPascalSpan;
    function Context: INyxPascalCompilerContext;
    function Value: TNyxPascalConstantValue;
    function Wide: Boolean;
    function UnitCount: Integer;
    function CodeUnit(AIndex: Integer): Integer;
    function SpecializationType: INyxPascalSemanticType;
  end;
  { Optional real literal evidence keeps exact source mathematics distinct from
    the qualified compiler-parser result. The ordinary literal Value remains
    Unknown: integer/Boolean constant evaluators must not consume this as a
    rounded constant, and a generic routine result inherits neither value. }
  INyxPascalRealLiteral = interface(IInterface)
    ['{B43025C3-05B9-4D63-877E-E0463B68BA83}']
    function SourceValue: TNyxPascalRealValue;
    function ParsedValue: TNyxPascalBinaryReal;
  end;
  { Optional finite constant proof on either a literal or a folded expression.
    The represented value is detached and distinct from exact source text.
    Generic specialization consumes only the ordinary result type; it must not
    copy this proof onto a routine's return value or an unrelated variable. }
  INyxPascalRealConstant = interface(IInterface)
    ['{50BCB30E-298A-4BB2-8A12-23C0CFCC9B1D}']
    function RealValue: TNyxPascalBinaryReal;
  end;
  { Optional admitted IEEE proof, including infinity and unordered NaN. The
    finite-only contract above keeps its original meaning and GUID. A generic
    result or runtime value inherits neither proof from its argument. }
  INyxPascalIEEEConstant = interface(IInterface)
    ['{05B9DE3B-2930-46CA-9F3A-7697270AA758}']
    function IEEEValue: TNyxPascalIEEEValue;
  end;
  { Optional source-literal evidence also admits a qualified parser infinity.
    Exact source mathematics and the parser result remain separate; finite-only
    literal/constant interfaces are available only when that result is finite.
    No generic return value inherits the argument's source or numeric proof. }
  INyxPascalIEEERealLiteral = interface(IInterface)
    ['{6A81224C-A2A2-4475-BD24-9729406A077B}']
    function SourceValue: TNyxPascalRealValue;
    function ParsedIEEEValue: TNyxPascalIEEEValue;
  end;
  { Character ordinals, untyped literal arrays and already typed string nodes
    have different compiler promotion rules even when their units are equal. }
  TNyxPascalStringConstantForm = (pscfCharacter, pscfLiteral, pscfString);
  { Representation evidence, not a host encoding guess. Unknown byte encoding
    still permits exact byte inspection; UTF-16 units may include lone surrogates. }
  TNyxPascalStringEncoding = (psceUnknown, psceASCII, psceUTF8, psceUTF16);
  { Optional immutable string/character proof. Code units retain embedded NUL
    and original width; no normalization, replacement or locale comparison is
    implicit. Generic/runtime results inherit none of their arguments' units. }
  INyxPascalStringConstant = interface(IInterface)
    ['{84E82C2B-967B-480B-9CC2-142758BC7CF2}']
    function Form: TNyxPascalStringConstantForm;
    function Encoding: TNyxPascalStringEncoding;
    function Wide: Boolean;
    function UnitCount: Integer;
    function CodeUnit(AIndex: Integer): Integer;
  end;
  { Detached scalar proof on a folded expression, independent of a source
    literal. Real expressions keep ConstantValue Unknown and expose their
    separate real capability. Generic calls consume ResultType only. This
    publishes the existing internal contract without changing its GUID/slots. }
  INyxPascalConstantExpression = interface(IInterface)
    ['{2B7D1843-83EC-41A6-ACB2-B6ED721281B5}']
    function ConstantValue: TNyxPascalConstantValue;
    function ResultType: INyxPascalSemanticType;
  end;
  { Optional first-class formal identity. Bound entries are source-resolved
    guaranteed member contracts, including an implied class root when qualified.
    They are not supplied arguments. No world retains these immutable results. }
  INyxPascalFormalParameter = interface(IInterface)
    ['{2E0A155F-9F53-4675-B93E-F746B1BAD0A5}']
    function DeclarationOwner: INyxPascalBoundSymbol;
    function ParameterIndex: Integer;
    function Name: TNyxText;
    function NameSpan: TNyxPascalSpan;
    function RequirementSpan: TNyxPascalSpan;
    function Origin: TNyxPascalSyntaxOrigin;
    function Requirements: TNyxPascalGenericRequirements;
    function Context: INyxPascalCompilerContext;
    function BoundCount: Integer;
    function BoundType(AIndex: Integer): INyxPascalSemanticType;
  end;
  { Bounded query result. Symbol alternatives are never silently discarded.
    BoundType may accompany an intrinsic/constructed type with zero symbols.
    Missing/unsupported/inaccessible states retain no guessed type. Call selection,
    general expression inference and compiler admission are independent owners. }
  INyxPascalResolution = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570E04}']
    function World: INyxPascalSemanticWorld;
    function Status: TNyxPascalBindingStatus;
    function Issue: TNyxPascalBindingIssue;
    { Bounded related unit/name when a gap can be repaired by focused capture. }
    function RelatedName: TNyxText;
    function MessageText: TNyxText;
    function Count: Integer;
    function SymbolAt(AIndex: Integer): INyxPascalBoundSymbol;
    function BoundType: INyxPascalSemanticType;
  end;
  { Small read-only member result tied to an exact world. A failed graph capture
    publishes no partial candidates. HasMore means at least one further visible
    row was checked; no guessed total or exhaustive SDK claim is exposed. }
  INyxPascalMemberPage = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570E08}']
    function ResultSet: INyxPascalResolution;
    function HasMore: Boolean;
    function NextOffset: Integer;
  end;
  { Optional world capability leaves the original exact-name interface intact.
    Walks supported captured ancestors and applies the same visibility/shadowing
    rules as Member. No expression inference, helper lookup or editor IO occurs. }
  INyxPascalMemberQueries = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570E09}']
    function Members(const AType: INyxPascalSemanticType; const ASite: INyxPascalBindingSite;
      const AQuery: TNyxPascalMemberQuery;
      const ACancellation: INyxCancellationScope = nil): INyxPascalMemberPage;
  end;
  { Optional source-owned postfix contracts. The window belongs to the site's
    module lexical view and ends no later than its caret. Calls inspect every
    declared alternative's result without selecting an overload or validating
    opaque argument expressions. Different/unsupported results refuse. No code,
    getter, index expression or callback executes. Access is valid only on a
    bound result; old Lookup/Member interfaces and ordinals stay unchanged. }
  INyxPascalExpressionQueries = interface(IInterface)
    ['{212ABC97-1934-42FA-A8EC-147F6D9F4DEA}']
    function ExpressionType(const ASite: INyxPascalBindingSite;
      const AWindow: TNyxPascalSpan; out AAccess: TNyxPascalMemberAccess;
      const ACancellation: INyxCancellationScope = nil): INyxPascalResolution;
  end;
  { A declared routine's complete header, independent of call eligibility or
    overload selection. The ordinary resolution contains a procedure/function
    semantic type; its parameters include grouped names and source defaults. }
  INyxPascalRoutineQueries = interface(IInterface)
    ['{6D246B11-C139-4A30-8F76-89FE1D971D95}']
    function RoutineSignature(const ARoutine: INyxPascalBoundSymbol;
      const ACancellation: INyxCancellationScope = nil): INyxPascalResolution;
  end;
  { Optional source ownership for a declared signature. Each parameter is its
    original source symbol, with mode/type/default spans and physical origin.
    Required count describes the authored trailing-default structure only; it
    does not prove conversions, default evaluation, varargs or legal calls. }
  INyxPascalDeclaredSignature = interface(IInterface)
    ['{0428DD0C-AD63-40E2-9D38-6DEB756D2C79}']
    function ParameterDeclaration(AIndex: Integer): INyxPascalBoundSymbol;
    function RequiredParameterCount: Integer;
  end;
  { Optional exact count. Total is -1 when a bounded page stopped after proving
    HasMore or when analysis failed; otherwise it counts all visible matches.
    CountAll requests require an exact successful total without extra rows. }
  INyxPascalMemberTotal = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570E0A}']
    function Total: Integer;
  end;
  { Optional bounded record/class operator lookup. Returned alternatives retain
    existing visibility, source-world, generic and cancellation guards. This
    selects a declared family, never an overload for operand values or a call. }
  INyxPascalOperatorQueries = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570E0C}']
    function OperatorMembers(const AType: INyxPascalSemanticType;
      const ASite: INyxPascalBindingSite; AKind: TNyxPascalOperatorKind;
      const ACancellation: INyxCancellationScope = nil): INyxPascalResolution;
  end;
  { Persistent module world, independent of DOM/LCL, an editor or compiler process.
    Modules have explicit captured translation contexts; their source views are
    immutable. Replacement detaches the vector and invalidates old site/symbol
    ownership for the new world. No provider or filesystem fallback is installed.
    Queries keep source/module data read-only and return current-world results.
    Source views may synchronize bounded retention of immutable unbound plans;
    no resolved query result is retained back inside its owning world. }
  INyxPascalSemanticWorld = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570E05}']
    function Options: TNyxPascalBindingOptions;
    function Count: Integer;
    function ModuleAt(AIndex: Integer): INyxPascalTranslatedSyntax;
    function FindModule(const AName: TNyxText): Integer;
    { Cached interface boundary avoids rescanning a whole unit in each query. }
    function InterfaceBoundary(AModule: Integer): Integer;
    { Add/replace an exact module view, retaining earlier worlds. Compiler family/
      version and shared qualified root/structural measurements cannot conflict.
      Missing evidence supplies no complete target/default/SDK coherence proof. }
    function WithModule(const AView: INyxPascalTranslatedSyntax): INyxPascalSemanticWorld;
    { A site belongs to this exact world and lies inside its declared scope. }
    function At(AModule, AScope, ACaret: Integer): INyxPascalBindingSite;
    { Explicit inspection handle; this alone confers no program visibility or
      source edit permission. Lookup/Member perform caller visibility decisions. }
    function Symbol(AModule, ADeclaration: Integer): INyxPascalBoundSymbol;
    { Lexical shadowing/direct use order and exact namespace qualification;
      uncaptured later imports refuse rather than guessing an earlier meaning. }
    function Lookup(const ASite: INyxPascalBindingSite; const AName: TNyxPascalBindingName;
      const ACancellation: INyxCancellationScope = nil): INyxPascalResolution;
    { Bind an explicitly inspected declaration's retained type/result, retaining
      concrete generic receiver arguments. Call inference/admission stay separate. }
    function TypeOfSymbol(const ASymbol: INyxPascalBoundSymbol;
      const ACancellation: INyxCancellationScope = nil): INyxPascalResolution;
    { Search the supported ordinary member/inheritance graph from this caller;
      overload families remain alternatives and general helper rules stay open. }
    function Member(const AType: INyxPascalSemanticType; const ASite: INyxPascalBindingSite;
      const AName: TNyxPascalBindingName; AAccess: TNyxPascalMemberAccess = pmaInstance;
      const ACancellation: INyxCancellationScope = nil): INyxPascalResolution;
  end;
  { Actual last module-admission boundary scan. Reused tokens are previously
    examined lexical ordinals proven unchanged by sealed append lineage; they
    do not grant cached resolutions, compiler facts or discounted query steps. }
  INyxPascalWorldBuildWork = interface(IInterface)
    ['{2764EE62-014B-4C9E-B1CB-F29AEF3B7BD0}']
    function BoundaryTokenReads: Integer;
    function ReusedBoundaryTokens: Integer;
  end;
  { Optional immutable module construction with a caller-owned execution scope.
    The new world retains source modules only, never the scope or its budget.
    Plain WithModule stays compatible; guarded construction refuses alternate
    worlds that cannot establish this pre-operation admission contract. }
  INyxPascalWorldAdmission = interface(IInterface)
    ['{58E2C39F-77C8-4326-A8F2-5297DA40DB81}']
    function WithModuleInScope(const AView: INyxPascalTranslatedSyntax;
      const AExecution: INyxCancellationScope): INyxPascalSemanticWorld;
  end;

{ Validate up to 1024 ASCII identifier/dot characters; each segment is nonempty
  and starts with a letter/underscore. No keyword/member catalog is hardcoded. }
function NyxPascalBindingName(const AName: TNyxText): TNyxPascalBindingName;
{ Empty persistent source world. Nothing parses, opens, executes or enrolls a
  project until the caller explicitly supplies an already constructed source view. }
function NewNyxPascalSemanticWorld(const AOptions: TNyxPascalBindingOptions): INyxPascalSemanticWorld;
{ Exact built-in immutable world ownership, checked inside its implementation.
  A delegating alternate cannot acquire this receipt through public getters or
  capabilities. True grants no source-byte, disk, compiler or target provenance. }
function NyxPascalOwnedSemanticWorld(const AWorld: INyxPascalSemanticWorld): Boolean;
{ Conservative correspondence of one visible name at two sites in the same
  immutable world. Both lookups and named type/alias definitions must retain
  exact declaration identity. Shadowing, uncertain scopes, anonymous/builtin
  leaves and generic substitutions/owners refuse. This establishes no size,
  layout, compiler eligibility or value identity; callers still own the native
  measurement site/context. All lookup/proof work enters the supplied budget. }
function NyxPascalSameStorageBinding(const ABefore, ACurrent: INyxPascalBindingSite;
  const AName: TNyxPascalBindingName;
  const ACancellation: INyxCancellationScope = nil): Boolean;

{ Admit actual boundary scans/module rows while building an immutable candidate.
  A work-bearing scope requires the optional construction capability; ordinary
  cancellation-only callers may use alternate worlds without a work guarantee. }
function WithNyxPascalSemanticModule(const AWorld: INyxPascalSemanticWorld;
  const AView: INyxPascalTranslatedSyntax;
  const AExecution: INyxCancellationScope): INyxPascalSemanticWorld;

{ Request a missing bound primitive's exact compiler evidence. Source ownership,
  origin and selected context are retained independently. Known/inconsistent facts
  or unqualified source provenance yield nil, never a guessed host request. }
function NyxPascalPrimitiveRequirement(const AType: INyxPascalSemanticType;
  const ASite: INyxPascalBindingSite): INyxPascalCompilerRequirement;
implementation

uses nyx.text.index, nyx.pascal.lexing.append, nyx.pascal.work, nyx.pascal.numeric,
  nyx.pascal.translation;

const
  { Keep implementation spellings local while the public typed request owns
    their stable ordinals and meaning. }
  gcrClass = pgrClass;
  gcrRecord = pgrRecord;
  gcrConstructor = pgrConstructor;

type
  { Immutable checked caller, retaining its world without any reverse cache. }
  TBindingSite = class(TInterfacedObject, INyxPascalBindingSite)
  public
    Owner: INyxPascalSemanticWorld;
    ModuleOrdinal: Integer;
    ScopeOrdinal: Integer;
    Position: Integer;
    function World: INyxPascalSemanticWorld;
    function ModuleIndex: Integer;
    function ScopeIndex: Integer;
    function Caret: Integer;
  end;
  { Immutable declaration handle; every getter reuses its own world's snapshot. }
  TBoundSymbol = class(TInterfacedObject, INyxPascalBoundSymbol, INyxPascalSubstitutedSymbol)
  public
    Owner: INyxPascalSemanticWorld;
    ModuleOrdinal: Integer;
    DeclarationOrdinal: Integer;
    Bindings: INyxPascalGenericEnvironment;
    function World: INyxPascalSemanticWorld;
    function ModuleIndex: Integer;
    function DeclarationIndex: Integer;
    function Declaration: TNyxPascalSyntaxDeclaration;
    function Origin: TNyxPascalSyntaxOrigin;
    function Environment: INyxPascalGenericEnvironment;
  end;
  { A queried constructor retains its actual immutable receiver separately from
    the declaring method and its generic frame. An inherited method can thus
    construct a descendant while navigation still names the original source.
    The receiver points to an earlier type value, never back to this handle. }
  TConstructorSymbol = class(TBoundSymbol)
  public
    ReceiverType: INyxPascalSemanticType;
  end;
  { In-unit provenance for the actual specialization site. A declaration's mode
    cannot substitute for the caller's mode-sensitive constraint semantics. }
  IGenericApplication = interface(IInterface)
    ['{7B228571-495B-4A0C-96CC-61036B15358C}']
    function Site: INyxPascalBindingSite;
  end;
  { Immutable concrete lexical frame, built completely before publication. Its
    owner is raw, its parent is an earlier outer frame and its arguments already
    exist; this direction prevents recursive class/member reference cycles. }
  TGenericEnvironment = class(TInterfacedObject, INyxPascalGenericEnvironment, IGenericApplication)
  public
    DeclarationOwner: INyxPascalBoundSymbol;
    Outer: INyxPascalGenericEnvironment;
    Names: array of TNyxText;
    Types: array of INyxPascalSemanticType;
    ApplicationSite: INyxPascalBindingSite;
    function Site: INyxPascalBindingSite;
    function Owner: INyxPascalBoundSymbol;
    function Parent: INyxPascalGenericEnvironment;
    function Count: Integer;
    function Name(AIndex: Integer): TNyxText;
    function Argument(AIndex: Integer): INyxPascalSemanticType;
  end;
  { Query-local identifier vector; it holds no interfaces or mutable source. }
  TGenericNames = array of TNyxText;
  { Query-owned argument answers preserve unavailable type facts separately
    from complete argument syntax. Only a formal-dependent position consumes
    its type answer; ordinary positions grant no compiler-call eligibility. }
  TInferredArgumentAnswers = array of INyxPascalResolution;
  { Authored eligibility requests from the inspected FPC grammar. Constructor
    intent is not proof that any particular constructor invocation will bind. }
  TGenericRequirement = TNyxPascalGenericRequirement;
  { Independent value flags; never a compiler-default or runtime type test. }
  TGenericRequirements = TNyxPascalGenericRequirements;
  { Original named requirement windows contain only source coordinates. Nested
    generic arguments keep their own commas and closing-angle ownership. }
  TGenericConstraintSpans = array of TNyxPascalSpan;
  { One declaration-owned parameter and its original shared requirement range.
    Grouped names get independent values. No interface occurs in this record. }
  TGenericParameter = record
    Name: TNyxText;
    NameSpan: TNyxPascalSpan;
    Requirements: TGenericRequirements;
    RequirementSpan: TNyxPascalSpan;
    NamedSpans: TGenericConstraintSpans;
  end;
  { Query-local plan is bounded by existing candidate/work limits. Its entries
    retain source coordinates and strings, without a world or caller lease. }
  TGenericParameters = array of TGenericParameter;
  { Builder-owned semantic shape. Published instances are read-only; interface
    vectors live in the class and point only to other immutable shapes/worlds. }
  TSemanticType = class(TInterfacedObject, INyxPascalSemanticType, INyxPascalIntrinsicIdentity)
  public
    Owner: INyxPascalSemanticWorld;
    TypeKind: TNyxPascalSemanticTypeKind;
    Intrinsic: TNyxPascalBuiltin;
    IntrinsicScope: TNyxPascalIntrinsicOrigin;
    IntrinsicName: TNyxPascalSizeReference;
    IntrinsicFacts: INyxPascalCompilerContext;
    Declared: INyxPascalBoundSymbol;
    Element: INyxPascalSemanticType;
    TypeSyntax: INyxPascalTypeSyntax;
    TypeNode: Integer;
    Names: array of TNyxText;
    Modes: array of TNyxPascalSyntaxParameterMode;
    Types: array of INyxPascalSemanticType;
    function World: INyxPascalSemanticWorld;
    function Kind: TNyxPascalSemanticTypeKind;
    function Builtin: TNyxPascalBuiltin;
    function IntrinsicOrigin: TNyxPascalIntrinsicOrigin;
    function IntrinsicReference: TNyxPascalSizeReference;
    function IntrinsicContext: INyxPascalCompilerContext;
    function Definition: INyxPascalBoundSymbol;
    function Target: INyxPascalSemanticType;
    function Syntax: INyxPascalTypeSyntax;
    function SyntaxNode: Integer;
    function ParameterCount: Integer;
    function ParameterName(AIndex: Integer): TNyxText;
    function ParameterMode(AIndex: Integer): TNyxPascalSyntaxParameterMode;
    function ParameterType(AIndex: Integer): INyxPascalSemanticType;
  end;
  { Private provenance for a constructed data pointer. Its captured source
    context qualifies representation-sensitive compatibility without changing
    nominal type identity or exposing a machine address/layout to the editor. }
  IDataPointerSource = interface(IInterface)
    ['{860E8806-4B1E-41A5-92C8-C6F56187B962}']
    function PointerContext: INyxPascalCompilerContext;
    function PointerTargetContext: INyxPascalCompilerContext;
  end;
  { Both declared pointer syntax and typed address expressions retain their
    own immutable context. Aliases/distinct wrappers still own their ordinary
    declaration; neither the world nor context retains the resulting type. }
  TDataPointerType = class(TSemanticType, IDataPointerSource)
  public
    CapturedContext: INyxPascalCompilerContext;
    InitialTargetContext: INyxPascalCompilerContext;
    function PointerContext: INyxPascalCompilerContext;
    function PointerTargetContext: INyxPascalCompilerContext;
  end;
  { Qualified primitive leaf adds only value-owned representation metadata.
    The inherited type/context/world remain the ordinary immutable identity;
    nothing retains a query, execution budget, compiler process or application. }
  TFloatingType = class(TSemanticType, INyxPascalFloatingIdentity)
  public
    FormatValue: TNyxPascalFloatingFormat;
    ByteCount: Integer;
    function FloatingFormat: TNyxPascalFloatingFormat;
    function StorageBytes: Integer;
  end;
  { Query-local literal proof extends an ordinary shape, with separately owned
    exact values, source context and deduction type. Its primitive child has no
    literal evidence, so generic result types cannot inherit a constant value. }
  TLiteralType = class(TSemanticType, INyxPascalLiteralType)
  public
    FLiteralKind: TNyxPascalLiteralKind;
    FModule: Integer;
    FSpan: TNyxPascalSpan;
    FContext: INyxPascalCompilerContext;
    FValue: TNyxPascalConstantValue;
    FWide: Boolean;
    FUnits: array of Integer;
    FDeduction: INyxPascalSemanticType;
    function LiteralKind: TNyxPascalLiteralKind;
    function ModuleIndex: Integer;
    function Span: TNyxPascalSpan;
    function Context: INyxPascalCompilerContext;
    function Value: TNyxPascalConstantValue;
    function Wide: Boolean;
    function UnitCount: Integer;
    function CodeUnit(AIndex: Integer): Integer;
    function SpecializationType: INyxPascalSemanticType;
  end;
  { String literals alone expose code-unit evidence. Other literal families do
    not acquire a string proof through the shared ordinary literal base. }
  TStringLiteralType = class(TLiteralType, INyxPascalStringConstant)
  public
    FEncoding: TNyxPascalStringEncoding;
    function Form: TNyxPascalStringConstantForm;
    function Encoding: TNyxPascalStringEncoding;
  end;
  { Admitted real literal, including parser infinity. Its ordinary shape owns
    source/world identity; detached numeric values retain no mutable arithmetic
    workspace or execution lease. Finite proofs belong to the subclass alone. }
  TIEEERealLiteralType = class(TLiteralType, INyxPascalIEEERealLiteral, INyxPascalIEEEConstant)
  public
    FSourceValue: TNyxPascalRealValue;
    FParsedValue: TNyxPascalIEEEValue;
    function SourceValue: TNyxPascalRealValue;
    function ParsedIEEEValue: TNyxPascalIEEEValue;
    function IEEEValue: TNyxPascalIEEEValue;
  end;
  { Finite specialization preserves both original literal and constant GUIDs
    and their finite-only result contracts, including signed inexact zero. }
  TRealLiteralType = class(TIEEERealLiteralType, INyxPascalRealLiteral, INyxPascalRealConstant)
  public
    function ParsedValue: TNyxPascalBinaryReal;
    function RealValue: TNyxPascalBinaryReal;
  end;
  { Internal spelling retains the existing consumer sites while the unchanged
    scalar proof contract is now available to external semantic consumers. }
  INumericExpressionValue = INyxPascalConstantExpression;
  { Query-owned folded builtin shape. The immutable ordinary child carries the
    exact intrinsic/context identity; no world owns this proof or query budget. }
  TNumericExpressionType = class(TSemanticType, INumericExpressionValue)
  public
    FConstant: TNyxPascalConstantValue;
    FResult: INyxPascalSemanticType;
    function ConstantValue: TNyxPascalConstantValue;
    function ResultType: INyxPascalSemanticType;
  end;
  { Detached code-unit vector shared only by value-owning objects and local
    decoders. Elements are integers, never interfaces or borrowed source rows. }
  TStringCodeUnits = array of Integer;
  { Query-owned string result; its ordinary child owns nominal/compiler type
    identity while this object owns detached units and representation facts.
    No source-literal interface, query budget or mutable decoder is retained. }
  TStringExpressionType = class(TNumericExpressionType, INyxPascalStringConstant)
  public
    FForm: TNyxPascalStringConstantForm;
    FEncoding: TNyxPascalStringEncoding;
    FWide: Boolean;
    FUnits: TStringCodeUnits;
    function Form: TNyxPascalStringConstantForm;
    function Encoding: TNyxPascalStringEncoding;
    function Wide: Boolean;
    function UnitCount: Integer;
    function CodeUnit(AIndex: Integer): Integer;
  end;
  { Qualified node-local switches share the scanner's pending-request lifetime.
    Selecting one channel grants no authority over other compiler settings. }
  TCompilerNodeSetting = (flsExcessPrecision, flsIEEEErrors, flsStringH);
  { Query-owned admitted IEEE expression. The ordinary result keeps nominal
    identity; no mutable arithmetic workspace or query budget is published. }
  TIEEEExpressionType = class(TNumericExpressionType, INyxPascalIEEEConstant)
  public
    FIEEEValue: TNyxPascalIEEEValue;
    function IEEEValue: TNyxPascalIEEEValue;
  end;
  { Query-owned finite real expression. Its inherited integer/Boolean value
    stays Unknown; the independent optional capability carries binary evidence.
    The ordinary primitive child supplies specialization and intrinsic identity. }
  TRealExpressionType = class(TIEEEExpressionType, INyxPascalRealConstant)
  public
    FRealValue: TNyxPascalBinaryReal;
    function RealValue: TNyxPascalBinaryReal;
  end;
  { A complete source routine header shares the ordinary semantic signature
    shape, retaining real parameter symbols instead of synthesized declarations.
    Only this query-owned object retains its vectors; worlds do not own it. }
  TDeclaredSignature = class(TSemanticType, INyxPascalDeclaredSignature)
  public
    Parameters: array of INyxPascalBoundSymbol;
    RequiredCount: Integer;
    function ParameterDeclaration(AIndex: Integer): INyxPascalBoundSymbol;
    function RequiredParameterCount: Integer;
  end;
  { A source formal owns its parameter identity and bounded immutable contracts.
    It holds no application frame or execution lease, avoiding recursive frame
    ownership and keeping parameter and ordinary declared-type identity apart. }
  TFormalType = class(TSemanticType, INyxPascalFormalParameter)
  public
    FDeclarationOwner: INyxPascalBoundSymbol;
    FIndex: Integer;
    FParameter: TGenericParameter;
    FOrigin: TNyxPascalSyntaxOrigin;
    FContext: INyxPascalCompilerContext;
    FBounds: array of INyxPascalSemanticType;
    function DeclarationOwner: INyxPascalBoundSymbol;
    function ParameterIndex: Integer;
    function Name: TNyxText;
    function NameSpan: TNyxPascalSpan;
    function RequirementSpan: TNyxPascalSpan;
    function Origin: TNyxPascalSyntaxOrigin;
    function Requirements: TNyxPascalGenericRequirements;
    function Context: INyxPascalCompilerContext;
    function BoundCount: Integer;
    function BoundType(AIndex: Integer): INyxPascalSemanticType;
  end;
  { One constructed response; the world retains no results. Symbols and types
    own their source world outward, preventing editor/model reference cycles. }
  TResolution = class(TInterfacedObject, INyxPascalResolution, INyxPascalCompilerRequirements)
  public
    Owner: INyxPascalSemanticWorld;
    Outcome: TNyxPascalBindingStatus;
    Reason: TNyxPascalBindingIssue;
    Related: TNyxText;
    Symbols: array of INyxPascalBoundSymbol;
    TypeValue: INyxPascalSemanticType;
    Requirements: INyxPascalCompilerRequirements;
    function World: INyxPascalSemanticWorld;
    function Status: TNyxPascalBindingStatus;
    function Issue: TNyxPascalBindingIssue;
    function RelatedName: TNyxText;
    function MessageText: TNyxText;
    function Count: Integer;
    function SymbolAt(AIndex: Integer): INyxPascalBoundSymbol;
    function BoundType: INyxPascalSemanticType;
    function RequirementCount: Integer;
    function RequirementAt(AIndex: Integer): INyxPascalCompilerRequirement;
  end;
  { Holds only the bounded reply and continuation offset. Like ordinary query
    results it points outward to its world without any reverse cache. }
  TMemberPage = class(TInterfacedObject, INyxPascalMemberPage, INyxPascalMemberTotal)
  public
    Reply: INyxPascalResolution;
    More: Boolean;
    Following: Integer;
    TotalValue: Integer;
    function ResultSet: INyxPascalResolution;
    function HasMore: Boolean;
    function NextOffset: Integer;
    function Total: Integer;
  end;
  { Owns a detached module vector/name index. Published worlds never mutate;
    each query has its own counters, alias/member trails and temporary type data. }
  TSemanticWorld = class(TInterfacedObject, INyxPascalSemanticWorld,
    INyxPascalMemberQueries, INyxPascalOperatorQueries, INyxPascalWorldBuildWork,
    INyxPascalWorldAdmission, INyxPascalExpressionQueries, INyxPascalRoutineQueries)
  private
    FLimits: TNyxPascalBindingOptions;
    FModules: array of INyxPascalTranslatedSyntax;
    FInterfaceEnds: array of Integer;
    FInterfaceTokens: array of Integer;
    FBoundaryReads: Integer;
    FBoundaryReused: Integer;
    FNames: TNyxTextIndex;
    procedure CheckModuleEvidence(const AView: INyxPascalTranslatedSyntax;
      AReplacing: Integer; const AExecution: INyxCancellationScope);
  public
    constructor Create(const AOptions: TNyxPascalBindingOptions);
    destructor Destroy; override;
    function Options: TNyxPascalBindingOptions;
    function Count: Integer;
    function ModuleAt(AIndex: Integer): INyxPascalTranslatedSyntax;
    function FindModule(const AName: TNyxText): Integer;
    function InterfaceBoundary(AModule: Integer): Integer;
    function BoundaryTokenReads: Integer;
    function ReusedBoundaryTokens: Integer;
    function WithModule(const AView: INyxPascalTranslatedSyntax): INyxPascalSemanticWorld;
    function WithModuleInScope(const AView: INyxPascalTranslatedSyntax;
      const AExecution: INyxCancellationScope): INyxPascalSemanticWorld;
    function At(AModule, AScope, ACaret: Integer): INyxPascalBindingSite;
    function Symbol(AModule, ADeclaration: Integer): INyxPascalBoundSymbol;
    function Lookup(const ASite: INyxPascalBindingSite; const AName: TNyxPascalBindingName;
      const ACancellation: INyxCancellationScope): INyxPascalResolution;
    function TypeOfSymbol(const ASymbol: INyxPascalBoundSymbol;
      const ACancellation: INyxCancellationScope): INyxPascalResolution;
    function Member(const AType: INyxPascalSemanticType; const ASite: INyxPascalBindingSite;
      const AName: TNyxPascalBindingName; AAccess: TNyxPascalMemberAccess;
      const ACancellation: INyxCancellationScope): INyxPascalResolution;
    function Members(const AType: INyxPascalSemanticType; const ASite: INyxPascalBindingSite;
      const AQuery: TNyxPascalMemberQuery;
      const ACancellation: INyxCancellationScope): INyxPascalMemberPage;
    function ExpressionType(const ASite: INyxPascalBindingSite;
      const AWindow: TNyxPascalSpan; out AAccess: TNyxPascalMemberAccess;
      const ACancellation: INyxCancellationScope): INyxPascalResolution;
    function RoutineSignature(const ARoutine: INyxPascalBoundSymbol;
      const ACancellation: INyxCancellationScope): INyxPascalResolution;
    function OperatorMembers(const AType: INyxPascalSemanticType; const ASite: INyxPascalBindingSite;
      AKind: TNyxPascalOperatorKind;
      const ACancellation: INyxCancellationScope): INyxPascalResolution;
  end;
  { Per-call query owner. Cancellation/steps/depth and recursion trails cannot
    leak into another query or mutate shared sources. No authored code executes. }
  TBindingQuery = class
  private
    FWorld: INyxPascalSemanticWorld;
    FCancel: INyxCancellationScope;
    FLimits: TNyxPascalBindingOptions;
    FSteps: Integer;
    FDepth: Integer;
    FAliases: array of INyxPascalBoundSymbol;
    FMembers: array of INyxPascalBoundSymbol;
    { Query-owned ancestry path detects cycles without retaining any mutable
      memo in a published world or erasing constructed argument identity. }
    FConstraintPath: array of INyxPascalSemanticType;
    { Declaration-bound value recursion is separate from type/ancestry paths;
      it is unwound on every refusal, cancellation and successful publication. }
    FConstants: array of INyxPascalBoundSymbol;
    { A requirement names a formal contract. Until formal parameter bounds own
      their own semantic shapes, a concrete substitution cannot stand in for it. }
    FBindingConstraint: Boolean;
    { Owner-name lookup must not recursively re-enter the same method header. }
    FResolvingImplementationOwner: Boolean;
    { Type-target lookup may see source formals, but never implicit Self/Result
      values merely because those expressions also have type-only answers. }
    FBindingTypeTarget: Boolean;
    { A query-local candidate is used only to prove absence for every possible
      interface style. It never changes or publishes a captured default. }
    FConsideringInterfaceRoot: Boolean;
    FEnvironment: INyxPascalGenericEnvironment;
    FTypeOwner: INyxPascalBoundSymbol;
    procedure Tick;
    procedure Enter;
    procedure Leave;
    procedure CheckSite(const ASite: INyxPascalBindingSite);
    procedure CheckSymbol(const ASymbol: INyxPascalBoundSymbol);
    function Answer(AStatus: TNyxPascalBindingStatus;
      AIssue: TNyxPascalBindingIssue; const ARelated: TNyxText = '';
      const ARequirement: INyxPascalCompilerRequirement = nil): INyxPascalResolution;
    function TypeAnswer(const AType: INyxPascalSemanticType): INyxPascalResolution;
    function OneSymbol(const ASymbol: INyxPascalBoundSymbol): INyxPascalResolution;
    function SymbolAnswer(const ASymbols: array of INyxPascalBoundSymbol): INyxPascalResolution;
    function InterfaceEnd(AModule: Integer): Integer;
    function ExportsKnown(AModule: Integer): Boolean;
    function ContextAt(const ASite: INyxPascalBindingSite): INyxPascalCompilerContext;
    function SafeScope(AModule, AScope, ACaret: Integer): Boolean;
    function Exact(AModule, AScope, ACaret: Integer; const AName: TNyxText;
      AExternal: Boolean; AOperator: TNyxPascalOperatorKind = pokNone): INyxPascalResolution;
    function Single(const ASite: INyxPascalBindingSite; const AName: TNyxText;
      AImports: Boolean): INyxPascalResolution;
    function Imports(const ASite: INyxPascalBindingSite; const AName: TNyxText): INyxPascalResolution;
    function UnitVisible(const ASite: INyxPascalBindingSite; const AName: TNyxText): Boolean;
    function UsesObjPas(const ASite: INyxPascalBindingSite): TNyxPascalKnowledge;
    function CurrentClass(const ASite: INyxPascalBindingSite): INyxPascalBoundSymbol;
    { Lookup through a validated open enclosing header, after exact prior lexical
      members have been searched. Never constructs/adopts an incomplete receiver. }
    function OpenScopeInherited(const AOwner: INyxPascalBoundSymbol;
      const ASite: INyxPascalBindingSite; const AName: TNyxText): INyxPascalResolution;
    { Shared ancestor traversal owns a detached seed vector and recursion trail.
      Unknown/inaccessible bases refuse the whole answer, preserving overloads. }
    function InheritedMembers(const AOwner: INyxPascalBoundSymbol;
      const ASite: INyxPascalBindingSite; const AName: TNyxText;
      AAccess: TNyxPascalMemberAccess; AOperator: TNyxPascalOperatorKind;
      const AOwn: array of INyxPascalBoundSymbol): INyxPascalResolution;
    function Descends(const AChild, AParent: INyxPascalBoundSymbol;
      out AUnresolved: Boolean): Boolean;
    function Access(const AMember, AOwner: INyxPascalBoundSymbol;
      const ASite: INyxPascalBindingSite): TNyxPascalKnowledge;
    function BuiltinType(const ASite: INyxPascalBindingSite;
      const AName: TNyxText; ASystem: Boolean = False): INyxPascalResolution;
    function BindNode(const ASite: INyxPascalBindingSite;
      const AType: INyxPascalTypeSyntax; ANode: Integer): INyxPascalResolution;
    function Bases(const AOwner: INyxPascalBoundSymbol; AOrdinal: Integer): INyxPascalResolution;
    function InterfaceStyleOf(const AOwner: INyxPascalBoundSymbol): TNyxPascalInterfaceStyle;
    function SameSymbol(const ALeft, ARight: INyxPascalBoundSymbol): Boolean;
    function Unwrap(const AType: INyxPascalSemanticType): INyxPascalSemanticType;
    function GenericNames(const ASymbol: INyxPascalBoundSymbol;
      out ANames: TGenericNames): TNyxPascalBindingIssue;
    function GenericParameters(const ASymbol: INyxPascalBoundSymbol;
      out AParameters: TGenericParameters): TNyxPascalBindingIssue;
    function CheckGenericRequirements(const ASymbol: INyxPascalBoundSymbol;
      const AFrame: INyxPascalGenericEnvironment;
      const AParameters: TGenericParameters): TNyxPascalBindingIssue;
    function CheckNamedGenericRequirements(const ASymbol: INyxPascalBoundSymbol;
      const AFrame: INyxPascalGenericEnvironment; const AParameter: TGenericParameter;
      const AArgument: INyxPascalSemanticType): TNyxPascalBindingIssue;
    function GenericRequirementType(const ASymbol: INyxPascalBoundSymbol;
      const AFrame: INyxPascalGenericEnvironment; const ASpan: TNyxPascalSpan): INyxPascalResolution;
    function FormalParameter(const ASymbol: INyxPascalBoundSymbol; AIndex: Integer;
      const AParameters: TGenericParameters): INyxPascalResolution;
    function FormalReceiver(const ASymbol: INyxPascalBoundSymbol): INyxPascalResolution;
    function GenericImplementationOwner(const ASite: INyxPascalBindingSite;
      const ARoutine: INyxPascalBoundSymbol): INyxPascalResolution;
    { Implicit storage is a type-only expression contract, never a fabricated
      declaration/navigation target. A nil answer lets outer lexical scopes run. }
    function ImplicitRoutine(const ASite: INyxPascalBindingSite; AScope: Integer;
      const AName: TNyxText): INyxPascalResolution;
    function MethodReceiver(const ASite: INyxPascalBindingSite;
      const ARoutine: INyxPascalBoundSymbol; out AHasSelf: Boolean): INyxPascalResolution;
    { Index contracts retain the actual default declaration, not a same-named
      hiding member. Arguments remain opaque and no accessor is executed. }
    function DefaultProperty(const AType: INyxPascalSemanticType;
      const ASite: INyxPascalBindingSite; AAccess: TNyxPascalMemberAccess): INyxPascalResolution;
    function IndexedProperty(const ASymbol: INyxPascalBoundSymbol;
      ACount: Integer): INyxPascalResolution;
    function FormalMember(const AFormal: INyxPascalFormalParameter;
      const ASite: INyxPascalBindingSite; const AName: TNyxText;
      AAccess: TNyxPascalMemberAccess; AOperator: TNyxPascalOperatorKind): INyxPascalResolution;
    { Ordinary lookup remains independent of the final queried receiver. The
      outer member boundary adds constructor receiver ownership after inheritance. }
    function MemberCore(const AType: INyxPascalSemanticType;
      const ASite: INyxPascalBindingSite; const AName: TNyxText;
      AAccess: TNyxPascalMemberAccess; AOperator: TNyxPascalOperatorKind): INyxPascalResolution;
    function ConstructorResult(const ASymbol: INyxPascalBoundSymbol;
      const ASite: INyxPascalBindingSite): INyxPascalResolution;
    function ConstructorResults(const AFound: INyxPascalResolution;
      const ASite: INyxPascalBindingSite): INyxPascalResolution;
    function ConstraintRelated(const AActual, ARequired: INyxPascalSemanticType): TNyxPascalKnowledge;
    function InEnvironment(const ASymbol: INyxPascalBoundSymbol;
      const AEnvironment: INyxPascalGenericEnvironment): INyxPascalBoundSymbol;
    function ParameterAt(const ASite: INyxPascalBindingSite; AScope: Integer;
      const AName: TNyxText): INyxPascalResolution;
    function ParameterFromOwner(const ASite: INyxPascalBindingSite;
      const AOwner: INyxPascalBoundSymbol; const AName: TNyxText): INyxPascalResolution;
    function Specialize(const ASite: INyxPascalBindingSite;
      const AType: INyxPascalTypeSyntax; ANode: Integer): INyxPascalResolution;
    { Explicit arguments bind at the caller while the declaration and its outer
      receiver frame retain their original owners. No runtime argument is read. }
    function ApplyGenericArguments(const ASite: INyxPascalBindingSite;
      const AType: INyxPascalTypeSyntax; ANode: Integer;
      const APrototype: INyxPascalBoundSymbol): INyxPascalResolution;
    function ApplyRoutineArguments(const ASite: INyxPascalBindingSite;
      const AFound: INyxPascalResolution; const AType: INyxPascalTypeSyntax;
      ANode: Integer): INyxPascalResolution;
    function InferRoutineArguments(const ASite: INyxPascalBindingSite;
      const AFound: INyxPascalResolution;
      const AArguments: TInferredArgumentAnswers): INyxPascalResolution;
    function TypeDeclaration(const ASite: INyxPascalBindingSite;
      const AType: INyxPascalTypeSyntax; ANode: Integer): INyxPascalResolution;
    function TypeOfSymbolCore(const ASymbol: INyxPascalBoundSymbol): INyxPascalResolution;
    { Publish detached numeric evidence while retaining the exact ordinary
      specialization type, including a declared alias/distinct cast target. }
    function ConstantAnswer(const AType: INyxPascalSemanticType;
      const AScalar: TNyxPascalConstantValue;
      const AReal: TNyxPascalIEEEValue): INyxPascalResolution;
    { Copy source-owned units into a value decoration, preserving the actual
      ordinary result type and its declaration without claiming literal origin. }
    function StringAnswer(const AType: INyxPascalSemanticType;
      const AValue: INyxPascalStringConstant): INyxPascalResolution;
    { Admit complete new unit storage through both the local semantic ceiling
      and an optional aggregate parent before allocating any vector. }
    procedure AdmitStringUnits(ACount: Integer);
    function BasesCore(const AOwner: INyxPascalBoundSymbol; AOrdinal: Integer): INyxPascalResolution;
    function CollectMemberNames(const AType: INyxPascalSemanticType;
      const APrefix: TNyxText; var ANames: TGenericNames): INyxPascalResolution;
    function SameEnvironment(const ALeft, ARight: INyxPascalGenericEnvironment): Boolean;
    function SameType(const ALeft, ARight: INyxPascalSemanticType): Boolean;
    function SameBinding(const ALeft, ARight: INyxPascalBoundSymbol): Boolean;
    { Compatibility is deliberately separate from exact declaration identity.
      These helpers grant only the qualified flat FPC data-pointer rules. }
    function DataPointer(const AType: INyxPascalSemanticType;
      out ALeaf: INyxPascalSemanticType; out AContext: INyxPascalCompilerContext): Boolean;
    function EqualDataPointers(const ALeft, ARight: INyxPascalSemanticType): Boolean;
    { Initial compiler/root facts retain target authority independently of
      authored DEFINE/UNDEF directives in a pointer's source namespace. }
    function InitialPointerContext(AModule: Integer): INyxPascalCompilerContext;
  public
    constructor Create(const AWorld: INyxPascalSemanticWorld;
      const ACancellation: INyxCancellationScope);
    function Lookup(const ASite: INyxPascalBindingSite; const AName: TNyxText): INyxPascalResolution;
    function TypeOfSymbol(const ASymbol: INyxPascalBoundSymbol): INyxPascalResolution;
    function RoutineSignature(const ARoutine: INyxPascalBoundSymbol): INyxPascalResolution;
    function Member(const AType: INyxPascalSemanticType; const ASite: INyxPascalBindingSite;
      const AName: TNyxText; AAccess: TNyxPascalMemberAccess;
      AOperator: TNyxPascalOperatorKind = pokNone): INyxPascalResolution;
    function Members(const AType: INyxPascalSemanticType; const ASite: INyxPascalBindingSite;
      const AQuery: TNyxPascalMemberQuery): INyxPascalMemberPage;
    function ExpressionType(const ASite: INyxPascalBindingSite;
      const AWindow: TNyxPascalSpan; out AAccess: TNyxPascalMemberAccess): INyxPascalResolution;
  end;

function NyxPascalBindingName(const AName: TNyxText): TNyxPascalBindingName;
var
  LAt: Integer;
  LStart: Boolean;
begin

  if (AName = '') or (Length(AName) > 1024) then
  begin
    raise ENyxPascalBinding.Create('A Pascal binding name needs 1 to 1024 identifier characters');
  end;
  LStart := True;
  for LAt := 1 to Length(AName) do
  begin

    if AName[LAt] = '.' then
    begin

      if LStart then
      begin
        raise ENyxPascalBinding.Create('A Pascal binding name has an empty segment');
      end;
      LStart := True;
    end
    else
    begin

      if not (AName[LAt] in ['A'..'Z', 'a'..'z', '_']) and
        not (not LStart and (AName[LAt] in ['0'..'9'])) then
      begin
        raise ENyxPascalBinding.Create('A Pascal binding name needs ASCII identifier segments');
      end;
      LStart := False;
    end;
  end;

  if LStart then
  begin
    raise ENyxPascalBinding.Create('A Pascal binding name cannot end in a dot');
  end;
  Result.FName := AName;
end;

class function TNyxPascalBindingOptions.Defaults: TNyxPascalBindingOptions;
begin
  Result := Default(TNyxPascalBindingOptions);
  Result.FDefined := True;
  Result.FModules := 512;
  Result.FDepth := 64;
  Result.FSteps := 100000;
  Result.FCandidates := 50;
end;

procedure TNyxPascalBindingOptions.Validate;
begin

  if not FDefined or (FModules < 1) or (FDepth < 1) or (FSteps < 1) or (FCandidates < 1) or
    (Ord(FImplicitScopes) < Ord(Low(TNyxPascalImplicitScopePolicy))) or
    (Ord(FImplicitScopes) > Ord(High(TNyxPascalImplicitScopePolicy))) then
  begin
    raise ENyxPascalBinding.Create('Use explicit positive Pascal binding budgets');
  end;
end;

function TNyxPascalBindingOptions.WithImplicitScopes(
  AValue: TNyxPascalImplicitScopePolicy): TNyxPascalBindingOptions;
begin
  Result := Self;
  Result.FImplicitScopes := AValue;
  Result.Validate;
end;

function TNyxPascalBindingOptions.UpToModules(AValue: Integer): TNyxPascalBindingOptions;
begin
  Result := Self;
  Result.FModules := AValue;
  Result.Validate;
end;

function TNyxPascalBindingOptions.UpToDepth(AValue: Integer): TNyxPascalBindingOptions;
begin
  Result := Self;
  Result.FDepth := AValue;
  Result.Validate;
end;

function TNyxPascalBindingOptions.UpToSteps(AValue: Integer): TNyxPascalBindingOptions;
begin
  Result := Self;
  Result.FSteps := AValue;
  Result.Validate;
end;

function TNyxPascalBindingOptions.UpToCandidates(AValue: Integer): TNyxPascalBindingOptions;
begin
  Result := Self;
  Result.FCandidates := AValue;
  Result.Validate;
end;

class function TNyxPascalMemberQuery.Defaults: TNyxPascalMemberQuery;
begin
  Result.FDefined := True;
  Result.FPrefix := '';
  Result.FOffset := 0;
  Result.FCount := 20;
  Result.FAccess := pmaInstance;
  Result.FCountAll := False;
end;

procedure TNyxPascalMemberQuery.Validate;
begin

  if not FDefined or (FOffset < 0) or (FCount < 1) or (FCount > 50) or
    (FOffset > High(Integer) - FCount - 1) then
  begin
    raise ENyxPascalBinding.Create('Use a positive member page of at most 50 rows and a bounded offset');
  end;

  if FPrefix <> '' then
  begin
    NyxPascalBindingName(FPrefix);

    if Pos('.', FPrefix) > 0 then
    begin
      raise ENyxPascalBinding.Create('A member prefix belongs to one receiver, not a qualified path');
    end;
  end;
end;

function TNyxPascalMemberQuery.Matching(const AValue: TNyxText): TNyxPascalMemberQuery;
begin
  Result := Self;
  Result.FPrefix := AValue;
  Result.Validate;
end;

function TNyxPascalMemberQuery.Skip(AValue: Integer): TNyxPascalMemberQuery;
begin
  Result := Self;
  Result.FOffset := AValue;
  Result.Validate;
end;

function TNyxPascalMemberQuery.Take(AValue: Integer): TNyxPascalMemberQuery;
begin
  Result := Self;
  Result.FCount := AValue;
  Result.Validate;
end;

function TNyxPascalMemberQuery.ForAccess(AValue: TNyxPascalMemberAccess): TNyxPascalMemberQuery;
begin
  Result := Self;
  Result.FAccess := AValue;
  Result.Validate;
end;

function TNyxPascalMemberQuery.CountAll(AValue: Boolean): TNyxPascalMemberQuery;
begin
  Result := Self;
  Result.FCountAll := AValue;
  Result.Validate;
end;

function TBindingSite.World: INyxPascalSemanticWorld;
begin
  Result := Owner;
end;

function TBindingSite.ModuleIndex: Integer;
begin
  Result := ModuleOrdinal;
end;

function TBindingSite.ScopeIndex: Integer;
begin
  Result := ScopeOrdinal;
end;

function TBindingSite.Caret: Integer;
begin
  Result := Position;
end;

function TBoundSymbol.World: INyxPascalSemanticWorld;
begin
  Result := Owner;
end;

function TBoundSymbol.ModuleIndex: Integer;
begin
  Result := ModuleOrdinal;
end;

function TBoundSymbol.DeclarationIndex: Integer;
begin
  Result := DeclarationOrdinal;
end;

function TBoundSymbol.Declaration: TNyxPascalSyntaxDeclaration;
begin
  Result := Owner.ModuleAt(ModuleOrdinal).Syntax.Declaration(DeclarationOrdinal);
end;

function TBoundSymbol.Origin: TNyxPascalSyntaxOrigin;
var
  LView: INyxPascalTranslatedSyntax;
begin
  LView := Owner.ModuleAt(ModuleOrdinal);
  Result := LView.Origin(LView.OriginAt(Declaration.NameSpan.Start));
end;

function TBoundSymbol.Environment: INyxPascalGenericEnvironment;
begin
  Result := Bindings;
end;

function TGenericEnvironment.Owner: INyxPascalBoundSymbol;
begin
  Result := DeclarationOwner;
end;

function TGenericEnvironment.Parent: INyxPascalGenericEnvironment;
begin
  Result := Outer;
end;

function TGenericEnvironment.Count: Integer;
begin
  Result := Length(Names);
end;

function TGenericEnvironment.Name(AIndex: Integer): TNyxText;
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ENyxPascalBinding.Create('Generic argument index is outside its frame');
  end;
  Result := Names[AIndex];
end;

function TGenericEnvironment.Argument(AIndex: Integer): INyxPascalSemanticType;
begin
  Name(AIndex);
  Result := Types[AIndex];
end;

function EnvironmentOf(const ASymbol: INyxPascalBoundSymbol): INyxPascalGenericEnvironment;
var
  LCapability: INyxPascalSubstitutedSymbol;
begin
  Result := nil;

  if (ASymbol <> nil) and Supports(ASymbol, INyxPascalSubstitutedSymbol, LCapability) then
  begin
    Result := LCapability.Environment;
  end;
end;

function TSemanticType.World: INyxPascalSemanticWorld;
begin
  Result := Owner;
end;

function TSemanticType.Kind: TNyxPascalSemanticTypeKind;
begin
  Result := TypeKind;
end;

function TSemanticType.Builtin: TNyxPascalBuiltin;
begin
  Result := Intrinsic;
end;

function TFloatingType.FloatingFormat: TNyxPascalFloatingFormat;
begin
  Result := FormatValue;
end;

function TFloatingType.StorageBytes: Integer;
begin
  Result := ByteCount;
end;

function TSemanticType.IntrinsicOrigin: TNyxPascalIntrinsicOrigin;
begin
  IntrinsicReference;
  Result := IntrinsicScope;
end;

function TSemanticType.IntrinsicReference: TNyxPascalSizeReference;
begin

  if (TypeKind <> pstBuiltin) or (IntrinsicName.Name = '') or (IntrinsicFacts = nil) then
  begin
    raise ENyxPascalBinding.Create('Only a captured builtin has intrinsic source identity');
  end;
  Result := IntrinsicName;
end;

function TSemanticType.IntrinsicContext: INyxPascalCompilerContext;
begin
  IntrinsicReference;
  Result := IntrinsicFacts;
end;

function TSemanticType.Definition: INyxPascalBoundSymbol;
begin
  Result := Declared;
end;

function TSemanticType.Target: INyxPascalSemanticType;
begin
  Result := Element;
end;

function TDataPointerType.PointerContext: INyxPascalCompilerContext;
begin
  Result := CapturedContext;
end;

function TDataPointerType.PointerTargetContext: INyxPascalCompilerContext;
begin
  Result := InitialTargetContext;
end;

function TSemanticType.Syntax: INyxPascalTypeSyntax;
begin
  Result := TypeSyntax;
end;

function TSemanticType.SyntaxNode: Integer;
begin
  Result := -1;

  if TypeSyntax <> nil then
  begin
    Result := TypeNode;
  end;
end;

function TSemanticType.ParameterCount: Integer;
begin
  Result := Length(Names);
end;

function TSemanticType.ParameterName(AIndex: Integer): TNyxText;
begin

  if (AIndex < 0) or (AIndex >= ParameterCount) then
  begin
    raise ENyxPascalBinding.Create('Semantic parameter index is outside its signature');
  end;
  Result := Names[AIndex];
end;

function TSemanticType.ParameterMode(AIndex: Integer): TNyxPascalSyntaxParameterMode;
begin
  ParameterName(AIndex);
  Result := Modes[AIndex];
end;

function TSemanticType.ParameterType(AIndex: Integer): INyxPascalSemanticType;
begin
  ParameterName(AIndex);
  Result := Types[AIndex];
end;

function TLiteralType.LiteralKind: TNyxPascalLiteralKind;
begin
  Result := FLiteralKind;
end;

function TLiteralType.ModuleIndex: Integer;
begin
  Result := FModule;
end;

function TLiteralType.Span: TNyxPascalSpan;
begin
  Result := FSpan;
end;

function TLiteralType.Context: INyxPascalCompilerContext;
begin
  Result := FContext;
end;

function TLiteralType.Value: TNyxPascalConstantValue;
begin
  Result := FValue;
end;

function TStringLiteralType.Form: TNyxPascalStringConstantForm;
begin
  Result := pscfLiteral;

  if FLiteralKind = plkCharacter then Result := pscfCharacter;
end;

function TStringLiteralType.Encoding: TNyxPascalStringEncoding;
begin
  Result := FEncoding;
end;

function TStringExpressionType.Form: TNyxPascalStringConstantForm;
begin
  Result := FForm;
end;

function TStringExpressionType.Encoding: TNyxPascalStringEncoding;
begin
  Result := FEncoding;
end;

function TStringExpressionType.Wide: Boolean;
begin
  Result := FWide;
end;

function TStringExpressionType.UnitCount: Integer;
begin
  Result := Length(FUnits);
end;

function TStringExpressionType.CodeUnit(AIndex: Integer): Integer;
begin

  if (AIndex < 0) or (AIndex >= Length(FUnits)) then
    raise ERangeError.Create('String constant code-unit index is out of range');
  Result := FUnits[AIndex];
end;

function TIEEERealLiteralType.SourceValue: TNyxPascalRealValue;
begin
  Result := FSourceValue;
end;

function TIEEERealLiteralType.ParsedIEEEValue: TNyxPascalIEEEValue;
begin
  Result := FParsedValue;
end;

function TRealLiteralType.ParsedValue: TNyxPascalBinaryReal;
begin
  Result := FParsedValue.FiniteValue;
end;

function TRealLiteralType.RealValue: TNyxPascalBinaryReal;
begin
  Result := FParsedValue.FiniteValue;
end;

function TRealExpressionType.RealValue: TNyxPascalBinaryReal;
begin
  Result := FRealValue;
end;

function TIEEERealLiteralType.IEEEValue: TNyxPascalIEEEValue;
begin
  Result := FParsedValue;
end;

function TIEEEExpressionType.IEEEValue: TNyxPascalIEEEValue;
begin
  Result := FIEEEValue;
end;

function TLiteralType.Wide: Boolean;
begin
  Result := FWide;
end;

function TLiteralType.UnitCount: Integer;
begin
  Result := Length(FUnits);
end;

function TLiteralType.CodeUnit(AIndex: Integer): Integer;
begin

  if (AIndex < 0) or (AIndex >= Length(FUnits)) then
    raise ENyxPascalBinding.Create('A literal code-unit ordinal is outside its source value');
  Result := FUnits[AIndex];
end;

function TLiteralType.SpecializationType: INyxPascalSemanticType;
begin
  Result := FDeduction;
end;

function TNumericExpressionType.ConstantValue: TNyxPascalConstantValue;
begin
  Result := FConstant;
end;

function TNumericExpressionType.ResultType: INyxPascalSemanticType;
begin
  Result := FResult;
end;

function TDeclaredSignature.ParameterDeclaration(AIndex: Integer): INyxPascalBoundSymbol;
begin
  ParameterName(AIndex);
  Result := Parameters[AIndex];
end;

function TDeclaredSignature.RequiredParameterCount: Integer;
begin
  Result := RequiredCount;
end;

function TFormalType.DeclarationOwner: INyxPascalBoundSymbol;
begin
  Result := FDeclarationOwner;
end;

function TFormalType.ParameterIndex: Integer;
begin
  Result := FIndex;
end;

function TFormalType.Name: TNyxText;
begin
  Result := FParameter.Name;
end;

function TFormalType.NameSpan: TNyxPascalSpan;
begin
  Result := FParameter.NameSpan;
end;

function TFormalType.RequirementSpan: TNyxPascalSpan;
begin
  Result := FParameter.RequirementSpan;
end;

function TFormalType.Origin: TNyxPascalSyntaxOrigin;
begin
  Result := FOrigin;
end;

function TFormalType.Requirements: TNyxPascalGenericRequirements;
begin
  Result := FParameter.Requirements;
end;

function TFormalType.Context: INyxPascalCompilerContext;
begin
  Result := FContext;
end;

function TFormalType.BoundCount: Integer;
begin
  Result := Length(FBounds);
end;

function TFormalType.BoundType(AIndex: Integer): INyxPascalSemanticType;
begin

  if (AIndex < 0) or (AIndex >= Length(FBounds)) then
  begin
    raise ENyxPascalBinding.Create('Formal bound index is outside its contract');
  end;
  Result := FBounds[AIndex];
end;

function TMemberPage.ResultSet: INyxPascalResolution;
begin
  Result := Reply;
end;

function TMemberPage.Total: Integer;
begin
  Result := TotalValue;
end;

function TMemberPage.HasMore: Boolean;
begin
  Result := More;
end;

function TMemberPage.NextOffset: Integer;
begin
  Result := Following;
end;

function TResolution.World: INyxPascalSemanticWorld;
begin
  Result := Owner;
end;

function TResolution.Status: TNyxPascalBindingStatus;
begin
  Result := Outcome;
end;

function TResolution.Issue: TNyxPascalBindingIssue;
begin
  Result := Reason;
end;

function TResolution.RelatedName: TNyxText;
begin
  Result := Related;
end;

function TResolution.RequirementCount: Integer;
begin
  Result := 0;

  if Requirements <> nil then
  begin
    Result := Requirements.RequirementCount;
  end;
end;

function TResolution.RequirementAt(AIndex: Integer): INyxPascalCompilerRequirement;
begin

  if Requirements = nil then
  begin
    raise ENyxPascalContext.Create('This binding result has no compiler requirement');
  end;
  Result := Requirements.RequirementAt(AIndex);
end;

function TResolution.MessageText: TNyxText;
const
  CMessages: array[TNyxPascalBindingIssue] of TNyxText =
    ('', 'This name has no supported visible declaration',
      'A required directly imported module was not captured',
      'This declaration is inaccessible from the caller',
      'This scope contains unmodeled declaration meaning',
      'This source prefix or declaration is incomplete',
      'This type needs additional semantic support',
      'Generic parameters or specialization require a binding environment',
      'This declaration has no supported explicit type',
      'This type or inheritance chain is recursive',
      'Additional captured compiler facts are required',
      'This member is unavailable for this receiver category',
      'Multiple declarations require disambiguation',
      'Overload or inherited overload meaning needs additional context',
      'The generic argument count differs from its declaration',
      'Generic constraints need additional semantic checking');
begin
  Result := CMessages[Reason];
end;

function TResolution.Count: Integer;
begin
  Result := Length(Symbols);
end;

function TResolution.SymbolAt(AIndex: Integer): INyxPascalBoundSymbol;
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ENyxPascalBinding.Create('Resolution symbol index is outside its response');
  end;
  Result := Symbols[AIndex];
end;

function TResolution.BoundType: INyxPascalSemanticType;
begin
  Result := TypeValue;
end;

constructor TSemanticWorld.Create(const AOptions: TNyxPascalBindingOptions);
begin
  inherited Create;
  AOptions.Validate;
  FLimits := AOptions;
  FNames := TNyxTextIndex.Create;
end;

constructor ENyxPascalModuleContext.Create(const ARelatedModule: TNyxText);
begin
  FRelatedModule := NyxPascalBindingName(ARelatedModule).Name;
  inherited Create('Captured compiler context conflicts with unit ' + String(FRelatedModule));
end;

destructor TSemanticWorld.Destroy;
begin
  FNames.Free;
  inherited Destroy;
end;

function TSemanticWorld.Options: TNyxPascalBindingOptions;
begin
  Result := FLimits;
end;

function TSemanticWorld.Count: Integer;
begin
  Result := Length(FModules);
end;

function TSemanticWorld.ModuleAt(AIndex: Integer): INyxPascalTranslatedSyntax;
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ENyxPascalBinding.Create('Semantic module index is outside its source world');
  end;
  Result := FModules[AIndex];
end;

function TSemanticWorld.FindModule(const AName: TNyxText): Integer;
begin
  Result := FNames.IndexOf(NyxPascalASCIIKey(AName));
end;

function TSemanticWorld.InterfaceBoundary(AModule: Integer): Integer;
begin
  ModuleAt(AModule);
  Result := FInterfaceEnds[AModule];
end;

function TSemanticWorld.BoundaryTokenReads: Integer;
begin
  Result := FBoundaryReads;
end;

function TSemanticWorld.ReusedBoundaryTokens: Integer;
begin
  Result := FBoundaryReused;
end;

function TSemanticWorld.WithModule(const AView: INyxPascalTranslatedSyntax): INyxPascalSemanticWorld;
begin
  Result := WithModuleInScope(AView, nil);
end;

procedure TSemanticWorld.CheckModuleEvidence(const AView: INyxPascalTranslatedSyntax;
  AReplacing: Integer; const AExecution: INyxCancellationScope);
var
  LInitial, LRoot, LOther, LOtherRoot: INyxPascalCompilerContext;
  LAt: Integer;
  LRelated: TNyxText;

  procedure Admit(AUnits: Integer = 1);
  begin

    if (AExecution <> nil) and AExecution.Cancelled then
      raise ENyxPascalAnalysisCancelled.Create('Semantic module evidence check cancelled');
    NyxPascalAdmitAnalysis(AExecution, papWorldEntry, AUnits);
  end;

  function RootOf(const AContext: INyxPascalCompilerContext): INyxPascalCompilerContext;
  begin
    Result := AContext;

    if NyxPascalCompilerScope(AContext) <> pcsoSystemBootstrap then
      Result := NyxPascalBootstrapContext(AContext);
  end;

  function HasStorage(const AContext: INyxPascalCompilerContext): Boolean;
  var
    LStorage: INyxPascalContextStorage;
  begin
    Result := (AContext <> nil) and Supports(AContext, INyxPascalContextStorage, LStorage) and
      (LStorage.StorageCount > 0);
  end;

  function HasSizes(const AContext: INyxPascalCompilerContext): Boolean;
  var
    LSizes: INyxPascalContextSizes;
  begin
    Result := (AContext <> nil) and Supports(AContext, INyxPascalContextSizes, LSizes) and
      (LSizes.SizeCount > 0);
  end;

  { Only compiler-root names are comparable across units. Ordinary source-mode
    names can denote different aliases. Even root answers retain their exact
    probe mode/string qualifiers: another question cannot contradict this one. }
  procedure CheckSizes(const ALeft, ARight: INyxPascalCompilerContext);
  var
    LLeft, LRight: INyxPascalContextSizes;
    LIndex, LLeftCount, LRightCount: Integer;
    LName: TNyxPascalSizeReference;
    LBefore, LAfter: TNyxPascalSizeFact;
  begin

    if (ALeft = nil) or (ARight = nil) or (ALeft = ARight) or
      not Supports(ALeft, INyxPascalContextSizes, LLeft) or
      not Supports(ARight, INyxPascalContextSizes, LRight) then Exit;
    LLeftCount := LLeft.SizeCount;
    LRightCount := LRight.SizeCount;

    if (LLeftCount < 0) or (LLeftCount > 64) or (LRightCount < 0) or (LRightCount > 64) then
      raise ENyxPascalBindingLimit.Create('Module root evidence exceeds its captured size quota');
    for LIndex := 0 to LLeftCount - 1 do
    begin
      Admit;
      LName := LLeft.SizeName(LIndex);
      { Context size lookup is a bounded vector search. Charge its full bound
        before either lookup rather than concealing repeated comparisons. }
      Admit(LLeftCount + LRightCount + Length(LName.Name));
      LBefore := LLeft.SizeFact(LName);
      LAfter := LRight.SizeFact(LName);

      if (LBefore.State <> pssUnknown) and (LAfter.State <> pssUnknown) and
        (LBefore.ProbeMode = LAfter.ProbeMode) and
        (LBefore.ProbeLongStrings = LAfter.ProbeLongStrings) and
        not LBefore.SameFact(LAfter) then raise ENyxPascalModuleContext.Create(LRelated);
    end;
  end;

  { Structural keys already include their full source/context/type identity.
    Only exact keys may conflict; the world never merges, retags or exports
    measurements. Every bounded key comparison is charged before reading it. }
  procedure CheckStorage(const ALeft, ARight: INyxPascalCompilerContext);
  var
    LLeft, LRight: INyxPascalContextStorage;
    LIndex, LOtherIndex, LLeftCount, LRightCount: Integer;
    LKey, LOtherKey: TNyxPascalStorageKey;
    LBefore, LAfter: TNyxPascalStorageFact;
  begin

    if (ALeft = nil) or (ARight = nil) or (ALeft = ARight) or
      not Supports(ALeft, INyxPascalContextStorage, LLeft) or
      not Supports(ARight, INyxPascalContextStorage, LRight) then Exit;
    LLeftCount := LLeft.StorageCount;
    LRightCount := LRight.StorageCount;

    if (LLeftCount < 0) or (LLeftCount > NyxPascalStorageFactLimit) or
      (LRightCount < 0) or (LRightCount > NyxPascalStorageFactLimit) then
      raise ENyxPascalBindingLimit.Create('Module structural evidence exceeds its captured quota');
    for LIndex := 0 to LLeftCount - 1 do
    begin
      Admit;
      LKey := LLeft.StorageKey(LIndex);
      LBefore := LLeft.StorageValueAt(LIndex);
      for LOtherIndex := 0 to LRightCount - 1 do
      begin
        Admit;
        LOtherKey := LRight.StorageKey(LOtherIndex);
        Admit(Length(LKey.Value) + Length(LOtherKey.Value));

        if not LKey.SameKey(LOtherKey) then Continue;
        LAfter := LRight.StorageValueAt(LOtherIndex);

        if (LBefore.State <> psfsUnknown) and (LAfter.State <> psfsUnknown) and
          not LBefore.SameFact(LAfter) then raise ENyxPascalModuleContext.Create(LRelated);
        Break;
      end;
    end;
  end;
begin
  LInitial := AView.Source.InitialContext;
  LRoot := RootOf(LInitial);

  if not HasSizes(LRoot) and not HasStorage(LInitial) and not HasStorage(LRoot) then Exit;

  if (AReplacing >= 0) and (LInitial = FModules[AReplacing].Source.InitialContext) then
  begin
    { This immutable parent world already checked this exact initial packet
      against every retained module. Source-prefix reconstruction changes the
      view, not its captured evidence. Charge the identity proof once; a new
      context object, even value-equal, still takes the complete comparison. }
    Admit;
    Exit;
  end;
  for LAt := 0 to Count - 1 do
  begin

    if LAt = AReplacing then Continue;
    Admit;
    LOther := FModules[LAt].Source.InitialContext;
    LOtherRoot := RootOf(LOther);
    LRelated := FModules[LAt].Syntax.Scope(0).Name;
    CheckSizes(LRoot, LOtherRoot);
    CheckStorage(LInitial, LOther);

    if LRoot <> LInitial then CheckStorage(LRoot, LOther);

    if LOtherRoot <> LOther then CheckStorage(LInitial, LOtherRoot);

    if (LRoot <> LInitial) and (LOtherRoot <> LOther) then CheckStorage(LRoot, LOtherRoot);
  end;
end;

function TSemanticWorld.WithModuleInScope(const AView: INyxPascalTranslatedSyntax;
  const AExecution: INyxCancellationScope): INyxPascalSemanticWorld;
var
  LCopy: TSemanticWorld;
  LLease: INyxPascalSemanticWorld;
  LAt: Integer;
  LTarget: Integer;
  LCount: Integer;
  LName: TNyxText;
  LLex: INyxPascalLexicalSnapshot;
  LTokenIndex: Integer;
  LRetained: Integer;
  LFirst: Integer;
  LKnownBoundary: Integer;
  LToken: TNyxPascalToken;
begin

  if (AView = nil) or (AView.Syntax.ScopeCount = 0) or
    (AView.Syntax.Scope(0).Kind <> psskModule) then
  begin
    raise ENyxPascalBinding.Create('A semantic module needs an explicit declared module view');
  end;
  LName := NyxPascalBindingName(AView.Syntax.Scope(0).Name).Name;

  if (Count > 0) and
    ((AView.Source.InitialContext.Family <> FModules[0].Source.InitialContext.Family) or
      (AView.Source.InitialContext.Version.FullNumber <>
        FModules[0].Source.InitialContext.Version.FullNumber)) then
  begin
    raise ENyxPascalModuleContext.Create(FModules[0].Syntax.Scope(0).Name);
  end;
  LTarget := FindModule(LName);
  CheckModuleEvidence(AView, LTarget, AExecution);
  LCount := Count;

  if LTarget < 0 then
  begin

    if LCount >= FLimits.MaximumModules then
    begin
      raise ENyxPascalBindingLimit.Create('Semantic modules exceed their world budget');
    end;
    LTarget := LCount;
    Inc(LCount);
  end;
  LCopy := TSemanticWorld.Create(FLimits);
  { Native interface function results may share their caller's destination.
    Keep all vector/name/boundary construction private until it succeeds. }
  LLease := LCopy;
  SetLength(LCopy.FModules, LCount);
  SetLength(LCopy.FInterfaceEnds, LCount);
  SetLength(LCopy.FInterfaceTokens, LCount);
  for LAt := 0 to LCount - 1 do
  begin

    if (AExecution <> nil) and AExecution.Cancelled then
    begin
      raise ENyxPascalAnalysisCancelled.Create('Semantic module construction cancelled');
    end;
    NyxPascalAdmitAnalysis(AExecution, papWorldEntry);

    if LAt = LTarget then
    begin
      LCopy.FModules[LAt] := AView;
      LLex := AView.Syntax.Lexical;
      LCopy.FInterfaceEnds[LAt] := LLex.Scalars;
      LCopy.FInterfaceTokens[LAt] := -1;
      LFirst := 0;
      LKnownBoundary := -1;

      if LTarget < Count then
      begin
        LRetained := NyxPascalRetainedLexicalTokens(FModules[LTarget].Syntax.Lexical, LLex);

        if LRetained >= 0 then
        begin
          LFirst := LRetained;

          if (FInterfaceTokens[LTarget] >= 0) and
            (FInterfaceTokens[LTarget] < LRetained) then
          begin
            LKnownBoundary := FInterfaceTokens[LTarget];
            LFirst := LKnownBoundary + 1;
          end;
          LCopy.FBoundaryReused := LFirst;
        end;
      end;

      if LKnownBoundary >= 0 then
      begin
        LCopy.FInterfaceEnds[LAt] := FInterfaceEnds[LTarget];
        LCopy.FInterfaceTokens[LAt] := LKnownBoundary;
        LFirst := LLex.Count;
      end;
      for LTokenIndex := LFirst to LLex.Count - 1 do
      begin

        if (AExecution <> nil) and AExecution.Cancelled then
        begin
          raise ENyxPascalAnalysisCancelled.Create('Semantic module boundary scan cancelled');
        end;
        NyxPascalAdmitAnalysis(AExecution, papWorldToken);
        Inc(LCopy.FBoundaryReads);
        LToken := LLex.Token(LTokenIndex);

        if (LToken.Kind = ptkWord) and (LToken.Word = pkwImplementation) and not LToken.Escaped then
        begin
          LCopy.FInterfaceEnds[LAt] := LToken.Span.Start;
          LCopy.FInterfaceTokens[LAt] := LTokenIndex;
          Break;
        end;
      end;
    end
    else
    begin
      LCopy.FModules[LAt] := FModules[LAt];
      LCopy.FInterfaceEnds[LAt] := FInterfaceEnds[LAt];
      LCopy.FInterfaceTokens[LAt] := FInterfaceTokens[LAt];
    end;
    LCopy.FNames.AddFirst(NyxPascalASCIIKey(LCopy.FModules[LAt].Syntax.Scope(0).Name), LAt);
  end;
  Result := LLease;
end;

function TSemanticWorld.At(AModule, AScope, ACaret: Integer): INyxPascalBindingSite;
var
  LSite: TBindingSite;
  LScope: TNyxPascalSyntaxScope;
begin
  LScope := ModuleAt(AModule).Syntax.Scope(AScope);

  if (ACaret < LScope.Span.Start) or (ACaret > LScope.Span.Finish) then
  begin
    raise ENyxPascalBinding.Create('A binding caller must lie inside its declared scope');
  end;
  LSite := TBindingSite.Create;
  Result := LSite;
  LSite.Owner := Self;
  LSite.ModuleOrdinal := AModule;
  LSite.ScopeOrdinal := AScope;
  LSite.Position := ACaret;
end;

function TSemanticWorld.Symbol(AModule, ADeclaration: Integer): INyxPascalBoundSymbol;
var
  LSymbol: TBoundSymbol;
begin
  ModuleAt(AModule).Syntax.Declaration(ADeclaration);
  LSymbol := TBoundSymbol.Create;
  Result := LSymbol;
  LSymbol.Owner := Self;
  LSymbol.ModuleOrdinal := AModule;
  LSymbol.DeclarationOrdinal := ADeclaration;
end;

constructor TBindingQuery.Create(const AWorld: INyxPascalSemanticWorld;
  const ACancellation: INyxCancellationScope);
begin
  inherited Create;
  FWorld := AWorld;
  FLimits := AWorld.Options;
  FCancel := ACancellation;
  Tick;
end;

procedure TBindingQuery.Tick;
begin
  NyxPascalAdmitAnalysis(FCancel, papBindingStep);

  if (FCancel <> nil) and FCancel.Cancelled then
  begin
    raise ENyxPascalAnalysisCancelled.Create('Pascal binding cancelled');
  end;

  if FSteps >= FLimits.MaximumSteps then
  begin
    raise ENyxPascalBindingLimit.Create('Pascal semantic steps exceed their query budget');
  end;
  Inc(FSteps);
end;

procedure TBindingQuery.Enter;
begin
  Tick;

  if FDepth >= FLimits.MaximumDepth then
  begin
    raise ENyxPascalBindingLimit.Create('Pascal semantic recursion exceeds its query budget');
  end;
  Inc(FDepth);
end;

procedure TBindingQuery.Leave;
begin
  Dec(FDepth);
end;

procedure TBindingQuery.CheckSite(const ASite: INyxPascalBindingSite);
begin

  if (ASite = nil) or (ASite.World <> FWorld) then
  begin
    raise ENyxPascalBinding.Create('This caller belongs to another semantic source world');
  end;
  FWorld.At(ASite.ModuleIndex, ASite.ScopeIndex, ASite.Caret);
end;

procedure TBindingQuery.CheckSymbol(const ASymbol: INyxPascalBoundSymbol);
begin

  if (ASymbol = nil) or (ASymbol.World <> FWorld) then
  begin
    raise ENyxPascalBinding.Create('This declaration belongs to another semantic source world');
  end;
  ASymbol.Declaration;
end;

function TBindingQuery.Answer(AStatus: TNyxPascalBindingStatus;
  AIssue: TNyxPascalBindingIssue; const ARelated: TNyxText;
  const ARequirement: INyxPascalCompilerRequirement): INyxPascalResolution;
var
  LResult: TResolution;
begin
  LResult := TResolution.Create;
  Result := LResult;
  LResult.Owner := FWorld;
  LResult.Outcome := AStatus;
  LResult.Reason := AIssue;
  LResult.Related := ARelated;

  if ARequirement <> nil then
  begin
    LResult.Requirements := NewNyxPascalRequirements(ARequirement);
  end;
end;

function TBindingQuery.TypeAnswer(const AType: INyxPascalSemanticType): INyxPascalResolution;
var
  LResult: TResolution;
begin
  LResult := TResolution.Create;
  Result := LResult;
  LResult.Owner := FWorld;
  LResult.Outcome := pbsBound;
  LResult.TypeValue := AType;
end;

function TBindingQuery.SymbolAnswer(const ASymbols: array of INyxPascalBoundSymbol): INyxPascalResolution;
var
  LResult: TResolution;
  LAt: Integer;
  LRoutines: Boolean;
begin

  if Length(ASymbols) > FLimits.MaximumCandidates then
  begin
    raise ENyxPascalBindingLimit.Create('Pascal candidates exceed their query budget');
  end;
  LResult := TResolution.Create;
  Result := LResult;
  LResult.Owner := FWorld;
  SetLength(LResult.Symbols, Length(ASymbols));
  LRoutines := True;
  for LAt := 0 to High(ASymbols) do
  begin
    LResult.Symbols[LAt] := ASymbols[LAt];
    LRoutines := LRoutines and (ASymbols[LAt].Declaration.Kind = psdkRoutine);
  end;
  LResult.Outcome := pbsBound;

  if Length(ASymbols) = 0 then
  begin
    LResult.Outcome := pbsMissing;
    LResult.Reason := pbiMissingName;
  end
  else if Length(ASymbols) > 1 then
  begin
    LResult.Outcome := pbsAmbiguous;
    LResult.Reason := pbiAmbiguousName;

    if LRoutines then
    begin
      LResult.Outcome := pbsOverloads;
      LResult.Reason := pbiOverloadContext;
    end;
  end;
end;

function TBindingQuery.OneSymbol(const ASymbol: INyxPascalBoundSymbol): INyxPascalResolution;
var
  LSymbols: array of INyxPascalBoundSymbol;
begin
  SetLength(LSymbols, 1);
  LSymbols[0] := ASymbol;
  Result := SymbolAnswer(LSymbols);
end;

function TBindingQuery.SameSymbol(const ALeft, ARight: INyxPascalBoundSymbol): Boolean;
begin
  Result := (ALeft <> nil) and (ARight <> nil) and
    (ALeft.ModuleIndex = ARight.ModuleIndex) and
    (ALeft.DeclarationIndex = ARight.DeclarationIndex);
end;

function TBindingQuery.InterfaceEnd(AModule: Integer): Integer;
begin
  Tick;
  Result := FWorld.InterfaceBoundary(AModule);
end;

function TBindingQuery.ExportsKnown(AModule: Integer): Boolean;
var
  LSyntax: INyxPascalSyntaxSnapshot;
  LBoundary: Integer;
  LAt: Integer;
begin
  LSyntax := FWorld.ModuleAt(AModule).Syntax;
  LBoundary := InterfaceEnd(AModule);
  Result := (LBoundary < LSyntax.Lexical.Scalars) and (LSyntax.ScopeAt(LBoundary) = 0);

  if not Result then
  begin
    Exit;
  end;
  for LAt := 0 to LSyntax.DiagnosticCount - 1 do
  begin
    Tick;

    if LSyntax.Diagnostic(LAt).Span.Start <= LBoundary then
    begin
      Exit(False);
    end;
  end;
end;

function TBindingQuery.SameBinding(const ALeft, ARight: INyxPascalBoundSymbol): Boolean;
begin
  Result := SameSymbol(ALeft, ARight) and
    SameEnvironment(EnvironmentOf(ALeft), EnvironmentOf(ARight));
end;

function TBindingQuery.SameEnvironment(const ALeft, ARight: INyxPascalGenericEnvironment): Boolean;
var
  LAt: Integer;
begin
  Enter;
  try

    if ALeft = ARight then
    begin
      Exit(True);
    end;

    if (ALeft = nil) or (ARight = nil) then
    begin
      Exit(False);
    end;

    if not SameSymbol(ALeft.Owner, ARight.Owner) or (ALeft.Count <> ARight.Count) or
      not SameEnvironment(ALeft.Parent, ARight.Parent) then
    begin
      Exit(False);
    end;
    for LAt := 0 to ALeft.Count - 1 do
    begin

      if not SameType(ALeft.Argument(LAt), ARight.Argument(LAt)) then
      begin
        Exit(False);
      end;
    end;
    Result := True;
  finally
    Leave;
  end;
end;

function TBindingQuery.SameType(const ALeft, ARight: INyxPascalSemanticType): Boolean;
var
  LAt: Integer;
  LLeftIntrinsic: INyxPascalIntrinsicIdentity;
  LRightIntrinsic: INyxPascalIntrinsicIdentity;
  LLeftFormal: INyxPascalFormalParameter;
  LRightFormal: INyxPascalFormalParameter;
  LLeftLiteral, LRightLiteral: INyxPascalLiteralType;
begin
  Enter;
  try

    if ALeft = ARight then
    begin
      Exit(True);
    end;

    if (ALeft = nil) or (ARight = nil) then
    begin
      Exit(False);
    end;

    { Transparent aliases preserve their source-facing result objects while
      comparing as their targets. Distinct types retain their own declaration. }

    if ALeft.Kind = pstAlias then Exit(SameType(ALeft.Target, ARight));
    if ARight.Kind = pstAlias then Exit(SameType(ALeft, ARight.Target));

    if (ALeft.Kind <> ARight.Kind) or (ALeft.Builtin <> ARight.Builtin) then
    begin
      Exit(False);
    end;

    if (ALeft.Kind = pstArray) and
      (Supports(ALeft, INyxPascalLiteralType, LLeftLiteral) or
       Supports(ARight, INyxPascalLiteralType, LRightLiteral)) then
    begin
      { A constant array's bound is part of its type. The general anonymous
        shape comparison cannot discard that bound or equate a source array
        declaration with compiler constant-string storage. }
      Exit(Supports(ALeft, INyxPascalLiteralType, LLeftLiteral) and
        Supports(ARight, INyxPascalLiteralType, LRightLiteral) and
        (LLeftLiteral.UnitCount = LRightLiteral.UnitCount) and
        (LLeftLiteral.Wide = LRightLiteral.Wide) and SameType(ALeft.Target, ARight.Target));
    end;

    if ALeft.Kind = pstFormal then
    begin
      { Separate parameters never become equal because their bounds agree. }
      Result := Supports(ALeft, INyxPascalFormalParameter, LLeftFormal) and
        Supports(ARight, INyxPascalFormalParameter, LRightFormal);

      if not Result then Exit;
      Exit(SameSymbol(LLeftFormal.DeclarationOwner, LRightFormal.DeclarationOwner) and
        (LLeftFormal.ParameterIndex = LRightFormal.ParameterIndex) and
        (LLeftFormal.NameSpan.Start = LRightFormal.NameSpan.Start) and
        (LLeftFormal.NameSpan.Length = LRightFormal.NameSpan.Length));
    end;

    if ALeft.Kind = pstBuiltin then
    begin

      if not Supports(ALeft, INyxPascalIntrinsicIdentity, LLeftIntrinsic) or
        not Supports(ARight, INyxPascalIntrinsicIdentity, LRightIntrinsic) then
      begin
        Exit(False);
      end;

      if (LLeftIntrinsic.IntrinsicOrigin <> LRightIntrinsic.IntrinsicOrigin) or
        (LLeftIntrinsic.IntrinsicReference.Name <> LRightIntrinsic.IntrinsicReference.Name) or
        not LLeftIntrinsic.IntrinsicContext.SameFacts(LRightIntrinsic.IntrinsicContext) then
      begin
        Exit(False);
      end;
    end;

    if (ALeft.Definition <> nil) or (ARight.Definition <> nil) then
    begin
      Exit(SameBinding(ALeft.Definition, ARight.Definition));
    end;
    { Anonymous bounds/conventions cannot be declared equivalent merely because
      their element types agree. Retain their exact lexical node identity. }

    if (ALeft.Syntax <> nil) or (ARight.Syntax <> nil) then
    begin

      if (ALeft.Syntax = nil) or (ARight.Syntax = nil) then
      begin
        Exit(False);
      end;

      if (ALeft.Syntax.Lexical <> ARight.Syntax.Lexical) or
        (ALeft.SyntaxNode <> ARight.SyntaxNode) or
        (ALeft.Syntax.Window.Start <> ARight.Syntax.Window.Start) or
        (ALeft.Syntax.Window.Length <> ARight.Syntax.Window.Length) then
      begin
        Exit(False);
      end;
    end;

    if not SameType(ALeft.Target, ARight.Target) or
      (ALeft.ParameterCount <> ARight.ParameterCount) then
    begin
      Exit(False);
    end;
    for LAt := 0 to ALeft.ParameterCount - 1 do
    begin

      if (ALeft.ParameterName(LAt) <> ARight.ParameterName(LAt)) or
        (ALeft.ParameterMode(LAt) <> ARight.ParameterMode(LAt)) or
        not SameType(ALeft.ParameterType(LAt), ARight.ParameterType(LAt)) then
      begin
        Exit(False);
      end;
    end;
    Result := True;
  finally
    Leave;
  end;
end;

function TBindingQuery.InitialPointerContext(AModule: Integer): INyxPascalCompilerContext;
var LRoot: INyxPascalCompilerContext;
begin
  Result := FWorld.ModuleAt(AModule).Source.InitialContext;
  LRoot := NyxPascalBootstrapContext(Result);

  if LRoot <> nil then Result := LRoot;
end;

function TBindingQuery.DataPointer(const AType: INyxPascalSemanticType;
  out ALeaf: INyxPascalSemanticType; out AContext: INyxPascalCompilerContext): Boolean;
var LSource: IDataPointerSource; LActive: INyxPascalCompilerContext;
begin
  Enter;
  try
    ALeaf := AType; AContext := nil;
    while (ALeaf <> nil) and (ALeaf.Kind in [pstAlias,pstDistinct]) do
    begin
      Tick;
      ALeaf := ALeaf.Target;
    end;

    if ALeaf = nil then Exit(False);

    if (ALeaf.Kind <> pstPointer) and
      ((ALeaf.Kind <> pstBuiltin) or (ALeaf.Builtin <> pbbPointer)) then Exit(False);

    if not Supports(ALeaf,IDataPointerSource,LSource) then Exit(False);
    LActive := LSource.PointerContext; AContext := LSource.PointerTargetContext;
    { This qualification supplies a common flat representation, not a guessed
      byte count. Source namespace changes cannot retarget the compiler. The
      declaration may belong to an imported FPC-mode System; the operator's
      caller mode is qualified independently at its existing query boundary. }
    Result := (AContext <> nil) and (AContext.Family = ppcFPC) and
      (AContext.Version.FullNumber = 30301) and (LActive <> nil) and
      (LActive.Family = ppcFPC) and (LActive.Version.FullNumber = 30301) and
      (LActive.Mode <> ppmUnknown) and
      (AContext.Symbol(NyxPascalDefine('CPUI386')).IsDefined = ppkTrue);
  finally Leave; end;
end;

function TBindingQuery.EqualDataPointers(const ALeft, ARight: INyxPascalSemanticType): Boolean;
var LLeft, LRight, LLeftTarget, LRightTarget: INyxPascalSemanticType;
  LLeftContext, LRightContext: INyxPascalCompilerContext;
  LLeftFrame, LRightFrame: INyxPascalGenericEnvironment;

  function Pointee(const AValue: INyxPascalSemanticType): INyxPascalSemanticType;
  var LLeaf: INyxPascalSemanticType;
  begin
    Result := AValue;
    while (Result <> nil) and (Result.Kind = pstAlias) do
    begin Tick; Result := Result.Target; end;

    if (Result = nil) or (Result.Kind <> pstDistinct) then Exit;
    LLeaf := Result;
    while (LLeaf <> nil) and (LLeaf.Kind in [pstAlias,pstDistinct]) do
    begin Tick; LLeaf := LLeaf.Target; end;
    { Cloning a primitive/pointer preserves its representation. A copied record
      or class gets its own declaration; matching storage shape is no equality. }

    if (LLeaf <> nil) and (LLeaf.Kind in [pstBuiltin,pstPointer]) then Result := LLeaf;
  end;
begin
  Enter;
  try
    Result := False;

    if not DataPointer(ALeft,LLeft,LLeftContext) or
      not DataPointer(ARight,LRight,LRightContext) then Exit;

    if (LLeft.Kind = pstBuiltin) or (LRight.Kind = pstBuiltin) then
      Exit((LLeft.Kind = pstBuiltin) and (LRight.Kind = pstBuiltin));
    LLeftTarget := Pointee(LLeft.Target); LRightTarget := Pointee(LRight.Target);

    if (LLeftTarget = nil) or (LRightTarget = nil) then Exit;
    { FPC's pointer constructors propagate the pointee specialization's generic
      owner, with no pointer generic-argument list. compare_defs_ext tests that
      owner before ordinary pointee equality. Preserve this compiler equality
      without erasing the actual arguments from either nominal semantic type. }
    LLeftFrame := nil; LRightFrame := nil;

    if LLeftTarget.Definition <> nil then LLeftFrame := EnvironmentOf(LLeftTarget.Definition);

    if LRightTarget.Definition <> nil then LRightFrame := EnvironmentOf(LRightTarget.Definition);

    if (LLeftFrame <> nil) and (LRightFrame <> nil) and
      SameSymbol(LLeftFrame.Owner,LRightFrame.Owner) and
      SameEnvironment(LLeftFrame.Parent,LRightFrame.Parent) then Exit(True);
    { Distinctness on the pointee is retained by SameType, but FPC's equal_defs
      can admit cloned primitive representations through a pointer. The local
      normalization grants no ordinary assignment or generic type equality. }

    if SameType(LLeftTarget,LRightTarget) then Exit(True);

    if (LLeftTarget <> nil) and (LRightTarget <> nil) and
      (LLeftTarget.Kind = pstPointer) and (LRightTarget.Kind = pstPointer) then
      Exit(EqualDataPointers(LLeftTarget,LRightTarget));
  finally Leave; end;
end;

function TBindingQuery.ContextAt(const ASite: INyxPascalBindingSite): INyxPascalCompilerContext;
var
  LView: INyxPascalTranslatedSyntax;
  LOrigin: Integer;
begin
  LView := FWorld.ModuleAt(ASite.ModuleIndex);
  LOrigin := LView.OriginAt(ASite.Caret);
  Result := nil;

  if LOrigin >= 0 then
  begin
    Result := LView.Source.State(LView.Origin(LOrigin).State);
  end
  else if (ASite.Caret = LView.Syntax.Lexical.Scalars) and LView.CompleteTranslation then
  begin
    Result := LView.Source.State(LView.Source.Occurrence(0).ExitState);
  end;
end;

function TBindingQuery.SafeScope(AModule, AScope, ACaret: Integer): Boolean;
var
  LSyntax: INyxPascalSyntaxSnapshot;
  LAt: Integer;
begin
  LSyntax := FWorld.ModuleAt(AModule).Syntax;
  Result := True;

  if not LSyntax.Scope(AScope).UnsupportedSyntax then
  begin
    Exit;
  end;

  if LSyntax.Scope(AScope).Kind <> psskModule then
  begin
    Exit(False);
  end;
  for LAt := 0 to LSyntax.DiagnosticCount - 1 do
  begin
    Tick;

    if (LSyntax.Diagnostic(LAt).Issue = psiUnsupportedSyntax) and
      (LSyntax.Diagnostic(LAt).Span.Start <= ACaret) then
    begin
      Exit(False);
    end;
  end;
end;

function TBindingQuery.Exact(AModule, AScope, ACaret: Integer; const AName: TNyxText;
  AExternal: Boolean; AOperator: TNyxPascalOperatorKind): INyxPascalResolution;
var
  LSyntax: INyxPascalSyntaxSnapshot;
  LAt: Integer;
  LDecl: TNyxPascalSyntaxDeclaration;
  LSymbols: array of INyxPascalBoundSymbol;
  LCount: Integer;
  LFullType: Integer;
  LForwardForm: TNyxPascalSyntaxTypeForm;
  LCompletion: Integer;
  LOperators: INyxPascalOperatorSyntax;
begin
  Tick;
  LSyntax := FWorld.ModuleAt(AModule).Syntax;

  if not SafeScope(AModule, AScope, ACaret) then
  begin
    Exit(Answer(pbsUnresolved, pbiUnsupportedScope));
  end;

  if AOperator = pokNone then
  begin
    LAt := LSyntax.Named(AScope, AName);
  end
  else
  begin

    if not Supports(LSyntax, INyxPascalOperatorSyntax, LOperators) then
    begin
      Exit(Answer(pbsUnresolved, pbiUnsupportedScope));
    end;
    LAt := LOperators.NamedOperator(AScope, AOperator);
  end;
  LCount := 0;
  LFullType := -1;
  SetLength(LSymbols, 0);
  while LAt >= 0 do
  begin
    Tick;
    LDecl := LSyntax.Declaration(LAt);

    if (LDecl.OwnerName = '') and (LDecl.NameSpan.Start <= ACaret) and
      (not AExternal or (LDecl.Visibility = psavInterface)) then
    begin

      if psdfConditional in LDecl.Flags then
      begin
        Exit(Answer(pbsUnresolved, pbiIncompleteSource));
      end;

      if LCount >= FLimits.MaximumCandidates then
      begin
        raise ENyxPascalBindingLimit.Create('Pascal declaration candidates exceed their query budget');
      end;
      SetLength(LSymbols, LCount + 1);
      LSymbols[LCount] := InEnvironment(FWorld.Symbol(AModule, LAt), FEnvironment);

      if (LDecl.Kind = psdkType) and (LDecl.NestedScope >= 0) and
        not (psdfForward in LDecl.Flags) then
      begin

        if LFullType >= 0 then
        begin
          LFullType := -2;
        end
        else if LFullType <> -2 then
        begin
          LFullType := LCount;
        end;
      end;
      Inc(LCount);
    end;
    LAt := LDecl.NextNamed;
  end;

  if (LFullType < 0) and (LCount > 0) then
  begin
    { A visible forward declaration grants the same type identity to its later
      completion. That completion may follow the original reference position. }
    LForwardForm := LSymbols[0].Declaration.TypeForm;
    LCompletion := -1;
    for LAt := 0 to LCount - 1 do
    begin

      if (LSymbols[LAt].Declaration.Kind <> psdkType) or
        not (psdfForward in LSymbols[LAt].Declaration.Flags) or
        (LSymbols[LAt].Declaration.TypeForm <> LForwardForm) then
      begin
        LCompletion := -2;
      end;
    end;

    if LCompletion <> -2 then
    begin
      LAt := LSyntax.Named(AScope, AName);
      while LAt >= 0 do
      begin
        Tick;
        LDecl := LSyntax.Declaration(LAt);

        if (LDecl.Kind = psdkType) and (LDecl.TypeForm = LForwardForm) and
          (LDecl.NestedScope >= 0) and not (psdfForward in LDecl.Flags) and
          (not AExternal or (LDecl.Visibility = psavInterface)) then
        begin

          if LCompletion >= 0 then
          begin
            Exit(Answer(pbsAmbiguous, pbiAmbiguousName));
          end;
          LCompletion := LAt;
        end;
        LAt := LDecl.NextNamed;
      end;

      if LCompletion >= 0 then
      begin
        Exit(OneSymbol(InEnvironment(FWorld.Symbol(AModule, LCompletion), FEnvironment)));
      end;
    end;
  end;

  if (LFullType >= 0) and (LCount > 1) then
  begin
    for LAt := 0 to LCount - 1 do
    begin

      if (LAt <> LFullType) and
        ((LSymbols[LAt].Declaration.Kind <> psdkType) or
          not (psdfForward in LSymbols[LAt].Declaration.Flags) or
          (LSymbols[LAt].Declaration.TypeForm <> LSymbols[LFullType].Declaration.TypeForm)) then
      begin
        Exit(SymbolAnswer(LSymbols));
      end;
    end;
    Exit(OneSymbol(LSymbols[LFullType]));
  end;
  { An unknown/unfinished imported interface cannot prove a name is absent.
    Returning Missing here would silently choose an earlier import or intrinsic.
    Known declarations stay usable; full overload/grammar meaning remains separate. }

  if AExternal and (LCount = 0) and (AScope = 0) and
    not ExportsKnown(AModule) then
  begin
    Exit(Answer(pbsUnresolved, pbiIncompleteSource, LSyntax.Scope(0).Name));
  end;
  Result := SymbolAnswer(LSymbols);
end;

function TBindingQuery.UsesObjPas(const ASite: INyxPascalBindingSite): TNyxPascalKnowledge;
var
  LContext: INyxPascalCompilerContext;
  LModule: TNyxText;
begin
  LModule := NyxPascalASCIIKey(FWorld.ModuleAt(ASite.ModuleIndex).Syntax.Scope(0).Name);

  if (LModule = 'system') or (LModule = 'objpas') then
  begin
    Exit(ppkFalse);
  end;
  LContext := ContextAt(ASite);
  Result := ppkUnknown;

  if LContext = nil then
  begin
    Exit;
  end;

  if LContext.Family = ppcPas2JS then
  begin
    Exit(ppkFalse);
  end;

  if LContext.Family <> ppcFPC then
  begin
    Exit;
  end;
  case LContext.Mode of
    ppmFPC, ppmTP:
      Result := ppkFalse;
    ppmObjFPC, ppmDelphi, ppmDelphiUnicode:
      Result := ppkTrue;
  else
    begin
      { Unqualified aliases in another mode need their own compiler contract. }
    end;
  end;
end;

function TBindingQuery.UnitVisible(const ASite: INyxPascalBindingSite;
  const AName: TNyxText): Boolean;
var
  LSyntax: INyxPascalSyntaxSnapshot;
  LAt: Integer;
begin
  LSyntax := FWorld.ModuleAt(ASite.ModuleIndex).Syntax;
  Result := (NyxPascalASCIIKey(LSyntax.Scope(0).Name) = NyxPascalASCIIKey(AName)) or
    (NyxPascalASCIIKey(AName) = 'system');

  if (FLimits.ImplicitScopes = pispCompilerUnits) and
    (NyxPascalASCIIKey(AName) = 'objpas') and (UsesObjPas(ASite) = ppkTrue) then
  begin
    Result := True;
  end;

  if Result then
  begin
    Exit;
  end;
  for LAt := 0 to LSyntax.ImportCount - 1 do
  begin
    Tick;

    if (LSyntax.ImportAt(LAt).Scope = 0) and
      (LSyntax.ImportAt(LAt).Span.Start < ASite.Caret) and
      (NyxPascalASCIIKey(LSyntax.ImportAt(LAt).Name) = NyxPascalASCIIKey(AName)) then
    begin
      Exit(True);
    end;
  end;
end;

function TBindingQuery.Imports(const ASite: INyxPascalBindingSite;
  const AName: TNyxText): INyxPascalResolution;
var
  LSyntax: INyxPascalSyntaxSnapshot;
  LImport: TNyxPascalSyntaxImport;
  LAt: Integer;
  LModule: Integer;
  LImplicit: TNyxPascalKnowledge;
begin
  LSyntax := FWorld.ModuleAt(ASite.ModuleIndex).Syntax;
  for LAt := LSyntax.ImportCount - 1 downto 0 do
  begin
    Tick;
    LImport := LSyntax.ImportAt(LAt);

    if (LImport.Scope <> 0) or (LImport.Span.Start >= ASite.Caret) then
    begin
      Continue;
    end;

    if LImport.Conditional then
    begin
      Exit(Answer(pbsUnresolved, pbiIncompleteSource));
    end;
    LModule := FWorld.FindModule(LImport.Name);

    if LModule < 0 then
    begin
      { A later missing unit can legally redeclare this name. Never skip it and
        confidently resolve an earlier use or an automatic System intrinsic. }
      Exit(Answer(pbsUnresolved, pbiMissingModule, LImport.Name));
    end;
    Result := Exact(LModule, 0, InterfaceEnd(LModule), AName, True);

    if Result.Status <> pbsMissing then
    begin
      Exit;
    end;
  end;

  if FLimits.ImplicitScopes = pispCompilerUnits then
  begin
    LImplicit := UsesObjPas(ASite);

    if LImplicit = ppkUnknown then
    begin
      Exit(Answer(pbsUnresolved, pbiCompilerContext));
    end;

    if LImplicit = ppkTrue then
    begin
      LModule := FWorld.FindModule('ObjPas');

      if LModule < 0 then
      begin
        Exit(Answer(pbsUnresolved, pbiMissingModule, 'ObjPas'));
      end;
      Result := Exact(LModule, 0, InterfaceEnd(LModule), AName, True);

      if Result.Status <> pbsMissing then
      begin
        Exit;
      end;
    end;
  end;
  LModule := FWorld.FindModule('System');

  if (LModule < 0) and (FLimits.ImplicitScopes = pispCompilerUnits) then
  begin
    Exit(Answer(pbsUnresolved, pbiMissingModule, 'System'));
  end;

  if (LModule >= 0) and (LModule <> ASite.ModuleIndex) then
  begin
    Result := Exact(LModule, 0, InterfaceEnd(LModule), AName, True);

    if Result.Status <> pbsMissing then
    begin
      Exit;
    end;
  end;
  Result := BuiltinType(ASite, AName, FLimits.ImplicitScopes = pispCompilerUnits);
end;

function TBindingQuery.Single(const ASite: INyxPascalBindingSite;
  const AName: TNyxText; AImports: Boolean): INyxPascalResolution;
var
  LScope: Integer;
  LSyntax: INyxPascalSyntaxSnapshot;
  LOwner: Integer;
  LClass: INyxPascalBoundSymbol;
  LClassType: INyxPascalResolution;
  LHeaderOwner: TNyxPascalSyntaxDeclaration;
  LHeaderLookup: Boolean;
begin
  LSyntax := FWorld.ModuleAt(ASite.ModuleIndex).Syntax;
  LScope := ASite.ScopeIndex;
  while LScope >= 0 do
  begin
    Tick;
    Result := ParameterAt(ASite, LScope, AName);

    if Result <> nil then
    begin
      Exit;
    end;
    Result := Exact(ASite.ModuleIndex, LScope, ASite.Caret, AName, False);

    if Result.Status <> pbsMissing then
    begin
      Exit;
    end;
    Result := ImplicitRoutine(ASite, LScope, AName);

    if Result <> nil then Exit;
    LOwner := LSyntax.Scope(LScope).OwnerDeclaration;

    if (LOwner >= 0) and (LSyntax.Declaration(LOwner).GenericSpan.Length > 0) and
      ((LSyntax.Declaration(LOwner).Kind <> psdkType) or
        (psdfIncomplete in LSyntax.Declaration(LOwner).Flags)) and (FEnvironment = nil) then
    begin
      { An unapplied routine still has a declared type header. Signature reads
        may resolve names outside that exact header's formal scope, without
        inventing an application frame or granting lookup to its body. }
      LHeaderLookup := False;

      if FBindingTypeTarget and (FTypeOwner <> nil) and
        (FTypeOwner.ModuleIndex = ASite.ModuleIndex) then
      begin
        LHeaderOwner := FTypeOwner.Declaration;

        if (LHeaderOwner.Kind = psdkRoutine) and (FTypeOwner.DeclarationIndex = LOwner) then
          LHeaderLookup := (ASite.Caret >= LHeaderOwner.HeaderSpan.Start) and
            (ASite.Caret < LHeaderOwner.HeaderSpan.Finish)
        else if (LHeaderOwner.Kind = psdkParameter) and (LHeaderOwner.Scope = LScope) then
          LHeaderLookup := (ASite.Caret >= LHeaderOwner.TypeSpan.Start) and
            (ASite.Caret < LHeaderOwner.TypeSpan.Finish);
      end;

      if not LHeaderLookup then Exit(Answer(pbsUnresolved, pbiGenericContext));
    end;

    if (LOwner >= 0) and (LSyntax.Declaration(LOwner).Kind = psdkType) and
      (NyxPascalASCIIKey(LSyntax.Declaration(LOwner).Name) = NyxPascalASCIIKey(AName)) then
    begin
      { An enclosing class/interface's own name is already in its lexical scope;
        an uncaptured ancestor cannot shadow that declaration with a nested type. }
      Exit(OneSymbol(InEnvironment(FWorld.Symbol(ASite.ModuleIndex, LOwner), FEnvironment)));
    end;

    if (LSyntax.Scope(LScope).Kind = psskMembers) or
      ((LOwner >= 0) and (LSyntax.Declaration(LOwner).OwnerName <> '')) then
    begin

      if LSyntax.Scope(LScope).Kind = psskMembers then
      begin

        if LOwner < 0 then
        begin
          Exit(Answer(pbsUnresolved, pbiIncompleteSource));
        end;
        { Each nested member scope owns its own header. Reusing the innermost
          CurrentClass while climbing would search that class repeatedly. }
        LClass := FWorld.Symbol(ASite.ModuleIndex, LOwner);
      end
      else
      begin
        LClass := CurrentClass(ASite);
      end;

      if LClass <> nil then
      begin
        LClass := InEnvironment(LClass, FEnvironment);

        if psdfIncomplete in LClass.Declaration.Flags then
        begin
          Result := OpenScopeInherited(LClass, ASite, AName);
        end
        else
        begin
          if (LClass.Declaration.GenericSpan.Length > 0) and (EnvironmentOf(LClass) = nil) then
          begin
            LClassType := FormalReceiver(LClass);
          end
          else
          begin
            LClassType := TypeOfSymbol(LClass);
          end;

          if LClassType.Status <> pbsBound then
          begin
            Exit(LClassType);
          end;

          if LSyntax.Scope(LScope).Kind = psskMembers then
          begin
            { Own lexical names were searched before this caret. A complete
              receiver search would incorrectly donate later nested names. }
            Result := InheritedMembers(LClass, ASite, AName, pmaInstance, pokNone, []);
          end
          else
          begin
            Result := Member(LClassType.BoundType, ASite, AName, pmaInstance);
          end;
        end;

        if Result.Status <> pbsMissing then
        begin
          Exit;
        end;
      end;
    end;
    LScope := LSyntax.Scope(LScope).Parent;
  end;

  if AImports then
  begin
    Result := Imports(ASite, AName);
  end
  else
  begin
    Result := Answer(pbsMissing, pbiMissingName);
  end;
end;

function TBindingQuery.Lookup(const ASite: INyxPascalBindingSite;
  const AName: TNyxText): INyxPascalResolution;
var
  LAt: Integer;
  LFirstDot: Integer;
  LPrefix: TNyxText;
  LTail: TNyxText;
  LModule: Integer;
  LLocal: INyxPascalResolution;
  LType: INyxPascalResolution;
  LRest: TNyxText;
  LPart: TNyxText;
  LAccess: TNyxPascalMemberAccess;
  LNamespaceFound: Boolean;
  LConstructor: Boolean;
begin
  Enter;
  try
    CheckSite(ASite);
    NyxPascalBindingName(AName);
    LFirstDot := Pos('.', AName);

    if LFirstDot = 0 then
    begin
      Exit(Single(ASite, AName, True));
    end;
    LLocal := Single(ASite, Copy(AName, 1, LFirstDot - 1), False);

    if LLocal.Status = pbsMissing then
    begin
      LNamespaceFound := False;
      { Longest directly visible module namespace; another captured module does
        not acquire use-clause authority merely by existing in this world. }
      for LAt := Length(AName) downto 1 do
      begin

        if AName[LAt] <> '.' then
        begin
          Continue;
        end;
        LPrefix := Copy(AName, 1, LAt - 1);

        if not UnitVisible(ASite, LPrefix) then
        begin
          Continue;
        end;
        LNamespaceFound := True;
        LModule := FWorld.FindModule(LPrefix);

        if LModule < 0 then
        begin

          if (NyxPascalASCIIKey(LPrefix) = 'system') and
            (FLimits.ImplicitScopes = pispDeclarationNames) then
          begin
            Exit(BuiltinType(ASite, Copy(AName, LAt + 1, MaxInt), True));
          end;
          Exit(Answer(pbsUnresolved, pbiMissingModule, LPrefix));
        end;
        LTail := Copy(AName, LAt + 1, MaxInt);
        LFirstDot := Pos('.', LTail);
        LPart := LTail;

        if LFirstDot > 0 then
        begin
          LPart := Copy(LTail, 1, LFirstDot - 1);
        end;

        if LModule = ASite.ModuleIndex then
        begin
          LLocal := Exact(LModule, 0, ASite.Caret, LPart, False);
        end
        else
        begin
          LLocal := Exact(LModule, 0, InterfaceEnd(LModule), LPart, True);
        end;

        if LFirstDot = 0 then
        begin

          if (LLocal.Status = pbsMissing) and (NyxPascalASCIIKey(LPrefix) = 'system') then
          begin
            Exit(BuiltinType(ASite, LPart, True));
          end;
          Exit(LLocal);
        end;
        LRest := Copy(LTail, LFirstDot + 1, MaxInt);
        Break;
      end;

      if LLocal.Status = pbsMissing then
      begin

        if LNamespaceFound then
        begin
          Exit(Answer(pbsMissing, pbiMissingName));
        end;
        { A qualified receiver can start with a type/value imported by USES,
          not only a local name or visible unit namespace. Keep the existing
          local/explicit-namespace routes first; ordinary imported lookup then
          retains use order and refuses uncaptured potential shadowing units. }
        LFirstDot := Pos('.', AName);
        LLocal := Single(ASite, Copy(AName, 1, LFirstDot - 1), True);
        LRest := Copy(AName, LFirstDot + 1, MaxInt);
      end;
    end
    else
    begin
      LRest := Copy(AName, LFirstDot + 1, MaxInt);
    end;

    if LLocal.Status <> pbsBound then
    begin
      Exit(LLocal);
    end;
    while LRest <> '' do
    begin
      Tick;

      if (LLocal.Count = 1) and (LLocal.SymbolAt(0).Declaration.Kind = psdkProperty) and
        ((LLocal.SymbolAt(0).Declaration.PropertyParameters.Length > 0) or
         (LLocal.SymbolAt(0).Declaration.PropertyAccess = [pspaWrite])) then
      begin
        Exit(Answer(pbsUnresolved, pbiUnsupportedType));
      end;

      LConstructor := (LLocal.Count > 0) and
        (LLocal.SymbolAt(0).Declaration.RoutineKind = prkConstructor);

      if (LLocal.BoundType = nil) and (LLocal.Count <> 1) and not LConstructor then
      begin
        Exit(Answer(pbsUnresolved, pbiOverloadContext));
      end;
      LAccess := pmaInstance;

      if (LLocal.Count = 1) and (LLocal.SymbolAt(0).Declaration.Kind = psdkType) then
      begin
        LAccess := pmaType;
      end;
      LType := LLocal;

      if LLocal.BoundType = nil then
      begin

        if LConstructor then LType := ConstructorResults(LLocal, ASite)
        else LType := TypeOfSymbol(LLocal.SymbolAt(0));
      end;

      if LType.Status <> pbsBound then
      begin
        Exit(LType);
      end;
      LFirstDot := Pos('.', LRest);
      LPart := LRest;

      if LFirstDot > 0 then
      begin
        LPart := Copy(LRest, 1, LFirstDot - 1);
      end;
      LLocal := Member(LType.BoundType, ASite, LPart, LAccess);

      if (LLocal.Status <> pbsBound) and not
        ((LLocal.Status = pbsOverloads) and (LFirstDot > 0) and (LLocal.Count > 0) and
         (LLocal.SymbolAt(0).Declaration.RoutineKind = prkConstructor)) then
      begin
        Exit(LLocal);
      end;

      if LFirstDot = 0 then
      begin
        Break;
      end;
      LRest := Copy(LRest, LFirstDot + 1, MaxInt);
    end;
    Result := LLocal;
  finally
    Leave;
  end;
end;

function TBindingQuery.CurrentClass(const ASite: INyxPascalBindingSite): INyxPascalBoundSymbol;
var
  LScope: Integer;
  LOwner: Integer;
  LSyntax: INyxPascalSyntaxSnapshot;
  LFound: INyxPascalResolution;
  LType: INyxPascalSemanticType;
begin
  Result := nil;
  LSyntax := FWorld.ModuleAt(ASite.ModuleIndex).Syntax;
  LScope := ASite.ScopeIndex;
  while LScope >= 0 do
  begin
    Tick;
    LOwner := LSyntax.Scope(LScope).OwnerDeclaration;

    if LOwner >= 0 then
    begin

      if LSyntax.Declaration(LOwner).Kind = psdkType then
      begin
        Exit(FWorld.Symbol(ASite.ModuleIndex, LOwner));
      end;

      if LSyntax.Declaration(LOwner).OwnerName <> '' then
      begin
        LFound := GenericImplementationOwner(ASite, FWorld.Symbol(ASite.ModuleIndex, LOwner));

        if LFound <> nil then
        begin

          if (LFound.Status = pbsBound) and (LFound.Count = 1) then Exit(LFound.SymbolAt(0));
          Exit;
        end;
        LFound := Lookup(FWorld.At(ASite.ModuleIndex, 0,
          LSyntax.Declaration(LOwner).HeaderSpan.Start), LSyntax.Declaration(LOwner).OwnerName);

        if (LFound.Status = pbsBound) and (LFound.Count = 1) then
        begin
          LFound := TypeOfSymbol(LFound.SymbolAt(0));

          if LFound.Status = pbsBound then
          begin
            LType := Unwrap(LFound.BoundType);

            if LType <> nil then
            begin
              Exit(LType.Definition);
            end;
          end;
        end;
        Exit;
      end;
    end;
    LScope := LSyntax.Scope(LScope).Parent;
  end;
end;

function TBindingQuery.Unwrap(const AType: INyxPascalSemanticType): INyxPascalSemanticType;
var
  LCount: Integer;
begin
  Result := AType;
  LCount := 0;
  while (Result <> nil) and (Result.Kind in [pstAlias, pstDistinct]) do
  begin
    Tick;
    Inc(LCount);

    if LCount > FLimits.MaximumDepth then
    begin
      raise ENyxPascalBindingLimit.Create('Semantic alias traversal exceeds its budget');
    end;
    Result := Result.Target;
  end;
end;

function TBindingQuery.BuiltinType(const ASite: INyxPascalBindingSite;
  const AName: TNyxText; ASystem: Boolean): INyxPascalResolution;
const
  CNames: array[TNyxPascalBuiltin] of TNyxText =
    ('', 'integer', 'byte', 'word', 'smallint', 'shortint', 'longint', 'longword',
      'int64', 'qword', 'nativeint', 'nativeuint', 'boolean', 'bytebool', 'wordbool',
      'longbool', 'char', 'ansichar', 'widechar', 'single', 'double', 'extended',
      'real', 'currency', 'pointer', 'ansistring', 'utf8string', 'unicodestring',
      'widestring', 'shortstring', 'comp');
var
  LValue: TNyxPascalBuiltin;
  LType: TSemanticType;
  LContext: INyxPascalCompilerContext;
  LBootstrap: INyxPascalCompilerContext;
  LFact: TNyxPascalSizeFact;
  LSystem: Integer;
  LFormat: TNyxPascalFloatingFormat;
  LBytes: Integer;
  LFloat: TFloatingType;
begin
  Result := Answer(pbsMissing, pbiMissingName);
  for LValue := Succ(Low(TNyxPascalBuiltin)) to High(TNyxPascalBuiltin) do
  begin
    Tick;

    if CNames[LValue] <> NyxPascalASCIIKey(AName) then
    begin
      Continue;
    end;
    LContext := ContextAt(ASite);
    LBootstrap := nil;

    if ASystem then
    begin
      LSystem := FWorld.FindModule('System');

      if LSystem >= 0 then
      begin
        { Only the actual System owner supplies compiler-root facts. Consumer
          modes, aliases and ordinary System.X measurements cannot manufacture
          pre-alias presence. Missing/imported source still refuses in lookup
          before reaching this fallback. }
        LBootstrap := NyxPascalBootstrapContext(FWorld.ModuleAt(LSystem).Source.InitialContext);
        { A terminal System primitive belongs to that unit's source context.
          The consumer or an ObjPas alias must not reassign its mode/identity. }
        LContext := ContextAt(FWorld.At(LSystem, 0, InterfaceEnd(LSystem)));
      end;
    end;

    if LBootstrap <> nil then
    begin
      LFact := NyxPascalCapturedSize(LBootstrap, NyxPascalSizeReference(AName));

      if LFact.State = pssUndeclared then
      begin
        Exit(Result);
      end;

      if LFact.State = pssUnknown then
      begin

        if (LBootstrap.Family <> ppcFPC) or (LBootstrap.Version.FullNumber = 0) or
          (LBootstrap.Mode = ppmUnknown) then
        begin
          { Incomplete output metadata still permits source inspection. It
            supplies a typed context gap, never an invalid native request. }
          Exit(Answer(pbsUnresolved, pbiCompilerContext, AName));
        end;
        Exit(Answer(pbsUnresolved, pbiCompilerContext, AName,
          NewNyxPascalPrimitiveRequirement(LBootstrap, NyxPascalSizeReference(AName),
            FWorld.ModuleAt(LSystem), FWorld.ModuleAt(ASite.ModuleIndex), -1, ASite.Caret)));
      end;
      LContext := LBootstrap;
    end;

    if (LContext = nil) or (LContext.Family = ppcUnknown) or (LContext.Mode = ppmUnknown) then
    begin
      Exit(Answer(pbsUnresolved, pbiCompilerContext));
    end;
    LFormat := pffUnknown;
    LBytes := 0;

    if (LValue in [pbbSingle,pbbDouble,pbbExtended,pbbCurrency,pbbComp]) and
      (LBootstrap <> nil) and (LContext.Family = ppcFPC) and
      (LContext.Version.FullNumber = 30301) and
      (NyxPascalCompilerScope(LContext) = pcsoSystemBootstrap) and
      (LContext.Symbol(NyxPascalDefine('CPUI386')).IsDefined = ppkTrue) then
    begin
      { Qualified compiler initialization creates these exact definitions on
        i386. The independent root presence/size observation must agree. Neither
        an authored alias nor a same-sized unrelated type can enter this path.
        Real is deliberately absent: this RTL declares it as type Double. }
      Tick;
      case LValue of
        pbbSingle: begin LFormat := pffBinary32; LBytes := 4; end;
        pbbDouble: begin LFormat := pffBinary64; LBytes := 8; end;
        pbbExtended: begin LFormat := pffX87Extended80; LBytes := 10; end;
        pbbCurrency: begin LFormat := pffCurrency64; LBytes := 8; end;
        pbbComp: begin LFormat := pffComp64; LBytes := 8; end;
      else
        begin LFormat := pffUnknown; LBytes := 0; end;
      end;
    end;

    if LFormat <> pffUnknown then
    begin

      if LFact.Bytes <> LBytes then Exit(Answer(pbsUnresolved,pbiCompilerContext,AName));
      LFloat := TFloatingType.Create;
      LType := LFloat;
      LFloat.FormatValue := LFormat;
      LFloat.ByteCount := LBytes;
    end
    else if LValue = pbbPointer then
    begin
      LType := TDataPointerType.Create;
      TDataPointerType(LType).CapturedContext := LContext;
      TDataPointerType(LType).InitialTargetContext := InitialPointerContext(ASite.ModuleIndex);

      if ASystem then
      begin

        if LSystem >= 0 then TDataPointerType(LType).InitialTargetContext := InitialPointerContext(LSystem);
      end;
    end
    else LType := TSemanticType.Create;
    LType.Owner := FWorld;
    LType.TypeKind := pstBuiltin;
    LType.Intrinsic := LValue;
    LType.IntrinsicScope := pioUnqualified;
    LType.IntrinsicName := NyxPascalSizeReference(AName);

    if ASystem then
    begin
      LType.IntrinsicScope := pioSystem;
      LType.IntrinsicName := NyxPascalSizeReference('System.' + AName);
    end;

    if LBootstrap <> nil then
    begin
      LType.IntrinsicScope := pioSystemBootstrap;
      LType.IntrinsicName := NyxPascalSizeReference(AName);
    end;
    LType.IntrinsicFacts := LContext;
    Result := TypeAnswer(LType);
    Exit;
  end;
end;

function TBindingQuery.GenericNames(const ASymbol: INyxPascalBoundSymbol;
  out ANames: TGenericNames): TNyxPascalBindingIssue;
var
  LParameters: TGenericParameters;
  LAt: Integer;
begin
  SetLength(ANames, 0);
  Result := GenericParameters(ASymbol, LParameters);

  if Result <> pbiNone then Exit;
  SetLength(ANames, Length(LParameters));
  for LAt := 0 to High(LParameters) do
  begin
    Tick;
    ANames[LAt] := LParameters[LAt].Name;
  end;
end;

function TBindingQuery.GenericParameters(const ASymbol: INyxPascalBoundSymbol;
  out AParameters: TGenericParameters): TNyxPascalBindingIssue;
var
  LLex: INyxPascalLexicalSnapshot;
  LSpan: TNyxPascalSpan;
  LToken: TNyxPascalToken;
  LAt: Integer;
  LSeen: Integer;
  LName: TNyxText;
  LFirst: Integer;
  LRequirement: TGenericRequirement;
  LRequirements: TGenericRequirements;
  LRequirementStart: Integer;
  LNamed: TGenericConstraintSpans;
  LNamedSpan: TNyxPascalSpan;
  LClosingSuffix: Boolean;

  procedure Next;
  begin
    repeat
      Inc(LAt);
      Tick;

      if LAt >= LLex.Count then Exit;
      LToken := LLex.Token(LAt);
    until not (LToken.Kind in [ptkWhitespace, ptkComment, ptkDirective]);
  end;

  function Closing: Boolean;
  begin
    Result := (LToken.Kind = ptkSymbol) and
      (((LToken.Symbol in [psoGreater, psoGreaterEqual]) and
        (LToken.Span.Start + 1 = LSpan.Finish)) or
       (LClosingSuffix and (LToken.Symbol = psoShiftRight) and
        (LToken.Span.Start + 2 = LSpan.Finish)));
  end;

  function NamedWindow(out AWindow: TNyxPascalSpan): Boolean;
  var
    LStart: Integer;
    LFinish: Integer;
    LAngle: Integer;
    LGroup: Integer;
  begin
    Result := False;
    LStart := LToken.Span.Start;
    LFinish := LStart;
    LAngle := 0;
    LGroup := 0;
    while LAt < LLex.Count do
    begin

      if (LToken.Kind = ptkEOF) or (LToken.Span.Start >= LSpan.Finish - 1) then Break;

      if LToken.Kind = ptkSymbol then
      begin

        if (LAngle = 0) and (LGroup = 0) and
          (LToken.Symbol in [psoComma, psoSemicolon, psoGreater, psoGreaterEqual]) then Break;
        case LToken.Symbol of
          psoLess: Inc(LAngle);
          psoGreater: Dec(LAngle);
          psoShiftRight:
            begin
              { The first character ends the named type; the second belongs to
                the surrounding parameter list. Neither source token is lost. }

              if (LAngle = 1) and (LGroup = 0) and
                (LToken.Span.Start + 2 = LSpan.Finish) then
              begin
                LFinish := LToken.Span.Start + 1;
                LAngle := 0;
                LClosingSuffix := True;
                Break;
              end;
              Dec(LAngle, 2);
            end;
          psoOpenParen, psoOpenBracket: Inc(LGroup);
          psoCloseParen, psoCloseBracket: Dec(LGroup);
        else
          { The existing type parser owns the meaning of the retained window. }
        end;

        if (LAngle < 0) or (LGroup < 0) then Exit;
      end;
      LFinish := LToken.Span.Finish;
      Next;
    end;

    if (LFinish <= LStart) or (LAngle <> 0) or (LGroup <> 0) then Exit;
    AWindow := NyxPascalSpan(LStart, LFinish - LStart);
    Result := True;
  end;
begin
  SetLength(AParameters, 0);
  LSpan := ASymbol.Declaration.GenericSpan;

  if LSpan.Length = 0 then
  begin
    Exit(pbiNone);
  end;
  LLex := FWorld.ModuleAt(ASymbol.ModuleIndex).Syntax.Lexical;
  LAt := LLex.TokenAt(LSpan.Start);

  Tick;
  LToken := LLex.Token(LAt);

  if (LToken.Kind <> ptkSymbol) or (LToken.Symbol <> psoLess) then
  begin
    Exit(pbiGenericContext);
  end;
  Next;
  while (LAt < LLex.Count) and (LToken.Span.Start < LSpan.Finish) do
  begin
    LFirst := Length(AParameters);
    LClosingSuffix := False;
    repeat

      if (LToken.Kind <> ptkWord) or
        ((LToken.Word <> pkwNone) and not LToken.Escaped) then
      begin
        Exit(pbiGenericContext);
      end;
      LName := LLex.Identifier(LAt);
      for LSeen := 0 to High(AParameters) do
      begin
        Tick;

        if NyxPascalASCIIKey(AParameters[LSeen].Name) = NyxPascalASCIIKey(LName) then
        begin
          Exit(pbiGenericContext);
        end;
      end;

      if Length(AParameters) >= FLimits.MaximumCandidates then
      begin
        raise ENyxPascalBindingLimit.Create('Generic parameters exceed their query budget');
      end;
      SetLength(AParameters, Length(AParameters) + 1);
      AParameters[High(AParameters)].Name := LName;
      AParameters[High(AParameters)].NameSpan := LToken.Span;
      Next;

      if (LToken.Kind <> ptkSymbol) or (LToken.Symbol <> psoComma) then Break;
      Next;
    until False;
    LRequirements := [];
    SetLength(LNamed, 0);

    if (LToken.Kind = ptkSymbol) and (LToken.Symbol = psoColon) then
    begin
      Next;
      LRequirementStart := LToken.Span.Start;
      repeat

        if LToken.Kind <> ptkWord then Exit(pbiGenericConstraint);

        if not LToken.Escaped and (LToken.Word in [pkwClass, pkwRecord, pkwConstructor]) then
        begin
          case LToken.Word of
            pkwClass: LRequirement := gcrClass;
            pkwRecord: LRequirement := gcrRecord;
          else
            LRequirement := gcrConstructor;
          end;

          if (LRequirement in LRequirements) or (gcrRecord in LRequirements) or
            ((LRequirement = gcrRecord) and ((LRequirements <> []) or (Length(LNamed) <> 0))) then
          begin
            Exit(pbiGenericConstraint);
          end;
          Include(LRequirements, LRequirement);
          Next;
        end
        else
        begin

          if (gcrRecord in LRequirements) or
            (not LToken.Escaped and not (LToken.Word in [pkwNone, pkwSpecialize])) or
            not NamedWindow(LNamedSpan) then Exit(pbiGenericConstraint);

          if Length(LNamed) >= FLimits.MaximumCandidates then
          begin
            raise ENyxPascalBindingLimit.Create('Generic requirements exceed their query budget');
          end;
          SetLength(LNamed, Length(LNamed) + 1);
          LNamed[High(LNamed)] := LNamedSpan;
        end;

        if (LToken.Kind <> ptkSymbol) or (LToken.Symbol <> psoComma) then Break;
        Next;
      until False;
      for LSeen := LFirst to High(AParameters) do
      begin
        Tick;
        AParameters[LSeen].Requirements := LRequirements;
        AParameters[LSeen].RequirementSpan := NyxPascalSpan(LRequirementStart,
          LToken.Span.Start - LRequirementStart);

        if LClosingSuffix then
        begin
          AParameters[LSeen].RequirementSpan := NyxPascalSpan(LRequirementStart,
            LSpan.Finish - 1 - LRequirementStart);
        end;
        AParameters[LSeen].NamedSpans := LNamed;
      end;
    end;

    if Closing then Exit(pbiNone);

    if (LToken.Kind <> ptkSymbol) or (LToken.Symbol <> psoSemicolon) then Exit(pbiGenericContext);
    Next;
  end;
  Result := pbiGenericContext;
end;

function TGenericEnvironment.Site: INyxPascalBindingSite;
begin
  Result := ApplicationSite;
end;

function TBindingQuery.CheckGenericRequirements(const ASymbol: INyxPascalBoundSymbol;
  const AFrame: INyxPascalGenericEnvironment;
  const AParameters: TGenericParameters): TNyxPascalBindingIssue;
var
  LAt: Integer;
  LType: INyxPascalSemanticType;
  LView: INyxPascalTranslatedSyntax;
  LOrigin: Integer;
  LContext: INyxPascalCompilerContext;
  LForm: TNyxPascalSyntaxTypeForm;
  LValue: Boolean;
  LApplication: IGenericApplication;
  LSite: INyxPascalBindingSite;
  LIssue: TNyxPascalBindingIssue;
  LFormal: INyxPascalFormalParameter;
  LBoundAt: Integer;
  LClassGuarantee: Boolean;
begin
  Result := pbiNone;

  if (AFrame = nil) or (AFrame.Count <> Length(AParameters)) then Exit(pbiGenericArity);
  if not SameSymbol(AFrame.Owner, ASymbol) then Exit(pbiGenericContext);
  LView := FWorld.ModuleAt(ASymbol.ModuleIndex);
  for LAt := 0 to High(AParameters) do
  begin
    Tick;

    if (NyxPascalASCIIKey(AFrame.Name(LAt)) <> NyxPascalASCIIKey(AParameters[LAt].Name)) or
      (AFrame.Argument(LAt) = nil) or (AFrame.Argument(LAt).World <> FWorld) then
    begin
      Exit(pbiGenericContext);
    end;

    if (AParameters[LAt].Requirements = []) and (Length(AParameters[LAt].NamedSpans) = 0) then Continue;
    LOrigin := LView.OriginAt(AParameters[LAt].RequirementSpan.Start);

    if LOrigin < 0 then Exit(pbiCompilerContext);
    LContext := LView.Source.State(LView.Origin(LOrigin).State);
    { The mode-sensitive value rule is qualified to the inspected compiler.
      No foreign/future compiler or unknown mode inherits its eligibility. }

    if (LContext.Family <> ppcFPC) or (LContext.Version.FullNumber <> 30301) or
      not (LContext.Mode in [ppmFPC, ppmObjFPC, ppmDelphi, ppmDelphiUnicode]) then
    begin
      Exit(pbiCompilerContext);
    end;

    if (gcrConstructor in AParameters[LAt].Requirements) and
      not (LContext.Mode in [ppmDelphi, ppmDelphiUnicode]) then Exit(pbiGenericConstraint);
    { Mode-sensitive checks run before FPC replays the generic body. Preserve
      that actual caller origin; an alternate frame without it cannot certify
      a constrained specialization. The site points outward to the same world. }

    if not Supports(AFrame, IGenericApplication, LApplication) then Exit(pbiCompilerContext);
    LSite := LApplication.Site;

    if LSite = nil then Exit(pbiCompilerContext);
    CheckSite(LSite);
    LContext := ContextAt(LSite);

    if (LContext = nil) or (LContext.Family <> ppcFPC) or
      (LContext.Version.FullNumber <> 30301) or
      not (LContext.Mode in [ppmFPC, ppmObjFPC, ppmDelphi, ppmDelphiUnicode]) then
    begin
      Exit(pbiCompilerContext);
    end;
    LType := Unwrap(AFrame.Argument(LAt));

    if (LType = nil) or (LType.World <> FWorld) then Exit(pbiGenericConstraint);
    LForm := pstfNone;

    if (LType.Kind = pstDeclared) and (LType.Definition <> nil) then
    begin
      LForm := LType.Definition.Declaration.TypeForm;
    end;

    if LType.Kind = pstFormal then
    begin

      if not Supports(LType, INyxPascalFormalParameter, LFormal) then Exit(pbiGenericContext);
      LClassGuarantee := False;
      for LBoundAt := 0 to LFormal.BoundCount - 1 do
      begin
        Tick;
        LClassGuarantee := LClassGuarantee or
          (LFormal.BoundType(LBoundAt).Definition.Declaration.TypeForm = pstfClass);
      end;

      if (AParameters[LAt].Requirements * [gcrClass, gcrConstructor] <> []) and
        not LClassGuarantee then Exit(pbiGenericContext);

      if gcrRecord in AParameters[LAt].Requirements then
      begin

        if not (pgrRecord in LFormal.Requirements) or
          (not (LContext.Mode in [ppmDelphi, ppmDelphiUnicode]) and
            (LFormal.Context.Mode in [ppmDelphi, ppmDelphiUnicode])) then Exit(pbiGenericContext);
      end;
      LIssue := CheckNamedGenericRequirements(ASymbol, AFrame, AParameters[LAt], LType);

      if LIssue <> pbiNone then Exit(LIssue);
      Continue;
    end;

    if AParameters[LAt].Requirements * [gcrClass, gcrConstructor] <> [] then
    begin

      if LForm <> pstfClass then Exit(pbiGenericConstraint);
    end;

    if gcrRecord in AParameters[LAt].Requirements then
    begin
      LValue := LForm = pstfRecord;

      if LContext.Mode in [ppmDelphi, ppmDelphiUnicode] then
      begin
        LValue := LValue or (LForm = pstfObject) or (LType.Kind in [pstEnumeration, pstSubrange]) or
          ((LType.Kind = pstBuiltin) and (LType.Builtin in [pbbInteger, pbbByte, pbbWord,
            pbbSmallInt, pbbShortInt, pbbLongInt, pbbLongWord, pbbInt64, pbbQWord,
            pbbNativeInt, pbbNativeUInt, pbbBoolean, pbbByteBool, pbbWordBool,
            pbbLongBool, pbbChar, pbbAnsiChar, pbbWideChar, pbbSingle, pbbDouble,
            pbbExtended, pbbReal, pbbCurrency, pbbComp]));
      end;

      if not LValue then Exit(pbiGenericConstraint);
    end;
    LIssue := CheckNamedGenericRequirements(ASymbol, AFrame, AParameters[LAt], LType);

    if LIssue <> pbiNone then Exit(LIssue);
  end;
end;

function TBindingQuery.GenericRequirementType(const ASymbol: INyxPascalBoundSymbol;
  const AFrame: INyxPascalGenericEnvironment; const ASpan: TNyxPascalSpan): INyxPascalResolution;
var
  LSyntax: INyxPascalTypeSyntax;
  LEnvironment: INyxPascalGenericEnvironment;
  LOwner: INyxPascalBoundSymbol;
  LBindingConstraint: Boolean;
begin
  LSyntax := NewNyxPascalTypeSyntax(FWorld.ModuleAt(ASymbol.ModuleIndex).Syntax.Lexical,
    ASpan, TNyxPascalTypeOptions.Defaults, FCancel);

  if not LSyntax.Complete then Exit(Answer(pbsUnresolved, pbiGenericConstraint));
  LEnvironment := FEnvironment;
  LOwner := FTypeOwner;
  LBindingConstraint := FBindingConstraint;
  FEnvironment := AFrame;
  FTypeOwner := ASymbol;
  FBindingConstraint := True;
  try
    { The declaration's exact source position owns every name. A supplied
      argument cannot manufacture a formal contract that FPC cannot declare. }
    Result := BindNode(FWorld.At(ASymbol.ModuleIndex, ASymbol.Declaration.Scope,
      ASpan.Start), LSyntax, LSyntax.Root);
  finally
    FEnvironment := LEnvironment;
    FTypeOwner := LOwner;
    FBindingConstraint := LBindingConstraint;
  end;
end;

function TBindingQuery.FormalParameter(const ASymbol: INyxPascalBoundSymbol;
  AIndex: Integer; const AParameters: TGenericParameters): INyxPascalResolution;
var
  LType: TFormalType;
  LLease: INyxPascalSemanticType;
  LView: INyxPascalTranslatedSyntax;
  LOrigin: Integer;
  LAt: Integer;
  LClasses: Integer;
  LInterfaces: Integer;
  LBound: INyxPascalResolution;
  LBoundType: INyxPascalSemanticType;
  LSystem: Integer;

  procedure Append(const AType: INyxPascalSemanticType);
  begin
    Tick;

    if Length(LType.FBounds) >= FLimits.MaximumCandidates then
    begin
      raise ENyxPascalBindingLimit.Create('Formal bounds exceed their query budget');
    end;
    SetLength(LType.FBounds, Length(LType.FBounds) + 1);
    LType.FBounds[High(LType.FBounds)] := AType;
  end;
begin
  Enter;
  try
    LView := FWorld.ModuleAt(ASymbol.ModuleIndex);
    LOrigin := LView.OriginAt(AParameters[AIndex].NameSpan.Start);

    if LOrigin < 0 then Exit(Answer(pbsUnresolved, pbiCompilerContext));
    LType := TFormalType.Create;
    LLease := LType;
    LType.Owner := FWorld;
    LType.TypeKind := pstFormal;
    LType.FDeclarationOwner := FWorld.Symbol(ASymbol.ModuleIndex, ASymbol.DeclarationIndex);
    LType.FIndex := AIndex;
    LType.FParameter := AParameters[AIndex];
    LType.FOrigin := LView.Origin(LOrigin);
    LType.FContext := LView.Source.State(LType.FOrigin.State);
    LClasses := 0;
    LInterfaces := 0;

    if (LType.FParameter.Requirements <> []) or (Length(LType.FParameter.NamedSpans) <> 0) then
    begin
      LOrigin := LView.OriginAt(LType.FParameter.RequirementSpan.Start);

      if LOrigin < 0 then Exit(Answer(pbsUnresolved, pbiCompilerContext));
      LType.FContext := LView.Source.State(LView.Origin(LOrigin).State);

      if (LType.FContext.Family <> ppcFPC) or (LType.FContext.Version.FullNumber <> 30301) or
        not (LType.FContext.Mode in [ppmFPC, ppmObjFPC, ppmDelphi, ppmDelphiUnicode]) then
      begin
        Exit(Answer(pbsUnresolved, pbiCompilerContext));
      end;

      if (gcrConstructor in LType.FParameter.Requirements) and
        not (LType.FContext.Mode in [ppmDelphi, ppmDelphiUnicode]) then
      begin
        Exit(Answer(pbsUnresolved, pbiGenericConstraint));
      end;
    end;
    for LAt := 0 to High(LType.FParameter.NamedSpans) do
    begin
      Tick;
      LBound := GenericRequirementType(ASymbol, nil, LType.FParameter.NamedSpans[LAt]);

      if LBound.Status <> pbsBound then Exit(LBound);
      LBoundType := Unwrap(LBound.BoundType);

      if (LBoundType = nil) or (LBoundType.Kind <> pstDeclared) or
        (LBoundType.Definition = nil) then Exit(Answer(pbsUnresolved, pbiGenericConstraint));
      case LBoundType.Definition.Declaration.TypeForm of
        pstfClass:
          begin
            Inc(LClasses);

            if (LClasses > 1) or (gcrClass in LType.FParameter.Requirements) then
            begin
              Exit(Answer(pbsUnresolved, pbiGenericConstraint));
            end;
          end;
        pstfInterface: Inc(LInterfaces);
      else
        Exit(Answer(pbsUnresolved, pbiGenericConstraint));
      end;
      Append(LBoundType);
    end;

    if (LClasses = 0) and ((LType.FParameter.Requirements * [gcrClass, gcrConstructor] <> []) or
      (LInterfaces > 1)) then
    begin
      LSystem := FWorld.FindModule('System');

      if LSystem < 0 then Exit(Answer(pbsUnresolved, pbiMissingModule, 'System'));
      LBound := Exact(LSystem, 0, InterfaceEnd(LSystem), 'TObject', True);

      if (LBound.Status <> pbsBound) or (LBound.Count <> 1) then Exit(LBound);
      LBound := TypeOfSymbol(LBound.SymbolAt(0));

      if LBound.Status <> pbsBound then Exit(LBound);
      LBoundType := Unwrap(LBound.BoundType);

      if (LBoundType = nil) or (LBoundType.Kind <> pstDeclared) or
        (LBoundType.Definition = nil) or (LBoundType.Definition.Declaration.TypeForm <> pstfClass) then
      begin
        Exit(Answer(pbsUnresolved, pbiGenericContext));
      end;
      Append(LBoundType);
    end;
    Result := TypeAnswer(LLease);
  finally
    Leave;
  end;
end;

function TBindingQuery.FormalReceiver(const ASymbol: INyxPascalBoundSymbol): INyxPascalResolution;
var
  LParameters: TGenericParameters;
  LIssue: TNyxPascalBindingIssue;
  LFrame: TGenericEnvironment;
  LLease: INyxPascalGenericEnvironment;
  LSymbol: TBoundSymbol;
  LSymbolLease: INyxPascalBoundSymbol;
  LAt: Integer;
  LType: INyxPascalResolution;
begin
  Enter;
  try
    LIssue := GenericParameters(ASymbol, LParameters);

    if LIssue <> pbiNone then Exit(Answer(pbsUnresolved, LIssue));
    LFrame := TGenericEnvironment.Create;
    LLease := LFrame;
    LFrame.DeclarationOwner := FWorld.Symbol(ASymbol.ModuleIndex, ASymbol.DeclarationIndex);
    LFrame.ApplicationSite := FWorld.At(ASymbol.ModuleIndex, ASymbol.Declaration.Scope,
      ASymbol.Declaration.TypeSpan.Start);
    SetLength(LFrame.Names, Length(LParameters));
    SetLength(LFrame.Types, Length(LParameters));
    for LAt := 0 to High(LParameters) do
    begin
      Tick;
      LType := FormalParameter(ASymbol, LAt, LParameters);

      if LType.Status <> pbsBound then Exit(LType);
      LFrame.Names[LAt] := LParameters[LAt].Name;
      LFrame.Types[LAt] := LType.BoundType;
    end;
    LSymbol := TBoundSymbol.Create;
    LSymbolLease := LSymbol;
    LSymbol.Owner := FWorld;
    LSymbol.ModuleOrdinal := ASymbol.ModuleIndex;
    LSymbol.DeclarationOrdinal := ASymbol.DeclarationIndex;
    LSymbol.Bindings := LLease;
    { A source receiver runs through the same bounded requirement checks. Its
      arguments remain formal shapes; it is never a concrete instantiation. }
    Result := TypeOfSymbol(LSymbolLease);
  finally
    Leave;
  end;
end;

function TBindingQuery.CheckNamedGenericRequirements(const ASymbol: INyxPascalBoundSymbol;
  const AFrame: INyxPascalGenericEnvironment; const AParameter: TGenericParameter;
  const AArgument: INyxPascalSemanticType): TNyxPascalBindingIssue;
var
  LAt: Integer;
  LClasses: Integer;
  LInterfaces: Integer;
  LResolved: INyxPascalResolution;
  LRequired: INyxPascalSemanticType;
  LEnvironment: INyxPascalGenericEnvironment;
  LOwner: INyxPascalBoundSymbol;
  LRelation: TNyxPascalKnowledge;
begin
  Result := pbiNone;
  LClasses := 0;
  LInterfaces := 0;
  LEnvironment := FEnvironment;
  LOwner := FTypeOwner;
  FEnvironment := AFrame;
  FTypeOwner := ASymbol;
  try
    for LAt := 0 to High(AParameter.NamedSpans) do
    begin
      Tick;
      LResolved := GenericRequirementType(ASymbol, AFrame, AParameter.NamedSpans[LAt]);

      if LResolved.Status <> pbsBound then Exit(LResolved.Issue);
      LRequired := Unwrap(LResolved.BoundType);

      if (LRequired = nil) or (LRequired.Kind <> pstDeclared) or
        (LRequired.Definition = nil) then Exit(pbiGenericConstraint);
      case LRequired.Definition.Declaration.TypeForm of
        pstfClass:
          begin
            Inc(LClasses);

            if (LClasses > 1) or (gcrClass in AParameter.Requirements) then Exit(pbiGenericConstraint);
          end;
        pstfInterface: Inc(LInterfaces);
      else
        Exit(pbiGenericConstraint);
      end;
      LRelation := ConstraintRelated(AArgument, LRequired);

      if LRelation = ppkUnknown then Exit(pbiGenericContext);
      if LRelation = ppkFalse then Exit(pbiGenericConstraint);
    end;
    { FPC constructs a class intersection for two or more interfaces. A single
      interface argument cannot stand in for that implementing class. }

    if (LInterfaces > 1) and ((AArgument.Kind <> pstDeclared) or
      (AArgument.Definition = nil) or
      (AArgument.Definition.Declaration.TypeForm <> pstfClass)) then Exit(pbiGenericConstraint);
  finally
    FEnvironment := LEnvironment;
    FTypeOwner := LOwner;
  end;
end;

function TBindingQuery.ConstraintRelated(const AActual,
  ARequired: INyxPascalSemanticType): TNyxPascalKnowledge;
var
  LAt: Integer;
  LTrail: Integer;
  LActualForm: TNyxPascalSyntaxTypeForm;
  LRequiredForm: TNyxPascalSyntaxTypeForm;
  LBase: INyxPascalResolution;
  LType: INyxPascalSemanticType;
  LRelated: TNyxPascalKnowledge;
  LHasClass: Boolean;
  LUnknown: Boolean;
  LSystem: Integer;
  LFormal: INyxPascalFormalParameter;
begin
  Enter;
  try

    if (AActual <> nil) and (AActual.Kind = pstFormal) then
    begin

      if not Supports(AActual, INyxPascalFormalParameter, LFormal) then Exit(ppkUnknown);
      for LAt := 0 to LFormal.BoundCount - 1 do
      begin
        Tick;
        LType := LFormal.BoundType(LAt);
        { A class bound guarantees its inherited implementation entries. A lone
          interface bound can also denote a class implementing only that exact
          interface, so it cannot manufacture a separate parent entry. }

        if SameType(LType, ARequired) then Exit(ppkTrue);

        if LType.Definition.Declaration.TypeForm = pstfClass then
        begin
          LRelated := ConstraintRelated(LType, ARequired);

          if LRelated = ppkTrue then Exit(ppkTrue);
        end;
      end;
      Exit(ppkUnknown);
    end;

    if (AActual = nil) or (AActual.Kind <> pstDeclared) or (AActual.Definition = nil) then Exit(ppkFalse);
    LActualForm := AActual.Definition.Declaration.TypeForm;
    LRequiredForm := ARequired.Definition.Declaration.TypeForm;

    if not (LActualForm in [pstfClass, pstfInterface]) or
      ((LRequiredForm = pstfClass) and (LActualForm <> pstfClass)) then Exit(ppkFalse);
    if SameType(AActual, ARequired) then Exit(ppkTrue);
    for LAt := 0 to High(FConstraintPath) do
    begin
      Tick;

      if SameType(FConstraintPath[LAt], AActual) then Exit(ppkUnknown);
    end;
    LTrail := Length(FConstraintPath);
    SetLength(FConstraintPath, LTrail + 1);
    FConstraintPath[LTrail] := AActual;
    try
      LAt := 0;
      LHasClass := False;
      LUnknown := False;
      while True do
      begin
        LBase := Bases(AActual.Definition, LAt);

        if LBase.Status = pbsMissing then Break;
        if LBase.Status <> pbsBound then Exit(ppkUnknown);
        LType := Unwrap(LBase.BoundType);

        if (LType = nil) or (LType.Kind <> pstDeclared) or (LType.Definition = nil) then Exit(ppkUnknown);
        LHasClass := LHasClass or (LType.Definition.Declaration.TypeForm = pstfClass);
        LRelated := ppkFalse;

        if (LActualForm = pstfClass) and (LType.Definition.Declaration.TypeForm = pstfInterface) then
        begin
          { The inspected compiler compares a class's actual implemented entries.
            It does not substitute a parent contract for a child-only entry. }

          if (LRequiredForm = pstfInterface) and SameType(LType, ARequired) then LRelated := ppkTrue;
        end
        else if LType.Definition.Declaration.TypeForm = LActualForm then
        begin
          LRelated := ConstraintRelated(LType, ARequired);
        end
        else
        begin
          LUnknown := True;
        end;

        if LRelated = ppkTrue then Exit(ppkTrue);
        LUnknown := LUnknown or (LRelated = ppkUnknown);
        Inc(LAt);
      end;
      { A class listing only interfaces still has the compiler's System.TObject
        parent. Resolve that exact declaration; an unrelated local TObject is
        never allowed to supply the relationship. }

      if (LActualForm = pstfClass) and not LHasClass and
        (AActual.Definition.Declaration.AncestorSpan.Length <> 0) then
      begin
        LSystem := FWorld.FindModule('System');

        if LSystem < 0 then Exit(ppkUnknown);
        LBase := Exact(LSystem, 0, InterfaceEnd(LSystem), 'TObject', True);

        if (LBase.Status <> pbsBound) or (LBase.Count <> 1) then Exit(ppkUnknown);
        LBase := TypeOfSymbol(LBase.SymbolAt(0));

        if LBase.Status <> pbsBound then Exit(ppkUnknown);
        LType := Unwrap(LBase.BoundType);

        if (LType = nil) or (LType.Kind <> pstDeclared) or (LType.Definition = nil) or
          (LType.Definition.Declaration.TypeForm <> pstfClass) then Exit(ppkUnknown);
        LRelated := ConstraintRelated(LType, ARequired);

        if LRelated = ppkTrue then Exit(ppkTrue);
        LUnknown := LUnknown or (LRelated = ppkUnknown);
      end;

      if LUnknown then Exit(ppkUnknown);
      Result := ppkFalse;
    finally
      SetLength(FConstraintPath, LTrail);
    end;
  finally
    Leave;
  end;
end;

function TBindingQuery.InEnvironment(const ASymbol: INyxPascalBoundSymbol;
  const AEnvironment: INyxPascalGenericEnvironment): INyxPascalBoundSymbol;
var
  LFrame: INyxPascalGenericEnvironment;
  LScope: Integer;
  LOwner: INyxPascalBoundSymbol;
  LSymbol: TBoundSymbol;
  LNested: Boolean;
begin
  Result := ASymbol;
  LFrame := AEnvironment;
  while LFrame <> nil do
  begin
    Tick;
    LOwner := LFrame.Owner;
    LNested := SameSymbol(ASymbol, LOwner);

    if ASymbol.ModuleIndex = LOwner.ModuleIndex then
    begin
      LScope := ASymbol.Declaration.Scope;
      while LScope >= 0 do
      begin
        Tick;
        LNested := LNested or (LScope = LOwner.Declaration.NestedScope);
        LScope := FWorld.ModuleAt(ASymbol.ModuleIndex).Syntax.Scope(LScope).Parent;
      end;
    end;

    if LNested then
    begin
      LSymbol := TBoundSymbol.Create;
      LSymbol.Owner := FWorld;
      LSymbol.ModuleOrdinal := ASymbol.ModuleIndex;
      LSymbol.DeclarationOrdinal := ASymbol.DeclarationIndex;
      LSymbol.Bindings := LFrame;
      Exit(LSymbol);
    end;
    LFrame := LFrame.Parent;
  end;
end;

function TBindingQuery.GenericImplementationOwner(const ASite: INyxPascalBindingSite;
  const ARoutine: INyxPascalBoundSymbol): INyxPascalResolution;
var
  LLex: INyxPascalLexicalSnapshot;
  LAt: Integer;
  LFirst: Integer;
  LLast: Integer;
  LToken: TNyxPascalToken;
  LType: INyxPascalTypeSyntax;
  LFound: INyxPascalResolution;
  LParameters: TGenericParameters;
  LIssue: TNyxPascalBindingIssue;
  LArgument: Integer;
  LResolving: Boolean;
  LHasArguments: Boolean;
  LAnyArguments: Boolean;
  LAngle: Integer;

  procedure Next;
  begin
    repeat
      Inc(LAt);
      Tick;

      if LAt >= LLex.Count then Exit;
      LToken := LLex.Token(LAt);
    until not (LToken.Kind in [ptkWhitespace, ptkComment]);
  end;
begin
  Result := nil;

  if (ARoutine.Declaration.Kind <> psdkRoutine) or (ARoutine.Declaration.OwnerName = '') then Exit;

  LLex := FWorld.ModuleAt(ARoutine.ModuleIndex).Syntax.Lexical;
  LAt := LLex.TokenAt(ARoutine.Declaration.HeaderSpan.Start) - 1;
  Next;

  if (LToken.Kind = ptkWord) and not LToken.Escaped and (LToken.Word = pkwClass) then Next;

  if (LToken.Kind <> ptkWord) or LToken.Escaped or
    not (LToken.Word in [pkwFunction, pkwProcedure, pkwConstructor, pkwDestructor]) then
  begin
    Exit(Answer(pbsUnresolved, pbiGenericContext));
  end;
  Next;
  LFirst := LToken.Span.Start;
  LLast := -1;
  LHasArguments := False;
  LAnyArguments := False;
  LAngle := 0;
  while (LAt < LLex.Count) and (LToken.Span.Start < ARoutine.Declaration.NameSpan.Finish) do
  begin

    if LToken.Kind = ptkSymbol then
    begin
      case LToken.Symbol of
        psoLess:
          begin
            Inc(LAngle);
            LAnyArguments := True;
          end;
        psoGreater: Dec(LAngle);
        psoShiftRight: Dec(LAngle, 2);
        psoDot:
          begin

            if LAngle = 0 then
            begin
              LLast := LAt;
              LHasArguments := LAnyArguments;
            end;
          end;
      else
        { Parameter delimiters retain the existing type parser's ownership. }
      end;
    end;
    Next;
  end;

  if not LHasArguments then Exit;

  if psdfConditional in ARoutine.Declaration.Flags then Exit(Answer(pbsUnresolved, pbiIncompleteSource));
  { A frozen prefix often ends inside a method body. Its complete owner header
    can still qualify parameters; body incompleteness is not header authority.
    Nongeneric headers above retain their prior ordinary lookup path. }

  if (LLast < 0) or (LLex.Token(LLast).Kind <> ptkSymbol) or
    (LLex.Token(LLast).Symbol <> psoDot) then Exit(Answer(pbsUnresolved, pbiGenericContext));
  LType := NewNyxPascalTypeSyntax(LLex, NyxPascalSpan(LFirst,
    LLex.Token(LLast).Span.Start - LFirst), TNyxPascalTypeOptions.Defaults, FCancel);

  if not LType.Complete then Exit(Answer(pbsUnresolved, pbiGenericContext));

  if LType.Node(LType.Root).Kind <> ptxGeneric then Exit;
  LResolving := FResolvingImplementationOwner;
  FResolvingImplementationOwner := True;
  try
    LFound := TypeDeclaration(FWorld.At(ASite.ModuleIndex, 0, LFirst), LType,
      LType.Child(LType.Root, ptrBase));
  finally
    FResolvingImplementationOwner := LResolving;
  end;

  if (LFound.Status <> pbsBound) or (LFound.Count <> 1) then Exit(LFound);

  if (LFound.SymbolAt(0).Declaration.Kind <> psdkType) or
    not (LFound.SymbolAt(0).Declaration.TypeForm in [pstfClass, pstfInterface, pstfRecord, pstfObject]) then
  begin
    Exit(Answer(pbsUnresolved, pbiGenericContext));
  end;
  LIssue := GenericParameters(LFound.SymbolAt(0), LParameters);

  if LIssue <> pbiNone then Exit(Answer(pbsUnresolved, LIssue));
  LArgument := LType.Child(LType.Root, ptrArgument);
  for LAt := 0 to High(LParameters) do
  begin
    Tick;

    if (LArgument < 0) or (LType.Node(LArgument).Role <> ptrArgument) or
      (LType.Node(LArgument).Kind <> ptxName) or
      (NyxPascalASCIIKey(LType.Node(LArgument).Name) <> NyxPascalASCIIKey(LParameters[LAt].Name)) then
    begin
      Exit(Answer(pbsUnresolved, pbiGenericContext));
    end;
    LArgument := LType.Node(LArgument).NextSibling;
  end;

  if (Length(LParameters) = 0) or (LArgument >= 0) then Exit(Answer(pbsUnresolved, pbiGenericContext));
  { An exact declaration and ordered plain formal names qualify this header.
    A constructed owner with literals or renamed/missing parameters does not. }
  Result := LFound;
end;

function TBindingQuery.MethodReceiver(const ASite: INyxPascalBindingSite;
  const ARoutine: INyxPascalBoundSymbol; out AHasSelf: Boolean): INyxPascalResolution;
var
  LOwner, LPrototype: INyxPascalBoundSymbol;
  LFound, LReceiver: INyxPascalResolution;
  LType: INyxPascalSemanticType;
  LClassReference: TSemanticType;
  LAt: Integer;
  LClass, LStatic: Boolean;
  LSyntax: INyxPascalSyntaxSnapshot;
begin
  AHasSelf := False;
  LFound := GenericImplementationOwner(ASite, ARoutine);

  if LFound = nil then
  begin
    LFound := Lookup(FWorld.At(ARoutine.ModuleIndex, 0,
      ARoutine.Declaration.HeaderSpan.Start), ARoutine.Declaration.OwnerName);
  end;

  if (LFound.Status <> pbsBound) or (LFound.Count <> 1) then Exit(LFound);
  LOwner := LFound.SymbolAt(0);

  if (LOwner.Declaration.GenericSpan.Length > 0) and (EnvironmentOf(LOwner) = nil) then
  begin
    LReceiver := FormalReceiver(LOwner);
  end
  else
  begin
    LReceiver := TypeOfSymbol(LOwner);
  end;

  if LReceiver.Status <> pbsBound then Exit(LReceiver);
  LType := Unwrap(LReceiver.BoundType);

  if (LType = nil) or (LType.Kind <> pstDeclared) or (LType.Definition = nil) or
    not (LType.Definition.Declaration.TypeForm in [pstfClass, pstfRecord, pstfObject]) then
  begin
    Exit(Answer(pbsUnresolved, pbiUnsupportedType));
  end;
  LOwner := LType.Definition;
  LSyntax := FWorld.ModuleAt(LOwner.ModuleIndex).Syntax;
  { STATIC belongs to the member declaration and is normally omitted from the
    implementation header. Inspect the exact owner's own declaration set. We
    do not choose an overload by its name or infer parameter compatibility: all
    candidates must agree on receiver storage before sharing this one fact. }
  LFound := Exact(LOwner.ModuleIndex, LOwner.Declaration.NestedScope,
    LSyntax.Scope(LOwner.Declaration.NestedScope).Span.Finish, ARoutine.Declaration.Name, False);

  if not (LFound.Status in [pbsBound, pbsOverloads]) then Exit(LFound);
  LClass := False;
  LStatic := False;
  for LAt := 0 to LFound.Count - 1 do
  begin
    Tick;
    LPrototype := LFound.SymbolAt(LAt);

    if (LPrototype.Declaration.Kind <> psdkRoutine) or
      (LPrototype.Declaration.RoutineKind <> ARoutine.Declaration.RoutineKind) or
      (LPrototype.Declaration.Flags * [psdfIncomplete, psdfConditional, psdfUnsupported] <> []) then
    begin
      Exit(Answer(pbsUnresolved, pbiIncompleteSource));
    end;

    if (LAt > 0) and
      ((LClass <> (psdfClassMember in LPrototype.Declaration.Flags)) or
       (LStatic <> (psdfStatic in LPrototype.Declaration.Flags))) then
    begin
      Exit(Answer(pbsUnresolved, pbiOverloadContext));
    end;
    LClass := psdfClassMember in LPrototype.Declaration.Flags;
    LStatic := psdfStatic in LPrototype.Declaration.Flags;
  end;

  if (LFound.Count = 0) or (LClass <> (psdfClassMember in ARoutine.Declaration.Flags)) then
  begin
    Exit(Answer(pbsUnresolved, pbiIncompleteSource));
  end;
  AHasSelf := not LStatic;

  if LClass and not LStatic then
  begin

    if LOwner.Declaration.TypeForm <> pstfClass then Exit(Answer(pbsUnresolved, pbiUnsupportedType));
    LClassReference := TSemanticType.Create;
    LClassReference.Owner := FWorld;
    LClassReference.TypeKind := pstClassOf;
    LClassReference.Element := LReceiver.BoundType;
    Exit(TypeAnswer(LClassReference));
  end;
  Result := LReceiver;
end;

function TBindingQuery.ImplicitRoutine(const ASite: INyxPascalBindingSite;
  AScope: Integer; const AName: TNyxText): INyxPascalResolution;
var
  LSyntax: INyxPascalSyntaxSnapshot;
  LRoutine: INyxPascalBoundSymbol;
  LOwner, LAt: Integer;
  LKey: TNyxText;
  LContext, LHeaderContext: INyxPascalCompilerContext;
  LParsed: INyxPascalTypeSyntax;
  LEnvironment: INyxPascalGenericEnvironment;
  LTypeOwner: INyxPascalBoundSymbol;
  LHasSelf: Boolean;
begin
  Result := nil;
  LKey := NyxPascalASCIIKey(AName);

  if FBindingTypeTarget then Exit;

  if (LKey <> 'self') and (LKey <> 'result') then Exit;
  LSyntax := FWorld.ModuleAt(ASite.ModuleIndex).Syntax;
  LOwner := LSyntax.Scope(AScope).OwnerDeclaration;

  if LOwner < 0 then Exit;
  LRoutine := FWorld.Symbol(ASite.ModuleIndex, LOwner);

  if (LRoutine.Declaration.Kind <> psdkRoutine) or
    (ASite.Caret < LRoutine.Declaration.HeaderSpan.Finish) then Exit;

  if (LKey = 'self') and (LRoutine.Declaration.OwnerName = '') then Exit;

  if (LKey = 'result') and (LRoutine.Declaration.RoutineKind <> prkFunction) then Exit;
  { A complete header remains useful while its body is being typed. Earlier
    malformed/conditional/unsupported syntax must still refuse; the missing
    final body terminator alone does not erase its declared result contract. }

  if LRoutine.Declaration.Flags * [psdfConditional, psdfUnsupported, psdfForward] <> [] then
  begin
    Exit(Answer(pbsUnresolved, pbiIncompleteSource));
  end;
  for LAt := 0 to LSyntax.DiagnosticCount - 1 do
  begin
    Tick;

    if (LSyntax.Diagnostic(LAt).Span.Start >= LRoutine.Declaration.HeaderSpan.Start) and
      (LSyntax.Diagnostic(LAt).Span.Start < LRoutine.Declaration.HeaderSpan.Finish) then
    begin
      Exit(Answer(pbsUnresolved, pbiIncompleteSource));
    end;
  end;
  LContext := ContextAt(ASite);

  if (LContext = nil) and (ASite.Caret > 0) then
  begin
    { Completion carets may sit just after an identifier, before a synthetic
      separator introduced for a removed comment. Only its immediately owned
      preceding scalar can supply that same expression's source context. }
    LContext := ContextAt(FWorld.At(ASite.ModuleIndex, AScope, ASite.Caret - 1));
  end;
  LHeaderContext := ContextAt(FWorld.At(ASite.ModuleIndex, LRoutine.Declaration.Scope,
    LRoutine.Declaration.HeaderSpan.Start));

  if (LContext = nil) or (LHeaderContext = nil) or
    (LContext.Family <> ppcFPC) or (LHeaderContext.Family <> ppcFPC) or
    (LContext.Version.FullNumber <> 30301) or (LHeaderContext.Version.FullNumber <> 30301) or
    not (LContext.Mode in [ppmObjFPC, ppmDelphi, ppmDelphiUnicode]) or
    not (LHeaderContext.Mode in [ppmObjFPC, ppmDelphi, ppmDelphiUnicode]) then
  begin
    Exit(Answer(pbsUnresolved, pbiCompilerContext));
  end;

  if LKey = 'self' then
  begin
    { Class lifecycle routines are static initialization/finalization bodies,
      not class methods with a metaclass parameter. Their CLASS keyword alone
      cannot grant either an instance or a class-reference Self. Ordinary lexical
      lookup may still find an explicitly declared name in an outer scope. }

    if (psdfClassMember in LRoutine.Declaration.Flags) and
      (LRoutine.Declaration.RoutineKind in [prkConstructor, prkDestructor]) then Exit;
    Result := MethodReceiver(ASite, LRoutine, LHasSelf);

    if (Result.Status = pbsBound) and not LHasSelf then Result := nil;
    Exit;
  end;

  if LRoutine.Declaration.TypeSpan.Length = 0 then Exit(Answer(pbsUnresolved, pbiMissingType));
  LParsed := FWorld.ModuleAt(ASite.ModuleIndex).TypeOfDeclaration(LOwner,
    TNyxPascalTypeOptions.Defaults, FCancel);

  if not LParsed.Complete then Exit(Answer(pbsUnresolved, pbiUnsupportedType));
  LEnvironment := FEnvironment;
  LTypeOwner := FTypeOwner;
  FEnvironment := EnvironmentOf(LRoutine);
  FTypeOwner := LRoutine;
  try
    Result := BindNode(FWorld.At(ASite.ModuleIndex, LRoutine.Declaration.NestedScope,
      LRoutine.Declaration.TypeSpan.Start), LParsed, LParsed.Root);
  finally
    FEnvironment := LEnvironment;
    FTypeOwner := LTypeOwner;
  end;
end;

function TBindingQuery.ParameterAt(const ASite: INyxPascalBindingSite;
  AScope: Integer; const AName: TNyxText): INyxPascalResolution;
var
  LOwner: INyxPascalBoundSymbol;
  LDecl: Integer;
  LImplementation: INyxPascalResolution;
begin
  Result := nil;
  LOwner := nil;
  LDecl := FWorld.ModuleAt(ASite.ModuleIndex).Syntax.Scope(AScope).OwnerDeclaration;

  if LDecl >= 0 then
  begin
    LOwner := FWorld.Symbol(ASite.ModuleIndex, LDecl);

    if LOwner.Declaration.Kind = psdkRoutine then
    begin
      { A method's own parameters shadow its class parameters. Preserve both
        source owners rather than replacing the inner lexical contract. }
      Result := ParameterFromOwner(ASite, LOwner, AName);

      if Result <> nil then Exit;
    end;
    LImplementation := nil;

    if not FResolvingImplementationOwner then LImplementation := GenericImplementationOwner(ASite, LOwner);

    if LImplementation <> nil then
    begin

      if (LImplementation.Status <> pbsBound) or (LImplementation.Count <> 1) then Exit(LImplementation);
      LOwner := LImplementation.SymbolAt(0);
    end;
  end;
  { A routine result is written in the outer scope, yet still belongs to its
    exact qualified implementation header. Validate that owner independently. }

  if not FResolvingImplementationOwner and (FTypeOwner <> nil) and
    (FTypeOwner.ModuleIndex = ASite.ModuleIndex) and (FTypeOwner.Declaration.Scope = AScope) and
    (FTypeOwner.Declaration.Kind = psdkRoutine) and (FTypeOwner.Declaration.OwnerName <> '') then
  begin
    Result := ParameterFromOwner(ASite, FTypeOwner, AName);

    if Result <> nil then Exit;
    LImplementation := GenericImplementationOwner(ASite, FTypeOwner);

    if LImplementation <> nil then
    begin

      if (LImplementation.Status <> pbsBound) or (LImplementation.Count <> 1) then Exit(LImplementation);
      LOwner := LImplementation.SymbolAt(0);
    end;
  end;
  { Alias expressions and ancestor clauses use the declaration's outer scope,
    yet their placeholders belong to the declaration currently being bound. }

  if (FTypeOwner <> nil) and (FTypeOwner.ModuleIndex = ASite.ModuleIndex) and
    (FTypeOwner.Declaration.Scope = AScope) and
    (FTypeOwner.Declaration.GenericSpan.Length > 0) and
    ((FTypeOwner.Declaration.Kind <> psdkRoutine) or (FTypeOwner.Declaration.OwnerName = '')) then
  begin
    LOwner := FTypeOwner;
  end;

  Result := ParameterFromOwner(ASite, LOwner, AName);
end;

function TBindingQuery.ParameterFromOwner(const ASite: INyxPascalBindingSite;
  const AOwner: INyxPascalBoundSymbol; const AName: TNyxText): INyxPascalResolution;
var
  LParameters: TGenericParameters;
  LIssue: TNyxPascalBindingIssue;
  LFrame: INyxPascalGenericEnvironment;
  LAt: Integer;
begin
  Result := nil;

  if (AOwner = nil) or (AOwner.Declaration.GenericSpan.Length = 0) then
  begin
    Exit;
  end;
  LIssue := GenericParameters(AOwner, LParameters);

  if LIssue <> pbiNone then
  begin
    Exit(Answer(pbsUnresolved, LIssue));
  end;
  for LAt := 0 to High(LParameters) do
  begin
    Tick;

    if NyxPascalASCIIKey(LParameters[LAt].Name) = NyxPascalASCIIKey(AName) then
    begin
      { Eligibility of a supplied class cannot prove that a formal parameter
        was an inheritable contract at this earlier declaration position. }

      if FBindingConstraint or (ASite.Caret < AOwner.Declaration.GenericSpan.Finish) then
      begin
        Exit(Answer(pbsUnresolved, pbiGenericContext));
      end;
      LFrame := FEnvironment;
      while LFrame <> nil do
      begin
        Tick;

        if SameSymbol(LFrame.Owner, AOwner) then
        begin
          Exit(TypeAnswer(LFrame.Argument(LAt)));
        end;
        LFrame := LFrame.Parent;
      end;
      Exit(FormalParameter(AOwner, LAt, LParameters));
    end;
  end;
end;

function TBindingQuery.TypeOfSymbol(const ASymbol: INyxPascalBoundSymbol): INyxPascalResolution;
var
  LEnvironment: INyxPascalGenericEnvironment;
  LOwner: INyxPascalBoundSymbol;
begin
  CheckSymbol(ASymbol);
  LEnvironment := FEnvironment;
  LOwner := FTypeOwner;
  FEnvironment := EnvironmentOf(ASymbol);
  FTypeOwner := ASymbol;
  try
    Result := TypeOfSymbolCore(ASymbol);
  finally
    FEnvironment := LEnvironment;
    FTypeOwner := LOwner;
  end;
end;

function TBindingQuery.RoutineSignature(const ARoutine: INyxPascalBoundSymbol): INyxPascalResolution;
var
  LDeclaration, LParameter: TNyxPascalSyntaxDeclaration;
  LView: INyxPascalTranslatedSyntax;
  LSignature: TDeclaredSignature;
  LLease: INyxPascalSemanticType;
  LResolved: INyxPascalResolution;
  LTypeSyntax: INyxPascalTypeSyntax;
  LAt, LCount: Integer;
  LSymbol: INyxPascalBoundSymbol;
  LUntyped: TSemanticType;
  LEnvironment: INyxPascalGenericEnvironment;
  LOwner: INyxPascalBoundSymbol;
  LTypeTarget, LDefaultSeen: Boolean;
  LGeneric: TGenericParameters;
  LIssue: TNyxPascalBindingIssue;
begin
  Enter;
  try
    CheckSymbol(ARoutine);
    LDeclaration := ARoutine.Declaration;

    if LDeclaration.Kind <> psdkRoutine then Exit(Answer(pbsUnresolved, pbiUnsupportedType));

    if (LDeclaration.Flags * [psdfConditional, psdfUnsupported, psdfIncompleteHeader] <> []) or
      (LDeclaration.HeaderSpan.Length = 0) or (LDeclaration.NestedScope < 0) then
      Exit(Answer(pbsUnresolved, pbiIncompleteSource));
    LView := FWorld.ModuleAt(ARoutine.ModuleIndex);

    if (LView.Syntax.Scope(LDeclaration.NestedScope).Kind <> psskRoutine) or
      (LView.Syntax.Scope(LDeclaration.NestedScope).OwnerDeclaration <> ARoutine.DeclarationIndex) then
      Exit(Answer(pbsUnresolved, pbiIncompleteSource));

    if LDeclaration.GenericSpan.Length > 0 then
    begin
      LIssue := GenericParameters(ARoutine, LGeneric);

      if LIssue <> pbiNone then Exit(Answer(pbsUnresolved, LIssue));
    end;
    LSignature := TDeclaredSignature.Create;
    LLease := LSignature;
    LSignature.Owner := FWorld;
    LSignature.Declared := ARoutine;
    LSignature.TypeKind := pstProcedure;
    LEnvironment := FEnvironment;
    LOwner := FTypeOwner;
    LTypeTarget := FBindingTypeTarget;
    FEnvironment := EnvironmentOf(ARoutine);
    FTypeOwner := ARoutine;
    FBindingTypeTarget := True;
    try
      { Bind the authored return expression directly, preserving an unapplied
        generic's formal identities. A signature is not its instantiated call. }
      if LDeclaration.TypeSpan.Length > 0 then
      begin
        LTypeSyntax := LView.TypeOfDeclaration(ARoutine.DeclarationIndex,
          TNyxPascalTypeOptions.Defaults, FCancel);

        if not LTypeSyntax.Complete then Exit(Answer(pbsUnresolved, pbiUnsupportedType));
        LResolved := BindNode(FWorld.At(ARoutine.ModuleIndex, LDeclaration.NestedScope,
          LDeclaration.TypeSpan.Start), LTypeSyntax, LTypeSyntax.Root);

        if LResolved.Status <> pbsBound then Exit(LResolved);
        LSignature.Element := LResolved.BoundType;
        LSignature.TypeKind := pstFunction;
      end
      else if LDeclaration.RoutineKind = prkFunction then
        Exit(Answer(pbsUnresolved, pbiMissingType));
      LDefaultSeen := False;
      LAt := LView.Syntax.Scope(LDeclaration.NestedScope).FirstDeclaration;
      while LAt >= 0 do
      begin
        Tick;
        LParameter := LView.Syntax.Declaration(LAt);

        { Parameters precede result storage and all body-local declarations.
          Header queries neither inspect nor pay to walk the routine's body. }
        if LParameter.Kind <> psdkParameter then Break;

        if LParameter.Scope <> LDeclaration.NestedScope then
          Exit(Answer(pbsUnresolved, pbiIncompleteSource));

        if (LParameter.Kind = psdkParameter) and
          (LParameter.Scope = LDeclaration.NestedScope) then
        begin

          if LParameter.Flags * [psdfIncomplete, psdfConditional, psdfUnsupported] <> [] then
            Exit(Answer(pbsUnresolved, pbiIncompleteSource));
          LSymbol := InEnvironment(FWorld.Symbol(ARoutine.ModuleIndex, LAt), FEnvironment);

          if LParameter.TypeSpan.Length > 0 then
          begin
            LResolved := TypeOfSymbol(LSymbol);

            if LResolved.Status <> pbsBound then Exit(LResolved);
          end
          else
          begin

            if LParameter.ParameterMode = pspmValue then
              Exit(Answer(pbsUnresolved, pbiMissingType));
            LUntyped := TSemanticType.Create;
            LUntyped.Owner := FWorld;
            LUntyped.TypeKind := pstUntyped;
            LResolved := TypeAnswer(LUntyped);
          end;
          LCount := Length(LSignature.Names);

          if LCount >= FLimits.MaximumCandidates then
            raise ENyxPascalBindingLimit.Create('Declared routine parameters exceed their query budget');

          if psdfDefaultArgument in LParameter.Flags then
          begin

            if LParameter.ExpressionSpan.Length = 0 then
              Exit(Answer(pbsUnresolved, pbiIncompleteSource));
            LDefaultSeen := True;
          end
          else
          begin

            if LDefaultSeen then Exit(Answer(pbsUnresolved, pbiUnsupportedType));
            Inc(LSignature.RequiredCount);
          end;
          SetLength(LSignature.Names, LCount + 1);
          SetLength(LSignature.Modes, LCount + 1);
          SetLength(LSignature.Types, LCount + 1);
          SetLength(LSignature.Parameters, LCount + 1);
          LSignature.Names[LCount] := LParameter.Name;
          LSignature.Modes[LCount] := LParameter.ParameterMode;
          LSignature.Types[LCount] := LResolved.BoundType;
          LSignature.Parameters[LCount] := LSymbol;
        end;
        LAt := LParameter.Next;
      end;
      Result := TypeAnswer(LLease);
    finally
      FEnvironment := LEnvironment;
      FTypeOwner := LOwner;
      FBindingTypeTarget := LTypeTarget;
    end;
  finally
    Leave;
  end;
end;

function TBindingQuery.ConstantAnswer(const AType: INyxPascalSemanticType;
  const AScalar: TNyxPascalConstantValue; const AReal: TNyxPascalIEEEValue): INyxPascalResolution;
var LNominal, LLeaf, LLease: INyxPascalSemanticType;
  LLiteral: INyxPascalLiteralType; LExpression: INyxPascalConstantExpression;
  LIdentity: INyxPascalIntrinsicIdentity; LValue: TNumericExpressionType;
  LRealValue: TRealExpressionType;
  LIEEEValue: TIEEEExpressionType;
begin
  LNominal := AType;

  if Supports(LNominal,INyxPascalLiteralType,LLiteral) then LNominal := LLiteral.SpecializationType;

  if Supports(LNominal,INyxPascalConstantExpression,LExpression) then LNominal := LExpression.ResultType;

  if LNominal = nil then Exit(Answer(pbsUnresolved,pbiUnsupportedType));

  if (AReal.Kind = pifUnknown) and not (AScalar.Kind in [pvvInteger,pvvBoolean]) then
    Exit(TypeAnswer(LNominal));
  LLeaf := LNominal;
  while (LLeaf <> nil) and (LLeaf.Kind in [pstAlias,pstDistinct]) do
  begin Tick; LLeaf := LLeaf.Target; end;

  if (LLeaf = nil) or (LLeaf.Kind <> pstBuiltin) or
    not Supports(LLeaf,INyxPascalIntrinsicIdentity,LIdentity) then
    Exit(Answer(pbsUnresolved,pbiCompilerContext));

  if AReal.Kind = pifFinite then
  begin
    LRealValue := TRealExpressionType.Create; LLease := LRealValue;
    LValue := LRealValue; LRealValue.FRealValue := AReal.FiniteValue;
    LRealValue.FIEEEValue := AReal;
  end
  else if AReal.Kind in [pifInfinity,pifNaN] then
  begin
    LIEEEValue := TIEEEExpressionType.Create; LLease := LIEEEValue;
    LValue := LIEEEValue; LIEEEValue.FIEEEValue := AReal;
  end
  else
  begin
    LValue := TNumericExpressionType.Create; LLease := LValue; LValue.FConstant := AScalar;
  end;
  LValue.Owner := FWorld; LValue.TypeKind := pstBuiltin; LValue.Intrinsic := LLeaf.Builtin;
  LValue.IntrinsicScope := LIdentity.IntrinsicOrigin;
  LValue.IntrinsicName := LIdentity.IntrinsicReference;
  LValue.IntrinsicFacts := LIdentity.IntrinsicContext;
  LValue.FResult := LNominal;

  if LNominal.Kind in [pstAlias,pstDistinct] then
  begin
    { Value evidence decorates the actual nominal shape. Ordinary member/type
      consumers must retain the declaration too, not only generic deduction. }
    LValue.TypeKind := LNominal.Kind; LValue.Intrinsic := pbbNone;
    LValue.Declared := LNominal.Definition; LValue.Element := LNominal.Target;
  end;
  Result := TypeAnswer(LLease);
end;

procedure TBindingQuery.AdmitStringUnits(ACount: Integer);
begin

  if (ACount < 0) or (ACount > FLimits.MaximumSteps-FSteps) then
    raise ENyxPascalBindingLimit.Create('String unit storage exceeds its semantic query budget');
  NyxPascalAdmitAnalysis(FCancel,papFrozenRow,ACount);
  Inc(FSteps,ACount);
end;

function TBindingQuery.StringAnswer(const AType: INyxPascalSemanticType;
  const AValue: INyxPascalStringConstant): INyxPascalResolution;
var LNominal, LLeaf, LLease: INyxPascalSemanticType;
  LLiteral: INyxPascalLiteralType; LExpression: INyxPascalConstantExpression;
  LIdentity: INyxPascalIntrinsicIdentity; LValue: TStringExpressionType; LAt: Integer;
begin
  LNominal := AType;

  if Supports(LNominal,INyxPascalLiteralType,LLiteral) then LNominal := LLiteral.SpecializationType;

  if Supports(LNominal,INyxPascalConstantExpression,LExpression) then LNominal := LExpression.ResultType;

  if (LNominal = nil) or (AValue = nil) then Exit(Answer(pbsUnresolved,pbiUnsupportedType));
  LLeaf := Unwrap(LNominal);

  if (LLeaf = nil) or not Supports(LLeaf,INyxPascalIntrinsicIdentity,LIdentity) then
    Exit(Answer(pbsUnresolved,pbiCompilerContext));
  LValue := TStringExpressionType.Create; LLease := LValue;
  LValue.Owner := FWorld; LValue.TypeKind := pstBuiltin; LValue.Intrinsic := LLeaf.Builtin;
  LValue.IntrinsicScope := LIdentity.IntrinsicOrigin; LValue.IntrinsicName := LIdentity.IntrinsicReference;
  LValue.IntrinsicFacts := LIdentity.IntrinsicContext; LValue.FResult := LNominal;
  LValue.FForm := AValue.Form; LValue.FEncoding := AValue.Encoding; LValue.FWide := AValue.Wide;
  AdmitStringUnits(AValue.UnitCount);
  SetLength(LValue.FUnits,AValue.UnitCount);
  for LAt := 0 to High(LValue.FUnits) do
  begin Tick; LValue.FUnits[LAt] := AValue.CodeUnit(LAt); end;

  if LNominal.Kind in [pstAlias,pstDistinct] then
  begin
    LValue.TypeKind := LNominal.Kind; LValue.Intrinsic := pbbNone;
    LValue.Declared := LNominal.Definition; LValue.Element := LNominal.Target;
  end;
  Result := TypeAnswer(LLease);
end;

function TBindingQuery.TypeOfSymbolCore(const ASymbol: INyxPascalBoundSymbol): INyxPascalResolution;
var
  LDecl: TNyxPascalSyntaxDeclaration;
  LType: TSemanticType;
  LSyntax: INyxPascalTypeSyntax;
  LResolved: INyxPascalResolution;
  LAt: Integer;
  LCount: Integer;
  LScope: Integer;
  LSite: INyxPascalBindingSite;
  LFrame: INyxPascalGenericEnvironment;
  LIssue: TNyxPascalBindingIssue;
  LParameters: TGenericParameters;
  LAccess: TNyxPascalMemberAccess;
  LLiteral: INyxPascalLiteralType;
  LExpression: INyxPascalConstantExpression;
  LReal: INyxPascalIEEEConstant;
  LString: INyxPascalStringConstant;
  LScalarValue: TNyxPascalConstantValue;
  LRealValue: TNyxPascalIEEEValue;
  LView: INyxPascalTranslatedSyntax;
  LCaret: Integer;
  LConstantContext: INyxPascalCompilerContext;
begin
  Enter;
  try
    CheckSymbol(ASymbol);
    LDecl := ASymbol.Declaration;

    if LDecl.Flags * [psdfIncomplete, psdfConditional, psdfUnsupported] <> [] then
    begin
      Exit(Answer(pbsUnresolved, pbiIncompleteSource));
    end;

    if (LDecl.Kind = psdkType) and (LDecl.TypeForm in
      [pstfRecordHelper, pstfClassHelper, pstfTypeHelper]) then
    begin
      { Parsed helper declarations retain their own source/member scopes. They
        supply neither ordinary record/class storage nor helper lookup rules;
        do not re-enter their structured body through alias type parsing. }
      Exit(Answer(pbsUnresolved, pbiUnsupportedType, LDecl.Name));
    end;

    if LDecl.Kind = psdkLabel then
    begin
      { Labels inhabit the lexical namespace (including DECLARED and shadowing),
        but cannot donate a variable/type or an expression-member receiver. }
      Exit(Answer(pbsUnresolved, pbiUnsupportedType, LDecl.Name));
    end;

    if (LDecl.GenericSpan.Length > 0) then
    begin
      LIssue := GenericParameters(ASymbol, LParameters);

      if LIssue <> pbiNone then
      begin
        Exit(Answer(pbsUnresolved, LIssue));
      end;
      LFrame := FEnvironment;
      while (LFrame <> nil) and not SameSymbol(LFrame.Owner, ASymbol) do
      begin
        Tick;
        LFrame := LFrame.Parent;
      end;

      if LFrame = nil then
      begin
        Exit(Answer(pbsUnresolved, pbiGenericContext));
      end;
      LIssue := CheckGenericRequirements(ASymbol, LFrame, LParameters);

      if LIssue <> pbiNone then Exit(Answer(pbsUnresolved, LIssue));
    end;

    if LDecl.Kind = psdkType then
    begin

      if LDecl.TypeForm in [pstfClass, pstfInterface, pstfRecord, pstfObject] then
      begin

        if (LDecl.NestedScope < 0) or (psdfForward in LDecl.Flags) then
        begin
          Exit(Answer(pbsUnresolved, pbiIncompleteSource));
        end;
        LType := TSemanticType.Create;
        LType.Owner := FWorld;
        LType.TypeKind := pstDeclared;
        LType.Declared := ASymbol;
        Exit(TypeAnswer(LType));
      end;
      for LAt := 0 to High(FAliases) do
      begin
        Tick;

        if SameSymbol(FAliases[LAt], ASymbol) then
        begin
          Exit(Answer(pbsUnresolved, pbiRecursiveType));
        end;
      end;
    end;

    if (LDecl.Kind = psdkConstant) and (LDecl.TypeSpan.Length = 0) and
      (LDecl.ExpressionSpan.Length > 0) then
    begin
      LConstantContext := ContextAt(FWorld.At(ASymbol.ModuleIndex,LDecl.Scope,LDecl.ExpressionSpan.Start));

      if (LConstantContext = nil) or (LConstantContext.Family <> ppcFPC) or
        (LConstantContext.Version.FullNumber <> 30301) or
        not (LConstantContext.Mode in [ppmDelphi,ppmDelphiUnicode]) then
        Exit(Answer(pbsUnresolved,pbiCompilerContext,'constant declaration'));
      for LAt := 0 to High(FConstants) do
      begin
        Tick;

        if SameBinding(FConstants[LAt],ASymbol) then
          Exit(Answer(pbsUnresolved,pbiRecursiveType,LDecl.Name));
      end;
      LCount := Length(FConstants); SetLength(FConstants,LCount+1); FConstants[LCount] := ASymbol;
      try
        { A constant's initializer sees its declaration scope and earlier source
          context. A later use cannot retag its minimum, FPU or visible names. }
        LView := FWorld.ModuleAt(ASymbol.ModuleIndex);
        LAt := LView.OriginAt(LDecl.ExpressionSpan.Finish-1);

        if LAt < 0 then Exit(Answer(pbsUnresolved,pbiIncompleteSource,LDecl.Name));
        Inc(LAt);
        while (LAt < LView.OriginCount) and
          (LView.Origin(LAt).VirtualSpan.Start < LDecl.ExpressionSpan.Finish) do
        begin Tick; Inc(LAt); end;
        LCaret := LView.Syntax.Lexical.Scalars;

        if LAt < LView.OriginCount then LCaret := LView.Origin(LAt).VirtualSpan.Start;
        { Token-union ends can sit in synthetic whitespace. Use the following
          exact origin for site context, just as ordinary expression callers do. }
        LSite := FWorld.At(ASymbol.ModuleIndex,LDecl.Scope,LCaret);
        LResolved := ExpressionType(LSite,LDecl.ExpressionSpan,LAccess);

        if LResolved.Status <> pbsBound then Exit(LResolved);

        if LAccess <> pmaInstance then Exit(Answer(pbsUnresolved,pbiWrongMemberKind,LDecl.Name));
        LScalarValue := TNyxPascalConstantValue.Unknown; LRealValue := TNyxPascalIEEEValue.Unknown;

        if Supports(LResolved.BoundType,INyxPascalLiteralType,LLiteral) then LScalarValue := LLiteral.Value
        else if Supports(LResolved.BoundType,INyxPascalConstantExpression,LExpression) then
          LScalarValue := LExpression.ConstantValue;

        if Supports(LResolved.BoundType,INyxPascalIEEEConstant,LReal) then LRealValue := LReal.IEEEValue;

        if Supports(LResolved.BoundType,INyxPascalStringConstant,LString) then
          Exit(StringAnswer(LResolved.BoundType,LString));

        if (LRealValue.Kind <> pifUnknown) or (LScalarValue.Kind in [pvvInteger,pvvBoolean]) then
          Exit(ConstantAnswer(LResolved.BoundType,LScalarValue,LRealValue));
        Exit(Answer(pbsUnresolved,pbiMissingType,LDecl.Name));
      finally SetLength(FConstants,LCount); end;
    end;

    if LDecl.TypeSpan.Length = 0 then
    begin
      Exit(Answer(pbsUnresolved, pbiMissingType));
    end;
    LCount := Length(FAliases);
    SetLength(FAliases, LCount + 1);
    FAliases[LCount] := ASymbol;
    try
      LSyntax := FWorld.ModuleAt(ASymbol.ModuleIndex).TypeOfDeclaration(ASymbol.DeclarationIndex,
        TNyxPascalTypeOptions.Defaults, FCancel);

      if not LSyntax.Complete then
      begin
        Exit(Answer(pbsUnresolved, pbiUnsupportedType));
      end;
      LScope := LDecl.Scope;

      if (LDecl.Kind = psdkRoutine) and (LDecl.NestedScope >= 0) then
      begin
        LScope := LDecl.NestedScope;
      end;
      LSite := FWorld.At(ASymbol.ModuleIndex, LScope, LDecl.TypeSpan.Start);
      LResolved := BindNode(LSite, LSyntax, LSyntax.Root);

      if LResolved.Status <> pbsBound then
      begin
        Exit(LResolved);
      end;

      if LDecl.Kind <> psdkType then
      begin
        Exit(LResolved);
      end;
      LType := TSemanticType.Create;
      LType.Owner := FWorld;
      LType.TypeKind := pstAlias;
      LType.Declared := ASymbol;
      LType.Element := LResolved.BoundType;

      if LSyntax.Node(LSyntax.Root).Kind = ptxDistinct then
      begin
        LType.TypeKind := pstDistinct;
        LType.Element := LResolved.BoundType.Target;
      end;
      LType.TypeSyntax := LSyntax;
      LType.TypeNode := LSyntax.Root;
      Result := TypeAnswer(LType);
    finally
      SetLength(FAliases, LCount);
    end;
  finally
    Leave;
  end;
end;

function TBindingQuery.TypeDeclaration(const ASite: INyxPascalBindingSite;
  const AType: INyxPascalTypeSyntax; ANode: Integer): INyxPascalResolution;
var
  LResolved: INyxPascalResolution;
  LBase: Integer;
  LMember: Integer;
  LName: TNyxText;

  { A unit-qualified prototype remains a lexical name. A constructed receiver
    must first bind its own arguments before looking up the nested declaration. }
  function FlatName(AIndex: Integer): TNyxText;
  var
    LLeft: TNyxText;
    LRight: Integer;
  begin
    Enter;
    try
      Result := '';

      if AType.Node(AIndex).Kind = ptxName then
      begin
        Exit(AType.Node(AIndex).Name);
      end;

      if AType.Node(AIndex).Kind = ptxQualified then
      begin
        LLeft := FlatName(AType.Child(AIndex, ptrBase));
        LRight := AType.Child(AIndex, ptrMember);

        if (LLeft <> '') and (AType.Node(LRight).Kind = ptxName) then
        begin
          Result := LLeft + '.' + AType.Node(LRight).Name;
        end;
      end;
    finally
      Leave;
    end;
  end;

begin
  Enter;
  try
    LName := FlatName(ANode);

    if LName <> '' then
    begin
      Exit(Lookup(ASite, LName));
    end;

    if AType.Node(ANode).Kind <> ptxQualified then
    begin
      Exit(Answer(pbsUnresolved, pbiGenericContext));
    end;
    LBase := AType.Child(ANode, ptrBase);
    LMember := AType.Child(ANode, ptrMember);

    if AType.Node(LMember).Kind <> ptxName then
    begin
      Exit(Answer(pbsUnresolved, pbiGenericContext));
    end;
    LResolved := BindNode(ASite, AType, LBase);

    if LResolved.Status <> pbsBound then
    begin
      Exit(LResolved);
    end;
    Result := Member(LResolved.BoundType, ASite, AType.Node(LMember).Name, pmaType);
  finally
    Leave;
  end;
end;

function TBindingQuery.Specialize(const ASite: INyxPascalBindingSite;
  const AType: INyxPascalTypeSyntax; ANode: Integer): INyxPascalResolution;
var
  LResolved: INyxPascalResolution;
begin
  LResolved := TypeDeclaration(ASite, AType, AType.Child(ANode, ptrBase));

  if LResolved.Status <> pbsBound then Exit(LResolved);

  if (LResolved.Count <> 1) or (LResolved.SymbolAt(0).Declaration.Kind <> psdkType) then
    Exit(Answer(pbsUnresolved, pbiGenericContext));
  LResolved := ApplyGenericArguments(ASite, AType, ANode, LResolved.SymbolAt(0));

  if LResolved.Status <> pbsBound then Exit(LResolved);
  Result := TypeOfSymbol(LResolved.SymbolAt(0));
end;

function TBindingQuery.ApplyGenericArguments(const ASite: INyxPascalBindingSite;
  const AType: INyxPascalTypeSyntax; ANode: Integer;
  const APrototype: INyxPascalBoundSymbol): INyxPascalResolution;
var
  LResolved: INyxPascalResolution;
  LPrototype: INyxPascalBoundSymbol;
  LFrame: TGenericEnvironment;
  LLease: INyxPascalGenericEnvironment;
  LNames: TGenericNames;
  LIssue: TNyxPascalBindingIssue;
  LChild: Integer;
  LAt: Integer;
  LSymbol: TBoundSymbol;
  LSymbolLease: INyxPascalBoundSymbol;
  LOuter: INyxPascalGenericEnvironment;
  LWasTypeTarget: Boolean;
begin
  LPrototype := APrototype;
  LOuter := EnvironmentOf(LPrototype);

  if (LOuter <> nil) and SameSymbol(LOuter.Owner, LPrototype) then
  begin
    LOuter := LOuter.Parent;
  end;
  { The supported compiler oracle rejects a generic declaration inside another
    generic. Ordinary nested types may capture the outer frame; creating another
    parameter frame here remains explicitly unsupported rather than simulated. }

  if (LOuter <> nil) and
    (LPrototype.Declaration.GenericSpan.Length > 0) then
  begin
    Exit(Answer(pbsUnresolved, pbiGenericContext));
  end;
  LIssue := GenericNames(LPrototype, LNames);

  if LIssue <> pbiNone then
  begin
    Exit(Answer(pbsUnresolved, LIssue));
  end;

  if Length(LNames) = 0 then
  begin
    Exit(Answer(pbsUnresolved, pbiGenericArity));
  end;
  LFrame := TGenericEnvironment.Create;
  LLease := LFrame;
  LFrame.DeclarationOwner := FWorld.Symbol(LPrototype.ModuleIndex, LPrototype.DeclarationIndex);
  LFrame.Outer := LOuter;
  LFrame.ApplicationSite := ASite;
  LFrame.Names := LNames;
  SetLength(LFrame.Types, Length(LNames));
  LChild := AType.Child(ANode, ptrArgument);
  LAt := 0;
  while (LChild >= 0) and (AType.Node(LChild).Role = ptrArgument) do
  begin
    Tick;

    if LAt >= Length(LNames) then
    begin
      Exit(Answer(pbsUnresolved, pbiGenericArity));
    end;
    { Explicit generic arguments name types, including real formal parameters.
      Implicit Self/Result storage must not masquerade as a type argument. }
    LWasTypeTarget := FBindingTypeTarget;
    FBindingTypeTarget := True;
    try
      LResolved := BindNode(ASite, AType, LChild);
    finally
      FBindingTypeTarget := LWasTypeTarget;
    end;

    if LResolved.Status <> pbsBound then
    begin
      Exit(LResolved);
    end;
    LFrame.Types[LAt] := LResolved.BoundType;
    Inc(LAt);
    LChild := AType.Node(LChild).NextSibling;
  end;

  if LAt <> Length(LNames) then
  begin
    Exit(Answer(pbsUnresolved, pbiGenericArity));
  end;
  LSymbol := TBoundSymbol.Create;
  LSymbolLease := LSymbol;
  LSymbol.Owner := FWorld;
  LSymbol.ModuleOrdinal := LPrototype.ModuleIndex;
  LSymbol.DeclarationOrdinal := LPrototype.DeclarationIndex;
  LSymbol.Bindings := LLease;
  Result := OneSymbol(LSymbolLease);
end;

function TBindingQuery.ApplyRoutineArguments(const ASite: INyxPascalBindingSite;
  const AFound: INyxPascalResolution; const AType: INyxPascalTypeSyntax;
  ANode: Integer): INyxPascalResolution;
var
  LContext: INyxPascalCompilerContext;
  LArguments, LChild, LAt, LCount, LScope, LOwner: Integer;
  LParameters: TGenericParameters;
  LPrototype: INyxPascalBoundSymbol;
  LApplied: INyxPascalResolution;
  LSymbols: array of INyxPascalBoundSymbol;
  LIssue: TNyxPascalBindingIssue;
  LSyntax: INyxPascalSyntaxSnapshot;
  LGeneric: Boolean;
  LExisting: INyxPascalGenericEnvironment;
begin

  if not (AFound.Status in [pbsBound, pbsOverloads]) then Exit(AFound);
  LContext := ContextAt(ASite);

  if (LContext = nil) or (LContext.Family <> ppcFPC) or
    (LContext.Version.FullNumber <> 30301) or
    not (LContext.Mode in [ppmDelphi, ppmDelphiUnicode]) then
    Exit(Answer(pbsUnresolved, pbiCompilerContext));
  LArguments := 0;
  LChild := AType.Child(ANode, ptrArgument);
  while LChild >= 0 do
  begin
    Tick;

    if AType.Node(LChild).Role <> ptrArgument then Break;
    Inc(LArguments);
    LChild := AType.Node(LChild).NextSibling;
  end;
  LGeneric := False;
  SetLength(LSymbols, 0);
  for LAt := 0 to AFound.Count - 1 do
  begin
    Tick;
    LPrototype := AFound.SymbolAt(LAt);

    if LPrototype.Declaration.Kind <> psdkRoutine then
      Exit(Answer(pbsUnresolved, pbiGenericContext));

    if LPrototype.Declaration.Flags * [psdfIncomplete, psdfConditional, psdfUnsupported] <> [] then
      Exit(Answer(pbsUnresolved, pbiIncompleteSource));

    if LPrototype.Declaration.GenericSpan.Length = 0 then Continue;
    LGeneric := True;
    LExisting := EnvironmentOf(LPrototype);

    if (LExisting <> nil) and SameSymbol(LExisting.Owner, LPrototype) then
      Exit(Answer(pbsUnresolved, pbiGenericContext));
    { The compiler rejects a generic declaration inside another generic. An
      unbound/formal outer receiver cannot turn that source into an eligible call. }
    LSyntax := FWorld.ModuleAt(LPrototype.ModuleIndex).Syntax;
    LScope := LPrototype.Declaration.Scope;
    while LScope >= 0 do
    begin
      Tick;
      LOwner := LSyntax.Scope(LScope).OwnerDeclaration;

      if (LOwner >= 0) and (LSyntax.Declaration(LOwner).GenericSpan.Length > 0) then
        Exit(Answer(pbsUnresolved, pbiGenericContext));
      LScope := LSyntax.Scope(LScope).Parent;
    end;
    LIssue := GenericParameters(LPrototype, LParameters);

    if LIssue <> pbiNone then Exit(Answer(pbsUnresolved, LIssue));
    { Explicit type-argument arity is source syntax. Runtime parameter count,
      defaults, conversions and value-based overload selection remain opaque. }

    if Length(LParameters) <> LArguments then Continue;
    LApplied := ApplyGenericArguments(ASite, AType, ANode, LPrototype);

    if LApplied.Status <> pbsBound then Exit(LApplied);
    LCount := Length(LSymbols);

    if LCount >= FLimits.MaximumCandidates then
      raise ENyxPascalBindingLimit.Create('Generic routine alternatives exceed their query budget');
    SetLength(LSymbols, LCount + 1);
    LSymbols[LCount] := LApplied.SymbolAt(0);
  end;

  if Length(LSymbols) = 0 then
  begin

    if LGeneric then Exit(Answer(pbsUnresolved, pbiGenericArity));
    Exit(Answer(pbsUnresolved, pbiGenericContext));
  end;
  Result := SymbolAnswer(LSymbols);
end;

function TBindingQuery.InferRoutineArguments(const ASite: INyxPascalBindingSite;
  const AFound: INyxPascalResolution;
  const AArguments: TInferredArgumentAnswers): INyxPascalResolution;
var
  LContext: INyxPascalCompilerContext;
  LPrototype: INyxPascalBoundSymbol;
  LSignature: INyxPascalResolution;
  LSource: INyxPascalDeclaredSignature;
  LFrame: TGenericEnvironment;
  LLease: INyxPascalGenericEnvironment;
  LNames: TGenericNames;
  LSymbols: array of INyxPascalBoundSymbol;
  LAt, LArgument, LScope, LOwner, LCount, LSuffix: Integer;
  LSyntax: INyxPascalSyntaxSnapshot;
  LIssue: TNyxPascalBindingIssue;
  LToken: TNyxPascalToken;
  LArgumentType: INyxPascalSemanticType;
  LLiteral: INyxPascalLiteralType;
  LNumeric: INumericExpressionValue;

  { Inspect only formal positions owned by this exact routine. A class member's
    fields are not traversed and other declarations' formals are not captured. }
  function NeedsArgument(const AType: INyxPascalSemanticType): Boolean;
  var
    LFormal: INyxPascalFormalParameter;
    LEnvironment: INyxPascalGenericEnvironment;
    LIndex: Integer;
  begin
    Enter;
    try
      Result := False;

      if AType = nil then Exit;

      if Supports(AType, INyxPascalFormalParameter, LFormal) then
        Exit(SameSymbol(LFormal.DeclarationOwner, LPrototype));

      if AType.Definition <> nil then
      begin
        LEnvironment := EnvironmentOf(AType.Definition);
        while LEnvironment <> nil do
        begin
          Tick;
          for LIndex := 0 to LEnvironment.Count - 1 do
          begin
            Tick;

            if NeedsArgument(LEnvironment.Argument(LIndex)) then Exit(True);
          end;
          LEnvironment := LEnvironment.Parent;
        end;
      end;

      if NeedsArgument(AType.Target) then Exit(True);
      for LIndex := 0 to AType.ParameterCount - 1 do
      begin
        Tick;

        if NeedsArgument(AType.ParameterType(LIndex)) then Exit(True);
      end;
    finally
      Leave;
    end;
  end;

  { Exact formal correspondence and named generic frame identity are qualified
    here, independently of conversions or compiler overload eligibility. Every
    occurrence must agree; no first-wins guess repairs conflicting arguments. }
  function MatchArgument(const AParameter, AActual: INyxPascalSemanticType): Boolean;
  var
    LFormal: INyxPascalFormalParameter;
    LExpected, LActual: INyxPascalGenericEnvironment;
    LIndex: Integer;
    LHasEnvironment: Boolean;
  begin
    Enter;
    try
      Result := False;

      if (AParameter = nil) or (AActual = nil) or
        (AParameter.World <> FWorld) or (AActual.World <> FWorld) then Exit;

      if Supports(AParameter, INyxPascalFormalParameter, LFormal) and
        SameSymbol(LFormal.DeclarationOwner, LPrototype) then
      begin
        LIndex := LFormal.ParameterIndex;

        if (LIndex < 0) or (LIndex >= Length(LFrame.Types)) or
          (NyxPascalASCIIKey(LFormal.Name) <> NyxPascalASCIIKey(LFrame.Names[LIndex])) then Exit;

        if LFrame.Types[LIndex] = nil then
        begin
          LFrame.Types[LIndex] := AActual;
          Exit(True);
        end;
        Exit(SameType(LFrame.Types[LIndex], AActual));
      end;

      if not NeedsArgument(AParameter) then Exit(SameType(AParameter, AActual));

      if (AParameter.Definition <> nil) and (AActual.Definition <> nil) and
        SameSymbol(AParameter.Definition, AActual.Definition) then
      begin
        LExpected := EnvironmentOf(AParameter.Definition);
        LActual := EnvironmentOf(AActual.Definition);
        LHasEnvironment := LExpected <> nil;
        while (LExpected <> nil) and (LActual <> nil) do
        begin
          Tick;

          if not SameSymbol(LExpected.Owner, LActual.Owner) or
            (LExpected.Count <> LActual.Count) then Exit;
          for LIndex := 0 to LExpected.Count - 1 do
          begin
            Tick;

            if (NyxPascalASCIIKey(LExpected.Name(LIndex)) <> NyxPascalASCIIKey(LActual.Name(LIndex))) or
              not MatchArgument(LExpected.Argument(LIndex), LActual.Argument(LIndex)) then Exit;
          end;
          LExpected := LExpected.Parent;
          LActual := LActual.Parent;
        end;

        if LHasEnvironment then Exit((LExpected = nil) and (LActual = nil));
      end;
      { Transparent nongeneric aliases preserve an earlier named frame. Distinct
        declarations, anonymous shapes and differing prototypes grant no new
        structural/conversion equivalence through this result-contract query. }
      if (AParameter.Kind = pstAlias) and
        ((AParameter.Definition = nil) or (AParameter.Definition.Declaration.GenericSpan.Length = 0)) then
        Exit(MatchArgument(AParameter.Target, AActual));

      if (AActual.Kind = pstAlias) and
        ((AActual.Definition = nil) or (AActual.Definition.Declaration.GenericSpan.Length = 0)) then
        Exit(MatchArgument(AParameter, AActual.Target));
    finally
      Leave;
    end;
  end;
begin
  SetLength(LSymbols, 0);

  if not (AFound.Status in [pbsBound, pbsOverloads]) then Exit(AFound);
  LContext := ContextAt(ASite);

  if (LContext = nil) or (LContext.Family <> ppcFPC) or
    (LContext.Version.FullNumber <> 30301) or
    not (LContext.Mode in [ppmDelphi, ppmDelphiUnicode]) or
    (LContext.Feature(ppfImplicitFunctionSpecialization) <> ppkTrue) then
    Exit(Answer(pbsUnresolved, pbiCompilerContext));
  for LAt := 0 to AFound.Count - 1 do
  begin
    Tick;
    LPrototype := AFound.SymbolAt(LAt);

    if LPrototype.Declaration.Kind <> psdkRoutine then
      Exit(Answer(pbsUnresolved, pbiGenericContext));
    LCount := Length(LSymbols);

    if LCount >= FLimits.MaximumCandidates then
      raise ENyxPascalBindingLimit.Create('Inferred routine alternatives exceed their query budget');

    if LPrototype.Declaration.GenericSpan.Length = 0 then
    begin
      SetLength(LSymbols, LCount + 1);
      LSymbols[LCount] := LPrototype;
      Continue;
    end;

    if EnvironmentOf(LPrototype) <> nil then
      Exit(Answer(pbsUnresolved, pbiGenericContext));
    LSyntax := FWorld.ModuleAt(LPrototype.ModuleIndex).Syntax;

    if LPrototype.Declaration.RoutineKind <> prkFunction then
      Exit(Answer(pbsUnresolved, pbiGenericContext));

    if LPrototype.Declaration.TypeSpan.Length = 0 then
      Exit(Answer(pbsUnresolved, pbiMissingType));
    { VARARGS changes positional admission. Its existing authored suffix is
      inspected after the result type, so a contextual parameter/type name
      cannot be mistaken for that modifier. No effective ABI is inferred. }
    LSuffix := LSyntax.Lexical.TokenAt(LPrototype.Declaration.TypeSpan.Finish);
    while LSuffix < LSyntax.Lexical.Count do
    begin
      Tick;
      LToken := LSyntax.Lexical.Token(LSuffix);

      if LToken.Span.Start >= LPrototype.Declaration.HeaderSpan.Finish then Break;

      if (LToken.Kind = ptkWord) and not LToken.Escaped and (LToken.Word = pkwVarargs) then
        Exit(Answer(pbsUnresolved, pbiGenericContext));
      Inc(LSuffix);
    end;
    LScope := LPrototype.Declaration.Scope;
    while LScope >= 0 do
    begin
      Tick;
      LOwner := LSyntax.Scope(LScope).OwnerDeclaration;

      if (LOwner >= 0) and (LSyntax.Declaration(LOwner).GenericSpan.Length > 0) then
        Exit(Answer(pbsUnresolved, pbiGenericContext));
      LScope := LSyntax.Scope(LScope).Parent;
    end;
    LContext := ContextAt(FWorld.At(LPrototype.ModuleIndex, LPrototype.Declaration.Scope,
      LPrototype.Declaration.HeaderSpan.Start));

    if (LContext = nil) or (LContext.Family <> ppcFPC) or
      (LContext.Version.FullNumber <> 30301) or
      (LContext.Feature(ppfImplicitFunctionSpecialization) <> ppkTrue) then
      Exit(Answer(pbsUnresolved, pbiCompilerContext));
    LIssue := GenericNames(LPrototype, LNames);

    if LIssue <> pbiNone then Exit(Answer(pbsUnresolved, LIssue));

    if Length(LNames) = 0 then Exit(Answer(pbsUnresolved, pbiGenericArity));
    LSignature := RoutineSignature(LPrototype);

    if LSignature.Status <> pbsBound then Exit(LSignature);

    if not Supports(LSignature.BoundType, INyxPascalDeclaredSignature, LSource) then
      Exit(Answer(pbsUnresolved, pbiGenericContext));

    if (Length(AArguments) < LSource.RequiredParameterCount) or
      (Length(AArguments) > LSignature.BoundType.ParameterCount) then Continue;
    LFrame := TGenericEnvironment.Create;
    LLease := LFrame;
    LFrame.DeclarationOwner := FWorld.Symbol(LPrototype.ModuleIndex, LPrototype.DeclarationIndex);
    LFrame.ApplicationSite := ASite;
    LFrame.Names := LNames;
    SetLength(LFrame.Types, Length(LNames));
    for LArgument := 0 to High(AArguments) do
    begin
      Tick;

      if not NeedsArgument(LSignature.BoundType.ParameterType(LArgument)) then Continue;

      if AArguments[LArgument] = nil then Exit(Answer(pbsUnresolved, pbiGenericContext));

      if AArguments[LArgument].Status <> pbsBound then Exit(AArguments[LArgument]);
      LArgumentType := AArguments[LArgument].BoundType;

      if Supports(LArgumentType, INyxPascalLiteralType, LLiteral) then
        LArgumentType := LLiteral.SpecializationType;

      if Supports(LArgumentType, INumericExpressionValue, LNumeric) then
        LArgumentType := LNumeric.ResultType;

      if not MatchArgument(LSignature.BoundType.ParameterType(LArgument), LArgumentType) then
        Exit(Answer(pbsUnresolved, pbiGenericContext));
    end;
    for LArgument := 0 to High(LFrame.Types) do
    begin
      Tick;

      if LFrame.Types[LArgument] = nil then Exit(Answer(pbsUnresolved, pbiGenericContext));
    end;
    SetLength(LSymbols, LCount + 1);
    LSymbols[LCount] := InEnvironment(LPrototype, LLease);
  end;

  if Length(LSymbols) = 0 then Exit(Answer(pbsUnresolved, pbiGenericArity));
  Result := SymbolAnswer(LSymbols);
end;

function TBindingQuery.BindNode(const ASite: INyxPascalBindingSite;
  const AType: INyxPascalTypeSyntax; ANode: Integer): INyxPascalResolution;
var
  LNode: TNyxPascalTypeNode;
  LType: TSemanticType;
  LLease: INyxPascalSemanticType;
  LResolved: INyxPascalResolution;
  LChild: Integer;
  LGroup: Integer;
  LName: Integer;
  LAt: Integer;
  LMode: TNyxPascalSyntaxParameterMode;
  LContext: INyxPascalCompilerContext;
  LOrigin: Integer;
  LCurrent: Integer;
  LDeclaredSyntax: INyxPascalDeclaredTypeSyntax;

begin
  Enter;
  try
    LNode := AType.Node(ANode);

    if LNode.Kind = ptxStructured then
    begin

      if not Supports(AType, INyxPascalDeclaredTypeSyntax, LDeclaredSyntax) or
        (LDeclaredSyntax.Declarations <> FWorld.ModuleAt(ASite.ModuleIndex).Syntax) then
      begin
        raise ENyxPascalBinding.Create('This structured type belongs to another declared source');
      end;
      LAt := LDeclaredSyntax.DeclarationOf(ANode);

      if LAt < 0 then
      begin
        Exit(Answer(pbsUnresolved, pbiUnsupportedType));
      end;
      Exit(TypeOfSymbol(InEnvironment(FWorld.Symbol(ASite.ModuleIndex, LAt), FEnvironment)));
    end;

    if LNode.Kind in [ptxName, ptxQualified] then
    begin
      LResolved := TypeDeclaration(ASite, AType, ANode);

      if LResolved.Status <> pbsBound then
      begin
        Exit(LResolved);
      end;

      if LResolved.BoundType <> nil then
      begin
        Exit(LResolved);
      end;

      if (LResolved.Count <> 1) or (LResolved.SymbolAt(0).Declaration.Kind <> psdkType) then
      begin
        Exit(Answer(pbsUnresolved, pbiUnsupportedType));
      end;
      Exit(TypeOfSymbol(LResolved.SymbolAt(0)));
    end;

    if LNode.Kind = ptxGeneric then
    begin
      Exit(Specialize(ASite, AType, ANode));
    end;

    if LNode.Kind = ptxPointer then
    begin
      LType := TDataPointerType.Create;
      TDataPointerType(LType).CapturedContext := ContextAt(
        FWorld.At(ASite.ModuleIndex,ASite.ScopeIndex,LNode.Span.Start));
      TDataPointerType(LType).InitialTargetContext := InitialPointerContext(ASite.ModuleIndex);
    end
    else LType := TSemanticType.Create;
    LLease := LType;
    LType.Owner := FWorld;
    LType.TypeSyntax := AType;
    LType.TypeNode := ANode;
    LChild := -1;
    case LNode.Kind of
      ptxPointer:
        begin
          LType.TypeKind := pstPointer;
          LChild := AType.Child(ANode, ptrPointee);
        end;
      ptxArray:
        begin
          LType.TypeKind := pstArray;
          LChild := AType.Child(ANode, ptrElement);
        end;
      ptxSet:
        begin
          LType.TypeKind := pstSet;
          LChild := AType.Child(ANode, ptrElement);
        end;
      ptxFile:
        begin
          LType.TypeKind := pstFile;
          LChild := AType.Child(ANode, ptrElement);
        end;
      ptxClassOf:
        begin
          LType.TypeKind := pstClassOf;
          LChild := AType.Child(ANode, ptrTarget);
        end;
      ptxDistinct:
        begin
          LType.TypeKind := pstDistinct;
          LChild := AType.Child(ANode, ptrTarget);
        end;
      ptxReference:
        begin
          LType.TypeKind := pstReference;
          LChild := AType.Child(ANode, ptrTarget);
        end;
      ptxProcedure, ptxFunction:
        begin
          LType.TypeKind := pstProcedure;

          if LNode.Kind = ptxFunction then
          begin
            LType.TypeKind := pstFunction;
            LChild := AType.Child(ANode, ptrResult);
          end;
        end;
      ptxUntyped:
        begin
          LType.TypeKind := pstUntyped;
        end;
      ptxEnumeration:
        begin
          LType.TypeKind := pstEnumeration;
        end;
      ptxSubrange:
        begin
          LType.TypeKind := pstSubrange;
        end;
      ptxEncodedString:
        begin
          { This is an authored structural request, analogous to an unevaluated
            array bound. Keep the exact code-page operand on Syntax and the
            separately resolved ANSI base on Target. In particular, neither
            CP_UTF8 spelling nor an arbitrary numeric operand proves an effective
            encoding, conversion rule, constant value or storage layout. }
          LChild := AType.Child(ANode, ptrBase);

          if (LChild < 0) or (AType.Child(ANode, ptrCodePage) < 0) then
            Exit(Answer(pbsUnresolved, pbiUnsupportedType));

          if AType.Node(LChild).Kind = ptxString then
          begin
            { The explicit String(codepage) grammar selects an ANSI base even
              under H-. Compiler-unit mode still requires the real System owner
              and its captured primitive presence, rather than a caller alias. }
            if (FLimits.ImplicitScopes = pispCompilerUnits) and
              (FWorld.FindModule('System') < 0) then
              Exit(Answer(pbsUnresolved, pbiMissingModule, 'System'));
            LResolved := BuiltinType(ASite, 'AnsiString',
              FLimits.ImplicitScopes = pispCompilerUnits);
          end
          else
            LResolved := BindNode(ASite, AType, LChild);

          if LResolved.Status <> pbsBound then
            Exit(LResolved);

          if (LResolved.BoundType = nil) or
            (LResolved.BoundType.Kind <> pstBuiltin) or
            (LResolved.BoundType.Builtin <> pbbAnsiString) then
            Exit(Answer(pbsUnresolved, pbiUnsupportedType));
          LType.TypeKind := pstEncodedString;
          LType.Element := LResolved.BoundType;
          LChild := -1;
        end;
      ptxString:
        begin
          LType.TypeKind := pstString;
          LType.Intrinsic := pbbShortString;

          if AType.Child(ANode, ptrLength) < 0 then
          begin
            LCurrent := ASite.ModuleIndex;
            LOrigin := FWorld.ModuleAt(LCurrent).OriginAt(LNode.Span.Start);

            if LOrigin < 0 then
            begin
              Exit(Answer(pbsUnresolved, pbiCompilerContext));
            end;
            LContext := FWorld.ModuleAt(LCurrent).Source.State(
              FWorld.ModuleAt(LCurrent).Origin(LOrigin).State);

            if (LContext.Option('H') = ppkUnknown) or
              ((LContext.Option('H') = ppkTrue) and
                (LContext.Feature(ppfUnicodeStrings) = ppkUnknown)) then
            begin
              Exit(Answer(pbsUnresolved, pbiCompilerContext));
            end;

            if LContext.Option('H') = ppkTrue then
            begin
              LType.Intrinsic := pbbAnsiString;

              if LContext.Feature(ppfUnicodeStrings) = ppkTrue then
              begin
                LType.Intrinsic := pbbUnicodeString;
              end;
            end;
          end;
        end;
    else
      begin
        Exit(Answer(pbsUnresolved, pbiUnsupportedType));
      end;
    end;

    if LChild >= 0 then
    begin
      LResolved := BindNode(ASite, AType, LChild);

      if LResolved.Status <> pbsBound then
      begin
        Exit(LResolved);
      end;
      LType.Element := LResolved.BoundType;
    end;
    LGroup := AType.Child(ANode, ptrParameter);
    while LGroup >= 0 do
    begin
      Tick;

      if AType.Node(LGroup).Role <> ptrParameter then
      begin
        Break;
      end;
      LResolved := BindNode(ASite, AType, AType.Child(LGroup, ptrParameterType));

      if LResolved.Status <> pbsBound then
      begin
        Exit(LResolved);
      end;
      LMode := AType.Node(LGroup).ParameterMode;
      LName := AType.Child(LGroup, ptrParameterName);
      while LName >= 0 do
      begin
        Tick;

        if AType.Node(LName).Role <> ptrParameterName then
        begin
          Break;
        end;
        LAt := Length(LType.Names);

        if LAt >= FLimits.MaximumCandidates then
        begin
          raise ENyxPascalBindingLimit.Create('Semantic signature parameters exceed their query budget');
        end;
        SetLength(LType.Names, LAt + 1);
        SetLength(LType.Modes, LAt + 1);
        SetLength(LType.Types, LAt + 1);
        LType.Names[LAt] := AType.Node(LName).Name;
        LType.Modes[LAt] := LMode;
        LType.Types[LAt] := LResolved.BoundType;
        LName := AType.Node(LName).NextSibling;
      end;
      LGroup := AType.Node(LGroup).NextSibling;
    end;
    Result := TypeAnswer(LLease);
  finally
    Leave;
  end;
end;

function TBindingQuery.Bases(const AOwner: INyxPascalBoundSymbol;
  AOrdinal: Integer): INyxPascalResolution;
var
  LEnvironment: INyxPascalGenericEnvironment;
  LOwner: INyxPascalBoundSymbol;
begin
  LEnvironment := FEnvironment;
  LOwner := FTypeOwner;
  FEnvironment := EnvironmentOf(AOwner);
  FTypeOwner := AOwner;
  try
    Result := BasesCore(AOwner, AOrdinal);
  finally
    FEnvironment := LEnvironment;
    FTypeOwner := LOwner;
  end;
end;

function TBindingQuery.InterfaceStyleOf(const AOwner: INyxPascalBoundSymbol): TNyxPascalInterfaceStyle;
var
  LContext: INyxPascalCompilerContext;
begin
  Tick;
  LContext := ContextAt(FWorld.At(AOwner.ModuleIndex, AOwner.Declaration.Scope,
    AOwner.Declaration.TypeSpan.Start));
  Result := pifsUnknown;

  if (LContext <> nil) and (LContext.Family = ppcFPC) and
    (LContext.Version.FullNumber = 30301) and
    (LContext.Mode in [ppmFPC, ppmObjFPC, ppmDelphi, ppmDelphiUnicode]) then
  begin
    Result := NyxPascalCompilerPolicies(LContext).InterfaceStyle;
  end;
end;

function TBindingQuery.BasesCore(const AOwner: INyxPascalBoundSymbol;
  AOrdinal: Integer): INyxPascalResolution;
var
  LDecl: TNyxPascalSyntaxDeclaration;
  LView: INyxPascalTranslatedSyntax;
  LLex: INyxPascalLexicalSnapshot;
  LAt: Integer;
  LStart: Integer;
  LAngle: Integer;
  LNested: Integer;
  LCount: Integer;
  LToken: TNyxPascalToken;
  LWindow: TNyxPascalSpan;
  LType: INyxPascalTypeSyntax;
  LImplicit: TNyxText;
  LModule: Integer;
  LFound: INyxPascalResolution;
  LContext: INyxPascalCompilerContext;
  LStyle: TNyxPascalInterfaceStyle;
begin
  Tick;
  LDecl := AOwner.Declaration;
  LView := FWorld.ModuleAt(AOwner.ModuleIndex);
  LLex := LView.Syntax.Lexical;

  if LDecl.AncestorSpan.Length = 0 then
  begin

    if AOrdinal <> 0 then
    begin
      Exit(Answer(pbsMissing, pbiMissingName));
    end;
    LImplicit := '';

    if LDecl.TypeForm = pstfClass then
    begin
      LImplicit := 'TObject';
    end
    else if LDecl.TypeForm = pstfInterface then
    begin
      { FPC registers the root IUnknown while compiling System and gives that
        exact root no parent. CORBA/Java interfaces also get no implicit parent.
        This qualified compiler-owned role is independent of a missing default. }

      if (LDecl.Scope = 0) and (LDecl.GenericSpan.Length = 0) and
        (NyxPascalASCIIKey(LView.Syntax.Scope(0).Name) = 'system') then
      begin

        if NyxPascalASCIIKey(LDecl.Name) = 'iinterface' then Exit(Answer(pbsMissing, pbiMissingName));
        LContext := ContextAt(FWorld.At(AOwner.ModuleIndex, LDecl.Scope, LDecl.TypeSpan.Start));

        if (LContext <> nil) and (LContext.Family = ppcFPC) and
          (LContext.Version.FullNumber = 30301) and
          (NyxPascalASCIIKey(LDecl.Name) = 'iunknown') then Exit(Answer(pbsMissing, pbiMissingName));
      end;
      LStyle := InterfaceStyleOf(AOwner);
      case LStyle of
        pifsCORBA: Exit(Answer(pbsMissing, pbiMissingName));
        pifsCOM: LImplicit := 'IUnknown';
      else
        begin
          LContext := ContextAt(FWorld.At(AOwner.ModuleIndex, LDecl.Scope, LDecl.TypeSpan.Start));

          if not FConsideringInterfaceRoot or (LContext = nil) or
            (LContext.Family <> ppcFPC) or (LContext.Version.FullNumber <> 30301) or
            not (LContext.Mode in [ppmFPC, ppmObjFPC, ppmDelphi, ppmDelphiUnicode]) then
          begin
            Exit(Answer(pbsUnresolved, pbiCompilerContext));
          end;
          LImplicit := 'IUnknown';
        end;
      end;
    end;

    if (LImplicit = '') or
      ((NyxPascalASCIIKey(LView.Syntax.Scope(0).Name) = 'system') and
        (NyxPascalASCIIKey(LDecl.Name) = NyxPascalASCIIKey(LImplicit))) then
    begin
      Exit(Answer(pbsMissing, pbiMissingName));
    end;
    LModule := FWorld.FindModule('System');

    if LModule < 0 then
    begin
      Exit(Answer(pbsUnresolved, pbiMissingModule, 'System'));
    end;
    LFound := Exact(LModule, 0, InterfaceEnd(LModule), LImplicit, True);

    if (LFound.Status = pbsMissing) and (LImplicit = 'IUnknown') and
      (FLimits.ImplicitScopes = pispDeclarationNames) then
    begin
      { The explicit declaration-only scope policy permits a symbolic IInterface
        root in small caller-owned models. Compiler-unit policy requires the
        actual FPC IUnknown declaration and never uses this symbolic substitute. }
      LFound := Exact(LModule, 0, InterfaceEnd(LModule), 'IInterface', True);
    end;

    if (LFound.Status <> pbsBound) or (LFound.Count <> 1) then
    begin
      Exit(LFound);
    end;
    Exit(TypeOfSymbol(LFound.SymbolAt(0)));
  end;
  LAt := LLex.TokenAt(LDecl.AncestorSpan.Start) + 1;
  LStart := LAt;
  LAngle := 0;
  LNested := 0;
  LCount := 0;
  while LAt < LLex.Count do
  begin
    Tick;
    LToken := LLex.Token(LAt);

    if LToken.Span.Start >= LDecl.AncestorSpan.Finish then
    begin
      Break;
    end;

    if (LToken.Kind = ptkSymbol) and (LAngle = 0) and (LNested = 0) and
      (LToken.Symbol in [psoComma, psoCloseParen]) then
    begin

      if LCount = AOrdinal then
      begin
        { Translation can insert separating whitespace with no authored context.
          Bind at the first real type token, preserving its exact source state;
          a synthetic separator cannot supply primitive/compiler identity. }
        while LStart < LAt do
        begin
          Tick;

          if not (LLex.Token(LStart).Kind in [ptkWhitespace, ptkComment]) then Break;
          Inc(LStart);
        end;
        LWindow := NyxPascalSpan(LLex.Token(LStart).Span.Start,
          LToken.Span.Start - LLex.Token(LStart).Span.Start);
        LType := NewNyxPascalTypeSyntax(LLex, LWindow, TNyxPascalTypeOptions.Defaults, FCancel);

        if not LType.Complete then
        begin
          Exit(Answer(pbsUnresolved, pbiUnsupportedType));
        end;
        LFound := BindNode(FWorld.At(AOwner.ModuleIndex, LDecl.Scope, LWindow.Start), LType, LType.Root);

        if LFound.Status = pbsMissing then
        begin
          { A selected explicit ancestor exists syntactically. Failure to bind it
            is not the end-of-base-list sentinel and cannot prove absence. }
          Exit(Answer(pbsUnresolved, pbiMissingType, LLex.Slice(LWindow)));
        end;
        Exit(LFound);
      end;
      Inc(LCount);
      LStart := LAt + 1;
    end
    else if LToken.Kind = ptkSymbol then
    begin
      case LToken.Symbol of
        psoLess:
          begin
            Inc(LAngle);
          end;
        psoGreater:
          begin
            Dec(LAngle);
          end;
        psoShiftRight:
          begin
            Dec(LAngle, 2);
          end;
        psoOpenParen, psoOpenBracket:
          begin
            Inc(LNested);
          end;
        psoCloseParen, psoCloseBracket:
          begin
            Dec(LNested);
          end;
      else
        begin
          { Other symbols remain inside their exact ancestor type window. }
        end;
      end;
    end;
    Inc(LAt);
  end;
  Result := Answer(pbsMissing, pbiMissingName);
end;

function TBindingQuery.Descends(const AChild, AParent: INyxPascalBoundSymbol;
  out AUnresolved: Boolean): Boolean;
var
  LAt: Integer;
  LBase: INyxPascalResolution;
  LType: INyxPascalSemanticType;
  LUnknown: Boolean;
begin
  Enter;
  try
    AUnresolved := False;

    if SameSymbol(AChild, AParent) then
    begin
      Exit(True);
    end;
    LAt := 0;
    while True do
    begin
      LBase := Bases(AChild, LAt);

      if LBase.Status = pbsMissing then
      begin
        Break;
      end;

      if LBase.Status <> pbsBound then
      begin
        AUnresolved := True;
        Exit(False);
      end;
      LType := Unwrap(LBase.BoundType);

      if (LType = nil) or (LType.Definition = nil) then
      begin
        AUnresolved := True;
        Exit(False);
      end;

      if Descends(LType.Definition, AParent, LUnknown) then
      begin
        Exit(True);
      end;
      AUnresolved := AUnresolved or LUnknown;
      Inc(LAt);
    end;
    Result := False;
  finally
    Leave;
  end;
end;

function TBindingQuery.Access(const AMember, AOwner: INyxPascalBoundSymbol;
  const ASite: INyxPascalBindingSite): TNyxPascalKnowledge;
var
  LVisibility: TNyxPascalSyntaxVisibility;
  LCaller: INyxPascalBoundSymbol;
  LUnknown: Boolean;
  LMode: TNyxPascalLanguageMode;
  LView: INyxPascalTranslatedSyntax;
  LOrigin: TNyxPascalSyntaxOrigin;
begin
  LVisibility := AMember.Declaration.Visibility;

  if LVisibility in [psavPublic, psavPublished, psavAutomated] then
  begin
    Exit(ppkTrue);
  end;
  LView := FWorld.ModuleAt(AMember.ModuleIndex);
  LOrigin := AMember.Origin;
  LMode := LView.Source.State(LOrigin.State).Mode;

  if not (LMode in [ppmFPC, ppmObjFPC, ppmDelphi, ppmDelphiUnicode]) then
  begin
    Exit(ppkUnknown);
  end;

  if LVisibility = psavUnspecified then
  begin
    { Default public/published sections have the same access. RTTI/streaming
      defaults remain unclaimed and are not needed for this visibility decision. }
    Exit(ppkTrue);
  end;

  if (LVisibility in [psavPrivate, psavProtected]) and
    (AMember.ModuleIndex = ASite.ModuleIndex) then
  begin
    Exit(ppkTrue);
  end;
  LCaller := CurrentClass(ASite);

  if LCaller = nil then
  begin
    Exit(ppkFalse);
  end;

  if SameSymbol(LCaller, AOwner) then
  begin
    Exit(ppkTrue);
  end;

  if LVisibility = psavStrictPrivate then
  begin
    Exit(ppkFalse);
  end;

  if LVisibility in [psavProtected, psavStrictProtected] then
  begin

    if Descends(LCaller, AOwner, LUnknown) then
    begin
      Exit(ppkTrue);
    end;

    if LUnknown then
    begin
      Exit(ppkUnknown);
    end;
  end;
  Result := ppkFalse;
end;

function TBindingQuery.OpenScopeInherited(const AOwner: INyxPascalBoundSymbol;
  const ASite: INyxPascalBindingSite; const AName: TNyxText): INyxPascalResolution;
var
  LView: INyxPascalTranslatedSyntax;
  LSyntax: INyxPascalSyntaxSnapshot;
  LDeclaration: TNyxPascalSyntaxDeclaration;
  LScope: TNyxPascalSyntaxScope;
  LAt: Integer;
  LLast: TNyxPascalToken;

  { This is only a lexical absence proof for an unfinished helper. It grants
    neither helper eligibility nor activation: the ordinary helper type/member
    APIs still refuse. With MULTIHELPERS off, a parentless primitive target has
    no nested member table that could hide the next enclosing lexical name. }
  function PrimitiveHelperPrefix: INyxPascalResolution;
  var
    LContext: INyxPascalCompilerContext;
    LLexical: INyxPascalLexicalSnapshot;
    LTargetSyntax: INyxPascalTypeSyntax;
    LTarget: INyxPascalResolution;
    LTerminal: INyxPascalSemanticType;
    LToken: TNyxPascalToken;
    LHeaderEnd, LTokenAt, LTargetStart: Integer;
    LIntroducer: TNyxPascalWord;

    { Every inspected token belongs to the checked header, before the first
      body token. Trivia cannot advance this proof into a member declaration. }
    function HeaderToken: Boolean;
    begin
      Result := False;
      while LTokenAt < LLexical.Count do
      begin
        Tick;
        LToken := LLexical.Token(LTokenAt);

        if LToken.Span.Start >= LHeaderEnd then Exit;

        if not (LToken.Kind in [ptkWhitespace, ptkComment, ptkDirective]) then
          Exit(True);
        Inc(LTokenAt);
      end;
    end;

    function Consume(AWord: TNyxPascalWord): Boolean;
    begin
      Result := HeaderToken and (LToken.Kind = ptkWord) and not LToken.Escaped and
        (LToken.Word = AWord);

      if Result then Inc(LTokenAt);
    end;

  begin
    Result := Answer(pbsUnresolved, pbiIncompleteSource);
    LToken := Default(TNyxPascalToken);

    if LDeclaration.AncestorSpan.Length <> 0 then Exit;
    LContext := ContextAt(ASite);

    if (LContext = nil) or (LContext.Family <> ppcFPC) or
      (LContext.Version.FullNumber <> 30301) or
      (NyxPascalCompilerPolicies(LContext).MultipleHelpers <> ppkFalse) then Exit;
    LLexical := LSyntax.Lexical;
    LHeaderEnd := LScope.Span.Start;

    if (LDeclaration.TypeSpan.Length = 0) or
      (LDeclaration.TypeSpan.Start >= LHeaderEnd) then Exit;
    LTokenAt := LLexical.TokenAt(LDeclaration.TypeSpan.Start);
    LIntroducer := pkwType;

    if LDeclaration.TypeForm = pstfRecordHelper then LIntroducer := pkwRecord
    else if LDeclaration.TypeForm = pstfClassHelper then LIntroducer := pkwClass;

    if not Consume(LIntroducer) or not Consume(pkwHelper) or not Consume(pkwFor) or
      not HeaderToken then Exit;
    LTargetStart := LToken.Span.Start;
    LTargetSyntax := NewNyxPascalTypeSyntax(LLexical,
      NyxPascalSpan(LTargetStart, LHeaderEnd - LTargetStart),
      TNyxPascalTypeOptions.Defaults, FCancel);

    if not LTargetSyntax.Complete or
      not (LTargetSyntax.Node(LTargetSyntax.Root).Kind in [ptxName, ptxQualified]) then Exit;
    { The target was authored outside the helper's member scope. Resolving it
      there avoids recursion and cannot borrow a same-spelled helper member. }
    LTarget := BindNode(FWorld.At(AOwner.ModuleIndex, LDeclaration.Scope,
      LTargetStart), LTargetSyntax, LTargetSyntax.Root);

    if LTarget.Status <> pbsBound then
    begin

      if LTarget.Status <> pbsMissing then Result := LTarget;
      Exit;
    end;
    LTerminal := Unwrap(LTarget.BoundType);

    if (LTerminal <> nil) and (LTerminal.Kind = pstBuiltin) and
      (LTerminal.Builtin <> pbbNone) then
      Result := Answer(pbsMissing, pbiMissingName);
  end;
begin
  Tick;
  LView := FWorld.ModuleAt(AOwner.ModuleIndex);
  LSyntax := LView.Syntax;
  LDeclaration := AOwner.Declaration;

  if (AOwner.ModuleIndex <> ASite.ModuleIndex) or not LView.CompleteTranslation or
    (LDeclaration.Kind <> psdkType) or
    not (LDeclaration.TypeForm in [pstfClass, pstfInterface, pstfObject, pstfRecord,
      pstfRecordHelper, pstfClassHelper, pstfTypeHelper]) or
    (LDeclaration.NestedScope < 0) or
    (LDeclaration.Flags * [psdfConditional, psdfUnsupported, psdfForward] <> []) then
  begin
    Exit(Answer(pbsUnresolved, pbiIncompleteSource));
  end;
  LScope := LSyntax.Scope(LDeclaration.NestedScope);

  if LScope.Complete or (LScope.Kind <> psskMembers) or
    (LScope.Span.Finish <> LSyntax.Lexical.Scalars) or
    (ASite.Caret < LScope.Span.Start) or (ASite.Caret > LScope.Span.Finish) or
    not SafeScope(AOwner.ModuleIndex, LDeclaration.NestedScope, ASite.Caret) then
  begin
    Exit(Answer(pbsUnresolved, pbiIncompleteSource));
  end;
  { A missing final END/semicolon at EOF does not obscure the earlier header.
    Any actual earlier syntax error can obscure shadowing and must still refuse. }
  for LAt := 0 to LSyntax.DiagnosticCount - 1 do
  begin
    Tick;

    if (LSyntax.Diagnostic(LAt).Span.Start >= LDeclaration.NameSpan.Start) and
      (LSyntax.Diagnostic(LAt).Span.Start < ASite.Caret) then
    begin
      Exit(Answer(pbsUnresolved, pbiIncompleteSource));
    end;
  end;

  if LDeclaration.TypeForm in [pstfRecordHelper, pstfClassHelper, pstfTypeHelper] then
    Exit(PrimitiveHelperPrefix);

  if LDeclaration.AncestorSpan.Length > 0 then
  begin
    LAt := LSyntax.Lexical.TokenAt(LDeclaration.AncestorSpan.Finish - 1);
    LLast := LSyntax.Lexical.Token(LAt);
    while (LAt > 0) and (LLast.Kind in [ptkWhitespace, ptkComment, ptkDirective]) do
    begin
      Tick;
      Dec(LAt);
      LLast := LSyntax.Lexical.Token(LAt);
    end;

    if (LLast.Kind <> ptkSymbol) or (LLast.Symbol <> psoCloseParen) or
      (LLast.Span.Start < LDeclaration.AncestorSpan.Start) then
    begin
      Exit(Answer(pbsUnresolved, pbiIncompleteSource));
    end;
  end;
  { Own members were searched at the caller's exact caret. Search only bases
    now, so later members cannot be donated by a full-source receiver lookup. }
  Result := InheritedMembers(AOwner, ASite, AName, pmaInstance, pokNone, []);
end;

function TBindingQuery.InheritedMembers(const AOwner: INyxPascalBoundSymbol;
  const ASite: INyxPascalBindingSite; const AName: TNyxText;
  AAccess: TNyxPascalMemberAccess; AOperator: TNyxPascalOperatorKind;
  const AOwn: array of INyxPascalBoundSymbol): INyxPascalResolution;
var
  LSymbols: array of INyxPascalBoundSymbol;
  LBase: INyxPascalResolution;
  LSymbol: INyxPascalBoundSymbol;
  LAt: Integer;
  LCount: Integer;
  LTrailCount: Integer;
  LSeen: Integer;
  LCompare: Integer;
  LPossible: INyxPascalResolution;
  LConsidering: Boolean;
  LDuplicate: Boolean;
begin
  Tick;
  LCount := Length(AOwn);

  if LCount > FLimits.MaximumCandidates then
  begin
    raise ENyxPascalBindingLimit.Create('Inherited seed candidates exceed their query budget');
  end;
  SetLength(LSymbols, LCount);
  for LAt := 0 to LCount - 1 do
  begin
    Tick;
    LSymbols[LAt] := AOwn[LAt];
  end;
  LTrailCount := Length(FMembers);
  for LAt := 0 to LTrailCount - 1 do
  begin
    Tick;

    if SameSymbol(FMembers[LAt], AOwner) then
    begin
      Exit(Answer(pbsUnresolved, pbiRecursiveType));
    end;
  end;
  SetLength(FMembers, LTrailCount + 1);
  FMembers[LTrailCount] := AOwner;
  try
    LAt := 0;
    while True do
    begin
      LBase := Bases(AOwner, LAt);

      if LBase.Status = pbsMissing then
      begin
        Break;
      end;

      if LBase.Status <> pbsBound then
      begin
        { With an unknown style, a source name can still be absent from every
          possible parent. CORBA/Java contribute no implicit parent; only the
          inspected COM root needs checking. Never publish its positive members
          unless the actual style is known. All extra reads use the same budget. }

        if (LAt = 0) and (LBase.Issue = pbiCompilerContext) and
          (AOwner.Declaration.TypeForm = pstfInterface) and
          (AOwner.Declaration.AncestorSpan.Length = 0) and
          (InterfaceStyleOf(AOwner) = pifsUnknown) then
        begin
          LConsidering := FConsideringInterfaceRoot;
          FConsideringInterfaceRoot := True;
          try
            LPossible := Bases(AOwner, 0);
          finally
            FConsideringInterfaceRoot := LConsidering;
          end;

          if LPossible.Status = pbsMissing then Break;

          if LPossible.Status = pbsBound then
          begin
            LPossible := Member(LPossible.BoundType, ASite, AName, AAccess, AOperator);

            if LPossible.Status = pbsMissing then Break;
          end;
        end;
        Exit(LBase);
      end;
      LBase := Member(LBase.BoundType, ASite, AName, AAccess, AOperator);

      if LBase.Status in [pbsBound, pbsOverloads] then
      begin
        for LSeen := 0 to LBase.Count - 1 do
        begin
          Tick;
          LSymbol := LBase.SymbolAt(LSeen);
          LDuplicate := False;
          for LCompare := 0 to LCount - 1 do
          begin
            Tick;

            if SameBinding(LSymbols[LCompare], LSymbol) then
            begin
              LDuplicate := True;
            end;
          end;

          if not LDuplicate then
          begin

            if LCount >= FLimits.MaximumCandidates then
            begin
              raise ENyxPascalBindingLimit.Create('Inherited member candidates exceed their query budget');
            end;
            SetLength(LSymbols, LCount + 1);
            LSymbols[LCount] := LSymbol;
            Inc(LCount);
          end;
        end;
      end
      else if LBase.Status <> pbsMissing then
      begin
        Exit(LBase);
      end;
      Inc(LAt);
    end;
    Result := SymbolAnswer(LSymbols);
  finally
    SetLength(FMembers, LTrailCount);
  end;
end;

function TBindingQuery.FormalMember(const AFormal: INyxPascalFormalParameter;
  const ASite: INyxPascalBindingSite; const AName: TNyxText;
  AAccess: TNyxPascalMemberAccess; AOperator: TNyxPascalOperatorKind): INyxPascalResolution;
var
  LAt: Integer;
  LEntry: Integer;
  LSeen: Integer;
  LBound: INyxPascalSemanticType;
  LFound: INyxPascalResolution;
  LSymbols: array of INyxPascalBoundSymbol;
  LSymbol: INyxPascalBoundSymbol;
  LDuplicate: Boolean;
begin
  SetLength(LSymbols, 0);
  for LAt := 0 to AFormal.BoundCount - 1 do
  begin
    Tick;
    LBound := AFormal.BoundType(LAt);

    LFound := Member(LBound, ASite, AName, AAccess, AOperator);

    if LFound.Status = pbsMissing then Continue;
    if not (LFound.Status in [pbsBound, pbsOverloads]) then Exit(LFound);
    for LEntry := 0 to LFound.Count - 1 do
    begin
      Tick;
      LSymbol := LFound.SymbolAt(LEntry);
      LDuplicate := False;
      for LSeen := 0 to High(LSymbols) do
      begin
        Tick;

        if SameBinding(LSymbols[LSeen], LSymbol) then LDuplicate := True;
      end;

      if LDuplicate then Continue;

      if Length(LSymbols) >= FLimits.MaximumCandidates then
      begin
        raise ENyxPascalBindingLimit.Create('Formal member alternatives exceed their query budget');
      end;
      SetLength(LSymbols, Length(LSymbols) + 1);
      LSymbols[High(LSymbols)] := LSymbol;
    end;
  end;

  if Length(LSymbols) = 0 then Exit(Answer(pbsUnresolved, pbiGenericContext));
  Result := SymbolAnswer(LSymbols);
end;

function TBindingQuery.Member(const AType: INyxPascalSemanticType;
  const ASite: INyxPascalBindingSite; const AName: TNyxText;
  AAccess: TNyxPascalMemberAccess; AOperator: TNyxPascalOperatorKind): INyxPascalResolution;
var
  LFound: INyxPascalResolution;
  LSymbols: array of INyxPascalBoundSymbol;
  LSymbol: INyxPascalBoundSymbol;
  LCopy: TConstructorSymbol;
  LAt: Integer;
begin
  LFound := MemberCore(AType, ASite, AName, AAccess, AOperator);
  Result := LFound;

  if not (LFound.Status in [pbsBound, pbsOverloads]) or (LFound.Count = 0) then Exit;

  if (LFound.SymbolAt(0).Declaration.RoutineKind <> prkConstructor) or
    (psdfClassMember in LFound.SymbolAt(0).Declaration.Flags) then Exit;
  { Public semantic interfaces grant no sealed receiver ownership. Ordinary
    lookup retains its prior result; constructor inference requires our exact
    already-published type value, including its concrete generic environment. }

  if not (AType is TSemanticType) or
    (((AType as TSemanticType) as INyxPascalSemanticType) <> AType) then Exit;
  SetLength(LSymbols, LFound.Count);
  for LAt := 0 to LFound.Count - 1 do
  begin
    Tick;
    LSymbol := LFound.SymbolAt(LAt);

    if (LSymbol.Declaration.Kind <> psdkRoutine) or
      (LSymbol.Declaration.RoutineKind <> prkConstructor) or
      (psdfClassMember in LSymbol.Declaration.Flags) then Exit;
    LCopy := TConstructorSymbol.Create;
    LSymbols[LAt] := LCopy;
    LCopy.Owner := FWorld;
    LCopy.ModuleOrdinal := LSymbol.ModuleIndex;
    LCopy.DeclarationOrdinal := LSymbol.DeclarationIndex;
    LCopy.Bindings := EnvironmentOf(LSymbol);
    LCopy.ReceiverType := AType;
  end;
  Result := SymbolAnswer(LSymbols);
end;

function TBindingQuery.ConstructorResult(const ASymbol: INyxPascalBoundSymbol;
  const ASite: INyxPascalBindingSite): INyxPascalResolution;
var
  LContext: INyxPascalCompilerContext;
  LResult, LType: INyxPascalSemanticType;
begin
  CheckSymbol(ASymbol);

  if not (ASymbol is TConstructorSymbol) or
    (((ASymbol as TConstructorSymbol) as INyxPascalBoundSymbol) <> ASymbol) then
    Exit(Answer(pbsUnresolved, pbiUnsupportedType));
  LContext := ContextAt(ASite);

  if (LContext = nil) or (LContext.Family <> ppcFPC) or
    (LContext.Version.FullNumber <> 30301) or
    not (LContext.Mode in [ppmObjFPC, ppmDelphi, ppmDelphiUnicode]) then
    Exit(Answer(pbsUnresolved, pbiCompilerContext));
  LResult := (ASymbol as TConstructorSymbol).ReceiverType;
  LType := Unwrap(LResult);

  if (LType <> nil) and (LType.Kind = pstClassOf) then
  begin
    { Runtime metaclasses may construct a descendant. Completion promises the
      declared instance contract, never an unobserved dynamic class or value. }
    LResult := LType.Target;
    LType := Unwrap(LResult);
  end;

  if (LType = nil) or (LType.Kind <> pstDeclared) or (LType.Definition = nil) or
    not (LType.Definition.Declaration.TypeForm in [pstfClass, pstfRecord]) then
    Exit(Answer(pbsUnresolved, pbiUnsupportedType));
  Result := TypeAnswer(LResult);
end;

function TBindingQuery.ConstructorResults(const AFound: INyxPascalResolution;
  const ASite: INyxPascalBindingSite): INyxPascalResolution;
var
  LAt: Integer;
  LSymbol: INyxPascalBoundSymbol;
  LFound: INyxPascalResolution;
  LCommon: INyxPascalSemanticType;
begin
  LCommon := nil;
  for LAt := 0 to AFound.Count - 1 do
  begin
    Tick;
    LSymbol := AFound.SymbolAt(LAt);

    if (LSymbol.Declaration.Kind <> psdkRoutine) or
      (LSymbol.Declaration.RoutineKind <> prkConstructor) or
      (psdfClassMember in LSymbol.Declaration.Flags) then
      Exit(Answer(pbsUnresolved, pbiUnsupportedType));
    LFound := ConstructorResult(LSymbol, ASite);

    if LFound.Status <> pbsBound then Exit(LFound);

    if LCommon = nil then LCommon := LFound.BoundType
    else if not SameType(LCommon, LFound.BoundType) then
      Exit(Answer(pbsUnresolved, pbiOverloadContext));
  end;

  if LCommon = nil then Exit(Answer(pbsUnresolved, pbiUnsupportedType));
  Result := TypeAnswer(LCommon);
end;

function TBindingQuery.MemberCore(const AType: INyxPascalSemanticType;
  const ASite: INyxPascalBindingSite; const AName: TNyxText;
  AAccess: TNyxPascalMemberAccess; AOperator: TNyxPascalOperatorKind): INyxPascalResolution;
var
  LType: INyxPascalSemanticType;
  LOwner: INyxPascalBoundSymbol;
  LScope: Integer;
  LFound: INyxPascalResolution;
  LAt: Integer;
  LCount: Integer;
  LSymbol: INyxPascalBoundSymbol;
  LSymbols: array of INyxPascalBoundSymbol;
  LVisible: TNyxPascalKnowledge;
  LClassAllowed: Boolean;
  LOverload: Boolean;
  LHeader: TNyxPascalToken;
  LSyntax: INyxPascalSyntaxSnapshot;
  LFormal: INyxPascalFormalParameter;
begin
  Enter;
  try
    CheckSite(ASite);

    if AOperator = pokNone then
    begin
      NyxPascalBindingName(AName);
    end
    else if (Ord(AOperator) <= Ord(pokUnknown)) or
      (Ord(AOperator) > Ord(High(TNyxPascalOperatorKind))) then
    begin
      raise ENyxPascalBinding.Create('Select one supported typed operator family');
    end;

    if (AType = nil) or (AType.World <> FWorld) then
    begin
      raise ENyxPascalBinding.Create('This receiver type belongs to another semantic source world');
    end;

    if Pos('.', AName) > 0 then
    begin
      Exit(Answer(pbsUnresolved, pbiUnsupportedType));
    end;
    LType := Unwrap(AType);

    if (LType <> nil) and (LType.Kind = pstClassOf) then
    begin
      LType := Unwrap(LType.Target);
      AAccess := pmaType;
    end;

    if (LType <> nil) and (LType.Kind = pstFormal) then
    begin

      if not Supports(LType, INyxPascalFormalParameter, LFormal) then
      begin
        Exit(Answer(pbsUnresolved, pbiGenericContext));
      end;
      Exit(FormalMember(LFormal, ASite, AName, AAccess, AOperator));
    end;

    if (LType = nil) or (LType.Kind <> pstDeclared) or (LType.Definition = nil) then
    begin
      Exit(Answer(pbsUnresolved, pbiUnsupportedType));
    end;
    LOwner := LType.Definition;
    for LAt := 0 to High(FMembers) do
    begin
      Tick;

      if SameSymbol(FMembers[LAt], LOwner) then
      begin
        Exit(Answer(pbsUnresolved, pbiRecursiveType));
      end;
    end;
    LScope := LOwner.Declaration.NestedScope;

    if LScope < 0 then
    begin
      Exit(Answer(pbsUnresolved, pbiIncompleteSource));
    end;
    LSyntax := FWorld.ModuleAt(LOwner.ModuleIndex).Syntax;
    LFound := Exact(LOwner.ModuleIndex, LScope, LSyntax.Scope(LScope).Span.Finish, AName, False, AOperator);
    SetLength(LSymbols, 0);
    LCount := 0;
    LOverload := False;

    if not (LFound.Status in [pbsBound, pbsOverloads, pbsMissing]) then
    begin
      Exit(LFound);
    end;
    for LAt := 0 to LFound.Count - 1 do
    begin
      Tick;
      LSymbol := LFound.SymbolAt(LAt);
      LSymbol := InEnvironment(FWorld.Symbol(LSymbol.ModuleIndex, LSymbol.DeclarationIndex),
        EnvironmentOf(LOwner));
      LVisible := Access(LSymbol, LOwner, ASite);

      if LVisible = ppkUnknown then
      begin
        Exit(Answer(pbsUnresolved, pbiCompilerContext));
      end;

      if LVisible = ppkFalse then
      begin
        Exit(Answer(pbsInaccessible, pbiVisibility));
      end;
      LClassAllowed := (LSymbol.Declaration.Kind in [psdkType, psdkConstant, psdkEnumValue]) or
        (psdfClassMember in LSymbol.Declaration.Flags);

      if LSymbol.Declaration.Kind = psdkRoutine then
      begin
        LHeader := LSyntax.Lexical.Token(LSyntax.Lexical.TokenAt(LSymbol.Declaration.HeaderSpan.Start));
        LClassAllowed := LClassAllowed or (LHeader.Word = pkwConstructor);
      end;

      if (AAccess = pmaType) and not LClassAllowed then
      begin
        Exit(Answer(pbsInaccessible, pbiWrongMemberKind));
      end;
      SetLength(LSymbols, LCount + 1);
      LSymbols[LCount] := LSymbol;
      Inc(LCount);
      LOverload := LOverload or (psdfOverload in LSymbol.Declaration.Flags);
    end;

    if (LCount > 0) and not LOverload then
    begin
      Exit(SymbolAnswer(LSymbols));
    end;
    Result := InheritedMembers(LOwner, ASite, AName, AAccess, AOperator, LSymbols);
  finally
    Leave;
  end;
end;

function TBindingQuery.CollectMemberNames(const AType: INyxPascalSemanticType;
  const APrefix: TNyxText; var ANames: TGenericNames): INyxPascalResolution;
var
  LType: INyxPascalSemanticType;
  LOwner: INyxPascalBoundSymbol;
  LSyntax: INyxPascalSyntaxSnapshot;
  LBase: INyxPascalResolution;
  LAt: Integer;
  LTrail: Integer;
  LScope: Integer;
  LDeclaration: Integer;
  LName: TNyxText;
  LKey: TNyxText;
  LPosition: Integer;
  LCopy: Integer;
  LFormal: INyxPascalFormalParameter;
  LBound: INyxPascalSemanticType;
begin
  Enter;
  try
    LType := Unwrap(AType);

    if (LType <> nil) and (LType.Kind = pstClassOf) then
    begin
      LType := Unwrap(LType.Target);
    end;

    if (LType <> nil) and (LType.Kind = pstFormal) then
    begin

      if not Supports(LType, INyxPascalFormalParameter, LFormal) or (LFormal.BoundCount = 0) then
      begin
        Exit(Answer(pbsUnresolved, pbiGenericContext));
      end;
      for LAt := 0 to LFormal.BoundCount - 1 do
      begin
        Tick;
        LBound := LFormal.BoundType(LAt);

        Result := CollectMemberNames(LBound, APrefix, ANames);

        if Result <> nil then Exit;
      end;
      Exit(nil);
    end;

    if (LType = nil) or (LType.Kind <> pstDeclared) or (LType.Definition = nil) then
    begin
      Exit(Answer(pbsUnresolved, pbiUnsupportedType));
    end;
    LOwner := LType.Definition;
    for LAt := 0 to High(FMembers) do
    begin
      Tick;

      if SameSymbol(FMembers[LAt], LOwner) then
      begin
        Exit(Answer(pbsUnresolved, pbiRecursiveType));
      end;
    end;
    LScope := LOwner.Declaration.NestedScope;

    if LScope < 0 then
    begin
      Exit(Answer(pbsUnresolved, pbiIncompleteSource));
    end;
    LSyntax := FWorld.ModuleAt(LOwner.ModuleIndex).Syntax;

    if not SafeScope(LOwner.ModuleIndex, LScope, LSyntax.Scope(LScope).Span.Finish) then
    begin
      Exit(Answer(pbsUnresolved, pbiUnsupportedScope));
    end;
    LDeclaration := LSyntax.Scope(LScope).FirstDeclaration;
    while LDeclaration >= 0 do
    begin
      Tick;

      if (LSyntax.Declaration(LDeclaration).OperatorKind <> pokNone) or
        LSyntax.Declaration(LDeclaration).Anonymous then
      begin
        LDeclaration := LSyntax.Declaration(LDeclaration).Next;
        Continue;
      end;
      LName := LSyntax.Declaration(LDeclaration).Name;
      LKey := NyxPascalASCIIKey(LName);

      if Copy(LKey, 1, Length(APrefix)) = APrefix then
      begin
        LPosition := 0;
        while (LPosition < Length(ANames)) and
          (NyxPascalASCIIKey(ANames[LPosition]) < LKey) do
        begin
          Tick;
          Inc(LPosition);
        end;

        if (LPosition = Length(ANames)) or
          (NyxPascalASCIIKey(ANames[LPosition]) <> LKey) then
        begin
          SetLength(ANames, Length(ANames) + 1);
          for LCopy := High(ANames) downto LPosition + 1 do
          begin
            Tick;
            ANames[LCopy] := ANames[LCopy - 1];
          end;
          ANames[LPosition] := LName;
        end;
      end;
      LDeclaration := LSyntax.Declaration(LDeclaration).Next;
    end;
    LTrail := Length(FMembers);
    SetLength(FMembers, LTrail + 1);
    FMembers[LTrail] := LOwner;
    try
      LAt := 0;
      while True do
      begin
        LBase := Bases(LOwner, LAt);

        if LBase.Status = pbsMissing then
        begin
          Break;
        end;

        if LBase.Status <> pbsBound then
        begin
          Exit(LBase);
        end;
        Result := CollectMemberNames(LBase.BoundType, APrefix, ANames);

        if Result <> nil then
        begin
          Exit;
        end;
        Inc(LAt);
      end;
      Result := nil;
    finally
      SetLength(FMembers, LTrail);
    end;
  finally
    Leave;
  end;
end;

function TBindingQuery.Members(const AType: INyxPascalSemanticType;
  const ASite: INyxPascalBindingSite; const AQuery: TNyxPascalMemberQuery): INyxPascalMemberPage;
var
  LPage: TMemberPage;
  LPageLease: INyxPascalMemberPage;
  LReply: TResolution;
  LReplyLease: INyxPascalResolution;
  LNames: TGenericNames;
  LFound: INyxPascalResolution;
  LAt: Integer;
  LAlternative: Integer;
  LVisible: Integer;
begin
  CheckSite(ASite);
  AQuery.Validate;

  if (AType = nil) or (AType.World <> FWorld) then
  begin
    raise ENyxPascalBinding.Create('This receiver type belongs to another semantic source world');
  end;

  if AQuery.Limit > FLimits.MaximumCandidates then
  begin
    raise ENyxPascalBindingLimit.Create('Member page exceeds the world candidate budget');
  end;
  LPage := TMemberPage.Create;
  { Keep the constructing page private. Native interface function results can
    share their caller's destination; assigning Result now would expose an
    unfinished page if any later budget/cancellation check raises. }
  LPageLease := LPage;
  LPage.Following := AQuery.Offset;
  LPage.TotalValue := -1;
  LFound := CollectMemberNames(AType, NyxPascalASCIIKey(AQuery.Prefix), LNames);

  if LFound <> nil then
  begin
    LPage.Reply := Answer(LFound.Status, LFound.Issue, LFound.RelatedName);
    Result := LPageLease;
    Exit;
  end;
  LReply := TResolution.Create;
  LReplyLease := LReply;
  LReply.Owner := FWorld;
  LReply.Outcome := pbsBound;
  LReply.Reason := pbiNone;
  LVisible := 0;
  for LAt := 0 to High(LNames) do
  begin
    LFound := Member(AType, ASite, LNames[LAt], AQuery.Access);

    if LFound.Status in [pbsInaccessible, pbsMissing] then
    begin
      Continue;
    end;

    if not (LFound.Status in [pbsBound, pbsOverloads]) then
    begin
      LPage.More := False;
      LPage.Reply := Answer(LFound.Status, LFound.Issue, LFound.RelatedName);
      Result := LPageLease;
      Exit;
    end;
    for LAlternative := 0 to LFound.Count - 1 do
    begin
      Tick;

      if LVisible >= AQuery.Offset then
      begin

        if Length(LReply.Symbols) = AQuery.Limit then
        begin
          LPage.More := True;

          if not AQuery.IncludesTotal then
          begin
            LPage.Reply := LReplyLease;
            LPage.Following := AQuery.Offset + Length(LReply.Symbols);
            Result := LPageLease;
            Exit;
          end;
        end
        else
        begin
          SetLength(LReply.Symbols, Length(LReply.Symbols) + 1);
          LReply.Symbols[High(LReply.Symbols)] := LFound.SymbolAt(LAlternative);
        end;
      end;
      Inc(LVisible);
    end;
  end;
  LPage.Reply := LReplyLease;
  LPage.Following := AQuery.Offset + Length(LReply.Symbols);
  LPage.TotalValue := LVisible;
  Result := LPageLease;
end;

function TBindingQuery.DefaultProperty(const AType: INyxPascalSemanticType;
  const ASite: INyxPascalBindingSite; AAccess: TNyxPascalMemberAccess): INyxPascalResolution;
var
  LType: INyxPascalSemanticType;
  LOwner, LChosen, LCandidate: INyxPascalBoundSymbol;
  LSyntax: INyxPascalSyntaxSnapshot;
  LContext: INyxPascalCompilerContext;
  LFound, LBase: INyxPascalResolution;
  LAt, LTrail, LBaseAt: Integer;
  LVisible: TNyxPascalKnowledge;
begin
  Enter;
  try
    LType := Unwrap(AType);

    if (LType <> nil) and (LType.Kind = pstClassOf) then
    begin
      LType := Unwrap(LType.Target);
      AAccess := pmaType;
    end;

    if (LType = nil) or (LType.Kind <> pstDeclared) or (LType.Definition = nil) then
      Exit(Answer(pbsUnresolved, pbiUnsupportedType));
    LOwner := LType.Definition;
    LContext := ContextAt(FWorld.At(LOwner.ModuleIndex, LOwner.Declaration.Scope,
      LOwner.Declaration.HeaderSpan.Start));

    if (LContext = nil) or (LContext.Family <> ppcFPC) or
      (LContext.Version.FullNumber <> 30301) or
      not (LContext.Mode in [ppmObjFPC, ppmDelphi, ppmDelphiUnicode]) then
      Exit(Answer(pbsUnresolved, pbiCompilerContext));

    if LOwner.Declaration.NestedScope < 0 then Exit(Answer(pbsUnresolved, pbiIncompleteSource));
    LTrail := Length(FMembers);
    for LAt := 0 to LTrail - 1 do
    begin
      Tick;

      if SameSymbol(FMembers[LAt], LOwner) then Exit(Answer(pbsUnresolved, pbiRecursiveType));
    end;
    SetLength(FMembers, LTrail + 1);
    FMembers[LTrail] := LOwner;
    try
      LSyntax := FWorld.ModuleAt(LOwner.ModuleIndex).Syntax;
      LAt := LSyntax.Scope(LOwner.Declaration.NestedScope).FirstDeclaration;
      LChosen := nil;
      while LAt >= 0 do
      begin
        Tick;
        LCandidate := InEnvironment(FWorld.Symbol(LOwner.ModuleIndex, LAt), EnvironmentOf(LOwner));

        if (LCandidate.Declaration.Kind = psdkProperty) and LCandidate.Declaration.DefaultProperty then
        begin

          if LChosen <> nil then Exit(Answer(pbsAmbiguous, pbiAmbiguousName));
          LChosen := LCandidate;
        end;
        LAt := LCandidate.Declaration.Next;
      end;

      if LChosen <> nil then
      begin
        LVisible := Access(LChosen, LOwner, ASite);

        if LVisible = ppkUnknown then Exit(Answer(pbsUnresolved, pbiCompilerContext));

        if LVisible = ppkFalse then Exit(Answer(pbsInaccessible, pbiVisibility));

        if (AAccess = pmaType) and not (psdfClassMember in LChosen.Declaration.Flags) then
          Exit(Answer(pbsUnresolved, pbiWrongMemberKind));
        Exit(OneSymbol(LChosen));
      end;
      LBaseAt := 0;
      while True do
      begin
        LBase := Bases(LOwner, LBaseAt);

        if LBase.Status = pbsMissing then Break;

        if LBase.Status <> pbsBound then Exit(LBase);
        LFound := DefaultProperty(LBase.BoundType, ASite, AAccess);

        if LFound.Status <> pbsMissing then
        begin

          if (LFound.Status <> pbsBound) or (LFound.Count <> 1) then Exit(LFound);
          LCandidate := LFound.SymbolAt(0);

          if (LChosen <> nil) and not SameBinding(LChosen, LCandidate) then
            Exit(Answer(pbsAmbiguous, pbiAmbiguousName));
          LChosen := LCandidate;
        end;
        Inc(LBaseAt);
      end;

      if LChosen <> nil then Exit(OneSymbol(LChosen));
      Result := Answer(pbsMissing, pbiMissingName);
    finally
      SetLength(FMembers, LTrail);
    end;
  finally
    Leave;
  end;
end;

function TBindingQuery.IndexedProperty(const ASymbol: INyxPascalBoundSymbol;
  ACount: Integer): INyxPascalResolution;
var
  LSyntax: INyxPascalSyntaxSnapshot;
  LAt, LCount: Integer;
  LDecl: TNyxPascalSyntaxDeclaration;
begin
  CheckSymbol(ASymbol);
  LDecl := ASymbol.Declaration;

  if (LDecl.Kind <> psdkProperty) or (LDecl.PropertyParameters.Length = 0) or
    (LDecl.NestedScope < 0) or not (pspaRead in LDecl.PropertyAccess) then
    Exit(Answer(pbsUnresolved, pbiUnsupportedType));
  LSyntax := FWorld.ModuleAt(ASymbol.ModuleIndex).Syntax;

  if not LSyntax.Scope(LDecl.NestedScope).Complete then Exit(Answer(pbsUnresolved, pbiIncompleteSource));
  LAt := LSyntax.Scope(LDecl.NestedScope).FirstDeclaration;
  LCount := 0;
  while LAt >= 0 do
  begin
    Tick;
    LDecl := LSyntax.Declaration(LAt);

    if (LDecl.Kind <> psdkParameter) or (LDecl.TypeSpan.Length = 0) or
      (LDecl.Flags * [psdfIncomplete, psdfConditional, psdfUnsupported] <> []) then
      Exit(Answer(pbsUnresolved, pbiIncompleteSource));
    Inc(LCount);
    LAt := LDecl.Next;
  end;

  if (LCount < 1) or (LCount <> ACount) then Exit(Answer(pbsUnresolved, pbiUnsupportedType));
  Result := TypeOfSymbol(ASymbol);
end;

function TBindingQuery.ExpressionType(const ASite: INyxPascalBindingSite;
  const AWindow: TNyxPascalSpan; out AAccess: TNyxPascalMemberAccess): INyxPascalResolution;
var
  LLex: INyxPascalLexicalSnapshot;
  LAt: Integer;
  LToken: TNyxPascalToken;
  LAvailable: Boolean;
  LBadBoundary: Boolean;
  LAccess: TNyxPascalMemberAccess;
  LAnswer: INyxPascalResolution;
  LFPUChecked: Boolean;
  LFPUContext: INyxPascalCompilerContext;

  procedure ReadToken;
  begin
    while LAt < LLex.Count do
    begin
      Tick;
      LToken := LLex.Token(LAt);
      LAvailable := LToken.Span.Start < AWindow.Finish;

      if not LAvailable then Exit;

      if (LToken.Span.Start < AWindow.Start) or (LToken.Span.Finish > AWindow.Finish) then
      begin
        LBadBoundary := True;
        LAvailable := False;
        Exit;
      end;

      if not (LToken.Kind in [ptkWhitespace, ptkComment]) then Exit;
      Inc(LAt);
    end;
    LAvailable := False;
  end;

  procedure Next;
  begin
    Inc(LAt);
    ReadToken;
  end;

  function IsSymbol(AValue: TNyxPascalSymbol): Boolean;
  begin
    Result := LAvailable and (LToken.Kind = ptkSymbol) and (LToken.Symbol = AValue);
  end;

  { FPC stores local switches when constructing a node, after parsing its
    operands. Scanner requests leading the next token are still pending;
    IFOPT explicitly flushes even when traversing a skipped conditional. }
  function EffectiveNodeSwitch(ASetting: TCompilerNodeSetting): TNyxPascalKnowledge;
  var
    LView: INyxPascalTranslatedSyntax;
    LSource: INyxPascalTranslation;
    LNext, LPrevious, LIndex, LBefore, LAfter, LOrigin, LOffset: Integer;
    LRow: TNyxPascalTranslatedToken;
    LName, LValue: TNyxText;
    LToken: TNyxPascalToken;
    function Selected(const AContext: INyxPascalCompilerContext): TNyxPascalKnowledge;
    begin
      case ASetting of
        flsIEEEErrors: Result := NyxPascalCompilerPolicies(AContext).IEEEErrorsRequested;
        flsExcessPrecision: Result := NyxPascalCompilerPolicies(AContext).ExcessPrecisionSetting;
        flsStringH: Result := AContext.Option('H');
      end;
    end;

  begin
    Result := ppkUnknown;
    LView := FWorld.ModuleAt(ASite.ModuleIndex);
    LSource := LView.Source;
    LNext := LAt;
    while LNext < LLex.Count do
    begin
      Tick;
      LToken := LLex.Token(LNext);

      if not (LToken.Kind in [ptkWhitespace,ptkComment]) then Break;
      Inc(LNext);
    end;

    if LNext >= LLex.Count then Exit;
    LOrigin := LView.OriginAt(LLex.Token(LNext).Span.Start);

    if LOrigin < 0 then Exit;
    LAfter := LView.Origin(LOrigin).TranslationToken;

    if LSource.Token(LAfter).Activity <> ppkTrue then Exit;
    LPrevious := LNext-1;
    while LPrevious >= 0 do
    begin
      Tick;

      if not (LLex.Token(LPrevious).Kind in [ptkWhitespace,ptkComment]) then Break;
      Dec(LPrevious);
    end;

    if LPrevious < 0 then Exit;
    LOrigin := LView.OriginAt(LLex.Token(LPrevious).Span.Start);

    if LOrigin < 0 then Exit;
    LBefore := LView.Origin(LOrigin).TranslationToken;

    if (LBefore >= LAfter) or (LSource.Token(LBefore).Activity <> ppkTrue) then Exit;
    Result := Selected(LSource.State(LView.Origin(LOrigin).State));
    for LIndex := LBefore+1 to LAfter-1 do
    begin
      Tick;
      LRow := LSource.Token(LIndex);

      if LRow.Expanded or (LSource.PhysicalToken(LIndex).Kind <> ptkDirective) then Continue;
      NyxPascalDirectiveParts(LSource.Text(LIndex),LName,LValue,LOffset);

      if LName = 'ifopt' then
        Result := Selected(LSource.State(LRow.State))
      else if (LName = 'mode') and (LRow.Activity <> ppkFalse) then
      begin
        { A qualified ignored late MODE leaves the exact state unchanged and
          does not flush pending locals. An applied MODE has a distinct state;
          its handler has independently proved the immediate setting effect. }

        if LSource.Token(LIndex+1).State <> LRow.State then
          Result := Selected(LSource.State(LSource.Token(LIndex+1).State));
      end;
    end;
  end;

  { The inspected FPC node checks positive infinity only. This is an admission
    rule for a newly constructed real constant, not an abstract IEEE rule or a
    host exception mask. Unary negation reuses its admitted child separately. }
  function SpecialValueAdmission(const AValue: TNyxPascalIEEEValue): INyxPascalResolution;
  var LChecking: TNyxPascalKnowledge;
  begin
    Result := nil;

    if (AValue.Kind <> pifInfinity) or AValue.Negative then Exit;
    LChecking := EffectiveNodeSwitch(flsIEEEErrors);

    if LChecking = ppkUnknown then
      Result := Answer(pbsUnresolved,pbiCompilerContext,'IEEEERRORS')
    else if LChecking = ppkTrue then
      Result := Answer(pbsUnresolved,pbiUnsupportedType,'real constant range');
  end;

  { Existing finite arithmetic reports mathematical outcomes, independently of
    source admission. Only this explicit integration boundary projects those
    outcomes into abstract special values; the caller still checks its node. }
  function MathematicalOutcome(const AValue: TNyxPascalBinaryReal): TNyxPascalIEEEValue;
  begin
    case AValue.Status of
      rrsFinite: Result := TNyxPascalIEEEValue.Finite(AValue);
      rrsOverflow, rrsDivisionByZero:
        Result := TNyxPascalIEEEValue.Infinity(AValue.Precision,AValue.Negative);
      rrsInvalidOperation: Result := TNyxPascalIEEEValue.NotANumber(AValue.Precision);
    else Result := TNyxPascalIEEEValue.Unknown;
    end;
  end;

  { Untyped byte arrays select their managed/short specialization at the current
    compiler node. Named literal copies retain their value/encoding while the
    active H and Unicode mode select a type independently of declaration time. }
  function DefaultStringResult: INyxPascalResolution;
  var LSetting: TNyxPascalKnowledge; LContext: INyxPascalCompilerContext; LName: TNyxText;
  begin
    LSetting := EffectiveNodeSwitch(flsStringH);

    if LSetting = ppkUnknown then Exit(Answer(pbsUnresolved,pbiCompilerContext,'H'));
    LName := 'shortstring';

    if LSetting = ppkTrue then
    begin
      LContext := ContextAt(ASite);

      if (LContext = nil) or (LContext.Feature(ppfUnicodeStrings) = ppkUnknown) then
        Exit(Answer(pbsUnresolved,pbiCompilerContext,'UNICODESTRINGS'));
      LName := 'ansistring';

      if LContext.Feature(ppfUnicodeStrings) = ppkTrue then LName := 'unicodestring';
    end;
    Result := BuiltinType(ASite,LName,FLimits.ImplicitScopes = pispCompilerUnits);
  end;

  { Only result contracts are compared. Arguments stay opaque and no candidate
    is dropped by guessed conversions, arity, defaults or runtime values. }
  function ValueOf(const AFound: INyxPascalResolution; ACall: Boolean;
    out AValueAccess: TNyxPascalMemberAccess): INyxPascalResolution;
  var
    LIndex: Integer;
    LResolved: INyxPascalResolution;
    LCommon, LValue: INyxPascalSemanticType;
    LSymbol: INyxPascalBoundSymbol;
    LIEEE: INyxPascalIEEEConstant;
    LString: INyxPascalStringConstant;
    LAdmission: INyxPascalResolution;
  begin
    AValueAccess := pmaInstance;

    if not (AFound.Status in [pbsBound, pbsOverloads]) then Exit(AFound);

    if (AFound.Count = 1) and (AFound.SymbolAt(0).Declaration.Kind = psdkProperty) and
      ((AFound.SymbolAt(0).Declaration.PropertyParameters.Length > 0) or
       (AFound.SymbolAt(0).Declaration.PropertyAccess = [pspaWrite])) then
      Exit(Answer(pbsUnresolved, pbiUnsupportedType));

    if (AFound.Count > 0) and
      (AFound.SymbolAt(0).Declaration.RoutineKind = prkConstructor) then
      Exit(ConstructorResults(AFound, ASite));

    if ACall and (AFound.Count > 0) and
      (AFound.SymbolAt(0).Declaration.Kind = psdkRoutine) then
    begin
      LCommon := nil;
      for LIndex := 0 to AFound.Count - 1 do
      begin
        Tick;
        LSymbol := AFound.SymbolAt(LIndex);

        if LSymbol.Declaration.Kind <> psdkRoutine then Exit(Answer(pbsUnresolved, pbiOverloadContext));
        LResolved := TypeOfSymbol(LSymbol);

        if LResolved.Status <> pbsBound then Exit(LResolved);

        if LCommon = nil then LCommon := LResolved.BoundType
        else if not SameType(LCommon, LResolved.BoundType) then Exit(Answer(pbsUnresolved, pbiOverloadContext));
      end;
      Exit(TypeAnswer(LCommon));
    end;

    if AFound.BoundType <> nil then LResolved := AFound
    else
    begin

      if AFound.Count <> 1 then Exit(Answer(pbsUnresolved, pbiOverloadContext));

      if AFound.SymbolAt(0).Declaration.Kind = psdkType then AValueAccess := pmaType;
      LResolved := TypeOfSymbol(AFound.SymbolAt(0));

      if (LResolved.Status = pbsBound) and
        (AFound.SymbolAt(0).Declaration.Kind = psdkConstant) and
        Supports(LResolved.BoundType,INyxPascalIEEEConstant,LIEEE) then
      begin
        { A constant reference constructs its own node at the use site. Its
          initializer remains declaration-owned; current local checks apply to
          this copy without reinterpreting the original represented value. }
        LAdmission := SpecialValueAdmission(LIEEE.IEEEValue);

        if LAdmission <> nil then Exit(LAdmission);
      end;

      if (LResolved.Status = pbsBound) and
        (AFound.SymbolAt(0).Declaration.Kind = psdkConstant) and
        Supports(LResolved.BoundType,INyxPascalStringConstant,LString) and
        (LString.Form = pscfLiteral) and not LString.Wide then
      begin
        LAdmission := DefaultStringResult;

        if LAdmission.Status <> pbsBound then Exit(LAdmission);
        LResolved := StringAnswer(LAdmission.BoundType,LString);
      end;
    end;

    if (LResolved.Status <> pbsBound) or not ACall then Exit(LResolved);

    if AValueAccess = pmaType then Exit(Answer(pbsUnresolved, pbiUnsupportedType));
    LValue := Unwrap(LResolved.BoundType);

    if (LValue <> nil) and (LValue.Kind = pstReference) then LValue := Unwrap(LValue.Target);

    if (LValue = nil) or (LValue.Kind <> pstFunction) or (LValue.Target = nil) then
      Exit(Answer(pbsUnresolved, pbiUnsupportedType));
    Result := TypeAnswer(LValue.Target);
  end;

  { Bounded balanced argument/index windows preserve strings and nested groups.
    Empty/trailing list entries and crossed delimiters refuse. This deliberately
    supplies no argument expression typing or compiler-call admission. }
  function Arguments: Integer;
  var
    LStack: array of TNyxPascalSymbol;
    LDepth: Integer;
    LCount: Integer;
    LContent: Boolean;
    LClose: TNyxPascalSymbol;
  begin
    Result := -1;
    LDepth := 1;
    SetLength(LStack, 1);
    LStack[0] := LToken.Symbol;
    LCount := 0;
    LContent := False;
    Next;
    while LAvailable do
    begin
      Tick;

      if IsSymbol(psoOpenParen) or IsSymbol(psoOpenBracket) then
      begin

        if LDepth >= FLimits.MaximumDepth then
          raise ENyxPascalBindingLimit.Create('Postfix argument nesting exceeds its budget');
        SetLength(LStack, LDepth + 1);
        LStack[LDepth] := LToken.Symbol;
        Inc(LDepth);
        LContent := True;
      end
      else if IsSymbol(psoCloseParen) or IsSymbol(psoCloseBracket) then
      begin
        LClose := psoCloseParen;

        if LStack[LDepth - 1] = psoOpenBracket then LClose := psoCloseBracket;

        if LToken.Symbol <> LClose then Exit;
        Dec(LDepth);

        if LDepth = 0 then
        begin

          if LContent then Inc(LCount)
          else if LCount > 0 then Exit;
          Next;
          Exit(LCount);
        end;
      end
      else if IsSymbol(psoComma) and (LDepth = 1) then
      begin

        if not LContent then Exit;
        Inc(LCount);
        LContent := False;
      end
      else
      begin

        if LToken.Kind in [ptkDirective, ptkInvalid, ptkAssembly, ptkEOF] then Exit;
        LContent := True;
      end;
      Next;
    end;
  end;

  { A cast follows an authored type contract. It neither performs an unsafe
    conversion nor predicts whether a checked assertion will succeed at runtime. }
  function CastContext: Boolean;
  var
    LContext: INyxPascalCompilerContext;
  begin
    LContext := ContextAt(ASite);
    Result := (LContext <> nil) and (LContext.Family = ppcFPC) and
      (LContext.Version.FullNumber = 30301) and
      (LContext.Mode in [ppmObjFPC, ppmDelphi, ppmDelphiUnicode]);
  end;

  function ReadTypeTarget(AMemberBoundary: Boolean = False): INyxPascalResolution;
  var
    LStart, LFinish, LAngle, LDot: Integer;
    LCurrent, LMember: INyxPascalResolution;

    function BindCurrent: INyxPascalResolution;
    var
      LParsed: INyxPascalTypeSyntax;
      LWasTypeTarget: Boolean;
    begin
      LParsed := NewNyxPascalTypeSyntax(LLex, NyxPascalSpan(LStart, LFinish - LStart),
        TNyxPascalTypeOptions.Defaults, FCancel);

      if not LParsed.Complete or not (LParsed.Node(LParsed.Root).Kind in [ptxName, ptxQualified, ptxGeneric]) then
        Exit(Answer(pbsUnresolved, pbiUnsupportedType));
      LWasTypeTarget := FBindingTypeTarget;
      FBindingTypeTarget := True;
      try
        Result := BindNode(ASite, LParsed, LParsed.Root);
      finally
        FBindingTypeTarget := LWasTypeTarget;
      end;
    end;

  begin
    LStart := LToken.Span.Start;
    LFinish := LStart;
    repeat

      if not LAvailable or (LToken.Kind <> ptkWord) then Exit(Answer(pbsUnresolved, pbiUnsupportedType));
      LFinish := LToken.Span.Finish;
      Next;

      if IsSymbol(psoLess) then
      begin
        LAngle := 1;
        Next;
        while LAvailable and (LAngle > 0) do
        begin

          if IsSymbol(psoLess) then Inc(LAngle)
          else if IsSymbol(psoGreater) then Dec(LAngle)
          else if IsSymbol(psoShiftRight) then Dec(LAngle, 2);

          if (LAngle < 0) or (LAngle > FLimits.MaximumDepth) then
            Exit(Answer(pbsUnresolved, pbiUnsupportedType));
          LFinish := LToken.Span.Finish;
          Next;
        end;

        if LAngle <> 0 then Exit(Answer(pbsUnresolved, pbiUnsupportedType));
      end;

      if AMemberBoundary then
      begin
        LCurrent := BindCurrent;

        if not IsSymbol(psoDot) then Exit(LCurrent);

        if (LCurrent.Status = pbsBound) and (LCurrent.BoundType <> nil) then
        begin
          { A constructed type may be followed by a nested type or an ordinary
            value member. Ask the existing member binder instead of consuming a
            constructor/function name as part of the type expression. Leave the
            dot for normal postfix processing when this is not a nested type. }
          LDot := LAt;
          Next;

          if not LAvailable or (LToken.Kind <> ptkWord) then
            Exit(Answer(pbsUnresolved, pbiUnsupportedType));
          LMember := Member(LCurrent.BoundType, ASite, LLex.Identifier(LAt), pmaType);
          LAt := LDot;
          ReadToken;

          if (LMember.Status <> pbsBound) or (LMember.Count <> 1) or
            (LMember.SymbolAt(0).Declaration.Kind <> psdkType) then Exit(LCurrent);
        end;
      end;

      if not IsSymbol(psoDot) then Break;
      Next;
    until False;
    Result := BindCurrent;
  end;

  { Explicit routine arguments use the existing type arena, with balanced group
    boundaries so a relational operator inside a bound cannot close the list.
    Only argument children bind; the already-resolved callee owns its namespace,
    visibility and receiver environment independently of this syntactic base. }
  function ReadRoutineArguments(const AFound: INyxPascalResolution;
    ANameStart: Integer): INyxPascalResolution;
  var
    LAngle, LDepth, LFinish: Integer;
    LStack: array of TNyxPascalSymbol;
    LClose: TNyxPascalSymbol;
    LClosed: Boolean;
    LParsed: INyxPascalTypeSyntax;
  begin
    LAngle := 0;
    LDepth := 0;
    LFinish := ANameStart;
    LClosed := False;
    while LAvailable do
    begin
      Tick;

      if IsSymbol(psoOpenParen) or IsSymbol(psoOpenBracket) then
      begin

        if LDepth >= FLimits.MaximumDepth then
          raise ENyxPascalBindingLimit.Create('Generic argument groups exceed their query depth');
        SetLength(LStack, LDepth + 1);
        LStack[LDepth] := LToken.Symbol;
        Inc(LDepth);
      end
      else if IsSymbol(psoCloseParen) or IsSymbol(psoCloseBracket) then
      begin

        if LDepth = 0 then Exit(Answer(pbsUnresolved, pbiUnsupportedType));
        LClose := psoCloseParen;

        if LStack[LDepth - 1] = psoOpenBracket then LClose := psoCloseBracket;

        if LToken.Symbol <> LClose then Exit(Answer(pbsUnresolved, pbiUnsupportedType));
        Dec(LDepth);
      end
      else if LDepth = 0 then
      begin

        if IsSymbol(psoLess) then Inc(LAngle)
        else if IsSymbol(psoGreater) then Dec(LAngle)
        else if IsSymbol(psoShiftRight) then Dec(LAngle, 2);

        if LAngle < 0 then Exit(Answer(pbsUnresolved, pbiUnsupportedType));

        if LAngle > FLimits.MaximumDepth then
          raise ENyxPascalBindingLimit.Create('Generic arguments exceed their query depth');

        if LAngle = 0 then
        begin
          LFinish := LToken.Span.Finish;
          LClosed := True;
          Next;
          Break;
        end;
      end;

      if LToken.Kind in [ptkDirective, ptkInvalid, ptkAssembly, ptkEOF] then
        Exit(Answer(pbsUnresolved, pbiUnsupportedType));
      Next;
    end;

    if not LClosed or (LDepth <> 0) then Exit(Answer(pbsUnresolved, pbiUnsupportedType));
    LParsed := NewNyxPascalTypeSyntax(LLex, NyxPascalSpan(ANameStart, LFinish - ANameStart),
      TNyxPascalTypeOptions.Defaults, FCancel);

    if not LParsed.Complete or (LParsed.Node(LParsed.Root).Kind <> ptxGeneric) then
      Exit(Answer(pbsUnresolved, pbiUnsupportedType));
    Result := ApplyRoutineArguments(ASite, AFound, LParsed, LParsed.Root);
  end;

  { Recursive grouping shares this query's token/depth/work scope. }
  function Postfix(out AValueAccess: TNyxPascalMemberAccess): INyxPascalResolution; forward;
  { Precedence is parsed independently of operator admission. Declared result
    contracts and qualified builtin rules supply types without evaluating code. }
  function Expression(out AValueAccess: TNyxPascalMemberAccess): INyxPascalResolution; forward;

  { Both typed decoding and opaque syntax require the same authored/effective
    adjacency proof. Projection separators never concatenate source literals. }
  function AdjacentFragments(const APrevious: TNyxPascalSyntaxOrigin): Boolean;
  var
    LView: INyxPascalTranslatedSyntax;
    LCurrent: TNyxPascalSyntaxOrigin;
    LPreviousToken, LCurrentToken: TNyxPascalToken;
  begin
    Result := False;

    if not LAvailable or not (LToken.Kind in [ptkString, ptkCharacter]) then Exit;
    LView := FWorld.ModuleAt(ASite.ModuleIndex);
    LCurrent := LView.Origin(LView.OriginAt(LToken.Span.Start));

    if APrevious.Occurrence <> LCurrent.Occurrence then Exit;

    if (APrevious.Expansion < 0) and (LCurrent.Expansion < 0) then
      Exit(APrevious.PhysicalSpan.Finish = LCurrent.PhysicalSpan.Start);

    if (APrevious.Expansion < 0) or (APrevious.Expansion <> LCurrent.Expansion) then Exit;
    LPreviousToken := NyxPascalEffectiveToken(LView.Source, APrevious.TranslationToken);
    LCurrentToken := NyxPascalEffectiveToken(LView.Source, LCurrent.TranslationToken);
    Result := LPreviousToken.Span.Finish = LCurrentToken.Span.Start;
  end;

  { Decode closed literals through the existing exact-value owner. Compiler
    range/specialization rules require their qualified source context; source
    origins, not the projection's inserted spaces, decide fragment adjacency. }
  function LiteralPrimary(out ACandidate: Boolean): INyxPascalResolution;
  const
    CTrueLiteral: TNyxText = 'true';
    CFalseLiteral: TNyxText = 'false';
    CUpper: array[0..7] of TNyxText = ('127','255','32767','65535',
      '2147483647','4294967295','9223372036854775807','18446744073709551615');
    CLower: array[0..7] of TNyxText = ('128','0','32768','0',
      '2147483648','0','9223372036854775808','0');
    CIntegerNames: array[0..7] of TNyxText = ('shortint','byte','smallint','word',
      'longint','longword','int64','qword');
  var
    LLiteral: TLiteralType;
    LLease: INyxPascalSemanticType;
    LContext: INyxPascalCompilerContext;
    LView: INyxPascalTranslatedSyntax;
    LStart, LFinish, LAtBound, LIndex, LScalar, LUnitCount, LBase, LCode: Integer;
    LPrevious: TNyxPascalSyntaxOrigin;
    LSpelling, LName, LPrimitive: TNyxText;
    LNegative, LSigned, LUTF8Checked, LUTF8, LStringASCII: Boolean;
    LValue, LLower: TNyxPascalConstantValue;
    LTypeAnswer: INyxPascalResolution;
    LIdentity: INyxPascalIntrinsicIdentity;
    LRealLiteral: TIEEERealLiteralType;
    LSourceValue: TNyxPascalRealValue;
    LParsedValue: TNyxPascalBinaryReal;
    LIEEEValue: TNyxPascalIEEEValue;
    LAdmission: INyxPascalResolution;
    LParser: TNyxPascalRealParserPolicy;
    LMinimum: TNyxPascalRealPrecisionRequest;
    LPrecision: TNyxPascalBinaryPrecision;
    LFPU: TNyxPascalFPUReference;
    LRoot: INyxPascalCompilerContext;
    LFloat: INyxPascalFloatingIdentity;
    LRealBudget: INyxPascalAnalysisBudget;

    function Primitive(const AName: TNyxText): INyxPascalResolution;
    begin
      Tick;

      if (FLimits.ImplicitScopes = pispCompilerUnits) and (FWorld.FindModule('System') < 0) then
        Exit(Answer(pbsUnresolved, pbiMissingModule, 'System'));
      Result := BuiltinType(FWorld.At(ASite.ModuleIndex, ASite.ScopeIndex, LStart),
        AName, FLimits.ImplicitScopes = pispCompilerUnits);
    end;

    { An explicit decoded UTF-8 file alone does not establish the compiler's
      source codepage. Follow earlier authored requests in translation order,
      including include occurrences and uncertain conditional participation. }
    function UTF8Source: Boolean;
    var
      LSource: INyxPascalTranslation;
      LRow: TNyxPascalTranslatedToken;
      LAt, LOffset: Integer;
      LDirective, LArgument: TNyxText;
    begin

      if LUTF8Checked then Exit(LUTF8);
      LUTF8Checked := True;
      LUTF8 := False;
      LSource := LView.Source;
      for LAt := 0 to LView.Origin(LView.OriginAt(LStart)).TranslationToken - 1 do
      begin
        Tick;
        LRow := LSource.Token(LAt);

        { Unknown textual effects revoke encoding evidence as well as ordinary
          language settings. A later MODE cannot resurrect an earlier codepage
          proof; an authored UTF8 request must establish it again. }
        if (LRow.Activity = ppkUnknown) or (LSource.State(LRow.State).Mode = ppmUnknown) then
          LUTF8 := False;

        if (LRow.Activity = ppkFalse) or (LSource.PhysicalToken(LAt).Kind <> ptkDirective) then Continue;
        NyxPascalDirectiveParts(LSource.Text(LAt), LDirective, LArgument, LOffset);

        if LDirective = 'codepage' then
          LUTF8 := (LRow.Activity = ppkTrue) and (NyxPascalASCIIKey(LArgument) = 'utf8');
      end;
      Result := LUTF8;
    end;

    procedure AppendUnit(AValue: Integer);
    var
      LCapacity: Integer;
    begin
      Tick;

      if LUnitCount = Length(LLiteral.FUnits) then
      begin
        LCapacity := Length(LLiteral.FUnits);

        if LCapacity > High(Integer) div 2 then
          raise ENyxPascalBindingLimit.Create('Literal code units exceed their bounded vector');

        if LCapacity = 0 then LCapacity := 16 else LCapacity := LCapacity * 2;
        NyxPascalAdmitAnalysis(FCancel, papFrozenRow, LCapacity - Length(LLiteral.FUnits));
        SetLength(LLiteral.FUnits, LCapacity);
      end;
      LLiteral.FUnits[LUnitCount] := AValue;

      if AValue >= $80 then LStringASCII := False;
      Inc(LUnitCount);
    end;

    procedure AppendScalar(AValue: Integer);
    begin

      if AValue <= $ffff then AppendUnit(AValue)
      else
      begin
        Dec(AValue, $10000);
        AppendUnit($d800 + (AValue shr 10));
        AppendUnit($dc00 + (AValue and $3ff));
      end;
    end;

    { Prior byte escapes are decoded together when the compiler promotes a
      literal to wide storage. Invalid UTF-8 stays an explicit unsupported
      conversion; no replacement character or host ANSI policy is guessed. }
    function MakeWide: Boolean;
    var
      LRead, LWrite, LLead, LTails, LMin, LValue, LTail: Integer;
    begin
      Result := True;

      if LLiteral.FWide then Exit;
      LRead := 0;
      LWrite := 0;
      while LRead < LUnitCount do
      begin
        Tick;
        LLead := LLiteral.FUnits[LRead];
        Inc(LRead);
        LValue := LLead;
        LTails := 0;
        LMin := 0;

        if LLead >= $80 then
        begin

          if not UTF8Source then Exit(False);

          if (LLead >= $c2) and (LLead <= $df) then
          begin LTails := 1; LValue := LLead and $1f; LMin := $80; end
          else if (LLead >= $e0) and (LLead <= $ef) then
          begin LTails := 2; LValue := LLead and $f; LMin := $800; end
          else if (LLead >= $f0) and (LLead <= $f4) then
          begin LTails := 3; LValue := LLead and $7; LMin := $10000; end
          else Exit(False);
          while LTails > 0 do
          begin
            Tick;

            if LRead >= LUnitCount then Exit(False);
            LTail := LLiteral.FUnits[LRead];

            if (LTail < $80) or (LTail > $bf) then Exit(False);
            LValue := LValue * 64 + (LTail and $3f);
            Inc(LRead);
            Dec(LTails);
          end;

          if (LValue < LMin) or (LValue > $10ffff) or
            ((LValue >= $d800) and (LValue <= $dfff)) then Exit(False);
        end;

        if LValue > $ffff then
        begin
          Dec(LValue, $10000);
          LLiteral.FUnits[LWrite] := $d800 + (LValue shr 10);
          Inc(LWrite);
          LValue := $dc00 + (LValue and $3ff);
        end;
        LLiteral.FUnits[LWrite] := LValue;
        Inc(LWrite);
      end;
      LUnitCount := LWrite;
      LLiteral.FWide := True;
    end;

  begin
    ACandidate := LAvailable and (LToken.Kind in [ptkNumber, ptkString, ptkCharacter]);
    LSigned := IsSymbol(psoPlus) or IsSymbol(psoMinus);
    ACandidate := ACandidate or LSigned;
    LName := '';

    if LAvailable and (LToken.Kind = ptkWord) and not LToken.Escaped and
      (LToken.Span.Length in [4, 5]) then
    begin
      LName := NyxPascalASCIIKey(LLex.Identifier(LAt));

      if (LName = CTrueLiteral) or (LName = CFalseLiteral) then
      begin
        Result := Lookup(ASite, LName);

        if (Result.Status <> pbsMissing) or (Result.Issue <> pbiMissingName) then
        begin
          ACandidate := Result.Status <> pbsBound;
          Exit;
        end;
        ACandidate := True;
      end;
    end;
    Result := nil;

    if not ACandidate then Exit;
    Result := Answer(pbsUnresolved, pbiUnsupportedType);
    LStart := LToken.Span.Start;
    LView := FWorld.ModuleAt(ASite.ModuleIndex);
    LContext := ContextAt(FWorld.At(ASite.ModuleIndex, ASite.ScopeIndex, LStart));

    if (LContext = nil) or (LContext.Family <> ppcFPC) or
      (LContext.Version.FullNumber <> 30301) or not (LContext.Mode in [ppmDelphi, ppmDelphiUnicode]) then
      Exit(Answer(pbsUnresolved, pbiCompilerContext));
    LPrimitive := '';
    LNegative := IsSymbol(psoMinus);

    if LSigned then Next;

    if not LAvailable or not LToken.Complete then Exit;
    LFinish := LToken.Span.Finish;
    LRealLiteral := nil;

    if (LToken.Kind = ptkNumber) and LToken.RealNumber then
    begin
      { Literal parsing precedes consumption of this numeric token. A source
        setting between its unary sign and token belongs to the numeric token;
        a following setting must not retag the already parsed value. }
      LContext := ContextAt(FWorld.At(ASite.ModuleIndex,ASite.ScopeIndex,LToken.Span.Start));

      if (LContext = nil) or (LContext.Family <> ppcFPC) or
        (LContext.Version.FullNumber <> 30301) or not (LContext.Mode in [ppmDelphi,ppmDelphiUnicode]) then
        Exit(Answer(pbsUnresolved,pbiCompilerContext));
    end
    else
    begin
      if LToken.Kind in [ptkString,ptkCharacter] then LLiteral := TStringLiteralType.Create
      else LLiteral := TLiteralType.Create;
      LLease := LLiteral;
    end;

    if LToken.Kind = ptkNumber then
    begin

      if LToken.RealNumber then
      begin
        LParser := NyxPascalRealParser(LContext);

        if (LParser.Kind <> prpkNearestEven) or
          not (LParser.Precision in [pbpBinary64,pbpX87Extended80]) then
          Exit(Answer(pbsUnresolved,pbiCompilerContext,'REALPARSER'));
        LMinimum := NyxPascalCompilerPolicies(LContext).MinimumRealPrecisionRequested;

        if LMinimum = prprUnknown then Exit(Answer(pbsUnresolved,pbiCompilerContext,'MINFPCONSTPREC'));
        LFPU := NyxPascalSelectedFPU(LContext);

        if not LFPU.Known or (LFPU.Name = 'none') then
          Exit(Answer(pbsUnresolved,pbiCompilerContext,'FPUTYPE'));
        if not ((LFPU.Name = 'x87') or (LFPU.Name = 'sse') or (LFPU.Name = 'sse2') or
          (LFPU.Name = 'sse3') or (LFPU.Name = 'ssse3') or (LFPU.Name = 'sse41') or
          (LFPU.Name = 'sse42') or (LFPU.Name = 'avx') or (LFPU.Name = 'fma') or
          (LFPU.Name = 'avx2') or (LFPU.Name = 'avx512f')) then
          Exit(Answer(pbsUnresolved,pbiCompilerContext,'FPUTYPE'));
        LRoot := NyxPascalBootstrapContext(LContext);

        if NyxPascalCompilerScope(LContext) = pcsoSystemBootstrap then LRoot := LContext;

        if (LRoot = nil) or (LRoot.Family <> ppcFPC) or (LRoot.Version.FullNumber <> 30301) or
          (LRoot.Symbol(NyxPascalDefine('CPUI386')).IsDefined <> ppkTrue) then
          Exit(Answer(pbsUnresolved,pbiCompilerContext,'real literal target'));

        if FSteps >= FLimits.MaximumSteps then
          raise ENyxPascalBindingLimit.Create('Pascal semantic steps exceed their query budget');
        { Arithmetic consumes the existing remaining semantic-step allowance,
          including when a caller supplies no aggregate budget. A supplied parent
          still receives every admission; no independent larger quota is added. }
        LRealBudget := NewNyxPascalAnalysisBudget(FLimits.MaximumSteps-FSteps,FCancel);
        try
          try
            { The qualified RTL parser takes the complete original short-string
              token. Longer inputs refuse; truncation would invent a value. }
            if Length(LLex.Text(LAt)) > 255 then Exit;
            LSourceValue := NyxPascalRealToken(LLex.Text(LAt),LRealBudget);

            if LNegative then LSourceValue := LSourceValue.Negated;
            LParsedValue := NyxPascalRoundRealToEven(LSourceValue,LParser.Precision,LRealBudget);
            LIEEEValue := MathematicalOutcome(LParsedValue);

            if not (LIEEEValue.Kind in [pifFinite,pifInfinity]) then Exit;
            LPrecision := pbpBinary32;

            if LMinimum = prprBinary64 then LPrecision := pbpBinary64;
            LPrimitive := 'extended';

            if (LIEEEValue.Kind = pifInfinity) or
              LParsedValue.SameValue(LParsedValue.RoundToEven(LPrecision,LRealBudget),LRealBudget) then
            begin

              if LPrecision = pbpBinary32 then LPrimitive := 'single' else LPrimitive := 'double';
            end;
          except
            on ENyxPascalValue do Exit;
          end;
        finally
          Inc(FSteps,LRealBudget.Receipt.Total); LRealBudget.Retire;
        end;
        { FPC constructs/checks the unsigned real node before consuming this
          token or applying its unary sign. Pending locals follow the same
          scanner chronology as other real nodes; MIN/FPU remain immediate. }
        { Nearest-even rounding is sign symmetric. Keep the signed parser
          result's original Inexact evidence, but check its unsigned node. }
        if LNegative then LAdmission := SpecialValueAdmission(LIEEEValue.Negated)
        else LAdmission := SpecialValueAdmission(LIEEEValue);

        if LAdmission <> nil then Exit(LAdmission);

        if LIEEEValue.Kind = pifFinite then LRealLiteral := TRealLiteralType.Create
        else LRealLiteral := TIEEERealLiteralType.Create;
        LLiteral := LRealLiteral; LLease := LLiteral;
        LRealLiteral.FSourceValue := LSourceValue;
        LRealLiteral.FParsedValue := LIEEEValue;
        LLiteral.FLiteralKind := plkReal;
        Next;
      end
      else
      begin
        LBase := 10;
        case LToken.NumberBase of
          pnbDecimal: LBase := 10;
          pnbHexadecimal: LBase := 16;
          pnbOctal: LBase := 8;
          pnbBinary: LBase := 2;
        end;
        try
          LSpelling := LLex.Text(LAt);
          NyxPascalAdmitAnalysis(FCancel, papSourceProof, Length(LSpelling));
          LValue := NyxPascalIntegerToken(LSpelling, LBase);

          if LNegative then LValue := LValue.Negated;
        except
          on ENyxPascalValue do Exit;
        end;

        if LValue.Kind <> pvvInteger then Exit;
        for LAtBound := 0 to High(CUpper) do
        begin
          Tick;
          LLower := NyxPascalIntegerToken(CLower[LAtBound], 10).Negated;

          if (LValue.Number.CompareTo(LLower.Number) >= 0) and
            (LValue.Number.CompareTo(NyxPascalIntegerToken(CUpper[LAtBound], 10).Number) <= 0) then
          begin
            LPrimitive := CIntegerNames[LAtBound];
            Break;
          end;
        end;
        LLiteral.FLiteralKind := plkInteger;
        LLiteral.FValue := LValue;
        Next;
      end;
    end
    else if LSigned then Exit
    else if (LName = CTrueLiteral) or (LName = CFalseLiteral) then
    begin
      LLiteral.FLiteralKind := plkBoolean;
      LLiteral.FValue := TNyxPascalConstantValue.BooleanValue(LName = CTrueLiteral);
      LPrimitive := 'boolean';
      Next;
    end
    else if LToken.Kind in [ptkString, ptkCharacter] then
    begin
      LUnitCount := 0;
      LUTF8Checked := False;
      LUTF8 := False;
      LStringASCII := True;
      repeat

        if not LToken.Complete then Exit;
        LSpelling := LLex.Text(LAt);
        LFinish := LToken.Span.Finish;

        if LToken.Kind = ptkCharacter then
        begin
          LSpelling := Copy(LSpelling, 2, Length(LSpelling));
          LBase := 10;

          if Length(LSpelling) = 0 then Exit;
          case LSpelling[1] of
            '$': LBase := 16;
            '&': LBase := 8;
            '%': LBase := 2;
          end;
          try
            NyxPascalAdmitAnalysis(FCancel, papSourceProof, Length(LSpelling));
            LValue := NyxPascalIntegerToken(LSpelling, LBase);
          except
            on ENyxPascalValue do Exit;
          end;

          if LValue.Number.CompareTo(TNyxPascalConstantValue.IntegerValue($10ffff).Number) > 0 then Exit;
          LCode := StrToInt(String(LValue.Number.ToText));

          if (LCode > 255) or (Length(LSpelling) > 3) then
          begin

            if not MakeWide then Exit;
          end
          else if LLiteral.FWide and (LCode >= $80) and not UTF8Source then
            Exit(Answer(pbsUnresolved, pbiCompilerContext));
          AppendScalar(LCode);
        end
        else
        begin
          LSpelling := Copy(LSpelling, 2, Length(LSpelling) - 2);
          LIndex := 1;
          while LIndex <= Length(LSpelling) do
          begin
            Tick;

            if not NyxNextScalar(LSpelling, LIndex, LScalar) then Exit;

            if LScalar = 39 then
            begin

              if (LIndex > Length(LSpelling)) or (LSpelling[LIndex] <> '''') then Exit;
              Inc(LIndex);
            end;

            if LScalar >= $80 then
            begin

              if not UTF8Source then Exit(Answer(pbsUnresolved, pbiCompilerContext));

              if not MakeWide then Exit;
            end;
            AppendScalar(LScalar);
          end;
        end;
        LPrevious := LView.Origin(LView.OriginAt(LToken.Span.Start));
        Next;
      until not AdjacentFragments(LPrevious);
      SetLength(LLiteral.FUnits, LUnitCount);

      if LLiteral.FWide then TStringLiteralType(LLiteral).FEncoding := psceUTF16
      else if LStringASCII then TStringLiteralType(LLiteral).FEncoding := psceASCII
      else if UTF8Source then TStringLiteralType(LLiteral).FEncoding := psceUTF8;

      if LUnitCount = 1 then
      begin
        LLiteral.FLiteralKind := plkCharacter;
        LLiteral.FValue := TNyxPascalConstantValue.IntegerValue(LLiteral.FUnits[0]).AsOrdinal;

        if LLiteral.FWide then LPrimitive := 'widechar' else LPrimitive := 'ansichar';
      end
      else
      begin
        LLiteral.FLiteralKind := plkString;

        if LLiteral.FWide then LPrimitive := 'unicodestring'
        else
        begin
          LLiteral.TypeKind := pstArray;
          LTypeAnswer := Primitive('ansichar');

          if LTypeAnswer.Status <> pbsBound then Exit(LTypeAnswer);
          LLiteral.Element := LTypeAnswer.BoundType;

          if LContext.Option('H') = ppkFalse then LPrimitive := 'shortstring'
          else if LContext.Option('H') = ppkTrue then
          begin

            if LContext.Feature(ppfUnicodeStrings) = ppkTrue then LPrimitive := 'unicodestring'
            else if LContext.Feature(ppfUnicodeStrings) = ppkFalse then LPrimitive := 'ansistring';
          end;
        end;
      end;
    end
    else Exit;
    LLiteral.Owner := FWorld;
    LLiteral.FContext := LContext; LLiteral.FModule := ASite.ModuleIndex;
    LLiteral.FSpan := NyxPascalSpan(LStart, LFinish - LStart);

    if (LPrimitive = '') and (LLiteral.TypeKind <> pstArray) then Exit;

    if LPrimitive <> '' then
    begin
      LTypeAnswer := Primitive(LPrimitive);

      if LTypeAnswer.Status <> pbsBound then Exit(LTypeAnswer);
      if (LRealLiteral <> nil) and
        (not Supports(LTypeAnswer.BoundType,INyxPascalFloatingIdentity,LFloat) or
        (LFloat.FloatingFormat = pffUnknown)) then
        Exit(Answer(pbsUnresolved,pbiCompilerContext,LPrimitive));
      LLiteral.FDeduction := LTypeAnswer.BoundType;

      if LLiteral.TypeKind <> pstArray then
      begin
        LLiteral.TypeKind := pstBuiltin;
        LLiteral.Intrinsic := LTypeAnswer.BoundType.Builtin;
        Supports(LTypeAnswer.BoundType, INyxPascalIntrinsicIdentity, LIdentity);
        LLiteral.IntrinsicScope := LIdentity.IntrinsicOrigin;
        LLiteral.IntrinsicName := LIdentity.IntrinsicReference;
        LLiteral.IntrinsicFacts := LIdentity.IntrinsicContext;
      end;
    end;
    Result := TypeAnswer(LLease);
  end;

  function WantsInference(const AFound: INyxPascalResolution): Boolean;
  var
    LContext: INyxPascalCompilerContext;
    LSymbol: INyxPascalBoundSymbol;
    LEnvironment: INyxPascalGenericEnvironment;
    LIndex: Integer;
  begin
    Result := False;

    if not (AFound.Status in [pbsBound, pbsOverloads]) then Exit;
    LContext := ContextAt(ASite);

    if (LContext = nil) or
      (LContext.Feature(ppfImplicitFunctionSpecialization) <> ppkTrue) then Exit;
    for LIndex := 0 to AFound.Count - 1 do
    begin
      Tick;
      LSymbol := AFound.SymbolAt(LIndex);

      if (LSymbol.Declaration.Kind <> psdkRoutine) or
        (LSymbol.Declaration.GenericSpan.Length = 0) then Continue;
      LEnvironment := EnvironmentOf(LSymbol);

      if (LEnvironment = nil) or not SameSymbol(LEnvironment.Owner, LSymbol) then Exit(True);
    end;
  end;

  { A closed literal expression has unambiguous argument boundaries without
    knowing its compiler type. This syntax-only path admits no general names,
    calls, generic angles or conversions. Boolean spellings keep ordinary
    lookup ownership. Standard factor/term/simple/relation
    boundaries preserve groups and missing operands; no operator is evaluated
    and no result type or compiler-call eligibility is inferred. }
  function ClosedArgumentSyntax: Boolean;

    function Factor: Boolean;
    var
      LPrevious: TNyxPascalSyntaxOrigin;
      LName: TNyxText;
    begin
      Enter;
      try
        Result := False;

        if not LAvailable or not LToken.Complete then Exit;

        if IsSymbol(psoPlus) or IsSymbol(psoMinus) or
          ((LToken.Kind = ptkWord) and not LToken.Escaped and (LToken.Word = pkwNot)) then
        begin
          Next;
          Exit(Factor);
        end;

        if IsSymbol(psoOpenParen) then
        begin
          Next;

          if not ClosedArgumentSyntax or not IsSymbol(psoCloseParen) then Exit;
          Next;
          Exit(True);
        end;

        if LToken.Kind = ptkNumber then
        begin
          Next;
          Exit(True);
        end;

        if (LToken.Kind = ptkWord) and not LToken.Escaped then
        begin

          if LToken.Word = pkwNil then
          begin
            Next;
            Exit(True);
          end;

          if LToken.Span.Length in [4, 5] then
          begin
            LName := NyxPascalASCIIKey(LLex.Identifier(LAt));

            if (LName = 'true') or (LName = 'false') then
            begin
              { This is only an atom boundary. A declaration shadowing these
                spellings keeps its original type answer in deduction. }
              Next;
              Exit(True);
            end;
          end;
        end;

        if not (LToken.Kind in [ptkString, ptkCharacter]) then Exit;
        repeat

          if not LToken.Complete then Exit;
          LPrevious := FWorld.ModuleAt(ASite.ModuleIndex).Origin(
            FWorld.ModuleAt(ASite.ModuleIndex).OriginAt(LToken.Span.Start));
          Next;
        until not AdjacentFragments(LPrevious);
        Result := True;
      finally
        Leave;
      end;
    end;

    function Term: Boolean;
    begin
      Result := False;

      if not Factor then Exit;
      while IsSymbol(psoStar) or IsSymbol(psoSlash) or
        (LAvailable and (LToken.Kind = ptkWord) and not LToken.Escaped and
          (LToken.Word in [pkwAnd, pkwDiv, pkwMod, pkwShl, pkwShr])) do
      begin
        Next;

        if not Factor then Exit;
      end;
      Result := True;
    end;

    function Simple: Boolean;
    begin
      Result := False;

      if not Term then Exit;
      while IsSymbol(psoPlus) or IsSymbol(psoMinus) or
        (LAvailable and (LToken.Kind = ptkWord) and not LToken.Escaped and
          (LToken.Word in [pkwOr, pkwXor])) do
      begin
        Next;

        if not Term then Exit;
      end;
      Result := True;
    end;

  begin
    Result := False;

    if not Simple then Exit;

    if IsSymbol(psoEqual) or IsSymbol(psoNotEqual) or IsSymbol(psoLess) or
      IsSymbol(psoGreater) or IsSymbol(psoLessEqual) or IsSymbol(psoGreaterEqual) then
    begin
      Next;

      if not Simple then Exit;
    end;
    Result := True;
  end;

  { Postfix owns nested calls and their explicit generic commas. An unavailable
    closed argument retains its original type refusal alongside complete syntax.
    The routine's formal correspondence decides whether that type is required;
    result typing never selects overload eligibility or executes an argument. }
  function InferredCall(const AFound: INyxPascalResolution): INyxPascalResolution;
  var
    LArguments: TInferredArgumentAnswers;
    LValue: INyxPascalResolution;
    LArgumentAccess: TNyxPascalMemberAccess;
    LCount: Integer;
    LSavedAt: Integer;
    LHasLiteral: Boolean;
    LString: INyxPascalStringConstant;

    { Generic unnamed array types are normalized after the complete call has
      been consumed. FPC's current H/Unicode switches at that stage can differ
      from the earlier argument-node settings; typed string nodes stay fixed. }
    function CompletedArguments: INyxPascalResolution;
    var LAt: Integer; LDefault: INyxPascalResolution; LType: INyxPascalSemanticType;
      LLiteral: INyxPascalLiteralType; LExpression: INyxPascalConstantExpression;
      LProof: INyxPascalStringConstant;
    begin
      LDefault := nil;

      if LHasLiteral then
        for LAt := 0 to High(LArguments) do
        begin
          Tick;

          if (LArguments[LAt].Status <> pbsBound) or
            not Supports(LArguments[LAt].BoundType,INyxPascalStringConstant,LProof) or
            (LProof.Form <> pscfLiteral) or LProof.Wide then Continue;

          if LDefault = nil then LDefault := DefaultStringResult;

          if LDefault.Status <> pbsBound then Exit(LDefault);
          LType := LArguments[LAt].BoundType;

          if Supports(LType,INyxPascalLiteralType,LLiteral) then LType := LLiteral.SpecializationType;

          if Supports(LType,INyxPascalConstantExpression,LExpression) then LType := LExpression.ResultType;

          if not SameType(LType,LDefault.BoundType) then
            LArguments[LAt] := StringAnswer(LDefault.BoundType,LProof);
        end;
      Result := InferRoutineArguments(ASite,AFound,LArguments);
    end;

  begin
    SetLength(LArguments, 0);
    LHasLiteral := False;
    Next;

    if IsSymbol(psoCloseParen) then
    begin
      Next;
      Exit(InferRoutineArguments(ASite, AFound, LArguments));
    end;
    while LAvailable do
    begin
      Tick;
      LSavedAt := LAt;
      LValue := Expression(LArgumentAccess);

      if (LValue.Status <> pbsBound) or not (IsSymbol(psoComma) or IsSymbol(psoCloseParen)) then
      begin

        if LValue.Status = pbsBound then LValue := Answer(pbsUnresolved, pbiUnsupportedType);
        LAt := LSavedAt;
        ReadToken;

        if not ClosedArgumentSyntax or not (IsSymbol(psoComma) or IsSymbol(psoCloseParen)) then Exit(LValue);
      end;

      if (LValue.Status = pbsBound) and (LArgumentAccess <> pmaInstance) then
        Exit(Answer(pbsUnresolved, pbiGenericContext));
      LCount := Length(LArguments);

      if LCount >= FLimits.MaximumCandidates then
        raise ENyxPascalBindingLimit.Create('Inferred call arguments exceed their query budget');
      SetLength(LArguments, LCount + 1);
      LArguments[LCount] := LValue;

      if (LValue.Status = pbsBound) and
        Supports(LValue.BoundType,INyxPascalStringConstant,LString) and
        (LString.Form = pscfLiteral) and not LString.Wide then LHasLiteral := True;

      if IsSymbol(psoCloseParen) then
      begin
        Next;
        Exit(CompletedArguments);
      end;

      if not IsSymbol(psoComma) then Exit(Answer(pbsUnresolved, pbiUnsupportedType));
      Next;
    end;
    Result := Answer(pbsUnresolved, pbiUnsupportedType);
  end;

  { A data-address expression describes storage; it never obtains a machine
    address. Dotted storage names retain each source receiver's role, so a
    property or record-valued function does not acquire storage merely because
    its final selector names a field. Routine/index/temporary address rules
    remain separate and unsupported until their own contracts are qualified. }
  function AddressedName(const AName: TNyxText; AOffset: Integer): INyxPascalResolution;
  var
    LContext: INyxPascalCompilerContext;
    LSite: INyxPascalBindingSite;
    LFound, LValue: INyxPascalResolution;
    LPrefix: TNyxText;
    LAt, LParts: Integer;
    LStorage, LHeap, LSeen: Boolean;
    LAccess: TNyxPascalMemberAccess;
    LType: INyxPascalSemanticType;
    LPointer: TDataPointerType;
  begin
    LSite := FWorld.At(ASite.ModuleIndex, ASite.ScopeIndex, AOffset);
    LContext := ContextAt(LSite);

    if (LContext = nil) or (LContext.Family <> ppcFPC) or
      (LContext.Version.FullNumber <> 30301) or not (LContext.Mode in [ppmDelphi, ppmDelphiUnicode]) or
      (LContext.Option('T') = ppkUnknown) then Exit(Answer(pbsUnresolved, pbiCompilerContext));
    LFound := Lookup(ASite, AName);

    if (LFound.Status <> pbsBound) or (LFound.Count <> 1) then Exit(LFound);
    LStorage := False;
    LHeap := False;
    LSeen := False;
    LParts := 0;
    for LAt := 1 to Length(AName) + 1 do
    begin
      Tick;

      if (LAt <= Length(AName)) and (AName[LAt] <> '.') then Continue;
      Inc(LParts);

      if LParts > FLimits.MaximumDepth then
        raise ENyxPascalBindingLimit.Create('Address selectors exceed their source-depth budget');
      LPrefix := Copy(AName, 1, LAt - 1);
      LValue := Lookup(ASite, LPrefix);

      if LValue.Status = pbsMissing then Continue;

      if (LValue.Status <> pbsBound) or (LValue.Count <> 1) then Exit(LValue);
      case LValue.SymbolAt(0).Declaration.Kind of
        psdkVariable, psdkParameter: LStorage := True;
        psdkField: LStorage := (not LSeen) or LStorage or LHeap or
          (psdfClassMember in LValue.SymbolAt(0).Declaration.Flags);
      else
        LStorage := False;
      end;
      LSeen := True;
      LValue := ValueOf(LValue, False, LAccess);

      if LValue.Status <> pbsBound then Exit(LValue);
      LType := Unwrap(LValue.BoundType);
      LHeap := (LAccess = pmaInstance) and (LType <> nil) and (LType.Kind = pstDeclared) and
        (LType.Definition <> nil) and (LType.Definition.Declaration.TypeForm = pstfClass);
    end;

    if not LSeen or not LStorage then Exit(Answer(pbsUnresolved, pbiUnsupportedType));
    LValue := ValueOf(LFound, False, LAccess);

    if LValue.Status <> pbsBound then Exit(LValue);

    if LAccess <> pmaInstance then Exit(Answer(pbsUnresolved, pbiWrongMemberKind));

    if LContext.Option('T') = ppkFalse then
    begin

      if (FLimits.ImplicitScopes = pispCompilerUnits) and (FWorld.FindModule('System') < 0) then
        Exit(Answer(pbsUnresolved, pbiMissingModule, 'System'));
      Exit(BuiltinType(LSite, 'Pointer', FLimits.ImplicitScopes = pispCompilerUnits));
    end;
    LPointer := TDataPointerType.Create;
    LPointer.Owner := FWorld;
    LPointer.CapturedContext := LContext;
    LPointer.InitialTargetContext := InitialPointerContext(ASite.ModuleIndex);
    LPointer.TypeKind := pstPointer;
    LPointer.Element := LValue.BoundType;
    Result := TypeAnswer(LPointer);
  end;

  { Qualified numeric casts evaluate one ordinary expression argument. Other
    target families retain their existing opaque argument contract. Constant
    conversion preserves the actual target identity; runtime values stay opaque. }
  function RealCast(const ATarget: INyxPascalSemanticType; AOffset: Integer;
    out AHandled: Boolean): INyxPascalResolution;
  var LLeaf, LSourceType: INyxPascalSemanticType; LFloat: INyxPascalFloatingIdentity;
    LLiteral: INyxPascalLiteralType; LScalar: INyxPascalConstantExpression;
    LReal: INyxPascalIEEEConstant; LArgument, LAdmission: INyxPascalResolution;
    LContext: INyxPascalCompilerContext; LEvaluation: TNyxPascalRealEvaluationPolicy;
    LPrecision: TNyxPascalBinaryPrecision; LAccess: TNyxPascalMemberAccess;
    LValue, LConverted: TNyxPascalIEEEValue; LInteger: TNyxPascalConstantValue;
    LBudget: INyxPascalAnalysisBudget; LFPU: TNyxPascalFPUReference;
  begin
    AHandled := False; Result := nil; LLeaf := ATarget;
    while (LLeaf <> nil) and (LLeaf.Kind in [pstAlias,pstDistinct]) do
    begin Tick; LLeaf := LLeaf.Target; end;

    if (LLeaf = nil) or (LLeaf.Kind <> pstBuiltin) or
      not (LLeaf.Builtin in [pbbSingle,pbbDouble,pbbExtended]) then Exit;
    LContext := ContextAt(FWorld.At(ASite.ModuleIndex,ASite.ScopeIndex,AOffset));

    if (LContext = nil) or (LContext.Family <> ppcFPC) or
      (LContext.Version.FullNumber <> 30301) or not (LContext.Mode in [ppmDelphi,ppmDelphiUnicode]) then Exit;
    AHandled := True; Result := Answer(pbsUnresolved,pbiCompilerContext,'real cast target');

    if not Supports(LLeaf,INyxPascalFloatingIdentity,LFloat) then Exit;
    LPrecision := pbpUnknown;
    case LFloat.FloatingFormat of
      pffBinary32: LPrecision := pbpBinary32;
      pffBinary64: LPrecision := pbpBinary64;
      pffX87Extended80: LPrecision := pbpX87Extended80;
    else Exit;
    end;
    Next; LArgument := Expression(LAccess);

    if not IsSymbol(psoCloseParen) then Exit(Answer(pbsUnresolved,pbiUnsupportedType,'real cast argument'));
    Next;

    if LArgument.Status <> pbsBound then Exit(LArgument);

    if LAccess <> pmaInstance then Exit(Answer(pbsUnresolved,pbiWrongMemberKind,'real cast argument'));
    LInteger := TNyxPascalConstantValue.Unknown; LValue := TNyxPascalIEEEValue.Unknown;
    LSourceType := LArgument.BoundType;

    if Supports(LSourceType,INyxPascalLiteralType,LLiteral) then
    begin LInteger := LLiteral.Value; LSourceType := LLiteral.SpecializationType; end
    else if Supports(LSourceType,INyxPascalConstantExpression,LScalar) then
    begin LInteger := LScalar.ConstantValue; LSourceType := LScalar.ResultType; end;

    if Supports(LArgument.BoundType,INyxPascalIEEEConstant,LReal) then LValue := LReal.IEEEValue;
    while (LSourceType <> nil) and (LSourceType.Kind in [pstAlias,pstDistinct]) do
    begin Tick; LSourceType := LSourceType.Target; end;

    if (LSourceType = nil) or (LSourceType.Kind <> pstBuiltin) or
      not (LSourceType.Builtin in [pbbSingle,pbbDouble,pbbExtended,pbbShortInt,pbbByte,
        pbbSmallInt,pbbWord,pbbLongInt,pbbLongWord,pbbInt64,pbbQWord,pbbInteger,pbbNativeInt,pbbNativeUInt]) then
      Exit(Answer(pbsUnresolved,pbiUnsupportedType,'real cast operand'));

    if (LValue.Kind = pifUnknown) and (LInteger.Kind <> pvvInteger) then Exit(TypeAnswer(ATarget));
    LFPU := NyxPascalSelectedFPU(LContext);

    if not ((LFPU.Name = 'x87') or (LFPU.Name = 'sse') or (LFPU.Name = 'sse2') or
      (LFPU.Name = 'sse3') or (LFPU.Name = 'ssse3') or (LFPU.Name = 'sse41') or
      (LFPU.Name = 'sse42') or (LFPU.Name = 'avx') or (LFPU.Name = 'fma') or
      (LFPU.Name = 'avx2') or (LFPU.Name = 'avx512f')) then
      Exit(Answer(pbsUnresolved,pbiCompilerContext,'FPUTYPE'));
    LEvaluation := NyxPascalRealEvaluation(LContext);

    if (LValue.Kind = pifUnknown) and ((LEvaluation.Kind <> prekNearestEven) or
      not (LEvaluation.Precision in [pbpBinary64,pbpX87Extended80])) then
      Exit(Answer(pbsUnresolved,pbiCompilerContext,'REALEVALUATION'));

    if FSteps >= FLimits.MaximumSteps then
      raise ENyxPascalBindingLimit.Create('Pascal semantic steps exceed their query budget');
    LBudget := NewNyxPascalAnalysisBudget(FLimits.MaximumSteps-FSteps,FCancel);
    try
      try

        if LValue.Kind = pifUnknown then
          LValue := TNyxPascalIEEEValue.Finite(
            NyxPascalRoundIntegerToRealEven(LInteger,LEvaluation.Precision,LBudget));

        if LValue.Kind = pifFinite then
          LConverted := MathematicalOutcome(LValue.FiniteValue.RoundToEven(LPrecision,LBudget))
        else LConverted := LValue.RoundToEven(LPrecision,LBudget);

        if LConverted.Kind = pifUnknown then
          Exit(Answer(pbsUnresolved,pbiUnsupportedType,'real cast range'));
      except on ENyxPascalValue do Exit(Answer(pbsUnresolved,pbiUnsupportedType,'real cast value')); end;
    finally Inc(FSteps,LBudget.Receipt.Total); LBudget.Retire; end;
    LAdmission := SpecialValueAdmission(LConverted);

    if LAdmission <> nil then Exit(LAdmission);
    Result := ConstantAnswer(ATarget,TNyxPascalConstantValue.Unknown,LConverted);
  end;

  function Postfix(out AValueAccess: TNyxPascalMemberAccess): INyxPascalResolution;
  var
    LFound, LValue: INyxPascalResolution;
    LType: INyxPascalSemanticType;
    LName: TNyxText;
    LCount: Integer;
    LDimensions: Integer;
    LDimension: Integer;
    LInnerAccess: TNyxPascalMemberAccess;
    LTypeStart: Integer;
    LTypeCallee: Boolean;
    LCastHandled: Boolean;
    LIsLiteral: Boolean;
    LFormal: INyxPascalFormalParameter;

    { Only an actual generic routine declaration can own explicit type
      arguments. A nongeneric parameterless function followed by '<' supplies
      its ordinary result to the relational expression instead. }
    function GenericRoutine: Boolean;
    var
      LIndex: Integer;
    begin
      Result := False;
      for LIndex := 0 to LFound.Count - 1 do
      begin
        Tick;

        if (LFound.SymbolAt(LIndex).Declaration.Kind = psdkRoutine) and
          (LFound.SymbolAt(LIndex).Declaration.GenericSpan.Length > 0) then Exit(True);
      end;
    end;

  begin
    Enter;
    try
      AValueAccess := pmaInstance;
      LName := '';
      LFound := LiteralPrimary(LIsLiteral);

      if LIsLiteral then
      begin

        if LFound.Status <> pbsBound then Exit(LFound);
      end
      else if IsSymbol(psoAt) then
      begin
        LTypeStart := LToken.Span.Start;
        Next;

        if not LAvailable or (LToken.Kind <> ptkWord) then Exit(Answer(pbsUnresolved, pbiUnsupportedType));
        LName := LLex.Identifier(LAt);
        Next;
        while IsSymbol(psoDot) do
        begin
          Next;

          if not LAvailable or (LToken.Kind <> ptkWord) then Exit(Answer(pbsUnresolved, pbiUnsupportedType));
          LName := LName + '.' + LLex.Identifier(LAt);
          Next;
        end;
        LFound := AddressedName(LName, LTypeStart);
      end
      else if IsSymbol(psoOpenParen) then
      begin
        Next;
        LFound := Expression(LInnerAccess);

        if not IsSymbol(psoCloseParen) then Exit(Answer(pbsUnresolved, pbiUnsupportedType));
        Next;
        AValueAccess := LInnerAccess;
      end
      else if LAvailable and (LToken.Kind = ptkWord) then
      begin
        LTypeStart := LToken.Span.Start;
        LName := LLex.Identifier(LAt);
        Next;
        while IsSymbol(psoDot) do
        begin
          Next;

          if not LAvailable or (LToken.Kind <> ptkWord) then Exit(Answer(pbsUnresolved, pbiUnsupportedType));
          LName := LName + '.' + LLex.Identifier(LAt);
          Next;
        end;
        LFound := Lookup(ASite, LName);

        if IsSymbol(psoLess) and (LFound.Count = 1) and
          (LFound.SymbolAt(0).Declaration.Kind = psdkType) then
        begin
          LAt := LLex.TokenAt(LTypeStart);
          ReadToken;
          LFound := ReadTypeTarget(True);
          AValueAccess := pmaType;
        end;
      end
      else Exit(Answer(pbsUnresolved, pbiUnsupportedType));
      while LAvailable do
      begin

        if IsSymbol(psoLess) and GenericRoutine then
        begin
          LFound := ReadRoutineArguments(LFound, LTypeStart);
          AValueAccess := pmaInstance;
        end
        else if IsSymbol(psoOpenParen) then
        begin
          LTypeCallee := ((LFound.Count = 1) and (LFound.SymbolAt(0).Declaration.Kind = psdkType)) or
            ((LFound.BoundType <> nil) and (AValueAccess = pmaType));

          if not LTypeCallee and (LFound.Count = 0) and (LFound.BoundType <> nil) then
          begin
            LTypeCallee := Supports(LFound.BoundType, INyxPascalFormalParameter, LFormal) and
              (NyxPascalASCIIKey(LFormal.Name) = NyxPascalASCIIKey(LName));

            if not LTypeCallee and (LFound.BoundType.Kind = pstBuiltin) then
            begin
              LValue := BuiltinType(ASite, LName);
              LTypeCallee := LValue.Status = pbsBound;
            end;
          end;

          if LTypeCallee then
          begin

            if not CastContext then Exit(Answer(pbsUnresolved, pbiCompilerContext));

            LValue := ValueOf(LFound, False, LInnerAccess);

            if LValue.Status <> pbsBound then Exit(LValue);
            LFound := RealCast(LValue.BoundType,LTypeStart,LCastHandled);

            if not LCastHandled then
            begin

              if Arguments <> 1 then Exit(Answer(pbsUnresolved, pbiUnsupportedType));
              LFound := LValue;
            end;
            AValueAccess := pmaInstance;
            Continue;
          end;

          if (LFound.BoundType <> nil) and (AValueAccess = pmaType) then
            Exit(Answer(pbsUnresolved, pbiUnsupportedType));

          if WantsInference(LFound) then
          begin
            LFound := InferredCall(LFound);

            if not (LFound.Status in [pbsBound, pbsOverloads]) then Exit(LFound);
          end
          else if Arguments < 0 then Exit(Answer(pbsUnresolved, pbiUnsupportedType));
          LFound := ValueOf(LFound, True, AValueAccess);
        end
        else if IsSymbol(psoOpenBracket) then
        begin

          if (LFound.BoundType <> nil) and (AValueAccess = pmaType) then
            Exit(Answer(pbsUnresolved, pbiUnsupportedType));
          LCount := Arguments;

          if LCount < 1 then Exit(Answer(pbsUnresolved, pbiUnsupportedType));

          if (LFound.Count = 1) and (LFound.SymbolAt(0).Declaration.Kind = psdkProperty) and
            (LFound.SymbolAt(0).Declaration.PropertyParameters.Length > 0) then
          begin
            LFound := IndexedProperty(LFound.SymbolAt(0), LCount);
            AValueAccess := pmaInstance;
            Continue;
          end;
          LValue := ValueOf(LFound, False, LInnerAccess);

          if LValue.Status <> pbsBound then Exit(LValue);
          LType := Unwrap(LValue.BoundType);

          if (LType <> nil) and (LType.Kind in [pstDeclared, pstClassOf]) then
          begin
            LValue := DefaultProperty(LType, ASite, LInnerAccess);

            if LValue.Status = pbsMissing then Exit(Answer(pbsUnresolved, pbiUnsupportedType));

            if (LValue.Status <> pbsBound) or (LValue.Count <> 1) then Exit(LValue);
            LFound := IndexedProperty(LValue.SymbolAt(0), LCount);
            AValueAccess := pmaInstance;
            Continue;
          end;
          while LCount > 0 do
          begin
            Tick;

            if (LType = nil) or (LType.Kind <> pstArray) then Exit(Answer(pbsUnresolved, pbiUnsupportedType));
            LDimensions := 0;

            if LType.Syntax <> nil then
            begin
              LDimension := LType.Syntax.Child(LType.SyntaxNode, ptrDimension);
              while LDimension >= 0 do
              begin
                Tick;

                if LType.Syntax.Node(LDimension).Role = ptrDimension then Inc(LDimensions);
                LDimension := LType.Syntax.Node(LDimension).NextSibling;
              end;
            end;

            if LDimensions = 0 then LDimensions := 1;

            if LCount < LDimensions then Exit(Answer(pbsUnresolved, pbiUnsupportedType));
            Dec(LCount, LDimensions);
            LType := Unwrap(LType.Target);
          end;
          LFound := TypeAnswer(LType);
          AValueAccess := pmaInstance;
        end
        else if IsSymbol(psoCaret) then
        begin

          if (LFound.BoundType <> nil) and (AValueAccess = pmaType) then
            Exit(Answer(pbsUnresolved, pbiUnsupportedType));
          LValue := ValueOf(LFound, False, LInnerAccess);

          if LValue.Status <> pbsBound then Exit(LValue);
          LType := Unwrap(LValue.BoundType);

          if (LType = nil) or (LType.Kind <> pstPointer) or (LType.Target = nil) then
            Exit(Answer(pbsUnresolved, pbiUnsupportedType));
          LFound := TypeAnswer(LType.Target);
          AValueAccess := pmaInstance;
          Next;
        end
        else if LAvailable and (LToken.Kind = ptkWord) and not LToken.Escaped and (LToken.Word = pkwAs) then
        begin
          LValue := ValueOf(LFound, False, LInnerAccess);

          if LValue.Status <> pbsBound then Exit(LValue);

          if LFound.BoundType <> nil then LInnerAccess := AValueAccess;
          LType := Unwrap(LValue.BoundType);

          if (LInnerAccess = pmaType) or (LType = nil) or (LType.Kind <> pstDeclared) or
            (LType.Definition = nil) or
            not (LType.Definition.Declaration.TypeForm in [pstfClass, pstfInterface]) then
            Exit(Answer(pbsUnresolved, pbiUnsupportedType));

          if not CastContext then Exit(Answer(pbsUnresolved, pbiCompilerContext));
          Next;
          LFound := ReadTypeTarget;

          if LFound.Status <> pbsBound then Exit(LFound);
          LType := Unwrap(LFound.BoundType);

          if (LType = nil) or (LType.Kind <> pstDeclared) or (LType.Definition = nil) or
            not (LType.Definition.Declaration.TypeForm in [pstfClass, pstfInterface]) then
            Exit(Answer(pbsUnresolved, pbiUnsupportedType));
          AValueAccess := pmaInstance;
        end
        else if IsSymbol(psoDot) then
        begin
          LValue := ValueOf(LFound, False, LInnerAccess);

          { Grouping retains the receiver's type/instance role even when its
            inner expression has already been reduced to a type-only answer. }

          if LFound.BoundType <> nil then LInnerAccess := AValueAccess;

          if LValue.Status <> pbsBound then Exit(LValue);
          Next;

          if not LAvailable or (LToken.Kind <> ptkWord) then Exit(Answer(pbsUnresolved, pbiUnsupportedType));
          LTypeStart := LToken.Span.Start;
          LFound := Member(LValue.BoundType, ASite, LLex.Identifier(LAt), LInnerAccess);
          Next;
        end
        else Break;

        if not (LFound.Status in [pbsBound, pbsOverloads]) then Exit(LFound);
      end;
      Result := ValueOf(LFound, False, LInnerAccess);

      if (Result.Status = pbsBound) and (LFound.BoundType = nil) then AValueAccess := LInnerAccess;
    finally
      Leave;
    end;
  end;

  { Closed language spellings map into the existing typed operator family.
    Conversion, lifecycle and unqualified extension operators are not inferred. }
  function CurrentOperator: TNyxPascalOperatorKind;
  begin
    Result := pokNone;

    if not LAvailable then Exit;

    if LToken.Kind = ptkSymbol then
      case LToken.Symbol of
        psoPlus: Result := pokAdd;
        psoMinus: Result := pokSubtract;
        psoStar: Result := pokMultiply;
        psoSlash: Result := pokDivide;
        psoEqual: Result := pokEqual;
        psoNotEqual: Result := pokNotEqual;
        psoLess: Result := pokLess;
        psoLessEqual: Result := pokLessEqual;
        psoGreater: Result := pokGreater;
        psoGreaterEqual: Result := pokGreaterEqual;
      else
        Result := pokNone;
      end
    else if (LToken.Kind = ptkWord) and not LToken.Escaped then
      case LToken.Word of
        pkwDiv: Result := pokIntDivide;
        pkwMod: Result := pokModulus;
        pkwAnd: Result := pokAnd;
        pkwOr: Result := pokOr;
        pkwXor: Result := pokXor;
        pkwShl: Result := pokShiftLeft;
        pkwShr: Result := pokShiftRight;
        pkwNot: Result := pokNot;
      else
        Result := pokNone;
      end;
  end;

  { Operator lookup preserves both operands' visible declarations and ordinary
    privacy/generic/source gaps. Ordinary signatures retain exact matching; the
    separately qualified data-pointer stage adds compiler equality, filtering
    and conversion ranks. Other conversions, ordinal promotion and commutative
    permutation remain separate. }
  function OperatorResult(AKind: TNyxPascalOperatorKind; AOffset: Integer;
    const ALeft, ARight: INyxPascalResolution; AUnary: Boolean): INyxPascalResolution;
  var
    LContext: INyxPascalCompilerContext;
    LLeft, LRight, LCommon, LBoolean: INyxPascalSemanticType;
    LSymbols: array of INyxPascalBoundSymbol;
    LFound, LSignature: INyxPascalResolution;
    LLiteral: INyxPascalLiteralType;
    LNumeric: INumericExpressionValue;
    LCandidate, LOperand, LArity: Integer;
    LRealLeft, LRealRight: INyxPascalIEEEConstant;
    LExact, LMatch, LSeen: Boolean;

    function OrdinaryType(const AValue: INyxPascalSemanticType): INyxPascalSemanticType;
    begin
      Result := AValue;

      if Supports(Result, INyxPascalLiteralType, LLiteral) then
        Result := LLiteral.SpecializationType;

      if Supports(Result, INumericExpressionValue, LNumeric) then
        Result := LNumeric.ResultType;
      while (Result <> nil) and (Result.Kind = pstAlias) do
      begin
        Tick;
        Result := Result.Target;
      end;
    end;

    { Type checking shares immediate FPU selection across an unparenthesized
      expression. Parentheses/call arguments have their own Expression frame.
      Only captured lexical lookahead is inspected, never later declarations.
      One cached boundary per frame avoids rescanning a long division chain. }
    function EvaluationFPU: INyxPascalCompilerContext;
    var
      LIndex, LRound, LSquare: Integer;
      LToken: TNyxPascalToken;
      LBoundary: Boolean;
    begin

      if LFPUChecked then Exit(LFPUContext);
      LFPUChecked := True;
      LRound := 0; LSquare := 0;
      LIndex := LLex.TokenAt(AOffset);
      while LIndex < LLex.Count do
      begin
        Tick;
        LToken := LLex.Token(LIndex);
        LBoundary := False;

        if (LRound = 0) and (LSquare = 0) then
          LBoundary := ((LToken.Kind = ptkSymbol) and
            (LToken.Symbol in [psoCloseParen,psoCloseBracket,psoComma,psoSemicolon])) or
            ((LToken.Kind = ptkWord) and not LToken.Escaped and
            (LToken.Word in [pkwEnd,pkwThen,pkwDo,pkwElse,pkwTo,pkwDownto,pkwOf,pkwUntil]));

        if LBoundary then
        begin
          LFPUContext := ContextAt(FWorld.At(ASite.ModuleIndex,ASite.ScopeIndex,LToken.Span.Start));
          Break;
        end;

        if LToken.Kind = ptkSymbol then
          case LToken.Symbol of
            psoOpenParen: Inc(LRound);
            psoCloseParen: Dec(LRound);
            psoOpenBracket: Inc(LSquare);
            psoCloseBracket: Dec(LSquare);
          else begin end;
          end;
        Inc(LIndex);
      end;
      Result := LFPUContext;
    end;

    { Qualified primitive representations grant arithmetic families, not
      blanket conversions. The actual selected operand object preserves Real
      and other distinct identities when FPC retains that operand definition. }
    function FloatingResult(AOperandType: Boolean = False): INyxPascalResolution;
    var
      LLeftLeaf, LRightLeaf, LChosen: INyxPascalSemanticType;
      LLeftFormat, LRightFormat: TNyxPascalFloatingFormat;
      LPrecision: TNyxPascalKnowledge;
      LSelection: TNyxPascalFPUReference;
      LSelectedContext: INyxPascalCompilerContext;
      LLeftInteger, LRightInteger: Boolean;

      function FloatingLeaf(const AType: INyxPascalSemanticType;
        out AFormat: TNyxPascalFloatingFormat): INyxPascalSemanticType;
      var LDepth: Integer; LInfo: INyxPascalFloatingIdentity;
      begin
        Result := AType; AFormat := pffUnknown; LDepth := 0;
        while (Result <> nil) and (Result.Kind in [pstAlias,pstDistinct]) do
        begin
          Tick; Inc(LDepth);

          if LDepth > FLimits.MaximumDepth then
            raise ENyxPascalBindingLimit.Create('Floating representation exceeds its alias depth budget');
          Result := Result.Target;
        end;

        if (Result = nil) or (Result.Kind <> pstBuiltin) or
          not (Result.Builtin in [pbbSingle,pbbDouble,pbbExtended,pbbCurrency,pbbComp]) then Exit(nil);

        if Supports(Result,INyxPascalFloatingIdentity,LInfo) then AFormat := LInfo.FloatingFormat;
      end;

      function IntegerOperand(const AType: INyxPascalSemanticType): Boolean;
      var LInfo: INyxPascalIntrinsicIdentity; LRootContext: INyxPascalCompilerContext;
      begin
        Result := (AType <> nil) and (AType.Kind = pstBuiltin) and
          (AType.Builtin in [pbbInteger,pbbByte,pbbWord,pbbSmallInt,pbbShortInt,
            pbbLongInt,pbbLongWord,pbbInt64,pbbQWord,pbbNativeInt,pbbNativeUInt]) and
          Supports(AType,INyxPascalIntrinsicIdentity,LInfo);

        if not Result then Exit;
        Result := LInfo.IntrinsicOrigin = pioSystemBootstrap;

        if not Result then Exit;
        Tick;
        LRootContext := LInfo.IntrinsicContext;
        Result := (LRootContext.Family = ppcFPC) and
          (LRootContext.Version.FullNumber = 30301) and
          (LRootContext.Symbol(NyxPascalDefine('CPUI386')).IsDefined = ppkTrue);
      end;

      function Canonical(AKind: TNyxPascalBuiltin): INyxPascalResolution;
      var LName: TNyxText;
      begin
        case AKind of
          pbbSingle: LName := 'single'; pbbDouble: LName := 'double';
          pbbExtended: LName := 'extended'; pbbCurrency: LName := 'currency';
          pbbComp: LName := 'comp'; pbbBoolean: LName := 'boolean';
        else raise ENyxPascalBinding.Create('A floating result needs its closed primitive kind');
        end;
        Result := BuiltinType(FWorld.At(ASite.ModuleIndex,ASite.ScopeIndex,AOffset),LName,True);
      end;

    begin
      Result := nil;

      if not (AKind in [pokAdd,pokSubtract,pokMultiply,pokDivide,pokEqual,pokNotEqual,
        pokLess,pokLessEqual,pokGreater,pokGreaterEqual]) then Exit;
      LLeftLeaf := FloatingLeaf(LLeft,LLeftFormat);
      LRightLeaf := FloatingLeaf(LRight,LRightFormat);

      if (LLeftLeaf = nil) and (LRightLeaf = nil) and
        (AUnary or (AKind <> pokDivide)) then Exit;
      LLeftInteger := (LLeftLeaf = nil) and IntegerOperand(LLeft);
      LRightInteger := (LRightLeaf = nil) and IntegerOperand(LRight);

      if AUnary then
      begin

        if (LLeftLeaf = nil) or not (AKind in [pokAdd,pokSubtract]) then Exit;
      end
      else
      begin

        if not ((LLeftLeaf <> nil) or LLeftInteger) or
          not ((LRightLeaf <> nil) or LRightInteger) then Exit;

        if (LLeftLeaf = nil) and (LRightLeaf = nil) and (AKind <> pokDivide) then Exit;
      end;

      if ((LLeftLeaf <> nil) and (LLeftFormat = pffUnknown)) or
        ((LRightLeaf <> nil) and (LRightFormat = pffUnknown)) then
        Exit(Answer(pbsUnresolved,pbiCompilerContext));

      if AKind = pokDivide then
      begin
        LSelectedContext := EvaluationFPU;
        LSelection := NyxPascalSelectedFPU(LSelectedContext);

        if not ((LSelection.Name = 'x87') or (LSelection.Name = 'sse') or
          (LSelection.Name = 'sse2') or (LSelection.Name = 'sse3') or
          (LSelection.Name = 'ssse3') or (LSelection.Name = 'sse41') or
          (LSelection.Name = 'sse42') or (LSelection.Name = 'avx') or
          (LSelection.Name = 'fma') or (LSelection.Name = 'avx2') or
          (LSelection.Name = 'avx512f')) then
          Exit(Answer(pbsUnresolved,pbiCompilerContext,'FPUTYPE'));

        if (LLeftLeaf = nil) and (LRightLeaf = nil) then
        begin

          if LSelection.Name = 'x87' then Exit(Canonical(pbbExtended));
          Exit(Canonical(pbbDouble));
        end;
      end;

      if AUnary then
      begin

        if (AKind = pokAdd) or (LLeftFormat in [pffCurrency64,pffComp64]) then Exit(TypeAnswer(LLeft));
        LPrecision := EffectiveNodeSwitch(flsExcessPrecision);

        if LPrecision = ppkUnknown then Exit(Answer(pbsUnresolved,pbiCompilerContext,'EXCESSPRECISION'));

        if LPrecision = ppkTrue then Exit(Canonical(pbbExtended));
        Exit(TypeAnswer(LLeft));
      end;

      if not AOperandType and (AKind in [pokEqual,pokNotEqual,pokLess,pokLessEqual,pokGreater,pokGreaterEqual]) then
        Exit(Canonical(pbbBoolean));

      if (LLeftFormat = pffCurrency64) or (LRightFormat = pffCurrency64) then Exit(Canonical(pbbCurrency));

      if LLeftLeaf = nil then Exit(TypeAnswer(LRight));

      if LRightLeaf = nil then Exit(TypeAnswer(LLeft));

      if (LLeftFormat = pffComp64) or (LRightFormat = pffComp64) then Exit(Canonical(pbbExtended));
      LPrecision := EffectiveNodeSwitch(flsExcessPrecision);

      if LPrecision = ppkUnknown then Exit(Answer(pbsUnresolved,pbiCompilerContext,'EXCESSPRECISION'));

      if LPrecision = ppkTrue then Exit(Canonical(pbbExtended));
      LChosen := LLeft;

      if ((LLeftFormat = pffBinary32) and (LRightFormat in [pffBinary64,pffX87Extended80])) or
        ((LLeftFormat = pffBinary64) and (LRightFormat = pffX87Extended80)) then LChosen := LRight;
      Result := TypeAnswer(LChosen);
    end;

    { Only owned literal/expression proofs supply scalar constants. Runtime
      variables and generic return values do not inherit their inputs' values. }
    function ScalarConstantOf(const AType: INyxPascalSemanticType): TNyxPascalConstantValue;
    var LSource: INyxPascalLiteralType; LExpression: INumericExpressionValue;
    begin
      Result := TNyxPascalConstantValue.Unknown;

      if Supports(AType,INyxPascalLiteralType,LSource) then Result := LSource.Value
      else if Supports(AType,INumericExpressionValue,LExpression) then Result := LExpression.ConstantValue;
    end;

    { String nodes retain character-vs-literal-vs-typed roles separately from
      their generic specialization. Only complete constant operands are folded;
      runtime strings supply type alone, including distinct and UTF-8 identity. }
    function StringResult: INyxPascalResolution;
    var
      LProofLeft, LProofRight: INyxPascalStringConstant;
      LLeafLeft, LLeafRight, LChosen, LLease: INyxPascalSemanticType;
      LKindLeft, LKindRight, LKind: TNyxPascalBuiltin;
      LDefault, LNominal: INyxPascalResolution;
      LUnitsLeft, LUnitsRight: TStringCodeUnits;
      LValue: TStringExpressionType; LIdentity: INyxPascalIntrinsicIdentity;
      LKnown, LNode, LWide, LASCII, LRelation: Boolean;
      LAt, LOrder, LCount: Integer;

      function Family(const AType: INyxPascalSemanticType;
        const AProof: INyxPascalStringConstant; out ALeaf: INyxPascalSemanticType;
        out AKind: TNyxPascalBuiltin): Boolean;
      begin
        ALeaf := Unwrap(AType); AKind := pbbNone;
        Result := (ALeaf <> nil) and (ALeaf.Kind in [pstBuiltin,pstString]) and
          (ALeaf.Builtin in [pbbAnsiChar,pbbWideChar,pbbAnsiString,pbbUTF8String,
            pbbUnicodeString,pbbWideString,pbbShortString]);

        if not Result then Exit;
        AKind := ALeaf.Builtin;

        if (AProof <> nil) and (AProof.Form = pscfLiteral) and not AProof.Wide then AKind := pbbNone;
      end;

      function Canonical(AKind: TNyxPascalBuiltin): INyxPascalResolution;
      var LName: TNyxText;
      begin
        case AKind of
          pbbAnsiString: LName := 'ansistring'; pbbUTF8String: LName := 'utf8string';
          pbbUnicodeString: LName := 'unicodestring'; pbbWideString: LName := 'widestring';
          pbbShortString: LName := 'shortstring'; pbbBoolean: LName := 'boolean';
        else Exit(Answer(pbsUnresolved,pbiUnsupportedType));
        end;
        Result := BuiltinType(ASite,LName,FLimits.ImplicitScopes = pispCompilerUnits);
      end;

      { Decode only qualified UTF-8 bytes when a string needs wide storage.
        Character ordinals widen directly. UTF-16 source units, including lone
        surrogates, are copied exactly; no Unicode normalization is performed. }
      function Units(const AValue: INyxPascalStringConstant; AWide: Boolean;
        out AUnits: TStringCodeUnits): Boolean;
      var LRead, LWrite, LCode, LTails, LTail, LMinimum, LCount: Integer;
      begin
        Result := False; LCount := AValue.UnitCount;

        if LCount < 0 then Exit;
        AdmitStringUnits(LCount);
        SetLength(AUnits,LCount); LRead := 0; LWrite := 0;
        while LRead < LCount do
        begin
          Tick; LCode := AValue.CodeUnit(LRead); Inc(LRead);

          if (LCode < 0) or (LCode > $ffff) or (not AValue.Wide and (LCode > 255)) then Exit;

          if AWide and not AValue.Wide and (AValue.Form <> pscfCharacter) and (LCode >= $80) then
          begin

            if AValue.Encoding <> psceUTF8 then Exit;
            LTails := 0; LMinimum := 0;

            if (LCode >= $c2) and (LCode <= $df) then
            begin LTails := 1; LCode := LCode and $1f; LMinimum := $80; end
            else if (LCode >= $e0) and (LCode <= $ef) then
            begin LTails := 2; LCode := LCode and $f; LMinimum := $800; end
            else if (LCode >= $f0) and (LCode <= $f4) then
            begin LTails := 3; LCode := LCode and $7; LMinimum := $10000; end
            else Exit;
            while LTails > 0 do
            begin
              Tick;

              if LRead >= LCount then Exit;
              LTail := AValue.CodeUnit(LRead); Inc(LRead);

              if (LTail < $80) or (LTail > $bf) then Exit;
              LCode := LCode * 64 + (LTail and $3f); Dec(LTails);
            end;

            if (LCode < LMinimum) or (LCode > $10ffff) or
              ((LCode >= $d800) and (LCode <= $dfff)) then Exit;
          end;

          if LCode > $ffff then
          begin
            Dec(LCode,$10000); AUnits[LWrite] := $d800 + (LCode shr 10); Inc(LWrite);
            LCode := $dc00 + (LCode and $3ff);
          end;
          AUnits[LWrite] := LCode; Inc(LWrite);
        end;
        SetLength(AUnits,LWrite); Result := True;
      end;

    begin
      Result := nil;

      if AUnary then Exit;
      Supports(ALeft.BoundType,INyxPascalStringConstant,LProofLeft);
      Supports(ARight.BoundType,INyxPascalStringConstant,LProofRight);

      if not Family(LLeft,LProofLeft,LLeafLeft,LKindLeft) or
        not Family(LRight,LProofRight,LLeafRight,LKindRight) then Exit;
      Result := Answer(pbsUnresolved,pbiUnsupportedType,'string operation');

      if not (AKind in [pokAdd,pokEqual,pokNotEqual,pokLess,pokLessEqual,pokGreater,pokGreaterEqual]) then Exit;
      LKnown := (LProofLeft <> nil) and (LProofRight <> nil);
      LNode := ((LProofLeft <> nil) and (LProofLeft.Form <> pscfCharacter)) or
        ((LProofRight <> nil) and (LProofRight.Form <> pscfCharacter));
      LKind := pbbShortString;

      if (LKindLeft in [pbbAnsiChar,pbbWideChar]) and (LKindRight in [pbbAnsiChar,pbbWideChar]) then
      begin

        if (LKindLeft = pbbWideChar) or (LKindRight = pbbWideChar) then LKind := pbbUnicodeString;
      end
      else
      begin
        LDefault := nil;

        if LNode then
        begin
          LDefault := DefaultStringResult;

          if LDefault.Status <> pbsBound then Exit(LDefault);
        end;

        if (LKindLeft = pbbUnicodeString) or (LKindRight = pbbUnicodeString) or
          ((LDefault <> nil) and (LDefault.BoundType.Builtin = pbbUnicodeString)) then LKind := pbbUnicodeString
        else if (LKindLeft in [pbbWideString,pbbWideChar]) or (LKindRight in [pbbWideString,pbbWideChar]) then LKind := pbbWideString
        else if (LKindLeft = pbbUTF8String) or (LKindRight = pbbUTF8String) then LKind := pbbUTF8String
        else if (LKindLeft = pbbAnsiString) or (LKindRight = pbbAnsiString) or
          ((LDefault <> nil) and (LDefault.BoundType.Builtin = pbbAnsiString)) then LKind := pbbAnsiString;
      end;
      LWide := LKind in [pbbUnicodeString,pbbWideString];

      if LKnown and (LProofLeft.Wide or LProofRight.Wide) then
      begin LWide := True; LKind := pbbUnicodeString; end;
      LChosen := nil;

      { FPC always returns its full ShortString type, independently of a
        String[n] operand's bound or nominal alias. Managed strings can retain
        their selected operand's declaration and code-page identity. }
      if LKind <> pbbShortString then
      begin

        if LKindLeft = LKind then LChosen := LLeft
        else if LKindRight = LKind then LChosen := LRight;
      end;
      LNominal := nil;

      if AKind <> pokAdd then LNominal := Canonical(pbbBoolean)
      else if LChosen = nil then LNominal := Canonical(LKind)
      else LNominal := TypeAnswer(LChosen);

      if LNominal.Status <> pbsBound then Exit(LNominal);

      if not LKnown then Exit(LNominal);

      if not Units(LProofLeft,LWide,LUnitsLeft) or not Units(LProofRight,LWide,LUnitsRight) then
        Exit(Answer(pbsUnresolved,pbiCompilerContext,'string encoding'));

      if AKind <> pokAdd then
      begin
        LOrder := 0; LCount := Length(LUnitsLeft);

        if Length(LUnitsRight) < LCount then LCount := Length(LUnitsRight);
        for LAt := 0 to LCount-1 do
        begin
          Tick;

          if LUnitsLeft[LAt] < LUnitsRight[LAt] then LOrder := -1
          else if LUnitsLeft[LAt] > LUnitsRight[LAt] then LOrder := 1;

          if LOrder <> 0 then Break;
        end;

        if LOrder = 0 then
        begin

          if Length(LUnitsLeft) < Length(LUnitsRight) then LOrder := -1
          else if Length(LUnitsLeft) > Length(LUnitsRight) then LOrder := 1;
        end;
        LRelation := False;
        case AKind of
          pokEqual: LRelation := LOrder = 0; pokNotEqual: LRelation := LOrder <> 0;
          pokLess: LRelation := LOrder < 0; pokLessEqual: LRelation := LOrder <= 0;
          pokGreater: LRelation := LOrder > 0; pokGreaterEqual: LRelation := LOrder >= 0;
        else begin end;
        end;
        Exit(ConstantAnswer(LNominal.BoundType,TNyxPascalConstantValue.BooleanValue(LRelation),
          TNyxPascalIEEEValue.Unknown));
      end;

      if Length(LUnitsLeft) > High(Integer)-Length(LUnitsRight) then
        raise ENyxPascalBindingLimit.Create('String concatenation exceeds its bounded vector');
      LCount := Length(LUnitsLeft)+Length(LUnitsRight);
      AdmitStringUnits(LCount);
      LValue := TStringExpressionType.Create; LLease := LValue;
      LValue.Owner := FWorld; LValue.FResult := LNominal.BoundType;
      LValue.FForm := pscfString; LValue.FWide := LWide;
      SetLength(LValue.FUnits,LCount); LASCII := True;
      for LAt := 0 to LCount-1 do
      begin
        Tick;

        if LAt < Length(LUnitsLeft) then LValue.FUnits[LAt] := LUnitsLeft[LAt]
        else LValue.FUnits[LAt] := LUnitsRight[LAt-Length(LUnitsLeft)];

        if LValue.FUnits[LAt] >= $80 then LASCII := False;
      end;

      if LWide then LValue.FEncoding := psceUTF16
      else if LASCII then LValue.FEncoding := psceASCII
      else if (LProofLeft.Encoding in [psceASCII,psceUTF8]) and
        (LProofRight.Encoding in [psceASCII,psceUTF8]) then LValue.FEncoding := psceUTF8;
      LChosen := Unwrap(LNominal.BoundType);
      LValue.TypeKind := pstBuiltin; LValue.Intrinsic := LChosen.Builtin;
      Supports(LChosen,INyxPascalIntrinsicIdentity,LIdentity);
      LValue.IntrinsicScope := LIdentity.IntrinsicOrigin; LValue.IntrinsicName := LIdentity.IntrinsicReference;
      LValue.IntrinsicFacts := LIdentity.IntrinsicContext;

      if LNominal.BoundType.Kind in [pstAlias,pstDistinct] then
      begin
        LValue.TypeKind := LNominal.BoundType.Kind; LValue.Intrinsic := pbbNone;
        LValue.Declared := LNominal.BoundType.Definition; LValue.Element := LNominal.BoundType.Target;
      end;
      Result := TypeAnswer(LLease);
    end;

    { Constant arithmetic has its own value and host-rounding contract. It uses
      the qualified source result-type rules, then folds at the independently
      observed compiler-host precision, then narrows to that result's storage.
      Integer constants convert through host precision and the common operand
      type before arithmetic/comparison. Runtime operands retain only type proof. }
    function RealConstantResult: INyxPascalResolution;
    var
      LNominal, LOperand, LAdmission: INyxPascalResolution;
      LFormat: TNyxPascalBinaryPrecision;
      LEvaluation: TNyxPascalRealEvaluationPolicy;
      LOperation: TNyxPascalRealOperation;
      LBudget: INyxPascalAnalysisBudget;
      LOrder: Integer;
      LIEEEOrder: TNyxPascalIEEEOrder;
      LRelation, LComparison, LNeedsInteger, LZeroDivisor: Boolean;
      LChecking: TNyxPascalKnowledge;
      LIntegerLeft, LIntegerRight: TNyxPascalConstantValue;
      LLeftValue, LRightValue, LValue: TNyxPascalIEEEValue;
    begin
      Result := Answer(pbsUnresolved,pbiUnsupportedType,'real constant operation');

      if not (AKind in [pokAdd,pokSubtract,pokMultiply,pokDivide,pokEqual,pokNotEqual,
        pokLess,pokLessEqual,pokGreater,pokGreaterEqual]) then Exit;
      LNeedsInteger := False;

      if AUnary then
      begin

        if (LRealLeft = nil) or not (AKind in [pokAdd,pokSubtract]) then Exit;
        { FPC mutates the admitted child for unary negation. It neither widens
          this constant nor constructs a second range-checked real node. }
        LNominal := TypeAnswer(LLeft);
      end
      else
      begin
        LNominal := FloatingResult;

        if (LNominal = nil) or (LNominal.Status <> pbsBound) then
        begin

          if LNominal <> nil then Result := LNominal;
          Exit;
        end;
        LIntegerLeft := ScalarConstantOf(ALeft.BoundType);
        LIntegerRight := ScalarConstantOf(ARight.BoundType);

        if ((LRealLeft = nil) and (LIntegerLeft.Kind <> pvvInteger)) or
          ((LRealRight = nil) and (LIntegerRight.Kind <> pvvInteger)) then Exit(LNominal);
        LNeedsInteger := (LRealLeft = nil) or (LRealRight = nil);
      end;

      if LNominal = nil then Exit;

      if LNominal.Status <> pbsBound then Exit(LNominal);
      LComparison := LNominal.BoundType.Builtin = pbbBoolean;
      LOperand := LNominal;

      if LComparison and LNeedsInteger then LOperand := FloatingResult(True);

      if LOperand = nil then Exit;

      if LOperand.Status <> pbsBound then Exit(LOperand);
      LFormat := pbpUnknown;
      case LOperand.BoundType.Builtin of
        pbbSingle: LFormat := pbpBinary32;
        pbbDouble: LFormat := pbpBinary64;
        pbbExtended: LFormat := pbpX87Extended80;
        pbbBoolean: begin end;
      else Exit;
      end;
      LEvaluation := NyxPascalRealEvaluation(LContext);

      if not AUnary and (not LComparison or LNeedsInteger) and
        ((LEvaluation.Kind <> prekNearestEven) or
        not (LEvaluation.Precision in [pbpBinary64,pbpX87Extended80])) then
        Exit(Answer(pbsUnresolved,pbiCompilerContext,'REALEVALUATION'));

      if not AUnary and (AKind = pokDivide) then
      begin
        LZeroDivisor := False;

        if LRealRight <> nil then LZeroDivisor := LRealRight.IEEEValue.IsZero
        else if LIntegerRight.Kind = pvvInteger then
          LZeroDivisor := LIntegerRight.Number.CompareTo(
            TNyxPascalConstantValue.IntegerValue(0).Number) = 0;

        if LZeroDivisor then
        begin
          { FPC diagnoses a zero divisor before folding, even when its
            mathematical IEEE result would be NaN or negative infinity. }
          LChecking := EffectiveNodeSwitch(flsIEEEErrors);

          if LChecking = ppkUnknown then
            Exit(Answer(pbsUnresolved,pbiCompilerContext,'IEEEERRORS'));

          if LChecking = ppkTrue then
            Exit(Answer(pbsUnresolved,pbiUnsupportedType,'real division by zero'));
        end;
      end;

      if FSteps >= FLimits.MaximumSteps then
        raise ENyxPascalBindingLimit.Create('Pascal semantic steps exceed their query budget');
      LBudget := NewNyxPascalAnalysisBudget(FLimits.MaximumSteps-FSteps,FCancel);
      try
        try
          LLeftValue := TNyxPascalIEEEValue.Unknown;
          LRightValue := LLeftValue;

          if LRealLeft <> nil then LLeftValue := LRealLeft.IEEEValue
          else LLeftValue := TNyxPascalIEEEValue.Finite(
            NyxPascalRoundIntegerToRealEven(LIntegerLeft,LEvaluation.Precision,LBudget)
            .RoundToEven(LFormat,LBudget));

          if not AUnary then
          begin

            if LRealRight <> nil then LRightValue := LRealRight.IEEEValue
            else LRightValue := TNyxPascalIEEEValue.Finite(
              NyxPascalRoundIntegerToRealEven(LIntegerRight,LEvaluation.Precision,LBudget)
              .RoundToEven(LFormat,LBudget));
          end;

          if (LLeftValue.Kind = pifUnknown) or
            (not AUnary and (LRightValue.Kind = pifUnknown)) then Exit;

          if LComparison then
          begin

            if (LLeftValue.Kind = pifFinite) and (LRightValue.Kind = pifFinite) then
            begin

              if not LLeftValue.FiniteValue.TryCompare(LRightValue.FiniteValue,LOrder,LBudget) then Exit;
              LIEEEOrder := pieoEqual;

              if LOrder < 0 then LIEEEOrder := pieoLess
              else if LOrder > 0 then LIEEEOrder := pieoGreater;
            end
            else LIEEEOrder := LLeftValue.Compare(LRightValue,LBudget);

            if LIEEEOrder = pieoUnknown then Exit;
            LRelation := False;
            case AKind of
              pokEqual: LRelation := LIEEEOrder = pieoEqual;
              pokNotEqual: LRelation := LIEEEOrder <> pieoEqual;
              pokLess: LRelation := LIEEEOrder = pieoLess;
              pokLessEqual: LRelation := LIEEEOrder in [pieoLess,pieoEqual];
              pokGreater: LRelation := LIEEEOrder = pieoGreater;
              pokGreaterEqual: LRelation := LIEEEOrder in [pieoGreater,pieoEqual];
            else begin end;
            end;
          end
          else
          begin

            if AUnary then
            begin
              NyxPascalAdmitAnalysis(LBudget,papConstantStep);
              LValue := LLeftValue;

              if AKind = pokSubtract then LValue := LValue.Negated;
            end
            else
            begin
              LOperation := proAdd;
              case AKind of
                pokAdd: LOperation := proAdd;
                pokSubtract: LOperation := proSubtract;
                pokMultiply: LOperation := proMultiply;
                pokDivide: LOperation := proDivide;
              else begin end;
              end;

              if (LLeftValue.Kind = pifFinite) and (LRightValue.Kind = pifFinite) then
                LValue := MathematicalOutcome(NyxPascalCalculateRealToEven(LOperation,
                  LLeftValue.FiniteValue,LRightValue.FiniteValue,LEvaluation.Precision,LBudget))
              else LValue := LLeftValue.Operate(LOperation,LRightValue,LEvaluation.Precision,LBudget);

              if LValue.Kind = pifFinite then
                LValue := MathematicalOutcome(LValue.FiniteValue.RoundToEven(LFormat,LBudget))
              else LValue := LValue.RoundToEven(LFormat,LBudget);
            end;

            if LValue.Kind = pifUnknown then Exit;
          end;
        except on ENyxPascalValue do Exit; end;
      finally Inc(FSteps,LBudget.Receipt.Total); LBudget.Retire; end;

      if LComparison then
        Exit(ConstantAnswer(LNominal.BoundType,TNyxPascalConstantValue.BooleanValue(LRelation),
          TNyxPascalIEEEValue.Unknown));

      if not AUnary then
      begin
        LAdmission := SpecialValueAdmission(LValue);

        if LAdmission <> nil then Exit(LAdmission);
      end;
      Result := ConstantAnswer(LNominal.BoundType,TNyxPascalConstantValue.Unknown,LValue);
    end;

    { Numeric rules receive only canonical fixed ranges. Native/Integer widths
      need their own captured intrinsic measurement; CPUINT controls arithmetic
      promotion and is not evidence about pointer size or authored RTL aliases. }
    function NumericResult: INyxPascalResolution;
    const
      CBuiltins: array[TNyxPascalIntegerKind] of TNyxPascalBuiltin =
        (pbbNone,pbbShortInt,pbbByte,pbbSmallInt,pbbWord,pbbLongInt,pbbLongWord,pbbInt64,pbbQWord);
      CALUNames: array[0..3] of TNyxText = ('CPUINT8','CPUINT16','CPUINT32','CPUINT64');
      CALUBits: array[0..3] of Integer = (8,16,32,64);
    var
      LInitial, LRoot: INyxPascalCompilerContext;
      LIntegerLeft, LIntegerRight: TNyxPascalIntegerOperand;
      LOutcome: TNyxPascalIntegerResult;
      LGap: INyxPascalResolution;
      LValueType, LLease: INyxPascalSemanticType;
      LComputed: TNumericExpressionType;
      LIdentity: INyxPascalIntrinsicIdentity;
      LWidth, LRootWidth, LSystem: Integer;
      LSite: INyxPascalBindingSite;

      function CapturedALU(const AContext: INyxPascalCompilerContext): Integer;
      var
        LIndex: Integer;
      begin
        Result := 0;

        if AContext = nil then Exit;
        for LIndex := 0 to High(CALUNames) do
        begin
          Tick;

          if AContext.Symbol(NyxPascalDefine(CALUNames[LIndex])).IsDefined = ppkTrue then
          begin

            if Result <> 0 then Exit(-1);
            Result := CALUBits[LIndex];
          end;
        end;
      end;

      function IntegerKind(const AType: INyxPascalSemanticType): TNyxPascalIntegerKind;
      var
        LKind: TNyxPascalIntegerKind;
        LIntrinsic: INyxPascalIntrinsicIdentity;
        LFact: TNyxPascalSizeFact;
        LBytes: Integer;
        LSigned: Boolean;
      begin
        Result := pikNone;

        if (AType = nil) or (AType.Kind <> pstBuiltin) then Exit;
        for LKind := pikShortInt to pikQWord do
        begin
          Tick;

          if AType.Builtin = CBuiltins[LKind] then Exit(LKind);
        end;

        if not (AType.Builtin in [pbbInteger,pbbNativeInt,pbbNativeUInt]) then Exit;

        if not Supports(AType,INyxPascalIntrinsicIdentity,LIntrinsic) then Exit;
        LFact := NyxPascalCapturedSize(LIntrinsic.IntrinsicContext,LIntrinsic.IntrinsicReference);

        if LFact.State <> pssKnown then
        begin
          LGap := Answer(pbsUnresolved,pbiCompilerContext,LIntrinsic.IntrinsicReference.Name,
            NyxPascalPrimitiveRequirement(AType,LSite));
          Exit;
        end;
        LBytes := LFact.Bytes;
        LSigned := AType.Builtin <> pbbNativeUInt;
        case LBytes of
          1: Result := pikShortInt;
          2: Result := pikSmallInt;
          4: Result := pikLongInt;
          8: Result := pikInt64;
        end;

        if not LSigned and (Result <> pikNone) then Result := Succ(Result);
      end;

    begin
      Result := nil;
      LSite := FWorld.At(ASite.ModuleIndex,ASite.ScopeIndex,AOffset);
      LGap := nil;
      LIntegerLeft.Kind := IntegerKind(LLeft);

      if LGap <> nil then Exit(LGap);
      LIntegerLeft.Constant := ScalarConstantOf(ALeft.BoundType);
      LIntegerRight := Default(TNyxPascalIntegerOperand);

      if not AUnary then
      begin
        LIntegerRight.Kind := IntegerKind(LRight);

        if LGap <> nil then Exit(LGap);
        LIntegerRight.Constant := ScalarConstantOf(ARight.BoundType);
      end;

      if (LIntegerLeft.Kind = pikNone) or (not AUnary and (LIntegerRight.Kind = pikNone)) then Exit;
      LInitial := FWorld.ModuleAt(ASite.ModuleIndex).Source.InitialContext;
      LWidth := CapturedALU(LInitial);
      LSystem := FWorld.FindModule('System');

      if (FLimits.ImplicitScopes = pispCompilerUnits) and (LSystem < 0) then
        Exit(Answer(pbsUnresolved,pbiMissingModule,'System'));

      if LSystem >= 0 then
      begin
        LRoot := NyxPascalBootstrapContext(FWorld.ModuleAt(LSystem).Source.InitialContext);

        if LRoot <> nil then
        begin
          LRootWidth := CapturedALU(LRoot);

          if (LRootWidth < 0) or ((LWidth > 0) and (LRootWidth > 0) and (LWidth <> LRootWidth)) then
            Exit(Answer(pbsUnresolved,pbiCompilerContext));

          if LWidth = 0 then LWidth := LRootWidth;
        end;
      end;

      if LWidth <= 0 then Exit(Answer(pbsUnresolved,pbiCompilerContext,'CPUINT8/16/32/64'));
      LOutcome := NyxPascalIntegerOperation(AKind,LIntegerLeft,LIntegerRight,AUnary,LWidth,FCancel);

      if LOutcome.BooleanResult then
        Exit(BuiltinType(LSite,'boolean',FLimits.ImplicitScopes = pispCompilerUnits));

      if LOutcome.Kind = pikNone then Exit(Answer(pbsUnresolved,pbiUnsupportedType));
      LValueType := nil;
      case LOutcome.Reuse of
        pirNone: LValueType := nil;
        pirLeft: LValueType := LLeft;
        pirRight: LValueType := LRight;
      end;

      if LValueType = nil then
      begin
        Result := BuiltinType(LSite,NyxPascalIntegerName(LOutcome.Kind),FLimits.ImplicitScopes = pispCompilerUnits);

        if Result.Status <> pbsBound then Exit;
        LValueType := Result.BoundType;
      end;

      if LOutcome.Constant.Kind <> pvvInteger then Exit(TypeAnswer(LValueType));
      LComputed := TNumericExpressionType.Create;
      LLease := LComputed;
      LComputed.Owner := FWorld;
      LComputed.TypeKind := pstBuiltin;
      LComputed.Intrinsic := LValueType.Builtin;
      Supports(LValueType,INyxPascalIntrinsicIdentity,LIdentity);
      LComputed.IntrinsicScope := LIdentity.IntrinsicOrigin;
      LComputed.IntrinsicName := LIdentity.IntrinsicReference;
      LComputed.IntrinsicFacts := LIdentity.IntrinsicContext;
      LComputed.FConstant := LOutcome.Constant;
      LComputed.FResult := LValueType;
      Result := TypeAnswer(LLease);
    end;

    { The compiler filters equivalent non-unique signatures before comparing
      argument ranks. Exact nominal identity remains in SameType; pointer
      equality and void conversion are separate, source-qualified stages. }
    function PointerResult: INyxPascalResolution;
    var LSignatures: array of INyxPascalSemanticType;
      LIndex, LEarlier, LArgument, LConversions, LExacts: Integer;
      LBestConversions, LBestExacts, LBestCount: Integer;
      LFormal, LActual, LLeaf: INyxPascalSemanticType;
      LPointerContext: INyxPascalCompilerContext;
      LReply: INyxPascalResolution;
      LDuplicate, LCompatible, LHasPointer, LPointerDifference, LUnknown: Boolean;

      function Transparent(const AValue: INyxPascalSemanticType): INyxPascalSemanticType;
      begin
        Result := AValue;
        while (Result <> nil) and (Result.Kind = pstAlias) do
        begin Tick; Result := Result.Target; end;
      end;

      function PointerLike(const AValue: INyxPascalSemanticType): Boolean;
      var LValue: INyxPascalSemanticType;
      begin
        LValue := AValue;
        while (LValue <> nil) and (LValue.Kind in [pstAlias,pstDistinct]) do
        begin Tick; LValue := LValue.Target; end;
        Result := (LValue <> nil) and ((LValue.Kind = pstPointer) or
          ((LValue.Kind = pstBuiltin) and (LValue.Builtin = pbbPointer)));
      end;

      function KnownTarget(const AValue: INyxPascalSemanticType): Boolean;
      var LValue: INyxPascalSemanticType;
      begin
        LValue := Transparent(AValue);

        if LValue = nil then Exit(False);

        if LValue.Kind in [pstDeclared,pstBuiltin,pstPointer] then Exit(True);

        if LValue.Kind <> pstDistinct then Exit(False);
        LValue := Unwrap(LValue);
        Result := (LValue <> nil) and ((LValue.Kind in [pstBuiltin,pstPointer]) or
          ((LValue.Kind = pstDeclared) and (LValue.Definition <> nil) and
            (LValue.Definition.Declaration.TypeForm = pstfRecord)));
      end;

      function EquivalentParameter(const AFirst, ASecond: INyxPascalSemanticType): Boolean;
      var LFirst, LSecond: INyxPascalSemanticType;
      begin
        LFirst := Transparent(AFirst); LSecond := Transparent(ASecond);

        if SameType(LFirst,LSecond) then Exit(True);

        if (LFirst = nil) or (LSecond = nil) or
          (LFirst.Kind = pstDistinct) or (LSecond.Kind = pstDistinct) then Exit(False);
        Result := EqualDataPointers(LFirst,LSecond);

        if Result then LPointerDifference := True;
      end;

      function ArgumentRank(const AParameter, AValue: INyxPascalSemanticType;
        AMode: TNyxPascalSyntaxParameterMode): Boolean;
      var LParameterLeaf, LValueLeaf: INyxPascalSemanticType;
        LParameterTarget, LValueTarget: INyxPascalSemanticType;
        LParameterContext, LValueContext: INyxPascalCompilerContext;
        LRelation: TNyxPascalKnowledge;
      begin

        if SameType(AParameter,AValue) then begin Inc(LExacts); Exit(True); end;
        Result := False;

        if not (AMode in [pspmValue,pspmConst]) then Exit;

        if not DataPointer(AParameter,LParameterLeaf,LParameterContext) or
          not DataPointer(AValue,LValueLeaf,LValueContext) then Exit;

        if EqualDataPointers(AParameter,AValue) then Exit(True);
        LParameterTarget := Transparent(LParameterLeaf.Target);
        LValueTarget := Transparent(LValueLeaf.Target);

        if (LParameterTarget <> nil) and (LValueTarget <> nil) and
          (LParameterTarget.Kind = pstDeclared) and (LValueTarget.Kind = pstDeclared) and
          (LParameterTarget.Definition <> nil) and (LValueTarget.Definition <> nil) and
          (LParameterTarget.Definition.Declaration.TypeForm = pstfClass) and
          (LValueTarget.Definition.Declaration.TypeForm = pstfClass) then
        begin
          LRelation := ConstraintRelated(LValueTarget,LParameterTarget);

          if LRelation = ppkUnknown then LUnknown := True;

          if LRelation = ppkTrue then begin Inc(LConversions); Exit(True); end;
        end;
        { A failed exact comparison is not proof of incompatibility for type
          families whose compiler equality has not been established here. Such
          a candidate could beat a void fallback, so preserve the gap instead
          of publishing that weaker candidate as an authoritative answer. }

        if (LParameterTarget <> nil) and (LValueTarget <> nil) and
          (not KnownTarget(LParameterTarget) or not KnownTarget(LValueTarget)) then LUnknown := True;
        LParameterTarget := Unwrap(LParameterTarget); LValueTarget := Unwrap(LValueTarget);

        if (LParameterTarget <> nil) and (LValueTarget <> nil) and
          (LParameterTarget.Kind = pstBuiltin) and (LValueTarget.Kind = pstBuiltin) and
          (LParameterTarget.Builtin = LValueTarget.Builtin) then LUnknown := True;
        { Void conversions are weaker than equal pointees. Character-pointer
          conversion penalties need their own string/conversion qualification;
          they are deliberately not borrowed from the flat data-pointer rule. }

        if ((LParameterLeaf.Kind = pstBuiltin) and (LValueLeaf.Kind = pstPointer)) or
          ((LValueLeaf.Kind = pstBuiltin) and (LParameterLeaf.Kind = pstPointer)) then
        begin
          LLeaf := LParameterLeaf.Target;

          if LParameterLeaf.Kind = pstBuiltin then LLeaf := LValueLeaf.Target;
          while (LLeaf <> nil) and (LLeaf.Kind in [pstAlias,pstDistinct]) do
          begin Tick; LLeaf := LLeaf.Target; end;

          if (LLeaf <> nil) and (LLeaf.Kind = pstBuiltin) and
            (LLeaf.Builtin in [pbbChar,pbbAnsiChar,pbbWideChar]) then Exit;
          Inc(LConversions); Result := True;
        end;
      end;

    begin
      Result := nil;
      LHasPointer := PointerLike(LLeft) or (not AUnary and PointerLike(LRight));

      if not LHasPointer then Exit;

      if (PointerLike(LLeft) and not DataPointer(LLeft,LLeaf,LPointerContext)) or
        (not AUnary and PointerLike(LRight) and not DataPointer(LRight,LLeaf,LPointerContext)) then
        Exit(Answer(pbsUnresolved,pbiCompilerContext,'data pointer representation'));
      SetLength(LSignatures,Length(LSymbols));
      LBestConversions := MaxInt; LBestExacts := -1; LBestCount := 0; LUnknown := False;
      for LIndex := 0 to High(LSymbols) do
      begin
        Tick;
        LReply := RoutineSignature(LSymbols[LIndex]);

        if LReply.Status <> pbsBound then Exit(LReply);

        if LReply.BoundType.ParameterCount <> LArity then Continue;

        if (LReply.BoundType.Kind <> pstFunction) or (LReply.BoundType.Target = nil) then
          Exit(Answer(pbsUnresolved,pbiUnsupportedType));
        LSignatures[LIndex] := LReply.BoundType;
        LDuplicate := False;
        for LEarlier := 0 to LIndex-1 do
        begin
          Tick;

          if (LSignatures[LEarlier] = nil) or
            (LSymbols[LEarlier].ModuleIndex <> LSymbols[LIndex].ModuleIndex) or
            (LSymbols[LEarlier].Declaration.Scope <> LSymbols[LIndex].Declaration.Scope) or
            not SameEnvironment(EnvironmentOf(LSymbols[LEarlier]),EnvironmentOf(LSymbols[LIndex])) then Continue;
          LCompatible := True; LPointerDifference := False;
          for LArgument := 0 to LArity-1 do
          begin
            Tick;

            if (LSignatures[LEarlier].ParameterMode(LArgument) <>
                LSignatures[LIndex].ParameterMode(LArgument)) and
              ((LSignatures[LEarlier].ParameterMode(LArgument) in [pspmVar,pspmOut,pspmConstRef]) or
               (LSignatures[LIndex].ParameterMode(LArgument) in [pspmVar,pspmOut,pspmConstRef])) then
              LCompatible := False;

            if not EquivalentParameter(LSignatures[LEarlier].ParameterType(LArgument),
              LSignatures[LIndex].ParameterType(LArgument)) then LCompatible := False;
          end;

          if LCompatible and LPointerDifference then begin LDuplicate := True; Break; end;
        end;

        if LDuplicate then begin LSignatures[LIndex] := nil; Continue; end;
        LConversions := 0; LExacts := 0; LCompatible := True;
        for LArgument := 0 to LArity-1 do
        begin
          Tick;
          LFormal := LSignatures[LIndex].ParameterType(LArgument); LActual := LLeft;

          if LArgument = 1 then LActual := LRight;

          if not ArgumentRank(LFormal,LActual,LSignatures[LIndex].ParameterMode(LArgument)) then
            LCompatible := False;
        end;

        if not LCompatible then Continue;

        if (LConversions < LBestConversions) or
          ((LConversions = LBestConversions) and (LExacts > LBestExacts)) then
        begin
          LBestConversions := LConversions; LBestExacts := LExacts;
          LBestCount := 1; LCommon := LSignatures[LIndex].Target;
        end
        else if (LConversions = LBestConversions) and (LExacts = LBestExacts) then Inc(LBestCount);
      end;

      if LUnknown then Exit(Answer(pbsUnresolved,pbiCompilerContext,'pointer conversion prerequisites'));

      if LBestCount > 1 then Exit(Answer(pbsUnresolved,pbiOverloadContext));

      if LBestCount = 0 then Exit(Answer(pbsUnresolved,pbiUnsupportedType));
      Result := TypeAnswer(LCommon);
    end;

    function Collect(const AType: INyxPascalSemanticType): INyxPascalResolution;
    var
      LIndex, LExisting: Integer;
      LDuplicate: Boolean;
    begin
      Result := nil;

      if (AType = nil) or not (AType.Kind in [pstDeclared, pstFormal]) then Exit;
      LFound := Member(AType, ASite, '', pmaType, AKind);

      if LFound.Status = pbsMissing then Exit;

      if not (LFound.Status in [pbsBound, pbsOverloads]) then Exit(LFound);
      for LIndex := 0 to LFound.Count - 1 do
      begin
        Tick;
        LDuplicate := False;
        for LExisting := 0 to High(LSymbols) do
        begin
          Tick;

          if SameBinding(LSymbols[LExisting], LFound.SymbolAt(LIndex)) then
          begin
            LDuplicate := True;
            Break;
          end;
        end;

        if LDuplicate then Continue;

        if Length(LSymbols) >= FLimits.MaximumCandidates then
          raise ENyxPascalBindingLimit.Create('Operator alternatives exceed their query budget');
        SetLength(LSymbols, Length(LSymbols) + 1);
        LSymbols[High(LSymbols)] := LFound.SymbolAt(LIndex);
      end;
    end;

  begin

    if ALeft.Status <> pbsBound then Exit(ALeft);

    if not AUnary and (ARight.Status <> pbsBound) then Exit(ARight);
    LContext := ContextAt(FWorld.At(ASite.ModuleIndex, ASite.ScopeIndex, AOffset));

    if (LContext = nil) or (LContext.Family <> ppcFPC) or
      (LContext.Version.FullNumber <> 30301) or not (LContext.Mode in [ppmDelphi, ppmDelphiUnicode]) then
      Exit(Answer(pbsUnresolved, pbiCompilerContext));
    LLeft := OrdinaryType(ALeft.BoundType);
    LRight := nil;

    if not AUnary then LRight := OrdinaryType(ARight.BoundType);

    if (LLeft = nil) or (not AUnary and (LRight = nil)) then
      Exit(Answer(pbsUnresolved, pbiUnsupportedType));
    Result := StringResult;

    if Result <> nil then Exit;
    Supports(ALeft.BoundType,INyxPascalIEEEConstant,LRealLeft);

    if not AUnary then Supports(ARight.BoundType,INyxPascalIEEEConstant,LRealRight);

    if (LRealLeft <> nil) or (LRealRight <> nil) or
      (not AUnary and (AKind = pokDivide) and NyxPascalRealEvaluation(LContext).Known and
      (ScalarConstantOf(ALeft.BoundType).Kind = pvvInteger) and
      (ScalarConstantOf(ARight.BoundType).Kind = pvvInteger)) then
      Exit(RealConstantResult);
    LBoolean := nil;

    if (LLeft.Kind = pstBuiltin) and (LLeft.Builtin = pbbBoolean) and
      ((AUnary and (AKind = pokNot)) or (not AUnary and
        (LRight.Kind = pstBuiltin) and (LRight.Builtin = pbbBoolean) and
        (AKind in [pokAnd, pokOr, pokXor]))) then LBoolean := LLeft;

    if LBoolean <> nil then Exit(TypeAnswer(LBoolean));
    Result := FloatingResult;

    if Result <> nil then Exit;
    Result := NumericResult;

    if Result <> nil then Exit;
    SetLength(LSymbols, 0);
    Result := Collect(LLeft);

    if Result <> nil then Exit;

    if not AUnary then
    begin
      Result := Collect(LRight);

      if Result <> nil then Exit;
    end;
    LArity := 2;

    if AUnary then LArity := 1;
    Result := PointerResult;

    if Result <> nil then Exit;
    LExact := False;
    LSeen := False;
    LCommon := nil;
    for LCandidate := 0 to High(LSymbols) do
    begin
      Tick;
      LSignature := RoutineSignature(LSymbols[LCandidate]);

      if LSignature.Status <> pbsBound then Exit(LSignature);

      if LSignature.BoundType.ParameterCount <> LArity then Continue;
      if (LSignature.BoundType.Kind <> pstFunction) or (LSignature.BoundType.Target = nil) then
        Exit(Answer(pbsUnresolved, pbiUnsupportedType));
      LMatch := True;
      for LOperand := 0 to LArity - 1 do
      begin
        Tick;

        if LOperand = 0 then
          LMatch := LMatch and SameType(LSignature.BoundType.ParameterType(LOperand), LLeft)
        else
          LMatch := LMatch and SameType(LSignature.BoundType.ParameterType(LOperand), LRight);
      end;
      if not LMatch then Continue;
      LSeen := True;
      LExact := True;

      if LCommon = nil then LCommon := LSignature.BoundType.Target
      else if not SameType(LCommon, LSignature.BoundType.Target) then
        Exit(Answer(pbsUnresolved, pbiOverloadContext));
    end;

    if not LSeen or not LExact then Exit(Answer(pbsUnresolved, pbiUnsupportedType));
    Result := TypeAnswer(LCommon);
  end;

  { Prefix recursion has the same depth/work owner as postfix parsing. Signed
    number tokens retain their qualified literal rule; other prefixes require
    an actual source operator or the closed Boolean intrinsic rule above. }
  function Factor(out AValueAccess: TNyxPascalMemberAccess): INyxPascalResolution;
  var
    LKind: TNyxPascalOperatorKind;
    LOperand: INyxPascalResolution;
    LInnerAccess: TNyxPascalMemberAccess;
    LSavedAt, LOffset: Integer;
  begin
    LKind := CurrentOperator;

    if not (LKind in [pokAdd, pokSubtract, pokNot]) then Exit(Postfix(AValueAccess));
    Enter;
    try
      LSavedAt := LAt;
      LOffset := LToken.Span.Start;
      Next;

      if (LKind in [pokAdd, pokSubtract]) and LAvailable and (LToken.Kind = ptkNumber) then
      begin
        LAt := LSavedAt;
        ReadToken;
        Exit(Postfix(AValueAccess));
      end;
      LOperand := Factor(LInnerAccess);
      AValueAccess := pmaInstance;

      if LInnerAccess <> pmaInstance then Exit(Answer(pbsUnresolved, pbiWrongMemberKind));
      Result := OperatorResult(LKind, LOffset, LOperand, nil, True);
    finally
      Leave;
    end;
  end;

  function Expression(out AValueAccess: TNyxPascalMemberAccess): INyxPascalResolution;

    function Term(out ATermAccess: TNyxPascalMemberAccess): INyxPascalResolution;
    var
      LRight: INyxPascalResolution;
      LRightAccess: TNyxPascalMemberAccess;
      LKind: TNyxPascalOperatorKind;
      LOffset: Integer;
    begin
      Result := Factor(ATermAccess);

      if Result.Status <> pbsBound then Exit;
      while CurrentOperator in [pokMultiply, pokDivide, pokIntDivide, pokModulus,
        pokAnd, pokShiftLeft, pokShiftRight] do
      begin
        LKind := CurrentOperator;
        LOffset := LToken.Span.Start;
        Next;
        LRight := Factor(LRightAccess);

        if (ATermAccess <> pmaInstance) or (LRightAccess <> pmaInstance) then
          Exit(Answer(pbsUnresolved, pbiWrongMemberKind));
        Result := OperatorResult(LKind, LOffset, Result, LRight, False);
        ATermAccess := pmaInstance;

        if Result.Status <> pbsBound then Exit;
      end;
    end;

    function Simple(out ASimpleAccess: TNyxPascalMemberAccess): INyxPascalResolution;
    var
      LRight: INyxPascalResolution;
      LRightAccess: TNyxPascalMemberAccess;
      LKind: TNyxPascalOperatorKind;
      LOffset: Integer;
    begin
      Result := Term(ASimpleAccess);

      if Result.Status <> pbsBound then Exit;
      while CurrentOperator in [pokAdd, pokSubtract, pokOr, pokXor] do
      begin
        LKind := CurrentOperator;
        LOffset := LToken.Span.Start;
        Next;
        LRight := Term(LRightAccess);

        if (ASimpleAccess <> pmaInstance) or (LRightAccess <> pmaInstance) then
          Exit(Answer(pbsUnresolved, pbiWrongMemberKind));
        Result := OperatorResult(LKind, LOffset, Result, LRight, False);
        ASimpleAccess := pmaInstance;

        if Result.Status <> pbsBound then Exit;
      end;
    end;

  var
    LRight: INyxPascalResolution;
    LRightAccess: TNyxPascalMemberAccess;
    LKind: TNyxPascalOperatorKind;
    LOffset: Integer;
    LSavedFPUContext: INyxPascalCompilerContext;
    LSavedFPUChecked: Boolean;
  begin
    LSavedFPUContext := LFPUContext;
    LSavedFPUChecked := LFPUChecked;
    LFPUContext := nil;
    LFPUChecked := False;
    try
      Result := Simple(AValueAccess);

      if Result.Status <> pbsBound then Exit;
      LKind := CurrentOperator;

      if LKind in [pokEqual, pokNotEqual, pokLess, pokLessEqual, pokGreater, pokGreaterEqual] then
      begin
        LOffset := LToken.Span.Start;
        Next;
        LRight := Simple(LRightAccess);

        if (AValueAccess <> pmaInstance) or (LRightAccess <> pmaInstance) then
          Exit(Answer(pbsUnresolved, pbiWrongMemberKind));
        Result := OperatorResult(LKind, LOffset, Result, LRight, False);
        AValueAccess := pmaInstance;
      end;
    finally
      LFPUContext := LSavedFPUContext;
      LFPUChecked := LSavedFPUChecked;
    end;
  end;
begin
  CheckSite(ASite);
  AAccess := pmaInstance;
  LLex := FWorld.ModuleAt(ASite.ModuleIndex).Syntax.Lexical;

  if (AWindow.Length < 1) or (AWindow.Start < 0) or (AWindow.Finish > ASite.Caret) or
    (AWindow.Finish > LLex.Scalars) then
    raise ENyxPascalBinding.Create('An expression needs an exact earlier window in its source module');
  LAt := LLex.TokenAt(AWindow.Start);
  LBadBoundary := False;
  LFPUChecked := False;
  ReadToken;
  LAnswer := Expression(LAccess);

  if LAnswer.Status <> pbsBound then Exit(LAnswer);

  if LAvailable or LBadBoundary then Exit(Answer(pbsUnresolved, pbiUnsupportedType));

  if LAnswer.Status = pbsBound then AAccess := LAccess;
  Result := LAnswer;
end;

function TSemanticWorld.RoutineSignature(const ARoutine: INyxPascalBoundSymbol;
  const ACancellation: INyxCancellationScope): INyxPascalResolution;
var
  LQuery: TBindingQuery;
begin
  LQuery := TBindingQuery.Create(Self, ACancellation);
  try
    Result := LQuery.RoutineSignature(ARoutine);
  finally
    LQuery.Free;
  end;
end;

function TSemanticWorld.ExpressionType(const ASite: INyxPascalBindingSite;
  const AWindow: TNyxPascalSpan; out AAccess: TNyxPascalMemberAccess;
  const ACancellation: INyxCancellationScope): INyxPascalResolution;
var
  LQuery: TBindingQuery;
begin
  LQuery := TBindingQuery.Create(Self, ACancellation);
  try
    Result := LQuery.ExpressionType(ASite, AWindow, AAccess);
  finally
    LQuery.Free;
  end;
end;

function TSemanticWorld.Members(const AType: INyxPascalSemanticType;
  const ASite: INyxPascalBindingSite; const AQuery: TNyxPascalMemberQuery;
  const ACancellation: INyxCancellationScope): INyxPascalMemberPage;
var
  LQuery: TBindingQuery;
begin
  LQuery := TBindingQuery.Create(Self, ACancellation);
  try
    Result := LQuery.Members(AType, ASite, AQuery);
  finally
    LQuery.Free;
  end;
end;

function TSemanticWorld.Lookup(const ASite: INyxPascalBindingSite;
  const AName: TNyxPascalBindingName; const ACancellation: INyxCancellationScope): INyxPascalResolution;
var
  LQuery: TBindingQuery;
begin
  LQuery := TBindingQuery.Create(Self, ACancellation);
  try
    Result := LQuery.Lookup(ASite, AName.Name);
  finally
    LQuery.Free;
  end;
end;

function TSemanticWorld.TypeOfSymbol(const ASymbol: INyxPascalBoundSymbol;
  const ACancellation: INyxCancellationScope): INyxPascalResolution;
var
  LQuery: TBindingQuery;
begin
  LQuery := TBindingQuery.Create(Self, ACancellation);
  try
    Result := LQuery.TypeOfSymbol(ASymbol);
  finally
    LQuery.Free;
  end;
end;

function TSemanticWorld.Member(const AType: INyxPascalSemanticType;
  const ASite: INyxPascalBindingSite; const AName: TNyxPascalBindingName;
  AAccess: TNyxPascalMemberAccess; const ACancellation: INyxCancellationScope): INyxPascalResolution;
var
  LQuery: TBindingQuery;
begin
  LQuery := TBindingQuery.Create(Self, ACancellation);
  try
    Result := LQuery.Member(AType, ASite, AName.Name, AAccess);
  finally
    LQuery.Free;
  end;
end;

function TSemanticWorld.OperatorMembers(const AType: INyxPascalSemanticType;
  const ASite: INyxPascalBindingSite; AKind: TNyxPascalOperatorKind;
  const ACancellation: INyxCancellationScope): INyxPascalResolution;
var
  LQuery: TBindingQuery;
begin

  if (Ord(AKind) <= Ord(pokUnknown)) or (Ord(AKind) > Ord(High(TNyxPascalOperatorKind))) then
  begin
    raise ENyxPascalBinding.Create('Select one supported typed operator family');
  end;
  LQuery := TBindingQuery.Create(Self, ACancellation);
  try
    Result := LQuery.Member(AType, ASite, '', pmaType, AKind);
  finally
    LQuery.Free;
  end;
end;

function NewNyxPascalSemanticWorld(const AOptions: TNyxPascalBindingOptions): INyxPascalSemanticWorld;
begin
  Result := TSemanticWorld.Create(AOptions);
end;

function NyxPascalOwnedSemanticWorld(const AWorld: INyxPascalSemanticWorld): Boolean;
var
  LWorld: INyxPascalSemanticWorld;
  LOwner: TSemanticWorld;
begin
  LWorld := AWorld;
  Result := False;

  if not (LWorld is TSemanticWorld) then
    Exit;
  LOwner := LWorld as TSemanticWorld;
  Result := (LOwner.ClassType = TSemanticWorld) and
    ((LOwner as INyxPascalSemanticWorld) = LWorld);
end;

function WithNyxPascalSemanticModule(const AWorld: INyxPascalSemanticWorld;
  const AView: INyxPascalTranslatedSyntax;
  const AExecution: INyxCancellationScope): INyxPascalSemanticWorld;
var
  LAdmission: INyxPascalWorldAdmission;
  LBudget: INyxPascalAnalysisBudget;
begin

  if AWorld = nil then
  begin
    raise ENyxPascalBinding.Create('Module admission needs its exact input world');
  end;

  if (AExecution <> nil) and AExecution.Cancelled then
  begin
    raise ENyxPascalAnalysisCancelled.Create('Semantic module construction cancelled');
  end;

  if Supports(AWorld, INyxPascalWorldAdmission, LAdmission) then
  begin
    Result := LAdmission.WithModuleInScope(AView, AExecution);
  end
  else
  begin

    if Supports(AExecution, INyxPascalAnalysisBudget, LBudget) then
    begin
      raise ENyxPascalBinding.Create('This semantic world cannot admit guarded module work');
    end;
    Result := AWorld.WithModule(AView);
  end;
end;

function NyxPascalPrimitiveRequirement(const AType: INyxPascalSemanticType;
  const ASite: INyxPascalBindingSite): INyxPascalCompilerRequirement;
var
  LIdentity: INyxPascalIntrinsicIdentity;
  LContext: INyxPascalCompilerContext;
  LExpected: INyxPascalCompilerContext;
  LOwner: INyxPascalTranslatedSyntax;
  LModule: Integer;
  LState: Integer;
  LOffset: Integer;
begin
  Result := nil;

  if (ASite = nil) or (ASite.World = nil) then
  begin
    Exit;
  end;

  if (AType <> nil) and (AType.World <> ASite.World) then
  begin
    raise ENyxPascalBinding.Create('A primitive requirement cannot borrow another source world');
  end;

  if (AType = nil) or (AType.Kind <> pstBuiltin) or
    not Supports(AType, INyxPascalIntrinsicIdentity, LIdentity) then
  begin
    Exit;
  end;

  LContext := LIdentity.IntrinsicContext;

  if (LContext.Family <> ppcFPC) or (LContext.Version.FullNumber = 0) or
    (NyxPascalCapturedSize(LContext, LIdentity.IntrinsicReference).State <> pssUnknown) then
  begin
    Exit;
  end;
  LModule := ASite.ModuleIndex;
  LOffset := ASite.Caret;

  if LIdentity.IntrinsicOrigin in [pioSystem, pioSystemBootstrap] then
  begin
    LModule := ASite.World.FindModule('System');

    if LModule < 0 then
    begin
      Exit;
    end;
    LOffset := ASite.World.InterfaceBoundary(LModule);
  end;
  LOwner := ASite.World.ModuleAt(LModule);

  if LIdentity.IntrinsicOrigin = pioSystemBootstrap then
  begin
    LState := -1;
    LExpected := NyxPascalBootstrapContext(LOwner.Source.InitialContext);
  end
  else
  begin
    LState := NyxPascalContextStateAt(LOwner, LOffset);
    LExpected := nil;

    if LState >= 0 then
    begin
      LExpected := LOwner.Source.State(LState);
    end;
  end;

  if (LExpected = nil) or not LExpected.SameFacts(LContext) then
  begin
    Exit;
  end;
  Result := NewNyxPascalPrimitiveRequirement(
    LContext, LIdentity.IntrinsicReference, LOwner,
    ASite.World.ModuleAt(ASite.ModuleIndex), LState, ASite.Caret);
end;

function NyxPascalSameStorageBinding(const ABefore, ACurrent: INyxPascalBindingSite;
  const AName: TNyxPascalBindingName;
  const ACancellation: INyxCancellationScope): Boolean;
var
  LWorld: INyxPascalSemanticWorld;
  LBefore, LCurrent, LTyped: INyxPascalResolution;
  LLeft, LRight: INyxPascalSemanticType;
  LDepth: Integer;

  procedure Admit;
  begin
    NyxPascalAdmitAnalysis(ACancellation, papBindingStep);

    if (ACancellation <> nil) and ACancellation.Cancelled then
      raise ENyxPascalAnalysisCancelled.Create('Storage binding correspondence cancelled');
  end;

  function Stable(const ASymbol: INyxPascalBoundSymbol): Boolean;
  var
    LSubstituted: INyxPascalSubstitutedSymbol;
    LView: INyxPascalTranslatedSyntax;
    LScope, LOwner, LDepth: Integer;
  begin
    Admit;
    Result := False;

    if (ASymbol = nil) or (ASymbol.World <> LWorld) or
      (ASymbol.Declaration.Flags * [psdfIncomplete, psdfConditional, psdfUnsupported] <> []) or
      (ASymbol.Declaration.GenericSpan.Length <> 0) then Exit;

    if Supports(ASymbol, INyxPascalSubstitutedSymbol, LSubstituted) and
      (LSubstituted.Environment <> nil) then Exit;
    LView := LWorld.ModuleAt(ASymbol.ModuleIndex);
    LScope := ASymbol.Declaration.Scope;
    LDepth := 0;
    while LScope >= 0 do
    begin
      Admit;
      Inc(LDepth);

      if LDepth > LWorld.Options.MaximumDepth then
        raise ENyxPascalBindingLimit.Create('Storage declaration ownership exceeds its depth budget');

      if LView.Syntax.Scope(LScope).UnsupportedSyntax then Exit;
      LOwner := LView.Syntax.Scope(LScope).OwnerDeclaration;

      if (LOwner >= 0) and (LView.Syntax.Declaration(LOwner).GenericSpan.Length <> 0) then Exit;
      LScope := LView.Syntax.Scope(LScope).Parent;
    end;
    Result := True;
  end;

  function SameSymbol(const ALeft, ARight: INyxPascalBoundSymbol): Boolean;
  begin
    Admit;
    Result := Stable(ALeft) and Stable(ARight) and
      (ALeft.ModuleIndex = ARight.ModuleIndex) and
      (ALeft.DeclarationIndex = ARight.DeclarationIndex);
  end;

begin
  Admit;
  Result := False;

  if (ABefore = nil) or (ACurrent = nil) or (ABefore.World <> ACurrent.World) then Exit;
  LWorld := ABefore.World;
  LBefore := LWorld.Lookup(ABefore, AName, ACancellation);
  LCurrent := LWorld.Lookup(ACurrent, AName, ACancellation);

  if (LBefore.Status <> pbsBound) or (LCurrent.Status <> pbsBound) or
    (LBefore.Count <> 1) or (LCurrent.Count <> 1) or
    not SameSymbol(LBefore.SymbolAt(0), LCurrent.SymbolAt(0)) then Exit;
  LTyped := LWorld.TypeOfSymbol(LBefore.SymbolAt(0), ACancellation);

  if LTyped.Status <> pbsBound then Exit;
  LLeft := LTyped.BoundType;
  LTyped := LWorld.TypeOfSymbol(LCurrent.SymbolAt(0), ACancellation);

  if LTyped.Status <> pbsBound then Exit;
  LRight := LTyped.BoundType;
  LDepth := 0;
  repeat
    Admit;
    Inc(LDepth);

    if LDepth > LWorld.Options.MaximumDepth then
      raise ENyxPascalBindingLimit.Create('Storage alias correspondence exceeds its depth budget');

    if (LLeft = nil) or (LRight = nil) or (LLeft.Kind <> LRight.Kind) or
      (LLeft.Kind = pstBuiltin) or not SameSymbol(LLeft.Definition, LRight.Definition) or
      (LLeft.Definition.Declaration.Kind <> psdkType) or
      LLeft.Definition.Declaration.Anonymous then Exit;

    if not (LLeft.Kind in [pstAlias, pstDistinct]) then Break;
    LLeft := LLeft.Target;
    LRight := LRight.Target;
  until False;
  Result := True;
end;

end.
