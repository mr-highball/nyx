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
  nyx.pascal.syntax, nyx.pascal.types, nyx.pascal.syntax.translated;

type
  { Invalid snapshot/site references are caller failures. Semantic missing or
    unsupported source meaning is returned as a typed resolution, never executed. }
  ENyxPascalBinding = class(EArgumentException);
  { Query budgets refuse the entire response, independently of missing names.
    No partial symbol list is presented as a complete semantic search. }
  ENyxPascalBindingLimit = class(ENyxPascalBinding);
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
    Numeric bounds/defaults, layout and assignment compatibility are not inferred. }
  TNyxPascalSemanticTypeKind = (pstDeclared, pstAlias, pstDistinct, pstPointer,
    pstArray, pstSet, pstFile, pstString, pstClassOf, pstProcedure, pstFunction,
    pstReference, pstEnumeration, pstSubrange, pstBuiltin, pstUntyped);
  { Language intrinsic names only. This never supplies hardcoded SDK members or
    hides a later uncaptured import. Sizes/ABI remain in captured compiler context. }
  TNyxPascalBuiltin = (pbbNone, pbbInteger, pbbByte, pbbWord, pbbSmallInt,
    pbbShortInt, pbbLongInt, pbbLongWord, pbbInt64, pbbQWord, pbbNativeInt,
    pbbNativeUInt, pbbBoolean, pbbByteBool, pbbWordBool, pbbLongBool,
    pbbChar, pbbAnsiChar, pbbWideChar, pbbSingle, pbbDouble, pbbExtended,
    pbbReal, pbbCurrency, pbbPointer, pbbAnsiString, pbbUTF8String,
    pbbUnicodeString, pbbWideString, pbbShortString);
  { Declaration-only callers may retain symbolic intrinsic classification.
    Compiler-unit policy instead resolves the implicit import chain, refusing a
    missing unit exactly like a missing explicit import. No SDK stub is created. }
  TNyxPascalImplicitScopePolicy = (pispDeclarationNames, pispCompilerUnits);
  { An unqualified symbolic name or an intrinsic supplied
    in System's scope. Equal spelling/category alone does not establish identity. }
  TNyxPascalIntrinsicOrigin = (pioUnqualified, pioSystem);
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

  { Concrete arguments for one exact generic declaration, with an optional outer
    lexical frame for nested types. Owner is an unspecialized declaration handle.
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
    Queries are read-only/cancellable and share only constructed immutable inputs. }
  INyxPascalSemanticWorld = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570E05}']
    function Options: TNyxPascalBindingOptions;
    function Count: Integer;
    function ModuleAt(AIndex: Integer): INyxPascalTranslatedSyntax;
    function FindModule(const AName: TNyxText): Integer;
    { Cached interface boundary avoids rescanning a whole unit in each query. }
    function InterfaceBoundary(AModule: Integer): Integer;
    { Add/replace an exact module view, retaining earlier worlds. Compiler family
      and version must agree; complete target/default/SDK coherence stays separate. }
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

{ Validate up to 1024 ASCII identifier/dot characters; each segment is nonempty
  and starts with a letter/underscore. No keyword/member catalog is hardcoded. }
function NyxPascalBindingName(const AName: TNyxText): TNyxPascalBindingName;
{ Empty persistent source world. Nothing parses, opens, executes or enrolls a
  project until the caller explicitly supplies an already constructed source view. }
function NewNyxPascalSemanticWorld(const AOptions: TNyxPascalBindingOptions): INyxPascalSemanticWorld;

implementation

uses nyx.text.index;

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
  { Immutable concrete lexical frame, built completely before publication. Its
    owner is raw, its parent is an earlier outer frame and its arguments already
    exist; this direction prevents recursive class/member reference cycles. }
  TGenericEnvironment = class(TInterfacedObject, INyxPascalGenericEnvironment)
  public
    DeclarationOwner: INyxPascalBoundSymbol;
    Outer: INyxPascalGenericEnvironment;
    Names: array of TNyxText;
    Types: array of INyxPascalSemanticType;
    function Owner: INyxPascalBoundSymbol;
    function Parent: INyxPascalGenericEnvironment;
    function Count: Integer;
    function Name(AIndex: Integer): TNyxText;
    function Argument(AIndex: Integer): INyxPascalSemanticType;
  end;
  { Query-local identifier vector; it holds no interfaces or mutable source. }
  TGenericNames = array of TNyxText;
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
  { One constructed response; the world retains no results. Symbols and types
    own their source world outward, preventing editor/model reference cycles. }
  TResolution = class(TInterfacedObject, INyxPascalResolution)
  public
    Owner: INyxPascalSemanticWorld;
    Outcome: TNyxPascalBindingStatus;
    Reason: TNyxPascalBindingIssue;
    Related: TNyxText;
    Symbols: array of INyxPascalBoundSymbol;
    TypeValue: INyxPascalSemanticType;
    function World: INyxPascalSemanticWorld;
    function Status: TNyxPascalBindingStatus;
    function Issue: TNyxPascalBindingIssue;
    function RelatedName: TNyxText;
    function MessageText: TNyxText;
    function Count: Integer;
    function SymbolAt(AIndex: Integer): INyxPascalBoundSymbol;
    function BoundType: INyxPascalSemanticType;
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
    INyxPascalMemberQueries, INyxPascalOperatorQueries)
  private
    FLimits: TNyxPascalBindingOptions;
    FModules: array of INyxPascalTranslatedSyntax;
    FInterfaceEnds: array of Integer;
    FNames: TNyxTextIndex;
  public
    constructor Create(const AOptions: TNyxPascalBindingOptions);
    destructor Destroy; override;
    function Options: TNyxPascalBindingOptions;
    function Count: Integer;
    function ModuleAt(AIndex: Integer): INyxPascalTranslatedSyntax;
    function FindModule(const AName: TNyxText): Integer;
    function InterfaceBoundary(AModule: Integer): Integer;
    function WithModule(const AView: INyxPascalTranslatedSyntax): INyxPascalSemanticWorld;
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
    FEnvironment: INyxPascalGenericEnvironment;
    FTypeOwner: INyxPascalBoundSymbol;
    procedure Tick;
    procedure Enter;
    procedure Leave;
    procedure CheckSite(const ASite: INyxPascalBindingSite);
    procedure CheckSymbol(const ASymbol: INyxPascalBoundSymbol);
    function Answer(AStatus: TNyxPascalBindingStatus;
      AIssue: TNyxPascalBindingIssue; const ARelated: TNyxText = ''): INyxPascalResolution;
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
    function Descends(const AChild, AParent: INyxPascalBoundSymbol;
      out AUnresolved: Boolean): Boolean;
    function Access(const AMember, AOwner: INyxPascalBoundSymbol;
      const ASite: INyxPascalBindingSite): TNyxPascalKnowledge;
    function BuiltinType(const ASite: INyxPascalBindingSite;
      const AName: TNyxText; ASystem: Boolean = False): INyxPascalResolution;
    function BindNode(const ASite: INyxPascalBindingSite;
      const AType: INyxPascalTypeSyntax; ANode: Integer): INyxPascalResolution;
    function Bases(const AOwner: INyxPascalBoundSymbol; AOrdinal: Integer): INyxPascalResolution;
    function SameSymbol(const ALeft, ARight: INyxPascalBoundSymbol): Boolean;
    function Unwrap(const AType: INyxPascalSemanticType): INyxPascalSemanticType;
    function GenericNames(const ASymbol: INyxPascalBoundSymbol;
      out ANames: TGenericNames): TNyxPascalBindingIssue;
    function InEnvironment(const ASymbol: INyxPascalBoundSymbol;
      const AEnvironment: INyxPascalGenericEnvironment): INyxPascalBoundSymbol;
    function ParameterAt(const ASite: INyxPascalBindingSite; AScope: Integer;
      const AName: TNyxText): INyxPascalResolution;
    function Specialize(const ASite: INyxPascalBindingSite;
      const AType: INyxPascalTypeSyntax; ANode: Integer): INyxPascalResolution;
    function TypeDeclaration(const ASite: INyxPascalBindingSite;
      const AType: INyxPascalTypeSyntax; ANode: Integer): INyxPascalResolution;
    function TypeOfSymbolCore(const ASymbol: INyxPascalBoundSymbol): INyxPascalResolution;
    function BasesCore(const AOwner: INyxPascalBoundSymbol; AOrdinal: Integer): INyxPascalResolution;
    function CollectMemberNames(const AType: INyxPascalSemanticType;
      const APrefix: TNyxText; var ANames: TGenericNames): INyxPascalResolution;
    function SameEnvironment(const ALeft, ARight: INyxPascalGenericEnvironment): Boolean;
    function SameType(const ALeft, ARight: INyxPascalSemanticType): Boolean;
    function SameBinding(const ALeft, ARight: INyxPascalBoundSymbol): Boolean;
  public
    constructor Create(const AWorld: INyxPascalSemanticWorld;
      const ACancellation: INyxCancellationScope);
    function Lookup(const ASite: INyxPascalBindingSite; const AName: TNyxText): INyxPascalResolution;
    function TypeOfSymbol(const ASymbol: INyxPascalBoundSymbol): INyxPascalResolution;
    function Member(const AType: INyxPascalSemanticType; const ASite: INyxPascalBindingSite;
      const AName: TNyxText; AAccess: TNyxPascalMemberAccess;
      AOperator: TNyxPascalOperatorKind = pokNone): INyxPascalResolution;
    function Members(const AType: INyxPascalSemanticType; const ASite: INyxPascalBindingSite;
      const AQuery: TNyxPascalMemberQuery): INyxPascalMemberPage;
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

function TSemanticWorld.WithModule(const AView: INyxPascalTranslatedSyntax): INyxPascalSemanticWorld;
var
  LCopy: TSemanticWorld;
  LAt: Integer;
  LTarget: Integer;
  LCount: Integer;
  LName: TNyxText;
  LLex: INyxPascalLexicalSnapshot;
  LTokenIndex: Integer;
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
    raise ENyxPascalBinding.Create('A semantic world cannot mix captured compiler families or versions');
  end;
  LTarget := FindModule(LName);
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
  Result := LCopy;
  SetLength(LCopy.FModules, LCount);
  SetLength(LCopy.FInterfaceEnds, LCount);
  for LAt := 0 to LCount - 1 do
  begin

    if LAt = LTarget then
    begin
      LCopy.FModules[LAt] := AView;
      LLex := AView.Syntax.Lexical;
      LCopy.FInterfaceEnds[LAt] := LLex.Scalars;
      for LTokenIndex := 0 to LLex.Count - 1 do
      begin

        if (LLex.Token(LTokenIndex).Kind = ptkWord) and
          (LLex.Token(LTokenIndex).Word = pkwImplementation) and
          not LLex.Token(LTokenIndex).Escaped then
        begin
          LCopy.FInterfaceEnds[LAt] := LLex.Token(LTokenIndex).Span.Start;
          Break;
        end;
      end;
    end
    else
    begin
      LCopy.FModules[LAt] := FModules[LAt];
      LCopy.FInterfaceEnds[LAt] := FInterfaceEnds[LAt];
    end;
    LCopy.FNames.AddFirst(NyxPascalASCIIKey(LCopy.FModules[LAt].Syntax.Scope(0).Name), LAt);
  end;
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
  AIssue: TNyxPascalBindingIssue; const ARelated: TNyxText): INyxPascalResolution;
var
  LResult: TResolution;
begin
  LResult := TResolution.Create;
  Result := LResult;
  LResult.Owner := FWorld;
  LResult.Outcome := AStatus;
  LResult.Reason := AIssue;
  LResult.Related := ARelated;
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

    if (ALeft.Kind <> ARight.Kind) or (ALeft.Builtin <> ARight.Builtin) then
    begin
      Exit(False);
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
    LOwner := LSyntax.Scope(LScope).OwnerDeclaration;

    if (LOwner >= 0) and (LSyntax.Declaration(LOwner).GenericSpan.Length > 0) and
      (FEnvironment = nil) then
    begin
      Exit(Answer(pbsUnresolved, pbiGenericContext));
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
      LClass := CurrentClass(ASite);

      if LClass <> nil then
      begin
        LClassType := TypeOfSymbol(InEnvironment(LClass, FEnvironment));

        if LClassType.Status <> pbsBound then
        begin
          Exit(LClassType);
        end;
        Result := Member(LClassType.BoundType, ASite, AName, pmaInstance);

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
        Exit(Answer(pbsMissing, pbiMissingName));
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

      if LLocal.Count <> 1 then
      begin
        Exit(Answer(pbsUnresolved, pbiOverloadContext));
      end;
      LAccess := pmaInstance;

      if LLocal.SymbolAt(0).Declaration.Kind = psdkType then
      begin
        LAccess := pmaType;
      end;
      LType := TypeOfSymbol(LLocal.SymbolAt(0));

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

      if LLocal.Status <> pbsBound then
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
      'widestring', 'shortstring');
var
  LValue: TNyxPascalBuiltin;
  LType: TSemanticType;
  LContext: INyxPascalCompilerContext;
  LSystem: Integer;
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

    if ASystem then
    begin
      LSystem := FWorld.FindModule('System');

      if LSystem >= 0 then
      begin
        { A terminal System primitive belongs to that unit's source context.
          The consumer or an ObjPas alias must not reassign its mode/identity. }
        LContext := ContextAt(FWorld.At(LSystem, 0, InterfaceEnd(LSystem)));
      end;
    end;

    if (LContext = nil) or (LContext.Family = ppcUnknown) or (LContext.Mode = ppmUnknown) then
    begin
      Exit(Answer(pbsUnresolved, pbiCompilerContext));
    end;
    LType := TSemanticType.Create;
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
    LType.IntrinsicFacts := LContext;
    Result := TypeAnswer(LType);
    Exit;
  end;
end;

function TBindingQuery.GenericNames(const ASymbol: INyxPascalBoundSymbol;
  out ANames: TGenericNames): TNyxPascalBindingIssue;
var
  LLex: INyxPascalLexicalSnapshot;
  LSpan: TNyxPascalSpan;
  LToken: TNyxPascalToken;
  LAt: Integer;
  LSeen: Integer;
  LName: TNyxText;
  LExpectName: Boolean;
begin
  SetLength(ANames, 0);
  LSpan := ASymbol.Declaration.GenericSpan;

  if LSpan.Length = 0 then
  begin
    Exit(pbiNone);
  end;
  LLex := FWorld.ModuleAt(ASymbol.ModuleIndex).Syntax.Lexical;
  LAt := LLex.TokenAt(LSpan.Start);

  if LLex.Token(LAt).Symbol <> psoLess then
  begin
    Exit(pbiGenericContext);
  end;
  Inc(LAt);
  LExpectName := True;
  while LAt < LLex.Count do
  begin
    Tick;
    LToken := LLex.Token(LAt);

    if LToken.Span.Start >= LSpan.Finish then
    begin
      Break;
    end;

    if not (LToken.Kind in [ptkWhitespace, ptkComment, ptkDirective]) then
    begin

      if (LToken.Kind = ptkSymbol) and (LToken.Symbol = psoColon) then
      begin
        Exit(pbiGenericConstraint);
      end;

      if LExpectName then
      begin

        if (LToken.Kind <> ptkWord) or
          ((LToken.Word <> pkwNone) and not LToken.Escaped) then
        begin
          Exit(pbiGenericContext);
        end;
        LName := LLex.Identifier(LAt);
        for LSeen := 0 to High(ANames) do
        begin
          Tick;

          if NyxPascalASCIIKey(ANames[LSeen]) = NyxPascalASCIIKey(LName) then
          begin
            Exit(pbiGenericContext);
          end;
        end;
        SetLength(ANames, Length(ANames) + 1);
        ANames[High(ANames)] := LName;
        LExpectName := False;
      end
      else if (LToken.Kind = ptkSymbol) and (LToken.Symbol = psoComma) then
      begin
        LExpectName := True;
      end
      else if (LToken.Kind = ptkSymbol) and
        (LToken.Symbol in [psoGreater, psoGreaterEqual]) and
        (LToken.Span.Start + 1 = LSpan.Finish) then
      begin
        Exit(pbiNone);
      end
      else
      begin
        Exit(pbiGenericContext);
      end;
    end;
    Inc(LAt);
  end;
  Result := pbiGenericContext;
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

function TBindingQuery.ParameterAt(const ASite: INyxPascalBindingSite;
  AScope: Integer; const AName: TNyxText): INyxPascalResolution;
var
  LOwner: INyxPascalBoundSymbol;
  LNames: TGenericNames;
  LIssue: TNyxPascalBindingIssue;
  LFrame: INyxPascalGenericEnvironment;
  LAt: Integer;
  LDecl: Integer;
begin
  Result := nil;
  LOwner := nil;
  LDecl := FWorld.ModuleAt(ASite.ModuleIndex).Syntax.Scope(AScope).OwnerDeclaration;

  if LDecl >= 0 then
  begin
    LOwner := FWorld.Symbol(ASite.ModuleIndex, LDecl);
  end;
  { Alias expressions and ancestor clauses use the declaration's outer scope,
    yet their placeholders belong to the declaration currently being bound. }

  if (FTypeOwner <> nil) and (FTypeOwner.ModuleIndex = ASite.ModuleIndex) and
    (FTypeOwner.Declaration.Scope = AScope) and
    (FTypeOwner.Declaration.GenericSpan.Length > 0) then
  begin
    LOwner := FTypeOwner;
  end;

  if (LOwner = nil) or (LOwner.Declaration.GenericSpan.Length = 0) then
  begin
    Exit;
  end;
  LIssue := GenericNames(LOwner, LNames);

  if LIssue <> pbiNone then
  begin
    Exit(Answer(pbsUnresolved, LIssue));
  end;
  for LAt := 0 to High(LNames) do
  begin
    Tick;

    if NyxPascalASCIIKey(LNames[LAt]) = NyxPascalASCIIKey(AName) then
    begin
      LFrame := FEnvironment;
      while LFrame <> nil do
      begin
        Tick;

        if SameSymbol(LFrame.Owner, LOwner) then
        begin
          Exit(TypeAnswer(LFrame.Argument(LAt)));
        end;
        LFrame := LFrame.Parent;
      end;
      Exit(Answer(pbsUnresolved, pbiGenericContext));
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
  LNames: TGenericNames;
  LIssue: TNyxPascalBindingIssue;
begin
  Enter;
  try
    CheckSymbol(ASymbol);
    LDecl := ASymbol.Declaration;

    if LDecl.Flags * [psdfIncomplete, psdfConditional, psdfUnsupported] <> [] then
    begin
      Exit(Answer(pbsUnresolved, pbiIncompleteSource));
    end;

    if (LDecl.GenericSpan.Length > 0) then
    begin
      LIssue := GenericNames(ASymbol, LNames);

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
begin
  LResolved := TypeDeclaration(ASite, AType, AType.Child(ANode, ptrBase));

  if LResolved.Status <> pbsBound then
  begin
    Exit(LResolved);
  end;

  if (LResolved.Count <> 1) or (LResolved.SymbolAt(0).Declaration.Kind <> psdkType) then
  begin
    Exit(Answer(pbsUnresolved, pbiGenericContext));
  end;
  LPrototype := LResolved.SymbolAt(0);
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
    LResolved := BindNode(ASite, AType, LChild);

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
  Result := TypeOfSymbol(LSymbolLease);
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

begin
  Enter;
  try
    LNode := AType.Node(ANode);

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
    LType := TSemanticType.Create;
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
      LImplicit := 'IInterface';
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
        LWindow := NyxPascalSpan(LLex.Token(LStart).Span.Start,
          LToken.Span.Start - LLex.Token(LStart).Span.Start);
        LType := NewNyxPascalTypeSyntax(LLex, LWindow, TNyxPascalTypeOptions.Defaults, FCancel);

        if not LType.Complete then
        begin
          Exit(Answer(pbsUnresolved, pbiUnsupportedType));
        end;
        Exit(BindNode(FWorld.At(AOwner.ModuleIndex, LDecl.Scope, LWindow.Start), LType, LType.Root));
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

function TBindingQuery.Member(const AType: INyxPascalSemanticType;
  const ASite: INyxPascalBindingSite; const AName: TNyxText;
  AAccess: TNyxPascalMemberAccess; AOperator: TNyxPascalOperatorKind): INyxPascalResolution;
var
  LType: INyxPascalSemanticType;
  LOwner: INyxPascalBoundSymbol;
  LScope: Integer;
  LFound: INyxPascalResolution;
  LBase: INyxPascalResolution;
  LAt: Integer;
  LCount: Integer;
  LTrailCount: Integer;
  LSeen: Integer;
  LSymbol: INyxPascalBoundSymbol;
  LSymbols: array of INyxPascalBoundSymbol;
  LVisible: TNyxPascalKnowledge;
  LClassAllowed: Boolean;
  LOverload: Boolean;
  LHeader: TNyxPascalToken;
  LSyntax: INyxPascalSyntaxSnapshot;
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
    LTrailCount := Length(FMembers);
    SetLength(FMembers, LTrailCount + 1);
    FMembers[LTrailCount] := LOwner;
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
        LBase := Member(LBase.BoundType, ASite, AName, AAccess, AOperator);

        if LBase.Status in [pbsBound, pbsOverloads] then
        begin
          for LSeen := 0 to LBase.Count - 1 do
          begin
            LSymbol := LBase.SymbolAt(LSeen);
            LVisible := ppkTrue;
            for LScope := 0 to LCount - 1 do
            begin

              if SameBinding(LSymbols[LScope], LSymbol) then
              begin
                LVisible := ppkFalse;
              end;
            end;

            if LVisible = ppkTrue then
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
begin
  Enter;
  try
    LType := Unwrap(AType);

    if (LType <> nil) and (LType.Kind = pstClassOf) then
    begin
      LType := Unwrap(LType.Target);
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

      if LSyntax.Declaration(LDeclaration).OperatorKind <> pokNone then
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
  Result := LPage;
  LPage.Following := AQuery.Offset;
  LPage.TotalValue := -1;
  LFound := CollectMemberNames(AType, NyxPascalASCIIKey(AQuery.Prefix), LNames);

  if LFound <> nil then
  begin
    LPage.Reply := Answer(LFound.Status, LFound.Issue, LFound.RelatedName);
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
end.
