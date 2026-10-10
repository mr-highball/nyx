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

unit nyx.source;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  SysUtils,
  nyx.text,
  nyx.dates,
  nyx.times,
  nyx.colors,
  nyx.images,
  nyx.bytes,
  nyx.resources,
  nyx.resources.rows,
  nyx.resource.sources,
  nyx.types,
  nyx.layout.policy,
  nyx.responsive,
  nyx.presentations,
  nyx.containers,
  nyx.layout.constraints,
  nyx.callbacks,
  nyx.model;

const
  NyxViewsBegin = '// <nyx:views>';
  NyxViewsEnd = '// </nyx:views>';

type
  { Open Pascal routine identity. Qualified methods and ordinary functions use
    compiler-style case-insensitive names; offsets never identify an edit. }
  TNyxRoutineRef = record
  private
    FName: TNyxText;
  public
    property Name: TNyxText read FName;
  end;
  TNyxRoutineKind = (nrProcedure, nrFunction, nrConstructor, nrDestructor);
  { Unit helper visibility is independent of target and class-member access. }
  TNyxRoutineVisibility = (rvInterface, rvImplementation);
  TNyxDeclarationPart = (dspInterface, dspImplementation);

  { An admitted standalone procedure/function definition. Signature and body
    are explicit Pascal source boundaries; kind/identity/visibility are typed.
    Creation never modifies an existing class or compiler-managed signature. }
  TNyxRoutineDeclaration = record
  private
    FRoutine: TNyxRoutineRef;
    FKind: TNyxRoutineKind;
    FVisibility: TNyxRoutineVisibility;
    FSignature: TNyxText;
    FImplementation: TNyxText;
  public
    property Routine: TNyxRoutineRef read FRoutine;
    property Kind: TNyxRoutineKind read FKind;
    property Visibility: TNyxRoutineVisibility read FVisibility;
    property Signature: TNyxText read FSignature;
    property Code: TNyxText read FImplementation;
  end;

  { Immutable counterpart inspection for one unit helper. Declaration is exact
    interface text through its semicolon, or empty for an implementation-only
    helper. Line is one-based; zero means there is no interface declaration.
    Private spans only belong to the inspected source snapshot. }
  TNyxRoutineDeclarationSource = record
  private
    FVisibility: TNyxRoutineVisibility;
    FSignature: TNyxText;
    FLine: Integer;
    FImplementationSignature: TNyxText;
    FImplementationLine: Integer;
    FStart: Integer;
    FFinish: Integer;
  public
    property Visibility: TNyxRoutineVisibility read FVisibility;
    { Typed counterpart access supports bounded signature windows independently
      of body size; an unknown part refuses instead of selecting a default. }
    function Text(APart: TNyxDeclarationPart): TNyxText;
    function SourceLine(APart: TNyxDeclarationPart): Integer;
    property Declaration: TNyxText read FSignature;
    property Line: Integer read FLine;
  end;

  { An immutable lexical implementation and its exact signature. Code begins
    after the signature semicolon and ends at the implementation semicolon.
    It includes local declarations/nested routines. Noneditable entries explain
    conditional/overload/generated ownership; they remain useful for discovery.
    Owned text outlives the source/session. Private offsets never escape. }
  TNyxRoutineSource = record
  private
    FRoutine: TNyxRoutineRef;
    FKind: TNyxRoutineKind;
    FSignature: TNyxText;
    FImplementation: TNyxText;
    FLine: Integer;
    FStart: Integer;
    FHeaderFinish: Integer;
    FFinish: Integer;
    FEditable: Boolean;
    FReason: TNyxText;
  public
    property Routine: TNyxRoutineRef read FRoutine;
    property Kind: TNyxRoutineKind read FKind;
    property Signature: TNyxText read FSignature;
    property Code: TNyxText read FImplementation;
    property Line: Integer read FLine;
    property Editable: Boolean read FEditable;
    property Reason: TNyxText read FReason;
  end;

  { Immutable declaration-order discovery. Nested routines belong to their
    enclosing implementation and never become independent edit targets. }
  TNyxRoutineCatalog = record
  private
    FEntries: array of TNyxRoutineSource;
    function GetCount: Integer;
  public
    function Item(AIndex: Integer): TNyxRoutineSource;
    property Count: Integer read GetCount;
  end;

  { Import sections are Pascal visibility choices, independent of output target. }
  TNyxImportSection = (nisInterface, nisImplementation);
  TNyxImportAction = (niaAdd, niaRemove);

  { Distinct open Pascal namespace reference. Construction validates ASCII
    namespace syntax; identity is case insensitive, like the compiler. }
  TNyxPascalUnitRef = record
  private
    FName: TNyxText;
  public
    property Name: TNyxText read FName;
  end;

  { Private lexical offsets belong only to their exact source snapshot. They
    are never wire coordinates or mutable editor identities. }
  TNyxImportSpan = record
  private
    FStart: Integer;
    FFinish: Integer;
  end;
  TNyxImportSource = record
  private
    FUnit: TNyxPascalUnitRef;
    FLine: Integer;
    FParts: array of TNyxImportSpan;
  end;

  { Owned immutable lexical clause. Ordinary comments/order survive editing;
    conditional/directive clauses and duplicate names refuse. An absent uses
    clause is a defined empty section and can receive its first import. }
  TNyxImportClause = record
  private
    FEntries: array of TNyxImportSource;
    FCommas: array of TNyxImportSpan;
    FUses: TNyxImportSpan;
    FEnd: TNyxImportSpan;
    FAnchor: Integer;
    function GetCount: Integer;
  public
    { Zero-based access returns owned names; out-of-range indices refuse.
      LineAt is a one-based navigation site in the exact accepted source. }
    function UnitAt(AIndex: Integer): TNyxPascalUnitRef;
    function LineAt(AIndex: Integer): Integer;
    property Count: Integer read GetCount;
  end;

  { Detached diagnostic for one exact editable source snapshot. Line/column are
    one-based Unicode-scalar coordinates; zero means the admission error has no
    trustworthy source site. UI code must never invent a location for it. }
  TNyxSourceDiagnostic = record
    Defined: Boolean;
    Message: TNyxText;
    Line: Integer;
    Column: Integer;
  end;

  { Explicit identity migration accompanies a visual state rename. Names are
    ordinary exact application keys; an empty key is valid and is not a sentinel. }
  TNyxSourceStateRename = record
    OldName: TNyxText;
    NewName: TNyxText;
  end;

  { An immutable lexical view of one ordinary Pascal Invoke implementation.
    Signature retains the exact procedure declaration through its semicolon;
    Code retains the following whitespace, local declarations and
    complete body through end;. Positions are private native/target offsets and
    never used as client identities. Line is a one-based source navigation site.
    Text survives source/workspace disposal and borrows no document or lexer. }
  TNyxHandlerSource = record
  private
    FHandler: TNyxHandlerRef;
    FSignature: TNyxText;
    FImplementation: TNyxText;
    FLine: Integer;
    FStart: Integer;
    FHeaderFinish: Integer;
    FFinish: Integer;
  public
    property Handler: TNyxHandlerRef read FHandler;
    property Signature: TNyxText read FSignature;
    property Code: TNyxText read FImplementation;
    property Line: Integer read FLine;
  end;

  {$ifdef NYX_SOURCE_PROFILE}
  { Opt-in benchmark instrumentation only: production builds contain no clock,
    counters or observer. The single-threaded harness supplies a monotonic clock;
    nested stage totals overlap and must not be added as exclusive costs. }
  TNyxSourceProfileStage = (spOldGeneration, spSymbols, spNames, spLegacy,
    spPrune, spScaffold, spPartition, spMetadataSplit, spMetadataMerge,
    spFrameMerge, spVerify, spLex, spSymbolRead, spCheckpoint, spCommit,
    spSave, spRestoreDocument, spSnapshot, spRestoreWorkspace, spRender,
    spRenderEncode, spCandidate, spCandidateValidate, spCandidateEncode);
  TNyxSourceProfileClock = function: Double;
  TNyxSourceProfileSample = record
    Milliseconds: Double;
    Calls: Integer;
  end;
  TNyxSourceProfileSamples = array[TNyxSourceProfileStage] of TNyxSourceProfileSample;
  {$endif}

  { Syntax and admission diagnostics belong to the user's retained draft. Line
    and column are one-based Unicode positions in the complete Pascal file. }
  ENyxSource = class(Exception)
  private
    FLine: Integer;
    FColumn: Integer;
    FDiagnosticText: TNyxText;
  public
    constructor CreateAt(const AMessage: TNyxText; const ASource: TNyxText;
      APosition: Integer);
    property Line: Integer read FLine;
    property Column: Integer read FColumn;
    { Exact portable text avoids native Exception.Message's ANSI boundary. }
    property DiagnosticText: TNyxText read FDiagnosticText;
  end;

  { A complete executed unit can contain ordinary Pascal beyond the declarative
    builder grammar. This origin is admission/lifetime metadata, not a target or
    a client claim that a compiler ran. Literal imports retain their own parser. }
  TNyxSourceOrigin = (nsoDeclarative, nsoExecuted);

  { Raised before replacing executed source when a visual change needs the
    compiler-aware reconciliation path. Retaining the constructor is mandatory;
    generated literals cannot stand in for its helper/control-flow meaning. }
  ENyxSourceExecutionRequired = class(ENyxSource);

  { Immutable in-memory companion value. Only a workspace captures these fields;
    callers cannot supply replacement source/design members. Managed text values
    survive workspace disposal independently and retain no document or reader.
    Design is the exact canonical document paired with this frame. StorageBytes
    counts retained text buffers (UTF-8 bytes natively, UTF-16 bytes in pas2js),
    without serializing strings into another escaped JSON representation. JSON
    Snapshot/Restore remain the explicit recovery/interchange boundary. }
  TNyxSourceCheckpoint = record
  private
    FPrefix: TNyxText;
    FBody: TNyxText;
    FSuffix: TNyxText;
    FDesign: TNyxText;
    FCustomFrame: Boolean;
    FOrigin: TNyxSourceOrigin;
    function GetStorageBytes: TNyxTextBytes;
    function GetSource: TNyxText;
  public
    property Design: TNyxText read FDesign;
    { Exact authored companion, assembled without parsing or regeneration. This
      read-only value supports native recovery of already admitted history. }
    property Source: TNyxText read GetSource;
    property StorageBytes: TNyxTextBytes read GetStorageBytes;
    property Origin: TNyxSourceOrigin read FOrigin;
  end;

  { An owned companion to the design, independent of a target compiler.
    For declarative origin, the delimited BuildNyxDocument builder is synchronized;
    Pascal helpers and
    imports outside it are retained exactly. The declarative edit reader admits
    typed control/reference declarations, specialized factories/class constructors,
    default compound recipes, root/child ownership, Title, Configure, defaults,
    bindings, scalar domains and structured extension construction. Unsupported
    executable builder syntax fails explicitly instead of guessing at Pascal.

    Candidate returns an independently owned document. Candidate/PrepareCandidate
    synchronize any direct public edits to the current baseline first; a rejected
    draft never replaces that pair. Accept stages a companion for a tree already
    admitted by its caller. PrepareCandidate stages both owners before publication.
    Render reconciles generated changes against the accepted builder. Deliberate
    control/state locals, comments and unchanged typed expressions are retained;
    removed declarations leave their notes as comments. An authored result must
    reconstruct the entire candidate before its frame is published. Unsupported
    overlap or the bounded edit budget fails without replacing the accepted pair.
    A rejected draft is never substituted for
    accepted source. Executed origin instead retains the complete exact unit and
    its admitted design; Render refuses changed meaning until a compiler-aware
    writer is supplied. Snapshot/Restore carry already admitted companions through
    history. Restoring metadata is not source execution or new project admission;
    portable .nyx and .pas exports remain separate files. }
  TNyxSourceWorkspace = class
  private
    FPrefix: TNyxText;
    FBody: TNyxText;
    FSuffix: TNyxText;
    FDesign: TNyxText;
    FCustomFrame: Boolean;
    FOrigin: TNyxSourceOrigin;
    { Derived canonical builder for FDesign only. It owns text, never a model or
      reader. Recovery/history deliberately omit this disposable workspace cache;
      Restore clears it, and every Render still freshly encodes the public tree. }
    FGeneratedBody: TNyxText;
    procedure StageAccepted(ADocument: TNyxDocument; const APrefix, ABody,
      ASuffix, ADesign: TNyxText);
  public
    { Admit exact source without borrowing an accepted document or workspace.
      Intended for isolated processors: all factories, tree validation and
      companion preparation belong to this invocation. Both outputs are owned;
      failure releases both. Publication still requires an editor baseline guard. }
    class function PrepareDraft(const ADraft: TNyxText;
      out AWorkspace: TNyxSourceWorkspace): TNyxDocument; static;
    function Render(ADocument: TNyxDocument): TNyxText; overload;
    function Render(ADocument: TNyxDocument;
      const ARenames: array of TNyxSourceStateRename): TNyxText; overload;
    { Replays a fresh independently owned document through the public typed
      builder grammar. Declaration/construction/ownership edits may change its
      shape; wrong types, stale references, duplicate owners and unsupported
      control flow fail before publication. ACompanionComparison is retained for
      existing callers; source editing and compiler verification now share the
      same reconstruction rules, including named versus inline references. }
    function Candidate(ADocument: TNyxDocument; const ADraft: TNyxText;
      ACompanionComparison: Boolean = False): TNyxDocument;
    { Admit one exact draft and return its independently owned document and
      companion together. Parses/reconstructs/validates the complete draft once;
      no accepted owner changes. On failure both outputs are released and the
      companion is nil. The caller owns both successful outputs and must publish
      them together without intervening model mutation. This is the session's
      source Apply path, not permission to skip ordinary compiler diagnostics. }
    function PrepareCandidate(ADocument: TNyxDocument; const ADraft: TNyxText;
      out AWorkspace: TNyxSourceWorkspace): TNyxDocument;
    procedure Accept(ADocument: TNyxDocument; const ASource: TNyxText);
    { Trusted external-construction admission. Caller supplies an actually
      executed, fully validated independent document and its exact complete unit.
      No fluent replay or managed markers are required. This does not execute
      source or authorize a file/HTTP/MCP import. Both values stage before swap.
      Until compiler-aware reconciliation is supplied, changed visual meaning
      raises ENyxSourceExecutionRequired instead of discarding Pascal. }
    procedure AcceptExecuted(ADocument: TNyxDocument; const ASource: TNyxText);
    property Origin: TNyxSourceOrigin read FOrigin;
    procedure Reset;
    { Capture without a document copies the accepted frame only. The document
      overload renders first, detecting public out-of-band edits through a fresh
      canonical encoding. Both return immutable values, never borrowed handles. }
    function Capture: TNyxSourceCheckpoint; overload;
    function Capture(ADocument: TNyxDocument): TNyxSourceCheckpoint; overload;
    function Snapshot: TNyxText;
    procedure Restore(const ASnapshot: TNyxText); overload;
    { Restore only an already-captured in-memory value. No JSON parsing is needed;
      the receiving owner gets independent fields. This is not source admission. }
    procedure Restore(const ACheckpoint: TNyxSourceCheckpoint); overload;
  end;

{ Compiler companions retain their Pascal namespace. Admission validates Unicode,
  the unit header, Delphi/UTF-8 settings and external-file compiler references
  before a service derives its confined filename. Helpers remain ordinary Pascal;
  their syntax/type errors belong to compiler diagnostics, not the design reader. }
function NyxCompanionUnitName(const ASource: TNyxText): TNyxText;
{ Typed semantic import editing borrows no session/model. All surrounding source
  and ordinary comments retain their exact bytes. Add requires absence; Remove
  requires presence in the exact section. File clauses and ambiguous ownership
  refuse. This is lexical admission, not unit resolution or compiler success. }
function NyxPascalUnit(const AName: TNyxText): TNyxPascalUnitRef;
{ Discover up to 4096 complete top-level implementations outside interface type
  declarations. Refuse malformed lexical boundaries. Read resolves one unique
  name; Replace additionally requires exact expected text and editable ownership.
  Signatures, surrounding comments and the managed builder are never replaced.
  Expression/type correctness remains the ordinary compiler's responsibility. }
function NyxRoutine(const AName: TNyxText): TNyxRoutineRef;
function ReadNyxRoutines(const ASource: TNyxText): TNyxRoutineCatalog;
function ReadNyxRoutineSource(const ASource: TNyxText;
  const ARoutine: TNyxRoutineRef): TNyxRoutineSource;
function ReplaceNyxRoutineImplementation(const ASource: TNyxText;
  const ARoutine: TNyxRoutineRef; const AExpected, AImplementation: TNyxText): TNyxText;
{ Create/remove ordinary unit helpers with exact paired visibility ownership.
  Removal requires exact implementation signature/body and interface counterpart
  (empty for private). Any possible retained lexical reference blocks removal;
  external-unit references and type correctness remain compiler diagnostics. }
function NyxRoutineDeclaration(AKind: TNyxRoutineKind; const ARoutine: TNyxRoutineRef;
  AVisibility: TNyxRoutineVisibility; const ASignature, AImplementation: TNyxText): TNyxRoutineDeclaration;
function ReadNyxRoutineDeclaration(const ASource: TNyxText;
  const ARoutine: TNyxRoutineRef): TNyxRoutineDeclarationSource;
function AddNyxRoutineDeclaration(const ASource: TNyxText;
  const ADeclaration: TNyxRoutineDeclaration): TNyxText;
function RemoveNyxRoutineDeclaration(const ASource: TNyxText;
  const ARoutine: TNyxRoutineRef;
  const AExpectedSignature, AExpectedImplementation, AExpectedDeclaration: TNyxText): TNyxText;
{ Replace a free unit helper's signature/body and public counterpart together.
  Identity and visibility stay retained. Every current counterpart must exactly
  match the supplied acknowledgement; conditional/overloaded/managed ownership
  refuses. The replacement owns its explicitly supplied Pascal fragments.
  Caller edits belong in the same group; compiler diagnostics establish their
  type compatibility, including callers outside this unit. }
function ReplaceNyxRoutineDeclaration(const ASource: TNyxText;
  const AReplacement: TNyxRoutineDeclaration;
  const AExpectedSignature, AExpectedImplementation, AExpectedDeclaration: TNyxText): TNyxText;

function ReadNyxImports(const ASource: TNyxText; ASection: TNyxImportSection): TNyxImportClause;
function EditNyxImport(const ASource: TNyxText; ASection: TNyxImportSection;
  AAction: TNyxImportAction; const AUnit: TNyxPascalUnitRef): TNyxText;
function NyxSourceStateRename(const AOldName, ANewName: TNyxText): TNyxSourceStateRename;
{ Returns independently owned text, never mutates either design/workspace. The
  builder must reproduce ADocument through supported typed admission. Full builds
  retain exact source; isolated builds reconcile only the delimited builder and
  verify that it reconstructs ABuiltDocument. }
function PrepareNyxCompanion(ADocument, ABuiltDocument: TNyxDocument;
  const ASource: TNyxText; AIsolated: Boolean): TNyxText;
{ Adds one ordinary Pascal callback class/registration outside managed views.
  Existing handwritten code is retained. The returned line identifies its TODO
  body for source navigation; unsupported conditional initialization layouts
  fail before modifying source/design/history. }
function AddNyxHandlerStub(const ASource: TNyxText; const AHandler: TNyxHandlerRef;
  ATrigger: TNyxTrigger; out ALine: Integer): TNyxText; overload;
function AddNyxHandlerStub(const ASource: TNyxText; const AHandler: TNyxHandlerRef;
  const AName: TNyxEventRef; out ALine: Integer): TNyxText; overload;
{ Find a real qualified Invoke implementation, ignoring comments/string literals
  and allowing ordinary Pascal whitespace and casing. Returns its one-based
  procedure line; missing/malformed source raises an owned source diagnostic. }
function NyxHandlerSourceLine(const ASource: TNyxText;
  const AHandler: TNyxHandlerRef): Integer;

{ Read/replace one uniquely qualified local callback, outside managed views.
  Replacement retains its exact signature and every surrounding byte. Expected
  is the complete implementation returned by Read, including local declarations
  and whitespace; mismatch refuses. Nested routines and ordinary Pascal blocks
  are balanced lexically, ignoring strings/comments. Conditional/directive bodies,
  duplicates and ambiguous boundaries refuse rather than guessing ownership.
  This locates a region, not a Pascal type checker: the normal compiler diagnoses
  helper syntax/type errors. The caller must still admit the complete candidate. }
function ReadNyxHandlerSource(const ASource: TNyxText;
  const AHandler: TNyxHandlerRef): TNyxHandlerSource;
function ReplaceNyxHandlerImplementation(const ASource: TNyxText;
  const AHandler: TNyxHandlerRef; const AExpected, AImplementation: TNyxText): TNyxText;

{ Return independently retained exact source regions through the same lexical
  boundary admission as the workspace. Position mapping may reuse unchanged
  prefix/helper regions after an isolated builder changes length. No output is
  published until both unique markers and their ordering have been admitted. }
procedure SplitNyxSourceFrame(const ASource: TNyxText;
  out APrefix, ABuilder, ASuffix: TNyxText);

{$ifdef NYX_SOURCE_PROFILE}
{ Benchmark owns the clock and samples. No synchronization is provided: never
  enable this diagnostic define in a concurrently serving application. }
procedure ResetNyxSourceProfile(AClock: TNyxSourceProfileClock);
function ReadNyxSourceProfile: TNyxSourceProfileSamples;
{ Shared command instrumentation uses the same opt-in clock. Parent samples
  include their children; these diagnostics never alter admission or ownership. }
function SourceProfileStart: Double;
procedure SourceProfileFinish(AStage: TNyxSourceProfileStage; AStarted: Double);
{$endif}

implementation

uses
  nyx.codegen,
  nyx.text.index,
  nyx.codec,
  nyx.data,
  nyx.state,
  nyx.collections,
  nyx.collections.registry,
  nyx.collections.view.types,
  nyx.collections.query,
  nyx.collections.selection,
  nyx.binding.types,
  nyx.contract,
  nyx.scheduler,
  nyx.json,
  nyx.schema,
  nyx.catalog,
  nyx.content,
  nyx.menu.declarations,
  nyx.menu.bar.declarations,
  nyx.menu.types,
  nyx.popover.types,
  nyx.typeahead,
  nyx.root.types,
  nyx.controls,
  nyx.design.tokens;

type
  { Source tables use the same exact-text lookup as fresh document admission.
    The alias remains private; ordered source arrays still own output order. }
  TSourceIndex = TNyxTextIndex;

{$ifdef NYX_SOURCE_PROFILE}
var
  GSourceProfileClock: TNyxSourceProfileClock;
  GSourceProfileSamples: TNyxSourceProfileSamples;

procedure ResetNyxSourceProfile(AClock: TNyxSourceProfileClock);
var
  LStage: TNyxSourceProfileStage;
begin
  GSourceProfileClock := AClock;
  for LStage := Low(TNyxSourceProfileStage) to High(TNyxSourceProfileStage) do
  begin
    GSourceProfileSamples[LStage].Milliseconds := 0;
    GSourceProfileSamples[LStage].Calls := 0;
  end;
end;

function ReadNyxSourceProfile: TNyxSourceProfileSamples;
begin
  Result := GSourceProfileSamples;
end;

function SourceProfileStart: Double;
begin
  Result := 0;

  if Assigned(GSourceProfileClock) then
  begin
    Result := GSourceProfileClock();
  end;
end;

procedure SourceProfileFinish(AStage: TNyxSourceProfileStage; AStarted: Double);
begin

  if Assigned(GSourceProfileClock) then
  begin
    GSourceProfileSamples[AStage].Milliseconds :=
      GSourceProfileSamples[AStage].Milliseconds + GSourceProfileClock() - AStarted;
    Inc(GSourceProfileSamples[AStage].Calls);
  end;
end;
{$endif}

type
  TTokenKind = (tkWord, tkString, tkNumber, tkCharacter, tkSymbol, tkComment);
  TToken = record
    Kind: TTokenKind;
    Text: TNyxText;
    Position: Integer;
    Finish: Integer;
  end;
  TTokens = array of TToken;

  { Tags enforce the same argument families as the public fluent API. Text at
    this lexical boundary never stands in for a layout enum or part reference. }
  TValueKind = (vkText, vkInteger, vkNumber, vkBoolean, vkKind, vkAttribute,
    vkLayout, vkVariant, vkAction, vkOverride, vkInput, vkPart, vkEvent,
    vkComponent, vkStyle, vkCustomKind, vkStateRef, vkBindingProperty,
    vkBindingDirection, vkDomain, vkTrigger, vkEventSource, vkExtension,
    vkData, vkDataField, vkDecimal, vkPolicy, vkHandler, vkCallbackID,
    vkConstruction, vkCollectionKey, vkCollectionField, vkCollectionItemRef,
    vkCollectionSchema, vkCollectionItem, vkNoDomain, vkScalarValue,
    vkCollectionView, vkCollectionScope, vkCollectionCellMode, vkSelectionMode,
    vkCollectionQuery, vkCollectionPredicate, vkCollectionSort, vkQueryField,
    vkSortDirection, vkQueryTextComparison, vkPlatform,
    vkSplitOrientation, vkSemanticEvent, vkTouchBehavior, vkFlowWrap,
    vkCrossAlignment, vkJustification, vkSizing, vkLayoutPolicy, vkSizeRange,
    vkSizeConstraints, vkViewportWidth, vkViewportCondition, vkViewportOrientation,
    vkPresentationRef, vkPresentationCondition, vkContainerRef, vkContainerContainment,
    vkCalendarDate, vkClockTime, vkRGBColor, vkTimePrecision, vkClockDomain, vkValueDomain,
    vkMenuRef, vkMenuCommand, vkMenuGroup, vkRootRef,
    vkMenuDefinition, vkMenuOptions, vkMenuBarDefinition, vkMenuBarOptions,
    vkPopoverOptions, vkTypeAheadOptions,
    vkMenuOpening, vkPopoverSide, vkPopoverAlignment, vkPopoverSizing,
    vkPopoverDismissal, vkTypeAheadMatch, vkThemeTokens, vkThemePreset,
    vkImageSource, vkImageLocation, vkImageFormat, vkImageFit, vkImageAnchor, vkImageValidation,
    vkResourceRef, vkResourceLocale, vkResourceLabel, vkResourceLabels,
    vkResourceDefinition, vkBytes, vkResourceValue,
    vkResourceKind, vkResourceURL, vkResourceCache, vkResourceServerPolicy,
    vkResourceRows, vkResourcePath, vkResourceImage);
  TValue = record
    Kind: TValueKind;
    Text: TNyxText;
    Ordinal: Integer;
    { Immutable public values retain their distinct Pascal families while the
      parser evaluates nested constructors. Only the tagged member is consumed;
      no borrowed JSON container or domain builder survives a reader. }
    Data: TNyxDataValue;
    TextDomain: TNyxTextDomain;
    BooleanDomain: TNyxBooleanDomain;
    IntegerDomain: TNyxIntegerDomain;
    NumberDomain: TNyxNumberDomain;
    EventSource: TNyxEventValueRef;
    CollectionSchema: TNyxCollectionSchema;
    CollectionItem: TNyxCollectionItem;
    CollectionItemRef: TNyxItemRef;
    ScalarValue: TNyxStateValue;
    CollectionView: TNyxCollectionViewSpec;
    CollectionQuery: TNyxCollectionQuery;
    CollectionPredicateData: TNyxDataValue;
    CollectionSort: TNyxCollectionSort;
    LayoutPolicy: TNyxLayoutPolicy;
    SizeRange: TNyxSizeRange;
    SizeConstraints: TNyxSizeConstraints;
    ViewportWidth: TNyxViewportWidth;
    ViewportCondition: TNyxViewportCondition;
    PresentationCondition: TNyxPresentationCondition;
    CalendarDate: TNyxCalendarDate;
    ClockTime: TNyxClockTime;
    RGBColor: TNyxRGBColor;
    ImageSource: TNyxImageSource;
    ImageValidation: TNyxImageValidationPolicy;
    ResourceDefinitionData: TNyxDataValue;
    ResourceValue: TNyxResourceValueRef;
    ResourceImage: TNyxResourceImageRef;
    ResourceRows: TNyxResourceRows;
    ResourcePath: TNyxResourcePath;
    ResourceCache: TNyxResourceCachePolicy;
    Bytes: TNyxBytes;
    ThemeTokens: TNyxThemeTokens;
    TimeDomain: TNyxTimeDomain;
    ValueDomain: TNyxValueDomain;
    RootRef: TNyxRootRef;
    MenuOptions: TNyxMenuOptions;
    MenuBarOptions: TNyxMenuBarOptions;
    PopoverOptions: TNyxPopoverOptions;
    TypeAheadOptions: TNyxTypeAheadOptions;
  end;
  TValues = array of TValue;
  { Closed authoring symbols carry their exact argument family and ordinal.
    These immutable scalar facts contain no managed application value. }
  TSourceEnumValue = record
    Kind: TValueKind;
    Ordinal: Integer;
  end;
  TControlLocal = record
    Name: TNyxText;
    ID: TNyxText;
    DeclaredType: TNyxText;
    Configured: Boolean;
    ContentConfigured: Boolean;
    Admitted: Boolean;
    Created: Boolean;
  end;
  TControlLocals = array of TControlLocal;
  { Declared Pascal reference types survive independently of their initialized
    key names. Lookup refuses use before assignment and factory/type mismatch. }
  TStateLocal = record
    Name: TNyxText;
    Kind: TNyxStateKind;
    Key: TNyxText;
    Assigned: Boolean;
  end;
  TStateLocals = array of TStateLocal;

  { Reader state is short-lived. Reconstructed controls are retained until the
    borrowed candidate document owns them, including every failure path.
    Lexical readers used for reconciliation create no controls. Keeping tokens
    and local mappings per reader isolates each import from the next draft. }
  TConfigurationReader = class
  private
    FSource: TNyxText;
    FTokens: TTokens;
    FCursor: Integer;
    FDepth: Integer;
    FLocals: TControlLocals;
    FStates: TStateLocals;
    { Per-reader indexes own text; arrays retain the original declaration order.
      Failure releases indexes together with staged controls. }
    FLocalNames: TSourceIndex;
    FStateNames: TSourceIndex;
    FControlIDs: TSourceIndex;
    { Reconstructed controls remain retained even between creation and adoption.
      Failure before an ownership call therefore releases every staged object. }
    FControls: array of INyxControl;
    { pas2js cannot place COM interfaces inside records. Tagged expression
      ordinals address this reader-owned array of immutable menu plans instead.
      No index survives source admission or refers to a target/runtime host. }
    FMenuDefinitions: array of INyxMenuDefinition;
    FMenuBarDefinitions: array of INyxMenuBarDefinition;
    { Default recipe blueprints are reader-owned. An isolated source processor
      must never enter the authoring factories' lazily shared UI registry. }
    FRecipeCatalog: TNyxCatalog;
    FDocument: TNyxDocument;
    FApply: Boolean;
    FTitleSeen: Boolean;
    FOwnedTry: Boolean;
    procedure Fail(const AMessage: TNyxText);
    function At(const AText: TNyxText): Boolean;
    procedure Expect(const AText: TNyxText);
    function Expression: TValue;
    function Primary: TValue;
    function Arguments(AMaximum: Integer = 3): TValues;
    function ArrayArguments(AMaximum: Integer): TValues;
    function Numeric(const AValue: TValue): Double;
    function Domain(AKind: TNyxStateKind; ACalendar: Boolean = False;
      ARGB: Boolean = False): TValue;
    { Closed clock constructors retain exact readings/precision and distinct
      domains. No locale text, callback execution or renderer state is evaluated. }
    function ClockValue(const AName: TNyxText): TValue;
    function ClockDomain: TValue;
    function ThemeTokens(const AName: TNyxText): TValue;
    function DataConstructor(const AName: TNyxText): TValue;
    function LocalIndex(const AName: TNyxText): Integer;
    function StateIndex(const AName: TNyxText): Integer;
    { Borrow the exact constructed local after its ownership statement. The
      retained interface fixes this reference for the reader's lifetime; no
      document-wide index or mutable model facts survive candidate replay. }
    function AdmittedNode(AIndex: Integer): TNyxNode;
    procedure ReferenceAssignment(AIndex: Integer);
    procedure Defaults;
    function CollectionConstructor(const AName: TNyxText): TValue;
    function QueryConstructor(const AName: TNyxText): TValue;
    { Declarative menu expressions retain immutable plans, never runtime hosts. }
    function MenuConstructor(const AName: TNyxText): TValue;
    procedure MenuDefaults;
    { Declarative resource values contain copied file data/help, never imports
      from a machine path or mutable runtime state/control handles. }
    function ResourceConstructor(const AName: TNyxText): TValue;
    procedure ResourceDefaults;
    function ResourceRowsConstructor(const AName: TNyxText): TValue;
    procedure ResourceCollectionDefaults;
    function CollectionScalar(const AValue: TValue; AKind: TNyxStateKind): TNyxStateValue;
    procedure PresentationDefaults;
    procedure CollectionDefaults;
    procedure Bindings(AIndex: Integer);
    procedure Contract(AIndex: Integer);
    procedure Extensions(AIndex: Integer);
    procedure Callbacks;
    procedure Configure(AIndex: Integer);
    procedure Content(AIndex: Integer);
    procedure ApplyCall(ANode: TNyxNode; const AMethod: TNyxText;
      const AArgs: TValues; APlatform: TNyxPlatform;
      const AViewport: TNyxViewportCondition; const APresentation: TNyxPresentationRef);
    procedure Declarations;
    procedure ConstructControl(AIndex: Integer);
    procedure OwnControl(AIndex: Integer; const AMethod: TNyxText);
    procedure BuilderStatement;
  public
    constructor Create(const ASource: TNyxText; const ATokens: TTokens;
      const ALocals: TControlLocals; ADocument: TNyxDocument; AApply: Boolean;
      AReconstruct: Boolean = False);
    destructor Destroy; override;
    { Strict replay of the public declarative builder into a fresh borrowed
      document. No arbitrary Pascal execution or accepted-tree mutation occurs. }
    procedure Reconstruct;
  end;

const
  CStateFactories: array[TNyxStateKind] of TNyxText =
    ('NyxTextState', 'NyxBooleanState', 'NyxIntegerState', 'NyxNumberState');
  CStateTypes: array[TNyxStateKind] of TNyxText =
    ('TNyxTextStateRef', 'TNyxBooleanStateRef', 'TNyxIntegerStateRef', 'TNyxNumberStateRef');
  CBindingMethods: array[TNyxBindingProperty] of TNyxText = (
    'Text', 'Value', 'Enabled', 'Visible', 'ReadOnly', 'Pressed', 'Placeholder',
    'Hint', 'AccessibleName', 'Width', 'Height', 'Left', 'Top', 'Padding', 'Gap',
    'Columns', 'Flex', 'Minimum', 'Maximum', 'Image');
  CMethods: array[TNyxAttribute] of TNyxText = (
    'Text', 'Value', 'Placeholder', 'Items', 'Hint', 'AccessibleName',
    'LinkTo', 'Source', 'AlternativeText', 'Layout', 'Padding', 'Gap',
    'Columns', 'Width', 'Height', 'Left', 'Top', 'Flex', 'Minimum', 'Maximum',
    'Enabled', 'Visible', 'ReadOnly', 'Surface', 'Compound', 'Pressed',
    'Variant', 'Action', 'ProjectAs', 'OverrideMode', 'InputType', 'PartName',
    'Target', 'Component', 'OnClick', 'OnChange', 'Option', 'OverridePath', '',
    'SplitOrientation', 'SplitPosition', 'SplitMinimum', 'SplitMaximum', 'SplitResizable',
    'DragSource', 'DropTarget', 'TouchBehavior', 'Wrap', 'Align', 'Justify',
    'WidthSizing', 'HeightSizing', 'MinimumWidth', 'MaximumWidth',
    'MinimumHeight', 'MaximumHeight', 'QueryContainer', 'Containment', 'SliderIntervals',
    'ImageFit', 'ImageHorizontal', 'ImageVertical');
  CAttributes: array[TNyxAttribute] of TNyxText = (
    'atText', 'atValue', 'atPlaceholder', 'atItems', 'atHint', 'atAccessibleName',
    'atHref', 'atSource', 'atAlt', 'atLayout', 'atPadding', 'atGap', 'atColumns',
    'atWidth', 'atHeight', 'atLeft', 'atTop', 'atFlex', 'atMinimum', 'atMaximum',
    'atEnabled', 'atVisible', 'atReadOnly', 'atSurface', 'atCompound', 'atPressed',
    'atVariant', 'atAction', 'atProjection', 'atOverrideMode', 'atInputType',
    'atPart', 'atTarget', 'atComponent', 'atEmit', 'atEmitChange', 'atOption',
    'atPath', 'atDesignID', 'atSplitOrientation', 'atSplitPosition',
    'atSplitMinimum', 'atSplitMaximum', 'atSplitResizable',
    'atDragSource', 'atDropTarget', 'atTouchBehavior', 'atFlowWrap',
    'atCrossAlignment', 'atJustification', 'atWidthSizing', 'atHeightSizing',
    'atMinimumWidth', 'atMaximumWidth', 'atMinimumHeight', 'atMaximumHeight',
    'atQueryContainer', 'atContainerContainment', 'atSliderIntervals',
    'atImageFit', 'atImageHorizontal', 'atImageVertical');

var
  { Built-in names are a finite immutable vocabulary. Initialize once at unit
    startup, before compiler workers can read it, rather than rebuilding stems
    and temporary strings during every token/declaration comparison. }
  GKindStems: array[TNyxKind] of TNyxText;
  GKindFactories: array[TNyxKind] of TNyxText;
  GKindClasses: array[TNyxKind] of TNyxText;
  GKindInterfaces: array[TNyxKind] of TNyxText;
  GKindEnums: array[TNyxKind] of TNyxText;
  { Finite specialized Pascal names are indexed once, then read only. These
    tables contain no application identities or parsed document/source state. }
  GFactoryKinds: TSourceIndex;
  GClassKinds: TSourceIndex;
  GInterfaceKinds: TSourceIndex;
  GEnumNames: TSourceIndex;
  GEnumValues: array of TSourceEnumValue;

constructor ENyxSource.CreateAt(const AMessage: TNyxText;
  const ASource: TNyxText; APosition: Integer);
var
  LIndex: Integer;
  LScalar: Integer;
begin
  FLine := 1;
  FColumn := 1;
  LIndex := 1;
  while (LIndex < APosition) and (LIndex <= Length(ASource)) do
  begin

    if not NyxNextScalar(ASource, LIndex, LScalar) then
    begin
      Inc(LIndex);
      LScalar := 0;
    end;

    if LScalar = 10 then
    begin
      Inc(FLine);
      FColumn := 1;
    end
    else if LScalar <> 13 then
    begin
      Inc(FColumn);
    end;
  end;
  FDiagnosticText := 'Pascal ' + IntToStr(FLine) + ':' + IntToStr(FColumn) +
    ': ' + AMessage;
  inherited Create(FDiagnosticText);
end;

function Lex(const ASource: TNyxText): TTokens;
var
  LIndex: Integer;
  LStart: Integer;
  LOpeningQuote: Integer;
  LCount: Integer;
  LScalar: Integer;
  LText: TNyxText;
  LKind: TTokenKind;
  LClosed: Boolean;
  LChar: Char;
begin
  { Bound allocation before scanning, validate Unicode once, then tokenize ASCII
    Pascal syntax without converting any caption through an ANSI collection.
    Comment text is retained for delimiter discovery; strings are decoded once.
    Growth is geometric so a large source does not repeatedly copy every token. }

  if Length(ASource) > 4 * 1024 * 1024 then
  begin
    raise ENyxSource.CreateAt('Source exceeds the editor budget', ASource, 1);
  end;
  LIndex := 1;
  while LIndex <= Length(ASource) do
  begin

    if not NyxNextScalar(ASource, LIndex, LScalar) then
    begin
      raise ENyxSource.CreateAt('Malformed Unicode source', ASource, LIndex);
    end;
  end;
  Result := nil;
  LCount := 0;
  LIndex := 1;
  while LIndex <= Length(ASource) do
  begin
    LChar := ASource[LIndex];

    if LChar in [#9, #10, #13, ' '] then
    begin
      Inc(LIndex);
      Continue;
    end;
    LStart := LIndex;
    LKind := tkSymbol;
    LText := '';

    if LChar in ['A'..'Z', 'a'..'z', '_'] then
    begin
      LKind := tkWord;
      Inc(LIndex);
      while (LIndex <= Length(ASource)) and
        (ASource[LIndex] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
      begin
        Inc(LIndex);
      end;
      LText := Copy(ASource, LStart, LIndex - LStart);
    end
    else if LChar in ['0'..'9'] then
    begin
      LKind := tkNumber;
      Inc(LIndex);
      while (LIndex <= Length(ASource)) and
        (ASource[LIndex] in ['0'..'9']) do
      begin
        Inc(LIndex);
      end;

      if (LIndex < Length(ASource)) and (ASource[LIndex] = '.') and
        (ASource[LIndex + 1] in ['0'..'9']) then
      begin
        Inc(LIndex);
        while (LIndex <= Length(ASource)) and
          (ASource[LIndex] in ['0'..'9']) do
        begin
          Inc(LIndex);
        end;
      end;

      if (LIndex <= Length(ASource)) and (ASource[LIndex] in ['e', 'E']) then
      begin
        Inc(LIndex);

        if (LIndex <= Length(ASource)) and (ASource[LIndex] in ['+', '-']) then
        begin
          Inc(LIndex);
        end;
        while (LIndex <= Length(ASource)) and
          (ASource[LIndex] in ['0'..'9']) do
        begin
          Inc(LIndex);
        end;
      end;
      LText := Copy(ASource, LStart, LIndex - LStart);
    end
    else if LChar = '''' then
    begin
      LKind := tkString;
      LOpeningQuote := LIndex;
      Inc(LIndex);
      LStart := LIndex;
      LClosed := False;
      while LIndex <= Length(ASource) do
      begin

        if ASource[LIndex] in [#10, #13] then
        begin
          Break;
        end;

        if ASource[LIndex] = '''' then
        begin
          LText := LText + Copy(ASource, LStart, LIndex - LStart);
          Inc(LIndex);

          if (LIndex <= Length(ASource)) and (ASource[LIndex] = '''') then
          begin
            LText := LText + '''';
            Inc(LIndex);
            LStart := LIndex;
            Continue;
          end;
          LClosed := True;
          Break;
        end;
        Inc(LIndex);
      end;

      if not LClosed then
      begin
        raise ENyxSource.CreateAt('Unterminated Pascal string', ASource, LStart - 1);
      end;
      { Recover the complete token's position, including its opening quote. }
      LStart := LOpeningQuote;
    end
    else if LChar = '#' then
    begin
      LKind := tkCharacter;
      Inc(LIndex);

      if (LIndex <= Length(ASource)) and (ASource[LIndex] = '$') then
      begin
        Inc(LIndex);
        while (LIndex <= Length(ASource)) and
          (ASource[LIndex] in ['0'..'9', 'a'..'f', 'A'..'F']) do
        begin
          Inc(LIndex);
        end;
      end
      else
      begin
        while (LIndex <= Length(ASource)) and
          (ASource[LIndex] in ['0'..'9']) do
        begin
          Inc(LIndex);
        end;
      end;
      LText := Copy(ASource, LStart + 1, LIndex - LStart - 1);
    end
    else if (LChar = '{') or
      ((LChar = '(') and (LIndex < Length(ASource)) and (ASource[LIndex + 1] = '*')) then
    begin
      LKind := tkComment;
      LClosed := False;

      if LChar = '{' then
      begin
        Inc(LIndex);
        while LIndex <= Length(ASource) do
        begin

          if ASource[LIndex] = '}' then
          begin
            Inc(LIndex);
            LClosed := True;
            Break;
          end;
          Inc(LIndex);
        end;
      end
      else
      begin
        Inc(LIndex, 2);
        while LIndex < Length(ASource) do
        begin

          if (ASource[LIndex] = '*') and (ASource[LIndex + 1] = ')') then
          begin
            Inc(LIndex, 2);
            LClosed := True;
            Break;
          end;
          Inc(LIndex);
        end;
      end;

      if not LClosed then
      begin
        raise ENyxSource.CreateAt('Unterminated Pascal comment', ASource, LStart);
      end;
      LText := Copy(ASource, LStart, LIndex - LStart);
    end
    else if (LChar = '/') and (LIndex < Length(ASource)) and
      (ASource[LIndex + 1] = '/') then
    begin
      LKind := tkComment;
      while (LIndex <= Length(ASource)) and not (ASource[LIndex] in [#10, #13]) do
      begin
        Inc(LIndex);
      end;
      LText := Copy(ASource, LStart, LIndex - LStart);
    end
    else
    begin
      LText := LChar;
      Inc(LIndex);

      if (LChar = ':') and (LIndex <= Length(ASource)) and (ASource[LIndex] = '=') then
      begin
        LText := ':=';
        Inc(LIndex);
      end;
    end;

    if LCount >= 200000 then
    begin
      raise ENyxSource.CreateAt('Source exceeds the token budget', ASource, LStart);
    end;

    if LCount = Length(Result) then
    begin
      SetLength(Result, LCount * 2 + 256);
    end;
    Result[LCount].Kind := LKind;
    Result[LCount].Text := LText;
    Result[LCount].Position := LStart;
    Result[LCount].Finish := LIndex;
    Inc(LCount);
  end;
  SetLength(Result, LCount);
end;

procedure Split(const ASource: TNyxText; out APrefix, ABody, ASuffix: TNyxText;
  out ABodyTokens: TTokens);
var
  LTokens: TTokens;
  LIndex: Integer;
  LBegin: Integer;
  LEnd: Integer;
  LCount: Integer;
begin
  LTokens := Lex(ASource);
  LBegin := -1;
  LEnd := -1;
  for LIndex := 0 to Length(LTokens) - 1 do
  begin

    if LTokens[LIndex].Kind <> tkComment then
    begin
      Continue;
    end;

    if Trim(LTokens[LIndex].Text) = NyxViewsBegin then
    begin

      if LBegin >= 0 then
      begin
        raise ENyxSource.CreateAt('Duplicate views boundary', ASource, LTokens[LIndex].Position);
      end;
      LBegin := LIndex;
    end;

    if Trim(LTokens[LIndex].Text) = NyxViewsEnd then
    begin

      if LEnd >= 0 then
      begin
        raise ENyxSource.CreateAt('Duplicate views boundary', ASource, LTokens[LIndex].Position);
      end;
      LEnd := LIndex;
    end;
  end;

  if (LBegin < 0) or (LEnd <= LBegin) then
  begin
    raise ENyxSource.CreateAt('Keep the opening and closing nyx:views comments', ASource, 1);
  end;
  APrefix := Copy(ASource, 1, LTokens[LBegin].Finish - 1);
  ABody := Copy(ASource, LTokens[LBegin].Finish,
    LTokens[LEnd].Position - LTokens[LBegin].Finish);
  ASuffix := Copy(ASource, LTokens[LEnd].Position, MaxInt);
  ABodyTokens := nil;
  LCount := 0;
  for LIndex := LBegin + 1 to LEnd - 1 do
  begin

    if LTokens[LIndex].Kind = tkComment then
    begin
      { Compiler directives alter Pascal semantics; comments cannot conceal an
        admitted configuration behind IFDEF or override its string encoding. }

      if (Copy(LTokens[LIndex].Text, 1, 2) = '{$') or
        (Copy(LTokens[LIndex].Text, 1, 3) = '(*$') then
      begin
        raise ENyxSource.CreateAt('Directives belong outside the views builder',
          ASource, LTokens[LIndex].Position);
      end;
      Continue;
    end;

    if LCount = Length(ABodyTokens) then
    begin
      SetLength(ABodyTokens, LCount * 2 + 256);
    end;
    ABodyTokens[LCount] := LTokens[LIndex];
    Inc(LCount);
  end;
  SetLength(ABodyTokens, LCount);
end;

function SymbolStem(const AName: TNyxText): TNyxText;
var
  LIndex: Integer;
  LUpper: Boolean;
  LChar: Char;
begin
  Result := '';
  LUpper := True;
  for LIndex := 1 to Length(AName) do
  begin
    LChar := AName[LIndex];

    if LChar = '-' then
    begin
      LUpper := True;
      Continue;
    end;

    if LUpper then
    begin
      LChar := UpCase(LChar);
    end;
    Result := Result + LChar;
    LUpper := False;
  end;
end;

{ Publish the closed Pascal vocabulary once, in its original matching order.
  The first declaration wins if two public spellings coincide. Only scalar
  type/ordinal facts are retained; no expression value, source or model survives. }
procedure InitializeEnumSymbols;
var
  LKind: TNyxKind;
  LAttribute: TNyxAttribute;
  LLayout: TNyxLayoutMode;
  LVariant: TNyxVariant;
  LAction: TNyxAction;
  LOverride: TNyxOverrideMode;
  LInput: TNyxInputType;
  LBinding: TNyxBindingProperty;
  LTrigger: TNyxTrigger;
  LSemantic: TNyxSemanticEvent;
  LTimePrecision: TNyxTimePrecision;

  procedure RegisterEnum(AKind: TValueKind; AOrdinal: Integer;
    const ASymbol: TNyxText);
  var
    LKey: TNyxText;
    LIndex: Integer;
  begin
    LKey := LowerCase(ASymbol);

    if GEnumNames.IndexOf(LKey) >= 0 then
    begin
      Exit;
    end;
    LIndex := Length(GEnumValues);
    SetLength(GEnumValues, LIndex + 1);
    GEnumValues[LIndex].Kind := AKind;
    GEnumValues[LIndex].Ordinal := AOrdinal;
    GEnumNames.AddFirst(LKey, LIndex);
  end;

begin
  GEnumNames := TSourceIndex.Create;
  GEnumValues := nil;

  for LTimePrecision := Low(TNyxTimePrecision) to High(TNyxTimePrecision) do
  begin
    RegisterEnum(vkTimePrecision, Ord(LTimePrecision), NyxTimePrecisionPascal(LTimePrecision));
  end;

  RegisterEnum(vkCollectionScope, Ord(csApplication), 'csApplication');
  RegisterEnum(vkCollectionScope, Ord(csInstance), 'csInstance');
  RegisterEnum(vkSelectionMode, Ord(nsmSingle), 'nsmSingle');
  RegisterEnum(vkSelectionMode, Ord(nsmMultiple), 'nsmMultiple');
  RegisterEnum(vkSortDirection, Ord(nsdAscending), 'nsdAscending');
  RegisterEnum(vkSortDirection, Ord(nsdDescending), 'nsdDescending');
  RegisterEnum(vkQueryTextComparison, Ord(nqtExact), 'nqtExact');
  RegisterEnum(vkQueryTextComparison, Ord(nqtAsciiInsensitive), 'nqtAsciiInsensitive');
  RegisterEnum(vkCollectionCellMode, Ord(cmReadOnly), 'cmReadOnly');
  RegisterEnum(vkCollectionCellMode, Ord(cmEditable), 'cmEditable');

  RegisterEnum(vkConstruction, Ord(ncoDefault), 'ncoDefault');
  RegisterEnum(vkImageFormat, Ord(nimPNG), 'nimPNG');
  RegisterEnum(vkImageFormat, Ord(nimJPEG), 'nimJPEG');
  RegisterEnum(vkResourceKind, Ord(nrkImage), 'nrkImage');
  RegisterEnum(vkResourceKind, Ord(nrkJSON), 'nrkJSON');
  RegisterEnum(vkResourceKind, Ord(nrkText), 'nrkText');
  RegisterEnum(vkResourceKind, Ord(nrkBinary), 'nrkBinary');
  RegisterEnum(vkResourceServerPolicy, Ord(rcspRespect), 'rcspRespect');
  RegisterEnum(vkResourceServerPolicy, Ord(rcspOverride), 'rcspOverride');
  RegisterEnum(vkImageFit, Ord(nifContain), 'nifContain');
  RegisterEnum(vkImageFit, Ord(nifCover), 'nifCover');
  RegisterEnum(vkImageFit, Ord(nifStretch), 'nifStretch');
  RegisterEnum(vkImageFit, Ord(nifNatural), 'nifNatural');
  RegisterEnum(vkImageFit, Ord(nifShrink), 'nifShrink');
  RegisterEnum(vkImageAnchor, Ord(niaStart), 'niaStart');
  RegisterEnum(vkImageAnchor, Ord(niaCenter), 'niaCenter');
  RegisterEnum(vkImageAnchor, Ord(niaEnd), 'niaEnd');
  RegisterEnum(vkConstruction, Ord(ncoDescriptor), 'ncoDescriptor');

  RegisterEnum(vkPolicy, Ord(neSequential), 'neSequential');
  RegisterEnum(vkPolicy, Ord(neAsynchronous), 'neAsynchronous');
  RegisterEnum(vkPolicy, Ord(neUIQueue), 'neUIQueue');
  RegisterEnum(vkPolicy, Ord(neThreaded), 'neThreaded');
  for LSemantic := Low(TNyxSemanticEvent) to High(TNyxSemanticEvent) do
  begin

    RegisterEnum(vkSemanticEvent, Ord(LSemantic), NyxSemanticSymbol(LSemantic));
  end;
  for LTrigger := Low(TNyxTrigger) to High(TNyxTrigger) do
  begin

    RegisterEnum(vkTrigger, Ord(LTrigger), 'nt' + SymbolStem(NyxTriggerName(LTrigger)));
  end;

  RegisterEnum(vkBindingDirection, Ord(bdFromState), 'bdFromState');
  RegisterEnum(vkBindingDirection, Ord(bdTwoWay), 'bdTwoWay');
  for LBinding := Low(TNyxBindingProperty) to High(TNyxBindingProperty) do
  begin

    RegisterEnum(vkBindingProperty, Ord(LBinding), 'bp' + CBindingMethods[LBinding]);
  end;

  RegisterEnum(vkVariant, Ord(nvDefault), 'nvDefault');
  RegisterEnum(vkAction, Ord(naNone), 'naNone');
  for LKind := Low(TNyxKind) to High(TNyxKind) do
  begin

    RegisterEnum(vkKind, Ord(LKind), GKindEnums[LKind]);
  end;
  for LAttribute := Low(TNyxAttribute) to High(TNyxAttribute) do
  begin

    RegisterEnum(vkAttribute, Ord(LAttribute), CAttributes[LAttribute]);
  end;

  RegisterEnum(vkPlatform, Ord(npfAny), 'npfAny');
  RegisterEnum(vkPlatform, Ord(npfBrowser), 'npfBrowser');
  RegisterEnum(vkPlatform, Ord(npfNativeLCL), 'npfNativeLCL');
  RegisterEnum(vkViewportOrientation, Ord(nvoAny), 'nvoAny');
  RegisterEnum(vkViewportOrientation, Ord(nvoPortrait), 'nvoPortrait');
  RegisterEnum(vkViewportOrientation, Ord(nvoLandscape), 'nvoLandscape');
  RegisterEnum(vkViewportOrientation, Ord(nvoSquare), 'nvoSquare');
  RegisterEnum(vkSplitOrientation, Ord(nsoStacked), 'nsoStacked');
  RegisterEnum(vkSplitOrientation, Ord(nsoSideBySide), 'nsoSideBySide');
  RegisterEnum(vkTouchBehavior, Ord(ntbAutomatic), 'ntbAutomatic');
  RegisterEnum(vkTouchBehavior, Ord(ntbNone), 'ntbNone');
  RegisterEnum(vkTouchBehavior, Ord(ntbPanX), 'ntbPanX');
  RegisterEnum(vkTouchBehavior, Ord(ntbPanY), 'ntbPanY');
  RegisterEnum(vkTouchBehavior, Ord(ntbManipulation), 'ntbManipulation');
  RegisterEnum(vkFlowWrap, Ord(nfwAutomatic), 'nfwAutomatic');
  RegisterEnum(vkContainerContainment, Ord(nccWidth), 'nccWidth');
  RegisterEnum(vkContainerContainment, Ord(nccSize), 'nccSize');
  RegisterEnum(vkFlowWrap, Ord(nfwNoWrap), 'nfwNoWrap');
  RegisterEnum(vkFlowWrap, Ord(nfwWrap), 'nfwWrap');
  RegisterEnum(vkCrossAlignment, Ord(ncaAutomatic), 'ncaAutomatic');
  RegisterEnum(vkCrossAlignment, Ord(ncaStart), 'ncaStart');
  RegisterEnum(vkCrossAlignment, Ord(ncaCenter), 'ncaCenter');
  RegisterEnum(vkCrossAlignment, Ord(ncaEnd), 'ncaEnd');
  RegisterEnum(vkCrossAlignment, Ord(ncaStretch), 'ncaStretch');
  RegisterEnum(vkJustification, Ord(njStart), 'njStart');
  RegisterEnum(vkJustification, Ord(njCenter), 'njCenter');
  RegisterEnum(vkJustification, Ord(njEnd), 'njEnd');
  RegisterEnum(vkJustification, Ord(njSpaceBetween), 'njSpaceBetween');
  RegisterEnum(vkJustification, Ord(njSpaceAround), 'njSpaceAround');
  RegisterEnum(vkJustification, Ord(njSpaceEvenly), 'njSpaceEvenly');
  RegisterEnum(vkSizing, Ord(nsAutomatic), 'nsAutomatic');
  RegisterEnum(vkSizing, Ord(nsContent), 'nsContent');
  RegisterEnum(vkSizing, Ord(nsFill), 'nsFill');
  RegisterEnum(vkMenuOpening, Ord(nmoFirst), 'nmoFirst');
  RegisterEnum(vkMenuOpening, Ord(nmoLast), 'nmoLast');
  RegisterEnum(vkPopoverSide, Ord(npsBelow), 'npsBelow');
  RegisterEnum(vkPopoverSide, Ord(npsAbove), 'npsAbove');
  RegisterEnum(vkPopoverSide, Ord(npsRight), 'npsRight');
  RegisterEnum(vkPopoverSide, Ord(npsLeft), 'npsLeft');
  RegisterEnum(vkPopoverAlignment, Ord(npaStart), 'npaStart');
  RegisterEnum(vkPopoverAlignment, Ord(npaCenter), 'npaCenter');
  RegisterEnum(vkPopoverAlignment, Ord(npaEnd), 'npaEnd');
  RegisterEnum(vkPopoverSizing, Ord(npzContent), 'npzContent');
  RegisterEnum(vkPopoverSizing, Ord(npzFixed), 'npzFixed');
  RegisterEnum(vkPopoverDismissal, Ord(npdEscape), 'npdEscape');
  RegisterEnum(vkPopoverDismissal, Ord(npdOutsidePress), 'npdOutsidePress');
  RegisterEnum(vkTypeAheadMatch, Ord(ntmFolded), 'ntmFolded');
  RegisterEnum(vkTypeAheadMatch, Ord(ntmExact), 'ntmExact');
  RegisterEnum(vkThemePreset, Ord(ntpLight), 'ntpLight');
  RegisterEnum(vkThemePreset, Ord(ntpDark), 'ntpDark');
  for LLayout := Low(TNyxLayoutMode) to High(TNyxLayoutMode) do
  begin

    RegisterEnum(vkLayout, Ord(LLayout), 'nl' + SymbolStem(NyxLayoutName(LLayout)));
  end;
  for LVariant := Low(TNyxVariant) to High(TNyxVariant) do
  begin

    RegisterEnum(vkVariant, Ord(LVariant), 'nv' + SymbolStem(NyxVariantName(LVariant)));
  end;
  for LAction := Low(TNyxAction) to High(TNyxAction) do
  begin

    RegisterEnum(vkAction, Ord(LAction), 'na' + SymbolStem(NyxActionName(LAction)));
  end;
  for LOverride := Low(TNyxOverrideMode) to High(TNyxOverrideMode) do
  begin

    RegisterEnum(vkOverride, Ord(LOverride), 'no' + SymbolStem(NyxOverrideName(LOverride)));
  end;
  for LInput := Low(TNyxInputType) to High(TNyxInputType) do
  begin

    RegisterEnum(vkInput, Ord(LInput), 'ni' + SymbolStem(NyxInputTypeName(LInput)));
  end;
end;


function EnumValue(const AName: TNyxText; out AValue: TValue): Boolean;
var
  LIndex: Integer;
begin
  AValue.Text := '';
  LIndex := GEnumNames.IndexOf(LowerCase(AName));
  Result := LIndex >= 0;

  if Result then
  begin
    AValue.Kind := GEnumValues[LIndex].Kind;
    AValue.Ordinal := GEnumValues[LIndex].Ordinal;
  end;
end;


procedure ValidateLocalName(const ASource, AName: TNyxText; APosition: Integer);
var
  LLower: TNyxText;
  LValue: TValue;
begin
  try
    TNyxCodegen.AdmitUnitName(AName);
  except
    raise ENyxSource.CreateAt('Use an ordinary Pascal local identifier', ASource, APosition);
  end;
  LLower := LowerCase(AName);

  if (Pos('.', AName) > 0) or (LLower = 'result') or
    (LLower = 'buildnyxdocument') or (LLower = 'high') or (LLower = 'low') or
    (LLower = 'integer') or (LLower = 'double') or (LLower = 'boolean') or
    (LLower = 'string') or (LLower = 'utf8string') or
    (Copy(LLower, 1, 3) = 'nyx') or (Copy(LLower, 1, 4) = 'tnyx') or
    (Copy(LLower, 1, 4) = 'inyx') or (Copy(LLower, 1, 6) = 'newnyx') or
    EnumValue(AName, LValue) then
  begin
    raise ENyxSource.CreateAt('A local cannot shadow the builder or Nyx public symbols',
      ASource, APosition);
  end;
end;


constructor TConfigurationReader.Create(const ASource: TNyxText;
  const ATokens: TTokens; const ALocals: TControlLocals;
  ADocument: TNyxDocument; AApply: Boolean; AReconstruct: Boolean);
var
  LIndex: Integer;
  LCount: Integer;
  LOther: Integer;
begin
  inherited Create;
  FSource := ASource;
  FTokens := ATokens;
  FDocument := ADocument;
  FApply := AApply;
  FLocalNames := TSourceIndex.Create;
  FStateNames := TSourceIndex.Create;
  FControlIDs := TSourceIndex.Create;

  if AReconstruct then
  begin
    { Full replay parses declarations and assignments in execution order. The
      lexical identity pre-scan below belongs only to source reconciliation. }
    Exit;
  end;
  { Reconciliation uses the same declaration grammar as source admission.
    Grouped locals and unused declarations must still reserve their names. }
  for LIndex := 0 to Length(FTokens) - 1 do
  begin

    if (FTokens[LIndex].Kind = tkWord) and SameText(FTokens[LIndex].Text, 'begin') then
    begin
      Break;
    end;

    if (FTokens[LIndex].Kind = tkWord) and SameText(FTokens[LIndex].Text, 'var') then
    begin
      FCursor := LIndex;
      Declarations;
      Break;
    end;
  end;
  for LIndex := 0 to Length(ALocals) - 1 do
  begin
    LOther := LocalIndex(ALocals[LIndex].Name);

    if LOther < 0 then
    begin
      raise ENyxSource.CreateAt('Declare every constructed control local', ASource, 1);
    end;
    FLocals[LOther].ID := ALocals[LIndex].ID;
  end;
  LIndex := 0;
  for LIndex := 0 to Length(FLocals) - 1 do
  begin
    ValidateLocalName(ASource, FLocals[LIndex].Name, 1);

    if (LocalIndex(FLocals[LIndex].Name) <> LIndex) or
      ((FLocals[LIndex].ID <> '') and (FControlIDs.IndexOf(FLocals[LIndex].ID) >= 0)) then
    begin
      raise ENyxSource.CreateAt('Control locals require unique names and identities', ASource, 1);
    end;

    if FLocals[LIndex].ID <> '' then
    begin
      FControlIDs.AddFirst(FLocals[LIndex].ID, LIndex);
    end;
  end;
  for LIndex := 0 to Length(FStates) - 1 do
  begin
    ValidateLocalName(ASource, FStates[LIndex].Name, 1);

    if LocalIndex(FStates[LIndex].Name) >= 0 then
    begin
      raise ENyxSource.CreateAt('Control and state locals share one Pascal namespace', ASource, 1);
    end;

    if StateIndex(FStates[LIndex].Name) <> LIndex then
    begin
      raise ENyxSource.CreateAt('State local names must be unique', ASource, 1);
    end;
  end;
  LIndex := 0;
  while LIndex + 1 < Length(FTokens) do
  begin
    LCount := StateIndex(FTokens[LIndex].Text);

    if (LCount >= 0) and (FTokens[LIndex + 1].Text = ':=') then
    begin
      FCursor := LIndex;
      ReferenceAssignment(LCount);
      LIndex := FCursor;
    end
    else
    begin
      Inc(LIndex);
    end;
  end;
  for LIndex := 0 to Length(FStates) - 1 do
  begin
    FStates[LIndex].Assigned := False;
  end;
  FCursor := 0;
end;

destructor TConfigurationReader.Destroy;
begin
  { Releasing staged interfaces frees controls that failed before adoption.
    Accepted controls remain retained independently by their document/parent. }
  FControls := nil;
  FMenuDefinitions := nil;
  FMenuBarDefinitions := nil;
  FRecipeCatalog.Free;
  FControlIDs.Free;
  FStateNames.Free;
  FLocalNames.Free;
  inherited Destroy;
end;

procedure TConfigurationReader.Fail(const AMessage: TNyxText);
var
  LPosition: Integer;
begin
  LPosition := Length(FSource) + 1;

  if FCursor < Length(FTokens) then
  begin
    LPosition := FTokens[FCursor].Position;
  end;
  raise ENyxSource.CreateAt(AMessage, FSource, LPosition);
end;

function TConfigurationReader.At(const AText: TNyxText): Boolean;
begin
  Result := (FCursor < Length(FTokens)) and
    SameText(FTokens[FCursor].Text, AText);

  if Result then
  begin
    { A quoted caption such as 'end' or ';' must never become builder syntax. }

    if AText[1] in ['a'..'z', 'A'..'Z', '_'] then
    begin
      Result := FTokens[FCursor].Kind = tkWord;
    end
    else
    begin
      Result := FTokens[FCursor].Kind = tkSymbol;
    end;
  end;
end;

procedure TConfigurationReader.Expect(const AText: TNyxText);
begin

  if not At(AText) then
  begin
    Fail('Expected ' + AText);
  end;
  Inc(FCursor);
end;

function TConfigurationReader.Arguments(AMaximum: Integer): TValues;
var
  LCount: Integer;
begin
  Result := nil;
  Expect('(');

  if not At(')') then
  begin
    repeat
      LCount := Length(Result);

      if LCount >= AMaximum then
      begin
        Fail('Configuration exceeds its typed argument budget');
      end;
      SetLength(Result, LCount + 1);
      Result[LCount] := Expression;

      if not At(',') then
      begin
        Break;
      end;
      Inc(FCursor);
    until False;
  end;
  Expect(')');
end;

function TConfigurationReader.ArrayArguments(AMaximum: Integer): TValues;
var
  LCount: Integer;
begin
  { Open-array syntax stays local to constructors and typed Choices calls. It
    never masquerades as text or as an untyped configuration argument. }
  Result := nil;
  Expect('[');

  if not At(']') then
  begin
    repeat
      LCount := Length(Result);

      if LCount >= AMaximum then
      begin
        Fail('Open array exceeds its constructor budget');
      end;
      SetLength(Result, LCount + 1);
      Result[LCount] := Expression;

      if not At(',') then
      begin
        Break;
      end;
      Inc(FCursor);
    until False;
  end;
  Expect(']');
end;

function TConfigurationReader.Numeric(const AValue: TValue): Double;
var
  LInteger: Integer;
begin
  { Follow the public Double overload's Integer widening rule, never a string
    conversion. Primary already checks finite values and signed Integer bounds. }
  case AValue.Kind of
    vkInteger:
      begin
        TryNyxStateInteger(AValue.Text, LInteger);
        Result := LInteger;
      end;
    vkNumber: TryNyxStateNumber(AValue.Text, Result);
  else
    Fail('A numeric domain requires an Integer or finite Double argument');
  end;
end;

function TConfigurationReader.Domain(AKind: TNyxStateKind; ACalendar: Boolean;
  ARGB: Boolean): TValue;
var
  LMethod: TNyxText;
  LArgs: TValues;
  LIndex: Integer;
  LTexts: array of TNyxText;
  LBooleans: array of Boolean;
  LIntegers: array of Integer;
  LNumbers: array of Double;
  LDates: array of TNyxCalendarDate;
  LColors: array of TNyxRGBColor;
begin
  Result.Kind := vkDomain;
  Result.Ordinal := Ord(AKind);
  case AKind of
    nskText: Result.TextDomain := NyxTextDomain;
    nskBoolean: Result.BooleanDomain := NyxBooleanDomain;
    nskInteger: Result.IntegerDomain := NyxIntegerDomain;
    nskNumber: Result.NumberDomain := NyxNumberDomain;
  end;

  if ACalendar then
  begin
    Result.TextDomain := NyxDateDomain;
  end;

  if ARGB then
  begin
    Result.TextDomain := NyxRGBDomain;
  end;

  if At('(') then
  begin
    LArgs := Arguments;

    if Length(LArgs) <> 0 then
    begin

      if not (ACalendar or ARGB) or (Length(LArgs) <> 1) then
      begin
        Fail('A domain factory takes no arguments or one text definition for enrichment');
      end;
      if ARGB then
      begin

        if (LArgs[0].Kind <> vkValueDomain) or
          (LArgs[0].ValueDomain.Kind <> nskText) then
        begin
          Fail('NyxRGBDomain enrichment requires an explicit text Definition');
        end;
        Result.TextDomain := NyxRGBDomain(LArgs[0].ValueDomain);
      end
      else
      begin

        if (LArgs[0].Kind <> vkDomain) or (LArgs[0].Ordinal <> Ord(nskText)) then
        begin
          Fail('A date enrichment requires a text domain');
        end;
        Result.TextDomain := NyxDateDomain(LArgs[0].TextDomain.Definition);
      end;
    end;
  end;
  while At('.') do
  begin
    Expect('.');

    if (FCursor >= Length(FTokens)) or (FTokens[FCursor].Kind <> tkWord) then
    begin
      Fail('Expected a typed domain method');
    end;
    LMethod := FTokens[FCursor].Text;
    Inc(FCursor);

    if SameText(LMethod, 'Range') then
    begin
      LArgs := Arguments;

      if Length(LArgs) <> 2 then
      begin
        Fail('Range requires two typed bounds');
      end;
      case AKind of
        nskText:
          begin

            if not Result.TextDomain.Definition.CalendarDate or
              (LArgs[0].Kind <> vkCalendarDate) or (LArgs[1].Kind <> vkCalendarDate) then
            begin
              Fail('Date Range requires typed calendar bounds');
            end;
            Result.TextDomain := Result.TextDomain.Range(
              LArgs[0].CalendarDate, LArgs[1].CalendarDate);
          end;
        nskInteger:
          begin

            if (LArgs[0].Kind <> vkInteger) or (LArgs[1].Kind <> vkInteger) then
            begin
              Fail('Integer Range requires signed Integer bounds');
            end;
            SetLength(LIntegers, 2);
            TryNyxStateInteger(LArgs[0].Text, LIntegers[0]);
            TryNyxStateInteger(LArgs[1].Text, LIntegers[1]);
            Result.IntegerDomain := Result.IntegerDomain.Range(LIntegers[0], LIntegers[1]);
          end;
        nskNumber:
          Result.NumberDomain := Result.NumberDomain.Range(Numeric(LArgs[0]), Numeric(LArgs[1]));
      else
        Fail('Only Integer, Number and Date domains have Range');
      end;
    end
    else if SameText(LMethod, 'Choices') then
    begin
      Expect('(');
      LArgs := ArrayArguments(NyxMaximumDomainChoices);
      Expect(')');
      case AKind of
        nskText:
          begin
            { A date domain accepts its typed overload as one homogeneous array.
              Text choices remain an explicit canonical wire-boundary overload. }

            if (Length(LArgs) > 0) and (LArgs[0].Kind = vkRGBColor) then
            begin
              SetLength(LColors, Length(LArgs));
              for LIndex := 0 to Length(LArgs) - 1 do
              begin

                if LArgs[LIndex].Kind <> vkRGBColor then
                begin
                  Fail('RGB Choices requires homogeneous typed colors');
                end;
                LColors[LIndex] := LArgs[LIndex].RGBColor;
              end;
              Result.TextDomain := Result.TextDomain.Choices(LColors);
              Continue;
            end;

            if (Length(LArgs) > 0) and (LArgs[0].Kind = vkCalendarDate) then
            begin
              SetLength(LDates, Length(LArgs));
              for LIndex := 0 to Length(LArgs) - 1 do
              begin

                if LArgs[LIndex].Kind <> vkCalendarDate then
                begin
                  Fail('Date Choices requires homogeneous typed calendar members');
                end;
                LDates[LIndex] := LArgs[LIndex].CalendarDate;
              end;
              Result.TextDomain := Result.TextDomain.Choices(LDates);
              Continue;
            end;
            SetLength(LTexts, Length(LArgs));
            for LIndex := 0 to Length(LArgs) - 1 do
            begin

              if LArgs[LIndex].Kind <> vkText then
              begin
                Fail('Text Choices requires text members');
              end;
              LTexts[LIndex] := LArgs[LIndex].Text;
            end;
            Result.TextDomain := Result.TextDomain.Choices(LTexts);
          end;
        nskBoolean:
          begin
            SetLength(LBooleans, Length(LArgs));
            for LIndex := 0 to Length(LArgs) - 1 do
            begin

              if LArgs[LIndex].Kind <> vkBoolean then
              begin
                Fail('Boolean Choices requires Boolean members');
              end;
              LBooleans[LIndex] := LArgs[LIndex].Ordinal <> 0;
            end;
            Result.BooleanDomain := Result.BooleanDomain.Choices(LBooleans);
          end;
        nskInteger:
          begin
            SetLength(LIntegers, Length(LArgs));
            for LIndex := 0 to Length(LArgs) - 1 do
            begin

              if LArgs[LIndex].Kind <> vkInteger then
              begin
                Fail('Integer Choices requires signed Integer members');
              end;
              TryNyxStateInteger(LArgs[LIndex].Text, LIntegers[LIndex]);
            end;
            Result.IntegerDomain := Result.IntegerDomain.Choices(LIntegers);
          end;
        nskNumber:
          begin
            SetLength(LNumbers, Length(LArgs));
            for LIndex := 0 to Length(LArgs) - 1 do
            begin
              LNumbers[LIndex] := Numeric(LArgs[LIndex]);
            end;
            Result.NumberDomain := Result.NumberDomain.Choices(LNumbers);
          end;
      end;
    end
    else if SameText(LMethod, 'RGBColor') then
    begin

      if AKind <> nskText then
      begin
        Fail('RGBColor requires a text domain');
      end;
      Result.TextDomain := Result.TextDomain.RGBColor;
    end
    else if SameText(LMethod, 'Definition') then
    begin

      if At('(') then
      begin
        LArgs := Arguments;

        if Length(LArgs) <> 0 then
        begin
          Fail('Domain Definition takes no arguments');
        end;
      end;
      case AKind of
        nskText:
          begin
            Result.ValueDomain := Result.TextDomain.Definition;
          end;
        nskBoolean:
          begin
            Result.ValueDomain := Result.BooleanDomain.Definition;
          end;
        nskInteger:
          begin
            Result.ValueDomain := Result.IntegerDomain.Definition;
          end;
        nskNumber:
          begin
            Result.ValueDomain := Result.NumberDomain.Definition;
          end;
      end;
      Result.Kind := vkValueDomain;
      Exit;
    end
    else if SameText(LMethod, 'CalendarDate') and (AKind = nskText) then
    begin

      if At('(') then
      begin
        LArgs := Arguments;

        if Length(LArgs) <> 0 then
        begin
          Fail('CalendarDate takes no arguments');
        end;
      end;
      Result.TextDomain := Result.TextDomain.CalendarDate;
    end
    else
    begin
      Fail('Unsupported domain method ' + LMethod);
    end;
  end;
end;

{$I nyx.source.collections.inc}
{$I nyx.source.query.inc}
{$I nyx.source.menus.inc}
{$I nyx.source.resources.inc}
{$I nyx.source.times.inc}
{$I nyx.source.themes.inc}

function TConfigurationReader.DataConstructor(const AName: TNyxText): TValue;
var
  LArgs: TValues;
  LItems: array of TNyxDataValue;
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LInteger: Integer;
begin
  Result.Kind := vkData;

  if SameText(AName, 'NyxNull') then
  begin

    if At('(') then
    begin
      Expect('(');
      Expect(')');
    end;
    Result.Data := NyxNull;
    Exit;
  end;

  if SameText(AName, 'NyxObject') or SameText(AName, 'NyxArray') then
  begin
    Expect('(');
    { Objects use the shared member limit. Arrays are byte-budgeted; the source
      lexer and public constructors also bound their complete representations. }
    LIndex := NyxMaximumJSONBytes;

    if SameText(AName, 'NyxObject') then
    begin
      LIndex := NyxMaximumJSONMembers;
    end;
    LArgs := ArrayArguments(LIndex);
    Expect(')');

    if SameText(AName, 'NyxObject') then
    begin
      SetLength(LFields, Length(LArgs));
      for LIndex := 0 to Length(LArgs) - 1 do
      begin

        if LArgs[LIndex].Kind <> vkDataField then
        begin
          Fail('NyxObject requires NyxField members');
        end;
        LFields[LIndex] := NyxField(LArgs[LIndex].Text, LArgs[LIndex].Data);
      end;
      Result.Data := NyxObject(LFields);
    end
    else
    begin
      SetLength(LItems, Length(LArgs));
      for LIndex := 0 to Length(LArgs) - 1 do
      begin

        if LArgs[LIndex].Kind <> vkData then
        begin
          Fail('NyxArray requires typed data members');
        end;
        LItems[LIndex] := LArgs[LIndex].Data.Copy;
      end;
      Result.Data := NyxArray(LItems);
    end;
    Exit;
  end;
  LArgs := Arguments;

  if SameText(AName, 'NyxField') then
  begin

    if (Length(LArgs) <> 2) or (LArgs[0].Kind <> vkText) or
      (LArgs[1].Kind <> vkData) then
    begin
      Fail('NyxField requires a text key and typed data value');
    end;
    Result.Kind := vkDataField;
    Result.Text := LArgs[0].Text;
    Result.Data := LArgs[1].Data.Copy;
    Exit;
  end;

  if Length(LArgs) <> 1 then
  begin
    Fail('A scalar data constructor requires one argument');
  end;

  if SameText(AName, 'NyxDecimal') then
  begin

    if LArgs[0].Kind <> vkText then
    begin
      Fail('NyxDecimal requires explicit decimal text');
    end;
    Result.Kind := vkDecimal;
    Result.Text := NyxDecimal(LArgs[0].Text).Text;
    Exit;
  end;
  case LArgs[0].Kind of
    vkText: Result.Data := NyxData(LArgs[0].Text);
    vkBoolean: Result.Data := NyxData(LArgs[0].Ordinal <> 0);
    vkInteger:
      begin
        TryNyxStateInteger(LArgs[0].Text, LInteger);
        Result.Data := NyxData(LInteger);
      end;
    vkNumber: Result.Data := NyxData(Numeric(LArgs[0]));
    vkDecimal: Result.Data := NyxData(NyxDecimal(LArgs[0].Text));
  else
    Fail('NyxData requires text, Boolean, Integer, Double or NyxDecimal');
  end;
end;

function TConfigurationReader.Expression: TValue;
var
  LRight: TValue;
begin
  Inc(FDepth);
  try

    if FDepth > NyxMaximumJSONDepth * 2 + 32 then
    begin
      Fail('Expression exceeds the nesting budget');
    end;
    Result := Primary;
    while At('+') do
    begin
      Inc(FCursor);
      LRight := Primary;

      if (Result.Kind <> vkText) or (LRight.Kind <> vkText) then
      begin
        Fail('Only text concatenation is supported in configuration expressions');
      end;
      Result.Text := Result.Text + LRight.Text;
    end;
  finally
    Dec(FDepth);
  end;
end;

function TConfigurationReader.Primary: TValue;
var
  LToken: TToken;
  LName: TNyxText;
  LSign: TNyxText;
  LArgs: TValues;
  LInteger: Integer;
  LNumber: Double;
  LState: Integer;
  LKind: TNyxStateKind;
begin

  if FCursor >= Length(FTokens) then
  begin
    Fail('Expected a typed configuration value');
  end;
  LToken := FTokens[FCursor];
  Inc(FCursor);
  Result.Kind := vkText;
  Result.CollectionSchema := Default(TNyxCollectionSchema);
  Result.CollectionItem := Default(TNyxCollectionItem);
  Result.CollectionItemRef := Default(TNyxItemRef);
  Result.CollectionView := Default(TNyxCollectionViewSpec);
  Result.CollectionQuery := Default(TNyxCollectionQuery);
  Result.CollectionPredicateData := Default(TNyxDataValue);
  Result.CollectionSort := Default(TNyxCollectionSort);
  Result.Text := '';
  Result.Ordinal := 0;
  case LToken.Kind of
    tkString:
      begin
        Result.Text := LToken.Text;
      end;
    tkCharacter:
      begin

        if not TryStrToInt(LToken.Text, LInteger) or (LInteger < 0) or
          (LInteger > 127) then
        begin
          Fail('Use NyxScalarText for non-ASCII Unicode scalars');
        end;
        Result.Text := NyxScalarText(LInteger);
      end;
    tkNumber, tkSymbol:
      begin
        LSign := '';

        if (LToken.Text = '-') or (LToken.Text = '+') then
        begin
          LSign := LToken.Text;

          if (FCursor >= Length(FTokens)) or (FTokens[FCursor].Kind <> tkNumber) then
          begin
            Fail('A sign requires a numeric literal');
          end;
          LToken := FTokens[FCursor];
          Inc(FCursor);
        end
        else if LToken.Text = '(' then
        begin
          Result := Expression;
          Expect(')');
          Exit;
        end
        else if LToken.Kind <> tkNumber then
        begin
          Fail('Unsupported configuration expression');
        end;
        Result.Text := LSign + LToken.Text;

        if (Pos('.', Result.Text) > 0) or (Pos('e', LowerCase(Result.Text)) > 0) then
        begin
          Result.Kind := vkNumber;

          if not TryNyxStateNumber(Result.Text, LNumber) then
          begin
            Fail('A finite representable number is required');
          end;
        end
        else
        begin
          Result.Kind := vkInteger;

          if not TryNyxStateInteger(Result.Text, LInteger) then
          begin
            Fail('An Integer literal must fit the signed 32-bit range');
          end;
        end;
      end;
    tkWord:
      begin
        LName := LowerCase(LToken.Text);

        if LName = 'nyxdefaultlocale' then
        begin
          Result.Kind := vkResourceLocale;
          Result.Text := '';
          Exit;
        end;

        if (LName = 'nyxresourcerows') or (LName = 'nyxresourcepath') then
        begin
          Exit(ResourceRowsConstructor(LName));
        end;

        if (LName = 'nyxresourceref') or (LName = 'nyxlocale') or
          (LName = 'nyxresourcevalue') or (LName = 'nyxresourceimage') or
          (LName = 'nyxhostedresource') or (LName = 'nyxresourceurl') or
          (LName = 'nyxresourcecache') or (LName = 'nyxresourcelabel') or
          (LName = 'nyxresourcelabels') or (LName = 'nyxresourcediscovery') or
          (LName = 'nyxtextresource') or (LName = 'nyxjsonresource') or
          (LName = 'nyximageresource') or (LName = 'nyxbinaryresource') or
          (LName = 'nyxdecodebase64') then
        begin
          Exit(ResourceConstructor(LName));
        end;

        if (LName = 'nyxthemetokens') or (LName = 'nyxthemepreset') then
        begin
          Exit(ThemeTokens(LName));
        end;

        if (LName = 'nyxtime') or (LName = 'nyxnotime') then
        begin
          Exit(ClockValue(LName));
        end;

        if (LName = 'nyxrgb') or (LName = 'nyxnocolor') or
          (LName = 'tnyxrgbcolor') then
        begin
          Result.Kind := vkRGBColor;
          Result.RGBColor := NyxNoColor;

          if LName = 'tnyxrgbcolor' then
          begin
            Expect('.');
            Expect('FromText');
            LArgs := Arguments;

            if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkText) then
            begin
              Fail('TNyxRGBColor.FromText requires exact text');
            end;
            Result.RGBColor := TNyxRGBColor.FromText(LArgs[0].Text);
          end
          else if (LName = 'nyxrgb') or At('(') then
          begin
            LArgs := Arguments;

            if LName = 'nyxrgb' then
            begin

              if (Length(LArgs) <> 3) or (LArgs[0].Kind <> vkInteger) or
                (LArgs[1].Kind <> vkInteger) or (LArgs[2].Kind <> vkInteger) then
              begin
                Fail('NyxRGB requires three Integer channels');
              end;
              Result.RGBColor := NyxRGB(StrToInt(LArgs[0].Text),
                StrToInt(LArgs[1].Text), StrToInt(LArgs[2].Text));
            end
            else if Length(LArgs) <> 0 then
            begin
              Fail('NyxNoColor takes no arguments');
            end;
          end;
          Exit;
        end;

        if LName = 'nyximagelocation' then
        begin
          LArgs := Arguments(1);

          if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkText) then
          begin
            Fail('NyxImageLocation requires exact resource text');
          end;
          Result := Default(TValue);
          Result.Kind := vkImageLocation;
          Result.Text := NyxImageLocation(LArgs[0].Text).Name;
          Exit;
        end;

        if LName = 'nyximagevalidation' then
        begin
          { Evaluate only this public scalar policy; no executable Pascal call,
            borrowed host decoder or raw string can substitute for a Boolean. }
          Result := Default(TValue);
          Result.Kind := vkImageValidation;
          Result.ImageValidation := NyxImageValidation;

          if At('(') then
          begin
            LArgs := Arguments;

            if Length(LArgs) <> 0 then
            begin
              Fail('NyxImageValidation takes no arguments');
            end;
          end;
          while At('.') do
          begin
            Inc(FCursor);
            Expect('ContainerChecksums');
            LArgs := Arguments(1);

            if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkBoolean) then
            begin
              Fail('ContainerChecksums requires one Boolean');
            end;
            Result.ImageValidation := Result.ImageValidation.ContainerChecksums(LArgs[0].Ordinal <> 0);
          end;
          Exit;
        end;

        if (LName = 'nyximage') or (LName = 'nyxembeddedimage') or
          (LName = 'nyxnoimage') or (LName = 'tnyximagesource') then
        begin
          Result := Default(TValue);
          Result.Kind := vkImageSource;
          Result.ImageSource := NyxNoImage;

          if LName = 'tnyximagesource' then
          begin
            Expect('.');
            Expect('FromWire');
            LArgs := Arguments;

            if not (Length(LArgs) in [1, 2]) or (LArgs[0].Kind <> vkText) then
            begin
              Fail('TNyxImageSource.FromWire requires exact boundary text');
            end;

            if Length(LArgs) = 2 then
            begin

              if LArgs[1].Kind <> vkImageValidation then
              begin
                Fail('Image wire override requires typed NyxImageValidation');
              end;
              Result.ImageSource := TNyxImageSource.FromWire(LArgs[0].Text, LArgs[1].ImageValidation);
            end
            else
            begin
              Result.ImageSource := TNyxImageSource.FromWire(LArgs[0].Text);
            end;
          end
          else if (LName <> 'nyxnoimage') or At('(') then
          begin
            LArgs := Arguments;

            if LName = 'nyximage' then
            begin

              if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkImageLocation) then
              begin
                Fail('NyxImage requires a distinct resource location');
              end;
              Result.ImageSource := NyxImage(NyxImageLocation(LArgs[0].Text));
            end
            else if LName = 'nyxembeddedimage' then
            begin

              if not (Length(LArgs) in [2, 3]) or (LArgs[0].Kind <> vkImageFormat) or
                (LArgs[1].Kind <> vkText) then
              begin
                Fail('NyxEmbeddedImage requires a closed format, base64 text and optional typed validation');
              end;

              if Length(LArgs) = 3 then
              begin

                if LArgs[2].Kind <> vkImageValidation then
                begin
                  Fail('Embedded image validation requires NyxImageValidation');
                end;
                Result.ImageSource := NyxEmbeddedImage(TNyxImageFormat(LArgs[0].Ordinal),
                  LArgs[1].Text, LArgs[2].ImageValidation);
              end
              else
              begin
                Result.ImageSource := NyxEmbeddedImage(TNyxImageFormat(LArgs[0].Ordinal), LArgs[1].Text);
              end;
            end
            else if Length(LArgs) <> 0 then
            begin
              Fail('NyxNoImage takes no arguments');
            end;
          end;
          Exit;
        end;

        if LName = 'nyxrgbdomain' then
        begin
          Exit(Domain(nskText, False, True));
        end;

        if LName = 'nyxtimedomain' then
        begin
          Exit(ClockDomain);
        end;

        if (LName = 'nyxdate') or (LName = 'nyxnodate') then
        begin
          { Evaluate closed integer parts, never locale text or arbitrary Pascal.
            The distinct tag prevents dates being substituted for other families. }
          Result.Kind := vkCalendarDate;
          Result.CalendarDate := NyxNoDate;

          if (LName = 'nyxdate') or At('(') then
          begin
            LArgs := Arguments;

            if LName = 'nyxdate' then
            begin

              if (Length(LArgs) <> 3) or (LArgs[0].Kind <> vkInteger) or
                (LArgs[1].Kind <> vkInteger) or (LArgs[2].Kind <> vkInteger) then
              begin
                Fail('NyxDate requires Integer year, month and day');
              end;
              Result.CalendarDate := NyxDate(StrToInt(LArgs[0].Text),
                StrToInt(LArgs[1].Text), StrToInt(LArgs[2].Text));
            end
            else if Length(LArgs) <> 0 then
            begin
              Fail('NyxNoDate takes no arguments');
            end;
          end;
          Exit;
        end;

        if LName = 'nyxdatedomain' then
        begin
          Exit(Domain(nskText, True));
        end;

        if (LName = 'nyxsizerange') or (LName = 'nyxsizeconstraints') then
        begin
          { A copied closed value builder, never application code execution.
            Its distinct tags prevent text, enums or a range being substituted
            for a complete two-axis constraint policy. }
          Result.Kind := vkSizeConstraints;
          Result.SizeConstraints := NyxSizeConstraints;

          if LName = 'nyxsizerange' then
          begin
            Result.Kind := vkSizeRange;
            Result.SizeRange := NyxSizeRange;
          end;

          if At('(') then
          begin
            LArgs := Arguments;

            if Length(LArgs) <> 0 then
            begin
              Fail('Size policy factories take no arguments');
            end;
          end;
          while At('.') do
          begin
            Expect('.');

            if FCursor >= Length(FTokens) then
            begin
              Fail('Expected a size policy method');
            end;
            LName := LowerCase(FTokens[FCursor].Text);
            Inc(FCursor);

            if (Result.Kind = vkSizeRange) and
              ((LName = 'withoutminimum') or (LName = 'withoutmaximum')) then
            begin

              if At('(') then
              begin
                LArgs := Arguments;

                if Length(LArgs) <> 0 then
                begin
                  Fail('Clearing a range bound takes no arguments');
                end;
              end;

              if LName = 'withoutminimum' then
              begin
                Result.SizeRange := Result.SizeRange.WithoutMinimum;
              end
              else
              begin
                Result.SizeRange := Result.SizeRange.WithoutMaximum;
              end;
              Continue;
            end;
            LArgs := Arguments;

            if Length(LArgs) <> 1 then
            begin
              Fail('A size policy method requires one typed argument');
            end;

            if (Result.Kind = vkSizeConstraints) and
              (LArgs[0].Kind = vkSizeRange) and ((LName = 'width') or (LName = 'height')) then
            begin

              if LName = 'width' then
              begin
                Result.SizeConstraints := Result.SizeConstraints.Width(LArgs[0].SizeRange);
              end
              else
              begin
                Result.SizeConstraints := Result.SizeConstraints.Height(LArgs[0].SizeRange);
              end;
              Continue;
            end;

            if (LArgs[0].Kind <> vkInteger) or
              not TryNyxStateInteger(LArgs[0].Text, LInteger) then
            begin
              Fail('A size bound requires an Integer');
            end;

            if Result.Kind = vkSizeRange then
            begin

              if LName = 'minimum' then
              begin
                Result.SizeRange := Result.SizeRange.Minimum(LInteger);
              end
              else if LName = 'maximum' then
              begin
                Result.SizeRange := Result.SizeRange.Maximum(LInteger);
              end
              else
              begin
                Fail('Unknown size range method');
              end;
            end
            else
            begin

              if LName = 'minimumwidth' then
              begin
                Result.SizeConstraints := Result.SizeConstraints.MinimumWidth(LInteger);
              end
              else if LName = 'maximumwidth' then
              begin
                Result.SizeConstraints := Result.SizeConstraints.MaximumWidth(LInteger);
              end
              else if LName = 'minimumheight' then
              begin
                Result.SizeConstraints := Result.SizeConstraints.MinimumHeight(LInteger);
              end
              else if LName = 'maximumheight' then
              begin
                Result.SizeConstraints := Result.SizeConstraints.MaximumHeight(LInteger);
              end
              else
              begin
                Fail('Unknown size constraints method');
              end;
            end;
          end;
          Exit;
        end;

        if LName = 'tnyxpresentationcondition' then
        begin
          Result.Kind := vkPresentationCondition;
          Expect('.');

          if At('Manual') then
          begin
            Inc(FCursor);
            Result.PresentationCondition := TNyxPresentationCondition.Manual;

            if At('(') then
            begin
              LArgs := Arguments;

              if Length(LArgs) <> 0 then
              begin
                Fail('Manual presentation has no viewport arguments');
              end;
            end;
          end
          else if At('Within') then
          begin
            Inc(FCursor);
            LArgs := Arguments;

            if (Length(LArgs) <> 2) or (LArgs[0].Kind <> vkContainerRef) or
              (LArgs[1].Kind <> vkViewportCondition) then
            begin
              Fail('Within requires a query container reference and typed size condition');
            end;
            Result.PresentationCondition := TNyxPresentationCondition.Within(
              NyxContainer(LArgs[0].Text), LArgs[1].ViewportCondition);
          end
          else
          begin
            Expect('Automatic');
            LArgs := Arguments;

            if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkViewportCondition) then
            begin
              Fail('Automatic presentation requires a typed viewport condition');
            end;
            Result.PresentationCondition := TNyxPresentationCondition.Automatic(LArgs[0].ViewportCondition);
          end;
          Exit;
        end;

        if LName = 'tnyxviewportcondition' then
        begin
          Result.Kind := vkViewportCondition;
          Result.ViewportCondition := TNyxViewportCondition.Any;
          Expect('.');
          Expect('Any');

          if At('(') then
          begin
            LArgs := Arguments;

            if Length(LArgs) <> 0 then
            begin
              Fail('Viewport Any has no arguments');
            end;
          end;
          while At('.') do
          begin
            Inc(FCursor);

            if FCursor >= Length(FTokens) then
            begin
              Fail('Expected a viewport condition method');
            end;
            LName := LowerCase(FTokens[FCursor].Text);
            Inc(FCursor);
            LArgs := Arguments;

            if (LName = 'orientation') and (Length(LArgs) = 1) and
              (LArgs[0].Kind = vkViewportOrientation) then
            begin
              Result.ViewportCondition := Result.ViewportCondition.Orientation(
                TNyxViewportOrientation(LArgs[0].Ordinal));
            end
            else if (Length(LArgs) = 1) and (LArgs[0].Kind = vkInteger) then
            begin
              LInteger := StrToInt(LArgs[0].Text);

              if LName = 'widthbelow' then
              begin
                Result.ViewportCondition := Result.ViewportCondition.WidthBelow(LInteger);
              end
              else if LName = 'widthatleast' then
              begin
                Result.ViewportCondition := Result.ViewportCondition.WidthAtLeast(LInteger);
              end
              else if LName = 'heightbelow' then
              begin
                Result.ViewportCondition := Result.ViewportCondition.HeightBelow(LInteger);
              end
              else if LName = 'heightatleast' then
              begin
                Result.ViewportCondition := Result.ViewportCondition.HeightAtLeast(LInteger);
              end
              else
              begin
                Fail('Unknown viewport condition method or argument family');
              end;
            end
            else if (Length(LArgs) = 2) and (LArgs[0].Kind = vkInteger) and
              (LArgs[1].Kind = vkInteger) then
            begin

              if LName = 'widthbetween' then
              begin
                Result.ViewportCondition := Result.ViewportCondition.WidthBetween(
                  StrToInt(LArgs[0].Text), StrToInt(LArgs[1].Text));
              end
              else if LName = 'heightbetween' then
              begin
                Result.ViewportCondition := Result.ViewportCondition.HeightBetween(
                  StrToInt(LArgs[0].Text), StrToInt(LArgs[1].Text));
              end
              else
              begin
                Fail('Unknown viewport interval method');
              end;
            end
            else
            begin
              Fail('Viewport methods require Integer bounds or a viewport orientation enum');
            end;
          end;
          Exit;
        end;

        if LName = 'tnyxviewportwidth' then
        begin
          Result.Kind := vkViewportWidth;
          Expect('.');

          if FCursor >= Length(FTokens) then
          begin
            Fail('Expected a viewport width factory');
          end;
          LName := LowerCase(FTokens[FCursor].Text);
          Inc(FCursor);
          LArgs := nil;

          if At('(') then
          begin
            LArgs := Arguments;
          end;

          if (LName = 'any') and (Length(LArgs) = 0) then
          begin
            Result.ViewportWidth := TNyxViewportWidth.Any;
          end
          else if (Length(LArgs) = 1) and (LArgs[0].Kind = vkInteger) and
            ((LName = 'below') or (LName = 'atleast')) then
          begin
            Result.ViewportWidth := TNyxViewportWidth.AtLeast(StrToInt(LArgs[0].Text));

            if LName = 'below' then
            begin
              Result.ViewportWidth := TNyxViewportWidth.Below(StrToInt(LArgs[0].Text));
            end;
          end
          else if (LName = 'between') and (Length(LArgs) = 2) and
            (LArgs[0].Kind = vkInteger) and (LArgs[1].Kind = vkInteger) then
          begin
            Result.ViewportWidth := TNyxViewportWidth.Between(StrToInt(LArgs[0].Text),
              StrToInt(LArgs[1].Text));
          end
          else
          begin
            Fail('Viewport width requires a known factory and Integer pixel bounds');
          end;
          Exit;
        end;

        if LName = 'tnyxlayoutpolicy' then
        begin
          { Evaluate only the closed public value builder, never arbitrary
            Pascal calls. Every method keeps its own enum argument family. }
          Result.Kind := vkLayoutPolicy;
          Expect('.');

          if FCursor >= Length(FTokens) then
          begin
            Fail('Expected a layout policy factory');
          end;
          LName := LowerCase(FTokens[FCursor].Text);
          Inc(FCursor);

          if LName = 'flow' then
          begin
            LArgs := Arguments;

            if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkLayout) then
            begin
              Fail('Flow requires a layout mode');
            end;
            Result.LayoutPolicy := TNyxLayoutPolicy.Flow(TNyxLayoutMode(LArgs[0].Ordinal));
          end
          else if (LName = 'row') or (LName = 'column') then
          begin
            Result.LayoutPolicy := TNyxLayoutPolicy.Row;

            if LName = 'column' then
            begin
              Result.LayoutPolicy := TNyxLayoutPolicy.Column;
            end;

            if At('(') then
            begin
              LArgs := Arguments;

              if Length(LArgs) <> 0 then
              begin
                Fail('Row/Column policy factories take no arguments');
              end;
            end;
          end
          else
          begin
            Fail('Unknown layout policy factory');
          end;
          while At('.') do
          begin
            Expect('.');

            if FCursor >= Length(FTokens) then
            begin
              Fail('Expected a layout policy method');
            end;
            LName := LowerCase(FTokens[FCursor].Text);
            Inc(FCursor);
            LArgs := Arguments;

            if Length(LArgs) <> 1 then
            begin
              Fail('A layout policy method requires one typed argument');
            end;

            if (LName = 'wrap') and (LArgs[0].Kind = vkFlowWrap) then
            begin
              Result.LayoutPolicy := Result.LayoutPolicy.Wrap(TNyxFlowWrap(LArgs[0].Ordinal));
            end
            else if (LName = 'align') and (LArgs[0].Kind = vkCrossAlignment) then
            begin
              Result.LayoutPolicy := Result.LayoutPolicy.Align(TNyxCrossAlignment(LArgs[0].Ordinal));
            end
            else if (LName = 'justify') and (LArgs[0].Kind = vkJustification) then
            begin
              Result.LayoutPolicy := Result.LayoutPolicy.Justify(TNyxJustification(LArgs[0].Ordinal));
            end
            else
            begin
              Fail('Wrong layout policy method or typed argument');
            end;
          end;
          Exit;
        end;

        LState := StateIndex(LName);

        if LState >= 0 then
        begin

          if not FStates[LState].Assigned then
          begin
            Fail('Initialize the typed state reference before using it');
          end;
          Result.Kind := vkStateRef;
          Result.Ordinal := Ord(FStates[LState].Kind);
          Result.Text := FStates[LState].Key;
          Exit;
        end;

        if (LName = 'true') or (LName = 'false') then
        begin
          Result.Kind := vkBoolean;
          Result.Ordinal := Ord(LName = 'true');
          Exit;
        end;

        if EnumValue(LName, Result) then
        begin
          Exit;
        end;

        for LKind := Low(TNyxStateKind) to High(TNyxStateKind) do
        begin

          if SameText(LName, 'Nyx' + SymbolStem(NyxStateKindName(LKind)) + 'Domain') then
          begin
            Exit(Domain(LKind));
          end;
        end;

        if (LName = 'newnyxmenudefinition') or (LName = 'nyxmenu') or
          (LName = 'newnyxmenubardefinition') or (LName = 'nyxmenubar') or
          (LName = 'nyxpopover') or (LName = 'nyxtypeahead') then
        begin
          Exit(MenuConstructor(LName));
        end;

        if (LName = 'nyxcollectionquery') or (LName = 'nyxwhere') or (LName = 'nyxsort') then
        begin
          Exit(QueryConstructor(LName));
        end;

        if (LName = 'nyxcollection') or (LName = 'nyxitem') or
          (LName = 'nyxcollectionschema') or (LName = 'nyxcollectionitem') or
          (LName = 'nyxtextfield') or (LName = 'nyxbooleanfield') or
          (LName = 'nyxintegerfield') or (LName = 'nyxnumberfield') or
          (LName = 'nyxnodomain') or (LName = 'tnyxstatevalue') or
          (LName = 'nyxcollectionview') then
        begin
          Exit(CollectionConstructor(LName));
        end;

        if (LName = 'nyxdata') or (LName = 'nyxnull') or
          (LName = 'nyxdecimal') or (LName = 'nyxfield') or
          (LName = 'nyxobject') or (LName = 'nyxarray') then
        begin
          Exit(DataConstructor(LName));
        end;

        if (LName = 'nyxnoeventvalue') or (LName = 'nyxtargetvalue') or
          (LName = 'nyxoriginvalue') or (LName = 'nyxsourcevalue') then
        begin

          if At('(') then
          begin
            Expect('(');
            Expect(')');
          end;
          Result.Kind := vkEventSource;
          Result.EventSource := NyxNoEventValue;

          if LName = 'nyxtargetvalue' then
          begin
            Result.EventSource := NyxTargetValue;
          end
          else if LName = 'nyxoriginvalue' then
          begin
            Result.EventSource := NyxOriginValue;
          end
          else if LName = 'nyxsourcevalue' then
          begin
            Result.EventSource := NyxSourceValue;
          end;
          Exit;
        end;

        if (LName = 'high') or (LName = 'low') then
        begin
          Expect('(');
          Expect('Integer');
          Expect(')');
          Result.Kind := vkInteger;
          Result.Text := IntToStr(High(Integer));

          if LName = 'low' then
          begin
            Result.Text := IntToStr(Low(Integer));
          end;
          Exit;
        end;
        LArgs := Arguments;

        if Length(LArgs) <> 1 then
        begin
          Fail('A typed value constructor requires one argument');
        end;

        if LName = 'nyxsemantic' then
        begin

          if LArgs[0].Kind <> vkSemanticEvent then
          begin
            Fail('NyxSemantic requires a TNyxSemanticEvent enum');
          end;
          Result.Kind := vkEvent;
          Result.Text := NyxSemantic(TNyxSemanticEvent(LArgs[0].Ordinal)).Name;
          Exit;
        end;

        if LName = 'tnyxtext' then
        begin

          if LArgs[0].Kind <> vkText then
          begin
            Fail('TNyxText requires text');
          end;
          Exit(LArgs[0]);
        end;

        if LName = 'nyxscalartext' then
        begin

          if LArgs[0].Kind <> vkInteger then
          begin
            Fail('NyxScalarText requires an Integer scalar');
          end;
          TryNyxStateInteger(LArgs[0].Text, LInteger);
          Result.Text := NyxScalarText(LInteger);
          Exit;
        end;

        if LName = 'nyxpartvalue' then
        begin

          if LArgs[0].Kind <> vkPart then
          begin
            Fail('NyxPartValue requires a typed part reference');
          end;
          Result.Kind := vkEventSource;
          Result.EventSource := NyxPartValue(NyxPart(LArgs[0].Text));
          Exit;
        end;

        if LArgs[0].Kind <> vkText then
        begin
          Fail('An open reference name requires text');
        end;
        Result.Text := LArgs[0].Text;
        for LKind := Low(TNyxStateKind) to High(TNyxStateKind) do
        begin

          if SameText(LName, CStateFactories[LKind]) then
          begin
            Result.Kind := vkStateRef;
            Result.Ordinal := Ord(LKind);
            case LKind of
              nskText: Result.Text := NyxTextState(Result.Text).Name;
              nskBoolean: Result.Text := NyxBooleanState(Result.Text).Name;
              nskInteger: Result.Text := NyxIntegerState(Result.Text).Name;
              nskNumber: Result.Text := NyxNumberState(Result.Text).Name;
            end;
            Exit;
          end;
        end;

        if (LName = 'nyxpageroot') or (LName = 'nyxreusableroot') then
        begin
          Result.Kind := vkRootRef;
          Result.RootRef := NyxPageRoot(Result.Text);

          if LName = 'nyxreusableroot' then
          begin
            Result.RootRef := NyxReusableRoot(Result.Text);
          end;
        end
        else if LName = 'nyxmenuref' then
        begin
          Result.Kind := vkMenuRef;
          Result.Text := NyxMenuRef(Result.Text).Name;
        end
        else if LName = 'nyxmenucommand' then
        begin
          Result.Kind := vkMenuCommand;
          Result.Text := NyxMenuCommand(Result.Text).Name;
        end
        else if LName = 'nyxmenugroup' then
        begin
          Result.Kind := vkMenuGroup;
          Result.Text := NyxMenuGroup(Result.Text).Name;
        end
        else if LName = 'nyxcontainer' then
        begin
          Result.Kind := vkContainerRef;
          Result.Text := NyxContainer(Result.Text).Name;
        end
        else if LName = 'nyxpresentation' then
        begin
          Result.Kind := vkPresentationRef;
          Result.Text := NyxPresentation(Result.Text).Name;
        end
        else if LName = 'nyxpart' then
        begin
          Result.Kind := vkPart;
        end
        else if LName = 'nyxevent' then
        begin
          Result.Kind := vkEvent;
        end
        else if LName = 'nyxcomponent' then
        begin
          Result.Kind := vkComponent;
        end
        else if LName = 'nyxstyle' then
        begin
          Result.Kind := vkStyle;
        end
        else if LName = 'nyxcustomkind' then
        begin
          Result.Kind := vkCustomKind;
        end
        else if LName = 'nyxextension' then
        begin
          Result.Kind := vkExtension;
          Result.Text := NyxExtension(Result.Text).Name;
        end
        else if LName = 'nyxhandler' then
        begin
          Result.Kind := vkHandler;
          Result.Text := NyxHandler(Result.Text).Name;
        end
        else if LName = 'nyxcallbackid' then
        begin
          Result.Kind := vkCallbackID;
          Result.Text := NyxCallbackID(Result.Text).Name;
        end
        else
        begin
          Fail('Unsupported value constructor ' + LToken.Text);
        end;
      end;
  else
    Fail('Unsupported configuration value');
  end;
end;

function TConfigurationReader.LocalIndex(const AName: TNyxText): Integer;
begin
  Result := FLocalNames.IndexOf(LowerCase(AName));
end;

function TConfigurationReader.StateIndex(const AName: TNyxText): Integer;
begin
  Result := FStateNames.IndexOf(LowerCase(AName));
end;

function TConfigurationReader.AdmittedNode(AIndex: Integer): TNyxNode;
begin

  if (AIndex < 0) or (AIndex >= Length(FLocals)) then
  begin
    Fail('A control member requires its declared local');
  end;

  if not FOwnedTry or not FLocals[AIndex].Created or
    not FLocals[AIndex].Admitted or (FControls[AIndex] = nil) then
  begin
    Fail('Construct and admit the control before using its fluent members');
  end;
  Result := FControls[AIndex].Node;

  if (Result = nil) or (Result.ID <> FLocals[AIndex].ID) then
  begin
    Fail('The retained control must preserve its constructed identity');
  end;
  { This is the Pascal local's reference, not an ID search through unrelated
    roots or implicit recipe parts. Full document validation still rejects all
    ambiguous/duplicate identities before either accepted owner is published.
    Builder grammar admits Add/Insert once, and cannot remove/rename a local's
    node; the existing parent/document and this interface retain its lifetime. }
end;

procedure TConfigurationReader.ReferenceAssignment(AIndex: Integer);
var
  LValue: TValue;
begin
  Inc(FCursor);
  Expect(':=');
  LValue := Expression;
  Expect(';');

  if (LValue.Kind <> vkStateRef) or
    (LValue.Ordinal <> Ord(FStates[AIndex].Kind)) then
  begin
    Fail('Reference initialization must match its declared Pascal scalar family');
  end;
  FStates[AIndex].Key := LValue.Text;
  FStates[AIndex].Assigned := True;
end;

procedure TConfigurationReader.Defaults;
var
  LMethod: TNyxText;
  LArgs: TValues;
  LValue: TValue;
  LKind: TNyxStateKind;
  LInteger: Integer;
  LNumber: Double;
  LAny: Boolean;
begin
  Inc(FCursor, 3);
  LAny := False;
  while At('.') do
  begin
    Expect('.');

    if (FCursor >= Length(FTokens)) or (FTokens[FCursor].Kind <> tkWord) then
    begin
      Fail('Expected a typed state method');
    end;
    LMethod := LowerCase(FTokens[FCursor].Text);
    Inc(FCursor);
    LArgs := Arguments;

    if (Length(LArgs) = 0) or (LArgs[0].Kind <> vkStateRef) then
    begin
      Fail('State methods require a typed state reference');
    end;
    LKind := TNyxStateKind(LArgs[0].Ordinal);

    if LMethod = 'remove' then
    begin

      if Length(LArgs) <> 1 then
      begin
        Fail('Remove requires one typed reference');
      end;

      if FApply then
      begin
        case LKind of
          nskText: FDocument.State.Remove(NyxTextState(LArgs[0].Text));
          nskBoolean: FDocument.State.Remove(NyxBooleanState(LArgs[0].Text));
          nskInteger: FDocument.State.Remove(NyxIntegerState(LArgs[0].Text));
          nskNumber: FDocument.State.Remove(NyxNumberState(LArgs[0].Text));
        end;
      end;
    end
    else if LMethod = 'setvalue' then
    begin

      if Length(LArgs) <> 2 then
      begin
        Fail('SetValue requires a typed reference and scalar default');
      end;
      LValue := LArgs[1];
      case LKind of
        nskText:
          begin

            if LValue.Kind <> vkText then
            begin
              Fail('Text state requires a text default');
            end;

            if FApply then
            begin
              FDocument.State.SetValue(NyxTextState(LArgs[0].Text), LValue.Text);
            end;
          end;
        nskBoolean:
          begin

            if LValue.Kind <> vkBoolean then
            begin
              Fail('Boolean state requires a Boolean default');
            end;

            if FApply then
            begin
              FDocument.State.SetValue(NyxBooleanState(LArgs[0].Text), LValue.Ordinal <> 0);
            end;
          end;
        nskInteger:
          begin

            if LValue.Kind <> vkInteger then
            begin
              Fail('Integer state requires a signed Integer default');
            end;
            TryNyxStateInteger(LValue.Text, LInteger);

            if FApply then
            begin
              FDocument.State.SetValue(NyxIntegerState(LArgs[0].Text), LInteger);
            end;
          end;
        nskNumber:
          begin

            if LValue.Kind = vkInteger then
            begin
              { Pascal widens an Integer argument to Double for this overload.
                Preserve that language rule without accepting text coercion. }
              TryNyxStateInteger(LValue.Text, LInteger);
              LNumber := LInteger;
            end
            else if LValue.Kind = vkNumber then
            begin
              TryNyxStateNumber(LValue.Text, LNumber);
            end
            else
            begin
              Fail('Number state requires a finite numeric default');
            end;

            if FApply then
            begin
              FDocument.State.SetValue(NyxNumberState(LArgs[0].Text), LNumber);
            end;
          end;
      end;
    end
    else
    begin
      Fail('Unsupported state method ' + LMethod);
    end;
    LAny := True;
  end;

  if not LAny then
  begin
    Fail('Result.State requires a typed method call');
  end;
  Expect(';');
end;

procedure TConfigurationReader.Bindings(AIndex: Integer);
var
  LNode: TNyxNode;
  LMethod: TNyxText;
  LArgs: TValues;
  LTarget: TNyxBindingProperty;
  LDirection: TNyxBindingDirection;
  LFound: Boolean;
  LSpec: TNyxBindingSpec;
begin
  Inc(FCursor, 3);
  LNode := AdmittedNode(AIndex);
  while At('.') do
  begin
    Expect('.');

    if (FCursor >= Length(FTokens)) or (FTokens[FCursor].Kind <> tkWord) then
    begin
      Fail('Expected a fluent binding method');
    end;
    LMethod := FTokens[FCursor].Text;
    Inc(FCursor);

    if SameText(LMethod, 'Done') then
    begin
      Expect(';');
      Exit;
    end;
    LArgs := nil;

    if At('(') then
    begin
      LArgs := Arguments;
    end;

    if SameText(LMethod, 'Collection') then
    begin

      if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkCollectionView) then
      begin
        Fail('Collection requires a typed NyxCollectionView');
      end;

      if FApply then
      begin
        LNode.Binds.Collection(LArgs[0].CollectionView);
      end;
      Continue;
    end;

    if SameText(LMethod, 'ClearCollection') or SameText(LMethod, 'InheritCollection') then
    begin

      if Length(LArgs) <> 0 then
      begin
        Fail('ClearCollection/InheritCollection accepts no arguments');
      end;

      if FApply then
      begin

        if SameText(LMethod, 'ClearCollection') then
        begin
          LNode.Binds.ClearCollection;
        end
        else
        begin
          LNode.Binds.InheritCollection;
        end;
      end;
      Continue;
    end;

    if SameText(LMethod, 'Clear') or SameText(LMethod, 'Inherit') then
    begin

      if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkBindingProperty) then
      begin
        Fail('Clear/Inherit requires a TNyxBindingProperty');
      end;
      LTarget := TNyxBindingProperty(LArgs[0].Ordinal);

      if FApply then
      begin

        if SameText(LMethod, 'Clear') then
        begin
          LNode.Binds.Clear(LTarget);
        end
        else
        begin
          LNode.Binds.Inherit(LTarget);
        end;
      end;
      Continue;
    end;
    LFound := False;
    for LTarget := Low(TNyxBindingProperty) to High(TNyxBindingProperty) do
    begin

      if SameText(LMethod, CBindingMethods[LTarget]) then
      begin
        LFound := True;
        Break;
      end;
    end;

    if not LFound then
    begin
      Fail('Unsupported Binds method ' + LMethod);
    end;

    if (Length(LArgs) = 1) and (LArgs[0].Kind = vkResourceValue) then
    begin
      LSpec := TNyxBindingSpec.Resource(LTarget, LArgs[0].ResourceValue);

      if FApply then
      begin
        LNode.SetBinding(LSpec);
      end;
      Continue;
    end;

    if (Length(LArgs) = 1) and (LArgs[0].Kind = vkResourceImage) then
    begin

      if LTarget <> bpImage then
      begin
        Fail('A typed image resource can only bind Image');
      end;
      LSpec := TNyxBindingSpec.Image(LArgs[0].ResourceImage);

      if FApply then
      begin
        LNode.SetBinding(LSpec);
      end;
      Continue;
    end;

    if (Length(LArgs) < 1) or (Length(LArgs) > 2) or
      (LArgs[0].Kind <> vkStateRef) then
    begin
      Fail('A binding requires a typed state reference');
    end;
    LDirection := bdFromState;

    if LTarget = bpValue then
    begin
      LDirection := bdTwoWay;
    end;

    if Length(LArgs) = 2 then
    begin

      if (LTarget <> bpValue) or (LArgs[1].Kind <> vkBindingDirection) then
      begin
        Fail('Only Value accepts a TNyxBindingDirection argument');
      end;
      LDirection := TNyxBindingDirection(LArgs[1].Ordinal);
    end;
    { The immutable public descriptor checks the same reference/target families
      as Binds' compile-time overloads. Whole-document admission additionally
      checks concrete domains, inherited values and required default existence. }
    LSpec := TNyxBindingSpec.Bound(LTarget, LArgs[0].Text,
      TNyxStateKind(LArgs[0].Ordinal), LDirection);

    if FApply then
    begin
      LNode.SetBinding(LSpec);
    end;
  end;
  Fail('Finish the Binds block with .Done;');
end;

procedure TConfigurationReader.Contract(AIndex: Integer);
var
  LNode: TNyxNode;
  LMethod: TNyxText;
  LArgs: TValues;
  LDomain: TValue;
  LPart: TNyxPartRef;
  LTrigger: TNyxTrigger;
  LSource: TNyxEventValueRef;
  LAny: Boolean;
begin
  Inc(FCursor, 3);
  LNode := AdmittedNode(AIndex);
  LAny := False;
  while At('.') do
  begin
    Expect('.');

    if (FCursor >= Length(FTokens)) or (FTokens[FCursor].Kind <> tkWord) then
    begin
      Fail('Expected a typed contract method');
    end;
    LMethod := FTokens[FCursor].Text;
    Inc(FCursor);
    LAny := True;

    if SameText(LMethod, 'NoValue') then
    begin

      if At('(') then
      begin
        Expect('(');
        Expect(')');
      end;

      if FApply then
      begin
        LNode.Contract.NoValue;
      end;
      Continue;
    end;
    LArgs := Arguments;

    if SameText(LMethod, 'Metadata') then
    begin

      if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkData) then
      begin
        Fail('Contract Metadata requires typed structured data');
      end;

      if FApply then
      begin
        LNode.Contract.Metadata(LArgs[0].Data);
      end;
      Continue;
    end;

    if SameText(LMethod, 'Signal') then
    begin

      if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkTrigger) then
      begin
        Fail('Signal requires a TNyxTrigger');
      end;

      if FApply then
      begin
        LNode.Contract.Signal(TNyxTrigger(LArgs[0].Ordinal));
      end;
      Continue;
    end;

    if SameText(LMethod, 'Value') then
    begin

      if (Length(LArgs) <> 1) or
        not (LArgs[0].Kind in [vkDomain, vkClockDomain, vkValueDomain]) then
      begin
        Fail('Contract Value requires a typed scalar domain');
      end;
      LDomain := LArgs[0];
    end
    else if SameText(LMethod, 'Field') then
    begin

      if (Length(LArgs) <> 2) or (LArgs[0].Kind <> vkPart) or
        not (LArgs[1].Kind in [vkDomain, vkClockDomain]) then
      begin
        Fail('Field requires a typed part reference and scalar domain');
      end;
      LPart := NyxPart(LArgs[0].Text);
      LDomain := LArgs[1];
    end
    else if SameText(LMethod, 'On') then
    begin

      if (Length(LArgs) <> 3) or (LArgs[0].Kind <> vkTrigger) or
        (LArgs[1].Kind <> vkEventSource) or
        not (LArgs[2].Kind in [vkDomain, vkClockDomain]) then
      begin
        Fail('On requires a trigger, typed event value source and scalar domain');
      end;
      LTrigger := TNyxTrigger(LArgs[0].Ordinal);
      LSource := LArgs[1].EventSource.Copy;
      LDomain := LArgs[2];
    end
    else
    begin
      Fail('Unsupported Contract method ' + LMethod);
    end;

    if not FApply then
    begin
      Continue;
    end;
    { Invoke the actual public overload for each family. Descriptor construction
      is reserved for the explicit Metadata boundary, never an implicit shortcut
      that accepts a domain the Pascal compiler would refuse for this call. }

    if LDomain.Kind = vkValueDomain then
    begin
      LNode.Contract.Value(LDomain.ValueDomain);
      Continue;
    end;

    if LDomain.Kind = vkClockDomain then
    begin

      if SameText(LMethod, 'Value') then
      begin
        LNode.Contract.Value(LDomain.TimeDomain);
      end
      else if SameText(LMethod, 'Field') then
      begin
        LNode.Contract.Field(LPart, LDomain.TimeDomain);
      end
      else
      begin
        LNode.Contract.On(LTrigger, LSource, LDomain.TimeDomain);
      end;
      Continue;
    end;
    case TNyxStateKind(LDomain.Ordinal) of
      nskText:
        begin

          if SameText(LMethod, 'Value') then
          begin
            LNode.Contract.Value(LDomain.TextDomain);
          end
          else if SameText(LMethod, 'Field') then
          begin
            LNode.Contract.Field(LPart, LDomain.TextDomain);
          end
          else
          begin
            LNode.Contract.On(LTrigger, LSource, LDomain.TextDomain);
          end;
        end;
      nskBoolean:
        begin

          if SameText(LMethod, 'Value') then
          begin
            LNode.Contract.Value(LDomain.BooleanDomain);
          end
          else if SameText(LMethod, 'Field') then
          begin
            LNode.Contract.Field(LPart, LDomain.BooleanDomain);
          end
          else
          begin
            LNode.Contract.On(LTrigger, LSource, LDomain.BooleanDomain);
          end;
        end;
      nskInteger:
        begin

          if SameText(LMethod, 'Value') then
          begin
            LNode.Contract.Value(LDomain.IntegerDomain);
          end
          else if SameText(LMethod, 'Field') then
          begin
            LNode.Contract.Field(LPart, LDomain.IntegerDomain);
          end
          else
          begin
            LNode.Contract.On(LTrigger, LSource, LDomain.IntegerDomain);
          end;
        end;
      nskNumber:
        begin

          if SameText(LMethod, 'Value') then
          begin
            LNode.Contract.Value(LDomain.NumberDomain);
          end
          else if SameText(LMethod, 'Field') then
          begin
            LNode.Contract.Field(LPart, LDomain.NumberDomain);
          end
          else
          begin
            LNode.Contract.On(LTrigger, LSource, LDomain.NumberDomain);
          end;
        end;
    end;
  end;

  if not LAny then
  begin
    Fail('Contract requires a typed method call');
  end;
  Expect(';');
end;

procedure TConfigurationReader.Extensions(AIndex: Integer);
var
  LStore: TNyxExtensions;
  LMethod: TNyxText;
  LArgs: TValues;
  LKey: TNyxExtensionRef;
  LAny: Boolean;
begin
  Inc(FCursor, 3);
  LStore := FDocument.Extensions;

  if AIndex >= 0 then
  begin
    LStore := AdmittedNode(AIndex).Extensions;
  end;
  LAny := False;
  while At('.') do
  begin
    Expect('.');

    if (FCursor >= Length(FTokens)) or (FTokens[FCursor].Kind <> tkWord) then
    begin
      Fail('Expected an extension-store method');
    end;
    LMethod := FTokens[FCursor].Text;
    Inc(FCursor);
    LArgs := Arguments;

    if (Length(LArgs) = 0) or (LArgs[0].Kind <> vkExtension) then
    begin
      Fail('Extension methods require a typed extension reference');
    end;
    LKey := NyxExtension(LArgs[0].Text);

    if SameText(LMethod, 'SetValue') then
    begin

      if (Length(LArgs) <> 2) or (LArgs[1].Kind <> vkData) then
      begin
        Fail('Extension SetValue requires a typed data value');
      end;

      if FApply then
      begin
        LStore.SetValue(LKey, LArgs[1].Data);
      end;
    end
    else if SameText(LMethod, 'Remove') then
    begin

      if Length(LArgs) <> 1 then
      begin
        Fail('Extension Remove requires one typed reference');
      end;

      if FApply then
      begin
        LStore.Remove(LKey);
      end;
    end
    else
    begin
      Fail('Unsupported extension-store method ' + LMethod);
    end;
    LAny := True;
  end;

  if not LAny then
  begin
    Fail('Extensions requires a typed method call');
  end;
  Expect(';');
end;

procedure TConfigurationReader.ApplyCall(ANode: TNyxNode;
  const AMethod: TNyxText; const AArgs: TValues; APlatform: TNyxPlatform;
  const AViewport: TNyxViewportCondition; const APresentation: TNyxPresentationRef);
var
  LAttribute: TNyxAttribute;
  LMethod: TNyxText;
  LValue: TValue;
  LInteger: Integer;
  LNumber: Double;
  LFound: Boolean;
  LConfigure: TNyxNodeConfig;

  procedure Require(AKind: TValueKind);
  begin

    if LValue.Kind <> AKind then
    begin
      Fail('Wrong typed argument for ' + AMethod);
    end;
  end;

begin
  LConfigure := ANode.Configure.ForPlatform(APlatform).WhenViewport(AViewport);

  if APresentation.Defined then
  begin
    LConfigure := LConfigure.WhenPresentation(APresentation);
  end;
  LMethod := LowerCase(AMethod);

  if (LMethod = 'nomenu') or (LMethod = 'inheritmenu') then
  begin

    if Length(AArgs) <> 0 then
    begin
      Fail(AMethod + ' takes no arguments');
    end;

    if LMethod = 'nomenu' then
    begin
      LConfigure.NoMenu;
    end
    else
    begin
      LConfigure.InheritMenu;
    end;
    Exit;
  end;

  if LMethod = 'menu' then
  begin

    if (Length(AArgs) <> 1) or (AArgs[0].Kind <> vkMenuRef) then
    begin
      Fail('Menu requires a typed menu reference');
    end;
    LConfigure.Menu(NyxMenuRef(AArgs[0].Text));
    Exit;
  end;

  if LMethod = 'menubar' then
  begin

    if (Length(AArgs) <> 1) or (AArgs[0].Kind <> vkMenuBarDefinition) then
    begin
      Fail('MenuBar requires an immutable typed row grouping');
    end;
    LConfigure.MenuBar(FMenuBarDefinitions[AArgs[0].Ordinal]);
    Exit;
  end;

  if (LMethod = 'nomenubar') or (LMethod = 'inheritmenubar') then
  begin

    if Length(AArgs) <> 0 then
    begin
      Fail('Menu bar mask/inheritance takes no arguments');
    end;

    if LMethod = 'nomenubar' then
    begin
      LConfigure.NoMenuBar;
    end
    else
    begin
      LConfigure.InheritMenuBar;
    end;
    Exit;
  end;

  if LMethod = 'extension' then
  begin

    if (Length(AArgs) <> 2) or (AArgs[0].Kind <> vkText) or
      (AArgs[1].Kind <> vkText) then
    begin
      Fail('Extension requires a text key and value');
    end;
    LConfigure.Extension(AArgs[0].Text, AArgs[1].Text);
    Exit;
  end;

  if LMethod = 'metadata' then
  begin

    if (Length(AArgs) <> 2) or (AArgs[0].Kind <> vkAttribute) or
      (AArgs[1].Kind <> vkText) then
    begin
      Fail('Metadata requires a TNyxAttribute and text');
    end;
    LConfigure.Metadata(TNyxAttribute(AArgs[0].Ordinal), AArgs[1].Text);
    Exit;
  end;

  if Length(AArgs) <> 1 then
  begin
    Fail(AMethod + ' requires one argument');
  end;
  LValue := AArgs[0];

  if LMethod = 'clear' then
  begin
    Require(vkAttribute);
    LConfigure.Clear(TNyxAttribute(LValue.Ordinal));
    Exit;
  end;

  if LMethod = 'constraints' then
  begin
    Require(vkSizeConstraints);
    LConfigure.Constraints(LValue.SizeConstraints);
    Exit;
  end;

  if LMethod = 'customvariant' then
  begin
    Require(vkStyle);
    LConfigure.CustomVariant(NyxStyle(LValue.Text));
    Exit;
  end;

  if LMethod = 'customprojection' then
  begin
    Require(vkCustomKind);
    LConfigure.CustomProjection(NyxCustomKind(LValue.Text));
    Exit;
  end;
  LFound := False;
  for LAttribute := Low(TNyxAttribute) to High(TNyxAttribute) do
  begin

    if (CMethods[LAttribute] <> '') and SameText(CMethods[LAttribute], AMethod) then
    begin
      LFound := True;
      Break;
    end;
  end;

  if not LFound then
  begin
    Fail('Unsupported Configure method ' + AMethod);
  end;
  case LAttribute of
    atText..atAlt:
      begin

        if LAttribute = atSource then
        begin

          if not (LValue.Kind in [vkText, vkImageSource]) then
          begin
            Fail('Source requires a typed image or explicit legacy resource text');
          end;
        end
        else if LAttribute <> atValue then
        begin
          Require(vkText);
        end;
      end;
    atPadding..atMaximum: Require(vkInteger);
    atEnabled..atPressed: Require(vkBoolean);
    atLayout:
      begin

        if not (LValue.Kind in [vkLayout, vkLayoutPolicy]) then
        begin
          Fail('Layout requires its mode or fluent policy value');
        end;
      end;
    atFlowWrap: Require(vkFlowWrap);
    atQueryContainer: Require(vkContainerRef);
    atContainerContainment: Require(vkContainerContainment);
    atSliderIntervals: Require(vkInteger);
    atImageFit: Require(vkImageFit);
    atImageHorizontal, atImageVertical: Require(vkImageAnchor);
    atCrossAlignment: Require(vkCrossAlignment);
    atJustification: Require(vkJustification);
    atWidthSizing, atHeightSizing: Require(vkSizing);
    atMinimumWidth, atMaximumWidth, atMinimumHeight, atMaximumHeight: Require(vkInteger);
    atSplitOrientation: Require(vkSplitOrientation);
    atSplitPosition, atSplitMinimum, atSplitMaximum: Require(vkInteger);
    atSplitResizable, atDragSource, atDropTarget: Require(vkBoolean);
    atTouchBehavior: Require(vkTouchBehavior);
    atVariant: Require(vkVariant);
    atAction: Require(vkAction);
    atProjection: Require(vkKind);
    atOverrideMode: Require(vkOverride);
    atInputType: Require(vkInput);
    atPart, atTarget, atPath: Require(vkPart);
    atComponent: Require(vkComponent);
    atEmit, atEmitChange: Require(vkEvent);
    atOption:
      begin

        if not (LValue.Kind in [vkText, vkBoolean, vkInteger, vkNumber]) then
        begin
          Fail('Option requires a scalar typed value');
        end;
      end;
    atDesignID: Fail('Use Metadata for an explicit design identity');
  end;

  if LValue.Kind = vkInteger then
  begin
    TryNyxStateInteger(LValue.Text, LInteger);
  end;

  if LValue.Kind = vkNumber then
  begin
    TryNyxStateNumber(LValue.Text, LNumber);
  end;
  case LAttribute of
    atText: LConfigure.Text(LValue.Text);
    atPlaceholder: LConfigure.Placeholder(LValue.Text);
    atItems: LConfigure.Items(LValue.Text);
    atHint: LConfigure.Hint(LValue.Text);
    atAccessibleName: LConfigure.AccessibleName(LValue.Text);
    atHref: LConfigure.LinkTo(LValue.Text);
    atSource:
      begin

        if LValue.Kind = vkImageSource then
        begin
          LConfigure.Source(LValue.ImageSource);
        end
        else
        begin
          LConfigure.Source(LValue.Text);
        end;
      end;
    atImageFit: LConfigure.ImageFit(TNyxImageFit(LValue.Ordinal));
    atImageHorizontal: LConfigure.ImageHorizontal(TNyxImageAnchor(LValue.Ordinal));
    atImageVertical: LConfigure.ImageVertical(TNyxImageAnchor(LValue.Ordinal));
    atAlt: LConfigure.AlternativeText(LValue.Text);
    atValue, atOption:
      begin
        case LValue.Kind of
          vkRGBColor:
            begin

              if LAttribute <> atValue then
              begin
                Fail('RGB colors are typed Value arguments, never Option');
              end;
              LConfigure.Value(LValue.RGBColor);
            end;
          vkClockTime:
            begin

              if LAttribute <> atValue then
              begin
                Fail('Clock times are typed Value arguments, never Option');
              end;
              LConfigure.Value(LValue.ClockTime);
            end;
          vkCalendarDate:
            begin

              if LAttribute <> atValue then
              begin
                Fail('Calendar dates are typed Value arguments, never Option');
              end;
              LConfigure.Value(LValue.CalendarDate);
            end;
          vkText:
            begin

              if LAttribute = atValue then
              begin
                LConfigure.Value(LValue.Text);
              end
              else
              begin
                LConfigure.Option(LValue.Text);
              end;
            end;
          vkInteger:
            begin

              if LAttribute = atValue then
              begin
                LConfigure.Value(LInteger);
              end
              else
              begin
                LConfigure.Option(LInteger);
              end;
            end;
          vkNumber:
            begin

              if LAttribute = atValue then
              begin
                LConfigure.Value(LNumber);
              end
              else
              begin
                LConfigure.Option(LNumber);
              end;
            end;
          vkBoolean:
            begin

              if LAttribute = atValue then
              begin
                LConfigure.Value(LValue.Ordinal <> 0);
              end
              else
              begin
                LConfigure.Option(LValue.Ordinal <> 0);
              end;
            end;
        else
          Fail('Value requires a typed scalar, CalendarDate or ClockTime; Option requires a scalar');
        end;
      end;
    atLayout:
      begin

        if LValue.Kind = vkLayoutPolicy then
        begin
          LConfigure.Layout(LValue.LayoutPolicy);
        end
        else
        begin
          LConfigure.Layout(TNyxLayoutMode(LValue.Ordinal));
        end;
      end;
    atFlowWrap: LConfigure.Wrap(TNyxFlowWrap(LValue.Ordinal));
    atCrossAlignment: LConfigure.Align(TNyxCrossAlignment(LValue.Ordinal));
    atJustification: LConfigure.Justify(TNyxJustification(LValue.Ordinal));
    atWidthSizing: LConfigure.WidthSizing(TNyxSizing(LValue.Ordinal));
    atHeightSizing: LConfigure.HeightSizing(TNyxSizing(LValue.Ordinal));
    atSplitOrientation: LConfigure.SplitOrientation(TNyxSplitOrientation(LValue.Ordinal));
    atSplitPosition: LConfigure.SplitPosition(LInteger);
    atSplitMinimum: LConfigure.SplitMinimum(LInteger);
    atSplitMaximum: LConfigure.SplitMaximum(LInteger);
    atSplitResizable: LConfigure.SplitResizable(LValue.Ordinal <> 0);
    atDragSource: LConfigure.DragSource(LValue.Ordinal <> 0);
    atDropTarget: LConfigure.DropTarget(LValue.Ordinal <> 0);
    atTouchBehavior: LConfigure.TouchBehavior(TNyxTouchBehavior(LValue.Ordinal));
    atPadding: LConfigure.Padding(LInteger);
    atGap: LConfigure.Gap(LInteger);
    atColumns: LConfigure.Columns(LInteger);
    atWidth: LConfigure.Width(LInteger);
    atHeight: LConfigure.Height(LInteger);
    atMinimumWidth: LConfigure.MinimumWidth(LInteger);
    atMaximumWidth: LConfigure.MaximumWidth(LInteger);
    atMinimumHeight: LConfigure.MinimumHeight(LInteger);
    atMaximumHeight: LConfigure.MaximumHeight(LInteger);
    atQueryContainer: LConfigure.QueryContainer(NyxContainer(LValue.Text));
    atContainerContainment: LConfigure.Containment(TNyxContainerContainment(LValue.Ordinal));
    atLeft: LConfigure.Left(LInteger);
    atTop: LConfigure.Top(LInteger);
    atFlex: LConfigure.Flex(LInteger);
    atMinimum: LConfigure.Minimum(LInteger);
    atMaximum: LConfigure.Maximum(LInteger);
    atSliderIntervals: LConfigure.SliderIntervals(LInteger);
    atEnabled: LConfigure.Enabled(LValue.Ordinal <> 0);
    atVisible: LConfigure.Visible(LValue.Ordinal <> 0);
    atReadOnly: LConfigure.ReadOnly(LValue.Ordinal <> 0);
    atSurface: LConfigure.Surface(LValue.Ordinal <> 0);
    atCompound: LConfigure.Compound(LValue.Ordinal <> 0);
    atPressed: LConfigure.Pressed(LValue.Ordinal <> 0);
    atVariant: LConfigure.Variant(TNyxVariant(LValue.Ordinal));
    atAction: LConfigure.Action(TNyxAction(LValue.Ordinal));
    atProjection: LConfigure.ProjectAs(TNyxKind(LValue.Ordinal));
    atOverrideMode: LConfigure.OverrideMode(TNyxOverrideMode(LValue.Ordinal));
    atInputType: LConfigure.InputType(TNyxInputType(LValue.Ordinal));
    atPart: LConfigure.PartName(NyxPart(LValue.Text));
    atTarget: LConfigure.Target(NyxPart(LValue.Text));
    atPath: LConfigure.OverridePath(NyxPart(LValue.Text));
    atComponent: LConfigure.Component(NyxComponent(LValue.Text));
    atEmit: LConfigure.OnClick(NyxEvent(LValue.Text));
    atEmitChange: LConfigure.OnChange(NyxEvent(LValue.Text));
  else
    Fail('Unsupported configuration attribute');
  end;
end;

procedure TConfigurationReader.Configure(AIndex: Integer);
var
  LNode: TNyxNode;
  LMethod: TNyxText;
  LArgs: TValues;
  LPlatform: TNyxPlatform;
  LViewport: TNyxViewportCondition;
  LPresentation: TNyxPresentationRef;
begin
  LPlatform := npfAny;
  LViewport := TNyxViewportCondition.Any;
  LPresentation := Default(TNyxPresentationRef);

  if FLocals[AIndex].Configured then
  begin
    Fail('Keep one Configure block per control');
  end;
  FLocals[AIndex].Configured := True;
  Inc(FCursor, 3);
  LNode := AdmittedNode(AIndex);
  while At('.') do
  begin
    Inc(FCursor);

    if (FCursor >= Length(FTokens)) or (FTokens[FCursor].Kind <> tkWord) then
    begin
      Fail('Expected a Configure method');
    end;
    LMethod := FTokens[FCursor].Text;
    Inc(FCursor);

    if SameText(LMethod, 'Done') then
    begin
      Expect(';');
      Exit;
    end;
    { Explicit local menu/bar masks and inheritance follow the public
      parameterless fluent spelling. Other methods require an argument frame. }
    LArgs := nil;

    if At('(') or (not SameText(LMethod, 'NoMenu') and
      not SameText(LMethod, 'InheritMenu') and not SameText(LMethod, 'NoMenuBar') and
      not SameText(LMethod, 'InheritMenuBar')) then
    begin
      LArgs := Arguments;
    end;

    if SameText(LMethod, 'ForPlatform') then
    begin

      if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkPlatform) then
      begin
        Fail('ForPlatform requires a TNyxPlatform enum');
      end;
      LPlatform := TNyxPlatform(LArgs[0].Ordinal);
    end
    else if SameText(LMethod, 'WhenViewport') then
    begin

      if (Length(LArgs) <> 1) or not
        (LArgs[0].Kind in [vkViewportWidth, vkViewportCondition]) then
      begin
        Fail('WhenViewport requires a typed viewport width or condition');
      end;
      LPresentation := Default(TNyxPresentationRef);
      if LArgs[0].Kind = vkViewportWidth then
      begin
        LViewport := TNyxViewportCondition.FromWidth(LArgs[0].ViewportWidth);
      end
      else
      begin
        LViewport := LArgs[0].ViewportCondition;
      end;
    end
    else if SameText(LMethod, 'WhenPresentation') then
    begin

      if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkPresentationRef) then
      begin
        Fail('WhenPresentation requires a typed presentation reference');
      end;
      LPresentation := NyxPresentation(LArgs[0].Text);
      LViewport := TNyxViewportCondition.Any;
    end
    else if FApply then
    begin
      ApplyCall(LNode, LMethod, LArgs, LPlatform, LViewport, LPresentation);
    end;
  end;
  Fail('Finish the Configure block with .Done;');
end;

procedure TConfigurationReader.Content(AIndex: Integer);
var
  LContent: INyxContent;
  LMethod: TNyxText;
  LArgs: TValues;
begin

  if FLocals[AIndex].ContentConfigured then
  begin
    Fail('Keep one Content block per reusable instance');
  end;
  FLocals[AIndex].ContentConfigured := True;
  Inc(FCursor, 3);
  LContent := NewNyxContent;

  if FApply then
  begin
    LContent := AdmittedNode(AIndex).Content;
  end;
  while At('.') do
  begin
    Inc(FCursor);

    if (FCursor >= Length(FTokens)) or (FTokens[FCursor].Kind <> tkWord) then
    begin
      Fail('Expected a Content method');
    end;
    LMethod := FTokens[FCursor].Text;
    Inc(FCursor);

    if SameText(LMethod, 'Done') then
    begin
      Expect(';');
      Exit;
    end;

    if SameText(LMethod, 'Clear') then
    begin
      LContent.Clear;
      Continue;
    end;
    LArgs := Arguments;

    if SameText(LMethod, 'Use') then
    begin

      if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkComponent) then
      begin
        Fail('Content.Use requires a typed component reference');
      end;
      LContent.Use(NyxComponent(LArgs[0].Text));
    end
    else if SameText(LMethod, 'ForPlatform') then
    begin

      if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkPlatform) then
      begin
        Fail('Content.ForPlatform requires a TNyxPlatform enum');
      end;
      LContent := LContent.ForPlatform(TNyxPlatform(LArgs[0].Ordinal));
    end
    else if SameText(LMethod, 'WhenPresentation') then
    begin

      if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkPresentationRef) then
      begin
        Fail('Content.WhenPresentation requires a typed presentation reference');
      end;
      LContent := LContent.WhenPresentation(NyxPresentation(LArgs[0].Text));
    end
    else if SameText(LMethod, 'WhenViewport') then
    begin

      if (Length(LArgs) <> 1) or not (LArgs[0].Kind in [vkViewportWidth, vkViewportCondition]) then
      begin
        Fail('Content.WhenViewport requires a typed viewport condition');
      end;

      if LArgs[0].Kind = vkViewportWidth then
      begin
        LContent := LContent.WhenViewport(LArgs[0].ViewportWidth);
      end
      else
      begin
        LContent := LContent.WhenViewport(LArgs[0].ViewportCondition);
      end;
    end
    else
    begin
      Fail('Unsupported Content method: ' + LMethod);
    end;
  end;
  Fail('Finish the Content block with .Done;');
end;

procedure TConfigurationReader.Callbacks;
var
  LLocal: Integer;
  LTrigger: TNyxTrigger;
  LName: TNyxEventRef;
  LMethod: TNyxText;
  LArgs: TValues;
  LEvents: INyxAuthoredEvents;
  LStream: INyxAuthoredEvent;
  LMatch: TNyxTrigger;
  LFound: Boolean;
begin
  Expect('NyxCallbacks');
  Expect('(');

  if FCursor >= Length(FTokens) then
  begin
    Fail('Callbacks require an owned specialized control');
  end;
  LLocal := LocalIndex(FTokens[FCursor].Text);

  if (LLocal < 0) or not FOwnedTry or not FLocals[LLocal].Admitted then
  begin
    Fail('Admit a control before declaring its callbacks');
  end;
  Inc(FCursor);
  Expect(')');
  Expect('.');
  LMethod := LowerCase(FTokens[FCursor].Text);
  Inc(FCursor);

  if FApply then
  begin
    LEvents := NyxCallbacks(AdmittedNode(LLocal));
  end;

  if (LMethod = 'metadata') or (LMethod = 'clear') or (LMethod = 'inherit') then
  begin

    if LMethod = 'metadata' then
    begin
      LArgs := Arguments;

      if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkData) then
      begin
        Fail('Callback metadata requires structured typed data');
      end;

      if FApply then
      begin
        LEvents.Metadata(LArgs[0].Data);
      end;
    end
    else if FApply then
    begin

      if LMethod = 'clear' then
      begin
        LEvents.Clear;
      end
      else
      begin
        LEvents.Inherit;
      end;
    end;
    Expect(';');
    Exit;
  end;
  LTrigger := ntClick;
  LName := Default(TNyxEventRef);

  if LMethod = 'onnamed' then
  begin
    LArgs := Arguments;

    if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkEvent) then
    begin
      Fail('OnNamed requires a typed event reference');
    end;
    LName := NyxEvent(LArgs[0].Text);

    if Trim(LName.Name) = '' then
    begin
      Fail('A named callback requires its exact event name');
    end;
    LTrigger := ntNamed;
  end
  else if LMethod = 'on' then
  begin
    LArgs := Arguments;

    if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkTrigger) then
    begin
      Fail('Authored callbacks require a typed trigger');
    end;
    LTrigger := TNyxTrigger(LArgs[0].Ordinal);
  end
  else
  begin
    { Recognize fluent spellings from the same closed registry as generation.
      Open named streams still have their separate typed-reference admission. }
    LFound := False;
    for LMatch := Low(TNyxTrigger) to High(TNyxTrigger) do
    begin

      if NyxIsRuntimeTrigger(LMatch) and
        (LowerCase(NyxTriggerTitle(LMatch)) = LMethod) then
      begin
        LTrigger := LMatch;
        LFound := True;
        Break;
      end;
    end;

    if not LFound then
    begin
      Fail('Unsupported authored callback event');
    end;
  end;

  if not NyxIsRuntimeTrigger(LTrigger) and (LMethod <> 'onnamed') then
  begin
    Fail('Design selection is not an application callback');
  end;

  if FApply then
  begin

    if LTrigger = ntNamed then
    begin
      LStream := LEvents.OnNamed(LName);
    end
    else
    begin
      LStream := LEvents.On(LTrigger);
    end;
  end;
  while At('.') do
  begin
    Expect('.');
    LMethod := LowerCase(FTokens[FCursor].Text);
    Inc(FCursor);
    LArgs := Arguments;

    if LMethod = 'policy' then
    begin

      if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkPolicy) then
      begin
        Fail('Callback policy requires TNyxExecutionPolicy');
      end;

      if FApply then
      begin
        LStream.Policy(TNyxExecutionPolicy(LArgs[0].Ordinal));
      end;
    end
    else if LMethod = 'add' then
    begin

      if (Length(LArgs) <> 2) or (LArgs[0].Kind <> vkHandler) or
        (LArgs[1].Kind <> vkCallbackID) then
      begin
        Fail('Callback Add requires a handler and distinct registration reference');
      end;

      if FApply then
      begin
        LStream.Add(NyxHandler(LArgs[0].Text), NyxCallbackID(LArgs[1].Text));
      end;
    end
    else if LMethod = 'remove' then
    begin

      if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkCallbackID) then
      begin
        Fail('Callback Remove requires its registration reference');
      end;

      if FApply then
      begin
        LStream.Remove(NyxCallbackID(LArgs[0].Text));
      end;
    end
    else
    begin
      Fail('Unsupported authored callback method');
    end;
  end;
  Expect(';');
end;

function IsNyxControlFactory(const AName: TNyxText): Boolean;
var
  LKind: TNyxKind;
begin
  Result := SameText(AName, 'NewNyxControl') or SameText(AName, 'NewNyxBuiltinControl');

  if Result then
  begin
    Exit;
  end;
  for LKind := Low(TNyxKind) to High(TNyxKind) do
  begin

    if SameText(AName, GKindFactories[LKind]) then
    begin
      Exit(True);
    end;
  end;
end;

{$I nyx.source.builder.inc}

{ Borrowed lexical reader has already parsed this exact source's declarations
  and scalar references. This identity scan moves its cursor but never owns or
  releases it. Candidate reconstruction still uses a separate strict reader. }
function ControlLocals(const ASource: TNyxText; const ATokens: TTokens;
  AReader: TConfigurationReader): TControlLocals;
var
  LIndex: Integer;
  LCursor: Integer;
  LDepth: Integer;
  LCount: Integer;
  LReader: TConfigurationReader;
  LID: TValue;
  LFactory: Boolean;
  LLegacy: Boolean;
  LConstructor: Boolean;
  LKind: TNyxKind;
begin
  Result := nil;
  LIndex := 0;
  LReader := AReader;

  if (LReader = nil) or (LReader.FSource <> ASource) then
  begin
    raise EArgumentException.Create('Control identity scans require their exact source reader');
  end;
  while LIndex + 6 < Length(ATokens) do
  begin

    { Factory recognition scans the specialized catalog. Most tokens are not
      assignments, so reject that grammar shape before consulting it. The old
      order performed both catalog walks at every token during each lexical
      reconciliation pass, dominating ordinary visual edits. }

    if (ATokens[LIndex].Kind <> tkWord) or (ATokens[LIndex + 1].Text <> ':=') then
    begin
      Inc(LIndex);
      Continue;
    end;

    LLegacy := SameText(ATokens[LIndex + 2].Text, 'TNyxNode') and
      (ATokens[LIndex + 3].Text = '.') and
      SameText(ATokens[LIndex + 4].Text, 'Create') and
      (ATokens[LIndex + 5].Text = '(');
    LConstructor := LLegacy or
      ((ATokens[LIndex + 3].Text = '.') and
      SameText(ATokens[LIndex + 4].Text, 'Create') and
      (ATokens[LIndex + 5].Text = '(') and
      SpecializedKind(ATokens[LIndex + 2].Text, 'TNyx', LKind));
    LFactory := IsNyxControlFactory(ATokens[LIndex + 2].Text) and
      (ATokens[LIndex + 3].Text = '(');

    if (ATokens[LIndex].Kind = tkWord) and (ATokens[LIndex + 1].Text = ':=') and
      (LConstructor or LFactory) then
    begin
      LCursor := LIndex + 4;

      if LConstructor then
      begin
        LCursor := LIndex + 6;
      end;

      if LLegacy or SameText(ATokens[LIndex + 2].Text, 'NewNyxControl') or
        SameText(ATokens[LIndex + 2].Text, 'NewNyxBuiltinControl') then
      begin
        { Dynamic/legacy construction puts kind before identity. Specialized
          factories/classes put identity first. This lexical identity scan is
          only a reconciliation index; candidate admission evaluates the full
          typed constructor and ownership frame separately. }
        LDepth := 0;
        while LCursor < Length(ATokens) do
        begin

          if ATokens[LCursor].Text = '(' then
          begin
            Inc(LDepth);
          end;

          if ATokens[LCursor].Text = ')' then
          begin
            Dec(LDepth);
          end;

          if (LDepth = 0) and (ATokens[LCursor].Text = ',') then
          begin
            Break;
          end;
          Inc(LCursor);
        end;
        Inc(LCursor);
      end;

      LReader.FCursor := LCursor;
      LID := LReader.Expression;

      if LID.Kind <> vkText then
      begin
        LReader.Fail('A control identity requires text');
      end;
      LCount := Length(Result);
      SetLength(Result, LCount + 1);
      Result[LCount].Name := ATokens[LIndex].Text;
      Result[LCount].ID := LID.Text;
      Result[LCount].Configured := False;
    end;
    Inc(LIndex);
  end;
end;

function NyxCompanionUnitName(const ASource: TNyxText): TNyxText;
var
  LTokens: TTokens;
  LCursor: Integer;
  LIndex: Integer;
  LDirective: TNyxText;
  LCommand: TNyxText;
  LArgument: TNyxText;
  LBreak: Integer;
  LUses: Boolean;
begin
  LTokens := Lex(ASource);
  LCursor := 0;
  while (LCursor < Length(LTokens)) and (LTokens[LCursor].Kind = tkComment) do
  begin
    Inc(LCursor);
  end;

  if (LCursor >= Length(LTokens)) or (LTokens[LCursor].Kind <> tkWord) or
    not SameText(LTokens[LCursor].Text, 'unit') then
  begin
    raise ENyxSource.CreateAt('A companion must declare a Pascal unit', ASource, 1);
  end;
  Inc(LCursor);
  Result := '';
  repeat

    if (LCursor >= Length(LTokens)) or (LTokens[LCursor].Kind <> tkWord) then
    begin
      raise ENyxSource.CreateAt('Invalid companion unit namespace', ASource, 1);
    end;
    Result := Result + LTokens[LCursor].Text;
    Inc(LCursor);

    if (LCursor >= Length(LTokens)) or (LTokens[LCursor].Text <> '.') then
    begin
      Break;
    end;
    Result := Result + '.';
    Inc(LCursor);
  until False;

  if (LCursor >= Length(LTokens)) or (LTokens[LCursor].Text <> ';') then
  begin
    raise ENyxSource.CreateAt('Finish the companion unit declaration with ;', ASource, 1);
  end;
  TNyxCodegen.AdmitUnitName(Result);

  if SameText(Result, 'nyx_preview') or SameText(Result, 'nyx_native') then
  begin
    raise ENyxSource.CreateAt('Companion namespace conflicts with the application host', ASource, 1);
  end;
  LUses := False;
  for LIndex := 0 to Length(LTokens) - 1 do
  begin

    if LTokens[LIndex].Kind = tkWord then
    begin

      if SameText(LTokens[LIndex].Text, 'uses') then
      begin
        LUses := True;
      end
      else if LUses and SameText(LTokens[LIndex].Text, 'in') then
      begin
        raise ENyxSource.CreateAt('Companion imports use admitted unit search paths, not file clauses',
          ASource, LTokens[LIndex].Position);
      end;
    end;

    if (LTokens[LIndex].Kind = tkSymbol) and (LTokens[LIndex].Text = ';') then
    begin
      LUses := False;
    end;

    if LTokens[LIndex].Kind <> tkComment then
    begin
      Continue;
    end;
    LDirective := Trim(LTokens[LIndex].Text);

    if Copy(LDirective, 1, 2) = '{$' then
    begin
      LDirective := Copy(LDirective, 3, Length(LDirective) - 3);
    end
    else if Copy(LDirective, 1, 3) = '(*$' then
    begin
      LDirective := Copy(LDirective, 4, Length(LDirective) - 5);
    end
    else
    begin
      Continue;
    end;
    LDirective := LowerCase(Trim(LDirective));
    LBreak := 1;
    while (LBreak <= Length(LDirective)) and
      (LDirective[LBreak] in ['a'..'z']) do
    begin
      Inc(LBreak);
    end;
    LCommand := Copy(LDirective, 1, LBreak - 1);
    LArgument := Trim(Copy(LDirective, LBreak, MaxInt));
    { These compiler directives read/link files or alter search/output paths.
      The service owns those inputs through machine profiles and confined jobs.
      Ordinary conditional/warning directives remain compiler-controlled. }

    if (LCommand = 'i') or (LCommand = 'include') or (LCommand = 'l') or
      (LCommand = 'link') or (LCommand = 'linklib') or (LCommand = 'linkframework') or
      (LCommand = 'r') or (LCommand = 'resource') or (LCommand = 'unitpath') or
      (LCommand = 'includepath') or (LCommand = 'librarypath') or
      (LCommand = 'outputdir') or (LCommand = 'unitoutputdir') then
    begin
      { I+/I-/R+/R- select runtime checks rather than external files. }

      if not (((LCommand = 'i') or (LCommand = 'r')) and
        ((LArgument = '+') or (LArgument = '-'))) then
      begin
        raise ENyxSource.CreateAt('External-file compiler directives are not build companions',
          ASource, LTokens[LIndex].Position);
      end;
    end;

    if ((LCommand = 'codepage') and (LArgument <> 'utf8')) or
      ((LCommand = 'mode') and (LArgument <> 'delphi')) then
    begin
      raise ENyxSource.CreateAt('Nyx companions use Delphi mode and UTF-8 source',
        ASource, LTokens[LIndex].Position);
    end;
  end;
end;

function JoinSourceFrame(const AParts: array of TNyxText): TNyxText;
var
  LParts: TNyxStrings;
  LIndex: Integer;
begin
  { Native concatenation can inherit an ASCII prefix's runtime codepage tag.
    Copy exact source bytes through the portable join, including Unicode in
    handwritten suffixes. Browser joins retain their UTF-16 storage directly. }
  LParts := TNyxStrings.Create;
  try
    for LIndex := 0 to High(AParts) do
    begin
      LParts.Add(AParts[LIndex]);
    end;
    Result := LParts.Join;
  finally
    LParts.Free;
  end;
end;

function NyxSourceStateRename(const AOldName, ANewName: TNyxText): TNyxSourceStateRename;
begin
  Result.OldName := AOldName;
  Result.NewName := ANewName;
end;

function WithNyxImport(const AFrame, AUnit: TNyxText): TNyxText;
var
  LTokens: TTokens;
  LIndex: Integer;
  LUses: Boolean;
  LInterfaceEnd: Integer;
  LAtUnitName: Boolean;
  LNameCursor: Integer;
  LQualifiedName: TNyxText;
begin
  { Recovery can retain a pre-interface companion frame. Regenerated builders
    now need nyx.controls; add that import while retaining all existing imports,
    comments, helper declarations and the user's unit namespace. }
  Result := AFrame;
  LTokens := Lex(AFrame);
  LUses := False;
  LInterfaceEnd := 0;
  LAtUnitName := False;
  for LIndex := 0 to Length(LTokens) - 1 do
  begin

    if LTokens[LIndex].Kind = tkComment then
    begin
      Continue;
    end;

    if SameText(LTokens[LIndex].Text, 'interface') then
    begin
      LInterfaceEnd := LTokens[LIndex].Finish - 1;
    end;

    if SameText(LTokens[LIndex].Text, 'implementation') then
    begin
      Break;
    end;

    if (LInterfaceEnd > 0) and SameText(LTokens[LIndex].Text, 'uses') then
    begin
      LUses := True;
      LAtUnitName := True;
      Continue;
    end;

    if LUses and LAtUnitName and (LTokens[LIndex].Kind = tkWord) then
    begin
      { Lex splits every namespace segment into a word. Compare the complete
        qualified name, including deeper units such as collections.view.types.
        A two-segment comparison used to append those imports on every edit,
        yielding a companion that admitted but could not compile. Comments may
        separate segments; names remain case insensitive like the compiler. }
      LAtUnitName := False;
      LQualifiedName := LTokens[LIndex].Text;
      LNameCursor := LIndex + 1;
      while LNameCursor < Length(LTokens) do
      begin

        if LTokens[LNameCursor].Kind = tkComment then
        begin
          Inc(LNameCursor);
          Continue;
        end;

        if LTokens[LNameCursor].Text <> '.' then
        begin
          Break;
        end;
        Inc(LNameCursor);
        while (LNameCursor < Length(LTokens)) and
          (LTokens[LNameCursor].Kind = tkComment) do
        begin
          Inc(LNameCursor);
        end;

        if (LNameCursor >= Length(LTokens)) or
          (LTokens[LNameCursor].Kind <> tkWord) then
        begin
          Break;
        end;
        LQualifiedName := LQualifiedName + '.' + LTokens[LNameCursor].Text;
        Inc(LNameCursor);
      end;

      if SameText(LQualifiedName, AUnit) then
      begin
        Exit;
      end;
    end;

    if LUses and (LTokens[LIndex].Text = ',') then
    begin
      LAtUnitName := True;
    end;

    if LUses and (LTokens[LIndex].Text = ';') then
    begin
      Result := JoinSourceFrame([Copy(AFrame, 1, LTokens[LIndex].Position - 1),
        ',' + #10 + '  ' + AUnit, Copy(AFrame, LTokens[LIndex].Position, MaxInt)]);
      Exit;
    end;
  end;

  if LInterfaceEnd > 0 then
  begin
    Result := JoinSourceFrame([Copy(AFrame, 1, LInterfaceEnd), #10 + #10 +
      'uses ' + AUnit + ';', Copy(AFrame, LInterfaceEnd + 1, MaxInt)]);
  end;
end;

function NyxHandlerSourceLine(const ASource: TNyxText;
  const AHandler: TNyxHandlerRef): Integer;
var
  LTokens: TTokens;
  LName: TNyxText;
  LIndex: Integer;
  LPosition: Integer;
  LCharacter: Integer;
begin
  LName := NyxHandler(AHandler.Name).Name;
  LTokens := Lex(ASource);
  LPosition := 0;
  for LIndex := 0 to Length(LTokens) - 4 do
  begin

    if (LTokens[LIndex].Kind = tkWord) and
      SameText(LTokens[LIndex].Text, 'procedure') and
      (LTokens[LIndex + 1].Kind = tkWord) and
      SameText(LTokens[LIndex + 1].Text, LName) and
      (LTokens[LIndex + 2].Text = '.') and
      (LTokens[LIndex + 3].Kind = tkWord) and
      SameText(LTokens[LIndex + 3].Text, 'Invoke') then
    begin
      LPosition := LTokens[LIndex].Position;
      Break;
    end;
  end;

  if LPosition = 0 then
  begin
    raise ENyxSource.CreateAt('The callback implementation is missing from this source',
      ASource, 1);
  end;
  Result := 1;
  for LCharacter := 1 to LPosition - 1 do
  begin

    if ASource[LCharacter] = #10 then
    begin
      Inc(Result);
    end;
  end;
end;

{$I nyx.source.handlers.inc}
{$I nyx.source.imports.inc}
{$I nyx.source.routines.inc}
{$I nyx.source.declarations.inc}

function WithNyxControlImport(const AFrame: TNyxText): TNyxText;
begin
  Result := WithNyxImport(AFrame, 'nyx.controls');
  Result := WithNyxImport(Result, 'nyx.behavior');
  Result := WithNyxImport(Result, 'nyx.editing');
  Result := WithNyxImport(Result, 'nyx.gestures');
  Result := WithNyxImport(Result, 'nyx.scheduler');
  Result := WithNyxImport(Result, 'nyx.events');
  Result := WithNyxImport(Result, 'nyx.callbacks');
end;

function AddHandlerStub(const ASource: TNyxText; const AHandler: TNyxHandlerRef;
  const AEventTitle: TNyxText; out ALine: Integer): TNyxText;
var
  LPrefix: TNyxText;
  LBody: TNyxText;
  LSuffix: TNyxText;
  LTokens: TTokens;
  LIndex: Integer;
  LInsert: Integer;
  LEnd: Integer;
  LInitialization: Integer;
  LInitializationCount: Integer;
  LFinalization: Integer;
  LClass: TNyxText;
  LRegistration: TNyxText;
  LNavigation: Integer;
begin
  LClass := NyxHandler(AHandler.Name).Name;
  Split(ASource, LPrefix, LBody, LSuffix, LTokens);
  LTokens := Lex(LSuffix);
  LInitialization := 0;
  LInitializationCount := 0;
  LFinalization := 0;
  LEnd := 0;
  for LIndex := 0 to High(LTokens) do
  begin

    if LTokens[LIndex].Kind <> tkWord then
    begin
      Continue;
    end;

    if SameText(LTokens[LIndex].Text, LClass) then
    begin
      raise ENyxSource.CreateAt('This callback class already exists', ASource, 1);
    end;

    if SameText(LTokens[LIndex].Text, 'initialization') then
    begin
      Inc(LInitializationCount);
      LInitialization := LTokens[LIndex].Position;
    end;

    if SameText(LTokens[LIndex].Text, 'finalization') then
    begin
      LFinalization := LTokens[LIndex].Position;
    end;

    if SameText(LTokens[LIndex].Text, 'end') and (LIndex < High(LTokens)) and
      (LTokens[LIndex + 1].Text = '.') then
    begin
      LEnd := LTokens[LIndex].Position;
    end;
  end;

  if (LEnd = 0) or (LInitializationCount > 1) then
  begin
    raise ENyxSource.CreateAt('Add handlers through one shared unit initialization section',
      ASource, 1);
  end;
  LInsert := LEnd;

  if LFinalization > 0 then
  begin
    LInsert := LFinalization;
  end;

  if LInitialization > 0 then
  begin
    LInsert := LInitialization;
  end;
  LRegistration := '  RegisterNyxCallback(NyxHandler(''' + LClass + '''), ' + LClass + ');' + #10;

  if LInitialization = 0 then
  begin
    LRegistration := 'initialization' + #10 + LRegistration + #10;
  end;
  { Insert the registration first so declaration insertion uses original offsets. }
  LNavigation := LEnd;

  if LFinalization > 0 then
  begin
    LNavigation := LFinalization;
  end;
  LSuffix := JoinSourceFrame([Copy(LSuffix, 1, LNavigation - 1), LRegistration,
    Copy(LSuffix, LNavigation, MaxInt)]);
  LSuffix := JoinSourceFrame([Copy(LSuffix, 1, LInsert - 1),
    #10 + 'type' + #10 +
    '  { Application callback for ' + AEventTitle + '.' + #10 +
    '    AEvent owns its payload; use a parent-linked UI job for worker results. }' + #10 +
    '  ' + LClass + ' = class(TNyxEventCallback)' + #10 +
    '  public' + #10 +
    '    procedure Invoke(const AEvent: TNyxEventInfo;' + #10 +
    '      const AExecution: INyxExecution); override;' + #10 +
    '  end;' + #10 + #10 +
    'procedure ' + LClass + '.Invoke(const AEvent: TNyxEventInfo;' + #10 +
    '  const AExecution: INyxExecution);' + #10 +
    'begin' + #10 + #10 +
    '  if AExecution.Cancelled then' + #10 +
    '  begin' + #10 +
    '    Exit;' + #10 +
    '  end;' + #10 +
    '  // TODO: implement ' + LClass + '.' + #10 +
    'end;' + #10 + #10, Copy(LSuffix, LInsert, MaxInt)]);
  Result := JoinSourceFrame([WithNyxControlImport(LPrefix), LBody, LSuffix]);
  LNavigation := Pos('// TODO: implement ' + LClass + '.', Result);
  ALine := 1;
  for LIndex := 1 to LNavigation - 1 do
  begin

    if Result[LIndex] = #10 then
    begin
      Inc(ALine);
    end;
  end;
end;

function AddNyxHandlerStub(const ASource: TNyxText; const AHandler: TNyxHandlerRef;
  ATrigger: TNyxTrigger; out ALine: Integer): TNyxText;
begin
  Result := AddHandlerStub(ASource, AHandler, NyxTriggerName(ATrigger), ALine);
end;

function AddNyxHandlerStub(const ASource: TNyxText; const AHandler: TNyxHandlerRef;
  const AName: TNyxEventRef; out ALine: Integer): TNyxText;
begin
  { Open names are data, never Pascal comment syntax. JSON text escapes line
    breaks and replacing braces prevents a creator name from ending a comment. }
  Result := AddHandlerStub(ASource, AHandler,
    StringReplace(StringReplace(NyxData(AName.Name).ToJSON, '}', ')', [rfReplaceAll]),
      '{', '(', [rfReplaceAll]), ALine);
end;

{$I nyx.source.reconcile.inc}

function PrepareNyxCompanion(ADocument, ABuiltDocument: TNyxDocument;
  const ASource: TNyxText; AIsolated: Boolean): TNyxText;
var
  LWorkspace: TNyxSourceWorkspace;
  LCandidate: TNyxDocument;
  LUnitName: TNyxText;
  LPrefix: TNyxText;
  LBody: TNyxText;
  LSuffix: TNyxText;
  LGeneratedPrefix: TNyxText;
  LGeneratedSuffix: TNyxText;
  LGeneratedBody: TNyxText;
  LAuthored: Boolean;
  LTokens: TTokens;
begin
  LUnitName := NyxCompanionUnitName(ASource);
  LWorkspace := TNyxSourceWorkspace.Create;
  LCandidate := nil;
  try
    LCandidate := LWorkspace.Candidate(ADocument, ASource, True);

    if TNyxCodec.Encode(LCandidate) <> TNyxCodec.Encode(ADocument) then
    begin
      raise ENyxSource.CreateAt('Companion differs from the accepted design; Apply Pascal before building',
        ASource, 1);
    end;
    Result := ASource;

    if AIsolated then
    begin
      Split(ASource, LPrefix, LBody, LSuffix, LTokens);
      Split(TNyxCodegen.Generate(ABuiltDocument, LUnitName), LGeneratedPrefix,
        LGeneratedBody, LGeneratedSuffix, LTokens);
      LBody := ReconcileNyxBuilder(ADocument, LBody, LGeneratedBody, [], LAuthored);
      Result := JoinSourceFrame([WithNyxControlImport(LPrefix), LBody, LSuffix]);
      LCandidate.Free;
      LCandidate := nil;
      LWorkspace.Reset;
      LCandidate := LWorkspace.Candidate(ABuiltDocument, Result, True);

      if TNyxCodec.Encode(LCandidate) <> TNyxCodec.Encode(ABuiltDocument) then
      begin
        raise ENyxSource.CreateAt('Isolated companion differs from its reduced design', Result, 1);
      end;
    end;
  finally
    LCandidate.Free;
    LWorkspace.Free;
  end;
end;

{ Complete fresh admission shared by source Apply and visual verification.
  Split validates the entire exact source and yields one strict builder token
  sequence. Nothing is cached between admissions: every constructor, ownership
  statement, property, default and persistence budget is checked on a new tree.
  The returned canonical encoding is the encoding actually admitted here. }
function ReconstructNyxDraft(const ADraft: TNyxText;
  out APrefix, ABody, ASuffix, ADesign: TNyxText): TNyxDocument;
var
  LDraftTokens: TTokens;
  LDraftReader: TConfigurationReader;
  {$ifdef NYX_SOURCE_PROFILE}
  LStarted: Double;
  LCandidateStarted: Double;
  {$endif}
begin
  {$ifdef NYX_SOURCE_PROFILE}LCandidateStarted := SourceProfileStart;{$endif}
  Split(ADraft, APrefix, ABody, ASuffix, LDraftTokens);
  Result := TNyxDocument.Create;
  LDraftReader := nil;
  try
    try
      LDraftReader := TConfigurationReader.Create(ADraft, LDraftTokens, nil,
        Result, True, True);
      LDraftReader.Reconstruct;
      {$ifdef NYX_SOURCE_PROFILE}LStarted := SourceProfileStart;{$endif}
      ValidateNyxDocumentProperties(Result);
      {$ifdef NYX_SOURCE_PROFILE}SourceProfileFinish(spCandidateValidate, LStarted);{$endif}
      {$ifdef NYX_SOURCE_PROFILE}LStarted := SourceProfileStart;{$endif}
      ADesign := TNyxCodec.Encode(Result);
      {$ifdef NYX_SOURCE_PROFILE}SourceProfileFinish(spCandidateEncode, LStarted);{$endif}
    except
      Result.Free;
      Result := nil;
      raise;
    end;
  finally
    LDraftReader.Free;
  end;
  {$ifdef NYX_SOURCE_PROFILE}SourceProfileFinish(spCandidate, LCandidateStarted);{$endif}
end;

procedure TNyxSourceWorkspace.Reset;
begin
  FPrefix := '';
  FBody := '';
  FSuffix := '';
  FDesign := '';
  FCustomFrame := False;
  FOrigin := nsoDeclarative;
  FGeneratedBody := '';
end;

procedure SplitNyxSourceFrame(const ASource: TNyxText;
  out APrefix, ABuilder, ASuffix: TNyxText);
var
  LPrefix: TNyxText;
  LBuilder: TNyxText;
  LSuffix: TNyxText;
  LTokens: TTokens;
begin
  Split(ASource, LPrefix, LBuilder, LSuffix, LTokens);
  APrefix := LPrefix;
  ABuilder := LBuilder;
  ASuffix := LSuffix;
end;

function TNyxSourceWorkspace.Render(ADocument: TNyxDocument): TNyxText;
begin
  Result := Render(ADocument, []);
end;

function TNyxSourceWorkspace.Render(ADocument: TNyxDocument;
  const ARenames: array of TNyxSourceStateRename): TNyxText;
var
  LDesign: TNyxText;
  LGenerated: TNyxText;
  LGeneratedBody: TNyxText;
  LBefore: TNyxText;
  LPrefix: TNyxText;
  LBody: TNyxText;
  LSuffix: TNyxText;
  LTokens: TTokens;
  LPrevious: TNyxDocument;
  LCandidate: TNyxDocument;
  LVerifiedDesign: TNyxText;
  LVerifiedPrefix: TNyxText;
  LVerifiedBody: TNyxText;
  LVerifiedSuffix: TNyxText;
  LAuthored: Boolean;
  LGeneratedImports: TNyxImportClause;
  LImportIndex: Integer;
  LNeedsTypeAhead: Boolean;
  {$ifdef NYX_SOURCE_PROFILE}
  LStarted: Double;
  LRenderStarted: Double;
  {$endif}
begin
  {$ifdef NYX_SOURCE_PROFILE}
  LRenderStarted := SourceProfileStart;
  LStarted := SourceProfileStart;
  {$endif}
  LDesign := TNyxCodec.Encode(ADocument);
  {$ifdef NYX_SOURCE_PROFILE}SourceProfileFinish(spRenderEncode, LStarted);{$endif}

  if FOrigin = nsoExecuted then
  begin

    if LDesign <> FDesign then
    begin
      raise ENyxSourceExecutionRequired.CreateAt(
        'Visual changes to this Pascal unit require compiler-aware reconciliation',
        FPrefix, 1);
    end;
    Exit(FPrefix);
  end;

  if FDesign <> LDesign then
  begin
    LGenerated := TNyxCodegen.Generate(ADocument);
    Split(LGenerated, LPrefix, LBody, LSuffix, LTokens);
    LGeneratedBody := LBody;
    { Read the generator's short interface frame, not its potentially large
      builder. Its canonical import decision includes saved policies in pages,
      reusable parts and menu contracts. Preserve handwritten frames below,
      while reconciling this required dependency through the ordinary boundary. }
    LGeneratedImports := ReadNyxImports(LPrefix, nisInterface);
    LNeedsTypeAhead := False;
    for LImportIndex := 0 to LGeneratedImports.Count - 1 do
    begin
      LNeedsTypeAhead := LNeedsTypeAhead or
        (LGeneratedImports.UnitAt(LImportIndex).Name = 'nyx.typeahead');
    end;

    if FCustomFrame then
    begin
      LPrefix := FPrefix;
      LSuffix := FSuffix;
    end;
    LPrefix := WithNyxControlImport(LPrefix);
    LPrefix := WithNyxImport(LPrefix, 'nyx.presentations');
    { Older accepted frames predate typed calendar constructors. The generator's
      stable public imports include their namespace; reconcile that dependency
      before publishing a new managed body, while preserving authored helpers,
      comments and import order through the existing admitted import boundary. }
    LPrefix := WithNyxImport(LPrefix, 'nyx.dates');
    LPrefix := WithNyxImport(LPrefix, 'nyx.times');
    LPrefix := WithNyxImport(LPrefix, 'nyx.colors');
    LPrefix := WithNyxImport(LPrefix, 'nyx.images');

    if ADocument.Resources.Count > 0 then
    begin
      LPrefix := WithNyxImport(LPrefix, 'nyx.resources');
      LPrefix := WithNyxImport(LPrefix, 'nyx.resource.sources');
      LPrefix := WithNyxImport(LPrefix, 'nyx.bytes');
    end;

    if NyxHasResourceCollections(ADocument.Collections) then
    begin
      LPrefix := WithNyxImport(LPrefix, 'nyx.resources.rows');
    end;
    LPrefix := WithNyxImport(LPrefix, 'nyx.design.tokens');

    if LNeedsTypeAhead then
    begin
      { An existing accepted unit may predate its first saved search choice.
        Source admission alone cannot prove compiler name resolution. Retain
        existing imports after reset: application helpers may still use them. }
      LPrefix := WithNyxImport(LPrefix, 'nyx.typeahead');
    end;

    if ADocument.Collections.Count > 0 then
    begin
      LPrefix := WithNyxImport(LPrefix, 'nyx.collections');
    end;

    if ADocument.HasCollectionViews then
    begin
      LPrefix := WithNyxImport(LPrefix, 'nyx.collections.view.types');
      LPrefix := WithNyxImport(LPrefix, 'nyx.collections.selection');
      LPrefix := WithNyxImport(LPrefix, 'nyx.collections.query');
    end;

    if FDesign <> '' then
    begin
      LBefore := FGeneratedBody;
      LPrevious := nil;
      LCandidate := nil;
      try

        if LBefore = '' then
        begin
          { Restored history/recovery carries only the accepted pair. Rebuild
            this derived value once, using its exact accepted canonical design;
            never borrow the possibly changed current public document. }
          {$ifdef NYX_SOURCE_PROFILE}LStarted := SourceProfileStart;{$endif}
          LPrevious := TNyxCodec.Decode(FDesign);
          Split(TNyxCodegen.Generate(LPrevious), LVerifiedPrefix, LBefore,
            LVerifiedSuffix, LTokens);
          {$ifdef NYX_SOURCE_PROFILE}SourceProfileFinish(spOldGeneration, LStarted);{$endif}
        end;
        LBody := ReconcileNyxBuilderFrames(LBefore, FBody, LBody, ARenames, LAuthored);

        if LAuthored then
        begin
          {$ifdef NYX_SOURCE_PROFILE}LStarted := SourceProfileStart;{$endif}
          { Verification needs a fresh reconstructed candidate, not another
            generated workspace baseline. Reuse this admission's encoding for
            exact comparison rather than serializing the same candidate twice. }
          LCandidate := ReconstructNyxDraft(JoinSourceFrame([LPrefix, LBody, LSuffix]),
            LVerifiedPrefix, LVerifiedBody, LVerifiedSuffix, LVerifiedDesign);

          if LVerifiedDesign <> LDesign then
          begin
            raise ENyxSource.CreateAt('Reconciled Pascal differs from the visual design; the accepted pair is retained',
              JoinSourceFrame([LPrefix, LBody, LSuffix]), 1);
          end;
          {$ifdef NYX_SOURCE_PROFILE}SourceProfileFinish(spVerify, LStarted);{$endif}
        end;
      finally
        LCandidate.Free;
        LPrevious.Free;
      end;
    end;
    { Publish all four members only after the proposed companion reconstructs
      the candidate. A failed reconciliation never leaves half a source frame. }
    FPrefix := LPrefix;
    FBody := LBody;
    FSuffix := LSuffix;
    FDesign := LDesign;
    FGeneratedBody := LGeneratedBody;
  end;
  Result := JoinSourceFrame([FPrefix, FBody, FSuffix]);
  {$ifdef NYX_SOURCE_PROFILE}SourceProfileFinish(spRender, LRenderStarted);{$endif}
end;

function TNyxSourceWorkspace.Candidate(ADocument: TNyxDocument;
  const ADraft: TNyxText; ACompanionComparison: Boolean): TNyxDocument;
var
  LPrefix: TNyxText;
  LBody: TNyxText;
  LSuffix: TNyxText;
  LDesign: TNyxText;
begin
  { Retain the established accepted workspace baseline, but never borrow its
    tree as the candidate. A source omission must remove its design meaning. }
  Render(ADocument);
  Result := ReconstructNyxDraft(ADraft, LPrefix, LBody, LSuffix, LDesign);
end;

function TNyxSourceWorkspace.PrepareCandidate(ADocument: TNyxDocument;
  const ADraft: TNyxText; out AWorkspace: TNyxSourceWorkspace): TNyxDocument;
begin
  AWorkspace := nil;
  Render(ADocument);
  Result := PrepareDraft(ADraft, AWorkspace);
end;

class function TNyxSourceWorkspace.PrepareDraft(const ADraft: TNyxText;
  out AWorkspace: TNyxSourceWorkspace): TNyxDocument;
var
  LPrefix: TNyxText;
  LBody: TNyxText;
  LSuffix: TNyxText;
  LDesign: TNyxText;
begin
  AWorkspace := nil;
  Result := ReconstructNyxDraft(ADraft, LPrefix, LBody, LSuffix, LDesign);
  try
    AWorkspace := TNyxSourceWorkspace.Create;
    AWorkspace.StageAccepted(Result, LPrefix, LBody, LSuffix, LDesign);
  except
    FreeAndNil(AWorkspace);
    Result.Free;
    Result := nil;
    raise;
  end;
end;

procedure TNyxSourceWorkspace.Accept(ADocument: TNyxDocument;
  const ASource: TNyxText);
var
  LPrefix: TNyxText;
  LBody: TNyxText;
  LSuffix: TNyxText;
  LDesign: TNyxText;
  LTokens: TTokens;
begin
  { Ordinary Accept retains its external caller's explicit admission contract.
    Stage the complete source and fresh canonical document before publication. }
  Split(ASource, LPrefix, LBody, LSuffix, LTokens);
  LDesign := TNyxCodec.Encode(ADocument);
  StageAccepted(ADocument, LPrefix, LBody, LSuffix, LDesign);
end;

procedure TNyxSourceWorkspace.StageAccepted(ADocument: TNyxDocument;
  const APrefix, ABody, ASuffix, ADesign: TNyxText);
var
  LGeneratedPrefix: TNyxText;
  LGeneratedBody: TNyxText;
  LGeneratedSuffix: TNyxText;
  LTokens: TTokens;
begin
  { Stage complete frames before publication. The session calls this only after
    candidate admission; an invalid source cannot partially replace a frame. }
  Split(TNyxCodegen.Generate(ADocument), LGeneratedPrefix, LGeneratedBody,
    LGeneratedSuffix, LTokens);
  FPrefix := APrefix;
  FBody := ABody;
  FSuffix := ASuffix;
  FDesign := ADesign;
  FCustomFrame := (APrefix <> LGeneratedPrefix) or (ASuffix <> LGeneratedSuffix);
  FOrigin := nsoDeclarative;
  FGeneratedBody := LGeneratedBody;
end;

procedure TNyxSourceWorkspace.AcceptExecuted(ADocument: TNyxDocument;
  const ASource: TNyxText);
var
  LDesign: TNyxText;
begin
  { Admission/encoding can fail. Publish none of the workspace members until
    the complete source identity and detached design have both been validated. }
  NyxCompanionUnitName(ASource);
  ValidateNyxDocumentProperties(ADocument);
  LDesign := TNyxCodec.Encode(ADocument);
  FPrefix := ASource;
  FBody := '';
  FSuffix := '';
  FDesign := LDesign;
  FCustomFrame := True;
  FOrigin := nsoExecuted;
  FGeneratedBody := '';
end;

function TNyxSourceCheckpoint.GetStorageBytes: TNyxTextBytes;
begin
  Result := TNyxTextBytes(Length(FPrefix)) + Length(FBody) + Length(FSuffix) + Length(FDesign);
  {$ifdef PAS2JS}
  Result := Result * 2;
  {$endif}
end;

function TNyxSourceCheckpoint.GetSource: TNyxText;
begin
  Result := JoinSourceFrame([FPrefix, FBody, FSuffix]);
end;

function TNyxSourceWorkspace.Capture: TNyxSourceCheckpoint;
begin
  Result.FPrefix := FPrefix;
  Result.FBody := FBody;
  Result.FSuffix := FSuffix;
  Result.FDesign := FDesign;
  Result.FCustomFrame := FCustomFrame;
  Result.FOrigin := FOrigin;
end;

function TNyxSourceWorkspace.Capture(ADocument: TNyxDocument): TNyxSourceCheckpoint;
begin
  Render(ADocument);
  Result := Capture;
end;

procedure TNyxSourceWorkspace.Restore(const ACheckpoint: TNyxSourceCheckpoint);
{$ifdef NYX_SOURCE_PROFILE}
var
  LStarted: Double;
{$endif}
begin
  {$ifdef NYX_SOURCE_PROFILE}LStarted := SourceProfileStart;{$endif}
  FPrefix := ACheckpoint.FPrefix;
  FBody := ACheckpoint.FBody;
  FSuffix := ACheckpoint.FSuffix;
  FDesign := ACheckpoint.FDesign;
  FCustomFrame := ACheckpoint.FCustomFrame;
  FOrigin := ACheckpoint.FOrigin;
  FGeneratedBody := '';
  {$ifdef NYX_SOURCE_PROFILE}SourceProfileFinish(spRestoreWorkspace, LStarted);{$endif}
end;

function TNyxSourceWorkspace.Snapshot: TNyxText;
{$ifdef NYX_SOURCE_PROFILE}
var
  LStarted: Double;
{$endif}
begin
  {$ifdef NYX_SOURCE_PROFILE}LStarted := SourceProfileStart;{$endif}
  { Keep the legacy literal worker's exact five-field shape. Only the explicit
    executed snapshot carries this additional marker; its receiver still refuses
    that shape. JSON restore is checkpoint restoration, never source execution. }

  if FOrigin = nsoExecuted then
  begin
    Result := NyxObject([
      NyxField('prefix', NyxData(FPrefix)), NyxField('body', NyxData(FBody)),
      NyxField('suffix', NyxData(FSuffix)), NyxField('design', NyxData(FDesign)),
      NyxField('custom', NyxData(FCustomFrame)),
      NyxField('executed', NyxData(True))]).ToJSON;
  end
  else
  begin
    Result := NyxObject([
      NyxField('prefix', NyxData(FPrefix)),
      NyxField('body', NyxData(FBody)),
      NyxField('suffix', NyxData(FSuffix)),
      NyxField('design', NyxData(FDesign)),
      NyxField('custom', NyxData(FCustomFrame))]).ToJSON;
  end;
  {$ifdef NYX_SOURCE_PROFILE}SourceProfileFinish(spSnapshot, LStarted);{$endif}
end;

procedure TNyxSourceWorkspace.Restore(const ASnapshot: TNyxText);
var
  LData: TNyxDataValue;
  LPrefix: TNyxText;
  LBody: TNyxText;
  LSuffix: TNyxText;
  LDesign: TNyxText;
  LCustom: Boolean;
  LOrigin: TNyxSourceOrigin;
  {$ifdef NYX_SOURCE_PROFILE}
  LStarted: Double;
  {$endif}
begin
  {$ifdef NYX_SOURCE_PROFILE}LStarted := SourceProfileStart;{$endif}
  LData := TNyxDataValue.ParseJSON(ASnapshot);
  LPrefix := LData.Field('prefix').AsText;
  LBody := LData.Field('body').AsText;
  LSuffix := LData.Field('suffix').AsText;
  LDesign := LData.Field('design').AsText;
  LCustom := LData.Field('custom').AsBoolean;
  LOrigin := nsoDeclarative;

  if LData.Kind <> ndObject then
  begin
    raise ENyxSource.CreateAt('Source checkpoint requires an object', ASnapshot, 1);
  end;

  if LData.Count = 6 then
  begin

    if (LData.Field('executed').Kind <> ndBoolean) or
      not LData.Field('executed').AsBoolean or not LCustom or
      (LBody <> '') or (LSuffix <> '') or (LDesign = '') then
    begin
      raise ENyxSource.CreateAt('Executed source checkpoint has invalid metadata', ASnapshot, 1);
    end;
    NyxCompanionUnitName(LPrefix);
    LOrigin := nsoExecuted;
  end
  else if LData.Count <> 5 then
  begin
    raise ENyxSource.CreateAt('Source checkpoint has unknown fields', ASnapshot, 1);
  end;
  FPrefix := LPrefix;
  FBody := LBody;
  FSuffix := LSuffix;
  FDesign := LDesign;
  FCustomFrame := LCustom;
  FOrigin := LOrigin;
  FGeneratedBody := '';
  {$ifdef NYX_SOURCE_PROFILE}SourceProfileFinish(spRestoreWorkspace, LStarted);{$endif}
end;

procedure InitializeSourceSymbols;
var
  LKind: TNyxKind;
begin
  GFactoryKinds := TSourceIndex.Create;
  GClassKinds := TSourceIndex.Create;
  GInterfaceKinds := TSourceIndex.Create;
  for LKind := Low(TNyxKind) to High(TNyxKind) do
  begin
    GKindStems[LKind] := SymbolStem(NyxKindName(LKind));
    GKindFactories[LKind] := 'NewNyx' + GKindStems[LKind];
    GKindClasses[LKind] := 'TNyx' + GKindStems[LKind];
    GKindInterfaces[LKind] := 'INyx' + GKindStems[LKind];
    GKindEnums[LKind] := 'nk' + GKindStems[LKind];
    GFactoryKinds.AddFirst(LowerCase(GKindFactories[LKind]), Ord(LKind));
    GClassKinds.AddFirst(LowerCase(GKindClasses[LKind]), Ord(LKind));
    GInterfaceKinds.AddFirst(LowerCase(GKindInterfaces[LKind]), Ord(LKind));
  end;
end;

initialization
  InitializeSourceSymbols;
  InitializeEnumSymbols;

{$ifndef PAS2JS}
{ Native unit finalization releases this process-lifetime immutable vocabulary.
  Browser unit finalization is unsupported; its four closed tables live with
  their owning module and are collected with that module's execution context. }
finalization
  GEnumNames.Free;
  GInterfaceKinds.Free;
  GClassKinds.Free;
  GFactoryKinds.Free;
{$endif}

end.
