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
  nyx.types,
  nyx.layout.policy,
  nyx.callbacks,
  nyx.model;

const
  NyxViewsBegin = '// <nyx:views>';
  NyxViewsEnd = '// </nyx:views>';

type
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
    function GetStorageBytes: TNyxTextBytes;
  public
    property Design: TNyxText read FDesign;
    property StorageBytes: TNyxTextBytes read GetStorageBytes;
  end;

  { An owned companion to the design, independent of a target compiler.
    The delimited BuildNyxDocument builder is synchronized; Pascal helpers and
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
    accepted source. Snapshot/Restore carry this companion through history and
    local recovery; portable .nyx and .pas exports remain separate files. }
  TNyxSourceWorkspace = class
  private
    FPrefix: TNyxText;
    FBody: TNyxText;
    FSuffix: TNyxText;
    FDesign: TNyxText;
    FCustomFrame: Boolean;
    { Derived canonical builder for FDesign only. It owns text, never a model or
      reader. Recovery/history deliberately omit this disposable workspace cache;
      Restore clears it, and every Render still freshly encodes the public tree. }
    FGeneratedBody: TNyxText;
    procedure StageAccepted(ADocument: TNyxDocument; const APrefix, ABody,
      ASuffix, ADesign: TNyxText);
  public
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
  nyx.collections.view.types,
  nyx.collections.selection,
  nyx.binding.types,
  nyx.contract,
  nyx.scheduler,
  nyx.json,
  nyx.schema,
  nyx.controls;

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
    vkCollectionView, vkCollectionScope, vkCollectionCellMode, vkSelectionMode, vkPlatform,
    vkSplitOrientation, vkSemanticEvent, vkTouchBehavior, vkFlowWrap,
    vkCrossAlignment, vkJustification, vkSizing, vkLayoutPolicy);
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
    LayoutPolicy: TNyxLayoutPolicy;
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
    FDocument: TNyxDocument;
    FApply: Boolean;
    FTitleSeen: Boolean;
    FOwnedTry: Boolean;
    procedure Fail(const AMessage: TNyxText);
    function At(const AText: TNyxText): Boolean;
    procedure Expect(const AText: TNyxText);
    function Expression: TValue;
    function Primary: TValue;
    function Arguments: TValues;
    function ArrayArguments(AMaximum: Integer): TValues;
    function Numeric(const AValue: TValue): Double;
    function Domain(AKind: TNyxStateKind): TValue;
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
    function CollectionScalar(const AValue: TValue; AKind: TNyxStateKind): TNyxStateValue;
    procedure CollectionDefaults;
    procedure Bindings(AIndex: Integer);
    procedure Contract(AIndex: Integer);
    procedure Extensions(AIndex: Integer);
    procedure Callbacks;
    procedure Configure(AIndex: Integer);
    procedure ApplyCall(ANode: TNyxNode; const AMethod: TNyxText;
      const AArgs: TValues; APlatform: TNyxPlatform);
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
    'Columns', 'Flex', 'Minimum', 'Maximum');
  CMethods: array[TNyxAttribute] of TNyxText = (
    'Text', 'Value', 'Placeholder', 'Items', 'Hint', 'AccessibleName',
    'LinkTo', 'Source', 'AlternativeText', 'Layout', 'Padding', 'Gap',
    'Columns', 'Width', 'Height', 'Left', 'Top', 'Flex', 'Minimum', 'Maximum',
    'Enabled', 'Visible', 'ReadOnly', 'Surface', 'Compound', 'Pressed',
    'Variant', 'Action', 'ProjectAs', 'OverrideMode', 'InputType', 'PartName',
    'Target', 'Component', 'OnClick', 'OnChange', 'Option', 'OverridePath', '',
    'SplitOrientation', 'SplitPosition', 'SplitMinimum', 'SplitMaximum', 'SplitResizable',
    'DragSource', 'DropTarget', 'TouchBehavior', 'Wrap', 'Align', 'Justify',
    'WidthSizing', 'HeightSizing');
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
    'atCrossAlignment', 'atJustification', 'atWidthSizing', 'atHeightSizing');

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

  RegisterEnum(vkCollectionScope, Ord(csApplication), 'csApplication');
  RegisterEnum(vkCollectionScope, Ord(csInstance), 'csInstance');
  RegisterEnum(vkSelectionMode, Ord(nsmSingle), 'nsmSingle');
  RegisterEnum(vkSelectionMode, Ord(nsmMultiple), 'nsmMultiple');
  RegisterEnum(vkCollectionCellMode, Ord(cmReadOnly), 'cmReadOnly');
  RegisterEnum(vkCollectionCellMode, Ord(cmEditable), 'cmEditable');

  RegisterEnum(vkConstruction, Ord(ncoDefault), 'ncoDefault');
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
  RegisterEnum(vkSplitOrientation, Ord(nsoStacked), 'nsoStacked');
  RegisterEnum(vkSplitOrientation, Ord(nsoSideBySide), 'nsoSideBySide');
  RegisterEnum(vkTouchBehavior, Ord(ntbAutomatic), 'ntbAutomatic');
  RegisterEnum(vkTouchBehavior, Ord(ntbNone), 'ntbNone');
  RegisterEnum(vkTouchBehavior, Ord(ntbPanX), 'ntbPanX');
  RegisterEnum(vkTouchBehavior, Ord(ntbPanY), 'ntbPanY');
  RegisterEnum(vkTouchBehavior, Ord(ntbManipulation), 'ntbManipulation');
  RegisterEnum(vkFlowWrap, Ord(nfwAutomatic), 'nfwAutomatic');
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

function TConfigurationReader.Arguments: TValues;
var
  LCount: Integer;
begin
  Result := nil;
  Expect('(');

  if not At(')') then
  begin
    repeat
      LCount := Length(Result);

      if LCount >= 3 then
      begin
        Fail('This configuration accepts at most three arguments');
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

function TConfigurationReader.Domain(AKind: TNyxStateKind): TValue;
var
  LMethod: TNyxText;
  LArgs: TValues;
  LIndex: Integer;
  LTexts: array of TNyxText;
  LBooleans: array of Boolean;
  LIntegers: array of Integer;
  LNumbers: array of Double;
begin
  Result.Kind := vkDomain;
  Result.Ordinal := Ord(AKind);
  case AKind of
    nskText: Result.TextDomain := NyxTextDomain;
    nskBoolean: Result.BooleanDomain := NyxBooleanDomain;
    nskInteger: Result.IntegerDomain := NyxIntegerDomain;
    nskNumber: Result.NumberDomain := NyxNumberDomain;
  end;

  if At('(') then
  begin
    Expect('(');
    Expect(')');
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
        Fail('Only Integer and Number domains have Range');
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
    else
    begin
      Fail('Unsupported domain method ' + LMethod);
    end;
  end;
end;

{$I nyx.source.collections.inc}

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

        if LName = 'nyxpart' then
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

      if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkDomain) then
      begin
        Fail('Contract Value requires a typed scalar domain');
      end;
      LDomain := LArgs[0];
    end
    else if SameText(LMethod, 'Field') then
    begin

      if (Length(LArgs) <> 2) or (LArgs[0].Kind <> vkPart) or
        (LArgs[1].Kind <> vkDomain) then
      begin
        Fail('Field requires a typed part reference and scalar domain');
      end;
      LPart := NyxPart(LArgs[0].Text);
      LDomain := LArgs[1];
    end
    else if SameText(LMethod, 'On') then
    begin

      if (Length(LArgs) <> 3) or (LArgs[0].Kind <> vkTrigger) or
        (LArgs[1].Kind <> vkEventSource) or (LArgs[2].Kind <> vkDomain) then
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
  const AMethod: TNyxText; const AArgs: TValues; APlatform: TNyxPlatform);
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
  LConfigure := ANode.Configure.ForPlatform(APlatform);
  LMethod := LowerCase(AMethod);

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

        if LAttribute <> atValue then
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
    atCrossAlignment: Require(vkCrossAlignment);
    atJustification: Require(vkJustification);
    atWidthSizing, atHeightSizing: Require(vkSizing);
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
    atSource: LConfigure.Source(LValue.Text);
    atAlt: LConfigure.AlternativeText(LValue.Text);
    atValue, atOption:
      begin
        case LValue.Kind of
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
          Fail('Value/Option requires text, Boolean, Integer or Double');
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
    atLeft: LConfigure.Left(LInteger);
    atTop: LConfigure.Top(LInteger);
    atFlex: LConfigure.Flex(LInteger);
    atMinimum: LConfigure.Minimum(LInteger);
    atMaximum: LConfigure.Maximum(LInteger);
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
begin
  LPlatform := npfAny;

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
    LArgs := Arguments;

    if SameText(LMethod, 'ForPlatform') then
    begin

      if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkPlatform) then
      begin
        Fail('ForPlatform requires a TNyxPlatform enum');
      end;
      LPlatform := TNyxPlatform(LArgs[0].Ordinal);
    end
    else if FApply then
    begin
      ApplyCall(LNode, LMethod, LArgs, LPlatform);
    end;
  end;
  Fail('Finish the Configure block with .Done;');
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
  LNameStage: Integer;
begin
  { Recovery can retain a pre-interface companion frame. Regenerated builders
    now need nyx.controls; add that import while retaining all existing imports,
    comments, helper declarations and the user's unit namespace. }
  Result := AFrame;
  LTokens := Lex(AFrame);
  LUses := False;
  LInterfaceEnd := 0;
  LNameStage := 0;
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
    end;

    if LUses and (LNameStage = 2) and
      SameText(LTokens[LIndex].Text, Copy(AUnit, 5, MaxInt)) then
    begin
      Exit;
    end;

    if LUses and (LNameStage = 1) and (LTokens[LIndex].Text = '.') then
    begin
      LNameStage := 2;
    end
    else if LUses and SameText(LTokens[LIndex].Text, 'nyx') then
    begin
      LNameStage := 1;
    end
    else
    begin
      LNameStage := 0;
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

  if FDesign <> LDesign then
  begin
    LGenerated := TNyxCodegen.Generate(ADocument);
    Split(LGenerated, LPrefix, LBody, LSuffix, LTokens);
    LGeneratedBody := LBody;

    if FCustomFrame then
    begin
      LPrefix := FPrefix;
      LSuffix := FSuffix;
    end;
    LPrefix := WithNyxControlImport(LPrefix);

    if ADocument.Collections.Count > 0 then
    begin
      LPrefix := WithNyxImport(LPrefix, 'nyx.collections');
    end;

    if ADocument.HasCollectionViews then
    begin
      LPrefix := WithNyxImport(LPrefix, 'nyx.collections.view.types');
      LPrefix := WithNyxImport(LPrefix, 'nyx.collections.selection');
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
var
  LPrefix: TNyxText;
  LBody: TNyxText;
  LSuffix: TNyxText;
  LDesign: TNyxText;
begin
  AWorkspace := nil;
  Render(ADocument);
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
  FGeneratedBody := LGeneratedBody;
end;

function TNyxSourceCheckpoint.GetStorageBytes: TNyxTextBytes;
begin
  Result := TNyxTextBytes(Length(FPrefix)) + Length(FBody) + Length(FSuffix) + Length(FDesign);
  {$ifdef PAS2JS}
  Result := Result * 2;
  {$endif}
end;

function TNyxSourceWorkspace.Capture: TNyxSourceCheckpoint;
begin
  Result.FPrefix := FPrefix;
  Result.FBody := FBody;
  Result.FSuffix := FSuffix;
  Result.FDesign := FDesign;
  Result.FCustomFrame := FCustomFrame;
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
  Result := NyxObject([
    NyxField('prefix', NyxData(FPrefix)),
    NyxField('body', NyxData(FBody)),
    NyxField('suffix', NyxData(FSuffix)),
    NyxField('design', NyxData(FDesign)),
    NyxField('custom', NyxData(FCustomFrame))
  ]).ToJSON;
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
  FPrefix := LPrefix;
  FBody := LBody;
  FSuffix := LSuffix;
  FDesign := LDesign;
  FCustomFrame := LCustom;
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
