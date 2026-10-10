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

unit nyx.studio.agents;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.types, nyx.model, nyx.schema, nyx.source,
  nyx.studio.session, nyx.studio.projects, nyx.studio.compiler, nyx.studio.builds,
  nyx.studio.sourceprojection,
  nyx.studio.rootedits, nyx.presentations, nyx.menu.declarations, nyx.root.types,
  nyx.application.resources, nyx.resources, nyx.resources.runtime.view,
  nyx.studio.projectimport;

type
  { Operator permissions are closed, session-local and never part of a design.
    Only the editor exchange can change them. MCP cannot grant itself access. }
  TNyxAgentPermission = (apDisabled, apReadOnly, apEdit);

  { Open application-run names use distinct references. Scope separates a full
    application, one mounted view and a resource-only preview. A resource preview
    cannot establish successful control publication in a full application. }
  TNyxStudioRuntimeScope = (srsApplication, srsView, srsResources);
  TNyxStudioRuntimeRef = record
  private
    FName: TNyxText;
  public
    property Name: TNyxText read FName;
  end;
  { Trusted host capability, deliberately without a wire codec or public token
    property. An agent query never grants publication/reload/cancellation. Exact
    design revision, runtime identity and catalog bind this transient authority;
    paired edits, recovery and explicit retirement revoke it. }
  TNyxStudioResourceObservation = record
  private
    FIdentity: TNyxText;
  end;
  TNyxStudioResourceRun = record
    Reference: TNyxStudioRuntimeRef;
    Scope: TNyxStudioRuntimeScope;
    View: TNyxText;
    Target: TNyxPlatform;
    Authority: TNyxText;
    Sequence: Integer;
    Active: Boolean;
  end;

  { Private host recovery contains authoring state and operator permission only.
    Transport credentials, receipts, removal-review tickets, activity and compiler
    callbacks deliberately expire with their owning connection/process. }
  TNyxAgentRecoveryFrame = record
    Session: TNyxStudioRecoveryFrame;
    Revision: Integer;
    Permission: TNyxAgentPermission;
    Claimed: Boolean;
  end;

  { One active, authoritative authoring session. Owns the ordinary Studio model
    and source/history; no shadow design format or browser automation exists.
    Native transports serialize all calls under an external lock. Portable tests
    and native controllers use the same command/query boundary directly. }
  TNyxAgentSession = class
  private
    FSession: TNyxStudioSession;
    FRevision: Integer;
    FPermission: TNyxAgentPermission;
    FClaimed: Boolean;
    FActivity: array of TNyxDataValue;
    FActivitySerial: Integer;
    FReport: INyxCompilerReport;
    FCompilerSequence: Integer;
    FResourceRuns: array of TNyxStudioResourceRun;
    { Separate managed storage: pas2js does not support COM interfaces inside
      records. Metadata arrays are detached on rollback; snapshots are immutable. }
    FResourceSnapshots: array of INyxResourceRuntimeSnapshot;
    FReceiptKeys: array of TNyxText;
    FReceiptRequests: array of TNyxText;
    FReceiptResults: array of TNyxDataValue;
    { Review tickets are bounded session state, never document content. Exact
      actor/revision/change bytes bind consent; accepted retries use receipts. }
    FCallbackReviews: array of TNyxDataValue;
    FCallbackReviewSerial: Integer;
    { Eight exact paired-text snapshots bound root-review memory. Metadata and
      immutable commands are retained together; no node/session is borrowed. }
    FRootReviews: array of TNyxDataValue;
    FRootRemovals: array of INyxRootRemoval;
    FRootReviewSerial: Integer;
    { Eight file reservations, at most 8 MiB in total. Uploads/candidates are
      immutable interfaces in separate arrays for pas2js. Metadata is detached
      for rollback; authority and review tickets never enter project recovery. }
    FProjectImports: array of TNyxDataValue;
    FProjectUploads: array of INyxProjectImportUpload;
    FProjectCandidates: array of INyxProjectImportCandidate;
    FProjectImportSerial: Integer;
    FProjectReviewSerial: Integer;
    { Lazy exact export values for scalar windows at one revision. Avoid
      regenerating the document/Pascal and re-encoding the complete packet on
      every page. Changed retires them with the same snapshot authority. }
    FProjectExportRevision: Integer;
    FProjectExportPair: TNyxProjectPair;
    FProjectExportPacket: TNyxText;
    procedure RemoveProjectImport(AIndex: Integer);
    function ProjectFile(const AArguments: TNyxDataValue;
      const AAuthority: TNyxText): TNyxDataValue;
    function ResourceRuntimeReports: TNyxDataValue; overload;
    function ResourceRuntimeReports(const AReference: TNyxResourceRef;
      const ALocale: TNyxLocaleRef): TNyxDataValue; overload;
    function GetPendingDraft: Boolean;
    function ResourceRuntimeQuery(const AArguments: TNyxDataValue): TNyxDataValue;
    procedure Changed;
    procedure RequireRevision(const AArguments: TNyxDataValue);
    procedure Log(const AActor, AOperation, AOutcome: TNyxText);
    function Summary: TNyxDataValue;
    function EditorState(AAfter: Integer): TNyxDataValue; overload;
    function EditorState(AAfter: Integer; const AReference: TNyxResourceRef;
      const ALocale: TNyxLocaleRef): TNyxDataValue; overload;
    function Outline(const AArguments: TNyxDataValue): TNyxDataValue;
    function NodeDetails(const AArguments: TNyxDataValue): TNyxDataValue;
    { One requested local/effective value domain, with bounded Unicode choice
      windows. Realized projections are released before the response escapes. }
    function ValueDomainDetails(ANode: TNyxNode;
      const AArguments: TNyxDataValue): TNyxDataValue;
    { Paged reachable named paths from an independent effective projection. }
    function NamedParts(ANode: TNyxNode; AOffset, ALimit: Integer): TNyxDataValue;
    function Components(const AArguments: TNyxDataValue): TNyxDataValue;
    function Presentations(const AArguments: TNyxDataValue): TNyxDataValue;
    { Lists summaries only; one exact name pages its policy title and entries. }
    function Menus(const AArguments: TNyxDataValue): TNyxDataValue;
    function Diagnostics(const AArguments: TNyxDataValue): TNyxDataValue;
    function SourceLines(const AArguments: TNyxDataValue): TNyxDataValue;
    function CompilerSnapshot: TNyxDataValue;
    function HandlerSource(const AArguments: TNyxDataValue;
      AApply: Boolean): TNyxDataValue;
    { Bounded accepted-default/binding context and one typed grouped mutation.
      The same revision, permission, receipt and paired Undo guards apply. }
    function StateBindings(const AArguments: TNyxDataValue): TNyxDataValue;
    { Paged document collection/schema/row/domain and exact authored view context;
      one typed grouped candidate uses the same guarded paired publication. }
    function Collections(const AArguments: TNyxDataValue): TNyxDataValue;
    function Resources(const AArguments: TNyxDataValue): TNyxDataValue;
    { Bounded exact-section import context and one typed paired source edit. }
    function Imports(const AArguments: TNyxDataValue; AApply: Boolean): TNyxDataValue;
    { Bounded helper discovery/text and guarded grouped implementation editing. }
    function Routines(const AArguments: TNyxDataValue; AApply: Boolean): TNyxDataValue;
    { Exact interface counterpart context and one grouped declaration edit. }
    function Declarations(const AArguments: TNyxDataValue; AApply: Boolean): TNyxDataValue;
    { Bounded exact managed-builder text and one guarded paired source command.
      Application helpers/imports remain outside its immutable edit boundary. }
    function Views(const AArguments: TNyxDataValue; AApply: Boolean): TNyxDataValue;
    { Bounded accepted-unit context and exact range groups share ordinary paired
      admission; no pending draft or compiler profile is edited through this API. }
    function SourceUnit(const AArguments: TNyxDataValue; AApply: Boolean): TNyxDataValue;
    function EditCallbacks(const AArguments: TNyxDataValue;
      const AActor: TNyxText; AApply: Boolean): TNyxDataValue;
    function RemoveRoots(const AArguments: TNyxDataValue;
      const AActor: TNyxText; AApply: Boolean): TNyxDataValue;
  public
    constructor Create; overload;
    { Trusted independent review seed; accepts an owned pair through ordinary
      Studio admission and empty history, without a sample/claim replacement. }
    constructor Create(const APair: TNyxProjectPair); overload;
    { New authoring owner from admitted durable values. Connection state expires. }
    constructor CreateRecovered(const AFrame: TNyxAgentRecoveryFrame);
    { Equivalent owned rollback constructor; origin remains wholly borrowed. }
    constructor CreateCopy(AOrigin: TNyxAgentSession);
    { Owned in-process rollback copy includes transient authority and retry state.
      This is never serialized; immutable commands/reports retain managed values,
      while the mutable Studio session and every backing array are independent. }
    function Clone: TNyxAgentSession;
    { Trusted transport retirement releases only this owner's private uploads.
      Accepted project, history and other live owners remain unchanged. }
    procedure ReleaseProjectImports(const AOwner: TNyxText);
    { Durable authoring values, excluding all transient transport authority. }
    function RecoveryFrame: TNyxAgentRecoveryFrame;
    { Small copied dirty metadata, independent of whole-document/history size. }
    function RecoveryStamp: TNyxText;
    destructor Destroy; override;
    { Arguments are admitted JSON data at this explicit semantic boundary.
      Results are bounded immutable copies. Rejected operations retain accepted
      pair, editable draft and history. Activity records both success and refusal. }
    { An owning transport may supply a private request identity independently
      of its friendly display actor. Receipts and warned-removal tickets use
      this identity; activity continues to show the readable actor. }
    function Call(const ATool, AActor: TNyxText;
      const AArguments: TNyxDataValue; const ARequestOwner: TNyxText = ''): TNyxDataValue;
    { Private editor transport, never advertised as an MCP tool. Observations
      send paired files only after a changed revision. Commit is compare-and-swap;
      first attachment may claim recovered local files, later views observe.
      Editor undo/redo uses this authoritative ordinary Studio history. }
    function Exchange(const ARequest: TNyxDataValue): TNyxDataValue;
    { Preview/render workers receive an independent accepted pair under the
      transport lock, then release it before invoking optional external renderers. }
    function PreviewPair(AExpected: Integer; const AView: TNyxText;
      const AActor: TNyxText = 'MCP client'): TNyxProjectPair; overload;
    { Validate a copied manual choice at the same revision before capturing the
      immutable pair. Observing editor selection/navigation/history stay intact. }
    function PreviewPair(AExpected: Integer; const AView, AActor: TNyxText;
      const ASelection: TNyxPresentationSelection): TNyxProjectPair; overload;
    { Transport-owned work, such as rendering outside the model lock, reports
      completion/refusal through the same bounded operator-visible activity. }
    procedure RecordActivity(const AActor, AOperation, AOutcome: TNyxText);
    { Native job admission captures immutable text, never the mutable session.
      Edit permission, exact revision and absence of a pending draft are required.
      Scope/root agreement is checked before any compiler can be launched. }
    function BuildPair(AExpected: Integer; AScope: TNyxBuildScope;
      const AView: TNyxText): TNyxProjectPair;
    { Trusted editor admission is independent of agent enablement. Only the
      authenticated private operator transport calls it; revisions, drafts and
      root/scope agreement remain identical to agent compilation. }
    function EditorBuildPair(AExpected: Integer; AScope: TNyxBuildScope;
      const AView: TNyxText): TNyxProjectPair;
    { Private operator compilation may inspect an unfinished source draft.
      Capture the exact current pair at this revision without publishing,
      consuming the draft or granting public agent execution authority. }
    function EditorSourcePair(AExpected: Integer): TNyxProjectPair;
    { Trusted compiler completion at the captured full baseline, not a wire tool.
      AProjection must already come from an owned execution channel. Pending
      user text must be that exact source; stale or divergent work is retained.
      Selection/view are typed copied identities, never borrowed result nodes. }
    function CommitSourceProjection(AExpected: Integer;
      const ABaseline: TNyxProjectPair; const AProjection: INyxSourceProjection;
      const ARequest: TNyxStudioSourceRequest; const ASchemas: INyxSchemaSnapshot;
      const ASelection, AView: TNyxControlRef): TNyxDataValue;
    { Capture the ordinary source request and immutable creator environment
      without changing the draft. Source must be the editor's exact current
      buffer, including accepted text when no draft is pending. }
    function CaptureSourcePublication(AExpected: Integer; const ASource: TNyxText;
      out ARequest: TNyxStudioSourceRequest;
      out ASchemas: INyxSchemaSnapshot): TNyxProjectPair;
    { Independently prepare an intent against this server-owned session. Supplied
      source must equal the server's writer output; an unfinished buffer stays
      untouched. Both results are immutable nonpublishable proposal values. }
    function CaptureVisualPublication(AExpected: Integer; const AIntent: TNyxDataValue;
      const ASource: TNyxText; out ARequest: TNyxStudioDesignRequest;
      out AProposal: INyxPreparedDesign;
      out ASchemas: INyxSchemaSnapshot): TNyxProjectPair;
    { Actual executed source must reproduce the whole captured proposal before
      one revision/history change. Stale files/draft/schema refuse atomically. }
    function CommitVisualProjection(AExpected: Integer; const ABaseline: TNyxProjectPair;
      const AProjection: INyxSourceProjection; const ARequest: TNyxStudioDesignRequest;
      const AProposal: INyxPreparedDesign; const ASchemas: INyxSchemaSnapshot): TNyxDataValue;
    { Trusted observing host captures pair and opaque admitted frame at one
      exact revision. Both are immutable values; no accepted tree escapes. }
    procedure CaptureEditorProject(AExpected: Integer; out APair: TNyxProjectPair;
      out ACheckpoint: TNyxSourceCheckpoint);
    { Exact accepted pair comparison, independent of revision/selection changes.
      Pending drafts make diagnostics stale even if the accepted source matches. }
    function CurrentPair(const APair: TNyxProjectPair): Boolean;
    procedure PublishCompilerReport(const AReport: INyxCompilerReport);
    { Trusted runtime host boundary, never an MCP/editor exchange operation.
      Capture snapshots on the application's UI thread, then publish under the
      Studio host lock. Snapshot data owns no borrowed runtime/control pointers.
      Admission requires current revision, no pending draft, exact declarations
      and a concrete target. Eight observations bound membership. In-process
      rollback copies preserve capabilities; durable recovery does not. }
    function ObserveResourceRuntime(AExpected: Integer;
      const AReference: TNyxStudioRuntimeRef; AScope: TNyxStudioRuntimeScope;
      ATarget: TNyxPlatform; const AView: TNyxText;
      const ASnapshot: INyxResourceRuntimeSnapshot): TNyxStudioResourceObservation;
    procedure PublishResourceRuntime(const AObservation: TNyxStudioResourceObservation;
      const ASnapshot: INyxResourceRuntimeSnapshot);
    { Retains the last copied report as explicitly inactive. Does not cancel a
      borrowed application; its host owns Stop and final snapshot capture. }
    procedure RetireResourceRuntime(const AObservation: TNyxStudioResourceObservation);
    { Trusted workspace-owner boundary. Returns independent accepted text at an
      exact revision, excluding any pending editor draft. It never changes the
      source baseline, selection or either history stack. }
    function ReviewSeed(AExpected: Integer): TNyxProjectPair;
    { Only an owning controller may inherit the operator's current permission.
      This is not a semantic tool or editor command and creates no revision/history. }
    procedure InheritPermission(AValue: TNyxAgentPermission);
    property Revision: Integer read FRevision;
    { Exact source-pair draft state, read without logging a semantic operation. }
    property PendingDraft: Boolean read GetPendingDraft;
    property Permission: TNyxAgentPermission read FPermission;
  end;

{ Shared strict field helpers at JSON admission boundaries. Unknown arguments
  are refused so a misspelled field never silently changes command meaning. }
function NyxAgentHas(const AValue: TNyxDataValue; const AKey: TNyxText): Boolean;
procedure NyxAgentFields(const AValue: TNyxDataValue; const AAllowed: TNyxText);
function NyxAgentPermissionName(AValue: TNyxAgentPermission): TNyxText;
function NyxStudioRuntime(const AName: TNyxText): TNyxStudioRuntimeRef;

implementation

uses
  nyx.source.preparation, nyx.studio.projectionediting,
  nyx.catalog, nyx.catalog.labels, nyx.callbacks, nyx.codec, nyx.composition,
  Math, nyx.design.tokens, nyx.studio.edits, nyx.studio.callbackedits,
  nyx.studio.handleredits, nyx.studio.stateedits, nyx.state, nyx.binding,
  nyx.binding.types, nyx.contract, nyx.collections, nyx.collections.view.types,
  nyx.collections.selection, nyx.collections.query,
  nyx.studio.collectionedits, nyx.studio.transactions, nyx.studio.importedits,
  nyx.studio.routineedits, nyx.studio.declarationedits, nyx.times,
  nyx.resource.sources, nyx.bytes, nyx.studio.resourceedits,
  nyx.resources.rows, nyx.resources.catalog, nyx.collections.registry, nyx.studio.viewsedits,
  nyx.studio.sourceedits;

function NyxAgentHas(const AValue: TNyxDataValue; const AKey: TNyxText): Boolean;
var
  LIndex: Integer;
begin
  Result := False;

  if AValue.Kind <> ndObject then
  begin
    raise ENyxModel.Create('Arguments must be an object');
  end;
  for LIndex := 0 to AValue.Count - 1 do
  begin

    if AValue.Key(LIndex) = AKey then
    begin
      Exit(True);
    end;
  end;
end;

procedure NyxAgentFields(const AValue: TNyxDataValue; const AAllowed: TNyxText);
var
  LIndex: Integer;
begin

  if AValue.Kind <> ndObject then
  begin
    raise ENyxModel.Create('Arguments must be an object');
  end;
  for LIndex := 0 to AValue.Count - 1 do
  begin

    { Pipe delimits this internal closed-key list; it is never part of a key.
      Refuse joined names instead of accepting a substring across two entries. }
    if (Pos('|', AValue.Key(LIndex)) > 0) or
      (Pos('|' + AValue.Key(LIndex) + '|', AAllowed) = 0) then
    begin
      raise ENyxModel.Create('Unknown argument: ' + AValue.Key(LIndex));
    end;
  end;
end;

function NyxAgentPermissionName(AValue: TNyxAgentPermission): TNyxText;
const
  CNames: array[TNyxAgentPermission] of TNyxText = ('disabled', 'readOnly', 'edit');
begin
  Result := CNames[AValue];
end;

function IntegerArgument(const AArgs: TNyxDataValue; const AKey: TNyxText;
  ADefault, AMinimum, AMaximum: Integer): Integer;
begin
  Result := ADefault;

  if NyxAgentHas(AArgs, AKey) then
  begin
    Result := AArgs.Field(AKey).AsInteger;
  end;

  if (Result < AMinimum) or (Result > AMaximum) then
  begin
    raise ENyxModel.Create('Argument outside its published bounds: ' + AKey);
  end;
end;

function TextArgument(const AArgs: TNyxDataValue; const AKey: TNyxText;
  const ADefault: TNyxText = ''): TNyxText;
begin
  Result := ADefault;

  if NyxAgentHas(AArgs, AKey) then
  begin
    Result := AArgs.Field(AKey).AsText;
  end;
end;

function TextSpan(const AText: TNyxText; AOffset, ALimit: Integer;
  out ATotal: Integer): TNyxText;
var
  LIndex: Integer;
  LBefore: Integer;
  LStart: Integer;
  LEnd: Integer;
  LScalar: Integer;
begin
  { Offsets/counts use Unicode scalars on both targets. Return an exact slice,
    never split UTF-8 or UTF-16 and never append decoration to application data. }
  ATotal := 0;
  LIndex := 1;
  LStart := Length(AText) + 1;
  LEnd := LStart;
  while LIndex <= Length(AText) do
  begin
    LBefore := LIndex;

    if not NyxNextScalar(AText, LIndex, LScalar) then
    begin
      raise ENyxModel.Create('Malformed Unicode in agent context');
    end;

    if ATotal = AOffset then
    begin
      LStart := LBefore;
    end;

    if ATotal < AOffset + ALimit then
    begin
      LEnd := LIndex;
    end;
    Inc(ATotal);
  end;
  Result := Copy(AText, LStart, LEnd - LStart);
end;

function CaptionText(const AText: TNyxText; ALimit: Integer = 256): TNyxText;
const
  CEllipsis: TNyxText = '…';
var
  LTotal: Integer;
begin
  Result := TextSpan(AText, 0, ALimit, LTotal);

  if LTotal > ALimit then
  begin
    Result := Result + CEllipsis;
  end;
end;

procedure BoundContext(const AValue: TNyxDataValue);
var
  LText: TNyxText;
  LIndex: Integer;
  LScalar: Integer;
  LBytes: Integer;
begin
  LText := AValue.ToJSON;
  LIndex := 1;
  LBytes := 0;
  while LIndex <= Length(LText) do
  begin

    if not NyxNextScalar(LText, LIndex, LScalar) then
    begin
      raise ENyxModel.Create('Malformed context encoding');
    end;

    if LScalar <= $7f then
    begin
      Inc(LBytes);
    end
    else if LScalar <= $7ff then
    begin
      Inc(LBytes, 2);
    end
    else if LScalar <= $ffff then
    begin
      Inc(LBytes, 3);
    end
    else
    begin
      Inc(LBytes, 4);
    end;

    if LBytes > 48 * 1024 then
    begin
      raise ENyxModel.Create('Context exceeds 48 KiB; request fewer items, property keys or text/source scalars');
    end;
  end;
end;

function Brief(ANode: TNyxNode): TNyxDataValue;
var
  LParent: TNyxText;
begin
  LParent := '';

  if ANode.Parent <> nil then
  begin
    LParent := ANode.Parent.ID;
  end;
  Result := NyxObject([NyxField('id', NyxData(ANode.ID)),
    NyxField('kind', NyxData(ANode.Kind)), NyxField('parent', NyxData(LParent)),
    NyxField('childCount', NyxData(ANode.Count))]);
end;

constructor TNyxAgentSession.CreateRecovered(const AFrame: TNyxAgentRecoveryFrame);
begin
  inherited Create;

  if AFrame.Revision < 1 then
  begin
    raise ENyxModel.Create('Recovery session revision must be positive');
  end;
  FSession := TNyxStudioSession.CreateRecovered(AFrame.Session);
  FRevision := AFrame.Revision;
  FPermission := AFrame.Permission;
  FClaimed := AFrame.Claimed;
end;

function TNyxAgentSession.RecoveryFrame: TNyxAgentRecoveryFrame;
begin
  Result.Session := FSession.RecoveryFrame;
  Result.Revision := FRevision;
  Result.Permission := FPermission;
  Result.Claimed := FClaimed;
end;

function TNyxAgentSession.RecoveryStamp: TNyxText;
begin
  Result := NyxObject([NyxField('revision', NyxData(FRevision)),
    NyxField('permission', NyxData(Ord(FPermission))),
    NyxField('claimed', NyxData(FClaimed)),
    NyxField('session', NyxData(FSession.RecoveryStamp))]).ToJSON;
end;

function TNyxAgentSession.Clone: TNyxAgentSession;
begin
  Result := TNyxAgentSession.CreateCopy(Self);
end;

constructor TNyxAgentSession.CreateCopy(AOrigin: TNyxAgentSession);
var
  LIndex: Integer;
begin
  inherited Create;

  if AOrigin = nil then
  begin
    raise ENyxModel.Create('An agent rollback copy requires its origin');
  end;
  FSession := AOrigin.FSession.Clone;
  FRevision := AOrigin.FRevision;
  FPermission := AOrigin.FPermission;
  FClaimed := AOrigin.FClaimed;
  FActivity := Copy(AOrigin.FActivity);
  FActivitySerial := AOrigin.FActivitySerial;
  FReport := AOrigin.FReport;
  FCompilerSequence := AOrigin.FCompilerSequence;
  SetLength(FResourceRuns, Length(AOrigin.FResourceRuns));
  for LIndex := 0 to High(FResourceRuns) do
  begin
    FResourceRuns[LIndex] := AOrigin.FResourceRuns[LIndex];
  end;
  FResourceSnapshots := Copy(AOrigin.FResourceSnapshots);
  FReceiptKeys := Copy(AOrigin.FReceiptKeys);
  FReceiptRequests := Copy(AOrigin.FReceiptRequests);
  FReceiptResults := Copy(AOrigin.FReceiptResults);
  FCallbackReviews := Copy(AOrigin.FCallbackReviews);
  FCallbackReviewSerial := AOrigin.FCallbackReviewSerial;
  FRootReviews := Copy(AOrigin.FRootReviews);
  FRootRemovals := Copy(AOrigin.FRootRemovals);
  FRootReviewSerial := AOrigin.FRootReviewSerial;
  FProjectImports := Copy(AOrigin.FProjectImports);
  FProjectUploads := Copy(AOrigin.FProjectUploads);
  FProjectCandidates := Copy(AOrigin.FProjectCandidates);
  FProjectImportSerial := AOrigin.FProjectImportSerial;
  FProjectReviewSerial := AOrigin.FProjectReviewSerial;
  FProjectExportRevision := AOrigin.FProjectExportRevision;
  FProjectExportPair := AOrigin.FProjectExportPair;
  FProjectExportPacket := AOrigin.FProjectExportPacket;
end;

constructor TNyxAgentSession.Create;
begin
  inherited Create;
  FSession := TNyxStudioSession.Create;
  FRevision := 1;
  { The product owner explicitly requested enabled editing by default. Operator
    controls can reduce or disable access without changing project data. }
  FPermission := apEdit;
end;

constructor TNyxAgentSession.Create(const APair: TNyxProjectPair);
begin
  inherited Create;
  FSession := TNyxStudioSession.Create(APair);
  FRevision := 1;
  FPermission := apEdit;
  FClaimed := True;
end;

destructor TNyxAgentSession.Destroy;
begin
  FReport := nil;
  FSession.Free;
  inherited Destroy;
end;

procedure TNyxAgentSession.Changed;
begin

  if FRevision = High(Integer) then
  begin
    raise ENyxModel.Create('Session revision budget exhausted; reconnect a new session');
  end;
  Inc(FRevision);
  FResourceRuns := nil;
  FResourceSnapshots := nil;
  { Every upload/review is pinned to this exact authoring revision. Retiring
    reservations here bounds stale memory and prevents accidental later use. }
  FProjectImports := nil;
  FProjectUploads := nil;
  FProjectCandidates := nil;
  FProjectExportRevision := 0;
  FProjectExportPair := NyxProjectPair('', '');
  FProjectExportPacket := '';
  FClaimed := True;
end;

procedure TNyxAgentSession.RequireRevision(const AArguments: TNyxDataValue);
begin

  if AArguments.Field('expectedRevision').AsInteger <> FRevision then
  begin
    raise ENyxModel.Create('Revision conflict; inspect current context before retrying');
  end;

  if FRevision = High(Integer) then
  begin
    raise ENyxModel.Create('Session revision budget exhausted');
  end;
end;

procedure TNyxAgentSession.Log(const AActor, AOperation, AOutcome: TNyxText);
var
  LIndex: Integer;
begin
  Inc(FActivitySerial);

  if Length(FActivity) = 24 then
  begin
    for LIndex := 1 to High(FActivity) do
    begin
      FActivity[LIndex - 1] := FActivity[LIndex];
    end;
    SetLength(FActivity, 23);
  end;
  SetLength(FActivity, Length(FActivity) + 1);
  FActivity[High(FActivity)] := NyxObject([
    NyxField('sequence', NyxData(FActivitySerial)),
    NyxField('actor', NyxData(CaptionText(AActor, 100))), NyxField('operation', NyxData(CaptionText(AOperation, 100))),
    NyxField('outcome', NyxData(CaptionText(AOutcome, 320))), NyxField('revision', NyxData(FRevision))]);
end;

function TNyxAgentSession.Summary: TNyxDataValue;
begin
  Result := NyxObject([NyxField('revision', NyxData(FRevision)),
    NyxField('permission', NyxData(NyxAgentPermissionName(FPermission))),
    NyxField('title', NyxData(CaptionText(FSession.Document.Title, 160))),
    NyxField('selection', NyxData(FSession.SelectedID)),
    NyxField('view', NyxData(FSession.ActiveViewID)),
    NyxField('pages', NyxData(FSession.Document.Count)),
    NyxField('components', NyxData(FSession.Document.ComponentCount)),
    NyxField('pendingDraft', NyxData(FSession.DraftSource <> FSession.Source)),
    NyxField('canUndo', NyxData(FSession.CanUndo)),
    NyxField('canRedo', NyxData(FSession.CanRedo)),
    NyxField('activitySequence', NyxData(FActivitySerial))]);
end;

function TNyxAgentSession.EditorState(AAfter: Integer): TNyxDataValue;
begin
  Result := EditorState(AAfter, Default(TNyxResourceRef), NyxDefaultLocale);
end;

function TNyxAgentSession.EditorState(AAfter: Integer;
  const AReference: TNyxResourceRef; const ALocale: TNyxLocaleRef): TNyxDataValue;
var
  LFields: array of TNyxDataField;
begin
  SetLength(LFields, 5);
  LFields[0] := NyxField('session', Summary);
  LFields[1] := NyxField('activity', NyxArray(FActivity));
  LFields[2] := NyxField('compiler', CompilerSnapshot);
  LFields[3] := NyxField('resourceRuntimes', ResourceRuntimeReports(AReference, ALocale));
  LFields[4] := NyxField('resourceRuntimeSelection', NyxData(True));

  if AAfter <> FRevision then
  begin
    SetLength(LFields, 6);
    LFields[5] := NyxField('project', NyxData(EncodeNyxProject(FSession.ProjectSnapshot)));
  end;
  Result := NyxObject(LFields);
end;

function TNyxAgentSession.Outline(const AArguments: TNyxDataValue): TNyxDataValue;
var
  LItems: array of TNyxDataValue;
  LParent: TNyxNode;
  LScope: TNyxText;
  LOffset: Integer;
  LLimit: Integer;
  LTotal: Integer;
  LIndex: Integer;
  LCount: Integer;
begin
  NyxAgentFields(AArguments, '|parent|scope|offset|limit|');
  LOffset := IntegerArgument(AArguments, 'offset', 0, 0, 100000);
  LLimit := IntegerArgument(AArguments, 'limit', 25, 1, 50);
  LScope := TextArgument(AArguments, 'scope', 'pages');
  LParent := nil;

  if NyxAgentHas(AArguments, 'parent') then
  begin
    LParent := FSession.Document.Find(AArguments.Field('parent').AsText);

    if LParent = nil then
    begin
      raise ENyxModel.Create('Outline parent is missing');
    end;
    LTotal := LParent.Count;
  end
  else if LScope = 'pages' then
  begin
    LTotal := FSession.Document.Count;
  end
  else if LScope = 'components' then
  begin
    LTotal := FSession.Document.ComponentCount;
  end
  else
  begin
    raise ENyxModel.Create('Outline scope must be pages or components');
  end;
  LCount := 0;
  SetLength(LItems, LLimit);
  for LIndex := LOffset to LTotal - 1 do
  begin

    if LCount = LLimit then
    begin
      Break;
    end;

    if LParent <> nil then
    begin
      LItems[LCount] := Brief(LParent.Children[LIndex]);
    end
    else if LScope = 'pages' then
    begin
      LItems[LCount] := Brief(FSession.Document.Pages[LIndex]);
    end
    else
    begin
      LItems[LCount] := Brief(FSession.Document.Components[LIndex]);
    end;
    Inc(LCount);
  end;
  SetLength(LItems, LCount);
  Result := NyxObject([NyxField('revision', NyxData(FRevision)),
    NyxField('offset', NyxData(LOffset)), NyxField('total', NyxData(LTotal)),
    NyxField('items', NyxArray(LItems))]);
end;

function PropertyValue(const AInfo: TNyxPropertyInfo; const AText: TNyxText): TNyxDataValue;
var
  LInteger: Integer;
begin

  if AText = '' then
  begin
    Exit(NyxNull);
  end;
  case AInfo.ValueType of
    npBoolean: Result := NyxData(AText = 'true');
    npInteger:
      begin

        if not TryNyxInteger(AText, LInteger) then
        begin
          raise ENyxModel.Create('Invalid admitted integer property');
        end;
        Result := NyxData(LInteger);
      end;
    npNumber: Result := NyxData(NyxDecimal(AText));
    else
    begin
      Result := NyxData(AText);
    end;
  end;
end;

function TNyxAgentSession.NamedParts(ANode: TNyxNode;
  AOffset, ALimit: Integer): TNyxDataValue;
var
  LContext, LProjection: TNyxNode;
  LItems: array of TNyxDataValue;
  LTotal: Integer;

  procedure Visit(APart: TNyxNode; const APath: TNyxText);
  var
    LIndex, LCount: Integer;
    LPath, LRuleID: TNyxText;
  begin

    if (LTotal >= AOffset) and (Length(LItems) < ALimit) then
    begin
      LRuleID := '';
      for LIndex := 0 to ANode.Count - 1 do
      begin

        if (ANode.Children[LIndex].Kind = 'slot-override') and
          (ANode.Children[LIndex].Prop('path') = APath) then
        begin
          LRuleID := ANode.Children[LIndex].ID;
          Break;
        end;
      end;
      LCount := Length(LItems);
      SetLength(LItems, LCount + 1);
      LItems[LCount] := NyxObject([NyxField('path', NyxData(APath)),
        NyxField('kind', NyxData(APart.Kind)),
        NyxField('source', NyxData(APart.SourceID)),
        NyxField('designID', NyxData(APart.DesignID)),
        NyxField('overrideID', NyxData(LRuleID))]);
    end;
    Inc(LTotal);
    for LIndex := 0 to APart.Count - 1 do
    begin
      LPath := APart.Children[LIndex].Prop('part');

      if LPath <> '' then
      begin
        { Resolve through the same contract before reporting a path. Duplicate
          sibling names refuse instead of returning misleading source identity. }
        APart.Part(NyxPart(LPath));

        if APath <> '.' then
        begin
          LPath := APath + '/' + LPath;
        end;
        { Only direct named children are reachable by the public Part contract.
          No unnamed ancestor is invented as a path segment. }
        Visit(APart.Children[LIndex], LPath);
      end;
    end;
  end;
begin
  LTotal := 0;
  LContext := RealizeNyxContext(FSession.Document, ANode, LProjection);
  try

    if LProjection <> nil then
    begin
      Visit(LProjection, '.');
    end;
    Result := NyxObject([NyxField('offset', NyxData(AOffset)),
      NyxField('total', NyxData(LTotal)), NyxField('items', NyxArray(LItems))]);
  finally
    LContext.Free;
  end;
end;

function TNyxAgentSession.NodeDetails(const AArguments: TNyxDataValue): TNyxDataValue;
const
  CTypes: array[TNyxPropertyType] of TNyxText = ('string', 'lines', 'boolean',
    'integer', 'number', 'enum', 'reference');
var
  LNode: TNyxNode;
  LInfos: TNyxPropertyInfos;
  LEventContext: TNyxNode;
  LEventProjection: TNyxNode;
  LItems: array of TNyxDataValue;
  LEvents: TNyxEventSchemas;
  LEventItems: array of TNyxDataValue;
  LEventOffset: Integer;
  LEventLimit: Integer;
  LEventCount: Integer;
  LPayload: TNyxDataValue;
  LRouteItems: array of TNyxDataValue;
  LRouteIndex: Integer;
  LRouteOffset: Integer;
  LRouteLimit: Integer;
  LRouteCount: Integer;
  LRouteEventCount: Integer;
  LRouteTotal: Integer;
  LAuthored: TNyxAuthoredEventInfos;
  LRegistrations: TNyxAuthoredEventInfos;
  LAuthoredIndex: Integer;
  LCallbackIndex: Integer;
  LRegistrationIndex: Integer;
  LRegistrationOffset: Integer;
  LRegistrationLimit: Integer;
  LRegistrationCount: Integer;
  LRegistrationTotal: Integer;
  LIndex: Integer;
  LCount: Integer;
  LOffset: Integer;
  LLimit: Integer;
  LFields: array of TNyxDataField;
  LKeys: TNyxDataValue;
  LTextOffset: Integer;
  LTextLimit: Integer;
  LKeyIndex: Integer;
  LMatch: Integer;
  LContentOffset: Integer;
  LContentLimit: Integer;
  LContentTotal: Integer;
  LContentItems: array of TNyxDataValue;
  LTotalText: Integer;
  LMatches: Boolean;
  LText: TNyxText;
  LValue: TNyxDataValue;
begin
  NyxAgentFields(AArguments, '|id|offset|limit|events|eventOffset|eventLimit|registrationOffset|registrationLimit|routeOffset|routeLimit|keys|textOffset|textLimit|parts|partOffset|partLimit|content|contentOffset|contentLimit|valueDomain|domainScope|domainOffset|domainLimit|');
  LNode := FSession.Document.Find(TextArgument(AArguments, 'id', FSession.SelectedID));

  if LNode = nil then
  begin
    raise ENyxModel.Create('Component is missing');
  end;
  LInfos := NyxProperties(LNode, FSession.Document);
  LOffset := IntegerArgument(AArguments, 'offset', 0, 0, 100000);
  LLimit := IntegerArgument(AArguments, 'limit', 20, 1, 50);
  LTextOffset := IntegerArgument(AArguments, 'textOffset', 0, 0, 1000000);
  LTextLimit := IntegerArgument(AArguments, 'textLimit', 512, 1, 2048);
  LKeys := NyxArray([]);

  if NyxAgentHas(AArguments, 'keys') then
  begin
    LKeys := AArguments.Field('keys');

    if (LKeys.Kind <> ndArray) or (LKeys.Count > 20) then
    begin
      raise ENyxModel.Create('Property keys require an array of at most 20 exact published names');
    end;
    for LKeyIndex := 0 to LKeys.Count - 1 do
    begin
      LMatches := False;
      for LIndex := 0 to High(LInfos) do
      begin

        if LInfos[LIndex].Key = LKeys.Item(LKeyIndex).AsText then
        begin
          LMatches := True;
        end;
      end;

      if not LMatches then
      begin
        raise ENyxModel.Create('Requested property is not published');
      end;
    end;
  end;
  SetLength(LItems, LLimit);
  LCount := 0;
  LMatch := 0;
  for LIndex := 0 to High(LInfos) do
  begin
    LMatches := LKeys.Count = 0;
    for LKeyIndex := 0 to LKeys.Count - 1 do
    begin

      if LKeys.Item(LKeyIndex).AsText = LInfos[LIndex].Key then
      begin
        LMatches := True;
      end;
    end;

    if not LMatches then
    begin
      Continue;
    end;
    Inc(LMatch);

    if (LMatch <= LOffset) or (LCount = LLimit) then
    begin
      Continue;
    end;
    LText := LNode.Prop(LInfos[LIndex].Key);
    LValue := PropertyValue(LInfos[LIndex], LText);
    LTotalText := 0;

    if (LValue.Kind = ndText) and (LInfos[LIndex].ValueType <> npReference) then
    begin
      LValue := NyxData(TextSpan(LText, LTextOffset, LTextLimit, LTotalText));
    end;

    LItems[LCount] := NyxObject([
      NyxField('key', NyxData(LInfos[LIndex].Key)),
      NyxField('title', NyxData(LInfos[LIndex].Title)),
      NyxField('type', NyxData(CTypes[LInfos[LIndex].ValueType])),
      NyxField('value', LValue),
      NyxField('textOffset', NyxData(LTextOffset)), NyxField('totalScalars', NyxData(LTotalText)),
      NyxField('truncated', NyxData((LValue.Kind = ndText) and (LInfos[LIndex].ValueType <> npReference) and
        ((LTextOffset > 0) or (LTotalText > LTextOffset + LTextLimit)))),
      NyxField('default', NyxData(CaptionText(LInfos[LIndex].DefaultValue, 160))),
      NyxField('choices', NyxData(CaptionText(LInfos[LIndex].Choices, 512))),
      NyxField('meaning', NyxData(NyxPropertyMeaningText(LInfos[LIndex].Support.Meaning))),
      NyxField('browser', NyxData(NyxCapabilityText(LInfos[LIndex].Support.Browser))),
      NyxField('native', NyxData(NyxCapabilityText(LInfos[LIndex].Support.Native))),
      NyxField('help', NyxData(CaptionText(LInfos[LIndex].Support.Description, 256))),
      NyxField('minimum', NyxData(LInfos[LIndex].Minimum)),
      NyxField('maximum', NyxData(LInfos[LIndex].Maximum))]);
    Inc(LCount);
  end;
  SetLength(LItems, LCount);
  SetLength(LFields, 5);
  LFields[0] := NyxField('revision', NyxData(FRevision));
  LFields[1] := NyxField('node', Brief(LNode));
  LFields[2] := NyxField('totalProperties', NyxData(LMatch));
  LFields[3] := NyxField('offset', NyxData(LOffset));
  LFields[4] := NyxField('properties', NyxArray(LItems));

  if NyxAgentHas(AArguments, 'events') and AArguments.Field('events').AsBoolean then
  begin
    LEvents := NyxEventsMetadata(LNode, FSession.Document);
    LEventOffset := IntegerArgument(AArguments, 'eventOffset', 0, 0, 100000);
    LEventLimit := IntegerArgument(AArguments, 'eventLimit', 32, 1, 50);
    LEventCount := 0;
    { Registrations must describe the same realized instance as discovery,
      including inherited callbacks and part overrides. Decode returns values;
      release the temporary owner before constructing the bounded response. }
    LEventContext := RealizeNyxContext(FSession.Document, LNode, LEventProjection);
    try
      LAuthored := nil;

      if LEventProjection <> nil then
      begin
        LAuthored := NyxAuthoredEvents(LEventProjection);
      end;
    finally
      LEventContext.Free;
    end;
    LRegistrations := nil;
    LRegistrationOffset := IntegerArgument(AArguments, 'registrationOffset', 0, 0, 100000);
    LRegistrationLimit := IntegerArgument(AArguments, 'registrationLimit', 16, 1, 50);
    LRegistrationCount := 0;
    LRegistrationTotal := 0;
    LRouteOffset := IntegerArgument(AArguments, 'routeOffset', 0, 0, 100000);
    LRouteLimit := IntegerArgument(AArguments, 'routeLimit', 16, 1, 50);
    LRouteCount := 0;
    LRouteTotal := 0;
    SetLength(LEventItems, LEventLimit);
    for LIndex := LEventOffset to High(LEvents) do
    begin

      if LEventCount >= LEventLimit then
      begin
        Break;
      end;
      LPayload := NyxNull;

      if LEvents[LIndex].Payload.Defined then
      begin
        LPayload := LEvents[LIndex].Payload.ToData;
      end;
      { Page routes across this exact event window, rather than multiplying the
        response limit by every event. Empty pages retain stream identity/count. }
      SetLength(LRouteItems, LRouteLimit);
      LRouteEventCount := 0;
      for LRouteIndex := 0 to High(LEvents[LIndex].Routes) do
      begin

        if (LRouteTotal >= LRouteOffset) and (LRouteCount < LRouteLimit) then
        begin
          LRouteItems[LRouteEventCount] := LEvents[LIndex].Routes[LRouteIndex].ToData;
          Inc(LRouteEventCount);
          Inc(LRouteCount);
        end;
        Inc(LRouteTotal);
      end;
      SetLength(LRouteItems, LRouteEventCount);
      LEventItems[LEventCount] := NyxObject([
        NyxField('trigger', NyxData(NyxTriggerName(LEvents[LIndex].Trigger))),
        NyxField('name', NyxData(LEvents[LIndex].Name.Name)),
        NyxField('title', NyxData(CaptionText(LEvents[LIndex].Title, 160))),
        NyxField('description', NyxData(CaptionText(LEvents[LIndex].Description, 512))),
        NyxField('payload', LPayload),
        NyxField('payloadOptional', NyxData(LEvents[LIndex].PayloadOptional)),
        NyxField('declaredProducer', NyxData(LEvents[LIndex].DeclaredProducer)),
        NyxField('contexts', NyxEventContextsData(LEvents[LIndex].Contexts)),
        NyxField('totalRoutes', NyxData(Length(LEvents[LIndex].Routes))),
        NyxField('routes', NyxArray(LRouteItems)),
        NyxField('browser', NyxData(NyxCapabilityText(LEvents[LIndex].Browser))),
        NyxField('native', NyxData(NyxCapabilityText(LEvents[LIndex].Native)))]);
      Inc(LEventCount);
      { Callback context belongs only to these exact event identities. Page the
        flattened registrations in metadata-event order, retaining callback order
        and empty authored streams' policies. The envelope explicitly marks its
        partial window; it is never presented as a complete saved descriptor. }
      for LAuthoredIndex := 0 to High(LAuthored) do
      begin

        if (LAuthored[LAuthoredIndex].Trigger <> LEvents[LIndex].Trigger) or
          (LAuthored[LAuthoredIndex].Name.Name <> LEvents[LIndex].Name.Name) then
        begin
          Continue;
        end;
        LRegistrationIndex := Length(LRegistrations);
        SetLength(LRegistrations, LRegistrationIndex + 1);
        LRegistrations[LRegistrationIndex].Trigger := LAuthored[LAuthoredIndex].Trigger;
        LRegistrations[LRegistrationIndex].Name := LAuthored[LAuthoredIndex].Name;
        LRegistrations[LRegistrationIndex].Policy := LAuthored[LAuthoredIndex].Policy;
        for LCallbackIndex := 0 to High(LAuthored[LAuthoredIndex].Callbacks) do
        begin

          if (LRegistrationTotal >= LRegistrationOffset) and
            (LRegistrationCount < LRegistrationLimit) then
          begin
            LCount := Length(LRegistrations[LRegistrationIndex].Callbacks);
            SetLength(LRegistrations[LRegistrationIndex].Callbacks, LCount + 1);
            LRegistrations[LRegistrationIndex].Callbacks[LCount] :=
              LAuthored[LAuthoredIndex].Callbacks[LCallbackIndex];
            Inc(LRegistrationCount);
          end;
          Inc(LRegistrationTotal);
        end;
      end;
    end;
    SetLength(LEventItems, LEventCount);
    SetLength(LFields, 15);
    LFields[5] := NyxField('events', NyxArray(LEventItems));
    LFields[6] := NyxField('registrations', EncodeNyxAuthoredEvents(LRegistrations));
    LFields[7] := NyxField('totalEvents', NyxData(Length(LEvents)));
    LFields[8] := NyxField('eventOffset', NyxData(LEventOffset));
    LFields[9] := NyxField('totalRegistrations', NyxData(LRegistrationTotal));
    LFields[10] := NyxField('registrationOffset', NyxData(LRegistrationOffset));
    LFields[11] := NyxField('registrationsPartial', NyxData(
      (Length(LRegistrations) < Length(LAuthored)) or
      (LRegistrationOffset > 0) or (LRegistrationCount < LRegistrationTotal)));
    LFields[12] := NyxField('totalRoutes', NyxData(LRouteTotal));
    LFields[13] := NyxField('routeOffset', NyxData(LRouteOffset));
    LFields[14] := NyxField('routesPartial', NyxData(
      (LRouteOffset > 0) or (LRouteCount < LRouteTotal)));
  end;

  { Invoker attachment is a distinct portable field, never an arbitrary property.
    This reports the authored local declaration; realization owns inheritance. }
  SetLength(LFields, Length(LFields) + 1);
  LFields[High(LFields)] := NyxField('menuAttachment', NyxObject([
    NyxField('localDeclared', NyxData(LNode.HasMenu)),
    NyxField('name', NyxData(LNode.MenuReference.Name))]));
  SetLength(LFields, Length(LFields) + 1);
  LFields[High(LFields)] := NyxField('menuBar', NyxObject([
    NyxField('localDeclared', NyxData(LNode.HasMenuBar)),
    NyxField('configured', NyxData(LNode.MenuBar <> nil))]));

  if NyxAgentHas(AArguments, 'parts') and AArguments.Field('parts').AsBoolean then
  begin
    SetLength(LFields, Length(LFields) + 1);
    LFields[High(LFields)] := NyxField('parts', NamedParts(LNode,
      IntegerArgument(AArguments, 'partOffset', 0, 0, 100000),
      IntegerArgument(AArguments, 'partLimit', 20, 1, 50)));
  end;

  if NyxAgentHas(AArguments, 'content') and AArguments.Field('content').AsBoolean then
  begin
    LContentOffset := IntegerArgument(AArguments, 'contentOffset', 0, 0, 100000);
    LContentLimit := IntegerArgument(AArguments, 'contentLimit', 8, 1, 16);
    LContentTotal := 0;

    if LNode.HasContent then
    begin
      LContentTotal := LNode.Content.Count;
    end;
    LCount := 0;
    SetLength(LContentItems, LContentLimit);
    for LIndex := LContentOffset to LContentTotal - 1 do
    begin

      if LCount >= LContentLimit then
      begin
        Break;
      end;
      LContentItems[LCount] := LNode.Content.Rule(LIndex).ToData;
      Inc(LCount);
    end;
    SetLength(LContentItems, LCount);
    SetLength(LFields, Length(LFields) + 1);
    LFields[High(LFields)] := NyxField('content', NyxObject([
      NyxField('totalRules', NyxData(LContentTotal)),
      NyxField('offset', NyxData(LContentOffset)), NyxField('rules', NyxArray(LContentItems))]));
  end;

  if NyxAgentHas(AArguments, 'valueDomain') and AArguments.Field('valueDomain').AsBoolean then
  begin
    SetLength(LFields, Length(LFields) + 1);
    LFields[High(LFields)] := NyxField('valueDomain', ValueDomainDetails(LNode, AArguments));
  end;
  Result := NyxObject(LFields);
end;

function TNyxAgentSession.ValueDomainDetails(ANode: TNyxNode;
  const AArguments: TNyxDataValue): TNyxDataValue;
var
  LDomain: TNyxValueDomain;
  LProjection: TNyxNode;
  LContext: TNyxNode;
  LScope: TNyxText;
  LDeclared: Boolean;
  LData: TNyxDataValue;
  LChoices: TNyxDataValue;
  LItems: array of TNyxDataValue;
  LValue: TNyxDataValue;
  LType: TNyxText;
  LFormat: TNyxText;
  LMinimum: TNyxDataValue;
  LMaximum: TNyxDataValue;
  LFields: array of TNyxDataField;
  LStep: TNyxDataValue;
  LStepBase: TNyxText;
  LCrossesMidnight: Boolean;
  LOffset: Integer;
  LLimit: Integer;
  LTextOffset: Integer;
  LTextLimit: Integer;
  LTotal: Integer;
  LIndex: Integer;
  LCount: Integer;
begin
  LDeclared := ANode.Contract.FindValue(LDomain);
  LScope := TextArgument(AArguments, 'domainScope', 'effective');

  if (LScope <> 'local') and (LScope <> 'effective') then
  begin
    raise ENyxContract.Create('Domain scope is local or effective');
  end;

  if LScope = 'effective' then
  begin
    LContext := RealizeNyxContext(FSession.Document, ANode, LProjection);
    try
      LDomain := NyxNodeValueDomain(LProjection).Copy;
    finally
      LContext.Free;
    end;
  end;
  LOffset := IntegerArgument(AArguments, 'domainOffset', 0, 0, NyxMaximumDomainChoices);
  LLimit := IntegerArgument(AArguments, 'domainLimit', 8, 1, 16);
  LTextOffset := IntegerArgument(AArguments, 'textOffset', 0, 0, 1000000);
  LTextLimit := IntegerArgument(AArguments, 'textLimit', 80, 1, 2048);
  LData := LDomain.ToData;
  LChoices := NyxArray([]);
  LType := 'none';
  LFormat := '';
  LMinimum := NyxNull;
  LMaximum := NyxNull;

  if LDomain.Defined then
  begin
    LType := NyxStateKindName(LDomain.Kind);

    if LDomain.CalendarDate then
    begin
      LFormat := 'date';
    end;

    if LDomain.ClockTime then
    begin
      LFormat := 'time';
    end;

    if LDomain.RGBColor then
    begin
      LFormat := 'rgb';
    end;

    if NyxAgentHas(LData, 'choices') then
    begin
      LChoices := LData.Field('choices');
    end;

    if NyxAgentHas(LData, 'min') then
    begin
      LMinimum := LData.Field('min');
    end;

    if NyxAgentHas(LData, 'max') then
    begin
      LMaximum := LData.Field('max');
    end;
  end;
  SetLength(LItems, LLimit);
  LCount := 0;
  for LIndex := LOffset to LChoices.Count - 1 do
  begin

    if LCount = LLimit then
    begin
      Break;
    end;
    LValue := LChoices.Item(LIndex);
    LTotal := 0;

    if LValue.Kind = ndText then
    begin
      LValue := NyxData(TextSpan(LValue.AsText, LTextOffset, LTextLimit, LTotal));
    end;
    LItems[LCount] := NyxObject([NyxField('index', NyxData(LIndex)),
      NyxField('value', LValue), NyxField('textOffset', NyxData(LTextOffset)),
      NyxField('totalScalars', NyxData(LTotal)), NyxField('truncated', NyxData(
        (LValue.Kind = ndText) and ((LTextOffset > 0) or
        (LTotal > LTextOffset + LTextLimit))))]);
    Inc(LCount);
  end;
  SetLength(LItems, LCount);
  Result := NyxObject([NyxField('scope', NyxData(LScope)),
    NyxField('localDeclared', NyxData(LDeclared)), NyxField('defined', NyxData(LDomain.Defined)),
    NyxField('type', NyxData(LType)), NyxField('format', NyxData(LFormat)),
    NyxField('minimum', LMinimum), NyxField('maximum', LMaximum),
    NyxField('totalChoices', NyxData(LChoices.Count)), NyxField('offset', NyxData(LOffset)),
    NyxField('choices', NyxArray(LItems))]);

  if LDomain.ClockTime then
  begin
    { Add only clock context, never a complete contract/document. Null step and
      stepDeclared preserve absence versus explicit Any; arithmetic remains
      exact integer milliseconds and the declared minimum is the step base. }
    LStep := NyxNull;

    if NyxAgentHas(LData, 'step') then
    begin
      LStep := LData.Field('step');
    end;
    LStepBase := '00:00';

    if LMinimum.Kind = ndText then
    begin
      LStepBase := LMinimum.AsText;
    end;
    LCrossesMidnight := False;

    if (LMinimum.Kind = ndText) and (LMaximum.Kind = ndText) then
    begin
      LCrossesMidnight := TNyxClockTime.FromText(LMinimum.AsText)
        .Compare(TNyxClockTime.FromText(LMaximum.AsText)) > 0;
    end;
    LCount := Result.Count;
    SetLength(LFields, LCount + 5);
    for LIndex := 0 to LCount - 1 do
    begin
      LFields[LIndex] := NyxField(Result.Key(LIndex), Result.Field(Result.Key(LIndex)));
    end;
    LFields[LCount] := NyxField('stepDeclared', NyxData(LStep.Kind <> ndNull));
    LFields[LCount + 1] := NyxField('step', LStep);
    LFields[LCount + 2] := NyxField('stepMilliseconds', NyxData(LDomain.TimeStepMilliseconds));
    LFields[LCount + 3] := NyxField('stepBase', NyxData(LStepBase));
    LFields[LCount + 4] := NyxField('crossesMidnight', NyxData(LCrossesMidnight));
    Result := NyxObject(LFields);
  end;
end;

function TNyxAgentSession.Components(const AArguments: TNyxDataValue): TNyxDataValue;
var
  LIndex: Integer;
  LMatch: Integer;
  LCount: Integer;
  LOffset: Integer;
  LLimit: Integer;
  LQuery: TNyxText;
  LGroup: TNyxPaletteGroup;
  LInfo: TNyxComponentInfo;
  LItems: array of TNyxDataValue;
begin
  NyxAgentFields(AArguments, '|query|group|offset|limit|');
  LQuery := TextArgument(AArguments, 'query');
  LGroup := pgAll;

  if not TryNyxPaletteGroup(TextArgument(AArguments, 'group', 'all'), LGroup) then
  begin
    raise ENyxModel.Create('Unknown component intent group');
  end;
  LOffset := IntegerArgument(AArguments, 'offset', 0, 0, 100000);
  LLimit := IntegerArgument(AArguments, 'limit', 12, 1, 50);
  SetLength(LItems, LLimit);
  LCount := 0;
  LMatch := 0;
  for LIndex := 0 to FSession.Catalog.Count - 1 do
  begin

    if FSession.Catalog.Matches(LIndex, LQuery, LGroup) then
    begin

      if (LMatch >= LOffset) and (LCount < LLimit) then
      begin
        LInfo := FSession.Catalog[LIndex];
        LItems[LCount] := NyxObject([NyxField('kind', NyxData(LInfo.Kind)),
          NyxField('title', NyxData(LInfo.Title)),
          NyxField('group', NyxData(NyxPaletteGroupKey(LInfo.Discovery.Group))),
          NyxField('labels', NyxData(NyxComponentLabelNames(LInfo.Discovery.Labels))),
          NyxField('description', NyxData(CaptionText(LInfo.Discovery.Description, 512))),
          NyxField('container', NyxData(LInfo.Container))]);
        Inc(LCount);
      end;
      Inc(LMatch);
    end;
  end;
  SetLength(LItems, LCount);
  Result := NyxObject([NyxField('revision', NyxData(FRevision)),
    NyxField('total', NyxData(LMatch)), NyxField('offset', NyxData(LOffset)),
    NyxField('items', NyxArray(LItems))]);
end;

function TNyxAgentSession.Presentations(const AArguments: TNyxDataValue): TNyxDataValue;
var
  LOffset: Integer;
  LLimit: Integer;
  LIndex: Integer;
  LCount: Integer;
  LName: TNyxText;
  LReference: TNyxPresentationRef;
  LItems: array of TNyxDataValue;
begin
  NyxAgentFields(AArguments, '|name|offset|limit|');
  LOffset := IntegerArgument(AArguments, 'offset', 0, 0, NyxMaximumPresentations);
  LLimit := IntegerArgument(AArguments, 'limit', 8, 1, 16);

  if NyxAgentHas(AArguments, 'name') then
  begin

    if NyxAgentHas(AArguments, 'offset') or NyxAgentHas(AArguments, 'limit') then
    begin
      raise ENyxModel.Create('Inspect one exact presentation name or a bounded page');
    end;
    LName := AArguments.Field('name').AsText;
    LReference := NyxPresentation(LName);
    Exit(NyxObject([NyxField('revision', NyxData(FRevision)),
      NyxField('definition', NyxPresentationDefinition(LReference,
        FSession.Document.Presentations.Definition(LReference)))]));
  end;
  SetLength(LItems, LLimit);
  LCount := 0;
  for LIndex := LOffset to FSession.Document.Presentations.Count - 1 do
  begin

    if LCount = LLimit then
    begin
      Break;
    end;
    LReference := FSession.Document.Presentations.Reference(LIndex);
    LItems[LCount] := NyxPresentationDefinition(LReference,
      FSession.Document.Presentations.Definition(LReference));
    Inc(LCount);
  end;
  SetLength(LItems, LCount);
  Result := NyxObject([NyxField('revision', NyxData(FRevision)),
    NyxField('total', NyxData(FSession.Document.Presentations.Count)),
    NyxField('offset', NyxData(LOffset)), NyxField('definitions', NyxArray(LItems)),
    NyxField('hasMore', NyxData(LOffset + LCount < FSession.Document.Presentations.Count))]);
end;

function TNyxAgentSession.Menus(const AArguments: TNyxDataValue): TNyxDataValue;
var
  LRow: TNyxNode;
  LAuthoredRow: TNyxNode;
  LContext: TNyxNode;
  LScope: TNyxText;
  LOffset: Integer;
  LLimit: Integer;
  LIndex: Integer;
  LCount: Integer;
  LTotalText: Integer;
  LTextOffset: Integer;
  LTextLimit: Integer;
  LReference: TNyxMenuRef;
  LDefinition: INyxMenuDefinition;
  LWire: TNyxDataValue;
  LPolicy: TNyxDataValue;
  LFields: array of TNyxDataField;
  LItems: array of TNyxDataValue;
  LText: TNyxText;
begin
  NyxAgentFields(AArguments, '|row|barScope|name|offset|limit|itemOffset|itemLimit|textOffset|textLimit|');
  LOffset := IntegerArgument(AArguments, 'offset', 0, 0, NyxMaximumMenuDefinitions);
  LLimit := IntegerArgument(AArguments, 'limit', 8, 1, 16);
  LTextOffset := IntegerArgument(AArguments, 'textOffset', 0, 0, 1000000);
  LTextLimit := IntegerArgument(AArguments, 'textLimit', 256, 1, 1024);

  if NyxAgentHas(AArguments, 'row') then
  begin

    if NyxAgentHas(AArguments, 'name') or NyxAgentHas(AArguments, 'offset') or
      NyxAgentHas(AArguments, 'limit') then
    begin
      raise ENyxModel.Create('Inspect one exact row group or one menu/summary page');
    end;
    LAuthoredRow := FSession.Document.Find(AArguments.Field('row').AsText);

    if LAuthoredRow = nil then
    begin
      raise ENyxModel.Create('Menu bar query requires an exact authored row ID');
    end;
    LScope := TextArgument(AArguments, 'barScope', 'effective');

    if (LScope <> 'local') and (LScope <> 'effective') then
    begin
      raise ENyxModel.Create('Menu bar scope must be local or effective');
    end;
    LRow := LAuthoredRow;
    LContext := nil;
    try

      if LScope = 'effective' then
      begin
        LContext := RealizeNyxContext(FSession.Document, LAuthoredRow, LRow);
      end;

      if (LRow = nil) or (LRow.MenuBar = nil) then
      begin
        Exit(NyxObject([NyxField('revision', NyxData(FRevision)),
          NyxField('row', NyxData(LAuthoredRow.ID)), NyxField('barScope', NyxData(LScope)),
          NyxField('localDeclared', NyxData(LAuthoredRow.HasMenuBar)),
          NyxField('configured', NyxData(False))]));
      end;
      LWire := LRow.MenuBar.ToData;
      LPolicy := LWire.Field('options');
      SetLength(LFields, LPolicy.Count);
      for LIndex := 0 to LPolicy.Count - 1 do
      begin
        LFields[LIndex] := NyxField(LPolicy.Key(LIndex), LPolicy.Field(LPolicy.Key(LIndex)));

        if LFields[LIndex].Name = 'label' then
        begin
          LFields[LIndex].Value := NyxData(TextSpan(LPolicy.Field('label').AsText,
            LTextOffset, LTextLimit, LTotalText));
        end;
      end;
      LOffset := IntegerArgument(AArguments, 'itemOffset', 0, 0, 64);
      LLimit := IntegerArgument(AArguments, 'itemLimit', 8, 1, 16);
      SetLength(LItems, LLimit);
      LCount := 0;
      for LIndex := LOffset to LRow.MenuBar.Count - 1 do
      begin

        if LCount >= LLimit then
        begin
          Break;
        end;
        LItems[LCount] := LWire.Field('headings').Item(LIndex);
        Inc(LCount);
      end;
      SetLength(LItems, LCount);
      Exit(NyxObject([NyxField('revision', NyxData(FRevision)),
        NyxField('row', NyxData(LAuthoredRow.ID)), NyxField('barScope', NyxData(LScope)),
        NyxField('localDeclared', NyxData(LAuthoredRow.HasMenuBar)),
        NyxField('configured', NyxData(True)), NyxField('options', NyxObject(LFields)),
        NyxField('labelTotalScalars', NyxData(LTotalText)),
        NyxField('textOffset', NyxData(LTextOffset)),
        NyxField('headings', NyxArray(LItems)), NyxField('total', NyxData(LRow.MenuBar.Count)),
        NyxField('offset', NyxData(LOffset)),
        NyxField('hasMore', NyxData(LOffset + LCount < LRow.MenuBar.Count))]));
    finally
      LContext.Free;
    end;
  end;

  if NyxAgentHas(AArguments, 'barScope') then
  begin
    raise ENyxModel.Create('Menu bar scope requires an exact row query');
  end;

  if NyxAgentHas(AArguments, 'name') then
  begin

    if NyxAgentHas(AArguments, 'offset') or NyxAgentHas(AArguments, 'limit') then
    begin
      raise ENyxModel.Create('Inspect one exact menu name or a bounded summary page');
    end;
    LReference := NyxMenuRef(AArguments.Field('name').AsText);
    LDefinition := FSession.Document.Menus.Definition(LReference);
    LWire := LDefinition.ToData;
    LPolicy := LWire.Field('options');
    SetLength(LFields, LPolicy.Count);
    for LIndex := 0 to LPolicy.Count - 1 do
    begin
      LFields[LIndex] := NyxField(LPolicy.Key(LIndex), LPolicy.Field(LPolicy.Key(LIndex)));

      if LFields[LIndex].Name = 'title' then
      begin
        LFields[LIndex].Value := NyxData(TextSpan(LPolicy.Field('title').AsText,
          LTextOffset, LTextLimit, LTotalText));
      end;
    end;
    LOffset := IntegerArgument(AArguments, 'itemOffset', 0, 0, NyxMaximumMenuItems);
    LLimit := IntegerArgument(AArguments, 'itemLimit', 8, 1, 16);
    SetLength(LItems, LLimit);
    LCount := 0;
    for LIndex := LOffset to LDefinition.Count - 1 do
    begin

      if LCount = LLimit then
      begin
        Break;
      end;
      LItems[LCount] := LWire.Field('items').Item(LIndex);
      Inc(LCount);
    end;
    SetLength(LItems, LCount);
    Exit(NyxObject([NyxField('revision', NyxData(FRevision)),
      NyxField('name', NyxData(LReference.Name)), NyxField('root', LWire.Field('root')),
      NyxField('options', NyxObject(LFields)), NyxField('textOffset', NyxData(LTextOffset)),
      NyxField('totalTitleScalars', NyxData(LTotalText)),
      NyxField('titleTruncated', NyxData((LTextOffset > 0) or
        (LTotalText > LTextOffset + LTextLimit))),
      NyxField('totalItems', NyxData(LDefinition.Count)), NyxField('itemOffset', NyxData(LOffset)),
      NyxField('items', NyxArray(LItems)),
      NyxField('hasMore', NyxData(LOffset + LCount < LDefinition.Count))]));
  end;

  if NyxAgentHas(AArguments, 'itemOffset') or NyxAgentHas(AArguments, 'itemLimit') then
  begin
    raise ENyxModel.Create('Menu entry pages require one exact menu name');
  end;
  SetLength(LItems, LLimit);
  LCount := 0;
  for LIndex := LOffset to FSession.Document.Menus.Count - 1 do
  begin

    if LCount = LLimit then
    begin
      Break;
    end;
    LReference := FSession.Document.Menus.Reference(LIndex);
    LDefinition := FSession.Document.Menus.Definition(LReference);
    LText := TextSpan(LDefinition.Options.Placement.Title,
      LTextOffset, LTextLimit, LTotalText);
    LItems[LCount] := NyxObject([NyxField('name', NyxData(LReference.Name)),
      NyxField('root', NyxObject([NyxField('kind', NyxData(NyxRootKindName(LDefinition.Root.Kind))),
        NyxField('name', NyxData(LDefinition.Root.Name))])),
      NyxField('title', NyxData(LText)), NyxField('textOffset', NyxData(LTextOffset)),
      NyxField('totalTitleScalars', NyxData(LTotalText)),
      NyxField('titleTruncated', NyxData((LTextOffset > 0) or
        (LTotalText > LTextOffset + LTextLimit))),
      NyxField('totalItems', NyxData(LDefinition.Count))]);
    Inc(LCount);
  end;
  SetLength(LItems, LCount);
  Result := NyxObject([NyxField('revision', NyxData(FRevision)),
    NyxField('total', NyxData(FSession.Document.Menus.Count)), NyxField('offset', NyxData(LOffset)),
    NyxField('definitions', NyxArray(LItems)),
    NyxField('hasMore', NyxData(LOffset + LCount < FSession.Document.Menus.Count))]);
end;

function TNyxAgentSession.Diagnostics(const AArguments: TNyxDataValue): TNyxDataValue;
var
  LItems: array of TNyxDataValue;
  LIndex: Integer;
  LOffset: Integer;
  LLimit: Integer;
  LCount: Integer;
  LTotal: Integer;
  LItem: TNyxCompilerDiagnostic;
  LCurrent: Boolean;
  LOrder: TNyxCompilerDiagnosticIndices;
begin
  NyxAgentFields(AArguments, '|offset|limit|');
  LOffset := IntegerArgument(AArguments, 'offset', 0, 0, 100000);
  LLimit := IntegerArgument(AArguments, 'limit', 20, 1, 50);
  LTotal := 0;
  LCurrent := False;

  if FReport <> nil then
  begin
    LTotal := FReport.Count;
    LCurrent := (FReport.Source = FSession.Source) and (FSession.DraftSource = FSession.Source);
  end;
  SetLength(LItems, LLimit);
  LOrder := NyxCompilerDiagnosticOrder(FReport);
  LCount := 0;
  for LIndex := LOffset to LTotal - 1 do
  begin

    if LCount = LLimit then
    begin
      Break;
    end;
    LItem := FReport.Item(LOrder[LIndex]);
    LItems[LCount] := NyxObject([
      NyxField('file', NyxData(LItem.FileName)),
      NyxField('severity', NyxData(Ord(LItem.Severity))),
      NyxField('message', NyxData(CaptionText(LItem.Message, 1024))),
      NyxField('line', NyxData(LItem.SourceLine)),
      NyxField('column', NyxData(LItem.SourceColumn)),
      NyxField('navigable', NyxData(LCurrent and LItem.Navigable))]);
    Inc(LCount);
  end;
  SetLength(LItems, LCount);
  Result := NyxObject([NyxField('revision', NyxData(FRevision)),
    NyxField('total', NyxData(LTotal)), NyxField('offset', NyxData(LOffset)),
    NyxField('currentSource', NyxData(LCurrent)), NyxField('order', NyxData('severity')),
    NyxField('items', NyxArray(LItems))]);
end;

function TNyxAgentSession.SourceLines(const AArguments: TNyxDataValue): TNyxDataValue;
var
  LLines: TNyxStrings;
  LItems: array of TNyxDataValue;
  LStart: Integer;
  LCount: Integer;
  LIndex: Integer;
begin
  NyxAgentFields(AArguments, '|line|count|');
  LStart := IntegerArgument(AArguments, 'line', 1, 1, 100000);
  LCount := IntegerArgument(AArguments, 'count', 20, 1, 80);
  LLines := TNyxStrings.Create;
  try
    LLines.Text := FSession.Source;

    if LStart > LLines.Count then
    begin
      raise ENyxModel.Create('Source line does not exist');
    end;

    if LCount > LLines.Count - LStart + 1 then
    begin
      LCount := LLines.Count - LStart + 1;
    end;
    SetLength(LItems, LCount);
    for LIndex := 0 to LCount - 1 do
    begin
      LItems[LIndex] := NyxData(LLines[LStart - 1 + LIndex]);
    end;
    Result := NyxObject([NyxField('revision', NyxData(FRevision)),
      NyxField('line', NyxData(LStart)), NyxField('totalLines', NyxData(LLines.Count)),
      NyxField('lines', NyxArray(LItems))]);
  finally
    LLines.Free;
  end;
end;

function WithResults(const ASummary, AResults: TNyxDataValue;
  const AMember: TNyxText; ABudgetCheck: Boolean = False): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin
  SetLength(LFields, ASummary.Count + 1);
  for LIndex := 0 to ASummary.Count - 1 do
  begin
    LFields[LIndex] := NyxField(ASummary.Key(LIndex), ASummary.Field(ASummary.Key(LIndex)));

    if ABudgetCheck then
    begin
      { Reserve the largest serialized post-publication revision/history values
        before committing. A successful edit cannot outgrow this preflight. }

      if LFields[LIndex].Name = 'revision' then
      begin
        LFields[LIndex].Value := NyxData(High(Integer));
      end
      else if (LFields[LIndex].Name = 'canUndo') or (LFields[LIndex].Name = 'canRedo') then
      begin
        LFields[LIndex].Value := NyxData(False);
      end;
    end;
  end;
  { High avoids a pas2js record-method-as-array-index ambiguity. }
  LFields[High(LFields)] := NyxField(AMember, AResults);
  Result := NyxObject(LFields);
end;

function TNyxAgentSession.EditCallbacks(const AArguments: TNyxDataValue;
  const AActor: TNyxText; AApply: Boolean): TNyxDataValue;
var
  LPatch: INyxCallbackPatch;
  LPair: TNyxProjectPair;
  LResults: TNyxCallbackEditResults;
  LCallbacks: TNyxDataValue;
  LChanges: TNyxDataValue;
  LReview: TNyxDataValue;
  LReviewID: TNyxText;
  LIndex: Integer;
  LReviewIndex: Integer;
  LRemoves: Boolean;
begin
  NyxAgentFields(AArguments, '|expectedRevision|operationId|mode|changes|reviewID|');
  RequireRevision(AArguments);
  LChanges := AArguments.Field('changes');
  LPatch := ReadNyxCallbackPatch(LChanges);
  LRemoves := False;
  for LIndex := 0 to LChanges.Count - 1 do
  begin
    LRemoves := LRemoves or (LChanges.Item(LIndex).Field('op').AsText = 'remove');
  end;
  LReviewIndex := -1;

  if AApply and LRemoves then
  begin
    LReviewID := TextArgument(AArguments, 'reviewID');
    for LIndex := 0 to High(FCallbackReviews) do
    begin
      LReview := FCallbackReviews[LIndex];

      if (LReview.Field('id').AsText = LReviewID) and
        (LReview.Field('actor').AsText = AActor) and
        (LReview.Field('revision').AsInteger = FRevision) and
        (LReview.Field('changes').ToJSON = LChanges.ToJSON) then
      begin
        LReviewIndex := LIndex;
        Break;
      end;
    end;

    if LReviewIndex < 0 then
    begin
      raise ENyxModel.Create('Removal requires a current reviewID for this actor and exact changes; call nyx_callbacks in review mode first');
    end;
  end
  else if NyxAgentHas(AArguments, 'reviewID') then
  begin
    raise ENyxModel.Create('reviewID applies only to a removal batch in apply mode');
  end;

  if not AApply then
  begin

    if not LRemoves or NyxAgentHas(AArguments, 'operationId') then
    begin
      raise ENyxModel.Create('Review requires a removal batch and has no mutation operationId');
    end;

    if FCallbackReviewSerial = High(Integer) then
    begin
      raise ENyxModel.Create('Callback review budget exhausted');
    end;
  end;

  { All commands run on a detached session, including code generation and
    exact event admission. Failure preserves live pair, draft, selection and
    history. Size refusal also precedes the sole live AdoptProject operation. }
  LPair := LPatch.Candidate(FSession, LResults);
  LCallbacks := EncodeNyxCallbackResults(LResults);
  BoundContext(WithResults(Summary, LCallbacks, 'callbacks', True));

  if AApply then
  begin
    FSession.AdoptProject(LPair);

    if LReviewIndex >= 0 then
    begin
      for LIndex := LReviewIndex + 1 to High(FCallbackReviews) do
      begin
        FCallbackReviews[LIndex - 1] := FCallbackReviews[LIndex];
      end;
      SetLength(FCallbackReviews, Length(FCallbackReviews) - 1);
    end;
    Exit(LCallbacks);
  end;

  LReviewID := 'callback-review-' + IntToStr(FCallbackReviewSerial + 1);
  Result := NyxObject([NyxField('revision', NyxData(FRevision)),
    NyxField('reviewID', NyxData(LReviewID)), NyxField('callbacks', LCallbacks)]);
  BoundContext(Result);
  Inc(FCallbackReviewSerial);

  if Length(FCallbackReviews) = 16 then
  begin
    for LIndex := 1 to High(FCallbackReviews) do
    begin
      FCallbackReviews[LIndex - 1] := FCallbackReviews[LIndex];
    end;
    SetLength(FCallbackReviews, 15);
  end;
  SetLength(FCallbackReviews, Length(FCallbackReviews) + 1);
  FCallbackReviews[High(FCallbackReviews)] := NyxObject([
    NyxField('id', NyxData(LReviewID)), NyxField('actor', NyxData(AActor)),
    NyxField('revision', NyxData(FRevision)), NyxField('changes', LChanges)]);
end;

function TNyxAgentSession.RemoveRoots(const AArguments: TNyxDataValue;
  const AActor: TNyxText; AApply: Boolean): TNyxDataValue;
var
  LRoots: TNyxDataValue;
  LReview: TNyxDataValue;
  LRemoval: INyxRootRemoval;
  LPair: TNyxProjectPair;
  LReviewID: TNyxText;
  LIndex: Integer;
  LFound: Integer;
  LDocument: TNyxDocument;
  LSummary: TNyxDataValue;
  LFields: array of TNyxDataField;
  LView: TNyxText;
  LSelection: TNyxText;
begin
  NyxAgentFields(AArguments, '|mode|expectedRevision|operationId|roots|reviewID|');
  RequireRevision(AArguments);
  LRoots := AArguments.Field('roots');

  if AApply then
  begin
    LReviewID := TextArgument(AArguments, 'reviewID');
    LFound := -1;
    for LIndex := 0 to High(FRootReviews) do
    begin
      LReview := FRootReviews[LIndex];

      if (LReview.Field('id').AsText = LReviewID) and
        (LReview.Field('actor').AsText = AActor) and
        (LReview.Field('revision').AsInteger = FRevision) and
        (LReview.Field('roots').ToJSON = LRoots.ToJSON) then
      begin
        LFound := LIndex;
        Break;
      end;
    end;

    if LFound < 0 then
    begin
      raise ENyxModel.Create('Root removal requires a current reviewID for this actor and exact roots');
    end;
    LRemoval := FRootRemovals[LFound];
    LPair := LRemoval.Candidate(FSession.ProjectSnapshot);
    Result := LRemoval.Inspect;
    { Response size, dependencies, draft and exact paired text are admitted
      before the only publication. Consume the review only after success. }
    { The surviving view/selection can have longer names than the removed ones.
      Preflight their actual fallback, rather than assume the old summary is an
      upper bound. Match ordinary AdoptProject's page/component/empty order. }
    LDocument := TNyxCodec.Decode(LPair.Design);
    try
      LView := FSession.ActiveViewID;
      LSelection := FSession.SelectedID;

      if LDocument.Find(LView) = nil then
      begin
        LView := '';

        if LDocument.Count > 0 then
        begin
          LView := LDocument.Pages[0].ID;
        end
        else if LDocument.ComponentCount > 0 then
        begin
          LView := LDocument.Components[0].ID;
        end;
      end;

      if LDocument.Find(LSelection) = nil then
      begin
        LSelection := LView;
      end;
      LSummary := Summary;
      SetLength(LFields, LSummary.Count);
      for LIndex := 0 to LSummary.Count - 1 do
      begin
        LFields[LIndex] := NyxField(LSummary.Key(LIndex), LSummary.Field(LSummary.Key(LIndex)));

        if LFields[LIndex].Name = 'view' then
        begin
          LFields[LIndex].Value := NyxData(LView);
        end
        else if LFields[LIndex].Name = 'selection' then
        begin
          LFields[LIndex].Value := NyxData(LSelection);
        end;
      end;
      BoundContext(WithResults(NyxObject(LFields), Result, 'removedRoots', True));
    finally
      LDocument.Free;
    end;
    FSession.AdoptProject(LPair);
    for LIndex := LFound + 1 to High(FRootReviews) do
    begin
      FRootReviews[LIndex - 1] := FRootReviews[LIndex];
      FRootRemovals[LIndex - 1] := FRootRemovals[LIndex];
    end;
    SetLength(FRootReviews, Length(FRootReviews) - 1);
    SetLength(FRootRemovals, Length(FRootRemovals) - 1);
    Exit;
  end;

  if NyxAgentHas(AArguments, 'operationId') or NyxAgentHas(AArguments, 'reviewID') then
  begin
    raise ENyxModel.Create('Root review has no mutation operationId or prior reviewID');
  end;

  if FRootReviewSerial = High(Integer) then
  begin
    raise ENyxModel.Create('Root review budget exhausted');
  end;
  LRemoval := ReadNyxRootRemoval(FSession.ProjectSnapshot, LRoots);
  LReviewID := 'root-review-' + IntToStr(FRootReviewSerial + 1);
  Result := NyxObject([NyxField('revision', NyxData(FRevision)),
    NyxField('reviewID', NyxData(LReviewID)), NyxField('removal', LRemoval.Inspect)]);
  BoundContext(Result);
  Inc(FRootReviewSerial);

  if Length(FRootReviews) = 8 then
  begin
    for LIndex := 1 to High(FRootReviews) do
    begin
      FRootReviews[LIndex - 1] := FRootReviews[LIndex];
      FRootRemovals[LIndex - 1] := FRootRemovals[LIndex];
    end;
    SetLength(FRootReviews, 7);
    SetLength(FRootRemovals, 7);
  end;
  LIndex := Length(FRootReviews);
  SetLength(FRootReviews, LIndex + 1);
  SetLength(FRootRemovals, LIndex + 1);
  FRootReviews[LIndex] := NyxObject([NyxField('id', NyxData(LReviewID)),
    NyxField('actor', NyxData(AActor)), NyxField('revision', NyxData(FRevision)),
    NyxField('roots', LRoots)]);
  FRootRemovals[LIndex] := LRemoval;
end;

function TNyxAgentSession.HandlerSource(const AArguments: TNyxDataValue;
  AApply: Boolean): TNyxDataValue;
var
  LPatch: INyxHandlerPatch;
  LPair: TNyxProjectPair;
  LResults: TNyxHandlerEditResults;
  LHandler: TNyxHandlerSource;
  LOffset: Integer;
  LCount: Integer;
  LTotal: Integer;
  LSignatureTotal: Integer;
  LSignature: TNyxText;
  LText: TNyxText;
begin

  if AApply then
  begin
    NyxAgentFields(AArguments, '|mode|expectedRevision|operationId|changes|');
    RequireRevision(AArguments);
    LPatch := ReadNyxHandlerPatch(AArguments.Field('changes'));
    LPair := LPatch.Candidate(FSession, LResults);
    Result := EncodeNyxHandlerResults(LResults);
    { Admit the final response before the only active-session publication. The
      detached patch preserves source/design/selection/history on every refusal. }
    BoundContext(WithResults(Summary, Result, 'handlers', True));
    FSession.AdoptProject(LPair);
    Exit;
  end;
  NyxAgentFields(AArguments, '|mode|handler|offset|count|');
  LHandler := ReadNyxHandlerSource(FSession.Source, NyxHandler(TextArgument(AArguments, 'handler')));
  LOffset := IntegerArgument(AArguments, 'offset', 0, 0, 4 * 1024 * 1024);
  LCount := IntegerArgument(AArguments, 'count', 2048, 1, 4096);
  LText := TextSpan(LHandler.Code, LOffset, LCount, LTotal);

  if LOffset > LTotal then
  begin
    raise ENyxModel.Create('Callback text offset is beyond the accepted implementation');
  end;
  LSignature := TextSpan(LHandler.Signature, 0, 1024, LSignatureTotal);
  Result := NyxObject([
    NyxField('revision', NyxData(FRevision)), NyxField('handler', NyxData(LHandler.Handler.Name)),
    NyxField('line', NyxData(LHandler.Line)), NyxField('signature', NyxData(LSignature)),
    NyxField('signatureCharacters', NyxData(LSignatureTotal)),
    NyxField('offset', NyxData(LOffset)), NyxField('total', NyxData(LTotal)),
    NyxField('nextOffset', NyxData(Min(LOffset + LCount, LTotal))),
    NyxField('text', NyxData(LText)),
    NyxField('pendingDraft', NyxData(FSession.DraftSource <> FSession.Source))]);
end;

function TNyxAgentSession.Views(const AArguments: TNyxDataValue;
  AApply: Boolean): TNyxDataValue;
var
  LPatch: INyxViewsPatch;
  LPair: TNyxProjectPair;
  LPrefix: TNyxText;
  LBuilder: TNyxText;
  LSuffix: TNyxText;
  LText: TNyxText;
  LOffset: Integer;
  LCount: Integer;
  LTotal: Integer;
  LLine: Integer;
  LIndex: Integer;
begin

  if AApply then
  begin
    NyxAgentFields(AArguments, '|mode|expectedRevision|operationId|expected|builder|');
    RequireRevision(AArguments);
    LPatch := NyxViewsPatch(TextArgument(AArguments, 'expected'),
      TextArgument(AArguments, 'builder'));
    LPair := LPatch.Candidate(FSession.ProjectSnapshot);
    { Bound the receipt before the sole accepted-session publication. Returned
      text is deliberately absent: nyx_source/views provide focused inspection. }
    Result := NyxObject([NyxField('replaced', NyxData(True))]);
    BoundContext(WithResults(Summary, Result, 'views', True));
    FSession.AdoptProject(LPair);
    Exit;
  end;
  NyxAgentFields(AArguments, '|mode|offset|count|');
  SplitNyxSourceFrame(FSession.Source, LPrefix, LBuilder, LSuffix);
  LOffset := IntegerArgument(AArguments, 'offset', 0, 0, 4 * 1024 * 1024);
  LCount := IntegerArgument(AArguments, 'count', 2048, 1, 4096);
  LText := TextSpan(LBuilder, LOffset, LCount, LTotal);

  if LOffset > LTotal then
  begin
    raise ENyxModel.Create('Views text offset is beyond the accepted builder');
  end;
  LLine := 1;
  for LIndex := 1 to Length(LPrefix) do
  begin

    if LPrefix[LIndex] = #10 then
    begin
      Inc(LLine);
    end;
  end;
  Result := NyxObject([
    NyxField('revision', NyxData(FRevision)), NyxField('line', NyxData(LLine)),
    NyxField('offset', NyxData(LOffset)), NyxField('total', NyxData(LTotal)),
    NyxField('nextOffset', NyxData(Min(LOffset + LCount, LTotal))),
    NyxField('text', NyxData(LText)),
    NyxField('maximumEditCharacters', NyxData(NyxMaximumViewsCharacters)),
    NyxField('pendingDraft', NyxData(FSession.SourceDraftPending))]);
end;

function TNyxAgentSession.SourceUnit(const AArguments: TNyxDataValue;
  AApply: Boolean): TNyxDataValue;
var
  LChanges: TNyxDataValue;
  LChange: TNyxDataValue;
  LEdits: array of TNyxSourceEdit;
  LIndex: Integer;
  LPair: TNyxProjectPair;
  LOffset: Integer;
  LCount: Integer;
  LTotal: Integer;
  LText: TNyxText;
  LSource: TNyxText;
  LLine: Integer;
  LCursor: Integer;
  LScalar: Integer;
  LRequestedLine: Integer;
  LScannedOffset: Integer;
begin

  if AApply then
  begin
    NyxAgentFields(AArguments, '|mode|expectedRevision|operationId|changes|');
    RequireRevision(AArguments);
    LChanges := AArguments.Field('changes');

    if (LChanges.Kind <> ndArray) or (LChanges.Count < 1) or
      (LChanges.Count > NyxMaximumSourceEdits) then
    begin
      raise ENyxModel.Create('Source edits require 1..16 changes');
    end;
    SetLength(LEdits, LChanges.Count);
    for LIndex := 0 to LChanges.Count - 1 do
    begin
      LChange := LChanges.Item(LIndex);
      NyxAgentFields(LChange, '|offset|expected|replacement|');

      if not NyxAgentHas(LChange, 'offset') or
        not NyxAgentHas(LChange, 'expected') or
        not NyxAgentHas(LChange, 'replacement') then
      begin
        raise ENyxModel.Create('Source changes require offset, expected and replacement');
      end;
      LEdits[LIndex] := NyxSourceEdit(IntegerArgument(LChange, 'offset', 0,
        0, NyxMaximumSourceCharacters), TextArgument(LChange, 'expected'),
        TextArgument(LChange, 'replacement'));
    end;
    LPair := NyxSourcePatch(LEdits).Candidate(FSession.ProjectSnapshot);
    Result := NyxObject([NyxField('changes', NyxData(Length(LEdits)))]);
    { Preflight the bounded receipt before the sole accepted-session publication. }
    BoundContext(WithResults(Summary, Result, 'sourceEdit', True));
    FSession.AdoptProject(LPair);
    Exit;
  end;
  NyxAgentFields(AArguments, '|mode|expectedRevision|offset|line|count|');

  if NyxAgentHas(AArguments, 'offset') and NyxAgentHas(AArguments, 'line') then
  begin
    raise ENyxModel.Create('Choose source scalar offset or source line, not both');
  end;

  if NyxAgentHas(AArguments, 'expectedRevision') then
  begin
    RequireRevision(AArguments);
  end;
  LOffset := IntegerArgument(AArguments, 'offset', 0, 0, NyxMaximumSourceCharacters);
  LCount := IntegerArgument(AArguments, 'count', 2048, 1, 4096);
  LSource := FSession.Source;
  LLine := 1;
  LCursor := 1;
  LRequestedLine := 0;
  LScannedOffset := 0;

  if NyxAgentHas(AArguments, 'line') then
  begin
    LRequestedLine := IntegerArgument(AArguments, 'line', 1, 1, 1000000);
    LOffset := 0;
  end;
  { Map compiler/routine source lines to scalar offsets inside the server.
    Callers need only the requested window, not every preceding source page. }
  while LCursor <= Length(LSource) do
  begin

    if ((LRequestedLine > 0) and (LLine = LRequestedLine)) or
      ((LRequestedLine = 0) and (LScannedOffset = LOffset)) then
    begin
      Break;
    end;

    if not NyxNextScalar(LSource, LCursor, LScalar) then
    begin
      raise ENyxModel.Create('Accepted source contains invalid Unicode');
    end;
    Inc(LScannedOffset);

    if LScalar = 10 then
    begin
      Inc(LLine);
    end;
  end;

  if LRequestedLine > 0 then
  begin

    if LLine <> LRequestedLine then
    begin
      raise ENyxModel.Create('Source line does not exist');
    end;
    LOffset := LScannedOffset;
  end;
  LText := TextSpan(LSource, LOffset, LCount, LTotal);

  if LOffset > LTotal then
  begin
    raise ENyxModel.Create('Source unit offset is outside the accepted source');
  end;

  if LCount > LTotal - LOffset then
  begin
    LCount := LTotal - LOffset;
  end;
  Result := NyxObject([NyxField('revision', NyxData(FRevision)),
    NyxField('offset', NyxData(LOffset)), NyxField('line', NyxData(LLine)),
    NyxField('total', NyxData(LTotal)),
    NyxField('nextOffset', NyxData(LOffset + LCount)),
    NyxField('text', NyxData(LText)),
    NyxField('maximumChanges', NyxData(NyxMaximumSourceEdits)),
    NyxField('maximumEditCharacters', NyxData(NyxMaximumSourceEditCharacters)),
    NyxField('pendingDraft', NyxData(FSession.SourceDraftPending))]);
end;

function TNyxAgentSession.Imports(const AArguments: TNyxDataValue;
  AApply: Boolean): TNyxDataValue;
var
  LPatch: INyxImportPatch;
  LPair: TNyxProjectPair;
  LClause: TNyxImportClause;
  LSection: TNyxImportSection;
  LItems: array of TNyxDataValue;
  LOffset: Integer;
  LLimit: Integer;
  LIndex: Integer;
begin

  if AApply then
  begin
    NyxAgentFields(AArguments, '|mode|expectedRevision|operationId|changes|');
    RequireRevision(AArguments);
    LPatch := ReadNyxImportPatch(AArguments.Field('changes'));
    LPair := LPatch.Candidate(FSession.ProjectSnapshot);
    Result := NyxObject([NyxField('changes', NyxData(LPatch.Count))]);
    BoundContext(WithResults(Summary, Result, 'imports', True));
    FSession.AdoptProject(LPair);
    Exit;
  end;
  NyxAgentFields(AArguments, '|mode|section|offset|limit|');
  LSection := ReadNyxImportSection(AArguments.Field('section').AsText);
  LClause := ReadNyxImports(FSession.Source, LSection);
  LOffset := IntegerArgument(AArguments, 'offset', 0, 0, 256);
  LLimit := IntegerArgument(AArguments, 'limit', 20, 1, 50);
  SetLength(LItems, Max(0, Min(LLimit, LClause.Count - LOffset)));
  for LIndex := 0 to High(LItems) do
  begin
    LItems[LIndex] := NyxObject([
      NyxField('unit', NyxData(LClause.UnitAt(LOffset + LIndex).Name)),
      NyxField('line', NyxData(LClause.LineAt(LOffset + LIndex)))]);
  end;
  Result := NyxObject([NyxField('revision', NyxData(FRevision)),
    NyxField('section', NyxData(NyxImportSectionName(LSection))),
    NyxField('offset', NyxData(LOffset)), NyxField('total', NyxData(LClause.Count)),
    NyxField('nextOffset', NyxData(Min(LOffset + Length(LItems), LClause.Count))),
    NyxField('units', NyxArray(LItems)),
    NyxField('pendingDraft', NyxData(FSession.DraftSource <> FSession.Source))]);
end;

function TNyxAgentSession.Routines(const AArguments: TNyxDataValue;
  AApply: Boolean): TNyxDataValue;
const
  CKindNames: array[TNyxRoutineKind] of TNyxText =
    ('procedure', 'function', 'constructor', 'destructor');
var
  LPatch: INyxRoutinePatch;
  LPair: TNyxProjectPair;
  LResults: TNyxRoutineEditResults;
  LCatalog: TNyxRoutineCatalog;
  LRoutine: TNyxRoutineSource;
  LItems: array of TNyxDataValue;
  LOffset: Integer;
  LLimit: Integer;
  LIndex: Integer;
  LTotal: Integer;
  LSignatureTotal: Integer;
  LSignature: TNyxText;
  LText: TNyxText;
begin

  if AApply then
  begin
    NyxAgentFields(AArguments, '|mode|expectedRevision|operationId|changes|');
    RequireRevision(AArguments);
    LPatch := ReadNyxRoutinePatch(AArguments.Field('changes'));
    LPair := LPatch.Candidate(FSession, LResults);
    Result := EncodeNyxRoutineResults(LResults);
    BoundContext(WithResults(Summary, Result, 'routines', True));
    FSession.AdoptProject(LPair);
    Exit;
  end;

  if AArguments.Field('mode').AsText = 'routine' then
  begin
    NyxAgentFields(AArguments, '|mode|routine|offset|count|');
    LRoutine := ReadNyxRoutineSource(FSession.Source,
      NyxRoutine(TextArgument(AArguments, 'routine')));
    LOffset := IntegerArgument(AArguments, 'offset', 0, 0, 4 * 1024 * 1024);
    LLimit := IntegerArgument(AArguments, 'count', 2048, 1, 4096);
    LText := TextSpan(LRoutine.Code, LOffset, LLimit, LTotal);

    if LOffset > LTotal then
    begin
      raise ENyxModel.Create('Routine offset is beyond the accepted implementation');
    end;
    LSignature := TextSpan(LRoutine.Signature, 0, 1024, LSignatureTotal);
    Result := NyxObject([
      NyxField('revision', NyxData(FRevision)), NyxField('routine', NyxData(LRoutine.Routine.Name)),
      NyxField('kind', NyxData(CKindNames[LRoutine.Kind])),
      NyxField('line', NyxData(LRoutine.Line)), NyxField('signature', NyxData(LSignature)),
      NyxField('signatureCharacters', NyxData(LSignatureTotal)),
      NyxField('editable', NyxData(LRoutine.Editable)), NyxField('reason', NyxData(LRoutine.Reason)),
      NyxField('offset', NyxData(LOffset)), NyxField('total', NyxData(LTotal)),
      NyxField('nextOffset', NyxData(Min(LOffset + LLimit, LTotal))),
      NyxField('text', NyxData(LText)),
      NyxField('pendingDraft', NyxData(FSession.DraftSource <> FSession.Source))]);
    Exit;
  end;
  NyxAgentFields(AArguments, '|mode|offset|limit|');
  LCatalog := ReadNyxRoutines(FSession.Source);
  LOffset := IntegerArgument(AArguments, 'offset', 0, 0, 4096);
  LLimit := IntegerArgument(AArguments, 'limit', 20, 1, 50);
  SetLength(LItems, Max(0, Min(LLimit, LCatalog.Count - LOffset)));
  for LIndex := 0 to High(LItems) do
  begin
    LRoutine := LCatalog.Item(LOffset + LIndex);
    LItems[LIndex] := NyxObject([
      NyxField('routine', NyxData(LRoutine.Routine.Name)),
      NyxField('kind', NyxData(CKindNames[LRoutine.Kind])),
      NyxField('line', NyxData(LRoutine.Line)), NyxField('editable', NyxData(LRoutine.Editable)),
      NyxField('reason', NyxData(LRoutine.Reason))]);
  end;
  Result := NyxObject([
    NyxField('revision', NyxData(FRevision)), NyxField('offset', NyxData(LOffset)),
    NyxField('total', NyxData(LCatalog.Count)),
    NyxField('nextOffset', NyxData(Min(LOffset + Length(LItems), LCatalog.Count))),
    NyxField('routines', NyxArray(LItems)),
    NyxField('pendingDraft', NyxData(FSession.DraftSource <> FSession.Source))]);
end;

function TNyxAgentSession.Declarations(const AArguments: TNyxDataValue;
  AApply: Boolean): TNyxDataValue;
var
  LPatch: INyxDeclarationPatch;
  LPair: TNyxProjectPair;
  LSite: TNyxRoutineDeclarationSource;
  LReference: TNyxRoutineRef;
  LOffset: Integer;
  LCount: Integer;
  LTotal: Integer;
  LText: TNyxText;
  LVisibility: TNyxText;
  LPart: TNyxDeclarationPart;
  LPartName: TNyxText;
begin

  if AApply then
  begin
    NyxAgentFields(AArguments, '|mode|expectedRevision|operationId|changes|');
    RequireRevision(AArguments);
    LPatch := ReadNyxDeclarationPatch(AArguments.Field('changes'));
    LPair := LPatch.Candidate(FSession.ProjectSnapshot);
    Result := NyxObject([NyxField('changes', NyxData(LPatch.Count))]);
    BoundContext(WithResults(Summary, Result, 'declarations', True));
    FSession.AdoptProject(LPair);
    Exit;
  end;
  NyxAgentFields(AArguments, '|mode|routine|part|offset|count|');
  LPart := dspInterface;
  LPartName := 'interface';

  if NyxAgentHas(AArguments, 'part') then
  begin
    LPartName := AArguments.Field('part').AsText;

    if LPartName = 'implementation' then
    begin
      LPart := dspImplementation;
    end
    else if LPartName <> 'interface' then
    begin
      raise ENyxModel.Create('Declaration part must be interface or implementation');
    end;
  end;
  LReference := NyxRoutine(AArguments.Field('routine').AsText);
  LSite := ReadNyxRoutineDeclaration(FSession.Source, LReference);
  LOffset := IntegerArgument(AArguments, 'offset', 0, 0, 4 * 1024 * 1024);
  LCount := IntegerArgument(AArguments, 'count', 2048, 1, 4096);
  LText := TextSpan(LSite.Text(LPart), LOffset, LCount, LTotal);

  if LOffset > LTotal then
  begin
    raise ENyxModel.Create('Declaration offset exceeds its exact accepted counterpart');
  end;
  LVisibility := 'implementation';

  if LSite.Visibility = rvInterface then
  begin
    LVisibility := 'interface';
  end;
  Result := NyxObject([
    NyxField('revision', NyxData(FRevision)), NyxField('routine', NyxData(LReference.Name)),
    NyxField('visibility', NyxData(LVisibility)), NyxField('part', NyxData(LPartName)),
    NyxField('line', NyxData(LSite.SourceLine(LPart))),
    NyxField('offset', NyxData(LOffset)), NyxField('total', NyxData(LTotal)),
    NyxField('nextOffset', NyxData(Min(LOffset + LCount, LTotal))), NyxField('text', NyxData(LText)),
    NyxField('pendingDraft', NyxData(FSession.DraftSource <> FSession.Source))]);
end;

{$I nyx.studio.agents.state.inc}
{$I nyx.studio.agents.collections.inc}
{$I nyx.studio.agents.resources.inc}
{$I nyx.studio.agents.resourceruntimes.inc}
{$I nyx.studio.agents.project.inc}

function TNyxAgentSession.Call(const ATool, AActor: TNyxText;
  const AArguments: TNyxDataValue; const ARequestOwner: TNyxText): TNyxDataValue;
var
  LBefore: TNyxText;
  LOperationID: TNyxText;
  LRequest: TNyxText;
  LKey: TNyxText;
  LIndex: Integer;
  LNode: TNyxNode;
  LMutation: Boolean;
  LCallbackApply: Boolean;
  LCallbackResults: TNyxDataValue;
  LHandlerApply: Boolean;
  LHandlerResults: TNyxDataValue;
  LImportApply: Boolean;
  LImportResults: TNyxDataValue;
  LRoutineApply: Boolean;
  LRoutineResults: TNyxDataValue;
  LDeclarationApply: Boolean;
  LDeclarationResults: TNyxDataValue;
  LRootApply: Boolean;
  LRootResults: TNyxDataValue;
  LStateApply: Boolean;
  LStateResults: TNyxDataValue;
  LCollectionApply: Boolean;
  LCollectionResults: TNyxDataValue;
  LResourceApply: Boolean;
  LResourceResults: TNyxDataValue;
  LAuthority: TNyxText;
  LViewsApply: Boolean;
  LViewsResults: TNyxDataValue;
  LSourceApply: Boolean;
  LSourceResults: TNyxDataValue;
  LProjectMutation: Boolean;
  LProjectApply: Boolean;
  LProjectResults: TNyxDataValue;
  LTransaction: INyxProjectTransaction;
  LDesignPatch: INyxDesignPatch;
begin
  LAuthority := ARequestOwner;

  if LAuthority = '' then
  begin
    LAuthority := AActor;
  end;
  LCallbackApply := False;
  LCallbackResults := NyxNull;
  LHandlerApply := False;
  LHandlerResults := NyxNull;
  LImportApply := False;
  LImportResults := NyxNull;
  LRoutineApply := False;
  LRoutineResults := NyxNull;
  LDeclarationApply := False;
  LDeclarationResults := NyxNull;
  LRootApply := False;
  LRootResults := NyxNull;
  LStateApply := False;
  LStateResults := NyxNull;
  LCollectionApply := False;
  LCollectionResults := NyxNull;
  LResourceApply := False;
  LResourceResults := NyxNull;
  LViewsApply := False;
  LViewsResults := NyxNull;
  LSourceApply := False;
  LSourceResults := NyxNull;
  LProjectMutation := False;
  LProjectApply := False;
  LProjectResults := NyxNull;

  try

    if FPermission = apDisabled then
    begin
      raise ENyxModel.Create('Agent access is disabled in Studio');
    end;

    if ATool = 'nyx_callbacks' then
    begin
      LCallbackApply := TextArgument(AArguments, 'mode') = 'apply';
    end;

    if ATool = 'nyx_pascal' then
    begin
      LHandlerApply := TextArgument(AArguments, 'mode') = 'apply';
      LImportApply := TextArgument(AArguments, 'mode') = 'edit-imports';
      LRoutineApply := TextArgument(AArguments, 'mode') = 'edit-routines';
      LDeclarationApply := TextArgument(AArguments, 'mode') = 'edit-declarations';
      LViewsApply := TextArgument(AArguments, 'mode') = 'edit-views';
      LSourceApply := TextArgument(AArguments, 'mode') = 'edit-unit';
    end;
    if ATool = 'nyx_roots' then
    begin
      LRootApply := TextArgument(AArguments, 'mode') = 'apply';
    end;

    if ATool = 'nyx_state' then
    begin
      LStateApply := TextArgument(AArguments, 'mode') = 'apply';
    end;

    if ATool = 'nyx_collections' then
    begin
      LCollectionApply := TextArgument(AArguments, 'mode') = 'apply';
    end;
    if ATool = 'nyx_resources' then
    begin
      LResourceApply := TextArgument(AArguments, 'mode') = 'apply';
    end;
    if ATool = 'nyx_project' then
    begin
      LProjectApply := TextArgument(AArguments, 'mode') = 'apply';
      LProjectMutation := (TextArgument(AArguments, 'mode') = 'begin-import') or
        (TextArgument(AArguments, 'mode') = 'append-import') or
        (TextArgument(AArguments, 'mode') = 'review-import') or
        (TextArgument(AArguments, 'mode') = 'cancel-import') or
        (TextArgument(AArguments, 'mode') = 'apply');
    end;
    LMutation := (ATool = 'nyx_transaction') or (ATool = 'nyx_select') or
      (ATool = 'nyx_history') or LCallbackApply or LHandlerApply or LRootApply or
      LStateApply or LCollectionApply or LResourceApply or
      LImportApply or LRoutineApply or LDeclarationApply or LViewsApply or LSourceApply or
      LProjectMutation;

    if LMutation and (FPermission <> apEdit) then
    begin
      raise ENyxModel.Create('Agent edits require Allow edits in Studio');
    end;

    if LMutation then
    begin
      LOperationID := AArguments.Field('operationId').AsText;

      if (LOperationID = '') or (Length(LOperationID) > 120) then
      begin
        raise ENyxModel.Create('Mutation operationId must contain 1..120 characters');
      end;
      LKey := NyxObject([NyxField('actor', NyxData(LAuthority)),
        NyxField('id', NyxData(LOperationID))]).ToJSON;
      LRequest := NyxObject([NyxField('tool', NyxData(ATool)),
        NyxField('arguments', AArguments)]).ToJSON;
      for LIndex := 0 to High(FReceiptKeys) do
      begin

        if FReceiptKeys[LIndex] = LKey then
        begin

          if FReceiptRequests[LIndex] <> LRequest then
          begin
            raise ENyxModel.Create('operationId was already used with different arguments');
          end;
          Log(AActor, ATool, 'retry returned original receipt');
          Exit(FReceiptResults[LIndex]);
        end;
      end;
      RequireRevision(AArguments);
      LBefore := '';

      if not LProjectMutation or LProjectApply then
      begin
        LBefore := EncodeNyxProject(FSession.ProjectSnapshot);
      end;
    end;

    if ATool = 'nyx_session' then
    begin
      NyxAgentFields(AArguments, '|');
      Result := Summary;
    end
    else if ATool = 'nyx_outline' then
    begin
      Result := Outline(AArguments);
    end
    else if ATool = 'nyx_node' then
    begin
      Result := NodeDetails(AArguments);
    end
    else if ATool = 'nyx_components' then
    begin
      Result := Components(AArguments);
    end
    else if ATool = 'nyx_diagnostics' then
    begin
      Result := Diagnostics(AArguments);
    end
    else if ATool = 'nyx_source' then
    begin
      Result := SourceLines(AArguments);
    end
    else if ATool = 'nyx_presentations' then
    begin
      Result := Presentations(AArguments);
    end
    else if ATool = 'nyx_menus' then
    begin
      Result := Menus(AArguments);
    end
    else if ATool = 'nyx_tokens' then
    begin
      NyxAgentFields(AArguments, '|');
      Result := NyxObject([NyxField('revision', NyxData(FRevision)),
        NyxField('tokens', NyxDesignTokens(FSession.Document))]);
    end
    else if ATool = 'nyx_transaction' then
    begin
      NyxAgentFields(AArguments, '|expectedRevision|operationId|operations|');
      LTransaction := ReadNyxProjectTransaction(AArguments.Field('operations'));

      if LTransaction.TryDesignPatch(LDesignPatch) then
      begin
        FSession.ApplyPatch(LDesignPatch);
      end
      else
      begin
        { Compose on independent paired candidates; publish exactly once through
          the ordinary Studio history/source gate. Navigation stays authoritative. }
        FSession.AdoptProject(LTransaction.Candidate(FSession.ProjectSnapshot));
      end;
    end
    else if ATool = 'nyx_state' then
    begin
      Result := StateBindings(AArguments);
      LStateResults := Result;
    end
    else if ATool = 'nyx_collections' then
    begin
      Result := Collections(AArguments);
      LCollectionResults := Result;
    end
    else if ATool = 'nyx_resources' then
    begin
      Result := Resources(AArguments);
      LResourceResults := Result;
    end
    else if ATool = 'nyx_callbacks' then
    begin

      if not LCallbackApply and (TextArgument(AArguments, 'mode') <> 'review') then
      begin
        raise ENyxModel.Create('Callback mode must be review or apply');
      end;
      Result := EditCallbacks(AArguments, LAuthority, LCallbackApply);
      LCallbackResults := Result;
    end
    else if ATool = 'nyx_pascal' then
    begin

      if LSourceApply or (TextArgument(AArguments, 'mode') = 'unit') then
      begin
        Result := SourceUnit(AArguments, LSourceApply);
        LSourceResults := Result;
      end
      else if LViewsApply or (TextArgument(AArguments, 'mode') = 'views') then
      begin
        Result := Views(AArguments, LViewsApply);
        LViewsResults := Result;
      end
      else if LDeclarationApply or (TextArgument(AArguments, 'mode') = 'declaration') then
      begin
        Result := Declarations(AArguments, LDeclarationApply);
        LDeclarationResults := Result;
      end
      else if LRoutineApply or (TextArgument(AArguments, 'mode') = 'routines') or
        (TextArgument(AArguments, 'mode') = 'routine') then
      begin
        Result := Routines(AArguments, LRoutineApply);
        LRoutineResults := Result;
      end
      else if LImportApply or (TextArgument(AArguments, 'mode') = 'imports') then
      begin
        Result := Imports(AArguments, LImportApply);
        LImportResults := Result;
      end
      else
      begin

        if not LHandlerApply and (TextArgument(AArguments, 'mode') <> 'inspect') then
        begin
          raise ENyxModel.Create('Unknown Pascal mode; inspect discovery for the supported source operations');
        end;
        Result := HandlerSource(AArguments, LHandlerApply);
        LHandlerResults := Result;
      end;
    end
    else if ATool = 'nyx_project' then
    begin
      Result := ProjectFile(AArguments, LAuthority);
      LProjectResults := Result;
    end
    else if ATool = 'nyx_roots' then
    begin

      if not LRootApply and (TextArgument(AArguments, 'mode') <> 'review') then
      begin
        raise ENyxModel.Create('Root mode must be review or apply');
      end;
      Result := RemoveRoots(AArguments, LAuthority, LRootApply);
      LRootResults := Result;
    end
    else if ATool = 'nyx_select' then
    begin
      NyxAgentFields(AArguments, '|expectedRevision|operationId|id|activate|');
      LNode := FSession.Document.Find(AArguments.Field('id').AsText);

      if LNode = nil then
      begin
        raise ENyxModel.Create('Selection is missing');
      end;

      if NyxAgentHas(AArguments, 'activate') and AArguments.Field('activate').AsBoolean then
      begin

        if LNode.Parent <> nil then
        begin
          raise ENyxModel.Create('Activate requires a page or reusable root');
        end;
        FSession.Activate(LNode.ID);
      end
      else
      begin
        FSession.Select(LNode.ID);
      end;
      Changed;
    end
    else if ATool = 'nyx_history' then
    begin
      NyxAgentFields(AArguments, '|expectedRevision|operationId|direction|');

      { History captures the complete current buffer/base before traversal.
        Pending text remains available through the opposite command; ordinary
        design/source edits continue to require deliberate draft resolution. }

      if AArguments.Field('direction').AsText = 'undo' then
      begin
        FSession.Undo;
      end
      else if AArguments.Field('direction').AsText = 'redo' then
      begin
        FSession.Redo;
      end
      else
      begin
        raise ENyxModel.Create('History direction must be undo or redo');
      end;
    end
    else
    begin
      raise ENyxModel.Create('Unknown Nyx tool: ' + ATool);
    end;

    if LMutation then
    begin

      if (ATool <> 'nyx_select') and (not LProjectMutation or LProjectApply) and
        (LBefore <> EncodeNyxProject(FSession.ProjectSnapshot)) then
      begin
        Changed;
      end;
      { Upload chunks own no design/source/history mutation. Keep their retry
        receipts small and avoid rendering/checkpointing the accepted unit for
        each window; apply still uses the ordinary full paired edit boundary. }

      if LProjectMutation and not LProjectApply then
      begin
        Result := NyxObject([NyxField('revision', NyxData(FRevision))]);
      end
      else
      begin
        Result := Summary;
      end;

      if LProjectMutation then
      begin
        Result := WithResults(Result, LProjectResults, 'projectImport');
      end;

      if LCallbackApply then
      begin
        Result := WithResults(Result, LCallbackResults, 'callbacks');
      end;

      if LHandlerApply then
      begin
        Result := WithResults(Result, LHandlerResults, 'handlers');
      end;

      if LImportApply then
      begin
        Result := WithResults(Result, LImportResults, 'imports');
      end;

      if LRoutineApply then
      begin
        Result := WithResults(Result, LRoutineResults, 'routines');
      end;

      if LDeclarationApply then
      begin
        Result := WithResults(Result, LDeclarationResults, 'declarations');
      end;

      if LViewsApply then
      begin
        Result := WithResults(Result, LViewsResults, 'views');
      end;

      if LSourceApply then
      begin
        Result := WithResults(Result, LSourceResults, 'sourceEdit');
      end;

      if LRootApply then
      begin
        Result := WithResults(Result, LRootResults, 'removedRoots');
      end;

      if LStateApply then
      begin
        Result := WithResults(Result, LStateResults, 'stateBindings');
      end;

      if LCollectionApply then
      begin
        Result := WithResults(Result, LCollectionResults, 'collections');
      end;

      if LResourceApply then
      begin
        Result := WithResults(Result, LResourceResults, 'resources');
      end;

      if Length(FReceiptKeys) = 64 then
      begin
        for LIndex := 1 to High(FReceiptKeys) do
        begin
          FReceiptKeys[LIndex - 1] := FReceiptKeys[LIndex];
          FReceiptRequests[LIndex - 1] := FReceiptRequests[LIndex];
          FReceiptResults[LIndex - 1] := FReceiptResults[LIndex];
        end;
        SetLength(FReceiptKeys, 63);
        SetLength(FReceiptRequests, 63);
        SetLength(FReceiptResults, 63);
      end;
      LIndex := Length(FReceiptKeys);
      SetLength(FReceiptKeys, LIndex + 1);
      SetLength(FReceiptRequests, LIndex + 1);
      SetLength(FReceiptResults, LIndex + 1);
      FReceiptKeys[LIndex] := LKey;
      FReceiptRequests[LIndex] := LRequest;
      FReceiptResults[LIndex] := Result;
    end;
    { Read responses are bounded before returning. Callback results were
      preflighted before publication; ordinary receipts have a small summary.
      No whole-document data is sent through agent queries. }

    if not LMutation then
    begin
      BoundContext(Result);
    end;
    Log(AActor, ATool, 'completed');
  except
    on LException: Exception do
    begin
      Log(AActor, ATool, 'refused: ' + LException.Message);
      raise;
    end;
  end;
end;

function TNyxAgentSession.Exchange(const ARequest: TNyxDataValue): TNyxDataValue;
var
  LOperation: TNyxText;
  LPair: TNyxProjectPair;
  LDocument: TNyxDocument;
  LSelected: TNyxText;
  LView: TNyxText;
  LBefore: TNyxText;
  LPermission: TNyxText;
  LAfter: Integer;
  LResource: TNyxResourceRef;
  LResourceLocale: TNyxLocaleRef;
begin
  LOperation := ARequest.Field('op').AsText;
  LAfter := IntegerArgument(ARequest, 'after', 0, 0, High(Integer));

  if LOperation = 'observe' then
  begin
    NyxAgentFields(ARequest, '|op|after|resource|');

    if NyxAgentHas(ARequest, 'resource') then
    begin
      NyxAgentFields(ARequest.Field('resource'), '|reference|locale|');
      LResource := NyxResourceRef(ARequest.Field('resource').Field('reference').AsText);
      LResourceLocale := NyxDefaultLocale;

      if ARequest.Field('resource').Field('locale').AsText <> '' then
      begin
        LResourceLocale := NyxLocale(ARequest.Field('resource').Field('locale').AsText);
      end;
      Exit(EditorState(LAfter, LResource, LResourceLocale));
    end;
    Exit(EditorState(LAfter));
  end;

  if LOperation = 'configure' then
  begin
    NyxAgentFields(ARequest, '|op|after|permission|');
    LPermission := ARequest.Field('permission').AsText;

    if LPermission = 'disabled' then
    begin
      FPermission := apDisabled;
    end
    else if LPermission = 'readOnly' then
    begin
      FPermission := apReadOnly;
    end
    else if LPermission = 'edit' then
    begin
      FPermission := apEdit;
    end
    else
    begin
      raise ENyxModel.Create('Unknown agent permission');
    end;
    Log('Studio operator', 'agent permissions', LPermission);
    Exit(EditorState(LAfter));
  end;

  if LOperation = 'report' then
  begin
    NyxAgentFields(ARequest, '|op|after|report|');
    PublishCompilerReport(DecodeNyxCompilerReport(ARequest.Field('report').AsText));
    Exit(EditorState(LAfter));
  end;

  if (LOperation = 'claim') or (LOperation = 'commit') then
  begin
    NyxAgentFields(ARequest, '|op|after|expectedRevision|project|selection|view|');

    if (LOperation = 'claim') and FClaimed then
    begin
      Exit(EditorState(0));
    end;

    if LOperation = 'commit' then
    begin
      RequireRevision(ARequest);
    end;
    LPair := DecodeNyxProject(ARequest.Field('project').AsText);
    LSelected := ARequest.Field('selection').AsText;
    LView := ARequest.Field('view').AsText;
    LDocument := TNyxCodec.Decode(LPair.Design);
    try

      if ((LSelected <> '') and (LDocument.Find(LSelected) = nil)) or
        ((LView <> '') and ((LDocument.Find(LView) = nil) or
          (LDocument.Find(LView).Parent <> nil))) then
      begin
        raise ENyxModel.Create('Editor selection/view is outside its candidate document');
      end;
    finally
      LDocument.Free;
    end;
    LBefore := EncodeNyxProject(FSession.ProjectSnapshot);

    if LOperation = 'claim' then
    begin
      FSession.LoadProject(LPair);
    end
    else
    begin
      FSession.AdoptProject(LPair, spaSynchronization);
    end;

    if LView <> '' then
    begin
      FSession.Activate(LView);
    end;

    if LSelected <> '' then
    begin
      FSession.Select(LSelected);
    end;

    if (LBefore <> EncodeNyxProject(FSession.ProjectSnapshot)) or
      (LOperation = 'commit') then
    begin
      Changed;
    end;
    FClaimed := True;
    Log('Studio', LOperation, 'completed');
  end
  else if LOperation = 'history' then
  begin
    NyxAgentFields(ARequest, '|op|after|expectedRevision|direction|');
    RequireRevision(ARequest);

    { The same full checkpoint policy applies to operator and MCP navigation. }
    LBefore := EncodeNyxProject(FSession.ProjectSnapshot);

    if ARequest.Field('direction').AsText = 'undo' then
    begin
      FSession.Undo;
    end
    else if ARequest.Field('direction').AsText = 'redo' then
    begin
      FSession.Redo;
    end
    else
    begin
      raise ENyxModel.Create('History direction must be undo or redo');
    end;

    if LBefore <> EncodeNyxProject(FSession.ProjectSnapshot) then
    begin
      Changed;
    end;
    Log('Studio', 'history', 'completed');
  end
  else
  begin
    raise ENyxModel.Create('Unknown editor exchange');
  end;
  Result := EditorState(0);
end;

procedure TNyxAgentSession.RecordActivity(const AActor, AOperation,
  AOutcome: TNyxText);
begin
  Log(AActor, AOperation, AOutcome);
end;

function TNyxAgentSession.BuildPair(AExpected: Integer; AScope: TNyxBuildScope;
  const AView: TNyxText): TNyxProjectPair;
begin

  if FPermission <> apEdit then
  begin
    raise ENyxModel.Create('Agent builds require Allow edits in Studio');
  end;
  Result := EditorBuildPair(AExpected, AScope, AView);
end;

function TNyxAgentSession.EditorSourcePair(AExpected: Integer): TNyxProjectPair;
begin

  if AExpected <> FRevision then
  begin
    raise ENyxModel.Create('Source compilation revision conflict');
  end;
  Result := FSession.ProjectSnapshot;
end;

function TNyxAgentSession.CommitSourceProjection(AExpected: Integer;
  const ABaseline: TNyxProjectPair; const AProjection: INyxSourceProjection;
  const ARequest: TNyxStudioSourceRequest; const ASchemas: INyxSchemaSnapshot;
  const ASelection, AView: TNyxControlRef): TNyxDataValue;
var
  LCurrent: TNyxProjectPair;
  LDocument: TNyxDocument;
  LView: TNyxNode;
  LPrepared: INyxPreparedSource;
  LCompletion: TNyxSourceCompletion;
begin
  LCurrent := EditorSourcePair(AExpected);

  if EncodeNyxProject(LCurrent) <> EncodeNyxProject(ABaseline) then
  begin
    raise ENyxProjectConflict.Create('Source publication baseline changed; current work is retained');
  end;

  if (AProjection = nil) or (AProjection.State <> spsExecuted) or
    (AProjection.Source <> ARequest.Source) then
  begin
    raise ENyxProjectConflict.Create('Source publication requires an actually executed result');
  end;

  if LCurrent.Pending and (LCurrent.Draft <> AProjection.Source) then
  begin
    raise ENyxProjectConflict.Create('Source publication would discard a different unfinished draft');
  end;
  LDocument := AProjection.CopyDocument;
  try
    LView := LDocument.Find(AView.ID);

    if ((ASelection.ID <> '') and (LDocument.Find(ASelection.ID) = nil)) or
      ((AView.ID <> '') and ((LView = nil) or (LView.Parent <> nil))) then
    begin
      raise ENyxProjectConflict.Create('Source publication selection/view is outside its result');
    end;
  finally
    LDocument.Free;
  end;
  { Reuse the ordinary execution preparation/completion contract. It isolates
    captured creators, revokes stale session/schema work and swaps both owners
    under the short creator guard with exactly the existing history policy. }

  if not ARequest.Changed and (AProjection.Design <> LCurrent.Design) then
  begin
    raise ENyxProjectConflict.Create('Unchanged source produced different meaning; accepted files are retained');
  end;
  LPrepared := PrepareNyxProjectedSource(AProjection, ASchemas);
  LCompletion := FSession.CompleteSourceRequest(ARequest, LPrepared);

  if not (LCompletion in [nscApplied, nscUnchanged]) then
  begin
    raise ENyxProjectConflict.Create('Source publication request or creators became stale or refused');
  end;

  if AView.ID <> '' then
  begin
    FSession.Activate(AView.ID);
  end;

  if ASelection.ID <> '' then
  begin
    FSession.Select(ASelection.ID);
  end;
  Changed;
  FClaimed := True;
  Log('Studio', 'source publication', 'completed');
  Result := EditorState(0);
end;

function TNyxAgentSession.CaptureSourcePublication(AExpected: Integer;
  const ASource: TNyxText; out ARequest: TNyxStudioSourceRequest;
  out ASchemas: INyxSchemaSnapshot): TNyxProjectPair;
begin
  Result := EditorSourcePair(AExpected);

  if ASource <> FSession.DraftSource then
  begin
    raise ENyxProjectConflict.Create('Source capture requires the exact current editor buffer');
  end;
  ASchemas := CaptureNyxSchemas;
  ARequest := FSession.PrepareSourceRequest(ASchemas.Revision);
end;

function TNyxAgentSession.CaptureVisualPublication(AExpected: Integer;
  const AIntent: TNyxDataValue; const ASource: TNyxText;
  out ARequest: TNyxStudioDesignRequest; out AProposal: INyxPreparedDesign;
  out ASchemas: INyxSchemaSnapshot): TNyxProjectPair;
var
  LEdit: TNyxStudioDesignEdit;
begin
  Result := EditorSourcePair(AExpected);
  LEdit := ReadNyxStudioDesignIntent(AIntent);
  ASchemas := CaptureNyxSchemas;
  ARequest := FSession.PrepareDesignRequest(LEdit, ASchemas.Revision);

  if not ARequest.RequiresExecution then
  begin
    raise ENyxProjectConflict.Create('Visual source publication requires an admitted executed baseline');
  end;
  AProposal := PrepareNyxStudioDesign(ARequest, ASchemas);

  if AProposal.Diagnostic.Defined or not AProposal.RequiresCompilation or
    (AProposal.Source <> ASource) then
  begin
    raise ENyxProjectConflict.Create('Visual source differs from the independently prepared semantic edit');
  end;
end;

function TNyxAgentSession.CommitVisualProjection(AExpected: Integer;
  const ABaseline: TNyxProjectPair; const AProjection: INyxSourceProjection;
  const ARequest: TNyxStudioDesignRequest; const AProposal: INyxPreparedDesign;
  const ASchemas: INyxSchemaSnapshot): TNyxDataValue;
var
  LCurrent: TNyxProjectPair;
  LPrepared: INyxPreparedDesign;
  LCompletion: TNyxSourceCompletion;
begin
  LCurrent := EditorSourcePair(AExpected);

  if EncodeNyxProject(LCurrent) <> EncodeNyxProject(ABaseline) then
  begin
    raise ENyxProjectConflict.Create('Visual publication baseline changed; current files and draft retained');
  end;
  LPrepared := PrepareNyxCompiledDesign(ARequest, AProposal, AProjection, ASchemas);

  if LPrepared.Diagnostic.Defined then
  begin
    raise ENyxProjectConflict.Create('Visual publication refused: ' + LPrepared.Diagnostic.Message);
  end;
  LCompletion := FSession.CompleteDesignRequest(ARequest, LPrepared);

  if not (LCompletion in [nscApplied, nscUnchanged]) then
  begin
    raise ENyxProjectConflict.Create('Visual publication request or creators became stale');
  end;
  Changed;
  FClaimed := True;
  Log('Studio', 'visual source publication', 'completed');
  Result := EditorState(0);
end;

procedure TNyxAgentSession.CaptureEditorProject(AExpected: Integer;
  out APair: TNyxProjectPair; out ACheckpoint: TNyxSourceCheckpoint);
begin
  APair := EditorSourcePair(AExpected);
  ACheckpoint := FSession.AcceptedSourceCheckpoint;
end;

function TNyxAgentSession.EditorBuildPair(AExpected: Integer; AScope: TNyxBuildScope;
  const AView: TNyxText): TNyxProjectPair;
var
  LNode: TNyxNode;
  LIndex: Integer;
  LFound: Boolean;
begin

  if AExpected <> FRevision then
  begin
    raise ENyxModel.Create('Build revision conflict');
  end;

  if FSession.DraftSource <> FSession.Source then
  begin
    raise ENyxModel.Create('Resolve the pending draft before building');
  end;

  if AScope = bsApplication then
  begin

    if AView <> '' then
    begin
      raise ENyxModel.Create('Application builds omit view');
    end;
  end
  else
  begin
    LNode := FSession.Document.Find(AView);
    LFound := False;

    if AScope = bsView then
    begin
      for LIndex := 0 to FSession.Document.Count - 1 do
      begin
        LFound := LFound or (FSession.Document.Pages[LIndex] = LNode);
      end;
    end
    else
    begin
      for LIndex := 0 to FSession.Document.ComponentCount - 1 do
      begin
        LFound := LFound or (FSession.Document.Components[LIndex] = LNode);
      end;
    end;

    if (LNode = nil) or not LFound then
    begin
      raise ENyxModel.Create('Build view must match the exact requested page/reusable scope');
    end;
  end;
  Result := FSession.ProjectSnapshot;
end;

procedure TNyxAgentSession.InheritPermission(AValue: TNyxAgentPermission);
begin
  FPermission := AValue;
end;

function TNyxAgentSession.ReviewSeed(AExpected: Integer): TNyxProjectPair;
var
  LPair: TNyxProjectPair;
begin

  if FPermission <> apEdit then
  begin
    raise ENyxProjectConflict.Create('Review creation requires Allow edits in Studio');
  end;

  if AExpected <> FRevision then
  begin
    raise ENyxProjectConflict.Create('Review seed revision conflict; inspect the active session');
  end;
  LPair := FSession.ProjectSnapshot;
  Result := NyxProjectPair(LPair.Design, LPair.Source);
end;

function TNyxAgentSession.CurrentPair(const APair: TNyxProjectPair): Boolean;
var
  LCurrent: TNyxProjectPair;
begin
  LCurrent := FSession.ProjectSnapshot;
  Result := not LCurrent.Pending and (LCurrent.Design = APair.Design) and
    (LCurrent.Source = APair.Source);
end;

procedure TNyxAgentSession.PublishCompilerReport(const AReport: INyxCompilerReport);
begin
  FReport := AReport;
  Inc(FCompilerSequence);
end;

function TNyxAgentSession.CompilerSnapshot: TNyxDataValue;
var
  LItems: array of TNyxDataValue;
  LItem: TNyxCompilerDiagnostic;
  LIndex: Integer;
  LTotal: Integer;
  LCount: Integer;
  LAccepted: Boolean;
  LOrder: TNyxCompilerDiagnosticIndices;
begin
  LTotal := 0;
  LAccepted := False;

  if FReport <> nil then
  begin
    LTotal := FReport.Count;
    LAccepted := FReport.Source = FSession.Source;
  end;
  LCount := LTotal;

  if LCount > 20 then
  begin
    LCount := 20;
  end;
  SetLength(LItems, LCount);
  LOrder := NyxCompilerDiagnosticOrder(FReport);
  for LIndex := 0 to High(LItems) do
  begin
    LItem := FReport.Item(LOrder[LIndex]);
    LItems[LIndex] := NyxObject([
      NyxField('file', NyxData(CaptionText(LItem.FileName, 512))),
      NyxField('severity', NyxData(Ord(LItem.Severity))),
      NyxField('message', NyxData(CaptionText(LItem.Message, 1024))),
      NyxField('line', NyxData(LItem.Line)), NyxField('column', NyxData(LItem.Column)),
      NyxField('sourceLine', NyxData(LItem.SourceLine)),
      NyxField('sourceColumn', NyxData(LItem.SourceColumn))]);
  end;
  { Observers already own accepted source. Send bounded diagnostics and exact
    source-match admission instead of duplicating a potentially large unit. }
  Result := NyxObject([NyxField('sequence', NyxData(FCompilerSequence)),
    NyxField('total', NyxData(LTotal)), NyxField('acceptedSource', NyxData(LAccepted)),
    NyxField('items', NyxArray(LItems))]);
end;

function TNyxAgentSession.PreviewPair(AExpected: Integer;
  const AView, AActor: TNyxText): TNyxProjectPair;
begin
  Result := PreviewPair(AExpected, AView, AActor, TNyxPresentationSelection.None);
end;

function TNyxAgentSession.PreviewPair(AExpected: Integer;
  const AView, AActor: TNyxText; const ASelection: TNyxPresentationSelection): TNyxProjectPair;
var
  LNode: TNyxNode;
begin

  if FPermission = apDisabled then
  begin
    raise ENyxModel.Create('Agent access is disabled');
  end;

  if AExpected <> FRevision then
  begin
    raise ENyxModel.Create('Preview revision conflict');
  end;
  LNode := FSession.Document.Find(AView);
  ASelection.Validate(FSession.Document.Presentations);

  if (LNode = nil) or (LNode.Parent <> nil) then
  begin
    raise ENyxModel.Create('Preview requires a page or reusable root');
  end;
  Result := FSession.ProjectSnapshot;
  Log(AActor, 'nyx_preview', 'render requested');
end;

end.
