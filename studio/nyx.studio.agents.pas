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
  nyx.expressions, nyx.expressions.editing, nyx.pascal.project,
  nyx.pascal.drafts, nyx.pascal.filechanges, nyx.pascal.sources, nyx.pascal.completion,
  nyx.canvas.types, nyx.canvas.geometry, nyx.canvas.precision,
  nyx.canvas.declarations, nyx.canvas.route.types,
  nyx.flow.types, nyx.flow.declarations, nyx.logic.catalog,
  nyx.programs.types, nyx.programs.contract, nyx.programs, nyx.programs.declarations,
  nyx.flow.routes, nyx.flow.route.model, nyx.diagram, nyx.diagram.declarations,
  nyx.studio.session, nyx.studio.projects, nyx.studio.compiler, nyx.studio.builds,
  nyx.studio.transactions,
  nyx.studio.sourceprojection,
  nyx.studio.rootedits, nyx.presentations, nyx.menu.declarations, nyx.root.types,
  nyx.application.resources, nyx.resources, nyx.resources.runtime.view,
  nyx.studio.projectimport;

type
  { Operator permissions are closed, session-local and never part of a design.
    Only the editor exchange can change them. MCP cannot grant itself access. }
  TNyxAgentPermission = (apDisabled, apReadOnly, apEdit);

  { Opaque transient completion admission. Session/load, revision and trusted
    caller role bind this exact read; agent reads also bind permission generation.
    No source interface, editor pointer, serialization or publication capability
    is retained. Operator authority cannot be reused as agent authority or vice
    versa. Default values refuse. }
  TNyxAgentPascalCompletionContext = record
  private
    FSession: TNyxStudioCommandContext;
    FRevision: Integer;
    FPermissionGeneration: Integer;
    FOperator: Boolean;
  end;

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
    and source/history; bounded toolbar inspection shares its authored policy.
    No shadow design format or browser automation exists.
    Native transports serialize all calls under an external lock. Portable tests
    and native controllers use the same command/query boundary directly. }
  TNyxAgentSession = class
  private
    FSession: TNyxStudioSession;
    FRevision: Integer;
    FPermission: TNyxAgentPermission;
    FPermissionGeneration: Integer;
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
    { Eight actor/revision/exact-group warnings. Values own bounded request text;
      no editor/node is borrowed and authority expires on change/disconnect. }
    FFileReviews: array of TNyxDataValue;
    FFileReviewSerial: Integer;
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
    { One immutable owning-unit index, keyed by revision and inspection origin.
      Editing snapshots remain read-only data; neither cache is an admission or
      compiler capability. Changed revokes both with ordinary source authority. }
    FSourceRoutineIndex: INyxPascalRoutineIndex;
    FSourceRoutineOwner: TNyxPascalSourceRef;
    FSourceRoutineRevision: Integer;
    FSourceRoutineKind: TNyxPascalSourceSnapshotKind;
    FSourceInspection: INyxPascalSourceSnapshot;
    FSourceInspectionRevision: Integer;
    { One detached declaration catalog per owning unit/origin/revision. Shared
      source capture is immutable; changing authority retires this cache too. }
    FSourceSymbols: INyxPascalSymbolIndex;
    FSourceSymbolsOwner: TNyxPascalSourceRef;
    FSourceSymbolsRevision: Integer;
    FSourceSymbolsKind: TNyxPascalSourceSnapshotKind;
    procedure RemoveProjectImport(AIndex: Integer);
    function ProjectImportIndex(const AOwner, AID: TNyxText): Integer;
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
    { Optional bounded tab context: copied policies and paged direct child IDs.
      No whole form baseline, tree or mutable state is returned to an agent. }
    function TabsDetails(ANode: TNyxNode; AOffset, ALimit: Integer): TNyxDataValue;
    function SelectionDetails(ANode: TNyxNode; AOffset, ALimit: Integer): TNyxDataValue;
    function DataToolbarDetails(ANode: TNyxNode;
      const AArguments: TNyxDataValue): TNyxDataValue;
    function CanvasDetails(ANode: TNyxNode; ALayerOffset, ALayerLimit,
      AItemOffset, AItemLimit: Integer; ALocal: Boolean;
      const AArguments: TNyxDataValue): TNyxDataValue;
    { One requested local/effective value domain, with bounded Unicode choice
      windows. Realized projections are released before the response escapes. }
    function ValueDomainDetails(ANode: TNyxNode;
      const AArguments: TNyxDataValue): TNyxDataValue;
    { Paged reachable named paths from an independent effective projection. }
    function NamedParts(ANode: TNyxNode; AOffset, ALimit: Integer): TNyxDataValue;
    function Components(const AArguments: TNyxDataValue): TNyxDataValue;
    function LogicQuery(const AArguments: TNyxDataValue): TNyxDataValue;
    function ProgramQuery(const AArguments: TNyxDataValue): TNyxDataValue;
    function DiagramQuery(const AArguments: TNyxDataValue): TNyxDataValue;
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
    function ProjectRoutines(const AArguments: TNyxDataValue): TNyxDataValue;
    { Bounded read-only lexical declarations; compiler resolution stays separate. }
    function SourceSymbols(const AArguments: TNyxDataValue): TNyxDataValue;
    { Trusted transport wrappers select authority in code, never from JSON. }
    function CaptureCompletion(const AArguments: TNyxDataValue;
      out AContext: TNyxAgentPascalCompletionContext;
      AOperator: Boolean): INyxPascalCompletionRequest;
    function FinishCompletion(const AArguments: TNyxDataValue;
      const AContext: TNyxAgentPascalCompletionContext;
      const ARequest: INyxPascalCompletionRequest;
      const AResult: INyxPascalCompletionResult; AOperator: Boolean;
      out AFrozenResult: INyxPascalCompletionResult): TNyxDataValue;
    { Decode the closed wire choice and require exact revision for buffer reads.
      Default accepted reads retain their existing optional-revision contract. }
    function SourceReadKind(const AArguments: TNyxDataValue): TNyxPascalSourceSnapshotKind;
    function SourceInspection(AKind: TNyxPascalSourceSnapshotKind): INyxPascalSourceSnapshot;
    function RoutineIndex(const AOwner: TNyxPascalSourceRef;
      AKind: TNyxPascalSourceSnapshotKind): INyxPascalRoutineIndex;
    { Exact interface counterpart context and one grouped declaration edit. }
    function Declarations(const AArguments: TNyxDataValue; AApply: Boolean): TNyxDataValue;
    { Bounded exact managed-builder text and one guarded paired source command.
      Application helpers/imports remain outside its immutable edit boundary. }
    function Views(const AArguments: TNyxDataValue; AApply: Boolean): TNyxDataValue;
    { Bounded accepted/current-buffer inspection and exact accepted range groups.
      Editing reads require their revision and grant no source publication;
      mutation still uses ordinary paired admission and retains draft guards. }
    function SourceUnit(const AArguments: TNyxDataValue; AApply: Boolean): TNyxDataValue;
    { Paged typed source identities only; no file text or host path is returned. }
    function SourceFiles(const AArguments: TNyxDataValue): TNyxDataValue;
    function EditCallbacks(const AArguments: TNyxDataValue;
      const AActor: TNyxText; AApply: Boolean): TNyxDataValue;
    { Reused at admission and completion. Tickets belong to the exact transport,
      revision and ordered changes; validation never consumes them. }
    function CallbackReviewIndex(const AArguments: TNyxDataValue;
      const AOwner: TNyxText; AApply: Boolean): Integer;
    procedure ConsumeCallbackReview(AIndex: Integer);
    function PrepareCallbackProposal(const AArguments: TNyxDataValue;
      out ARequest: TNyxStudioDesignRequest; out AProposal: INyxPreparedDesign;
      out ASchemas: INyxSchemaSnapshot; out AResults: TNyxDataValue): TNyxProjectPair;
    function CompiledReceipt(const ATool, AOwner: TNyxText;
      const AArguments: TNyxDataValue; out AReceipt: TNyxDataValue): Boolean;
    procedure RememberCompiledReceipt(const ATool, AOwner: TNyxText;
      const AArguments, AReceipt: TNyxDataValue);
    function RemoveRoots(const AArguments: TNyxDataValue;
      const AActor: TNyxText; AApply: Boolean): TNyxDataValue;
    function ReviewSourceFiles(const AArguments: TNyxDataValue;
      const AOwner: TNyxText): TNyxDataValue;
    procedure RequireFileReview(const AArguments: TNyxDataValue;
      const AOwner: TNyxText);
  public
    constructor Create; overload;
    { Serialized transport admission captures acknowledged buffers and an opaque
      session/load identity. Completion is read-only but always revision-pinned;
      the host must release its model guard before calling an external provider. }
    function CapturePascalCompletion(const AArguments: TNyxDataValue;
      out AContext: TNyxAgentPascalCompletionContext): INyxPascalCompletionRequest;
    { Reacquire this current session before calling. Recheck permission, revision,
      load identity, complete sources and query/response identity. Return only
      bounded detached rows; no buffer, selection, pair or history is changed. }
    function FinishPascalCompletion(const AArguments: TNyxDataValue;
      const AContext: TNyxAgentPascalCompletionContext;
      const ARequest: INyxPascalCompletionRequest;
      const AResult: INyxPascalCompletionResult): TNyxDataValue;
    { Private authenticated operator inspection remains available independently
      of agent permission. Only the owning editor transport calls these wrappers;
      context/source/revision checks are identical and no mutation is granted. }
    function CaptureEditorCompletion(const AArguments: TNyxDataValue;
      out AContext: TNyxAgentPascalCompletionContext): INyxPascalCompletionRequest;
    function FinishEditorCompletion(const AArguments: TNyxDataValue;
      const AContext: TNyxAgentPascalCompletionContext;
      const ARequest: INyxPascalCompletionRequest;
      const AResult: INyxPascalCompletionResult): TNyxDataValue; overload;
    { Private typed handoff preserves optional project navigation only after all
      ordinary admission succeeds. No additional field enters public MCP JSON. }
    function FinishEditorCompletion(const AArguments: TNyxDataValue;
      const AContext: TNyxAgentPascalCompletionContext;
      const ARequest: INyxPascalCompletionRequest;
      const AResult: INyxPascalCompletionResult;
      out AFrozenResult: INyxPascalCompletionResult): TNyxDataValue; overload;
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
    { Trusted transport retirement releases this owner's private uploads and
      source-file reviews. Accepted history and other owners remain unchanged. }
    procedure ReleaseProjectImports(const AOwner: TNyxText);
    { Explicit compiler review is staged transport work, not document adoption.
      Capture is pure; Start follows successful queue admission, so a full queue
      retains an earlier review. Completion rechecks exact live upload/ticket,
      baseline, revision, schema and permission before exposing its candidate. }
    function ProjectReviewReceipt(const AOwner: TNyxText;
      const AArguments: TNyxDataValue; out AReceipt: TNyxDataValue): Boolean;
    function CaptureProjectImportReview(const AOwner: TNyxText;
      const AArguments: TNyxDataValue): INyxProjectImportReviewRequest;
    procedure StartProjectImportReview(const ARequest: INyxProjectImportReviewRequest;
      const AReceipt: TNyxDataValue);
    function CompleteProjectImportReview(const ARequest: INyxProjectImportReviewRequest;
      const ABaseline: TNyxProjectPair; const AProjection: INyxSourceProjection;
      const ASchemas: INyxSchemaSnapshot): TNyxDataValue;
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
    { A private editor Open captures its imported pair separately from the live
      baseline. Successful owned execution publishes one complete history command;
      no file, checkpoint decoder or agent flag grants this admission. }
    function CaptureProjectOpening(AExpected: Integer; const APair: TNyxProjectPair;
      AResolution: TNyxProjectResolution; out ARequest: TNyxStudioProjectRequest;
      out ASchemas: INyxSchemaSnapshot): TNyxProjectPair;
    function CommitProjectOpening(AExpected: Integer; const ABaseline: TNyxProjectPair;
      const AProjection: INyxSourceProjection; const ARequest: TNyxStudioProjectRequest;
      const ASchemas: INyxSchemaSnapshot): TNyxDataValue;
    { Capture the ordinary source request and immutable creator environment
      without changing the draft. Source must be the editor's exact current
      buffer, including accepted text when no draft is pending. }
    function CaptureSourcePublication(AExpected: Integer; const ASource: TNyxText;
      out ARequest: TNyxStudioSourceRequest;
      out ASchemas: INyxSchemaSnapshot): TNyxProjectPair;
    { Owning-editor helper capture reconstructs the candidate from its own
      acknowledged draft/base and accepted inventory, never proposed wire files. }
    function CaptureFilePublication(AExpected: Integer; const ASource: TNyxText;
      const AFile: TNyxPascalSourceRef; out ARequest: TNyxStudioSourceRequest;
      out ASchemas: INyxSchemaSnapshot): TNyxProjectPair;
    { Reconstruct one explicit inventory change against this editor's accepted
      files; supplied candidates cannot replace other helpers or their buffers. }
    function CaptureFileChangePublication(AExpected: Integer; const ASource: TNyxText;
      const AChange: TNyxPascalFileChange; out ARequest: TNyxStudioSourceRequest;
      out ASchemas: INyxSchemaSnapshot): TNyxProjectPair;
    { Independently prepare an intent against this server-owned session. Supplied
      source must equal the server's writer output; an unfinished buffer stays
      untouched. Both results are immutable nonpublishable proposal values. }
    function CaptureVisualPublication(AExpected: Integer; const AIntent: TNyxDataValue;
      const ASource: TNyxText; out ARequest: TNyxStudioDesignRequest;
      out AProposal: INyxPreparedDesign;
      out ASchemas: INyxSchemaSnapshot): TNyxProjectPair;
    { Public semantic transaction admission uses the same immutable visual
      proposal as Studio. Authority belongs to its already authenticated caller;
      no source or executed-origin flag is accepted from that caller. }
    function RequiresCompiledTransaction: Boolean;
    { Admit shape/permission and return an independent original receipt for an
      exact transport-owned retry. Changed arguments refuse even after success;
      False does not reserve a job or bypass the later revision/draft guards. }
    function TransactionReceipt(const AOwner: TNyxText;
      const AArguments: TNyxDataValue; out AReceipt: TNyxDataValue): Boolean;
    { Serialized host admission only. Retain the last 64 bounded queued receipts
      in this context; compiler status owns their eventual publication outcome. }
    procedure RememberTransactionReceipt(const AOwner: TNyxText;
      const AArguments, AReceipt: TNyxDataValue);
    { Focused data jobs keep the original tool/Apply envelope in the shared
      transport-owned receipt budget. Normalization cannot alias a mixed call. }
    function DataReceipt(ADomain: TNyxTransactionDomain; const AOwner: TNyxText;
      const AArguments: TNyxDataValue; out AReceipt: TNyxDataValue): Boolean;
    procedure RememberDataReceipt(ADomain: TNyxTransactionDomain;
      const AOwner: TNyxText; const AArguments, AReceipt: TNyxDataValue);
    { Callback jobs share exact retry storage with all ordinary mutations. }
    function CallbackReceipt(const AOwner: TNyxText;
      const AArguments: TNyxDataValue; out AReceipt: TNyxDataValue): Boolean;
    { Body groups share bounded exact-retry storage with other semantic jobs. }
    function HandlerReceipt(const AOwner: TNyxText;
      const AArguments: TNyxDataValue; out AReceipt: TNyxDataValue): Boolean;
    procedure RememberHandlerReceipt(const AOwner: TNyxText;
      const AArguments, AReceipt: TNyxDataValue);
    { An exact unchanged proposal needs no new execution or editor command.
      Returns the ordinary bounded summary while preserving existing Redo. }
    function HandlerNoOpReceipt(const ARequest: TNyxStudioDesignRequest;
      const AProposal: INyxPreparedDesign; const AResults: TNyxDataValue): TNyxDataValue;
    { Capture exact body intent from this session's trusted full checkpoint.
      Proposed source must compile and retain the complete design to publish. }
    function CaptureHandlerPublication(const AArguments: TNyxDataValue;
      out ARequest: TNyxStudioDesignRequest; out AProposal: INyxPreparedDesign;
      out ASchemas: INyxSchemaSnapshot; out AResults: TNyxDataValue): TNyxProjectPair;
    { Exact general range edits keep proposed text off the accepted buffer.
      Capture validates the complete immutable group; only the owning compiler
      transport may publish its subsequently executed source request. }
    function CaptureSourceEditPublication(const AArguments: TNyxDataValue;
      const AOwner: TNyxText;
      out ARequest: TNyxStudioSourceRequest;
      out ASchemas: INyxSchemaSnapshot): TNyxProjectPair;
    { Supporting-input edits always need construction, including a literal main
      baseline. This decision grants no admission; capture validates all fields. }
    function SourceEditRequiresCompiler(const AArguments: TNyxDataValue): Boolean;
    { Whole-unit and body-only operations share the same bounded Pascal-tool
      retry budget; changed arguments or cross-tool operation IDs refuse. }
    function SourceEditReceipt(const AOwner: TNyxText;
      const AArguments: TNyxDataValue; out AReceipt: TNyxDataValue): Boolean;
    procedure RememberSourceEditReceipt(const AOwner: TNyxText;
      const AArguments, AReceipt: TNyxDataValue);
    procedure RememberCallbackReceipt(const AOwner: TNyxText;
      const AArguments, AReceipt: TNyxDataValue);
    { Trusted backend capture, never a client-supplied source or trust flag. }
    function CaptureCallbackPublication(const AArguments: TNyxDataValue;
      const AOwner: TNyxText; out ARequest: TNyxStudioDesignRequest;
      out AProposal: INyxPreparedDesign; out ASchemas: INyxSchemaSnapshot;
      out AResults: TNyxDataValue): TNyxProjectPair;
    { Recheck the exact review immediately before the ordinary whole-pair
      publication. Only a successful admitted group consumes its removal ticket. }
    function CommitCallbackProjection(AExpected: Integer; const ABaseline: TNyxProjectPair;
      const AProjection: INyxSourceProjection; const ARequest: TNyxStudioDesignRequest;
      const AProposal: INyxPreparedDesign; const ASchemas: INyxSchemaSnapshot;
      const AArguments: TNyxDataValue; const AOwner: TNyxText): TNyxDataValue;
    { Capture fresh owned values and creators, then independently prepare the
      whole typed group. Invalid/stale/pending/unsupported proposals refuse before
      a worker is allocated; the returned pair contains no mutable tree borrow. }
    function CaptureTransactionPublication(const AArguments: TNyxDataValue;
      out ARequest: TNyxStudioDesignRequest; out AProposal: INyxPreparedDesign;
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
    { Trusted in-process accepted fork. Keeps the live opaque source checkpoint
      so executed Pascal never falls back to literal parsing. The caller owns
      the returned session: fresh revision/history/navigation and no draft,
      report, retry/upload/removal authority or connection presence. The original
      session stays borrowed and unchanged. Permission/revision refuse first;
      no external checkpoint or source is accepted at this boundary. }
    function ForkAccepted(AExpected: Integer): TNyxAgentSession;
    { Only an owning controller may inherit the operator's current permission.
      This is not a semantic tool or editor command and creates no revision/history. }
    procedure InheritPermission(AValue: TNyxAgentPermission);
    property Revision: Integer read FRevision;
    { Exact source-pair draft state, read without logging a semantic operation. }
    property PendingDraft: Boolean read GetPendingDraft;
    property Permission: TNyxAgentPermission read FPermission;
    { Transient authority generation, also changed by disable/reenable. Hosts
      compare it around external reads; it is never checkpoint/project data. }
    property PermissionGeneration: Integer read FPermissionGeneration;
  end;

{ Shared strict field helpers at JSON admission boundaries. Unknown arguments
  are refused so a misspelled field never silently changes command meaning. }
function NyxAgentHas(const AValue: TNyxDataValue; const AKey: TNyxText): Boolean;
procedure NyxAgentFields(const AValue: TNyxDataValue; const AAllowed: TNyxText);
function NyxAgentPermissionName(AValue: TNyxAgentPermission): TNyxText;
function NyxStudioRuntime(const AName: TNyxText): TNyxStudioRuntimeRef;

implementation

uses
  nyx.source.preparation, nyx.studio.projectionediting, nyx.text.search, nyx.editing,
  nyx.catalog, nyx.catalog.labels, nyx.callbacks, nyx.codec, nyx.composition,
  Math, nyx.design.tokens, nyx.tabs, nyx.selection, nyx.data.toolbar,
  nyx.studio.edits, nyx.studio.callbackedits,
  nyx.studio.handleredits, nyx.studio.stateedits, nyx.state, nyx.binding,
  nyx.binding.types, nyx.contract, nyx.collections, nyx.collections.view.types,
  nyx.collections.selection, nyx.collections.query, nyx.collections.query.preview,
  nyx.collections.bindings, nyx.codegen,
  nyx.studio.collectionedits, nyx.studio.importedits,
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
  FPermissionGeneration := AOrigin.FPermissionGeneration;
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
  FFileReviews := Copy(AOrigin.FFileReviews);
  FFileReviewSerial := AOrigin.FFileReviewSerial;
  FProjectImports := Copy(AOrigin.FProjectImports);
  FProjectUploads := Copy(AOrigin.FProjectUploads);
  FProjectCandidates := Copy(AOrigin.FProjectCandidates);
  FProjectImportSerial := AOrigin.FProjectImportSerial;
  FProjectReviewSerial := AOrigin.FProjectReviewSerial;
  FProjectExportRevision := AOrigin.FProjectExportRevision;
  FSourceRoutineIndex := AOrigin.FSourceRoutineIndex;
  FSourceRoutineOwner := AOrigin.FSourceRoutineOwner;
  FSourceRoutineRevision := AOrigin.FSourceRoutineRevision;
  FSourceRoutineKind := AOrigin.FSourceRoutineKind;
  FSourceInspection := AOrigin.FSourceInspection;
  FSourceInspectionRevision := AOrigin.FSourceInspectionRevision;
  FSourceSymbols := AOrigin.FSourceSymbols;
  FSourceSymbolsOwner := AOrigin.FSourceSymbolsOwner;
  FSourceSymbolsRevision := AOrigin.FSourceSymbolsRevision;
  FSourceSymbolsKind := AOrigin.FSourceSymbolsKind;
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
  FFileReviews := nil;
  FProjectExportRevision := 0;
  FProjectExportPair := NyxProjectPair('', '');
  FProjectExportPacket := '';
  FSourceRoutineIndex := nil;
  FSourceRoutineRevision := 0;
  FSourceInspection := nil;
  FSourceInspectionRevision := 0;
  FSourceSymbols := nil;
  FSourceSymbolsRevision := 0;
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
    NyxField('pendingDraft', NyxData(FSession.SourceDraftPending)),
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
  LScope := TextArgument(AArguments, 'scope', 'pages');

  if (LScope = 'diagrams') or (LScope = 'diagram-vertices') or (LScope = 'diagram-edges') then
  begin
    Exit(DiagramQuery(AArguments));
  end;
  NyxAgentFields(AArguments, '|parent|scope|offset|limit|expectedRevision|');

  if NyxAgentHas(AArguments, 'expectedRevision') then
  begin
    RequireRevision(AArguments);
  end;
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

function TNyxAgentSession.CanvasDetails(ANode: TNyxNode;
  ALayerOffset, ALayerLimit, AItemOffset, AItemLimit: Integer; ALocal: Boolean;
  const AArguments: TNyxDataValue): TNyxDataValue;
const
  CGridStyles: array[TNyxCanvasGridStyle] of TNyxText = ('none', 'dots', 'lines');
  CSnapModes: array[TNyxCanvasSnapMode] of TNyxText = ('free', 'grid');
var
  LDefinition: INyxCanvasDefinition;
  LLayers: array of TNyxDataValue;
  LItems: array of TNyxDataValue;
  LLayer: TNyxCanvasLayerInfo;
  LItem: TNyxCanvasDeclarationItem;
  LCamera: TNyxCanvasCamera;
  LGrid: TNyxCanvasGrid;
  LIndex: Integer;
  LCount: Integer;
  LContext: TNyxNode;
  LProjection: TNyxNode;
  LLocalDeclared: Boolean;
  LConnections: TNyxCanvasRouteChanges;
  LConnectionIndex: Integer;
  LConnectionCount: Integer;
  LGraphLayout: INyxCanvasDefinitionGraphLayout;
  LGraphNodeTotal: Integer;
  LFields: array of TNyxDataField;

  { Saved precision is opt-in and independently paged. Ruler values are bounded;
    guide windows never expand into a whole document or live viewport query. }
  function PrecisionDetails: TNyxDataValue;
  var
    LOptions: TNyxCanvasPrecisionOptions;
    LOffset: Integer;
    LLimit: Integer;
    LCount: Integer;
    LIndex: Integer;
    LItems: array of TNyxDataValue;
  begin
    LOptions := NyxCanvasPrecisionOptionsOf(LDefinition);
    LOffset := IntegerArgument(AArguments, 'canvasGuideOffset', 0, 0, 100000);
    LLimit := IntegerArgument(AArguments, 'canvasGuideLimit', 10, 1, 50);
    LCount := Min(LLimit, Max(0, LOptions.Count - LOffset));
    SetLength(LItems, LCount);
    for LIndex := 0 to LCount - 1 do
    begin
      LItems[LIndex] := LOptions.Guide(LOffset + LIndex).ToData;
    end;
    Result := NyxObject([NyxField('defined', NyxData(LOptions.Defined)),
      NyxField('rulers', LOptions.Rulers.ToData),
      NyxField('snapping', NyxData(Ord(LOptions.SnapMode))),
      NyxField('tolerancePixels', NyxData(LOptions.TolerancePixels)),
      NyxField('guideTotal', NyxData(LOptions.Count)), NyxField('guideOffset', NyxData(LOffset)),
      NyxField('guides', NyxArray(LItems)), NyxField('savedOnly', NyxData(True))]);
  end;

  { Inspect only explicit saved positions. A missing graph/step can retain
    dormant intent; this window never resolves runtime cards or computes their
    automatic positions. Filters and offsets address this one immutable plan. }
  function GraphNodeDetails: TNyxDataValue;
  var
    LOffset: Integer;
    LLimit: Integer;
    LGraphFilter: TNyxText;
    LStepFilter: TNyxText;
    LNode: TNyxCanvasGraphNode;
    LItems: array of TNyxDataValue;
    LIndex: Integer;
    LMatched: Integer;
    LCount: Integer;
  begin
    LOffset := IntegerArgument(AArguments, 'canvasGraphNodeOffset', 0, 0, 100000);
    LLimit := IntegerArgument(AArguments, 'canvasGraphNodeLimit', 10, 1, 50);
    LGraphFilter := TextArgument(AArguments, 'canvasGraphNodeGraph', '');
    LStepFilter := TextArgument(AArguments, 'canvasGraphNodeStep', '');

    if LGraphFilter <> '' then
    begin
      LGraphFilter := NyxFlowRef(LGraphFilter).Name;
    end;

    if LStepFilter <> '' then
    begin
      LStepFilter := NyxFlowStep(LStepFilter).Name;
    end;
    SetLength(LItems, LLimit);
    LMatched := 0;
    LCount := 0;
    for LIndex := 0 to LGraphNodeTotal - 1 do
    begin
      LNode := LGraphLayout.GraphNode(LIndex);

      if ((LGraphFilter <> '') and (LNode.Graph.Name <> LGraphFilter)) or
        ((LStepFilter <> '') and (LNode.Step.Name <> LStepFilter)) then
      begin
        Continue;
      end;

      if (LMatched >= LOffset) and (LCount < LLimit) then
      begin
        LItems[LCount] := NyxObject([NyxField('graph', NyxData(LNode.Graph.Name)),
          NyxField('step', NyxData(LNode.Step.Name)), NyxField('x', NyxData(LNode.Position.X)),
          NyxField('y', NyxData(LNode.Position.Y))]);
        Inc(LCount);
      end;
      Inc(LMatched);
    end;
    SetLength(LItems, LCount);
    Result := NyxObject([NyxField('offset', NyxData(LOffset)),
      NyxField('total', NyxData(LMatched)), NyxField('items', NyxArray(LItems)),
      NyxField('savedOnly', NyxData(True)), NyxField('explicitOnly', NyxData(True))]);
  end;

  { Explicit opt-in pages saved masks independently of scene geometry. Route
    windows span the returned mask page, rather than multiplying the limit by
    each control. Empty masks remain visible because they disable inheritance. }
  function ConnectionDetails: TNyxDataValue;
  var
    LOffset: Integer;
    LLimit: Integer;
    LRouteOffset: Integer;
    LRouteLimit: Integer;
    LItemFilter: TNyxText;
    LControlFilter: TNyxText;
    LMatch: Integer;
    LCount: Integer;
    LIndex: Integer;
    LRouteIndex: Integer;
    LRouteCount: Integer;
    LRouteMatch: Integer;
    LCurrentCount: Integer;
    LRoutes: INyxFlowRoutes;
    LItems: array of TNyxDataValue;
    LRouteItems: array of TNyxDataValue;
  begin
    LOffset := IntegerArgument(AArguments, 'canvasConnectionOffset', 0, 0, 100000);
    LLimit := IntegerArgument(AArguments, 'canvasConnectionLimit', 10, 1, 50);
    LRouteOffset := IntegerArgument(AArguments, 'canvasRouteOffset', 0, 0, 100000);
    LRouteLimit := IntegerArgument(AArguments, 'canvasRouteLimit', 4, 1, 16);
    LItemFilter := TextArgument(AArguments, 'canvasConnectionItem', '');
    LControlFilter := TextArgument(AArguments, 'canvasConnectionControl', '');
    SetLength(LItems, LLimit);
    LMatch := 0;
    LCount := 0;
    LRouteCount := 0;
    LRouteMatch := 0;
    for LIndex := 0 to High(LConnections) do
    begin

      if ((LItemFilter <> '') and (LConnections[LIndex].Item.Name <> LItemFilter)) or
        ((LControlFilter <> '') and (LConnections[LIndex].Control.ID <> LControlFilter)) then
      begin
        Continue;
      end;

      if (LMatch >= LOffset) and (LCount < LLimit) then
      begin
        LRoutes := LConnections[LIndex].Routes;
        SetLength(LRouteItems, LRouteLimit);
        LCurrentCount := 0;
        for LRouteIndex := 0 to LRoutes.Count - 1 do
        begin

          if (LRouteMatch >= LRouteOffset) and (LRouteCount < LRouteLimit) then
          begin
            LRouteItems[LCurrentCount] := LRoutes.Route(LRouteIndex).ToData;
            Inc(LCurrentCount);
            Inc(LRouteCount);
          end;
          Inc(LRouteMatch);
        end;
        SetLength(LRouteItems, LCurrentCount);
        LItems[LCount] := NyxObject([NyxField('item', NyxData(LConnections[LIndex].Item.Name)),
          NyxField('control', NyxData(LConnections[LIndex].Control.ID)),
          NyxField('routeTotal', NyxData(LRoutes.Count)), NyxField('routes', NyxArray(LRouteItems))]);
        Inc(LCount);
      end;
      Inc(LMatch);
    end;
    SetLength(LItems, LCount);
    Result := NyxObject([NyxField('offset', NyxData(LOffset)), NyxField('total', NyxData(LMatch)),
      NyxField('items', NyxArray(LItems)), NyxField('routeOffset', NyxData(LRouteOffset)),
      NyxField('routeTotal', NyxData(LRouteMatch))]);
  end;

begin
  LLocalDeclared := ANode.HasCanvasScene;
  LContext := RealizeNyxContext(FSession.Document, ANode, LProjection);
  try

    if (LProjection = nil) or (LProjection.ProjectionKind <> NyxKindName(nkCanvas)) then
    begin
      raise ENyxModel.Create('Canvas context requires a canvas control');
    end;

    if not ALocal then
    begin
      ANode := LProjection;
    end;

    if not ANode.CanvasScene.Defined then
    begin
      Exit(NyxObject([NyxField('defined', NyxData(False)),
        NyxField('localDeclared', NyxData(LLocalDeclared))]));
    end;
    LDefinition := FSession.Document.Canvases.Definition(ANode.CanvasScene);
    LConnections := NyxCanvasConnectionChanges(LDefinition);
    LGraphNodeTotal := 0;
    LGraphLayout := nil;

    if Supports(LDefinition, INyxCanvasDefinitionGraphLayout, LGraphLayout) then
    begin
      LGraphNodeTotal := LGraphLayout.GraphNodeCount;
    end;
    LCamera := LDefinition.Camera;
    LGrid := LDefinition.Grid;
    LCount := Min(ALayerLimit, Max(0, LDefinition.LayerCount - ALayerOffset));
    SetLength(LLayers, LCount);
    for LIndex := 0 to LCount - 1 do
    begin
      LLayer := LDefinition.Layers[ALayerOffset + LIndex];
      LLayers[LIndex] := NyxObject([NyxField('name', NyxData(LLayer.Layer.Name)),
        NyxField('visible', NyxData(LLayer.IsVisible)), NyxField('pickable', NyxData(LLayer.IsPickable))]);
    end;
    LCount := Min(AItemLimit, Max(0, LDefinition.ItemCount - AItemOffset));
    SetLength(LItems, LCount);
    for LIndex := 0 to LCount - 1 do
    begin
      LItem := LDefinition.Items[AItemOffset + LIndex];
      LConnectionCount := 0;
      for LConnectionIndex := 0 to High(LConnections) do
      begin

        if LConnections[LConnectionIndex].Item.Name = LItem.Item.Name then
        begin
          Inc(LConnectionCount);
        end;
      end;
      LItems[LIndex] := NyxObject([NyxField('name', NyxData(LItem.Item.Name)),
        NyxField('layer', NyxData(LItem.Placement.Layer.Name)),
        NyxField('rootKind', NyxData(NyxRootKindName(LItem.Content.Kind))),
        NyxField('root', NyxData(LItem.Content.Name)),
        NyxField('left', NyxData(LItem.Placement.Bounds.Left)),
        NyxField('top', NyxData(LItem.Placement.Bounds.Top)),
        NyxField('width', NyxData(LItem.Placement.Bounds.Width)),
        NyxField('height', NyxData(LItem.Placement.Bounds.Height)),
        NyxField('visible', NyxData(LItem.Placement.IsVisible)),
        NyxField('pickable', NyxData(LItem.Placement.IsPickable)),
        NyxField('connectionTotal', NyxData(LConnectionCount))]);
    end;
    Result := NyxObject([NyxField('defined', NyxData(True)),
      NyxField('scene', NyxData(ANode.CanvasScene.Name)),
      NyxField('scaling', NyxData(NyxCanvasScalingName(LDefinition.Scaling))),
      NyxField('initialCamera', NyxCanvasCameraToData(LCamera)),
      NyxField('grid', NyxObject([NyxField('spacing', NyxData(LGrid.Spacing)),
        NyxField('style', NyxData(CGridStyles[LGrid.Style])), NyxField('snap', NyxData(CSnapModes[LGrid.SnapMode])),
        NyxField('originX', NyxData(LGrid.Origin.X)), NyxField('originY', NyxData(LGrid.Origin.Y))])),
      NyxField('layerTotal', NyxData(LDefinition.LayerCount)), NyxField('layerOffset', NyxData(ALayerOffset)),
      NyxField('layers', NyxArray(LLayers)), NyxField('itemTotal', NyxData(LDefinition.ItemCount)),
      NyxField('itemOffset', NyxData(AItemOffset)), NyxField('items', NyxArray(LItems)),
      NyxField('localDeclared', NyxData(LLocalDeclared)),
      NyxField('connectionTotal', NyxData(Length(LConnections))),
      NyxField('graphNodeTotal', NyxData(LGraphNodeTotal))]);

    if NyxAgentHas(AArguments, 'canvasPrecision') and
      AArguments.Field('canvasPrecision').AsBoolean then
    begin
      SetLength(LFields, Result.Count + 1);
      for LIndex := 0 to Result.Count - 1 do
      begin
        LFields[LIndex] := NyxField(Result.Key(LIndex), Result.Field(Result.Key(LIndex)));
      end;
      LFields[High(LFields)] := NyxField('precision', PrecisionDetails);
      Result := NyxObject(LFields);
    end;

    if NyxAgentHas(AArguments, 'canvasGraphNodes') and
      AArguments.Field('canvasGraphNodes').AsBoolean then
    begin
      SetLength(LFields, Result.Count + 1);
      for LIndex := 0 to Result.Count - 1 do
      begin
        LFields[LIndex] := NyxField(Result.Key(LIndex), Result.Field(Result.Key(LIndex)));
      end;
      LFields[High(LFields)] := NyxField('graphNodes', GraphNodeDetails);
      Result := NyxObject(LFields);
    end;

    if NyxAgentHas(AArguments, 'canvasConnections') and
      AArguments.Field('canvasConnections').AsBoolean then
    begin
      SetLength(LFields, Result.Count + 1);
      for LIndex := 0 to Result.Count - 1 do
      begin
        LFields[LIndex] := NyxField(Result.Key(LIndex), Result.Field(Result.Key(LIndex)));
      end;
      LFields[High(LFields)] := NyxField('connections', ConnectionDetails);
      Result := NyxObject(LFields);
    end;
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
  NyxAgentFields(AArguments, '|expectedRevision|id|offset|limit|events|eventOffset|eventLimit|registrationOffset|registrationLimit|routeOffset|routeLimit|keys|textOffset|textLimit|parts|partOffset|partLimit|content|contentOffset|contentLimit|valueDomain|domainScope|domainOffset|domainLimit|tabs|tabOffset|tabLimit|selectionStyle|selectionOffset|selectionLimit|dataToolbar|toolbarScope|toolbarSortOffset|toolbarSortLimit|toolbarKeyOffset|toolbarKeyLimit|canvas|canvasScope|canvasLayerOffset|canvasLayerLimit|canvasItemOffset|canvasItemLimit|canvasConnections|canvasConnectionOffset|canvasConnectionLimit|canvasConnectionItem|canvasConnectionControl|canvasRouteOffset|canvasRouteLimit|canvasPrecision|canvasGuideOffset|canvasGuideLimit|canvasGraphNodes|canvasGraphNodeOffset|canvasGraphNodeLimit|canvasGraphNodeGraph|canvasGraphNodeStep|');

  if NyxAgentHas(AArguments, 'expectedRevision') then
  begin
    RequireRevision(AArguments);
  end;
  LNode := FSession.Document.Find(TextArgument(AArguments, 'id', FSession.SelectedID));

  if NyxAgentHas(AArguments, 'canvasScope') and
    (TextArgument(AArguments, 'canvasScope') <> 'local') and
    (TextArgument(AArguments, 'canvasScope') <> 'effective') then
  begin
    raise ENyxModel.Create('Canvas scope must be local or effective');
  end;

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
        NyxField('allowedPolicies', NyxEventPoliciesData(LEvents[LIndex].AllowedPolicies)),
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

  if NyxAgentHas(AArguments, 'tabs') and AArguments.Field('tabs').AsBoolean then
  begin
    SetLength(LFields, Length(LFields) + 1);
    LFields[High(LFields)] := NyxField('tabs', TabsDetails(LNode,
      IntegerArgument(AArguments, 'tabOffset', 0, 0, 100000),
      IntegerArgument(AArguments, 'tabLimit', 20, 1, 50)));
  end;
  if NyxAgentHas(AArguments, 'selectionStyle') and AArguments.Field('selectionStyle').AsBoolean then
  begin
    SetLength(LFields, Length(LFields) + 1);
    LFields[High(LFields)] := NyxField('selectionStyle', SelectionDetails(LNode,
      IntegerArgument(AArguments, 'selectionOffset', 0, 0, 100000),
      IntegerArgument(AArguments, 'selectionLimit', 20, 1, 50)));
  end;

  if NyxAgentHas(AArguments, 'dataToolbar') and AArguments.Field('dataToolbar').AsBoolean then
  begin
    SetLength(LFields, Length(LFields) + 1);
    LFields[High(LFields)] := NyxField('dataToolbar', DataToolbarDetails(LNode, AArguments));
  end;

  if NyxAgentHas(AArguments, 'canvas') and AArguments.Field('canvas').AsBoolean then
  begin
    SetLength(LFields, Length(LFields) + 1);
    LFields[High(LFields)] := NyxField('canvas', CanvasDetails(LNode,
      IntegerArgument(AArguments, 'canvasLayerOffset', 0, 0, 100000),
      IntegerArgument(AArguments, 'canvasLayerLimit', 10, 1, 50),
      IntegerArgument(AArguments, 'canvasItemOffset', 0, 0, 100000),
      IntegerArgument(AArguments, 'canvasItemLimit', 20, 1, 50),
      TextArgument(AArguments, 'canvasScope', 'effective') = 'local', AArguments));
  end;
  Result := NyxObject(LFields);
end;

{$include nyx.studio.agents.toolbars.inc}

function TNyxAgentSession.SelectionDetails(ANode: TNyxNode; AOffset, ALimit: Integer): TNyxDataValue;
var
  LContext: TNyxNode;
  LProjection: TNyxNode;
  LLocal: TNyxDataValue;
  LParts: array of TNyxDataValue;
  LIndex: Integer;
  LTotal: Integer;
  LCount: Integer;
begin
  LContext := RealizeNyxContext(FSession.Document, ANode, LProjection);
  try

    if not NyxNodeValueDomain(LProjection).Defined then
    begin
      raise ENyxModel.Create('Selection appearance requires a scalar compound');
    end;
    LLocal := NyxNull;

    if ANode.Extensions.Has(NyxExtension(NyxSelectionOptionsKey)) then
    begin
      LLocal := ANode.Extensions.Value(NyxExtension(NyxSelectionOptionsKey));
    end;
    SetLength(LParts, ALimit);
    LTotal := 0;
    LCount := 0;
    for LIndex := 0 to LProjection.Count - 1 do
    begin

      if LProjection.Children[LIndex].Prop(NyxAttributeName(atAction)) = NyxActionName(naSelect) then
      begin

        if (LTotal >= AOffset) and (LCount < ALimit) then
        begin
          { Exact source identities only. Captions/options/variants use existing
            bounded node queries instead of multiplying user text in this page. }
          LParts[LCount] := NyxObject([
            NyxField('id', NyxData(LProjection.Children[LIndex].SourceID))]);
          Inc(LCount);
        end;
        Inc(LTotal);
      end;
    end;

    if LTotal = 0 then
    begin
      raise ENyxModel.Create('Selection appearance requires direct selection parts');
    end;
    SetLength(LParts, LCount);
    Result := NyxObject([NyxField('local', LLocal),
      NyxField('effective', NyxSelectionOptions(LProjection).ToData),
      NyxField('total', NyxData(LTotal)), NyxField('offset', NyxData(AOffset)),
      NyxField('parts', NyxArray(LParts))]);
  finally
    LContext.Free;
  end;
end;

function TNyxAgentSession.TabsDetails(ANode: TNyxNode; AOffset, ALimit: Integer): TNyxDataValue;
var
  LContext: TNyxNode;
  LProjection: TNyxNode;
  LState: TNyxTabState;
  LLocal: TNyxDataValue;
  LChildren: array of TNyxDataValue;
  LIndex: Integer;
  LCount: Integer;
begin
  LContext := RealizeNyxContext(FSession.Document, ANode, LProjection);
  try
    LState := TNyxTabState.Create(LProjection);
    try
      LLocal := NyxNull;

      if ANode.Extensions.Has(NyxExtension(NyxTabsOptionsKey)) then
      begin
        LLocal := ANode.Extensions.Value(NyxExtension(NyxTabsOptionsKey));
      end;
      SetLength(LChildren, ALimit);
      LCount := 0;
      for LIndex := AOffset to LState.Count - 1 do
      begin

        if LCount = ALimit then
        begin
          Break;
        end;
        LChildren[LCount] := NyxObject([
          NyxField('id', NyxData(LProjection.Children[LIndex].SourceID)),
          NyxField('available', NyxData(LState.Entry(LIndex).Available))]);
        Inc(LCount);
      end;
      SetLength(LChildren, LCount);
      Result := NyxObject([NyxField('local', LLocal),
        NyxField('effective', LState.Options.ToData), NyxField('total', NyxData(LState.Count)),
        NyxField('offset', NyxData(AOffset)), NyxField('children', NyxArray(LChildren))]);
    finally
      LState.Free;
    end;
  finally
    LContext.Free;
  end;
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

{$include nyx.studio.agents.logic.inc}
{$include nyx.studio.agents.programs.inc}
{$include nyx.studio.agents.diagrams.inc}

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

function TNyxAgentSession.CallbackReviewIndex(const AArguments: TNyxDataValue;
  const AOwner: TNyxText; AApply: Boolean): Integer;
var
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
  ReadNyxCallbackPatch(LChanges);
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
        (LReview.Field('actor').AsText = AOwner) and
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

  Result := LReviewIndex;
end;

procedure TNyxAgentSession.ConsumeCallbackReview(AIndex: Integer);
var
  LIndex: Integer;
begin

  if AIndex < 0 then
  begin
    Exit;
  end;
  for LIndex := AIndex + 1 to High(FCallbackReviews) do
  begin
    FCallbackReviews[LIndex - 1] := FCallbackReviews[LIndex];
  end;
  SetLength(FCallbackReviews, Length(FCallbackReviews) - 1);
end;

function TNyxAgentSession.PrepareCallbackProposal(const AArguments: TNyxDataValue;
  out ARequest: TNyxStudioDesignRequest; out AProposal: INyxPreparedDesign;
  out ASchemas: INyxSchemaSnapshot; out AResults: TNyxDataValue): TNyxProjectPair;
var
  LEdit: TNyxStudioDesignEdit;
  LGroup: INyxPreparedCallbackGroup;
begin
  Result := FSession.ProjectSnapshot;
  LEdit := Default(TNyxStudioDesignEdit);
  LEdit.Action := sdaCallbacks;
  LEdit.Selection := FSession.SelectedID;
  LEdit.View := FSession.ActiveViewID;
  LEdit.Callbacks := CaptureNyxCallbackBatch(ReadNyxCallbackPatch(AArguments.Field('changes')));
  ASchemas := CaptureNyxSchemas;
  ARequest := FSession.PrepareDesignRequest(LEdit, ASchemas.Revision);
  AProposal := PrepareNyxStudioDesign(ARequest, ASchemas);

  if AProposal.Diagnostic.Defined or not AProposal.RequiresCompilation or
    not Supports(AProposal, INyxPreparedCallbackGroup, LGroup) then
  begin
    raise ENyxProjectConflict.Create('Compiled callback proposal refused: ' +
      AProposal.Diagnostic.Message);
  end;
  AResults := LGroup.CallbackResults;
  BoundContext(WithResults(Summary, AResults, 'callbacks', True));
end;

function TNyxAgentSession.EditCallbacks(const AArguments: TNyxDataValue;
  const AActor: TNyxText; AApply: Boolean): TNyxDataValue;
var
  LIndex: Integer;
  LPatch: INyxCallbackPatch;
  LPair: TNyxProjectPair;
  LResults: TNyxCallbackEditResults;
  LCallbacks: TNyxDataValue;
  LChanges: TNyxDataValue;
  LReviewID: TNyxText;
  LReviewIndex: Integer;
  LRequest: TNyxStudioDesignRequest;
  LProposal: INyxPreparedDesign;
  LSchemas: INyxSchemaSnapshot;
begin
  LReviewIndex := CallbackReviewIndex(AArguments, AActor, AApply);
  LChanges := AArguments.Field('changes');

  if RequiresCompiledTransaction then
  begin

    if AApply then
    begin
      raise ENyxProjectConflict.Create('Executed callback edits require the owning compiler-aware MCP transport');
    end;
    LPair := PrepareCallbackProposal(AArguments, LRequest, LProposal, LSchemas, LCallbacks);
  end
  else
  begin
    { The declarative synchronous contract is retained. The executed review
      above prepares meaning only; it cannot publish without real construction. }
    LPatch := ReadNyxCallbackPatch(LChanges);
    LPair := LPatch.Candidate(FSession, LResults);
    LCallbacks := EncodeNyxCallbackResults(LResults);
    BoundContext(WithResults(Summary, LCallbacks, 'callbacks', True));
  end;

  if AApply then
  begin
    FSession.AdoptProject(LPair);
    ConsumeCallbackReview(LReviewIndex);
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

    if RequiresCompiledTransaction then
    begin
      raise ENyxProjectConflict.Create('Executed handler edits require the owning compiler-aware MCP transport');
    end;
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
    NyxField('pendingDraft', NyxData(FSession.SourceDraftPending))]);
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

{ Public selectors name portable accepted files, never paths or an implicit
  fallback. Main has no name; helper leaves retain their exact extension/case. }
function SourceFileSelection(const AData: TNyxDataValue): TNyxPascalSourceRef;
var
  LKind: TNyxText;
  LName: TNyxText;
begin
  NyxAgentFields(AData, '|kind|name|');
  LKind := TextArgument(AData, 'kind');

  if LKind = 'main' then
  begin

    if AData.Count <> 1 then
    begin
      raise ENyxModel.Create('Main source selection has no helper name');
    end;
    Exit(TNyxPascalSourceRef.Main);
  end;

  if (AData.Count <> 2) or not NyxAgentHas(AData, 'name') then
  begin
    raise ENyxModel.Create('Supporting source selection requires its exact kind and name');
  end;
  LName := TextArgument(AData, 'name');

  if LKind = 'unit' then
  begin

    if (Length(LName) <= 4) or (Copy(LName, Length(LName) - 3, 4) <> '.pas') then
    begin
      raise ENyxModel.Create('A unit source selector requires its exact .pas leaf');
    end;
    Result := TNyxPascalSourceRef.UnitFile(NyxPascalUnit(Copy(LName, 1, Length(LName) - 4)));
  end
  else if LKind = 'include' then
  begin
    Result := TNyxPascalSourceRef.IncludeFile(NyxPascalInclude(LName));
  end
  else
  begin
    raise ENyxModel.Create('Source selection kind must be main, unit or include');
  end;
end;

{ Small round-trippable identities keep discovery independent of source size. }
function SourceFileSelectionData(const AFile: TNyxPascalSourceRef): TNyxDataValue;
var
  LKind: TNyxText;
begin

  if AFile.Role = npsMain then
  begin
    Exit(NyxObject([NyxField('kind', NyxData('main'))]));
  end;
  LKind := 'unit';

  if AFile.Role = npsInclude then
  begin
    LKind := 'include';
  end;
  Result := NyxObject([NyxField('kind', NyxData(LKind)),
    NyxField('name', NyxData(AFile.Name))]);
end;

{ This strict wire decoder is shared by literal admission and compiler capture.
  The immutable patch recaptures ordered scalar ranges and both text budgets. }
function ReadSourceEditPatch(const AChanges: TNyxDataValue): INyxSourcePatch;
var
  LChange: TNyxDataValue;
  LEdits: array of TNyxSourceEdit;
  LIndex: Integer;
begin

  if (AChanges.Kind <> ndArray) or (AChanges.Count < 1) or
    (AChanges.Count > NyxMaximumSourceEdits) then
  begin
    raise ENyxModel.Create('Source edits require 1..16 changes');
  end;
  SetLength(LEdits, AChanges.Count);
  for LIndex := 0 to AChanges.Count - 1 do
  begin
    LChange := AChanges.Item(LIndex);
    NyxAgentFields(LChange, '|offset|expected|replacement|file|');

    if NyxAgentHas(LChange, 'file') and
      (SourceFileSelection(LChange.Field('file')).Role <> npsMain) then
    begin
      raise ENyxModel.Create('Supporting source ranges require project compiler preparation');
    end;

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
  Result := NyxSourcePatch(LEdits);
end;

type
  { Detached typed ranges decoded only at the semantic boundary; caller arrays
    never become editor state and the immutable patch recaptures every value. }
  TSourceFileEdits = array of TNyxSourceFileEdit;
  { Chronological helper intents carry exact typed references and copied text. }
  TSourceFileChanges = array of TNyxPascalFileChange;

{ File groups reuse the same scalar ranges and complete group budgets. Omitted
  selectors retain legacy main semantics; all other identities are explicit. }
function ReadSourceFileEdits(const AChanges: TNyxDataValue): TSourceFileEdits;
var
  LChange: TNyxDataValue;
  LFile: TNyxPascalSourceRef;
  LIndex: Integer;
begin
  Result := nil;

  if (AChanges.Kind <> ndArray) or (AChanges.Count < 1) or
    (AChanges.Count > NyxMaximumSourceEdits) then
  begin
    raise ENyxModel.Create('Source edits require 1..16 changes');
  end;
  SetLength(Result, AChanges.Count);
  for LIndex := 0 to AChanges.Count - 1 do
  begin
    LChange := AChanges.Item(LIndex);
    NyxAgentFields(LChange, '|offset|expected|replacement|file|');

    if not NyxAgentHas(LChange, 'offset') or
      not NyxAgentHas(LChange, 'expected') or not NyxAgentHas(LChange, 'replacement') then
    begin
      raise ENyxModel.Create('Source changes require offset, expected and replacement');
    end;
    LFile := TNyxPascalSourceRef.Main;

    if NyxAgentHas(LChange, 'file') then
    begin
      LFile := SourceFileSelection(LChange.Field('file'));
    end;
    Result[LIndex] := NyxSourceFileEdit(LFile, NyxSourceEdit(
      IntegerArgument(LChange, 'offset', 0, 0, NyxMaximumSourceCharacters),
      TextArgument(LChange, 'expected'), TextArgument(LChange, 'replacement')));
  end;
end;

function ReadSourceProjectEditPatch(const AChanges: TNyxDataValue): INyxSourceProjectPatch;
begin
  Result := NyxSourceProjectPatch(ReadSourceFileEdits(AChanges));
end;

{ Parse membership and its optional related ranges once, before any compiler or
  review effect. Closed selectors prevent main/path/case fallback semantics. }
function ReadSourceMembership(const AArguments: TNyxDataValue;
  out AChanges: TSourceFileChanges): INyxSourceProjectPatch;
var
  LItems: TNyxDataValue;
  LItem: TNyxDataValue;
  LFile: TNyxPascalSourceRef;
  LEdits: TSourceFileEdits;
  LIndex: Integer;
begin
  LItems := AArguments.Field('changes');

  if (LItems.Kind <> ndArray) or (LItems.Count < 1) or
    (LItems.Count > NyxMaximumSourceEdits) then
  begin
    raise ENyxModel.Create('File membership requires 1..16 changes');
  end;
  SetLength(AChanges, LItems.Count);
  for LIndex := 0 to LItems.Count - 1 do
  begin
    LItem := LItems.Item(LIndex);
    NyxAgentFields(LItem, '|op|file|text|');
    LFile := SourceFileSelection(LItem.Field('file'));

    if LFile.Role = npsMain then
    begin
      raise ENyxModel.Create('Main source cannot be created or removed');
    end;

    if TextArgument(LItem, 'op') = 'create' then
    begin

      if LFile.Role = npsUnit then
      begin
        AChanges[LIndex] := TNyxPascalFileChange.CreateUnit(
          NyxPascalUnit(Copy(LFile.Name, 1, Length(LFile.Name) - 4)));
      end
      else
      begin
        AChanges[LIndex] := TNyxPascalFileChange.CreateInclude(NyxPascalInclude(LFile.Name));
      end;

      if NyxAgentHas(LItem, 'text') then
      begin
        AChanges[LIndex] := AChanges[LIndex].WithText(TextArgument(LItem, 'text'));
      end;
    end
    else if TextArgument(LItem, 'op') = 'remove' then
    begin

      if NyxAgentHas(LItem, 'text') then
      begin
        raise ENyxModel.Create('Removal cannot supply creation text');
      end;
      AChanges[LIndex] := TNyxPascalFileChange.Remove(LFile);
    end
    else
    begin
      raise ENyxModel.Create('File membership op must be create or remove');
    end;
  end;
  LEdits := nil;

  if NyxAgentHas(AArguments, 'edits') then
  begin
    LEdits := ReadSourceFileEdits(AArguments.Field('edits'));
  end;
  Result := NyxSourceProjectMutation(AChanges, LEdits);
end;

{ Exact wire group, excluding the apply mode/receipt and private review ticket.
  Omitted related ranges normalize to an empty array for equivalent reviews. }
function SourceMembershipKey(const AArguments: TNyxDataValue): TNyxText;
var
  LEdits: TNyxDataValue;
begin
  LEdits := NyxArray([]);

  if NyxAgentHas(AArguments, 'edits') then
  begin
    LEdits := AArguments.Field('edits');
  end;
  Result := NyxObject([NyxField('changes', AArguments.Field('changes')),
    NyxField('edits', LEdits)]).ToJSON;
end;

function TNyxAgentSession.ReviewSourceFiles(const AArguments: TNyxDataValue;
  const AOwner: TNyxText): TNyxDataValue;
var
  LChanges: TSourceFileChanges;
  LInputs: INyxPascalProject;
  LWarnings: array of TNyxDataValue;
  LCount: Integer;
  LIndex: Integer;
  LID: TNyxText;
begin
  NyxAgentFields(AArguments, '|mode|expectedRevision|changes|edits|');
  RequireRevision(AArguments);

  if FSession.SourceDraftsPending then
  begin
    raise ENyxStudioDraftResolutionRequired.Create('Resolve all Pascal drafts before file review');
  end;
  ReadSourceMembership(AArguments, LChanges);
  LInputs := FSession.ProjectSnapshot.Files.Open(FSession.Source);

  if LInputs = nil then
  begin
    LInputs := NewNyxPascalProject(NyxPascalUnit(NyxCompanionUnitName(FSession.Source)), FSession.Source);
  end;
  { Validate inventory independently of still-used dependencies. Review names
    every removal before final source preparation/compilation may refuse it. }
  LInputs := NyxPascalFileChanges(LChanges).Stage(LInputs);
  SetLength(LWarnings, Length(LChanges));
  LCount := 0;
  for LIndex := 0 to High(LChanges) do
  begin

    if LChanges[LIndex].Kind = npfcRemove then
    begin
      FSession.ReviewFileRemoval(LChanges[LIndex].FileReference);
      LWarnings[LCount] := SourceFileSelectionData(LChanges[LIndex].FileReference);
      Inc(LCount);
    end;
  end;

  if (LCount = 0) or (AOwner = '') or (FFileReviewSerial = High(Integer)) then
  begin
    raise ENyxModel.Create('Removal review requires a transport owner and at least one existing removal');
  end;
  SetLength(LWarnings, LCount);
  LID := 'file-review-' + IntToStr(FFileReviewSerial + 1);
  Result := NyxObject([NyxField('revision', NyxData(FRevision)),
    NyxField('reviewID', NyxData(LID)), NyxField('removals', NyxArray(LWarnings)),
    NyxField('warning', NyxData('These files will be removed. Repair retained uses/include references in the same group. Undo restores the complete project.'))]);
  BoundContext(Result);

  if Length(FFileReviews) = 8 then
  begin
    for LIndex := 1 to High(FFileReviews) do
    begin
      FFileReviews[LIndex - 1] := FFileReviews[LIndex];
    end;
    SetLength(FFileReviews, 7);
  end;
  Inc(FFileReviewSerial);
  LIndex := Length(FFileReviews);
  SetLength(FFileReviews, LIndex + 1);
  FFileReviews[LIndex] := NyxObject([NyxField('id', NyxData(LID)),
    NyxField('owner', NyxData(AOwner)), NyxField('revision', NyxData(FRevision)),
    NyxField('group', NyxData(SourceMembershipKey(AArguments)))]);
end;

procedure TNyxAgentSession.RequireFileReview(const AArguments: TNyxDataValue;
  const AOwner: TNyxText);
var
  LChanges: TNyxDataValue;
  LIndex: Integer;
  LRequired: Boolean;
  LID: TNyxText;
begin
  LRequired := False;
  LChanges := AArguments.Field('changes');
  for LIndex := 0 to LChanges.Count - 1 do
  begin
    LRequired := LRequired or (TextArgument(LChanges.Item(LIndex), 'op') = 'remove');
  end;

  if not LRequired then
  begin

    if NyxAgentHas(AArguments, 'reviewID') then
    begin
      raise ENyxModel.Create('Creation-only groups do not take a removal reviewID');
    end;
    Exit;
  end;
  LID := TextArgument(AArguments, 'reviewID');
  for LIndex := 0 to High(FFileReviews) do
  begin

    if (FFileReviews[LIndex].Field('id').AsText = LID) and
      (FFileReviews[LIndex].Field('owner').AsText = AOwner) and
      (FFileReviews[LIndex].Field('revision').AsInteger = FRevision) and
      (FFileReviews[LIndex].Field('group').AsText = SourceMembershipKey(AArguments)) then
    begin
      Exit;
    end;
  end;
  raise ENyxModel.Create('Removal needs a current owner/revision/exact-group reviewID');
end;

{$I nyx.studio.agents.projectroutines.inc}
{$I nyx.studio.agents.completion.inc}

function TNyxAgentSession.SourceFiles(const AArguments: TNyxDataValue): TNyxDataValue;
var
  LKind: TNyxPascalSourceSnapshotKind;
  LOffset: Integer;
  LLimit: Integer;
  LCount: Integer;
  LIndex: Integer;
  LFile: TNyxPascalSourceRef;
  LItems: array of TNyxDataValue;
  LPending: Boolean;
begin
  NyxAgentFields(AArguments, '|mode|expectedRevision|snapshot|offset|limit|');
  LKind := SourceReadKind(AArguments);
  LOffset := IntegerArgument(AArguments, 'offset', 0, 0, 4096);
  LLimit := IntegerArgument(AArguments, 'limit', 20, 1, 50);
  LCount := FSession.SourceFileCount;

  if LOffset > LCount then
  begin
    raise ENyxModel.Create('Source file offset is outside the accepted inventory');
  end;

  if LLimit > LCount - LOffset then
  begin
    LLimit := LCount - LOffset;
  end;
  SetLength(LItems, LLimit);
  for LIndex := 0 to LLimit - 1 do
  begin
    LFile := FSession.SourceFile(LOffset + LIndex);
    LPending := FSession.SourceDraftPending;

    if LFile.Role <> npsMain then
    begin
      LPending := FSession.FileDrafts.Contains(LFile);
    end;
    LItems[LIndex] := NyxObject([
      NyxField('file', SourceFileSelectionData(LFile)),
      NyxField('title', NyxData(FSession.SourceFileTitle(LFile))),
      NyxField('pendingDraft', NyxData(LPending))]);
  end;
  Result := NyxObject([NyxField('revision', NyxData(FRevision)),
    NyxField('offset', NyxData(LOffset)), NyxField('total', NyxData(LCount)),
    NyxField('nextOffset', NyxData(LOffset + LLimit)),
    NyxField('files', NyxArray(LItems)),
    NyxField('pendingDrafts', NyxData(FSession.SourceDraftsPending))]);
  Result := SourceReadContext(Result, AArguments, LKind, nil);
end;

function TNyxAgentSession.SourceUnit(const AArguments: TNyxDataValue;
  AApply: Boolean): TNyxDataValue;
var
  LKind: TNyxPascalSourceSnapshotKind;
  LChanges: TNyxDataValue;
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
  LFile: TNyxPascalSourceRef;
  LPending: Boolean;
begin

  if AApply then
  begin
    NyxAgentFields(AArguments, '|mode|expectedRevision|operationId|changes|');
    RequireRevision(AArguments);

    if SourceEditRequiresCompiler(AArguments) then
    begin
      raise ENyxProjectConflict.Create(
        'Executed source edits require the owning compiler-aware MCP transport');
    end;
    LChanges := AArguments.Field('changes');
    LPair := ReadSourceEditPatch(LChanges).Candidate(FSession.ProjectSnapshot);
    Result := NyxObject([NyxField('changes', NyxData(LChanges.Count))]);
    { Preflight the bounded receipt before the sole accepted-session publication. }
    BoundContext(WithResults(Summary, Result, 'sourceEdit', True));
    FSession.AdoptProject(LPair);
    Exit;
  end;
  NyxAgentFields(AArguments, '|mode|expectedRevision|snapshot|offset|line|count|file|');

  if NyxAgentHas(AArguments, 'offset') and NyxAgentHas(AArguments, 'line') then
  begin
    raise ENyxModel.Create('Choose source scalar offset or source line, not both');
  end;

  LKind := SourceReadKind(AArguments);
  LOffset := IntegerArgument(AArguments, 'offset', 0, 0, NyxMaximumSourceCharacters);
  LCount := IntegerArgument(AArguments, 'count', 2048, 1, 4096);
  LFile := TNyxPascalSourceRef.Main;

  if NyxAgentHas(AArguments, 'file') then
  begin
    LFile := SourceFileSelection(AArguments.Field('file'));
  end;

  if LKind = pssEditing then
  begin
    LSource := SourceInspection(LKind).Text(LFile);
  end
  else
  begin
    LSource := FSession.SourceFileText(LFile);
  end;
  LPending := FSession.SourceDraftPending;

  if LFile.Role <> npsMain then
  begin
    LPending := FSession.FileDrafts.Contains(LFile);
  end;
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
      raise ENyxModel.Create('Inspected source contains invalid Unicode');
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
    raise ENyxModel.Create('Source unit offset is outside the inspected source');
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
    NyxField('pendingDraft', NyxData(LPending))]);

  if NyxAgentHas(AArguments, 'file') then
  begin
    Result := WithResults(Result, SourceFileSelectionData(LFile), 'file');
  end;
  Result := SourceReadContext(Result, AArguments, LKind, nil);
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
    NyxField('pendingDraft', NyxData(FSession.SourceDraftPending))]);
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

  if not AApply and NyxAgentHas(AArguments, 'expectedRevision') then
  begin
    RequireRevision(AArguments);
  end;

  if not AApply and (NyxAgentHas(AArguments, 'file') or
    NyxAgentHas(AArguments, 'owner') or NyxAgentHas(AArguments, 'ordinal') or
    NyxAgentHas(AArguments, 'snapshot') or
    not FSession.ProjectSnapshot.Files.IsEmpty) then
  begin
    Exit(ProjectRoutines(AArguments));
  end;

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
    NyxAgentFields(AArguments, '|mode|expectedRevision|routine|offset|count|');
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
      NyxField('pendingDraft', NyxData(FSession.SourceDraftPending))]);
    Exit;
  end;
  NyxAgentFields(AArguments, '|mode|expectedRevision|offset|limit|');
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
    NyxField('pendingDraft', NyxData(FSession.SourceDraftPending))]);
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

  if not AApply and NyxAgentHas(AArguments, 'expectedRevision') then
  begin
    RequireRevision(AArguments);
  end;

  if not AApply and (NyxAgentHas(AArguments, 'file') or
    NyxAgentHas(AArguments, 'owner') or NyxAgentHas(AArguments, 'ordinal') or
    NyxAgentHas(AArguments, 'snapshot') or
    not FSession.ProjectSnapshot.Files.IsEmpty) then
  begin
    Exit(ProjectRoutines(AArguments));
  end;

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
  NyxAgentFields(AArguments, '|mode|expectedRevision|routine|part|offset|count|');
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
    NyxField('pendingDraft', NyxData(FSession.SourceDraftPending))]);
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
      LSourceApply := (TextArgument(AArguments, 'mode') = 'edit-unit') or
        (TextArgument(AArguments, 'mode') = 'edit-files');
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
    else if ATool = 'nyx_logic' then
    begin
      Result := LogicQuery(AArguments);
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

      if TextArgument(AArguments, 'mode') = 'complete' then
      begin
        raise ENyxProjectConflict.Create('Completion requires the owning worker-aware MCP transport');
      end
      else if TextArgument(AArguments, 'mode') = 'review-files' then
      begin
        Result := ReviewSourceFiles(AArguments, LAuthority);
      end
      else if TextArgument(AArguments, 'mode') = 'edit-files' then
      begin
        raise ENyxProjectConflict.Create('File membership requires the owning compiler-aware MCP transport');
      end
      else if TextArgument(AArguments, 'mode') = 'files' then
      begin
        Result := SourceFiles(AArguments);
      end
      else if TextArgument(AArguments, 'mode') = 'symbols' then
      begin
        Result := SourceSymbols(AArguments);
      end
      else if LSourceApply or (TextArgument(AArguments, 'mode') = 'unit') then
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
      InheritPermission(apDisabled);
    end
    else if LPermission = 'readOnly' then
    begin
      InheritPermission(apReadOnly);
    end
    else if LPermission = 'edit' then
    begin
      InheritPermission(apEdit);
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

  if not ARequest.IsFileEdit and LCurrent.Pending and
    (LCurrent.Draft <> AProjection.Source) then
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

function TNyxAgentSession.CaptureProjectOpening(AExpected: Integer;
  const APair: TNyxProjectPair; AResolution: TNyxProjectResolution;
  out ARequest: TNyxStudioProjectRequest;
  out ASchemas: INyxSchemaSnapshot): TNyxProjectPair;
begin
  Result := EditorSourcePair(AExpected);
  ASchemas := CaptureNyxSchemas;
  ARequest := FSession.PrepareProjectRequest(APair, AResolution, ASchemas.Revision,
    pohPairedCommand);
end;

function TNyxAgentSession.CommitProjectOpening(AExpected: Integer;
  const ABaseline: TNyxProjectPair; const AProjection: INyxSourceProjection;
  const ARequest: TNyxStudioProjectRequest;
  const ASchemas: INyxSchemaSnapshot): TNyxDataValue;
var
  LCurrent: TNyxProjectPair;
  LPrepared: INyxPreparedSource;
  LCompletion: TNyxSourceCompletion;
begin
  LCurrent := EditorSourcePair(AExpected);

  if EncodeNyxProject(LCurrent) <> EncodeNyxProject(ABaseline) then
  begin
    raise ENyxProjectConflict.Create('Project Open baseline changed; current work is retained');
  end;

  if (AProjection = nil) or (AProjection.State <> spsExecuted) or
    (AProjection.Source <> ARequest.Source) or (ARequest.History <> pohPairedCommand) then
  begin
    raise ENyxProjectConflict.Create('Project Open needs its exact executed constructor and history policy');
  end;
  LPrepared := PrepareNyxProjectedSource(AProjection, ASchemas);
  LCompletion := FSession.CompleteProjectRequest(ARequest, LPrepared);

  if not (LCompletion in [nscApplied, nscUnchanged]) then
  begin
    raise ENyxProjectConflict.Create('Project Open became stale or refused; current work is retained');
  end;
  Changed;
  FClaimed := True;
  Log('Studio', 'project Open', 'completed');
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

function TNyxAgentSession.CaptureFilePublication(AExpected: Integer;
  const ASource: TNyxText; const AFile: TNyxPascalSourceRef;
  out ARequest: TNyxStudioSourceRequest;
  out ASchemas: INyxSchemaSnapshot): TNyxProjectPair;
begin
  Result := EditorSourcePair(AExpected);
  ASchemas := CaptureNyxSchemas;
  ARequest := FSession.PrepareFileRequest(AFile, ASchemas.Revision);

  if not ARequest.IsFileEdit or (ASource <> ARequest.Source) then
  begin
    raise ENyxProjectConflict.Create('Selected-file publication requires the accepted main source');
  end;
end;

function TNyxAgentSession.CaptureFileChangePublication(AExpected: Integer;
  const ASource: TNyxText; const AChange: TNyxPascalFileChange;
  out ARequest: TNyxStudioSourceRequest;
  out ASchemas: INyxSchemaSnapshot): TNyxProjectPair;
begin
  Result := EditorSourcePair(AExpected);
  ASchemas := CaptureNyxSchemas;
  ARequest := FSession.PrepareFileChangeRequest(AChange, ASchemas.Revision);

  if ASource <> ARequest.Source then
  begin
    raise ENyxProjectConflict.Create('File management requires the accepted main source');
  end;
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

function TNyxAgentSession.RequiresCompiledTransaction: Boolean;
begin
  Result := FSession.AcceptedSourceCheckpoint.Origin = nsoExecuted;
end;

function TNyxAgentSession.TransactionReceipt(const AOwner: TNyxText;
  const AArguments: TNyxDataValue; out AReceipt: TNyxDataValue): Boolean;
begin
  Result := CompiledReceipt('nyx_transaction', AOwner, AArguments, AReceipt);
end;

function TNyxAgentSession.DataReceipt(ADomain: TNyxTransactionDomain;
  const AOwner: TNyxText; const AArguments: TNyxDataValue;
  out AReceipt: TNyxDataValue): Boolean;
begin

  if ADomain = ntdDesign then
  begin
    raise ENyxModel.Create('Focused data receipt requires a data domain');
  end;
  Result := CompiledReceipt(NyxTransactionToolName(ADomain), AOwner, AArguments, AReceipt);
end;

function TNyxAgentSession.CallbackReceipt(const AOwner: TNyxText;
  const AArguments: TNyxDataValue; out AReceipt: TNyxDataValue): Boolean;
begin
  Result := CompiledReceipt('nyx_callbacks', AOwner, AArguments, AReceipt);
end;

function TNyxAgentSession.HandlerReceipt(const AOwner: TNyxText;
  const AArguments: TNyxDataValue; out AReceipt: TNyxDataValue): Boolean;
begin
  Result := CompiledReceipt('nyx_pascal', AOwner, AArguments, AReceipt);
end;

function TNyxAgentSession.SourceEditReceipt(const AOwner: TNyxText;
  const AArguments: TNyxDataValue; out AReceipt: TNyxDataValue): Boolean;
begin

  if (AArguments.Field('mode').AsText <> 'edit-unit') and
    (AArguments.Field('mode').AsText <> 'edit-files') and
    (AArguments.Field('mode').AsText <> 'edit-routines') then
  begin
    raise ENyxModel.Create('Source edit receipts require a supported compiled source mode');
  end;
  Result := CompiledReceipt('nyx_pascal', AOwner, AArguments, AReceipt);
end;

function TNyxAgentSession.CompiledReceipt(const ATool, AOwner: TNyxText;
  const AArguments: TNyxDataValue; out AReceipt: TNyxDataValue): Boolean;
var
  LKey: TNyxText;
  LRequest: TNyxText;
  LOperation: TNyxText;
  LIndex: Integer;
begin
  Result := False;
  AReceipt := NyxNull;

  if (ATool = 'nyx_pascal') or (ATool = 'nyx_state') or
    (ATool = 'nyx_collections') or (ATool = 'nyx_resources') then
  begin

    if ATool = 'nyx_pascal' then
    begin
      NyxAgentFields(AArguments, '|expectedRevision|operationId|mode|changes|edits|reviewID|');
    end
    else
    begin
      NyxAgentFields(AArguments, '|expectedRevision|operationId|mode|changes|');
    end;

    if (AArguments.Field('mode').AsText <> 'apply') and
      not ((ATool = 'nyx_pascal') and
        ((AArguments.Field('mode').AsText = 'edit-unit') or
          (AArguments.Field('mode').AsText = 'edit-files') or
          (AArguments.Field('mode').AsText = 'edit-routines'))) then
    begin
      raise ENyxModel.Create('Compiled focused receipts require their supported mutation mode');
    end;
  end
  else if ATool = 'nyx_callbacks' then
  begin
    NyxAgentFields(AArguments, '|expectedRevision|operationId|mode|changes|reviewID|');

    if AArguments.Field('mode').AsText <> 'apply' then
    begin
      raise ENyxModel.Create('Compiled callback receipts require apply mode');
    end;
  end
  else if ATool = 'nyx_project' then
  begin
    NyxAgentFields(AArguments,
      '|expectedRevision|operationId|mode|import|resolution|admission|');

    if (AArguments.Field('mode').AsText <> 'review-import') or
      (AArguments.Field('admission').AsText <> 'compiler') then
    begin
      raise ENyxModel.Create('Compiled project receipts require explicit compiler review');
    end;
  end
  else
  begin
    NyxAgentFields(AArguments, '|expectedRevision|operationId|operations|');
  end;

  if FPermission <> apEdit then
  begin
    raise ENyxModel.Create('Agent edits require Allow edits in Studio');
  end;
  LOperation := AArguments.Field('operationId').AsText;

  if (AOwner = '') or (LOperation = '') or (Length(LOperation) > 120) then
  begin
    raise ENyxModel.Create('Semantic transaction needs its owning transport and 1..120 character operationId');
  end;
  LKey := NyxObject([NyxField('actor', NyxData(AOwner)),
    NyxField('id', NyxData(LOperation))]).ToJSON;
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
      AReceipt := FReceiptResults[LIndex].Copy;
      Exit(True);
    end;
  end;
end;

procedure TNyxAgentSession.RememberTransactionReceipt(const AOwner: TNyxText;
  const AArguments, AReceipt: TNyxDataValue);
begin
  RememberCompiledReceipt('nyx_transaction', AOwner, AArguments, AReceipt);
end;

procedure TNyxAgentSession.RememberDataReceipt(ADomain: TNyxTransactionDomain;
  const AOwner: TNyxText; const AArguments, AReceipt: TNyxDataValue);
begin

  if ADomain = ntdDesign then
  begin
    raise ENyxModel.Create('Focused data receipt requires a data domain');
  end;
  RememberCompiledReceipt(NyxTransactionToolName(ADomain), AOwner, AArguments, AReceipt);
end;

procedure TNyxAgentSession.RememberCallbackReceipt(const AOwner: TNyxText;
  const AArguments, AReceipt: TNyxDataValue);
begin
  RememberCompiledReceipt('nyx_callbacks', AOwner, AArguments, AReceipt);
end;

procedure TNyxAgentSession.RememberHandlerReceipt(const AOwner: TNyxText;
  const AArguments, AReceipt: TNyxDataValue);
begin
  RememberCompiledReceipt('nyx_pascal', AOwner, AArguments, AReceipt);
end;

procedure TNyxAgentSession.RememberSourceEditReceipt(const AOwner: TNyxText;
  const AArguments, AReceipt: TNyxDataValue);
begin
  RememberCompiledReceipt('nyx_pascal', AOwner, AArguments, AReceipt);
end;

procedure TNyxAgentSession.RememberCompiledReceipt(const ATool, AOwner: TNyxText;
  const AArguments, AReceipt: TNyxDataValue);
var
  LIndex: Integer;
begin
  { The original queued acknowledgement stays exact after publication, just as
    a build receipt does. Status names the final result. Receipt eviction never
    authorizes an old expectedRevision to start another mutation. }

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
  FReceiptKeys[LIndex] := NyxObject([NyxField('actor', NyxData(AOwner)),
    NyxField('id', AArguments.Field('operationId'))]).ToJSON;
  FReceiptRequests[LIndex] := NyxObject([NyxField('tool', NyxData(ATool)),
    NyxField('arguments', AArguments)]).ToJSON;
  FReceiptResults[LIndex] := AReceipt.Copy;
end;

function TNyxAgentSession.CaptureSourceEditPublication(const AArguments: TNyxDataValue;
  const AOwner: TNyxText;
  out ARequest: TNyxStudioSourceRequest;
  out ASchemas: INyxSchemaSnapshot): TNyxProjectPair;
var
  LPatch: INyxSourcePatch;
  LPreparation: INyxSourcePatchPreparation;
  LProjectPatch: INyxSourceProjectPatch;
  LInputs: INyxPascalProject;
  LProjectGroup: Boolean;
  LChanges: TNyxDataValue;
  LIndex: Integer;
  LMembership: TSourceFileChanges;
begin

  if AArguments.Field('mode').AsText = 'edit-files' then
  begin
    NyxAgentFields(AArguments, '|mode|expectedRevision|operationId|changes|edits|reviewID|');
  end
  else
  begin
    NyxAgentFields(AArguments, '|mode|expectedRevision|operationId|changes|');
  end;
  RequireRevision(AArguments);

  if ((AArguments.Field('mode').AsText <> 'edit-unit') and
    (AArguments.Field('mode').AsText <> 'edit-files') and
    (AArguments.Field('mode').AsText <> 'edit-routines')) or
    (FPermission <> apEdit) or not SourceEditRequiresCompiler(AArguments) then
  begin
    raise ENyxModel.Create('Compiled source edits require Allow edits and a captured compiler intent');
  end;
  Result := FSession.ProjectSnapshot;

  if Result.Pending or (Result.FileDrafts.Count > 0) then
  begin
    raise ENyxStudioDraftResolutionRequired.Create(
      'Resolve the pending Pascal draft before source edits');
  end;

  if AArguments.Field('mode').AsText = 'edit-routines' then
  begin
    LInputs := Result.Files.Open(Result.Source);

    if LInputs = nil then
    begin
      LInputs := NewNyxPascalProject(NyxPascalUnit(NyxCompanionUnitName(Result.Source)), Result.Source);
    end;
    LProjectPatch := ReadProjectRoutinePatch(LInputs, AArguments.Field('changes'));
    ASchemas := CaptureNyxSchemas;
    ARequest := FSession.PrepareSourceEditRequest(
      LProjectPatch.PrepareProject(LInputs), ASchemas.Revision);
    Exit;
  end;

  if AArguments.Field('mode').AsText = 'edit-files' then
  begin
    LProjectPatch := ReadSourceMembership(AArguments, LMembership);
    RequireFileReview(AArguments, AOwner);
    LInputs := Result.Files.Open(Result.Source);

    if LInputs = nil then
    begin
      LInputs := NewNyxPascalProject(NyxPascalUnit(NyxCompanionUnitName(Result.Source)), Result.Source);
    end;
    ASchemas := CaptureNyxSchemas;
    ARequest := FSession.PrepareSourceFileChanges(LMembership,
      LProjectPatch.PrepareProject(LInputs), ASchemas.Revision);
    Exit;
  end;
  LChanges := AArguments.Field('changes');
  LProjectGroup := False;
  for LIndex := 0 to LChanges.Count - 1 do
  begin

    if NyxAgentHas(LChanges.Item(LIndex), 'file') and
      (SourceFileSelection(LChanges.Item(LIndex).Field('file')).Role <> npsMain) then
    begin
      LProjectGroup := True;
    end;
  end;
  ASchemas := CaptureNyxSchemas;

  if LProjectGroup then
  begin
    LProjectPatch := ReadSourceProjectEditPatch(LChanges);
    LInputs := Result.Files.Open(Result.Source);

    if LInputs = nil then
    begin
      LInputs := NewNyxPascalProject(NyxPascalUnit(NyxCompanionUnitName(Result.Source)), Result.Source);
    end;
    ARequest := FSession.PrepareSourceEditRequest(
      LProjectPatch.PrepareProject(LInputs), ASchemas.Revision);
    Exit;
  end;
  LPatch := ReadSourceEditPatch(LChanges);

  if not Supports(LPatch, INyxSourcePatchPreparation, LPreparation) then
  begin
    raise ENyxModel.Create('Source patch has no independent text preparation');
  end;
  ARequest := FSession.PrepareSourceEditRequest(
    LPreparation.PrepareSource(Result.Source), ASchemas.Revision);
end;

function TNyxAgentSession.SourceEditRequiresCompiler(const AArguments: TNyxDataValue): Boolean;
var
  LChanges: TNyxDataValue;
  LIndex: Integer;
begin

  if AArguments.Field('mode').AsText = 'edit-files' then
  begin
    Exit(True);
  end;
  Result := RequiresCompiledTransaction;

  if Result then
  begin
    Exit;
  end;
  LChanges := AArguments.Field('changes');
  for LIndex := 0 to LChanges.Count - 1 do
  begin

    if (AArguments.Field('mode').AsText = 'edit-routines') and
      (NyxAgentHas(LChanges.Item(LIndex), 'file') or NyxAgentHas(LChanges.Item(LIndex), 'owner')) then
    begin
      Exit(True);
    end;

    if NyxAgentHas(LChanges.Item(LIndex), 'file') and
      (SourceFileSelection(LChanges.Item(LIndex).Field('file')).Role <> npsMain) then
    begin
      Exit(True);
    end;
  end;
end;

function TNyxAgentSession.CaptureTransactionPublication(const AArguments: TNyxDataValue;
  out ARequest: TNyxStudioDesignRequest; out AProposal: INyxPreparedDesign;
  out ASchemas: INyxSchemaSnapshot): TNyxProjectPair;
var
  LEdit: TNyxStudioDesignEdit;
begin
  RequireRevision(AArguments);

  if (FPermission <> apEdit) or not RequiresCompiledTransaction then
  begin
    raise ENyxModel.Create('Compiled transaction requires Allow edits and an admitted executed baseline');
  end;
  Result := FSession.ProjectSnapshot;
  LEdit := Default(TNyxStudioDesignEdit);
  LEdit.Action := sdaTransaction;
  LEdit.Selection := FSession.SelectedID;
  LEdit.View := FSession.ActiveViewID;
  LEdit.Transaction := CaptureNyxProjectTransaction(
    ReadNyxProjectTransaction(AArguments.Field('operations')));
  ASchemas := CaptureNyxSchemas;
  ARequest := FSession.PrepareDesignRequest(LEdit, ASchemas.Revision);
  AProposal := PrepareNyxStudioDesign(ARequest, ASchemas);

  if AProposal.Diagnostic.Defined or not AProposal.RequiresCompilation then
  begin
    raise ENyxProjectConflict.Create('Compiled transaction proposal refused: ' + AProposal.Diagnostic.Message);
  end;
end;

function TNyxAgentSession.HandlerNoOpReceipt(const ARequest: TNyxStudioDesignRequest;
  const AProposal: INyxPreparedDesign; const AResults: TNyxDataValue): TNyxDataValue;
begin

  if not RequiresCompiledTransaction or (AProposal = nil) or
    not AProposal.Matches(ARequest) or AProposal.Diagnostic.Defined or
    (AProposal.Source <> FSession.Source) or (AProposal.Design <> FSession.Save) then
  begin
    raise ENyxProjectConflict.Create('Unchanged handler receipt requires the exact admitted pair');
  end;
  BoundContext(WithResults(Summary, AResults, 'handlers', True));
  Result := WithResults(Summary, AResults, 'handlers');
end;

function TNyxAgentSession.CaptureHandlerPublication(const AArguments: TNyxDataValue;
  out ARequest: TNyxStudioDesignRequest; out AProposal: INyxPreparedDesign;
  out ASchemas: INyxSchemaSnapshot; out AResults: TNyxDataValue): TNyxProjectPair;
var
  LEdit: TNyxStudioDesignEdit;
  LGroup: INyxPreparedHandlerGroup;
begin
  RequireRevision(AArguments);

  if (FPermission <> apEdit) or not RequiresCompiledTransaction or
    (TextArgument(AArguments, 'mode') <> 'apply') then
  begin
    raise ENyxProjectConflict.Create('Compiled handlers require Allow edits and an admitted executed baseline');
  end;
  Result := FSession.ProjectSnapshot;
  LEdit := Default(TNyxStudioDesignEdit);
  LEdit.Action := sdaHandlers;
  LEdit.Selection := FSession.SelectedID;
  LEdit.View := FSession.ActiveViewID;
  LEdit.Handlers := CaptureNyxHandlerBatch(ReadNyxHandlerPatch(AArguments.Field('changes')));
  ASchemas := CaptureNyxSchemas;
  ARequest := FSession.PrepareDesignRequest(LEdit, ASchemas.Revision);
  AProposal := PrepareNyxStudioDesign(ARequest, ASchemas);

  if AProposal.Diagnostic.Defined or not AProposal.RequiresCompilation or
    not Supports(AProposal, INyxPreparedHandlerGroup, LGroup) then
  begin
    raise ENyxProjectConflict.Create('Compiled handler proposal refused: ' + AProposal.Diagnostic.Message);
  end;
  AResults := LGroup.HandlerResults;
  BoundContext(WithResults(Summary, AResults, 'handlers', True));
end;

function TNyxAgentSession.CaptureCallbackPublication(const AArguments: TNyxDataValue;
  const AOwner: TNyxText; out ARequest: TNyxStudioDesignRequest;
  out AProposal: INyxPreparedDesign; out ASchemas: INyxSchemaSnapshot;
  out AResults: TNyxDataValue): TNyxProjectPair;
begin

  if (FPermission <> apEdit) or not RequiresCompiledTransaction or
    (TextArgument(AArguments, 'mode') <> 'apply') then
  begin
    raise ENyxProjectConflict.Create('Compiled callbacks require Allow edits and an admitted executed baseline');
  end;
  CallbackReviewIndex(AArguments, AOwner, True);
  Result := PrepareCallbackProposal(AArguments, ARequest, AProposal, ASchemas, AResults);
end;

function TNyxAgentSession.CommitCallbackProjection(AExpected: Integer;
  const ABaseline: TNyxProjectPair; const AProjection: INyxSourceProjection;
  const ARequest: TNyxStudioDesignRequest; const AProposal: INyxPreparedDesign;
  const ASchemas: INyxSchemaSnapshot; const AArguments: TNyxDataValue;
  const AOwner: TNyxText): TNyxDataValue;
var
  LReviewIndex: Integer;
begin
  { The actor-bound review is checked again after compilation. Durable failure
    restores this transient ticket together with the pair in the caller's ordinary
    runtime rollback. No cancelled, stale or failed job consumes permission. }
  LReviewIndex := CallbackReviewIndex(AArguments, AOwner, True);
  Result := CommitVisualProjection(AExpected, ABaseline, AProjection, ARequest,
    AProposal, ASchemas);
  ConsumeCallbackReview(LReviewIndex);
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

  { Build admission follows the same deliberate main-draft state as editor
    facts, even when the draft already matches the accepted main unit. }

  if FSession.SourceDraftPending then
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

  if FPermission <> AValue then
  begin

    if FPermissionGeneration = High(Integer) then
    begin
      raise ENyxProjectConflict.Create('Agent permission generation exhausted');
    end;
    Inc(FPermissionGeneration);
    FPermission := AValue;
  end;
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

function TNyxAgentSession.ForkAccepted(AExpected: Integer): TNyxAgentSession;
var
  LFrame: TNyxAgentRecoveryFrame;
begin
  LFrame := Default(TNyxAgentRecoveryFrame);
  { ReviewSeed checks permission/currentness and excludes the parent's draft.
    Capture only the accepted checkpoint, not its command history or private
    transport receipts. CreateRecovered independently admits both exact files. }
  LFrame.Session.Pair := ReviewSeed(AExpected);
  LFrame.Session.AcceptedCheckpoint := FSession.AcceptedSourceCheckpoint;
  LFrame.Revision := 1;
  LFrame.Permission := FPermission;
  LFrame.Claimed := True;

  if FSession.Document.Count > 0 then
  begin
    LFrame.Session.View := FSession.Document.Pages[0].ID;
  end
  else if FSession.Document.ComponentCount > 0 then
  begin
    LFrame.Session.View := FSession.Document.Components[0].ID;
  end;
  LFrame.Session.Selection := LFrame.Session.View;
  Result := TNyxAgentSession.CreateRecovered(LFrame);
end;

function TNyxAgentSession.CurrentPair(const APair: TNyxProjectPair): Boolean;
var
  LCurrent: TNyxProjectPair;
begin
  LCurrent := FSession.ProjectSnapshot;
  Result := not LCurrent.Pending and (LCurrent.Design = APair.Design) and
    (LCurrent.Source = APair.Source) and (LCurrent.Files.Encoded = APair.Files.Encoded);
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
