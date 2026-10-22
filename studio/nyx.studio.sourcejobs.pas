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

unit nyx.studio.sourcejobs;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.types, nyx.responsive, nyx.presentations, nyx.model, nyx.scheduler, nyx.schema, nyx.callbacks,
  nyx.pascal.drafts, nyx.pascal.filechanges,
  nyx.behavior,
  nyx.source.preparation, nyx.studio.sourcecompilation, nyx.studio.session,
  nyx.studio.sourcecompilation.shared, nyx.studio.sourcepublications,
  nyx.studio.projects,
  nyx.studio.edits, nyx.projection.refresh,
  nyx.studio.inspector, nyx.canvas.editor, nyx.studio.collectionintent, nyx.studio.collections,
  nyx.tabs.pages.editor, nyx.programs.declarations, nyx.canvas.adoption,
  nyx.diagram.adoption, nyx.diagram.declarations
  {$ifdef PAS2JS}, JS, Web{$endif};

type
  { Authoritative ordinary-command outcome. Preparation is provisional; Applied
    means paired publication, separately from later presentation callbacks. }
  TNyxSourceCommandState = (nssIdle, nssPreparing, nssApplied, nssRejected,
    nssStale, nssCancelled, nssFailed);
  { Borrowed UI receiver; detach the processor before disposing its host.
    Notifications may enqueue work but cannot free the notifying processor. }
  TNyxSourceCommandChanged = procedure(AState: TNyxSourceCommandState;
    const AMessage: TNyxText) of object;
  { Forward declaration for the revocable port's non-owning processor link. }
  TNyxSourceCommands = class;

  { Retained couriers borrow only this revocable UI port. Detach removes its
    receiver before editor/session retirement. Workers own copied tickets and
    creator snapshots, and never dereference an accepted tree or controller. }
  INyxSourceCommandPort = interface(INyxSharedSourceDispatch)
    ['{8B6CF439-AE07-4C41-A6D8-79C501B85404}']
    procedure Deliver(ASequence: Integer; const ASource: INyxPreparedSource;
      const ADesign: INyxPreparedDesign; const AFailure: TNyxText);
    { Specialized immutable delivery keeps remote admission/context separate
      from local compiler success. It is processed only on the owning UI queue. }
    procedure DeliverShared(ASequence: Integer; const ASource: INyxPreparedSource;
      const ADesign: INyxPreparedDesign;
      AOutcome: TNyxSourcePublicationOutcome;
      const AReceipt: TNyxSourcePublicationReceipt; const AMessage: TNyxText);
    procedure Detach;
  end;

  { Closed operation and value-only inputs. Design targets are captured at the
    UI event; their fresh baseline is captured at dispatch after earlier edits. }
  TNyxEditorSourceKind = (eskPascal, eskDesign, eskProject);
  { UI-owned immutable metadata lease. pas2js forbids COM interfaces in record
    fields. A native worker retains Value in its OWN class field before dispatch;
    it never borrows this holder or depends on its UI lifetime. }
  TNyxEditorSchemaLease = class
  private
    FValue: INyxSchemaSnapshot;
    FAdoption: INyxCanvasRouteAdoptionRequest;
    FDiagramSave: INyxDiagramSaveRequest;
  public
    constructor Create(const AValue: INyxSchemaSnapshot);
    destructor Destroy; override;
    { UI-only single-shot reply retirement. Cancellation resolves the borrowed
      inspector now while native preparation may still drain independently.
      Presentation errors cannot prevent worker/queue ownership retirement. }
    procedure RetireAdoption(const AReason: TNyxText);
    property Value: INyxSchemaSnapshot read FValue;
    { Optional UI-owned reply, never copied to worker/wire records. Retirement
      refuses an unresolved request before releasing its last queue lease. }
    property Adoption: INyxCanvasRouteAdoptionRequest read FAdoption write FAdoption;
    property DiagramSave: INyxDiagramSaveRequest read FDiagramSave write FDiagramSave;
  end;
  { One queue slot owns its metadata holder exactly once. Moves between queue
    and active slots transfer that raw owner; copied portable requests contain
    no UI receiver. RetireJob is the sole holder disposal boundary. }
  TNyxEditorSourceJob = record
    Kind: TNyxEditorSourceKind;
    Sequence: Integer;
    Context: TNyxStudioCommandContext;
    Source: TNyxStudioSourceRequest;
    Project: TNyxStudioProjectRequest;
    Edit: TNyxStudioDesignEdit;
    Design: TNyxStudioDesignRequest;
    Schemas: TNyxEditorSchemaLease;
  end;

  { One processor controller belongs to one session. Native work uses the public
    scheduler and browser work the compiled Pascal worker. Apply coalesces old
    waiting drafts per file; design edits remain FIFO, coalescing only adjacent queued changes to
    the same field. Each admitted command owns one paired Undo step. Up to 64
    waiting intents are retained; excess input refuses without dropping a job.

    Detach is UI-only and revokes callbacks immediately. Native destruction
    drains workers after detachment while servicing UI handoffs; a host must
    detach ALL project contexts before destroying any. Browser destruction
    terminates its worker. Callbacks may enqueue work but must not free this
    controller inline during its own delivery stack. }
  TNyxSourceCommands = class
  private
    FSession: TNyxStudioSession;
    FScheduler: INyxScheduler;
    FPort: INyxSourceCommandPort;
    FChanged: TNyxSourceCommandChanged;
    FState: TNyxSourceCommandState;
    FMessage: TNyxText;
    FSequence: Integer;
    FActive: TNyxEditorSourceJob;
    FQueue: array of TNyxEditorSourceJob;
    FRunning: Boolean;
    FDiscardActive: Boolean;
    FDetached: Boolean;
    FPublishedDesign: Boolean;
    FPublishedProject: Boolean;
    FPublishedCanvasAdoption: Boolean;
    FRestoredDraft: Boolean;
    FPublishedAction: TNyxStudioDesignAction;
    FCanvasCompletion: Boolean;
    FCompletedEdit: TNyxStudioDesignEdit;
    FCompletedContext: TNyxStudioCommandContext;
    FCompletedHandler: TNyxHandlerRef;
    FCompletedFileChange: TNyxPascalFileChange;
    FCompiler: INyxSourceCompiler;
    FProjectCompiler: INyxSourceCompiler;
    FSharedHost: INyxSharedSourceHost;
    FCompilation: INyxSourceCompilation;
    FCompilations: array of INyxSourceCompilation;
    { Project-owned presentation drafts never enter the design/source pair.
      Exact load and scalar-family checks retire them before a later project
      can reuse the same names. Failed rename admission retains the draft. }
    FNameDrafts: array of TNyxStudioPendingField;
    FNameDraftContext: TNyxStudioCommandContext;
    {$ifndef PAS2JS}
    FExecutions: array of INyxExecution;
    {$else}
    FWorker: TJSWorker;
    FWorkerURL: TNyxText;
    FTimeout: NativeInt;
    FReceiveHandler: TJSEventHandler;
    FErrorHandler: TJSEventHandler;
    function Receive(AEvent: TJSEvent): Boolean;
    function WorkerError(AEvent: TJSEvent): Boolean;
    procedure WorkerTimeout;
    procedure RetireWorker;
    {$endif}
    procedure RetainNameDraft(const AEdit: TNyxStudioDesignEdit);
    procedure RetirePublishedNameDraft;
    function GetBusy: Boolean;
    function GetProjectCompilerAvailable: Boolean;
    function GetCanOpenProject: Boolean;
    function NextSequence: Integer;
    procedure Notify(AState: TNyxSourceCommandState; const AMessage: TNyxText;
      ADesign: Boolean = False; AAction: TNyxStudioDesignAction = sdaProperty;
      AProject: Boolean = False; ARestoredDraft: Boolean = False;
      ACanvasAdoption: Boolean = False);
    procedure QueueEdit(const AEdit: TNyxStudioDesignEdit;
      const AAdoption: INyxCanvasRouteAdoptionRequest;
      const ADiagramSave: INyxDiagramSaveRequest = nil);
    procedure SaveDiagram(const AContext: TNyxStudioCommandContext;
      const ARequest: INyxDiagramSaveRequest);
    procedure AdoptCanvas(const AContext: TNyxStudioCommandContext;
      const AView: TNyxControlRef; const ARequest: INyxCanvasRouteAdoptionRequest);
    procedure Enqueue(const AJob: TNyxEditorSourceJob);
    procedure ClearQueue;
    { Retire the active lifetime without discarding other queued intents. }
    procedure CancelActive;
    procedure StartQueued;
    { Continue a detached executed-source proposal without releasing the FIFO
      head. Only the actual compiler's guarded completion can publish it. }
    procedure StartDesignCompilation(const AProposal: INyxPreparedDesign);
    procedure Finish(ASequence: Integer; const ASource: INyxPreparedSource;
      const ADesign: INyxPreparedDesign; const AFailure: TNyxText;
      ACompleted: Boolean = False; ACompletion: TNyxSourceCompletion = nscStale);
    procedure FinishShared(ASequence: Integer; const ASource: INyxPreparedSource;
      const ADesign: INyxPreparedDesign;
      AOutcome: TNyxSourcePublicationOutcome;
      const AReceipt: TNyxSourcePublicationReceipt; const AMessage: TNyxText);
  public
    constructor Create(ASession: TNyxStudioSession;
      AChanged: TNyxSourceCommandChanged; const AWorkerURL: TNyxText = 'nyx_source_worker.js');
    destructor Destroy; override;
    { Capture the exact draft; repeated Apply supersedes prior Apply while design
      commands retain their order and their own fresh paired/draft guards. }
    procedure Apply;
    { Apply only this retained source buffer. Repeated waiting intents coalesce
      per file; other files retain FIFO order and their independent drafts. }
    procedure ApplyFile(const AFile: TNyxPascalSourceRef);
    { Inventory proposals retain FIFO order and never coalesce with typing.
      Removal callers should present a warning before submitting a sealed ticket. }
    procedure ChangeFile(const AChange: TNyxPascalFileChange); overload;
    procedure ChangeFile(const ARequest: TNyxStudioSourceRequest); overload;
    { Retire only this file's waiting/running Apply intent. Other files and
      visual commands retain FIFO ownership; shared uncertainty still freezes
      the ordinary bridge for explicit reconciliation. }
    procedure CancelFile(const AFile: TNyxPascalSourceRef);
    { Install/remove the trusted execution strategy while this context is idle.
      Configuration is host-owned and never saved in the design or history.
      Existing drafts and literal authoring remain valid without a compiler. }
    procedure UseCompiler(const ACompiler: INyxSourceCompiler);
    { Saved/imported source uses compile-only execution, never shared publication
      before its saved design is compared. UseCompiler also supplies this driver;
      shared hosts can configure a separate project driver explicitly while idle.
      No compiler is required for ordinary literal projects or choosing Design. }
    procedure UseProjectCompiler(const ACompiler: INyxSourceCompiler);
    { Ordinary Open/recovery workflow. A configured driver executes the accepted
      saved source, not its unfinished draft, and then admits the complete pair
      under sealed file/load/schema guards. Open deliberately starts new history;
      it is not Apply. Original input belongs to the host until PublishedProject.
      Without a driver, existing strict compiler-independent admission applies. }
    procedure OpenProject(const APair: TNyxProjectPair;
      AResolution: TNyxProjectResolution = nprRequireMatch);
    { Explicit alternative for owning-server compilation. Its coordinator gates
      dispatch/observing acknowledgement and cannot be installed as local-only
      compilation. Configure while idle; detach the borrowed UI port on retirement. }
    procedure UseSharedCompiler(const AHost: INyxSharedSourceHost);
    { Queue closed intent with explicit target/view identities. Full admission
      and source reconciliation run on independently reconstructed owners. }
    procedure Edit(const AEdit: TNyxStudioDesignEdit);
    { Bind an explicit save host to this owned session/load/view. The host
      retains the controller's revocable port, never the controller/session.
      Detach retires its authority; live submissions enter the ordinary FIFO
      transaction/compiler/history path and receive its actual outcome. }
    function CanvasAdoptionHost(const AContext: TNyxStudioCommandContext;
      const AView: TNyxControlRef): INyxCanvasRouteAdoptionHost;
    { Save a copied applied visual graph through this same owning queue. The
      retained host carries load identity and a revocable port, never a session. }
    function DiagramSaveHost(const AContext: TNyxStudioCommandContext): INyxDiagramSaveHost;
    { Synchronously capture immutable proposal values only, then enqueue the
      ordinary isolated design command. Neither worker nor queue retains ANode. }
    procedure CanvasValue(ANode: TNyxNode; APlatform: TNyxPlatform;
      const AMountContext: TNyxStudioCommandContext);
    { Current completed canvas notification's exact field reset, including
      rejected/failed proposals whose physical input must regain the accepted
      value. False for preparation, other commands, retired loads and another
      active view. The adapter copies this value until its deferred paint;
      no tree is exposed and no failed command is reported as published. }
    function CanvasRestore(out ARestore: TNyxProjectionValueRestore): Boolean;
    { Only this notification's successful default creation. Hosts may clear the
      same form name after checking exact text; later user input stays owned. }
    function NewDefaultCreated(out AName: TNyxText): Boolean;
    { Successful callback notification only. The host follows an added stub only
      while its captured owner/view is still selected; removal warnings clear
      only for the exact reviewed registration. False clears every output. }
    function CompletedEvent(out AIntent: TNyxStudioEventIntent;
      out AOwner, AView: TNyxText; out AAddedHandler: TNyxHandlerRef): Boolean;
    { Presentation-only event commands stay immediate; mutations enqueue copied
      typed intent. Neither source generation nor publication occurs inline. }
    function RouteEvents(ANode: TNyxNode; ATrigger: TNyxTrigger;
      const AReview: TNyxCallbackRemoval; out AEffect: TNyxInspectorEffect;
      out ALine: Integer; out ARemoval: TNyxCallbackRemoval): Boolean;
    { Copy pending field/title values for observing chrome. No queued intent,
      accepted document or mutable processor owner is exposed or modified. }
    function PendingDesign: TNyxStudioPendingDesign;
    { Successful ordinary transaction notification only. Returns the last saved
      definition still present in this exact project context; failed/cancelled/
      stale proposals never retarget a local program form. No source is parsed. }
    function CompletedProgram(out AReference: TNyxProgramRef): Boolean;
    { Cancel pending intent, preserving accepted files and source draft. Native
      work may finish but a cancelled result cannot publish. }
    procedure Cancel;
    procedure Detach;
    { Consume Nyx Apply/Restore, scalar state/binding, inspector and structural
      events. Current shell form fields are borrowed only during typed capture.
      Collection capture also submits only scoped, family-qualified values. }
    function Route(ANode: TNyxNode; ATrigger: TNyxTrigger;
      AShellRoot: TNyxNode = nil): Boolean; overload;
    { Compound events retain the parent as source and the physical child as
      origin. Resolve an owned bar action only inside that exact source subtree;
      other editor routes keep their existing source semantics. }
    function Route(ANode: TNyxNode; const AEvent: TNyxEventInfo;
      AShellRoot: TNyxNode): Boolean; overload;
    property State: TNyxSourceCommandState read FState;
    { Defined only during the applied notification; presentation may reset the
      selected file's caret, without retaining the queue slot or compiler. }
    property CompletedFileChange: TNyxPascalFileChange read FCompletedFileChange;
    property Message: TNyxText read FMessage;
    property Busy: Boolean read GetBusy;
    { Current notification's typed design effect. False for source/status-only
      notifications; consumers may follow structural results without parsing
      status text or retargeting another project's callback. }
    property PublishedDesign: Boolean read FPublishedDesign;
    { True only during a successful opening notification; the adapter can retire
      exact imported input and remount the new project's retained controls. }
    property PublishedProject: Boolean read FPublishedProject;
    { True only during actual successful snapshot adoption whose original
      inspector still accepted its reply. Adapters retain that live canvas and
      unfinished forms; other design/source publications keep their old policy. }
    property PublishedCanvasAdoption: Boolean read FPublishedCanvasAdoption;
    { True only inside the explicit Restore accepted notification. The owning
      project can publish its changed draft frame without claiming compiler
      application, replacing the canvas or adding a design/history command.
      Status/cancellation notifications and later reads return False. }
    property RestoredDraft: Boolean read FRestoredDraft;
    property ProjectCompilerAvailable: Boolean read GetProjectCompilerAvailable;
    { Unlike Busy, this also refuses an older load's still-running producer.
      Hosts check it before changing their retained import packet or file metadata. }
    property CanOpenProject: Boolean read GetCanOpenProject;
    property PublishedAction: TNyxStudioDesignAction read FPublishedAction;
    property OnChanged: TNyxSourceCommandChanged read FChanged write FChanged;
  end;

implementation

uses
  SysUtils, nyx.data, nyx.state, nyx.collections, nyx.binding.types, nyx.studio.authoring,
  nyx.studio.sourceprojection, nyx.studio.projectionediting,
  nyx.studio.commands, nyx.codec, nyx.canvas.editing, nyx.canvas.route.types,
  nyx.studio.transactions, nyx.flow.editing, nyx.programs.editing, nyx.diagram.editing,
  nyx.studio.callbackedits
  {$ifndef PAS2JS}, Classes{$endif};

type
  { Private optional submission face on the existing revocable source port.
    Bound hosts carry copied load/view identity and no borrowed session pointer. }
  ICanvasAdoptionSubmission = interface(IInterface)
    ['{6A292A3D-43FB-4E98-8675-AE300A9C9B90}']
    procedure SubmitCanvas(const AContext: TNyxStudioCommandContext;
      const AView: TNyxControlRef; const ARequest: INyxCanvasRouteAdoptionRequest);
  end;

  { Managed host borrows its processor only through the revocable port. Captured
    context/view never retarget a different load; pending tickets stay owned by
    the issuing processor's queue, including inactive native project contexts. }
  TCanvasAdoptionHost = class(TInterfacedObject, INyxCanvasRouteAdoptionHost)
  public
    Port: ICanvasAdoptionSubmission;
    Context: TNyxStudioCommandContext;
    View: TNyxControlRef;
    procedure Submit(const ARequest: INyxCanvasRouteAdoptionRequest);
  end;

  { Private diagram submission/inspection face on the existing revocable UI
    port. Its project/load context cannot retarget a later owner. Source captures
    contain inert accepted data; current-source checks grant no compiler or
    application execution authority. Connected calls belong to the owner's UI. }
  IDiagramSaveSubmission = interface(IInterface)
    ['{08CB4FF5-BDD8-439C-AD7F-54CCB1016003}']
    procedure SubmitDiagram(const AContext: TNyxStudioCommandContext;
      const ARequest: INyxDiagramSaveRequest);
    function InspectDiagram(const AContext: TNyxStudioCommandContext;
      const AReference: TNyxDiagramRef): TNyxDiagramSaveTarget;
    function InspectDiagramSource(const AContext: TNyxStudioCommandContext;
      const AReference: TNyxDiagramRef): TNyxDiagramSourceSnapshot;
    function DiagramSourceCurrent(const AContext: TNyxStudioCommandContext;
      const ASnapshot: TNyxDiagramSourceSnapshot): Boolean;
  end;

  { An independent scheduler rejects worker entry before borrowing the port's
    owner. This reference-counted host retains the revocable port and scheduler,
    never the session itself. Saving and source inspection share one project/load
    context; neither can retarget another session after detach or load change. }
  TDiagramSaveHost = class(TInterfacedObject, INyxDiagramSaveHost, INyxDiagramSaveInspection,
    INyxDiagramSourceInspection)
  public
    Port: IDiagramSaveSubmission;
    Scheduler: INyxScheduler;
    Context: TNyxStudioCommandContext;
    procedure Submit(const ARequest: INyxDiagramSaveRequest);
    function Current(const AReference: TNyxDiagramRef): TNyxDiagramSaveTarget;
    function CurrentSource(const AReference: TNyxDiagramRef): TNyxDiagramSourceSnapshot;
    function IsCurrent(const ASnapshot: TNyxDiagramSourceSnapshot): Boolean;
  end;

  { UI delivery borrows its processor only while the common port remains live. }
  TSourcePort = class(TInterfacedObject, INyxSourceCommandPort, ICanvasAdoptionSubmission,
    IDiagramSaveSubmission)
  public
    Owner: TNyxSourceCommands;
    procedure Deliver(ASequence: Integer; const ASource: INyxPreparedSource;
      const ADesign: INyxPreparedDesign; const AFailure: TNyxText);
    procedure DeliverShared(ASequence: Integer; const ASource: INyxPreparedSource;
      const ADesign: INyxPreparedDesign;
      AOutcome: TNyxSourcePublicationOutcome;
      const AReceipt: TNyxSourcePublicationReceipt; const AMessage: TNyxText);
    procedure Resume;
    procedure Detach;
    procedure SubmitCanvas(const AContext: TNyxStudioCommandContext;
      const AView: TNyxControlRef; const ARequest: INyxCanvasRouteAdoptionRequest);
    procedure SubmitDiagram(const AContext: TNyxStudioCommandContext;
      const ARequest: INyxDiagramSaveRequest);
    function InspectDiagram(const AContext: TNyxStudioCommandContext;
      const AReference: TNyxDiagramRef): TNyxDiagramSaveTarget;
    function InspectDiagramSource(const AContext: TNyxStudioCommandContext;
      const AReference: TNyxDiagramRef): TNyxDiagramSourceSnapshot;
    function DiagramSourceCurrent(const AContext: TNyxStudioCommandContext;
      const ASnapshot: TNyxDiagramSourceSnapshot): Boolean;
  end;
  { One UI courier owns immutable prepared results and a revocable port, never
    the session/controller. Scheduler retirement releases these managed leases. }
  TSourceDelivery = class(TInterfacedObject, INyxWork)
  public
    Port: INyxSourceCommandPort;
    Sequence: Integer;
    Source: INyxPreparedSource;
    Design: INyxPreparedDesign;
    Failure: TNyxText;
    {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
    { Worker-owned timing crosses in this courier as values. Only its UI
      consumer writes diagnostics, avoiding concurrent output/global clocks. }
    HasPreparationTiming: Boolean;
    PreparationMilliseconds: QWord;
    {$endif}
    procedure Execute(const AExecution: INyxExecution);
  end;
  { Retained by the external compiler, not by the editor. Only the revocable
    ordinary port and immutable creator/source values survive controller release. }
  TCompilationPort = class(TInterfacedObject, INyxSourceCompilationPort)
  public
    Port: INyxSourceCommandPort;
    Scheduler: INyxScheduler;
    Schemas: INyxSchemaSnapshot;
    Sequence: Integer;
    ExpectedSource: TNyxText;
    DesignRequest: TNyxStudioDesignRequest;
    DesignProposal: INyxPreparedDesign;
    procedure Complete(const AProjection: INyxSourceProjection;
      const AFailure: TNyxText = '');
  end;
  { Worker-owned copied source/creators and revocable courier; no bridge, accepted
    session or controller survives in the producer callback. Shared delivery is
    posted to the same UI scheduler as ordinary compiler preparation. }
  TSharedCompilationPort = class(TInterfacedObject, INyxSharedSourceCompilationPort)
  public
    Port: INyxSourceCommandPort;
    Scheduler: INyxScheduler;
    Schemas: INyxSchemaSnapshot;
    Sequence: Integer;
    ExpectedSource: TNyxText;
    DesignRequest: TNyxStudioDesignRequest;
    DesignProposal: INyxPreparedDesign;
    procedure Complete(AOutcome: TNyxSourcePublicationOutcome;
      const AProjection: INyxSourceProjection;
      const AReceipt: TNyxSourcePublicationReceipt; const AMessage: TNyxText = '');
  end;
  { Scoped remote-outcome courier carries exact receipt and result values to
    the owning UI queue. Its managed port becomes inert after host detachment. }
  TSharedDelivery = class(TInterfacedObject, INyxWork)
  public
    Port: INyxSourceCommandPort;
    Sequence: Integer;
    Source: INyxPreparedSource;
    Design: INyxPreparedDesign;
    Outcome: TNyxSourcePublicationOutcome;
    Receipt: TNyxSourcePublicationReceipt;
    Message: TNyxText;
    procedure Execute(const AExecution: INyxExecution);
  end;
  {$ifndef PAS2JS}
  { Native worker owns copied requests and a separate schema lease. Its copied
    job never owns the UI metadata holder; only the UI queue retires that holder. }
  TSourcePreparation = class(TInterfacedObject, INyxWork)
  public
    Port: INyxSourceCommandPort;
    Scheduler: INyxScheduler;
    Job: TNyxEditorSourceJob;
    Schemas: INyxSchemaSnapshot;
    procedure Execute(const AExecution: INyxExecution);
  end;
  {$endif}

constructor TNyxEditorSchemaLease.Create(const AValue: INyxSchemaSnapshot);
begin
  inherited Create;
  FValue := AValue;
end;

{$I nyx.studio.sourcejobs.canvas.inc}
{$I nyx.studio.sourcejobs.diagrams.inc}

procedure RetireJob(var AJob: TNyxEditorSourceJob);
begin
  AJob.Schemas.Free;
  AJob := Default(TNyxEditorSourceJob);
end;

procedure TNyxSourceCommands.ClearQueue;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FQueue) do
  begin
    RetireJob(FQueue[LIndex]);
  end;
  FQueue := nil;
end;

procedure TSourcePort.Detach;
begin
  Owner := nil;
end;

procedure TSourcePort.Deliver(ASequence: Integer; const ASource: INyxPreparedSource;
  const ADesign: INyxPreparedDesign; const AFailure: TNyxText);
begin

  if Owner <> nil then
  begin
    Owner.Finish(ASequence, ASource, ADesign, AFailure);
  end;
end;

procedure TSourcePort.DeliverShared(ASequence: Integer; const ASource: INyxPreparedSource;
  const ADesign: INyxPreparedDesign;
  AOutcome: TNyxSourcePublicationOutcome;
  const AReceipt: TNyxSourcePublicationReceipt; const AMessage: TNyxText);
begin

  if Owner <> nil then
  begin
    Owner.FinishShared(ASequence, ASource, ADesign, AOutcome, AReceipt, AMessage);
  end;
end;

procedure TSourcePort.Resume;
begin

  if Owner <> nil then
  begin
    Owner.StartQueued;
  end;
end;

procedure TSharedDelivery.Execute(const AExecution: INyxExecution);
begin
  Port.DeliverShared(Sequence, Source, Design, Outcome, Receipt, Message);
end;

procedure TSharedCompilationPort.Complete(AOutcome: TNyxSourcePublicationOutcome;
  const AProjection: INyxSourceProjection;
  const AReceipt: TNyxSourcePublicationReceipt; const AMessage: TNyxText);
var
  LDelivery: TSharedDelivery;
  LWork: INyxWork;
begin
  LDelivery := TSharedDelivery.Create;
  LWork := LDelivery;
  LDelivery.Port := Port;
  LDelivery.Sequence := Sequence;
  LDelivery.Outcome := AOutcome;
  LDelivery.Receipt := AReceipt;
  LDelivery.Message := AMessage;
  try

    if AOutcome = npoCommitted then
    begin

      if (AProjection = nil) or (AProjection.State <> spsExecuted) or
        (AProjection.Source <> ExpectedSource) then
      begin
        raise ENyxModel.Create('Shared compiler completion differs from its captured construction');
      end;

      if DesignProposal <> nil then
      begin
        LDelivery.Design := PrepareNyxCompiledDesign(DesignRequest, DesignProposal,
          AProjection, Schemas);

        if LDelivery.Design.Diagnostic.Defined then
        begin
          raise ENyxModel.Create(LDelivery.Design.Diagnostic.Message);
        end;
      end
      else
      begin
        LDelivery.Source := PrepareNyxProjectedSource(AProjection, Schemas);
      end;
    end
    else if (AOutcome = npoRefused) and (DesignProposal = nil) and
      (AProjection <> nil) and (AProjection.State <> spsExecuted) and
      (AProjection.Source = ExpectedSource) then
    begin
      { Failure data grants no admission. Retain its exact source diagnostic so
        a selected-helper refusal can be displayed beside that pending buffer. }
      LDelivery.Source := PrepareNyxProjectedSource(AProjection, Schemas);
    end;
  except
    on LException: Exception do
    begin
      LDelivery.Outcome := npoUnconfirmed;
      LDelivery.Message := LException.Message;
    end;
  end;
  Scheduler.PostUI(LWork);
end;

procedure TSourceDelivery.Execute(const AExecution: INyxExecution);
begin

  if not AExecution.Cancelled then
  begin
    {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}

    if HasPreparationTiming then
    begin
      WriteLn('source-command,prepare,', Sequence, ',', PreparationMilliseconds);
    end;
    {$endif}
    Port.Deliver(Sequence, Source, Design, Failure);
  end;
end;

procedure TCompilationPort.Complete(const AProjection: INyxSourceProjection;
  const AFailure: TNyxText);
var
  LDelivery: TSourceDelivery;
  LWork: INyxWork;
begin
  LDelivery := TSourceDelivery.Create;
  LWork := LDelivery;
  LDelivery.Port := Port;
  LDelivery.Sequence := Sequence;
  LDelivery.Failure := AFailure;
  try

    if AFailure = '' then
    begin

      if (AProjection = nil) or (AProjection.Source <> ExpectedSource) then
      begin
        raise ENyxModel.Create('Compiler completion differs from its captured source');
      end;

      if DesignProposal <> nil then
      begin
        LDelivery.Design := PrepareNyxCompiledDesign(DesignRequest,
          DesignProposal, AProjection, Schemas);
      end
      else
      begin
        LDelivery.Source := PrepareNyxProjectedSource(AProjection, Schemas);
      end;
    end;
  except
    on LException: Exception do
    begin
      LDelivery.Failure := LException.Message;
    end;
  end;
  Scheduler.PostUI(LWork);
end;

{$ifndef PAS2JS}
procedure TSourcePreparation.Execute(const AExecution: INyxExecution);
var
  LDelivery: TSourceDelivery;
  LWork: INyxWork;
  {$ifdef NYX_STUDIO_PROFILE}
  LStarted: QWord;
  {$endif}
begin
  LDelivery := TSourceDelivery.Create;
  LWork := LDelivery;
  LDelivery.Port := Port;
  LDelivery.Sequence := Job.Sequence;
  {$ifdef NYX_STUDIO_PROFILE}LStarted := GetTickCount64;{$endif}
  try

    if Job.Kind = eskPascal then
    begin
      LDelivery.Source := PrepareNyxSource(Job.Source.Source, Schemas);
    end
    else
    begin
      LDelivery.Design := PrepareNyxStudioDesign(Job.Design, Schemas);
    end;
  except
    on LException: Exception do
    begin
      LDelivery.Failure := UTF8Encode(UnicodeString(LException.Message));
    end;
  end;
  {$ifdef NYX_STUDIO_PROFILE}
  LDelivery.PreparationMilliseconds := GetTickCount64 - LStarted;
  LDelivery.HasPreparationTiming := True;
  {$endif}

  if not AExecution.Cancelled then
  begin
    Scheduler.PostUI(LWork);
  end;
end;
{$endif}

constructor TNyxSourceCommands.Create(ASession: TNyxStudioSession;
  AChanged: TNyxSourceCommandChanged; const AWorkerURL: TNyxText);
var
  LPort: TSourcePort;
begin
  inherited Create;

  if ASession = nil then
  begin
    raise ENyxModel.Create('Source scheduling requires an owned Studio session');
  end;
  FScheduler := NewNyxScheduler;
  FScheduler.RequireUI;
  FSession := ASession;
  FChanged := AChanged;
  LPort := TSourcePort.Create;
  FPort := LPort;
  LPort.Owner := Self;
  {$ifdef PAS2JS}
  FWorkerURL := AWorkerURL;
  FTimeout := -1;
  FReceiveHandler := Receive;
  FErrorHandler := WorkerError;
  {$endif}
end;

function TNyxSourceCommands.GetBusy: Boolean;
var
  LIndex: Integer;
begin
  Result := FRunning and not FDiscardActive and
    FSession.MatchesCommandContext(FActive.Context);
  for LIndex := 0 to High(FQueue) do
  begin
    Result := Result or FSession.MatchesCommandContext(FQueue[LIndex].Context);
  end;

  if FSharedHost <> nil then
  begin
    Result := Result or FSharedHost.Waiting;
  end;
end;

function TNyxSourceCommands.NextSequence: Integer;
begin

  if FDetached then
  begin
    raise ENyxModel.Create('This source-command context has retired');
  end;

  if FSequence = High(Integer) then
  begin
    raise ENyxModel.Create('Source command sequence is exhausted');
  end;
  Inc(FSequence);
  Result := FSequence;
end;

procedure TNyxSourceCommands.Notify(AState: TNyxSourceCommandState;
  const AMessage: TNyxText; ADesign: Boolean; AAction: TNyxStudioDesignAction;
  AProject: Boolean; ARestoredDraft: Boolean; ACanvasAdoption: Boolean);
begin
  FState := AState;
  FMessage := AMessage;
  FPublishedDesign := (AState = nssApplied) and ADesign;
  FPublishedProject := (AState = nssApplied) and AProject;
  FPublishedCanvasAdoption := (AState = nssApplied) and ACanvasAdoption;
  FRestoredDraft := (AState = nssIdle) and ARestoredDraft;
  FPublishedAction := AAction;
  { A rejected proposal still changed the physical input before admission.
    Restore that exact field without treating the rejected pair as published.
    Every status-only notification clears this transient completion effect. }
  FCanvasCompletion := ADesign and (AAction = sdaCanvasValue) and
    (AState in [nssApplied, nssRejected, nssStale, nssFailed]);

  try

    if not FDetached and Assigned(FChanged) then
    begin
      FChanged(AState, AMessage);
    end;
  finally
    FPublishedProject := False;
    FPublishedCanvasAdoption := False;
    FRestoredDraft := False;
    FCompletedFileChange := Default(TNyxPascalFileChange);
  end;
end;

procedure TNyxSourceCommands.Enqueue(const AJob: TNyxEditorSourceJob);
var
  LCount: Integer;
begin
  LCount := Length(FQueue);

  if LCount >= 64 then
  begin
    raise ENyxModel.Create('Wait for pending editor changes before adding more commands');
  end;
  SetLength(FQueue, LCount + 1);
  FQueue[LCount] := AJob;
end;

procedure TNyxSourceCommands.Apply;
begin
  ApplyFile(TNyxPascalSourceRef.Main);
end;

procedure TNyxSourceCommands.ApplyFile(const AFile: TNyxPascalSourceRef);
var
  LJob: TNyxEditorSourceJob;
  LIndex: Integer;
  LCount: Integer;
begin
  FScheduler.RequireUI;
  LJob := Default(TNyxEditorSourceJob);
  LJob.Kind := eskPascal;
  LJob.Context := FSession.CommandContext;
  LJob.Sequence := NextSequence;
  LJob.Schemas := TNyxEditorSchemaLease.Create(CaptureNyxSchemas);
  try
    LJob.Source := FSession.PrepareFileRequest(AFile, LJob.Schemas.Value.Revision);
    LCount := 0;
    for LIndex := 0 to High(FQueue) do
    begin

      if (FQueue[LIndex].Kind <> eskPascal) or FQueue[LIndex].Source.FileChange.IsDefined or
        not FQueue[LIndex].Source.FileReference.SameFile(AFile) then
      begin
        Inc(LCount);
      end;
    end;

    if (LCount >= 64) and LJob.Source.Changed then
    begin
      raise ENyxModel.Create('Wait for pending editor changes before adding more commands');
    end;
    { Only waiting source drafts are superseded. Refuse excess input BEFORE
      removing intent or suppressing a running result. Design order stays exact. }
    LCount := 0;
    for LIndex := 0 to High(FQueue) do
    begin

      if (FQueue[LIndex].Kind <> eskPascal) or FQueue[LIndex].Source.FileChange.IsDefined or
        not FQueue[LIndex].Source.FileReference.SameFile(AFile) then
      begin
        FQueue[LCount] := FQueue[LIndex];
        Inc(LCount);
      end
      else
      begin
        FQueue[LIndex].Schemas.Free;
      end;
    end;
    SetLength(FQueue, LCount);

    if FRunning and (FActive.Kind = eskPascal) and not FActive.Source.FileChange.IsDefined and
      FActive.Source.FileReference.SameFile(AFile) then
    begin
      FDiscardActive := True;
    end;

    if not LJob.Source.Changed then
    begin
      FSession.DiscardFileDraft(AFile);

      if Busy then
      begin
        Notify(nssPreparing, 'Pending design changes / accepted Pascal retained');
      end
      else
      begin
        Notify(nssApplied, 'Accepted Pascal is current');
      end;
      Exit;
    end;
    Enqueue(LJob);
    LJob.Schemas := nil;
    try
      Notify(nssPreparing, 'Preparing Pascal / current design retained');
    finally
      StartQueued;
    end;
  finally
    LJob.Schemas.Free;
  end;
end;

procedure TNyxSourceCommands.ChangeFile(const AChange: TNyxPascalFileChange);
begin
  FScheduler.RequireUI;
  ChangeFile(FSession.PrepareFileChangeRequest(AChange, NyxSchemaRevision));
end;

procedure TNyxSourceCommands.ChangeFile(const ARequest: TNyxStudioSourceRequest);
var
  LJob: TNyxEditorSourceJob;
begin
  FScheduler.RequireUI;

  if not ARequest.FileChange.IsDefined then
  begin
    raise ENyxModel.Create('Inventory commands require an explicit create/remove ticket');
  end;
  LJob := Default(TNyxEditorSourceJob);
  LJob.Kind := eskPascal;
  LJob.Context := FSession.CommandContext;
  LJob.Sequence := NextSequence;
  LJob.Schemas := TNyxEditorSchemaLease.Create(CaptureNyxSchemas);
  try
    LJob.Source := FSession.RefreshFileRequest(ARequest, LJob.Schemas.Value.Revision);
    Enqueue(LJob);
    LJob.Schemas := nil;
    try
      Notify(nssPreparing, 'Preparing source-file change / current files retained');
    finally
      StartQueued;
    end;
  finally
    LJob.Schemas.Free;
  end;
end;

procedure TNyxSourceCommands.UseCompiler(const ACompiler: INyxSourceCompiler);
begin
  FScheduler.RequireUI;

  if FDetached or FRunning or (Length(FQueue) <> 0) or
    ((FSharedHost <> nil) and FSharedHost.Waiting) then
  begin
    raise ENyxModel.Create('Configure source compilation on an idle editor context');
  end;

  if FSharedHost <> nil then
  begin
    FSharedHost.Attach(nil);
    FSharedHost := nil;
  end;
  FCompiler := ACompiler;
  FProjectCompiler := ACompiler;
end;

function TNyxSourceCommands.GetProjectCompilerAvailable: Boolean;
begin
  Result := FProjectCompiler <> nil;
end;

function TNyxSourceCommands.GetCanOpenProject: Boolean;
begin
  Result := not FDetached and not FRunning and (Length(FQueue) = 0) and
    ((FSharedHost = nil) or not FSharedHost.Waiting);
end;

procedure TNyxSourceCommands.UseProjectCompiler(const ACompiler: INyxSourceCompiler);
begin
  FScheduler.RequireUI;

  if FDetached or FRunning or (Length(FQueue) <> 0) or
    ((FSharedHost <> nil) and FSharedHost.Waiting) then
  begin
    raise ENyxModel.Create('Configure project compilation on an idle editor context');
  end;
  FProjectCompiler := ACompiler;
end;

procedure TNyxSourceCommands.OpenProject(const APair: TNyxProjectPair;
  AResolution: TNyxProjectResolution);
var
  LJob: TNyxEditorSourceJob;
begin
  FScheduler.RequireUI;

  if not CanOpenProject then
  begin
    raise ENyxModel.Create('Wait for pending editor changes before opening a project');
  end;

  if (FProjectCompiler = nil) or (AResolution = nprUseDesign) then
  begin
    FSession.LoadProject(APair, AResolution);
    Notify(nssApplied, 'Project opened with its Pascal companion and retained draft',
      False, sdaProperty, True);
    Exit;
  end;
  LJob := Default(TNyxEditorSourceJob);
  LJob.Kind := eskProject;
  LJob.Context := FSession.CommandContext;
  LJob.Sequence := NextSequence;
  LJob.Schemas := TNyxEditorSchemaLease.Create(CaptureNyxSchemas);
  try
    LJob.Project := FSession.PrepareProjectRequest(APair, AResolution,
      LJob.Schemas.Value.Revision);
    Enqueue(LJob);
    LJob.Schemas := nil;
    try
      Notify(nssPreparing, 'Checking saved Pascal / current project and input retained');
    finally
      StartQueued;
    end;
  finally
    LJob.Schemas.Free;
  end;
end;

procedure TNyxSourceCommands.UseSharedCompiler(const AHost: INyxSharedSourceHost);
begin
  FScheduler.RequireUI;

  if FDetached or FRunning or (Length(FQueue) <> 0) or
    ((FSharedHost <> nil) and FSharedHost.Waiting) then
  begin
    raise ENyxModel.Create('Configure shared source compilation on an idle editor context');
  end;

  if FSharedHost <> nil then
  begin
    FSharedHost.Attach(nil);
  end;
  FCompiler := nil;
  FSharedHost := AHost;

  if FSharedHost <> nil then
  begin
    FSharedHost.Attach(FPort);
  end;
end;

procedure TNyxSourceCommands.Edit(const AEdit: TNyxStudioDesignEdit);
begin
  QueueEdit(AEdit, nil);
end;

procedure TNyxSourceCommands.QueueEdit(const AEdit: TNyxStudioDesignEdit;
  const AAdoption: INyxCanvasRouteAdoptionRequest;
  const ADiagramSave: INyxDiagramSaveRequest);
var
  LJob: TNyxEditorSourceJob;
  LLast: Integer;
  LPending: TNyxStudioPendingDesign;
  LChanges: TNyxCallbackChanges;
  LChange: Integer;
  {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
  { Opt-in UI-thread phase samples contain no source, identity or credentials.
    Ordinary native/browser builds perform no profiling clock/output work. }
  LProfileStarted: QWord;
  {$endif}
begin
  {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
  LProfileStarted := GetTickCount64;
  {$endif}
  FScheduler.RequireUI;
  LPending := PendingDesign;

  if AEdit.Action = sdaCollection then
  begin
    AEdit.Collection.Validate;

    if (AEdit.Collection.Action = scaCreate) and LPending.CollectionCreationPending then
    begin
      raise ENyxCollection.Create('Wait for collection creation before submitting this form again');
    end;

    if LPending.CollectionLocked(AEdit.Collection.Key) or
      (NyxStudioCollectionViewAction(AEdit.Collection.Action) and
        LPending.CollectionViewLocked(AEdit.Selection)) then
    begin
      raise ENyxCollection.Create('Wait for the exact collection/view structure change before editing it');
    end;
  end;

  if (AEdit.Action in [sdaSetStateDefault, sdaRenameStateDefault, sdaRemoveStateDefault]) and
    LPending.StateLocked(AEdit.Name) then
  begin
    raise ENyxState.Create('Wait for this state rename before editing its row');
  end;

  if (AEdit.Action = sdaCreateStateDefault) and LPending.NewDefaultPending then
  begin
    raise ENyxState.Create('Wait for default creation before submitting this form again');
  end;

  if (AEdit.Action = sdaEvent) and LPending.EventLocked(AEdit.Selection,
    AEdit.Event.Trigger, AEdit.Event.Name) then
  begin
    raise ENyxModel.Create('Wait for the pending callback change before editing its event');
  end;

  if AEdit.Action = sdaCallbacks then
  begin
    LChanges := ReadNyxCallbackChanges(AEdit.Callbacks);
    for LChange := 0 to High(LChanges) do
    begin

      if LPending.EventLocked(LChanges[LChange].Owner,
        LChanges[LChange].Event.Trigger, LChanges[LChange].Event.Name) then
      begin
        raise ENyxModel.Create('Wait for the pending callback change before editing its event');
      end;
    end;
  end;

  if (AEdit.Action = sdaSetBinding) and not AEdit.Binding.Cleared and
    LPending.StateLocked(AEdit.Binding.StateName) then
  begin
    raise ENyxState.Create('Wait for this state rename before binding its old name');
  end;
  LJob := Default(TNyxEditorSourceJob);
  LJob.Kind := eskDesign;
  LJob.Context := FSession.CommandContext;

  if AEdit.Action in [sdaCanvasValue, sdaPlacement, sdaResize, sdaPosition] then
  begin
    LJob.Context := AEdit.CanvasContext;
  end;

  if not FSession.MatchesCommandContext(LJob.Context) then
  begin
    raise ENyxModel.Create('Captured editor intent belongs to an earlier project load');
  end;
  LJob.Edit := AEdit;
  LJob.Edit.Binding := AEdit.Binding.Copy;
  LJob.Sequence := NextSequence;
  LJob.Schemas := TNyxEditorSchemaLease.Create(CaptureNyxSchemas);
  try
    LLast := High(FQueue);
    LJob.Schemas.Adoption := AAdoption;
    LJob.Schemas.DiagramSave := ADiagramSave;

    if (LLast >= 0) and (FQueue[LLast].Kind = eskDesign) and
      FSession.MatchesCommandContext(FQueue[LLast].Context) and
      (AEdit.Action in [sdaProperty, sdaTitle, sdaCanvasValue, sdaSetStateDefault,
        sdaSetBinding, sdaEvent, sdaCollection]) and
      (FQueue[LLast].Edit.Action = AEdit.Action) and
      (FQueue[LLast].Edit.Selection = AEdit.Selection) and
      (FQueue[LLast].Edit.View = AEdit.View) and (FQueue[LLast].Edit.Name = AEdit.Name) and
      (FQueue[LLast].Edit.Platform = AEdit.Platform) and
      (FQueue[LLast].Edit.StateInput = AEdit.StateInput) and
      (FQueue[LLast].Edit.Binding.Target = AEdit.Binding.Target) and
      ((AEdit.Action <> sdaEvent) or
        ((AEdit.Event.Action = seaPolicy) and
        (FQueue[LLast].Edit.Event.Action = seaPolicy) and
        (FQueue[LLast].Edit.Event.Trigger = AEdit.Event.Trigger) and
        (FQueue[LLast].Edit.Event.Name.Name = AEdit.Event.Name.Name))) and
      ((AEdit.Action <> sdaCollection) or
        ((AEdit.Collection.Action in [scaDefault, scaCell, scaTitle, scaMode,
          scaScope, scaSelection, scaColumnFixed, scaColumnFlex,
          scaColumnMinimum, scaColumnMaximum, scaPageSize, scaInitialPage]) and
        (FQueue[LLast].Edit.Collection.Action = AEdit.Collection.Action) and
        (FQueue[LLast].Edit.Collection.Key.Name = AEdit.Collection.Key.Name) and
        FQueue[LLast].Edit.Collection.Field.SameReference(AEdit.Collection.Field) and
        (FQueue[LLast].Edit.Collection.Item.Defined = AEdit.Collection.Item.Defined) and
        (not AEdit.Collection.Item.Defined or
          (FQueue[LLast].Edit.Collection.Item.ID = AEdit.Collection.Item.ID)) and
        (FQueue[LLast].Edit.Collection.Input = AEdit.Collection.Input) and
        (FQueue[LLast].Edit.Collection.Projection = AEdit.Collection.Projection))) then
    begin
      FQueue[LLast].Schemas.Free;
      FQueue[LLast] := LJob;
    end
    else
    begin
      Enqueue(LJob);
    end;
    LJob.Schemas := nil;
    try
      {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
      WriteLn('editor-edit,enqueue,', GetTickCount64 - LProfileStarted);
      LProfileStarted := GetTickCount64;
      {$endif}
      Notify(nssPreparing, 'Preparing design / accepted files retained');
      {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
      WriteLn('editor-edit,notify,', GetTickCount64 - LProfileStarted);
      LProfileStarted := GetTickCount64;
      {$endif}
    finally
      StartQueued;
      {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
      WriteLn('editor-edit,start,', GetTickCount64 - LProfileStarted);
      {$endif}
    end;
  finally
    LJob.Schemas.Free;
  end;
end;

procedure TNyxSourceCommands.CanvasValue(ANode: TNyxNode; APlatform: TNyxPlatform;
  const AMountContext: TNyxStudioCommandContext);
begin
  FScheduler.RequireUI;
  Edit(FSession.CaptureCanvasValue(ANode, APlatform, AMountContext));
end;

function TNyxSourceCommands.CanvasRestore(out ARestore: TNyxProjectionValueRestore): Boolean;
begin
  FScheduler.RequireUI;
  ARestore := Default(TNyxProjectionValueRestore);
  Result := FCanvasCompletion and
    FSession.MatchesCommandContext(FCompletedContext) and
    (FCompletedEdit.View = FSession.ActiveViewID) and
    (FCompletedEdit.Name <> '') and (FCompletedEdit.Selection <> '');

  if Result then
  begin
    ARestore := TNyxProjectionValueRestore.ForField(FCompletedEdit.Name,
      FCompletedEdit.Selection);
  end;
end;

function TNyxSourceCommands.CompletedProgram(out AReference: TNyxProgramRef): Boolean;
var
  LOperations: TNyxDataValue;
  LIndex: Integer;
  LReference: TNyxProgramRef;
begin
  FScheduler.RequireUI;
  AReference := Default(TNyxProgramRef);
  Result := False;

  if not FPublishedDesign or (FPublishedAction <> sdaTransaction) or
    not FSession.MatchesCommandContext(FCompletedContext) or
    not FCompletedEdit.Transaction.Defined then
  begin
    Exit;
  end;
  LOperations := FCompletedEdit.Transaction.ToData;
  for LIndex := 0 to LOperations.Count - 1 do
  begin

    if LOperations.Item(LIndex).Field('op').AsText = 'program-define' then
    begin
      LReference := NyxProgramRef(LOperations.Item(LIndex).Field('name').AsText);

      if FSession.Document.Programs.Contains(LReference) then
      begin
        AReference := LReference;
        Result := True;
      end;
    end;
  end;
end;

function TNyxSourceCommands.NewDefaultCreated(out AName: TNyxText): Boolean;
begin
  AName := '';
  Result := FPublishedDesign and (FPublishedAction = sdaCreateStateDefault) and
    FSession.MatchesCommandContext(FCompletedContext);

  if Result then
  begin
    AName := FCompletedEdit.Name;
  end;
end;

procedure TNyxSourceCommands.RetainNameDraft(const AEdit: TNyxStudioDesignEdit);
var
  LIndex: Integer;
  LCount: Integer;
  LKey: TNyxText;
begin

  if not FSession.MatchesCommandContext(FNameDraftContext) then
  begin
    FNameDrafts := nil;
    FNameDraftContext := FSession.CommandContext;
  end;

  if PendingDesign.StateLocked(AEdit.Name) then
  begin
    raise ENyxState.Create('Wait for this state rename before editing its name');
  end;
  LKey := NyxStateKindName(NyxStudioStateInputKind(AEdit.StateInput));

  if FSession.Document.State.Value(AEdit.Name).Kind <> NyxStudioStateInputKind(AEdit.StateInput) then
  begin
    raise ENyxState.Create('State name draft belongs to another scalar family');
  end;
  LCount := Length(FNameDrafts);
  for LIndex := 0 to LCount - 1 do
  begin

    if (FNameDrafts[LIndex].Selection = AEdit.Name) and (FNameDrafts[LIndex].Key = LKey) then
    begin
      FNameDrafts[LIndex].Value := AEdit.Value;
      Exit;
    end;
  end;
  SetLength(FNameDrafts, LCount + 1);
  FNameDrafts[LCount].Selection := AEdit.Name;
  FNameDrafts[LCount].Key := LKey;
  FNameDrafts[LCount].Value := AEdit.Value;
  FNameDrafts[LCount].StateInput := AEdit.StateInput;
end;

procedure TNyxSourceCommands.RetirePublishedNameDraft;
var
  LIndex: Integer;
  LCount: Integer;
begin

  if not FSession.MatchesCommandContext(FNameDraftContext) or
    (FCompletedEdit.Action <> sdaRenameStateDefault) then
  begin
    Exit;
  end;
  LCount := 0;
  for LIndex := 0 to High(FNameDrafts) do
  begin

    if (FNameDrafts[LIndex].Selection <> FCompletedEdit.Name) or
      (FNameDrafts[LIndex].Key <> NyxStateKindName(NyxStudioStateInputKind(FCompletedEdit.StateInput))) or
      (FNameDrafts[LIndex].Value <> FCompletedEdit.Value) then
    begin
      FNameDrafts[LCount] := FNameDrafts[LIndex];
      Inc(LCount);
    end;
  end;
  SetLength(FNameDrafts, LCount);
end;

function TNyxSourceCommands.PendingDesign: TNyxStudioPendingDesign;
var
  LIndex: Integer;
  LDraftCount: Integer;

  procedure Include(const AEdit: TNyxStudioDesignEdit);
  var
    LCount: Integer;
    LChanges: TNyxCallbackChanges;
    LChange: Integer;
  begin

    if AEdit.Action = sdaTitle then
    begin
      Result.TitleDefined := True;
      Result.Title := AEdit.Value;
    end
    else if AEdit.Action = sdaProperty then
    begin
      LCount := Length(Result.Fields);
      SetLength(Result.Fields, LCount + 1);
      Result.Fields[LCount].Selection := AEdit.Selection;
      Result.Fields[LCount].Key := AEdit.Name;
      Result.Fields[LCount].Value := AEdit.Value;
    end
    else if AEdit.Action = sdaCanvasValue then
    begin
      LCount := Length(Result.CanvasValues);
      SetLength(Result.CanvasValues, LCount + 1);
      Result.CanvasValues[LCount].View := AEdit.View;
      Result.CanvasValues[LCount].Owner := AEdit.Selection;
      Result.CanvasValues[LCount].RuntimeID := AEdit.Name;
      Result.CanvasValues[LCount].Value := AEdit.Value;
    end
    else if AEdit.Action = sdaSetStateDefault then
    begin
      LCount := Length(Result.StateValues);
      SetLength(Result.StateValues, LCount + 1);
      Result.StateValues[LCount].Selection := AEdit.Name;
      Result.StateValues[LCount].Key := NyxStateKindName(NyxStudioStateInputKind(AEdit.StateInput));
      Result.StateValues[LCount].Value := AEdit.Value;
      Result.StateValues[LCount].StateInput := AEdit.StateInput;
    end
    else if AEdit.Action = sdaRenameStateDefault then
    begin
      LCount := Length(Result.RenamingStates);
      SetLength(Result.RenamingStates, LCount + 1);
      Result.RenamingStates[LCount] := AEdit.Name;
      LCount := Length(Result.StateNames);
      SetLength(Result.StateNames, LCount + 1);
      Result.StateNames[LCount].Selection := AEdit.Name;
      Result.StateNames[LCount].Key := NyxStateKindName(NyxStudioStateInputKind(AEdit.StateInput));
      Result.StateNames[LCount].Value := AEdit.Value;
      Result.StateNames[LCount].StateInput := AEdit.StateInput;
    end
    else if AEdit.Action = sdaCreateStateDefault then
    begin
      Result.NewDefaultPending := True;
    end
    else if AEdit.Action in [sdaSetBinding, sdaInheritBinding] then
    begin
      LCount := Length(Result.Bindings);
      SetLength(Result.Bindings, LCount + 1);
      Result.Bindings[LCount].Owner := AEdit.Selection;
      Result.Bindings[LCount].Spec := AEdit.Binding.Copy;
      Result.Bindings[LCount].Inherit := AEdit.Action = sdaInheritBinding;
    end
    else if AEdit.Action = sdaEvent then
    begin
      LCount := Length(Result.Events);
      SetLength(Result.Events, LCount + 1);
      Result.Events[LCount].Owner := AEdit.Selection;
      Result.Events[LCount].Intent := AEdit.Event;
    end
    else if AEdit.Action = sdaCallbacks then
    begin
      LChanges := ReadNyxCallbackChanges(AEdit.Callbacks);
      for LChange := 0 to High(LChanges) do
      begin
        LCount := Length(Result.EventLocks);
        SetLength(Result.EventLocks, LCount + 1);
        Result.EventLocks[LCount].Owner := LChanges[LChange].Owner;
        Result.EventLocks[LCount].Trigger := LChanges[LChange].Event.Trigger;
        Result.EventLocks[LCount].Name := LChanges[LChange].Event.Name;

        if LChanges[LChange].Kind = nccPolicy then
        begin
          { Append in active/queue order, alongside ordinary policy commands.
            The latest exact policy wins without fabricating a removal intent. }
          LCount := Length(Result.Events);
          SetLength(Result.Events, LCount + 1);
          Result.Events[LCount].Owner := LChanges[LChange].Owner;
          Result.Events[LCount].Intent := Default(TNyxStudioEventIntent);
          Result.Events[LCount].Intent.Action := seaPolicy;
          Result.Events[LCount].Intent.Trigger := LChanges[LChange].Event.Trigger;
          Result.Events[LCount].Intent.Name := LChanges[LChange].Event.Name;
          Result.Events[LCount].Intent.Policy := LChanges[LChange].Policy;
        end;
      end;
    end
    else if AEdit.Action = sdaCollection then
    begin
      LCount := Length(Result.Collections);
      SetLength(Result.Collections, LCount + 1);
      Result.Collections[LCount].Owner := AEdit.Selection;
      Result.Collections[LCount].Intent := AEdit.Collection;
    end;
  end;

begin
  FScheduler.RequireUI;
  Result := Default(TNyxStudioPendingDesign);

  if not FSession.MatchesCommandContext(FNameDraftContext) then
  begin
    FNameDrafts := nil;
  end;
  { Copy each record explicitly: pas2js arrays/records must not share mutable
    presentation storage with this project's retained draft owner. }
  for LIndex := 0 to High(FNameDrafts) do
  begin

    if FSession.Document.State.Has(FNameDrafts[LIndex].Selection) and
      (NyxStateKindName(FSession.Document.State.Value(FNameDrafts[LIndex].Selection).Kind) =
        FNameDrafts[LIndex].Key) then
    begin
      SetLength(Result.StateNames, Length(Result.StateNames) + 1);
      Result.StateNames[High(Result.StateNames)] := FNameDrafts[LIndex];
    end;
  end;
  { Once an exact row disappears, retire its draft rather than attaching it to
    a subsequently created default with the same spelling and scalar family. }
  LDraftCount := Length(Result.StateNames);
  SetLength(FNameDrafts, LDraftCount);
  for LIndex := 0 to LDraftCount - 1 do
  begin
    FNameDrafts[LIndex] := Result.StateNames[LIndex];
  end;

  if FRunning and not FDiscardActive and (FActive.Kind = eskDesign) and
    FSession.MatchesCommandContext(FActive.Context) then
  begin
    Include(FActive.Edit);
  end;
  for LIndex := 0 to High(FQueue) do
  begin

    if (FQueue[LIndex].Kind = eskDesign) and
      FSession.MatchesCommandContext(FQueue[LIndex].Context) then
    begin
      Include(FQueue[LIndex].Edit);
    end;
  end;
end;

procedure TNyxSourceCommands.StartDesignCompilation(const AProposal: INyxPreparedDesign);
var
  LPort: TCompilationPort;
  LLease: INyxSourceCompilationPort;
  LIndex: Integer;
  LCount: Integer;
  LSharedHost: INyxSharedDesignSourceHost;
  LSharedPort: TSharedCompilationPort;
  LSharedLease: INyxSharedSourceCompilationPort;
begin
  FScheduler.RequireUI;

  if (FCompiler = nil) and (FSharedHost = nil) then
  begin
    raise ENyxModel.Create('Handwritten visual edits require a configured source compiler');
  end;

  if (AProposal = nil) or not AProposal.RequiresCompilation or
    AProposal.Diagnostic.Defined or not AProposal.Matches(FActive.Design) or
    not FSession.MatchesCommandContext(FActive.Context) then
  begin
    raise ENyxModel.Create('Visual compilation requires the active exact proposal');
  end;
  LCount := 0;
  for LIndex := 0 to High(FCompilations) do
  begin

    if FCompilations[LIndex].State in [scsPending, scsRunning] then
    begin
      FCompilations[LCount] := FCompilations[LIndex];
      Inc(LCount);
    end;
  end;
  SetLength(FCompilations, LCount);

  if FSharedHost <> nil then
  begin

    if not Supports(FSharedHost, INyxSharedDesignSourceHost, LSharedHost) then
    begin
      raise ENyxModel.Create('Shared source host does not support visual continuation');
    end;
    LSharedPort := TSharedCompilationPort.Create;
    LSharedLease := LSharedPort;
    LSharedPort.Port := FPort;
    LSharedPort.Scheduler := FScheduler;
    LSharedPort.Schemas := FActive.Schemas.Value;
    LSharedPort.Sequence := FActive.Sequence;
    LSharedPort.ExpectedSource := AProposal.Source;
    LSharedPort.DesignRequest := FActive.Design;
    LSharedPort.DesignProposal := AProposal;
    FCompilation := LSharedHost.StartDesign(FActive.Design, AProposal, LSharedLease);
  end
  else
  begin
    LPort := TCompilationPort.Create;
    LLease := LPort;
    LPort.Port := FPort;
    LPort.Scheduler := FScheduler;
    LPort.Schemas := FActive.Schemas.Value;
    LPort.Sequence := FActive.Sequence;
    LPort.ExpectedSource := AProposal.Source;
    LPort.DesignRequest := FActive.Design;
    LPort.DesignProposal := AProposal;
    FCompilation := StartNyxSourceCompilation(FCompiler, AProposal.Source,
      FActive.Design.Files, LLease);
  end;

  if FCompilation = nil then
  begin
    raise ENyxModel.Create('Visual compiler returned no operation lifetime');
  end;
  SetLength(FCompilations, LCount + 1);
  FCompilations[LCount] := FCompilation;
end;

procedure TNyxSourceCommands.StartQueued;
var
  LIndex: Integer;
  LCompilationPort: TCompilationPort;
  LCompilationLease: INyxSourceCompilationPort;
  LCompilationCount: Integer;
  LSharedPort: TSharedCompilationPort;
  LSharedLease: INyxSharedSourceCompilationPort;
  LPreparationHost: INyxSharedDesignPreparationHost;
  {$ifdef PAS2JS}
  LProposal: INyxPreparedDesign;
  {$endif}
  {$ifndef PAS2JS}
  LWork: TSourcePreparation;
  LLease: INyxWork;
  LCount: Integer;
  {$endif}
begin

  if FDetached or FRunning or (Length(FQueue) = 0) then
  begin
    Exit;
  end;

  if (FSharedHost <> nil) and (FQueue[0].Kind <> eskProject) and not FSharedHost.Ready then
  begin
    Exit;
  end;
  FActive := FQueue[0];
  for LIndex := 1 to High(FQueue) do
  begin
    FQueue[LIndex - 1] := FQueue[LIndex];
  end;
  SetLength(FQueue, Length(FQueue) - 1);
  FDiscardActive := False;
  { Target IDs can exist in an unrelated newly opened project. Refuse the old
    load BEFORE capturing a fresh baseline; exact IDs alone are insufficient.
    Older preparation may drain, but cannot paint pending values in this load. }

  if not FSession.MatchesCommandContext(FActive.Context) then
  begin
    RetireJob(FActive);
    try
      Notify(nssStale, 'Queued change belongs to an earlier project load / current files retained');
    finally
      StartQueued;
    end;
    Exit;
  end;
  try

    if FActive.Kind = eskDesign then
    begin
      FActive.Design := FSession.PrepareDesignRequest(FActive.Edit, FActive.Schemas.Value.Revision);
    end;

    if FActive.Kind = eskPascal then
    begin
      FActive.Source := FSession.RefreshFileRequest(FActive.Source, FActive.Schemas.Value.Revision);

      if FActive.Source.IsFileEdit and (FCompiler = nil) and (FSharedHost = nil) then
      begin
        raise ENyxModel.Create('Configure a project source compiler before applying a supporting file');
      end;
    end;
    FRunning := True;

    if (FActive.Kind = eskDesign) and FActive.Design.RequiresExecution then
    begin

      if Supports(FSharedHost, INyxSharedDesignPreparationHost, LPreparationHost) then
      begin
        LPreparationHost.ReserveDesign(FActive.Design);
      end;

      if (FCompiler = nil) and (FSharedHost = nil) then
      begin
        raise ENyxModel.Create('Configure a source compiler before editing handwritten Pascal visually');
      end;
      {$ifdef PAS2JS}
      { This bounded browser proposal remains local: an opaque admitted
        checkpoint cannot be serialized into the literal parsing worker. Actual
        compilation/execution still uses the configured owning worker service.
        Off-loop proposal preparation remains a performance qualification gap. }
      LProposal := PrepareNyxStudioDesign(FActive.Design, FActive.Schemas.Value);

      if LProposal.Diagnostic.Defined then
      begin
        Finish(FActive.Sequence, nil, LProposal, '');
      end
      else
      begin
        StartDesignCompilation(LProposal);
      end;
      Exit;
      {$endif}
    end;

    if ((FActive.Kind = eskPascal) and ((FCompiler <> nil) or (FSharedHost <> nil))) or
      (FActive.Kind = eskProject) then
    begin
      LCompilationCount := 0;
      for LIndex := 0 to High(FCompilations) do
      begin

        if FCompilations[LIndex].State in [scsPending, scsRunning] then
        begin
          FCompilations[LCompilationCount] := FCompilations[LIndex];
          Inc(LCompilationCount);
        end;
      end;
      SetLength(FCompilations, LCompilationCount);

      if (FActive.Kind = eskPascal) and (FSharedHost <> nil) then
      begin
        LSharedPort := TSharedCompilationPort.Create;
        LSharedLease := LSharedPort;
        LSharedPort.Port := FPort;
        LSharedPort.Scheduler := FScheduler;
        LSharedPort.Schemas := FActive.Schemas.Value;
        LSharedPort.Sequence := FActive.Sequence;
        LSharedPort.ExpectedSource := FActive.Source.Source;

        if FActive.Source.FileChange.IsDefined then
        begin
          FCompilation := StartNyxSharedFileChangeCompilation(FSharedHost,
            FActive.Source, LSharedLease);
        end
        else if FActive.Source.IsFileEdit then
        begin
          FCompilation := StartNyxSharedFileCompilation(FSharedHost,
            FActive.Source, LSharedLease);
        end
        else
        begin
          FCompilation := FSharedHost.Start(FActive.Source.Source, LSharedLease);
        end;
      end
      else
      begin
        LCompilationPort := TCompilationPort.Create;
        LCompilationLease := LCompilationPort;
        LCompilationPort.Port := FPort;
        LCompilationPort.Scheduler := FScheduler;
        LCompilationPort.Schemas := FActive.Schemas.Value;
        LCompilationPort.Sequence := FActive.Sequence;

        if FActive.Kind = eskProject then
        begin
          LCompilationPort.ExpectedSource := FActive.Project.Source;
          FCompilation := StartNyxSourceCompilation(FProjectCompiler,
            FActive.Project.Source, FActive.Project.Files, LCompilationLease);
        end
        else
        begin
          LCompilationPort.ExpectedSource := FActive.Source.Source;
          FCompilation := StartNyxSourceCompilation(FCompiler,
            FActive.Source.Source, FActive.Source.Files, LCompilationLease);
        end;
      end;

      if FCompilation = nil then
      begin
        raise ENyxModel.Create('Source compiler returned no operation lifetime');
      end;
      SetLength(FCompilations, LCompilationCount + 1);
      FCompilations[LCompilationCount] := FCompilation;
      Exit;
    end;
    {$ifdef PAS2JS}
    FWorker := TJSWorker.new(FWorkerURL);
    FWorker.addEventListener('message', FReceiveHandler);
    FWorker.addEventListener('error', FErrorHandler);
    FTimeout := window.setTimeout(@WorkerTimeout, 30000);

    if FActive.Kind = eskPascal then
    begin
      FWorker.postMessage(NyxObject([NyxField('version', NyxData(1)),
        NyxField('source', NyxData(FActive.Source.Source)),
        NyxField('schemas', FActive.Schemas.Value.ToData)]).ToJSON);
    end
    else
    begin
      FWorker.postMessage(NyxObject([NyxField('version', NyxData(2)),
        NyxField('request', FActive.Design.ToData),
        NyxField('schemas', FActive.Schemas.Value.ToData)]).ToJSON);
    end;
    {$else}
    LCount := 0;
    for LIndex := 0 to High(FExecutions) do
    begin

      if (FExecutions[LIndex] <> nil) and
        (FExecutions[LIndex].Status in [nesPending, nesRunning]) then
      begin
        FExecutions[LCount] := FExecutions[LIndex];
        Inc(LCount);
      end;
    end;
    SetLength(FExecutions, LCount);
    LWork := TSourcePreparation.Create;
    LLease := LWork;
    LWork.Port := FPort;
    LWork.Scheduler := FScheduler;
    LWork.Job := FActive;
    LWork.Schemas := FActive.Schemas.Value;
    LWork.Job.Schemas := nil;
    SetLength(FExecutions, LCount + 1);
    FExecutions[LCount] := FScheduler.Submit(LLease, neThreaded);
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}RetireWorker;{$endif}

      if Supports(FSharedHost, INyxSharedDesignPreparationHost, LPreparationHost) then
      begin
        LPreparationHost.ReleaseDesignPreparation;
      end;
      FCompilation := nil;
      FRunning := False;
      RetireJob(FActive);
      try

        if LException is ENyxStudioDraftResolutionRequired then
        begin
          Notify(nssRejected, LException.Message);
        end
        else
        begin
          Notify(nssFailed, 'Editor processor unavailable: ' + LException.Message);
        end;
      finally
        StartQueued;
      end;
      Exit;
    end;
  end;

  if FState <> nssPreparing then
  begin
    Notify(nssPreparing, 'Preparing queued editor change / accepted files retained');
  end;
end;

procedure TNyxSourceCommands.Finish(ASequence: Integer;
  const ASource: INyxPreparedSource; const ADesign: INyxPreparedDesign;
  const AFailure: TNyxText; ACompleted: Boolean; ACompletion: TNyxSourceCompletion);
var
  LCompletion: TNyxSourceCompletion;
  LState: TNyxSourceCommandState;
  LMessage: TNyxText;
  LDesign: Boolean;
  LProject: Boolean;
  LAction: TNyxStudioDesignAction;
  LAdoption: INyxCanvasRouteAdoptionRequest;
  LDiagramSave: INyxDiagramSaveRequest;
  LAdopted: Boolean;
  LPreparationHost: INyxSharedDesignPreparationHost;
  {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
  LAdmissionStarted: QWord;
  LNotificationStarted: QWord;
  {$endif}
begin
  FScheduler.RequireUI;

  if FDetached or not FRunning or (ASequence <> FActive.Sequence) then
  begin
    Exit;
  end;

  if not FDiscardActive and (AFailure = '') and (ADesign <> nil) and
    ADesign.RequiresCompilation and not ADesign.Diagnostic.Defined then
  begin
    try
      StartDesignCompilation(ADesign);
      Exit;
    except
      on LException: Exception do
      begin
        Finish(ASequence, nil, nil, LException.Message);
        Exit;
      end;
    end;
  end;
  { No compiler was started for a diagnostic/no-op preparation. Release only
    that phase; an active compiler or observing acknowledgement stays owned. }

  if Supports(FSharedHost, INyxSharedDesignPreparationHost, LPreparationHost) then
  begin
    LPreparationHost.ReleaseDesignPreparation;
  end;
  FRunning := False;
  FCompilation := nil;

  if not FDiscardActive then
  begin
    {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
    LAdmissionStarted := GetTickCount64;
    {$endif}
    try

      if AFailure <> '' then
      begin
        LState := nssFailed;
        LMessage := AFailure;
      end
      else
      begin

        if ACompleted then
        begin
          LCompletion := ACompletion;
        end
        else if FActive.Kind = eskPascal then
        begin
          LCompletion := FSession.CompleteSourceRequest(FActive.Source, ASource);
        end
        else if FActive.Kind = eskProject then
        begin
          LCompletion := FSession.CompleteProjectRequest(FActive.Project, ASource);
        end
        else
        begin
          LCompletion := FSession.CompleteDesignRequest(FActive.Design, ADesign);
        end;
        case LCompletion of
          nscUnchanged, nscApplied:
            begin
              LState := nssApplied;

              if FActive.Kind = eskProject then
              begin
                LMessage := 'Project opened with its Pascal companion and retained draft';
              end
              else if FActive.Kind = eskPascal then
              begin
                LMessage := 'Pascal applied / one Undo restores the pair';

                if FActive.Source.FileChange.IsDefined then
                begin

                  if FActive.Source.FileChange.Kind = npfcCreate then
                  begin
                    LMessage := 'Created ' + FActive.Source.FileReference.Name;
                  end
                  else
                  begin
                    LMessage := 'Removed ' + FActive.Source.FileReference.Name;
                  end;
                  LMessage := LMessage + ' / one Undo restores the pair';
                end;
              end
              else
              begin
                LMessage := 'Design / Pascal updated / one Undo restores the pair';
              end;
            end;
          nscRejected:
            begin
              LState := nssRejected;

              if FActive.Kind = eskProject then
              begin
                LMessage := ASource.Diagnostic.Message;
              end
              else if FActive.Kind = eskPascal then
              begin
                { The user may now be viewing another buffer. Report this exact
                  job's rejection without borrowing the active-file diagnostic. }
                LMessage := ASource.Diagnostic.Message;
              end
              else
              begin
                LMessage := ADesign.Diagnostic.Message;
              end;
            end;
          nscStale:
            begin
              LState := nssStale;
              LMessage := 'Editor result is stale / current design and draft retained';
            end;
        end;
      end;
    except
      on LException: Exception do
      begin
        LState := nssFailed;
        LMessage := LException.Message;
      end;
    end;
    { Notifications remain outside admission. Presentation cannot turn a
      published pair into a failed command or trigger a second publication. }
    {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
    WriteLn('source-command,admit,', ASequence, ',', GetTickCount64 - LAdmissionStarted);
    {$endif}
    LDesign := FActive.Kind = eskDesign;
    LProject := FActive.Kind = eskProject;
    LAction := FActive.Edit.Action;
    FCompletedEdit := FActive.Edit;
    FCompletedContext := FActive.Context;
    FCompletedHandler := Default(TNyxHandlerRef);
    FCompletedFileChange := Default(TNyxPascalFileChange);

    if (LState = nssApplied) and (FActive.Kind = eskPascal) then
    begin
      FCompletedFileChange := FActive.Source.FileChange;
    end;

    if (LState = nssApplied) and LDesign and
      (LAction in [sdaEvent, sdaCallbacks]) then
    begin
      FCompletedHandler := ADesign.AddedHandler;
    end;

    if (LState = nssApplied) and LDesign then
    begin
      RetirePublishedNameDraft;
    end;
    LAdopted := False;
    LAdoption := FActive.Schemas.Adoption;

    if LAdoption <> nil then
    begin
      try

        if LState = nssApplied then
        begin
          LAdopted := LAdoption.Accepted(FSession.Document);
        end
        else
        begin
          LAdoption.Refused(LMessage);
        end;
      except
        on LException: Exception do
        begin
          { Publication already has its authoritative outcome. A retired or
            failed presentation acknowledgement cannot undo or reclassify it. }
          LMessage := LMessage + ' / connection presentation: ' + LException.Message;

          try

            if LAdoption.Pending then
            begin
              LAdoption.Refused(LException.Message);
            end;
          except
            on LReplyException: Exception do
            begin
              { Creator requests may also fail during refusal. Queue retirement
                and the already-authoritative publication status still proceed. }
              LMessage := LMessage + ' / connection reply: ' + LReplyException.Message;
            end;
          end;
        end;
      end;
    end;
    LDiagramSave := FActive.Schemas.DiagramSave;

    if LDiagramSave <> nil then
    begin
      try

        if LState = nssApplied then
        begin
          LDiagramSave.Accepted(FSession.Document);
        end
        else
        begin
          LDiagramSave.Refused(LMessage);
        end;
      except
        on LException: Exception do
        begin
          LMessage := LMessage + ' / diagram presentation: ' + LException.Message;
          try
            LDiagramSave.Refused(LException.Message);
          except
            { An already published pair remains authoritative after a bad reply. }
          end;
        end;
      end;
    end;
    RetireJob(FActive);
    try
      {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
      LNotificationStarted := GetTickCount64;
      {$endif}
      Notify(LState, LMessage, LDesign, LAction, LProject, False, LAdopted);
      {$if defined(NYX_STUDIO_PROFILE) and not defined(PAS2JS)}
      WriteLn('source-command,notify,', ASequence, ',', GetTickCount64 - LNotificationStarted);
      {$endif}
    finally
      StartQueued;
    end;
  end
  else
  begin
    RetireJob(FActive);

    try

      if not FDiscardActive and (Length(FQueue) = 0) then
      begin
        Notify(nssApplied, 'Accepted Pascal is current');
      end;
    finally
      StartQueued;
    end;
  end;
end;

procedure TNyxSourceCommands.FinishShared(ASequence: Integer;
  const ASource: INyxPreparedSource; const ADesign: INyxPreparedDesign;
  AOutcome: TNyxSourcePublicationOutcome;
  const AReceipt: TNyxSourcePublicationReceipt; const AMessage: TNyxText);
var
  LCompletion: TNyxSourceCompletion;
  LMessage: TNyxText;
  LSharedHost: INyxSharedDesignSourceHost;
begin
  FScheduler.RequireUI;

  if FDetached or not FRunning or (ASequence <> FActive.Sequence) or
    (FSharedHost = nil) then
  begin
    Exit;
  end;
  LMessage := AMessage;
  try

    if FDiscardActive then
    begin
      FSharedHost.Abandon(npoUnconfirmed, 'A newer Apply superseded the shared result');
      Finish(ASequence, nil, nil, 'Shared result needs reconciliation with the current draft');
      Exit;
    end;

    if AOutcome <> npoCommitted then
    begin

      if LMessage = '' then
      begin
        LMessage := 'Shared source compilation did not confirm publication';
      end;
      FSharedHost.Abandon(AOutcome, LMessage);

      if (AOutcome = npoRefused) and (FActive.Kind = eskPascal) and
        FActive.Source.IsFileEdit and (ASource <> nil) and ASource.Diagnostic.Defined then
      begin
        { The diagnostic-only sealed completion checks the exact current buffer
          before recording its message. No shared/local pair is published. }
        LCompletion := FSession.CompleteSourceRequest(FActive.Source, ASource);
        Finish(ASequence, ASource, nil, '', True, LCompletion);
        Exit;
      end;
      Finish(ASequence, nil, nil, LMessage);
      Exit;
    end;
    if FActive.Kind = eskDesign then
    begin

      if not Supports(FSharedHost, INyxSharedDesignSourceHost, LSharedHost) then
      begin
        raise ENyxModel.Create('Shared visual completion lost its specialized host');
      end;
      LCompletion := LSharedHost.AdmitDesign(FActive.Design, ADesign, AReceipt);
    end
    else
    begin
      LCompletion := FSharedHost.Admit(FActive.Source, ASource, AReceipt);
    end;
    Finish(ASequence, ASource, ADesign, '', True, LCompletion);
  except
    on LException: Exception do
    begin
      FSharedHost.Abandon(npoUnconfirmed, LException.Message);
      Finish(ASequence, nil, nil, LException.Message);
    end;
  end;
end;

procedure TNyxSourceCommands.Cancel;
begin
  FScheduler.RequireUI;
  NextSequence;
  ClearQueue;
  CancelActive;
end;

procedure TNyxSourceCommands.CancelFile(const AFile: TNyxPascalSourceRef);
var
  LIndex: Integer;
  LCount: Integer;
begin
  FScheduler.RequireUI;
  AFile.Validate;

  if FRunning and (FActive.Kind = eskPascal) and not FActive.Source.FileChange.IsDefined and
    FActive.Source.FileReference.SameFile(AFile) then
  begin
    NextSequence;
  end;
  LCount := 0;
  for LIndex := 0 to High(FQueue) do
  begin

    if (FQueue[LIndex].Kind = eskPascal) and not FQueue[LIndex].Source.FileChange.IsDefined and
      FQueue[LIndex].Source.FileReference.SameFile(AFile) then
    begin
      FQueue[LIndex].Schemas.Free;
    end
    else
    begin
      FQueue[LCount] := FQueue[LIndex];
      Inc(LCount);
    end;
  end;
  SetLength(FQueue, LCount);

  if FRunning and (FActive.Kind = eskPascal) and not FActive.Source.FileChange.IsDefined and
    FActive.Source.FileReference.SameFile(AFile) then
  begin
    CancelActive;
  end;
  StartQueued;
end;

procedure TNyxSourceCommands.CancelActive;
begin
  FScheduler.RequireUI;
  FDiscardActive := True;

  if FActive.Schemas <> nil then
  begin
    FActive.Schemas.RetireAdoption('Connection save cancelled; current live wiring is retained.');
  end;

  if FSharedHost <> nil then
  begin
    FSharedHost.Abandon(npoUnconfirmed, 'Pending shared source command was cancelled');
  end;

  if FCompilation <> nil then
  begin
    FCompilation.Cancel;
    FCompilation := nil;
    RetireJob(FActive);
    FRunning := False;
  end;
  {$ifdef PAS2JS}
  RetireWorker;
  RetireJob(FActive);
  FRunning := False;
  {$endif}
  Notify(nssCancelled, 'Pending editor changes cancelled / current files and draft retained');
end;

procedure TNyxSourceCommands.Detach;
var
  LIndex: Integer;
begin
  FScheduler.RequireUI;
  FDetached := True;
  FChanged := nil;
  ClearQueue;
  FDiscardActive := True;

  if FPort <> nil then
  begin
    FPort.Detach;
  end;

  if FSharedHost <> nil then
  begin
    FSharedHost.Attach(nil);
    FSharedHost.Abandon(npoUnconfirmed, 'Shared source editor retired');
  end;
  { Cancel independent producers after revoking delivery. The destructor drains
    native terminal lifetimes without exposing this retired session to them. }
  for LIndex := 0 to High(FCompilations) do
  begin
    FCompilations[LIndex].Cancel;
  end;
end;

function TNyxSourceCommands.CompletedEvent(out AIntent: TNyxStudioEventIntent;
  out AOwner, AView: TNyxText; out AAddedHandler: TNyxHandlerRef): Boolean;
var
  LEvent: TNyxCallbackEvent;
  LPolicy: TNyxExecutionPolicy;
begin
  AIntent := Default(TNyxStudioEventIntent);
  AOwner := '';
  AView := '';
  AAddedHandler := Default(TNyxHandlerRef);
  Result := FPublishedDesign and (FPublishedAction in [sdaEvent, sdaCallbacks]) and
    FSession.MatchesCommandContext(FCompletedContext);

  if Result then
  begin

    if FPublishedAction = sdaCallbacks then
    begin
      Result := TryNyxCallbackPolicyAddShape(FCompletedEdit.Callbacks,
        AOwner, LEvent, LPolicy);

      if not Result then
      begin
        Exit;
      end;
      { Only this exact group's sole addition participates in ordinary TODO
        navigation. Plural generic callback batches retain their own results. }
      AIntent.Action := seaAdd;
      AIntent.Trigger := LEvent.Trigger;
      AIntent.Name := LEvent.Name;
    end
    else
    begin
      AIntent := FCompletedEdit.Event;
      AOwner := FCompletedEdit.Selection;
    end;
    AView := FCompletedEdit.View;
    AAddedHandler := FCompletedHandler;
  end;
end;

function TNyxSourceCommands.RouteEvents(ANode: TNyxNode; ATrigger: TNyxTrigger;
  const AReview: TNyxCallbackRemoval; out AEffect: TNyxInspectorEffect;
  out ALine: Integer; out ARemoval: TNyxCallbackRemoval): Boolean;
var
  LEdit: TNyxStudioDesignEdit;
begin
  FScheduler.RequireUI;

  if FDetached then
  begin
    raise ENyxModel.Create('This event-command context has retired');
  end;
  Result := CaptureNyxStudioEvents(FSession, ANode, ATrigger, AReview,
    PendingDesign, LEdit, AEffect, ALine, ARemoval);

  if Result and (LEdit.Action in [sdaEvent, sdaCallbacks]) then
  begin
    Edit(LEdit);
  end;
end;

function TNyxSourceCommands.Route(ANode: TNyxNode; const AEvent: TNyxEventInfo;
  AShellRoot: TNyxNode): Boolean;
var
  LSource: TNyxNode;
  LOrigin: TNyxNode;
  LEdit: TNyxStudioDesignEdit;
begin

  if FDetached then
  begin
    raise ENyxModel.Create('This source-command context has retired');
  end;

  if (AShellRoot <> nil) and (AEvent.Trigger = ntClick) and not AEvent.DefaultPrevented then
  begin
    LSource := AShellRoot.Find(AEvent.SourceID);

    if LSource <> nil then
    begin
      LOrigin := LSource.Find(AEvent.OriginID);

      if (LOrigin <> nil) and
        (CaptureNyxStudioQuery(FSession, LOrigin, AShellRoot, PendingDesign, LEdit) or
        CaptureNyxMenuBarInspector(FSession, LOrigin, AShellRoot, LEdit) or
        CaptureNyxSelectionInspector(FSession, LOrigin, AShellRoot, LEdit) or
        CaptureNyxKanbanInspector(FSession, LOrigin, AShellRoot, LEdit) or
        CaptureNyxDateRangeInspector(FSession, LOrigin, AShellRoot, LEdit) or
        CaptureNyxMasterDetailInspector(FSession, LOrigin, AShellRoot, LEdit) or
        CaptureNyxCanvasInspector(FSession, LOrigin, AShellRoot, LEdit) or
        CaptureNyxLogicInspector(FSession, LOrigin, AShellRoot, LEdit) or
        CaptureNyxPartsInspector(FSession, LOrigin, AShellRoot, LEdit) or
        CaptureNyxTabsInspector(FSession, LOrigin, AShellRoot, LEdit)) then
      begin
        Edit(LEdit);
        Exit(True);
      end;
    end;
  end;
  Result := Route(ANode, AEvent.Trigger, AShellRoot);
end;

function TNyxSourceCommands.Route(ANode: TNyxNode; ATrigger: TNyxTrigger;
  AShellRoot: TNyxNode): Boolean;
const
  CApply = 'action-apply-source';
  CRestore = 'action-reset-source';
  CDelete = 'action-delete';
  CDuplicate = 'action-duplicate';
  CUp = 'action-up';
  CDown = 'action-down';
  CPlaceStart = 'action-place-start';
  CPlaceCancel = 'action-place-cancel';
  CPlaceInside = 'action-place-inside';
  CPlaceBefore = 'action-place-before';
  CPlaceAfter = 'action-place-after';
  CPage = 'action-add-page';
  CComponent = 'action-component';
  CTitle = 'project-title';
  CPropertyKey = 'prop-key';
  CValue = 'value';
  CAddKind = 'add-kind';
  CComponentID = 'component-id';
  COverridePath = 'override-path';
  CSave = 'action-save';
  CSaveProject = 'action-project-save';
  CExportProject = 'action-project-export';
  CExportFiles = 'action-project-export-files';
  CExportSource = 'action-export-source';
  CSaveCopy = 'action-project-copy';
var
  LEdit: TNyxStudioDesignEdit;
  LCapture: TNyxStudioAuthoringCapture;
  LPlacement: TNyxPlacement;
  LAttribute: TNyxAttribute;
  LPlatform: TNyxPlatform;
  LViewport: TNyxViewportCondition;
  LPresentation: TNyxPresentationRef;
begin
  Result := False;

  if ANode = nil then
  begin
    Exit;
  end;

  if FDetached then
  begin
    raise ENyxModel.Create('This source-command context has retired');
  end;

  if (ATrigger = ntChange) and RouteNyxTabPageChoice(ANode, AShellRoot) then
  begin
    { Only disposable form fields changed. Ordinary hosts retain their drafts
      during this shell refresh; no source command or history entry is queued. }
    Notify(nssIdle, 'Tab page selected');
    Exit(True);
  end;

  if ATrigger = ntClick then
  begin

    if RouteNyxCanvasPrecisionGuide(ANode, AShellRoot) then
    begin
      Notify(nssIdle, 'Guide fields loaded');
      Exit(True);
    end;

    if CaptureNyxStudioQuery(FSession, ANode, AShellRoot, PendingDesign, LEdit) then
    begin
      Edit(LEdit);
      Exit(True);
    end;

    if CaptureNyxMenuBarInspector(FSession, ANode, AShellRoot, LEdit) then
    begin
      Edit(LEdit);
      Exit(True);
    end;

    if CaptureNyxLogicInspector(FSession, ANode, AShellRoot, LEdit) then
    begin
      Edit(LEdit);
      Exit(True);
    end;

    if CaptureNyxCanvasInspector(FSession, ANode, AShellRoot, LEdit) then
    begin
      Edit(LEdit);
      Exit(True);
    end;

    if CaptureNyxSelectionInspector(FSession, ANode, AShellRoot, LEdit) then
    begin
      Edit(LEdit);
      Exit(True);
    end;

    if CaptureNyxDateRangeInspector(FSession, ANode, AShellRoot, LEdit) then
    begin
      Edit(LEdit);
      Exit(True);
    end;

    if CaptureNyxMasterDetailInspector(FSession, ANode, AShellRoot, LEdit) then
    begin
      Edit(LEdit);
      Exit(True);
    end;

    if CaptureNyxKanbanInspector(FSession, ANode, AShellRoot, LEdit) then
    begin
      Edit(LEdit);
      Exit(True);
    end;

    if CaptureNyxPartsInspector(FSession, ANode, AShellRoot, LEdit) then
    begin
      Edit(LEdit);
      Exit(True);
    end;

    if CaptureNyxReusableInspector(FSession, ANode, AShellRoot, LEdit) then
    begin
      Edit(LEdit);
      Exit(True);
    end;
    if CaptureNyxExpressionInspector(FSession, ANode, AShellRoot, LEdit) then
    begin
      Edit(LEdit);
      Exit(True);
    end;

    if CaptureNyxTabsInspector(FSession, ANode, AShellRoot, LEdit) then
    begin
      Edit(LEdit);
      Exit(True);
    end;

    if CaptureNyxMenuInspector(FSession, ANode, AShellRoot, LEdit) then
    begin
      Edit(LEdit);
      Exit(True);
    end;

    if CaptureNyxDateDomainInspector(FSession, ANode, AShellRoot, LEdit) then
    begin
      Edit(LEdit);
      Exit(True);
    end;

    if CaptureNyxTimeDomainInspector(FSession, ANode, AShellRoot, LEdit) then
    begin
      Edit(LEdit);
      Exit(True);
    end;

    if CaptureNyxContentInspector(FSession, ANode, AShellRoot, LEdit) then
    begin
      Edit(LEdit);
      Exit(True);
    end;

    if CaptureNyxViewportInspector(FSession, ANode, AShellRoot, LEdit) then
    begin
      Edit(LEdit);
      Exit(True);
    end;

    if ANode.Prop(NyxStudioPropertyClearKey) <> '' then
    begin
      LEdit := Default(TNyxStudioDesignEdit);
      LEdit.Name := ANode.Prop(NyxStudioPropertyClearKey);
      LEdit.Selection := ANode.Prop(NyxStudioPropertyOwnerKey);

      if (LEdit.Selection = '') or (LEdit.Selection <> FSession.SelectedID) or
        not (TryNyxAttribute(LEdit.Name, LAttribute) or
        TryNyxPlatformKey(LEdit.Name, LPlatform, LAttribute) or
        TryNyxViewportKey(LEdit.Name, LViewport, LPlatform, LAttribute) or
        TryNyxPresentationKey(LEdit.Name, LPresentation, LPlatform, LAttribute)) then
      begin
        raise ENyxModel.Create('Select this component again before unsetting its size bound');
      end;

      if not (LAttribute in [atMinimumWidth, atMaximumWidth,
        atMinimumHeight, atMaximumHeight]) then
      begin
        raise ENyxModel.Create('This reset action requires a published size bound');
      end;
      LEdit.Action := sdaProperty;
      LEdit.View := FSession.ActiveViewID;
      LEdit.Value := '';
      Edit(LEdit);
      Exit(True);
    end;

    if (ANode.ID = CPlaceStart) or (ANode.ID = CPlaceCancel) then
    begin

      if ANode.ID = CPlaceStart then
      begin

        if Busy then
        begin
          raise ENyxModel.Create('Wait for pending editor work before choosing a move source');
        end;
        FSession.BeginPlacement;
        Notify(nssIdle, 'Choose a destination on the canvas or in the hierarchy');
      end
      else
      begin
        FSession.CancelPlacement;
        Notify(nssIdle, 'Placement canceled');
      end;
      Exit(True);
    end;

    if (ANode.ID = CPlaceInside) or (ANode.ID = CPlaceBefore) or
      (ANode.ID = CPlaceAfter) then
    begin

      if FSession.PlacementSource.ID = '' then
      begin
        raise ENyxModel.Create('The pending move changed; choose the source control again');
      end;
      LPlacement := nplInside;

      if ANode.ID = CPlaceBefore then
      begin
        LPlacement := nplBefore;
      end
      else if ANode.ID = CPlaceAfter then
      begin
        LPlacement := nplAfter;
      end;
      LEdit := FSession.CapturePlacement(NyxPlaceControl(FSession.PlacementSource,
        NyxControl(FSession.SelectedID), LPlacement), FSession.CommandContext);
      Edit(LEdit);
      Exit(True);
    end;
  end;
  { Collection data is document-scoped, while view intent captures its exact
    selected owner. Capture never generates source or borrows a runtime store. }

  if CaptureNyxStudioCollection(FSession, ANode, ATrigger, PendingDesign, LEdit) then
  begin
    Edit(LEdit);
    Exit(True);
  end;
  LCapture := CaptureNyxStudioAuthoring(FSession, ANode, ATrigger, AShellRoot,
    PendingDesign, LEdit);
  case LCapture of
    sacNameDraft:
      begin
        RetainNameDraft(LEdit);
        Exit(True);
      end;
    sacPresentation:
      begin
        Exit(True);
      end;
    sacEdit:
      begin
        Edit(LEdit);
        Exit(True);
      end;
    sacNone:
      begin
        { Other editor events continue through the existing typed routes. }
      end;
  end;
  { A save/export must not announce success for an earlier accepted pair while
    the visible fields still describe queued edits. A deliberate source-draft
    export remains independent; obsolete-load work does not claim Busy. }

  if (ATrigger = ntClick) and Busy and
    ((ANode.ID = CSave) or (ANode.ID = CSaveProject) or
    (ANode.ID = CExportProject) or (ANode.ID = CExportFiles) or
    (ANode.ID = CExportSource) or (ANode.ID = CSaveCopy)) then
  begin
    Notify(nssPreparing, 'Wait for pending editor changes before saving or exporting');
    Exit(True);
  end;
  LEdit := Default(TNyxStudioDesignEdit);
  LEdit.Selection := FSession.SelectedID;
  LEdit.View := FSession.ActiveViewID;

  if ATrigger = ntChange then
  begin

    if ANode.Prop(CPropertyKey) <> '' then
    begin
      LEdit.Action := sdaProperty;
      LEdit.Name := ANode.Prop(CPropertyKey);
      LEdit.Value := ANode.Prop(CValue);
    end
    else if ANode.ID = CTitle then
    begin
      LEdit.Action := sdaTitle;
      LEdit.Value := ANode.Prop(CValue);
    end
    else
    begin
      Exit;
    end;
  end
  else if ATrigger = ntClick then
  begin

    if ANode.ID = CApply then
    begin
      ApplyFile(FSession.ActiveSourceFile);
      Exit(True);
    end;

    if ANode.ID = CRestore then
    begin
      FSession.DiscardFileDraft(FSession.ActiveSourceFile);
      CancelFile(FSession.ActiveSourceFile);

      if Busy then
      begin
        Notify(nssPreparing, 'Selected file restored / other pending work retained',
          False, sdaProperty, False, True);
      end
      else
      begin
        Notify(nssIdle, 'Accepted Pascal restored', False, sdaProperty, False, True);
      end;
      Exit(True);
    end;

    if ANode.Prop(CAddKind) <> '' then
    begin
      LEdit.Action := sdaAddKind;
      LEdit.Name := ANode.Prop(CAddKind);
    end
    else if ANode.Prop(CComponentID) <> '' then
    begin
      LEdit.Action := sdaAddInstance;
      LEdit.Name := ANode.Prop(CComponentID);
    end
    else if ANode.Prop(COverridePath) <> '' then
    begin
      LEdit.Action := sdaCustomizePart;
      LEdit.Name := ANode.Prop(COverridePath);
    end
    else
    begin
      { Decode chrome identity once at this boundary. Native Delphi-dialect
        FPC does not share pas2js's string CASE extension. The worker contract
        below still dispatches a closed Pascal enum. }

      if ANode.ID = CDelete then
      begin
        LEdit.Action := sdaDelete;
      end
      else if ANode.ID = CDuplicate then
      begin
        LEdit.Action := sdaDuplicate;
      end
      else if (ANode.ID = CUp) or (ANode.ID = CDown) then
      begin
        LEdit.Action := sdaMove;
      end
      else if ANode.ID = CPage then
      begin
        LEdit.Action := sdaAddPage;
      end
      else if ANode.ID = CComponent then
      begin
        LEdit.Action := sdaCreateComponent;
      end
      else
      begin
        Exit;
      end;

      if ANode.ID = CUp then
      begin
        LEdit.Direction := nmdPrevious;
      end
      else if ANode.ID = CDown then
      begin
        LEdit.Direction := nmdNext;
      end;
    end;
  end
  else
  begin
    Exit;
  end;
  Edit(LEdit);
  Result := True;
end;

{$ifdef PAS2JS}
procedure TNyxSourceCommands.RetireWorker;
begin

  if FTimeout >= 0 then
  begin
    window.clearTimeout(FTimeout);
    FTimeout := -1;
  end;

  if FWorker <> nil then
  begin
    FWorker.removeEventListener('message', FReceiveHandler);
    FWorker.removeEventListener('error', FErrorHandler);
    FWorker.terminate;
    FWorker := nil;
  end;
end;

function TNyxSourceCommands.Receive(AEvent: TJSEvent): Boolean;
var
  LDelivery: TSourceDelivery;
  LWork: INyxWork;
  LData: TNyxDataValue;
begin
  Result := True;
  LDelivery := TSourceDelivery.Create;
  LWork := LDelivery;
  LDelivery.Port := FPort;
  LDelivery.Sequence := FActive.Sequence;
  try

    if not isString(TJSMessageEvent(AEvent).data) then
    begin
      raise ENyxModel.Create('Editor worker returned non-text protocol data');
    end;
    LData := TNyxDataValue.ParseJSON(TNyxText(TJSMessageEvent(AEvent).data));

    if FActive.Kind = eskPascal then
    begin
      LDelivery.Source := ReceiveNyxPreparedSource(LData, FActive.Source.Source, FActive.Schemas.Value);
    end
    else
    begin
      LDelivery.Design := ReceiveNyxPreparedDesign(LData, FActive.Design, FActive.Schemas.Value);
    end;
  except
    on LException: Exception do
    begin
      LDelivery.Failure := LException.Message;
    end;
  end;
  RetireWorker;
  FScheduler.PostUI(LWork);
end;

function TNyxSourceCommands.WorkerError(AEvent: TJSEvent): Boolean;
begin
  Result := False;
  AEvent.preventDefault;
  WorkerTimeout;
end;

procedure TNyxSourceCommands.WorkerTimeout;
begin
  RetireWorker;
  Finish(FActive.Sequence, nil, nil,
    'Editor processor failed or timed out / current files and draft retained');
end;
{$endif}

destructor TNyxSourceCommands.Destroy;
{$ifndef PAS2JS}
var
  LIndex: Integer;
  LWaiting: Boolean;
{$endif}
begin

  if FScheduler <> nil then
  begin
    Detach;
    {$ifdef PAS2JS}
    RetireWorker;
    {$else}
    for LIndex := 0 to High(FExecutions) do
    begin

      if FExecutions[LIndex] <> nil then
      begin
        FExecutions[LIndex].Cancel;
      end;
    end;
    repeat
      LWaiting := False;
      for LIndex := 0 to High(FExecutions) do
      begin

        if (FExecutions[LIndex] <> nil) and
          (FExecutions[LIndex].Status in [nesPending, nesRunning]) then
        begin
          LWaiting := True;
        end;
      end;
      for LIndex := 0 to High(FCompilations) do
      begin

        if FCompilations[LIndex].State in [scsPending, scsRunning] then
        begin
          LWaiting := True;
        end;
      end;

      if LWaiting then
      begin
        CheckSynchronize(1);
        Sleep(1);
      end;
    until not LWaiting;
    FExecutions := nil;
    {$endif}
  end;
  FCompilations := nil;
  FCompilation := nil;
  FCompiler := nil;
  FPort := nil;
  RetireJob(FActive);
  ClearQueue;
  inherited Destroy;
end;

end.
