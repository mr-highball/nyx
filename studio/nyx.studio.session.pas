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

unit nyx.studio.session;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  Classes,
  nyx.text,
  nyx.pascal.drafts,
  nyx.pascal.filechanges,
  nyx.data,
  nyx.types,
  SysUtils,
  nyx.model,
  nyx.root.types,
  nyx.state,
  nyx.binding.types,
  nyx.expressions,
  nyx.expressions.editing,
  nyx.collections,
  nyx.collections.view.types,
  nyx.codec,
  nyx.catalog,
  nyx.controls,
  nyx.design.tokens,
  nyx.image.editor,
  nyx.resources.editor,
  nyx.studio.resources,
  nyx.images,
  nyx.designer.resize,
  nyx.codegen,
  nyx.source, nyx.pascal.project,
  nyx.source.preparation,
  nyx.schema,
  nyx.studio.history,
  nyx.studio.projects,
  nyx.studio.sourceprojection,
  nyx.studio.edits,
  nyx.studio.transactions,
  nyx.studio.callbackintent,
  nyx.studio.handlerintent,
  nyx.studio.rootedits,
  nyx.callbacks,
  nyx.scheduler,
  nyx.studio.authoring,
  nyx.studio.collectionintent,
  nyx.sample;

type
  { Deliberate grouped-command admission refusal, not compiler unavailability.
    Hosts keep the accepted pair and unfinished draft/base untouched and ask the
    caller to resolve that draft before retrying. The distinct type lets editor
    queues report Rejected without classifying unrelated provider failures alike. }
  ENyxStudioDraftResolutionRequired = class(ENyxModel);

  { An immutable source-command ticket owns only values. Its private session
    identity prevents a result from being applied to another project, even when
    both projects happen to contain identical files. No accepted node is borrowed. }
  TNyxStudioSourceRequest = record
  private
    FOwner: TNyxText;
    FGeneration: Integer;
    FAccepted: TNyxText;
    FCheckpoint: TNyxSourceCheckpoint;
    FSource: TNyxText;
    FInputFiles: TNyxPascalProjectFiles;
    FFile: TNyxPascalSourceRef;
    FFileBase: TNyxText;
    { Inventory changes retain their own closed intent. Selection is copied
      only to avoid stealing a later file navigation on delayed completion. }
    FFileChange: TNyxPascalFileChange;
    FActiveFile: TNyxPascalSourceRef;
    { Shared/local publication must record the same dispatch-time Undo frame.
      Later unrelated typing stays current and is captured in opposite history. }
    FUndo: TNyxStudioCheckpoint;
    { Ordinary Apply captures its current draft; an explicit source-range edit
      captures the unchanged buffer and keeps its proposed text off the editor.
      Both completion paths refuse intervening changes to that selected buffer. }
    FBufferBaseline: TNyxText;
    FSourceEdit: Boolean;
    { Sealed complete-project source capture may replace retained inputs or apply
      explicit typed membership. Main identity/limits and visible buffers remain
      fixed until complete constructor admission and paired publication. }
    FProjectSourceEdit: Boolean;
    FSchemaRevision: Integer;
    FChanged: Boolean;
    function GetOrigin: TNyxSourceOrigin;
    function GetFiles: TNyxPascalProjectFiles;
  public
    property Source: TNyxText read FSource;
    { Closed captured construction origin. This lets a host choose its explicit
      compiler dispatch without guessing from comments or raw source strings. }
    property Origin: TNyxSourceOrigin read GetOrigin;
    { Frozen candidate supporting files contain a selected helper edit, explicit
      inventory intent or sealed grouped source ranges. All unedited inputs stay
      exact. Compilation uses this snapshot, never later editor buffers. }
    property Files: TNyxPascalProjectFiles read GetFiles;
    property SchemaRevision: Integer read FSchemaRevision;
    property Changed: Boolean read FChanged;
    property IsSourceEdit: Boolean read FSourceEdit;
    property IsProjectSourceEdit: Boolean read FProjectSourceEdit;
    { The selected file alone supplies unfinished input. Other helper/main
      buffers remain outside this captured compiler candidate. }
    property FileReference: TNyxPascalSourceRef read FFile;
    property FileChange: TNyxPascalFileChange read FFileChange;
    function IsFileEdit: Boolean;
  end;

  { Warning-only removal review. It seals one session/load and exact accepted/
    unfinished target bytes; it is deliberately not a compilable source request.
    Confirmation revalidates these values before constructing a candidate. }
  TNyxStudioFileRemovalReview = record
  private
    FOwner: TNyxText;
    FGeneration: Integer;
    FFile: TNyxPascalSourceRef;
    FAccepted: TNyxText;
    FBuffer: TNyxText;
    function GetDefined: Boolean;
  public
    property IsDefined: Boolean read GetDefined;
    property FileReference: TNyxPascalSourceRef read FFile;
  end;

  { Closed admission result; only Applied transfers a changed accepted pair.
    Diagnostics/stale work retain the current owners and independent buffers. }
  TNyxSourceCompletion = (nscUnchanged, nscApplied, nscRejected, nscStale);

  { Direct loading can start an independent history; an editor Open command
    instead keeps the previous complete pair as one Undo entry. This host choice
    is never inferred from a saved file or a constructor's execution result. }
  TNyxProjectOpeningHistory = (pohNewHistory, pohPairedCommand);

  { A saved project is data, never execution evidence. This sealed opening
    request captures its complete pair, explicit resolution, creator revision
    and the current session/load/files/draft. Compiler preparation owns no live
    editor; completion may replace the project only while this baseline matches. }
  TNyxStudioProjectRequest = record
  private
    FOwner: TNyxText;
    FGeneration: Integer;
    FBaseline: TNyxText;
    FPair: TNyxProjectPair;
    FResolution: TNyxProjectResolution;
    FHistory: TNyxProjectOpeningHistory;
    FSchemaRevision: Integer;
    function GetSource: TNyxText;
    function GetFiles: TNyxPascalProjectFiles;
  public
    property Source: TNyxText read GetSource;
    property Files: TNyxPascalProjectFiles read GetFiles;
    property Pair: TNyxProjectPair read FPair;
    property Resolution: TNyxProjectResolution read FResolution;
    property History: TNyxProjectOpeningHistory read FHistory;
  end;

  { Closed editor intent. Names/values are data at the inspector/catalog boundary;
    processing invokes existing typed commands, never arbitrary Pascal methods. }
  TNyxStudioDesignAction = (sdaProperty, sdaAddKind, sdaDelete, sdaDuplicate,
    sdaMove, sdaTitle, sdaAddPage, sdaCreateComponent, sdaAddInstance, sdaCustomizePart,
    sdaCanvasValue, sdaSetStateDefault, sdaCreateStateDefault,
    sdaRenameStateDefault, sdaRemoveStateDefault, sdaSetBinding, sdaInheritBinding,
    sdaEvent, sdaCollection, sdaPlacement, sdaResize, sdaPresentation, sdaPosition,
    sdaContent, sdaValueDomain, sdaMenu, sdaTheme, sdaImage, sdaResource, sdaTransaction,
    sdaCallbacks, sdaHandlers);
  { Callback operations carry exact typed event/registration references. Removal
    includes the handler the user reviewed; IDs alone cannot authorize replacing
    a registration. Empty references belong only to add/policy intent. }
  TNyxStudioEventAction = (seaAdd, seaPolicy, seaRemove);
  TNyxStudioEventIntent = record
    Action: TNyxStudioEventAction;
    Trigger: TNyxTrigger;
    Name: TNyxEventRef;
    Policy: TNyxExecutionPolicy;
    ID: TNyxCallbackRef;
    Handler: TNyxHandlerRef;
  end;
  { A structural move is one sibling step. Arbitrary integer offsets are not
    accepted by the queued editor contract. }
  TNyxStudioMoveDirection = (nmdPrevious, nmdNext);
  { Immutable session/load identity for queued intent before its full baseline
    is captured. This is not a document revision: publication still compares
    the fresh paired files, draft and creator generation. }
  TNyxStudioCommandContext = record
  private
    FOwner: TNyxText;
    FGeneration: Integer;
  end;
  { Captured value-only editor intent. Each action interprets only its declared
    fields; queued preparation owns copies and never follows later navigation.
    Baselines constrain publication without carrying runtime or UI authority. }
  TNyxStudioDesignEdit = record
  private
    FCanvasContext: TNyxStudioCommandContext;
  public
    Action: TNyxStudioDesignAction;
    { Exact authored IDs captured when the UI emits intent. They do not follow
      later navigation or another session/load with matching control names. }
    Selection: TNyxText;
    View: TNyxText;
    { Inspector key, catalog kind, reusable definition ID, named part path or
      exact canvas runtime identity, according to Action. This is the explicit
      metadata/extension boundary, never a method or property to execute. }
    Name: TNyxText;
    { Typed schema admission interprets field input; title remains user text.
      No method name or executable statement is taken from either string. }
    Value: TNyxText;
    Direction: TNyxStudioMoveDirection;
    { Canvas replay projects the same concrete platform's typed overrides.
      Other actions use npfAny. Neither DOM nor LCL handles cross this boundary. }
    Platform: TNyxPlatform;
    { State editor notation retains its declared scalar family. Parsing runs on
      the independent processor so incomplete input never mutates accepted data.
      Binding descriptors own copied values and contain no runtime references. }
    StateInput: TNyxStudioStateInput;
    Binding: TNyxBindingSpec;
    { Value-only callback intent, with no source pointer or executable closure. }
    Event: TNyxStudioEventIntent;
    { Normalized value-only callback group, sharing the typed patch contract. }
    Callbacks: TNyxCallbackBatchSnapshot;
    Handlers: TNyxHandlerBatchSnapshot;
    { Collection-scoped values and family-qualified field/item references.
      Data operations have no selected owner; view operations capture one. }
    Collection: TNyxStudioCollectionIntent;
    { Exact relative placement, copied before independent preparation. }
    Placement: TNyxPlacementChange;
    { One grouped typed dimension change; never an executable property string. }
    Resize: TNyxResizeChange;
    { Both absolute origin axes belong to one paired history operation. }
    Position: TNyxPositionChange;
    { Named definition/override intent shares the same isolated paired job. }
    Presentation: TNyxPresentationEdit;
    { Copied whole-recipe command and exact registry shown when captured.
      A queued edit cannot silently replace choices changed by an earlier job.
      Both fields contain values only, never the mutable authoring facade. }
    Content: TNyxContentEdit;
    ContentBaseline: TNyxText;
    { Copied exact-owner policy and mounted local/effective baseline. The
      independent processor refuses a changed policy before paired publication. }
    ValueDomain: TNyxValueDomainEdit;
    ValueDomainBaseline: TNyxText;
    { Complete copied menu intent and exact registry/local attachment baseline.
      Contains no document, control, renderer or mutable authoring facade. }
    Menu: TNyxMenuEdit;
    MenuBaseline: TNyxText;
    { Copied palette proposal with exact absent/local declaration baseline.
      Reset reveals the independent host base. No renderer or theme is retained. }
    Theme: TNyxThemeTokens;
    ThemeReset: Boolean;
    ThemeBaseline: TNyxText;
    { One copied complete image proposal. Imported bytes remain portable;
      source, alternative text and sizing share one paired admission/history. }
    Image: TNyxImageEditorChange;
    { Whole resource proposal and optional scalar binding travel through one
      independent paired processor. Values contain no file/provider/UI handle. }
    Resource: TNyxResourceEditorChange;
    { Complete ordered semantic group, captured from its typed managed contract.
      The portable snapshot owns only immutable normalized values. The source
      processor materializes its own transaction and verifies the whole meaning. }
    Transaction: TNyxProjectTransactionSnapshot;
    { Optional public scene-picker baseline, copied at actual form capture and
      checked again by the owning source processor before grouped publication. }
    CanvasSceneBaseline: TNyxText;
    { Optional exact portable design captured by an external runtime adoption.
      Its queued transaction refuses intervening model changes at preparation;
      ordinary unguarded transactions retain their existing wire and behavior. }
    TransactionDesignBaseline: TNyxText;
    { Immutable origin of a canvas capture. Queue admission uses this mounted
      session/load identity even when the caller retains intent before enqueue. }
    property CanvasContext: TNyxStudioCommandContext read FCanvasContext;
  end;

  { Owned presentation values for uncommitted field edits. This snapshot affects
    only Studio chrome, never admission, source or the accepted design tree. }
  TNyxStudioPendingField = record
    Selection: TNyxText;
    Key: TNyxText;
    Value: TNyxText;
    { State fields retain their editor notation while an earlier value publishes.
      Other presentation fields leave this at its default text notation. }
    StateInput: TNyxStudioStateInput;
  end;
  { Copied presentation of an uncommitted field proposal. The exact view,
    editable owner and runtime field distinguish separate reusable instances. }
  TNyxStudioPendingCanvasValue = record
    View: TNyxText;
    Owner: TNyxText;
    RuntimeID: TNyxText;
    Value: TNyxText;
  end;
  { The latest exact owner/target descriptor is presentation only. Inherit means
    its effective descriptor must be realized after admission, not guessed from
    the currently accepted local override. }
  TNyxStudioPendingBinding = record
    Owner: TNyxText;
    Spec: TNyxBindingSpec;
    Inherit: Boolean;
  end;
  { Presentation-only callback intent qualified by its exact authored owner. }
  TNyxStudioPendingEvent = record
    Owner: TNyxText;
    Intent: TNyxStudioEventIntent;
  end;
  { A grouped edit locks only events it actually touches. These copied keys
    carry no batch implementation, callback collection or source authority. }
  TNyxStudioPendingEventLock = record
    Owner: TNyxText;
    Trigger: TNyxTrigger;
    Name: TNyxEventRef;
  end;
  { Value-only pending collection proposal. Owner is empty for document data
    and exact authored identity for collection-view operations. }
  TNyxStudioPendingCollection = record
    Owner: TNyxText;
    Intent: TNyxStudioCollectionIntent;
  end;
  TNyxStudioPendingDesign = record
    TitleDefined: Boolean;
    Title: TNyxText;
    Fields: array of TNyxStudioPendingField;
    CanvasValues: array of TNyxStudioPendingCanvasValue;
    { Pending state rows use exact names/families rather than positional widget
      IDs. Rename drafts are presentation only, applied by explicit user intent. }
    StateValues: array of TNyxStudioPendingField;
    StateNames: array of TNyxStudioPendingField;
    RenamingStates: array of TNyxText;
    NewDefaultPending: Boolean;
    Bindings: array of TNyxStudioPendingBinding;
    Events: array of TNyxStudioPendingEvent;
    EventLocks: array of TNyxStudioPendingEventLock;
    Collections: array of TNyxStudioPendingCollection;
    { Structural data locks guard pre-paint clicks and pending form duplication;
      view locks belong to the exact authored control, never a positional ID. }
    function CollectionLocked(const AKey: TNyxCollectionRef): Boolean;
    function CollectionViewLocked(const AOwner: TNyxText): Boolean;
    function CollectionCreationPending: Boolean;
    { Latest matching scalar/column proposal keeps its original notation.
      False clears the output; accepted data/history are never modified. }
    function CollectionValue(const AKey: TNyxCollectionRef;
      const AItem: TNyxItemRef; const AField: TNyxStudioCollectionFieldRef;
      AAction: TNyxStudioCollectionAction; const AOwner: TNyxText;
      out AIntent: TNyxStudioCollectionIntent): Boolean;
    { Latest waiting policy wins without changing the accepted event contract. }
    function EventPolicy(const AOwner: TNyxText; ATrigger: TNyxTrigger;
      const AName: TNyxEventRef; out APolicy: TNyxExecutionPolicy): Boolean;
    { Confirmed removal and grouped changes lock their exact events until
      admission retires, including clicks before disabled controls repaint. }
    function EventLocked(const AOwner: TNyxText; ATrigger: TNyxTrigger;
      const AName: TNyxEventRef): Boolean;
    { Copy the latest exact authored owner/target. False returns a cleared
      descriptor and False inheritance flag; no effective binding is invented. }
    function Binding(const AOwner: TNyxText; ATarget: TNyxBindingProperty;
      out ASpec: TNyxBindingSpec; out AInherit: Boolean): Boolean;
    { Latest editor text for this exact name/family. False clears the output;
      empty text can still be a defined pending value. }
    function StateValue(const AName: TNyxText; AKind: TNyxStateKind;
      out AValue: TNyxText): Boolean;
    { Preserve the latest queued notation while earlier values publish. False
      supplies text notation; callers then use the accepted value's notation. }
    function StateEditorInput(const AName: TNyxText; AKind: TNyxStateKind;
      out AInput: TNyxStudioStateInput): Boolean;
    { Latest retained/queued name draft. It never changes accepted identity.
      False clears the output; the caller displays the accepted name instead. }
    function StateName(const AName: TNyxText; AKind: TNyxStateKind;
      out AValue: TNyxText): Boolean;
    { True only while an exact rename is active/waiting in this load. }
    function StateLocked(const AName: TNyxText): Boolean;
    { Last pending value wins for the exact owner/key. False clears AValue;
      empty text can still be a defined value. The snapshot owns its array. }
    function PropertyValue(const ASelection, AKey: TNyxText;
      out AValue: TNyxText): Boolean;
    { Overlay only exact pending fields on a realized presentation. Last value
      wins, without document/state/history mutation. Missing/retired fields are
      ignored; authored roots refuse. True asks the adapter to synchronize. }
    function ApplyCanvasValues(ARoot: TNyxNode; const AView: TNyxText): Boolean;
  end;

  { An immutable processor ticket captures the complete admitted pair and exact
    pending draft. Workers reconstruct independent session/catalog/history owners;
    neither this record nor a result borrows an accepted node or renderer. }
  TNyxStudioDesignRequest = record
  private
    FOwner: TNyxText;
    FGeneration: Integer;
    FNextID: Integer;
    FSchemaRevision: Integer;
    FPair: TNyxProjectPair;
    { Only an already admitted live owner supplies this opaque checkpoint. It
      never travels through the literal worker protocol as execution authority. }
    FAcceptedCheckpoint: TNyxSourceCheckpoint;
    FEdit: TNyxStudioDesignEdit;
    function GetRequiresExecution: Boolean;
    function GetFiles: TNyxPascalProjectFiles;
  public
    function ToData: TNyxDataValue;
    { Intent-only semantic transport contains no owner, accepted pair/checkpoint
      or execution claim. The receiving server captures its own live baseline
      and independently prepares the same edit before permitting compilation. }
    function IntentData: TNyxDataValue;
    function SameRequest(const AOther: TNyxStudioDesignRequest): Boolean;
    property SchemaRevision: Integer read FSchemaRevision;
    property RequiresExecution: Boolean read GetRequiresExecution;
    { Retained accepted helper inventory for a compiler-aware visual proposal.
      Its main source is the proposal's exact Source, not this old checkpoint. }
    property Files: TNyxPascalProjectFiles read GetFiles;
  end;

  { Trusted private processor result. Mutable paired owners are never exposed
    until the receiving UI consumes Take once; diagnostics own no partial pair. }
  INyxPreparedDesign = interface(INyxPreparedSource)
    ['{95241E55-7889-4B38-8686-08D249352B98}']
    function Matches(const ARequest: TNyxStudioDesignRequest): Boolean;
    function GetSelection: TNyxText;
    function GetView: TNyxText;
    function GetNextID: Integer;
    { Nonempty only after successful independent add preparation. Consumers use
      it only after publication, at the captured owner/view, to locate the stub. }
    function GetAddedHandler: TNyxHandlerRef;
    { Proposals have no publishable executed workspace. Compile and verify the
      exact source and whole design before Take can succeed. }
    function GetRequiresCompilation: Boolean;
    property Selection: TNyxText read GetSelection;
    property View: TNyxText read GetView;
    property NextID: Integer read GetNextID;
    property AddedHandler: TNyxHandlerRef read GetAddedHandler;
    property RequiresCompilation: Boolean read GetRequiresCompilation;
  end;

  { Optional bounded callback context from independent preparation. This is
    proposed metadata, never proof of publication or executed source authority. }
  INyxPreparedCallbackGroup = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001004002703}']
    function CallbackResults: TNyxDataValue;
  end;

  { Optional bounded source navigation for a proposed exact body group. These
    rows confer no admission and become usable only at committed revision. }
  INyxPreparedHandlerGroup = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001004003003}']
    function HandlerResults: TNyxDataValue;
  end;

  { Closed command destination; extension names and values remain typed data. }
  TNyxStudioExtensionOwner = (seoDocument, seoSelection);

  { Deliberate file adoption is one complete editor command. Synchronization
    publishes ordinary local typing metadata without creating a history command
    for each keystroke; accepted-file changes still record one command. }
  TNyxStudioProjectAdoption = (spaCommand, spaSynchronization);

  { Trusted recovery value, independent of any filesystem or target control.
    History entries are immutable admitted editor checkpoints, ordered oldest
    first. The native recovery codec admits every paired history entry before
    constructing a session. Arrays returned by RecoveryFrame are independent;
    their values retain no mutable document, renderer or session owner. }
  TNyxStudioRecoveryFrame = record
    Pair: TNyxProjectPair;
    { In-memory admission only. Runtime codecs never serialize this opaque
      checkpoint; readers must rebuild it through literal or executed admission.
      It permits current executed source to retain its original complete unit. }
    AcceptedCheckpoint: TNyxSourceCheckpoint;
    Selection: TNyxText;
    View: TNyxText;
    NextID: Integer;
    Undo: array of TNyxStudioCheckpoint;
    Redo: array of TNyxStudioCheckpoint;
  end;

  { Portable designer state, independent of Studio's browser/native shell.
    Commands operate on the same owned document applications consume. Snapshots
    provide deterministic reversible history and never retain renderer handles.
    SelectedID and ActiveViewID are stable identities, not borrowed node pointers,
    because undo/load can replace the entire tree. }
  TNyxStudioSession = class
  private
    { Transient two-step placement belongs to this project session. It retains
      copied identities/pair only, never a selected node or another view. }
    FPlacementSource: TNyxControlRef;
    FPlacementContext: TNyxStudioCommandContext;
    FPlacementPair: TNyxProjectPair;
    function GetPlacementSource: TNyxControlRef;
  private
    FDocument: TNyxDocument;
    FCatalog: TNyxCatalog;
    FUndo: TNyxStudioHistory;
    FRedo: TNyxStudioHistory;
    FSourceWorkspace: TNyxSourceWorkspace;
    FSourceDraft: TNyxText;
    FSourceDraftBase: TNyxText;
    FSourceDraftPending: Boolean;
    FFileDrafts: TNyxPascalFileDrafts;
    { Accepted immutable input cache avoids reparsing all helpers on each
      keystroke. It owns no document/provider; exact source/inventory keys expire
      it lazily after any admitted accepted-input change. }
    FFileInputCache: INyxPascalProject;
    FFileInputKey: TNyxText;
    { Presentation belongs to this editor context, never accepted source/history. }
    FActiveSourceFile: TNyxPascalSourceRef;
    FSourceDiagnostic: TNyxSourceDiagnostic;
    FSourceDiagnosticSource: TNyxText;
    { The latest rejection belongs to one exact editable buffer. Helper compiler
      errors keep their message; legacy scalar mapping grants main sites only. }
    FSourceDiagnosticFile: TNyxPascalSourceRef;
    FSourceIdentity: TNyxText;
    FSourceGeneration: Integer;
    FSelectedID: TNyxText;
    FActiveViewID: TNyxText;
    FNextID: Integer;
    { Disposable executed-source planners reserve names from the retained
      Pascal as well as their literal companion. This is not admission authority
      and never enters persistence, history or the live session. }
    FCallbackReservedSource: TNyxText;
    function GetSourceDiagnostic: TNyxSourceDiagnostic;
    { Retain a compiler site only against the sealed request's exact candidate
      file and the still-current editable buffer. This never admits source. }
    procedure CaptureCompilerDiagnostic(const ARequest: TNyxStudioSourceRequest;
      const APrepared: INyxPreparedSource);
    function SourceFileInputs: INyxPascalProject;
    { Creation requires continued absence; existing-file intents require exact
      accepted/buffer bytes. Missing members refuse without dereferencing them. }
    function MatchesSourceFileRequest(const ARequest: TNyxStudioSourceRequest): Boolean;
    function GetActiveSourceFile: TNyxPascalSourceRef;
    procedure SetActiveSourceFile(const AFile: TNyxPascalSourceRef);
    function GetActiveSourceText: TNyxText;
    procedure DoApplySourceDraft;
    function NewID(const AKind: TNyxText): TNyxText;
    { Construction-only empty owners, with one private session identity. Does
      not create a starter project or grant admission to any incoming pair. }
    procedure InitializeOwners;
    procedure Checkpoint;
    procedure TrimUndoHistory;
    procedure Commit;
    procedure Rollback;
    procedure Restore(const ASource: TNyxText);
    { Capture synchronized accepted files and exact unfinished text/base as one
      immutable value. No mutable document/session/control owner escapes. }
    function CaptureHistory: TNyxStudioCheckpoint;
    { Decode/validate a complete checkpoint before swapping either accepted
      owner. Optional ARemember receives the freshly synchronized current pair
      after target admission and before publication; failures retain both owners
      and history. Rollback deliberately omits capture of an invalid candidate. }
    procedure RestorePair(const ACheckpoint: TNyxStudioCheckpoint;
      ARemember: TNyxStudioHistory = nil);
    procedure Reidentify(ANode: TNyxNode);
    { All content insertion uses one destination contract, including reusable
      payloads inside instance layout overrides. InsertNode consumes its newly
      constructed candidate on success or failure; admission/history own cleanup. }
    function InsertionParent: TNyxNode;
    procedure InsertNode(ANode: TNyxNode);
    { Admit/no-op-check a detached document before checkpoint/publication. On
      success ownership transfers and the argument becomes nil; otherwise the
      caller still owns it. Accepted nodes, selection and redo remain untouched. }
    procedure PublishCandidate(var ACandidate: TNyxDocument); overload;
    procedure PublishCandidate(var ACandidate: TNyxDocument;
      const ARenames: array of TNyxSourceStateRename); overload;
    { Transfers an already-admitted design/workspace pair with one checkpoint.
      Callers retain both owners if checkpointing fails; success clears them. }
    procedure PublishPair(var ACandidate: TNyxDocument;
      var AWorkspace: TNyxSourceWorkspace);
    procedure PublishCapturedPair(var ACandidate: TNyxDocument;
      var AWorkspace: TNyxSourceWorkspace; const ACheckpoint: TNyxSourceCheckpoint;
      ARememberDraft: Boolean = True; AAllowProjectFiles: Boolean = False);
    { Same atomic owner transfer with an explicit immutable Undo baseline.
      Source commands capture it before independent compiler work begins. }
    procedure PublishHistoryPair(var ACandidate: TNyxDocument;
      var AWorkspace: TNyxSourceWorkspace; const ACheckpoint: TNyxSourceCheckpoint;
      const AHistory: TNyxStudioCheckpoint; AAllowProjectFiles: Boolean);
    { Shared final publication for fully admitted independent owners. Success
      transfers/clears them; wrappers release them on refusal or an exact no-op. }
    procedure AdoptAdmittedProject(var ADocument: TNyxDocument;
      var AWorkspace: TNyxSourceWorkspace; const AResolved: TNyxProjectPair;
      AAdoption: TNyxStudioProjectAdoption);
    { Transfers staged owners for an intentional project load. Both admission
      wrappers use this same history/reset policy; no parsing follows transfer. }
    procedure LoadAdmittedProject(ADocument: TNyxDocument;
      AWorkspace: TNyxSourceWorkspace; const AResolved: TNyxProjectPair);
    { Transfer one explicitly opened pair through ordinary history, retiring
      input captured in the old load even when visible identities are reused. }
    procedure PublishOpenedProject(var ADocument: TNyxDocument;
      var AWorkspace: TNyxSourceWorkspace; const AResolved: TNyxProjectPair);
    function CallbackCandidate: TNyxDocument;
    function DoAddCallback(ATrigger: TNyxTrigger; const AName: TNyxEventRef;
      out ALine: Integer): TNyxHandlerRef;
    procedure DoSetCallbackPolicy(ATrigger: TNyxTrigger; const AName: TNyxEventRef;
      APolicy: TNyxExecutionPolicy);
    procedure DoRemoveCallback(ATrigger: TNyxTrigger; const AName: TNyxEventRef;
      const AID: TNyxCallbackRef);
    { Resolve a value-only canvas ticket on this independently owned session.
      Platform/default projection precedes policy and typed command admission. }
    procedure SetCapturedCanvasValue(const AEdit: TNyxStudioDesignEdit);
  public
    constructor Create; overload;
    { Owned literal planner; reserved text affects name collisions only. It
      cannot create an executed checkpoint or bypass ordinary pair admission. }
    class function ForCallbackPlanning(const APair: TNyxProjectPair;
      const AReservedSource: TNyxText): TNyxStudioSession; static;
    { Admit an independently owned paired seed directly, without constructing
      the demonstration project first. History starts empty. Failed admission
      frees all candidate/session owners; no caller's document is borrowed. }
    constructor Create(const APair: TNyxProjectPair); overload;
    { Stages the accepted pair and navigation before returning a new owner. The
      trusted codec supplies admitted immutable history; default checkpoints and
      out-of-range counters/counts refuse. A failure frees the complete candidate. }
    constructor CreateRecovered(const AFrame: TNyxStudioRecoveryFrame);
    { Equivalent owned rollback constructor; does not consume AOrigin and uses
      its exact existing command identity. Intended for the serialized host. }
    constructor CreateCopy(AOrigin: TNyxStudioSession);
    { Independent owned model/source/history copy for a host transaction rollback.
      The accepted tree is cloned without reparsing Pascal. Queued contexts and
      transient placement remain exact; no mutable owner is shared. }
    function Clone: TNyxStudioSession;
    { Complete immutable paired/history values for a trusted native codec. }
    function RecoveryFrame: TNyxStudioRecoveryFrame;
    { Cheap copied durable metadata. Queries need not serialize all paired files
      or history to determine whether a host checkpoint needs replacement. }
    function RecoveryStamp: TNyxText;
    destructor Destroy; override;
    function Selected: TNyxNode;
    function ActiveView: TNyxNode;
    procedure Select(const AID: TNyxText);
    procedure Activate(const AID: TNyxText);
    procedure SetProperty(const AKey, AValue: TNyxText);
    { Append an independent descriptor clone through the ordinary undoable
      insertion command. The reference-counted control is borrowed; the caller
      retains its implementation/lifetime and the session owns only the clone. }
    procedure AddControl(const AControl: INyxControl);
    { Persist a design-canvas field through one undoable command. The borrowed
      realized node must come from this document's current view. Ordinary fields
      edit their authored owner; inherited reusable fields create/update an
      instance-only named-part override, leaving the definition untouched. }
    procedure SetCanvasValue(ARuntimeNode: TNyxNode);
    { Capture only owned values from a borrowed current-view proposal. Retired
      views and npfAny refuse; no realized node or renderer survives this call.
      Queued consumers replay the exact runtime field, never current selection. }
    function CaptureCanvasValue(ARuntimeNode: TNyxNode;
      APlatform: TNyxPlatform; const AMountContext: TNyxStudioCommandContext): TNyxStudioDesignEdit;
    { Project naming is an undoable document edit, independent of output choices
      and machine tooling. The title is ordinary user text in the portable file. }
    procedure SetTitle(const ATitle: TNyxText);
    { One undoable authored-default edit. Typed assignments and full document
      admission run on a detached clone before history/publication; rejection and
      no-ops preserve the accepted document, selection and redo stack. }
    procedure SetStateValues(const AValues: array of TNyxStateAssignment);
    { Undoable opaque project/control data edits. Scope protects standard fields;
      complete detached admission precedes history. No-ops and rejected edits
      retain document handles and redo. Selection denotes the authored owner. }
    procedure SetExtension(AOwner: TNyxStudioExtensionOwner;
      const AKey: TNyxExtensionRef; const AValue: TNyxDataValue);
    procedure RemoveExtension(AOwner: TNyxStudioExtensionOwner;
      const AKey: TNyxExtensionRef);
    { Open keys and tagged values are the explicit designer input boundary.
      Creation refuses duplicates; rename retains type/order and updates every
      authored binding reference in one command. Removal refuses used defaults. }
    procedure CreateState(const AName: TNyxText; const AValue: TNyxStateValue);
    procedure RenameState(const AOldName, ANewName: TNyxText);
    procedure RemoveState(const AName: TNyxText);
    { Binding metadata edits apply only to the selected authored owner/override.
      Inherit removes the local descriptor; Clear records deliberate unbinding.
      Unsupported targets/defaults fail on a detached complete document. }
    procedure SetBinding(const ASpec: TNyxBindingSpec);
    procedure InheritBinding(ATarget: TNyxBindingProperty);
    { Complete typed collection definitions and view edits are detached, admitted
      commands. Removing a used definition rejects; Clear and Inherit preserve
      their distinct reusable override meanings in paired source/history. }
    procedure DefineCollection(const AKey: TNyxCollectionRef;
      const ASchema: TNyxCollectionSchema; const AItems: array of TNyxCollectionItem);
    procedure RemoveCollection(const AKey: TNyxCollectionRef);
    procedure SetCollectionView(const ASpec: TNyxCollectionViewSpec);
    procedure InheritCollectionView;
    { Read the contract beneath an exact authored local clear mask on an owned
      temporary document. No selection/source/history changes. An unbound
      inherited contract returns undefined; missing/non-cleared owners refuse.
      The returned typed descriptor owns its values beyond this call. }
    function ClearedCollectionInheritance(const AOwner: TNyxText): TNyxCollectionViewSpec;
    { Apply a typed collection proposal on this session's detached candidate.
      Exact field families, scoped item identity and view projection are checked
      before mutation; unknown/missing identities refuse without partial data.
      The ordinary command retains independent Pascal drafts and paired history. }
    procedure ApplyCollectionIntent(const AIntent: TNyxStudioCollectionIntent);
    { Execute one closed event command against this session's current selection.
      All supported-event and reviewed-registration checks precede mutation.
      Add requires accepted Pascal and returns its owned TODO stub reference/line;
      policy/removal retain independent drafts and handwritten implementations. }
    procedure ApplyEventIntent(const AIntent: TNyxStudioEventIntent;
      out AHandler: TNyxHandlerRef; out ALine: Integer);
    { Event edits preserve exact registration identity/order and inheritance on
      detached candidates. Adding a handler is one paired design/source history
      command and returns its TODO line. Removal retains application code. }
    function AddCallback(ATrigger: TNyxTrigger; out ALine: Integer): TNyxHandlerRef; overload;
    function AddCallback(const AName: TNyxEventRef; out ALine: Integer): TNyxHandlerRef; overload;
    procedure SetCallbackPolicy(ATrigger: TNyxTrigger; APolicy: TNyxExecutionPolicy); overload;
    procedure SetCallbackPolicy(const AName: TNyxEventRef; APolicy: TNyxExecutionPolicy); overload;
    procedure RemoveCallback(ATrigger: TNyxTrigger; const AID: TNyxCallbackRef); overload;
    procedure RemoveCallback(const AName: TNyxEventRef; const AID: TNyxCallbackRef); overload;
    { Reorder an exact selected-owner registration through the same detached,
      source-preserving command as inspector policy/removal. Positions are
      zero-based final order; inherited metadata is materialized independently. }
    procedure MoveCallback(ATrigger: TNyxTrigger;
      const AID: TNyxCallbackRef; AIndex: Integer); overload;
    procedure MoveCallback(const AName: TNyxEventRef;
      const AID: TNyxCallbackRef; AIndex: Integer); overload;
    function CallbackLine(const AHandler: TNyxHandlerRef): Integer;
    { Caller owns this independent realized selection (or customized part),
      projected against saved defaults. Removed part rules return nil. }
    function SelectedProjection: TNyxNode;
    procedure AddKind(const AKind: TNyxText);
    procedure DeleteSelected;
    procedure DuplicateSelected;
    procedure MoveSelected(ADirection: Integer);
    procedure AddPage;
    procedure CreateComponent;
    procedure AddComponentInstance(const ADefinitionID: TNyxText);
    { Select an independently owned customization of a named reusable part.
      The ordinary property inspector edits it. Adding through the palette then
      appends content when that part is a container, through the public model API. }
    procedure CustomizePart(const APath: TNyxText);
    procedure Undo;
    procedure Redo;
    procedure Load(const ASource: TNyxText);
    function Save: TNyxText;
    function Source: TNyxText;
    { Snapshot pairs accepted files and the exact pending draft/base. Load stages
      the whole pair before one publication and deliberately starts new history. }
    function ProjectSnapshot: TNyxProjectPair;
    procedure LoadProject(const APair: TNyxProjectPair;
      AResolution: TNyxProjectResolution = nprRequireMatch);
    { Ordinary editor Open keeps the previous complete pair as one Undo command.
      Unsupported/conflicting files refuse before any accepted owner is changed. }
    procedure OpenProject(const APair: TNyxProjectPair;
      AResolution: TNyxProjectResolution = nprRequireMatch);
    { Explicit compiler-backed file/recovery opening. Capture validates the
      saved design and packet but grants no authority to a serialized origin.
      Complete receives fully admitted independent owners from the configured
      compiler and preserves pending draft/base verbatim. Default loading starts
      fresh history; an explicit editor policy records one paired command.
      Stale/failing results retain owners, navigation, buffers and Undo/Redo. }
    function PrepareProjectRequest(const APair: TNyxProjectPair;
      AResolution: TNyxProjectResolution; ASchemaRevision: Integer;
      AHistory: TNyxProjectOpeningHistory = pohNewHistory): TNyxStudioProjectRequest;
    { Read-only dispatch guard: imported intent must still belong to this exact
      load, live pair and creator generation before any external compiler work. }
    function ProjectRequestCurrent(const ARequest: TNyxStudioProjectRequest): Boolean;
    function CompleteProjectRequest(const ARequest: TNyxStudioProjectRequest;
      const APrepared: INyxPreparedSource): TNyxSourceCompletion;
    { Explicit owning-editor load of a live admitted source frame. Stages current
      property/creator admission and resets history only after success. This is
      distinct from general file/recovery admission through LoadProject. }
    procedure LoadCapturedProject(const APair: TNyxProjectPair;
      const ACheckpoint: TNyxSourceCheckpoint);
    { Shared semantic command boundary for MCP and other controllers. All
      related edits stage a complete candidate and publish once with one paired
      undo checkpoint. Pending source drafts block external design mutation. }
    procedure ApplyPatch(const APatch: INyxDesignPatch);
    { One ordinary candidate/paired Undo, then activate the moved control's
      owning view. Callers borrow no accepted nodes across publication. }
    procedure Place(const AChange: TNyxPlacementChange);
    { Explicit authored controls/instances only; inherited part descriptors
      retain their existing Inspector customization path. One paired Undo. }
    procedure Resize(const AChange: TNyxResizeChange);
    function CaptureResize(const AChange: TNyxResizeChange;
      const AMountContext: TNyxStudioCommandContext): TNyxStudioDesignEdit;
    { A baseline absolute-layout origin is one paired command. Conditional or
      bound origins refuse instead of guessing an authored scope. Capture owns
      copied values and verifies the borrowed mount's session/load identity. }
    procedure Position(const AChange: TNyxPositionChange);
    function CapturePosition(const AChange: TNyxPositionChange;
      const AMountContext: TNyxStudioCommandContext): TNyxStudioDesignEdit;
    { Arm the selected authored control, then choose a destination through the
      ordinary canvas/hierarchy. Arming/canceling are presentation, not Undo.
      A changed pair, draft or project load invalidates this pending move. }
    procedure BeginPlacement;
    procedure CancelPlacement;
    property PlacementSource: TNyxControlRef read GetPlacementSource;
    { Capture value-only intent from a still-mounted editor. Retired session/
      load contexts refuse before enqueue, even when identities match. }
    function CapturePlacement(const AChange: TNyxPlacementChange;
      const AMountContext: TNyxStudioCommandContext): TNyxStudioDesignEdit;
    { Reserve a readable unused catalog identity, then capture a value-only new
      placement. Reservation advances only the private name counter; accepted
      content/history change only through the ordinary isolated processor. }
    function CaptureNewPlacement(const AKind: TNyxKindRef;
      const ATarget: TNyxControlRef; APlacement: TNyxPlacement;
      const AMountContext: TNyxStudioCommandContext): TNyxStudioDesignEdit;
    { Apply an immutable reviewed root group through one paired Undo command.
      Stale reviews, dangling reusable references and pending drafts retain all
      owners/history. Imports/helpers and document state remain deliberate. }
    procedure RemoveRoots(const AReview: INyxRootRemoval);
    { Admit exact paired files/draft through one command by default. The explicit
      synchronization policy retains draft-only typing as metadata, without
      clearing existing Redo. Unlike LoadProject, neither policy resets history.
      Failed admission retains all owners, exact buffers and both history lists. }
    procedure AdoptProject(const APair: TNyxProjectPair;
      AAdoption: TNyxStudioProjectAdoption = spaCommand);
    { Trusted owning compiler/observer seams, never project wire admission.
      Executed results or opaque live checkpoints must match both accepted files.
      These use the same draft and paired history policy as ordinary adoption. }
    procedure AdoptProjectedProject(const APair: TNyxProjectPair;
      const AProjection: INyxSourceProjection;
      AAdoption: TNyxStudioProjectAdoption = spaCommand);
    procedure AdoptCapturedProject(const APair: TNyxProjectPair;
      const ACheckpoint: TNyxSourceCheckpoint;
      AAdoption: TNyxStudioProjectAdoption = spaSynchronization);
    { Opaque immutable live value. Capturing synchronizes accepted owners, never
      parses a pending buffer; no wire codec or compiler authority is returned. }
    function AcceptedSourceCheckpoint: TNyxSourceCheckpoint;
    function CanUndo: Boolean;
    function CanRedo: Boolean;
    { Source is accepted Pascal; DraftSource is the editable buffer. Applying a
      draft stages both model and companion source before one history entry.
      Unsupported/invalid edits retain the buffer, document and redo history. }
    function DraftSource: TNyxText;
    procedure SetSourceDraft(const ASource: TNyxText);
    { Typed file buffers remain independent. Main dispatches to the established
      main draft contract; auxiliaries keep their first accepted base until an
      explicit discard or a future acknowledged file application. }
    function SourceFileText(const AFile: TNyxPascalSourceRef): TNyxText;
    { Immutable accepted inputs for read-only library consumers. Independent
      unfinished buffers are excluded. The cached identity changes with any
      accepted source/inventory change; this grants no compilation provenance. }
    function AcceptedPascalProject: INyxPascalProject;
    { Stable current inventory order, with the main buffer at zero. Returned
      references are values; missing members after project changes refuse. }
    function SourceFileCount: Integer;
    function SourceFile(AIndex: Integer): TNyxPascalSourceRef;
    { Display a typed leaf without reading every helper's text. Main resolves
      the current namespace; membership is enforced when selecting/reading. }
    function SourceFileTitle(const AFile: TNyxPascalSourceRef): TNyxText;
    property ActiveSourceFile: TNyxPascalSourceRef read GetActiveSourceFile write SetActiveSourceFile;
    property ActiveSourceText: TNyxText read GetActiveSourceText;
    function DraftFileText(const AFile: TNyxPascalSourceRef): TNyxText;
    procedure SetFileDraft(const AFile: TNyxPascalSourceRef; const AText: TNyxText);
    procedure DiscardFileDraft(const AFile: TNyxPascalSourceRef);
    property FileDrafts: TNyxPascalFileDrafts read FFileDrafts;
    function SourceDraftsPending: Boolean;
    { Cached editor state for physical input guards. Reading this flag neither
      renders/encodes the document nor rebases a pending draft. }
    property SourceDraftPending: Boolean read FSourceDraftPending;
    { Recovery retains the draft's original accepted source, so reloading cannot
      turn a stale draft into an edit of a newer visual design. }
    procedure RestoreSourceDraft(const ASource, ABase: TNyxText);
    procedure ApplySourceDraft;
    { Capture a fresh accepted baseline before dispatching isolated admission.
      Pending drafts retain their original base; an already stale draft refuses.
      Complete compares fresh document/source values and creator publication,
      then transfers both owners through one paired Undo entry. Rejected/stale
      results never replace the accepted pair or erase the current draft. }
    function PrepareSourceRequest(ASchemaRevision: Integer): TNyxStudioSourceRequest;
    { Capture one helper against accepted complete inputs. A stale first base
      refuses; unfinished unrelated files never enter compilation. }
    function PrepareFileRequest(const AFile: TNyxPascalSourceRef;
      ASchemaRevision: Integer): TNyxStudioSourceRequest;
    { Create/remove one helper against accepted complete inputs, retaining every
      independent buffer in dispatch history. The result grants no execution. }
    function PrepareFileChangeRequest(const AChange: TNyxPascalFileChange;
      ASchemaRevision: Integer): TNyxStudioSourceRequest;
    { Review does not prepare/validate a removal candidate, so even a still-used
      dependency receives its warning before confirmation can refuse it. }
    function ReviewFileRemoval(const AFile: TNyxPascalSourceRef): TNyxStudioFileRemovalReview;
    function PrepareReviewedFileRemoval(const AReview: TNyxStudioFileRemovalReview;
      ASchemaRevision: Integer): TNyxStudioSourceRequest;
    { FIFO dispatch may follow another file's Apply. Rebase only when this
      selected file/buffer/base and project load still match their exact ticket. }
    function RefreshFileRequest(const ARequest: TNyxStudioSourceRequest;
      ASchemaRevision: Integer): TNyxStudioSourceRequest;
    { Capture an explicit whole-source proposal without replacing the visible
      buffer. Pending drafts and unchanged proposals refuse. This sealed value
      grants no execution; an owning host must supply an admitted processor
      result at the same session/load/schema and exact accepted baseline. }
    function PrepareSourceEditRequest(const ASource: TNyxText;
      ASchemaRevision: Integer): TNyxStudioSourceRequest; overload;
    { Stage one complete source-only group. Membership/order/main identity and
      limits stay fixed; all unfinished buffers must be resolved first. }
    function PrepareSourceEditRequest(const AProject: INyxPascalProject;
      ASchemaRevision: Integer): TNyxStudioSourceRequest; overload;
    { Explicit membership proposal. Typed intents determine the exact final
      inventory/order; related source ranges may repair dependencies together.
      All drafts refuse and only subsequent complete construction can publish. }
    function PrepareSourceFileChanges(const AChanges: array of TNyxPascalFileChange;
      const AProject: INyxPascalProject;
      ASchemaRevision: Integer): TNyxStudioSourceRequest;
    function CompleteSourceRequest(const ARequest: TNyxStudioSourceRequest;
      const APrepared: INyxPreparedSource): TNyxSourceCompletion;
    { Capture a queued explicit design intent against the fresh current pair.
      Processing and paired publication remain separate; captured selection/view
      never follow a later user navigation. }
    function PrepareDesignRequest(const AEdit: TNyxStudioDesignEdit;
      ASchemaRevision: Integer): TNyxStudioDesignRequest;
    { Capture/compare on the UI thread. Reloading identical files retires this
      context, preventing waiting commands from being replayed on another load. }
    function CommandContext: TNyxStudioCommandContext;
    function MatchesCommandContext(const AContext: TNyxStudioCommandContext): Boolean;
    function CompleteDesignRequest(const ARequest: TNyxStudioDesignRequest;
      const APrepared: INyxPreparedDesign): TNyxSourceCompletion;
    procedure DiscardSourceDraft;
    { Companion source has independent local recovery. Import first loads the
      portable design, then applies its paired Pascal. .nyx export stays portable. }
    property Document: TNyxDocument read FDocument;
    property Catalog: TNyxCatalog read FCatalog;
    property SelectedID: TNyxText read FSelectedID;
    property ActiveViewID: TNyxText read FActiveViewID;
    property SourceDraftBase: TNyxText read FSourceDraftBase;
    { Read-only presentation of the most recent failed Apply. It is valid only
      for the exact current draft, is never saved as design/history data, and
      clears on editing/restoring/success. Exceptions still propagate normally. }
    property SourceDiagnostic: TNyxSourceDiagnostic read GetSourceDiagnostic;
    { Meaningful only with a currently defined SourceDiagnostic. It selects the
      copied target buffer without borrowing a report or changing paired history. }
    property SourceDiagnosticFile: TNyxPascalSourceRef read FSourceDiagnosticFile;
  end;

{ Private worker protocol only. File/HTTP/MCP imports keep strict project/source
  admission. Decode validates closed fields and exact values, without publishing. }
function ReadNyxStudioDesignRequest(const AData: TNyxDataValue): TNyxStudioDesignRequest;
{ Decode semantic intent only using the existing closed editor grammar. The
  result is data, never a sealed request or source-execution authority. }
function ReadNyxStudioDesignIntent(const AData: TNyxDataValue): TNyxStudioDesignEdit;
function PrepareNyxStudioDesign(const ARequest: TNyxStudioDesignRequest;
  const ASchemas: INyxSchemaSnapshot): INyxPreparedDesign;
{ Trusted compiler continuation of one detached visual proposal. Executed source
  and canonical design must match it exactly under captured creators. It returns
  independent paired owners; CompleteDesignRequest still performs the fresh
  owner/load/source/draft/schema guard and supplies one paired Undo step. }
function PrepareNyxCompiledDesign(const ARequest: TNyxStudioDesignRequest;
  const AProposal: INyxPreparedDesign; const AProjection: INyxSourceProjection;
  const ASchemas: INyxSchemaSnapshot): INyxPreparedDesign;
function ReceiveNyxPreparedDesign(const AData: TNyxDataValue;
  const ARequest: TNyxStudioDesignRequest;
  const ASchemas: INyxSchemaSnapshot): INyxPreparedDesign;

implementation

uses
  nyx.pascal.names,
  nyx.binding,
  nyx.composition,
  nyx.platform,
  nyx.interaction,
  nyx.contract,
  nyx.menu.editor,
  nyx.menu.bar.editor,
  nyx.canvas.editor,
  nyx.studio.projectionediting,
  nyx.studio.compiler,
  nyx.studio.callbackedits, nyx.studio.handleredits;

type
  { UI publication borrows the session only inside its synchronous creator guard.
    The action owns both candidate resources until the paired swap clears them. }
  TSourcePairPublication = class(TInterfacedObject, INyxSchemaAction)
  public
    Session: TNyxStudioSession;
    Document: TNyxDocument;
    Workspace: TNyxSourceWorkspace;
    Checkpoint: TNyxSourceCheckpoint;
    { Fallback publication retains an independent main draft when requested.
      Selected-file and visual completions supply CapturedHistory explicitly. }
    RememberDraft: Boolean;
    { Only a sealed selected-helper command may replace the captured inventory. }
    AllowProjectFiles: Boolean;
    { Prepared before publication, so consuming a helper buffer cannot allocate
      or fail after the accepted owners and paired history have been swapped. }
    RemainingFileDrafts: TNyxPascalFileDrafts;
    { Explicit source/visual requests retain their dispatch frame. Other admitted
      operations use the current-buffer policy through PublishCapturedPair. }
    UseCapturedHistory: Boolean;
    CapturedHistory: TNyxStudioCheckpoint;
    destructor Destroy; override;
    procedure Execute;
  end;
  { Synchronous creator-guarded opening owns both replacements until the chosen
    history policy publishes them together. Its session borrow exists only for
    CommitNyxSchemaRevision, never on a worker. }
  TProjectLoadPublication = class(TInterfacedObject, INyxSchemaAction)
  public
    Session: TNyxStudioSession;
    Document: TNyxDocument;
    Workspace: TNyxSourceWorkspace;
    Pair: TNyxProjectPair;
    History: TNyxProjectOpeningHistory;
    destructor Destroy; override;
    procedure Execute;
  end;

destructor TProjectLoadPublication.Destroy;
begin
  Workspace.Free;
  Document.Free;
  inherited Destroy;
end;

procedure TProjectLoadPublication.Execute;
begin

  if History = pohPairedCommand then
  begin
    Session.PublishOpenedProject(Document, Workspace, Pair);
  end
  else
  begin
    Session.LoadAdmittedProject(Document, Workspace, Pair);
    Document := nil;
    Workspace := nil;
  end;
end;

destructor TSourcePairPublication.Destroy;
begin
  Workspace.Free;
  Document.Free;
  inherited Destroy;
end;

procedure TSourcePairPublication.Execute;
begin
  { Source Apply consumes its staging buffer; visual editing retains it. Both
    use the same atomic owner swap with their explicit history policy. }

  if UseCapturedHistory then
  begin
    Session.PublishHistoryPair(Document, Workspace, Checkpoint, CapturedHistory,
      AllowProjectFiles);
  end
  else
  begin
    Session.PublishCapturedPair(Document, Workspace, Checkpoint, RememberDraft,
      AllowProjectFiles);
  end;

  if AllowProjectFiles then
  begin
    Session.FFileDrafts := RemainingFileDrafts;
  end;
end;

constructor TNyxStudioSession.CreateRecovered(const AFrame: TNyxStudioRecoveryFrame);
var
  LIndex: Integer;
begin
  inherited Create;
  InitializeOwners;

  if AFrame.AcceptedCheckpoint.Design <> '' then
  begin
    LoadCapturedProject(AFrame.Pair, AFrame.AcceptedCheckpoint);
  end
  else
  begin
    LoadProject(AFrame.Pair);
  end;

  if (AFrame.NextID < 0) or (Length(AFrame.Undo) + Length(AFrame.Redo) > 50) then
  begin
    raise ENyxModel.Create('Recovery counters/history exceed the session budget');
  end;

  if ((AFrame.Selection <> '') and (FDocument.Find(AFrame.Selection) = nil)) or
    ((AFrame.View <> '') and ((FDocument.Find(AFrame.View) = nil) or
      (FDocument.Find(AFrame.View).Parent <> nil))) then
  begin
    raise ENyxModel.Create('Recovery navigation is outside the accepted document');
  end;
  for LIndex := 0 to High(AFrame.Undo) do
  begin

    if AFrame.Undo[LIndex].Design = '' then
    begin
      raise ENyxModel.Create('Recovery Undo checkpoint is not admitted');
    end;
    FUndo.Add(AFrame.Undo[LIndex]);
  end;
  for LIndex := 0 to High(AFrame.Redo) do
  begin

    if AFrame.Redo[LIndex].Design = '' then
    begin
      raise ENyxModel.Create('Recovery Redo checkpoint is not admitted');
    end;
    FRedo.Add(AFrame.Redo[LIndex]);
  end;
  FSelectedID := AFrame.Selection;
  FActiveViewID := AFrame.View;
  FNextID := AFrame.NextID;
end;

function TNyxStudioSession.RecoveryFrame: TNyxStudioRecoveryFrame;
var
  LIndex: Integer;
begin
  Result.Pair := ProjectSnapshot;
  Result.AcceptedCheckpoint := AcceptedSourceCheckpoint;
  Result.Selection := FSelectedID;
  Result.View := FActiveViewID;
  Result.NextID := FNextID;
  SetLength(Result.Undo, FUndo.Count);
  SetLength(Result.Redo, FRedo.Count);
  for LIndex := 0 to FUndo.Count - 1 do
  begin
    Result.Undo[LIndex] := FUndo.State(LIndex);
  end;
  for LIndex := 0 to FRedo.Count - 1 do
  begin
    Result.Redo[LIndex] := FRedo.State(LIndex);
  end;
end;

function TNyxStudioSession.RecoveryStamp: TNyxText;
begin
  Result := NyxObject([NyxField('selection', NyxData(FSelectedID)),
    NyxField('view', NyxData(FActiveViewID)), NyxField('nextID', NyxData(FNextID)),
    NyxField('undo', NyxData(FUndo.Count)), NyxField('redo', NyxData(FRedo.Count))]).ToJSON;
end;

function TNyxStudioSession.Clone: TNyxStudioSession;
begin
  Result := TNyxStudioSession.CreateCopy(Self);
end;

constructor TNyxStudioSession.CreateCopy(AOrigin: TNyxStudioSession);
var
  LIndex: Integer;
begin
  inherited Create;

  if AOrigin = nil then
  begin
    raise ENyxModel.Create('A session rollback copy requires its origin');
  end;
  FCatalog := TNyxCatalog.Create;
  FUndo := TNyxStudioHistory.Create;
  FRedo := TNyxStudioHistory.Create;
  FSourceWorkspace := TNyxSourceWorkspace.Create;
  FDocument := AOrigin.FDocument.Clone;
  FSourceWorkspace.Restore(AOrigin.FSourceWorkspace.Capture(AOrigin.FDocument));
  for LIndex := 0 to AOrigin.FUndo.Count - 1 do
  begin
    FUndo.Add(AOrigin.FUndo.State(LIndex));
  end;
  for LIndex := 0 to AOrigin.FRedo.Count - 1 do
  begin
    FRedo.Add(AOrigin.FRedo.State(LIndex));
  end;
  FSourceDraft := AOrigin.FSourceDraft;
  FSourceDraftBase := AOrigin.FSourceDraftBase;
  FSourceDraftPending := AOrigin.FSourceDraftPending;
  FFileDrafts := AOrigin.FFileDrafts;
  FSourceDiagnostic := AOrigin.FSourceDiagnostic;
  FSourceDiagnosticSource := AOrigin.FSourceDiagnosticSource;
  FSourceDiagnosticFile := AOrigin.FSourceDiagnosticFile;
  FSourceIdentity := AOrigin.FSourceIdentity;
  FSourceGeneration := AOrigin.FSourceGeneration;
  FSelectedID := AOrigin.FSelectedID;
  FActiveViewID := AOrigin.FActiveViewID;
  FNextID := AOrigin.FNextID;
  FPlacementSource := AOrigin.FPlacementSource;
  FPlacementContext := AOrigin.FPlacementContext;
  FPlacementPair := AOrigin.FPlacementPair;
end;

class function TNyxStudioSession.ForCallbackPlanning(const APair: TNyxProjectPair;
  const AReservedSource: TNyxText): TNyxStudioSession;
begin
  Result := TNyxStudioSession.Create(APair);
  Result.FCallbackReservedSource := AReservedSource;
end;

constructor TNyxStudioSession.Create;
var
  LIdentity: TGUID;
begin
  inherited Create;
  CreateGUID(LIdentity);
  FSourceIdentity := GUIDToString(LIdentity);
  FDocument := CreateNyxSample;
  FCatalog := TNyxCatalog.Create;
  FUndo := TNyxStudioHistory.Create;
  FRedo := TNyxStudioHistory.Create;
  FSourceWorkspace := TNyxSourceWorkspace.Create;
  FActiveViewID := FDocument.Pages[0].ID;
  FSelectedID := FActiveViewID;
end;

procedure TNyxStudioSession.InitializeOwners;
var
  LIdentity: TGUID;
begin
  CreateGUID(LIdentity);
  FSourceIdentity := GUIDToString(LIdentity);
  FDocument := TNyxDocument.Create;
  FCatalog := TNyxCatalog.Create;
  FUndo := TNyxStudioHistory.Create;
  FRedo := TNyxStudioHistory.Create;
  FSourceWorkspace := TNyxSourceWorkspace.Create;
end;

constructor TNyxStudioSession.Create(const APair: TNyxProjectPair);
begin
  inherited Create;
  InitializeOwners;
  LoadProject(APair);
end;

destructor TNyxStudioSession.Destroy;
begin
  FSourceWorkspace.Free;
  FRedo.Free;
  FUndo.Free;
  FCatalog.Free;
  FDocument.Free;
  inherited Destroy;
end;

function TNyxStudioSession.NewID(const AKind: TNyxText): TNyxText;
begin
  repeat
    Inc(FNextID);
    Result := AKind + '-' + IntToStr(FNextID);
  until FDocument.Find(Result) = nil;
end;

procedure TNyxStudioSession.Checkpoint;
  {$ifdef NYX_SOURCE_PROFILE}
var
  LStarted: Double;
  {$endif}
begin
  {$ifdef NYX_SOURCE_PROFILE}LStarted := SourceProfileStart;{$endif}
  { Capture encodes the current public document once, synchronizes authored
    source, then stores the complete immutable pair in one history entry. }
  FUndo.Add(CaptureHistory);
  {$ifdef NYX_SOURCE_PROFILE}SourceProfileFinish(spCheckpoint, LStarted);{$endif}
end;

procedure TNyxStudioSession.TrimUndoHistory;
begin
  { Keep the latest fifty commands within a 16 MiB retained-text budget. The
    immutable checkpoint carries its paired design once and accounting uses real
    target text bytes, without JSON escaping or a second design history list.
    Retain one oversized entry so the immediately previous command stays undoable. }
  while FUndo.Count > 50 do
  begin
    FUndo.Delete(0);
  end;
  while (FUndo.StorageBytes > 16 * 1024 * 1024) and (FUndo.Count > 1) do
  begin
    FUndo.Delete(0);
  end;
end;

procedure TNyxStudioSession.Rollback;
begin
  RestorePair(FUndo.LastState);
  FUndo.Delete(FUndo.Count - 1);
end;

procedure TNyxStudioSession.Commit;
{$ifdef NYX_SOURCE_PROFILE}
var
  LStarted: Double;
{$endif}
begin
  {$ifdef NYX_SOURCE_PROFILE}LStarted := SourceProfileStart;{$endif}
  { Admit the entire candidate after a command. Invalid references/cycles restore
    the accepted snapshot, including the old redo history, before surfacing the
    diagnostic. The next command always starts from a valid document. }
  try
    ValidateNyxDocumentProperties(FDocument);
    FSourceWorkspace.Render(FDocument);
  except
    Rollback;
    raise;
  end;
  FRedo.Clear;
  TrimUndoHistory;
  CancelPlacement;
  {$ifdef NYX_SOURCE_PROFILE}SourceProfileFinish(spCommit, LStarted);{$endif}
end;

procedure TNyxStudioSession.Restore(const ASource: TNyxText);
var
  LCandidate: TNyxDocument;
  {$ifdef NYX_SOURCE_PROFILE}
  LStarted: Double;
  {$endif}
begin
  {$ifdef NYX_SOURCE_PROFILE}LStarted := SourceProfileStart;{$endif}
  LCandidate := TNyxCodec.Decode(ASource);
  try
    ValidateNyxDocumentProperties(LCandidate);
  except
    LCandidate.Free;
    raise;
  end;
  FDocument.Free;
  FDocument := LCandidate;

  if FDocument.Find(FActiveViewID) = nil then
  begin
    FActiveViewID := '';

    if FDocument.Count > 0 then
    begin
      FActiveViewID := FDocument.Pages[0].ID;
    end
    else if FDocument.ComponentCount > 0 then
    begin
      FActiveViewID := FDocument.Components[0].ID;
    end;
  end;

  if FDocument.Find(FSelectedID) = nil then
  begin
    FSelectedID := FActiveViewID;
  end;
  {$ifdef NYX_SOURCE_PROFILE}SourceProfileFinish(spRestoreDocument, LStarted);{$endif}
end;

function TNyxStudioSession.Selected: TNyxNode;
begin
  Result := FDocument.Find(FSelectedID);
end;

function TNyxStudioSession.CaptureHistory: TNyxStudioCheckpoint;
begin
  Result := NyxStudioCheckpoint(FSourceWorkspace.Capture(FDocument),
    FSourceDraft, FSourceDraftBase, FSourceDraftPending, FFileDrafts);
end;

procedure TNyxStudioSession.RestorePair(const ACheckpoint: TNyxStudioCheckpoint;
  ARemember: TNyxStudioHistory);
var
  LCandidate: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LPrevious: TNyxDocument;
  LPreviousSource: TNyxSourceWorkspace;
  {$ifdef NYX_SOURCE_PROFILE}
  LStarted: Double;
  {$endif}
begin
  LCandidate := nil;
  LWorkspace := nil;
  try
    {$ifdef NYX_SOURCE_PROFILE}LStarted := SourceProfileStart;{$endif}
    LCandidate := TNyxCodec.Decode(ACheckpoint.Design);
    ValidateNyxDocumentProperties(LCandidate);
    {$ifdef NYX_SOURCE_PROFILE}SourceProfileFinish(spRestoreDocument, LStarted);{$endif}
    LWorkspace := TNyxSourceWorkspace.Create;
    LWorkspace.Restore(ACheckpoint.Frame);

    if ARemember <> nil then
    begin
      { Current public nodes may have changed since the previous command. Never
        remember a stale frame; a failed reconciliation leaves the target pair
        unpublished and the history entry unrecorded. }
      ARemember.Add(CaptureHistory);
    end;
    LPrevious := FDocument;
    LPreviousSource := FSourceWorkspace;
    FDocument := LCandidate;
    LCandidate := nil;
    FSourceWorkspace := LWorkspace;
    LWorkspace := nil;
    LPrevious.Free;
    LPreviousSource.Free;

    { Admission and opposite-history allocation precede the complete swap.
      Restore immutable draft values without parsing/rendering after publication. }
    DiscardSourceDraft;
    FSourceDraft := ACheckpoint.Draft;
    FSourceDraftBase := ACheckpoint.DraftBase;
    FSourceDraftPending := ACheckpoint.Pending;
    FFileDrafts := ACheckpoint.FileDrafts;

    if FDocument.Find(FActiveViewID) = nil then
    begin
      FActiveViewID := '';

      if FDocument.Count > 0 then
      begin
        FActiveViewID := FDocument.Pages[0].ID;
      end
      else if FDocument.ComponentCount > 0 then
      begin
        FActiveViewID := FDocument.Components[0].ID;
      end;
    end;

    if FDocument.Find(FSelectedID) = nil then
    begin
      FSelectedID := FActiveViewID;
    end;
  finally
    LWorkspace.Free;
    LCandidate.Free;
  end;
end;

function TNyxStudioSession.ActiveView: TNyxNode;
begin
  Result := FDocument.Find(FActiveViewID);
end;

procedure TNyxStudioSession.Select(const AID: TNyxText);
begin

  if FDocument.Find(AID) = nil then
  begin
    raise ENyxModel.Create('Selection is missing');
  end;
  FSelectedID := AID;
end;

procedure TNyxStudioSession.Activate(const AID: TNyxText);
begin

  if FDocument.Find(AID) = nil then
  begin
    raise ENyxModel.Create('View is missing');
  end;
  FActiveViewID := AID;
  FSelectedID := AID;
end;

procedure TNyxStudioSession.SetProperty(const AKey, AValue: TNyxText);
begin

  if Selected = nil then
  begin
    raise ENyxModel.Create('Select a component first');
  end;
  Checkpoint;
  try
    Selected.SetProp(AKey, AValue);
  except
    Rollback;
    raise;
  end;
  Commit;
end;

function TNyxStudioSession.CaptureCanvasValue(ARuntimeNode: TNyxNode;
  APlatform: TNyxPlatform; const AMountContext: TNyxStudioCommandContext): TNyxStudioDesignEdit;
var
  LRoot: TNyxNode;
begin

  if (ARuntimeNode = nil) or not ARuntimeNode.IsRealized or (APlatform = npfAny) then
  begin
    raise ENyxModel.Create('Canvas input requires a realized field and concrete platform');
  end;

  if not MatchesCommandContext(AMountContext) then
  begin
    raise ENyxModel.Create('Canvas input belongs to an earlier mounted project load');
  end;
  LRoot := ARuntimeNode;
  while LRoot.Parent <> nil do
  begin
    LRoot := LRoot.Parent;
  end;

  if (LRoot.DesignID <> FActiveViewID) or
    (FDocument.Find(ARuntimeNode.DesignID) = nil) then
  begin
    raise ENyxModel.Create('Canvas input belongs to a retired or different view');
  end;
  Result := Default(TNyxStudioDesignEdit);
  Result.Action := sdaCanvasValue;
  Result.Selection := ARuntimeNode.DesignID;
  Result.View := LRoot.DesignID;
  Result.Name := ARuntimeNode.ID;
  Result.Value := ARuntimeNode.Prop(NyxAttributeName(atValue));
  Result.Platform := APlatform;
  Result.FCanvasContext := AMountContext;
end;

procedure TNyxStudioSession.SetCapturedCanvasValue(const AEdit: TNyxStudioDesignEdit);
var
  LRoot: TNyxNode;
  LField: TNyxNode;
begin

  if AEdit.Platform = npfAny then
  begin
    raise ENyxModel.Create('Canvas replay requires its concrete platform');
  end;
  LRoot := RealizeNyxView(FDocument, ActiveView);
  try
    ApplyNyxPlatform(LRoot, AEdit.Platform);
    ApplyNyxBindings(LRoot, FDocument.State);
    LField := LRoot.Find(AEdit.Name);

    if (LField = nil) or (LField.DesignID <> AEdit.Selection) then
    begin
      raise ENyxModel.Create('Canvas runtime field no longer belongs to its captured owner');
    end;
    { The existing typed authoring command owns default/override semantics.
      This independent realization supplies its fresh inherited bindings and
      named-part ancestry; no adapter proposal node is trusted or borrowed. }
    { Adapter text is a wire proposal, including numeric/Boolean spellings.
      SetCanvasValue decodes it through the field's typed binding/default
      contract; public Configure.Value(String) deliberately permits text only. }
    LField.SetProp(NyxAttributeName(atValue), AEdit.Value);
    SetCanvasValue(LField);
  finally
    LRoot.Free;
  end;
end;

procedure TNyxStudioSession.SetCanvasValue(ARuntimeNode: TNyxNode);
var
  LBinding: TNyxBindingSpec;
  LStateValue: TNyxStateValue;
  LOwner: TNyxNode;
  LTarget: TNyxNode;
  LPart: TNyxNode;
  LPath: TNyxText;
  LValue: TNyxText;
  LCount: Integer;
  LIndex: Integer;
begin

  if (ARuntimeNode = nil) or not ARuntimeNode.IsRealized then
  begin
    raise ENyxModel.Create('Canvas value requires a realized field');
  end;
  LOwner := FDocument.Find(ARuntimeNode.DesignID);

  if LOwner = nil then
  begin
    raise ENyxModel.Create('Canvas field has no editable owner');
  end;

  if not NyxInteractionPolicy(ARuntimeNode).CanEditValue or
    (NyxBindingKinds(ARuntimeNode, bpValue) = []) then
  begin
    raise ENyxState.Create('Canvas field is not editable');
  end;
  LValue := ARuntimeNode.Prop('value');

  if ARuntimeNode.FindBinding(bpValue, LBinding) then
  begin
    { A bound canvas edit changes its authored default, never a hidden fallback
      property that would immediately be overwritten by the projection. }

    if LBinding.Direction <> bdTwoWay then
    begin
      raise ENyxState.Create('This value projects from state; edit its default in State');
    end;
    LStateValue := FDocument.State.Value(LBinding.StateName);
    LStateValue := ParseNyxStudioStateInput(NyxStudioStateInputFor(LStateValue), LValue);
    SetStateValues([NyxStateAssign(LBinding.StateName, LStateValue)]);
    Exit;
  end;
  LTarget := LOwner;
  LPath := '.';

  if LOwner.ProjectionKind = 'component' then
  begin
    { Root fields use ".". Descendants follow the public direct named-part path,
      including expanded nested references. An unnamed edge cannot be addressed
      unambiguously; refuse it rather than changing shared template defaults. }
    LPart := ARuntimeNode;
    LPath := '';
    while (LPart.Parent <> nil) and (LPart.Parent.DesignID = LOwner.ID) do
    begin

      if LPart.Prop('part') = '' then
      begin
        raise ENyxModel.Create('Reusable canvas field needs a named part to edit');
      end;

      if LPath = '' then
      begin
        LPath := LPart.Prop('part');
      end
      else
      begin
        LPath := LPart.Prop('part') + '/' + LPath;
      end;
      LPart := LPart.Parent;
    end;

    if LPath = '' then
    begin
      LPath := '.';
    end;
    LTarget := nil;
    for LIndex := 0 to LOwner.Count - 1 do
    begin

      if LOwner.Children[LIndex].Prop('path') = LPath then
      begin
        LTarget := LOwner.Children[LIndex];
      end;
    end;
  end;

  if (LTarget <> nil) and (LTarget.Prop('value') = LValue) then
  begin
    Exit;
  end;
  Checkpoint;
  try

    if LTarget = nil then
    begin
      LCount := LOwner.Count;
      { Empty mode preserves an existing append/replace descriptor if present. }
      LTarget := LOwner.OverridePart(LPath);

      if LOwner.Count > LCount then
      begin
        LTarget.Named(NewID('part'));
      end;
    end;
    LTarget.SetProp('value', LValue);
  except
    Rollback;
    raise;
  end;
  Commit;
end;

procedure TNyxStudioSession.SetTitle(const ATitle: TNyxText);
begin

  if FDocument.Title = ATitle then
  begin
    Exit;
  end;
  Checkpoint;
  FDocument.Title := ATitle;
  Commit;
end;

procedure TNyxStudioSession.PublishCandidate(var ACandidate: TNyxDocument);
begin
  PublishCandidate(ACandidate, []);
end;

procedure TNyxStudioSession.PublishCandidate(var ACandidate: TNyxDocument;
  const ARenames: array of TNyxSourceStateRename);
var
  LWorkspace: TNyxSourceWorkspace;
  LCheckpoint: TNyxSourceCheckpoint;
begin
  ValidateNyxDocumentProperties(ACandidate);
  LCheckpoint := FSourceWorkspace.Capture(FDocument);

  if TNyxCodec.Encode(ACandidate) = LCheckpoint.Design then
  begin
    Exit;
  end;
  LWorkspace := TNyxSourceWorkspace.Create;
  try
    LWorkspace.Restore(LCheckpoint);
    LWorkspace.Render(ACandidate, ARenames);
    PublishPair(ACandidate, LWorkspace);
  finally
    LWorkspace.Free;
  end;
end;

procedure TNyxStudioSession.PublishPair(var ACandidate: TNyxDocument;
  var AWorkspace: TNyxSourceWorkspace);
begin
  PublishCapturedPair(ACandidate, AWorkspace, FSourceWorkspace.Capture(FDocument));
end;

procedure TNyxStudioSession.PublishCapturedPair(var ACandidate: TNyxDocument;
  var AWorkspace: TNyxSourceWorkspace; const ACheckpoint: TNyxSourceCheckpoint;
  ARememberDraft: Boolean; AAllowProjectFiles: Boolean);
begin
  PublishHistoryPair(ACandidate, AWorkspace, ACheckpoint,
    NyxStudioCheckpoint(ACheckpoint, FSourceDraft, FSourceDraftBase,
      ARememberDraft and FSourceDraftPending, FFileDrafts), AAllowProjectFiles);
end;

procedure TNyxStudioSession.PublishHistoryPair(var ACandidate: TNyxDocument;
  var AWorkspace: TNyxSourceWorkspace; const ACheckpoint: TNyxSourceCheckpoint;
  const AHistory: TNyxStudioCheckpoint; AAllowProjectFiles: Boolean);
var
  LPrevious: TNyxDocument;
  LPreviousSource: TNyxSourceWorkspace;
begin
  { Source-only/visual commands cannot accidentally drop or replace helper
    meaning. Complete project admission and a sealed helper/inventory command may
    change that inventory. Check
    before history allocation/publication, preserving both staged owners. }

  if not AAllowProjectFiles and
    (ACheckpoint.Files.Encoded <> AWorkspace.Capture.Files.Encoded) then
  begin
    raise ENyxProjectConflict.Create('This command did not retain the exact supporting Pascal files');
  end;

  if (AHistory.Source <> ACheckpoint.Source) or (AHistory.Design <> ACheckpoint.Design) or
    (AHistory.Frame.Files.Encoded <> ACheckpoint.Files.Encoded) then
  begin
    raise ENyxProjectConflict.Create('Source Undo baseline does not match its accepted checkpoint');
  end;
  FUndo.Add(AHistory);
  { No fallible reconciliation follows publication. Source edits already have
    their exact admitted companion; regenerating that unused intermediate would
    reject valid authored arrangements and perform the same work twice. }
  LPrevious := FDocument;
  LPreviousSource := FSourceWorkspace;
  FDocument := ACandidate;
  ACandidate := nil;
  FSourceWorkspace := AWorkspace;
  AWorkspace := nil;
  LPrevious.Free;
  LPreviousSource.Free;
  FRedo.Clear;
  TrimUndoHistory;
  CancelPlacement;
end;

procedure TNyxStudioSession.SetExtension(AOwner: TNyxStudioExtensionOwner;
  const AKey: TNyxExtensionRef; const AValue: TNyxDataValue);
var
  LCandidate: TNyxDocument;
  LStore: TNyxExtensions;
  LNode: TNyxNode;
begin
  LCandidate := FDocument.Clone;
  try
    { A foreign/cast owner must retain the detached candidate's refusal path. }
    case Ord(AOwner) of
      Ord(seoDocument):
        begin
          LStore := LCandidate.Extensions;
        end;
      Ord(seoSelection):
        begin
          LNode := LCandidate.Find(FSelectedID);

          if LNode = nil then
          begin
            raise ENyxModel.Create('Select an authored extension owner');
          end;
          LStore := LNode.Extensions;
        end;
      else
        begin
          raise ENyxModel.Create('Unknown extension command owner');
        end;
    end;
    LStore.SetValue(AKey, AValue);
    PublishCandidate(LCandidate);
  finally
    LCandidate.Free;
  end;
end;

function TNyxStudioSession.CallbackCandidate: TNyxDocument;
var
  LProjection: TNyxNode;
  LNode: TNyxNode;
begin

  if Selected = nil then
  begin
    raise ENyxModel.Create('Select a component before editing its events');
  end;
  Result := FDocument.Clone;
  try
    LNode := Result.Find(FSelectedID);
    LProjection := SelectedProjection;
    try
      { Materialize inherited registrations only on this edited instance/part.
        The definition and sibling instances keep their independent metadata. }

      if not LNode.Extensions.Has(NyxExtension(NyxCallbacksKey)) and
        (LProjection <> nil) and
        LProjection.Extensions.Has(NyxExtension(NyxCallbacksKey)) then
      begin
        NyxCallbacks(LNode).Metadata(
          LProjection.Extensions.Value(NyxExtension(NyxCallbacksKey)));
      end;
    finally
      LProjection.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TNyxStudioSession.DoAddCallback(ATrigger: TNyxTrigger;
  const AName: TNyxEventRef; out ALine: Integer): TNyxHandlerRef;
var
  LCandidate: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSource: TNyxText;
  LStem: TNyxText;
  LName: TNyxText;
  LIndex: Integer;
  LSerial: Integer;
  LUpper: Boolean;
  LChar: Char;
  LMetadata: TNyxEventSchemas;
  LSupported: Boolean;
begin

  if DraftSource <> Source then
  begin
    raise ENyxSource.CreateAt('Apply or restore the Pascal draft before adding a handler',
      DraftSource, 1);
  end;
  LMetadata := NyxEventsMetadata(Selected, Document);
  LSupported := False;
  for LIndex := 0 to High(LMetadata) do
  begin
    LSupported := LSupported or ((LMetadata[LIndex].Trigger = ATrigger) and
      (LMetadata[LIndex].Name.Name = AName.Name));
  end;

  if not LSupported then
  begin
    raise ENyxModel.Create('The selected component does not publish this callback');
  end;
  { Readable bounded Pascal class names describe authored purpose and event.
    Whole-source collision checking protects the user's existing helper types. }
  LStem := '';
  LUpper := True;
  LName := Selected.ID;

  if (LowerCase(Copy(LName, 1, Length(Selected.Kind))) <> Selected.Kind) and
    (LowerCase(Copy(LName, Length(LName) - Length(Selected.Kind) + 1, MaxInt)) <>
      Selected.Kind) then
  begin
    LName := LName + '-' + Selected.Kind;
  end;

  if ATrigger = ntNamed then
  begin
    LName := LName + '-' + AName.Name;
  end
  else
  begin
    LName := LName + '-' + NyxTriggerName(ATrigger);
  end;
  for LIndex := 1 to Length(LName) do
  begin
    LChar := LName[LIndex];

    if LChar in ['A'..'Z', 'a'..'z', '0'..'9'] then
    begin

      if LUpper then
      begin
        LChar := UpCase(LChar);
      end;
      LStem := LStem + LChar;
      LUpper := False;

      if Length(LStem) >= 90 then
      begin
        Break;
      end;
    end
    else
    begin
      LUpper := True;
    end;
  end;
  LStem := 'T' + LStem;
  LName := LStem;
  LSerial := 1;
  LSource := Source;
  while (Pos(LowerCase(LName), LowerCase(LSource)) > 0) or
    (Pos(LowerCase(LName), LowerCase(FCallbackReservedSource)) > 0) do
  begin
    Inc(LSerial);
    LName := LStem + IntToStr(LSerial);
  end;
  Result := NyxHandler(LName);
  LCandidate := CallbackCandidate;
  LWorkspace := TNyxSourceWorkspace.Create;
  try

    if ATrigger = ntNamed then
    begin
      NyxCallbacks(LCandidate.Find(FSelectedID)).OnNamed(AName)
        .Add(Result, NyxCallbackID(LName));
    end
    else
    begin
      NyxCallbacks(LCandidate.Find(FSelectedID)).On(ATrigger)
        .Add(Result, NyxCallbackID(LName));
    end;
    ValidateNyxDocumentProperties(LCandidate);
    LWorkspace.Restore(FSourceWorkspace.Capture);
    LSource := LWorkspace.Render(LCandidate);

    if ATrigger = ntNamed then
    begin
      LSource := AddNyxHandlerStub(LSource, Result, AName, ALine);
    end
    else
    begin
      LSource := AddNyxHandlerStub(LSource, Result, ATrigger, ALine);
    end;
    LWorkspace.Accept(LCandidate, LSource);
    PublishPair(LCandidate, LWorkspace);
    DiscardSourceDraft;
  finally
    LWorkspace.Free;
    LCandidate.Free;
  end;
end;

function TNyxStudioSession.AddCallback(ATrigger: TNyxTrigger;
  out ALine: Integer): TNyxHandlerRef;
begin

  if not NyxIsRuntimeTrigger(ATrigger) then
  begin
    raise ENyxModel.Create('Use an exact event reference for a named callback');
  end;
  Result := DoAddCallback(ATrigger, Default(TNyxEventRef), ALine);
end;

function TNyxStudioSession.AddCallback(const AName: TNyxEventRef;
  out ALine: Integer): TNyxHandlerRef;
begin
  Result := DoAddCallback(ntNamed, AName, ALine);
end;

procedure TNyxStudioSession.DoSetCallbackPolicy(ATrigger: TNyxTrigger;
  const AName: TNyxEventRef; APolicy: TNyxExecutionPolicy);
var
  LCandidate: TNyxDocument;
begin
  LCandidate := CallbackCandidate;
  try

    if ATrigger = ntNamed then
    begin
      NyxCallbacks(LCandidate.Find(FSelectedID)).OnNamed(AName).Policy(APolicy);
    end
    else
    begin
      NyxCallbacks(LCandidate.Find(FSelectedID)).On(ATrigger).Policy(APolicy);
    end;
    PublishCandidate(LCandidate);
  finally
    LCandidate.Free;
  end;
end;

procedure TNyxStudioSession.SetCallbackPolicy(ATrigger: TNyxTrigger;
  APolicy: TNyxExecutionPolicy);
begin
  DoSetCallbackPolicy(ATrigger, Default(TNyxEventRef), APolicy);
end;

procedure TNyxStudioSession.SetCallbackPolicy(const AName: TNyxEventRef;
  APolicy: TNyxExecutionPolicy);
begin
  DoSetCallbackPolicy(ntNamed, AName, APolicy);
end;

procedure TNyxStudioSession.DoRemoveCallback(ATrigger: TNyxTrigger;
  const AName: TNyxEventRef; const AID: TNyxCallbackRef);
var
  LCandidate: TNyxDocument;
begin
  LCandidate := CallbackCandidate;
  try

    if ATrigger = ntNamed then
    begin
      NyxCallbacks(LCandidate.Find(FSelectedID)).OnNamed(AName).Remove(AID);
    end
    else
    begin
      NyxCallbacks(LCandidate.Find(FSelectedID)).On(ATrigger).Remove(AID);
    end;
    PublishCandidate(LCandidate);
  finally
    LCandidate.Free;
  end;
end;

procedure TNyxStudioSession.RemoveCallback(ATrigger: TNyxTrigger;
  const AID: TNyxCallbackRef);
begin
  DoRemoveCallback(ATrigger, Default(TNyxEventRef), AID);
end;

procedure TNyxStudioSession.RemoveCallback(const AName: TNyxEventRef;
  const AID: TNyxCallbackRef);
begin
  DoRemoveCallback(ntNamed, AName, AID);
end;

procedure TNyxStudioSession.MoveCallback(ATrigger: TNyxTrigger;
  const AID: TNyxCallbackRef; AIndex: Integer);
var
  LCandidate: TNyxDocument;
begin
  LCandidate := CallbackCandidate;
  try
    NyxCallbacks(LCandidate.Find(FSelectedID)).On(ATrigger).Move(AID, AIndex);
    PublishCandidate(LCandidate);
  finally
    LCandidate.Free;
  end;
end;

procedure TNyxStudioSession.MoveCallback(const AName: TNyxEventRef;
  const AID: TNyxCallbackRef; AIndex: Integer);
var
  LCandidate: TNyxDocument;
begin
  LCandidate := CallbackCandidate;
  try
    NyxCallbacks(LCandidate.Find(FSelectedID)).OnNamed(AName).Move(AID, AIndex);
    PublishCandidate(LCandidate);
  finally
    LCandidate.Free;
  end;
end;

function TNyxStudioSession.CallbackLine(const AHandler: TNyxHandlerRef): Integer;
begin
  Result := NyxHandlerSourceLine(DraftSource, AHandler);
end;

procedure TNyxStudioSession.RemoveExtension(AOwner: TNyxStudioExtensionOwner;
  const AKey: TNyxExtensionRef);
var
  LCandidate: TNyxDocument;
  LStore: TNyxExtensions;
  LNode: TNyxNode;
begin
  LCandidate := FDocument.Clone;
  try
    { Validate the same explicit owner boundary used when setting extensions. }
    case Ord(AOwner) of
      Ord(seoDocument):
        begin
          LStore := LCandidate.Extensions;
        end;
      Ord(seoSelection):
        begin
          LNode := LCandidate.Find(FSelectedID);

          if LNode = nil then
          begin
            raise ENyxModel.Create('Select an authored extension owner');
          end;
          LStore := LNode.Extensions;
        end;
      else
        begin
          raise ENyxModel.Create('Unknown extension command owner');
        end;
    end;
    LStore.Remove(AKey);
    PublishCandidate(LCandidate);
  finally
    LCandidate.Free;
  end;
end;

procedure TNyxStudioSession.SetStateValues(const AValues: array of TNyxStateAssignment);
var
  LCandidate: TNyxDocument;
begin
  LCandidate := FDocument.Clone;
  try
    LCandidate.State.Apply(AValues, FDocument.State.Revision);

    if LCandidate.State.Revision <> FDocument.State.Revision then
    begin
      PublishCandidate(LCandidate);
    end;
  finally
    LCandidate.Free;
  end;
end;

procedure TNyxStudioSession.CreateState(const AName: TNyxText;
  const AValue: TNyxStateValue);
begin

  if FDocument.State.Has(AName) then
  begin
    raise ENyxState.Create('State key already exists: ' + AName);
  end;
  SetStateValues([NyxStateAssign(AName, AValue)]);
end;

procedure TNyxStudioSession.RemoveState(const AName: TNyxText);
begin
  SetStateValues([NyxStateRemove(AName)]);
end;

procedure TNyxStudioSession.RenameState(const AOldName, ANewName: TNyxText);
var
  LCandidate: TNyxDocument;
  LIndex: Integer;

  procedure RenameBindings(ANode: TNyxNode);
  var
    LBindingIndex: Integer;
    LSpec: TNyxBindingSpec;
  begin
    for LBindingIndex := 0 to ANode.BindingCount - 1 do
    begin
      LSpec := ANode.Bindings[LBindingIndex];

      if not LSpec.Cleared and (LSpec.Source = bsExpression) then
      begin
        case FDocument.State.Value(AOldName).Kind of
          nskText:
            begin
              ANode.SetBinding(TNyxBindingSpec.Computed(LSpec.Target,
                RenameNyxExpressionState(LSpec.Expression,
                  NyxTextState(AOldName), NyxTextState(ANewName))));
            end;
          nskBoolean:
            begin
              ANode.SetBinding(TNyxBindingSpec.Computed(LSpec.Target,
                RenameNyxExpressionState(LSpec.Expression,
                  NyxBooleanState(AOldName), NyxBooleanState(ANewName))));
            end;
          nskInteger:
            begin
              ANode.SetBinding(TNyxBindingSpec.Computed(LSpec.Target,
                RenameNyxExpressionState(LSpec.Expression,
                  NyxIntegerState(AOldName), NyxIntegerState(ANewName))));
            end;
          nskNumber:
            begin
              ANode.SetBinding(TNyxBindingSpec.Computed(LSpec.Target,
                RenameNyxExpressionState(LSpec.Expression,
                  NyxNumberState(AOldName), NyxNumberState(ANewName))));
            end;
        end;
      end
      else if not LSpec.Cleared and (LSpec.Source = bsState) and
        (LSpec.StateName = AOldName) then
      begin
        ANode.SetBinding(TNyxBindingSpec.Bound(LSpec.Target, ANewName,
          LSpec.ValueKind, LSpec.Direction));
      end;
    end;
    for LBindingIndex := 0 to ANode.Count - 1 do
    begin
      RenameBindings(ANode.Children[LBindingIndex]);
    end;
  end;

begin
  FDocument.State.Value(AOldName);

  if AOldName = ANewName then
  begin
    Exit;
  end;

  if FDocument.State.Has(ANewName) then
  begin
    raise ENyxState.Create('State key already exists: ' + ANewName);
  end;
  LCandidate := FDocument.Clone;
  try
    { Use the public typed, order-preserving state operation. The surrounding
      document candidate also rewrites bound references and admits the full graph
      before one source/design publication; the live document stays untouched. }
    case FDocument.State.Value(AOldName).Kind of
      nskText:
        begin
          LCandidate.State.Rename(NyxTextState(AOldName), NyxTextState(ANewName));
        end;
      nskBoolean:
        begin
          LCandidate.State.Rename(NyxBooleanState(AOldName), NyxBooleanState(ANewName));
        end;
      nskInteger:
        begin
          LCandidate.State.Rename(NyxIntegerState(AOldName), NyxIntegerState(ANewName));
        end;
      nskNumber:
        begin
          LCandidate.State.Rename(NyxNumberState(AOldName), NyxNumberState(ANewName));
        end;
    end;
    for LIndex := 0 to LCandidate.Count - 1 do
    begin
      RenameBindings(LCandidate.Pages[LIndex]);
    end;
    for LIndex := 0 to LCandidate.ComponentCount - 1 do
    begin
      RenameBindings(LCandidate.Components[LIndex]);
    end;
    PublishCandidate(LCandidate, [NyxSourceStateRename(AOldName, ANewName)]);
  finally
    LCandidate.Free;
  end;
end;

procedure TNyxStudioSession.SetBinding(const ASpec: TNyxBindingSpec);
var
  LCandidate: TNyxDocument;
  LNode: TNyxNode;
begin
  LCandidate := FDocument.Clone;
  try
    LNode := LCandidate.Find(FSelectedID);

    if LNode = nil then
    begin
      raise ENyxModel.Create('Select a control to bind');
    end;
    LNode.SetBinding(ASpec);
    PublishCandidate(LCandidate);
  finally
    LCandidate.Free;
  end;
end;

procedure TNyxStudioSession.InheritBinding(ATarget: TNyxBindingProperty);
var
  LCandidate: TNyxDocument;
  LNode: TNyxNode;
begin
  LCandidate := FDocument.Clone;
  try
    LNode := LCandidate.Find(FSelectedID);

    if LNode = nil then
    begin
      raise ENyxModel.Create('Select a control to restore its inherited binding');
    end;
    LNode.Binds.Inherit(ATarget).Done;
    PublishCandidate(LCandidate);
  finally
    LCandidate.Free;
  end;
end;

procedure TNyxStudioSession.DefineCollection(const AKey: TNyxCollectionRef;
  const ASchema: TNyxCollectionSchema; const AItems: array of TNyxCollectionItem);
var
  LCandidate: TNyxDocument;
begin

  if FDocument.ResourceCollections.HasSource(AKey) then
  begin
    raise ENyxCollection.Create('Edit this collection through its resource row recipe');
  end;
  LCandidate := FDocument.Clone;
  try
    LCandidate.Collections.Define(AKey, ASchema, AItems);
    PublishCandidate(LCandidate);
  finally
    LCandidate.Free;
  end;
end;

procedure TNyxStudioSession.RemoveCollection(const AKey: TNyxCollectionRef);
var
  LCandidate: TNyxDocument;
begin
  LCandidate := FDocument.Clone;
  try
    LCandidate.Collections.Remove(AKey);
    PublishCandidate(LCandidate);
  finally
    LCandidate.Free;
  end;
end;

procedure TNyxStudioSession.SetCollectionView(const ASpec: TNyxCollectionViewSpec);
var
  LCandidate: TNyxDocument;
  LNode: TNyxNode;
begin
  LCandidate := FDocument.Clone;
  try
    LNode := LCandidate.Find(FSelectedID);

    if LNode = nil then
    begin
      raise ENyxModel.Create('Select a control for its collection binding');
    end;
    LNode.SetCollectionView(ASpec);
    PublishCandidate(LCandidate);
  finally
    LCandidate.Free;
  end;
end;

procedure TNyxStudioSession.InheritCollectionView;
var
  LCandidate: TNyxDocument;
  LNode: TNyxNode;
begin
  LCandidate := FDocument.Clone;
  try
    LNode := LCandidate.Find(FSelectedID);

    if LNode = nil then
    begin
      raise ENyxModel.Create('Select a control to restore its inherited collection binding');
    end;
    LNode.RemoveCollectionView;
    PublishCandidate(LCandidate);
  finally
    LCandidate.Free;
  end;
end;

function TNyxStudioSession.SelectedProjection: TNyxNode;
var
  LSelected: TNyxNode;
  LRuntime: TNyxNode;
begin
  Result := nil;
  LSelected := Selected;

  if LSelected = nil then
  begin
    Exit;
  end;

  if LSelected.Kind = 'slot-override' then
  begin

    if LSelected.Prop('mode') = 'remove' then
    begin
      Exit;
    end;
    LRuntime := RealizeNyxView(FDocument, LSelected.Parent);
    try
      ApplyNyxBindings(LRuntime, FDocument.State);
      Result := LRuntime.Part(LSelected.Prop('path')).Clone;
    finally
      LRuntime.Free;
    end;
  end
  else
  begin
    LRuntime := RealizeNyxView(FDocument, LSelected);
    try
      ApplyNyxBindings(LRuntime, FDocument.State);
      Result := LRuntime;
      LRuntime := nil;
    finally
      LRuntime.Free;
    end;
  end;
end;

function TNyxStudioSession.InsertionParent: TNyxNode;
var
  LParent: TNyxNode;
  LIndex: Integer;
  LRuntime: TNyxNode;
  LInfo: TNyxPrimitiveInfo;
begin
  LParent := Selected;

  if LParent = nil then
  begin
    raise ENyxModel.Create('Select a container first');
  end;
  LIndex := FCatalog.IndexOf(LParent.Kind);

  if (LParent.Kind <> 'slot-override') and
    ((LIndex < 0) or not FCatalog[LIndex].Container) then
  begin
    LParent := LParent.Parent;
  end;

  if LParent = nil then
  begin
    raise ENyxModel.Create('Selected view has no editable container');
  end;

  if LParent.Kind = 'slot-override' then
  begin

    if (LParent.Prop('mode') <> 'properties') and
      (LParent.Prop('mode') <> 'append') and (LParent.Prop('mode') <> 'prepend') then
    begin
      raise ENyxModel.Create('Select an appended part to add content');
    end;
    LRuntime := RealizeNyxView(FDocument, LParent.Parent);
    try

      if not FindNyxPrimitive(LRuntime.Part(LParent.Prop('path')).ProjectionKind, LInfo) or
        not LInfo.Container then
      begin
        raise ENyxModel.Create('This named part is a leaf; select a layout part to add content');
      end;
    finally
      LRuntime.Free;
    end;
  end;
  Result := LParent;
end;

procedure TNyxStudioSession.InsertNode(ANode: TNyxNode);
var
  LParent: TNyxNode;
  LOwned: Boolean;
begin
  LOwned := False;
  try
    LParent := InsertionParent;
    Checkpoint;
    try
      LParent.Add(ANode);
      LOwned := True;

      if (LParent.Kind = 'slot-override') and (LParent.Prop('mode') = 'properties') then
      begin
        LParent.SetProp('mode', 'append');
      end;
    except
      Rollback;
      raise;
    end;
    FSelectedID := ANode.ID;
    { Commit performs its own rollback if effective paths, values or references
      fail. The ownership flag prevents touching the freed candidate afterward. }
    Commit;
  finally

    if not LOwned then
    begin
      ANode.Free;
    end;
  end;
end;

procedure TNyxStudioSession.AddKind(const AKind: TNyxText);
begin
  InsertNode(FCatalog.NewNode(AKind, NewID(AKind)));
end;

procedure TNyxStudioSession.AddControl(const AControl: INyxControl);
var
  LNode: TNyxNode;
begin

  if AControl = nil then
  begin
    raise ENyxModel.Create('A control is required');
  end;
  LNode := AControl.Node;
  ValidateNyxPropertyTree(LNode);
  InsertNode(LNode.Clone);
end;

procedure TNyxStudioSession.CustomizePart(const APath: TNyxText);
var
  LReference: TNyxNode;
  LRule: TNyxNode;
  LCount: Integer;
  LMode: TNyxText;
  LIndex: Integer;
begin
  LReference := Selected;

  if (LReference = nil) or (LReference.ProjectionKind <> 'component') then
  begin
    raise ENyxModel.Create('Select a reusable instance to customize its parts');
  end;
  LCount := LReference.Count;
  LMode := 'properties';
  for LIndex := 0 to LReference.Count - 1 do
  begin

    if LReference.Children[LIndex].Prop('path') = APath then
    begin
      LMode := LReference.Children[LIndex].Prop('mode');
    end;
  end;
  Checkpoint;
  try
    LRule := LReference.OverridePart(APath, LMode);

    if LReference.Count > LCount then
    begin
      LRule.Named(NewID('part'));
    end;
  except
    Rollback;
    raise;
  end;
  Commit;
  FSelectedID := LRule.ID;
end;

procedure TNyxStudioSession.DeleteSelected;
var
  LNode: TNyxNode;
  LParent: TNyxNode;
begin
  LNode := Selected;

  if LNode = nil then
  begin
    Exit;
  end;
  LParent := LNode.Parent;

  if LParent = nil then
  begin
    raise ENyxModel.Create('Root views are retained; select a child to delete');
  end;
  Checkpoint;
  FSelectedID := LParent.ID;
  LParent.Remove(LNode);

  if (LParent.Kind = 'slot-override') and (LParent.Count = 0) then
  begin
    LParent.SetProp('mode', 'properties');
  end;
  Commit;
end;

procedure TNyxStudioSession.Reidentify(ANode: TNyxNode);
var
  LIndex: Integer;
begin
  ANode.Named(NewID(ANode.Kind));
  for LIndex := 0 to ANode.Count - 1 do
  begin
    Reidentify(ANode.Children[LIndex]);
  end;
end;

procedure TNyxStudioSession.DuplicateSelected;
var
  LNode: TNyxNode;
  LCopy: TNyxNode;
begin
  LNode := Selected;

  if (LNode = nil) or (LNode.Parent = nil) then
  begin
    raise ENyxModel.Create('Select a child to duplicate');
  end;
  LCopy := LNode.Clone;
  try
    Reidentify(LCopy);
    Checkpoint;
    LNode.Parent.Add(LCopy);
  except
    LCopy.Free;
    Rollback;
    raise;
  end;
  FSelectedID := LCopy.ID;
  Commit;
end;

procedure TNyxStudioSession.MoveSelected(ADirection: Integer);
var
  LNode: TNyxNode;
  LParent: TNyxNode;
  LIndex: Integer;
  LDestination: Integer;
  LRoot: TNyxRootRef;
  LCount: Integer;
begin
  LNode := Selected;

  if LNode = nil then
  begin
    Exit;
  end;

  if LNode.Parent = nil then
  begin
    { Root identity belongs to its document partition, not its visual kind:
      reusable definitions can be cards, columns or any other control. The
      ordinary detached patch preserves exact source/history and all anchors. }
    LRoot := NyxPageRoot(LNode.ID);
    LCount := FDocument.Count;
    LIndex := 0;

    if FDocument.FindRoot(LRoot) = LNode then
    begin
      while FDocument.Pages[LIndex] <> LNode do
      begin
        Inc(LIndex);
      end;
    end
    else
    begin
      LRoot := NyxReusableRoot(LNode.ID);
      LCount := FDocument.ComponentCount;

      if FDocument.FindRoot(LRoot) <> LNode then
      begin
        raise ENyxModel.Create('The selected root no longer belongs to this document');
      end;
      while FDocument.Components[LIndex] <> LNode do
      begin
        Inc(LIndex);
      end;
    end;
    LDestination := LIndex + ADirection;

    if (LDestination >= 0) and (LDestination < LCount) then
    begin
      ApplyPatch(NyxRootOrderPatch([NyxMoveRoot(LRoot, LDestination)]));
    end;
    Exit;
  end;
  LParent := LNode.Parent;
  LIndex := 0;
  while LParent.Children[LIndex] <> LNode do
  begin
    Inc(LIndex);
  end;
  LDestination := LIndex + ADirection;

  if (LDestination < 0) or (LDestination >= LParent.Count) then
  begin
    Exit;
  end;
  Checkpoint;
  LNode := LParent.Extract(LIndex);
  LParent.Insert(LDestination, LNode);
  Commit;
end;

procedure TNyxStudioSession.AddPage;
var
  LNode: TNyxNode;
begin
  LNode := FCatalog.NewNode('page', NewID('page'));
  try
    Checkpoint;
    FDocument.AddPage(LNode);
  except
    LNode.Free;
    raise;
  end;
  Activate(LNode.ID);
  Commit;
end;

procedure TNyxStudioSession.CreateComponent;
var
  LDefinition: TNyxComponentRef;
  LIdentities: array of TNyxIdentityAssignment;

  procedure IdentifyDescendants(ANode: TNyxNode);
  var
    LIndex, LPosition: Integer;
  begin
    for LIndex := 0 to ANode.Count - 1 do
    begin
      LPosition := Length(LIdentities);
      SetLength(LIdentities, LPosition + 1);
      LIdentities[LPosition] := NyxIdentity(NyxControl(ANode.Children[LIndex].ID),
        NyxControl(NewID(ANode.Children[LIndex].Kind)));
      IdentifyDescendants(ANode.Children[LIndex]);
    end;
  end;
begin

  if Selected = nil then
  begin
    raise ENyxModel.Create('Select a subtree to make reusable');
  end;
  { The ordinary inspector and agents share independent derivation and paired
    publication. This convenience command assigns IDs; the public/MCP contract
    also accepts an author's crafted exact identities. }
  LDefinition := NyxComponent(NewID(Selected.Kind));
  IdentifyDescendants(Selected);
  ApplyPatch(NyxReusablePatch([NyxDeriveComponent(NyxControl(Selected.ID),
    LDefinition, LIdentities)]));
  Activate(LDefinition.Name);
end;

procedure TNyxStudioSession.AddComponentInstance(const ADefinitionID: TNyxText);
begin

  if FDocument.FindComponent(ADefinitionID) = nil then
  begin
    raise ENyxModel.Create('Reusable definition is missing');
  end;
  InsertNode(TNyxNode.Create('component', NewID('component'))
    .SetProp('component', ADefinitionID));
end;

procedure TNyxStudioSession.Undo;
begin

  if FUndo.Count = 0 then
  begin
    Exit;
  end;
  RestorePair(FUndo.LastState, FRedo);
  FUndo.Delete(FUndo.Count - 1);
  CancelPlacement;
end;

procedure TNyxStudioSession.Redo;
begin

  if FRedo.Count = 0 then
  begin
    Exit;
  end;
  RestorePair(FRedo.LastState, FUndo);
  FRedo.Delete(FRedo.Count - 1);
  CancelPlacement;
end;

procedure TNyxStudioSession.Load(const ASource: TNyxText);
begin

  if FSourceGeneration = High(Integer) then
  begin
    raise ENyxModel.Create('Source session generation is exhausted');
  end;
  { Admission precedes releasing the baseline. A successful import deliberately
    starts a new history; an invalid import preserves both document and history. }
  Restore(ASource);
  Inc(FSourceGeneration);
  CancelPlacement;
  FSourceWorkspace.Reset;
  DiscardSourceDraft;
  FFileDrafts := Default(TNyxPascalFileDrafts);
  FFileInputCache := nil;
  FFileInputKey := '';
  FActiveSourceFile := TNyxPascalSourceRef.Main;
  FUndo.Clear;
  FRedo.Clear;
end;

function TNyxStudioSession.Save: TNyxText;
{$ifdef NYX_SOURCE_PROFILE}
var
  LStarted: Double;
{$endif}
begin
  {$ifdef NYX_SOURCE_PROFILE}LStarted := SourceProfileStart;{$endif}
  Result := TNyxCodec.Encode(FDocument);
  {$ifdef NYX_SOURCE_PROFILE}SourceProfileFinish(spSave, LStarted);{$endif}
end;

function TNyxStudioSession.Source: TNyxText;
begin
  Result := FSourceWorkspace.Render(FDocument);
end;

function TNyxStudioSession.ProjectSnapshot: TNyxProjectPair;
begin
  Result := NyxProjectPair(Save, Source);
  Result.Files := FSourceWorkspace.Capture.Files;
  Result.FileDrafts := FFileDrafts;
  Result.Pending := FSourceDraftPending;

  if Result.Pending then
  begin
    Result.Draft := FSourceDraft;
    Result.DraftBase := FSourceDraftBase;
  end;
end;

procedure TNyxStudioSession.LoadProject(const APair: TNyxProjectPair;
  AResolution: TNyxProjectResolution);
var
  LDocument: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LResolved: TNyxProjectPair;
begin

  if FSourceGeneration = High(Integer) then
  begin
    raise ENyxModel.Create('Source session generation is exhausted');
  end;
  AdmitNyxProject(APair, AResolution, LDocument, LWorkspace, LResolved);
  LoadAdmittedProject(LDocument, LWorkspace, LResolved);
end;

procedure TNyxStudioSession.PublishOpenedProject(var ADocument: TNyxDocument;
  var AWorkspace: TNyxSourceWorkspace; const AResolved: TNyxProjectPair);
begin

  if FSourceGeneration = High(Integer) then
  begin
    raise ENyxModel.Create('Source session generation is exhausted');
  end;
  AdoptAdmittedProject(ADocument, AWorkspace, AResolved, spaCommand);
  Inc(FSourceGeneration);
  CancelPlacement;
  FActiveSourceFile := TNyxPascalSourceRef.Main;
end;

procedure TNyxStudioSession.OpenProject(const APair: TNyxProjectPair;
  AResolution: TNyxProjectResolution);
var
  LDocument: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LResolved: TNyxProjectPair;
begin
  LDocument := nil;
  LWorkspace := nil;
  try
    AdmitNyxProject(APair, AResolution, LDocument, LWorkspace, LResolved);
    PublishOpenedProject(LDocument, LWorkspace, LResolved);
  finally
    LWorkspace.Free;
    LDocument.Free;
  end;
end;

procedure TNyxStudioSession.LoadCapturedProject(const APair: TNyxProjectPair;
  const ACheckpoint: TNyxSourceCheckpoint);
var
  LDocument: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LResolved: TNyxProjectPair;
begin

  if FSourceGeneration = High(Integer) then
  begin
    raise ENyxModel.Create('Source session generation is exhausted');
  end;
  AdmitNyxCapturedProject(APair, ACheckpoint, LDocument, LWorkspace, LResolved);
  LoadAdmittedProject(LDocument, LWorkspace, LResolved);
end;

function TNyxStudioProjectRequest.GetSource: TNyxText;
begin
  Result := FPair.Source;
end;

function TNyxStudioProjectRequest.GetFiles: TNyxPascalProjectFiles;
begin
  Result := FPair.Files;
end;

function TNyxStudioSession.PrepareProjectRequest(const APair: TNyxProjectPair;
  AResolution: TNyxProjectResolution; ASchemaRevision: Integer;
  AHistory: TNyxProjectOpeningHistory): TNyxStudioProjectRequest;
var
  LDocument: TNyxDocument;
begin

  if not (AResolution in [nprRequireMatch, nprUsePascal]) then
  begin
    raise ENyxModel.Create('Compiler-backed opening requires a Pascal resolution');
  end;

  if not (AHistory in [pohNewHistory, pohPairedCommand]) then
  begin
    raise ENyxModel.Create('Unknown project opening history policy');
  end;
  Result := Default(TNyxStudioProjectRequest);
  Result.FPair := DecodeNyxProject(EncodeNyxProject(APair));
  { Refuse malformed saved design before invoking any application constructor.
    Canonicalization admits ordinary codec formatting without rewriting Pascal
    or the independent unfinished buffer. Current creators are checked again by
    isolated preparation and the atomic completion generation guard. }
  LDocument := TNyxCodec.Decode(Result.FPair.Design);
  try
    ValidateNyxDocumentProperties(LDocument);
    Result.FPair.Design := TNyxCodec.Encode(LDocument);
  finally
    LDocument.Free;
  end;

  if Result.FPair.Files.IsEmpty then
  begin
    NyxCompanionUnitName(Result.FPair.Source);
  end
  else
  begin
    Result.FPair.Files.Open(Result.FPair.Source);
  end;
  Result.FOwner := FSourceIdentity;
  Result.FGeneration := FSourceGeneration;
  Result.FBaseline := EncodeNyxProject(ProjectSnapshot);
  Result.FResolution := AResolution;
  Result.FHistory := AHistory;
  Result.FSchemaRevision := ASchemaRevision;
end;

function TNyxStudioSession.ProjectRequestCurrent(
  const ARequest: TNyxStudioProjectRequest): Boolean;
begin
  Result := (ARequest.FOwner = FSourceIdentity) and
    (ARequest.FGeneration = FSourceGeneration) and
    (ARequest.FSchemaRevision = NyxSchemaRevision) and
    (ARequest.FBaseline = EncodeNyxProject(ProjectSnapshot));
end;

function TNyxStudioSession.CompleteProjectRequest(
  const ARequest: TNyxStudioProjectRequest;
  const APrepared: INyxPreparedSource): TNyxSourceCompletion;
var
  LPublication: TProjectLoadPublication;
  LAction: INyxSchemaAction;
begin
  Result := nscStale;

  if not ProjectRequestCurrent(ARequest) then
  begin
    Exit;
  end;

  if (APrepared = nil) or (APrepared.Source <> ARequest.Source) or
    (APrepared.SchemaRevision <> ARequest.FSchemaRevision) then
  begin
    raise ENyxProjectConflict.Create('Project completion differs from its captured saved source');
  end;

  if APrepared.Diagnostic.Defined then
  begin
    Exit(nscRejected);
  end;

  if (ARequest.FResolution = nprRequireMatch) and
    (ARequest.FPair.Design <> APrepared.Design) then
  begin
    raise ENyxProjectConflict.Create(
      'Compiled Pascal and saved design differ. Choose which version to open');
  end;

  if FSourceGeneration = High(Integer) then
  begin
    raise ENyxModel.Create('Source session generation is exhausted');
  end;
  LPublication := TProjectLoadPublication.Create;
  LAction := LPublication;
  LPublication.Session := Self;
  LPublication.Pair := ARequest.FPair;
  LPublication.History := ARequest.FHistory;
  LPublication.Pair.Design := APrepared.Design;
  APrepared.Take(LPublication.Document, LPublication.Workspace);

  if LPublication.Workspace.Capture.Files.Encoded <> ARequest.FPair.Files.Encoded then
  begin
    raise ENyxProjectConflict.Create('Project preparation has different supporting Pascal files');
  end;

  if CommitNyxSchemaRevision(ARequest.FSchemaRevision, LAction) then
  begin
    Result := nscApplied;
  end;
end;

procedure TNyxStudioSession.LoadAdmittedProject(ADocument: TNyxDocument;
  AWorkspace: TNyxSourceWorkspace; const AResolved: TNyxProjectPair);
begin
  { Publication contains no parsing, filesystem calls or callback execution.
    Both old owners remain alive until the entire replacement has been admitted. }
  FDocument.Free;
  FSourceWorkspace.Free;
  FDocument := ADocument;
  FSourceWorkspace := AWorkspace;
  Inc(FSourceGeneration);
  CancelPlacement;
  FActiveViewID := '';

  if FDocument.Count > 0 then
  begin
    FActiveViewID := FDocument.Pages[0].ID;
  end
  else if FDocument.ComponentCount > 0 then
  begin
    FActiveViewID := FDocument.Components[0].ID;
  end;
  FSelectedID := FActiveViewID;
  FNextID := 0;
  DiscardSourceDraft;
  FFileDrafts := AResolved.FileDrafts;
  FActiveSourceFile := TNyxPascalSourceRef.Main;

  if AResolved.Pending then
  begin
    FSourceDraft := AResolved.Draft;
    FSourceDraftBase := AResolved.DraftBase;
    FSourceDraftPending := True;
  end;
  FUndo.Clear;
  FRedo.Clear;
end;

function TNyxStudioSession.CanUndo: Boolean;
begin
  Result := FUndo.Count > 0;
end;

function TNyxStudioSession.CanRedo: Boolean;
begin
  Result := FRedo.Count > 0;
end;

procedure TNyxStudioSession.ApplyPatch(const APatch: INyxDesignPatch);
var
  LCandidate: TNyxDocument;
begin

  if (APatch = nil) or (DraftSource <> Source) then
  begin
    raise ENyxModel.Create('Resolve the pending Pascal draft before applying an agent transaction');
  end;
  LCandidate := APatch.Candidate(FDocument, FCatalog);
  try
    PublishCandidate(LCandidate);

    if FDocument.Find(FSelectedID) = nil then
    begin
      FSelectedID := FActiveViewID;
    end;
  finally
    LCandidate.Free;
  end;
end;

function TNyxStudioSession.GetPlacementSource: TNyxControlRef;
begin
  Result := Default(TNyxControlRef);

  if MatchesCommandContext(FPlacementContext) and (FPlacementSource.ID <> '') and
    (DraftSource = Source) and (FPlacementPair.Source = Source) and
    (FPlacementPair.Design = FSourceWorkspace.Capture.Design) then
  begin
    Result := FPlacementSource;
  end;
end;

procedure TNyxStudioSession.BeginPlacement;
var
  LNode: TNyxNode;
begin

  if DraftSource <> Source then
  begin
    raise ENyxModel.Create('Resolve the Pascal draft before moving a control');
  end;
  LNode := Selected;

  if (LNode = nil) or (LNode.Parent = nil) or (LNode.Kind = 'slot-override') then
  begin
    raise ENyxModel.Create('Select an authored control to move');
  end;
  FPlacementPair := ProjectSnapshot;
  FPlacementContext := CommandContext;
  FPlacementSource := NyxControl(LNode.ID);
end;

procedure TNyxStudioSession.CancelPlacement;
begin
  FPlacementSource := Default(TNyxControlRef);
  FPlacementContext := Default(TNyxStudioCommandContext);
  FPlacementPair := Default(TNyxProjectPair);
end;

procedure TNyxStudioSession.Place(const AChange: TNyxPlacementChange);
var
  LRoot: TNyxNode;
begin
  ApplyPatch(NyxPlacementPatch([AChange]));
  LRoot := FDocument.Find(AChange.Control.ID);
  while LRoot.Parent <> nil do
  begin
    LRoot := LRoot.Parent;
  end;
  Activate(LRoot.ID);
  Select(AChange.Control.ID);
end;

function TNyxStudioSession.CapturePlacement(const AChange: TNyxPlacementChange;
  const AMountContext: TNyxStudioCommandContext): TNyxStudioDesignEdit;
begin

  if not MatchesCommandContext(AMountContext) then
  begin
    raise ENyxModel.Create('Placement belongs to an earlier session or project load');
  end;
  { Validate construction without encoding the project or retaining a node. }
  AChange.ToData;
  Result := Default(TNyxStudioDesignEdit);
  Result.Action := sdaPlacement;
  Result.Selection := SelectedID;
  Result.View := ActiveViewID;
  Result.Placement := AChange;
  Result.FCanvasContext := AMountContext;
end;

procedure TNyxStudioSession.Resize(const AChange: TNyxResizeChange);
var
  LNode: TNyxNode;
  LContext: TNyxNode;
  LProjection: TNyxNode;
  LClearFlex: Boolean;
begin
  AChange.ToData;
  LNode := FDocument.Find(AChange.Control.ID);

  if (LNode = nil) or (LNode.Parent = nil) or (LNode.Kind = 'slot-override') then
  begin
    raise ENyxModel.Create('Resize requires an exact authored non-root control');
  end;
  { Realized context resolves an inherited slot's actual parent instead of
    guessing flow from its authored override descriptor or catalog kind. }
  LContext := RealizeNyxContext(FDocument, LNode, LProjection);
  try
    LClearFlex := NyxResizeReleasesWeight(LProjection.Parent, AChange.Axis, AChange.Platform);

    if (AChange.Platform = npfAny) and
      ((LClearFlex <> NyxResizeReleasesWeight(LProjection.Parent, AChange.Axis, npfBrowser)) or
      (LClearFlex <> NyxResizeReleasesWeight(LProjection.Parent, AChange.Axis, npfNativeLCL))) then
    begin
      raise ENyxModel.Create('One-axis resize requires shared parent flow or an explicit target scope');
    end;
  finally
    LContext.Free;
  end;
  ApplyPatch(ReadNyxDesignPatch(NyxArray([AChange.Operation(LClearFlex)])));
end;

function TNyxStudioSession.CaptureResize(const AChange: TNyxResizeChange;
  const AMountContext: TNyxStudioCommandContext): TNyxStudioDesignEdit;
begin

  if not MatchesCommandContext(AMountContext) then
  begin
    raise ENyxModel.Create('Resize belongs to an earlier session or project load');
  end;
  AChange.ToData;
  Result := Default(TNyxStudioDesignEdit);
  Result.Action := sdaResize;
  Result.Selection := AChange.Control.ID;
  Result.View := ActiveViewID;
  Result.Resize := AChange;
  Result.FCanvasContext := AMountContext;
end;

procedure TNyxStudioSession.Position(const AChange: TNyxPositionChange);
begin
  AChange.ToData;
  ValidateNyxPositionOwner(FDocument, AChange.Control);
  ApplyPatch(ReadNyxDesignPatch(NyxArray([AChange.Operation])));
end;

function TNyxStudioSession.CapturePosition(const AChange: TNyxPositionChange;
  const AMountContext: TNyxStudioCommandContext): TNyxStudioDesignEdit;
begin

  if not MatchesCommandContext(AMountContext) then
  begin
    raise ENyxModel.Create('Positioning belongs to an earlier session or project load');
  end;
  AChange.ToData;
  ValidateNyxPositionOwner(FDocument, AChange.Control);
  Result := Default(TNyxStudioDesignEdit);
  Result.Action := sdaPosition;
  Result.Selection := AChange.Control.ID;
  Result.View := ActiveViewID;
  Result.Position := AChange;
  Result.FCanvasContext := AMountContext;
end;

function TNyxStudioSession.CaptureNewPlacement(const AKind: TNyxKindRef;
  const ATarget: TNyxControlRef; APlacement: TNyxPlacement;
  const AMountContext: TNyxStudioCommandContext): TNyxStudioDesignEdit;
begin

  if not MatchesCommandContext(AMountContext) or
    (FCatalog.IndexOf(AKind.Name) < 0) or FSourceDraftPending then
  begin
    raise ENyxModel.Create('New placement requires the current editor, catalog kind and resolved source');
  end;
  Result := CapturePlacement(NyxPlaceNewControl(AKind, NyxControl(NewID(AKind.Name)),
    ATarget, APlacement), AMountContext);
end;

procedure TNyxStudioSession.RemoveRoots(const AReview: INyxRootRemoval);
var
  LPair: TNyxProjectPair;
begin

  if AReview = nil then
  begin
    raise ENyxModel.Create('Review the exact root group before removing it');
  end;
  LPair := AReview.Candidate(ProjectSnapshot);
  AdoptProject(LPair);
end;

procedure TNyxStudioSession.AdoptProject(const APair: TNyxProjectPair;
  AAdoption: TNyxStudioProjectAdoption);
var
  LDocument: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LResolved: TNyxProjectPair;
begin
  LDocument := nil;
  LWorkspace := nil;
  try
    { A current executed pair is already admitted. Draft-only synchronization
      may reuse its live opaque checkpoint, but different accepted file strings
      still take strict admission. Imported origin claims never select this path. }

    if (FSourceWorkspace.Origin = nsoExecuted) and (APair.Design = Save) and
      (APair.Source = Source) then
    begin
      AdmitNyxCapturedProject(APair, AcceptedSourceCheckpoint,
        LDocument, LWorkspace, LResolved);
    end
    else
    begin
      AdmitNyxProject(APair, nprRequireMatch, LDocument, LWorkspace, LResolved);
    end;
    AdoptAdmittedProject(LDocument, LWorkspace, LResolved, AAdoption);
  finally
    LDocument.Free;
    LWorkspace.Free;
  end;
end;

procedure TNyxStudioSession.AdoptProjectedProject(const APair: TNyxProjectPair;
  const AProjection: INyxSourceProjection; AAdoption: TNyxStudioProjectAdoption);
var
  LDocument: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LResolved: TNyxProjectPair;
begin
  LDocument := nil;
  LWorkspace := nil;
  try
    AdmitNyxProjectedProject(APair, AProjection, LDocument, LWorkspace, LResolved);
    AdoptAdmittedProject(LDocument, LWorkspace, LResolved, AAdoption);
  finally
    LDocument.Free;
    LWorkspace.Free;
  end;
end;

procedure TNyxStudioSession.AdoptCapturedProject(const APair: TNyxProjectPair;
  const ACheckpoint: TNyxSourceCheckpoint; AAdoption: TNyxStudioProjectAdoption);
var
  LDocument: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LResolved: TNyxProjectPair;
begin
  LDocument := nil;
  LWorkspace := nil;
  try
    AdmitNyxCapturedProject(APair, ACheckpoint, LDocument, LWorkspace, LResolved);
    AdoptAdmittedProject(LDocument, LWorkspace, LResolved, AAdoption);
  finally
    LDocument.Free;
    LWorkspace.Free;
  end;
end;

function TNyxStudioSession.AcceptedSourceCheckpoint: TNyxSourceCheckpoint;
begin
  Result := FSourceWorkspace.Capture(FDocument);
end;

procedure TNyxStudioSession.AdoptAdmittedProject(var ADocument: TNyxDocument;
  var AWorkspace: TNyxSourceWorkspace; const AResolved: TNyxProjectPair;
  AAdoption: TNyxStudioProjectAdoption);
var
  LCurrent: TNyxProjectPair;
  LActiveView: TNyxNode;
begin

  if not (AAdoption in [spaCommand, spaSynchronization]) then
  begin
    raise ENyxModel.Create('Unknown project adoption policy');
  end;
  LCurrent := ProjectSnapshot;

  if (AResolved.Design = LCurrent.Design) and (AResolved.Source = LCurrent.Source) and
    (AResolved.Files.Encoded = LCurrent.Files.Encoded) and
    (AResolved.FileDrafts.Encoded = LCurrent.FileDrafts.Encoded) and
    (AResolved.Pending = LCurrent.Pending) and (AResolved.Draft = LCurrent.Draft) and
    (AResolved.DraftBase = LCurrent.DraftBase) then
  begin
    Exit;
  end;
  { Saved draft/base changes are editor state too. A synchronized successful
    Apply consumes only this exact pending buffer; independent drafts remain in
    the previous checkpoint. Draft-only synchronization preserves existing Redo. }

  if (AAdoption = spaCommand) or (AResolved.Design <> LCurrent.Design) or
    (AResolved.Source <> LCurrent.Source) or
    (AResolved.Files.Encoded <> LCurrent.Files.Encoded) then
  begin
    PublishCapturedPair(ADocument, AWorkspace, FSourceWorkspace.Capture(FDocument),
      not ((AAdoption = spaSynchronization) and not AResolved.Pending and
        FSourceDraftPending and (FSourceDraft = AResolved.Source)), True);
  end;
  DiscardSourceDraft;

  if AResolved.Pending then
  begin
    FSourceDraft := AResolved.Draft;
    FSourceDraftBase := AResolved.DraftBase;
    FSourceDraftPending := True;
  end;
  FFileDrafts := AResolved.FileDrafts;

  LActiveView := FDocument.Find(FActiveViewID);

  if (LActiveView = nil) or (LActiveView.Parent <> nil) then
  begin
    FActiveViewID := '';

    if FDocument.Count > 0 then
    begin
      FActiveViewID := FDocument.Pages[0].ID;
    end
    else if FDocument.ComponentCount > 0 then
    begin
      FActiveViewID := FDocument.Components[0].ID;
    end;
    LActiveView := FDocument.Find(FActiveViewID);
  end;

  { Complete source can turn the former root into a descendant or move the
    selected identity to another root. Keep observation usable like ordinary
    compiler completion; existence anywhere in the document is insufficient. }

  if (LActiveView = nil) or (LActiveView.Find(FSelectedID) = nil) then
  begin
    FSelectedID := FActiveViewID;
  end;
end;

function TNyxStudioSession.DraftSource: TNyxText;
begin

  if FSourceDraftPending then
  begin
    Exit(FSourceDraft);
  end;
  Result := Source;
end;

function TNyxStudioSession.SourceFileInputs: INyxPascalProject;
var
  LSource: TNyxText;
  LFiles: TNyxPascalProjectFiles;
begin
  LSource := Source;
  LFiles := FSourceWorkspace.Capture.Files;

  if LFiles.IsEmpty then
  begin
    raise ENyxModel.Create('This project has no supporting Pascal file inventory');
  end;

  if (FFileInputCache = nil) or (FFileInputCache.Source <> LSource) or
    (FFileInputKey <> LFiles.Encoded) then
  begin
    FFileInputCache := LFiles.Open(LSource);
    FFileInputKey := LFiles.Encoded;
  end;
  Result := FFileInputCache;
end;

function TNyxStudioSession.AcceptedPascalProject: INyxPascalProject;
var
  LSource: TNyxText;
begin

  if not FSourceWorkspace.Capture.Files.IsEmpty then
  begin
    Exit(SourceFileInputs);
  end;
  LSource := Source;

  if (FFileInputCache = nil) or (FFileInputCache.Source <> LSource) or
    (FFileInputKey <> '') then
  begin
    FFileInputCache := NewNyxPascalProject(NyxPascalUnit(NyxCompanionUnitName(LSource)), LSource);
    FFileInputKey := '';
  end;
  Result := FFileInputCache;
end;

function TNyxStudioSession.GetActiveSourceFile: TNyxPascalSourceRef;
var
  LIndex: Integer;
begin
  Result := TNyxPascalSourceRef.Main;
  for LIndex := 0 to SourceFileCount - 1 do
  begin

    if SourceFile(LIndex).SameFile(FActiveSourceFile) then
    begin
      Exit(FActiveSourceFile);
    end;
  end;
end;

procedure TNyxStudioSession.SetActiveSourceFile(const AFile: TNyxPascalSourceRef);
begin
  SourceFileText(AFile);
  FActiveSourceFile := AFile;
end;

function TNyxStudioSession.GetActiveSourceText: TNyxText;
begin
  Result := DraftFileText(ActiveSourceFile);
end;

function TNyxStudioSession.SourceFileCount: Integer;
begin
  Result := 1;

  if not FSourceWorkspace.Capture.Files.IsEmpty then
  begin
    Inc(Result, SourceFileInputs.Count);
  end;
end;

function TNyxStudioSession.SourceFile(AIndex: Integer): TNyxPascalSourceRef;
var
  LInput: INyxPascalInput;
begin

  if (AIndex < 0) or (AIndex >= SourceFileCount) then
  begin
    raise ENyxModel.Create('Source file index is outside this project');
  end;

  if AIndex = 0 then
  begin
    Exit(TNyxPascalSourceRef.Main);
  end;
  LInput := SourceFileInputs.Input(AIndex - 1);

  if LInput.Kind = npiUnit then
  begin
    Result := TNyxPascalSourceRef.UnitFile(LInput.UnitReference);
  end
  else
  begin
    Result := TNyxPascalSourceRef.IncludeFile(NyxPascalInclude(LInput.Name));
  end;
end;

function TNyxStudioSession.SourceFileTitle(const AFile: TNyxPascalSourceRef): TNyxText;
begin
  AFile.Validate;

  if AFile.Role <> npsMain then
  begin
    Exit(AFile.Name);
  end;

  if not FSourceWorkspace.Capture.Files.IsEmpty then
  begin
    Result := SourceFileInputs.MainUnit.Name + '.pas';
  end
  else
  begin
    Result := NyxCompanionUnitName(Source) + '.pas';
  end;
end;

function TNyxStudioSession.SourceFileText(const AFile: TNyxPascalSourceRef): TNyxText;
begin
  AFile.Validate;

  if AFile.Role = npsMain then
  begin
    Exit(Source);
  end;
  Result := AFile.Text(SourceFileInputs);
end;

function TNyxStudioSession.DraftFileText(const AFile: TNyxPascalSourceRef): TNyxText;
begin
  AFile.Validate;

  if AFile.Role = npsMain then
  begin
    Exit(DraftSource);
  end;

  if FFileDrafts.Contains(AFile) then
  begin
    Exit(FFileDrafts.Draft(AFile).Text);
  end;
  Result := SourceFileText(AFile);
end;

procedure TNyxStudioSession.SetFileDraft(const AFile: TNyxPascalSourceRef;
  const AText: TNyxText);
begin
  AFile.Validate;

  if AFile.Role = npsMain then
  begin
    SetSourceDraft(AText);
    Exit;
  end;
  FFileDrafts := FFileDrafts.Editing(AFile, SourceFileText(AFile), AText);

  if FSourceDiagnosticFile.SameFile(AFile) then
  begin
    FSourceDiagnostic := Default(TNyxSourceDiagnostic);
    FSourceDiagnosticSource := '';
  end;
end;

procedure TNyxStudioSession.DiscardFileDraft(const AFile: TNyxPascalSourceRef);
begin
  AFile.Validate;

  if AFile.Role = npsMain then
  begin
    DiscardSourceDraft;
  end
  else
  begin
    FFileDrafts := FFileDrafts.Without(AFile);

    if FSourceDiagnosticFile.SameFile(AFile) then
    begin
      FSourceDiagnostic := Default(TNyxSourceDiagnostic);
      FSourceDiagnosticSource := '';
    end;
  end;
end;

function TNyxStudioSession.SourceDraftsPending: Boolean;
begin
  Result := FSourceDraftPending or (FFileDrafts.Count > 0);
end;

procedure TNyxStudioSession.SetSourceDraft(const ASource: TNyxText);
var
  LAccepted: TNyxText;
begin
  { Once a draft is open, its base deliberately stays fixed. Ordinary typing
    must not regenerate the entire accepted project on every keystroke. Reverting
    to that base still refreshes it, so direct public mutations remain visible. }

  if FSourceDraftPending and (ASource <> FSourceDraftBase) then
  begin

    if ASource <> FSourceDraft then
    begin
      FSourceDiagnostic := Default(TNyxSourceDiagnostic);
      FSourceDiagnosticSource := '';
    end;
    FSourceDraft := ASource;
    Exit;
  end;
  LAccepted := Source;

  if ASource <> LAccepted then
  begin
    CancelPlacement;
  end;

  if ASource <> DraftSource then
  begin
    FSourceDiagnostic := Default(TNyxSourceDiagnostic);
    FSourceDiagnosticSource := '';
  end;

  if not FSourceDraftPending then
  begin
    FSourceDraftBase := LAccepted;
  end;
  FSourceDraft := ASource;
  FSourceDraftPending := ASource <> LAccepted;
end;

procedure TNyxStudioSession.DiscardSourceDraft;
begin
  FSourceDiagnostic := Default(TNyxSourceDiagnostic);
  FSourceDiagnosticSource := '';
  FSourceDraft := '';
  FSourceDraftBase := '';
  FSourceDraftPending := False;
end;

procedure TNyxStudioSession.RestoreSourceDraft(const ASource, ABase: TNyxText);
begin
  FSourceDiagnostic := Default(TNyxSourceDiagnostic);
  FSourceDiagnosticSource := '';
  FSourceDraft := ASource;
  FSourceDraftBase := ABase;
  FSourceDraftPending := ASource <> Source;
end;

procedure TNyxStudioSession.DoApplySourceDraft;
var
  LCandidate: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSource: TNyxText;
begin
  LSource := DraftSource;

  if LSource = Source then
  begin
    DiscardSourceDraft;
    Exit;
  end;

  if FSourceDraftBase <> Source then
  begin
    raise ENyxSource.CreateAt(
      'The design changed while this draft was open. Save the draft, restore ' +
      'accepted Pascal and merge your edits before applying', LSource, 1);
  end;
  LCandidate := nil;
  LWorkspace := nil;
  try
    LCandidate := FSourceWorkspace.PrepareCandidate(FDocument, LSource, LWorkspace);
    { Comment/helper-only edits and changed designs publish their exact admitted
      pair through one source/design checkpoint. }
    PublishCapturedPair(LCandidate, LWorkspace, FSourceWorkspace.Capture(FDocument), False);
    DiscardSourceDraft;
    TrimUndoHistory;

    if FDocument.Find(FActiveViewID) = nil then
    begin
      FActiveViewID := '';
    end;

    if FDocument.Find(FSelectedID) = nil then
    begin
      FSelectedID := FActiveViewID;
    end;
  finally
    LWorkspace.Free;
    LCandidate.Free;
  end;
end;

function TNyxStudioSourceRequest.GetOrigin: TNyxSourceOrigin;
begin
  Result := FCheckpoint.Origin;
end;

function TNyxStudioSourceRequest.GetFiles: TNyxPascalProjectFiles;
begin
  Result := FInputFiles;
end;

function TNyxStudioSourceRequest.IsFileEdit: Boolean;
begin
  Result := FFile.Role <> npsMain;
end;

function TNyxStudioSession.PrepareSourceRequest(
  ASchemaRevision: Integer): TNyxStudioSourceRequest;
var
  LAccepted: TNyxText;
begin
  Result := Default(TNyxStudioSourceRequest);
  LAccepted := Source;
  Result.FOwner := FSourceIdentity;
  Result.FGeneration := FSourceGeneration;
  Result.FAccepted := LAccepted;
  Result.FFileBase := LAccepted;
  Result.FCheckpoint := FSourceWorkspace.Capture;
  Result.FUndo := NyxStudioCheckpoint(Result.FCheckpoint, '', '', False, FFileDrafts);
  Result.FInputFiles := Result.FCheckpoint.Files;
  Result.FFile := TNyxPascalSourceRef.Main;
  Result.FSource := DraftSource;
  Result.FBufferBaseline := Result.FSource;
  Result.FSchemaRevision := ASchemaRevision;
  Result.FChanged := Result.FSource <> LAccepted;

  if Result.FChanged and (FSourceDraftBase <> LAccepted) then
  begin
    raise ENyxSource.CreateAt(
      'The design changed while this draft was open. Save the draft, restore ' +
      'accepted Pascal and merge your edits before applying', Result.FSource, 1);
  end;
end;

function TNyxStudioSession.PrepareFileRequest(const AFile: TNyxPascalSourceRef;
  ASchemaRevision: Integer): TNyxStudioSourceRequest;
var
  LProject: INyxPascalProject;
  LInput: INyxPascalInput;
  LIndex: Integer;
begin
  AFile.Validate;

  if AFile.Role = npsMain then
  begin
    Exit(PrepareSourceRequest(ASchemaRevision));
  end;
  Result := Default(TNyxStudioSourceRequest);
  Result.FOwner := FSourceIdentity;
  Result.FGeneration := FSourceGeneration;
  Result.FCheckpoint := FSourceWorkspace.Capture;
  Result.FUndo := NyxStudioCheckpoint(Result.FCheckpoint,
    FSourceDraft, FSourceDraftBase, FSourceDraftPending, FFileDrafts);
  Result.FAccepted := Source;
  Result.FSource := Result.FAccepted;
  Result.FInputFiles := Result.FCheckpoint.Files;
  Result.FFile := AFile;
  Result.FFileBase := SourceFileText(AFile);
  Result.FBufferBaseline := DraftFileText(AFile);
  Result.FSchemaRevision := ASchemaRevision;
  Result.FChanged := Result.FFileBase <> Result.FBufferBaseline;

  if not Result.FChanged then
  begin
    Exit;
  end;

  if FFileDrafts.Draft(AFile).Base <> Result.FFileBase then
  begin
    raise ENyxStudioDraftResolutionRequired.Create(
      'This file changed after the draft began. Save or merge the draft before applying');
  end;
  LProject := SourceFileInputs;
  for LIndex := 0 to LProject.Count - 1 do
  begin
    LInput := LProject.Input(LIndex);

    if LInput.Name = AFile.Name then
    begin

      if AFile.Role = npsUnit then
      begin
        LProject := LProject.WithUnit(LInput.UnitReference, Result.FBufferBaseline);
      end
      else
      begin
        LProject := LProject.WithInclude(NyxPascalInclude(LInput.Name), Result.FBufferBaseline);
      end;
      Break;
    end;
  end;
  Result.FInputFiles := TNyxPascalProjectFiles.Capture(LProject);
end;

function TNyxStudioFileRemovalReview.GetDefined: Boolean;
begin
  Result := FOwner <> '';
end;

function TNyxStudioSession.ReviewFileRemoval(
  const AFile: TNyxPascalSourceRef): TNyxStudioFileRemovalReview;
begin
  TNyxPascalFileChange.Remove(AFile);
  Result := Default(TNyxStudioFileRemovalReview);
  Result.FOwner := FSourceIdentity;
  Result.FGeneration := FSourceGeneration;
  Result.FFile := AFile;
  Result.FAccepted := SourceFileText(AFile);
  Result.FBuffer := DraftFileText(AFile);
end;

function TNyxStudioSession.PrepareReviewedFileRemoval(
  const AReview: TNyxStudioFileRemovalReview;
  ASchemaRevision: Integer): TNyxStudioSourceRequest;
var
  LIndex: Integer;
  LFound: Boolean;
begin

  if not AReview.IsDefined or (AReview.FOwner <> FSourceIdentity) or
    (AReview.FGeneration <> FSourceGeneration) then
  begin
    raise ENyxProjectConflict.Create('This source-file removal review belongs to an earlier project');
  end;
  LFound := False;
  for LIndex := 0 to SourceFileCount - 1 do
  begin

    if SourceFile(LIndex).SameFile(AReview.FFile) then
    begin
      LFound := True;
      Break;
    end;
  end;

  if not LFound or (SourceFileText(AReview.FFile) <> AReview.FAccepted) or
    (DraftFileText(AReview.FFile) <> AReview.FBuffer) then
  begin
    raise ENyxProjectConflict.Create('The reviewed source file or draft changed; review removal again');
  end;
  Result := PrepareFileChangeRequest(TNyxPascalFileChange.Remove(AReview.FFile), ASchemaRevision);
end;

function TNyxStudioSession.PrepareFileChangeRequest(const AChange: TNyxPascalFileChange;
  ASchemaRevision: Integer): TNyxStudioSourceRequest;
var
  LProject: INyxPascalProject;
begin
  AChange.Validate;

  if FSourceWorkspace.Capture.Files.IsEmpty then
  begin
    { A legacy single-unit project has no captured helper scope yet. Stage its
      declared main identity independently; only successful publication may add
      the first inventory. Existing-file lookup retains its stricter contract. }
    LProject := NewNyxPascalProject(NyxPascalUnit(NyxCompanionUnitName(Source)), Source);
  end
  else
  begin
    LProject := SourceFileInputs;
  end;
  LProject := AChange.Candidate(LProject);
  Result := Default(TNyxStudioSourceRequest);
  Result.FOwner := FSourceIdentity;
  Result.FGeneration := FSourceGeneration;
  Result.FCheckpoint := FSourceWorkspace.Capture;
  Result.FUndo := NyxStudioCheckpoint(Result.FCheckpoint,
    FSourceDraft, FSourceDraftBase, FSourceDraftPending, FFileDrafts);
  Result.FAccepted := Source;
  Result.FSource := Result.FAccepted;
  Result.FInputFiles := TNyxPascalProjectFiles.Capture(LProject);
  Result.FFile := AChange.FileReference;
  Result.FFileChange := AChange;
  Result.FActiveFile := ActiveSourceFile;
  Result.FSchemaRevision := ASchemaRevision;
  Result.FChanged := True;

  if AChange.Kind = npfcRemove then
  begin
    Result.FFileBase := SourceFileText(Result.FFile);
    Result.FBufferBaseline := DraftFileText(Result.FFile);
  end;
end;

function TNyxStudioSession.MatchesSourceFileRequest(
  const ARequest: TNyxStudioSourceRequest): Boolean;
var
  LIndex: Integer;
  LFound: Boolean;
begin

  if ARequest.IsProjectSourceEdit and SourceDraftsPending then
  begin
    Exit(False);
  end;
  LFound := False;
  for LIndex := 0 to SourceFileCount - 1 do
  begin

    if SourceFile(LIndex).SameFile(ARequest.FileReference) then
    begin
      LFound := True;
      Break;
    end;
  end;

  if ARequest.FileChange.IsDefined and (ARequest.FileChange.Kind = npfcCreate) then
  begin
    Exit(not LFound);
  end;
  Result := LFound and (SourceFileText(ARequest.FFile) = ARequest.FFileBase) and
    (DraftFileText(ARequest.FFile) = ARequest.FBufferBaseline);
end;

function TNyxStudioSession.RefreshFileRequest(const ARequest: TNyxStudioSourceRequest;
  ASchemaRevision: Integer): TNyxStudioSourceRequest;
begin

  if ARequest.IsSourceEdit or (ARequest.FOwner <> FSourceIdentity) or
    (ARequest.FGeneration <> FSourceGeneration) or
    not MatchesSourceFileRequest(ARequest) then
  begin
    raise ENyxProjectConflict.Create('Queued file Apply no longer matches its selected buffer');
  end;
  if ARequest.FileChange.IsDefined then
  begin
    Result := PrepareFileChangeRequest(ARequest.FileChange, ASchemaRevision);
    { A confirmation captures presentation too; queued dispatch must not replace
      its original navigation baseline with the user's later file choice. }
    Result.FActiveFile := ARequest.FActiveFile;
  end
  else
  begin
    Result := PrepareFileRequest(ARequest.FFile, ASchemaRevision);
  end;
end;

function TNyxStudioSession.PrepareSourceEditRequest(const ASource: TNyxText;
  ASchemaRevision: Integer): TNyxStudioSourceRequest;
begin

  if SourceDraftsPending then
  begin
    raise ENyxStudioDraftResolutionRequired.Create(
      'Resolve the pending Pascal draft before source edits');
  end;

  if (ASource = '') or (ASource = Source) then
  begin
    raise ENyxModel.Create('Source edit requires changed nonempty proposed Pascal');
  end;
  Result := PrepareSourceRequest(ASchemaRevision);
  Result.FSource := ASource;
  Result.FChanged := True;
  Result.FSourceEdit := True;
end;

function TNyxStudioSession.PrepareSourceEditRequest(const AProject: INyxPascalProject;
  ASchemaRevision: Integer): TNyxStudioSourceRequest;
var
  LProject: INyxPascalProject;
  LOriginal: INyxPascalProject;
  LFiles: TNyxPascalProjectFiles;
  LIndex: Integer;
begin

  if SourceDraftsPending then
  begin
    raise ENyxStudioDraftResolutionRequired.Create(
      'Resolve all pending Pascal file drafts before project source edits');
  end;
  LProject := CaptureNyxPascalProject(AProject);
  LOriginal := FSourceWorkspace.Capture.Files.Open(Source);

  if LOriginal = nil then
  begin
    LOriginal := NewNyxPascalProject(NyxPascalUnit(NyxCompanionUnitName(Source)), Source);
  end;

  if (LProject.MainUnit.Name <> LOriginal.MainUnit.Name) or
    (LProject.Count <> LOriginal.Count) or
    (LProject.Limits.MaximumFiles <> LOriginal.Limits.MaximumFiles) or
    (LProject.Limits.MaximumBytes <> LOriginal.Limits.MaximumBytes) then
  begin
    raise ENyxProjectConflict.Create('Source ranges retain the complete file inventory and limits');
  end;
  for LIndex := 0 to LOriginal.Count - 1 do
  begin

    if (LProject.Input(LIndex).Kind <> LOriginal.Input(LIndex).Kind) or
      (LProject.Input(LIndex).Name <> LOriginal.Input(LIndex).Name) then
    begin
      raise ENyxProjectConflict.Create('Source ranges cannot rename, reorder, create or remove files');
    end;
  end;
  LFiles := TNyxPascalProjectFiles.Capture(LProject);

  if (LProject.Source = Source) and
    (LFiles.Encoded = TNyxPascalProjectFiles.Capture(LOriginal).Encoded) then
  begin
    raise ENyxModel.Create('Project source edit requires changed accepted input');
  end;
  Result := PrepareSourceRequest(ASchemaRevision);
  Result.FSource := LProject.Source;
  Result.FInputFiles := LFiles;
  Result.FSourceEdit := True;
  Result.FProjectSourceEdit := True;
  Result.FChanged := True;
end;

function TNyxStudioSession.PrepareSourceFileChanges(
  const AChanges: array of TNyxPascalFileChange;
  const AProject: INyxPascalProject;
  ASchemaRevision: Integer): TNyxStudioSourceRequest;
var
  LOriginal: INyxPascalProject;
  LExpected: INyxPascalProject;
  LProject: INyxPascalProject;
  LIndex: Integer;
begin

  if SourceDraftsPending then
  begin
    raise ENyxStudioDraftResolutionRequired.Create(
      'Resolve all pending Pascal file drafts before membership changes');
  end;
  LOriginal := FSourceWorkspace.Capture.Files.Open(Source);

  if LOriginal = nil then
  begin
    LOriginal := NewNyxPascalProject(NyxPascalUnit(NyxCompanionUnitName(Source)), Source);
  end;
  LExpected := NyxPascalFileChanges(AChanges).Stage(LOriginal);
  LProject := CaptureNyxPascalProject(AProject);

  if (LProject.MainUnit.Name <> LExpected.MainUnit.Name) or
    (LProject.Count <> LExpected.Count) or
    (LProject.Limits.MaximumFiles <> LExpected.Limits.MaximumFiles) or
    (LProject.Limits.MaximumBytes <> LExpected.Limits.MaximumBytes) then
  begin
    raise ENyxProjectConflict.Create('The proposal differs from its typed membership inventory');
  end;
  for LIndex := 0 to LExpected.Count - 1 do
  begin

    if (LProject.Input(LIndex).Kind <> LExpected.Input(LIndex).Kind) or
      (LProject.Input(LIndex).Name <> LExpected.Input(LIndex).Name) then
    begin
      raise ENyxProjectConflict.Create('Typed membership retains exact file identities and order');
    end;
  end;
  for LIndex := 0 to High(AChanges) do
  begin

    if (AChanges[LIndex].Kind = npfcCreate) and
      (AChanges[LIndex].FileReference.Text(LProject) <> AChanges[LIndex].Text) then
    begin
      raise ENyxProjectConflict.Create('A created file retains its exact typed initial text');
    end;
  end;
  Result := PrepareSourceRequest(ASchemaRevision);
  Result.FSource := LProject.Source;
  Result.FInputFiles := TNyxPascalProjectFiles.Capture(LProject);
  Result.FSourceEdit := True;
  Result.FProjectSourceEdit := True;
  Result.FChanged := (LProject.Source <> Source) or
    (Result.FInputFiles.Encoded <> TNyxPascalProjectFiles.Capture(LOriginal).Encoded);

  if not Result.FChanged then
  begin
    raise ENyxModel.Create('File membership group has no resulting changes');
  end;
end;

function TNyxStudioSession.CompleteSourceRequest(
  const ARequest: TNyxStudioSourceRequest;
  const APrepared: INyxPreparedSource): TNyxSourceCompletion;
var
  LAccepted: TNyxText;
  LCheckpoint: TNyxSourceCheckpoint;
  LCandidate: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LPublication: TSourcePairPublication;
  LAction: INyxSchemaAction;
  LActiveView: TNyxNode;
  LRemainingFileDrafts: TNyxPascalFileDrafts;
begin
  Result := nscStale;

  if (ARequest.FOwner <> FSourceIdentity) or
    (ARequest.FGeneration <> FSourceGeneration) or
    (ARequest.FSchemaRevision <> NyxSchemaRevision) then
  begin
    Exit;
  end;
  { An executed workspace cannot reconcile an out-of-band visual mutation on
    this completion path. Refuse the stale result before asking Render to change
    source, and leave the caller's newer document and detached result untouched. }

  if (FSourceWorkspace.Origin = nsoExecuted) and
    (TNyxCodec.Encode(FDocument) <> ARequest.FCheckpoint.Design) then
  begin
    Exit;
  end;
  LAccepted := Source;
  LCheckpoint := FSourceWorkspace.Capture;

  if (LAccepted <> ARequest.FAccepted) or
    (LCheckpoint.Design <> ARequest.FCheckpoint.Design) or
    (LCheckpoint.Files.Encoded <> ARequest.FCheckpoint.Files.Encoded) or
    not MatchesSourceFileRequest(ARequest) then
  begin
    Exit;
  end;

  if not ARequest.Changed then
  begin
    DiscardFileDraft(ARequest.FFile);
    Exit(nscUnchanged);
  end;

  if (APrepared = nil) or (APrepared.Source <> ARequest.Source) or
    (APrepared.SchemaRevision <> ARequest.SchemaRevision) then
  begin
    raise ENyxModel.Create('Source completion does not match its captured command');
  end;

  if APrepared.Diagnostic.Defined then
  begin
    FSourceDiagnostic := APrepared.Diagnostic;
    FSourceDiagnosticSource := ARequest.Source;
    FSourceDiagnosticFile := ARequest.FileReference;

    if ARequest.IsFileEdit then
    begin
      FSourceDiagnosticSource := ARequest.FBufferBaseline;
      { Legacy providers have no helper mapping. A copied compiler report may
        supply a site below only after exact candidate/buffer checks. }
      FSourceDiagnostic.Line := 0;
      FSourceDiagnostic.Column := 0;
    end;
    CaptureCompilerDiagnostic(ARequest, APrepared);
    Exit(nscRejected);
  end;
  LCandidate := nil;
  LWorkspace := nil;
  try
    LRemainingFileDrafts := FFileDrafts;

    if ARequest.IsFileEdit then
    begin
      LRemainingFileDrafts := FFileDrafts.Without(ARequest.FileReference);
    end;
    APrepared.Take(LCandidate, LWorkspace);
    { A provider cannot satisfy a helper command with a main-only result or a
      different helper snapshot, even when both happen to construct equal UI. }

    if (LWorkspace = nil) or
      (LWorkspace.Capture.Files.Encoded <> ARequest.Files.Encoded) then
    begin
      raise ENyxProjectConflict.Create('Source completion has different supporting Pascal inputs');
    end;
    { The trusted processor contract already completely admitted this independent
      pair. No mutable staged handle was exposed before Take; replay/validation
      on the UI would duplicate that work. Serialized project/MCP imports do not
      enter here. The private browser handoff decodes and validates its pair. }

    { LCheckpoint was freshly synchronized above. Reusing that exact value
      avoids a second full accepted encoding on the UI thread. Admission itself
      already happened on the isolated processor; no Pascal replay occurs here. }
    LPublication := TSourcePairPublication.Create;
    LAction := LPublication;
    LPublication.Session := Self;
    LPublication.Checkpoint := LCheckpoint;
    LPublication.RememberDraft := ARequest.IsFileEdit;
    LPublication.AllowProjectFiles := ARequest.IsFileEdit or ARequest.IsProjectSourceEdit;
    LPublication.RemainingFileDrafts := LRemainingFileDrafts;
    LPublication.UseCapturedHistory := True;
    LPublication.CapturedHistory := ARequest.FUndo;
    LPublication.Document := LCandidate;
    LPublication.Workspace := LWorkspace;
    LCandidate := nil;
    LWorkspace := nil;

    if not CommitNyxSchemaRevision(ARequest.SchemaRevision, LAction) then
    begin
      Exit;
    end;

    if not ARequest.IsFileEdit then
    begin
      DiscardSourceDraft;
    end
    else
    begin
      FSourceDiagnostic := Default(TNyxSourceDiagnostic);
      FSourceDiagnosticSource := '';
    end;

    if ARequest.FileChange.IsDefined and (ARequest.FileChange.Kind = npfcCreate) and
      ActiveSourceFile.SameFile(ARequest.FActiveFile) then
    begin
      FActiveSourceFile := ARequest.FileReference;
    end;

    LActiveView := FDocument.Find(FActiveViewID);

    if (LActiveView = nil) or (LActiveView.Parent <> nil) then
    begin
      FActiveViewID := '';

      if FDocument.Count > 0 then
      begin
        FActiveViewID := FDocument.Pages[0].ID;
      end
      else if FDocument.ComponentCount > 0 then
      begin
        FActiveViewID := FDocument.Components[0].ID;
      end;
      LActiveView := FDocument.Find(FActiveViewID);
    end;

    { Complete Pascal may replace every root or move an old selected ID into
      another view. Publish a usable owned root and selection just as history
      restoration does, instead of leaving a successful Apply with a blank canvas. }

    if (LActiveView = nil) or (LActiveView.Find(FSelectedID) = nil) then
    begin
      FSelectedID := FActiveViewID;
    end;
    Result := nscApplied;
  finally
    LWorkspace.Free;
    LCandidate.Free;
  end;
end;

{$include nyx.studio.session.design.inc}
{$include nyx.studio.session.collections.inc}

procedure TNyxStudioSession.CaptureCompilerDiagnostic(
  const ARequest: TNyxStudioSourceRequest; const APrepared: INyxPreparedSource);
var
  LPrepared: INyxPreparedSourceReport;
  LReport: INyxCompilerReport;
  LContext: INyxCompilerSourceReport;
  LItem: TNyxCompilerDiagnostic;
  LInputs: INyxPascalProject;
  LIndex: Integer;
  LFileIndex: Integer;
  LExpected: TNyxText;
  LFound: Boolean;
begin

  if not Supports(APrepared, INyxPreparedSourceReport, LPrepared) then
  begin
    Exit;
  end;
  LReport := LPrepared.CompilerReport;

  if (LReport = nil) or (LReport.Source <> ARequest.Source) or
    not Supports(LReport, INyxCompilerSourceReport, LContext) then
  begin
    Exit;
  end;
  LInputs := nil;
  for LIndex := 0 to LReport.Count - 1 do
  begin
    LItem := LReport.Item(LIndex);

    if not LItem.Navigable or not (LItem.Severity in [csError, csFatal]) then
    begin
      Continue;
    end;
    LExpected := ARequest.Source;

    if LItem.SourceFile.Role <> npsMain then
    begin

      if ARequest.Files.IsEmpty then
      begin
        Continue;
      end;

      if LInputs = nil then
      begin
        LInputs := ARequest.Files.Open(ARequest.Source);
      end;
      LFound := False;
      for LFileIndex := 0 to LInputs.Count - 1 do
      begin

        if LInputs.Input(LFileIndex).Name = LItem.SourceFile.Name then
        begin
          LExpected := LItem.SourceFile.Text(LInputs);
          LFound := True;
          Break;
        end;
      end;

      if not LFound then
      begin
        Continue;
      end;
    end;

    if (LContext.SourceText(LItem.SourceFile) <> LExpected) or
      (DraftFileText(LItem.SourceFile) <> LExpected) then
    begin
      Continue;
    end;
    FSourceDiagnosticFile := LItem.SourceFile;
    FSourceDiagnosticSource := LExpected;
    FSourceDiagnostic.Line := LItem.SourceLine;
    FSourceDiagnostic.Column := LItem.SourceColumn;
    FSourceDiagnostic.Message := NyxCompilerSeverityName(LItem.Severity) + ': ' + LItem.Message;
    Exit;
  end;
end;

function TNyxStudioSession.GetSourceDiagnostic: TNyxSourceDiagnostic;
var
  LIndex: Integer;
begin
  Result := Default(TNyxSourceDiagnostic);

  if not FSourceDiagnostic.Defined then
  begin
    Exit;
  end;
  for LIndex := 0 to SourceFileCount - 1 do
  begin

    if SourceFile(LIndex).SameFile(FSourceDiagnosticFile) then
    begin

      if FSourceDiagnosticSource = DraftFileText(FSourceDiagnosticFile) then
      begin
        Result := FSourceDiagnostic;
      end;
      Exit;
    end;
  end;
end;

procedure TNyxStudioSession.ApplySourceDraft;
var
  LSource: TNyxText;
begin
  LSource := DraftSource;
  FSourceDiagnostic := Default(TNyxSourceDiagnostic);
  FSourceDiagnosticSource := '';
  FSourceDiagnosticFile := TNyxPascalSourceRef.Main;
  try
    DoApplySourceDraft;
  except
    on LException: Exception do
    begin
      FSourceDiagnostic.Defined := True;
      FSourceDiagnostic.Message := LException.Message;
      FSourceDiagnosticSource := LSource;

      if LException is ENyxSource then
      begin
        FSourceDiagnostic.Message := ENyxSource(LException).DiagnosticText;
        FSourceDiagnostic.Line := ENyxSource(LException).Line;
        FSourceDiagnostic.Column := ENyxSource(LException).Column;
      end;
      raise;
    end;
  end;
end;

end.
