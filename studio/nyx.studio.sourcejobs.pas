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
  nyx.source.preparation, nyx.studio.session, nyx.studio.edits, nyx.projection.refresh,
  nyx.studio.inspector, nyx.studio.collectionintent, nyx.studio.collections
  {$ifdef PAS2JS}, JS, Web{$endif};

type
  TNyxSourceCommandState = (nssIdle, nssPreparing, nssApplied, nssRejected,
    nssStale, nssCancelled, nssFailed);
  TNyxSourceCommandChanged = procedure(AState: TNyxSourceCommandState;
    const AMessage: TNyxText) of object;
  TNyxSourceCommands = class;

  { Retained couriers borrow only this revocable UI port. Detach removes its
    receiver before editor/session retirement. Workers own copied tickets and
    creator snapshots, and never dereference an accepted tree or controller. }
  INyxSourceCommandPort = interface(IInterface)
    ['{8B6CF439-AE07-4C41-A6D8-79C501B85404}']
    procedure Deliver(ASequence: Integer; const ASource: INyxPreparedSource;
      const ADesign: INyxPreparedDesign; const AFailure: TNyxText);
    procedure Detach;
  end;

  { Closed operation and value-only inputs. Design targets are captured at the
    UI event; their fresh baseline is captured at dispatch after earlier edits. }
  TNyxEditorSourceKind = (eskPascal, eskDesign);
  { UI-owned immutable metadata lease. pas2js forbids COM interfaces in record
    fields. A native worker retains Value in its OWN class field before dispatch;
    it never borrows this holder or depends on its UI lifetime. }
  TNyxEditorSchemaLease = class
  private
    FValue: INyxSchemaSnapshot;
  public
    constructor Create(const AValue: INyxSchemaSnapshot);
    property Value: INyxSchemaSnapshot read FValue;
  end;
  TNyxEditorSourceJob = record
    Kind: TNyxEditorSourceKind;
    Sequence: Integer;
    Context: TNyxStudioCommandContext;
    Source: TNyxStudioSourceRequest;
    Edit: TNyxStudioDesignEdit;
    Design: TNyxStudioDesignRequest;
    Schemas: TNyxEditorSchemaLease;
  end;

  { One processor controller belongs to one session. Native work uses the public
    scheduler and browser work the compiled Pascal worker. Apply coalesces old
    drafts; design edits remain FIFO, coalescing only adjacent queued changes to
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
    FLatestSourceSequence: Integer;
    FActive: TNyxEditorSourceJob;
    FQueue: array of TNyxEditorSourceJob;
    FRunning: Boolean;
    FDiscardActive: Boolean;
    FDetached: Boolean;
    FPublishedDesign: Boolean;
    FPublishedAction: TNyxStudioDesignAction;
    FCanvasCompletion: Boolean;
    FCompletedEdit: TNyxStudioDesignEdit;
    FCompletedContext: TNyxStudioCommandContext;
    FCompletedHandler: TNyxHandlerRef;
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
    function NextSequence: Integer;
    procedure Notify(AState: TNyxSourceCommandState; const AMessage: TNyxText;
      ADesign: Boolean = False; AAction: TNyxStudioDesignAction = sdaProperty);
    procedure Enqueue(const AJob: TNyxEditorSourceJob);
    procedure ClearQueue;
    procedure StartQueued;
    procedure Finish(ASequence: Integer; const ASource: INyxPreparedSource;
      const ADesign: INyxPreparedDesign; const AFailure: TNyxText);
  public
    constructor Create(ASession: TNyxStudioSession;
      AChanged: TNyxSourceCommandChanged; const AWorkerURL: TNyxText = 'nyx_source_worker.js');
    destructor Destroy; override;
    { Capture the exact draft; repeated Apply supersedes prior Apply while design
      commands retain their order and their own fresh paired/draft guards. }
    procedure Apply;
    { Queue closed intent with explicit target/view identities. Full admission
      and source reconciliation run on independently reconstructed owners. }
    procedure Edit(const AEdit: TNyxStudioDesignEdit);
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
    { Cancel pending intent, preserving accepted files and source draft. Native
      work may finish but a cancelled result cannot publish. }
    procedure Cancel;
    procedure Detach;
    { Consume Nyx Apply/Restore, scalar state/binding, inspector and structural
      events. Current shell form fields are borrowed only during typed capture.
      Collection capture also submits only scoped, family-qualified values. }
    function Route(ANode: TNyxNode; ATrigger: TNyxTrigger;
      AShellRoot: TNyxNode = nil): Boolean;
    property State: TNyxSourceCommandState read FState;
    property Message: TNyxText read FMessage;
    property Busy: Boolean read GetBusy;
    { Current notification's typed design effect. False for source/status-only
      notifications; consumers may follow structural results without parsing
      status text or retargeting another project's callback. }
    property PublishedDesign: Boolean read FPublishedDesign;
    property PublishedAction: TNyxStudioDesignAction read FPublishedAction;
    property OnChanged: TNyxSourceCommandChanged read FChanged write FChanged;
  end;

implementation

uses
  SysUtils, nyx.data, nyx.state, nyx.collections, nyx.binding.types, nyx.studio.authoring,
  nyx.studio.commands
  {$ifndef PAS2JS}, Classes{$endif};

type
  TSourcePort = class(TInterfacedObject, INyxSourceCommandPort)
  public
    Owner: TNyxSourceCommands;
    procedure Deliver(ASequence: Integer; const ASource: INyxPreparedSource;
      const ADesign: INyxPreparedDesign; const AFailure: TNyxText);
    procedure Detach;
  end;
  TSourceDelivery = class(TInterfacedObject, INyxWork)
  public
    Port: INyxSourceCommandPort;
    Sequence: Integer;
    Source: INyxPreparedSource;
    Design: INyxPreparedDesign;
    Failure: TNyxText;
    procedure Execute(const AExecution: INyxExecution);
  end;
  {$ifndef PAS2JS}
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

procedure TSourceDelivery.Execute(const AExecution: INyxExecution);
begin

  if not AExecution.Cancelled then
  begin
    Port.Deliver(Sequence, Source, Design, Failure);
  end;
end;

{$ifndef PAS2JS}
procedure TSourcePreparation.Execute(const AExecution: INyxExecution);
var
  LDelivery: TSourceDelivery;
  LWork: INyxWork;
begin
  LDelivery := TSourceDelivery.Create;
  LWork := LDelivery;
  LDelivery.Port := Port;
  LDelivery.Sequence := Job.Sequence;
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
  const AMessage: TNyxText; ADesign: Boolean; AAction: TNyxStudioDesignAction);
begin
  FState := AState;
  FMessage := AMessage;
  FPublishedDesign := (AState = nssApplied) and ADesign;
  FPublishedAction := AAction;
  { A rejected proposal still changed the physical input before admission.
    Restore that exact field without treating the rejected pair as published.
    Every status-only notification clears this transient completion effect. }
  FCanvasCompletion := ADesign and (AAction = sdaCanvasValue) and
    (AState in [nssApplied, nssRejected, nssStale, nssFailed]);

  if not FDetached and Assigned(FChanged) then
  begin
    FChanged(AState, AMessage);
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
    LJob.Source := FSession.PrepareSourceRequest(LJob.Schemas.Value.Revision);
    LCount := 0;
    for LIndex := 0 to High(FQueue) do
    begin

      if FQueue[LIndex].Kind <> eskPascal then
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

      if FQueue[LIndex].Kind <> eskPascal then
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
    FLatestSourceSequence := LJob.Sequence;

    if not LJob.Source.Changed then
    begin
      FSession.DiscardSourceDraft;

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

procedure TNyxSourceCommands.Edit(const AEdit: TNyxStudioDesignEdit);
var
  LJob: TNyxEditorSourceJob;
  LLast: Integer;
  LPending: TNyxStudioPendingDesign;
begin
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
    raise ENyxModel.Create('Wait for this callback removal before editing its event');
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
          scaScope, scaSelection]) and
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
      Notify(nssPreparing, 'Preparing design / accepted files retained');
    finally
      StartQueued;
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

procedure TNyxSourceCommands.StartQueued;
var
  LIndex: Integer;
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
    FRunning := True;
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
      FRunning := False;
      RetireJob(FActive);
      try
        Notify(nssFailed, 'Editor processor unavailable: ' + LException.Message);
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
  const AFailure: TNyxText);
var
  LCompletion: TNyxSourceCompletion;
  LState: TNyxSourceCommandState;
  LMessage: TNyxText;
  LDesign: Boolean;
  LAction: TNyxStudioDesignAction;
begin
  FScheduler.RequireUI;

  if FDetached or not FRunning or (ASequence <> FActive.Sequence) then
  begin
    Exit;
  end;
  FRunning := False;

  if not FDiscardActive and ((FActive.Kind = eskDesign) or
    (ASequence = FLatestSourceSequence)) then
  begin
    try

      if AFailure <> '' then
      begin
        LState := nssFailed;
        LMessage := AFailure;
      end
      else
      begin

        if FActive.Kind = eskPascal then
        begin
          LCompletion := FSession.CompleteSourceRequest(FActive.Source, ASource);
        end
        else
        begin
          LCompletion := FSession.CompleteDesignRequest(FActive.Design, ADesign);
        end;
        case LCompletion of
          nscUnchanged, nscApplied:
            begin
              LState := nssApplied;

              if FActive.Kind = eskPascal then
              begin
                LMessage := 'Pascal applied / one Undo restores the pair';
              end
              else
              begin
                LMessage := 'Design / Pascal updated / one Undo restores the pair';
              end;
            end;
          nscRejected:
            begin
              LState := nssRejected;

              if FActive.Kind = eskPascal then
              begin
                LMessage := FSession.SourceDiagnostic.Message;
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
    LDesign := FActive.Kind = eskDesign;
    LAction := FActive.Edit.Action;
    FCompletedEdit := FActive.Edit;
    FCompletedContext := FActive.Context;
    FCompletedHandler := Default(TNyxHandlerRef);

    if (LState = nssApplied) and LDesign and (LAction = sdaEvent) then
    begin
      FCompletedHandler := ADesign.AddedHandler;
    end;

    if (LState = nssApplied) and LDesign then
    begin
      RetirePublishedNameDraft;
    end;
    RetireJob(FActive);
    try
      Notify(LState, LMessage, LDesign, LAction);
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

procedure TNyxSourceCommands.Cancel;
begin
  FScheduler.RequireUI;
  NextSequence;
  ClearQueue;
  FDiscardActive := True;
  {$ifdef PAS2JS}
  RetireWorker;
  RetireJob(FActive);
  FRunning := False;
  {$endif}
  Notify(nssCancelled, 'Pending editor changes cancelled / current files and draft retained');
end;

procedure TNyxSourceCommands.Detach;
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
end;

function TNyxSourceCommands.CompletedEvent(out AIntent: TNyxStudioEventIntent;
  out AOwner, AView: TNyxText; out AAddedHandler: TNyxHandlerRef): Boolean;
begin
  AIntent := Default(TNyxStudioEventIntent);
  AOwner := '';
  AView := '';
  AAddedHandler := Default(TNyxHandlerRef);
  Result := FPublishedDesign and (FPublishedAction = sdaEvent) and
    FSession.MatchesCommandContext(FCompletedContext);

  if Result then
  begin
    AIntent := FCompletedEdit.Event;
    AOwner := FCompletedEdit.Selection;
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

  if Result and (LEdit.Action = sdaEvent) then
  begin
    Edit(LEdit);
  end;
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

  if ATrigger = ntClick then
  begin

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
      Apply;
      Exit(True);
    end;

    if ANode.ID = CRestore then
    begin
      Cancel;
      FSession.DiscardSourceDraft;
      Notify(nssIdle, 'Accepted Pascal restored');
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

      if LWaiting then
      begin
        CheckSynchronize(1);
        Sleep(1);
      end;
    until not LWaiting;
    FExecutions := nil;
    {$endif}
  end;
  FPort := nil;
  RetireJob(FActive);
  ClearQueue;
  inherited Destroy;
end;

end.
