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
unit nyx.collections.clipboard.commands;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.collections.view, nyx.collections.mount,
  nyx.collections.clipboard.actions, nyx.events, nyx.clipboard, nyx.scheduler;

{ Mounts provide the ORIGINAL canonical input port and a borrowed synchronous
  final-fence factory. The controller owns no mount, renderer or control. The
  factory is withdrawn on Revoke and is entered only while Input.Current. }
function NewNyxCollectionClipboardCommands(const AView: INyxCollectionView;
  const AInput: INyxCollectionInputLifetime; const AEvents: INyxEvents;
  const AHost: INyxClipboardWriteHost; const AScheduler: INyxScheduler;
  const AOptions: TNyxCollectionClipboardOptions;
  AReadiness: TNyxCollectionClipboardReadinessFactory): INyxCollectionClipboardCommands;

implementation

uses SysUtils, nyx.text, nyx.collections, nyx.collections.selection,
  nyx.collections.arrangement, nyx.collections.arrangement.controls,
  nyx.collections.clipboard, nyx.collections.view.types;

type
  { Forward controller is borrowed only under an entered managed owner lease. }
  TCommands = class;

  { Forward command never belongs to the controller's pending-host slot. }
  TCommand = class;

  { Owner-free cancellation passed to asynchronous clipboard hosts. It retains
    only certified flags-only parent readiness, canonical input and its own
    synchronized cancel flag. The mounted final fence is deliberately absent. }
  TCopyScope = class(TInterfacedObject, INyxCancellationScope, INyxCancellationReadiness)
  private
    FInput: INyxCollectionInputLifetime;
    FEpoch: Integer;
    FParent: INyxCancellationReadiness;
    FLocal: INyxCancellationScope;
  public
    constructor Create(const AInput: INyxCollectionInputLifetime; AEpoch: Integer;
      const AParent: INyxCancellationReadiness);
    procedure Cancel;
    function GetCancelled: Boolean;
    procedure CheckReady;
  end;

  { One host invocation's weak receiver. Controller detaches Owner before
    cancellation; local managed leases protect an entered inline callback.
    Payload is immutable and independent of the mounted final input fence. }
  TReceipt = class(TInterfacedObject)
  public
    Owner: TCommands;
    Serial: Integer;
    Delivered: Boolean;
    Payload: INyxCollectionClipboardText;
    Scope: INyxCancellationReadiness;
    Reply: TNyxClipboardWriteReply;
    ReplyOwner: IInterface;
    procedure Receive(const AResult: TNyxClipboardWriteResult);
  end;

  { Exact-slot cancellation owns the command controller, never its mount.
    A stale ticket cannot cancel a newer accepted keyboard or context action. }
  TCopyTicket = class(TInterfacedObject, INyxClipboardWriteRequest)
  private
    FOwner: TCommands;
    FLease: INyxCollectionClipboardCommands;
    FSerial: Integer;
    FCancelled: Boolean;
  public
    constructor Create(AOwner: TCommands; ASerial: Integer);
    procedure Cancel;
  end;

  { Prepared input token owns exact source/page/selection/column proof and the
    synchronous final fence. It retains its controller, never the reverse.
    Retire after transfer cannot cancel the independent asynchronous request. }
  TCommand = class(TInterfacedObject, INyxCollectionClipboardCommand,
    INyxCollectionClipboardCellCommand, INyxCollectionClipboardPreparedScope)
  private
    FOwner: TCommands;
    FOwnerLease: INyxCollectionClipboardCommands;
    FView: INyxCollectionView;
    FInput: INyxCollectionInputLifetime;
    FEvents: INyxEvents;
    FOptions: TNyxCollectionClipboardOptions;
    FAction: TNyxCollectionClipboardAction;
    FItem: TNyxItemRef;
    FField: TNyxColumnIdentity;
    FEpoch: Integer;
    FSerial: Integer;
    FEntryReady: Boolean;
    FEligible: Boolean;
    FCellResolved: Boolean;
    FPrepared: Boolean;
    FCompleteStarted: Boolean;
    FRetired: Boolean;
    FChecking: Boolean;
    FReadiness: INyxCancellationReadiness;
    FStore: INyxCollection;
    FSource: INyxCollectionSnapshot;
    FPage: INyxCollectionSnapshot;
    FSelection: TNyxCollectionSelectionSnapshot;
    FColumns: INyxCollectionColumns;
    FColumnSerial: Integer;
    FColumnBaseline: TNyxText;
    FParent: INyxCancellationScope;
    FParentReady: INyxCancellationReadiness;
    FScope: INyxCancellationScope;
    FScopeReady: INyxCancellationReadiness;
    FRevision: Integer;
    FPayload: INyxCollectionClipboardText;
    function OriginalFlags(ACompleting: Boolean = False): Boolean;
    function CurrentProof(ACompleting: Boolean): Boolean;
    function CheckProof(ACompleting: Boolean): Boolean;
    function GetCurrent: Boolean;
    function GetDispatchCurrent: Boolean;
    function GetPayload: INyxCollectionClipboardText;
    function GetReadiness: INyxCancellationReadiness;
    procedure Prepare;
    procedure ResolveCell(const AItem: TNyxItemRef;
      const AField: TNyxColumnIdentity);
    function Complete(AAllowDefault: Boolean): Boolean;
    procedure Retire;
  public
    constructor Create(AOwner: TCommands; AAction: TNyxCollectionClipboardAction;
      const AItem: TNyxItemRef; const AField: TNyxColumnIdentity;
      AResolveLater: Boolean = False);
  end;

  { Runtime controller keeps one exact pending receipt/request and immutable
    authority. A receipt borrows this owner; it is revoked before host Cancel.
    No member retains a command or its mount-retaining synchronous fence. }
  TCommands = class(TInterfacedObject, INyxCollectionClipboardCommands,
    INyxCollectionClipboardCellInput, INyxCollectionClipboardCapturedWrite)
  private
    FView: INyxCollectionView;
    FInput: INyxCollectionInputLifetime;
    FEvents: INyxEvents;
    FHost: INyxClipboardWriteHost;
    FScheduler: INyxScheduler;
    FOptions: TNyxCollectionClipboardOptions;
    FReadiness: TNyxCollectionClipboardReadinessFactory;
    FRevoked: Boolean;
    FSerial: Integer;
    FRequest: INyxClipboardWriteRequest;
    FReceipt: TReceipt;
    FReceiptLease: IInterface;
    FPhase: TNyxCollectionClipboardPhase;
    FResult: TNyxClipboardWriteResult;
    FPayload: INyxCollectionClipboardText;
    FObserver: TNyxCollectionClipboardObserver;
    procedure Start(ACommand: TCommand);
    function SubmitPayload(const APayload: INyxCollectionClipboardText;
      const ACancellation: INyxCancellationScope;
      const AReadiness: INyxCancellationReadiness;
      AReply: TNyxClipboardWriteReply; const AReplyOwner: IInterface): INyxClipboardWriteRequest;
    function WriteCaptured(const APayload: INyxCollectionClipboardText;
      const ACancellation: INyxCancellationScope;
      const AReadiness: INyxCancellationReadiness;
      AReply: TNyxClipboardWriteReply; const AReplyOwner: IInterface): INyxClipboardWriteRequest;
    procedure Received(AReceipt: TReceipt; const AResult: TNyxClipboardWriteResult);
    procedure CancelCurrent;
    procedure RevokeInternal;
    function BeginCell(const AItem: TNyxItemRef;
      const AField: TNyxColumnIdentity): INyxCollectionClipboardCommand;
    function BeginCellInput: INyxCollectionClipboardCellCommand;
    function BeginRows(AMembership: TNyxCollectionCopyMembership): INyxCollectionClipboardCommand;
    procedure Observe(AObserver: TNyxCollectionClipboardObserver);
    procedure CancelPending;
    procedure Revoke;
    function GetPhase: TNyxCollectionClipboardPhase;
    function GetLastResult: TNyxClipboardWriteResult;
    function GetLastPayload: INyxCollectionClipboardText;
  public
    constructor Create(const AView: INyxCollectionView;
      const AInput: INyxCollectionInputLifetime; const AEvents: INyxEvents;
      const AHost: INyxClipboardWriteHost; const AScheduler: INyxScheduler;
      const AOptions: TNyxCollectionClipboardOptions;
      AReadiness: TNyxCollectionClipboardReadinessFactory);
    destructor Destroy; override;
  end;

constructor TCopyScope.Create(const AInput: INyxCollectionInputLifetime;
  AEpoch: Integer; const AParent: INyxCancellationReadiness);
begin
  inherited Create;
  FInput := AInput;
  FEpoch := AEpoch;
  FParent := AParent;
  FLocal := NewNyxCancellationScope;
end;

procedure TCopyScope.Cancel;
begin
  FLocal.Cancel;
end;

procedure TCopyScope.CheckReady;
begin
  FParent.CheckReady;

  if FLocal.Cancelled or (FEpoch <= 0) or (FEpoch >= High(Integer)) or
    not FInput.Ready or (FInput.Epoch <> FEpoch) then
  begin
    raise ENyxCollectionClipboard.Create('Original collection copy input is no longer ready');
  end;
end;

function TCopyScope.GetCancelled: Boolean;
begin
  Result := True;
  try
    CheckReady;
    Result := False;
  except
    { Certified final readiness reports refusal through exceptions. It cannot
      invoke application code; no arbitrary Cancelled getter runs on a worker. }
  end;
end;

procedure TReceipt.Receive(const AResult: TNyxClipboardWriteResult);
var
  LLease: IInterface;
  LOwnerLease: INyxCollectionClipboardCommands;
  LOwner: TCommands;
begin
  LLease := Self;
  Delivered := True;
  LOwner := Owner;

  if LOwner = nil then
  begin
    Exit;
  end;
  LOwnerLease := LOwner;
  LOwner.Received(Self, AResult);
end;

constructor TCopyTicket.Create(AOwner: TCommands; ASerial: Integer);
begin
  inherited Create;
  FOwner := AOwner;
  FLease := AOwner;
  FSerial := ASerial;
end;

procedure TCopyTicket.Cancel;
var
  LLease: INyxCollectionClipboardCommands;
begin

  if FCancelled then
  begin
    Exit;
  end;
  FCancelled := True;
  LLease := FLease;

  if not FOwner.FRevoked and (FOwner.FSerial = FSerial) then
  begin
    FOwner.CancelCurrent;
  end;
end;

constructor TCommand.Create(AOwner: TCommands; AAction: TNyxCollectionClipboardAction;
  const AItem: TNyxItemRef; const AField: TNyxColumnIdentity; AResolveLater: Boolean);
var
  LFactory: TNyxCollectionClipboardReadinessFactory;
begin
  inherited Create;
  FOwnerLease := AOwner;
  FOwner := AOwner;
  FView := AOwner.FView;
  FInput := AOwner.FInput;
  FEvents := AOwner.FEvents;
  FOptions := AOwner.FOptions;
  FAction := AAction;
  FSerial := AOwner.FSerial;

  if (FAction = nccaCell) and not AResolveLater then
  begin
    FItem := NyxItem(AItem.Collection, AItem.ID);
    FField := TNyxColumnIdentity.FromData(AField.ToData);
    FCellResolved := True;
  end;

  if (FInput = nil) or AOwner.FRevoked then
  begin
    Exit;
  end;
  FEpoch := FInput.Epoch;
  FEligible := FAction in FOptions.Actions;
  FEntryReady := FInput.Ready and
    (FEpoch > 0) and (FEpoch < High(Integer)) and
    (FSerial > 0) and (FSerial < High(Integer));
  LFactory := AOwner.FReadiness;

  if FEntryReady and FInput.Current and Assigned(LFactory) then
  begin
    FReadiness := LFactory();
  end;
end;

function TCommand.OriginalFlags(ACompleting: Boolean): Boolean;
begin
  Result := not FRetired and (ACompleting or not FCompleteStarted) and
    not FOwner.FRevoked and FEntryReady and (FSerial = FOwner.FSerial) and
    (FSerial > 0) and (FSerial < High(Integer)) and
    (FReadiness <> nil) and (FInput <> nil);

  if Result then
  begin
    try
      FReadiness.CheckReady;
    except
      Exit(False);
    end;
    { Canonical epoch+Ready are LAST after the captured flags-only mount fence. }
    Result := FInput.Ready and (FInput.Epoch = FEpoch);
  end;
end;

function TCommand.GetDispatchCurrent: Boolean;
begin
  Result := not FRetired and not FCompleteStarted and not FOwner.FRevoked and
    (FInput <> nil) and FInput.Current;
end;

function TCommand.CurrentProof(ACompleting: Boolean): Boolean;
begin
  Result := False;

  if FChecking then
  begin
    Exit;
  end;
  FChecking := True;
  try
    Result := CheckProof(ACompleting);
  finally
    FChecking := False;
  end;
end;

function TCommand.CheckProof(ACompleting: Boolean): Boolean;
var
  LStore: INyxCollection;
  LSource: INyxCollectionSnapshot;
  LPage: INyxCollectionSnapshot;
  LSelection: INyxCollectionSelection;
  LCapture: INyxCollectionColumnsCapture;
  LBaseline: TNyxText;
  LCancelled: Boolean;
begin
  Result := False;

  if not OriginalFlags(ACompleting) then
  begin
    Exit;
  end;

  if not FPrepared then
  begin
    Exit(True);
  end;

  if FPayload = nil then
  begin
    Exit;
  end;
  LStore := FView.Store;

  if not OriginalFlags(ACompleting) or (LStore <> FStore) then
  begin
    Exit;
  end;
  LSource := LStore.Snapshot;

  if not OriginalFlags(ACompleting) or (LSource <> FSource) then
  begin
    Exit;
  end;
  LPage := FView.Snapshot;

  if not OriginalFlags(ACompleting) or (LPage <> FPage) then
  begin
    Exit;
  end;

  if FAction <> nccaCell then
  begin
    LSelection := FView.Selection;

    if not OriginalFlags(ACompleting) or (LSelection = nil) then
    begin
      Exit;
    end;

    if not FSelection.SameState(LSelection.Snapshot) or not OriginalFlags(ACompleting) then
    begin
      Exit;
    end;
  end;
  LCapture := FColumns.CaptureColumns;

  if not OriginalFlags(ACompleting) or (LCapture = nil) then
  begin
    Exit;
  end;

  if (LCapture.Serial <> FColumnSerial) or not OriginalFlags(ACompleting) then
  begin
    Exit;
  end;
  LBaseline := LCapture.Spec.ToData.ToJSON + #10 + LCapture.Preferences.ToData.ToJSON;

  if not OriginalFlags(ACompleting) or (LBaseline <> FColumnBaseline) then
  begin
    Exit;
  end;

  if FEvents.ViewRevision <> FRevision then
  begin
    Exit;
  end;

  if not OriginalFlags(ACompleting) then
  begin
    Exit;
  end;
  LCancelled := FParent.Cancelled;

  if LCancelled or not OriginalFlags(ACompleting) then
  begin
    Exit;
  end;
  try
    FScopeReady.CheckReady;
  except
    Exit;
  end;
  Result := OriginalFlags(ACompleting);
end;

function TCommand.GetCurrent: Boolean;
begin
  Result := CurrentProof(False);
end;

function TCommand.GetPayload: INyxCollectionClipboardText;
begin
  Result := FPayload;
end;

function TCommand.GetReadiness: INyxCancellationReadiness;
begin
  Result := nil;

  if FPrepared and (FPayload <> nil) then
  begin
    Result := FScopeReady;
  end;
end;

procedure TCommand.Prepare;
var
  LCapture: INyxCollectionColumnsCapture;
  LObservation: TNyxColumnLayoutObservation;
  LFields: TNyxCollectionClipboardFields;
  LSelection: INyxCollectionSelection;
  LScope: TCopyScope;
  LParentReady: INyxCancellationReadiness;
begin

  if FPrepared or FCompleteStarted or FRetired then
  begin
    raise ENyxCollectionClipboard.Create('Collection copy preparation is single-use');
  end;
  FPrepared := True;

  if not FEligible or ((FAction = nccaCell) and not FCellResolved) then
  begin
    Exit;
  end;

  if not OriginalFlags or not NyxCollectionViewPublicationReady(FView) or
    not OriginalFlags then
  begin
    Exit;
  end;
  FStore := FView.Store;

  if not OriginalFlags or (FStore = nil) then
  begin
    Exit;
  end;
  FSource := FStore.Snapshot;

  if not OriginalFlags or (FSource = nil) then
  begin
    Exit;
  end;
  FPage := FView.Snapshot;

  if not OriginalFlags or (FPage = nil) then
  begin
    Exit;
  end;

  if FAction <> nccaCell then
  begin
    LSelection := FView.Selection;

    if not OriginalFlags or (LSelection = nil) then
    begin
      Exit;
    end;
    FSelection := LSelection.Snapshot;

    if not OriginalFlags then
    begin
      Exit;
    end;
  end;
  FColumns := NyxCollectionColumns(FView);

  if not OriginalFlags or (FColumns = nil) then
  begin
    Exit;
  end;
  LCapture := FColumns.CaptureColumns;

  if not OriginalFlags or (LCapture = nil) then
  begin
    Exit;
  end;
  FColumnSerial := LCapture.Serial;
  FColumnBaseline := LCapture.Spec.ToData.ToJSON + #10 + LCapture.Preferences.ToData.ToJSON;

  if not OriginalFlags then
  begin
    Exit;
  end;
  FParent := NyxEventGeneration(FEvents);

  if not OriginalFlags or (FParent = nil) or
    not Supports(FParent, INyxCancellationReadiness, LParentReady) then
  begin
    Exit;
  end;
  { Explicit field assignment preserves the managed lease on both compilers;
    an interface field is never used as a temporary QI out parameter. }
  FParentReady := LParentReady;

  if not OriginalFlags then
  begin
    Exit;
  end;
  FRevision := FEvents.ViewRevision;

  if not OriginalFlags then
  begin
    Exit;
  end;
  LScope := TCopyScope.Create(FInput, FEpoch, FParentReady);
  FScope := LScope;
  FScopeReady := LScope;

  if FAction = nccaCell then
  begin
    FPayload := NewNyxCollectionClipboardCell(FSource, FItem, FField, FOptions.WorkLimits);
  end
  else
  begin
    LObservation := TNyxColumnLayoutObservation.Read(LCapture);
    LFields := NyxCollectionClipboardFields(LObservation, FOptions.FieldChoice);

    if FAction = nccaSelectedSource then
    begin
      FPayload := NewNyxCollectionClipboardRows(FSource, FSelection, LFields,
        nccmAllSelectedSource, nil, FOptions.IncludeHeaders, FOptions.Lines, FOptions.WorkLimits);
    end
    else
    begin
      FPayload := NewNyxCollectionClipboardRows(FSource, FSelection, LFields,
        nccmSelectedPage, FPage, FOptions.IncludeHeaders, FOptions.Lines, FOptions.WorkLimits);
    end;
  end;

  if not CurrentProof(False) then
  begin
    FPayload := nil;
  end;
end;

procedure TCommand.ResolveCell(const AItem: TNyxItemRef;
  const AField: TNyxColumnIdentity);
begin

  if (FAction <> nccaCell) or FCellResolved or FPrepared or FCompleteStarted or FRetired then
  begin
    raise ENyxCollectionClipboard.Create('Physical cell resolution is single-use before preparation');
  end;
  FCellResolved := True;

  if not OriginalFlags then
  begin
    Exit;
  end;
  FItem := NyxItem(AItem.Collection, AItem.ID);
  FField := TNyxColumnIdentity.FromData(AField.ToData);
end;

function TCommand.Complete(AAllowDefault: Boolean): Boolean;
begin
  Result := True;

  if FCompleteStarted or FRetired then
  begin
    Exit;
  end;

  if AAllowDefault and CurrentProof(False) and FPrepared then
  begin
    FCompleteStarted := True;
    FOwner.Start(Self);
  end
  else
  begin
    FCompleteStarted := True;
  end;
end;

procedure TCommand.Retire;
begin
  FRetired := True;
end;

constructor TCommands.Create(const AView: INyxCollectionView;
  const AInput: INyxCollectionInputLifetime; const AEvents: INyxEvents;
  const AHost: INyxClipboardWriteHost; const AScheduler: INyxScheduler;
  const AOptions: TNyxCollectionClipboardOptions;
  AReadiness: TNyxCollectionClipboardReadinessFactory);
var
  LView: INyxCollectionView;
  LInput: INyxCollectionInputLifetime;
  LEvents: INyxEvents;
  LHost: INyxClipboardWriteHost;
  LScheduler: INyxScheduler;
  LOptions: TNyxCollectionClipboardOptions;
begin
  inherited Create;
  LView := AView;
  LInput := AInput;
  LEvents := AEvents;
  LHost := AHost;
  LScheduler := AScheduler;
  LOptions := AOptions;
  LOptions.Validate;

  if (LView = nil) or (LInput = nil) or (LEvents = nil) or
    (LHost = nil) or (LScheduler = nil) or not Assigned(AReadiness) then
  begin
    raise ENyxCollectionClipboard.Create('Collection copy requires original view/input/events/host/scheduler/fence');
  end;
  LScheduler.RequireUI;

  if (LView.Projection <> cpTable) or LInput.Retired or not LInput.Current then
  begin
    raise ENyxCollectionClipboard.Create('Collection copy configuration requires a live table incarnation');
  end;
  FView := LView;
  FInput := LInput;
  FEvents := LEvents;
  FHost := LHost;
  FScheduler := LScheduler;
  FOptions := LOptions;
  FReadiness := AReadiness;
  FSerial := 1;
end;

destructor TCommands.Destroy;
begin
  try
    RevokeInternal;
  except
    { Explicit Revoke is the fallible boundary; final release cannot escape. }
  end;
  inherited Destroy;
end;

function TCommands.BeginCell(const AItem: TNyxItemRef;
  const AField: TNyxColumnIdentity): INyxCollectionClipboardCommand;
var
  LLease: INyxCollectionClipboardCommands;
begin
  LLease := Self;
  Result := TCommand.Create(Self, nccaCell, AItem, AField);
end;

function TCommands.BeginCellInput: INyxCollectionClipboardCellCommand;
var
  LLease: INyxCollectionClipboardCommands;
begin
  LLease := Self;
  Result := TCommand.Create(Self, nccaCell, Default(TNyxItemRef),
    Default(TNyxColumnIdentity), True);
end;

function TCommands.BeginRows(AMembership: TNyxCollectionCopyMembership): INyxCollectionClipboardCommand;
var
  LLease: INyxCollectionClipboardCommands;
  LAction: TNyxCollectionClipboardAction;
begin
  LLease := Self;

  if (Ord(AMembership) < Ord(Low(TNyxCollectionCopyMembership))) or
    (Ord(AMembership) > Ord(High(TNyxCollectionCopyMembership))) then
  begin
    raise ENyxCollectionClipboard.Create('Unknown collection copy membership');
  end;
  case AMembership of
    nccmAllSelectedSource: LAction := nccaSelectedSource;
    nccmSelectedPage: LAction := nccaSelectedPage;
  end;
  Result := TCommand.Create(Self, LAction, Default(TNyxItemRef), Default(TNyxColumnIdentity));
end;

procedure TCommands.Start(ACommand: TCommand);
var
  LLease: INyxCollectionClipboardCommands;
begin
  LLease := Self;

  if FRevoked or (FSerial >= High(Integer) - 1) then
  begin
    FSerial := High(Integer);
    CancelCurrent;
    Exit;
  end;
  Inc(FSerial);
  ACommand.FSerial := FSerial;
  { Withdraw old authority before Cancel can reenter. A later accepted action
    wins even when its payload bytes are equal. }
  CancelCurrent;

  if ACommand.CurrentProof(True) then
  begin
    SubmitPayload(ACommand.FPayload, ACommand.FScope, ACommand.FScopeReady, nil, nil);
  end;
end;

function TCommands.WriteCaptured(const APayload: INyxCollectionClipboardText;
  const ACancellation: INyxCancellationScope;
  const AReadiness: INyxCancellationReadiness;
  AReply: TNyxClipboardWriteReply; const AReplyOwner: IInterface): INyxClipboardWriteRequest;
var
  LLease: INyxCollectionClipboardCommands;
  LSerial: Integer;
begin
  Result := nil;
  LLease := Self;

  if FRevoked or not (nccaCell in FOptions.Actions) or (APayload = nil) or
    (ACancellation = nil) or (AReadiness = nil) then
  begin
    Exit;
  end;
  AReadiness.CheckReady;

  if FRevoked or (FSerial >= High(Integer) - 1) then
  begin
    FSerial := High(Integer);
    CancelCurrent;
    Exit;
  end;
  Inc(FSerial);
  LSerial := FSerial;
  CancelCurrent;

  if FRevoked or (FSerial <> LSerial) then
  begin
    Exit;
  end;
  AReadiness.CheckReady;

  if FRevoked or (FSerial <> LSerial) or not FInput.Ready then
  begin
    Exit;
  end;
  Result := SubmitPayload(APayload, ACancellation, AReadiness, AReply, AReplyOwner);
end;

function TCommands.SubmitPayload(const APayload: INyxCollectionClipboardText;
  const ACancellation: INyxCancellationScope;
  const AReadiness: INyxCancellationReadiness;
  AReply: TNyxClipboardWriteReply; const AReplyOwner: IInterface): INyxClipboardWriteRequest;
var
  LReceipt: TReceipt;
  LReceiptLease: IInterface;
  LRequest: INyxClipboardWriteRequest;
  LHost: INyxClipboardWriteHost;
  LScheduler: INyxScheduler;
begin
  LHost := FHost;
  LScheduler := FScheduler;
  Result := TCopyTicket.Create(Self, FSerial);
  LReceipt := TReceipt.Create;
  LReceiptLease := LReceipt;
  LReceipt.Owner := Self;
  LReceipt.Serial := FSerial;
  LReceipt.Payload := APayload;
  LReceipt.Scope := AReadiness;
  LReceipt.Reply := AReply;
  LReceipt.ReplyOwner := AReplyOwner;
  FReceipt := LReceipt;
  FReceiptLease := LReceiptLease;
  FPayload := APayload;
  FPhase := nccpPending;
  try
    LRequest := WriteNyxCollectionClipboard(APayload, LHost, LScheduler,
      ACancellation, {$ifdef PAS2JS}@{$endif}LReceipt.Receive);
  except
    { An entered inline client failure is already terminal. It is never
      replayed or reclassified as a second host-admission result. }

    if FReceipt = LReceipt then
    begin
      LReceipt.Owner := nil;
      FReceipt := nil;
      FReceiptLease := nil;
      FRequest := nil;
      FPhase := nccpCancelled;
    end;
    raise;
  end;

  if not LReceipt.Delivered then
  begin

    if not FRevoked and (FReceipt = LReceipt) and (FSerial = LReceipt.Serial) then
    begin
      FRequest := LRequest;
    end
    else if LRequest <> nil then
    begin
      LRequest.Cancel;
    end;
  end;
end;
procedure TCommands.Received(AReceipt: TReceipt; const AResult: TNyxClipboardWriteResult);
var
  LLease: INyxCollectionClipboardCommands;
  LRequest: INyxClipboardWriteRequest;
  LReceiptLease: IInterface;
  LObserver: TNyxCollectionClipboardObserver;
  LPayload: INyxCollectionClipboardText;
  LResult: TNyxClipboardWriteResult;
  LReady: Boolean;
  LReply: TNyxClipboardWriteReply;
  LReplyOwner: IInterface;
begin
  LLease := Self;

  if FRevoked or (FReceipt <> AReceipt) or (FSerial <> AReceipt.Serial) then
  begin
    Exit;
  end;
  LResult := AResult;
  LReply := AReceipt.Reply;
  LReplyOwner := AReceipt.ReplyOwner;
  LPayload := AReceipt.Payload;
  LReady := False;
  try
    AReceipt.Scope.CheckReady;
    LReady := True;
  except
    { Only certified original flags are consulted after host completion. }
  end;
  LRequest := FRequest;
  LReceiptLease := FReceiptLease;
  AReceipt.Owner := nil;
  FReceipt := nil;
  FReceiptLease := nil;
  FRequest := nil;
  FPhase := nccpCancelled;
  { Alternate hosts share the same terminal receipt contract as built-ins.
    Detach first: an invalid receipt is terminal even when validation raises. }
  ValidateNyxClipboardWriteResult(LResult, LPayload.ByteCount);

  if not LReady then
  begin
    Exit;
  end;
  FResult := LResult;
  FPayload := LPayload;
  FPhase := nccpCompleted;
  LObserver := FObserver;

  if Assigned(LObserver) then
  begin
    LObserver(LPayload, LResult);
  end;

  if not FRevoked and (FSerial = AReceipt.Serial) and Assigned(LReply) then
  begin
    try
      AReceipt.Scope.CheckReady;
    except
      Exit;
    end;
    LReply(LResult);
  end;
end;

procedure TCommands.CancelCurrent;
var
  LRequest: INyxClipboardWriteRequest;
  LReceiptLease: IInterface;
begin
  LRequest := FRequest;
  LReceiptLease := FReceiptLease;

  if FReceipt <> nil then
  begin
    FReceipt.Owner := nil;
  end;
  FRequest := nil;
  FReceipt := nil;
  FReceiptLease := nil;

  if FPhase = nccpPending then
  begin
    FPhase := nccpCancelled;
  end;

  if LRequest <> nil then
  begin
    LRequest.Cancel;
  end;
end;

procedure TCommands.CancelPending;
var
  LLease: INyxCollectionClipboardCommands;
begin

  if FRevoked then
  begin
    Exit;
  end;
  LLease := Self;
  CancelCurrent;
end;

procedure TCommands.RevokeInternal;
var
  LView: INyxCollectionView;
  LInput: INyxCollectionInputLifetime;
  LEvents: INyxEvents;
  LHost: INyxClipboardWriteHost;
  LScheduler: INyxScheduler;
begin

  if FRevoked then
  begin
    Exit;
  end;
  FRevoked := True;
  FReadiness := nil;
  FObserver := nil;
  LView := FView;
  LInput := FInput;
  LEvents := FEvents;
  LHost := FHost;
  LScheduler := FScheduler;
  FView := nil;
  FInput := nil;
  FEvents := nil;
  FHost := nil;
  FScheduler := nil;
  CancelCurrent;
end;

procedure TCommands.Revoke;
var
  LLease: INyxCollectionClipboardCommands;
begin

  if FRevoked then
  begin
    Exit;
  end;
  LLease := Self;
  RevokeInternal;
end;

procedure TCommands.Observe(AObserver: TNyxCollectionClipboardObserver);
begin

  if FRevoked and Assigned(AObserver) then
  begin
    raise ENyxCollectionClipboard.Create('Retired collection copy receiver cannot be replaced');
  end;
  FObserver := AObserver;
end;

function TCommands.GetPhase: TNyxCollectionClipboardPhase;
begin
  Result := FPhase;
end;

function TCommands.GetLastResult: TNyxClipboardWriteResult;
begin

  if FPhase <> nccpCompleted then
  begin
    raise ENyxCollectionClipboard.Create('Collection copy has no completed host result');
  end;
  Result := FResult;
end;

function TCommands.GetLastPayload: INyxCollectionClipboardText;
begin
  Result := FPayload;
end;

function NewNyxCollectionClipboardCommands(const AView: INyxCollectionView;
  const AInput: INyxCollectionInputLifetime; const AEvents: INyxEvents;
  const AHost: INyxClipboardWriteHost; const AScheduler: INyxScheduler;
  const AOptions: TNyxCollectionClipboardOptions;
  AReadiness: TNyxCollectionClipboardReadinessFactory): INyxCollectionClipboardCommands;
begin
  Result := TCommands.Create(AView, AInput, AEvents, AHost, AScheduler, AOptions, AReadiness);
end;

end.
