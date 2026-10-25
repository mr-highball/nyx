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

uses nyx.collections.view, nyx.collections.inspection, nyx.collections.mount,
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
  nyx.collections.clipboard, nyx.collections.view.types, nyx.collections.export.actions,
  nyx.collections.representation, nyx.contract, nyx.state,
  nyx.collections.range, nyx.collections.range.actions, nyx.collections.range.commands,
  nyx.collections.identities;

type
  { Forward controller is borrowed only under an entered managed owner lease. }
  TCommands = class;

  { Forward command never belongs to the controller's pending-host slot. }
  TCommand = class;

  { Forward seal is private implementation data, never an author-supplied
    action/payload tuple or a backlink to its originating controller. }
  TCapturedAction = class;

  { Forward identity seal remains a private value object with no owner backlink. }
  TIdentityAction = class;

  { Only the built-in immutable identity seal exposes this borrowed data under
    an owning QI lease; copied public values cannot grant controller authority. }
  IIdentityActionProof = interface(IInterface)
    ['{791EA2E2-C920-41F9-B0D4-B891B48DCF87}']
    function IdentityData: TIdentityAction;
  end;

  { Original-controller witness seals one exact source/page ID representation.
    Identity is inert, and readiness is certified flags-only, without live views. }
  TIdentityAction = class(TInterfacedObject, INyxCollectionClipboardIdentityAction,
    IIdentityActionProof)
  public
    Identity: IInterface;
    Value: INyxCollectionIdentityText;
    Ready: INyxCancellationReadiness;
    constructor Create(const AIdentity: IInterface; const AValue: INyxCollectionIdentityText;
      const AReady: INyxCancellationReadiness);
    function GetValue: INyxCollectionIdentityText;
    function GetReadiness: INyxCancellationReadiness;
    function IdentityData: TIdentityAction;
  end;

  { Only actual built-in seals expose this private value witness. The owning
    QI local retains its borrowed class data through admission/reentry checks. }
  ICapturedActionProof = interface(IInterface)
    ['{CB869C02-A396-4DD9-9DFD-4DDF737F1783}']
    function CapturedData: TCapturedAction;
  end;

  { Immutable authorized values carry one independent controller identity and
    inert readiness. No controller/view/mount is retained by a sealed action. }
  TCapturedAction = class(TInterfacedObject, INyxCollectionClipboardActionCapture,
    ICapturedActionProof)
  public
    Identity: IInterface;
    Action: TNyxCollectionClipboardAction;
    Payload: INyxCollectionClipboardText;
    Readiness: INyxCancellationReadiness;
    constructor Create(const AIdentity: IInterface; AAction: TNyxCollectionClipboardAction;
      const APayload: INyxCollectionClipboardText; const AReady: INyxCancellationReadiness);
    function GetAction: TNyxCollectionClipboardAction;
    function GetPayload: INyxCollectionClipboardText;
    function GetReadiness: INyxCancellationReadiness;
    function CapturedData: TCapturedAction;
  end;

  { Owner-free cancellation passed to asynchronous clipboard hosts. It retains
    only certified flags-only parent readiness, canonical input and its own
    synchronized cancel flag. The mounted final fence is deliberately absent. }
  TCopyScope = class(TInterfacedObject, INyxCancellationScope, INyxCancellationReadiness)
  private
    FInput: INyxCollectionInputLifetime;
    FEpoch: Integer;
    FParent: INyxCancellationReadiness;
    FAdditional: INyxCancellationReadiness;
    FLocal: INyxCancellationScope;
  public
    constructor Create(const AInput: INyxCollectionInputLifetime; AEpoch: Integer;
      const AParent: INyxCancellationReadiness;
      const AAdditional: INyxCancellationReadiness = nil);
    procedure Cancel;
    function GetCancelled: Boolean;
    procedure CheckReady;
  end;

  { One host invocation's weak receiver. Controller detaches Owner before
    cancellation; local managed leases protect an entered inline callback.
    Exactly one RAW-column, cell-representation or identity-only value supplies
    its distinct receipt; none retains the mounted synchronous final fence. }
  TReceipt = class(TInterfacedObject)
  public
    Owner: TCommands;
    Serial: Integer;
    Delivered: Boolean;
    Payload: INyxCollectionClipboardText;
    Value: INyxCollectionCellText;
    Identities: INyxCollectionIdentityText;
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

  { Prepared input token owns exact source/page/row-or-cell-selection/column
    proof and the synchronous final fence. It retains its controller, never the
    reverse. Optional context-range authority is separate from RAW cell proof;
    clearing the range cannot cancel accepted-cell inspection or copy. No range
    controller enters an asynchronous host. Retire after transfer cannot cancel
    that request. Readonly inspection can capture without cell-copy permission;
    the same token still cannot complete a disallowed write. }
  TCommand = class(TInterfacedObject, INyxCollectionClipboardCommand,
    INyxCollectionClipboardCellCommand, INyxCollectionClipboardPreparedScope,
    INyxCollectionClipboardOutputCapture, INyxCollectionClipboardDisplayCapture,
    INyxCollectionClipboardRangeCommand, INyxCollectionClipboardCellRangeContext,
    INyxCollectionClipboardCellPermission, INyxCollectionClipboardCellRowsContext,
    INyxCollectionClipboardContextTransfers, INyxCollectionClipboardIdentityTransfers)
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
    FInspection: Boolean;
    FRowsCapture: INyxCollectionSelectedRowsCapture;
    FSourceRowsPayload: INyxCollectionClipboardText;
    FPageRowsPayload: INyxCollectionClipboardText;
    FRowsReady: INyxCancellationReadiness;
    FSourceIdentityAction: INyxCollectionClipboardIdentityAction;
    FPageIdentityAction: INyxCollectionClipboardIdentityAction;
    FRangeAction: INyxCollectionClipboardActionCapture;
    FSourceRowsAction: INyxCollectionClipboardActionCapture;
    FPageRowsAction: INyxCollectionClipboardActionCapture;
    FCellResolved: Boolean;
    FRangeStarted: Boolean;
    FRangeResolved: Boolean;
    FRangeSource: INyxCollectionSnapshot;
    FRangePage: INyxCollectionSnapshot;
    FRangeAnchor: TNyxCollectionCellIdentity;
    FRangeFocus: TNyxCollectionCellIdentity;
    FRangeSerial: Integer;
    FRangeReady: INyxCancellationReadiness;
    FContextRangeStarted: Boolean;
    FContextRangePayload: INyxCollectionClipboardText;
    FContextRangeReady: INyxCancellationReadiness;
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
    FOutput: INyxCollectionExportCapture;
    FDisplayPolicy: TNyxCollectionCellDisplayPolicy;
    function OriginalFlags(ACompleting: Boolean = False): Boolean;
    function CurrentProof(ACompleting: Boolean): Boolean;
    function CheckProof(ACompleting: Boolean): Boolean;
    function GetCurrent: Boolean;
    function GetDispatchCurrent: Boolean;
    function GetPayload: INyxCollectionClipboardText;
    function GetReadiness: INyxCancellationReadiness;
    procedure PrepareOutput;
    function GetOutput: INyxCollectionExportCapture;
    function GetDisplayPolicy: TNyxCollectionCellDisplayPolicy;
    function GetCellCopyAllowed: Boolean;
    function GetSourceRowsPayload: INyxCollectionClipboardText;
    function GetPageRowsPayload: INyxCollectionClipboardText;
    function GetRowsReadiness: INyxCancellationReadiness;
    procedure PrepareContextRows;
    function GetRangeAction: INyxCollectionClipboardActionCapture;
    function GetSourceRowsAction: INyxCollectionClipboardActionCapture;
    function GetPageRowsAction: INyxCollectionClipboardActionCapture;
    function GetSourceIdentityAction: INyxCollectionClipboardIdentityAction;
    function GetPageIdentityAction: INyxCollectionClipboardIdentityAction;
    procedure Prepare;
    procedure ResolveCell(const AItem: TNyxItemRef;
      const AField: TNyxColumnIdentity);
    procedure ResolveRange(const AIntent: INyxCollectionCellRangeIntent);
    procedure CaptureRange(const AIntent: INyxCollectionCellRangeIntent);
    procedure AttachRange(const AIntent: INyxCollectionCellRangeIntent);
    function GetRangePayload: INyxCollectionClipboardText;
    function GetRangeReadiness: INyxCancellationReadiness;
    procedure PrepareContextRange(const AColumns: INyxCollectionColumnsCapture);
    function Complete(AAllowDefault: Boolean): Boolean;
    procedure Retire;
  public
    constructor Create(AOwner: TCommands; AAction: TNyxCollectionClipboardAction;
      const AItem: TNyxItemRef; const AField: TNyxColumnIdentity;
      AResolveLater: Boolean = False; AInspection: Boolean = False);
  end;

  { Runtime controller keeps one exact pending receipt/request and immutable
    authority for RAW cell, explicit cell rectangle, row, identity and represented writes.
    A receipt borrows this owner; it is revoked before host Cancel. No member
    retains a command, live range selection or mount-retaining synchronous fence.
    Optional readonly context entry shares original command arbitration without
    changing physical-copy eligibility. }
  TCommands = class(TInterfacedObject, INyxCollectionClipboardCommands,
    INyxCollectionClipboardCellInput, INyxCollectionClipboardCapturedWrite,
    INyxCollectionClipboardRepresentationWrite, INyxCollectionClipboardRangeInput,
    INyxCollectionClipboardInspectionInput, INyxCollectionClipboardCapturedActionWrite,
    INyxCollectionClipboardIdentityWrite)
  private
    FView: INyxCollectionView;
    FRowsView: INyxCollectionSelectedRowsView;
    FTransferIdentity: IInterface;
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
    FRepresentationPhase: TNyxCollectionClipboardPhase;
    FRepresentationResult: TNyxClipboardWriteResult;
    FRepresentation: INyxCollectionCellText;
    FRepresentationObserver: TNyxCollectionRepresentationObserver;
    FIdentityPhase: TNyxCollectionClipboardPhase;
    FIdentityResult: TNyxClipboardWriteResult;
    FIdentities: INyxCollectionIdentityText;
    FIdentityObserver: TNyxCollectionIdentityObserver;
    procedure Start(ACommand: TCommand);
    function SubmitPayload(const APayload: INyxCollectionClipboardText;
      const ACancellation: INyxCancellationScope;
      const AReadiness: INyxCancellationReadiness;
      AReply: TNyxClipboardWriteReply; const AReplyOwner: IInterface;
      const AValue: INyxCollectionCellText = nil;
      const AIdentities: INyxCollectionIdentityText = nil): INyxClipboardWriteRequest;
    function WriteIdentities(const AAction: INyxCollectionClipboardIdentityAction;
      const ACancellation: INyxCancellationScope; const AReadiness: INyxCancellationReadiness;
      AReply: TNyxClipboardWriteReply; const AReplyOwner: IInterface): INyxClipboardWriteRequest;
    procedure ObserveIdentities(AObserver: TNyxCollectionIdentityObserver);
    function GetIdentityPhase: TNyxCollectionClipboardPhase;
    function GetLastIdentityResult: TNyxClipboardWriteResult;
    function GetLastIdentities: INyxCollectionIdentityText;
    function WriteCaptured(const APayload: INyxCollectionClipboardText;
      const ACancellation: INyxCancellationScope;
      const AReadiness: INyxCancellationReadiness;
      AReply: TNyxClipboardWriteReply; const AReplyOwner: IInterface): INyxClipboardWriteRequest;
    function WriteCapturedAction(const AAction: INyxCollectionClipboardActionCapture;
      const ACancellation: INyxCancellationScope;
      const AReadiness: INyxCancellationReadiness;
      AReply: TNyxClipboardWriteReply; const AReplyOwner: IInterface): INyxClipboardWriteRequest;
    function SubmitCapturedAction(AAction: TNyxCollectionClipboardAction;
      const APayload: INyxCollectionClipboardText;
      const ACancellation: INyxCancellationScope;
      const AReadiness: INyxCancellationReadiness;
      AReply: TNyxClipboardWriteReply; const AReplyOwner: IInterface;
      const AIdentities: INyxCollectionIdentityText = nil): INyxClipboardWriteRequest;
    procedure Received(AReceipt: TReceipt; const AResult: TNyxClipboardWriteResult);
    procedure CancelCurrent;
    procedure RevokeInternal;
    function BeginCell(const AItem: TNyxItemRef;
      const AField: TNyxColumnIdentity): INyxCollectionClipboardCommand;
    function BeginCellInput: INyxCollectionClipboardCellCommand;
    function BeginCellInspection: INyxCollectionClipboardCellCommand;
    function BeginRangeInput: INyxCollectionClipboardRangeCommand;
    function BeginRows(AMembership: TNyxCollectionCopyMembership): INyxCollectionClipboardCommand;
    procedure Observe(AObserver: TNyxCollectionClipboardObserver);
    procedure CancelPending;
    procedure Revoke;
    function GetPhase: TNyxCollectionClipboardPhase;
    function GetLastResult: TNyxClipboardWriteResult;
    function GetLastPayload: INyxCollectionClipboardText;
    function WriteRepresented(const AValue: INyxCollectionCellText;
      const ACancellation: INyxCancellationScope;
      const AReadiness: INyxCancellationReadiness;
      AReply: TNyxClipboardWriteReply; const AReplyOwner: IInterface): INyxClipboardWriteRequest;
    procedure ObserveRepresentation(AObserver: TNyxCollectionRepresentationObserver);
    function GetRepresentationPhase: TNyxCollectionClipboardPhase;
    function GetLastRepresentationResult: TNyxClipboardWriteResult;
    function GetLastRepresentation: INyxCollectionCellText;
  public
    constructor Create(const AView: INyxCollectionView;
      const AInput: INyxCollectionInputLifetime; const AEvents: INyxEvents;
      const AHost: INyxClipboardWriteHost; const AScheduler: INyxScheduler;
      const AOptions: TNyxCollectionClipboardOptions;
      AReadiness: TNyxCollectionClipboardReadinessFactory);
    destructor Destroy; override;
  end;

constructor TIdentityAction.Create(const AIdentity: IInterface;
  const AValue: INyxCollectionIdentityText; const AReady: INyxCancellationReadiness);
begin
  inherited Create;
  Identity := AIdentity;
  Value := AValue;
  Ready := AReady;
end;

function TIdentityAction.GetValue: INyxCollectionIdentityText;
begin
  Result := Value;
end;

function TIdentityAction.GetReadiness: INyxCancellationReadiness;
begin
  Result := Ready;
end;

function TIdentityAction.IdentityData: TIdentityAction;
begin
  Result := Self;
end;

constructor TCapturedAction.Create(const AIdentity: IInterface;
  AAction: TNyxCollectionClipboardAction; const APayload: INyxCollectionClipboardText;
  const AReady: INyxCancellationReadiness);
begin
  inherited Create;
  Identity := AIdentity;
  Action := AAction;
  Payload := APayload;
  Readiness := AReady;
end;

function TCapturedAction.GetAction: TNyxCollectionClipboardAction;
begin
  Result := Action;
end;

function TCapturedAction.GetPayload: INyxCollectionClipboardText;
begin
  Result := Payload;
end;

function TCapturedAction.GetReadiness: INyxCancellationReadiness;
begin
  Result := Readiness;
end;

function TCapturedAction.CapturedData: TCapturedAction;
begin
  Result := Self;
end;

constructor TCopyScope.Create(const AInput: INyxCollectionInputLifetime;
  AEpoch: Integer; const AParent: INyxCancellationReadiness;
  const AAdditional: INyxCancellationReadiness);
begin
  inherited Create;
  FInput := AInput;
  FEpoch := AEpoch;
  FParent := AParent;
  FAdditional := AAdditional;
  FLocal := NewNyxCancellationScope;
end;

procedure TCopyScope.Cancel;
begin
  FLocal.Cancel;
end;

procedure TCopyScope.CheckReady;
begin
  FParent.CheckReady;

  if FAdditional <> nil then
  begin
    FAdditional.CheckReady;
  end;

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
  const AItem: TNyxItemRef; const AField: TNyxColumnIdentity; AResolveLater: Boolean;
  AInspection: Boolean);
var
  LFactory: TNyxCollectionClipboardReadinessFactory;
  LRowsView: INyxCollectionSelectedRowsView;
begin
  inherited Create;
  FOwnerLease := AOwner;
  FOwner := AOwner;
  FView := AOwner.FView;
  FInput := AOwner.FInput;
  FEvents := AOwner.FEvents;
  FOptions := AOwner.FOptions;
  FAction := AAction;
  FInspection := AInspection and (FAction = nccaCell);
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
  { The optional port was retained during configuration. Its contract captures
    only owned flags/snapshots, BEFORE the outward mount-input factory below. }

  if FInspection and FEntryReady and FInput.Current and
    ((nccaSelectedSource in FOptions.Actions) or (nccaSelectedPage in FOptions.Actions)) then
  begin
    LRowsView := AOwner.FRowsView;

    if LRowsView <> nil then
    begin
      FRowsCapture := LRowsView.CaptureSelectedRows;
    end;
  end;
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

      if (FAction = nccaCellRange) and FRangeResolved and FPrepared then
      begin

        if FRangeReady = nil then
        begin
          Exit(False);
        end;
        FRangeReady.CheckReady;
      end;
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
  LInspection: INyxCollectionInspection;
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
  LInspection := NyxCollectionInspection(FView);

  if not OriginalFlags(ACompleting) then
  begin
    Exit;
  end;

  if LInspection <> nil then
  begin

    if LInspection.CaptureInspection.Mode <> ncimPaused then
    begin
      LInspection := nil;
    end;
  end;

  if not OriginalFlags(ACompleting) then
  begin
    Exit;
  end;

  if LInspection <> nil then
  begin
    LSource := LInspection.GetDisplayedSource;
  end
  else
  begin
    LSource := LStore.Snapshot;
  end;

  if not OriginalFlags(ACompleting) or (LSource <> FSource) then
  begin
    Exit;
  end;
  LPage := FView.Snapshot;

  if not OriginalFlags(ACompleting) or (LPage <> FPage) then
  begin
    Exit;
  end;

  if FAction in [nccaSelectedSource, nccaSelectedPage] then
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

function TCommand.GetDisplayPolicy: TNyxCollectionCellDisplayPolicy;
begin
  Result := FDisplayPolicy.Copy;
end;

function TCommand.GetCellCopyAllowed: Boolean;
begin
  Result := (FAction = nccaCell) and FEligible;
end;

function TCommand.GetSourceRowsPayload: INyxCollectionClipboardText;
begin
  Result := FSourceRowsPayload;
end;

function TCommand.GetPageRowsPayload: INyxCollectionClipboardText;
begin
  Result := FPageRowsPayload;
end;

function TCommand.GetRowsReadiness: INyxCancellationReadiness;
begin
  Result := FRowsReady;
end;

function TCommand.GetRangeAction: INyxCollectionClipboardActionCapture;
begin
  Result := FRangeAction;
end;

function TCommand.GetSourceRowsAction: INyxCollectionClipboardActionCapture;
begin
  Result := FSourceRowsAction;
end;

function TCommand.GetPageRowsAction: INyxCollectionClipboardActionCapture;
begin
  Result := FPageRowsAction;
end;

function TCommand.GetSourceIdentityAction: INyxCollectionClipboardIdentityAction;
begin
  Result := FSourceIdentityAction;
end;

function TCommand.GetPageIdentityAction: INyxCollectionClipboardIdentityAction;
begin
  Result := FPageIdentityAction;
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
  LInspection: INyxCollectionInspection;
  LCapture: INyxCollectionColumnsCapture;
  LObservation: TNyxColumnLayoutObservation;
  LFields: TNyxCollectionClipboardFields;
  LSelection: INyxCollectionSelection;
  LScope: TCopyScope;
  LParentReady: INyxCancellationReadiness;
  LSpec: TNyxCollectionViewSpec;
  LColumn: TNyxCollectionColumn;
  LDefinition: TNyxCollectionField;
  LSchema: TNyxCollectionSchema;
  LIndex: Integer;
  LRange: INyxCollectionCellRangeCapture;
  LAdditional: INyxCancellationReadiness;
begin

  if FPrepared or FCompleteStarted or FRetired then
  begin
    raise ENyxCollectionClipboard.Create('Collection copy preparation is single-use');
  end;
  FPrepared := True;

  if (not FEligible and not FInspection) or ((FAction = nccaCell) and not FCellResolved) or
    ((FAction = nccaCellRange) and not FRangeResolved) then
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
  LInspection := NyxCollectionInspection(FView);

  if not OriginalFlags then
  begin
    Exit;
  end;

  if LInspection <> nil then
  begin

    if LInspection.CaptureInspection.Mode <> ncimPaused then
    begin
      LInspection := nil;
    end;
  end;

  if not OriginalFlags then
  begin
    Exit;
  end;

  if LInspection <> nil then
  begin
    FSource := LInspection.GetDisplayedSource;
  end
  else
  begin
    FSource := FStore.Snapshot;
  end;

  if not OriginalFlags or (FSource = nil) then
  begin
    Exit;
  end;
  FPage := FView.Snapshot;

  if not OriginalFlags or (FPage = nil) then
  begin
    Exit;
  end;

  if (FAction = nccaCellRange) and
    ((FRangeSource <> FSource) or (FRangePage <> FPage)) then
  begin
    Exit;
  end;

  if FAction in [nccaSelectedSource, nccaSelectedPage] then
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
  LSpec := LCapture.Spec;

  if not OriginalFlags then
  begin
    Exit;
  end;

  if (FAction = nccaCellRange) and (FRangeSerial <> FColumnSerial) then
  begin
    Exit;
  end;
  FColumnBaseline := LSpec.ToData.ToJSON + #10 + LCapture.Preferences.ToData.ToJSON;

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
  { An optional selected-cells action has independent cancellation. Clearing
    that range must not disable this ordinary cell's RAW/inspection actions. }

  if FAction = nccaCellRange then
  begin
    LAdditional := FRangeReady;
  end;
  LScope := TCopyScope.Create(FInput, FEpoch, FParentReady, LAdditional);
  FScope := LScope;
  FScopeReady := LScope;

  if FAction = nccaCellRange then
  begin
    LObservation := TNyxColumnLayoutObservation.Read(LCapture);
    LFields := NyxCollectionClipboardFields(LObservation, nccfVisiblePresentation);
    LRange := NewNyxCollectionCellRange(FSource, FPage, LFields,
      FRangeAnchor, FRangeFocus, FColumnSerial, FOptions.IncludeHeaders,
      FOptions.Lines, FOptions.WorkLimits);
    FPayload := LRange.Payload;
  end
  else if FAction = nccaCell then
  begin
    FPayload := NewNyxCollectionClipboardCell(FSource, FItem, FField, FOptions.WorkLimits);
    { Ordinary RAW/context entry captures metadata only. Strict formatting is
      deliberate later work and cannot make valid accepted RAW refuse. }
    for LIndex := 0 to LSpec.Count - 1 do
    begin
      LColumn := LSpec.ColumnAt(LIndex);

      if LColumn.Identity.Same(FField) and LColumn.HasFormatPolicy then
      begin
        LSchema := FSource.Schema;

        if not OriginalFlags then
        begin
          FPayload := nil;
          Exit;
        end;
        case LColumn.Kind of
          nskText: LDefinition := LSchema.Definition(NyxTextField(LColumn.FieldName));
          nskBoolean: LDefinition := LSchema.Definition(NyxBooleanField(LColumn.FieldName));
          nskInteger: LDefinition := LSchema.Definition(NyxIntegerField(LColumn.FieldName));
          nskNumber: LDefinition := LSchema.Definition(NyxNumberField(LColumn.FieldName));
        end;
        FDisplayPolicy := NyxCollectionCellDisplayPolicy(FField,
          LColumn.FormatPolicy, LDefinition.Domain, True);
        Break;
      end;
    end;
    PrepareContextRange(LCapture);
    PrepareContextRows;
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
    FContextRangePayload := nil;
    FContextRangeReady := nil;
    FSourceRowsPayload := nil;
    FPageRowsPayload := nil;
    FRowsReady := nil;
    FRangeAction := nil;
    FSourceRowsAction := nil;
    FPageRowsAction := nil;
    FSourceIdentityAction := nil;
    FPageIdentityAction := nil;
  end;
end;

procedure TCommand.PrepareOutput;
var
  LProvider: INyxCollectionExportView;
  LCapture: INyxCollectionExportCapture;
begin

  if not FPrepared or FCompleteStarted or FRetired or (FOutput <> nil) then
  begin
    raise ENyxCollectionClipboard.Create('Context output capture requires one prepared original command');
  end;

  if not GetCurrent or not Supports(FView, INyxCollectionExportView, LProvider) or
    not GetCurrent then
  begin
    Exit;
  end;
  LCapture := LProvider.CaptureExport;

  if GetCurrent then
  begin
    FOutput := LCapture;
  end;
end;

function TCommand.GetOutput: INyxCollectionExportCapture;
begin
  Result := FOutput;
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

procedure TCommand.ResolveRange(const AIntent: INyxCollectionCellRangeIntent);
begin

  if (FAction <> nccaCellRange) or FRangeStarted or FPrepared or FCompleteStarted or FRetired then
  begin
    raise ENyxCollectionRange.Create('Cell range resolution is single-use before preparation');
  end;
  FRangeStarted := True;
  CaptureRange(AIntent);
end;

procedure TCommand.AttachRange(const AIntent: INyxCollectionCellRangeIntent);
begin

  if (FAction <> nccaCell) or FContextRangeStarted or FPrepared or FCompleteStarted or FRetired then
  begin
    raise ENyxCollectionRange.Create('Cell range context attachment is single-use before preparation');
  end;
  FContextRangeStarted := True;

  if not (nccaCellRange in FOptions.Actions) then
  begin
    Exit; { No foreign range getter is entered without the original permission. }
  end;
  CaptureRange(AIntent);
end;

procedure TCommand.CaptureRange(const AIntent: INyxCollectionCellRangeIntent);
var
  LLease: INyxCollectionClipboardCommand;
  LIntent: INyxCollectionCellRangeIntent;
  LReady: INyxCancellationReadiness;
  LCapture: INyxCollectionCellRangeCapture;
  LSource: INyxCollectionSnapshot;
  LPage: INyxCollectionSnapshot;
begin
  LLease := Self;
  LIntent := AIntent;

  if not OriginalFlags or (LIntent = nil) then
  begin
    Exit;
  end;
  LReady := LIntent.Readiness;

  if not OriginalFlags or (LReady = nil) then
  begin
    Exit;
  end;
  FRangeReady := LReady;
  FRangeResolved := True;

  if not OriginalFlags then
  begin
    Exit;
  end;
  LCapture := LIntent.Capture;

  if not OriginalFlags or (LCapture = nil) then
  begin
    FRangeResolved := False;
    Exit;
  end;
  LSource := LIntent.Source;

  if not OriginalFlags or (LSource = nil) then
  begin
    FRangeResolved := False;
    Exit;
  end;
  LPage := LIntent.Page;

  if not OriginalFlags or (LPage = nil) then
  begin
    FRangeResolved := False;
    Exit;
  end;
  FRangeAnchor := LCapture.Anchor;

  if not OriginalFlags then
  begin
    Exit;
  end;
  FRangeFocus := LCapture.Focus;

  if not OriginalFlags then
  begin
    Exit;
  end;
  FRangeSerial := LCapture.ColumnSerial;

  if not OriginalFlags then
  begin
    Exit;
  end;
  FRangeSource := LSource;
  FRangePage := LPage;
end;

procedure TCommand.PrepareContextRows;
var
  LRows: INyxCollectionSelectedRowsCapture;
  LReady: INyxCancellationReadiness;
  LColumns: INyxCollectionColumnsCapture;
  LSelection: TNyxCollectionSelectionSnapshot;
  LFields: TNyxCollectionClipboardFields;
  LObservation: TNyxColumnLayoutObservation;
  LSourcePayload: INyxCollectionClipboardText;
  LPagePayload: INyxCollectionClipboardText;
  LSourceIDs: INyxCollectionIdentityText;
  LPageIDs: INyxCollectionIdentityText;
  LScope: TCopyScope;

  function Produce(AMembership: TNyxCollectionCopyMembership): INyxCollectionClipboardText;
  var
    LPayload: INyxCollectionClipboardText;
    LPage: INyxCollectionSnapshot;
  begin
    Result := nil;

    if AMembership = nccmSelectedPage then
    begin
      LPage := FPage;
    end;
    try
      LPayload := NewNyxCollectionClipboardRows(FSource, LSelection, LFields,
        AMembership, LPage, FOptions.IncludeHeaders, FOptions.Lines, FOptions.WorkLimits);

      if LPayload.RowCount > 0 then
      begin
        Result := LPayload;
      end;
    except
      on E: ENyxCollectionClipboard do
      begin
        { Optional membership/output admission cannot disable valid RAW
          inspection. Application callback failures keep their normal path. }
      end;
    end;
  end;

  function ProduceIDs(AMembership: TNyxCollectionCopyMembership): INyxCollectionIdentityText;
  var
    LPage: INyxCollectionSnapshot;
  begin
    Result := nil;

    if AMembership = nccmSelectedPage then
    begin
      LPage := FPage;
    end;
    try
      Result := NewNyxCollectionIdentityText(FSource, LSelection, AMembership,
        LPage, FOptions.IncludeHeaders, FOptions.Lines, FOptions.WorkLimits);
    except
      on E: ENyxCollectionClipboard do
      begin
        { Expected independent identity budget/membership refusal does not
          suppress RAW inspection or a valid selected-row field payload. }
      end;
    end;
  end;

begin
  LRows := FRowsCapture;

  if not FInspection or (LRows = nil) or not OriginalFlags then
  begin
    Exit;
  end;
  LReady := LRows.Readiness;

  if LReady = nil then
  begin
    Exit;
  end;
  try
    LReady.CheckReady;
  except
    Exit;
  end;

  if not OriginalFlags or (LRows.Source <> FSource) or (LRows.Page <> FPage) then
  begin
    Exit;
  end;
  LColumns := LRows.Columns;
  LSelection := LRows.Selection;

  if not OriginalFlags or (LColumns = nil) or (LColumns.Serial <> FColumnSerial) or
    (LSelection.Count = 0) then
  begin
    Exit;
  end;

  if (LColumns.Spec.ToData.ToJSON + #10 + LColumns.Preferences.ToData.ToJSON) <> FColumnBaseline then
  begin
    Exit;
  end;
  LObservation := TNyxColumnLayoutObservation.Read(LColumns);
  LFields := NyxCollectionClipboardFields(LObservation, FOptions.FieldChoice);

  if nccaSelectedSource in FOptions.Actions then
  begin
    LSourcePayload := Produce(nccmAllSelectedSource);
    LSourceIDs := ProduceIDs(nccmAllSelectedSource);
  end;

  if nccaSelectedPage in FOptions.Actions then
  begin
    LPagePayload := Produce(nccmSelectedPage);
    LPageIDs := ProduceIDs(nccmSelectedPage);
  end;

  if not OriginalFlags or ((LSourcePayload = nil) and (LPagePayload = nil) and
    (LSourceIDs = nil) and (LPageIDs = nil)) then
  begin
    Exit;
  end;
  try
    LReady.CheckReady;
  except
    Exit;
  end;
  LScope := TCopyScope.Create(FInput, FEpoch, FParentReady, LReady);
  FRowsReady := LScope;
  FSourceRowsPayload := LSourcePayload;
  FPageRowsPayload := LPagePayload;

  if LSourcePayload <> nil then
  begin
    FSourceRowsAction := TCapturedAction.Create(FOwner.FTransferIdentity,
      nccaSelectedSource, LSourcePayload, LScope);
  end;

  if LPagePayload <> nil then
  begin
    FPageRowsAction := TCapturedAction.Create(FOwner.FTransferIdentity,
      nccaSelectedPage, LPagePayload, LScope);
  end;

  if LSourceIDs <> nil then
  begin
    FSourceIdentityAction := TIdentityAction.Create(FOwner.FTransferIdentity, LSourceIDs, LScope);
  end;

  if LPageIDs <> nil then
  begin
    FPageIdentityAction := TIdentityAction.Create(FOwner.FTransferIdentity, LPageIDs, LScope);
  end;
end;

procedure TCommand.PrepareContextRange(const AColumns: INyxCollectionColumnsCapture);
var
  LObservation: TNyxColumnLayoutObservation;
  LFields: TNyxCollectionClipboardFields;
  LRange: INyxCollectionCellRangeCapture;
  LScope: TCopyScope;
begin

  if not FContextRangeStarted or not FRangeResolved or (FRangeReady = nil) or
    not (nccaCellRange in FOptions.Actions) or (FRangeSource <> FSource) or
    (FRangePage <> FPage) or (FRangeSerial <> FColumnSerial) or not OriginalFlags then
  begin
    Exit;
  end;
  try
    FRangeReady.CheckReady;
  except
    Exit; { Optional range cancellation never withdraws accepted-cell inspection. }
  end;
  LObservation := TNyxColumnLayoutObservation.Read(AColumns);
  LFields := NyxCollectionClipboardFields(LObservation, nccfVisiblePresentation);

  if not OriginalFlags then
  begin
    Exit;
  end;
  try
    { Re-admit against the command's exact original snapshots and current typed
      visible order, rather than trusting a foreign capture's payload/membership. }
    LRange := NewNyxCollectionCellRange(FSource, FPage, LFields,
      FRangeAnchor, FRangeFocus, FColumnSerial, FOptions.IncludeHeaders,
      FOptions.Lines, FOptions.WorkLimits);
  except
    on E: ENyxCollectionClipboard do
    begin
      { Rectangle/schema admission and its RAW TSV producer share this family.
        Optional multi-cell work/byte refusal must preserve a valid RAW cell. }
      Exit;
    end;
  end;

  if not OriginalFlags or not LRange.Contains(FItem, FField) then
  begin
    Exit;
  end;
  try
    FRangeReady.CheckReady;
  except
    Exit;
  end;
  LScope := TCopyScope.Create(FInput, FEpoch, FParentReady, FRangeReady);
  FContextRangeReady := LScope;
  FContextRangePayload := LRange.Payload;
  FRangeAction := TCapturedAction.Create(FOwner.FTransferIdentity, nccaCellRange,
    FContextRangePayload, LScope);
end;

function TCommand.GetRangePayload: INyxCollectionClipboardText;
begin
  Result := FContextRangePayload;
end;

function TCommand.GetRangeReadiness: INyxCancellationReadiness;
begin
  Result := FContextRangeReady;
end;

function TCommand.Complete(AAllowDefault: Boolean): Boolean;
begin
  Result := True;

  if FCompleteStarted or FRetired then
  begin
    Exit;
  end;

  if AAllowDefault and FEligible and CurrentProof(False) and FPrepared then
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
  LRowsView: INyxCollectionSelectedRowsView;
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
  Supports(LView, INyxCollectionSelectedRowsView, LRowsView);
  FRowsView := LRowsView;
  FTransferIdentity := TInterfacedObject.Create;
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

function TCommands.BeginCellInspection: INyxCollectionClipboardCellCommand;
var
  LLease: INyxCollectionClipboardCommands;
begin
  LLease := Self;
  Result := TCommand.Create(Self, nccaCell, Default(TNyxItemRef),
    Default(TNyxColumnIdentity), True, True);
end;

function TCommands.BeginRangeInput: INyxCollectionClipboardRangeCommand;
var
  LLease: INyxCollectionClipboardCommands;
begin
  LLease := Self;
  Result := TCommand.Create(Self, nccaCellRange, Default(TNyxItemRef),
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
begin
  LLease := Self;
  Result := SubmitCapturedAction(nccaCell, APayload, ACancellation, AReadiness,
    AReply, AReplyOwner);
end;

function TCommands.WriteCapturedAction(const AAction: INyxCollectionClipboardActionCapture;
  const ACancellation: INyxCancellationScope; const AReadiness: INyxCancellationReadiness;
  AReply: TNyxClipboardWriteReply;
  const AReplyOwner: IInterface): INyxClipboardWriteRequest;
var
  LLease: INyxCollectionClipboardCommands;
  LAction: INyxCollectionClipboardActionCapture;
  LProof: ICapturedActionProof;
  LData: TCapturedAction;
  LSerial: Integer;
  LReady: INyxCancellationReadiness;
begin
  LLease := Self;
  LAction := AAction;
  LSerial := FSerial;
  Result := nil;

  if FRevoked or (LAction = nil) or (AReadiness = nil) or
    not Supports(LAction, ICapturedActionProof, LProof) then
  begin
    Exit;
  end;
  LData := LProof.CapturedData;

  if FRevoked or (FSerial <> LSerial) or (LData = nil) or
    (LData.Identity <> FTransferIdentity) then
  begin
    Exit;
  end;
  LReady := TCopyScope.Create(FInput, FInput.Epoch, LData.Readiness, AReadiness);

  if FRevoked or (FSerial <> LSerial) then
  begin
    Exit;
  end;
  Result := SubmitCapturedAction(LData.Action, LData.Payload, ACancellation,
    LReady, AReply, AReplyOwner);
end;

function TCommands.WriteIdentities(const AAction: INyxCollectionClipboardIdentityAction;
  const ACancellation: INyxCancellationScope; const AReadiness: INyxCancellationReadiness;
  AReply: TNyxClipboardWriteReply; const AReplyOwner: IInterface): INyxClipboardWriteRequest;
var
  LLease: INyxCollectionClipboardCommands;
  LAction: INyxCollectionClipboardIdentityAction;
  LProof: IIdentityActionProof;
  LData: TIdentityAction;
  LSerial: Integer;
  LScheduler: INyxScheduler;
  LReady: INyxCancellationReadiness;
  LPermission: TNyxCollectionClipboardAction;
begin
  LLease := Self;
  LAction := AAction;
  LSerial := FSerial;
  LScheduler := FScheduler;
  Result := nil;

  if FRevoked or (LScheduler = nil) or (LAction = nil) or
    (AReadiness = nil) or not Supports(LAction, IIdentityActionProof, LProof) then
  begin
    Exit;
  end;
  LScheduler.RequireUI;
  LData := LProof.IdentityData;

  if FRevoked or (FSerial <> LSerial) or (LData = nil) or
    (LData.Identity <> FTransferIdentity) or (LData.Value = nil) then
  begin
    Exit;
  end;
  LPermission := nccaSelectedSource;

  if LData.Value.Membership = nccmSelectedPage then
  begin
    LPermission := nccaSelectedPage;
  end;
  LReady := TCopyScope.Create(FInput, FInput.Epoch, LData.Ready, AReadiness);

  if not FRevoked and (FSerial = LSerial) then
  begin
    Result := SubmitCapturedAction(LPermission, nil, ACancellation, LReady,
      AReply, AReplyOwner, LData.Value);
  end;
end;

function TCommands.SubmitCapturedAction(AAction: TNyxCollectionClipboardAction;
  const APayload: INyxCollectionClipboardText;
  const ACancellation: INyxCancellationScope;
  const AReadiness: INyxCancellationReadiness;
  AReply: TNyxClipboardWriteReply; const AReplyOwner: IInterface;
  const AIdentities: INyxCollectionIdentityText): INyxClipboardWriteRequest;
var
  LLease: INyxCollectionClipboardCommands;
  LSerial: Integer;
begin
  Result := nil;
  LLease := Self;

  if FRevoked or (Ord(AAction) < Ord(Low(TNyxCollectionClipboardAction))) or
    (Ord(AAction) > Ord(High(TNyxCollectionClipboardAction))) or
    not (AAction in FOptions.Actions) or ((APayload = nil) = (AIdentities = nil)) or
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
  Result := SubmitPayload(APayload, ACancellation, AReadiness, AReply, AReplyOwner,
    nil, AIdentities);
end;

function TCommands.WriteRepresented(const AValue: INyxCollectionCellText;
  const ACancellation: INyxCancellationScope;
  const AReadiness: INyxCancellationReadiness;
  AReply: TNyxClipboardWriteReply; const AReplyOwner: IInterface): INyxClipboardWriteRequest;
var
  LLease: INyxCollectionClipboardCommands;
  LScheduler: INyxScheduler;
  LInput: INyxCollectionInputLifetime;
  LSerial: Integer;
  LEpoch: Integer;
  LValue: INyxCollectionCellText;
  LCancelReady: INyxCancellationReadiness;
  LScope: INyxCancellationScope;
  LReady: INyxCancellationReadiness;
  LCopyScope: TCopyScope;

  function Original: Boolean;
  begin
    Result := not FRevoked and (FSerial = LSerial) and (FInput = LInput) and
      (LInput <> nil) and (LEpoch > 0) and (LEpoch < High(Integer)) and
      LInput.Ready and (LInput.Epoch = LEpoch);
  end;

begin
  Result := nil;
  LLease := Self;
  LScheduler := FScheduler;
  LInput := FInput;
  LSerial := FSerial;

  if FRevoked or (LInput = nil) or (LScheduler = nil) or
    not (nccaCell in FOptions.Actions) or (AValue = nil) or
    (ACancellation = nil) or (AReadiness = nil) then
  begin
    Exit;
  end;
  LEpoch := LInput.Epoch;
  LScheduler.RequireUI;

  if not Original then
  begin
    Exit;
  end;
  AReadiness.CheckReady;

  if not Original or not Supports(ACancellation, INyxCancellationReadiness, LCancelReady) then
  begin
    Exit;
  end;
  LValue := CopyNyxCollectionCellText(AValue, FOptions.WorkLimits.MaximumUTF8Bytes);

  if not Original then
  begin
    Exit;
  end;

  if not (LValue.Representation in [nccrFormattedDisplay, nccrExplicitDraft]) then
  begin
    raise ENyxCollectionRepresentation.Create('Represented transfer requires an explicit display or draft value');
  end;
  LCopyScope := TCopyScope.Create(LInput, LEpoch, AReadiness, LCancelReady);
  LScope := LCopyScope;
  LReady := LCopyScope;
  LReady.CheckReady;

  if not Original then
  begin
    Exit;
  end;

  if FSerial >= High(Integer) - 1 then
  begin
    FSerial := High(Integer);
    CancelCurrent;
    Exit;
  end;
  Inc(FSerial);
  LSerial := FSerial;
  CancelCurrent;

  if not Original then
  begin
    Exit;
  end;
  LReady.CheckReady;

  if Original then
  begin
    Result := SubmitPayload(nil, LScope, LReady, AReply, AReplyOwner, LValue);
  end;
end;

function TCommands.SubmitPayload(const APayload: INyxCollectionClipboardText;
  const ACancellation: INyxCancellationScope;
  const AReadiness: INyxCancellationReadiness;
  AReply: TNyxClipboardWriteReply; const AReplyOwner: IInterface;
  const AValue: INyxCollectionCellText;
  const AIdentities: INyxCollectionIdentityText): INyxClipboardWriteRequest;
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
  LReceipt.Value := AValue;
  LReceipt.Identities := AIdentities;
  LReceipt.Scope := AReadiness;
  LReceipt.Reply := AReply;
  LReceipt.ReplyOwner := AReplyOwner;
  FReceipt := LReceipt;
  FReceiptLease := LReceiptLease;

  if AIdentities <> nil then
  begin
    FIdentities := AIdentities;
    FIdentityPhase := nccpPending;
  end
  else if AValue = nil then
  begin
    FPayload := APayload;
    FPhase := nccpPending;
  end
  else
  begin
    FRepresentation := AValue;
    FRepresentationPhase := nccpPending;
  end;
  try

    if AIdentities <> nil then
    begin
      LRequest := LHost.BeginWriteText(AIdentities.Text, LScheduler,
        ACancellation, {$ifdef PAS2JS}@{$endif}LReceipt.Receive);
    end
    else if AValue = nil then
    begin
      LRequest := WriteNyxCollectionClipboard(APayload, LHost, LScheduler,
        ACancellation, {$ifdef PAS2JS}@{$endif}LReceipt.Receive);
    end
    else
    begin
      LRequest := LHost.BeginWriteText(AValue.Text, LScheduler,
        ACancellation, {$ifdef PAS2JS}@{$endif}LReceipt.Receive);
    end;
  except
    { An entered inline client failure is already terminal. It is never
      replayed or reclassified as a second host-admission result. }

    if FReceipt = LReceipt then
    begin
      LReceipt.Owner := nil;
      FReceipt := nil;
      FReceiptLease := nil;
      FRequest := nil;

      if AIdentities <> nil then
      begin
        FIdentityPhase := nccpCancelled;
      end
      else if AValue = nil then
      begin
        FPhase := nccpCancelled;
      end
      else
      begin
        FRepresentationPhase := nccpCancelled;
      end;
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
  LValue: INyxCollectionCellText;
  LRepresentationObserver: TNyxCollectionRepresentationObserver;
  LIdentities: INyxCollectionIdentityText;
  LIdentityObserver: TNyxCollectionIdentityObserver;
  LBytes: Integer;
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
  LValue := AReceipt.Value;
  LIdentities := AReceipt.Identities;
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
  LBytes := 0;

  if LIdentities <> nil then
  begin
    FIdentityPhase := nccpCancelled;
    LBytes := LIdentities.ByteCount;
  end
  else if LValue = nil then
  begin
    FPhase := nccpCancelled;
    LBytes := LPayload.ByteCount;
  end
  else
  begin
    FRepresentationPhase := nccpCancelled;
    LBytes := LValue.ByteCount;
  end;
  { Alternate hosts share the same terminal receipt contract as built-ins.
    Detach first: an invalid receipt is terminal even when validation raises. }
  ValidateNyxClipboardWriteResult(LResult, LBytes);

  if not LReady then
  begin
    Exit;
  end;

  if LIdentities <> nil then
  begin
    FIdentityResult := LResult;
    FIdentities := LIdentities;
    FIdentityPhase := nccpCompleted;
    LIdentityObserver := FIdentityObserver;

    if Assigned(LIdentityObserver) then
    begin
      LIdentityObserver(LIdentities, LResult);
    end;
  end
  else if LValue = nil then
  begin
    FResult := LResult;
    FPayload := LPayload;
    FPhase := nccpCompleted;
    LObserver := FObserver;

    if Assigned(LObserver) then
    begin
      LObserver(LPayload, LResult);
    end;
  end
  else
  begin
    FRepresentationResult := LResult;
    FRepresentation := LValue;
    FRepresentationPhase := nccpCompleted;
    LRepresentationObserver := FRepresentationObserver;

    if Assigned(LRepresentationObserver) then
    begin
      LRepresentationObserver(LValue, LResult);
    end;
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

  if FRepresentationPhase = nccpPending then
  begin
    FRepresentationPhase := nccpCancelled;
  end;

  if FIdentityPhase = nccpPending then
  begin
    FIdentityPhase := nccpCancelled;
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
  LRowsView: INyxCollectionSelectedRowsView;
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
  FRepresentationObserver := nil;
  FIdentityObserver := nil;
  LView := FView;
  LRowsView := FRowsView;
  LInput := FInput;
  LEvents := FEvents;
  LHost := FHost;
  LScheduler := FScheduler;
  FView := nil;
  FRowsView := nil;
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

procedure TCommands.ObserveRepresentation(AObserver: TNyxCollectionRepresentationObserver);
begin

  if FRevoked and Assigned(AObserver) then
  begin
    raise ENyxCollectionClipboard.Create('Retired represented-copy receiver cannot be replaced');
  end;
  FRepresentationObserver := AObserver;
end;

function TCommands.GetRepresentationPhase: TNyxCollectionClipboardPhase;
begin
  Result := FRepresentationPhase;
end;

function TCommands.GetLastRepresentationResult: TNyxClipboardWriteResult;
begin

  if FRepresentationPhase <> nccpCompleted then
  begin
    raise ENyxCollectionClipboard.Create('Represented copy has no completed host result');
  end;
  Result := FRepresentationResult;
end;

function TCommands.GetLastRepresentation: INyxCollectionCellText;
begin
  Result := FRepresentation;
end;

procedure TCommands.ObserveIdentities(AObserver: TNyxCollectionIdentityObserver);
begin

  if FRevoked and Assigned(AObserver) then
  begin
    raise ENyxCollectionClipboard.Create('Retired identity-copy receiver cannot be replaced');
  end;
  FIdentityObserver := AObserver;
end;

function TCommands.GetIdentityPhase: TNyxCollectionClipboardPhase;
begin
  Result := FIdentityPhase;
end;

function TCommands.GetLastIdentityResult: TNyxClipboardWriteResult;
begin

  if FIdentityPhase <> nccpCompleted then
  begin
    raise ENyxCollectionClipboard.Create('Identity copy has no completed host result');
  end;
  Result := FIdentityResult;
end;

function TCommands.GetLastIdentities: INyxCollectionIdentityText;
begin
  Result := FIdentities;
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
