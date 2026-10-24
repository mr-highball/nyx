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
unit nyx.collections.export.commands;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.collections.view, nyx.collections.mount, nyx.collections.export,
  nyx.collections.export.actions, nyx.collections.clipboard.actions,
  nyx.events, nyx.files.save, nyx.scheduler;

{ The borrowed factory returns the same original mount/input fence as copy.
  Only a synchronous command may own that fence; asynchronous saves receive
  independent canonical input/event cancellation facts and copied bytes. }
function NewNyxCollectionExportCommands(const AView: INyxCollectionView;
  const AInput: INyxCollectionInputLifetime; const AEvents: INyxEvents;
  const AHost: INyxFileSaveHost; const AScheduler: INyxScheduler;
  const AOptions: TNyxCollectionExportOptions;
  AReadiness: TNyxCollectionClipboardReadinessFactory): INyxCollectionExportCommands;

implementation

uses SysUtils, nyx.text, nyx.bytes, nyx.collections;

type
  { Controller is borrowed only under a local specialized entry lease. }
  TExports = class;
  { Forward launch object never belongs to the controller's pending slot. }
  TExportCommand = class;

  { Host ancestry owns only readonly input and certified flags. No store, view,
    command, mount, renderer or synchronous final fence crosses async entry. }
  TExportScope = class(TInterfacedObject, INyxCancellationScope, INyxCancellationReadiness)
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

  { Exact managed host receiver borrows its controller until terminal detach.
    ReplyOwner is retained through callbacks, never reconstructed from data. }
  TExportReceipt = class(TInterfacedObject)
  public
    Owner: TExports;
    Serial: Integer;
    Delivered: Boolean;
    Payload: INyxCollectionExportText;
    Readiness: INyxCancellationReadiness;
    Reply: TNyxFileSaveReply;
    ReplyOwner: IInterface;
    procedure Receive(const AResult: TNyxFileSaveResult);
  end;

  { Caller cancellation applies only to the operation admitted at this serial.
    The retained controller owns no mount and cannot redirect it to a successor. }
  TExportTicket = class(TInterfacedObject, INyxFileSaveRequest)
  public
    Owner: TExports;
    Lease: INyxCollectionExportCommands;
    Serial: Integer;
    Cancelled: Boolean;
    procedure Cancel;
  end;

  { Original pre-canonical authority, complete actual data capture and ephemeral
    mount fence. No controller member retains this launch, preventing cycles. }
  TExportCommand = class(TInterfacedObject, INyxCollectionExportCommand)
  private
    FOwner: TExports;
    FLease: INyxCollectionExportCommands;
    FInput: INyxCollectionInputLifetime;
    FEpoch: Integer;
    FSerial: Integer;
    FEntryReady: Boolean;
    FMembership: TNyxCollectionExportRows;
    FCapture: INyxCollectionExportCapture;
    FMountReady: INyxCancellationReadiness;
    FParent: INyxCancellationScope;
    FParentReady: INyxCancellationReadiness;
    FPrepared: Boolean;
    FChecking: Boolean;
    FRevision: Integer;
    FCompleted: Boolean;
    FRetired: Boolean;
    function Flags: Boolean;
  public
    constructor Create(AOwner: TExports; AMembership: TNyxCollectionExportRows);
    procedure Prepare;
    function GetCurrent: Boolean;
    function GetDispatchCurrent: Boolean;
    function GetCapture: INyxCollectionExportCapture;
    function Complete(AAllowDefault: Boolean): Boolean;
    procedure Retire;
  end;

  { One independent export slot joins a live view to injected save authority.
    Pending fields are detached before every callback/Cancel; raw receiver
    ownership never survives logical retirement. }
  TExports = class(TInterfacedObject, INyxCollectionExportCommands,
    INyxCollectionExportCapturedSave, INyxCollectionExportPermissions)
  public
    View: INyxCollectionView;
    Output: INyxCollectionExportView;
    Input: INyxCollectionInputLifetime;
    Events: INyxEvents;
    Host: INyxFileSaveHost;
    Scheduler: INyxScheduler;
    Options: TNyxCollectionExportOptions;
    MountReady: TNyxCollectionClipboardReadinessFactory;
    Revoked: Boolean;
    Destructing: Boolean;
    Serial: Integer;
    Phase: TNyxCollectionExportPhase;
    Request: INyxFileSaveRequest;
    Receipt: TExportReceipt;
    ReceiptLease: IInterface;
    Observer: TNyxCollectionExportObserver;
    LastPayload: INyxCollectionExportText;
    LastResult: TNyxFileSaveResult;
    constructor Create;
    destructor Destroy; override;
    function BeginExport(AMembership: TNyxCollectionExportRows): INyxCollectionExportCommand;
    procedure Observe(AObserver: TNyxCollectionExportObserver);
    procedure CancelPending;
    procedure Revoke;
    function GetPhase: TNyxCollectionExportPhase;
    function GetLastResult: TNyxFileSaveResult;
    function GetLastPayload: INyxCollectionExportText;
    function GetAllowedRows: TNyxCollectionExportMemberships;
    function SaveCaptured(const ACapture: INyxCollectionExportCapture;
      AMembership: TNyxCollectionExportRows; const ACancellation: INyxCancellationScope;
      const AReadiness: INyxCancellationReadiness; AReply: TNyxFileSaveReply;
      const AReplyOwner: IInterface): INyxFileSaveRequest;
    procedure Received(AReceipt: TExportReceipt; const AResult: TNyxFileSaveResult);
  end;

constructor TExportScope.Create(const AInput: INyxCollectionInputLifetime;
  AEpoch: Integer; const AParent: INyxCancellationReadiness);
begin
  inherited Create;
  FInput := AInput;
  FEpoch := AEpoch;
  FParent := AParent;
  FLocal := NewNyxCancellationScope;
end;

procedure TExportScope.Cancel;
begin
  FLocal.Cancel;
end;

procedure TExportScope.CheckReady;
begin
  FParent.CheckReady;

  if FLocal.Cancelled or not FInput.Current or not FInput.Ready or
    (FEpoch <= 0) or (FEpoch >= High(Integer)) or (FInput.Epoch <> FEpoch) then
  begin
    raise ENyxCollectionExport.Create('Original export input was retired');
  end;
end;

function TExportScope.GetCancelled: Boolean;
begin
  Result := True;
  try
    CheckReady;
    Result := False;
  except
    { Only explicitly certified callback-free scope facts are sampled here. }
  end;
end;

procedure TExportReceipt.Receive(const AResult: TNyxFileSaveResult);
var
  LSelf: IInterface;
  LOwner: TExports;
  LLease: INyxCollectionExportCommands;
begin
  LSelf := Self;
  Delivered := True;
  LOwner := Owner;

  if LOwner = nil then
  begin
    Exit;
  end;
  LLease := LOwner;
  LOwner.Received(Self, AResult);
end;

procedure TExportTicket.Cancel;
var
  LLease: INyxCollectionExportCommands;
begin

  if Cancelled then
  begin
    Exit;
  end;
  Cancelled := True;
  LLease := Lease;

  if not Owner.Revoked and (Owner.Serial = Serial) then
  begin
    Owner.CancelPending;
  end;
end;

constructor TExportCommand.Create(AOwner: TExports;
  AMembership: TNyxCollectionExportRows);
var
  LFactory: TNyxCollectionClipboardReadinessFactory;
begin
  inherited Create;
  FLease := AOwner;
  FOwner := AOwner;
  FInput := AOwner.Input;
  FEpoch := FInput.Epoch;
  FSerial := AOwner.Serial;
  FMembership := AMembership;
  FEntryReady := not AOwner.Revoked and FInput.Ready and
    (FEpoch > 0) and (FEpoch < High(Integer)) and
    (FSerial > 0) and (FSerial < High(Integer));
  LFactory := AOwner.MountReady;

  if FEntryReady and Assigned(LFactory) and FInput.Current then
  begin
    FMountReady := LFactory();
  end;
end;

function TExportCommand.Flags: Boolean;
begin
  Result := not FRetired and not FCompleted and FEntryReady and
    not FOwner.Revoked and (FOwner.Serial = FSerial) and (FMountReady <> nil);

  if Result then
  begin
    try
      FMountReady.CheckReady;
    except
      Exit(False);
    end;
    Result := FInput.Ready and (FInput.Epoch = FEpoch);
  end;
end;

procedure TExportCommand.Prepare;
var
  LCapture: INyxCollectionExportCapture;
  LParent: INyxCancellationScope;
  LReady: INyxCancellationReadiness;
begin

  if FPrepared or FCompleted or FRetired then
  begin
    raise ENyxCollectionExport.Create('Export preparation is single-use');
  end;
  FPrepared := True;

  if not Flags then
  begin
    Exit;
  end;
  LCapture := FOwner.Output.CaptureExport;

  if not Flags then
  begin
    Exit;
  end;
  LParent := NyxEventGeneration(FOwner.Events);

  if not Flags or (LParent = nil) or not Supports(LParent,
    INyxCancellationReadiness, LReady) then
  begin
    Exit;
  end;
  FCapture := LCapture;
  FParent := LParent;
  FParentReady := LReady;
  FRevision := FOwner.Events.ViewRevision;
end;

function TExportCommand.GetCurrent: Boolean;
begin
  Result := False;

  if FChecking or not Flags then
  begin
    Exit;
  end;

  if not FPrepared then
  begin
    Exit(True);
  end;

  if FCapture = nil then
  begin
    Exit;
  end;
  FChecking := True;
  try
    try
      FOwner.Output.CheckExportCurrent(FCapture);

      if not Flags or (FOwner.Events.ViewRevision <> FRevision) or
        not Flags or FParent.Cancelled then
      begin
        Exit;
      end;
      FParentReady.CheckReady;
      Result := Flags;
    except
      Result := False;
    end;
  finally
    FChecking := False;
  end;
end;

function TExportCommand.GetDispatchCurrent: Boolean;
begin
  Result := not FCompleted and not FRetired and not FOwner.Revoked and FInput.Current;
end;

function TExportCommand.GetCapture: INyxCollectionExportCapture;
begin
  Result := FCapture;
end;

function TExportCommand.Complete(AAllowDefault: Boolean): Boolean;
var
  LScope: TExportScope;
  LScopeLease: INyxCancellationScope;
begin
  Result := True;

  if FCompleted or FRetired then
  begin
    Exit;
  end;

  if AAllowDefault and GetCurrent and FPrepared and (FCapture <> nil) then
  begin
    FCompleted := True;
    LScope := TExportScope.Create(FInput, FEpoch, FParentReady);
    LScopeLease := LScope;
    FOwner.SaveCaptured(FCapture, FMembership, LScopeLease, LScope, nil, nil);
  end
  else
  begin
    FCompleted := True;
  end;
end;

procedure TExportCommand.Retire;
begin
  FRetired := True;
end;

constructor TExports.Create;
begin
  inherited Create;
  Serial := 1;
end;

destructor TExports.Destroy;
begin
  Destructing := True;
  try
    Revoke;
  except
    { Explicit cancellation reports host cleanup failures; final release cannot. }
  end;
  inherited Destroy;
end;

function TExports.BeginExport(AMembership: TNyxCollectionExportRows): INyxCollectionExportCommand;
begin

  if Revoked or (Ord(AMembership) < Ord(Low(TNyxCollectionExportRows))) or
    (Ord(AMembership) > Ord(High(TNyxCollectionExportRows))) or
    not Options.Allows(AMembership) then
  begin
    raise ENyxCollectionExport.Create('Export command requires an active known membership');
  end;
  Result := TExportCommand.Create(Self, AMembership);
end;

function TExports.GetAllowedRows: TNyxCollectionExportMemberships;
begin
  Result := [];

  if not Revoked then
  begin
    Result := Options.AllowedRows;
  end;
end;

procedure TExports.Observe(AObserver: TNyxCollectionExportObserver);
begin

  if Revoked then
  begin
    raise ENyxCollectionExport.Create('Export observer cannot attach after retirement');
  end;
  Observer := AObserver;
end;

procedure TExports.CancelPending;
var
  LSelf: INyxCollectionExportCommands;
  LRequest: INyxFileSaveRequest;
  LReceipt: IInterface;
begin

  if not Destructing then
  begin
    LSelf := Self;
  end;
  LRequest := Request;
  LReceipt := ReceiptLease;

  if Receipt <> nil then
  begin
    Receipt.Owner := nil;
    Receipt.Reply := nil;
  end;
  Request := nil;
  Receipt := nil;
  ReceiptLease := nil;

  if Phase = ncepPending then
  begin
    Phase := ncepCancelled;
  end;

  if Serial < High(Integer) then
  begin
    Inc(Serial);
  end;

  if LRequest <> nil then
  begin
    LRequest.Cancel;
  end;
end;

procedure TExports.Revoke;
var
  LSelf: INyxCollectionExportCommands;
begin

  if Revoked then
  begin
    Exit;
  end;

  if not Destructing then
  begin
    LSelf := Self;
  end;
  Revoked := True;
  MountReady := nil;
  Observer := nil;
  try
    CancelPending;
  finally
    View := nil;
    Output := nil;
    Input := nil;
    Events := nil;
    Host := nil;
    Scheduler := nil;
  end;
end;

function TExports.GetPhase: TNyxCollectionExportPhase;
begin
  Result := Phase;
end;

function TExports.GetLastResult: TNyxFileSaveResult;
begin

  if Phase <> ncepCompleted then
  begin
    raise ENyxCollectionExport.Create('Export has no current terminal host receipt');
  end;
  Result := LastResult;
end;

function TExports.GetLastPayload: INyxCollectionExportText;
begin
  Result := LastPayload;
end;

function TExports.SaveCaptured(const ACapture: INyxCollectionExportCapture;
  AMembership: TNyxCollectionExportRows; const ACancellation: INyxCancellationScope;
  const AReadiness: INyxCancellationReadiness; AReply: TNyxFileSaveReply;
  const AReplyOwner: IInterface): INyxFileSaveRequest;
var
  LLease: INyxCollectionExportCommands;
  LCapture: INyxCollectionExportCapture;
  LReady: INyxCancellationReadiness;
  LCancel: INyxCancellationScope;
  LOutput: INyxCollectionExportView;
  LInput: INyxCollectionInputLifetime;
  LHost: INyxFileSaveHost;
  LScheduler: INyxScheduler;
  LOptions: TNyxCollectionExportOptions;
  LPayload: INyxCollectionExportText;
  LBytes: TNyxBytes;
  LReceipt: TExportReceipt;
  LReceiptLease: IInterface;
  LRequest: INyxFileSaveRequest;
  LTicket: TExportTicket;
  LSerial: Integer;
  LEpoch: Integer;

  function Current: Boolean;
  begin
    Result := not Revoked and (Serial = LSerial) and
      (LSerial > 0) and (LSerial < High(Integer));

    if Result then
    begin
      try
        LReady.CheckReady;
        Result := LInput.Current and LInput.Ready and (LInput.Epoch = LEpoch);
      except
        Result := False;
      end;
    end;
  end;

  function CaptureCurrent: Boolean;
  begin
    Result := False;
    try
      LOutput.CheckExportCurrent(LCapture);
      Result := Current;
    except
      on E: ENyxCollection do
      begin
        { An old context menu is an ordinary refused default. Serialization
          and arbitrary host/callback errors remain outside this boundary. }
      end;
    end;
  end;

begin
  Result := nil;
  LLease := Self;
  LCapture := ACapture;
  LReady := AReadiness;
  LCancel := ACancellation;
  LOutput := Output;
  LInput := Input;
  LHost := Host;
  LScheduler := Scheduler;
  LOptions := Options.Copy;
  LSerial := Serial;

  if Revoked or (LCapture = nil) or (LReady = nil) or (LCancel = nil) or
    (LInput = nil) or (LSerial >= High(Integer) - 1) then
  begin
    Exit;
  end;
  LEpoch := LInput.Epoch;
  LReady.CheckReady;

  if not Current or LCancel.Cancelled or not Current then
  begin
    Exit;
  end;

  if not CaptureCurrent then
  begin
    Exit;
  end;
  LPayload := NewNyxCollectionExportText(LCapture, AMembership, LOptions);
  LBytes := LPayload.Bytes;

  if not CaptureCurrent then
  begin
    Exit;
  end;
  { Cancellation withdraws the old receiver and advances intent BEFORE host
    cleanup. Only that single expected advance belongs to this launch; any
    nested accepted operation advances further and must remain the winner. }
  Inc(LSerial);
  CancelPending;

  if not Current then
  begin
    Exit;
  end;

  if not CaptureCurrent then
  begin
    Exit;
  end;
  LReceipt := TExportReceipt.Create;
  LReceiptLease := LReceipt;
  LReceipt.Owner := Self;
  LReceipt.Serial := LSerial;
  LReceipt.Payload := LPayload;
  LReceipt.Readiness := LReady;
  LReceipt.Reply := AReply;
  LReceipt.ReplyOwner := AReplyOwner;
  Receipt := LReceipt;
  ReceiptLease := LReceiptLease;
  Phase := ncepPending;
  LastPayload := LPayload;
  try
    LRequest := LHost.BeginSave(LOptions.SaveOptions, LBytes, LScheduler, LCancel,
      {$ifdef PAS2JS}@{$endif}LReceipt.Receive);
  except

    if Receipt = LReceipt then
    begin
      LReceipt.Owner := nil;
      Receipt := nil;
      ReceiptLease := nil;
      Request := nil;
      Phase := ncepCancelled;
    end;
    raise;
  end;

  if not LReceipt.Delivered then
  begin

    if Current and (Receipt = LReceipt) and (LRequest <> nil) then
    begin
      Request := LRequest;
    end
    else
    begin

      if Receipt = LReceipt then
      begin
        LReceipt.Owner := nil;
        Receipt := nil;
        ReceiptLease := nil;
        Phase := ncepCancelled;
      end;

      if LRequest <> nil then
      begin
        LRequest.Cancel;
      end;

      if Current and (LRequest = nil) then
      begin
        raise ENyxCollectionExport.Create('File host returned neither a pending token nor an inline receipt');
      end;
    end;
  end;
  LTicket := TExportTicket.Create;
  Result := LTicket;
  LTicket.Owner := Self;
  LTicket.Lease := LLease;
  LTicket.Serial := LSerial;
end;

procedure TExports.Received(AReceipt: TExportReceipt; const AResult: TNyxFileSaveResult);
var
  LLease: INyxCollectionExportCommands;
  LReceiptLease: IInterface;
  LRequest: INyxFileSaveRequest;
  LObserver: TNyxCollectionExportObserver;
  LReply: TNyxFileSaveReply;
  LReplyOwner: IInterface;
  LPayload: INyxCollectionExportText;
  LResult: TNyxFileSaveResult;
  LSerial: Integer;
begin
  LLease := Self;

  if Revoked or (Receipt <> AReceipt) or (Serial <> AReceipt.Serial) then
  begin
    Exit;
  end;
  LSerial := Serial;
  LResult := AResult;
  LReceiptLease := ReceiptLease;
  LRequest := Request;
  LObserver := Observer;
  LReply := AReceipt.Reply;
  LReplyOwner := AReceipt.ReplyOwner;
  LPayload := AReceipt.Payload;
  AReceipt.Owner := nil;
  AReceipt.Reply := nil;
  Receipt := nil;
  ReceiptLease := nil;
  Request := nil;
  { Malformed host receipts are terminal too. Detach before validation so
    refusal cannot leave a fictitious pending operation with no receiver. }
  Phase := ncepCancelled;
  try
    AReceipt.Readiness.CheckReady;
  except
    Exit;
  end;
  ValidateNyxFileSaveResult(LResult, LPayload.ByteCount);
  LastResult := LResult;
  Phase := ncepCompleted;

  if Assigned(LObserver) then
  begin
    LObserver(LPayload, LResult);
  end;

  if not Revoked and (Serial = LSerial) and Assigned(LReply) then
  begin
    try
      AReceipt.Readiness.CheckReady;
    except
      Exit;
    end;
    LReply(LResult);
  end;
end;

function NewNyxCollectionExportCommands(const AView: INyxCollectionView;
  const AInput: INyxCollectionInputLifetime; const AEvents: INyxEvents;
  const AHost: INyxFileSaveHost; const AScheduler: INyxScheduler;
  const AOptions: TNyxCollectionExportOptions;
  AReadiness: TNyxCollectionClipboardReadinessFactory): INyxCollectionExportCommands;
var
  LOwner: TExports;
  LOutput: INyxCollectionExportView;
  LOptions: TNyxCollectionExportOptions;
begin
  LOptions := AOptions.Copy;

  if (AView = nil) or (AInput = nil) or (AEvents = nil) or (AHost = nil) or
    (AScheduler = nil) or not Assigned(AReadiness) or
    not Supports(AView, INyxCollectionExportView, LOutput) then
  begin
    raise ENyxCollectionExport.Create('Mounted export requires actual table/input/event/save authority');
  end;
  AScheduler.RequireUI;
  LOwner := TExports.Create;
  Result := LOwner;
  LOwner.View := AView;
  LOwner.Output := LOutput;
  LOwner.Input := AInput;
  LOwner.Events := AEvents;
  LOwner.Host := AHost;
  LOwner.Scheduler := AScheduler;
  LOwner.Options := LOptions;
  LOwner.MountReady := AReadiness;
end;

end.
