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
unit nyx.collections.cell.actions;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.model, nyx.root.types, nyx.events, nyx.scheduler,
  nyx.menu, nyx.popover, nyx.clipboard, nyx.collections.clipboard,
  nyx.collections.clipboard.actions, nyx.collections.export,
  nyx.collections.export.actions, nyx.collections.view, nyx.files.save;

const
  NyxCellActionsInspectID = 'nyx-cell-actions-inspect';
  NyxCellActionsCopyID = 'nyx-cell-actions-copy';
  NyxCellActionsExportPageID = 'nyx-cell-actions-export-page';
  NyxCellActionsExportFilteredID = 'nyx-cell-actions-export-filtered';
  NyxCellInspectorValueID = 'nyx-cell-inspector-value';
  NyxCellInspectorCopyID = 'nyx-cell-inspector-copy';
  NyxCellInspectorCloseID = 'nyx-cell-inspector-close';
  NyxCellInspectorStatusState = 'nyx-cell-inspector-status';

type
  { One synchronous presentation transition. Target input guards nest and keep
    actual drafts inert during focus changes. Leave is idempotent; the managed
    guard may retain its original mount only until this call finishes. }
  INyxCollectionCellPresentation = interface(IInterface)
    ['{4C459B2E-B7E8-430D-9207-26CAB00FD4D7}']
    procedure Leave;
  end;

  { Borrowed mount factory. Controllers withdraw it before retirement and never
    retain the returned mount-owning guard beyond a presentation call. }
  TNyxCollectionCellPresentation = function: INyxCollectionCellPresentation of object;

  { Target factory clones ordinary Nyx menu/inspector documents through their
    existing managed presenters. Anchors are borrowed until Revoke, which clears
    target access before renderer teardown. No factory retains a mount. }
  INyxCollectionCellPresenterFactory = interface(IInterface)
    ['{49A669A8-7D52-4BA8-9F8F-91FEA864627C}']
    function CreateMenu(ADocument: TNyxDocument; const ARoot: TNyxRootRef;
      const AItems: TNyxMenuItems): INyxMenu;
    function CreateInspector(ADocument: TNyxDocument;
      const ARoot: TNyxRootRef): INyxPopover;
    procedure Revoke;
  end;

  { One mounted table's managed accepted-value actions. OpenCell temporarily
    transfers the ORIGINAL command to existing queued UI work when True is
    returned; callers then retire only their outer physical envelope. Work ends
    original command ownership before storing only its immutable value
    and owner-free cancellation facts. A later model edit never redirects the
    inspector or Copy to a new value: the UI explicitly identifies its captured
    revision. Disable, input-epoch change and retirement refuse old actions.
    Readonly inspection text visibly escapes NUL/control characters; Copy uses
    the original raw bytes. No editor draft is read or committed here.
    Revoke is terminal and flags-only; Disconnect also cleans target resources. }
  INyxCollectionCellActions = interface(IInterface)
    ['{DA0CCBAF-B0D5-462E-B251-5935D4A76C8E}']
    function OpenCell(const ALaunch: INyxCollectionClipboardCommand): Boolean;
    procedure Close;
    procedure Revoke;
    procedure Disconnect;
    function GetMenu: INyxMenu;
    function GetInspector: INyxPopover;
    function GetSnapshot: INyxCollectionClipboardText;
    function GetLastResult: TNyxClipboardWriteResult;
    function GetLastExecution: INyxExecution;
    property Menu: INyxMenu read GetMenu;
    property Inspector: INyxPopover read GetInspector;
    property Snapshot: INyxCollectionClipboardText read GetSnapshot;
    property LastResult: TNyxClipboardWriteResult read GetLastResult;
    { Queued presentation/default failures retain the scheduler's actual
      terminal diagnostic. Nil means no queued action has been admitted. }
    property LastExecution: INyxExecution read GetLastExecution;
  end;

  { Optional flags-only intent witness. A prepared outer context cannot replace
    a later accepted nested context, even with identical row/value/geometry.
    Zero reports terminal/exhausted authority and never becomes current again. }
  INyxCollectionCellActionIntent = interface(IInterface)
    ['{F82465BE-F251-4B7F-A5D0-8AAC5C12125B}']
    function GetIntent: Integer;
    property Intent: Integer read GetIntent;
  end;

  { Optional file authority exposes RAW export only when explicitly configured.
    It leaves the established cell-actions ABI intact. Receipts distinguish a
    completed write from a browser download request; neither is inferred from
    opening the menu or from cancelling a receiver. }
  INyxCollectionCellExportActions = interface(IInterface)
    ['{4DA5F702-B618-4FAD-A894-7EABAC9CCDC6}']
    function GetExportEnabled: Boolean;
    function GetLastExportResult: TNyxFileSaveResult;
    property ExportEnabled: Boolean read GetExportEnabled;
    property LastExportResult: TNyxFileSaveResult read GetLastExportResult;
  end;

  { Optional mount capability leaves every existing GUID/vtable unchanged.
    ConfigureClipboard must already provide accepted-cell capture and explicit
    host authority; no second write engine or saved capability is introduced.
    Factory ownership transfers only through a successful managed assignment. }
  INyxCollectionCellActionsMount = interface(IInterface)
    ['{26E7D2DD-C22F-40E7-A4A8-0121B695C30C}']
    procedure ConfigureCellActions(const AFactory: INyxCollectionCellPresenterFactory;
      const AScheduler: INyxScheduler);
    function GetCellActions: INyxCollectionCellActions;
    property CellActions: INyxCollectionCellActions read GetCellActions;
  end;

{ Neither constructor retains a mount, tree or renderer. The temporary launch
  envelope may retain the mount's final fence only during canonical dispatch. }
function NewNyxCollectionCellActions(const AFactory: INyxCollectionCellPresenterFactory;
  const AWriter: INyxCollectionClipboardCapturedWrite; const AScheduler: INyxScheduler;
  APresentation: TNyxCollectionCellPresentation = nil;
  const AExporter: INyxCollectionExportCapturedSave = nil): INyxCollectionCellActions;
function NewNyxCollectionCellActionCommand(const ALaunch: INyxCollectionClipboardCommand;
  const AActions: INyxCollectionCellActions): INyxCollectionClipboardCommand;
{ Reversible display notation: backslashes and C0/DEL controls are escaped;
  line breaks remain readable. Clipboard bytes never use this representation. }
function NyxCollectionCellInspectionText(const AText: TNyxText): TNyxText;

implementation

uses
  SysUtils, nyx.types, nyx.controls, nyx.state, nyx.behavior
  {$ifdef PAS2JS}, JS{$endif};

type
  { Forward owner is borrowed only while a local specialized lease is held. }
  TCellActions = class;

  { Weak monotonic session identity prevents old menu/subscription/request
    callbacks from entering a replacement snapshot or a retired controller. }
  TCellPort = class(TInterfacedObject)
  public
    Owner: TCellActions;
    Active: Boolean;
    Serial: Integer;
    RequestSerial: Integer;
    ExportSerial: Integer;
  end;

  { The existing scheduler owns delayed menu entry, never the mount/controller.
    Original launch ownership ends at execution or cancellation; no controller
    field stores this command, so its synchronous mount fence cannot cycle. }
  TCellOpenWork = class(TInterfacedObject, INyxWork)
  public
    Port: TCellPort;
    PortLease: IInterface;
    Serial: Integer;
    Launch: INyxCollectionClipboardCommand;
    destructor Destroy; override;
    procedure Execute(const AExecution: INyxExecution);
  end;

  { Owner-free current-session scope joins only certified original flags and a
    canonical local cancellation port; asynchronous hosts retain no UI owner. }
  TCellScope = class(TInterfacedObject, INyxCancellationScope, INyxCancellationReadiness)
  private
    FOriginal: INyxCancellationReadiness;
    FLocal: INyxCancellationScope;
    FOpen: INyxPopoverOpenLifetime;
  public
    constructor Create(const AOriginal: INyxCancellationReadiness;
      const AOpen: INyxPopoverOpenLifetime = nil);
    procedure Cancel;
    procedure CheckReady;
    function GetCancelled: Boolean;
  end;

  { Managed menu/form callbacks carry an inert weak port and exact session,
    never a controller/presenter back-reference. }
  TCellCallback = class(TNyxEventCallback)
  public
    Port: TCellPort;
    PortLease: IInterface;
    Serial: Integer;
    Kind: Integer;
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

  { Form defaults are observed after canonical callbacks, then deferred beyond
    the renderer's legacy event tail. Neither observer nor work owns the form. }
  TCellButtonObserver = class(TNyxEventObserver)
  public
    Port: TCellPort;
    PortLease: IInterface;
    Serial: Integer;
    Kind: Integer;
    procedure Observe(const AEvent: TNyxEventInfo; ACallbackCount: Integer); override;
  end;

  { Exact original form/main readiness and successful-open admission survive
    dispatch without borrowing a controller, renderer or mutable popup owner. }
  TCellButtonWork = class(TInterfacedObject, INyxWork)
  public
    Port: TCellPort;
    PortLease: IInterface;
    Serial: Integer;
    Kind: Integer;
    MainReady: INyxCancellationReadiness;
    FormReady: INyxCancellationReadiness;
    Open: INyxPopoverOpenLifetime;
    procedure Execute(const AExecution: INyxExecution);
  end;

  { Menu activation closes its presenter before OnInvoke. This deferred default
    retains only the original main flags and weak session, so later canonical
    or legacy callbacks can retire it without retaining a closed menu/mount. }
  TCellExportWork = class(TInterfacedObject, INyxWork)
  public
    Port: TCellPort;
    PortLease: IInterface;
    Serial: Integer;
    Membership: TNyxCollectionExportRows;
    Ready: INyxCancellationReadiness;
    procedure Execute(const AExecution: INyxExecution);
  end;

  { Save reply owns inert session facts through host completion. No controller
    or presenter is owned by this borrowed callback; the explicit ReplyOwner
    keeps this receiver alive even for inline completion and reentry. }
  TCellExportReply = class(TInterfacedObject)
  public
    Port: TCellPort;
    PortLease: IInterface;
    Serial: Integer;
    RequestSerial: Integer;
    Delivered: Boolean;
    Ready: INyxCancellationReadiness;
    procedure Receive(const AResult: TNyxFileSaveResult);
  end;

  { One terminal host reply retains copied provenance and weak session facts.
    Delivered distinguishes inline reply exceptions from host admission errors. }
  TCellReply = class(TInterfacedObject)
  public
    Port: TCellPort;
    PortLease: IInterface;
    Serial: Integer;
    RequestSerial: Integer;
    Delivered: Boolean;
    Readiness: INyxCancellationReadiness;
    procedure Receive(const AResult: TNyxClipboardWriteResult);
  end;

  { Real first-error retention is independent of an empty message. Arbitrary
    browser throws retain exact identity, including null/undefined. }
  TCellCleanupFailure = record
    Failed: Boolean;
    {$ifdef PAS2JS}
    Value: JSValue;
    {$else}
    Value: Exception;
    {$endif}
  end;

  { Controller stores detached snapshot/cancellation only. Its menu and form
    subscriptions refer back through TCellPort, avoiding mount/presenter cycles. }
  TCellActions = class(TInterfacedObject, INyxCollectionCellActions,
    INyxCollectionCellActionIntent, INyxCollectionCellExportActions)
  private
    FFactory: INyxCollectionCellPresenterFactory;
    FWriter: INyxCollectionClipboardCapturedWrite;
    FExporter: INyxCollectionExportCapturedSave;
    FOutput: INyxCollectionExportCapture;
    FExportRequest: INyxFileSaveRequest;
    FExportReply: TCellExportReply;
    FExportReplyLease: IInterface;
    FExportResult: TNyxFileSaveResult;
    FHasExportResult: Boolean;
    FScheduler: INyxScheduler;
    FPresentation: TNyxCollectionCellPresentation;
    FPort: TCellPort;
    FPortLease: IInterface;
    FMenu: INyxMenu;
    FInspector: INyxPopover;
    FOpen: INyxPopoverOpenLifetime;
    FSnapshot: INyxCollectionClipboardText;
    FScope: TCellScope;
    FScopeLease: INyxCancellationScope;
    FTokens: array of INyxEventSubscription;
    FRequest: INyxClipboardWriteRequest;
    FReply: TCellReply;
    FReplyLease: IInterface;
    FResult: TNyxClipboardWriteResult;
    FHasResult: Boolean;
    FLastExecution: INyxExecution;
    function Current(ASerial: Integer): Boolean;
    function Advance: Integer;
    procedure Cleanup;
    procedure ShowCell(const ALaunch: INyxCollectionClipboardCommand; ASerial: Integer);
    procedure Subscribe(const AStream: INyxEventStream; AKind, ASerial: Integer);
    procedure Invoked(const AEvent: TNyxEventInfo; AKind, ASerial: Integer);
    procedure ObserveButton(const AEvents: INyxEvents; const AID: TNyxText;
      AKind, ASerial: Integer);
    procedure QueueInspector(const AEvent: TNyxEventInfo; AKind, ASerial: Integer);
    procedure InspectorDefault(AKind, ASerial: Integer;
      const AOpen: INyxPopoverOpenLifetime);
    procedure Inspect(ASerial: Integer);
    procedure CopyValue(ASerial: Integer);
    procedure QueueExport(AMembership: TNyxCollectionExportRows; ASerial: Integer);
    procedure ExportValue(AMembership: TNyxCollectionExportRows; ASerial: Integer);
    procedure Exported(AReply: TCellExportReply; const AResult: TNyxFileSaveResult);
    procedure Received(AReply: TCellReply; const AResult: TNyxClipboardWriteResult);
    procedure Status(const AText: TNyxText; ASerial: Integer);
  public
    constructor Create(const AFactory: INyxCollectionCellPresenterFactory;
      const AWriter: INyxCollectionClipboardCapturedWrite; const AScheduler: INyxScheduler;
      APresentation: TNyxCollectionCellPresentation;
      const AExporter: INyxCollectionExportCapturedSave);
    destructor Destroy; override;
    function OpenCell(const ALaunch: INyxCollectionClipboardCommand): Boolean;
    procedure Close;
    procedure Revoke;
    procedure Disconnect;
    function GetMenu: INyxMenu;
    function GetInspector: INyxPopover;
    function GetSnapshot: INyxCollectionClipboardText;
    function GetLastResult: TNyxClipboardWriteResult;
    function GetLastExecution: INyxExecution;
    function GetIntent: Integer;
    function GetExportEnabled: Boolean;
    function GetLastExportResult: TNyxFileSaveResult;
  end;

  { Short-lived original physical-input envelope changes only its default:
    opening a managed menu instead of immediately writing the clipboard. }
  TCellActionCommand = class(TInterfacedObject, INyxCollectionClipboardCommand)
  private
    FLaunch: INyxCollectionClipboardCommand;
    FActions: INyxCollectionCellActions;
    FCompleted: Boolean;
    FTransferred: Boolean;
    FIntent: INyxCollectionCellActionIntent;
    FOriginalIntent: Integer;
    FWantOutput: Boolean;
  public
    constructor Create(const ALaunch: INyxCollectionClipboardCommand;
      const AActions: INyxCollectionCellActions);
    procedure Prepare;
    function GetCurrent: Boolean;
    function GetDispatchCurrent: Boolean;
    function GetPayload: INyxCollectionClipboardText;
    function Complete(AAllowDefault: Boolean): Boolean;
    procedure Retire;
  end;

procedure CaptureCleanup(var AFailure: TCellCleanupFailure;
  {$ifdef PAS2JS}const AValue: JSValue{$else}AValue: Exception{$endif});
begin

  if not AFailure.Failed then
  begin
    AFailure.Failed := True;
    {$ifdef PAS2JS}
    AFailure.Value := AValue;
    {$else}
    AFailure.Value := Exception(AcquireExceptionObject);
    {$endif}
  end;
end;

procedure RaiseCleanup(const AFailure: TCellCleanupFailure);
begin

  if AFailure.Failed then
  begin
    {$ifdef PAS2JS}
    { pas2js casts Exception(value) through rtl.getObject, coercing undefined
      to null. This boundary glue must preserve the original platform throw. }
    asm throw AFailure.Value; end;
    {$else}
    raise AFailure.Value;
    {$endif}
  end;
end;

function NyxCollectionCellInspectionText(const AText: TNyxText): TNyxText;
var
  LIndex: Integer;
  LCode: Integer;
begin
  Result := '';
  for LIndex := 1 to Length(AText) do
  begin
    LCode := Ord(AText[LIndex]);
    case LCode of
      0: Result := Result + '\0';
      9: Result := Result + '\t';
      10: Result := Result + #10;
      13: Result := Result + '\r';
      92: Result := Result + '\\';
    else
      begin

        if (LCode < 32) or (LCode = 127) then
        begin
          Result := Result + '\u' + TNyxText(IntToHex(LCode, 4));
        end
        else
        begin
          Result := Result + AText[LIndex];
        end;
      end;
    end;
  end;
end;

constructor TCellScope.Create(const AOriginal: INyxCancellationReadiness;
  const AOpen: INyxPopoverOpenLifetime);
begin
  inherited Create;
  FOriginal := AOriginal;
  FLocal := NewNyxCancellationScope;
  FOpen := AOpen;
end;

procedure TCellScope.Cancel;
begin
  FLocal.Cancel;
end;

procedure TCellScope.CheckReady;
begin
  FOriginal.CheckReady;

  if FLocal.Cancelled or ((FOpen <> nil) and not FOpen.Current) then
  begin
    raise ENyxCollectionClipboard.Create('Original cell action was retired');
  end;
end;

function TCellScope.GetCancelled: Boolean;
begin
  Result := True;
  try
    CheckReady;
    Result := False;
  except
    { Both inputs are explicit callback-free canonical facts. }
  end;
end;

procedure TCellCallback.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
var
  LOwner: TCellActions;
  LLease: INyxCollectionCellActions;
begin
  LOwner := Port.Owner;

  if (LOwner = nil) or not Port.Active or (Port.Serial <> Serial) then
  begin
    Exit;
  end;
  LLease := LOwner;
  LOwner.Invoked(AEvent, Kind, Serial);
end;

procedure TCellButtonObserver.Observe(const AEvent: TNyxEventInfo;
  ACallbackCount: Integer);
var
  LOwner: TCellActions;
  LLease: INyxCollectionCellActions;
begin
  LOwner := Port.Owner;

  if (LOwner = nil) or not Port.Active or (Port.Serial <> Serial) or
    AEvent.DefaultPrevented then
  begin
    Exit;
  end;
  LLease := LOwner;
  LOwner.QueueInspector(AEvent, Kind, Serial);
end;

procedure TCellButtonWork.Execute(const AExecution: INyxExecution);
var
  LOwner: TCellActions;
  LLease: INyxCollectionCellActions;
begin

  if not Port.Active or (Port.Serial <> Serial) or (Open = nil) or
    not Open.Current then
  begin
    Exit;
  end;
  try
    FormReady.CheckReady;
    MainReady.CheckReady;
  except
    { Retired original facts are an ordinary refused default, not a new action. }
    Exit;
  end;
  LOwner := Port.Owner;

  if (LOwner = nil) or not Port.Active or (Port.Serial <> Serial) or
    not Open.Current then
  begin
    Exit;
  end;
  LLease := LOwner;
  LOwner.InspectorDefault(Kind, Serial, Open);
end;

procedure TCellExportWork.Execute(const AExecution: INyxExecution);
var
  LOwner: TCellActions;
  LLease: INyxCollectionCellActions;
begin

  if not Port.Active or (Port.Serial <> Serial) then
  begin
    Exit;
  end;
  try
    Ready.CheckReady;
  except
    Exit;
  end;
  LOwner := Port.Owner;

  if (LOwner = nil) or not Port.Active or (Port.Serial <> Serial) then
  begin
    Exit;
  end;
  LLease := LOwner;
  LOwner.ExportValue(Membership, Serial);
end;

procedure TCellExportReply.Receive(const AResult: TNyxFileSaveResult);
var
  LSelf: IInterface;
  LOwner: TCellActions;
  LLease: INyxCollectionCellActions;
begin
  LSelf := Self;
  Delivered := True;
  LOwner := Port.Owner;

  if (LOwner = nil) or not Port.Active or (Port.Serial <> Serial) or
    (Port.ExportSerial <> RequestSerial) then
  begin
    Exit;
  end;
  LLease := LOwner;
  LOwner.Exported(Self, AResult);
end;

destructor TCellOpenWork.Destroy;
begin

  if Launch <> nil then
  begin
    Launch.Retire;
  end;
  inherited Destroy;
end;

procedure TCellOpenWork.Execute(const AExecution: INyxExecution);
var
  LOwner: TCellActions;
  LLease: INyxCollectionCellActions;
  LLaunch: INyxCollectionClipboardCommand;
begin
  LLaunch := Launch;
  Launch := nil;
  try
    LOwner := Port.Owner;

    if (LOwner = nil) or not Port.Active or (Port.Serial <> Serial) or
      not LLaunch.Current then
    begin
      Exit;
    end;
    LLease := LOwner;
    LOwner.ShowCell(LLaunch, Serial);
  finally
    LLaunch.Complete(False);
    LLaunch.Retire;
  end;
end;

procedure TCellReply.Receive(const AResult: TNyxClipboardWriteResult);
var
  LOwner: TCellActions;
  LLease: INyxCollectionCellActions;
  LSelf: IInterface;
begin
  LSelf := Self;
  Delivered := True;
  LOwner := Port.Owner;

  if (LOwner = nil) or not Port.Active or (Port.Serial <> Serial) or
    (Port.RequestSerial <> RequestSerial) then
  begin
    Exit;
  end;
  LLease := LOwner;
  LOwner.Received(Self, AResult);
end;

constructor TCellActions.Create(const AFactory: INyxCollectionCellPresenterFactory;
  const AWriter: INyxCollectionClipboardCapturedWrite; const AScheduler: INyxScheduler;
  APresentation: TNyxCollectionCellPresentation;
  const AExporter: INyxCollectionExportCapturedSave);
begin
  inherited Create;

  if (AFactory = nil) or (AWriter = nil) or (AScheduler = nil) then
  begin
    raise ENyxCollectionClipboard.Create('Cell actions require a factory, mounted writer and scheduler');
  end;
  FFactory := AFactory;
  FWriter := AWriter;
  FExporter := AExporter;
  FScheduler := AScheduler;
  FPresentation := APresentation;
  FPort := TCellPort.Create;
  FPortLease := FPort;
  FPort.Owner := Self;
  FPort.Active := True;
  FPort.Serial := 1;
  FPort.RequestSerial := 1;
  FPort.ExportSerial := 1;
end;

destructor TCellActions.Destroy;
begin
  try
    Disconnect;
  except
    { Explicit Disconnect reports failures; final release never throws. }
  end;
  inherited Destroy;
end;

function TCellActions.Advance: Integer;
begin

  if FPort.Serial < High(Integer) then
  begin
    Inc(FPort.Serial);
  end;
  Result := FPort.Serial;
end;

function TCellActions.Current(ASerial: Integer): Boolean;
begin
  Result := FPort.Active and (FPort.Serial = ASerial) and
    (ASerial > 0) and (ASerial < High(Integer)) and (FScope <> nil);

  if Result then
  begin
    Result := not FScope.GetCancelled;
  end;
end;

procedure TCellActions.Cleanup;
var
  LMenu: INyxMenu;
  LInspector: INyxPopover;
  LTokens: array of INyxEventSubscription;
  LRequest: INyxClipboardWriteRequest;
  LExport: INyxFileSaveRequest;
  LIndex: Integer;
  LFailure: TCellCleanupFailure;
  LGuard: INyxCollectionCellPresentation;
  LEnter: TNyxCollectionCellPresentation;
begin
  LEnter := FPresentation;

  if FScope <> nil then
  begin
    FScope.Cancel;
  end;
  LMenu := FMenu;
  LInspector := FInspector;
  LTokens := FTokens;
  LRequest := FRequest;
  LExport := FExportRequest;
  FMenu := nil;
  FInspector := nil;
  FOpen := nil;
  FTokens := nil;
  FRequest := nil;
  FExportRequest := nil;
  FExportReply := nil;
  FExportReplyLease := nil;
  FOutput := nil;
  FReply := nil;
  FReplyLease := nil;
  FSnapshot := nil;
  FScope := nil;
  FScopeLease := nil;
  LFailure := Default(TCellCleanupFailure);
  try

    if (FPort <> nil) and FPort.Active and Assigned(LEnter) then
    begin
      LGuard := LEnter();
    end;
  except
    {$ifdef PAS2JS}CaptureCleanup(LFailure, JSExceptValue);
    {$else}on E: Exception do CaptureCleanup(LFailure, E);{$endif}
  end;
  for LIndex := 0 to Length(LTokens) - 1 do
  begin
    try
      LTokens[LIndex].Cancel;
    except
      {$ifdef PAS2JS}CaptureCleanup(LFailure, JSExceptValue);
      {$else}on E: Exception do CaptureCleanup(LFailure, E);{$endif}
    end;
  end;
  try

    if LRequest <> nil then
    begin
      LRequest.Cancel;
    end;
  except
    {$ifdef PAS2JS}CaptureCleanup(LFailure, JSExceptValue);
    {$else}on E: Exception do CaptureCleanup(LFailure, E);{$endif}
  end;
  try

    if LExport <> nil then
    begin
      LExport.Cancel;
    end;
  except
    {$ifdef PAS2JS}CaptureCleanup(LFailure, JSExceptValue);
    {$else}on E: Exception do CaptureCleanup(LFailure, E);{$endif}
  end;
  try

    if LMenu <> nil then
    begin
      LMenu.Close;
    end;
  except
    {$ifdef PAS2JS}CaptureCleanup(LFailure, JSExceptValue);
    {$else}on E: Exception do CaptureCleanup(LFailure, E);{$endif}
  end;
  try

    if LInspector <> nil then
    begin
      LInspector.Close;
    end;
  except
    {$ifdef PAS2JS}CaptureCleanup(LFailure, JSExceptValue);
    {$else}on E: Exception do CaptureCleanup(LFailure, E);{$endif}
  end;
  try

    if LGuard <> nil then
    begin
      LGuard.Leave;
    end;
  except
    {$ifdef PAS2JS}CaptureCleanup(LFailure, JSExceptValue);
    {$else}on E: Exception do CaptureCleanup(LFailure, E);{$endif}
  end;
  RaiseCleanup(LFailure);
end;
procedure TCellActions.Subscribe(const AStream: INyxEventStream; AKind, ASerial: Integer);
var
  LCallback: TCellCallback;
  LLease: INyxEventCallback;
  LToken: INyxEventSubscription;
begin
  LCallback := TCellCallback.Create;
  LLease := LCallback;
  LCallback.Port := FPort;
  LCallback.PortLease := FPortLease;
  LCallback.Serial := ASerial;
  LCallback.Kind := AKind;
  LToken := AStream.Subscribe(LLease);

  if not Current(ASerial) then
  begin
    LToken.Cancel;
    Exit;
  end;
  SetLength(FTokens, Length(FTokens) + 1);
  FTokens[High(FTokens)] := LToken;
end;

procedure TCellActions.ObserveButton(const AEvents: INyxEvents;
  const AID: TNyxText; AKind, ASerial: Integer);
var
  LObservation: INyxEventObservation;
  LObserver: TCellButtonObserver;
  LLease: INyxEventObserver;
  LToken: INyxEventSubscription;
begin

  if not Supports(AEvents, INyxEventObservation, LObservation) then
  begin
    raise ENyxCollectionClipboard.Create('Cell inspector requires canonical event observation');
  end;

  if not Current(ASerial) then
  begin
    Exit;
  end;
  LObserver := TCellButtonObserver.Create;
  LLease := LObserver;
  LObserver.Port := FPort;
  LObserver.PortLease := FPortLease;
  LObserver.Serial := ASerial;
  LObserver.Kind := AKind;
  LToken := LObservation.Observe(NyxControlEvents(AID, niRuntime), ntClick,
    LLease, nopAfterDispatch, NyxEvent(''));

  if not Current(ASerial) then
  begin
    LToken.Cancel;
    Exit;
  end;
  SetLength(FTokens, Length(FTokens) + 1);
  FTokens[High(FTokens)] := LToken;
end;

procedure TCellActions.QueueInspector(const AEvent: TNyxEventInfo;
  AKind, ASerial: Integer);
var
  LInspector: INyxPopover;
  LOpen: INyxPopoverOpenLifetime;
  LEvents: INyxEvents;
  LGeneration: INyxCancellationScope;
  LFormReady: INyxCancellationReadiness;
  LScheduler: INyxScheduler;
  LWork: TCellButtonWork;
  LWorkLease: INyxWork;
  LExecution: INyxExecution;
begin
  LInspector := FInspector;
  LOpen := FOpen;
  LScheduler := FScheduler;

  if AEvent.DefaultPrevented or not Current(ASerial) or (LInspector = nil) or
    (LOpen = nil) or not LOpen.Current or (LScheduler = nil) then
  begin
    Exit;
  end;
  LEvents := LInspector.Events;

  if not Current(ASerial) or (FInspector <> LInspector) or
    (FOpen <> LOpen) or not LOpen.Current then
  begin
    Exit;
  end;
  LGeneration := NyxEventGeneration(LEvents);
  LWork := TCellButtonWork.Create;
  LWorkLease := LWork;

  if (LGeneration = nil) or not Supports(LGeneration,
    INyxCancellationReadiness, LFormReady) then
  begin
    raise ENyxCollectionClipboard.Create('Cell inspector requires its original form readiness');
  end;

  if not Current(ASerial) or (FInspector <> LInspector) or
    (FOpen <> LOpen) or not LOpen.Current then
  begin
    Exit;
  end;
  LWork.Port := FPort;
  LWork.PortLease := FPortLease;
  LWork.Serial := ASerial;
  LWork.Kind := AKind;
  LWork.FormReady := LFormReady;
  LWork.Open := LOpen;
  LWork.MainReady := FScope;
  { PostUI is deferred by contract. Original successful-open and form flags
    are tested after legacy callbacks too; reopening cannot revive this work. }
  LExecution := LScheduler.PostUI(LWorkLease);

  if GetIntent = ASerial then
  begin
    FLastExecution := LExecution;
  end;
end;

procedure TCellActions.InspectorDefault(AKind, ASerial: Integer;
  const AOpen: INyxPopoverOpenLifetime);
begin

  if not Current(ASerial) or (FOpen <> AOpen) or not AOpen.Current then
  begin
    Exit;
  end;

  if AKind = 1 then
  begin
    CopyValue(ASerial);
  end
  else
  begin
    Close;
  end;
end;

function TCellActions.OpenCell(const ALaunch: INyxCollectionClipboardCommand): Boolean;
var
  LWork: TCellOpenWork;
  LLease: INyxWork;
  LSelf: INyxCollectionCellActions;
  LScheduler: INyxScheduler;
  LOriginalIntent: Integer;
  LExecution: INyxExecution;
  LSerial: Integer;
begin
  Result := False;
  LSelf := Self;
  LScheduler := FScheduler;
  LOriginalIntent := GetIntent;

  if (LOriginalIntent = 0) or (LScheduler = nil) or (ALaunch = nil) or
    not ALaunch.Current or (ALaunch.Payload = nil) or (GetIntent <> LOriginalIntent) then
  begin
    Exit;
  end;
  LWork := TCellOpenWork.Create;
  LLease := LWork;
  LWork.Port := FPort;
  LWork.PortLease := FPortLease;
  LWork.Serial := Advance;
  LSerial := LWork.Serial;
  LWork.Launch := ALaunch;

  if FScope <> nil then
  begin
    FScope.Cancel;
  end;
  { Native presentation leaves the original physical context-message frame.
    Existing scheduler ownership also handles cancellation before execution. }
  LExecution := LScheduler.PostUI(LLease);

  if GetIntent = LSerial then
  begin
    FLastExecution := LExecution;
  end;
  Result := True;
end;

procedure TCellActions.ShowCell(const ALaunch: INyxCollectionClipboardCommand; ASerial: Integer);
var
  LLease: INyxCollectionCellActions;
  LSerial: Integer;
  LPrepared: INyxCollectionClipboardPreparedScope;
  LScope: INyxCancellationReadiness;
  LPayload: INyxCollectionClipboardText;
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LMenu: INyxMenu;
  LFactory: INyxCollectionCellPresenterFactory;
  LItems: TNyxMenuItems;
  LGuard: INyxCollectionCellPresentation;
  LEnter: TNyxCollectionCellPresentation;
  LScheduler: INyxScheduler;
  LOutputPort: INyxCollectionClipboardOutputCapture;
  LOutput: INyxCollectionExportCapture;
begin
  LLease := Self;
  LScheduler := FScheduler;

  if (GetIntent <> ASerial) or (LScheduler = nil) then
  begin
    Exit;
  end;
  LScheduler.RequireUI;

  if (GetIntent <> ASerial) or (ALaunch = nil) or not ALaunch.Current then
  begin
    Exit;
  end;
  LPayload := ALaunch.Payload;

  if (LPayload = nil) or (LPayload.RowCount <> 1) or (LPayload.FieldCount <> 1) or
    not Supports(ALaunch, INyxCollectionClipboardPreparedScope, LPrepared) then
  begin
    Exit;
  end;
  LScope := LPrepared.Readiness;

  if (FExporter <> nil) and Supports(ALaunch,
    INyxCollectionClipboardOutputCapture, LOutputPort) then
  begin
    LOutput := LOutputPort.Output;
  end;

  if (LScope = nil) or not ALaunch.Current or (GetIntent <> ASerial) then
  begin
    Exit;
  end;
  LSerial := ASerial;
  LFactory := FFactory;
  Cleanup;

  if not FPort.Active or (FPort.Serial <> LSerial) or
    (LSerial >= High(Integer)) or not ALaunch.Current then
  begin
    Exit;
  end;
  FSnapshot := LPayload;
  FOutput := LOutput;
  FScope := TCellScope.Create(LScope);
  FScopeLease := FScope;
  LDocument := TNyxDocument.Create;
  try
    LPage := NewNyxPage('nyx-cell-actions-page');
    LPage.Configure.Padding(8).Gap(4).Done;
    LPage.Add(NewNyxButton(NyxCellActionsInspectID).Configure
      .PartName(NyxPart(NyxCellActionsInspectID)).Text('Inspect accepted value').Done);
    LPage.Add(NewNyxButton(NyxCellActionsCopyID).Configure
      .PartName(NyxPart(NyxCellActionsCopyID)).Text('Copy accepted value').Done);
    LDocument.AddPage(LPage);
    LItems := NyxMenuItems.Add(NyxMenuAction(NyxPart(NyxCellActionsInspectID),
      NyxMenuCommand('inspect'))).Add(NyxMenuAction(NyxPart(NyxCellActionsCopyID), NyxMenuCommand('copy')));

    if (FExporter <> nil) and (LOutput <> nil) then
    begin
      LPage.Add(NewNyxButton(NyxCellActionsExportPageID).Configure
        .PartName(NyxPart(NyxCellActionsExportPageID)).Text('Export current page as RAW TSV').Done);
      LPage.Add(NewNyxButton(NyxCellActionsExportFilteredID).Configure
        .PartName(NyxPart(NyxCellActionsExportFilteredID)).Text('Export all filtered rows as RAW TSV').Done);
      LItems := LItems.Add(NyxMenuAction(NyxPart(NyxCellActionsExportPageID),
        NyxMenuCommand('export-page'))).Add(NyxMenuAction(NyxPart(NyxCellActionsExportFilteredID),
        NyxMenuCommand('export-filtered')));
    end;
    LMenu := LFactory.CreateMenu(LDocument, NyxPageRoot(LPage.ID), LItems);
  finally
    LDocument.Free;
  end;

  if not Current(LSerial) or not ALaunch.Current then
  begin
    Exit;
  end;
  FMenu := LMenu;
  Subscribe(LMenu.OnInvoke, 0, LSerial);

  if not Current(LSerial) or not ALaunch.Current then
  begin
    Exit;
  end;
  LEnter := FPresentation;

  if Assigned(LEnter) then
  begin
    LGuard := LEnter();
  end;
  try

    if Current(LSerial) and ALaunch.Current then
    begin
      LMenu.Open(NyxMenu('Cell actions'));
    end;
  finally

    if LGuard <> nil then
    begin
      LGuard.Leave;
    end;
  end;

  if not Current(LSerial) or not ALaunch.Current then
  begin
    LMenu.Close;
  end;
end;

procedure TCellActions.Invoked(const AEvent: TNyxEventInfo; AKind, ASerial: Integer);
var
  LInvocation: TNyxMenuInvocation;
begin

  if not Current(ASerial) then
  begin
    Exit;
  end;

  if AKind = 0 then
  begin
    LInvocation := NyxMenuInvocation(AEvent);

    if LInvocation.Command.Name = 'inspect' then
    begin
      Inspect(ASerial);
    end
    else if LInvocation.Command.Name = 'copy' then
    begin
      CopyValue(ASerial);
    end
    else if LInvocation.Command.Name = 'export-page' then
    begin
      QueueExport(ncerCurrentPage, ASerial);
    end
    else if LInvocation.Command.Name = 'export-filtered' then
    begin
      QueueExport(ncerAllFiltered, ASerial);
    end;
  end;
end;

procedure TCellActions.Inspect(ASerial: Integer);
var
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LInspector: INyxPopover;
  LFactory: INyxCollectionCellPresenterFactory;
  LPayload: INyxCollectionClipboardText;
  LAdmission: INyxPopoverOpenAdmission;
  LMemo: INyxMemo;
  LStatus: INyxLabel;
  LGuard: INyxCollectionCellPresentation;
  LEnter: TNyxCollectionCellPresentation;
  LOpen: INyxPopoverOpenLifetime;
  LEvents: INyxEvents;
begin
  LFactory := FFactory;
  LPayload := FSnapshot;
  LDocument := TNyxDocument.Create;
  try
    LPage := NewNyxPage('nyx-cell-inspector-page');
    LPage.Configure.Padding(12).Gap(8).Done;
    LPage.Add(NewNyxHeading('nyx-cell-inspector-heading').Configure.Text('Accepted cell value').Done);
    LPage.Add(NewNyxLabel('nyx-cell-inspector-provenance').Configure.Text(
      LPayload.ItemAt(0).ID + ' / ' + LPayload.FieldAt(0).Name +
      ' / captured revision ' + TNyxText(IntToStr(LPayload.Revision))).Done);
    LPage.Add(NewNyxLabel('nyx-cell-inspector-notation').Configure.Text(
      'Escaped controls and backslashes; Copy preserves the exact raw value.').Done);
    LMemo := NewNyxMemo(NyxCellInspectorValueID);
    LMemo.Value := NyxCollectionCellInspectionText(LPayload.Text);
    LMemo.ReadOnly := True;
    LMemo.Configure.Height(200).Done;
    LPage.Add(LMemo);
    LPage.Add(NewNyxButton(NyxCellInspectorCopyID).Configure.Text('Copy accepted value').Done);
    LPage.Add(NewNyxButton(NyxCellInspectorCloseID).Configure
      .PartName(NyxPart(NyxCellInspectorCloseID)).Text('Close').Done);
    LDocument.State.SetValue(NyxTextState(NyxCellInspectorStatusState), '');
    LStatus := NewNyxLabel('nyx-cell-inspector-status');
    LStatus.Binds.Text(NyxTextState(NyxCellInspectorStatusState)).Done;
    LPage.Add(LStatus);
    LDocument.AddPage(LPage);
    LInspector := LFactory.CreateInspector(LDocument, NyxPageRoot(LPage.ID));
  finally
    LDocument.Free;
  end;

  if not Current(ASerial) then
  begin
    Exit;
  end;
  FInspector := LInspector;
  LEvents := LInspector.Events;

  if not Current(ASerial) or (FInspector <> LInspector) then
  begin
    Exit;
  end;
  ObserveButton(LEvents, NyxCellInspectorCopyID, 1, ASerial);
  ObserveButton(LEvents, NyxCellInspectorCloseID, 2, ASerial);

  if not Current(ASerial) then
  begin
    Exit;
  end;
  LEnter := FPresentation;

  if Assigned(LEnter) then
  begin
    LGuard := LEnter();
  end;
  try

    if Current(ASerial) then
    begin
      LInspector.Open(NyxPopover('Accepted cell value').Size(480, 400).Focus(NyxPart(NyxCellInspectorCloseID)));
    end;
  finally

    if LGuard <> nil then
    begin
      LGuard.Leave;
    end;
  end;

  if not Current(ASerial) or not Supports(LInspector, INyxPopoverOpenAdmission, LAdmission) then
  begin
    LInspector.Close;
    Exit;
  end;
  LOpen := LAdmission.GetOpenLifetime;

  if Current(ASerial) and (FInspector = LInspector) and
    (LOpen <> nil) and LOpen.Current then
  begin
    FOpen := LOpen;
  end
  else
  begin
    LInspector.Close;
  end;
end;

procedure TCellActions.Status(const AText: TNyxText; ASerial: Integer);
var
  LInspector: INyxPopover;
  LOpen: INyxPopoverOpenLifetime;
  LState: TNyxState;
begin
  LInspector := FInspector;
  LOpen := FOpen;

  if Current(ASerial) and (LInspector <> nil) and (LOpen <> nil) and LOpen.Current then
  begin
    LState := LInspector.State;

    if Current(ASerial) and (FInspector = LInspector) and
      (FOpen = LOpen) and LOpen.Current then
    begin
      LState.SetValue(NyxTextState(NyxCellInspectorStatusState), AText);
    end;
  end;
end;

procedure TCellActions.CopyValue(ASerial: Integer);
var
  LPayload: INyxCollectionClipboardText;
  LScope: INyxCancellationScope;
  LOld: INyxClipboardWriteRequest;
  LRequest: INyxClipboardWriteRequest;
  LReply: TCellReply;
  LReplyLease: IInterface;
  LWriter: INyxCollectionClipboardCapturedWrite;
  LRequestSerial: Integer;
  LCopyScope: TCellScope;
begin

  if not Current(ASerial) or (FPort.RequestSerial >= High(Integer) - 1) then
  begin
    Exit;
  end;
  Inc(FPort.RequestSerial);
  LRequestSerial := FPort.RequestSerial;
  LOld := FRequest;
  FRequest := nil;
  FReply := nil;
  FReplyLease := nil;

  if LOld <> nil then
  begin
    LOld.Cancel;
  end;

  if not Current(ASerial) or (FPort.RequestSerial <> LRequestSerial) then
  begin
    Exit;
  end;
  LPayload := FSnapshot;
  LCopyScope := TCellScope.Create(FScope, FOpen);
  LScope := LCopyScope;
  LWriter := FWriter;
  LReply := TCellReply.Create;
  LReplyLease := LReply;
  LReply.Port := FPort;
  LReply.PortLease := FPortLease;
  LReply.Serial := ASerial;
  LReply.RequestSerial := LRequestSerial;
  LReply.Readiness := LCopyScope;
  FReply := LReply;
  FReplyLease := LReplyLease;
  try
    LRequest := LWriter.WriteCaptured(LPayload, LScope, LCopyScope,
      {$ifdef PAS2JS}@{$endif}LReply.Receive, LReplyLease);
  except

    if FReply = LReply then
    begin
      FReply := nil;
      FReplyLease := nil;
      FRequest := nil;
    end;
    raise;
  end;

  if not LReply.Delivered then
  begin

    if (LRequest <> nil) and Current(ASerial) and (FReply = LReply) and
      (FPort.RequestSerial = LRequestSerial) then
    begin
      FRequest := LRequest;
    end
    else if LRequest <> nil then
    begin
      LRequest.Cancel;
    end;

    if (LRequest = nil) and (FReply = LReply) then
    begin
      FReply := nil;
      FReplyLease := nil;
    end;
  end;
end;

procedure TCellActions.Received(AReply: TCellReply; const AResult: TNyxClipboardWriteResult);
var
  LRequest: INyxClipboardWriteRequest;
  LReplyLease: IInterface;
  LSerial: Integer;
begin

  if not Current(AReply.Serial) or (FReply <> AReply) then
  begin
    Exit;
  end;
  LSerial := AReply.Serial;
  LRequest := FRequest;
  LReplyLease := FReplyLease;
  FRequest := nil;
  FReply := nil;
  FReplyLease := nil;
  try
    AReply.Readiness.CheckReady;
  except
    Exit;
  end;
  ValidateNyxClipboardWriteResult(AResult, FSnapshot.ByteCount);
  FResult := AResult;
  FHasResult := True;

  if AResult.Outcome = ncwoWritten then
  begin
    Status('Copied the captured accepted value.', LSerial);
  end
  else
  begin
    Status('Clipboard request did not write: ' + AResult.Error, LSerial);
  end;
end;

procedure TCellActions.QueueExport(AMembership: TNyxCollectionExportRows;
  ASerial: Integer);
var
  LWork: TCellExportWork;
  LLease: INyxWork;
  LScheduler: INyxScheduler;
  LExecution: INyxExecution;
begin

  if not Current(ASerial) or (FExporter = nil) or (FOutput = nil) then
  begin
    Exit;
  end;
  LScheduler := FScheduler;
  LWork := TCellExportWork.Create;
  LLease := LWork;
  LWork.Port := FPort;
  LWork.PortLease := FPortLease;
  LWork.Serial := ASerial;
  LWork.Membership := AMembership;
  LWork.Ready := FScope;
  LExecution := LScheduler.PostUI(LLease);

  if GetIntent = ASerial then
  begin
    FLastExecution := LExecution;
  end;
end;

procedure TCellActions.ExportValue(AMembership: TNyxCollectionExportRows;
  ASerial: Integer);
var
  LExporter: INyxCollectionExportCapturedSave;
  LOutput: INyxCollectionExportCapture;
  LOld: INyxFileSaveRequest;
  LRequest: INyxFileSaveRequest;
  LReply: TCellExportReply;
  LReplyLease: IInterface;
  LScope: TCellScope;
  LScopeLease: INyxCancellationScope;
  LSerial: Integer;
begin

  if not Current(ASerial) or (FExporter = nil) or (FOutput = nil) or
    (FPort.ExportSerial >= High(Integer) - 1) then
  begin
    Exit;
  end;
  Inc(FPort.ExportSerial);
  LSerial := FPort.ExportSerial;
  LOld := FExportRequest;
  FExportRequest := nil;
  FExportReply := nil;
  FExportReplyLease := nil;

  if LOld <> nil then
  begin
    LOld.Cancel;
  end;

  if not Current(ASerial) or (FPort.ExportSerial <> LSerial) then
  begin
    Exit;
  end;
  LExporter := FExporter;
  LOutput := FOutput;
  Inspect(ASerial);

  if not Current(ASerial) or (FPort.ExportSerial <> LSerial) or
    (FOpen = nil) or not FOpen.Current then
  begin
    Exit;
  end;
  Status('Exporting the captured accepted rows as RAW TSV.', ASerial);

  if not Current(ASerial) or (FPort.ExportSerial <> LSerial) then
  begin
    Exit;
  end;
  LScope := TCellScope.Create(FScope, FOpen);
  LScopeLease := LScope;
  LReply := TCellExportReply.Create;
  LReplyLease := LReply;
  LReply.Port := FPort;
  LReply.PortLease := FPortLease;
  LReply.Serial := ASerial;
  LReply.RequestSerial := LSerial;
  LReply.Ready := LScope;
  FExportReply := LReply;
  FExportReplyLease := LReplyLease;
  try
    LRequest := LExporter.SaveCaptured(LOutput, AMembership, LScopeLease, LScope,
      {$ifdef PAS2JS}@{$endif}LReply.Receive, LReplyLease);
  except

    if FExportReply = LReply then
    begin
      FExportReply := nil;
      FExportReplyLease := nil;
      FExportRequest := nil;
    end;
    raise;
  end;

  if not LReply.Delivered then
  begin

    if Current(ASerial) and (FPort.ExportSerial = LSerial) and
      (FExportReply = LReply) then
    begin
      FExportRequest := LRequest;

      if LRequest = nil then
      begin
        FExportReply := nil;
        FExportReplyLease := nil;
        Status('Export refused: the original table capture is no longer current.', ASerial);
      end;
    end
    else if LRequest <> nil then
    begin
      LRequest.Cancel;
    end;
  end;
end;

procedure TCellActions.Exported(AReply: TCellExportReply;
  const AResult: TNyxFileSaveResult);
var
  LRequest: INyxFileSaveRequest;
  LReplyLease: IInterface;
  LSerial: Integer;
begin

  if not Current(AReply.Serial) or (FExportReply <> AReply) then
  begin
    Exit;
  end;
  LSerial := AReply.Serial;
  LRequest := FExportRequest;
  LReplyLease := FExportReplyLease;
  FExportRequest := nil;
  FExportReply := nil;
  FExportReplyLease := nil;
  try
    AReply.Ready.CheckReady;
  except
    Exit;
  end;
  { The export controller has already validated the complete payload/result. }
  FExportResult := AResult;
  FHasExportResult := True;
  case AResult.Outcome of
    nfsoWritten: Status('The captured RAW TSV was written.', LSerial);
    nfsoDownloadRequested: Status('A download was requested for the captured RAW TSV.', LSerial);
    nfsoCancelled: Status('Export was cancelled.', LSerial);
    nfsoActivationRequired: Status('Export requires a new user activation.', LSerial);
    nfsoUnavailable: Status('This host cannot save the requested RAW TSV.', LSerial);
    nfsoFailed: Status('Export failed: ' + AResult.Error, LSerial);
  end;
end;

procedure TCellActions.Close;
var
  LLease: INyxCollectionCellActions;
begin
  LLease := Self;
  Advance;
  Cleanup;
end;

procedure TCellActions.Revoke;
begin

  if FPort = nil then
  begin
    Exit;
  end;
  FPort.Active := False;
  FPort.Owner := nil;
  FPort.Serial := High(Integer);
  FPresentation := nil;

  if FScope <> nil then
  begin
    FScope.Cancel;
  end;
end;

procedure TCellActions.Disconnect;
var
  LFactory: INyxCollectionCellPresenterFactory;
  LFailure: TCellCleanupFailure;
begin
  Revoke;
  LFactory := FFactory;
  FFactory := nil;
  FWriter := nil;
  FExporter := nil;
  FScheduler := nil;
  LFailure := Default(TCellCleanupFailure);
  try
    Cleanup;
  except
    {$ifdef PAS2JS}CaptureCleanup(LFailure, JSExceptValue);
    {$else}on E: Exception do CaptureCleanup(LFailure, E);{$endif}
  end;
  try

    if LFactory <> nil then
    begin
      LFactory.Revoke;
    end;
  except
    {$ifdef PAS2JS}CaptureCleanup(LFailure, JSExceptValue);
    {$else}on E: Exception do CaptureCleanup(LFailure, E);{$endif}
  end;
  RaiseCleanup(LFailure);
end;

function TCellActions.GetMenu: INyxMenu;
begin
  Result := FMenu;
end;

function TCellActions.GetInspector: INyxPopover;
begin
  Result := FInspector;
end;

function TCellActions.GetSnapshot: INyxCollectionClipboardText;
begin
  Result := FSnapshot;
end;

function TCellActions.GetLastResult: TNyxClipboardWriteResult;
begin

  if not FHasResult then
  begin
    raise ENyxCollectionClipboard.Create('Cell actions have no terminal clipboard receipt');
  end;
  Result := FResult;
end;

function TCellActions.GetIntent: Integer;
begin
  Result := 0;

  if FPort.Active and (FPort.Serial > 0) and (FPort.Serial < High(Integer)) then
  begin
    Result := FPort.Serial;
  end;
end;

function TCellActions.GetExportEnabled: Boolean;
begin
  Result := FPort.Active and (FExporter <> nil);
end;

function TCellActions.GetLastExportResult: TNyxFileSaveResult;
begin

  if not FHasExportResult then
  begin
    raise ENyxCollectionExport.Create('Cell actions have no terminal export receipt');
  end;
  Result := FExportResult;
end;

function TCellActions.GetLastExecution: INyxExecution;
begin
  Result := FLastExecution;
end;

constructor TCellActionCommand.Create(const ALaunch: INyxCollectionClipboardCommand;
  const AActions: INyxCollectionCellActions);
var
  LIntent: INyxCollectionCellActionIntent;
  LExport: INyxCollectionCellExportActions;
begin
  inherited Create;
  FLaunch := ALaunch;
  FActions := AActions;

  if (FLaunch = nil) or (FActions = nil) or
    not Supports(FActions, INyxCollectionCellActionIntent, LIntent) then
  begin
    raise ENyxCollectionClipboard.Create('Cell context requires canonical original intent authority');
  end;
  { QI into an owning local, then assign the field normally. pas2js treats an
    interface field passed directly as an out parameter as a temporary lease;
    its routine-final release would leave this field owning a released facade. }
  FIntent := LIntent;
  FOriginalIntent := FIntent.Intent;
  FWantOutput := Supports(FActions, INyxCollectionCellExportActions, LExport) and
    LExport.ExportEnabled;
end;

procedure TCellActionCommand.Prepare;
var
  LOutput: INyxCollectionClipboardOutputCapture;
begin
  FLaunch.Prepare;

  if FWantOutput and GetCurrent then
  begin

    if not Supports(FLaunch, INyxCollectionClipboardOutputCapture, LOutput) then
    begin
      raise ENyxCollectionExport.Create('Configured cell export requires original output capture');
    end;

    if GetCurrent then
    begin
      LOutput.PrepareOutput;
    end;
  end;
end;

function TCellActionCommand.GetCurrent: Boolean;
begin
  Result := not FCompleted and FLaunch.Current and (FOriginalIntent > 0) and
    (FOriginalIntent = FIntent.Intent);
end;

function TCellActionCommand.GetDispatchCurrent: Boolean;
begin
  Result := not FCompleted and FLaunch.DispatchCurrent;
end;

function TCellActionCommand.GetPayload: INyxCollectionClipboardText;
begin
  Result := FLaunch.Payload;
end;

function TCellActionCommand.Complete(AAllowDefault: Boolean): Boolean;
begin
  Result := True;

  if FCompleted then
  begin
    Exit;
  end;
  FCompleted := True;
  try

    if AAllowDefault and FLaunch.Current and (FOriginalIntent > 0) and
      (FOriginalIntent = FIntent.Intent) then
    begin
      FTransferred := FActions.OpenCell(FLaunch);
    end;
  finally

    if not FTransferred then
    begin
      FLaunch.Complete(False);
    end;
  end;
end;

procedure TCellActionCommand.Retire;
begin
  FCompleted := True;

  if not FTransferred then
  begin
    FLaunch.Retire;
  end;
end;

function NewNyxCollectionCellActions(const AFactory: INyxCollectionCellPresenterFactory;
  const AWriter: INyxCollectionClipboardCapturedWrite; const AScheduler: INyxScheduler;
  APresentation: TNyxCollectionCellPresentation;
  const AExporter: INyxCollectionExportCapturedSave): INyxCollectionCellActions;
begin
  Result := TCellActions.Create(AFactory, AWriter, AScheduler, APresentation, AExporter);
end;

function NewNyxCollectionCellActionCommand(const ALaunch: INyxCollectionClipboardCommand;
  const AActions: INyxCollectionCellActions): INyxCollectionClipboardCommand;
begin
  Result := TCellActionCommand.Create(ALaunch, AActions);
end;

end.
