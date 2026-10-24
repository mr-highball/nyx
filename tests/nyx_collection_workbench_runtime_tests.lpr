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
program nyx_collection_workbench_runtime_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.bytes, nyx.collections, nyx.collections.view,
  nyx.collections.view.types, nyx.collections.query, nyx.collections.paging,
  nyx.collections.arrangement, nyx.collections.clipboard, nyx.collections.clipboard.actions,
  nyx.collections.cell.actions, nyx.collections.mount, nyx.collections.workbench.policy,
  nyx.collections.workbench.runtime, nyx.collections.export, nyx.collections.export.actions,
  nyx.clipboard, nyx.clipboard.unavailable, nyx.files.save, nyx.scheduler, nyx.events,
  nyx.model, nyx.root.types, nyx.menu, nyx.popover
  {$ifdef PAS2JS}, Web{$endif};

type
  { Borrowed synchronous adversary is retained by the surrounding fixture. }
  TCall = procedure of object;

  { Real portable mount retains the original canonical input/view lifetime.
    It paints nothing; these tests establish configuration/commands, not target UI. }
  TMount = class(TNyxCollectionMountBase)
  public
    Detached: Integer;
  protected
    procedure RenderDataset; override;
    procedure DetachTarget; override;
  end;

  { No presentation is requested during configuration. Unexpected construction
    fails loudly instead of pretending a nil menu is a usable target control. }
  TFactory = class(TInterfacedObject, INyxCollectionCellPresenterFactory)
  public
    Revoked: Boolean;
    Retirements: Integer;
    function CreateMenu(ADocument: TNyxDocument; const ARoot: TNyxRootRef;
      const AItems: TNyxMenuItems): INyxMenu;
    function CreateInspector(ADocument: TNyxDocument; const ARoot: TNyxRootRef): INyxPopover;
    procedure Revoke;
  end;

  { Forward no-effect authority is borrowed while the fixture owns its lease. }
  TSaveHost = class;

  { Actual shared save request provides inline/pending cancellation semantics;
    only copied bytes and counters are affected, never files or downloads. }
  TSave = class(TNyxFileSaveRequest)
  private
    FOwner: TSaveHost;
  protected
    procedure EndHost; override;
  public
    procedure Start(AOwner: TSaveHost);
    procedure Finish(AOutcome: TNyxFileSaveOutcome);
  end;

  { Explicit no-effect file host records exact options/bytes and can return
    distinct delivered, cancelled or unavailable results through the real base. }
  TSaveHost = class(TInterfacedObject, INyxFileSaveHost)
  public
    Saves: Integer;
    Endings: Integer;
    Outcome: TNyxFileSaveOutcome;
    InlineCompletion: Boolean;
    Bytes: TNyxBytes;
    Options: TNyxFileSaveOptions;
    Last: TSave;
    LastLease: INyxFileSaveRequest;
    function BeginSave(const AOptions: TNyxFileSaveOptions; const ABytes: TNyxBytes;
      const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
      AReply: TNyxFileSaveReply): INyxFileSaveRequest;
  end;

  { Scheduler delegates real execution/admission and exposes only a synchronous
    RequireUI callout. The guarded callback can fail after export installation,
    exercising actual partial configuration rather than an invented flag seam. }
  TScheduler = class(TInterfacedObject, INyxScheduler)
  public
    Inner: INyxScheduler;
    OnRequire: TCall;
    Entering: Boolean;
    Requirements: Integer;
    function GetCapabilities: TNyxSchedulerCapabilities;
    procedure RequireUI;
    procedure Admit(APolicy: TNyxExecutionPolicy);
    function Submit(const AWork: INyxWork; APolicy: TNyxExecutionPolicy): INyxExecution;
    function PostUI(const AWork: INyxWork; const AParent: INyxExecution = nil): INyxExecution;
    procedure Shutdown;
  end;

  { Foreign optional services may reenter or withdraw the caller's sole service
    lease. Getters delegate through owning locals and retain no mount/tree. }
  TForeignServices = class(TInterfacedObject, INyxTableWorkbenchServices,
    INyxTableWorkbenchExportServices)
  public
    Inner: INyxTableWorkbenchServices;
    OnHost: TCall;
    OnOptions: TCall;
    destructor Destroy; override;
    function GetClipboardHost: INyxClipboardWriteHost;
    function GetCopyLimits: TNyxCollectionClipboardLimits;
    function GetFileSaveHost: INyxFileSaveHost;
    function GetExportOptions: TNyxCollectionExportOptions;
  end;

  { Fixture owns borrowed callbacks, actual model/mount/events and explicit
    injected hosts. Every pending request is cancelled before receivers retire. }
  TFixture = class
  public
    Store: INyxCollection;
    View: INyxCollectionView;
    ObjectMount: TMount;
    Mount: INyxCollectionMount;
    Factory: TFactory;
    FactoryLease: INyxCollectionCellPresenterFactory;
    SaveHost: TSaveHost;
    SaveLease: INyxFileSaveHost;
    Scheduler: TScheduler;
    SchedulerLease: INyxScheduler;
    Events: INyxEvents;
    Services: INyxTableWorkbenchServices;
    Policy: TNyxTableWorkbenchPolicy;
    ExportOptions: TNyxCollectionExportOptions;
    Replies: Integer;
    LastOutcome: TNyxFileSaveOutcome;
    LastPayload: INyxCollectionExportText;
    SawPartialExport: Boolean;
    SawSchedulerABA: Boolean;
    PartialExport: INyxCollectionExportCommands;
    constructor Create(AStaged: Boolean = False);
    destructor Destroy; override;
    procedure Configure;
    function ExportCommands: INyxCollectionExportCommands;
    function Clipboard: INyxCollectionClipboardCommands;
    function CellExports: INyxCollectionCellExportActions;
    procedure Receive(const APayload: INyxCollectionExportText; const AResult: TNyxFileSaveResult);
    procedure WithdrawServices;
    procedure RetireMount;
    procedure DisableABA;
    procedure DisableDuringClipboard;
    procedure FailAfterExport;
    procedure ForeignServices(AHostCallback, AOptionsCallback: TCall);
  end;

const
  CLong: TNyxText = 'target 🌙' + #0 + 'line' + #13#10 + 'next';
  CFileName: TNyxText = 'trace 🌙.tsv';

var
  GChecks: Integer;
  GForeignRetirements: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxCollectionExport.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure TMount.RenderDataset;
begin
end;

procedure TMount.DetachTarget;
begin
  Inc(Detached);
end;

function TFactory.CreateMenu(ADocument: TNyxDocument; const ARoot: TNyxRootRef;
  const AItems: TNyxMenuItems): INyxMenu;
begin
  Result := nil;
  raise ENyxCollectionClipboard.Create('Pure services fixture cannot create target menus');
end;

function TFactory.CreateInspector(ADocument: TNyxDocument; const ARoot: TNyxRootRef): INyxPopover;
begin
  Result := nil;
  raise ENyxCollectionClipboard.Create('Pure services fixture cannot create target inspectors');
end;

procedure TFactory.Revoke;
begin

  if not Revoked then
  begin
    Revoked := True;
    Inc(Retirements);
  end;
end;

procedure TSave.Start(AOwner: TSaveHost);
begin
  FOwner := AOwner;
end;

procedure TSave.EndHost;
var
  LOwner: TSaveHost;
begin
  LOwner := FOwner;
  FOwner := nil;

  if LOwner <> nil then
  begin
    Inc(LOwner.Endings);
  end;
end;

procedure TSave.Finish(AOutcome: TNyxFileSaveOutcome);
begin

  if AOutcome in [nfsoWritten, nfsoDownloadRequested] then
  begin
    Complete(AOutcome, FOptions.SuggestedName, Length(FBytes));
  end
  else if AOutcome = nfsoUnavailable then
  begin
    Complete(AOutcome, FOptions.SuggestedName, 0, 'injected unavailable save host');
  end
  else
  begin
    Complete(AOutcome, FOptions.SuggestedName, 0);
  end;
end;

function TSaveHost.BeginSave(const AOptions: TNyxFileSaveOptions; const ABytes: TNyxBytes;
  const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
  AReply: TNyxFileSaveReply): INyxFileSaveRequest;
var
  LSave: TSave;
begin
  LSave := TSave.Create(AOptions, ABytes, AScheduler, ACancellation, AReply);
  Result := LSave;
  LastLease := Result;
  Last := LSave;
  LSave.Start(Self);
  Bytes := Copy(ABytes, 0, Length(ABytes));
  Options := AOptions.Copy;
  Inc(Saves);

  if InlineCompletion then
  begin
    LSave.Finish(Outcome);
  end;
end;

function TScheduler.GetCapabilities: TNyxSchedulerCapabilities;
begin
  Result := Inner.Capabilities;
end;

procedure TScheduler.RequireUI;
var
  LCall: TCall;
begin
  Inner.RequireUI;
  Inc(Requirements);

  if Entering then
  begin
    Exit;
  end;
  LCall := OnRequire;

  if Assigned(LCall) then
  begin
    Entering := True;
    try
      LCall;
    finally
      Entering := False;
    end;
  end;
end;

procedure TScheduler.Admit(APolicy: TNyxExecutionPolicy);
begin
  Inner.Admit(APolicy);
end;

function TScheduler.Submit(const AWork: INyxWork; APolicy: TNyxExecutionPolicy): INyxExecution;
begin
  Result := Inner.Submit(AWork, APolicy);
end;

function TScheduler.PostUI(const AWork: INyxWork; const AParent: INyxExecution): INyxExecution;
begin
  Result := Inner.PostUI(AWork, AParent);
end;

procedure TScheduler.Shutdown;
begin
  OnRequire := nil;
  Inner.Shutdown;
end;

destructor TForeignServices.Destroy;
begin
  Inc(GForeignRetirements);
  inherited Destroy;
end;

function TForeignServices.GetClipboardHost: INyxClipboardWriteHost;
begin
  Result := Inner.ClipboardHost;
end;

function TForeignServices.GetCopyLimits: TNyxCollectionClipboardLimits;
begin
  Result := Inner.CopyLimits;
end;

function TForeignServices.GetFileSaveHost: INyxFileSaveHost;
var
  LExport: INyxTableWorkbenchExportServices;
begin
  Supports(Inner, INyxTableWorkbenchExportServices, LExport);

  if Assigned(OnHost) then
  begin
    OnHost;
  end;
  Result := LExport.FileSaveHost;
end;

function TForeignServices.GetExportOptions: TNyxCollectionExportOptions;
var
  LExport: INyxTableWorkbenchExportServices;
begin
  Supports(Inner, INyxTableWorkbenchExportServices, LExport);

  if Assigned(OnOptions) then
  begin
    OnOptions;
  end;
  Result := LExport.ExportOptions;
end;

constructor TFixture.Create(AStaged: Boolean);
var
  LKey: TNyxCollectionRef;
  LPhase: TNyxCollectionInputPhase;
begin
  inherited Create;
  LKey := NyxCollection('runtime services 🌙');
  Store := NewNyxCollection(LKey, NyxCollectionSchema.Text(NyxTextField('Name'), '')
    .Integer(NyxIntegerField('Score'), 0), [
      NyxCollectionItem(NyxItem(LKey, 'a')).WithValue(NyxTextField('Name'), 'hidden')
        .WithValue(NyxIntegerField('Score'), 1),
      NyxCollectionItem(NyxItem(LKey, 'b')).WithValue(NyxTextField('Name'), 'B path')
        .WithValue(NyxIntegerField('Score'), 2),
      NyxCollectionItem(NyxItem(LKey, 'c 🌙')).WithValue(NyxTextField('Name'), CLong)
        .WithValue(NyxIntegerField('Score'), 3)]);
  Policy := NyxTableWorkbench.Headers(False).LineEnding(nwleCRLF);
  View := NewNyxCollectionView(Store, NyxCollectionView(LKey)
    .Column(NyxTextField('Name'), 'Name').Column(NyxIntegerField('Score'), 'Score')
    .TableWorkbench(Policy), cpTable);
  View.ConfigureQuery(NyxCollectionQuery.Where(NyxWhere(NyxIntegerField('Score')).AtLeast(2)));
  NyxCollectionPages(View).ConfigurePaging(NyxCollectionPaging.PageSize(1));
  LPhase := ncipReady;

  if AStaged then
  begin
    LPhase := ncipStaged;
  end;
  ObjectMount := TMount.Create(View, LPhase);
  Mount := ObjectMount;
  ObjectMount.Activate;
  Factory := TFactory.Create;
  FactoryLease := Factory;
  SaveHost := TSaveHost.Create;
  SaveLease := SaveHost;
  SaveHost.Outcome := nfsoWritten;
  SaveHost.InlineCompletion := True;
  Scheduler := TScheduler.Create;
  SchedulerLease := Scheduler;
  Scheduler.Inner := NewNyxScheduler;
  Events := NewNyxEvents(SchedulerLease);
  ExportOptions := TNyxCollectionExportOptions.Defaults.Headers(False)
    .LineEnding(nccleCRLF).SaveAs(NyxFileSaveOptions(CFileName).Allow('tsv'));
  Services := NewNyxTableWorkbenchServices(nil, SaveLease, ExportOptions);
end;

destructor TFixture.Destroy;
begin
  Scheduler.OnRequire := nil;
  ObjectMount.Disconnect;

  if SaveHost.LastLease <> nil then
  begin
    SaveHost.LastLease.Cancel;
  end;
  SaveHost.LastLease := nil;
  Services := nil;
  PartialExport := nil;
  Mount := nil;
  Events.Close;
  Events := nil;
  SchedulerLease.Shutdown;
  SchedulerLease := nil;
  SaveLease := nil;
  FactoryLease := nil;
  View := nil;
  Store := nil;
  inherited Destroy;
end;

procedure TFixture.Configure;
begin
  ConfigureNyxTableWorkbench(Mount, Policy, Services, FactoryLease, Events);
end;

function TFixture.ExportCommands: INyxCollectionExportCommands;
var
  LExport: INyxCollectionExportMount;
begin
  Supports(Mount, INyxCollectionExportMount, LExport);
  Result := LExport.ExportCommands;
end;

function TFixture.Clipboard: INyxCollectionClipboardCommands;
var
  LClipboard: INyxCollectionClipboardMount;
begin
  Supports(Mount, INyxCollectionClipboardMount, LClipboard);
  Result := LClipboard.ClipboardCommands;
end;

function TFixture.CellExports: INyxCollectionCellExportActions;
var
  LActions: INyxCollectionCellActionsMount;
  LCells: INyxCollectionCellActions;
begin
  Supports(Mount, INyxCollectionCellActionsMount, LActions);
  LCells := LActions.CellActions;
  Supports(LCells, INyxCollectionCellExportActions, Result);
end;

procedure TFixture.Receive(const APayload: INyxCollectionExportText; const AResult: TNyxFileSaveResult);
begin
  Inc(Replies);
  LastOutcome := AResult.Outcome;
  LastPayload := APayload;
end;

procedure TFixture.WithdrawServices;
begin
  Services := nil;
end;

procedure TFixture.RetireMount;
begin
  ObjectMount.Disconnect;
end;

procedure TFixture.DisableABA;
begin
  ObjectMount.SetInteraction(False, True);
  ObjectMount.SetInteraction(True, True);
end;

procedure TFixture.DisableDuringClipboard;
begin
  { Leave the bridge's explicit scheduler preflight alone, then reenter the
    actual command constructor admission. Its new epoch must not be adopted. }

  if Scheduler.Requirements < 2 then
  begin
    Exit;
  end;
  Scheduler.OnRequire := nil;
  SawSchedulerABA := True;
  DisableABA;
end;

procedure TFixture.FailAfterExport;
begin
  PartialExport := ExportCommands;

  if PartialExport <> nil then
  begin
    SawPartialExport := True;
    raise Exception.Create('deliberate post-export configuration refusal');
  end;
end;

procedure TFixture.ForeignServices(AHostCallback, AOptionsCallback: TCall);
var
  LProxy: TForeignServices;
begin
  LProxy := TForeignServices.Create;
  LProxy.Inner := Services;
  LProxy.OnHost := AHostCallback;
  LProxy.OnOptions := AOptionsCallback;
  Services := LProxy;
end;

procedure ServiceValues;
var
  LFixture: TFixture;
  LExport: INyxTableWorkbenchExportServices;
  LOptions: TNyxCollectionExportOptions;
  LNoExport: INyxTableWorkbenchServices;
  LPrevious: INyxTableWorkbenchServices;
  LRejected: Boolean;
begin
  LFixture := TFixture.Create;
  try
    Check(Supports(LFixture.Services, INyxTableWorkbenchExportServices, LExport),
      'explicit factory adds an optional export facet without changing clipboard service ABI');
    LFixture.ExportOptions := LFixture.ExportOptions.Headers(True).LineEnding(nccleLF)
      .SaveAs(LFixture.ExportOptions.SaveOptions.Suggest('changed.tsv').Allow('csv'));
    LOptions := LExport.ExportOptions;
    Check(not LOptions.IncludeHeaders and (LOptions.Lines = nccleCRLF) and
      (LOptions.SaveOptions.SuggestedName = CFileName) and
      (LOptions.SaveOptions.ExtensionCount = 1), 'factory copies complete original export/save values');
    LOptions := LOptions.SaveAs(LOptions.SaveOptions.Allow('other'));
    Check(LExport.ExportOptions.SaveOptions.ExtensionCount = 1,
      'returned export values cannot mutate the provider retained options');
    LNoExport := NewNyxTableWorkbenchServices;
    LExport := nil;
    Check(not Supports(LNoExport, INyxTableWorkbenchExportServices, LExport) and
      (LNoExport.ClipboardHost <> nil), 'old default factory keeps export absent and clipboard Unavailable explicit');
    LRejected := False;
    LPrevious := LNoExport;
    try
      LNoExport := NewNyxTableWorkbenchServices(nil, nil, LOptions);
    except
      on E: ENyxCollectionExport do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LNoExport = LPrevious),
      'export-enabled factory refuses absent file authority without replacing the caller previous service value');
  finally
    LExport := nil;
    LNoExport := nil;
    LPrevious := nil;
    LFixture.Free;
  end;
end;

procedure MountedExports;
var
  LFixture: TFixture;
  LExports: INyxCollectionExportCommands;
  LCommand: INyxCollectionExportCommand;
  LClipboard: INyxCollectionClipboardCommands;
  LCopy: INyxCollectionClipboardCommand;
  LOld: INyxFileSaveRequest;
  LOldObject: TSave;
  LBefore: Integer;
  LSourceJSON: TNyxText;
begin
  LFixture := TFixture.Create;
  try
    LSourceJSON := LFixture.View.Spec.ToData.ToJSON;
    LFixture.Configure;
    LExports := LFixture.ExportCommands;
    LClipboard := LFixture.Clipboard;
    Check((LExports <> nil) and (LClipboard <> nil) and LFixture.CellExports.ExportEnabled,
      'actual shared mount installs export BEFORE its managed cell actions capture the exporter');
    Check((LFixture.View.Spec.ToData.ToJSON = LSourceJSON) and (LFixture.SaveHost.Saves = 0),
      'runtime services neither rewrite saved policy nor invoke file authority during configuration');
    LExports.Observe({$ifdef PAS2JS}@{$endif}LFixture.Receive);
    LCommand := LExports.BeginExport(ncerCurrentPage);
    LCommand.Prepare;
    Check(LCommand.Current, 'configured export uses the actual current original-input command');
    LCommand.Complete(True);
    Check((LFixture.Replies = 1) and (LFixture.LastOutcome = nfsoWritten) and
      (LFixture.LastPayload.RowCount = 1) and (NyxDecodeUTF8(LFixture.SaveHost.Bytes) = 'B path' + #9 + '2'),
      'current-page export writes exact accepted headerless data through injected authority');
    LCommand := LExports.BeginExport(ncerAllFiltered);
    LCommand.Prepare;
    LCommand.Complete(True);
    Check((LFixture.LastPayload.RowCount = 2) and
      (LFixture.LastPayload.CellValue(1, 0).TextValue = CLong) and
      (LFixture.SaveHost.Options.SuggestedName = CFileName) and
      (NyxDecodeUTF8(LFixture.SaveHost.Bytes) = LFixture.LastPayload.Text),
      'all-filtered export retains exact NUL/newline/Unicode and captured save options without selected-row substitution');
    LFixture.SaveHost.Outcome := nfsoCancelled;
    LCommand := LExports.BeginExport(ncerCurrentPage);
    LCommand.Prepare;
    LCommand.Complete(True);
    Check((LFixture.LastOutcome = nfsoCancelled) and (LExports.LastResult.ByteCount = 0),
      'injected save cancellation is a typed undelivered result');
    LFixture.SaveHost.Outcome := nfsoUnavailable;
    LCommand := LExports.BeginExport(ncerCurrentPage);
    LCommand.Prepare;
    LCommand.Complete(True);
    Check((LExports.LastResult.Outcome = nfsoUnavailable) and (LExports.LastResult.ByteCount = 0),
      'unavailable file host never becomes a Written or requested-download claim');
    LFixture.SaveHost.InlineCompletion := False;
    LCommand := LExports.BeginExport(ncerCurrentPage);
    LCommand.Prepare;
    LCommand.Complete(True);
    LOld := LFixture.SaveHost.LastLease;
    LOldObject := LFixture.SaveHost.Last;
    LBefore := LFixture.Replies;
    LExports.CancelPending;
    LOldObject.Finish(nfsoWritten);
    Check((LFixture.Replies = LBefore) and (LExports.Phase = ncepCancelled),
      'pending export cancellation detaches only its real request and suppresses late host completion');
    LCopy := LClipboard.BeginCell(NyxItem(LFixture.Store.Snapshot.Key, 'b'),
      NyxColumnIdentity(NyxTextField('Name')));
    LCopy.Prepare;
    LCopy.Complete(True);
    Check((LClipboard.LastResult.Outcome = ncwoUnavailable) and (LFixture.SaveHost.Saves = 5),
      'adding file authority never grants clipboard authority or redirects a cell copy to save');
    LFixture.ObjectMount.Disconnect;
    Check(not LFixture.Mount.Connected and (LFixture.Factory.Retirements = 1),
      'ordinary retirement withdraws actual exporters and presenter factory exactly once');
  finally
    LCopy := nil;
    LClipboard := nil;
    LOld := nil;
    LCommand := nil;
    LExports := nil;
    LFixture.Free;
  end;
end;

procedure AdmissionCases;
var
  LFixture: TFixture;
  LCommand: INyxCollectionExportCommand;
  LInput: INyxCollectionInputAdmission;
  LExportMount: INyxCollectionExportMount;
  LExisting: INyxCollectionExportCommands;
  LRefused: Boolean;
  LForeignBefore: Integer;
begin
  LFixture := TFixture.Create(True);
  try
    LFixture.Configure;
    LCommand := LFixture.ExportCommands.BeginExport(ncerCurrentPage);
    LCommand.Prepare;
    Check(not LCommand.Current and (LCommand.Capture = nil) and (LFixture.SaveHost.Saves = 0),
      'staged authored mount configures services without admitting output before publication');
    Supports(LFixture.Mount, INyxCollectionInputAdmission, LInput);
    LInput.InstallInput;
    Check(not LCommand.Current, 'later input installation cannot revive a staged original command');
    LCommand := LFixture.ExportCommands.BeginExport(ncerCurrentPage);
    LCommand.Prepare;
    Check(LCommand.Current, 'the same staged mount admits a fresh original command after installation');
  finally
    LInput := nil;
    LCommand := nil;
    LFixture.Free;
  end;
  LFixture := TFixture.Create;
  try
    LFixture.ObjectMount.SetInteraction(False, True);
    LFixture.Configure;
    Supports(LFixture.Mount, INyxCollectionInputAdmission, LInput);
    Check(not LInput.InputReady and (LFixture.ExportCommands <> nil) and
      LFixture.CellExports.ExportEnabled and LFixture.Mount.Connected,
      'initially disabled legacy table normalizes owned flags before configuration without admitting output');
    LCommand := LFixture.ExportCommands.BeginExport(ncerCurrentPage);
    LCommand.Prepare;
    Check(not LCommand.Current and (LCommand.Capture = nil),
      'disabled authored export preserves configuration while refusing its original command');
    LFixture.ObjectMount.SetInteraction(True, True);
    Check(LInput.InputReady and not LCommand.Current,
      'enabling the configured table cannot revive a command prepared while disabled');
    LCommand := LFixture.ExportCommands.BeginExport(ncerCurrentPage);
    LCommand.Prepare;
    Check(LCommand.Current and (LFixture.SaveHost.Saves = 0),
      'fresh enabled command uses the same configured service without a premature host effect');
  finally
    LInput := nil;
    LCommand := nil;
    LFixture.Free;
  end;
  LFixture := TFixture.Create;
  try
    LFixture.Services := NewNyxTableWorkbenchServices;
    LFixture.Configure;
    Check((LFixture.ExportCommands = nil) and not LFixture.CellExports.ExportEnabled,
      'absence of the optional service keeps file commands and contextual export disabled');
  finally
    LFixture.Free;
  end;
  LFixture := TFixture.Create;
  try
    Supports(LFixture.Mount, INyxCollectionExportMount, LExportMount);
    LExportMount.ConfigureExport(LFixture.SaveLease, LFixture.Events,
      LFixture.SchedulerLease, LFixture.ExportOptions);
    LExisting := LFixture.ExportCommands;
    LFixture.Services := NewNyxTableWorkbenchServices;
    LFixture.Configure;
    Check((LFixture.ExportCommands = LExisting) and LFixture.CellExports.ExportEnabled,
      'old clipboard-only services preserve separately preconfigured explicit export authority');
  finally
    LExisting := nil;
    LExportMount := nil;
    LFixture.Free;
  end;
  LFixture := TFixture.Create;
  try
    Supports(LFixture.Mount, INyxCollectionExportMount, LExportMount);
    LExportMount.ConfigureExport(LFixture.SaveLease, LFixture.Events,
      LFixture.SchedulerLease, LFixture.ExportOptions);
    LExisting := LFixture.ExportCommands;
    LRefused := False;
    try
      LFixture.Configure;
    except
      on E: ENyxCollectionExport do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and LFixture.Mount.Connected and (LFixture.Clipboard = nil) and
      (LFixture.ExportCommands = LExisting) and (LFixture.Factory.Retirements = 0),
      'new export facet refuses an already configured export mount before altering independent existing authority');
  finally
    LExisting := nil;
    LExportMount := nil;
    LFixture.Free;
  end;
  LFixture := TFixture.Create;
  try
    LForeignBefore := GForeignRetirements;
    LFixture.ForeignServices(nil, {$ifdef PAS2JS}@{$endif}LFixture.WithdrawServices);
    LFixture.Configure;
    Check((LFixture.Services = nil) and LFixture.CellExports.ExportEnabled and
      (GForeignRetirements = LForeignBefore + 1),
      'entered bridge retains foreign services through a getter withdrawing the caller sole lease');
  finally
    LFixture.Free;
  end;
  LFixture := TFixture.Create;
  try
    LFixture.ForeignServices({$ifdef PAS2JS}@{$endif}LFixture.RetireMount, nil);
    LRefused := False;
    try
      LFixture.Configure;
    except
      on E: ENyxCollectionClipboard do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and not LFixture.Mount.Connected and (LFixture.SaveHost.Saves = 0),
      'foreign host getter cannot configure a retired original mount');
  finally
    LFixture.Free;
  end;
  LFixture := TFixture.Create;
  try
    LFixture.ForeignServices(nil, {$ifdef PAS2JS}@{$endif}LFixture.DisableABA);
    LRefused := False;
    try
      LFixture.Configure;
    except
      on E: ENyxCollectionClipboard do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and LFixture.Mount.Connected and (LFixture.Clipboard = nil) and
      (LFixture.ExportCommands = nil) and (LFixture.SaveHost.Saves = 0),
      'foreign options getter disable/enable ABA cannot recapture a newer input epoch');
  finally
    LFixture.Free;
  end;
  LFixture := TFixture.Create;
  try
    LFixture.Scheduler.OnRequire := {$ifdef PAS2JS}@{$endif}LFixture.DisableDuringClipboard;
    LRefused := False;
    try
      LFixture.Configure;
    except
      on E: ENyxCollectionClipboard do
      begin
        LRefused := True;
      end;
    end;
    LFixture.Scheduler.OnRequire := nil;
    Check(LRefused and LFixture.SawSchedulerABA and not LFixture.Mount.Connected and
      (LFixture.Factory.Retirements = 1) and (LFixture.SaveHost.Saves = 0),
      'scheduler disable/enable during real clipboard construction refuses the original epoch and retires the partial incarnation');
  finally
    LFixture.Free;
  end;
  LFixture := TFixture.Create;
  try
    LFixture.Scheduler.OnRequire := {$ifdef PAS2JS}@{$endif}LFixture.FailAfterExport;
    LRefused := False;
    try
      LFixture.Configure;
    except
      on E: Exception do
      begin
        LRefused := E.Message = 'deliberate post-export configuration refusal';
      end;
    end;
    LFixture.Scheduler.OnRequire := nil;
    Check(LRefused and LFixture.SawPartialExport and not LFixture.Mount.Connected and
      (LFixture.Factory.Retirements = 1) and (LFixture.SaveHost.Saves = 0),
      'actual post-export failure preserves original error and retires the exact partial mount/factory');
    LRefused := False;
    try
      LCommand := LFixture.PartialExport.BeginExport(ncerCurrentPage);
    except
      on E: ENyxCollectionExport do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused,
      'retained partial export controller cannot reacquire authority after its failed incarnation retires');
  finally
    LCommand := nil;
    LFixture.Free;
  end;
end;

{ Authored membership/formatting overlays retain runtime host hints and bounds.
  These real portable controllers use injected authority, not mounted target UI. }
procedure AuthoredExports;
var
  LFixture: TFixture;
  LOptions: TNyxCollectionExportOptions;
  LCommands: INyxCollectionExportCommands;
  LPermissions: INyxCollectionExportPermissions;
  LCommand: INyxCollectionExportCommand;
  LAllowed: TNyxCollectionExportMemberships;
  LRows: TNyxCollectionExportRows;
  LExportMount: INyxCollectionExportMount;
  LCase: Integer;
  LBefore: Integer;
  LRefused: Boolean;
begin
  for LCase := 0 to 4 do
  begin
    LFixture := TFixture.Create;
    try
      LFixture.Policy := NyxTableWorkbench.Export(NyxTableExport
        .Columns(nwcfAllArrangement).Headers(True).LineEnding(nwleCRLF));
      LAllowed := [ncerCurrentPage, ncerAllFiltered];
      case LCase of
        0:
          begin
            LFixture.Policy := LFixture.Policy.Export(NyxTableExport.Allow([nweaCurrentPage]));
            LAllowed := [ncerCurrentPage];
          end;
        1:
          begin
            LFixture.Policy := LFixture.Policy.Export(NyxTableExport.Allow([nweaAllFiltered]));
            LAllowed := [ncerAllFiltered];
          end;
        2:
          begin
            LFixture.Policy := LFixture.Policy.Export(NyxTableExport.Allow([]));
            LAllowed := [];
          end;
        3:
          begin
            LFixture.Services := nil;
          end;
        4:
          begin
            LFixture.ExportOptions := LFixture.ExportOptions.AllowRows([ncerCurrentPage]);
            LFixture.Services := NewNyxTableWorkbenchServices(nil,
              LFixture.SaveLease, LFixture.ExportOptions);
            LAllowed := [ncerCurrentPage];
          end;
      end;
      LOptions := NyxTableWorkbenchExportOptions(LFixture.Policy.ExportPolicy, LFixture.ExportOptions);
      Check((LOptions.AllowedRows = LAllowed) and
        (LOptions.WorkLimits.MaximumUTF8Bytes = LFixture.ExportOptions.WorkLimits.MaximumUTF8Bytes) and
        (LOptions.SaveOptions.ToData.ToJSON = LFixture.ExportOptions.SaveOptions.ToData.ToJSON),
        'authored options intersect membership authority and preserve exact runtime save hints/bounds');
      LFixture.Configure;
      LCommands := LFixture.ExportCommands;

      if LAllowed = [] then
      begin
        Check((LCommands = nil) and not LFixture.CellExports.ExportEnabled and
          (LFixture.SaveHost.Saves = 0), 'explicit empty export disables managed actions without invoking authority');
        Continue;
      end;
      Check(Supports(LCommands, INyxCollectionExportPermissions, LPermissions) and
        (LPermissions.AllowedRows = LAllowed), 'actual controller exposes only admitted membership flags');
      LCommands.Observe({$ifdef PAS2JS}@{$endif}LFixture.Receive);
      for LRows := Low(TNyxCollectionExportRows) to High(TNyxCollectionExportRows) do
      begin
        LBefore := LFixture.SaveHost.Saves;
        LRefused := False;
        try
          LCommand := LCommands.BeginExport(LRows);
          LCommand.Prepare;
          LCommand.Complete(True);
        except
          on E: ENyxCollectionExport do
          begin
            LRefused := True;
          end;
        end;
        Check(LRefused = not (LRows in LAllowed), 'denied membership refuses before capture/save authority');

        if LRows in LAllowed then
        begin

          if LCase = 3 then
          begin
            Check((LFixture.LastOutcome = nfsoUnavailable) and
              (LFixture.SaveHost.Saves = LBefore),
              'missing services give an observable typed Unavailable result, without inferred access');
          end
          else
          begin
            Check((LFixture.SaveHost.Saves = LBefore + 1) and
              (LFixture.LastOutcome = nfsoWritten) and
              (Pos('Name' + #9 + 'Score', LFixture.LastPayload.Text) = 1),
              'permitted authored export uses its own headers and the captured exact runtime host');
          end;
        end
        else
        begin
          Check(LFixture.SaveHost.Saves = LBefore, 'denied membership creates no external save request');
        end;
      end;
    finally
      LCommand := nil;
      LPermissions := nil;
      LCommands := nil;
      LFixture.Free;
    end;
  end;
  LFixture := TFixture.Create;
  try
    Check(Supports(LFixture.Mount, INyxCollectionExportMount, LExportMount),
      'preflight fixture uses the actual export-mount contract');
    LExportMount.ConfigureExport(LFixture.SaveLease, LFixture.Events,
      LFixture.Events.Scheduler, LFixture.ExportOptions);
    LFixture.Policy := LFixture.Policy.Export(NyxTableExport.Allow([]));
    LRefused := False;
    try
      LFixture.Configure;
    except
      on E: ENyxCollectionExport do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and LFixture.Mount.Connected and (LFixture.Clipboard = nil) and
      (LFixture.Factory.Retirements = 0) and (LFixture.SaveHost.Saves = 0),
      'explicit disable refuses a non-fresh exporter before partial action configuration or host effects');
  finally
    LExportMount := nil;
    LFixture.Free;
  end;
end;

begin
  try
    ServiceValues;
    AuthoredExports;
    MountedExports;
    AdmissionCases;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-workbench-runtime-services', 'passed');
    document.body.setAttribute('data-workbench-runtime-service-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' workbench runtime export-service checks; no file/clipboard effects');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-workbench-runtime-services', 'failed');
      document.body.setAttribute('data-workbench-runtime-service-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
