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
program nyx_collection_export_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.text, nyx.bytes, nyx.state, nyx.collections,
  nyx.collections.view, nyx.collections.view.types, nyx.collections.query,
  nyx.collections.paging, nyx.collections.arrangement,
  nyx.collections.arrangement.controls, nyx.collections.clipboard,
  nyx.collections.export, nyx.collections.export.actions,
  nyx.collections.export.commands, nyx.collections.mount,
  nyx.files.save, nyx.scheduler, nyx.events
  , nyx.publication.types
  {$ifdef PAS2JS}, Web{$endif};

type
  { Readonly input facts model real epoch withdrawal without supplying file
    authority. The actual command/final fences own their original facts. }
  TInput = class(TInterfacedObject, INyxCollectionInputLifetime)
  public
    Live: Boolean;
    Admitted: Boolean;
    EpochValue: Integer;
    function GetCurrent: Boolean;
    function GetReady: Boolean;
    function GetRetired: Boolean;
    function GetEpoch: Integer;
  end;

  { Original synchronous fence is certified flags-only. Its input lease is
    detached from mounts/renderers and remains valid through retirement. }
  TFence = class(TInterfacedObject, INyxCancellationReadiness)
  private
    FInput: INyxCollectionInputLifetime;
    FEpoch: Integer;
  public
    constructor Create(const AInput: INyxCollectionInputLifetime);
    procedure CheckReady;
  end;

  { Host is borrowed only while the fixture/controller retains its interface. }
  THost = class;

  { The real managed save request keeps cancellation/inline-result behavior.
    Its only effect is a copied byte array in the injected fixture host. }
  TSave = class(TNyxFileSaveRequest)
  protected
    procedure EndHost; override;
  public
    Owner: THost;
    function Admitted: Boolean;
    procedure Finish(AOutcome: TNyxFileSaveOutcome);
  end;

  { Explicit no-effect authority exercises pending, inline and late delivery.
    No native/browser default file host or real filesystem save is installed. }
  THost = class(TInterfacedObject, INyxFileSaveHost)
  public
    Calls: Integer;
    Bytes: TNyxBytes;
    InlineReply: Boolean;
    Outcome: TNyxFileSaveOutcome;
    Last: TSave;
    LastLease: INyxFileSaveRequest;
    OnEnd: TNotifyEvent;
    function BeginSave(const AOptions: TNyxFileSaveOptions; const ABytes: TNyxBytes;
      const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
      AReply: TNyxFileSaveReply): INyxFileSaveRequest;
  end;

  { One actual store/query/page/arrangement owner. Helpers bound fluent owning
    temporaries to short calls so pas2js does not retain live owners until Run
    exits. Only the file host and cached input facts substitute platform access. }
  TFixture = class
  public
    Store: INyxCollection;
    View: INyxCollectionView;
    Output: INyxCollectionExportView;
    Input: TInput;
    InputLease: INyxCollectionInputLifetime;
    Host: THost;
    HostLease: INyxFileSaveHost;
    Scheduler: INyxScheduler;
    Events: INyxEvents;
    Commands: INyxCollectionExportCommands;
    Replies: Integer;
    ThrowReply: Boolean;
    constructor Create;
    destructor Destroy; override;
    function Fence: INyxCancellationReadiness;
    function Launch(ARows: TNyxCollectionExportRows): INyxCollectionExportCommand;
    procedure Query;
    procedure EquivalentQuery;
    procedure EmptyQuery;
    procedure Columns(AHidden: Boolean);
    procedure Nested(Sender: TObject);
    procedure Page;
    procedure Reply(const APayload: INyxCollectionExportText;
      const AResult: TNyxFileSaveResult);
  end;

  { Actual shared mount exercises original admission/suspension/refresh shields.
    It has no widgets; target control behavior is checked by the separate real
    dual-target collection controls consumer rather than inferred here. }
  TTestMount = class(TNyxCollectionMountBase)
  public
    Probe: Boolean;
    ProbeRefused: Boolean;
    procedure SuspendInput(AValue: Boolean);
  protected
    procedure RenderDataset; override;
    procedure DetachTarget; override;
  end;

  { Observer outlives the real source owners and retained detached payload.
    Disconnected public tokens prove output does not retain a store/view through
    a hidden ownership backlink on either compiler. }
  TWatch = class
  public
    procedure ViewChanged(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
    procedure StoreChanged(const AStore: INyxCollection;
      const AChanges: INyxCollectionChanges);
  end;

  { Real source semantics remain the existing store's. Only the public Busy
    getter adds a one-shot caller callback, exercising admitted alternative
    source reentry without constructing a fake view or a private owner hook. }
  TAtomicSource = class(TInterfacedObject, INyxCollection, INyxAtomicCollection)
  private
    FInner: INyxCollection;
    FAtomic: INyxAtomicCollection;
  public
    OnBusy: TNotifyEvent;
    constructor Create(const AInner: INyxCollection);
    function Snapshot: INyxCollectionSnapshot;
    function Append(const AItem: TNyxCollectionItem): INyxCollection;
    function Insert(AIndex: Integer; const AItem: TNyxCollectionItem): INyxCollection;
    function Remove(const ARef: TNyxItemRef): INyxCollection;
    function Move(const ARef: TNyxItemRef; AIndex: Integer): INyxCollection;
    function Update(const AItem: TNyxCollectionItem): INyxCollection;
    function Replace(const AItem: TNyxCollectionItem): INyxCollection;
    procedure Apply(const AEdits: array of TNyxCollectionEdit; AExpectedRevision: Integer = -1);
    procedure Assign(const ASource: INyxCollectionSnapshot; AExpectedRevision: Integer = -1);
    function Clone: INyxCollection;
    function Subscribe(AObserver: TNyxCollectionObserver;
      AValidator: TNyxCollectionValidator = nil): INyxCollectionSubscription;
    function PrepareAssign(const ASource: INyxCollectionSnapshot;
      AExpectedRevision: Integer = -1): INyxPreparedPublication;
    function SubscribePrepared(AObserver: TNyxCollectionObserver;
      AValidator, AInstallation: TNyxCollectionValidator;
      ARetirement: TNyxCollectionRetirement): INyxCollectionSubscription;
    function GetBusy: Boolean;
  end;

  { The last caller view/facet leases really live in these fields. Prepare is a
    short ownership boundary, preventing routine-wide pas2js fluent temporaries
    from accidentally keeping an entered receiver alive for the test. }
  TEntryProbe = class
  public
    View: INyxCollectionView;
    Output: INyxCollectionExportView;
    Token: INyxCollectionViewSubscription;
    Calls: Integer;
    procedure Prepare(const ASource: INyxCollection);
    procedure ReleaseFields(Sender: TObject);
    procedure Changed(const AView: INyxCollectionView; const AChanges: INyxCollectionChanges);
  end;

const
  CName: TNyxText = 'Name';
  CQuoted: TNyxText = 'Note"';
  CValue: TNyxText = 'accepted 🌙' + #0 + 'tab' + #9 + '"quoted"' + #13#10 + 'next';

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function TInput.GetCurrent: Boolean;
begin
  Result := Live;
end;

function TInput.GetReady: Boolean;
begin
  Result := Live and Admitted;
end;

function TInput.GetRetired: Boolean;
begin
  Result := not Live;
end;

function TInput.GetEpoch: Integer;
begin
  Result := EpochValue;
end;

constructor TFence.Create(const AInput: INyxCollectionInputLifetime);
begin
  inherited Create;
  FInput := AInput;
  FEpoch := AInput.Epoch;
end;

procedure TFence.CheckReady;
begin

  if not FInput.Current or not FInput.Ready or (FInput.Epoch <> FEpoch) then
  begin
    raise Exception.Create('Original export input retired');
  end;
end;

function TSave.Admitted: Boolean;
begin
  Result := Pending;
end;

procedure TSave.EndHost;
var
  LOwner: THost;
  LCallback: TNotifyEvent;
begin
  LOwner := Owner;
  Owner := nil;

  if LOwner <> nil then
  begin
    LCallback := LOwner.OnEnd;
    LOwner.OnEnd := nil;

    if Assigned(LCallback) then
    begin
      LCallback(LOwner);
    end;
  end;
end;

procedure TSave.Finish(AOutcome: TNyxFileSaveOutcome);
begin

  if AOutcome in [nfsoWritten, nfsoDownloadRequested] then
  begin
    Complete(AOutcome, 'captured.tsv', Length(FBytes));
  end
  else
  begin
    Complete(AOutcome, '', 0);
  end;
end;

function THost.BeginSave(const AOptions: TNyxFileSaveOptions;
  const ABytes: TNyxBytes; const AScheduler: INyxScheduler;
  const ACancellation: INyxCancellationScope; AReply: TNyxFileSaveReply): INyxFileSaveRequest;
var
  LSave: TSave;
begin
  LSave := TSave.Create(AOptions, ABytes, AScheduler, ACancellation, AReply);
  Result := LSave;

  if not LSave.Admitted then
  begin
    Exit;
  end;
  Inc(Calls);
  Bytes := Copy(ABytes, 0, Length(ABytes));
  Last := LSave;
  LastLease := Result;
  LSave.Owner := Self;

  if InlineReply then
  begin
    LSave.Finish(Outcome);
  end;
end;

constructor TFixture.Create;
var
  LKey: TNyxCollectionRef;
  LSpec: TNyxCollectionViewSpec;
  LOutput: INyxCollectionExportView;
begin
  inherited Create;
  LKey := NyxCollection('export-source');
  Store := NewNyxCollection(LKey, NyxCollectionSchema.Text(NyxTextField(CName), '')
    .Text(NyxTextField(CQuoted), '').Integer(NyxIntegerField('Score'), 0)
    .Boolean(NyxBooleanField('Flag'), False).Number(NyxNumberField('Rate'), 0), [
    NyxCollectionItem(NyxItem(LKey, 'a 🌙')).WithValue(NyxTextField(CName), CValue)
      .WithValue(NyxTextField(CQuoted), 'plain').WithValue(NyxIntegerField('Score'), 2)
      .WithValue(NyxBooleanField('Flag'), True).WithValue(NyxNumberField('Rate'), 1.25),
    NyxCollectionItem(NyxItem(LKey, 'b')).WithValue(NyxTextField(CName), 'B')
      .WithValue(NyxIntegerField('Score'), 1),
    NyxCollectionItem(NyxItem(LKey, 'excluded')).WithValue(NyxTextField(CName), 'not filtered')
      .WithValue(NyxIntegerField('Score'), -1)]);
  LSpec := NyxCollectionView(LKey).Column(NyxTextField(CName), 'Display name')
    .Column(NyxTextField(CQuoted), 'Display note').Column(NyxIntegerField('Score'), 'Score')
    .Column(NyxBooleanField('Flag'), 'Flag').Column(NyxNumberField('Rate'), 'Rate')
    .TableLayout(NyxTableLayout);
  View := NewNyxCollectionView(Store, LSpec, cpTable);
  Supports(View, INyxCollectionExportView, LOutput);
  Output := LOutput;
  Query;
  Page;
  Input := TInput.Create;
  InputLease := Input;
  Input.Live := True;
  Input.Admitted := True;
  Input.EpochValue := 1;
  Host := THost.Create;
  HostLease := Host;
  Host.InlineReply := True;
  Host.Outcome := nfsoWritten;
  Scheduler := NewNyxScheduler;
  Events := NewNyxEvents(Scheduler);
  Commands := NewNyxCollectionExportCommands(View, InputLease, Events, HostLease,
    Scheduler, TNyxCollectionExportOptions.Defaults, {$ifdef PAS2JS}@{$endif}Fence);
  Commands.Observe({$ifdef PAS2JS}@{$endif}Reply);
end;

destructor TFixture.Destroy;
begin

  if Commands <> nil then
  begin
    Commands.Revoke;
  end;
  Commands := nil;

  if (Host <> nil) and (Host.LastLease <> nil) then
  begin
    Host.LastLease.Cancel;
  end;

  if Host <> nil then
  begin
    Host.LastLease := nil;
  end;
  HostLease := nil;

  if Events <> nil then
  begin
    Events.Close;
  end;
  Events := nil;

  if Scheduler <> nil then
  begin
    Scheduler.Shutdown;
  end;
  Scheduler := nil;
  Output := nil;
  View := nil;
  Store := nil;
  InputLease := nil;
  inherited Destroy;
end;

procedure TFixture.Query;
begin
  View.ConfigureQuery(NyxCollectionQuery.Where(NyxWhere(NyxIntegerField('Score')).AtLeast(0))
    .OrderBy(NyxIntegerField('Score')));
end;

procedure TFixture.Page;
var
  LPages: INyxCollectionPages;
begin
  LPages := NyxCollectionPages(View);
  LPages.ConfigurePaging(NyxCollectionPaging.PageSize(1));
end;

procedure TFixture.EquivalentQuery;
begin
  View.ConfigureQuery(NyxCollectionQuery.Where(NyxWhere(NyxIntegerField('Score')).AtLeast(1))
    .OrderBy(NyxIntegerField('Score')));
end;

procedure TFixture.EmptyQuery;
begin
  View.ConfigureQuery(NyxCollectionQuery.Where(NyxWhere(NyxIntegerField('Score')).AtLeast(3)));
end;

procedure TFixture.Columns(AHidden: Boolean);
var
  LColumns: INyxCollectionColumns;
  LPreferences: TNyxColumnPreferences;
begin
  LColumns := NyxCollectionColumns(View);
  LPreferences := NyxColumnPreferences;

  if AHidden then
  begin
    LPreferences := LPreferences.WithColumn(NyxColumnPreference(
      NyxColumnIdentity(NyxTextField(CQuoted))).Visible(False));
  end;
  LColumns.ConfigureColumns(LPreferences);
end;

function TFixture.Fence: INyxCancellationReadiness;
begin
  Result := TFence.Create(InputLease);
end;

function TFixture.Launch(ARows: TNyxCollectionExportRows): INyxCollectionExportCommand;
begin
  Result := Commands.BeginExport(ARows);
  Result.Prepare;
end;

procedure TFixture.Reply(const APayload: INyxCollectionExportText;
  const AResult: TNyxFileSaveResult);
begin
  Inc(Replies);
  Check(AResult.ByteCount = APayload.ByteCount, 'terminal result accounts for the exact captured bytes');

  if ThrowReply then
  begin
    raise Exception.Create('intentional export observer failure');
  end;
end;

procedure TFixture.Nested(Sender: TObject);
var
  LCommand: INyxCollectionExportCommand;
begin
  LCommand := Launch(ncerCurrentPage);
  LCommand.Complete(True);
end;

procedure TTestMount.SuspendInput(AValue: Boolean);
begin
  SetCellEditsSuspended(AValue);
end;

procedure TTestMount.RenderDataset;
var
  LCommands: INyxCollectionExportCommands;
  LCommand: INyxCollectionExportCommand;
begin

  if Probe then
  begin
    LCommands := GetExportCommands;

    if LCommands <> nil then
    begin
      LCommand := LCommands.BeginExport(ncerCurrentPage);
      LCommand.Prepare;
      ProbeRefused := not LCommand.Current and (LCommand.Capture = nil);
      LCommand.Retire;
    end;
  end;
end;

procedure TTestMount.DetachTarget;
begin
end;

procedure MountCases(AFixture: TFixture);
var
  LObject: TTestMount;
  LMount: INyxCollectionMount;
  LExport: INyxCollectionExportMount;
  LAdmission: INyxCollectionInputAdmission;
  LInput: INyxCollectionInputLifetime;
  LGate: INyxCollectionInputGate;
  LCommands: INyxCollectionExportCommands;
  LCommand: INyxCollectionExportCommand;
  LBefore: Integer;
begin
  LGate := NewNyxCollectionInputGate;
  LObject := TTestMount.Create(AFixture.View, ncipStaged, LGate);
  LMount := LObject;
  LObject.Activate;
  Check(Supports(LMount, INyxCollectionExportMount, LExport) and
    Supports(LMount, INyxCollectionInputAdmission, LAdmission),
    'actual mount exposes optional export and original admission without changing old vtables');
  LInput := LAdmission.GetInputLifetime;
  LExport.ConfigureExport(AFixture.HostLease, AFixture.Events, AFixture.Scheduler,
    TNyxCollectionExportOptions.Defaults);
  LCommands := LExport.ExportCommands;
  LBefore := AFixture.Host.Calls;
  LCommand := LCommands.BeginExport(ncerCurrentPage);
  LCommand.Prepare;
  LCommand.Complete(True);
  Check(not LInput.Ready and (AFixture.Host.Calls = LBefore),
    'staged original mount cannot export before local and ancestor admission');
  LAdmission.InstallInput;
  Check(not LInput.Ready, 'local export installation cannot bypass a staged ancestor');
  LGate.Install;
  LCommand := LCommands.BeginExport(ncerCurrentPage);
  LCommand.Prepare;
  LCommand.Complete(True);
  Check(LInput.Ready and (AFixture.Host.Calls = LBefore + 1),
    'same actual mount exports after both original admissions');
  LObject.Probe := True;
  LMount.Refresh;
  Check(LObject.ProbeRefused and LInput.Ready,
    'render callback cannot prepare export while original refresh input shield is entered');
  LObject.Probe := False;
  LCommand := LCommands.BeginExport(ncerCurrentPage);
  LCommand.Prepare;
  LMount.SetInteraction(False, False);
  LMount.SetInteraction(True, False);
  LBefore := AFixture.Host.Calls;
  LCommand.Complete(True);
  Check(LInput.Ready and (AFixture.Host.Calls = LBefore),
    'actual disable ABA refuses an already-prepared original export');
  AFixture.Host.InlineReply := False;
  LCommand := LCommands.BeginExport(ncerCurrentPage);
  LCommand.Prepare;
  LCommand.Complete(True);
  Check(LCommands.Phase = ncepPending, 'actual mounted export retains pending host work');
  LObject.SuspendInput(True);
  Check(not LInput.Ready and (LCommands.Phase = ncepCancelled) and
    not AFixture.Host.Last.Admitted,
    'actual suspension withdraws input and detaches the exact pending save');
  LObject.SuspendInput(False);
  LMount.Disconnect;
  Check(LInput.Retired and not LInput.Ready, 'actual mount retirement is terminal for retained export facts');
  LCommand := nil;
  LCommands := nil;
  LExport := nil;
  LAdmission := nil;
  LMount := nil;
  AFixture.Host.InlineReply := True;
end;

procedure TWatch.ViewChanged(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
begin
end;

procedure TWatch.StoreChanged(const AStore: INyxCollection;
  const AChanges: INyxCollectionChanges);
begin
end;

constructor TAtomicSource.Create(const AInner: INyxCollection);
var
  LAtomic: INyxAtomicCollection;
begin
  inherited Create;
  FInner := AInner;
  Check(Supports(AInner, INyxAtomicCollection, LAtomic),
    'alternative export source delegates an actual prepared store');
  FAtomic := LAtomic;
end;

function TAtomicSource.Snapshot: INyxCollectionSnapshot;
begin
  Result := FInner.Snapshot;
end;

function TAtomicSource.Append(const AItem: TNyxCollectionItem): INyxCollection;
begin
  Result := FInner.Append(AItem);
end;

function TAtomicSource.Insert(AIndex: Integer; const AItem: TNyxCollectionItem): INyxCollection;
begin
  Result := FInner.Insert(AIndex, AItem);
end;

function TAtomicSource.Remove(const ARef: TNyxItemRef): INyxCollection;
begin
  Result := FInner.Remove(ARef);
end;

function TAtomicSource.Move(const ARef: TNyxItemRef; AIndex: Integer): INyxCollection;
begin
  Result := FInner.Move(ARef, AIndex);
end;

function TAtomicSource.Update(const AItem: TNyxCollectionItem): INyxCollection;
begin
  Result := FInner.Update(AItem);
end;

function TAtomicSource.Replace(const AItem: TNyxCollectionItem): INyxCollection;
begin
  Result := FInner.Replace(AItem);
end;

procedure TAtomicSource.Apply(const AEdits: array of TNyxCollectionEdit; AExpectedRevision: Integer);
begin
  FInner.Apply(AEdits, AExpectedRevision);
end;

procedure TAtomicSource.Assign(const ASource: INyxCollectionSnapshot; AExpectedRevision: Integer);
begin
  FInner.Assign(ASource, AExpectedRevision);
end;

function TAtomicSource.Clone: INyxCollection;
begin
  Result := FInner.Clone;
end;

function TAtomicSource.Subscribe(AObserver: TNyxCollectionObserver;
  AValidator: TNyxCollectionValidator): INyxCollectionSubscription;
begin
  Result := FInner.Subscribe(AObserver, AValidator);
end;

function TAtomicSource.PrepareAssign(const ASource: INyxCollectionSnapshot;
  AExpectedRevision: Integer): INyxPreparedPublication;
begin
  Result := FAtomic.PrepareAssign(ASource, AExpectedRevision);
end;

function TAtomicSource.SubscribePrepared(AObserver: TNyxCollectionObserver;
  AValidator, AInstallation: TNyxCollectionValidator;
  ARetirement: TNyxCollectionRetirement): INyxCollectionSubscription;
begin
  Result := FAtomic.SubscribePrepared(AObserver, AValidator, AInstallation, ARetirement);
end;

function TAtomicSource.GetBusy: Boolean;
var
  LCallback: TNotifyEvent;
begin
  LCallback := OnBusy;
  OnBusy := nil;

  if Assigned(LCallback) then
  begin
    LCallback(Self);
  end;
  Result := FAtomic.Busy;
end;

procedure TEntryProbe.Prepare(const ASource: INyxCollection);
var
  LOutput: INyxCollectionExportView;
begin
  View := NewNyxCollectionView(ASource, NyxCollectionView(ASource.Snapshot.Key)
    .Column(NyxTextField(CName), 'Name'), cpTable);
  Check(Supports(View, INyxCollectionExportView, LOutput),
    'alternative source table exposes the actual optional export view');
  Output := LOutput;
  Token := View.Subscribe({$ifdef PAS2JS}@{$endif}Changed);
end;

procedure TEntryProbe.ReleaseFields(Sender: TObject);
begin
  Inc(Calls);
  Output := nil;
  View := nil;
end;

procedure TEntryProbe.Changed(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
begin
end;

procedure ExportEntryLifetime(const AInner: INyxCollection);
var
  LSource: TAtomicSource;
  LSourceLease: INyxCollection;
  LProbe: TEntryProbe;
  LCapture: INyxCollectionExportCapture;
begin
  LSource := TAtomicSource.Create(AInner);
  LSourceLease := LSource;
  LProbe := TEntryProbe.Create;
  try
    LProbe.Prepare(LSourceLease);
    LSource.OnBusy := {$ifdef PAS2JS}@{$endif}LProbe.ReleaseFields;
    LCapture := LProbe.Output.CaptureExport;
    Check((LProbe.Calls = 1) and (LProbe.View = nil) and (LProbe.Output = nil) and
      not LProbe.Token.Connected and (LCapture.Source.Count = AInner.Snapshot.Count),
      'CaptureExport survives actual Busy field retirement then returns only detached snapshots');
    LProbe.Token := nil;
    LProbe.Prepare(LSourceLease);
    LCapture := LProbe.Output.CaptureExport;
    LSource.OnBusy := {$ifdef PAS2JS}@{$endif}LProbe.ReleaseFields;
    LProbe.Output.CheckExportCurrent(LCapture);
    Check((LProbe.Calls = 2) and (LProbe.View = nil) and (LProbe.Output = nil) and
      not LProbe.Token.Connected and (LCapture.Page.Count = AInner.Snapshot.Count),
      'CheckExportCurrent owns its complete entry past the inner PublicationReady lease');
  finally
    LSource.OnBusy := nil;

    if LProbe.Token <> nil then
    begin
      LProbe.Token.Disconnect;
    end;
    LProbe.Output := nil;
    LProbe.View := nil;
    LProbe.Free;
  end;
end;

procedure Run;
var
  LFixture: TFixture;
  LCapture: INyxCollectionExportCapture;
  LPayload: INyxCollectionExportText;
  LCommand: INyxCollectionExportCommand;
  LNext: INyxCollectionExportCommand;
  LOptions: TNyxCollectionExportOptions;
  LBytes: TNyxBytes;
  LBefore: Integer;
  LRefused: Boolean;
  LOriginal: TNyxText;
  LWatch: TWatch;
  LViewToken: INyxCollectionViewSubscription;
  LStoreToken: INyxCollectionSubscription;
begin
  LFixture := TFixture.Create;
  LWatch := TWatch.Create;
  try
    try
      LViewToken := LFixture.View.Subscribe({$ifdef PAS2JS}@{$endif}LWatch.ViewChanged);
      LStoreToken := LFixture.Store.Subscribe({$ifdef PAS2JS}@{$endif}LWatch.StoreChanged);
      LCapture := LFixture.Output.CaptureExport;
      Check((LCapture.Source.Count = 3) and (LCapture.Filtered.Count = 2) and
        (LCapture.Page.Count = 1), 'one capture has actual source, unpaged query result and page membership');
      LOptions := TNyxCollectionExportOptions.Defaults;
      LPayload := NewNyxCollectionExportText(LCapture, ncerCurrentPage, LOptions);
      Check((LPayload.RowCount = 1) and (LPayload.ItemAt(0).ID = 'b') and
        (LPayload.FieldAt(0).Kind = nskText), 'page export uses actual order and full typed provenance');
      Check(Pos('"Note"""', LPayload.Text) > 0,
        'TSV headers escape exact typed names rather than display labels');
      LPayload := NewNyxCollectionExportText(LCapture, ncerAllFiltered, LOptions.Headers(False));
      Check((LPayload.RowCount = 2) and (LPayload.ItemAt(0).ID = 'b') and
        (LPayload.ItemAt(1).ID = TNyxText('a 🌙')), 'all-filtered export includes rows outside the page in actual query order');
      Check((LPayload.CellValue(1, 0).TextValue = CValue) and
        (LPayload.CellValue(1, 3).Kind = nskBoolean) and LPayload.CellValue(1, 3).BooleanValue and
        (LPayload.CellValue(1, 4).NumberText = '1.25'), 'NUL/newline/Unicode and invariant typed scalars stay exact');
      Check(Pos('"accepted 🌙' + #0 + 'tab' + #9 + '""quoted""' + #13#10 + 'next"',
        LPayload.Text) > 0, 'RAW TSV preserves embedded control bytes and quotes without normalization');
      LOriginal := LPayload.Text;
      LBytes := LPayload.Bytes;
      LBytes[0] := 0;
      Check((LPayload.Text = LOriginal) and (LPayload.Bytes[0] <> 0),
        'callers cannot mutate published bytes through returned arrays');
      LRefused := False;
      try
        NewNyxCollectionExportText(LCapture, ncerAllFiltered,
          LOptions.Limits(TNyxCollectionClipboardLimits.Defaults.Rows(1)));
      except
        on E: ENyxCollectionExport do LRefused := True;
      end;
      Check(LRefused and (LFixture.Host.Calls = 0), 'output row budgets refuse before file authority');
      LRefused := False;
      try
        NewNyxCollectionExportText(LCapture, ncerAllFiltered,
          LOptions.Limits(TNyxCollectionClipboardLimits.Defaults.UTF8Bytes(8)));
      except
        on E: ENyxCollectionExport do LRefused := True;
      end;
      Check(LRefused and (LFixture.Host.Calls = 0), 'escaped TSV output budget refuses without an external save effect');
      LFixture.Columns(True);
      LCapture := LFixture.Output.CaptureExport;
      Check(NewNyxCollectionExportText(LCapture, ncerAllFiltered, LOptions).FieldCount = 4,
        'visible presentation export excludes the actual hidden typed field');
      Check(NewNyxCollectionExportText(LCapture, ncerAllFiltered,
        LOptions.Fields(nccfAllArrangement)).FieldCount = 5,
        'explicit complete arrangement export includes hidden typed fields');
      LFixture.Columns(False);
      LFixture.EmptyQuery;
      LCapture := LFixture.Output.CaptureExport;
      Check((LCapture.Filtered.Count = 0) and
        (NewNyxCollectionExportText(LCapture, ncerAllFiltered, LOptions.Headers(False)).ByteCount = 0),
        'actual empty filtered result is an explicitly chosen zero-byte export without headers');
      Check(NewNyxCollectionExportText(LCapture, ncerAllFiltered, LOptions).Text =
        'Name' + #9 + '"Note"""' + #9 + 'Score' + #9 + 'Flag' + #9 + 'Rate',
        'empty filtered export with headers has exact escaped fields and no trailing row terminator');
      LFixture.Query;
      LCapture := LFixture.Output.CaptureExport;
      LCommand := LFixture.Launch(ncerCurrentPage);
      LCommand.Complete(False);
      Check((LFixture.Host.Calls = 0) and (LFixture.Replies = 0), 'consumed canonical input has no file effect');
      LCommand := LFixture.Launch(ncerAllFiltered);
      LCommand.Complete(True);
      Check((LFixture.Host.Calls = 1) and (LFixture.Replies = 1) and
        (LFixture.Commands.LastPayload.RowCount = 2) and
        (LFixture.Commands.LastResult.Outcome = nfsoWritten), 'accepted command writes its original unpaged result exactly once');
      LFixture.Host.Outcome := nfsoDownloadRequested;
      LCommand := LFixture.Launch(ncerCurrentPage);
      LCommand.Complete(True);
      Check((LFixture.Commands.LastResult.Outcome = nfsoDownloadRequested) and
        (LFixture.Commands.LastPayload.RowCount = 1), 'download request remains distinct from actual written receipt');
      LBefore := LFixture.Host.Calls;
      LCommand := LFixture.Launch(ncerAllFiltered);
      Inc(LFixture.Input.EpochValue);
      LFixture.Input.Admitted := False;
      LFixture.Input.Admitted := True;
      Check(not LCommand.Current and LCommand.DispatchCurrent,
        'disable/suspension epoch ABA refuses old default without revoking canonical callback lifetime');
      LCommand.Complete(True);
      Check(LFixture.Host.Calls = LBefore, 're-enabled input cannot revive an old file command');
      LCommand := LFixture.Launch(ncerAllFiltered);
      LFixture.EquivalentQuery;
      Check(not LCommand.Current, 'query reconfiguration invalidates original capture even when membership bytes agree');
      LCommand.Complete(True);
      Check(LFixture.Host.Calls = LBefore, 'stale query capture never silently recaptures a fresh result');
      LFixture.Host.InlineReply := False;
      LCommand := LFixture.Launch(ncerCurrentPage);
      LCommand.Complete(True);
      Check(LFixture.Commands.Phase = ncepPending, 'pending save retains its exact managed receiver');
      LFixture.Commands.CancelPending;
      LBefore := LFixture.Replies;
      LFixture.Host.Last.Finish(nfsoWritten);
      Check((LFixture.Replies = LBefore) and (LFixture.Commands.Phase = ncepCancelled),
        'cancelled late completion cannot report or retarget a receiver');
      LCommand := LFixture.Launch(ncerCurrentPage);
      LCommand.Complete(True);
      LFixture.Host.OnEnd := {$ifdef PAS2JS}@{$endif}LFixture.Nested;
      LBefore := LFixture.Host.Calls;
      LNext := LFixture.Launch(ncerAllFiltered);
      LNext.Complete(True);
      Check((LFixture.Host.Calls = LBefore + 1) and
        (LFixture.Commands.LastPayload.RowCount = 1),
        'newer nested save during old host cancellation wins before outer host entry');
      LFixture.Commands.CancelPending;
      LFixture.Host.InlineReply := True;
      LFixture.Host.Outcome := nfsoWritten;
      LFixture.ThrowReply := True;
      LCommand := LFixture.Launch(ncerCurrentPage);
      LBefore := LFixture.Replies;
      LRefused := False;
      try
        LCommand.Complete(True);
      except
        on E: Exception do LRefused := E.Message = 'intentional export observer failure';
      end;
      Check(LRefused and (LFixture.Replies = LBefore + 1) and
        (LFixture.Commands.Phase = ncepCompleted), 'inline client exception propagates once after terminal completion');
      LFixture.ThrowReply := False;
      LCommand := LFixture.Launch(ncerCurrentPage);
      LNext := LFixture.Launch(ncerCurrentPage);
      LNext.Complete(True);
      LBefore := LFixture.Host.Calls;
      LCommand.Complete(True);
      Check(LFixture.Host.Calls = LBefore, 'later accepted export wins even with identical captured bytes');
      MountCases(LFixture);
      ExportEntryLifetime(LFixture.Store);
      LBefore := LFixture.Host.Calls;
      LCommand := LFixture.Launch(ncerAllFiltered);
      LFixture.Commands.Revoke;
      Check(not LCommand.Current and not LCommand.DispatchCurrent,
        'mount retirement withdraws both default and canonical entry authority');
      LCommand.Complete(True);
      Check(LFixture.Host.Calls = LBefore, 'retired command never enters file host');
      LCommand := nil;
      LNext := nil;
      Check((LPayload.Text = LOriginal) and (LPayload.RowCount = 2),
        'detached payload remains exact after source/controller retirement');
    finally
      LFixture.Free;
    end;
    Check(not LViewToken.Connected and not LStoreToken.Connected and
      (LPayload.Text = LOriginal) and (LCapture.Source.Count = 3),
      'retained capture/output keep no view/store owner and both real subscriptions retire');
  finally

    if LViewToken <> nil then
    begin
      LViewToken.Disconnect;
    end;

    if LStoreToken <> nil then
    begin
      LStoreToken.Disconnect;
    end;
    LWatch.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-collection-export-tests', IntToStr(GChecks));
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' RAW collection export checks; injected host only';
    {$else}
    WriteLn('PASS ', GChecks, ' RAW collection export checks; injected host only');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-collection-export-error', E.Message);
      document.body.textContent := 'FAIL ' + E.Message;
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
