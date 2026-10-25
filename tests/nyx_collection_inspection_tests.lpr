{ Nyx. Copyright (c) 2020 mr-highball. MIT license; see LICENSE. }
program nyx_collection_inspection_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.state, nyx.contract, nyx.collections,
  nyx.collections.view, nyx.collections.view.types, nyx.collections.query,
  nyx.collections.inspection, nyx.collections.selection
  {$ifdef PAS2JS}, Web{$endif};

type
  { Borrowed callbacks belong to the surrounding fixture. Detached observations
    retain no receiver/view; each owned token disconnects before this is freed. }
  TProbe = class
  public
    DataCalls: Integer;
    StatusCalls: Integer;
    OtherStatusCalls: Integer;
    StoreCalls: Integer;
    Reject: Boolean;
    FailStatus: Boolean;
    Last: TNyxCollectionInspectionObservation;
    procedure DataChanged(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
    procedure StatusChanged(const AObservation: TNyxCollectionInspectionObservation;
      ACause: TNyxCollectionInspectionCause);
    procedure OtherStatus(const AObservation: TNyxCollectionInspectionObservation;
      ACause: TNyxCollectionInspectionCause);
    procedure Validate(const AData: INyxCollectionSnapshot;
      const AChanges: INyxCollectionChanges);
    procedure StoreChanged(const AStore: INyxCollection;
      const AChanges: INyxCollectionChanges);
  end;

  { Two independently subscribed projections share the SAME authoritative store.
    The frame owns callbacks and tokens explicitly; paused values are not backed
    by another mutable store, queue or replacement application. }
  TFixture = class
  public
    Store: INyxCollection;
    First: INyxCollectionView;
    Peer: INyxCollectionView;
    Inspection: INyxCollectionInspection;
    Probe: TProbe;
    PeerProbe: TProbe;
    DataToken: INyxCollectionViewSubscription;
    PeerToken: INyxCollectionViewSubscription;
    StatusToken: INyxCollectionViewSubscription;
    OtherToken: INyxCollectionViewSubscription;
    ValidationToken: INyxCollectionSubscription;
    constructor Create(AProjection: TNyxCollectionProjection = cpTable);
    destructor Destroy; override;
  end;

const
  COriginal: TNyxText = 'An exact value 🌙' + #0 + #13#10 + 'with a tail';

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AMessage: TNyxText);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create(AMessage);
  end;
end;

function Key: TNyxCollectionRef;
begin
  Result := NyxCollection('inspection source');
end;

function Item(const AID: TNyxText): TNyxItemRef;
begin
  Result := NyxItem(Key, AID);
end;

procedure TProbe.DataChanged(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
begin
  Inc(DataCalls);
end;

procedure TProbe.StatusChanged(const AObservation: TNyxCollectionInspectionObservation;
  ACause: TNyxCollectionInspectionCause);
begin
  Inc(StatusCalls);
  Last := AObservation;

  if FailStatus and (ACause = ncicSourceChanged) then
  begin
    raise Exception.Create('Expected inspection observer failure');
  end;
end;

procedure TProbe.OtherStatus(const AObservation: TNyxCollectionInspectionObservation;
  ACause: TNyxCollectionInspectionCause);
begin
  Inc(OtherStatusCalls);
end;

procedure TProbe.Validate(const AData: INyxCollectionSnapshot;
  const AChanges: INyxCollectionChanges);
begin

  if Reject then
  begin
    raise ENyxCollection.Create('Expected source refusal');
  end;
end;

procedure TProbe.StoreChanged(const AStore: INyxCollection;
  const AChanges: INyxCollectionChanges);
begin
  Inc(StoreCalls);
end;

constructor TFixture.Create(AProjection: TNyxCollectionProjection);
var
  LSpec: TNyxCollectionViewSpec;
begin
  inherited Create;
  Probe := TProbe.Create;
  PeerProbe := TProbe.Create;
  Store := NewNyxCollection(Key, NyxCollectionSchema
    .Text(NyxTextField('Name'), '').Text(NyxTextField('Parent'), ''), [
      NyxCollectionItem(Item('a')).WithValue(NyxTextField('Name'), COriginal),
      NyxCollectionItem(Item('b')).WithValue(NyxTextField('Name'), 'B')
        .WithValue(NyxTextField('Parent'), 'a'),
      NyxCollectionItem(Item('c')).WithValue(NyxTextField('Name'), 'C')
        .WithValue(NyxTextField('Parent'), 'b')]);
  LSpec := NyxCollectionView(Key).Selection(nsmMultiple)
    .Column(NyxTextField('Name'), 'Name', cmEditable);

  if AProjection = cpTree then
  begin
    LSpec := LSpec.Parent(NyxTextField('Parent'));
  end;
  First := NewNyxCollectionView(Store, LSpec, AProjection);
  Peer := NewNyxCollectionView(Store, LSpec, AProjection);
  Inspection := NyxCollectionInspection(First);
  DataToken := First.Subscribe(Probe.DataChanged);
  PeerToken := Peer.Subscribe(PeerProbe.DataChanged);
  StatusToken := Inspection.SubscribeInspection(Probe.StatusChanged);
  OtherToken := Inspection.SubscribeInspection(Probe.OtherStatus);
  ValidationToken := Store.Subscribe(Probe.StoreChanged, Probe.Validate);
end;

destructor TFixture.Destroy;
begin
  DataToken := nil;
  PeerToken := nil;
  StatusToken := nil;
  OtherToken := nil;
  ValidationToken := nil;
  Inspection := nil;
  First := nil;
  Peer := nil;
  Store := nil;
  Probe.Free;
  PeerProbe.Free;
  inherited Destroy;
end;

procedure PauseCases;
var
  LFixture: TFixture;
  LBefore: INyxCollectionSnapshot;
  LStatus: TNyxCollectionInspectionObservation;
  LRetained: TNyxCollectionInspectionObservation;
  LRefused: Boolean;
  LCalls: Integer;
begin
  LFixture := TFixture.Create;
  try
    LStatus := LFixture.Inspection.CaptureInspection;
    Check(LStatus.Supported and not LStatus.Configured and
      (LStatus.Mode = ncimFollowing), 'ordinary views retain opt-in inspection defaults');
    LFixture.Inspection.ConfigureInspection(NyxInspectionPolicy);
    LFixture.First.Select(Item('b'));
    LFixture.Probe.DataCalls := 0;
    LBefore := LFixture.First.Snapshot;
    LFixture.Inspection.PauseInspection;
    LFixture.Store.Update(NyxCollectionItem(Item('a')).WithValue(NyxTextField('Name'), 'Changed'));
    LFixture.Store.Append(NyxCollectionItem(Item('d')).WithValue(NyxTextField('Name'), 'Added'));
    LStatus := LFixture.Inspection.CaptureInspection;
    LRetained := LStatus;
    Check((LFixture.First.Snapshot = LBefore) and
      (LFixture.First.CellText(Item('a'), 0) = COriginal) and
      (LFixture.First.Selection.Focus.ID = 'b'),
      'pause keeps exact displayed Unicode/NUL values, selection and snapshot identity');
    Check((LFixture.Peer.Snapshot.Count = 4) and
      (LFixture.Peer.CellText(Item('a'), 0) = 'Changed') and
      (LFixture.PeerProbe.DataCalls = 2) and (LFixture.Probe.StoreCalls = 2),
      'the same producer and independent peer continue publishing while one view is paused');
    Check((LStatus.PendingPublications = 2) and (LStatus.DisplayRevision = LBefore.Revision) and
      (LStatus.IncomingRevision = LFixture.Store.Snapshot.Revision) and
      (LFixture.Probe.DataCalls = 0) and (LStatus.RetainedIdentities = 0),
      'pending status does not masquerade as displayed data or retain intermediate snapshots');
    LRefused := False;
    try
      LFixture.First.Edit(Item('a'), 0, TNyxStateValue.FromText('Old-frame write'));
    except
      on E: ENyxCollectionInspection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LFixture.Store.Snapshot.Item(Item('a'))
      .GetValue(NyxTextField('Name')) = 'Changed'),
      'paused view edits refuse instead of overwriting newer live values');
    LFixture.First.ConfigureQuery(NyxCollectionQuery.OrderBy(NyxTextField('Name'), nsdDescending));
    Check((LFixture.First.Snapshot.Revision = LBefore.Revision) and
      (LFixture.Inspection.CaptureInspection.PendingPublications = 2),
      'query inspection uses the frozen source without silently resuming updates');
    LFixture.Inspection.ResumeInspection;
    LStatus := LFixture.Inspection.CaptureInspection;
    Check((LStatus.Mode = ncimFollowing) and (LStatus.PendingPublications = 0) and
      (LFixture.First.CellText(Item('a'), 0) = 'Changed') and
      (LFixture.First.Selection.Focus.ID = 'b') and (LStatus.MissingSelectedRows = 0),
      'deliberate resume adopts latest data and preserves a surviving inspected identity');
    Check((LRetained.Mode = ncimPaused) and (LRetained.PendingPublications = 2) and
      (LBefore.Item(Item('a')).GetValue(NyxTextField('Name')) = COriginal),
      'retained status and source captures remain detached original facts');
    LFixture.Inspection.PauseInspection;
    LFixture.Probe.Reject := True;
    LRefused := False;
    try
      LFixture.Store.Update(NyxCollectionItem(Item('b')).WithValue(NyxTextField('Name'), 'Refused'));
    except
      on E: ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    LFixture.Probe.Reject := False;
    Check(LRefused and (LFixture.Inspection.CaptureInspection.PendingPublications = 0) and
      (LFixture.Peer.CellText(Item('b'), 0) = 'B'),
      'failed source admission leaves pending state and independent peers unchanged');
    LFixture.Probe.FailStatus := True;
    LCalls := LFixture.Probe.OtherStatusCalls;
    LRefused := False;
    try
      LFixture.Store.Update(NyxCollectionItem(Item('b')).WithValue(NyxTextField('Name'), 'Accepted'));
    except
      on E: ENyxCollectionNotification do
      begin
        LRefused := True;
      end;
    end;
    LFixture.Probe.FailStatus := False;
    Check(LRefused and (LFixture.Probe.OtherStatusCalls = LCalls + 1) and
      (LFixture.Peer.CellText(Item('b'), 0) = 'Accepted') and
      (LFixture.First.CellText(Item('b'), 0) = 'B') and
      (LFixture.Inspection.CaptureInspection.PendingPublications = 1),
      'status receiver failure is postcommit and cannot suppress sibling status or peer publication');
    LFixture.Inspection.ResumeInspection;
    Check(LFixture.First.CellText(Item('b'), 0) = 'Accepted',
      'the inspection remains resumable after a failed status observer');
  finally
    LFixture.Free;
  end;
end;

procedure RemovalCases(AProjection: TNyxCollectionProjection; AOverflow: Boolean);
var
  LFixture: TFixture;
  LTree: INyxTreeHierarchy;
  LStatus: TNyxCollectionInspectionObservation;
begin
  LFixture := TFixture.Create(AProjection);
  try

    if AProjection = cpTree then
    begin
      LTree := NyxTreeHierarchy(LFixture.First);
      LTree.ExpandAll;
    end;
    LFixture.First.Select(Item('b'));

    if AOverflow then
    begin
      LFixture.Inspection.ConfigureInspection(NyxInspectionPolicy.RemovedIdentities(0));
    end
    else
    begin
      LFixture.Inspection.ConfigureInspection(NyxInspectionPolicy.RemovedIdentities(2));
    end;
    LFixture.Inspection.PauseInspection;
    LFixture.Store.Apply([
      NyxUpdate(NyxCollectionItem(Item('c')).WithValue(NyxTextField('Parent'), 'a')),
      NyxRemove(Item('b'))]);
    LFixture.Store.Append(NyxCollectionItem(Item('b')).WithValue(NyxTextField('Name'), 'New B')
      .WithValue(NyxTextField('Parent'), 'a'));
    LFixture.Store.Update(NyxCollectionItem(Item('c')).WithValue(NyxTextField('Parent'), 'b'));
    Check((LFixture.First.CellText(Item('b'), 0) = 'B') and
      (LFixture.First.Selection.Focus.ID = 'b'),
      'removed and reinserted IDs do not replace the still-readable paused row');
    LStatus := LFixture.Inspection.CaptureInspection;
    Check((LStatus.IdentityTrackingOverflow = AOverflow) and
      (LStatus.PendingPublications = 3), 'bounded removal tracking reports exact coalesced context');
    LFixture.Inspection.ResumeInspection;
    LStatus := LFixture.Inspection.CaptureInspection;
    Check((LFixture.First.Selection.Count = 0) and (LStatus.MissingSelectedRows = 1) and
      (LStatus.ContextResetOnResume = AOverflow) and
      (LFixture.First.CellText(Item('b'), 0) = 'New B'),
      'resume never lends an old selection to the replacement row incarnation');

    if AProjection = cpTree then
    begin
      Check(not LTree.IsExpanded(Item('b')),
        'a replacement tree parent never inherits the old paused disclosure');
    end;
  finally
    LTree := nil;
    LFixture.Free;
  end;
end;

procedure BudgetCases;
var
  LFixture: TFixture;
  LRefused: Boolean;
  LBefore: INyxCollectionSnapshot;
  LCalls: Integer;
  LToken: INyxCollectionViewSubscription;
begin
  LFixture := TFixture.Create(cpList);
  try
    LFixture.Inspection.ConfigureInspection(NyxInspectionPolicy.PausedBytes(0));
    LRefused := False;
    try
      LFixture.Inspection.PauseInspection;
    except
      on E: ENyxCollectionInspection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LFixture.Inspection.CaptureInspection.Mode = ncimFollowing),
      'zero retained-byte budget refuses a nonempty frame without changing delivery mode');
    LFixture.Inspection.ConfigureInspection(NyxInspectionPolicy.PausedBytes(1024 * 1024));
    LFixture.Inspection.PauseInspection;
    Check(LFixture.Inspection.CaptureInspection.Mode = ncimPaused,
      'the caller can raise the payload budget and pause an ordinary list');
    LBefore := LFixture.First.Snapshot;
    LCalls := LFixture.Probe.StatusCalls;
    LFixture.Inspection.PauseInspection;
    Check((LFixture.Probe.StatusCalls = LCalls) and
      (LFixture.First.Snapshot = LBefore), 'repeated pause is an idempotent observation');
    LRefused := False;
    try
      LFixture.Inspection.ConfigureInspection(Default(TNyxCollectionInspectionPolicy));
    except
      on E: ENyxCollectionInspection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and LFixture.Inspection.GetInspectionPolicy.Defined and
      (LFixture.First.Snapshot = LBefore),
      'disabling a paused policy refuses before abandoning its displayed frame');
    LFixture.Store.Remove(Item('c'));
    LFixture.First.Select(Item('b'));
    LFixture.Inspection.ConfigureInspection(NyxInspectionPolicy.RemovedIdentities(0));
    Check(LFixture.Inspection.CaptureInspection.IdentityTrackingOverflow,
      'lowering a populated identity budget makes conservative recovery explicit');
    LFixture.Inspection.ResumeInspection;
    Check(LFixture.Inspection.CaptureInspection.ContextResetOnResume and
      not LFixture.First.Selection.Focus.Defined,
      'discarded removal history never grants an uncertain old selection to current data');
    LFixture.Inspection.ConfigureInspection(Default(TNyxCollectionInspectionPolicy));
    Check(not LFixture.Inspection.CaptureInspection.Configured,
      'a resumed view can deliberately release its optional policy');
    LCalls := LFixture.Probe.StatusCalls;
    LFixture.Inspection.ResumeInspection;
    Check(LCalls = LFixture.Probe.StatusCalls, 'following resume is idempotent');
    LToken := LFixture.StatusToken;
    LFixture.StatusToken := nil;
    LFixture.Inspection := nil;
    LFixture.First := nil;
    Check(not LToken.Connected, 'retained weak status tokens disconnect with their last view owner');
  finally
    LFixture.Free;
  end;
end;

begin
  try
    PauseCases;
    RemovalCases(cpTable, False);
    RemovalCases(cpTable, True);
    RemovalCases(cpTree, False);
    RemovalCases(cpTree, True);
    BudgetCases;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-inspection-tests', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' collection inspection checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-inspection-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
