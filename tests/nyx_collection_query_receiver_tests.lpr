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
program nyx_collection_query_receiver_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.collections, nyx.scheduler, nyx.linq,
  nyx.linq.execution, nyx.linq.publication, nyx.publication.types
  {$ifdef PAS2JS}, Web{$endif};

type
  { A valid alternative cancellation scope deliberately omits the optional
    noncalling readiness facet. Its ordinary getter must not be consulted. }
  TLegacyScope = class(TInterfacedObject, INyxCancellationScope)
  public
    Reads: Integer;
    CancelledFlag: Boolean;
    procedure Cancel;
    function GetCancelled: Boolean;
  end;

  { Caller-owned phase counters outlive each alternative plan and subscription.
    They distinguish explicit refusal/retirement from executing unsafe phases. }
  TCapabilityProbe = class
  public
    Prepared: Integer;
    Validated: Integer;
    Installed: Integer;
    Notified: Integer;
    Retired: Integer;
  end;

  { A normal prepared publication intentionally exposes no final-readiness
    capability. Its delegated reservation is always retired, including refusal. }
  TLegacyPlan = class(TInterfacedObject, INyxPreparedPublication)
  private
    FPlan: INyxPreparedPublication;
    FProbe: TCapabilityProbe;
  public
    constructor Create(const APlan: INyxPreparedPublication; AProbe: TCapabilityProbe);
    destructor Destroy; override;
    procedure Validate;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

  { Alternative store preserves the original collection contract and delegates
    its operations to an owned built-in store; it omits atomic capabilities. }
  TLegacyStore = class(TInterfacedObject, INyxCollection)
  protected
    FInner: INyxCollection;
  public
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
  end;

  { This alternative adds ordinary prepared publication, but returns the legacy
    plan above. The receiver must refuse before any validator or installation. }
  TLegacyAtomicStore = class(TLegacyStore, INyxAtomicCollection)
  public
    Probe: TCapabilityProbe;
    function PrepareAssign(const ASource: INyxCollectionSnapshot;
      AExpectedRevision: Integer = -1): INyxPreparedPublication;
    function SubscribePrepared(AObserver: TNyxCollectionObserver;
      AValidator, AInstallation: TNyxCollectionValidator;
      ARetirement: TNyxCollectionRetirement): INyxCollectionSubscription;
    function GetBusy: Boolean;
  end;

  { A validator can revoke delivery without vetoing the candidate itself. The
    notify failure instead happens after commit and must retain that result. }
  TReceiverProbeMode = (rpmObserve, rpmClose, rpmCancel, rpmReject, rpmNotifyFail);

  { Borrowed callbacks are disconnected before this probe dies. A retained port
    outlives its receiver lease; neither test nor implementation owns a widget. }
  TProbe = class
  public
    Receiver: INyxCollectionQueryReceiver;
    Scope: INyxCancellationScope;
    Mode: TReceiverProbeMode;
    Notifications: Integer;
    procedure Validate(const AData: INyxCollectionSnapshot;
      const AChanges: INyxCollectionChanges);
    procedure Changed(const AStore: INyxCollection; const AChanges: INyxCollectionChanges);
  end;

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create('Query receiver: ' + AReason);
  end;
end;

procedure TLegacyScope.Cancel;
begin
  CancelledFlag := True;
end;

function TLegacyScope.GetCancelled: Boolean;
begin
  Inc(Reads);
  Result := CancelledFlag;
end;

constructor TLegacyPlan.Create(const APlan: INyxPreparedPublication; AProbe: TCapabilityProbe);
begin
  inherited Create;
  FPlan := APlan;
  FProbe := AProbe;
end;

destructor TLegacyPlan.Destroy;
begin
  Retire;
  inherited Destroy;
end;

procedure TLegacyPlan.Validate;
begin
  Inc(FProbe.Validated);
  FPlan.Validate;
end;

procedure TLegacyPlan.Install;
begin
  Inc(FProbe.Installed);
  FPlan.Install;
end;

procedure TLegacyPlan.Notify;
begin
  Inc(FProbe.Notified);
  FPlan.Notify;
end;

procedure TLegacyPlan.Retire;
begin

  if FPlan <> nil then
  begin
    Inc(FProbe.Retired);
    FPlan.Retire;
    FPlan := nil;
  end;
end;

constructor TLegacyStore.Create(const AInner: INyxCollection);
begin
  inherited Create;
  FInner := AInner;
end;

function TLegacyStore.Snapshot: INyxCollectionSnapshot;
begin
  Result := FInner.Snapshot;
end;

function TLegacyStore.Append(const AItem: TNyxCollectionItem): INyxCollection;
begin
  FInner.Append(AItem);
  Result := Self;
end;

function TLegacyStore.Insert(AIndex: Integer; const AItem: TNyxCollectionItem): INyxCollection;
begin
  FInner.Insert(AIndex, AItem);
  Result := Self;
end;

function TLegacyStore.Remove(const ARef: TNyxItemRef): INyxCollection;
begin
  FInner.Remove(ARef);
  Result := Self;
end;

function TLegacyStore.Move(const ARef: TNyxItemRef; AIndex: Integer): INyxCollection;
begin
  FInner.Move(ARef, AIndex);
  Result := Self;
end;

function TLegacyStore.Update(const AItem: TNyxCollectionItem): INyxCollection;
begin
  FInner.Update(AItem);
  Result := Self;
end;

function TLegacyStore.Replace(const AItem: TNyxCollectionItem): INyxCollection;
begin
  FInner.Replace(AItem);
  Result := Self;
end;

procedure TLegacyStore.Apply(const AEdits: array of TNyxCollectionEdit; AExpectedRevision: Integer);
begin
  FInner.Apply(AEdits, AExpectedRevision);
end;

procedure TLegacyStore.Assign(const ASource: INyxCollectionSnapshot; AExpectedRevision: Integer);
begin
  FInner.Assign(ASource, AExpectedRevision);
end;

function TLegacyStore.Clone: INyxCollection;
begin
  Result := TLegacyStore.Create(FInner.Clone);
end;

function TLegacyStore.Subscribe(AObserver: TNyxCollectionObserver;
  AValidator: TNyxCollectionValidator): INyxCollectionSubscription;
begin
  Result := FInner.Subscribe(AObserver, AValidator);
end;

function TLegacyAtomicStore.PrepareAssign(const ASource: INyxCollectionSnapshot;
  AExpectedRevision: Integer): INyxPreparedPublication;
var
  LAtomic: INyxAtomicCollection;
begin
  Supports(FInner, INyxAtomicCollection, LAtomic);
  Inc(Probe.Prepared);
  Result := TLegacyPlan.Create(LAtomic.PrepareAssign(ASource, AExpectedRevision), Probe);
end;

function TLegacyAtomicStore.SubscribePrepared(AObserver: TNyxCollectionObserver;
  AValidator, AInstallation: TNyxCollectionValidator;
  ARetirement: TNyxCollectionRetirement): INyxCollectionSubscription;
var
  LAtomic: INyxAtomicCollection;
begin
  Supports(FInner, INyxAtomicCollection, LAtomic);
  Result := LAtomic.SubscribePrepared(AObserver, AValidator, AInstallation, ARetirement);
end;

function TLegacyAtomicStore.GetBusy: Boolean;
var
  LAtomic: INyxAtomicCollection;
begin
  Supports(FInner, INyxAtomicCollection, LAtomic);
  Result := LAtomic.Busy;
end;

procedure CapabilityCases(const AScheduler: INyxScheduler; const AStore: INyxCollection;
  const AValues: TNyxSequenceArray<TNyxCollectionItem>);
var
  LScopeObject: TLegacyScope;
  LScope: INyxCancellationScope;
  LStore: INyxCollection;
  LAtomicObject: TLegacyAtomicStore;
  LAtomic: INyxAtomicCollection;
  LReceiver: INyxCollectionQueryReceiver;
  LBefore: INyxCollectionSnapshot;
  LProbe: TCapabilityProbe;
  LRefused: Boolean;
begin
  LProbe := TCapabilityProbe.Create;
  try
    LBefore := AStore.Snapshot;
    LScopeObject := TLegacyScope.Create;
    LScope := LScopeObject;
    LRefused := False;
    try
      LReceiver := NyxCollectionQueryReceiver(AScheduler, AStore, LScope);
    except
      on E: ENyxQuery do
      begin
        LRefused := Pos('noncalling cancellation readiness', E.Message) > 0;
      end;
    end;
    Check(LRefused and (LScopeObject.Reads = 0) and (AStore.Snapshot = LBefore),
      'alternative cancellation without final readiness refuses without calling its getter');
    LStore := TLegacyStore.Create(AStore);
    LRefused := False;
    try
      LReceiver := NyxCollectionQueryReceiver(AScheduler, LStore);
    except
      on E: ENyxQuery do
      begin
        LRefused := Pos('prepared collection publication', E.Message) > 0;
      end;
    end;
    Check(LRefused and (AStore.Snapshot = LBefore),
      'alternative store without prepared publication explicitly refuses at capture');
    LAtomicObject := TLegacyAtomicStore.Create(AStore);
    LStore := LAtomicObject;
    LAtomicObject.Probe := LProbe;
    Supports(LStore, INyxAtomicCollection, LAtomic);
    LReceiver := NyxCollectionQueryReceiver(AScheduler, LStore);
    LRefused := False;
    try
      LReceiver.Port.Publish(AValues);
    except
      on E: ENyxQuery do
      begin
        LRefused := Pos('final admission readiness', E.Message) > 0;
      end;
    end;
    Check(LRefused and (AStore.Snapshot = LBefore) and (LProbe.Prepared = 1) and
      (LProbe.Validated = 0) and (LProbe.Installed = 0) and (LProbe.Notified = 0) and
      (LProbe.Retired = 1) and not LAtomic.Busy,
      'alternative plan without readiness retires its reservation before any outward phase');
  finally
    LReceiver := nil;
    LAtomic := nil;
    LStore := nil;
    LProbe.Free;
  end;
end;

procedure TProbe.Validate(const AData: INyxCollectionSnapshot;
  const AChanges: INyxCollectionChanges);
begin
  case Mode of
    rpmClose:
      begin
        Receiver.Close;
        Receiver := nil;
      end;
    rpmCancel:
      Scope.Cancel;
    rpmReject:
      raise ENyxCollection.Create('Expected source veto');
    rpmObserve, rpmNotifyFail:
      begin
        { Ordinary admission is unchanged. }
      end;
  end;
end;

procedure TProbe.Changed(const AStore: INyxCollection;
  const AChanges: INyxCollectionChanges);
begin
  Inc(Notifications);

  if Mode = rpmNotifyFail then
  begin
    raise ENyxCollection.Create('Expected committed notification failure');
  end;
end;

procedure Run;
const
  CExact: TNyxText = 'New 🌙' + #0 + #13#10 + 'value';
var
  LScheduler: INyxScheduler;
  LStore: INyxCollection;
  LBefore: INyxCollectionSnapshot;
  LProbe: TProbe;
  LToken: INyxCollectionSubscription;
  LPort: INyxQueryResultPort<TNyxCollectionItem>;
  LValues: TNyxSequenceArray<TNyxCollectionItem>;
  LItem: TNyxItemRef;
  LRefused: Boolean;
  LDelivered: Boolean;
begin
  LProbe := TProbe.Create;
  try
    LScheduler := NewNyxScheduler;
    LItem := NyxItem(NyxCollection('receiver'), 'record');
    LStore := NewNyxCollection(LItem.Collection, NyxCollectionSchema.Text(NyxTextField('name'), ''),
      [NyxCollectionItem(LItem).WithValue(NyxTextField('name'), 'Old')]);
    LToken := LStore.Subscribe(LProbe.Changed, LProbe.Validate);
    SetLength(LValues, 1);
    LValues[0] := NyxCollectionItem(LItem).WithValue(NyxTextField('name'), CExact);
    CapabilityCases(LScheduler, LStore, LValues);
    LProbe.Scope := NewNyxCancellationScope;
    LProbe.Receiver := NyxCollectionQueryReceiver(LScheduler, LStore, LProbe.Scope);
    LPort := LProbe.Receiver.Port;
    LProbe.Mode := rpmClose;
    LBefore := LStore.Snapshot;
    LDelivered := LPort.Publish(LValues);
    Check(not LDelivered and (LStore.Snapshot = LBefore) and
      (LProbe.Notifications = 0), 'closure inside validation discards before source installation' +
      ' [delivered=' + BoolToStr(LDelivered, True) + '; same=' +
      BoolToStr(LStore.Snapshot = LBefore, True) + '; notifications=' +
      IntToStr(LProbe.Notifications) + ']');
    Check(not LPort.Publish(LValues), 'a retained closed port cannot revive its destination');
    LProbe.Scope := NewNyxCancellationScope;
    LProbe.Receiver := NyxCollectionQueryReceiver(LScheduler, LStore, LProbe.Scope);
    LPort := LProbe.Receiver.Port;
    LProbe.Mode := rpmCancel;
    Check(not LPort.Publish(LValues) and (LStore.Snapshot = LBefore) and
      (LProbe.Notifications = 0), 'generation cancelled by a validator refuses final installation');
    LProbe.Scope := NewNyxCancellationScope;
    LProbe.Receiver := NyxCollectionQueryReceiver(LScheduler, LStore, LProbe.Scope);
    LPort := LProbe.Receiver.Port;
    LProbe.Mode := rpmReject;
    LRefused := False;
    try
      LPort.Publish(LValues);
    except
      on LException: ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LStore.Snapshot = LBefore) and (LProbe.Notifications = 0),
      'ordinary validator veto retains its original atomic refusal');
    LProbe.Mode := rpmObserve;
    LValues[0] := NyxCollectionItem(LItem).WithValue(NyxTextField('unknown'), CExact);
    LRefused := False;
    try
      LPort.Publish(LValues);
    except
      on E: ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LStore.Snapshot = LBefore) and (LProbe.Notifications = 0),
      'malformed result refuses atomically before accepted source or observers change');
    LValues[0] := NyxCollectionItem(LItem).WithValue(NyxTextField('name'), CExact);
    Check(LPort.Publish(LValues) and (LStore.Snapshot.Revision = LBefore.Revision + 1) and
      (LStore.Snapshot.Item(LItem).GetValue(NyxTextField('name')) = CExact) and
      (LProbe.Notifications = 1), 'successful delivery publishes exact scalar text once');
    LRefused := False;
    try
      LPort.Publish(LValues);
    except
      on LException: ENyxQuery do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LProbe.Notifications = 1), 'the captured receiver revision cannot overwrite a newer source');
    LProbe.Receiver := NyxCollectionQueryReceiver(LScheduler, LStore, LProbe.Scope);
    LPort := LProbe.Receiver.Port;
    LValues[0] := NyxCollectionItem(LItem).WithValue(NyxTextField('name'), 'Committed');
    LProbe.Mode := rpmNotifyFail;
    LRefused := False;
    try
      LPort.Publish(LValues);
    except
      on LException: ENyxCollectionNotification do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LStore.Snapshot.Item(LItem).GetValue(NyxTextField('name')) = 'Committed') and
      (LProbe.Notifications = 2), 'notification failure reports committed data rather than pretending rollback');
  finally
    LToken := nil;
    LPort := nil;
    LProbe.Receiver := nil;
    LProbe.Free;

    if LScheduler <> nil then
    begin
      LScheduler.Shutdown;
    end;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-query-receiver-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' collection query receiver checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-query-receiver-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
