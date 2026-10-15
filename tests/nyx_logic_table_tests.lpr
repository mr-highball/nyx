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

program nyx_logic_table_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, nyx.text, nyx.data, nyx.state, nyx.collections, nyx.contract,
  nyx.data.rows, nyx.resources, nyx.resources.rows, nyx.publication,
  nyx.flow, nyx.flow.types, nyx.logic, nyx.flow.execution, nyx.scheduler,
  nyx.application.context, nyx.collections.registry, nyx.linq.execution,
  nyx.test.logic.table, nyx.model, nyx.codegen, nyx.codec, nyx.source
  {$ifdef PAS2JS}, JS, Web{$endif};

type
  TProbe = class
  public
    State: TNyxState;
    Rows: INyxCollection;
    StateToken: TNyxStateSubscription;
    RowToken: INyxCollectionSubscription;
    Reject: Boolean;
    Kill: Boolean;
    ThrowObserver: Boolean;
    StateCalls: Integer;
    RowCalls: Integer;
    ExpectedCaption: TNyxText;
    constructor Create;
    destructor Destroy; override;
    procedure ValidateState(ACandidate: TNyxState; AChanges: TNyxStateChanges);
    procedure ValidateRows(const ACandidate: INyxCollectionSnapshot;
      const AChanges: INyxCollectionChanges);
    procedure ObserveState(AState: TNyxState; AChanges: TNyxStateChanges);
    procedure ObserveRows(const ACollection: INyxCollection; const AChanges: INyxCollectionChanges);
  end;
  THost = class
  public
    State: TNyxState;
    Stores: INyxCollections;
    Scheduler: INyxScheduler;
    Generation: INyxCancellationScope;
    Lease: INyxApplicationContextLease;
    constructor Create;
    destructor Destroy; override;
    function GetState: TNyxState;
    function GetStores: INyxCollections;
    function GetGeneration: INyxCancellationScope;
  end;

var
  GChecks: Integer;
  GHost: THost;
  GRun: INyxQueryRun;
  GStage: Integer;
  GWaiting: Boolean;
  GDone: Boolean;
  GStarted: Double;

procedure Check(AValue: Boolean; const AMessage: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

procedure SourceCompanion;
var
  LDocument: TNyxDocument;
  LParsed: TNyxDocument;
  LSource: TNyxText;
  LWorkspace: TNyxSourceWorkspace;
  {$ifndef PAS2JS}LFile: TFileStream;{$endif}
begin
  LDocument := LogicTableDocument;
  LParsed := nil;
  LWorkspace := nil;
  try
    LSource := TNyxCodegen.Generate(LDocument);
    LParsed := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(LWorkspace.Render(LParsed) = LSource, 'typed Table graph source retains exact text');
    Check(TNyxCodec.Encode(LParsed) = TNyxCodec.Encode(LDocument),
      'typed graph and Table bindings reconstruct exactly');
    {$ifndef PAS2JS}
    if ParamCount = 1 then
    begin
      ForceDirectories(ParamStr(1));
      LFile := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(1)) +
        'nyx.generated.view.pas', fmCreate);
      try
        LFile.WriteBuffer(LSource[1], Length(LSource));
      finally
        LFile.Free;
      end;
    end;
    {$endif}
  finally
    LWorkspace.Free;
    LParsed.Free;
    LDocument.Free;
  end;
end;

function Mapping: TNyxDataRows;
begin
  Result := NyxDataRows.Identity(NyxResourcePath.Field('id'))
    .Text(NyxTextField('name'));
end;

function Candidate(const AName: TNyxText): INyxCollectionSnapshot;
begin
  Result := Mapping.Read(NyxArray([NyxObject([NyxField('id', NyxData('one')),
    NyxField('name', NyxData(AName))])]), NyxCollection('people'));
end;

constructor TProbe.Create;
begin
  inherited Create;
  State := TNyxState.Create;
  State.SetValue(NyxTextState('caption'), 'ready');
  Rows := NewNyxCollection(NyxCollection('people'), Mapping.Schema, []);
  ExpectedCaption := 'loaded';
  StateToken := State.Subscribe(ObserveState, ValidateState);
  RowToken := Rows.Subscribe(ObserveRows, ValidateRows);
end;

destructor TProbe.Destroy;
begin
  RowToken := nil;
  StateToken.Free;
  State.Free;
  inherited Destroy;
end;

procedure TProbe.ValidateState(ACandidate: TNyxState; AChanges: TNyxStateChanges);
begin
  Check((State.GetValue(NyxTextState('caption')) = 'ready') and (Rows.Snapshot.Count = 0),
    'state validation sees both old models');
  Check(ACandidate.ReadOnly, 'state candidate is read-only');
end;

procedure TProbe.ValidateRows(const ACandidate: INyxCollectionSnapshot;
  const AChanges: INyxCollectionChanges);
begin
  Check((State.GetValue(NyxTextState('caption')) = 'ready') and (Rows.Snapshot.Count = 0),
    'collection validation sees both old models');

  if Kill then
  begin
    FreeAndNil(State);
  end;

  if Reject then
  begin
    raise Exception.Create('rows refused');
  end;
end;

procedure TProbe.ObserveState(AState: TNyxState; AChanges: TNyxStateChanges);
begin
  Inc(StateCalls);
  Check((AState.GetValue(NyxTextState('caption')) = ExpectedCaption) and
    (Rows.Snapshot.Count = 1), 'state observer sees the complete installed frame');

  if ThrowObserver then
  begin
    raise ENyxState.Create('receiver failed after commit 🌟');
  end;
end;

procedure TProbe.ObserveRows(const ACollection: INyxCollection;
  const AChanges: INyxCollectionChanges);
begin
  Inc(RowCalls);
  Check((State.GetValue(NyxTextState('caption')) = ExpectedCaption) and
    (ACollection.Snapshot.Count = 1), 'collection observer sees the complete installed frame');
end;

procedure Shared;
var
  LRecipe: TNyxDataRows;
  LCopy: TNyxDataRows;
  LSnapshot: INyxCollectionSnapshot;
  LSource: TNyxDataValue;
  LProbe: TProbe;
  LAtomic: INyxAtomicCollection;
  LPrepared: INyxPreparedPublication;
  LOther: INyxPreparedPublication;
  LGroup: INyxPreparedPublication;
  LRejected: Boolean;
  LMode: Integer;
  LStateRevision: Integer;
  LSchema: TNyxCollectionSchema;
  LHold: INyxStateSnapshotHold;
begin
  LRecipe := NyxDataRows.Identity(NyxResourcePath.Field('id')).Text(NyxTextField('name'))
    .DecimalText(NyxTextField('rank')).Boolean(NyxBooleanField('active'));
  LCopy := LRecipe.Field('items');
  LSource := TNyxDataValue.ParseJSON('[{"id":"one","name":"Hello 🌟\u0000!","rank":9007199254740993,"active":true}]');
  LSnapshot := LRecipe.Read(LSource, NyxCollection('people'));
  Check(LSnapshot.ItemAt(0).GetValue(NyxTextField('rank')) = '9007199254740993', 'decimal text preserves numbers beyond IEEE precision');
  Check(LSnapshot.ItemAt(0).GetValue(NyxTextField('name')) = TNyxText('Hello 🌟') + #0 + TNyxText('!'),
    'data rows retain supplementary text and NUL');
  Check(LRecipe.Path.ToData.ToJSON = '[]', 'fluent row recipe preserves its baseline');
  Check(TNyxDataRows.FromData(LCopy.ToData).ToData.ToJSON = LCopy.ToData.ToJSON, 'row recipe persistence is exact');
  LSchema := NyxCollectionSchema.Boolean(NyxBooleanField('active'), False)
    .Text(NyxTextField('rank'), 'not loaded').Text(NyxTextField('name'), 'default');
  LSnapshot := LRecipe.Read(LSource, NyxCollection('people'), LSchema);
  Check(LSnapshot.Schema.SameSchema(LSchema), 'runtime schema order/defaults remain owned by the destination');

  LRecipe := NyxDataRows.Identity(NyxResourcePath.Field('id')).Number(NyxNumberField('score'));
  LSource := TNyxDataValue.ParseJSON('[{"id":"one","score":9007199254740993}]');
  LRejected := False;
  try
    LSnapshot := LRecipe.Read(LSource, NyxCollection('people'));
  except
    on E: Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'implicit numeric precision loss refuses');
  LSnapshot := LRecipe.Numbers(nrnpApproximate).Read(LSource, NyxCollection('people'));
  Check(LSnapshot.ItemAt(0).GetValue(NyxNumberField('score')) = 9007199254740992.0,
    'explicit approximate numeric projection remains available');
  LRejected := False;
  try
    LSnapshot := Mapping.Read(TNyxDataValue.ParseJSON(
      '[{"id":"same","name":"first"},{"id":"same","name":"second"}]'), NyxCollection('people'));
  except
    on E: Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'duplicate row identities refuse the entire snapshot');

  for LMode := 0 to 6 do
  begin
    LProbe := TProbe.Create;
    LPrepared := nil;
    LOther := nil;
    LGroup := nil;
    try
      LProbe.Reject := LMode = 1;
      LProbe.ThrowObserver := LMode = 2;
      LProbe.Kill := LMode in [3, 4];
      LStateRevision := LProbe.State.Revision;

      if LMode = 6 then
      begin
        LProbe.ExpectedCaption := 'ready';
      end;
      LPrepared := LProbe.State.PrepareApply([
        NyxStateValue(NyxTextState('caption'), LProbe.ExpectedCaption)], LStateRevision);
      Check(LProbe.State.Busy and (LProbe.State.GetValue(NyxTextState('caption')) = 'ready'),
        'prepared state reserves without publishing');
      LRejected := False;
      try
        LHold := LProbe.State.HoldSnapshot;
      except
        on E: Exception do
        begin
          LRejected := True;

        end;
      end;
      Check(LRejected, 'prepared state refuses overlapping snapshot capture');

      if LMode = 5 then
      begin
        LPrepared := nil;
        Check(not LProbe.State.Busy and (LProbe.State.Revision = LStateRevision),
          'dropping an abandoned preparation releases its reservation');
        Continue;
      end;
      LAtomic := nil;
      Supports(LProbe.Rows, INyxAtomicCollection, LAtomic);
      LOther := LAtomic.PrepareAssign(Candidate('Ada'), 0);
      LRejected := False;
      try

        if LMode = 4 then
        begin
          LGroup := PrepareNyxGroup([LPrepared]);
          PublishNyxGroup([LGroup, LOther]);
        end
        else
        begin
          PublishNyxGroup([LPrepared, LOther]);
        end;
      except
        on E: Exception do
        begin
          LRejected := True;

          if LMode = 2 then
          begin
            Check((E is ENyxPublicationNotification) and
              (ENyxPublicationNotification(E).ReceiverMessage =
                TNyxText('State committed; observer failed: receiver failed after commit 🌟')),
              'committed notification failure retains exact supplementary diagnostic text');
          end;
        end;
      end;

      if LMode = 2 then
      begin
        Check(LRejected, 'committed receiver failure remains an explicit error');
      end;

      if LMode in [1, 3, 4] then
      begin
        Check(LRejected and (LProbe.Rows.Snapshot.Count = 0) and
          (LProbe.StateCalls = 0) and (LProbe.RowCalls = 0),
          'rejected/revoked owner in later validation cannot partially install');

        if LMode = 1 then
        begin
          Check((LProbe.State.Revision = LStateRevision) and not LProbe.State.Busy,
            'rejected row validation releases unchanged state');
        end;
      end
      else
      begin
        Check((LProbe.Rows.Snapshot.Count = 1) and (LProbe.RowCalls = 1),
          'whole frame installs and notifies the collection once');
        Check(LRejected = (LMode = 2), 'observer failure describes committed publication');
        Check(not LProbe.State.Busy, 'successful/notification-failed frame releases state');

        if LMode = 6 then
        begin
          Check((LProbe.StateCalls = 0) and (LProbe.State.Revision = LStateRevision),
            'no-op state guards the frame without creating a notification/revision');
        end;
      end;
    finally
      LGroup := nil;
      LOther := nil;
      LPrepared := nil;
      LAtomic := nil;
      LProbe.Free;
    end;
  end;
end;

constructor THost.Create;
var
  LDefaults: INyxCollectionDefaults;
begin
  inherited Create;
  State := TNyxState.Create;
  State.SetValue(NyxTextState('caption'), 'ready');
  Scheduler := NewNyxScheduler;
  Generation := NewNyxCancellationScope;
  LDefaults := NewNyxCollectionDefaults.Define(NyxCollection('people'), Mapping.Schema, [])
    .Define(NyxCollection('mirror'), Mapping.Schema, []);
  Stores := NewNyxCollections(LDefaults);
  Lease := NewNyxApplicationContext(Scheduler, GetState, GetStores, GetGeneration);
end;

destructor THost.Destroy;
{$ifndef PAS2JS}
var
  LMonitor: INyxSchedulerMonitor;
  LUntil: QWord;
{$endif}
begin
  Lease.Close;
  Generation.Cancel;
  {$ifndef PAS2JS}
  LMonitor := Scheduler as INyxSchedulerMonitor;
  {$endif}
  Scheduler.Shutdown;
  State.Free;
  {$ifndef PAS2JS}
  { This console qualification owns its worker lifetime; product shutdown itself
    remains nonblocking. Service final UI couriers before heap finalization. }
  LUntil := GetTickCount64 + 100;
  while (GetTickCount64 < LUntil) or (LMonitor.WorkerLoad.ActiveWorkers > 0) do
  begin
    CheckSynchronize;
    Sleep(1);

    if GetTickCount64 > LUntil + 5000 then
    begin
      raise Exception.Create('Owned grouped-flow workers did not retire');
    end;
  end;
  {$endif}
  inherited Destroy;
end;

function THost.GetState: TNyxState;
begin
  Result := State;
end;

function THost.GetStores: INyxCollections;
begin
  Result := Stores;
end;

function THost.GetGeneration: INyxCancellationScope;
begin
  Result := Generation;
end;

function Clock: Double;
begin
  {$ifdef PAS2JS}Result := TJSDate.now;{$else}Result := GetTickCount64;{$endif}
end;

procedure Step;
var
  LGraph: INyxFlowGraph;
  LSource: TNyxText;
  LRows: INyxCollection;
begin

  if not GWaiting then
  begin

    if GStage = 7 then
    begin
      GDone := True;
      Exit;
    end;
    GHost := THost.Create;
    LSource := '[{"id":"one","name":"Ada"}]';

    if GStage = 1 then
    begin
      LSource := '[{"id":"one","name":123}]';
    end;
    LGraph := NewNyxFlowGraph.WithStep(NyxFlowStep('parse'), NewNyxJSONParse)
      .WithStep(NyxFlowStep('label'), NewNyxTextJoin).Build;
    if GStage = 5 then
    begin
      GRun := TNyxFlowApplication.Submit(GHost.Lease.Context, LGraph, [
        NyxFlowInput(NyxFlowEndpoint(NyxFlowStep('parse'), NyxLogicPort(nlpText)), NyxFlowText(LSource)),
        NyxFlowInput(NyxFlowEndpoint(NyxFlowStep('label'), NyxLogicPort(nlpLeft)), NyxFlowText('loaded')),
        NyxFlowInput(NyxFlowEndpoint(NyxFlowStep('label'), NyxLogicPort(nlpRight)), NyxFlowText(''))], [], [
        NyxFlowAssignRows(NyxCollection('people'),
          NyxFlowEndpoint(NyxFlowStep('parse'), NyxLogicPort(nlpJSON)), Mapping)], neAsynchronous);
    end
    else if GStage = 6 then
    begin
      GRun := TNyxFlowApplication.Submit(GHost.Lease.Context, LGraph, [
        NyxFlowInput(NyxFlowEndpoint(NyxFlowStep('parse'), NyxLogicPort(nlpText)), NyxFlowText(LSource)),
        NyxFlowInput(NyxFlowEndpoint(NyxFlowStep('label'), NyxLogicPort(nlpLeft)), NyxFlowText('loaded')),
        NyxFlowInput(NyxFlowEndpoint(NyxFlowStep('label'), NyxLogicPort(nlpRight)), NyxFlowText(''))], [
        NyxFlowAssign(NyxTextState('caption'), NyxFlowEndpoint(NyxFlowStep('label'), NyxLogicPort(nlpText)))], [
        NyxFlowAssignRows(NyxCollection('people'),
          NyxFlowEndpoint(NyxFlowStep('parse'), NyxLogicPort(nlpJSON)), Mapping),
        NyxFlowAssignRows(NyxCollection('mirror'),
          NyxFlowEndpoint(NyxFlowStep('parse'), NyxLogicPort(nlpJSON)),
          NyxDataRows.Identity(NyxResourcePath.Field('id')).Text(NyxTextField('name'),
            NyxResourcePath.Field('missing')))], neAsynchronous);
    end
    else
    begin
      GRun := TNyxFlowApplication.Submit(GHost.Lease.Context, LGraph, [
        NyxFlowInput(NyxFlowEndpoint(NyxFlowStep('parse'), NyxLogicPort(nlpText)), NyxFlowText(LSource)),
        NyxFlowInput(NyxFlowEndpoint(NyxFlowStep('label'), NyxLogicPort(nlpLeft)), NyxFlowText('loaded')),
        NyxFlowInput(NyxFlowEndpoint(NyxFlowStep('label'), NyxLogicPort(nlpRight)), NyxFlowText(''))], [
        NyxFlowAssign(NyxTextState('caption'), NyxFlowEndpoint(NyxFlowStep('label'), NyxLogicPort(nlpText)))], [
        NyxFlowAssignRows(NyxCollection('people'),
          NyxFlowEndpoint(NyxFlowStep('parse'), NyxLogicPort(nlpJSON)), Mapping)], neAsynchronous);
    end;
    GWaiting := True;

    if GStage = 2 then
    begin
      GHost.State.SetValue(NyxTextState('caption'), 'user edit');
    end;

    if GStage = 3 then
    begin
      LRows := GHost.Stores.Collection(NyxCollection('people'));
      LRows.Assign(Candidate('user row'));
    end;

    if GStage = 4 then
    begin
      GHost.Generation.Cancel;
    end;
    Exit;
  end;

  if GRun.Phase in [nqpPending, nqpComputing, nqpAwaitingUI] then
  begin
    Exit;
  end;
  LRows := GHost.Stores.Collection(NyxCollection('people'));
  case GStage of
    0:
    begin
      Check((GRun.Phase = nqpSucceeded) and (LRows.Snapshot.Count = 1) and
        (GHost.State.GetValue(NyxTextState('caption')) = 'loaded'), 'graph publishes typed rows and state as one frame');
    end;
    1:
    begin
      Check((GRun.Phase = nqpFailed) and (LRows.Snapshot.Count = 0) and
        (GHost.State.GetValue(NyxTextState('caption')) = 'ready'), 'wrong JSON field kind preserves the complete old frame');
    end;
    2: Check((GRun.Phase = nqpFailed) and (LRows.Snapshot.Count = 0) and
      (GHost.State.GetValue(NyxTextState('caption')) = 'user edit'), 'stale state refuses collection installation');
    3: Check((GRun.Phase = nqpFailed) and
      (LRows.Snapshot.ItemAt(0).GetValue(NyxTextField('name')) = 'user row') and
      (GHost.State.GetValue(NyxTextState('caption')) = 'ready'), 'stale collection refuses scalar installation');
    4: Check((GRun.Phase in [nqpCancelled, nqpDiscarded]) and (LRows.Snapshot.Count = 0) and
      (GHost.State.GetValue(NyxTextState('caption')) = 'ready'), 'retired generation changes neither model');
    5: Check((GRun.Phase = nqpSucceeded) and (LRows.Snapshot.Count = 1) and
      (GHost.State.GetValue(NyxTextState('caption')) = 'ready'),
      'rows-only publication retains scalar state and its revision');
    6:
    begin
      Check((GRun.Phase = nqpFailed) and (LRows.Snapshot.Count = 0) and
        (GHost.Stores.Collection(NyxCollection('mirror')).Snapshot.Count = 0) and
        (GHost.State.GetValue(NyxTextState('caption')) = 'ready'),
        'a bad second row recipe changes neither earlier rows nor state');
      LRows.Assign(Candidate('after refusal'));
      GHost.State.SetValue(NyxTextState('caption'), 'after refusal');
      Check((LRows.Snapshot.Count = 1) and not GHost.State.Busy,
        'partial preparation failure releases every earlier reservation');
    end;
  end;
  GRun := nil;
  FreeAndNil(GHost);
  GWaiting := False;
  Inc(GStage);
end;

{$ifdef PAS2JS}
procedure Drive;
begin
  try
    Step;

    if GDone then
    begin
      document.body.setAttribute('data-logic-table', 'passed');
      document.body.setAttribute('data-logic-table-checks', IntToStr(GChecks));
      Exit;
    end;

    if Clock - GStarted > 30000 then
    begin
      raise Exception.Create('Logic table fixture deadline ' + IntToStr(GStage));
    end;
    window.setTimeout(@Drive, 5);
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-logic-table', 'failed');
      document.body.setAttribute('data-logic-table-error', E.Message);
    end;
  end;
end;
{$endif}

begin
  try
    Shared;
    SourceCompanion;
    GStarted := Clock;
    {$ifdef PAS2JS}Drive;{$else}
    while not GDone do
    begin
      CheckSynchronize;
      Step;

      if Clock - GStarted > 30000 then
      begin
        raise Exception.Create('Logic table fixture deadline');
      end;
      Sleep(1);
    end;
    WriteLn('PASS ', GChecks, ' typed row/grouped graph checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-logic-table', 'failed');
      document.body.setAttribute('data-logic-table-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
