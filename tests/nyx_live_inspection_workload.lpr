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
program nyx_live_inspection_workload;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  {$ifdef PAS2JS}
  JS, Web, nyx.application.browser, nyx.test.keyboard.browser,
  {$else}
  Interfaces, Classes, Forms, Controls, StdCtrls, Grids, Windows,
  nyx.application.lcl, nyx.collections.lcl.grid, nyx.editing.lcl,
  {$endif}
  SysUtils, Math, nyx.text, nyx.types, nyx.behavior, nyx.model, nyx.controls,
  nyx.state, nyx.codec, nyx.codegen, nyx.events, nyx.scheduler, nyx.editing,
  nyx.collections, nyx.collections.view, nyx.collections.mount,
  nyx.collections.inspection, nyx.collections.position, nyx.collections.query,
  nyx.collections.viewport, nyx.collections.selection,
  nyx.collections.view.types, nyx.collections.workbench.runtime,
  nyx.service.monitor, nyx.live.inspection
  {$ifdef NYX_INSPECTION_WORKLOAD_GENERATED}, nyx.generated.workload{$endif};

const
  CServices = 10000;
  CBursts = 20;
  CBatchSize = 10;
  CPopulateMS = 3000;
  CUpdateP95MS = 100;
  CUpdateMaximumMS = 250;
  CActionMS = 250;
  CHeapGrowth = 128 * 1024 * 1024;
  CRetireID = 'finish-workload';
  CRetireReady = 'workload-ready-to-retire';
  CDraft: TNyxText = 'Keep 🌙 my service note';

type
  { Closed measurement phases separate immutable preparation, committed work,
    actual target turns and user actions. These are elapsed wall milliseconds;
    a frame opportunity is not a physical display or hardware input timestamp. }
  TWorkloadPhase = (wpPrepare, wpPopulate, wpUpdate, wpAdmission, wpTargets, wpTurn,
    wpFilteredUpdate, wpFilteredTurn, wpPause, wpResume,
    wpFilter, wpSort, wpScroll, wpInput, wpRetire);
  { One bounded scalar sample; no snapshot, widget or user draft is retained. }
  TWorkloadSample = record
    Phase: TWorkloadPhase;
    Ordinal: Integer;
    Milliseconds: Double;
    HeapBytes: Double;
  end;
  { Last prepared subscriber timestamps completed admission and installation.
    It observes the same actual publication without replacing data or target
    behavior. Its owner disconnects the borrowed callbacks before freeing it. }
  TPublicationProbe = class
    ValidatedAt: Double;
    InstalledAt: Double;
    procedure Validated(const AData: INyxCollectionSnapshot;
      const AChanges: INyxCollectionChanges);
    procedure Installed(const AData: INyxCollectionSnapshot;
      const AChanges: INyxCollectionChanges);
  end;
  {$ifdef PAS2JS}
  { The observing browser intentionally retains the finished application until
    this real button is clicked. The entered receiver leases itself while it
    retires its router/app; result reporting touches only the test host DOM. }
  TFinishCallback = class(TNyxEventCallback)
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;
  {$endif}

var
  GDocument: TNyxDocument;
  GDemo: TNyxLiveInspectionDemo;
  GFirst: INyxCollectionMount;
  GPeer: INyxCollectionMount;
  GStore: INyxCollection;
  GChecks: Integer;
  GSamples: array of TWorkloadSample;
  GReport: TNyxText;
  GFailedBudgets: Integer;
  GInitialHeap: Double;
  GReadyToFinish: Boolean;
  GProbe: TPublicationProbe;
  GProbeToken: INyxCollectionSubscription;
  {$ifdef PAS2JS}
  GApplication: TNyxBrowserApplication;
  GFinish: INyxEventCallback;
  GFinishToken: INyxEventSubscription;
  {$else}
  GApplication: TNyxLCLApplication;
  GFrequency: Int64;
  {$endif}

function ClockMS: Double;
{$ifndef PAS2JS}
var
  LCounter: Int64;
{$endif}
begin
  {$ifdef PAS2JS}
  Result := window.performance.now;
  {$else}
  QueryPerformanceCounter(LCounter);
  Result := LCounter * 1000.0 / GFrequency;
  {$endif}
end;

function HeapBytes: Double;
{$ifdef PAS2JS}
var
  LMemory: JSValue;
  LUsed: JSValue;
{$endif}
begin
  {$ifdef PAS2JS}
  { Chromium's optional whole-context estimate can be coarse or unavailable.
    Report -1 honestly; it is not interchangeable with native managed heap. }
  Result := -1;
  LMemory := TJSObject(window.performance)['memory'];

  if isObject(LMemory) then
  begin
    LUsed := TJSObject(LMemory)['usedJSHeapSize'];

    if isNumber(LUsed) then
    begin
      Result := Double(LUsed);
    end;
  end;
  {$else}
  Result := GetFPCHeapStatus.CurrHeapUsed;
  {$endif}
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not ACondition then
  begin
    raise ENyxCollection.Create('Live workload: ' + AReason);
  end;
end;

procedure TPublicationProbe.Validated(const AData: INyxCollectionSnapshot;
  const AChanges: INyxCollectionChanges);
begin
  ValidatedAt := ClockMS;
end;

procedure TPublicationProbe.Installed(const AData: INyxCollectionSnapshot;
  const AChanges: INyxCollectionChanges);
begin
  InstalledAt := ClockMS;
end;

procedure RecordSample(APhase: TWorkloadPhase; AOrdinal: Integer; AStarted: Double;
  ACompleted: Double = -1);
const
  CNames: array[TWorkloadPhase] of String = ('prepare', 'populate', 'update',
    'admission', 'targets-and-retire', 'turn', 'filtered-update', 'filtered-turn',
    'pause', 'resume', 'filter', 'sort', 'scroll', 'input', 'retire');
var
  LSample: TWorkloadSample;
  LFormat: TFormatSettings;
  LLine: TNyxText;
begin
  LSample.Phase := APhase;
  LSample.Ordinal := AOrdinal;

  if ACompleted < 0 then
  begin
    ACompleted := ClockMS;
  end;
  LSample.Milliseconds := ACompleted - AStarted;
  LSample.HeapBytes := HeapBytes;
  SetLength(GSamples, Length(GSamples) + 1);
  GSamples[High(GSamples)] := LSample;
  LFormat := FormatSettings;
  LFormat.DecimalSeparator := '.';
  LLine := TNyxText(CNames[APhase]) + ',' + TNyxText(IntToStr(AOrdinal)) + ',' +
    TNyxText(FloatToStr(LSample.Milliseconds, LFormat)) + ',' +
    TNyxText(FloatToStr(LSample.HeapBytes, LFormat));
  GReport := GReport + LLine + #10;
  {$ifdef PAS2JS}
  document.body.setAttribute('data-live-workload-phase', CNames[APhase]);
  document.body.setAttribute('data-live-workload-samples', GReport);
  {$else}
  WriteLn(LLine);
  Flush(Output);
  {$endif}
end;

procedure Budget(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    Inc(GFailedBudgets);
    GReport := GReport + 'BUDGET,' + AReason + #10;
    {$ifndef PAS2JS}WriteLn('BUDGET ', AReason);{$endif}
  end;
end;

procedure AssessBudgets;
var
  LPhase: TWorkloadPhase;
  LValues: array of Double;
  LSample: TWorkloadSample;
  LIndex: Integer;
  LMove: Integer;
  LValue: Double;
  LP95: Double;
  LPeak: Double;
begin
  LPeak := GInitialHeap;
  for LPhase := Low(TWorkloadPhase) to High(TWorkloadPhase) do
  begin
    LValues := nil;
    for LIndex := 0 to High(GSamples) do
    begin
      LSample := GSamples[LIndex];
      LPeak := Max(LPeak, LSample.HeapBytes);

      if LSample.Phase = LPhase then
      begin
        SetLength(LValues, Length(LValues) + 1);
        LValues[High(LValues)] := LSample.Milliseconds;
      end;
    end;
    for LIndex := 1 to High(LValues) do
    begin
      LValue := LValues[LIndex];
      LMove := LIndex - 1;
      while (LMove >= 0) and (LValues[LMove] > LValue) do
      begin
        LValues[LMove + 1] := LValues[LMove];
        Dec(LMove);
      end;
      LValues[LMove + 1] := LValue;
    end;

    if Length(LValues) = 0 then
    begin
      Continue;
    end;
    LP95 := LValues[Max(0, Ceil(Length(LValues) * 0.95) - 1)];
    case LPhase of
      wpPopulate: Budget(LValues[High(LValues)] <= CPopulateMS, 'visible population exceeds 3000ms');
      wpUpdate, wpTurn, wpFilteredUpdate, wpFilteredTurn:
      begin
        Budget(LP95 <= CUpdateP95MS, 'update/UI-turn p95 exceeds 100ms');
        Budget(LValues[High(LValues)] <= CUpdateMaximumMS, 'update/UI-turn maximum exceeds 250ms');
      end;
      wpPause, wpResume, wpFilter, wpSort, wpScroll, wpInput:
        Budget(LValues[High(LValues)] <= CActionMS, 'an input/query/scroll action exceeds 250ms');
      wpPrepare, wpAdmission, wpTargets, wpRetire:
      begin
        { Preparation and retirement are reported separately; they do not
          borrow the input/update budget or hide inside a paint sample. }
      end;
    end;
  end;
  {$ifndef PAS2JS}
  Budget(LPeak - GInitialHeap <= CHeapGrowth, 'native managed-heap growth exceeds 128MiB');
  {$endif}
end;

procedure Finish;
var
  LStarted: Double;
begin
  GReadyToFinish := False;
  LStarted := ClockMS;
  GProbeToken.Disconnect;
  GProbeToken := nil;
  FreeAndNil(GProbe);
  {$ifdef PAS2JS}

  if GFinishToken <> nil then
  begin
    GFinishToken.Cancel;
    GFinishToken := nil;
  end;
  GFinish := nil;
  {$endif}
  FreeAndNil(GDemo);
  FreeAndNil(GApplication);
  Check(not GFirst.Connected and not GPeer.Connected,
    'retirement revokes both real large-data mounts');
  Check(GStore.Snapshot.Count = CServices,
    'retained data remains readable after both targets retire');
  GFirst := nil;
  GPeer := nil;
  GStore := nil;
  FreeAndNil(GDocument);
  RecordSample(wpRetire, 0, LStarted);
  AssessBudgets;
  {$ifdef PAS2JS}
  document.body.setAttribute('data-live-workload-checks', IntToStr(GChecks));
  document.body.setAttribute('data-live-workload-budget-failures', IntToStr(GFailedBudgets));
  document.body.setAttribute('data-live-workload-samples', GReport);
  document.body.setAttribute('data-live-workload-retired', 'true');
  {$else}
  WriteLn('CHECKS ', GChecks, '; BUDGET FAILURES ', GFailedBudgets);

  if GFailedBudgets > 0 then
  begin
    ExitCode := 2;
  end;
  {$endif}
end;

{$ifdef PAS2JS}
procedure TFinishCallback.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
var
  LLease: INyxEventCallback;
begin
  LLease := Self;

  if GReadyToFinish and (AEvent.OriginID = CRetireID) and
    (AExecution <> nil) and not AExecution.Cancelled then
  begin
    Finish;
  end;
end;
{$endif}

{$ifdef PAS2JS}
function Journey: JSValue; async;
{$else}
procedure Journey;
{$endif}
var
  LSeed: INyxCollection;
  LItems: array of TNyxCollectionItem;
  LSnapshot: INyxCollectionSnapshot;
  LInspection: INyxCollectionInspection;
  LWorker: TNyxItemRef;
  LDistant: TNyxItemRef;
  LAcceptedName: TNyxText;
  LIndex: Integer;
  LBurst: Integer;
  LStarted: Double;
  LSaved: TNyxText;
  LKey: TNyxCollectionRef;
  LAtomic: INyxAtomicCollection;
  LViewport: INyxCollectionViewport;
  LReveal: INyxCollectionReveal;
  LSelectionBeforeReveal: INyxCollectionSelection;
  LWait: Integer;
  {$ifdef PAS2JS}
  LHost: TJSHTMLElement;
  LTable: TJSHTMLElement;
  LEditor: TJSHTMLInputElement;
  LCell: TJSHTMLElement;
  LDistantCell: TJSHTMLElement;
  LRevealBounds: TJSDOMRect;
  LClipBounds: TJSDOMRect;
  LHeaderBottom: Double;
  LPreviousVisibility: String;
  LClipParent: TJSHTMLElement;
  LPreviousOverflow: String;
  LPreviousHeight: String;
  LPreviousMinHeight: String;
  LPreviousMaxHeight: String;
  {$else}
  LTable: TStringGrid;
  LFace: TStringGrid;
  LPhysical: Integer;
  LReadFace: TStringGrid;
  LReadColumn: Integer;
  LEditor: TCustomEdit;
  LGenerated: TNyxText;
  LStream: TFileStream;
  LFocusedControl: TWinControl;
  LRevealBounds: TRect;
  LClipBounds: TRect;
  LPreviousVisibility: Boolean;
  LClipParent: TWinControl;
  LPreviousHeight: Integer;
  {$endif}

  {$ifdef PAS2JS}
  function PaintTurn: JSValue; async;
  {$else}
  procedure PaintTurn;
  {$endif}
  begin
    {$ifdef PAS2JS}
    Result := await(TJSPromise.resolve(TJSPromise.new(procedure(AResolve, AReject: TJSPromiseResolver)
      begin
        window.requestAnimationFrame(procedure(ATime: Double)
          begin
            window.requestAnimationFrame(procedure(ANextTime: Double)
              begin
                AResolve(Undefined);
              end);
          end);
      end)));
    {$else}
    Application.ProcessMessages;
    GApplication.Window.Update;
    CheckSynchronize(0);
    {$endif}
  end;

  procedure Click(const AID: TNyxText);
  begin
    {$ifdef PAS2JS}
    TJSHTMLElement(GApplication.View.ElementFor(AID)).click;
    {$else}
    TButton(GApplication.View.ControlFor(AID)).Click;
    {$endif}
  end;

  procedure CheckDraft;
  {$ifndef PAS2JS}
  var
    LSelection: TNyxTextSelection;
  {$endif}
  begin
    {$ifdef PAS2JS}
    Check((LEditor <> nil) and (LEditor.value = CDraft) and document.contains(LEditor) and
      (LEditor.selectionStart = 5) and (LEditor.selectionEnd = 7),
      'the same visible browser editor retains exact unfinished Unicode text and caret');
    {$else}
    LSelection := CaptureNyxLCLSelection(LEditor);
    Check(LFace.EditorMode and (LFace.Editor = LEditor) and (TNyxText(LEditor.Text) = CDraft),
      'the same native editor retains exact unfinished Unicode text');
    Check(LSelection.Defined and (LSelection.Start = 5) and (LSelection.Finish = 6),
      'the native editor retains its exact scalar caret under the large live workload');
    {$endif}
    Check(GStore.Snapshot.Item(LWorker).GetValue(NyxTextField('name')) = LAcceptedName,
      'unfinished input never overwrites accepted source data');
  end;

  {$ifndef PAS2JS}
  function FieldFace(ACanonical: Integer; out AColumn: Integer): TStringGrid;
  var
    LChild: Integer;
    LCandidate: TStringGrid;
    LTitle: TNyxText;

    function FindField(AGrid: TStringGrid): Boolean;
    var
      LColumn: Integer;
    begin
      Result := False;
      for LColumn := AGrid.FixedCols to AGrid.ColCount - 1 do
      begin

        if Pos(LTitle, TNyxText(AGrid.Cells[LColumn, 0])) = 1 then
        begin
          AColumn := LColumn;
          Exit(True);
        end;
      end;
    end;

  begin
    { An authored field can occupy a pinned child grid. Resolve its
      real header rather than sending F2 to the outer grid's spacer/PID field. }
    AColumn := -1;
    Result := LTable;
    LTitle := GFirst.View.Spec.ColumnAt(ACanonical).Title;

    if FindField(LTable) then
    begin
      Exit;
    end;
    for LChild := 0 to LTable.ControlCount - 1 do
    begin

      if (LTable.Controls[LChild] is TStringGrid) and LTable.Controls[LChild].Visible then
      begin
        LCandidate := TStringGrid(LTable.Controls[LChild]);

        if FindField(LCandidate) then
        begin
          Exit(LCandidate);
        end;
      end;
    end;
    Result := nil;
  end;
  {$endif}

  procedure BeginDraft(const AItem: TNyxItemRef);
  {$ifndef PAS2JS}
  var
    LRow: Integer;
  {$endif}
  begin
    LWorker := AItem;
    LAcceptedName := GStore.Snapshot.Item(LWorker).GetValue(NyxTextField('name'));
    LStarted := ClockMS;
    Check(GFirst.Select(LWorker), 'the exact admitted draft owner can be selected');
    {$ifdef PAS2JS}
    LTable := GApplication.View.ElementFor(ServiceTableID);
    LCell := TJSHTMLElement(LTable.querySelector('[data-nyx-item="' + LWorker.ID +
      '"] [data-nyx-column="0"]'));
    Check(LCell <> nil, 'the exact selected service cell is resident before keyboard entry');
    LCell.focus;
    LCell.dispatchEvent(NyxTestKeyboard(ntKeyDown, 'F2'));
    LEditor := TJSHTMLInputElement(LCell.querySelector('input'));
    Check(LEditor <> nil, 'F2 exposes the actual browser input');
    LEditor.value := CDraft;
    LEditor.setSelectionRange(5, 7);
    {$else}
    LTable := TStringGrid(GApplication.View.ControlFor(ServiceTableID));
    LFace := FieldFace(0, LPhysical);
    LRow := GFirst.View.Snapshot.IndexOf(LWorker) + 1;
    Check((LFace <> nil) and (LPhysical >= 0) and (LRow > 0) and
      (TNyxText(LFace.Cells[LPhysical, LRow]) = LAcceptedName),
      'the real header and independent accepted value identify the physical native field');
    LFace.Col := LPhysical;
    LFace.Row := LRow;
    LFace.SetFocus;
    SendMessage(LFace.Handle, WM_KEYDOWN, VK_F2, 0);
    LEditor := TCustomEdit(LFace.Editor);
    Check((LEditor <> nil) and LFace.EditorMode, 'F2 exposes the actual native input');
    RestoreNyxLCLInputText(LEditor, CDraft);
    SelectNyxLCLText(LEditor, NyxTextSelection(CDraft, 5, 6));
    {$endif}
    RecordSample(wpInput, 0, LStarted);
  end;

  procedure CancelDraft;
  begin
    {$ifdef PAS2JS}
    LEditor.dispatchEvent(NyxTestKeyboard(ntKeyDown, 'Escape'));
    {$else}
    SendMessage(LEditor.Handle, WM_KEYDOWN, VK_ESCAPE, 0);
    {$endif}
  end;

  {$ifdef PAS2JS}
  function UpdateBursts(APublish, ATurn: TWorkloadPhase;
    const APrefix: TNyxText): JSValue; async;
  {$else}
  procedure UpdateBursts(APublish, ATurn: TWorkloadPhase; const APrefix: TNyxText);
  {$endif}
  var
    LEdits: array of TNyxCollectionEdit;
    LTick: Integer;
    LEdit: Integer;
    LRef: TNyxItemRef;
    LBegin: Double;
    LPublish: Double;
    LTurn: Double;
  begin
    {$ifdef PAS2JS}Result := Undefined;{$endif}
    SetLength(LEdits, CBatchSize);
    for LTick := 0 to CBursts - 1 do
    begin
      for LEdit := 0 to High(LEdits) do
      begin
        LRef := NyxItem(LKey, 'worker-' + TNyxText(IntToStr(4 + LTick * CBatchSize + LEdit)));

        if LEdit = High(LEdits) then
        begin
          { The distant row stays offscreen during updates. Its eventual real
            target read must show the final value, not an old resident cache. }
          LRef := LDistant;
        end;
        LEdits[LEdit] := NyxUpdate(NyxCollectionItem(LRef)
          .WithValue(NyxTextField('notes'), APrefix + TNyxText(IntToStr(LTick))));
      end;
      LBegin := ClockMS;
      GStore.Apply(LEdits, GStore.Snapshot.Revision);
      LPublish := ClockMS;
      {$ifdef PAS2JS}await(PaintTurn);{$else}PaintTurn;{$endif}
      LTurn := ClockMS;
      RecordSample(APublish, LTick, LBegin, LPublish);
      RecordSample(wpAdmission, LTick, LBegin, GProbe.ValidatedAt);
      RecordSample(wpTargets, LTick, GProbe.InstalledAt, LPublish);
      RecordSample(ATurn, LTick, LBegin, LTurn);
      Check(LPublish >= LBegin, 'the monotonic publication clock does not move backwards');
      CheckDraft;
    end;
  end;

begin
  {$ifdef PAS2JS}Result := Undefined;{$endif}
  GReport := 'phase,ordinal,milliseconds,heap-bytes' + #10;
  {$ifdef NYX_INSPECTION_WORKLOAD_GENERATED}
  GDocument := nyx.generated.workload.BuildNyxDocument;
  {$else}
  GDocument := BuildLiveInspectionDocument;
  {$endif}
  {$ifndef PAS2JS}

  if ParamCount > 1 then
  begin
    Check(not FileExists(ParamStr(2)), 'the workload companion is a fresh source export');
    LGenerated := TNyxCodegen.Generate(GDocument, 'nyx.generated.workload');
    LStream := TFileStream.Create(ParamStr(2), fmCreate);
    try
      LStream.WriteBuffer(Pointer(LGenerated)^, Length(LGenerated));
    finally
      LStream.Free;
    end;
  end;
  {$endif}
  {$ifdef PAS2JS}
  { Added only to the test host after capturing the actual public design.
    It lets visual review precede explicit large-data target retirement. }
  GDocument.Find('services').Add(NewNyxButton(CRetireID).WithText('Finish workload and retire views'));
  GDocument.State.SetValue(NyxBooleanState(CRetireReady), False);
  GDocument.Find(CRetireID).Binds.Enabled(NyxBooleanState(CRetireReady)).Done;
  LSaved := TNyxCodec.Encode(GDocument);
  LHost := TJSHTMLElement(document.createElement('section'));
  document.body.appendChild(LHost);
  GApplication := TNyxBrowserApplication.Create;
  GApplication.View.TableWorkbenchServices := NewNyxTableWorkbenchServices;
  GApplication.Run(GDocument, LHost);
  {$else}
  LSaved := TNyxCodec.Encode(GDocument);
  GApplication := TNyxLCLApplication.Create;
  GApplication.Mount(GDocument);
  GApplication.Window.ClientWidth := StrToInt(ParamStr(1));
  GApplication.Window.ClientHeight := 900;

  if GApplication.Window.ClientWidth = 390 then
  begin
    GApplication.Window.ClientHeight := 844;
  end;
  GApplication.Window.Show;
  {$endif}
  GFirst := GApplication.View.CollectionMount(ServiceTableID);
  GPeer := GApplication.View.CollectionMount(InspectionPeerID);
  GStore := GFirst.View.Store;
  LKey := GStore.Snapshot.Key;
  LInspection := NyxCollectionInspection(GFirst.View);
  LInspection.ConfigureInspection(LInspection.GetInspectionPolicy.PausedBytes(8 * 1024 * 1024));
  GDemo := TNyxLiveInspectionDemo.Create(GApplication.Context, GApplication.View.Events,
    GFirst.View, GPeer.View, NyxCollectionPositionState(GFirst));
  {$ifdef PAS2JS}
  GFinish := TFinishCallback.Create;
  GFinishToken := GApplication.View.Events.On(NyxControlEvents(CRetireID), ntClick).Subscribe(GFinish);
  await(PaintTurn);
  {$else}PaintTurn;{$endif}
  GInitialHeap := HeapBytes;
  Check(Supports(GStore, INyxAtomicCollection, LAtomic),
    'the actual application store exposes prepared publication for phase attribution');
  GProbe := TPublicationProbe.Create;
  GProbeToken := LAtomic.SubscribePrepared(nil, {$ifdef PAS2JS}@{$endif}GProbe.Validated,
    {$ifdef PAS2JS}@{$endif}GProbe.Installed, nil);
  LStarted := ClockMS;
  SetLength(LItems, CServices);
  LSnapshot := GStore.Snapshot;
  for LIndex := 0 to 3 do
  begin
    LItems[LIndex] := LSnapshot.ItemAt(LIndex);
  end;
  for LIndex := 4 to High(LItems) do
  begin
    LItems[LIndex] := NyxCollectionItem(NyxItem(LKey, 'worker-' + TNyxText(IntToStr(LIndex))))
      .WithValue(NyxTextField('name'), 'Worker ' + TNyxText(IntToStr(LIndex)))
      .WithValue(NyxIntegerField('pid'), 3000 + LIndex)
      .WithValue(NyxTextField('command'), 'worker --queue=default')
      .WithValue(NyxTextField('notes'), 'Ready for the next job.')
      .WithValue(NyxTextField('status'), 'Running');
  end;
  LSeed := NewNyxCollection(LKey, LSnapshot.Schema, LItems);
  LItems := nil;
  RecordSample(wpPrepare, 0, LStarted);
  LStarted := ClockMS;
  GStore.Assign(LSeed.Snapshot, GStore.Snapshot.Revision);
  {$ifdef PAS2JS}await(PaintTurn);{$else}PaintTurn;{$endif}
  RecordSample(wpPopulate, 0, LStarted);
  LSeed := nil;
  Check((GFirst.View.Snapshot.Count = CServices) and (GPeer.View.Snapshot.Count = CServices),
    'both actual public tables receive all ten thousand services');
  LDistant := NyxItem(LKey, 'worker-5000');
  BeginDraft(NyxItem(LKey, 'gateway'));
  {$ifdef PAS2JS}await(UpdateBursts(wpUpdate, wpTurn, 'Update '));
  {$else}UpdateBursts(wpUpdate, wpTurn, 'Update ');{$endif}
  { End editing explicitly before Pause: pause intentionally withdraws editing
    authority. Escape preserves accepted source and avoids committing the draft. }
  CancelDraft;
  LStarted := ClockMS;
  Click(InspectionPauseID);
  RecordSample(wpPause, 0, LStarted);
  LSnapshot := GFirst.View.Snapshot;
  for LBurst := 0 to 9 do
  begin
    GStore.Update(NyxCollectionItem(LWorker).WithValue(NyxTextField('status'),
      'Tick ' + TNyxText(IntToStr(LBurst))));
    {$ifdef PAS2JS}await(PaintTurn);{$else}PaintTurn;{$endif}
    Check((GFirst.View.Snapshot = LSnapshot) and
      (GPeer.View.Snapshot.Item(LWorker).GetValue(NyxTextField('status')) =
      'Tick ' + TNyxText(IntToStr(LBurst))),
      'the paused display stays exact while its independently mounted peer receives every tick');
  end;
  LStarted := ClockMS;
  Click(InspectionResumeID);
  {$ifdef PAS2JS}await(PaintTurn);{$else}PaintTurn;{$endif}
  RecordSample(wpResume, 0, LStarted);
  Check(GFirst.View.Snapshot.Item(LWorker).GetValue(NyxTextField('status')) = 'Tick 9',
    'the ordinary Resume action displays the latest admitted tick');
  LStarted := ClockMS;
  GFirst.View.ConfigureQuery(NyxCollectionQuery.Where(NyxWhere(NyxTextField('status')).EqualTo('Running')));
  {$ifdef PAS2JS}await(PaintTurn);{$else}PaintTurn;{$endif}
  RecordSample(wpFilter, 0, LStarted);
  Check((GFirst.View.Snapshot.Count = CServices - 3) and (GPeer.View.Snapshot.Count = CServices),
    'a real filter applies only to the inspecting view');
  BeginDraft(NyxItem(LKey, 'worker-4'));
  {$ifdef PAS2JS}await(UpdateBursts(wpFilteredUpdate, wpFilteredTurn, 'Filtered update '));
  {$else}UpdateBursts(wpFilteredUpdate, wpFilteredTurn, 'Filtered update ');{$endif}
  CancelDraft;
  LStarted := ClockMS;
  GFirst.View.ConfigureQuery(NyxCollectionQuery.Where(NyxWhere(NyxTextField('status')).EqualTo('Running'))
    .OrderBy(NyxIntegerField('pid'), nsdDescending));
  {$ifdef PAS2JS}await(PaintTurn);{$else}PaintTurn;{$endif}
  RecordSample(wpSort, 0, LStarted);
  BeginDraft(NyxItem(LKey, 'worker-4'));
  LSelectionBeforeReveal := GFirst.View.Selection;
  LViewport := NyxCollectionViewport(GFirst);
  Check((LViewport <> nil) and LViewport.Active, 'the real mounted table exposes an independent portable viewport');
  {$ifdef PAS2JS}
  Check(document.activeElement = LEditor, 'the reveal journey begins with the actual Unicode editor focused');
  LReveal := LViewport.Reveal(NyxRevealItem(NyxItem(LKey, 'worker-2000'))
    .InField(NyxTextField('notes')));
  Check(LReveal.Pending, 'the distant browser request awaits a measured physical frame');
  { An injected wheel qualifies capture/cancellation, not trusted hardware input.
    The ordinary virtual measurement wake must never revive this withdrawn work. }
  LTable.dispatchEvent(TJSEvent.new('wheel'));
  Check((LReveal.Capture.Status = ncvsCancelled) and
    (LReveal.Capture.Reason = ncvrViewportMoved),
    'new wheel input cancels pending reveal before measurement can pull the user back');
  await(PaintTurn);
  Check(LReveal.Capture.Status = ncvsCancelled,
    'a later measured frame cannot revive the interrupted receipt');
  CheckDraft;
  {$else}
  LFocusedControl := Screen.ActiveControl;
  Check(LFocusedControl <> nil, 'the native reveal journey has an actual active control');
  {$endif}
  LReveal := LViewport.Reveal(NyxRevealItem(LDistant).InField(NyxTextField('notes')));
  for LWait := 0 to 31 do
  begin

    if not LReveal.Pending then
    begin
      Break;
    end;
    {$ifdef PAS2JS}await(PaintTurn);{$else}PaintTurn; Application.Idle(False);{$endif}
  end;
  Check(LReveal.Capture.Status = ncvsCompleted,
    'portable reveal settles the actual distant field without direct target scroll');
  Check(GFirst.View.Selection = LSelectionBeforeReveal,
    'default reveal preserves exact row membership and canonical focus');
  CheckDraft;
  {$ifdef PAS2JS}
  Check(document.activeElement = LEditor, 'default reveal never blurs the original unfinished input');
  {$else}
  Check(Screen.ActiveControl = LFocusedControl, 'default native reveal retains the original active control');
  {$endif}
  CancelDraft;
  LStarted := ClockMS;
  Check(GFirst.Select(LDistant), 'deliberate distant navigation selects the requested stable service');
  LReveal := LViewport.Reveal(NyxRevealItem(LDistant).InField(NyxTextField('notes')).Focus(ncvfTake));
  for LWait := 0 to 31 do
  begin

    if not LReveal.Pending then
    begin
      Break;
    end;
    {$ifdef PAS2JS}await(PaintTurn);{$else}PaintTurn; Application.Idle(False);{$endif}
  end;
  Check(LReveal.Capture.Status = ncvsCompleted, 'deliberate focus reveal completes through the same public contract');
  {$ifdef PAS2JS}
  LDistantCell := TJSHTMLElement(LTable.querySelector('[data-nyx-item="worker-5000"]'));
  Check(LDistantCell <> nil, 'the deliberate distant selection realizes its actual row');
  await(PaintTurn);
  LRevealBounds := LDistantCell.getBoundingClientRect;
  LClipBounds := LTable.getBoundingClientRect;
  LHeaderBottom := LClipBounds.top;
  for LIndex := 0 to LTable.querySelectorAll('thead th').length - 1 do
  begin
    LHeaderBottom := Max(LHeaderBottom,
      TJSHTMLElement(LTable.querySelectorAll('thead th')[LIndex]).getBoundingClientRect.bottom);
  end;
  Check((LRevealBounds.top >= LHeaderBottom - 1) and
    (LRevealBounds.bottom <= LClipBounds.bottom + 1),
    'the requested row is actually visible below sticky headers and inside the table clip');
  Check(TJSHTMLElement(document.activeElement).closest('[data-nyx-item]') = LDistantCell,
    'explicit focus belongs to the revealed stable row');
  Check(LTable.querySelectorAll('[data-nyx-item]').length <= 300,
    'the large visible browser table retains a bounded resident row window');
  LDistantCell := TJSHTMLElement(LTable.querySelector(
    '[data-nyx-item="worker-5000"] [data-nyx-column="3"] .nyx-cell-value'));
  Check((LDistantCell <> nil) and (LDistantCell.textContent = 'Filtered update 19'),
    'newly realized browser cells show the final offscreen producer value');
  {$else}
  PaintTurn;
  Check(LTable.TopRow >= 4990, 'the real native grid reaches a distant service range');
  LReadFace := FieldFace(3, LReadColumn);
  Check((LReadFace <> nil) and (LReadColumn >= 0) and
    (LReadFace.Cells[LReadColumn, GFirst.View.Snapshot.IndexOf(LDistant) + 1] = 'Filtered update 19'),
    'the actual native field reader shows the final offscreen producer value');
  LRevealBounds := LReadFace.CellRect(LReadColumn, GFirst.View.Snapshot.IndexOf(LDistant) + 1);
  LClipBounds := LReadFace.ClientRect;
  LClipBounds.Top := LReadFace.CellRect(LReadColumn, 0).Bottom;

  if LRevealBounds.Bottom - LRevealBounds.Top <= LClipBounds.Bottom - LClipBounds.Top then
  begin
    Check((LRevealBounds.Top >= LClipBounds.Top) and (LRevealBounds.Bottom <= LClipBounds.Bottom),
      'a native row which fits is wholly visible below the header and inside the data clip');
  end
  else
  begin
    { The unchanged compact160px allocation leaves31px below its native header,
      while this row is39px. Useful placement must span that available area and
      report partial visibility honestly; no scroll can make39px fit into31px. }
    Check((LRevealBounds.Top <= LClipBounds.Top) and (LRevealBounds.Bottom >= LClipBounds.Bottom) and
      not LReveal.Capture.FullyVisible,
      'an oversized native row spans the available clip and honestly reports partial visibility');
  end;
  {$endif}
  RecordSample(wpScroll, 0, LStarted);
  {$ifdef PAS2JS}
  LPreviousVisibility := LTable.style.getPropertyValue('visibility');
  LTable.style.setProperty('visibility', 'hidden');
  {$else}
  LPreviousVisibility := LTable.Visible;
  LTable.Visible := False;
  {$endif}
  try
    LReveal := LViewport.Reveal(NyxRevealItem(LDistant).InField(NyxTextField('notes')));
    Check((LReveal.Capture.Status = ncvsRefused) and
      (LReveal.Capture.Reason = ncvrNoViewport),
      'a hidden target refuses even when its retained layout geometry is nonempty');
  finally
    {$ifdef PAS2JS}
    LTable.style.setProperty('visibility', LPreviousVisibility);
    {$else}
    LTable.Visible := LPreviousVisibility;
    {$endif}
  end;
  {$ifdef PAS2JS}
  LClipParent := TJSHTMLElement(LTable.parentElement);
  LPreviousOverflow := LClipParent.style.getPropertyValue('overflow');
  LPreviousHeight := LClipParent.style.getPropertyValue('height');
  LPreviousMinHeight := LClipParent.style.getPropertyValue('min-height');
  LPreviousMaxHeight := LClipParent.style.getPropertyValue('max-height');
  LClipParent.style.setProperty('overflow', 'hidden');
  LClipParent.style.setProperty('height', '1px');
  LClipParent.style.setProperty('min-height', '0');
  LClipParent.style.setProperty('max-height', '1px');
  {$else}
  { The page layout owns the table's immediate parent height. Clip the actual
    containing form instead, then settle its ordinary layout before admission. }
  LClipParent := GApplication.Window;
  LPreviousHeight := LClipParent.Height;
  LClipParent.Height := 1;
  PaintTurn;
  Application.Idle(False);
  Check(LClipParent.ClientHeight < LTable.CellRect(0, 0).Bottom,
    'the native fixture actually clips the data below its containing form');
  {$endif}
  try
    LReveal := LViewport.Reveal(NyxRevealItem(LDistant).InField(NyxTextField('notes')));
    for LWait := 0 to 31 do
    begin

      if not LReveal.Pending then
      begin
        Break;
      end;
      {$ifdef PAS2JS}await(PaintTurn);{$else}PaintTurn; Application.Idle(False);{$endif}
    end;
    Check((LReveal.Capture.Status = ncvsRefused) and
      (LReveal.Capture.Reason = ncvrNoViewport),
      {$ifdef PAS2JS}
      'a wholly clipped parent refuses without scrolling or opening that parent'
      {$else}
      TNyxText(Format('a wholly clipped parent refuses without scrolling or opening that parent: status=%d reason=%d parent=%d grid=%d',
        [Ord(LReveal.Capture.Status), Ord(LReveal.Capture.Reason),
         LClipParent.ClientHeight, LTable.ClientHeight]))
      {$endif});
  finally
    {$ifdef PAS2JS}
    LClipParent.style.setProperty('overflow', LPreviousOverflow);
    LClipParent.style.setProperty('height', LPreviousHeight);
    LClipParent.style.setProperty('min-height', LPreviousMinHeight);
    LClipParent.style.setProperty('max-height', LPreviousMaxHeight);
    {$else}
    LClipParent.Height := LPreviousHeight;
    {$endif}
  end;
  Check(TNyxCodec.Encode(GDocument) = LSaved, 'runtime scale, queries, edits and preferences preserve the public saved design');
  {$ifdef PAS2JS}
  GReadyToFinish := True;
  GApplication.Context.State.SetValue(NyxBooleanState(CRetireReady), True);
  document.body.setAttribute('data-live-workload-ready', 'true');
  document.body.setAttribute('data-live-workload-checks', IntToStr(GChecks));
  {$else}
  Finish;
  {$endif}
end;

{$ifdef PAS2JS}
function RunBrowser: JSValue; async;
begin
  Result := Undefined;
  try
    await(Journey);
  except
    on LError: Exception do
    begin
      document.body.setAttribute('data-live-workload-error', LError.Message);
      document.body.setAttribute('data-live-workload-samples', GReport);
    end;
  end;
end;
{$endif}

begin
  {$ifdef PAS2JS}
  RunBrowser;
  {$else}
  try
    Application.Initialize;
    Application.CaptureExceptions := False;
    Check((ParamStr(1) = '1280') or (ParamStr(1) = '390'),
      'native workload width must select a declared qualification allocation');
    QueryPerformanceFrequency(GFrequency);
    Journey;
  except
    on LError: Exception do
    begin
      WriteLn('FAIL ', LError.Message);
      DumpExceptionBackTrace(Output);
      GProbeToken := nil;
      GProbe.Free;
      GDemo.Free;
      GApplication.Free;
      GDocument.Free;
      ExitCode := 1;
    end;
  end;
  {$endif}
end.
