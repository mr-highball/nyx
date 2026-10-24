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

program nyx_column_activation_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch externalclass}

uses
  SysUtils, JS, Web,
  nyx.text, nyx.types, nyx.model, nyx.controls, nyx.schema, nyx.codec, nyx.contract,
  nyx.state, nyx.events, nyx.scheduler, nyx.publication, nyx.behavior,
  nyx.collections, nyx.collections.columns, nyx.collections.arrangement,
  nyx.collections.view, nyx.collections.view.types, nyx.collections.mount,
  nyx.collections.browser,
  nyx.render.browser;

type
  { Older Web units omit the capture argument of standard EventTarget removal.
    This target-only bridge borrows the actual table and removes precisely the
    capture listener installed by the fixture; no dependency source is changed. }
  TActivationEventTarget = class external name 'EventTarget'(TJSEventTarget)
    procedure Unlisten(const AName: String; AHandler: TJSEventHandler;
      ACapture: Boolean); external name 'removeEventListener';
  end;

  { Browser-only standard KeyboardEvent construction exercises the actual
    renderer capture and table bubbling routes. Synthetic keys do not establish
    trusted browser default synthesis, hardware or assistive-technology use. }
  TActivationKey = class external name 'KeyboardEvent'(TJSKeyboardEvent)
    constructor new(const AType: String; AOptions: TJSObject); reintroduce;
  end;

  { Standard DOM mouse activation carries detail/cancelability through the real
    mounted Click/DoubleClick receivers; no mount default is called directly. }
  TActivationMouse = class external name 'MouseEvent'(TJSMouseEvent)
    constructor new(const AType: String; AOptions: TJSObject); reintroduce;
  end;

  { The standard PointerEvent bridge verifies that AutoSize-only chrome cannot
    begin a Resize gesture. It does not claim trusted pointer capture. }
  TActivationPointer = class external name 'PointerEvent'(TJSPointerEvent)
    constructor new(const AType: String; AOptions: TJSObject); reintroduce;
  end;

  { Independent mounted cases isolate publication from callback cancellation;
    each owns its real document, renderer, external state and collection view. }
  TActivationCase = (acSingleClick, acEnter, acSpace, acConsumedKey, acSuspendedInput, acNoSubscribers,
    acPreventedBefore, acPreventedDuring, acResizeDenied,
    acDoubleSequence, acCallbackRetirement, acDirectMount,
    acSpaceRelease, acSpaceRepeat, acSpaceBlur, acSpaceWindowBlur,
    acSpaceConsumedRelease, acSpaceDisabled, acSpaceRetirement, acDirectSpace,
    acSpaceLayoutChanged);

  { A callback's owner is borrowed until its subscriptions are disconnected. }
  TActivationReview = class;

  { Snapshot-only event adapter retains no renderer/node. Its borrowed review
    lives until subscription teardown, including synchronous view retirement. }
  TActivationCallback = class(TNyxEventCallback)
  public
    Owner: TActivationReview;
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;

  { Real public renderer ownership stays outside callbacks. The externally owned
    scalar state remains valid through renderer Unmount and destruction. Column
    facets are observations of that exact accepted table, never a second model. }
  TActivationReview = class
  private
    FDocument: TNyxDocument;
    FRenderer: TNyxBrowserRenderer;
    FHost: TJSHTMLElement;
    FTable: TJSHTMLElement;
    FGrip: TJSHTMLElement;
    FStore: TNyxState;
    FView: INyxCollectionView;
    FColumns: INyxCollectionColumns;
    FMount: INyxCollectionMount;
    FToken: INyxEventSubscription;
    FKeyToken: INyxEventSubscription;
    FReleaseToken: INyxEventSubscription;
    FCallback: INyxEventCallback;
    FKind: TActivationCase;
    FAuthored: TNyxText;
    FRows: INyxCollectionSnapshot;
    FRowsRevision: Integer;
    FSemantic: Integer;
    FLegacy: Integer;
    FKeys: Integer;
    FKeyUps: Integer;
    FNotifications: Integer;
    FViewToken: INyxCollectionViewSubscription;
    FCurrentClick: TJSMouseEvent;
    FCallbackPrevented: Boolean;
    function PreventClick(AEvent: TEventListenerEvent): Boolean;
    procedure Semantic(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution);
    procedure Legacy(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure ColumnsChanged(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
  public
    constructor Create(AKind: TActivationCase);
    destructor Destroy; override;
    procedure CheckBaseline;
    procedure Click(ADetail: Integer = 0);
    procedure Key(const AKey: String);
    procedure KeyPhase(const AType: String; const AKey: String;
      ARepeat: Boolean = False);
    function Capture: INyxCollectionColumnsCapture;
    procedure CheckDelivery(AClicks: Integer);
    procedure CheckUnchangedData;
    procedure CheckFit(AOldSerial: Integer);
    procedure Run; async;
  end;

const
  CTable = 'activation-table';
  CClick = 'column-activated';
  CCaseNames: array[TActivationCase] of String = ('single-click', 'enter',
    'space', 'consumed-key', 'suspended-input', 'no-subscribers', 'prevented-before', 'prevented-during',
    'resize-denied', 'double-sequence', 'callback-retirement', 'direct-mount',
    'space-release', 'space-repeat', 'space-blur', 'space-window-blur',
    'space-consumed-release', 'space-disabled', 'space-retirement', 'direct-space',
    'space-layout-changed');

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Column activation: ' + AReason);
  end;
  Inc(GChecks);
end;

function Services: TNyxCollectionRef;
begin
  Result := NyxCollection('activation-services');
end;

function ServiceName: TNyxTextFieldRef;
begin
  Result := NyxTextField('service-name');
end;

function ActivationCount: TNyxIntegerStateRef;
begin
  Result := NyxIntegerState('activation-count');
end;

function ColumnSpec: TNyxCollectionViewSpec;
begin
  { AutoSize is a separate capability. Resize, pin, hide and reorder are absent,
    so a host cannot make this action work by silently enabling drag resizing. }
  Result := NyxCollectionView(Services).Column(ServiceName, 'Service', cmEditable)
    .ColumnSizing(NyxColumnSizing.Fixed(64).Minimum(40).Maximum(360))
    .ColumnLayout(NyxColumnLayout.AllowOnly([nclfAutoSize]))
    .TableLayout(NyxTableLayout.AllowOnly([nclfAutoSize]));
end;

function Fixture: TNyxDocument;
var
  LPage: INyxPage;
  LTable: INyxTable;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Column single activation';
    Result.State.SetValue(ActivationCount, 0);
    Result.Collections.Define(Services,
      NyxCollectionSchema.Text(ServiceName, ''), [
      NyxCollectionItem(NyxItem(Services, 'service-a')).WithValue(ServiceName,
        'A deliberately long accepted service name / 🌙'),
      NyxCollectionItem(NyxItem(Services, 'service-b')).WithValue(ServiceName, 'Worker')]);
    LPage := NewNyxPage('home');
    LPage.Configure.Padding(12).Gap(12).Done;
    LTable := NewNyxTable(CTable);
    { A real authored scalar action makes bypassing LiveBindings.Dispatch
      observable. The table declares its own typed value; it does not borrow a
      synthetic child button or substitute a Signal for the canonical Click. }
    LTable.Node.Contract.Value(NyxIntegerDomain);
    LTable.Configure.Height(190).Value(0).Action(naIncrement)
      .OnClick(NyxEvent(CClick)).Done;
    LTable.Node.Binds.Value(ActivationCount).Done;
    LTable.Binds.Collection(ColumnSpec);
    LPage.Add(LTable);
    Result.AddPage(LPage);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

{ Ordered rendering opportunities drain the mount's existing RAF publisher.
  No alternate queue, timer delay or direct publication callback is introduced. }
function Frame: TJSPromise;
begin
  Result := TJSPromise.new(procedure(AResolve, AReject: TJSPromiseResolver)
    begin
      window.requestAnimationFrame(procedure(ATime: Double)
        begin
          AResolve(JSValue(True));
        end);
    end);
end;

procedure Drain; async;
begin
  await(Frame);
  await(Frame);
  await(Frame);
end;

procedure TActivationCallback.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  Owner.Semantic(AEvent, AExecution);
end;

constructor TActivationReview.Create(AKind: TActivationCase);
var
  LCallback: TActivationCallback;
begin
  inherited Create;
  FKind := AKind;
  FDocument := Fixture;
  FAuthored := TNyxCodec.Encode(FDocument);
  FStore := FDocument.State.Clone;
  FRenderer := TNyxBrowserRenderer.Create;
  FRenderer.OnEvent := @Legacy;
  FHost := TJSHTMLElement(document.createElement('main'));
  FHost.style.setProperty('width', '100%');
  document.body.appendChild(FHost);
  FRenderer.Render(FDocument, FDocument.Pages[0], FHost, False, FStore);
  FTable := FRenderer.ElementFor(CTable);
  FGrip := TJSHTMLElement(FTable.querySelector('[data-nyx-column-resize]'));
  FView := FRenderer.CollectionView(CTable);
  FColumns := NyxCollectionColumns(FView);
  FMount := FRenderer.CollectionMount(CTable);
  FRows := FView.Snapshot;
  FRowsRevision := FView.Store.Snapshot.Revision;
  FViewToken := FView.Subscribe(@ColumnsChanged);
  LCallback := TActivationCallback.Create;
  LCallback.Owner := Self;
  FCallback := LCallback;

  if AKind <> acNoSubscribers then
  begin
    FToken := FRenderer.Events.OnNamed(NyxControlEvents(CTable), NyxEvent(CClick))
      .Subscribe(FCallback);
    FKeyToken := FRenderer.Events.OnKeyDown(NyxControlEvents(CTable))
      .Subscribe(FCallback);
    FReleaseToken := FRenderer.Events.OnKeyUp(NyxControlEvents(CTable))
      .Subscribe(FCallback);
  end
  else
  begin
    FRenderer.OnEvent := nil;
  end;
end;

destructor TActivationReview.Destroy;
begin

  if FViewToken <> nil then
  begin
    FViewToken.Disconnect;
  end;
  FViewToken := nil;

  if FToken <> nil then
  begin
    FToken.Cancel;
  end;

  if FKeyToken <> nil then
  begin
    FKeyToken.Cancel;
  end;

  if FReleaseToken <> nil then
  begin
    FReleaseToken.Cancel;
  end;
  FToken := nil;
  FKeyToken := nil;
  FReleaseToken := nil;
  FCallback := nil;

  if FTable <> nil then
  begin
    TActivationEventTarget(FTable).Unlisten('click', @PreventClick, True);
  end;
  FMount := nil;
  FColumns := nil;
  FView := nil;
  FRows := nil;
  FRenderer.Free;
  FStore.Free;
  FDocument.Free;

  if FHost <> nil then
  begin
    FHost.remove;
  end;
  inherited Destroy;
end;

function TActivationReview.PreventClick(AEvent: TEventListenerEvent): Boolean;
begin
  { Same-host capture prevention intentionally does not stop propagation: an
    ordinary onclick receiver on this host must still honor the claimed event. }
  AEvent.preventDefault;
  Result := True;
end;

procedure TActivationReview.Semantic(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
var
  LResponse: INyxEventResponse;
begin
  Check((AEvent.OriginID = CTable) and (AEvent.SourceID = CTable),
    'canonical event retains the exact authored table origin and source');

  if AEvent.Trigger = ntKeyDown then
  begin
    Inc(FKeys);

    if FKind = acConsumedKey then
    begin
      Check(Supports(AExecution, INyxEventResponse, LResponse) and
        LResponse.CanConsume, 'real canonical key callback owns its synchronous response');
      LResponse.Consume;
    end;
    Exit;
  end;

  if AEvent.Trigger = ntKeyUp then
  begin
    Inc(FKeyUps);

    if FKind = acSpaceConsumedRelease then
    begin
      Check(Supports(AExecution, INyxEventResponse, LResponse) and
        LResponse.CanConsume, 'matching Space release exposes its canonical response');
      LResponse.Consume;
    end;
    Exit;
  end;
  Check((AEvent.Trigger = ntClick) and AEvent.IsNamed(NyxEvent(CClick)),
    'canonical Click retains its real authored application event name');
  Check(AEvent.HasValue and (AEvent.ValueKind = nskInteger) and
    (AEvent.Value.AsInteger = FStore.GetValue(ActivationCount)),
    'semantic Click observes the committed authored integer action');
  Inc(FSemantic);

  if FKind = acPreventedDuring then
  begin
    Check(FCurrentClick <> nil, 'callback owns the current actual DOM click');
    FCurrentClick.preventDefault;
    FCallbackPrevented := True;
  end;

  if FKind = acCallbackRetirement then
  begin
    FRenderer.Unmount;
  end;
end;

procedure TActivationReview.Legacy(ANode: TNyxNode;
  const AEvent: TNyxEventInfo);
begin
  Check((ANode.ID = CTable) and (AEvent.Trigger = ntClick) and
    AEvent.IsNamed(NyxEvent(CClick)),
    'legacy callback retains the same authored source and Click name');
  Inc(FLegacy);
end;

procedure TActivationReview.ColumnsChanged(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
begin
  Check((AView = FView) and (AChanges = nil),
    'Fit notifies the original table once as a presentation-only publication');
  Inc(FNotifications);
end;

function TActivationReview.Capture: INyxCollectionColumnsCapture;
begin
  Result := FColumns.CaptureColumns;
end;

procedure TActivationReview.CheckBaseline;
var
  LCapture: INyxCollectionColumnsCapture;
begin
  Check((FGrip <> nil) and FMount.Connected,
    'real mounted table exposes a live column action');
  Check((FGrip.getAttribute('role') = 'button') and
    (FGrip.getAttribute('aria-label') = 'Fit Service') and
    (FGrip.getAttribute('aria-disabled') = 'false') and (FGrip.tabIndex = 0),
    'AutoSize-only action has a named enabled keyboard-reachable button face');
  LCapture := Capture;
  Check(LCapture.Spec.TableLayoutPolicy.Allows(nclfAutoSize) and
    not LCapture.Spec.TableLayoutPolicy.Allows(nclfResize) and
    LCapture.Spec.ColumnAt(0).LayoutPolicy.Allows(nclfAutoSize) and
    not LCapture.Spec.ColumnAt(0).LayoutPolicy.Allows(nclfResize),
    'both authored policies allow AutoSize while denying Resize');
  Check((LCapture.Preferences.Count = 0) and
    (FStore.GetValue(ActivationCount) = 0), 'each case owns an independent baseline');
end;

procedure TActivationReview.Click(ADetail: Integer);
var
  LOptions: TJSObject;
begin
  LOptions := TJSObject.new;
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;
  LOptions['button'] := 0;
  LOptions['detail'] := ADetail;
  FCurrentClick := TActivationMouse.new('click', LOptions);
  try
    { A detail-zero click is also the standard programmatic/AT-style DOM
      activation shape. It does not certify a particular screen reader. }
    FGrip.dispatchEvent(FCurrentClick);
  finally
    FCurrentClick := nil;
  end;
end;

procedure TActivationReview.Key(const AKey: String);
begin
  KeyPhase('keydown', AKey);
  KeyPhase('keyup', AKey);
end;

procedure TActivationReview.KeyPhase(const AType: String; const AKey: String;
  ARepeat: Boolean);
var
  LOptions: TJSObject;
begin
  LOptions := TJSObject.new;
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;
  LOptions['key'] := AKey;
  LOptions['repeat'] := ARepeat;
  FGrip.dispatchEvent(TActivationKey.new(AType, LOptions));
end;

procedure TActivationReview.CheckDelivery(AClicks: Integer);
begin
  Check(FStore.GetValue(ActivationCount) = AClicks,
    'real authored Click action executes exactly once per accepted activation');
  Check((FSemantic = AClicks) and (FLegacy = AClicks),
    'semantic and legacy Click delivery each run exactly once');
end;

procedure TActivationReview.CheckUnchangedData;
begin
  Check((FView.Snapshot = FRows) and
    (FView.Store.Snapshot.Revision = FRowsRevision),
    'Fit and scalar Click action preserve accepted collection rows and revision');
  Check(TNyxCodec.Encode(FDocument) = FAuthored,
    'activation never rewrites the authored document or default arrangement');
end;

procedure TActivationReview.CheckFit(AOldSerial: Integer);
var
  LCapture: INyxCollectionColumnsCapture;
  LPreference: TNyxColumnPreference;
  LIndex: Integer;
begin
  LCapture := Capture;
  LIndex := LCapture.Preferences.IndexOf(NyxColumnIdentity(ServiceName));
  Check(LIndex >= 0, 'single activation publishes a real runtime width preference' +
    ' / mount: ' + FMount.ErrorText + ' / renderer: ' + FRenderer.LastBindingError +
    ' / notifications: ' + IntToStr(FNotifications));
  LPreference := LCapture.Preferences.ColumnAt(LIndex);
  Check(LPreference.HasWidth and (LPreference.WidthValue > 64) and
    (LPreference.WidthValue <= 360) and (LCapture.Serial = AOldSerial + 1),
    'Fit measures accepted current-page text within authored logical-pixel bounds');
  Check(FNotifications = 1, 'one activation creates exactly one changed layout publication');
end;

procedure TActivationReview.Run; async;
var
  LBefore: INyxCollectionColumnsCapture;
  LAfter: INyxCollectionColumnsCapture;
  LOptions: TJSObject;
  LCell: TJSHTMLElement;
  LInput: TJSHTMLInputElement;
  LRejected: Boolean;
  LStage: INyxPreparedPublication;
  LNotifications: Integer;
  LWidth: Integer;
  LDirect: TJSHTMLElement;
  LDirectGrip: TJSHTMLElement;
  LDirectMount: INyxCollectionMount;
  LEditMount: INyxCollectionEditMount;
  LEdits: INyxCollectionEditsSnapshot;
begin
  await(Drain);
  CheckBaseline;
  LBefore := Capture;
  case FKind of
    acSingleClick:
      begin
        { Native HTMLElement.click is the primary missing single-activation
          expectation; do not replace this with DoubleClick or PrepareAutoSize. }
        FGrip.click;
        await(Drain);
        CheckDelivery(1);
        CheckFit(LBefore.Serial);
      end;
    acEnter, acSpace:
      begin
        FGrip.focus;

        if FKind = acEnter then
        begin
          Key('Enter');
        end
        else
        begin
          Key(' ');
        end;
        await(Drain);
        Check(FKeys = 1, 'keyboard activation preserves one real canonical key callback');
        Check(Capture.Serial = LBefore.Serial + 1,
          'button Enter/Space invokes the same one-shot Fit command');
        CheckDelivery(1);
        CheckFit(LBefore.Serial);
      end;
    acConsumedKey:
      begin
        FGrip.focus;
        Key('Enter');
        await(Drain);
        Check(FKeys = 1, 'consumed key preserves its ordinary canonical phase once');
        CheckDelivery(0);
        Check((Capture.Serial = LBefore.Serial) and (FNotifications = 0),
          'consuming the activation key suppresses both canonical Click and Fit default');
      end;
    acSuspendedInput:
      begin
        Check(Supports(FMount, INyxCollectionEditMount, LEditMount),
          'real renderer mount exposes its existing reversible edit suspension');
        LEdits := LEditMount.SuspendCellEdits;
        try
          Check((LEdits <> nil) and FMount.Connected,
            'explicit suspension retains the same connected physical mount');
          FGrip.focus;
          Key('Enter');
          await(Drain);
          Check(FKeys = 0,
            'renderer capture refuses initially suspended input before canonical key callbacks');
          CheckDelivery(0);
          Check((Capture.Serial = LBefore.Serial) and (FNotifications = 0),
            'initially suspended activation creates no authored command or Fit publication');
        finally
          LEditMount.ResumeCellEdits(LEdits, False);
        end;
        LEdits := nil;
        await(Drain);
        FGrip.click;
        await(Drain);
        CheckDelivery(1);
        CheckFit(LBefore.Serial);
      end;
    acNoSubscribers:
      begin
        FGrip.click;
        await(Drain);
        Check(FStore.GetValue(ActivationCount) = 1,
          'no-subscriber fast path still executes the authored Click action once');
        Check((FSemantic = 0) and (FLegacy = 0),
          'empty registrations do not fabricate callback delivery');
        CheckFit(LBefore.Serial);
      end;
    acPreventedBefore:
      begin
        FTable.addEventListener('click', @PreventClick, True);
        Click;
        await(Drain);
        CheckDelivery(0);
        Check((Capture.Serial = LBefore.Serial) and (FNotifications = 0),
          'already-prevented same-host click cannot fall through to legacy action or Fit');
      end;
    acPreventedDuring:
      begin
        Click;
        await(Drain);
        CheckDelivery(1);
        Check(FCallbackPrevented and (Capture.Serial = LBefore.Serial) and
          (FNotifications = 0),
          'canonical callback prevention preserves its committed action but consumes Fit default');
      end;
    acResizeDenied:
      begin
        LRejected := False;
        try
          LStage := FColumns.PrepareColumns(LBefore,
            NyxColumnPreferences.WithColumn(NyxColumnPreference(
              NyxColumnIdentity(ServiceName)).Width(120)));
          { Preparation is intentionally detached. The public coordinated
            publication performs capability validation before any install. }
          PublishNyxGroup([LStage]);
        except
          on E: Exception do
          begin
            LRejected := True;
          end;
        end;
        LStage := nil;
        Check(LRejected, 'runtime Resize cannot borrow the separate AutoSize capability');
        LOptions := TJSObject.new;
        LOptions['bubbles'] := True;
        LOptions['cancelable'] := True;
        LOptions['pointerId'] := 1;
        LOptions['button'] := 0;
        LOptions['buttons'] := 1;
        LOptions['clientX'] := 60;
        FGrip.dispatchEvent(TActivationPointer.new('pointerdown', LOptions));
        LOptions['clientX'] := 170;
        FGrip.dispatchEvent(TActivationPointer.new('pointermove', LOptions));
        LOptions['buttons'] := 0;
        FGrip.dispatchEvent(TActivationPointer.new('pointerup', LOptions));
        FGrip.focus;
        Key('ArrowRight');
        await(Drain);
        Check((Capture.Serial = LBefore.Serial) and (FNotifications = 0),
          'pointer and arrow resizing remain denied on the Fit-only action');
        { Open a genuine cell editor through the ordinary F2 route, then retain
          an unfinished draft. Click must not commit or replace this editor. }
        LCell := TJSHTMLElement(FTable.querySelector('[role=gridcell]'));
        Check(LCell <> nil, 'accepted real row exposes its canonical data cell');
        LCell.focus;
        LOptions['key'] := 'F2';
        LCell.dispatchEvent(TActivationKey.new('keydown', LOptions));
        LInput := TJSHTMLInputElement(LCell.querySelector('input'));
        Check((LInput <> nil) and (document.activeElement = LInput),
          'ordinary F2 owns the real cell editor before header activation');
        { F2 also selects its row and realizes editor geometry. Finish that
          distinct input before observing Fit; its selection notification is
          not a layout publication caused by the later header activation. }
        await(Drain);
        FNotifications := 0;
        LBefore := Capture;
        LInput.value := 'Unfinished / 🌙';
        LInput.selectionStart := 2;
        LInput.selectionEnd := 6;
        FGrip.click;
        await(Drain);
        Check((document.activeElement = LInput) and
          (LInput.value = 'Unfinished / 🌙') and
          (LInput.selectionStart = 2) and (LInput.selectionEnd = 6),
          'Fit-only Click preserves unfinished editor focus draft and exact caret');
        CheckDelivery(1);
        CheckFit(LBefore.Serial);
      end;
    acDoubleSequence:
      begin
        Click(1);
        await(Drain);
        CheckDelivery(1);
        Click(2);
        await(Drain);
        CheckDelivery(2);
        { Repeated equal Fit can be a model no-op, so a serial-only check would
          miss a third request. Install an explicitly measured setup width via
          the public AutoSize capability before sending the final dblclick.
          This setup is not evidence that the physical activation worked. }
        LAfter := Capture;
        LStage := FColumns.PrepareAutoSize(LAfter, NyxColumnIdentity(ServiceName), 80);
        PublishNyxGroup([LStage]);
        LStage := nil;
        await(Drain);
        LAfter := Capture;
        LWidth := LAfter.Preferences.ColumnAt(LAfter.Preferences.IndexOf(
          NyxColumnIdentity(ServiceName))).WidthValue;
        Check(LWidth = 80, 'third-default oracle has an actual distinct accepted setup width');
        LNotifications := FNotifications;
        LOptions := TJSObject.new;
        LOptions['bubbles'] := True;
        LOptions['cancelable'] := True;
        LOptions['button'] := 0;
        LOptions['detail'] := 2;
        FGrip.dispatchEvent(TActivationMouse.new('dblclick', LOptions));
        await(Drain);
        CheckDelivery(2);
        Check((Capture.Serial = LAfter.Serial) and
          (Capture.Preferences.ToData.ToJSON = LAfter.Preferences.ToData.ToJSON) and
          (FNotifications = LNotifications),
          'two Fit-only Click activations are not followed by a third dblclick Fit');
      end;
    acCallbackRetirement:
      begin
        FGrip.click;
        await(Drain);
        Check((FStore.GetValue(ActivationCount) = 1) and (FSemantic = 1),
          'callback retirement preserves the one real committed authored action');
        Check((FLegacy = 0) and (FRenderer.Root = nil) and not FMount.Connected,
          'semantic Unmount retires old authority before stale legacy/default delivery');
        Check((Capture.Serial = LBefore.Serial) and (FNotifications = 0),
          'retired Click cannot publish Fit through its retained original view');
      end;
    acDirectMount, acDirectSpace:
      begin
        { The ordinary direct API remains usable without a renderer key/click
          capture listener. A second physical table observes this exact view;
          it does not allocate another store, query owner or preference engine. }
        LDirect := TJSHTMLElement(document.createElement('table'));
        { The direct adapter contract consumes a Nyx-projected host. Copy its
          standard paint class, but never duplicate the authored DOM identity. }
        LDirect.className := FTable.className;
        FHost.appendChild(LDirect);
        try
          LDirectMount := MountNyxBrowserCollection(LDirect, FView);
          { A newly appended table must have its first actual layout before a
            user can activate its header. Retain the normal observer/RAF path. }
          await(Drain);
          LDirectGrip := TJSHTMLElement(LDirect.querySelector('[data-nyx-column-resize]'));
          Check((LDirectGrip <> nil) and LDirectMount.Connected,
            'direct public mount exposes its actual Fit-only column action');

          if FKind = acDirectSpace then
          begin
            LDirectGrip.focus;
            LOptions := TJSObject.new;
            LOptions['key'] := ' ';
            LOptions['bubbles'] := True;
            LOptions['cancelable'] := True;
            LDirectGrip.dispatchEvent(TActivationKey.new('keydown', LOptions));
            Check(Capture.Serial = LBefore.Serial,
              'direct mount Space press does not activate before release');
            LDirectGrip.dispatchEvent(TActivationKey.new('keyup', LOptions));
          end
          else
          begin
            LDirectGrip.click;
          end;
          await(Drain);
          Check(LDirectMount.ErrorText = '',
            'direct mount Fit reports no metric/publication failure: ' + LDirectMount.ErrorText);
          CheckFit(LBefore.Serial);
          CheckDelivery(0);
        finally

          if LDirectMount <> nil then
          begin
            LDirectMount.Disconnect;
          end;
          LDirectMount := nil;
          LDirect.remove;
        end;
      end;
    acSpaceRelease, acSpaceRepeat, acSpaceBlur, acSpaceWindowBlur,
    acSpaceConsumedRelease, acSpaceDisabled, acSpaceRetirement, acSpaceLayoutChanged:
      begin
        FGrip.focus;
        KeyPhase('keydown', ' ');
        CheckDelivery(0);
        Check((Capture.Serial = LBefore.Serial) and (FNotifications = 0),
          'holding Space has no Click action or Fit publication before release');
        await(Drain);
        CheckDelivery(0);

        if FKind = acSpaceRepeat then
        begin
          KeyPhase('keydown', ' ', True);
          KeyPhase('keydown', ' ', True);
          CheckDelivery(0);
        end;

        if FKind = acSpaceBlur then
        begin
          FGrip.blur;
          FGrip.focus;
        end;

        if FKind = acSpaceWindowBlur then
        begin
          window.dispatchEvent(TJSEvent.new('blur'));
        end;

        if FKind = acSpaceDisabled then
        begin
          FMount.SetInteraction(False, False);
          FMount.SetInteraction(True, False);
        end;

        if FKind = acSpaceRetirement then
        begin
          FRenderer.Unmount;
        end;

        if FKind = acSpaceLayoutChanged then
        begin
          LStage := FColumns.PrepareAutoSize(Capture, NyxColumnIdentity(ServiceName), 80);
          PublishNyxGroup([LStage]);
          LStage := nil;
          LAfter := Capture;
          FNotifications := 0;
        end;
        KeyPhase('keyup', ' ');
        await(Drain);

        if FKind in [acSpaceRelease, acSpaceRepeat] then
        begin
          CheckDelivery(1);
          CheckFit(LBefore.Serial);
          Check(FKeyUps = 1, 'Space release delivers its ordinary KeyUp phase once');
          KeyPhase('keyup', ' ');
          await(Drain);
          CheckDelivery(1);
          Check(Capture.Serial = LBefore.Serial + 1,
            'an unmatched repeated release cannot reactivate the old press');
        end
        else if FKind = acSpaceLayoutChanged then
        begin
          CheckDelivery(1);
          Check((Capture.Serial = LAfter.Serial) and
            (Capture.Preferences.ToData.ToJSON = LAfter.Preferences.ToData.ToJSON) and
            (FNotifications = 0),
            'changed layout retains canonical activation but refuses the original stale Fit');
        end
        else
        begin
          CheckDelivery(0);
          Check((Capture.Serial = LBefore.Serial) and (FNotifications = 0),
            'blur, disable, consumed release or retirement terminally cancels held Space');
        end;
      end;
  end;
  CheckUnchangedData;
end;

procedure Run; async;
var
  LKind: TActivationCase;
  LReview: TActivationReview;
  LStatus: TJSHTMLElement;
begin
  try
    for LKind := Low(TActivationCase) to High(TActivationCase) do
    begin
      document.body.setAttribute('data-column-activation-case', CCaseNames[LKind]);
      LReview := TActivationReview.Create(LKind);
      try
        await(LReview.Run);
      finally
        LReview.Free;
      end;
    end;
    document.body.setAttribute('data-column-activation-checks', IntToStr(GChecks));
    document.body.setAttribute('data-column-activation', 'passed');
    LStatus := TJSHTMLElement(document.createElement('p'));
    LStatus.id := 'column-activation-result';
    LStatus.textContent := 'PASS ' + IntToStr(GChecks) + ' actual column activation checks';
    document.body.appendChild(LStatus);
    console.log('PASS ', GChecks, ' actual column activation checks');
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-column-activation-checks', IntToStr(GChecks));
      document.body.setAttribute('data-column-activation-error', E.Message);
      document.body.setAttribute('data-column-activation', 'failed');
      LStatus := TJSHTMLElement(document.createElement('p'));
      LStatus.id := 'column-activation-result';
      LStatus.textContent := 'FAIL ' + E.Message + ' / checks ' + IntToStr(GChecks);
      document.body.appendChild(LStatus);
    end;
  end;
end;

begin
  Run;
end.
