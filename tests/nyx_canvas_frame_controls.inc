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

type
  { Closed actions by a fixture validator. Each targets the one ordinary
    coordinated-publication boundary; no production hook or model API is added. }
  TFrameInterruption = (fiAccept, fiReject, fiValidationNotification,
    fiCancel, fiPage, fiOwner, fiView, fiScene,
    fiResize, fiNotification);

  { Fixture-owned weak application lease/receivers. State and views stay owned by
    the enclosing actual-control harness. Callbacks are disconnected before this
    receiver dies; they never free state inside its callbacks. }
  TFrameOwner = class
  public
    Scheduler: INyxScheduler;
    Generation: INyxCancellationScope;
    Lease: INyxApplicationContextLease;
    Collections: INyxCollections;
    Scope: INyxCancellationScope;
    Mode: TFrameInterruption;
    Watching: Boolean;
    ExpectedText: TNyxText;
    ExpectedPayload: TNyxText;
    ExpectedLeft: Double;
    ExpectedCameraX: Double;
    Observations: Integer;
    function ResolveState: TNyxState;
    function ResolveCollections: INyxCollections;
    function ResolveGeneration: INyxCancellationScope;
    procedure Validate(ACandidate: TNyxState; AChanges: TNyxStateChanges);
    procedure Observe(AState: TNyxState; AChanges: TNyxStateChanges);
    procedure Renew;
  end;

  { Two ordinary registrations borrow only harness globals, so neither callback
    retains a view through its own event router. The second observes the complete
    first publication in registration order, using the same actual Nyx button. }
  TFrameDispatch = class(TNyxEventCallback)
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

  { Worker computation owns immutable code/capture only. Native execution must
    enter a real worker; no view, state store or UI courier is read here. }
  TFrameProducer = class(TInterfacedObject, INyxSelector<Integer, INyxProgramResult>)
  public
    ProgramValue: INyxProgram;
    Capture: INyxCanvasProgramSnapshot;
    Inputs: INyxProgramValues;
    Scope: INyxCancellationScope;
    function Project(const AValue: Integer): INyxProgramResult;
  end;

  { The existing query runner enters this managed courier only on UI. Delivery
    holds revocable context authority and performs the whole prepared publication. }
  TFrameCourier = class(TInterfacedObject, INyxQueryResultPort<INyxProgramResult>)
  public
    Delivery: INyxCanvasProgramDelivery;
    function Publish(const AValues: TNyxSequenceArray<INyxProgramResult>): Boolean;
  end;

var
  GFrameDelivery: INyxCanvasProgramDelivery;
  GFrameResult: INyxProgramResult;
  GFrameCalls: Integer;
  GFrameApplied: Boolean;

procedure TFrameDispatch.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin
  Inc(GFrameCalls);

  if GFrameCalls = 1 then
  begin
    GFrameApplied := GFrameDelivery.Apply(GFrameResult);
  end
  else
  begin
    Check(GFrameApplied and GState.GetValue(NyxBooleanState('layout-ready')),
      'A second ordinary event registration sees the complete first publication');
  end;
end;

function TFrameProducer.Project(const AValue: Integer): INyxProgramResult;
begin
  {$ifndef PAS2JS}

  if GetCurrentThreadID = MainThreadID then
  begin
    raise Exception.Create('Threaded canvas program must execute on a real worker');
  end;
  {$endif}
  Result := ProgramValue.Run(Inputs,
    NyxProgramRun.Grants(CanvasProgramGrants).Queries(Capture).Cancellation(Scope));
end;

function TFrameCourier.Publish(const AValues: TNyxSequenceArray<INyxProgramResult>): Boolean;
begin
  Check((Length(AValues) = 1) and (AValues[0] <> nil),
    'Scheduled canvas result arrives as one complete owned frame on UI');
  Result := Delivery.Apply(AValues[0]);
end;

{$ifdef PAS2JS}
{ A real timer turn permits the ordinary scheduler/resize observer to run;
  promise microtasks alone would starve deferred UI work. No clock is virtualized. }
function FrameTurn: TJSPromise;
begin
  Result := TJSPromise.new(procedure(AResolve, AReject: TJSPromiseResolver)
    begin
      window.setTimeout(procedure
        begin
          AResolve(Undefined);
        end, 25);
    end);
end;
{$endif}

function TFrameOwner.ResolveState: TNyxState;
begin
  Result := GState;
end;

function TFrameOwner.ResolveCollections: INyxCollections;
begin
  Result := Collections;
end;

function TFrameOwner.ResolveGeneration: INyxCancellationScope;
begin
  Result := Generation;
end;

procedure TFrameOwner.Renew;
begin

  if (Lease <> nil) and Lease.Context.Active then
  begin
    Lease.Close;
  end;
  Generation := NewNyxCancellationScope;
  Lease := NewNyxApplicationContext(Scheduler, ResolveState,
    ResolveCollections, ResolveGeneration);
end;

procedure TFrameOwner.Validate(ACandidate: TNyxState; AChanges: TNyxStateChanges);
var
  LLayouts: INyxCanvasSceneLayouts;
begin

  if not Watching then
  begin
    Exit;
  end;
  Supports(GScene, INyxCanvasSceneLayouts, LLayouts);
  Check((LLayouts.PlacementFor(GReply).Bounds.Left = ExpectedLeft - 20) and
    (GView.Camera.WorldCenter.X = ExpectedCameraX - 10),
    'State validator sees the entire previous scene/camera before any install');
  Check(ACandidate.GetValue(NyxTextState('layout-summary')) = ExpectedText,
    'State validator sees the complete detached program output candidate');
  case Mode of
    fiReject: raise ENyxState.Create('Fixture refuses this complete state candidate');
    fiValidationNotification: raise ENyxPublicationNotification.CreateReceiverFailure(
      'Fixture validator throws a notification-typed exception before installation');
    fiCancel: Scope.Cancel;
    fiPage: Generation.Cancel;
    fiOwner: Lease.Close;
    fiView: GView.Disconnect;
    fiScene: GScene.Place(GReply, LLayouts.PlacementFor(GReply));
    fiResize:
    begin
      {$ifdef PAS2JS}
      GView.Surface.style.setProperty('width', IntToStr(GView.Surface.clientWidth - 1) + 'px');
      {$else}
      { The normal surface is client-aligned. Temporarily own its allocation
        so the fixture's one-pixel resize is not immediately undone by LCL. }
      GView.Surface.Align := alNone;
      GView.Surface.Width := GView.Surface.Width - 1;
      {$endif}
    end;
  else
    begin
      { Acceptance and notification-only failure do not alter validation. }
    end;
  end;
end;

procedure TFrameOwner.Observe(AState: TNyxState; AChanges: TNyxStateChanges);
var
  LLayouts: INyxCanvasSceneLayouts;
begin

  if not Watching then
  begin
    Exit;
  end;
  Supports(GScene, INyxCanvasSceneLayouts, LLayouts);
  Check((LLayouts.PlacementFor(GReply).Bounds.Left = ExpectedLeft) and
    (GView.Camera.WorldCenter.X = ExpectedCameraX),
    'State observer sees the newly installed scene and camera together');
  Check((AState.GetValue(NyxTextState('layout-summary')) = ExpectedText) and
    AState.GetValue(NyxBooleanState('layout-ready')) and
    (AState.GetValue(NyxIntegerState('layout-count')) = GScene.Count) and
    (AState.GetValue(NyxTextState('layout-payload')) = ExpectedPayload),
    'State observer sees all scalar families in the same installed frame');
  Inc(Observations);

  if Mode = fiNotification then
  begin
    raise Exception.Create('Fixture observer refuses after complete publication');
  end;
end;

{ Compare every ordered public scalar entry, independent of internal vectors or
  a renderer snapshot. A refused proposal may retain no hidden partial state. }
function SameFrameState(ALeft, ARight: TNyxState): Boolean;
var
  LIndex: Integer;
begin
  Result := ALeft.Count = ARight.Count;

  if not Result then
  begin
    Exit;
  end;
  for LIndex := 0 to ALeft.Count - 1 do
  begin

    if (ALeft.Key(LIndex) <> ARight.Key(LIndex)) or
      not ALeft.Value(ALeft.Key(LIndex)).SameValue(ARight.Value(ARight.Key(LIndex))) then
    begin
      Exit(False);
    end;
  end;
end;

{$ifdef PAS2JS}
function ProgramFrames: JSValue; async;
{$else}
procedure ProgramFrames;
{$endif}
var
  LOwner: TFrameOwner;
  LSubscription: TNyxStateSubscription;
  LDelivery: INyxCanvasProgramDelivery;
  LPartial: INyxCanvasProgramDelivery;
  LCapture: INyxCanvasProgramSnapshot;
  LResult: INyxProgramResult;
  LProgram: INyxProgram;
  LInputs: INyxProgramValues;
  LBindings: INyxProgramStateBindings;
  LView: INyxCanvasLayoutView;
  LCoordinated: INyxCanvasCoordinatedLayoutView;
  LGeometry: INyxCanvasProgramFrameSnapshot;
  LPrepared: INyxCanvasProgramLayout;
  LCompanion: INyxPreparedPublication;
  LLayouts: INyxCanvasSceneLayouts;
  LCard: INyxCard;
  LLabel: INyxLabel;
  LFirst: INyxEventSubscription;
  LSecond: INyxEventSubscription;
  LStateBefore: TNyxState;
  LSceneBefore: Integer;
  LPresentationBefore: Integer;
  LStateRevision: Integer;
  LLeftBefore: Double;
  LCameraBefore: TNyxCanvasCamera;
  LMode: TFrameInterruption;
  LFailed: Boolean;
  LApplied: Boolean;
  LChecksBefore: Integer;
  LOrdinal: Integer;
  LWidth: Integer;
  LFace: {$ifdef PAS2JS}TJSHTMLElement{$else}TControl{$endif};
  LProducer: TFrameProducer;
  LProducerLease: INyxSelector<Integer, INyxProgramResult>;
  LCourier: TFrameCourier;
  LCourierLease: INyxQueryResultPort<INyxProgramResult>;
  LRun: INyxQueryRun;
  LStarted: Double;
  LPhysicalLeft: Double;
  {$ifndef PAS2JS}
  LBitmap: TBitmap;
  LPNG: TPortableNetworkGraphic;
  LAncestor: TControl;
  LScreenDC: HDC;
  LOrigin: Types.TPoint;
  {$endif}

  function PhysicalLeft: Double;
  begin
    {$ifdef PAS2JS}
    Result := GView.InputFor(GReply, NyxControl('reply-memo')).getBoundingClientRect.left;
    {$else}
    Result := GView.InputFor(GReply, NyxControl('reply-memo')).ClientToScreen(Types.Point(0, 0)).X;
    {$endif}
  end;

  procedure Prepare(const AGeometry: TNyxText = ''; const ACaption: TNyxText = '');
  var
    LSource: TNyxText;
    LScene: INyxCanvasSnapshot;
    LIndex: Integer;
    LReply: TNyxText;
  begin
    Inc(LOrdinal);
    LCapture := CaptureNyxCanvasProgram(GScene, GView.Camera);
    LDelivery := CaptureNyxCanvasProgramDelivery(GView, LCapture, LOwner.Lease.Context, LBindings);
    LOwner.ExpectedText := 'Arrangement ' + TNyxText(IntToStr(LOrdinal));

    if ACaption <> '' then
    begin
      LOwner.ExpectedText := ACaption;
    end;
    LInputs := NyxProgramValues.WithValue(NyxProgramSymbol('CaptionText'),
      NyxProgramValue(LOwner.ExpectedText)).WithValue(NyxProgramSymbol('RawPayload'),
      NyxProgramValue(LOwner.ExpectedPayload));
    LOwner.ExpectedLeft := LLayouts.PlacementFor(GReply).Bounds.Left + 20;
    LOwner.ExpectedCameraX := GView.Camera.WorldCenter.X + 10;
    { Earlier ownership tests deliberately remove/reinsert the reply. Its
      current ordered snapshot index is authoritative, not its historical slot. }
    LScene := GScene.Snapshot;
    LReply := '';
    for LIndex := 0 to LScene.Count - 1 do
    begin

      if LScene.Items[LIndex].Item.Name = GReply.Name then
      begin
        LReply := 'SceneItem(' + TNyxText(IntToStr(LIndex)) + ')';
        Break;
      end;
    end;
    Check(LReply <> '', 'Program capture identifies the current ordered reply item');
    LSource := 'begin Summary := CaptionText; Payload := RawPayload; ' +
      'Count := SceneCount(); Ready := True; Zoom := CameraZoom(); ' +
      'MoveItem(' + LReply + ', ItemX(' + LReply + ') + 20, ItemY(' + LReply + ')); ' +
      'MoveItem(SceneItem(SceneCount() - 1), CameraX() - 80, CameraY() + 120); ' +
      'SetCamera(CameraX() + 10, CameraY(), CameraZoom()); ' + AGeometry + ' end.';
    LProgram := CompileNyxProgram(LSource, LCapture.Contract
      .Input(NyxProgramSymbol('CaptionText'), NyxProgramType(npkText))
      .Input(NyxProgramSymbol('RawPayload'), NyxProgramType(npkText))
      .Output(NyxProgramSymbol('Summary'), NyxProgramType(npkText))
      .Output(NyxProgramSymbol('Count'), NyxProgramType(npkInteger))
      .Output(NyxProgramSymbol('Ready'), NyxProgramType(npkBoolean))
      .Output(NyxProgramSymbol('Zoom'), NyxProgramType(npkNumber))
      .Output(NyxProgramSymbol('Payload'), NyxProgramType(npkText)));
    LResult := LProgram.Run(LInputs,
      NyxProgramRun.Grants(CanvasProgramGrants).Queries(LCapture));
  end;

  procedure Baseline;
  begin
    FreeAndNil(LStateBefore);
    LStateBefore := GState.Clone;
    LSceneBefore := GScene.Revision;
    LPresentationBefore := LView.PresentationRevision;
    LStateRevision := GState.Revision;
    LLeftBefore := LLayouts.PlacementFor(GReply).Bounds.Left;
    LCameraBefore := GView.Camera;
    LPhysicalLeft := PhysicalLeft;
  end;

  procedure AssertRefused(AIndependentScene: Boolean = False; ARetiredView: Boolean = False);
  begin
    Check(SameFrameState(GState, LStateBefore) and (GState.Revision = LStateRevision),
      'Refused program frame retains every scalar entry and its revision');
    Check((LLayouts.PlacementFor(GReply).Bounds.Left = LLeftBefore) and
      (GScene.Revision = LSceneBefore + Ord(AIndependentScene)),
      'Refused program frame retains scene placement; only an independent edit may advance');

    if ARetiredView then
    begin
      Check(not GView.Connected and not GState.Busy,
        'Retired view performs no later presentation and releases state reservations');
    end
    else
    begin
      Check((LView.PresentationRevision = LPresentationBefore) and
        (GView.Camera.WorldCenter.X = LCameraBefore.WorldCenter.X) and not GState.Busy,
        'Refusal retains camera/presentation and releases every state reservation');
      Check(PhysicalLeft = LPhysicalLeft,
        'Rejected program frame cannot reconcile its proposed physical editor position');
    end;
  end;

begin
  {$ifdef PAS2JS}Result := Undefined;{$endif}
  LChecksBefore := GChecks;
  LOrdinal := 0;
  LStateBefore := nil;
  LOwner := TFrameOwner.Create;
  LSubscription := nil;
  try
    LOwner.Scheduler := NewNyxScheduler;
    LOwner.Collections := NewNyxCollections(GDocument.Collections);
    LOwner.Renew;
    LOwner.ExpectedPayload := 'Owned program payload';
    Supports(GView, INyxCanvasLayoutView, LView);
    Supports(GScene, INyxCanvasSceneLayouts, LLayouts);
    LBindings := NyxProgramStateBindings
      .OutputState(NyxProgramSymbol('Summary'), NyxTextState('layout-summary'))
      .OutputState(NyxProgramSymbol('Count'), NyxIntegerState('layout-count'))
      .OutputState(NyxProgramSymbol('Ready'), NyxBooleanState('layout-ready'))
      .OutputState(NyxProgramSymbol('Zoom'), NyxNumberState('layout-zoom'))
      .OutputState(NyxProgramSymbol('Payload'), NyxTextState('layout-payload'));
    LCard := NewNyxCard('program-summary');
    LLabel := NewNyxLabel('program-summary-label');
    LLabel.Binds.Text(NyxTextState('layout-summary')).Done;
    LCard.Add(LLabel);
    GScene.AddControl(NyxCanvasItem('Program summary'), NyxCanvasPlacement(GLayer,
      NyxCanvasBounds(10000, 10000, 220, 80)), LCard);
    GView.Refresh;
    LSubscription := GState.Subscribe(LOwner.Observe, LOwner.Validate);
    Prepare;
    Baseline;
    LPartial := CaptureNyxCanvasProgramDelivery(GView, LCapture, LOwner.Lease.Context,
      NyxProgramStateBindings.OutputState(NyxProgramSymbol('Summary'), NyxTextState('layout-summary')));
    LFailed := False;
    try
      LPartial.Apply(LResult);
    except
      on E: ENyxProgram do
      begin
        LFailed := E.Diagnostic.Kind = npdType;
      end;
    end;
    Check(LFailed, 'Unmapped scalar outputs refuse before either owner changes');
    AssertRefused;
    LFailed := False;
    try
      LPartial := CaptureNyxCanvasProgramDelivery(GView, LCapture, LOwner.Lease.Context,
        NyxProgramStateBindings.OutputState(NyxProgramSymbol('Summary'), NyxIntegerState('layout-summary')));
    except
      on E: ENyxProgram do
      begin
        LFailed := E.Diagnostic.Kind = npdType;
      end;
    end;
    Check(LFailed, 'A typed output destination must match the existing state family');
    AssertRefused;
    LFailed := False;
    try
      LPrepared := LCapture.Prepare(LResult, GScene);
    except
      on E: ENyxProgram do
      begin
        LFailed := E.Diagnostic.Kind = npdType;
      end;
    end;
    Check(LFailed, 'The original scene-only API still refuses to discard scalar outputs');
    AssertRefused;
    LOwner.Watching := True;
    GFrameDelivery := LDelivery;
    GFrameResult := LResult;
    GFrameCalls := 0;
    GFrameApplied := False;
    LFirst := GView.EventsFor(GReply).On(NyxControlEvents('send-button'), ntClick)
      .Subscribe(TFrameDispatch.Create);
    LSecond := GView.EventsFor(GReply).On(NyxControlEvents('send-button'), ntClick)
      .Subscribe(TFrameDispatch.Create);
    try
      {$ifdef PAS2JS}
      TJSHTMLButtonElement(GView.RendererFor(GReply).ElementFor('send-button')).click;
      {$else}
      TNyxLCLButton(GView.RendererFor(GReply).ControlFor('send-button')).Click;
      {$endif}
      Check(GFrameApplied and (GFrameCalls = 2),
        'Actual embedded Nyx button publishes one complete scalar/scene/camera frame');
    finally
      LFirst.Cancel;
      LSecond.Cancel;
      GFrameDelivery := nil;
      GFrameResult := nil;
    end;
    Check((GScene.Revision = LSceneBefore + 1) and
      (LView.PresentationRevision = LPresentationBefore + 1) and
      (GState.Revision = LStateRevision + 1) and (LOwner.Observations = 1),
      'All three owners advance exactly once; observation starts after complete installation');
    LFace := GView.RendererFor(NyxCanvasItem('Program summary'))
      .{$ifdef PAS2JS}ElementFor{$else}ControlFor{$endif}('program-summary-label');
    Check(LFace <> nil, 'Newly visible control binds before state reservation and mounts on both targets');
    {$ifdef PAS2JS}
    Check(LFace.textContent = LOwner.ExpectedText, 'Browser bound status shows the same accepted frame');
    {$else}
    Check(TNyxText(TLabel(LFace).Caption) = LOwner.ExpectedText,
      'Native bound status shows the same accepted frame');
    {$endif}
    for LMode := fiReject to fiResize do
    begin
      LOwner.Watching := False;
      LOwner.Renew;
      LOwner.Mode := LMode;
      LOwner.Scope := NewNyxCancellationScope;
      Prepare;
      Baseline;
      LWidth := GView.Surface.{$ifdef PAS2JS}clientWidth{$else}Width{$endif};
      LOwner.Watching := True;
      LFailed := False;
      LApplied := False;
      try
        LApplied := LDelivery.Apply(LResult, LOwner.Scope);
      except
        on E: ENyxState do
        begin

          if not (LMode in [fiReject, fiValidationNotification]) then
          begin
            raise;
          end;
          LFailed := True;
        end;
        on E: ENyxCanvasScene do
        begin

          if not (LMode in [fiScene, fiResize]) then
          begin
            raise;
          end;
          LFailed := True;
        end;
      end;
      Check(not LApplied and ((LMode in [fiCancel, fiPage, fiOwner, fiView]) or LFailed),
        'Admission interruption ' + TNyxText(IntToStr(Ord(LMode))) +
        ' must refuse before install / applied=' + TNyxText(BoolToStr(LApplied, True)) +
        ' / expected refusal=' + TNyxText(BoolToStr(LFailed, True)));
      AssertRefused(LMode = fiScene, LMode = fiView);
      LOwner.Watching := False;

      if LMode = fiView then
      begin
        Supports(GView, INyxCanvasCoordinatedLayoutView, LCoordinated);
        Supports(LCapture, INyxCanvasProgramFrameSnapshot, LGeometry);
        LPrepared := LGeometry.PrepareFrameGeometry(LResult, GScene, 1024);
        LCompanion := GState.PrepareApply([], GState.Revision);
        try
          Check(not LCoordinated.ApplyCoordinatedLayout(LPrepared.Plan, LPrepared.Camera,
            LPresentationBefore, LCompanion) and not LPrepared.Plan.Committed,
            'Direct coordinated delivery to a retired view performs no publication');
        finally
          LCompanion.Retire;
        end;
        {$ifdef PAS2JS}
        GView := MountNyxBrowserCanvasWorkspace(GWorkspace, GDocument, GHost, GState);
        {$else}
        GView := MountNyxLCLCanvasWorkspace(GWorkspace, GDocument, GHost, GState);
        Application.ProcessMessages;
        {$endif}
        GView.SetCamera(LCameraBefore);
        Supports(GView, INyxCanvasLayoutView, LView);
        GInput := {$ifdef PAS2JS}TJSHTMLTextAreaElement{$else}TMemo{$endif}(
          GView.InputFor(GReply, NyxControl('reply-memo')));
        LFace := GView.RendererFor(NyxCanvasItem('Program summary'))
          .{$ifdef PAS2JS}ElementFor{$else}ControlFor{$endif}('program-summary-label');
      end;

      if LMode = fiResize then
      begin
        {$ifdef PAS2JS}
        GView.Surface.style.setProperty('width', IntToStr(LWidth) + 'px');
        {$else}
        GView.Surface.Width := LWidth;
        GView.Surface.Align := alClient;
        Application.ProcessMessages;
        {$endif}
      end;
    end;
    LOwner.Renew;
    LOwner.Mode := fiAccept;
    Prepare('MoveItem(SceneItem(0), CameraX(), CameraY()); ' +
      'SizeItem(SceneItem(0), 1000000000, 200);');
    Baseline;
    LFailed := False;
    try
      LDelivery.Apply(LResult);
    except
      on E: ENyxCanvasScene do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'Impossible physical allocation refuses a complete scalar/scene proposal');
    AssertRefused;
    Prepare;
    LOwner.Watching := False;
    GState.SetValue(NyxTextState('layout-summary'), 'An independent user edit');
    Baseline;
    LFailed := False;
    try
      LDelivery.Apply(LResult);
    except
      on E: ENyxState do
      begin
        LFailed := True;
      end;
      on E: ENyxProgram do
      begin
        { A live bound edit may also reallocate the native workspace. Either
          captured owner must refuse; both exact post-edit baselines survive. }
        LFailed := E.Diagnostic.Kind = npdStale;
      end;
    end;
    Check(LFailed, 'Captured state revision cannot overwrite an independent user edit');
    AssertRefused;
    Prepare;
    Baseline;
    LOwner.Mode := fiNotification;
    LOwner.Watching := True;
    LFailed := False;
    try
      LDelivery.Apply(LResult);
    except
      on E: ENyxPublicationNotification do
      begin
        LFailed := E.HasReceiverMessage and
          (Pos('Fixture observer refuses after complete publication', E.ReceiverMessage) > 0);
      end;
    end;
    Check(LFailed and (GScene.Revision = LSceneBefore + 1) and
      (GState.Revision = LStateRevision + 1) and
      (LView.PresentationRevision = LPresentationBefore + 1),
      'Observer failure reports the committed whole frame without fabricated rollback');
    Check((GView.InputFor(GReply, NyxControl('reply-memo')) = GInput) and not GState.Busy,
      'Coordinated publication retains the actual editor and releases reservations');
    {$ifdef PAS2JS}
    Check(LFace.textContent = LOwner.ExpectedText,
      'Browser physical notification still completes after another receiver fails');
    {$else}
    Check(TNyxText(TLabel(LFace).Caption) = LOwner.ExpectedText,
      'Native physical notification still completes after another receiver fails');
    {$endif}
    LOwner.Watching := False;
    LOwner.Mode := fiAccept;
    Prepare('', TNyxText('Rejected control text') + #0 + 'exact');
    Baseline;
    LFailed := False;
    try
      LDelivery.Apply(LResult);
    except
      on E: Exception do
      begin

        if Pos('Portable control text cannot contain NUL', E.Message) <> 1 then
        begin
          raise;
        end;
        LFailed := True;
      end;
    end;
    Check(LFailed, 'Portable control projection rejects NUL before any frame owner installs');
    AssertRefused;
    LOwner.ExpectedPayload := TNyxText('Exact payload 🌙') + #0 + 'retained';
    Prepare('', 'Unicode qualification 🌙');
    Baseline;
    LOwner.Watching := True;
    Check(LDelivery.Apply(LResult), 'Supplementary caption and exact data payload publish in one complete frame');
    Check((GState.GetValue(NyxTextState('layout-summary')) = LOwner.ExpectedText) and
      (GState.GetValue(NyxTextState('layout-payload')) = LOwner.ExpectedPayload),
      'Coordinated state preserves supplementary text and unbound NUL payload exactly');
    {$ifdef PAS2JS}
    Check(LFace.textContent = LOwner.ExpectedText, 'Actual browser label displays the exact supplementary caption');
    {$else}
    Check(TNyxText(TLabel(LFace).Caption) = LOwner.ExpectedText,
      'Actual native label displays the exact supplementary caption');
    {$endif}
    LOwner.Watching := False;
    LOwner.ExpectedPayload := 'Owned program payload';
    {$ifdef PAS2JS}
    await(FrameTurn);
    {$else}
    Application.ProcessMessages;
    {$endif}
    Prepare;
    Baseline;
    LOwner.Watching := True;
    LProducer := TFrameProducer.Create;
    LProducerLease := LProducer;
    LProducer.ProgramValue := LProgram;
    LProducer.Capture := LCapture;
    LProducer.Inputs := LInputs;
    LProducer.Scope := LOwner.Generation;
    LCourier := TFrameCourier.Create;
    LCourierLease := LCourier;
    LCourier.Delivery := LDelivery;
    LRun := TNyxQuery<INyxProgramResult>.Submit(
      TNyxSelect<Integer, INyxProgramResult>.From(TNyxSequence<Integer>.From([0]), LProducerLease),
      LOwner.Scheduler, LCourierLease, {$ifdef PAS2JS}neUIQueue{$else}neThreaded{$endif},
      LOwner.Generation);
    LStarted := {$ifdef PAS2JS}TJSDate.now{$else}GetTickCount64{$endif};
    while not (LRun.Phase in [nqpSucceeded, nqpFailed, nqpDiscarded, nqpCancelled]) do
    begin
      {$ifdef PAS2JS}
      await(FrameTurn);
      {$else}
      CheckSynchronize;
      Application.ProcessMessages;
      Sleep(1);
      {$endif}

      if {$ifdef PAS2JS}TJSDate.now{$else}GetTickCount64{$endif} - LStarted > 20000 then
      begin
        raise Exception.Create('Scheduled complete canvas frame exceeded its real-clock budget');
      end;
    end;
    Check(LRun.Phase = nqpSucceeded, 'Existing scheduler delivers the complete program frame: ' + LRun.Failure);
    Check((GScene.Revision = LSceneBefore + 1) and
      (GState.Revision = LStateRevision + 1) and
      (LView.PresentationRevision = LPresentationBefore + 1),
      'Scheduled program publication advances scene, scalar state and camera/view together');
    Check(TNyxCodec.Encode(GDocument) = GBefore,
      'Complete runtime program publication preserves authored defaults and document bytes');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-canvas-frame-checks', IntToStr(GChecks - LChecksBefore));
    {$else}
    WriteLn('PASS ', GChecks - LChecksBefore, ' coordinated actual canvas/state checks');

    if ParamCount > 0 then
    begin
      { Flush ordinary native painting after the queued result; caption/model
        assertions alone cannot qualify a visible newly mounted status face. }
      Application.ProcessMessages;
      GHost.Update;
      WriteLn('Native summary / visible=', BoolToStr(LFace.Visible, True),
        ' / parent visible=', BoolToStr(LFace.Parent.Visible, True),
        ' / screen=', LFace.ClientToScreen(Types.Point(0, 0)).X, ',',
        LFace.ClientToScreen(Types.Point(0, 0)).Y,
        ' / extent=', LFace.Width, 'x', LFace.Height);
      LAncestor := LFace;
      while LAncestor <> nil do
      begin
        WriteLn('Native summary ancestor / ', LAncestor.ClassName,
          ' / visible=', BoolToStr(LAncestor.Visible, True),
          ' / rect=', LAncestor.Left, ',', LAncestor.Top, ',',
          LAncestor.Width, ',', LAncestor.Height);
        LAncestor := LAncestor.Parent;
      end;
      LBitmap := GHost.GetFormImage;
      LPNG := TPortableNetworkGraphic.Create;
      try
        LPNG.Assign(LBitmap);
        LPNG.SaveToFile(ParamStr(1));
        { PaintTo/GetFormImage can disagree with overlapping Win32 child
          windows. Qualify actual foreground pixels separately; refuse capture
          of any other foreground application rather than exposing its desktop. }
        GHost.BringToFront;
        Application.ProcessMessages;
        GHost.Update;

        if Windows.GetForegroundWindow <> GHost.Handle then
        begin
          raise Exception.Create('Owned native canvas is not the foreground window');
        end;
        LOrigin := GHost.ClientToScreen(Types.Point(0, 0));
        LScreenDC := Windows.GetDC(0);
        try

          if not Windows.BitBlt(LBitmap.Canvas.Handle, 0, 0, LBitmap.Width,
            LBitmap.Height, LScreenDC, LOrigin.X, LOrigin.Y, SRCCOPY) then
          begin
            raise Exception.Create('Owned native canvas pixel capture failed');
          end;
        finally
          Windows.ReleaseDC(0, LScreenDC);
        end;
        LPNG.Assign(LBitmap);
        LPNG.SaveToFile(ChangeFileExt(ParamStr(1), '') + '-screen.png');
      finally
        LPNG.Free;
        LBitmap.Free;
      end;
    end;
    {$endif}
  finally
    LOwner.Watching := False;
    LSubscription.Free;
    LDelivery := nil;
    LOwner.Lease.Close;
    LOwner.Scheduler.Shutdown;
    LOwner.Free;
    LStateBefore.Free;
  end;
end;
