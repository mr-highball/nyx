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

var
  GTools: INyxToolCanvasView;
  GAuthor: INyxToolCanvasRouteAuthoring;
  GRouteEditing: INyxCanvasRouteEditing;
  GToolTarget: {$ifdef PAS2JS}INyxBrowserToolCanvasView{$else}INyxLCLToolCanvasView{$endif};
  GNameFace: {$ifdef PAS2JS}TJSHTMLElement{$else}TControl{$endif};
  GCanvasNameFace: {$ifdef PAS2JS}TJSHTMLElement{$else}TControl{$endif};
  GExportChanges: TNyxCanvasRouteChanges;
  GOldEvents: INyxEvents;
  GKeptContent: INyxNode;
  GRouteOldScene: Integer;
  GRouteOldRevision: Integer;

procedure RetireRouteAuthoring;
begin
  GToolTarget := nil;
  GAuthor := nil;
  GTools := nil;
  GRouteEditing := nil;
  GOldEvents := nil;
  GKeptContent := nil;
end;

function ToolID(const ASuffix: TNyxText): TNyxText;
begin
  Result := GTools.Inspector.Root.ID + ASuffix;
end;

function ToolNode(const ASuffix: TNyxText): TNyxNode;
begin
  Result := GToolTarget.Renderer.Root.Find(ToolID(ASuffix));

  if Result = nil then
  begin
    raise Exception.Create('Missing actual Tool Canvas input: ' + ASuffix);
  end;
end;

procedure ToolClickID(const AID: TNyxText);
begin
  {$ifdef PAS2JS}
  TJSHTMLButtonElement(GToolTarget.Renderer.ElementFor(AID)).click;
  {$else}
  TNyxLCLButton(GToolTarget.Renderer.ControlFor(AID)).Click;
  {$endif}
end;

procedure ToolClick(const ASuffix: TNyxText);
begin
  ToolClickID(ToolID(ASuffix));
end;

procedure ToolChange(const ASuffix, AValue: TNyxText);
var
  {$ifdef PAS2JS}LInput: TJSHTMLElement;{$else}LInput: TControl;{$endif}
begin
  LInput := GToolTarget.Renderer.InputFor(ToolID(ASuffix));
  Check(LInput <> nil, 'route authoring uses an actual mounted host input');
  {$ifdef PAS2JS}

  if LInput is TJSHTMLSelectElement then
  begin
    TJSHTMLSelectElement(LInput).value := AValue;
    LInput.dispatchEvent(TJSEvent.new('change'));
  end
  else
  begin
    TJSHTMLInputElement(LInput).value := AValue;
    LInput.dispatchEvent(TJSEvent.new('input'));
  end;
  {$else}

  if LInput is TComboBox then
  begin
    TComboBox(LInput).ItemIndex := TComboBox(LInput).Items.IndexOf(AValue);
    TComboBox(LInput).OnChange(LInput);
  end
  else
  begin
    TEdit(LInput).Text := AValue;
  end;
  {$endif}
  Check(ToolNode(ASuffix).Prop('value') = AValue,
    'the real host event retains exact route proposal text for ' + ASuffix +
    ': expected ' + AValue + ', received ' + ToolNode(ASuffix).Prop('value') +
    '; binding: ' + GToolTarget.Renderer.LastBindingError);
end;

procedure ToolChoose(const AField, ACaption: TNyxText);
var
  LLabels: TNyxStrings;
  LIndex: Integer;
begin
  LLabels := TNyxStrings.Create;
  try
    LLabels.Text := ToolNode('-event-routes-' + AField).Prop('items');
    for LIndex := 0 to LLabels.Count - 1 do
    begin

      if Copy(LLabels[LIndex], Length(LLabels[LIndex]) - Length(ACaption) + 1,
        Length(ACaption)) = ACaption then
      begin
        ToolChange('-event-routes-' + AField, LLabels[LIndex]);
        Exit;
      end;
    end;
  finally
    LLabels.Free;
  end;
  raise Exception.Create('Missing real route choice: ' + AField + ' / ' + ACaption);
end;

function ToolIdle: Boolean;
begin
  Result := True;

  if GTools.RefreshExecution <> nil then
  begin
    Result := not (GTools.RefreshExecution.Status in [nesPending, nesRunning]);

    if GTools.RefreshExecution.Status = nesFailed then
    begin
      raise Exception.Create('Actual Tool Canvas publication: ' + GTools.RefreshExecution.Failure);
    end;
  end;
end;

procedure EditSecond(const AText: TNyxText);
var
  {$ifdef PAS2JS}LInput: TJSHTMLElement;{$else}LInput: TControl;{$endif}
begin
  LInput := GCanvasView.InputFor(NyxCanvasItem('Second greeting'), NyxControl('name-input'));
  {$ifdef PAS2JS}
  TJSHTMLInputElement(LInput).value := AText;
  LInput.dispatchEvent(TJSEvent.new('input'));
  {$else}
  TEdit(LInput).Text := AText;
  {$endif}
end;

procedure AcknowledgeReload;
begin
  {$ifdef PAS2JS}
  TJSHTMLInputElement(GToolTarget.Renderer.InputFor(ToolID('-route-discard'))).click;
  {$else}
  TCheckBox(GToolTarget.Renderer.InputFor(ToolID('-route-discard'))).Checked := True;
  {$endif}
end;

{ Borrow the runtime's current scene through its optional portable capability;
  the original workspace face deliberately does not expose document content. }
function CanvasScene: INyxCanvasScene;
var
  LRuntime: INyxCanvasRuntimeView;
begin
  Check(Supports(GCanvasView, INyxCanvasRuntimeView, LRuntime),
    'the actual workspace retains its explicit runtime scene capability');
  Result := LRuntime.Scene;
end;

{ An otherwise valid removal cannot escape as a successful prefix when a later
  item target is missing. Observe the installed behavior and exact exported
  intent, rather than comparing private candidate implementation fields. }
procedure RefusePartialRoutes;
var
  LBefore: TNyxCanvasRouteChanges;
  LAfter: TNyxCanvasRouteChanges;
  LRevision: Integer;
  LFailed: Boolean;
  LRuntime: INyxCanvasRuntimeView;
  LBinding: INyxFlowRouteBinding;
begin
  LBefore := GRouteEditing.CaptureRoutes;
  LRevision := GRouteEditing.RouteRevision;
  Supports(GCanvasView, INyxCanvasRuntimeView, LRuntime);
  LBinding := LRuntime.BehaviorFor(NyxCanvasItem('Greeting')).Routes;
  LFailed := False;
  try
    GRouteEditing.ApplyRouteChanges([
      NyxCanvasRoutes(NyxCanvasItem('Greeting'), NyxControl('name-input'), NewNyxFlowRoutes),
      NyxCanvasRoutes(NyxCanvasItem('Second greeting'), NyxControl('missing-input'), NewNyxFlowRoutes)],
      CanvasScene.Revision, LRevision);
  except
    on E: ENyxCanvasScene do
    begin
      LFailed := True;
    end;
  end;
  LAfter := GRouteEditing.CaptureRoutes;
  Check(LFailed and (LAfter[0].ToData.ToJSON = LBefore[0].ToData.ToJSON) and
    (Length(LAfter) = Length(LBefore)) and (GRouteEditing.RouteRevision = LRevision) and
    (LRuntime.BehaviorFor(NyxCanvasItem('Greeting')).Routes = LBinding),
    'a later missing item target refuses the entire removal without changing routes, exports or revision');
end;

procedure Step;
var
  LRuntime: INyxCanvasRuntimeView;
  LRouteEditing: INyxCanvasRouteEditing;
  LAuthor: INyxToolCanvasRouteAuthoring;
  LToolTarget: {$ifdef PAS2JS}INyxBrowserToolCanvasView{$else}INyxLCLToolCanvasView{$endif};
  LFlows: INyxCanvasFlowEditing;
  LSceneLayouts: INyxCanvasSceneLayouts;
  LPlacement: TNyxCanvasPlacement;
  LChanges: TNyxCanvasRouteChanges;
  LDisclosure: INyxDisclosure;
  LFailed: Boolean;
begin

  if (GTools <> nil) and not ToolIdle then
  begin
    Exit;
  end;
  case GStage of
    0:
      begin

        if not SetupCanvas then
        begin
          Exit;
        end;
        Check(Supports(GCanvasView, INyxCanvasRouteEditing, LRouteEditing),
          'actual canvas exposes optional typed per-item route authoring');
        GRouteEditing := LRouteEditing;
        Supports(GCanvasView, INyxCanvasRuntimeView, LRuntime);
        Check((GBindings.Count = 0) and
          (LRuntime.BehaviorFor(NyxCanvasItem('Second greeting')).Routes.Count = 0),
          'two actual reusable instances begin without any graph routes');
        GTools := GApp.View.ToolCanvasView('program-tools');
        GTools.Inspector.SelectControl(NyxCanvasItem('Greeting'), NyxControl('name-input'));
        GTools.Refresh;
        Check(Supports(GTools.Inspector, INyxToolCanvasRouteAuthoring, LAuthor),
          'the existing public Tool Canvas inspector composes the specialized route form');
        GAuthor := LAuthor;
        Supports(GTools, {$ifdef PAS2JS}INyxBrowserToolCanvasView{$else}INyxLCLToolCanvasView{$endif}, LToolTarget);
        GToolTarget := LToolTarget;
        GCanvasNameFace := MainView.InputFor('name-input');
        SubscribeCallbacks;
        ToolClick('-tab-event-routes');
      end;
    1:
      begin
        GNameFace := GToolTarget.Renderer.InputFor(ToolID('-event-routes-name'));
        ToolChange('-event-routes-name', 'Typed once');
        {$ifdef PAS2JS}
        TJSHTMLInputElement(GNameFace).focus;
        TJSHTMLInputElement(GNameFace).setSelectionRange(2, 2);
        {$else}
        TEdit(GNameFace).SetFocus;
        TEdit(GNameFace).SelStart := 2;
        TEdit(GNameFace).SelLength := 0;
        {$endif}
      end;
    2:
      begin
        Check(GToolTarget.Renderer.InputFor(ToolID('-event-routes-name')) = GNameFace,
          'a route keystroke synchronizes without replacing its actual editor');
        {$ifdef PAS2JS}
        Check((document.activeElement = GNameFace) and
          (TJSHTMLInputElement(GNameFace).selectionStart = 2),
          'browser route typing retains exact focus and caret across a real UI turn');
        {$else}
        Check(TEdit(GNameFace).SelStart = 2,
          'native route typing retains its caret across a real UI turn');
        {$endif}
        ToolChange('-event-routes-name', 'Live greeting');
        ToolChoose('event', 'OnChange');
        ToolClick('-event-routes-map-input');
      end;
    3:
      begin
        ToolChoose('output', 'greeting / Caption / Text');
        ToolChoose('destination', 'caption-label / Text');
        ToolClick('-event-routes-map-output');
      end;
    4:
      begin
        Check((Length(GRouteEditing.CaptureRoutes) = 0) and (GBindings.Count = 0),
          'staged actual form mappings create neither runtime authority nor exported overrides');
        ToolClick('-event-routes-apply');
      end;
    5:
      begin
        Supports(GCanvasView, INyxCanvasRuntimeView, LRuntime);
        GBindings := LRuntime.BehaviorFor(NyxCanvasItem('Greeting')).Routes;
        Check((GBindings.Count = 1) and (Length(GRouteEditing.CaptureRoutes) = 1),
          'actual Apply installs the newly authored route and detached intent together');
        Check(MainView.InputFor('name-input') = GCanvasNameFace,
          'adding route topology retains the exact embedded input control');
        RefusePartialRoutes;
        EditName('Grace');
        GRevision := GApp.State.Revision;
      end;
    6:
      begin

        if not Idle(GBindings) then
        begin
          Exit;
        end;

        if not GCaptured then
        begin
          Check((Caption = 'Hello, Grace!') and (GCallbackCount = 2) and
            (GApp.State.Revision = GRevision + 1),
            'actual UI-authored control event runs Pascal and updates its bound caption after ordinary callbacks');
          Check(GOther.State.GetValue(NyxTextState('caption')) = 'Ready for a greeting',
            'a separate application retains its independent runtime defaults');
          GCaptured := True;
          {$ifdef PAS2JS}
          document.body.setAttribute('data-capture-checkpoint', 'dynamic-control-routes');
          Exit;
          {$endif}
        end;
        {$ifdef PAS2JS}

        if document.body.getAttribute('data-capture-observed') <> 'dynamic-control-routes' then
        begin
          Exit;
        end;
        {$endif}
        EditSecond('Other card');
      end;
    7:
      begin
        Supports(GCanvasView, INyxCanvasRuntimeView, LRuntime);
        Check((LRuntime.BehaviorFor(NyxCanvasItem('Second greeting')).Routes.Count = 0) and
          (Caption = 'Hello, Grace!') and (GCallbackCount = 2),
          'another instance of the same template acquires no event route from the first item');
        GExportChanges := GRouteEditing.CaptureRoutes;
        Check((GExportChanges[0].Item.Name = 'Greeting') and
          (GExportChanges[0].Control.ID = 'name-input') and
          (GExportChanges[0].Routes.Route(0).Input(0).Source = nfisEventValue),
          'export captures exact typed per-item intent rather than widget/host authority');
        EditName('First');
        EditName('Latest');
        GRouteEditing.ApplyRouteChanges(GExportChanges, CanvasScene.Revision,
          GRouteEditing.RouteRevision);
        Check(LRuntime.BehaviorFor(NyxCanvasItem('Greeting')).Routes = GBindings,
          'an identical runtime route intent retains its pending fragment and event slots');
      end;
    8:
      begin

        if not Idle(GBindings) then
        begin
          Exit;
        end;
        Check((Caption = 'Hello, Latest!') and (GCallbackCount = 6) and
          (GBindings.Report(0).Field('cancelled').AsInteger >= 1),
          'newly added route topology retains latest-run cancellation and ordinary callbacks');
        ToolChange('-event-routes-name', 'Unfinished route');
        LDisclosure := RetainNyxControl(ToolNode('-event-routes-execution')) as INyxDisclosure;
        ToolClickID(LDisclosure.HeaderButton.ID);
      end;
    9:
      begin
        ToolChange('-event-routes-limit', 'not yet a number');
        ToolClick('-tab-ports');
      end;
    10: ToolClick('-tab-event-routes');
    11:
      begin
        Check((ToolNode('-event-routes-name').Prop('value') = 'Unfinished route') and
          (ToolNode('-event-routes-limit').Prop('value') = 'not yet a number'),
          'hiding/reopening runtime route authoring retains exact unfinished fields');
        ToolChoose('route', 'Live greeting');
        ToolChange('-event-routes-name', 'Draft remains');
        Supports(GCanvasView, INyxCanvasFlowEditing, LFlows);
        LFlows.ApplyFlowChanges([NyxDefineFlow(NyxFlowRef('Greeting flow'),
          LFlows.RuntimeFlows.Definition(NyxFlowRef('Greeting flow')))],
          CanvasScene.Revision, LFlows.FlowRevision);
        ToolClick('-event-routes-apply');
      end;
    12:
      begin
        LChanges := GRouteEditing.CaptureRoutes;
        Check((Pos('stale', ToolNode('-route-feedback').Prop('text')) > 0) and
          (ToolNode('-event-routes-name').Prop('value') = 'Draft remains') and
          (LChanges[0].ToData.ToJSON = GExportChanges[0].ToData.ToJSON),
          'stale actual Apply retains the whole override and unfinished proposal');
        LFailed := False;
        try
          GAuthor.ReloadEventRoutes;
        except
          on E: ENyxModel do
          begin
            LFailed := True;
          end;
        end;
        Check(LFailed and (ToolNode('-event-routes-name').Prop('value') = 'Draft remains'),
          'public reload cannot silently discard unfinished runtime authoring');
        ToolClick('-tab-event-routes');
      end;
    13: ToolClick('-route-reload');
    14:
      begin
        Check(ToolNode('-event-routes-name').Prop('value') = 'Draft remains',
          'actual unacknowledged reload retains the local draft');
        AcknowledgeReload;
        ToolClick('-route-reload');
      end;
    15:
      begin
        Check((ToolNode('-event-routes-name').Prop('value') = 'New route') and
          (Length(GRouteEditing.CaptureRoutes) = 1),
          'acknowledged reload replaces only the local form proposal');
        GRouteOldScene := CanvasScene.Revision;
        GRouteOldRevision := GRouteEditing.RouteRevision;
        GOldEvents := MainView.Events;
        EditName('Retired item');
        GKeptContent := CanvasScene.Control(NyxCanvasItem('Greeting'));
        Supports(CanvasScene, INyxCanvasSceneLayouts, LSceneLayouts);
        LPlacement := LSceneLayouts.PlacementFor(NyxCanvasItem('Greeting'));
        CanvasScene.Remove(NyxCanvasItem('Greeting'));
        GCanvasView.Refresh;
        CanvasScene.AddControl(NyxCanvasItem('Greeting'), LPlacement, GKeptContent);
        GKeptContent := nil;
        GCanvasView.Refresh;
      end;
    16:
      begin
        Supports(GCanvasView, INyxCanvasRuntimeView, LRuntime);
        Check((LRuntime.BehaviorFor(NyxCanvasItem('Greeting')).Routes.Count = 0) and
          (Length(GRouteEditing.CaptureRoutes) = 0) and
          not GOldEvents.HasSubscribers(ntChange) and not GCallbacks[0].Active and
          (Caption = 'Hello, Latest!'),
          'same-name content recreation retires old subscriptions/work and cannot resurrect runtime routes');
        LFailed := False;
        try
          GRouteEditing.ApplyRouteChanges(GExportChanges, GRouteOldScene, GRouteOldRevision);
        except
          on E: ENyxCanvasScene do
          begin
            LFailed := True;
          end;
        end;
        Check(LFailed, 'a retired item proposal cannot bypass exact scene/lifetime admission');
        EditName('Unwired replacement');
        EditName('Independent', True);
        Click(True);
      end;
    17:
      begin

        if not Idle(GOther.FlowRoutes) then
        begin
          Exit;
        end;
        Check((Caption = 'Hello, Latest!') and (Caption(True) = 'Welcome, Independent!'),
          'fresh unwired content and the independent application retain distinct runtime behavior');
        Check(TNyxCodec.Encode(GDocument) = GBefore,
          'runtime control route editing and item retirement preserve every authored document byte');
        GDone := True;
      end;
  end;
  Inc(GStage);
end;
