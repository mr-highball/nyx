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

{ This controller contributes ordinary public Nyx controls only. Target adapters
  choose synchronization versus replacement from the typed result below. }

function TInspector.GetRouteEditor: INyxControlRouteEditor;
begin
  RequireConnected;
  Result := FRouteEditor;
end;

procedure TInspector.ShowEventRoutes(AVisible: Boolean);
begin
  RequireConnected;

  if AVisible and (FRouteEditing = nil) then
  begin
    raise ENyxModel.Create('This canvas does not support runtime control route authoring');
  end;

  if FRouteEditor <> nil then
  begin
    FRouteDraft.Capture(FRouteEditor.ID, FRouteEditor.Node);
  end;
  FTab := titPorts;

  if AVisible then
  begin
    FTab := titEventRoutes;
  end;
  Rebuild;
end;

procedure TInspector.ReloadEventRoutes(ADiscardDraft: Boolean);
begin
  RequireConnected;

  if FRouteEditor <> nil then
  begin
    FRouteDraft.Capture(FRouteEditor.ID, FRouteEditor.Node);
  end;

  if FRouteDraft.Defined and not ADiscardDraft then
  begin
    raise ENyxModel.Create('Acknowledge discarding the unfinished route proposal before reloading');
  end;
  FRouteDraft.Clear;
  FRouteEditor := nil;
  FreeAndNil(FRouteContext);
  FRouteFeedback := '';
  Rebuild;
end;

procedure TInspector.BuildEventRoutes(const AParent: INyxCard);
var
  LContext: TNyxDocument;
  LRoot: TNyxNode;
  LEditor: INyxControlRouteEditor;
  LSnapshot: INyxCanvasSnapshot;
  LScene: Integer;
  LRevision: Integer;
  LOwnContext: Boolean;
  LStale: Boolean;
begin
  AParent.Add(NewNyxHeading(PartID('event-title')).WithText('Control event routes'));

  if FRouteEditing = nil then
  begin
    AParent.Add(NewNyxLabel(PartID('event-unavailable')).WithText(
      'This canvas offers inspection without runtime route editing.'));
    Exit;
  end;

  if not FSelectedItem.Defined then
  begin
    LSnapshot := FRuntime.Scene.Snapshot;

    if LSnapshot.Count = 0 then
    begin
      AParent.Add(NewNyxLabel(PartID('event-unavailable')).WithText(
        'Add a canvas item and select its authored control in Control ports.'));
      Exit;
    end;
    FSelectedItem := LSnapshot.Items[0].Item;
  end;

  if FRouteEditor <> nil then
  begin
    FRouteDraft.Capture(FRouteEditor.ID, FRouteEditor.Node);
  end;
  LContext := nil;
  LOwnContext := False;
  try
    LScene := FRuntime.Scene.Revision;
    LRevision := FRouteEditing.RouteRevision;

    if (FRouteContext <> nil) and (FRouteItem.Name = FSelectedItem.Name) and
      (FRouteControl.ID = FSelectedControl.ID) then
    begin
      { Keep a stale proposal attached to its old inert context. It cannot be
        silently rebased onto replaced content; Apply checks the live revisions. }
      LContext := FRouteContext;
      LScene := FRouteScene;
      LRevision := FRouteRevision;
    end
    else
    begin
      LContext := FRouteEditing.RouteContext(FSelectedItem, LRoot);
      LOwnContext := True;

      if FSelectedControl.ID = '' then
      begin
        FSelectedControl := NyxControl(LRoot.ID);
      end;

      if LRoot.Find(FSelectedControl.ID) = nil then
      begin
        AParent.Add(NewNyxLabel(PartID('event-unavailable')).WithText(
          'Select an authored control in Control ports. Computed parts need their owning component editor.'));
        Exit;
      end;
    end;
    LStale := (LScene <> FRuntime.Scene.Revision) or
      (LRevision <> FRouteEditing.RouteRevision);
    AParent.Add(NewNyxLabel(PartID('event-selection')).WithText(
      FSelectedItem.Name + ' / ' + FSelectedControl.ID));

    if LStale then
    begin
      AParent.Add(NewNyxLabel(PartID('event-stale')).WithText(
        'The canvas changed. This proposal is retained; reload current routes before applying.'));
      AParent.Add(NewNyxSwitch(PartID('route-discard')).Configure
        .Text('Discard this unfinished proposal when reloading').Value(False).Done);
      AParent.Add(NewNyxButton(PartID('route-reload')).WithText('Reload current routes'));
    end;
    LEditor := NewNyxControlRouteEditor(PartID('event-routes'), LContext,
      FSelectedControl, LRevision);
    FRouteDraft.Restore(LEditor.ID, LEditor.Node);
    AParent.Add(LEditor);
    AParent.Add(NewNyxLabel(PartID('route-feedback')).WithText(FRouteFeedback));

    if LOwnContext then
    begin
      FRouteContext.Free;
      FRouteContext := LContext;
      LOwnContext := False;
    end;
    FRouteItem := FSelectedItem;
    FRouteControl := FSelectedControl;
    FRouteScene := LScene;
    FRouteRevision := LRevision;
    FRouteEditor := LEditor;
  finally

    if LOwnContext then
    begin
      LContext.Free;
    end;
  end;
end;

function TInspector.Handle(ANode: TNyxNode; const AEvent: TNyxEventInfo): Boolean;
var
  LRoot: TNyxNode;
begin
  LRoot := ANode;

  while (LRoot <> nil) and (LRoot.Parent <> nil) do
  begin
    LRoot := LRoot.Parent;
  end;
  Result := HandleInput(ANode, LRoot, AEvent) <> ntcuNone;
end;

function TInspector.HandleInput(ANode, ARoot: TNyxNode;
  const AEvent: TNyxEventInfo): TNyxToolCanvasUpdate;
var
  LLease: INyxToolCanvasInspector;
  LChange: TNyxControlRouteEditorChange;
  LFeedback: TNyxNode;
  LDiscard: TNyxNode;
  LHandled: Boolean;
begin
  RequireConnected;
  LLease := Self;
  Result := ntcuNone;

  if (ANode = nil) or (ARoot = nil) or (ARoot.Find(ANode.ID) <> ANode) then
  begin
    Exit;
  end;

  { The physical adapter owns a distinct rendered tree. Capture its complete
    unfinished fields before tab switches or other inspector publications. }

  if (FTab = titPrograms) and (FProgramEditor <> nil) then
  begin
    FProgramDraft.Capture(FProgramEditor.ID, ARoot);
    FProgramDraft.Restore(FProgramEditor.ID, FRoot.Node);
    Result := HandlePrograms(ANode, ARoot, AEvent);

    if Result <> ntcuNone then
    begin
      Exit;
    end;
  end;

  if (FTab = titLogic) and (FLogicEditor <> nil) and
    (AEvent.Trigger in [ntClick, ntChange]) then
  begin
    { Value changes inside Logic are captured by HandleLogic after its local
      action. Capture before external actions/tab changes, but never restore the
      complete toolbox for keyboard/focus notifications that changed no fields. }

    if (AEvent.Trigger = ntClick) or (FLogicEditor.Node.Find(ANode.ID) = nil) then
    begin
      FLogicDraft.Capture(FLogicEditor.ID, ARoot);
      FLogicDraft.Restore(FLogicEditor.ID, FRoot.Node, nlrmCurrentPresentation);
    end;
    Result := HandleLogic(ANode, ARoot, AEvent);

    if Result <> ntcuNone then
    begin
      Exit;
    end;
  end;

  if (ANode.ID = PartID('save-connections')) and (AEvent.Trigger = ntClick) then
  begin
    try
      SaveConnections;
    except
      on E: Exception do
      begin
        FAdoptionFeedback := E.Message;
        UpdateAdoptionFeedback;
      end;
    end;
    Exit(ntcuSynchronize);
  end;

  if (FTab = titEventRoutes) and (FRouteContext <> nil) and
    (FRouteEditor <> nil) and (AEvent.Trigger in [ntClick, ntChange]) then
  begin
    try

      if (ANode.ID = PartID('route-reload')) and (AEvent.Trigger = ntClick) then
      begin
        LDiscard := ARoot.Find(PartID('route-discard'));
        ReloadEventRoutes((LDiscard <> nil) and (LDiscard.Prop('value') = 'true'));
        Exit(ntcuRebuild);
      end;
      LHandled := HandleNyxControlRouteEditor(ANode, ARoot, FRouteContext, FRouteRevision);

      if not LHandled and CaptureNyxControlRouteEditor(ANode, ARoot,
        FRouteContext, FRouteRevision, LChange) then
      begin

        if (FRuntime.Scene.Revision <> FRouteScene) or
          (FRouteEditing.RouteRevision <> FRouteRevision) then
        begin
          raise ENyxCanvasScene.Create('This retained route proposal is stale; acknowledge reload before applying');
        end;
        FRouteEditing.ApplyRouteChanges([NyxCanvasRoutes(FRouteItem, LChange.Owner,
          NyxFlowRoutesFromData(LChange.RoutesData))], FRouteScene, FRouteRevision);
        FRouteDraft.Clear;
        FRouteEditor := nil;
        FreeAndNil(FRouteContext);
        FRouteFeedback := 'Event routes applied to this runtime item.';

        if not FConnected then
        begin
          Exit(ntcuNone);
        end;
        Rebuild;
        Exit(ntcuRebuild);
      end;

      if LHandled then
      begin
        FRouteDraft.Capture(PartID('event-routes'), ARoot);
        { Mirror the complete admitted local form into its separate blueprint.
          The target adapter then synchronizes its existing actual editor. }
        FRouteDraft.Restore(PartID('event-routes'), FRoot.Node);
        FRouteFeedback := '';
        LFeedback := ARoot.Find(PartID('route-feedback'));

        if LFeedback <> nil then
        begin
          LFeedback.Configure.Text('').Done;
        end;
        Exit(ntcuSynchronize);
      end;
    except
      on E: Exception do
      begin
        FRouteDraft.Capture(PartID('event-routes'), ARoot);
        FRouteDraft.Restore(PartID('event-routes'), FRoot.Node);
        FRouteFeedback := E.Message;
        LFeedback := ARoot.Find(PartID('route-feedback'));

        if LFeedback <> nil then
        begin
          LFeedback.Configure.Text(FRouteFeedback).Done;
        end;
        LFeedback := FRoot.Node.Find(PartID('route-feedback'));

        if LFeedback <> nil then
        begin
          LFeedback.Configure.Text(FRouteFeedback).Done;
        end;
        Exit(ntcuSynchronize);
      end;
    end;
  end;

  if HandleBase(ANode, AEvent) then
  begin
    Result := ntcuRebuild;
  end;
end;
