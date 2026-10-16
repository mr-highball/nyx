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
{ Shared portable Logic workbench. Both physical adapters render these public
  Nyx controls and consume the existing synchronization/rebuild result. }

function TInspector.GetLogicEditor: INyxCard;
begin
  RequireConnected;
  Result := FLogicEditor;
end;

function TInspector.GetHasPendingLogic: Boolean;
begin
  RequireConnected;
  Result := (FLogicContext <> nil) and (FLogicBaseFlows <> nil) and
    (FLogicContext.Flows.ToData.ToJSON <> FLogicBaseFlows.ToData.ToJSON);
end;

procedure TInspector.ApplyLogic;
var
  LLease: INyxToolCanvasInspector;
  LChanges: array of TNyxFlowChange;
  LReference: TNyxFlowRef;
  LIndex: Integer;
  LPosition: Integer;

  procedure Append(const AChange: TNyxFlowChange);
  begin
    SetLength(LChanges, Length(LChanges) + 1);
    LChanges[High(LChanges)] := AChange.Expect(
      NyxFlowBaseline(FLogicBaseFlows, AChange.Reference));
  end;

begin
  RequireConnected;
  LLease := Self;

  if not GetHasPendingLogic then
  begin
    Exit;
  end;

  if (FRuntime.Scene.Revision <> FLogicScene) or
    (FFlowEditing.FlowRevision <> FLogicRevision) then
  begin
    raise ENyxCanvasScene.Create(
      'This retained Logic proposal is stale; reload current Logic before applying');
  end;
  { Prefer one change per affected graph. Editing one step must not consume the
    existing 64-change group budget merely because other graphs are present. }
  LChanges := nil;
  for LIndex := 0 to FLogicBaseFlows.Count - 1 do
  begin
    LReference := FLogicBaseFlows.Reference(LIndex);

    if not FLogicContext.Flows.Contains(LReference) then
    begin
      Append(NyxRemoveFlow(LReference));
    end;
  end;
  for LIndex := 0 to FLogicContext.Flows.Count - 1 do
  begin
    LReference := FLogicContext.Flows.Reference(LIndex);

    if NyxFlowBaseline(FLogicBaseFlows, LReference).ToJSON <>
      NyxFlowBaseline(FLogicContext.Flows, LReference).ToJSON then
    begin
      Append(NyxDefineFlow(LReference, FLogicContext.Flows.Definition(LReference)));
    end;
  end;
  { Remove/recreate may change registry order without changing a definition.
    Rebuild exact membership in that uncommon case; do not silently sort it.
    Every expectation still addresses the original registry. Admission retains
    its existing 64-change limit and refuses larger proposals without mutation. }

  if (Length(LChanges) = 0) or
    (EditNyxFlows(FLogicBaseFlows, LChanges).ToData.ToJSON <>
      FLogicContext.Flows.ToData.ToJSON) then
  begin
    SetLength(LChanges, FLogicBaseFlows.Count + FLogicContext.Flows.Count);
    LPosition := 0;
    for LIndex := 0 to FLogicBaseFlows.Count - 1 do
    begin
      LReference := FLogicBaseFlows.Reference(LIndex);
      LChanges[LPosition] := NyxRemoveFlow(LReference)
        .Expect(NyxFlowBaseline(FLogicBaseFlows, LReference));
      Inc(LPosition);
    end;
    for LIndex := 0 to FLogicContext.Flows.Count - 1 do
    begin
      LReference := FLogicContext.Flows.Reference(LIndex);
      LChanges[LPosition] := NyxDefineFlow(LReference,
        FLogicContext.Flows.Definition(LReference))
        .Expect(NyxFlowBaseline(FLogicBaseFlows, LReference));
      Inc(LPosition);
    end;
  end;
  FFlowEditing.ApplyFlowChanges(LChanges, FLogicScene, FLogicRevision);

  if not FConnected then
  begin
    Exit;
  end;
  FLogicBaseFlows := FFlowEditing.RuntimeFlows;
  FLogicRevision := FFlowEditing.FlowRevision;
  FLogicFeedback := 'Logic applied to this live canvas. Graph changes are not saved to the project.';
  Rebuild;
end;

procedure TInspector.ShowLogic(AVisible: Boolean);
begin
  RequireConnected;

  if AVisible and (FFlowEditing = nil) then
  begin
    raise ENyxCanvasScene.Create('This canvas does not support runtime Logic editing');
  end;
  FTab := titPorts;

  if AVisible then
  begin
    FTab := titLogic;
  end;
  Rebuild;
end;

procedure TInspector.SelectLogicFlow(const AReference: TNyxFlowRef;
  ADiscardDraft: Boolean);
var
  LFlows: INyxFlowDeclarations;
begin
  RequireConnected;

  if FFlowEditing = nil then
  begin
    raise ENyxCanvasScene.Create('This canvas does not support runtime Logic editing');
  end;

  if FLogicEditor <> nil then
  begin
    FLogicDraft.Capture(FLogicEditor.ID, FLogicEditor.Node);
  end;

  if (GetHasPendingLogic or not FLogicDraft.SameProposal(FLogicPristine)) and not ADiscardDraft then
  begin
    raise ENyxCanvasScene.Create(
      'Apply the unfinished Logic proposal or acknowledge discarding it before opening another graph');
  end;
  LFlows := FFlowEditing.RuntimeFlows;

  if AReference.Defined and not LFlows.Contains(AReference) then
  begin
    raise ENyxCanvasScene.Create('Choose an existing runtime graph');
  end;
  FLogicDraft.Clear;
  FLogicPristine.Clear;
  FLogicEditor := nil;
  FLogicBaseFlows := nil;
  FreeAndNil(FLogicContext);
  FLogicFlow := AReference;
  FLogicFeedback := '';
  FTab := titLogic;
  Rebuild;
end;

procedure TInspector.ReloadLogic(ADiscardDraft: Boolean);
var
  LReference: TNyxFlowRef;
begin
  RequireConnected;
  LReference := FLogicFlow;

  if (FFlowEditing <> nil) and LReference.Defined and
    not FFlowEditing.RuntimeFlows.Contains(LReference) then
  begin
    LReference := Default(TNyxFlowRef);
  end;
  SelectLogicFlow(LReference, ADiscardDraft);
end;

procedure TInspector.BuildLogic(const AParent: INyxCard);
var
  LContext: TNyxDocument;
  LFlows: INyxFlowDeclarations;
  LEditor: INyxCard;
  LActions: INyxRow;
  LReference: TNyxFlowRef;
  LPristine: TNyxLogicEditorDraft;
  LScene: Integer;
  LRevision: Integer;
  LIndex: Integer;
  LOwnContext: Boolean;
begin
  AParent.Configure.Padding(0).WidthSizing(nsFill).Done;
  AParent.Add(NewNyxHeading(PartID('logic-workbench-title')).WithText('Build a tool with Logic'));

  if FFlowEditing = nil then
  begin
    AParent.Add(NewNyxLabel(PartID('logic-unavailable')).WithText(
      'This canvas offers inspection without runtime Logic editing.'));
    Exit;
  end;
  AParent.Add(NewNyxLabel(PartID('logic-workbench-help')).WithText(
    'Stage operations and connections, then apply them together. ' +
    'Saved Pascal programs appear in the Logic collection.'));
  AParent.Add(NewNyxLabel(PartID('logic-workbench-lifetime')).WithText(
    'Changes are live only. Closing this canvas discards unsaved graphs and proposals.'));

  if FAdoptionHost <> nil then
  begin
    AParent.Add(NewNyxLabel(PartID('logic-workbench-saving')).WithText(
      'Save connections saves control routes only. Export graph changes separately.'));
  end;
  LContext := FLogicContext;
  LOwnContext := LContext = nil;
  LReference := FLogicFlow;
  LScene := FLogicScene;
  LRevision := FLogicRevision;
  try

    if LOwnContext then
    begin
      LScene := FRuntime.Scene.Revision;
      LRevision := FFlowEditing.FlowRevision;
      LFlows := FFlowEditing.RuntimeFlows;
      LContext := FSource.Clone;
      while LContext.Flows.Count > 0 do
      begin
        LContext.Flows.Remove(LContext.Flows.Reference(0));
      end;
      for LIndex := 0 to LFlows.Count - 1 do
      begin
        LContext.Flows.Define(LFlows.Reference(LIndex),
          LFlows.Definition(LFlows.Reference(LIndex)));
      end;

      if not LReference.Defined and (LFlows.Count > 0) then
      begin
        LReference := LFlows.Reference(0);
      end;
    end;

    if (LScene <> FRuntime.Scene.Revision) or
      (LRevision <> FFlowEditing.FlowRevision) then
    begin
      AParent.Add(NewNyxLabel(PartID('logic-stale')).WithText(
        'The canvas changed. Your proposal is retained; reload current Logic before applying.'));
    end;
    AParent.Add(NewNyxSwitch(PartID('logic-discard')).Configure
      .Text('Discard unfinished Logic when opening or reloading a graph').Value(False).Done);
    LActions := NewNyxRow(PartID('logic-workbench-actions'));
    LActions.Configure.Wrap(nfwWrap).Gap(8).Done;
    LActions.Add(NewNyxButton(PartID('logic-reload')).WithText('Reload current Logic'));
    LActions.Add(NewNyxButton(PartID('logic-apply')).Configure.Text('Apply live graph')
      .Enabled(GetHasPendingLogic).Done);
    AParent.Add(LActions);

    if GetHasPendingLogic then
    begin
      AParent.Add(NewNyxBadge(PartID('logic-workbench-state')).WithText('Graph proposal pending'));
    end
    else
    begin
      AParent.Add(NewNyxBadge(PartID('logic-workbench-state')).WithText('No unapplied graph changes'));
    end;
    AParent.Add(NewNyxLabel(PartID('logic-feedback')).Configure.Text(FLogicFeedback)
      .Visible(FLogicFeedback <> '').Done);
    LEditor := NewNyxLogicEditor(PartID('logic'), LContext, LReference);
    LEditor.Configure.Padding(0).WidthSizing(nsFill).Done;
    LEditor.Node.Find(LEditor.ID + '-title').Configure.Visible(False).Done;
    LEditor.Node.Find(LEditor.ID + '-help').Configure.Visible(False).Done;
    { The shared form's source/history host applies individual commands. Here
      those same commands stage toward one runtime publication; say so beside
      the user's actions without changing their typed capture boundary. }
    LEditor.Node.Find(LEditor.ID + '-add').Configure.Text('Add step to proposal').Done;
    LEditor.Node.Find(LEditor.ID + '-set').Configure.Text('Stage configuration').Done;
    LEditor.Node.Find(LEditor.ID + '-connect').Configure.Text('Stage connection').Done;
    LEditor.Node.Find(LEditor.ID + '-disconnect').Configure.Text('Stage disconnection').Done;
    LEditor.Node.Find(LEditor.ID + '-remove-step').Configure.Text('Stage step removal').Done;
    LEditor.Node.Find(LEditor.ID + '-remove-graph').Configure.Text('Stage graph removal').Done;
    LPristine.Capture(LEditor.ID, LEditor.Node);
    FLogicDraft.Restore(LEditor.ID, LEditor.Node);
    AParent.Add(LEditor);

    if LOwnContext then
    begin
      FLogicContext := LContext;
      FLogicBaseFlows := LFlows;
      LOwnContext := False;
    end;
    FLogicScene := LScene;
    FLogicRevision := LRevision;
    FLogicFlow := LReference;
    FLogicPristine := LPristine;
    FLogicEditor := LEditor;
  finally

    if LOwnContext then
    begin
      LContext.Free;
    end;
  end;
end;

function TInspector.HandleLogic(ANode, ARoot: TNyxNode;
  const AEvent: TNyxEventInfo): TNyxToolCanvasUpdate;
var
  LCapture: TNyxLogicEditorCapture;
  LHandled: Boolean;
  LDiscard: TNyxNode;
  LFeedback: TNyxNode;
  LFlows: INyxFlowDeclarations;
  LIndex: Integer;

  function Confirmed: Boolean;
  begin
    LDiscard := ARoot.Find(PartID('logic-discard'));
    Result := (LDiscard <> nil) and
      (LDiscard.Prop(NyxAttributeName(atValue)) = 'true');
  end;

  procedure Feedback(const AMessage: TNyxText);
  begin
    FLogicFeedback := AMessage;
    LFeedback := ARoot.Find(PartID('logic-feedback'));

    if LFeedback <> nil then
    begin
      LFeedback.Configure.Text(AMessage).Visible(AMessage <> '').Done;
    end;
    LFeedback := FRoot.Node.Find(PartID('logic-feedback'));

    if LFeedback <> nil then
    begin
      LFeedback.Configure.Text(AMessage).Visible(AMessage <> '').Done;
    end;
  end;

begin
  Result := ntcuNone;

  if not (AEvent.Trigger in [ntClick, ntChange]) then
  begin
    Exit;
  end;
  try

    if (ANode.ID = PartID('logic-reload')) and (AEvent.Trigger = ntClick) then
    begin
      ReloadLogic(Confirmed);
      Exit(ntcuRebuild);
    end;

    if (ANode.ID = PartID('logic-apply')) and (AEvent.Trigger = ntClick) then
    begin
      ApplyLogic;
      Exit(ntcuRebuild);
    end;

    if (ANode.ID = PartID('logic-discard')) or
      (FLogicEditor.Node.Find(ANode.ID) = nil) then
    begin
      Exit;
    end;
    LHandled := HandleNyxLogicEditor(ANode, ARoot);

    if not LHandled and (AEvent.Trigger = ntClick) and
      CaptureNyxLogicEditor(ANode, ARoot, LCapture) then
    begin

      if LCapture.Open then
      begin
        SelectLogicFlow(LCapture.Reference, Confirmed);
        Exit(ntcuRebuild);
      end;

      if (FRuntime.Scene.Revision <> FLogicScene) or
        (FFlowEditing.FlowRevision <> FLogicRevision) then
      begin
        raise ENyxCanvasScene.Create(
          'This retained Logic proposal is stale; reload current Logic before applying');
      end;
      { Local graph admission checks types, cycles and exact step identities.
        Whole application routes are checked only at ApplyLogic, allowing the
        user to add a step before connecting its required input. }
      LFlows := EditNyxFlows(FLogicContext.Flows, [LCapture.Change]);
      while FLogicContext.Flows.Count > 0 do
      begin
        FLogicContext.Flows.Remove(FLogicContext.Flows.Reference(0));
      end;
      for LIndex := 0 to LFlows.Count - 1 do
      begin
        FLogicContext.Flows.Define(LFlows.Reference(LIndex),
          LFlows.Definition(LFlows.Reference(LIndex)));
      end;
      FLogicFlow := LCapture.Reference;

      if LCapture.Change.Kind = nfcRemove then
      begin
        FLogicFlow := Default(TNyxFlowRef);
      end;
      FLogicEditor := nil;
      FLogicDraft.Clear;
      FLogicPristine.Clear;
      FLogicFeedback := 'Graph proposal updated. Apply live graph when its steps and connections are ready.';
      Rebuild;
      Exit(ntcuRebuild);
    end;
    { Includes ordinary field edits that need no special routing. Preserve the
      full nested draft in the independently owned blueprint before Sync. }
    FLogicDraft.Capture(PartID('logic'), ARoot);
    FLogicDraft.Restore(PartID('logic'), FRoot.Node);
    Feedback('');
    Result := ntcuSynchronize;
  except
    on E: Exception do
    begin

      if FConnected then
      begin
        FLogicDraft.Capture(PartID('logic'), ARoot);
        FLogicDraft.Restore(PartID('logic'), FRoot.Node);
        Feedback(E.Message);
        Result := ntcuSynchronize;
      end;
    end;
  end;
end;
