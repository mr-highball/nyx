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

constructor TLogicGraphHost.Create(AOwner: TInspector;
  const AEditing: INyxCanvasFlowEditing);
begin
  inherited Create;
  Owner := AOwner;
  FEditing := AEditing;
end;

function TLogicGraphHost.Acquire: INyxToolCanvasInspector;
begin
  FEditing.GetFlowRevision;

  if Owner = nil then
  begin
    raise ENyxFlow.Create('The shared Logic owner is retired');
  end;
  Result := Owner;
  Owner.RequireConnected;
end;

function TLogicGraphHost.OpenGraph(const AFlow: TNyxFlowRef;
  ADiscardDraft: Boolean): TNyxToolGraphFrame;
var
  LLease: INyxToolCanvasInspector;
begin
  LLease := Acquire;

  if (Owner.FLogicContext <> nil) and (Owner.FLogicFlow.Name <> AFlow.Name) then
  begin
    Owner.SelectLogicProposal(AFlow, ADiscardDraft, False);
  end;
  Owner.EnsureLogicContext;

  if Owner.FLogicFlow.Name <> AFlow.Name then
  begin
    Owner.SelectLogicProposal(AFlow, ADiscardDraft, False);
    Owner.EnsureLogicContext;
  end;
  Result := Owner.CaptureLogicFrame(AFlow);
end;

function TLogicGraphHost.ReadGraph(const AFlow: TNyxFlowRef): TNyxToolGraphFrame;
var
  LLease: INyxToolCanvasInspector;
begin
  LLease := Acquire;
  Result := Owner.CaptureLogicFrame(AFlow);
end;

function TLogicGraphHost.GraphVersion(const AFlow: TNyxFlowRef): Integer;
var
  LLease: INyxToolCanvasInspector;
begin
  LLease := Acquire;
  Result := -1;

  if (Owner.FLogicContext <> nil) and (Owner.FLogicFlow.Name = AFlow.Name) and
    (Owner.FRuntime.Scene.Revision = Owner.FLogicScene) and
    (Owner.FFlowEditing.FlowRevision = Owner.FLogicRevision) then
  begin
    Result := Owner.FLogicVersion;
  end;
end;

function TLogicGraphHost.StageGraph(const AExpected: TNyxToolGraphFrame;
  const AChanges: array of TNyxFlowChange): TNyxToolGraphFrame;
var
  LLease: INyxToolCanvasInspector;
  LIndex: Integer;
begin
  LLease := Acquire;
  Owner.CheckLogicFrame(AExpected);
  for LIndex := 0 to High(AChanges) do
  begin

    if (AChanges[LIndex].Reference.Name <> AExpected.Flow.Name) or
      not (AChanges[LIndex].Kind in [nfcConnect, nfcDisconnect]) then
    begin
      raise ENyxFlow.Create('Canvas wiring accepts only connections in its exact open graph');
    end;
  end;
  Owner.StageLogicChanges(AChanges);
  Result := Owner.CaptureLogicFrame(AExpected.Flow);
  Owner.Rebuild;
end;

function TLogicGraphHost.ApplyGraph(const AExpected: TNyxToolGraphFrame): TNyxToolGraphFrame;
var
  LLease: INyxToolCanvasInspector;
begin
  LLease := Acquire;
  Owner.CheckLogicFrame(AExpected);
  Owner.ApplyLogic;
  Result := Owner.CaptureLogicFrame(AExpected.Flow);
end;

function TLogicGraphHost.ReloadGraph(const AFlow: TNyxFlowRef;
  ADiscardDraft: Boolean): TNyxToolGraphFrame;
var
  LLease: INyxToolCanvasInspector;
begin
  LLease := Acquire;
  Owner.SelectLogicProposal(AFlow, ADiscardDraft, False);
  Owner.EnsureLogicContext;
  Result := Owner.CaptureLogicFrame(AFlow);
end;

function TInspector.NewLogicContext(out AFlows: INyxFlowDeclarations): TNyxDocument;
var
  LContext: TNyxDocument;
  LPrograms: INyxProgramDeclarations;
  LIndex: Integer;
begin
  AFlows := FFlowEditing.RuntimeFlows;
  { The shared Logic editor snapshots only programs and flows. Keep unrelated
    pages, resources, collections and scene content out of each staged clone.
    Whole-application validation still belongs to prepared Apply, not this form. }
  LContext := TNyxDocument.Create;
  try

    if FProgramEditing <> nil then
    begin
      LPrograms := FProgramEditing.RuntimePrograms;
    end
    else
    begin
      LPrograms := FSource.Programs;
    end;
    for LIndex := 0 to LPrograms.Count - 1 do
    begin
      LContext.Programs.Define(LPrograms.Reference(LIndex),
        LPrograms.Definition(LPrograms.Reference(LIndex)));
    end;
    for LIndex := 0 to AFlows.Count - 1 do
    begin
      LContext.Flows.Define(AFlows.Reference(LIndex),
        AFlows.Definition(AFlows.Reference(LIndex)));
    end;
    Result := LContext;
    LContext := nil;
  finally
    LContext.Free;
  end;
end;

procedure TInspector.EnsureLogicContext;
var
  LFlows: INyxFlowDeclarations;
  LContext: TNyxDocument;
  LScene: Integer;
  LRevision: Integer;
begin
  RequireConnected;
  FFlowEditing.GetFlowRevision;

  if FLogicContext <> nil then
  begin
    Exit;
  end;

  if FLogicVersion = High(Integer) then
  begin
    raise ENyxFlow.Create('Logic proposal version capacity exhausted');
  end;
  LScene := FRuntime.Scene.Revision;
  LRevision := FFlowEditing.FlowRevision;
  LContext := NewLogicContext(LFlows);
  try

    if (LScene <> FRuntime.Scene.Revision) or
      (LRevision <> FFlowEditing.FlowRevision) then
    begin
      raise ENyxFlow.Create('Runtime changed while opening the Logic proposal');
    end;
    FLogicContext := LContext;
    LContext := nil;
    FLogicBaseFlows := LFlows;
    FLogicScene := LScene;
    FLogicRevision := LRevision;

    if not FLogicFlow.Defined and (LFlows.Count > 0) then
    begin
      FLogicFlow := LFlows.Reference(0);
    end;
    Inc(FLogicVersion);
  finally
    LContext.Free;
  end;
end;

function TInspector.CaptureLogicFrame(const AFlow: TNyxFlowRef): TNyxToolGraphFrame;
begin
  RequireConnected;
  FFlowEditing.GetFlowRevision;

  if (FLogicContext = nil) or (FLogicFlow.Name <> AFlow.Name) or
    not FLogicContext.Flows.Contains(AFlow) then
  begin
    raise ENyxFlow.Create('Read only the existing open Logic proposal');
  end;

  if FLogicEditor <> nil then
  begin
    FLogicDraft.Capture(FLogicEditor.ID, FLogicEditor.Node);
  end;
  Result := Default(TNyxToolGraphFrame);
  Result.Flow := AFlow;
  Result.Definition := FLogicContext.Flows.Definition(AFlow).ToData;
  Result.SceneRevision := FLogicScene;
  Result.FlowRevision := FLogicRevision;
  Result.ProposalVersion := FLogicVersion;
  Result.Pending := GetHasPendingLogic;
  Result.FieldsPending := not FLogicDraft.SameProposal(FLogicPristine);
  Result.Stale := (FRuntime.Scene.Revision <> FLogicScene) or
    (FFlowEditing.FlowRevision <> FLogicRevision);
end;

procedure TInspector.CheckLogicFrame(const AExpected: TNyxToolGraphFrame);
var
  LCurrent: TNyxToolGraphFrame;
begin
  LCurrent := CaptureLogicFrame(AExpected.Flow);

  if LCurrent.Stale or (LCurrent.SceneRevision <> AExpected.SceneRevision) or
    (LCurrent.FlowRevision <> AExpected.FlowRevision) or
    (LCurrent.ProposalVersion <> AExpected.ProposalVersion) or
    (LCurrent.Definition.ToJSON <> AExpected.Definition.ToJSON) then
  begin
    raise ENyxFlow.Create('The shared Logic proposal changed; reopen current node connections');
  end;

  if LCurrent.FieldsPending then
  begin
    raise ENyxFlow.Create('Stage or explicitly discard unfinished Logic fields before editing node connections');
  end;
end;

procedure TInspector.StageLogicChanges(const AChanges: array of TNyxFlowChange);
var
  LFlows: INyxFlowDeclarations;
  LContext: TNyxDocument;
  LIndex: Integer;
begin

  if FLogicVersion = High(Integer) then
  begin
    raise ENyxFlow.Create('Logic proposal version capacity exhausted');
  end;
  LFlows := EditNyxFlows(FLogicContext.Flows, AChanges);
  { Assemble the complete independent proposal before replacing its owner.
    Required application inputs may remain incomplete until ApplyLogic. }
  LContext := FLogicContext.Clone;
  try
    while LContext.Flows.Count > 0 do
    begin
      LContext.Flows.Remove(LContext.Flows.Reference(0));
    end;
    for LIndex := 0 to LFlows.Count - 1 do
    begin
      LContext.Flows.Define(LFlows.Reference(LIndex),
        LFlows.Definition(LFlows.Reference(LIndex)));
    end;
    FLogicContext.Free;
    FLogicContext := LContext;
    LContext := nil;
    Inc(FLogicVersion);
    FLogicEditor := nil;
    FLogicDraft.Clear;
    FLogicPristine.Clear;
    FLogicFeedback := 'Graph proposal updated. Apply live graph when its steps and connections are ready.';
  finally
    LContext.Free;
  end;
end;

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

  if FLogicVersion = High(Integer) then
  begin
    raise ENyxFlow.Create('Logic proposal version capacity exhausted');
  end;
  FFlowEditing.ApplyFlowChanges(LChanges, FLogicScene, FLogicRevision);

  if not FConnected then
  begin
    Exit;
  end;
  FLogicBaseFlows := FFlowEditing.RuntimeFlows;
  FLogicRevision := FFlowEditing.FlowRevision;
  { Applying through the form retires any separately retained node frame, even
    when the staged topology itself did not change during that publication. }
  Inc(FLogicVersion);
  FLogicFeedback := 'Logic applied to this live canvas. Export its graphs to retain the changes.';

  if FAdoptionHost <> nil then
  begin
    FLogicFeedback := 'Logic applied. Save graphs and connections to retain the changes in your project.';
  end;
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
begin
  SelectLogicProposal(AReference, ADiscardDraft, True);
end;

procedure TInspector.SelectLogicProposal(const AReference: TNyxFlowRef;
  ADiscardDraft, AActivate: Boolean);
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

  if FLogicVersion = High(Integer) then
  begin
    raise ENyxFlow.Create('Logic proposal version capacity exhausted');
  end;
  Inc(FLogicVersion);
  FLogicDraft.Clear;
  FLogicPristine.Clear;
  FLogicEditor := nil;
  FLogicBaseFlows := nil;
  FreeAndNil(FLogicContext);
  FLogicFlow := AReference;
  FLogicFeedback := '';

  if AActivate then
  begin
    FTab := titLogic;
  end;
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

  if FAdoptionHost <> nil then
  begin
    AParent.Add(NewNyxLabel(PartID('logic-workbench-lifetime')).WithText(
      'Apply, then save graphs and connections to retain changes in your project. ' +
      'Unapplied proposals stay local.'));
  end
  else
  begin
    AParent.Add(NewNyxLabel(PartID('logic-workbench-lifetime')).WithText(
      'Changes are live only. Closing this canvas discards unsaved graphs and proposals.'));
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

      if FLogicVersion = High(Integer) then
      begin
        raise ENyxFlow.Create('Logic proposal version capacity exhausted');
      end;
      LContext := NewLogicContext(LFlows);

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
    LActions.Add(NewNyxButton(PartID('logic-show-graph')).Configure.Text('Show graph on canvas')
      .Enabled(LReference.Defined).Done);
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
    { The factory's default discovery presentation is already valid. Restore
      changed filters, but do not rebuild its whole toolbox for default fields. }
    FLogicDraft.Restore(LEditor.ID, LEditor.Node, nlrmCurrentPresentation);
    AParent.Add(LEditor);

    if LOwnContext then
    begin
      FLogicContext := LContext;
      FLogicBaseFlows := LFlows;
      Inc(FLogicVersion);
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
  LMirror: TNyxNode;

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

    if (ANode.ID = PartID('logic-show-graph')) and (AEvent.Trigger = ntClick) then
    begin
      ShowGraphConnections(FLogicFlow);
      Exit(ntcuRebuild);
    end;

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
      StageLogicChanges([LCapture.Change]);
      FLogicFlow := LCapture.Reference;

      if LCapture.Change.Kind = nfcRemove then
      begin
        FLogicFlow := Default(TNyxFlowRef);
      end;
      Rebuild;
      Exit(ntcuRebuild);
    end;
    { Includes ordinary field edits that need no special routing. Preserve the
      full nested draft in the independently owned blueprint before Sync. }
    FLogicDraft.Capture(PartID('logic'), ARoot);
    LMirror := nil;

    if not LHandled and (AEvent.Trigger = ntChange) then
    begin
      LMirror := FRoot.Node.Find(ANode.ID);
    end;

    if LMirror <> nil then
    begin
      { The shared handler reported no structural/configuration action. Only
        this value changed; restoring every inactive nested editor would repeat
        their parsing/validation on each keystroke. The complete draft above
        still owns all fields, and the actual renderer retains its caret. }
      LMirror.Configure.Value(ANode.Prop(NyxAttributeName(atValue))).Done;
    end
    else
    begin
      FLogicDraft.Restore(PartID('logic'), FRoot.Node, nlrmCurrentPresentation);
    end;
    Feedback('');
    Result := ntcuSynchronize;
  except
    on E: Exception do
    begin

      if FConnected then
      begin
        FLogicDraft.Capture(PartID('logic'), ARoot);
        FLogicDraft.Restore(PartID('logic'), FRoot.Node, nlrmCurrentPresentation);
        Feedback(E.Message);
        Result := ntcuSynchronize;
      end;
    end;
  end;
end;
