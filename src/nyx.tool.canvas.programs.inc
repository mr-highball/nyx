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
{ Shared interpreted Pascal workbench. Forms retain copied source/schema data;
  the existing prepared canvas owner alone publishes live executable meaning. }

function TInspector.GetProgramEditor: INyxCard;
begin
  RequireConnected;
  Result := FProgramEditor;
end;

function TInspector.GetHasPendingProgram: Boolean;
begin
  RequireConnected;
  Result := not FProgramDraft.SameProposal(FProgramPristine);
end;

procedure TInspector.ShowPrograms(AVisible: Boolean);
begin
  RequireConnected;

  if AVisible and (FProgramEditing = nil) then
  begin
    raise ENyxCanvasScene.Create('This canvas does not support runtime Pascal editing');
  end;
  FTab := titPorts;

  if AVisible then
  begin
    FTab := titPrograms;
  end;
  Rebuild;
end;

procedure TInspector.SelectProgram(const AReference: TNyxProgramRef; ADiscardDraft: Boolean);
var
  LPrograms: INyxProgramDeclarations;
begin
  RequireConnected;

  if FProgramEditing = nil then
  begin
    raise ENyxCanvasScene.Create('This canvas does not support runtime Pascal editing');
  end;

  if FProgramEditor <> nil then
  begin
    FProgramDraft.Capture(FProgramEditor.ID, FProgramEditor.Node);
  end;

  if GetHasPendingProgram and not ADiscardDraft then
  begin
    raise ENyxCanvasScene.Create(
      'Apply or restore the unfinished Pascal proposal, or acknowledge discarding it before navigation');
  end;
  LPrograms := FProgramEditing.RuntimePrograms;

  if AReference.Defined and not LPrograms.Contains(AReference) then
  begin
    raise ENyxCanvasScene.Create('Choose an existing runtime Pascal program');
  end;
  FProgramDraft.Clear;
  FProgramPristine.Clear;
  FProgramEditor := nil;
  FreeAndNil(FProgramContext);
  FProgramReference := AReference;
  FProgramCheckFeedback := '';
  FTab := titPrograms;
  Rebuild;
end;

procedure TInspector.ReloadProgram(ADiscardDraft: Boolean);
var
  LReference: TNyxProgramRef;
begin
  RequireConnected;
  LReference := FProgramReference;

  if (FProgramEditing <> nil) and LReference.Defined and
    not FProgramEditing.RuntimePrograms.Contains(LReference) then
  begin
    LReference := Default(TNyxProgramRef);
  end;
  SelectProgram(LReference, ADiscardDraft);
end;

procedure TInspector.BuildPrograms(const AParent: INyxCard);
var
  LContext: TNyxDocument;
  LPrograms: INyxProgramDeclarations;
  LFlows: INyxFlowDeclarations;
  LEditor: INyxCard;
  LPristine: TNyxProgramEditorDraft;
  LScene: Integer;
  LRevision: Integer;
  LIndex: Integer;
  LOwnContext: Boolean;
begin
  AParent.Configure.Padding(0).WidthSizing(nsFill).Done;
  AParent.Add(NewNyxHeading(PartID('program-title')).WithText('Write a Pascal behavior'));

  if FProgramEditing = nil then
  begin
    AParent.Add(NewNyxLabel(PartID('program-unavailable')).WithText(
      'This canvas offers inspection without runtime Pascal editing.'));
    Exit;
  end;
  AParent.Add(NewNyxLabel(PartID('program-workbench-help')).WithText(
    'Check typed source, apply it live, then reload Logic to use new programs. ' +
    'Applying replaces affected graph work while retaining embedded controls.'));

  if FAdoptionHost <> nil then
  begin
    AParent.Add(NewNyxLabel(PartID('program-lifetime')).WithText(
      'Save graphs and connections to retain applied programs in your project. Unapplied text stays local.'));
  end
  else
  begin
    AParent.Add(NewNyxLabel(PartID('program-lifetime')).WithText(
      'Applied programs are live only. Export RuntimePrograms to retain them before closing this canvas.'));
  end;
  LContext := FProgramContext;
  LOwnContext := LContext = nil;
  LScene := FProgramScene;
  LRevision := FProgramRevision;
  try

    if LOwnContext then
    begin
      LScene := FRuntime.Scene.Revision;
      LRevision := FProgramEditing.ProgramRevision;
      LPrograms := FProgramEditing.RuntimePrograms;
      LContext := FSource.Clone;
      while LContext.Programs.Count > 0 do
      begin
        LContext.Programs.Remove(LContext.Programs.Reference(0));
      end;
      for LIndex := 0 to LPrograms.Count - 1 do
      begin
        LContext.Programs.Define(LPrograms.Reference(LIndex),
          LPrograms.Definition(LPrograms.Reference(LIndex)));
      end;

      if FFlowEditing <> nil then
      begin
        LFlows := FFlowEditing.RuntimeFlows;
        while LContext.Flows.Count > 0 do
        begin
          LContext.Flows.Remove(LContext.Flows.Reference(0));
        end;
        for LIndex := 0 to LFlows.Count - 1 do
        begin
          LContext.Flows.Define(LFlows.Reference(LIndex), LFlows.Definition(LFlows.Reference(LIndex)));
        end;
      end;
    end;
    RequireConnected;

    if (LScene <> FRuntime.Scene.Revision) or (LRevision <> FProgramEditing.ProgramRevision) then
    begin
      AParent.Add(NewNyxLabel(PartID('program-stale')).WithText(
        'The canvas changed. Your text is retained; reload the current program before applying.'));
    end;
    AParent.Add(NewNyxSwitch(PartID('program-discard')).Configure
      .Text('Discard unfinished Pascal when reloading').Value(False).Done);
    AParent.Add(NewNyxButton(PartID('program-reload')).WithText('Reload current program'));
    AParent.Add(NewNyxLabel(PartID('program-publication-feedback')).Configure
      .Text(FProgramFeedback).Visible(FProgramFeedback <> '').Done);
    LEditor := NewNyxProgramEditor(PartID('program'), LContext, FProgramReference);
    LEditor.Configure.Padding(0).WidthSizing(nsFill).Done;
    LEditor.Node.Find(LEditor.ID + '-heading').Configure.Visible(False).Done;
    LEditor.Node.Find(LEditor.ID + '-apply').Configure.Text('Apply live program').Done;

    if FProgramCheckFeedback <> '' then
    begin
      LEditor.Node.Find(LEditor.ID + '-feedback').Configure.Text(FProgramCheckFeedback).Done;
    end;
    LPristine.Capture(LEditor.ID, LEditor.Node);
    FProgramDraft.Restore(LEditor.ID, LEditor.Node);
    AParent.Add(LEditor);

    if LOwnContext then
    begin
      FProgramContext := LContext;
      LOwnContext := False;
    end;
    FProgramScene := LScene;
    FProgramRevision := LRevision;
    FProgramPristine := LPristine;
    FProgramEditor := LEditor;
    FProgramDraft.Capture(LEditor.ID, LEditor.Node);
  finally

    if LOwnContext then
    begin
      LContext.Free;
    end;
  end;
end;

function TInspector.HandlePrograms(ANode, ARoot: TNyxNode;
  const AEvent: TNyxEventInfo): TNyxToolCanvasUpdate;
var
  LCapture: TNyxProgramEditorCapture;
  LHandled: Boolean;
  LDiscard: TNyxNode;
  LFeedback: TNyxNode;

  procedure Feedback(const AMessage: TNyxText);
  begin
    FProgramFeedback := AMessage;
    LFeedback := ARoot.Find(PartID('program-publication-feedback'));

    if LFeedback <> nil then
    begin
      LFeedback.Configure.Text(AMessage).Visible(AMessage <> '').Done;
    end;
    LFeedback := FRoot.Node.Find(PartID('program-publication-feedback'));

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

    if (ANode.ID = PartID('program-reload')) and (AEvent.Trigger = ntClick) then
    begin
      LDiscard := ARoot.Find(PartID('program-discard'));
      ReloadProgram((LDiscard <> nil) and (LDiscard.Prop('value') = 'true'));
      Exit(ntcuRebuild);
    end;

    if (ANode.ID = PartID('program-discard')) or (FProgramEditor.Node.Find(ANode.ID) = nil) then
    begin
      Exit;
    end;
    LHandled := HandleNyxProgramEditor(ANode, ARoot);

    if not LHandled and (AEvent.Trigger = ntClick) and
      CaptureNyxProgramEditor(ANode, ARoot, LCapture, FProgramContext) then
    begin

      if LCapture.Action = npeaOpen then
      begin
        SelectProgram(LCapture.Reference);
        Exit(ntcuRebuild);
      end;

      if (FRuntime.Scene.Revision <> FProgramScene) or
        (FProgramEditing.ProgramRevision <> FProgramRevision) then
      begin
        raise ENyxCanvasScene.Create(
          'This Pascal proposal is stale; reload its current context before applying');
      end;
      FProgramEditing.ApplyProgramChanges([LCapture.Change], FProgramScene, FProgramRevision);

      if not FConnected then
      begin
        Exit;
      end;
      FProgramReference := LCapture.Reference;

      if LCapture.Action = npeaRemove then
      begin
        FProgramReference := Default(TNyxProgramRef);
      end;
      FProgramEditor := nil;
      FProgramDraft.Clear;
      FProgramPristine.Clear;
      FreeAndNil(FProgramContext);
      FProgramFeedback := 'Pascal program applied. Reload Logic to discover its current signature.';
      FProgramCheckFeedback := '';
      Rebuild;
      Exit(ntcuRebuild);
    end;
    FProgramDraft.Capture(PartID('program'), ARoot);
    FProgramDraft.Restore(PartID('program'), FRoot.Node);
    { The adapter synchronizes from the independent blueprint. Local checking
      changes presentation outside the retained draft, so copy its diagnostic
      explicitly instead of overwriting it with the blueprint's older caption. }
    LFeedback := ARoot.Find(PartID('program-feedback'));

    if (LFeedback <> nil) and not LHandled and (AEvent.Trigger = ntChange) then
    begin
      LFeedback.Configure.Text('Source or settings changed. Check this proposal again.').Done;
    end;

    if LFeedback <> nil then
    begin
      FProgramCheckFeedback := LFeedback.Prop('text');
      FRoot.Node.Find(PartID('program-feedback')).Configure.Text(FProgramCheckFeedback).Done;
    end;
    Feedback('');
    Result := ntcuSynchronize;
  except
    on E: Exception do
    begin

      if FConnected then
      begin
        FProgramDraft.Capture(PartID('program'), ARoot);
        FProgramDraft.Restore(PartID('program'), FRoot.Node);
        Feedback(E.Message);
        Result := ntcuSynchronize;
      end;
    end;
  end;
end;
