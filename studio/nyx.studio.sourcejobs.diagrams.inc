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
{ Diagram saving shares the existing source FIFO, compiler and complete paired
  candidate guards. The only retained UI reply stays on the schema lease, never
  in a worker or wire record. }

procedure TDiagramSaveHost.Submit(const ARequest: INyxDiagramSaveRequest);
var
  LLease: INyxDiagramSaveHost;
  LRequest: INyxDiagramSaveRequest;
  LPort: IDiagramSaveSubmission;
begin
  Scheduler.RequireUI;
  LLease := Self;
  LRequest := ARequest;
  LPort := Port;

  if LPort = nil then
  begin
    raise ENyxModel.Create('The diagram save host has retired');
  end;
  LPort.SubmitDiagram(Context, LRequest);
end;

function TDiagramSaveHost.Current(const AReference: TNyxDiagramRef): TNyxDiagramSaveTarget;
begin
  Scheduler.RequireUI;

  if Port = nil then
  begin
    raise ENyxModel.Create('The diagram inspection host has retired');
  end;
  Result := Port.InspectDiagram(Context, AReference);
end;

function TSourcePort.InspectDiagram(const AContext: TNyxStudioCommandContext;
  const AReference: TNyxDiagramRef): TNyxDiagramSaveTarget;
var
  LOwner: TNyxSourceCommands;
begin
  LOwner := Owner;

  if LOwner = nil then
  begin
    raise ENyxModel.Create('The owning diagram project has retired');
  end;
  LOwner.FScheduler.RequireUI;

  if LOwner.FDetached or not LOwner.FSession.MatchesCommandContext(AContext) then
  begin
    raise ENyxModel.Create('The diagram belongs to an earlier project load');
  end;
  Result := NyxDiagramSaveTarget(LOwner.FSession.Document, AReference);
end;

procedure TSourcePort.SubmitDiagram(const AContext: TNyxStudioCommandContext;
  const ARequest: INyxDiagramSaveRequest);
var
  LOwner: TNyxSourceCommands;
begin
  LOwner := Owner;

  if LOwner = nil then
  begin
    raise ENyxModel.Create('The owning project has retired its diagram save host');
  end;
  LOwner.SaveDiagram(AContext, ARequest);
end;

function TNyxSourceCommands.DiagramSaveHost(
  const AContext: TNyxStudioCommandContext): INyxDiagramSaveHost;
var
  LPort: IDiagramSaveSubmission;
  LHost: TDiagramSaveHost;
begin
  FScheduler.RequireUI;

  if FDetached or not FSession.MatchesCommandContext(AContext) or
    not Supports(FPort, IDiagramSaveSubmission, LPort) then
  begin
    raise ENyxModel.Create('A diagram save host requires its current owning project load');
  end;
  LHost := TDiagramSaveHost.Create;
  Result := LHost;
  LHost.Port := LPort;
  LHost.Context := AContext;
  LHost.Scheduler := FScheduler;
end;

procedure TNyxSourceCommands.SaveDiagram(const AContext: TNyxStudioCommandContext;
  const ARequest: INyxDiagramSaveRequest);
var
  LRequest: INyxDiagramSaveRequest;
  LSnapshot: TNyxDiagramSaveSnapshot;
  LEdit: TNyxStudioDesignEdit;
  LChange: TNyxDiagramChange;
begin
  FScheduler.RequireUI;
  LRequest := ARequest;

  if FDetached or not FSession.MatchesCommandContext(AContext) then
  begin
    raise ENyxModel.Create('The diagram belongs to an earlier project load');
  end;

  if (LRequest = nil) or not LRequest.Pending then
  begin
    raise ENyxModel.Create('Submit a current diagram save request');
  end;

  if Busy then
  begin
    raise ENyxModel.Create('Wait for the current source command before saving the diagram');
  end;
  LSnapshot := LRequest.Snapshot;

  if not LSnapshot.Defined then
  begin
    raise ENyxModel.Create('An absent diagram proposal cannot be saved');
  end;
  { Normalize copied intent at this creator boundary. Expect protects the
    editor's original definition; normal dispatch protects complete source/drafts. }
  LChange := TNyxDiagramChange.FromData(LSnapshot.Change.ToData);
  { Pending/Snapshot are foreign UI calls. A getter may load another project or
    enqueue work even when all accepted bytes happen to stay equal. Recheck the
    captured load and queue after those calls, before capturing any new intent. }

  if FDetached or not FSession.MatchesCommandContext(AContext) then
  begin
    raise ENyxModel.Create('The diagram save belongs to a project load retired during capture');
  end;

  if Busy then
  begin
    raise ENyxModel.Create('The source queue changed while capturing this diagram save');
  end;
  LEdit := Default(TNyxStudioDesignEdit);
  LEdit.Action := sdaTransaction;
  LEdit.Selection := FSession.SelectedID;
  LEdit.View := FSession.ActiveViewID;
  LEdit.Transaction := CaptureNyxProjectTransaction(NyxProjectTransaction([
    NyxDesignStep(NyxDiagramPatch([LChange]))]));
  QueueEdit(LEdit, nil, LRequest);
end;
