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
{ Included by nyx.studio.sourcejobs. Adoption retains the existing per-project
  queue, compiler, exact candidate guards and one paired history publication. }

destructor TNyxEditorSchemaLease.Destroy;
begin
  RetireAdoption('Connection save cancelled; current project and live wiring are retained.');
  inherited Destroy;
end;

procedure TNyxEditorSchemaLease.RetireAdoption(const AReason: TNyxText);
var
  LRequest: INyxCanvasRouteAdoptionRequest;
begin
  LRequest := FAdoption;
  FAdoption := nil;

  if LRequest <> nil then
  begin
    try

      if LRequest.Pending then
      begin
        LRequest.Refused(AReason);
      end;
    except
      { A presentation failure cannot prevent retiring a queue/worker owner.
        The ordinary source-command status remains separately authoritative. }
    end;
  end;
end;

procedure TCanvasAdoptionHost.Submit(const ARequest: INyxCanvasRouteAdoptionRequest);
var
  LLease: INyxCanvasRouteAdoptionHost;
  LRequest: INyxCanvasRouteAdoptionRequest;
  LPort: ICanvasAdoptionSubmission;
begin
  LLease := Self;
  LRequest := ARequest;
  LPort := Port;

  if LPort = nil then
  begin
    raise ENyxModel.Create('The connection save host has retired');
  end;
  LPort.SubmitCanvas(Context, View, LRequest);
end;

procedure TSourcePort.SubmitCanvas(const AContext: TNyxStudioCommandContext;
  const AView: TNyxControlRef; const ARequest: INyxCanvasRouteAdoptionRequest);
var
  LOwner: TNyxSourceCommands;
begin
  LOwner := Owner;

  if LOwner = nil then
  begin
    raise ENyxModel.Create('The owning project has retired its connection save host');
  end;
  LOwner.AdoptCanvas(AContext, AView, ARequest);
end;

function TNyxSourceCommands.CanvasAdoptionHost(const AContext: TNyxStudioCommandContext;
  const AView: TNyxControlRef): INyxCanvasRouteAdoptionHost;
var
  LPort: ICanvasAdoptionSubmission;
  LHost: TCanvasAdoptionHost;
begin
  FScheduler.RequireUI;

  if FDetached or not FSession.MatchesCommandContext(AContext) or
    (FSession.ActiveViewID <> AView.ID) or
    not Supports(FPort, ICanvasAdoptionSubmission, LPort) then
  begin
    raise ENyxModel.Create('Connection save host requires the current project load and view');
  end;
  LHost := TCanvasAdoptionHost.Create;
  Result := LHost;
  LHost.Port := LPort;
  LHost.Context := AContext;
  LHost.View := NyxControl(AView.ID);
end;

procedure TNyxSourceCommands.AdoptCanvas(const AContext: TNyxStudioCommandContext;
  const AView: TNyxControlRef; const ARequest: INyxCanvasRouteAdoptionRequest);
var
  LRequest: INyxCanvasRouteAdoptionRequest;
  LSnapshot: TNyxCanvasRouteAdoptionSnapshot;
  LChanges: TNyxCanvasRouteChanges;
  LGroup: TNyxCanvasRouteChanges;
  LOperations: array of TNyxCanvasChange;
  LFlowChanges: TNyxFlowChanges;
  LProgramChanges: TNyxProgramChanges;
  LSteps: array of TNyxTransactionStep;
  LEdit: TNyxStudioDesignEdit;
  LPosition: Integer;
  LCount: Integer;
  LIndex: Integer;
begin
  FScheduler.RequireUI;
  LRequest := ARequest;

  if FDetached or not FSession.MatchesCommandContext(AContext) or
    (FSession.ActiveViewID <> AView.ID) then
  begin
    raise ENyxModel.Create('The live canvas belongs to an earlier project load or view');
  end;

  if (LRequest = nil) or not LRequest.Pending then
  begin
    raise ENyxModel.Create('Submit a current connection save request');
  end;

  if Busy then
  begin
    raise ENyxModel.Create('Wait for the current source command before saving connections');
  end;
  LSnapshot := LRequest.Snapshot;

  if not LSnapshot.Defined or (TNyxCodec.Encode(FSession.Document) <> LSnapshot.BaseDesign) then
  begin
    raise ENyxModel.Create('The project changed; reopen its live canvas before saving these connections');
  end;

  if (FSession.Document.Find(AView.ID) = nil) or
    (FSession.Document.Find(AView.ID).Find(LSnapshot.Owner.ID) = nil) then
  begin
    raise ENyxModel.Create('Save connections only for a canvas in the owning active view');
  end;
  LChanges := LSnapshot.Changes;
  LFlowChanges := LSnapshot.FlowChanges;
  LProgramChanges := LSnapshot.ProgramChanges;

  if not LSnapshot.HasChanges then
  begin
    LRequest.Accepted(FSession.Document);
    Exit;
  end;
  LPosition := 0;
  LCount := (Length(LChanges) + MaximumNyxCanvasConnectionEdits - 1) div
    MaximumNyxCanvasConnectionEdits;
  SetLength(LOperations, LCount);
  for LIndex := 0 to High(LOperations) do
  begin
    LCount := Length(LChanges) - LPosition;

    if LCount > MaximumNyxCanvasConnectionEdits then
    begin
      LCount := MaximumNyxCanvasConnectionEdits;
    end;
    LGroup := Copy(LChanges, LPosition, LCount);
    LOperations[LIndex] := NyxSetCanvasConnections(LSnapshot.Scene, LGroup);
    Inc(LPosition, LCount);
  end;
  LEdit := Default(TNyxStudioDesignEdit);
  LEdit.Action := sdaTransaction;
  LEdit.Selection := FSession.SelectedID;
  LEdit.View := AView.ID;
  LEdit.TransactionDesignBaseline := LSnapshot.BaseDesign;
  LSteps := nil;

  if Length(LProgramChanges) > 0 then
  begin
    SetLength(LSteps, 1);
    LSteps[0] := NyxDesignStep(NyxProgramPatch(LProgramChanges));
  end;

  if Length(LFlowChanges) > 0 then
  begin
    SetLength(LSteps, Length(LSteps) + 1);
    LSteps[High(LSteps)] := NyxDesignStep(NyxFlowPatch(LFlowChanges));
  end;

  if Length(LOperations) > 0 then
  begin
    SetLength(LSteps, Length(LSteps) + 1);
    LSteps[High(LSteps)] := NyxDesignStep(NyxCanvasPatch(LOperations));
  end;
  { Adjacent design steps coalesce in the existing transaction. New graph
    references and their route masks therefore admit as one complete candidate,
    never an intermediate project with dangling routes. }
  LEdit.Transaction := CaptureNyxProjectTransaction(NyxProjectTransaction(LSteps));
  QueueEdit(LEdit, LRequest);
end;
