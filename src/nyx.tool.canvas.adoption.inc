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
{ Included by nyx.tool.canvas. Private tickets borrow their issuing inspector;
  Disconnect revokes that pointer before host/widget/source retirement. }

constructor TAdoptionRequest.Create(AOwner: TInspector;
  const ASnapshot: TNyxCanvasRouteAdoptionSnapshot);
begin
  inherited Create;
  FOwner := AOwner;
  FSnapshot := ASnapshot;
  FPending := True;
end;

procedure TAdoptionRequest.Retire;
begin
  FPending := False;
  FOwner := nil;
end;

function TAdoptionRequest.GetSnapshot: TNyxCanvasRouteAdoptionSnapshot;
begin
  Result := FSnapshot;
end;

function TAdoptionRequest.GetPending: Boolean;
begin
  Result := FPending and (FOwner <> nil);
end;

function TAdoptionRequest.Accepted(ADocument: TNyxDocument): Boolean;
var
  LRequest: INyxCanvasRouteAdoptionRequest;
  LInspector: INyxToolCanvasInspector;
  LOwner: TInspector;
  LSource: TNyxDocument;
  LOwnedSource: TNyxDocument;
  LMessage: TNyxText;
begin
  Result := False;
  LRequest := Self;
  LOwner := FOwner;

  if not GetPending then
  begin
    Exit;
  end;
  LInspector := LOwner;

  if (ADocument = nil) or (TNyxCodec.Encode(ADocument) <> FSnapshot.CandidateDesign) then
  begin
    raise ENyxCanvasScene.Create('Acknowledge only the exact published connection snapshot');
  end;
  LSource := ADocument.Clone;
  try

    if not GetPending or (FOwner <> LOwner) or not LOwner.FConnected or
      (LOwner.FAdoptionRequest <> Self) then
    begin
      Exit;
    end;
    LMessage := 'Applied graphs and connections saved.';

    if (LOwner.FRuntime.Scene.Revision <> FSnapshot.SceneRevision) or
      (LOwner.FRouteEditing.RouteRevision <> FSnapshot.RouteRevision) or
      ((FSnapshot.FlowRevision >= 0) and ((LOwner.FFlowEditing = nil) or
        (LOwner.FFlowEditing.FlowRevision <> FSnapshot.FlowRevision))) then
    begin
      LMessage := 'Snapshot saved. Newer live changes remain available to save.';
    end;
    Retire;
    LOwnedSource := LSource;
    LSource := nil;
    { The receiver takes ownership at entry, including its refusal path. }
    LOwner.AdoptionCompleted(Self, LOwnedSource, LMessage);
    Result := True;
  finally
    LSource.Free;
  end;
end;

function TAdoptionRequest.Refused(const AMessage: TNyxText): Boolean;
var
  LRequest: INyxCanvasRouteAdoptionRequest;
  LInspector: INyxToolCanvasInspector;
  LOwner: TInspector;
  LMessage: TNyxText;
begin
  Result := False;
  LRequest := Self;
  LOwner := FOwner;

  if not GetPending then
  begin
    Exit;
  end;
  LInspector := LOwner;
  LMessage := AMessage;

  if LMessage = '' then
  begin
    LMessage := 'Connections were not saved; live wiring is retained.';
  end;
  Retire;
  LOwner.AdoptionCompleted(Self, nil, LMessage);
  Result := True;
end;

procedure TInspector.ConfigureAdoption(const AOwner: TNyxControlRef;
  const AHost: INyxCanvasRouteAdoptionHost; AChanged: TNyxContentUpdate);
var
  LLease: INyxToolCanvasInspector;
  LHost: INyxCanvasRouteAdoptionHost;
begin
  RequireConnected;
  LLease := Self;
  LHost := AHost;

  if GetAdoptionPending then
  begin
    raise ENyxCanvasScene.Create('Wait for the pending connection save before replacing its project host');
  end;

  if LHost <> nil then
  begin
    FAdoptionOwner := NyxControl(AOwner.ID);
  end
  else
  begin
    FAdoptionOwner := Default(TNyxControlRef);
  end;
  FAdoptionHost := LHost;
  FAdoptionChanged := AChanged;
  FAdoptionFeedback := '';
  Rebuild;
end;

function TInspector.GetAdoptionPending: Boolean;
begin
  Result := (FAdoptionRequest <> nil) and FAdoptionRequest.GetPending;
end;

function TInspector.GetAdoptionFeedback: TNyxText;
begin
  Result := FAdoptionFeedback;
end;

procedure TInspector.UpdateAdoptionFeedback;
var
  LNode: TNyxNode;
  LChanged: TNyxContentUpdate;
begin

  if not FConnected or (FRoot = nil) then
  begin
    Exit;
  end;
  LNode := FRoot.Node.Find(PartID('save-connections-feedback'));

  if LNode <> nil then
  begin
    LNode.Configure.Text(FAdoptionFeedback).Done;
  end;
  LNode := FRoot.Node.Find(PartID('save-connections'));

  if LNode <> nil then
  begin
    LNode.Configure.Enabled(not GetAdoptionPending).Done;
  end;
  LChanged := FAdoptionChanged;

  if Assigned(LChanged) then
  begin
    LChanged;
  end;
end;

procedure TInspector.AdoptionCompleted(ARequest: TAdoptionRequest;
  AOwnedSource: TNyxDocument; const AMessage: TNyxText);
var
  LLease: INyxToolCanvasInspector;
begin
  LLease := Self;
  try

    if not FConnected or (FAdoptionRequest <> ARequest) then
    begin
      Exit;
    end;

    if AOwnedSource <> nil then
    begin
      FSource.Free;
      FSource := AOwnedSource;
      AOwnedSource := nil;
      { Discovery is recreated on its next use. Its currently mounted editor
        and separately owned event-route proposal remain completely intact. }
      ReleaseNyxNode(FEffectiveRoot);
      FEffectiveRoot := nil;
      FreeAndNil(FEffectiveContext);
    end;
    FAdoptionRequest := nil;
    FAdoptionLease := nil;
    FAdoptionFeedback := AMessage;
    try
      UpdateAdoptionFeedback;
    except
      on E: Exception do
      begin
        { Baseline/ticket admission is already complete. A borrowed host's
          presentation failure cannot turn a saved snapshot into a refusal or
          request remounting live controls. Retain its diagnostic for inspection. }
        FAdoptionFeedback := AMessage + ' / Feedback refresh failed: ' + E.Message;
      end;
    end;
  finally
    AOwnedSource.Free;
  end;
end;

procedure TInspector.SaveConnections;
var
  LLease: INyxToolCanvasInspector;
  LHost: INyxCanvasRouteAdoptionHost;
  LSnapshot: TNyxCanvasRouteAdoptionSnapshot;
  LRequest: INyxCanvasRouteAdoptionRequest;
  LTicket: TAdoptionRequest;
begin
  RequireConnected;
  LLease := Self;
  LHost := FAdoptionHost;

  if LHost = nil then
  begin
    raise ENyxCanvasScene.Create('Connect a project save host before adopting connections');
  end;

  if GetAdoptionPending then
  begin
    raise ENyxCanvasScene.Create('A connection snapshot is already being saved');
  end;
  LSnapshot := CaptureNyxCanvasRouteAdoption(FSource, FAdoptionOwner, FView);

  if not LSnapshot.HasChanges then
  begin
    FAdoptionFeedback := 'Applied graphs and connections are already saved. Unapplied proposals stay local.';
    UpdateAdoptionFeedback;
    Exit;
  end;
  LTicket := TAdoptionRequest.Create(Self, LSnapshot);
  LRequest := LTicket;
  FAdoptionLease := LRequest;
  FAdoptionRequest := LTicket;
  FAdoptionFeedback := 'Saving applied graphs and connections...';
  try
    UpdateAdoptionFeedback;

    if not LRequest.Pending or not FConnected then
    begin
      Exit;
    end;
    LHost.Submit(LRequest);
  except
    on E: Exception do
    begin
      { A host notification may fail after acknowledging actual publication.
        Never overwrite that completed result or invent a second failure. }
      if LRequest.Pending then
      begin
        LRequest.Refused(E.Message);
      end;
    end;
  end;
end;
