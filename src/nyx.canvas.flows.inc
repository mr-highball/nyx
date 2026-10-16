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
  { The view's private copied source is adopted after every fragment has staged
    its replacement. The outer call retains the view. This preparation owns its
    candidate; after install it owns the former source until notification/retire
    finish. No document destruction or creator callout occurs during Install. }
  TCanvasFlowPublication = class(TInterfacedObject, INyxPreparedPublication,
    INyxPublicationReadiness)
  public
    Owner: TCanvasView;
    Candidate: TNyxDocument;
    Former: TNyxDocument;
    SceneRevision: Integer;
    FlowRevision: Integer;
    Validated: Boolean;
    Installed: Boolean;
    destructor Destroy; override;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

destructor TCanvasFlowPublication.Destroy;
begin
  Candidate.Free;

  if Installed then
  begin
    Former.Free;
  end;
  inherited Destroy;
end;

procedure TCanvasFlowPublication.CheckReady;
begin

  if (Owner = nil) or not Owner.FConnected or Owner.FDisconnectPending or
    not Owner.FUpdating or Installed or
    (Owner.FDocument <> Former) or
    (Owner.FWorkspace.Scene.Revision <> SceneRevision) or
    (Owner.FFlowRevision <> FlowRevision) then
  begin
    raise ENyxCanvasScene.Create('Canvas changed during connection admission');
  end;
end;

procedure TCanvasFlowPublication.Validate;
begin

  if Validated then
  begin
    raise ENyxCanvasScene.Create('Canvas connection publication is single use');
  end;
  CheckReady;
  Validated := True;
end;

procedure TCanvasFlowPublication.Install;
begin

  if not Validated or Installed or (Owner = nil) then
  begin
    Exit;
  end;
  Owner.FDocument := Candidate;
  Candidate := nil;
  Inc(Owner.FPresentationRevision);
  Inc(Owner.FFlowRevision);
  Installed := True;
end;

procedure TCanvasFlowPublication.Notify;
begin
  { The controller refreshes ordinary port/connection chrome after the group. }
end;

procedure TCanvasFlowPublication.Retire;
begin
  Owner := nil;
end;

function TCanvasView.RuntimeFlows: INyxFlowDeclarations;
begin
  RequireConnected;
  Result := FDocument.Flows.Clone;
end;

function TCanvasView.GetFlowRevision: Integer;
begin
  RequireConnected;
  Result := FFlowRevision;
end;

procedure TCanvasView.ApplyFlowChanges(const AChanges: array of TNyxFlowChange;
  AExpectedScene, AExpectedFlows: Integer);
var
  LLease: INyxCanvasWorkspaceView;
  LSource: TCanvasFlowPublication;
  LSourceLease: INyxPreparedPublication;
  LPrepared: TNyxPreparedPublications;
  LFlows: INyxFlowDeclarations;
  LFragment: INyxViewFlowEditing;
  LContext: TNyxDocument;
  LRoot: TNyxNode;
  LIndex: Integer;
  LCount: Integer;
  LSeen: array of TNyxFlowRef;
  LChanged: array of TNyxFlowRef;
  LReference: TNyxFlowRef;
  LPrevious: Integer;
  LRouteIndex: Integer;
  LFound: Boolean;
  LDiffers: Boolean;
  LBinding: INyxFlowRouteBinding;
begin
  LLease := Self;
  RequireConnected;

  if FUpdating or (AExpectedScene <> FWorkspace.Scene.Revision) or
    (AExpectedFlows <> FFlowRevision) or (FFlowRevision = High(Integer)) or
    (FPresentationRevision = High(Integer)) or (Length(FMounts) > 2047) or
    (Length(AChanges) = 0) then
  begin
    raise ENyxCanvasScene.Create('Connections require one exact unreserved workspace revision');
  end;
  LSeen := nil;
  LChanged := nil;
  FUpdating := True;
  try
    LSource := TCanvasFlowPublication.Create;
    LSourceLease := LSource;
    LSource.Owner := Self;
    LSource.Former := FDocument;
    LSource.SceneRevision := AExpectedScene;
    LSource.FlowRevision := AExpectedFlows;
    LSource.Candidate := FDocument.Clone;
    LFlows := EditNyxFlows(FDocument.Flows, AChanges);
    { Route ownership is per embedded fragment. Preserve unrelated fragments,
      including their in-flight work. An exact no-op still advances the owning
      graph revision, but needs no observer/context replacement. }
    for LIndex := 0 to High(AChanges) do
    begin
      LReference := AChanges[LIndex].Reference;
      LFound := False;
      for LPrevious := 0 to High(LSeen) do
      begin
        LFound := LFound or (LSeen[LPrevious].Name = LReference.Name);
      end;

      if LFound then
      begin
        Continue;
      end;
      SetLength(LSeen, Length(LSeen) + 1);
      LSeen[High(LSeen)] := LReference;
      LDiffers := FDocument.Flows.Contains(LReference) <> LFlows.Contains(LReference);

      if not LDiffers and LFlows.Contains(LReference) then
      begin
        LDiffers := FDocument.Flows.Definition(LReference).ToData.ToJSON <>
          LFlows.Definition(LReference).ToData.ToJSON;
      end;

      if LDiffers then
      begin
        SetLength(LChanged, Length(LChanged) + 1);
        LChanged[High(LChanged)] := LReference;
      end;
    end;
    while LSource.Candidate.Flows.Count > 0 do
    begin
      LSource.Candidate.Flows.Remove(LSource.Candidate.Flows.Reference(0));
    end;
    for LIndex := 0 to LFlows.Count - 1 do
    begin
      LSource.Candidate.Flows.Define(LFlows.Reference(LIndex),
        LFlows.Definition(LFlows.Reference(LIndex)));
    end;
    LSource.Candidate.Validate;
    SetLength(LPrepared, Length(FMounts) + 1);
    LCount := 0;
    try
      for LIndex := 0 to High(FMounts) do
      begin

        if FMounts[LIndex].Behavior = nil then
        begin
          Continue;
        end;
        LFound := False;
        LBinding := FMounts[LIndex].Behavior.Routes;
        for LRouteIndex := 0 to LBinding.Count - 1 do
        begin
          for LPrevious := 0 to High(LChanged) do
          begin
            { Report is the existing copied status boundary, not executable
              text. Compare its exact graph identity against typed changes. }
            LFound := LFound or
              (LBinding.Report(LRouteIndex).Field('flow').AsText = LChanged[LPrevious].Name);
          end;
        end;

        if not LFound then
        begin
          Continue;
        end;
        LFragment := nil;

        if not Supports(FMounts[LIndex].Behavior, INyxViewFlowEditing, LFragment) then
        begin
          raise ENyxCanvasScene.Create('Embedded owner cannot prepare connection changes');
        end;
        LContext := BuildRouteContext(LSource.Candidate, FMounts[LIndex].Reference,
          FMounts[LIndex].Content.Node, FRouteOverrides, LRoot);
        try
          LPrepared[LCount] := LFragment.PrepareFlowRoutes(LContext, LRoot);
          Inc(LCount);
        finally
          LContext.Free;
        end;
      end;
      LPrepared[LCount] := LSourceLease;
      SetLength(LPrepared, LCount + 1);
      PublishNyxGroup(LPrepared);
    finally
      for LIndex := 0 to High(LPrepared) do
      begin

        if LPrepared[LIndex] <> nil then
        begin
          LPrepared[LIndex].Retire;
        end;
      end;
    end;
  finally
    FUpdating := False;

    if FDisconnectPending then
    begin
      Disconnect;
    end;
  end;
end;
