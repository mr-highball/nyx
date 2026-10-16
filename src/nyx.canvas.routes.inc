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
  { Whole private per-item overlay publication. The public call retains the view;
    this preparation owns its candidate and, after Install, its former overlay.
    No renderer/control/document changes ownership. All affected fragment plans
    install in the same group before notifications or old-work cancellation. }
  TCanvasRoutePublication = class(TInterfacedObject, INyxPreparedPublication,
    INyxPublicationReadiness)
  public
    Owner: TCanvasView;
    Candidate: TNyxCanvasRouteOverrides;
    Former: TNyxCanvasRouteOverrides;
    SceneRevision: Integer;
    RouteRevision: Integer;
    Validated: Boolean;
    Installed: Boolean;
    destructor Destroy; override;
    procedure CheckReady;
    procedure Validate;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

destructor TCanvasRoutePublication.Destroy;
begin
  Candidate.Free;

  if Installed then
  begin
    Former.Free;
  end;
  inherited Destroy;
end;

procedure TCanvasRoutePublication.CheckReady;
begin

  if (Owner = nil) or not Owner.FConnected or Owner.FDisconnectPending or
    not Owner.FUpdating or Installed or (Owner.FRouteOverrides <> Former) or
    (Owner.FWorkspace.Scene.Revision <> SceneRevision) or
    (Owner.FFlowRevision <> RouteRevision) then
  begin
    raise ENyxCanvasScene.Create('Canvas route ownership changed during admission');
  end;
end;

procedure TCanvasRoutePublication.Validate;
begin

  if Validated then
  begin
    raise ENyxCanvasScene.Create('Canvas route publication is single use');
  end;
  CheckReady;
  Validated := True;
end;

procedure TCanvasRoutePublication.Install;
begin

  if not Validated or Installed or (Owner = nil) then
  begin
    Exit;
  end;
  Owner.FRouteOverrides := Candidate;
  Candidate := nil;
  Inc(Owner.FFlowRevision);
  Inc(Owner.FPresentationRevision);
  Installed := True;
end;

procedure TCanvasRoutePublication.Notify;
begin
  { Presentation follows the coordinated group; no event is replayed. }
end;

procedure TCanvasRoutePublication.Retire;
begin
  Owner := nil;
end;

function TCanvasView.BuildRouteContext(ASource: TNyxDocument;
  const AItem: TNyxCanvasItemRef; AContent: TNyxNode;
  AOverrides: TNyxCanvasRouteOverrides; out AOwnedRoot: TNyxNode): TNyxDocument;
var
  LIdentity: IInterface;
begin
  Result := NewNyxCanvasRenderContext(ASource, AContent, AOwnedRoot);
  try

    if (AOverrides <> nil) and AOverrides.HasItem(AItem) then
    begin
      LIdentity := NyxCanvasContentIdentity(FWorkspace.Scene, AItem);
      AOverrides.Configure(AItem, LIdentity, Result, AOwnedRoot);
    end;
  except
    AOwnedRoot := nil;
    Result.Free;
    raise;
  end;
end;

function TCanvasView.RouteContext(const AItem: TNyxCanvasItemRef;
  out AOwnedRoot: TNyxNode): TNyxDocument;
var
  LContent: INyxNode;
begin
  AOwnedRoot := nil;
  RequireConnected;
  NyxCanvasContentIdentity(FWorkspace.Scene, AItem);
  LContent := FWorkspace.Scene.Control(AItem);
  Result := BuildRouteContext(FDocument, AItem, LContent.Node, FRouteOverrides, AOwnedRoot);
  try
    Result.Validate;
  except
    AOwnedRoot := nil;
    Result.Free;
    raise;
  end;
end;

function TCanvasView.CaptureRoutes: TNyxCanvasRouteChanges;
begin
  RequireConnected;
  Result := FRouteOverrides.Capture(FWorkspace.Scene);
end;

function TCanvasView.GetRouteRevision: Integer;
begin
  RequireConnected;
  Result := FFlowRevision;
end;

procedure TCanvasView.ApplyRouteChanges(const AChanges: array of TNyxCanvasRouteChange;
  AExpectedScene, AExpectedRoutes: Integer);
var
  LLease: INyxCanvasWorkspaceView;
  LSource: TCanvasRoutePublication;
  LSourceLease: INyxPreparedPublication;
  LPrepared: TNyxPreparedPublications;
  LFragment: INyxViewRouteTopology;
  LContext: TNyxDocument;
  LFormerContext: TNyxDocument;
  LRoot: TNyxNode;
  LFormerRoot: TNyxNode;
  LTarget: TNyxNode;
  LContent: INyxNode;
  LItems: array of TNyxCanvasItemRef;
  LChanged: array of Boolean;
  LIndex: Integer;
  LPrevious: Integer;
  LItemIndex: Integer;
  LCount: Integer;
  LFound: Boolean;
begin
  LLease := Self;
  RequireConnected;

  if FUpdating or FDesignMode or (FParentContext = nil) or not FParentContext.Active or
    (AExpectedScene <> FWorkspace.Scene.Revision) or (AExpectedRoutes <> FFlowRevision) or
    (FFlowRevision = High(Integer)) or (FPresentationRevision = High(Integer)) or
    (Length(FMounts) > 2047) or (Length(AChanges) = 0) or (Length(AChanges) > 64) then
  begin
    raise ENyxCanvasScene.Create('Routes require 1..64 exact changes in an active unreserved runtime workspace');
  end;
  LItems := nil;
  LChanged := nil;
  LPrepared := nil;
  FUpdating := True;
  try
    LSource := TCanvasRoutePublication.Create;
    LSourceLease := LSource;
    LSource.Owner := Self;
    LSource.Former := FRouteOverrides;
    LSource.SceneRevision := AExpectedScene;
    LSource.RouteRevision := AExpectedRoutes;
    LSource.Candidate := FRouteOverrides.Clone(FWorkspace.Scene);
    for LIndex := 0 to High(AChanges) do
    begin
      for LPrevious := 0 to LIndex - 1 do
      begin

        if (AChanges[LIndex].Item.Name = AChanges[LPrevious].Item.Name) and
          (AChanges[LIndex].Control.ID = AChanges[LPrevious].Control.ID) then
        begin
          raise ENyxCanvasScene.Create('Combine each item/control route replacement once before publication');
        end;
      end;
      LFound := False;
      for LPrevious := 0 to High(LItems) do
      begin
        LFound := LFound or (LItems[LPrevious].Name = AChanges[LIndex].Item.Name);
      end;

      if not LFound then
      begin
        SetLength(LItems, Length(LItems) + 1);
        LItems[High(LItems)] := AChanges[LIndex].Item;
      end;
      LSource.Candidate.Define(FWorkspace.Scene, AChanges[LIndex]);
    end;
    SetLength(LChanged, Length(LItems));
    for LItemIndex := 0 to High(LItems) do
    begin
      LContent := FWorkspace.Scene.Control(LItems[LItemIndex]);
      LContext := BuildRouteContext(FDocument, LItems[LItemIndex], LContent.Node,
        LSource.Candidate, LRoot);
      LFormerContext := nil;
      try
        LContext.Validate;
        LFormerContext := BuildRouteContext(FDocument, LItems[LItemIndex], LContent.Node,
          FRouteOverrides, LFormerRoot);
        for LIndex := 0 to High(AChanges) do
        begin

          if AChanges[LIndex].Item.Name <> LItems[LItemIndex].Name then
          begin
            Continue;
          end;
          LTarget := LFormerRoot.Find(AChanges[LIndex].Control.ID);

          if LTarget = nil then
          begin
            raise ENyxCanvasScene.Create('Choose an exact authored control inside this canvas item');
          end;
          { An explicit empty set masks inherited routes, so absent/local-empty
            are distinct even when their immediate route counts both read zero. }
          LChanged[LItemIndex] := LChanged[LItemIndex] or
            not LTarget.Extensions.Has(NyxExtension(NyxFlowRoutesKey)) or
            (NyxControlFlowRoutes(LTarget).ToData.ToJSON <> AChanges[LIndex].Routes.ToData.ToJSON);
        end;
      finally
        LFormerContext.Free;
        LContext.Free;
      end;
    end;
    SetLength(LPrepared, Length(FMounts) + 1);
    LCount := 0;
    try
      for LIndex := 0 to High(FMounts) do
      begin
        LFound := False;
        for LItemIndex := 0 to High(LItems) do
        begin
          LFound := LFound or (LChanged[LItemIndex] and
            (FMounts[LIndex].Reference.Name = LItems[LItemIndex].Name));
        end;

        if not LFound then
        begin
          Continue;
        end;

        if not Supports(FMounts[LIndex].Behavior, INyxViewRouteTopology, LFragment) then
        begin
          raise ENyxCanvasScene.Create('This embedded item cannot prepare dynamic event routes');
        end;
        LContext := BuildRouteContext(FDocument, FMounts[LIndex].Reference,
          FMounts[LIndex].Content.Node, LSource.Candidate, LRoot);
        try
          LPrepared[LCount] := LFragment.PrepareRouteChanges(LContext, LRoot);
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
