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

{ Immutable saved connection configuration; the owning unit supplies all portable
  types. No document tree, live value, host object or executable creator is owned. }

function NyxCanvasConnectionChanges(const ADefinition: INyxCanvasDefinition): TNyxCanvasRouteChanges;
var
  LDefinition: INyxCanvasDefinition;
  LConnections: INyxCanvasDefinitionConnections;
  LIndex: Integer;
  LCount: Integer;
begin
  LDefinition := ADefinition;

  if LDefinition = nil then
  begin
    raise ENyxCanvasScene.Create('Connection inspection requires a saved canvas definition');
  end;
  Result := nil;

  if not Supports(LDefinition, INyxCanvasDefinitionConnections, LConnections) then
  begin
    Exit;
  end;
  LCount := LConnections.ConnectionCount;

  if (LCount < 0) or (LCount > MaximumNyxCanvasConnections) then
  begin
    raise ENyxCanvasScene.Create('Saved canvas connection count exceeds its declared capacity');
  end;
  SetLength(Result, LCount);
  for LIndex := 0 to LCount - 1 do
  begin
    Result[LIndex] := TNyxCanvasRouteChange.FromData(LConnections.Connection(LIndex).ToData);
  end;
end;

{ Normalize arbitrary creator data once, then copy all admitted intent into a
  raw private plan. The caller takes sole ownership until wrapping the builder. }
function CopyAdmittedCanvasPlan(const ADefinition: INyxCanvasDefinition): TCanvasDefinition;
var
  LLease: INyxCanvasDefinition;
  LNormalized: INyxCanvasDefinition;
  LPlan: TCanvasDefinition;
  LIndex: Integer;
begin
  LLease := ADefinition;

  if LLease = nil then
  begin
    raise ENyxCanvasScene.Create('Connection configuration requires a saved canvas definition');
  end;
  { Normalize creator data once while retaining its source interface. Subsequent
    getters address Nyx's admitted implementation, never arbitrary host code. }
  LNormalized := NyxCanvasDefinitionFromData(LLease.ToData);
  LPlan := TCanvasDefinition.Create;
  try
    LPlan.FCamera := LNormalized.Camera;
    LPlan.FGrid := LNormalized.Grid;
    LPlan.FScaling := LNormalized.Scaling;
    LPlan.FPrecision := NyxCanvasPrecisionOptionsOf(LNormalized);
    SetLength(LPlan.FLayers, LNormalized.LayerCount);
    SetLength(LPlan.FItems, LNormalized.ItemCount);
    for LIndex := 0 to LNormalized.LayerCount - 1 do
    begin
      LPlan.FLayers[LIndex] := LNormalized.Layers[LIndex];
    end;
    for LIndex := 0 to LNormalized.ItemCount - 1 do
    begin
      LPlan.FItems[LIndex] := LNormalized.Items[LIndex];
    end;
    LPlan.FConnections := NyxCanvasConnectionChanges(LNormalized);
    LPlan.FGraphNodes := NyxCanvasGraphNodes(LNormalized);
    Result := LPlan;
    LPlan := nil;
  finally
    LPlan.Free;
  end;
end;

function NyxCanvasConnections(const ADefinition: INyxCanvasDefinition): INyxCanvasConnections;
var
  LPlan: TCanvasDefinition;
begin
  LPlan := CopyAdmittedCanvasPlan(ADefinition);
  try
    Result := TCanvasConnections.CreateOwned(LPlan);
    LPlan := nil;
  finally
    LPlan.Free;
  end;
end;

constructor TCanvasConnections.CreateOwned(APlan: TCanvasDefinition);
begin
  inherited Create;

  if APlan = nil then
  begin
    raise ENyxCanvasScene.Create('A connection builder owns one independent saved plan');
  end;
  FPlan := APlan;
end;

destructor TCanvasConnections.Destroy;
begin
  FPlan.Free;
  inherited Destroy;
end;

function TCanvasDefinition.GetConnectionCount: Integer;
begin
  Result := Length(FConnections);
end;

function TCanvasDefinition.Connection(AIndex: Integer): TNyxCanvasRouteChange;
begin

  if (AIndex < 0) or (AIndex >= Length(FConnections)) then
  begin
    raise ENyxCanvasScene.Create('Saved canvas connection index is outside its definition');
  end;
  Result := TNyxCanvasRouteChange.FromData(FConnections[AIndex].ToData);
end;

procedure TCanvasDefinition.SetConnection(const AChange: TNyxCanvasRouteChange);
var
  LIndex: Integer;
  LPosition: Integer;
  LFound: Boolean;
  LItem: TNyxCanvasItemRef;
  LControl: TNyxControlRef;
  LChange: TNyxCanvasRouteChange;
begin
  LItem := AChange.Item;
  LControl := AChange.Control;
  LChange := nyx.canvas.route.types.NyxCanvasRoutes(LItem, LControl, AChange.Routes);
  LFound := False;
  for LIndex := 0 to High(FItems) do
  begin
    LFound := LFound or (FItems[LIndex].Item.Name = LItem.Name);
  end;

  if not LFound then
  begin
    raise ENyxCanvasScene.Create('A saved connection requires an item declared in that scene');
  end;
  LPosition := -1;
  for LIndex := 0 to High(FConnections) do
  begin

    if (FConnections[LIndex].Item.Name = LItem.Name) and
      (FConnections[LIndex].Control.ID = LControl.ID) then
    begin
      LPosition := LIndex;
      Break;
    end;
  end;

  if LPosition < 0 then
  begin

    if Length(FConnections) = MaximumNyxCanvasConnections then
    begin
      raise ENyxCanvasScene.Create('Saved canvas connections exceed their declared capacity');
    end;
    LPosition := Length(FConnections);
    SetLength(FConnections, LPosition + 1);
  end;
  FConnections[LPosition] := LChange;
end;

function TCanvasConnections.ForControl(const AItem: TNyxCanvasItemRef;
  const AControl: TNyxControlRef; const ARoutes: INyxFlowRoutes): INyxCanvasConnections;
var
  LLease: INyxCanvasConnections;
  LRoutes: INyxFlowRoutes;
begin
  LLease := Self;
  LRoutes := ARoutes;
  Result := Controls([nyx.canvas.route.types.NyxCanvasRoutes(AItem, AControl, LRoutes)]);
end;

function TCanvasConnections.Controls(
  const AChanges: array of TNyxCanvasRouteChange): INyxCanvasConnections;
var
  LPlan: TCanvasDefinition;
  LIndex: Integer;
  LPrevious: Integer;
begin

  if (Length(AChanges) < 1) or (Length(AChanges) > MaximumNyxCanvasConnections) then
  begin
    raise ENyxCanvasScene.Create('A connection group requires 1..2048 distinct item/control changes');
  end;
  LPlan := FPlan.CopyPlan;
  try
    for LIndex := 0 to High(AChanges) do
    begin
      for LPrevious := 0 to LIndex - 1 do
      begin

        if (AChanges[LIndex].Item.Name = AChanges[LPrevious].Item.Name) and
          (AChanges[LIndex].Control.ID = AChanges[LPrevious].Control.ID) then
        begin
          raise ENyxCanvasScene.Create('Combine each saved item/control route replacement once per group');
        end;
      end;
      LPlan.SetConnection(AChanges[LIndex]);
    end;

    if NyxUTF8ByteCount(LPlan.ToData.ToJSON) > MaximumNyxCanvasDefinitionBytes then
    begin
      raise ENyxCanvasScene.Create('Saved canvas connections exceed the complete definition byte budget');
    end;
    Result := TCanvasConnections.CreateOwned(LPlan);
    LPlan := nil;
  finally
    LPlan.Free;
  end;
end;

function TCanvasConnections.Inherit(const AItem: TNyxCanvasItemRef;
  const AControl: TNyxControlRef): INyxCanvasConnections;
var
  LPlan: TCanvasDefinition;
  LIndex: Integer;
  LPosition: Integer;
begin
  NyxCanvasItem(AItem.Name);
  NyxControl(AControl.ID);
  LPosition := -1;
  for LIndex := 0 to High(FPlan.FConnections) do
  begin

    if (FPlan.FConnections[LIndex].Item.Name = AItem.Name) and
      (FPlan.FConnections[LIndex].Control.ID = AControl.ID) then
    begin
      LPosition := LIndex;
      Break;
    end;
  end;

  if LPosition < 0 then
  begin
    raise ENyxCanvasScene.Create('Choose an existing saved connection before restoring inheritance');
  end;
  LPlan := FPlan.CopyPlan;
  try
    for LIndex := LPosition to High(LPlan.FConnections) - 1 do
    begin
      LPlan.FConnections[LIndex] := TNyxCanvasRouteChange.FromData(
        LPlan.FConnections[LIndex + 1].ToData);
    end;
    SetLength(LPlan.FConnections, Length(LPlan.FConnections) - 1);
    Result := TCanvasConnections.CreateOwned(LPlan);
    LPlan := nil;
  finally
    LPlan.Free;
  end;
end;

function TCanvasConnections.Done: INyxCanvasDefinition;
begin
  Result := FPlan.CopyPlan;
end;
