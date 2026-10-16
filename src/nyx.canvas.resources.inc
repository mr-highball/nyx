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
{ Both canvas adapters contribute their complete retained scopes to the existing
  application resource coordinator. No transport, cache or document mutation is
  introduced here; future mounts read the same owner's current context. }

function TCanvasView.ResourcesReady(const AContext: INyxResourceContext): Boolean;
var
  LIndex: Integer;
begin
  FScheduler.RequireUI;
  Result := FConnected and not FUpdating and not FConstructing and not FSourcePreparing;

  if Result and (FGuideInput <> nil) then
  begin
    Result := not FGuideInput.Active;
  end;

  if not Result then
  begin
    Exit;
  end;
  Result := FChrome.CanReloadResources(AContext);

  if not Result then
  begin
    Exit;
  end;
  for LIndex := 0 to High(FMounts) do
  begin

    if not FMounts[LIndex].Renderer.CanReloadResources(AContext) then
    begin
      Exit(False);
    end;
  end;
end;

function TCanvasView.PrepareResources(const AContext: INyxResourceContext;
  out APrepared: INyxPreparedPublication): Boolean;
var
  LStages: TNyxPreparedPublications;
  LGroups: TNyxPreparedPublications;
  LBatch: TNyxPreparedPublications;
  LIndex: Integer;
  LMember: Integer;
  LFirst: Integer;
begin
  APrepared := nil;
  FScheduler.RequireUI;
  { The broker already ran every readiness validator before entering any
    preparer. An earlier application/scalar participant may now hold this same
    state snapshot. Re-running busy validation here would turn the coordinator's
    own reservation into a permanent Waiting result. Each preparation still
    admits its current scope and owns its ordinary retirement fences. }
  Result := FConnected and not FUpdating and not FConstructing and not FSourcePreparing;

  if Result and (FGuideInput <> nil) then
  begin
    Result := not FGuideInput.Active;
  end;

  if not Result then
  begin
    Exit;
  end;
  SetLength(LStages, Length(FMounts) + 1);
  try
    LStages[0] := FChrome.PrepareResourceReload(AContext);
    for LIndex := 0 to High(FMounts) do
    begin
      LStages[LIndex + 1] := FMounts[LIndex].Renderer.PrepareResourceReload(AContext);
    end;
    { Retained mounts can span the scene's full 10,000 items after navigation.
      Bound each leaf group below the coordinator's 2,048-participant limit;
      nesting still installs every scope before any target notification. This
      does not establish a large-scene frame/update performance budget. }
    SetLength(LGroups, (Length(LStages) + 255) div 256);
    for LIndex := 0 to High(LGroups) do
    begin
      LFirst := LIndex * 256;
      SetLength(LBatch, Min(256, Length(LStages) - LFirst));
      for LMember := 0 to High(LBatch) do
      begin
        LBatch[LMember] := LStages[LFirst + LMember];
      end;
      LGroups[LIndex] := PrepareNyxGroup(LBatch);
    end;
    APrepared := PrepareNyxGroup(LGroups);
  except
    { Also retire reservations acquired before a later scope refused. }

    for LIndex := 0 to High(LStages) do
    begin

      if LStages[LIndex] <> nil then
      begin
        LStages[LIndex].Retire;
      end;
    end;
    raise;
  end;
end;
