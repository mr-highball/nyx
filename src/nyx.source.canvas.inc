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
{ Closed canvas grammar invokes only the public immutable authoring contract.
  Tagged geometry values and descriptor snapshots retain no document/adapter. }

function TConfigurationReader.CanvasConstructor(const AName: TNyxText): TValue;
var
  LArgs: TValues;
  LMethod: TNyxText;
  LDefinition: INyxCanvasDefinition;
  LConnections: INyxCanvasConnections;
  LLayout: INyxCanvasGraphLayout;
  LVisible: Boolean;
  LPickable: Boolean;
  LZoom: Double;
  LLimits: TNyxCanvasZoomLimits;
  LStyle: TNyxCanvasGridStyle;
  LSnap: TNyxCanvasSnapMode;

  procedure Count(AMinimum, AMaximum: Integer);
  begin

    if (Length(LArgs) < AMinimum) or (Length(LArgs) > AMaximum) then
    begin
      Fail('Wrong canvas constructor argument count');
    end;
  end;

  procedure Kind(AIndex: Integer; AKind: TValueKind);
  begin

    if (AIndex >= Length(LArgs)) or (LArgs[AIndex].Kind <> AKind) then
    begin
      Fail('Wrong typed canvas argument family');
    end;
  end;

begin
  Result := Default(TValue);

  if AName = 'newnyxcanvasdefinition' then
  begin
    LArgs := nil;

    if At('(') then
    begin
      LArgs := Arguments;
    end;
    Count(0, 0);
    Result.Kind := vkCanvasDefinition;
    LDefinition := NewNyxCanvasDefinition;
  end
  else
  begin
    LArgs := Arguments(5);

    if AName = 'nyxcanvasgraphlayout' then
    begin
      Count(1, 1);
      Kind(0, vkCanvasDefinition);
      Result.Kind := vkCanvasGraphLayout;
      LLayout := NyxCanvasGraphLayout(NyxCanvasDefinitionFromData(LArgs[0].Data));
    end
    else if AName = 'nyxcanvasconnections' then
    begin
      Count(1, 1);
      Kind(0, vkCanvasDefinition);
      Result.Kind := vkCanvasConnections;
      LConnections := NyxCanvasConnections(NyxCanvasDefinitionFromData(LArgs[0].Data));
    end
    else if (AName = 'nyxcanvasref') or (AName = 'nyxcanvasitem') or (AName = 'nyxcanvaslayer') then
    begin
      Count(1, 1);
      Kind(0, vkText);
      Result.Text := LArgs[0].Text;

      if (AName = 'nyxcanvasref') then
      begin
        Result.Kind := vkCanvasRef;
        Result.Text := NyxCanvasRef(Result.Text).Name;
      end
      else if (AName = 'nyxcanvasitem') then
      begin
        Result.Kind := vkCanvasItem;
        Result.Text := NyxCanvasItem(Result.Text).Name;
      end
      else if (AName = 'nyxcanvaslayer') then
      begin
        Result.Kind := vkCanvasLayer;
        Result.Text := NyxCanvasLayer(Result.Text).Name;
      end;
    end
    else if (AName = 'nyxcanvaspoint') then
    begin
      Count(2, 2);
      Result.Kind := vkCanvasPoint;
      Result.CanvasPoint := NyxCanvasPoint(Numeric(LArgs[0]), Numeric(LArgs[1]));
    end
    else if (AName = 'nyxcanvasbounds') then
    begin
      Count(4, 4);
      Result.Kind := vkCanvasBounds;
      Result.CanvasBounds := NyxCanvasBounds(Numeric(LArgs[0]), Numeric(LArgs[1]),
        Numeric(LArgs[2]), Numeric(LArgs[3]));
    end
    else if (AName = 'nyxcanvaszoomlimits') then
    begin
      Count(0, 2);

      if Length(LArgs) = 1 then
      begin
        Fail('Zoom limits require both bounds');
      end;
      Result.Kind := vkCanvasLimits;
      Result.CanvasLimits := NyxCanvasZoomLimits;

      if Length(LArgs) = 2 then
      begin
        Result.CanvasLimits := NyxCanvasZoomLimits(Numeric(LArgs[0]), Numeric(LArgs[1]));
      end;
    end
    else if (AName = 'nyxcanvascamera') then
    begin
      Count(3, 5);
      Kind(2, vkCanvasPoint);
      LZoom := 1;
      LLimits := NyxCanvasZoomLimits;

      if Length(LArgs) >= 4 then
      begin
        LZoom := Numeric(LArgs[3]);
      end;

      if Length(LArgs) = 5 then
      begin
        Kind(4, vkCanvasLimits);
        LLimits := LArgs[4].CanvasLimits;
      end;
      Result.Kind := vkCanvasCamera;
      Result.CanvasCamera := NyxCanvasCamera(Numeric(LArgs[0]), Numeric(LArgs[1]),
        LArgs[2].CanvasPoint, LZoom, LLimits);
    end
    else if (AName = 'nyxcanvasgrid') then
    begin
      Count(0, 3);
      Result.Kind := vkCanvasGrid;
      Result.CanvasGrid := NyxCanvasGrid;
      LStyle := ncgsDots;
      LSnap := ncsmFree;

      if Length(LArgs) >= 2 then
      begin
        Kind(1, vkCanvasGridStyle);
        LStyle := TNyxCanvasGridStyle(LArgs[1].Ordinal);
      end;

      if Length(LArgs) = 3 then
      begin
        Kind(2, vkCanvasSnap);
        LSnap := TNyxCanvasSnapMode(LArgs[2].Ordinal);
      end;

      if Length(LArgs) > 0 then
      begin
        Result.CanvasGrid := NyxCanvasGrid(Numeric(LArgs[0]), LStyle, LSnap);
      end;
    end
    else if (AName = 'nyxcanvasplacement') then
    begin
      Count(2, 2);
      Kind(0, vkCanvasLayer);
      Kind(1, vkCanvasBounds);
      Result.Kind := vkCanvasPlacement;
      Result.CanvasPlacement := NyxCanvasPlacement(NyxCanvasLayer(LArgs[0].Text),
        LArgs[1].CanvasBounds);
    end
    else
      Fail('Unsupported typed canvas constructor');
  end;
  while At('.') do
  begin
    Inc(FCursor);
    LMethod := LowerCase(FTokens[FCursor].Text);
    Inc(FCursor);
    LArgs := nil;

    if At('(') then
    begin
      LArgs := Arguments;
    end;
    case Result.Kind of
      vkCanvasGraphLayout:
        begin

          if LMethod = 'place' then
          begin
            Count(3, 3);
            Kind(0, vkFlowRef);
            Kind(1, vkFlowStep);
            Kind(2, vkCanvasPoint);
            LLayout := LLayout.Place(NyxFlowRef(LArgs[0].Text), NyxFlowStep(LArgs[1].Text),
              LArgs[2].CanvasPoint);
          end
          else if LMethod = 'automatic' then
          begin
            Count(2, 2);
            Kind(0, vkFlowRef);
            Kind(1, vkFlowStep);
            LLayout := LLayout.Automatic(NyxFlowRef(LArgs[0].Text), NyxFlowStep(LArgs[1].Text));
          end
          else if LMethod = 'done' then
          begin
            Count(0, 0);
            LDefinition := LLayout.Done;
            Result.Kind := vkCanvasDefinition;
          end
          else
          begin
            Fail('Unsupported saved graph-layout method');
          end;
        end;
      vkCanvasConnections:
        begin

          if LMethod = 'forcontrol' then
          begin
            Count(3, 3);
            Kind(0, vkCanvasItem);
            Kind(1, vkControlRef);
            Kind(2, vkFlowRoutes);
            LConnections := LConnections.ForControl(NyxCanvasItem(LArgs[0].Text),
              NyxControl(LArgs[1].Text), NyxFlowRoutesFromData(LArgs[2].Data));
          end
          else if LMethod = 'inherit' then
          begin
            Count(2, 2);
            Kind(0, vkCanvasItem);
            Kind(1, vkControlRef);
            LConnections := LConnections.Inherit(NyxCanvasItem(LArgs[0].Text), NyxControl(LArgs[1].Text));
          end
          else if LMethod = 'done' then
          begin
            Count(0, 0);
            LDefinition := LConnections.Done;
            Result.Kind := vkCanvasDefinition;
          end
          else
          begin
            Fail('Unsupported saved connection configuration method');
          end;
        end;
      vkCanvasDefinition:
        begin

          if (LMethod = 'withcamera') then
          begin
            Count(1, 1);
            Kind(0, vkCanvasCamera);
            LDefinition := LDefinition.WithCamera(LArgs[0].CanvasCamera);
          end
          else if (LMethod = 'withgrid') then
          begin
            Count(1, 1);
            Kind(0, vkCanvasGrid);
            LDefinition := LDefinition.WithGrid(LArgs[0].CanvasGrid);
          end
          else if (LMethod = 'withscaling') then
          begin
            Count(1, 1);
            Kind(0, vkCanvasScaling);
            LDefinition := LDefinition.WithScaling(TNyxCanvasControlScaling(LArgs[0].Ordinal));
          end
          else if (LMethod = 'withlayer') then
          begin
            Count(1, 3);
            Kind(0, vkCanvasLayer);
            LVisible := True;
            LPickable := True;

            if Length(LArgs) >= 2 then
            begin
              Kind(1, vkBoolean);
              LVisible := LArgs[1].Ordinal <> 0;
            end;

            if Length(LArgs) = 3 then
            begin
              Kind(2, vkBoolean);
              LPickable := LArgs[2].Ordinal <> 0;
            end;
            LDefinition := LDefinition.WithLayer(NyxCanvasLayer(LArgs[0].Text),
              LVisible, LPickable);
          end
          else if (LMethod = 'withitem') then
          begin
            Count(3, 3);
            Kind(0, vkCanvasItem);
            Kind(1, vkRootRef);
            Kind(2, vkCanvasPlacement);
            LDefinition := LDefinition.WithItem(NyxCanvasItem(LArgs[0].Text),
              LArgs[1].RootRef, LArgs[2].CanvasPlacement);
          end
          else
            Fail('Unsupported immutable canvas definition method');
        end;
      vkCanvasGrid:
        begin
          Count(1, 1);

          if (LMethod = 'atorigin') then
          begin
            Kind(0, vkCanvasPoint);
            Result.CanvasGrid := Result.CanvasGrid.AtOrigin(LArgs[0].CanvasPoint);
          end
          else if (LMethod = 'styled') then
          begin
            Kind(0, vkCanvasGridStyle);
            Result.CanvasGrid := Result.CanvasGrid.Styled(TNyxCanvasGridStyle(LArgs[0].Ordinal));
          end
          else if (LMethod = 'snapping') then
          begin
            Kind(0, vkCanvasSnap);
            Result.CanvasGrid := Result.CanvasGrid.Snapping(TNyxCanvasSnapMode(LArgs[0].Ordinal));
          end
          else
            Fail('Unsupported typed grid method');
        end;
      vkCanvasPlacement:
        begin
          Count(1, 1);

          if (LMethod = 'visible') then
          begin
            Kind(0, vkBoolean);
            Result.CanvasPlacement := Result.CanvasPlacement.Visible(LArgs[0].Ordinal <> 0);
          end
          else if (LMethod = 'pickable') then
          begin
            Kind(0, vkBoolean);
            Result.CanvasPlacement := Result.CanvasPlacement.Pickable(LArgs[0].Ordinal <> 0);
          end
          else if (LMethod = 'atbounds') then
          begin
            Kind(0, vkCanvasBounds);
            Result.CanvasPlacement := Result.CanvasPlacement.AtBounds(LArgs[0].CanvasBounds);
          end
          else if (LMethod = 'onlayer') then
          begin
            Kind(0, vkCanvasLayer);
            Result.CanvasPlacement := Result.CanvasPlacement.OnLayer(NyxCanvasLayer(LArgs[0].Text));
          end
          else
            Fail('Unsupported typed placement method');
        end;
    else
      Fail('Canvas expression has no fluent method family');
    end;
  end;

  if Result.Kind = vkCanvasDefinition then
  begin
    Result.Data := LDefinition.ToData;
  end;

  if Result.Kind = vkCanvasConnections then
  begin
    Result.Data := LConnections.Done.ToData;
  end;

  if Result.Kind = vkCanvasGraphLayout then
  begin
    Result.Data := LLayout.Done.ToData;
  end;
end;

procedure TConfigurationReader.CanvasDefaults;
var
  LArgs: TValues;
begin
  Inc(FCursor, 3);
  repeat
    Expect('.');
    Expect('Define');
    LArgs := Arguments;

    if (Length(LArgs) <> 2) or (LArgs[0].Kind <> vkCanvasRef) or
      (LArgs[1].Kind <> vkCanvasDefinition) then
    begin
      Fail('Canvases.Define requires distinct reference and immutable definition');
    end;

    if FApply then
    begin

      if FDocument.Canvases.Contains(NyxCanvasRef(LArgs[0].Text)) then
      begin
        Fail('Define each canvas once in the managed builder');
      end;
      FDocument.Canvases.Define(NyxCanvasRef(LArgs[0].Text),
        NyxCanvasDefinitionFromData(LArgs[1].Data));
    end;
  until not At('.');
  Expect(';');
end;
