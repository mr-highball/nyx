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
unit nyx.split;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  SysUtils, Math, nyx.text, nyx.types, nyx.model, nyx.behavior, nyx.data, nyx.state;

type
  { Shared physical geometry. Extents are nonnegative, consume the available
    axis exactly. Explicit child minima constrain allocation without changing
    the requested proportion; infeasible minima compress proportionally. }
  TNyxSplitGeometry = record
    FirstExtent: Integer;
    DividerExtent: Integer;
    SecondExtent: Integer;
  end;

  { Adapter-owned sizing state, with no widget or descriptor ownership. A drag
    starts at the existing divider and measures deltas, avoiding an initial jump
    when the user touches either edge of its generous hit area. Cancellation
    restores the starting proportion. Layout never changes that proportion. }
  TNyxSplitState = class
  private
    FOrientation: TNyxSplitOrientation;
    FPosition: Integer;
    FMinimum: Integer;
    FMaximum: Integer;
    FResizable: Boolean;
    FDragging: Boolean;
    FStartPosition: Integer;
    FStartDragPosition: Integer;
    FStartCoordinate: Double;
    FStartExtent: Integer;
    FFirstMinimum: Integer;
    FSecondMinimum: Integer;
    FLayoutExtent: Integer;
    FLayoutDivider: Integer;
    FEffectivePosition: Integer;
    function CalculateGeometry(AExtent, ADivider: Integer): TNyxSplitGeometry;
    procedure UpdateEffective(const AGeometry: TNyxSplitGeometry);
    function SetInteractivePosition(APercent: Integer): Boolean;
  public
    constructor Create(ANode: TNyxNode);
    { Copy the two children's explicit minimum along this split's axis. The
      adapter calls after projected configuration changes; no node is retained.
      Negative minima refuse before either copied minimum changes. }
    procedure ConfigurePanes(ANode: TNyxNode);
    function SetPosition(APercent: Integer): Boolean;
    { Remember only geometry for the next gesture. Passive sizing never writes
      Position, so enlarging the host restores its requested proportion. }
    function Geometry(AExtent: Integer; ADivider: Integer = 28): TNyxSplitGeometry;
    procedure BeginDrag(ACoordinate: Double; AExtent: Integer);
    function Drag(ACoordinate: Double): Boolean;
    procedure EndDrag(ACancel: Boolean);
    function Key(AKey: TNyxKey; AShift: Boolean = False): Boolean;
    property Orientation: TNyxSplitOrientation read FOrientation;
    property Position: Integer read FPosition;
    { Physical first-pane percentage after minimum-size constraints. Useful for
      separator accessibility; Position remains the user's requested value. }
    property EffectivePosition: Integer read FEffectivePosition;
    property Minimum: Integer read FMinimum;
    property Maximum: Integer read FMaximum;
    property Resizable: Boolean read FResizable;
    property Dragging: Boolean read FDragging;
  end;

{ A completed resize is a normal typed change callback with an integer percent
  snapshot. The caller mutates only its realized view, then emits this event.
  Application callbacks may retain the snapshot or dispose/navigate the view. }
function NyxSplitChange(ANode: TNyxNode; APercent: Integer): TNyxDispatch;

implementation

constructor TNyxSplitState.Create(ANode: TNyxNode);
begin
  inherited Create;

  if (ANode = nil) or (ANode.ProjectionKind <> 'split-view') then
  begin
    raise ENyxModel.Create('Split sizing requires a split-view descriptor');
  end;
  FOrientation := nsoStacked;

  if ANode.Prop('split-orientation', 'stacked') = 'side-by-side' then
  begin
    FOrientation := nsoSideBySide;
  end;
  FMinimum := StrToIntDef(ANode.Prop('split-minimum'), 15);
  FMaximum := StrToIntDef(ANode.Prop('split-maximum'), 85);
  FPosition := StrToIntDef(ANode.Prop('split-position'), 65);
  FResizable := ANode.Prop('split-resizable', 'true') <> 'false';

  if (FMinimum < 0) or (FMaximum > 100) or (FMinimum > FMaximum) or
    (FPosition < FMinimum) or (FPosition > FMaximum) then
  begin
    raise ENyxModel.Create('Split sizing has invalid percentage bounds');
  end;
  FEffectivePosition := FPosition;
  ConfigurePanes(ANode);
end;

procedure TNyxSplitState.ConfigurePanes(ANode: TNyxNode);
var
  LKey: TNyxText;
  LFirst: Integer;
  LSecond: Integer;
begin
  LKey := 'min-height';

  if FOrientation = nsoSideBySide then
  begin
    LKey := 'min-width';
  end;
  LFirst := 0;
  LSecond := 0;

  if (ANode <> nil) and (ANode.Count > 0) then
  begin
    LFirst := StrToIntDef(ANode.Children[0].Prop(LKey), 0);
  end;

  if (ANode <> nil) and (ANode.Count > 1) then
  begin
    LSecond := StrToIntDef(ANode.Children[1].Prop(LKey), 0);
  end;

  if (LFirst < 0) or (LSecond < 0) then
  begin
    raise ENyxModel.Create('Split child minimum sizes must be nonnegative');
  end;
  FFirstMinimum := LFirst;
  FSecondMinimum := LSecond;
end;

function TNyxSplitState.SetPosition(APercent: Integer): Boolean;
var
  LPosition: Integer;
begin
  LPosition := EnsureRange(APercent, FMinimum, FMaximum);
  Result := LPosition <> FPosition;
  FPosition := LPosition;
  UpdateEffective(CalculateGeometry(FLayoutExtent, FLayoutDivider));
end;

function TNyxSplitState.CalculateGeometry(AExtent, ADivider: Integer): TNyxSplitGeometry;
var
  LExtent: Integer;
  LMinimumTotal: Double;
begin
  LExtent := Max(0, AExtent);
  Result.DividerExtent := Min(LExtent, Max(0, ADivider));
  Dec(LExtent, Result.DividerExtent);
  Result.FirstExtent := Round(Double(LExtent) * FPosition / 100);
  { Use floating-point addition before summing potentially large caller minima;
    native Integer and the browser's safe-number domain then agree without an
    overflowing intermediate. No impossible minimum can grow the host. }
  LMinimumTotal := Double(FFirstMinimum) + FSecondMinimum;

  if LMinimumTotal > LExtent then
  begin
    Result.FirstExtent := Round(LExtent * (FFirstMinimum / LMinimumTotal));
  end
  else
  begin
    Result.FirstExtent := EnsureRange(Result.FirstExtent,
      FFirstMinimum, LExtent - FSecondMinimum);
  end;
  Result.SecondExtent := LExtent - Result.FirstExtent;
end;

function TNyxSplitState.Geometry(AExtent, ADivider: Integer): TNyxSplitGeometry;
begin
  Result := CalculateGeometry(AExtent, ADivider);
  FLayoutExtent := Max(0, AExtent);
  FLayoutDivider := Result.DividerExtent;
  UpdateEffective(Result);
end;

procedure TNyxSplitState.UpdateEffective(const AGeometry: TNyxSplitGeometry);
var
  LAvailable: Integer;
begin
  LAvailable := FLayoutExtent - FLayoutDivider;
  FEffectivePosition := FPosition;

  if (LAvailable > 0) and
    (AGeometry.FirstExtent <> Round(Double(LAvailable) * FPosition / 100)) then
  begin
    FEffectivePosition := Round(AGeometry.FirstExtent * 100.0 / LAvailable);
  end;
end;

function TNyxSplitState.SetInteractivePosition(APercent: Integer): Boolean;
var
  LBefore: TNyxSplitGeometry;
  LAfter: TNyxSplitGeometry;
  LPosition: Integer;
begin
  LPosition := FPosition;
  LBefore := CalculateGeometry(FLayoutExtent, FLayoutDivider);
  Result := SetPosition(APercent);

  if Result and (FLayoutExtent > FLayoutDivider) and
    ((FFirstMinimum > 0) or (FSecondMinimum > 0)) then
  begin
    LAfter := CalculateGeometry(FLayoutExtent, FLayoutDivider);

    if LAfter.FirstExtent = LBefore.FirstExtent then
    begin
      { Do not accumulate invisible movement against a constrained edge. A
        following inward arrow/drag must respond at the visible divider. }
      FPosition := LPosition;
      UpdateEffective(LBefore);
      Result := False;
    end;
  end;
end;

procedure TNyxSplitState.BeginDrag(ACoordinate: Double; AExtent: Integer);
begin
  FDragging := FResizable and (AExtent > 0);

  if FDragging then
  begin
    FStartPosition := FPosition;
    FStartDragPosition := FEffectivePosition;
    FStartCoordinate := ACoordinate;
    FStartExtent := AExtent;
  end;
end;

function TNyxSplitState.Drag(ACoordinate: Double): Boolean;
begin
  Result := False;

  if FDragging then
  begin

    if (ACoordinate = FStartCoordinate) and (FPosition = FStartPosition) then
    begin
      Exit;
    end;
    Result := SetInteractivePosition(FStartDragPosition +
      Round((ACoordinate - FStartCoordinate) * 100 / FStartExtent));
  end;
end;

procedure TNyxSplitState.EndDrag(ACancel: Boolean);
begin

  if FDragging and ACancel then
  begin
    SetPosition(FStartPosition);
  end;
  FDragging := False;
end;

function TNyxSplitState.Key(AKey: TNyxKey; AShift: Boolean): Boolean;
var
  LStep: Integer;
begin
  Result := False;

  if not FResizable then
  begin
    Exit;
  end;
  LStep := 1;

  if AShift then
  begin
    LStep := 10;
  end;
  case AKey of
    nkHomeKey: Result := SetInteractivePosition(FMinimum);
    nkEndKey: Result := SetInteractivePosition(FMaximum);
    nkUpKey, nkDownKey:
      begin

        if FOrientation = nsoStacked then
        begin

          if AKey = nkUpKey then
          begin
            LStep := -LStep;
          end;
          Result := SetInteractivePosition(FEffectivePosition + LStep);
        end;
      end;
    nkLeftKey, nkRightKey:
      begin

        if FOrientation = nsoSideBySide then
        begin

          if AKey = nkLeftKey then
          begin
            LStep := -LStep;
          end;
          Result := SetInteractivePosition(FEffectivePosition + LStep);
        end;
      end;
    else
      begin
        { Other keys leave the split untouched and remain available to the host. }
      end;
  end;
end;

function NyxSplitChange(ANode: TNyxNode; APercent: Integer): TNyxDispatch;
begin
  Result := DispatchNyxBehavior(ANode, ntChange);
  Result.Info.Value := NyxData(APercent);
  Result.Info.ValueKind := nskInteger;
  Result.Info.HasValue := True;
  Result.Info.ValueID := ANode.ID;
  Result.Info.Changed := True;
end;

end.
