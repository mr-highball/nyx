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
  SysUtils, Math, nyx.types, nyx.model, nyx.behavior, nyx.data, nyx.state;

type
  { Shared physical geometry. Extents are nonnegative, consume the available
    axis exactly and retain the requested proportion even in tiny hosts. }
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
    FStartCoordinate: Double;
    FStartExtent: Integer;
  public
    constructor Create(ANode: TNyxNode);
    function SetPosition(APercent: Integer): Boolean;
    function Geometry(AExtent: Integer; ADivider: Integer = 28): TNyxSplitGeometry;
    procedure BeginDrag(ACoordinate: Double; AExtent: Integer);
    function Drag(ACoordinate: Double): Boolean;
    procedure EndDrag(ACancel: Boolean);
    function Key(AKey: TNyxKey; AShift: Boolean = False): Boolean;
    property Orientation: TNyxSplitOrientation read FOrientation;
    property Position: Integer read FPosition;
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
end;

function TNyxSplitState.SetPosition(APercent: Integer): Boolean;
var
  LPosition: Integer;
begin
  LPosition := EnsureRange(APercent, FMinimum, FMaximum);
  Result := LPosition <> FPosition;
  FPosition := LPosition;
end;

function TNyxSplitState.Geometry(AExtent: Integer; ADivider: Integer): TNyxSplitGeometry;
var
  LExtent: Integer;
begin
  LExtent := Max(0, AExtent);
  Result.DividerExtent := Min(LExtent, Max(0, ADivider));
  Dec(LExtent, Result.DividerExtent);
  Result.FirstExtent := Round(LExtent * FPosition / 100);
  Result.SecondExtent := LExtent - Result.FirstExtent;
end;

procedure TNyxSplitState.BeginDrag(ACoordinate: Double; AExtent: Integer);
begin
  FDragging := FResizable and (AExtent > 0);

  if FDragging then
  begin
    FStartPosition := FPosition;
    FStartCoordinate := ACoordinate;
    FStartExtent := AExtent;
  end;
end;

function TNyxSplitState.Drag(ACoordinate: Double): Boolean;
begin
  Result := False;

  if FDragging then
  begin
    Result := SetPosition(FStartPosition +
      Round((ACoordinate - FStartCoordinate) * 100 / FStartExtent));
  end;
end;

procedure TNyxSplitState.EndDrag(ACancel: Boolean);
begin

  if FDragging and ACancel then
  begin
    FPosition := FStartPosition;
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
    nkHomeKey: Result := SetPosition(FMinimum);
    nkEndKey: Result := SetPosition(FMaximum);
    nkUpKey, nkDownKey:
      begin

        if FOrientation = nsoStacked then
        begin

          if AKey = nkUpKey then
          begin
            LStep := -LStep;
          end;
          Result := SetPosition(FPosition + LStep);
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
          Result := SetPosition(FPosition + LStep);
        end;
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
