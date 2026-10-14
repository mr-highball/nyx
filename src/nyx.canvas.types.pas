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
unit nyx.canvas.types;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.canvas.geometry;

const
  { The node field is recognized only in design version 13. Earlier designs
    retain this spelling as opaque extension data until deliberate promotion. }
  NyxCanvasNodeWireField = 'canvasScene';
  MaximumNyxCanvasItems = 10000;
  MaximumNyxCanvasLayers = 256;
  MaximumNyxCanvasQueryItems = 256;

type
  ENyxCanvasScene = class(Exception);

  { Exact, case-sensitive open names. Item and layer identities are distinct
    Pascal types, never control IDs or renderer addresses. Names contain 1..128
    Unicode scalars without controls and at least one nonspace scalar. Spaces
    inside names are preserved; a default reference is absent. References
    do not retain a scene. Reusing an explicitly removed name is caller intent. }
  TNyxCanvasItemRef = record
  private
    FName: TNyxText;
    function GetName: TNyxText;
    function GetDefined: Boolean;
  public
    property Name: TNyxText read GetName;
    property Defined: Boolean read GetDefined;
  end;

  TNyxCanvasLayerRef = record
  private
    FName: TNyxText;
    function GetName: TNyxText;
    function GetDefined: Boolean;
  public
    property Name: TNyxText read GetName;
    property Defined: Boolean read GetDefined;
  end;

  { Immutable placement: world bounds, membership and independent display/hit
    flags. Hidden items are excluded from visible/interactive queries; an item
    can remain visible while refusing picking. Closed edges admit point/line
    content. These bounds are broad-phase geometry, not a shape's precise hit
    test, affine transform or an embedded widget scaling policy. }
  TNyxCanvasPlacement = record
  private
    FBounds: TNyxCanvasBounds;
    FLayer: TNyxCanvasLayerRef;
    FVisible: Boolean;
    FPickable: Boolean;
  public
    function AtBounds(const ABounds: TNyxCanvasBounds): TNyxCanvasPlacement;
    function OnLayer(const ALayer: TNyxCanvasLayerRef): TNyxCanvasPlacement;
    function Visible(AValue: Boolean): TNyxCanvasPlacement;
    function Pickable(AValue: Boolean): TNyxCanvasPlacement;
    property Bounds: TNyxCanvasBounds read FBounds;
    property Layer: TNyxCanvasLayerRef read FLayer;
    property IsVisible: Boolean read FVisible;
    property IsPickable: Boolean read FPickable;
  end;

  { Metadata owns no content, control, scene or adapter. Layer flags are already
    folded into the placement in a snapshot. PaintOrder is back-to-front, zero
    based, and stable for that snapshot. Exact references survive reordering. }
  TNyxCanvasItemInfo = record
    Item: TNyxCanvasItemRef;
    Placement: TNyxCanvasPlacement;
    PaintOrder: Integer;
  end;

  { Detached layer metadata includes empty layers so an editor can discover and
    configure them without inferring membership from a full item snapshot. }
  TNyxCanvasLayerInfo = record
    Layer: TNyxCanvasLayerRef;
    IsVisible: Boolean;
    IsPickable: Boolean;
    PaintOrder: Integer;
  end;

  { Closed allocation policy shared by saved definitions and both adapters. }
  TNyxCanvasControlScaling = (nccsReflow, nccsLogicalSize);

  { A saved canvas identity is distinct from an item, layer or UI control. }
  TNyxCanvasRef = record
  private
    FName: TNyxText;
    function GetName: TNyxText;
    function GetDefined: Boolean;
  public
    property Name: TNyxText read GetName;
    property Defined: Boolean read GetDefined;
  end;


function NyxCanvasItem(const AName: TNyxText): TNyxCanvasItemRef;
function NyxCanvasLayer(const AName: TNyxText): TNyxCanvasLayerRef;
function NyxCanvasRef(const AName: TNyxText): TNyxCanvasRef;
function NyxCanvasPlacement(const ALayer: TNyxCanvasLayerRef;
  const ABounds: TNyxCanvasBounds): TNyxCanvasPlacement;

implementation

procedure RequireName(const AName: TNyxText);
var
  LIndex: Integer;
  LScalar: Integer;
  LCount: Integer;
  LHasNonSpace: Boolean;
begin
  LIndex := 1;
  LCount := 0;
  LHasNonSpace := False;
  while LIndex <= Length(AName) do
  begin

    if not NyxNextScalar(AName, LIndex, LScalar) or (LScalar < 32) or
      ((LScalar >= 127) and (LScalar <= 159)) then
    begin
      raise ENyxCanvasScene.Create('Canvas names require valid Unicode without controls');
    end;
    Inc(LCount);
    LHasNonSpace := LHasNonSpace or (LScalar <> 32);
  end;

  if (LCount = 0) or (LCount > 128) or not LHasNonSpace then
  begin
    raise ENyxCanvasScene.Create('Canvas names require 1..128 scalars and nonspace content');
  end;
end;

function NyxCanvasItem(const AName: TNyxText): TNyxCanvasItemRef;
begin
  RequireName(AName);
  Result.FName := AName;
end;

function NyxCanvasLayer(const AName: TNyxText): TNyxCanvasLayerRef;
begin
  RequireName(AName);
  Result.FName := AName;
end;

function TNyxCanvasItemRef.GetName: TNyxText;
begin

  if not Defined then
  begin
    raise ENyxCanvasScene.Create('Construct a canvas item reference first');
  end;
  Result := FName;
end;

function TNyxCanvasItemRef.GetDefined: Boolean;
begin
  Result := FName <> '';
end;

function TNyxCanvasLayerRef.GetName: TNyxText;
begin

  if not Defined then
  begin
    raise ENyxCanvasScene.Create('Construct a canvas layer reference first');
  end;
  Result := FName;
end;

function TNyxCanvasLayerRef.GetDefined: Boolean;
begin
  Result := FName <> '';
end;

procedure RequirePlacement(const APlacement: TNyxCanvasPlacement);
begin
  APlacement.Layer.GetName;

  if not APlacement.Bounds.Defined then
  begin
    raise ENyxCanvasScene.Create('Canvas placement requires admitted world bounds');
  end;
end;

function NyxCanvasPlacement(const ALayer: TNyxCanvasLayerRef;
  const ABounds: TNyxCanvasBounds): TNyxCanvasPlacement;
begin
  Result.FLayer := ALayer;
  Result.FBounds := ABounds;
  Result.FVisible := True;
  Result.FPickable := True;
  RequirePlacement(Result);
end;

function TNyxCanvasPlacement.AtBounds(const ABounds: TNyxCanvasBounds): TNyxCanvasPlacement;
begin
  RequirePlacement(Self);
  Result := NyxCanvasPlacement(FLayer, ABounds).Visible(FVisible).Pickable(FPickable);
end;

function TNyxCanvasPlacement.OnLayer(const ALayer: TNyxCanvasLayerRef): TNyxCanvasPlacement;
begin
  RequirePlacement(Self);
  Result := NyxCanvasPlacement(ALayer, FBounds).Visible(FVisible).Pickable(FPickable);
end;

function TNyxCanvasPlacement.Visible(AValue: Boolean): TNyxCanvasPlacement;
begin
  RequirePlacement(Self);
  Result := Self;
  Result.FVisible := AValue;
end;

function TNyxCanvasPlacement.Pickable(AValue: Boolean): TNyxCanvasPlacement;
begin
  RequirePlacement(Self);
  Result := Self;
  Result.FPickable := AValue;
end;

function TNyxCanvasRef.GetName: TNyxText;
begin

  if not Defined then
  begin
    raise ENyxCanvasScene.Create('Construct a canvas definition reference first');
  end;
  Result := FName;
end;

function TNyxCanvasRef.GetDefined: Boolean;
begin
  Result := FName <> '';
end;


function NyxCanvasRef(const AName: TNyxText): TNyxCanvasRef;
begin
  RequireName(AName);
  Result.FName := AName;
end;

end.
