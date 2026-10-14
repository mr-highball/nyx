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
unit nyx.canvas.scene;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.model, nyx.canvas.geometry;

const
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

  TNyxCanvasQueryMode = (ncqmVisible, ncqmInteractive, ncqmAll);

  { Immutable, bounded result ordered front-to-back. TotalMatches includes
    matches omitted by Offset/Limit. VisitedBounds is deterministic index work,
    not a latency/production-budget promise. Entries remain valid after scene
    mutation/disposal; no interface is stored inside a portable record. }
  INyxCanvasQuery = interface(IInterface)
    ['{737A7921-4621-4C6F-8C01-0CA000000001}']
    function GetCount: Integer;
    function GetItem(AIndex: Integer): TNyxCanvasItemInfo;
    function GetTotalMatches: Integer;
    function GetVisitedBounds: Integer;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TNyxCanvasItemInfo read GetItem;
    property TotalMatches: Integer read GetTotalMatches;
    property VisitedBounds: Integer read GetVisitedBounds;
  end;

  { Read-only spatial snapshot with a balanced bounding-volume index. It owns
    copied geometry/identities only; workers may query a published snapshot
    concurrently, using ordinary safe interface transfer. Query allocates local
    traversal/results. Capturing belongs to the scene's single authoring thread.
    A view retains a snapshot instead of a cycle back to its document/scene. }
  INyxCanvasSnapshot = interface(IInterface)
    ['{737A7921-4621-4C6F-8C01-0CA000000002}']
    function GetRevision: Integer;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): TNyxCanvasItemInfo;
    function Query(const AWorldBounds: TNyxCanvasBounds;
      AMode: TNyxCanvasQueryMode = ncqmVisible; AOffset: Integer = 0;
      ALimit: Integer = 64): INyxCanvasQuery;
    { Return the frontmost broad-phase interactive item; an absent reference
      means no match. Adapters apply precise shape/control hit tests afterward. }
    function Pick(const AWorldPoint: TNyxCanvasPoint): TNyxCanvasItemRef;
    property Revision: Integer read GetRevision;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TNyxCanvasItemInfo read GetItem;
  end;

  { A retained scene owns independent roots using the existing Nyx tree lifetime
    contract. AddControl adopts an unowned managed control tree and retains its
    actual implementation, including future/custom controls. It never clones or
    substitutes that implementation. The portable ownership anchor is internal;
    callers must not extract, reparent or Free the borrowed root while admitted.
    Editing normal control properties/children is allowed on the authoring thread.
    Such edits are outside the geometry revision; document/history integration
    must own their revision. No root/control keeps a scene interface backreference.

    All mutation is single-threaded. Every successful scene operation increments
    Revision once, including no-op configuration; failures preserve membership,
    placement, revision and ownership. Snapshot always captures fresh metadata.
    Layers paint in explicit order; insertion/reordering orders items within each
    layer. There is no automatic layer, target, renderer or runtime store.
    Persistence, control rendering and behavior graphs consume this foundation
    separately; this interface does not claim those integrations. }
  INyxCanvasScene = interface(IInterface)
    ['{737A7921-4621-4C6F-8C01-0CA000000003}']
    function GetRevision: Integer;
    function GetCount: Integer;
    function GetLayerCount: Integer;
    function GetLayer(AIndex: Integer): TNyxCanvasLayerInfo;
    function AddLayer(const ALayer: TNyxCanvasLayerRef): INyxCanvasScene;
    function ConfigureLayer(const ALayer: TNyxCanvasLayerRef;
      AVisible, APickable: Boolean): INyxCanvasScene;
    function OrderLayer(const ALayer: TNyxCanvasLayerRef;
      AIndex: Integer): INyxCanvasScene;
    procedure RemoveLayer(const ALayer: TNyxCanvasLayerRef);
    function AddControl(const AItem: TNyxCanvasItemRef;
      const APlacement: TNyxCanvasPlacement; const AControl: INyxNode): INyxCanvasScene;
    function Place(const AItem: TNyxCanvasItemRef;
      const APlacement: TNyxCanvasPlacement): INyxCanvasScene;
    { Reorder within the item's layer, using a zero-based final position. }
    function OrderItem(const AItem: TNyxCanvasItemRef;
      AIndex: Integer): INyxCanvasScene;
    function Control(const AItem: TNyxCanvasItemRef): INyxNode;
    { Removing releases this scene's ownership. Retained control interfaces
      survive with their own descendants and can be admitted into another scene. }
    procedure Remove(const AItem: TNyxCanvasItemRef);
    function Snapshot: INyxCanvasSnapshot;
    property Revision: Integer read GetRevision;
    property Count: Integer read GetCount;
    property LayerCount: Integer read GetLayerCount;
    property Layers[AIndex: Integer]: TNyxCanvasLayerInfo read GetLayer;
  end;

function NyxCanvasItem(const AName: TNyxText): TNyxCanvasItemRef;
function NyxCanvasLayer(const AName: TNyxText): TNyxCanvasLayerRef;
function NyxCanvasPlacement(const ALayer: TNyxCanvasLayerRef;
  const ABounds: TNyxCanvasBounds): TNyxCanvasPlacement;
function NewNyxCanvasScene: INyxCanvasScene;

implementation

uses
  Math, nyx.types;

type
  TIndexArray = array of Integer;
  TInfoArray = array of TNyxCanvasItemInfo;

  TCanvasQuery = class(TInterfacedObject, INyxCanvasQuery)
  private
    FItems: TInfoArray;
    FTotal: Integer;
    FVisited: Integer;
  public
    function GetCount: Integer;
    function GetItem(AIndex: Integer): TNyxCanvasItemInfo;
    function GetTotalMatches: Integer;
    function GetVisitedBounds: Integer;
  end;

  { Unions may span twice the public coordinate limit. Keep their finite edges
    internally instead of trying to admit that union as one content bounds. }
  TSpatialBounds = record
    Left: Double;
    Top: Double;
    Right: Double;
    Bottom: Double;
  end;

  TSpatialBranch = record
    Bounds: TSpatialBounds;
    First: Integer;
    Last: Integer;
    ItemIndex: Integer;
  end;

  TCanvasSnapshot = class(TInterfacedObject, INyxCanvasSnapshot)
  private
    FRevision: Integer;
    FItems: TInfoArray;
    FBranches: array of TSpatialBranch;
    FBranchCount: Integer;
    FRoot: Integer;
    function Build(var AIndices, ATemporary: TIndexArray;
      AFirst, ALast: Integer): Integer;
    procedure SortSpatial(var AIndices, ATemporary: TIndexArray;
      AFirst, ALast: Integer; AHorizontal: Boolean);
    procedure Collect(ABranch: Integer; const ABounds: TNyxCanvasBounds;
      AMode: TNyxCanvasQueryMode; var AMatches: TIndexArray;
      var ACount, AVisited: Integer);
  public
    constructor Create(ARevision: Integer; const AItems: TInfoArray);
    function GetRevision: Integer;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): TNyxCanvasItemInfo;
    function Query(const AWorldBounds: TNyxCanvasBounds;
      AMode: TNyxCanvasQueryMode; AOffset, ALimit: Integer): INyxCanvasQuery;
    function Pick(const AWorldPoint: TNyxCanvasPoint): TNyxCanvasItemRef;
  end;

  TSceneLayer = record
    Reference: TNyxCanvasLayerRef;
    Visible: Boolean;
    Pickable: Boolean;
  end;

  { Each item owns a small model anchor. Its child retention already preserves
    managed implementations and detaches survivors when released. Storing one
    class pointer per member avoids COM interfaces inside pas2js record arrays. }
  TSceneItem = class
  public
    Reference: TNyxCanvasItemRef;
    Placement: TNyxCanvasPlacement;
    Anchor: TNyxNode;
    destructor Destroy; override;
  end;

  TCanvasScene = class(TInterfacedObject, INyxCanvasScene)
  private
    FRevision: Integer;
    FLayers: array of TSceneLayer;
    FItems: array of TSceneItem;
    function FindLayer(const ALayer: TNyxCanvasLayerRef): Integer;
    function FindItem(const AItem: TNyxCanvasItemRef): Integer;
    procedure CheckRevision;
  public
    destructor Destroy; override;
    function GetRevision: Integer;
    function GetCount: Integer;
    function GetLayerCount: Integer;
    function GetLayer(AIndex: Integer): TNyxCanvasLayerInfo;
    function AddLayer(const ALayer: TNyxCanvasLayerRef): INyxCanvasScene;
    function ConfigureLayer(const ALayer: TNyxCanvasLayerRef;
      AVisible, APickable: Boolean): INyxCanvasScene;
    function OrderLayer(const ALayer: TNyxCanvasLayerRef;
      AIndex: Integer): INyxCanvasScene;
    procedure RemoveLayer(const ALayer: TNyxCanvasLayerRef);
    function AddControl(const AItem: TNyxCanvasItemRef;
      const APlacement: TNyxCanvasPlacement; const AControl: INyxNode): INyxCanvasScene;
    function Place(const AItem: TNyxCanvasItemRef;
      const APlacement: TNyxCanvasPlacement): INyxCanvasScene;
    function OrderItem(const AItem: TNyxCanvasItemRef;
      AIndex: Integer): INyxCanvasScene;
    function Control(const AItem: TNyxCanvasItemRef): INyxNode;
    procedure Remove(const AItem: TNyxCanvasItemRef);
    function Snapshot: INyxCanvasSnapshot;
  end;

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

function TCanvasQuery.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TCanvasQuery.GetItem(AIndex: Integer): TNyxCanvasItemInfo;
begin

  if (AIndex < 0) or (AIndex >= Length(FItems)) then
  begin
    raise ENyxCanvasScene.Create('Canvas query index is outside the result');
  end;
  Result := FItems[AIndex];
end;

function TCanvasQuery.GetTotalMatches: Integer;
begin
  Result := FTotal;
end;

function TCanvasQuery.GetVisitedBounds: Integer;
begin
  Result := FVisited;
end;

function SpatialBounds(const ABounds: TNyxCanvasBounds): TSpatialBounds;
begin
  Result.Left := ABounds.Left;
  Result.Top := ABounds.Top;
  Result.Right := ABounds.Left + ABounds.Width;
  Result.Bottom := ABounds.Top + ABounds.Height;
end;

function SpatialUnion(const AFirst, ASecond: TSpatialBounds): TSpatialBounds;
begin
  Result.Left := Min(AFirst.Left, ASecond.Left);
  Result.Top := Min(AFirst.Top, ASecond.Top);
  Result.Right := Max(AFirst.Right, ASecond.Right);
  Result.Bottom := Max(AFirst.Bottom, ASecond.Bottom);
end;

function SpatialIntersects(const AFirst: TSpatialBounds;
  const ASecond: TNyxCanvasBounds): Boolean;
begin
  Result := (AFirst.Left <= ASecond.Left + ASecond.Width) and
    (AFirst.Right >= ASecond.Left) and (AFirst.Top <= ASecond.Top + ASecond.Height) and
    (AFirst.Bottom >= ASecond.Top);
end;

constructor TCanvasSnapshot.Create(ARevision: Integer; const AItems: TInfoArray);
var
  LIndices: TIndexArray;
  LTemporary: TIndexArray;
  LIndex: Integer;
begin
  inherited Create;
  FRevision := ARevision;
  SetLength(FItems, Length(AItems));
  SetLength(LIndices, Length(AItems));
  SetLength(LTemporary, Length(AItems));
  for LIndex := 0 to High(AItems) do
  begin
    FItems[LIndex] := AItems[LIndex];
    LIndices[LIndex] := LIndex;
  end;
  FRoot := -1;

  if Length(FItems) > 0 then
  begin
    SetLength(FBranches, Length(FItems) * 2 - 1);
    FRoot := Build(LIndices, LTemporary, 0, High(LIndices));
  end;
end;

procedure TCanvasSnapshot.SortSpatial(var AIndices, ATemporary: TIndexArray;
  AFirst, ALast: Integer; AHorizontal: Boolean);
var
  LMiddle: Integer;
  LLeft: Integer;
  LRight: Integer;
  LIndex: Integer;

  function ComesFirst(AFirstIndex, ASecondIndex: Integer): Boolean;
  var
    LFirst: Double;
    LSecond: Double;
    LBounds: TNyxCanvasBounds;
  begin
    LBounds := FItems[AFirstIndex].Placement.Bounds;

    if AHorizontal then
    begin
      LFirst := LBounds.Left + LBounds.Width / 2;
    end
    else
    begin
      LFirst := LBounds.Top + LBounds.Height / 2;
    end;
    LBounds := FItems[ASecondIndex].Placement.Bounds;

    if AHorizontal then
    begin
      LSecond := LBounds.Left + LBounds.Width / 2;
    end
    else
    begin
      LSecond := LBounds.Top + LBounds.Height / 2;
    end;
    Result := (LFirst < LSecond) or
      ((LFirst = LSecond) and (AFirstIndex <= ASecondIndex));
  end;

begin

  if AFirst >= ALast then
  begin
    Exit;
  end;
  LMiddle := AFirst + (ALast - AFirst) div 2;
  SortSpatial(AIndices, ATemporary, AFirst, LMiddle, AHorizontal);
  SortSpatial(AIndices, ATemporary, LMiddle + 1, ALast, AHorizontal);
  LLeft := AFirst;
  LRight := LMiddle + 1;
  for LIndex := AFirst to ALast do
  begin

    if (LLeft <= LMiddle) and ((LRight > ALast) or
      ComesFirst(AIndices[LLeft], AIndices[LRight])) then
    begin
      ATemporary[LIndex] := AIndices[LLeft];
      Inc(LLeft);
    end
    else
    begin
      ATemporary[LIndex] := AIndices[LRight];
      Inc(LRight);
    end;
  end;
  for LIndex := AFirst to ALast do
  begin
    AIndices[LIndex] := ATemporary[LIndex];
  end;
end;

function TCanvasSnapshot.Build(var AIndices, ATemporary: TIndexArray;
  AFirst, ALast: Integer): Integer;
var
  LBounds: TSpatialBounds;
  LIndex: Integer;
  LMiddle: Integer;
  LFirstBranch: Integer;
  LLastBranch: Integer;
begin
  Result := FBranchCount;
  Inc(FBranchCount);
  LBounds := SpatialBounds(FItems[AIndices[AFirst]].Placement.Bounds);
  for LIndex := AFirst + 1 to ALast do
  begin
    LBounds := SpatialUnion(LBounds, SpatialBounds(FItems[AIndices[LIndex]].Placement.Bounds));
  end;
  FBranches[Result].Bounds := LBounds;
  FBranches[Result].ItemIndex := -1;

  if AFirst = ALast then
  begin
    FBranches[Result].ItemIndex := AIndices[AFirst];
    Exit;
  end;
  { Median partitions guarantee logarithmic tree depth even with identical
    bounds. Stable merge sorting avoids adversarial quicksort inputs; this first
    immutable build is O(n log² n), queries prune disjoint bounding branches. }
  SortSpatial(AIndices, ATemporary, AFirst, ALast,
    LBounds.Right - LBounds.Left >= LBounds.Bottom - LBounds.Top);
  LMiddle := AFirst + (ALast - AFirst) div 2;
  LFirstBranch := Build(AIndices, ATemporary, AFirst, LMiddle);
  LLastBranch := Build(AIndices, ATemporary, LMiddle + 1, ALast);
  FBranches[Result].First := LFirstBranch;
  FBranches[Result].Last := LLastBranch;
end;

procedure TCanvasSnapshot.Collect(ABranch: Integer; const ABounds: TNyxCanvasBounds;
  AMode: TNyxCanvasQueryMode; var AMatches: TIndexArray; var ACount, AVisited: Integer);
var
  LItem: Integer;
  LPlacement: TNyxCanvasPlacement;
begin

  if ABranch < 0 then
  begin
    Exit;
  end;
  Inc(AVisited);

  if not SpatialIntersects(FBranches[ABranch].Bounds, ABounds) then
  begin
    Exit;
  end;
  LItem := FBranches[ABranch].ItemIndex;

  if LItem >= 0 then
  begin
    LPlacement := FItems[LItem].Placement;

    if (AMode <> ncqmAll) and not LPlacement.IsVisible then
    begin
      Exit;
    end;

    if (AMode = ncqmInteractive) and not LPlacement.IsPickable then
    begin
      Exit;
    end;
    AMatches[ACount] := LItem;
    Inc(ACount);
    Exit;
  end;
  Collect(FBranches[ABranch].First, ABounds, AMode, AMatches, ACount, AVisited);
  Collect(FBranches[ABranch].Last, ABounds, AMode, AMatches, ACount, AVisited);
end;

function TCanvasSnapshot.GetRevision: Integer;
begin
  Result := FRevision;
end;

function TCanvasSnapshot.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TCanvasSnapshot.GetItem(AIndex: Integer): TNyxCanvasItemInfo;
begin

  if (AIndex < 0) or (AIndex >= Length(FItems)) then
  begin
    raise ENyxCanvasScene.Create('Canvas snapshot index is outside the scene');
  end;
  Result := FItems[AIndex];
end;

function TCanvasSnapshot.Query(const AWorldBounds: TNyxCanvasBounds;
  AMode: TNyxCanvasQueryMode; AOffset, ALimit: Integer): INyxCanvasQuery;
var
  LMatches: TIndexArray;
  LMatched: array of Boolean;
  LCount: Integer;
  LVisited: Integer;
  LIndex: Integer;
  LWritten: Integer;
  LQuery: TCanvasQuery;
begin

  if not AWorldBounds.Defined or (AOffset < 0) or
    (ALimit < 1) or (ALimit > MaximumNyxCanvasQueryItems) or
    not (AMode in [ncqmVisible, ncqmInteractive, ncqmAll]) then
  begin
    raise ENyxCanvasScene.Create('Canvas query requires bounds, a closed mode and bounded pagination');
  end;
  SetLength(LMatches, Length(FItems));
  LCount := 0;
  LVisited := 0;
  Collect(FRoot, AWorldBounds, AMode, LMatches, LCount, LVisited);
  SetLength(LMatched, Length(FItems));
  for LIndex := 0 to LCount - 1 do
  begin
    LMatched[LMatches[LIndex]] := True;
  end;
  LQuery := TCanvasQuery.Create;
  Result := LQuery;
  LQuery.FTotal := LCount;
  LQuery.FVisited := LVisited;
  SetLength(LQuery.FItems, Min(ALimit, Max(0, LCount - AOffset)));
  LWritten := 0;
  { Paint order is the immutable metadata order. A linear rank pass preserves
    front-to-back pagination without a second sort; geometry tests are indexed.
    Query still uses O(n) temporary storage/rank work, explicitly not O(k). }
  for LIndex := High(FItems) downto 0 do
  begin

    if not LMatched[LIndex] then
    begin
      Continue;
    end;

    if AOffset > 0 then
    begin
      Dec(AOffset);
      Continue;
    end;

    if LWritten = Length(LQuery.FItems) then
    begin
      Break;
    end;
    LQuery.FItems[LWritten] := FItems[LIndex];
    Inc(LWritten);
  end;
end;

function TCanvasSnapshot.Pick(const AWorldPoint: TNyxCanvasPoint): TNyxCanvasItemRef;
var
  LQuery: INyxCanvasQuery;
begin

  if not AWorldPoint.Defined then
  begin
    raise ENyxCanvasScene.Create('Canvas picking requires a world point');
  end;
  Result := Default(TNyxCanvasItemRef);
  LQuery := Query(NyxCanvasBounds(AWorldPoint.X, AWorldPoint.Y, 0, 0),
    ncqmInteractive, 0, 1);

  if LQuery.Count > 0 then
  begin
    Result := LQuery.Items[0].Item;
  end;
end;

destructor TSceneItem.Destroy;
begin
  Anchor.Free;
  inherited Destroy;
end;

destructor TCanvasScene.Destroy;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FItems) do
  begin
    FItems[LIndex].Free;
  end;
  inherited Destroy;
end;

function NewNyxCanvasScene: INyxCanvasScene;
begin
  Result := TCanvasScene.Create;
end;

function TCanvasScene.GetRevision: Integer;
begin
  Result := FRevision;
end;

function TCanvasScene.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TCanvasScene.GetLayerCount: Integer;
begin
  Result := Length(FLayers);
end;

function TCanvasScene.GetLayer(AIndex: Integer): TNyxCanvasLayerInfo;
begin

  if (AIndex < 0) or (AIndex >= Length(FLayers)) then
  begin
    raise ENyxCanvasScene.Create('Canvas layer index is outside the scene');
  end;
  Result.Layer := FLayers[AIndex].Reference;
  Result.IsVisible := FLayers[AIndex].Visible;
  Result.IsPickable := FLayers[AIndex].Pickable;
  Result.PaintOrder := AIndex;
end;

procedure TCanvasScene.CheckRevision;
begin

  if FRevision = High(Integer) then
  begin
    raise ENyxCanvasScene.Create('Canvas revision exhausted; create a new scene');
  end;
end;

function TCanvasScene.FindLayer(const ALayer: TNyxCanvasLayerRef): Integer;
var
  LName: TNyxText;
begin
  LName := ALayer.Name;
  for Result := 0 to High(FLayers) do
  begin

    if FLayers[Result].Reference.Name = LName then
    begin
      Exit;
    end;
  end;
  Result := -1;
end;

function TCanvasScene.FindItem(const AItem: TNyxCanvasItemRef): Integer;
var
  LName: TNyxText;
begin
  LName := AItem.Name;
  for Result := 0 to High(FItems) do
  begin

    if FItems[Result].Reference.Name = LName then
    begin
      Exit;
    end;
  end;
  Result := -1;
end;

function TCanvasScene.AddLayer(const ALayer: TNyxCanvasLayerRef): INyxCanvasScene;
var
  LLayers: array of TSceneLayer;
begin
  CheckRevision;

  if (FindLayer(ALayer) >= 0) or (Length(FLayers) = MaximumNyxCanvasLayers) then
  begin
    raise ENyxCanvasScene.Create('Canvas layer is duplicate or layer capacity is exhausted');
  end;
  LLayers := Copy(FLayers, 0, Length(FLayers));
  SetLength(LLayers, Length(LLayers) + 1);
  LLayers[High(LLayers)].Reference := ALayer;
  LLayers[High(LLayers)].Visible := True;
  LLayers[High(LLayers)].Pickable := True;
  FLayers := LLayers;
  Inc(FRevision);
  Result := Self;
end;

function TCanvasScene.ConfigureLayer(const ALayer: TNyxCanvasLayerRef;
  AVisible, APickable: Boolean): INyxCanvasScene;
var
  LIndex: Integer;
begin
  CheckRevision;
  LIndex := FindLayer(ALayer);

  if LIndex < 0 then
  begin
    raise ENyxCanvasScene.Create('Canvas layer does not exist');
  end;
  FLayers[LIndex].Visible := AVisible;
  FLayers[LIndex].Pickable := APickable;
  Inc(FRevision);
  Result := Self;
end;

function TCanvasScene.OrderLayer(const ALayer: TNyxCanvasLayerRef;
  AIndex: Integer): INyxCanvasScene;
var
  LFrom: Integer;
  LIndex: Integer;
  LLayer: TSceneLayer;
begin
  CheckRevision;
  LFrom := FindLayer(ALayer);

  if (LFrom < 0) or (AIndex < 0) or (AIndex >= Length(FLayers)) then
  begin
    raise ENyxCanvasScene.Create('Canvas layer order requires an existing layer and final index');
  end;
  LLayer := FLayers[LFrom];
  for LIndex := LFrom to AIndex - 1 do
  begin
    FLayers[LIndex] := FLayers[LIndex + 1];
  end;
  for LIndex := LFrom downto AIndex + 1 do
  begin
    FLayers[LIndex] := FLayers[LIndex - 1];
  end;
  FLayers[AIndex] := LLayer;
  Inc(FRevision);
  Result := Self;
end;

procedure TCanvasScene.RemoveLayer(const ALayer: TNyxCanvasLayerRef);
var
  LFrom: Integer;
  LIndex: Integer;
begin
  CheckRevision;
  LFrom := FindLayer(ALayer);

  if LFrom < 0 then
  begin
    raise ENyxCanvasScene.Create('Canvas layer does not exist');
  end;
  for LIndex := 0 to High(FItems) do
  begin

    if FItems[LIndex].Placement.Layer.Name = ALayer.Name then
    begin
      raise ENyxCanvasScene.Create('Move or remove canvas items before removing their layer');
    end;
  end;
  for LIndex := LFrom to High(FLayers) - 1 do
  begin
    FLayers[LIndex] := FLayers[LIndex + 1];
  end;
  SetLength(FLayers, Length(FLayers) - 1);
  Inc(FRevision);
end;

function TCanvasScene.AddControl(const AItem: TNyxCanvasItemRef;
  const APlacement: TNyxCanvasPlacement; const AControl: INyxNode): INyxCanvasScene;
var
  LItem: TSceneItem;
  LItems: array of TSceneItem;
begin
  CheckRevision;
  RequirePlacement(APlacement);

  if (FindItem(AItem) >= 0) or (Length(FItems) = MaximumNyxCanvasItems) or
    (FindLayer(APlacement.Layer) < 0) or (AControl = nil) then
  begin
    raise ENyxCanvasScene.Create('Canvas admission requires a unique item, existing layer and control');
  end;
  LItems := Copy(FItems, 0, Length(FItems));
  SetLength(LItems, Length(LItems) + 1);
  LItem := TSceneItem.Create;
  try
    LItem.Reference := AItem;
    LItem.Placement := APlacement;
    LItem.Anchor := TNyxNode.Create(nkPanel, 'canvas-ownership');
    { Existing admission checks reject roots owned by a document/another item,
      realized trees and cycles before changing their ownership. Allocation of
      the scene vector has already succeeded, so publication cannot strand it. }
    LItem.Anchor.Add(AControl);
  except
    LItem.Free;
    raise;
  end;
  LItems[High(LItems)] := LItem;
  FItems := LItems;
  Inc(FRevision);
  Result := Self;
end;

function TCanvasScene.Place(const AItem: TNyxCanvasItemRef;
  const APlacement: TNyxCanvasPlacement): INyxCanvasScene;
var
  LIndex: Integer;
begin
  CheckRevision;
  RequirePlacement(APlacement);
  LIndex := FindItem(AItem);

  if (LIndex < 0) or (FindLayer(APlacement.Layer) < 0) then
  begin
    raise ENyxCanvasScene.Create('Canvas placement requires an existing item and layer');
  end;
  FItems[LIndex].Placement := APlacement;
  Inc(FRevision);
  Result := Self;
end;

function TCanvasScene.OrderItem(const AItem: TNyxCanvasItemRef;
  AIndex: Integer): INyxCanvasScene;
var
  LFrom: Integer;
  LIndex: Integer;
  LCount: Integer;
  LPositions: TIndexArray;
  LItem: TSceneItem;
begin
  CheckRevision;
  LFrom := FindItem(AItem);

  if LFrom < 0 then
  begin
    raise ENyxCanvasScene.Create('Canvas item does not exist');
  end;
  SetLength(LPositions, Length(FItems));
  LCount := 0;
  LItem := FItems[LFrom];
  for LIndex := 0 to High(FItems) do
  begin

    if FItems[LIndex].Placement.Layer.Name = LItem.Placement.Layer.Name then
    begin
      LPositions[LCount] := LIndex;

      if FItems[LIndex] = LItem then
      begin
        LFrom := LCount;
      end;
      Inc(LCount);
    end;
  end;

  if (AIndex < 0) or (AIndex >= LCount) then
  begin
    raise ENyxCanvasScene.Create('Canvas item order is outside its layer');
  end;
  for LIndex := LFrom to AIndex - 1 do
  begin
    FItems[LPositions[LIndex]] := FItems[LPositions[LIndex + 1]];
  end;
  for LIndex := LFrom downto AIndex + 1 do
  begin
    FItems[LPositions[LIndex]] := FItems[LPositions[LIndex - 1]];
  end;
  FItems[LPositions[AIndex]] := LItem;
  Inc(FRevision);
  Result := Self;
end;

function TCanvasScene.Control(const AItem: TNyxCanvasItemRef): INyxNode;
var
  LIndex: Integer;
begin
  LIndex := FindItem(AItem);

  if LIndex < 0 then
  begin
    raise ENyxCanvasScene.Create('Canvas item does not exist');
  end;
  Result := FItems[LIndex].Anchor.Children[0].ComponentReference;
end;

procedure TCanvasScene.Remove(const AItem: TNyxCanvasItemRef);
var
  LFrom: Integer;
  LIndex: Integer;
  LItem: TSceneItem;
begin
  CheckRevision;
  LFrom := FindItem(AItem);

  if LFrom < 0 then
  begin
    raise ENyxCanvasScene.Create('Canvas item does not exist');
  end;
  LItem := FItems[LFrom];
  for LIndex := LFrom to High(FItems) - 1 do
  begin
    FItems[LIndex] := FItems[LIndex + 1];
  end;
  SetLength(FItems, Length(FItems) - 1);
  Inc(FRevision);
  LItem.Free;
end;

function TCanvasScene.Snapshot: INyxCanvasSnapshot;
var
  LItems: TInfoArray;
  LLayer: Integer;
  LItem: Integer;
  LCount: Integer;
begin
  SetLength(LItems, Length(FItems));
  LCount := 0;
  for LLayer := 0 to High(FLayers) do
  begin
    for LItem := 0 to High(FItems) do
    begin

      if FItems[LItem].Placement.Layer.Name <> FLayers[LLayer].Reference.Name then
      begin
        Continue;
      end;
      LItems[LCount].Item := FItems[LItem].Reference;
      LItems[LCount].Placement := FItems[LItem].Placement
        .Visible(FItems[LItem].Placement.IsVisible and FLayers[LLayer].Visible)
        .Pickable(FItems[LItem].Placement.IsPickable and FLayers[LLayer].Pickable);
      LItems[LCount].PaintOrder := LCount;
      Inc(LCount);
    end;
  end;
  Result := TCanvasSnapshot.Create(FRevision, LItems);
end;

end.
