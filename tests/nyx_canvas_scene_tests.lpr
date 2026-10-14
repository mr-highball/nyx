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
program nyx_canvas_scene_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Math, nyx.text, nyx.types, nyx.model, nyx.controls,
  nyx.canvas.geometry, nyx.canvas.scene
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

type
  IFixtureMarker = interface(IInterface)
    ['{737A7921-4621-4C6F-8C01-0CA00000F001}']
    function Marker: Integer;
  end;

  { Distinguishable creator implementation proves that scene admission retains
    the actual interface object instead of reconstructing a generic descriptor. }
  TTrackedControl = class(TNyxControl, IFixtureMarker)
  public
    destructor Destroy; override;
    function Marker: Integer;
  end;

  {$ifndef PAS2JS}
  { Each real worker retains the same already-published immutable snapshot.
    Neither the worker nor its query result retains a scene/control. }
  TSnapshotWorker = class(TThread)
  private
    FSnapshot: INyxCanvasSnapshot;
  protected
    procedure Execute; override;
  public
    Failure: TNyxText;
    Passed: Boolean;
    constructor Create(const ASnapshot: INyxCanvasSnapshot);
  end;
  {$endif}

var
  GChecks: Integer;
  GDestroyed: Integer;
  GPrunedBounds: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Canvas scene: ' + AReason);
  end;
  Inc(GChecks);
end;

destructor TTrackedControl.Destroy;
begin
  Inc(GDestroyed);
  inherited Destroy;
end;

function TTrackedControl.Marker: Integer;
begin
  Result := 314;
end;

{$ifndef PAS2JS}
constructor TSnapshotWorker.Create(const ASnapshot: INyxCanvasSnapshot);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FSnapshot := ASnapshot;
end;

procedure TSnapshotWorker.Execute;
var
  LIndex: Integer;
  LQuery: INyxCanvasQuery;
begin
  try
    for LIndex := 1 to 200 do
    begin
      LQuery := FSnapshot.Query(NyxCanvasBounds(-3000, -3000, 1, 1), ncqmAll, 0, 1);

      if (LQuery.Count <> 1) or (LQuery.TotalMatches <> 1) or
        (LQuery.Items[0].Item.Name <> 'item-33') then
      begin
        raise Exception.Create('Concurrent snapshot result changed');
      end;
    end;
    Passed := True;
  except
    on LException: Exception do
    begin
      Failure := TNyxText(LException.Message);
    end;
  end;
end;
{$endif}

procedure Ownership;
var
  LScene: INyxCanvasScene;
  LOther: INyxCanvasScene;
  LLayer: TNyxCanvasLayerRef;
  LItem: TNyxCanvasItemRef;
  LPlacement: TNyxCanvasPlacement;
  LMemo: INyxMemo;
  LRetained: INyxNode;
  LSnapshot: INyxCanvasSnapshot;
  LRevision: Integer;
  LRefused: Boolean;

  procedure Prepare;
  var
    LCard: INyxCard;
    LButton: INyxButton;
  begin
    LScene := NewNyxCanvasScene;
    LOther := NewNyxCanvasScene;
    LScene.AddLayer(LLayer);
    LOther.AddLayer(LLayer);
    LCard := NewNyxCard('reply-card');
    LMemo := NewNyxMemo('reply-memo');
    LMemo.Text := 'Write a reply';
    LMemo.Value := 'Exact reply 🐾';
    LButton := NewNyxButton('send-button');
    LButton.Text := 'Post reply';
    LCard.Add(LMemo).Add(LButton);
    LScene.AddControl(LItem, LPlacement, LCard);
    LRetained := LScene.Control(LItem);
    Check(LRetained.Node = LCard.Node, 'embedded compound keeps exact descriptor');
    Check(Supports(LRetained, INyxCard), 'embedded compound keeps specialized interface');
    Check((LRetained.Node.Count = 2) and
      (LRetained.Node.Children[0] = LMemo.Node), 'compound owns real nested memo/button');
  end;

  procedure Transfer;
  begin
    LOther.AddControl(LItem, LPlacement, LRetained);
  end;

  procedure InjectCreator;
  var
    LTracked: INyxControl;
    LMarker: IFixtureMarker;
  begin
    LTracked := TTrackedControl.Create(nkLabel, 'creator-control', ncoDescriptor);
    LScene.AddControl(NyxCanvasItem('custom'), LPlacement, LTracked);
    Check(Supports(LScene.Control(NyxCanvasItem('custom')), IFixtureMarker, LMarker)
      and (LMarker.Marker = 314), 'creator implementation is retained without substitution');
  end;

begin
  LLayer := NyxCanvasLayer('content');
  LItem := NyxCanvasItem('reply-editor');
  LPlacement := NyxCanvasPlacement(LLayer, NyxCanvasBounds(-100, -80, 300, 180));
  { Fluent return temporaries live until their Pascal routine ends. Separate
    construction scopes make retirement assertions observe actual last owners. }
  Prepare;
  LMemo.Value := 'Edited after injection';
  Check(LRetained.Node.Children[0].Prop('value') =
    'Edited after injection', 'specialized edits reach the same retained control');
  LRevision := LOther.Revision;
  LRefused := False;
  try
    LOther.AddControl(LItem, LPlacement, LRetained);
  except
    on ENyxModel do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LOther.Count = 0) and (LOther.Revision = LRevision),
    'second owner refuses without scene or original ownership mutation');
  LSnapshot := LScene.Snapshot;
  LScene.Remove(LItem);
  Check((LRetained.Node.Parent = nil) and (LRetained.Node.Count = 2),
    'removed retained root survives independently with descendants');
  Transfer;
  LOther := nil;
  Check((LRetained.Node.Parent = nil) and (LMemo.Node.Parent = LRetained.Node),
    'scene retirement detaches retained root while preserving child ownership');
  LRetained := nil;
  Check((LMemo.Node.Parent = nil) and (LMemo.Value = 'Edited after injection'),
    'retained nested field survives complete root retirement');
  LMemo := nil;
  InjectCreator;
  LScene := nil;
  Check(GDestroyed = 1, 'scene releases unretained creator implementation exactly once');
  Check((LSnapshot.Count = 1) and (LSnapshot.Items[0].Item.Name = 'reply-editor') and
    (LSnapshot.Pick(NyxCanvasPoint(0, 0)).Name = 'reply-editor'),
    'geometry snapshot survives all scene/control owners');
end;

procedure OrderingAndFailure;
var
  LScene: INyxCanvasScene;
  LSnapshot: INyxCanvasSnapshot;
  LOriginal: INyxCanvasSnapshot;
  LQuery: INyxCanvasQuery;
  LBack: TNyxCanvasLayerRef;
  LFront: TNyxCanvasLayerRef;
  LPlacement: TNyxCanvasPlacement;
  LControl: INyxLabel;
  LItem: TNyxCanvasItemRef;
  LEmptyItem: TNyxCanvasItemRef;
  LEmptyLayer: TNyxCanvasLayerRef;
  LEmptyBounds: TNyxCanvasBounds;
  LEmptyPlacement: TNyxCanvasPlacement;
  LPoint: TNyxCanvasPoint;
  LInfo: TNyxCanvasItemInfo;
  LRevision: Integer;
  LCase: Integer;
  LRefused: Boolean;
  LUnicodeName: TNyxText;
begin
  LEmptyItem := Default(TNyxCanvasItemRef);
  LEmptyLayer := Default(TNyxCanvasLayerRef);
  LEmptyBounds := Default(TNyxCanvasBounds);
  LEmptyPlacement := Default(TNyxCanvasPlacement);
  Check(not LEmptyItem.Defined and not LEmptyLayer.Defined,
    'default typed identities remain absent');
  LItem := NyxCanvasItem('reply editor');
  Check(LItem.Name = 'reply editor', 'item names retain natural word breaks');
  LBack := NyxCanvasLayer('My tools');
  Check(LBack.Name = 'My tools', 'layer names retain natural word breaks');
  LUnicodeName := '';
  for LCase := 1 to 128 do
  begin
    LUnicodeName := LUnicodeName + TNyxText('🐾');
  end;
  LItem := NyxCanvasItem(LUnicodeName);
  Check(LItem.Name = LUnicodeName, 'supplementary identity limit counts scalars, not bytes');
  LRefused := False;
  try
    LItem := NyxCanvasItem(LUnicodeName + TNyxText('🐾'));
  except
    on ENyxCanvasScene do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'overlong scalar identity refuses');
  LScene := NewNyxCanvasScene;
  LBack := NyxCanvasLayer('background');
  LFront := NyxCanvasLayer('foreground');
  LScene.AddLayer(LBack).AddLayer(LFront);
  Check((LScene.LayerCount = 2) and (LScene.Layers[1].Layer.Name = 'foreground') and
    LScene.Layers[1].IsVisible, 'empty layers have bounded discoverable metadata');
  LPlacement := NyxCanvasPlacement(LBack, NyxCanvasBounds(-10, -10, 20, 20));
  LControl := NewNyxLabel('first');
  LScene.AddControl(NyxCanvasItem('A'), LPlacement, LControl);
  LControl := NewNyxLabel('second');
  LScene.AddControl(NyxCanvasItem('B'), LPlacement.OnLayer(LFront), LControl);
  LControl := NewNyxLabel('third');
  LScene.AddControl(NyxCanvasItem('C'), LPlacement, LControl);
  LOriginal := LScene.Snapshot;
  LPoint := NyxCanvasPoint(10, 10);
  Check(LOriginal.Pick(LPoint).Name = 'B', 'layer order beats later item insertion');
  LQuery := LOriginal.Query(LPlacement.Bounds, ncqmAll, 1, 1);
  Check((LQuery.TotalMatches = 3) and (LQuery.Count = 1) and
    (LQuery.Items[0].Item.Name = 'C'), 'bounded front-to-back pagination');
  LScene.OrderLayer(LBack, 1);
  Check((LScene.Layers[1].Layer.Name = 'background') and
    (LScene.Layers[1].PaintOrder = 1), 'layer metadata follows explicit reordering');
  LSnapshot := LScene.Snapshot;
  Check(LSnapshot.Pick(LPoint).Name = 'C', 'layer reordering changes frontmost item');
  LScene.OrderItem(NyxCanvasItem('A'), 1);
  LSnapshot := LScene.Snapshot;
  Check(LSnapshot.Pick(LPoint).Name = 'A', 'within-layer reordering changes picking');
  LScene.ConfigureLayer(LBack, True, False);
  LSnapshot := LScene.Snapshot;
  Check((LSnapshot.Pick(LPoint).Name = 'B') and
    (LSnapshot.Query(LPlacement.Bounds).TotalMatches = 3),
    'nonpickable layer remains visible');
  LScene.ConfigureLayer(LBack, False, True);
  LSnapshot := LScene.Snapshot;
  Check((LSnapshot.Query(LPlacement.Bounds).TotalMatches = 1) and
    (LSnapshot.Query(LPlacement.Bounds, ncqmAll).TotalMatches = 3),
    'hidden layer is queryable explicitly without becoming interactive');
  LScene.ConfigureLayer(LBack, True, True);
  LScene.Place(NyxCanvasItem('A'), LPlacement.Pickable(False));
  LSnapshot := LScene.Snapshot;
  Check(LSnapshot.Pick(LPoint).Name = 'C', 'item can opt out of picking');
  LScene.Place(NyxCanvasItem('C'), LPlacement.Visible(False));
  LSnapshot := LScene.Snapshot;
  Check(LSnapshot.Pick(LPoint).Name = 'B', 'hidden item is not pickable');
  LScene.Place(NyxCanvasItem('B'), LPlacement.OnLayer(LBack)
    .AtBounds(NyxCanvasBounds(100, 200, 0, 0)));
  LScene.RemoveLayer(LFront);
  LSnapshot := LScene.Snapshot;
  Check(LSnapshot.Pick(NyxCanvasPoint(100, 200)).Name = 'B', 'point bounds remain queryable');
  Check(not LSnapshot.Pick(NyxCanvasPoint(101, 200)).Defined, 'absent pick is typed absence');
  Check(LOriginal.Pick(LPoint).Name = 'B', 'retained snapshot does not change after edits');
  LInfo := LOriginal.Items[0];
  LInfo.Item := NyxCanvasItem('changed-copy');
  LInfo.Placement := LInfo.Placement.AtBounds(NyxCanvasBounds(999, 999, 1, 1));
  Check((LOriginal.Items[0].Item.Name = 'A') and
    (LOriginal.Items[0].Placement.Bounds.Left = -10), 'metadata return values are detached');

  { Existing scene plus unowned candidate establish atomic failure preservation;
    malformed names/default values never become content or hidden fallbacks. }
  LControl := NewNyxLabel('unowned');
  LRevision := LScene.Revision;
  for LCase := 0 to 21 do
  begin
    LRefused := False;
    try
      case LCase of
        0: LScene.AddControl(NyxCanvasItem('A'), LPlacement, LControl);
        1: LScene.AddControl(NyxCanvasItem('D'), LPlacement.OnLayer(LFront), LControl);
        2: LScene.AddControl(NyxCanvasItem('D'), LPlacement, nil);
        3: LScene.AddLayer(LBack);
        4: LScene.Place(NyxCanvasItem('missing'), LPlacement);
        5: LScene.Place(NyxCanvasItem('A'), LEmptyPlacement);
        6: LScene.RemoveLayer(LBack);
        7: LScene.OrderItem(NyxCanvasItem('A'), 3);
        8: LScene.OrderItem(NyxCanvasItem('A'), -1);
        9: LScene.OrderLayer(LBack, 1);
        10: LScene.ConfigureLayer(LFront, True, True);
        11: LScene.Remove(NyxCanvasItem('missing'));
        12: LScene.Control(LEmptyItem);
        13: LSnapshot.Query(LEmptyBounds);
        14: LSnapshot.Query(LPlacement.Bounds, ncqmAll, -1, 1);
        15: LSnapshot.Query(LPlacement.Bounds, ncqmAll, 0, 0);
        16: LSnapshot.Query(LPlacement.Bounds, ncqmAll, 0, 257);
        17: LItem := NyxCanvasItem('');
        18: LItem := NyxCanvasItem('invalid' + #10);
        19: LScene.AddLayer(LEmptyLayer);
        20: LPlacement.AtBounds(LEmptyBounds);
        21: LItem := NyxCanvasItem('   ');
      end;
    except
      on ENyxCanvasScene do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LScene.Revision = LRevision) and (LScene.Count = 3) and
      (LControl.Node.Parent = nil), 'atomic admission refusal ' + IntToStr(LCase));
  end;
  LQuery := LSnapshot.Query(LPlacement.Bounds, ncqmAll, High(Integer), 256);
  Check((LQuery.Count = 0) and (LQuery.TotalMatches = 2),
    'large offsets do not overflow or change total matches');
end;

procedure SpatialQueries;
const
  ItemCount = 1024;
var
  LScene: INyxCanvasScene;
  LSnapshot: INyxCanvasSnapshot;
  LQuery: INyxCanvasQuery;
  LLayer: TNyxCanvasLayerRef;
  LControl: INyxControl;
  LBounds: TNyxCanvasBounds;
  LExpected: array of Integer;
  LInfo: TNyxCanvasItemInfo;
  LCase: Integer;
  LIndex: Integer;
  LCount: Integer;
  LOffset: Integer;
  LLimit: Integer;
  LMode: TNyxCanvasQueryMode;
  {$ifndef PAS2JS}
  LWorkers: array[0..3] of TSnapshotWorker;
  {$endif}
begin
  LScene := NewNyxCanvasScene;
  LLayer := NyxCanvasLayer('workspace');
  LScene.AddLayer(LLayer);
  for LIndex := 0 to ItemCount - 1 do
  begin
    LControl := NewNyxBuiltinControl(nkLabel, 'content-' + IntToStr(LIndex), ncoDescriptor);
    LBounds := NyxCanvasBounds((LIndex mod 32) * 200 - 3200,
      (LIndex div 32) * 200 - 3200, 80, 60);
    LScene.AddControl(NyxCanvasItem('item-' + IntToStr(LIndex)),
      NyxCanvasPlacement(LLayer, LBounds).Visible(LIndex mod 7 <> 0)
        .Pickable(LIndex mod 5 <> 0), LControl);
  end;
  LControl := nil;
  LSnapshot := LScene.Snapshot;
  SetLength(LExpected, ItemCount);
  for LCase := 0 to 89 do
  begin
    LBounds := NyxCanvasBounds((LCase * 337 mod 6500) - 3400,
      (LCase * 619 mod 6500) - 3400, (LCase mod 11) * 91, (LCase mod 13) * 83);
    LMode := TNyxCanvasQueryMode(LCase mod 3);
    LOffset := LCase mod 4;
    LLimit := 1 + LCase mod 9;
    LQuery := LSnapshot.Query(LBounds, LMode, LOffset, LLimit);
    LCount := 0;
    { Independent brute-force oracle over immutable metadata. It checks both
      spatial matching and the exact final rank/pagination, not BVH internals. }
    for LIndex := ItemCount - 1 downto 0 do
    begin
      LInfo := LSnapshot.Items[LIndex];

      if not LInfo.Placement.Bounds.Intersects(LBounds) or
        ((LMode <> ncqmAll) and not LInfo.Placement.IsVisible) or
        ((LMode = ncqmInteractive) and not LInfo.Placement.IsPickable) then
      begin
        Continue;
      end;
      LExpected[LCount] := LIndex;
      Inc(LCount);
    end;
    Check(LQuery.TotalMatches = LCount, 'indexed region equals complete oracle ' + IntToStr(LCase));
    Check((LQuery.Count <= LLimit) and (LQuery.Count <= LCount), 'bounded result count');
    for LIndex := 0 to LQuery.Count - 1 do
    begin
      Check(LQuery.Items[LIndex].PaintOrder = LExpected[LIndex + LOffset],
        'exact front-to-back indexed pagination');
    end;
    Check(LQuery.Count = Min(LLimit, Max(0, LCount - LOffset)), 'no omitted page entries');
  end;
  LQuery := LSnapshot.Query(NyxCanvasBounds(-3000, -3000, 1, 1), ncqmAll, 0, 1);
  GPrunedBounds := LQuery.VisitedBounds;
  Check((LQuery.TotalMatches = 1) and (LQuery.VisitedBounds < 64),
    'small query prunes over one thousand off-screen bounds');
  LQuery := LSnapshot.Query(NyxCanvasBounds(10000, 10000, 1, 1));
  Check((LQuery.Count = 0) and (LQuery.VisitedBounds = 1), 'disjoint query prunes at root');
  LScene := nil;
  Check((LSnapshot.Count = ItemCount) and
    (LSnapshot.Query(NyxCanvasBounds(-3000, -3000, 1, 1), ncqmAll).TotalMatches = 1),
    'retired large scene leaves independent index usable');
  {$ifndef PAS2JS}
  for LIndex := 0 to High(LWorkers) do
  begin
    LWorkers[LIndex] := nil;
  end;
  try
    for LIndex := 0 to High(LWorkers) do
    begin
      LWorkers[LIndex] := TSnapshotWorker.Create(LSnapshot);
      LWorkers[LIndex].Start;
    end;
    for LIndex := 0 to High(LWorkers) do
    begin
      LWorkers[LIndex].WaitFor;
      Check(LWorkers[LIndex].Passed, 'real snapshot worker: ' + LWorkers[LIndex].Failure);
    end;
  finally
    for LIndex := 0 to High(LWorkers) do
    begin

      if LWorkers[LIndex] <> nil then
      begin
        LWorkers[LIndex].WaitFor;
        LWorkers[LIndex].Free;
      end;
    end;
  end;
  {$endif}
end;

procedure LimitsAndExtremes;
var
  LScene: INyxCanvasScene;
  LLayer: TNyxCanvasLayerRef;
  LPlacement: TNyxCanvasPlacement;
  LControl: INyxControl;
  LSnapshot: INyxCanvasSnapshot;
  LRevision: Integer;
  LIndex: Integer;
  LRefused: Boolean;
begin
  LScene := NewNyxCanvasScene;
  for LIndex := 0 to MaximumNyxCanvasLayers - 1 do
  begin
    LScene.AddLayer(NyxCanvasLayer('layer-' + IntToStr(LIndex)));
  end;
  LRevision := LScene.Revision;
  LRefused := False;
  try
    LScene.AddLayer(NyxCanvasLayer('excess-layer'));
  except
    on ENyxCanvasScene do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LScene.Revision = LRevision), 'layer capacity refuses atomically');
  LScene := NewNyxCanvasScene;
  LLayer := NyxCanvasLayer('world');
  LScene.AddLayer(LLayer);
  LPlacement := NyxCanvasPlacement(LLayer, NyxCanvasBounds(0, 0, 0, 0));
  for LIndex := 0 to MaximumNyxCanvasItems - 1 do
  begin
    LControl := NewNyxBuiltinControl(nkLabel, 'content-' + IntToStr(LIndex), ncoDescriptor);
    LScene.AddControl(NyxCanvasItem('item-' + IntToStr(LIndex)), LPlacement, LControl);
  end;
  LControl := NewNyxBuiltinControl(nkLabel, 'unowned', ncoDescriptor);
  LRevision := LScene.Revision;
  LRefused := False;
  try
    LScene.AddControl(NyxCanvasItem('excess-item'), LPlacement, LControl);
  except
    on ENyxCanvasScene do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LScene.Revision = LRevision) and (LControl.Node.Parent = nil),
    'item capacity refuses without stealing the candidate');
  LScene.Place(NyxCanvasItem('item-0'), LPlacement
    .AtBounds(NyxCanvasBounds(-MaximumNyxCanvasCoordinate, -MaximumNyxCanvasCoordinate, 0, 0)));
  LScene.Place(NyxCanvasItem('item-9999'), LPlacement
    .AtBounds(NyxCanvasBounds(MaximumNyxCanvasCoordinate, MaximumNyxCanvasCoordinate, 0, 0)));
  LSnapshot := LScene.Snapshot;
  Check(LSnapshot.Pick(NyxCanvasPoint(-MaximumNyxCanvasCoordinate,
    -MaximumNyxCanvasCoordinate)).Name = 'item-0', 'index union admits negative world extreme');
  Check(LSnapshot.Pick(NyxCanvasPoint(MaximumNyxCanvasCoordinate,
    MaximumNyxCanvasCoordinate)).Name = 'item-9999', 'index union admits positive world extreme');
  Check((LSnapshot.Query(LPlacement.Bounds, ncqmAll, 0, 256).Count = 256) and
    (LSnapshot.Query(LPlacement.Bounds, ncqmAll, 0, 256).TotalMatches = 9998),
    'coincident maximum-capacity scene remains balanced and bounded');
end;

begin
  try
    Ownership;
    OrderingAndFailure;
    SpatialQueries;
    LimitsAndExtremes;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-canvas-scene', 'passed');
    document.body.textContent := 'PASS ' + IntToStr(GChecks) +
      ' canvas scene checks; narrow query visits ' + IntToStr(GPrunedBounds) + ' bounds';
    {$else}
    WriteLn('PASS ', GChecks, ' canvas scene checks; narrow query visits ', GPrunedBounds, ' bounds');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-canvas-scene', 'failed');
      document.body.setAttribute('data-canvas-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
