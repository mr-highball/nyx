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
{ Prepared membership extends the existing scene/layout owner. Candidate item
  metadata is independent; explicit content leases preserve retained roots. }

type
  { Immutable captured intent owns a supplied managed implementation through its
    interface, but adopts no tree. Values have no scene/view backreference. }
  TContentChange = class(TInterfacedObject, INyxCanvasContentChange)
  public
    ValueKind: TNyxCanvasContentChangeKind;
    ValueItem: TNyxCanvasItemRef;
    ValuePlacement: TNyxCanvasPlacement;
    ValueControl: INyxNode;
    function GetKind: TNyxCanvasContentChangeKind;
    function GetItem: TNyxCanvasItemRef;
    function GetPlacement: TNyxCanvasPlacement;
    function GetControl: INyxNode;
  end;

  { One cached foreign input. Capturing all getters precedes owner reservation;
    Root borrows the exact implementation held by Control. No interface lives
    inside a portable record, and no caller getters run during final install. }
  TContentInput = class
  public
    Kind: TNyxCanvasContentChangeKind;
    Item: TNyxCanvasItemRef;
    Placement: TNyxCanvasPlacement;
    Control: INyxNode;
    Root: TNyxNode;
  end;

  { Prepared membership owns independent candidate/previous item metadata and a
    retained scene lease. Staging reserves that scene before adopting new roots.
    Unchanged roots share content leases and lifetime identity. Install swaps
    admitted vectors only; Retire releases old or refused anchors afterward.
    Staging lookup is revoked at retirement; spatial Preview remains detached. }
  TContentPlan = class(TInterfacedObject, INyxCanvasLayoutPlan,
    INyxCanvasContentPlan, INyxPreparedPublication, INyxPublicationReadiness)
  public
    Owner: INyxCanvasScene;
    Scene: TCanvasScene;
    Items: TSceneItems;
    Former: TSceneItems;
    Snapshot: INyxCanvasSnapshot;
    Names: TNyxTextIndex;
    Revision: Integer;
    WasCommitted: Boolean;
    Validated: Boolean;
    Retired: Boolean;
    destructor Destroy; override;
    function GetPreview: INyxCanvasSnapshot;
    function GetExpectedRevision: Integer;
    function GetCommitted: Boolean;
    function BelongsTo(const AScene: INyxCanvasScene): Boolean;
    function PreviewControl(const AItem: TNyxCanvasItemRef): INyxNode;
    procedure Commit;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

function TContentChange.GetKind: TNyxCanvasContentChangeKind;
begin
  Result := ValueKind;
end;

function TContentChange.GetItem: TNyxCanvasItemRef;
begin
  Result := ValueItem;
end;

function TContentChange.GetPlacement: TNyxCanvasPlacement;
begin
  Result := ValuePlacement;
end;

function TContentChange.GetControl: INyxNode;
begin
  Result := ValueControl;
end;

function ContentChange(AKind: TNyxCanvasContentChangeKind;
  const AItem: TNyxCanvasItemRef; const APlacement: TNyxCanvasPlacement;
  const AControl: INyxNode): INyxCanvasContentChange;
var
  LChange: TContentChange;
begin
  NyxCanvasItem(AItem.Name);

  if AKind <> ncccRemove then
  begin
    RequirePlacement(APlacement);
  end;

  if (AKind in [ncccAdd, ncccReplace]) and (AControl = nil) then
  begin
    raise ENyxCanvasScene.Create('New content requires its managed control');
  end;
  LChange := TContentChange.Create;
  Result := LChange;
  LChange.ValueKind := AKind;
  LChange.ValueItem := AItem;
  LChange.ValuePlacement := APlacement;
  LChange.ValueControl := AControl;
end;

function NyxCanvasAddControl(const AItem: TNyxCanvasItemRef;
  const APlacement: TNyxCanvasPlacement; const AControl: INyxNode): INyxCanvasContentChange;
begin
  Result := ContentChange(ncccAdd, AItem, APlacement, AControl);
end;

function NyxCanvasReplaceControl(const AItem: TNyxCanvasItemRef;
  const APlacement: TNyxCanvasPlacement; const AControl: INyxNode): INyxCanvasContentChange;
begin
  Result := ContentChange(ncccReplace, AItem, APlacement, AControl);
end;

function NyxCanvasRemoveControl(const AItem: TNyxCanvasItemRef): INyxCanvasContentChange;
begin
  Result := ContentChange(ncccRemove, AItem, Default(TNyxCanvasPlacement), nil);
end;

function NyxCanvasPlaceControl(const AItem: TNyxCanvasItemRef;
  const APlacement: TNyxCanvasPlacement): INyxCanvasContentChange;
begin
  Result := ContentChange(ncccPlace, AItem, APlacement, nil);
end;

{ Only roots introduced by this candidate are adopted. Existing metadata shares
  an explicit anchor lease, so preparation neither reparents nor clones controls. }
function CopyContentItem(AItem: TSceneItem): TSceneItem;
begin
  Result := TSceneItem.Create;
  Result.Reference := AItem.Reference;
  Result.Placement := AItem.Placement;
  Result.Anchor := AItem.Anchor;
  Result.ContentLease := AItem.ContentLease;
  Result.Identity := AItem.Identity;
end;

function CreateContentItem(AInput: TContentInput): TSceneItem;
var
  LContent: TSceneContent;
begin
  Result := TSceneItem.Create;
  try
    Result.Reference := AInput.Item;
    Result.Placement := AInput.Placement;
    Result.Identity := TInterfacedObject.Create;
    LContent := TSceneContent.Create;
    Result.ContentLease := LContent;
    LContent.Anchor := TNyxNode.Create(nkPanel, 'canvas-ownership');
    Result.Anchor := LContent.Anchor;
    Result.Anchor.Add(AInput.Control);

    if (Result.Anchor.Count <> 1) or (Result.Anchor.Children[0] <> AInput.Root) then
    begin
      raise ENyxCanvasScene.Create('A content implementation changed its root during admission');
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure FreeContentItems(var AItems: TSceneItems);
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(AItems) do
  begin
    AItems[LIndex].Free;
    AItems[LIndex] := nil;
  end;
  AItems := nil;
end;

function TCanvasScene.PrepareContent(const AChanges: array of INyxCanvasContentChange;
  AExpectedRevision: Integer): INyxCanvasLayoutPlan;
var
  LPlan: TContentPlan;
  LInputs: array of TContentInput;
  LOriginal: TNyxTextIndex;
  LEdits: TNyxTextIndex;
  LIndex: Integer;
  LInputIndex: Integer;
  LOriginalIndex: Integer;
  LCount: Integer;
  LFinalCount: Integer;
  LInput: TContentInput;
  LLayer: Integer;
  LInfo: TInfoArray;
  LItem: TSceneItem;
  LPrepared: Boolean;
begin
  CheckRevision;

  if (AExpectedRevision <> FRevision) or (Length(AChanges) > MaximumNyxCanvasItems) then
  begin
    raise ENyxCanvasScene.Create('Content preparation requires the current revision and bounded edits');
  end;
  LPlan := TContentPlan.Create;
  Result := LPlan;
  LPlan.Owner := Self;
  LPlan.Scene := Self;
  LPlan.Revision := FRevision;
  LInputs := nil;
  LOriginal := nil;
  LEdits := nil;
  LPrepared := False;
  try
    SetLength(LInputs, Length(AChanges));
    for LIndex := 0 to High(AChanges) do
    begin

      if AChanges[LIndex] = nil then
      begin
        raise ENyxCanvasScene.Create('A content change cannot be absent');
      end;
      LInput := TContentInput.Create;
      LInputs[LIndex] := LInput;
      LInput.Kind := AChanges[LIndex].Kind;
      LInput.Item := AChanges[LIndex].Item;
      LInput.Placement := AChanges[LIndex].Placement;
      LInput.Control := AChanges[LIndex].Control;

      if LInput.Control <> nil then
      begin
        LInput.Root := LInput.Control.Node;
      end;
    end;
    { Creator getters have completed. Refuse their intervening mutation before
      borrowing current item pointers or adopting any new root. }
    CheckRevision;

    if FRevision <> AExpectedRevision then
    begin
      raise ENyxCanvasScene.Create('Scene changed while content inputs were captured');
    end;
    LOriginal := TNyxTextIndex.Create;
    LEdits := TNyxTextIndex.Create;
    for LIndex := 0 to High(FItems) do
    begin
      LOriginal.AddFirst(FItems[LIndex].Reference.Name, LIndex);
    end;
    LFinalCount := Length(FItems);
    for LIndex := 0 to High(LInputs) do
    begin
      LInput := LInputs[LIndex];
      NyxCanvasItem(LInput.Item.Name);

      if (Ord(LInput.Kind) < Ord(Low(TNyxCanvasContentChangeKind))) or
        (Ord(LInput.Kind) > Ord(High(TNyxCanvasContentChangeKind))) or
        (LEdits.IndexOf(LInput.Item.Name) >= 0) then
      begin
        raise ENyxCanvasScene.Create('Use one known final content change per exact item');
      end;
      LEdits.AddFirst(LInput.Item.Name, LIndex);
      LOriginalIndex := LOriginal.IndexOf(LInput.Item.Name);

      if ((LInput.Kind = ncccAdd) and (LOriginalIndex >= 0)) or
        ((LInput.Kind <> ncccAdd) and (LOriginalIndex < 0)) then
      begin
        raise ENyxCanvasScene.Create('Content addition/replacement requires exact item membership');
      end;

      if LInput.Kind <> ncccRemove then
      begin
        RequirePlacement(LInput.Placement);

        if FindLayer(LInput.Placement.Layer) < 0 then
        begin
          raise ENyxCanvasScene.Create('Content placement requires an existing layer');
        end;
      end;

      if (LInput.Kind in [ncccAdd, ncccReplace]) and (LInput.Root = nil) then
      begin
        raise ENyxCanvasScene.Create('New content requires its current managed root');
      end;

      if (LInput.Kind in [ncccRemove, ncccPlace]) and (LInput.Control <> nil) then
      begin
        raise ENyxCanvasScene.Create('Only content additions/replacements carry a control');
      end;

      if LInput.Kind = ncccAdd then
      begin
        Inc(LFinalCount);
      end
      else if LInput.Kind = ncccRemove then
      begin
        Dec(LFinalCount);
      end;
    end;

    if LFinalCount > MaximumNyxCanvasItems then
    begin
      raise ENyxCanvasScene.Create('Content candidate exceeds the scene item limit');
    end;
    { From here through Retire, ordinary scene writes refuse. Root admission may
      enter a creator interface, but it cannot invalidate captured scene pointers. }
    FPreparedLayout := LPlan;
    SetLength(LPlan.Items, LFinalCount);
    LCount := 0;
    for LIndex := 0 to High(FItems) do
    begin
      LInputIndex := LEdits.IndexOf(FItems[LIndex].Reference.Name);

      if (LInputIndex >= 0) and (LInputs[LInputIndex].Kind = ncccRemove) then
      begin
        Continue;
      end;

      if (LInputIndex >= 0) and (LInputs[LInputIndex].Kind = ncccReplace) then
      begin
        LPlan.Items[LCount] := CreateContentItem(LInputs[LInputIndex]);
      end
      else
      begin
        LPlan.Items[LCount] := CopyContentItem(FItems[LIndex]);

        if LInputIndex >= 0 then
        begin
          LPlan.Items[LCount].Placement := LInputs[LInputIndex].Placement;
        end;
      end;
      Inc(LCount);
    end;
    for LIndex := 0 to High(LInputs) do
    begin

      if LInputs[LIndex].Kind = ncccAdd then
      begin
        LPlan.Items[LCount] := CreateContentItem(LInputs[LIndex]);
        Inc(LCount);
      end;
    end;
    SetLength(LInfo, LFinalCount);
    LPlan.Names := TNyxTextIndex.Create;
    for LIndex := 0 to High(LPlan.Items) do
    begin
      LPlan.Names.AddFirst(LPlan.Items[LIndex].Reference.Name, LIndex);
    end;
    LCount := 0;
    for LLayer := 0 to High(FLayers) do
    begin
      for LIndex := 0 to High(LPlan.Items) do
      begin
        LItem := LPlan.Items[LIndex];

        if LItem.Placement.Layer.Name <> FLayers[LLayer].Reference.Name then
        begin
          Continue;
        end;
        LInfo[LCount].Item := LItem.Reference;
        LInfo[LCount].Placement := LItem.Placement
          .Visible(LItem.Placement.IsVisible and FLayers[LLayer].Visible)
          .Pickable(LItem.Placement.IsPickable and FLayers[LLayer].Pickable);
        LInfo[LCount].PaintOrder := LCount;
        Inc(LCount);
      end;
    end;
    LPlan.Snapshot := TCanvasSnapshot.Create(FRevision + 1, LInfo);
    LPrepared := True;
  finally

    if not LPrepared then
    begin
      LPlan.Retire;
    end;
    for LIndex := 0 to High(LInputs) do
    begin
      LInputs[LIndex].Free;
    end;
    LOriginal.Free;
    LEdits.Free;
  end;
end;

destructor TContentPlan.Destroy;
begin
  Retire;
  inherited Destroy;
end;

function TContentPlan.GetPreview: INyxCanvasSnapshot;
begin
  Result := Snapshot;
end;

function TContentPlan.GetExpectedRevision: Integer;
begin
  Result := Revision;
end;

function TContentPlan.GetCommitted: Boolean;
begin
  Result := WasCommitted;
end;

function TContentPlan.BelongsTo(const AScene: INyxCanvasScene): Boolean;
begin
  Result := (Owner <> nil) and (Owner = AScene);
end;

function TContentPlan.PreviewControl(const AItem: TNyxCanvasItemRef): INyxNode;
var
  LIndex: Integer;
begin

  if Retired or (Names = nil) then
  begin
    raise ENyxCanvasScene.Create('Prepared content lookup has retired');
  end;
  LIndex := Names.IndexOf(AItem.Name);

  if LIndex < 0 then
  begin
    raise ENyxCanvasScene.Create('Prepared content item does not exist');
  end;
  Result := Items[LIndex].Anchor.Children[0].ComponentReference;
end;

procedure TContentPlan.Commit;
begin
  try
    Validate;
    CheckReady;
    Install;
  finally
    Retire;
  end;
end;

procedure TContentPlan.Validate;
begin

  if Retired or WasCommitted or Validated or (Scene = nil) or
    (Scene.FRevision <> Revision) or (Scene.FPreparedLayout <> Self) then
  begin
    raise ENyxCanvasScene.Create('Prepared content requires its unchanged reserved owner');
  end;
  Validated := True;
  CheckReady;
end;

procedure TContentPlan.CheckReady;
var
  LIndex: Integer;
begin

  if not Validated or Retired or WasCommitted or (Scene = nil) or
    (Scene.FRevision <> Revision) or (Scene.FPreparedLayout <> Self) then
  begin
    raise ENyxCanvasScene.Create('Prepared content owner/revision changed');
  end;
  for LIndex := 0 to High(Items) do
  begin

    if (Items[LIndex].Anchor = nil) or (Items[LIndex].Anchor.Count <> 1) or
      (Items[LIndex].Anchor.Children[0].Parent <> Items[LIndex].Anchor) then
    begin
      raise ENyxCanvasScene.Create('Prepared content root ownership changed');
    end;
  end;
end;

procedure TContentPlan.Install;
begin

  if not Validated or Retired or WasCommitted then
  begin
    Exit;
  end;
  Former := Scene.FItems;
  Scene.FItems := Items;
  Scene.FRevision := Revision + 1;
  WasCommitted := True;
end;

procedure TContentPlan.Notify;
begin
  { The owning view reconciles faces after every model participant installs. }
end;

procedure TContentPlan.Retire;
begin

  if Retired then
  begin
    Exit;
  end;
  Retired := True;
  try

    if WasCommitted then
    begin
      Items := nil;
      FreeContentItems(Former);
    end
    else
    begin
      FreeContentItems(Items);
    end;
  finally
    Names.Free;
    Names := nil;

    if (Scene <> nil) and (Scene.FPreparedLayout = Self) then
    begin
      Scene.FPreparedLayout := nil;
    end;
    Scene := nil;
    Owner := nil;
  end;
end;
