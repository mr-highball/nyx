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
unit nyx.canvas.palette;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls,
  nyx.catalog, nyx.catalog.labels, nyx.root.types, nyx.canvas.types,
  nyx.canvas.geometry, nyx.canvas.editing;

type
  { Closed placement inputs use world coordinates, independent of a target's
    widget pixels. Text fields deliberately permit incomplete editing; Capture
    admits finite numeric values and complete bounds before creating intent. }
  TNyxCanvasPalettePlacementField = (ncpfLeft, ncpfTop, ncpfWidth, ncpfHeight);

  { Copied item insertion intent. Source/history remain host-owned. NewControl
    requests one catalog-created reusable root; otherwise Content references an
    existing reusable. Define and optional attachment belong in the same atomic
    transaction as root creation. No document, facade or callback is retained. }
  TNyxCanvasItemAddition = record
    Owner: TNyxControlRef;
    Baseline: TNyxText;
    NewControl: Boolean;
    Kind: TNyxKindRef;
    Content: TNyxRootRef;
    Definition: TNyxCanvasChange;
    Attach: Boolean;
    Attachment: TNyxCanvasChange;
  end;

  { Local disposable form values, independent of history/source. Capture parks
    a draft when the editor is absent. Restore requires the exact copied palette
    context, including scene membership, and retires only a changed context.
    Values own no interfaces, trees, catalog, application or target handles. }
  TNyxCanvasPaletteDraft = record
  private
    FEditor: TNyxText;
    FContext: TNyxText;
    FValues: TNyxDataValue;
    FExpanded: Boolean;
    function GetDefined: Boolean;
  public
    procedure Capture(const AEditor: TNyxText; ARoot: TNyxNode);
    function Restore(ARoot: TNyxNode): Boolean;
    procedure Clear;
    { Strict editor-presentation data only. Unknown shapes/fields, non-text
      inputs and oversized context refuse; no request or execution is restored. }
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxCanvasPaletteDraft; static;
    property Defined: Boolean read GetDefined;
  end;

  { Specialized public Nyx card. Its ordinary children and copied discovery
    packet outlive the borrowed source/catalog independently. Native/browser
    renderers consume the portable tree; exact-root routers below also work on
    their realized copies. It owns no session, source engine or runtime canvas. }
  INyxCanvasItemPalette = interface(INyxCard)
    ['{C2069236-8FEC-41A9-B0A2-726CC3E8E486}']
    function GetSearchInput: INyxInput;
    function GetGroupSelect: INyxSelect;
    function GetComponentSelect: INyxSelect;
    function GetItemNameInput: INyxInput;
    function GetSceneNameInput: INyxInput;
    function GetLayerSelect: INyxSelect;
    function GetPlacementInput(AField: TNyxCanvasPalettePlacementField): INyxInput;
    function GetAddButton: INyxButton;
    property SearchInput: INyxInput read GetSearchInput;
    property GroupSelect: INyxSelect read GetGroupSelect;
    property ComponentSelect: INyxSelect read GetComponentSelect;
    property ItemNameInput: INyxInput read GetItemNameInput;
    property SceneNameInput: INyxInput read GetSceneNameInput;
    property LayerSelect: INyxSelect read GetLayerSelect;
    property AddButton: INyxButton read GetAddButton;
  end;

  { Default card retains typed child facades, whose descriptor leases have no
    reference back to this owner. Copied metadata stays in the portable Node;
    cloning a rendered form never reconstructs a document or catalog lease. }
  TNyxCanvasItemPalette = class(TNyxCard, INyxCanvasItemPalette)
  private
    FSearch: INyxInput;
    FGroup: INyxSelect;
    FChoice: INyxSelect;
    FItem: INyxInput;
    FScene: INyxInput;
    FLayer: INyxSelect;
    { pas2js supports managed interfaces in dynamic class arrays, not static
      interface arrays. Public access remains the checked closed field enum. }
    FPlacement: array of INyxInput;
    FAdd: INyxButton;
  public
    constructor Create(const AID: TNyxText; ADocument: TNyxDocument;
      ACatalog: TNyxCatalog; const AOwner: TNyxControlRef); reintroduce;
    function GetSearchInput: INyxInput;
    function GetGroupSelect: INyxSelect;
    function GetComponentSelect: INyxSelect;
    function GetItemNameInput: INyxInput;
    function GetSceneNameInput: INyxInput;
    function GetLayerSelect: INyxSelect;
    function GetPlacementInput(AField: TNyxCanvasPalettePlacementField): INyxInput;
    function GetAddButton: INyxButton;
  end;

{ Resolve a direct canvas or an owned Tool Canvas workspace part. Inherited
  template parts are not retargeted through a different selected instance.
  False clears the output and is ordinary non-canvas selection. }
function TryNyxCanvasPaletteOwner(ADocument: TNyxDocument;
  const ASelection: TNyxControlRef; out AOwner: TNyxControlRef): Boolean;
{ Borrow source/catalog only during construction. Catalog purpose descriptions,
  aliases/labels and saved reusable names supply filtering and touch-visible help.
  Page/component-reference/slot descriptors require their own authoring context. }
function NewNyxCanvasItemPalette(const AID: TNyxText; ADocument: TNyxDocument;
  ACatalog: TNyxCatalog; const AOwner: TNyxControlRef): INyxCanvasItemPalette;
{ Change only disposable fields in the exact mounted form. Filtering preserves
  its input controls/caret; callers Sync rather than rebuilding their editor.
  Unrelated input returns False; malformed owned packets fail explicitly. }
function RouteNyxCanvasItemPalette(ANode, ARoot: TNyxNode;
  ATrigger: TNyxTrigger): Boolean;
{ Capture owned Add against the current source. The host must recheck selection
  and queue the complete insertion with its ordinary exact paired baseline.
  Duplicate names, missing choices/layers and incomplete numbers refuse. }
function CaptureNyxCanvasItemPalette(AButton, ARoot: TNyxNode;
  ADocument: TNyxDocument; out AAddition: TNyxCanvasItemAddition): Boolean;

implementation

uses nyx.canvas.editor, nyx.canvas.declarations, nyx.composition, nyx.menu.editor,
  nyx.bytes, nyx.disclosure;

const
  CPalette = 'nyx.canvas-item-palette';
  CPlacementSuffix: array[TNyxCanvasPalettePlacementField] of TNyxText =
    ('left', 'top', 'width', 'height');
  CPlacementCaption: array[TNyxCanvasPalettePlacementField] of TNyxText =
    ('World X', 'World Y', 'Width', 'Height');
  CFieldSuffixes = '|search|group|choice|item|scene|layer|left|top|width|height|';

{ Default fields may be rearranged into reusable parts. Additional creator
  inputs keep their own behavior/drafts; this base router never claims them. }
function IsPaletteField(AForm, ANode: TNyxNode): Boolean;
var
  LSuffix: TNyxText;
begin
  Result := False;

  if (AForm = nil) or (ANode = nil) or (Pos(AForm.ID + '-', ANode.ID) <> 1) then
  begin
    Exit;
  end;
  LSuffix := Copy(ANode.ID, Length(AForm.ID) + 2, MaxInt);
  Result := (LSuffix <> '') and (Pos('|', LSuffix) = 0) and
    (Pos('|' + LSuffix + '|', CFieldSuffixes) > 0) and
    ((ANode.Kind = NyxKindName(nkInput)) or (ANode.Kind = NyxKindName(nkSelect)));
end;

{ Purpose-derived descriptor identity keeps the control type last, including
  collision suffixes. Unicode scalars are copied whole; ASCII punctuation forms
  separators. Pascal code generation still owns identifier escaping/collisions. }
function RootStem(const AName: TNyxText): TNyxText;
var
  LAt: Integer;
  LStart: Integer;
  LScalar: Integer;
begin
  Result := '';
  LAt := 1;
  while LAt <= Length(AName) do
  begin
    LStart := LAt;

    if not NyxNextScalar(AName, LAt, LScalar) then
    begin
      raise ENyxModel.Create('Canvas item name contains malformed Unicode');
    end;

    if (LScalar >= Ord('A')) and (LScalar <= Ord('Z')) then
    begin
      Result := Result + TNyxText(Chr(LScalar + Ord('a') - Ord('A')));
    end
    else if (LScalar >= 128) or
      ((LScalar >= Ord('a')) and (LScalar <= Ord('z'))) or
      ((LScalar >= Ord('0')) and (LScalar <= Ord('9'))) then
    begin
      Result := Result + Copy(AName, LStart, LAt - LStart);
    end
    else if (Result <> '') and (Result[Length(Result)] <> '-') then
    begin
      Result := Result + '-';
    end;
  end;

  if (Result <> '') and (Result[Length(Result)] = '-') then
  begin
    SetLength(Result, Length(Result) - 1);
  end;

  if Result = '' then
  begin
    Result := 'item';
  end;
end;

function TryNyxCanvasPaletteOwner(ADocument: TNyxDocument;
  const ASelection: TNyxControlRef; out AOwner: TNyxControlRef): Boolean;
var
  LNode: TNyxNode;
  LPart: TNyxNode;
begin
  Result := False;
  AOwner := Default(TNyxControlRef);

  if ADocument = nil then
  begin
    Exit;
  end;
  LNode := ADocument.Find(ASelection.ID);

  if LNode = nil then
  begin
    Exit;
  end;

  if LNode.ProjectionKind = NyxKindName(nkCanvas) then
  begin
    AOwner := NyxControl(LNode.ID);
    Exit(True);
  end;

  if LNode.Kind <> NyxKindName(nkToolCanvas) then
  begin
    Exit;
  end;
  LPart := LNode.Part(NyxPart('workspace'));

  if (LPart <> nil) and (LPart.ProjectionKind = NyxKindName(nkCanvas)) and
    (LNode.Find(LPart.ID) = LPart) then
  begin
    AOwner := NyxControl(LPart.ID);
    Result := True;
  end;
end;

{ Explicit metadata boundary for a disposable form. Search labels are copied
  into aliases so retaining the form never borrows registry/recipe storage. }
function Entry(const AInfo: TNyxComponentInfo; const ARoot: TNyxText;
  AIndex: Integer): TNyxDataValue;
var
  LAliases: TNyxText;
  LLabel: TNyxComponentLabel;
begin
  LAliases := AInfo.Discovery.Aliases + ' ' + AInfo.Title;
  for LLabel := Low(TNyxComponentLabel) to High(TNyxComponentLabel) do
  begin

    if LLabel in AInfo.Discovery.Labels then
    begin
      LAliases := LAliases + ' ' + NyxComponentLabelName(LLabel) + ' ' +
        NyxComponentLabelAliases(LLabel);
    end;
  end;
  Result := NyxObject([
    NyxField('kind', NyxData(AInfo.Kind)), NyxField('root', NyxData(ARoot)),
    NyxField('title', NyxData(NyxMenuEditorChoiceCaption(AIndex, AInfo.Title))),
    NyxField('category', NyxData(AInfo.Category)),
    NyxField('group', NyxData(NyxPaletteGroupKey(AInfo.Discovery.Group))),
    NyxField('aliases', NyxData(LAliases)),
    NyxField('description', NyxData(AInfo.Discovery.Description))]);
end;

function EntryInfo(const AValue: TNyxDataValue): TNyxComponentInfo;
begin
  Result := Default(TNyxComponentInfo);
  Result.Kind := AValue.Field('kind').AsText;
  Result.Title := AValue.Field('title').AsText;
  Result.Category := AValue.Field('category').AsText;

  if not TryNyxPaletteGroup(AValue.Field('group').AsText, Result.Discovery.Group) then
  begin
    raise ENyxModel.Create('Canvas component has an unknown discovery group');
  end;
  Result.Discovery.Aliases := AValue.Field('aliases').AsText;
  Result.Discovery.Description := AValue.Field('description').AsText;
end;

function Packet(AForm: TNyxNode): TNyxDataValue;
begin
  Result := AForm.Extensions.Value(NyxExtension(CPalette));

  if (Result.Kind <> ndObject) or (Result.Count <> 6) or
    (Result.Field('version').AsInteger <> 1) then
  begin
    raise ENyxModel.Create('Canvas palette requires its copied form context');
  end;
end;

procedure Filter(AForm: TNyxNode);
var
  LEntries: TNyxDataValue;
  LEntry: TNyxDataValue;
  LInfo: TNyxComponentInfo;
  LGroup: TNyxPaletteGroup;
  LQuery: TNyxText;
  LItems: TNyxText;
  LSelected: TNyxText;
  LFirst: TNyxText;
  LDescription: TNyxText;
  LCount: Integer;
  LIndex: Integer;
begin
  LEntries := Packet(AForm).Field('entries');
  LQuery := AForm.Find(AForm.ID + '-search').Prop(NyxAttributeName(atValue));

  if not TryNyxPaletteGroup(AForm.Find(AForm.ID + '-group').Prop(NyxAttributeName(atValue)), LGroup) then
  begin
    raise ENyxModel.Create('Choose a listed component intent group');
  end;
  LSelected := AForm.Find(AForm.ID + '-choice').Prop(NyxAttributeName(atValue));
  LItems := '';
  LFirst := '';
  LDescription := '';
  LCount := 0;
  for LIndex := 0 to LEntries.Count - 1 do
  begin
    LEntry := LEntries.Item(LIndex);
    LInfo := EntryInfo(LEntry);

    if not NyxComponentMatches(LInfo, LQuery, LGroup) then
    begin
      Continue;
    end;

    if LCount > 0 then
    begin
      LItems := LItems + #10;
    end;
    LItems := LItems + LInfo.Title;

    if LCount = 0 then
    begin
      LFirst := LInfo.Title;
      LDescription := LInfo.Discovery.Description;
    end;
    Inc(LCount);
  end;
  { Retain only a still-visible choice. Exact captions are unique through their
    private numeric prefix; application names themselves never select behavior. }
  LCount := 0;
  for LIndex := 0 to LEntries.Count - 1 do
  begin
    LEntry := LEntries.Item(LIndex);
    LInfo := EntryInfo(LEntry);

    if (LInfo.Title = LSelected) and NyxComponentMatches(LInfo, LQuery, LGroup) then
    begin
      Inc(LCount);
      LDescription := LInfo.Discovery.Description;
    end;
  end;

  if LCount = 0 then
  begin
    LSelected := LFirst;
  end;
  AForm.Find(AForm.ID + '-choice').Configure.Items(LItems).Value(LSelected).Done;
  AForm.Find(AForm.ID + '-description').Configure.Text(LDescription).Done;
  AForm.Find(AForm.ID + '-add').Configure.Enabled(LSelected <> '').Done;
end;

constructor TNyxCanvasItemPalette.Create(const AID: TNyxText; ADocument: TNyxDocument;
  ACatalog: TNyxCatalog; const AOwner: TNyxControlRef);
var
  LEntries: array of TNyxDataValue;
  LInfo: TNyxComponentInfo;
  LRoot: TNyxNode;
  LOwner: TNyxNode;
  LContext: TNyxNode;
  LProjection: TNyxNode;
  LScene: TNyxText;
  LLayers: TNyxText;
  LLayer: TNyxText;
  LGroups: TNyxText;
  LPlan: INyxCanvasDefinition;
  LIndex: Integer;
  LCount: Integer;
  LNumber: Integer;
  LExists: Boolean;
  LField: TNyxCanvasPalettePlacementField;
  LGroup: TNyxPaletteGroup;
  LItemNames: array of TNyxDataValue;
  LDetails: INyxDisclosure;
begin
  inherited Create(AID, ncoDescriptor);

  if (ADocument = nil) or (ACatalog = nil) then
  begin
    raise ENyxModel.Create('Canvas palette requires its source and component catalog');
  end;
  LOwner := ADocument.Find(AOwner.ID);

  if LOwner = nil then
  begin
    raise ENyxModel.Create('Canvas palette owner is no longer available');
  end;
  LContext := RealizeNyxContext(ADocument, LOwner, LProjection);
  try
    LScene := '';
    LLayers := '';
    LLayer := 'Content';
    LNumber := 1;
    LItemNames := nil;

    if (LProjection = nil) or (LProjection.ProjectionKind <> NyxKindName(nkCanvas)) then
    begin
      raise ENyxModel.Create('Canvas palette requires an authored canvas owner');
    end;

    if LProjection.CanvasScene.Defined then
    begin
      LScene := LProjection.CanvasScene.Name;
      LPlan := ADocument.Canvases.Definition(LProjection.CanvasScene);
      for LIndex := 0 to LPlan.LayerCount - 1 do
      begin

        if LIndex > 0 then
        begin
          LLayers := LLayers + #10;
        end;
        LLayers := LLayers + LPlan.Layers[LIndex].Layer.Name;
      end;

      if LPlan.LayerCount > 0 then
      begin
        LLayer := LPlan.Layers[0].Layer.Name;
      end;
      LNumber := LPlan.ItemCount + 1;
      SetLength(LItemNames, LPlan.ItemCount);
      for LIndex := 0 to High(LItemNames) do
      begin
        LItemNames[LIndex] := NyxData(LPlan.Items[LIndex].Item.Name);
      end;
    end;
    LEntries := nil;
    for LIndex := 0 to ACatalog.Count - 1 do
    begin
      LInfo := ACatalog[LIndex];

      if (LInfo.Kind = NyxKindName(nkPage)) or
        (LInfo.Kind = NyxKindName(nkComponent)) or
        (LInfo.Kind = NyxKindName(nkSlotOverride)) then
      begin
        Continue;
      end;
      LCount := Length(LEntries);
      SetLength(LEntries, LCount + 1);
      LEntries[LCount] := Entry(LInfo, '', LCount);
    end;
    for LIndex := 0 to ADocument.ComponentCount - 1 do
    begin
      LRoot := ADocument.Components[LIndex];
      LInfo := Default(TNyxComponentInfo);
      LInfo.Kind := LRoot.Kind;
      LInfo.Title := 'Saved / ' + LRoot.ID;
      LInfo.Category := 'Reusable components';
      LInfo.Discovery.Group := pgComposition;
      LInfo.Discovery.Labels := [clReusable];
      LInfo.Discovery.Description := 'Reuse ' + LRoot.ID + ' as an independently mounted canvas item.';
      LCount := Length(LEntries);
      SetLength(LEntries, LCount + 1);
      LEntries[LCount] := Entry(LInfo, LRoot.ID, LCount);
    end;
    Node.Extensions.SetValue(NyxExtension(CPalette), NyxObject([
      NyxField('version', NyxData(1)), NyxField('owner', NyxData(AOwner.ID)),
      NyxField('baseline', NyxData(NyxCanvasEditorBaseline(ADocument, AOwner))),
      NyxField('scene', NyxData(LScene)), NyxField('entries', NyxArray(LEntries)),
      NyxField('items', NyxArray(LItemNames))]));
    Node.Configure.Padding(12).Gap(10).Done;
    Add(NewNyxHeading(AID + '-title').WithText('Add to canvas'));
    FSearch := NewNyxInput(AID + '-search').WithText('Find a control or saved component');
    FSearch.Configure.Placeholder('Name, purpose or label...').Done;
    Add(FSearch);
    LGroups := '';
    for LGroup := Low(TNyxPaletteGroup) to High(TNyxPaletteGroup) do
    begin

      if LGroups <> '' then
      begin
        LGroups := LGroups + #10;
      end;
      LGroups := LGroups + NyxPaletteGroupName(LGroup);
    end;
    FGroup := NewNyxSelect(AID + '-group').WithText('Intent group');
    FGroup.Configure.Items(LGroups).Value(NyxPaletteGroupName(pgAll)).Done;
    Add(FGroup);
    FChoice := NewNyxSelect(AID + '-choice').WithText('Component');
    Add(FChoice);
    Add(NewNyxLabel(AID + '-description'));
    FAdd := NewNyxButton(AID + '-add').WithText('Add component');
    FAdd.Configure.Variant(nvPrimary).Done;
    Add(FAdd);
    LDetails := NewNyxDisclosure(AID + '-placement').WithText('Name and placement');
    LDetails.Node.Configure.Value(False).Done;
    Add(LDetails);
    FItem := NewNyxInput(AID + '-item').WithText('Item name');
    FItem.Configure.Value('Item ' + TNyxText(IntToStr(LNumber))).Done;
    LDetails.BodyColumn.Add(FItem);
    FScene := NewNyxInput(AID + '-scene').WithText('Scene name');

    if LScene <> '' then
    begin
      FScene.Configure.Value(LScene).ReadOnly(True).Done;
    end
    else
    begin
      LNumber := 0;
      repeat
        Inc(LNumber);
        LScene := 'Canvas ' + TNyxText(IntToStr(LNumber));
        LExists := False;
        for LIndex := 0 to ADocument.Canvases.Count - 1 do
        begin
          LExists := LExists or (ADocument.Canvases.Reference(LIndex).Name = LScene);
        end;
      until not LExists;
      FScene.Configure.Value(LScene).Done;
    end;
    LDetails.BodyColumn.Add(FScene);

    if LLayers = '' then
    begin
      LLayers := LLayer;
    end;
    FLayer := NewNyxSelect(AID + '-layer').WithText('Layer');
    FLayer.Configure.Items(LLayers).Value(LLayer).Done;
    LDetails.BodyColumn.Add(FLayer);
    SetLength(FPlacement, Ord(High(TNyxCanvasPalettePlacementField)) + 1);
    for LField := Low(TNyxCanvasPalettePlacementField) to High(TNyxCanvasPalettePlacementField) do
    begin
      FPlacement[Ord(LField)] := NewNyxInput(AID + '-' + CPlacementSuffix[LField]).WithText(CPlacementCaption[LField]);
      FPlacement[Ord(LField)].Configure.Value('0').Done;

      if LField = ncpfWidth then
      begin
        FPlacement[Ord(LField)].Configure.Value('280').Done;
      end
      else if LField = ncpfHeight then
      begin
        FPlacement[Ord(LField)].Configure.Value('120').Done;
      end;
      LDetails.BodyColumn.Add(FPlacement[Ord(LField)]);
    end;
    Add(NewNyxLabel(AID + '-shared').WithText('Saved scene changes apply wherever this scene is used.'));
    Filter(Node);
  finally
    LContext.Free;
  end;
end;

function TNyxCanvasItemPalette.GetSearchInput: INyxInput;
begin
  Result := FSearch;
end;

function TNyxCanvasItemPalette.GetGroupSelect: INyxSelect;
begin
  Result := FGroup;
end;

function TNyxCanvasItemPalette.GetComponentSelect: INyxSelect;
begin
  Result := FChoice;
end;

function TNyxCanvasItemPalette.GetItemNameInput: INyxInput;
begin
  Result := FItem;
end;

function TNyxCanvasItemPalette.GetSceneNameInput: INyxInput;
begin
  Result := FScene;
end;

function TNyxCanvasItemPalette.GetLayerSelect: INyxSelect;
begin
  Result := FLayer;
end;

function TNyxCanvasItemPalette.GetPlacementInput(AField: TNyxCanvasPalettePlacementField): INyxInput;
begin

  if (Ord(AField) < Ord(Low(TNyxCanvasPalettePlacementField))) or
    (Ord(AField) > Ord(High(TNyxCanvasPalettePlacementField))) then
  begin
    raise ENyxModel.Create('Choose an existing typed placement field');
  end;
  Result := FPlacement[Ord(AField)];
end;

function TNyxCanvasItemPalette.GetAddButton: INyxButton;
begin
  Result := FAdd;
end;

function NewNyxCanvasItemPalette(const AID: TNyxText; ADocument: TNyxDocument;
  ACatalog: TNyxCatalog; const AOwner: TNyxControlRef): INyxCanvasItemPalette;
begin
  Result := TNyxCanvasItemPalette.Create(AID, ADocument, ACatalog, AOwner);
end;

function FormFor(ANode, ARoot: TNyxNode): TNyxNode;
begin
  Result := nil;

  if (ANode = nil) or (ARoot = nil) or (ARoot.Find(ANode.ID) <> ANode) then
  begin
    Exit;
  end;
  Result := ANode.Parent;

  while (Result <> nil) and not Result.Extensions.Has(NyxExtension(CPalette)) do
  begin
    Result := Result.Parent;
  end;

  if (Result = nil) or not Result.Extensions.Has(NyxExtension(CPalette)) then
  begin
    Result := nil;
  end;
end;

function RouteNyxCanvasItemPalette(ANode, ARoot: TNyxNode;
  ATrigger: TNyxTrigger): Boolean;
var
  LForm: TNyxNode;
begin
  Result := False;

  if ATrigger <> ntChange then
  begin
    Exit;
  end;
  LForm := FormFor(ANode, ARoot);

  if LForm = nil then
  begin
    Exit;
  end;

  if (ANode.ID = LForm.ID + '-search') or (ANode.ID = LForm.ID + '-group') or
    (ANode.ID = LForm.ID + '-choice') then
  begin
    Filter(LForm);
    Result := True;
  end;

  if IsPaletteField(LForm, ANode) then
  begin
    Result := True;
  end;
end;

function CaptureNyxCanvasItemPalette(AButton, ARoot: TNyxNode;
  ADocument: TNyxDocument; out AAddition: TNyxCanvasItemAddition): Boolean;
var
  LForm: TNyxNode;
  LPacket: TNyxDataValue;
  LEntries: TNyxDataValue;
  LEntry: TNyxDataValue;
  LChoice: TNyxText;
  LKind: TNyxText;
  LRoot: TNyxText;
  LStem: TNyxText;
  LScene: TNyxCanvasRef;
  LItem: TNyxCanvasItemRef;
  LLayer: TNyxCanvasLayerRef;
  LPlan: INyxCanvasDefinition;
  LPlacement: TNyxCanvasPlacement;
  LValues: array[TNyxCanvasPalettePlacementField] of Double;
  LField: TNyxCanvasPalettePlacementField;
  LNumber: Integer;
  LIndex: Integer;
  LFound: Boolean;
begin
  Result := False;
  AAddition := Default(TNyxCanvasItemAddition);
  LForm := FormFor(AButton, ARoot);

  if (LForm = nil) or (AButton.ID <> LForm.ID + '-add') then
  begin
    Exit;
  end;
  LPacket := Packet(LForm);
  AAddition.Owner := NyxControl(LPacket.Field('owner').AsText);
  AAddition.Baseline := LPacket.Field('baseline').AsText;

  if NyxCanvasEditorBaseline(ADocument, AAddition.Owner) <> AAddition.Baseline then
  begin
    raise ENyxModel.Create('Select this canvas again before adding its content');
  end;
  LChoice := LForm.Find(LForm.ID + '-choice').Prop(NyxAttributeName(atValue));
  LEntries := LPacket.Field('entries');
  LFound := False;
  for LIndex := 0 to LEntries.Count - 1 do
  begin
    LEntry := LEntries.Item(LIndex);

    if LEntry.Field('title').AsText = LChoice then
    begin
      LKind := LEntry.Field('kind').AsText;
      LRoot := LEntry.Field('root').AsText;
      LFound := True;
      Break;
    end;
  end;

  if not LFound then
  begin
    raise ENyxModel.Create('Choose a listed control or saved reusable component');
  end;
  LScene := NyxCanvasRef(LForm.Find(LForm.ID + '-scene').Prop(NyxAttributeName(atValue)));
  AAddition.Attach := LPacket.Field('scene').AsText = '';

  if AAddition.Attach then
  begin
    for LIndex := 0 to ADocument.Canvases.Count - 1 do
    begin

      if ADocument.Canvases.Reference(LIndex).Name = LScene.Name then
      begin
        raise ENyxModel.Create('Choose a new scene name or attach the existing scene explicitly');
      end;
    end;
    LPlan := NewNyxCanvasDefinition;
  end
  else
  begin

    if LScene.Name <> LPacket.Field('scene').AsText then
    begin
      raise ENyxModel.Create('The palette cannot retarget a different saved scene');
    end;
    LPlan := ADocument.Canvases.Definition(LScene);
  end;
  LItem := NyxCanvasItem(LForm.Find(LForm.ID + '-item').Prop(NyxAttributeName(atValue)));
  for LIndex := 0 to LPlan.ItemCount - 1 do
  begin

    if LPlan.Items[LIndex].Item.Name = LItem.Name then
    begin
      raise ENyxModel.Create('This scene already contains that item name');
    end;
  end;
  LLayer := NyxCanvasLayer(LForm.Find(LForm.ID + '-layer').Prop(NyxAttributeName(atValue)));
  LFound := False;
  for LIndex := 0 to LPlan.LayerCount - 1 do
  begin
    LFound := LFound or (LPlan.Layers[LIndex].Layer.Name = LLayer.Name);
  end;

  if LPlan.LayerCount = 0 then
  begin
    LPlan := LPlan.WithLayer(LLayer);
  end
  else if not LFound then
  begin
    raise ENyxModel.Create('Choose an existing scene layer');
  end;
  for LField := Low(TNyxCanvasPalettePlacementField) to High(TNyxCanvasPalettePlacementField) do
  begin
    LValues[LField] := TNyxDataValue.ParseJSON(
      LForm.Find(LForm.ID + '-' + CPlacementSuffix[LField]).Prop(NyxAttributeName(atValue))).AsNumber;
  end;

  if (LValues[ncpfWidth] <= 0) or (LValues[ncpfHeight] <= 0) then
  begin
    raise ENyxModel.Create('Embedded controls require positive width and height');
  end;
  LPlacement := NyxCanvasPlacement(LLayer, NyxCanvasBounds(LValues[ncpfLeft],
    LValues[ncpfTop], LValues[ncpfWidth], LValues[ncpfHeight]));
  AAddition.Kind := NyxCustomKind(LKind);
  AAddition.NewControl := LRoot = '';

  if AAddition.NewControl then
  begin
    LNumber := 0;
    LStem := RootStem(LItem.Name);
    repeat
      Inc(LNumber);
      LRoot := LStem;

      if LNumber > 1 then
      begin
        LRoot := LRoot + '-' + TNyxText(IntToStr(LNumber));
      end;
      LRoot := LRoot + '-' + LKind;
    until ADocument.Find(LRoot) = nil;
  end
  else if ADocument.FindComponent(LRoot) = nil then
  begin
    raise ENyxModel.Create('The selected reusable component is no longer available');
  end;
  AAddition.Content := NyxReusableRoot(LRoot);
  LPlan := LPlan.WithItem(LItem, AAddition.Content, LPlacement);
  AAddition.Definition := NyxDefineCanvas(LScene, LPlan);

  if AAddition.Attach then
  begin
    AAddition.Attachment := NyxAttachCanvas(AAddition.Owner, LScene);
  end;
  Result := True;
end;

{$I nyx.canvas.palette.draft.inc}

end.
