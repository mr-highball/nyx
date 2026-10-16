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
unit nyx.diagram.materialization;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses Math, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.diagram,
  nyx.diagram.content, nyx.canvas.geometry;

{ Rectangular content allocation within the shape's world bounds. Padding and
  optional caption space clamp to available extent; an exhausted box becomes
  zero-sized instead of moving/resizing the authored shape. Mathematical shape
  clipping is not implied. Normal canvas reflow/clipping owns target allocation. }
function NyxDiagramContentBounds(const AVertex: TNyxDiagramVertex): TNyxCanvasBounds;

{ Construct an independently owned authored Nyx image/component descriptor.
  It retains the ordinary resource binding/reusable reference, never a resolved
  bitmap or renderer. The canvas resolves it against its copied source context.
  The caller may customize the returned specialized implementation before scene
  admission; no document or scene owns it until that explicit adoption. }
function NewNyxDiagramContentControl(const AID: TNyxText;
  const AVertex: TNyxDiagramVertex): INyxNode;

{ Identity-preserving geometry/padding/header edits can reuse their live content.
  Image source/fit/alt caption or reusable-template changes require replacement.
  This compares immutable descriptor values and enters no renderer or provider. }
function SameNyxDiagramContent(const ABefore, AAfter: TNyxDiagramVertex): Boolean;

{ Compare the copied source meaning used by unchanged content selectors. Reusable
  closure/global binding declarations are exact; unrelated roots, project title
  and visual diagram declarations do not replace a component. No view is retained.
  The caller validates documents before comparison and may cache per reference. }
function SameNyxDiagramSource(ABefore, AAfter: TNyxDocument;
  const AContent: TNyxDiagramContent): Boolean;

implementation

uses nyx.codec, nyx.data, nyx.root.types, nyx.canvas.declarations,
  nyx.menu.types, nyx.menu.declarations;

{ Focused source captures retain every transitive reusable and binding context.
  Excluding the visual editor's graph avoids replacing unchanged controls after
  moving an unrelated shape. All remaining globals conservatively participate. }
function DiagramSourceMeaning(ADocument: TNyxDocument;
  const AContent: TNyxDiagramContent): TNyxText;
var
  LData: TNyxDataValue;
  LFields: array of TNyxDataField;
  LItems: array of TNyxDataValue;
  LDependencies: array of TNyxDataValue;
  LVisited: TNyxStrings;
  LIndex: Integer;
  LCount: Integer;

  procedure VisitNode(ANode: TNyxNode); forward;

  { Capture only owned immutable source values. A dependency is never moved to
    another root partition or validated as a different standalone document. }
  procedure VisitRoot(const AReference: TNyxRootRef);
  var
    LKey: TNyxText;
    LNode: TNyxNode;
    LIndex: Integer;
  begin
    LKey := NyxRootKindName(AReference.Kind) + '/' + AReference.Name;

    if LVisited.IndexOf(LKey) >= 0 then
    begin
      Exit;
    end;
    LVisited.Add(LKey);
    LNode := ADocument.FindRoot(AReference);
    LIndex := Length(LDependencies);
    SetLength(LDependencies, LIndex + 1);
    LDependencies[LIndex] := NyxObject([NyxField('kind', NyxData(NyxRootKindName(AReference.Kind))),
      NyxField('node', TNyxCodec.NodeSnapshot(LNode))]);
    VisitNode(LNode);
  end;

  procedure VisitMenu(const AReference: TNyxMenuRef);
  var
    LDefinition: INyxMenuDefinition;
    LIndex: Integer;
    LCount: Integer;
  begin

    if LVisited.IndexOf(TNyxText('menu/') + AReference.Name) >= 0 then
    begin
      Exit;
    end;
    LVisited.Add(TNyxText('menu/') + AReference.Name);
    LDefinition := ADocument.Menus.Definition(AReference);
    LCount := Length(LDependencies);
    SetLength(LDependencies, LCount + 1);
    LDependencies[LCount] := NyxObject([NyxField('menu', NyxData(AReference.Name)),
      NyxField('definition', LDefinition.ToData)]);
    VisitRoot(LDefinition.Root);
    for LIndex := 0 to LDefinition.Count - 1 do
    begin

      if LDefinition.Item(LIndex).Kind = nmiSubmenu then
      begin
        VisitMenu(LDefinition.Item(LIndex).Submenu);
      end;
    end;
  end;

  procedure VisitNode(ANode: TNyxNode);
  var
    LIndex: Integer;
    LDefinition: INyxCanvasDefinition;
  begin

    if ANode.HasMenu and (ANode.MenuReference.Name <> '') then
    begin
      VisitMenu(ANode.MenuReference);
    end;

    if ANode.HasMenuBar and (ANode.MenuBar <> nil) then
    begin
      for LIndex := 0 to ANode.MenuBar.Count - 1 do
      begin
        VisitMenu(ANode.MenuBar.Item(LIndex).Menu);
      end;
    end;

    if (ANode.Kind = NyxKindName(nkComponent)) or
      (ANode.ProjectionKind = NyxKindName(nkComponent)) then
    begin
      VisitRoot(NyxReusableRoot(ANode.DefaultComponent.Name));

      if ANode.Prop(NyxAttributeName(atComponent)) <> '' then
      begin
        VisitRoot(NyxReusableRoot(ANode.Prop(NyxAttributeName(atComponent))));
      end;

      if ANode.HasContent then
      begin
        for LIndex := 0 to ANode.Content.Count - 1 do
        begin
          VisitRoot(NyxReusableRoot(ANode.Content.Rule(LIndex).Component.Name));
        end;
      end;
    end;

    if ANode.CanvasScene.Defined and
      (LVisited.IndexOf(TNyxText('canvas/') + ANode.CanvasScene.Name) < 0) then
    begin
      LVisited.Add(TNyxText('canvas/') + ANode.CanvasScene.Name);
      LDefinition := ADocument.Canvases.Definition(ANode.CanvasScene);
      LIndex := Length(LDependencies);
      SetLength(LDependencies, LIndex + 1);
      LDependencies[LIndex] := NyxObject([NyxField('canvas', NyxData(ANode.CanvasScene.Name)),
        NyxField('definition', LDefinition.ToData)]);
      for LIndex := 0 to LDefinition.ItemCount - 1 do
      begin
        VisitRoot(LDefinition.Items[LIndex].Content);
      end;
    end;
    for LIndex := 0 to ANode.Count - 1 do
    begin
      VisitNode(ANode.Children[LIndex]);
    end;
  end;
begin

  if AContent.Kind = ndckImage then
  begin
    SetLength(LItems, ADocument.Resources.Count);
    LCount := 0;
    for LIndex := 0 to ADocument.Resources.Count - 1 do
    begin

      if ADocument.Resources.Reference(LIndex).Name = AContent.Image.Reference.Name then
      begin
        LItems[LCount] := NyxObject([
          NyxField('locale', NyxData(ADocument.Resources.Locale(LIndex).Name)),
          NyxField('definition', ADocument.Resources.Definition(
            ADocument.Resources.Reference(LIndex), ADocument.Resources.Locale(LIndex)).ToData)]);
        Inc(LCount);
      end;
    end;
    SetLength(LItems, LCount);
    Exit(NyxArray(LItems).ToJSON);
  end;
  LVisited := TNyxStrings.Create;
  try
    VisitRoot(NyxReusableRoot(AContent.Component.Name));
    LData := TNyxDataValue.ParseJSON(TNyxCodec.Encode(ADocument));
    SetLength(LFields, LData.Count + 1);
    LFields[0] := NyxField('dependencies', NyxArray(LDependencies));
    LCount := 1;
    for LIndex := 0 to LData.Count - 1 do
    begin

      if (LData.Key(LIndex) = 'version') or (LData.Key(LIndex) = 'title') or
        (LData.Key(LIndex) = 'diagrams') or (LData.Key(LIndex) = 'pages') or
        (LData.Key(LIndex) = 'components') or (LData.Key(LIndex) = 'menus') or
        (LData.Key(LIndex) = 'canvases') then
      begin
        Continue;
      end;
      LFields[LCount] := NyxField(LData.Key(LIndex), LData.Field(LData.Key(LIndex)));
      Inc(LCount);
    end;
    SetLength(LFields, LCount);
    Result := NyxObject(LFields).ToJSON;
  finally
    LVisited.Free;
  end;
end;

function SameNyxDiagramSource(ABefore, AAfter: TNyxDocument;
  const AContent: TNyxDiagramContent): Boolean;
begin
  Result := (ABefore = AAfter) or (AContent.Kind = ndckNone);

  if Result then
  begin
    Exit;
  end;

  if (ABefore = nil) or (AAfter = nil) then
  begin
    raise ENyxDiagram.Create('Content source comparison requires both admitted contexts');
  end;
  Result := DiagramSourceMeaning(ABefore, AContent) = DiagramSourceMeaning(AAfter, AContent);
end;

function NyxDiagramContentBounds(const AVertex: TNyxDiagramVertex): TNyxCanvasBounds;
var
  LHeader: Double;
  LX: Double;
  LY: Double;
begin
  LHeader := 0;

  if AVertex.Caption <> '' then
  begin
    LHeader := Min(AVertex.Content.HeaderHeight, AVertex.Bounds.Height);
  end;
  LX := Min(AVertex.Content.Padding, AVertex.Bounds.Width / 2);
  LY := Min(AVertex.Content.Padding, (AVertex.Bounds.Height - LHeader) / 2);
  Result := NyxCanvasBounds(AVertex.Bounds.Left + LX, AVertex.Bounds.Top + LHeader + LY,
    AVertex.Bounds.Width - 2 * LX, AVertex.Bounds.Height - LHeader - 2 * LY);
end;

function NewNyxDiagramContentControl(const AID: TNyxText;
  const AVertex: TNyxDiagramVertex): INyxNode;
var
  LImage: INyxImage;
  LComponent: INyxComponent;
begin
  { Standalone vertex admission captures a closed value before constructing
    any owned descriptor; target/document reference admission remains separate. }
  AVertex.ToData;
  case AVertex.Content.Kind of
    ndckImage:
      begin
        LImage := NewNyxImage(AID).WithText(AVertex.Caption);
        LImage.Configure.WidthSizing(nsFill).HeightSizing(nsFill).MinimumHeight(0)
          .ImageFit(AVertex.Content.ImageFit).Done;
        LImage.Binds.Image(AVertex.Content.Image);
        Result := LImage;
      end;
    ndckComponent:
      begin
        LComponent := NewNyxComponent(AID);
        LComponent.Configure.Component(AVertex.Content.Component)
          .WidthSizing(nsFill).HeightSizing(nsFill).MinimumHeight(0).Done;
        Result := LComponent;
      end;
  else
    raise ENyxDiagram.Create('A content control requires an image or reusable descriptor');
  end;
end;

function SameNyxDiagramContent(const ABefore, AAfter: TNyxDiagramVertex): Boolean;
begin
  Result := ABefore.Content.Kind = AAfter.Content.Kind;

  if not Result then
  begin
    Exit;
  end;
  case ABefore.Content.Kind of
    ndckNone:
      begin
        Result := True;
      end;
    ndckImage:
      begin
        Result := (ABefore.Content.Image.ToData.ToJSON = AAfter.Content.Image.ToData.ToJSON) and
          (ABefore.Content.ImageFit = AAfter.Content.ImageFit) and (ABefore.Caption = AAfter.Caption);
      end;
    ndckComponent:
      begin
        Result := ABefore.Content.Component.Name = AAfter.Content.Component.Name;
      end;
  end;
end;

end.
