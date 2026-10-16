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

implementation

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
