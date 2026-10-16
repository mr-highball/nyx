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
unit nyx.diagram.content;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, Math, nyx.text, nyx.types, nyx.data, nyx.resources, nyx.images,
  nyx.canvas.types;

type
  { Closed optional content family. Image selectors retain hosted/embedded
    resource abstraction; components name reusable Nyx templates, not classes. }
  TNyxDiagramContentKind = (ndckNone, ndckImage, ndckComponent);

  { Immutable value-only content intent. Padding uses world units; the owning
    shape supplies its content box. Image fit preserves ordinary Nyx image
    behavior. Component content is independently materialized by the canvas,
    preserving the reusable's ordinary bindings/events/parts. This record owns
    names and selectors only, never controls, resources, documents or callbacks.
    None is explicit absence. Availability/cycles are document admission concerns;
    target allocation, pixel decoding and hosted loading remain adapter concerns. }
  TNyxDiagramContent = record
  private
    FKind: TNyxDiagramContentKind;
    FImage: TNyxResourceImageRef;
    FComponent: TNyxComponentRef;
    FFit: TNyxImageFit;
    FPadding: Double;
    FCaptionSpace: Double;
  public
    { Derive finite 0..1,000,000 world-unit inset. None refuses configuration;
      available space is clamped later by the shape's content allocation. }
    function Padded(AWorldUnits: Double): TNyxDiagramContent;
    { Image-only closed fit policy; other content kinds refuse rather than
      retaining unused behavior flags. The original record remains unchanged. }
    function Fitted(AFit: TNyxImageFit): TNyxDiagramContent;
    { Reserve a world-space header above content when a caption exists. Zero
      deliberately permits filling beneath it. Small shapes can clip to no area. }
    function CaptionSpace(AWorldUnits: Double): TNyxDiagramContent;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxDiagramContent; static;
    property Kind: TNyxDiagramContentKind read FKind;
    property Image: TNyxResourceImageRef read FImage;
    property Component: TNyxComponentRef read FComponent;
    property ImageFit: TNyxImageFit read FFit;
    property Padding: Double read FPadding;
    property HeaderHeight: Double read FCaptionSpace;
  end;

{ Capture typed selectors/reusable names; no registry or control is retained.
  Default/No content is the deliberate clear value. Invalid arguments refuse. }
function NyxDiagramImage(const AImage: TNyxResourceImageRef): TNyxDiagramContent;
function NyxDiagramComponent(const AComponent: TNyxComponentRef): TNyxDiagramContent;
function NyxNoDiagramContent: TNyxDiagramContent;

implementation

procedure RequireKind(AKind: Integer);
begin

  if (AKind < Ord(Low(TNyxDiagramContentKind))) or
    (AKind > Ord(High(TNyxDiagramContentKind))) then
  begin
    raise EArgumentException.Create('Unknown diagram content kind');
  end;
end;

procedure RequireFit(AFit: Integer);
begin

  if (AFit < Ord(Low(TNyxImageFit))) or (AFit > Ord(High(TNyxImageFit))) then
  begin
    raise EArgumentException.Create('Unknown diagram image fit');
  end;
end;

function NyxNoDiagramContent: TNyxDiagramContent;
begin
  Result := Default(TNyxDiagramContent);
end;

function NyxDiagramImage(const AImage: TNyxResourceImageRef): TNyxDiagramContent;
begin
  Result := NyxNoDiagramContent;
  Result.FImage := TNyxResourceImageRef.FromData(AImage.ToData);
  Result.FKind := ndckImage;
  Result.FPadding := 8;
  Result.FCaptionSpace := 24;
end;

function NyxDiagramComponent(const AComponent: TNyxComponentRef): TNyxDiagramContent;
begin
  Result := NyxNoDiagramContent;
  Result.FComponent := NyxComponent(NyxCanvasRef(AComponent.Name).Name);
  Result.FKind := ndckComponent;
  Result.FPadding := 8;
  Result.FCaptionSpace := 24;
end;

function TNyxDiagramContent.Padded(AWorldUnits: Double): TNyxDiagramContent;
begin
  RequireKind(Ord(FKind));

  if (FKind = ndckNone) or IsNan(AWorldUnits) or IsInfinite(AWorldUnits) or
    (AWorldUnits < 0) or (AWorldUnits > 1000000) then
  begin
    raise EArgumentException.Create('Content padding requires content and finite 0..1,000,000 world units');
  end;
  Result := Self;
  Result.FPadding := AWorldUnits;
end;

function TNyxDiagramContent.CaptionSpace(AWorldUnits: Double): TNyxDiagramContent;
begin
  RequireKind(Ord(FKind));

  if (FKind = ndckNone) or IsNan(AWorldUnits) or IsInfinite(AWorldUnits) or
    (AWorldUnits < 0) or (AWorldUnits > 1000000) then
  begin
    raise EArgumentException.Create('Content caption space requires finite 0..1,000,000 world units');
  end;
  Result := Self;
  Result.FCaptionSpace := AWorldUnits;
end;

function TNyxDiagramContent.Fitted(AFit: TNyxImageFit): TNyxDiagramContent;
begin
  RequireFit(Ord(AFit));

  if FKind <> ndckImage then
  begin
    raise EArgumentException.Create('Only image content has an image-fit policy');
  end;
  Result := Self;
  Result.FFit := AFit;
end;

function TNyxDiagramContent.ToData: TNyxDataValue;
var
  LValue: TNyxDiagramContent;
begin
  RequireKind(Ord(FKind));

  if FKind = ndckNone then
  begin
    Exit(NyxNull);
  end;

  if FKind = ndckImage then
  begin
    LValue := NyxDiagramImage(FImage).Padded(FPadding).Fitted(FFit).CaptionSpace(FCaptionSpace);
    Result := NyxObject([NyxField('kind', NyxData('image')),
      NyxField('image', LValue.Image.ToData), NyxField('fit', NyxData(Ord(LValue.ImageFit))),
      NyxField('padding', NyxData(LValue.Padding)), NyxField('captionSpace', NyxData(LValue.HeaderHeight))]);
  end
  else
  begin
    LValue := NyxDiagramComponent(FComponent).Padded(FPadding).CaptionSpace(FCaptionSpace);
    Result := NyxObject([NyxField('kind', NyxData('component')),
      NyxField('component', NyxData(LValue.Component.Name)),
      NyxField('padding', NyxData(LValue.Padding)), NyxField('captionSpace', NyxData(LValue.HeaderHeight))]);
  end;
end;

class function TNyxDiagramContent.FromData(const AData: TNyxDataValue): TNyxDiagramContent;
var
  LKind: TNyxText;
  LFit: Integer;
begin

  if AData.Kind = ndNull then
  begin
    Exit(NyxNoDiagramContent);
  end;

  if AData.Kind <> ndObject then
  begin
    raise EArgumentException.Create('Diagram content requires its closed object or explicit null');
  end;
  LKind := AData.Field('kind').AsText;

  if (LKind = 'image') and (AData.Count = 5) then
  begin
    LFit := AData.Field('fit').AsInteger;
    RequireFit(LFit);
    Result := NyxDiagramImage(TNyxResourceImageRef.FromData(AData.Field('image')))
      .Padded(AData.Field('padding').AsNumber).Fitted(TNyxImageFit(LFit))
      .CaptionSpace(AData.Field('captionSpace').AsNumber);
  end
  else if (LKind = 'component') and (AData.Count = 4) then
  begin
    Result := NyxDiagramComponent(NyxComponent(AData.Field('component').AsText))
      .Padded(AData.Field('padding').AsNumber).CaptionSpace(AData.Field('captionSpace').AsNumber);
  end
  else
  begin
    raise EArgumentException.Create('Unknown or malformed diagram content fields');
  end;
end;

end.
