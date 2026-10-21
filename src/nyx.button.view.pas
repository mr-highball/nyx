{ Nyx. Copyright (c) 2020 mr-highball. MIT license; see LICENSE. }
unit nyx.button.view;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.model, nyx.images, nyx.button.presentation;

{ Read a copied effective presentation from an authored or realized descriptor.
  An empty override restores the ordinary caption face. No mutable state is kept. }
function NyxButtonPresentationOf(ANode: TNyxNode): TNyxButtonPresentation;
{ Resolve through the same realized resource catalog as ordinary Image bindings.
  A valid hosted resource with no published bytes returns NyxNoImage, preserving
  the command's caption while the existing application resource owner loads it. }
function NyxButtonGlyphSource(ANode: TNyxNode;
  const APresentation: TNyxButtonPresentation): TNyxImageSource;

implementation

uses SysUtils, nyx.types, nyx.data;

function NyxButtonPresentationOf(ANode: TNyxNode): TNyxButtonPresentation;
begin
  Result := NyxButtonPresentation;

  if ANode = nil then
  begin
    raise EArgumentException.Create('Button presentation requires a descriptor');
  end;

  if ANode.Prop(NyxAttributeName(atButtonPresentation)) <> '' then
  begin
    Result := TNyxButtonPresentation.FromData(
      TNyxDataValue.ParseJSON(ANode.Prop(NyxAttributeName(atButtonPresentation))));
  end;
end;

function NyxButtonGlyphSource(ANode: TNyxNode;
  const APresentation: TNyxButtonPresentation): TNyxImageSource;
begin
  Result := NyxNoImage;
  case APresentation.GlyphKind of
    nbgNone:
      begin
        Exit;
      end;
    nbgImage:
      begin
        Result := APresentation.ImageSource;
      end;
    nbgResource:
      begin
        ANode.TryReadResourceImage(APresentation.ResourceImage, Result);
      end;
  end;
end;

end.
