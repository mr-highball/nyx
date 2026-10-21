{ Nyx. Copyright (c) 2020 mr-highball. MIT license; see LICENSE. }
unit nyx.studio.glyphs;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.images, nyx.resources, nyx.button.presentation;

type
  { Stable semantic roles in the generated atlas. Layout changes never infer a
    glyph from a button caption or font symbol; each command chooses its role. }
  TNyxStudioGlyph = (nsgUndo, nsgRedo, nsgPascal, nsgOpen, nsgSave, nsgOutputs,
    nsgAgents, nsgActions, nsgBuildView, nsgBuildApp, nsgDesktop, nsgPhone,
    nsgInteract, nsgTools, nsgExpand, nsgTheme);

function NyxStudioGlyphResource: TNyxResourceRef;
{ Validated immutable image data is initialized once; every shell document owns
  one resource definition and its buttons retain only a small typed selector. }
function NyxStudioGlyphAtlas: TNyxImageSource;
function NyxStudioGlyphPresentation(AGlyph: TNyxStudioGlyph): TNyxButtonPresentation;

implementation

uses SysUtils;

{$I assets/glyphs/nyx.studio.glyphs.inc}

var
  GAtlas: TNyxImageSource;

function NyxStudioGlyphResource: TNyxResourceRef;
begin
  Result := NyxResourceRef('nyx.studio.glyph-atlas');
end;

function NyxStudioGlyphAtlas: TNyxImageSource;
begin
  Result := GAtlas;
end;

function NyxStudioGlyphPresentation(AGlyph: TNyxStudioGlyph): TNyxButtonPresentation;
begin

  if (Ord(AGlyph) < Ord(Low(TNyxStudioGlyph))) or
    (Ord(AGlyph) > Ord(High(TNyxStudioGlyph))) then
  begin
    raise EArgumentException.Create('Unknown Studio glyph role');
  end;
  Result := NyxButtonPresentation.Glyph(NyxResourceImage(NyxStudioGlyphResource))
    .Cell(NyxGlyphCell(4, 4, Ord(AGlyph) mod 4, Ord(AGlyph) div 4))
    .Content(nbcGlyphCaption).Density(nbdCompact).FontWeight(nbwSemibold);
end;

initialization
  { Initialization runs before application worker threads exist. Immutable
    resource reads therefore need no lazy mutable cache or competing writes. }
  GAtlas := NyxEmbeddedImage(nimPNG, CStudioGlyphAtlas);

end.
