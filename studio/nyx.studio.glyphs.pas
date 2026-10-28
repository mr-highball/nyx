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
unit nyx.studio.glyphs;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.images, nyx.resources, nyx.button.presentation;

type
  { Stable semantic roles in the generated atlas. Layout changes never infer a
    glyph from a button caption or font symbol; each command chooses its role. }
  TNyxStudioGlyph = (nsgUndo, nsgRedo, nsgPascal, nsgOpen, nsgSave, nsgOutputs,
    nsgAgents, nsgActions, nsgBuildView, nsgBuildApp, nsgDesktop, nsgPhone,
    nsgInteract, nsgTools, nsgExpand, nsgTheme,
    nsgNewProject, nsgSwitchProject, nsgDuplicate, nsgMoveUp,
    nsgMoveDown, nsgReusable, nsgDelete, nsgExport,
    nsgProperties, nsgEvents, nsgSearch, nsgAdd,
    nsgClose, nsgHelp, nsgApply, nsgRefresh);

function NyxStudioGlyphResource: TNyxResourceRef;
{ Validated immutable image data is initialized once; every shell document owns
  one resource definition and its buttons retain only a small typed selector. }
function NyxStudioGlyphAtlas: TNyxImageSource;
{ An independent membership snapshot of the pre-admitted glyph catalog. The
  shared startup catalog is never exposed for mutation; repeated shell builds
  import its immutable entries without decoding/formatting the atlas again. }
function NyxStudioGlyphResources: INyxResources;
function NyxStudioGlyphPresentation(AGlyph: TNyxStudioGlyph): TNyxButtonPresentation;

implementation

uses SysUtils;

{$I assets/glyphs/nyx.studio.glyphs.inc}
{$I assets/glyphs/nyx.studio.editglyphs.inc}
{$I assets/glyphs/nyx.studio.movedownglyph.inc}
{$I assets/glyphs/nyx.studio.deleteglyph.inc}

var
  GAtlas: TNyxImageSource;
  GResources: INyxResources;

function NyxStudioGlyphResource: TNyxResourceRef;
begin
  Result := NyxResourceRef('nyx.studio.glyph-atlas');
end;

function NyxStudioGlyphAtlas: TNyxImageSource;
begin
  Result := GAtlas;
end;

function NyxStudioGlyphResources: INyxResources;
begin
  Result := GResources.Clone;
end;

function NyxStudioGlyphPresentation(AGlyph: TNyxStudioGlyph): TNyxButtonPresentation;
var
  LResource: TNyxResourceRef;
  LCell: Integer;
begin

  if (Ord(AGlyph) < Ord(Low(TNyxStudioGlyph))) or
    (Ord(AGlyph) > Ord(High(TNyxStudioGlyph))) then
  begin
    raise EArgumentException.Create('Unknown Studio glyph role');
  end;
  LResource := NyxStudioGlyphResource;
  LCell := Ord(AGlyph);

  if AGlyph in [nsgMoveDown, nsgDelete] then
  begin
    { The sheet's preceding documents cross these cell boundaries. Dedicated
      immutable images keep the semantic roles/ordinals and remove neighboring
      pixels without target-specific cropping or changing the source atlas. }

    if AGlyph = nsgMoveDown then
    begin
      LResource := NyxResourceRef('nyx.studio.move-down-glyph');
    end
    else
    begin
      LResource := NyxResourceRef('nyx.studio.delete-glyph');
    end;
    Exit(NyxButtonPresentation.Glyph(NyxResourceImage(LResource))
      .Content(nbcGlyphCaption).Density(nbdCompact).FontWeight(nbwSemibold));
  end;

  if LCell >= Ord(nsgNewProject) then
  begin
    LResource := NyxResourceRef('nyx.studio.edit-glyph-atlas');
    Dec(LCell, Ord(nsgNewProject));
  end;
  Result := NyxButtonPresentation.Glyph(NyxResourceImage(LResource))
    .Cell(NyxGlyphCell(4, 4, LCell mod 4, LCell div 4))
    .Content(nbcGlyphCaption).Density(nbdCompact).FontWeight(nbwSemibold);
end;

initialization
  { Initialization runs before application worker threads exist. Immutable
    resource reads therefore need no lazy mutable cache or competing writes. }
  GAtlas := NyxEmbeddedImage(nimPNG, CStudioGlyphAtlas);
  GResources := NewNyxResources;
  GResources.Define(NyxStudioGlyphResource,
    NyxImageResource(GAtlas).Describe('Studio glyphs',
      'A locally embedded family of decorative command glyphs.'));
  GResources.Define(NyxResourceRef('nyx.studio.edit-glyph-atlas'),
    NyxImageResource(NyxEmbeddedImage(nimPNG, CStudioEditGlyphAtlas)).Describe(
      'Studio editing glyphs', 'Project, selection and Inspector command glyphs.'));
  GResources.Define(NyxResourceRef('nyx.studio.move-down-glyph'),
    NyxImageResource(NyxEmbeddedImage(nimPNG, CStudioMoveDownGlyph)).Describe(
      'Move down glyph', 'An isolated downward arrow with transparent padding.'));
  GResources.Define(NyxResourceRef('nyx.studio.delete-glyph'),
    NyxImageResource(NyxEmbeddedImage(nimPNG, CStudioDeleteGlyph)).Describe(
      'Delete glyph', 'An isolated trash can with transparent padding.'));

{$ifndef PAS2JS}
finalization
  GResources := nil;
{$endif}
  { pas2js has no unit finalization. Its immutable startup values belong to the
    browser realm; each shell still owns independently releasable membership. }

end.
