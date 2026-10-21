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
program nyx_button_presentation_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.images, nyx.resources, nyx.button.presentation
  {$ifdef PAS2JS}, Web{$endif};

const
  { The same admitted 100-by-50 PNG used by the existing resource consumers.
    These checks admit bytes/metadata; they deliberately invoke no decoder. }
  CPNG: TNyxText =
    'iVBORw0KGgoAAAANSUhEUgAAAGQAAAAyEAIAAAB1xzWqAAAACXBIWXMAAAAAAAAAAACdYiYy' +
    'AAABMElEQVR4nO3OsQ0AIAzAsP7/dOEEtsgSGTxnduf2fbE/gMwHIPsDyHwAsj+AzAcg+wPI' +
    'fACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsD' +
    'yHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7' +
    'A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg' +
    '+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwH' +
    'IPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDxwO6T+sr8laFkAAAAABJRU5E' +
    'rkJggg==';
  CDefaults: TNyxText = '{"version":1,"glyph":{"kind":"none","value":null},' +
    '"cell":{"columns":1,"rows":1,"column":0,"row":0},"glyphSize":28,"glyphGap":6,' +
    '"content":"caption","density":"normal","alignment":"center",' +
    '"fontWeight":"inherit","glyphTone":"original"}';

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Button presentation: ' + AReason);
  end;
  Inc(GChecks);
end;

{ Replace only a copied wire field. This is an adversarial serialization input,
  never an implementation helper for the typed presentation under test. }
function Replaced(const AData: TNyxDataValue; const AKey: TNyxText;
  const AValue: TNyxDataValue): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin
  SetLength(LFields, AData.Count);
  for LIndex := 0 to AData.Count - 1 do
  begin
    LFields[LIndex] := NyxField(AData.Key(LIndex), AData.Field(AData.Key(LIndex)));

    if AData.Key(LIndex) = AKey then
    begin
      LFields[LIndex] := NyxField(AKey, AValue);
    end;
  end;
  Result := NyxObject(LFields);
end;

{ A failed reader assignment must preserve the caller's independently authored
  value. Every malformed case checks both refusal and that destination. }
procedure RefuseData(const AData: TNyxDataValue; const AReason: TNyxText);
var
  LValue: TNyxButtonPresentation;
  LBefore: TNyxText;
  LRejected: Boolean;
begin
  LValue := NyxButtonPresentation.GlyphGap(0).Density(nbdTouch);
  LBefore := LValue.ToData.ToJSON;
  LRejected := False;
  try
    LValue := TNyxButtonPresentation.FromData(AData);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, AReason);
  Check(LValue.ToData.ToJSON = LBefore, 'refused reader retains the previous copied value');
end;

procedure DefaultAndCopyCases;
var
  LDefault: TNyxButtonPresentation;
  LChanged: TNyxButtonPresentation;
  LCopy: TNyxButtonPresentation;
  LData: TNyxDataValue;
  LCell: TNyxGlyphCell;
  LBounds: TNyxGlyphBounds;
begin
  LDefault := Default(TNyxButtonPresentation);
  LDefault.Validate;
  Check((LDefault.ContentMode = nbcCaption) and (LDefault.ControlDensity = nbdNormal) and
    (LDefault.ContentAlignment = nbaCenter) and (LDefault.Weight = nbwInherit) and
    (LDefault.Tone = ngtOriginal), 'a default value preserves the legacy caption face');
  Check(not LDefault.HasGlyph and (LDefault.GlyphKind = nbgNone) and
    (LDefault.GlyphBoxSize = 28) and (LDefault.GlyphSpacing = 6),
    'default image absence and logical metrics are explicit');
  Check(LDefault.ToData.ToJSON = TNyxDataValue.ParseJSON(CDefaults).ToJSON,
    'canonical version-one default wire has the independent expected fields and choices');
  Check(LDefault.ToData.ToJSON = NyxButtonPresentation.ToData.ToJSON,
    'default and factory values have the same canonical meaning');
  LCell := Default(TNyxGlyphCell);
  Check((LCell.Columns = 1) and (LCell.Rows = 1) and
    (LCell.Column = 0) and (LCell.Row = 0), 'default cell covers the whole image');
  LBounds := LCell.PixelBounds(1254, 1254);
  Check((LBounds.Left = 0) and (LBounds.Top = 0) and (LBounds.Right = 1254) and
    (LBounds.Bottom = 1254), 'whole-image defaults use actual decoded dimensions');
  LChanged := LDefault.GlyphGap(0).GlyphSize(20).Density(nbdCompact).
    Alignment(nbaStart).FontWeight(nbwRegular).GlyphTone(ngtForeground);
  Check((LChanged.GlyphSpacing = 0) and (LChanged.GlyphBoxSize = 20) and
    (LChanged.ControlDensity = nbdCompact) and (LChanged.ContentAlignment = nbaStart) and
    (LChanged.Weight = nbwRegular) and (LChanged.Tone = ngtForeground),
    'typed presentation changes preserve an explicit zero gap');
  Check((LDefault.GlyphSpacing = 6) and (LDefault.GlyphBoxSize = 28) and
    (LDefault.ControlDensity = nbdNormal), 'fluent changes never edit the original');
  LCopy := LChanged.Copy;
  LChanged := LChanged.GlyphGap(17).FontWeight(nbwSemibold);
  Check((LCopy.GlyphSpacing = 0) and (LCopy.Weight = nbwRegular),
    'an independently copied presentation retains its original fields');
  LData := LCopy.ToData;
  LChanged := TNyxButtonPresentation.FromData(LData);
  Check((LChanged.ToData.ToJSON = LCopy.ToData.ToJSON) and (LChanged.GlyphSpacing = 0),
    'canonical round trip preserves zero gap without default replacement');
  LData := Replaced(LData, 'glyphGap', NyxData(19));
  LChanged := TNyxButtonPresentation.FromData(LData);
  Check((LChanged.GlyphSpacing = 19) and (LCopy.GlyphSpacing = 0),
    'detached wire proposals cannot edit retained typed values');
end;

procedure AtlasCases;
const
  { Actual chosen atlas extent is 1254, not the preview's displayed 1280.
    These independent endpoints qualify the proportional contract on both hosts. }
  CEdges: array[0..4] of Integer = (0, 313, 627, 940, 1254);
var
  LColumn: Integer;
  LRow: Integer;
  LArea: Integer;
  LCell: TNyxGlyphCell;
  LCopy: TNyxGlyphCell;
  LBounds: TNyxGlyphBounds;
begin
  LArea := 0;
  for LRow := 0 to 3 do
  begin
    for LColumn := 0 to 3 do
    begin
      LCell := NyxGlyphCell(4, 4, LColumn, LRow);
      LBounds := LCell.PixelBounds(1254, 1254);
      Check((LBounds.Left = CEdges[LColumn]) and (LBounds.Right = CEdges[LColumn + 1]) and
        (LBounds.Top = CEdges[LRow]) and (LBounds.Bottom = CEdges[LRow + 1]),
        'every odd atlas cell uses shared floor endpoints without gaps or overlap');
      LArea := LArea + LBounds.Width * LBounds.Height;
      LCopy := TNyxGlyphCell.FromData(LCell.ToData);
      Check((LCopy.Columns = 4) and (LCopy.Rows = 4) and
        (LCopy.Column = LColumn) and (LCopy.Row = LRow),
        'atlas coordinate serialization preserves the selected logical cell');
    end;
  end;
  Check(LArea = 1254 * 1254, 'all sixteen source cells cover the atlas exactly once');
  LCell := NyxGlyphCell(4, 4, 1, 2);
  LCopy := LCell.Copy;
  LCell := NyxGlyphCell(4, 4, 3, 3);
  Check((LCopy.Column = 1) and (LCopy.Row = 2), 'copied atlas cell does not share mutable state');
  LBounds := LCopy.PixelBounds(1254, 731);
  Check((LBounds.Width = 314) and (LBounds.Top = 365) and (LBounds.Bottom = 548),
    'source cell preserves a nonsquare atlas proportion rather than stretching source bounds');
  LBounds := LCell.PixelBounds(High(Integer), High(Integer));
  Check((LBounds.Right = High(Integer)) and (LBounds.Bottom = High(Integer)),
    'endpoint arithmetic converts before multiplying and cannot wrap a 32-bit intermediate');
end;

procedure SourceCases;
var
  LImage: TNyxImageSource;
  LLocation: TNyxImageSource;
  LRef: TNyxResourceImageRef;
  LValue: TNyxButtonPresentation;
  LCopy: TNyxButtonPresentation;
  LBytes: TNyxImageBytes;
  LBefore: TNyxText;
begin
  LImage := NyxEmbeddedImage(nimPNG, CPNG);
  Check((LImage.Width = 100) and (LImage.Height = 50),
    'inline source is the complete existing admitted fixture, without pixel decoding');
  LValue := NyxButtonPresentation.Glyph(LImage).Cell(NyxGlyphCell(4, 4, 2, 3)).
    GlyphGap(0).Content(nbcGlyphCaption);
  LCopy := TNyxButtonPresentation.FromData(LValue.ToData);
  Check((LCopy.GlyphKind = nbgImage) and LCopy.HasGlyph and
    (LCopy.ImageSource.ToWire = LImage.ToWire) and (LCopy.GlyphCell.Column = 2) and
    (LCopy.GlyphCell.Row = 3) and (LCopy.ContentMode = nbcGlyphCaption),
    'inline image source, atlas cell and caption mode survive the typed wire boundary');
  LBytes := LCopy.ImageSource.Bytes;
  LBytes[0] := 0;
  Check(LCopy.ImageSource.Bytes[0] = 137,
    'a caller-owned byte read cannot alter the retained image source');
  LValue := LValue.Content(nbcGlyphOnly).GlyphTone(ngtForeground);
  Check((LValue.ContentMode = nbcGlyphOnly) and (LValue.Tone = ngtForeground) and
    (LValue.ImageSource.ToWire = LCopy.ImageSource.ToWire),
    'theme ink changes presentation only and never authors a replacement bitmap');
  LLocation := NyxImage(NyxImageLocation('https://glyphs.invalid/not-requested.png'));
  LValue := NyxButtonPresentation.Glyph(LLocation).Cell(NyxGlyphCell(4, 4, 3, 3));
  Check((LValue.ImageSource.Kind = nisLocation) and
    (TNyxButtonPresentation.FromData(LValue.ToData).ImageSource.ToWire = LLocation.ToWire),
    'location admission stores a reference without issuing or requiring transport');
  LRef := NyxResourceImage(NyxResourceRef('studio/atlas 🌙')).
    Localize(NyxLocale('en-GB'), NyxLocale('en'));
  LValue := NyxButtonPresentation.Glyph(LRef).Cell(NyxGlyphCell(4, 4, 0, 1)).
    Content(nbcGlyphOnly).Density(nbdTouch).GlyphTone(ngtForeground);
  LBefore := LValue.ToData.ToJSON;
  LCopy := TNyxButtonPresentation.FromData(LValue.ToData);
  LRef := NyxResourceImage(NyxResourceRef('another/open-name'));
  Check((LCopy.GlyphKind = nbgResource) and
    (LCopy.ResourceImage.Reference.Name = TNyxText('studio/atlas 🌙')) and
    (LCopy.ResourceImage.Locale.Name = 'en-GB') and
    (LCopy.ResourceImage.Fallback.Name = 'en') and LCopy.ResourceImage.Localized,
    'resource source keeps exact open names and locale selectors in an independent copy');
  Check(LCopy.ToData.ToJSON = LBefore,
    'resource round trip is canonical and independent of a later caller selector');
  LValue := NyxButtonPresentation.Glyph(LRef);
  Check(not LValue.ResourceImage.Localized and LValue.HasGlyph,
    'inherited resource locale needs no catalog, cached bytes or loader');
  LValue := LValue.Glyph(NyxNoImage);
  Check((LValue.GlyphKind = nbgNone) and not LValue.HasGlyph,
    'explicit empty image clears a source without retaining the previous selector');
end;

procedure InvalidFluentCases;
var
  LIndex: Integer;
  LInvalid: Integer;
  LValue: TNyxButtonPresentation;
  LBefore: TNyxText;
  LRejected: Boolean;
begin
  LInvalid := 99;
  for LIndex := 0 to 20 do
  begin
    LValue := NyxButtonPresentation.GlyphGap(0);
    LBefore := LValue.ToData.ToJSON;
    LRejected := False;
    try
      case LIndex of
        0: LValue := LValue.GlyphSize(0);
        1: LValue := LValue.GlyphSize(NyxMaximumButtonGlyphSize + 1);
        2: LValue := LValue.GlyphGap(-1);
        3: LValue := LValue.GlyphGap(NyxMaximumButtonGlyphGap + 1);
        4: LValue := LValue.Content(nbcGlyphOnly);
        5: LValue := LValue.Content(TNyxButtonContent(LInvalid));
        6: LValue := LValue.Density(TNyxButtonDensity(LInvalid));
        7: LValue := LValue.Alignment(TNyxButtonContentAlignment(LInvalid));
        8: LValue := LValue.FontWeight(TNyxButtonFontWeight(LInvalid));
        9: LValue := LValue.GlyphTone(TNyxGlyphTone(LInvalid));
        10: NyxGlyphCell(0, 4, 0, 0);
        11: NyxGlyphCell(4, 0, 0, 0);
        12: NyxGlyphCell(4, 4, -1, 0);
        13: NyxGlyphCell(4, 4, 0, -1);
        14: NyxGlyphCell(4, 4, 4, 0);
        15: NyxGlyphCell(NyxImageMaximumDimension + 1, 4, 0, 0);
        16: NyxGlyphCell(4, 4, 0, 0).PixelBounds(0, 1254);
        17: NyxGlyphCell(4, 4, 0, 0).PixelBounds(1254, -1);
        18: NyxGlyphCell(4, 4, 0, 0).PixelBounds(1, 1);
        19: LValue := LValue.Glyph(NyxEmbeddedImage(nimPNG, CPNG)).
          Cell(NyxGlyphCell(128, 128, 0, 0));
        20: LValue := LValue.Glyph(Default(TNyxResourceImageRef));
      end;
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'invalid fluent value/cast/grid/dimensions refuses case ' + IntToStr(LIndex));
    Check(LValue.ToData.ToJSON = LBefore, 'a rejected fluent assignment cannot change its receiver');
  end;
end;

procedure InvalidDataCases;
var
  LData: TNyxDataValue;
  LGlyph: TNyxDataValue;
  LCell: TNyxDataValue;
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin
  RefuseData(Default(TNyxDataValue), 'undefined wire is not a default declaration');
  RefuseData(NyxNull, 'null wire is not a versioned presentation');
  RefuseData(NyxArray([]), 'array wire is not a presentation object');
  LData := NyxButtonPresentation.ToData;
  RefuseData(Replaced(LData, 'version', NyxData(2)), 'unknown version refuses');
  RefuseData(Replaced(LData, 'version', NyxData('1')), 'version must be a typed integer');
  RefuseData(Replaced(LData, 'glyphSize', NyxData(0)), 'zero glyph box refuses');
  RefuseData(Replaced(LData, 'glyphGap', NyxData(-1)), 'negative glyph gap refuses');
  RefuseData(Replaced(LData, 'glyphGap', NyxData(1.5)), 'fractional glyph gap refuses');
  RefuseData(Replaced(LData, 'content', NyxData('icon')), 'unknown content choice refuses');
  RefuseData(Replaced(LData, 'content', NyxData('glyph-only')), 'glyph-only needs a declared source');
  RefuseData(Replaced(LData, 'density', NyxData('Touch')), 'closed enum spelling is exact');
  RefuseData(Replaced(LData, 'alignment', NyxData('left')), 'alignment is semantic start or center');
  RefuseData(Replaced(LData, 'fontWeight', NyxData('bold')), 'unsupported font weight refuses');
  RefuseData(Replaced(LData, 'glyphTone', NyxData('white')), 'ink tone is not an authored tint string');
  LGlyph := NyxObject([NyxField('kind', NyxData('sprite')), NyxField('value', NyxNull)]);
  RefuseData(Replaced(LData, 'glyph', LGlyph), 'unknown glyph source family refuses');
  LGlyph := NyxObject([NyxField('kind', NyxData('none')), NyxField('value', NyxData('asset'))]);
  RefuseData(Replaced(LData, 'glyph', LGlyph), 'absent glyph value must be explicit null');
  LGlyph := NyxObject([NyxField('kind', NyxData('image')), NyxField('value', NyxData(''))]);
  RefuseData(Replaced(LData, 'glyph', LGlyph), 'image family cannot hide an empty image');
  LGlyph := Replaced(LGlyph, 'value', NyxData(True));
  RefuseData(Replaced(LData, 'glyph', LGlyph), 'image wire requires text');
  LGlyph := Replaced(LGlyph, 'value', NyxData('data:image/png;base64,YQ=='));
  RefuseData(Replaced(LData, 'glyph', LGlyph), 'inline corrupt raster metadata refuses');
  LGlyph := NyxObject([NyxField('kind', NyxData('resource')),
    NyxField('value', NyxObject([]))]);
  RefuseData(Replaced(LData, 'glyph', LGlyph), 'resource source requires its strict selector');
  LCell := NyxObject([NyxField('columns', NyxData(4)), NyxField('rows', NyxData(4)),
    NyxField('column', NyxData(0)), NyxField('foreign', NyxData(0))]);
  RefuseData(Replaced(LData, 'cell', LCell), 'foreign cell key cannot replace a required key');
  LCell := Replaced(NyxGlyphCell(4, 4, 0, 0).ToData, 'column', NyxData(4));
  RefuseData(Replaced(LData, 'cell', LCell), 'wire cell index must belong to its grid');
  LCell := Replaced(NyxGlyphCell(4, 4, 0, 0).ToData, 'rows', NyxData(2.5));
  RefuseData(Replaced(LData, 'cell', LCell), 'wire grid dimensions require exact integers');
  SetLength(LFields, LData.Count - 1);
  for LIndex := 0 to High(LFields) do
  begin
    LFields[LIndex] := NyxField(LData.Key(LIndex), LData.Field(LData.Key(LIndex)));
  end;
  RefuseData(NyxObject(LFields), 'missing top-level field cannot inherit a silent default');
  SetLength(LFields, LData.Count);
  for LIndex := 0 to High(LFields) do
  begin
    LFields[LIndex] := NyxField(LData.Key(LIndex), LData.Field(LData.Key(LIndex)));
  end;
  LFields[High(LFields)] := NyxField('foreign', NyxData('original'));
  RefuseData(NyxObject(LFields), 'foreign top-level key cannot replace a required key');
end;

procedure EnumCases;
var
  LContent: TNyxButtonContent;
  LDensity: TNyxButtonDensity;
  LAlignment: TNyxButtonContentAlignment;
  LWeight: TNyxButtonFontWeight;
  LTone: TNyxGlyphTone;
begin
  for LContent := Low(TNyxButtonContent) to High(TNyxButtonContent) do
  begin
    Check(ReadNyxButtonContent(NyxButtonContentName(LContent)) = LContent,
      'content symbols round trip through the closed reader');
  end;
  for LDensity := Low(TNyxButtonDensity) to High(TNyxButtonDensity) do
  begin
    Check(ReadNyxButtonDensity(NyxButtonDensityName(LDensity)) = LDensity,
      'density symbols round trip through the closed reader');
  end;
  for LAlignment := Low(TNyxButtonContentAlignment) to High(TNyxButtonContentAlignment) do
  begin
    Check(ReadNyxButtonAlignment(NyxButtonAlignmentName(LAlignment)) = LAlignment,
      'alignment symbols round trip through the closed reader');
  end;
  for LWeight := Low(TNyxButtonFontWeight) to High(TNyxButtonFontWeight) do
  begin
    Check(ReadNyxButtonFontWeight(NyxButtonFontWeightName(LWeight)) = LWeight,
      'weight symbols round trip through the closed reader');
  end;
  for LTone := Low(TNyxGlyphTone) to High(TNyxGlyphTone) do
  begin
    Check(ReadNyxGlyphTone(NyxGlyphToneName(LTone)) = LTone,
      'ink symbols round trip through the closed reader');
  end;
end;

begin
  try
    DefaultAndCopyCases;
    AtlasCases;
    SourceCases;
    InvalidFluentCases;
    InvalidDataCases;
    EnumCases;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-button-presentation', 'passed');
    document.body.setAttribute('data-button-presentation-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' typed button presentation checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-button-presentation', 'failed');
      document.body.setAttribute('data-button-presentation-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message, ' at check count ', GChecks);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
