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
unit nyx.button.presentation;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, Math, nyx.text, nyx.data, nyx.images, nyx.resources;

const
  NyxMaximumButtonGlyphSize = 256;
  NyxMaximumButtonGlyphGap = 256;

type
  { Invalid presentation data has no model, renderer or transport dependency. }
  ENyxButtonPresentation = class(Exception);
  { A glyph is absent, a copied ordinary image, or a copied named image selector.
    A resource selector never starts loading or grants host authority. }
  TNyxButtonGlyphSourceKind = (nbgNone, nbgImage, nbgResource);
  { Caption remains command text even when a renderer hides it visually. }
  TNyxButtonContent = (nbcCaption, nbcGlyphCaption, nbcGlyphOnly);
  { Logical density is independent of image pixels and host DPI. }
  TNyxButtonDensity = (nbdNormal, nbdCompact, nbdTouch);
  { Align the complete glyph/caption group within the existing button. }
  TNyxButtonContentAlignment = (nbaCenter, nbaStart);
  { Inherit preserves the owning button/theme's established font treatment. }
  TNyxButtonFontWeight = (nbwInherit, nbwRegular, nbwSemibold);
  { Foreground uses original alpha as an ink mask; no second bitmap is authored. }
  TNyxGlyphTone = (ngtOriginal, ngtForeground);

  { Half-open decoded pixel endpoints. Shared edges are computed independently
    from the actual source extent, so a nondivisible atlas has no gaps/overlap. }
  TNyxGlyphBounds = record
  private
    FLeft: Integer;
    FTop: Integer;
    FRight: Integer;
    FBottom: Integer;
    function GetWidth: Integer;
    function GetHeight: Integer;
  public
    property Left: Integer read FLeft;
    property Top: Integer read FTop;
    property Right: Integer read FRight;
    property Bottom: Integer read FBottom;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
  end;

  { Immutable proportional atlas cell. Default is the whole image, not an
    uninitialized zero grid. No decoded image, crop or platform object is held. }
  TNyxGlyphCell = record
  private
    FMarker: TNyxText;
    FColumns: Integer;
    FRows: Integer;
    FColumn: Integer;
    FRow: Integer;
    function GetColumns: Integer;
    function GetRows: Integer;
    function GetColumn: Integer;
    function GetRow: Integer;
  public
    function Copy: TNyxGlyphCell;
    procedure Validate;
    function PixelBounds(AWidth, AHeight: Integer): TNyxGlyphBounds;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxGlyphCell; static;
    property Columns: Integer read GetColumns;
    property Rows: Integer read GetRows;
    property Column: Integer read GetColumn;
    property Row: Integer read GetRow;
  end;

  { Immutable portable button face. Defaults preserve the legacy caption face;
    initialized zero gap is distinct from the default six logical pixels.
    Glyph chooses its source only: Content explicitly chooses its presentation.
    Resource admission here validates selector shape without catalog access or
    loading. Node admission owns kind, accessible-name and document references. }
  TNyxButtonPresentation = record
  private
    FMarker: TNyxText;
    FGlyphKind: TNyxButtonGlyphSourceKind;
    FImage: TNyxImageSource;
    FResource: TNyxResourceImageRef;
    FCell: TNyxGlyphCell;
    FGlyphSize: Integer;
    FGlyphGap: Integer;
    FContent: TNyxButtonContent;
    FDensity: TNyxButtonDensity;
    FAlignment: TNyxButtonContentAlignment;
    FWeight: TNyxButtonFontWeight;
    FTone: TNyxGlyphTone;
    function Normalized: TNyxButtonPresentation;
    function GetGlyphKind: TNyxButtonGlyphSourceKind;
    function GetHasGlyph: Boolean;
    function GetImage: TNyxImageSource;
    function GetResource: TNyxResourceImageRef;
    function GetCell: TNyxGlyphCell;
    function GetGlyphSize: Integer;
    function GetGlyphGap: Integer;
    function GetContent: TNyxButtonContent;
    function GetDensity: TNyxButtonDensity;
    function GetAlignment: TNyxButtonContentAlignment;
    function GetWeight: TNyxButtonFontWeight;
    function GetTone: TNyxGlyphTone;
  public
    function Glyph(const ASource: TNyxImageSource): TNyxButtonPresentation; overload;
    function Glyph(const AResource: TNyxResourceImageRef): TNyxButtonPresentation; overload;
    function Cell(const ACell: TNyxGlyphCell): TNyxButtonPresentation;
    function GlyphSize(AValue: Integer): TNyxButtonPresentation;
    function GlyphGap(AValue: Integer): TNyxButtonPresentation;
    function Content(AValue: TNyxButtonContent): TNyxButtonPresentation;
    function Density(AValue: TNyxButtonDensity): TNyxButtonPresentation;
    function Alignment(AValue: TNyxButtonContentAlignment): TNyxButtonPresentation;
    function FontWeight(AValue: TNyxButtonFontWeight): TNyxButtonPresentation;
    function GlyphTone(AValue: TNyxGlyphTone): TNyxButtonPresentation;
    function Copy: TNyxButtonPresentation;
    procedure Validate;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxButtonPresentation; static;
    property GlyphKind: TNyxButtonGlyphSourceKind read GetGlyphKind;
    property HasGlyph: Boolean read GetHasGlyph;
    property ImageSource: TNyxImageSource read GetImage;
    property ResourceImage: TNyxResourceImageRef read GetResource;
    property GlyphCell: TNyxGlyphCell read GetCell;
    property GlyphBoxSize: Integer read GetGlyphSize;
    property GlyphSpacing: Integer read GetGlyphGap;
    property ContentMode: TNyxButtonContent read GetContent;
    property ControlDensity: TNyxButtonDensity read GetDensity;
    property ContentAlignment: TNyxButtonContentAlignment read GetAlignment;
    property Weight: TNyxButtonFontWeight read GetWeight;
    property Tone: TNyxGlyphTone read GetTone;
  end;

{ Whole is the default cell. Explicit grids use zero-based column/row indexes. }
function NyxGlyphCell(AColumns, ARows, AColumn, ARow: Integer): TNyxGlyphCell;
{ Complete immutable defaults. Builders return an admitted independent value. }
function NyxButtonPresentation: TNyxButtonPresentation;
{ Exact closed persistence names; readers never infer meaning from captions. }
function NyxButtonContentName(AValue: TNyxButtonContent): TNyxText;
function ReadNyxButtonContent(const AName: TNyxText): TNyxButtonContent;
function NyxButtonDensityName(AValue: TNyxButtonDensity): TNyxText;
function ReadNyxButtonDensity(const AName: TNyxText): TNyxButtonDensity;
function NyxButtonAlignmentName(AValue: TNyxButtonContentAlignment): TNyxText;
function ReadNyxButtonAlignment(const AName: TNyxText): TNyxButtonContentAlignment;
function NyxButtonFontWeightName(AValue: TNyxButtonFontWeight): TNyxText;
function ReadNyxButtonFontWeight(const AName: TNyxText): TNyxButtonFontWeight;
function NyxGlyphToneName(AValue: TNyxGlyphTone): TNyxText;
function ReadNyxGlyphTone(const AName: TNyxText): TNyxGlyphTone;

implementation

const
  CCellMarker = 'nyx.glyph-cell';
  CPresentationMarker = 'nyx.button-presentation';
  CContents: array[TNyxButtonContent] of TNyxText =
    ('caption', 'glyph-caption', 'glyph-only');
  CDensities: array[TNyxButtonDensity] of TNyxText = ('normal', 'compact', 'touch');
  CAlignments: array[TNyxButtonContentAlignment] of TNyxText = ('center', 'start');
  CWeights: array[TNyxButtonFontWeight] of TNyxText = ('inherit', 'regular', 'semibold');
  CTones: array[TNyxGlyphTone] of TNyxText = ('original', 'foreground');

{ Bound every ordinal before indexing, preserving defensive invalid-cast
  refusal without an unreachable exhaustive-enum case on either compiler. }
function EnumName(AValue: Integer; const ANames: array of TNyxText): TNyxText;
begin

  if (AValue < 0) or (AValue >= Length(ANames)) then
  begin
    raise ENyxButtonPresentation.Create('Unknown button presentation choice');
  end;
  Result := ANames[AValue];
end;

function ReadEnum(const AName: TNyxText; const ANames: array of TNyxText): Integer;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(ANames) do
  begin

    if AName = ANames[LIndex] then
    begin
      Exit(LIndex);
    end;
  end;
  raise ENyxButtonPresentation.Create('Unknown button presentation choice: ' + AName);
end;

{ Check the full key set before reading values. Count alone cannot distinguish
  a required field replaced by a foreign field; object order is normalized. }
procedure RequireKeys(const AData: TNyxDataValue; const AKeys: array of TNyxText);
var
  LIndex: Integer;
  LKeyIndex: Integer;
  LFound: Boolean;
begin

  if not AData.Defined or (AData.Kind <> ndObject) or
    (AData.Count <> Length(AKeys)) then
  begin
    raise ENyxButtonPresentation.Create('Button presentation requires its exact object shape');
  end;
  for LIndex := 0 to AData.Count - 1 do
  begin
    LFound := False;
    for LKeyIndex := 0 to High(AKeys) do
    begin

      if AData.Key(LIndex) = AKeys[LKeyIndex] then
      begin
        LFound := True;
        Break;
      end;
    end;

    if not LFound then
    begin
      raise ENyxButtonPresentation.Create('Unknown button presentation member: ' + AData.Key(LIndex));
    end;
  end;
end;

function NyxButtonContentName(AValue: TNyxButtonContent): TNyxText;
begin
  Result := EnumName(Ord(AValue), CContents);
end;

function ReadNyxButtonContent(const AName: TNyxText): TNyxButtonContent;
begin
  Result := TNyxButtonContent(ReadEnum(AName, CContents));
end;

function NyxButtonDensityName(AValue: TNyxButtonDensity): TNyxText;
begin
  Result := EnumName(Ord(AValue), CDensities);
end;

function ReadNyxButtonDensity(const AName: TNyxText): TNyxButtonDensity;
begin
  Result := TNyxButtonDensity(ReadEnum(AName, CDensities));
end;

function NyxButtonAlignmentName(AValue: TNyxButtonContentAlignment): TNyxText;
begin
  Result := EnumName(Ord(AValue), CAlignments);
end;

function ReadNyxButtonAlignment(const AName: TNyxText): TNyxButtonContentAlignment;
begin
  Result := TNyxButtonContentAlignment(ReadEnum(AName, CAlignments));
end;

function NyxButtonFontWeightName(AValue: TNyxButtonFontWeight): TNyxText;
begin
  Result := EnumName(Ord(AValue), CWeights);
end;

function ReadNyxButtonFontWeight(const AName: TNyxText): TNyxButtonFontWeight;
begin
  Result := TNyxButtonFontWeight(ReadEnum(AName, CWeights));
end;

function NyxGlyphToneName(AValue: TNyxGlyphTone): TNyxText;
begin
  Result := EnumName(Ord(AValue), CTones);
end;

function ReadNyxGlyphTone(const AName: TNyxText): TNyxGlyphTone;
begin
  Result := TNyxGlyphTone(ReadEnum(AName, CTones));
end;

function TNyxGlyphBounds.GetWidth: Integer;
begin
  Result := FRight - FLeft;
end;

function TNyxGlyphBounds.GetHeight: Integer;
begin
  Result := FBottom - FTop;
end;

function NyxGlyphCell(AColumns, ARows, AColumn, ARow: Integer): TNyxGlyphCell;
begin
  Result := Default(TNyxGlyphCell);
  Result.FMarker := CCellMarker;
  Result.FColumns := AColumns;
  Result.FRows := ARows;
  Result.FColumn := AColumn;
  Result.FRow := ARow;
  Result.Validate;
end;

function TNyxGlyphCell.GetColumns: Integer;
begin
  Result := 1;

  if FMarker <> '' then
  begin
    Result := FColumns;
  end;
end;

function TNyxGlyphCell.GetRows: Integer;
begin
  Result := 1;

  if FMarker <> '' then
  begin
    Result := FRows;
  end;
end;

function TNyxGlyphCell.GetColumn: Integer;
begin
  Result := 0;

  if FMarker <> '' then
  begin
    Result := FColumn;
  end;
end;

function TNyxGlyphCell.GetRow: Integer;
begin
  Result := 0;

  if FMarker <> '' then
  begin
    Result := FRow;
  end;
end;

procedure TNyxGlyphCell.Validate;
begin

  if ((FMarker <> '') and (FMarker <> CCellMarker)) or
    (Columns < 1) or (Rows < 1) or
    (Columns > NyxImageMaximumDimension) or (Rows > NyxImageMaximumDimension) or
    (Column < 0) or (Column >= Columns) or (Row < 0) or (Row >= Rows) then
  begin
    raise ENyxButtonPresentation.Create('Glyph cell requires a positive grid and in-range coordinates');
  end;
end;

function TNyxGlyphCell.Copy: TNyxGlyphCell;
begin
  Validate;
  Result := NyxGlyphCell(Columns, Rows, Column, Row);
end;

function TNyxGlyphCell.PixelBounds(AWidth, AHeight: Integer): TNyxGlyphBounds;
begin
  Validate;

  if (AWidth < 1) or (AHeight < 1) then
  begin
    raise ENyxButtonPresentation.Create('Glyph pixel bounds require positive decoded dimensions');
  end;
  { Convert before multiplication: even a caller's large decoded dimension
    cannot overflow a 32-bit intermediate or shift a shared atlas edge. }
  Result.FLeft := Floor(Double(Column) * AWidth / Columns);
  Result.FTop := Floor(Double(Row) * AHeight / Rows);
  Result.FRight := Floor(Double(Column + 1) * AWidth / Columns);
  Result.FBottom := Floor(Double(Row + 1) * AHeight / Rows);

  if (Result.Width < 1) or (Result.Height < 1) then
  begin
    raise ENyxButtonPresentation.Create('Glyph cell is empty at the decoded image dimensions');
  end;
end;

function TNyxGlyphCell.ToData: TNyxDataValue;
begin
  Validate;
  Result := NyxObject([NyxField('columns', NyxData(Columns)),
    NyxField('rows', NyxData(Rows)), NyxField('column', NyxData(Column)),
    NyxField('row', NyxData(Row))]);
end;

class function TNyxGlyphCell.FromData(const AData: TNyxDataValue): TNyxGlyphCell;
begin
  RequireKeys(AData, ['columns', 'rows', 'column', 'row']);
  Result := NyxGlyphCell(AData.Field('columns').AsInteger, AData.Field('rows').AsInteger,
    AData.Field('column').AsInteger, AData.Field('row').AsInteger);
end;

function NyxButtonPresentation: TNyxButtonPresentation;
begin
  Result := Default(TNyxButtonPresentation);
  Result.FMarker := CPresentationMarker;
  Result.FGlyphKind := nbgNone;
  Result.FCell := NyxGlyphCell(1, 1, 0, 0);
  Result.FGlyphSize := 28;
  Result.FGlyphGap := 6;
  Result.FContent := nbcCaption;
  Result.FDensity := nbdNormal;
  Result.FAlignment := nbaCenter;
  Result.FWeight := nbwInherit;
  Result.FTone := ngtOriginal;
end;

function TNyxButtonPresentation.Normalized: TNyxButtonPresentation;
begin

  if FMarker = '' then
  begin
    Exit(NyxButtonPresentation);
  end;

  if FMarker <> CPresentationMarker then
  begin
    raise ENyxButtonPresentation.Create('Unknown button presentation value');
  end;
  Result := Self;
end;

function TNyxButtonPresentation.GetGlyphKind: TNyxButtonGlyphSourceKind;
begin
  Result := Normalized.FGlyphKind;
end;

function TNyxButtonPresentation.GetHasGlyph: Boolean;
begin
  Result := GetGlyphKind <> nbgNone;
end;

function TNyxButtonPresentation.GetImage: TNyxImageSource;
begin

  if GetGlyphKind <> nbgImage then
  begin
    raise ENyxButtonPresentation.Create('Button glyph does not contain an inline image source');
  end;
  Result := FImage;
end;

function TNyxButtonPresentation.GetResource: TNyxResourceImageRef;
begin

  if GetGlyphKind <> nbgResource then
  begin
    raise ENyxButtonPresentation.Create('Button glyph does not contain an image resource selector');
  end;
  Result := FResource;
end;

function TNyxButtonPresentation.GetCell: TNyxGlyphCell;
begin
  Result := Normalized.FCell.Copy;
end;

function TNyxButtonPresentation.GetGlyphSize: Integer;
begin
  Result := Normalized.FGlyphSize;
end;

function TNyxButtonPresentation.GetGlyphGap: Integer;
begin
  Result := Normalized.FGlyphGap;
end;

function TNyxButtonPresentation.GetContent: TNyxButtonContent;
begin
  Result := Normalized.FContent;
end;

function TNyxButtonPresentation.GetDensity: TNyxButtonDensity;
begin
  Result := Normalized.FDensity;
end;

function TNyxButtonPresentation.GetAlignment: TNyxButtonContentAlignment;
begin
  Result := Normalized.FAlignment;
end;

function TNyxButtonPresentation.GetWeight: TNyxButtonFontWeight;
begin
  Result := Normalized.FWeight;
end;

function TNyxButtonPresentation.GetTone: TNyxGlyphTone;
begin
  Result := Normalized.FTone;
end;

function TNyxButtonPresentation.Copy: TNyxButtonPresentation;
begin
  Validate;
  Result := Normalized;
  Result.FCell := Result.FCell.Copy;
end;

procedure TNyxButtonPresentation.Validate;
var
  LValue: TNyxButtonPresentation;
begin
  LValue := Normalized;
  LValue.FCell.Validate;
  NyxButtonContentName(LValue.FContent);
  NyxButtonDensityName(LValue.FDensity);
  NyxButtonAlignmentName(LValue.FAlignment);
  NyxButtonFontWeightName(LValue.FWeight);
  NyxGlyphToneName(LValue.FTone);

  if (LValue.FGlyphSize < 1) or (LValue.FGlyphSize > NyxMaximumButtonGlyphSize) or
    (LValue.FGlyphGap < 0) or (LValue.FGlyphGap > NyxMaximumButtonGlyphGap) then
  begin
    raise ENyxButtonPresentation.Create('Button glyph size or gap exceeds its logical pixel bounds');
  end;
  case Ord(LValue.FGlyphKind) of
    Ord(nbgNone):
      begin

        if LValue.FContent = nbcGlyphOnly then
        begin
          raise ENyxButtonPresentation.Create('Glyph-only content requires a declared glyph');
        end;
      end;
    Ord(nbgImage):
      begin

        if LValue.FImage.Kind = nisEmpty then
        begin
          raise ENyxButtonPresentation.Create('Image glyph requires a nonempty image source');
        end;
        TNyxImageSource.FromWire(LValue.FImage.ToWire);

        if LValue.FImage.Kind = nisEmbedded then
        begin
          LValue.FCell.PixelBounds(LValue.FImage.Width, LValue.FImage.Height);
        end;
      end;
    Ord(nbgResource):
      begin
        { Strict selector round trip admits names/locales without looking up or
          retaining a catalog, issuing a request or reading hosted bytes. }
        TNyxResourceImageRef.FromData(LValue.FResource.ToData);
      end;
  else
    begin
      raise ENyxButtonPresentation.Create('Unknown button glyph source kind');
    end;
  end;
end;

function TNyxButtonPresentation.Glyph(const ASource: TNyxImageSource): TNyxButtonPresentation;
begin
  Result := Copy;
  Result.FImage := TNyxImageSource.FromWire(ASource.ToWire);
  Result.FResource := Default(TNyxResourceImageRef);
  Result.FGlyphKind := nbgNone;

  if Result.FImage.Kind <> nisEmpty then
  begin
    Result.FGlyphKind := nbgImage;
  end;
  Result.Validate;
end;

function TNyxButtonPresentation.Glyph(
  const AResource: TNyxResourceImageRef): TNyxButtonPresentation;
begin
  Result := Copy;
  Result.FResource := TNyxResourceImageRef.FromData(AResource.ToData);
  Result.FImage := NyxNoImage;
  Result.FGlyphKind := nbgResource;
  Result.Validate;
end;

function TNyxButtonPresentation.Cell(const ACell: TNyxGlyphCell): TNyxButtonPresentation;
begin
  Result := Copy;
  Result.FCell := ACell.Copy;
  Result.Validate;
end;

function TNyxButtonPresentation.GlyphSize(AValue: Integer): TNyxButtonPresentation;
begin
  Result := Copy;
  Result.FGlyphSize := AValue;
  Result.Validate;
end;

function TNyxButtonPresentation.GlyphGap(AValue: Integer): TNyxButtonPresentation;
begin
  Result := Copy;
  Result.FGlyphGap := AValue;
  Result.Validate;
end;

function TNyxButtonPresentation.Content(AValue: TNyxButtonContent): TNyxButtonPresentation;
begin
  Result := Copy;
  Result.FContent := AValue;
  Result.Validate;
end;

function TNyxButtonPresentation.Density(AValue: TNyxButtonDensity): TNyxButtonPresentation;
begin
  Result := Copy;
  Result.FDensity := AValue;
  Result.Validate;
end;

function TNyxButtonPresentation.Alignment(
  AValue: TNyxButtonContentAlignment): TNyxButtonPresentation;
begin
  Result := Copy;
  Result.FAlignment := AValue;
  Result.Validate;
end;

function TNyxButtonPresentation.FontWeight(
  AValue: TNyxButtonFontWeight): TNyxButtonPresentation;
begin
  Result := Copy;
  Result.FWeight := AValue;
  Result.Validate;
end;

function TNyxButtonPresentation.GlyphTone(AValue: TNyxGlyphTone): TNyxButtonPresentation;
begin
  Result := Copy;
  Result.FTone := AValue;
  Result.Validate;
end;

function TNyxButtonPresentation.ToData: TNyxDataValue;
var
  LValue: TNyxButtonPresentation;
  LGlyph: TNyxDataValue;
begin
  LValue := Copy;
  LGlyph := NyxObject([NyxField('kind', NyxData('none')), NyxField('value', NyxNull)]);
  case LValue.FGlyphKind of
    nbgNone:
      begin
        { The explicit none/null source above is the canonical absent glyph. }
      end;
    nbgImage:
      begin
        LGlyph := NyxObject([NyxField('kind', NyxData('image')),
          NyxField('value', NyxData(LValue.FImage.ToWire))]);
      end;
    nbgResource:
      begin
        LGlyph := NyxObject([NyxField('kind', NyxData('resource')),
          NyxField('value', LValue.FResource.ToData)]);
      end;
  end;
  Result := NyxObject([NyxField('version', NyxData(1)), NyxField('glyph', LGlyph),
    NyxField('cell', LValue.FCell.ToData), NyxField('glyphSize', NyxData(LValue.FGlyphSize)),
    NyxField('glyphGap', NyxData(LValue.FGlyphGap)),
    NyxField('content', NyxData(NyxButtonContentName(LValue.FContent))),
    NyxField('density', NyxData(NyxButtonDensityName(LValue.FDensity))),
    NyxField('alignment', NyxData(NyxButtonAlignmentName(LValue.FAlignment))),
    NyxField('fontWeight', NyxData(NyxButtonFontWeightName(LValue.FWeight))),
    NyxField('glyphTone', NyxData(NyxGlyphToneName(LValue.FTone)))]);
end;

class function TNyxButtonPresentation.FromData(
  const AData: TNyxDataValue): TNyxButtonPresentation;
var
  LValue: TNyxButtonPresentation;
  LGlyph: TNyxDataValue;
  LKind: TNyxText;
begin
  RequireKeys(AData, ['version', 'glyph', 'cell', 'glyphSize', 'glyphGap',
    'content', 'density', 'alignment', 'fontWeight', 'glyphTone']);

  if AData.Field('version').AsInteger <> 1 then
  begin
    raise ENyxButtonPresentation.Create('Unsupported button presentation version');
  end;
  LValue := NyxButtonPresentation;
  LGlyph := AData.Field('glyph');
  RequireKeys(LGlyph, ['kind', 'value']);
  LKind := LGlyph.Field('kind').AsText;

  if LKind = 'none' then
  begin

    if LGlyph.Field('value').Kind <> ndNull then
    begin
      raise ENyxButtonPresentation.Create('Absent glyph requires its explicit null value');
    end;
  end
  else if LKind = 'image' then
  begin
    LValue.FGlyphKind := nbgImage;
    LValue.FImage := TNyxImageSource.FromWire(LGlyph.Field('value').AsText);
  end
  else if LKind = 'resource' then
  begin
    LValue.FGlyphKind := nbgResource;
    LValue.FResource := TNyxResourceImageRef.FromData(LGlyph.Field('value'));
  end
  else
  begin
    raise ENyxButtonPresentation.Create('Unknown button glyph source kind: ' + LKind);
  end;
  LValue.FCell := TNyxGlyphCell.FromData(AData.Field('cell'));
  LValue.FGlyphSize := AData.Field('glyphSize').AsInteger;
  LValue.FGlyphGap := AData.Field('glyphGap').AsInteger;
  LValue.FContent := ReadNyxButtonContent(AData.Field('content').AsText);
  LValue.FDensity := ReadNyxButtonDensity(AData.Field('density').AsText);
  LValue.FAlignment := ReadNyxButtonAlignment(AData.Field('alignment').AsText);
  LValue.FWeight := ReadNyxButtonFontWeight(AData.Field('fontWeight').AsText);
  LValue.FTone := ReadNyxGlyphTone(AData.Field('glyphTone').AsText);
  LValue.Validate;
  Result := LValue;
end;

end.
