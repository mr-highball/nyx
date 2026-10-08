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

unit nyx.design.tokens;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.model, nyx.theme, nyx.colors;

const
  NyxDesignTokensKey = 'nyx.designTokens';

type
  { Closed semantic roles. Colors are defined eight-bit sRGB values; metrics
    use logical pixels. Absence inherits the renderer's independent base theme. }
  TNyxThemeColor = (ntcBackground, ntcSurface, ntcText, ntcMuted, ntcBorder,
    ntcAccent, ntcAccentText);
  TNyxThemeMetric = (ntmRadius, ntmControlRadius, ntmFontSize);
  TNyxThemePreset = (ntpLight, ntpDark);

  { Immutable fluent document palette. Copies share only immutable data, never
    a theme, document or renderer. Partial declarations retain exact presence,
    ordering and admitted hexadecimal case through persistence and generation.
    FromData is the strict extension boundary; unknown roles, absent colors and
    noninteger/out-of-range metrics refuse before publication. Default is empty. }
  TNyxThemeTokens = record
  private
    FValues: TNyxDataValue;
    function WithValue(const AName: TNyxText;
      const AValue: TNyxDataValue): TNyxThemeTokens;
  public
    class function FromData(const AValues: TNyxDataValue): TNyxThemeTokens; static;
    function ToData: TNyxDataValue;
    function Has(AColor: TNyxThemeColor): Boolean; overload;
    function Has(AMetric: TNyxThemeMetric): Boolean; overload;
    { Reads require a declared role; effective palettes can be obtained from
      NyxDesignTokens. No missing value silently becomes black or zero. }
    function ColorValue(AColor: TNyxThemeColor): TNyxRGBColor;
    function MetricValue(AMetric: TNyxThemeMetric): Integer;
    function Color(AColor: TNyxThemeColor;
      const AValue: TNyxRGBColor): TNyxThemeTokens;
    function Metric(AMetric: TNyxThemeMetric; AValue: Integer): TNyxThemeTokens;
    function Background(const AValue: TNyxRGBColor): TNyxThemeTokens;
    function Surface(const AValue: TNyxRGBColor): TNyxThemeTokens;
    function Text(const AValue: TNyxRGBColor): TNyxThemeTokens;
    function Muted(const AValue: TNyxRGBColor): TNyxThemeTokens;
    function Border(const AValue: TNyxRGBColor): TNyxThemeTokens;
    function Accent(const AValue: TNyxRGBColor): TNyxThemeTokens;
    function AccentText(const AValue: TNyxRGBColor): TNyxThemeTokens;
    function Radius(AValue: Integer): TNyxThemeTokens;
    function ControlRadius(AValue: Integer): TNyxThemeTokens;
    function FontSize(AValue: Integer): TNyxThemeTokens;
  end;

{ An empty declaration inherits every role. Presets return independent complete
  proposals; selecting one never changes a document or an existing theme. }
function NyxThemeTokens: TNyxThemeTokens;
function NyxThemePreset(APreset: TNyxThemePreset): TNyxThemeTokens;
function NyxDeclaredThemeTokens(ADocument: TNyxDocument): TNyxThemeTokens;
{ Exact extension declaration for persistence/history guards; null means absent. }
function NyxThemeDeclaration(ADocument: TNyxDocument): TNyxDataValue;
{ Replace the exact local declaration atomically, preserving partial overrides.
  Reset removes only the declaration, revealing the host's inherited base.
  Nil owners raise ENyxModel. Neither procedure owns or changes that base. }
procedure SetNyxThemeTokens(ADocument: TNyxDocument;
  const ATokens: TNyxThemeTokens);
procedure ResetNyxThemeTokens(ADocument: TNyxDocument);
{ Stable persistence names are exposed solely for codecs/editor identity. }
function NyxThemeColorName(AColor: TNyxThemeColor): TNyxText;
function NyxThemeMetricName(AMetric: TNyxThemeMetric): TNyxText;

{ Returns the complete effective semantic palette/metrics as typed immutable
  data. Only the names published by TNyxTheme are admitted. ABase is borrowed;
  nil chooses Nyx defaults. Returned themes are always independently owned. }
function NewNyxDocumentTheme(ADocument: TNyxDocument;
  ABase: TNyxTheme = nil): TNyxTheme;
function NyxDesignTokens(ADocument: TNyxDocument): TNyxDataValue;
{ Ordinary designs without an override allocate no theme/data merely to validate
  properties. Present known token data still receives full semantic admission. }
procedure ValidateNyxDesignTokens(ADocument: TNyxDocument);
{ Merge typed overrides on a detached/admitted owner. Null resets a token to its
  inherited default. Validate the complete palette before replacing extension
  data; failures retain the old tokens. Persistence/source history owns copies. }
procedure SetNyxDesignTokens(ADocument: TNyxDocument; const AValues: TNyxDataValue);

implementation

function NyxThemeColorName(AColor: TNyxThemeColor): TNyxText;
const
  CNames: array[TNyxThemeColor] of TNyxText = ('background', 'surface', 'text',
    'muted', 'border', 'accent', 'accentText');
begin
  Result := CNames[AColor];
end;

function NyxThemeMetricName(AMetric: TNyxThemeMetric): TNyxText;
const
  CNames: array[TNyxThemeMetric] of TNyxText = ('radius', 'controlRadius', 'fontSize');
begin
  Result := CNames[AMetric];
end;

function HasToken(const AValues: TNyxDataValue; const AName: TNyxText): Boolean;
var
  LIndex: Integer;
begin
  Result := False;
  for LIndex := 0 to AValues.Count - 1 do
  begin

    if AValues.Key(LIndex) = AName then
    begin
      Exit(True);
    end;
  end;
end;

function NyxThemeTokens: TNyxThemeTokens;
begin
  Result := Default(TNyxThemeTokens);
  Result.FValues := NyxObject([]);
end;

function TNyxThemeTokens.ToData: TNyxDataValue;
begin
  Result := FValues;

  if not Result.Defined then
  begin
    Result := NyxObject([]);
  end;
end;

function TNyxThemeTokens.Has(AColor: TNyxThemeColor): Boolean;
begin
  Result := HasToken(ToData, NyxThemeColorName(AColor));
end;

function TNyxThemeTokens.Has(AMetric: TNyxThemeMetric): Boolean;
begin
  Result := HasToken(ToData, NyxThemeMetricName(AMetric));
end;

function TNyxThemeTokens.ColorValue(AColor: TNyxThemeColor): TNyxRGBColor;
begin
  Result := TNyxRGBColor.FromText(ToData.Field(NyxThemeColorName(AColor)).AsText);
end;

function TNyxThemeTokens.MetricValue(AMetric: TNyxThemeMetric): Integer;
begin
  Result := ToData.Field(NyxThemeMetricName(AMetric)).AsInteger;
end;

function TNyxThemeTokens.WithValue(const AName: TNyxText;
  const AValue: TNyxDataValue): TNyxThemeTokens;
var
  LCurrent: TNyxDataValue;
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LFound: Boolean;
begin
  LCurrent := ToData;
  LFound := HasToken(LCurrent, AName);
  SetLength(LFields, LCurrent.Count + Ord(not LFound));
  for LIndex := 0 to LCurrent.Count - 1 do
  begin
    LFields[LIndex] := NyxField(LCurrent.Key(LIndex), LCurrent.Field(LCurrent.Key(LIndex)));

    if LFields[LIndex].Name = AName then
    begin
      LFields[LIndex] := NyxField(AName, AValue);
    end;
  end;

  if not LFound then
  begin
    LFields[High(LFields)] := NyxField(AName, AValue);
  end;
  Result := Self;
  Result.FValues := NyxObject(LFields);
end;

function TNyxThemeTokens.Color(AColor: TNyxThemeColor;
  const AValue: TNyxRGBColor): TNyxThemeTokens;
begin

  if not AValue.Defined then
  begin
    raise ENyxModel.Create('Theme colors require a defined RGB value');
  end;
  Result := WithValue(NyxThemeColorName(AColor), NyxData(AValue.ToText));
end;

function TNyxThemeTokens.Metric(AMetric: TNyxThemeMetric;
  AValue: Integer): TNyxThemeTokens;
var
  LMinimum: Integer;
  LMaximum: Integer;
begin
  LMinimum := 0;
  LMaximum := 1000;

  if AMetric = ntmFontSize then
  begin
    LMinimum := 1;
    LMaximum := 256;
  end;

  if (AValue < LMinimum) or (AValue > LMaximum) then
  begin
    raise ENyxModel.Create('Theme metric is outside its logical pixel range');
  end;
  Result := WithValue(NyxThemeMetricName(AMetric), NyxData(AValue));
end;

function TNyxThemeTokens.Background(const AValue: TNyxRGBColor): TNyxThemeTokens;
begin
  Result := Color(ntcBackground, AValue);
end;

function TNyxThemeTokens.Surface(const AValue: TNyxRGBColor): TNyxThemeTokens;
begin
  Result := Color(ntcSurface, AValue);
end;

function TNyxThemeTokens.Text(const AValue: TNyxRGBColor): TNyxThemeTokens;
begin
  Result := Color(ntcText, AValue);
end;

function TNyxThemeTokens.Muted(const AValue: TNyxRGBColor): TNyxThemeTokens;
begin
  Result := Color(ntcMuted, AValue);
end;

function TNyxThemeTokens.Border(const AValue: TNyxRGBColor): TNyxThemeTokens;
begin
  Result := Color(ntcBorder, AValue);
end;

function TNyxThemeTokens.Accent(const AValue: TNyxRGBColor): TNyxThemeTokens;
begin
  Result := Color(ntcAccent, AValue);
end;

function TNyxThemeTokens.AccentText(const AValue: TNyxRGBColor): TNyxThemeTokens;
begin
  Result := Color(ntcAccentText, AValue);
end;

function TNyxThemeTokens.Radius(AValue: Integer): TNyxThemeTokens;
begin
  Result := Metric(ntmRadius, AValue);
end;

function TNyxThemeTokens.ControlRadius(AValue: Integer): TNyxThemeTokens;
begin
  Result := Metric(ntmControlRadius, AValue);
end;

function TNyxThemeTokens.FontSize(AValue: Integer): TNyxThemeTokens;
begin
  Result := Metric(ntmFontSize, AValue);
end;

procedure ValidateNyxDesignTokens(ADocument: TNyxDocument);
var
  LTheme: TNyxTheme;
begin

  if not ADocument.Extensions.Has(NyxExtension(NyxDesignTokensKey)) then
  begin
    Exit;
  end;
  LTheme := NewNyxDocumentTheme(ADocument);
  LTheme.Free;
end;

procedure Overlay(ATheme: TNyxTheme; const AValues: TNyxDataValue);
var
  LIndex: Integer;
  LName: TNyxText;
  LValue: TNyxDataValue;
begin

  if AValues.Kind <> ndObject then
  begin
    raise ENyxModel.Create('Design tokens require an object');
  end;
  for LIndex := 0 to AValues.Count - 1 do
  begin
    LName := AValues.Key(LIndex);
    LValue := AValues.Field(LName);

    if LName = 'background' then
    begin
      ATheme.Background := LValue.AsText;
    end
    else if LName = 'surface' then
    begin
      ATheme.Surface := LValue.AsText;
    end
    else if LName = 'text' then
    begin
      ATheme.Text := LValue.AsText;
    end
    else if LName = 'muted' then
    begin
      ATheme.Muted := LValue.AsText;
    end
    else if LName = 'border' then
    begin
      ATheme.Border := LValue.AsText;
    end
    else if LName = 'accent' then
    begin
      ATheme.Accent := LValue.AsText;
    end
    else if LName = 'accentText' then
    begin
      ATheme.AccentText := LValue.AsText;
    end
    else if LName = 'radius' then
    begin
      ATheme.Radius := LValue.AsInteger;
    end
    else if LName = 'controlRadius' then
    begin
      ATheme.ControlRadius := LValue.AsInteger;
    end
    else if LName = 'fontSize' then
    begin
      ATheme.FontSize := LValue.AsInteger;
    end
    else
    begin
      raise ENyxModel.Create('Unknown design token: ' + LName);
    end;
  end;
  ATheme.Validate;
end;

function NewNyxDocumentTheme(ADocument: TNyxDocument;
  ABase: TNyxTheme): TNyxTheme;
begin
  Result := TNyxTheme.Create;
  try

    if ABase <> nil then
    begin
      Result.Background := ABase.Background;
      Result.Surface := ABase.Surface;
      Result.Text := ABase.Text;
      Result.Muted := ABase.Muted;
      Result.Border := ABase.Border;
      Result.Accent := ABase.Accent;
      Result.AccentText := ABase.AccentText;
      Result.Radius := ABase.Radius;
      Result.ControlRadius := ABase.ControlRadius;
      Result.FontSize := ABase.FontSize;
    end;

    if (ADocument <> nil) and
      ADocument.Extensions.Has(NyxExtension(NyxDesignTokensKey)) then
    begin
      Overlay(Result, ADocument.Extensions.Value(NyxExtension(NyxDesignTokensKey)));
    end;
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

class function TNyxThemeTokens.FromData(const AValues: TNyxDataValue): TNyxThemeTokens;
var
  LTheme: TNyxTheme;
  LColor: TNyxThemeColor;
  LValue: TNyxRGBColor;
begin
  LTheme := TNyxTheme.Create;
  try
    Overlay(LTheme, AValues);
    for LColor := Low(TNyxThemeColor) to High(TNyxThemeColor) do
    begin

      if HasToken(AValues, NyxThemeColorName(LColor)) then
      begin
        LValue := TNyxRGBColor.FromText(AValues.Field(NyxThemeColorName(LColor)).AsText);

        if not LValue.Defined then
        begin
          raise ENyxModel.Create('Theme colors cannot be empty');
        end;
      end;
    end;
    Result := NyxThemeTokens;
    Result.FValues := AValues.Copy;
  finally
    LTheme.Free;
  end;
end;

function NyxDeclaredThemeTokens(ADocument: TNyxDocument): TNyxThemeTokens;
begin
  Result := NyxThemeTokens;

  if (ADocument <> nil) and ADocument.Extensions.Has(NyxExtension(NyxDesignTokensKey)) then
  begin
    Result := TNyxThemeTokens.FromData(ADocument.Extensions.Value(NyxExtension(NyxDesignTokensKey)));
  end;
end;

function NyxThemeDeclaration(ADocument: TNyxDocument): TNyxDataValue;
begin
  Result := NyxNull;

  if (ADocument <> nil) and ADocument.Extensions.Has(NyxExtension(NyxDesignTokensKey)) then
  begin
    Result := ADocument.Extensions.Value(NyxExtension(NyxDesignTokensKey));
  end;
end;

procedure SetNyxThemeTokens(ADocument: TNyxDocument; const ATokens: TNyxThemeTokens);
var
  LAdmitted: TNyxThemeTokens;
begin

  if ADocument = nil then
  begin
    raise ENyxModel.Create('Theme authoring requires a document');
  end;
  LAdmitted := TNyxThemeTokens.FromData(ATokens.ToData);
  ADocument.Extensions.SetValue(NyxExtension(NyxDesignTokensKey), LAdmitted.ToData);
end;

procedure ResetNyxThemeTokens(ADocument: TNyxDocument);
begin

  if ADocument = nil then
  begin
    raise ENyxModel.Create('Theme reset requires a document');
  end;
  ADocument.Extensions.Remove(NyxExtension(NyxDesignTokensKey));
end;

function NyxThemePreset(APreset: TNyxThemePreset): TNyxThemeTokens;
var
  LTheme: TNyxTheme;
begin
  LTheme := TNyxTheme.Create(APreset = ntpDark);
  try
    Result := NyxThemeTokens
      .Background(TNyxRGBColor.FromText(LTheme.Background))
      .Surface(TNyxRGBColor.FromText(LTheme.Surface))
      .Text(TNyxRGBColor.FromText(LTheme.Text))
      .Muted(TNyxRGBColor.FromText(LTheme.Muted))
      .Border(TNyxRGBColor.FromText(LTheme.Border))
      .Accent(TNyxRGBColor.FromText(LTheme.Accent))
      .AccentText(TNyxRGBColor.FromText(LTheme.AccentText))
      .Radius(LTheme.Radius).ControlRadius(LTheme.ControlRadius).FontSize(LTheme.FontSize);
  finally
    LTheme.Free;
  end;
end;

function NyxDesignTokens(ADocument: TNyxDocument): TNyxDataValue;
var
  LTheme: TNyxTheme;
begin
  LTheme := NewNyxDocumentTheme(ADocument);
  try
    Result := NyxObject([
      NyxField('background', NyxData(LTheme.Background)),
      NyxField('surface', NyxData(LTheme.Surface)),
      NyxField('text', NyxData(LTheme.Text)),
      NyxField('muted', NyxData(LTheme.Muted)),
      NyxField('border', NyxData(LTheme.Border)),
      NyxField('accent', NyxData(LTheme.Accent)),
      NyxField('accentText', NyxData(LTheme.AccentText)),
      NyxField('radius', NyxData(LTheme.Radius)),
      NyxField('controlRadius', NyxData(LTheme.ControlRadius)),
      NyxField('fontSize', NyxData(LTheme.FontSize))]);
  finally
    LTheme.Free;
  end;
end;

procedure SetNyxDesignTokens(ADocument: TNyxDocument; const AValues: TNyxDataValue);
var
  LFields: array of TNyxDataField;
  LCurrent: TNyxDataValue;
  LDefault: TNyxDataValue;
  LTheme: TNyxTheme;
  LName: TNyxText;
  LIndex: Integer;
  LInputIndex: Integer;
  LValue: TNyxDataValue;
  LCount: Integer;
begin
  LCurrent := NyxDesignTokens(ADocument);
  LDefault := NyxDesignTokens(nil);
  LTheme := TNyxTheme.Create;
  try

    if AValues.Kind <> ndObject then
    begin
      raise ENyxModel.Create('Design tokens require typed values');
    end;
    for LInputIndex := 0 to AValues.Count - 1 do
    begin
      LName := AValues.Key(LInputIndex);
      LIndex := 0;
      while (LIndex < LCurrent.Count) and (LCurrent.Key(LIndex) <> LName) do
      begin
        Inc(LIndex);
      end;

      if LIndex = LCurrent.Count then
      begin
        raise ENyxModel.Create('Unknown design token: ' + LName);
      end;
    end;
    SetLength(LFields, LCurrent.Count);
    LCount := 0;
    for LIndex := 0 to LCurrent.Count - 1 do
    begin
      LName := LCurrent.Key(LIndex);
      LValue := LCurrent.Field(LName);
      for LInputIndex := 0 to AValues.Count - 1 do
      begin

        if AValues.Key(LInputIndex) = LName then
        begin
          LValue := AValues.Field(LName);

          if LValue.Kind = ndNull then
          begin
            LValue := LDefault.Field(LName);
          end;
        end;
      end;
      LFields[LCount] := NyxField(LName, LValue);
      Inc(LCount);
    end;
    LCurrent := NyxObject(LFields);
    Overlay(LTheme, LCurrent);
    ADocument.Extensions.SetValue(NyxExtension(NyxDesignTokensKey), LCurrent);
  finally
    LTheme.Free;
  end;
end;

end.
