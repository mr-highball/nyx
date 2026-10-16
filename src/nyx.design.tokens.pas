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
  SysUtils, nyx.text, nyx.data, nyx.model, nyx.theme, nyx.colors, nyx.design.tokens.core;

const
  NyxDesignTokensKey = 'nyx.designTokens';

type
  { Same color identity; its portable values do not depend on a document. }
  TNyxThemeColor = nyx.design.tokens.core.TNyxThemeColor;
  { Same metric identity for existing authoring and source readers. }
  TNyxThemeMetric = nyx.design.tokens.core.TNyxThemeMetric;
  { Same closed default-palette choice. }
  TNyxThemePreset = nyx.design.tokens.core.TNyxThemePreset;
  { Same immutable portable record; document-facing helpers remain in this unit. }
  TNyxThemeTokens = nyx.design.tokens.core.TNyxThemeTokens;

const
  { Preserve existing enum spellings and ordinals in public authoring. }
  ntcBackground = nyx.design.tokens.core.ntcBackground;
  ntcSurface = nyx.design.tokens.core.ntcSurface;
  ntcText = nyx.design.tokens.core.ntcText;
  ntcMuted = nyx.design.tokens.core.ntcMuted;
  ntcBorder = nyx.design.tokens.core.ntcBorder;
  ntcAccent = nyx.design.tokens.core.ntcAccent;
  ntcAccentText = nyx.design.tokens.core.ntcAccentText;
  ntcDanger = nyx.design.tokens.core.ntcDanger;
  ntcDangerText = nyx.design.tokens.core.ntcDangerText;
  ntcSuccess = nyx.design.tokens.core.ntcSuccess;
  ntcSuccessText = nyx.design.tokens.core.ntcSuccessText;
  ntcWarning = nyx.design.tokens.core.ntcWarning;
  ntcWarningText = nyx.design.tokens.core.ntcWarningText;
  ntmRadius = nyx.design.tokens.core.ntmRadius;
  ntmControlRadius = nyx.design.tokens.core.ntmControlRadius;
  ntmFontSize = nyx.design.tokens.core.ntmFontSize;
  ntpLight = nyx.design.tokens.core.ntpLight;
  ntpDark = nyx.design.tokens.core.ntpDark;

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
begin
  Result := nyx.design.tokens.core.NyxThemeColorName(AColor);
end;

function NyxThemeMetricName(AMetric: TNyxThemeMetric): TNyxText;
begin
  Result := nyx.design.tokens.core.NyxThemeMetricName(AMetric);
end;

function NyxThemeTokens: TNyxThemeTokens;
begin
  Result := nyx.design.tokens.core.NyxThemeTokens;
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
begin
  ApplyNyxThemeTokenValues(ATheme, AValues);
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
      Result.Danger := ABase.Danger;
      Result.DangerText := ABase.DangerText;
      Result.Success := ABase.Success;
      Result.SuccessText := ABase.SuccessText;
      Result.Warning := ABase.Warning;
      Result.WarningText := ABase.WarningText;
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
begin
  Result := nyx.design.tokens.core.NyxThemePreset(APreset);
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
      NyxField('danger', NyxData(LTheme.Danger)),
      NyxField('dangerText', NyxData(LTheme.DangerText)),
      NyxField('success', NyxData(LTheme.Success)),
      NyxField('successText', NyxData(LTheme.SuccessText)),
      NyxField('warning', NyxData(LTheme.Warning)),
      NyxField('warningText', NyxData(LTheme.WarningText)),
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
