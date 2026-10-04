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
  SysUtils, nyx.text, nyx.data, nyx.model, nyx.theme;

const
  NyxDesignTokensKey = 'nyx.designTokens';

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
