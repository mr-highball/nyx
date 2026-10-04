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

unit nyx.studio.outputs;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  SysUtils,
  fpjson,
  nyx.text,
  nyx.model;

type
  { Optional machine-local output profiles. These paths never enter a design
    document or its undo history. Empty fields are valid during authoring;
    filesystem/toolchain readiness is checked by the server for a chosen build.
    Profiles are data, not compiler argument lists or shell command strings. }
  TNyxOutputConfiguration = class
  private
    FFields: TNyxStrings;
  public
    constructor Create;
    destructor Destroy; override;
    { Known omitted fields return empty text. Unknown keys raise a diagnostic,
      keeping field spelling errors distinct from intentionally absent tools. }
    function Field(const AKey: TNyxText): TNyxText;
    { Admits empty values and Unicode paths without checking file existence.
      Rejects unknown keys, control separators and values beyond 4096 units. }
    procedure SetField(const AKey, AValue: TNyxText);
    { Deterministic, versioned local JSON; emits every known field, including
      empty ones, so an explicit clear overrides an inherited startup hint. }
    function Encode: TNyxText;
    { Returns an owned configuration. Invalid input frees the partial candidate;
      callers replace their accepted configuration only after successful decode. }
    class function Decode(const ASource: TNyxText): TNyxOutputConfiguration; static;
  end;

const
  NyxOutputFields: array[0..5] of TNyxText = ('pas2js', 'runtime', 'fpc',
    'lazarus', 'platform', 'widgetset');

{ Empty means no output chosen. Choosing a target is independent of having its
  tools installed; neither operation changes the authored document. }
procedure ValidateNyxOutputTarget(const ATarget: TNyxText);

implementation

uses
  {$IFDEF PAS2JS}
  JS,
  fpjsonjs;
  {$ELSE}
  jsonparser;
  {$ENDIF}

function IsOutputField(const AKey: TNyxText): Boolean;
var
  LIndex: Integer;
begin
  Result := False;
  for LIndex := 0 to High(NyxOutputFields) do
  begin

    if NyxOutputFields[LIndex] = AKey then
    begin
      Exit(True);
    end;
  end;
end;

procedure ValidateNyxOutputTarget(const ATarget: TNyxText);
begin

  if (ATarget <> '') and (ATarget <> 'browser') and (ATarget <> 'lcl') then
  begin
    raise ENyxModel.Create('Choose Browser or Native LCL in Target / output');
  end;
end;

constructor TNyxOutputConfiguration.Create;
begin
  inherited Create;
  FFields := TNyxStrings.Create;
end;

destructor TNyxOutputConfiguration.Destroy;
begin
  FFields.Free;
  inherited Destroy;
end;

function TNyxOutputConfiguration.Field(const AKey: TNyxText): TNyxText;
var
  LIndex: Integer;
begin

  if not IsOutputField(AKey) then
  begin
    raise ENyxModel.Create('Unknown output field: ' + AKey);
  end;
  Result := '';
  LIndex := FFields.IndexOfName(AKey);

  if LIndex >= 0 then
  begin
    Result := Copy(FFields[LIndex], Length(AKey) + 2, MaxInt);
  end;
end;

procedure TNyxOutputConfiguration.SetField(const AKey, AValue: TNyxText);
var
  LIndex: Integer;
begin

  if not IsOutputField(AKey) then
  begin
    raise ENyxModel.Create('Unknown output field: ' + AKey);
  end;

  if (Length(AValue) > 4096) or (Pos(#0, AValue) > 0) or
    (Pos(#10, AValue) > 0) or (Pos(#13, AValue) > 0) then
  begin
    raise ENyxModel.Create('Output fields must be single-line paths or identifiers');
  end;
  LIndex := FFields.IndexOfName(AKey);

  if LIndex >= 0 then
  begin
    FFields[LIndex] := AKey + '=' + AValue;
  end
  else
  begin
    FFields.Add(AKey + '=' + AValue);
  end;
end;

function TNyxOutputConfiguration.Encode: TNyxText;
var
  LRoot: TJSONObject;
  LFields: TJSONObject;
  LIndex: Integer;
begin
  LRoot := TJSONObject.Create;
  try
    LRoot.Add('version', 1);
    LFields := TJSONObject.Create;
    LRoot.Add('fields', LFields);
    for LIndex := 0 to High(NyxOutputFields) do
    begin
      LFields.Add(NyxOutputFields[LIndex], Field(NyxOutputFields[LIndex]));
    end;
    Result := LRoot.AsJSON;
  finally
    LRoot.Free;
  end;
end;

class function TNyxOutputConfiguration.Decode(
  const ASource: TNyxText): TNyxOutputConfiguration;
var
  LData: TJSONData;
  LRoot: TJSONObject;
  LFields: TJSONObject;
  LValue: TJSONData;
  LIndex: Integer;
  LDepth: Integer;
  LQuoted: Boolean;
  LEscaped: Boolean;
begin
  { Bound input before either JSON parser sees it. This flat protocol needs no
    deep nesting, and browser/native parsers share the same admission limits. }

  if Length(ASource) > 32768 then
  begin
    raise ENyxModel.Create('Output configuration exceeds 32 KiB');
  end;
  LDepth := 0;
  LQuoted := False;
  LEscaped := False;
  for LIndex := 1 to Length(ASource) do
  begin

    if LQuoted then
    begin

      if LEscaped then
      begin
        LEscaped := False;
      end
      else if ASource[LIndex] = '\' then
      begin
        LEscaped := True;
      end
      else if ASource[LIndex] = '"' then
      begin
        LQuoted := False;
      end;
    end
    else
    begin
      case ASource[LIndex] of
        '"':
          begin
            LQuoted := True;
          end;
        '{', '[':
          begin
            Inc(LDepth);
          end;
        '}', ']':
          begin
            Dec(LDepth);
          end;
      end;

      if (LDepth > 4) or (LDepth < 0) then
      begin
        raise ENyxModel.Create('Invalid output configuration nesting');
      end;
    end;
  end;
  {$IFDEF PAS2JS}
  LData := JSValueToJSONData(TJSJSON.parse(ASource));
  {$ELSE}
  LData := GetJSON(ASource);
  {$ENDIF}
  Result := nil;
  try

    if not (LData is TJSONObject) then
    begin
      raise ENyxModel.Create('Output configuration must be an object');
    end;
    LRoot := TJSONObject(LData);
    LValue := LRoot.Find('version');

    if (LValue = nil) or (LValue.JSONType <> jtNumber) or (LValue.AsFloat <> 1) then
    begin
      raise ENyxModel.Create('Unsupported output configuration version');
    end;
    LValue := LRoot.Find('fields');

    if not (LValue is TJSONObject) or (LRoot.Count <> 2) then
    begin
      raise ENyxModel.Create('Output configuration requires version and fields');
    end;
    LFields := TJSONObject(LValue);
    Result := TNyxOutputConfiguration.Create;
    try
      for LIndex := 0 to LFields.Count - 1 do
      begin

        if LFields.Items[LIndex].JSONType <> jtString then
        begin
          raise ENyxModel.Create('Output fields must be strings');
        end;
        Result.SetField(LFields.Names[LIndex], LFields.Items[LIndex].AsString);
      end;
    except
      Result.Free;
      Result := nil;
      raise;
    end;
  finally
    LData.Free;
  end;
end;

end.
