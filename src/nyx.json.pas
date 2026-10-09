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

unit nyx.json;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  SysUtils,
  fpjson,
  nyx.text;

const
  NyxMaximumJSONBytes = 4 * 1024 * 1024;
  NyxMaximumJSONDepth = 270;
  NyxMaximumJSONMembers = 1024;

type
  { Strict portable JSON admission. Diagnostics retain UTF-8 on native Windows.
    Position is one-based in target storage units (UTF-8 bytes or UTF-16 units).
    This is a persistence boundary, independent of model/DOM/LCL types. }
  ENyxJSON = class(Exception)
  public
    constructor Create(const AMessage: TNyxText); reintroduce;
  end;

{ Returns a caller-owned fpjson tree, with exact decoded strings on both targets.
  Strict JSON syntax, finite decimal numbers, valid Unicode, UTF-8 byte/depth/
  per-object budgets and decoded duplicate keys are checked before admission.
  Partial trees are released on failure; input text is never mutated.
  Admitted numeric spellings survive serialization and Clone, including decimal
  precision beyond Double. AsFloat remains an explicit approximate conversion.

  An owned decoder is required because the older native JSON scanner drops
  escaped U+0000 and browser JSON.parse silently collapses duplicate keys.
  fpjson still provides standard data containers and numeric/container formatting. }
function DecodeNyxJSON(const ASource: TNyxText): TJSONData;

{ Quote one text value using the existing fpjson wire spelling, including short
  control escapes and uppercase hexadecimal escapes. Strict additionally escapes
  the solidus. Raw Unicode/storage units are retained, never normalized; this is
  formatting, not admission. DecodeNyxJSON still validates the complete result.
  Browser formatting copies ordinary runs without allocating a character set for
  every character. Native formatting keeps the matched fpjson implementation. }
function EncodeNyxJSONString(const AText: TNyxText;
  AStrict: Boolean = False): TNyxText;

implementation

uses
  nyx.state;

type
  {$ifdef PAS2JS}
  { Keep standard fpjson ownership/setters, but use the run-based formatter for
    decoded strings. Clone must retain that formatter; no tree/reader is retained
    by a value. The inherited class-wide StrictEscaping preference still applies. }
  TNyxJSONString = class(TJSONString)
  protected
    function GetAsJSON: TJSONStringType; override;
  public
    function Clone: TJSONData; override;
  end;
  {$endif}

  { fpjson still provides number conversion and standard object/array ownership.
    Preserve the admitted spelling until an explicit fpjson setter/clear;
    caller writes use fpjson's normal formatter, even for the same Double.
    This avoids rounding opaque extension IDs such as 9007199254740993 while
    keeping the existing finite-number admission contract. }
  TNyxJSONDecimalNumber = class(TJSONFloatNumber)
  private
    FDecimal: TNyxText;
  protected
    function GetAsJSON: TJSONStringType; override;
    function GetAsString: TJSONStringType; override;
    procedure SetAsBoolean(const AValue: Boolean); override;
    procedure SetAsFloat(const AValue: TJSONFloat); override;
    procedure SetAsInteger(const AValue: Integer); override;
    procedure SetAsString(const AValue: TJSONStringType); override;
    procedure SetValue(const AValue: TJSONVariant); override;
    {$IFNDEF PAS2JS}
    procedure SetAsInt64(const AValue: Int64); override;
    procedure SetAsQWord(const AValue: QWord); override;
    {$ELSE}
    procedure SetAsNativeInt(const AValue: NativeInt); override;
    {$ENDIF}
  public
    constructor Create(const ADecimal: TNyxText; AParsed: Double); reintroduce;
    function Clone: TJSONData; override;
    procedure Clear; override;
  end;

  TNyxJSONReader = class
  private
    FSource: TNyxText;
    FIndex: Integer;
    procedure Problem(const AReason: TNyxText);
    procedure SkipSpace;
    procedure Require(AChar: Char);
    procedure Word(const AWord: TNyxText);
    function HexQuad: Integer;
    function ReadString: TNyxText;
    function ReadNumber: TJSONData;
    function ReadObject(ADepth: Integer): TJSONObject;
    function ReadArray(ADepth: Integer): TJSONArray;
    function ReadValue(ADepth: Integer): TJSONData;
  public
    constructor Create(const ASource: TNyxText);
    function Read: TJSONData;
  end;

function EncodeNyxJSONString(const AText: TNyxText;
  AStrict: Boolean): TNyxText;
{$ifdef PAS2JS}
var
  LParts: TNyxStrings;
  LIndex: Integer;
  LStart: Integer;
  LChar: Char;
  LEscape: TNyxText;
{$endif}
begin
  {$ifdef PAS2JS}
  LParts := TNyxStrings.Create;
  try
    LParts.Add('"');
    LStart := 1;
    for LIndex := 1 to Length(AText) do
    begin
      LChar := AText[LIndex];

      if (Ord(LChar) < 32) or (LChar = '"') or (LChar = '\') or
        (AStrict and (LChar = '/')) then
      begin
        LParts.Add(Copy(AText, LStart, LIndex - LStart));
        case LChar of
          '"':
            begin
              LEscape := '\"';
            end;
          '\':
            begin
              LEscape := '\\';
            end;
          '/':
            begin
              LEscape := '\/';
            end;
          #8:
            begin
              LEscape := '\b';
            end;
          #9:
            begin
              LEscape := '\t';
            end;
          #10:
            begin
              LEscape := '\n';
            end;
          #12:
            begin
              LEscape := '\f';
            end;
          #13:
            begin
              LEscape := '\r';
            end;
          else
            begin
              LEscape := '\u' + IntToHex(Ord(LChar), 4);
            end;
        end;
        LParts.Add(LEscape);
        LStart := LIndex + 1;
      end;
    end;
    LParts.Add(Copy(AText, LStart, Length(AText) - LStart + 1));
    LParts.Add('"');
    Result := LParts.Join;
  finally
    LParts.Free;
  end;
  {$else}
  Result := '"' + StringToJSONString(TJSONStringType(AText), AStrict) + '"';
  {$endif}
end;

{$ifdef PAS2JS}
function TNyxJSONString.GetAsJSON: TJSONStringType;
begin
  Result := EncodeNyxJSONString(AsString, StrictEscaping);
end;

function TNyxJSONString.Clone: TJSONData;
begin
  Result := TNyxJSONString.Create(AsString);
end;
{$endif}

constructor TNyxJSONDecimalNumber.Create(const ADecimal: TNyxText; AParsed: Double);
begin
  inherited Create(AParsed);
  FDecimal := ADecimal;
end;

function TNyxJSONDecimalNumber.GetAsJSON: TJSONStringType;
begin

  if FDecimal <> '' then
  begin
    Result := FDecimal;
  end
  else
  begin
    { The base GetAsJSON calls virtual GetAsString. Call its concrete formatter
      directly to avoid re-entering this class after a caller numeric write. }
    Result := inherited GetAsString;
  end;
end;

function TNyxJSONDecimalNumber.GetAsString: TJSONStringType;
begin
  Result := GetAsJSON;
end;

function TNyxJSONDecimalNumber.Clone: TJSONData;
begin
  Result := TNyxJSONDecimalNumber.Create(GetAsJSON, AsFloat);
end;

procedure TNyxJSONDecimalNumber.SetAsBoolean(const AValue: Boolean);
begin
  inherited SetAsBoolean(AValue);
  FDecimal := '';
end;

procedure TNyxJSONDecimalNumber.SetAsFloat(const AValue: TJSONFloat);
begin
  inherited SetAsFloat(AValue);
  FDecimal := '';
end;

procedure TNyxJSONDecimalNumber.SetAsInteger(const AValue: Integer);
begin
  inherited SetAsInteger(AValue);
  FDecimal := '';
end;

procedure TNyxJSONDecimalNumber.SetAsString(const AValue: TJSONStringType);
begin
  inherited SetAsString(AValue);
  FDecimal := '';
end;

procedure TNyxJSONDecimalNumber.SetValue(const AValue: TJSONVariant);
begin
  inherited SetValue(AValue);
  FDecimal := '';
end;

{$IFNDEF PAS2JS}
procedure TNyxJSONDecimalNumber.SetAsInt64(const AValue: Int64);
begin
  inherited SetAsInt64(AValue);
  FDecimal := '';
end;

procedure TNyxJSONDecimalNumber.SetAsQWord(const AValue: QWord);
begin
  inherited SetAsQWord(AValue);
  FDecimal := '';
end;
{$ELSE}
procedure TNyxJSONDecimalNumber.SetAsNativeInt(const AValue: NativeInt);
begin
  inherited SetAsNativeInt(AValue);
  FDecimal := '';
end;
{$ENDIF}

procedure TNyxJSONDecimalNumber.Clear;
begin
  inherited Clear;
  FDecimal := '';
end;

constructor ENyxJSON.Create(const AMessage: TNyxText);
{$IFNDEF PAS2JS}
var
  LMessage: String;
{$ENDIF}
begin
  inherited Create('');
  {$IFDEF PAS2JS}
  Message := AMessage;
  {$ELSE}
  RawByteString(LMessage) := RawByteString(AMessage);
  Message := LMessage;
  {$ENDIF}
end;

constructor TNyxJSONReader.Create(const ASource: TNyxText);
var
  LIndex: Integer;
  LScalar: Integer;
  LBytes: Integer;
begin
  inherited Create;

  if Length(ASource) > NyxMaximumJSONBytes then
  begin
    raise ENyxJSON.Create('JSON exceeds 4 MiB input budget');
  end;
  LIndex := 1;
  LBytes := 0;
  while LIndex <= Length(ASource) do
  begin
    { Count ordinary ASCII without a scalar call/bridge for every source unit.
      Non-ASCII still uses exactly the strict decoder, and the complete UTF-8
      byte budget remains checked before the reader can publish any JSON tree. }

    if Ord(ASource[LIndex]) <= $7f then
    begin
      Inc(LIndex);
      Inc(LBytes);
    end
    else
    begin

      if not NyxNextScalar(ASource, LIndex, LScalar) then
      begin
        raise ENyxJSON.Create('JSON contains malformed Unicode');
      end;

      if LScalar <= $7ff then
      begin
        Inc(LBytes, 2);
      end
      else if LScalar <= $ffff then
      begin
        Inc(LBytes, 3);
      end
      else
      begin
        Inc(LBytes, 4);
      end;
    end;

    if LBytes > NyxMaximumJSONBytes then
    begin
      raise ENyxJSON.Create('JSON exceeds 4 MiB UTF-8 input budget');
    end;
  end;
  FSource := ASource;
  FIndex := 1;
end;

procedure TNyxJSONReader.Problem(const AReason: TNyxText);
begin
  raise ENyxJSON.Create(AReason + ' at JSON position ' + IntToStr(FIndex));
end;

procedure TNyxJSONReader.SkipSpace;
begin
  while (FIndex <= Length(FSource)) and (FSource[FIndex] in [#9, #10, #13, ' ']) do
  begin
    Inc(FIndex);
  end;
end;

procedure TNyxJSONReader.Require(AChar: Char);
begin
  SkipSpace;

  if (FIndex > Length(FSource)) or (FSource[FIndex] <> AChar) then
  begin
    Problem('Expected ' + TNyxText(AChar));
  end;
  Inc(FIndex);
end;

procedure TNyxJSONReader.Word(const AWord: TNyxText);
begin

  if Copy(FSource, FIndex, Length(AWord)) <> AWord then
  begin
    Problem('Invalid JSON value');
  end;
  Inc(FIndex, Length(AWord));
end;

function TNyxJSONReader.HexQuad: Integer;
var
  LIndex: Integer;
  LChar: Char;
  LDigit: Integer;
begin
  Result := 0;
  for LIndex := 1 to 4 do
  begin

    if FIndex > Length(FSource) then
    begin
      Problem('Incomplete Unicode escape');
    end;
    LChar := FSource[FIndex];
    case LChar of
      '0'..'9':
        begin
          LDigit := Ord(LChar) - Ord('0');
        end;
      'a'..'f':
        begin
          LDigit := Ord(LChar) - Ord('a') + 10;
        end;
      'A'..'F':
        begin
          LDigit := Ord(LChar) - Ord('A') + 10;
        end;
      else
        begin
          Problem('Invalid Unicode escape');
          LDigit := 0;
        end;
    end;
    Result := Result * 16 + LDigit;
    Inc(FIndex);
  end;
end;

function TNyxJSONReader.ReadString: TNyxText;
var
  LParts: TNyxStrings;
  LStart: Integer;
  LChar: Char;
  LScalar: Integer;
  LLow: Integer;
begin
  Require('"');
  LParts := TNyxStrings.Create;
  try
    LStart := FIndex;
    while FIndex <= Length(FSource) do
    begin
      LChar := FSource[FIndex];

      if LChar = '"' then
      begin
        LParts.Add(Copy(FSource, LStart, FIndex - LStart));
        Inc(FIndex);
        Exit(LParts.Join);
      end;

      if Ord(LChar) < 32 then
      begin
        Problem('Unescaped control inside JSON string');
      end;

      if LChar = '\' then
      begin
        LParts.Add(Copy(FSource, LStart, FIndex - LStart));
        Inc(FIndex);

        if FIndex > Length(FSource) then
        begin
          Problem('Incomplete JSON escape');
        end;
        LChar := FSource[FIndex];
        Inc(FIndex);
        case LChar of
          '"', '\', '/':
            begin
              LScalar := Ord(LChar);
            end;
          'b':
            begin
              LScalar := 8;
            end;
          'f':
            begin
              LScalar := 12;
            end;
          'n':
            begin
              LScalar := 10;
            end;
          'r':
            begin
              LScalar := 13;
            end;
          't':
            begin
              LScalar := 9;
            end;
          'u':
            begin
              LScalar := HexQuad;

              if (LScalar >= $d800) and (LScalar <= $dbff) then
              begin

                if Copy(FSource, FIndex, 2) <> '\u' then
                begin
                  Problem('High surrogate requires a low surrogate escape');
                end;
                Inc(FIndex, 2);
                LLow := HexQuad;

                if (LLow < $dc00) or (LLow > $dfff) then
                begin
                  Problem('High surrogate requires a low surrogate');
                end;
                LScalar := $10000 + (LScalar - $d800) * 1024 + (LLow - $dc00);
              end
              else if (LScalar >= $dc00) and (LScalar <= $dfff) then
              begin
                Problem('Unpaired low surrogate');
              end;
            end;
          else
            begin
              Problem('Unknown JSON escape');
              LScalar := 0;
            end;
        end;
        LParts.Add(NyxScalarText(LScalar));
        LStart := FIndex;
      end
      else
      begin
        { Copy full raw runs; a native UTF-8 byte is not an ANSI character. }
        Inc(FIndex);
      end;
    end;
    Problem('Unterminated JSON string');
    Result := '';
  finally
    LParts.Free;
  end;
end;

function TNyxJSONReader.ReadNumber: TJSONData;
var
  LStart: Integer;
  LNumber: Double;
begin
  LStart := FIndex;
  while (FIndex <= Length(FSource)) and
    (FSource[FIndex] in ['0'..'9', '-', '+', '.', 'e', 'E']) do
  begin
    Inc(FIndex);
  end;

  if not TryNyxStateNumber(Copy(FSource, LStart, FIndex - LStart), LNumber) then
  begin
    Problem('JSON number requires a complete finite decimal');
  end;
  Result := TNyxJSONDecimalNumber.Create(Copy(FSource, LStart, FIndex - LStart), LNumber);
end;

function TNyxJSONReader.ReadObject(ADepth: Integer): TJSONObject;
var
  LKeys: TNyxStrings;
  LKey: TNyxText;
  LChild: TJSONData;
begin
  Require('{');
  Result := TJSONObject.Create;
  LKeys := nil;
  try
    try
      LKeys := TNyxStrings.Create;
      SkipSpace;

      if (FIndex <= Length(FSource)) and (FSource[FIndex] = '}') then
      begin
        Inc(FIndex);
        Exit;
      end;
      repeat
        LKey := ReadString;

        if LKeys.IndexOf(LKey) >= 0 then
        begin
          Problem('Duplicate JSON object key: ' + LKey);
        end;

        if LKeys.Count >= NyxMaximumJSONMembers then
        begin
          Problem('JSON object exceeds 1024 member budget');
        end;
        LKeys.Add(LKey);
        Require(':');
        LChild := ReadValue(ADepth + 1);
        try
          Result.Add(LKey, LChild);
          LChild := nil;
        finally
          LChild.Free;
        end;
        SkipSpace;

        if (FIndex <= Length(FSource)) and (FSource[FIndex] = '}') then
        begin
          Inc(FIndex);
          Break;
        end;
        Require(',');
      until False;
    except
      Result.Free;
      raise;
    end;
  finally
    LKeys.Free;
  end;
end;

function TNyxJSONReader.ReadArray(ADepth: Integer): TJSONArray;
var
  LChild: TJSONData;
begin
  Require('[');
  Result := TJSONArray.Create;
  try
    SkipSpace;

    if (FIndex <= Length(FSource)) and (FSource[FIndex] = ']') then
    begin
      Inc(FIndex);
      Exit;
    end;
    repeat
      LChild := ReadValue(ADepth + 1);
      try
        Result.Add(LChild);
        LChild := nil;
      finally
        LChild.Free;
      end;
      SkipSpace;

      if (FIndex <= Length(FSource)) and (FSource[FIndex] = ']') then
      begin
        Inc(FIndex);
        Break;
      end;
      Require(',');
    until False;
  except
    Result.Free;
    raise;
  end;
end;

function TNyxJSONReader.ReadValue(ADepth: Integer): TJSONData;
begin
  SkipSpace;

  if FIndex > Length(FSource) then
  begin
    Problem('JSON value is required');
  end;

  if (ADepth > NyxMaximumJSONDepth) and (FSource[FIndex] in ['{', '[']) then
  begin
    Problem('JSON exceeds nesting budget');
  end;
  case FSource[FIndex] of
    '{':
      begin
        Result := ReadObject(ADepth);
      end;
    '[':
      begin
        Result := ReadArray(ADepth);
      end;
    '"':
      begin
        {$ifdef PAS2JS}
        Result := TNyxJSONString.Create(ReadString);
        {$else}
        Result := TJSONString.Create(ReadString);
        {$endif}
      end;
    '-', '0'..'9':
      begin
        Result := ReadNumber;
      end;
    't':
      begin
        Word('true');
        Result := TJSONBoolean.Create(True);
      end;
    'f':
      begin
        Word('false');
        Result := TJSONBoolean.Create(False);
      end;
    'n':
      begin
        Word('null');
        Result := TJSONNull.Create;
      end;
    else
      begin
        Problem('Invalid JSON value');
        Result := nil;
      end;
  end;
end;

function TNyxJSONReader.Read: TJSONData;
begin
  Result := ReadValue(1);
  try
    SkipSpace;

    if FIndex <= Length(FSource) then
    begin
      Problem('Trailing data after JSON value');
    end;
  except
    Result.Free;
    raise;
  end;
end;

function DecodeNyxJSON(const ASource: TNyxText): TJSONData;
var
  LReader: TNyxJSONReader;
begin
  LReader := TNyxJSONReader.Create(ASource);
  try
    Result := LReader.Read;
  finally
    LReader.Free;
  end;
end;

end.
