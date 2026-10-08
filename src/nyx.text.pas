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

unit nyx.text;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  SysUtils;

type
  { Browser String is Unicode. Native UTF8String makes concatenation and API
    admission independent of the user's Windows ANSI codepage. }
  {$IFDEF PAS2JS}
  TNyxText = String;
  {$ELSE}
  TNyxText = UTF8String;
  {$ENDIF}

  { Retained text accounting needs wider intermediates than native 32-bit
    lengths. pas2js NativeInt uses number arithmetic; admitted source/history
    budgets keep these integral counts within JavaScript's exact range. Int64
    is not implemented by pas2js, so keep it solely on native targets. }
  {$IFDEF PAS2JS}
  TNyxTextBytes = NativeInt;
  {$ELSE}
  TNyxTextBytes = Int64;
  {$ENDIF}

  { Ordered portable text storage. The native RTL's TStringList accepts the
    system ANSI String type; on Windows that can convert UTF-8 captions into
    question marks before they reach JSON or a renderer. This collection keeps
    Nyx's UTF-8 TNyxText contract without changing the process-wide codepage.
    pas2js keeps ordinary Unicode strings through the same API.

    Names/IndexOfName support the node's existing name=value representation.
    Exact comparisons preserve case-sensitive property and document identities.
    Text accepts CR, LF and CRLF and emits LF. The collection owns its strings;
    Assign copies them, so subsequent mutation never aliases another collection. }
  TNyxStrings = class
  private
    FItems: array of TNyxText;
    FCount: Integer;
    procedure CheckIndex(AIndex: Integer);
    function GetString(AIndex: Integer): TNyxText;
    procedure SetString(AIndex: Integer; const AValue: TNyxText);
    function GetName(AIndex: Integer): TNyxText;
    function GetText: TNyxText;
    procedure SetText(const AValue: TNyxText);
  public
    function Add(const AValue: TNyxText): Integer;
    procedure Delete(AIndex: Integer);
    procedure Clear;
    procedure Assign(ASource: TNyxStrings);
    { Exchange owned storage without allocation or text conversion. Both objects
      must exist. Prepared model installers use this only after admission; the
      displaced strings remain owned by the detached candidate until retirement. }
    procedure ExchangeStorage(AOther: TNyxStrings);
    function IndexOf(const AValue: TNyxText): Integer;
    { Exact first name match, without allocating candidate name substrings for
      ordinary nonempty names. Equality compares native UTF-8 bytes / browser
      UTF-16 units; it never normalizes Unicode, case or embedded NUL. Empty
      names retain Names semantics, including items without a separator. }
    function IndexOfName(const AName: TNyxText): Integer;
    { Join admitted items without an extra trailing separator. Native allocation
      is sized once; browser uses its standard array join. Embedded NUL and
      supplementary text retain their exact storage units on both targets. }
    function Join(const ASeparator: TNyxText = ''): TNyxText;
    property Count: Integer read FCount;
    property Strings[AIndex: Integer]: TNyxText read GetString write SetString; default;
    property Names[AIndex: Integer]: TNyxText read GetName;
    property Text: TNyxText read GetText write SetText;
  end;

{ Read one Unicode scalar from a native UTF-8 or browser UTF-16 text value.
  Index is one-based in that target's storage units; successful reads advance it.
  False means malformed encoding or an out-of-range index and leaves Index intact.
  Overlong UTF-8, surrogate scalars and unpaired UTF-16 surrogates are refused. }
function NyxNextScalar(const AText: TNyxText; var AIndex: Integer;
  out AScalar: Integer): Boolean;
{ Unicode White_Space, independent of RTL Trim implementation or locale. }
function NyxScalarWhitespace(AScalar: Integer): Boolean;
{ Encode one Unicode scalar, including U+0000. Surrogates/out-of-range values
  are refused. Native bytes are written into UTF8String directly, avoiding ANSI
  conversions and older WideString-to-UTF8 routines that treat NUL as an end. }
function NyxScalarText(AScalar: Integer): TNyxText;
{ Resolve one-based LF-delimited line/Unicode-scalar column to one-based storage
  offset (UTF-8 bytes natively, UTF-16 units in the browser). CRLF is one newline;
  columns clamp before its CR and missing lines clamp at EOF. Invalid coordinates
  or malformed encoding encountered in the scanned prefix raise instead of
  placing a caret inside a scalar. }
function NyxTextPosition(const AText: TNyxText; ALine, AColumn: Integer): Integer;

implementation

{$IFDEF PAS2JS}
uses
  JS;
{$ENDIF}

procedure TNyxStrings.ExchangeStorage(AOther: TNyxStrings);
var
  LItems: array of TNyxText;
  LCount: Integer;
begin
  LItems := FItems;
  FItems := AOther.FItems;
  AOther.FItems := LItems;
  LCount := FCount;
  FCount := AOther.FCount;
  AOther.FCount := LCount;
end;

function NyxTextPosition(const AText: TNyxText; ALine, AColumn: Integer): Integer;
var
  LIndex, LStart, LLine, LColumn, LScalar: Integer;
begin

  if (ALine < 1) or (AColumn < 1) then
  begin
    raise EArgumentException.Create('Text coordinates are one-based');
  end;
  LIndex := 1;
  LLine := 1;
  LColumn := 1;
  while LIndex <= Length(AText) do
  begin
    LStart := LIndex;

    if not NyxNextScalar(AText, LIndex, LScalar) then
    begin
      raise EArgumentException.Create('Malformed Unicode text');
    end;

    if LLine = ALine then
    begin

      if (LColumn >= AColumn) or (LScalar = 10) or
        ((LScalar = 13) and (LIndex <= Length(AText)) and (AText[LIndex] = #10)) then
      begin
        Exit(LStart);
      end;
      Inc(LColumn);
    end;

    if LScalar = 10 then
    begin
      Inc(LLine);
      LColumn := 1;
    end;
  end;
  Result := Length(AText) + 1;
end;

function NyxScalarText(AScalar: Integer): TNyxText;
begin

  if (AScalar < 0) or (AScalar > $10ffff) or
    ((AScalar >= $d800) and (AScalar <= $dfff)) then
  begin
    raise EArgumentException.Create('A Unicode scalar is required');
  end;
  {$IFDEF PAS2JS}

  if AScalar <= $ffff then
  begin
    Result := Chr(AScalar);
  end
  else
  begin
    Dec(AScalar, $10000);
    Result := Chr($d800 + (AScalar shr 10)) + Chr($dc00 + (AScalar and $3ff));
  end;
  {$ELSE}

  if AScalar <= $7f then
  begin
    SetLength(Result, 1);
    Result[1] := Chr(AScalar);
  end
  else if AScalar <= $7ff then
  begin
    SetLength(Result, 2);
    Result[1] := Chr($c0 or (AScalar shr 6));
    Result[2] := Chr($80 or (AScalar and $3f));
  end
  else if AScalar <= $ffff then
  begin
    SetLength(Result, 3);
    Result[1] := Chr($e0 or (AScalar shr 12));
    Result[2] := Chr($80 or ((AScalar shr 6) and $3f));
    Result[3] := Chr($80 or (AScalar and $3f));
  end
  else
  begin
    SetLength(Result, 4);
    Result[1] := Chr($f0 or (AScalar shr 18));
    Result[2] := Chr($80 or ((AScalar shr 12) and $3f));
    Result[3] := Chr($80 or ((AScalar shr 6) and $3f));
    Result[4] := Chr($80 or (AScalar and $3f));
  end;
  {$ENDIF}
end;

function NyxNextScalar(const AText: TNyxText; var AIndex: Integer;
  out AScalar: Integer): Boolean;
var
  LLead: Integer;
  LTrail: Integer;
  LUnits: Integer;
  {$IFNDEF PAS2JS}
  LMinimum: Integer;
  LOffset: Integer;
  {$ENDIF}
begin
  Result := False;
  AScalar := 0;

  if (AIndex < 1) or (AIndex > Length(AText)) then
  begin
    Exit;
  end;
  LLead := Ord(AText[AIndex]);
  LUnits := 1;
  {$IFDEF PAS2JS}

  if (LLead >= $d800) and (LLead <= $dbff) then
  begin

    if AIndex = Length(AText) then
    begin
      Exit;
    end;
    LTrail := Ord(AText[AIndex + 1]);

    if (LTrail < $dc00) or (LTrail > $dfff) then
    begin
      Exit;
    end;
    AScalar := $10000 + (LLead - $d800) * $400 + LTrail - $dc00;
    LUnits := 2;
  end
  else
  begin

    if (LLead >= $dc00) and (LLead <= $dfff) then
    begin
      Exit;
    end;
    AScalar := LLead;
  end;
  {$ELSE}
  LMinimum := 0;

  if LLead <= $7f then
  begin
    AScalar := LLead;
  end
  else if (LLead >= $c2) and (LLead <= $df) then
  begin
    AScalar := LLead and $1f;
    LUnits := 2;
    LMinimum := $80;
  end
  else if (LLead >= $e0) and (LLead <= $ef) then
  begin
    AScalar := LLead and $0f;
    LUnits := 3;
    LMinimum := $800;
  end
  else if (LLead >= $f0) and (LLead <= $f4) then
  begin
    AScalar := LLead and $07;
    LUnits := 4;
    LMinimum := $10000;
  end
  else
  begin
    Exit;
  end;

  if AIndex + LUnits - 1 > Length(AText) then
  begin
    Exit;
  end;
  for LOffset := 1 to LUnits - 1 do
  begin
    LTrail := Ord(AText[AIndex + LOffset]);

    if (LTrail < $80) or (LTrail > $bf) then
    begin
      Exit;
    end;
    AScalar := (AScalar shl 6) or (LTrail and $3f);
  end;

  if (AScalar < LMinimum) or (AScalar > $10ffff) or
    ((AScalar >= $d800) and (AScalar <= $dfff)) then
  begin
    Exit;
  end;
  {$ENDIF}
  Inc(AIndex, LUnits);
  Result := True;
end;

function NyxScalarWhitespace(AScalar: Integer): Boolean;
begin
  Result := ((AScalar >= 9) and (AScalar <= 13)) or (AScalar = 32) or
    (AScalar = $85) or (AScalar = $a0) or (AScalar = $1680) or
    ((AScalar >= $2000) and (AScalar <= $200a)) or (AScalar = $2028) or
    (AScalar = $2029) or (AScalar = $202f) or (AScalar = $205f) or (AScalar = $3000);
end;

procedure TNyxStrings.CheckIndex(AIndex: Integer);
begin

  if (AIndex < 0) or (AIndex >= FCount) then
  begin
    raise ERangeError.Create('Text index out of range');
  end;
end;

function TNyxStrings.GetString(AIndex: Integer): TNyxText;
begin
  CheckIndex(AIndex);
  Result := FItems[AIndex];
end;

procedure TNyxStrings.SetString(AIndex: Integer; const AValue: TNyxText);
begin
  CheckIndex(AIndex);
  FItems[AIndex] := AValue;
end;

function TNyxStrings.GetName(AIndex: Integer): TNyxText;
var
  LSeparator: Integer;
begin
  CheckIndex(AIndex);
  LSeparator := Pos('=', FItems[AIndex]);
  Result := '';

  if LSeparator > 0 then
  begin
    Result := Copy(FItems[AIndex], 1, LSeparator - 1);
  end;
end;

function TNyxStrings.Add(const AValue: TNyxText): Integer;
begin
  { Geometric growth avoids copying the entire array on every generated source
    line. Unused capacity contains empty strings and is invisible to callers. }

  if FCount = Length(FItems) then
  begin
    SetLength(FItems, FCount * 2 + 16);
  end;
  Result := FCount;
  FItems[FCount] := AValue;
  Inc(FCount);
end;

procedure TNyxStrings.Delete(AIndex: Integer);
var
  LIndex: Integer;
begin
  CheckIndex(AIndex);
  for LIndex := AIndex to FCount - 2 do
  begin
    FItems[LIndex] := FItems[LIndex + 1];
  end;
  Dec(FCount);
  FItems[FCount] := '';
end;

procedure TNyxStrings.Clear;
begin
  FItems := nil;
  FCount := 0;
end;

procedure TNyxStrings.Assign(ASource: TNyxStrings);
var
  LIndex: Integer;
begin

  if ASource = Self then
  begin
    Exit;
  end;

  if ASource = nil then
  begin
    raise EArgumentException.Create('Text source is required');
  end;
  Clear;
  SetLength(FItems, ASource.Count);
  for LIndex := 0 to ASource.Count - 1 do
  begin
    Add(ASource[LIndex]);
  end;
end;

function TNyxStrings.IndexOf(const AValue: TNyxText): Integer;
var
  LIndex: Integer;
begin
  for LIndex := 0 to FCount - 1 do
  begin

    if FItems[LIndex] = AValue then
    begin
      Exit(LIndex);
    end;
  end;
  Result := -1;
end;

function TNyxStrings.IndexOfName(const AName: TNyxText): Integer;
var
  LIndex: Integer;
  LNameLength: Integer;
  LUnit: Integer;
  LSeparator: Integer;
begin
  LNameLength := Length(AName);

  if LNameLength = 0 then
  begin
    { Names returns empty both for an empty prefix and for an unseparated
      item. Preserve that existing behavior rather than interpreting every
      item as a name/value pair. This uncommon path needs no name copy either. }
    for LIndex := 0 to FCount - 1 do
    begin
      LSeparator := Pos('=', FItems[LIndex]);

      if (LSeparator = 0) or (LSeparator = 1) then
      begin
        Exit(LIndex);
      end;
    end;
    Exit(-1);
  end;

  if Pos('=', AName) > 0 then
  begin
    { A name ends at the first separator and can never contain one. }
    Exit(-1);
  end;
  for LIndex := 0 to FCount - 1 do
  begin

    if Length(FItems[LIndex]) <= LNameLength then
    begin
      Continue;
    end;

    if FItems[LIndex][LNameLength + 1] <> '=' then
    begin
      Continue;
    end;
    LUnit := 1;
    while LUnit <= LNameLength do
    begin

      if FItems[LIndex][LUnit] <> AName[LUnit] then
      begin
        Break;
      end;
      Inc(LUnit);
    end;

    if LUnit > LNameLength then
    begin
      Exit(LIndex);
    end;
  end;
  Result := -1;
end;

function TNyxStrings.Join(const ASeparator: TNyxText): TNyxText;
var
  LIndex: Integer;
  {$IFDEF PAS2JS}
  LItems: array of TNyxText;
  {$ELSE}
  LLength: Integer;
  LOffset: Integer;
  {$ENDIF}
begin
  {$IFDEF PAS2JS}
  { Copy only admitted items; geometric spare capacity must not add separators. }
  SetLength(LItems, FCount);
  for LIndex := 0 to FCount - 1 do
  begin
    LItems[LIndex] := FItems[LIndex];
  end;
  Result := TJSArray(LItems).join(ASeparator);
  {$ELSE}
  LLength := 0;
  for LIndex := 0 to FCount - 1 do
  begin
    Inc(LLength, Length(FItems[LIndex]));

    if LIndex > 0 then
    begin
      Inc(LLength, Length(ASeparator));
    end;
  end;
  SetLength(Result, LLength);
  { FPC can reuse the caller's result buffer with a codepage-zero/ANSI tag.
    These are already exact UTF-8 bytes; label them without conversion so Copy,
    concatenation and the next typed consumer cannot interpret them as ANSI. }
  SetCodePage(RawByteString(Result), CP_UTF8, False);
  LOffset := 1;
  for LIndex := 0 to FCount - 1 do
  begin

    if (LIndex > 0) and (ASeparator <> '') then
    begin
      Move(ASeparator[1], Result[LOffset], Length(ASeparator));
      Inc(LOffset, Length(ASeparator));
    end;

    if FItems[LIndex] <> '' then
    begin
      Move(FItems[LIndex][1], Result[LOffset], Length(FItems[LIndex]));
      Inc(LOffset, Length(FItems[LIndex]));
    end;
  end;
  {$ENDIF}
end;

function TNyxStrings.GetText: TNyxText;
begin
  Result := Join(#10);

  if FCount > 0 then
  begin
    Result := Result + #10;
  end;
end;

procedure TNyxStrings.SetText(const AValue: TNyxText);
var
  LStart: Integer;
  LIndex: Integer;
begin
  Clear;
  LStart := 1;
  LIndex := 1;
  while LIndex <= Length(AValue) do
  begin

    if (AValue[LIndex] = #10) or (AValue[LIndex] = #13) then
    begin
      Add(Copy(AValue, LStart, LIndex - LStart));

      if (AValue[LIndex] = #13) and (LIndex < Length(AValue)) and
        (AValue[LIndex + 1] = #10) then
      begin
        Inc(LIndex);
      end;
      LStart := LIndex + 1;
    end;
    Inc(LIndex);
  end;

  if LStart <= Length(AValue) then
  begin
    Add(Copy(AValue, LStart, MaxInt));
  end;
end;

end.
