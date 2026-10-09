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

unit nyx.test.json;

{$mode delphi}{$H+}
{$codepage utf8}

interface

function RunNyxJSONTests: Integer;

implementation

uses
  SysUtils,
  fpjson,
  nyx.text,
  nyx.json;

procedure Check(ACondition: Boolean; const AReason: TNyxText; var ACount: Integer);
begin

  if not ACondition then
  begin
    raise ENyxJSON.Create('FAIL JSON: ' + AReason);
  end;
  Inc(ACount);
end;

procedure Reject(const ASource: TNyxText; var ACount: Integer);
var
  LData: TJSONData;
  LRejected: Boolean;
begin
  LData := nil;
  LRejected := False;
  try
    try
      LData := DecodeNyxJSON(ASource);
    except
      on LException: ENyxJSON do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'strict malformed/over-budget input rejection', ACount);
  finally
    LData.Free;
  end;
end;

function RunNyxJSONTests: Integer;
const
  CInvalid: array[0..23] of TNyxText = (
    '', 'true trailing', 'True', 'NULL', '+1', '01', '.5', '1.',
    '1e', '1e9999', 'NaN', 'Infinity', '[1,]', '[,1]', '{"x":1,}',
    '{"x" 1}', '{"x":}', '{"x":1,"x":2}', '{"x":1,"\u0078":2}',
    '"unterminated', '"\q"', '"\uXX00"', '"\ud800"', '"\udc00"');
var
  LData: TJSONData;
  LExpected: TNyxText;
  LIndex: Integer;
  LText: TNyxText;
  LParts: TNyxStrings;
  LString: TJSONString;
  LClone: TJSONData;
  LStrict: Boolean;
begin
  Result := 0;
  { Preserve fpjson's established wire spelling across all control characters,
    mixed raw Unicode and both solidus policies. A decoded/cloned tree must use
    the same public formatter without retaining its original reader. }
  LParts := TNyxStrings.Create;
  try
    for LIndex := 0 to 31 do
    begin
      LParts.Add(NyxScalarText(LIndex));
    end;
    LText := LParts.Join + ' " / \ English 🌙 €';
  finally
    LParts.Free;
  end;
  LString := TJSONString.Create(LText);
  LStrict := TJSONString.StrictEscaping;
  try
    TJSONString.StrictEscaping := False;
    LExpected := LString.AsJSON;
    Check(EncodeNyxJSONString(LText) = LExpected,
      'run formatter retains existing ordinary escape spelling', Result);
    LData := DecodeNyxJSON(LExpected);
    try
      Check((LData.AsString = LText) and (LData.AsJSON = LExpected),
        'decoded string retains exact raw and escaped text', Result);
      LClone := LData.Clone;
      try
        LData.Free;
        LData := nil;
        Check((LClone.AsString = LText) and (LClone.AsJSON = LExpected),
          'decoded clone retains exact formatter and independent lifetime', Result);
        TJSONString.StrictEscaping := True;
        Check((EncodeNyxJSONString(LText, True) = LString.AsJSON) and
          (LClone.AsJSON = LString.AsJSON),
          'explicit and tree strict escaping retain fpjson spelling', Result);
      finally
        LClone.Free;
      end;
    finally
      LData.Free;
    end;
  finally
    TJSONString.StrictEscaping := LStrict;
    LString.Free;
  end;
  LData := DecodeNyxJSON(
    '"quote\" slash\/ path\\ tab\t line\n null\u0000 moon\ud83c\udf19"');
  try
    LExpected := TNyxText('quote" slash/ path\ tab') + TNyxText(#9) +
      TNyxText(' line') + TNyxText(#10) + ' null' + NyxScalarText(0) + TNyxText(' moon🌙');
    Check((LData.JSONType = jtString) and (LData.AsString = LExpected),
      'every escaped run retains Unicode/NUL meaning', Result);
  finally
    LData.Free;
  end;
  LData := DecodeNyxJSON('"controls\b\f\r \u0041\u20ac"');
  try
    Check(LData.AsString = TNyxText('controls') + TNyxText(#8#12#13) + TNyxText(' A€'),
      'remaining JSON controls and BMP scalars decode exactly', Result);
  finally
    LData.Free;
  end;
  LData := DecodeNyxJSON(' ' + #9#10#13 +
    '{"A":true,"a":false,"array":[null,-42,1.25],"quote":"{}[]"} ' + #10);
  try
    Check((LData.JSONType = jtObject) and (LData.Count = 4) and
      TJSONObject(LData).Booleans['A'] and not TJSONObject(LData).Booleans['a'],
      'object keys remain case-sensitive and whitespace is explicit', Result);
    Check((TJSONObject(LData).Arrays['array'].Items[0].JSONType = jtNull) and
      (TJSONObject(LData).Arrays['array'].Items[1].AsFloat = -42) and
      (TJSONObject(LData).Arrays['array'].Items[2].AsFloat = 1.25),
      'scalar arrays preserve finite numeric and null values', Result);
  finally
    LData.Free;
  end;
  for LIndex := 0 to High(CInvalid) do
  begin
    Reject(CInvalid[LIndex], Result);
  end;
  Reject('"raw' + #9 + 'tab"', Result);
  LText := StringOfChar('[', NyxMaximumJSONDepth) + '0' +
    StringOfChar(']', NyxMaximumJSONDepth);
  LData := DecodeNyxJSON(LText);
  try
    Check(LData.JSONType = jtArray, 'maximum admitted nesting is usable', Result);
  finally
    LData.Free;
  end;
  Reject('[' + LText + ']', Result);
  LParts := TNyxStrings.Create;
  try
    for LIndex := 0 to NyxMaximumJSONMembers - 1 do
    begin
      LParts.Add('"key-' + IntToStr(LIndex) + '":null');
    end;
    LText := '{' + LParts.Join(',') + '}';
    LData := DecodeNyxJSON(LText);
    try
      Check(LData.Count = NyxMaximumJSONMembers,
        'member boundary accepts every admitted unique key', Result);
    finally
      LData.Free;
    end;
    Reject('{' + LParts.Join(',') + ',"extra":null}', Result);
  finally
    LParts.Free;
  end;
  { Same supplementary payload on both targets. Browser Length is below 4 MiB;
    its UTF-8 content size still exceeds the shared admission limit. }
  LText := '界';
  for LIndex := 1 to 21 do
  begin
    LText := LText + LText;
  end;
  Reject('"' + LText + '"', Result);
end;

end.
