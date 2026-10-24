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
program nyx_extension_reference_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, fpjson, nyx.text, nyx.json, nyx.data
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

{ The unchanged data factory is the independent admission oracle. It still
  formats/parses JSON and projects its admitted text, including the read budget.
  The new reference must make the same decision and preserve the exact name. }
procedure CompareAdmission(const AText: TNyxText);
var
  LExpected: TNyxText;
  LReference: TNyxExtensionRef;
  LData: TNyxDataValue;
  LOldAccepted: Boolean;
  LNewAccepted: Boolean;
begin
  LOldAccepted := False;
  LNewAccepted := False;
  LReference := NyxExtension('unchanged');
  try
    LData := NyxData(AText);
    LExpected := LData.AsText;
    LOldAccepted := True;
  except
    on ENyxJSON do
    begin
      { Refusal is compared below; no malformed text becomes an identity. }
    end;
  end;
  try
    LReference := NyxExtension(AText);
    LNewAccepted := True;
  except
    on ENyxJSON do
    begin
      { Failed admission must leave the existing caller reference untouched. }
    end;
  end;
  Check(LOldAccepted = LNewAccepted, 'Reference admission differs from the JSON-text contract');

  if LOldAccepted then
  begin
    Check((LReference.Name = AText) and (LReference.Copy.Name = LExpected),
      'Reference preserves exact text without normalization or truncation');
  end
  else
  begin
    Check(LReference.Name = 'unchanged', 'Failed identity construction preserves prior reference');
  end;
end;

{ Verify encoded UTF-8 bytes independently of the new escaped-length routine.
  This deliberately constructs/decodes the ordinary formatter's output. }
procedure CompareQuotedBytes(const AText: TNyxText; AStrict: Boolean);
var
  LQuoted: TNyxText;
  LIndex: Integer;
  LScalar: Integer;
  LExpected: Integer;
begin
  LQuoted := EncodeNyxJSONString(AText, AStrict);
  LExpected := 0;
  LIndex := 1;
  while LIndex <= Length(LQuoted) do
  begin
    Check(NyxNextScalar(LQuoted, LIndex, LScalar), 'Formatter emits complete Unicode scalars');

    if LScalar < $80 then
    begin
      Inc(LExpected);
    end
    else if LScalar < $800 then
    begin
      Inc(LExpected, 2);
    end
    else if LScalar < $10000 then
    begin
      Inc(LExpected, 3);
    end
    else
    begin
      Inc(LExpected, 4);
    end;
  end;
  Check(NyxJSONStringByteLength(AText, AStrict) = LExpected,
    'Quoted UTF-8 byte count matches independently decoded formatter output');
end;

procedure Run;
var
  LStrict: Boolean;
  LOriginalStrict: Boolean;
  LIndex: Integer;
  LText: TNyxText;
  LInvalid: TNyxText;
  LStore: TNyxExtensions;
  LReference: TNyxExtensionRef;
begin
  LOriginalStrict := TJSONString.StrictEscaping;
  try
    for LStrict := False to True do
    begin
      TJSONString.StrictEscaping := LStrict;
      CompareAdmission('');
      CompareAdmission('nyx.disclosure');
      CompareAdmission('A/a="\');
      for LIndex := 0 to 127 do
      begin
        LText := NyxScalarText(LIndex);
        CompareAdmission(LText);
        CompareQuotedBytes(LText, LStrict);
      end;
      LText := TNyxText('é/é/🌙/') + NyxScalarText(0) + 'end';
      { Browser storage units differ from UTF-8 bytes for this corpus. }
      CompareQuotedBytes(LText, LStrict);
      LReference := NyxExtension(LText);
      Check(LReference.Name = NyxData(LText).AsText,
        'Supplementary, composed/decomposed and NUL names remain exact');
      {$ifdef PAS2JS}
      LInvalid := Chr($d800);
      CompareAdmission(LInvalid);
      LInvalid := Chr($dc00);
      CompareAdmission(LInvalid);
      LInvalid := Chr($d800) + 'x';
      CompareAdmission(LInvalid);
      {$else}
      SetLength(LInvalid, 1);
      LInvalid[1] := AnsiChar($80);
      CompareAdmission(LInvalid);
      SetLength(LInvalid, 2);
      LInvalid[1] := AnsiChar($c0);
      LInvalid[2] := AnsiChar($af);
      CompareAdmission(LInvalid);
      SetLength(LInvalid, 3);
      LInvalid[1] := AnsiChar($ed);
      LInvalid[2] := AnsiChar($a0);
      LInvalid[3] := AnsiChar($80);
      CompareAdmission(LInvalid);
      SetLength(LInvalid, 4);
      LInvalid[1] := AnsiChar($f4);
      LInvalid[2] := AnsiChar($90);
      LInvalid[3] := AnsiChar($80);
      LInvalid[4] := AnsiChar($80);
      CompareAdmission(LInvalid);
      {$endif}
      { Keep the original complete 4 MiB budget. Escaped NUL and strict solidus
        must be counted by their wire length, rather than raw storage length. }
      LText := StringOfChar('a', NyxMaximumJSONBytes - 2);
      CompareAdmission(LText);
      CompareAdmission(LText + 'a');
      LText := StringOfChar(#0, (NyxMaximumJSONBytes - 2) div 6) + 'ab';
      CompareAdmission(LText);
      CompareAdmission(LText + NyxScalarText(0));
      LText := StringOfChar('/', (NyxMaximumJSONBytes - 2) div 2);
      CompareAdmission(LText);
      CompareAdmission(LText + '/');
    end;
  finally
    TJSONString.StrictEscaping := LOriginalStrict;
  end;
  LStore := TNyxExtensions.Create(nesNode);
  try
    LReference := NyxExtension(TNyxText('🌙=') + NyxScalarText(0));
    LStore.SetValue(LReference, NyxData('accepted'));
    Check(LStore.Has(LReference.Copy) and
      (LStore.Value(NyxExtension(LReference.Name)).AsText = 'accepted'),
      'Admitted copied identity reaches the existing exact extension store');
    Check(not LStore.Has(NyxExtension('🌙=')), 'Embedded NUL remains part of lookup identity');
  finally
    LStore.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' extension reference checks';
    document.body.setAttribute('data-result', 'passed');
    document.body.setAttribute('data-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' extension reference checks');
    {$endif}
  except
    on LError: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-result', 'failed');
      document.body.setAttribute('data-error', LError.Message);
      {$else}
      WriteLn(StdErr, 'FAIL ', LError.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
