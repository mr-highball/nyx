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
program nyx_time_reconstruction;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.model, nyx.codec, nyx.codegen, nyx.test.times, nyx.generated.time
  {$ifdef PAS2JS}, Web{$endif};

var
  LExpected: TNyxDocument;
  LActual: TNyxDocument;

{ Exact wire/source admission must agree with actual compiled constructors.
  A bounded first difference diagnoses ownership/order errors without dumping
  a complete application or weakening the comparison to semantic equivalence. }
procedure RequireEqual(const AExpected, AActual, AMeaning: TNyxText);
var
  LPosition: Integer;
begin

  if AExpected = AActual then
  begin
    Exit;
  end;
  LPosition := 1;
  while (LPosition <= Length(AExpected)) and (LPosition <= Length(AActual)) and
    (AExpected[LPosition] = AActual[LPosition]) do
  begin
    Inc(LPosition);
  end;
  raise Exception.Create(AMeaning + ' differs at ' + IntToStr(LPosition) +
    ': expected ' + Copy(AExpected, LPosition, 100) +
    ' / actual ' + Copy(AActual, LPosition, 100));
end;

begin
  LExpected := nil;
  LActual := nil;
  try
    LExpected := CreateNyxTimeFixture;
    LActual := nyx.generated.time.BuildNyxDocument;

    RequireEqual(TNyxCodec.Encode(LExpected), TNyxCodec.Encode(LActual), 'Compiled clock wire');
    RequireEqual(TNyxCodegen.Generate(LExpected), TNyxCodegen.Generate(LActual), 'Compiled clock source');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-time-reconstruction', 'passed');
    {$else}
    WriteLn('PASS exact compiled typed clock companion reconstruction');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-time-reconstruction', 'failed');
      document.body.setAttribute('data-time-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
  LActual.Free;
  LExpected.Free;
end.
