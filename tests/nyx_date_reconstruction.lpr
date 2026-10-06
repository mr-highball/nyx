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

program nyx_date_reconstruction;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$IFDEF PAS2JS}Web,{$ENDIF}
  SysUtils, nyx.model, nyx.codec, nyx.codegen, nyx.test.dates,
  nyx.generated.view, nyx.generated.date;

var
  LExpected: TNyxDocument;
  LActual: TNyxDocument;

begin
  LExpected := nil;
  LActual := nil;
  try
    LExpected := nyx.generated.view.BuildNyxDocument;
    ConfigureNyxDateReview(LExpected);
    LActual := nyx.generated.date.BuildNyxDocument;

    if (TNyxCodec.Encode(LActual) <> TNyxCodec.Encode(LExpected)) or
      (TNyxCodegen.Generate(LActual) <> TNyxCodegen.Generate(LExpected)) then
    begin
      raise Exception.Create('Typed date reconstruction differs from the semantic companion/enrichment');
    end;
    {$IFDEF PAS2JS}
    document.body.setAttribute('data-date-reconstruction', 'passed');
    {$ELSE}
    WriteLn('PASS exact compiled typed date companion reconstruction');
    {$ENDIF}
  except
    on LException: Exception do
    begin
      {$IFDEF PAS2JS}
      document.body.setAttribute('data-date-reconstruction', 'failed');
      document.body.setAttribute('data-event-error', LException.Message);
      {$ELSE}
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
      {$ENDIF}
    end;
  end;
  LActual.Free;
  LExpected.Free;
end.
