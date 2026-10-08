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

program nyx_theme_generated;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.model, nyx.design.tokens, nyx.generated.theme
  {$ifdef PAS2JS}, Web{$endif};

var
  LDocument: TNyxDocument;
  LTokens: TNyxThemeTokens;

begin
  LDocument := BuildNyxDocument;
  try
    LTokens := NyxDeclaredThemeTokens(LDocument);

    if (LTokens.ToData.Count <> 3) or
      (LTokens.ColorValue(ntcAccent).ToText <> '#AbCdEf') or
      (LTokens.MetricValue(ntmFontSize) <> 22) or
      (LTokens.MetricValue(ntmRadius) <> 333) or
      (LDocument.Find('welcome-title').Prop('text') <> 'Create with confidence.') then
    begin
      raise Exception.Create('Compiled theme differs from the exact emitted semantic candidate');
    end;
    WriteLn('PASS / compiled theme / 5 checks');
    {$ifdef PAS2JS}document.body.setAttribute('data-test-result', 'passed');{$endif}
  finally
    LDocument.Free;
  end;
end.
