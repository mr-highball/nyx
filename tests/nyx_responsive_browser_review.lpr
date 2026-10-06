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
program nyx_responsive_browser_review;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, base64, nyx.text, nyx.data, nyx.test.browser.host;

type
  { This owner only observes bounded Pascal-fixture results and captures the
    actual rendered page. It never evaluates scripts, edits a design or uses
    accelerated clocks; ResizeObserver delivery occurs on ordinary browser frames. }
  TResponsiveReview = class(TNyxBrowserHost)
  public
    procedure Run;
  end;

procedure TResponsiveReview.Run;
var
  LStarted: QWord;
  LMarker: TNyxText;
begin
  LStarted := GetTickCount64;
  repeat
    Pump;
    LMarker := Attribute('data-nyx-responsive-controls');

    if LMarker = 'failed' then
    begin
      raise Exception.Create(Attribute('data-nyx-responsive-error'));
    end;

    if LMarker = 'passed' then
    begin
      Save('result.json', NyxObject([NyxField('checks',
        NyxData(StrToInt(Attribute('data-nyx-responsive-checks'))))]).ToJSON);
      Save('capture.png', DecodeStringBase64(Command('Page.captureScreenshot',
        NyxObject([NyxField('format', NyxData('png'))])).Field('data').AsText));
      WriteLn('PASS actual browser responsive controls / ',
        Attribute('data-nyx-responsive-checks'), ' checks');
      Exit;
    end;

    if GetTickCount64 - LStarted > 15000 then
    begin
      raise Exception.Create('Responsive fixture did not finish on ordinary browser frames');
    end;
    Sleep(10);
  until False;
end;

var
  LReview: TResponsiveReview;
begin
  LReview := nil;
  try

    if ParamCount <> 2 then
    begin
      raise Exception.Create('Supply isolated loopback fixture and artifact directory');
    end;
    LReview := TResponsiveReview.Create(ParamStr(1), ParamStr(2));
    LReview.Run;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
  LReview.Free;
end.
