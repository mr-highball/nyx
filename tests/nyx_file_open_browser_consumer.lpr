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

program nyx_file_open_browser_consumer;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.test.browser.pipe, nyx.test.files.openfixture;

var
  LHost: TNyxBrowserPipe;
  LStarted: QWord;
  LSource: TNyxText;
  LWidth: Integer;

{ Observe only fixture receipts; selection and reads still pass through trusted
  browser input and the actual file input/FileReader implementation. This driver
  supplies four owned disk files, never a substituted File or reader result. }
procedure WaitStage(const AValue: TNyxText);
begin
  LStarted := GetTickCount64;
  repeat

    if (LHost.Attribute('data-file-open-controls') = 'failed') or
      (LHost.RuntimeError <> '') then
    begin
      raise Exception.Create('File opening failed: ' +
        String(LHost.Attribute('data-file-open-error') + LHost.RuntimeError));
    end;

    if GetTickCount64 - LStarted > 15000 then
    begin
      LHost.Capture('open-timeout');
      raise Exception.Create('File opening did not reach ' + String(AValue));
    end;

    if LHost.Attribute('data-file-open-stage') = AValue then
    begin
      Exit;
    end;
    Sleep(20);
  until False;
end;

begin
  LHost := nil;
  try

    if ParamCount <> 4 then
    begin
      raise Exception.Create('Supply loopback fixture URL, fresh output directory, CSS width, owned input directory');
    end;
    LWidth := StrToInt(ParamStr(3));

    if not DirectoryExists(ParamStr(4)) then
    begin
      raise Exception.Create('Owned input fixture directory is unavailable');
    end;
    LSource := TNyxText(ExpandFileName(ParamStr(4))) + TNyxText(PathDelim);
    LHost := TNyxBrowserPipe.Create(ParamStr(1), ParamStr(2), LWidth, 900);
    WaitStage('ready');
    LHost.Capture('before-open');
    LHost.PickResourceFile('[data-node="open-report"]', LSource + OpenFixtureLeaf);
    WaitStage('read');
    LHost.Capture('report-read');
    LHost.PickResourceFile('[data-node="open-report"]', LSource + 'large.bin');
    WaitStage('limited');
    LHost.PickResourceFile('[data-node="open-report"]', LSource + 'invalid.bin');
    WaitStage('invalid');
    LHost.PickResourceFile('[data-node="open-report"]', LSource + 'empty.txt');
    WaitStage('empty');

    if LHost.Attribute('data-file-open-controls') <> 'passed' then
    begin
      raise Exception.Create('Actual browser file read checks did not finish');
    end;
    LHost.Capture('file-open-complete');
    WriteLn('PASS ', String(LHost.Attribute('data-file-open-checks')),
      ' compiled browser file/read/metadata/graph checks, CSS ', LWidth);
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);

      if LHost <> nil then
      begin
        LHost.Capture('file-open-failed');
      end;
      ExitCode := 1;
    end;
  end;
  LHost.Free;
end.
