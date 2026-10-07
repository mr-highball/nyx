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

program nyx_studio_help_observer;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.test.browser.pipe;

var
  GHost: TNyxBrowserPipe;

procedure WaitFor(const ASelector: TNyxText; AExists: Boolean = True);
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat

    if GHost.RuntimeError <> '' then
    begin
      raise Exception.Create('Actual Studio raised; inspect runtime-error.json');
    end;

    if (GHost.ElementHTML(ASelector) <> '') = AExists then
    begin
      Exit;
    end;

    if GetTickCount64 - LStarted > 30000 then
    begin
      raise Exception.Create('Studio help did not reach expected host state');
    end;
    Sleep(50);
  until False;
end;

var
  LWidth: Integer;
begin
  try

    if ParamCount <> 3 then
    begin
      raise Exception.Create('Use help observer <loopback candidate URL> <capture directory> <CSS width>');
    end;
    LWidth := StrToInt(ParamStr(3));
    GHost := TNyxBrowserPipe.Create(ParamStr(1), ParamStr(2), LWidth);

    if LWidth <= 960 then
    begin
      WaitFor('[data-node=action-panel-inspector]');
      GHost.Click('[data-node=action-panel-inspector]');
    end;
    WaitFor('[data-node=action-component-help]');
    GHost.Click('[data-node=action-component-help]');
    WaitFor('.nyx-popover:popover-open [data-node=component-help-description]');
    GHost.Capture('studio-component-help');
    GHost.Click('.nyx-popover:popover-open [data-node=component-help-close]');
    WaitFor('.nyx-popover:popover-open', False);
    GHost.Capture('studio-help-closed');
    FreeAndNil(GHost);
    WriteLn('PASS actual full browser Studio help open/close / CSS ', LWidth);
  except
    on E: Exception do
    begin

      if GHost <> nil then
      begin
        GHost.Capture('failure');
      end;
      GHost.Free;
      WriteLn('FAIL ', E.Message);
      ExitCode := 1;
    end;
  end;
end.

