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

program nyx_menu_bar_observer;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.test.browser.pipe;

var
  LHost: TNyxBrowserPipe;
  LStarted: QWord;
  LRequest: TNyxText;
  LLastRequest: TNyxText;
  LCaptured: Boolean;
  LWidth: Integer;

begin
  LHost := nil;
  try

    if ParamCount <> 3 then
    begin
      raise Exception.Create('Use menu bar observer <HTTP fixture URL> <new evidence directory> <CSS width>');
    end;
    LWidth := StrToInt(ParamStr(3));
    LHost := TNyxBrowserPipe.Create(ParamStr(1), ParamStr(2), LWidth);
    LStarted := GetTickCount64;
    LCaptured := False;
    LLastRequest := '';
    repeat

      if (LHost.RuntimeError <> '') or (LHost.Attribute('data-menu-bar') = 'failed') then
      begin
        LHost.Capture('failure');
        raise Exception.Create('Actual menu bar refused / ' + LHost.Attribute('data-event-error'));
      end;

      if not LCaptured and
        (LHost.Attribute('data-capture-checkpoint') = 'menu-bar-family') then
      begin
        LHost.Capture('menu-bar-family');
        LHost.SetAttribute('data-capture-observed', 'menu-bar-family');
        LCaptured := True;
      end;
      LRequest := LHost.Attribute('data-host-tab-request');

      if (LRequest <> '') and (LRequest <> LLastRequest) then
      begin

        if (LRequest <> 'forward') and (LRequest <> 'backward') then
        begin
          raise Exception.Create('Unknown fixture host Tab request');
        end;
        LHost.Tab(LRequest = 'backward');
        LHost.SetAttribute('data-host-tab-observed', LRequest);
        LLastRequest := LRequest;
      end;

      if LHost.Attribute('data-menu-bar') = 'passed' then
      begin
        Break;
      end;

      if GetTickCount64 - LStarted > 90000 then
      begin
        raise Exception.Create('Menu bar real-clock qualification timed out');
      end;
      Sleep(30);
    until False;
    LHost.Capture('completed');
    WriteLn('PASS ', LHost.Attribute('data-menu-bar-checks'),
      ' actual HTTP menu bar checks / CSS ', LWidth, ' / real host Tab');
    FreeAndNil(LHost);
  except
    on E: Exception do
    begin
      LHost.Free;
      WriteLn('FAIL ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
