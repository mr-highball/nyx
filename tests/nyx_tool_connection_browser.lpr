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

program nyx_tool_connection_browser;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.test.browser.pipe;

var
  LBrowser: TNyxBrowserPipe;
  LRequest: TNyxText;
  LLast: TNyxText;
  LState: TNyxText;
  LStarted: QWord;
  LKeys: Integer;
  LDragRequest: TNyxText;
  LLastDrag: TNyxText;
  LDrags: Integer;
  LCaptured: Boolean;
begin
  LBrowser := nil;
  LLast := '';
  LKeys := 0;
  LLastDrag := '';
  LDrags := 0;
  LCaptured := False;
  try

    if ParamCount <> 3 then
    begin
      raise Exception.Create('Supply owned loopback fixture URL, fresh evidence directory and CSS width');
    end;
    { Constructor admission includes this deliberately synchronous qualification
      fixture's first complete model/guard stage. Readiness still requires all
      later assertions, never merely a load event. Preserve its real clock. }
    LBrowser := TNyxBrowserPipe.Create(ParamStr(1), ParamStr(2), StrToInt(ParamStr(3)),
      900, nil, nboLoopback, 120000);
    LStarted := GetTickCount64;
    repeat
      LState := LBrowser.Attribute('data-tool-canvas');

      if LState = 'failed' then
      begin
        raise Exception.Create(String(LBrowser.Attribute('data-tool-canvas-error')));
      end;

      if LState = 'passed' then
      begin

        if LBrowser.Attribute('data-tool-connection-checks') = '' then
        begin
          raise Exception.Create('Connection consumer was not enabled');
        end;
        Break;
      end;
      LRequest := LBrowser.Attribute('data-tool-key');

      if (LRequest <> '') and (LRequest <> LLast) then
      begin
        { The Pascal fixture focuses its real button. Chromium performs ordinary
          Enter activation; no synthetic DOM keyboard or click event is injected.
          Only the owned fixture handshake is acknowledged through body metadata. }
        LBrowser.Key(nbkEnter);
        Inc(LKeys);
        LLast := LRequest;
        LBrowser.SetAttribute('data-tool-key-ack', LRequest);
      end;
      LDragRequest := LBrowser.Attribute('data-tool-drag');

      if (LDragRequest <> '') and (LDragRequest <> LLastDrag) then
      begin
        LBrowser.DragBetween('[data-runtime-id="' + LBrowser.Attribute('data-tool-drag-source') + '"]',
          '[data-runtime-id="' + LBrowser.Attribute('data-tool-drag-target') + '"]');
        Inc(LDrags);
        LLastDrag := LDragRequest;
        LBrowser.SetAttribute('data-tool-drag-ack', LDragRequest);
      end;

      if not LCaptured and (LBrowser.Attribute('data-tool-capture') = 'connected') then
      begin
        LBrowser.Capture('connected');
        LCaptured := True;
        LBrowser.SetAttribute('data-tool-capture-ack', 'connected');
      end;

      if GetTickCount64 - LStarted > 180000 then
      begin
        raise Exception.Create('Tool connection input exceeded its real-clock budget at stage ' +
          String(LBrowser.Attribute('data-tool-canvas-stage')));
      end;
      Sleep(10);
    until False;
    LBrowser.Capture('capture');
    WriteLn('PASS ', LBrowser.Attribute('data-tool-connection-checks'),
      ' Tool Canvas connection checks / ', LKeys, ' actual Chromium key activations / ',
      LDrags, ' actual browser drags');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);

      if LBrowser <> nil then
      begin
        LBrowser.CaptureRuntimeError;
        LBrowser.Capture('failure');
      end;
      ExitCode := 1;
    end;
  end;
  LBrowser.Free;
end.
