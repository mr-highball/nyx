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

program nyx_canvas_connector_input;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.test.browser.pipe;

var
  LHost: TNyxBrowserPipe;
  LStarted: QWord;
  LRequest: TNyxText;
  LObserved: TNyxText;
  LKind: TNyxText;
  LMarker: TNyxText;
  LSource: TNyxBrowserBox;
  LTarget: TNyxBrowserBox;
  LInputCount: Integer;

begin
  LHost := nil;
  try

    if ParamCount <> 4 then
    begin
      raise Exception.Create('Supply owned fixture URL, fresh capture directory, CSS width and height');
    end;
    LHost := TNyxBrowserPipe.Create(ParamStr(1), ParamStr(2),
      StrToInt(ParamStr(3)), StrToInt(ParamStr(4)));
    LStarted := GetTickCount64;
    LObserved := '';
    LInputCount := 0;
    repeat
      LMarker := LHost.Attribute('data-program-logic-controls');

      if (LMarker = 'failed') or (LHost.RuntimeError <> '') then
      begin
        LHost.Capture('failure');
        raise Exception.Create('Actual connector fixture: ' +
          LHost.Attribute('data-program-logic-controls-error'));
      end;
      LRequest := LHost.Attribute('data-wire-request');

      if (LRequest <> '') and (LRequest <> LObserved) then
      begin
        LKind := LHost.Attribute('data-wire-kind');

        if LKind = 'space' then
        begin
          LHost.Key(nbkSpace);
        end
        else if LKind = 'right' then
        begin
          LHost.Key(nbkRight);
        end
        else if LKind = 'right-twice' then
        begin
          LHost.Key(nbkRight);
          LHost.Key(nbkRight);
        end
        else if LKind = 'home' then
        begin
          LHost.Key(nbkHome);
        end
        else if LKind = 'edit-node-name' then
        begin
          LHost.ReplaceText('[data-wire-editor="true"]', 'Node writer');
        end
        else if LKind = 'edit-name' then
        begin
          LHost.ReplaceText('[data-wire-editor="true"]', 'Grace');
        end
        else if LKind = 'touch-wire' then
        begin
          LSource := LHost.Bounds('[data-wire-source="true"]');
          LTarget := LHost.Bounds('[data-wire-target="true"]');
          LHost.DragTouch('[data-wire-source="true"]',
            LTarget.Left + LTarget.Width / 2 - LSource.Left - LSource.Width / 2,
            LTarget.Top + LTarget.Height / 2 - LSource.Top - LSource.Height / 2);
        end
        else if LKind = 'touch-panel' then
        begin
          LHost.DragTouch('[data-wire-panel="true"]', 0, 32);
        end
        else
        begin
          raise Exception.Create('Unknown owned fixture input request');
        end;
        LObserved := LRequest;
        LHost.SetAttribute('data-wire-observed', LRequest);
        Inc(LInputCount);
        WriteLn('Host input ', LRequest, ' / ', LKind);
      end;

      if (LHost.Attribute('data-capture-checkpoint') = 'control-connectors') and
        (LHost.Attribute('data-capture-observed') <> 'control-connectors') then
      begin
        LHost.Capture('control-connectors');
        LHost.SetAttribute('data-capture-observed', 'control-connectors');
      end;

      if LMarker = 'passed' then
      begin
        Break;
      end;

      if GetTickCount64 - LStarted > 120000 then
      begin
        LHost.Capture('timeout');
        raise Exception.Create('Connector input deadline; last request ' + LObserved);
      end;
      Sleep(20);
    until False;
    WriteLn('PASS ', LHost.Attribute('data-program-logic-controls-checks'),
      ' actual browser connector checks / ', LInputCount, ' host input actions');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      ExitCode := 1;
    end;
  end;
  LHost.Free;
end.
