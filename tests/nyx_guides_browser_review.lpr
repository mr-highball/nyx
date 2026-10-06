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
program nyx_guides_browser_review;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, base64, nyx.text, nyx.data, nyx.test.browser.host;

type
  { Owns one headless input host. All document composition is semantic; CDP is
    used only for actual pointer capture/paint that document tools cannot prove. }
  TGuideReview = class(TNyxBrowserHost)
  public
    procedure Run(ACompact: Boolean);
  end;

procedure TGuideReview.Run(ACompact: Boolean);
var
  LStarted: QWord;
  LMarker, LInput: TNyxText;
  LPhase: Integer;
  LNode: Integer;
  LBox: TNyxDataValue;
  LX, LY: Double;

  procedure Mouse(const AKind: TNyxText; AX, AY: Double; AButtons: Integer);
  begin
    Command('Input.dispatchMouseEvent', NyxObject([
      NyxField('type', NyxData(AKind)), NyxField('x', NyxData(AX)),
      NyxField('y', NyxData(AY)), NyxField('button', NyxData('left')),
      NyxField('buttons', NyxData(AButtons)), NyxField('clickCount', NyxData(1))]));
  end;

begin

  if ACompact then
  begin
    Command('Emulation.setDeviceMetricsOverride', NyxObject([
      NyxField('width', NyxData(390)), NyxField('height', NyxData(900)),
      NyxField('deviceScaleFactor', NyxData(1)), NyxField('mobile', NyxData(False))]));
  end;
  LPhase := 0;
  LX := 0;
  LY := 0;
  LStarted := GetTickCount64;
  repeat
    Pump;
    LMarker := Attribute('data-nyx-guides');
    LInput := Attribute('data-nyx-guides-input');

    if LMarker = 'failed' then
    begin
      Save('failure.png', DecodeStringBase64(Command('Page.captureScreenshot',
        NyxObject([NyxField('format', NyxData('png'))])).Field('data').AsText));
      raise Exception.Create(Attribute('data-nyx-guides-error'));
    end;

    if (LPhase = 0) and (LInput = 'ready') then
    begin
      LNode := Node('[data-node="nyx-canvas-resize-both"]');
      Command('DOM.scrollIntoViewIfNeeded', NyxObject([NyxField('nodeId', NyxData(LNode))]));
      LBox := Command('DOM.getBoxModel', NyxObject([NyxField('nodeId', NyxData(LNode))]))
        .Field('model').Field('border');
      LX := (LBox.Item(0).AsNumber + LBox.Item(4).AsNumber) / 2;
      LY := (LBox.Item(1).AsNumber + LBox.Item(5).AsNumber) / 2;
      Mouse('mousePressed', LX, LY, 1);
      Mouse('mouseMoved', LX + 14, LY + 22, 1);
      LPhase := 1;
    end;

    if (LPhase = 1) and (LInput = 'release') then
    begin
      Save('guides.png', DecodeStringBase64(Command('Page.captureScreenshot',
        NyxObject([NyxField('format', NyxData('png'))])).Field('data').AsText));
      Mouse('mouseReleased', LX + 14, LY + 22, 0);
      LPhase := 2;
    end;

    if LMarker = 'passed' then
    begin
      Save('result.json', NyxObject([NyxField('checks', NyxData(StrToInt(
        Attribute('data-nyx-guides-checks')))), NyxField('pointerJourney', NyxData(LPhase = 2))]).ToJSON);
      Save('capture.png', DecodeStringBase64(Command('Page.captureScreenshot',
        NyxObject([NyxField('format', NyxData('png'))])).Field('data').AsText));
      WriteLn('PASS actual browser Studio guides / ', Attribute('data-nyx-guides-checks'), ' checks');
      Exit;
    end;

    if GetTickCount64 - LStarted > 45000 then
    begin
      Save('timeout.png', DecodeStringBase64(Command('Page.captureScreenshot',
        NyxObject([NyxField('format', NyxData('png'))])).Field('data').AsText));
      raise Exception.Create('Guide input fixture timed out in phase ' + IntToStr(LPhase));
    end;
    Sleep(10);
  until False;
end;

var
  LReview: TGuideReview;
begin
  LReview := nil;
  try

    if (ParamCount < 2) or (ParamCount > 3) then
    begin
      raise Exception.Create('Supply loopback MCP-observing fixture and capture directory');
    end;

    if (ParamCount = 3) and (ParamStr(3) <> 'compact') then
    begin
      raise Exception.Create('Unknown guide review presentation');
    end;
    LReview := TGuideReview.Create(ParamStr(1), ParamStr(2));
    LReview.Run(ParamCount = 3);
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
  LReview.Free;
end.
