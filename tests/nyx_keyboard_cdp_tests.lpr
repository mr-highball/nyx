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
program nyx_keyboard_cdp_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, base64, nyx.text, nyx.types, nyx.data, nyx.test.browser.host;

type
  { Closed Pascal key choices map to CDP only at this host boundary. Input
    commands exercise browser default Tab/Space/Enter, unlike synthetic events.
    Semantic assertions observe the Pascal fixture's small published attributes. }
  TKeyboardDriver = class(TNyxBrowserHost)
  private
    procedure Press(AKey: TNyxKey; AShift: Boolean = False);
    procedure Wait(const AName, AValue: TNyxText);
    procedure ExpectFocus(const AID: TNyxText);
  public
    procedure Run;
  end;

procedure TKeyboardDriver.Press(AKey: TNyxKey; AShift: Boolean);
var
  LName: TNyxText;
  LCode: TNyxText;
  LVirtual: Integer;
  LModifiers: Integer;
  LPhase: TNyxText;
  LText: TNyxText;
  LIndex: Integer;
begin
  case AKey of
    nkTabKey:
      begin
        LName := 'Tab';
        LCode := 'Tab';
        LVirtual := 9;
      end;
    nkEnterKey:
      begin
        LName := 'Enter';
        LCode := 'Enter';
        LVirtual := 13;
      end;
    nkSpaceKey:
      begin
        LName := ' ';
        LCode := 'Space';
        LVirtual := 32;
      end;
    nkEscapeKey:
      begin
        LName := 'Escape';
        LCode := 'Escape';
        LVirtual := 27;
      end;
    nkDownKey:
      begin
        LName := 'ArrowDown';
        LCode := 'ArrowDown';
        LVirtual := 40;
      end;
    nkUpKey:
      begin
        LName := 'ArrowUp';
        LCode := 'ArrowUp';
        LVirtual := 38;
      end;
    nkDeleteKey:
      begin
        LName := 'Delete';
        LCode := 'Delete';
        LVirtual := 46;
      end;
    nkF2Key:
      begin
        LName := 'F2';
        LCode := 'F2';
        LVirtual := 113;
      end;
  else
    raise Exception.Create('This keyboard journey does not map the requested key');
  end;
  LModifiers := 0;

  if AShift then
  begin
    LModifiers := 8;
  end;
  for LIndex := 0 to 1 do
  begin
    LPhase := 'keyDown';

    if LIndex = 1 then
    begin
      LPhase := 'keyUp';
    end;
    { Enter's character phase activates a native HTML button. CDP supplies it
      through text on keyDown; omitting it only proves key notifications. }
    LText := '';

    if LIndex = 0 then
    begin
      case AKey of
        nkEnterKey: LText := #13;
        nkSpaceKey: LText := ' ';
      end;
    end;
    Command('Input.dispatchKeyEvent', NyxObject([
      NyxField('type', NyxData(LPhase)), NyxField('key', NyxData(LName)),
      NyxField('text', NyxData(LText)),
      NyxField('code', NyxData(LCode)), NyxField('windowsVirtualKeyCode', NyxData(LVirtual)),
      NyxField('nativeVirtualKeyCode', NyxData(LVirtual)),
      NyxField('modifiers', NyxData(LModifiers))]));
  end;
end;

procedure TKeyboardDriver.Wait(const AName, AValue: TNyxText);
var
  LStarted: QWord;
  LActual: TNyxText;
  LCapture: TNyxDataValue;
begin
  LStarted := GetTickCount64;
  repeat
    LActual := Attribute(AName);

    if Attribute('data-keyboard-result') = 'failed' then
    begin
      raise Exception.Create('Pascal keyboard fixture: ' + String(Attribute('data-keyboard-error')));
    end;

    if LActual = AValue then
    begin
      Exit;
    end;

    if GetTickCount64 - LStarted > 15000 then
    begin
      Save('failure.json', NyxObject([
        NyxField('expectedAttribute', NyxData(AName)), NyxField('expected', NyxData(AValue)),
        NyxField('observed', NyxData(LActual)),
        NyxField('focus', NyxData(Attribute('data-keyboard-focus'))),
        NyxField('items', NyxData(Attribute('data-keyboard-items')))]).ToJSON);
      LCapture := Command('Page.captureScreenshot', NyxObject([]));
      Save('failure.png', DecodeStringBase64(LCapture.Field('data').AsText));
      raise Exception.Create('Host keyboard expected ' + String(AName) + '=' +
        String(AValue) + ', observed ' + String(LActual));
    end;
    Pump;
    Sleep(10);
  until False;
end;

procedure TKeyboardDriver.ExpectFocus(const AID: TNyxText);
begin
  Wait('data-keyboard-focus', AID);
end;

procedure TKeyboardDriver.Run;
var
  LCapture: TNyxDataValue;
begin
  Wait('data-keyboard-result', 'ready');
  { Initial positioning is explicit. All subsequent focus and activation below
    use host key input, including leaving the composite at its Tab boundary. }
  Command('DOM.focus', NyxObject([NyxField('nodeId', NyxData(Node(
    '[data-runtime-id="' + Attribute('data-keyboard-query') + '"] input')))]));
  ExpectFocus(Attribute('data-keyboard-query'));
  Press(nkTabKey);
  ExpectFocus(Attribute('data-keyboard-search'));
  Press(nkEnterKey);
  Wait('data-keyboard-searched', '1');
  Press(nkTabKey);
  ExpectFocus(Attribute('data-keyboard-clear'));
  Press(nkSpaceKey);
  Wait('data-keyboard-cleared', '1');
  Press(nkTabKey);
  ExpectFocus(Attribute('data-keyboard-decrement'));
  Press(nkEnterKey);
  Wait('data-keyboard-decremented', '1');
  Press(nkTabKey);
  ExpectFocus(Attribute('data-keyboard-value'));
  Press(nkTabKey);
  ExpectFocus(Attribute('data-keyboard-increment'));
  Press(nkSpaceKey);
  Wait('data-keyboard-incremented', '1');
  Press(nkTabKey);
  ExpectFocus(Attribute('data-keyboard-action'));
  Press(nkEnterKey);
  Wait('data-keyboard-activated', '1');
  Press(nkSpaceKey);
  Wait('data-keyboard-activated', '2');
  Press(nkTabKey);
  ExpectFocus('keyboard-review-readonly');
  WriteLn('PASS host Tab, Enter and Space, ordered compound actions and disabled descendants');
  Press(nkTabKey);
  ExpectFocus('table.sketch');
  Press(nkF2Key);
  ExpectFocus('table.sketch.0');
  Press(nkTabKey);
  ExpectFocus('table.sketch.1');
  Press(nkTabKey);
  ExpectFocus('keyboard-after');
  Press(nkTabKey, True);
  ExpectFocus('table.sketch');
  Press(nkF2Key);
  ExpectFocus('table.sketch.0');
  Press(nkTabKey);
  ExpectFocus('table.sketch.1');
  Press(nkTabKey, True);
  ExpectFocus('table.sketch.0');
  Press(nkEscapeKey);
  ExpectFocus('table.sketch');
  Press(nkTabKey, True);
  ExpectFocus('keyboard-review-readonly');
  Press(nkTabKey);
  ExpectFocus('table.sketch');
  Press(nkDownKey);
  ExpectFocus('table.build');
  Press(nkDeleteKey);
  Wait('data-keyboard-removed', '1');
  ExpectFocus('table.share');
  Press(nkDeleteKey);
  Wait('data-keyboard-removed', '2');
  ExpectFocus('table.sketch');
  Press(nkDeleteKey);
  Wait('data-keyboard-items', '0');
  ExpectFocus('table.empty');
  Press(nkTabKey);
  ExpectFocus('keyboard-after');
  WriteLn('PASS host grid entry, cell traversal, Escape, row removal, empty focus and Tab exit');
  Save('result.json', NyxObject([
    NyxField('activated', NyxData(Attribute('data-keyboard-activated'))),
    NyxField('searched', NyxData(Attribute('data-keyboard-searched'))),
    NyxField('cleared', NyxData(Attribute('data-keyboard-cleared'))),
    NyxField('decremented', NyxData(Attribute('data-keyboard-decremented'))),
    NyxField('incremented', NyxData(Attribute('data-keyboard-incremented'))),
    NyxField('removed', NyxData(Attribute('data-keyboard-removed'))),
    NyxField('focus', NyxData(Attribute('data-keyboard-focus')))]).ToJSON);
  LCapture := Command('Page.captureScreenshot', NyxObject([]));
  Save('capture.png', DecodeStringBase64(LCapture.Field('data').AsText));
end;

var
  LDriver: TKeyboardDriver;
begin
  try

    if ParamCount <> 2 then
    begin
      raise Exception.Create('Supply loopback fixture URL and artifact directory');
    end;
    LDriver := TKeyboardDriver.Create(ParamStr(1), ParamStr(2));
    try
      LDriver.Run;
    finally
      LDriver.Free;
    end;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(StdErr);
      ExitCode := 1;
    end;
  end;
end.
