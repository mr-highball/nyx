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
program nyx_catalog_focus_cdp_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, base64, nyx.text, nyx.types, nyx.data, nyx.test.browser.host;

type
  { The browser owner sends actual host keys and reads bounded Pascal fixture
    attributes. It never injects source, mutates a design or automates Studio. }
  TCatalogDriver = class(TNyxBrowserHost)
  private
    procedure Wait(const AAttribute, AValue: TNyxText);
    procedure Press(AKey: TNyxKey);
    procedure TabPast(const APrevious, AProjection: TNyxText);
    procedure Activate(const ASelector: TNyxText);
    procedure Capture(const AName: String);
  public
    procedure Run(AIsPhone, APeerOnly: Boolean);
  end;

procedure TCatalogDriver.Capture(const AName: String);
begin
  Save(AName, DecodeStringBase64(Command('Page.captureScreenshot', NyxObject([
    NyxField('format', NyxData('png')), NyxField('captureBeyondViewport', NyxData(False))]))
    .Field('data').AsText));
end;

procedure TCatalogDriver.TabPast(const APrevious, AProjection: TNyxText);
var
  LKind: TNyxKind;
  LSegmented: Boolean;
  LSteps: Integer;
begin
  LSegmented := TryNyxKind(AProjection, LKind) and (LKind in [nkDate, nkTime]);
  LSteps := 0;
  repeat
    Press(nkTabKey);
    Inc(LSteps);
    { Native HTML date/time editors have shadow date-part Tab stops. They retain
      one DOM/control focus identity; qualify their normal traversal rather than
      declaring these standard segments missing Nyx controls. No other face may
      silently absorb extra stops. This is bounded host observation, not script. }
    Sleep(30);
    Pump;

    if not LSegmented or (Attribute('data-catalog-focus') <> APrevious) then
    begin
      Exit;
    end;

    if LSteps >= 8 then
    begin
      raise Exception.Create('Native date/time segments did not release Tab');
    end;
  until False;
end;

procedure TCatalogDriver.Wait(const AAttribute, AValue: TNyxText);
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    Pump;

    if Attribute('data-catalog-result') = 'failed' then
    begin
      Capture('failure.png');
      raise Exception.Create('Pascal catalog assertion: ' + Attribute('data-catalog-error'));
    end;

    if Attribute(AAttribute) = AValue then
    begin
      Exit;
    end;

    if GetTickCount64 - LStarted > 15000 then
    begin
      Capture('failure.png');
      Save('failure.json', NyxObject([
        NyxField('kind', NyxData(Attribute('data-catalog-kind'))),
        NyxField('peerPhase', NyxData(Attribute('data-catalog-radio-phase'))),
        NyxField('expectedAttribute', NyxData(AAttribute)),
        NyxField('expected', NyxData(AValue)),
        NyxField('actual', NyxData(Attribute(AAttribute))),
        NyxField('focus', NyxData(Attribute('data-catalog-focus')))]).ToJSON);
      raise Exception.Create('Catalog host expected ' + AAttribute + '=' + AValue +
        ', observed ' + Attribute(AAttribute));
    end;
    Sleep(10);
  until False;
end;

procedure TCatalogDriver.Press(AKey: TNyxKey);
var
  LKey: TNyxText;
  LVirtual: Integer;
  LPhase: TNyxText;
  LIndex: Integer;
begin
  case AKey of
    nkTabKey: begin LKey := 'Tab'; LVirtual := 9; end;
    nkF8Key: begin LKey := 'F8'; LVirtual := 119; end;
    nkEnterKey: begin LKey := 'Enter'; LVirtual := 13; end;
  else
    raise Exception.Create('Unmapped catalog qualification key');
  end;
  for LIndex := 0 to 1 do
  begin
    LPhase := 'keyDown';

    if LIndex = 1 then
    begin
      LPhase := 'keyUp';
    end;
    Command('Input.dispatchKeyEvent', NyxObject([
      NyxField('type', NyxData(LPhase)), NyxField('key', NyxData(LKey)),
      NyxField('code', NyxData(LKey)), NyxField('windowsVirtualKeyCode', NyxData(LVirtual)),
      NyxField('nativeVirtualKeyCode', NyxData(LVirtual))]));
  end;
end;

procedure TCatalogDriver.Activate(const ASelector: TNyxText);
begin
  Command('DOM.focus', NyxObject([NyxField('nodeId', NyxData(Node(ASelector)))]));
  { A DOM click is unnecessary: a real Enter character activates the harness
    button. Only this positioning and the outer boundaries use explicit focus. }
  Command('Input.dispatchKeyEvent', NyxObject([
    NyxField('type', NyxData('keyDown')), NyxField('key', NyxData('Enter')),
    NyxField('code', NyxData('Enter')), NyxField('text', NyxData(#13)),
    NyxField('windowsVirtualKeyCode', NyxData(13))]));
  Command('Input.dispatchKeyEvent', NyxObject([
    NyxField('type', NyxData('keyUp')), NyxField('key', NyxData('Enter')),
    NyxField('code', NyxData('Enter')), NyxField('windowsVirtualKeyCode', NyxData(13))]));
end;

procedure TCatalogDriver.Run(AIsPhone, APeerOnly: Boolean);
var
  LCases: Integer;
  LCase: Integer;
  LPolicy: Integer;
  LFace: Integer;
  LFaces: Integer;
  LKind: TNyxText;
  LPrevious: TNyxText;
  LProjection: TNyxText;
begin

  if AIsPhone then
  begin
    Command('Emulation.setDeviceMetricsOverride', NyxObject([
      NyxField('width', NyxData(390)), NyxField('height', NyxData(844)),
      NyxField('deviceScaleFactor', NyxData(1)), NyxField('mobile', NyxData(True))]));
  end;
  LCases := 0;

  if not APeerOnly then
  begin
    Wait('data-catalog-result', 'ready');
    LCases := StrToInt(Attribute('data-catalog-total'));
  end;
  for LCase := 0 to LCases - 1 do
  begin
    Wait('data-catalog-index', IntToStr(LCase));
    LKind := Attribute('data-catalog-kind');
    LFaces := StrToInt(Attribute('data-catalog-face-count'));
    for LPolicy := 0 to 3 do
    begin
      Wait('data-catalog-policy', IntToStr(LPolicy));
      Command('DOM.focus', NyxObject([NyxField('nodeId', NyxData(Node(
        '[data-runtime-id="' + Attribute('data-catalog-before') + '"]')))]));
      Wait('data-catalog-focus', Attribute('data-catalog-before'));
      LPrevious := Attribute('data-catalog-before');
      LProjection := '';

      if LPolicy <> 2 then
      begin
        for LFace := 0 to LFaces - 1 do
        begin
          TabPast(LPrevious, LProjection);
          Wait('data-catalog-focus', Attribute('data-catalog-face-' + IntToStr(LFace)));
          Press(nkF8Key);
          LPrevious := Attribute('data-catalog-face-' + IntToStr(LFace));
          LProjection := Attribute('data-catalog-projection-' + IntToStr(LFace));

          if (LPolicy = 0) and (LFace = 0) and
            ((LKind = 'code') or (LKind = 'list') or (LKind = 'tree')) then
          begin
            Capture(String(LKind) + '-focus.png');
          end;
        end;
      end;
      TabPast(LPrevious, LProjection);
      Wait('data-catalog-focus', Attribute('data-catalog-after'));

      if LPolicy < 3 then
      begin
        Activate('#catalog-policy');
      end;
    end;
    Activate('#catalog-next');
    WriteLn('PASS ', LKind, ' / ', LFaces, ' host Tab/F8 faces and all interaction policies');
  end;
  Wait('data-catalog-result', 'radio');
  { Browser-owned radio groups and the LCL peer adapter share one forward Tab
    entry. Inspect actual host navigation through the same compiled fixture. }
  for LPolicy := 0 to 8 do
  begin
    Wait('data-catalog-radio-phase', IntToStr(LPolicy));
    Command('DOM.focus', NyxObject([NyxField('nodeId', NyxData(Node(
      '[data-runtime-id="radio-before"]')))]));
    Wait('data-catalog-focus', 'radio-before');
    Press(nkTabKey);
    Wait('data-catalog-focus', Attribute('data-catalog-radio-entry'));

    if LPolicy <> 6 then
    begin

      if LPolicy = 8 then
      begin
        Press(nkTabKey);
        Wait('data-catalog-focus', 'radio-middle');
        Press(nkTabKey);
        Wait('data-catalog-focus', 'radio-last');
      end;
      Press(nkTabKey);
      Wait('data-catalog-focus', 'radio-after');
    end;

    if LPolicy < 8 then
    begin
      Activate('#catalog-policy');
    end;
  end;
  Activate('#catalog-next');
  WriteLn('PASS browser radio peer entry / unchecked, checked, disabled, re-enabled, read-only and hidden');
  Wait('data-catalog-result', 'passed');

  if APeerOnly then
  begin
    Save('result.json', NyxObject([NyxField('radioPeerPhases', NyxData(8)),
      NyxField('unnamedHTMLInputs', NyxData(3))]).ToJSON);
    WriteLn('PASS focused radio peer journey');
    Exit;
  end;
  Save('result.json', NyxObject([
    NyxField('kinds', NyxData(LCases)),
    NyxField('faces', NyxData(StrToInt(Attribute('data-catalog-face-total')))),
    NyxField('checks', NyxData(StrToInt(Attribute('data-catalog-checks'))))]).ToJSON);
  Capture('completed.png');
  WriteLn('PASS complete catalog / ', LCases, ' kinds / ', Attribute('data-catalog-face-total'),
    ' physical keyboard faces / ', Attribute('data-catalog-checks'), ' Pascal checks');
end;

var
  LDriver: TCatalogDriver;
begin
  LDriver := nil;
  try

    if (ParamCount <> 2) and (ParamCount <> 3) then
    begin
      raise Exception.Create('Supply loopback catalog page, artifact directory and optional phone');
    end;
    LDriver := TCatalogDriver.Create(ParamStr(1), ParamStr(2));
    LDriver.Run(ParamStr(3) = 'phone', ParamStr(3) = 'peers');
  except
    on LException: Exception do
    begin
      WriteLn(StdErr, 'FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
  LDriver.Free;
end.
