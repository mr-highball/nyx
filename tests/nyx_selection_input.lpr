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

program nyx_selection_input;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.test.browser.pipe;

var
  GHost: TNyxBrowserPipe;
  GChecks: Integer;
  GStarted: QWord;
  GThree: TNyxText;
  GFour: TNyxText;
  GFirstTwo: TNyxText;
  GFirstFour: TNyxText;
  GSecondTwo: TNyxText;
  GSecondThree: TNyxText;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function Target(const AAttribute: TNyxText): TNyxText;
var
  LIdentity: TNyxText;
begin
  LIdentity := GHost.Attribute(AAttribute);

  if LIdentity = '' then
  begin
    raise Exception.Create('Selection review did not publish its exact runtime identity');
  end;
  Result := '[data-runtime-id="' + LIdentity + '"]';
end;

begin
  GHost := nil;
  try

    if ParamCount <> 3 then
    begin
      raise Exception.Create('Supply exact #review URL, NEW capture directory and CSS width');
    end;
    { This owner sends real Chromium pointer and keyboard input. It creates no
      design, evaluates no scripts and never changes a user's observing project. }
    GHost := TNyxBrowserPipe.Create(ParamStr(1), ParamStr(2), StrToInt(ParamStr(3)), 900);
    GStarted := GetTickCount64;
    repeat

      if (GHost.RuntimeError <> '') or (GetTickCount64 - GStarted > 30000) then
      begin
        raise Exception.Create('Selection review is not ready; inspect owned transport receipts');
      end;
      Sleep(5);
    until GHost.Attribute('data-selection-review') = 'ready';
    GThree := Target('data-selection-step-three');
    GFour := Target('data-selection-step-four');
    GFirstTwo := Target('data-selection-first-two');
    GFirstFour := Target('data-selection-first-four');
    GSecondTwo := Target('data-selection-second-two');
    GSecondThree := Target('data-selection-second-three');
    Check(GHost.Exists(GThree + '[aria-pressed="true"][data-variant=""]'),
      'review begins at its programmatically selected step');
    GHost.Click(GFour);
    Check(GHost.Exists(GFour + '[aria-pressed="true"][data-variant=""]:focus') and
      GHost.Exists(GThree + '[aria-pressed="false"][data-variant="primary"]'),
      'trusted pointer selects one step and updates both actual styled faces');
    GHost.Tab(True);
    Check(GHost.Exists(GThree + ':focus'), 'reverse Tab reaches the preceding retained step');
    GHost.Key(nbkSpace);
    Check(GHost.Exists(GThree + '[aria-pressed="true"][data-variant=""]:focus') and
      GHost.Exists(GFour + '[aria-pressed="false"][data-variant="primary"]'),
      'trusted Space uses ordinary button activation and copied selection policy');
    GHost.Key(nbkEnter);
    Check(GHost.Exists(GThree + '[aria-pressed="true"]:focus'),
      'trusted Enter on the accepted step retains correct selection and focus');
    GHost.Click(GFirstTwo);
    Check(GHost.Exists(GFirstFour + '[aria-pressed="true"]') and
      GHost.Exists(GFirstTwo + '[aria-pressed="false"][data-variant="secondary"]'),
      'readonly authored instance refuses trusted value changes without replacing its variant');
    GHost.Click(GSecondTwo);
    Check(GHost.Exists(GSecondTwo + '[aria-pressed="true"][data-variant="primary"]') and
      GHost.Exists(GSecondThree + '[aria-pressed="false"][data-variant=""]') and
      GHost.Exists(GFirstFour + '[aria-pressed="true"]'),
      'trusted selection changes only the independent instance with its local policy');
    GHost.Capture('selection-styles');
    Check(GHost.RuntimeError = '', 'trusted policy journey produces no browser exception');
    WriteLn('PASS ', GChecks, ' trusted selection checks / width ', ParamStr(3));
  except
    on LException: Exception do
    begin

      if GHost <> nil then
      begin
        GHost.Capture('failure');
      end;
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
  GHost.Free;
end.
