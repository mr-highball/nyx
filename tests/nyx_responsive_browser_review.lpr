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
  { Closed review choices are typed internally; only the command-line boundary
    accepts their advertised names. Unknown names refuse before browser startup. }
  TResponsiveReviewKind = (rrControls, rrStudio, rrSourceEditor, rrContracts, rrResizeContracts);
  { This owner only observes bounded Pascal-fixture results and captures the
    actual rendered page. It never evaluates scripts, edits a design or uses
    accelerated clocks; ResizeObserver delivery occurs on ordinary browser frames. }
  TResponsiveReview = class(TNyxBrowserHost)
  public
    { The optional suffix selects another ordinary responsive consumer using
      the same bounded marker/capture protocol. It changes no editor behavior. }
    procedure Run(AKind: TResponsiveReviewKind);
  end;

procedure TResponsiveReview.Run(AKind: TResponsiveReviewKind);
const
  CNames: array[TResponsiveReviewKind] of TNyxText =
    ('controls', 'studio', 'source-editor', 'contracts', 'resize-contracts');
var
  LStarted: QWord;
  LMarker: TNyxText;
  LChecksName: TNyxText;
  LResultName: TNyxText;
  LErrorName: TNyxText;
  LSuffix: TNyxText;
  LBudget: QWord;
begin
  LBudget := 30000;

  if AKind = rrStudio then
  begin
    { The ordinary Studio path now includes twelve worker/history stages.
      Its final paired Undo can finish just after the previous thirty-second
      deadline. Allow the complete bounded journey on ordinary frames. }
    LBudget := 60000;
  end;
  LSuffix := CNames[AKind];
  LResultName := 'data-nyx-responsive-' + LSuffix;
  LErrorName := 'data-nyx-responsive-' + LSuffix + '-error';
  LChecksName := 'data-nyx-responsive-checks';

  if AKind <> rrControls then
  begin
    LChecksName := 'data-nyx-responsive-' + LSuffix + '-checks';
  end;

  if AKind = rrResizeContracts then
  begin
    LResultName := 'data-result';
    LChecksName := 'data-checks';
    LErrorName := 'data-error';
  end
  else if AKind = rrContracts then
  begin
    LResultName := 'data-nyx-responsive';
    LChecksName := 'data-nyx-responsive-checks';
  end
  else if AKind = rrControls then
  begin
    LErrorName := 'data-nyx-responsive-error';
  end
  else if AKind = rrSourceEditor then
  begin
    { Reuse the existing source workspace's marker protocol for the regression
      affected by responsive captions and retained chrome. No new fixture or
      script injection is needed to inspect its actual controls and modal. }
    LResultName := 'data-source-editor';
    LChecksName := 'data-source-editor-checks';
    LErrorName := 'data-source-editor-error';
  end;
  LStarted := GetTickCount64;
  repeat
    Pump;
    LMarker := Attribute(LResultName);

    if LMarker = 'failed' then
    begin
      Save('failure.png', DecodeStringBase64(Command('Page.captureScreenshot',
        NyxObject([NyxField('format', NyxData('png'))])).Field('data').AsText));

      raise Exception.Create(Attribute(LErrorName));
    end;

    if LMarker = 'passed' then
    begin
      Save('result.json', NyxObject([NyxField('checks',
        NyxData(StrToInt(Attribute(LChecksName))))]).ToJSON);
      Save('capture.png', DecodeStringBase64(Command('Page.captureScreenshot',
        NyxObject([NyxField('format', NyxData('png'))])).Field('data').AsText));
      WriteLn('PASS actual browser responsive ', LSuffix, ' / ',
        Attribute(LChecksName), ' checks');
      Exit;
    end;

    if GetTickCount64 - LStarted > LBudget then
    begin
      { Retain the failed gate before teardown. Fixture-owned stage/check
        markers provide bounded diagnostics without script injection or a
        design dump; the capture is only for locating a rendering failure. }
      Save('timeout.png', DecodeStringBase64(Command('Page.captureScreenshot',
        NyxObject([NyxField('format', NyxData('png'))])).Field('data').AsText));
      Save('timeout-attributes.json', Command('DOM.getAttributes',
        NyxObject([NyxField('nodeId', NyxData(Node('body')))])).ToJSON);
      raise Exception.Create('Responsive fixture did not finish on ordinary browser frames / stage ' +
        Attribute('data-nyx-responsive-studio-stage') + ' / checks ' + Attribute(LChecksName) +
        ' / comparison ' + Attribute('data-nyx-responsive-studio-comparison'));
    end;
    Sleep(10);
  until False;
end;

var
  LReview: TResponsiveReview;
  LKind: TResponsiveReviewKind;
begin
  LReview := nil;
  try

    if (ParamCount < 2) or (ParamCount > 3) then
    begin
      raise Exception.Create('Supply isolated loopback fixture and artifact directory');
    end;
    LKind := rrControls;

    if ParamCount = 3 then
    begin
      if ParamStr(3) = 'controls' then
      begin
        LKind := rrControls;
      end
      else if ParamStr(3) = 'studio' then
      begin
        LKind := rrStudio;
      end
      else if ParamStr(3) = 'source-editor' then
      begin
        LKind := rrSourceEditor;
      end
      else if ParamStr(3) = 'contracts' then
      begin
        LKind := rrContracts;
      end
      else if ParamStr(3) = 'resize-contracts' then
      begin
        LKind := rrResizeContracts;
      end
      else
      begin
        raise Exception.Create('Unknown responsive review kind');
      end;
    end;
    LReview := TResponsiveReview.Create(ParamStr(1), ParamStr(2));
    LReview.Run(LKind);
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
  LReview.Free;
end.
