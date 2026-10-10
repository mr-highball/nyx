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

program nyx_browser_ready_capture;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.resources.editor, nyx.test.browser.pipe,
  nyx.test.resource.workbench.files;

type
  { Assertion fixtures finish with "passed". Rendered application/preview
    adapters finish with "true". Both must be observed at their real clocks;
    accepting a loaded page alone would hide initialization failures. }
  TNyxReadyCompletion = (nrcFixturePassed, nrcApplicationReady);

const
  CDefaultFixtureSeconds = 180;
  CMaximumFixtureSeconds = 600;
  CFixtureTimeoutPrefix = '--fixture-timeout=';
  CResourceFilesPrefix = '--resource-files=';

var
  LHost: TNyxBrowserPipe;
  LMarker: TNyxText;
  LCheckpoint: TNyxText;
  LLastCheckpoint: TNyxText;
  LWidth: Integer;
  LHeight: Integer;
  LStarted: QWord;
  LCaptured: Integer;
  LCompletion: TNyxReadyCompletion;
  LExpected: TNyxText;
  LTimeoutSeconds: Integer;
  LTimeoutMilliseconds: QWord;
  LResourceDirectory: TNyxText;
  LResourceFile: TNyxText;
  LURL: TNyxText;
  LResourcePicks: Integer;

begin
  LHost := nil;
  try

    if (ParamCount = 2) and (ParamStr(1) = '--prepare-resource-files') then
    begin
      PrepareNyxWorkbenchFiles(TNyxText(ExpandFileName(ParamStr(2))));
      WriteLn('PASS / exact owned workbench files prepared');
      Exit;
    end;

    if (ParamCount < 3) or (ParamCount > 7) then
    begin
      raise Exception.Create('Supply URL, owned output directory, marker, optional CSS ' +
        'width/height, --application-ready or --fixture-timeout=<seconds>, ' +
        'and optional --resource-files=<admitted-directory>');
    end;
    LWidth := 1100;
    LHeight := 900;
    LLastCheckpoint := '';
    LCaptured := 0;
    LCompletion := nrcFixturePassed;
    LTimeoutSeconds := CDefaultFixtureSeconds;
    LResourceDirectory := '';
    LResourcePicks := 0;
    LURL := TNyxText(ParamStr(1));

    if ParamCount >= 4 then
    begin
      LWidth := StrToInt(ParamStr(4));
    end;

    if ParamCount >= 5 then
    begin
      LHeight := StrToInt(ParamStr(5));
    end;

    if ParamCount >= 6 then
    begin

      if ParamStr(6) = '--application-ready' then
      begin
        LCompletion := nrcApplicationReady;
      end;

      if Copy(ParamStr(6), 1, Length(CFixtureTimeoutPrefix)) = CFixtureTimeoutPrefix then
      begin
        { A complete multi-import/history journey can exceed a short fixture's
          overall budget. This explicit boundary changes only its wall-clock
          observation deadline, never browser clocks or fixture step assertions. }

        if not TryStrToInt(Copy(ParamStr(6), Length(CFixtureTimeoutPrefix) + 1, MaxInt),
          LTimeoutSeconds) or (LTimeoutSeconds < 1) or
          (LTimeoutSeconds > CMaximumFixtureSeconds) then
        begin
          raise Exception.Create('Fixture timeout requires 1..600 whole seconds');
        end;
      end
      else if LCompletion <> nrcApplicationReady then
      begin
        raise Exception.Create('Completion option is --application-ready or --fixture-timeout=<seconds>');
      end;
    end;

    if ParamCount = 7 then
    begin

      if Copy(ParamStr(7), 1, Length(CResourceFilesPrefix)) <> CResourceFilesPrefix then
      begin
        raise Exception.Create('File delivery option is --resource-files=<admitted-directory>');
      end;
      LResourceDirectory := TNyxText(ExpandFileName(Copy(ParamStr(7),
        Length(CResourceFilesPrefix) + 1, MaxInt)));

      if (ParamStr(3) <> 'data-test-result') or (LCompletion <> nrcFixturePassed) then
      begin
        raise Exception.Create('Complete resource journey requires its data-test-result marker');
      end;
      AdmitNyxWorkbenchFiles(LResourceDirectory);

      if Pos('?', LURL) = 0 then
      begin
        LURL := LURL + TNyxText('?');
      end
      else
      begin
        LURL := LURL + TNyxText('&');
      end;
      LURL := LURL + TNyxText('resource-files=trusted');
    end;
    LExpected := 'passed';

    if LCompletion = nrcApplicationReady then
    begin
      LExpected := 'true';
    end;
    LHost := TNyxBrowserPipe.Create(LURL, ParamStr(2), LWidth, LHeight);
    LTimeoutMilliseconds := QWord(LTimeoutSeconds) * 1000;
    LStarted := GetTickCount64;
    repeat
      LMarker := LHost.Attribute(TNyxText(ParamStr(3)));

      if LHost.RuntimeError <> '' then
      begin
        raise Exception.Create('Actual browser exception; see runtime-error.json');
      end;

      if LMarker = 'failed' then
      begin
        LHost.Capture('failure');
        raise Exception.Create('Pascal fixture refused / ' + LHost.Attribute('data-event-error'));
      end;
      if LResourceDirectory <> '' then
      begin
        LResourceFile := LHost.Attribute('data-resource-file');

        if (LResourceFile <> '') and
          (LHost.Attribute('data-resource-file-observed') <> LResourceFile) then
        begin
          LHost.PickResourceFile('[data-node="' +
            NyxResourceEditorActionID('studio-resource-editor', reaImport) + '"]',
            NyxWorkbenchFilePath(LResourceDirectory, NyxWorkbenchFileRole(LResourceFile)));
          LHost.SetAttribute('data-resource-file-observed', LResourceFile);
          Inc(LResourcePicks);
          WriteLn('Observed actual chooser delivery / ', LResourceFile);
        end;
      end;
      LCheckpoint := LHost.Attribute('data-capture-checkpoint');

      if (LCheckpoint <> '') and (LCheckpoint <> LLastCheckpoint) then
      begin
        { A fixture pauses its own journey at a meaningful actual-control state.
          Acknowledge only after PNG/DOM evidence is saved; never invoke editor
          commands, alter source or accelerate worker clocks from this driver.
          The explicit resource option above qualifies only physical import input. }
        LHost.Capture(LCheckpoint);
        LHost.SetAttribute('data-capture-observed', LCheckpoint);
        LLastCheckpoint := LCheckpoint;
        Inc(LCaptured);
        WriteLn('Observed actual fixture checkpoint / ', LCheckpoint,
          ' / elapsed milliseconds ', GetTickCount64 - LStarted);
      end;

      if LMarker = LExpected then
      begin
        Break;
      end;

      if GetTickCount64 - LStarted > LTimeoutMilliseconds then
      begin
        LHost.Capture('timeout');
        raise Exception.Create('Real-clock fixture did not reach a terminal result within ' +
          IntToStr(LTimeoutSeconds) + ' seconds');
      end;
      Sleep(50);
    until False;

    if (LResourceDirectory <> '') and (LResourcePicks <> 5) then
    begin
      { JSON is imported at the start, as a hosted proposal, and again as its
        explicit embedded fallback. Text and binary supply the other two files. }
      raise Exception.Create('Complete workbench must deliver all five actual imports');
    end;
    LHost.Capture('capture');
    FreeAndNil(LHost);
    WriteLn('PASS real-clock browser / ', LWidth, ' x ', LHeight, ' / checkpoints ', LCaptured,
      ' / elapsed milliseconds ', GetTickCount64 - LStarted,
      ' / budget seconds ', LTimeoutSeconds, ' / actual resource picks ', LResourcePicks);
  except
    on LException: Exception do
    begin
      LHost.Free;
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
