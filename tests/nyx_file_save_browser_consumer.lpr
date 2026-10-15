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

program nyx_file_save_browser_consumer;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, nyx.text, nyx.bytes, nyx.data, nyx.logic,
  nyx.test.browser.pipe;

var
  LHost: TNyxBrowserPipe;
  LStarted: QWord;
  LMarker: TNyxText;
  LContract: Boolean;
  LActivation: Boolean;
  LActivationContract: Boolean;
  LPicker: Boolean;
  LApplication: Boolean;
  LReady: Boolean;
  LWidth: Integer;
  LDownload: TNyxBrowserDownloadResult;
  LStream: TFileStream;
  LBytes: RawByteString;
  LExpected: TNyxBytes;
  LIndex: Integer;

procedure WaitPicker(AAction: TNyxNativeSavePickerAction;
  const ALeaf: TNyxText = 'evening-notes.json');
begin
  repeat

    if (LHost.Attribute('data-file-save-controls') = 'failed') or
      (LHost.RuntimeError <> '') then
    begin
      raise Exception.Create('Actual picker input failed: ' + LHost.Attribute('data-file-save-error'));
    end;

    if GetTickCount64 - LStarted > 30000 then
    begin
      LHost.Capture('picker-timeout');
      raise Exception.Create('The owned browser did not offer a usable native save picker');
    end;

    if LHost.TryNativeSavePicker(ALeaf, AAction) then
    begin
      Exit;
    end;
    Sleep(20);
  until False;
end;

procedure WaitAttribute(const AName, AValue: TNyxText);
begin
  repeat

    if (LHost.Attribute('data-file-save-controls') = 'failed') or
      (LHost.RuntimeError <> '') then
    begin
      raise Exception.Create('Actual activation input failed: ' + LHost.Attribute('data-file-save-error'));
    end;

    if GetTickCount64 - LStarted > 30000 then
    begin
      LHost.Capture('activation-timeout');
      raise Exception.Create('Timed out waiting for ' + AName + ' = ' + AValue);
    end;
    Sleep(20);
  until LHost.Attribute(AName) = AValue;
end;

procedure WaitPickerClosed;
begin
  while LHost.TryNativeSavePicker('evening-notes.json', nspaObserve) do
  begin

    if GetTickCount64 - LStarted > 30000 then
    begin
      raise Exception.Create('The owned native picker did not dismiss');
    end;
    Sleep(20);
  end;
end;

begin
  LHost := nil;
  try

    if ParamCount <> 4 then
    begin
      raise Exception.Create('Supply loopback URL, owned output directory, CSS width, contract|controls|activation|activation-contract|picker|compiled-picker');
    end;
    LWidth := StrToInt(ParamStr(3));
    LActivation := ParamStr(4) = 'activation';
    LActivationContract := ParamStr(4) = 'activation-contract';
    LContract := (ParamStr(4) = 'contract') or LActivationContract;
    LApplication := ParamStr(4) = 'compiled-picker';
    LPicker := (ParamStr(4) = 'picker') or LApplication;

    if not LContract and not LActivation and not LPicker and (ParamStr(4) <> 'controls') then
    begin
      raise Exception.Create('Unknown fixture mode');
    end;
    if LPicker then
    begin
      LHost := TNyxBrowserPipe.Create(ParamStr(1), ParamStr(2), LWidth, 900,
        nil, nboLoopback, 15000, nbdNativeDialogs);
    end
    else
    begin
      LHost := TNyxBrowserPipe.Create(ParamStr(1), ParamStr(2), LWidth, 900);
    end;
    LStarted := GetTickCount64;

    if LContract then
    begin
      LMarker := 'data-file-save-contract';

      if LActivationContract then
      begin
        LMarker := 'data-save-activation';
      end;
    end
    else
    begin
      LMarker := 'data-file-save-controls';

      if LApplication then
      begin
        LMarker := 'data-nyx-ready';
      end;
      LHost.AllowOneDownload;

      if not LApplication then
      begin
        LHost.ReplaceText('[data-node="save-options-name"] input', 'evening-notes.json');
      end;
      LHost.Capture('before-save');
      LHost.Click('[data-node="save-report"]');

      if LApplication then
      begin
        WaitPicker(nspaSelect);
      end
      else if LPicker then
      begin
        WaitPicker(nspaSelect);
        WaitAttribute('data-save-picker-stage', 'saved');
        LHost.Capture('picker-written');
        LHost.Click('[data-node="save-report"]');
        WaitPicker(nspaCancel);
        WaitAttribute('data-save-picker-stage', 'cancelled');
        LHost.Click('[data-node="save-report"]');
        WaitPicker(nspaObserve);
        LHost.SetAttribute('data-picker-checkpoint', 'retire-open-picker');
        WaitAttribute('data-save-picker-stage', 'retired');
        { The platform can finish an already-open picker after scope retirement.
          Select a new private target to exercise that successful late promise;
          the cancelled Nyx request must never open/write its stream. }
        WaitPicker(nspaSelect, 'retired-report.json');
        WaitPickerClosed;
        LHost.SetAttribute('data-picker-checkpoint', 'picker-closed');
      end;

      if LActivation then
      begin
        LHost.ReplaceText('[data-node="report-source"] textarea',
          'A later edit must not replace the prepared report.');
        repeat

          if (LHost.Attribute(LMarker) = 'failed') or (LHost.RuntimeError <> '') then
          begin
            raise Exception.Create('Activation preparation failed: ' + LHost.Attribute('data-file-save-error'));
          end;

          if GetTickCount64 - LStarted > 25000 then
          begin
            LHost.Capture('activation-timeout');
            raise Exception.Create('Prepared save did not offer a fresh action');
          end;
          Sleep(20);
        until LHost.Attribute('data-save-activation-prompt') = 'open';

        if LHost.Attribute('data-save-activation-expired') <> 'true' then
        begin
          raise Exception.Create('The original activation was not observed expired');
        end;
        LHost.Capture('prepared-save-prompt');
        LHost.Click(LHost.Attribute('data-save-confirm-selector'));
        WaitAttribute('data-save-activation-stage', 'saved');
        LHost.ReplaceText('[data-node="report-source"] textarea',
          '{"title":"Evening notes","count":9007199254740993,"detail":"Hello 🌙\u0000world"}');
        LHost.Click('[data-node="save-report"]');
        WaitAttribute('data-save-activation-prompt-count', '2');
        LHost.Capture('cancel-save-prompt');
        LHost.Key(nbkEscape);
        WaitAttribute('data-save-activation-stage', 'cancelled');
        LHost.Click('[data-node="save-report"]');
        WaitAttribute('data-save-activation-stage', 'retiring');
        LHost.Capture('retiring-save-prompt');
        LHost.SetAttribute('data-capture-observed', 'retiring-save-prompt');
      end;
    end;
    repeat

      if LHost.Attribute(LMarker) = 'failed' then
      begin
        LHost.Capture('failed');

        if LActivationContract then
        begin
          raise Exception.Create(LHost.Attribute('data-save-activation-error'));
        end;
        raise Exception.Create(LHost.Attribute('data-file-save-error'));
      end;

      if LHost.RuntimeError <> '' then
      begin
        raise Exception.Create('Actual browser raised a runtime exception');
      end;
      LDownload := LHost.DownloadResult;
      LReady := LHost.Attribute(LMarker) = 'passed';

      if LApplication then
      begin
        LReady := (LHost.Attribute(LMarker) = 'true') and
          (Pos('evening-notes.json', LHost.ElementHTML('[data-node="saved-name"]')) > 0);
      end;

      if LReady and
        (LContract or LPicker or (LDownload.State = nbdsCompleted)) then
      begin
        Break;
      end;

      if GetTickCount64 - LStarted > 30000 then
      begin
        LHost.Capture('timeout');
        raise Exception.Create('Owned save browser journey exceeded its deadline');
      end;
      Sleep(20);
    until False;

    if not LContract then
    begin

      if LPicker and FileExists(LHost.SelectedFilePath('retired-report.json')) then
      begin
        { Selecting a path may itself create an empty platform file. That is
          separate from opening a writable stream or writing a cancelled payload. }
        LStream := TFileStream.Create(LHost.SelectedFilePath('retired-report.json'), fmOpenRead);
        try

          if LStream.Size <> 0 then
          begin
            raise Exception.Create('A retired picker wrote payload bytes after successful late selection');
          end;
        finally
          LStream.Free;
        end;
      end;

      if LPicker and (LDownload.Count <> 0) then
      begin
        raise Exception.Create('Picker selection/cancellation unexpectedly dispatched a download');
      end;

      if not LPicker and ((LDownload.Count <> 1) or (LDownload.Name <> 'evening-notes.json')) then
      begin
        raise Exception.Create('Unexpected download identity/count');
      end;
      LExpected := NyxEncodeUTF8(NewNyxJSONFormat(njlIndented, 2).Format(
        TNyxDataValue.ParseJSON(
          '{"title":"Evening notes","count":9007199254740993,"detail":"Hello 🌙\u0000world"}')));
      if LPicker then
      begin
        LStream := TFileStream.Create(LHost.SelectedFilePath('evening-notes.json'), fmOpenRead);
      end
      else
      begin
        LStream := TFileStream.Create(LHost.DownloadDirectory + PathDelim + 'evening-notes.json', fmOpenRead);
      end;
      try
        SetLength(LBytes, LStream.Size);

        if LStream.Size > 0 then
        begin
          LStream.ReadBuffer(LBytes[1], Length(LBytes));
        end;
      finally
        LStream.Free;
      end;

      if (Length(LBytes) <> Length(LExpected)) or
        (not LPicker and (LDownload.ReceivedBytes <> Length(LExpected))) then
      begin
        raise Exception.Create('Actual downloaded byte length differs');
      end;
      for LIndex := 0 to High(LExpected) do
      begin

        if Ord(LBytes[LIndex + 1]) <> LExpected[LIndex] then
        begin
          raise Exception.Create('Actual downloaded bytes differ');
        end;
      end;
    end;
    LHost.Capture('passed');
    LMarker := 'data-file-save-checks';

    if LActivationContract then
    begin
      LMarker := 'data-save-activation-checks';
    end;
    LMarker := LHost.Attribute(LMarker);

    if LApplication then
    begin
      LMarker := 'ordinary compiled application';
    end;
    WriteLn('PASS browser ', ParamStr(4), ' CSS-', LWidth,
      ' / checks ', LMarker,
      ' / independent exact saved bytes ', Length(LExpected));
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
