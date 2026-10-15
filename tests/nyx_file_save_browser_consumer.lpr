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
  LWidth: Integer;
  LDownload: TNyxBrowserDownloadResult;
  LStream: TFileStream;
  LBytes: RawByteString;
  LExpected: TNyxBytes;
  LIndex: Integer;

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

begin
  LHost := nil;
  try

    if ParamCount <> 4 then
    begin
      raise Exception.Create('Supply loopback URL, owned output directory, CSS width, contract|controls|activation|activation-contract');
    end;
    LWidth := StrToInt(ParamStr(3));
    LActivation := ParamStr(4) = 'activation';
    LActivationContract := ParamStr(4) = 'activation-contract';
    LContract := (ParamStr(4) = 'contract') or LActivationContract;

    if not LContract and not LActivation and (ParamStr(4) <> 'controls') then
    begin
      raise Exception.Create('Unknown fixture mode');
    end;
    LHost := TNyxBrowserPipe.Create(ParamStr(1), ParamStr(2), LWidth, 900);
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
      LHost.AllowOneDownload;
      LHost.ReplaceText('[data-node="save-options-name"] input', 'evening-notes.json');
      LHost.Capture('before-save');
      LHost.Click('[data-node="save-report"]');

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

      if (LHost.Attribute(LMarker) = 'passed') and
        (LContract or (LDownload.State = nbdsCompleted)) then
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

      if (LDownload.Count <> 1) or (LDownload.Name <> 'evening-notes.json') then
      begin
        raise Exception.Create('Unexpected download identity/count');
      end;
      LExpected := NyxEncodeUTF8(NewNyxJSONFormat(njlIndented, 2).Format(
        TNyxDataValue.ParseJSON(
          '{"title":"Evening notes","count":9007199254740993,"detail":"Hello 🌙\u0000world"}')));
      LStream := TFileStream.Create(LHost.DownloadDirectory + PathDelim + 'evening-notes.json', fmOpenRead);
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
        (LDownload.ReceivedBytes <> Length(LExpected)) then
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
    WriteLn('PASS browser ', ParamStr(4), ' CSS-', LWidth,
      ' / checks ', LHost.Attribute(LMarker),
      ' / independent exact downloaded bytes ', Length(LExpected));
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
