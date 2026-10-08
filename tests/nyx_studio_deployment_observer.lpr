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


program nyx_studio_deployment_observer;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, nyx.text, nyx.data, nyx.studio.projects,
  nyx.test.browser.pipe;

{ Observe the actual preserved primary in an ordinary browser. Trusted input
  joins its existing pair and opens presentation panels only; this fixture never
  authors, compiles, accepts source or modifies a protected design. The external
  deployment checker and guard verify exact durable pairs before and afterward. }

var
  GBrowser: TNyxBrowserPipe;

procedure Pump(ADuration: Integer);
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    GBrowser.Attribute('data-nyx-studio-ready');

    if GBrowser.RuntimeError <> '' then
    begin
      GBrowser.CaptureRuntimeError;
      raise Exception.Create('Installed Studio runtime failed; inspect private receipts');
    end;
    Sleep(25);
  until GetTickCount64 - LStarted >= QWord(ADuration);
end;

procedure WaitFace(const ASelector: TNyxText);
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat

    if GBrowser.Exists(ASelector) then
    begin
      Exit;
    end;

    if GetTickCount64 - LStarted > 15000 then
    begin
      raise Exception.Create('Installed observing Studio face did not become ready');
    end;
    Pump(50);
  until False;
end;

var
  LFile: TFileStream;
  LText: TNyxText;
  LExpected: TNyxDataValue;
  LPair: TNyxProjectPair;
  LActual: TNyxText;
  LStarted: QWord;
begin
  GBrowser := nil;
  try

    if ParamCount <> 3 then
    begin
      raise Exception.Create('Supply loopback origin, exact protected pairs and owned capture directory');
    end;
    LFile := TFileStream.Create(ParamStr(2), fmOpenRead or fmShareDenyNone);
    try

      if (LFile.Size < 1) or (LFile.Size > 4 * 1024 * 1024) then
      begin
        raise Exception.Create('Observing baseline exceeds its private byte bound');
      end;
      SetLength(LText, LFile.Size);
      LFile.ReadBuffer(LText[1], Length(LText));
    finally
      LFile.Free;
    end;
    LExpected := TNyxDataValue.ParseJSON(LText).Item(0);
    LPair := DecodeNyxProject(LExpected.Field('project').AsText);
    GBrowser := TNyxBrowserPipe.Create(ParamStr(1) + '/', ParamStr(3), 1280, 960);
    WaitFace('[data-node="action-code"]');
    Pump(1200);

    if GBrowser.Exists('[data-node="action-agent-accept"]') then
    begin
      GBrowser.Click('[data-node="action-agent-accept"]');
      Pump(1200);
    end;
    GBrowser.Click('[data-node="action-code"]');
    WaitFace('[data-node="action-expand-source"]');
    GBrowser.Click('[data-node="action-expand-source"]');
    LStarted := GetTickCount64;
    repeat

      if GBrowser.TryFieldValue('[data-node="studio-code"]', LActual) and
        (LActual = LPair.Source) then
      begin
        Break;
      end;

      if GetTickCount64 - LStarted > 15000 then
      begin
        GBrowser.Capture('installed-source-refusal');
        raise Exception.Create('Installed ordinary source does not match the exact retained companion');
      end;
      Pump(50);
    until False;
    GBrowser.Capture('installed-source-desktop');
    GBrowser.Click('[data-node="action-expand-source"]');
    GBrowser.Click('[data-node="action-code"]');
    GBrowser.Resize(390, 844);
    Pump(800);
    GBrowser.Capture('installed-primary-narrow');
    GBrowser.Click('[data-node="action-panel-inspector"]');
    Pump(400);
    GBrowser.Capture('installed-inspector-narrow');
    GBrowser.Click('[data-node="action-panel-design"]');
    Pump(400);
    GBrowser.Resize(1280, 960);
    Pump(600);
    GBrowser.Capture('installed-primary-desktop');
    WriteLn('Installed ordinary observer: exact source, modal, narrow panels and desktop return passed');
  finally
    GBrowser.Free;
  end;
end.
