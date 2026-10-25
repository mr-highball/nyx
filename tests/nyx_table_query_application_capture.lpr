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
program nyx_table_query_application_capture;

{$mode delphi}{$H+}{$codepage utf8}

{ Compile beside the complete Studio journey's own exported Pascal module.
  Record display pixels or an explicitly requested diagnostic redraw. A redraw
  does not qualify displayed clipping; no failed display request is silently
  substituted. The compiled-application regression owns input/event assertions.
  Only this process's shown form can be captured; no desktop or foreign window
  is read. Other widgetsets require a separately qualified display capture. }
uses Interfaces, Forms, Classes, SysUtils, nyx.text, nyx.model, nyx.codec,
  nyx.application.lcl, nyx.fixture.table.query.studio, nyx.test.capture.lcl;

var
  LDocument: TNyxDocument;
  LApplication: TNyxLCLApplication;
  LWidth: Integer;
  LBefore: TNyxText;
  LCapture: TNyxText;
  LMode: TNyxNativeCaptureMode;
begin
  LDocument := nil;
  LApplication := nil;
  try
    try

      if ((ParamCount <> 2) and (ParamCount <> 3)) or
        not TryStrToInt(ParamStr(1), LWidth) or
        ((LWidth <> 1280) and (LWidth <> 390)) then
      begin
        raise Exception.Create('Expected width 1280/390, a fresh PNG path, and optional display/print');
      end;
      LMode := ncmDisplay;

      if ParamCount = 3 then
      begin
        { Command-line names terminate here at the typed capture boundary. }
        if ParamStr(3) = 'print' then
        begin
          LMode := ncmPrint;
        end
        else
        begin

          if ParamStr(3) <> 'display' then
          begin
            raise Exception.Create('Capture mode must be display or print');
          end;
        end;
      end;
      LCapture := ParamStr(2);

      if FileExists(LCapture) then
      begin
        raise Exception.Create('Application capture must use a new output file');
      end;
      Application.Initialize;
      LDocument := BuildNyxDocument;
      LBefore := TNyxCodec.Encode(LDocument);
      LApplication := TNyxLCLApplication.Create;
      { The application borrows the authored document and owns its runtime
        projection/stores. Retire it before releasing that borrowed document. }
      LApplication.Mount(LDocument);
      LApplication.Window.ClientWidth := LWidth;
      LApplication.Window.ClientHeight := 900;
      LApplication.Window.Show;
      LApplication.Window.BringToFront;
      LApplication.Window.SetFocus;
      Application.ProcessMessages;
      LApplication.Window.Update;

      if LApplication.Window.ClientWidth <> LWidth then
      begin
        raise Exception.Create('The displayed application has an unexpected client width');
      end;
      SaveNyxNativeCapture(LApplication.Window, LCapture, LMode);

      if TNyxCodec.Encode(LDocument) <> LBefore then
      begin
        raise Exception.Create('Mounting/capturing changed the authored document');
      end;
    finally
      LApplication.Free;
      LDocument.Free;
      { Drain retirement queued by the owned native form, after its callbacks
        unwind. The capture owns no application or model callback receiver. }
      Application.ProcessMessages;
      CheckSynchronize;
      Application.ProcessMessages;
    end;

    if LMode = ncmDisplay then
    begin
      WriteLn('PASS displayed compiled application capture at ', LWidth);
    end
    else
    begin
      WriteLn('PASS diagnostic redraw of compiled application at ', LWidth,
        '; displayed clipping is not qualified');
    end;
  except
    on LError: Exception do
    begin
      WriteLn('FAIL ', LError.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
