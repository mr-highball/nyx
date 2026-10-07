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
unit nyx.test.capture.lcl;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  Forms,
  nyx.text;

type
  { Print is a diagnostic redraw and may differ from displayed clipping. Display
    reads the actual visible owned Win32 window, including its nonclient frame.
    These are qualification modes, not application/document presentation policy. }
  TNyxNativeCaptureMode = (ncmPrint, ncmDisplay);

{ Borrow an already shown form on its UI thread and save one owned PNG. Display
  requires the foreground window to belong to that exact form; this helper never
  captures an entire desktop or an arbitrary foreground application. It releases
  its DC/bitmap/image/writer on failure. Other widgetsets are not qualified for
  Display; unsupported targets refuse rather than silently substitute printing. }
procedure SaveNyxNativeCapture(AForm: TCustomForm; const AFileName: TNyxText;
  AMode: TNyxNativeCaptureMode);

implementation

uses
  SysUtils,
  Graphics,
  IntfGraphics,
  FPWritePNG
  {$IFDEF WINDOWS}
  , Windows
  {$ENDIF}
  ;

procedure SaveNyxNativeCapture(AForm: TCustomForm; const AFileName: TNyxText;
  AMode: TNyxNativeCaptureMode);
var
  LBitmap: Graphics.TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
  {$IFDEF WINDOWS}
  LBounds: Windows.TRect;
  LDisplay: HDC;
  {$ENDIF}
begin

  if (AForm = nil) or not AForm.HandleAllocated or not AForm.Visible then
  begin
    raise Exception.Create('Native capture requires an already shown owned form');
  end;
  {$IFNDEF WINDOWS}

  if AMode = ncmDisplay then
  begin
    raise Exception.Create('Displayed window capture is not qualified on this widgetset');
  end;
  {$ENDIF}
  LBitmap := Graphics.TBitmap.Create;
  LImage := nil;
  LWriter := nil;
  try
    {$IFDEF WINDOWS}

    if not Windows.GetWindowRect(AForm.Handle, LBounds) then
    begin
      raise Exception.Create('Unable to measure the native capture window');
    end;
    LBitmap.SetSize(LBounds.Right - LBounds.Left, LBounds.Bottom - LBounds.Top);

    if AMode = ncmDisplay then
    begin

      if Windows.GetAncestor(Windows.GetForegroundWindow, GA_ROOT) <> AForm.Handle then
      begin
        raise Exception.Create('Displayed capture requires its owned form in foreground');
      end;
      { Acquire only this form's display DC. Unlike PaintTo/WM_PRINT, copying its
        displayed pixels does not ask parked zero-area children to redraw. }
      LDisplay := Windows.GetWindowDC(AForm.Handle);

      if LDisplay = 0 then
      begin
        raise Exception.Create('Unable to acquire the owned window display');
      end;
      try

        if not Windows.BitBlt(LBitmap.Canvas.Handle, 0, 0, LBitmap.Width, LBitmap.Height,
          LDisplay, 0, 0, SRCCOPY) then
        begin
          raise Exception.Create('Unable to copy the owned window display');
        end;
      finally
        Windows.ReleaseDC(AForm.Handle, LDisplay);
      end;
    end
    else
    begin
      AForm.PaintTo(LBitmap.Canvas, 0, 0);
    end;
    {$ELSE}
    LBitmap.SetSize(AForm.ClientWidth, AForm.ClientHeight);
    AForm.PaintTo(LBitmap.Canvas, 0, 0);
    {$ENDIF}
    LImage := LBitmap.CreateIntfImage;
    LWriter := TFPWriterPNG.Create;
    LImage.SaveToFile(AFileName, LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
end;

end.
