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

program nyx_lcl_visual;

{$mode delphi}{$H+}
{$codepage utf8}

uses
  Interfaces,
  Forms,
  Controls,
  Types,
  Graphics,
  IntfGraphics,
  FPWritePNG,
  SysUtils,
  nyx.text,
  nyx.model,
  nyx.theme,
  nyx.widgets.lcl,
  nyx.render.lcl,
  nyx.sample;

procedure Capture(const ADirectory, AName: TNyxText; ADark: Boolean; AWidth: Integer;
  ACustomize: Boolean = False; ACustomTheme: Boolean = False);
var
  LForm: TForm;
  LRenderer: TNyxLCLRenderer;
  LTheme: TNyxTheme;
  LDocument: TNyxDocument;
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
  LButton: TNyxLCLButton;
  LSurface: TNyxLCLSurface;
  LInputFrame: TNyxLCLSurface;
  LCaptionControl: TControl;
  LPreferredWidth: Integer;
  LPreferredHeight: Integer;

  procedure CheckPixel(AControl: TControl; AX, AY: Integer; AExpected: TColor;
    const AToken: TNyxText);
  var
    LPoint: TPoint;
  begin
    { Inspect the actual captured hierarchy. Palette property assertions alone
      would miss a widgetset painting its own default face instead of Nyx's. }
    LPoint := LForm.ScreenToClient(AControl.ClientToScreen(Point(AX, AY)));

    if ColorToRGB(LBitmap.Canvas.Pixels[LPoint.X, LPoint.Y]) <> ColorToRGB(AExpected) then
    begin
      raise ENyxModel.Create('Native captured theme token differs: ' + AName + ' / ' + AToken +
        ' / actual ' + IntToHex(ColorToRGB(LBitmap.Canvas.Pixels[LPoint.X, LPoint.Y]), 6) +
        ' expected ' + IntToHex(ColorToRGB(AExpected), 6));
    end;
  end;
begin
  { Paint actual LCL controls into a bitmap using a window outside the desktop.
    Win32 does not paint native child widgets while their ancestor is hidden.
    These artifacts show the native projection and theme; they do not establish
    physical input, display scaling or assistive-technology acceptance. }
  LForm := TForm.CreateNew(nil);
  LTheme := TNyxTheme.Create(ADark);
  LRenderer := TNyxLCLRenderer.Create(LTheme);
  LDocument := CreateNyxSample;
  LBitmap := TBitmap.Create;
  LImage := TLazIntfImage.Create(0, 0);
  LWriter := TFPWriterPNG.Create;
  try
    { Mutating a borrowed palette before mounting is supported. Validate and
      snapshot it at the projection boundary, including caller-selected metrics. }

    if ACustomTheme then
    begin
      LTheme.Accent := '#123456';
      LTheme.AccentText := '#fedcba';
      LTheme.Border := '#8899aa';
      LTheme.Radius := 23;
      LTheme.ControlRadius := 19;
      LTheme.FontSize := 17;
    end;
    { Record one actual instance customization alongside the baseline light/dark
      samples. Both changes use the public model contract and leave the reusable
      definition untouched; this is visual evidence for composition, not a new
      renderer-only demonstration or a claim of complete native Studio. }

    if ACustomize then
    begin
      { Keep the companion screenshot's initial copy in English, matching the
        browser view. Dedicated control journeys still exercise Unicode edits. }
      LDocument.Find('welcome-instance').OverridePart('title')
        .SetProp('text', 'My activity');
      LDocument.Find('welcome-instance').OverridePart('.', 'append')
        .Add(TNyxNode.Create('button', 'custom-view-action')
          .SetProp('text', '+ Add to this view').SetProp('emit', 'add'));
    end;
    LForm.BorderStyle := bsNone;
    LForm.Position := poDesigned;
    LForm.Left := -30000;
    LForm.Top := -30000;
    LForm.ClientWidth := AWidth;
    LForm.ClientHeight := 900;
    LRenderer.Render(LDocument, LDocument.Pages[0], LForm);
    LForm.HandleNeeded;
    LForm.Show;
    { A newly shown form automatically focuses its first edit. Select a stable
      button as keyboard focus so the input's resting border can be checked too. }
    TWinControl(LRenderer.ControlFor('create-project')).SetFocus;
    Application.ProcessMessages;
    LBitmap.SetSize(LForm.ClientWidth, LForm.ClientHeight);
    LBitmap.Canvas.Brush.Color := clWhite;
    LBitmap.Canvas.FillRect(0, 0, LBitmap.Width, LBitmap.Height);
    LForm.PaintTo(LBitmap.Canvas.Handle, 0, 0);
    LButton := TNyxLCLButton(LRenderer.ControlFor('create-project'));
    LSurface := TNyxLCLSurface(LRenderer.ControlFor('welcome-instance/welcome-card'));
    LInputFrame := TNyxLCLSurface(LRenderer.InputFor('project-name').Parent);
    LCaptionControl := LRenderer.ControlFor('remember-preferences');
    LPreferredWidth := 0;
    LPreferredHeight := 0;
    LCaptionControl.GetPreferredSize(LPreferredWidth, LPreferredHeight, True, False);

    if LCaptionControl.Width < LPreferredWidth then
    begin
      raise ENyxModel.Create('Native captured option caption is clipped: ' + AName);
    end;
    CheckPixel(LButton, 12, LButton.Height div 2, NyxLCLColor(LTheme.Accent), 'Accent');
    CheckPixel(LSurface, 20, LSurface.Height div 2, NyxLCLColor(LTheme.Surface), 'Surface');
    CheckPixel(LSurface, 0, LSurface.Height div 2, NyxLCLColor(LTheme.Border), 'Border');
    CheckPixel(LSurface, 0, 0, NyxLCLColor(LTheme.Background), 'rounded backdrop');
    CheckPixel(LInputFrame, 6, LInputFrame.Height div 2,
      NyxLCLColor(LTheme.Surface), 'input surface');
    CheckPixel(LInputFrame, 0, LInputFrame.Height div 2,
      NyxLCLColor(LTheme.Border), 'input border');

    if (LButton.Font.Color <> NyxLCLColor(LTheme.AccentText)) or
      (LSurface.BorderColor <> NyxLCLColor(LTheme.Border)) or
      (LSurface.CornerRadius <> LTheme.Radius) or
      (LButton.CornerRadius <> LTheme.ControlRadius) or
      (LInputFrame.CornerRadius <> LTheme.ControlRadius) or
      (LButton.Font.Height <> -LTheme.FontSize) then
    begin
      raise ENyxModel.Create('Native controls discarded caller-owned theme fields');
    end;
    LImage.LoadFromBitmap(LBitmap.Handle, LBitmap.MaskHandle);
    LImage.SaveToFile(IncludeTrailingPathDelimiter(ADirectory) + AName + '.png', LWriter);
    TWinControl(LRenderer.InputFor('project-name')).SetFocus;
    Application.ProcessMessages;
    LForm.PaintTo(LBitmap.Canvas.Handle, 0, 0);
    CheckPixel(LInputFrame, 0, LInputFrame.Height div 2,
      NyxLCLColor(LTheme.Accent), 'focused input border');
    LButton.SetFocus;
    Application.ProcessMessages;
    LForm.PaintTo(LBitmap.Canvas.Handle, 0, 0);
    CheckPixel(LInputFrame, 0, LInputFrame.Height div 2,
      NyxLCLColor(LTheme.Border), 'blurred input border');
    WriteLn('Captured ', AName, ' / ', LBitmap.Width, 'x', LBitmap.Height,
      ' / actual surface, button and input theme pixels pass');
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
    LRenderer.Free;
    LDocument.Free;
    LTheme.Free;
    LForm.Free;
  end;
end;

var
  LDirectory: TNyxText;
begin
  try
    Application.Initialize;
    LDirectory := 'build';

    if ParamCount > 0 then
    begin
      LDirectory := ParamStr(1);
    end;
    ForceDirectories(LDirectory);
    Capture(LDirectory, 'native-light-desktop', False, 900);
    Capture(LDirectory, 'native-dark-desktop', True, 900);
    Capture(LDirectory, 'native-light-narrow', False, 390);
    Capture(LDirectory, 'native-light-instance-parts', False, 900, True);
    Capture(LDirectory, 'native-custom-theme', False, 900, False, True);
  except
    on LException: Exception do
    begin
      { Headless capture failures must reach the build log, never a modal LCL
        exception dialog that leaves an unattended verification process waiting. }
      WriteLn('FAIL native visual: ', LException.Message);
      Halt(1);
    end;
  end;
end.
