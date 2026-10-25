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
program nyx_button_theme_sync_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Interfaces, Classes, SysUtils, Types, Forms, Controls, Graphics, ExtCtrls,
  {$ifdef MSWINDOWS}Windows, LCLPlatformDef,{$endif}
  nyx.text, nyx.types, nyx.model, nyx.controls, nyx.render.lcl,
  nyx.theme, nyx.widgets.lcl, nyx.button.presentation, nyx.button.configuration,
  nyx.paint.buffer.lcl;

type
  { Observe actual TFont notifications while preserving the control's original
    LCL receiver. Font and receiver are borrowed; free this probe before the
    control/renderer. No callback, model or widget is retained after destruction. }
  TFontProbe = class
  private
    FFont: TFont;
    FPrevious: TNotifyEvent;
    FChanges: Integer;
    procedure Changed(ASender: TObject);
  public
    constructor Create(AFont: TFont);
    destructor Destroy; override;
    procedure Reset;
    property Changes: Integer read FChanges;
  end;
  { Each observer is independently owned by the native workload; the vector
    owns no fonts and is dismantled before its renderer releases any control. }
  TFontProbes = array of TFontProbe;

  {$ifdef MSWINDOWS}
  { An ordinary Nyx button observes actual paint cardinality and can recreate
    its HWND during a paint. The temporary application exception slot borrows
    this fixture-owned receiver; it is restored before the button is freed. }
  TPaintButton = class(TNyxLCLButton)
  public
    Paints: Integer;
    RecreateNext: Boolean;
    Failure: TNyxText;
    procedure Paint; override;
    procedure UseBuffering(AEnabled: Boolean);
    procedure Failed(ASender: TObject; AError: Exception);
  end;
  {$endif}

var
  GChecks: Integer;

constructor TFontProbe.Create(AFont: TFont);
begin
  inherited Create;
  FFont := AFont;
  FPrevious := FFont.OnChange;
  FFont.OnChange := Changed;
end;

destructor TFontProbe.Destroy;
begin
  FFont.OnChange := FPrevious;
  inherited Destroy;
end;

procedure TFontProbe.Changed(ASender: TObject);
begin
  Inc(FChanges);

  if Assigned(FPrevious) then
  begin
    FPrevious(ASender);
  end;
end;

procedure TFontProbe.Reset;
begin
  FChanges := 0;
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

{$ifdef MSWINDOWS}
procedure TPaintButton.Paint;
begin
  Inc(Paints);

  if RecreateNext then
  begin
    RecreateNext := False;
    RecreateWnd(Self);
    Exit;
  end;
  inherited Paint;
end;

procedure TPaintButton.UseBuffering(AEnabled: Boolean);
begin
  DoubleBuffered := AEnabled;
end;

procedure TPaintButton.Failed(ASender: TObject; AError: Exception);
begin

  if Failure = '' then
  begin
    Failure := TNyxText(AError.ClassName) + ': ' + TNyxText(AError.Message);
  end;
end;

{ Qualify the real window path separately from printing. Small dirty regions
  must stay small; supplied-DC printing must remain caller-owned. These checks
  make no foreground/display-compositor or physical-input claim. }
procedure BufferCases(AHost: TForm);
var
  LParent: TPanel;
  LButton: TPaintButton;
  LTheme: TNyxTheme;
  LBitmap: Graphics.TBitmap;
  LBefore: TNyxLCLPaintBufferStats;
  LAfter: TNyxLCLPaintBufferStats;
  LPrevious: TExceptionEvent;
  LDirty: TRect;
  LPaints: Integer;
  LIndex: Integer;
begin

  if GetBuildLCLWidgetType <> lpWin32 then
  begin
    Exit;
  end;
  LParent := TPanel.Create(AHost);
  LButton := TPaintButton.Create(LParent);
  LTheme := TNyxTheme.Create;
  LBitmap := Graphics.TBitmap.Create;
  LPrevious := Application.OnException;
  try
    Application.OnException := LButton.Failed;
    LParent.Parent := AHost;
    LParent.BevelOuter := bvNone;
    LParent.Color := $00392720;
    LParent.SetBounds(20, 20, 200, 90);
    LButton.Parent := LParent;
    LButton.SetBounds(10, 10, 140, 48);
    LButton.Caption := 'A native button';
    LButton.ApplyTheme(LTheme, NyxVariantName(nvPrimary));
    LButton.HandleNeeded;
    Application.ProcessMessages;
    Check(LButton.PaintBufferStats.Attached,
      'an actual button owns its native paint scope');
    LDirty := Types.Rect(1, 1, 9, 7);
    for LIndex := 1 to 3 do
    begin
      LBefore := LButton.PaintBufferStats;
      LPaints := LButton.Paints;
      Windows.InvalidateRect(LButton.Handle, @LDirty, False);
      Windows.UpdateWindow(LButton.Handle);
      LAfter := LButton.PaintBufferStats;
      Check(LButton.Failure = '', 'native button painting raises no hidden application error');
      Check((LButton.Paints = LPaints + 1) and
        (LAfter.BufferedPaints = LBefore.BufferedPaints + 1),
        'one dirty native button region paints exactly once');
      Check((LAfter.Width > 0) and (LAfter.Width <= 8) and
        (LAfter.Height > 0) and (LAfter.Height <= 6),
        'button buffering follows the dirty region rather than its whole extent');
      RecreateWnd(LButton);
      Check(LButton.PaintBufferStats.Attached,
        'ordinary handle recreation installs an independent live button scope');
      Application.ProcessMessages;
    end;
    LBefore := LButton.PaintBufferStats;
    LButton.UseBuffering(False);
    LPaints := LButton.Paints;
    Windows.InvalidateRect(LButton.Handle, @LDirty, False);
    Windows.UpdateWindow(LButton.Handle);
    Check((LButton.Paints = LPaints + 1) and
      (LButton.PaintBufferStats.BufferedPaints = LBefore.BufferedPaints),
      'caller-disabled buffering retains the ordinary native painter');
    LButton.UseBuffering(True);
    LBefore := LButton.PaintBufferStats;
    LBitmap.SetSize(LButton.Width, LButton.Height);
    LBitmap.Canvas.Brush.Color := clFuchsia;
    LBitmap.Canvas.FillRect(Types.Rect(0, 0, LBitmap.Width, LBitmap.Height));
    LButton.PaintTo(LBitmap.Canvas, 0, 0);
    Check(LButton.PaintBufferStats.BufferedPaints = LBefore.BufferedPaints,
      'supplied-DC button printing does not enter the display buffer');
    Check((ColorToRGB(LBitmap.Canvas.Pixels[0, 0]) = ColorToRGB(LParent.Color)) and
      (ColorToRGB(LBitmap.Canvas.Pixels[LBitmap.Width - 1, LBitmap.Height - 1]) =
        ColorToRGB(LParent.Color)),
      'rounded button corners retain their parent backdrop during supplied-DC painting');
    LButton.RecreateNext := True;
    Windows.InvalidateRect(LButton.Handle, @LDirty, False);
    Windows.UpdateWindow(LButton.Handle);
    Application.ProcessMessages;
    Check((LButton.Failure = '') and not LButton.RecreateNext and
      LButton.PaintBufferStats.Attached,
      'recreation from inside paint retires the old scope without a duplicate attachment');
  finally
    Application.OnException := LPrevious;
    LBitmap.Free;
    LTheme.Free;
    LParent.Free;
  end;
end;
{$endif}

procedure DirectThemeCases(AHost: TForm; out AUnchangedNotifications: Integer);
var
  LButton: TNyxLCLButton;
  LTheme: TNyxTheme;
  LProbe: TFontProbe;
  LIndex: Integer;
  LWarning: TNyxText;
  LColor: TColor;
  LRefused: Boolean;
begin
  LTheme := TNyxTheme.Create;
  LButton := TNyxLCLButton.Create(AHost);
  LProbe := nil;
  try
    LButton.Parent := AHost;
    LButton.ApplyPresentation(NyxButtonPresentation.FontWeight(nbwRegular), nil);
    LButton.ApplyTheme(LTheme, NyxVariantName(nvPrimary));
    LProbe := TFontProbe.Create(LButton.Font);
    for LIndex := 1 to 8 do
    begin
      LButton.ApplyTheme(LTheme, NyxVariantName(nvPrimary));
    end;
    AUnchangedNotifications := LProbe.Changes;
    Check((LButton.Font.Style = []) and (LButton.Font.Height = -LTheme.FontSize) and
      (LButton.Font.Color = NyxLCLColor(LTheme.AccentText)),
      'repeated regular theme retains its accepted font and foreground');
    LTheme.FontSize := 19;
    LTheme.AccentText := '#123456';
    LProbe.Reset;
    LButton.ApplyTheme(LTheme, NyxVariantName(nvPrimary));
    Check((LButton.Font.Height = -19) and (LButton.Font.Style = []) and
      (LButton.Font.Color = NyxLCLColor('#123456')) and (LProbe.Changes > 0),
      'a changed mutable theme still updates the actual native font');
    LButton.Font.Style := [fsBold];
    LProbe.Reset;
    LButton.ApplyTheme(LTheme, NyxVariantName(nvPrimary));
    Check((LButton.Font.Style = []) and (LProbe.Changes = 1),
      'regular theme repairs external font drift with one final style assignment');
    LButton.ApplyPresentation(NyxButtonPresentation.FontWeight(nbwInherit), nil);
    LButton.ApplyTheme(LTheme, NyxVariantName(nvPrimary));
    Check(LButton.Font.Style = [fsBold], 'inherited weight retains its native legacy appearance');
    LButton.ApplyPresentation(NyxButtonPresentation.FontWeight(nbwSemibold), nil);
    LButton.ApplyTheme(LTheme, NyxVariantName(nvPrimary));
    Check(LButton.Font.Style = [fsBold], 'semibold weight retains its native bold face');
    LWarning := LTheme.Warning;
    LColor := LButton.Font.Color;
    LTheme.Warning := 'invalid-color';
    LProbe.Reset;
    LRefused := False;
    try
      LButton.ApplyTheme(LTheme, NyxVariantName(nvPrimary));
    except
      on LException: ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    LTheme.Warning := LWarning;
    Check(LRefused and (LProbe.Changes = 0) and (LButton.Font.Color = LColor),
      'invalid unused palette entries still refuse before native font mutation');
  finally
    LProbe.Free;
    LButton.Free;
    LTheme.Free;
  end;
end;

procedure RendererCases(AHost: TForm; out AUnchangedNotifications: Integer);
const
  CButtons = 64;
  CRefreshes = 32;
var
  LDocument: TNyxDocument;
  LRenderer: TNyxLCLRenderer;
  LPage: INyxPage;
  LButton: INyxButton;
  LNative: TNyxLCLButton;
  LProbes: TFontProbes;
  LBounds: array of TRect;
  LIndex: Integer;
  LRefresh: Integer;
  LStarted: QWord;
  LElapsed: QWord;
begin
  LDocument := TNyxDocument.Create;
  LRenderer := nil;
  SetLength(LProbes, CButtons);
  SetLength(LBounds, CButtons);
  try
    LPage := NewNyxPage('button-refresh');
    LDocument.AddPage(LPage);
    for LIndex := 0 to CButtons - 1 do
    begin
      LButton := NewNyxButton('action-' + IntToStr(LIndex));
      LButton.Configure.Text('An English action ' + IntToStr(LIndex)).Done;
      NyxButtonConfigure(LButton.Configure).Presentation(
        NyxButtonPresentation.FontWeight(nbwRegular)).Done;
      LPage.Add(LButton);
    end;
    LButton := nil;
    LPage := nil;
    LRenderer := TNyxLCLRenderer.Create;
    LRenderer.Render(LDocument, LDocument.Pages[0], AHost);
    for LIndex := 0 to CButtons - 1 do
    begin
      LNative := TNyxLCLButton(LRenderer.ControlFor('action-' + IntToStr(LIndex)));
      LBounds[LIndex] := LNative.BoundsRect;
      LProbes[LIndex] := TFontProbe.Create(LNative.Font);
    end;
    LStarted := GetTickCount64;
    for LRefresh := 1 to CRefreshes do
    begin
      LRenderer.Sync;
    end;
    LElapsed := GetTickCount64 - LStarted;
    AUnchangedNotifications := 0;
    for LIndex := 0 to CButtons - 1 do
    begin
      Inc(AUnchangedNotifications, LProbes[LIndex].Changes);
      LNative := TNyxLCLButton(LRenderer.ControlFor('action-' + IntToStr(LIndex)));
      Check((LNative.Font.Style = []) and EqualRect(LBounds[LIndex], LNative.BoundsRect),
        'ordinary native Sync retains regular typography and exact button geometry');
    end;
    WriteLn('PROFILE native button refresh buttons=', CButtons, ' calls=', CRefreshes,
      ' elapsed-ms=', LElapsed, ' font-notifications=', AUnchangedNotifications);
  finally
    for LIndex := 0 to High(LProbes) do
    begin
      LProbes[LIndex].Free;
    end;
    LRenderer.Free;
    LDocument.Free;
  end;
end;

var
  LHost: TForm;
  LDirectNotifications: Integer;
  LRendererNotifications: Integer;
begin
  LHost := nil;
  try
    try
      Application.Initialize;
      LHost := TForm.CreateNew(nil);
      LHost.ClientWidth := 960;
      LHost.ClientHeight := 900;
      LHost.Show;
      DirectThemeCases(LHost, LDirectNotifications);
      RendererCases(LHost, LRendererNotifications);
      {$ifdef MSWINDOWS}
      BufferCases(LHost);
      {$endif}
      WriteLn('PROFILE direct unchanged font-notifications=', LDirectNotifications);
      Check(LDirectNotifications = 0, 'Unchanged regular theme must emit no font notifications');
      Check(LRendererNotifications = 0, 'Unchanged native Sync must emit no font notifications');
      WriteLn('PASS ', GChecks, ' native button theme synchronization checks');
    finally
      LHost.Free;
    end;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
