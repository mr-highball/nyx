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
  Interfaces, Classes, SysUtils, Types, Forms, Controls, Graphics,
  nyx.text, nyx.types, nyx.model, nyx.controls, nyx.render.lcl,
  nyx.theme, nyx.widgets.lcl, nyx.button.presentation, nyx.button.configuration;

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
