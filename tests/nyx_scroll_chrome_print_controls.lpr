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
program nyx_scroll_chrome_print_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Interfaces, Forms, SysUtils, Math, Classes, Types, Controls, StdCtrls, ExtCtrls,
  Grids, ComCtrls, Graphics, Windows, IntfGraphics, FPWritePNG,
  nyx.text, nyx.theme, nyx.scroll.chrome, nyx.scroll.chrome.lcl, nyx.gestures.lcl,
  nyx.test.capture.lcl;

type
  { The same print and native-input checks consume each supported control
    family. Names are used only for diagnostic files, never behavior dispatch. }
  TPrintFamily = (npfMemo, npfGrid, npfScrollArea, npfTree);

  { Borrow protected color access without constructing or retaining a control.
    The host remains the sole native component owner throughout the fixture. }
  TPrintControlAccess = class(TWinControl);

  { Readonly test authority owns its physical frame, not a component or renderer.
    Revocation is synchronous and the native subclass retains this managed port. }
  TPrintLifetime = class(TInterfacedObject, INyxLCLScrollChromeLifetime)
  public
    Active: Boolean;
    Physical: INyxNativeGestureFrame;
    function Current: Boolean;
    function InputReady: Boolean;
    function InputEpoch: Integer;
    function Frame: INyxNativeGestureFrame;
  end;

  { The application borrows this error receiver during the finite regression.
    A callback exception fails the process instead of opening a modal dialog. }
  TPrintFailures = class
  public
    MessageText: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
  end;

  { A real grid HWND is retired from inside its client painter. Destruction is
    counted independently to prove the physical frame outlives that callback. }
  TPrintTrackedGrid = class(TStringGrid)
  public
    destructor Destroy; override;
  end;

  { The grid borrows this method receiver. Flags/Grid are borrowed until the
    first callout; Chrome is a managed lease. The fixture keeps the receiver and
    flags alive until queued physical retirement has completed. }
  TPrintRetirement = class
  public
    Flags: TPrintLifetime;
    Chrome: INyxLCLScrollChrome;
    Grid: TStringGrid;
    Called: Boolean;
    EnteredFrame: Boolean;
    procedure DrawCell(ASender: TObject; ACol: Integer; ARow: Integer;
      ARect: Types.TRect; AState: TGridDrawState);
  end;

const
  CNames: array[TPrintFamily] of TNyxText = ('memo', 'grid', 'scroll-area', 'tree');
  CScales: array[0..2] of Double = (1, 0.5, 1.5);
  COffsetX = 11;
  COffsetY = 13;
  CSentinel: TColor = $00B975E1;
  CClient: TColor = $00E5F1DA;
  CThumb: TColor = $00216ED0;
  CTrack: TColor = $004F3118;
  CArrow: TColor = $00CDE2F1;

var
  GChecks: Integer;
  GDirectory: TNyxText;
  GFailures: TPrintFailures;
  GRetiredGrids: Integer;
  GDisplayCaptures: Integer;

destructor TPrintTrackedGrid.Destroy;
begin
  Inc(GRetiredGrids);
  inherited Destroy;
end;

procedure TPrintRetirement.DrawCell(ASender: TObject; ACol: Integer; ARow: Integer;
  ARect: Types.TRect; AState: TGridDrawState);
begin

  if Called then
  begin
    Exit;
  end;
  Called := True;
  EnteredFrame := Flags.Physical.Dispatching;
  Flags.Active := False;
  Chrome.Retire;
  Flags.Physical.Retire(Grid);
  Grid := nil;
end;

function TPrintLifetime.Current: Boolean;
begin
  Result := Active;
end;

function TPrintLifetime.InputReady: Boolean;
begin
  Result := Active;
end;

function TPrintLifetime.InputEpoch: Integer;
begin
  Result := 0;
end;

function TPrintLifetime.Frame: INyxNativeGestureFrame;
begin
  Result := Physical;
end;

procedure TPrintFailures.Failed(ASender: TObject; AException: Exception);
begin

  if MessageText = '' then
  begin
    MessageText := AException.Message;
  end;
end;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure SavePNG(ABitmap: Graphics.TBitmap; const AName: TNyxText);
var
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
begin

  if GDirectory = '' then
  begin
    Exit;
  end;
  LImage := ABitmap.CreateIntfImage;
  LWriter := TFPWriterPNG.Create;
  try
    LImage.SaveToFile(GDirectory + AName + '.png', LWriter);
  finally
    LWriter.Free;
    LImage.Free;
  end;
end;

procedure RunFamily(AFamily: TPrintFamily);
var
  LHost: TForm;
  LControl: TWinControl;
  LFlags: TPrintLifetime;
  LLifetime: INyxLCLScrollChromeLifetime;
  LChrome: INyxLCLScrollChrome;
  LTheme: TNyxTheme;
  LBitmap: Graphics.TBitmap;
  LWindow: Windows.TRect;
  LClientRect: Windows.TRect;
  LOrigin: Windows.TPoint;
  LBeforeOrigin: Windows.TPoint;
  LAfterOrigin: Windows.TPoint;
  LBeforeClip: Windows.TRect;
  LAfterClip: Windows.TRect;
  LInfo: TScrollInfo;
  LRows: TNyxStrings;
  LScale: Double;
  LScaleIndex: Integer;
  LIndex: Integer;
  LSavedDC: Integer;
  LDC: HDC;
  LChromeX: Integer;
  LChromeY: Integer;
  LClientX: Integer;
  LClientY: Integer;
  LWidth: Integer;
  LHeight: Integer;
  LBeforePosition: Integer;
  LScreen: Windows.TPoint;
  LPanel: TPanel;
  LLabel: TLabel;
  LArrowLength: Integer;
  LArrowSize: Integer;
  LHorizontal: TScrollInfo;

  procedure CheckArrow(AX: Integer; AY: Integer; ASize: Integer;
    AAxis: TNyxScrollAxis; AAction: TNyxScrollChromePart);
  var
    LInk: Windows.TPoint;
    LQuiet: Windows.TPoint;
  begin
    LInk := Types.Point(AX, AY);
    LQuiet := LInk;

    if AAxis = nsaVertical then
    begin

      if AAction = nscpDecrement then
      begin
        Dec(LInk.Y);
        Inc(LQuiet.Y, ASize div 2 + 1);
      end
      else
      begin
        Inc(LInk.Y);
        Dec(LQuiet.Y, ASize div 2 + 1);
      end;
    end
    else
    begin

      if AAction = nscpDecrement then
      begin
        Dec(LInk.X);
        Inc(LQuiet.X, ASize div 2 + 1);
      end
      else
      begin
        Inc(LInk.X);
        Dec(LQuiet.X, ASize div 2 + 1);
      end;
    end;
    Check(LBitmap.Canvas.Pixels[COffsetX + LInk.X, COffsetY + LInk.Y] = CArrow,
      'the printed arrow points toward its semantic scroll action');
    Check(LBitmap.Canvas.Pixels[COffsetX + LQuiet.X, COffsetY + LQuiet.Y] = CTrack,
      'the printed arrow does not point in the opposite direction');
  end;

  procedure Clear;
  begin
    LBitmap.Canvas.Brush.Color := CSentinel;
    LBitmap.Canvas.FillRect(Types.Rect(0, 0, LBitmap.Width, LBitmap.Height));
  end;

  procedure Print(AFlags: LPARAM; APartial: Boolean = False);
  begin
    LDC := LBitmap.Canvas.Handle;
    LSavedDC := SaveDC(LDC);
    try
      SetViewportOrgEx(LDC, COffsetX, COffsetY, nil);
      IntersectClipRect(LDC, 0, 0, LWidth, LHeight);

      if APartial then
      begin
        IntersectClipRect(LDC, 30, 30, 80, 80);
      end;
      GetViewportOrgEx(LDC, LBeforeOrigin);
      GetClipBox(LDC, LBeforeClip);
      SendMessage(LControl.Handle, WM_PRINT, WPARAM(LDC), AFlags);
      GetViewportOrgEx(LDC, LAfterOrigin);
      GetClipBox(LDC, LAfterClip);
      Check((LBeforeOrigin.X = LAfterOrigin.X) and
        (LBeforeOrigin.Y = LAfterOrigin.Y), 'printing preserves the caller DC origin');
      Check(EqualRect(LBeforeClip, LAfterClip), 'printing preserves the caller DC clip');
    finally
      RestoreDC(LDC, LSavedDC);
    end;
    Check(LBitmap.Canvas.Pixels[0, 0] = CSentinel, 'printing stays inside the caller clip');
  end;

  function Coordinates(AX, AY: Integer): LPARAM;
  begin
    Result := LPARAM(LongWord(Word(AX)) or (LongWord(Word(AY)) shl 16));
  end;
begin
  LHost := TForm.CreateNew(nil);
  LTheme := TNyxTheme.Create;
  LFlags := TPrintLifetime.Create;
  LLifetime := LFlags;
  LFlags.Active := True;
  LFlags.Physical := NewNyxNativeGestureFrame;
  LBitmap := Graphics.TBitmap.Create;
  LControl := nil;
  try
    LTheme.Background := '#18314F';
    LTheme.Muted := '#D06E21';
    LTheme.Border := '#31566E';
    LTheme.Text := '#F1E2CD';
    LHost.ClientWidth := 400;
    LHost.ClientHeight := 300;
    case AFamily of
      npfMemo:
      begin
        LControl := TMemo.Create(LHost);
        TMemo(LControl).ScrollBars := ssBoth;
        TMemo(LControl).WordWrap := False;
        LRows := TNyxStrings.Create;
        try
          { A long first line requires horizontal scrolling; later empty lines
            provide a real glyph-free client probe beside the vertical bar. }
          LRows.Add(TNyxText('Source 🌙 / ') + TNyxText(StringOfChar('W', 80)));
          for LIndex := 2 to 80 do
          begin
            LRows.Add('');
          end;
          TMemo(LControl).Text := LRows.Text;
        finally
          LRows.Free;
        end;
      end;
      npfGrid:
      begin
        LControl := TStringGrid.Create(LHost);
        TStringGrid(LControl).ColCount := 10;
        TStringGrid(LControl).RowCount := 300;
        TStringGrid(LControl).Cells[1, 1] := 'Source';
      end;
      npfScrollArea:
      begin
        LControl := TScrollBox.Create(LHost);
        TScrollBox(LControl).HorzScrollBar.Range := 1800;
        TScrollBox(LControl).VertScrollBar.Range := 2400;
        LPanel := TPanel.Create(LHost);
        LPanel.Parent := LControl;
        LPanel.ParentBackground := False;
        LPanel.BevelOuter := bvNone;
        LPanel.Color := CClient;
        LPanel.SetBounds(0, 0, 1800, 2400);
        LLabel := TLabel.Create(LHost);
        LLabel.Parent := LPanel;
        LLabel.Caption := TNyxText('Embedded content 🌙');
        LLabel.SetBounds(12, 12, 180, 24);
      end;
      npfTree:
      begin
        LControl := TTreeView.Create(LHost);
        for LIndex := 1 to 100 do
        begin
          TTreeView(LControl).Items.Add(nil, 'Source ' + IntToStr(LIndex));
        end;
      end;
    end;
    LControl.Parent := LHost;
    LControl.SetBounds(12, 18, 320, 220);
    TPrintControlAccess(LControl).Color := CClient;
    LHost.Show;
    Application.ProcessMessages;
    LChrome := AttachNyxLCLScrollChrome(LControl, LLifetime);
    Check(LChrome <> nil, 'the actual supported family admits scroll chrome');
    for LScaleIndex := Low(CScales) to High(CScales) do
    begin
      LScale := CScales[LScaleIndex];
      LChrome.Project(LScale, LTheme);
      Application.ProcessMessages;
      GetWindowRect(LControl.Handle, LWindow);
      GetClientRect(LControl.Handle, LClientRect);
      LWidth := LWindow.Right - LWindow.Left;
      LHeight := LWindow.Bottom - LWindow.Top;
      LOrigin.X := 0;
      LOrigin.Y := 0;
      Windows.ClientToScreen(LControl.Handle, LOrigin);
      Dec(LOrigin.X, LWindow.Left);
      Dec(LOrigin.Y, LWindow.Top);
      LInfo := Default(TScrollInfo);
      LInfo.cbSize := SizeOf(LInfo);
      LInfo.fMask := SIF_ALL;
      Check(GetScrollInfo(LControl.Handle, SB_VERT, LInfo) and (LInfo.nPos = 0) and
        (LInfo.nMax > Integer(LInfo.nPage)), 'the actual family begins with a scrollable native range');
      LChromeX := (LClientRect.Right + LWidth) div 2;
      LArrowLength := Trunc(17 * LScale + 0.5);
      LArrowSize := Max(1, (LWidth - LClientRect.Right - LOrigin.X * 2) div 4);
      LChromeY := LOrigin.Y + LArrowLength + 2;
      LClientX := LOrigin.X + LClientRect.Right - 2;
      LClientY := LOrigin.Y + LClientRect.Bottom div 2;
      LBitmap.SetSize(LWidth + COffsetX * 2, LHeight + COffsetY * 2);

      Clear;
      Print(PRF_NONCLIENT);
      SavePNG(LBitmap, CNames[AFamily] + '-' + TNyxText(IntToStr(LScaleIndex)) + '-nonclient');
      Check(LBitmap.Canvas.Pixels[COffsetX + LChromeX, COffsetY + LChromeY] = CThumb,
        'nonclient printing uses the copied Nyx thumb color at its scaled allocation');
      Check(LBitmap.Canvas.Pixels[COffsetX + LClientX, COffsetY + LClientY] = CSentinel,
        'nonclient-only printing leaves the enlarged real client untouched');
      CheckArrow(LChromeX, LOrigin.Y + LArrowLength div 2, LArrowSize,
        nsaVertical, nscpDecrement);
      CheckArrow(LChromeX, LOrigin.Y + LClientRect.Bottom - LArrowLength div 2,
        LArrowSize, nsaVertical, nscpIncrement);
      LHorizontal := Default(TScrollInfo);
      LHorizontal.cbSize := SizeOf(LHorizontal);
      LHorizontal.fMask := SIF_ALL;

      if GetScrollInfo(LControl.Handle, SB_HORZ, LHorizontal) and
        (NyxScrollRange(LHorizontal.nMin, LHorizontal.nMax, LHorizontal.nPos,
          LHorizontal.nPage).LastPosition > LHorizontal.nMin) then
      begin
        LArrowSize := Max(1, (LHeight - LClientRect.Bottom - LOrigin.Y * 2) div 4);
        CheckArrow(LOrigin.X + LArrowLength div 2,
          (LClientRect.Bottom + LHeight) div 2, LArrowSize, nsaHorizontal, nscpDecrement);
        CheckArrow(LOrigin.X + LClientRect.Right - LArrowLength div 2,
          (LClientRect.Bottom + LHeight) div 2, LArrowSize, nsaHorizontal, nscpIncrement);
      end;

      Clear;
      Print(PRF_CLIENT or PRF_NONCLIENT or PRF_ERASEBKGND or PRF_CHILDREN);
      SavePNG(LBitmap, CNames[AFamily] + '-' + TNyxText(IntToStr(LScaleIndex)) + '-combined');
      Check(LBitmap.Canvas.Pixels[COffsetX + LChromeX, COffsetY + LChromeY] = CThumb,
        'combined printing preserves the same scaled Nyx chrome');

      if (LScaleIndex = 1) and (GDirectory <> '') and
        (Windows.GetAncestor(Windows.GetForegroundWindow, GA_ROOT) = LHost.Handle) then
      begin
        { The helper rechecks foreground ownership before copying only this
          shown form's window DC. A PrintTo image is never relabelled Display. }
        LHost.Update;
        LControl.Update;
        SaveNyxNativeCapture(LHost, GDirectory + CNames[AFamily] + '-display.png', ncmDisplay);
        Inc(GDisplayCaptures);
      end;

      if AFamily = npfMemo then
      begin
        Check(LBitmap.Canvas.Pixels[COffsetX + LClientX, COffsetY + LClientY] = CClient,
          'the stock scrollbar does not paint over the expanded memo client');
      end;

      Clear;
      Print(PRF_CLIENT or PRF_ERASEBKGND);
      Check(LBitmap.Canvas.Pixels[COffsetX + LChromeX, COffsetY + LChromeY] = CSentinel,
        'client-only printing does not add nonclient chrome');

      Clear;
      Print(PRF_CLIENT or PRF_NONCLIENT or PRF_ERASEBKGND or PRF_CHILDREN, True);
      Check(LBitmap.Canvas.Pixels[COffsetX + 50, COffsetY + 50] <> CSentinel,
        'a partial client print paints inside the admitted clip');
      Check((LBitmap.Canvas.Pixels[COffsetX + LChromeX, COffsetY + LChromeY] = CSentinel) and
        (LBitmap.Canvas.Pixels[COffsetX + LClientX, COffsetY + LClientY] = CSentinel),
        'client, child and chrome printing respect a restrictive caller clip');

      LControl.Visible := False;
      Clear;
      Print(PRF_CHECKVISIBLE or PRF_CLIENT or PRF_NONCLIENT);
      Check((LBitmap.Canvas.Pixels[COffsetX + LChromeX, COffsetY + LChromeY] = CSentinel) and
        (LBitmap.Canvas.Pixels[COffsetX + LClientX, COffsetY + LClientY] = CSentinel),
        'visibility-checked printing leaves a hidden control untouched');
      Clear;
      Print(PRF_CLIENT or PRF_NONCLIENT or PRF_ERASEBKGND or PRF_CHILDREN);
      Check(LBitmap.Canvas.Pixels[COffsetX + LChromeX, COffsetY + LChromeY] = CThumb,
        'an explicit unchecked preview can still render a hidden control');
      LBeforePosition := LInfo.nPos;
      LScreen.X := LWindow.Left + LChromeX;
      LScreen.Y := LWindow.Top + LOrigin.Y + LClientRect.Bottom - 2;
      SendMessage(LControl.Handle, WM_NCLBUTTONDOWN, HTVSCROLL, Coordinates(LScreen.X, LScreen.Y));
      Windows.ScreenToClient(LControl.Handle, LScreen);
      SendMessage(LControl.Handle, WM_LBUTTONUP, 0, Coordinates(LScreen.X, LScreen.Y));
      Check(GetScrollInfo(LControl.Handle, SB_VERT, LInfo) and (LInfo.nPos = LBeforePosition),
        'a hidden control refuses a stale nonclient input message');
      LControl.Visible := True;
    end;

    LBeforePosition := LInfo.nPos;
    LScreen.X := LWindow.Left + LChromeX;
    LScreen.Y := LWindow.Top + LOrigin.Y + LClientRect.Bottom - 2;
    SendMessage(LControl.Handle, WM_NCLBUTTONDOWN, HTVSCROLL, Coordinates(LScreen.X, LScreen.Y));
    Windows.ScreenToClient(LControl.Handle, LScreen);
    SendMessage(LControl.Handle, WM_LBUTTONUP, 0, Coordinates(LScreen.X, LScreen.Y));
    Check(GetScrollInfo(LControl.Handle, SB_VERT, LInfo) and (LInfo.nPos > LBeforePosition),
      'the rendered lower arrow scrolls this actual control family');
    LBeforePosition := LInfo.nPos;
    LControl.Enabled := False;
    Windows.ClientToScreen(LControl.Handle, LScreen);
    SendMessage(LControl.Handle, WM_NCLBUTTONDOWN, HTVSCROLL, Coordinates(LScreen.X, LScreen.Y));
    Windows.ScreenToClient(LControl.Handle, LScreen);
    SendMessage(LControl.Handle, WM_LBUTTONUP, 0, Coordinates(LScreen.X, LScreen.Y));
    Check(GetScrollInfo(LControl.Handle, SB_VERT, LInfo) and (LInfo.nPos = LBeforePosition),
      'disabled rendered arrows refuse input for this control family');
    LControl.Enabled := True;

    if AFamily = npfMemo then
    begin
      TMemo(LControl).Text := TNyxText('One quiet line 🌙');
      Application.ProcessMessages;
      Check(GetScrollInfo(LControl.Handle, SB_VERT, LInfo) and
        (NyxScrollRange(LInfo.nMin, LInfo.nMax, LInfo.nPos, LInfo.nPage).LastPosition = LInfo.nMin),
        'the actual always-visible memo scrollbar has no remaining travel');
      Clear;
      Print(PRF_CLIENT or PRF_NONCLIENT or PRF_ERASEBKGND);
      SavePNG(LBitmap, 'memo-inactive');
      Check(LBitmap.Canvas.Pixels[COffsetX + LChromeX, COffsetY + LChromeY] = CTrack,
        'an inactive memo bar does not show a full-track active thumb');
      LScreen.X := LWindow.Left + LChromeX;
      LScreen.Y := LWindow.Top + LOrigin.Y + LClientRect.Bottom - 2;
      SendMessage(LControl.Handle, WM_NCLBUTTONDOWN, HTVSCROLL, Coordinates(LScreen.X, LScreen.Y));
      Check(GetCapture <> LControl.Handle, 'an inactive scrollbar does not capture a pointer stroke');
      Windows.ScreenToClient(LControl.Handle, LScreen);
      SendMessage(LControl.Handle, WM_LBUTTONUP, 0, Coordinates(LScreen.X, LScreen.Y));
      Check(GetScrollInfo(LControl.Handle, SB_VERT, LInfo) and (LInfo.nPos = LInfo.nMin),
        'an inactive scrollbar preserves its actual viewport');
      LChrome.Retire;
      LChrome := nil;
      TMemo(LControl).ScrollBars := ssVertical;
      TMemo(LControl).BorderStyle := bsNone;
      LControl.SetBounds(0, 0, 1, 1);
      Application.ProcessMessages;
      LChrome := AttachNyxLCLScrollChrome(LControl, LLifetime);
      Check(LChrome <> nil, 'a recreated borderless memo receives its own exact attachment');
      LChrome.Project(0.5, LTheme);
      GetWindowRect(LControl.Handle, LWindow);
      Check((LWindow.Right - LWindow.Left = 1) and (LWindow.Bottom - LWindow.Top = 1),
        'the native tiny-window print case has a real one-pixel allocation');
      Clear;
      LDC := LBitmap.Canvas.Handle;
      LSavedDC := SaveDC(LDC);
      try
        SetViewportOrgEx(LDC, COffsetX, COffsetY, nil);
        { Deliberately leave the caller clip larger than the HWND. Its system
          scroll inset exceeds this allocation; the adapter owns that boundary. }
        SendMessage(LControl.Handle, WM_PRINT, WPARAM(LDC), PRF_NONCLIENT);
        GetViewportOrgEx(LDC, LAfterOrigin);
        Check((LAfterOrigin.X = COffsetX) and (LAfterOrigin.Y = COffsetY),
          'tiny-window printing preserves the caller origin');
      finally
        RestoreDC(LDC, LSavedDC);
      end;
      Check((LBitmap.Canvas.Pixels[COffsetX - 1, COffsetY] = CSentinel) and
        (LBitmap.Canvas.Pixels[COffsetX + 1, COffsetY] = CSentinel) and
        (LBitmap.Canvas.Pixels[COffsetX, COffsetY + 1] = CSentinel),
        'tiny nonclient printing never overpaints neighboring caller pixels');
    end;
    LChrome.Retire;
    LChrome := nil;
    WriteLn('PASS print/input family ', CNames[AFamily]);
  finally

    if LChrome <> nil then
    begin
      LChrome.Retire;
    end;
    LChrome := nil;
    LFlags.Active := False;
    LBitmap.Free;
    LHost.Free;
    LTheme.Free;
    LLifetime := nil;
  end;
  Application.ProcessMessages;
end;

procedure RunPrintRetirement;
var
  LHost: TForm;
  LGrid: TPrintTrackedGrid;
  LFlags: TPrintLifetime;
  LLifetime: INyxLCLScrollChromeLifetime;
  LChrome: INyxLCLScrollChrome;
  LTheme: TNyxTheme;
  LBitmap: Graphics.TBitmap;
  LReceiver: TPrintRetirement;
  LBefore: Integer;
  LDC: HDC;
  LWindow: HWND;
  LOrigin: Windows.TPoint;
begin
  LHost := TForm.CreateNew(nil);
  LTheme := TNyxTheme.Create;
  LBitmap := Graphics.TBitmap.Create;
  LReceiver := TPrintRetirement.Create;
  LFlags := TPrintLifetime.Create;
  LLifetime := LFlags;
  LFlags.Active := True;
  LFlags.Physical := NewNyxNativeGestureFrame;
  LBefore := GRetiredGrids;
  try
    LHost.ClientWidth := 400;
    LHost.ClientHeight := 300;
    LGrid := TPrintTrackedGrid.Create(LHost);
    LGrid.Parent := LHost;
    LGrid.SetBounds(12, 18, 320, 220);
    LGrid.ColCount := 8;
    LGrid.RowCount := 100;
    LHost.Show;
    Application.ProcessMessages;
    LChrome := AttachNyxLCLScrollChrome(LGrid, LLifetime);
    Check(LChrome <> nil, 'the retirement grid has a real native attachment');
    LChrome.Project(0.5, LTheme);
    Application.ProcessMessages;
    LReceiver.Flags := LFlags;
    LReceiver.Chrome := LChrome;
    LReceiver.Grid := LGrid;
    LGrid.OnDrawCell := LReceiver.DrawCell;
    LBitmap.SetSize(350, 250);
    LBitmap.Canvas.Brush.Color := CSentinel;
    LBitmap.Canvas.FillRect(Types.Rect(0, 0, LBitmap.Width, LBitmap.Height));
    LDC := LBitmap.Canvas.Handle;
    SetViewportOrgEx(LDC, COffsetX, COffsetY, nil);
    LWindow := LGrid.Handle;
    SendMessage(LWindow, WM_PRINT, WPARAM(LDC), PRF_CLIENT or PRF_NONCLIENT);
    { The grid may be queued for physical destruction now. Never dereference
      the borrowed grid again; only its detached receiver/result flags remain. }
    LGrid := nil;
    GetViewportOrgEx(LDC, LOrigin);
    Check(LReceiver.Called and LReceiver.EnteredFrame,
      'the actual client painter retires its grid inside the original physical frame');
    Check(not LChrome.Attached, 'client retirement revokes the print attachment');
    Check((LOrigin.X = COffsetX) and (LOrigin.Y = COffsetY),
      'retirement preserves the borrowed print DC origin');
    SetViewportOrgEx(LDC, 0, 0, nil);
    Check(LBitmap.Canvas.Pixels[COffsetX + 319, COffsetY + 30] = CSentinel,
      'a retired client cannot receive a late nonclient chrome draw');
    Application.ProcessMessages;
    Check(GRetiredGrids = LBefore + 1, 'physical retirement destroys the original grid exactly once');
    SavePNG(LBitmap, 'retired-client');
  finally
    LFlags.Active := False;

    if LChrome <> nil then
    begin
      LChrome.Retire;
    end;
    LChrome := nil;
    LReceiver.Chrome := nil;
    LBitmap.Free;
    LHost.Free;
    Application.ProcessMessages;
    LReceiver.Free;
    LTheme.Free;
    LLifetime := nil;
  end;
end;

var
  LFamily: TPrintFamily;
begin
  Application.Initialize;
  GFailures := TPrintFailures.Create;
  Application.OnException := GFailures.Failed;
  try
    try

      if ParamCount > 0 then
      begin
        GDirectory := IncludeTrailingPathDelimiter(TNyxText(ParamStr(1)));
        ForceDirectories(GDirectory);
      end;
      for LFamily := Low(TPrintFamily) to High(TPrintFamily) do
      begin
        RunFamily(LFamily);
      end;
      RunPrintRetirement;
      Check(GFailures.MessageText = '', 'native printing completes without asynchronous callback errors');
      WriteLn('PASS ', GChecks, ' actual native scroll-chrome print checks');
      WriteLn('DISPLAY ', GDisplayCaptures, ' owned foreground-window captures');
    except
      on E: Exception do
      begin
        WriteLn('FAIL ', E.Message);
        ExitCode := 1;
      end;
    end;
  finally
    Application.OnException := nil;
    GFailures.Free;
  end;
end.
