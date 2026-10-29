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

unit nyx.scroll.chrome.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses Controls, nyx.text, nyx.scroll.chrome, nyx.scroll.presentation,
  nyx.theme, nyx.gestures.lcl;

type
  { Weak exact mount authority. Getters read admission flags without application
    callouts; Frame returns the original physical retirement scope. The bridge
    owns this port, never its renderer, document or borrowed native control. }
  INyxLCLScrollChromeLifetime = interface(IInterface)
    ['{4908D335-D53A-4FF5-9BE7-96CFDD379EEE}']
    function Current: Boolean;
    function InputReady: Boolean;
    { Captured before a pointer stroke. A changed/saturated epoch cannot revive
      that stroke after edit suspension or enabled-state cycling. }
    function InputEpoch: Integer;
    function Frame: INyxNativeGestureFrame;
  end;

  { UI-only per-window attachment. Project copies absolute logical metrics and
    semantic colors; no theme pointer survives. Retire revokes input before
    removing its own native subclass. Original editor/HWND identity is retained. }
  INyxLCLScrollChrome = interface(IInterface)
    ['{A6C7DAB2-045E-434D-9F53-B25694F3DB0D}']
    procedure Project(AScale: Double; ATheme: TNyxTheme);
    { Memo positions use content units. Other families retain their own existing
      viewport owner; this port must not overwrite grid projection snapshots. }
    procedure RememberViewport;
    procedure RestoreViewport;
    procedure Retire;
    function Attached: Boolean;
  end;

  { Additive caller-choice capability preserves the original chrome interface
    layout. Alternate attachments can retain their existing implementation;
    explicit policies require this facet. It copies intent, never theme/widget
    ownership, and resolves size against original widgetset system metrics. }
  INyxLCLScrollPresentation = interface(IInterface)
    ['{2BE28849-0D5C-4C2F-9178-1E064532685D}']
    procedure ProjectPresentation(AScale: Double; ATheme: TNyxTheme;
      const APresentation: TNyxScrollPresentation);
  end;

{ Win32 memo, scrolling-host, grid and Tree nonclient scrolling. Unknown controls
  and other widgetsets return nil, retaining their original implementation. The
  attachment uses Comctl32 subclass chaining, not the obsolete FlatSB API.
  Standard edit horizontal positions beyond the native 16-bit thumb message use
  nearest character steps; endpoints stay exact. No large position is truncated.
  Attach only on the original HWND's UI thread with an exact live authority. }
function AttachNyxLCLScrollChrome(AControl: TWinControl;
  const ALifetime: INyxLCLScrollChromeLifetime): INyxLCLScrollChrome;

{ Allocation-free family/widgetset query. A renderer can skip constructing a
  scoped input authority for controls whose native chrome it cannot project. }
function NyxLCLSupportsScrollChrome(AControl: TWinControl): Boolean;

implementation

uses SysUtils, Classes, Types, Forms, StdCtrls, ExtCtrls, Grids, ComCtrls, Graphics,
  Math, LMessages
  {$ifdef MSWINDOWS}, Windows, CommCtrl, InterfaceBase, LCLPlatformDef{$endif};

{$ifdef MSWINDOWS}
type
  { Protected LCL dispatch and bounds realization only. The original widgetset
    realizes its own pending bounds before a frame-only native notification.
    Custom controls consume full-width TLMScroll.Pos rather than a packed word. }
  TScrollControlAccess = class(TWinControl);

  { A copied per-axis anchor vector. A named value type keeps assignments exact
    across FPC/Delphi instead of relying on anonymous-array compatibility. }
  TScrollPositions = array[TNyxScrollAxis] of Integer;

  { Native HWND scope. The installed chain retains a private lease and each
    callback leases it again. Control is borrowed until explicit retirement or
    WM_NCDESTROY; source changes/enable cycling cancel the original drag. }
  TScrollChrome = class(TInterfacedObject, INyxLCLScrollChrome, INyxLCLScrollPresentation)
  private
    FControl: TWinControl;
    FWindow: HWND;
    FNativeLease: INyxLCLScrollChrome;
    FLifetime: INyxLCLScrollChromeLifetime;
    FFrame: INyxNativeGestureFrame;
    FActive: Boolean;
    FScale: Double;
    FInsets: Windows.TRect;
    FHaveInsets: Boolean;
    FBackground: TColor;
    FBorder: TColor;
    FThumb: TColor;
    FForeground: TColor;
    FMetrics: TNyxScrollChromeMetrics;
    FDown: Boolean;
    FAxis: TNyxScrollAxis;
    FPart: TNyxScrollChromePart;
    FGrab: Integer;
    FTrackPosition: Integer;
    FStrokeEpoch: Integer;
    FTimer: UINT_PTR;
    FRepeat: Boolean;
    FHaveViewport: Boolean;
    FViewportEpoch: Integer;
    FViewportInputEpoch: Integer;
    FPositions: TScrollPositions;
    FSeeking: Boolean;
    FInstance: UINT_PTR;
    FRestoreQueued: Boolean;
    FRestoreEpoch: Integer;
    function Current: Boolean;
    function Ready: Boolean;
    function StrokeReady: Boolean;
    procedure Detach(ADead: Boolean);
    procedure CancelStroke;
    function Range(AAxis: TNyxScrollAxis): TNyxScrollRange;
    function Bar(AAxis: TNyxScrollAxis; out ARect: Windows.TRect;
      out AGeometry: TNyxScrollChromeGeometry): Boolean;
    function Pick(AX, AY: Integer; out AAxis: TNyxScrollAxis;
      out APart: TNyxScrollChromePart; out AAlong: Integer): Boolean;
    procedure Scroll(AAxis: TNyxScrollAxis; APart: TNyxScrollChromePart;
      APosition: Integer = 0);
    procedure Paint(ADC: HDC = 0);
    procedure Press(AX, AY: Integer);
    procedure Move(AX, AY: Integer);
    procedure RepeatStroke;
    procedure InvalidateViewport;
    procedure SeekMemoHorizontal(const ARange: TNyxScrollRange; APosition: Integer);
    procedure Seek(AAxis: TNyxScrollAxis; APosition: Integer;
      ACommit: Boolean = False);
    procedure ReleaseStroke;
    procedure ApplyViewport;
  public
    constructor Create(AControl: TWinControl;
      const ALifetime: INyxLCLScrollChromeLifetime);
    function Install: Boolean;
    function Handle(AWindow: HWND; AMessage: UINT; AWParam: WPARAM;
      ALParam: LPARAM): LRESULT;
    procedure Project(AScale: Double; ATheme: TNyxTheme);
    procedure ProjectPresentation(AScale: Double; ATheme: TNyxTheme;
      const APresentation: TNyxScrollPresentation);
    procedure RememberViewport;
    procedure RestoreViewport;
    procedure Retire;
    function Attached: Boolean;
  end;

const
  CScrollOwner: PWideChar = 'nyx.scroll.chrome.a6c7dab2';
  CScrollSubclass = 1;
  CNativeAxis: array[TNyxScrollAxis] of Integer = (SB_HORZ, SB_VERT);

var
  { UI-thread monotonically admitted receipts. Queued HWND messages carry only
    these integers, so handle/address reuse cannot redirect an old restoration. }
  GScrollInstance: UINT_PTR;
  { Registered Win32 message namespace avoids collisions with application/LCL
    private messages. It carries no object pointer or ownership lease. }
  GRestoreMessage: UINT;

function ScrollSubclass(AWindow: HWND; AMessage: UINT; AWParam: WPARAM;
  ALParam: LPARAM; AID: UINT_PTR; AReference: DWORD_PTR): LRESULT; stdcall;
var
  LOwner: TScrollChrome;
  LLease: INyxLCLScrollChrome;
  LFrame: INyxNativeGestureFrame;
begin
  LOwner := TScrollChrome(Pointer(AReference));
  LLease := LOwner;
  LFrame := LOwner.FFrame;
  Result := 0;
  LFrame.Enter;
  try
    try
      Result := LOwner.Handle(AWindow, AMessage, AWParam, ALParam);
    except
      Application.HandleException(LOwner);
    end;
  finally
    LFrame.Leave;
  end;
end;

constructor TScrollChrome.Create(AControl: TWinControl;
  const ALifetime: INyxLCLScrollChromeLifetime);
begin
  inherited Create;
  FControl := AControl;
  FWindow := AControl.Handle;
  FLifetime := ALifetime;
  FFrame := ALifetime.Frame;
  FScale := 1;
  FMetrics := NyxScrollChromeMetrics;
end;

function TScrollChrome.Install: Boolean;
begin

  if GScrollInstance = High(UINT_PTR) then
  begin
    raise ENyxScrollChrome.Create('Scroll window identity space is exhausted');
  end;

  if GRestoreMessage = 0 then
  begin
    GRestoreMessage := RegisterWindowMessageW('nyx.scroll.chrome.restore.a6c7dab2');

    if GRestoreMessage = 0 then
    begin
      raise ENyxScrollChrome.Create('Unable to register scroll viewport restoration');
    end;
  end;
  Inc(GScrollInstance);
  FInstance := GScrollInstance;

  if GetPropW(FWindow, CScrollOwner) <> 0 then
  begin
    raise ENyxScrollChrome.Create('Scroll chrome already has an owner on this window');
  end;
  FNativeLease := Self;
  Result := SetPropW(FWindow, CScrollOwner, Windows.HANDLE(Self)) and
    SetWindowSubclass(FWindow, @ScrollSubclass, CScrollSubclass, DWORD_PTR(Self));
  FActive := Result;

  if not Result then
  begin

    if GetPropW(FWindow, CScrollOwner) = Windows.HANDLE(Self) then
    begin
      RemovePropW(FWindow, CScrollOwner);
    end;
    FControl := nil;
    FWindow := 0;
    FNativeLease := nil;
  end;
end;

function TScrollChrome.Current: Boolean;
begin
  Result := FActive and (FWindow <> 0) and (FControl <> nil) and
    FLifetime.Current and (GetPropW(FWindow, CScrollOwner) = Windows.HANDLE(Self));
end;

function TScrollChrome.Ready: Boolean;
begin
  Result := Current and FLifetime.InputReady and IsWindowEnabled(FWindow) and
    IsWindowVisible(FWindow);
end;

function TScrollChrome.StrokeReady: Boolean;
begin
  Result := FDown and Ready and (FStrokeEpoch >= 0) and
    (FStrokeEpoch < High(Integer)) and (FLifetime.InputEpoch = FStrokeEpoch);
end;

function TScrollChrome.Attached: Boolean;
begin
  Result := Current;
end;

procedure TScrollChrome.CancelStroke;
var
  LWindow: HWND;
begin
  FDown := False;
  LWindow := FWindow;

  if (LWindow <> 0) and (FTimer <> 0) then
  begin
    KillTimer(LWindow, FTimer);
  end;
  FTimer := 0;

  if (LWindow <> 0) and (GetCapture = LWindow) then
  begin
    ReleaseCapture;
  end;
end;

procedure TScrollChrome.Detach(ADead: Boolean);
var
  LWindow: HWND;
  LRemoved: Boolean;
begin
  FActive := False;
  CancelStroke;
  FControl := nil;
  LWindow := FWindow;

  if LWindow <> 0 then
  begin
    LRemoved := RemoveWindowSubclass(LWindow, @ScrollSubclass, CScrollSubclass);

    if not LRemoved and not ADead and IsWindow(LWindow) then
    begin
      { Opaque native reference data must remain owned until actual destruction. }
      Exit;
    end;

    if GetPropW(LWindow, CScrollOwner) = Windows.HANDLE(Self) then
    begin
      RemovePropW(LWindow, CScrollOwner);
    end;
    FWindow := 0;
  end;
  FNativeLease := nil;
end;

procedure TScrollChrome.Retire;
var
  LLease: INyxLCLScrollChrome;
  LWindow: HWND;
  LRestore: Boolean;
begin
  LLease := Self;
  LWindow := FWindow;
  LRestore := Current and not (csDestroying in FControl.ComponentState);

  if (LWindow <> 0) and IsWindow(LWindow) and
    (GetWindowThreadProcessId(LWindow, nil) <> GetCurrentThreadId) then
  begin
    raise ENyxScrollChrome.Create('Scroll chrome retirement requires the window thread');
  end;
  Detach(False);

  if LRestore and (FWindow = 0) and IsWindow(LWindow) then
  begin
    SetWindowPos(LWindow, 0, 0, 0, 0, 0,
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER or SWP_NOACTIVATE or SWP_FRAMECHANGED);
  end;
end;

function TScrollChrome.Range(AAxis: TNyxScrollAxis): TNyxScrollRange;
var
  LInfo: TScrollInfo;
  LCount: Int64;
begin
  LInfo := Default(TScrollInfo);
  LInfo.cbSize := SizeOf(LInfo);
  LInfo.fMask := SIF_ALL;

  if not Current or not GetScrollInfo(FWindow, CNativeAxis[AAxis], LInfo) then
  begin
    Exit(NyxScrollRange(0, 0, 0, 1));
  end;
  LCount := Int64(LInfo.nMax) - Int64(LInfo.nMin) + 1;
  Result := NyxScrollRange(LInfo.nMin, LInfo.nMax, LInfo.nPos,
    Min(Int64(LInfo.nPage), LCount));
end;

function Pixel(AValue: Integer; AScale: Double): Integer;
var
  LValue: Double;
begin
  LValue := AValue * AScale;

  if IsNan(LValue) or IsInfinite(LValue) or (Abs(LValue) > High(Integer) - 1.0) then
  begin
    raise ENyxScrollChrome.Create('Native scroll inset exceeds its integer range');
  end;
  Result := Round(LValue);

  if AValue > 0 then
  begin
    Result := Max(1, Result);
  end;
end;

function TScrollChrome.Bar(AAxis: TNyxScrollAxis; out ARect: Windows.TRect;
  out AGeometry: TNyxScrollChromeGeometry): Boolean;
var
  LWindow: Windows.TRect;
  LStyle: LongInt;
  LBorder: Integer;
  LLeft: Boolean;
  LExtent: Integer;
begin
  Result := False;
  ARect := Default(Windows.TRect);
  AGeometry := Default(TNyxScrollChromeGeometry);

  if not Current or not FHaveInsets then
  begin
    Exit;
  end;
  LStyle := GetWindowLong(FWindow, GWL_STYLE);

  if ((AAxis = nsaVertical) and ((LStyle and WS_VSCROLL) = 0)) or
    ((AAxis = nsaHorizontal) and ((LStyle and WS_HSCROLL) = 0)) then
  begin
    Exit;
  end;
  GetWindowRect(FWindow, LWindow);
  LWindow.Right := LWindow.Right - LWindow.Left;
  LWindow.Bottom := LWindow.Bottom - LWindow.Top;
  LWindow.Left := 0;
  LWindow.Top := 0;
  LLeft := (GetWindowLong(FWindow, GWL_EXSTYLE) and
    (WS_EX_LEFTSCROLLBAR or WS_EX_LAYOUTRTL)) <> 0;

  if AAxis = nsaVertical then
  begin
    ARect.Top := Pixel(FInsets.Top, FScale);
    ARect.Bottom := LWindow.Bottom - Pixel(FInsets.Bottom, FScale);

    if LLeft then
    begin
      LBorder := Max(0, FInsets.Left - GetSystemMetrics(SM_CXVSCROLL));
      ARect.Left := Pixel(LBorder, FScale);
      ARect.Right := Pixel(FInsets.Left, FScale);
    end
    else
    begin
      LBorder := Max(0, FInsets.Right - GetSystemMetrics(SM_CXVSCROLL));
      ARect.Left := LWindow.Right - Pixel(FInsets.Right, FScale);
      ARect.Right := LWindow.Right - Pixel(LBorder, FScale);
    end;
    LExtent := ARect.Bottom - ARect.Top;
  end
  else
  begin
    ARect.Left := Pixel(FInsets.Left, FScale);
    ARect.Right := LWindow.Right - Pixel(FInsets.Right, FScale);
    LBorder := Max(0, FInsets.Bottom - GetSystemMetrics(SM_CYHSCROLL));
    ARect.Top := LWindow.Bottom - Pixel(FInsets.Bottom, FScale);
    ARect.Bottom := LWindow.Bottom - Pixel(LBorder, FScale);
    LExtent := ARect.Right - ARect.Left;
  end;
  Result := (ARect.Right > ARect.Left) and (ARect.Bottom > ARect.Top);

  if Result then
  begin
    AGeometry := LayoutNyxScrollChrome(LExtent, FScale, Range(AAxis), FMetrics);

    if FDown and (FPart = nscpThumb) and (FAxis = AAxis) then
    begin
      { Deferred-tracking grids keep their source position until release. The
        thumb still follows the admitted pointer without changing that policy. }
      AGeometry.Source.Position := AGeometry.Source.Clamp(FTrackPosition);
      AGeometry := LayoutNyxScrollChrome(LExtent, FScale, AGeometry.Source, FMetrics);
    end;
  end;
end;

function TScrollChrome.Pick(AX, AY: Integer; out AAxis: TNyxScrollAxis;
  out APart: TNyxScrollChromePart; out AAlong: Integer): Boolean;
var
  LRect: Windows.TRect;
  LGeometry: TNyxScrollChromeGeometry;
  LAxis: TNyxScrollAxis;
begin
  Result := False;
  for LAxis := Low(TNyxScrollAxis) to High(TNyxScrollAxis) do
  begin

    if Bar(LAxis, LRect, LGeometry) and
      (AX >= LRect.Left) and (AX < LRect.Right) and
      (AY >= LRect.Top) and (AY < LRect.Bottom) then
    begin

      AAxis := LAxis;

      if AAxis = nsaVertical then
      begin
        AAlong := AY - LRect.Top;
      end
      else
      begin
        AAlong := AX - LRect.Left;
      end;
      APart := LGeometry.Hit(AAlong);
      Exit(True);
    end;
  end;
end;

procedure TScrollChrome.Scroll(AAxis: TNyxScrollAxis;
  APart: TNyxScrollChromePart; APosition: Integer);
const
  CCommands: array[TNyxScrollChromePart] of SmallInt =
    (SB_ENDSCROLL, SB_LINEUP, SB_PAGEUP, SB_THUMBTRACK, SB_PAGEDOWN, SB_LINEDOWN);
var
  LMessage: TLMScroll;
  LControl: TWinControl;
  LRange: TNyxScrollRange;
begin

  if not Ready then
  begin
    CancelStroke;
    Exit;
  end;
  LRange := Range(AAxis);

  if LRange.LastPosition <= LRange.Minimum then
  begin
    CancelStroke;
    Exit;
  end;
  InvalidateViewport;

  if APart = nscpThumb then
  begin
    Seek(AAxis, APosition);

    if Current then
    begin
      Paint;
    end;
    Exit;
  end;
  LControl := FControl;
  LMessage := Default(TLMScroll);
  LMessage.Msg := LM_HSCROLL;

  if AAxis = nsaVertical then
  begin
    LMessage.Msg := LM_VSCROLL;
  end;
  LMessage.ScrollCode := CCommands[APart];
  LMessage.Pos := APosition;

  if LControl is TCustomMemo then
  begin
    SendMessage(FWindow, LMessage.Msg, WPARAM(LMessage.ScrollCode), 0);
  end
  else
  begin
    { LCL's own full-width message, not packed Windows thumb coordinates. }
    TScrollControlAccess(LControl).WindowProc(LMessages.PLMessage(@LMessage)^);
  end;

  if Current then
  begin
    Paint;
  end;
end;

procedure TScrollChrome.InvalidateViewport;
begin
  FHaveViewport := False;
  FRestoreQueued := False;

  if FViewportEpoch < High(Integer) then
  begin
    Inc(FViewportEpoch);
  end;
end;

procedure TScrollChrome.SeekMemoHorizontal(const ARange: TNyxScrollRange;
  APosition: Integer);
var
  LTarget: HWND;
  LDC: HDC;
  LFont: HFONT;
  LPrevious: HGDIOBJ;
  LMetrics: TTextMetricW;
  LCharacters: Integer;
begin
  LTarget := FWindow;

  if APosition = ARange.Minimum then
  begin
    SendMessage(LTarget, WM_HSCROLL, SB_LEFT, 0);
  end
  else if APosition = ARange.LastPosition then
  begin
    SendMessage(LTarget, WM_HSCROLL, SB_RIGHT, 0);
  end
  else if (APosition >= 0) and (APosition <= High(Word)) then
  begin
    SendMessage(LTarget, WM_HSCROLL,
      WPARAM(SB_THUMBPOSITION) or (WPARAM(APosition) shl 16), 0);
  end
  else
  begin
    { Standard EDIT exposes pixels in its horizontal scroll range, but
      EM_LINESCROLL takes average-character steps. Read the actual native font;
      a pixel delta used as characters overshoots, then oscillates on release.
      The full-width fallback has native character granularity, never modulo
      65536. The small-position route above retains exact pixel positioning. }
    LDC := GetDC(LTarget);

    if LDC = 0 then
    begin
      raise ENyxScrollChrome.Create('Unable to measure native horizontal scroll units');
    end;
    LPrevious := 0;
    try
      LFont := HFONT(SendMessage(LTarget, WM_GETFONT, 0, 0));

      if not Current then
      begin
        Exit;
      end;

      if LFont <> 0 then
      begin
        LPrevious := SelectObject(LDC, LFont);
      end;

      if not GetTextMetricsW(LDC, @LMetrics) then
      begin
        raise ENyxScrollChrome.Create('Unable to read native horizontal scroll units');
      end;
    finally

      if LPrevious <> 0 then
      begin
        SelectObject(LDC, LPrevious);
      end;
      ReleaseDC(LTarget, LDC);
    end;
    LCharacters := Round((Double(ARange.Position) - APosition) /
      Max(1, LMetrics.tmAveCharWidth));
    TCustomMemo(FControl).ScrollBy(LCharacters, 0);
  end;
end;

procedure TScrollChrome.Seek(AAxis: TNyxScrollAxis; APosition: Integer;
  ACommit: Boolean);
var
  LRange: TNyxScrollRange;
  LControl: TWinControl;
  LMessage: TLMScroll;
  LDelta: Integer;
  LWasSeeking: Boolean;
begin

  if not Current then
  begin
    Exit;
  end;
  LRange := Range(AAxis);
  APosition := LRange.Clamp(APosition);

  if (APosition = LRange.Position) and not ACommit then
  begin
    Exit;
  end;
  LControl := FControl;
  LWasSeeking := FSeeking;
  FSeeking := True;
  try

    if LControl is TCustomMemo then
    begin
      LDelta := Integer(Int64(LRange.Position) - Int64(APosition));

      if AAxis = nsaVertical then
      begin
        TCustomMemo(LControl).ScrollBy(0, LDelta);
      end
      else
      begin
        SeekMemoHorizontal(LRange, APosition);
      end;
    end
    else
    begin
      LMessage := Default(TLMScroll);
      LMessage.Msg := LM_HSCROLL;

      if AAxis = nsaVertical then
      begin
        LMessage.Msg := LM_VSCROLL;
      end;
      LMessage.ScrollCode := SB_THUMBTRACK;

      if ACommit then
      begin
        LMessage.ScrollCode := SB_THUMBPOSITION;
      end;
      LMessage.Pos := APosition;
      TScrollControlAccess(LControl).WindowProc(LMessages.PLMessage(@LMessage)^);
    end;
  finally
    FSeeking := LWasSeeking;
  end;
end;

procedure TScrollChrome.RememberViewport;
var
  LAxis: TNyxScrollAxis;
  LInputEpoch: Integer;
begin

  if not Current or not (FControl is TCustomMemo) or
    (FViewportEpoch = High(Integer)) then
  begin
    FHaveViewport := False;
    Exit;
  end;
  LInputEpoch := FLifetime.InputEpoch;

  if FHaveViewport and (FViewportInputEpoch <> LInputEpoch) then
  begin
    InvalidateViewport;
  end;

  if (LInputEpoch < 0) or (LInputEpoch = High(Integer)) or
    (FViewportEpoch = High(Integer)) then
  begin
    InvalidateViewport;
    Exit;
  end;

  if FRestoreQueued and FHaveViewport and (FRestoreEpoch = FViewportEpoch) then
  begin
    { Several projections may complete before their queued realization. Retain
      the original anchor instead of capturing an intermediate native clamp. }
    Exit;
  end;
  FHaveViewport := False;
  FViewportInputEpoch := LInputEpoch;
  for LAxis := Low(TNyxScrollAxis) to High(TNyxScrollAxis) do
  begin

    if LAxis = nsaVertical then
    begin
      FPositions[LAxis] := TCustomMemo(FControl).VertScrollBar.Position;
    end
    else
    begin
      FPositions[LAxis] := TCustomMemo(FControl).HorzScrollBar.Position;
    end;
  end;
  FHaveViewport := True;
end;

procedure TScrollChrome.ApplyViewport;
var
  LLease: INyxLCLScrollChrome;
  LEpoch: Integer;
  LAxis: TNyxScrollAxis;
  LPositions: TScrollPositions;
begin
  LLease := Self;
  LEpoch := FViewportEpoch;
  LPositions := FPositions;

  if FHaveViewport and (FLifetime.InputEpoch <> FViewportInputEpoch) then
  begin
    { A controlled collection refresh can move a native viewport while physical
      input is suspended. Its older queued anchor cannot retarget the new epoch. }
    InvalidateViewport;
    Exit;
  end;
  for LAxis := Low(TNyxScrollAxis) to High(TNyxScrollAxis) do
  begin

    if not Current or not FHaveViewport or (FViewportEpoch <> LEpoch) or
      (FLifetime.InputEpoch <> FViewportInputEpoch) then
    begin
      Exit;
    end;
    Seek(LAxis, LPositions[LAxis]);
  end;
end;

procedure TScrollChrome.RestoreViewport;
var
  LLease: INyxLCLScrollChrome;
begin
  LLease := Self;
  ApplyViewport;

  if Current and FHaveViewport and not FRestoreQueued then
  begin
    { LCL may still own a deferred native bounds batch when projection returns.
      Restore again after that realization, carrying only copied anchor/epoch
      values. Retirement removes the callback; a reused HWND cannot match this
      attachment receipt. No logical section or physical frame crosses a turn. }
    FRestoreEpoch := FViewportEpoch;
    FRestoreQueued := PostMessage(FWindow, GRestoreMessage, FInstance, FRestoreEpoch);

    if not FRestoreQueued then
    begin
      raise ENyxScrollChrome.Create('Unable to queue native scroll viewport restoration');
    end;
  end;
end;

procedure TScrollChrome.Paint(ADC: HDC);
var
  LDC: HDC;
  LSave: Integer;
  LCanvas: TCanvas;
  LWindow: Windows.TRect;
  LClient: Windows.TRect;
  LOrigin: Windows.TPoint;
  LRect: Windows.TRect;
  LThumb: Windows.TRect;
  LGeometry: TNyxScrollChromeGeometry;
  LAxis: TNyxScrollAxis;
  LCenter: Integer;
  LCross: Integer;
  LSize: Integer;
  LStep: Integer;
  LArrow: Integer;
  LTarget: HWND;
  LScrollable: Boolean;
  LEnabled: Boolean;
  LDirection: Integer;
  LBase: Integer;
begin

  if not Current or not FHaveInsets then
  begin
    Exit;
  end;
  LTarget := FWindow;
  LDC := ADC;

  if LDC = 0 then
  begin
    LDC := GetWindowDC(LTarget);
  end;

  if LDC = 0 then
  begin
    Exit;
  end;
  LCanvas := nil;
  LSave := SaveDC(LDC);
  try

    if LSave = 0 then
    begin
      raise ENyxScrollChrome.Create('Unable to preserve the scroll paint context');
    end;
    { Both DC ownership and restoration already have a finally before canvas
      allocation. Failure must not leak an owned window DC or change a borrowed
      caller's state. No drawing starts without a restorable context. }
    LCanvas := TCanvas.Create;
    GetWindowRect(LTarget, LWindow);
    GetClientRect(LTarget, LClient);
    LOrigin.X := 0;
    LOrigin.Y := 0;
    Windows.ClientToScreen(LTarget, LOrigin);
    OffsetRect(LClient, LOrigin.X - LWindow.Left, LOrigin.Y - LWindow.Top);
    { A borrowed print DC need not have a window clip. Small allocations must
      never let a bar or glyph paint outside this HWND's own footprint. }
    IntersectClipRect(LDC, 0, 0, LWindow.Right - LWindow.Left,
      LWindow.Bottom - LWindow.Top);
    ExcludeClipRect(LDC, LClient.Left, LClient.Top, LClient.Right, LClient.Bottom);
    LCanvas.Handle := LDC;
    LCanvas.Brush.Color := FBorder;
    LCanvas.FillRect(Types.Rect(0, 0,
      LWindow.Right - LWindow.Left, LWindow.Bottom - LWindow.Top));
    LEnabled := IsWindowEnabled(LTarget);
    for LAxis := Low(TNyxScrollAxis) to High(TNyxScrollAxis) do
    begin

      if not Bar(LAxis, LRect, LGeometry) then
      begin
        Continue;
      end;
      LCanvas.Brush.Color := FBackground;
      LCanvas.FillRect(LRect);
      LScrollable := LGeometry.Source.LastPosition > LGeometry.Source.Minimum;
      LThumb := LRect;

      if LAxis = nsaVertical then
      begin
        Inc(LThumb.Top, LGeometry.ThumbStart);
        LThumb.Bottom := LThumb.Top + LGeometry.ThumbLength;
        LCross := (LRect.Left + LRect.Right) div 2;
        LSize := Max(1, (LRect.Right - LRect.Left) div 4);
      end
      else
      begin
        Inc(LThumb.Left, LGeometry.ThumbStart);
        LThumb.Right := LThumb.Left + LGeometry.ThumbLength;
        LCross := (LRect.Top + LRect.Bottom) div 2;
        LSize := Max(1, (LRect.Bottom - LRect.Top) div 4);
      end;
      { An always-visible native bar can have no travel. Its fully paged range
        must not become an active-looking thumb filling the entire track. }

      if LScrollable then
      begin
        LCanvas.Brush.Color := FThumb;

        if not LEnabled then
        begin
          LCanvas.Brush.Color := FBorder;
        end;
        LCanvas.FillRect(LThumb);
      end;
      LCanvas.Pen.Color := FForeground;

      if not LScrollable or not LEnabled then
      begin
        LCanvas.Pen.Color := FBorder;
      end;

      if LGeometry.Arrow = 0 then
      begin
        Continue;
      end;
      LSize := Min(LSize, Max(0, (LGeometry.Arrow - 1) div 2));
      for LArrow := 0 to 1 do
      begin
        LCenter := LGeometry.Arrow div 2;

        if LArrow = 1 then
        begin
          LCenter := LGeometry.Extent - LGeometry.Arrow div 2;
        end;

        if LAxis = nsaVertical then
        begin
          Inc(LCenter, LRect.Top);
        end
        else
        begin
          Inc(LCenter, LRect.Left);
        end;
        { The leading action decreases position (up/left), and the trailing
          action increases it (down/right). Center each filled triangle in its
          actual arrow cell; include its tip despite LineTo's exclusive end. }
        LDirection := LArrow * 2 - 1;
        LBase := LCenter - LDirection * (LSize div 2);
        for LStep := -LSize to LSize do
        begin

          if LAxis = nsaVertical then
          begin
            LCanvas.MoveTo(LCross + LStep, LBase);
            LCanvas.LineTo(LCross + LStep,
              LBase + LDirection * (LSize - Abs(LStep) + 1));
          end
          else
          begin
            LCanvas.MoveTo(LBase, LCross + LStep);
            LCanvas.LineTo(LBase + LDirection * (LSize - Abs(LStep) + 1),
              LCross + LStep);
          end;
        end;
      end;
    end;
  finally

    if LCanvas <> nil then
    begin
      LCanvas.Handle := 0;
      LCanvas.Free;
    end;

    if LSave <> 0 then
    begin
      RestoreDC(LDC, LSave);
    end;

    if ADC = 0 then
    begin
      ReleaseDC(LTarget, LDC);
    end;
  end;
end;

procedure TScrollChrome.Press(AX, AY: Integer);
var
  LAlong: Integer;
  LRect: Windows.TRect;
  LGeometry: TNyxScrollChromeGeometry;
  LRange: TNyxScrollRange;
begin

  if not Ready or not Pick(AX, AY, FAxis, FPart, LAlong) then
  begin
    Exit;
  end;
  LRange := Range(FAxis);

  if LRange.LastPosition <= LRange.Minimum then
  begin
    Exit;
  end;
  FDown := True;
  FRepeat := False;
  FStrokeEpoch := FLifetime.InputEpoch;
  FTrackPosition := Range(FAxis).Position;
  SetCapture(FWindow);

  if not StrokeReady then
  begin
    CancelStroke;
    Exit;
  end;

  if FPart = nscpThumb then
  begin
    Bar(FAxis, LRect, LGeometry);
    FGrab := LAlong - LGeometry.ThumbStart;
  end
  else
  begin
    Scroll(FAxis, FPart);

    if StrokeReady then
    begin
      FTimer := SetTimer(FWindow, UINT_PTR(Self), 350, nil);
    end;
  end;
end;

procedure TScrollChrome.Move(AX, AY: Integer);
var
  LRect: Windows.TRect;
  LGeometry: TNyxScrollChromeGeometry;
  LAlong: Integer;
begin

  if not FDown then
  begin
    Exit;
  end;

  if not StrokeReady or (GetCapture <> FWindow) then
  begin
    CancelStroke;
    Exit;
  end;

  if (FPart = nscpThumb) and Bar(FAxis, LRect, LGeometry) then
  begin
    LAlong := AX - LRect.Left;

    if FAxis = nsaVertical then
    begin
      LAlong := AY - LRect.Top;
    end;
    FTrackPosition := LGeometry.PositionAt(LAlong, FGrab);
    Scroll(FAxis, nscpThumb, FTrackPosition);
  end;
end;

procedure TScrollChrome.RepeatStroke;
var
  LPoint: Windows.TPoint;
  LWindow: Windows.TRect;
  LAxis: TNyxScrollAxis;
  LPart: TNyxScrollChromePart;
  LAlong: Integer;
begin

  if not StrokeReady or (GetCapture <> FWindow) then
  begin
    CancelStroke;
    Exit;
  end;

  if not FRepeat then
  begin
    FRepeat := True;
    SetTimer(FWindow, FTimer, 60, nil);
  end;

  if GetCursorPos(LPoint) and GetWindowRect(FWindow, LWindow) and
    Pick(LPoint.X - LWindow.Left, LPoint.Y - LWindow.Top, LAxis, LPart, LAlong) and
    (LAxis = FAxis) and (LPart = FPart) then
  begin
    Scroll(FAxis, FPart);
  end;
end;

procedure TScrollChrome.ReleaseStroke;
var
  LAxis: TNyxScrollAxis;
  LPosition: Integer;
  LInputEpoch: Integer;
  LSourceEpoch: Integer;
  LCommit: Boolean;
begin
  LAxis := FAxis;
  LPosition := FTrackPosition;
  LInputEpoch := FStrokeEpoch;
  LSourceEpoch := FViewportEpoch;
  LCommit := StrokeReady and (FPart = nscpThumb) and (GetCapture = FWindow);
  CancelStroke;
  { Releasing capture can run application code, including replacing the source
    or disabling and re-enabling its mount. Never finish against a successor. }

  if not Ready or (FLifetime.InputEpoch <> LInputEpoch) or
    (FViewportEpoch <> LSourceEpoch) then
  begin
    Exit;
  end;

  if LCommit then
  begin
    Seek(LAxis, LPosition, True);
  end;

  if Ready and (FLifetime.InputEpoch = LInputEpoch) and
    (FViewportEpoch = LSourceEpoch) then
  begin
    Scroll(LAxis, nscpNone);
  end;
end;

function TScrollChrome.Handle(AWindow: HWND; AMessage: UINT;
  AWParam: WPARAM; ALParam: LPARAM): LRESULT;
var
  LOuter: Windows.TRect;
  LClient: Windows.PRect;
  LPoint: Windows.TPoint;
  LAxis: TNyxScrollAxis;
  LPart: TNyxScrollChromePart;
  LAlong: Integer;
  LPrintDC: HDC;
  LSavedDC: Integer;
begin

  if AMessage = WM_NCDESTROY then
  begin
    Detach(True);
    Exit(DefSubclassProc(AWindow, AMessage, AWParam, ALParam));
  end;

  if not Current then
  begin
    Exit(DefSubclassProc(AWindow, AMessage, AWParam, ALParam));
  end;

  if AMessage = GRestoreMessage then
  begin

    if (AWParam = FInstance) and FRestoreQueued and
      (ALParam = FRestoreEpoch) and (FViewportEpoch = FRestoreEpoch) then
    begin
      FRestoreQueued := False;
      ApplyViewport;
    end;
    Exit(0);
  end;

  if not FSeeking and ((AMessage = WM_SETTEXT) or (AMessage = EM_REPLACESEL) or
    (AMessage = WM_CHAR) or (AMessage = WM_PASTE) or
    (AMessage = WM_CUT) or (AMessage = WM_CLEAR) or (AMessage = WM_UNDO) or
    ((AMessage = WM_ENABLE) and (AWParam = 0)) or
    (FLifetime.InputReady and ((AMessage = WM_KEYDOWN) or
      (AMessage = WM_VSCROLL) or (AMessage = WM_HSCROLL) or
      (AMessage = WM_MOUSEWHEEL) or (AMessage = EM_LINESCROLL)))) then
  begin
    InvalidateViewport;
  end;

  if AMessage = WM_NCCALCSIZE then
  begin
    LClient := Windows.PRect(ALParam);
    LOuter := LClient^;
    Result := DefSubclassProc(AWindow, AMessage, AWParam, ALParam);

    if Current then
    begin
      FInsets.Left := LClient^.Left - LOuter.Left;
      FInsets.Top := LClient^.Top - LOuter.Top;
      FInsets.Right := LOuter.Right - LClient^.Right;
      FInsets.Bottom := LOuter.Bottom - LClient^.Bottom;
      FHaveInsets := True;
      LClient^.Left := Min(LOuter.Right, LOuter.Left + Pixel(FInsets.Left, FScale));
      LClient^.Top := Min(LOuter.Bottom, LOuter.Top + Pixel(FInsets.Top, FScale));
      LClient^.Right := Max(LClient^.Left, LOuter.Right - Pixel(FInsets.Right, FScale));
      LClient^.Bottom := Max(LClient^.Top, LOuter.Bottom - Pixel(FInsets.Bottom, FScale));
    end;
    Exit;
  end;

  if AMessage = WM_NCPAINT then
  begin
    Paint;
    Exit(0);
  end;

  if AMessage = WM_PRINT then
  begin

    if (AWParam = 0) or (((ALParam and PRF_CHECKVISIBLE) <> 0) and
      not IsWindowVisible(AWindow)) then
    begin
      Exit(0);
    end;
    { Borrow the caller's window-origin DC for this synchronous message only.
      Stock nonclient printing uses system scroll metrics and can overpaint the
      expanded client. Preserve ordinary client/child/owned drawing, then draw
      our nonclient face through the same painter as the live window. Native
      control printing may also alter the supplied clip/origin: restore it before
      painting chrome, including on retirement or an exception in client code. }
    LPrintDC := HDC(AWParam);
    LSavedDC := SaveDC(LPrintDC);

    if LSavedDC = 0 then
    begin
      raise ENyxScrollChrome.Create('Unable to preserve the native print context');
    end;
    try
      GetWindowRect(AWindow, LOuter);
      LPoint.X := 0;
      LPoint.Y := 0;
      Windows.ClientToScreen(AWindow, LPoint);
      { Without PRF_NONCLIENT the native control receives a client-origin DC.
        Keep the public WM_PRINT window origin by applying its measured inset,
        rather than assuming the stock border/scroll metric or a fixed offset. }
      OffsetViewportOrgEx(LPrintDC, LPoint.X - LOuter.Left,
        LPoint.Y - LOuter.Top, nil);
      Result := DefSubclassProc(AWindow, AMessage, AWParam,
        ALParam and not LPARAM(PRF_NONCLIENT));
    finally
      RestoreDC(LPrintDC, LSavedDC);
    end;

    if Current and ((ALParam and PRF_NONCLIENT) <> 0) then
    begin
      Paint(LPrintDC);
    end;
    Exit;
  end;

  if (AMessage = WM_NCMOUSEMOVE) and (AWParam in [HTVSCROLL, HTHSCROLL]) then
  begin
    { The stock hover procedure uses unscaled system metrics and may paint over
      the enlarged client area. This attachment owns its bar's nonclient face. }
    Paint;
    Exit(0);
  end;

  if AMessage = WM_NCHITTEST then
  begin
    GetWindowRect(AWindow, LOuter);
    LPoint.X := SmallInt(LOWORD(ALParam)) - LOuter.Left;
    LPoint.Y := SmallInt(HIWORD(ALParam)) - LOuter.Top;

    if Pick(LPoint.X, LPoint.Y, LAxis, LPart, LAlong) then
    begin

      if LAxis = nsaVertical then
      begin
        Exit(HTVSCROLL);
      end;
      Exit(HTHSCROLL);
    end;
    Result := DefSubclassProc(AWindow, AMessage, AWParam, ALParam);

    if Result in [HTVSCROLL, HTHSCROLL] then
    begin
      Result := HTCLIENT;
    end;
    Exit;
  end;

  if (AMessage = WM_NCLBUTTONDOWN) or (AMessage = WM_NCLBUTTONDBLCLK) then
  begin
    GetWindowRect(AWindow, LOuter);
    LPoint.X := SmallInt(LOWORD(ALParam)) - LOuter.Left;
    LPoint.Y := SmallInt(HIWORD(ALParam)) - LOuter.Top;

    if Pick(LPoint.X, LPoint.Y, LAxis, LPart, LAlong) then
    begin
      Press(LPoint.X, LPoint.Y);
      Exit(0);
    end;
  end;

  if FDown and ((AMessage = WM_MOUSEMOVE) or (AMessage = WM_LBUTTONUP)) then
  begin
    LPoint.X := SmallInt(LOWORD(ALParam));
    LPoint.Y := SmallInt(HIWORD(ALParam));
    Windows.ClientToScreen(AWindow, LPoint);
    GetWindowRect(AWindow, LOuter);

    if AMessage = WM_MOUSEMOVE then
    begin
      Move(LPoint.X - LOuter.Left, LPoint.Y - LOuter.Top);
    end
    else
    begin
      ReleaseStroke;
    end;
    Exit(0);
  end;

  if (AMessage = WM_TIMER) and (FTimer <> 0) and (AWParam = FTimer) then
  begin
    RepeatStroke;
    Exit(0);
  end;

  if (AMessage = WM_CANCELMODE) or (AMessage = WM_CAPTURECHANGED) or
    (AMessage = WM_SETTEXT) or (AMessage = WM_CHAR) or
    (AMessage = EM_REPLACESEL) or (AMessage = WM_PASTE) or
    (AMessage = WM_CUT) or (AMessage = WM_CLEAR) or (AMessage = WM_UNDO) or
    (AMessage = WM_KEYDOWN) or (AMessage = WM_MOUSEWHEEL) or
    ((AMessage = WM_ENABLE) and (AWParam = 0)) or
    ((AMessage = WM_SHOWWINDOW) and (AWParam = 0)) then
  begin
    CancelStroke;

    if not Current then
    begin
      Exit(0);
    end;
  end;
  Result := DefSubclassProc(AWindow, AMessage, AWParam, ALParam);

  if Current and ((AMessage = WM_PAINT) or (AMessage = WM_VSCROLL) or
    (AMessage = WM_HSCROLL) or (AMessage = WM_MOUSEWHEEL) or
    (AMessage = WM_KEYDOWN) or (AMessage = WM_CHAR) or
    (AMessage = WM_SETTEXT) or (AMessage = WM_SIZE)) then
  begin
    Paint;
  end;
end;

procedure TScrollChrome.ProjectPresentation(AScale: Double; ATheme: TNyxTheme;
  const APresentation: TNyxScrollPresentation);
begin
  { Both axes must meet the physical floor. System metrics belong to the
    original host; the stored frame insets may include borders as well. }
  Project(APresentation.EffectiveScale(AScale,
    Min(GetSystemMetrics(SM_CXVSCROLL), GetSystemMetrics(SM_CYHSCROLL))), ATheme);
end;

procedure TScrollChrome.Project(AScale: Double; ATheme: TNyxTheme);
var
  LLease: INyxLCLScrollChrome;
  LRGB: Integer;

  function Color(const AValue: TNyxText): TColor;
  begin
    LRGB := NyxThemeRGB(AValue);
    Result := RGB((LRGB shr 16) and 255, (LRGB shr 8) and 255, LRGB and 255);
  end;

begin
  LLease := Self;

  if not Current then
  begin
    Exit;
  end;
  LayoutNyxScrollChrome(1, AScale, NyxScrollRange(0, 0, 0, 1), FMetrics);
  ATheme.Validate;
  FBackground := Color(ATheme.Background);
  FBorder := Color(ATheme.Border);
  FThumb := Color(ATheme.Muted);
  FForeground := Color(ATheme.Text);
  { A frame-only SetWindowPos emits native WM_SIZE. If LCL still has pending
    projected bounds, that message would import the older HWND size back into
    the live control (observed on a compact code block after Fit). Realize through
    LCL's own widgetset path first; never duplicate its parent/scroll conversion. }
  TScrollControlAccess(FControl).RealizeBounds;

  if not Current then
  begin
    Exit;
  end;

  if not FHaveInsets then
  begin
    SetWindowPos(FWindow, 0, 0, 0, 0, 0,
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER or SWP_NOACTIVATE or SWP_FRAMECHANGED);

    if not Current then
    begin
      Exit;
    end;
  end;
  Pixel(FInsets.Left, AScale);
  Pixel(FInsets.Top, AScale);
  Pixel(FInsets.Right, AScale);
  Pixel(FInsets.Bottom, AScale);

  if (FScale = AScale) and FHaveInsets then
  begin
    Paint;
    Exit;
  end;
  CancelStroke;
  FScale := AScale;
  SetWindowPos(FWindow, 0, 0, 0, 0, 0,
    SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER or SWP_NOACTIVATE or SWP_FRAMECHANGED);

  if Current then
  begin
    Paint;
  end;
end;
{$endif}

function NyxLCLSupportsScrollChrome(AControl: TWinControl): Boolean;
begin
  Result := False;
  {$ifdef MSWINDOWS}
  Result := (WidgetSet <> nil) and (WidgetSet.LCLPlatform = lpWin32) and
    ((AControl is TCustomMemo) or (AControl is TScrollingWinControl) or
      (AControl is TCustomGrid) or (AControl is TCustomTreeView));
  {$endif}
end;

function AttachNyxLCLScrollChrome(AControl: TWinControl;
  const ALifetime: INyxLCLScrollChromeLifetime): INyxLCLScrollChrome;
{$ifdef MSWINDOWS}
var
  LOwner: TScrollChrome;
{$endif}
begin
  Result := nil;
  {$ifdef MSWINDOWS}

  if (AControl = nil) or not AControl.HandleAllocated or (ALifetime = nil) or
    not ALifetime.Current then
  begin
    Exit;
  end;

  if not NyxLCLSupportsScrollChrome(AControl) then
  begin
    Exit;
  end;

  if (GetCurrentThreadId <> MainThreadID) or
    (GetWindowThreadProcessId(AControl.Handle, nil) <> GetCurrentThreadId) then
  begin
    raise ENyxScrollChrome.Create('Scroll chrome attaches only on the window thread');
  end;

  if ALifetime.Frame = nil then
  begin
    raise ENyxScrollChrome.Create('Scroll chrome requires its original physical retirement frame');
  end;
  LOwner := TScrollChrome.Create(AControl, ALifetime);
  Result := LOwner;

  if not LOwner.Install then
  begin
    Result := nil;
  end;
  {$endif}
end;

end.
