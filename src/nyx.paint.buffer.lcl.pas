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
unit nyx.paint.buffer.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses Controls, ExtCtrls;

type
  { UI-thread value snapshot, never a native-handle lease. Dimensions describe
    the last allocated visible/update rectangle, not the whole virtual control.
    Counters saturate rather than introducing long-running paint overflows. }
  TNyxLCLPaintBufferStats = record
    Attached: Boolean;
    BufferedPaints: Integer;
    DirectFallbacks: Integer;
    FailedCommits: Integer;
    Width: Integer;
    Height: Integer;
  end;
  { Revocable native-window scope. It borrows its custom control and must be
    retired before that owner tears down/recreates the handle. Native destruction
    also revokes it. The installed callback retains a private lease; releasing a
    caller reference alone never leaves an unowned platform callback. UI only. }
  INyxLCLPaintBuffer = interface(IInterface)
    ['{B1B99571-0B9E-47A4-B349-4FD54F87594D}']
    { Idempotently revoke control access and remove only this subclass entry. }
    procedure Retire;
    { Copy current statistics without retaining a control, DC or bitmap. }
    function Stats: TNyxLCLPaintBufferStats;
  end;
  { Ordinary LCL panel with a revocable per-HWND viewport buffer. It preserves
    normal layout, child controls, color, input and DoubleBuffered inheritance;
    unsupported native paths keep the original LCL painter. Nested native window
    creation retires its existing field-owned attachment before replacement;
    external duplicate ownership remains invalid. It owns no model. }
  TNyxLCLBufferedPanel = class(TPanel)
  private
    FPaintBuffer: INyxLCLPaintBuffer;
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
  public
    destructor Destroy; override;
    { Copy diagnostics; absent or unsupported native handles return zeros. }
    function PaintBufferStats: TNyxLCLPaintBufferStats;
  end;

{ Attach to an already-created custom-control window on its owning UI thread.
  Nil/missing handles, duplicate ownership and a wrong thread refuse. Unsupported
  widgetsets/API capabilities return nil and retain the ordinary implementation.
  Only LCL custom-painted controls/scrolling hosts are admitted; native system
  widgets stay outside this adapter. Windows honors DoubleBuffered; supplied-DC/
  printing, nonclient borders, tab pages, RTL and nested paints keep the normal path.
  Buffer allocation failure falls back to the existing supplied-DC paint path. }
function AttachNyxLCLPaintBuffer(AControl: TWinControl): INyxLCLPaintBuffer;

implementation

uses SysUtils, Classes, Forms, LMessages, LCLPlatformDef
  {$ifdef MSWINDOWS}, Windows, CommCtrl, Win32Proc, Themes{$endif};

{$ifdef MSWINDOWS}
type
  { Access to protected LCL presentation flags only. This cast neither constructs
    nor owns a control and is confined to a live, explicitly borrowed base. }
  TNativeControlAccess = class(TWinControl);
  { Optional Comctl32 entry point; the module is borrowed from the LCL host.
    Resolve by name before admission instead of assuming DLL name exports. }
  TSetNativeSubclass = function(AWindow: HWND; AProcedure: SUBCLASSPROC;
    AID: UINT_PTR; AReference: DWORD_PTR): BOOL; stdcall;
  { Removes only the matching callback/ID on its window thread. No ownership
    transfer or Pascal reference counting is provided by this native function. }
  TRemoveNativeSubclass = function(AWindow: HWND; AProcedure: SUBCLASSPROC;
    AID: UINT_PTR): BOOL; stdcall;
  { Forward through Comctl32's current subclass chain, preserving other clients.
    This borrowed function is valid only within an admitted native callback. }
  TNextNativeSubclass = function(AWindow: HWND; AMessage: UINT; AWParam: WPARAM;
    ALParam: LPARAM): LRESULT; stdcall;
  { Comctl32 SetWindowSubclass bridge for a single Win32/LCL custom HWND.
    The callback chain owns a private interface lease, each callback adds a
    temporary lease, and Retire revokes the borrowed control before detaching.
    Per-paint DC/bitmap handles are local and never survive publication/retirement.
    Other widgetsets, supplied-DC painting and caller-disabled buffering bypass
    this bridge. No window procedure, HWND user data or other subclass is replaced. }
  TViewportBuffer = class(TInterfacedObject, INyxLCLPaintBuffer)
  private
    FControl: TWinControl;
    FWindow: HWND;
    FNativeLease: INyxLCLPaintBuffer;
    FBeginningPaint: Boolean;
    FDepth: Integer;
    FStats: TNyxLCLPaintBufferStats;
    FSetSubclass: TSetNativeSubclass;
    FRemoveSubclass: TRemoveNativeSubclass;
    FNextSubclass: TNextNativeSubclass;
    function Current(AWindow: HWND): Boolean;
    function Eligible: Boolean;
    procedure Detach(AWindowDead: Boolean);
    procedure DrawInto(AWindow: HWND; ADC: HDC);
    function Paint(AWindow: HWND): LRESULT;
  public
    constructor Create(AControl: TWinControl);
    function Install: Boolean;
    function HandleMessage(AWindow: HWND; AMessage: UINT; AWParam: WPARAM;
      ALParam: LPARAM): LRESULT;
    procedure Retire;
    function Stats: TNyxLCLPaintBufferStats;
  end;

const
  { The procedure/ID pair is unique per HWND; unrelated subclasses keep theirs. }
  ViewportSubclassID = 1;
  { A per-HWND ownership receipt, not window user data or a procedure replacement.
    Its opaque borrowed receiver is kept alive by the installed native lease. }
  ViewportProperty: PWideChar = 'nyx.paint.buffer.owner.b1b99571';

procedure CountPaint(var ACounter: Integer);
begin

  if ACounter < High(Integer) then
  begin
    Inc(ACounter);
  end;
end;

{ Windows supplies borrowed HWND/message values and opaque reference data.
  Comctl32 performs no Pascal reference counting, so hold an explicit callback
  lease even if owner retirement occurs in a nested paint/message delivery.
  All subclass operations stay on the HWND's thread. See Microsoft's contracts:
  https://learn.microsoft.com/windows/win32/api/commctrl/nf-commctrl-setwindowsubclass
  https://learn.microsoft.com/windows/win32/api/commctrl/nf-commctrl-defsubclassproc }
function ViewportSubclass(AWindow: HWND; AMessage: UINT; AWParam: WPARAM;
  ALParam: LPARAM; AID: UINT_PTR; AReference: DWORD_PTR): LRESULT; stdcall;
var
  LBridge: TViewportBuffer;
  LLease: INyxLCLPaintBuffer;
begin
  LBridge := TViewportBuffer(Pointer(AReference));
  LLease := LBridge;
  Result := 0;
  try
    Result := LBridge.HandleMessage(AWindow, AMessage, AWParam, ALParam);
  except
    { Match the LCL message boundary: report to its application exception owner;
      no Pascal exception escapes through a Windows callback. Paint's finally
      still balances BeginPaint and releases every temporary native object. }
    Application.HandleException(LBridge);
  end;
end;

constructor TViewportBuffer.Create(AControl: TWinControl);
begin
  inherited Create;
  FControl := AControl;
  FWindow := AControl.Handle;
end;

function TViewportBuffer.Install: Boolean;
var
  LModule: HMODULE;
begin
  Result := False;
  LModule := GetModuleHandleW('comctl32.dll');

  if LModule = 0 then
  begin
    Exit;
  end;
  FSetSubclass := TSetNativeSubclass(GetProcAddress(LModule, 'SetWindowSubclass'));
  FRemoveSubclass := TRemoveNativeSubclass(GetProcAddress(LModule, 'RemoveWindowSubclass'));
  FNextSubclass := TNextNativeSubclass(GetProcAddress(LModule, 'DefSubclassProc'));

  if not Assigned(FSetSubclass) or not Assigned(FRemoveSubclass) or
    not Assigned(FNextSubclass) then
  begin
    Exit;
  end;

  if GetPropW(FWindow, ViewportProperty) <> 0 then
  begin
    raise EArgumentException.Create('Native paint buffer already has an owner');
  end;
  FNativeLease := Self;

  if SetPropW(FWindow, ViewportProperty, HANDLE(Self)) then
  begin
    Result := FSetSubclass(FWindow, @ViewportSubclass, ViewportSubclassID, DWORD_PTR(Self));
  end;
  FStats.Attached := Result;

  if not Result then
  begin

    if GetPropW(FWindow, ViewportProperty) = HANDLE(Self) then
    begin
      RemovePropW(FWindow, ViewportProperty);
    end;
    FControl := nil;
    FWindow := 0;
    FNativeLease := nil;
  end;
end;

function TViewportBuffer.Current(AWindow: HWND): Boolean;
begin
  Result := FStats.Attached and (FControl <> nil) and (FWindow = AWindow) and
    (GetPropW(AWindow, ViewportProperty) = HANDLE(Self));
end;

function TViewportBuffer.Eligible: Boolean;
var
  LInfo: PWin32WindowInfo;
begin
  Result := (FControl <> nil) and FStats.Attached and
    TNativeControlAccess(FControl).DoubleBuffered and
    not (csDesigning in FControl.ComponentState) and
    (FControl.Width = FControl.ClientWidth) and (FControl.Height = FControl.ClientHeight) and
    ((GetWindowLong(FWindow, GWL_EXSTYLE) and WS_EX_LAYOUTRTL) = 0);

  if Result then
  begin
    LInfo := GetWin32WindowInfo(FWindow);
    Result := (LInfo <> nil) and (LInfo^.WinControl = FControl) and not LInfo^.isTabPage;
  end;
end;

procedure TViewportBuffer.Detach(AWindowDead: Boolean);
var
  LRemoved: Boolean;
begin
  FControl := nil;
  FStats.Attached := False;

  if FWindow <> 0 then
  begin
    LRemoved := FRemoveSubclass(FWindow, @ViewportSubclass, ViewportSubclassID);

    if not LRemoved and not AWindowDead and IsWindow(FWindow) then
    begin
      { A failed removal must not release Windows' opaque callback receiver.
        Control access is already revoked; WM_NCDESTROY releases this lease. }
      Exit;
    end;

    if GetPropW(FWindow, ViewportProperty) = HANDLE(Self) then
    begin
      RemovePropW(FWindow, ViewportProperty);
    end;
    FWindow := 0;
  end;
  FNativeLease := nil;
end;

procedure TViewportBuffer.Retire;
begin

  if (FWindow <> 0) and IsWindow(FWindow) and
    (GetWindowThreadProcessId(FWindow, nil) <> GetCurrentThreadId) then
  begin
    raise EInvalidOp.Create('Native paint buffer retirement requires the window thread');
  end;
  Detach(False);
end;

function TViewportBuffer.Stats: TNyxLCLPaintBufferStats;
begin
  Result := FStats;
end;

procedure TViewportBuffer.DrawInto(AWindow: HWND; ADC: HDC);
var
  LControl: TWinControl;
  LInfo: PWin32WindowInfo;
  LParentPaint: Boolean;
  LSaved: Integer;
begin

  if not Current(AWindow) then
  begin
    Exit;
  end;
  LControl := FControl;
  { Borrow the public Win32/LCL window metadata only for this decision. A
    nonopaque style alone does not mean the widgetset supplies only a parent
    background. Match its actual theme/parent rule to avoid a duplicate OnPaint.
    No metadata pointer survives the following application callbacks. }
  LInfo := GetWin32WindowInfo(AWindow);
  LParentPaint := (LInfo <> nil) and LInfo^.needParentPaint and
    not (csOpaque in TNativeControlAccess(LControl).ControlStyle);

  if ThemeServices.ThemesEnabled then
  begin
    LParentPaint := LParentPaint or
      ([csParentBackground, csOpaque] * TNativeControlAccess(LControl).ControlStyle =
        [csParentBackground]);
  end;
  LControl.IncLCLRefCount;
  try
    { Win32/LCL's supplied-DC path paints the normal owning background without
      creating its world-sized shared bitmap. When it supplies only a parent
      background, deliver the ordinary LCL paint
      once afterward. This retains its Canvas, OnPaint and WindowProc owners. }
    FNextSubclass(AWindow, WM_PAINT, WPARAM(ADC), 0);

    if LParentPaint and Current(AWindow) then
    begin
      { Match the widgetset's foreground boundary as well as its background.
        A custom OnPaint may change the DC origin/clip; keep that local to its
        paint instead of shifting the later native buffer publication. }
      LSaved := Windows.SaveDC(ADC);
      try
        LControl.Perform(LM_PAINT, WParam(ADC), 0);
      finally

        if LSaved <> 0 then
        begin
          Windows.RestoreDC(ADC, LSaved);
        end;
      end;
    end;
  finally
    LControl.DecLCLRefCount;
  end;
end;

function TViewportBuffer.Paint(AWindow: HWND): LRESULT;
var
  LPaint: TPaintStruct;
  LDC: HDC;
  LMemory: HDC;
  LBitmap: HBITMAP;
  LPrevious: HGDIOBJ;
  LClip: TRect;
  LClient: TRect;
  LWidth: Integer;
  LHeight: Integer;
begin
  Result := 0;
  LPaint := Default(TPaintStruct);
  FBeginningPaint := True;
  try
    LDC := Windows.BeginPaint(AWindow, LPaint);
  finally
    FBeginningPaint := False;
  end;

  if LDC = 0 then
  begin
    Exit(FNextSubclass(AWindow, WM_PAINT, 0, 0));
  end;
  LMemory := 0;
  LBitmap := 0;
  LPrevious := 0;
  Inc(FDepth);
  try

    if not Current(AWindow) then
    begin
      Exit;
    end;

    if (Windows.GetClipBox(LDC, LClip) <> Windows.ERROR) and
      Windows.GetClientRect(AWindow, LClient) then
    begin
      { BeginPaint's display DC already intersects visibility and dirty input.
        The final BitBlt retains that exact complex clip; only its bounding box
        is allocated. No viewport/window transform or caller clip is changed. }
      IntersectRect(LClip, LClip, LClient);
      LWidth := LClip.Right - LClip.Left;
      LHeight := LClip.Bottom - LClip.Top;

      if (LWidth > 0) and (LHeight > 0) then
      begin
        LMemory := CreateCompatibleDC(LDC);

        if LMemory <> 0 then
        begin
          LBitmap := CreateCompatibleBitmap(LDC, LWidth, LHeight);

          if LBitmap <> 0 then
          begin
            LPrevious := SelectObject(LMemory, LBitmap);
          end;
        end;

        { This selection is a bitmap, not a region: Win32 reports failure as
          a null previous object. No region-only HGDI_ERROR value is involved. }

        if (LPrevious <> 0) and
          SetWindowOrgEx(LMemory, LClip.Left, LClip.Top, nil) and
          (Windows.FillRect(LMemory, LClip, GetSysColorBrush(COLOR_BTNFACE)) <> 0) then
        begin
          { Match Win32/LCL's initial cache color when a custom eraser declines
            drawing. Initialize only the owned rectangle; the system brush is
            borrowed and never deleted. No uninitialized pixels can publish. }
          FStats.Width := LWidth;
          FStats.Height := LHeight;
          DrawInto(AWindow, LMemory);

          if Current(AWindow) then
          begin

            if BitBlt(LDC, LClip.Left, LClip.Top, LWidth, LHeight,
              LMemory, LClip.Left, LClip.Top, SRCCOPY) then
            begin
              CountPaint(FStats.BufferedPaints);
            end
            else
            begin
              CountPaint(FStats.FailedCommits);
            end;
          end;
          Exit;
        end;
      end;
    end;
    { Unknown/empty clips and native allocation refusal retain real painting and
      validation. They never discard the accepted frame or invoke an OnPaint
      callback twice merely to retry an already attempted publication. }
    CountPaint(FStats.DirectFallbacks);
    DrawInto(AWindow, LDC);
  finally

    if LPrevious <> 0 then
    begin
      SelectObject(LMemory, LPrevious);
    end;

    if LBitmap <> 0 then
    begin
      DeleteObject(LBitmap);
    end;

    if LMemory <> 0 then
    begin
      DeleteDC(LMemory);
    end;
    Dec(FDepth);
    Windows.EndPaint(AWindow, LPaint);
  end;
end;

function TViewportBuffer.HandleMessage(AWindow: HWND; AMessage: UINT; AWParam: WPARAM;
  ALParam: LPARAM): LRESULT;
var
  LBeginning: Boolean;
begin

  if AMessage = WM_NCDESTROY then
  begin
    Detach(True);
    Exit(FNextSubclass(AWindow, AMessage, AWParam, ALParam));
  end;

  if (AMessage = WM_ERASEBKGND) and FBeginningPaint then
  begin
    { BeginPaint may synchronously request erase. The normal background and
      foreground are composed together into the owned buffer, avoiding a visible
      intermediate erase. Explicit supplied-DC erase/printing stays untouched. }
    Exit(1);
  end;

  if (AMessage = WM_PAINT) and FBeginningPaint then
  begin
    { An independent subclass can reenter during BeginPaint's erase callback.
      Keep that nested request on LCL's normal path, including its own erasure;
      never recursively enter this buffer or consume the outer begin guard. }
    LBeginning := FBeginningPaint;
    FBeginningPaint := False;
    try
      Result := FNextSubclass(AWindow, AMessage, AWParam, ALParam);
    finally
      FBeginningPaint := LBeginning;
    end;
    Exit;
  end;

  if Current(AWindow) and Eligible and (AMessage = WM_PAINT) and
    (AWParam = 0) and (FDepth = 0) then
  begin
    Exit(Paint(AWindow));
  end;
  Result := FNextSubclass(AWindow, AMessage, AWParam, ALParam);
end;
{$endif}

function AttachNyxLCLPaintBuffer(AControl: TWinControl): INyxLCLPaintBuffer;
{$ifdef MSWINDOWS}
var
  LBuffer: TViewportBuffer;
{$endif}
begin
  Result := nil;
  {$ifdef MSWINDOWS}

  if GetBuildLCLWidgetType <> lpWin32 then
  begin
    Exit;
  end;

  if (AControl = nil) or not AControl.HandleAllocated then
  begin
    raise EArgumentException.Create('Native paint buffer requires an existing custom HWND');
  end;

  if not (AControl is TCustomControl) and not (AControl is TScrollingWinControl) then
  begin
    Exit;
  end;

  if GetWindowThreadProcessId(AControl.Handle, nil) <> GetCurrentThreadId then
  begin
    raise EInvalidOp.Create('Native paint buffer attachment requires the window thread');
  end;
  LBuffer := TViewportBuffer.Create(AControl);
  Result := LBuffer;

  if not LBuffer.Install then
  begin
    Result := nil;
  end;
  {$endif}
end;

procedure TNyxLCLBufferedPanel.CreateWnd;
begin
  inherited CreateWnd;
  { Inherited creation can complete a nested CreateWnd before returning. Its
    installed scope is already owned by this field; retire that exact lease
    before replacing it. External duplicate attachments must still refuse. }

  if FPaintBuffer <> nil then
  begin
    FPaintBuffer.Retire;
    FPaintBuffer := nil;
  end;
  FPaintBuffer := AttachNyxLCLPaintBuffer(Self);
end;

procedure TNyxLCLBufferedPanel.DestroyWnd;
begin

  if FPaintBuffer <> nil then
  begin
    FPaintBuffer.Retire;
    FPaintBuffer := nil;
  end;
  inherited DestroyWnd;
end;

destructor TNyxLCLBufferedPanel.Destroy;
begin

  if FPaintBuffer <> nil then
  begin
    FPaintBuffer.Retire;
    FPaintBuffer := nil;
  end;
  inherited Destroy;
end;

function TNyxLCLBufferedPanel.PaintBufferStats: TNyxLCLPaintBufferStats;
begin
  Result := Default(TNyxLCLPaintBufferStats);

  if FPaintBuffer <> nil then
  begin
    Result := FPaintBuffer.Stats;
  end;
end;

end.
