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
unit nyx.layout.panel.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses Classes, Controls, ExtCtrls, LMessages, nyx.gestures.lcl;

type
  { Exact native mount authority, independent of widgets. Getters read flags
    and return the existing physical frame without invoking application code.
    A retired mount stays revoked even if a successor reuses design identities. }
  INyxLCLLayoutLifetime = interface(IInterface)
    ['{54602F52-D86E-46A4-8EC8-88981E5812E7}']
    function Current: Boolean;
    function GetFrame: INyxNativeGestureFrame;
  end;
  { Ordinary zero-border layout host. Eligible Win32 sibling panels publish
    their already calculated geometry together, before completing the normal
    LCL realization walk. No LCL cache is patched and no write survives return.
    Unknown/custom/unsupported configurations retain ordinary LCL realization.
    The mount lease retains no renderer; the physical frame owns retirement. }
  TNyxLCLLayoutPanel = class(TPanel)
  private
    FLifetime: INyxLCLLayoutLifetime;
    FWindowIdentity: IInterface;
    FGroupAttempted: Boolean;
    {$ifdef MSWINDOWS}
    FGroupedWindow: PtrUInt;
    FGroupedParent: PtrUInt;
    FGroupedIdentity: IInterface;
    function ReadyForGroup: Boolean;
    procedure PublishSiblingGroup;
    {$endif}
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure RealizeBounds; override;
    procedure WndProc(var AMessage: TLMessage); override;
    {$ifdef NYX_STUDIO_PROFILE}
    procedure DoSendBoundsToInterface; override;
    {$endif}
  public
    { Bind once before parenting can create a native window. Standalone panels
      without a mount lease keep ordinary LCL behavior. }
    procedure ConnectLayout(const ALifetime: INyxLCLLayoutLifetime);
  end;

implementation

uses SysUtils, Types, LCLPlatformDef
  {$ifdef MSWINDOWS}, Windows, Win32WSControls{$endif};

{$ifdef MSWINDOWS}
type
  { A copied native command and retained incarnation. LCL references are held
    explicitly during publication; tokens own no window, control or document. }
  TPanelChange = record
    Panel: TNyxLCLLayoutPanel;
    Identity: IInterface;
    Window: HWND;
    Bounds: TRect;
    Changed: Boolean;
  end;
  { Bounded local command storage, never a queued or reusable admission proof. }
  TPanelChanges = array of TPanelChange;

const
  { One temporary sibling cohort; larger/heterogeneous trees keep normal LCL. }
  MaximumSiblingPanels = 1024;
  { Per-HWND incarnation receipt. Retained tokens prevent pointer ABA; the
    window manager removes its borrowed property when a raw HWND is destroyed. }
  GeometryWindowProperty: PWideChar = 'nyx.layout.window.54602f52';

threadvar
  { Native callbacks may reenter layout. Nested work uses the ordinary path. }
  GPublishingGeometry: Boolean;

function TNyxLCLLayoutPanel.ReadyForGroup: Boolean;
begin
  Result := (ClassType = TNyxLCLLayoutPanel) and (FLifetime <> nil) and
    FLifetime.Current and HandleAllocated and (FWindowIdentity <> nil) and
    ([csLoading, csDestroying] * ComponentState = []) and Visible and
    not AutoSize and not DockSite and (BorderWidth = 0) and
    (BevelInner = bvNone) and (BevelOuter = bvNone) and
    not Assigned(OnResize) and not Assigned(OnChangeBounds) and
    not Assigned(OnConstrainedResize) and
    (Left >= Low(SmallInt)) and (Left <= High(SmallInt)) and
    (Top >= Low(SmallInt)) and (Top <= High(SmallInt)) and
    (Width >= 0) and (Width <= High(SmallInt)) and
    (Height >= 0) and (Height <= High(SmallInt));

  if Result then
  begin
    Result := IsWindowVisible(Handle) and
      (GetWindowThreadProcessId(Handle, nil) = GetCurrentThreadId) and
      (GetPropW(Handle, GeometryWindowProperty) = Windows.HANDLE(Pointer(FWindowIdentity))) and
      ((GetWindowLong(Handle, GWL_EXSTYLE) and WS_EX_LAYOUTRTL) = 0);
  end;
end;

procedure TNyxLCLLayoutPanel.PublishSiblingGroup;
var
  LParent: TNyxLCLLayoutPanel;
  LPanel: TNyxLCLLayoutPanel;
  LChanges: TPanelChanges;
  LFrame: INyxNativeGestureFrame;
  LChildFrame: INyxNativeGestureFrame;
  LParentIdentity: IInterface;
  LParentWindow: HWND;
  LNative: TRect;
  LCount: Integer;
  LHeld: Integer;
  LIndex: Integer;
  LBatch: HDWP;
  {$ifdef NYX_STUDIO_PROFILE}LStarted: QWord;{$endif}
begin

  if GPublishingGeometry or LockWindowPosChanging or not ReadyForGroup or
    (Parent = nil) or (Parent.ClassType <> TNyxLCLLayoutPanel) then
  begin
    Exit;
  end;
  LParent := TNyxLCLLayoutPanel(Parent);

  if LParent.FGroupAttempted or not LParent.ReadyForGroup or
    (LParent.ControlCount < 2) or (LParent.ControlCount > MaximumSiblingPanels) then
  begin
    Exit;
  end;
  { Reset at the parent's postorder RealizeBounds, including refused attempts.
    This avoids repeatedly probing an incompatible cohort for every sibling. }
  LParent.FGroupAttempted := True;
  LParent.IncLCLRefCount;
  try
    LFrame := LParent.FLifetime.GetFrame;

    if LFrame = nil then
    begin
      Exit;
    end;
    LFrame.Enter;
    try
      LParentIdentity := LParent.FWindowIdentity;
      LParentWindow := LParent.Handle;
      SetLength(LChanges, LParent.ControlCount);
      LCount := 0;
      LHeld := 0;
      try
        for LIndex := 0 to LParent.ControlCount - 1 do
        begin

          if LParent.Controls[LIndex].ClassType <> TNyxLCLLayoutPanel then
          begin
            Exit;
          end;
          LPanel := TNyxLCLLayoutPanel(LParent.Controls[LIndex]);

          if not LPanel.ReadyForGroup or (Windows.GetParent(LPanel.Handle) <> LParentWindow) then
          begin
            Exit;
          end;
          LChildFrame := LPanel.FLifetime.GetFrame;

          if LChildFrame <> LFrame then
          begin
            Exit;
          end;
          LPanel.IncLCLRefCount;
          LChanges[LHeld].Panel := LPanel;
          Inc(LHeld);
          LChanges[LHeld - 1].Identity := LPanel.FWindowIdentity;
          LChanges[LHeld - 1].Window := LPanel.Handle;
          LChanges[LHeld - 1].Bounds := LPanel.BoundsRect;
          LNative := Types.Rect(0, 0, 0, 0);

          if not GetWindowRect(LPanel.Handle, LNative) then
          begin
            Exit;
          end;
          { Exact zero-border, non-scrolling, LTR parents use their native client
            origin directly. Unknown frame/RTL contracts were refused above. }
          MapWindowPoints(0, LParentWindow, LNative, 2);

          LChanges[LHeld - 1].Changed := not Types.EqualRect(LNative, LChanges[LHeld - 1].Bounds);

          if LChanges[LHeld - 1].Changed then
          begin
            Inc(LCount);
          end;
        end;

        if (LCount < 2) or not LParent.FLifetime.Current or
          (LParent.FWindowIdentity <> LParentIdentity) then
        begin
          Exit;
        end;
        for LIndex := 0 to LHeld - 1 do
        begin
          LPanel := LChanges[LIndex].Panel;

          if not LPanel.ReadyForGroup or
            (LPanel.FWindowIdentity <> LChanges[LIndex].Identity) or
            (LPanel.Handle <> LChanges[LIndex].Window) or (LPanel.Parent <> LParent) or
            not Types.EqualRect(LPanel.BoundsRect, LChanges[LIndex].Bounds) then
          begin
            Exit;
          end;
        end;
        { A native batch may call user code before delivering later commands.
          Each original window rechecks its mount/incarnation and current LCL
          allocation in WndProc, so a reentrant edit cannot be overwritten by
          this earlier plan. These receipts exist only across the native call. }
        for LIndex := 0 to LHeld - 1 do
        begin
          LPanel := LChanges[LIndex].Panel;
          LPanel.FGroupedWindow := LChanges[LIndex].Window;
          LPanel.FGroupedParent := LParentWindow;
          LPanel.FGroupedIdentity := LChanges[LIndex].Identity;
        end;
        { All storage and source reads precede native admission. DeferWindowPos
          builds only its command structure; EndDeferWindowPos delivers native
          callbacks. The physical frame keeps retired controls alive through it.
          No copied command is replayed after that irreversible platform call.
          https://learn.microsoft.com/windows/win32/api/winuser/nf-winuser-deferwindowpos }
        LBatch := BeginDeferWindowPos(LCount);

        if LBatch = 0 then
        begin
          Exit;
        end;
        for LIndex := 0 to LHeld - 1 do
        begin

          if not LChanges[LIndex].Changed then
          begin
            Continue;
          end;
          LBatch := DeferWindowPos(LBatch, LChanges[LIndex].Window, 0,
            LChanges[LIndex].Bounds.Left, LChanges[LIndex].Bounds.Top,
            LChanges[LIndex].Bounds.Right - LChanges[LIndex].Bounds.Left,
            LChanges[LIndex].Bounds.Bottom - LChanges[LIndex].Bounds.Top,
            SWP_NOACTIVATE or SWP_NOZORDER);

          if LBatch = 0 then
          begin
            { The platform discards a failed construction; do not call End.
              Ordinary LCL realization remains responsible for every window. }
            Exit;
          end;
        end;
        GPublishingGeometry := True;
        {$ifdef NYX_STUDIO_PROFILE}LStarted := GetTickCount64;{$endif}
        try
          EndDeferWindowPos(LBatch);
        finally
          GPublishingGeometry := False;
        end;
        {$ifdef NYX_STUDIO_PROFILE}
        WriteLn('native-layout-group,windows,', LCount, ',', GetTickCount64 - LStarted);
        {$endif}
      finally
        for LIndex := 0 to LHeld - 1 do
        begin
          LChanges[LIndex].Panel.FGroupedWindow := 0;
          LChanges[LIndex].Panel.FGroupedParent := 0;
          LChanges[LIndex].Panel.FGroupedIdentity := nil;
          LChanges[LIndex].Identity := nil;
          LChanges[LIndex].Panel.DecLCLRefCount;
        end;
      end;
    finally
      LFrame.Leave;
    end;
  finally
    LParent.DecLCLRefCount;
  end;
end;
{$endif}

procedure TNyxLCLLayoutPanel.WndProc(var AMessage: TLMessage);
{$ifdef MSWINDOWS}
var
  LPosition: LMessages.PWindowPos;
{$endif}
begin
  inherited WndProc(AMessage);
  {$ifdef MSWINDOWS}

  if GPublishingGeometry and not LockWindowPosChanging and
    (FGroupedWindow <> 0) and (AMessage.Msg = LM_WINDOWPOSCHANGING) then
  begin
    LPosition := TLMWindowPosChanging(AMessage).WindowPos;

    if (LPosition <> nil) and (PtrUInt(LPosition^.hwnd) = FGroupedWindow) then
    begin

      if (csDestroying in ComponentState) or (FLifetime = nil) or
        not FLifetime.Current or (FWindowIdentity <> FGroupedIdentity) or
        not HandleAllocated or (PtrUInt(Handle) <> FGroupedWindow) or
        (PtrUInt(Windows.GetParent(LPosition^.hwnd)) <> FGroupedParent) or
        (GetPropW(LPosition^.hwnd, GeometryWindowProperty) <>
          Windows.HANDLE(Pointer(FGroupedIdentity))) then
      begin
        LPosition^.flags := LPosition^.flags or SWP_NOMOVE or SWP_NOSIZE;
      end
      else
      begin
        { Read today's intended rectangle, after preceding native callbacks.
          LCL keeps its normal anticipated client-area contract; it receives
          the resulting native messages and performs its own final realization. }
        LPosition^.x := Left;
        LPosition^.y := Top;
        LPosition^.cx := Width;
        LPosition^.cy := Height;
      end;
    end;
  end;
  {$endif}
end;

procedure TNyxLCLLayoutPanel.ConnectLayout(const ALifetime: INyxLCLLayoutLifetime);
begin

  if HandleAllocated then
  begin
    raise EInvalidOp.Create('Connect layout lifetime before native window creation');
  end;
  FLifetime := ALifetime;
end;

procedure TNyxLCLLayoutPanel.CreateWnd;
begin
  inherited CreateWnd;
  FGroupAttempted := False;
  {$ifdef MSWINDOWS}

  if GetBuildLCLWidgetType = lpWin32 then
  begin
    FWindowIdentity := TInterfacedObject.Create;

    if not SetPropW(Handle, GeometryWindowProperty, Windows.HANDLE(Pointer(FWindowIdentity))) then
    begin
      FWindowIdentity := nil;
    end;
  end;
  {$endif}
end;

procedure TNyxLCLLayoutPanel.DestroyWnd;
begin
  {$ifdef MSWINDOWS}

  if HandleAllocated and (FWindowIdentity <> nil) and
    (GetPropW(Handle, GeometryWindowProperty) = Windows.HANDLE(Pointer(FWindowIdentity))) then
  begin
    RemovePropW(Handle, GeometryWindowProperty);
  end;
  {$endif}
  FWindowIdentity := nil;
  FGroupAttempted := False;
  inherited DestroyWnd;
end;

procedure TNyxLCLLayoutPanel.RealizeBounds;
{$ifdef MSWINDOWS}
var
  LIdentity: IInterface;
{$endif}
begin
  FGroupAttempted := False;
  {$ifdef MSWINDOWS}

  if GetBuildLCLWidgetType = lpWin32 then
  begin
    IncLCLRefCount;
    try
      LIdentity := FWindowIdentity;

      if (FLifetime <> nil) and not FLifetime.Current then
      begin
        Exit;
      end;

      if (LIdentity <> nil) and (not HandleAllocated or
        (GetPropW(Handle, GeometryWindowProperty) <> Windows.HANDLE(Pointer(LIdentity)))) then
      begin
        Exit;
      end;
      PublishSiblingGroup;

      if (csDestroying in ComponentState) or (FWindowIdentity <> LIdentity) or
        ((FLifetime <> nil) and not FLifetime.Current) then
      begin
        Exit;
      end;

      if (LIdentity <> nil) and (not HandleAllocated or
        (GetPropW(Handle, GeometryWindowProperty) <> Windows.HANDLE(Pointer(LIdentity)))) then
      begin
        Exit;
      end;
      { Let LCL observe the resulting windows and complete its normal caches,
        callbacks and any remaining publication, including partial native failure. }
      inherited RealizeBounds;
    finally
      DecLCLRefCount;
    end;
    Exit;
  end;
  {$endif}
  inherited RealizeBounds;
end;

{$ifdef NYX_STUDIO_PROFILE}
procedure TNyxLCLLayoutPanel.DoSendBoundsToInterface;
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  inherited DoSendBoundsToInterface;
  WriteLn('native-layout-panel,bounds,', GetTickCount64 - LStarted);
end;
{$endif}

end.
