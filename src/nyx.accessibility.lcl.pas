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
unit nyx.accessibility.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses Controls;

type
  { Native publication capability, independent of document admission. Deferred
    means an opted-in control has no HWND yet; Unsupported leaves its widgetset's
    normal provider intact. Unavailable includes a refused platform annotation.
    Published establishes only names/descriptions, not roles/actions or full AT. }
  TNyxLCLAccessibilityStatus = (nlasUnsupported, nlasDeferred, nlasUnavailable,
    nlasPublished);

{ Opt an existing control into native accessible name/description publication.
  UI-thread only; nil refuses. The control owns the private bridge until disposal,
  so no document, renderer, event receiver or caller interface is retained.
  Existing HWNDs update immediately; no handle is created merely for metadata.
  Later MSAA requests refresh recreated handles through UI-thread Windows message
  observation. Empty fields restore the standard provider's defaults. Other widgetsets
  and non-windowed controls keep ordinary behavior. Win32 uses Oleacc dynamic
  annotations, not a replacement provider; the API's zero-terminated strings
  cannot carry embedded NUL. Its typed status reports that limitation/failure. }
function SyncNyxLCLAccessibility(AControl: TControl): TNyxLCLAccessibilityStatus;

implementation

uses Classes, SysUtils, LCLPlatformDef, nyx.text
  {$ifdef MSWINDOWS}, Windows, ActiveX, Forms{$endif};

{$ifdef MSWINDOWS}
type
  { Optional User32 NotifyWinEvent entry point. The LCL host owns that module;
    calls borrow an existing HWND and copy scalar event IDs, retaining nothing. }
  TNativeAccessibilityNotification = procedure(AEvent: DWORD; AWindow: HWND;
    AObject, AChild: LongInt); stdcall;
  { Native seven-method prefix of Oleacc's IAccPropServices. GUID/VARIANT inputs
    are by value, pointer arrays are borrowed for each call, and COM owns the
    manager. Only HWND string set/clear are used; the unexposed trailing identity/
    menu methods are not called. Availability is checked with CoCreateInstance.
    ABI authority: Microsoft's oleacc.h and SetHwndPropStr/ClearHwndProps. }
  INativeAnnotationServices = interface(IUnknown)
    ['{6E26E776-04F0-495D-80E4-3330352E3169}']
    function SetPropValue(AID: PByte; ALength: DWORD; AProperty: TGUID;
      AValue: OleVariant): HRESULT; stdcall;
    function SetPropServer(AID: PByte; ALength: DWORD; AProperties: PGUID;
      ACount: Integer; AServer: IUnknown; AScope: Integer): HRESULT; stdcall;
    function ClearProps(AID: PByte; ALength: DWORD; AProperties: PGUID;
      ACount: Integer): HRESULT; stdcall;
    function SetHwndProp(AWindow: HWND; AObject, AChild: DWORD;
      AProperty: TGUID; AValue: OleVariant): HRESULT; stdcall;
    function SetHwndPropStr(AWindow: HWND; AObject, AChild: DWORD;
      AProperty: TGUID; AText: PWideChar): HRESULT; stdcall;
    function SetHwndPropServer(AWindow: HWND; AObject, AChild: DWORD;
      AProperties: PGUID; ACount: Integer; AServer: IUnknown;
      AScope: Integer): HRESULT; stdcall;
    function ClearHwndProps(AWindow: HWND; AObject, AChild: DWORD;
      AProperties: PGUID; ACount: Integer): HRESULT; stdcall;
  end;
  { One control-owned annotation lifetime. Its borrowed control remains alive
    with the owning component. No COM callback retains
    this object. A native property identifies each annotated HWND so retirement
    never clears a recycled or foreign window. Destroy messages clear before
    forwarding; lazy/recreated windows refresh when MSAA requests their object.
    Its COM initialization is balanced on this same UI thread, without changing
    an already selected apartment. Standard providers retain value/focus/actions. }
  TAccessibleMetadata = class(TComponent)
  private
    FControl: TWinControl;
    FHookLease: Boolean;
    FService: INativeAnnotationServices;
    FNotify: TNativeAccessibilityNotification;
    FCOMLease: Boolean;
    FWindow: HWND;
    FName: TNyxText;
    FDescription: TNyxText;
    FPublishing: Boolean;
    procedure ClearWindow;
    function WriteText(AWindow: HWND; AProperty: TGUID;
      const AText: TNyxText): Boolean;
  public
    { Opt in without forcing a native handle; the control owns this component. }
    constructor CreateFor(AControl: TWinControl);
    { Clear only owned native annotations before releasing this UI COM lease. }
    destructor Destroy; override;
    { Copy both live LCL fields and optionally notify native property listeners. }
    function Publish(ANotify: Boolean): TNyxLCLAccessibilityStatus;
  end;

const
  AnnotationClass: TGUID = '{B5F8350B-0548-48B1-A6EE-88BD00B4A5E7}';
  NameProperty: TGUID = '{608D3DF8-8128-4AA7-A428-F55E49267291}';
  DescriptionProperty: TGUID = '{4D48DFE4-BD3F-491F-A648-492D6F20C588}';
  ClientObject: DWORD = $FFFFFFFC;
  NameChangedEvent: DWORD = $800C;
  DescriptionChangedEvent: DWORD = $800D;
  { This opaque owner receipt neither replaces HWND user data nor owns a window. }
  AnnotationOwner: PWideChar = 'nyx.accessibility.owner.82fd7d87';

var
  { UI-thread-only observation, shared by opted-in controls. A static callback
    observes this process's main thread only, never the desktop or another app.
    Last component retirement removes it; no control/model is retained globally. }
  GMessageHook: HHOOK;
  GMessageHookUsers: Integer;

function MetadataFor(AControl: TControl): TAccessibleMetadata;
var
  LIndex: Integer;
begin
  Result := nil;

  if AControl = nil then
  begin
    Exit;
  end;
  for LIndex := 0 to AControl.ComponentCount - 1 do
  begin

    if AControl.Components[LIndex] is TAccessibleMetadata then
    begin
      Exit(TAccessibleMetadata(AControl.Components[LIndex]));
    end;
  end;
end;

{ LCL does not deliver WM_GETOBJECT to TControl.WindowProc on this Win32 path.
  Observe its real UI-thread message before the existing procedure instead.
  Windows owns CWPSTRUCT for this callback only. All messages continue through
  CallNextHookEx unchanged, including unrelated controls and negative hook codes.
  https://learn.microsoft.com/windows/win32/winmsg/callwndproc }
function AccessibilityMessage(ACode: LongInt; AWParam: WPARAM;
  ALParam: LPARAM): LRESULT; stdcall;
var
  LMessage: PCWPSTRUCT;
  LBridge: TAccessibleMetadata;
begin
  try

    if (ACode = HC_ACTION) and (ALParam <> 0) then
    begin
      LMessage := PCWPSTRUCT(ALParam);

      if LMessage^.message = WM_GETOBJECT then
      begin
        LBridge := MetadataFor(FindControl(LMessage^.hwnd));

        if LBridge <> nil then
        begin
          LBridge.Publish(False);
        end;
      end
      else if LMessage^.message = WM_DESTROY then
      begin
        LBridge := TAccessibleMetadata(Pointer(GetPropW(LMessage^.hwnd, AnnotationOwner)));

        if LBridge <> nil then
        begin
          LBridge.ClearWindow;
        end;
      end;
    end;
  except
    { Native callback boundaries must not carry a Pascal exception. This owns no
      application handler; the existing LCL application reports the failure. }
    Application.HandleException(nil);
  end;
  Result := CallNextHookEx(GMessageHook, ACode, AWParam, ALParam);
end;

constructor TAccessibleMetadata.CreateFor(AControl: TWinControl);
var
  LCode: HRESULT;
begin
  inherited Create(AControl);
  FControl := AControl;
  LCode := CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
  FCOMLease := LCode >= 0;
  { RPC_E_CHANGED_MODE retains the host's existing apartment. CoCreateInstance
    decides capability there; an unavailable service does not prevent designing. }
  CoCreateInstance(AnnotationClass, nil, CLSCTX_INPROC_SERVER,
    INativeAnnotationServices, FService);
  FNotify := TNativeAccessibilityNotification(
    GetProcAddress(GetModuleHandleW('user32.dll'), 'NotifyWinEvent'));

  if FService <> nil then
  begin

    if GMessageHook = 0 then
    begin
      GMessageHook := SetWindowsHookEx(WH_CALLWNDPROC, @AccessibilityMessage, 0, MainThreadID);
    end;
    FHookLease := GMessageHook <> 0;

    if FHookLease then
    begin
      Inc(GMessageHookUsers);
    end;
  end;
end;

destructor TAccessibleMetadata.Destroy;
begin
  ClearWindow;

  if FHookLease then
  begin
    FHookLease := False;
    Dec(GMessageHookUsers);

    if GMessageHookUsers = 0 then
    begin
      if UnhookWindowsHookEx(GMessageHook) then
      begin
        GMessageHook := 0;
      end;
      { A refused detach keeps the static callback/handle recorded. A later
        lease reuses it rather than installing an untracked duplicate. }
    end;
  end;
  FControl := nil;
  FService := nil;

  if FCOMLease then
  begin
    CoUninitialize;
  end;
  inherited Destroy;
end;

procedure TAccessibleMetadata.ClearWindow;
var
  LProperties: array[0..1] of TGUID;
begin

  if (FWindow <> 0) and (GetPropW(FWindow, AnnotationOwner) = HANDLE(Self)) then
  begin
    LProperties[0] := NameProperty;
    LProperties[1] := DescriptionProperty;

    if FService <> nil then
    begin
      FService.ClearHwndProps(FWindow, ClientObject, 0, @LProperties[0], 2);
    end;
    RemovePropW(FWindow, AnnotationOwner);
  end;
  FWindow := 0;
end;

function TAccessibleMetadata.WriteText(AWindow: HWND; AProperty: TGUID;
  const AText: TNyxText): Boolean;
var
  LText: UnicodeString;
begin

  if AText = '' then
  begin
    Result := FService.ClearHwndProps(AWindow, ClientObject, 0, @AProperty, 1) >= 0;
  end
  else
  begin
    LText := UTF8Decode(AText);
    Result := FService.SetHwndPropStr(AWindow, ClientObject, 0, AProperty,
      PWideChar(LText)) >= 0;
  end;
end;

function TAccessibleMetadata.Publish(ANotify: Boolean): TNyxLCLAccessibilityStatus;
var
  LWindow: HWND;
  LName: TNyxText;
  LDescription: TNyxText;
  LNameChanged: Boolean;
  LDescriptionChanged: Boolean;
  LNotify: TNativeAccessibilityNotification;
begin
  Result := nlasUnavailable;

  if (FControl = nil) or (csDestroying in ComponentState) or
    (FService = nil) or not FHookLease or FPublishing then
  begin
    Exit;
  end;

  if not FControl.HandleAllocated then
  begin
    Result := nlasDeferred;
    Exit;
  end;
  LName := FControl.AccessibleName;
  LDescription := FControl.AccessibleDescription;

  if (Pos(#0, LName) > 0) or (Pos(#0, LDescription) > 0) then
  begin
    ClearWindow;
    Exit;
  end;
  LWindow := FControl.Handle;
  LNameChanged := (FWindow <> LWindow) or (FName <> LName) or
    (GetPropW(LWindow, AnnotationOwner) <> HANDLE(Self));
  LDescriptionChanged := (FWindow <> LWindow) or (FDescription <> LDescription) or
    (GetPropW(LWindow, AnnotationOwner) <> HANDLE(Self));

  if not LNameChanged and not LDescriptionChanged then
  begin
    Result := nlasPublished;
    Exit;
  end;

  if (GetPropW(LWindow, AnnotationOwner) <> 0) and
    (GetPropW(LWindow, AnnotationOwner) <> HANDLE(Self)) then
  begin
    Exit;
  end;
  ClearWindow;

  if not SetPropW(LWindow, AnnotationOwner, HANDLE(Self)) then
  begin
    Exit;
  end;
  FWindow := LWindow;
  FPublishing := True;
  try

    if not WriteText(LWindow, NameProperty, LName) or
      not WriteText(LWindow, DescriptionProperty, LDescription) then
    begin
      ClearWindow;
      Exit;
    end;
    FName := LName;
    FDescription := LDescription;
    Result := nlasPublished;
  finally
    FPublishing := False;
  end;
  LNotify := FNotify;
  { Queries refresh silently. Explicit publication notifies only after both
    copied properties are current, avoiding an annotation/query feedback loop.
    Copy the native entry point before notifying: an in-context client may retire
    this control. No instance field is read after the first notification. }
  if ANotify and Assigned(LNotify) and LNameChanged then
  begin
    LNotify(NameChangedEvent, LWindow, LongInt(ClientObject), 0);
  end;

  if ANotify and Assigned(LNotify) and LDescriptionChanged and
    (GetPropW(LWindow, AnnotationOwner) = HANDLE(Self)) then
  begin
    LNotify(DescriptionChangedEvent, LWindow, LongInt(ClientObject), 0);
  end;
end;

{$endif}

function SyncNyxLCLAccessibility(AControl: TControl): TNyxLCLAccessibilityStatus;
{$ifdef MSWINDOWS}
var
  LBridge: TAccessibleMetadata;
{$endif}
begin

  if GetCurrentThreadID <> MainThreadID then
  begin
    raise EInvalidOperation.Create('Native accessibility publication requires the UI thread');
  end;

  if AControl = nil then
  begin
    raise EArgumentNilException.Create('Accessible control is required');
  end;
  Result := nlasUnsupported;
  {$ifdef MSWINDOWS}

  if (GetBuildLCLWidgetType <> lpWin32) or not (AControl is TWinControl) then
  begin
    Exit;
  end;
  LBridge := MetadataFor(AControl);

  if LBridge = nil then
  begin
    LBridge := TAccessibleMetadata.CreateFor(TWinControl(AControl));
  end;
  Result := LBridge.Publish(True);
  {$endif}
end;

end.
