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
unit nyx.accessibility.button.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses Controls;

type
  { Copied native button presentation, independent of document/state ownership.
    These are live physical press/focus flags, not an authored checked value. }
  TNyxLCLAccessibleButtonState = record
    Pressed: Boolean;
    Focused: Boolean;
  end;
  { UI-only snapshot reader borrowed from the exact attached control. Copy state
    without mutation/application callbacks. Retire revokes this method before its
    receiver or native window can be disposed. }
  TNyxLCLAccessibleButtonStateReader = function: TNyxLCLAccessibleButtonState of object;
  { Owns one native HWND attachment, never its control or application. The
    control owner retires this lease before DestroyWnd. Native callbacks retain
    their own short lease; externally held MSAA objects are Windows proxies and
    retain no Pascal control, renderer, event callback or document reference. }
  INyxLCLAccessibleButton = interface(IInterface)
    ['{274856B6-80E8-4C78-AC18-50830278EA86}']
    { UI-only, idempotent revocation. Failed native detach retains only the
      inert callback until WM_NCDESTROY; no later action can reach the control. }
    procedure Retire;
  end;

{ Attach an existing button-like HWND on its owning UI thread. The control must
  implement ExecuteDefaultAction and supply its own live state reader; the reader
  must belong to that same control. No handle is allocated here. Win32 delegates
  role, default action, geometry, navigation and COM automation to Windows' system
  BUTTON proxy. Only the button message protocol and OBJID_CLIENT response are
  adapted. Win32 requires its system Oleacc DLL (Windows 2000+); other widgetsets
  or missing optional subclass/proxy capabilities return nil. This is not
  a general menu/virtual-child/UIA provider or a substitute for their qualification. }
function AttachNyxLCLAccessibleButton(AControl: TWinControl;
  AReadState: TNyxLCLAccessibleButtonStateReader): INyxLCLAccessibleButton;

implementation

uses Classes, SysUtils, LCLPlatformDef
  {$ifdef MSWINDOWS}, Windows, ActiveX, CommCtrl, Forms{$endif};

{$ifdef MSWINDOWS}
type
  { Optional Comctl32 subclass install. The LCL host owns the module; Windows
    borrows this callback/reference data until removal or native destruction. }
  TSetNativeSubclass = function(AWindow: HWND; AProcedure: SUBCLASSPROC;
    AID: UINT_PTR; AReference: DWORD_PTR): BOOL; stdcall;
  { Remove only the exact callback/id pair; unrelated native hooks remain live. }
  TRemoveNativeSubclass = function(AWindow: HWND; AProcedure: SUBCLASSPROC;
    AID: UINT_PTR): BOOL; stdcall;
  { Forward an unhandled native message through the current Comctl32 chain.
    This borrowed entry point is used only inside an admitted window callback. }
  TNextNativeSubclass = function(AWindow: HWND; AMessage: UINT;
    AWParam: WPARAM; ALParam: LPARAM): LRESULT; stdcall;
  { One revocable system-proxy bridge for one native window. Windows holds an
    explicit private interface lease while its opaque callback receiver is
    installed. Each callback adds a temporary lease across reentrant retirement.
    State readers and the borrowed control are cleared before detachment; the
    cached system COM object contains no Pascal receiver or application authority. }
  TButtonAccessibility = class(TInterfacedObject, INyxLCLAccessibleButton)
  private
    FControl: TWinControl;
    FReadState: TNyxLCLAccessibleButtonStateReader;
    FWindow: HWND;
    FProxy: IUnknown;
    FCOMLease: Boolean;
    FCallbackDepth: Integer;
    FNativeLease: INyxLCLAccessibleButton;
    FInstalled: Boolean;
    FSetSubclass: TSetNativeSubclass;
    FRemoveSubclass: TRemoveNativeSubclass;
    FNextSubclass: TNextNativeSubclass;
    function Current(AWindow: HWND): Boolean;
    procedure Detach(AWindowDead: Boolean);
    procedure ReleaseRetiredApartment;
  public
    { Borrow an already allocated native control; no native message is sent. }
    constructor CreateFor(AControl: TWinControl;
      AReadState: TNyxLCLAccessibleButtonStateReader);
    { Release the native proxy before balancing this UI-thread COM lease. }
    destructor Destroy; override;
    { Resolve native capabilities and install exactly one scoped receiver. }
    function Install: Boolean;
    { Revoke the borrowed receiver before any native detach or COM release. }
    procedure Retire;
    { Handle the native button protocol; every unrelated message is forwarded. }
    function HandleMessage(AWindow: HWND; AMessage: UINT;
      AWParam: WPARAM; ALParam: LPARAM): LRESULT;
  end;

const
  AccessibleIID: TGUID = '{618736E0-3C3D-11CF-810C-00AA00389B71}';
  ClientObject: DWORD = $FFFFFFFC;
  ButtonSubclassID = 1;
  ButtonPressedFlag: LRESULT = $0004;
  ButtonFocusFlag: LRESULT = $0008;
  SystemButtonClass: PWideChar = 'BUTTON';
  { A private HWND owner receipt, not window user data or a model reference. }
  ButtonOwner: PWideChar = 'nyx.accessibility.button.274856b6';

{ Windows system accessibility entry points. Importing Oleacc keeps their code
  loaded with this module, including while native proxies outlive a control.
  The qualified target is an ordinary Win32 application; caller module/DLL unload
  still requires normal COM/host lifetime discipline. APIs require Windows 2000+
  and an initialized COM apartment; failures retain the ordinary LCL provider.
  https://learn.microsoft.com/windows/win32/api/oleacc/nf-oleacc-createstdaccessibleproxyw }
function CreateNativeButtonProxy(AWindow: HWND; AClassName: PWideChar;
  AObject: LongInt; const AIID: TGUID; out AProxy: IUnknown): HRESULT;
  stdcall; external 'oleacc.dll' name 'CreateStdAccessibleProxyW';

{ Called afresh for each OBJID_CLIENT request with its original wParam.
  Windows adds a COM reference and returns an opaque marshaling result, which is
  never cached/reused between clients. The borrowed proxy has a local call lease.
  https://learn.microsoft.com/windows/win32/api/oleacc/nf-oleacc-lresultfromobject }
function NativeResultFromObject(const AIID: TGUID; AWParam: WPARAM;
  AProxy: IUnknown): LRESULT;
  stdcall; external 'oleacc.dll' name 'LresultFromObject';

{ Windows borrows the HWND/scalars and opaque receiver for this callback only.
  The local interface keeps that receiver alive if an action retires its window.
  No Pascal exception escapes the native callback boundary. }
function ButtonSubclass(AWindow: HWND; AMessage: UINT; AWParam: WPARAM;
  ALParam: LPARAM; AID: UINT_PTR; AReference: DWORD_PTR): LRESULT; stdcall;
var
  LBridge: TButtonAccessibility;
  LLease: INyxLCLAccessibleButton;
begin
  LBridge := TButtonAccessibility(Pointer(AReference));
  LLease := LBridge;
  Result := 0;
  Inc(LBridge.FCallbackDepth);
  try
    try
      Result := LBridge.HandleMessage(AWindow, AMessage, AWParam, ALParam);
    except
      try
        Application.HandleException(LBridge);
      except
        { A caller's exception reporter can itself fail. That secondary failure
          must not unwind through Comctl32/USER32 or skip native retirement. }
      end;
    end;
  finally
    Dec(LBridge.FCallbackDepth);
    LBridge.ReleaseRetiredApartment;
  end;
end;

constructor TButtonAccessibility.CreateFor(AControl: TWinControl;
  AReadState: TNyxLCLAccessibleButtonStateReader);
begin
  inherited Create;
  FControl := AControl;
  FReadState := AReadState;
  FWindow := AControl.Handle;
end;

destructor TButtonAccessibility.Destroy;
begin
  FProxy := nil;

  if FCOMLease then
  begin
    { Only a failed UI-thread attachment can reach destruction still holding
      this lease. Normal retirement balances it on the UI thread independently
      of where an external caller eventually releases the inert interface. }
    CoUninitialize;
  end;
  inherited Destroy;
end;

function TButtonAccessibility.Install: Boolean;
var
  LModule: HMODULE;
  LCode: HRESULT;
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

  if GetPropW(FWindow, ButtonOwner) <> 0 then
  begin
    raise EArgumentException.Create('Native button accessibility already has an owner');
  end;
  LCode := CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
  FCOMLease := LCode >= 0;
  LCode := CreateNativeButtonProxy(FWindow, SystemButtonClass, LongInt(ClientObject),
    AccessibleIID, FProxy);

  if (LCode < 0) or (FProxy = nil) then
  begin
    Exit;
  end;
  FNativeLease := Self;

  if SetPropW(FWindow, ButtonOwner, HANDLE(Self)) then
  begin
    Result := FSetSubclass(FWindow, @ButtonSubclass, ButtonSubclassID, DWORD_PTR(Self));
  end;
  FInstalled := Result;

  if not Result then
  begin
    Detach(False);
  end;
end;

function TButtonAccessibility.Current(AWindow: HWND): Boolean;
begin
  Result := FInstalled and (FControl <> nil) and (FWindow = AWindow) and
    (GetPropW(AWindow, ButtonOwner) = HANDLE(Self));
end;

procedure TButtonAccessibility.Detach(AWindowDead: Boolean);
var
  LRemoved: Boolean;
begin
  FControl := nil;
  FReadState := nil;
  FProxy := nil;

  if FWindow <> 0 then
  begin
    LRemoved := not FInstalled;

    if FInstalled then
    begin
      LRemoved := FRemoveSubclass(FWindow, @ButtonSubclass, ButtonSubclassID);
    end;

    if not LRemoved and not AWindowDead and IsWindow(FWindow) then
    begin
      { Keep the inert receiver until WM_NCDESTROY if Windows refuses removal. }
      ReleaseRetiredApartment;
      Exit;
    end;

    if GetPropW(FWindow, ButtonOwner) = HANDLE(Self) then
    begin
      RemovePropW(FWindow, ButtonOwner);
    end;
    FWindow := 0;
  end;
  FInstalled := False;
  FNativeLease := nil;
  ReleaseRetiredApartment;
end;

procedure TButtonAccessibility.ReleaseRetiredApartment;
begin

  if FCOMLease and (FControl = nil) and (FCallbackDepth = 0) then
  begin
    FCOMLease := False;
    CoUninitialize;
  end;
end;

procedure TButtonAccessibility.Retire;
var
  LLease: INyxLCLAccessibleButton;
begin

  if GetCurrentThreadID <> MainThreadID then
  begin
    raise EInvalidOperation.Create('Native button accessibility retirement requires the UI thread');
  end;
  LLease := Self;
  Detach(False);
end;

function TButtonAccessibility.HandleMessage(AWindow: HWND; AMessage: UINT;
  AWParam: WPARAM; ALParam: LPARAM): LRESULT;
var
  LProxy: IUnknown;
  LControl: TWinControl;
  LState: TNyxLCLAccessibleButtonState;
begin

  if AMessage = WM_NCDESTROY then
  begin
    Detach(True);
    Exit(FNextSubclass(AWindow, AMessage, AWParam, ALParam));
  end;

  { Painting, layout and unrelated input need no native ownership lookup. }

  if ((AMessage = WM_GETOBJECT) or (AMessage = BM_GETSTATE) or
    (AMessage = BM_CLICK)) and Current(AWindow) then
  begin

    if (AMessage = WM_GETOBJECT) and (DWORD(ALParam) = ClientObject) then
    begin
      LProxy := FProxy;
      Exit(NativeResultFromObject(AccessibleIID, AWParam, LProxy));
    end;

    if AMessage = BM_GETSTATE then
    begin
      LState := FReadState();
      Result := 0;

      if LState.Pressed then
      begin
        Result := Result or ButtonPressedFlag;
      end;

      if LState.Focused then
      begin
        Result := Result or ButtonFocusFlag;
      end;
      Exit;
    end;

    if AMessage = BM_CLICK then
    begin
      LControl := FControl;
      LControl.ExecuteDefaultAction;
      { The action may retire the receiver/window. Read no borrowed fields. }
      Exit(0);
    end;
  end;
  Result := FNextSubclass(AWindow, AMessage, AWParam, ALParam);
end;
{$endif}

function AttachNyxLCLAccessibleButton(AControl: TWinControl;
  AReadState: TNyxLCLAccessibleButtonStateReader): INyxLCLAccessibleButton;
{$ifdef MSWINDOWS}
var
  LBridge: TButtonAccessibility;
{$endif}
begin
  Result := nil;
  {$ifdef MSWINDOWS}

  if GetBuildLCLWidgetType <> lpWin32 then
  begin
    Exit;
  end;

  if GetCurrentThreadID <> MainThreadID then
  begin
    raise EInvalidOperation.Create('Native button accessibility attachment requires the UI thread');
  end;

  if (AControl = nil) or not AControl.HandleAllocated or not Assigned(AReadState) then
  begin
    raise EArgumentException.Create('Native accessible button requires a live HWND and state reader');
  end;

  if GetWindowThreadProcessId(AControl.Handle, nil) <> GetCurrentThreadID then
  begin
    raise EInvalidOperation.Create('Native button accessibility attachment requires the UI thread');
  end;

  if TMethod(AReadState).Data <> Pointer(AControl) then
  begin
    raise EArgumentException.Create('Native button state reader must belong to its control');
  end;
  LBridge := TButtonAccessibility.CreateFor(AControl, AReadState);
  Result := LBridge;

  if not LBridge.Install then
  begin
    Result := nil;
  end;
  {$endif}
end;

end.
