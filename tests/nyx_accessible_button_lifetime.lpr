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
program nyx_accessible_button_lifetime;

{$mode delphi}{$H+}{$codepage utf8}

{ Observe actual COM apartment initialization/cleanup, not a private bridge
  counter. The registered observers never alter a result, initialize/uninitialize
  an apartment, invoke application code or throw through the COM boundary. }
uses Interfaces, Forms, Controls, Classes, SysUtils, Windows, ActiveX, ComObj,
  nyx.accessibility.button.lcl;

type
  { Native IInitializeSpy ABI. Ole32 calls this interface on the registering
    thread and owns one reference until that same thread revokes its cookie. }
  INativeInitializeSpy = interface(IUnknown)
    ['{00000034-0000-0000-C000-000000000046}']
    function PreInitialize(AFlags, AReferences: DWORD): HRESULT; stdcall;
    function PostInitialize(ACode: HRESULT; AFlags, AReferences: DWORD): HRESULT; stdcall;
    function PreUninitialize(AReferences: DWORD): HRESULT; stdcall;
    function PostUninitialize(AReferences: DWORD): HRESULT; stdcall;
  end;
  { Passive per-thread COM observer. Scalar counters are read only after that
    thread's work is joined; no UI/control or application owner is retained. }
  TApartmentObserver = class(TInterfacedObject, INativeInitializeSpy)
  public
    Initializations: Integer;
    Uninitializations: Integer;
    LastUninitializationThread: TThreadID;
    function PreInitialize(AFlags, AReferences: DWORD): HRESULT; stdcall;
    function PostInitialize(ACode: HRESULT; AFlags, AReferences: DWORD): HRESULT; stdcall;
    function PreUninitialize(AReferences: DWORD): HRESULT; stdcall;
    function PostUninitialize(AReferences: DWORD): HRESULT; stdcall;
  end;
  { Alternative ordinary LCL base proves the public bridge is not tied to
    TCDButton inheritance. Its normal native action retires the lease inside
    the callback; an explicit fault mode checks exception containment. This
    control fabricates no COM response. }
  TProtocolButton = class(TCustomControl)
  public
    Lease: INyxLCLAccessibleButton;
    Observer: TApartmentObserver;
    Actions: Integer;
    UninitializationsInsideAction: Integer;
    RaiseOnAction: Boolean;
    ReportedExpectedException: Boolean;
    function ReadNativeState: TNyxLCLAccessibleButtonState;
    procedure ExecuteDefaultAction; override;
    { Deliberately fail application reporting after observing the original
      action failure. This fault is confined to this owned fixture process. }
    procedure FailingExceptionHandler(ASender: TObject; AException: Exception);
  end;
  { Last public interface release occurs on this joined worker after UI retire.
    Its own COM observer detects any accidental cleanup on the worker thread. }
  TReleaseWorker = class(TThread)
  public
    Lease: INyxLCLAccessibleButton;
    RegisterResult: HRESULT;
    RevokeResult: HRESULT;
    Uninitializations: Integer;
    procedure Execute; override;
  end;

{ Ole32's thread-specific observation API owns the registration, not the spy's
  apartment state. The opaque 64-bit cookie must be revoked on the same thread.
  These Windows XP SP1+ APIs are fixture requirements, not product dependencies.
  https://learn.microsoft.com/windows/win32/api/objbase/nf-objbase-coregisterinitializespy }
function RegisterInitializeSpy(ASpy: INativeInitializeSpy; out ACookie: QWord): HRESULT;
  stdcall; external 'ole32.dll' name 'CoRegisterInitializeSpy';

{ Revoke the exact observer without balancing or changing any caller COM lease. }
function RevokeInitializeSpy(ACookie: QWord): HRESULT;
  stdcall; external 'ole32.dll' name 'CoRevokeInitializeSpy';

function TApartmentObserver.PreInitialize(AFlags, AReferences: DWORD): HRESULT;
begin
  Result := S_OK;
end;

function TApartmentObserver.PostInitialize(ACode: HRESULT;
  AFlags, AReferences: DWORD): HRESULT;
begin

  if ACode >= 0 then
  begin
    Inc(Initializations);
  end;
  { PostInitialize participates in result chaining. Preserve the host's exact
    S_OK/S_FALSE/failure result rather than converting it into another result. }
  Result := ACode;
end;

function TApartmentObserver.PreUninitialize(AReferences: DWORD): HRESULT;
begin
  Inc(Uninitializations);
  LastUninitializationThread := GetCurrentThreadID;
  Result := S_OK;
end;

function TApartmentObserver.PostUninitialize(AReferences: DWORD): HRESULT;
begin
  Result := S_OK;
end;

function TProtocolButton.ReadNativeState: TNyxLCLAccessibleButtonState;
begin
  Result.Pressed := False;
  Result.Focused := Focused;
end;

procedure TProtocolButton.ExecuteDefaultAction;
var
  LBefore: Integer;
begin

  if RaiseOnAction then
  begin
    raise Exception.Create('Expected native action failure');
  end;
  Inc(Actions);
  LBefore := Observer.Uninitializations;
  Lease.Retire;
  UninitializationsInsideAction := Observer.Uninitializations - LBefore;
end;

procedure TProtocolButton.FailingExceptionHandler(ASender: TObject;
  AException: Exception);
begin
  ReportedExpectedException := AException.Message = 'Expected native action failure';
  raise Exception.Create('Expected application exception-handler failure');
end;

procedure TReleaseWorker.Execute;
var
  LObserver: TApartmentObserver;
  LObserverLease: INativeInitializeSpy;
  LCookie: QWord;
begin
  LObserver := TApartmentObserver.Create;
  LObserverLease := LObserver;
  RegisterResult := RegisterInitializeSpy(LObserverLease, LCookie);

  if RegisterResult < 0 then
  begin
    Exit;
  end;
  try
    Lease := nil;
    Uninitializations := LObserver.Uninitializations;
  finally
    RevokeResult := RevokeInitializeSpy(LCookie);
  end;
end;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: String);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

{ Exercise the actual native callback with both a failing control action and
  a failing application reporter. Reporting state remains process-local; the
  public bridge and control are retired before restoring ordinary ownership. }
procedure ReviewFailureContainment(AHost: TForm);
var
  LButton: TProtocolButton;
  LLease: INyxLCLAccessibleButton;
  LPreviousException: TExceptionEvent;
begin
  LButton := TProtocolButton.Create(AHost);
  LPreviousException := Application.OnException;
  try
    LButton.Parent := AHost;
    LButton.SetBounds(16, 72, 180, 40);
    LButton.HandleNeeded;
    LLease := AttachNyxLCLAccessibleButton(LButton, LButton.ReadNativeState);
    Check(LLease <> nil, 'the failure consumer attaches an actual native callback');
    LButton.RaiseOnAction := True;
    Application.OnException := LButton.FailingExceptionHandler;
    SendMessage(LButton.Handle, BM_CLICK, 0, 0);
    Check(LButton.ReportedExpectedException,
      'the native callback reports the original failure and contains a failing application reporter');
  finally
    Application.OnException := LPreviousException;

    if LLease <> nil then
    begin
      LLease.Retire;
      LLease := nil;
    end;
    LButton.Free;
  end;
end;

procedure Review;
var
  LHost: TForm;
  LButton: TProtocolButton;
  LLease: INyxLCLAccessibleButton;
  LObserver: TApartmentObserver;
  LObserverLease: INativeInitializeSpy;
  LCookie: QWord;
  LRegistered: Boolean;
  LWorker: TReleaseWorker;
  LWindow: HWND;
begin
  LHost := TForm.CreateNew(nil);
  LObserver := TApartmentObserver.Create;
  LObserverLease := LObserver;
  LRegistered := False;
  try
    LHost.ClientWidth := 390;
    LHost.ClientHeight := 180;
    LButton := TProtocolButton.Create(LHost);
    LButton.Parent := LHost;
    LButton.SetBounds(16, 16, 180, 40);
    LHost.Show;
    Application.ProcessMessages;
    LWindow := LButton.Handle;
    OleCheck(RegisterInitializeSpy(LObserverLease, LCookie));
    LRegistered := True;
    LLease := AttachNyxLCLAccessibleButton(LButton, LButton.ReadNativeState);
    Check(LLease <> nil, 'the bridge supports an independently implemented LCL control');
    LButton.Lease := LLease;
    LButton.Observer := LObserver;
    Check(LObserver.Initializations - LObserver.Uninitializations = 1,
      'the actual UI COM observer sees the bridge apartment lease');
    SendMessage(LWindow, BM_CLICK, 0, 0);
    Check(LButton.Actions = 1, 'the real native message invokes the alternative control implementation');
    Check(LButton.UninitializationsInsideAction = 0,
      'retirement keeps COM initialized until its native callback returns');
    Check(LObserver.Initializations = LObserver.Uninitializations,
      'the actual UI COM observer sees balanced cleanup after callback return');
    Check(LObserver.LastUninitializationThread = MainThreadID,
      'native apartment cleanup occurs on the owning UI thread');
    SendMessage(LWindow, BM_CLICK, 0, 0);
    Check(LButton.Actions = 1, 'retired native callbacks cannot invoke the live alternative control');
    LButton.Lease := nil;
    LButton.Free;
    LWorker := TReleaseWorker.Create(True);
    try
      LWorker.Lease := LLease;
      LLease := nil;
      LWorker.Start;
      LWorker.WaitFor;
      Check((LWorker.RegisterResult >= 0) and (LWorker.RevokeResult >= 0),
        'the independent worker COM observer registers and revokes normally');
      Check(LWorker.Uninitializations = 0,
        'the last retired interface release performs no COM cleanup on a worker');
    finally
      LWorker.Free;
    end;
    Check(LObserver.Initializations = LObserver.Uninitializations,
      'worker release preserves the UI apartment balance');
    ReviewFailureContainment(LHost);
    Check(LObserver.Initializations = LObserver.Uninitializations,
      'a contained application failure also balances the native UI apartment lease');
    WriteLn('PASS ', GChecks, ' native button COM lifetime checks');
  finally

    if LLease <> nil then
    begin
      LLease.Retire;
      LLease := nil;
    end;

    if LRegistered then
    begin
      OleCheck(RevokeInitializeSpy(LCookie));
    end;
    LHost.Free;
  end;
end;

begin
  try
    Application.Initialize;
    Review;
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
