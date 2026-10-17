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
program nyx_accessible_button_controls;

{$mode delphi}{$H+}{$codepage utf8}

{ Actual Windows roles/actions and ordinary Nyx callback/lifetime integration.
  The owned child receives only this fixture's HWND. No HTTP host, user project,
  recovery, agent connection, screen capture or synthetic COM provider is used. }
uses Interfaces, Forms, Controls, StdCtrls, ExtCtrls, Buttons, Classes, SysUtils,
  Types, Windows, ActiveX, ComObj, Variants, DynLibs, Process, nyx.text, nyx.types,
  nyx.model, nyx.controls, nyx.data, nyx.codec, nyx.events, nyx.behavior, nyx.scheduler,
  nyx.event.emitter, nyx.gestures.lcl, nyx.widgets.lcl, nyx.render.lcl;

type
  { Borrow Oleacc's client API while main owns its DLL lease. Returned COM
    interfaces are independently reference counted, including across processes. }
  TFetchAccessible = function(AWindow: HWND; AObject: DWORD;
    const AIID: TGUID; out AAccessible): HRESULT; stdcall;
  { Native IAccessible vtable through DoDefaultAction, from Microsoft's oleacc.h.
    VARIANT child IDs are by value; BSTR/VARIANT outputs have normal COM ownership.
    The standard native button calibrates the ABI before any Nyx assertion. }
  IButtonAccessible = interface(IDispatch)
    ['{618736E0-3C3D-11CF-810C-00AA00389B71}']
    function GetParent(out AParent: IDispatch): HRESULT; stdcall;
    function GetChildCount(out ACount: LongInt): HRESULT; stdcall;
    function GetChild(AChild: OleVariant; out AObject: IDispatch): HRESULT; stdcall;
    function GetName(AChild: OleVariant; out AName: WideString): HRESULT; stdcall;
    function GetValue(AChild: OleVariant; out AValue: WideString): HRESULT; stdcall;
    function GetDescription(AChild: OleVariant; out AText: WideString): HRESULT; stdcall;
    function GetRole(AChild: OleVariant; out ARole: OleVariant): HRESULT; stdcall;
    function GetState(AChild: OleVariant; out AState: OleVariant): HRESULT; stdcall;
    function GetHelp(AChild: OleVariant; out AText: WideString): HRESULT; stdcall;
    function GetHelpTopic(out AFile: WideString; AChild: OleVariant; out ATopic: LongInt): HRESULT; stdcall;
    function GetShortcut(AChild: OleVariant; out AText: WideString): HRESULT; stdcall;
    function GetFocus(out AFocus: OleVariant): HRESULT; stdcall;
    function GetSelection(out ASelection: OleVariant): HRESULT; stdcall;
    function GetDefaultAction(AChild: OleVariant; out AText: WideString): HRESULT; stdcall;
    function Select(AFlags: LongInt; AChild: OleVariant): HRESULT; stdcall;
    function GetLocation(out ALeft, ATop, AWidth, AHeight: LongInt;
      AChild: OleVariant): HRESULT; stdcall;
    function Navigate(ADirection: LongInt; AStart: OleVariant;
      out ATarget: OleVariant): HRESULT; stdcall;
    function HitTest(AX, AY: LongInt; out ATarget: OleVariant): HRESULT; stdcall;
    function DoDefaultAction(AChild: OleVariant): HRESULT; stdcall;
  end;
  { Ordinary library button exposes only HWND recreation for lifetime evidence.
    The form owns this control; it supplies no fake accessibility implementation. }
  TRecreatedButton = class(TNyxLCLButton)
  public
    procedure Rebuild;
  end;
  { Form-owned callback/exception receiver. Renderer and controls are borrowed
    until the owning fixture retires them. No callback retains an owning view. }
  TProbe = class(TComponent)
  protected
    { Track the actual component. Safe frame reparenting can replace its HWND
      before its callback ends without disposing the Pascal receiver. }
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    Host: TForm;
    Renderer: TNyxLCLRenderer;
    StandardClicks: Integer;
    StandaloneClicks: Integer;
    CreatorClicks: Integer;
    ApplicationClicks: Integer;
    DesignSelections: Integer;
    ConsumeKey: Boolean;
    DuringSync: Boolean;
    SyncRequests: Integer;
    UnmountOnClick: Boolean;
    AliveInCallback: Boolean;
    RetiredActionRefused: Boolean;
    TrackedControl: TComponent;
    TrackedDestroyed: Boolean;
    Error: TNyxText;
    procedure StandardClicked(ASender: TObject);
    procedure StandaloneClicked(ASender: TObject);
    procedure CreatorClicked(ASender: TObject);
    procedure Resized(ASender: TObject);
    procedure KeyDown(ASender: TObject; var AKey: Word; AShift: TShiftState);
    procedure Failed(ASender: TObject; AException: Exception);
    function Sink(const AOrigin: TNyxText; const AName: TNyxEventRef;
      const APayload: TNyxDataValue; AHasPayload: Boolean): Boolean;
    procedure ApplicationClicked;
    procedure Selected(ANode: TNyxNode; const AEvent: TNyxEventInfo);
  end;
  { Router-owned portable callback borrows the live fixture receiver. The main
    fixture also retains its interface until the renderer/subscription retires. }
  TApplicationCallback = class(TNyxEventCallback)
  public
    Probe: TProbe;
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;
  { Joined worker checks that the public default-action entry point refuses
    before any borrowed control or producer scope is read. }
  TWorker = class(TThread)
  public
    Control: TNyxLCLButton;
    Refused: Boolean;
    procedure Execute; override;
  end;

const
  ClientObject: DWORD = $FFFFFFFC;
  PushButtonRole: LongInt = $2B;
  PressedState: LongInt = $0008;
  UnavailableState: LongInt = $0001;
  { Native factory registration is an explicit open extension boundary. }
  ButtonKind: TNyxText = 'button';

var
  GFetch: TFetchAccessible;
  GChecks: Integer;
  { Borrowed only during Review, including native factory construction. }
  GProbe: TProbe;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function ChildSelf: OleVariant;
begin
  Result := VarAsType(Integer(0), varInteger);
end;

function Accessible(AWindow: HWND): IButtonAccessible;
begin
  Result := nil;
  OleCheck(GFetch(AWindow, ClientObject, IButtonAccessible, Result));
  Check(Result <> nil, 'the real MSAA client obtains a native object');
end;

function Role(const AObject: IButtonAccessible): LongInt;
var
  LRole: OleVariant;
begin
  OleCheck(AObject.GetRole(ChildSelf, LRole));
  Result := LRole;
end;

function State(const AObject: IButtonAccessible): LongInt;
var
  LState: OleVariant;
begin
  OleCheck(AObject.GetState(ChildSelf, LState));
  Result := LState;
end;

procedure Pump;
var
  LTurn: Integer;
begin
  for LTurn := 1 to 3 do
  begin
    Application.ProcessMessages;
    Sleep(1);
  end;
  Check(GProbe.Error = '', 'native callbacks report no hidden failure: ' + GProbe.Error);
end;

procedure Invoke(const AObject: IButtonAccessible; ADispatch: Boolean = False);
var
  LDispatch: OleVariant;
begin

  if ADispatch then
  begin
    LDispatch := IDispatch(AObject);
    LDispatch.accDoDefaultAction(ChildSelf);
  end
  else
  begin
    OleCheck(AObject.DoDefaultAction(ChildSelf));
  end;
  Pump;
end;

procedure TRecreatedButton.Rebuild;
begin
  RecreateWnd(Self);
end;

procedure TProbe.StandardClicked(ASender: TObject);
begin
  Inc(StandardClicks);
end;

procedure TProbe.StandaloneClicked(ASender: TObject);
begin
  Inc(StandaloneClicks);
end;

procedure TProbe.CreatorClicked(ASender: TObject);
begin
  Inc(CreatorClicks);
end;

procedure TProbe.Resized(ASender: TObject);
begin

  if DuringSync then
  begin
    Inc(SyncRequests);
    SendMessage(TWinControl(ASender).Handle, BM_CLICK, 0, 0);
  end;
end;

procedure TProbe.KeyDown(ASender: TObject; var AKey: Word; AShift: TShiftState);
begin

  if ConsumeKey then
  begin
    AKey := 0;
  end;
end;

procedure TProbe.Failed(ASender: TObject; AException: Exception);
begin
  Error := TNyxText(AException.ClassName) + ': ' + TNyxText(AException.Message);
end;

function TProbe.Sink(const AOrigin: TNyxText; const AName: TNyxEventRef;
  const APayload: TNyxDataValue; AHasPayload: Boolean): Boolean;
begin
  Result := False;
end;

procedure TProbe.ApplicationClicked;
var
  LControl: TNyxLCLButton;
  LRetirementHost: TCustomForm;
  LWindow: HWND;
begin
  Inc(ApplicationClicks);

  if UnmountOnClick then
  begin
    LControl := TNyxLCLButton(Renderer.ControlFor('run'));
    TrackedControl := LControl;
    LControl.FreeNotification(Self);
    Renderer.Unmount;
    AliveInCallback := not TrackedDestroyed and (TrackedControl = LControl);

    if AliveInCallback then
    begin
      LWindow := LControl.Handle;
      { Retirement can reparent this control into a hidden lifetime host.
        Observe that actual parent, not the original visible form. ModalResult
        acts before OnClick, so clearing callbacks alone cannot pass this check. }
      LRetirementHost := GetParentForm(LControl);
      LRetirementHost.ModalResult := mrNone;
      LControl.Kind := bkCustom;
      LControl.ModalResult := mrOk;
      SendMessage(LWindow, BM_CLICK, 0, 0);
      RetiredActionRefused := (LRetirementHost.ModalResult = mrNone) and Host.Visible;
    end;
  end;
end;

procedure TProbe.Notification(AComponent: TComponent; AOperation: TOperation);
begin

  if (AOperation = opRemove) and (AComponent = TrackedControl) then
  begin
    TrackedControl := nil;
    TrackedDestroyed := True;
  end;
  inherited Notification(AComponent, AOperation);
end;

procedure TApplicationCallback.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  Probe.ApplicationClicked;
end;

procedure TProbe.Selected(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin

  if AEvent.Trigger = ntDesignSelect then
  begin
    Inc(DesignSelections);
  end;
end;

procedure TWorker.Execute;
begin
  try
    Control.ExecuteDefaultAction;
  except
    on EInvalidOperation do
    begin
      Refused := True;
    end;
  end;
end;

{ Native extension boundary: return an unparented library control owned by the
  supplied host. Build connects its producer/frame before allocating its HWND. }
function FactoryButton(ANode: TNyxNode; AOwner: TComponent): TControl;
var
  LButton: TNyxLCLButton;
begin
  LButton := TNyxLCLButton.Create(AOwner);
  LButton.Caption := ANode.Prop(NyxAttributeName(atText));
  LButton.OnClick := GProbe.CreatorClicked;
  LButton.AddHandlerOnResize(GProbe.Resized);
  Result := LButton;
end;

{ Standard output is a borrowed UTF-8 byte handle, not an ANSI text console. }
procedure ClientPacket(const AValue: TNyxDataValue);
var
  LStream: THandleStream;
  LBytes: TNyxText;
begin
  LBytes := AValue.ToJSON + #10;
  LStream := THandleStream.Create(GetStdHandle(STD_OUTPUT_HANDLE));
  try
    LStream.WriteBuffer(LBytes[1], Length(LBytes));
  finally
    LStream.Free;
  end;
end;

procedure ClientInvoke(AWindow: HWND);
var
  LObject: IButtonAccessible;
  LAction: WideString;
  LRole: Integer;
begin
  LObject := Accessible(AWindow);
  LRole := Role(LObject);
  OleCheck(LObject.GetDefaultAction(ChildSelf, LAction));
  OleCheck(LObject.DoDefaultAction(ChildSelf));
  ClientPacket(NyxObject([NyxField('role', NyxData(LRole)),
    NyxField('action', NyxData(TNyxText(UTF8Encode(LAction))))]));
end;

procedure InvokeFromChild(AWindow: HWND);
var
  LProcess: TProcess;
  LOutput: TMemoryStream;
  LText: TNyxText;
  LFirstLine: Integer;
  LValue: TNyxDataValue;
  LDeadline: QWord;
begin
  LProcess := TProcess.Create(nil);
  LOutput := TMemoryStream.Create;
  try
    LProcess.Executable := ParamStr(0);
    LProcess.Parameters.Add('--invoke-window');
    LProcess.Parameters.Add(IntToStr(AWindow));
    LProcess.Options := [poUsePipes, poStderrToOutPut, poNoConsole];
    LProcess.Execute;
    LDeadline := GetTickCount64 + 10000;
    while LProcess.Running and (GetTickCount64 < LDeadline) do
    begin
      Application.ProcessMessages;
      Sleep(1);
    end;

    if LProcess.Running then
    begin
      LProcess.Terminate(1);
      LProcess.WaitOnExit;
      raise Exception.Create('The exact owned MSAA action client did not finish');
    end;
    LOutput.CopyFrom(LProcess.Output, 0);
    Check(LProcess.ExitStatus = 0, 'the independent native action client completes');
    SetLength(LText, LOutput.Size);

    if LOutput.Size > 0 then
    begin
      LOutput.Position := 0;
      LOutput.ReadBuffer(LText[1], Length(LText));
    end;
    Check(Pos('0 unfreed memory blocks : 0', LText) > 0, 'the independent client has a clean Pascal heap');
    LFirstLine := Pos(#10, LText);
    Check(LFirstLine > 0, 'the independent client emits its complete result');
    LValue := TNyxDataValue.ParseJSON(Copy(LText, 1, LFirstLine - 1));
    Check(LValue.Field('role').AsInteger = PushButtonRole, 'the external client reads a real push-button role');
    Check(LValue.Field('action').AsText <> '', 'the external client reads the system action verb');
    Pump;
  finally
    LOutput.Free;
    LProcess.Free;
  end;
end;

procedure Review(AWidth: Integer);
var
  LHost: TForm;
  LMount: TPanel;
  LStandard: StdCtrls.TButton;
  LStandalone: TRecreatedButton;
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LButton: INyxButton;
  LRenderer: TNyxLCLRenderer;
  LCallback: TApplicationCallback;
  LCallbackLease: INyxEventCallback;
  LSubscription: INyxEventSubscription;
  LSource: INyxEventEmitterScope;
  LFrame: INyxNativeGestureFrame;
  LObject: IButtonAccessible;
  LOldObject: IButtonAccessible;
  LNative: TNyxLCLButton;
  LWorker: TWorker;
  LAction: WideString;
  LName: WideString;
  LCode: HRESULT;
  LBefore: Integer;
  LCreatorBefore: Integer;
  LOldWindow: HWND;
  LSourceBefore: TNyxText;
  LPreviousException: TExceptionEvent;
begin
  LHost := TForm.CreateNew(nil);
  LDocument := TNyxDocument.Create;
  LRenderer := TNyxLCLRenderer.Create;
  GProbe := TProbe.Create(LHost);
  GProbe.Host := LHost;
  GProbe.Renderer := LRenderer;
  LPreviousException := Application.OnException;
  Application.OnException := GProbe.Failed;
  try
    LHost.ClientWidth := AWidth;
    LHost.ClientHeight := 640;
    LStandard := StdCtrls.TButton.Create(LHost);
    LStandard.Parent := LHost;
    LStandard.Caption := 'Standard action';
    LStandard.OnClick := GProbe.StandardClicked;
    LStandard.SetBounds(8, 16, 160, 40);
    LStandalone := TRecreatedButton.Create(LHost);
    LStandalone.Parent := LHost;
    LStandalone.Caption := 'Standalone action';
    LStandalone.SetBounds(184, 16, 180, 40);
    LStandalone.OnClick := GProbe.StandaloneClicked;
    LStandalone.OnKeyDown := GProbe.KeyDown;
    LMount := TPanel.Create(LHost);
    LMount.Parent := LHost;
    LMount.SetBounds(0, 96, AWidth, 500);
    LHost.Show;
    Pump;
    Check(LHost.ClientWidth = AWidth, 'the native host has the actual requested width');
    LObject := Accessible(LStandard.Handle);
    Check(Role(LObject) = PushButtonRole, 'the standard reference calibrates the native role');
    Invoke(LObject);
    Check(GProbe.StandardClicks = 1, 'the standard reference calibrates the actual native action');
    LObject := Accessible(LStandalone.Handle);
    Check(Role(LObject) = PushButtonRole, 'the library button exposes a real push-button role');
    OleCheck(LObject.GetDefaultAction(ChildSelf, LAction));
    Check(LAction <> '', 'the library button exposes its system-localized action');
    Invoke(LObject);
    Invoke(LObject, True);
    Check(GProbe.StandaloneClicks = 2, 'direct and automation actions invoke the actual library button once each');
    LStandalone.Enabled := False;
    Check((State(LObject) and UnavailableState) <> 0, 'disabled state is visible to native clients');
    Invoke(LObject);
    Check(GProbe.StandaloneClicks = 2, 'disabled native activation produces no click');
    LStandalone.Enabled := True;
    LStandalone.SetFocus;
    SendMessage(LStandalone.Handle, WM_KEYDOWN, VK_SPACE, 0);
    Check((State(LObject) and PressedState) <> 0, 'native clients observe the actual pressed keyboard state');
    SendMessage(LStandalone.Handle, WM_KEYUP, VK_SPACE, 0);
    Pump;
    Check((State(LObject) and PressedState) = 0, 'native clients observe keyboard release');
    Check(GProbe.StandaloneClicks = 3, 'ordinary keyboard activation remains single-shot');
    GProbe.ConsumeKey := True;
    SendMessage(LStandalone.Handle, WM_KEYDOWN, VK_SPACE, 0);
    SendMessage(LStandalone.Handle, WM_KEYUP, VK_SPACE, 0);
    Pump;
    Check(GProbe.StandaloneClicks = 3, 'a consumed activation key still suppresses the default click');
    GProbe.ConsumeKey := False;
    LSource := NewNyxEventEmitterScope(LRenderer.Events.Scheduler);
    LFrame := NewNyxNativeGestureFrame;
    LStandalone.ConnectAccessibility(LSource.ForControl('standalone'), LFrame);
    Invoke(LObject);
    Check(GProbe.StandaloneClicks = 3, 'a dormant managed source cannot activate a button');
    LSource.Activate(GProbe.Sink);
    Invoke(LObject);
    Check(GProbe.StandaloneClicks = 4, 'an admitted managed source uses the actual Click path');
    LStandalone.DisconnectAccessibility;
    Invoke(LObject);
    Check(GProbe.StandaloneClicks = 4, 'disconnected activation refuses while its HWND remains live');
    LStandalone.ConnectAccessibility(LSource.ForControl('standalone'), LFrame);
    Invoke(LObject);
    Check(GProbe.StandaloneClicks = 5, 'a retained control can reconnect to its admitted context');
    LSource.Disconnect;
    Invoke(LObject);
    Check(GProbe.StandaloneClicks = 5, 'producer revocation also prevents activation');
    LOldObject := LObject;
    LStandalone.Rebuild;
    LObject := Accessible(LStandalone.Handle);
    Check(Role(LObject) = PushButtonRole, 'a recreated HWND receives its own native button provider');
    LCode := LOldObject.DoDefaultAction(ChildSelf);
    Pump;
    Check(GProbe.StandaloneClicks = 5, 'an old native provider cannot activate the recreated control');
    LOldObject := nil;
    LWorker := TWorker.Create(True);
    try
      LWorker.Control := LStandalone;
      LWorker.Start;
      LWorker.WaitFor;
      Check(LWorker.Refused, 'direct worker activation refuses before touching UI context');
    finally
      LWorker.Free;
    end;

    LPage := NewNyxPage('home');
    LDocument.AddPage(LPage);
    LButton := NewNyxButton('run').WithText('Run application');
    LButton.Configure.Width(180).AccessibleName('Run the application action').Done;
    LPage.Add(LButton);
    LRenderer.RegisterFactory(ButtonKind, FactoryButton);
    LRenderer.Render(LDocument, LDocument.Pages[0], LMount);
    LCallback := TApplicationCallback.Create;
    LCallback.Probe := GProbe;
    LCallbackLease := LCallback;
    LSubscription := LRenderer.Events.On(NyxControlEvents('run'), ntClick).Subscribe(LCallbackLease);
    LNative := TNyxLCLButton(LRenderer.ControlFor('run'));
    LObject := Accessible(LNative.Handle);
    Check(Role(LObject) = PushButtonRole, 'ordinary rendered custom-factory buttons inherit the provider');
    OleCheck(LObject.GetName(ChildSelf, LName));
    Check(TNyxText(UTF8Encode(LName)) = 'Run the application action', 'native action support retains authored names');
    Invoke(LObject);
    Check((GProbe.ApplicationClicks = 1) and (GProbe.CreatorClicks = 1),
      'native activation traverses both the ordinary creator hook and Nyx event router once');
    InvokeFromChild(LNative.Handle);
    Check((GProbe.ApplicationClicks = 2) and (GProbe.CreatorClicks = 2),
      'an independent process invokes the current ordinary application callback once');
    LBefore := GProbe.ApplicationClicks;
    LCreatorBefore := GProbe.CreatorClicks;
    GProbe.DuringSync := True;
    try
      LRenderer.Root.Find('run').Configure.Width(240).Height(64);
      LRenderer.Sync;
    finally
      GProbe.DuringSync := False;
    end;
    Pump;
    Check(GProbe.SyncRequests > 0, 'a real native resize requests activation during synchronization');
    Check((GProbe.ApplicationClicks = LBefore) and (GProbe.CreatorClicks = LCreatorBefore),
      'synchronization refuses activation before any creator or application click');
    LSourceBefore := TNyxCodec.Encode(LDocument);
    GProbe.UnmountOnClick := True;
    LOldWindow := LNative.Handle;
    Invoke(LObject);
    Check(GProbe.ApplicationClicks = LBefore + 1, 'the current callback can unmount its own view');
    Check(GProbe.AliveInCallback, 'the existing native frame defers control disposal through its callback');
    Check(GProbe.RetiredActionRefused, 'a logically retired button cannot change even its actual parent modal result');
    Pump;
    Check(GProbe.TrackedDestroyed, 'the deferred component is disposed after the real message returns');
    Check(not IsWindow(LOldWindow), 'the old native window and provider are retired');
    LCode := LObject.GetName(ChildSelf, LName);
    Check((LCode < 0) or (LName = ''), 'a retained Windows proxy exposes no live retired control');
    Check(TNyxCodec.Encode(LDocument) = LSourceBefore, 'native application activation does not rewrite the authored document');
    GProbe.UnmountOnClick := False;
    LBefore := GProbe.ApplicationClicks;
    LCreatorBefore := GProbe.CreatorClicks;
    LRenderer.OnEvent := GProbe.Selected;
    LRenderer.Render(LDocument, LDocument.Pages[0], LMount, True);
    LSubscription := LRenderer.Events.On(NyxControlEvents('run'), ntClick).Subscribe(LCallbackLease);
    LObject := Accessible(TNyxLCLButton(LRenderer.ControlFor('run')).Handle);
    Invoke(LObject);
    Check(GProbe.DesignSelections = 1, 'native accessibility can select an ordinary design-preview control');
    Check((GProbe.ApplicationClicks = LBefore) and (GProbe.CreatorClicks = LCreatorBefore),
      'design preview pauses creator and application behavior for native actions');
    WriteLn('PASS ', GChecks, ' native button role/action checks at width ', AWidth);
  finally
    GProbe.DuringSync := False;
    LOldObject := nil;
    LObject := nil;
    LRenderer.Free;
    LSubscription := nil;
    LCallbackLease := nil;
    LSource := nil;
    LFrame := nil;
    LDocument.Free;
    Application.OnException := LPreviousException;
    LHost.Free;
    GProbe := nil;
  end;
end;

var
  LLibrary: TLibHandle;
  LCOM: HRESULT;
  LWidth: Integer;
  LWindow: QWord;
begin
  LLibrary := NilHandle;
  LCOM := CoInitialize(nil);
  try
    LLibrary := LoadLibrary('oleacc.dll');
    Check(LLibrary <> NilHandle, 'Oleacc is available');
    GFetch := TFetchAccessible(GetProcedureAddress(LLibrary, 'AccessibleObjectFromWindow'));
    Check(Assigned(GFetch), 'the actual MSAA entry point is available');

    if (ParamCount = 2) and (ParamStr(1) = '--invoke-window') then
    begin
      Check(TryStrToQWord(ParamStr(2), LWindow) and (LWindow <= High(HWND)), 'the owned HWND is valid');
      ClientInvoke(HWND(LWindow));
    end
    else
    begin
      LWidth := 800;

      if ParamCount > 0 then
      begin
        Check((ParamCount = 1) and TryStrToInt(ParamStr(1), LWidth) and
          ((LWidth = 800) or (LWidth = 390)), 'supply width 800 or 390');
      end;
      Application.Initialize;
      Review(LWidth);
    end;
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      ExitCode := 1;
    end;
  end;

  if LLibrary <> NilHandle then
  begin
    UnloadLibrary(LLibrary);
  end;

  if LCOM >= 0 then
  begin
    CoUninitialize;
  end;
end.
