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
program nyx_accessibility_lcl_controls;

{$mode delphi}{$H+}{$codepage utf8}

{ Actual Win32 MSAA calls, not just LCL metadata. The child client reads only
  HWNDs supplied by this owning fixture while its UI pumps real messages.
  No HTTP listener, project recovery, operator design or capture is involved. }
uses Interfaces, Forms, Controls, StdCtrls, Classes, SysUtils, Windows, ActiveX,
  ComObj, Variants, DynLibs, Process, nyx.text, nyx.data, nyx.types, nyx.model,
  nyx.controls, nyx.codec, nyx.state, nyx.render.lcl, nyx.canvas.lcl,
  nyx.accessibility.lcl;

type
  { Borrow Oleacc's native client API; each returned COM interface has a normal
    independent lease. The module remains loaded until every client retires. }
  TAccessibleFromWindow = function(AWindow: HWND; AObject: DWORD;
    const AIID: TGUID; out AAccessible): HRESULT; stdcall;
  { Read-only IAccessible vtable prefix from Microsoft's oleacc.h. VARIANT child
    IDs are passed by value and BSTR results have native WideString ownership.
    Direct calls matter: Oleacc's automation dispatch bypasses annotations on
    this host. This client implements no provider or synthetic MSAA response. }
  IAccessibleRead = interface(IDispatch)
    ['{618736E0-3C3D-11CF-810C-00AA00389B71}']
    function GetParent(out AParent: IDispatch): HRESULT; stdcall;
    function GetChildCount(out ACount: LongInt): HRESULT; stdcall;
    function GetChild(AChild: OleVariant; out AObject: IDispatch): HRESULT; stdcall;
    function GetName(AChild: OleVariant; out AName: WideString): HRESULT; stdcall;
    function GetValue(AChild: OleVariant; out AValue: WideString): HRESULT; stdcall;
    function GetDescription(AChild: OleVariant; out AText: WideString): HRESULT; stdcall;
    function GetRole(AChild: OleVariant; out ARole: OleVariant): HRESULT; stdcall;
    function GetState(AChild: OleVariant; out AState: OleVariant): HRESULT; stdcall;
  end;
  { Dedicated ordinary edit exposes only handle recreation for lifetime review.
    Its form owns it; no model, renderer or provider implementation is involved. }
  TRecreatedEdit = class(TEdit)
  public
    procedure Rebuild;
  end;
  { A joined worker borrows one live control only to verify off-thread refusal.
    It never reads LCL fields or initializes COM and owns no application callback. }
  TWorkerProbe = class(TThread)
  public
    Control: TControl;
    Refused: Boolean;
    procedure Execute; override;
  end;
  { Form-owned receiver for an actual standard MSAA default action. Its button
    is disposed before this receiver; no document/application event is simulated. }
  TClickCounter = class(TComponent)
  public
    Count: Integer;
    procedure Clicked(ASender: TObject);
  end;

const
  ClientObject: DWORD = $FFFFFFFC;
  UnicodeName: TNyxText = 'Planning 🌙 / 東京';

var
  GFetch: TAccessibleFromWindow;
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

function ReadAccessible(AWindow: HWND): IAccessibleRead;
begin
  Result := nil;
  OleCheck(GFetch(AWindow, ClientObject, IAccessibleRead, Result));
  Check(Result <> nil, 'the actual MSAA API returns an interface');
end;

function Snapshot(AWindow: HWND): TNyxDataValue;
var
  LAccessible: IAccessibleRead;
  LChild: OleVariant;
  LName: WideString;
  LDescription: WideString;
  LValue: WideString;
  LValueCode: HRESULT;
begin
  LAccessible := ReadAccessible(AWindow);
  { A numeric literal may marshal as VT_I2. CHILDID_SELF explicitly requires
    VT_I4 even when its value is zero. S_FALSE legitimately means empty text. }
  LChild := VarAsType(Integer(0), varInteger);
  OleCheck(LAccessible.GetName(LChild, LName));
  OleCheck(LAccessible.GetDescription(LChild, LDescription));
  { Generic non-input providers may not implement Value. Preserve that native
    result explicitly; input checks below require their actual value text. }
  LValueCode := LAccessible.GetValue(LChild, LValue);
  Result := NyxObject([NyxField('name', NyxData(TNyxText(UTF8Encode(LName)))),
    NyxField('description', NyxData(TNyxText(UTF8Encode(LDescription)))),
    NyxField('value', NyxData(TNyxText(UTF8Encode(LValue)))),
    NyxField('valueResult', NyxData(Integer(LValueCode)))]);
end;

procedure TRecreatedEdit.Rebuild;
begin
  RecreateWnd(Self);
end;

procedure TWorkerProbe.Execute;
begin
  try
    SyncNyxLCLAccessibility(Control);
  except
    on EInvalidOperation do
    begin
      Refused := True;
    end;
  end;
end;

procedure TClickCounter.Clicked(ASender: TObject);
begin
  Inc(Count);
end;

function ReadFromChild(AWindow: HWND): TNyxDataValue;
var
  LProcess: TProcess;
  LOutput: TMemoryStream;
  LText: TNyxText;
  LFirstLine: Integer;
  LDeadline: QWord;
begin
  LProcess := TProcess.Create(nil);
  LOutput := TMemoryStream.Create;
  try
    LProcess.Executable := ParamStr(0);
    LProcess.Parameters.Add('--read-window');
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
      { This is the exact child process handle created above, never a PID search
        or another service. Join it before releasing its owner on a failed run. }
      LProcess.Terminate(1);
      LProcess.WaitOnExit;
      raise Exception.Create('Owned MSAA client did not finish');
    end;
    LOutput.CopyFrom(LProcess.Output, 0);
    Check(LProcess.ExitStatus = 0, 'the independent MSAA client completes');
    SetLength(LText, LOutput.Size);

    if LOutput.Size > 0 then
    begin
      LOutput.Position := 0;
      LOutput.ReadBuffer(LText[1], Length(LText));
    end;
    LFirstLine := Pos(#10, LText);
    Check(LFirstLine > 0, 'the independent client emits its complete JSON line');
    Check(Pos('0 unfreed memory blocks : 0', LText) > 0, 'the independent client has a clean Pascal heap');
    LText := Copy(LText, 1, LFirstLine - 1);

    if (LText <> '') and (LText[Length(LText)] = #13) then
    begin
      Delete(LText, Length(LText), 1);
    end;
    Result := TNyxDataValue.ParseJSON(LText);
  finally
    LOutput.Free;
    LProcess.Free;
  end;
end;

procedure CheckExternal(AWindow: HWND; const AExpected: TNyxDataValue);
var
  LObserved: TNyxDataValue;
begin
  LObserved := ReadFromChild(AWindow);

  if LObserved.ToJSON <> AExpected.ToJSON then
  begin
    WriteLn('Expected native snapshot: ', AExpected.ToJSON);
    WriteLn('Observed external snapshot: ', LObserved.ToJSON);
  end;
  Check(LObserved.ToJSON = AExpected.ToJSON,
    'an independent process reads the exact current metadata/value');
end;

{ Standard output is a borrowed byte handle. A text-console conversion must not
  change supplementary names before the owning parent parses their exact JSON. }
procedure WriteClientPacket(const AValue: TNyxDataValue);
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

procedure Review(AWidth: Integer);
var
  LHost: TForm;
  LDocument: TNyxDocument;
  LRenderer: TNyxLCLRenderer;
  LState: TNyxState;
  LPage: INyxPage;
  LCanvas: INyxCanvas;
  LButton: INyxButton;
  LMemo: INyxMemo;
  LView: INyxLCLCanvasWorkspaceView;
  LEdit: TRecreatedEdit;
  LReference: TButton;
  LWorker: TWorkerProbe;
  LClicks: TClickCounter;
  LBefore: TNyxText;
  LSnapshot: TNyxDataValue;
  LOldAccessible: IAccessibleRead;
  LCanvasAccessible: IAccessibleRead;
  LAutomation: OleVariant;
  LRole: OleVariant;
  LChild: OleVariant;
  LOldName: WideString;
  LOldCode: HRESULT;
  LProcedureBefore: TMethod;
begin
  LHost := TForm.CreateNew(nil);
  LDocument := TNyxDocument.Create;
  LRenderer := TNyxLCLRenderer.Create;
  LState := nil;
  try
    LHost.ClientWidth := AWidth;
    LHost.ClientHeight := 640;
    LPage := NewNyxPage('home');
    LDocument.AddPage(LPage);
    LDocument.State.SetValue(NyxTextState('board-name'), 'Planning board');
    LCanvas := NewNyxCanvas('board');
    LCanvas.Configure.Height(160).AccessibleName('Planning board').
      Hint('Explore the planning board').Done;
    LCanvas.Binds.AccessibleName(NyxTextState('board-name'));
    LPage.Add(LCanvas);
    LButton := NewNyxButton('save').WithText('Save');
    LButton.Configure.AccessibleName('Save this diagram').Done;
    LPage.Add(LButton);
    LMemo := NewNyxMemo('notes').WithText('Notes');
    { WithText names the field's label; Value configures its editable contents. }
    LMemo.Configure.AccessibleName('Diagram notes').Value('Notes').Height(100).Done;
    LPage.Add(LMemo);
    LDocument.Validate;
    LBefore := TNyxCodec.Encode(LDocument);
    LState := LDocument.State.Clone;
    LHost.Show;
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost, False, LState);
    Application.ProcessMessages;
    Check(LHost.ClientWidth = AWidth, 'actual host allocation matches the requested width');
    LClicks := TClickCounter.Create(LHost);
    LReference := TButton.Create(LHost);
    LReference.Parent := LHost;
    LReference.Caption := 'Reference button';
    LReference.OnClick := LClicks.Clicked;
    LReference.SetBounds(8, 560, 160, 32);
    LSnapshot := Snapshot(LReference.Handle);
    Check(LSnapshot.Field('name').AsText = 'Reference button',
      'an unannotated standard button calibrates the native client');
    LView := LRenderer.CanvasView('board') as INyxLCLCanvasWorkspaceView;
    LSnapshot := Snapshot(LView.Surface.Handle);
    Check(LSnapshot.Field('name').AsText = 'Planning board', 'MSAA reads the authored Canvas name');
    Check(LSnapshot.Field('description').AsText = 'Explore the planning board',
      'MSAA reads the authored Canvas description');
    CheckExternal(LView.Surface.Handle, LSnapshot);
    LSnapshot := Snapshot(TWinControl(LRenderer.ControlFor('save')).Handle);
    Check(LSnapshot.Field('name').AsText = 'Save this diagram',
      'MSAA reads the authored custom button name');
    CheckExternal(TWinControl(LRenderer.ControlFor('save')).Handle, LSnapshot);
    LSnapshot := Snapshot(TWinControl(LRenderer.InputFor('notes')).Handle);
    Check(LSnapshot.Field('name').AsText = 'Diagram notes', 'MSAA reads the framed memo name');
    Check(LSnapshot.Field('value').AsText = 'Notes', 'the standard editable value remains available');
    CheckExternal(TWinControl(LRenderer.InputFor('notes')).Handle, LSnapshot);
    LCanvasAccessible := ReadAccessible(LView.Surface.Handle);
    LState.SetValue(NyxTextState('board-name'), UnicodeName);
    Application.ProcessMessages;
    LChild := VarAsType(Integer(0), varInteger);
    OleCheck(LCanvasAccessible.GetName(LChild, LOldName));
    Check(TNyxText(UTF8Encode(LOldName)) = UnicodeName,
      'a retained native client observes the exact updated state binding');
    CheckExternal(LView.Surface.Handle, Snapshot(LView.Surface.Handle));
    LCanvasAccessible := nil;
    LReference.AccessibleName := 'Reference accessible name';
    LReference.AccessibleDescription := 'Reference help';
    Check(SyncNyxLCLAccessibility(LReference) = nlasPublished, 'standard annotation capability is available');
    LSnapshot := Snapshot(LReference.Handle);
    Check(LSnapshot.Field('name').AsText = 'Reference accessible name', 'standard name override is visible');
    Check(LSnapshot.Field('description').AsText = 'Reference help', 'standard description is visible');
    Check(LReference.Caption = 'Reference button', 'annotation preserves the visible caption');
    LOldAccessible := ReadAccessible(LReference.Handle);
    OleCheck(LOldAccessible.GetRole(LChild, LRole));
    Check(Integer(LRole) = $2B, 'the standard native push-button role remains available');
    LAutomation := IDispatch(LOldAccessible);
    LAutomation.accDoDefaultAction(LChild);
    Application.ProcessMessages;
    Check(LClicks.Count = 1, 'the standard MSAA action still invokes the actual button');
    VarClear(LAutomation);
    LOldAccessible := nil;
    LReference.AccessibleName := '';
    LReference.AccessibleDescription := '';
    SyncNyxLCLAccessibility(LReference);
    LSnapshot := Snapshot(LReference.Handle);
    Check(LSnapshot.Field('name').AsText = 'Reference button', 'clearing restores standard name behavior');
    Check(LSnapshot.Field('description').AsText = '', 'clearing restores standard description behavior');

    LEdit := TRecreatedEdit.Create(LHost);
    LEdit.AccessibleName := UnicodeName;
    LEdit.AccessibleDescription := 'Independent edit help';
    LEdit.Text := 'Preserved draft';
    LProcedureBefore := TMethod(LEdit.WindowProc);
    Check(not LEdit.HandleAllocated, 'the independent edit starts without a native handle');
    Check(SyncNyxLCLAccessibility(LEdit) = nlasDeferred, 'metadata permits a lazy native handle');
    Check(not LEdit.HandleAllocated, 'metadata does not force handle creation');
    Check((LProcedureBefore.Code = TMethod(LEdit.WindowProc).Code) and
      (LProcedureBefore.Data = TMethod(LEdit.WindowProc).Data),
      'metadata preserves the existing LCL window procedure');
    LEdit.Parent := LHost;
    LEdit.SetBounds(180, 560, 160, 32);
    LEdit.HandleNeeded;
    LSnapshot := Snapshot(LEdit.Handle);
    Check(LSnapshot.Field('name').AsText = UnicodeName, 'lazy MSAA query publishes supplementary text');
    Check(LSnapshot.Field('description').AsText = 'Independent edit help', 'lazy description is visible');
    LEdit.SetFocus;
    LEdit.SelStart := 3;
    LEdit.SelLength := 4;
    LEdit.AccessibleName := 'Updated edit name';
    SyncNyxLCLAccessibility(LEdit);
    Check(LEdit.Focused and (LEdit.SelStart = 3) and (LEdit.SelLength = 4),
      'metadata preserves focus, caret and selection');
    LSnapshot := Snapshot(LEdit.Handle);
    Check(LSnapshot.Field('value').AsText = 'Preserved draft', 'metadata preserves the actual input value');
    Check(LSnapshot.Field('name').AsText = 'Updated edit name', 'live metadata updates reach MSAA');
    LOldAccessible := ReadAccessible(LEdit.Handle);
    LEdit.Rebuild;
    LOldCode := LOldAccessible.GetName(LChild, LOldName);
    Check((LOldCode < 0) or (LOldName = ''), 'recreated-window clients expose no stale annotated name');
    LOldAccessible := ReadAccessible(LEdit.Handle);
    LSnapshot := Snapshot(LEdit.Handle);
    Check(LSnapshot.Field('name').AsText = 'Updated edit name', 'recreated handles publish their names');
    Check(LSnapshot.Field('description').AsText = 'Independent edit help',
      'recreated handles publish their descriptions');
    CheckExternal(LEdit.Handle, LSnapshot);
    LWorker := TWorkerProbe.Create(True);
    try
      LWorker.Control := LEdit;
      LWorker.Start;
      LWorker.WaitFor;
      Check(LWorker.Refused, 'worker publication refuses before touching the control');
    finally
      LWorker.Free;
    end;
    Check(Snapshot(LEdit.Handle).ToJSON = LSnapshot.ToJSON, 'worker refusal preserves native metadata/value');
    LEdit.AccessibleName := 'A name' + #0 + 'with an embedded NUL';
    Check(SyncNyxLCLAccessibility(LEdit) = nlasUnavailable,
      'a zero-terminated platform API reports unsupported NUL without truncating it');
    Check(Pos(#0, LEdit.AccessibleName) > 0, 'platform refusal preserves the caller metadata bytes');
    LEdit.AccessibleName := 'Updated edit name';
    Check(SyncNyxLCLAccessibility(LEdit) = nlasPublished, 'valid metadata resumes after platform refusal');
    LEdit.Free;
    LChild := VarAsType(Integer(0), varInteger);
    LOldCode := LOldAccessible.GetName(LChild, LOldName);
    Check((LOldCode < 0) or (LOldName = ''), 'retired native clients expose no stale annotated name');
    LOldAccessible := nil;
    LCanvasAccessible := nil;
    VarClear(LAutomation);
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'native metadata has no authored document authority');
    WriteLn('PASS ', GChecks, ' actual Win32 MSAA checks at width ', AWidth);
  finally
    LOldAccessible := nil;
    LView := nil;
    LRenderer.Free;
    LState.Free;
    LDocument.Free;
    LHost.Free;
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
    GFetch := TAccessibleFromWindow(GetProcedureAddress(LLibrary, 'AccessibleObjectFromWindow'));
    Check(Assigned(GFetch), 'the native MSAA entry point is available');

    if (ParamCount = 2) and (ParamStr(1) = '--read-window') then
    begin
      Check(TryStrToQWord(ParamStr(2), LWindow) and (LWindow <= High(HWND)), 'owned HWND is valid');
      WriteClientPacket(Snapshot(HWND(LWindow)));
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
