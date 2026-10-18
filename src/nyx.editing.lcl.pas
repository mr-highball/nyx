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

unit nyx.editing.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses Controls, nyx.text, nyx.editing, nyx.editing.commands, nyx.events, nyx.observation;

type
  { UI receiver borrows no input past invocation; copied snapshots own their text. }
  TNyxLCLEditingHandler = procedure(const AOriginID: TNyxText;
    const AEditing: TNyxEditingSnapshot) of object;
  { A view-owned observer borrows admitted text controls. Win32 IME messages
    chain the existing LCL WindowProc; final admission waits for UI idle because
    an IME may deliver its committed characters after ENDComposition.
    Text selection is coalesced at idle. There is no timer or guessed key intent.
    Disconnect restores producers before any borrowed control is destroyed. }
  INyxLCLEditingObserver = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001002000031}']
    procedure Add(const AOriginID: TNyxText; AControl: TWinControl);
    procedure Activate(const AEvents: INyxEvents; AHandler: TNyxLCLEditingHandler);
    procedure Disconnect;
    { True while the native control's previous procedure is on the stack. Its
      component must retire through the LCL release queue, not be freed inline. }
    function Dispatching: Boolean;
  end;

  { Optional capability preserves the original editing observer interface.
    A managed history authority attaches only to an admitted physical input;
    nil restores platform ownership and disconnect revokes the retained port. }
  INyxLCLTextHistoryObserver = interface(IInterface)
    ['{64938F47-A813-49A7-A539-2BC024048DD0}']
    procedure BindHistory(const AOriginID: TNyxText;
      const ACommands: INyxTextHistoryCommands);
  end;

function NyxLCLInputText(AInput: TWinControl): TNyxText;
{ Write an already admitted runtime text frame through Win32 Unicode text rather
  than LCL's line-ending normalizer. Mixed physical line endings remain exact.
  The owning renderer/controller must suppress its own change publication while
  restoring. Unsupported controls/widgetsets, invalid Unicode and NUL refuse. }
procedure RestoreNyxLCLInputText(AInput: TWinControl; const AText: TNyxText);
{ True only for the admitted Win32 Common Controls message bridge. Other
  widgetsets keep their ordinary editing behavior until separately qualified. }
function NyxLCLTextHistorySupported: Boolean;
function CaptureNyxLCLSelection(AInput: TWinControl): TNyxTextSelection;
function CaptureNyxLCLEditing(AInput: TWinControl; APhase: TNyxEditingPhase;
  AComposing: Boolean): TNyxEditingSnapshot;
procedure SelectNyxLCLText(AInput: TWinControl; const ASelection: TNyxTextSelection);
function NewNyxLCLEditingObserver: INyxLCLEditingObserver;

implementation

uses SysUtils, Classes, Forms, StdCtrls, LMessages, InterfaceBase, LCLPlatformDef,
  nyx.types
  {$ifdef WINDOWS}, Windows, CommCtrl{$endif};

type
  { Managed hook lease keeps the current message dispatch alive across callbacks.
    Its control/previous procedure are borrowed and revoked on disconnect. }
  INyxLCLEditHook = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001002000032}']
    procedure Connect(const AEvents: INyxEvents; AHandler: TNyxLCLEditingHandler);
    procedure Disconnect;
    procedure Poll;
    procedure PublishRevision(ARevision: Integer);
    function Dispatching: Boolean;
    function BindHistory(const AOriginID: TNyxText;
      const ACommands: INyxTextHistoryCommands): Boolean;
  end;
  { One borrowed LCL input/procedure chain. Win32 edit messages that bypass LCL
    use Common Controls SetWindowSubclass on the borrowed HWND. The same managed
    lease/dispatch depth owns retirement for both callbacks. No HWND is owned;
    NCDESTROY removes the subclass and ordinary rebinding/idle admits recreation. }
  TNyxLCLEditHook = class(TInterfacedObject, INyxLCLEditHook)
  private
    FID: TNyxText;
    FControl: TWinControl;
    FPrevious: TWndMethod;
    FEvents: INyxEvents;
    FHandler: TNyxLCLEditingHandler;
    FRevision: Integer;
    FConnected: Boolean;
    FHooked: Boolean;
    FComposing: Boolean;
    FEndPending: Boolean;
    FDispatchDepth: Integer;
    FSelection: TNyxTextSelection;
    FHistory: INyxTextHistoryCommands;
    {$ifdef WINDOWS}
    FHistoryWindow: HWND;
    procedure SyncHistorySubclass;
    procedure RemoveHistorySubclass;
    function HistoryMessage(AWindow: HWND; AMessage: UINT;
      AWParam: WPARAM; ALParam: LPARAM): LRESULT;
    function TryHistoryMessage(AMessage: Cardinal; out AResult: PtrInt): Boolean;
    {$endif}
    procedure WindowMessage(var AMessage: TLMessage);
    procedure Notify(const AEditing: TNyxEditingSnapshot);
  public
    constructor Create(const AID: TNyxText; AControl: TWinControl);
    destructor Destroy; override;
    procedure Connect(const AEvents: INyxEvents; AHandler: TNyxLCLEditingHandler);
    procedure Disconnect;
    procedure Poll;
    procedure PublishRevision(ARevision: Integer);
    function Dispatching: Boolean;
    function BindHistory(const AOriginID: TNyxText;
      const ACommands: INyxTextHistoryCommands): Boolean;
  end;
  { View-owned ordered hook collection. Disconnect retires all callbacks before
    any physical input is released; publication advances existing hook epochs. }
  TNyxLCLEditingObserver = class(TInterfacedObject, INyxLCLEditingObserver,
    INyxObservationPublication, INyxLCLTextHistoryObserver)
  private
    FHooks: array of INyxLCLEditHook;
    FEvents: INyxEvents;
    FRevision: Integer;
    FConnected: Boolean;
    procedure Idle(ASender: TObject; var ADone: Boolean);
  public
    destructor Destroy; override;
    procedure Add(const AOriginID: TNyxText; AControl: TWinControl);
    procedure Activate(const AEvents: INyxEvents; AHandler: TNyxLCLEditingHandler);
    procedure Disconnect;
    function Dispatching: Boolean;
    function GetReady: Boolean;
    procedure PublishRevision(ARevision: Integer);
    procedure BindHistory(const AOriginID: TNyxText;
      const ACommands: INyxTextHistoryCommands);
  end;

function NativeSelectionSupported: Boolean;
begin
  {$ifdef WINDOWS}
  Result := (WidgetSet <> nil) and (WidgetSet.LCLPlatform = lpWin32);
  {$else}
  Result := False;
  {$endif}
end;

function NyxLCLTextHistorySupported: Boolean;
begin
  Result := NativeSelectionSupported;
end;

{$ifdef WINDOWS}
{ Common Controls SUBCLASSPROC bridge. The reference data borrows an admitted
  managed hook until explicit removal/NCDESTROY, never an application or tree.
  Each callback acquires its own hook lease before forwarding into host code. }
function HistorySubclass(AWindow: HWND; AMessage: UINT; AWParam: WPARAM;
  ALParam: LPARAM; ASubclassID: UINT_PTR; AData: DWORD_PTR): LRESULT; stdcall;
var
  LHook: TNyxLCLEditHook;
  LKeepAlive: INyxLCLEditHook;
begin
  LHook := TNyxLCLEditHook(Pointer(AData));
  LKeepAlive := LHook;
  Inc(LHook.FDispatchDepth);
  try
    try
      Result := LHook.HistoryMessage(AWindow, AMessage, AWParam, ALParam);
    except
      { Pascal exceptions never cross the external OS callback. Keep the hook
        leased/dispatching while the ordinary application error handler runs. }
      Application.HandleException(Application);
      Result := 0;
    end;
  finally
    Dec(LHook.FDispatchDepth);
  end;
end;

procedure TNyxLCLEditHook.RemoveHistorySubclass;
var
  LWindow: HWND;
begin
  LWindow := FHistoryWindow;
  FHistoryWindow := 0;

  if LWindow <> 0 then
  begin
    CommCtrl.RemoveWindowSubclass(LWindow, @HistorySubclass, UINT_PTR(Self));
  end;
end;

procedure TNyxLCLEditHook.SyncHistorySubclass;
begin

  if not FConnected or (FHistory = nil) or not NativeSelectionSupported then
  begin
    RemoveHistorySubclass;
    Exit;
  end;

  if not FControl.HandleAllocated then
  begin
    RemoveHistorySubclass;
    Exit;
  end;

  if FHistoryWindow = FControl.Handle then
  begin
    Exit;
  end;
  RemoveHistorySubclass;

  if not CommCtrl.SetWindowSubclass(FControl.Handle, @HistorySubclass,
    UINT_PTR(Self), DWORD_PTR(Self)) then
  begin
    raise EArgumentException.Create('This native input cannot admit owned text history');
  end;
  FHistoryWindow := FControl.Handle;
end;

function TNyxLCLEditHook.TryHistoryMessage(AMessage: Cardinal;
  out AResult: PtrInt): Boolean;
var
  LHistory: INyxTextHistoryCommands;
  LEditable: Boolean;
begin
  Result := (FHistory <> nil) and ((AMessage = WM_UNDO) or
    (AMessage = EM_UNDO) or (AMessage = EM_CANUNDO));
  AResult := 0;

  if not Result then
  begin
    Exit;
  end;
  LHistory := FHistory;
  LEditable := FConnected and (FEvents.ViewRevision = FRevision) and
    not FComposing and not FEndPending and FControl.Enabled and
    not TCustomEdit(FControl).ReadOnly;

  if LEditable then
  begin

    if AMessage <> EM_CANUNDO then
    begin
      LHistory.BeforeEdit(CaptureNyxLCLEditing(FControl, nepBeforeEdit, FComposing));
    end;
    AResult := Ord(LHistory.CanExecute(nhcUndo));

    if AMessage <> EM_CANUNDO then
    begin
      LHistory.Execute(nhcUndo);
    end;
  end;
end;

function TNyxLCLEditHook.HistoryMessage(AWindow: HWND; AMessage: UINT;
  AWParam: WPARAM; ALParam: LPARAM): LRESULT;
var
  LKeepAlive: INyxLCLEditHook;
  LResult: PtrInt;
  LHistory: INyxTextHistoryCommands;
begin
  LKeepAlive := Self;
  Inc(FDispatchDepth);
  try

    if AMessage = WM_NCDESTROY then
    begin
      RemoveHistorySubclass;
    end
    else if TryHistoryMessage(AMessage, LResult) then
    begin
      Exit(LResult);
    end;
    LHistory := FHistory;

    if FConnected and (FEvents.ViewRevision = FRevision) and (LHistory <> nil) and
      ((AMessage = WM_CHAR) or (AMessage = WM_KEYDOWN) or (AMessage = WM_PASTE) or
      (AMessage = WM_CUT) or (AMessage = WM_CLEAR) or (AMessage = EM_REPLACESEL)) then
    begin
      LHistory.BeforeEdit(CaptureNyxLCLEditing(FControl, nepBeforeEdit, FComposing));
    end;
    Result := CommCtrl.DefSubclassProc(AWindow, AMessage, AWParam, ALParam);
  finally
    Dec(FDispatchDepth);
  end;
end;
{$endif}

function NyxLCLInputText(AInput: TWinControl): TNyxText;
begin

  if not (AInput is TCustomEdit) then
  begin
    raise EArgumentException.Create('Editing requires a mounted LCL text input');
  end;
  Result := TCustomEdit(AInput).Text;
end;

procedure RestoreNyxLCLInputText(AInput: TWinControl; const AText: TNyxText);
{$ifdef WINDOWS}
var
  LText: UnicodeString;
{$endif}
begin

  if not NativeSelectionSupported or (AInput = nil) or
    not (AInput is TCustomEdit) or not AInput.HandleAllocated or (Pos(#0, AText) > 0) then
  begin
    raise EArgumentException.Create('Exact text restore requires an admitted native text input without NUL');
  end;
  NyxTextScalarCount(AText);
  {$ifdef WINDOWS}
  LText := UTF8Decode(AText);

  if not Windows.SetWindowTextW(AInput.Handle, PWideChar(LText)) then
  begin
    raise EArgumentException.Create('The native input refused its exact text frame');
  end;
  {$endif}
end;

function CaptureNyxLCLSelection(AInput: TWinControl): TNyxTextSelection;
{$ifdef WINDOWS}
var
  LStart: DWORD;
  LFinish: DWORD;
{$endif}
begin
  Result := Default(TNyxTextSelection);

  if (AInput = nil) or not (AInput is TCustomEdit) or
    not AInput.HandleAllocated or not NativeSelectionSupported then
  begin
    Exit;
  end;
  {$ifdef WINDOWS}
  LStart := 0;
  LFinish := 0;
  Windows.SendMessageW(AInput.Handle, EM_GETSEL, Windows.WPARAM(@LStart),
    Windows.LPARAM(@LFinish));
  try
    { Win32 uses UTF-16 regardless of the older LCL SelStart UTF-8 comment.
      The physical Text value and range share their exact line endings. }
    Result := NyxTextSelectionUTF16(NyxLCLInputText(AInput), Integer(LStart),
      Integer(LFinish), ntdUnknown);
  except
    on LException: EArgumentException do
    begin
      Result := Default(TNyxTextSelection);
    end;
  end;
  {$endif}
end;

function CaptureNyxLCLEditing(AInput: TWinControl; APhase: TNyxEditingPhase;
  AComposing: Boolean): TNyxEditingSnapshot;
begin
  Result := NyxEditingSnapshot(APhase, neiUnknown, NyxLCLInputText(AInput),
    '', False, CaptureNyxLCLSelection(AInput), AComposing, False);
end;

procedure SelectNyxLCLText(AInput: TWinControl; const ASelection: TNyxTextSelection);
var
  LText: TNyxText;
begin

  if not ASelection.Defined or (AInput = nil) or not (AInput is TCustomEdit) or
    not AInput.HandleAllocated or not NativeSelectionSupported then
  begin
    raise EArgumentException.Create('This widgetset has no admitted scalar selection bridge');
  end;
  { Reading a current scalar range is not a capability check for writing one.
    After native handle recreation, a transient physical endpoint can lie
    inside a surrogate pair and Capture deliberately returns Undefined. The
    requested scalar range is independently validated against current Text
    below before EM_SETSEL; no malformed range or unsupported widgetset enters. }

  if ASelection.Direction in [ntdForward, ntdBackward] then
  begin
    { This bridge cannot qualify a requested active endpoint through standard
      LCL slots. Refuse before changing the range instead of discarding intent. }
    raise EArgumentException.Create('Native selection direction requires an endpoint bridge; use None or Unknown');
  end;
  LText := NyxLCLInputText(AInput);
  NyxTextSelection(LText, ASelection.Start, ASelection.Finish, ASelection.Direction);
  {$ifdef WINDOWS}
  { Direct selection avoids LCL's additional EM_SCROLLCARET side effect. This
    API never focuses the control or moves the editor's containing viewport. }
  Windows.SendMessageW(AInput.Handle, EM_SETSEL,
    NyxTextUTF16Offset(LText, ASelection.Start),
    NyxTextUTF16Offset(LText, ASelection.Finish));
  {$endif}
end;

{$ifdef WINDOWS}
const
  { imm.h flags are not exported by the matched FPC Windows unit. These stable
    OS boundary constants select preedit and committed UTF-16 string data. }
  CCompositionString = $0008;
  CCompositionResult = $0800;

function NyxImmGetContext(AWindow: HWND): THandle; stdcall;
  external 'imm32.dll' name 'ImmGetContext';
function NyxImmReleaseContext(AWindow: HWND; AContext: THandle): BOOL; stdcall;
  external 'imm32.dll' name 'ImmReleaseContext';
function NyxImmGetCompositionString(AContext: THandle; AIndex: DWORD;
  ABuffer: Pointer; ASize: DWORD): LongInt; stdcall;
  external 'imm32.dll' name 'ImmGetCompositionStringW';

function ReadComposition(AControl: TWinControl; AFlags: PtrInt;
  out AData: TNyxText): Boolean;
var
  LContext: THandle;
  LBytes: LongInt;
  LRead: LongInt;
  LWide: UnicodeString;
  LIndex: DWORD;
begin
  AData := '';
  Result := False;
  LIndex := CCompositionString;

  if (AFlags and CCompositionResult) <> 0 then
  begin
    LIndex := CCompositionResult;
  end
  else if (AFlags and CCompositionString) = 0 then
  begin
    Exit;
  end;
  LContext := NyxImmGetContext(AControl.Handle);

  if LContext = 0 then
  begin
    Exit;
  end;
  try
    LBytes := NyxImmGetCompositionString(LContext, LIndex, nil, 0);

    if (LBytes < 0) or ((LBytes mod SizeOf(WideChar)) <> 0) then
    begin
      Exit;
    end;

    if LBytes = 0 then
    begin
      Exit(True);
    end;
    SetLength(LWide, LBytes div SizeOf(WideChar));
    LRead := NyxImmGetCompositionString(LContext, LIndex, @LWide[1], LBytes);

    if LRead <> LBytes then
    begin
      Exit;
    end;
    AData := UTF8Encode(LWide);
    NyxTextScalarCount(AData);
    Result := True;
  finally
    NyxImmReleaseContext(AControl.Handle, LContext);
  end;
end;
{$endif}

constructor TNyxLCLEditHook.Create(const AID: TNyxText; AControl: TWinControl);
begin
  inherited Create;
  FID := AID;
  FControl := AControl;
end;

destructor TNyxLCLEditHook.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

procedure TNyxLCLEditHook.Connect(const AEvents: INyxEvents;
  AHandler: TNyxLCLEditingHandler);
begin
  FEvents := AEvents;
  FRevision := AEvents.ViewRevision;
  FHandler := AHandler;
  FSelection := CaptureNyxLCLSelection(FControl);
  FConnected := True;

  if NativeSelectionSupported then
  begin
    FPrevious := FControl.WindowProc;
    FControl.WindowProc := WindowMessage;
    FHooked := True;
  end;
end;

procedure TNyxLCLEditHook.PublishRevision(ARevision: Integer);
begin
  FRevision := ARevision;
end;

procedure TNyxLCLEditHook.Disconnect;
var
  LMethod: TWndMethod;
begin
  FConnected := False;
  {$ifdef WINDOWS}
  RemoveHistorySubclass;
  {$endif}
  FHistory := nil;

  if FHooked then
  begin
    LMethod := WindowMessage;

    if (TMethod(FControl.WindowProc).Code = TMethod(LMethod).Code) and
      (TMethod(FControl.WindowProc).Data = TMethod(LMethod).Data) then
    begin
      FControl.WindowProc := FPrevious;
    end;
  end;
  FHooked := False;
  FPrevious := nil;
  FHandler := nil;
  FControl := nil;
  FEvents := nil;
end;

function TNyxLCLEditHook.Dispatching: Boolean;
begin
  Result := FDispatchDepth > 0;
end;

function TNyxLCLEditHook.BindHistory(const AOriginID: TNyxText;
  const ACommands: INyxTextHistoryCommands): Boolean;
begin
  Result := FConnected and (FID = AOriginID);

  if Result then
  begin

    if (ACommands <> nil) and not NativeSelectionSupported then
    begin
      raise EArgumentException.Create('This widgetset has no admitted text history message bridge');
    end;
    FHistory := ACommands;
    {$ifdef WINDOWS}
    try
      SyncHistorySubclass;
    except
      FHistory := nil;
      RemoveHistorySubclass;
      raise;
    end;
    {$endif}
  end;
end;

procedure TNyxLCLEditHook.Notify(const AEditing: TNyxEditingSnapshot);
begin

  if FConnected and (FEvents.ViewRevision = FRevision) then
  begin
    FHandler(FID, AEditing);
  end;
end;

procedure TNyxLCLEditHook.WindowMessage(var AMessage: TLMessage);
var
  LKeepAlive: INyxLCLEditHook;
  LMessageID: Cardinal;
  {$ifdef WINDOWS}
  LData: TNyxText;
  LHasData: Boolean;
  LResult: PtrInt;
  {$endif}
begin
  LKeepAlive := Self;
  LMessageID := AMessage.Msg;
  Inc(FDispatchDepth);
  try

    if not FConnected then
    begin
      Exit;
    end;
    {$ifdef WINDOWS}

    if TryHistoryMessage(LMessageID, LResult) then
    begin
      AMessage.Result := LResult;
      Exit;
    end;

    if LMessageID = WM_IME_STARTCOMPOSITION then
    begin
      FComposing := True;
      FEndPending := False;
      Notify(NyxEditingSnapshot(nepCompositionStart, neiInsertCompositionText,
        NyxLCLInputText(FControl), '', False, CaptureNyxLCLSelection(FControl), True, False));

      if not FConnected then
      begin
        Exit;
      end;
    end;
    LData := '';
    LHasData := False;

    if LMessageID = WM_IME_COMPOSITION then
    begin
      LHasData := ReadComposition(FControl, AMessage.LParam, LData);
    end;
    {$endif}
    FPrevious(AMessage);

    if not FConnected then
    begin
      Exit;
    end;
    {$ifdef WINDOWS}

    if (LMessageID = WM_IME_COMPOSITION) and FComposing then
    begin
      Notify(NyxEditingSnapshot(nepCompositionUpdate, neiInsertCompositionText,
        NyxLCLInputText(FControl), LData, LHasData,
        CaptureNyxLCLSelection(FControl), True, False));
    end
    else if (LMessageID = WM_IME_ENDCOMPOSITION) and FComposing then
    begin
      { END is a genuine OS notification, but committed WM_CHAR messages may
        follow it. Preserve the draft guard until idle observes the final text. }
      FEndPending := True;
    end;
    {$endif}
  finally
    Dec(FDispatchDepth);
  end;
end;

procedure TNyxLCLEditHook.Poll;
var
  LKeepAlive: INyxLCLEditHook;
  LSelection: TNyxTextSelection;
  LEditing: TNyxEditingSnapshot;
begin
  LKeepAlive := Self;

  if not FConnected or (FEvents.ViewRevision <> FRevision) then
  begin
    Exit;
  end;
  {$ifdef WINDOWS}
  SyncHistorySubclass;
  {$endif}

  if FEndPending then
  begin
    FEndPending := False;
    FComposing := False;
    LEditing := NyxEditingSnapshot(nepCompositionEnd, neiInsertCompositionText,
      NyxLCLInputText(FControl), '', False, CaptureNyxLCLSelection(FControl), False, False);
    Notify(LEditing);

    if not FConnected then
    begin
      Exit;
    end;
  end;

  if not FEvents.HasSubscribers(ntTextSelectionChange) then
  begin
    Exit;
  end;
  LSelection := CaptureNyxLCLSelection(FControl);

  if not LSelection.Defined or FSelection.SameRange(LSelection) then
  begin
    Exit;
  end;
  FSelection := LSelection;
  Notify(CaptureNyxLCLEditing(FControl, nepSelectionChange, FComposing));
end;

function NewNyxLCLEditingObserver: INyxLCLEditingObserver;
begin
  Result := TNyxLCLEditingObserver.Create;
end;

destructor TNyxLCLEditingObserver.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

function TNyxLCLEditingObserver.Dispatching: Boolean;
var
  LIndex: Integer;
begin
  Result := False;
  for LIndex := 0 to High(FHooks) do
  begin

    if FHooks[LIndex].Dispatching then
    begin
      Exit(True);
    end;
  end;
end;

procedure TNyxLCLEditingObserver.Add(const AOriginID: TNyxText; AControl: TWinControl);
var
  LIndex: Integer;
begin

  if FConnected or (AControl = nil) or not (AControl is TCustomEdit) then
  begin
    raise EArgumentException.Create('Register LCL text inputs before observer admission');
  end;
  LIndex := Length(FHooks);
  SetLength(FHooks, LIndex + 1);
  FHooks[LIndex] := TNyxLCLEditHook.Create(AOriginID, AControl);
end;

procedure TNyxLCLEditingObserver.BindHistory(const AOriginID: TNyxText;
  const ACommands: INyxTextHistoryCommands);
var
  LIndex: Integer;
begin

  if not FConnected or (FEvents.ViewRevision <> FRevision) then
  begin
    raise EArgumentException.Create('Text history requires the current admitted input mount');
  end;
  FEvents.Scheduler.RequireUI;
  for LIndex := 0 to High(FHooks) do
  begin

    if FHooks[LIndex].BindHistory(AOriginID, ACommands) then
    begin
      Exit;
    end;
  end;
  raise EArgumentException.Create('Text history input is not mounted');
end;

procedure TNyxLCLEditingObserver.Activate(const AEvents: INyxEvents;
  AHandler: TNyxLCLEditingHandler);
var
  LIndex: Integer;
begin

  if FConnected or (AEvents = nil) or not Assigned(AHandler) then
  begin
    raise EArgumentException.Create('Editing observation requires an admitted UI owner');
  end;
  AEvents.Scheduler.RequireUI;
  FEvents := AEvents;
  FRevision := AEvents.ViewRevision;
  for LIndex := 0 to High(FHooks) do
  begin
    FHooks[LIndex].Connect(AEvents, AHandler);
  end;
  FConnected := True;
  Application.AddOnIdleHandler(Idle);
end;

function TNyxLCLEditingObserver.GetReady: Boolean;
begin
  Result := FConnected and (FEvents <> nil);
end;

procedure TNyxLCLEditingObserver.PublishRevision(ARevision: Integer);
var
  LIndex: Integer;
begin
  { The connected WindowProc chain stays intact. Only its admitted epoch changes;
    publication allocates nothing and enters no widget, scheduler or receiver. }
  FRevision := ARevision;
  for LIndex := 0 to High(FHooks) do
  begin
    FHooks[LIndex].PublishRevision(ARevision);
  end;
end;

procedure TNyxLCLEditingObserver.Disconnect;
var
  LIndex: Integer;
begin

  if FConnected then
  begin
    Application.RemoveOnIdleHandler(Idle);
  end;
  FConnected := False;
  for LIndex := 0 to High(FHooks) do
  begin
    FHooks[LIndex].Disconnect;
  end;
  FHooks := nil;
  FEvents := nil;
end;

procedure TNyxLCLEditingObserver.Idle(ASender: TObject; var ADone: Boolean);
var
  LKeepAlive: INyxLCLEditingObserver;
  LEvents: INyxEvents;
  LIndex: Integer;
begin
  LKeepAlive := Self;
  LEvents := FEvents;

  if not FConnected or (LEvents = nil) or (LEvents.ViewRevision <> FRevision) then
  begin
    Exit;
  end;
  for LIndex := 0 to High(FHooks) do
  begin
    FHooks[LIndex].Poll;

    if not FConnected or (LEvents.ViewRevision <> FRevision) then
    begin
      Exit;
    end;
  end;
end;

end.
