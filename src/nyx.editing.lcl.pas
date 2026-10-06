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

uses Controls, nyx.text, nyx.editing, nyx.events;

type
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

function NyxLCLInputText(AInput: TWinControl): TNyxText;
function CaptureNyxLCLSelection(AInput: TWinControl): TNyxTextSelection;
function CaptureNyxLCLEditing(AInput: TWinControl; APhase: TNyxEditingPhase;
  AComposing: Boolean): TNyxEditingSnapshot;
procedure SelectNyxLCLText(AInput: TWinControl; const ASelection: TNyxTextSelection);
function NewNyxLCLEditingObserver: INyxLCLEditingObserver;

implementation

uses SysUtils, Classes, Forms, StdCtrls, LMessages, InterfaceBase, LCLPlatformDef,
  nyx.types
  {$ifdef WINDOWS}, Windows{$endif};

type
  INyxLCLEditHook = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001002000032}']
    procedure Connect(const AEvents: INyxEvents; AHandler: TNyxLCLEditingHandler);
    procedure Disconnect;
    procedure Poll;
    function Dispatching: Boolean;
  end;
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
    procedure WindowMessage(var AMessage: TLMessage);
    procedure Notify(const AEditing: TNyxEditingSnapshot);
  public
    constructor Create(const AID: TNyxText; AControl: TWinControl);
    destructor Destroy; override;
    procedure Connect(const AEvents: INyxEvents; AHandler: TNyxLCLEditingHandler);
    procedure Disconnect;
    procedure Poll;
    function Dispatching: Boolean;
  end;
  TNyxLCLEditingObserver = class(TInterfacedObject, INyxLCLEditingObserver)
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
  end;

function NativeSelectionSupported: Boolean;
begin
  {$ifdef WINDOWS}
  Result := (WidgetSet <> nil) and (WidgetSet.LCLPlatform = lpWin32);
  {$else}
  Result := False;
  {$endif}
end;

function NyxLCLInputText(AInput: TWinControl): TNyxText;
begin

  if not (AInput is TCustomEdit) then
  begin
    raise EArgumentException.Create('Editing requires a mounted LCL text input');
  end;
  Result := TCustomEdit(AInput).Text;
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

procedure TNyxLCLEditHook.Disconnect;
var
  LMethod: TWndMethod;
begin
  FConnected := False;

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
