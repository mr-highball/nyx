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
unit nyx.clipboard.browser;

{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch externalclass}

interface

uses Web, nyx.clipboard;

type
  { Required admits only a currently observed transient gesture. BrowserPermission
    delegates gesture/permission policy to the actual Clipboard API, permitting
    permission-backed browsers without a userActivation capability. API refusal
    remains Denied; this choice never manufactures or requests permission. }
  TNyxBrowserClipboardActivation = (nbcaRequired, nbcaBrowserPermission);

const
  { The web prefix selects the optional lossless custom-format channel. Blob
    bytes are strictly admitted UTF-8, with no BOM, JSON or newline conversion. }
  NyxBrowserClipboardExactMediaType = 'application/vnd.nyx.text';
  NyxBrowserClipboardExactFormat = 'web application/vnd.nyx.text';

{ Explicit runtime authority for one browser Window. Exact is the default:
  multiline/NUL text requires the optional custom UTF-8 representation. NUL
  uses that channel alone; no-NUL text also supplies interoperable text/plain.
  Unsupported formats refuse without a weaker retry.
  PlatformText permits OS line-ending normalization and supports full multiline
  copy, but refuses NUL rather than risking truncation. Each result names its
  actual representation. No operation reads the clipboard, focuses a control,
  changes selection, installs global listeners or uses execCommand.
  Current activation is required by default; BrowserPermission explicitly lets
  the API enforce its own permission policy. The Window overload supports iframe
  hosts; its environment must remain usable until each operation settles. }
function NewNyxBrowserClipboardWriteHost(
  AMode: TNyxClipboardTextMode = nctmExact;
  AActivation: TNyxBrowserClipboardActivation = nbcaRequired): INyxClipboardWriteHost; overload;
function NewNyxBrowserClipboardWriteHost(AWindow: TJSWindow;
  AMode: TNyxClipboardTextMode = nctmExact;
  AActivation: TNyxBrowserClipboardActivation = nbcaRequired): INyxClipboardWriteHost; overload;

implementation

uses SysUtils, JS, nyx.text, nyx.bytes, nyx.scheduler;

type
  { Reflect.construct invokes the ClipboardItem constructor captured from the
    borrowed target Window, including an iframe realm. This standard ECMAScript
    bridge creates no authority and requires an already-admitted constructor. }
  TClipboardReflect = class external name 'Reflect'(TJSObject)
    class function construct(ATarget: TJSFunction;
      const AArguments: array of JSValue): TJSObject; static;
  end;

  { Forward concrete request is borrowed only beneath managed request leases. }
  TBrowserClipboardRequest = class;

  { One scheduler-owned completion carries copied result facts and an exact
    managed request. A skipped work item silently cancels the request during
    final release; execution clears its borrowed pointer before any client call. }
  TClipboardDelivery = class(TInterfacedObject, INyxWork)
  private
    FOwner: TBrowserClipboardRequest;
    FRequest: INyxClipboardWriteRequest;
    FOutcome: TNyxClipboardWriteOutcome;
    FBytes: Integer;
    FError: TNyxText;
    FRepresentation: TNyxClipboardTextRepresentation;
    FEntered: Boolean;
  public
    constructor Create(AOwner: TBrowserClipboardRequest;
      AOutcome: TNyxClipboardWriteOutcome; ABytes: Integer;
      const AError: TNyxText; ARepresentation: TNyxClipboardTextRepresentation);
    destructor Destroy; override;
    procedure Execute(const AExecution: INyxExecution);
    property Entered: Boolean read FEntered;
  end;

  { Each async invocation holds its own managed request through platform promise
    settlement. Request.Cancel withdraws its borrowed reply/payload immediately;
    it cannot undo an OS write or abort the Clipboard API promise. Completion is
    marshaled through the supplied UI scheduler, never a retained widget/tree. }
  TBrowserClipboardRequest = class(TNyxClipboardWriteRequest)
  private
    FWindow: TJSWindow;
    FMode: TNyxClipboardTextMode;
    FActivation: TNyxBrowserClipboardActivation;
    FDelivery: INyxExecution;
    function BeginPlatform(out AOutcome: TNyxClipboardWriteOutcome;
      out AError: TNyxText;
      out ARepresentation: TNyxClipboardTextRepresentation): TJSPromise;
    function AwaitPlatform(APromise: TJSPromise; ABytes: Integer;
      ARepresentation: TNyxClipboardTextRepresentation): JSValue; async;
    procedure Queue(AOutcome: TNyxClipboardWriteOutcome; ABytes: Integer;
      const AError: TNyxText; ARepresentation: TNyxClipboardTextRepresentation);
    procedure Deliver(AOutcome: TNyxClipboardWriteOutcome; ABytes: Integer;
      const AError: TNyxText; ARepresentation: TNyxClipboardTextRepresentation);
  protected
    procedure EndHost; override;
  public
    constructor Create(AWindow: TJSWindow; AMode: TNyxClipboardTextMode;
      AActivation: TNyxBrowserClipboardActivation;
      const AText: TNyxText; const AScheduler: INyxScheduler;
      const AScope: INyxCancellationScope; AReply: TNyxClipboardWriteReply);
    procedure Start;
  end;

  { A host retains only its explicit browser environment and closed text policy.
    Every write has an independent payload/reply/cancellation lifetime. No host
    holds an active request or reconstructs permission from document content. }
  TBrowserClipboardHost = class(TInterfacedObject, INyxClipboardWriteHost)
  private
    FWindow: TJSWindow;
    FMode: TNyxClipboardTextMode;
    FActivation: TNyxBrowserClipboardActivation;
  public
    constructor Create(AWindow: TJSWindow; AMode: TNyxClipboardTextMode;
      AActivation: TNyxBrowserClipboardActivation);
    function BeginWriteText(const AText: TNyxText;
      const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
      AReply: TNyxClipboardWriteReply): INyxClipboardWriteRequest;
  end;

{ Normalize fulfillment/rejection BEFORE await: raw platform values need not be
  Pascal exceptions, and null/undefined must still select the rejected branch. }
function Accepted(AValue: JSValue): JSValue;
begin
  Result := TJSObject.new;
  TJSObject(Result)['ok'] := True;
end;

function Rejected(AValue: JSValue): JSValue;
begin
  Result := TJSObject.new;
  TJSObject(Result)['ok'] := False;
  TJSObject(Result)['error'] := AValue;
end;

function FailureOutcome(AValue: JSValue; out AError: TNyxText): TNyxClipboardWriteOutcome;
var
  LName: JSValue;
begin
  Result := ncwoFailed;
  AError := 'The browser could not complete the clipboard write';
  try

    if not isObject(AValue) then
    begin
      Exit;
    end;
    LName := TJSObject(AValue)['name'];

    if not isString(LName) then
    begin
      Exit;
    end;

    if (String(LName) = 'NotAllowedError') or (String(LName) = 'SecurityError') then
    begin
      Result := ncwoDenied;
      AError := 'The browser denied clipboard access';
    end
    else if String(LName) = 'AbortError' then
    begin
      Result := ncwoCancelled;
      AError := '';
    end
    else if String(LName) = 'NotSupportedError' then
    begin
      Result := ncwoUnavailable;
      AError := 'This browser does not support the requested clipboard representation';
    end;
  except
    { Host error properties are untrusted. No toString/getter failure may revive
      a request or replace its bounded operational diagnostic. }
  end;
end;

constructor TClipboardDelivery.Create(AOwner: TBrowserClipboardRequest;
  AOutcome: TNyxClipboardWriteOutcome; ABytes: Integer; const AError: TNyxText;
  ARepresentation: TNyxClipboardTextRepresentation);
begin
  inherited Create;
  FRequest := AOwner;
  FOwner := AOwner;
  FOutcome := AOutcome;
  FBytes := ABytes;
  FError := AError;
  FRepresentation := ARepresentation;
end;

destructor TClipboardDelivery.Destroy;
var
  LRequest: INyxClipboardWriteRequest;
begin
  LRequest := FRequest;
  FOwner := nil;
  FRequest := nil;
  try

    if LRequest <> nil then
    begin
      LRequest.Cancel;
    end;
  except
    { A skipped delivery cannot report from destruction or resurrect a reply. }
  end;
  inherited Destroy;
end;

procedure TClipboardDelivery.Execute(const AExecution: INyxExecution);
var
  LRequest: INyxClipboardWriteRequest;
  LOwner: TBrowserClipboardRequest;
begin
  FEntered := True;
  LRequest := FRequest;
  LOwner := FOwner;
  FOwner := nil;
  FRequest := nil;

  if (LRequest = nil) or (LOwner = nil) then
  begin
    Exit;
  end;

  if (AExecution = nil) or AExecution.Cancelled then
  begin
    LRequest.Cancel;
    Exit;
  end;
  LOwner.Deliver(FOutcome, FBytes, FError, FRepresentation);
end;

constructor TBrowserClipboardRequest.Create(AWindow: TJSWindow;
  AMode: TNyxClipboardTextMode; AActivation: TNyxBrowserClipboardActivation;
  const AText: TNyxText;
  const AScheduler: INyxScheduler; const AScope: INyxCancellationScope;
  AReply: TNyxClipboardWriteReply);
var
  LWindow: TJSWindow;
begin
  LWindow := AWindow;
  inherited Create(AText, AScheduler, AScope, AReply);
  FWindow := LWindow;
  FMode := AMode;
  FActivation := AActivation;
end;

procedure TBrowserClipboardRequest.EndHost;
var
  LDelivery: INyxExecution;
begin
  LDelivery := FDelivery;
  FDelivery := nil;
  FWindow := nil;

  if LDelivery <> nil then
  begin
    LDelivery.Cancel;
  end;
end;

function TBrowserClipboardRequest.BeginPlatform(
  out AOutcome: TNyxClipboardWriteOutcome; out AError: TNyxText;
  out ARepresentation: TNyxClipboardTextRepresentation): TJSPromise;
var
  LWindow: TJSWindow;
  LNavigator: JSValue;
  LClipboard: JSValue;
  LActivation: JSValue;
  LValue: JSValue;
  LWrite: TJSFunction;
  LConstructor: JSValue;
  LSupports: JSValue;
  LData: TJSObject;
  LItem: TJSObject;
  LOptions: TJSBlobInit;
  LArray: TJSUint8Array;
  LBytes: TNyxBytes;
  LText: TNyxText;
  LIndex: Integer;
  LRich: Boolean;
begin
  Result := nil;
  AOutcome := ncwoUnavailable;
  AError := 'This browser context does not provide secure clipboard access';
  ARepresentation := nctrNone;
  LWindow := FWindow;
  LText := FText;

  if not Pending or (LWindow = nil) then
  begin
    Exit;
  end;
  LValue := TJSObject(LWindow)['isSecureContext'];

  if not Pending or not isBoolean(LValue) or not Boolean(LValue) then
  begin
    Exit;
  end;
  LNavigator := TJSObject(LWindow)['navigator'];

  if not Pending or not isObject(LNavigator) then
  begin
    Exit;
  end;
  LClipboard := TJSObject(LNavigator)['clipboard'];

  if not Pending or not isObject(LClipboard) then
  begin
    Exit;
  end;

  if FActivation = nbcaRequired then
  begin
    LActivation := TJSObject(LNavigator)['userActivation'];

    if not Pending or not isObject(LActivation) then
    begin
      AError := 'This browser does not expose current clipboard user activation';
      Exit;
    end;
    LValue := TJSObject(LActivation)['isActive'];

    if not Pending then
    begin
      Exit;
    end;

    if not isBoolean(LValue) or not Boolean(LValue) then
    begin
      AOutcome := ncwoActivationRequired;
      AError := 'Choose Copy from a current user action';
      Exit;
    end;
  end;
  LRich := (FMode = nctmExact) and
    ((Pos(#0, LText) > 0) or (Pos(#13, LText) > 0) or (Pos(#10, LText) > 0));

  if LRich then
  begin
    AError := 'Exact multiline or NUL copy requires the browser custom UTF-8 clipboard format';
    LValue := TJSObject(LClipboard)['write'];

    if not Pending or not isFunction(LValue) then
    begin
      Exit;
    end;
    LWrite := TJSFunction(LValue);
    LConstructor := TJSObject(LWindow)['ClipboardItem'];

    if not Pending or not isFunction(LConstructor) then
    begin
      Exit;
    end;
    LSupports := TJSObject(LConstructor)['supports'];

    if not Pending or not isFunction(LSupports) then
    begin
      Exit;
    end;
    LValue := TJSFunction(LSupports).call(TJSObject(LConstructor), NyxBrowserClipboardExactFormat);

    if not Pending or not isBoolean(LValue) or not Boolean(LValue) then
    begin
      Exit;
    end;
    LBytes := NyxEncodeUTF8(LText);
    LArray := TJSUint8Array.new(Length(LBytes));
    for LIndex := 0 to High(LBytes) do
    begin
      LArray[LIndex] := LBytes[LIndex];
    end;
    LData := TJSObject.new;
    LOptions := TJSBlobInit.new;
    LOptions.type_ := NyxBrowserClipboardExactMediaType;
    LData[NyxBrowserClipboardExactFormat] := TJSBlob.new([LArray.buffer], LOptions);
    ARepresentation := nctrExactUTF8;

    if Pos(#0, LText) = 0 then
    begin
      LOptions := TJSBlobInit.new;
      LOptions.type_ := 'text/plain';
      LData['text/plain'] := TJSBlob.new([LArray.buffer], LOptions);
      ARepresentation := nctrExactUTF8WithPlatformText;
    end;
    LItem := TClipboardReflect.construct(TJSFunction(LConstructor), [LData]);

    if not Pending then
    begin
      Exit;
    end;
    LValue := LWrite.call(TJSObject(LClipboard), TJSArray.new(LItem));
  end
  else
  begin

    if (FMode = nctmPlatformText) and (Pos(#0, LText) > 0) then
    begin
      AError := 'Platform text copy cannot guarantee NUL preservation; choose exact copy';
      Exit;
    end;
    LValue := TJSObject(LClipboard)['writeText'];

    if not Pending or not isFunction(LValue) then
    begin
      Exit;
    end;
    LWrite := TJSFunction(LValue);

    if not Pending then
    begin
      Exit;
    end;
    LValue := LWrite.call(TJSObject(LClipboard), LText);
    ARepresentation := nctrExactPlainText;

    if FMode = nctmPlatformText then
    begin
      ARepresentation := nctrPlatformText;
    end;
  end;
  AOutcome := ncwoFailed;
  AError := 'The browser clipboard API did not return a completion promise';

  if not isObject(LValue) or not isFunction(TJSObject(LValue)['then']) then
  begin
    Exit;
  end;
  { Promise.resolve assimilates a thenable once and defers even inline hosts.
    No custom then callback receives an unretained Pascal object. }
  Result := TJSPromise.resolve(LValue);
end;

procedure TBrowserClipboardRequest.Queue(AOutcome: TNyxClipboardWriteOutcome;
  ABytes: Integer; const AError: TNyxText;
  ARepresentation: TNyxClipboardTextRepresentation);
var
  LLease: INyxClipboardWriteRequest;
  LWork: INyxWork;
  LExecution: INyxExecution;
  LScheduler: INyxScheduler;
  LDelivery: TClipboardDelivery;
  LEntered: Boolean;
begin
  LLease := Self;
  LScheduler := FScheduler;
  LDelivery := nil;
  try

    if not Pending then
    begin
      Cancel;
      Exit;
    end;
    LDelivery := TClipboardDelivery.Create(Self, AOutcome, ABytes, AError, ARepresentation);
    LWork := LDelivery;
    LExecution := LScheduler.PostUI(LWork);

    if Pending and (LExecution <> nil) then
    begin
      FDelivery := LExecution;
    end
    else
    begin

      if LExecution <> nil then
      begin
        LExecution.Cancel;
      end;
      Cancel;
    end;
  except
    { A genuine admission refusal silently retires the pending receiver. An
      alternative scheduler may have entered work inline BEFORE raising: that
      original work/client error must propagate after terminal cleanup. }
    LEntered := (LDelivery <> nil) and LDelivery.Entered;
    try
      Cancel;
    except
      { Cancellation is already terminal even if private cleanup also refuses. }
    end;

    if LEntered then
    begin
      raise;
    end;
  end;
end;

procedure TBrowserClipboardRequest.Deliver(AOutcome: TNyxClipboardWriteOutcome;
  ABytes: Integer; const AError: TNyxText;
  ARepresentation: TNyxClipboardTextRepresentation);
begin
  Complete(AOutcome, ABytes, AError, ARepresentation);
end;

function TBrowserClipboardRequest.AwaitPlatform(APromise: TJSPromise;
  ABytes: Integer; ARepresentation: TNyxClipboardTextRepresentation): JSValue; async;
var
  LLease: INyxClipboardWriteRequest;
  LPacket: TJSObject;
  LOutcome: TNyxClipboardWriteOutcome;
  LRepresentation: TNyxClipboardTextRepresentation;
  LError: TNyxText;
  LBytes: Integer;
begin
  Result := Undefined;
  LLease := Self;
  LBytes := ABytes;
  LRepresentation := ARepresentation;
  try
    FScheduler.RequireUI;
    LPacket := TJSObject(await(APromise._then(@Accepted, @Rejected)));
    FScheduler.RequireUI;

    if Boolean(LPacket['ok']) then
    begin
      LOutcome := ncwoWritten;
      LError := '';
    end
    else
    begin
      LOutcome := FailureOutcome(LPacket['error'], LError);
    end;
  except
    LOutcome := FailureOutcome(JSExceptValue, LError);
  end;

  if LOutcome <> ncwoWritten then
  begin
    LBytes := 0;
    LRepresentation := nctrNone;
  end;
  Queue(LOutcome, LBytes, LError, LRepresentation);
end;

procedure TBrowserClipboardRequest.Start;
var
  LLease: INyxClipboardWriteRequest;
  LPromise: TJSPromise;
  LOutcome: TNyxClipboardWriteOutcome;
  LRepresentation: TNyxClipboardTextRepresentation;
  LError: TNyxText;
  LBytes: Integer;
begin
  LLease := Self;
  LBytes := FByteCount;
  FScheduler.RequireUI;
  try
    LPromise := BeginPlatform(LOutcome, LError, LRepresentation);
  except
    LPromise := nil;
    LOutcome := FailureOutcome(JSExceptValue, LError);
  end;

  if LPromise = nil then
  begin
    { Keep immediate refusals synchronous with this handoff. If a scheduler
      enters the reply inline and it raises, Begin preserves that exact error. }
    Queue(LOutcome, 0, LError, nctrNone);
    Exit;
  end;
  { AwaitPlatform takes a managed lease synchronously and keeps it across await.
    The queued work then owns delivery, so no unobserved promise self-cycle is
    needed when scheduler admission refuses after an external write settles. }
  AwaitPlatform(LPromise, LBytes, LRepresentation);
end;

constructor TBrowserClipboardHost.Create(AWindow: TJSWindow; AMode: TNyxClipboardTextMode;
  AActivation: TNyxBrowserClipboardActivation);
begin
  inherited Create;

  if (AWindow = nil) or (Ord(AMode) < Ord(Low(TNyxClipboardTextMode))) or
    (Ord(AMode) > Ord(High(TNyxClipboardTextMode))) or
    (Ord(AActivation) < Ord(Low(TNyxBrowserClipboardActivation))) or
    (Ord(AActivation) > Ord(High(TNyxBrowserClipboardActivation))) then
  begin
    raise ENyxClipboard.Create('Clipboard host requires a browser Window and known text mode');
  end;
  FWindow := AWindow;
  FMode := AMode;
  FActivation := AActivation;
end;

function TBrowserClipboardHost.BeginWriteText(const AText: TNyxText;
  const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
  AReply: TNyxClipboardWriteReply): INyxClipboardWriteRequest;
var
  LHost: INyxClipboardWriteHost;
  LRequest: TBrowserClipboardRequest;
  LWindow: TJSWindow;
  LMode: TNyxClipboardTextMode;
  LActivation: TNyxBrowserClipboardActivation;
begin
  LHost := Self;
  LWindow := FWindow;
  LMode := FMode;
  LActivation := FActivation;
  LRequest := TBrowserClipboardRequest.Create(LWindow, LMode, LActivation, AText,
    AScheduler, ACancellation, AReply);
  Result := LRequest;
  LRequest.Start;
end;

function NewNyxBrowserClipboardWriteHost(AMode: TNyxClipboardTextMode;
  AActivation: TNyxBrowserClipboardActivation): INyxClipboardWriteHost;
begin
  Result := NewNyxBrowserClipboardWriteHost(window, AMode, AActivation);
end;

function NewNyxBrowserClipboardWriteHost(AWindow: TJSWindow;
  AMode: TNyxClipboardTextMode; AActivation: TNyxBrowserClipboardActivation): INyxClipboardWriteHost;
begin
  Result := TBrowserClipboardHost.Create(AWindow, AMode, AActivation);
end;

end.
