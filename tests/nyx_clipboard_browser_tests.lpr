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
program nyx_clipboard_browser_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, JS, Web, nyx.text, nyx.bytes, nyx.scheduler, nyx.clipboard,
  nyx.clipboard.browser;

type
  { An explicit substitute never delegates to navigator.clipboard. The fixture
    controls promise scheduling/errors, while the real adapter and scheduler
    retain all production admission, payload and cancellation responsibilities. }
  TWriteBehavior = (wbResolved, wbRejected, wbDeferred, wbInline,
    wbBrokenReturn, wbThrows);

  { The borrowed reply is owned by the fixture until canceled/settled. Self
    disposal tests the no-tail-access contract without retaining a request or
    introducing a receiver/request reference cycle. }
  TReply = class
  public
    Calls: Integer;
    Last: TNyxClipboardWriteResult;
    DisposeOnReply: Boolean;
    ThrowOnReply: Boolean;
    procedure Receive(const AResult: TNyxClipboardWriteResult);
  end;

  { Only plain JavaScript objects model an explicitly supplied target Window.
    No property on the real Window/navigator is changed. Blob and Promise are
    real standard objects; neither produces a user clipboard side effect. }
  TEnvironment = class
  public
    Context: TJSObject;
    Navigator: TJSObject;
    Clipboard: TJSObject;
    Activation: TJSObject;
    ItemConstructor: JSValue;
    CustomSupported: Boolean;
    Behavior: TWriteBehavior;
    Failure: JSValue;
    Writes: Integer;
    TextWrites: Integer;
    ItemWrites: Integer;
    LastText: TNyxText;
    LastData: TJSObject;
    Resolve: TJSPromiseResolver;
    Reject: TJSPromiseResolver;
    ScopeOnWrite: INyxCancellationScope;
    ScopeOnSupports: INyxCancellationScope;
    constructor Create;
    function WriteText(AValue: JSValue): JSValue;
    function WriteItems(AValue: JSValue): JSValue;
    function Item(AData: JSValue): JSValue;
    function Supports(AType: JSValue): JSValue;
    function InlineThen(AResolve, AReject: TJSPromiseResolver): JSValue;
    function Operation: JSValue;
  end;

  { UI-only execution facts for the deliberate inline-handoff stress case.
    No work queue/thread is simulated and no running operation escapes the test. }
  TInlineExecution = class(TInterfacedObject, INyxExecution)
  private
    FCancelled: Boolean;
  public
    procedure Cancel;
    function GetCancelled: Boolean;
    function GetStatus: TNyxExecutionStatus;
    function GetFailure: TNyxText;
  end;

  { This adversarial alternative enters PostUI immediately, then lets the client
    exception escape. It tests the adapter's admission-versus-entry distinction;
    it does not claim to implement the built-in scheduler's deferred contract. }
  TInlineScheduler = class(TInterfacedObject, INyxScheduler)
  public
    function GetCapabilities: TNyxSchedulerCapabilities;
    procedure RequireUI;
    procedure Admit(APolicy: TNyxExecutionPolicy);
    function Submit(const AWork: INyxWork;
      APolicy: TNyxExecutionPolicy): INyxExecution;
    function PostUI(const AWork: INyxWork;
      const AParent: INyxExecution = nil): INyxExecution;
    procedure Shutdown;
  end;

var
  GChecks: Integer;
  GDisposedReplies: Integer;
  GDeliveredReplies: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxClipboard.Create('Browser clipboard: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure TReply.Receive(const AResult: TNyxClipboardWriteResult);
begin
  Inc(GDeliveredReplies);
  Inc(Calls);
  Last := AResult;

  if ThrowOnReply then
  begin
    raise ENyxClipboard.Create('Intentional inline browser clipboard receiver failure');
  end;

  if DisposeOnReply then
  begin
    Inc(GDisposedReplies);
    Free;
  end;
end;

constructor TEnvironment.Create;
begin
  inherited Create;
  Context := TJSObject.new;
  Navigator := TJSObject.new;
  Clipboard := TJSObject.new;
  Activation := TJSObject.new;
  Context['isSecureContext'] := True;
  Context['navigator'] := Navigator;
  Navigator['clipboard'] := Clipboard;
  Navigator['userActivation'] := Activation;
  Activation['isActive'] := True;
  Clipboard['writeText'] := @WriteText;
  Clipboard['write'] := @WriteItems;
  ItemConstructor := @Item;
  TJSObject(ItemConstructor)['supports'] := @Supports;
  Context['ClipboardItem'] := ItemConstructor;
  CustomSupported := True;
end;

function TEnvironment.Item(AData: JSValue): JSValue;
begin
  LastData := TJSObject(AData);
  Result := TJSObject.new;
  TJSObject(Result)['data'] := AData;
end;

function TEnvironment.Supports(AType: JSValue): JSValue;
begin

  if ScopeOnSupports <> nil then
  begin
    ScopeOnSupports.Cancel;
  end;
  Result := CustomSupported and (String(AType) = NyxBrowserClipboardExactFormat);
end;

function TEnvironment.InlineThen(AResolve, AReject: TJSPromiseResolver): JSValue;
begin
  AResolve(Undefined);
  AReject(Failure);
  AResolve(Undefined);
  Result := Undefined;
end;

function TEnvironment.Operation: JSValue;
var
  LFailure: JSValue;
begin
  Inc(Writes);

  if ScopeOnWrite <> nil then
  begin
    ScopeOnWrite.Cancel;
  end;
  case Behavior of
    wbResolved: Result := TJSPromise.resolve;
    wbRejected: Result := TJSPromise.reject(Failure);
    wbDeferred:
    begin
      Result := TJSPromise.new(procedure(AResolve, AReject: TJSPromiseResolver)
        begin
          Resolve := AResolve;
          Reject := AReject;
        end);
    end;
    wbInline:
    begin
      Result := TJSObject.new;
      TJSObject(Result)['then'] := @InlineThen;
    end;
    wbBrokenReturn: Result := Undefined;
    wbThrows:
    begin
      LFailure := Failure;
      { Inject a real arbitrary platform throw, including undefined/null.
        Exception(value) would coerce primitives through rtl.getObject. }
      asm
        throw LFailure;
      end;
    end;
  end;
end;

function TEnvironment.WriteText(AValue: JSValue): JSValue;
begin
  Inc(TextWrites);
  LastText := String(AValue);
  Result := Operation;
end;

function TEnvironment.WriteItems(AValue: JSValue): JSValue;
begin
  Inc(ItemWrites);
  Check(isArray(AValue) and (TJSArray(AValue).length = 1),
    'one ClipboardItem carries exact and interoperable representations atomically');
  Result := Operation;
end;

procedure TInlineExecution.Cancel;
begin
  FCancelled := True;
end;

function TInlineExecution.GetCancelled: Boolean;
begin
  Result := FCancelled;
end;

function TInlineExecution.GetStatus: TNyxExecutionStatus;
begin
  Result := nesRunning;
end;

function TInlineExecution.GetFailure: TNyxText;
begin
  Result := '';
end;

function TInlineScheduler.GetCapabilities: TNyxSchedulerCapabilities;
begin
  Result := [];
end;

procedure TInlineScheduler.RequireUI;
begin
  { The entire stress case runs on the browser's actual event-loop thread. }
end;

procedure TInlineScheduler.Admit(APolicy: TNyxExecutionPolicy);
begin
end;

function TInlineScheduler.Submit(const AWork: INyxWork;
  APolicy: TNyxExecutionPolicy): INyxExecution;
begin
  Result := PostUI(AWork);
end;

function TInlineScheduler.PostUI(const AWork: INyxWork;
  const AParent: INyxExecution): INyxExecution;
begin
  Result := TInlineExecution.Create;
  AWork.Execute(Result);
end;

procedure TInlineScheduler.Shutdown;
begin
end;

procedure Pause(AResolve, AReject: TJSPromiseResolver);
begin
  window.setTimeout(procedure
    begin
      AResolve(Undefined);
    end, 5);
end;

function Turns: JSValue; async;
begin
  await(TJSPromise.resolve(TJSPromise.new(@Pause)));
  await(TJSPromise.resolve(TJSPromise.new(@Pause)));
  Result := Undefined;
end;

function AwaitReply(AReply: TReply; AExpected: Integer): JSValue; async;
var
  LTurn: Integer;
begin
  for LTurn := 0 to 99 do
  begin

    if AReply.Calls >= AExpected then
    begin
      Break;
    end;
    await(TJSPromise.resolve(TJSPromise.new(@Pause)));
  end;
  Check(AReply.Calls = AExpected, 'actual scheduler delivers the exact expected reply count');
  Result := Undefined;
end;

function BlobText(ABlob: TJSBlob): JSValue; async;
var
  LArray: TJSUint8Array;
  LBytes: TNyxBytes;
  LIndex: Integer;
begin
  LArray := TJSUint8Array.new(TJSArrayBuffer(await(ABlob.arrayBuffer)));
  SetLength(LBytes, LArray.length);
  for LIndex := 0 to High(LBytes) do
  begin
    LBytes[LIndex] := LArray[LIndex];
  end;
  Result := NyxDecodeUTF8(LBytes);
end;

function Run: JSValue; async;
const
  CSingle: TNyxText = 'Exact value / 🌙 / 漢字';
  CMultiline: TNyxText = 'Row / 🌙' + #13#10 + 'Second' + #10 + 'Third';
  CComplete: TNyxText = 'Row / 🌙' + #13#10 + #0 + 'Second' + #10 + 'Third';
var
  LEnvironment: TEnvironment;
  LReply: TReply;
  LHost: INyxClipboardWriteHost;
  LToken: INyxClipboardWriteRequest;
  LScope: INyxCancellationScope;
  LScheduler: INyxScheduler;
  LText: TNyxText;
  LError: TJSObject;
  LCase: Integer;
  LBefore: Integer;
  LBeforeWrites: Integer;
  LDisposed: Integer;
  LInlineScheduler: INyxScheduler;
  LRejected: Boolean;
begin
  Result := Undefined;
  LScheduler := NewNyxScheduler;
  LReply := TReply.Create;
  LEnvironment := TEnvironment.Create;
  try
    for LCase := 0 to 4 do
    begin
      LEnvironment.Context['isSecureContext'] := LCase <> 0;
      LEnvironment.Navigator['clipboard'] := LEnvironment.Clipboard;
      LEnvironment.Navigator['userActivation'] := LEnvironment.Activation;
      LEnvironment.Activation['isActive'] := True;
      LEnvironment.Clipboard['writeText'] := @LEnvironment.WriteText;
      case LCase of
        1: LEnvironment.Navigator['clipboard'] := Undefined;
        2: LEnvironment.Navigator['userActivation'] := Undefined;
        3: LEnvironment.Activation['isActive'] := False;
        4: LEnvironment.Clipboard['writeText'] := Undefined;
      end;
      LHost := NewNyxBrowserClipboardWriteHost(TJSWindow(LEnvironment.Context));
      LBefore := LReply.Calls;
      LToken := LHost.BeginWriteText(CSingle, LScheduler, nil, @LReply.Receive);
      await(AwaitReply(LReply, LBefore + 1));
      Check((LReply.Last.ByteCount = 0) and
        (LReply.Last.Representation = nctrNone) and (LEnvironment.Writes = 0) and
        (((LCase = 3) and (LReply.Last.Outcome = ncwoActivationRequired)) or
        ((LCase <> 3) and (LReply.Last.Outcome = ncwoUnavailable))),
        'secure context, API and activation refusals have distinct typed results before any effect');
      LToken := nil;
    end;
    LEnvironment.Clipboard['writeText'] := @LEnvironment.WriteText;
    LEnvironment.Activation['isActive'] := True;
    LInlineScheduler := TInlineScheduler.Create;
    LEnvironment.Context['isSecureContext'] := False;
    LHost := NewNyxBrowserClipboardWriteHost(TJSWindow(LEnvironment.Context));
    LBefore := LReply.Calls;
    LBeforeWrites := LEnvironment.Writes;
    LReply.ThrowOnReply := True;
    LRejected := False;
    try
      LToken := LHost.BeginWriteText(CSingle, LInlineScheduler, nil, @LReply.Receive);
    except
      on E: ENyxClipboard do
      begin
        LRejected := E.Message = 'Intentional inline browser clipboard receiver failure';
      end;
    end;
    Check(LRejected and (LReply.Calls = LBefore + 1) and
      (LEnvironment.Writes = LBeforeWrites),
      'inline client failure propagates from Begin after exactly one terminal reply, rather than becoming admission refusal');
    LReply.ThrowOnReply := False;
    LInlineScheduler := nil;
    LEnvironment.Context['isSecureContext'] := True;

    LEnvironment.Navigator['userActivation'] := Undefined;
    LHost := NewNyxBrowserClipboardWriteHost(TJSWindow(LEnvironment.Context),
      nctmExact, nbcaBrowserPermission);
    LBefore := LReply.Calls;
    LToken := LHost.BeginWriteText(CSingle, LScheduler, nil, @LReply.Receive);
    await(AwaitReply(LReply, LBefore + 1));
    Check(NyxClipboardWritten(LReply.Last),
      'explicit browser-permission policy permits an API-backed write without userActivation capability');
    LEnvironment.Navigator['userActivation'] := LEnvironment.Activation;
    LHost := NewNyxBrowserClipboardWriteHost(TJSWindow(LEnvironment.Context));
    LBefore := LReply.Calls;
    LText := CSingle;
    LToken := LHost.BeginWriteText(LText, LScheduler, nil, @LReply.Receive);
    LText := 'changed caller text';
    Check(LReply.Calls = LBefore, 'resolved host promise never bypasses scheduler delivery');
    await(AwaitReply(LReply, LBefore + 1));
    Check((LEnvironment.LastText = CSingle) and NyxClipboardWritten(LReply.Last) and
      (LReply.Last.ByteCount = NyxUTF8ByteCount(CSingle)) and
      (LReply.Last.Representation = nctrExactPlainText),
      'single-line Unicode writes the exact originally copied value and reports its actual representation');

    LBefore := LReply.Calls;
    LToken := LHost.BeginWriteText(CComplete, LScheduler, nil, @LReply.Receive);
    await(AwaitReply(LReply, LBefore + 1));
    Check(NyxClipboardWritten(LReply.Last) and
      (LReply.Last.Representation = nctrExactUTF8) and
      (LReply.Last.ByteCount = NyxUTF8ByteCount(CComplete)) and
      (String(await(BlobText(TJSBlob(LEnvironment.LastData[NyxBrowserClipboardExactFormat])))) = CComplete) and
      isUndefined(LEnvironment.LastData['text/plain']),
      'multiline/NUL/supplementary input has exact UTF8 bytes without a misleading truncating text representation');
    LBefore := LReply.Calls;
    LToken := LHost.BeginWriteText(CMultiline, LScheduler, nil, @LReply.Receive);
    await(AwaitReply(LReply, LBefore + 1));
    Check(NyxClipboardWritten(LReply.Last) and
      (LReply.Last.Representation = nctrExactUTF8WithPlatformText) and
      (String(await(BlobText(TJSBlob(LEnvironment.LastData[NyxBrowserClipboardExactFormat])))) = CMultiline) and
      (String(await(BlobText(TJSBlob(LEnvironment.LastData['text/plain'])))) = CMultiline),
      'no-NUL multiline publishes exact UTF8 and interoperable text together in ONE write');
    LBefore := LReply.Calls;
    LToken := LHost.BeginWriteText('', LScheduler, nil, @LReply.Receive);
    await(AwaitReply(LReply, LBefore + 1));
    Check(NyxClipboardWritten(LReply.Last) and (LReply.Last.ByteCount = 0) and
      (LEnvironment.LastText = ''), 'empty text is a completed write, not missing data');

    LEnvironment.CustomSupported := False;
    LBeforeWrites := LEnvironment.Writes;
    LBefore := LReply.Calls;
    LToken := LHost.BeginWriteText(CMultiline, LScheduler, nil, @LReply.Receive);
    await(AwaitReply(LReply, LBefore + 1));
    Check((LReply.Last.Outcome = ncwoUnavailable) and
      (LEnvironment.Writes = LBeforeWrites),
      'unsupported richer format refuses exact multiline without silently retrying lossy text');
    LHost := NewNyxBrowserClipboardWriteHost(TJSWindow(LEnvironment.Context), nctmPlatformText);
    LBefore := LReply.Calls;
    LToken := LHost.BeginWriteText(CMultiline, LScheduler, nil, @LReply.Receive);
    await(AwaitReply(LReply, LBefore + 1));
    Check(NyxClipboardWritten(LReply.Last) and
      (LReply.Last.Representation = nctrPlatformText) and
      (LEnvironment.LastText = CMultiline) and
      (LReply.Last.ByteCount = NyxUTF8ByteCount(CMultiline)),
      'explicit platform mode admits full multiline and names its OS text behavior');
    LBeforeWrites := LEnvironment.Writes;
    LBefore := LReply.Calls;
    LToken := LHost.BeginWriteText(CComplete, LScheduler, nil, @LReply.Receive);
    await(AwaitReply(LReply, LBefore + 1));
    Check((LReply.Last.Outcome = ncwoUnavailable) and
      (LEnvironment.Writes = LBeforeWrites),
      'platform-only NUL refuses before any truncating write');

    LHost := NewNyxBrowserClipboardWriteHost(TJSWindow(LEnvironment.Context));
    LEnvironment.CustomSupported := True;
    LScope := NewNyxCancellationScope;
    LEnvironment.ScopeOnSupports := LScope;
    LBefore := LReply.Calls;
    LBeforeWrites := LEnvironment.Writes;
    LToken := LHost.BeginWriteText(CComplete, LScheduler, LScope, @LReply.Receive);
    await(Turns);
    Check((LReply.Calls = LBefore) and (LEnvironment.Writes = LBeforeWrites),
      'cancellation during actual format-capability callout refuses before the external write');
    LToken := nil;
    LEnvironment.ScopeOnSupports := nil;
    LScope := nil;

    for LCase := 0 to 4 do
    begin
      LEnvironment.Behavior := wbRejected;
      LError := TJSObject.new;
      case LCase of
        0: LError['name'] := 'NotAllowedError';
        1: LError['name'] := 'SecurityError';
        2: LError['name'] := 'AbortError';
        3: LError['name'] := 'NotSupportedError';
        4: LError['name'] := 'OtherHostError';
      end;
      LEnvironment.Failure := LError;
      LBefore := LReply.Calls;
      LToken := LHost.BeginWriteText(CSingle, LScheduler, nil, @LReply.Receive);
      await(AwaitReply(LReply, LBefore + 1));
      Check((LReply.Last.ByteCount = 0) and (LReply.Last.Representation = nctrNone) and
        (((LCase in [0, 1]) and (LReply.Last.Outcome = ncwoDenied)) or
        ((LCase = 2) and (LReply.Last.Outcome = ncwoCancelled)) or
        ((LCase = 3) and (LReply.Last.Outcome = ncwoUnavailable)) or
        ((LCase = 4) and (LReply.Last.Outcome = ncwoFailed))),
        'platform promise rejection maps to bounded typed permission/cancellation/capability/failure results');
    end;

    for LCase := 0 to 2 do
    begin
      LEnvironment.Behavior := wbInline;

      if LCase = 1 then
      begin
        LEnvironment.Behavior := wbBrokenReturn;
      end
      else if LCase = 2 then
      begin
        LEnvironment.Behavior := wbThrows;
        LEnvironment.Failure := Undefined;
      end;
      LBefore := LReply.Calls;
      LToken := LHost.BeginWriteText(CSingle, LScheduler, nil, @LReply.Receive);
      await(AwaitReply(LReply, LBefore + 1));
      await(Turns);
      Check((LReply.Calls = LBefore + 1) and
        (((LCase = 0) and NyxClipboardWritten(LReply.Last)) or
        ((LCase <> 0) and (LReply.Last.Outcome = ncwoFailed))),
        'inline/duplicate thenables and invalid or raw-throwing hosts settle once without unhandled rejections');
    end;

    for LCase := 0 to 3 do
    begin
      LEnvironment.Behavior := wbDeferred;
      LScope := NewNyxCancellationScope;
      LEnvironment.ScopeOnWrite := nil;

      if LCase = 2 then
      begin
        LScope.Cancel;
      end
      else if LCase = 3 then
      begin
        LEnvironment.ScopeOnWrite := LScope;
      end;
      LBefore := LReply.Calls;
      LBeforeWrites := LEnvironment.Writes;
      LToken := LHost.BeginWriteText(CSingle, LScheduler, LScope, @LReply.Receive);

      if LCase = 0 then
      begin
        LToken.Cancel;
      end
      else
      begin
        LScope.Cancel;
      end;

      if LCase <> 2 then
      begin
        LEnvironment.Resolve(Undefined);
      end;
      await(Turns);
      Check((LReply.Calls = LBefore) and
        (LEnvironment.Writes = LBeforeWrites + Ord(LCase <> 2)),
        'explicit/scope/pre-admission/inside-effect cancellation suppresses every late reply without inventing undo');
      LToken := nil;
      LScope := nil;
      LEnvironment.ScopeOnWrite := nil;
    end;

    LEnvironment.Behavior := wbDeferred;
    LBefore := LReply.Calls;
    LToken := LHost.BeginWriteText(CSingle, LScheduler, nil, @LReply.Receive);
    LToken := nil;
    LHost := nil;
    LEnvironment.Resolve(Undefined);
    await(AwaitReply(LReply, LBefore + 1));
    Check(NyxClipboardWritten(LReply.Last),
      'pending platform promise owns completion after caller releases host and token');

    LHost := NewNyxBrowserClipboardWriteHost(TJSWindow(LEnvironment.Context));
    LToken := LHost.BeginWriteText(CSingle, LScheduler, nil, @LReply.Receive);
    LToken.Cancel;
    LToken := nil;
    LBefore := GDeliveredReplies;
    LReply.Free;
    LReply := nil;
    LEnvironment.Reject(Null);
    await(Turns);
    Check(GDeliveredReplies = LBefore,
      'late rejection after receiver disposal touches no borrowed client');

    LReply := TReply.Create;
    LReply.DisposeOnReply := True;
    LDisposed := GDisposedReplies;
    LToken := LHost.BeginWriteText(CSingle, LScheduler, nil, @LReply.Receive);
    LReply := nil;
    LEnvironment.Resolve(Undefined);
    await(Turns);
    Check(GDisposedReplies = LDisposed + 1,
      'client can destroy itself in actual scheduler delivery with no stale receiver tail');
    LToken := nil;

    LReply := TReply.Create;
    LBefore := GDeliveredReplies;
    LToken := LHost.BeginWriteText(CSingle, LScheduler, nil, @LReply.Receive);
    LScheduler.Shutdown;
    LEnvironment.Resolve(Undefined);
    await(Turns);
    Check(GDeliveredReplies = LBefore,
      'settled platform work cannot bypass a scheduler that refused UI delivery');
    LToken.Cancel;
    LToken := nil;
  finally

    if LToken <> nil then
    begin
      LToken.Cancel;
    end;
    LToken := nil;
    LHost := nil;
    LScope := nil;
    LReply.Free;
    LScheduler.Shutdown;
    LScheduler := nil;
    LEnvironment.Free;
  end;
end;

function Main: JSValue; async;
begin
  Result := Undefined;
  try
    await(Run);
    document.body.setAttribute('data-browser-clipboard', 'passed');
    document.body.setAttribute('data-browser-clipboard-checks', IntToStr(GChecks));
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-browser-clipboard', 'failed');
      document.body.setAttribute('data-browser-clipboard-error', E.Message);
    end;
  end;
end;

begin
  Main;
end.
