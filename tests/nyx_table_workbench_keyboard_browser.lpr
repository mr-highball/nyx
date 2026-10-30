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
program nyx_table_workbench_keyboard_browser;

{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch externalclass}

{ Compile beside the unchanged Studio-generated Service monitor. Passive capture
  observes actual input without canonical key subscriptions. A separately labeled
  button produces untrusted synthetic keys for explicit adapter checks. The default target
  records writes without touching the OS/browser clipboard. Explicit ?browser
  delegates through the real browser host with its normal activation policy.
  Explicit ?unavailable uses the ordinary missing-service host for feedback. }
uses
  SysUtils, JS, Web, nyx.text, nyx.model, nyx.codec, nyx.scheduler,
  nyx.application.browser, nyx.clipboard, nyx.clipboard.browser,
  nyx.clipboard.unavailable,
  nyx.collections, nyx.collections.mount, nyx.collections.clipboard,
  nyx.collections.clipboard.actions, nyx.collections.workbench.runtime,
  nyx.generated.view;

type
  { Closed fixture authority choices are parsed only at the URL boundary.
    Recording has no OS effects; unavailable exercises the built-in refusal. }
  TKeyboardHostMode = (khmRecording, khmBrowser, khmUnavailable);

  { Browser Document's capture-listener overload borrows this document and
    receiver only while mounted. Capture reads the original event before Nyx
    handlers, without preventing defaults, stopping propagation or editing it. }
  TCaptureDocument = class external name 'Document'(TJSDocument)
    procedure addEventListener(const AName: String;
      AListener: TJSRawEventHandler; ACapture: Boolean); reintroduce;
    procedure removeEventListener(const AName: String;
      AListener: TJSRawEventHandler; ACapture: Boolean); reintroduce;
  end;

  { Explicit synthetic producer uses the standard browser KeyboardEvent API.
    These events are untrusted test input; the surrounding button click alone
    supplies a real gesture to the optional browser clipboard host. }
  TProducedKeyboardEvent = class external name 'KeyboardEvent'(TJSKeyboardEvent)
    constructor new(const AType: String; AInit: TJSObject); reintroduce;
  end;

  { Forward receiver is borrowed by the tracing host. Page retirement revokes
    that pointer before releasing the application, host or authored document. }
  TKeyboardObservation = class;

  { Instrument only the explicit host boundary. A nil inner host means a pure
    recording publication; otherwise original text, scheduler, cancellation and
    borrowed reply pass unchanged to the real adapter. No application is owned. }
  TTracedClipboard = class(TInterfacedObject, INyxClipboardWriteHost)
  private
    FOwner: TKeyboardObservation;
    FInner: INyxClipboardWriteHost;
  public
    constructor Create(AOwner: TKeyboardObservation;
      const AInner: INyxClipboardWriteHost);
    procedure Revoke;
    function BeginWriteText(const AText: TNyxText;
      const AScheduler: INyxScheduler;
      const ACancellation: INyxCancellationScope;
      AReply: TNyxClipboardWriteReply): INyxClipboardWriteRequest;
  end;

  { One explicitly owned application and data-only observations. Controller
    completion and DOM listeners borrow Self; pagehide revokes both before any
    application/document teardown. It registers no Nyx keyboard subscribers. }
  TKeyboardObservation = class
  private
    FDocument: TNyxDocument;
    FAuthored: TNyxText;
    FApp: TNyxBrowserApplication;
    FHost: INyxClipboardWriteHost;
    FTrace: TTracedClipboard;
    FCommands: INyxCollectionClipboardCommands;
    FTable: TJSHTMLElement;
    FLastCell: TJSHTMLElement;
    FSend: TJSHTMLElement;
    FRetire: TJSHTMLElement;
    FStopped: Boolean;
    FKeys: Integer;
    FHostCalls: Integer;
    FCompletions: Integer;
    FCopies: Integer;
    procedure KeyDown(AEvent: TJSEvent);
    procedure KeyUp(AEvent: TJSEvent);
    procedure CopyInvoked(AEvent: TJSEvent);
    procedure FocusIn(AEvent: TJSEvent);
    procedure SendSynthetic(AEvent: TJSEvent);
    procedure Retire(AEvent: TJSEvent);
    procedure PageHide(AEvent: TJSEvent);
    procedure Received(const APayload: INyxCollectionClipboardText;
      const AResult: TNyxClipboardWriteResult);
  public
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    procedure HostEntry(const AText: TNyxText);
  end;

constructor TTracedClipboard.Create(AOwner: TKeyboardObservation;
  const AInner: INyxClipboardWriteHost);
begin
  inherited Create;
  FOwner := AOwner;
  FInner := AInner;
end;

procedure TTracedClipboard.Revoke;
begin
  FOwner := nil;
end;

function TTracedClipboard.BeginWriteText(const AText: TNyxText;
  const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
  AReply: TNyxClipboardWriteReply): INyxClipboardWriteRequest;
var
  LResult: TNyxClipboardWriteResult;
begin
  AScheduler.RequireUI;

  if FOwner = nil then
  begin
    raise ENyxClipboard.Create('The keyboard observation has retired');
  end;
  FOwner.HostEntry(AText);

  if FInner <> nil then
  begin
    Exit(FInner.BeginWriteText(AText, AScheduler, ACancellation, AReply));
  end;
  { A terminal inline receipt needs no retained request token. This explicit
    recording authority claims only its copied value, never an OS side effect. }
  LResult := Default(TNyxClipboardWriteResult);
  LResult.Outcome := ncwoWritten;
  LResult.ByteCount := NyxClipboardTextByteCount(AText);
  LResult.Representation := nctrExactPlainText;

  if Assigned(AReply) then
  begin
    AReply(LResult);
  end;
  Result := nil;
end;

procedure TKeyboardObservation.HostEntry(const AText: TNyxText);
begin
  Inc(FHostCalls);
  document.body.setAttribute('data-keyboard-host-calls', IntToStr(FHostCalls));
  document.body.setAttribute('data-keyboard-host-text', AText);
end;

procedure TKeyboardObservation.Received(const APayload: INyxCollectionClipboardText;
  const AResult: TNyxClipboardWriteResult);
begin
  Inc(FCompletions);
  document.body.setAttribute('data-keyboard-completions', IntToStr(FCompletions));
  document.body.setAttribute('data-keyboard-outcome', NyxClipboardWriteOutcomeName(AResult.Outcome));
  document.body.setAttribute('data-keyboard-error', AResult.Error);
  document.body.setAttribute('data-keyboard-bytes', IntToStr(AResult.ByteCount));

  if FCommands <> nil then
  begin
    document.body.setAttribute('data-keyboard-phase', IntToStr(Ord(FCommands.Phase)));
  end;

  if APayload <> nil then
  begin
    document.body.setAttribute('data-keyboard-result-text', APayload.Text);
  end;
end;

procedure TKeyboardObservation.KeyDown(AEvent: TJSEvent);
var
  LKey: TJSKeyboardEvent;
begin
  LKey := TJSKeyboardEvent(AEvent);
  Inc(FKeys);
  document.body.setAttribute('data-keyboard-key-count', IntToStr(FKeys));
  document.body.setAttribute('data-keyboard-key', LKey.key);
  document.body.setAttribute('data-keyboard-code', String(TJSObject(LKey)['code']));
  document.body.setAttribute('data-keyboard-control', BoolToStr(LKey.ctrlKey, True));
  document.body.setAttribute('data-keyboard-meta', BoolToStr(LKey.metaKey, True));
  document.body.setAttribute('data-keyboard-shift', BoolToStr(LKey.shiftKey, True));
  document.body.setAttribute('data-keyboard-alt', BoolToStr(LKey.altKey, True));
  document.body.setAttribute('data-keyboard-altgraph',
    BoolToStr(LKey.getModifierState('AltGraph'), True));
  document.body.setAttribute('data-keyboard-composing', BoolToStr(LKey.isComposing, True));
  document.body.setAttribute('data-keyboard-trusted',
    BoolToStr(Boolean(TJSObject(LKey)['isTrusted']), True));
end;

procedure TKeyboardObservation.KeyUp(AEvent: TJSEvent);
begin

  if FCommands <> nil then
  begin
    document.body.setAttribute('data-keyboard-phase', IntToStr(Ord(FCommands.Phase)));
  end;
end;

procedure TKeyboardObservation.CopyInvoked(AEvent: TJSEvent);
begin
  { Browser Edit/Copy may produce clipboard input independently of keydown.
    Observing it must not publish data, claim the event or read clipboard data. }
  Inc(FCopies);
  document.body.setAttribute('data-keyboard-copy-events', IntToStr(FCopies));
  document.body.setAttribute('data-keyboard-copy-trusted',
    BoolToStr(Boolean(TJSObject(AEvent)['isTrusted']), True));
  document.body.setAttribute('data-keyboard-copy-cancelable', BoolToStr(AEvent.cancelable, True));
  document.body.setAttribute('data-keyboard-copy-bubbles', BoolToStr(AEvent.bubbles, True));

  if AEvent.target is TJSHTMLElement then
  begin
    document.body.setAttribute('data-keyboard-copy-target',
      TJSHTMLElement(AEvent.target).tagName + '/' +
      TJSHTMLElement(AEvent.target).getAttribute('role'));
  end;
end;

procedure TKeyboardObservation.PageHide(AEvent: TJSEvent);
begin
  Stop;
end;

procedure TKeyboardObservation.FocusIn(AEvent: TJSEvent);
var
  LElement: TJSHTMLElement;
begin

  if not FStopped and (AEvent.target is TJSHTMLElement) then
  begin
    LElement := TJSHTMLElement(AEvent.target);

    if (FTable <> nil) and FTable.contains(LElement) and
      (LElement.getAttribute('role') = 'gridcell') then
    begin
      FLastCell := LElement;
    end;
  end;
end;

procedure TKeyboardObservation.SendSynthetic(AEvent: TJSEvent);
var
  LCell: TJSHTMLElement;
  LOptions: TJSObject;
  LKey: TJSKeyboardEvent;
begin
  LCell := FLastCell;

  if FStopped or (LCell = nil) or (FTable = nil) or not FTable.contains(LCell) then
  begin
    document.body.setAttribute('data-keyboard-error', 'Focus an original data cell first');
    Exit;
  end;
  LCell.focus;
  LOptions := TJSObject.new;
  LOptions['key'] := 'c';
  LOptions['code'] := 'KeyC';
  LOptions['ctrlKey'] := True;
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;
  LKey := TProducedKeyboardEvent.new('keydown', LOptions);
  LCell.dispatchEvent(LKey);
  document.body.setAttribute('data-keyboard-synthetic-claimed',
    BoolToStr(LKey.defaultPrevented, True));

  if not FStopped and (FTable <> nil) and FTable.contains(LCell) then
  begin
    LCell.dispatchEvent(TProducedKeyboardEvent.new('keyup', LOptions));
  end;
end;

procedure TKeyboardObservation.Retire(AEvent: TJSEvent);
begin
  Stop;
end;

procedure TKeyboardObservation.Start;
var
  LInner: INyxClipboardWriteHost;
  LHost: TJSHTMLElement;
  LBar: TJSHTMLElement;
  LMount: INyxCollectionMount;
  LClipboard: INyxCollectionClipboardMount;
  LMode: TKeyboardHostMode;
begin
  LInner := nil;
  case window.location.search of
    '':
    begin
      LMode := khmRecording;
    end;
    '?browser':
    begin
      LMode := khmBrowser;
    end;
    '?unavailable':
    begin
      LMode := khmUnavailable;
    end;
    else
    begin
      raise Exception.Create('Expected recording, ?browser or ?unavailable host');
    end;
  end;
  case LMode of
    khmRecording:
    begin
      document.body.setAttribute('data-keyboard-authority', 'recording');
    end;
    khmBrowser:
    begin
      LInner := NewNyxBrowserClipboardWriteHost(window);
      document.body.setAttribute('data-keyboard-authority', 'browser');
    end;
    khmUnavailable:
    begin
      LInner := NewNyxUnavailableClipboardWriteHost;
      document.body.setAttribute('data-keyboard-authority', 'unavailable');
    end;
  end;
  FDocument := BuildNyxDocument;
  FAuthored := TNyxCodec.Encode(FDocument);

  if FDocument.Title <> 'Service monitor' then
  begin
    raise Exception.Create('This consumer requires the unchanged generated Service monitor');
  end;
  FTrace := TTracedClipboard.Create(Self, LInner);
  FHost := FTrace;
  FApp := TNyxBrowserApplication.Create;
  FApp.ConfigureTableWorkbench(NewNyxTableWorkbenchServices(FHost));
  LHost := TJSHTMLElement(document.createElement('main'));
  document.body.appendChild(LHost);
  FApp.Run(FDocument, LHost);
  LMount := FApp.View.CollectionMount('service-table');

  if not Supports(LMount, INyxCollectionClipboardMount, LClipboard) then
  begin
    raise Exception.Create('The saved application must expose its original clipboard owner');
  end;
  FCommands := LClipboard.ClipboardCommands;
  FCommands.Observe(@Received);
  FTable := FApp.View.ElementFor('service-table');
  LBar := TJSHTMLElement(document.createElement('nav'));
  FSend := TJSHTMLElement(document.createElement('button'));
  FSend.textContent := 'Send synthetic Ctrl+C to the last focused cell';
  FSend.addEventListener('click', @SendSynthetic);
  LBar.appendChild(FSend);
  FRetire := TJSHTMLElement(document.createElement('button'));
  FRetire.textContent := 'Retire keyboard consumer';
  FRetire.addEventListener('click', @Retire);
  LBar.appendChild(FRetire);
  document.body.insertBefore(LBar, LHost);
  TCaptureDocument(document).addEventListener('keydown', @KeyDown, True);
  TCaptureDocument(document).addEventListener('keyup', @KeyUp, True);
  TCaptureDocument(document).addEventListener('focusin', @FocusIn, True);
  TCaptureDocument(document).addEventListener('copy', @CopyInvoked, True);
  window.addEventListener('pagehide', @PageHide);
  document.body.setAttribute('data-keyboard-host-calls', '0');
  document.body.setAttribute('data-keyboard-completions', '0');
  document.body.setAttribute('data-keyboard-key-count', '0');
  document.body.setAttribute('data-keyboard-copy-events', '0');
  document.body.setAttribute('data-keyboard-status', 'ready');
end;

procedure TKeyboardObservation.Stop;
begin

  if FStopped then
  begin
    Exit;
  end;
  FStopped := True;
  TCaptureDocument(document).removeEventListener('keydown', @KeyDown, True);
  TCaptureDocument(document).removeEventListener('keyup', @KeyUp, True);
  TCaptureDocument(document).removeEventListener('focusin', @FocusIn, True);
  TCaptureDocument(document).removeEventListener('copy', @CopyInvoked, True);
  window.removeEventListener('pagehide', @PageHide);
  FLastCell := nil;
  FTable := nil;

  if FSend <> nil then
  begin
    FSend.removeEventListener('click', @SendSynthetic);
    FSend.setAttribute('disabled', 'disabled');
  end;

  if FRetire <> nil then
  begin
    FRetire.removeEventListener('click', @Retire);
    FRetire.setAttribute('disabled', 'disabled');
  end;

  if FCommands <> nil then
  begin
    FCommands.Observe(nil);
  end;

  if FTrace <> nil then
  begin
    FTrace.Revoke;
  end;
  FreeAndNil(FApp);
  FCommands := nil;
  FHost := nil;
  FTrace := nil;

  if FDocument <> nil then
  begin
    document.body.setAttribute('data-keyboard-authored-unchanged',
      BoolToStr(TNyxCodec.Encode(FDocument) = FAuthored, True));
  end;
  FreeAndNil(FDocument);
  document.body.setAttribute('data-keyboard-status', 'retired');
end;

destructor TKeyboardObservation.Destroy;
begin
  Stop;
  inherited Destroy;
end;

var
  LObservation: TKeyboardObservation;
begin
  LObservation := TKeyboardObservation.Create;
  try
    LObservation.Start;
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-keyboard-error', E.Message);
      LObservation.Free;
    end;
  end;
end.
