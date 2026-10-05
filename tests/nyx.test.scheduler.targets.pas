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
unit nyx.test.scheduler.targets;

{$mode delphi}{$H+}
{$codepage utf8}

interface

function RunNyxEventTargetJourney: Integer;

implementation

uses
  SysUtils,
  nyx.text,
  nyx.types,
  nyx.data,
  nyx.model,
  nyx.controls,
  nyx.contract,
  nyx.behavior,
  nyx.events,
  nyx.scheduler,
  {$IFDEF PAS2JS}
  Web,
  nyx.test.keyboard.browser,
  nyx.render.browser;
  {$ELSE}
  Classes,
  Controls,
  Forms,
  StdCtrls,
  LMessages,
  LCLType,
  nyx.widgets.lcl,
  nyx.render.lcl;
  {$ENDIF}

type
  {$IFNDEF PAS2JS}
  TWinControlAccess = class(TWinControl);
  {$ENDIF}

  { An owned trace avoids borrowed fixture state inside retained callbacks. }
  ITrace = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001003000001}']
    procedure Append(const AText: TNyxText);
  end;
  TTrace = class(TInterfacedObject, ITrace)
  public
    Text: TNyxText;
    procedure Append(const AText: TNyxText);
  end;

  TListener = class(TNyxEventCallback)
  public
    Trace: ITrace;
    Name: TNyxText;
    Count: Integer;
    Last: TNyxEventInfo;
    RemoveView: Boolean;
    SawCancellation: Boolean;
    Scheduler: INyxScheduler;
    ChildWork: INyxWork;
    ChildTicket: INyxExecution;
    ConsumeInput: Boolean;
    SawConsumed: Boolean;
    RetainedContext: INyxExecution;
    {$IFDEF PAS2JS}
    Renderer: TNyxBrowserRenderer;
    {$ELSE}
    Renderer: TNyxLCLRenderer;
    {$ENDIF}
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

  TSnapshotTask = class(TInterfacedObject, INyxWork)
  private
    FCallback: INyxEventCallback;
    FEvent: TNyxEventInfo;
  public
    constructor Create(const ACallback: INyxEventCallback; const AEvent: TNyxEventInfo);
    procedure Execute(const AExecution: INyxExecution);
  end;

  TLegacyListener = class
  public
    Count: Integer;
    procedure Invoke(ANode: TNyxNode; const AEvent: TNyxEventInfo);
  end;

procedure TLegacyListener.Invoke(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin
  Inc(Count);
end;

constructor TSnapshotTask.Create(const ACallback: INyxEventCallback;
  const AEvent: TNyxEventInfo);
begin
  inherited Create;
  FCallback := ACallback;
  FEvent := AEvent.Copy;
end;

procedure TSnapshotTask.Execute(const AExecution: INyxExecution);
begin
  FCallback.Invoke(FEvent, AExecution);
end;

procedure TTrace.Append(const AText: TNyxText);
begin
  Text := Text + AText;
end;

procedure TListener.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin
  Inc(Count);
  Last := AEvent.Copy;
  RetainedContext := AExecution;

  if AEvent.HasKeyboard then
  begin
    SawConsumed := NyxEventResponse(AExecution).Consumed;
  end;

  if ConsumeInput then
  begin
    NyxEventResponse(AExecution).Consume;
  end;

  if ChildWork <> nil then
  begin
    ChildTicket := Scheduler.PostUI(ChildWork, AExecution);
  end;

  if Trace <> nil then
  begin
    Trace.Append(Name);
  end;

  if RemoveView then
  begin
    Renderer.Unmount;
    SawCancellation := AExecution.Cancelled;
  end;
end;

procedure Check(ACondition: Boolean; const AMessage: TNyxText; var ACount: Integer);
begin

  if not ACondition then
  begin
    raise ENyxModel.Create('FAIL event target: ' + AMessage);
  end;
  Inc(ACount);
end;

function RunNyxKeyboardTargetJourney: Integer;
var
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LCompound: INyxColumn;
  LMemo: INyxMemo;
  LButton: INyxButton;
  LLegacy: TLegacyListener;
  LFirst: TListener;
  LSecond: TListener;
  LSemantic: TListener;
  LRelease: TListener;
  LFirstLease: INyxEventCallback;
  LSecondLease: INyxEventCallback;
  LSemanticLease: INyxEventCallback;
  LReleaseLease: INyxEventCallback;
  LFirstToken: INyxEventSubscription;
  LSecondToken: INyxEventSubscription;
  LSemanticToken: INyxEventSubscription;
  LTrace: TTrace;
  LTraceLease: ITrace;
  LBefore: Integer;
  {$IFDEF PAS2JS}
  LRenderer: TNyxBrowserRenderer;
  LHost: TJSHTMLElement;
  LInput: TJSHTMLElement;
  LButtonEvent: TJSKeyboardEvent;
  {$ELSE}
  LRenderer: TNyxLCLRenderer;
  LHost: TForm;
  LInput: TWinControl;
  LButtonControl: TWinControl;
  {$ENDIF}

  function SendKey(ATrigger: TNyxTrigger; ARepeat: Boolean = False;
    AComposing: Boolean = False): Boolean;
  {$IFDEF PAS2JS}
  var
    LEvent: TJSKeyboardEvent;
  begin
    LEvent := NyxTestKeyboard(ATrigger, 'Enter', [nmControl], ARepeat, AComposing);
    LInput.dispatchEvent(LEvent);
    Result := LEvent.defaultPrevented;
  end;
  {$ELSE}
  var
    LKey: Word;
  begin
    LKey := $0D;

    if AComposing then
    begin
      LKey := $E5;
    end;

    if ATrigger = ntKeyDown then
    begin
      TWinControlAccess(LInput).OnKeyDown(LInput, LKey, [ssCtrl]);
    end
    else
    begin
      TWinControlAccess(LInput).OnKeyUp(LInput, LKey, [ssCtrl]);
    end;
    Result := LKey = 0;
  end;
  {$ENDIF}

  procedure Mount;
  begin
    LRenderer.Render(LDocument, LPage.Node, LHost);
    {$IFDEF PAS2JS}
    LInput := TJSHTMLElement(LRenderer.ElementFor('reply').querySelector('textarea'));
    {$ELSE}
    LInput := TWinControl(LRenderer.InputFor('reply'));
    LHost.Show;
    Application.ProcessMessages;
    LInput.SetFocus;
    {$ENDIF}
  end;

begin
  Result := 0;
  LDocument := TNyxDocument.Create;
  LPage := NewNyxPage('home');
  LDocument.AddPage(LPage);
  LCompound := NewNyxColumn('thread');
  LCompound.Configure.Compound(True).Done;
  LPage.Add(LCompound);
  LMemo := NewNyxMemo('reply');
  LMemo.Value := 'Keyboard snapshot / 🌙';
  LMemo.Contract.On(ntKeyDown, NyxOriginValue, NyxTextDomain)
    .On(ntKeyUp, NyxOriginValue, NyxTextDomain);
  LCompound.Add(LMemo);
  LButton := NewNyxButton('send').WithText('Send');
  LPage.Add(LButton);
  LLegacy := TLegacyListener.Create;
  LFirst := TListener.Create;
  LFirstLease := LFirst;
  LSecond := TListener.Create;
  LSecondLease := LSecond;
  LSemantic := TListener.Create;
  LSemanticLease := LSemantic;
  LRelease := TListener.Create;
  LReleaseLease := LRelease;
  LTrace := TTrace.Create;
  LTraceLease := LTrace;
  LFirst.Trace := LTraceLease;
  LFirst.Name := 'A';
  LSecond.Trace := LTraceLease;
  LSecond.Name := 'B';
  LSemantic.Trace := LTraceLease;
  LSemantic.Name := 'C';
  {$IFDEF PAS2JS}
  LHost := TJSHTMLElement(document.createElement('div'));
  document.body.appendChild(LHost);
  LRenderer := TNyxBrowserRenderer.Create;
  {$ELSE}
  LHost := TForm.CreateNew(nil);
  LRenderer := TNyxLCLRenderer.Create;
  {$ENDIF}
  LFirst.Renderer := LRenderer;
  LRenderer.OnEvent := LLegacy.Invoke;
  try
    Mount;
    LFirstToken := LRenderer.Events.OnKeyDown(NyxControlEvents('reply')).Subscribe(LFirstLease);
    LSecondToken := LRenderer.Events.OnKeyDown(NyxControlEvents('reply')).Subscribe(LSecondLease);
    LSemanticToken := LRenderer.Events.OnKeyDown(NyxCompoundEvents('thread')).Subscribe(LSemanticLease);
    LRenderer.Events.OnKeyUp(NyxControlEvents('reply')).Subscribe(LReleaseLease);
    LFirst.ConsumeInput := True;
    Check(SendKey(ntKeyDown), 'real memo keyboard hook consumes its platform default', Result);
    Check((LTrace.Text = 'ABC') and LSecond.SawConsumed and LSemantic.SawConsumed,
      'one physical key invokes ordered origin and compound callbacks once', Result);
    Check(LFirst.Last.HasKeyboard and
      LFirst.Last.Keyboard.Matches(nkEnterKey, [nmControl]) and
      (LFirst.Last.OriginID = 'reply') and (LFirst.Last.SourceID = 'thread'),
      'real keyboard event captures typed modifiers and semantic identities', Result);
    Check(LFirst.Last.Value.AsText = TNyxText('Keyboard snapshot / 🌙'),
      'keyboard callback captures the accepted scalar domain independently', Result);
    Check(not NyxEventResponse(LFirst.RetainedContext).CanConsume,
      'real returned input callback seals its consumption window', Result);
    LFirst.ConsumeInput := False;
    Check(not SendKey(ntKeyDown, True) and LFirst.Last.Keyboard.Repeating,
      'real repeat notification retains its repeat flag and default', Result);
    Check(not LFirst.Last.Keyboard.Matches(nkEnterKey, [nmControl]),
      'one-shot shortcut refuses a repeated real-control key', Result);
    {$IFDEF PAS2JS}
    TJSHTMLTextAreaElement(LInput).value := 'Fresh keyboard draft / 🌙';
    {$ELSE}
    { LCL accepts its native UTF-8 text directly. A UnicodeString intermediate
      would introduce an unnecessary implicit conversion back to native text. }
    TMemo(LInput).Text := TNyxText('Fresh keyboard draft / 🌙');
    {$ENDIF}
    Check(not SendKey(ntKeyUp) and (LRelease.Count = 1) and
      not LRelease.Last.Keyboard.Repeating,
      'real key-up callback retains release semantics', Result);
    Check((LRelease.Last.Value.AsText = TNyxText('Fresh keyboard draft / 🌙')) and
      (LRenderer.Root.Find('reply').Prop('value') = TNyxText('Fresh keyboard draft / 🌙')),
      'keyboard captures current accepted text without requiring browser blur: payload=' +
      LRelease.Last.Value.ToJSON + ', model=' + LRenderer.Root.Find('reply').Prop('value'), Result);
    SendKey(ntKeyDown);
    Check(not LFirst.Last.Keyboard.Repeating, 'key-up clears native repeat tracking', Result);
    {$IFDEF PAS2JS}
    LInput.dispatchEvent(TJSEvent.new('blur'));
    {$ELSE}
    TWinControlAccess(LInput).OnExit(LInput);
    {$ENDIF}
    SendKey(ntKeyDown);
    Check(not LFirst.Last.Keyboard.Repeating,
      'focus loss resets missing key-up state', Result);
    LBefore := LFirst.Count;
    Check(not SendKey(ntKeyDown, False, True) and (LFirst.Count = LBefore),
      'composition/process keys bypass shortcut dispatch and remain editable', Result);
    LFirst.RemoveView := True;
    LBefore := LSecond.Count;
    Check(SendKey(ntKeyDown) and (LRenderer.Root = nil) and LFirst.SawCancellation,
      'keyboard navigation invalidates the input safely on both adapters', Result);
    Check(LSecond.Count = LBefore, 'keyboard navigation suppresses stale sibling invocations', Result);
    Check(LFirst.Last.Keyboard.Key = nkEnterKey,
      'retained keyboard snapshot survives source/control disposal', Result);
    LFirst.RemoveView := False;
    Mount;
    LSemanticToken.Cancel;
    LRenderer.Events.OnKeyDown(NyxControlEvents('reply')).Policy(neUIQueue);
    LBefore := LFirst.Count;
    Check(not SendKey(ntKeyDown) and (LFirst.Count = LBefore),
      'real queued keyboard callbacks never consume or run inline', Result);
    LRenderer.Unmount;
    Check((LFirstToken.LastExecution.Status = nesCancelled) and
      (LSecondToken.LastExecution.Status = nesCancelled),
      'unmount cancels all queued keyboard registrations', Result);
    {$IFNDEF PAS2JS}
    CheckSynchronize;
    {$ENDIF}
    Check(LFirst.Count = LBefore, 'cancelled real key work cannot enter a disposed view', Result);
    LLegacy.Count := 0;
    Mount;
    LRelease.ConsumeInput := True;
    LRenderer.Events.OnKeyDown(NyxControlEvents('send')).Subscribe(LReleaseLease);
    {$IFDEF PAS2JS}
    LButtonEvent := NyxTestKeyboard(ntKeyDown, ' ');
    LRenderer.ElementFor('send').dispatchEvent(LButtonEvent);
    Check(LButtonEvent.defaultPrevented,
      'actual browser button listener prevents a consumed Space default', Result);
    {$ELSE}
    LHost.Show;
    Application.ProcessMessages;
    LButtonControl := TWinControl(LRenderer.ControlFor('send'));
    LButtonControl.SetFocus;
    LButtonControl.Perform(CN_KEYDOWN, VK_SPACE, 0);
    LButtonControl.Perform(CN_KEYUP, VK_SPACE, 0);
    Check((LLegacy.Count = 0) and (LRelease.Last.Keyboard.Key = nkSpaceKey),
      'native CN Space messages reach Nyx and suppress default button activation', Result);
    {$ENDIF}
    Check(LRelease.Last.HasKeyboard and (LRelease.Last.OriginID = 'send'),
      'button keys use their own typed origin without leaking memo registrations', Result);
    LRelease.ConsumeInput := False;
    LSemantic.ConsumeInput := True;
    LRenderer.Events.OnKeyUp(NyxControlEvents('send')).Subscribe(LSemanticLease);
    {$IFDEF PAS2JS}
    LRenderer.ElementFor('send').dispatchEvent(NyxTestKeyboard(ntKeyDown, ' '));
    LButtonEvent := NyxTestKeyboard(ntKeyUp, ' ');
    LRenderer.ElementFor('send').dispatchEvent(LButtonEvent);
    Check(LButtonEvent.defaultPrevented and (LSemantic.Last.Trigger = ntKeyUp),
      'browser button release callbacks consume before its default', Result);
    {$ELSE}
    LButtonControl.Perform(CN_KEYDOWN, VK_SPACE, 0);
    LButtonControl.Perform(CN_KEYUP, VK_SPACE, 0);
    Check((LLegacy.Count = 0) and (LSemantic.Last.Trigger = ntKeyUp),
      'native button release callbacks consume before default activation', Result);
    {$ENDIF}
  finally
    LRenderer.Free;
    LLegacy.Free;
    {$IFDEF PAS2JS}
    LHost.remove;
    {$ELSE}
    LHost.Free;
    {$ENDIF}
    LDocument.Free;
  end;
end;

function RunNyxEventTargetJourney: Integer;
var
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LMemo: INyxMemo;
  LButton: INyxButton;
  LFirst: TListener;
  LSecond: TListener;
  LExit: TListener;
  LFirstLease: INyxEventCallback;
  LSecondLease: INyxEventCallback;
  LExitLease: INyxEventCallback;
  LFirstToken: INyxEventSubscription;
  LSecondToken: INyxEventSubscription;
  LExitToken: INyxEventSubscription;
  LQueuedToken: INyxEventSubscription;
  LClickToken: INyxEventSubscription;
  LChangeToken: INyxEventSubscription;
  LTrace: TTrace;
  LTraceLease: ITrace;
  LLegacy: TLegacyListener;
  {$IFDEF PAS2JS}
  LRenderer: TNyxBrowserRenderer;
  LHost: TJSHTMLElement;
  LInput: TJSHTMLTextAreaElement;
  LAction: TJSHTMLElement;
  {$ELSE}
  LRenderer: TNyxLCLRenderer;
  LHost: TForm;
  LInput: TMemo;
  LAction: TNyxLCLButton;
  {$ENDIF}
begin
  Result := RunNyxKeyboardTargetJourney;
  LDocument := TNyxDocument.Create;
  LPage := NewNyxPage('home');
  LDocument.AddPage(LPage);
  LMemo := NewNyxMemo('reply');
  LMemo.Value := 'Focus snapshot / 🌙';
  LMemo.Contract.On(ntAfterEnter, NyxOriginValue, NyxTextDomain)
    .On(ntAfterExit, NyxOriginValue, NyxTextDomain);
  LButton := NewNyxButton('send').WithText('Send');
  LPage.Add(LMemo).Add(LButton);
  LFirst := TListener.Create;
  LFirstLease := LFirst;
  LSecond := TListener.Create;
  LSecondLease := LSecond;
  LExit := TListener.Create;
  LExitLease := LExit;
  LTrace := TTrace.Create;
  LTraceLease := LTrace;
  LFirst.Trace := LTraceLease;
  LFirst.Name := 'A';
  LSecond.Trace := LTraceLease;
  LSecond.Name := 'B';
  LLegacy := TLegacyListener.Create;
  {$IFDEF PAS2JS}
  LHost := TJSHTMLElement(document.createElement('div'));
  document.body.appendChild(LHost);
  LRenderer := TNyxBrowserRenderer.Create;
  {$ELSE}
  LHost := TForm.CreateNew(nil);
  LRenderer := TNyxLCLRenderer.Create;
  {$ENDIF}
  try
    LRenderer.Render(LDocument, LPage.Node, LHost);
    {$IFDEF PAS2JS}
    LInput := TJSHTMLTextAreaElement(LRenderer.ElementFor('reply').querySelector('textarea'));
    LAction := LRenderer.ElementFor('send');
    LAction.focus;
    {$ELSE}
    LInput := TMemo(LRenderer.InputFor('reply'));
    LAction := TNyxLCLButton(LRenderer.ControlFor('send'));
    LHost.Show;
    LAction.SetFocus;
    Application.ProcessMessages;
    {$ENDIF}
    LFirstToken := LRenderer.Events.OnAfterEnter(NyxControlEvents('reply')).Subscribe(LFirstLease);
    LSecondToken := LRenderer.Events.OnAfterEnter(NyxControlEvents('reply')).Subscribe(LSecondLease);
    LExitToken := LRenderer.Events.OnAfterExit(NyxControlEvents('reply')).Subscribe(LExitLease);
    {$IFDEF PAS2JS}
    LInput.focus;
    {$ELSE}
    LInput.SetFocus;
    Application.ProcessMessages;
    {$ENDIF}
    Check((LFirst.Count = 1) and (LSecond.Count = 1) and (LTrace.Text = 'AB'),
      'real memo focus invokes both callbacks in registration order', Result);
    Check((LFirst.Last.Trigger = ntAfterEnter) and LFirst.Last.HasValue and
      (LFirst.Last.Value.AsText = TNyxText('Focus snapshot / 🌙')),
      'focus captures the declared exact scalar payload', Result);
    {$IFDEF PAS2JS}
    LAction.focus;
    {$ELSE}
    LAction.SetFocus;
    Application.ProcessMessages;
    {$ENDIF}
    Check((LExit.Count = 1) and (LExit.Last.Trigger = ntAfterExit),
      'actual focus exit follows the same portable contract', Result);
    LSecondToken.Cancel;
    {$IFDEF PAS2JS}
    LInput.focus;
    {$ELSE}
    LInput.SetFocus;
    Application.ProcessMessages;
    {$ENDIF}
    Check((LFirst.Count = 2) and (LSecond.Count = 1) and
      (LRenderer.Events.OnAfterEnter(NyxControlEvents('reply')).Count = 1),
      'removal updates visible registrations and future physical focus', Result);
    LChangeToken := LRenderer.Events.On(NyxControlEvents('reply'), ntChange).Subscribe(LExitLease);
    {$IFDEF PAS2JS}
    LInput.value := 'Accepted control edit / 🌙';
    LInput.dispatchEvent(TJSEvent.new('change'));
    {$ELSE}
    LInput.Text := TNyxText('Accepted control edit / 🌙');
    Application.ProcessMessages;
    {$ENDIF}
    Check((LExit.Last.Trigger = ntChange) and
      (LExit.Last.Value.AsText = TNyxText('Accepted control edit / 🌙')),
      'actual change event reaches the multiple-registration router', Result);
    LClickToken := LRenderer.Events.On(NyxControlEvents('send'), ntClick).Subscribe(LSecondLease);
    LAction.Click;
    Check((LSecond.Count = 2) and (LSecond.Last.Trigger = ntClick),
      'actual button click reaches its registered callback', Result);
    LFirstToken.Cancel;
    LQueuedToken := LRenderer.Events.OnAfterEnter(NyxControlEvents('reply'))
      .Policy(neUIQueue).Subscribe(LFirstLease);
    {$IFDEF PAS2JS}
    LAction.focus;
    LInput.focus;
    {$ELSE}
    LAction.SetFocus;
    LInput.SetFocus;
    {$ENDIF}
    Check(LQueuedToken.LastExecution.Status = nesPending,
      'actual focus event schedules deferred work', Result);
    LRenderer.Unmount;
    Check((LQueuedToken.LastExecution.Status = nesCancelled) and (LFirst.Count = 2),
      'unmount cancels a queued control callback before it runs', Result);
    {$IFNDEF PAS2JS}
    CheckSynchronize;
    {$ENDIF}
    LRenderer.Render(LDocument, LPage.Node, LHost);
    LSecond.Renderer := LRenderer;
    LSecond.RemoveView := True;
    LSecond.Scheduler := LRenderer.Events.Scheduler;
    LSecond.ChildWork := TSnapshotTask.Create(LFirstLease, LFirst.Last);
    LRenderer.OnEvent := LLegacy.Invoke;
    LRenderer.Events.On(NyxControlEvents('send'), ntClick).Policy(neSequential);
    {$IFDEF PAS2JS}
    LAction := LRenderer.ElementFor('send');
    {$ELSE}
    LAction := TNyxLCLButton(LRenderer.ControlFor('send'));
    {$ENDIF}
    LAction.Click;
    Check(LSecond.SawCancellation and (LRenderer.Root = nil),
      'synchronous navigation/disposal cancels its own event scope safely', Result);
    Check(LSecond.ChildTicket.Status = nesCancelled,
      'UI child work inherits cancellation even after its parent callback returned', Result);
    {$IFNDEF PAS2JS}
    CheckSynchronize;
    {$ENDIF}
    Check(LFirst.Count = 2, 'cancelled parent cannot publish a stale UI result', Result);
    Check(LLegacy.Count = 0, 'navigation suppresses the remaining legacy callback safely', Result);
    LRenderer.Free;
    LRenderer := nil;
    Check(not LClickToken.Active and not LExitToken.Active and not LChangeToken.Active,
      'renderer disposal closes every retained registration', Result);
    Check(LFirst.Last.Value.AsText = TNyxText('Focus snapshot / 🌙'),
      'retained event payload remains valid after target view disposal', Result);
  finally
    LRenderer.Free;
    LLegacy.Free;
    {$IFDEF PAS2JS}
    LHost.remove;
    {$ELSE}
    LHost.Free;
    {$ENDIF}
    LDocument.Free;
  end;
end;

end.
