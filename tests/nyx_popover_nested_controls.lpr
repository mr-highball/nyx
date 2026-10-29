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
program nyx_popover_nested_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  {$ifdef PAS2JS}JS, Web, nyx.popover.browser, nyx.render.browser,
  {$else}Interfaces, Classes, Forms, Controls, StdCtrls, LCLType, LMessages,
    nyx.popover.lcl, nyx.render.lcl, nyx.test.capture.lcl,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.root.types, nyx.model, nyx.controls,
  nyx.events, nyx.scheduler, nyx.behavior, nyx.data, nyx.popover,
  nyx.generated.view;

type
  {$ifdef PAS2JS}
  TPopover = INyxBrowserPopover;
  TRenderer = TNyxBrowserRenderer;
  TFace = TJSHTMLElement;
  TKeyboard = class external name 'KeyboardEvent'(TJSKeyboardEvent)
    constructor new(const AType: String; const AOptions: TJSObject); reintroduce;
  end;
  TPointer = class external name 'PointerEvent'(TJSPointerEvent)
    constructor new(const AType: String; const AOptions: TJSObject); reintroduce;
  end;
  {$else}
  TPopover = INyxLCLPopover;
  TRenderer = TNyxLCLRenderer;
  TFace = TWinControl;
  {$endif}

  { Callback observes only the fixture-owned aliases. Subscriptions retire before
    fixture globals, and observations retain no document, presenter or widget. }
  TCompletion = class(TNyxEventCallback)
  public
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;
  { A real input's ordered focus callback revokes its presenting context. }
  TCloseParentOnEnter = class(TNyxEventCallback)
  public
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;
  { Retirement of a later snapshot member must invalidate its weak owner. }
  TReleaseOtherChild = class(TNyxEventCallback)
  public
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;

var
  GChecks: Integer;
  GDocument: TNyxDocument;
  GBackground: TNyxDocument;
  GView: TRenderer;
  GParent: TPopover;
  GChild: TPopover;
  GGrandchild: TPopover;
  GSubscription: INyxEventSubscription;
  GCompletions: Integer;
  GAttemptReopen: Boolean;
  GReopenRefused: Boolean;
  {$ifdef PAS2JS}
  GHost: TJSHTMLElement;
  GResumeTimer: NativeInt;
  {$else}
  GHost: TForm;
  {$endif}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise ENyxModel.Create('Nested popover: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure Pump;
begin
  {$ifndef PAS2JS}Application.ProcessMessages;{$endif}
end;

procedure Focus(AFace: TFace);
begin
  {$ifdef PAS2JS}NyxFocusWithoutScroll(AFace);
  {$else}AFace.SetFocus;{$endif}
end;

function Focused(AFace: TFace): Boolean;
begin
  {$ifdef PAS2JS}Result := document.activeElement = AFace;
  {$else}Result := Screen.ActiveControl = AFace;{$endif}
end;

function NewChild(const AParent: TPopover): TPopover;
begin
  {$ifdef PAS2JS}
  Result := NewNyxBrowserPopover(AParent.Renderer.FocusFor('notes-close'),
    GDocument, NyxPageRoot('quick-notes'));
  {$ifdef NYX_BASELINE_NESTING}
  { Exact prior browser menu seam declares physical containment manually. This
    comparison preserves that historical relation without the new coordinator. }
  AParent.Element.appendChild(Result.Element);
  {$endif}
  {$else}
  Result := NewNyxLCLPopover(AParent.Renderer.FocusFor('notes-close'),
    GDocument, NyxPageRoot('quick-notes'), nil
    {$ifdef NYX_BASELINE_NESTING}, AParent{$endif});
  {$endif}
  {$ifndef NYX_BASELINE_NESTING}
  Result.Within(AParent);
  {$endif}
end;

procedure OpenChild(const AChild, AParent: TPopover; const ATitle: TNyxText);
begin
  Focus(AParent.Renderer.FocusFor('notes-close'));
  AChild.Open(NyxPopover(ATitle).Size(280, 360).Placement(npsRight, npaStart)
    .Focus(NyxPart('notes')));
  Pump;
end;

procedure Press(AFace: TFace);
{$ifdef PAS2JS}
var
  LOptions: TJSObject;
{$else}
var
  LMessage: TLMessage;
{$endif}
begin
  Focus(AFace);
  {$ifdef PAS2JS}
  LOptions := TJSObject.new;
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;
  AFace.dispatchEvent(TPointer.new('pointerdown', LOptions));
  {$else}
  LMessage := Default(TLMessage);
  LMessage.Msg := LM_LBUTTONDOWN;
  Application.NotifyUserInputHandler(AFace, LMessage);
  {$endif}
  Pump;
end;

procedure Escape(AFace: TFace);
{$ifdef PAS2JS}
var
  LOptions: TJSObject;
  LEvent: TKeyboard;
{$else}
var
  LKey: Word;
{$endif}
begin
  Focus(AFace);
  {$ifdef PAS2JS}
  LOptions := TJSObject.new;
  LOptions['key'] := 'Escape';
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;
  LEvent := TKeyboard.new('keydown', LOptions);
  AFace.dispatchEvent(LEvent);
  Check(LEvent.defaultPrevented, 'only owning Escape is consumed');
  {$else}
  LKey := VK_ESCAPE;
  Application.NotifyKeyDownHandler(AFace, LKey, []);
  Check(LKey = 0, 'only owning native Escape is consumed');
  {$endif}
  Pump;
end;

procedure TCompletion.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  Inc(GCompletions);
  Check(NyxPopoverDismissReason(AEvent) = nprAnchorUnavailable,
    'parent closure supplies the typed child retirement reason');

  if GAttemptReopen then
  begin
    try
      GChild.Open(NyxPopover('Cannot escape a closing parent'));
    except
      on ENyxModel do
      begin
        GReopenRefused := True;
      end;
    end;
  end;
end;

procedure TCloseParentOnEnter.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  GParent.Dismiss(nprOutsidePress);
end;

procedure TReleaseOtherChild.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  GChild := nil;
end;

procedure RunChecks;
var
  LRefused: Boolean;
  LForeign: TPopover;
  LRetained: INyxControl;
  LBefore: Integer;
  LFocusToken: INyxEventSubscription;
  LAdmitted: INyxPopover;
  LSibling: TPopover;
  LSiblingToken: INyxEventSubscription;
begin
  try
    Press(GGrandchild.Renderer.FocusFor('notes-memo'));
    Check(GParent.IsOpen and GChild.IsOpen and GGrandchild.IsOpen,
      'grandchild delivered input belongs to every declared ancestor');
    {$ifdef PAS2JS}
    { Near-invoker DOM placement is another valid manual top-layer layout.
      Parent document listeners must still leave Escape with the inner popup. }
    GChild.Element.appendChild(GGrandchild.Element);
    {$endif}
    Escape(GGrandchild.Renderer.FocusFor('notes-memo'));
    Check(not GGrandchild.IsOpen and GParent.IsOpen and GChild.IsOpen,
      'grandchild Escape leaves both ancestors open');
    Check(Focused(GChild.Renderer.FocusFor('notes-close')),
      'grandchild Escape returns to its exact invoking part');
    Escape(GChild.Renderer.FocusFor('notes-memo'));
    Check(not GChild.IsOpen and GParent.IsOpen,
      'child Escape leaves the root presentation open');
    Check(Focused(GParent.Renderer.FocusFor('notes-close')),
      'child Escape returns to its exact invoking part');

    {$ifndef NYX_BASELINE_NESTING}
    { Retire the fluent result explicitly: pas2js may otherwise keep an ignored
      interface temporary until the end of this procedure, after the teardown
      assertions. That is a caller lease, not a presenter/graph retention cycle. }
    LAdmitted := GChild.Within(GParent);
    Check((LAdmitted <> nil) and (LAdmitted.State = GChild.State),
      'fluent nesting retains the same independent runtime store');
    LAdmitted := nil;
    {$endif}
    OpenChild(GChild, GParent, 'Nested notes');
    OpenChild(GGrandchild, GChild, 'One more thought');
    GSubscription := GChild.OnDismiss.Subscribe(TCompletion.Create);
    GAttemptReopen := True;
    GParent.Close;
    Check(not GParent.IsOpen and not GChild.IsOpen and not GGrandchild.IsOpen,
      'closing parent synchronously closes every descendant');
    Check(GChild.LastReason = nprAnchorUnavailable,
      'child closure does not masquerade as its own Escape');
    Check((GCompletions = 1) and GReopenRefused,
      'same-parent admission is unique and child completion cannot reopen into closure');
    GAttemptReopen := False;

    {$ifndef NYX_BASELINE_NESTING}
    LRefused := False;
    try
      GParent.Within(GGrandchild);
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'closed retained ancestry cannot become a cycle');
    {$endif}
    Focus(GView.FocusFor('open-notes'));
    GParent.Open(NyxPopover('Quick notes').Size(340, 420).Focus(NyxPart('notes')));
    OpenChild(GChild, GParent, 'Nested notes');
    Press(GParent.Renderer.FocusFor('notes-memo'));
    Check(GParent.IsOpen and not GChild.IsOpen,
      'pressing parent closes its child without dismissing the parent');
    OpenChild(GChild, GParent, 'Nested notes');
    Press(GView.FocusFor('keep-working'));
    Check(not GParent.IsOpen and not GChild.IsOpen,
      'genuine outside input closes the family');
    Check(Focused(GView.FocusFor('keep-working')),
      'family outside dismissal preserves the delivered focus destination');

    {$ifndef NYX_BASELINE_NESTING}
    Focus(GView.FocusFor('open-notes'));
    GParent.Open(NyxPopover('Quick notes').Size(340, 420).Focus(NyxPart('notes')));
    LFocusToken := GChild.Events.On(NyxControlEvents('notes-memo', niRuntime),
      ntAfterEnter).Subscribe(TCloseParentOnEnter.Create);
    LRefused := False;
    try
      OpenChild(GChild, GParent, 'Revoked during focus');
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and not GParent.IsOpen and not GChild.IsOpen,
      'actual focus callback retirement refuses pending child publication');
    LFocusToken.Cancel;
    LFocusToken := nil;
    {$endif}

    Focus(GView.FocusFor('open-notes'));
    GParent.Open(NyxPopover('Quick notes').Size(340, 420).Focus(NyxPart('notes')));
    {$ifndef NYX_BASELINE_NESTING}
    {$ifdef PAS2JS}
    LForeign := NewNyxBrowserPopover(GView.FocusFor('keep-working'), GDocument,
      NyxPageRoot('quick-notes'));
    {$else}
    LForeign := NewNyxLCLPopover(GView.FocusFor('keep-working'), GDocument,
      NyxPageRoot('quick-notes'));
    {$endif}
    LRefused := False;
    try
      LForeign.Within(GParent);
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and GParent.IsOpen, 'unrelated anchor refuses without disturbing its candidate parent');
    LForeign := nil;
    {$ifdef PAS2JS}
    LForeign := NewNyxBrowserPopover(GView.FocusFor('open-notes'), GDocument,
      NyxPageRoot('quick-notes'));
    {$else}
    LForeign := NewNyxLCLPopover(GView.FocusFor('open-notes'), GDocument,
      NyxPageRoot('quick-notes'));
    {$endif}
    LRefused := False;
    try
      LForeign.Within(GParent);
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and GParent.IsOpen, 'shared background invoker is not a nested content anchor');
    LForeign := nil;
    OpenChild(GChild, GParent, 'Nested notes');
    LSibling := NewChild(GParent);
    OpenChild(LSibling, GParent, 'Another note');
    LSiblingToken := LSibling.OnDismiss.Subscribe(TReleaseOtherChild.Create);
    GParent.Close;
    Check((GChild = nil) and not LSibling.IsOpen and not GParent.IsOpen,
      'completion may retire a later child without using its stale snapshot pointer');
    LSiblingToken.Cancel;
    LSiblingToken := nil;
    LSibling := nil;
    GGrandchild := nil;
    GSubscription := nil;
    Focus(GView.FocusFor('open-notes'));
    GParent.Open(NyxPopover('Quick notes').Size(340, 420).Focus(NyxPart('notes')));
    GChild := NewChild(GParent);
    OpenChild(GChild, GParent, 'Nested notes');
    GGrandchild := NewChild(GChild);
    GChild.Close;
    GSubscription := GChild.OnDismiss.Subscribe(TCompletion.Create);
    {$endif}
    OpenChild(GChild, GParent, 'Nested notes');
    OpenChild(GGrandchild, GChild, 'One more thought');
    LRetained := GChild.Content;
    LBefore := GCompletions;
    GParent := nil;
    Check(not GChild.IsOpen and not GGrandchild.IsOpen,
      'weak graph does not extend retired ancestor presentation lifetime');
    Check(GCompletions = LBefore, 'destruction retires descendants silently');
    Check(LRetained.ID = 'quick-notes', 'retained child content survives family retirement');
    GSubscription := nil;
    GGrandchild := nil;
    GChild := nil;
    LRetained := nil;
    {$ifdef PAS2JS}
    Check(document.querySelectorAll('.nyx-popover').length = 0,
      'family hosts and subscriptions retire completely');
    document.body.setAttribute('data-nested-popover-checks', IntToStr(GChecks));
    document.body.setAttribute('data-nested-popover', 'passed');
    {$else}
    WriteLn('PASS ', GChecks, ' actual nested popover checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-nested-popover', 'failed');
      document.body.setAttribute('data-nested-popover-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end;

{$ifdef PAS2JS}
procedure ResumeAfterCapture;
begin

  if document.body.getAttribute('data-capture-observed') = 'nested-popovers' then
  begin
    window.clearInterval(GResumeTimer);
    RunChecks;
  end;
end;
{$endif}

procedure Prepare;
var
  LPage: INyxPage;
begin
  GDocument := BuildNyxDocument;
  GBackground := TNyxDocument.Create;
  LPage := NewNyxPage('background');
  LPage.Configure.Padding(24).Done;
  LPage.Add(NewNyxHeading('heading').WithText('A thought within a thought'));
  LPage.Add(NewNyxButton('open-notes').WithText('Quick notes'));
  LPage.Add(NewNyxButton('keep-working').WithText('Keep working'));
  GBackground.AddPage(LPage);
  {$ifdef PAS2JS}
  GHost := TJSHTMLElement(document.createElement('main'));
  document.body.appendChild(GHost);
  {$else}
  Application.Initialize;
  Application.CaptureExceptions := False;
  GHost := TForm.CreateNew(nil);
  GHost.ClientWidth := 1000;
  GHost.ClientHeight := 780;
  GHost.Show;
  {$endif}
  GView := TRenderer.Create;
  GView.Render(GBackground, LPage.Node, GHost);
  Focus(GView.FocusFor('open-notes'));
  {$ifdef PAS2JS}
  GParent := NewNyxBrowserPopover(GView.FocusFor('open-notes'), GDocument,
    NyxPageRoot('quick-notes'));
  {$else}
  GParent := NewNyxLCLPopover(GView.FocusFor('open-notes'), GDocument,
    NyxPageRoot('quick-notes'));
  {$endif}
  GParent.Open(NyxPopover('Quick notes').Size(340, 420).Focus(NyxPart('notes')));
  GChild := NewChild(GParent);
  GChild.Content.Part(NyxPart('title')).Configure.Text('Nested notes').Done;
  OpenChild(GChild, GParent, 'Nested notes');
  GGrandchild := NewChild(GChild);
  GGrandchild.Content.Part(NyxPart('title')).Configure.Text('One more thought').Done;
  OpenChild(GGrandchild, GChild, 'One more thought');
  {$ifdef PAS2JS}
  document.body.setAttribute('data-capture-checkpoint', 'nested-popovers');
  { Opt-in noninteractive execution releases only the screenshot checkpoint;
    the original nested input, focus and retirement assertions remain intact. }

  if window.location.search = '?run' then
  begin
    document.body.setAttribute('data-capture-observed', 'nested-popovers');
  end;
  GResumeTimer := window.setInterval(@ResumeAfterCapture, 25);
  {$else}

  if ParamCount > 0 then
  begin
    GGrandchild.Window.BringToFront;
    Pump;
    SaveNyxNativeCapture(GGrandchild.Window, TNyxText(ParamStr(1)), ncmDisplay);
  end;
  RunChecks;
  {$endif}
end;

begin
  try
    Prepare;
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-nested-popover', 'failed');
      document.body.setAttribute('data-nested-popover-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
  {$ifndef PAS2JS}
  GSubscription := nil;
  GGrandchild := nil;
  GChild := nil;
  GParent := nil;
  GView.Free;
  GDocument.Free;
  GBackground.Free;
  GHost.Free;
  {$endif}
end.
