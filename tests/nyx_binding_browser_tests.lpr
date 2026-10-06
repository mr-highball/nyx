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

program nyx_binding_browser_tests;

{$mode delphi}{$H+}
{$codepage utf8}

uses
  nyx.types,
  nyx.behavior,
  SysUtils,
  Web,
  nyx.text,
  nyx.state,
  nyx.binding.types,
  nyx.model,
  nyx.render.browser,
  nyx.application.browser,
  nyx.test.binding;

var
  GRejectUpdate: Boolean = False;

function CaptionFactory(ANode: TNyxNode): TJSHTMLElement;
var
  LCaption: TJSHTMLElement;
  LDecoration: TJSHTMLElement;
begin
  Result := TJSHTMLElement(document.createElement('div'));
  LCaption := TJSHTMLElement(document.createElement('span'));
  LCaption.className := 'custom-caption';
  Result.appendChild(LCaption);
  LDecoration := TJSHTMLElement(document.createElement('span'));
  LDecoration.textContent := ' •';
  Result.appendChild(LDecoration);
end;

procedure UpdateCaption(ANode: TNyxNode; AElement: TJSHTMLElement);
begin

  if GRejectUpdate then
  begin
    raise ENyxState.Create('Admitted custom notification failed');
  end;
  AElement.querySelector('.custom-caption').textContent := ANode.Prop('text');
end;

procedure RejectCaptionUpdate(ANode: TNyxNode; AElement: TJSHTMLElement);
begin
  raise ENyxState.Create('Custom candidate updater rejected');
end;

type
  TBrowserBindingJourney = class
  public
    Calls: Integer;
    Reject: Boolean;
    LastInfo: TNyxEventInfo;
    LastSourceID: TNyxText;
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure Validate(ACandidate: TNyxState; AChanges: TNyxStateChanges);
    function Run: Integer;
    function CustomJourney: Integer;
  end;

procedure TBrowserBindingJourney.Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin
  LastInfo := AEvent.Copy;
  LastSourceID := ANode.ID;
  Inc(Calls);
end;

function TBrowserBindingJourney.CustomJourney: Integer;
var
  LDocument: TNyxDocument;
  LRenderer: TNyxBrowserRenderer;
  LHost: TJSHTMLElement;
  LCaption: TJSHTMLElement;
  LRoot: TNyxNode;
  LRejected: Boolean;
  LMemo: TJSHTMLTextAreaElement;
  LStore: TNyxState;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise ENyxState.Create('Custom browser binding: ' + AReason);
    end;
    Inc(Result);
  end;

begin
  Result := 0;
  LDocument := CreateNyxBindingFixture;
  LRenderer := TNyxBrowserRenderer.Create;
  LHost := TJSHTMLElement(document.createElement('div'));
  document.body.appendChild(LHost);
  try
    LRenderer.RegisterFactory('label', @CaptionFactory, @UpdateCaption);
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    LCaption := LRenderer.ElementFor('reply-caption');
    LRoot := LRenderer.Root;
    Check((LCaption.querySelector('.custom-caption').textContent = 'Café / 🌙 / 漢字') and
      (LCaption.childElementCount = 2), 'custom caption retains extension markup');
    LRenderer.State.SetValue(NyxTextState('🌙/reply'), 'Updated custom caption');
    Check((LRenderer.ElementFor('reply-caption') = LCaption) and
      (LCaption.querySelector('.custom-caption').textContent = 'Updated custom caption') and
      (LCaption.childElementCount = 2), 'custom updater retains target and children');
    LMemo := TJSHTMLTextAreaElement(LRenderer.ElementFor('reply-memo').querySelector('textarea'));
    GRejectUpdate := True;
    LMemo.value := 'Committed despite repaint failure';
    LMemo.dispatchEvent(TJSEvent.new('change'));
    Check((LRenderer.State.GetValue(NyxTextState('🌙/reply')) = LMemo.value) and
      (LRenderer.Root.Find('reply-memo').Prop('value') = LMemo.value) and
      (LRenderer.LastBindingFailure = nbfNotificationFailed),
      'failed notification reports committed state without retry or rollback');
    GRejectUpdate := False;
    LRenderer.State.SetValue(NyxTextState('🌙/reply'), 'Recovered custom view');
    Check(LCaption.querySelector('.custom-caption').textContent = 'Recovered custom view',
      'custom view recovers through its existing subscription');
    LMemo.value := 'Accepted after recovery';
    LMemo.dispatchEvent(TJSEvent.new('change'));
    Check((LRenderer.LastBindingError = '') and (LRenderer.LastBindingFailure = nbfNone),
      'accepted control command clears the notification diagnostic');
    LRenderer.RegisterFactory('label', @CaptionFactory, @RejectCaptionUpdate);
    LRejected := False;
    try
      LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LRenderer.Root = LRoot) and
      (LRenderer.ElementFor('reply-caption') = LCaption), 'failed updater retains accepted view');
    LRenderer.State.SetValue(NyxTextState('🌙/reply'), 'Accepted updater remains attached');
    Check(LCaption.querySelector('.custom-caption').textContent = 'Accepted updater remains attached',
      'registry changes preserve mounted updater ownership');
    LRenderer.RegisterFactory('label', @CaptionFactory);
    LRejected := False;
    try
      LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LRenderer.Root = LRoot) and
      (LRenderer.ElementFor('reply-caption') = LCaption), 'missing updater preserves accepted view');
    LRenderer.RegisterFactory('label', @CaptionFactory, @UpdateCaption);
    LStore := LRenderer.State;
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost, False, LStore);
    Check((LRenderer.State = LStore) and
      (LRenderer.State.GetValue(NyxTextState('🌙/reply')) = 'Accepted updater remains attached'),
      'explicit owned-store reuse survives a full remount');
    LRenderer.State.SetValue(NyxTextState('🌙/reply'), 'After owned-store remount');
    Check(LRenderer.ElementFor('reply-caption').querySelector('.custom-caption').textContent =
      'After owned-store remount', 'remounted owned store keeps an active coordinator');
  finally
    GRejectUpdate := False;
    LRenderer.Free;
    LDocument.Free;
  end;
end;

procedure TBrowserBindingJourney.Validate(ACandidate: TNyxState; AChanges: TNyxStateChanges);
begin

  if Reject then
  begin
    raise ENyxState.Create('Domain command rejected');
  end;
end;

function TBrowserBindingJourney.Run: Integer;
var
  LDocument: TNyxDocument;
  LApplication: TNyxBrowserApplication;
  LToken: TNyxStateSubscription;
  LMemo: TJSHTMLTextAreaElement;
  LMirror: TJSHTMLTextAreaElement;
  LNumber: TJSHTMLInputElement;
  LCheckbox: TJSHTMLInputElement;
  LHost: TJSHTMLElement;
  LNode: TNyxNode;
  LRevision: Integer;
  LCalls: Integer;
  LRejected: Boolean;
  LRetained: TNyxEventInfo;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise ENyxState.Create('Browser binding: ' + AReason);
    end;
    Inc(Result);
  end;

  procedure Change(AInput: TJSHTMLElement);
  begin
    AInput.dispatchEvent(TJSEvent.new('change'));
  end;

begin
  Result := 0;
  LDocument := CreateNyxBindingFixture;
  LApplication := TNyxBrowserApplication.Create;
  LToken := nil;
  LHost := TJSHTMLElement(document.createElement('div'));
  document.body.appendChild(LHost);
  try
    LApplication.View.OnEvent := @Event;
    LApplication.Run(LDocument, LHost);
    LToken := LApplication.State.Subscribe(nil, @Validate);
    LMemo := TJSHTMLTextAreaElement(LApplication.View.ElementFor('reply-memo').querySelector('textarea'));
    LMirror := TJSHTMLTextAreaElement(LApplication.View.ElementFor('reply-mirror').querySelector('textarea'));
    LNumber := TJSHTMLInputElement(LApplication.View.ElementFor('ratio-input').querySelector('input'));
    LCheckbox := TJSHTMLInputElement(LApplication.View.ElementFor('remember-checkbox').querySelector('input'));
    LNode := LApplication.View.Root.Find('reply-memo');
    Check((LMemo.value = 'Café / 🌙 / 漢字') and (LNumber._type = 'number') and
      (LNumber.value = '0.1'), 'typed defaults reach actual controls');
    NyxFocusWithoutScroll(LMemo);
    LMemo.value := 'Crafted / 🌙' + #10 + 'Second line';
    LMemo.selectionStart := 3;
    LMemo.selectionEnd := 7;
    Change(LMemo);
    Check((LApplication.State.GetValue(NyxTextState('🌙/reply')) = LMemo.value) and
      (LMirror.value = LMemo.value) and
      (LApplication.View.ElementFor('reply-caption').textContent = LMemo.value),
      'physical memo edit synchronizes state, caption and sibling');
    Check((document.activeElement = LMemo) and (LMemo.selectionStart = 3) and
      (LMemo.selectionEnd = 7) and
      (LApplication.View.Root.Find('reply-memo') = LNode),
      'accepted edit preserves focused control, selection and node identity');
    Check(Calls = 1, 'one physical edit emits one semantic event');
    Check((LastInfo.Trigger = ntChange) and LastInfo.IsNamed(NyxEvent('change')) and
      (LastInfo.SourceID = 'reply-memo') and (LastSourceID = LastInfo.SourceID) and
      (LastInfo.OriginID = 'reply-memo') and (LastInfo.TargetID = 'reply-memo'),
      'browser callback carries typed routing');
    Check(LastInfo.HasValue and (LastInfo.ValueKind = nskText) and
      (LastInfo.Value.AsText = LMemo.value), 'browser callback owns exact text');
    LRetained := LastInfo.Copy;
    LCalls := Calls;
    LApplication.State.SetValue(NyxBooleanState('checked'), True);
    Check(LCheckbox.checked and (Calls = LCalls), 'programmatic Boolean update causes no feedback event');
    LCheckbox.checked := False;
    Change(LCheckbox);
    Check(not LApplication.State.GetValue(NyxBooleanState('checked')) and (Calls = LCalls + 1),
      'physical checkbox edit retains Boolean state');
    Check((LastInfo.ValueKind = nskBoolean) and not LastInfo.Value.AsBoolean,
      'browser callback keeps Boolean value type');
    LNumber.value := '0.125';
    Change(LNumber);
    Check(LApplication.State.GetValue(NyxNumberState('ratio')) = 0.125,
      'physical numeric input writes Double state');
    Check((LastInfo.ValueKind = nskNumber) and (LastInfo.Value.AsNumber = 0.125),
      'browser callback keeps finite number type');
    LRevision := LApplication.State.Revision;
    LCalls := Calls;
    LNumber.value := 'invalid';
    Change(LNumber);
    Check((LApplication.State.Revision = LRevision) and (LNumber.value = '0.125') and
      (Calls = LCalls) and (LApplication.View.LastBindingError <> ''),
      'invalid numeric edit restores accepted value without a semantic event');
    Check((LastInfo.TargetID = 'ratio-input') and (LastInfo.Value.AsNumber = 0.125),
      'rejected edit cannot replace last browser event');
    Reject := True;
    LMemo.value := 'Rejected draft';
    Change(LMemo);
    Check((LApplication.State.Revision = LRevision) and
      (LMemo.value = LApplication.State.GetValue(NyxTextState('🌙/reply'))) and
      (document.activeElement = LMemo), 'domain rejection restores the same focused memo');
    Reject := False;
    LApplication.View.ElementFor(LApplication.View.Root.Find('quantity-stepper').Part('increment').ID).click;
    Check((LApplication.State.GetValue(NyxIntegerState('quantity')) = 3) and
      (LApplication.View.ElementFor('quantity-caption').textContent = '3') and
      (LApplication.View.LastBindingError = ''), 'compound increment synchronizes integer consumers');
    Check((LastInfo.Trigger = ntClick) and LastInfo.IsNamed(NyxEvent('increment')) and
      (LastInfo.SourceID = 'quantity-stepper') and (LastSourceID = 'quantity-stepper') and
      (LastInfo.OriginID = LApplication.View.Root.Find('quantity-stepper').Part('increment').ID) and
      (LastInfo.TargetID = LApplication.View.Root.Find('quantity-stepper').Part('value').ID),
      'browser compound callback separates source, origin and target');
    Check((LastInfo.ValueKind = nskInteger) and (LastInfo.Value.AsInteger = 3),
      'browser compound callback owns admitted integer');
    LCalls := Calls;
    LMemo.value := 'Unfinished focused draft';
    LMemo.selectionStart := 4;
    LMemo.selectionEnd := 9;
    LApplication.State.SetValue(NyxIntegerState('quantity'), 4);
    Check((LMemo.value = 'Unfinished focused draft') and (LMemo.selectionStart = 4) and
      (LMemo.selectionEnd = 9) and (document.activeElement = LMemo) and (Calls = LCalls),
      'unrelated state update preserves unfinished focused draft');
    Change(LMemo);
    LApplication.State.SetValue(NyxBooleanState('readonly'), True);
    Check(LMemo.readOnly, 'read-only state reaches textarea');
    LApplication.State.SetValue(NyxBooleanState('readonly'), False);
    LApplication.State.SetValue(NyxBooleanState('enabled'), False);
    Check(LMemo.disabled and LCheckbox.disabled, 'ancestor disabled state reaches actual controls');
    LApplication.State.SetValue(NyxBooleanState('enabled'), True);
    Check(not LMemo.disabled and not LCheckbox.disabled, 'enabled controls recover in place');
    LApplication.State.SetValue(NyxBooleanState('visible'), False);
    Check(LApplication.View.ElementFor('reply-caption').style.getPropertyValue('display') = 'none',
      'visibility false hides caption');
    LApplication.State.SetValue(NyxBooleanState('visible'), True);
    Check(LApplication.View.ElementFor('reply-caption').style.getPropertyValue('display') <> 'none',
      'visibility true restores caption');
    LApplication.State.SetValue(NyxIntegerState('width'), 420);
    Check(LApplication.View.ElementFor('reply-memo').style.getPropertyValue('width') = '420px',
      'layout state updates the existing field');
    LApplication.ShowPage('review');
    Check(LApplication.View.ElementFor('review-caption').textContent = 'Unfinished focused draft',
      'navigation keeps application state');
    Check((LRetained.SourceID = 'reply-memo') and
      (LRetained.Value.AsText = TNyxText('Crafted / 🌙' + #10 + 'Second line')),
      'browser retained event survives navigation and control destruction');
    LRevision := LApplication.State.Revision;
    LRejected := False;
    try
      LApplication.State.SetValue(NyxIntegerState('quantity'), 1000);
    except
      on LException: ENyxState do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LApplication.State.Revision = LRevision),
      'unmounted page constraints reject invalid updates');
    LApplication.ShowPage('editor');
    LMemo := TJSHTMLTextAreaElement(LApplication.View.ElementFor('reply-memo').querySelector('textarea'));
    Check((LMemo.value = 'Unfinished focused draft') and
      (LDocument.State.GetValue(NyxTextState('🌙/reply')) = 'Café / 🌙 / 漢字'),
      'returning page reconstructs runtime values while defaults remain unchanged');
    LApplication.View.ElementFor(LApplication.View.Root.Find('search').Part('clear').ID).click;
    Check((LMemo.value = '') and (LApplication.State.GetValue(NyxTextState('🌙/reply')) = ''),
      'compound clear updates mounted field and state');
    Check(LastInfo.HasValue and (LastInfo.ValueKind = nskText) and
      (LastInfo.Value.AsText = '') and (LastInfo.SourceID = 'search'),
      'browser clear callback distinguishes empty text from absent data');
    LApplication.State.SetValue(NyxTextState('🌙/reply'), 'Declared query / 🌙');
    LApplication.View.ElementFor(LApplication.View.Root.Find('search').Part('search').ID).click;
    Check(LastInfo.HasValue and (LastInfo.Value.AsText = TNyxText('Declared query / 🌙')) and
      (LastInfo.ValueID = LApplication.View.Root.Find('search').Part('query').ID) and
      (LastInfo.TargetID = LApplication.View.Root.Find('search').Part('search').ID),
      'browser search declares a payload separate from its command target');
    LApplication.View.ElementFor('fixture-ratio-commit').click;
    Check((LastInfo.ValueKind = nskNumber) and (LastInfo.Value.AsNumber = 0.125) and
      (LastInfo.ValueID = 'fixture-ratio-input'), 'browser named field payload is numeric');
    LNumber := TJSHTMLInputElement(LApplication.View.ElementFor('fixture-ratio-input')
      .querySelector('input'));
    LNumber.value := '0.5';
    LNumber.dispatchEvent(TJSEvent.new('change'));
    Check((LApplication.View.Root.Find('fixture-ratio-input').Prop('value') = '0.5') and
      (LastInfo.ValueKind = nskNumber), 'browser field admits a declared choice');
    LCalls := Calls;
    LNumber.value := '0.2';
    LNumber.dispatchEvent(TJSEvent.new('change'));
    Check((LNumber.value = '0.5') and (Calls = LCalls) and
      (LApplication.View.LastBindingError <> ''), 'browser rejects an undeclared choice atomically');
    LApplication.View.ElementFor(LApplication.View.Root.Find('fixture-rating').Part('star-5').ID).click;
    Check((LastInfo.ValueKind = nskInteger) and (LastInfo.Value.AsInteger = 5),
      'browser rating selection emits an integer');
    LNumber := TJSHTMLInputElement(LApplication.View.ElementFor('fixture-integer-input')
      .querySelector('input'));
    LNumber.value := '2147483646';
    LNumber.dispatchEvent(TJSEvent.new('change'));
    Check((LastInfo.ValueKind = nskInteger) and (LastInfo.Value.AsInteger = 2147483646),
      'browser numeric input retains the full declared integer family');
    LCalls := Calls;
    LNumber.value := '1.5';
    LNumber.dispatchEvent(TJSEvent.new('change'));
    Check((LNumber.value = '2147483646') and (Calls = LCalls),
      'browser integer domain rejects fractions without narrowing or notification');
  finally
    LToken.Free;
    LApplication.Free;
    LDocument.Free;
  end;
  Check(LRetained.Value.AsText = TNyxText('Crafted / 🌙' + #10 + 'Second line'),
    'browser retained event outlives its application');
end;

var
  LJourney: TBrowserBindingJourney;
  LCount: Integer;
begin
  LJourney := TBrowserBindingJourney.Create;
  try
    try
      LCount := LJourney.Run + LJourney.CustomJourney;
      document.body.setAttribute('data-binding-tests', 'passed');
      document.body.setAttribute('data-binding-checks', IntToStr(LCount));
    except
      on LException: Exception do
      begin
        document.body.textContent := 'FAIL ' + LException.Message;
        document.body.setAttribute('data-binding-tests', 'failed');
        document.body.setAttribute('data-binding-error', LException.Message);
      end;
    end;
  finally
    LJourney.Free;
  end;
end.
