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

program nyx_callback_controls_tests;

{$mode delphi}{$H+}
{$codepage utf8}

uses
  {$IFDEF PAS2JS}
  Web,
  nyx.test.keyboard.browser,
  nyx.application.browser,
  {$ELSE}
  Classes,
  Interfaces,
  Forms,
  Controls,
  StdCtrls,
  nyx.application.lcl,
  {$ENDIF}
  SysUtils,
  nyx.types,
  nyx.text,
  nyx.model,
  nyx.codec,
  nyx.test.callbacks,
  nyx.callback.fixture;

{$IFNDEF PAS2JS}
type
  TFocusAccess = class(TWinControl);
  TClickAccess = class(TControl);
  { A custom native click can replace the view while the old binding is on the
    call stack. This probe borrows its application and must not outlive it. }
  TClickNavigationProbe = class
  public
    App: TNyxLCLApplication;
    Calls: Integer;
    procedure Click(ASender: TObject);
  end;

var
  GClickNavigationProbe: TClickNavigationProbe;

procedure TClickNavigationProbe.Click(ASender: TObject);
begin
  Inc(Calls);
  App.ShowPage('other');
end;

function NavigatingLabel(ANode: TNyxNode; AOwner: TComponent): TControl;
begin
  Result := TLabel.Create(AOwner);
  TLabel(Result).Caption := ANode.Prop('text');
  TLabel(Result).OnClick := GClickNavigationProbe.Click;
end;
{$ENDIF}

var
  LDocument: TNyxDocument;
  LExpected: TNyxDocument;
  LSource: TNyxText;
  LChecks: Integer;
  {$IFDEF PAS2JS}
  LApplication: TNyxBrowserApplication;
  {$ELSE}
  LApplication: TNyxLCLApplication;
  {$ENDIF}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxModel.Create('Compiled callback controls: ' + AReason);
  end;
  Inc(LChecks);
end;

procedure Enter(const AID: TNyxText);
{$IFDEF PAS2JS}
var
  LHost: TJSHTMLElement;
  LInput: TJSHTMLElement;
begin
  LHost := LApplication.View.ElementFor(AID);
  LInput := TJSHTMLElement(LHost.querySelector('textarea'));
  LInput.focus;
end;
{$ELSE}
var
  LInput: TWinControl;
begin
  LInput := TWinControl(LApplication.View.InputFor(AID));
  Check(Assigned(TFocusAccess(LInput).OnEnter), 'actual native memo has its focus bridge');
  TFocusAccess(LInput).OnEnter(LInput);
end;
{$ENDIF}

function Keyboard(ATrigger: TNyxTrigger; ARepeating: Boolean = False): Boolean;
{$IFDEF PAS2JS}
var
  LEvent: TJSKeyboardEvent;
  LInput: TJSHTMLElement;
begin
  LInput := TJSHTMLElement(LApplication.View.ElementFor('reply-memo').querySelector('textarea'));
  LEvent := NyxTestKeyboard(ATrigger, 'Enter', [nmControl], ARepeating);
  LInput.dispatchEvent(LEvent);
  Result := LEvent.defaultPrevented;
end;
{$ELSE}
var
  LInput: TWinControl;
  LKey: Word;
begin
  LInput := TWinControl(LApplication.View.InputFor('reply-memo'));
  LKey := $0D;

  if ATrigger = ntKeyDown then
  begin
    TFocusAccess(LInput).OnKeyDown(LInput, LKey, [ssCtrl]);
  end
  else
  begin
    TFocusAccess(LInput).OnKeyUp(LInput, LKey, [ssCtrl]);
  end;
  Result := LKey = 0;
end;
{$ENDIF}

begin
  try
    {$IFNDEF PAS2JS}
    Application.Initialize;
    {$ENDIF}
    LDocument := nyx.callback.fixture.BuildNyxDocument;
    LExpected := CreateNyxCallbackFixture(LSource);
    try
      Check(TNyxCodec.Encode(LDocument) = TNyxCodec.Encode(LExpected),
        'compiled fluent callbacks reconstruct exact registration identity and inheritance');
    finally
      LExpected.Free;
    end;
    {$IFDEF PAS2JS}
    LApplication := TNyxBrowserApplication.Create;
    {$ELSE}
    LApplication := TNyxLCLApplication.Create;
    {$ENDIF}
    try
      {$IFDEF PAS2JS}
      LApplication.Run(LDocument, TJSHTMLElement(document.body));
      {$ELSE}
      LApplication.Mount(LDocument);
      {$ENDIF}
      Enter('reply-memo');
      Check(CallbackInvocations = 11, 'two handwritten callbacks execute through the real control bridge');
      LApplication.ShowPage('other');
      Check(CallbackInvocations = 11, 'navigation does not synthesize extra entry callbacks');
      LApplication.ShowPage('home');
      Enter('reply-memo');
      Check(CallbackInvocations = 22, 'remount retains registrations without duplicate installation');
      Enter('first-reply/template-reply');
      Check(CallbackInvocations = 122, 'first reusable instance executes its inherited compiled callback');
      Enter('second-reply/template-reply');
      Check(CallbackInvocations = 222, 'sibling instance resolves its independent runtime identity');
      {$IFDEF PAS2JS}
      LApplication.View.ElementFor('reply-help-label').click;
      {$ELSE}
      TClickAccess(LApplication.View.ControlFor('reply-help-label')).Click;
      {$ENDIF}
      Check(CallbackInvocations = 1222, 'a label runs its authored click callback on either adapter');
      {$IFDEF PAS2JS}
      TJSHTMLElement(LApplication.View.ElementFor('reply-memo').querySelector('textarea')).click;
      {$ELSE}
      TClickAccess(LApplication.View.InputFor('reply-memo')).Click;
      {$ENDIF}
      Check(CallbackInvocations = 11222, 'the actual framed memo input runs its authored click callback');
      Check(Keyboard(ntKeyDown) and (CallbackInvocations = 31222),
        'compiled typed Ctrl+Enter consumes the actual memo key-down');
      Check(not Keyboard(ntKeyDown, True) and (CallbackInvocations = 31222),
        'compiled one-shot shortcut admits no repeated command');
      Check(not Keyboard(ntKeyUp) and (CallbackInvocations = 61222),
        'compiled key-up observes the release and preserves its default');
      {$IFNDEF PAS2JS}
      GClickNavigationProbe := TClickNavigationProbe.Create;
      try
        GClickNavigationProbe.App := LApplication;
        LApplication.ShowPage('other');
        LApplication.View.RegisterFactory('label', NavigatingLabel);
        LApplication.ShowPage('home');
        TClickAccess(LApplication.View.ControlFor('reply-help-label')).Click;
        Check(GClickNavigationProbe.Calls = 1, 'custom native click survives the portable bridge');
        Check((CallbackInvocations = 61222) and
          (LApplication.View.Root.Find('reply-help-label') = nil),
          'custom navigation cancels the old binding before an authored click can use it');
      finally
        FreeAndNil(GClickNavigationProbe);
      end;
      {$ENDIF}
    finally
      LApplication.Free;
      LDocument.Free;
    end;
    {$IFDEF PAS2JS}
    document.body.setAttribute('data-nyx-compiled-callbacks', 'passed');
    document.body.setAttribute('data-nyx-compiled-callback-checks', IntToStr(LChecks));
    {$ELSE}
    WriteLn('PASS ', LChecks, ' compiled callback real-control checks');
    {$ENDIF}
  except
    on LException: Exception do
    begin
      {$IFDEF PAS2JS}
      document.body.setAttribute('data-nyx-compiled-callbacks', 'failed');
      document.body.setAttribute('data-nyx-compiled-callback-error', LException.Message);
      {$ELSE}
      WriteLn(LException.Message);
      Halt(1);
      {$ENDIF}
    end;
  end;
end.
