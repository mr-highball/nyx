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

program nyx_studio_authoring_tests;

{$mode delphi}{$H+}
{$codepage utf8}

uses
  SysUtils,
  Web,
  nyx.text,
  nyx.binding.types,
  nyx.test.source.managed,
  nyx.test.source.structural,
  nyx.studio.authoring,
  nyx.studio.inspector,
  nyx.studio.source,
  nyx.studio.browser;

var
  GStudio: TNyxStudio;
  GChecks: Integer;
  GCompact: Boolean;
  GFrame: TJSHTMLIFrameElement;
  GPolls: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Studio authoring: ' + AReason);
  end;
  Inc(GChecks);
end;

function Find(const AID: TNyxText): TJSHTMLElement;
begin
  Result := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));

  if (Result = nil) and GCompact then
  begin
    { Compact Studio exposes the same Nyx fields through its panel navigation. }

    if (Pos('state-', AID) = 1) or (AID = NyxStudioStateToggleID) or
      (AID = NyxStudioAddStateID) then
    begin
      TJSHTMLButtonElement(document.querySelector('[data-node=action-panel-project]')).click;
    end
    else if (Pos('binding-', AID) = 1) or (Pos('inspector-', AID) = 1) or
      (Pos('event-', AID) = 1) or
      (AID = 'action-advanced-properties') or
      (AID = NyxStudioBindingsToggleID) then
    begin
      TJSHTMLButtonElement(document.querySelector('[data-node=action-panel-inspector]')).click;
    end
    else
    begin
      TJSHTMLButtonElement(document.querySelector('[data-node=action-panel-design]')).click;
    end;
    Result := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));
  end;

  if Result = nil then
  begin
    raise Exception.Create('Missing Studio control: ' + AID);
  end;
end;

function Field(const AID: TNyxText): TJSHTMLElement;
begin
  Result := Find(AID);

  if not ((Result is TJSHTMLInputElement) or (Result is TJSHTMLTextAreaElement) or
    (Result is TJSHTMLSelectElement)) then
  begin
    Result := TJSHTMLElement(Result.querySelector('input,textarea,select'));
  end;

  if Result = nil then
  begin
    raise Exception.Create('Missing Studio editor: ' + AID);
  end;
end;

procedure Change(const AID, AValue: TNyxText);
var
  LField: TJSHTMLElement;
begin
  LField := Field(AID);
  TJSHTMLInputElement(LField).value := AValue;
  LField.dispatchEvent(TJSEvent.new('change'));
end;

procedure Click(const AID: TNyxText);
begin
  TJSHTMLButtonElement(Find(AID)).click;
end;

function Source: TNyxText;
begin
  Result := TJSHTMLTextAreaElement(Field('studio-code')).value;
end;

procedure CreateDefault(const AName, AType, AValue: TNyxText);
begin
  Change(NyxStudioNewStateNameID, AName);
  Change(NyxStudioNewStateInputID, AType);
  Change(NyxStudioNewStateValueID, AValue);
  Click(NyxStudioAddStateID);
end;

procedure Run;
var
  LButton: TJSHTMLElement;
  LMemo: TJSHTMLTextAreaElement;
  LBefore: TNyxText;
  LField: TJSHTMLElement;
  LSourceDraft: TNyxText;
begin
  GStudio := TNyxStudio.Create;
  GStudio.Run(False);
  GCompact := window.innerWidth < 900;
  Click('action-code');
  Click(NyxStudioStateToggleID);
  LButton := Find(NyxStudioAddStateID);
  Change(NyxStudioNewStateNameID, 'reply 🌙');
  Change(NyxStudioNewStateValueID, 'Starting reply / 🌙');
  Check(Find(NyxStudioAddStateID) = LButton,
    'committing new-default drafts retains the Add control through field blur');
  Click(NyxStudioAddStateID);
  Check((Pos('LReplyTextState: TNyxTextStateRef;', Source) > 0) and
    (Pos('Starting reply / 🌙', Source) > 0), 'typed default reaches crafted source immediately');
  Check(TJSHTMLInputElement(Field(NyxStudioNewStateNameID)).value = '',
    'successful creation resets its name while keeping the selected type');
  Find('project-description').dispatchEvent(TJSMouseEvent.new('click'));
  Check((Pos('Browser: Available / LCL: Basic support',
    Field('inspector-placeholder').title) > 0) and
    (Pos('widgetset', Field('inspector-placeholder').title) > 0),
    'real property editor explains the qualified native placeholder capability');
  LBefore := Source;
  Click('action-advanced-properties');
  Check((Pos('Pane sizing belongs to the split-view projection.',
    document.body.textContent) > 0) and (Source = LBefore),
    'touch-visible unsupported-property help preserves the accepted source');
  Click('action-advanced-properties');
  Click(NyxStudioBindingsToggleID);
  Check(TJSHTMLSelectElement(Field(NyxStudioBindingTargetID)).value = 'Value',
    'memo selects its portable Value target');
  Click('binding-state-0');
  LMemo := TJSHTMLTextAreaElement(Field('project-description'));
  Check((LMemo.value = 'Starting reply / 🌙') and
    TJSHTMLInputElement(Field('inspector-value')).disabled,
    'binding pulls its default and disables the invisible fallback editor');
  Check((Pos('LProjectDescriptionMemo.Binds', Source) > 0) and
    (Pos('.Value(LReplyTextState)', Source) > 0),
    'binding uses the same named typed reference in generated Pascal');
  Change('state-default-0', 'Edited reply / 漢字');
  Check(TJSHTMLTextAreaElement(Field('project-description')).value = 'Edited reply / 漢字',
    'saved default edits update the design projection');
  Click('action-undo');
  Check(TJSHTMLTextAreaElement(Field('project-description')).value = 'Starting reply / 🌙',
    'default undo restores the bound projection');
  Click('action-redo');
  Check(TJSHTMLTextAreaElement(Field('project-description')).value = 'Edited reply / 漢字',
    'default redo restores its accepted edit');
  Change('state-name-0', 'reply / 漢字');
  Check((Pos('NyxTextState(''reply / 漢字'')', Source) > 0) and
    (Pos('NyxTextState(''reply 🌙'')', Source) = 0),
    'name editing migrates references and generated source');
  LBefore := Source;
  LMemo := TJSHTMLTextAreaElement(Field('project-description'));
  Click('state-remove-0');
  Check((Source = LBefore) and (GCompact or (Field('project-description') = LMemo)),
    'used-default removal preserves source and the accepted canvas controls');
  Check(Pos('missing', LowerCase(Find('studio-status').textContent)) > 0,
    'rejected removal reports an accessible diagnostic');
  Change(NyxStudioBindingFlowID, NyxStudioBindingDirectionTitle(bdFromState));
  Check(Pos('.Value(LReplyTextState, bdFromState)', Source) > 0,
    'one-way selection generates an enum direction');
  Click('binding-clear');
  Check(Pos('LProjectDescriptionMemo.Binds' + #10 + '      .Clear(bpValue)', Source) > 0,
    'unbinding generates an explicit typed clear');
  Click('state-remove-0');
  Check(document.querySelector('[data-node="state-row-0"]') = nil,
    'unused defaults can be removed');
  Click('action-undo');
  Find(NyxStudioStateToggleID);
  Check(document.querySelector('[data-node="state-row-0"]') <> nil,
    'default removal is undoable');
  Click('action-redo');
  CreateDefault('enabled', 'Boolean', 'true');
  Check(Pos('LEnabledBooleanState: TNyxBooleanStateRef;', Source) > 0,
    'Boolean form creates a Boolean declaration');
  CreateDefault('count', 'Integer', '42');
  LBefore := Source;
  Change('state-default-1', '1.5');
  Check((Source = LBefore) and (TJSHTMLInputElement(Field('state-default-1')).value = '42'),
    'fractional integer input restores the accepted default');
  CreateDefault('payload', 'Text (escaped)', '"🌙\u0000exact"');
  Check((Pos('NyxScalarText(0)', Source) > 0) and
    (Pos(#0, TJSHTMLTextAreaElement(Field('state-default-2')).value) = 0),
    'escaped text form preserves NUL in source while the widget stays representable');
  Find('project-description').dispatchEvent(TJSMouseEvent.new('click'));
  Change(NyxStudioBindingTargetID, 'Enabled');
  Check((document.querySelector('[data-node="binding-state-0"]') <> nil) and
    (document.querySelector('[data-node="binding-state-1"]') = nil) and
    (document.querySelector('[data-node="binding-state-2"]') = nil),
    'Boolean target offers Boolean keys and excludes integer/text keys');
  Click('binding-state-0');
  Check(Pos('.Enabled(LEnabledBooleanState)', Source) > 0,
    'Boolean binding generates its exact typed reference');
  LBefore := Source;
  LMemo := TJSHTMLTextAreaElement(Field('project-description'));
  CreateDefault('enabled', 'Boolean', 'false');
  Check((Source = LBefore) and (GCompact or (Field('project-description') = LMemo)),
    'duplicate creation preserves source and canvas identity');
  LBefore := Source;
  LSourceDraft := StringReplace(LBefore, '.SetValue(LCountIntegerState, 42)',
    '.SetValue(LCountIntegerState, 64)', []);
  LSourceDraft := StringReplace(LSourceDraft, 'Result.State' + #10,
    'Result.State' + #10 + '      .SetValue(NyxTextState(''codeReply''), ''Typed from Pascal / 🌙'')' + #10, []);
  LSourceDraft := StringReplace(LSourceDraft, '.Enabled(LEnabledBooleanState)' + #10 +
    '      .Done;', '.Enabled(LEnabledBooleanState)' + #10 +
    '      .Value(NyxTextState(''codeReply''), bdFromState)' + #10 + '      .Done;', []);
  Change('studio-code', LSourceDraft);
  Click('action-apply-source');
  Check(Source = LSourceDraft, 'source default/binding edit retains authored Pascal');
  Check(TJSHTMLTextAreaElement(Field('project-description')).value = 'Typed from Pascal / 🌙',
    'source-created default and Value binding reach the actual canvas memo');
  Check(TJSHTMLInputElement(Field('state-default-2')).value = '64',
    'source numeric default reaches the Nyx state editor');
  Click('action-undo');
  Check((Source = LBefore) and (TJSHTMLInputElement(Field('state-default-1')).value = '42'),
    'actual undo restores source-created defaults and bindings');
  Click('action-redo');
  Check((Source = LSourceDraft) and
    (TJSHTMLTextAreaElement(Field('project-description')).value = 'Typed from Pascal / 🌙'),
    'actual redo restores source-created control projection');
  LMemo := TJSHTMLTextAreaElement(Field('project-description'));
  LBefore := Source;
  Change('studio-code', StringReplace(LBefore, '.Enabled(LEnabledBooleanState)',
    '.Enabled(NyxTextState(''codeReply''))', []));
  Click('action-apply-source');
  Check((Pos('Boolean binding target', Find('studio-status').textContent) > 0) and
    (GCompact or (Field('project-description') = LMemo)),
    'wrong reference family retains accepted memo and reports typed failure');
  Click('action-reset-source');
  Check(Source = LBefore, 'Restore accepted discards invalid binding draft');
  LBefore := Source;
  LSourceDraft := StringReplace(LBefore, '  except',
    '    LProjectDescriptionMemo.Contract.Value(NyxTextDomain.Choices(['''', ' +
    '''Typed from Pascal / 🌙'']));' + #10 +
    '    LProjectDescriptionMemo.Extensions.SetValue(NyxExtension(''app.validation''),' + #10 +
    '      NyxObject([NyxField(''limit'', NyxData(NyxDecimal(''9007199254740993'')))]));' + #10 +
    '  except', []);
  Change('studio-code', LSourceDraft);
  Click('action-apply-source');
  Check(Source = LSourceDraft, 'actual Apply retains typed contract/extension source');
  Check(TJSHTMLTextAreaElement(Field('project-description')).value = 'Typed from Pascal / 🌙',
    'declared Text choices admit the actual bound canvas memo');
  LMemo := TJSHTMLTextAreaElement(Field('project-description'));
  Change('studio-code', StringReplace(LSourceDraft, '''Typed from Pascal / 🌙'']))',
    '''Outside choices'']))', []));
  Click('action-apply-source');
  Check((Pos('choices', Find('studio-status').textContent) > 0) and
    (GCompact or (Field('project-description') = LMemo)) and
    (Pos('Outside choices', Source) > 0),
    'invalid domain retains draft and accepted actual control');
  Click('action-reset-source');
  Check(Source = LSourceDraft, 'Restore accepted restores typed declaration source');
  Click('action-undo');
  Check(Source = LBefore, 'Undo restores paired contract/extension source');
  Click('action-redo');
  Check((Source = LSourceDraft) and
    (TJSHTMLTextAreaElement(Field('project-description')).value = 'Typed from Pascal / 🌙'),
    'Redo restores declarations/data and actual control projection');
  LField := Field(NyxStudioNewStateNameID);
  TJSHTMLInputElement(LField).value := 'Pending authoring draft / 🌙';
  LField.focus;
  Click(NyxStudioStateToggleID);
  Click(NyxStudioStateToggleID);
  Check(TJSHTMLInputElement(Field(NyxStudioNewStateNameID)).value = 'Pending authoring draft / 🌙',
    'panel hide/show preserves a focused uncommitted new-default draft');
  Click(NyxInspectorEventsID);
  Check(Find('event-after-enter-policy') <> nil,
    'actual Events tab exposes the selected memo lifecycle callback');
  LBefore := Source;
  Click('event-after-enter-add');
  LMemo := TJSHTMLTextAreaElement(Field('studio-code'));
  Check((document.activeElement = LMemo) and
    (Pos('// TODO: implement TProjectDescriptionMemoAfterEnter.', LMemo.value) > 0) and
    (Pos('// TODO:', Copy(LMemo.value, LMemo.selectionStart + 1, 64)) = 3),
    'Add callback creates its managed Pascal TODO and focuses its exact source line');
  Click('event-after-enter-add');
  Check(Find('event-after-enter-count').textContent = '2 registrations',
    'Events tab visibly lists independent registrations');
  Change('event-after-enter-policy', 'ui-queue');
  Check(Pos('.Policy(neUIQueue)', Source) > 0,
    'actual policy editor generates a typed policy enum');
  Click('event-after-enter-callback-0-remove');
  Check((Find('event-removal-warning') <> nil) and
    (Find('event-after-enter-count').textContent = '2 registrations'),
    'remove opens a warning before changing registrations');
  Click('event-removal-cancel');
  Check(Find('event-after-enter-count').textContent = '2 registrations',
    'keeping registration cancels removal');
  Click('event-after-enter-callback-0-remove');
  Click('event-removal-confirm');
  Check((Find('event-after-enter-count').textContent = '1 registrations') and
    (Pos('procedure TProjectDescriptionMemoAfterEnter.Invoke', Source) > 0),
    'confirmed removal retains the handwritten implementation');
  Click('action-undo');
  Check(Find('event-after-enter-count').textContent = '2 registrations',
    'actual undo restores ordered callback registrations');
  Check((Find('event-key-down-policy') <> nil) and (Find('event-key-up-policy') <> nil),
    'actual memo Events tab exposes both typed keyboard families');
  Click('event-key-down-add');
  Check((Find('event-key-down-count').textContent = '1 registrations') and
    (Pos('// TODO: implement TProjectDescriptionMemoKeyDown.', Source) > 0),
    'keyboard Add creates its purposeful handler and visible registration');
  Change('event-key-down-policy', 'asynchronous');
  Check((Pos('.OnKeyDown', Source) > 0) and (Pos('.Policy(neAsynchronous)', Source) > 0),
    'keyboard policy changes reach typed companion source');
  Click('event-key-up-add');
  Check((Find('event-key-up-count').textContent = '1 registrations') and
    (Pos('.OnKeyUp', Source) > 0), 'key release has its independent authored registration');
  Check((Find('event-before-key-press-policy') <> nil) and
    (Find('event-after-key-press-policy') <> nil) and
    (Find('event-before-text-input-policy') <> nil) and
    (Find('event-pointer-down-policy') <> nil) and
    (Find('event-context-menu-policy') <> nil),
    'actual Inspector exposes key phases, text proposals and pointer/menu families');
  Click('event-before-key-press-add');
  Check((Pos('.OnBeforeKeyPress', Source) > 0) and
    (Pos('// TODO: implement TProjectDescriptionMemoBeforeKeyPress.', Source) > 0),
    'new keyboard phase adds a crafted callback and editable source template');
  Click('action-undo');
  Check(Find('event-before-key-press-count').textContent = '0 registrations',
    'new event registration is an ordinary paired undoable command');
  Click(NyxInspectorPropertiesID);
  Check(Field('inspector-text') <> nil, 'Properties tab returns the selected control text editor');
  LBefore := EditNyxManagedFixture(Source, 'LProjectDescriptionMemo', 'LReplyDraftMemo');
  LBefore := EditNyxManagedFixture(LBefore, 'LReplyDraftMemo := NewNyxMemo',
    '{ Notes for the reply / 🌙 }' + #10 + '    LReplyDraftMemo := NewNyxMemo');
  Change('studio-code', LBefore);
  Click('action-apply-source');
  Check((Source = LBefore) and (TJSHTMLInputElement(Field('inspector-text')).value = 'Description'),
    'actual Apply admits a deliberate memo name and comment without changing its control');
  Change('inspector-text', 'Reply notes');
  LSourceDraft := Source;
  Check((Pos('LReplyDraftMemo: INyxMemo;', LSourceDraft) > 0) and
    (Pos('Notes for the reply / 🌙', LSourceDraft) > 0) and
    (Pos('Reply notes', Find('project-description').textContent) > 0),
    'actual property editing retains crafted names/comments and updates the canvas');
  Click('action-undo');
  Check((Source = LBefore) and (TJSHTMLInputElement(Field('inspector-text')).value = 'Description'),
    'actual undo restores the exact crafted companion and the visible property');
  Click('action-redo');
  Check((Source = LSourceDraft) and (TJSHTMLInputElement(Field('inspector-text')).value = 'Reply notes'),
    'actual redo restores the paired visual change');
  Change('studio-code', EditNyxManagedFixture(Source, 'LReplyDraftMemo: INyxMemo;',
    'Result: INyxMemo;'));
  Click('action-apply-source');
  Check((TJSHTMLInputElement(Field('inspector-text')).value = 'Reply notes') and
    (Pos('Result: INyxMemo;', Source) > 0),
    'a reserved local retains accepted actual controls and the rejected editable draft');
  Click('action-reset-source');
  LBefore := Source;
  LSourceDraft := LBefore + #10 + '{ 🌙漢字 } ''unfinished';
  Change('studio-code', LSourceDraft);
  Click('action-apply-source');
  Check((Source = LSourceDraft) and
    (Pos('Pascal ', Find('studio-source-diagnostic-message').textContent) = 1),
    'failed Apply retains the exact editable draft and visible Nyx diagnostic');
  Click(NyxStudioDiagnosticGoID);
  LMemo := TJSHTMLTextAreaElement(Field('studio-code'));
  Check((document.activeElement = LMemo) and
    (Copy(LMemo.value, LMemo.selectionStart + 1, 11) = '''unfinished'),
    'explicit diagnostic action focuses its Unicode column without replacing the draft');
  Change('studio-code', LBefore);
  Check(TJSHTMLElement(document.querySelector('[data-node="studio-source-diagnostic"]')).style.getPropertyValue('display') = 'none',
    'typing hides the stale error while preserving mounted editor identity');
  Click('action-apply-source');
  Check((Source = LBefore) and
    (document.querySelector('[data-node="studio-source-diagnostic"]') = nil),
    'successful Apply removes the diagnostic and retains accepted Pascal');
  LSourceDraft := AddNyxStructuralFixture(LBefore);
  Change('studio-code', LSourceDraft);
  Click('action-apply-source');
  Check(Source = LSourceDraft, 'actual Apply accepts the exact structural companion');

  if GCompact then
  begin
    Click('action-panel-project');
  end;
  Click('view-page-1');
  Check((Find('code-reply') <> nil) and (Find('code-send') <> nil),
    'source-created page shows its real memo and action on the design surface');
  Check(TJSHTMLTextAreaElement(Field('code-reply')).value = 'From crafted source / 🌙',
    'source-created memo consumes its new typed bound default');
  Click('code-reply');
  Change('inspector-text', 'Notes from the canvas');
  Check((Pos('A page written by hand / 🌙 漢字.', Source) > 0) and
    (Pos('LReplyEditor: INyxMemo;', Source) > 0),
    'actual visual property events preserve the source-created component contract');
  Click('action-undo');
  Click('action-undo');
  Check(Source = LBefore, 'actual paired undo removes the source-created page and companion');
  Click('action-redo');
  Check(Source = LSourceDraft, 'actual paired redo restores the new page and exact source');
  document.body.setAttribute('data-nyx-authoring-width', IntToStr(window.innerWidth));
  document.body.setAttribute('data-nyx-authoring', 'passed');
  document.body.setAttribute('data-nyx-authoring-checks', IntToStr(GChecks));
end;

procedure CheckFrame;
var
  LBody: TJSHTMLElement;
  LResult: TNyxText;
begin
  Inc(GPolls);
  LBody := TJSHTMLElement(GFrame.contentDocument.body);
  LResult := LBody.getAttribute('data-nyx-authoring');

  if LResult = 'passed' then
  begin
    document.body.setAttribute('data-nyx-authoring-host', 'passed');
    document.body.setAttribute('data-nyx-authoring-frame-width',
      LBody.getAttribute('data-nyx-authoring-width'));
    document.body.setAttribute('data-nyx-authoring-frame-checks',
      LBody.getAttribute('data-nyx-authoring-checks'));
  end
  else if (LResult = 'failed') or (GPolls >= 200) then
  begin
    document.body.setAttribute('data-nyx-authoring-host', 'failed');
    document.body.setAttribute('data-nyx-authoring-error',
      LBody.getAttribute('data-nyx-authoring-error'));
  end
  else
  begin
    window.setTimeout(@CheckFrame, 25);
  end;
end;

procedure Host;
begin
  TJSHTMLElement(document.body).style.setProperty('margin', '0');
  GFrame := TJSHTMLIFrameElement(document.createElement('iframe'));
  GFrame.style.setProperty('width', '390px');
  GFrame.style.setProperty('height', '844px');
  GFrame.style.setProperty('border', '0');
  GFrame.src := 'authoring.html?frame=1';
  document.body.appendChild(GFrame);
  window.setTimeout(@CheckFrame, 25);
end;

begin
  try

    if window.location.search = '?host=1' then
    begin
      Host;
    end
    else
    begin
      Run;
    end;
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-nyx-authoring', 'failed');
      document.body.setAttribute('data-nyx-authoring-error', LException.Message);
    end;
  end;
end.
