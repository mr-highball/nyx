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

program nyx_browser_journey;

{$mode delphi}{$H+}
{$codepage utf8}

uses
  nyx.types,
  nyx.behavior,
  nyx.text,
  SysUtils,
  Web,
  nyx.model,
  nyx.catalog,
  nyx.schema,
  nyx.theme,
  nyx.data,
  nyx.design.tokens,
  nyx.render.browser,
  nyx.test.identity,
  nyx.test.controls.targets,
  nyx.test.scheduler.targets,
  nyx.studio.browser;

type
  { Programmatic interaction through actual DOM controls. This supplements the
    shared fixtures; it reports DOM journeys, not physical mouse/device testing. }
  TBrowserJourney = class
  private
    FRenderer: TNyxBrowserRenderer;
    FDocument: TNyxDocument;
    FCatalog: TNyxCatalog;
    FStudio: TNyxStudio;
    FLastEvent: TNyxText;
    FLastDesignID: TNyxText;
    FLastSourceID: TNyxText;
    FCount: Integer;
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure Check(ACondition: Boolean; const AMessage: TNyxText);
    function Find(const ASelector: TNyxText): TJSHTMLElement;
    procedure IdentityJourney;
  public
    procedure Run;
  end;

function FailingFactory(ANode: TNyxNode): TJSHTMLElement;
begin
  Result := nil;
  raise ENyxModel.Create('Intentional browser factory failure');
end;

function BaseButtonFactory(ANode: TNyxNode): TJSHTMLElement;
begin
  Result := TJSHTMLElement(document.createElement('button'));
  Result.textContent := 'Base factory';
end;

function ExactButtonFactory(ANode: TNyxNode): TJSHTMLElement;
begin
  Result := TJSHTMLElement(document.createElement('button'));
  Result.textContent := 'Exact factory';
end;

procedure TBrowserJourney.Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin
  FLastEvent := AEvent.Name.Name;
  FLastDesignID := ANode.DesignID;
  FLastSourceID := ANode.SourceID;
end;

procedure TBrowserJourney.IdentityJourney;
var
  LLiteral: TJSHTMLElement;
  LRuntimeControl: TJSHTMLElement;
  LNested: TNyxNode;
  LAccepted: TNyxNode;
  LRejected: Boolean;
  LDetached: TNyxNode;
begin
  AddNyxIdentityFixture(FDocument);
  FRenderer.Render(FDocument, FDocument.Find('identity/page~🌙'),
    TJSHTMLElement(document.body));
  LLiteral := FRenderer.ElementFor('literal/instance', niDesign);
  LRuntimeControl := FRenderer.ElementFor('literal/instance', niRuntime);
  Check((LLiteral <> LRuntimeControl) and (LLiteral.textContent = 'Literal action') and
    (LRuntimeControl.textContent = 'Runtime action') and
    (FRenderer.ElementFor('literal/instance') = LRuntimeControl),
    'browser explicit identity modes and automatic runtime precedence');
  LRuntimeControl.click;
  Check((FLastEvent = 'runtime') and (FLastDesignID = 'literal') and
    (FLastSourceID = 'instance'), 'reusable event keeps editable and template identity');
  LLiteral.click;
  Check((FLastEvent = 'literal') and (FLastDesignID = 'literal/instance'),
    'authored separator identity reaches its own browser action');
  LNested := FRenderer.Root.Find(NyxQualifiedID(
    NyxQualifiedID(NyxQualifiedID('', NyxIdentityInstanceID), 'inner/ref~🌙'),
    NyxIdentityDefinitionID));
  FRenderer.ElementFor(LNested.Part('action').ID, niRuntime).click;
  Check((FLastEvent = 'save') and (FLastDesignID = NyxIdentityInstanceID) and
    (FLastSourceID = TNyxText('action/🌙~1')),
    'long Unicode nested runtime key dispatches the selecting instance');
  LAccepted := FRenderer.Root;
  LDetached := TNyxNode.Create('label', 'detached');
  try
    LRejected := False;
    try
      FRenderer.Render(FDocument, LDetached, TJSHTMLElement(document.body));
    except
      on LException: ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (FRenderer.Root = LAccepted) and
      (FRenderer.ElementFor('literal/instance', niDesign) = LLiteral),
      'unchecked detached view retains accepted browser identity/control');
  finally
    LDetached.Free;
  end;
  FRenderer.Render(FDocument, FDocument.Find('identity/page~🌙'),
    TJSHTMLElement(document.body), True);
  LNested := FRenderer.Root.Find(NyxQualifiedID(
    NyxQualifiedID(NyxQualifiedID('', NyxIdentityInstanceID), 'inner/ref~🌙'),
    NyxIdentityDefinitionID));
  FRenderer.ElementFor(LNested.Part('action').ID, niRuntime).click;
  FRenderer.Select(NyxIdentityInstanceID);
  Check((FLastEvent = 'select') and (FLastDesignID = NyxIdentityInstanceID) and
    FRenderer.ElementFor(NyxIdentityInstanceID, niDesign).classList.contains('nyx-selected') and
    not FRenderer.ElementFor(LNested.Part('action').ID, niRuntime).classList.contains('nyx-selected') and
    not FRenderer.ElementFor('own/action~🌙', niDesign).classList.contains('nyx-selected'),
    'browser design selection distinguishes inherited parts from own payload');
end;

procedure TBrowserJourney.Check(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('FAIL: ' + AMessage);
  end;
  Inc(FCount);
end;

function TBrowserJourney.Find(const ASelector: TNyxText): TJSHTMLElement;
begin
  Result := TJSHTMLElement(document.querySelector(ASelector));

  if Result = nil then
  begin
    raise Exception.Create('Missing DOM control: ' + ASelector);
  end;
end;

procedure TBrowserJourney.Run;
var
  LRoot: TNyxNode;
  LValue: TJSHTMLInputElement;
  LSource: TNyxText;
  LAccepted: TNyxNode;
  LRejected: Boolean;
  LEditor: TJSHTMLTextAreaElement;
  LTemplate: TNyxNode;
  LControl: TJSHTMLElement;
  LTheme: TNyxTheme;
  LThemeRenderer: TNyxBrowserRenderer;
  LThemeHost: TJSHTMLElement;
  LThemedControl: TJSHTMLElement;
  LCodeDraft: TNyxText;
begin
  FCatalog := TNyxCatalog.Create;
  FDocument := TNyxDocument.Create;
  FRenderer := TNyxBrowserRenderer.Create;
  FRenderer.OnEvent := Event;
  try
    LRoot := FCatalog.NewNode('number-stepper', 'stepper');
    FDocument.AddPage(LRoot);
    FRenderer.Render(FDocument, LRoot, TJSHTMLElement(document.body));
    LValue := TJSHTMLInputElement(Find('input'));
    Find('[data-node="' + LRoot.Part('increment').ID + '"]').click;
    Check((LValue.value = '2') and (FLastEvent = 'increment'),
      'browser action sync and semantic event');
    Check(TJSHTMLInputElement(Find('input')) = LValue, 'runtime sync preserves DOM identity');
    { A themed embedded view must retain its own CSS tokens without changing
      the outer renderer. This is the Studio shell/preview isolation boundary. }
    LTheme := TNyxTheme.Create;
    LThemeRenderer := nil;
    LThemeHost := TJSHTMLElement(document.createElement('div'));
    document.body.appendChild(LThemeHost);
    try
      LTheme.Accent := '#123456';
      LTheme.Radius := 23;
      LTheme.ControlRadius := 19;
      LTheme.FontSize := 17;
      LThemeRenderer := TNyxBrowserRenderer.Create(LTheme);
      LTemplate := FCatalog.NewNode('button', 'theme-action');
      FDocument.AddPage(LTemplate);
      LThemeRenderer.Render(FDocument, LTemplate, LThemeHost);
      LThemedControl := LThemeRenderer.ElementFor('theme-action');
      Check((window.getComputedStyle(LThemedControl).getPropertyValue('background-color') = 'rgb(18, 52, 86)') and
        (window.getComputedStyle(LThemedControl).getPropertyValue('border-radius') = '19px') and
        (window.getComputedStyle(LThemedControl).getPropertyValue('font-size') = '17px'),
        'custom theme tokens reach an actual embedded browser button');
      Check((window.getComputedStyle(FRenderer.ElementFor(LRoot.Part('increment').ID))
        .getPropertyValue('background-color') = 'rgb(104, 88, 232)') and
        (window.getComputedStyle(document.body).getPropertyValue('font-size') = '14px'),
        'embedded theme preserves the outer renderer palette and font');
      LAccepted := LThemeRenderer.Root;
      LTheme.Accent := 'invalid-color';
      LRejected := False;
      try
        LThemeRenderer.Render(FDocument, LTemplate, LThemeHost);
      except
        on LException: ENyxModel do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected and (LThemeRenderer.Root = LAccepted) and
        (LThemeRenderer.ElementFor('theme-action') = LThemedControl) and
        (window.getComputedStyle(LThemedControl).getPropertyValue('background-color') = 'rgb(18, 52, 86)'),
        'invalid theme retains the accepted browser view and computed palette');
      LTheme.Accent := '#123456';
      SetNyxDesignTokens(FDocument, NyxObject([NyxField('accent', NyxData('#a020c0')),
        NyxField('fontSize', NyxData(18))]));
      LThemeRenderer.Render(FDocument, LTemplate, LThemeHost);
      LThemedControl := LThemeRenderer.ElementFor('theme-action');
      Check((window.getComputedStyle(LThemedControl).getPropertyValue('background-color') = 'rgb(160, 32, 192)') and
        (window.getComputedStyle(LThemedControl).getPropertyValue('font-size') = '18px') and
        (LTheme.Accent = '#123456') and (LTheme.FontSize = 17),
        'document design tokens style real controls without mutating borrowed theme');
      FDocument.Extensions.Remove(NyxExtension(NyxDesignTokensKey));
      LThemeRenderer.Render(FDocument, LTemplate, LThemeHost);
      LThemedControl := LThemeRenderer.ElementFor('theme-action');
      Check((window.getComputedStyle(LThemedControl).getPropertyValue('background-color') = 'rgb(18, 52, 86)') and
        (window.getComputedStyle(LThemedControl).getPropertyValue('font-size') = '17px'),
        'removing document tokens restores caller palette without stale overrides');
    finally
      LThemeRenderer.Free;
      LThemeHost.remove;
      LTheme.Free;
    end;
    LRoot := FCatalog.NewNode('search-field', 'search');
    FDocument.AddPage(LRoot);
    FRenderer.Render(FDocument, LRoot, TJSHTMLElement(document.body));
    LValue := TJSHTMLInputElement(Find('input'));
    LValue.value := 'A query';
    LValue.dispatchEvent(TJSEvent.new('change'));
    Check(FRenderer.Root.Part('query').Prop('value') = 'A query', 'input bridges runtime state');
    Find('[data-node="' + LRoot.Part('clear').ID + '"]').click;
    Check((LValue.value = '') and (FLastEvent = 'clear'), 'compound clear updates actual input');
    LAccepted := FRenderer.Root;
    FRenderer.RegisterFactory('failing-extension', @FailingFactory);
    LRoot := TNyxNode.Create('column', 'failed-view')
      .Add(TNyxNode.Create('label', 'staged-label').SetProp('text', 'Offscreen candidate'))
      .Add(TNyxNode.Create('failing-extension', 'failed-child'));
    FDocument.AddPage(LRoot);
    LRejected := False;
    try
      FRenderer.Render(FDocument, LRoot, TJSHTMLElement(document.body));
    except
      on LException: ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (FRenderer.Root = LAccepted) and
      (TJSHTMLInputElement(Find('input')) = LValue),
      'failed extension preserves accepted tree and DOM identity');
    LValue.value := '🌙 漢字';
    LValue.dispatchEvent(TJSEvent.new('change'));
    Check(FRenderer.Root.Part('query').Prop('value') = '🌙 漢字',
      'retained controls continue dispatching Unicode state after failed render');
    LRoot := FCatalog.NewNode('code-editor', 'shared-code')
      .SetProp('value', 'begin' + #10 + '  // 🌙 漢字' + #10 + 'end.');
    FDocument.AddPage(LRoot);
    FRenderer.Render(FDocument, LRoot, TJSHTMLElement(document.body));
    LEditor := TJSHTMLTextAreaElement(FRenderer.ElementFor('shared-code'));
    Check((Pos('🌙', LEditor.value) > 0) and not LEditor.readOnly,
      'public code editor projects Unicode source through public lookup');
    LEditor.value := 'A source edit';
    LEditor.dispatchEvent(TJSEvent.new('change'));
    Check((FRenderer.Root.Prop('value') = 'A source edit') and (FLastEvent = 'change'),
      'public code editor emits portable text changes');
    LTemplate := FCatalog.NewNode('button', 'button-template')
      .SetProp('text', 'Derived / 🌙').SetProp('emit', 'accept');
    try
      FCatalog.RegisterRecipe('derived-button', 'Derived', 'Custom', LTemplate);
    finally
      LTemplate.Free;
    end;
    LRoot := FCatalog.NewNode('derived-button', 'derived-button-instance');
    FDocument.AddPage(LRoot);
    FRenderer.Render(FDocument, LRoot, TJSHTMLElement(document.body));
    LControl := FRenderer.ElementFor('derived-button-instance');
    LControl.click;
    Check((LControl is TJSHTMLButtonElement) and LControl.classList.contains('nyx-button') and
      (FLastEvent = 'accept') and (FRenderer.Capability(LRoot) = ncAvailable),
      'derived button retains native DOM type, base styling and event behavior');
    LAccepted := FRenderer.Root;
    LRoot := TNyxNode.Create('unknown-widget', 'missing-adapter');
    FDocument.AddPage(LRoot);
    LRejected := False;
    try
      FRenderer.Render(FDocument, LRoot, TJSHTMLElement(document.body));
    except
      on LException: ENyxModel do
      begin
        LRejected := Pos('No browser projection', LException.Message) > 0;
      end;
    end;
    Check(LRejected and (FRenderer.Root = LAccepted) and
      (FRenderer.ElementFor('derived-button-instance') = LControl) and
      (FRenderer.Capability(LRoot) = ncMissing),
      'unknown projection reports capability and retains accepted controls');
    FRenderer.RegisterFactory('button', @BaseButtonFactory);
    FRenderer.RegisterFactory('derived-button', @ExactButtonFactory);
    LRoot := FDocument.Find('derived-button-instance');
    FRenderer.Render(FDocument, LRoot, TJSHTMLElement(document.body));
    Check((FRenderer.ElementFor(LRoot.ID).textContent = 'Exact factory') and
      (FRenderer.Capability(LRoot) = ncCustom), 'exact factory precedes the base override');
    LTemplate := FCatalog.NewNode('derived-button', 'second-template');
    try
      FCatalog.RegisterRecipe('another-derived-button', 'Another', 'Custom', LTemplate);
    finally
      LTemplate.Free;
    end;
    LRoot := FCatalog.NewNode('another-derived-button', 'base-factory-instance');
    FDocument.AddPage(LRoot);
    FRenderer.Render(FDocument, LRoot, TJSHTMLElement(document.body));
    FRenderer.ElementFor(LRoot.ID).click;
    Check((FRenderer.ElementFor(LRoot.ID).textContent = 'Base factory') and
      (FLastEvent = 'accept'), 'base factory also serves unoverridden derived recipes');
    { The preceding registry intentionally replaces all buttons. Use an ordinary
      public renderer for actual catalog control identity/event proof. }
    FreeAndNil(FRenderer);
    FRenderer := TNyxBrowserRenderer.Create;
    FRenderer.OnEvent := Event;
    IdentityJourney;
  finally
    FRenderer.Free;
    FDocument.Free;
    FCatalog.Free;
  end;
  { Studio is retained for its DOM/callback lifetime. Its shell is itself Nyx,
    so these clicks exercise both authoring commands and renderer event routing. }
  FStudio := TNyxStudio.Create;
  FStudio.Run(False);
  Find('[data-node=action-code]').click;
  Check(Find('[data-node=studio-code]').classList.contains('nyx-code-editor') and
    Find('[data-node=studio-canvas]').classList.contains('nyx-design-surface'),
    'Studio uses public Nyx editor and design-surface components');
  LEditor := TJSHTMLTextAreaElement(Find('[data-node=studio-code]'));
  Check(not LEditor.readOnly, 'optional Pascal editor is editable');
  LSource := LEditor.value;
  LCodeDraft := StringReplace(LSource, '''Untitled project''',
    '''Typed in Pascal / 🌙''', []);
  LEditor.focus;
  LEditor.value := LCodeDraft;
  LEditor.selectionStart := 40;
  LEditor.selectionEnd := 44;
  LEditor.scrollTop := 120;
  LEditor.dispatchEvent(TJSEvent.new('change'));
  Check((TJSHTMLTextAreaElement(Find('[data-node=studio-code]')) = LEditor) and
    (LEditor.selectionStart = 40) and (LEditor.selectionEnd = 44) and
    (document.activeElement = LEditor), 'typing retains editor identity, caret and focus');
  Find('[data-node=action-apply-source]').click;
  Check((Pos('Typed in Pascal / 🌙', Find('[data-node=studio-subtitle]').textContent) > 0) and
    (TJSHTMLTextAreaElement(Find('[data-node=studio-code]')).value = LCodeDraft),
    'actual Apply Pascal updates design and retains authored source');
  Find('[data-node=action-undo]').click;
  Check(TJSHTMLTextAreaElement(Find('[data-node=studio-code]')).value = LSource,
    'actual undo restores paired source');
  Find('[data-node=action-redo]').click;
  Check(TJSHTMLTextAreaElement(Find('[data-node=studio-code]')).value = LCodeDraft,
    'actual redo restores accepted Pascal edit');
  LSource := LCodeDraft;
  LCodeDraft := StringReplace(LSource, '.Gap(20)', '.Gap(''twenty'')', []);
  LEditor := TJSHTMLTextAreaElement(Find('[data-node=studio-code]'));
  LEditor.value := LCodeDraft;
  LEditor.dispatchEvent(TJSEvent.new('change'));
  Find('[data-node=action-apply-source]').click;
  Check((TJSHTMLTextAreaElement(Find('[data-node=studio-code]')).value = LCodeDraft) and
    (Pos('Wrong typed argument', Find('[data-node=studio-status]').textContent) > 0) and
    (Pos('Typed in Pascal / 🌙', Find('[data-node=studio-subtitle]').textContent) > 0),
    'rejected typed edit retains draft and accepted design');
  Find('[data-node=action-code]').click;
  Find('[data-node=action-code]').click;
  Check(TJSHTMLTextAreaElement(Find('[data-node=studio-code]')).value = LCodeDraft,
    'pending code draft survives hiding and showing split view');
  Find('[data-node=action-reset-source]').click;
  Check(TJSHTMLTextAreaElement(Find('[data-node=studio-code]')).value = LSource,
    'Restore accepted explicitly discards the rejected buffer');
  Find('[data-node=action-undo]').click;
  Find('[data-node=action-build-view]').click;
  Check((Find('[data-node=studio-outputs]') <> nil) and
    (Pos('Choose an output', Find('[data-node=studio-status]').textContent) > 0),
    'building without a target opens optional output configuration');
  LSource := TJSHTMLTextAreaElement(Find('[data-node=studio-code]')).value;
  Find('[data-node=output-browser]').click;
  Check((Find('[data-node=output-pas2js] input') <> nil) and
    (TJSHTMLTextAreaElement(Find('[data-node=studio-code]')).value = LSource),
    'late browser selection retains designed Pascal');
  Find('[data-node=output-lcl]').click;
  Check((Find('[data-node=output-fpc] input') <> nil) and
    (TJSHTMLTextAreaElement(Find('[data-node=studio-code]')).value = LSource),
    'late native selection needs no compiler to continue authoring');
  Find('[data-node=output-none]').click;
  Find('[data-node=action-outputs]').click;
  LValue := TJSHTMLInputElement(Find('[data-node=project-title] input'));
  LValue.value := 'Personal workspace / 🌙';
  LValue.dispatchEvent(TJSEvent.new('change'));
  Check((Pos('Personal workspace / 🌙', Find('[data-node=studio-subtitle]').textContent) > 0) and
    (Pos('Personal workspace / 🌙', TJSHTMLTextAreaElement(Find('[data-node=studio-code]')).value) > 0),
    'editable project identity updates chrome and generated Pascal');
  Find('[data-node=action-undo]').click;
  Check(Pos('Untitled project', Find('[data-node=studio-subtitle]').textContent) > 0,
    'project naming is undoable through the ordinary Studio action');
  Find('[data-node=palette-card]').click;
  Check(Find('[data-node=selected-label]').textContent = 'card / card-1',
    'palette creates selected component');
  Find('[data-node=palette-heading]').click;
  Check((Find('[data-node=inspector-enabled] select') <> nil) and
    (Find('[data-node=inspector-width] input').getAttribute('type') = 'number'),
    'inspector uses typed selectors and number fields');
  LValue := TJSHTMLInputElement(Find('[data-node=inspector-text] input'));
  LValue.value := 'A designed card';
  LValue.dispatchEvent(TJSEvent.new('change'));
  LSource := TJSHTMLTextAreaElement(Find('[data-node=studio-code]')).value;
  Check((Pos('A designed card', LSource) > 0) and
    (Pos('A designed card', Find('[data-node=studio-canvas]').textContent) > 0),
    'inspector change updates real heading and adjacent Pascal');
  Find('[data-node=action-undo]').click;
  LSource := TJSHTMLTextAreaElement(Find('[data-node=studio-code]')).value;
  Check(Pos('A designed card', LSource) = 0, 'actual undo button restores source');
  Find('[data-node=action-redo]').click;
  LSource := TJSHTMLTextAreaElement(Find('[data-node=studio-code]')).value;
  Check(Pos('A designed card', LSource) > 0, 'actual redo button restores source');
  LValue := TJSHTMLInputElement(Find('[data-node=inspector-width] input'));
  LValue.value := '120000';
  LValue.dispatchEvent(TJSEvent.new('change'));
  Check((Pos('Invalid width', Find('[data-node=studio-status]').textContent) > 0) and
    (TJSHTMLTextAreaElement(Find('[data-node=studio-code]')).value = LSource),
    'typed invalid edit keeps accepted preview/source and reports the property');
  Find('[data-node=action-add-page]').click;
  Check(Find('[data-node=active-view-label]').textContent = 'page-3', 'multi-page authoring');
  Find('[data-node=instance-component-0]').click;
  Check(Pos('component / ', Find('[data-node=selected-label]').textContent) = 1,
    'reusable component authoring');
  Find('[data-node=customize-part-1]').click;
  LValue := TJSHTMLInputElement(Find('[data-node=inspector-text] input'));
  LValue.value := 'My own welcome / 🌙';
  LValue.dispatchEvent(TJSEvent.new('change'));
  Check(Find('[data-node=studio-canvas] .nyx-heading').textContent = 'My own welcome / 🌙',
    'instance part inspector changes the actual customized heading');
  LSource := TJSHTMLTextAreaElement(Find('[data-node=studio-code]')).value;
  Check((Pos('NewNyxSlotOverride', LSource) > 0) and
    (Pos('INyxSlotOverride', LSource) > 0),
    'instance customization generates the specialized public Pascal contract');
  Find('[data-node=tree-node-2]').click;
  Find('[data-node=customize-part-root]').click;
  Find('[data-node=palette-button]').click;
  Check(Find('[data-node=studio-canvas] button').textContent = 'Button',
    'palette adds an actual button to this reusable instance');
  Find('[data-node=action-undo]').click;
  Check(document.querySelector('[data-node=studio-canvas] button') = nil,
    'undo removes the instance payload without editing its definition');
  Find('[data-node=action-redo]').click;
  Check(Find('[data-node=studio-canvas] button').textContent = 'Button',
    'redo reconstructs the independent instance payload');
  Find('[data-node=tree-node-2]').click;
  Find('[data-node=customize-part-root]').click;
  Find('[data-node=instance-component-0]').click;
  Check(document.querySelectorAll('[data-node=studio-canvas] .nyx-heading').length = 2,
    'a reusable view can itself be added to customized instance content');
  { Leave the exercised output section visible for visual review. This ephemeral
    Studio instance does not read or write the user's browser recovery state. }
  Find('[data-node=action-outputs]').click;
  Find('[data-node=output-browser]').click;
  document.body.setAttribute('data-nyx-journey', 'passed');
  document.body.setAttribute('data-nyx-journey-checks', IntToStr(FCount));
end;

var
  GJourney: TBrowserJourney;
begin
  GJourney := TBrowserJourney.Create;
  try
    document.body.setAttribute('data-managed-journey-checks', IntToStr(RunNyxManagedTargetJourney));
    document.body.setAttribute('data-event-journey-checks', IntToStr(RunNyxEventTargetJourney));
    GJourney.Run;
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-nyx-journey', 'failed');
      document.body.setAttribute('data-nyx-journey-error', LException.Message);
    end;
  end;
end.
