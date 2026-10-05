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

program nyx_lcl_tests;

{$mode delphi}{$H+}
{$codepage utf8}

uses
  nyx.types,
  nyx.behavior,
  nyx.text,
  Interfaces,
  Forms,
  Controls,
  StdCtrls,
  Classes,
  SysUtils,
  nyx.model,
  nyx.catalog,
  nyx.schema,
  nyx.theme,
  nyx.data,
  nyx.design.tokens,
  nyx.studio.agentview,
  nyx.widgets.lcl,
  LMessages,
  LCLType,
  Spin,
  nyx.studio.session,
  nyx.studio.view,
  nyx.render.lcl,
  nyx.test.binding.lcl,
  nyx.test.authoring.lcl,
  nyx.test.identity,
  nyx.test.controls.targets,
  nyx.test.scheduler.targets;

type
  { Exercise the actual LCL event bridge using native controls. Owned windows
    are realized where geometry/focus requires a real viewport. Widget events
    are programmatic; desktop screenshots/hardware input remain separate evidence. }
  TNativeJourney = class
  private
    FForm: TForm;
    FRenderer: TNyxLCLRenderer;
    FDocument: TNyxDocument;
    FCatalog: TNyxCatalog;
    FEventCount: Integer;
    FLastEvent: TNyxText;
    FLastDesignID: TNyxText;
    FLastSourceID: TNyxText;
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    function FindButton(AParent: TWinControl; const ACaption: TNyxText): TNyxLCLButton;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
    procedure IdentityJourney;
  end;

procedure RunThemeJourney;
var
  LForm: TForm;
  LTheme: TNyxTheme;
  LRenderer: TNyxLCLRenderer;
  LDocument: TNyxDocument;
  LPage: TNyxNode;
  LInput: TControl;
  LAccepted: TNyxNode;
  LRejected: Boolean;
  LAgents: TNyxStudioAgentView;
begin
  { Exercise caller-selected typography and failure recovery through the public
    renderer. A failed borrowed palette must retain the accepted editable widget,
    its current user text, and the existing realized view. }
  LForm := TForm.CreateNew(nil);
  LTheme := TNyxTheme.Create;
  LTheme.FontSize := 96;
  LRenderer := TNyxLCLRenderer.Create(LTheme);
  LDocument := TNyxDocument.Create;
  try
    LForm.ClientWidth := 1000;
    LForm.ClientHeight := 800;
    LPage := TNyxNode.Create('page', 'theme-page');
    LDocument.AddPage(LPage);
    LPage.Add(TNyxNode.Create('input', 'theme-input').SetProp('text', 'Name')
      .SetProp('value', 'Draft / 🌙 漢字').SetProp('aria-label', 'Project / 🌙'));
    LPage.Add(TNyxNode.Create('button', 'theme-button').SetProp('text', 'Save'));
    LRenderer.Render(LDocument, LPage, LForm);
    LInput := LRenderer.InputFor('theme-input');

    if not (LInput is TEdit) or (LInput.Height < LTheme.FontSize) or
      (LRenderer.ControlFor('theme-button').Height < LTheme.FontSize) or
      (TNyxText(LInput.AccessibleName) <> TNyxText('Project / 🌙')) or
      (LRenderer.InputFor('theme-button') <> nil) then
    begin
      raise ENyxModel.Create('Native input identity, accessible name or auto typography failed');
    end;
    TEdit(LInput).Text := 'Unsaved edit / 🌙';
    LAccepted := LRenderer.Root;
    LTheme.Accent := 'invalid';
    LRejected := False;
    try
      LRenderer.Render(LDocument, LPage, LForm);
    except
      on LException: ENyxModel do
      begin
        LRejected := True;
      end;
    end;

    if not LRejected or (LRenderer.Root <> LAccepted) or
      (LRenderer.InputFor('theme-input') <> LInput) or
      (TNyxText(TEdit(LInput).Text) <> TNyxText('Unsaved edit / 🌙')) then
    begin
      raise ENyxModel.Create('Invalid native palette destroyed the accepted editing state');
    end;
    WriteLn('PASS native large-font input identity, Unicode naming and invalid-theme recovery');
    LTheme.Accent := '#123456';
    SetNyxDesignTokens(LDocument, NyxObject([NyxField('accent', NyxData('#a020c0')),
      NyxField('fontSize', NyxData(18))]));
    LRenderer.Render(LDocument, LPage, LForm);

    if (TNyxLCLButton(LRenderer.ControlFor('theme-button')).Font.Height <> -18) or
      (LTheme.FontSize <> 96) or (LTheme.Accent <> '#123456') then
    begin
      raise ENyxModel.Create('Native token overlay mutated caller palette or lost typography');
    end;
    LDocument.Extensions.Remove(NyxExtension(NyxDesignTokensKey));
    LRenderer.Render(LDocument, LPage, LForm);

    if TNyxLCLButton(LRenderer.ControlFor('theme-button')).Font.Height <> -96 then
    begin
      raise ENyxModel.Create('Removing native tokens retained stale typography');
    end;
    LTheme.FontSize := 14;
    LAgents := DefaultNyxStudioAgentView;
    LAgents.Connected := True;
    LAgents.Status := 'Scooty working / 🌙漢字';
    LAgents.Activity := NyxArray([NyxObject([
      NyxField('actor', NyxData('Scooty')), NyxField('operation', NyxData('nyx_transaction')),
      NyxField('outcome', NyxData('completed 🌙漢字')), NyxField('revision', NyxData(7))])]);
    LPage := BuildNyxStudioAgents(LAgents);
    LDocument.AddPage(LPage);
    LRenderer.Render(LDocument, LPage, LForm);

    if (LRenderer.ControlFor('action-agent-disabled') = nil) or
      (LRenderer.ControlFor('action-agent-readOnly') = nil) or
      (LRenderer.ControlFor('action-agent-edit') = nil) or
      (TNyxText(TLabel(LRenderer.ControlFor('studio-agents-status')).Caption) <> LAgents.Status) then
    begin
      raise ENyxModel.Create('Native agent composition lost permission controls or exact activity text');
    end;
    WriteLn('PASS native document token overlay/removal and Nyx agent panel controls');
  finally
    LRenderer.Free;
    LDocument.Free;
    LTheme.Free;
    LForm.Free;
  end;
end;

function FailingFactory(ANode: TNyxNode; AOwner: TComponent): TControl;
begin
  Result := nil;
  raise ENyxModel.Create('Intentional native factory failure');
end;

function BaseButtonFactory(ANode: TNyxNode; AOwner: TComponent): TControl;
begin
  Result := TButton.Create(AOwner);
  TButton(Result).Caption := 'Base factory';
end;

function ExactButtonFactory(ANode: TNyxNode; AOwner: TComponent): TControl;
begin
  Result := TButton.Create(AOwner);
  TButton(Result).Caption := 'Exact factory';
end;

constructor TNativeJourney.Create;
begin
  inherited Create;
  FForm := TForm.CreateNew(nil);
  FForm.SetBounds(0, 0, 800, 700);
  FRenderer := TNyxLCLRenderer.Create;
  FRenderer.OnEvent := Event;
  FDocument := TNyxDocument.Create;
  FCatalog := TNyxCatalog.Create;
end;

destructor TNativeJourney.Destroy;
begin
  FRenderer.Free;
  FForm.Free;
  FDocument.Free;
  FCatalog.Free;
  inherited Destroy;
end;

procedure TNativeJourney.Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin
  Inc(FEventCount);
  FLastEvent := AEvent.Name.Name;
  FLastDesignID := ANode.DesignID;
  FLastSourceID := ANode.SourceID;
end;

procedure TNativeJourney.IdentityJourney;
var
  LRenderer: TNyxLCLRenderer;
  LLiteral: TNyxLCLButton;
  LRuntimeButton: TNyxLCLButton;
  LNested: TNyxNode;
  LAccepted: TNyxNode;
  LDetached: TNyxNode;
  LRejected: Boolean;
begin
  { Use the standard projection independently of the earlier custom-factory
    registry. Original text, qualified keys and event ownership must agree with
    the browser journey even when a literal ID resembles another runtime path. }
  AddNyxIdentityFixture(FDocument);
  LRenderer := TNyxLCLRenderer.Create;
  try
    LRenderer.OnEvent := Event;
    LRenderer.Render(FDocument, FDocument.Find('identity/page~🌙'), FForm);
    LLiteral := TNyxLCLButton(LRenderer.ControlFor('literal/instance', niDesign));
    LRuntimeButton := TNyxLCLButton(LRenderer.ControlFor('literal/instance', niRuntime));

    if (LLiteral = LRuntimeButton) or (LLiteral.Caption <> 'Literal action') or
      (LRuntimeButton.Caption <> 'Runtime action') or
      (LRenderer.ControlFor('literal/instance') <> LRuntimeButton) then
    begin
      raise ENyxModel.Create('Native explicit identity modes or runtime precedence failed');
    end;
    LRuntimeButton.Click;

    if (FLastEvent <> 'runtime') or (FLastDesignID <> 'literal') or
      (FLastSourceID <> 'instance') then
    begin
      raise ENyxModel.Create('Native reusable event lost editable/template identities');
    end;
    LLiteral.Click;

    if (FLastEvent <> 'literal') or (FLastDesignID <> 'literal/instance') then
    begin
      raise ENyxModel.Create('Native authored separator action lost identity');
    end;
    LNested := LRenderer.Root.Find(NyxQualifiedID(
      NyxQualifiedID(NyxQualifiedID('', NyxIdentityInstanceID), 'inner/ref~🌙'),
      NyxIdentityDefinitionID));
    TNyxLCLButton(LRenderer.ControlFor(LNested.Part('action').ID, niRuntime)).Click;

    if (FLastEvent <> 'save') or (FLastDesignID <> NyxIdentityInstanceID) or
      (FLastSourceID <> TNyxText('action/🌙~1')) or
      (LRenderer.InputFor(LNested.Part('action').ID, niRuntime) <> nil) then
    begin
      raise ENyxModel.Create('Native long Unicode qualification lost instance event ownership');
    end;
    LAccepted := LRenderer.Root;
    LDetached := TNyxNode.Create('label', 'detached');
    try
      LRejected := False;
      try
        LRenderer.Render(FDocument, LDetached, FForm);
      except
        on LException: ENyxModel do
        begin
          LRejected := True;
        end;
      end;

      if not LRejected or (LRenderer.Root <> LAccepted) or
        (LRenderer.ControlFor('literal/instance', niDesign) <> LLiteral) then
      begin
        raise ENyxModel.Create('Unchecked detached root destroyed accepted native identities');
      end;
    finally
      LDetached.Free;
    end;
    WriteLn('PASS native long Unicode/separator identity, explicit lookup and event/recovery journey');
  finally
    LRenderer.Free;
  end;
end;

function TNativeJourney.FindButton(AParent: TWinControl; const ACaption: TNyxText): TNyxLCLButton;
var
  LIndex: Integer;
  LChild: TControl;
begin
  Result := nil;
  for LIndex := 0 to AParent.ControlCount - 1 do
  begin
    LChild := AParent.Controls[LIndex];

    if (LChild is TNyxLCLButton) and (TNyxLCLButton(LChild).Caption = ACaption) then
    begin
      Exit(TNyxLCLButton(LChild));
    end;

    if LChild is TWinControl then
    begin
      Result := FindButton(TWinControl(LChild), ACaption);

      if Result <> nil then
      begin
        Exit;
      end;
    end;
  end;
end;

procedure TNativeJourney.Run;
var
  LRoot: TNyxNode;
  LButton: TNyxLCLButton;
  LIndex: Integer;
  LCount: Integer;
  LAccepted: TNyxNode;
  LRejected: Boolean;
  LEditor: TMemo;
  LSession: TNyxStudioSession;
  LShell: TNyxDocument;
  LShellState: TNyxStudioViewState;
  LTemplate: TNyxNode;
begin
  LRoot := FCatalog.NewNode('number-stepper', 'stepper');
  LRoot.Part('increment').SetProp('aria-label', 'Increase value / 🌙');
  FDocument.AddPage(LRoot);
  FRenderer.Render(FDocument, LRoot, FForm);
  LButton := FindButton(FForm, '+');

  if LButton = nil then
  begin
    raise Exception.Create('Native compound action button is missing');
  end;
  LButton.Click;

  if (FRenderer.Root.Part('value').Prop('value') <> '2') or
    (FLastEvent <> 'increment') or (FEventCount <> 1) then
  begin
    raise Exception.Create('Native compound action/event parity failed');
  end;
  { Windowed focus and real LCL keyboard messages exercise the inherited button
    behavior. Keep this temporary form outside the desktop and taskbar, then
    hide it again; these are programmatic journeys, not physical input proof. }
  FForm.Position := poDesigned;
  FForm.ShowInTaskBar := stNever;
  FForm.SetBounds(-30000, -30000, 390, 700);
  FForm.Show;
  Application.ProcessMessages;
  LButton.SetFocus;

  if not LButton.Focused or not LButton.TabStop or
    (LButton.AccessibleRole <> larButton) or
    (LButton.AccessibleName <> TNyxText('Increase value / 🌙')) then
  begin
    raise ENyxModel.Create('Themed native button lost focus or accessible intent');
  end;
  { Widgetsets send CN key messages before their native handling. Sending only
    LM messages exercises the after-interface phase, which has no KeyUp callback. }
  LButton.Perform(CN_KEYDOWN, VK_SPACE, 0);
  LButton.Perform(CN_KEYUP, VK_SPACE, 0);
  LButton.Perform(CN_KEYDOWN, VK_RETURN, 0);
  LButton.Perform(CN_KEYUP, VK_RETURN, 0);

  if (FRenderer.Root.Part('value').Prop('value') <> '4') or (FEventCount <> 3) then
  begin
    raise ENyxModel.Create('Native Space/Enter must each activate once');
  end;
  LButton.Enabled := False;
  LButton.Click;
  LButton.Perform(CN_KEYDOWN, VK_SPACE, 0);
  LButton.Perform(CN_KEYUP, VK_SPACE, 0);

  if FEventCount <> 3 then
  begin
    raise ENyxModel.Create('Disabled themed native button activated');
  end;
  LButton.Enabled := True;
  LButton.Parent.Enabled := False;
  LButton.Click;

  if FEventCount <> 3 then
  begin
    raise ENyxModel.Create('Themed button activated through a disabled ancestor');
  end;
  LButton.Parent.Enabled := True;
  FForm.Hide;
  WriteLn('PASS themed native keyboard/focus/disabled/accessible-intent journey');
  FForm.SetBounds(0, 0, 390, 700);
  Application.ProcessMessages;
  LRoot := FCatalog.NewNode('button', 'native-unicode')
    .SetProp('text', '🌙 漢字');
  FDocument.AddPage(LRoot);
  FRenderer.Render(FDocument, LRoot, FForm);

  if FindButton(FForm, '🌙 漢字') = nil then
  begin
    raise Exception.Create('Native Unicode caption changed across the LCL boundary');
  end;
  LAccepted := FRenderer.Root;
  FRenderer.RegisterFactory('failing-extension', FailingFactory);
  LRoot := TNyxNode.Create('column', 'failed-view')
    .Add(TNyxNode.Create('label', 'staged-label').SetProp('text', 'Offscreen candidate'))
    .Add(TNyxNode.Create('failing-extension', 'failed-child'));
  FDocument.AddPage(LRoot);
  LRejected := False;
  try
    FRenderer.Render(FDocument, LRoot, FForm);
  except
    on LException: ENyxModel do
    begin
      LRejected := True;
    end;
  end;

  if not LRejected or (FRenderer.Root <> LAccepted) or
    (FindButton(FForm, '🌙 漢字') = nil) then
  begin
    raise Exception.Create('Failed native extension replaced the accepted view');
  end;
  LRoot := FCatalog.NewNode('code-editor', 'shared-code').SetProp('value', '🌙 漢字');
  FDocument.AddPage(LRoot);
  FRenderer.Render(FDocument, LRoot, FForm);
  LEditor := TMemo(FRenderer.ControlFor('shared-code'));
  LEditor.HandleNeeded;

  if (LEditor.Text <> TNyxText('🌙 漢字')) or LEditor.ReadOnly then
  begin
    raise Exception.Create('Public native code editor changed Unicode source');
  end;
  LEditor.Text := 'A source edit';

  if (FRenderer.Root.Prop('value') <> 'A source edit') or (FLastEvent <> 'change') then
  begin
    raise Exception.Create('Public native code editor did not dispatch its text change');
  end;
  LCount := 0;
  for LIndex := 0 to FCatalog.Count - 1 do
  begin

    if FCatalog[LIndex].Kind <> 'component' then
    begin
      LRoot := FCatalog.NewNode(FCatalog[LIndex].Kind, 'native-' + IntToStr(LIndex));
      FDocument.AddPage(LRoot);
      FRenderer.Render(FDocument, LRoot, FForm);
      Inc(LCount);
    end;
  end;
  WriteLn('PASS native increment/event/Unicode/failure recovery journey and ', LCount, ' catalog projections');
  LTemplate := FCatalog.NewNode('button', 'button-template')
    .SetProp('text', 'Derived / 🌙').SetProp('emit', 'accept');
  try
    FCatalog.RegisterRecipe('derived-button', 'Derived', 'Custom', LTemplate);
  finally
    LTemplate.Free;
  end;
  LRoot := FCatalog.NewNode('derived-button', 'derived-button-instance');
  FDocument.AddPage(LRoot);
  FRenderer.Render(FDocument, LRoot, FForm);
  LButton := TNyxLCLButton(FRenderer.ControlFor(LRoot.ID));
  LButton.Click;

  if (LButton.Caption <> TNyxText('Derived / 🌙')) or (FLastEvent <> 'accept') or
    (FRenderer.Capability(LRoot) <> ncAvailable) then
  begin
    raise Exception.Create('Derived native button lost its primitive behavior');
  end;
  LAccepted := FRenderer.Root;
  LRoot := TNyxNode.Create('unknown-widget', 'missing-adapter');
  FDocument.AddPage(LRoot);
  LRejected := False;
  try
    FRenderer.Render(FDocument, LRoot, FForm);
  except
    on LException: ENyxModel do
    begin
      LRejected := Pos('No native projection', LException.Message) > 0;
    end;
  end;

  if not LRejected or (FRenderer.Root <> LAccepted) or
    (FRenderer.ControlFor('derived-button-instance') <> LButton) or
    (FRenderer.Capability(LRoot) <> ncMissing) then
  begin
    raise Exception.Create('Missing native projection replaced accepted controls');
  end;
  FRenderer.RegisterFactory('button', @BaseButtonFactory);
  FRenderer.RegisterFactory('derived-button', @ExactButtonFactory);
  LRoot := FDocument.Find('derived-button-instance');
  FRenderer.Render(FDocument, LRoot, FForm);

  if (TButton(FRenderer.ControlFor(LRoot.ID)).Caption <> 'Exact factory') or
    (FRenderer.Capability(LRoot) <> ncCustom) then
  begin
    raise Exception.Create('Exact native factory did not take precedence');
  end;
  LTemplate := FCatalog.NewNode('derived-button', 'second-template');
  try
    FCatalog.RegisterRecipe('another-derived-button', 'Another', 'Custom', LTemplate);
  finally
    LTemplate.Free;
  end;
  LRoot := FCatalog.NewNode('another-derived-button', 'base-factory-instance');
  FDocument.AddPage(LRoot);
  FRenderer.Render(FDocument, LRoot, FForm);
  TButton(FRenderer.ControlFor(LRoot.ID)).Click;

  if (TButton(FRenderer.ControlFor(LRoot.ID)).Caption <> 'Base factory') or
    (FLastEvent <> 'accept') then
  begin
    raise Exception.Create('Native base factory did not serve the derived recipe');
  end;
  WriteLn('PASS native primitive derivation, capability/admission and factory precedence');
  { Custom registrations are fixture-local. The shared Studio proof below uses
    the default public adapter rather than the caption-overriding test factories. }
  FRenderer.Free;
  FRenderer := TNyxLCLRenderer.Create;
  FRenderer.OnEvent := Event;
  { The same instance-part contract must produce real native controls and route
    an appended action, not merely survive serialization in a shared fixture. }
  LTemplate := FCatalog.NewNode('list-card', 'native-activity-definition');
  FDocument.AddComponent(LTemplate);
  LRoot := TNyxNode.Create('component', 'native-activity-instance')
    .SetProp('component', LTemplate.ID);
  FDocument.AddPage(LRoot);
  LRoot.OverridePart('title').Named('native-custom-title')
    .SetProp('text', 'My native activity / 🌙');
  LRoot.OverridePart('actions', 'append').Named('native-custom-actions')
    .Add(FCatalog.NewNode('button', 'native-floating-add')
      .SetProp('part', 'floating').SetProp('text', '+ Add').SetProp('emit', 'add'));
  FRenderer.Render(FDocument, LRoot, FForm);
  LButton := TNyxLCLButton(FRenderer.ControlFor('native-activity-instance/native-floating-add'));
  LButton.Click;

  if (FLastEvent <> 'add') or
    (TLabel(FRenderer.ControlFor('native-activity-instance/' + LTemplate.Part('title').ID))
      .Caption <> TNyxText('My native activity / 🌙')) or
    (LTemplate.Part('title').Prop('text') = TNyxText('My native activity / 🌙')) then
  begin
    raise ENyxModel.Create('Native reusable part customization changed definition or behavior');
  end;
  WriteLn('PASS native independent part customization and appended action');
  { Render Studio's shared Nyx document through the native adapter too. This
    establishes the shell contract, while a complete native Studio controller
    and its authoring/file/compiler workflows remain separate acceptance work. }
  LSession := TNyxStudioSession.Create;
  LShell := nil;
  try
    LShellState := DefaultNyxStudioViewState;
    LShellState.CodeVisible := True;
    LShellState.Phone := False;
    LShellState.Palette.Search := '';
    LShellState.Log := '';
    LShellState.Status := 'Native Studio contract';
    LShellState.OutputVisible := True;
    LShellState.OutputTarget := 'lcl';
    LShell := BuildNyxStudioView(LSession, LShellState);
    FRenderer.Render(LShell, LShell.Pages[0], FForm);

    if not (FRenderer.ControlFor('studio-code') is TMemo) or
      not (FRenderer.ControlFor('studio-canvas') is TWinControl) or
      (Pos('BuildNyxDocument', TMemo(FRenderer.ControlFor('studio-code')).Text) = 0) then
    begin
      raise Exception.Create('Studio shell did not use public native Nyx authoring controls');
    end;
    WriteLn('PASS shared Nyx Studio shell renders with native public authoring controls');

    if not (FRenderer.InputFor('output-fpc') is TEdit) then
    begin
      raise Exception.Create('Native Studio output section did not use public Nyx inputs');
    end;
    WriteLn('PASS native Studio optional output configuration uses public Nyx controls');

    if not (FRenderer.InputFor('inspector-enabled') is TComboBox) or
      not (FRenderer.InputFor('inspector-width') is TSpinEdit) then
    begin
      raise Exception.Create('Native Studio inspector did not consume typed Nyx properties');
    end;
    WriteLn('PASS native Studio typed property inspector');
    FreeAndNil(LShell);
    LShellState.Compact := True;
    LShellState.Panel := nspInspector;
    LShell := BuildNyxStudioView(LSession, LShellState);
    FRenderer.Render(LShell, LShell.Pages[0], FForm);

    if (FRenderer.Root.Find('studio-center') <> nil) or
      (FRenderer.Root.Find('studio-left') <> nil) or
      not (FRenderer.InputFor('inspector-width') is TSpinEdit) or
      not (FRenderer.ControlFor('action-panel-design') is TNyxLCLButton) or
      (FRenderer.ControlFor('studio-right').Width <= 0) then
    begin
      raise ENyxModel.Create('Compact Studio did not retain a full native public inspector');
    end;
    WriteLn('PASS native compact Studio public navigation/inspector projection');
  finally
    LShell.Free;
    LSession.Free;
  end;
end;

var
  LJourney: TNativeJourney;
begin
  try
    Application.Initialize;
    WriteLn('PASS ', RunNyxManagedTargetJourney, ' native managed component/control checks');
    WriteLn('PASS ', RunNyxEventTargetJourney, ' native event registration/control checks');
    RunThemeJourney;
    WriteLn('PASS ', RunNyxNativeBindingJourney, ' native state/control binding checks');
    WriteLn('PASS ', RunNyxNativeAuthoringJourney, ' native Studio state/binding authoring checks');
    LJourney := TNativeJourney.Create;
    try
      LJourney.Run;
      LJourney.IdentityJourney;
    finally
      LJourney.Free;
    end;
  except
    on LException: Exception do
    begin
      { Native fixture failures must fail the build, not wait on a GUI dialog. }
      WriteLn('FAIL native: ', LException.Message);
      DumpExceptionBackTrace(StdErr);
      { Let the exception and unit owners unwind before heap tracing; Halt
        would retain the diagnostic object itself and hide the real leak count. }
      ExitCode := 1;
    end;
  end;
end.
