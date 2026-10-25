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
program nyx_studio_section_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifndef PAS2JS}Interfaces, Classes, Forms, Controls, StdCtrls, ExtCtrls,
    nyx.test.capture.lcl,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.contract,
  nyx.theme, nyx.behavior, nyx.events, nyx.scheduler, nyx.content.mount,
  nyx.studio.session,
  nyx.studio.view, nyx.studio.sections, nyx.studio.section.views,
  nyx.studio.inspector.groups, nyx.studio.resize,
  nyx.content, nyx.presentations, nyx.resources, nyx.resources.browser,
  nyx.resources.editor,
  nyx.collections, nyx.collections.view, nyx.collections.mount,
  nyx.collections.view.types, nyx.collections.arrangement,
  nyx.collections.query.policy, nyx.studio.collections,
  nyx.menu.editor,
  nyx.studio.resource.browser
  {$ifdef PAS2JS}, JS, Web, nyx.studio.browser{$endif};

type
  {$ifndef PAS2JS}
  { An ordinary embedding host can refuse insertion before frame ownership is
    assigned. The local proposal must be freed, with the old frame untouched. }
  TRefusalPanel = class(TPanel)
  public
    procedure InsertControl(AControl: TControl; AIndex: Integer); override;
  end;
  {$endif}
  { This receiver borrows no view. Its event token is canceled before the
    receiver is released; the facade/configurer is destroyed before its theme. }
  TScenario = class(TNyxEventCallback)
  public
    Clicks: Integer;
    Configurations: Integer;
    Views: TNyxStudioSectionViews; { borrowed; revoked before facade retirement }
    Continuity: TNyxContentFaceStates;
    BusyCaptureRefused: Boolean;
    BusyRestoreRefused: Boolean;
    procedure Configure(ARenderer: TNyxStudioSectionRenderer);
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;

var
  GChecks: Integer;
  GFailBuild: Boolean;
  GFailPreview: Boolean;
  GProjectPreviewed: Boolean;
  GFailInsertion: Boolean;
  GInsertionRefusals: Integer;
  GInspectorUpdates: Integer;
  {$ifdef PAS2JS}
  { Borrowed only during the one embedding-host refusal. Restore the original
    method before clearing these handles; no production object retains them. }
  GAppendParent: TJSNode;
  GOriginalAppend: TJSFunction;
  {$endif}

{$ifdef PAS2JS}
function AppendFrame(AChild: TJSNode): TJSNode;
begin
  Result := TJSNode(GOriginalAppend.call(GAppendParent, AChild));

  if GFailInsertion and (AChild is TJSHTMLElement) and
    TJSHTMLElement(AChild).hasAttribute('data-nyx-shell-frame') then
  begin
    GFailInsertion := False;
    Inc(GInsertionRefusals);
    { The browser insertion succeeded before the extension refused. Retiring
      the unassigned candidate must remove this actual attached DOM proposal. }
    raise ENyxModel.Create('Intentional frame host insertion refusal');
  end;
end;
{$else}
procedure TRefusalPanel.InsertControl(AControl: TControl; AIndex: Integer);
begin

  if GFailInsertion and (AControl is TPanel) then
  begin
    GFailInsertion := False;
    Inc(GInsertionRefusals);
    raise ENyxModel.Create('Intentional frame host insertion refusal');
  end;
  inherited InsertControl(AControl, AIndex);
end;
{$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxModel.Create('Studio section recovery: ' + AReason);
  end;
  Inc(GChecks);
  WriteLn('PASS ', GChecks, ' / ', AReason);
end;

{ Refusal occurs only once the candidate leaves its detached parking host.
  Native visible ancestors and browser inert ancestors establish that physical
  boundary. The earlier Project preview must already have succeeded. Neither
  helper patches production publication or substitutes a model-only event. }
function LiveFace(AFace: TNyxStudioSectionControl): Boolean;
{$ifndef PAS2JS}
var
  LParent: TControl;
{$endif}
begin
  {$ifdef PAS2JS}
  Result := (AFace <> nil) and document.body.contains(AFace) and
    (AFace.closest('[inert]') = nil);
  {$else}
  Result := False;
  LParent := AFace;
  while LParent <> nil do
  begin

    if not LParent.Visible then
    begin
      Exit;
    end;
    LParent := LParent.Parent;
  end;
  Result := AFace <> nil;
  {$endif}
end;

{$ifdef PAS2JS}
function BadgeFace(ANode: TNyxNode): TJSHTMLElement;
{$else}
function BadgeFace(ANode: TNyxNode; AOwner: TComponent): TControl;
{$endif}
begin

  if GFailBuild and (ANode.ID = 'recovery-inspector-badge') then
  begin
    raise ENyxModel.Create('Intentional Studio Inspector factory refusal');
  end;
  {$ifdef PAS2JS}
  Result := TJSHTMLElement(document.createElement('button'));
  Result.className := 'nyx-button';
  Result.textContent := ANode.Prop('text');
  {$else}
  Result := TButton.Create(AOwner);
  TButton(Result).Caption := ANode.Prop('text');
  {$endif}
end;

procedure UpdateBadge(ANode: TNyxNode; AFace: TNyxStudioSectionControl);
begin

  if ANode.ID = 'recovery-inspector-badge' then
  begin
    Inc(GInspectorUpdates);
  end;

  if GFailPreview and LiveFace(AFace) then
  begin

    if ANode.ID = 'recovery-project-badge' then
    begin
      GProjectPreviewed := True;
    end
    else if ANode.ID = 'recovery-inspector-badge' then
    begin
      GFailPreview := False;
      raise ENyxModel.Create('Intentional Studio Inspector physical preview refusal');
    end;
  end;
  {$ifdef PAS2JS}
  AFace.textContent := ANode.Prop('text');
  {$else}
  TButton(AFace).Caption := ANode.Prop('text');
  {$endif}
end;

procedure TScenario.Configure(ARenderer: TNyxStudioSectionRenderer);
begin
  Inc(Configurations);
  ARenderer.RegisterFactory(NyxKindName(nkBadge),
    {$ifdef PAS2JS}@{$endif}BadgeFace, {$ifdef PAS2JS}@{$endif}UpdateBadge);
end;

procedure TScenario.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
var
  LCopied: TNyxContentFaceStates;
begin

  if (AEvent.Trigger = ntClick) and (AExecution <> nil) then
  begin
    Inc(Clicks);
    BusyCaptureRefused := not Views.SectionView(nssChrome).CaptureInteraction(LCopied);
    BusyRestoreRefused := not Views.SectionView(nssChrome).RestoreInteraction(Continuity);
  end;
end;

function InputText(AFace: TNyxStudioSectionControl): TNyxText;
begin
  {$ifdef PAS2JS}
  Result := TJSHTMLInputElement(AFace).value;
  {$else}
  Result := TCustomEdit(AFace).Text;
  {$endif}
end;

procedure Draft(AFace: TNyxStudioSectionControl; const AText: TNyxText);
begin
  {$ifdef PAS2JS}
  TJSHTMLInputElement(AFace).value := AText;
  AFace.dispatchEvent(TJSEvent.new('input'));
  {$else}
  TCustomEdit(AFace).Text := AText;
  {$endif}
end;

procedure Click(AFace: TNyxStudioSectionControl);
begin
  {$ifdef PAS2JS}
  AFace.click;
  {$else}
  TButton(AFace).Click;
  {$endif}
end;

function Shell(ASession: TNyxStudioSession): TNyxDocument;
var
  LInput: INyxInput;
begin
  Result := BuildNyxStudioView(ASession, DefaultNyxStudioViewState);
  try
    { A creator-supplied numeric field retains its physical draft until editing
      completes on both targets. Its accepted value is independently observable;
      a retained control alone does not establish preservation of that draft. }
    LInput := NewNyxInput('recovery-budget');
    LInput.Text := 'Draft budget';
    LInput.Configure.InputType(niNumber).Value(10.0).Done;
    LInput.Contract.Value(NyxNumberDomain.Range(0, 100));
    Result.Pages[0].Add(LInput);
    Result.Pages[0].Add(NewNyxButton('recovery-action').WithText('Check callbacks'));
    Result.Pages[0].Find('studio-right').Add(
      NewNyxBadge('recovery-inspector-badge').WithText('Inspector extension'));
  except
    Result.Free;
    raise;
  end;
end;

{$ifdef PAS2JS}
procedure Pause(AResolve, AReject: TJSPromiseResolver);
begin
  window.setTimeout(
    procedure
    begin
      AResolve(True);
    end, 15);
end;

function ContinueCapture(AEvent: TJSMouseEvent): Boolean;
begin
  { The harness exposes an ordinary action for the observer to acknowledge an
    actual capture. It cannot acknowledge a different checkpoint or bypass any
    recovery assertion, and is detached when this checkpoint finishes. }

  if document.body.getAttribute('data-capture-checkpoint') = 'studio-recovery-live' then
  begin
    document.body.setAttribute('data-capture-observed', 'studio-recovery-live');
  end;
  Result := True;
end;
{$endif}

procedure Idle(AViews: TNyxStudioSectionViews); {$ifdef PAS2JS}async;{$endif}
var
  LTurns: Integer;
begin
  LTurns := 0;
  repeat
    {$ifdef PAS2JS}
    await(TJSPromise.resolve(TJSPromise.new(@Pause)));
    {$else}
    Application.ProcessMessages;
    Sleep(10);
    {$endif}
    Inc(LTurns);

    if LTurns > 2000 then
    begin
      raise ENyxModel.Create('Studio section input boundary did not settle');
    end;
  until not AViews.Dispatching;
end;

{ These additional cases use the ordinary owner without the original badge
  configurator: a registered custom factory intentionally prevents retained
  refresh and therefore cannot establish the static-membership decision. All
  render/refresh lifetimes go through the facade; borrowed faces and roots are
  used only for lookup, input and identity observations. }
procedure StaticMembershipCases(ASession: TNyxStudioSession; ATheme: TNyxTheme;
  AHost: {$ifdef PAS2JS}TJSHTMLElement{$else}TWinControl{$endif});
  {$ifdef PAS2JS}async;{$endif}
const
  CInput = 'static-input/~';
  CAction = 'static-action/~';
  CAdded = 'static-added/~';
  CDynamicInput = 'dynamic-input/~';
var
  LViews: TNyxStudioSectionViews;
  LBase, LNext, LInvalid, LDynamic: TNyxDocument;
  LScenario: TScenario;
  LLease: INyxEventCallback;
  LToken: INyxEventSubscription;
  LDisclosure: INyxDisclosure;
  LInputSpec: INyxInput;
  LDefinition: INyxColumn;
  LReference: INyxComponent;
  LProject, LBody, LFrom, LTo, LRoot: TNyxNode;
  LRemoved: TNyxNode;
  LInput, LAction: TNyxStudioSectionControl;
  LInputID, LActionID, LDynamicID: TNyxText;
  LRefused: Boolean;
begin
  LViews := TNyxStudioSectionViews.Create(ATheme, nscEditorOwnedHierarchy);
  LScenario := TScenario.Create;
  LLease := LScenario;
  LScenario.Views := LViews;
  LBase := nil;
  LNext := nil;
  LInvalid := nil;
  LDynamic := nil;
  try
    LBase := Shell(ASession);
    LProject := LBase.Pages[0].Find('studio-left');
    while LProject.Count > 0 do
    begin
      LRemoved := LProject.Extract(LProject.Count - 1);
      LRemoved.Free;
    end;
    LDisclosure := NewNyxDisclosure('static-disclosure/~');
    LDisclosure.Configure.Text('Retained details').Value(True).Done;
    LBody := LDisclosure.Node.Part(NyxPart('body'));
    LBody.Add(NewNyxColumn('static-from'));
    LBody.Add(NewNyxColumn('static-to'));
    LFrom := LBody.Find('static-from');
    LInputSpec := NewNyxInput(CInput);
    LInputSpec.Configure.Text('Draft').InputType(niNumber).Value(10.0).Done;
    LInputSpec.Contract.Value(NyxNumberDomain.Range(0, 100));
    LFrom.Add(LInputSpec);
    LInputSpec := nil;
    LFrom.Add(NewNyxButton(CAction).WithText('Observe retained callback'));
    LProject.Add(LDisclosure);
    LDisclosure := nil;
    LViews.Render(LBase, LBase.Pages[0], AHost);
    {$ifdef PAS2JS}await(Idle(LViews));{$else}Idle(LViews);{$endif}
    LRoot := LViews.SectionRoot(nssProject);
    LInputID := NyxQualifiedID('', CInput);
    LActionID := NyxQualifiedID('', CAction);
    LInput := LViews.InputFor(LInputID, niRuntime);
    LAction := LViews.ControlFor(LActionID, niRuntime);
    Check((LInputID <> CInput) and (LActionID <> CAction) and
      (LBase.Find(CInput) <> nil) and (LRoot.Find(CInput) = nil) and
      (LRoot.Find(LInputID).SourceID = CInput),
      'static source slash/tilde identities use canonical escaped runtime keys');
    Check((LRoot.Find(NyxQualifiedID('', 'static-disclosure/~')).Kind =
      NyxKindName(nkDisclosure)) and (LInput <> nil) and (LAction <> nil),
      'materialized Disclosure owns its ordinary physical descendants');
    LToken := LViews.SectionEvents(nssProject).On(
      NyxControlEvents(LActionID, niRuntime), ntClick).Subscribe(LLease);
    Draft(LInput, '12.5');
    Check((InputText(LInput) = '12.5') and
      (LRoot.Find(LInputID).Prop('value') = '10'),
      'static section starts with an unfinished physical numeric draft');

    { A global set, rather than source traversal order or parent-local sets,
      decides only a negative result. Reparent and reorder the same IDs. }
    LNext := LBase.Clone;
    LFrom := LNext.Find('static-from');
    LTo := LNext.Find('static-to');
    LTo.Add(LFrom.Extract(0));
    LTo.Insert(0, LFrom.Extract(0));
    Check(LViews.TryRefresh(LNext, LNext.Pages[0]),
      'same global static membership permits ordinary reparent/reorder refresh');
    Check((LViews.SectionRoot(nssProject) = LRoot) and
      (LViews.InputFor(LInputID, niRuntime) = LInput) and
      (LViews.ControlFor(LActionID, niRuntime) = LAction) and
      (LRoot.Find(LInputID).Parent.ID = 'static-to') and
      (LRoot.Find('static-to').Children[0].ID = LActionID),
      'same-set escaped identities retain exact roots/faces with the new arrangement');
    Check((InputText(LInput) = '12.5') and LToken.Active,
      'same-set arrangement retains the unfinished draft and callback scope');
    Click(LAction);
    Check(LScenario.Clicks = 1,
      'retained physical action executes its original callback once');
    {$ifdef PAS2JS}await(Idle(LViews));{$else}Idle(LViews);{$endif}
    Check(LViews.TryRefresh(LBase, LBase.Pages[0]) and
      (LViews.SectionRoot(nssProject) = LRoot) and
      (LViews.InputFor(LInputID, niRuntime) = LInput) and LToken.Active,
      'inverse same-set arrangement remains a retained refresh');

    { Replace one exact ID without changing any node count. A proven set
      difference still uses complete fresh admission. Failure may not retire
      accepted controls, draft or event authority. }
    LInvalid := LBase.Clone;
    LInvalid.Find(CAction).Named(CAdded);
    LInvalid.Find(CInput).SetProp('height', 'not-a-length');
    LRefused := False;
    try
      LViews.TryRefresh(LInvalid, LInvalid.Pages[0]);
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LViews.SectionRoot(nssProject) = LRoot) and
      (LViews.InputFor(LInputID, niRuntime) = LInput) and
      (InputText(LInput) = '12.5') and LToken.Active and
      (LViews.Root.Find(NyxQualifiedID('', CAdded)) = nil),
      'invalid changed-membership source refuses without touching accepted draft/faces/events');
    LNext.Free;
    LNext := LBase.Clone;
    LNext.Find(CAction).Named(CAdded);
    Check(LViews.TryRefresh(LNext, LNext.Pages[0]),
      'valid changed static membership reaches ordinary fresh section publication');
    Check((LViews.SectionRoot(nssProject) <> LRoot) and
      (LViews.Root.Find(NyxQualifiedID('', CAdded)) <> nil) and not LToken.Active,
      'same-count changed static IDs replace the role and retire its former event scope');
    LToken := nil;

    { Dynamic candidates must finish closure classification before a negative
      result: their raw component IDs do not describe realized membership. }
    LDynamic := LBase.Clone;
    LProject := LDynamic.Find('studio-left');
    while LProject.Count > 0 do
    begin
      LRemoved := LProject.Extract(LProject.Count - 1);
      LRemoved.Free;
    end;
    LDefinition := NewNyxColumn('dynamic-template');
    LInputSpec := NewNyxInput(CDynamicInput);
    LInputSpec.Configure.Text('Dynamic draft').InputType(niNumber).Value(10.0).Done;
    LInputSpec.Contract.Value(NyxNumberDomain.Range(0, 100));
    LDefinition.Add(LInputSpec);
    LInputSpec := nil;
    LDefinition.Add(NewNyxLabel('dynamic-note').WithText('Before'));
    LDynamic.AddComponent(LDefinition);
    LDefinition := nil;
    LReference := NewNyxComponent('dynamic-reference');
    LReference.Configure.Component(NyxComponent('dynamic-template')).Done;
    LProject.Add(LReference);
    LReference := nil;
    LViews.Render(LDynamic, LDynamic.Pages[0], AHost);
    {$ifdef PAS2JS}await(Idle(LViews));{$else}Idle(LViews);{$endif}
    LRoot := LViews.SectionRoot(nssProject);
    LDynamicID := NyxQualifiedID(NyxQualifiedID('', 'dynamic-reference'), CDynamicInput);
    LInput := LViews.InputFor(LDynamicID, niRuntime);
    Draft(LInput, '13.5');
    LNext.Free;
    LNext := LDynamic.Clone;
    LNext.Find('dynamic-note').SetProp('text', 'After');
    Check(LViews.TryRefresh(LNext, LNext.Pages[0]) and
      (LViews.SectionRoot(nssProject) = LRoot) and
      (LViews.InputFor(LDynamicID, niRuntime) = LInput) and
      (InputText(LInput) = '13.5') and
      (LRoot.Find(NyxQualifiedID('dynamic-reference', 'dynamic-note')).Prop('text') = 'After'),
      'component source falls back to ordinary retained realized-identity admission');

    LDynamic.Presentations.Define(NyxPresentation('inactive-alternative'),
      TNyxPresentationCondition.Manual);
    LDefinition := NewNyxColumn('dynamic-alternative');
    LDefinition.Add(NewNyxInput('inactive-input').Configure.Value('Inactive').Done);
    LDynamic.AddComponent(LDefinition);
    LDefinition := nil;
    LDynamic.Find('dynamic-reference').Content.Use(NyxComponent('dynamic-template'))
      .WhenPresentation(NyxPresentation('inactive-alternative'))
      .Use(NyxComponent('dynamic-alternative')).Done;
    LViews.Render(LDynamic, LDynamic.Pages[0], AHost);
    {$ifdef PAS2JS}await(Idle(LViews));{$else}Idle(LViews);{$endif}
    LRoot := LViews.SectionRoot(nssProject);
    LInput := LViews.InputFor(LDynamicID, niRuntime);
    Draft(LInput, '14.5');
    LNext.Free;
    LNext := LDynamic.Clone;
    LNext.Find('dynamic-alternative').Add(NewNyxLabel('inactive-new').WithText('Later'));
    Check(LViews.TryRefresh(LNext, LNext.Pages[0]) and
      (LViews.SectionRoot(nssProject) = LRoot) and
      (LViews.InputFor(LDynamicID, niRuntime) = LInput) and
      (InputText(LInput) = '14.5'),
      'inactive content changes keep ordinary fallback and the active physical draft');
    Check((LViews.Root.Find(NyxQualifiedID('dynamic-reference', 'inactive-input')) = nil) and
      (LViews.Root.Find(NyxQualifiedID('dynamic-reference', 'inactive-new')) = nil),
      'ordinary content fallback does not mount an inactive manual branch');
  finally

    if LToken <> nil then
    begin
      LToken.Cancel;
    end;
    LToken := nil;
    LScenario.Views := nil;
    LViews.Free;
    LLease := nil;
    LReference := nil;
    LDefinition := nil;
    LInputSpec := nil;
    LDisclosure := nil;
    LNext.Free;
    LInvalid.Free;
    LDynamic.Free;
    LBase.Free;
  end;
end;

{ The real metadata browser mounts both public list faces and updates their
  selected-action state. Its synchronization must not run an unrelated creator
  updater or disturb a physical Inspector draft. No renderer/mock Sync bypasses
  the ordinary section facade or the collection selection owner. }
procedure ResourceSynchronizationCases(ASession: TNyxStudioSession;
  ATheme: TNyxTheme; AHost: TNyxStudioSectionHost);
  {$ifdef PAS2JS}async;{$endif}
var
  LViews: TNyxStudioSectionViews;
  LBrowser: TNyxStudioResourceBrowser;
  LScenario: TScenario;
  LLease: INyxEventCallback;
  LDocument: TNyxDocument;
  LInputSpec: INyxInput;
  LState: TNyxResourceBrowserState;
  LIntent: TNyxStudioResourceBrowseIntent;
  LSelection: TNyxResourceEditorSelection;
  LFirst: INyxCollectionMount;
  LSecond: INyxCollectionMount;
  LInput: TNyxStudioSectionControl;
  LAction: TNyxStudioSectionControl;
  LNode: TNyxNode;
  LEvent: TNyxEventInfo;
  LBefore: Integer;
begin
  LScenario := TScenario.Create;
  LLease := LScenario;
  LViews := TNyxStudioSectionViews.Create(ATheme, nscEditorOwnedHierarchy,
    {$ifdef PAS2JS}@{$endif}LScenario.Configure);
  LScenario.Views := LViews;
  LBrowser := TNyxStudioResourceBrowser.Create;
  LDocument := nil;
  try
    ASession.Document.Resources.Define(NyxResourceRef('scope-note'),
      NyxTextResource(TNyxText('A complete resource / 🌙')));
    LState := NyxResourceBrowserState;
    LDocument := Shell(ASession);
    { Ordinary native text input admits its value on Change. A contracted
      numeric input deliberately defers admission until editing completes, so
      this fixture establishes a real unfinished physical draft first. }
    LInputSpec := NewNyxInput('scope-inspector-draft');
    LInputSpec.Configure.Text('Independent draft').InputType(niNumber).Value(10.0).Done;
    LInputSpec.Contract.Value(NyxNumberDomain.Range(0, 100));
    LDocument.Find('studio-right').Add(LInputSpec);
    LInputSpec := nil;
    LDocument.Pages[0].Add(NewNyxColumn('studio-resources').Add(
      NewNyxResourceBrowser(NyxStudioResourceBrowserID, LState)));
    LViews.Render(LDocument, LDocument.Pages[0], AHost);
    {$ifdef PAS2JS}await(Idle(LViews));{$else}Idle(LViews);{$endif}
    LInput := LViews.InputFor('scope-inspector-draft');
    Draft(LInput, '12.75');
    Check((InputText(LInput) = '12.75') and
      (LViews.Root.Find('scope-inspector-draft').Prop('value') = '10'),
      'resource isolation starts with a genuinely unaccepted physical numeric draft');
    LBefore := GInspectorUpdates;
    LBrowser.Mount(LViews, LBrowser.Prepare(ASession), LState, NyxNewResourceSelection);
    Check(GInspectorUpdates = LBefore,
      'resource mounting does not synchronize the unrelated Inspector creator');
    LFirst := LViews.CollectionMount(NyxResourceBrowserListID(NyxStudioResourcePickerID));
    LSecond := LViews.CollectionMount(NyxResourceBrowserListID(NyxStudioResourceBrowserID));
    Check(LFirst.Connected and LSecond.Connected and
      (LFirst.View = LSecond.View) and (LFirst.View.Snapshot.Count = 1),
      'both actual resource faces mount the same complete metadata view');
    LAction := LViews.ControlFor(NyxResourceBrowserActionID(NyxStudioResourceBrowserID, rbaOpen));
    Check({$ifdef PAS2JS}TJSHTMLButtonElement(LAction).disabled{$else}not LAction.Enabled{$endif},
      'the mounted Open action is physically disabled without a selected resource');
    LFirst.View.Select(LFirst.View.Snapshot.ItemAt(0).Ref);
    Check({$ifdef PAS2JS}not TJSHTMLButtonElement(LAction).disabled{$else}LAction.Enabled{$endif},
      'ordinary metadata selection physically enables the resource Open action');
    Check(GInspectorUpdates = LBefore,
      'resource selection synchronizes its own action without visiting Inspector');
    LFirst.View.ClearSelection;
    Check({$ifdef PAS2JS}TJSHTMLButtonElement(LAction).disabled{$else}not LAction.Enabled{$endif},
      'clearing ordinary resource selection physically disables Open again');
    LNode := LViews.Root.Find(NyxResourceBrowserFieldID(NyxStudioResourcePickerID, rbfSearch));
    LNode.Configure.Value('No resource has this name').Done;
    LEvent := Default(TNyxEventInfo);
    LEvent.Trigger := ntChange;
    Check(LBrowser.Handle(LNode, LEvent, LViews, ASession, LState, LIntent, LSelection) and
      (LFirst.View.Snapshot.Count = 0) and (LSecond.View.Snapshot.Count = 0),
      'resource filtering updates both actual mounted lists through the public controller');
    Check(InputText(LViews.InputFor(NyxResourceBrowserFieldID(
      NyxStudioResourceBrowserID, rbfSearch))) = 'No resource has this name',
      'the companion resource search face receives the shared filter proposal');
    LNode := LViews.Root.Find(NyxResourceBrowserActionID(NyxStudioResourceBrowserID, rbaReset));
    LEvent.Trigger := ntClick;
    Check(LBrowser.Handle(LNode, LEvent, LViews, ASession, LState, LIntent, LSelection) and
      (LFirst.View.Snapshot.Count = 1) and (LSecond.View.Snapshot.Count = 1),
      'the actual resource Reset action restores both mounted results');
    Check((GInspectorUpdates = LBefore) and
      (LViews.InputFor('scope-inspector-draft') = LInput) and
      (InputText(LInput) = '12.75') and
      (LViews.Root.Find('scope-inspector-draft').Prop('value') = '10'),
      'resource mount selection filter and reset leave unrelated updater draft and accepted value untouched');
  finally
    LFirst := nil;
    LSecond := nil;
    LBrowser.Free;
    LScenario.Views := nil;
    LViews.Free;
    LLease := nil;
    LInputSpec := nil;
    LDocument.Free;
  end;
end;

{ Stock composition and actual facade, with one extra numeric fixture field in
  the primary property form. These are private seeded documents, not a source/
  history or independent generated-application acceptance claim. }
procedure InspectorFormCases(ATheme: TNyxTheme;
  AHost: {$ifdef PAS2JS}TJSHTMLElement{$else}TWinControl{$endif});
  {$ifdef PAS2JS}async;{$endif}
var
  LSession: TNyxStudioSession;
  LViews: TNyxStudioSectionViews;
  LDocument: TNyxDocument;
  LNext: TNyxDocument;
  LRoot: TNyxNode;
  LBefore: TNyxNode;
  LField: TNyxStudioSectionControl;
  LEventBefore: INyxCancellationScope;
  {$ifndef PAS2JS}
  LHandle: PtrUInt;
  {$endif}

  procedure ExplainReplacement(AFrom, ATo: TNyxNode);
  var
    LIndex: Integer;
    LKey: TNyxText;
  begin

    if (AFrom.ID <> ATo.ID) or (AFrom.Count <> ATo.Count) or
      (AFrom.Kind <> ATo.Kind) then
    begin
      WriteLn('Inspector replacement shape / ', AFrom.ID, ' / ', ATo.ID,
        ' / children ', AFrom.Count, ' -> ', ATo.Count);
      Exit;
    end;
    for LIndex := 0 to AFrom.Props.Count - 1 do
    begin
      LKey := AFrom.Props.Names[LIndex];

      if AFrom.StoredProp(LKey) <> ATo.StoredProp(LKey) then
      begin
        { Only copied fixture identities/property names enter this diagnostic;
          long reusable source baselines and user values are never printed. }
        WriteLn('Inspector replacement property / ', AFrom.ID, ' / ', LKey);
      end;
    end;
    for LIndex := 0 to AFrom.Count - 1 do
    begin
      ExplainReplacement(AFrom.Children[LIndex], ATo.Children[LIndex]);
    end;
  end;

  function ComposeInspectorFixture: TNyxDocument;
  var
    LInput: INyxInput;
  begin
    Result := BuildNyxStudioView(LSession, DefaultNyxStudioViewState);
    try
      LInput := NewNyxInput('inspector-form-draft');
      LInput.Configure.Text('Independent draft').InputType(niNumber).Value(7.0).Done;
      LInput.Contract.Value(NyxNumberDomain.Range(0, 100));
      Result.Find(NyxInspectorGroupID(nigProperties)).Part(NyxPart('body')).Add(LInput);
    except
      Result.Free;
      raise;
    end;
  end;

begin
  LSession := TNyxStudioSession.Create;
  LViews := TNyxStudioSectionViews.Create(ATheme, nscEditorOwnedHierarchy);
  LDocument := nil;
  LNext := nil;
  LBefore := nil;
  try
    LSession.Document.Pages[0].Add(NewNyxLabel('form-first').WithText('First label'));
    LSession.Document.Pages[0].Add(NewNyxLabel('form-second').WithText('Second label'));
    LSession.Select('form-first');
    LDocument := ComposeInspectorFixture;
    LViews.Render(LDocument, LDocument.Pages[0], AHost);
    {$ifdef PAS2JS}await(Idle(LViews));{$else}Idle(LViews);{$endif}
    LRoot := LViews.SectionRoot(nssInspector);
    LField := LViews.InputFor('inspector-form-draft');
    {$ifndef PAS2JS}
    Check(TWinControl(LField).HandleAllocated, 'stock Inspector fixture owns an allocated numeric window');
    LHandle := PtrUInt(TWinControl(LField).Handle);
    {$endif}
    Draft(LField, '12.75');
    Check((InputText(LField) = '12.75') and
      (LRoot.Find('inspector-form-draft').Prop('value') = '7'),
      'stock Inspector holds an independent unfinished numeric draft');
    LSession.Document.Find('form-first').Configure.Text('Changed first label').Done;
    LNext := ComposeInspectorFixture;
    Check(LViews.TryRefresh(LNext, LNext.Pages[0]),
      'stock Inspector admits a changed reusable baseline through grouped form publication');
    Check((LViews.SectionRoot(nssInspector) = LRoot) and
      (LViews.InputFor('inspector-form-draft') = LField) and
      (InputText(LField) = '12.75'),
      'same-owner property update retains Inspector controls and unrelated property draft');
    FreeAndNil(LNext);
    LBefore := LRoot.Clone;
    LEventBefore := NyxEventGeneration(LViews.SectionEvents(nssInspector));
    LSession.Select('form-second');
    LNext := ComposeInspectorFixture;
    Check(LViews.TryRefresh(LNext, LNext.Pages[0]),
      'stock Inspector admits another same-kind selected owner without replacing its section');

    if LViews.SectionRoot(nssInspector) <> LRoot then
    begin
      ExplainReplacement(LBefore, LViews.SectionRoot(nssInspector));
    end;
    Check((LViews.SectionRoot(nssInspector) = LRoot) and
      (LViews.InputFor('inspector-form-draft') = LField) and
      (InputText(LField) = '7') and
      (LRoot.Find(NyxInspectorGroupID(nigProperties)).Prop(NyxStudioInspectorContextKey) = 'form-second'),
      'new selected owner resets its own property form on the same physical control / retained-root=' +
      TNyxText(BoolToStr(LViews.SectionRoot(nssInspector) = LRoot, True)) +
      ' / retained-field=' + TNyxText(BoolToStr(LViews.InputFor('inspector-form-draft') = LField, True)) +
      ' / current-value=' + InputText(LViews.InputFor('inspector-form-draft')) +
      ' / current-owner=' + LViews.SectionRoot(nssInspector).Find(
        NyxInspectorGroupID(nigProperties)).Prop(NyxStudioInspectorContextKey));
    Check(LEventBefore.Cancelled and
      (LRoot.Find(NyxStudioResizeWidthID).Prop(NyxStudioResizeOwnerKey) = 'form-second'),
      'retained geometry commands adopt the new selected owner with retired old event authority');
    {$ifndef PAS2JS}
    Check(PtrUInt(TWinControl(LField).Handle) = LHandle,
      'stock Inspector source and owner changes retain the exact native input window');
    {$endif}
  finally
    LViews.Free;
    LBefore.Free;
    LNext.Free;
    LDocument.Free;
    LSession.Free;
  end;
end;

{ Real stock collection forms advance their complete accepted command baseline.
  Physical numeric probes expose unchanged-default restoration separately from
  the unrelated primary property draft. This checks actual section adoption,
  not a source/history command or a whole-editor latency budget. }
procedure CollectionFormCases(ATheme: TNyxTheme;
  AHost: {$ifdef PAS2JS}TJSHTMLElement{$else}TWinControl{$endif});
  {$ifdef PAS2JS}async;{$endif}
var
  LSession: TNyxStudioSession;
  LViews: TNyxStudioSectionViews;
  LDocument: TNyxDocument;
  LNext: TNyxDocument;
  LRoot: TNyxNode;
  LTable: INyxTable;
  LSpec: TNyxCollectionViewSpec;
  LQueryField: TNyxStudioSectionControl;
  LLayoutField: TNyxStudioSectionControl;
  LPropertyField: TNyxStudioSectionControl;
  LEventBefore: INyxCancellationScope;
  LBaseline: TNyxText;
  LMinimumID: TNyxText;
  LRefused: Boolean;
  {$if defined(NYX_SECTION_REFRESH_BENCHMARK) and not defined(PAS2JS)}
  LRepeat: Integer;
  LStarted: QWord;
  {$endif}
  {$ifndef PAS2JS}
  LQueryHandle: PtrUInt;
  LLayoutHandle: PtrUInt;
  {$endif}

  function ComposeCollectionFixture: TNyxDocument;
  var
    LState: TNyxStudioViewState;
    LInput: INyxInput;
  begin
    LState := DefaultNyxStudioViewState;
    LState.BindingsVisible := True;
    Result := BuildNyxStudioView(LSession, LState);
    try
      LInput := NewNyxInput('collection-query-draft');
      LInput.Configure.Text('Query form draft').InputType(niNumber).Value(7.0).Done;
      LInput.Contract.Value(NyxNumberDomain.Range(0, 100));
      Result.Find(NyxStudioQueryEditorID).Add(LInput);
      LInput := NewNyxInput('collection-property-draft');
      LInput.Configure.Text('Independent property draft').InputType(niNumber).Value(8.0).Done;
      LInput.Contract.Value(NyxNumberDomain.Range(0, 100));
      Result.Find(NyxInspectorGroupID(nigProperties)).Part(NyxPart('body')).Add(LInput);
    except
      Result.Free;
      raise;
    end;
  end;

begin
  LSession := TNyxStudioSession.Create;
  LViews := TNyxStudioSectionViews.Create(ATheme, nscEditorOwnedHierarchy);
  LDocument := nil;
  LNext := nil;
  try
    LSession.Document.Collections.Define(NyxCollection('form-records'),
      NyxCollectionSchema.Text(NyxTextField('name'), ''),
      [NyxCollectionItem(NyxItem(NyxCollection('form-records'), 'ada'))
        .WithValue(NyxTextField('name'), 'Ada')]);
    LSpec := NyxCollectionView(NyxCollection('form-records'))
      .Column(NyxTextField('name'), 'Name').TableQuery(NyxTableQuery)
      .TableLayout(NyxTableLayout);
    LTable := NewNyxTable('collection-form-table');
    LTable.Binds.Collection(LSpec);
    LSession.Document.Pages[0].Add(LTable);
    LTable := nil;
    LSession.Select('collection-form-table');
    LDocument := ComposeCollectionFixture;
    Check(Length(NyxStudioCollectionFormContexts(LDocument.Pages[0])) = 2,
      'stock collection owner supplies disjoint query and layout baseline scopes');
    LViews.Render(LDocument, LDocument.Pages[0], AHost);
    {$ifdef PAS2JS}await(Idle(LViews));{$else}Idle(LViews);{$endif}
    LRoot := LViews.SectionRoot(nssInspector);
    LQueryField := LViews.InputFor('collection-query-draft');
    LMinimumID := NyxStudioLayoutPolicyFieldID(-1, slpfMinimumCenter);
    LLayoutField := LViews.InputFor(LMinimumID);
    LPropertyField := LViews.InputFor('collection-property-draft');
    {$ifndef PAS2JS}
    LQueryHandle := TWinControl(LQueryField).Handle;
    LLayoutHandle := TWinControl(LLayoutField).Handle;
    {$endif}
    Draft(LQueryField, '12.75');
    Draft(LLayoutField, '123');
    Draft(LPropertyField, '19.5');
    Check((InputText(LQueryField) = '12.75') and
      (InputText(LLayoutField) = '123') and (InputText(LPropertyField) = '19.5') and
      (LSession.Selected.CollectionView.TableLayoutPolicy.MinimumScrollableWidth <> 123),
      'query, stock layout and independent property inputs hold unsubmitted form values');
    LBaseline := LRoot.Find(NyxStudioQueryEditorID).Prop(NyxMenuFormBaselineKey);
    LEventBefore := NyxEventGeneration(LViews.SectionEvents(nssInspector));
    LSession.Selected.SetCollectionView(LSpec.TableQuery(NyxTableQuery.Filtering(False)));
    LNext := ComposeCollectionFixture;
    Check(LViews.TryRefresh(LNext, LNext.Pages[0]),
      'stock collection policy admission participates in grouped form publication');
    Check((LViews.SectionRoot(nssInspector) = LRoot) and
      (LViews.InputFor('collection-query-draft') = LQueryField) and
      (LViews.InputFor(LMinimumID) = LLayoutField) and
      (LViews.InputFor('collection-property-draft') = LPropertyField),
      'changed collection baselines retain the actual Inspector and all independent controls');
    Check((InputText(LQueryField) = '7') and
      (InputText(LLayoutField) = TNyxText(IntToStr(LSpec.TableLayoutPolicy.MinimumScrollableWidth))) and
      (InputText(LPropertyField) = '19.5'),
      'new collection baselines restore their accepted defaults while unrelated property draft stays unfinished');
    Check(LEventBefore.Cancelled and
      (LRoot.Find(NyxStudioQueryEditorID).Prop(NyxMenuFormBaselineKey) <> LBaseline) and
      (LRoot.Find(NyxStudioQueryEditorID).Prop(NyxMenuFormBaselineKey) =
        LNext.Find(NyxStudioQueryEditorID).Prop(NyxMenuFormBaselineKey)),
      'retained collection forms adopt exact new command baselines and retire old event authority');
    {$ifndef PAS2JS}
    Check((PtrUInt(TWinControl(LQueryField).Handle) = LQueryHandle) and
      (PtrUInt(TWinControl(LLayoutField).Handle) = LLayoutHandle),
      'collection baseline updates keep exact native query and stock layout windows');
    {$endif}

    { An unchanged context must keep its event generation and independent drafts.
      Selecting a faster preparation path must not turn every scalar refresh
      into a context reset or revoke callbacks which still own valid work. }
    Draft(LQueryField, '13.25');
    Draft(LLayoutField, '124');
    LEventBefore := NyxEventGeneration(LViews.SectionEvents(nssInspector));
    FreeAndNil(LNext);
    LNext := ComposeCollectionFixture;
    Check(LViews.TryRefresh(LNext, LNext.Pages[0]),
      'unchanged collection context still admits an ordinary retained refresh');
    Check(not LEventBefore.Cancelled and (LViews.SectionRoot(nssInspector) = LRoot),
      'unchanged collection context preserves its exact event generation and Inspector');
    Check((InputText(LQueryField) = '13.25') and (InputText(LLayoutField) = '124') and
      (InputText(LPropertyField) = '19.5'),
      'unchanged collection context preserves every unfinished independent draft');

    {$if defined(NYX_SECTION_REFRESH_BENCHMARK) and not defined(PAS2JS)}
    { Compare the same mounted stock Inspector and source composition on both
      sides of a routing change. Keep all original checks and safety flags.
      These repeated context changes measure this path, not whole-editor latency. }
    LStarted := GetTickCount64;
    for LRepeat := 0 to 7 do
    begin
      Draft(LQueryField, '12.75');
      Draft(LLayoutField, '123');
      LEventBefore := NyxEventGeneration(LViews.SectionEvents(nssInspector));
      LSession.Selected.SetCollectionView(LSpec.TableQuery(
        NyxTableQuery.Filtering(not Odd(LRepeat))));
      FreeAndNil(LNext);
      LNext := ComposeCollectionFixture;
      Check(LViews.TryRefresh(LNext, LNext.Pages[0]),
        'repeated stock collection context admits through the ordinary section owner');
      Check((LViews.SectionRoot(nssInspector) = LRoot) and
        (LViews.InputFor('collection-query-draft') = LQueryField) and
        (LViews.InputFor(LMinimumID) = LLayoutField),
        'repeated context changes retain their exact actual Inspector inputs');
      Check(LEventBefore.Cancelled and (InputText(LQueryField) = '7') and
        (InputText(LLayoutField) = TNyxText(IntToStr(LSpec.TableLayoutPolicy.MinimumScrollableWidth))) and
        (InputText(LPropertyField) = '19.5'),
        'each repeated context retires old authority and resets only its own drafts');
      Check((PtrUInt(TWinControl(LQueryField).Handle) = LQueryHandle) and
        (PtrUInt(TWinControl(LLayoutField).Handle) = LLayoutHandle),
        'repeated collection context retains both native windows');
    end;
    WriteLn('PROFILE Inspector context changes=8 elapsed-ms=', GetTickCount64 - LStarted);
    {$endif}

    { Positive metadata hints are never admission. Keep the same complete static
      node set and change the collection baseline, then corrupt an ordinary
      property which the fresh renderer must reject before any form adoption. }
    Draft(LQueryField, '14.75');
    Draft(LLayoutField, '125');
    LEventBefore := NyxEventGeneration(LViews.SectionEvents(nssInspector));
    LSession.Selected.SetCollectionView(LSpec.TableQuery(NyxTableQuery.Filtering(True)));
    FreeAndNil(LNext);
    LNext := ComposeCollectionFixture;
    LNext.Find('collection-query-draft').Props.Add('width=invalid-length');
    LRefused := False;
    try
      LViews.TryRefresh(LNext, LNext.Pages[0]);
    except
      on LException: ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'changed-context hint never bypasses fresh scalar property admission');
    Check(not LEventBefore.Cancelled and (LViews.SectionRoot(nssInspector) = LRoot) and
      (LViews.InputFor('collection-query-draft') = LQueryField) and
      (LViews.InputFor(LMinimumID) = LLayoutField) and
      (InputText(LQueryField) = '14.75') and (InputText(LLayoutField) = '125') and
      (InputText(LPropertyField) = '19.5'),
      'refused hinted form preserves exact controls, every draft and prior callback authority');
  finally
    LViews.Free;
    LNext.Free;
    LDocument.Free;
    LSession.Free;
  end;
end;

procedure Run; {$ifdef PAS2JS}async;{$endif}
var
  LSession: TNyxStudioSession;
  LTheme: TNyxTheme;
  LViews: TNyxStudioSectionViews;
  LBase: TNyxDocument;
  LNext: TNyxDocument;
  LFull: TNyxDocument;
  LScenario: TScenario;
  LLease: INyxEventCallback;
  LToken: INyxEventSubscription;
  LInput: TNyxStudioSectionControl;
  LAction: TNyxStudioSectionControl;
  LTitle: TNyxStudioSectionControl;
  LBeforeInputs: TNyxContentFaceStates;
  LBadInputs: TNyxContentFaceStates;
  LIndex: Integer;
  LInputIndex: Integer;
  LTitleText: TNyxText;
  LRoots: array[TNyxStudioSection] of TNyxNode;
  LRole: TNyxStudioSection;
  LRefused: Boolean;
  LMessage: TNyxText;
  LHostChildren: Integer;
  LRootsRetained: Boolean;
  {$ifdef PAS2JS}
  LStarted: Double;
  LContinue: TJSHTMLElement;
  LHost: TJSHTMLElement;
  LStyle: TJSHTMLElement;
  {$else}
  LWindow: TForm;
  LHost: TPanel;
  LInputHandle: PtrUInt;
  LTitleHandle: PtrUInt;
  LInputParent: TWinControl;
  LTitleParent: TWinControl;
  {$endif}
begin
  GFailBuild := False;
  LSession := TNyxStudioSession.Create;
  LTheme := TNyxTheme.Create;
  LTheme.Accent := '#227766';
  LTheme.FontSize := 17;
  LScenario := TScenario.Create;
  LLease := LScenario;
  LViews := TNyxStudioSectionViews.Create(LTheme, nscEditorOwnedHierarchy,
    {$ifdef PAS2JS}@{$endif}LScenario.Configure);
  LScenario.Views := LViews;
  LBase := nil;
  LNext := nil;
  LFull := nil;
  LToken := nil;
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.getElementById('studio-sections'));
  LStyle := TJSHTMLElement(document.createElement('style'));
  LStyle.textContent := NyxStudioBrowserCSS;
  document.head.appendChild(LStyle);
  {$else}
  Application.Initialize;
  LWindow := TForm.CreateNew(nil);
  LWindow.Caption := 'Nyx Studio section recovery';
  LWindow.SetBounds(20, 20, 1240, 820);
  LHost := TRefusalPanel.Create(LWindow);
  LHost.BevelOuter := bvNone;
  LHost.Parent := LWindow;
  LHost.Align := alClient;
  LWindow.Show;
  {$endif}
  try
    {$ifdef NYX_INSPECTOR_FORMS_ONLY}
    { A named diagnostic build isolates this real stock-Inspector journey. It
      earns only its own assertions; the ordinary complete regression below is
      still required after any repair. Both use the same composition and case. }
    {$ifdef PAS2JS}await(InspectorFormCases(LTheme, LHost));
    {$else}InspectorFormCases(LTheme, LHost);{$endif}
    {$ifdef PAS2JS}await(CollectionFormCases(LTheme, LHost));
    {$else}CollectionFormCases(LTheme, LHost);{$endif}
    WriteLn('PASS ', GChecks, ' focused stock Inspector form checks');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-section-checks', IntToStr(GChecks));
    document.body.setAttribute('data-test-result', 'passed');
    {$endif}
    {$else}
    LBase := Shell(LSession);
    LViews.Render(LBase, LBase.Pages[0], LHost);
    {$ifdef PAS2JS}await(Idle(LViews));{$else}Idle(LViews);{$endif}
    LInput := LViews.InputFor('recovery-budget');
    LAction := LViews.ControlFor('recovery-action');
    LTitle := LViews.InputFor('project-title');
    for LRole := Low(TNyxStudioSection) to High(TNyxStudioSection) do
    begin
      LRoots[LRole] := LViews.SectionRoot(LRole);
    end;
    LToken := LViews.Events.On(NyxControlEvents('recovery-action', niRuntime),
      ntClick).Subscribe(LLease);
    Draft(LInput, '12.5');
    Check((InputText(LInput) = '12.5') and
      (LViews.Root.Find('recovery-budget').Prop('value') = '10'),
      'unfinished numeric edit is separate from accepted model text');
    Draft(LTitle, TNyxText('A retained thought / 🌙'));
    LTitleText := InputText(LTitle);
    {$ifdef PAS2JS}
    LTitle.focus;
    TJSHTMLInputElement(LTitle).setSelectionRange(2, 7);
    {$else}
    TCustomEdit(LTitle).SetFocus;
    TCustomEdit(LTitle).SelStart := 2;
    TCustomEdit(LTitle).SelLength := 5;
    { Object identity alone cannot detect LCL destroying and recreating an
      allocated window during a parent move. Keep the real accepted handles
      and their parents through refusal and grouped rollback as well. }
    Check(TWinControl(LInput).HandleAllocated and TWinControl(LTitle).HandleAllocated,
      'accepted Studio inputs have live native windows before preparation');
    LInputHandle := PtrUInt(TWinControl(LInput).Handle);
    LTitleHandle := PtrUInt(TWinControl(LTitle).Handle);
    LInputParent := LInput.Parent;
    LTitleParent := LTitle.Parent;
    {$endif}
    Check(LViews.SectionView(nssChrome).CaptureInteraction(LBeforeInputs),
      'idle renderer exposes copied continuity without borrowing its controls');
    LScenario.Continuity := LBeforeInputs;
    LInputIndex := -1;
    SetLength(LBadInputs, Length(LBeforeInputs) + 1);
    for LIndex := 0 to High(LBeforeInputs) do
    begin
      LBadInputs[LIndex] := LBeforeInputs[LIndex];

      if LBeforeInputs[LIndex].Identity.RuntimeID = 'recovery-budget' then
      begin
        LInputIndex := LIndex;
      end;
    end;
    Check(LInputIndex >= 0, 'continuity contains copied identity for the unfinished field');
    LBadInputs[High(LBadInputs)] := LBeforeInputs[LInputIndex];
    LRefused := False;
    try
      LViews.SectionView(nssChrome).RestoreInteraction(LBadInputs);
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (InputText(LInput) = '12.5'),
      'ambiguous continuity refuses before changing physical drafts');
    LBadInputs := nil;
    LNext := LBase.Clone;
    LNext.Pages[0].Find('recovery-budget').Configure.Value(11.0).Done;
    LNext.Pages[0].Find('project-title').Configure.Text('Changed candidate title').Done;
    GFailBuild := True;
    LRefused := False;
    LMessage := '';
    try
      LViews.TryRefresh(LNext, LNext.Pages[0]);
    except
      on LException: Exception do
      begin
        LRefused := True;
        LMessage := LException.Message;
      end;
    end;
    GFailBuild := False;
    Check(LRefused and (Pos('Intentional Studio Inspector factory refusal',
      LMessage) > 0), 'later Inspector preparation actually refuses');
    for LRole := Low(TNyxStudioSection) to High(TNyxStudioSection) do
    begin
      Check(LViews.SectionRoot(LRole) = LRoots[LRole],
        'preparation refusal preserves exact root / ' + NyxStudioSectionRootID(LRole));
    end;
    Check((LViews.InputFor('recovery-budget') = LInput) and
      (LViews.ControlFor('recovery-action') = LAction),
      'retained Chrome input and action identities survive refusal');
    Check((LViews.Root.Find('recovery-budget').Prop('value') = '10') and
      (InputText(LInput) = '12.5'), 'preparation rollback preserves exact unfinished draft');
    {$ifdef PAS2JS}
    Check((document.activeElement = LTitle) and
      (TJSHTMLInputElement(LTitle).selectionStart = 2) and
      (TJSHTMLInputElement(LTitle).selectionEnd = 7) and
      (InputText(LTitle) = LTitleText), 'preparation rollback preserves focused text/range');
    {$else}
    Check((Screen.ActiveControl = LTitle) and (TCustomEdit(LTitle).SelStart = 2) and
      (TCustomEdit(LTitle).SelLength = 5) and (InputText(LTitle) = LTitleText),
      'preparation rollback preserves focused text/range');
    Check(TWinControl(LInput).HandleAllocated and TWinControl(LTitle).HandleAllocated and
      (PtrUInt(TWinControl(LInput).Handle) = LInputHandle) and
      (PtrUInt(TWinControl(LTitle).Handle) = LTitleHandle) and
      (LInput.Parent = LInputParent) and (LTitle.Parent = LTitleParent),
      'preparation refusal retains allocated input windows under their original parents');
    {$endif}
    Click(LAction);
    Check(LToken.Active and (LScenario.Clicks = 1),
      'original registered callback remains live after refusal');
    Check(LScenario.BusyCaptureRefused and LScenario.BusyRestoreRefused and
      (InputText(LInput) = '12.5'),
      'actual borrowed-input callback refuses capture/restore without touching its draft');
    {$ifdef PAS2JS}await(Idle(LViews));{$else}Idle(LViews);{$endif}
    LNext.Pages[0].Find('studio-left').Add(
      NewNyxBadge('recovery-project-badge').WithText('Project extension'));
    GFailPreview := True;
    GProjectPreviewed := False;
    LRefused := False;
    LMessage := '';
    try
      LViews.TryRefresh(LNext, LNext.Pages[0]);
    except
      on LException: Exception do
      begin
        LRefused := True;
        LMessage := LException.Message;
      end;
    end;
    GFailPreview := False;
    Check(LRefused and GProjectPreviewed and
      (Pos('Intentional Studio Inspector physical preview refusal', LMessage) > 0),
      'second physical publication refuses after first Project preview');
    for LRole := Low(TNyxStudioSection) to High(TNyxStudioSection) do
    begin
      Check(LViews.SectionRoot(LRole) = LRoots[LRole],
        'grouped preview rollback preserves exact root / ' + NyxStudioSectionRootID(LRole));
    end;
    Check((InputText(LInput) = '12.5') and
      (LViews.Root.Find('recovery-budget').Prop('value') = '10') and LToken.Active,
      'grouped preview rollback preserves draft/model/callback');
    {$ifdef PAS2JS}
    Check((document.activeElement = LTitle) and
      (TJSHTMLInputElement(LTitle).selectionStart = 2) and
      (TJSHTMLInputElement(LTitle).selectionEnd = 7) and
      (InputText(LTitle) = LTitleText), 'grouped rollback preserves focused text/range');
    {$else}
    Check((Screen.ActiveControl = LTitle) and (TCustomEdit(LTitle).SelStart = 2) and
      (TCustomEdit(LTitle).SelLength = 5) and (InputText(LTitle) = LTitleText),
      'grouped rollback preserves focused text/range');
    Check(TWinControl(LInput).HandleAllocated and TWinControl(LTitle).HandleAllocated and
      (PtrUInt(TWinControl(LInput).Handle) = LInputHandle) and
      (PtrUInt(TWinControl(LTitle).Handle) = LTitleHandle) and
      (LInput.Parent = LInputParent) and (LTitle.Parent = LTitleParent),
      'grouped physical rollback preserves native input handles and their parents');
    {$endif}
    Check(LViews.Root.Find('recovery-project-badge') = nil,
      'refused candidate extension never joins the mounted lookup forest');
    Click(LAction);
    Check(LScenario.Clicks = 2, 'retained callback still executes once after grouped rollback');
    {$ifdef PAS2JS}await(Idle(LViews));{$else}Idle(LViews);{$endif}
    Check(LViews.TryRefresh(LNext, LNext.Pages[0]),
      'fresh later grouped refresh succeeds after both failure paths');
    Check((LViews.SectionRoot(nssChrome) = LRoots[nssChrome]) and
      (LViews.InputFor('recovery-budget') = LInput) and
      (LViews.SectionRoot(nssProject) <> LRoots[nssProject]) and
      (LViews.SectionRoot(nssInspector) <> LRoots[nssInspector]),
      'successful group replaces only Project and Inspector');
    Check((InputText(LInput) = '11') and
      (LViews.Root.Find('recovery-budget').Prop('value') = '11'),
      'explicit successful authored value supersedes the unfinished draft');
    Check(LScenario.Configurations >= 8,
      'creator configurator runs for freshly staged replacement renderers');
    {$ifdef PAS2JS}
    Check(Trim(window.getComputedStyle(LViews.ControlFor('recovery-project-badge'))
      .getPropertyValue('--nyx-accent')) = LTheme.Accent,
      'borrowed custom theme reaches the replacement Project view');
    {$else}
    Check(TCustomEdit(LViews.InputFor('project-title')).Font.Height = -LTheme.FontSize,
      'replacement Project input has its physical themed font');
    Check(TWinControl(LInput).HandleAllocated and
      (PtrUInt(TWinControl(LInput).Handle) = LInputHandle) and
      (LInput.Parent = LInputParent),
      'successful other-section publication retains the Chrome input native window');
    {$endif}
    Click(LAction);
    Check(LToken.Active and (LScenario.Clicks = 3), 'retained callback survives successful group');
    Check(LViews.SectionView(nssChrome).RestoreInteraction(LBeforeInputs) and
      (InputText(LInput) = '11') and
      (LViews.Root.Find('recovery-budget').Prop('value') = '11'),
      'stale copied draft cannot override a newly admitted authored value');
    { Exercise the complete owner, rather than only independent section swaps.
      A new Chrome descendant changes membership of its physical hierarchy. }
    LTitle := LViews.InputFor('project-title');
    Draft(LInput, '11.75');
    Draft(LTitle, TNyxText('A complete-frame thought / 🌙'));
    LTitleText := InputText(LTitle);
    {$ifdef PAS2JS}
    LTitle.focus;
    TJSHTMLInputElement(LTitle).setSelectionRange(2, 7);
    {$else}
    TCustomEdit(LTitle).SetFocus;
    TCustomEdit(LTitle).SelStart := 2;
    TCustomEdit(LTitle).SelLength := 5;
    {$endif}
    {$ifdef PAS2JS}await(Idle(LViews));{$else}Idle(LViews);{$endif}
    {$ifndef PAS2JS}
    { Project was intentionally replaced above. Capture that newly accepted
      title before testing refusal of the complete containing frame. }
    Check(TWinControl(LTitle).HandleAllocated,
      'replacement Project title has its accepted native window');
    LTitleHandle := PtrUInt(TWinControl(LTitle).Handle);
    LTitleParent := LTitle.Parent;
    {$endif}
    LFull := LNext.Clone;
    { The ordinary string field admits input to its private model immediately;
      the numeric domain above defers admission. Forward the accepted string
      baseline as a real composer would, rather than request an explicit reset. }
    LFull.Pages[0].Find('project-title').SetProp('value',
      LViews.Root.Find('project-title').Prop('value'));
    LFull.Pages[0].Add(NewNyxButton('recovery-new-chrome').WithText('New chrome'));
    Check(not LViews.TryRefresh(LFull, LFull.Pages[0]),
      'structural Chrome change requests complete frame admission');
    for LRole := Low(TNyxStudioSection) to High(TNyxStudioSection) do
    begin
      LRoots[LRole] := LViews.SectionRoot(LRole);
    end;
    LRefused := False;
    GFailBuild := True;
    try
      LViews.Render(LFull, LFull.Pages[0], LHost);
    except
      on LException: Exception do
      begin
        LRefused := True;
        LMessage := LException.Message;
      end;
    end;
    GFailBuild := False;
    Check(LRefused and (Pos('Intentional Studio Inspector factory refusal', LMessage) > 0),
      'later complete-frame factory failure refuses before old ownership retires');
    for LRole := Low(TNyxStudioSection) to High(TNyxStudioSection) do
    begin
      Check(LViews.SectionRoot(LRole) = LRoots[LRole],
        'complete refusal preserves exact root / ' + NyxStudioSectionRootID(LRole));
    end;
    Check((LViews.InputFor('recovery-budget') = LInput) and
      (LViews.InputFor('project-title') = LTitle) and LToken.Active and
      (InputText(LInput) = '11.75') and (InputText(LTitle) = LTitleText),
      'complete refusal recovers original controls, drafts and callback scope');
    {$ifdef PAS2JS}
    Check((document.activeElement = LTitle) and
      (TJSHTMLInputElement(LTitle).selectionStart = 2) and
      (TJSHTMLInputElement(LTitle).selectionEnd = 7),
      'complete refusal recovers physical focus and range');
    {$else}
    Check(TCustomEdit(LTitle).Focused and (TCustomEdit(LTitle).SelStart = 2) and
      (TCustomEdit(LTitle).SelLength = 5),
      'complete refusal recovers physical focus and range');
    Check(TWinControl(LInput).HandleAllocated and TWinControl(LTitle).HandleAllocated and
      (PtrUInt(TWinControl(LInput).Handle) = LInputHandle) and
      (PtrUInt(TWinControl(LTitle).Handle) = LTitleHandle) and
      (LInput.Parent = LInputParent) and (LTitle.Parent = LTitleParent),
      'complete-frame factory refusal retains accepted native windows and parents');
    {$endif}
    { Refuse at the embedding host boundary, earlier than any component
      factory. This covers locally owned allocation as well as frame recovery. }
    {$ifdef PAS2JS}
    GAppendParent := LHost.parentNode;
    GOriginalAppend := TJSFunction(TJSObject(GAppendParent)['appendChild']);
    LHostChildren := GAppendParent.childNodes.length;
    TJSObject(GAppendParent)['appendChild'] := @AppendFrame;
    {$else}
    LHostChildren := LHost.ControlCount;
    {$endif}
    GFailInsertion := True;
    LRefused := False;
    try
      try
        LViews.Render(LFull, LFull.Pages[0], LHost);
      except
        on LException: Exception do
        begin
          LRefused := True;
          LMessage := LException.Message;
        end;
      end;
    finally
      GFailInsertion := False;
      {$ifdef PAS2JS}
      TJSObject(GAppendParent)['appendChild'] := GOriginalAppend;
      GOriginalAppend := nil;
      GAppendParent := nil;
      {$endif}
    end;
    Check(LRefused and (GInsertionRefusals = 1) and
      (Pos('Intentional frame host insertion refusal', LMessage) > 0),
      'embedding host refusal reaches the actual unassigned frame boundary');
    LRootsRetained := True;
    for LRole := Low(TNyxStudioSection) to High(TNyxStudioSection) do
    begin
      LRootsRetained := LRootsRetained and (LViews.SectionRoot(LRole) = LRoots[LRole]);
    end;
    Check(LRootsRetained and (LViews.InputFor('project-title') = LTitle) and LToken.Active,
      'host refusal preserves exact prior roots, input and callback scope');
    {$ifdef PAS2JS}
    Check(LHost.parentNode.childNodes.length = LHostChildren,
      'host refusal detaches the actual local candidate');
    Check((document.activeElement = LTitle) and (InputText(LTitle) = LTitleText) and
      (TJSHTMLInputElement(LTitle).selectionStart = 2) and
      (TJSHTMLInputElement(LTitle).selectionEnd = 7),
      'host refusal preserves physical Unicode text, focus and range');
    {$else}
    Check(LHost.ControlCount = LHostChildren,
      'host refusal retains only the original host children');
    Check(TCustomEdit(LTitle).Focused and (InputText(LTitle) = LTitleText) and
      (TCustomEdit(LTitle).SelStart = 2) and (TCustomEdit(LTitle).SelLength = 5),
      'host refusal preserves physical Unicode text, focus and range');
    Check(TWinControl(LInput).HandleAllocated and TWinControl(LTitle).HandleAllocated and
      (PtrUInt(TWinControl(LInput).Handle) = LInputHandle) and
      (PtrUInt(TWinControl(LTitle).Handle) = LTitleHandle) and
      (LInput.Parent = LInputParent) and (LTitle.Parent = LTitleParent),
      'embedding-host refusal retains accepted native windows and parents');
    {$endif}
    LViews.Render(LFull, LFull.Pages[0], LHost, False, nil,
      [nssChrome, nssProject, nssInspector]);
    LInput := LViews.InputFor('recovery-budget');
    LTitle := LViews.InputFor('project-title');
    Check(InputText(LInput) = '11.75',
      'complete frame retains explicitly selected numeric draft / actual=' + InputText(LInput));
    Check(InputText(LTitle) = LTitleText,
      'complete frame retains explicitly selected Unicode draft / actual=' + InputText(LTitle));
    Check((LViews.SectionRoot(nssChrome) <> LRoots[nssChrome]) and
      (LViews.ControlFor('recovery-new-chrome') <> nil) and not LToken.Active and
      (LViews.Root.Find('recovery-budget').Prop('value') = '11'),
      'complete admission changes physical owners and revokes old callbacks without editing model values');
    {$ifdef PAS2JS}
    Check((document.activeElement = LTitle) and
      (TJSHTMLInputElement(LTitle).selectionStart = 2) and
      (TJSHTMLInputElement(LTitle).selectionEnd = 7),
      'complete admission transfers physical focus and range');
    {$else}
    Check(TCustomEdit(LTitle).Focused and (TCustomEdit(LTitle).SelStart = 2) and
      (TCustomEdit(LTitle).SelLength = 5),
      'complete admission transfers physical focus and range');
    {$endif}
    LViews.Render(LFull, LFull.Pages[0], LHost);
    LInput := LViews.InputFor('recovery-budget');
    LTitle := LViews.InputFor('project-title');
    Check((InputText(LInput) = '11') and
      (InputText(LTitle) = LFull.Pages[0].Find('project-title').Prop('value')),
      'default complete admission discards unqualified physical drafts');
    Draft(LInput, '11.875');
    LFull.Pages[0].Find('recovery-budget').SetProp('value', '13');
    LViews.Render(LFull, LFull.Pages[0], LHost, False, nil, [nssChrome]);
    LInput := LViews.InputFor('recovery-budget');
    Check(InputText(LInput) = '13',
      'forward continuity cannot overwrite a changed accepted value');
    LToken := LViews.Events.On(NyxControlEvents('recovery-action', niRuntime),
      ntClick).Subscribe(LLease);
    Click(LViews.ControlFor('recovery-action'));
    Check(LToken.Active and (LScenario.Clicks = 4),
      'new complete-frame callback scope executes once');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-capture-checkpoint', 'studio-recovery-live');
    LStarted := window.performance.now;
    LContinue := TJSHTMLElement(document.createElement('button'));
    LContinue.id := 'studio-recovery-capture-continue';
    LContinue.setAttribute('type', 'button');
    LContinue.className := 'nyx-button';
    LContinue.textContent := 'Continue after capture';
    LContinue.style.setProperty('position', 'fixed');
    LContinue.style.setProperty('right', '12px');
    LContinue.style.setProperty('bottom', '12px');
    LContinue.style.setProperty('z-index', '1000');
    LContinue.onclick := @ContinueCapture;
    document.body.appendChild(LContinue);
    try
      repeat
        await(TJSPromise.resolve(TJSPromise.new(@Pause)));

        if window.performance.now - LStarted > 30000 then
        begin
          raise ENyxModel.Create('Studio recovery capture acknowledgment timed out');
        end;
      until document.body.getAttribute('data-capture-observed') = 'studio-recovery-live';
    finally
      LContinue.onclick := nil;
      LContinue.remove;
    end;
    {$else}
    Application.ProcessMessages;

    if ParamCount > 0 then
    begin
      SaveNyxNativeCapture(LWindow, ParamStr(1), ncmPrint);
    end;
    {$endif}
    LScenario.Views := nil;
    LViews.Free;
    LViews := nil;
    Check(not LToken.Active, 'facade retirement revokes the retained event token');
    {$ifdef PAS2JS}await(StaticMembershipCases(LSession, LTheme, LHost));
    {$else}StaticMembershipCases(LSession, LTheme, LHost);{$endif}
    {$ifdef PAS2JS}await(ResourceSynchronizationCases(LSession, LTheme, LHost));
    {$else}ResourceSynchronizationCases(LSession, LTheme, LHost);{$endif}
    {$ifdef PAS2JS}await(InspectorFormCases(LTheme, LHost));
    {$else}InspectorFormCases(LTheme, LHost);{$endif}
    {$ifdef PAS2JS}await(CollectionFormCases(LTheme, LHost));
    {$else}CollectionFormCases(LTheme, LHost);{$endif}
    WriteLn('PASS ', GChecks, ' actual Studio section recovery checks');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-section-checks', IntToStr(GChecks));
    document.body.setAttribute('data-test-result', 'passed');
    {$endif}
    {$endif NYX_INSPECTOR_FORMS_ONLY}
  finally

    if LToken <> nil then
    begin
      LToken.Cancel;
    end;
    LToken := nil;
    LScenario.Views := nil;
    LViews.Free;
    LLease := nil;
    LFull.Free;
    LNext.Free;
    LBase.Free;
    LTheme.Free;
    LSession.Free;
    {$ifdef PAS2JS}LStyle.remove;{$endif}
    {$ifndef PAS2JS}LWindow.Free;{$endif}
  end;
end;

{$ifdef PAS2JS}
procedure Start; async;
begin
  try
    await(Run);
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-section-error', LException.Message);
      document.body.setAttribute('data-test-result', 'failed');
    end;
  end;
end;
{$endif}

begin
  {$ifdef PAS2JS}Start;{$else}
  try
    Run;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
  {$endif}
end.
