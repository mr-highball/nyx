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

unit nyx.studio.view;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  SysUtils,
  nyx.text,
  nyx.data,
  nyx.model,
  nyx.controls,
  nyx.view.recovery,
  nyx.colors,
  nyx.design.tokens,
  nyx.menu.editor,
  nyx.menu.bar.editor,
  nyx.collections.query.editor,
  nyx.times.editor,
  nyx.content.editor,
  nyx.theme.editor,
  nyx.image.editor,
  nyx.resources.editor,
  nyx.resources.browser,
  nyx.resources.workspace,
  nyx.resources.rows.editor,
  nyx.resources.runtime.view,
  nyx.contract,
  nyx.schema,
  nyx.types,
  nyx.responsive,
  nyx.presentations,
  nyx.layout.policy,
  nyx.state,
  nyx.binding.types,
  nyx.studio.authoring,
  nyx.studio.collections,
  nyx.studio.inspector,
  nyx.studio.help,
  nyx.studio.palette,
  nyx.studio.hierarchy,
  nyx.studio.session,
  nyx.studio.source,
  nyx.studio.compiler,
  nyx.studio.diagnostics,
  nyx.studio.agentview,
  nyx.studio.outputs, nyx.studio.edits, nyx.studio.drag, nyx.studio.resize, nyx.studio.move;

const
  { Stable mount for the library recovery compound on both editor targets. }
  NyxStudioDisplayRecoveryID = 'studio-display-recovery';

type
  { Compact hosts show one ordinary Nyx workspace panel at a time. The choice
    belongs to editor presentation and never changes project data/history. }
  TNyxStudioPanel = (nspDesign, nspProject, nspInspector);
  { Hosts can mount the same public source editor independently to preserve
    control lifetime across chrome refreshes. Inline remains the default for
    ordinary shell consumers; hosted adds a code panel and pane-hosted delegates
    the complete reusable workspace to an independently retained Nyx renderer. }
  TNyxStudioCodePresentation = (ncpInline, ncpHosted, ncpPaneHosted);
  { Source/messages are editor presentation; neither changes document history. }
  TNyxStudioSourceTab = (nstSource, nstMessages);
  { Closed allocation commands, separate from application design commands. }
  TNyxStudioWorkspaceAction = (swaUnknown, swaDetails, swaTools, swaExpand, swaRestore);

  { Target-independent Studio chrome and state. Both adapters consume this same
    Nyx document, including the public designer host and source editor. Platform
    controllers only route events, file/storage access and compiler transport.
    This record borrows no widgets or document nodes. Outputs is an optional
    borrowed configuration, read only while the view is built; nil means empty
    profiles. Copying the record never transfers profile ownership. }
  TNyxStudioViewState = record
    CodeVisible: Boolean;
    CodePresentation: TNyxStudioCodePresentation;
    SourceTab: TNyxStudioSourceTab;
    SourceExpanded: Boolean;
    { Editor presentation, never project content/history. Proportional sizing
      survives panel switches and host viewport changes on both targets. }
    CanvasPercent: Integer;
    { Transient workspace allocation. Collapse retains sync choices and tool
      settings; expansion temporarily gives the canvas the available host.
      The detail split uses the public Nyx touch/keyboard resize contract. }
    DetailsPercent: Integer;
    DetailsExpanded: Boolean;
    CanvasToolsVisible: Boolean;
    CanvasExpanded: Boolean;
    Phone: Boolean;
    { Exclusive manual preview choice, independent of authored pair/history. }
    PresentationSelection: TNyxPresentationSelection;
    { Transient physical-drop position; closed intent decoded at the UI boundary.
      It never changes exported documents or the accepted pair on its own. }
    DesignerPlacement: TNyxPlacement;
    DesignerAutomaticPlacement: Boolean;
    Palette: TNyxStudioPaletteState;
    Log: TNyxText;
    Status: TNyxText;
    { Source processor status sits beside the source actions as well as the
      global footer, so a compact host can observe preparation and refusal. }
    SourceStatus: TNyxText;
    { Transient target display readiness; never persisted with project content
      or treated as a source/compile result. The controller owns retry authority. }
    DisplayRecovery: TNyxViewRecovery;
    { Copied pending values keep typing visible while the independent processor
      prepares its pair. These affect only editor fields, never project content. }
    PendingDesign: TNyxStudioPendingDesign;
    OutputVisible: Boolean;
    OutputTarget: TNyxText;
    Outputs: TNyxOutputConfiguration;
    { Host execution capabilities, independent of the uncompiled designer. }
    CompiledPreviewAvailable: Boolean;
    CompiledPreviewRunning: Boolean;
    { File UI state is independent of output readiness. Conflicts are presented
      as explicit choices; no accepted session is replaced while one is pending. }
    FilesVisible: Boolean;
    ProjectName: TNyxText;
    ProjectConflict: Boolean;
    ImportConflict: Boolean;
    ProjectBusy: Boolean;
    AdvancedProperties: Boolean;
    InspectorTab: TNyxInspectorTab;
    { Open menu definition belongs to presentation, never application history. }
    MenuEditorReference: TNyxMenuRef;
    { Copied incomplete form input; exact context guards prevent stale replay. }
    MenuEditorDraft: TNyxMenuEditorDraft;
    { Independent scalar draft; shares the public form capture contract. }
    MenuBarEditorDraft: TNyxMenuBarEditorDraft;
    { Independent query input, guarded by the complete schema/binding baseline. }
    QueryEditorDraft: TNyxQueryEditorDraft;
    { Unsubmitted clock policy belongs to this project's editor presentation.
      Exact owner/local-effective context prevents stale inherited replay. }
    TimeDomainEditorDraft: TNyxTimeDomainEditorDraft;
    { Copied recipe proposal survives inspector parking and shell transitions. }
    ContentEditorDraft: TNyxContentEditorDraft;
    { Theme visibility/proposal belongs to this project's editor presentation.
      Changing it never edits a document or the independent Studio chrome theme. }
    ThemeVisible: Boolean;
    ThemeEditorDraft: TNyxThemeEditorDraft;
    { Imported pictures are independent proposals, retired when their exact
      selected image or its effective baseline changes. }
    ImageEditorDraft: TNyxImageEditorDraft;
    { Common file authoring is presentation until explicit paired Apply. }
    ResourcesVisible: Boolean;
    ResourceSelection: TNyxResourceEditorSelection;
    ResourceEditorDraft: TNyxResourceEditorDraft;
    ResourceBrowser: TNyxResourceBrowserState;
    ResourcesScroll: Integer;
    { Independent retained pane positions; compact navigation changes only
      presentation, preserving the complete file/row proposals and selection. }
    ResourcePane: TNyxResourceWorkspacePane;
    ResourceCatalogScroll: Integer;
    ResourceEditorScroll: Integer;
    ResourceRowsDraft: TNyxResourceRowsDraft;
    CallbackRemoval: TNyxCallbackRemoval;
    { Copied confirmation metadata, not an interface or borrowed model. }
    RootRemoval: TNyxDataValue;
    StateVisible: Boolean;
    BindingsVisible: Boolean;
    BindingTarget: TNyxBindingProperty;
    BindingDirection: TNyxBindingDirection;
    { New-default drafts are editor presentation, retained across shell/viewport
      refreshes. Only the explicit Add command admits them into project history. }
    NewStateName: TNyxText;
    NewStateInput: TNyxStudioStateInput;
    NewStateValue: TNyxText;
    { Compact uses one full-width panel. Panel is editor presentation state;
      neither field changes the application's designed phone/desktop output. }
    Compact: Boolean;
    Panel: TNyxStudioPanel;
    AgentsVisible: Boolean;
    BuildsVisible: Boolean;
    BuildControlReady: Boolean;
    Agents: TNyxStudioAgentView;
  end;

{ Initializes every field deliberately, including borrowed optional profiles. }
function DefaultNyxStudioViewState: TNyxStudioViewState;
{ Available-space policy shared by both controllers. Logical host dimensions,
  rather than operating system names, include short landscape phone windows. }
function NyxStudioCompactHost(AWidth, AHeight: Double): Boolean;
{ Closed workspace commands decoded only at the editor UI boundary. Returns
  False for unrelated IDs. Allocation never changes design/source/history. }
function RouteNyxStudioWorkspace(var AState: TNyxStudioViewState;
  const AControlID: TNyxText): Boolean;
{ Managed ordinary Nyx code editor, shared by inline and retained hosted views.
  The caller retains its interface or transfers ownership into a Nyx document. }
function NewNyxStudioCodeEditor(const ASource: TNyxText): INyxCodeEditor;
{ Owned source document with an independent readable semantic palette. Both
  adapters consume these typed tokens, including when the retained editor moves
  between inline and modal hosts. The caller owns/frees the document and may
  replace its tokens through SetNyxThemeTokens; project and shell themes remain
  independent. Allocation/composition failure frees the candidate before raising. }
function NewNyxStudioCodeDocument(const ASource: TNyxText): TNyxDocument;
{ Owned reusable source workspace. Its mutually exclusive source/messages views
  prevent compiler output from consuming editor height. Controllers may mount it
  independently and move the same Nyx view into a public modal host. Session and
  report are borrowed; a nil session raises ENyxModel before allocating a tree. }
function BuildNyxStudioSourcePane(ASession: TNyxStudioSession;
  const AState: TNyxStudioViewState; const AReport: INyxCompilerReport): TNyxNode;

{ Immediately suppress the previous variant's observation after retained
  proposal navigation. Root is borrowed; only the eight bounded stock detail
  cards show the new selection awaiting its reply. The controller synchronizes
  its mounted Nyx view. Incomplete stock parts raise before stale data is shown.
  The next exact observer reply rebuilds the matching detail presentation;
  this operation neither fetches data nor adds document/source history. }
procedure SuspendNyxStudioResourceSelectionViews(ARoot: TNyxNode;
  const ASelected: TNyxResourceEditorSelection; ASupported: Boolean);

{ Returns an owned Nyx UI document; session is borrowed and remains unmodified.
  The shell expresses application meaning through public Nyx component kinds.
  Native/browser styling and physical host lifetime belong to their adapters. }
function BuildNyxStudioView(ASession: TNyxStudioSession;
  const AState: TNyxStudioViewState): TNyxDocument; overload;
{ Report is borrowed during composition. Passing it directly keeps interface
  ownership explicit and works on pas2js, which forbids COM interfaces in records. }
function BuildNyxStudioView(ASession: TNyxStudioSession;
  const AState: TNyxStudioViewState; const AReport: INyxCompilerReport): TNyxDocument; overload;

implementation

uses
  nyx.binding, nyx.resources, nyx.application.resources,
  nyx.composition, nyx.studio.rootview, nyx.studio.buildview;

procedure SuspendNyxStudioResourceSelectionViews(ARoot: TNyxNode;
  const ASelected: TNyxResourceEditorSelection; ASupported: Boolean);
var
  LIndex: Integer;
  LCard: TNyxNode;
  LEntry: TNyxResourceRuntimeEntry;
begin

  if ARoot = nil then
  begin
    Exit;
  end;
  LEntry := Default(TNyxResourceRuntimeEntry);
  LEntry.Reference := ASelected.Reference;
  LEntry.Locale := ASelected.Locale;
  for LIndex := 0 to 7 do
  begin
    LCard := ARoot.Find('studio-resource-runtime-' + TNyxText(IntToStr(LIndex)) + '-selection');

    if LCard <> nil then
    begin

      if not TryRefreshNyxResourceRuntimeDetailView(LCard, LEntry, rdaAwaiting) then
      begin
        raise ENyxModel.Create('Selected resource observation parts changed');
      end;
      LCard.Configure.Visible(ASupported and ASelected.Reference.Defined).Done;
    end;
  end;
end;

{ Runtime reports arrive as bounded copied transport values. Ordinary Nyx cards
  paint the same summary on both targets, independently of authored resources.
  No report grants this view a handle to start or cancel an application. }
procedure AddResourceRuntimeViews(AParent: TNyxNode; const AReports: TNyxDataValue;
  const ASelected: TNyxResourceEditorSelection; ASupportSelected: Boolean);
var
  LIndex: Integer;
  LFieldIndex: Integer;
  LReport: TNyxDataValue;
  LSelection: TNyxDataValue;
  LDetail: TNyxResourceRuntimeDetail;
  LEntry: TNyxResourceRuntimeEntry;
  LAvailability: TNyxResourceDetailAvailability;
  LCard: INyxCard;
  LRetired: INyxBadge;
  LGroup: INyxColumn;
  LID: TNyxText;
begin
  AParent.Add(NewNyxHeading('studio-resource-runtime-title').WithText('Runtime observations'));

  if not AReports.Defined or (AReports.Kind = ndNull) or (AReports.Count = 0) then
  begin
    AParent.Add(NewNyxLabel('studio-resource-runtime-empty').WithText(
      'No application host has shared a resource report. Authored defaults remain available above.'));
    Exit;
  end;

  if (AReports.Kind <> ndArray) or (AReports.Count > 8) then
  begin
    raise ENyxModel.Create('Resource runtime observer requires at most eight reports');
  end;
  for LIndex := 0 to AReports.Count - 1 do
  begin
    LReport := AReports.Item(LIndex);
    LID := 'studio-resource-runtime-' + TNyxText(IntToStr(LIndex));
    LGroup := NewNyxColumn(LID);
    LGroup.Configure.Gap(8).Done;
    LGroup.Add(NewNyxLabel(LID + '-identity').WithText(LReport.Field('run').AsText +
      ' / ' + LReport.Field('scope').AsText + ' / ' + LReport.Field('target').AsText));

    LRetired := NewNyxBadge(LID + '-retired').WithText('Observation retired');
    LRetired.Configure.Visible(not LReport.Field('active').AsBoolean).Done;
    LGroup.Add(LRetired);
    LGroup.Add(NewNyxResourceRuntimeView(LID + '-status',
      TNyxResourceRuntimeSummary.FromData(LReport.Field('summary'))));
    LSelection := NyxNull;
    for LFieldIndex := 0 to LReport.Count - 1 do
    begin

      if LReport.Key(LFieldIndex) = 'selection' then
      begin
        LSelection := LReport.Field('selection');
      end;
    end;

    LAvailability := rdaAwaiting;
    LEntry := Default(TNyxResourceRuntimeEntry);
    LEntry.Reference := ASelected.Reference;
    LEntry.Locale := ASelected.Locale;

    if (LSelection.Kind = ndObject) and (LSelection.Count = 3) and
      (ASelected.Reference.Name <> '') and
      (LSelection.Field('reference').AsText = ASelected.Reference.Name) and
      (LSelection.Field('locale').AsText = ASelected.Locale.Name) then
    begin

      if LSelection.Field('entry').Kind = ndNull then
      begin
        LAvailability := rdaMissing;
      end
      else
      begin
        LDetail := TNyxResourceRuntimeDetail.FromData(LSelection.Field('entry'));

        if (LDetail.Entry.Reference.Name <> ASelected.Reference.Name) or
          (LDetail.Entry.Locale.Name <> ASelected.Locale.Name) then
        begin
          raise ENyxResource.Create('Observed detail belongs to another selected resource');
        end;
        LEntry := LDetail.Entry;
        LAvailability := rdaAvailable;
      end;
    end;
    LCard := NewNyxResourceRuntimeDetailView(LID + '-selection', LEntry, LAvailability);
    LCard.Configure.Visible(ASupportSelected and (ASelected.Reference.Name <> '')).Done;
    LGroup.Add(LCard);
    AParent.Add(LGroup);
  end;
end;

const
  CCompactWidth = 961;
  CShortWidth = 1201;
  CShortHeight = 501;

function NyxStudioCompactHost(AWidth, AHeight: Double): Boolean;
begin
  Result := TNyxViewportCondition.Any.WidthBelow(CCompactWidth).Matches(AWidth, AHeight) or
    TNyxViewportCondition.Any.WidthBelow(CShortWidth).HeightBelow(CShortHeight)
      .Matches(AWidth, AHeight);
end;

function RouteNyxStudioWorkspace(var AState: TNyxStudioViewState;
  const AControlID: TNyxText): Boolean;
const
  CControlIDs: array[TNyxStudioWorkspaceAction] of TNyxText = ('',
    'action-details-toggle', 'action-canvas-tools', 'action-canvas-expand',
    'action-workspace-restore');
var
  LAction: TNyxStudioWorkspaceAction;
begin
  LAction := swaUnknown;
  for LAction := swaDetails to swaRestore do
  begin

    if AControlID = CControlIDs[LAction] then
    begin
      Break;
    end;
  end;
  Result := AControlID = CControlIDs[LAction];

  if not Result then
  begin
    Exit;
  end;
  case LAction of
    swaDetails:
      begin
        AState.DetailsExpanded := not AState.DetailsExpanded;
      end;
    swaTools:
      begin
        AState.CanvasToolsVisible := not AState.CanvasToolsVisible;
        AState.CanvasExpanded := False;
        AState.Panel := nspDesign;
      end;
    swaExpand:
      begin
        AState.CanvasExpanded := not AState.CanvasExpanded;
        AState.Panel := nspDesign;
      end;
    swaRestore:
      begin
        AState.CanvasExpanded := False;
      end;
  else
    begin
      Result := False;
    end;
  end;
end;

function NewNyxStudioCodeEditor(const ASource: TNyxText): INyxCodeEditor;
begin
  Result := NewNyxCodeEditor('studio-code');
  Result.Configure.Text('Pascal source').ReadOnly(False).Flex(1)
    .Hint('Edit typed configuration, defaults, bindings, contracts and data, then Apply Pascal. Keep application helpers outside nyx:views.')
    .Value(ASource).Done;
end;

function NewNyxStudioCodeDocument(const ASource: TNyxText): TNyxDocument;
begin
  Result := TNyxDocument.Create;
  try
    { A source pane is its own Nyx view. Its palette belongs to that view rather
      than unscoped browser chrome, which cannot override an isolated theme and
      would leave the native adapter with different colors. Complete defaults
      retain the other semantic roles beside the source-specific colors. }
    SetNyxThemeTokens(Result, NyxThemePreset(ntpDark)
      .Surface(NyxRGB(23, 27, 41)).Text(NyxRGB(203, 213, 237))
      .Border(NyxRGB(52, 58, 78)));
    Result.AddPage(NewNyxStudioCodeEditor(ASource));
  except
    Result.Free;
    raise;
  end;
end;

function DefaultNyxStudioViewState: TNyxStudioViewState;
begin
  { Managed strings/arrays do not initialize every native record scalar. Start
    the complete presentation record deliberately, including pending row/form
    flags, before supplying its nonzero editor defaults. }
  Result := Default(TNyxStudioViewState);
  Result.CodeVisible := False;
  Result.CodePresentation := ncpInline;
  Result.PresentationSelection := TNyxPresentationSelection.None;
  Result.CanvasPercent := 65;
  Result.DetailsPercent := 32;
  Result.Phone := False;
  Result.Palette := DefaultNyxStudioPaletteState;
  Result.ResourceBrowser := NyxResourceBrowserState;
  Result.Log := '';
  Result.Status := 'Ready to design';
  Result.OutputVisible := False;
  Result.OutputTarget := '';
  Result.Outputs := nil;
  Result.CompiledPreviewAvailable := False;
  Result.CompiledPreviewRunning := False;
  Result.FilesVisible := False;
  Result.ProjectName := '';
  Result.ProjectConflict := False;
  Result.ImportConflict := False;
  Result.ProjectBusy := False;
  Result.AdvancedProperties := False;
  Result.InspectorTab := nitProperties;
  Result.CallbackRemoval.Pending := False;
  Result.RootRemoval := NyxNull;
  Result.StateVisible := False;
  Result.BindingsVisible := False;
  Result.BindingTarget := bpValue;
  Result.BindingDirection := bdTwoWay;
  Result.NewStateName := '';
  Result.NewStateInput := ssiText;
  Result.NewStateValue := '';
  Result.Compact := False;
  Result.Panel := nspDesign;
  Result.AgentsVisible := False;
  Result.Agents := DefaultNyxStudioAgentView;
end;

function Button(const AID, AText: TNyxText): TNyxNode;
begin
  Result := TNyxNode.Create(nkButton, AID).Configure.Text(AText).Done;
end;

function Caption(const AID, AText: TNyxText): TNyxNode;
begin
  Result := TNyxNode.Create(nkLabel, AID).Configure.Text(AText).Done;
end;

function BuildNyxStudioSourcePane(ASession: TNyxStudioSession;
  const AState: TNyxStudioViewState; const AReport: INyxCompilerReport): TNyxNode;
var
  LActions: TNyxNode;
  LTabs: TNyxNode;
  LButton: TNyxNode;
  LMessages: TNyxNode;
  LField: TNyxNode;
  LMessageCount: Integer;
begin

  if ASession = nil then
  begin
    raise ENyxModel.Create('A source workspace requires its borrowed Studio session');
  end;
  Result := TNyxNode.Create(nkColumn, 'studio-source-pane')
    .Configure.Gap(0).Padding(0).Flex(1).Done;
  try
    Result.Add(NewNyxLabel('studio-source-status').Configure
      .Text(AState.SourceStatus).Hint('Current Pascal source operation')
      .Visible(AState.SourceStatus <> '')
      .WhenViewport(TNyxViewportWidth.Below(640)).Visible(False).Done);
    LActions := TNyxNode.Create(nkRow, 'studio-code-actions')
      .Configure.Gap(8).Layout(TNyxLayoutPolicy.Row.Wrap(nfwWrap)).Done;
    Result.Add(LActions);
    { These width conditions use the source pane's available space on both
      targets. The same buttons, callbacks and full hints remain reachable;
      compact presentation does not create a second set of editor actions. }
    LActions.Add(Button('action-apply-source', 'Apply Pascal').Configure
      .Hint('Apply the Pascal draft')
      .WhenViewport(TNyxViewportWidth.Below(640)).Text('Apply').Done);
    LActions.Add(Button('action-reset-source', 'Restore accepted').Configure
      .Hint('Discard the pending draft and restore accepted Pascal')
      .WhenViewport(TNyxViewportWidth.Below(640)).Text('Restore').Done);
    LActions.Add(Button('action-export-source-draft', 'Save draft').Configure
      .Hint('Download the current Pascal draft')
      .WhenViewport(TNyxViewportWidth.Below(640)).Text('Save draft').Done);
    LButton := Button('action-expand-source', 'Expand');
    LActions.Add(LButton);

    if AState.SourceExpanded then
    begin
      LButton.Configure.Text('Close').Hint('Return to the split editor').Done;
    end;
    LTabs := TNyxNode.Create(nkRow, 'studio-source-tabs')
      .Configure.Gap(8).Layout(TNyxLayoutPolicy.Row.Wrap(nfwWrap)).Done;
    Result.Add(LTabs);
    LButton := Button('action-source-tab', 'Source');
    LButton.Configure.Pressed(AState.SourceTab = nstSource).Done;
    LTabs.Add(LButton);

    if AState.SourceTab = nstSource then
    begin
      LButton.Configure.Variant(nvPrimary).Done;
    end;
    LMessageCount := 0;

    if AReport <> nil then
    begin
      LMessageCount := AReport.Count;
    end;
    LButton := Button('action-messages-tab', 'Compiler messages (' + IntToStr(LMessageCount) + ')');
    LButton.Configure.Pressed(AState.SourceTab = nstMessages)
      .Hint('Compiler messages')
      .WhenViewport(TNyxViewportWidth.Below(640))
      .Text('Messages (' + IntToStr(LMessageCount) + ')').Done;
    LTabs.Add(LButton);

    if AState.SourceTab = nstMessages then
    begin
      LButton.Configure.Variant(nvPrimary).Done;
    end;
    LMessages := TNyxNode.Create(nkColumn, 'studio-source-messages')
      .Configure.Gap(8).Padding(0).Flex(1).Visible(AState.SourceTab = nstMessages).Done;
    Result.Add(LMessages);
    LField := BuildNyxSourceDiagnostic(ASession);

    if LField <> nil then
    begin
      LMessages.Add(LField);
    end;
    LField := BuildNyxCompilerDiagnostics(ASession, AReport);

    if LField <> nil then
    begin
      LField.Configure.Clear(atHeight).Flex(1).Done;
      LMessages.Add(LField);
    end
    else
    begin
      LMessages.Add(NewNyxLabel('studio-source-no-messages').Configure
        .Text('No compiler messages. Build a view or application to see results here.').Done);
    end;

    if AState.CodePresentation <> ncpInline then
    begin
      Result.Add(TNyxNode.Create(nkPanel, 'studio-code-host').Configure
        .Layout(nlColumn).Gap(0).Padding(0).Flex(1).Visible(AState.SourceTab = nstSource).Done);
    end
    else
    begin
      Result.Add(NewNyxStudioCodeEditor(ASession.DraftSource).Configure
        .Visible(AState.SourceTab = nstSource).Done);
    end;
  except
    Result.Free;
    raise;
  end;
end;

function StateEditor(const AID, ATitle, AValue: TNyxText;
  AInput: TNyxStudioStateInput): TNyxNode;
var
  LKind: TNyxKind;
begin
  LKind := nkInput;

  if AInput in [ssiText, ssiEscapedText] then
  begin
    LKind := nkMemo;
  end
  else if AInput = ssiBoolean then
  begin
    LKind := nkSelect;
  end;
  Result := TNyxNode.Create(LKind, AID);
  try
    Result.Configure.Text(ATitle).Value(AValue).Done;

    if AInput = ssiBoolean then
    begin
      Result.Configure.Items('false' + #10 + 'true').Done;
    end
    else if AInput = ssiEscapedText then
    begin
      Result.Configure.Hint('One quoted text literal; escapes preserve control characters.').Done;
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure AddStatePanel(AParent: TNyxNode; ASession: TNyxStudioSession;
  const AState: TNyxStudioViewState);
var
  LPanel: TNyxNode;
  LRow: TNyxNode;
  LField: TNyxNode;
  LIndex: Integer;
  LInput: TNyxStudioStateInput;
  LValue: TNyxStateValue;
  LKey: TNyxText;
  LItems: TNyxText;
  LEditorText: TNyxText;
  LNameText: TNyxText;
  LPendingInput: TNyxStudioStateInput;
begin
  AParent.Add(Button(NyxStudioStateToggleID, 'Data (' +
    IntToStr(ASession.Document.State.Count + ASession.Document.Collections.Count) + ')'));

  if not AState.StateVisible then
  begin
    Exit;
  end;
  LPanel := TNyxNode.Create(nkColumn, 'studio-state');
  AParent.Add(LPanel);
  LPanel.Configure.Gap(12).Done;
  LPanel.Add(Caption('state-help',
    'Saved defaults initialize each application. Bind controls in the inspector.'));
  for LIndex := 0 to ASession.Document.State.Count - 1 do
  begin
    LKey := ASession.Document.State.Key(LIndex);
    LValue := ASession.Document.State.Value(LKey);
    LInput := NyxStudioStateInputFor(LValue);

    if AState.PendingDesign.StateEditorInput(LKey, LValue.Kind, LPendingInput) then
    begin
      LInput := LPendingInput;
    end;

    if not AState.PendingDesign.StateName(LKey, LValue.Kind, LNameText) then
    begin
      LNameText := LKey;
    end;
    LEditorText := NyxStudioStateEditorText(LValue);

    if not AState.PendingDesign.StateValue(LKey, LValue.Kind, LEditorText) then
    begin
      LEditorText := NyxStudioStateEditorText(LValue);
    end;
    LRow := TNyxNode.Create(nkPanel, 'state-row-' + IntToStr(LIndex));
    LPanel.Add(LRow);
    LRow.Configure.Surface(True).Padding(12).Gap(8)
      .Enabled(not AState.PendingDesign.StateLocked(LKey)).Done;
    LField := TNyxNode.Create(nkInput, 'state-name-' + IntToStr(LIndex));
    LRow.Add(LField);
    LField.Configure.Text('Name').Value(LNameText)
      .Extension(NyxStudioStateKey, LKey)
      .Extension(NyxStudioStateInputKey, NyxStudioStateInputName(LInput))
      .Extension(NyxStudioStateCommandKey, NyxStudioStateCommandName(sscRenameDraft)).Done;
    LRow.Add(Button('state-rename-' + IntToStr(LIndex), 'Rename').Configure
      .Extension(NyxStudioStateKey, LKey)
      .Extension(NyxStudioStateInputKey, NyxStudioStateInputName(LInput))
      .Extension(NyxStudioStateNameInputKey, LField.ID)
      .Extension(NyxStudioStateCommandKey, NyxStudioStateCommandName(sscRename)).Done);
    LField := StateEditor('state-default-' + IntToStr(LIndex),
      NyxStudioStateInputName(LInput) + ' default', LEditorText, LInput);
    LRow.Add(LField);
    LField.Configure.Extension(NyxStudioStateKey, LKey)
      .Extension(NyxStudioStateInputKey, NyxStudioStateInputName(LInput))
      .Extension(NyxStudioStateCommandKey, NyxStudioStateCommandName(sscDefault)).Done;
    LRow.Add(Button('state-remove-' + IntToStr(LIndex), 'Remove default').Configure
      .Extension(NyxStudioStateKey, LKey)
      .Extension(NyxStudioStateInputKey, NyxStudioStateInputName(LInput))
      .Extension(NyxStudioStateCommandKey, NyxStudioStateCommandName(sscRemove)).Done);
  end;
  LRow := TNyxNode.Create(nkPanel, 'state-new');
  LPanel.Add(LRow);
  LRow.Configure.Surface(True).Padding(12).Gap(8)
    .Enabled(not AState.PendingDesign.NewDefaultPending).Done;
  LRow.Add(Caption('state-new-title', 'NEW DEFAULT'));
  LRow.Add(TNyxNode.Create(nkInput, NyxStudioNewStateNameID).Configure
    .Text('Name').Placeholder('replyText').Value(AState.NewStateName).Done);
  LItems := '';
  for LInput := Low(TNyxStudioStateInput) to High(TNyxStudioStateInput) do
  begin

    if LItems <> '' then
    begin
      LItems := LItems + #10;
    end;
    LItems := LItems + NyxStudioStateInputName(LInput);
  end;
  LRow.Add(TNyxNode.Create(nkSelect, NyxStudioNewStateInputID).Configure.Text('Type')
    .Items(LItems).Value(NyxStudioStateInputName(AState.NewStateInput)).Done);
  LRow.Add(StateEditor(NyxStudioNewStateValueID, 'Default', AState.NewStateValue, AState.NewStateInput));
  LRow.Add(Button(NyxStudioAddStateID, 'Add default').Configure.Variant(nvPrimary).Done);
end;

procedure AddBindingsPanel(AParent: TNyxNode; ASession: TNyxStudioSession;
  AProjection: TNyxNode; const AState: TNyxStudioViewState);
var
  LPanel: TNyxNode;
  LChoices: TNyxBindingTargetInfos;
  LTarget: TNyxBindingProperty;
  LSpec: TNyxBindingSpec;
  LHasBinding: Boolean;
  LLocal: Boolean;
  LFound: Boolean;
  LIndex: Integer;
  LCount: Integer;
  LKey: TNyxText;
  LItems: TNyxText;
  LDirection: TNyxBindingDirection;
  LValue: TNyxStateValue;
  LAllowedKinds: TNyxStateKinds;
  LPendingBinding: Boolean;
  LInheritPending: Boolean;
begin
  AParent.Add(Button(NyxStudioBindingsToggleID, 'Bindings'));

  if not AState.BindingsVisible then
  begin
    Exit;
  end;
  LPanel := TNyxNode.Create(nkPanel, 'studio-bindings');
  AParent.Add(LPanel);
  LPanel.Configure.Surface(True).Padding(12).Gap(8).Done;

  if AProjection = nil then
  begin
    LPanel.Add(Caption('binding-help', 'This selection has no control projection.'));
    Exit;
  end;
  LChoices := NyxBindingTargets(AProjection);

  if Length(LChoices) = 0 then
  begin
    LPanel.Add(Caption('binding-help', 'This control exposes no portable binding targets.'));
    Exit;
  end;
  LTarget := AState.BindingTarget;
  LFound := False;
  LItems := '';
  for LIndex := 0 to Length(LChoices) - 1 do
  begin
    LFound := LFound or (LChoices[LIndex].Target = LTarget);

    if LItems <> '' then
    begin
      LItems := LItems + #10;
    end;
    LItems := LItems + LChoices[LIndex].Title;
  end;

  if not LFound then
  begin
    LTarget := LChoices[0].Target;
  end;
  LPanel.Add(TNyxNode.Create(nkSelect, NyxStudioBindingTargetID).Configure.Text('Control property')
    .Items(LItems).Value(NyxBindingPropertyTitle(LTarget)).Done);
  LHasBinding := AProjection.FindBinding(LTarget, LSpec);
  LPendingBinding := AState.PendingDesign.Binding(ASession.SelectedID, LTarget,
    LSpec, LInheritPending);

  if LPendingBinding then
  begin
    LHasBinding := not LSpec.Cleared and not LInheritPending;
  end
  else
  begin
    { An absent pending descriptor clears its out value; retain the accepted
      effective projection rather than letting discovery change presentation. }
    LHasBinding := AProjection.FindBinding(LTarget, LSpec);
  end;
  LKey := 'Unbound';
  LDirection := AState.BindingDirection;

  if LHasBinding then
  begin
    LKey := LSpec.StateName;
    LDirection := LSpec.Direction;
  end;

  if LInheritPending then
  begin
    LKey := 'Inherited binding pending';
  end;
  LPanel.Add(Caption('binding-current', 'Current: ' + LKey));

  if LTarget = bpValue then
  begin
    LPanel.Add(TNyxNode.Create(nkSelect, NyxStudioBindingFlowID).Configure.Text('Flow')
      .Items(NyxStudioBindingDirectionTitle(bdTwoWay) + #10 +
        NyxStudioBindingDirectionTitle(bdFromState))
      .Value(NyxStudioBindingDirectionTitle(LDirection))
      .Enabled(not LInheritPending)
      .Extension(NyxStudioBindingOwnerKey, ASession.SelectedID)
      .Extension(NyxStudioBindingTargetKey, NyxBindingPropertyName(LTarget)).Done);
  end;
  LPanel.Add(Caption('binding-choices-title', 'COMPATIBLE STATE TYPES'));
  LCount := 0;
  LAllowedKinds := NyxBindingKinds(AProjection, LTarget);
  for LIndex := 0 to ASession.Document.State.Count - 1 do
  begin
    LKey := ASession.Document.State.Key(LIndex);
    LValue := ASession.Document.State.Value(LKey);

    if LValue.Kind in LAllowedKinds then
    begin
      Inc(LCount);
      LPanel.Add(Button('binding-state-' + IntToStr(LIndex), LKey).Configure
        .Enabled(not AState.PendingDesign.StateLocked(LKey))
        .Extension(NyxStudioBindingOwnerKey, ASession.SelectedID)
        .Extension(NyxStudioStateKey, LKey)
        .Extension(NyxStudioBindingTargetKey, NyxBindingPropertyName(LTarget))
        .Extension(NyxStudioBindingCommandKey, NyxStudioBindingCommandName(sbcChoose)).Done);
    end;
  end;

  if LCount = 0 then
  begin
    LPanel.Add(Caption('binding-empty', 'Add a compatible default in Project > State.'));
  end;
  LPanel.Add(Button('binding-clear', 'Unbind property').Configure.Enabled(LHasBinding)
    .Extension(NyxStudioBindingOwnerKey, ASession.SelectedID)
    .Extension(NyxStudioBindingTargetKey, NyxBindingPropertyName(LTarget))
    .Extension(NyxStudioBindingCommandKey, NyxStudioBindingCommandName(sbcClear)).Done);

  if (ASession.Selected.Kind = 'slot-override') or
    (ASession.Selected.ProjectionKind = 'component') then
  begin
    LLocal := False;
    for LIndex := 0 to ASession.Selected.BindingCount - 1 do
    begin
      LLocal := LLocal or (ASession.Selected.Bindings[LIndex].Target = LTarget);
    end;

    if LPendingBinding then
    begin
      LLocal := not LInheritPending;
    end;
    LPanel.Add(Button('binding-inherit', 'Use inherited binding').Configure.Enabled(LLocal)
      .Extension(NyxStudioBindingOwnerKey, ASession.SelectedID)
      .Extension(NyxStudioBindingTargetKey, NyxBindingPropertyName(LTarget))
      .Extension(NyxStudioBindingCommandKey, NyxStudioBindingCommandName(sbcInherit)).Done);
  end;
end;

procedure AddOutputPanel(AParent: TNyxNode; const AState: TNyxStudioViewState);
const
  CLabels: array[0..5] of TNyxText = ('pas2js compiler', 'Matching rtl.js',
    'FPC compiler', 'Lazarus root', 'Platform (CPU-OS)', 'Widgetset');
var
  LPanel: TNyxNode;
  LTargets: TNyxNode;
  LButton: TNyxNode;
  LField: TNyxNode;
  LIndex: Integer;
begin
  { Output selection has no dependency on profile readiness. These ordinary
    Nyx fields edit local tool settings; no machine paths enter project nodes. }
  LPanel := TNyxNode.Create('column', 'studio-outputs');
  AParent.Add(LPanel);
  LPanel.Add(TNyxNode.Create('heading', 'outputs-title').SetProp('text', 'Target / output'));
  LPanel.Add(Caption('outputs-help',
    'Design freely. Choose and configure an output whenever you are ready to build.'));
  LTargets := TNyxNode.Create('row', 'output-targets');
  LPanel.Add(LTargets);
  for LIndex := 0 to 2 do
  begin
    case LIndex of
      0:
        begin
          LButton := Button('output-none', 'Choose later').SetProp('output-target', '');
        end;
      1:
        begin
          LButton := Button('output-browser', 'Browser').SetProp('output-target', 'browser');
        end;
      2:
        begin
          LButton := Button('output-lcl', 'Native LCL').SetProp('output-target', 'lcl');
        end;
    end;

    if LButton.Prop('output-target') = AState.OutputTarget then
    begin
      LButton.SetProp('variant', 'primary');
    end;
    LTargets.Add(LButton);
  end;
  for LIndex := 0 to High(NyxOutputFields) do
  begin

    if ((AState.OutputTarget = 'browser') and (LIndex < 2)) or
      ((AState.OutputTarget = 'lcl') and (LIndex >= 2)) then
    begin
      LField := TNyxNode.Create('input', 'output-' + NyxOutputFields[LIndex])
        .SetProp('text', CLabels[LIndex]).SetProp('output-field', NyxOutputFields[LIndex])
        .SetProp('placeholder', 'Optional until this output is built');

      if AState.Outputs <> nil then
      begin
        LField.SetProp('value', AState.Outputs.Field(NyxOutputFields[LIndex]));
      end;
      LPanel.Add(LField);
    end;
  end;
  LPanel.Add(Caption('outputs-privacy', 'Compiler paths are saved only on this machine.'));
  LPanel.Add(Button('action-save-outputs', 'Apply configuration'));
  LPanel.Add(Button('action-reload-outputs', 'Reload saved configuration'));
end;

procedure AddPartChoices(AParent, ARuntime: TNyxNode;
  const APath: TNyxText; var ACount: Integer);
var
  LIndex: Integer;
  LPart: TNyxNode;
  LPath: TNyxText;
begin
  { A named path follows the same direct-part contract as TNyxNode.Part.
    Buttons carry the path as data; integer chrome IDs avoid user text becoming
    a command identity. Expanded nested reusable parts are ordinary choices. }
  for LIndex := 0 to ARuntime.Count - 1 do
  begin
    LPart := ARuntime.Children[LIndex];

    if LPart.Prop('part') <> '' then
    begin
      LPath := LPart.Prop('part');

      if APath <> '' then
      begin
        LPath := APath + '/' + LPath;
      end;
      Inc(ACount);
      AParent.Add(Button('customize-part-' + IntToStr(ACount), 'Customize ' + LPath)
        .SetProp('override-path', LPath));
      AddPartChoices(AParent, LPart, LPath, ACount);
    end;
  end;
end;

function BuildNyxStudioView(ASession: TNyxStudioSession;
  const AState: TNyxStudioViewState): TNyxDocument;
begin
  Result := BuildNyxStudioView(ASession, AState, nil);
end;

{ Populate a borrowed owning document. The public wrapper releases it if any
  creator, projection or pending inspector proposal refuses composition. }
procedure PopulateNyxStudioView(ASession: TNyxStudioSession;
  const AState: TNyxStudioViewState; const AReport: INyxCompilerReport;
  ADocument: TNyxDocument);
var
  Result: TNyxDocument;
  LRoot: TNyxNode;
  LHeader: TNyxNode;
  LWorkspace: TNyxNode;
  LSplit: TNyxNode;
  LCodePane: TNyxNode;
  LLeft: TNyxNode;
  LCenter: TNyxNode;
  LStage: TNyxNode;
  LDetails: TNyxNode;
  LDetailSplit: TNyxNode;
  LSummary: TNyxNode;
  LRight: TNyxNode;
  LResources: TNyxNode;
  LResourceForm: INyxColumn;
  LViews: TNyxNode;
  LViewbar: TNyxNode;
  LCanvas: TNyxNode;
  LFooter: TNyxNode;
  LButton: TNyxNode;
  LField: TNyxNode;
  LSelected: TNyxNode;
  LCanDragSelection: Boolean;
  LMoveTools: TNyxNode;
  LHelpIndex: Integer;
  LIndex: Integer;
  LKind: TNyxText;
  LName: TNyxText;
  LPendingValue: TNyxText;
  LProperties: TNyxPropertyInfos;
  LPrimitive: TNyxPrimitiveInfo;
  LPropertyIndex: Integer;
  LKnown: Boolean;
  LMetadataSource: TNyxNode;
  LPartView: TNyxNode;
  LPartCount: Integer;
  LPanelbar: TNyxNode;
  LInspectorTabs: TNyxNode;
  LPlacementActions: TNyxNode;
  LSelectedProjection: TNyxNode;
  LBindingTarget: TNyxBindingProperty;
  LBinding: TNyxBindingSpec;
  LAttribute: TNyxAttribute;
  LPlatform: TNyxPlatform;
  LViewport: TNyxPresentationCondition;
  LPresentation: TNyxPresentationRef;
  LFieldID: TNyxText;
  LHasDetails: Boolean;
begin
  { Reject a missing controller before allocating any owned shell nodes. }

  if ASession = nil then
  begin
    raise ENyxModel.Create('Studio session is required');
  end;
  Result := ADocument;
  Result.Title := 'Nyx Studio';
  Result.Presentations.Define(NyxPresentation('compact'),
    TNyxViewportCondition.Any.WidthBelow(CCompactWidth));
  Result.Presentations.Define(NyxPresentation('short'),
    TNyxViewportCondition.Any.WidthBelow(CShortWidth).HeightBelow(CShortHeight));
  LRoot := TNyxNode.Create('page', 'studio-shell');
  Result.AddPage(LRoot);
  LRoot.Configure.ForPlatform(npfNativeLCL).Padding(0).Gap(0).Done;
  { Stable node IDs are command identities shared by platform controllers.
    Building chrome as ordinary nodes makes it inspectable and renderable by
    the same public adapters used for the applications Studio designs. }
  LHeader := TNyxNode.Create('row', 'studio-header');
  { The same portable flow policy used by applications owns Studio's toolbar.
    Actions wrap by their actual captions rather than native equal-width cells. }
  LHeader.Configure.Layout(TNyxLayoutPolicy.Row.Wrap(nfwWrap).Align(ncaCenter)).Done;
  LRoot.Add(LHeader);
  { Keep this Nyx compound mounted even while hidden. A failed target display
    can expose Retry through the surviving Chrome without recomposing a canvas. }
  LRoot.Add(NewNyxViewRecovery(NyxStudioDisplayRecoveryID, AState.DisplayRecovery));
  LHeader.Add(Caption('studio-logo', 'nyx'));
  LHeader.Add(Caption('studio-subtitle', 'STUDIO  /  ' + ASession.Document.Title));
  LHeader.Add(Button('action-undo', 'Undo'));
  LHeader.Add(Button('action-redo', 'Redo'));
  LHeader.Add(Button('action-code', 'Pascal'));
  LHeader.Add(Button('action-import', 'Open'));
  LHeader.Add(Button('action-save', 'Save'));
  LHeader.Add(Button('action-outputs', 'Outputs'));
  LHeader.Add(Button('action-agents', 'Agents'));
  LHeader.Add(Button('action-actions', 'Actions'));

  if AState.Agents.CanControlBuilds then
  begin
    LHeader.Add(Button('action-builds', 'Builds (' +
      IntToStr(AState.Agents.BuildJobs.Field('total').AsInteger) + ')'));
  end;
  LHeader.Add(Button('action-build-view', 'Build view').SetProp('variant', 'primary'));
  LHeader.Add(Button('action-build-app', 'Build app'));
  { Narrow chrome keeps ordinary actions in the document for shared routing,
    while public managed menus make the hidden actions reachable. This changes
    chrome allocation only: design text and application scale stay untouched. }
  for LIndex := 1 to LHeader.Count - 1 do
  begin
    LButton := LHeader.Children[LIndex];

    if (LButton.ID <> 'action-actions') and
      ((AState.CanvasExpanded) or ((AState.Compact) and
        (LButton.ID <> 'action-undo') and (LButton.ID <> 'action-redo'))) then
    begin
      LButton.Configure.Visible(False).Done;
    end;
  end;

  if AState.CanvasExpanded then
  begin
    LHeader.Add(Button('action-workspace-restore', 'Restore')
      .Configure.AccessibleName('Restore workspace controls').Done);
  end;

  if AState.Compact then
  begin
    LPanelbar := TNyxNode.Create('row', 'studio-panelbar');
    LPanelbar.Configure.Wrap(nfwNoWrap).Done;
    LRoot.Add(LPanelbar);
    LPanelbar.Add(Button('action-panel-project', 'Project').Configure.Flex(1).Done);
    LPanelbar.Add(Button('action-panel-design', 'Design').Configure.Flex(1).Done);
    LPanelbar.Add(Button('action-panel-inspector', 'Inspector').Configure.Flex(1).Done);
    case AState.Panel of
      nspProject:
        begin
          LPanelbar.Children[0].SetProp('variant', 'primary');
        end;
      nspDesign:
        begin
          LPanelbar.Children[1].SetProp('variant', 'primary');
        end;
      nspInspector:
        begin
          LPanelbar.Children[2].SetProp('variant', 'primary');
        end;
    end;
    LPanelbar.Configure.Visible(not AState.CanvasExpanded)
      .WhenPresentation(NyxPresentation('short')).Visible(False).Done;
  end;
  LWorkspace := TNyxNode.Create('row', 'studio-workspace');
  LWorkspace.Configure.ForPlatform(npfNativeLCL).Flex(1).Padding(0).Gap(0)
    .Layout(TNyxLayoutPolicy.Row.Wrap(nfwNoWrap).Align(ncaStretch)).Done;
  LRoot.Add(LWorkspace);
  { Independent public scroll views keep native palettes/inspectors inside the
    workspace, rather than making the complete application one tall scroll page.
    Browser chrome continues to consume these same identities and compositions. }
  LLeft := TNyxNode.Create(nkScroll, 'studio-left').Configure.Layout(nlColumn).Done;
  LLeft.Configure.ForPlatform(npfNativeLCL).Width(250).Padding(12).Gap(10).Done;

  if AState.Compact then
  begin
    LLeft.Configure.ForPlatform(npfNativeLCL).Clear(atWidth).Flex(1).Done;
  end;
  { Pages and definitions remain separate document roots. Palette buttons carry
    semantic add-kind metadata instead of retaining widget-specific callbacks. }
  LWorkspace.Add(LLeft);
  LLeft.Add(TNyxNode.Create('heading', 'views-title').SetProp('text', 'PROJECT'));
  LLeft.Add(TNyxNode.Create('input', 'project-title').SetProp('text', 'Project title')
    .SetProp('value', ASession.Document.Title));

  if AState.PendingDesign.TitleDefined then
  begin
    LLeft.Find('project-title').Configure.Value(AState.PendingDesign.Title).Done;
  end;

  if AState.FilesVisible then
  begin
    LViews := TNyxNode.Create(nkColumn, 'studio-project-files');
    LLeft.Add(LViews);
    LViews.Add(TNyxNode.Create(nkInput, 'project-file-name').Configure
      .Text('Saved project name').Placeholder('my-project').Value(AState.ProjectName).Done);
    LViews.Add(Caption('project-file-hint',
      'Save keeps the design, Pascal and any draft together on this computer.'));
    LViews.Add(Button('action-project-open', 'Open saved project').Configure
      .Enabled(not AState.ProjectBusy).Done);
    LViews.Add(Button('action-project-save', 'Save paired files').Configure
      .Enabled(not AState.ProjectBusy).Done);
    LViews.Add(Button('action-project-export', 'Download project backup'));
    LViews.Add(Button('action-project-import', 'Import project or paired files'));
    LViews.Add(Button('action-project-export-files', 'Download design + Pascal'));

    if AState.ProjectConflict then
    begin
      LViews.Add(Caption('project-conflict-warning',
        'The saved files changed. Your work is retained. Open the saved version ' +
        'after downloading your work, or change the name to save a separate copy.'));
      LViews.Add(Button('action-project-use-remote', 'Back up mine and open saved'));
      LViews.Add(Button('action-project-copy', 'Save mine as a new project'));
    end;

    if AState.ImportConflict then
    begin
      LViews.Add(Caption('project-import-warning',
        'The files disagree or Pascal is unsupported. Export the input before ' +
        'merging. Your current project is unchanged.'));
      LViews.Add(Button('action-project-input-backup', 'Download imported backup'));
      LViews.Add(Button('action-project-use-pascal', 'Open using Pascal values'));
      LViews.Add(Button('action-project-use-design', 'Open design; keep Pascal as draft'));
      LViews.Add(Button('action-project-cancel-import', 'Cancel import'));
    end;
  end;
  LViews := TNyxNode.Create('column', 'studio-views').SetProp('gap', '2');
  LLeft.Add(LViews);
  for LIndex := 0 to ASession.Document.Count - 1 do
  begin
    LName := ASession.Document.Pages[LIndex].ID;
    LButton := Button('view-page-' + IntToStr(LIndex), 'Page / ' + LName)
      .SetProp('view-id', LName);

    if LName = ASession.ActiveViewID then
    begin
      LButton.SetProp('variant', 'primary');
    end;
    LViews.Add(LButton);
  end;
  LViews.Add(Button('action-add-page', '+ New page'));
  for LIndex := 0 to ASession.Document.ComponentCount - 1 do
  begin
    LName := ASession.Document.Components[LIndex].ID;
    LViews.Add(Button('view-component-' + IntToStr(LIndex), 'Component / ' + LName)
      .SetProp('view-id', LName));
    LViews.Add(Button('instance-component-' + IntToStr(LIndex), '+ Use ' + LName)
      .SetProp('component-id', LName));
  end;
  LViews.Add(Button(NyxStudioReviewRootID, 'Remove active view').Configure
    .Enabled(ASession.ActiveViewID <> '').Done);
  AddStatePanel(LLeft, ASession, AState);
  LLeft.Add(NewNyxButton('action-theme-toggle').Configure.Text('Theme')
    .Hint('Edit the application palette and logical metrics.').Done);

  if AState.ThemeVisible then
  begin
    LLeft.Add(NewNyxThemeEditor('studio-theme-editor', ASession.Document));
  end;
  LLeft.Add(NewNyxButton('action-resources-toggle').WithText('Resources'));
  LLeft.Add(NewNyxResourceBrowser('studio-resource-picker', AState.ResourceBrowser, rbmCompact));

  if AState.ResourcesVisible then
  begin
    LResources := TNyxNode.Create(nkScroll, 'studio-resources').Configure
      .Layout(TNyxLayoutPolicy.Column).Padding(16).Gap(16).Flex(1).Done;
    LResources.Configure.ForPlatform(npfNativeLCL).Flex(1).Done;
    LWorkspace.Add(LResources);
    LResources.Add(NewNyxHeading('studio-resources-heading').WithText('Resources'));
    LResources.Add(NewNyxLabel('studio-resources-help').WithText(
      'Find project files by category, intent or tag. Changes stay proposals until you apply them.'));
    LResources.Add(NewNyxButton('action-resources-close').WithText('Back to design'));
    LResourceForm := NewNyxColumn('studio-resource-form');
    LResourceForm.Configure.Layout(TNyxLayoutPolicy.Column).Gap(16).Done;
    LSelectedProjection := ASession.SelectedProjection;
    try
      LResourceForm.Add(NewNyxResourceEditor('studio-resource-editor', ASession.Document.Resources,
        AState.ResourceSelection, ASession.Selected, LSelectedProjection, recExternal));
      LResourceForm.Add(NewNyxResourceRowsEditor('studio-resource-rows', ASession.Document.Resources,
        ASession.Document.Collections));
      AddResourceRuntimeViews(LResourceForm.Node, AState.Agents.ResourceRuntimes,
        AState.ResourceSelection, AState.Agents.CanInspectResourceRuntime);
    finally
      LSelectedProjection.Free;
    end;
    LResources.Add(NewNyxResourceWorkspace('studio-resource-workspace',
      NewNyxResourceBrowser('studio-resource-browser', AState.ResourceBrowser),
      LResourceForm, NyxPresentation('compact'), AState.ResourcePane));
  end;
  AddNyxCollectionDefaultsPanel(LLeft, ASession, AState.StateVisible, AState.PendingDesign);
  AddNyxStudioPalette(LLeft, ASession.Catalog, AState.Palette);
  LCenter := TNyxNode.Create('column', 'studio-center');
  LCenter.Configure.ForPlatform(npfNativeLCL).Flex(1).Padding(0).Gap(0).Done;
  { The authoring area is itself Nyx: a public nested-view surface and an
    optional public source editor. This avoids a second Studio-only widget API. }
  LWorkspace.Add(LCenter);
  LDetails := TNyxNode.Create(nkScroll, 'studio-details')
    .Configure.Layout(nlColumn).Gap(0).Padding(0).Done;
  { The owning center holds both groups, including collapsed detail controls.
    Hiding an unchosen group never admits a sync decision or loses its state. }
  LCenter.Add(LDetails);
  LStage := TNyxNode.Create(nkColumn, 'studio-stage')
    .Configure.Flex(1).Gap(0).Padding(0).Done;
  LCenter.Add(LStage);

  if AState.AgentsVisible then
  begin
    LDetails.Add(BuildNyxStudioAgents(AState.Agents));
  end;

  if AState.BuildsVisible and AState.Agents.CanControlBuilds then
  begin
    LDetails.Add(BuildNyxStudioBuildJobs(AState.Agents.BuildJobs, AState.BuildControlReady));
  end;

  if AState.OutputVisible then
  begin
    AddOutputPanel(LDetails, AState);
  end;

  if AState.RootRemoval.Kind = ndObject then
  begin
    LDetails.Add(BuildNyxRootRemovalCard(AState.RootRemoval));
  end;
  LHasDetails := LDetails.Count > 0;

  if LHasDetails and not AState.CanvasExpanded then
  begin
    LSummary := nil;

    if AState.Compact then
    begin
      LSummary := TNyxNode.Create(nkRow, 'studio-details-summary')
        .Configure.Layout(TNyxLayoutPolicy.Row.Wrap(nfwNoWrap).Align(ncaCenter))
        .Padding(6).Gap(8).Done;
      LSummary.Add(Caption('studio-details-label', 'Workspace details')
        .Configure.Flex(1).Done);

      if AState.Agents.Conflict and AState.AgentsVisible then
      begin
        LSummary.Children[0].Configure.Text('Shared project differs')
          .Hint('Your local project is retained. Review the choices before changing sync.').Done;
      end;
      LSummary.Add(Button('action-details-toggle', 'Review')
        .Configure.AccessibleName('Show workspace details').Done);
      LCenter.Insert(0, LSummary);
    end;

    if not AState.Compact or AState.DetailsExpanded then
    begin

      if LSummary <> nil then
      begin
        LSummary.Children[1].Configure.Text('Hide')
          .AccessibleName('Hide workspace details').Done;
      end;
      { One public allocation contract bounds aggregate details on both hosts.
        Desktop keeps them visible; compact hosts retain their collapse choice.
        A growing session/build list scrolls inside its pane instead of giving
        the design/source stage zero height. No percentage CSS depends on an
        intrinsically sized details ancestor. }
      LDetailSplit := TNyxNode.Create(nkSplitView, 'studio-details-split')
        .Configure.SplitOrientation(nsoStacked).SplitPosition(AState.DetailsPercent)
        .SplitMinimum(15).SplitMaximum(60).SplitResizable(True).Flex(1)
        .AccessibleName('Workspace details and design size').Done;
      { Extract transfers ownership; Remove would destroy the independently
        composed group before it can become the public split's first pane. }
      LCenter.Extract(Ord(AState.Compact));
      LCenter.Extract(Ord(AState.Compact));
      LDetailSplit.Add(LDetails);
      LDetailSplit.Add(LStage);
      LCenter.Add(LDetailSplit);
      LDetails.Configure.Flex(1).Done;
    end
    else
    begin
      LDetails.Configure.Visible(False).Done;
    end;
  end;

  if not LHasDetails or AState.CanvasExpanded then
  begin
    LDetails.Configure.Visible(False).Done;
  end;
  LViewbar := TNyxNode.Create('row', 'studio-viewbar');
  { The canvas row can be narrower than the application window. Explicit wrap
    therefore follows this row's allocated space on both adapters, including a
    desktop canvas between open side panels. It preserves each control face. }
  LViewbar.Configure.Layout(TNyxLayoutPolicy.Row.Wrap(nfwWrap).Align(ncaCenter)).Done;
  LStage.Add(LViewbar);
  { A flex caption must retain room for a useful view name when the Inspector
    narrows the canvas. Let the existing row wrap controls onto the next line. }
  LViewbar.Add(Caption('active-view-label', ASession.ActiveViewID)
    .Configure.MinimumWidth(64).Flex(1).Done);
  LViewbar.Add(Button('action-desktop', 'Desktop'));
  LViewbar.Add(Button('action-phone', 'Phone'));
  LViewbar.Add(Button('action-preview', 'Interact'));
  LViewbar.Add(Button('action-canvas-tools', 'Tools')
    .Configure.Visible(AState.Compact).Hint('Show placement and drag controls.').Done);
  LViewbar.Add(Button('action-canvas-expand', 'Expand')
    .Configure.AccessibleName('Expand design canvas').Done);

  if AState.Compact then
  begin
    LViewbar.Find('action-desktop').Configure.Visible(False).Done;
    LViewbar.Find('action-phone').Configure.Visible(False).Done;
  end;
  { Omit a chooser when no manual configurations exist. The actual target
    controller reconciles its copied choice against the current document. }

  if Pos(TNyxText(#10), NyxStudioPresentationItems(ASession.Document.Presentations)) > 0 then
  begin
    LViewbar.Add(NewNyxSelect(NyxStudioPresentationPreviewID).Configure
      .Text('Presentation').Width(216)
      .Items(NyxStudioPresentationItems(ASession.Document.Presentations))
      .Value(NyxStudioPresentationChoice(AState.PresentationSelection.Reconciled(
        ASession.Document.Presentations)))
      .AccessibleName('Preview presentation')
      .Hint('Switch a manual configuration without editing the design or its history.').Done);
  end;
  LViewbar.Add(TNyxNode.Create(nkSelect, NyxStudioDropPositionID)
    .Configure.Text('Drop position').Items(NyxStudioAutomaticPlacement + #10 +
      NyxPlacementName(nplInside) + #10 + NyxPlacementName(nplBefore) + #10 + NyxPlacementName(nplAfter))
    .Value(NyxStudioPlacementChoice(AState.DesignerPlacement, AState.DesignerAutomaticPlacement))
    .Width(144)
    .AccessibleName('Drop position')
    .Hint('Automatic uses row/column edge zones. Inside, before and after remain explicit choices.')
    .Visible(not AState.Compact or AState.CanvasToolsVisible)
    .WhenPresentation(NyxPresentation('compact')).Text('').Width(128).Done);
  { Keep this ordinary Nyx drag source beside the placement selector. Compact
    Studio hides the Inspector while designing, so a source there cannot be
    dragged onto its canvas. Authored inputs remain ordinary editable inputs. }
  LSelected := ASession.Selected;
  LCanDragSelection := (LSelected <> nil) and (LSelected.Parent <> nil) and
    (LSelected.Kind <> 'slot-override');
  { Selection changes capability, not toolbar ownership. Keep this declared
    source in the view tree even when a root cannot move; ordinary typed
    visibility/enabled refresh preserves the surrounding header and its inputs.
    Drag start still validates the captured selection independently. }
  LViewbar.Add(Button(NyxStudioDragMoveID, 'Drag selected')
    .Configure.DragSource(True).AccessibleName('Drag selected control')
    .Enabled(LCanDragSelection)
    .Visible(LCanDragSelection and (not AState.Compact or AState.CanvasToolsVisible))
    .Hint('Drag onto the canvas using the selected drop position.').Done
    .SetProp(NyxStudioDragSelectionKey, 'true'));

  if AState.CompiledPreviewAvailable then
  begin
    LViewbar.Add(Button('action-compiled-run', 'Run compiled preview'));
  end;

  if AState.CompiledPreviewRunning then
  begin
    LViewbar.Add(Button('action-compiled-stop', 'Stop preview'));
  end;
  LViewbar.Add(Caption('output-summary', 'Output: ' + AState.OutputTarget));

  if AState.OutputTarget = '' then
  begin
    LViewbar.Children[LViewbar.Count - 1].SetProp('text', 'Output: choose anytime');
  end;
  LViewbar.Find('output-summary').Configure
    .Visible(not AState.Compact or AState.CanvasToolsVisible).Done;
  LViewbar.Configure.Visible(not AState.CanvasExpanded).Done;
  LCanvas := TNyxNode.Create('column', 'studio-canvas-wrap');
  LCanvas.Configure.ForPlatform(npfNativeLCL).Flex(1).Padding(0).Gap(0).Done;
  { Explicit pane minima reserve an editable workspace when Outputs or Agents
    share the center. Both split adapters consume this ordinary typed contract;
    the divider still adjusts in place and tiny hosts compress boundedly. }
  LCanvas.Configure.MinimumHeight(96).Done;

  if AState.CodeVisible and not AState.CanvasExpanded then
  begin
    LSplit := TNyxNode.Create(nkSplitView, 'studio-split')
      .Configure.SplitOrientation(nsoStacked).SplitPosition(AState.CanvasPercent)
      .SplitMinimum(10).SplitMaximum(90).SplitResizable(True)
      .Flex(1).Done;
    LStage.Add(LSplit);
    LSplit.Add(LCanvas);
  end
  else
  begin
    LStage.Add(LCanvas);
  end;
  LField := TNyxNode.Create('design-surface', 'studio-canvas')
    .SetProp('aria-label', 'Visual design canvas');
  LField.Configure.ForPlatform(npfNativeLCL).Flex(1).Padding(0).Gap(0).Done;
  LCanvas.Add(LField);

  if AState.Phone then
  begin
    LField.SetProp('width', '390');
  end;

  if AState.CodeVisible and not AState.CanvasExpanded then
  begin
    if AState.CodePresentation = ncpPaneHosted then
    begin
      LCodePane := TNyxNode.Create(nkPanel, 'studio-source-mount')
        .Configure.Layout(nlColumn).Gap(0).Padding(0).Flex(1).Done;
    end
    else
    begin
      LCodePane := BuildNyxStudioSourcePane(ASession, AState, AReport);
    end;
    LCodePane.Configure.MinimumHeight(280).Done;
    LSplit.Add(LCodePane);
  end;
  LRight := TNyxNode.Create(nkScroll, 'studio-right').Configure.Layout(nlColumn).Done;
  LRight.Configure.ForPlatform(npfNativeLCL).Width(290).Padding(12).Gap(10).Done;

  if AState.Compact then
  begin
    LRight.Configure.ForPlatform(npfNativeLCL).Clear(atWidth).Flex(1).Done;
  end;
  { Inspector fields describe model properties. A controller sends their changes
    through the session's undoable command boundary, then rebuilds this view. }
  LWorkspace.Add(LRight);
  LRight.Add(TNyxNode.Create('heading', 'inspector-title').SetProp('text', 'INSPECTOR'));
  LSelected := ASession.Selected;

  if LSelected <> nil then
  begin
    LRight.Add(Caption('selected-label', LSelected.Kind + ' / ' + LSelected.ID));

    if (LSelected.Parent <> nil) and (LSelected.Kind <> 'slot-override') then
    begin
      LRight.Add(BuildNyxStudioResizeTools(NyxControl(LSelected.ID)));
      LMoveTools := BuildNyxStudioMoveTools(ASession.Document, NyxControl(LSelected.ID));

      if LMoveTools <> nil then
      begin
        LRight.Add(LMoveTools);
      end;
    end;
    { Placement stays beside selection, ahead of potentially long property/event
      lists. Ordinary canvas/hierarchy selection supplies the destination; the
      project session owns only copied pending identity and accepted-pair data. }

    if ASession.PlacementSource.ID <> '' then
    begin
      LRight.Add(Caption('placement-source', 'Moving ' + ASession.PlacementSource.ID));
      LRight.Add(Caption('placement-help',
        'Select a destination on the canvas or in the hierarchy, then choose its position.'));
      LPlacementActions := TNyxNode.Create(nkRow, 'placement-actions')
        .Configure.Wrap(nfwWrap).Gap(6).Done;
      LRight.Add(LPlacementActions);
      LPlacementActions.Add(Button('action-place-inside', 'Place inside'));
      LPlacementActions.Add(Button('action-place-before', 'Place before'));
      LPlacementActions.Add(Button('action-place-after', 'Place after'));
      LPlacementActions.Add(Button('action-place-cancel', 'Cancel move'));
    end
    else if (LSelected.Parent <> nil) and (LSelected.Kind <> 'slot-override') then
    begin
      LRight.Add(Button('action-place-start', 'Move to another layout'));
    end;
    LMetadataSource := NyxProjectionSource(LSelected, ASession.Document);
    { Show the creator's explanation where the control is being edited, including
      the Events tab. A registered recipe keeps its own intent rather than the
      description of its layout root. Unregistered projections may use their
      primitive's help; no guessed description is saved into the user's design. }
    LHelpIndex := ASession.Catalog.IndexOf(LMetadataSource.Kind);

    if LHelpIndex < 0 then
    begin
      LHelpIndex := ASession.Catalog.IndexOf(LMetadataSource.ProjectionKind);
    end;

    if (LHelpIndex >= 0) and
      (ASession.Catalog[LHelpIndex].Discovery.Description <> '') then
    begin
      LRight.Add(Caption('selected-component-help',
        ASession.Catalog[LHelpIndex].Discovery.Description));
      LRight.Add(Button(NyxStudioComponentHelpID, 'About this component'));
    end;
    LInspectorTabs := TNyxNode.Create(nkRow, 'inspector-tabs');
    LInspectorTabs.Configure.Gap(6).Done;
    LRight.Add(LInspectorTabs);
    LButton := Button(NyxInspectorPropertiesID, 'Properties');

    if AState.InspectorTab = nitProperties then
    begin
      LButton.Configure.Variant(nvPrimary).Done;
    end;
    LInspectorTabs.Add(LButton);
    LButton := Button(NyxInspectorEventsID, 'Events');

    if AState.InspectorTab = nitEvents then
    begin
      LButton.Configure.Variant(nvPrimary).Done;
    end;
    LInspectorTabs.Add(LButton);
    { Relevant typed fields replace the generic list formerly shown for every
      kind. Read metadata without inserting defaults into the user's document. }

    if FindNyxPrimitive(LMetadataSource.ProjectionKind, LPrimitive) then
    begin
      LRight.Add(Caption('selected-capabilities', 'Browser: ' +
        NyxCapabilityText(LPrimitive.Browser) + ' / LCL: ' + NyxCapabilityText(LPrimitive.Native)));
    end;

    if LSelected.ProjectionKind = 'component' then
    begin
      LRight.Add(Caption('instance-parts-title', 'INSTANCE PARTS'));
      LRight.Add(Button('customize-part-root', 'Customize content').SetProp('override-path', '.'));
      LPartView := RealizeNyxView(ASession.Document, LSelected);
      try
        LPartCount := 0;
        AddPartChoices(LRight, LPartView, '', LPartCount);
      finally
        LPartView.Free;
      end;
    end;

    if LSelected.Kind = 'slot-override' then
    begin
      LRight.Add(Caption('instance-part-help',
        'Edit this instance part. For layout parts, add content through the palette.'));
    end;
    LSelectedProjection := ASession.SelectedProjection;
    try

      if AState.InspectorTab = nitEvents then
      begin

        if LSelectedProjection <> nil then
        begin
          AddNyxEventsInspector(LRight, ASession, LSelectedProjection,
            AState.CallbackRemoval, AState.PendingDesign);
        end;
      end
      else
      begin

        if LSelected.ProjectionKind = 'component' then
        begin
          AddNyxContentInspector(LRight, ASession);
        end;
        AddBindingsPanel(LRight, ASession, LSelectedProjection, AState);
        AddNyxDateDomainInspector(LRight, ASession);
        AddNyxTimeDomainInspector(LRight, ASession);

        if (LSelectedProjection <> nil) and
          (LSelectedProjection.ProjectionKind = NyxKindName(nkImage)) then
        begin
          LRight.Add(NewNyxImageEditor('inspector-image', LSelected, LSelectedProjection));
        end;
        AddNyxMenuBarInspector(LRight, ASession, LSelectedProjection);
        AddNyxMenuInspector(LRight, ASession, AState.MenuEditorReference);

        if AState.BindingsVisible then
        begin
          AddNyxCollectionBindingPanel(LRight, ASession, LSelectedProjection, AState.PendingDesign);
        end;
        LProperties := NyxProperties(LSelected, ASession.Document);
        AddNyxViewportInspector(LRight, LSelected.ID, ASession.Document);
        for LIndex := 0 to Length(LProperties) - 1 do
        begin

          if (LSelectedProjection <> nil) and
            (LSelectedProjection.ProjectionKind = NyxKindName(nkImage)) and
            TryNyxAttribute(LProperties[LIndex].Key, LAttribute) and (LAttribute in
              [atSource, atAlt, atImageFit, atImageHorizontal, atImageVertical]) and
            ((LAttribute <> atSource) or not AState.AdvancedProperties) then
          begin
            { Default authoring uses the grouped typed form. Advanced properties
              retain the explicit existing source-reference/wire boundary. }
            Continue;
          end;

          if LProperties[LIndex].Advanced and not AState.AdvancedProperties and
            not LSelected.TryPresentationRule(LProperties[LIndex].Key, LViewport, LPlatform, LAttribute) then
          begin
            Continue;
          end;
          LKind := 'input';
          case LProperties[LIndex].ValueType of
            npText, npNumber, npReference:
              begin
                LKind := 'input';
              end;
            npLines:
              begin
                LKind := 'memo';
              end;
            npBoolean, npChoice:
              begin
                LKind := 'select';
              end;
            npInteger:
              begin
                LKind := 'spin';

                if (LProperties[LIndex].Minimum < -1000000) or
                  (LProperties[LIndex].Maximum > 1000000) then
                begin
                  LKind := 'input';
                end;
              end;
          end;
          LFieldID := LProperties[LIndex].Key;

          if TryNyxPresentationKey(LProperties[LIndex].Key, LPresentation, LPlatform, LAttribute) then
          begin
            { Exact application names can exceed a control ID once the wire
              namespace is added. Chrome identity is bounded independently;
              prop-key retains the complete exact authored scope. }
            LFieldID := TNyxText('presentation-property-') + TNyxText(IntToStr(LIndex));
          end;
          LField := TNyxNode.Create(LKind, TNyxText('inspector-') + LFieldID)
            .SetProp('text', LProperties[LIndex].Title)
            .SetProp('prop-key', LProperties[LIndex].Key)
            .SetProp('value', LSelected.Prop(LProperties[LIndex].Key,
              LProperties[LIndex].DefaultValue));
          { Admit the field before later configuration can refuse. A failed shell
            construction must release it with its owning document. }
          LRight.Add(LField);

          if AState.PendingDesign.PropertyValue(LSelected.ID,
            LProperties[LIndex].Key, LPendingValue) then
          begin
            { This is an inspector wire proposal, not an accepted application
              value. The integer field's published range is installed below;
              validating against the primitive's temporary default 0..100 would
              reject a legitimate pending dimension such as 170 prematurely.
              Candidate/renderer admission remains responsible for typed values. }
            LField.SetProp('value', LPendingValue);
          end;
          LField.Configure.Hint(LProperties[LIndex].Support.Description + #10 +
            'Browser: ' + NyxCapabilityText(LProperties[LIndex].Support.Browser) +
            ' / LCL: ' + NyxCapabilityText(LProperties[LIndex].Support.Native)).Done;

          if LProperties[LIndex].ValueType = npBoolean then
          begin
            LField.SetProp('items', 'true' + #10 + 'false');
          end
          else if LProperties[LIndex].ValueType = npChoice then
          begin
            LField.SetProp('items', #10 + LProperties[LIndex].Choices);
          end
          else if LProperties[LIndex].ValueType = npNumber then
          begin
            LField.Configure.InputType(niNumber).Done;
          end
          else if LProperties[LIndex].ValueType = npInteger then
          begin

            if LKind = 'input' then
            begin
              { Logical signed-32-bit values can exceed the primitive spin
                projection's bounds. Keep a numeric draft input with an exact
                integer contract instead of silently narrowing its range. }
              LField.Configure.InputType(niNumber).Done;
              LField.Contract.Value(NyxIntegerDomain.Range(LProperties[LIndex].Minimum,
                LProperties[LIndex].Maximum));
            end
            else
            begin
              LField.SetProp('min', IntToStr(LProperties[LIndex].Minimum))
                .SetProp('max', IntToStr(LProperties[LIndex].Maximum));
            end;
          end;
          { Bound properties show the effective default. Editing their raw fallback
            would have no visible result, so direct the user to State/Bindings. }

          if (LSelectedProjection <> nil) and
            TryNyxBindingProperty(LProperties[LIndex].Key, LBindingTarget) and
            LSelectedProjection.FindBinding(LBindingTarget, LBinding) then
          begin
            { This inspector holds wire drafts. The admitted model remains strongly
              typed; initial draft text is deliberately copied at this UI boundary. }
            LField.SetProp('value', LSelectedProjection.Prop(LProperties[LIndex].Key,
              LProperties[LIndex].DefaultValue)).Configure.Enabled(False)
              .Hint('Bound to ' + LBinding.StateName + '; edit State or Bindings.').Done;
          end;

          if TryNyxAttribute(LProperties[LIndex].Key, LAttribute) or
            TryNyxPlatformKey(LProperties[LIndex].Key, LPlatform, LAttribute) or
            LSelected.TryPresentationRule(LProperties[LIndex].Key, LViewport, LPlatform, LAttribute) then
          begin

            if LAttribute in [atMinimumWidth, atMaximumWidth,
              atMinimumHeight, atMaximumHeight] then
            begin
              { Zero is a real limit. Reset submits the same optional-property
                command with empty wire data; a spin's displayed zero alone
                cannot communicate or restore absence on both targets. }
              LRight.Add(Button('inspector-unset-' + LFieldID,
                'Unset ' + LowerCase(LProperties[LIndex].Title))
                .Configure.Hint('Remove this size limit. Zero remains an explicit limit.').Done
                .SetProp(NyxStudioPropertyClearKey, LProperties[LIndex].Key)
                .SetProp(NyxStudioPropertyOwnerKey, LSelected.ID));
            end;
          end;

          if (LProperties[LIndex].Support.Browser = ncMissing) or
            (LProperties[LIndex].Support.Native = ncMissing) then
          begin
            { A missing target effect changes an output/customization decision.
              Keep it visible for touch users; ordinary help remains a hint. }
            LRight.Add(Caption('property-support-' + IntToStr(LIndex),
              LProperties[LIndex].Support.Description));
          end;
        end;
      end;
    finally
      LSelectedProjection.Free;
    end;

    if AState.InspectorTab = nitProperties then
    begin
      LRight.Add(Button('action-advanced-properties', 'More properties'));
    end;

    if AState.AdvancedProperties and (AState.InspectorTab = nitProperties) then
    begin
      { Preserve the open string contract. Extra/recipe metadata remains editable
        without misrepresenting it as a built-in typed property or target feature. }
      for LIndex := 0 to LSelected.Props.Count - 1 do
      begin
        LName := LSelected.Props.Names[LIndex];
        LKnown := False;
        for LPropertyIndex := 0 to Length(LProperties) - 1 do
        begin

          if LProperties[LPropertyIndex].Key = LName then
          begin
            LKnown := True;
            Break;
          end;
        end;

        if not LKnown then
        begin
          LKind := 'input';

          if Pos(#10, LSelected.Prop(LName)) > 0 then
          begin
            LKind := 'memo';
          end;
          LRight.Add(TNyxNode.Create(LKind, 'inspector-extra-' + IntToStr(LIndex))
            .SetProp('text', LName).SetProp('prop-key', LName)
            .SetProp('value', LSelected.Prop(LName)));
        end;
      end;
    end;
  end;
  LRight.Add(Button('action-duplicate', 'Duplicate'));
  LRight.Add(Button('action-up', 'Move up'));
  LRight.Add(Button('action-down', 'Move down'));
  LRight.Add(Button('action-component', 'Make reusable'));
  LRight.Add(Button('action-delete', 'Delete'));
  LRight.Add(Button('action-export-source', 'Export Pascal'));
  LRight.Add(TNyxNode.Create('heading', 'hierarchy-title').SetProp('text', 'HIERARCHY'));
  LRight.Add(BuildNyxStudioHierarchy(Result, ASession));

  if AState.Log <> '' then
  begin
    LRight.Add(TNyxNode.Create('code', 'studio-log').SetProp('text', AState.Log));
  end;
  LFooter := TNyxNode.Create('row', 'studio-footer');
  LRoot.Add(LFooter);
  LFooter.Add(Caption('studio-status', AState.Status));
  LFooter.Children[0].Configure.Hint(AState.Status).Done;
  LFooter.Configure.Visible(not AState.CanvasExpanded).Done;
  { The resources section owns its broad host. Keep already mounted design
    contexts hidden on desktop so ordinary binding consumers and the optional
    source workspace can retain their independent lifetimes. Compact navigation
    still omits inactive panels through the existing policy below. }
  LLeft.Configure.Visible(not AState.ResourcesVisible).Done;
  LCenter.Configure.Visible(not AState.ResourcesVisible).Done;
  LRight.Configure.Visible(not AState.ResourcesVisible).Done;
  { Each compact panel remains the same public Nyx composition as its desktop
    counterpart. Omit inactive roots so both adapters give the active panel its
    full host width rather than reserving space for invisible siblings. }

  if AState.Compact or AState.CanvasExpanded then
  begin

    if (AState.Panel <> nspProject) or AState.CanvasExpanded then
    begin
      LWorkspace.Remove(LLeft);
    end;

    if (AState.Panel <> nspDesign) and not AState.CanvasExpanded then
    begin
      LWorkspace.Remove(LCenter);
    end;

    if (AState.Panel <> nspInspector) or AState.CanvasExpanded then
    begin
      LWorkspace.Remove(LRight);
    end;
  end;
end;

function BuildNyxStudioView(ASession: TNyxStudioSession;
  const AState: TNyxStudioViewState; const AReport: INyxCompilerReport): TNyxDocument;
begin

  if ASession = nil then
  begin
    raise ENyxModel.Create('Studio session is required');
  end;
  Result := TNyxDocument.Create;
  try
    PopulateNyxStudioView(ASession, AState, AReport, Result);
  except
    Result.Free;
    raise;
  end;
end;

end.
