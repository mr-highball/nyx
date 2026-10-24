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
unit nyx.studio.collections;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text,
  nyx.types,
  nyx.behavior,
  nyx.model,
  nyx.collections.query, nyx.collections.arrangement,
  nyx.projection.refresh,
  nyx.studio.session,
  nyx.studio.collectionintent;

type
  { Stable logical roles for the ordinary query-policy form. Column -1 names
    the table; nonnegative columns are transport positions whose copied field
    and scalar family are independently rechecked during capture. }
  TNyxStudioQueryPolicyField = (sqpfEnabled, sqpfFiltering, sqpfSorting,
    sqpfSortMode, sqpfTextComparison, sqpfFamilyOperators);
  { Scalar roles of the independent authored-layout form. The column index in
    a chrome ID is transport only; capture verifies the copied typed field and
    complete schema/binding baseline before admitting a source command. }
  TNyxStudioLayoutPolicyField = (slpfEnabled, slpfVisible, slpfPin,
    slpfMinimumCenter, slpfAutoSizeRows);
  { Stable scalar roles inside the existing layout/display draft subtree. Form
    positions never replace the independently verified full typed field. }
  TNyxStudioFormatPolicyField = (sfpfStyle, sfpfName, sfpfRevision, sfpfLocale,
    sfpfDecimal, sfpfGroup, sfpfScale, sfpfRounding, sfpfDateOrder,
    sfpfDateSeparator, sfpfTimePrecision, sfpfFirst, sfpfSecond);
  { Closed roles inside the retained table-defaults draft owner. These are
    copied authored choices, independently of host clipboard permissions,
    runtime selection, user preferences or unfinished accepted-data edits. }
  TNyxStudioWorkbenchPolicyField = (swpfRangeSelection, swpfCopyCell,
    swpfCopySelectedSource, swpfCopySelectedPage, swpfCopyCellRange,
    swpfCopyFields, swpfHeaders, swpfLineEnding, swpfFocusedDraft);
  { Presentation form roles remain typed while incomplete line-count input stays
    an inert draft. Column -1 addresses the independent body-row spacing form. }
  TNyxStudioPresentationPolicyField = (sppfDensity, sppfTextOverflow, sppfMaximumLines);
  { Independent optional logical axes in the existing layout draft owner.
    Transport column positions never replace the captured full typed field. }
  TNyxStudioColumnAlignmentField = (scafHorizontal, scafVertical);
  { A copied presentation receipt, never design data/history. Exact command
    context prevents a later project, selection or revision from inheriting its
    report. Rows/controls/stores are not retained. Last preview describes its
    captured draft, rather than claiming to follow further form edits live. }
  TNyxStudioQueryPreview = record
    Defined: Boolean;
    Context: TNyxStudioCommandContext;
    OwnerID: TNyxText;
    ViewID: TNyxText;
    Text: TNyxText;
  end;
  { Opaque, value-only chrome identity at the transport boundary. The six
    metadata values distinguish collection/row/column ownership when positional
    widget IDs are reused. This is presentation identity, not authoring intent;
    CaptureNyxStudioCollection decodes its behavioral choices into typed values. }
  TNyxStudioCollectionChromeIdentity = record
  private
    FDefined: Boolean;
    FCommand: TNyxText;
    FKey: TNyxText;
    FField: TNyxText;
    FRow: TNyxText;
    FOwner: TNyxText;
    FKind: TNyxText;
  public
    { Borrow only while copying; no accepted node or physical input is kept. }
    class function FromNode(ANode: TNyxNode): TNyxStudioCollectionChromeIdentity; static;
    { Undefined identities never match. A defined identity requires all copied
      values, so a reused positional widget ID cannot inherit another field's focus. }
    function Matches(ANode: TNyxNode): Boolean;
    property Defined: Boolean read FDefined;
  end;

{ Nyx-built collection authoring. Values cross normal control events, then the
  router decodes closed choices and invokes detached, undoable session commands.
  No target handles, callbacks or mutable stores are retained by this UI. }
procedure AddNyxCollectionDefaultsPanel(AParent: TNyxNode;
  ASession: TNyxStudioSession; AVisible: Boolean); overload;
procedure AddNyxCollectionDefaultsPanel(AParent: TNyxNode;
  ASession: TNyxStudioSession; AVisible: Boolean;
  const APending: TNyxStudioPendingDesign); overload;
procedure AddNyxCollectionBindingPanel(AParent: TNyxNode;
  ASession: TNyxStudioSession; AProjection: TNyxNode); overload;
procedure AddNyxCollectionBindingPanel(AParent: TNyxNode;
  ASession: TNyxStudioSession; AProjection: TNyxNode;
  const APending: TNyxStudioPendingDesign); overload;
{ Exact independent draft owners used by the stock collection inspector.
  Only their accepted command baselines may change during retained publication;
  field identities, scalar domains, query structure and every other extension
  keep ordinary strict admission. A changed baseline resets that entire form,
  while unrelated property and peer-form drafts remain independent. The root
  is borrowed only during lookup; returned contexts retain copied IDs/keys. }
function NyxStudioCollectionFormContexts(AShellRoot: TNyxNode): TNyxProjectionFormContexts;
{ Copied control identities for physical hosts and public authoring consumers.
  They do not authorize a command or identify a column independently of its
  admitted form's owner/schema/full-binding baseline. }
function NyxStudioQueryPolicyFormID(AColumn: Integer): TNyxText;
function NyxStudioQueryPolicyFieldID(AColumn: Integer;
  AField: TNyxStudioQueryPolicyField): TNyxText;
function NyxStudioQueryPolicyOperatorID(AColumn: Integer;
  AComparison: TNyxQueryComparison): TNyxText;
function NyxStudioQueryPolicyApplyID(AColumn: Integer): TNyxText;
function NyxStudioQueryPolicyDefaultID(AColumn: Integer): TNyxText;

const
  NyxStudioQueryEditorID = 'inspector-collection-query';
  NyxStudioLayoutEditorID = 'inspector-collection-layout';

{ Stable source IDs locate ordinary inspector parts. Metadata independently
  qualifies their exact owner, typed field and accepted schema/binding. }
function NyxStudioLayoutPolicyFormID(AColumn: Integer): TNyxText;
function NyxStudioLayoutPolicyFieldID(AColumn: Integer;
  AField: TNyxStudioLayoutPolicyField): TNyxText;
function NyxStudioLayoutPolicyFeatureID(AColumn: Integer;
  AFeature: TNyxColumnLayoutFeature): TNyxText;
function NyxStudioLayoutPolicyApplyID(AColumn: Integer): TNyxText;
function NyxStudioLayoutPolicyDefaultID(AColumn: Integer): TNyxText;
{ Capture one complete authored layout policy, never runtime preferences. The
  mounted root is borrowed only during this call; stale/locked forms refuse. }
function CaptureNyxStudioLayout(ASession: TNyxStudioSession;
  AButton, AShellRoot: TNyxNode; const APending: TNyxStudioPendingDesign;
  out AEdit: TNyxStudioDesignEdit): Boolean;
{ Display forms share the exact layout-card draft/baseline owner. Apply is one
  typed source/history intent; malformed numeric text remains an inert draft. }
function NyxStudioFormatPolicyFormID(AColumn: Integer): TNyxText;
function NyxStudioFormatPolicyFieldID(AColumn: Integer;
  AField: TNyxStudioFormatPolicyField): TNyxText;
function NyxStudioFormatPolicyApplyID(AColumn: Integer): TNyxText;
function NyxStudioFormatPolicyDefaultID(AColumn: Integer): TNyxText;
function CaptureNyxStudioFormat(ASession: TNyxStudioSession;
  AButton, AShellRoot: TNyxNode; const APending: TNyxStudioPendingDesign;
  out AEdit: TNyxStudioDesignEdit): Boolean;
{ The complete workbench form shares the existing table-defaults draft owner.
  Only explicit Apply/reset submits one baseline-qualified, undoable command. }
function NyxStudioWorkbenchPolicyFormID: TNyxText;
function NyxStudioWorkbenchPolicyFieldID(AField: TNyxStudioWorkbenchPolicyField): TNyxText;
function NyxStudioWorkbenchPolicyApplyID: TNyxText;
function NyxStudioWorkbenchPolicyDefaultID: TNyxText;
function CaptureNyxStudioWorkbench(ASession: TNyxStudioSession;
  AButton, AShellRoot: TNyxNode; const APending: TNyxStudioPendingDesign;
  out AEdit: TNyxStudioDesignEdit): Boolean;
{ Authored spacing/text forms share the layout draft owner. IDs are transport;
  capture independently checks owner, full typed field and current baseline. }
function NyxStudioPresentationPolicyFormID(AColumn: Integer): TNyxText;
function NyxStudioPresentationPolicyFieldID(AColumn: Integer;
  AField: TNyxStudioPresentationPolicyField): TNyxText;
function NyxStudioPresentationPolicyApplyID(AColumn: Integer): TNyxText;
function NyxStudioPresentationPolicyDefaultID(AColumn: Integer): TNyxText;
function CaptureNyxStudioPresentation(ASession: TNyxStudioSession;
  AButton, AShellRoot: TNyxNode; const APending: TNyxStudioPendingDesign;
  out AEdit: TNyxStudioDesignEdit): Boolean;
{ Ordinary Nyx alignment forms retain unfinished choices until explicit Apply.
  Default reset changes no spacing, overflow, formatter, source data or peers. }
function NyxStudioColumnAlignmentFormID(AColumn: Integer): TNyxText;
function NyxStudioColumnAlignmentFieldID(AColumn: Integer;
  AField: TNyxStudioColumnAlignmentField): TNyxText;
function NyxStudioColumnAlignmentApplyID(AColumn: Integer): TNyxText;
function NyxStudioColumnAlignmentDefaultID(AColumn: Integer): TNyxText;
function CaptureNyxStudioColumnAlignment(ASession: TNyxStudioSession;
  AButton, AShellRoot: TNyxNode; const APending: TNyxStudioPendingDesign;
  out AEdit: TNyxStudioDesignEdit): Boolean;
{ Capture owns no accepted node/store and performs no source generation.
  Partial scalar notation stays text until the isolated session parses it.
  False means this is not the command's supported event. Invalid closed choices,
  stale mounted identities or pending structural locks raise before enqueueing. }
function CaptureNyxStudioCollection(ASession: TNyxStudioSession;
  ANode: TNyxNode; AEvent: TNyxTrigger; const APending: TNyxStudioPendingDesign;
  out AEdit: TNyxStudioDesignEdit): Boolean;
{ Public query compound capture retains its exact owner/schema/binding. A pending
  structural operation refuses before enqueue; the worker rechecks the baseline. }
function CaptureNyxStudioQuery(ASession: TNyxStudioSession;
  AButton, AShellRoot: TNyxNode; const APending: TNyxStudioPendingDesign;
  out AEdit: TNyxStudioDesignEdit): Boolean;
{ Read-only candidate exploration. False denotes another action. Exact mounted
  editor/binding/schema/pending guards precede source evaluation; accepted design,
  Pascal, pending draft/base and history stay unchanged, including on failure. }
function CaptureNyxStudioQueryPreview(ASession: TNyxStudioSession;
  AButton, AShellRoot: TNyxNode; const APending: TNyxStudioPendingDesign;
  out APreview: TNyxStudioQueryPreview): Boolean;
{ Resolve compound retargeting through the current source's exact descendant
  OriginID. Consumed defaults/absent sources cannot preview another subtree. }
function RouteNyxStudioQueryPreview(ASession: TNyxStudioSession;
  const AEvent: TNyxEventInfo; AShellRoot: TNyxNode;
  const APending: TNyxStudioPendingDesign; out APreview: TNyxStudioQueryPreview): Boolean;
{ Apply only to this context's newly composed shell. Stale copied reports are
  retired rather than rendered under another selected control/project. }
procedure RestoreNyxStudioQueryPreview(var APreview: TNyxStudioQueryPreview;
  ASession: TNyxStudioSession; AShellRoot: TNyxNode);
{ Pure expansion intent: exact current source/origin and receipt context only.
  No document or source/history mutation. A stale/absent receipt cannot open. }
function RouteNyxStudioQueryPreviewExpansion(ASession: TNyxStudioSession;
  const AEvent: TNyxEventInfo; AShellRoot: TNyxNode;
  const APreview: TNyxStudioQueryPreview): Boolean;

const
  { Explicit shared chrome metadata; adapters use the complete logical tuple
    when restoring focus across positional collection/row/column IDs. }
  NyxStudioCollectionCommandKey = 'collection-command';
  NyxStudioCollectionKey = 'collection-key';
  NyxStudioCollectionFieldKey = 'collection-field';
  NyxStudioCollectionRowKey = 'collection-row';
  NyxStudioCollectionOwnerKey = 'collection-owner';
  NyxStudioCollectionKindKey = 'collection-kind';

{ Compatibility consumer for synchronous/embedded authoring. Studio's adapters
  consume Capture through their independent source queue before this router.
  A successful route uses the same typed replay and paired session boundary. }
function RouteNyxStudioCollection(ASession: TNyxStudioSession;
  ANode: TNyxNode; AEvent: TNyxTrigger): Boolean;

implementation

uses
  nyx.data,
  SysUtils,
  nyx.state,
  nyx.contract,
  nyx.formatting,
  nyx.collections.presentation,
  nyx.collections.alignment,
  nyx.resources,
  nyx.times,
  nyx.numbers.decimal,
  nyx.collections,
  nyx.resources.rows,
  nyx.controls,
  nyx.collections.view.types,
  nyx.collections.columns,
  nyx.collections.paging,
  nyx.collections.selection,
  nyx.collections.query.editor,
  nyx.collections.query.policy,
  nyx.collections.workbench.policy,
  nyx.collections.query.preview,
  nyx.collections.bindings,
  nyx.menu.editor,
  nyx.studio.authoring;

type
  TCollectionCommand = TNyxStudioCollectionAction;

const
  CCommandKey = 'collection-command';
  CKey = 'collection-key';
  CField = 'collection-field';
  CRow = 'collection-row';
  COwner = 'collection-owner';
  CKind = 'collection-kind';
  CCommands: array[TCollectionCommand] of TNyxText =
    ('create', 'remove', 'add-field', 'default', 'add-row', 'remove-row',
    'cell', 'bind', 'scope', 'title', 'mode', 'parent', 'remove-column',
    'add-column', 'clear', 'inherit', 'selection', 'query',
    'column-content', 'column-fixed', 'column-flex', 'column-minimum',
    'column-maximum', 'column-default', 'page-size', 'initial-page', 'default-paging',
    'table-query', 'column-query', 'default-table-query', 'default-column-query',
    'table-layout', 'column-layout', 'default-table-layout', 'default-column-layout',
    'column-format', 'default-column-format', 'table-workbench', 'default-table-workbench',
     'table-presentation', 'default-table-presentation', 'column-text', 'default-column-text',
     'column-alignment', 'default-column-alignment');
  CQueryEditor = NyxStudioQueryEditorID;
  CPolicyColumn = 'studio-query-policy-column';
  CPolicyAction = 'studio-query-policy-action';
  CPolicyBaseline = 'studio-query-policy-baseline';
  CPolicyApply = 'apply';
  CPolicyDefault = 'default';
  CLayoutColumn = 'studio-layout-policy-column';
  CLayoutAction = 'studio-layout-policy-action';
  CLayoutFields: array[TNyxStudioLayoutPolicyField] of TNyxText =
    ('enabled', 'visible', 'pin', 'minimum-center', 'auto-size-rows');
  CLayoutFeatures: array[TNyxColumnLayoutFeature] of TNyxText =
    ('resize', 'auto-size', 'reorder', 'visibility', 'pinning');
  CLayoutFeatureCaptions: array[TNyxColumnLayoutFeature] of TNyxText =
    ('Allow user resizing', 'Allow Fit current page', 'Allow user reordering',
     'Allow showing and hiding', 'Allow pinning and unpinning');
  CLayoutPins: array[TNyxColumnPin] of TNyxText =
    ('Unpinned', 'Start edge', 'End edge');
  CSingleSort = 'Single key';
  CMultipleSort = 'Multiple keys';
  CExactText = 'Exact text';
  CAsciiText = 'ASCII insensitive';
  CPolicyFields: array[TNyxStudioQueryPolicyField] of TNyxText =
    ('enabled', 'filtering', 'sorting', 'sort-mode', 'text-comparison', 'family-operators');
  CComparisonCaptions: array[TNyxQueryComparison] of TNyxText =
    ('Equals', 'Does not equal', 'Less than', 'At most', 'Greater than',
     'At least', 'Contains', 'Starts with', 'Ends with');

function NyxStudioCollectionFormContexts(AShellRoot: TNyxNode): TNyxProjectionFormContexts;

  procedure Add(const AID: TNyxText);
  var
    LCount: Integer;
  begin

    if AShellRoot.Find(AID) = nil then
    begin
      Exit;
    end;
    LCount := Length(Result);
    SetLength(Result, LCount + 1);
    Result[LCount] := TNyxProjectionFormContext.ForForm(AID, AID,
      [NyxExtension(NyxMenuFormBaselineKey), NyxExtension(CPolicyBaseline)]);
  end;

begin
  Result := nil;

  if AShellRoot = nil then
  begin
    Exit;
  end;
  { Each root owns one complete draft baseline, including its policy forms.
    Keep these disjoint: accepting query controls must not borrow a layout
    form's draft, or reset an unrelated primary property form. Query semantics,
    fields, owner identities and unknown metadata remain strict contracts. }
  Add(CQueryEditor);
  Add(NyxStudioLayoutEditorID);
end;

function NyxStudioQueryPolicyFormID(AColumn: Integer): TNyxText;
begin

  if AColumn < -1 then
  begin
    raise ENyxCollection.Create('A query-policy form requires a table or column');
  end;
  Result := CQueryEditor + '-controls-table';

  if AColumn >= 0 then
  begin
    Result := CQueryEditor + '-controls-column-' + TNyxText(IntToStr(AColumn));
  end;
end;

function NyxStudioQueryPolicyFieldID(AColumn: Integer;
  AField: TNyxStudioQueryPolicyField): TNyxText;
begin

  if (Ord(AField) < Ord(Low(TNyxStudioQueryPolicyField))) or
    (Ord(AField) > Ord(High(TNyxStudioQueryPolicyField))) then
  begin
    raise ENyxCollection.Create('Unknown query-policy form field');
  end;
  Result := NyxStudioQueryPolicyFormID(AColumn) + '-' + CPolicyFields[AField];
end;

function NyxStudioQueryPolicyOperatorID(AColumn: Integer;
  AComparison: TNyxQueryComparison): TNyxText;
begin

  if (AColumn < 0) or (Ord(AComparison) < Ord(Low(TNyxQueryComparison))) or
    (Ord(AComparison) > Ord(High(TNyxQueryComparison))) then
  begin
    raise ENyxCollection.Create('An operator control requires its exact column and comparison');
  end;
  Result := NyxStudioQueryPolicyFormID(AColumn) + '-operator-' +
    TNyxText(IntToStr(Ord(AComparison)));
end;

function NyxStudioQueryPolicyApplyID(AColumn: Integer): TNyxText;
begin
  Result := NyxStudioQueryPolicyFormID(AColumn) + '-apply';
end;

function NyxStudioQueryPolicyDefaultID(AColumn: Integer): TNyxText;
begin
  Result := NyxStudioQueryPolicyFormID(AColumn) + '-default';
end;

function NyxStudioLayoutPolicyFormID(AColumn: Integer): TNyxText;
begin

  if (AColumn < -1) or (AColumn >= NyxMaximumColumnLayoutFields) then
  begin
    raise ENyxCollection.Create('A layout-policy form requires a bounded table or column');
  end;
  Result := NyxStudioLayoutEditorID + '-table';

  if AColumn >= 0 then
  begin
    Result := NyxStudioLayoutEditorID + '-column-' + TNyxText(IntToStr(AColumn));
  end;
end;

function NyxStudioLayoutPolicyFieldID(AColumn: Integer;
  AField: TNyxStudioLayoutPolicyField): TNyxText;
begin

  if (Ord(AField) < Ord(Low(TNyxStudioLayoutPolicyField))) or
    (Ord(AField) > Ord(High(TNyxStudioLayoutPolicyField))) then
  begin
    raise ENyxCollection.Create('Unknown layout-policy scalar role');
  end;
  Result := NyxStudioLayoutPolicyFormID(AColumn) + '-' + CLayoutFields[AField];
end;

function NyxStudioLayoutPolicyFeatureID(AColumn: Integer;
  AFeature: TNyxColumnLayoutFeature): TNyxText;
begin

  if (Ord(AFeature) < Ord(Low(TNyxColumnLayoutFeature))) or
    (Ord(AFeature) > Ord(High(TNyxColumnLayoutFeature))) then
  begin
    raise ENyxCollection.Create('Unknown layout-policy feature');
  end;
  Result := NyxStudioLayoutPolicyFormID(AColumn) + '-feature-' + CLayoutFeatures[AFeature];
end;

function NyxStudioLayoutPolicyApplyID(AColumn: Integer): TNyxText;
begin
  Result := NyxStudioLayoutPolicyFormID(AColumn) + '-apply';
end;

function NyxStudioLayoutPolicyDefaultID(AColumn: Integer): TNyxText;
begin
  Result := NyxStudioLayoutPolicyFormID(AColumn) + '-default';
end;

{ This separate card owns no runtime arrangement. Target adapters copy only
  its unsubmitted input through the generic exact-baseline form draft contract;
  Apply/default alone enters the existing paired source-command queue. }
{$include nyx.studio.collections.formatting.inc}
{$include nyx.studio.collections.workbench.inc}
{$include nyx.studio.collections.presentation.inc}
{$include nyx.studio.collections.alignment.inc}

procedure AddLayoutPolicyForms(AParent: TNyxNode; const AOwner: TNyxText;
  const ASchema: TNyxCollectionSchema; const ASpec: TNyxCollectionViewSpec;
  ALocked: Boolean);
var
  LEditor: INyxCard;
  LForm: TNyxNode;
  LDisclosure: INyxDisclosure;
  LTable: TNyxTableLayoutPolicy;
  LColumn: TNyxCollectionColumn;
  LPolicy: TNyxColumnLayoutPolicy;
  LIndex: Integer;

  procedure BeginForm(AColumn: Integer; const ATitle: TNyxText; ADeclared: Boolean);
  var
    LStatus: TNyxText;
  begin
    LForm := TNyxNode.Create(nkCard, NyxStudioLayoutPolicyFormID(AColumn));
    LForm.Configure.Padding(8).Gap(6).Surface(True).Done;
    LForm.SetProp(CLayoutColumn, TNyxText(IntToStr(AColumn)))
      .SetProp(COwner, AOwner).SetProp(CKey, ASpec.Key.Name)
      .SetProp(CPolicyBaseline, LEditor.Node.Prop(NyxMenuFormBaselineKey));
    LForm.Add(TNyxNode.Create(nkHeading, LForm.ID + '-title').Configure.Text(ATitle).Done);
    LStatus := 'Using Nyx defaults. Apply declares these choices explicitly.';

    if ADeclared then
    begin
      LStatus := 'Explicit choices. Use Nyx defaults clears only this policy.';
    end;
    LForm.Add(TNyxNode.Create(nkLabel, LForm.ID + '-status').Configure.Text(LStatus).Done);

    if AColumn < 0 then
    begin
      LEditor.Node.Add(LForm);
    end
    else
    begin
      LDisclosure := NewNyxDisclosure(LForm.ID + '-section').WithText(ATitle);
      LDisclosure.Configure.Value(False).Surface(False).Done;
      LDisclosure.BodyColumn.Node.Add(LForm);
      LEditor.Node.Add(LDisclosure);
    end;
  end;

  procedure Flag(AColumn: Integer; AField: TNyxStudioLayoutPolicyField;
    const ACaption: TNyxText; AValue: Boolean);
  begin
    LForm.Add(TNyxNode.Create(nkCheckbox, NyxStudioLayoutPolicyFieldID(AColumn, AField))
      .Configure.Text(ACaption).Value(AValue).Enabled(not ALocked).Done);
  end;

  procedure Features(AColumn: Integer; AFeatures: TNyxColumnLayoutFeatures);
  var
    LFeature: TNyxColumnLayoutFeature;
  begin
    for LFeature := Low(TNyxColumnLayoutFeature) to High(TNyxColumnLayoutFeature) do
    begin
      LForm.Add(TNyxNode.Create(nkCheckbox, NyxStudioLayoutPolicyFeatureID(AColumn, LFeature))
        .Configure.Text(CLayoutFeatureCaptions[LFeature]).Value(LFeature in AFeatures)
        .Enabled(not ALocked).Done);
    end;
  end;

  procedure EndForm(AColumn: Integer);
  begin
    LForm.Add(TNyxNode.Create(nkButton, NyxStudioLayoutPolicyApplyID(AColumn))
      .Configure.Text('Apply layout defaults').Enabled(not ALocked)
      .Extension(CLayoutAction, CPolicyApply).Done);
    LForm.Add(TNyxNode.Create(nkButton, NyxStudioLayoutPolicyDefaultID(AColumn))
      .Configure.Text('Use Nyx defaults').Enabled(not ALocked)
      .Extension(CLayoutAction, CPolicyDefault).Done);
  end;

begin
  LEditor := NewNyxCard(NyxStudioLayoutEditorID);
  LEditor.Configure.Layout(nlColumn).Gap(8).Padding(12).Compound(True).Done;
  LEditor.Node.SetProp(NyxMenuFormOwnerKey, AOwner)
    .SetProp(NyxMenuFormBaselineKey, NyxQueryEditorBaseline(ASchema, ASpec))
    .SetProp(NyxMenuFormReferenceKey, '');
  LEditor.Node.Add(TNyxNode.Create(nkHeading, NyxStudioLayoutEditorID + '-title')
    .Configure.Text('Table defaults').Done);
  LEditor.Node.Add(TNyxNode.Create(nkLabel, NyxStudioLayoutEditorID + '-help')
    .Configure.Text('Author table defaults here; runtime Columns choices remain independent. ' +
      'Disabled user controls still honor authored visibility and pins. ' +
      'Use collection inheritance to restore a reusable binding.').Done);
  LTable := ASpec.TableLayoutPolicy;
  BeginForm(-1, 'Table layout controls', ASpec.HasTableLayoutPolicy);
  Flag(-1, slpfEnabled, 'Enable user column controls', LTable.IsEnabled);
  Features(-1, LTable.Features);
  LForm.Add(TNyxNode.Create(nkInput, NyxStudioLayoutPolicyFieldID(-1, slpfMinimumCenter))
    .Configure.Text('Minimum scrollable center width (logical px)')
    .Value(IntToStr(LTable.MinimumScrollableWidth)).Enabled(not ALocked).Done);
  LForm.Add(TNyxNode.Create(nkInput, NyxStudioLayoutPolicyFieldID(-1, slpfAutoSizeRows))
    .Configure.Text('Maximum current-page rows sampled by Fit (1..1024)')
    .Value(IntToStr(LTable.AutoSizeRows)).Enabled(not ALocked).Done);
  EndForm(-1);
  AddWorkbenchPolicyForm(LEditor.Node, AOwner, ASpec, ALocked);
  for LIndex := 0 to ASpec.Count - 1 do
  begin
    LColumn := ASpec.ColumnAt(LIndex);
    LPolicy := LColumn.LayoutPolicy;
    BeginForm(LIndex, LColumn.Title + ' layout controls', LColumn.HasLayoutPolicy);
    LForm.SetProp(CField, LColumn.FieldName).SetProp(CKind, NyxStateKindName(LColumn.Kind));
    Flag(LIndex, slpfVisible, 'Initially visible', LPolicy.IsVisible);
    LForm.Add(TNyxNode.Create(nkSelect, NyxStudioLayoutPolicyFieldID(LIndex, slpfPin))
      .Configure.Text('Authored pinned edge')
      .Items(CLayoutPins[ncpNone] + #10 + CLayoutPins[ncpStart] + #10 + CLayoutPins[ncpEnd])
      .Value(CLayoutPins[LPolicy.Pin]).Enabled(not ALocked).Done);
    Features(LIndex, LPolicy.Features);
    EndForm(LIndex);
  end;
  AddFormatPolicyForms(LEditor.Node, AOwner, ASchema, ASpec, ALocked);
  AddPresentationPolicyForms(LEditor.Node, AOwner, ASpec, ALocked);
  AddColumnAlignmentForms(LEditor.Node, AOwner, ASpec, ALocked);
  AParent.Add(LEditor);
end;

{ Policy controls are children of the existing query editor. Both Studio hosts
  already capture that complete subtree through their copied draft owner. These
  controls have no immediate collection command: only Apply/default captures one
  complete typed policy, never a sequence of interim invalid declarations. }
procedure AddQueryPolicyForms(AEditor: TNyxNode; const AOwner: TNyxText;
  const ASpec: TNyxCollectionViewSpec; ALocked: Boolean);
var
  LForm: TNyxNode;
  LColumn: TNyxCollectionColumn;
  LTable: TNyxTableQueryPolicy;
  LPolicy: TNyxColumnQueryPolicy;
  LIndex, LOperatorIndex: Integer;
  LChosen: Boolean;
  LComparison: TNyxQueryComparison;
  LFamily: TNyxQueryComparisons;

  procedure Toggle(AColumn: Integer; AField: TNyxStudioQueryPolicyField;
    const ACaption: TNyxText; AValue: Boolean);
  begin
    LForm.Add(TNyxNode.Create(nkCheckbox, NyxStudioQueryPolicyFieldID(AColumn, AField))
      .Configure.Text(ACaption).Value(AValue).Enabled(not ALocked).Done);
  end;

  procedure Choice(AColumn: Integer; AField: TNyxStudioQueryPolicyField;
    const ACaption, AChoices, AValue: TNyxText);
  begin
    LForm.Add(TNyxNode.Create(nkSelect, NyxStudioQueryPolicyFieldID(AColumn, AField))
      .Configure.Text(ACaption).Items(AChoices).Value(AValue).Enabled(not ALocked).Done);
  end;

  procedure BeginForm(AColumn: Integer; const ATitle: TNyxText; ADeclared: Boolean);
  var
    LStatus: TNyxText;
    LDisclosure: INyxDisclosure;
  begin
    LForm := TNyxNode.Create(nkCard, NyxStudioQueryPolicyFormID(AColumn));
    LForm.Configure.Padding(8).Gap(6).Surface(True).Done;
    LForm.SetProp(CPolicyColumn, TNyxText(IntToStr(AColumn)))
      .SetProp(COwner, AOwner).SetProp(CKey, ASpec.Key.Name)
      .SetProp(CPolicyBaseline, AEditor.Prop(NyxMenuFormBaselineKey));
    LForm.Add(TNyxNode.Create(nkHeading, LForm.ID + '-title').Configure.Text(ATitle).Done);
    LStatus := 'Using Nyx defaults. Apply declares these choices explicitly.';

    if ADeclared then
    begin
      LStatus := 'Explicit choices. Use Nyx defaults clears only this policy.';
    end;
    LForm.Add(TNyxNode.Create(nkLabel, LForm.ID + '-status').Configure.Text(LStatus).Done);

    if AColumn < 0 then
    begin
      AEditor.Add(LForm);
    end
    else
    begin
      { The existing Disclosure owns only local presentation. Its retained body
        remains in the query editor's ordinary draft capture subtree. }
      LDisclosure := NewNyxDisclosure(LForm.ID + '-section').WithText(ATitle);
      LDisclosure.Configure.Value(False).Surface(False).Done;
      LDisclosure.BodyColumn.Node.Add(LForm);
      AEditor.Add(LDisclosure);
    end;
  end;

  procedure EndForm(AColumn: Integer);
  begin
    LForm.Add(TNyxNode.Create(nkButton, NyxStudioQueryPolicyApplyID(AColumn))
      .Configure.Text('Apply query controls').Enabled(not ALocked)
      .Extension(CPolicyAction, CPolicyApply).Done);
    LForm.Add(TNyxNode.Create(nkButton, NyxStudioQueryPolicyDefaultID(AColumn))
      .Configure.Text('Use Nyx defaults').Enabled(not ALocked)
      .Extension(CPolicyAction, CPolicyDefault).Done);
  end;

begin
  LTable := ASpec.TableQueryPolicy;
  BeginForm(-1, 'Table query controls', ASpec.HasTableQueryPolicy);
  Toggle(-1, sqpfEnabled, 'Enable user query controls', LTable.IsEnabled);
  Toggle(-1, sqpfFiltering, 'Allow user filtering', LTable.FilterEnabled);
  Toggle(-1, sqpfSorting, 'Allow user sorting', LTable.SortEnabled);

  if LTable.Mode = nqsmSingle then
  begin
    Choice(-1, sqpfSortMode, 'User sort keys', CSingleSort + #10 + CMultipleSort, CSingleSort);
  end
  else
  begin
    Choice(-1, sqpfSortMode, 'User sort keys', CSingleSort + #10 + CMultipleSort, CMultipleSort);
  end;
  EndForm(-1);
  for LIndex := 0 to ASpec.Count - 1 do
  begin
    LColumn := ASpec.ColumnAt(LIndex);
    LPolicy := LColumn.QueryPolicy;
    BeginForm(LIndex, LColumn.Title + ' query controls', LColumn.HasQueryPolicy);
    LForm.SetProp(CField, LColumn.FieldName).SetProp(CKind, NyxStateKindName(LColumn.Kind));
    Toggle(LIndex, sqpfFiltering, 'Allow filtering this column', LPolicy.FilterEnabled);
    Toggle(LIndex, sqpfSorting, 'Allow sorting this column', LPolicy.SortEnabled);
    Toggle(LIndex, sqpfFamilyOperators, 'Use the field family default operators', not LPolicy.HasOperators);

    if LColumn.Kind = nskText then
    begin

      if LPolicy.TextMode = nqtAsciiInsensitive then
      begin
        Choice(LIndex, sqpfTextComparison, 'User text comparison',
          CExactText + #10 + CAsciiText, CAsciiText);
      end
      else
      begin
        Choice(LIndex, sqpfTextComparison, 'User text comparison',
          CExactText + #10 + CAsciiText, CExactText);
      end;
    end;
    LFamily := NyxQueryFamilyComparisons(LColumn.Kind);
    for LComparison := Low(TNyxQueryComparison) to High(TNyxQueryComparison) do
    begin

      if LComparison in LFamily then
      begin
        LChosen := not LPolicy.HasOperators;
        for LOperatorIndex := 0 to LPolicy.ComparisonCount - 1 do
        begin
          LChosen := LChosen or (LPolicy.ComparisonAt(LOperatorIndex) = LComparison);
        end;
        LForm.Add(TNyxNode.Create(nkCheckbox,
          NyxStudioQueryPolicyOperatorID(LIndex, LComparison)).Configure
          .Text(CComparisonCaptions[LComparison]).Value(LChosen)
          .Enabled(not ALocked).Done);
      end;
    end;
    EndForm(LIndex);
  end;
end;

class function TNyxStudioCollectionChromeIdentity.FromNode(ANode: TNyxNode):
  TNyxStudioCollectionChromeIdentity;
begin
  Result := Default(TNyxStudioCollectionChromeIdentity);

  if (ANode = nil) or (ANode.Prop(CCommandKey) = '') then
  begin
    Exit;
  end;
  Result.FDefined := True;
  Result.FCommand := ANode.Prop(CCommandKey);
  Result.FKey := ANode.Prop(CKey);
  Result.FField := ANode.Prop(CField);
  Result.FRow := ANode.Prop(CRow);
  Result.FOwner := ANode.Prop(COwner);
  Result.FKind := ANode.Prop(CKind);
end;

function TNyxStudioCollectionChromeIdentity.Matches(ANode: TNyxNode): Boolean;
var
  LOther: TNyxStudioCollectionChromeIdentity;
begin
  LOther := FromNode(ANode);
  Result := FDefined and LOther.FDefined and (FCommand = LOther.FCommand) and
    (FKey = LOther.FKey) and (FField = LOther.FField) and (FRow = LOther.FRow) and
    (FOwner = LOther.FOwner) and (FKind = LOther.FKind);
end;

function Command(AKind: TNyxKind; const AID, ATitle, AKey: TNyxText;
  ACommand: TCollectionCommand): TNyxNode;
begin
  Result := TNyxNode.Create(AKind, AID).Configure.Text(ATitle)
    .Extension(CCommandKey, CCommands[ACommand]).Extension(CKey, AKey).Done;
end;

function Editor(const AID, ATitle, AKey, AField, ARow: TNyxText;
  const AValue: TNyxStateValue; ACommand: TCollectionCommand): TNyxNode;
var
  LInput: TNyxStudioStateInput;
  LKind: TNyxKind;
begin
  LInput := NyxStudioStateInputFor(AValue);
  LKind := nkInput;

  if LInput in [ssiText, ssiEscapedText] then
  begin
    LKind := nkMemo;
  end
  else if LInput = ssiBoolean then
  begin
    LKind := nkSelect;
  end;
  Result := Command(LKind, AID, ATitle, AKey, ACommand);
  Result.Configure.Value(NyxStudioStateEditorText(AValue))
    .Extension(CField, AField).Extension(CRow, ARow)
    .Extension(CKind, NyxStateKindName(AValue.Kind))
    .Extension(NyxStudioStateInputKey, NyxStudioStateInputName(LInput)).Done;

  if LInput = ssiBoolean then
  begin
    Result.Configure.Items('false' + #10 + 'true').Done;
  end;
end;

procedure AddNyxCollectionDefaultsPanel(AParent: TNyxNode;
  ASession: TNyxStudioSession; AVisible: Boolean);
begin
  AddNyxCollectionDefaultsPanel(AParent, ASession, AVisible,
    Default(TNyxStudioPendingDesign));
end;

procedure AddNyxCollectionDefaultsPanel(AParent: TNyxNode;
  ASession: TNyxStudioSession; AVisible: Boolean;
  const APending: TNyxStudioPendingDesign);
var
  LPanel, LCard, LRow: TNyxNode;
  LData: INyxCollectionSnapshot;
  LSchema: TNyxCollectionSchema;
  LField: TNyxCollectionField;
  LCollection, LIndex, LItem: Integer;
  LKind: TNyxStateKind;
  LKey, LPrefix: TNyxText;
  LPending: TNyxStudioCollectionIntent;
  LItemRef: TNyxItemRef;
  LLocked: Boolean;
  LResourceRows: TNyxResourceRows;
  LSourceBacked: Boolean;

  function PendingEditor(const AID, ATitle, AField: TNyxText;
    const AItem: TNyxItemRef; const AValue: TNyxStateValue;
    AAction: TNyxStudioCollectionAction): TNyxNode;
  var
    LRowID: TNyxText;
    LInput: TNyxStudioStateInput;
    LReference: TNyxStudioCollectionFieldRef;
  begin
    LRowID := '';

    if AItem.Defined then
    begin
      LRowID := AItem.ID;
    end;
    Result := Editor(AID, ATitle, LKey, AField, LRowID, AValue, AAction);
    Result.Configure.Enabled(not LLocked).Done;
    LReference := TNyxStudioCollectionFieldRef.FromMetadata(LField);

    if APending.CollectionValue(NyxCollection(LKey), AItem, LReference,
      AAction, '', LPending) then
    begin
      { A newer waiting escaped/plain notation owns this field presentation.
        Earlier publication or failure must not normalize/drop its exact text. }
      LInput := LPending.Input;
      Result.Configure.Value(LPending.Value)
        .Extension(NyxStudioStateInputKey, NyxStudioStateInputName(LInput)).Done;
    end;
  end;
begin

  if not AVisible then
  begin
    Exit;
  end;
  LPanel := TNyxNode.Create(nkColumn, 'studio-collections').Configure.Gap(12).Done;
  AParent.Add(LPanel);
  LPanel.Add(TNyxNode.Create(nkHeading, 'collections-title').Configure.Text('Collections').Done);
  LPanel.Add(TNyxNode.Create(nkLabel, 'collections-help').Configure.Text(
    'Define typed fields and saved rows. Bind a list, table or tree in the inspector.').Done);
  LPanel.Add(Command(nkButton, 'collection-create', 'Add collection', '', scaCreate)
    .Configure.Enabled(not APending.CollectionCreationPending).Done);
  for LCollection := 0 to ASession.Document.Collections.Count - 1 do
  begin
    LKey := ASession.Document.Collections.Key(LCollection).Name;
    LData := ASession.Document.Collections.Snapshot(NyxCollection(LKey));
    LSchema := LData.Schema;
    LSourceBacked := ASession.Document.ResourceCollections.HasSource(NyxCollection(LKey));
    LLocked := APending.CollectionLocked(NyxCollection(LKey)) or LSourceBacked;
    LPrefix := 'collection-' + IntToStr(LCollection);
    LCard := TNyxNode.Create(nkPanel, LPrefix).Configure.Surface(True).Padding(12).Gap(8).Done;
    LPanel.Add(LCard);
    LCard.Add(TNyxNode.Create(nkHeading, LPrefix + '-title').Configure.Text(LKey).Done);

    if LSourceBacked then
    begin
      LResourceRows := ASession.Document.ResourceCollections.Source(NyxCollection(LKey));
      LCard.Add(NewNyxLabel(LPrefix + TNyxText('-source')).WithText(
        'Resource: ' + LResourceRows.Reference.Name + TNyxText('. Schema only; rows load at runtime. ')
        + TNyxText('Use Resources to edit or keep the authored rows and detach.')).Node);
    end;
    LCard.Add(Command(nkButton, LPrefix + '-remove', 'Remove collection', LKey, scaRemove)
      .Configure.Enabled(not LLocked).Done);
    for LIndex := 0 to LSchema.Count - 1 do
    begin
      LField := LSchema.FieldAt(LIndex);
      LCard.Add(PendingEditor(LPrefix + '-field-' + IntToStr(LIndex),
        LField.Name + ' / ' + NyxStateKindName(LField.Kind) + ' default',
        LField.Name, Default(TNyxItemRef), LField.DefaultValue, scaDefault));
    end;
    LRow := TNyxNode.Create(nkGrid, LPrefix + '-add-fields').Configure.Columns(2).Gap(6).Done;
    LCard.Add(LRow);
    for LKind := Low(TNyxStateKind) to High(TNyxStateKind) do
    begin
      LRow.Add(Command(nkButton, LPrefix + '-add-' + NyxStateKindName(LKind),
        '+ ' + NyxStateKindName(LKind) + ' field', LKey, scaAddField).Configure
        .Extension(CKind, NyxStateKindName(LKind)).Enabled(not LLocked).Done);
    end;
    for LItem := 0 to LData.Count - 1 do
    begin
      LItemRef := LData.ItemAt(LItem).Ref;
      LRow := TNyxNode.Create(nkPanel, LPrefix + '-row-' + IntToStr(LItem))
        .Configure.Surface(True).Padding(8).Gap(6).Done;
      LCard.Add(LRow);
      LRow.Add(TNyxNode.Create(nkLabel, LRow.ID + '-id').Configure.Text(LData.ItemAt(LItem).Ref.ID).Done);
      for LIndex := 0 to LSchema.Count - 1 do
      begin
        LField := LSchema.FieldAt(LIndex);
        LRow.Add(PendingEditor(LRow.ID + '-cell-' + IntToStr(LIndex), LField.Name,
          LField.Name, LItemRef,
          LData.ItemAt(LItem).FieldValue(LIndex), scaCell));
      end;
      LRow.Add(Command(nkButton, LRow.ID + '-remove', 'Remove row', LKey, scaRemoveRow)
        .Configure.Extension(CRow, LItemRef.ID).Enabled(not LLocked).Done);
    end;
    LCard.Add(Command(nkButton, LPrefix + '-add-row', 'Add row', LKey, scaAddRow)
      .Configure.Enabled(not LLocked).Done);
  end;
end;

procedure AddNyxCollectionBindingPanel(AParent: TNyxNode;
  ASession: TNyxStudioSession; AProjection: TNyxNode);
begin
  AddNyxCollectionBindingPanel(AParent, ASession, AProjection,
    Default(TNyxStudioPendingDesign));
end;

procedure AddNyxCollectionBindingPanel(AParent: TNyxNode;
  ASession: TNyxStudioSession; AProjection: TNyxNode;
  const APending: TNyxStudioPendingDesign);
var
  LPanel, LRow: TNyxNode;
  LSpec: TNyxCollectionViewSpec;
  LInherited: TNyxCollectionViewSpec;
  LColumn: TNyxCollectionColumn;
  LColumnSizing: TNyxColumnSizing;
  LQueryEditor: INyxCard;
  LPaging: TNyxCollectionPaging;
  LSchema: TNyxCollectionSchema;
  LIndex, LFieldIndex: Integer;
  LKey, LID: TNyxText;
  LFound: Boolean;
  LLocked: Boolean;
  LPendingIndex: Integer;

  function Action(AKind: TNyxKind; const AID, ATitle: TNyxText;
    ACommand: TCollectionCommand): TNyxNode;
  begin
    Result := Command(AKind, AID, ATitle, LKey, ACommand).Configure
      .Extension(COwner, ASession.SelectedID).Enabled(not LLocked).Done;
  end;

begin

  if (AProjection = nil) or not
    ((AProjection.ProjectionKind = 'list') or (AProjection.ProjectionKind = 'table') or
    (AProjection.ProjectionKind = 'tree')) then
  begin
    Exit;
  end;
  LPanel := TNyxNode.Create(nkPanel, 'studio-collection-binding').Configure
    .Surface(True).Padding(12).Gap(8).Done;
  AParent.Add(LPanel);
  LPanel.Add(TNyxNode.Create(nkHeading, 'collection-binding-title').Configure.Text('Collection binding').Done);
  LKey := '';

  if AProjection.HasCollectionView then
  begin
    LSpec := AProjection.CollectionView;
  end
  else
  begin
    LSpec := Default(TNyxCollectionViewSpec);
  end;
  LLocked := APending.CollectionViewLocked(ASession.SelectedID);
  { Query-policy proposals retain the accepted editor specification until the
    paired candidate is admitted. Overlaying their prospective policy here
    changes the exact form baseline, discards copied drafts and forces a
    physical replacement before publication. The existing owner lock above
    still disables the pending form, and capture independently refuses it.
    Admission installs the new policy normally; refusal or cancellation can
    restore the same accepted form and its unfinished values. Other pending
    collection actions retain their established presentation behavior. }
  for LPendingIndex := 0 to High(APending.Collections) do
  begin

    if (APending.Collections[LPendingIndex].Owner = ASession.SelectedID) and
      NyxStudioCollectionViewAction(APending.Collections[LPendingIndex].Intent.Action) and
      (APending.Collections[LPendingIndex].Intent.Action <> scaInherit) and
      not (APending.Collections[LPendingIndex].Intent.Action in
        [scaTableQuery, scaColumnQuery, scaDefaultTableQuery, scaDefaultColumnQuery,
         scaTableLayout, scaColumnLayout, scaDefaultTableLayout, scaDefaultColumnLayout,
         scaColumnFormat, scaDefaultColumnFormat, scaTableWorkbench, scaDefaultTableWorkbench,
         scaTablePresentation, scaDefaultTablePresentation, scaColumnText, scaDefaultColumnText,
         scaColumnAlignment, scaDefaultColumnAlignment]) then
    begin
      try
        LSchema := ASession.Document.Collections.Snapshot(
          APending.Collections[LPendingIndex].Intent.Key).Schema;
        LSpec := ApplyNyxStudioCollectionViewIntent(
          APending.Collections[LPendingIndex].Intent, LSpec, LSchema);
      except
        on ENyxCollection do
        begin
          { Pending presentation is never admission. A valid typed proposal can
            still reference a missing collection/field or a changed family.
            Preserve the last displayable specification while isolated replay
            owns its diagnostic; do not throw from the editor paint callback. }
        end;
        on EArgumentException do
        begin
          { Width policies reject incomplete or inverted bounds before compiler
            admission. Their pending face must retain the last displayable
            specification while the ordinary job reports that refusal. }
        end;
      end;
    end;
  end;

  if LSpec.Defined then
  begin
    LKey := LSpec.Key.Name;
    LLocked := LLocked or APending.CollectionLocked(LSpec.Key);
    LPanel.Add(TNyxNode.Create(nkLabel, 'collection-binding-current').Configure.Text('Current: ' + LKey).Done);
    LPanel.Add(Action(nkSelect, 'collection-binding-scope', 'Data scope', scaScope)
      .Configure.Items('Application' + #10 + 'Reusable instance').Value('Application').Done);

    if LSpec.Scope = csInstance then
    begin
      LPanel.Children[LPanel.Count - 1].Configure.Value('Reusable instance').Done;
    end;
    LPanel.Add(Action(nkSelect, 'collection-binding-selection', 'Selection', scaSelection)
      .Configure.Items('Single item' + #10 + 'Multiple items').Value('Single item').Done);

    if LSpec.SelectionMode = nsmMultiple then
    begin
      LPanel.Children[LPanel.Count - 1].Configure.Value('Multiple items').Done;
    end;

    if AProjection.ProjectionKind <> 'tree' then
    begin
      LPaging := LSpec.PagingPolicy;

      if not LPaging.Defined then
      begin
        LPaging := NyxCollectionPaging;
        LPanel.Add(TNyxNode.Create(nkLabel, 'collection-paging-status').Configure
          .Text('Paging is off. Set a page size to enable it.').Done);
      end
      else
      begin
        LPanel.Add(TNyxNode.Create(nkLabel, 'collection-paging-status').Configure
          .Text('Paged results follow filtering and ordering.').Done);
      end;
      LPanel.Add(Action(nkInput, 'collection-page-size', 'Items per page', scaPageSize)
        .Configure.InputType(niNumber).Value(LPaging.ItemsPerPage).Done);
      LPanel.Add(Action(nkInput, 'collection-initial-page', 'Initial page', scaInitialPage)
        .Configure.InputType(niNumber).Value(LPaging.FirstPage).Done);
      LPanel.Add(Action(nkButton, 'collection-default-paging', 'Show all results', scaDefaultPaging));
    end;
    for LIndex := 0 to LSpec.Count - 1 do
    begin
      LColumn := LSpec.ColumnAt(LIndex);
      LID := 'collection-column-' + IntToStr(LIndex);
      LRow := TNyxNode.Create(nkColumn, LID).Configure.Gap(6).Done;
      LPanel.Add(LRow);
      LRow.Add(Action(nkInput, LID + '-title', LColumn.FieldName + ' title', scaTitle)
        .Configure.Value(LColumn.Title).Extension(CField, LColumn.FieldName)
        .Extension(CKind, NyxStateKindName(LColumn.Kind)).Done);
      LRow.Add(Action(nkSelect, LID + '-mode', 'Editing', scaMode)
        .Configure.Items('Read only' + #10 + 'Editable').Value('Read only')
        .Extension(CField, LColumn.FieldName)
        .Extension(CKind, NyxStateKindName(LColumn.Kind)).Done);

      if LColumn.Mode = cmEditable then
      begin
        LRow.Children[LRow.Count - 1].Configure.Value('Editable').Done;
      end;

      if AProjection.Kind = NyxKindName(nkTable) then
      begin
        LColumnSizing := LColumn.Sizing;

        if not LColumnSizing.Defined then
        begin
          LColumnSizing := NyxColumnSizing;
        end;
        LRow.Add(Action(nkButton, LID + '-content', 'Fit content proportions', scaColumnContent)
          .Configure.Extension(CField, LColumn.FieldName)
          .Extension(CKind, NyxStateKindName(LColumn.Kind)).Done);
        LRow.Add(Action(nkInput, LID + '-fixed', 'Fixed width (pixels)', scaColumnFixed)
          .Configure.Placeholder('Enter a fixed width').Extension(CField, LColumn.FieldName)
          .Extension(CKind, NyxStateKindName(LColumn.Kind)).Done);

        if LColumnSizing.Mode = ncsFixed then
        begin
          LRow.Children[LRow.Count - 1].Configure.Value(IntToStr(LColumnSizing.Value)).Done;
        end;
        LRow.Add(Action(nkInput, LID + '-flex', 'Flexible proportion', scaColumnFlex)
          .Configure.Placeholder('Enter a positive proportion').Extension(CField, LColumn.FieldName)
          .Extension(CKind, NyxStateKindName(LColumn.Kind)).Done);

        if LColumnSizing.Mode = ncsFlex then
        begin
          LRow.Children[LRow.Count - 1].Configure.Value(IntToStr(LColumnSizing.Value)).Done;
        end;
        LRow.Add(Action(nkInput, LID + '-minimum', 'Minimum width', scaColumnMinimum)
          .Configure.Value(IntToStr(LColumnSizing.Bounds.MinimumValue)).Extension(CField, LColumn.FieldName)
          .Extension(CKind, NyxStateKindName(LColumn.Kind)).Done);
        LRow.Add(Action(nkInput, LID + '-maximum', 'Maximum width', scaColumnMaximum)
          .Configure.Value(IntToStr(LColumnSizing.Bounds.MaximumValue)).Extension(CField, LColumn.FieldName)
          .Extension(CKind, NyxStateKindName(LColumn.Kind)).Done);
        LRow.Add(Action(nkButton, LID + '-default-sizing', 'Restore default sizing', scaColumnDefault)
          .Configure.Extension(CField, LColumn.FieldName)
          .Extension(CKind, NyxStateKindName(LColumn.Kind)).Done);
      end;
      LRow.Add(Action(nkButton, LID + '-remove', 'Remove column', scaRemoveColumn)
        .Configure.Extension(CField, LColumn.FieldName)
        .Extension(CKind, NyxStateKindName(LColumn.Kind)).Done);
    end;
    LSchema := ASession.Document.Collections.Snapshot(LSpec.Key).Schema;
    LQueryEditor := NewNyxQueryEditor(NyxStudioQueryEditorID,
      NyxControl(ASession.SelectedID), LSchema, LSpec);
    LQueryEditor.Configure.Enabled(not LLocked).Done;
    AddNyxQueryEditorPreview(LQueryEditor);

    if AProjection.ProjectionKind = NyxKindName(nkTable) then
    begin
      AddQueryPolicyForms(LQueryEditor.Node, ASession.SelectedID, LSpec, LLocked);
    end;
    LPanel.Add(LQueryEditor);

    if AProjection.ProjectionKind = NyxKindName(nkTable) then
    begin
      AddLayoutPolicyForms(LPanel, ASession.SelectedID, LSchema, LSpec, LLocked);
    end;
    for LFieldIndex := 0 to LSchema.Count - 1 do
    begin
      LFound := False;
      for LIndex := 0 to LSpec.Count - 1 do
      begin
        LFound := LFound or (LSpec.ColumnAt(LIndex).FieldName = LSchema.FieldAt(LFieldIndex).Name);
      end;

      if not LFound then
      begin
        LPanel.Add(Action(nkButton, 'collection-column-add-' + IntToStr(LFieldIndex),
          '+ ' + LSchema.FieldAt(LFieldIndex).Name + ' column', scaAddColumn)
          .Configure.Extension(CField, LSchema.FieldAt(LFieldIndex).Name)
          .Extension(CKind, NyxStateKindName(LSchema.FieldAt(LFieldIndex).Kind)).Done);
      end;

      if (AProjection.ProjectionKind = 'tree') and (LSchema.FieldAt(LFieldIndex).Kind = nskText) then
      begin
        LPanel.Add(Action(nkButton, 'collection-parent-' + IntToStr(LFieldIndex),
          'Parent field: ' + LSchema.FieldAt(LFieldIndex).Name, scaParent)
          .Configure.Extension(CField, LSchema.FieldAt(LFieldIndex).Name)
          .Extension(CKind, NyxStateKindName(nskText)).Done);
      end;
    end;

    if AProjection.ProjectionKind = 'tree' then
    begin
      LPanel.Add(Action(nkButton, 'collection-parent-none', 'No parent field', scaParent));
    end;
    LPanel.Add(Action(nkButton, 'collection-binding-clear', 'Unbind collection', scaClear));
    LPanel.Add(Action(nkButton, 'collection-binding-inherit', 'Restore inherited binding', scaInherit));
  end
  else if (ASession.Selected <> nil) and ASession.Selected.HasCollectionView and
    not ASession.Selected.CollectionView.Defined then
  begin
    { An explicit clear masks effective metadata; it must still offer a way
      back to this exact owner's inherited contract. Pending structural paint
      keeps the same locks, and capture retains that contract's exact key. }
    LInherited := ASession.ClearedCollectionInheritance(ASession.SelectedID);

    if LInherited.Defined then
    begin
      LKey := LInherited.Key.Name;
      LLocked := LLocked or APending.CollectionLocked(LInherited.Key);
      LPanel.Add(TNyxNode.Create(nkLabel, 'collection-binding-inherited').Configure
        .Text('Inherited collection: ' + LKey).Done);
      LPanel.Add(Action(nkButton, 'collection-binding-inherit', 'Restore inherited binding', scaInherit));
    end;
  end;
  for LIndex := 0 to ASession.Document.Collections.Count - 1 do
  begin
    LKey := ASession.Document.Collections.Key(LIndex).Name;
    LPanel.Add(Action(nkButton, 'collection-bind-' + IntToStr(LIndex), 'Bind ' + LKey, scaBind));
  end;

  if ASession.Document.Collections.Count = 0 then
  begin
    LPanel.Add(TNyxNode.Create(nkLabel, 'collection-binding-empty').Configure
      .Text('Add a collection in the project Data section first.').Done);
  end;
end;


function CaptureNyxStudioQueryPreview(ASession: TNyxStudioSession;
  AButton, AShellRoot: TNyxNode; const APending: TNyxStudioPendingDesign;
  out APreview: TNyxStudioQueryPreview): Boolean;
var
  LEditor: TNyxNode;
  LSave: TNyxNode;
  LChange: TNyxQueryEditorChange;
  LProjection: TNyxNode;
  LSpec: TNyxCollectionViewSpec;
  LSource: INyxCollectionSnapshot;
  LReport: TNyxQueryPreview;
begin
  APreview := Default(TNyxStudioQueryPreview);
  Result := (AButton <> nil) and
    (AButton.ID = NyxQueryEditorPreviewID('inspector-collection-query'));

  if not Result then
  begin
    Exit;
  end;

  if (ASession = nil) or (AShellRoot = nil) or
    (AShellRoot.Find(AButton.ID) <> AButton) then
  begin
    raise ENyxCollection.Create('Use the current mounted query preview');
  end;
  LEditor := AShellRoot.Find('inspector-collection-query');

  if LEditor = nil then
  begin
    raise ENyxCollection.Create('Query editor is no longer mounted');
  end;
  LSave := LEditor.Find(NyxQueryEditorActionID(LEditor.ID, nqeSave));
  APreview.Context := ASession.CommandContext;
  APreview.OwnerID := ASession.SelectedID;
  APreview.ViewID := ASession.ActiveViewID;
  APreview.Defined := True;
  try

    if not CaptureNyxQueryEditor(LSave, AShellRoot, LChange) or
      (ASession.SelectedID <> LChange.Owner.ID) or
      APending.CollectionViewLocked(LChange.Owner.ID) then
    begin
      raise ENyxCollection.Create('Select this bound control again after pending edits');
    end;
    LProjection := ASession.SelectedProjection;
    try

      if (LProjection = nil) or not LProjection.HasCollectionView then
      begin
        raise ENyxCollection.Create('This control no longer has a collection binding');
      end;
      LSpec := LProjection.CollectionView;
      LSource := ASession.Document.Collections.Snapshot(LSpec.Key);

      if APending.CollectionLocked(LSpec.Key) or
        (NyxQueryEditorBaseline(LSource.Schema, LSpec) <> LChange.Baseline) then
      begin
        raise ENyxCollection.Create('Query binding/schema changed; use its current inspector');
      end;
      LReport := PreviewNyxCollectionQuery(LSource, LSpec,
        NyxCollectionProjectionForNode(LProjection), LChange.Query,
        TNyxQueryPreviewOptions.Defaults);
      APreview.Text := LReport.Text;
    finally
      LProjection.Free;
    end;
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}APreview.Text := 'Preview refused / ' + LException.Message;
      {$else}APreview.Text := TNyxText('Preview refused / ') + UTF8Encode(UnicodeString(LException.Message));{$endif}
    end;
  end;
end;

function RouteNyxStudioQueryPreview(ASession: TNyxStudioSession;
  const AEvent: TNyxEventInfo; AShellRoot: TNyxNode;
  const APending: TNyxStudioPendingDesign; out APreview: TNyxStudioQueryPreview): Boolean;
var
  LSource: TNyxNode;
  LOrigin: TNyxNode;
begin
  Result := False;
  APreview := Default(TNyxStudioQueryPreview);

  if (AShellRoot = nil) or (AEvent.Trigger <> ntClick) or AEvent.DefaultPrevented then
  begin
    Exit;
  end;
  LSource := AShellRoot.Find(AEvent.SourceID);

  if LSource = nil then
  begin
    Exit;
  end;
  LOrigin := LSource.Find(AEvent.OriginID);

  if LOrigin <> nil then
  begin
    Result := CaptureNyxStudioQueryPreview(ASession, LOrigin, AShellRoot, APending, APreview);
  end;
end;

procedure RestoreNyxStudioQueryPreview(var APreview: TNyxStudioQueryPreview;
  ASession: TNyxStudioSession; AShellRoot: TNyxNode);
var
  LOutput: TNyxNode;
begin

  if not APreview.Defined then
  begin
    Exit;
  end;

  if (ASession = nil) or not ASession.MatchesCommandContext(APreview.Context) or
    (ASession.SelectedID <> APreview.OwnerID) or (ASession.ActiveViewID <> APreview.ViewID) then
  begin
    APreview := Default(TNyxStudioQueryPreview);
    Exit;
  end;

  if AShellRoot = nil then
  begin
    Exit;
  end;
  LOutput := AShellRoot.Find(NyxQueryEditorPreviewOutputID('inspector-collection-query'));

  if LOutput <> nil then
  begin
    LOutput.Configure.Value(APreview.Text).Done;
  end;
  LOutput := AShellRoot.Find(NyxQueryEditorPreviewExpandID('inspector-collection-query'));

  if LOutput <> nil then
  begin
    LOutput.Configure.Enabled(True).Done;
  end;
end;

function RouteNyxStudioQueryPreviewExpansion(ASession: TNyxStudioSession;
  const AEvent: TNyxEventInfo; AShellRoot: TNyxNode;
  const APreview: TNyxStudioQueryPreview): Boolean;
var
  LSource: TNyxNode;
begin
  Result := False;

  if (ASession = nil) or (AShellRoot = nil) or not APreview.Defined or
    (AEvent.Trigger <> ntClick) or AEvent.DefaultPrevented or
    (AEvent.OriginID <> NyxQueryEditorPreviewExpandID('inspector-collection-query')) or
    not ASession.MatchesCommandContext(APreview.Context) or
    (ASession.SelectedID <> APreview.OwnerID) or (ASession.ActiveViewID <> APreview.ViewID) then
  begin
    Exit;
  end;
  LSource := AShellRoot.Find(AEvent.SourceID);
  Result := (LSource <> nil) and (LSource.Find(AEvent.OriginID) <> nil);
end;

{ Read only the exact current mounted form. Closed choices and direct role
  shapes are verified before constructing either immutable policy; the form's
  copied baseline is rechecked again by the isolated collection candidate. }
function CaptureQueryPolicy(ASession: TNyxStudioSession;
  AButton, AShellRoot: TNyxNode; const APending: TNyxStudioPendingDesign;
  out AEdit: TNyxStudioDesignEdit): Boolean;
var
  LForm, LEditor, LProjection, LRole: TNyxNode;
  LColumnIndex, LIndex, LExpectedCount, LCount: Integer;
  LAction, LBaseline, LOwner, LText: TNyxText;
  LSpec: TNyxCollectionViewSpec;
  LSchema: TNyxCollectionSchema;
  LColumn: TNyxCollectionColumn;
  LReference: TNyxStudioCollectionFieldRef;
  LFamily: TNyxQueryComparisons;
  LComparison: TNyxQueryComparison;
  LOperators: array of TNyxQueryComparison;
  LTablePolicy: TNyxTableQueryPolicy;
  LColumnPolicy: TNyxColumnQueryPolicy;

  function Role(const AID: TNyxText; AKind: TNyxKind): TNyxNode;
  begin
    Result := LForm.Find(AID);

    if (Result = nil) or (Result.Parent <> LForm) or
      (Result.Kind <> NyxKindName(AKind)) or
      (Result.ProjectionKind <> NyxKindName(AKind)) then
    begin
      raise ENyxCollection.Create('Use the current query-policy form and its direct typed controls');
    end;
  end;

  function Flag(AField: TNyxStudioQueryPolicyField): Boolean;
  var
    LValue: TNyxText;
  begin
    LValue := Role(NyxStudioQueryPolicyFieldID(LColumnIndex, AField), nkCheckbox)
      .Prop(NyxAttributeName(atValue));

    if (LValue <> 'true') and (LValue <> 'false') then
    begin
      raise ENyxCollection.Create('Query-policy Boolean choices require true or false');
    end;
    Result := LValue = 'true';
  end;

begin
  AEdit := Default(TNyxStudioDesignEdit);
  Result := (AButton <> nil) and (AButton.Prop(CPolicyAction) <> '');

  if not Result then
  begin
    Exit;
  end;

  if (ASession = nil) or (AShellRoot = nil) or
    (AShellRoot.Find(AButton.ID) <> AButton) or (AButton.Parent = nil) then
  begin
    raise ENyxCollection.Create('Use the current mounted query-policy action');
  end;
  LForm := AButton.Parent;
  LEditor := AShellRoot.Find(CQueryEditor);

  if (LEditor = nil) or (LEditor.Find(LForm.ID) <> LForm) or
    (LForm.Kind <> NyxKindName(nkCard)) or
    (LForm.ProjectionKind <> NyxKindName(nkCard)) or
    (LEditor.Kind <> NyxKindName(nkCard)) or
    (LEditor.ProjectionKind <> NyxKindName(nkCard)) or
    not TryStrToInt(LForm.Prop(CPolicyColumn), LColumnIndex) or
    (LColumnIndex < -1) then
  begin
    raise ENyxCollection.Create('Query-policy action has no current table or column form');
  end;

  if (LForm.ID <> NyxStudioQueryPolicyFormID(LColumnIndex)) or
    (LForm.Prop(CPolicyColumn) <> TNyxText(IntToStr(LColumnIndex))) then
  begin
    raise ENyxCollection.Create('Query-policy form identity changed');
  end;
  LAction := AButton.Prop(CPolicyAction);

  if ((LAction <> CPolicyApply) and (LAction <> CPolicyDefault)) or
    ((LAction = CPolicyApply) and (AButton.ID <> NyxStudioQueryPolicyApplyID(LColumnIndex))) or
    ((LAction = CPolicyDefault) and (AButton.ID <> NyxStudioQueryPolicyDefaultID(LColumnIndex))) then
  begin
    raise ENyxCollection.Create('Unknown query-policy action');
  end;
  Role(NyxStudioQueryPolicyApplyID(LColumnIndex), nkButton);
  Role(NyxStudioQueryPolicyDefaultID(LColumnIndex), nkButton);
  LOwner := LForm.Prop(COwner);
  LBaseline := LForm.Prop(CPolicyBaseline);

  if (LOwner = '') or (ASession.SelectedID <> LOwner) or
    (LEditor.Prop(NyxMenuFormOwnerKey) <> LOwner) or (LBaseline = '') or
    (LEditor.Prop(NyxMenuFormBaselineKey) <> LBaseline) or
    APending.CollectionViewLocked(LOwner) or ASession.SourceDraftPending then
  begin
    raise ENyxCollection.Create('Select this table again after pending edits or Pascal drafts');
  end;
  LProjection := ASession.SelectedProjection;
  try

    if (LProjection = nil) or (LProjection.ProjectionKind <> NyxKindName(nkTable)) or
      not LProjection.HasCollectionView or not LProjection.CollectionView.Defined then
    begin
      raise ENyxCollection.Create('Query controls require the selected bound table');
    end;
    LSpec := LProjection.CollectionView;
    LSchema := ASession.Document.Collections.Snapshot(LSpec.Key).Schema;

    if (LForm.Prop(CKey) <> LSpec.Key.Name) or APending.CollectionLocked(LSpec.Key) or
      (NyxQueryEditorBaseline(LSchema, LSpec) <> LBaseline) then
    begin
      raise ENyxCollection.Create('Table binding or schema changed; reopen its current policy form');
    end;
    Role(NyxStudioQueryPolicyFieldID(LColumnIndex, sqpfFiltering), nkCheckbox);
    Role(NyxStudioQueryPolicyFieldID(LColumnIndex, sqpfSorting), nkCheckbox);
    LExpectedCount := 8;

    if LColumnIndex = -1 then
    begin
      Role(NyxStudioQueryPolicyFieldID(-1, sqpfEnabled), nkCheckbox);
      Role(NyxStudioQueryPolicyFieldID(-1, sqpfSortMode), nkSelect);

      if (LForm.Prop(CField) <> '') or (LForm.Prop(CKind) <> '') then
      begin
        raise ENyxCollection.Create('Table query controls cannot impersonate a column');
      end;
    end
    else
    begin

      if LColumnIndex >= LSpec.Count then
      begin
        raise ENyxCollection.Create('The policy column is no longer present');
      end;
      LColumn := LSpec.ColumnAt(LColumnIndex);

      if (LForm.Prop(CField) <> LColumn.FieldName) or
        (LForm.Prop(CKind) <> NyxStateKindName(LColumn.Kind)) then
      begin
        raise ENyxCollection.Create('The policy column changed its identity or scalar family');
      end;
      LReference := Default(TNyxStudioCollectionFieldRef);
      for LIndex := 0 to LSchema.Count - 1 do
      begin

        if (LSchema.FieldAt(LIndex).Name = LColumn.FieldName) and
          (LSchema.FieldAt(LIndex).Kind = LColumn.Kind) then
        begin
          LReference := TNyxStudioCollectionFieldRef.FromMetadata(LSchema.FieldAt(LIndex));
          Break;
        end;
      end;

      if not LReference.Defined then
      begin
        raise ENyxCollection.Create('The policy field disappeared or changed its family');
      end;
      Role(NyxStudioQueryPolicyFieldID(LColumnIndex, sqpfFamilyOperators), nkCheckbox);
      LExpectedCount := 7;

      if LColumn.Kind = nskText then
      begin
        Role(NyxStudioQueryPolicyFieldID(LColumnIndex, sqpfTextComparison), nkSelect);
        Inc(LExpectedCount);
      end;
      LFamily := NyxQueryFamilyComparisons(LColumn.Kind);
      for LComparison := Low(TNyxQueryComparison) to High(TNyxQueryComparison) do
      begin

        if LComparison in LFamily then
        begin
          Role(NyxStudioQueryPolicyOperatorID(LColumnIndex, LComparison), nkCheckbox);
          Inc(LExpectedCount);
        end;
      end;
    end;

    if LForm.Count <> LExpectedCount then
    begin
      raise ENyxCollection.Create('The policy form structure changed; use its current controls');
    end;
    AEdit.Action := sdaCollection;
    AEdit.Selection := LOwner;
    AEdit.View := ASession.ActiveViewID;
    AEdit.Collection.Key := LSpec.Key;
    AEdit.Collection.Projection := cpTable;
    AEdit.Collection.QueryBaseline := LBaseline;

    if LColumnIndex = -1 then
    begin

      if LAction = CPolicyDefault then
      begin
        AEdit.Collection.Action := scaDefaultTableQuery;
      end
      else
      begin
        LTablePolicy := NyxTableQuery.Enabled(Flag(sqpfEnabled))
          .Filtering(Flag(sqpfFiltering)).Sorting(Flag(sqpfSorting));
        LText := Role(NyxStudioQueryPolicyFieldID(-1, sqpfSortMode), nkSelect)
          .Prop(NyxAttributeName(atValue));

        if LText = CSingleSort then
        begin
          LTablePolicy := LTablePolicy.SortMode(nqsmSingle);
        end
        else if LText = CMultipleSort then
        begin
          LTablePolicy := LTablePolicy.SortMode(nqsmMultiple);
        end
        else
        begin
          raise ENyxCollection.Create('Choose single or multiple user sort keys');
        end;
        AEdit.Collection.Action := scaTableQuery;
        AEdit.Collection.TableQuery := LTablePolicy;
      end;
    end
    else
    begin
      AEdit.Collection.Field := LReference;

      if LAction = CPolicyDefault then
      begin
        AEdit.Collection.Action := scaDefaultColumnQuery;
      end
      else
      begin
        LColumnPolicy := NyxColumnQuery.Filtering(Flag(sqpfFiltering)).Sorting(Flag(sqpfSorting));

        if LColumn.Kind = nskText then
        begin
          LText := Role(NyxStudioQueryPolicyFieldID(LColumnIndex, sqpfTextComparison), nkSelect)
            .Prop(NyxAttributeName(atValue));

          if LText = CExactText then
          begin
            LColumnPolicy := LColumnPolicy.TextComparison(nqtExact);
          end
          else if LText = CAsciiText then
          begin
            LColumnPolicy := LColumnPolicy.TextComparison(nqtAsciiInsensitive);
          end
          else
          begin
            raise ENyxCollection.Create('Choose exact or ASCII-insensitive text comparison');
          end;
        end;

        if not Flag(sqpfFamilyOperators) then
        begin
          LOperators := nil;
          for LComparison := Low(TNyxQueryComparison) to High(TNyxQueryComparison) do
          begin

            if LComparison in LFamily then
            begin
              LRole := Role(NyxStudioQueryPolicyOperatorID(LColumnIndex, LComparison), nkCheckbox);
              LText := LRole.Prop(NyxAttributeName(atValue));

              if (LText <> 'true') and (LText <> 'false') then
              begin
                raise ENyxCollection.Create('Operator choices require true or false');
              end;

              if LText = 'true' then
              begin
                LCount := Length(LOperators);
                SetLength(LOperators, LCount + 1);
                LOperators[LCount] := LComparison;
              end;
            end;
          end;
          LColumnPolicy := LColumnPolicy.Operators(LOperators);
        end;
        LColumnPolicy.Validate(LColumn.Kind);
        AEdit.Collection.Action := scaColumnQuery;
        AEdit.Collection.ColumnQuery := LColumnPolicy;
      end;
    end;
    AEdit.Collection.Validate;
  finally
    LProjection.Free;
  end;
end;

function CaptureNyxStudioLayout(ASession: TNyxStudioSession;
  AButton, AShellRoot: TNyxNode; const APending: TNyxStudioPendingDesign;
  out AEdit: TNyxStudioDesignEdit): Boolean;
var
  LEditor: TNyxNode;
  LForm: TNyxNode;
  LProjection: TNyxNode;
  LSpec: TNyxCollectionViewSpec;
  LSchema: TNyxCollectionSchema;
  LColumn: TNyxCollectionColumn;
  LColumnIndex: Integer;
  LFieldIndex: Integer;
  LExpectedCount: Integer;
  LOwner: TNyxText;
  LBaseline: TNyxText;
  LAction: TNyxText;
  LPinText: TNyxText;
  LMinimum: Integer;
  LRows: Integer;
  LFeatures: TNyxColumnLayoutFeatures;
  LFeature: TNyxColumnLayoutFeature;
  LPin: TNyxColumnPin;
  LPinFound: Boolean;

  function Role(const AID: TNyxText; AKind: TNyxKind): TNyxNode;
  begin
    Result := LForm.Find(AID);

    if (Result = nil) or (Result.Parent <> LForm) or
      (Result.Kind <> NyxKindName(AKind)) or
      (Result.ProjectionKind <> NyxKindName(AKind)) then
    begin
      raise ENyxCollection.Create('Use the current layout form and its direct typed controls');
    end;
  end;

  function Flag(const AID: TNyxText): Boolean;
  var
    LValue: TNyxText;
  begin
    LValue := Role(AID, nkCheckbox).Prop(NyxAttributeName(atValue));

    if (LValue <> 'true') and (LValue <> 'false') then
    begin
      raise ENyxCollection.Create('Layout Boolean choices require true or false');
    end;
    Result := LValue = 'true';
  end;

begin

  if CaptureNyxStudioColumnAlignment(ASession, AButton, AShellRoot, APending, AEdit) then
  begin
    Exit(True);
  end;

  if CaptureNyxStudioPresentation(ASession, AButton, AShellRoot, APending, AEdit) then
  begin
    Exit(True);
  end;

  if CaptureNyxStudioWorkbench(ASession, AButton, AShellRoot, APending, AEdit) then
  begin
    Exit(True);
  end;

  if CaptureNyxStudioFormat(ASession, AButton, AShellRoot, APending, AEdit) then
  begin
    Exit(True);
  end;
  AEdit := Default(TNyxStudioDesignEdit);
  Result := (AButton <> nil) and (AButton.Prop(CLayoutAction) <> '');

  if not Result then
  begin
    Exit;
  end;

  if (ASession = nil) or (AShellRoot = nil) or
    (AShellRoot.Find(AButton.ID) <> AButton) or (AButton.Parent = nil) then
  begin
    raise ENyxCollection.Create('Use the current mounted layout-policy action');
  end;
  LForm := AButton.Parent;
  LEditor := AShellRoot.Find(NyxStudioLayoutEditorID);

  if (LEditor = nil) or (LEditor.Find(LForm.ID) <> LForm) or
    (LEditor.Kind <> NyxKindName(nkCard)) or
    (LEditor.ProjectionKind <> NyxKindName(nkCard)) or
    (LForm.Kind <> NyxKindName(nkCard)) or
    (LForm.ProjectionKind <> NyxKindName(nkCard)) or
    not TryStrToInt(LForm.Prop(CLayoutColumn), LColumnIndex) or
    (LColumnIndex < -1) or (LColumnIndex >= NyxMaximumColumnLayoutFields) then
  begin
    raise ENyxCollection.Create('Layout action has no current table or column form');
  end;

  if (LForm.ID <> NyxStudioLayoutPolicyFormID(LColumnIndex)) or
    (LForm.Prop(CLayoutColumn) <> TNyxText(IntToStr(LColumnIndex))) then
  begin
    raise ENyxCollection.Create('Layout-policy form identity changed');
  end;
  LAction := AButton.Prop(CLayoutAction);

  if ((LAction <> CPolicyApply) and (LAction <> CPolicyDefault)) or
    ((LAction = CPolicyApply) and (AButton.ID <> NyxStudioLayoutPolicyApplyID(LColumnIndex))) or
    ((LAction = CPolicyDefault) and (AButton.ID <> NyxStudioLayoutPolicyDefaultID(LColumnIndex))) then
  begin
    raise ENyxCollection.Create('Unknown layout-policy action');
  end;
  Role(NyxStudioLayoutPolicyApplyID(LColumnIndex), nkButton);
  Role(NyxStudioLayoutPolicyDefaultID(LColumnIndex), nkButton);
  Role(LForm.ID + '-title', nkHeading);
  Role(LForm.ID + '-status', nkLabel);
  LOwner := LForm.Prop(COwner);
  LBaseline := LForm.Prop(CPolicyBaseline);

  if (LOwner = '') or (ASession.SelectedID <> LOwner) or
    (LEditor.Prop(NyxMenuFormOwnerKey) <> LOwner) or (LBaseline = '') or
    (LEditor.Prop(NyxMenuFormBaselineKey) <> LBaseline) or
    APending.CollectionViewLocked(LOwner) or ASession.SourceDraftPending then
  begin
    raise ENyxCollection.Create('Select this table again after pending edits or Pascal drafts');
  end;
  LProjection := ASession.SelectedProjection;
  try

    if (LProjection = nil) or (LProjection.ProjectionKind <> NyxKindName(nkTable)) or
      not LProjection.HasCollectionView or not LProjection.CollectionView.Defined then
    begin
      raise ENyxCollection.Create('Layout defaults require the selected bound table');
    end;
    LSpec := LProjection.CollectionView;
    LSchema := ASession.Document.Collections.Snapshot(LSpec.Key).Schema;

    if (LForm.Prop(CKey) <> LSpec.Key.Name) or APending.CollectionLocked(LSpec.Key) or
      (NyxQueryEditorBaseline(LSchema, LSpec) <> LBaseline) then
    begin
      raise ENyxCollection.Create('Table binding or schema changed; reopen its layout form');
    end;
    for LFeature := Low(TNyxColumnLayoutFeature) to High(TNyxColumnLayoutFeature) do
    begin
      Role(NyxStudioLayoutPolicyFeatureID(LColumnIndex, LFeature), nkCheckbox);
    end;

    if LColumnIndex < 0 then
    begin
      Role(NyxStudioLayoutPolicyFieldID(-1, slpfEnabled), nkCheckbox);
      Role(NyxStudioLayoutPolicyFieldID(-1, slpfMinimumCenter), nkInput);
      Role(NyxStudioLayoutPolicyFieldID(-1, slpfAutoSizeRows), nkInput);
      LExpectedCount := 12;

      if (LForm.Prop(CField) <> '') or (LForm.Prop(CKind) <> '') then
      begin
        raise ENyxCollection.Create('Table layout defaults cannot impersonate a column');
      end;
    end
    else
    begin

      if LColumnIndex >= LSpec.Count then
      begin
        raise ENyxCollection.Create('Layout column no longer exists');
      end;
      LColumn := LSpec.ColumnAt(LColumnIndex);

      if (LForm.Prop(CField) <> LColumn.FieldName) or
        (LForm.Prop(CKind) <> NyxStateKindName(LColumn.Kind)) then
      begin
        raise ENyxCollection.Create('Layout column changed its typed identity');
      end;
      Role(NyxStudioLayoutPolicyFieldID(LColumnIndex, slpfVisible), nkCheckbox);
      Role(NyxStudioLayoutPolicyFieldID(LColumnIndex, slpfPin), nkSelect);
      LExpectedCount := 11;
    end;

    if LForm.Count <> LExpectedCount then
    begin
      raise ENyxCollection.Create('Use the exact complete layout-policy form');
    end;
    AEdit.Action := sdaCollection;
    AEdit.Selection := LOwner;
    AEdit.View := ASession.ActiveViewID;
    AEdit.Collection.Key := LSpec.Key;
    AEdit.Collection.Projection := cpTable;
    AEdit.Collection.LayoutBaseline := LBaseline;

    if LColumnIndex < 0 then
    begin
      AEdit.Collection.Action := scaDefaultTableLayout;
    end
    else
    begin
      AEdit.Collection.Action := scaDefaultColumnLayout;
      for LFieldIndex := 0 to LSchema.Count - 1 do
      begin

        if (LSchema.FieldAt(LFieldIndex).Name = LColumn.FieldName) and
          (LSchema.FieldAt(LFieldIndex).Kind = LColumn.Kind) then
        begin
          AEdit.Collection.Field := TNyxStudioCollectionFieldRef.FromMetadata(
            LSchema.FieldAt(LFieldIndex));
          Break;
        end;
      end;

      if not AEdit.Collection.Field.Defined then
      begin
        raise ENyxCollection.Create('Layout column no longer matches its schema family');
      end;
    end;

    if LAction = CPolicyApply then
    begin
      LFeatures := [];
      for LFeature := Low(TNyxColumnLayoutFeature) to High(TNyxColumnLayoutFeature) do
      begin

        if Flag(NyxStudioLayoutPolicyFeatureID(LColumnIndex, LFeature)) then
        begin
          Include(LFeatures, LFeature);
        end;
      end;

      if LColumnIndex < 0 then
      begin

        if not TryNyxStateInteger(Role(NyxStudioLayoutPolicyFieldID(-1, slpfMinimumCenter),
            nkInput).Prop(NyxAttributeName(atValue)), LMinimum) or
          not TryNyxStateInteger(Role(NyxStudioLayoutPolicyFieldID(-1, slpfAutoSizeRows),
            nkInput).Prop(NyxAttributeName(atValue)), LRows) then
        begin
          raise ENyxCollection.Create('Layout bounds and Fit sample require exact Integers');
        end;
        AEdit.Collection.Action := scaTableLayout;
        AEdit.Collection.TableLayout := NyxTableLayout
          .Enabled(Flag(NyxStudioLayoutPolicyFieldID(-1, slpfEnabled)))
          .AllowOnly(LFeatures).MinimumCenter(LMinimum).AutoSizeSample(LRows);
      end
      else
      begin
        LPinText := Role(NyxStudioLayoutPolicyFieldID(LColumnIndex, slpfPin), nkSelect)
          .Prop(NyxAttributeName(atValue));
        LPinFound := False;
        for LPin := Low(TNyxColumnPin) to High(TNyxColumnPin) do
        begin

          if LPinText = CLayoutPins[LPin] then
          begin
            LPinFound := True;
            Break;
          end;
        end;

        if not LPinFound then
        begin
          raise ENyxCollection.Create('Choose an exact authored pinned edge');
        end;
        AEdit.Collection.Action := scaColumnLayout;
        AEdit.Collection.ColumnLayout := NyxColumnLayout.AllowOnly(LFeatures)
          .Visible(Flag(NyxStudioLayoutPolicyFieldID(LColumnIndex, slpfVisible))).Pinned(LPin);
      end;
    end;
    ApplyNyxStudioCollectionViewIntent(AEdit.Collection, LSpec, LSchema);
  finally
    LProjection.Free;
  end;
end;

function CaptureNyxStudioQuery(ASession: TNyxStudioSession;
  AButton, AShellRoot: TNyxNode; const APending: TNyxStudioPendingDesign;
  out AEdit: TNyxStudioDesignEdit): Boolean;
var
  LChange: TNyxQueryEditorChange;
  LProjection: TNyxNode;
  LSpec: TNyxCollectionViewSpec;
  LSchema: TNyxCollectionSchema;
begin
  AEdit := Default(TNyxStudioDesignEdit);

  if CaptureQueryPolicy(ASession, AButton, AShellRoot, APending, AEdit) then
  begin
    Exit(True);
  end;
  Result := CaptureNyxQueryEditor(AButton, AShellRoot, LChange);

  if not Result then
  begin
    Exit;
  end;

  if (ASession = nil) or (ASession.SelectedID <> LChange.Owner.ID) or
    APending.CollectionViewLocked(LChange.Owner.ID) then
  begin
    raise ENyxCollection.Create('Select this bound control again after pending edits');
  end;
  LProjection := ASession.SelectedProjection;
  try

    if (LProjection = nil) or not LProjection.HasCollectionView then
    begin
      raise ENyxCollection.Create('This control no longer has a collection binding');
    end;
    LSpec := LProjection.CollectionView;
    LSchema := ASession.Document.Collections.Snapshot(LSpec.Key).Schema;

    if APending.CollectionLocked(LSpec.Key) or
      (NyxQueryEditorBaseline(LSchema, LSpec) <> LChange.Baseline) then
    begin
      raise ENyxCollection.Create('Query binding/schema changed; use its current inspector');
    end;
    LChange.Query.Validate(LSchema);
    AEdit.Action := sdaCollection;
    AEdit.Selection := LChange.Owner.ID;
    AEdit.View := ASession.ActiveViewID;
    AEdit.Collection.Action := scaQuery;
    AEdit.Collection.Key := LSpec.Key;
    AEdit.Collection.Query := LChange.Query.Copy;
    AEdit.Collection.QueryBaseline := LChange.Baseline;

    if LProjection.ProjectionKind = NyxKindName(nkList) then
    begin
      AEdit.Collection.Projection := cpList;
    end
    else if LProjection.ProjectionKind = NyxKindName(nkTable) then
    begin
      AEdit.Collection.Projection := cpTable;
    end
    else if LProjection.ProjectionKind = NyxKindName(nkTree) then
    begin
      AEdit.Collection.Projection := cpTree;
    end
    else
    begin
      raise ENyxCollection.Create('Query authoring requires list/table/tree projection');
    end;
    AEdit.Collection.Validate;
  finally
    LProjection.Free;
  end;
end;

function CaptureNyxStudioCollection(ASession: TNyxStudioSession;
  ANode: TNyxNode; AEvent: TNyxTrigger; const APending: TNyxStudioPendingDesign;
  out AEdit: TNyxStudioDesignEdit): Boolean;
var
  LAction: TNyxStudioCollectionAction;
  LFound: Boolean;
  LKey: TNyxText;
  LFieldName: TNyxText;
  LName: TNyxText;
  LData: INyxCollectionSnapshot;
  LSchema: TNyxCollectionSchema;
  LField: TNyxCollectionField;
  LProjection: TNyxNode;
  LSpec: TNyxCollectionViewSpec;
  LIndex: Integer;
  LKind: TNyxStateKind;
begin
  Result := False;
  AEdit := Default(TNyxStudioDesignEdit);

  if (ANode = nil) or (ASession = nil) or (ANode.Prop(CCommandKey) = '') then
  begin
    Exit;
  end;
  LFound := False;
  for LAction := Low(TNyxStudioCollectionAction) to High(TNyxStudioCollectionAction) do
  begin

    if ANode.Prop(CCommandKey) = CCommands[LAction] then
    begin
      LFound := True;
      Break;
    end;
  end;

  if not LFound then
  begin
    raise ENyxCollection.Create('Unknown collection command');
  end;

  if LAction in [scaTableQuery, scaColumnQuery, scaDefaultTableQuery, scaDefaultColumnQuery,
    scaTableLayout, scaColumnLayout, scaDefaultTableLayout, scaDefaultColumnLayout,
    scaColumnFormat, scaDefaultColumnFormat, scaTableWorkbench, scaDefaultTableWorkbench,
    scaTablePresentation, scaDefaultTablePresentation, scaColumnText, scaDefaultColumnText,
    scaColumnAlignment, scaDefaultColumnAlignment] then
  begin
    raise ENyxCollection.Create('Use the current complete query or layout policy form');
  end;

  if ((LAction in [scaDefault, scaCell, scaScope, scaTitle, scaMode, scaSelection,
    scaColumnFixed, scaColumnFlex, scaColumnMinimum, scaColumnMaximum,
    scaPageSize, scaInitialPage]) and
    (AEvent <> ntChange)) or
    (not (LAction in [scaDefault, scaCell, scaScope, scaTitle, scaMode, scaSelection,
      scaColumnFixed, scaColumnFlex, scaColumnMinimum, scaColumnMaximum,
      scaPageSize, scaInitialPage]) and
    (AEvent <> ntClick)) then
  begin
    Exit;
  end;
  AEdit.Action := sdaCollection;
  AEdit.Collection.Action := LAction;
  LKey := ANode.Prop(CKey);

  if LAction = scaCreate then
  begin

    if APending.CollectionCreationPending then
    begin
      raise ENyxCollection.Create('Wait for collection creation before submitting this form again');
    end;
    LIndex := 1;
    repeat
      LKey := 'collection' + IntToStr(LIndex);
      Inc(LIndex);
    until not ASession.Document.Collections.Has(NyxCollection(LKey));
    AEdit.Collection.Key := NyxCollection(LKey);
    AEdit.Collection.Validate;
    Exit(True);
  end;
  AEdit.Collection.Key := NyxCollection(LKey);

  if APending.CollectionLocked(AEdit.Collection.Key) then
  begin
    raise ENyxCollection.Create('Wait for this collection structure change before editing it');
  end;
  LData := ASession.Document.Collections.Snapshot(AEdit.Collection.Key);
  LSchema := LData.Schema;
  LFieldName := ANode.Prop(CField);

  if LFieldName <> '' then
  begin
    LFound := False;
    for LIndex := 0 to LSchema.Count - 1 do
    begin
      LField := LSchema.FieldAt(LIndex);

      if LField.Name = LFieldName then
      begin

        if (ANode.Prop(CKind) <> '') and
          (ANode.Prop(CKind) <> NyxStateKindName(LField.Kind)) then
        begin
          raise ENyxCollection.Create('Mounted collection field changed its scalar family');
        end;
        AEdit.Collection.Field := TNyxStudioCollectionFieldRef.FromMetadata(LField);
        LFound := True;
        Break;
      end;
    end;

    if not LFound then
    begin
      raise ENyxCollection.Create('Collection field no longer exists');
    end;
  end;

  if LAction = scaAddField then
  begin
    LFound := False;
    for LKind := Low(TNyxStateKind) to High(TNyxStateKind) do
    begin

      if ANode.Prop(CKind) = NyxStateKindName(LKind) then
      begin
        AEdit.Collection.Kind := LKind;
        LFound := True;
        Break;
      end;
    end;

    if not LFound then
    begin
      raise ENyxCollection.Create('Unknown collection field family');
    end;
  end;

  if LAction = scaAddRow then
  begin
    LIndex := 1;
    repeat
      LName := 'row' + IntToStr(LIndex);
      Inc(LIndex);
    until LData.IndexOf(NyxItem(AEdit.Collection.Key, LName)) < 0;
    AEdit.Collection.Item := NyxItem(AEdit.Collection.Key, LName);
  end
  else if LAction in [scaCell, scaRemoveRow] then
  begin
    AEdit.Collection.Item := NyxItem(AEdit.Collection.Key, ANode.Prop(CRow));

    if LData.IndexOf(AEdit.Collection.Item) < 0 then
    begin
      raise ENyxCollection.Create('Collection row no longer exists');
    end;
  end;

  if LAction in [scaDefault, scaCell] then
  begin

    if not TryNyxStudioStateInput(ANode.Prop(NyxStudioStateInputKey), AEdit.Collection.Input) then
    begin
      raise ENyxCollection.Create('Unknown collection scalar notation');
    end;
    AEdit.Collection.Value := ANode.Prop('value');
  end
  else if LAction in [scaTitle, scaColumnFixed, scaColumnFlex, scaColumnMinimum, scaColumnMaximum,
    scaPageSize, scaInitialPage] then
  begin
    AEdit.Collection.Value := ANode.Prop('value');
  end;

  if NyxStudioCollectionViewAction(LAction) then
  begin

    if ANode.Prop(COwner) <> ASession.SelectedID then
    begin
      raise ENyxCollection.Create('Collection selection changed; use its current inspector');
    end;

    if APending.CollectionViewLocked(ASession.SelectedID) then
    begin
      raise ENyxCollection.Create('Wait for this collection view structure change before editing it');
    end;
    AEdit.Selection := ASession.SelectedID;
    AEdit.View := ASession.ActiveViewID;
    LProjection := ASession.SelectedProjection;
    try

      if LProjection = nil then
      begin
        raise ENyxCollection.Create('Select a list, table or tree');
      end;

      if LProjection.ProjectionKind = NyxKindName(nkList) then
      begin
        AEdit.Collection.Projection := cpList;
      end
      else if LProjection.ProjectionKind = NyxKindName(nkTable) then
      begin
        AEdit.Collection.Projection := cpTable;
      end
      else if LProjection.ProjectionKind = NyxKindName(nkTree) then
      begin
        AEdit.Collection.Projection := cpTree;
      end
      else
      begin
        raise ENyxCollection.Create('Selected control no longer supports collection views');
      end;

      LSpec := LProjection.CollectionView;

      if (LAction = scaInherit) and not LSpec.Defined then
      begin
        LSpec := ASession.ClearedCollectionInheritance(ASession.SelectedID);
      end;

      if (LAction <> scaBind) and
        (not LSpec.Defined or (LSpec.Key.Name <> LKey)) then
      begin
        raise ENyxCollection.Create('Collection binding changed; use its current inspector');
      end;
    finally
      LProjection.Free;
    end;
    case LAction of
      scaScope:
        begin

          if ANode.Prop('value') = 'Application' then
          begin
            AEdit.Collection.Scope := csApplication;
          end
          else if ANode.Prop('value') = 'Reusable instance' then
          begin
            AEdit.Collection.Scope := csInstance;
          end
          else
          begin
            raise ENyxCollection.Create('Unknown collection scope');
          end;
        end;
      scaSelection:
        begin

          if ANode.Prop('value') = 'Single item' then
          begin
            AEdit.Collection.SelectionMode := nsmSingle;
          end
          else if ANode.Prop('value') = 'Multiple items' then
          begin
            AEdit.Collection.SelectionMode := nsmMultiple;
          end
          else
          begin
            raise ENyxCollection.Create('Unknown collection selection mode');
          end;
        end;
      scaMode:
        begin

          if ANode.Prop('value') = 'Read only' then
          begin
            AEdit.Collection.Mode := cmReadOnly;
          end
          else if ANode.Prop('value') = 'Editable' then
          begin
            AEdit.Collection.Mode := cmEditable;
          end
          else
          begin
            raise ENyxCollection.Create('Unknown collection column editability');
          end;
        end;
    else
      begin
        { Other operations carry no extra closed view choice. }
      end;
    end;
  end;
  AEdit.Collection.Validate;

  if LAction in [scaColumnContent, scaColumnFixed, scaColumnFlex,
    scaColumnMinimum, scaColumnMaximum, scaColumnDefault, scaPageSize,
    scaInitialPage, scaDefaultPaging] then
  begin
    { Refuse an incomplete extent or inverted bound at the ordinary input
      boundary, where the adapter can restore the exact accepted field face.
      This detached policy check admits no document/source/history. The queued
      candidate still rechecks the current schema and binding independently. }
    ApplyNyxStudioCollectionViewIntent(AEdit.Collection, LSpec, LSchema);
  end;
  Result := True;
end;

function RouteNyxStudioCollection(ASession: TNyxStudioSession;
  ANode: TNyxNode; AEvent: TNyxTrigger): Boolean;
var
  LEdit: TNyxStudioDesignEdit;
begin
  Result := CaptureNyxStudioCollection(ASession, ANode, AEvent,
    Default(TNyxStudioPendingDesign), LEdit);

  if Result then
  begin
    ASession.ApplyCollectionIntent(LEdit.Collection);
  end;
end;

end.
