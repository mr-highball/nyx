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


{ Maintained by tools/nyx_control_contracts.lpr; edit the contract source, then regenerate. }
{ Own a managed page. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxPage(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxPage;

{ Own a managed column. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxColumn(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxColumn;

{ Own a managed row. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxRow(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxRow;

{ Own a managed grid. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxGrid(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxGrid;

{ Own a managed panel. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxPanel(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxPanel;

{ Own a managed card. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxCard(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxCard;

{ Own a managed group. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxGroup(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxGroup;

{ Own a managed toolbar. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxToolbar(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxToolbar;

{ Own a managed scroll. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxScroll(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxScroll;

{ Own a managed tabs. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxTabs(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxTabs;

{ Own a managed tab. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxTab(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxTab;

{ Own a managed heading. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxHeading(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxHeading;

{ Own a managed label. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxLabel(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxLabel;

{ Own a managed button. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxButton(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxButton;

{ Own a managed link. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxLink(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxLink;

{ Own a managed input. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxInput(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxInput;

{ Own a managed memo. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxMemo(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxMemo;

{ Own a managed checkbox. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxCheckbox(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxCheckbox;

{ Own a managed switch. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxSwitch(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxSwitch;

{ Own a managed radio. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxRadio(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxRadio;

{ Own a managed select. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxSelect(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxSelect;

{ Own a managed spin. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxSpin(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxSpin;

{ Own a managed slider. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxSlider(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxSlider;

{ Own a managed date. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxDate(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxDate;

{ Own a managed time. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxTime(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxTime;

{ Own a managed color. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxColor(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxColor;

{ Own a managed list. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxList(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxList;

{ Own a managed table. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxTable(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxTable;

{ Own a managed tree. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxTree(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxTree;

{ Own a managed image. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxImage(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxImage;

{ Own a managed avatar. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxAvatar(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxAvatar;

{ Own a managed progress. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxProgress(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxProgress;

{ Own a managed badge. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxBadge(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxBadge;

{ Own a managed alert. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxAlert(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxAlert;

{ Own a managed separator. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxSeparator(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxSeparator;

{ Own a managed spacer. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxSpacer(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxSpacer;

{ Own a managed code. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxCode(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxCode;

{ Own a managed code-editor. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxCodeEditor(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxCodeEditor;

{ Own a managed design-surface. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxDesignSurface(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxDesignSurface;

{ Own a managed component. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxComponent(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxComponent;

{ Own a managed split-view. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxSplitView(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxSplitView;

{ Own a managed labeled-button. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxLabeledButton(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxLabeledButton;

{ Own a managed split-button. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxSplitButton(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxSplitButton;

{ Own a managed search-field. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxSearchField(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxSearchField;

{ Own a managed form-field. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxFormField(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxFormField;

{ Own a managed login-form. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxLoginForm(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxLoginForm;

{ Own a managed settings-panel. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxSettingsPanel(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxSettingsPanel;

{ Own a managed date-range. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxDateRange(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxDateRange;

{ Own a managed number-stepper. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxNumberStepper(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxNumberStepper;

{ Own a managed segmented-control. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxSegmentedControl(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxSegmentedControl;

{ Own a managed rating. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxRating(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxRating;

{ Own a managed data-toolbar. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxDataToolbar(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxDataToolbar;

{ Own a managed filter-bar. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxFilterBar(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxFilterBar;

{ Own a managed pagination. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxPagination(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxPagination;

{ Own a managed breadcrumbs. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxBreadcrumbs(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxBreadcrumbs;

{ Own a managed sidebar-nav. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxSidebarNav(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxSidebarNav;

{ Own a managed master-detail. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxMasterDetail(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxMasterDetail;

{ Own a managed list-card. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxListCard(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxListCard;

{ Own a managed data-card. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxDataCard(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxDataCard;

{ Own a managed kanban-board. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxKanbanBoard(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxKanbanBoard;

{ Own a managed timeline. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxTimeline(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxTimeline;

{ Own a managed stat-card. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxStatCard(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxStatCard;

{ Own a managed metric-grid. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxMetricGrid(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxMetricGrid;

{ Own a managed empty-state. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxEmptyState(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxEmptyState;

{ Own a managed confirmation-dialog. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxConfirmationDialog(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxConfirmationDialog;

{ Own a managed notification-card. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxNotificationCard(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxNotificationCard;

{ Own a managed toast. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxToast(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxToast;

{ Own a managed wizard-step. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxWizardStep(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxWizardStep;

{ Own a managed stepper. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxStepper(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxStepper;

{ Own a managed profile-card. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxProfileCard(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxProfileCard;

{ Own a managed media-card. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxMediaCard(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxMediaCard;

{ Own a managed avatar-group. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxAvatarGroup(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxAvatarGroup;

{ Own a managed comment-thread. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxCommentThread(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxCommentThread;

{ Own a managed property-grid. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxPropertyGrid(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxPropertyGrid;

{ Own a managed command-bar. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxCommandBar(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxCommandBar;

{ Own a managed floating-action-panel. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxFloatingActionPanel(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxFloatingActionPanel;

{ Own a managed slot-override. Default compounds include independently owned recipe parts;
  ncoDescriptor constructs an empty exact descriptor for source reconstruction. }
function NewNyxSlotOverride(const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxSlotOverride;
