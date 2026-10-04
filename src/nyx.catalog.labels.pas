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

unit nyx.catalog.labels;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  SysUtils,
  nyx.text,
  nyx.types;

type
  { One intent group gives each palette entry a single home. pgAll is a filter,
    never a component's home. This vocabulary does not restrict extensible kind
    names or project behavior; it describes how a developer finds a control. }
  TNyxPaletteGroup = (pgAll, pgLayout, pgText, pgInputs, pgActions,
    pgNavigation, pgData, pgMedia, pgFeedback, pgForms, pgAuthoring,
    pgComposition, pgOther);

  { Cross-cutting intent labels supplement the home group. Avoid tagging every
    composite with all of its internal parts: a search field is useful for text
    and search, rather than matching every layout, label and action query. }
  TNyxComponentLabel = (clText, clMultiline, clChoice, clNumeric, clDateTime,
    clColor, clSearch, clFilter, clAccount, clMessaging, clRecords,
    clHierarchy, clStatus, clProgress, clConfirmation, clEditor, clReusable,
    clCompound);
  TNyxComponentLabels = set of TNyxComponentLabel;

  { Immutable-by-copy metadata: no borrowed template or renderer handles.
    Aliases are searchable descriptive words supplied by an extension author.
    Description explains the intended use at a high level; palette details,
    tooltips and contextual help display this same author text. Both are help
    data and never choose runtime behavior or alter generated application source. }
  TNyxComponentDiscovery = record
    Group: TNyxPaletteGroup;
    Labels: TNyxComponentLabels;
    Aliases: TNyxText;
    Description: TNyxText;
  end;

{ Names are human captions; keys are stable presentation/adapter identifiers.
  Try accepts only keys and captions, and leaves AGroup unchanged on failure. }
function NyxPaletteGroupName(AGroup: TNyxPaletteGroup): TNyxText;
function NyxPaletteGroupKey(AGroup: TNyxPaletteGroup): TNyxText;
function TryNyxPaletteGroup(const AValue: TNyxText; out AGroup: TNyxPaletteGroup): Boolean;
function NyxComponentLabelName(ALabel: TNyxComponentLabel): TNyxText;
{ Comma-separated captions suitable for accessible hints and catalog references. }
function NyxComponentLabelNames(ALabels: TNyxComponentLabels): TNyxText;
{ Explicit default intent metadata. Unknown extension kinds must supply their own
  metadata in the catalog; membership in a default kind is never guessed. }
function NyxDefaultComponentDiscovery(AKind: TNyxKind): TNyxComponentDiscovery;
{ Additional everyday search terms for the small shared label vocabulary. }
function NyxComponentLabelAliases(ALabel: TNyxComponentLabel): TNyxText;

implementation

const
  CGroupNames: array[TNyxPaletteGroup] of TNyxText = ('All groups', 'Layout',
    'Text & display', 'Inputs', 'Actions', 'Navigation', 'Data', 'Media',
    'Feedback', 'Forms', 'Authoring', 'Composition', 'Other');
  CGroupKeys: array[TNyxPaletteGroup] of TNyxText = ('all', 'layout', 'text',
    'inputs', 'actions', 'navigation', 'data', 'media', 'feedback', 'forms',
    'authoring', 'composition', 'other');
  CLabelNames: array[TNyxComponentLabel] of TNyxText = ('Text', 'Multiline',
    'Choice', 'Numeric', 'Date & time', 'Color', 'Search', 'Filter', 'Account',
    'Messaging', 'Records', 'Hierarchy', 'Status', 'Progress', 'Confirmation',
    'Editor', 'Reusable', 'Compound');
  CLabelAliases: array[TNyxComponentLabel] of TNyxText = ('string',
    'textarea paragraph', 'selection picker options', 'number integer decimal',
    'calendar clock date time', 'colour palette', 'find query', 'filtering',
    'user identity authentication', 'conversation comment reply chat',
    'record collection dataset', 'nested tree hierarchy', 'state indicator',
    'loading completion', 'confirm dialog', 'editing source',
    'reuse template instance', 'composite composed');

function NyxPaletteGroupName(AGroup: TNyxPaletteGroup): TNyxText;
begin
  Result := CGroupNames[AGroup];
end;

function NyxPaletteGroupKey(AGroup: TNyxPaletteGroup): TNyxText;
begin
  Result := CGroupKeys[AGroup];
end;

function TryNyxPaletteGroup(const AValue: TNyxText; out AGroup: TNyxPaletteGroup): Boolean;
var
  LGroup: TNyxPaletteGroup;
  LValue: TNyxText;
begin
  LValue := LowerCase(Trim(AValue));
  Result := False;
  for LGroup := Low(TNyxPaletteGroup) to High(TNyxPaletteGroup) do
  begin

    if (LValue = CGroupKeys[LGroup]) or (LValue = LowerCase(CGroupNames[LGroup])) then
    begin
      AGroup := LGroup;
      Exit(True);
    end;
  end;
end;

function NyxComponentLabelName(ALabel: TNyxComponentLabel): TNyxText;
begin
  Result := CLabelNames[ALabel];
end;

function NyxComponentLabelNames(ALabels: TNyxComponentLabels): TNyxText;
var
  LLabel: TNyxComponentLabel;
begin
  Result := '';
  for LLabel := Low(TNyxComponentLabel) to High(TNyxComponentLabel) do
  begin

    if LLabel in ALabels then
    begin

      if Result <> '' then
      begin
        Result := Result + ', ';
      end;
      Result := Result + CLabelNames[LLabel];
    end;
  end;
end;

function NyxComponentLabelAliases(ALabel: TNyxComponentLabel): TNyxText;
begin
  Result := CLabelAliases[ALabel];
end;

function NyxDefaultComponentDiscovery(AKind: TNyxKind): TNyxComponentDiscovery;
begin
  Result.Group := pgOther;
  Result.Labels := [];
  Result.Aliases := '';
  Result.Description := '';
  case AKind of
    nkPage, nkColumn, nkRow, nkGrid, nkPanel, nkCard, nkGroup, nkScroll, nkSpacer, nkSplitView:
      Result.Group := pgLayout;
    nkHeading, nkLabel, nkSeparator, nkCode:
      Result.Group := pgText;
    nkInput, nkMemo, nkCheckbox, nkSwitch, nkRadio, nkSelect, nkSpin, nkSlider,
      nkDate, nkTime, nkColor, nkSearchField, nkDateRange, nkNumberStepper,
      nkSegmentedControl, nkRating:
      Result.Group := pgInputs;
    nkButton, nkLink, nkLabeledButton, nkSplitButton, nkCommandBar:
      Result.Group := pgActions;
    nkToolbar, nkTabs, nkTab, nkPagination, nkBreadcrumbs, nkSidebarNav,
      nkWizardStep, nkStepper:
      Result.Group := pgNavigation;
    nkList, nkTable, nkTree, nkDataToolbar, nkFilterBar, nkMasterDetail,
      nkListCard, nkDataCard, nkKanbanBoard, nkTimeline, nkStatCard, nkMetricGrid,
      nkFloatingActionPanel:
      Result.Group := pgData;
    nkImage, nkAvatar, nkProfileCard, nkMediaCard, nkAvatarGroup, nkCommentThread:
      Result.Group := pgMedia;
    nkProgress, nkBadge, nkAlert, nkEmptyState, nkConfirmationDialog,
      nkNotificationCard, nkToast:
      Result.Group := pgFeedback;
    nkFormField, nkLoginForm, nkSettingsPanel, nkPropertyGrid:
      Result.Group := pgForms;
    nkCodeEditor, nkDesignSurface:
      Result.Group := pgAuthoring;
    nkComponent, nkSlotOverride:
      Result.Group := pgComposition;
  end;
  { A compound is a useful explicit search facet, not its primary purpose. }

  if (AKind >= nkLabeledButton) and (AKind <= nkFloatingActionPanel) then
  begin
    Include(Result.Labels, clCompound);
  end;
  case AKind of
    nkHeading, nkLabel, nkCode, nkInput, nkMemo, nkSearchField, nkFormField,
      nkCodeEditor, nkCommentThread:
      Include(Result.Labels, clText);
  end;
  case AKind of
    nkMemo, nkCode, nkCodeEditor, nkCommentThread:
      Include(Result.Labels, clMultiline);
  end;
  case AKind of
    nkCheckbox, nkSwitch, nkRadio, nkSelect, nkColor, nkSegmentedControl, nkRating:
      Include(Result.Labels, clChoice);
  end;
  case AKind of
    nkSpin, nkSlider, nkNumberStepper, nkRating, nkStatCard, nkMetricGrid:
      Include(Result.Labels, clNumeric);
    nkDate, nkTime, nkDateRange: Include(Result.Labels, clDateTime);
    nkColor: Include(Result.Labels, clColor);
  end;
  case AKind of
    nkSearchField, nkDataToolbar: Include(Result.Labels, clSearch);
    nkFilterBar: Include(Result.Labels, clFilter);
  end;
  case AKind of
    nkDataToolbar, nkFilterBar, nkList, nkTable, nkTree, nkMasterDetail,
      nkListCard, nkDataCard, nkKanbanBoard, nkTimeline, nkPagination,
      nkStatCard, nkMetricGrid, nkFloatingActionPanel:
      Include(Result.Labels, clRecords);
  end;
  case AKind of
    nkLoginForm, nkProfileCard, nkAvatar, nkAvatarGroup:
      Include(Result.Labels, clAccount);
    nkCommentThread: Include(Result.Labels, clMessaging);
  end;
  case AKind of
    nkTree, nkBreadcrumbs, nkSidebarNav: Include(Result.Labels, clHierarchy);
    nkProgress, nkStepper, nkWizardStep: Include(Result.Labels, clProgress);
  end;
  case AKind of
    nkProgress, nkBadge, nkAlert, nkEmptyState, nkNotificationCard, nkToast:
      Include(Result.Labels, clStatus);
    nkConfirmationDialog: Include(Result.Labels, clConfirmation);
    nkCodeEditor, nkDesignSurface, nkPropertyGrid: Include(Result.Labels, clEditor);
    nkComponent, nkSlotOverride: Include(Result.Labels, clReusable);
  end;
  case AKind of
    nkMemo: Result.Aliases := 'memo text area';
    nkInput: Result.Aliases := 'input textbox text box';
    nkLabel: Result.Aliases := 'label caption';
    nkGroup: Result.Aliases := 'groupbox fieldset';
    nkSwitch: Result.Aliases := 'toggle boolean';
    nkCheckbox: Result.Aliases := 'boolean check box';
    nkSelect: Result.Aliases := 'dropdown combo box';
    nkSpin: Result.Aliases := 'spinbox spin edit';
    nkScroll: Result.Aliases := 'scrolling overflow';
    nkSplitView: Result.Aliases := 'splitter resize divider panes workspace';
    nkStatCard, nkMetricGrid: Result.Aliases := 'dashboard statistics metrics';
    nkKanbanBoard: Result.Aliases := 'board lanes workflow';
    nkConfirmationDialog: Result.Aliases := 'dialog modal confirmation';
    nkToast: Result.Aliases := 'notification snackbar';
    nkLoginForm: Result.Aliases := 'login sign in password email';
    nkSettingsPanel: Result.Aliases := 'preferences configuration';
    nkFloatingActionPanel: Result.Aliases := 'floating action button';
  end;
  { Descriptions explain intent without promising unsupported target features.
    They are shared catalog data, so search, tooltips and future help surfaces
    receive the same explanation rather than maintaining browser-only copy. }
  case AKind of
    nkPage: Result.Description := 'A top-level application view containing a layout and controls.';
    nkColumn: Result.Description := 'Arrange child controls vertically with consistent spacing.';
    nkRow: Result.Description := 'Arrange child controls horizontally, wrapping when space is limited.';
    nkGrid: Result.Description := 'Arrange child controls in a configurable number of columns.';
    nkPanel: Result.Description := 'A general container for grouping and arranging related controls.';
    nkCard: Result.Description := 'Present related content and actions on a distinct surface.';
    nkGroup: Result.Description := 'Group related controls inside a captioned section.';
    nkToolbar: Result.Description := 'Group commands and navigation controls in a horizontal bar.';
    nkScroll: Result.Description := 'Contain content that needs its own scrolling area.';
    nkSplitView: Result.Description := 'Divide a workspace into two panes with a bounded draggable and keyboard-accessible divider.';
    nkTabs: Result.Description := 'Organize related content into selectable tab pages.';
    nkTab: Result.Description := 'A named content section inside a tabs control.';
    nkHeading: Result.Description := 'Introduce a page or content section with prominent text.';
    nkLabel: Result.Description := 'Display a caption, explanation or short piece of text.';
    nkButton: Result.Description := 'Let a user invoke a command or registered callback.';
    nkLink: Result.Description := 'Offer a text action or link to another location.';
    nkInput: Result.Description := 'Collect a single line of text, with optional placeholder and input type.';
    nkMemo: Result.Description := 'Collect or edit multiline text, such as a description or reply.';
    nkCheckbox: Result.Description := 'Let a user turn an independent Boolean option on or off.';
    nkSwitch: Result.Description := 'Present an on/off setting as a toggle.';
    nkRadio: Result.Description := 'Present a selectable option alongside related choices.';
    nkSelect: Result.Description := 'Let a user choose one item from a dropdown list.';
    nkSpin: Result.Description := 'Edit a numeric value with minimum and maximum bounds.';
    nkSlider: Result.Description := 'Choose a numeric value along a bounded range.';
    nkDate: Result.Description := 'Collect a calendar date.';
    nkTime: Result.Description := 'Collect a time of day.';
    nkColor: Result.Description := 'Let a user choose a color value.';
    nkList: Result.Description := 'Display a collection of items for selection.';
    nkTable: Result.Description := 'Display records in rows and columns.';
    nkTree: Result.Description := 'Display nested items in a hierarchy.';
    nkImage: Result.Description := 'Display an image with an accessible text alternative.';
    nkAvatar: Result.Description := 'Represent a person or account with compact initials or imagery.';
    nkProgress: Result.Description := 'Show progress toward completion of a bounded task.';
    nkBadge: Result.Description := 'Emphasize a short status, count or category near other content.';
    nkAlert: Result.Description := 'Present a visible message that deserves attention.';
    nkSeparator: Result.Description := 'Visually divide neighboring content sections.';
    nkSpacer: Result.Description := 'Reserve flexible space between layout elements.';
    nkCode: Result.Description := 'Display a block of preformatted source or technical text.';
    nkCodeEditor: Result.Description := 'Edit source text with selection and source-line navigation.';
    nkDesignSurface: Result.Description := 'Host an editable or interactive Nyx view inside an authoring tool.';
    nkComponent: Result.Description := 'Instantiate a reusable component definition with independent overrides.';
    nkLabeledButton: Result.Description := 'Pair a descriptive label with a button as one reusable action.';
    nkSplitButton: Result.Description := 'Pair a primary action with a separate secondary action.';
    nkSearchField: Result.Description := 'Collect a search query with explicit search and clear actions.';
    nkFormField: Result.Description := 'Combine a text field with nearby guidance and validation feedback.';
    nkLoginForm: Result.Description := 'Compose account sign-in fields, a remember option and a submit action.';
    nkSettingsPanel: Result.Description := 'Compose related preference controls and a save action.';
    nkDateRange: Result.Description := 'Collect start and end dates as a related pair.';
    nkNumberStepper: Result.Description := 'Combine a bounded number field with increment and decrement actions.';
    nkSegmentedControl: Result.Description := 'Choose one value using a row of related action buttons.';
    nkRating: Result.Description := 'Choose a bounded rating using a series of numbered actions.';
    nkDataToolbar: Result.Description := 'Compose record search, sorting, creation and refresh controls.';
    nkFilterBar: Result.Description := 'Compose filter choices and query text with apply and reset actions.';
    nkPagination: Result.Description := 'Navigate a numbered series of result pages.';
    nkBreadcrumbs: Result.Description := 'Show a navigation path with actions for earlier locations.';
    nkSidebarNav: Result.Description := 'Organize workspace destinations in a vertical navigation section.';
    nkMasterDetail: Result.Description := 'Place a selectable record list beside its detail content.';
    nkListCard: Result.Description := 'Present a titled item collection and related actions on a card.';
    nkDataCard: Result.Description := 'Present a titled record table and related actions on a card.';
    nkKanbanBoard: Result.Description := 'Organize work items into editable lanes representing workflow stages.';
    nkTimeline: Result.Description := 'Present a sequence of milestones with time labels and descriptions.';
    nkStatCard: Result.Description := 'Emphasize one numeric statistic with supporting context.';
    nkMetricGrid: Result.Description := 'Arrange related statistic cards into a dashboard grid.';
    nkEmptyState: Result.Description := 'Explain an empty collection and offer a useful next action.';
    nkConfirmationDialog: Result.Description := 'Compose a confirmation message with explicit accept and cancel actions.';
    nkNotificationCard: Result.Description := 'Present a notification message with related actions.';
    nkToast: Result.Description := 'Present a compact notification message and a dismiss action.';
    nkWizardStep: Result.Description := 'Compose one guided step with content and navigation actions.';
    nkStepper: Result.Description := 'Show the current position in a sequence of steps.';
    nkProfileCard: Result.Description := 'Present account identity, descriptive text and profile actions.';
    nkMediaCard: Result.Description := 'Present an image with a title, explanation and related action.';
    nkAvatarGroup: Result.Description := 'Present several account avatars together.';
    nkCommentThread: Result.Description := 'Compose conversation entries with authors, messages and a reply editor.';
    nkPropertyGrid: Result.Description := 'Compose labeled setting editors in a property-oriented form.';
    nkCommandBar: Result.Description := 'Group application commands into a reusable action bar.';
    nkFloatingActionPanel: Result.Description := 'Compose a record list with a nearby prominent create action.';
    nkSlotOverride: Result.Description := 'Customize a named part of a reusable component instance.';
  end;
end;

end.
