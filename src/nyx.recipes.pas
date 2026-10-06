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

unit nyx.recipes;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text,
  nyx.types,
  nyx.contract,
  nyx.state,
  nyx.schema,
  SysUtils,
  nyx.model,
  nyx.catalog;

{ Default compound recipes are ordinary owned Nyx trees. No DOM/LCL types appear
  here: every named part can be themed, replaced, moved or extended by consumers.
  Registration copies each template, keeping factory instances independent. }
procedure RegisterNyxRecipes(ACatalog: TNyxCatalog);

implementation

function Part(AKind: TNyxKind; const AName, AText: TNyxText): TNyxNode;
begin
  Result := TNyxNode.Create(AKind, AName).Configure
    .PartName(NyxPart(AName)).Text(AText).Done;
end;

function Stack(const AName: TNyxText): TNyxNode;
begin
  Result := Part(nkColumn, AName, '').Configure.Layout(nlColumn).Gap(10).Done;
end;

function Row(const AName: TNyxText): TNyxNode;
begin
  Result := Part(nkRow, AName, '').Configure.Layout(nlRow).Gap(10).Done;
end;

function Card(const AName: TNyxText): TNyxNode;
begin
  Result := Stack(AName).Configure.Surface(True).Padding(20).Done;
end;

function Action(const AName, AText: TNyxText; const AEvent: TNyxEventRef): TNyxNode;
begin
  Result := Part(nkButton, AName, AText).Configure.Variant(nvPrimary)
    .OnClick(AEvent).Done;
end;

procedure RegisterNyxRecipes(ACatalog: TNyxCatalog);
var
  LRecipe: TNyxNode;
  LGroup: TNyxNode;
  LIndex: Integer;

  procedure DeclareFields(ANode: TNyxNode; const APrefix: TNyxText);
  var
    LChildIndex: Integer;
    LChild: TNyxNode;
    LPath: TNyxText;
    LDomain: TNyxValueDomain;
    LMinimum: Integer;
    LMaximum: Integer;
  begin
    for LChildIndex := 0 to ANode.Count - 1 do
    begin
      LChild := ANode.Children[LChildIndex];
      LPath := LChild.Prop(NyxAttributeName(atPart));

      if LPath = '' then
      begin
        Continue;
      end;

      if APrefix <> '' then
      begin
        LPath := APrefix + '/' + LPath;
      end;
      LDomain := NyxNodeValueDomain(LChild);

      if LDomain.Defined then
      begin
        case LDomain.Kind of
          nskText:
            begin
              LRecipe.Contract.Field(NyxPart(LPath), NyxTextDomain);
            end;
          nskBoolean:
            begin
              LRecipe.Contract.Field(NyxPart(LPath), NyxBooleanDomain);
            end;
          nskInteger:
            begin
              LMinimum := 0;
              LMaximum := 100;
              TryNyxInteger(LChild.Prop('min', '0'), LMinimum);
              TryNyxInteger(LChild.Prop('max', '100'), LMaximum);
              LRecipe.Contract.Field(NyxPart(LPath),
                NyxIntegerDomain.Range(LMinimum, LMaximum));
            end;
          nskNumber:
            begin
              LRecipe.Contract.Field(NyxPart(LPath), NyxNumberDomain);
            end;
        end;
      end;
      DeclareFields(LChild, LPath);
    end;
  end;

  procedure Register(AKind: TNyxKind; const ATitle, ACategory: TNyxText);
  var
    LDomain: TNyxValueDomain;
  begin
    { The registry copies the template. Release our construction tree immediately
      after registration so later recipes cannot accidentally retain its parts. }
    try

      if not LRecipe.Contract.FindValue(LDomain) then
      begin
        LRecipe.Contract.NoValue;
      end;
      DeclareFields(LRecipe, '');
      ACatalog.RegisterRecipe(NyxKindName(AKind), ATitle, ACategory, LRecipe);
    finally
      LRecipe.Free;
      LRecipe := nil;
    end;
  end;

begin
  { Action compounds expose presentation and interaction as separate parts.
    'emit' names the semantic event applications receive from the shared behavior
    layer, while the inner control remains a normal keyboard-accessible button. }
  LRecipe := Row('labeled-button')
    .Add(Part(nkLabel, 'label', 'A useful action'))
    .Add(Action('button', 'Continue', NyxSemantic(nseActivate)));
  Register(nkLabeledButton, 'Labeled button', 'Compound actions');

  LRecipe := Row('split-button')
    .Add(Action('primary', 'Create', NyxSemantic(nsePrimary)))
    .Add(Action('menu', 'More...', NyxSemantic(nseMenu)));
  Register(nkSplitButton, 'Split button', 'Compound actions');

  LRecipe := Row('search-field')
    .Add(Part(nkInput, 'query', 'Search').Configure.Placeholder('Search records...')
      .Flex(1).Done)
    .Add(Action('search', 'Search', NyxSemantic(nseSearch)))
    .Add(Action('clear', 'Clear', NyxSemantic(nseClear)).Configure.Action(naClear).Target(NyxPart('query')).Done);
  LRecipe.Part(NyxPart('search')).Contract
    .On(ntClick, NyxPartValue(NyxPart('query')), NyxTextDomain);
  Register(nkSearchField, 'Search field', 'Compound inputs');

  LRecipe := Stack('form-field')
    .Add(Part(nkInput, 'input', 'Field label').Configure.Placeholder('Enter a value').Done)
    .Add(Part(nkLabel, 'help', 'Helpful guidance belongs near the field.'))
    .Add(Part(nkAlert, 'error', 'This field is required.').Configure.Visible(False).Done);
  Register(nkFormField, 'Field with help', 'Forms');

  LRecipe := Card('login-form')
    .Add(Part(nkHeading, 'title', 'Welcome back'))
    .Add(Part(nkInput, 'email', 'Email').Configure.Placeholder('you@example.com').Done)
    .Add(Part(nkInput, 'password', 'Password').Configure.InputType(niPassword).Done)
    .Add(Part(nkCheckbox, 'remember', 'Remember me'))
    .Add(Action('submit', 'Sign in', NyxSemantic(nseSubmit)));
  Register(nkLoginForm, 'Sign-in form', 'Forms');

  LRecipe := Card('settings-panel')
    .Add(Part(nkHeading, 'title', 'Preferences'))
    .Add(Part(nkSwitch, 'notifications', 'Enable notifications').Configure.Value(True).Done)
    .Add(Part(nkSwitch, 'sync', 'Sync across devices').Configure.Value(True).Done)
    .Add(Part(nkSelect, 'language', 'Language')
      .Configure.Items('English' + #10 + 'French' + #10 + 'Spanish').Done)
    .Add(Action('save', 'Save preferences', NyxSemantic(nseSave)));
  Register(nkSettingsPanel, 'Settings panel', 'Forms');

  LRecipe := Row('date-range')
    .Add(Part(nkDate, 'start', 'Start date'))
    .Add(Part(nkLabel, 'separator', 'to'))
    .Add(Part(nkDate, 'finish', 'End date'));
  Register(nkDateRange, 'Date range', 'Compound inputs');

  LRecipe := Row('number-stepper')
    .Add(Action('decrement', '-', NyxSemantic(nseDecrement)).Configure.Action(naDecrement).Target(NyxPart('value')).Done)
    .Add(Part(nkSpin, 'value', 'Quantity').Configure.Value(1).Minimum(0).Maximum(999).Done)
    .Add(Action('increment', '+', NyxSemantic(nseIncrement)).Configure.Action(naIncrement).Target(NyxPart('value')).Done);
  Register(nkNumberStepper, 'Number stepper', 'Compound inputs');

  LRecipe := Row('segmented-control');
  LRecipe.Contract.Value(NyxTextDomain.Choices(['day', 'week', 'month']));
  LRecipe.Configure.Value('day').Done;
  LRecipe.Add(Action('day', 'Day', NyxSemantic(nseSelect)).Configure.Action(naSelect).Option('day').Done);
  LRecipe.Add(Action('week', 'Week', NyxSemantic(nseSelect)).Configure.Action(naSelect).Option('week').Done);
  LRecipe.Add(Action('month', 'Month', NyxSemantic(nseSelect)).Configure.Action(naSelect).Option('month').Done);
  Register(nkSegmentedControl, 'Segmented control', 'Compound inputs');

  LRecipe := Row('rating');
  LRecipe.Contract.Value(NyxIntegerDomain.Range(1, 5));
  LRecipe.Configure.Value(3).Done;
  for LIndex := 1 to 5 do
  begin
    LRecipe.Add(Action('star-' + IntToStr(LIndex), IntToStr(LIndex), NyxSemantic(nseRate))
      .Configure.Action(naSelect).Option(LIndex).Done);
  end;
  Register(nkRating, 'Rating', 'Compound inputs');

  { Data/navigation compounds define useful slots rather than hardwiring a data
    service. Consumers bind the query/filter/selection parts to their own model. }
  LRecipe := Row('data-toolbar')
    .Add(Part(nkInput, 'search', 'Filter').Configure.Placeholder('Find a record...').Flex(1).Done)
    .Add(Part(nkSelect, 'sort', 'Sort by').Configure.Items('Newest' + #10 + 'Name' + #10 + 'Status').Done)
    .Add(Action('create', 'New record', NyxSemantic(nseCreate)))
    .Add(Action('refresh', 'Refresh', NyxSemantic(nseRefresh)));
  Register(nkDataToolbar, 'Data toolbar', 'Data compounds');

  LRecipe := Row('filter-bar')
    .Add(Part(nkSelect, 'status', 'Status').Configure.Items('All statuses' + #10 + 'Open' + #10 + 'Complete').Done)
    .Add(Part(nkInput, 'query', 'Contains'))
    .Add(Action('apply', 'Apply filters', NyxSemantic(nseApply)))
    .Add(Action('reset', 'Reset', NyxSemantic(nseReset)));
  Register(nkFilterBar, 'Filter bar', 'Data compounds');

  LRecipe := Row('pagination')
    .Add(Action('previous', 'Previous', NyxSemantic(nsePrevious)).Configure.Action(naDecrement).Target(NyxPart('page')).Done)
    .Add(Part(nkSpin, 'page', 'Page').Configure.Value(1).Minimum(1).Maximum(100).Done)
    .Add(Part(nkLabel, 'total', 'of 100'))
    .Add(Action('next', 'Next', NyxSemantic(nseNext)).Configure.Action(naIncrement).Target(NyxPart('page')).Done);
  Register(nkPagination, 'Pagination', 'Navigation compounds');

  LRecipe := Row('breadcrumbs')
    .Add(Action('home', 'Home', NyxSemantic(nseHome)))
    .Add(Part(nkLabel, 'separator', '/'))
    .Add(Action('section', 'Projects', NyxSemantic(nseSection)))
    .Add(Part(nkLabel, 'current', '/ Current project'));
  Register(nkBreadcrumbs, 'Breadcrumbs', 'Navigation compounds');

  LRecipe := Card('sidebar-nav')
    .Add(Part(nkHeading, 'title', 'Workspace'))
    .Add(Action('overview', 'Overview', NyxSemantic(nseOverview)))
    .Add(Action('projects', 'Projects', NyxSemantic(nseProjects)))
    .Add(Action('settings', 'Settings', NyxSemantic(nseSettings)));
  Register(nkSidebarNav, 'Sidebar navigation', 'Navigation compounds');

  LRecipe := Row('master-detail');
  LRecipe.Add(Part(nkList, 'master', '').Configure.Items('First record' + #10 + 'Second record')
    .Width(220).Done);
  LRecipe.Add(Card('detail').Configure.Flex(1).Done
    .Add(Part(nkHeading, 'title', 'Record details'))
    .Add(Part(nkLabel, 'description', 'Select a record to inspect its details.'))
    .Add(Action('edit', 'Edit record', NyxSemantic(nseEdit))));
  Register(nkMasterDetail, 'Master / detail', 'Data compounds');

  LRecipe := Card('list-card')
    .Add(Part(nkHeading, 'title', 'Recent activity'))
    .Add(Part(nkList, 'list', '').Configure.Items('New project created' + #10 + 'Design updated').Done)
    .Add(Row('actions').Add(Action('primary', 'View all', NyxSemantic(nseViewAll))));
  Register(nkListCard, 'List card', 'Data compounds');

  LRecipe := Card('data-card')
    .Add(Part(nkHeading, 'title', 'Project overview'))
    .Add(Part(nkTable, 'table', '').Configure.Items('Name' + #9 + 'Status' + #10 + 'Nyx' + #9 + 'In progress').Done)
    .Add(Row('actions').Add(Action('export', 'Export', NyxSemantic(nseExport))));
  Register(nkDataCard, 'Data card', 'Data compounds');

  LRecipe := Row('kanban-board').Configure.Gap(20).Done;
  for LIndex := 0 to 2 do
  begin
    LGroup := Card('lane-' + IntToStr(LIndex)).Configure.Flex(1).Done;
    LGroup.Add(Part(nkHeading, 'title', 'Stage ' + IntToStr(LIndex + 1)));
    LGroup.Add(Part(nkList, 'items', '').Configure.Items('Plan an idea' + #10 + 'Build a view').Done);
    LGroup.Add(Action('add', 'Add card', NyxSemantic(nseAdd)));
    LRecipe.Add(LGroup);
  end;
  Register(nkKanbanBoard, 'Kanban board', 'Data compounds');

  LRecipe := Stack('timeline');
  for LIndex := 1 to 3 do
  begin
    LRecipe.Add(Row('entry-' + IntToStr(LIndex))
      .Add(Part(nkBadge, 'time', 'Day ' + IntToStr(LIndex)))
      .Add(Stack('content').Add(Part(nkHeading, 'title', 'Milestone ' + IntToStr(LIndex)))
        .Add(Part(nkLabel, 'description', 'A meaningful step forward.'))));
  end;
  Register(nkTimeline, 'Timeline', 'Data compounds');

  { Feedback/media compounds retain named action slots, allowing a floating
    action or alternative renderer to be added without replacing the composition. }
  LRecipe := Card('stat-card')
    .Add(Part(nkLabel, 'label', 'Active projects'))
    .Add(Part(nkHeading, 'value', '128'))
    .Add(Part(nkBadge, 'trend', '+12% this month'));
  Register(nkStatCard, 'Statistic card', 'Dashboard compounds');

  LRecipe := TNyxNode.Create(nkGrid, 'metric-grid').Configure.Layout(nlGrid)
    .Columns(3).Gap(16).Done;
  for LIndex := 1 to 3 do
  begin
    LRecipe.Add(Card('metric-' + IntToStr(LIndex))
      .Add(Part(nkLabel, 'label', 'Metric ' + IntToStr(LIndex)))
      .Add(Part(nkHeading, 'value', IntToStr(LIndex * 42)))
      .Add(Part(nkProgress, 'progress', 'Progress').Configure.Value(LIndex * 25).Done));
  end;
  Register(nkMetricGrid, 'Metric grid', 'Dashboard compounds');

  LRecipe := Card('empty-state')
    .Add(Part(nkAvatar, 'illustration', '+'))
    .Add(Part(nkHeading, 'title', 'Nothing here yet'))
    .Add(Part(nkLabel, 'description', 'Create your first item to get started.'))
    .Add(Action('create', 'Create an item', NyxSemantic(nseCreate)));
  Register(nkEmptyState, 'Empty state', 'Feedback compounds');

  LRecipe := Card('confirmation-dialog')
    .Add(Part(nkHeading, 'title', 'Continue with this action?'))
    .Add(Part(nkLabel, 'description', 'Review the change before confirming.'))
    .Add(Row('actions').Configure.Wrap(nfwWrap).Done
      .Add(Action('cancel', 'Cancel', NyxSemantic(nseCancel)))
      .Add(Action('confirm', 'Confirm', NyxSemantic(nseConfirm))));
  Register(nkConfirmationDialog, 'Confirmation panel', 'Feedback compounds');

  LRecipe := Row('notification-card').Configure.Surface(True).Padding(16).Done
    .Add(Part(nkBadge, 'status', 'NEW'))
    .Add(Stack('content').Configure.Flex(1).Done
      .Add(Part(nkHeading, 'title', 'Your build is ready'))
      .Add(Part(nkLabel, 'message', 'Everything compiled successfully.')))
    .Add(Action('dismiss', 'Dismiss', NyxSemantic(nseDismiss)).Configure.Action(naDismiss).Done);
  Register(nkNotificationCard, 'Notification card', 'Feedback compounds');

  LRecipe := Row('toast').Configure.Surface(True).Padding(14).Done
    .Add(Part(nkLabel, 'message', 'Changes saved'))
    .Add(Action('undo', 'Undo', NyxSemantic(nseUndo)))
    .Add(Action('dismiss', 'Close', NyxSemantic(nseDismiss)).Configure.Action(naDismiss).Done);
  Register(nkToast, 'Toast', 'Feedback compounds');

  LRecipe := Card('wizard-step')
    .Add(Part(nkBadge, 'step', 'STEP 1 OF 3'))
    .Add(Part(nkHeading, 'title', 'Tell us about your project'))
    .Add(Stack('content').Add(Part(nkInput, 'name', 'Project name')))
    .Add(Row('navigation').Add(Action('back', 'Back', NyxSemantic(nseBack)))
      .Add(Action('next', 'Continue', NyxSemantic(nseNext))));
  Register(nkWizardStep, 'Wizard step', 'Navigation compounds');

  LRecipe := Row('stepper');
  LRecipe.Contract.Value(NyxIntegerDomain.Range(1, 4));
  LRecipe.Configure.Value(1).Done;
  for LIndex := 1 to 4 do
  begin
    LRecipe.Add(Action('step-' + IntToStr(LIndex), 'Step ' + IntToStr(LIndex), NyxSemantic(nseStep))
      .Configure.Action(naSelect).Option(LIndex).Done);
  end;
  Register(nkStepper, 'Step indicator', 'Navigation compounds');

  LRecipe := Card('profile-card')
    .Add(Part(nkAvatar, 'avatar', 'NY'))
    .Add(Part(nkHeading, 'name', 'Nyx developer'))
    .Add(Part(nkLabel, 'bio', 'Making delightful Pascal applications.'))
    .Add(Row('actions').Add(Action('message', 'Message', NyxSemantic(nseMessage)))
      .Add(Action('follow', 'Follow', NyxSemantic(nseFollow))));
  Register(nkProfileCard, 'Profile card', 'Media compounds');

  LRecipe := Card('media-card')
    .Add(Part(nkImage, 'media', 'Project cover').Configure.Height(160).Done)
    .Add(Part(nkHeading, 'title', 'A beautiful project'))
    .Add(Part(nkLabel, 'description', 'Built with a fluent Pascal contract.'))
    .Add(Row('actions').Add(Action('open', 'Open project', NyxSemantic(nseOpen)))
      .Add(Action('favorite', 'Favorite', NyxSemantic(nseFavorite))));
  Register(nkMediaCard, 'Media card', 'Media compounds');

  LRecipe := Row('avatar-group');
  for LIndex := 1 to 4 do
  begin
    LRecipe.Add(Part(nkAvatar, 'member-' + IntToStr(LIndex), 'N' + IntToStr(LIndex)));
  end;
  LRecipe.Add(Part(nkLabel, 'overflow', '+8 more'));
  Register(nkAvatarGroup, 'Avatar group', 'Media compounds');

  LRecipe := Card('comment-thread');
  for LIndex := 1 to 2 do
  begin
    LRecipe.Add(Row('comment-' + IntToStr(LIndex))
      .Add(Part(nkAvatar, 'author', 'NY'))
      .Add(Stack('body').Add(Part(nkLabel, 'name', 'A Pascal developer'))
        .Add(Part(nkLabel, 'message', 'This component is yours to reshape.'))));
  end;
  LRecipe.Add(Part(nkMemo, 'reply', 'Write a reply')).Add(Action('send', 'Post reply', NyxSemantic(nseReply)));
  Register(nkCommentThread, 'Comment thread', 'Media compounds');

  LRecipe := Card('property-grid');
  for LIndex := 1 to 3 do
  begin
    LRecipe.Add(Row('property-' + IntToStr(LIndex))
      .Add(Part(nkLabel, 'name', 'Property ' + IntToStr(LIndex)).Configure.Width(120).Done)
      .Add(Part(nkInput, 'value', '').Configure.Flex(1).Done));
  end;
  Register(nkPropertyGrid, 'Property grid', 'Forms');

  LRecipe := Row('command-bar')
    .Add(Action('new', 'New', NyxSemantic(nseNew)))
    .Add(Action('open', 'Open', NyxSemantic(nseOpen)))
    .Add(Action('save', 'Save', NyxSemantic(nseSave)))
    .Add(Part(nkSpacer, 'spacer', ''))
    .Add(Part(nkInput, 'command', 'Command').Configure.Placeholder('Find an action...').Done);
  Register(nkCommandBar, 'Command bar', 'Compound actions');

  LRecipe := Stack('floating-action-panel')
    .Add(Stack('content').Add(Part(nkList, 'list', '').Configure.Items('First item' + #10 + 'Second item').Done))
    .Add(Row('actions').Add(Action('floating', '+ Add', NyxSemantic(nseAdd))));
  Register(nkFloatingActionPanel, 'List with floating action', 'Data compounds');
end;

end.
