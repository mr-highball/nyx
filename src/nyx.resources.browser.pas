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
unit nyx.resources.browser;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.data, nyx.resources, nyx.resources.catalog, nyx.model, nyx.controls;

type
  TNyxResourceBrowserMode = (rbmCompact, rbmWorkspace);
  TNyxResourceBrowserField = (rbfSearch, rbfSources, rbfLocales, rbfLabelMatch);
  TNyxResourceBrowserAction = (rbaOpen, rbaReset, rbaFilters);
  { Disclosure changes presentation only. Collapsed fields remain owned and
    their predicates continue to filter; opening them never chooses a resource. }
  TNyxResourceFilterDisclosure = (rfdCollapsed, rfdExpanded);

  { Independent project presentation. The query contains accepted tag names;
    incomplete tag text and its selected member belong to the filter editor,
    not to any resource. No catalog, rows, payload, widget or controller survives
    in this copied state. ToData/FromData admit closed choices and query budgets. }
  TNyxResourceBrowserState = record
    Query: TNyxResourceCatalogQuery;
    TagInput: TNyxText;
    TagSelection: TNyxResourceLabelRef;
    FilterDisclosure: TNyxResourceFilterDisclosure;
    { Fluent copied presentation; no DOM, widgetset or viewport is required. }
    function Filters(AValue: TNyxResourceFilterDisclosure): TNyxResourceBrowserState;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxResourceBrowserState; static;
  end;

function NyxResourceBrowserState: TNyxResourceBrowserState;
{ The card owns fixed ordinary Nyx fields and a list. Mount that list through
  the public target collection contract; no resource buttons or payload copies
  are invented here. Compact shares its full query but discloses only Search.
  Hosts own query publication, row selection, open intent and synchronization. }
function NewNyxResourceBrowser(const AID: TNyxText; const AState: TNyxResourceBrowserState;
  AMode: TNyxResourceBrowserMode = rbmWorkspace): INyxCard;
function NyxResourceBrowserListID(const AID: TNyxText): TNyxText;
function NyxResourceBrowserTagsID(const AID: TNyxText): TNyxText;
function NyxResourceBrowserFiltersID(const AID: TNyxText): TNyxText;
function NyxResourceBrowserFieldID(const AID: TNyxText;
  AField: TNyxResourceBrowserField): TNyxText;
function NyxResourceBrowserKindID(const AID: TNyxText; AKind: TNyxResourceKind): TNyxText;
function NyxResourceBrowserActionID(const AID: TNyxText;
  AAction: TNyxResourceBrowserAction): TNyxText;
{ Read exact owned proposal fields. Unknown closed choices refuse. Restore
  validates the complete state before writing, retaining descendant identities
  and caller styling. Neither operation changes resources or history. }
function ReadNyxResourceBrowser(AEditor: TNyxNode): TNyxResourceBrowserState;
procedure RestoreNyxResourceBrowser(AEditor: TNyxNode; const AState: TNyxResourceBrowserState);
function NyxResourceBrowserInput(ANode, AEditor: TNyxNode): Boolean;
{ Prepare disclosure, Reset or Add/Remove filter tags without writing live fields.
  Disclosure retains the complete query; hosts must not republish it as data.
  Tag changes use a detached clone. Unrelated actions
  return False. Invalid names or over-budget composed queries leave every mounted
  property unchanged. Publish the candidate query, then Restore and synchronize.
  Open remains a host intent; this compound never chooses a resource itself. }
function PrepareNyxResourceBrowserAction(AButton, AEditor: TNyxNode;
  out AState: TNyxResourceBrowserState): Boolean;

implementation

uses SysUtils, nyx.types, nyx.layout.policy, nyx.resources.labels.editor;

const
  CState = 'nyx.resource-browser.state';
  CMode = 'nyx.resource-browser.mode';
  CFields: array[TNyxResourceBrowserField] of TNyxText = ('search', 'sources', 'locales', 'label-match');
  CActions: array[TNyxResourceBrowserAction] of TNyxText = ('open', 'reset', 'filters');
  CKinds: array[TNyxResourceKind] of TNyxText = ('Images', 'JSON', 'Text', 'Data files');
  CSources: array[TNyxResourceCatalogSources] of TNyxText = ('Any source', 'Embedded', 'Hosted');
  CLocales: array[TNyxResourceCatalogLocales] of TNyxText = ('Any locale', 'Default locale', 'Localized');
  CMatches: array[TNyxResourceLabelMatch] of TNyxText = ('All selected tags', 'Any selected tag');

function NyxResourceBrowserState: TNyxResourceBrowserState;
begin
  Result := Default(TNyxResourceBrowserState);
  Result.Query := NyxResourceCatalogQuery;
end;

function TNyxResourceBrowserState.Filters(
  AValue: TNyxResourceFilterDisclosure): TNyxResourceBrowserState;
begin

  if not (Ord(AValue) in [Ord(rfdCollapsed), Ord(rfdExpanded)]) then
  begin
    raise ENyxResource.Create('Unsupported resource filter disclosure');
  end;
  Result := Self;
  Result.FilterDisclosure := AValue;
end;

function TNyxResourceBrowserState.ToData: TNyxDataValue;
var
  LTags: TNyxResourceLabelsEditorState;
begin
  Filters(FilterDisclosure);
  LTags := Default(TNyxResourceLabelsEditorState);
  LTags.Labels := Query.LabelValues;
  LTags.Input := TagInput;
  LTags.Selection := TagSelection;
  Result := NyxObject([NyxField('version', NyxData(2)), NyxField('query', Query.ToData),
    NyxField('tagEditor', LTags.ToData),
    NyxField('filterDisclosure', NyxData(Ord(FilterDisclosure)))]);
end;

class function TNyxResourceBrowserState.FromData(
  const AData: TNyxDataValue): TNyxResourceBrowserState;
var
  LState: TNyxResourceBrowserState;
  LTags: TNyxResourceLabelsEditorState;
  LVersion: Integer;
  LDisclosure: Integer;
begin

  if AData.Kind <> ndObject then
  begin
    raise ENyxResource.Create('Resource browser state requires an object');
  end;
  LVersion := AData.Field('version').AsInteger;

  if not (LVersion in [1, 2]) or
    ((LVersion = 1) and (AData.Count <> 3)) or
    ((LVersion = 2) and (AData.Count <> 4)) then
  begin
    raise ENyxResource.Create('Resource browser state requires its exact version and fields');
  end;
  LState := NyxResourceBrowserState;
  LState.Query := TNyxResourceCatalogQuery.FromData(AData.Field('query'));
  LTags := TNyxResourceLabelsEditorState.FromData(AData.Field('tagEditor'));

  if LTags.Labels.ToData.ToJSON <> LState.Query.LabelValues.ToData.ToJSON then
  begin
    raise ENyxResource.Create('Resource filter tags must match the query');
  end;
  LState.TagInput := LTags.Input;
  LState.TagSelection := LTags.Selection;
  { Historical discovery keeps its exact predicates and partial input; the
    newly available disclosure starts collapsed without discarding any filter. }

  if LVersion = 2 then
  begin
    LDisclosure := AData.Field('filterDisclosure').AsInteger;

    if not (LDisclosure in [Ord(rfdCollapsed), Ord(rfdExpanded)]) then
    begin
      raise ENyxResource.Create('Unsupported resource filter disclosure');
    end;
    LState.FilterDisclosure := TNyxResourceFilterDisclosure(LDisclosure);
  end;
  Result := LState;
end;

function NyxResourceBrowserListID(const AID: TNyxText): TNyxText;
begin
  Result := AID + TNyxText('-list');
end;

function NyxResourceBrowserTagsID(const AID: TNyxText): TNyxText;
begin
  Result := AID + TNyxText('-tags');
end;

function NyxResourceBrowserFiltersID(const AID: TNyxText): TNyxText;
begin
  Result := AID + TNyxText('-filter-fields');
end;

function NyxResourceBrowserFieldID(const AID: TNyxText;
  AField: TNyxResourceBrowserField): TNyxText;
begin
  Result := AID + TNyxText('-') + CFields[AField];
end;

function NyxResourceBrowserKindID(const AID: TNyxText; AKind: TNyxResourceKind): TNyxText;
begin
  Result := AID + TNyxText('-kind-') + TNyxText(IntToStr(Ord(AKind)));
end;

function NyxResourceBrowserActionID(const AID: TNyxText;
  AAction: TNyxResourceBrowserAction): TNyxText;
begin
  Result := AID + TNyxText('-') + CActions[AAction];
end;

function Mode(AEditor: TNyxNode): TNyxResourceBrowserMode;
var
  LMode: Integer;
begin

  if (AEditor = nil) or (AEditor.Props.IndexOfName(CState) < 0) then
  begin
    raise ENyxResource.Create('Resource browsing requires a complete compound');
  end;
  LMode := TNyxDataValue.ParseJSON(AEditor.Prop(CMode)).AsInteger;

  if not (LMode in [Ord(rbmCompact), Ord(rbmWorkspace)]) then
  begin
    raise ENyxResource.Create('Unsupported resource browser mode');
  end;
  Result := TNyxResourceBrowserMode(LMode);
end;

function Field(AEditor: TNyxNode; AField: TNyxResourceBrowserField): TNyxNode;
begin
  Result := AEditor.Find(NyxResourceBrowserFieldID(AEditor.ID, AField));

  if Result = nil then
  begin
    raise ENyxResource.Create('Resource filter field is missing');
  end;
end;

function Choice(AEditor: TNyxNode; AField: TNyxResourceBrowserField;
  const AChoices: array of TNyxText): Integer;
var
  LValue: TNyxText;
begin
  LValue := Field(AEditor, AField).Prop('value');
  for Result := 0 to High(AChoices) do
  begin

    if AChoices[Result] = LValue then
    begin
      Exit;
    end;
  end;
  raise ENyxResource.Create('Unknown resource filter choice');
end;

procedure RequireComplete(AEditor: TNyxNode);
var
  LMode: TNyxResourceBrowserMode;
  LField: TNyxResourceBrowserField;
  LKind: TNyxResourceKind;
  LAction: TNyxResourceBrowserAction;
begin
  LMode := Mode(AEditor);
  Field(AEditor, rbfSearch);

  if AEditor.Find(NyxResourceBrowserListID(AEditor.ID)) = nil then
  begin
    raise ENyxResource.Create('Resource list is missing');
  end;
  for LAction := Low(TNyxResourceBrowserAction) to High(TNyxResourceBrowserAction) do
  begin

    if (LAction = rbaFilters) and (LMode = rbmCompact) then
    begin
      Continue;
    end;

    if AEditor.Find(NyxResourceBrowserActionID(AEditor.ID, LAction)) = nil then
    begin
      raise ENyxResource.Create('Resource browser action is missing');
    end;
  end;

  if LMode = rbmWorkspace then
  begin

    if AEditor.Find(NyxResourceBrowserFiltersID(AEditor.ID)) = nil then
    begin
      raise ENyxResource.Create('Resource filter group is missing');
    end;
    for LField := rbfSources to High(TNyxResourceBrowserField) do
    begin
      Field(AEditor, LField);
    end;
    for LKind := Low(TNyxResourceKind) to High(TNyxResourceKind) do
    begin

      if AEditor.Find(NyxResourceBrowserKindID(AEditor.ID, LKind)) = nil then
      begin
        raise ENyxResource.Create('Resource category field is missing');
      end;
    end;
    ReadNyxResourceLabelsEditor(AEditor.Find(NyxResourceBrowserTagsID(AEditor.ID)));
  end;
end;

function ReadNyxResourceBrowser(AEditor: TNyxNode): TNyxResourceBrowserState;
var
  LKinds: TNyxResourceCatalogKinds;
  LKind: TNyxResourceKind;
  LTags: TNyxResourceLabelsEditorState;
  LNode: TNyxNode;
begin
  RequireComplete(AEditor);
  Result := TNyxResourceBrowserState.FromData(TNyxDataValue.ParseJSON(AEditor.Prop(CState)));
  Result.Query := Result.Query.Search(Field(AEditor, rbfSearch).Prop('value'), Result.Query.Comparison);

  if Mode(AEditor) = rbmWorkspace then
  begin
    LKinds := [];
    for LKind := Low(TNyxResourceKind) to High(TNyxResourceKind) do
    begin
      LNode := AEditor.Find(NyxResourceBrowserKindID(AEditor.ID, LKind));

      if LNode = nil then
      begin
        raise ENyxResource.Create('Resource category field is missing');
      end;

      if LNode.Prop('value') = 'true' then
      begin
        Include(LKinds, LKind);
      end
      else if LNode.Prop('value') <> 'false' then
      begin
        raise ENyxResource.Create('Resource category requires a Boolean choice');
      end;
    end;
    Result.Query := Result.Query.Kinds(LKinds);

    if LKinds = [Low(TNyxResourceKind)..High(TNyxResourceKind)] then
    begin
      Result.Query := Result.Query.AnyKind;
    end;
    LTags := ReadNyxResourceLabelsEditor(AEditor.Find(NyxResourceBrowserTagsID(AEditor.ID)));
    Result.Query := Result.Query.Sources(TNyxResourceCatalogSources(Choice(AEditor, rbfSources, CSources)))
      .Locales(TNyxResourceCatalogLocales(Choice(AEditor, rbfLocales, CLocales)))
      .Labels(LTags.Labels, TNyxResourceLabelMatch(Choice(AEditor, rbfLabelMatch, CMatches)));
    Result.TagInput := LTags.Input;
    Result.TagSelection := LTags.Selection;
  end;
  Result.Query.ToQuery;
end;

procedure RestoreNyxResourceBrowser(AEditor: TNyxNode; const AState: TNyxResourceBrowserState);
var
  LState: TNyxResourceBrowserState;
  LTags: TNyxResourceLabelsEditorState;
  LKind: TNyxResourceKind;
  LActive: Integer;
  LCaption: TNyxText;
begin
  LState := TNyxResourceBrowserState.FromData(AState.ToData);
  RequireComplete(AEditor);
  Field(AEditor, rbfSearch).Configure.Value(LState.Query.SearchText).Done;

  if Mode(AEditor) = rbmWorkspace then
  begin
    { The owned fields stay in place while hidden. A public toggle button shows
      its state and active predicates; closing this group never resets a query. }
    AEditor.Find(NyxResourceBrowserFiltersID(AEditor.ID)).Configure
      .Visible(LState.FilterDisclosure = rfdExpanded).Done;
    LActive := 0;

    if LState.Query.SearchText <> '' then
    begin
      Inc(LActive);
    end;

    if LState.Query.KindFilter then
    begin
      Inc(LActive);
    end;

    if LState.Query.SourceValues <> rcsAny then
    begin
      Inc(LActive);
    end;

    if LState.Query.LocaleValues <> rclAny then
    begin
      Inc(LActive);
    end;

    if LState.Query.LabelValues.Count > 0 then
    begin
      Inc(LActive);
    end;
    LCaption := 'Show filters';

    if LState.FilterDisclosure = rfdExpanded then
    begin
      LCaption := 'Hide filters';
    end;

    if LActive > 0 then
    begin
      LCaption := LCaption + TNyxText(' (') + TNyxText(IntToStr(LActive)) + TNyxText(' active)');
    end;
    AEditor.Find(NyxResourceBrowserActionID(AEditor.ID, rbaFilters)).Configure
      .Text(LCaption).AccessibleName(LCaption)
      .Pressed(LState.FilterDisclosure = rfdExpanded).Done;
    for LKind := Low(TNyxResourceKind) to High(TNyxResourceKind) do
    begin
      AEditor.Find(NyxResourceBrowserKindID(AEditor.ID, LKind)).Configure
        .Value(not LState.Query.KindFilter or (LKind in LState.Query.KindValues)).Done;
    end;
    Field(AEditor, rbfSources).Configure.Value(CSources[LState.Query.SourceValues]).Done;
    Field(AEditor, rbfLocales).Configure.Value(CLocales[LState.Query.LocaleValues]).Done;
    Field(AEditor, rbfLabelMatch).Configure.Value(CMatches[LState.Query.LabelMatch]).Done;
    LTags := Default(TNyxResourceLabelsEditorState);
    LTags.Labels := LState.Query.LabelValues;
    LTags.Input := LState.TagInput;
    LTags.Selection := LState.TagSelection;
    RestoreNyxResourceLabelsEditor(AEditor.Find(NyxResourceBrowserTagsID(AEditor.ID)), LTags);
  end;
  AEditor.SetProp(CState, LState.ToData.ToJSON);
end;

function NewNyxResourceBrowser(const AID: TNyxText; const AState: TNyxResourceBrowserState;
  AMode: TNyxResourceBrowserMode): INyxCard;
var
  LState: TNyxResourceBrowserState;
  LKinds: INyxRow;
  LFilters: INyxColumn;
  LActions: INyxRow;
  LKind: TNyxResourceKind;
  LTitle: TNyxText;
begin

  if not (Ord(AMode) in [Ord(rbmCompact), Ord(rbmWorkspace)]) then
  begin
    raise ENyxResource.Create('Unsupported resource browser mode');
  end;
  LState := TNyxResourceBrowserState.FromData(AState.ToData);
  Result := NewNyxCard(AID);
  Result.Configure.Layout(TNyxLayoutPolicy.Column).Gap(10).Done;
  Result.Node.SetProp(CMode, NyxData(Ord(AMode)).ToJSON).SetProp(CState, LState.ToData.ToJSON);
  Result.Add(NewNyxInput(NyxResourceBrowserFieldID(AID, rbfSearch)).Configure
    .Text('Find a resource').AccessibleName('Find a resource').Placeholder('Name, intent or tag...').Done);
  { Discovery results and explicit Open precede detailed controls on every
    target. The same owned group can be expanded in a narrow or wide viewport. }
  Result.Add(NewNyxList(NyxResourceBrowserListID(AID)).Configure.Height(140)
    .AccessibleName('Project resources').Done);
  LTitle := 'Open resource';

  if AMode = rbmCompact then
  begin
    LTitle := 'Manage resources';
  end;
  LActions := NewNyxRow(AID + TNyxText('-actions'));
  LActions.Configure.Layout(TNyxLayoutPolicy.Row.Wrap(nfwWrap)).Gap(10).Done;
  LActions.Add(NewNyxButton(NyxResourceBrowserActionID(AID, rbaOpen)).WithText(LTitle));
  LActions.Add(NewNyxButton(NyxResourceBrowserActionID(AID, rbaReset)).WithText('Reset filters'));
  Result.Add(LActions);

  if AMode = rbmWorkspace then
  begin
    Result.Add(NewNyxButton(NyxResourceBrowserActionID(AID, rbaFilters)));
    LFilters := NewNyxColumn(NyxResourceBrowserFiltersID(AID));
    LFilters.Configure.Layout(TNyxLayoutPolicy.Column).Gap(10).Done;
    Result.Add(LFilters);
    LFilters.Add(NewNyxLabel(AID + TNyxText('-categories')).WithText('Categories'));
    LKinds := NewNyxRow(AID + TNyxText('-kinds'));
    LKinds.Configure.Layout(TNyxLayoutPolicy.Row.Wrap(nfwWrap)).Gap(10).Done;
    for LKind := Low(TNyxResourceKind) to High(TNyxResourceKind) do
    begin
      LKinds.Add(NewNyxCheckbox(NyxResourceBrowserKindID(AID, LKind)).WithText(CKinds[LKind]));
    end;
    LFilters.Add(LKinds);
    LFilters.Add(NewNyxSelect(NyxResourceBrowserFieldID(AID, rbfSources)).Configure
      .Text('Source').AccessibleName('Resource source').Items(CSources[rcsAny] + TNyxText(#10) +
        CSources[rcsEmbedded] + TNyxText(#10) + CSources[rcsHosted]).Done);
    LFilters.Add(NewNyxSelect(NyxResourceBrowserFieldID(AID, rbfLocales)).Configure
      .Text('Locale').AccessibleName('Resource locale').Items(CLocales[rclAny] + TNyxText(#10) +
        CLocales[rclDefault] + TNyxText(#10) + CLocales[rclLocalized]).Done);
    LFilters.Add(NewNyxResourceLabelsEditor(NyxResourceBrowserTagsID(AID),
      LState.Query.LabelValues, rlepFilter));
    LFilters.Add(NewNyxSelect(NyxResourceBrowserFieldID(AID, rbfLabelMatch)).Configure
      .Text('Tag matching').AccessibleName('Tag matching').Items(CMatches[rlmAll] +
        TNyxText(#10) + CMatches[rlmAny]).Done);
  end;
  RestoreNyxResourceBrowser(Result.Node, LState);
end;

function NyxResourceBrowserInput(ANode, AEditor: TNyxNode): Boolean;
var
  LField: TNyxResourceBrowserField;
  LKind: TNyxResourceKind;
begin
  Result := False;

  if (ANode = nil) or (AEditor = nil) or (AEditor.Find(ANode.ID) <> ANode) then
  begin
    Exit;
  end;
  for LField := Low(TNyxResourceBrowserField) to High(TNyxResourceBrowserField) do
  begin

    if ANode.ID = NyxResourceBrowserFieldID(AEditor.ID, LField) then
    begin
      Exit(True);
    end;
  end;
  for LKind := Low(TNyxResourceKind) to High(TNyxResourceKind) do
  begin

    if ANode.ID = NyxResourceBrowserKindID(AEditor.ID, LKind) then
    begin
      Exit(True);
    end;
  end;
  Result := NyxResourceLabelsEditorInput(ANode, AEditor.Find(NyxResourceBrowserTagsID(AEditor.ID)));
end;

function PrepareNyxResourceBrowserAction(AButton, AEditor: TNyxNode;
  out AState: TNyxResourceBrowserState): Boolean;
var
  LCandidate: TNyxNode;
begin
  Result := False;
  AState := NyxResourceBrowserState;

  if (AButton = nil) or (AEditor = nil) or (AEditor.Find(AButton.ID) <> AButton) then
  begin
    Exit;
  end;

  if AButton.ID = NyxResourceBrowserActionID(AEditor.ID, rbaReset) then
  begin
    { Reset predicates, not the user's current disclosure preference. }
    AState.FilterDisclosure := ReadNyxResourceBrowser(AEditor).FilterDisclosure;
    Exit(True);
  end;

  if (Mode(AEditor) = rbmWorkspace) and
    (AButton.ID = NyxResourceBrowserActionID(AEditor.ID, rbaFilters)) then
  begin
    AState := ReadNyxResourceBrowser(AEditor);

    if AState.FilterDisclosure = rfdCollapsed then
    begin
      AState := AState.Filters(rfdExpanded);
    end
    else
    begin
      AState := AState.Filters(rfdCollapsed);
    end;
    Exit(True);
  end;
  LCandidate := AEditor.Clone;
  try

    if HandleNyxResourceLabelsEditorAction(LCandidate.Find(AButton.ID),
      LCandidate.Find(NyxResourceBrowserTagsID(LCandidate.ID))) then
    begin
      AState := ReadNyxResourceBrowser(LCandidate);
      Result := True;
    end;
  finally
    LCandidate.Free;
  end;
end;

end.
