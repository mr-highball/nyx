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

program nyx_table_query_studio_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifdef PAS2JS}JS, Web, nyx.render.browser,{$else}
  Interfaces, Classes, Forms, Controls, StdCtrls, nyx.render.lcl,
  nyx.studio.lcl, nyx.studio.section.views,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.data, nyx.state,
  nyx.behavior, nyx.codec, nyx.codegen, nyx.source, nyx.disclosure,
  nyx.collections, nyx.collections.codec, nyx.collections.selection, nyx.collections.view.types,
  nyx.collections.columns, nyx.collections.paging, nyx.collections.query,
  nyx.collections.query.editor, nyx.collections.query.policy,
  nyx.studio.projects, nyx.studio.session, nyx.studio.sourcejobs,
  nyx.studio.view, nyx.studio.collections, nyx.studio.collectionintent,
  nyx.studio.collectionedits, nyx.studio.stateedits, nyx.studio.authoring,
  nyx.studio.inspector, nyx.menu.editor;

type
  {$ifdef PAS2JS}
  TRenderer = TNyxBrowserRenderer;
  {$else}
  TRenderer = TNyxStudioSectionViews;
  TControlAccess = class(TControl);
  { Observe exceptions that an ordinary LCL event callback would otherwise
    report through Application rather than returning to the test frame. }
  TFailureObserver = class
    Text: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
  end;
  {$endif}
  { The native branch owns a real Studio application at the requested client
    width. The browser branch mounts the same shared Inspector with the same
    source queue. Both drive live widget values and ordinary Click/Change events;
    neither writes a live form descriptor to simulate successful user input. }
  TReview = class
  private
    FRenderer: TRenderer;
    FSession: TNyxStudioSession;
    FCommands: TNyxSourceCommands;
    {$ifdef PAS2JS}
    FHost: TJSHTMLElement;
    FShell: TNyxDocument;
    FState: TNyxStudioViewState;
    {$else}
    FHost: TForm;
    FStudio: TNyxNativeStudio;
    FFailure: TFailureObserver;
    {$endif}
    FStage: Integer;
    FBefore, FAfter, FDefinition, FSibling, FDefaults: TNyxText;
    procedure Ready;
    procedure Refresh;
    {$ifdef PAS2JS}
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure Changed(AState: TNyxSourceCommandState; const AMessage: TNyxText);
    {$endif}
    procedure Change(const AID, AValue: TNyxText);
    procedure Click(const AID: TNyxText);
    procedure Expand(AColumn: Integer);
    function Pair: TNyxText;
    { Return a real section root rather than passing the native lookup forest
      to a compound capture that requires an actual owned model tree. }
    function Root: TNyxNode;
    function Spec: TNyxCollectionViewSpec;
    procedure Isolated;
    procedure History;
    procedure CaptureRefused(const AID: TNyxText);
  public
    constructor Create;
    destructor Destroy; override;
    function Step: Boolean;
  end;

const
  CEditor = 'inspector-collection-query';
  CFirst = 'first-table';
  CSecond = 'second-table';
  CDefinition = 'table-definition';
var
  GChecks, GPolls, GWidth: Integer;
  GReview: TReview;
  GEvidence: TNyxText;

{ Diagnostic presentation only. Native output is flushed at each completed
  boundary so an interrupted synchronous call retains its last entered phase.
  No widget, scheduler, query or pending-state getter is evaluated for logging. }
procedure Trace(const APhase: TNyxText; AStage: Integer = -1);
begin
  {$ifdef PAS2JS}
  document.body.setAttribute('data-query-authoring-stage', APhase +
    ';stage=' + IntToStr(AStage) + ';checks=' + IntToStr(GChecks));
  {$else}
  WriteLn('TRACE ticks=', GetTickCount64, ' stage=', AStage,
    ' checks=', GChecks, ' ', APhase);
  Flush(Output);
  {$endif}
end;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Table query authoring: ' + AReason);
  end;
  Inc(GChecks);
end;

function People: TNyxCollectionRef;
begin
  Result := NyxCollection('people');
end;

function PeopleSchema: TNyxCollectionSchema;
begin
  Result := NyxCollectionSchema.Text(NyxTextField('name'), '')
    .Integer(NyxIntegerField('age'), 0).Number(NyxNumberField('score'), 0)
    .Boolean(NyxBooleanField('active'), False).Text(NyxTextField('notes'), '');
end;

function InitialSpec: TNyxCollectionViewSpec;
begin
  Result := NyxCollectionView(People).Column(NyxTextField('name'), 'Name')
    .ColumnQuery(NyxColumnQuery.Operators([nqcEqual, nqcContains])
      .TextComparison(nqtAsciiInsensitive))
    .Column(NyxIntegerField('age'), 'Age')
    .ColumnQuery(NyxColumnQuery.Operators([nqcEqual, nqcAtLeast]))
    .Column(NyxNumberField('score'), 'Score')
    .ColumnQuery(NyxColumnQuery.Sorting(False))
    .Column(NyxBooleanField('active'), 'Active')
    .ColumnQuery(NyxColumnQuery.Filtering(False).Operators([nqcEqual]))
    .Scoped(csInstance).Selection(nsmMultiple)
    .Query(NyxCollectionQuery.OrderBy(NyxTextField('name'), nsdDescending))
    .Paged(NyxCollectionPaging.PageSize(2))
    .TableQuery(NyxTableQuery.SortMode(nqsmSingle));
end;

function Fixture: TNyxDocument;
var
  LTable: INyxTable;
  LInstance: INyxComponent;
  LPage: INyxPage;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Typed query controls authoring';
    Result.Collections.Define(People, PeopleSchema, [
      NyxCollectionItem(NyxItem(People, 'ada')).WithValue(NyxTextField('name'), 'Ada')
        .WithValue(NyxIntegerField('age'), 36).WithValue(NyxNumberField('score'), 9.5)
        .WithValue(NyxBooleanField('active'), True),
      NyxCollectionItem(NyxItem(People, 'grace')).WithValue(NyxTextField('name'), 'Grace')
        .WithValue(NyxIntegerField('age'), 40).WithValue(NyxNumberField('score'), 8.25)
        .WithValue(NyxBooleanField('active'), False)]);
    LTable := NewNyxTable(CDefinition);
    LTable.Configure.Height(200).Done;
    LTable.Binds.Collection(InitialSpec);
    Result.AddComponent(LTable);
    LPage := NewNyxPage('home');
    LInstance := NewNyxComponent(CFirst);
    LInstance.Reference := NyxComponent(CDefinition);
    LPage.Add(LInstance);
    LInstance := NewNyxComponent(CSecond);
    LInstance.Reference := NyxComponent(CDefinition);
    LPage.Add(LInstance);
    Result.AddPage(LPage);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

function FixturePair: TNyxProjectPair;
var
  LDocument: TNyxDocument;
begin
  LDocument := Fixture;
  try
    Result := NyxProjectPair(TNyxCodec.Encode(LDocument),
      TNyxCodegen.Generate(LDocument, 'nyx.fixture.table.query.studio'));
  finally
    LDocument.Free;
  end;
end;

function Intent(AAction: TNyxStudioCollectionAction;
  const ASpec: TNyxCollectionViewSpec): TNyxStudioCollectionIntent;
begin
  Result := Default(TNyxStudioCollectionIntent);
  Result.Action := AAction;
  Result.Key := People;
  Result.Projection := cpTable;

  if AAction in [scaQuery, scaTableQuery, scaColumnQuery,
    scaDefaultTableQuery, scaDefaultColumnQuery] then
  begin
    Result.QueryBaseline := NyxQueryEditorBaseline(PeopleSchema, ASpec);
  end;
end;

function HasField(const AData: TNyxDataValue; const AName: TNyxText): Boolean;
var LIndex: Integer;
begin
  Result := False;
  for LIndex := 0 to AData.Count - 1 do
  begin

    if AData.Key(LIndex) = AName then
    begin
      Exit(True);
    end;
  end;
end;

procedure RefuseIntent(const AIntent: TNyxStudioCollectionIntent;
  const ASpec: TNyxCollectionViewSpec; const AReason: TNyxText);
var
  LRefused: Boolean;
  LResult: TNyxCollectionViewSpec;
begin
  LRefused := False;
  try
    LResult := ApplyNyxStudioCollectionViewIntent(AIntent, ASpec, PeopleSchema);
    LResult.Validate;
  except
    on E: ENyxCollection do LRefused := True;
    on E: EArgumentException do LRefused := True;
  end;
  Check(LRefused, AReason);
end;

{ Pure descriptor/candidate checks accompany, rather than replace, the physical
  Studio journey below. These use the same immutable spec and public patch API. }
procedure DescriptorCases;
var
  LSpec, LChanged, LDefault, LAdded: TNyxCollectionViewSpec;
  LIntent, LRead: TNyxStudioCollectionIntent;
  LData, LDiscovery, LBranches, LProperties, LVersion: TNyxDataValue;
  LPatch, LReadPatch: INyxCollectionPatch;
  LPair, LCandidate, LResolved: TNyxProjectPair;
  LDoc: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LIndex, LBranch, LFound, LCount: Integer;
  LAction: TNyxStudioCollectionAction;
begin
  LSpec := InitialSpec;
  Check(LSpec.ToData.Field('version').AsInteger = 7,
    'explicit policies use the saved v7 descriptor');
  Check(TNyxCollectionViewSpec.FromData(LSpec.ToData).ToData.ToJSON = LSpec.ToData.ToJSON,
    'table and four scalar column policies survive strict descriptor roundtrip');
  LIntent := Intent(scaTitle, LSpec);
  LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('name'));
  LIntent.Value := 'Display name';
  Check(LIntent.ToData.Count = 11, 'former title intent retains eleven fields');
  LChanged := ApplyNyxStudioCollectionViewIntent(LIntent, LSpec, PeopleSchema);
  Check(LChanged.TableQueryPolicy.ToData.ToJSON = LSpec.TableQueryPolicy.ToData.ToJSON,
    'unrelated title reconstruction preserves explicit table policy');
  for LIndex := 0 to LSpec.Count - 1 do
  begin
    Check(LChanged.ColumnAt(LIndex).HasQueryPolicy and
      (LChanged.ColumnAt(LIndex).QueryPolicy.ToData.ToJSON =
      LSpec.ColumnAt(LIndex).QueryPolicy.ToData.ToJSON),
      'unrelated title reconstruction preserves each existing column policy');
  end;
  LIntent := Intent(scaMode, LSpec);
  LIntent.Field := TNyxStudioCollectionFieldRef.Integer(NyxIntegerField('age'));
  LIntent.Mode := cmEditable;
  LChanged := ApplyNyxStudioCollectionViewIntent(LIntent, LSpec, PeopleSchema);
  Check((LChanged.ColumnAt(1).Mode = cmEditable) and LChanged.HasTableQueryPolicy and
    LChanged.ColumnAt(1).HasQueryPolicy, 'editable mode does not clear query capability choices');
  LIntent := Intent(scaScope, LSpec);
  LIntent.Scope := csApplication;
  LChanged := ApplyNyxStudioCollectionViewIntent(LIntent, LSpec, PeopleSchema);
  Check((LChanged.Scope = csApplication) and LChanged.ColumnAt(0).HasQueryPolicy and
    LChanged.HasTableQueryPolicy, 'scope change retains raw policy declarations');
  LIntent := Intent(scaColumnFixed, LSpec);
  LIntent.Field := TNyxStudioCollectionFieldRef.Number(NyxNumberField('score'));
  LIntent.Value := '120';
  LChanged := ApplyNyxStudioCollectionViewIntent(LIntent, LSpec, PeopleSchema);
  Check(LChanged.ColumnAt(2).Sizing.Defined and LChanged.ColumnAt(2).HasQueryPolicy and
    LChanged.HasTableQueryPolicy, 'column sizing changes retain policies');
  LIntent := Intent(scaAddColumn, LSpec);
  LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('notes'));
  LAdded := ApplyNyxStudioCollectionViewIntent(LIntent, LSpec, PeopleSchema);
  Check((LAdded.Count = 5) and not LAdded.ColumnAt(4).HasQueryPolicy and
    LAdded.ColumnAt(0).HasQueryPolicy and LAdded.HasTableQueryPolicy,
    'new column uses defaults while existing raw choices survive');
  LIntent := Intent(scaRemoveColumn, LAdded);
  LIntent.Field := TNyxStudioCollectionFieldRef.Integer(NyxIntegerField('age'));
  LChanged := ApplyNyxStudioCollectionViewIntent(LIntent, LAdded, PeopleSchema);
  Check((LChanged.Count = 4) and (LChanged.ColumnAt(1).FieldName = 'score') and
    LChanged.ColumnAt(1).HasQueryPolicy and LChanged.HasTableQueryPolicy,
    'removing a column preserves surviving field policies at their new positions');
  LIntent := Intent(scaQuery, LSpec);
  LIntent.Query := NyxCollectionQuery.Where(NyxWhere(NyxIntegerField('age')).AtLeast(18));
  Check(LIntent.ToData.Count = 13, 'former query intent retains thirteen fields');
  for LAction := scaTableQuery to scaDefaultColumnQuery do
  begin
    LIntent := Intent(LAction, LSpec);

    if LAction in [scaColumnQuery, scaDefaultColumnQuery] then
    begin
      LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('name'));
    end;

    if LAction = scaTableQuery then
    begin
      LIntent.TableQuery := NyxTableQuery.Filtering(False);
    end;

    if LAction = scaColumnQuery then
    begin
      LIntent.ColumnQuery := NyxColumnQuery.Sorting(False);
    end;
    LCount := 12;

    if LAction in [scaTableQuery, scaColumnQuery] then
    begin
      LCount := 13;
    end;
    Check(LIntent.ToData.Count = LCount, 'policy wire contains only its action-specific payload');
    LRead := TNyxStudioCollectionIntent.FromData(LIntent.ToData);
    Check(LIntent.SameIntent(LRead), 'four typed actions retain closed values and exact baseline');
    Check(NyxStudioCollectionStructuralAction(LAction), 'all policy actions hold structural pending locks');
  end;
  LIntent := Intent(scaTableQuery, LSpec);
  RefuseIntent(LIntent, LSpec, 'undefined table setter refuses');
  LIntent := Intent(scaColumnQuery, LSpec);
  LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('name'));
  RefuseIntent(LIntent, LSpec, 'undefined column setter refuses');
  LIntent := Intent(scaDefaultTableQuery, LSpec);
  LIntent.TableQuery := NyxTableQuery;
  RefuseIntent(LIntent, LSpec, 'default command refuses an unused table policy');
  LIntent := Intent(scaDefaultColumnQuery, LSpec);
  LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('name'));
  LIntent.ColumnQuery := NyxColumnQuery;
  RefuseIntent(LIntent, LSpec, 'default command refuses an unused column policy');
  LIntent := Intent(scaTableQuery, LSpec);
  LIntent.TableQuery := NyxTableQuery;
  LIntent.Projection := cpList;
  RefuseIntent(LIntent, LSpec, 'table policy cannot be retargeted to a list');
  LIntent := Intent(scaColumnQuery, LSpec);
  LIntent.ColumnQuery := NyxColumnQuery;
  LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('notes'));
  RefuseIntent(LIntent, LSpec, 'policy cannot append a missing column');
  LIntent.Field := TNyxStudioCollectionFieldRef.Number(NyxNumberField('age'));
  RefuseIntent(LIntent, LSpec, 'changed field family refuses');
  LIntent := Intent(scaTableQuery, LSpec);
  LIntent.TableQuery := NyxTableQuery;
  RefuseIntent(LIntent, LSpec.TableQuery(NyxTableQuery.Enabled(False)),
    'stale full-spec baseline refuses even when the field schema is unchanged');
  LIntent := Intent(scaDefaultTableQuery, LSpec);
  LDefault := ApplyNyxStudioCollectionViewIntent(LIntent, LSpec, PeopleSchema);
  Check(not LDefault.HasTableQueryPolicy and LDefault.ColumnAt(0).HasQueryPolicy,
    'table defaults clear only the raw table declaration');
  LIntent := Intent(scaDefaultColumnQuery, LDefault);
  LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('name'));
  LDefault := ApplyNyxStudioCollectionViewIntent(LIntent, LDefault, PeopleSchema);
  Check(not LDefault.ColumnAt(0).HasQueryPolicy and LDefault.ColumnAt(1).HasQueryPolicy,
    'column defaults clear only the chosen raw column declaration');
  LDefault := NyxCollectionView(People).Column(NyxTextField('name'), 'Name');
  LIntent := Intent(scaTitle, LDefault);
  LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('name'));
  LIntent.Value := 'Renamed';
  LChanged := ApplyNyxStudioCollectionViewIntent(LIntent, LDefault, PeopleSchema);
  Check(not LChanged.HasQueryControls and (LChanged.ToData.Field('version').AsInteger = 1),
    'unrelated edits never materialize resolved defaults or upgrade old descriptor bytes');

  LPair := FixturePair;
  LIntent := Intent(scaTableQuery, LSpec);
  LIntent.TableQuery := NyxTableQuery.Filtering(False).SortMode(nqsmMultiple);
  LPatch := NyxCollectionPatch([NyxCollectionIntentChange(NyxBindingOwner(CFirst), LIntent)]);
  LReadPatch := ReadNyxCollectionPatch(LPatch.ToData);
  Check(LReadPatch.ToData.ToJSON = LPatch.ToData.ToJSON,
    'semantic patch advertises the same typed action/baseline as ordinary authoring');
  LCandidate := LReadPatch.Candidate(LPair);
  LDoc := nil;
  LWorkspace := nil;
  try
    AdmitNyxProject(LCandidate, nprRequireMatch, LDoc, LWorkspace, LResolved);
    Check(LDoc.Find(CFirst).HasCollectionView and
      not LDoc.Find(CFirst).CollectionView.TableQueryPolicy.FilterEnabled,
      'semantic patch has a matching local binding and generated Pascal');
    Check(not LDoc.Find(CSecond).HasCollectionView and
      LDoc.Find(CDefinition).CollectionView.TableQueryPolicy.FilterEnabled,
      'semantic patch preserves definition and sibling ownership');
    Check(Pos('NyxTableQuery', LCandidate.Source) > 0,
      'policy candidate uses a typed source factory');
  finally
    LWorkspace.Free;
    LDoc.Free;
  end;

  LDiscovery := NyxCollectionAgentSchema;
  LBranches := LDiscovery.Field('oneOf');
  LBranches := LBranches.Item(LBranches.Count - 1).Field('properties')
    .Field('changes').Field('items').Field('oneOf');
  LFound := 0;
  for LBranch := 0 to LBranches.Count - 1 do
  begin
    LProperties := LBranches.Item(LBranch).Field('properties');

    if HasField(LProperties.Field('op'), 'const') and
      (LProperties.Field('op').Field('const').AsText = 'bind') then
    begin
      LData := LProperties.Field('spec').Field('oneOf');
      Check(LData.Count = 7, 'semantic discovery retains six former versions plus v7');
      LVersion := LData.Item(6);
      Check((LVersion.Field('properties').Field('version').Field('const').AsInteger = 7) and
        (LVersion.Field('required').Count = 10) and
        not LVersion.Field('additionalProperties').AsBoolean,
        'v7 discovery is closed and requires the complete ten-field root');
      LData := LVersion.Field('properties').Field('columns').Field('items');
      Check((LData.Field('required').Count = 6) and
        (LData.Field('properties').Field('sizing').Field('anyOf').Item(0)
          .Field('type').AsText = 'null') and
        (LData.Field('properties').Field('queryControls').Field('anyOf').Item(0)
          .Field('type').AsText = 'null'),
        'v7 six-field columns independently admit absent sizing and query choices');
      Check(LVersion.Field('anyOf').Item(1).Field('properties').Field('columns')
        .Field('contains').Field('properties').Field('queryControls')
        .Field('type').AsText = 'object',
        'all-null column choices cannot falsely promote a descriptor to v7');
      Inc(LFound);
    end;
    for LIndex := 0 to LProperties.Count - 1 do
    begin

      if LProperties.Key(LIndex) = 'action' then
      begin
        LData := LProperties.Field('action').Field('const');

        if (LData.AsText = 'table-query') or (LData.AsText = 'column-query') or
          (LData.AsText = 'default-table-query') or (LData.AsText = 'default-column-query') then
        begin
          Check((LProperties.Field('projection').Field('const').AsText = 'table') and
            (LProperties.Field('baseline').Field('minLength').AsInteger = 1) and
            not LBranches.Item(LBranch).Field('additionalProperties').AsBoolean,
            'each semantic policy action requires exact table/baseline and closed fields');
          Inc(LFound);
        end;
      end;
    end;
  end;
  Check(LFound = 5, 'semantic discovery contains v7 binding and all four policy actions');
end;

{ Malformed, stale, pending and mismatched-owner checks operate on an isolated
  ordinary panel. Mutations below deliberately corrupt that test panel to prove
  refusal; successful policy choices use real mounted inputs in TReview. }
procedure AdmissionCases;
var
  LSession: TNyxStudioSession;
  LPanel, LProjection, LForm, LRole: TNyxNode;
  LEdit: TNyxStudioDesignEdit;
  LPending: TNyxStudioPendingDesign;
  LBefore, LValue: TNyxText;
  LIndex: Integer;
  LAction: TNyxStudioCollectionAction;
  LIntent, LRead: TNyxStudioCollectionIntent;
  LFields: array of TNyxDataField;
  LData: TNyxDataValue;
  LRefused: Boolean;

  procedure Panel;
  begin
    FreeAndNil(LPanel);
    LPanel := TNyxNode.Create(nkColumn, 'test-inspector');
    LProjection := LSession.SelectedProjection;
    try
      AddNyxCollectionBindingPanel(LPanel, LSession, LProjection);
    finally
      LProjection.Free;
    end;
  end;

  procedure Refused(AColumn: Integer; const AReason: TNyxText);
  var LFailed: Boolean;
  begin
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    LFailed := False;
    try
      CaptureNyxStudioQuery(LSession, LPanel.Find(NyxStudioQueryPolicyApplyID(AColumn)),
        LPanel, LPending, LEdit);
    except
      on E: ENyxCollection do LFailed := True;
      on E: EArgumentException do LFailed := True;
    end;
    Check(LFailed and (EncodeNyxProject(LSession.ProjectSnapshot) = LBefore), AReason);
  end;

begin
  LPanel := nil;
  LSession := TNyxStudioSession.Create(FixturePair);
  try
    LSession.Activate('home');
    LSession.Select(CFirst);
    LPending := Default(TNyxStudioPendingDesign);
    Panel;
    Check(CaptureNyxStudioQuery(LSession, LPanel.Find(NyxStudioQueryPolicyApplyID(-1)),
      LPanel, LPending, LEdit) and (LEdit.Action = sdaCollection) and
      (LEdit.Selection = CFirst) and (LEdit.View = 'home') and
      (LEdit.Collection.Action = scaTableQuery),
      'complete ordinary form captures one existing paired collection edit');
    SetLength(LPending.Collections, 1);
    LPending.Collections[0].Owner := CFirst;
    LPending.Collections[0].Intent := LEdit.Collection;
    Refused(-1, 'pending structural policy blocks duplicate pre-paint capture');
    LPending := Default(TNyxStudioPendingDesign);
    LSession.Select(CSecond);
    Refused(-1, 'old first-owner form cannot target a newly selected sibling');
    LSession.Select(CFirst);
    LSession.SetCollectionView(InitialSpec.TableQuery(NyxTableQuery.Enabled(False)));
    Refused(-1, 'copied baseline refuses a changed complete binding');
    LSession.Undo;
    Panel;
    LForm := LPanel.Find(NyxStudioQueryPolicyFormID(0));
    LValue := LForm.Prop(NyxStudioCollectionKindKey);
    LForm.SetProp(NyxStudioCollectionKindKey, 'number');
    Refused(0, 'same positional column cannot impersonate another scalar family');
    LForm.SetProp(NyxStudioCollectionKindKey, LValue);
    LRole := LPanel.Find(NyxStudioQueryPolicyApplyID(0));
    LRole.Configure.ProjectAs(nkLabel).Done;
    Refused(0, 'projected button role cannot authorize whole-form Apply');
    LRole.Configure.ProjectAs(nkButton).Done;
    LForm.Add(TNyxNode.Create(nkCheckbox, 'extra-policy-role').Configure.Value(True).Done);
    Refused(0, 'unexpected form member refuses before producing an intent');
    Panel;
    LSession.SetSourceDraft(LSession.Source + #10 + '{ parked handwritten draft }');
    Refused(-1, 'parked Pascal draft prevents form capture without rewriting either side');
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    for LAction := scaTableQuery to scaDefaultColumnQuery do
    begin
      LIntent := Intent(LAction, InitialSpec);

      if LAction in [scaColumnQuery, scaDefaultColumnQuery] then
      begin
        LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('name'));
      end;

      if LAction = scaTableQuery then
      begin
        LIntent.TableQuery := NyxTableQuery;
      end;

      if LAction = scaColumnQuery then
      begin
        LIntent.ColumnQuery := NyxColumnQuery;
      end;
      LRefused := False;
      try
        LSession.ApplyCollectionIntent(LIntent);
      except
        on E: ENyxCollection do LRefused := True;
        on E: EArgumentException do LRefused := True;
      end;
      Check(LRefused and LSession.SourceDraftPending and
        (EncodeNyxProject(LSession.ProjectSnapshot) = LBefore),
        'each replay action refuses pending source before any paired mutation');
    end;
    LSession.DiscardSourceDraft;
    LIntent := Intent(scaTableQuery, InitialSpec);
    LIntent.TableQuery := NyxTableQuery;
    LData := LIntent.ToData;
    SetLength(LFields, LData.Count + 1);
    for LIndex := 0 to LData.Count - 1 do
    begin
      LFields[LIndex] := NyxField(LData.Key(LIndex), LData.Field(LData.Key(LIndex)));
    end;
    LFields[LData.Count] := NyxField('unexpected', NyxData(True));
    LRefused := False;
    try
      LRead := TNyxStudioCollectionIntent.FromData(NyxObject(LFields));
      LRead.Validate;
    except
      on E: ENyxCollection do LRefused := True;
      on E: EArgumentException do LRefused := True;
    end;
    Check(LRefused, 'private intent decoder rejects extra payload');
  finally
    LPanel.Free;
    LSession.Free;
  end;
end;

{$ifndef PAS2JS}
procedure TFailureObserver.Failed(ASender: TObject; AException: Exception);
begin
  Text := TNyxText(AException.Message);
end;
{$endif}

constructor TReview.Create;
begin
  inherited Create;
  Trace('constructor:begin', FStage);
  {$ifdef PAS2JS}
  Trace('constructor:browser-session:before', FStage);
  FSession := TNyxStudioSession.Create(FixturePair);
  Trace('constructor:browser-session:after', FStage);
  Trace('constructor:activate-select:before', FStage);
  FSession.Activate('home');
  FSession.Select(CFirst);
  Trace('constructor:activate-select:after', FStage);
  FState := DefaultNyxStudioViewState;
  FState.BindingsVisible := True;
  FCommands := TNyxSourceCommands.Create(FSession, @Changed);
  FRenderer := TRenderer.Create;
  FRenderer.OnEvent := @Event;
  FHost := TJSHTMLElement(document.createElement('div'));
  FHost.style.cssText := 'height:900px;overflow:auto;width:100%;';
  document.body.appendChild(FHost);
  Trace('constructor:browser-refresh:before', FStage);
  Refresh;
  Trace('constructor:browser-refresh:after', FStage);
  {$else}
  Trace('constructor:native-host:before', FStage);
  FFailure := TFailureObserver.Create;
  Application.OnException := FFailure.Failed;
  FHost := TForm.CreateNew(nil);
  FHost.ClientWidth := GWidth;
  FHost.ClientHeight := 900;
  FHost.Show;
  Check(FHost.ClientWidth = GWidth, 'ordinary Studio actual client width equals requested allocation');
  WriteLn('CLIENT_WIDTH=', FHost.ClientWidth);
  Trace('constructor:native-host:after', FStage);
  Trace('constructor:studio-create:before', FStage);
  FStudio := TNyxNativeStudio.Create(FHost, IncludeTrailingPathDelimiter(GEvidence) + 'studio-projects');
  Trace('constructor:studio-create:after', FStage);
  Trace('constructor:fixture-and-load-project:before', FStage);
  FStudio.LoadProject(FixturePair);
  Trace('constructor:fixture-and-load-project:after', FStage);
  FSession := FStudio.Session;
  Trace('constructor:activate-select:before', FStage);
  FSession.Activate('home');
  FSession.Select(CFirst);
  Trace('constructor:activate-select:after', FStage);
  Trace('constructor:studio-run:before', FStage);
  FStudio.Run;
  Trace('constructor:studio-run:after', FStage);
  FRenderer := FStudio.ShellView;
  FCommands := FStudio.SourceCommands;
  Trace('constructor:initial-ready:before', FStage);
  Ready;
  Trace('constructor:initial-ready:after', FStage);

  if FRenderer.RootFor('action-panel-inspector') <> nil then
  begin
    Trace('constructor:inspector:before', FStage);
    Click('action-panel-inspector');
    Trace('constructor:inspector:after', FStage);
  end;
  Trace('constructor:bindings:before', FStage);
  Click(NyxStudioBindingsToggleID);
  Trace('constructor:bindings:after', FStage);
  {$endif}
  Trace('constructor:baseline-snapshots:before', FStage);
  FDefinition := FSession.Document.Find(CDefinition).CollectionView.ToData.ToJSON;
  FSibling := TNyxCodec.NodeSnapshot(FSession.Document.Find(CSecond)).ToJSON;
  FDefaults := EncodeNyxCollectionDefaults(FSession.Document.Collections);
  Trace('constructor:baseline-snapshots:after', FStage);
  Trace('constructor:end', FStage);
end;

destructor TReview.Destroy;
begin
  {$ifdef PAS2JS}
  FCommands.Free;
  FRenderer.Free;
  FShell.Free;
  FSession.Free;
  FHost.remove;
  {$else}
  FStudio.Free;
  FHost.Free;
  Application.OnException := nil;
  FFailure.Free;
  {$endif}
  inherited Destroy;
end;

procedure TReview.Ready;
{$ifndef PAS2JS}var LStarted: QWord;{$endif}
begin
  Trace('ready:before', FStage);
  {$ifndef PAS2JS}
  LStarted := GetTickCount64;
  repeat
    Application.ProcessMessages;
    CheckSynchronize;

    if FFailure.Text <> '' then
    begin
      raise Exception.Create(FFailure.Text);
    end;

    if GetTickCount64 - LStarted > 60000 then
    begin
      raise Exception.Create('Ordinary policy authoring did not finish: ' + FStudio.Status);
    end;
    Sleep(1);
  until not FStudio.PresentationPending and not FCommands.Busy;
  {$endif}
  Trace('ready:after', FStage);
end;

procedure TReview.Refresh;
{$ifdef PAS2JS}var LShell: TNyxDocument;{$endif}
begin
  {$ifdef PAS2JS}
  FState.QueryEditorDraft.Capture(CEditor, FRenderer.Root);
  FState.PendingDesign := FCommands.PendingDesign;
  LShell := BuildNyxStudioView(FSession, FState);
  try
    FState.QueryEditorDraft.Restore(LShell.Pages[0]);
    FRenderer.Render(LShell, LShell.Find('studio-right'), FHost);
    FreeAndNil(FShell);
    FShell := LShell;
    LShell := nil;
  finally
    LShell.Free;
  end;
  {$else}
  Ready;
  {$endif}
end;

{$ifdef PAS2JS}
procedure TReview.Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin
  FCommands.Route(ANode, AEvent, FRenderer.Root);
end;

procedure TReview.Changed(AState: TNyxSourceCommandState; const AMessage: TNyxText);
begin

  if AState = nssApplied then
  begin
    Refresh;
  end;
end;
{$endif}

function TReview.Pair: TNyxText;
begin
  Result := EncodeNyxProject(FSession.ProjectSnapshot);
end;

function TReview.Root: TNyxNode;
begin
  {$ifdef PAS2JS}Result := FRenderer.Root;
  {$else}Result := FRenderer.RootFor(CEditor);{$endif}
end;

function TReview.Spec: TNyxCollectionViewSpec;
var LProjection: TNyxNode;
begin
  LProjection := FSession.SelectedProjection;
  try
    Result := LProjection.CollectionView;
  finally
    LProjection.Free;
  end;
end;

procedure TReview.Isolated;
begin
  Check(FSession.Document.Find(CDefinition).CollectionView.ToData.ToJSON = FDefinition,
    'policy edits leave the reusable definition exact');
  Check(TNyxCodec.NodeSnapshot(FSession.Document.Find(CSecond)).ToJSON = FSibling,
    'policy edits leave the independent sibling exact');
  Check(EncodeNyxCollectionDefaults(FSession.Document.Collections) = FDefaults,
    'query-control edits never mutate authored row/default data');
end;

procedure TReview.Change(const AID, AValue: TNyxText);
var
  {$ifdef PAS2JS}LInput: TJSHTMLElement;{$else}LInput: TControl;{$endif}
begin
  Trace('change:before:' + AID, FStage);
  LInput := FRenderer.InputFor(AID);
  Check(LInput <> nil, 'ordinary Inspector input is physically mounted: ' + AID);
  {$ifdef PAS2JS}

  if Root.Find(AID).ProjectionKind = NyxKindName(nkCheckbox) then
  begin
    TJSHTMLInputElement(LInput).checked := AValue = 'true';
  end
  else
  begin
    TJSHTMLInputElement(LInput).value := AValue;
  end;
  LInput.dispatchEvent(TJSEvent.new('change'));
  {$else}

  if LInput is TCheckBox then
  begin
    TCheckBox(LInput).Checked := AValue = 'true';
    TCheckBox(LInput).OnChange(LInput);
  end
  else if LInput is TComboBox then
  begin
    TComboBox(LInput).ItemIndex := TComboBox(LInput).Items.IndexOf(AValue);
    TComboBox(LInput).OnChange(LInput);
  end
  else
  begin
    TEdit(LInput).Text := AValue;
    TEdit(LInput).OnChange(LInput);
  end;
  {$endif}
  Check(Root.Find(AID).Prop(NyxAttributeName(atValue)) = AValue,
    'normal input event retains the exact mounted policy value');
  Trace('change:after:' + AID, FStage);
end;

procedure TReview.Click(const AID: TNyxText);
begin
  Trace('click:before:' + AID, FStage);
  Check(FRenderer.Root.Find(AID) <> nil, 'ordinary Inspector action exists: ' + AID);
  {$ifdef PAS2JS}
  FRenderer.ElementFor(AID).click;
  {$else}
  TControlAccess(FRenderer.ControlFor(AID)).Click;
  Ready;
  {$endif}
  Trace('click:after:' + AID, FStage);
end;

procedure TReview.Expand(AColumn: Integer);
var LSection, LHeader: TNyxNode;
begin
  LSection := Root.Find(NyxStudioQueryPolicyFormID(AColumn) + '-section');
  Check((LSection <> nil) and NyxIsDisclosure(LSection),
    'each column uses existing retained Disclosure presentation');

  if not NyxDisclosureExpanded(LSection) then
  begin
    LHeader := LSection.Part(NyxPart('header'));
    Click(LHeader.ID);
  end;
end;

procedure TReview.History;
begin
  FAfter := Pair;
  Check(FAfter <> FBefore, 'whole-policy Apply changes the paired project');
  {$ifdef PAS2JS}FSession.Undo; Refresh;{$else}Click('action-undo');{$endif}
  Check(Pair = FBefore, 'one ordinary Undo restores exact design and adjacent source');
  {$ifdef PAS2JS}FSession.Redo; Refresh;{$else}Click('action-redo');{$endif}
  Check(Pair = FAfter, 'one ordinary Redo restores exact policy and source');
  Isolated;
end;

procedure TReview.CaptureRefused(const AID: TNyxText);
var
  LEdit: TNyxStudioDesignEdit;
  LRefused: Boolean;
  LBefore: TNyxText;
begin
  LBefore := Pair;
  LRefused := False;
  try
    CaptureNyxStudioQuery(FSession, Root.Find(AID), Root,
      FCommands.PendingDesign, LEdit);
  except
    on E: ENyxCollection do LRefused := True;
    on E: EArgumentException do LRefused := True;
  end;
  Check(LRefused and (Pair = LBefore), 'invalid policy draft refuses without changing the accepted pair');
end;

function TReview.Step: Boolean;
var
  LIndex: Integer;
  LComparison: TNyxQueryComparison;
  LFamily: TNyxQueryComparisons;
begin
  Result := False;

  if FCommands.Busy then
  begin
    Exit;
  end;
  Trace('step:before', FStage);
  case FStage of
    0:
      begin
        Check(not FSession.Selected.HasCollectionView and Spec.HasTableQueryPolicy,
          'initial selected instance inherits its complete bound table');
        for LIndex := 0 to 3 do
        begin
          Check(not NyxDisclosureExpanded(Root.Find(
            NyxStudioQueryPolicyFormID(LIndex) + '-section')),
            'column operator forms start collapsed');
          LFamily := NyxQueryFamilyComparisons(Spec.ColumnAt(LIndex).Kind);
          for LComparison := Low(TNyxQueryComparison) to High(TNyxQueryComparison) do
          begin
            Check((Root.Find(NyxStudioQueryPolicyOperatorID(LIndex, LComparison)) <> nil) =
              (LComparison in LFamily), 'each scalar form exposes exactly its portable operator family');
          end;
        end;
        FBefore := Pair;
        Change(NyxStudioQueryPolicyFieldID(-1, sqpfFiltering), 'false');
        Change(NyxStudioQueryPolicyFieldID(-1, sqpfSortMode), 'Multiple keys');
        Check(Pair = FBefore, 'live choices remain draft until whole-policy Apply');
        {$ifdef PAS2JS}Refresh;{$else}
        Click(NyxInspectorEventsID);
        Click(NyxInspectorPropertiesID);
        {$endif}
        Check((Root.Find(NyxStudioQueryPolicyFieldID(-1, sqpfFiltering))
          .Prop(NyxAttributeName(atValue)) = 'false') and (Pair = FBefore),
          'ordinary Inspector replacement preserves the unsubmitted policy draft');
        Click(NyxStudioQueryPolicyApplyID(-1));
      end;
    1:
      begin
        Check((FCommands.State = nssApplied) and FSession.Selected.HasCollectionView and
          not Spec.TableQueryPolicy.FilterEnabled and (Spec.TableQueryPolicy.Mode = nqsmMultiple),
          'ordinary Apply admits one complete local table policy: ' + FCommands.Message);
        Check((Pos('.TableQuery(', FSession.Source) > 0) and
          (Pos('NyxTableQuery', FSession.Source) > 0), 'adjacent Pascal expresses typed table policy');
        History;
        Expand(0);
        FBefore := Pair;
        Change(NyxStudioQueryPolicyFieldID(0, sqpfSorting), 'false');
        Change(NyxStudioQueryPolicyFieldID(0, sqpfTextComparison), 'Exact text');
        Click(NyxStudioQueryPolicyApplyID(0));
      end;
    2:
      begin
        Check((FCommands.State = nssApplied) and not Spec.ColumnAt(0).QueryPolicy.SortEnabled and
          (Spec.ColumnAt(0).QueryPolicy.TextMode = nqtExact),
          'text column form applies sorting and text comparison together');
        History;
        Expand(1);
        FBefore := Pair;
        Change(NyxStudioQueryPolicyFieldID(1, sqpfFamilyOperators), 'false');
        LFamily := NyxQueryFamilyComparisons(nskInteger);
        for LComparison := Low(TNyxQueryComparison) to High(TNyxQueryComparison) do
        begin

          if LComparison in LFamily then
          begin
            Change(NyxStudioQueryPolicyOperatorID(1, LComparison), 'false');
          end;
        end;
        CaptureRefused(NyxStudioQueryPolicyApplyID(1));
        Check(Root.Find(NyxStudioQueryPolicyFieldID(1, sqpfFamilyOperators))
          .Prop(NyxAttributeName(atValue)) = 'false', 'invalid explicit-empty draft remains visible');
        {$ifdef PAS2JS}Refresh;{$else}
        Click(NyxStudioBindingsToggleID);
        Click(NyxStudioBindingsToggleID);
        {$endif}
        Expand(1);
        Check((Root.Find(NyxStudioQueryPolicyFieldID(1, sqpfFamilyOperators))
          .Prop(NyxAttributeName(atValue)) = 'false') and (Pair = FBefore),
          'hide/reopen retains invalid policy draft flags and the exact accepted pair');
        for LComparison := Low(TNyxQueryComparison) to High(TNyxQueryComparison) do
        begin

          if LComparison in LFamily then
          begin
            Check(Root.Find(NyxStudioQueryPolicyOperatorID(1, LComparison))
              .Prop(NyxAttributeName(atValue)) = 'false',
              'hide/reopen retains each unsubmitted empty operator choice');
          end;
        end;
        Change(NyxStudioQueryPolicyOperatorID(1, nqcAtLeast), 'true');
        Click(NyxStudioQueryPolicyApplyID(1));
      end;
    3:
      begin
        Check((FCommands.State = nssApplied) and
          (Spec.ColumnAt(1).QueryPolicy.ComparisonCount = 1) and
          Spec.ColumnAt(1).QueryPolicy.Allows(nskInteger, nqcAtLeast),
          'integer form admits only the chosen compatible operator');
        History;
        Expand(2);
        FBefore := Pair;
        Change(NyxStudioQueryPolicyFieldID(2, sqpfSorting), 'true');
        Click(NyxStudioQueryPolicyApplyID(2));
      end;
    4:
      begin
        Check((FCommands.State = nssApplied) and Spec.ColumnAt(2).QueryPolicy.SortEnabled,
          'number column form has independent capability choices');
        History;
        Expand(3);
        Check(Root.Find(NyxStudioQueryPolicyOperatorID(3, nqcContains)) = nil,
          'Boolean form never advertises text-only operators');
        FBefore := Pair;
        Change(NyxStudioQueryPolicyFieldID(3, sqpfFiltering), 'true');
        Click(NyxStudioQueryPolicyApplyID(3));
      end;
    5:
      begin
        Check((FCommands.State = nssApplied) and Spec.ColumnAt(3).QueryPolicy.FilterEnabled and
          (Spec.ColumnAt(3).QueryPolicy.ComparisonCount = 1) and
          Spec.ColumnAt(3).QueryPolicy.Allows(nskBoolean, nqcEqual) and
          not Spec.ColumnAt(3).QueryPolicy.Allows(nskBoolean, nqcNotEqual),
          'enabling Boolean filtering retains its previously disabled explicit operator set');
        History;
        FBefore := Pair;
        Click(NyxStudioQueryPolicyDefaultID(-1));
      end;
    6:
      begin
        Check((FCommands.State = nssApplied) and not Spec.HasTableQueryPolicy and
          FSession.Selected.HasCollectionView and Spec.TableQueryPolicy.FilterEnabled and
          Spec.ColumnAt(0).HasQueryPolicy, 'library defaults remove only the local table choice');
        History;
        Expand(0);
        FBefore := Pair;
        Click(NyxStudioQueryPolicyDefaultID(0));
      end;
    7:
      begin
        Check((FCommands.State = nssApplied) and not Spec.ColumnAt(0).HasQueryPolicy and
          (Spec.ColumnAt(0).QueryPolicy.TextMode = nqtExact) and
          Spec.ColumnAt(0).QueryPolicy.SortEnabled and Spec.ColumnAt(1).HasQueryPolicy,
          'column defaults mask inherited explicit choices with a local full spec');
        History;
        FBefore := Pair;
        Change('collection-column-0-title', 'Display name');
      end;
    8:
      begin
        Check((FCommands.State = nssApplied) and (Spec.ColumnAt(0).Title = 'Display name') and
          not Spec.HasTableQueryPolicy and not Spec.ColumnAt(0).HasQueryPolicy and
          Spec.ColumnAt(1).HasQueryPolicy, 'ordinary title edit preserves raw absence and other policies');
        History;
        FBefore := Pair;
        Change('collection-binding-scope', 'Application');
      end;
    9:
      begin
        Check((FCommands.State = nssApplied) and (Spec.Scope = csApplication) and
          Spec.ColumnAt(1).HasQueryPolicy, 'ordinary scope edit preserves surviving explicit policies');
        History;
        FBefore := Pair;
        Click('collection-binding-inherit');
      end;
    10:
      begin
        Check((FCommands.State = nssApplied) and not FSession.Selected.HasCollectionView and
          (Spec.ToData.ToJSON = InitialSpec.ToData.ToJSON),
          'whole-binding Inherit restores current definition independently of library defaults');
        History;
        Result := True;
      end;
  end;
  Trace('step:after', FStage);
  Inc(FStage);
end;

procedure Drive;
begin
  try
    Inc(GPolls);

    if GPolls > 2400 then
    begin
      raise Exception.Create('Policy authoring queue exceeded its completion bound');
    end;

    if GReview.Step then
    begin
      FreeAndNil(GReview);
      WriteLn('PASS ', GChecks, ' actual Studio query-policy checks');
      {$ifdef PAS2JS}
      document.body.setAttribute('data-query-authoring', 'passed');
      document.body.setAttribute('data-query-authoring-checks', IntToStr(GChecks));
      {$endif}
    end
    {$ifdef PAS2JS}else
    begin
      window.setTimeout(@Drive, 25);
    end{$endif};
  except
    on E: Exception do
    begin
      FreeAndNil(GReview);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-query-authoring', 'failed: ' + E.Message);
      {$else}WriteLn('FAIL ', E.Message); ExitCode := 1;{$endif}
    end;
  end;
end;

{$ifdef PAS2JS}
function Closed(AEvent: TJSEvent): Boolean;
begin
  FreeAndNil(GReview);
  Result := True;
end;
{$endif}

begin
  try
    {$ifdef PAS2JS}
    GWidth := window.innerWidth;
    {$else}

    if (ParamCount <> 2) or not TryStrToInt(ParamStr(2), GWidth) or
      ((GWidth <> 390) and (GWidth <> 1280)) then
    begin
      raise Exception.Create('Expected fresh evidence directory and actual client width 1280/390');
    end;
    GEvidence := ExpandFileName(ParamStr(1));

    if DirectoryExists(GEvidence) then
    begin
      raise Exception.Create('Policy evidence directory must be new');
    end;
    Check(ForceDirectories(GEvidence), 'fresh evidence directory is created explicitly');
    Application.Initialize;
    {$endif}
    Trace('descriptor-cases:before');
    DescriptorCases;
    Trace('descriptor-cases:after');
    Trace('admission-cases:before');
    AdmissionCases;
    Trace('admission-cases:after');
    GReview := TReview.Create;
    {$ifdef PAS2JS}
    window.addEventListener('pagehide', @Closed);
    Drive;
    {$else}
    while GReview <> nil do
    begin
      Drive;
      Application.ProcessMessages;
      CheckSynchronize(1);
    end;
    CheckSynchronize;
    {$endif}
  except
    on E: Exception do
    begin
      FreeAndNil(GReview);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-query-authoring', 'failed: ' + E.Message);
      {$else}WriteLn('FAIL ', E.Message); ExitCode := 1;{$endif}
    end;
  end;
end.
