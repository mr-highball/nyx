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

program nyx_collection_publication_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.state, nyx.publication, nyx.resources,
  nyx.resources.rows, nyx.collections, nyx.collections.view,
  nyx.collections.view.types, nyx.collections.query, nyx.collections.mount,
  nyx.model, nyx.controls, nyx.codec
  {$ifdef PAS2JS}, JS, Web, nyx.render.browser
  {$else}, Interfaces, Forms, Grids, nyx.render.lcl{$endif};

const
  CInitial: TNyxText = '{"people":[{"id":"ada","name":"Ada","score":9,"parent":""},{"id":"sam","name":"Sam","score":7,"parent":"ada"}]}';
  CUpdated: TNyxText = '{"people":[{"id":"ada","name":"Ada 🌙","score":10,"parent":""},{"id":"sam","name":"Sam","score":8,"parent":"ada"}]}';
  CBroken: TNyxText = '{"people":[{"id":"ada","name":"Must not appear","score":10,"parent":"missing"},{"id":"sam","name":"Sam","score":8,"parent":"ada"}]}';

type
  {$ifdef PAS2JS}
  TRenderer = TNyxBrowserRenderer;
  THost = TJSHTMLElement;
  {$else}
  TRenderer = TNyxLCLRenderer;
  THost = TForm;
  {$endif}

  { Receivers are borrowed. Tokens disconnect before this probe dies, and
    managed views/stores may outlive their rendered host independently. }
  TProbe = class
  public
    Left: INyxCollection;
    Right: INyxCollection;
    LeftView: INyxCollectionView;
    RightView: INyxCollectionView;
    Stage: INyxPreparedPublication;
    Calls: Integer;
    Validations: Integer;
    ExpectedRevision: Integer;
    ExpectedValidationRevision: Integer;
    Coherent: Boolean;
    ReentryRefused: Boolean;
    Fail: Boolean;
    EmptyFailure: Boolean;
    CancelValidation: Boolean;
    ReenterValidation: Boolean;
    DropStage: Boolean;
    DropGroup: Boolean;
    Cancel: INyxCollectionSubscription;
    procedure Changed(const AStore: INyxCollection; const AChanges: INyxCollectionChanges);
    procedure Validate(const AData: INyxCollectionSnapshot; const AChanges: INyxCollectionChanges);
  end;

var
  GChecks: Integer;
  GGroup: array of INyxPreparedPublication;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxCollection.Create('Publication qualification: ' + AReason);
  end;
  Inc(GChecks);
end;

function Recipe: TNyxResourceRows;
begin
  Result := NyxResourceRows(NyxResourceRef('team')).Field('people')
    .Identity(NyxResourcePath.Field('id')).Text(NyxTextField('name'))
    .Integer(NyxIntegerField('score')).Text(NyxTextField('parent'));
end;

procedure TProbe.Changed(const AStore: INyxCollection; const AChanges: INyxCollectionChanges);
begin
  Inc(Calls);
  Coherent := Coherent and (Left.Snapshot.Revision = ExpectedRevision)
    and (Right.Snapshot.Revision = ExpectedRevision)
    and (LeftView.Snapshot.Revision = ExpectedRevision)
    and (RightView.Snapshot.Revision = ExpectedRevision);
  try
    RightView.ClearSelection;
  except
    on ENyxCollection do
    begin
      ReentryRefused := True;
    end;
  end;

  if Cancel <> nil then
  begin
    Cancel.Disconnect;
    Cancel := nil;
  end;

  if DropStage then
  begin
    Stage := nil;
  end;

  if DropGroup then
  begin
    GGroup := nil;
  end;

  if Fail then
  begin

    if EmptyFailure then
    begin
      raise ENyxCollection.Create('');
    end;
    raise ENyxCollection.Create('Receiver 🌙');
  end;
end;

procedure TProbe.Validate(const AData: INyxCollectionSnapshot; const AChanges: INyxCollectionChanges);
begin
  Inc(Validations);
  Check((Left.Snapshot.Revision = ExpectedValidationRevision)
    and (Right.Snapshot.Revision = ExpectedValidationRevision),
    'validators observe the complete old model');

  if ReenterValidation then
  begin
    try
      Stage.Validate;
    except
      on ENyxCollection do
      begin
        ReentryRefused := True;
      end;
    end;
  end;

  if CancelValidation then
  begin
    Stage.Retire;
  end;
end;

{ These cases exercise public ownership/admission failure paths independently
  of the target renderer. The target journey below qualifies mounted controls. }
procedure Lifetime;
var
  LStore: INyxCollection;
  LOther: INyxCollection;
  LAtomic: INyxAtomicCollection;
  LResources: INyxResources;
  LStage: INyxPreparedPublication;
  LSecond: INyxPreparedPublication;
  LView: INyxCollectionView;
  LRefused: Boolean;
begin
  LResources := NewNyxResources;
  LResources.Define(NyxResourceRef('team'), NyxJSONResource(CInitial));
  LStore := NewNyxCollection(Recipe.Read(LResources, NyxCollection('left'),
    NyxDefaultLocale, NyxDefaultLocale));
  LOther := LStore.Clone;
  Check(Supports(LStore, INyxAtomicCollection, LAtomic), 'built-in stores offer optional coordinated capability');
  LResources.Define(NyxResourceRef('team'), NyxJSONResource(CUpdated));
  LStage := Recipe.PrepareReload(LStore, LResources, NyxDefaultLocale, NyxDefaultLocale, 0);
  Check(LAtomic.Busy and (LStore.Snapshot.Revision = 0), 'preparation reserves without publication');
  LRefused := False;
  try
    LStore.Append(NyxCollectionItem(NyxItem(NyxCollection('left'), 'extra')));
  except
    on ENyxCollection do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'reserved stores reject competing commands');
  LStage := nil;
  Check(not LAtomic.Busy, 'abandoned preparation retires reservation');
  LStage := Recipe.PrepareReload(LStore, LResources, NyxDefaultLocale, NyxDefaultLocale, 0);
  LRefused := False;
  try
    PublishNyxGroup([LStage, LStage]);
  except
    on ENyxPublication do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and not LAtomic.Busy and (LStore.Snapshot.Revision = 0),
    'duplicate participant rejects before any installation and retires');
  LStage := Recipe.PrepareReload(LStore, LResources, NyxDefaultLocale, NyxDefaultLocale, 0);
  LRefused := False;
  try
    PublishNyxGroup([LStage, nil]);
  except
    on ENyxPublication do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and not LAtomic.Busy, 'missing participant retires every supplied preparation');
  LStage := Recipe.PrepareReload(LStore, LResources, NyxDefaultLocale, NyxDefaultLocale, 0);
  LSecond := Recipe.PrepareReload(LOther, LResources, NyxDefaultLocale, NyxDefaultLocale, 0);
  LRefused := False;
  try
    LView := NewNyxCollectionView(LStore, NyxCollectionView(NyxCollection('left'))
      .Column(NyxTextField('name'), 'Name'), cpTable);
  except
    on ENyxCollection do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LView = nil), 'new views cannot bypass reserved validator membership');
  LStage.Retire;
  LSecond.Retire;
  LStage := nil;
  LSecond := nil;
  LRefused := False;
  try
    Recipe.PrepareReload(LStore, LResources, NyxDefaultLocale, NyxDefaultLocale, 9);
  except
    on ENyxResource do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and not LAtomic.Busy, 'stale recipe preparation leaves the store idle');
end;

procedure ReservationCases;
var
  LResources: INyxResources;
  LStore: INyxCollection;
  LAtomic: INyxAtomicCollection;
  LView: INyxCollectionView;
  LStage: INyxPreparedPublication;
  LProbe: TProbe;
  LToken: INyxCollectionSubscription;
  LRefused: Boolean;
begin
  LResources := NewNyxResources;
  LResources.Define(NyxResourceRef('team'), NyxJSONResource(CInitial));
  LStore := NewNyxCollection(Recipe.Read(LResources, NyxCollection('left'),
    NyxDefaultLocale, NyxDefaultLocale));
  Supports(LStore, INyxAtomicCollection, LAtomic);
  LView := NewNyxCollectionView(LStore, NyxCollectionView(NyxCollection('left'))
    .Column(NyxTextField('name'), 'Name'), cpTable);
  LResources.Define(NyxResourceRef('team'), NyxJSONResource(CUpdated));
  LStage := Recipe.PrepareReload(LStore, LResources, NyxDefaultLocale, NyxDefaultLocale, 0);
  LRefused := False;
  try
    LView.Select(NyxItem(NyxCollection('left'), 'ada'));
  except
    on ENyxCollection do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and not LView.HasSelection, 'prepared views reject competing selection');
  LRefused := False;
  try
    LView.ConfigureQuery(NyxCollectionQuery.OrderBy(NyxTextField('name')));
  except
    on ENyxCollection do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'prepared views reject competing query');
  LStage.Validate;
  LView := nil;
  LStage.Install;
  LStage.Notify;
  LStage.Retire;
  Check(not LAtomic.Busy and (LStore.Snapshot.Revision = 1),
    'disconnecting a prepared view revokes its borrowed installer safely');

  LProbe := TProbe.Create;
  try
    LProbe.Left := LStore;
    LProbe.Right := LStore;
    LProbe.ExpectedRevision := 1;
    LProbe.ExpectedValidationRevision := 1;
    LToken := LStore.Subscribe(nil, LProbe.Validate);
    LResources.Define(NyxResourceRef('team'), NyxJSONResource(CInitial));
    LProbe.Stage := Recipe.PrepareReload(LStore, LResources, NyxDefaultLocale, NyxDefaultLocale, 1);
    LProbe.ReenterValidation := True;
    LProbe.Stage.Validate;
    Check(LProbe.ReentryRefused, 'nested validation refuses without repeating receivers');
    LProbe.Stage.Retire;
    LProbe.Stage := Recipe.PrepareReload(LStore, LResources, NyxDefaultLocale, NyxDefaultLocale, 1);
    LProbe.ReenterValidation := False;
    LProbe.CancelValidation := True;
    LRefused := False;
    try
      PublishNyxGroup([LProbe.Stage]);
    except
      on ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and not LAtomic.Busy and (LStore.Snapshot.Revision = 1),
      'retirement inside validation refuses before model publication');
    LToken.Disconnect;
    LToken := nil;
  finally
    LToken := nil;
    LProbe.Free;
  end;
end;

procedure Controls;
var
  LDocument: TNyxDocument;
  LPage: INyxColumn;
  LResources: INyxResources;
  LBad: INyxResources;
  LLeft: INyxCollection;
  LRight: INyxCollection;
  LSibling: INyxCollection;
  LLeftView: INyxCollectionView;
  LRightView: INyxCollectionView;
  LTree: INyxCollectionView;
  LTreeSelection: TNyxItemRef;
  LLeftMount: INyxCollectionMount;
  LRightMount: INyxCollectionMount;
  LLeftToken: INyxCollectionSubscription;
  LRightToken: INyxCollectionSubscription;
  LStages: array of INyxPreparedPublication;
  LProbe: TProbe;
  LRenderer: TRenderer;
  LHost: THost;
  LRefused: Boolean;
  LError: TNyxText;
  LBefore: TNyxText;
  LRefreshLeft: Integer;
  LRefreshRight: Integer;
  {$ifdef PAS2JS}
  LLeftControl: TJSHTMLElement;
  LRightControl: TJSHTMLElement;
  {$else}
  LLeftControl: TStringGrid;
  LRightControl: TStringGrid;
  {$endif}

  procedure Cell(AFirst: Boolean; const AExpected: TNyxText);
  begin
    {$ifdef PAS2JS}

    if AFirst then
    begin
      Check(Pos(AExpected, LLeftControl.textContent) > 0, 'left browser table renders accepted text');
    end
    else
    begin
      Check(Pos(AExpected, LRightControl.textContent) > 0, 'right browser table renders accepted text');
    end;
    {$else}

    if AFirst then
    begin
      Check(TNyxText(LLeftControl.Cells[0, 1]) = AExpected, 'left native table renders accepted text');
    end
    else
    begin
      Check(TNyxText(LRightControl.Cells[0, 1]) = AExpected, 'right native table renders accepted text');
    end;
    {$endif}
  end;

begin
  SetLength(LStages, 2);
  LDocument := TNyxDocument.Create;
  LProbe := TProbe.Create;
  LRenderer := TRenderer.Create;
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('div'));
  document.body.appendChild(LHost);
  {$else}
  LHost := TForm.CreateNew(nil);
  LHost.SetBounds(0, 0, 800, 600);
  {$endif}
  try
    LDocument.Title := 'Coordinated resource tables';
    LDocument.Resources.Define(NyxResourceRef('team'), NyxJSONResource(CInitial));
    LPage := NewNyxColumn('home');
    LPage.Add(NewNyxTable('left-table')).Add(NewNyxTable('right-table'));
    LDocument.AddPage(LPage);
    LBefore := TNyxCodec.Encode(LDocument);
    LResources := LDocument.Resources.Clone;
    LLeft := NewNyxCollection(Recipe.Read(LResources, NyxCollection('left'),
      NyxDefaultLocale, NyxDefaultLocale));
    LRight := NewNyxCollection(Recipe.Read(LResources, NyxCollection('right'),
      NyxDefaultLocale, NyxDefaultLocale));
    LSibling := LLeft.Clone;
    LLeftView := NewNyxCollectionView(LLeft, NyxCollectionView(NyxCollection('left'))
      .Column(NyxTextField('name'), 'Name').Column(NyxIntegerField('score'), 'Score'), cpTable);
    LRightView := NewNyxCollectionView(LRight, NyxCollectionView(NyxCollection('right'))
      .Column(NyxTextField('name'), 'Name'), cpTable);
    LTree := NewNyxCollectionView(LRight, NyxCollectionView(NyxCollection('right'))
      .Column(NyxTextField('name'), 'Name').Parent(NyxTextField('parent')), cpTree);
    LTreeSelection := NyxItem(NyxCollection('right'), 'sam');
    LTree.Select(LTreeSelection);
    LLeftView.Select(NyxItem(NyxCollection('left'), 'ada'));
    LProbe.Left := LLeft;
    LProbe.Right := LRight;
    LProbe.LeftView := LLeftView;
    LProbe.RightView := LRightView;
    LProbe.Coherent := True;
    LProbe.ExpectedRevision := 0;
    LLeftToken := LLeft.Subscribe(LProbe.Changed, LProbe.Validate);
    LRightToken := LRight.Subscribe(LProbe.Changed, LProbe.Validate);
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    LLeftMount := LRenderer.BindCollection('left-table', LLeftView);
    LRightMount := LRenderer.BindCollection('right-table', LRightView);
    {$ifdef PAS2JS}
    LLeftControl := LRenderer.ElementFor('left-table');
    LRightControl := LRenderer.ElementFor('right-table');
    {$else}
    LLeftControl := TStringGrid(LRenderer.ControlFor('left-table'));
    LRightControl := TStringGrid(LRenderer.ControlFor('right-table'));
    {$endif}
    Cell(True, 'Ada');
    Cell(False, 'Ada');
    LRefreshLeft := LLeftMount.RefreshCount;
    LRefreshRight := LRightMount.RefreshCount;
    LResources.Define(NyxResourceRef('team'), NyxJSONResource(CUpdated));
    LBad := LResources.Clone;
    LBad.Define(NyxResourceRef('team'), NyxJSONResource(CBroken));
    LStages[0] := Recipe.PrepareReload(LLeft, LResources, NyxDefaultLocale, NyxDefaultLocale, 0);
    LStages[1] := Recipe.PrepareReload(LRight, LBad, NyxDefaultLocale, NyxDefaultLocale, 0);
    LRefused := False;
    try
      PublishNyxGroup(LStages);
    except
      on ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LProbe.Calls = 0) and (LLeft.Snapshot.Revision = 0)
      and (LRight.Snapshot.Revision = 0), 'invalid later tree rejects the complete group');
    Check((LLeftMount.RefreshCount = LRefreshLeft)
      and (LRightMount.RefreshCount = LRefreshRight), 'refused group does not synchronize either mounted control');
    Check(LTree.Selected.ID = LTreeSelection.ID, 'refusal preserves exact selected identity');
    Check(NyxTreeHierarchy(LTree).IsExpanded(NyxItem(NyxCollection('right'), 'ada')),
      'refusal preserves existing tree disclosure');
    Cell(True, 'Ada');
    Cell(False, 'Ada');
    LStages[0] := Recipe.PrepareReload(LLeft, LResources, NyxDefaultLocale, NyxDefaultLocale, 0);
    LStages[1] := Recipe.PrepareReload(LRight, LResources, NyxDefaultLocale, NyxDefaultLocale, 0);
    { Validators read old revisions. Observers must read new revisions of BOTH
      stores/views even when their own store is notified first. }
    LProbe.ExpectedRevision := 1;
    PublishNyxGroup(LStages);
    Check(LProbe.Coherent and (LProbe.Calls = 2), 'first observer sees the complete accepted group');
    Check(LProbe.ReentryRefused, 'cross-view mutation refuses throughout group notifications');
    Check(LLeftView.Selected.ID = 'ada', 'successful group retains stable table selection');
    Check((LLeftMount.RefreshCount = LRefreshLeft + 1)
      and (LRightMount.RefreshCount = LRefreshRight + 1), 'each mounted table synchronizes once');
    Cell(True, 'Ada 🌙');
    Cell(False, 'Ada 🌙');
    {$ifdef PAS2JS}
    Check((LLeftControl = LRenderer.ElementFor('left-table'))
      and (LRightControl = LRenderer.ElementFor('right-table')), 'group preserves target element identity');
    {$else}
    Check((LLeftControl = LRenderer.ControlFor('left-table'))
      and (LRightControl = LRenderer.ControlFor('right-table')), 'group preserves target control identity');
    {$endif}
    Check(LSibling.Snapshot.Item(NyxItem(NyxCollection('left'), 'ada'))
      .GetValue(NyxTextField('name')) = 'Ada', 'group leaves an independent sibling store unchanged');

    { Configure borrowed subscriptions before reserving participant membership. }
    LLeftToken.Disconnect;
    LRightToken.Disconnect;
    LLeftToken := LLeft.Subscribe(LProbe.Changed);
    LRightToken := LRight.Subscribe(LProbe.Changed);
    LResources.Define(NyxResourceRef('team'), NyxJSONResource(CInitial));
    LStages[0] := Recipe.PrepareReload(LLeft, LResources, NyxDefaultLocale, NyxDefaultLocale, 1);
    LStages[1] := Recipe.PrepareReload(LRight, LResources, NyxDefaultLocale, NyxDefaultLocale, 1);
    LProbe.ExpectedRevision := 2;
    LProbe.Fail := True;
    LError := '';
    try
      PublishNyxGroup(LStages);
    except
      on LException: ENyxPublicationNotification do
      begin
        {$ifdef PAS2JS}LError := LException.Message;{$else}
        LError := RawByteString(LException.Message);
        SetCodePage(RawByteString(LError), CP_UTF8, False);
        {$endif}
      end;
    end;
    Check(Pos('Receiver 🌙', LError) > 0, 'notification diagnostics preserve supplementary Unicode');
    Check((LProbe.Calls = 4) and (LLeft.Snapshot.Revision = 2)
      and (LRight.Snapshot.Revision = 2), 'notification failures do not skip another participant');
    Cell(True, 'Ada');
    Cell(False, 'Ada');
    LProbe.Fail := False;
    LProbe.EmptyFailure := True;
    LProbe.Fail := True;
    LResources.Define(NyxResourceRef('team'), NyxJSONResource(CUpdated));
    LStages[0] := Recipe.PrepareReload(LLeft, LResources, NyxDefaultLocale, NyxDefaultLocale, 2);
    LStages[1] := Recipe.PrepareReload(LRight, LResources, NyxDefaultLocale, NyxDefaultLocale, 2);
    LProbe.ExpectedRevision := 3;
    LRefused := False;
    try
      PublishNyxGroup(LStages);
    except
      on ENyxPublicationNotification do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LProbe.Calls = 6), 'empty observer errors still report committed publication');
    LProbe.Fail := False;
    LProbe.DropGroup := True;
    LResources.Define(NyxResourceRef('team'), NyxJSONResource(CInitial));
    SetLength(GGroup, 2);
    GGroup[0] := Recipe.PrepareReload(LLeft, LResources, NyxDefaultLocale, NyxDefaultLocale, 3);
    GGroup[1] := Recipe.PrepareReload(LRight, LResources, NyxDefaultLocale, NyxDefaultLocale, 3);
    LProbe.ExpectedRevision := 4;
    PublishNyxGroup(GGroup);
    Check((Length(GGroup) = 0) and (LProbe.Calls = 8) and LProbe.Coherent,
      'borrowed observers may drop the entire caller vector without skipping participants');
    LRenderer.Unmount;
    Check(not LLeftMount.Connected and not LRightMount.Connected,
      'mount retirement disconnects both physical receivers');
    LLeftToken.Disconnect;
    LRightToken.Disconnect;
    LStages[0] := Recipe.PrepareReload(LLeft, LResources, NyxDefaultLocale, NyxDefaultLocale, 4);
    LStages[1] := Recipe.PrepareReload(LRight, LResources, NyxDefaultLocale, NyxDefaultLocale, 4);
    PublishNyxGroup(LStages);
    Check((LLeft.Snapshot.Revision = 4) and (LRight.Snapshot.Revision = 4),
      'unchanged prepared datasets are revision/notification no-ops');
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'runtime groups preserve exact authored resources/document');
  finally
    LStages[0] := nil;
    LStages[1] := nil;
    LLeftToken := nil;
    LRightToken := nil;
    LProbe.Free;
    LLeftMount := nil;
    LRightMount := nil;
    LRenderer.Free;
    {$ifdef PAS2JS}LHost.remove;{$else}LHost.Free;{$endif}
    LDocument.Free;
  end;
end;

begin
  {$ifndef PAS2JS}Application.Initialize;{$endif}
  try
    Lifetime;
    ReservationCases;
    Controls;
    WriteLn('PASS / coordinated resource collection publication / ', GChecks, ' checks');
    {$ifdef PAS2JS}document.body.setAttribute('data-test-result', 'passed');{$endif}
  except
    on LException: Exception do
    begin
      WriteLn('FAIL / ', LException.Message);
      {$ifdef PAS2JS}document.body.setAttribute('data-test-result', 'failed');{$else}
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
