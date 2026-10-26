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
program nyx_data_toolbar_reuse_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifdef PAS2JS}JS, Web, nyx.application.browser, nyx.collections.query.browser,{$else}
  Classes, Interfaces, Forms, Controls, StdCtrls, nyx.application.lcl,
  nyx.collections.query.lcl,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.data, nyx.codec,
  nyx.collections, nyx.collections.view, nyx.collections.view.types,
  nyx.collections.query, nyx.collections.query.session, nyx.collections.selection,
  nyx.data.toolbar, nyx.data.toolbar.query, nyx.collections.query.controls,
  nyx.coordinators, nyx.events, nyx.behavior, nyx.scheduler,
  nyx.publication;

type
  {$ifdef PAS2JS}
  { Ordinary browser App owns its renderer and independent runtime stores. }
  TReviewApplication = TNyxBrowserApplication;
  { Observe only the exact managed browser popup retained by the public facade. }
  TReviewQueryPresenter = INyxBrowserQueryFormPresenter;
  {$else}
  { Ordinary native App mounts inside the fixture's existing LCL message loop. }
  TReviewApplication = TNyxLCLApplication;
  { Observe only the exact managed native popup retained by the public facade. }
  TReviewQueryPresenter = INyxLCLQueryFormPresenter;
  { Turn a widgetset callback failure into a finite assertion, not a modal. }
  TFailures = class
  public
    Message: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
  end;
  {$endif}
  { Application-owned callback observes copied events after the toolbar's
    default. It owns no App, renderer, document or collection view. }
  TSearchCallback = class(TNyxEventCallback)
  public
    Ordinal: Integer;
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;
  { Inject one notification failure after installation. The fixture explicitly
    disconnects its subscription before destroying this borrowed receiver. }
  TNotificationFault = class
  public
    Armed: Boolean;
    procedure Changed(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
  end;

var
  GApps: array[0..1] of TReviewApplication;
  GDocument: TNyxDocument;
  GChecks: Integer;
  GCallbacks: Integer;
  GWidth: Integer = 1280;
  GTable: Boolean;
  {$ifdef PAS2JS}
  GHosts: array[0..1] of TJSHTMLElement;
  {$else}
  GFailures: TFailures;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin
  {$ifndef PAS2JS}

  if GFailures.Message <> '' then
  begin
    raise Exception.Create('Native callback: ' + GFailures.Message);
  end;
  {$endif}

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
  {$ifndef PAS2JS}
  WriteLn('CHECK ', GChecks, ' / ', AReason);
  Flush(Output);
  {$endif}
end;

{$ifndef PAS2JS}
procedure TFailures.Failed(ASender: TObject; AException: Exception);
begin

  if Message = '' then
  begin
    Message := AException.ClassName + ': ' + AException.Message;
  end;
end;
{$endif}

function OwnerID(const AInstance: TNyxText): TNyxText;
begin
  Result := NyxQualifiedID(AInstance, 'tools');
end;

function Rows(AApp: Integer; const AInstance: TNyxText): INyxCollectionView;
begin
  Result := GApps[AApp].View.CollectionView(NyxQualifiedID(AInstance, 'people-list'));
end;

function Toolbar(AApp: Integer; const AInstance: TNyxText): INyxDataToolbarBinding;
begin
  Check(Supports(GApps[AApp].View.CoordinatorBinding(OwnerID(AInstance)),
    INyxDataToolbarBinding, Result), 'reusable occurrence has its specialized toolbar binding');
end;

function RoleID(AApp: Integer; const AInstance, APart: TNyxText;
  const AToolbar: TNyxText = 'tools'): TNyxText;
begin
  Result := GApps[AApp].View.Root.Find(NyxQualifiedID(AInstance, AToolbar)).Part(NyxPart(APart)).ID;
end;

procedure Pump;
begin
  {$ifndef PAS2JS}Application.ProcessMessages; CheckSynchronize(1);{$endif}
end;

procedure TypeSearch(AApp: Integer; const AInstance, AText: TNyxText;
  const AToolbar: TNyxText = 'tools');
var
  LID: TNyxText;
begin
  LID := RoleID(AApp, AInstance, 'search', AToolbar);
  GApps[AApp].View.Reveal(LID, niRuntime);
  {$ifdef PAS2JS}
  TJSHTMLInputElement(GApps[AApp].View.InputFor(LID)).value := AText;
  GApps[AApp].View.InputFor(LID).dispatchEvent(TJSEvent.new('input'));
  {$else}
  TCustomEdit(GApps[AApp].View.InputFor(LID)).Text := AText;
  {$endif}
  Pump;
end;

function QueryBinding(AApp: Integer; const AInstance: TNyxText): INyxDataToolbarQueryBinding;
begin
  Check(Supports(GApps[AApp].View.CoordinatorBinding(OwnerID(AInstance)),
    INyxDataToolbarQueryBinding, Result), 'toolbar exposes copied query context');
end;

{ Use the real toolbar button and actual target popup, not a programmatic reset. }
function OpenQuery(AApp: Integer; const AInstance: TNyxText): INyxCollectionQueryWorkbench;
var
  LID: TNyxText;
begin
  LID := RoleID(AApp, AInstance, NyxDataToolbarQueryDetailsPart);
  GApps[AApp].View.Reveal(LID, niRuntime);
  {$ifdef PAS2JS}
  GApps[AApp].View.ElementFor(LID).click;
  {$else}
  TButton(GApps[AApp].View.ControlFor(LID)).Click;
  {$endif}
  Pump;
  Result := QueryBinding(AApp, AInstance).QueryControls;
  Check((Result <> nil) and (Result.Presenter <> nil) and Result.Presenter.IsOpen,
    'actual toolbar button opens its managed query detail surface');
end;

function QueryNode(const APresenter: INyxQueryFormPresenter; const AID: TNyxText): TNyxNode;
var
  LTarget: TReviewQueryPresenter;
begin
  Check(Supports(APresenter, TReviewQueryPresenter, LTarget),
    'query form exposes its exact platform presentation');
  Result := LTarget.Presentation.Renderer.Root.Find(AID);
  Check(Result <> nil, 'query detail control is present: ' + AID);
end;

procedure ClickQuery(const APresenter: INyxQueryFormPresenter; const AID: TNyxText);
var
  LTarget: TReviewQueryPresenter;
begin
  Check(Supports(APresenter, TReviewQueryPresenter, LTarget),
    'query action uses its exact platform presentation');
  LTarget.Presentation.Renderer.Reveal(AID, niRuntime);
  {$ifdef PAS2JS}
  LTarget.Presentation.Renderer.ElementFor(AID).click;
  {$else}
  TButton(LTarget.Presentation.Renderer.ControlFor(AID)).Click;
  {$endif}
  Pump;
end;

procedure CheckVisibleStatus(AApp: Integer; const AInstance: TNyxText);
var
  LID: TNyxText;
  LText: TNyxText;
begin
  LID := RoleID(AApp, AInstance, NyxDataToolbarQueryStatusPart);
  {$ifdef PAS2JS}
  LText := GApps[AApp].View.ElementFor(LID).textContent;
  {$else}
  LText := TLabel(GApps[AApp].View.ControlFor(LID)).Caption;
  {$endif}
  Check(LText = QueryBinding(AApp, AInstance).QueryStatus,
    'real rendered summary follows the accepted query observation');
end;

procedure ClickClear(AApp: Integer; const AInstance: TNyxText);
var
  LID: TNyxText;
begin
  LID := RoleID(AApp, AInstance, 'clear');
  GApps[AApp].View.Reveal(LID, niRuntime);
  {$ifdef PAS2JS}
  GApps[AApp].View.ElementFor(LID).click;
  {$else}
  TButton(GApps[AApp].View.ControlFor(LID)).Click;
  {$endif}
  Pump;
end;

procedure ChooseSort(AApp: Integer; const AInstance: TNyxText);
var
  LID: TNyxText;
  {$ifndef PAS2JS}
  LSelect: TComboBox;
  {$endif}
begin
  LID := RoleID(AApp, AInstance, 'sort');
  GApps[AApp].View.Reveal(LID, niRuntime);
  {$ifdef PAS2JS}
  TJSHTMLSelectElement(GApps[AApp].View.InputFor(LID)).value := 'Name descending';
  GApps[AApp].View.InputFor(LID).dispatchEvent(TJSEvent.new('change'));
  {$else}
  LSelect := TComboBox(GApps[AApp].View.InputFor(LID));
  LSelect.ItemIndex := LSelect.Items.IndexOf('Name descending');
  Check(LSelect.ItemIndex >= 0, 'native Sort offers the configured typed ordering');
  LSelect.OnChange(LSelect);
  {$endif}
  Pump;
end;

procedure TSearchCallback.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  Inc(GCallbacks);
  Check(GCallbacks = Ordinal, 'application callbacks retain registration order');
  Check((AEvent.Value.AsText = 'AD') and (Rows(0, 'left').Snapshot.Count = 1),
    'each application callback sees the copied input and already filtered result');
end;

procedure TNotificationFault.Changed(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
begin

  if Armed then
  begin
    Armed := False;
    raise ENyxCollection.Create('Deliberate committed query notification failure');
  end;
end;

function Fixture: TNyxDocument;
var
  LWorkspace: INyxCard;
  LList: INyxList;
  LTarget: INyxControl;
  LPage: INyxPage;
  LInstance: INyxComponent;
  LToolbar: INyxDataToolbar;
  LPeople: TNyxCollectionRef;
  LName: TNyxTextFieldRef;
  LOptions: TNyxDataToolbarOptions;
  LIndex: Integer;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Independent people workspaces';
    LPeople := NyxCollection('people');
    LName := NyxTextField('name');
    Result.Collections.Define(LPeople, NyxCollectionSchema.Text(LName, ''), [
      NyxCollectionItem(NyxItem(LPeople, 'ada')).WithValue(LName, 'Ada'),
      NyxCollectionItem(NyxItem(LPeople, 'bea')).WithValue(LName, 'Bea'),
      NyxCollectionItem(NyxItem(LPeople, 'grace')).WithValue(LName, 'Grace')]);
    LOptions := NyxDataToolbarOptions(NyxControl('people-list'), LName)
      .QueryContext(NyxDataToolbarQueryOptions.Display(ntqdDetails))
      .Sort(NyxToolbarSort('Name descending'), NyxSort(LName, nsdDescending));
    LWorkspace := NewNyxCard('people-workspace');
    LWorkspace.Configure.Layout(nlColumn).Gap(8).Padding(12).Done;
    LToolbar := ConfigureNyxDataToolbar(NewNyxDataToolbar('tools'), LOptions);
    LToolbar.Configure.PartName(NyxPart('tools')).Done;
    LWorkspace.Add(LToolbar);
    LToolbar := ConfigureNyxDataToolbar(NewNyxDataToolbar('peer-tools'), LOptions);
    LToolbar.Configure.PartName(NyxPart('peer-tools')).Done;
    LWorkspace.Add(LToolbar);

    if GTable then
    begin
      LTarget := NewNyxTable('people-list');
    end
    else
    begin
      LTarget := NewNyxList('people-list');
    end;
    LTarget.Configure.PartName(NyxPart('people')).Height(96).Done;
    LTarget.Binds.Collection(NyxCollectionView(LPeople).Column(LName, 'People')
      .Selection(nsmMultiple).Scoped(csInstance).Query(NyxCollectionQuery.OrderBy(LName)));
    LWorkspace.Add(LTarget);
    Result.AddComponent(LWorkspace);
    LPage := NewNyxPage('home');
    for LIndex := 0 to 1 do
    begin

      if LIndex = 0 then
      begin
        LInstance := NewNyxComponent('left');
      end
      else
      begin
        LInstance := NewNyxComponent('right');
      end;
      LInstance.Reference := NyxComponent('people-workspace');
      LPage.Add(LInstance);
    end;
    Result.AddPage(LPage);
    LPage := NewNyxPage('away');
    LPage.Add(NewNyxHeading('away-title').WithText('Take a little break'));
    Result.AddPage(LPage);
    { A tempting global target exists, but the reusable toolbar must not fall
      back to it when its exact expansion-prefix target is missing. }
    LWorkspace := NewNyxCard('foreign-workspace');
    LWorkspace.Add(ConfigureNyxDataToolbar(NewNyxDataToolbar('foreign-tools'),
      NyxDataToolbarOptions(NyxControl('outside-people'), LName)));
    Result.AddComponent(LWorkspace);
    LPage := NewNyxPage('refusal');
    LList := NewNyxList('outside-people');
    LList.Binds.Collection(NyxCollectionView(LPeople).Column(LName, 'People'));
    LPage.Add(LList);
    LInstance := NewNyxComponent('foreign');
    LInstance.Reference := NyxComponent('foreign-workspace');
    LPage.Add(LInstance);
    Result.AddPage(LPage);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

procedure Run;
var
  LIndex: Integer;
  LOther: Integer;
  LStores: array of INyxCollection;
  LBinding: INyxDataToolbarBinding;
  LSuccessor: INyxDataToolbarBinding;
  LStream: INyxEventStream;
  LCallback: TSearchCallback;
  LTokens: array of INyxEventSubscription;
  LRoot: TNyxNode;
  LRows: INyxCollectionView;
  LFault: TNotificationFault;
  LFaultToken: INyxCollectionViewSubscription;
  LRefused: Boolean;
  LCommitted: Boolean;
  LAuthored: TNyxText;
  LSession: INyxCollectionQuerySession;
  LSnapshot: INyxCollectionQuerySessionSnapshot;
  LControls: INyxCollectionQueryWorkbench;
  LOldControls: INyxCollectionQueryWorkbench;
  LOldPresenter: INyxQueryFormPresenter;
  LPresenter: INyxQueryFormPresenter;
  LBefore: TNyxQueryWorkbenchObservation;
begin
  GCallbacks := 0;
  GDocument := Fixture;
  SetLength(LStores, 4);
  SetLength(LTokens, 2);
  LAuthored := TNyxCodec.Encode(GDocument);
  try
    for LIndex := 0 to 1 do
    begin
      GApps[LIndex] := TReviewApplication.Create;
      {$ifdef PAS2JS}
      GHosts[LIndex] := TJSHTMLElement(Web.document.createElement('main'));
      Web.document.body.appendChild(GHosts[LIndex]);
      GApps[LIndex].Run(GDocument, GHosts[LIndex]);
      {$else}
      GApps[LIndex].Mount(GDocument);
      GApps[LIndex].Window.ClientWidth := GWidth;
      GApps[LIndex].Window.ClientHeight := 800;
      GApps[LIndex].Window.Show;
      {$endif}
      Pump;
      LStores[LIndex * 2] := Rows(LIndex, 'left').Store;
      LStores[LIndex * 2 + 1] := Rows(LIndex, 'right').Store;
    end;
    for LIndex := 0 to High(LStores) do
    begin
      for LOther := LIndex + 1 to High(LStores) do
      begin
        Check(LStores[LIndex] <> LStores[LOther],
          'all four actual App/reusable collection stores are independent');
      end;
    end;
    LBinding := Toolbar(0, 'left');
    LRows := Rows(0, 'left');
    LStream := GApps[0].View.Events.On(
      NyxControlEvents(RoleID(0, 'left', 'search'), niRuntime), ntChange).Policy(neSequential);
    for LIndex := 0 to 1 do
    begin
      LCallback := TSearchCallback.Create;
      LCallback.Ordinal := LIndex + 1;
      LTokens[LIndex] := LStream.Subscribe(LCallback);
    end;
    TypeSearch(0, 'left', 'AD');
    Check((GCallbacks = 2) and (LStream.Count = 2) and
      (LStream.ExecutionPolicy = neSequential),
      'toolbar preserves multiple caller callbacks and their execution policy');
    for LIndex := 0 to 1 do
    begin
      LTokens[LIndex].Cancel;
    end;
    Check((LRows.Snapshot.Count = 1) and (Rows(0, 'right').Snapshot.Count = 3) and
      (Rows(1, 'left').Snapshot.Count = 3) and (Rows(1, 'right').Snapshot.Count = 3),
      'qualified reusable Search changes only its exact occurrence');
    ClickClear(0, 'left');
    Check(LRows.Snapshot.Count = 3, 'real reusable Clear restores its admitted baseline');
    LFault := TNotificationFault.Create;
    try
      LFaultToken := LRows.Subscribe({$ifdef PAS2JS}@{$endif}LFault.Changed);
      LFault.Armed := True;
      LCommitted := False;
      try
        LBinding.Search('Grace');
      except
        on ENyxPublicationNotification do
        begin
          LCommitted := True;
        end;
      end;
      Check(LCommitted and (LRows.Snapshot.Count = 1) and
        (LRows.Snapshot.ItemAt(0).GetValue(NyxTextField('name')) = 'Grace'),
        'query observer failure reports an already committed reusable Search');
      Check(GCallbacks = 2, 'programmatic publication synthesizes no input callbacks');
    finally
      LFaultToken.Disconnect;
      LFaultToken := nil;
      LFault.Free;
    end;
    ChooseSort(0, 'left');
    CheckVisibleStatus(0, 'left');
    LOldControls := OpenQuery(0, 'left');
    LOldPresenter := LOldControls.Presenter;
    LRoot := GApps[0].View.Root;
    LRefused := False;
    try
      GApps[0].ShowPage('refusal');
    except
      on ENyxCoordinator do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (GApps[0].View.Root = LRoot) and LBinding.Connected and
      (LRows.Snapshot.Count = 1),
      'foreign target refusal preserves the accepted page and active toolbar');
    Check(LOldPresenter.Connected and LOldPresenter.IsOpen and
      (LOldControls.Presenter = LOldPresenter),
      'failed page candidate preserves the accepted query detail form');
    GApps[0].ShowPage('away');
    Pump;
    Check(not LBinding.Connected, 'navigation revokes the former reusable command facade');
    Check(not LOldControls.Connected and not LOldPresenter.Connected and not LOldPresenter.IsOpen,
      'navigation retires the old detail controller and physical form');
    LRefused := False;
    try
      LBinding.Clear;
    except
      on ENyxDataToolbarCancelled do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LRows.Snapshot.Count = 1),
      'old navigation commands cannot rewrite the retained query');
    TypeSearch(1, 'right', 'Bea');
    Check(Rows(1, 'right').Snapshot.Count = 1,
      'another ordinary App remains usable after peer navigation');
    GApps[0].ShowPage('home');
    Pump;
    LSuccessor := Toolbar(0, 'left');
    Check(LSuccessor.Connected and (LSuccessor <> LBinding),
      'returning home creates a fresh command facade without reviving the old one');
    LSession := NyxCollectionQuerySession(Rows(0, 'left'));
    LSnapshot := LSession.Capture;
    Check((LSnapshot.Count = 1) and not LSnapshot.ContributionAt(0).Owner.Connected,
      'previous accepted search is an explicit orphan rather than stolen by a same-named actor');
    Check(LSnapshot.HasUserSort and not LSnapshot.SortOwner.Connected,
      'the former shared sort remains visibly retained under its original owner');
    TypeSearch(0, 'left', 'gr', 'peer-tools');
    ClickClear(0, 'left');
    Check((LSession.Capture.Count = 2) and (Rows(0, 'left').Snapshot.Count = 1),
      'fresh local Clear cannot claim either a retained filter or a live peer filter');
    LControls := OpenQuery(0, 'left');
    LPresenter := LControls.Presenter;
    Check(Pos('retained from earlier activity',
      QueryNode(LPresenter, 'nyx-query-workbench-chip-0').Prop(NyxAttributeName(atText))) > 0,
      'ordinary detail button explains that the old filter is retained');
    Check(Pos('another control',
      QueryNode(LPresenter, 'nyx-query-workbench-chip-1').Prop(NyxAttributeName(atText))) > 0,
      'ordinary detail button identifies an active peer contribution');
    Check(Pos('retained from earlier activity',
      QueryNode(LPresenter, 'nyx-query-workbench-counts-sort').Prop(NyxAttributeName(atText))) > 0,
      'ordinary detail text identifies retained shared sorting independently of filters');
    TypeSearch(0, 'left', 'be', 'peer-tools');
    ClickQuery(LPresenter, 'nyx-query-workbench-chip-0');
    Check((LSession.Capture.Count = 2) and (Rows(0, 'left').Snapshot.Count = 0) and
      LPresenter.Connected and LPresenter.IsOpen,
      'stale captured detail action refuses without changing either accepted filter');
    ClickQuery(LPresenter, 'nyx-query-workbench-reload');
    Check(not LPresenter.Connected, 'explicit Reload retires the previous captured form');
    LPresenter := LControls.Presenter;
    ClickQuery(LPresenter, 'nyx-query-workbench-chip-0');
    Check((LSession.Capture.Count = 1) and (Rows(0, 'left').Snapshot.Count = 1) and
      (Rows(0, 'left').Snapshot.ItemAt(0).GetValue(NyxTextField('name')) = 'Bea'),
      'real detail control removes exactly the retained filter and preserves the live peer');
    CheckVisibleStatus(0, 'left');
    LBefore := LControls.Observation;
    LRefused := False;
    try
      LOldControls.ResetAll;
    except
      on ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and LBefore.SamePolicy(LControls.Observation),
      'retained old detail facade cannot mutate its successor');
    LControls := OpenQuery(0, 'left');
    ClickQuery(LControls.Presenter, 'nyx-query-workbench-clear-all');
    Check((LSession.Capture.Count = 0) and (Rows(0, 'left').Snapshot.Count = 3),
      'visible Clear all user filters deliberately withdraws the remaining peer rule');
    Check(LSession.Capture.HasUserSort,
      'clearing filters preserves shared sorting until a separate deliberate reset');
    LControls := OpenQuery(0, 'left');
    ClickQuery(LControls.Presenter, 'nyx-query-workbench-reset-sort');
    Check(not LSession.Capture.HasUserSort and
      (Rows(0, 'left').Snapshot.ItemAt(0).GetValue(NyxTextField('name')) = 'Ada'),
      'visible Reset shared sorting restores the authored ordering without changing source rows');
    CheckVisibleStatus(0, 'left');
    ClickClear(0, 'left');
    Check(Rows(0, 'left').Snapshot.Count = 3,
      'local Clear remains usable after deliberate removal through actual query controls');
    Check((Rows(0, 'right').Snapshot.Count = 3) and (Rows(1, 'left').Snapshot.Count = 3) and
      (Rows(1, 'right').Snapshot.Count = 1),
      'detail removal preserves independent reusable instances and the peer App');
    Check(TNyxCodec.Encode(GDocument) = LAuthored,
      'ordinary App navigation and queries leave the shared authored document unchanged');
  finally
    for LIndex := 0 to 1 do
    begin
      FreeAndNil(GApps[LIndex]);
      {$ifdef PAS2JS}

      if GHosts[LIndex] <> nil then
      begin
        GHosts[LIndex].remove;
      end;
      {$endif}
    end;
    FreeAndNil(GDocument);
  end;
end;

begin
  try
    {$ifndef PAS2JS}
    Application.Initialize;

    if ParamCount = 1 then
    begin
      GWidth := StrToInt(ParamStr(1));
    end;
    GFailures := TFailures.Create;
    try
      Application.OnException := GFailures.Failed;
      Run;
      GTable := True;
      Run;
      Check(GFailures.Message = '', 'native message dispatch has no unhandled callback failure');
    finally
      Application.OnException := nil;
      GFailures.Free;
    end;
    WriteLn('PASS ', GChecks, ' DataToolbar reusable App checks');
    {$else}
    Run;
    GTable := True;
    Run;
    Web.document.body.setAttribute('data-toolbar-reuse-checks', IntToStr(GChecks));
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      Web.document.body.setAttribute('data-toolbar-reuse-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.ClassName, ': ', LException.Message);
      DumpExceptionBackTrace(StdErr);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
