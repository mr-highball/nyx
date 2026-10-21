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
program nyx_coordinator_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  SysUtils,
  {$ifdef PAS2JS}JS, Web, nyx.render.browser,
  {$else}Classes, Interfaces, Forms, Controls, StdCtrls, LCLType, LMessages,
    nyx.render.lcl,{$endif}
  nyx.text, nyx.types, nyx.model, nyx.controls, nyx.data, nyx.codec,
  nyx.composition, nyx.collections, nyx.collections.view,
  nyx.collections.bindings, nyx.collections.view.types, nyx.collections.selection,
  nyx.collections.query, nyx.coordinators, nyx.selection.summary,
  nyx.events, nyx.behavior, nyx.scheduler, nyx.publication;

type
  {$ifdef PAS2JS}
  TReviewRenderer = TNyxBrowserRenderer;
  TReviewHost = TJSHTMLElement;
  { Standard MouseEventInit fields omitted by this installed Web declaration.
    The fixture still enters actual DOM row handlers, without running scripts. }
  TReviewMouseOptions = class external name 'Object'(TJSObject)
    bubbles: Boolean;
    cancelable: Boolean;
    ctrlKey: Boolean;
  end;
  TReviewMouseEvent = class external name 'MouseEvent'(TJSMouseEvent)
    constructor new(const AType: String; AOptions: TReviewMouseOptions);
  end;
  {$else}
  TReviewRenderer = TNyxLCLRenderer;
  TReviewHost = TForm;
  { Retain widgetset callback failures instead of opening an exception dialog. }
  TFailureObserver = class
    Failure: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
  end;
  {$endif}
  { Faults enter the ordinary postcommit publication owner, after real controls
    and custom binding ownership have already adopted. No special renderer API. }
  TNotifyAction = (nfaNone, nfaFail, nfaDispose);
  { Wrap an independently authored real factory only to inject a late failure
    or disposal. Return its exact custom binding and preserve every readiness
    assertion, not a fabricated successful/static participant. }
  TReviewPublication = class(TInterfacedObject, INyxPreparedCoordinator,
    INyxPreparedPublication, INyxPublicationReadiness)
  public
    Inner: INyxPreparedCoordinator;
    Fence: INyxPublicationReadiness;
    function GetBinding: INyxCoordinatorBinding;
    procedure Revoke;
    procedure Disconnect;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;
  { The application registers this ordinary trusted Pascal creator. }
  TReviewFactory = class(TInterfacedObject, INyxCoordinatorFactory)
    function Prepare(const AProjection: INyxCoordinatorProjection;
      const AIntent: TNyxDataValue; const AViews: INyxCollectionBindings;
      const AView: INyxPreparedEventView; const AScheduler: INyxScheduler;
      const ARefresh: INyxCoordinatorRefresh): INyxPreparedCoordinator;
  end;
  { An alternate managed registry may release a caller's aliased input slot
    while supplying its immutable snapshot. The admitted collection owner must
    already be retained by RenderFrame, independently of that mutable slot. }
  TAliasRegistry = class(TInterfacedObject, INyxCoordinatorRegistry)
  public
    Inner: INyxCoordinatorRegistry;
    App: Integer;
    procedure RegisterFactory(const AKind: TNyxKindRef;
      const AIntentKey: TNyxExtensionRef; const AFactory: INyxCoordinatorFactory);
    function Snapshot: INyxCoordinatorRegistrySnapshot;
  end;

const
  CSummaryID = 'summary';
  CItemsID = 'summary-items';
  CCountID = 'summary-count';
  CClearID = 'summary-clear';

var
  GDocument: TNyxDocument;
  GRegistry: INyxCoordinatorRegistry;
  GRenderers: array[0..1] of TReviewRenderer;
  GHosts: array[0..1] of TReviewHost;
  { Managed interface vectors remain portable to the pas2js COM interface model. }
  GBindings: array of INyxCollectionBindings;
  GSummaries: array of INyxSelectionSummaryBinding;
  GDisposedBinding: INyxSelectionSummaryBinding;
  GRefuseLabel: Boolean;
  GNotifyAction: TNotifyAction;
  GNotifyCalls: Integer;
  GRetireCalls: Integer;
  GChecks: Integer;
  {$ifndef PAS2JS}GFailures: TFailureObserver; GWidth: Integer;{$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin
  {$ifndef PAS2JS}

  if GFailures.Failure <> '' then
  begin
    raise Exception.Create('Native coordinator callback: ' + GFailures.Failure);
  end;
  {$endif}

  if not ACondition then
  begin
    raise Exception.Create('Coordinator controls: ' + AReason);
  end;
  Inc(GChecks);
end;

function People: TNyxCollectionRef;
begin
  Result := NyxCollection('people');
end;

function PersonName: TNyxTextFieldRef;
begin
  Result := NyxTextField('name');
end;

function Rows(AApp: Integer): INyxCollectionView;
begin
  Result := GBindings[AApp].ViewFor(CItemsID);
end;

function Caption(AApp: Integer): TNyxText;
begin
  {$ifdef PAS2JS}
  Result := GRenderers[AApp].ElementFor(CCountID).textContent;
  {$else}
  Result := TLabel(GRenderers[AApp].ControlFor(CCountID)).Caption;
  {$endif}
end;

function ClearEnabled(AApp: Integer): Boolean;
begin
  {$ifdef PAS2JS}
  Result := not TJSHTMLButtonElement(GRenderers[AApp].ElementFor(CClearID)).disabled;
  {$else}
  Result := GRenderers[AApp].ControlFor(CClearID).Enabled;
  {$endif}
end;

procedure Pump;
begin
  {$ifndef PAS2JS}Application.ProcessMessages; CheckSynchronize(1);{$endif}
end;

{$ifdef PAS2JS}
function UITurn: TJSPromise;
begin
  Result := TJSPromise.new(procedure(AResolve, AReject: TJSPromiseResolver)
    begin
      window.setTimeout(procedure
        begin
          AResolve(JSValue(True));
        end, 0);
    end);
end;
{$endif}

{ Await actual custom owner AND physical text completion, retaining interfaces
  through browser suspension. A fixed delay is never a success predicate. }
procedure AwaitCount(AApp, ACount: Integer); {$ifdef PAS2JS}async;{$endif}
var
  LBinding: INyxSelectionSummaryBinding;
  {$ifdef PAS2JS}LStarted: Double;{$else}LStarted: QWord;{$endif}
begin
  LBinding := GSummaries[AApp];
  {$ifdef PAS2JS}LStarted := TJSDate.now;{$else}LStarted := GetTickCount64;{$endif}
  while (LBinding.SelectedCount <> ACount) or
    (Caption(AApp) <> SelectionSummaryCaption(ACount)) do
  begin
    {$ifdef PAS2JS}

    if TJSDate.now - LStarted > 10000 then
    {$else}

    if GetTickCount64 - LStarted > 10000 then
    {$endif}
    begin
      raise Exception.Create('Actual selection presentation did not complete: expected=' +
        IntToStr(ACount) + ', cached=' + IntToStr(LBinding.SelectedCount) +
        ', physical="' + Caption(AApp) + '", membership=' +
        IntToStr(Rows(AApp).Selection.Count));
    end;
    {$ifdef PAS2JS}await(UITurn);{$else}Pump;{$endif}
  end;
  Check(ClearEnabled(AApp) = (ACount > 0), 'Clear availability matches retained membership');
end;

function TReviewPublication.GetBinding: INyxCoordinatorBinding;
begin
  Result := Inner.Binding;
end;

procedure TReviewPublication.Revoke;
begin
  Inner.Revoke;
end;

procedure TReviewPublication.Disconnect;
begin
  Inner.Disconnect;
end;

procedure TReviewPublication.Validate;
begin
  Inner.Validate;
end;

procedure TReviewPublication.CheckReady;
begin
  Fence.CheckReady;
end;

procedure TReviewPublication.Install;
begin
  Inner.Install;
end;

procedure TReviewPublication.Notify;
var
  LLease: INyxPreparedCoordinator;
  LAction: TNotifyAction;
begin
  LLease := Self;
  Inner.Notify;
  Inc(GNotifyCalls);
  LAction := GNotifyAction;
  GNotifyAction := nfaNone;
  case LAction of
    nfaNone:
      begin
        { Ordinary publication deliberately has no injected callback action. }
      end;
    nfaFail:
      raise ENyxPublication.Create('Deliberate committed creator notification failure');
    nfaDispose:
      begin
        Check(Supports(Inner.Binding, INyxSelectionSummaryBinding, GDisposedBinding),
          'committed custom interface is retained before notification disposes its renderer');
        FreeAndNil(GRenderers[0]);
      end;
  end;
end;

procedure TReviewPublication.Retire;
begin
  Inc(GRetireCalls);
  Inner.Retire;
end;

function TReviewFactory.Prepare(const AProjection: INyxCoordinatorProjection;
  const AIntent: TNyxDataValue; const AViews: INyxCollectionBindings;
  const AView: INyxPreparedEventView; const AScheduler: INyxScheduler;
  const ARefresh: INyxCoordinatorRefresh): INyxPreparedCoordinator;
var
  LFactory: INyxCoordinatorFactory;
  LPlan: TReviewPublication;
begin
  LFactory := NewSelectionSummaryFactory;
  LPlan := TReviewPublication.Create;
  Result := LPlan;
  LPlan.Inner := LFactory.Prepare(AProjection, AIntent, AViews, AView, AScheduler, ARefresh);

  if not Supports(LPlan.Inner, INyxPublicationReadiness, LPlan.Fence) then
  begin
    raise ENyxCoordinator.Create('Example must preserve its complete final readiness assertion');
  end;
end;

procedure TAliasRegistry.RegisterFactory(const AKind: TNyxKindRef;
  const AIntentKey: TNyxExtensionRef; const AFactory: INyxCoordinatorFactory);
begin
  Inner.RegisterFactory(AKind, AIntentKey, AFactory);
end;

function TAliasRegistry.Snapshot: INyxCoordinatorRegistrySnapshot;
var
  LLease: INyxCoordinatorRegistry;
  LInner: INyxCoordinatorRegistry;
begin
  LLease := Self;
  LInner := Inner;
  Result := LInner.Snapshot;
  GBindings[App] := nil;
end;

{$ifdef PAS2JS}
function ReviewLabel(ANode: TNyxNode): TJSHTMLElement;
begin

  if GRefuseLabel then
  begin
    raise ENyxModel.Create('Deliberate physical label factory refusal');
  end;
  Result := TJSHTMLElement(document.createElement('span'));
  Result.textContent := ANode.Prop(NyxAttributeName(atText));
end;

procedure UpdateReviewLabel(ANode: TNyxNode; AControl: TJSHTMLElement);
begin
  { A custom factory owns its physical projection, including subsequent text. }
  AControl.textContent := ANode.Prop(NyxAttributeName(atText));
end;
{$else}
function ReviewLabel(ANode: TNyxNode; AOwner: TComponent): TControl;
begin

  if GRefuseLabel then
  begin
    raise ENyxModel.Create('Deliberate physical label factory refusal');
  end;
  Result := TLabel.Create(AOwner);
  TLabel(Result).Caption := ANode.Prop(NyxAttributeName(atText));
end;

procedure UpdateReviewLabel(ANode: TNyxNode; AControl: TControl);
begin
  { A custom factory owns its physical projection, including subsequent text. }
  TLabel(AControl).Caption := ANode.Prop(NyxAttributeName(atText));
end;

procedure TFailureObserver.Failed(ASender: TObject; AException: Exception);
begin
  Failure := AException.ClassName + ': ' + AException.Message;
end;
{$endif}

procedure SelectTwo(AApp: Integer);
{$ifdef PAS2JS}
var
  LRow: TJSHTMLElement;
  LOptions: TReviewMouseOptions;
{$else}
var
  LList: TListBox;
{$endif}
begin
  {$ifdef PAS2JS}
  LRow := TJSHTMLElement(GRenderers[AApp].ElementFor(CItemsID).
    querySelector('[data-nyx-item="alder"]'));
  Check(LRow <> nil, 'first actual list row exists');
  LRow.click;
  LOptions := TReviewMouseOptions.new;
  LOptions.bubbles := True;
  LOptions.cancelable := True;
  LOptions.ctrlKey := True;
  LRow := TJSHTMLElement(GRenderers[AApp].ElementFor(CItemsID).
    querySelector('[data-nyx-item="birch"]'));
  Check(LRow <> nil, 'second actual list row exists');
  LRow.dispatchEvent(TReviewMouseEvent.new('click', LOptions));
  {$else}
  LList := TListBox(GRenderers[AApp].ControlFor(CItemsID));
  Check(LList.MultiSelect and (LList.Items.Count = 3),
    'ordinary native list exposes its authored multiple-selection contract');
  LList.SetFocus;
  LList.ItemIndex := 0;
  LList.Selected[0] := True;
  LList.Selected[1] := True;
  Check(Assigned(LList.OnSelectionChange), 'actual native selection handler is mounted');
  LList.OnSelectionChange(LList, True);
  {$endif}
  Pump;
end;

procedure ClearByKeyboard(AApp: Integer);
{$ifndef PAS2JS}
var
  LButton: TWinControl;
{$endif}
begin
  Check(ClearEnabled(AApp), 'ordinary Clear is enabled before input');
  {$ifdef PAS2JS}
  { DOM click enters the same default observation. Trusted browser keyboard
    defaults remain an explicitly separate execution/input qualification. }
  GRenderers[AApp].ElementFor(CClearID).click;
  {$else}
  LButton := TWinControl(GRenderers[AApp].ControlFor(CClearID));
  LButton.SetFocus;
  Check(GHosts[AApp].ActiveControl = LButton, 'Clear is an ordinary focusable button');

  if LButton.Perform(CN_KEYDOWN, VK_SPACE, 0) = 0 then
  begin
    LButton.Perform(LM_KEYDOWN, VK_SPACE, 0);
  end;

  if LButton.Perform(CN_KEYUP, VK_SPACE, 0) = 0 then
  begin
    LButton.Perform(LM_KEYUP, VK_SPACE, 0);
  end;
  {$endif}
  Pump;
end;

procedure CaptureBinding(AApp: Integer);
begin
  Check(Supports(GRenderers[AApp].CoordinatorBinding(CSummaryID),
    INyxSelectionSummaryBinding, GSummaries[AApp]),
    'generic mount preserves the creator specialized interface and exact binding');
end;

procedure RenderApp(AApp: Integer);
begin
  GRenderers[AApp].Render(GDocument, GDocument.Pages[0], GHosts[AApp], False,
    GRenderers[AApp].State, GBindings[AApp]);
end;

procedure Cases; {$ifdef PAS2JS}async;{$endif}
var
  LPage: INyxPage;
  LSummary: INyxSelectionSummary;
  LRoot: TNyxNode;
  LOldRoot: TNyxNode;
  LOld: INyxSelectionSummaryBinding;
  LGeneration: INyxCancellationScope;
  LQuery: TNyxCollectionQuery;
  LQueryText: TNyxText;
  LAuthored: TNyxText;
  LRejected: Boolean;
  LApp: Integer;
  LNotifyBefore: Integer;
  LRetireBefore: Integer;
  LBindings: INyxCollectionBindings;
  LAlias: TAliasRegistry;
  LAliasLease: INyxCoordinatorRegistry;
begin
  GDocument := TNyxDocument.Create;
  GDocument.Collections.Define(People, NyxCollectionSchema.Text(PersonName, ''), [
    NyxCollectionItem(NyxItem(People, 'alder')).WithValue(PersonName, 'Alder'),
    NyxCollectionItem(NyxItem(People, 'birch')).WithValue(PersonName, 'Birch'),
    NyxCollectionItem(NyxItem(People, 'cedar')).WithValue(PersonName, 'Cedar')]);
  LPage := NewNyxPage('home');
  LPage.Configure.Padding(16).Gap(12).Done;
  LPage.Add(NewNyxHeading('title').WithText('A thoughtful little selection'));
  LSummary := NewSelectionSummary(CSummaryID, NyxCollectionView(People).
    Column(PersonName, 'People').Selection(nsmMultiple));
  Check((LSummary.ItemsList.ID = CItemsID) and
    (LSummary.CountLabel.ID = CCountID) and (LSummary.ClearButton.ID = CClearID),
    'creator compound exposes specialized ordinary parts');
  LPage.Add(LSummary);
  GDocument.AddPage(LPage);
  LSummary := nil;
  LPage := nil;
  LAuthored := TNyxCodec.Encode(GDocument);
  GRegistry := NewNyxCoordinatorRegistry;
  GRegistry.RegisterFactory(NyxCustomKind(NyxKindName(nkCard)),
    SelectionSummaryIntent, TReviewFactory.Create);
  for LApp := 0 to 1 do
  begin
    LRoot := RealizeNyxView(GDocument, GDocument.Pages[0]);
    try
      GBindings[LApp] := NewNyxCollectionBindings(LRoot,
        NewNyxCollectionContext(GDocument.Collections));
    finally
      LRoot.Free;
    end;
    {$ifdef PAS2JS}
    GHosts[LApp] := TJSHTMLElement(document.createElement('div'));
    GHosts[LApp].style.cssText := 'min-height:600px;width:100%;';
    document.body.appendChild(GHosts[LApp]);
    {$else}
    GHosts[LApp] := TForm.CreateNew(nil);
    GHosts[LApp].SetBounds(40 + LApp * 25, 40 + LApp * 25, GWidth, 800);
    GHosts[LApp].ClientWidth := GWidth;
    {$endif}
    GRenderers[LApp] := TReviewRenderer.Create;
    GRenderers[LApp].CoordinatorRegistry := GRegistry;
    GRenderers[LApp].RegisterFactory(NyxKindName(nkLabel),
      {$ifdef PAS2JS}@{$endif}ReviewLabel, {$ifdef PAS2JS}@{$endif}UpdateReviewLabel);
    RenderApp(LApp);
    {$ifndef PAS2JS}
    GHosts[LApp].Show;
    Check(GHosts[LApp].ClientWidth = GWidth,
      'native owner has the requested actual client width');
    {$endif}
    Pump;
    CaptureBinding(LApp);
    Check(GSummaries[LApp].Connected and (Caption(LApp) = SelectionSummaryCaption(0)) and
      not ClearEnabled(LApp), 'initial text and AND availability were present before input');
  end;
  Check(Rows(0).Store <> Rows(1).Store,
    'separate ordinary applications own independent stores for identical logical keys');
  SelectTwo(0);
  {$ifdef PAS2JS}await(AwaitCount(0, 2));{$else}AwaitCount(0, 2);{$endif}
  Check((Rows(0).Selection.Count = 2) and (GSummaries[1].SelectedCount = 0) and
    not ClearEnabled(1), 'actual multiple selection changes only its own application');
  LQuery := NyxCollectionQuery.Where(NyxWhere(PersonName).EqualTo('Cedar'));
  Rows(0).ConfigureQuery(LQuery);
  {$ifdef PAS2JS}await(AwaitCount(0, 2));{$else}AwaitCount(0, 2);{$endif}
  Check((Rows(0).Snapshot.Count = 1) and (Rows(0).Selection.Count = 2) and
    (Rows(1).Snapshot.Count = 3), 'filtered-out members remain selected and truthfully counted');
  LQueryText := Rows(0).QueryPolicy.ToData.ToJSON;
  ClearByKeyboard(0);
  {$ifdef PAS2JS}await(AwaitCount(0, 0));{$else}AwaitCount(0, 0);{$endif}
  Check((Rows(0).Selection.Count = 0) and (Rows(0).Snapshot.Count = 1) and
    (Rows(0).QueryPolicy.ToData.ToJSON = LQueryText) and (Rows(0).Store.Snapshot.Count = 3),
    'Clear uses existing membership without changing query, rows or data');

  Rows(0).SetSelection([NyxItem(People, 'alder'), NyxItem(People, 'birch')],
    NyxItem(People, 'cedar'), NyxItem(People, 'cedar'));
  {$ifdef PAS2JS}await(AwaitCount(0, 2));{$else}AwaitCount(0, 2);{$endif}
  LBindings := GBindings[0];
  LAlias := TAliasRegistry.Create;
  LAliasLease := LAlias;
  LAlias.Inner := GRegistry;
  LAlias.App := 0;
  GRenderers[0].CoordinatorRegistry := LAliasLease;
  RenderApp(0);
  Check((GBindings[0] = nil) and
    (GRenderers[0].CollectionView(CItemsID).Store = LBindings.ViewFor(CItemsID).Store) and
    (GRenderers[0].CollectionView(CItemsID).Selection.Count = 2) and
    (Caption(0) = SelectionSummaryCaption(2)),
    'snapshot getter can release the aliased caller slot without replacing its admitted data owner');
  GBindings[0] := LBindings;
  GRenderers[0].CoordinatorRegistry := GRegistry;
  LAliasLease := nil;
  LBindings := nil;
  CaptureBinding(0);
  LOld := GSummaries[0];
  LGeneration := NyxEventGeneration(GRenderers[0].Events);
  LOldRoot := GRenderers[0].Root;
  GRefuseLabel := True;
  LRejected := False;
  try
    RenderApp(0);
  except
    on ENyxModel do LRejected := True;
  end;
  GRefuseLabel := False;
  Check(LRejected and (GRenderers[0].Root = LOldRoot) and not LGeneration.Cancelled and
    LOld.Connected and (Rows(0).Selection.Count = 2),
    'hidden physical factory refusal preserves accepted controls, owner, membership and generation');
  RenderApp(0);
  CaptureBinding(0);
  Check(not LOld.Connected and LGeneration.Cancelled and
    (GSummaries[0] <> LOld) and (Caption(0) = SelectionSummaryCaption(2)),
    'replacement prepares current retained count before Build and retires exact old binding');
  LRejected := False;
  try
    LOld.Clear;
  except
    on ENyxCoordinatorCancelled do LRejected := True;
  end;
  Check(LRejected and (Rows(0).Selection.Count = 2), 'retained old custom command cannot clear successor data');

  GRenderers[0].CoordinatorRegistry := NewNyxCoordinatorRegistry;
  Check(GSummaries[0].Connected and (GSummaries[0].SelectedCount = 2),
    'registry replacement does not rewrite a mounted snapshot');
  RenderApp(0);
  Check((GRenderers[0].CoordinatorBinding(CSummaryID) = nil) and not GSummaries[0].Connected and
    (Caption(0) = SelectionSummaryCaption(0)) and (Rows(0).Selection.Count = 2),
    'no registered creator means static saved controls, not executable authority from metadata');
  GRenderers[0].CoordinatorRegistry := GRegistry;
  RenderApp(0);
  CaptureBinding(0);
  Check(Caption(0) = SelectionSummaryCaption(2), 'a later registered candidate adopts current data');

  LOld := GSummaries[0];
  LGeneration := NyxEventGeneration(GRenderers[0].Events);
  LNotifyBefore := GNotifyCalls;
  LRetireBefore := GRetireCalls;
  GNotifyAction := nfaFail;
  LRejected := False;
  try
    RenderApp(0);
  except
    on ENyxPublicationNotification do LRejected := True;
  end;
  CaptureBinding(0);
  Check(LRejected and LGeneration.Cancelled and not LOld.Connected and GSummaries[0].Connected and
    (GNotifyCalls > LNotifyBefore) and (GRetireCalls > LRetireBefore),
    'notification failure reports committed successor while independent retirement still completes');
  ClearByKeyboard(0);
  {$ifdef PAS2JS}await(AwaitCount(0, 0));{$else}AwaitCount(0, 0);{$endif}
  Check(Rows(0).Selection.Count = 0, 'new actual controls remain usable after committed failure');
  GNotifyAction := nfaDispose;
  RenderApp(0);
  Check((GRenderers[0] = nil) and (GDisposedBinding <> nil) and not GDisposedBinding.Connected,
    'postcommit creator notification can dispose its renderer without retaining the host');
  LRejected := False;
  try
    GDisposedBinding.Clear;
  except
    on ENyxCoordinatorCancelled do LRejected := True;
  end;
  Check(LRejected and GSummaries[1].Connected and (Rows(1).Snapshot.Count = 3),
    'disposed custom commands refuse and independent application remains usable');
  Check(TNyxCodec.Encode(GDocument) = LAuthored,
    'runtime presentation, query, input, refusal and cleanup never rewrite authored defaults');
end;

procedure Release;
var
  LApp: Integer;
begin
  GRefuseLabel := False;
  GNotifyAction := nfaNone;
  for LApp := 0 to 1 do
  begin
    FreeAndNil(GRenderers[LApp]);
    GSummaries[LApp] := nil;
    GBindings[LApp] := nil;
    {$ifdef PAS2JS}

    if GHosts[LApp] <> nil then
    begin
      GHosts[LApp].remove;
      GHosts[LApp] := nil;
    end;
    {$else}FreeAndNil(GHosts[LApp]);{$endif}
  end;
  GDisposedBinding := nil;
  GRegistry := nil;
  FreeAndNil(GDocument);
end;

procedure Run; {$ifdef PAS2JS}async;{$endif}
begin
  try
    try
      {$ifdef PAS2JS}await(Cases);
      document.body.setAttribute('data-coordinator-controls', 'passed');
      document.body.setAttribute('data-coordinator-checks', IntToStr(GChecks));
      {$else}Cases;
      WriteLn('PASS ', GChecks, ' ordinary coordinator controls / width=', GWidth);{$endif}
    except
      on E: Exception do
      begin
        {$ifdef PAS2JS}
        document.body.setAttribute('data-coordinator-controls', 'failed');
        document.body.setAttribute('data-coordinator-error', E.Message);
        {$else}
        WriteLn('FAIL ', GChecks, ' ', E.Message);
        DumpExceptionBackTrace(Output);
        ExitCode := 1;
        {$endif}
      end;
    end;
  finally
    Release;
  end;
end;

begin
  SetLength(GBindings, 2);
  SetLength(GSummaries, 2);
  {$ifndef PAS2JS}
  Application.Initialize;
  GWidth := 1280;

  if ParamCount > 0 then
  begin
    GWidth := StrToInt(ParamStr(1));
  end;
  GFailures := TFailureObserver.Create;
  Application.OnException := GFailures.Failed;
  try
    Run;
  finally
    Application.OnException := nil;
    GFailures.Free;
  end;
  {$else}Run;{$endif}
end.
