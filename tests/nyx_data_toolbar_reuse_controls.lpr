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
  {$ifdef PAS2JS}JS, Web, nyx.application.browser,{$else}
  Classes, Interfaces, Forms, Controls, StdCtrls, nyx.application.lcl,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.data, nyx.codec,
  nyx.collections, nyx.collections.view, nyx.collections.view.types,
  nyx.collections.query, nyx.collections.query.session, nyx.collections.selection,
  nyx.data.toolbar, nyx.coordinators, nyx.events, nyx.behavior, nyx.scheduler,
  nyx.publication;

type
  {$ifdef PAS2JS}
  { Ordinary browser App owns its renderer and independent runtime stores. }
  TReviewApplication = TNyxBrowserApplication;
  {$else}
  { Ordinary native App mounts inside the fixture's existing LCL message loop. }
  TReviewApplication = TNyxLCLApplication;
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

function RoleID(AApp: Integer; const AInstance, APart: TNyxText): TNyxText;
begin
  Result := GApps[AApp].View.Root.Find(OwnerID(AInstance)).Part(NyxPart(APart)).ID;
end;

procedure Pump;
begin
  {$ifndef PAS2JS}Application.ProcessMessages; CheckSynchronize(1);{$endif}
end;

procedure TypeSearch(AApp: Integer; const AInstance, AText: TNyxText);
var
  LID: TNyxText;
begin
  LID := RoleID(AApp, AInstance, 'search');
  GApps[AApp].View.Reveal(LID, niRuntime);
  {$ifdef PAS2JS}
  TJSHTMLInputElement(GApps[AApp].View.InputFor(LID)).value := AText;
  GApps[AApp].View.InputFor(LID).dispatchEvent(TJSEvent.new('input'));
  {$else}
  TCustomEdit(GApps[AApp].View.InputFor(LID)).Text := AText;
  {$endif}
  Pump;
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
      .Sort(NyxToolbarSort('Name descending'), NyxSort(LName, nsdDescending));
    LWorkspace := NewNyxCard('people-workspace');
    LWorkspace.Configure.Layout(nlColumn).Gap(8).Padding(12).Done;
    LToolbar := ConfigureNyxDataToolbar(NewNyxDataToolbar('tools'), LOptions);
    LToolbar.Configure.PartName(NyxPart('tools')).Done;
    LWorkspace.Add(LToolbar);
    LList := NewNyxList('people-list');
    LList.Configure.PartName(NyxPart('people')).Height(96).Done;
    LList.Binds.Collection(NyxCollectionView(LPeople).Column(LName, 'People')
      .Selection(nsmMultiple).Scoped(csInstance).Query(NyxCollectionQuery.OrderBy(LName)));
    LWorkspace.Add(LList);
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
  LResetActor: INyxCollectionQueryOwner;
begin
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
    GApps[0].ShowPage('away');
    Pump;
    Check(not LBinding.Connected, 'navigation revokes the former reusable command facade');
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
    { This deliberately explicit global user operation is different from local
      Clear. Ordinary query tooling can remove retained/orphan contributions. }
    LResetActor := LSession.AcquireOwner(NyxQueryOrigin('explicit-reset'), 'Reset all queries');
    PublishNyxGroup([LSession.PrepareSession(LSnapshot, LResetActor,
      NyxQuerySessionEdits.ResetAll)]);
    ClickClear(0, 'left');
    Check(Rows(0, 'left').Snapshot.Count = 3,
      'explicit query reset followed by local Clear restores the baseline after navigation');
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
      Check(GFailures.Message = '', 'native message dispatch has no unhandled callback failure');
    finally
      Application.OnException := nil;
      GFailures.Free;
    end;
    WriteLn('PASS ', GChecks, ' DataToolbar reusable App checks');
    {$else}
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
