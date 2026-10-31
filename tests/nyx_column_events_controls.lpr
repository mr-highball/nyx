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
program nyx_column_events_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifdef PAS2JS}JS, Web, nyx.application.browser, nyx.test.keyboard.browser,
  nyx.collections.arrangement.browser,
  {$else}Interfaces, Forms, Controls, StdCtrls, Classes, nyx.application.lcl,
  nyx.collections.arrangement.lcl,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.state, nyx.codec,
  nyx.behavior, nyx.events, nyx.scheduler, nyx.publication,
  nyx.collections, nyx.collections.view, nyx.collections.mount,
  nyx.collections.arrangement, nyx.collections.column.events,
  nyx.collections.arrangement.presenter, nyx.collections.arrangement.controls,
  { The same runtime qualification consumes either the source-unit fixture or
    the exact accepted unit exported from the live Studio/MCP project. }
  {$ifdef NYX_MCP_COLUMN_EVENTS}nyx.generated.view{$else}
  nyx.generated.column.events{$endif};

type
  { UI-owned callback probe borrows the application through this test's frame.
    Retained event data must survive cancellation and same-ID remounts. }
  TProbe = class(TNyxEventCallback)
  public
    Calls: Integer;
    Last: TNyxEventInfo;
    Fail: Boolean;
    Retire: Boolean;
    Sibling: INyxEventSubscription;
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;
  { Worker uses only immutable copied event data. It never reads the application,
    test globals, widget handles or state. UI observes it after terminal status. }
  TDetachedProbe = class(TNyxEventCallback)
  public
    Payload: TNyxColumnChangesSnapshot;
    FieldName: TNyxText;
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

const
  CField: TNyxText = 'service 🌙';

var
  GChecks: Integer;
  GDocument: TNyxDocument;
  {$ifdef PAS2JS}
  GApp: TNyxBrowserApplication;
  GHost: TJSHTMLElement;
  {$else}
  GApp: TNyxLCLApplication;
  {$endif}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Column controls: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure TProbe.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin
  Inc(Calls);
  Last := AEvent.Copy;
  Check((Last.Trigger = ntColumnsChanged) and Last.CollectionColumns.Defined and
    not NyxEventResponse(AExecution).CanConsume,
    'real adapter delivers a committed typed notification without veto authority');

  if Sibling <> nil then
  begin
    Sibling.Cancel;
    Sibling := nil;
  end;

  if Fail then
  begin
    raise Exception.Create('Expected independent event callback failure');
  end;

  if Retire then
  begin
    GApp.View.Unmount;
  end;
end;

procedure TDetachedProbe.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin

  if AExecution.Cancelled then
  begin
    Exit;
  end;
  Payload := AEvent.CollectionColumns;

  if not Payload.Defined or (Payload.Count = 0) then
  begin
    raise Exception.Create('Worker requires an owned column delta');
  end;
  FieldName := Payload.ChangeAt(0).Field.Name;
end;

{$ifdef PAS2JS}
function Frame: TJSPromise;
begin
  Result := TJSPromise.new(procedure(AResolve, AReject: TJSPromiseResolver)
    begin
      window.requestAnimationFrame(procedure(ATime: Double)
        begin
          AResolve(JSValue(True));
        end);
    end);
end;
{$endif}

procedure Drain; {$ifdef PAS2JS}async;{$endif}
{$ifndef PAS2JS}var LIndex: Integer;{$endif}
begin
  {$ifdef PAS2JS}
  await(Frame);
  await(Frame);
  await(Frame);
  {$else}
  for LIndex := 1 to 8 do
  begin
    Application.ProcessMessages;
    Application.Idle(False);
    CheckSynchronize(0);
  end;
  {$endif}
end;

{$ifdef PAS2JS}
function ContinueCapture(AEvent: TJSMouseEvent): Boolean;
begin
  document.body.setAttribute('data-column-capture', 'continue');
  Result := True;
end;

{ Pause this same compiled application for optional visible control input and
  a representative image. No second document or mirrored event state is used. }
procedure Capture; async;
var
  LButton: TJSHTMLElement;
  LStarted: Double;
begin

  if Pos('capture=1', window.location.search) = 0 then
  begin
    Exit;
  end;
  document.body.setAttribute('data-column-capture', 'ready');
  LButton := TJSHTMLElement(document.createElement('button'));
  LButton.textContent := 'Continue column checks';
  LButton.onclick := @ContinueCapture;
  { Keep this optional harness control reachable even when the child browsing
    context is taller than the outer automation viewport. }
  LButton.style.cssText := 'position:fixed;right:12px;top:8px;z-index:1000';
  document.body.appendChild(LButton);
  LStarted := window.performance.now;
  try
    repeat
      await(Frame);

      if window.performance.now - LStarted > 120000 then
      begin
        raise Exception.Create('Column capture acknowledgment timed out');
      end;
    until document.body.getAttribute('data-column-capture') = 'continue';
  finally
    LButton.onclick := nil;
    LButton.remove;
  end;
end;
{$endif}

{ Keep compiler-created fluent result leases inside this short command frame. }
procedure Configure(const AView: INyxCollectionView; const APreferences: TNyxColumnPreferences);
var
  LResult: INyxCollectionColumns;
begin
  LResult := NyxCollectionColumns(AView).ConfigureColumns(APreferences);
  Check(LResult <> nil, 'public programmatic operation returns the columns capability');
end;

function Width(AWidth: Integer): TNyxColumnPreferences;
begin
  Result := NyxColumnPreferences.WithColumn(
    NyxColumnPreference(NyxColumnIdentity(NyxTextField(CField))).Width(AWidth));
end;

{ Exercise the application's actual retained manager and its ordinary Nyx
  buttons. Widget activation is software-driven; no physical pointer or global
  cursor position is fabricated. The callback probe owns copied payloads only. }
procedure ManagerCases(const AView, APeer: INyxCollectionView); {$ifdef PAS2JS}async;{$endif}
var
  LMount: INyxCollectionColumnArrangementMount;
  LManager: INyxColumnArrangementManager;
  LPresenter: INyxColumnArrangementPresenter;
  LProbe: TProbe;
  LCallback: INyxEventCallback;
  LToken: INyxEventSubscription;
  LCount: Integer;
  LSerial: Integer;
  LAction: TNyxText;
  {$ifdef PAS2JS}
  LTarget: INyxBrowserColumnArrangementPresenter;
  {$else}
  LTarget: INyxLCLColumnArrangementPresenter;
  {$endif}

  procedure Activate(const AAction: TNyxText);
  begin
    { A successful command may replace the popup generation. Its earlier
      presenter is retired; obtain the current public presentation each time. }
    LPresenter := LManager.Presenter;
    {$ifdef PAS2JS}
    Check(Supports(LPresenter, INyxBrowserColumnArrangementPresenter, LTarget),
      'the current browser manager generation supplies its own action target');
    {$else}
    Check(Supports(LPresenter, INyxLCLColumnArrangementPresenter, LTarget),
      'the current native manager generation supplies its own action target');
    {$endif}
    LTarget.Presentation.Renderer.Reveal(AAction, niRuntime);
    {$ifdef PAS2JS}
    Check(LTarget.Presentation.Renderer.ElementFor(AAction) <> nil,
      'the actual manager action exists in its retained browser presentation');
    TJSHTMLElement(LTarget.Presentation.Renderer.ElementFor(AAction)).click;
    {$else}
    Check(LTarget.Presentation.Renderer.ControlFor(AAction) <> nil,
      'the actual manager action exists in its retained native presentation');
    TButton(LTarget.Presentation.Renderer.ControlFor(AAction)).Click;
    {$endif}
  end;

begin
  Check(Supports(GApp.View.CollectionMount('service-table'),
    INyxCollectionColumnArrangementMount, LMount),
    'the generated application mounts the ordinary column manager');
  LManager := LMount.ColumnArrangement;
  LProbe := TProbe.Create;
  LCallback := LProbe;
  LToken := GApp.View.Events.On(NyxControlEvents('service-table'), ntColumnsChanged)
    .Subscribe(LCallback);
  try
    LManager.Open;
    {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    LPresenter := LManager.Presenter;
    {$ifdef PAS2JS}
    Check(Supports(LPresenter, INyxBrowserColumnArrangementPresenter, LTarget),
      'the browser exposes its actual manager presentation');
    {$else}
    Check(Supports(LPresenter, INyxLCLColumnArrangementPresenter, LTarget),
      'the native application exposes its actual manager presentation');
    {$endif}
    LCount := GApp.State.GetValue(NyxIntegerState('column-change-count'));
    LSerial := NyxCollectionColumns(AView).CaptureColumns.Serial;
    LAction := NyxColumnArrangementActionID(NyxColumnArrangementEditorID,
      LManager.Observation, NyxColumnIdentity(NyxIntegerField('pid')), ncaaHide);
    Activate(LAction);
    {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    Check((LProbe.Calls = 1) and (LProbe.Last.CollectionColumns.Origin = nccoManager) and
      (LProbe.Last.CollectionColumns.BeforeSerial = LSerial) and
      (LProbe.Last.CollectionColumns.Serial = LSerial + 1) and
      (LProbe.Last.CollectionColumns.Count = 1) and
      (LProbe.Last.CollectionColumns.ChangeAt(0).Field.Name = 'pid') and
      (LProbe.Last.CollectionColumns.ChangeAt(0).Properties = [nccVisibility]),
      'actual Hide emits one exact visibility delta through the application router');
    Check((GApp.State.GetValue(NyxIntegerState('column-change-count')) = LCount + 1) and
      (GApp.State.GetValue(NyxTextState('column-status')) = 'pid'),
      'the saved callback reports the manager operation through ordinary bound state');
    LSerial := NyxCollectionColumns(AView).CaptureColumns.Serial;
    LAction := NyxColumnArrangementActionID(NyxColumnArrangementEditorID,
      LManager.Observation, NyxColumnIdentity(NyxTextField(CField)), ncaaFitWidth);
    Activate(LAction);
    {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    Check((LProbe.Calls = 2) and (LProbe.Last.CollectionColumns.Origin = nccoAutoSize) and
      (LProbe.Last.CollectionColumns.Serial = LSerial + 1) and
      (LProbe.Last.CollectionColumns.Count = 1) and
      (LProbe.Last.CollectionColumns.ChangeAt(0).Field.Name = CField) and
      (LProbe.Last.CollectionColumns.ChangeAt(0).Properties = [nccWidth]),
      'actual Fit measures the rendered field and reports the authoritative auto-size origin');
    Check((GApp.State.GetValue(NyxIntegerState('column-change-count')) = LCount + 2) and
      (GApp.State.GetValue(NyxTextState('column-status')) = CField) and
      (GApp.State.GetValue(NyxTextState('column-preferences')) =
        NyxCollectionColumns(AView).CaptureColumns.Preferences.ToData.ToJSON),
      'both authored callbacks consume the manager Fit result with exact field identity');
    LSerial := NyxCollectionColumns(AView).CaptureColumns.Serial;
    LAction := NyxColumnArrangementActionID(NyxColumnArrangementEditorID, ncaaResetAll);
    Activate(LAction);
    {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    Check((LProbe.Calls = 3) and (LProbe.Last.CollectionColumns.Origin = nccoReset) and
      (LProbe.Last.CollectionColumns.Serial = LSerial + 1) and
      (LProbe.Last.CollectionColumns.Count = 2) and
      (NyxCollectionColumns(AView).CaptureColumns.Preferences.Count = 0),
      'actual Reset groups both fields into one committed callback with the reset origin');
    Activate(LAction);
    {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
    Check((LProbe.Calls = 3) and
      (GApp.State.GetValue(NyxIntegerState('column-change-count')) = LCount + 3),
      'a repeated unchanged manager Reset emits no successful-change callback');
    Check(NyxCollectionColumns(APeer).CaptureColumns.Preferences.Count = 0,
      'the independent peer does not inherit any manager or Fit preference');
  finally
    LToken.Cancel;

    if LPresenter <> nil then
    begin
      LPresenter.Close;
    end;
  end;
end;

procedure Run; {$ifdef PAS2JS}async;{$endif}
var
  LView: INyxCollectionView;
  LPeer: INyxCollectionView;
  LCapture: INyxCollectionColumnsCapture;
  LFirst: TProbe;
  LSecond: TProbe;
  LWorker: TDetachedProbe;
  LFirstCallback: INyxEventCallback;
  LSecondCallback: INyxEventCallback;
  LWorkerCallback: INyxEventCallback;
  LFirstToken: INyxEventSubscription;
  LSecondToken: INyxEventSubscription;
  LWorkerToken: INyxEventSubscription;
  LSaved: TNyxText;
  LCount: Integer;
  LFailed: Boolean;
  {$ifndef PAS2JS}
  LStarted: QWord;
  {$else}
  LGrip: TJSHTMLElement;
  {$endif}
begin
  try
    GDocument := BuildNyxDocument;
    LSaved := TNyxCodec.Encode(GDocument);
    try
    {$ifdef PAS2JS}
    GApp := TNyxBrowserApplication.Create;
    GHost := TJSHTMLElement(document.createElement('main'));
    document.body.appendChild(GHost);
    GApp.Run(GDocument, GHost);
    {$else}
    GApp := TNyxLCLApplication.Create;
    GApp.Mount(GDocument);
    GApp.Window.ClientWidth := StrToIntDef(ParamStr(1), 1280);
    GApp.Window.ClientHeight := 820;
    GApp.Window.Show;
    Check(GApp.Window.ClientWidth = StrToIntDef(ParamStr(1), 1280),
      'native checks use the requested actual client allocation');
    {$endif}
      {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
      LView := GApp.View.CollectionView('service-table');
      LPeer := GApp.View.CollectionView('peer-table');
      Check(GApp.State.GetValue(NyxIntegerState('column-change-count')) = 0,
        'hidden mount/initial defaults dispatch no application column callback');
      LCapture := NyxCollectionColumns(LView).CaptureColumns;
      Configure(LView, Width(200));
      Check(GApp.State.GetValue(NyxIntegerState('column-change-count')) = 0,
        'generated neUIQueue callback waits for the existing UI scheduler');
      {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
      Check((GApp.State.GetValue(NyxIntegerState('column-change-count')) = 1) and
        (GApp.State.GetValue(NyxTextState('column-status')) = CField),
        'first generated application callback observes exact supplementary Unicode field');
      Check(GApp.State.GetValue(NyxTextState('column-preferences')) =
        NyxCollectionColumns(LView).CaptureColumns.Preferences.ToData.ToJSON,
        'second generated callback persists the exact detached arrangement');
      {$ifdef PAS2JS}
      Check(GApp.View.ElementFor('column-status-label').textContent = CField,
        'callback-driven state appears in the actual browser label');
      {$else}
      Check(TNyxText(TLabel(GApp.View.ControlFor('column-status-label')).Caption) = CField,
        'callback-driven state appears in the actual native label');
      {$endif}
      Check(NyxCollectionColumns(LPeer).CaptureColumns.Preferences.Count = 0,
        'a table sharing the same store retains an independent arrangement');
      Configure(LView, Width(200));
      {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
      Check(GApp.State.GetValue(NyxIntegerState('column-change-count')) = 1,
        'no-op emits no generated callback');
      LFailed := False;
      try
        PublishNyxGroup([NyxCollectionColumns(LView).PrepareColumns(LCapture, Width(201))]);
      except
        on ENyxCollection do
        begin
          LFailed := True;
        end;
      end;
      Check(LFailed, 'stale capture refuses through the public publication path');
      {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
      Check(GApp.State.GetValue(NyxIntegerState('column-change-count')) = 1,
        'refused operation has no success callback');
      {$ifdef PAS2JS}await(ManagerCases(LView, LPeer));{$else}ManagerCases(LView, LPeer);{$endif}
      {$ifdef PAS2JS}await(Capture);{$endif}

      LFirst := TProbe.Create;
      LFirstCallback := LFirst;
      LSecond := TProbe.Create;
      LSecondCallback := LSecond;
      LFirstToken := GApp.View.Events.On(NyxControlEvents('service-table'), ntColumnsChanged)
        .Policy(neSequential).Subscribe(LFirstCallback);
      LSecondToken := GApp.View.Events.On(NyxControlEvents('service-table'), ntColumnsChanged)
        .Subscribe(LSecondCallback);
      Configure(LView, Width(220));
      Check((LFirst.Calls = 1) and (LSecond.Calls = 1) and
        LFirst.Last.CollectionColumns.View.Same(LSecond.Last.CollectionColumns.View) and
        (LFirst.Last.CollectionColumns.Serial = LSecond.Last.CollectionColumns.Serial),
        'multiple router registrations receive the same immutable admitted payload');
      LFirst.Fail := True;
      Configure(LView, Width(230));
      Check((LFirstToken.LastExecution.Status = nesFailed) and (LSecond.Calls = 2) and
        (NyxCollectionColumns(LView).CaptureColumns.Preferences.ColumnAt(0).WidthValue = 230),
        'throwing router callback preserves the installed width and independent receivers');
      LFirst.Fail := False;
      LFirst.Sibling := LSecondToken;
      Configure(LView, Width(240));
      Check((LFirst.Calls = 3) and (LSecond.Calls = 2) and not LSecondToken.Active,
        'an earlier callback can cancel a later registration');

      {$ifdef PAS2JS}
      { The resizer attribute is a marker, not a column ordinal. This fixture's
        first declared field remains first; dispatch through its real handle. }
      LGrip := TJSHTMLElement(GApp.View.ElementFor('service-table').querySelector(
        '[data-nyx-column-resize]'));
      Check(LGrip <> nil, 'real bound table exposes its native browser resize affordance');
      LGrip.focus;
      LGrip.dispatchEvent(NyxTestKeyboard(ntKeyDown, 'ArrowRight'));
      LGrip.dispatchEvent(NyxTestKeyboard(ntKeyDown, 'Enter'));
      await(Drain);
      Check((LFirst.Calls = 4) and (LFirst.Last.CollectionColumns.Origin = nccoResize),
        'actual browser resize keyboard input reports the authoritative resize path');
      {$endif}

      LWorker := TDetachedProbe.Create;
      LWorkerCallback := LWorker;
      LWorkerToken := GApp.View.Events.On(NyxControlEvents('peer-table'), ntColumnsChanged)
        .Policy(neAsynchronous).Subscribe(LWorkerCallback);
      Configure(LPeer, Width(210));
      {$ifdef PAS2JS}
      await(Drain);
      {$else}
      LStarted := GetTickCount64;
      repeat
        Drain;
        Sleep(1);
      until (LWorkerToken.LastExecution.Status in [nesSucceeded, nesFailed, nesCancelled]) or
        (GetTickCount64 - LStarted > 5000);
      {$endif}
      Check((LWorkerToken.LastExecution.Status = nesSucceeded) and
        (LWorker.FieldName = CField) and (LWorker.Payload.Origin = nccoProgrammatic),
        'existing asynchronous scheduler delivers a detached payload without UI access');

      LCount := GApp.State.GetValue(NyxIntegerState('column-change-count'));
      GApp.View.Events.On(NyxControlEvents('service-table'), ntColumnsChanged).Policy(neUIQueue);
      Configure(LView, Width(250));
      GApp.ShowPage('monitor');
      {$ifdef PAS2JS}await(Drain);{$else}Drain;{$endif}
      Check(GApp.State.GetValue(NyxIntegerState('column-change-count')) = LCount,
        'same-ID remount cancels the old generation queued event before UI writes');
      Check(LFirst.Last.CollectionColumns.ChangeAt(0).Field.Name = CField,
        'retained callback data survives original generation retirement');
      Check(TNyxCodec.Encode(GDocument) = LSaved,
        'runtime arrangement, events and status never change saved defaults or callback metadata');
      LFirstToken.Cancel;
      LSecondToken.Cancel;
      LWorkerToken.Cancel;
      LFirst.Retire := True;
      LFirstToken := GApp.View.Events.On(NyxControlEvents('service-table'), ntColumnsChanged)
        .Policy(neSequential).Subscribe(LFirstCallback);
      LCount := LFirst.Calls;
      Configure(GApp.View.CollectionView('service-table'), Width(270));
      Check((GApp.View.Root = nil) and (LFirst.Calls = LCount + 1) and
        LFirst.Last.CollectionColumns.Defined,
        'a callback retires the entered real generation while its copied data remains alive');
      LFirstToken.Cancel;
    finally
      LView := nil;
      LPeer := nil;
      LCapture := nil;
      GApp.Free;
      GApp := nil;
      GDocument.Free;
      GDocument := nil;
    end;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-column-controls', 'passed');
    document.body.setAttribute('data-column-controls-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' generated application column-control checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-column-controls', 'failed');
      document.body.setAttribute('data-column-controls-error', LException.Message);
      {$else}
      WriteLn('FAIL ', TNyxText(LException.Message));
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end;

begin
  {$ifndef PAS2JS}
  Application.Initialize;
  Application.ShowMainForm := False;
  {$endif}
  Run;
end.
