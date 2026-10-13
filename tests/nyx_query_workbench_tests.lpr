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

program nyx_query_workbench_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, nyx.text, nyx.model, nyx.codec, nyx.generated.view, nyx.state,
  nyx.application.context, nyx.collections, nyx.collections.registry,
  nyx.scheduler, nyx.events, nyx.callbacks, nyx.behavior,
  nyx.linq, nyx.linq.execution, nyx.application.queryfeedback,
  nyx.application.navigation, nyx.test.application.navigation
  {$ifdef PAS2JS}, Web, nyx.application.browser
  {$else}, Interfaces, Forms, StdCtrls, Grids, nyx.application.lcl,
    nyx.test.capture.lcl{$endif};

type
  {$ifdef PAS2JS}
  TApplicationHost = TNyxBrowserApplication;
  {$else}
  TApplicationHost = TNyxLCLApplication;
  TButtonAccess = class(TButton);
  TCheckAccess = class(TCheckBox);
  {$endif}

  { Capture through the optional interface on a different callback base.
    A later missing handler causes binding/mount refusal after this attachment. }
  TFailedMountCallback = class(TNyxEventCallback, INyxApplicationEventCallback)
  public
    procedure AttachContext(const AContext: INyxApplicationContext);
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

  { Execute the exact semantically admitted callback through actual controls.
    Polling observes the scheduler instead of blocking a UI thread on workers. }
  TJourney = class
  private
    FDocument: TNyxDocument;
    FApp: TApplicationHost;
    FContext: INyxApplicationContext;
    FScheduler: INyxScheduler;
    FStore: INyxCollection;
    FGeneration: INyxCancellationScope;
    FOutcomeRun: INyxQueryRun;
    FFeedbackPort: INyxQueryResultPort<TNyxQueryOutcome>;
    FStage: Integer;
    FChecks: Integer;
    FPolls: Integer;
    FExpected: TNyxText;
    FAuthored: TNyxText;
    procedure Check(ACondition: Boolean; const AReason: TNyxText);
    procedure Calculate;
    procedure Navigate(AIndex: Integer);
    function NavigationText(AIndex: Integer): TNyxText;
    procedure SetImportant;
    procedure StartStage;
    procedure Finish(const AFailure: TNyxText);
    procedure VerifyRetired;
    function Summary: TNyxText;
  public
    Done: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Poll;
  end;

var
  GJourney: TJourney;
  GFailedContext: INyxApplicationContext;

procedure TFailedMountCallback.AttachContext(const AContext: INyxApplicationContext);
begin
  GFailedContext := AContext;
end;

procedure TFailedMountCallback.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  raise Exception.Create('A refused mount must never dispatch this callback');
end;

procedure TJourney.Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Stage ' + IntToStr(FStage) + ': ' + AReason);
  end;
  Inc(FChecks);
end;

constructor TJourney.Create;
var
  LOther: TApplicationHost;
  LContext: INyxApplicationContext;
  LFailedDocument: TNyxDocument;
  LRefused: Boolean;
  {$ifdef PAS2JS}
  LHost: TJSHTMLElement;
  {$endif}
begin
  inherited Create;
  FChecks := VerifyNyxPageNavigation;
  FDocument := BuildNyxDocument;
  FAuthored := TNyxCodec.Encode(FDocument);
  FApp := TApplicationHost.Create;
  {$ifdef PAS2JS}
  FApp.Run(FDocument, TJSHTMLElement(document.body));
  {$else}
  FApp.Mount(FDocument);
  FApp.Window.Show;
  Application.ProcessMessages;
  {$endif}
  FContext := FApp.Context;
  FScheduler := FContext.Scheduler;
  FStore := FContext.Collections.Collection(NyxCollection('tasks'));
  Check((NavigationText(0) = 'Tasks') and (NavigationText(1) = 'About'),
    'actual navigation controls use authored captions rather than routing IDs');
  Check(FContext.Active and (FContext.State = FApp.State), 'live context exposes the independent runtime state');
  Check(FStore.Snapshot.Count = 6, 'semantic companion supplies six English task rows');
  { Two hosts may borrow the same immutable design. Callback factories must
    create independent instances; state/default stores never belong to that
    authored tree, and closing one capability cannot retire the other. }
  LOther := TApplicationHost.Create;
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('div'));
  document.body.appendChild(LHost);
  {$endif}
  try
    {$ifdef PAS2JS}LOther.Run(FDocument, LHost);{$else}LOther.Mount(FDocument);{$endif}
    LContext := LOther.Context;
    Check((LContext <> FContext) and (LOther.State <> FApp.State),
      'two mounted hosts receive independent contexts and runtime stores');
    LOther.State.SetValue(NyxTextState('summary'), 'This belongs to the other window.');
    Check(Summary <> 'This belongs to the other window.', 'other application state never leaks into the first');
  finally
    LOther.Free;
    {$ifdef PAS2JS}LHost.remove;{$endif}
  end;
  Check(not LContext.Active and FContext.Active, 'one host disposal leaves the other context active');
  LContext := nil;
  RegisterNyxCallback(NyxHandler('TFailedMountCallback'), TFailedMountCallback);
  LFailedDocument := BuildNyxDocument;
  LOther := TApplicationHost.Create;
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('div'));
  document.body.appendChild(LHost);
  {$endif}
  try
    NyxCallbacks(LFailedDocument.Find('calculate-summary')).OnClick
      .Add(NyxHandler('TFailedMountCallback'), NyxCallbackID('capture-failed-mount'))
      .Add(NyxHandler('TDeliberatelyMissingMountCallback'), NyxCallbackID('missing-mount-handler'));
    LRefused := False;
    try
      {$ifdef PAS2JS}LOther.Run(LFailedDocument, LHost);{$else}LOther.Mount(LFailedDocument);{$endif}
    except
      on LException: ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (GFailedContext <> nil), 'actual failed mount attaches the alternative callback base first');
    Check(not GFailedContext.Active, 'failed mount revokes a separately retained injected context');
  finally
    LOther.Free;
    LFailedDocument.Free;
    GFailedContext := nil;
    {$ifdef PAS2JS}LHost.remove;{$endif}
  end;
  {$ifdef PAS2JS}
  Check(not (nscWorkerThreads in FScheduler.Capabilities), 'browser explicitly uses event-loop execution');
  {$else}
  Check(nscWorkerThreads in FScheduler.Capabilities, 'native workbench uses worker-thread capability');
  Check(TStringGrid(FApp.View.ControlFor('workbench-tasks')).RowCount = 7,
    'actual native table displays the six admitted rows');
  {$endif}
  StartStage;
end;

destructor TJourney.Destroy;
begin
  FApp.Free;
  FDocument.Free;

  if FScheduler <> nil then
  begin
    FScheduler.Shutdown;
  end;
  FContext := nil;
  FGeneration := nil;
  FStore := nil;
  FScheduler := nil;
  inherited Destroy;
end;

function TJourney.Summary: TNyxText;
begin
  Result := FApp.State.GetValue(NyxTextState('summary'));
end;

procedure TJourney.Calculate;
{$ifdef PAS2JS}
var
  LButton: TJSHTMLElement;
{$endif}
begin
  {$ifdef PAS2JS}
  LButton := FApp.View.ElementFor('calculate-summary');

  if not (LButton is TJSHTMLButtonElement) then
  begin
    LButton := TJSHTMLElement(LButton.querySelector('button'));
  end;
  TJSHTMLButtonElement(LButton).click;
  {$else}
  TButtonAccess(FApp.View.ControlFor('calculate-summary')).Click;
  {$endif}
  Check(Summary = 'Calculating selected tasks...', 'actual button enters the generated contextual callback');
end;

function TJourney.NavigationText(AIndex: Integer): TNyxText;
begin
  {$ifdef PAS2JS}
  Result := FApp.NavigationView.ElementFor(NyxPageNavigationButtonID(AIndex)).textContent;
  {$else}
  Result := TNyxText(TButton(FApp.NavigationView.ControlFor(NyxPageNavigationButtonID(AIndex))).Caption);
  {$endif}
end;

procedure TJourney.Navigate(AIndex: Integer);
var
  LID: TNyxText;
  {$ifdef PAS2JS}LButton: TJSHTMLElement;{$else}LButton: TButton;{$endif}
begin
  LID := NyxPageNavigationButtonID(AIndex);
  {$ifdef PAS2JS}
  LButton := FApp.NavigationView.ElementFor(LID);
  TJSHTMLButtonElement(LButton).click;
  Check(FApp.NavigationView.ElementFor(LID) = LButton,
    'navigation retains the notifying browser button across page replacement');
  {$else}
  LButton := TButton(FApp.NavigationView.ControlFor(LID));
  TButtonAccess(LButton).Click;
  Check(FApp.NavigationView.ControlFor(LID) = LButton,
    'navigation retains the notifying native button across page replacement');
  {$endif}

  if AIndex = 0 then
  begin
    {$ifdef PAS2JS}
    Check(FApp.View.ElementFor('calculate-summary') <> nil, 'captioned Tasks button routes to its stable page');
    {$else}
    Check(FApp.View.ControlFor('calculate-summary') <> nil, 'captioned Tasks button routes to its stable page');
    {$endif}
  end
  else
  begin
    {$ifdef PAS2JS}
    Check(FApp.View.ElementFor('about-title') <> nil, 'captioned About button routes to its stable page');
    {$else}
    Check(FApp.View.ControlFor('about-title') <> nil, 'captioned About button routes to its stable page');
    {$endif}
  end;
end;

procedure TJourney.SetImportant;
{$ifdef PAS2JS}
var
  LFace: TJSHTMLElement;
  LInput: TJSHTMLInputElement;
{$endif}
begin
  {$ifdef PAS2JS}
  LFace := FApp.View.ElementFor('important-filter');
  LInput := TJSHTMLInputElement(LFace.querySelector('input'));

  if LFace is TJSHTMLInputElement then
  begin
    LInput := TJSHTMLInputElement(LFace);
  end;
  LInput.checked := True;
  LInput.dispatchEvent(TJSEvent.new('change'));
  {$else}
  TCheckBox(FApp.View.ControlFor('important-filter')).Checked := True;
  TCheckAccess(FApp.View.ControlFor('important-filter')).Click;
  {$endif}
  Check(FApp.State.GetValue(NyxBooleanState('important-only')), 'actual checkbox updates typed Boolean state');
end;

procedure TJourney.StartStage;
var
  LBefore: Integer;
  LSource: INyxQueryProgress;
  LText: TNyxQueryFeedbackText;
  LValues: TNyxSequenceArray<TNyxQueryOutcome>;
begin
  FPolls := 0;
  FOutcomeRun := nil;
  FFeedbackPort := nil;
  case FStage of
    0:
    begin
      FExpected := 'Showing 6 tasks / 32 points';
      Calculate;
    end;
    1:
    begin
      SetImportant;
      FExpected := 'Showing 3 tasks / 24 points';
      Calculate;
    end;
    2:
    begin
      FStore.Update(FStore.Snapshot.ItemAt(0).WithValue(NyxIntegerField('points'), 10));
      FExpected := 'Showing 3 tasks / 31 points';
      Calculate;
    end;
    3:
    begin
      LBefore := FApp.State.GetValue(NyxIntegerState('query-request'));
      Calculate;
      FApp.State.SetValue(NyxBooleanState('important-only'), False);
      Calculate;
      Check(FApp.State.GetValue(NyxIntegerState('query-request')) = LBefore + 2,
        'two actual button requests own distinct publication versions');
      FExpected := 'Showing 6 tasks / 39 points';
    end;
    4:
    begin
      Calculate;
      FStore.Update(FStore.Snapshot.ItemAt(0).WithValue(NyxIntegerField('points'), 11));
      FExpected := 'Tasks changed. Calculate again.';
    end;
    5:
    begin
      FExpected := 'Showing 6 tasks / 40 points';
      Calculate;
    end;
    6:
    begin
      Calculate;
      FGeneration := FContext.Generation;
      Navigate(1);
      Check(FGeneration.Cancelled, 'navigation retires the query generation');
      Check(FContext.Active, 'application context survives a page transition');
      Check(not FContext.Generation.Cancelled, 'context resolves the newly mounted generation');
      FExpected := 'Calculating selected tasks...';
    end;
    7:
    begin
      Navigate(0);
      FExpected := 'Showing 6 tasks / 40 points';
      Calculate;
    end;
    8, 9, 10, 11, 12, 13:
    begin
      LBefore := FContext.State.GetValue(NyxIntegerState('query-request'));
      LText := TNyxQueryFeedbackText.Defaults;

      if FStage = 10 then
      begin
        LText := LText.Cancelled('Stopped safely.');
      end;
      FFeedbackPort := NyxQueryFeedback(FContext, NyxTextState('summary'),
        NyxIntegerState('query-request'), LBefore, LText);
      LSource := NewNyxQueryProgress;
      case FStage of
        8:
        begin
          LSource.Fail('Diagnostic 🌙');
          FExpected := 'Could not calculate: Diagnostic 🌙';
        end;
        9:
        begin
          LSource.Fail('');
          FExpected := 'Could not calculate: The calculation failed without a diagnostic.';
        end;
        10:
        begin
          LSource.Cancel;
          FExpected := 'Stopped safely.';
        end;
        11:
        begin
          LSource.Finish(True);
          FExpected := 'Stopped safely.';
        end;
        12, 13:
        begin
          LSource.Fail('This belongs to the obsolete request.');
          FExpected := 'Keep the newer request';
          FContext.State.SetValue(NyxTextState('summary'), FExpected);

          if FStage = 12 then
          begin
            FContext.State.SetValue(NyxIntegerState('query-request'), LBefore + 1);
          end;
        end;
      end;
      FOutcomeRun := TNyxQueryObservation.Observe(LSource, FScheduler, FFeedbackPort,
        TNyxQueryObservationOptions.Defaults.CheckEvery(1));

      if FStage = 13 then
      begin
        FApp.ShowPage('about-workbench');
        SetLength(LValues, 1);
        LValues[0].Phase := nqpCancelled;
        LValues[0].Failure := '';
        Check(not FFeedbackPort.Publish(LValues),
          'retained feedback port refuses the retired generation before state entry');
      end;
    end;
    14:
    begin
      FApp.ShowPage('task-workbench');
      Calculate;
      FGeneration := FContext.Generation;
      FreeAndNil(FApp);
      Check(FGeneration.Cancelled and not FContext.Active,
        'disposal revokes both entered work and retained application context');
      VerifyRetired;
      FScheduler.Shutdown;
    end;
  end;
end;

procedure TJourney.VerifyRetired;
var
  LRefused: Boolean;
  LState: TNyxState;
  LCollections: INyxCollections;
  LScope: INyxCancellationScope;
  LScheduler: INyxScheduler;
  LIndex: Integer;
begin
  for LIndex := 0 to 3 do
  begin
    LRefused := False;
    try
      case LIndex of
        0:
        begin
          LState := FContext.State;
        end;
        1:
        begin
          LCollections := FContext.Collections;
        end;
        2:
        begin
          LScope := FContext.Generation;
        end;
        3:
        begin
          LScheduler := FContext.Scheduler;
        end;
      end;
    except
      on LException: ENyxApplicationContext do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'retired access refuses before any application resolver entry');
  end;
  Check(FStore.Snapshot.Count = 6, 'independently retained collection survives application disposal');
end;

{$ifdef PAS2JS}
procedure ReleaseJourney;
begin
  FreeAndNil(GJourney);
  document.body.setAttribute('data-nyx-workbench-disposed', 'true');
end;
{$endif}

procedure TJourney.Finish(const AFailure: TNyxText);
begin
  Done := True;
  {$ifdef PAS2JS}

  if AFailure <> '' then
  begin
    document.body.textContent := 'FAIL ' + AFailure;
    document.body.setAttribute('data-nyx-workbench', 'failed');
  end
  else
  begin
    document.body.textContent := 'PASS ' + IntToStr(FChecks) + ' actual workbench/context checks';
    document.body.setAttribute('data-nyx-workbench', 'passed');
  end;
  window.setTimeout(@ReleaseJourney, 0);
  {$else}

  if AFailure <> '' then
  begin
    WriteLn('FAIL ', AFailure);
    ExitCode := 1;
  end
  else
  begin
    WriteLn('PASS ', FChecks, ' actual workbench/context checks');
  end;
  {$endif}
end;

procedure TJourney.Poll;
var
  LMonitor: INyxSchedulerMonitor;
begin
  try
    Inc(FPolls);
    if FPolls > 2000 then
    begin
      raise Exception.Create('Workbench completion observation expired: ' + FExpected);
    end;

    if not Supports(FScheduler, INyxSchedulerMonitor, LMonitor) then
    begin
      raise Exception.Create('Scheduler retirement observation is required');
    end;

    if (LMonitor.WorkerLoad.Running <> 0) or (LMonitor.WorkerLoad.Pending <> 0) then
    begin
      {$ifdef PAS2JS}window.setTimeout(@Poll, 5);{$endif}
      Exit;
    end;

    if (FOutcomeRun <> nil) and
      (FOutcomeRun.Phase in [nqpPending, nqpComputing, nqpAwaitingUI]) then
    begin
      {$ifdef PAS2JS}window.setTimeout(@Poll, 5);{$endif}
      Exit;
    end;

    if FStage < 14 then
    begin

      if Summary <> FExpected then
      begin
        {$ifdef PAS2JS}window.setTimeout(@Poll, 5);{$endif}
        Exit;
      end;
      {$ifdef PAS2JS}

      if not (FStage in [6, 13]) then
      begin
        Check(Pos(FExpected, FApp.View.ElementFor('workbench-summary').textContent) > 0,
          'actual bound browser label displays the complete typed result');
      end;
      {$else}

      if not (FStage in [6, 13]) then
      begin
        Check(TNyxText(TLabel(FApp.View.ControlFor('workbench-summary')).Caption) = FExpected,
          'actual bound native label displays the complete typed result');
      end;

      if (FStage = 0) and (ParamCount > 0) then
      begin
        SaveNyxNativeCapture(FApp.Window, TNyxText(ParamStr(1)), ncmPrint);
      end;
      {$endif}

      if FOutcomeRun <> nil then
      begin
        Check(((FOutcomeRun.Phase = nqpDiscarded) and (FStage in [12, 13])) or
          ((FOutcomeRun.Phase = nqpSucceeded) and not (FStage in [12, 13])),
          'request/view ownership decides whether terminal feedback is delivered');
      end;
      Check(TNyxCodec.Encode(FDocument) = FAuthored,
        'runtime terminal feedback preserves the exact authored document and state defaults');
      Inc(FStage);
      StartStage;
      {$ifdef PAS2JS}window.setTimeout(@Poll, 5);{$endif}
      Exit;
    end;

    if LMonitor.WorkerLoad.ActiveWorkers <> 0 then
    begin
      {$ifdef PAS2JS}window.setTimeout(@Poll, 5);{$endif}
      Exit;
    end;
    Finish('');
  except
    on LException: Exception do
    begin
      Finish(LException.Message);
    end;
  end;
end;

begin
  {$ifndef PAS2JS}Application.Initialize;{$endif}
  try
    GJourney := TJourney.Create;
    {$ifdef PAS2JS}
    GJourney.Poll;
    {$else}
    while not GJourney.Done do
    begin
      CheckSynchronize(1);
      Application.ProcessMessages;
      GJourney.Poll;
      Sleep(1);
    end;
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-nyx-workbench', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
  {$ifndef PAS2JS}GJourney.Free;{$endif}
end.
