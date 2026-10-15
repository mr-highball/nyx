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



program nyx_programs_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.model, nyx.codec, nyx.controls, nyx.state,
  nyx.types, nyx.events, nyx.behavior, nyx.callbacks, nyx.application.context, nyx.scheduler,
  nyx.linq, nyx.linq.execution, nyx.programs, nyx.programs.types,
  nyx.programs.contract, nyx.programs.execution, nyx.test.programs
  {$ifdef PAS2JS}, JS, Web, nyx.application.browser
  {$else}, Interfaces, Classes, Forms, Controls, StdCtrls, Graphics, SyncObjs,
  nyx.widgets.lcl, nyx.application.lcl{$endif};

type
  { Application-owned callback compiles explicit editable behavior data on demand.
    Only the public program courier can publish its typed output bindings. }
  TProgramRunHandler = class(TNyxApplicationEventCallback)
  public
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

  { Revocable UI-only terminal feedback, independent of the program's atomic
    output group. Failed outcomes update only the diagnostic caption afterward. }
  TProgramFeedback = class(TInterfacedObject, INyxQueryResultPort<TNyxQueryOutcome>)
  public
    Context: INyxApplicationContext;
    Generation: INyxCancellationScope;
    function Publish(const AValues: TNyxSequenceArray<TNyxQueryOutcome>): Boolean;
  end;

  { Alternative immutable scalar implementation returned by a pure host query.
    Its deliberately reentrant UI conversion navigates once, proving publication
    must revalidate the captured owner after all external value getters return.
    This fixture owns only its scalar payload, never an application reference. }
  TReentrantValue = class(TInterfacedObject, INyxProgramValue, INyxProgramQueries)
  private
    FValue: INyxProgramValue;
  public
    constructor Create;
    function GetValueType: TNyxProgramType;
    function Defined: Boolean;
    function AsBoolean: Boolean;
    function AsInteger: Integer;
    function AsNumber: Double;
    function AsText: TNyxText;
    function AsJSON: TNyxDataValue;
    function AsBytes: TNyxBytes;
    function AsHandle: INyxProgramHandle;
    function AsState: TNyxStateValue;
    function Copy: INyxProgramValue;
    function ByteSize: TNyxTextBytes;
    function Query(const ARoutine: TNyxProgramSymbolRef;
      const AArguments: INyxProgramArguments): INyxProgramValue;
  end;

  {$ifndef PAS2JS}
  { Native-only synchronized probe proves actual worker entry and observation
    of caller cancellation. Event reads are nonblocking in the ordinary UI loop.
    It owns no application, document or borrowed interpreter state. }
  TWorkerScope = class(TInterfacedObject, INyxCancellationScope, INyxProgramClock)
  public
    Local: INyxCancellationScope;
    Entered: TEvent;
    Observed: TEvent;
    ClockReads: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure Cancel;
    function GetCancelled: Boolean;
    function Milliseconds: Double;
  end;
  {$endif}

var
  GDocument: TNyxDocument;
  GBefore: TNyxText;
  GRun: INyxQueryRun;
  GFeedback: INyxQueryRun;
  GStage: Integer;
  GChecks: Integer;
  GRevision: Integer;
  GCaption: TNyxText;
  GCount: Integer;
  GWidth: Integer;
  GFinished: Boolean;
  GRetiredDuringConversion: Boolean;
  {$ifdef PAS2JS}
  GApp: TNyxBrowserApplication;
  GOther: TNyxBrowserApplication;
  GHost: TJSHTMLElement;
  GOtherHost: TJSHTMLElement;
  GSourceControl: TJSHTMLElement;
  GStarted: Double;
  {$else}
  GApp: TNyxLCLApplication;
  GOther: TNyxLCLApplication;
  GSourceControl: TControl;
  GStarted: QWord;
  GWorker: TWorkerScope;
  GWorkerLease: INyxCancellationScope;
  {$endif}

procedure Check(AValue: Boolean; const AMessage: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

constructor TReentrantValue.Create;
begin
  inherited Create;
  FValue := NyxProgramValue('A retired conversion must not publish');
end;

function TReentrantValue.GetValueType: TNyxProgramType;
begin
  Result := FValue.ValueType;
end;

function TReentrantValue.Defined: Boolean;
begin
  Result := FValue.Defined;
end;

function TReentrantValue.AsBoolean: Boolean;
begin
  Result := FValue.AsBoolean;
end;

function TReentrantValue.AsInteger: Integer;
begin
  Result := FValue.AsInteger;
end;

function TReentrantValue.AsNumber: Double;
begin
  Result := FValue.AsNumber;
end;

function TReentrantValue.AsText: TNyxText;
begin
  Result := FValue.AsText;
end;

function TReentrantValue.AsJSON: TNyxDataValue;
begin
  Result := FValue.AsJSON;
end;

function TReentrantValue.AsBytes: TNyxBytes;
begin
  Result := FValue.AsBytes;
end;

function TReentrantValue.AsHandle: INyxProgramHandle;
begin
  Result := FValue.AsHandle;
end;

function TReentrantValue.AsState: TNyxStateValue;
begin
  GRetiredDuringConversion := True;
  GApp.ShowPage('other');
  Result := FValue.AsState;
end;

function TReentrantValue.Copy: INyxProgramValue;
begin
  Result := Self;
end;

function TReentrantValue.ByteSize: TNyxTextBytes;
begin
  Result := FValue.ByteSize;
end;

function TReentrantValue.Query(const ARoutine: TNyxProgramSymbolRef;
  const AArguments: INyxProgramArguments): INyxProgramValue;
begin
  Result := Self;
end;

function TProgramFeedback.Publish(const AValues: TNyxSequenceArray<TNyxQueryOutcome>): Boolean;
begin
  Result := Context.Active and not Generation.Cancelled;

  if Result and (Length(AValues) = 1) and (AValues[0].Phase = nqpFailed) then
  begin
    Context.State.SetValue(NyxTextState('error'), AValues[0].Failure);
  end;
end;

procedure TProgramRunHandler.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
var
  LProgram: INyxProgram;
  LFeedback: TProgramFeedback;
  LPort: INyxQueryResultPort<TNyxQueryOutcome>;
begin
  Context.State.SetValue(NyxTextState('error'), '');
  GRun := nil;
  try
    LProgram := CompileNyxProgram(Context.State.GetValue(NyxTextState('source')), GreetingContract);
    GRun := TNyxProgramExecution.Submit(Context, LProgram, GreetingBindings);
    LFeedback := TProgramFeedback.Create;
    LPort := LFeedback;
    LFeedback.Context := Context;
    LFeedback.Generation := Context.Generation;
    GFeedback := TNyxQueryObservation.Observe(GRun, Context.Scheduler, LPort,
      TNyxQueryObservationOptions.Defaults, LFeedback.Generation);
  except
    on E: ENyxProgram do
    begin
      Context.State.SetValue(NyxTextState('error'), E.Diagnostic.Message);
    end;
  end;
end;

{$ifndef PAS2JS}
constructor TWorkerScope.Create;
begin
  inherited Create;
  Local := NewNyxCancellationScope;
  Entered := TEvent.Create(nil, True, False, '');
  Observed := TEvent.Create(nil, True, False, '');
end;

destructor TWorkerScope.Destroy;
begin
  Entered.Free;
  Observed.Free;
  inherited Destroy;
end;

procedure TWorkerScope.Cancel;
begin
  Local.Cancel;
end;

function TWorkerScope.GetCancelled: Boolean;
begin
  Result := Local.Cancelled;

  if GetCurrentThreadID <> MainThreadID then
  begin
    if Result and (Entered.WaitFor(0) = wrSignaled) then
    begin
      Observed.SetEvent;
    end;
  end;
end;

function TWorkerScope.Milliseconds: Double;
begin
  Inc(ClockReads);

  if (GetCurrentThreadID <> MainThreadID) and (ClockReads >= 3) then
  begin
    { Only the interpreter reads this clock. The third observation occurs at
      its 64-step checkpoint, after actual VM entry, not generic queue polling. }
    Entered.SetEvent;
  end;
  Result := Double(GetTickCount64);
end;
{$endif}

procedure EditName(const AText: TNyxText);
begin
  {$ifdef PAS2JS}
  TJSHTMLInputElement(GApp.View.InputFor('name-input')).value := AText;
  GApp.View.InputFor('name-input').dispatchEvent(TJSEvent.new('input'));
  {$else}
  TEdit(GApp.View.InputFor('name-input')).Text := AText;
  {$endif}
end;

procedure EditSource(const AText: TNyxText);
begin
  {$ifdef PAS2JS}
  TJSHTMLTextAreaElement(GApp.View.InputFor('source-memo')).value := AText;
  GApp.View.InputFor('source-memo').dispatchEvent(TJSEvent.new('input'));
  {$else}
  TMemo(GApp.View.InputFor('source-memo')).Text := AText;
  {$endif}
end;

procedure Click;
begin
  {$ifdef PAS2JS}
  GApp.View.ElementFor('run-button').click;
  {$else}
  TNyxLCLButton(GApp.View.ControlFor('run-button')).Click;
  {$endif}
end;

function Idle: Boolean;
begin
  Result := (GRun <> nil) and not (GRun.Phase in [nqpPending, nqpComputing, nqpAwaitingUI]);
end;

procedure SaveFrame;
begin
  GCaption := GApp.State.GetValue(NyxTextState('caption'));
  GCount := GApp.State.GetValue(NyxIntegerState('count'));
  GWidth := GApp.State.GetValue(NyxIntegerState('preview-width'));
end;

procedure RetainedFrame;
begin
  Check((GApp.State.GetValue(NyxTextState('caption')) = GCaption) and
    (GApp.State.GetValue(NyxIntegerState('count')) = GCount) and
    (GApp.State.GetValue(NyxIntegerState('preview-width')) = GWidth),
    'The entire accepted caption/count/layout frame is retained');
end;

procedure VisibleFrame(const ACaption: TNyxText; ACount, AWidth: Integer);
begin
  {$ifdef PAS2JS}
  Check((GApp.View.ElementFor('caption-label').textContent = ACaption) and
    (GApp.View.ElementFor('count-label').textContent = IntToStr(ACount)) and
    (Abs(GApp.View.ElementFor('preview-card').getBoundingClientRect.width - AWidth) < 1),
    'Actual browser controls display the complete typed text/count/layout frame');
  {$else}
  Check((TNyxText(TLabel(GApp.View.ControlFor('caption-label')).Caption) = ACaption) and
    (TLabel(GApp.View.ControlFor('count-label')).Caption = IntToStr(ACount)) and
    (GApp.View.ControlFor('preview-card').Width = AWidth),
    'Actual LCL controls display the complete typed text/count/layout frame');
  {$endif}
end;

procedure Step;
var
  LProgram: INyxProgram;
  LFailed: Boolean;
  LQueries: INyxProgramQueries;
  {$ifndef PAS2JS}
  LBitmap: TBitmap;
  LPNG: TPortableNetworkGraphic;
  {$endif}
begin
  case GStage of
    0:
    begin
      GSourceControl := GApp.View.InputFor('source-memo');
      Click;
      Check(GRun <> nil, 'An actual button event invokes compiled interpreted behavior');
      GRevision := GApp.State.Revision;
    end;
    1:
    begin

      if not Idle then
      begin
        Exit;
      end;
      Check(GRun.Phase = nqpSucceeded, 'Ordinary program result reaches its guarded UI courier');
      Check(GApp.State.Revision = GRevision + 1, 'Three output values publish in one ordinary state Apply');
      VisibleFrame('Hello, Ada!', 3, 184);
      Check(GOther.State.GetValue(NyxTextState('caption')) = 'Run your behavior',
        'Independent applications share declarations, never runtime values');
      Check(GApp.View.InputFor('source-memo') = GSourceControl, 'Publication retains the same source editor face');
      EditName('Grace Hopper');
      Click;
    end;
    2:
    begin

      if not Idle then
      begin
        Exit;
      end;
      VisibleFrame('Hello, Grace Hopper!', 12, 256);
      SaveFrame;
      LProgram := CompileNyxProgram(ProgramGreetingSource, GreetingContract);
      GRun := TNyxProgramExecution.Submit(GApp.Context, LProgram, GreetingBindings);
      EditName('A later edit');
    end;
    3:
    begin

      if not Idle then
      begin
        Exit;
      end;
      Check(GRun.Phase = nqpFailed, 'A state revision changed after capture refuses the complete delivery');
      RetainedFrame;
      EditSource('begin Caption := Source; Count := 1; PreviewWidth := -1 end.');
      Click;
    end;
    4:
    begin

      if not Idle then
      begin
        Exit;
      end;
      Check(GRun.Phase = nqpFailed, 'Bound-control admission rejects an invalid generated layout');
      RetainedFrame;
      EditSource('begin Caption := ''partial''; Count := 1 div 0; PreviewWidth := 250 end.');
      Click;
    end;
    5:
    begin

      if not Idle then
      begin
        Exit;
      end;
      Check(GRun.Phase = nqpFailed, 'A program failure cannot publish an earlier output assignment');
      RetainedFrame;
      EditSource('begin Caption := MissingSymbol end.');
      Click;
      Check((GRun = nil) and (GApp.State.GetValue(NyxTextState('error')) <> ''),
        'Compile failure shows useful feedback without queuing executable code');
      RetainedFrame;
      EditSource(ProgramGreetingSource);
      EditName('Barbara Liskov');
      Click;
    end;
    6:
    begin

      if not Idle then
      begin
        Exit;
      end;
      VisibleFrame('Hello, Barbara Liskov!', 14, 272);
      SaveFrame;
      {$ifdef PAS2JS}
      GApp.View.ElementFor('source-memo').scrollIntoView(True);
      document.body.setAttribute('data-capture-checkpoint', 'program-workbench');
      {$else}

      if ParamCount = 1 then
      begin
        LBitmap := GApp.Window.GetFormImage;
        LPNG := TPortableNetworkGraphic.Create;
        try
          LPNG.Assign(LBitmap);
          LPNG.SaveToFile(ParamStr(1));
        finally
          LPNG.Free;
          LBitmap.Free;
        end;
      end;
      {$endif}
    end;
    7:
    begin
      {$ifdef PAS2JS}

      if document.body.getAttribute('data-capture-observed') <> 'program-workbench' then
      begin
        Exit;
      end;
      {$endif}
      LProgram := CompileNyxProgram(ProgramGreetingSource, GreetingContract);
      GRun := TNyxProgramExecution.Submit(GApp.Context, LProgram, GreetingBindings);
      GRun.Cancel;
    end;
    8:
    begin

      if not Idle then
      begin
        Exit;
      end;
      Check(GRun.Phase = nqpCancelled, 'Queued cancellation prevents publication');
      RetainedFrame;
      LProgram := CompileNyxProgram(ProgramGreetingSource, GreetingContract);
      GRun := TNyxProgramExecution.Submit(GApp.Context, LProgram, GreetingBindings);
      GApp.ShowPage('other');
    end;
    9:
    begin

      if not Idle then
      begin
        Exit;
      end;
      Check(GRun.Phase in [nqpCancelled, nqpDiscarded], 'Page generation retirement revokes old behavior delivery');
      RetainedFrame;
      GApp.ShowPage('home');
      {$ifndef PAS2JS}
      GWorker := TWorkerScope.Create;
      GWorkerLease := GWorker;
      LProgram := CompileNyxProgram('begin while True do begin end end.', GreetingContract,
        NyxProgramLimits.Steps(50000000).Milliseconds(60000));
      GRun := TNyxProgramExecution.Submit(GApp.Context, LProgram, GreetingBindings,
        NyxProgramRun.Cancellation(GWorkerLease).Clock(GWorker), neThreaded);
      {$else}
      LProgram := CompileNyxProgram(ProgramGreetingSource, GreetingContract);
      LFailed := False;
      try
        TNyxProgramExecution.Submit(GApp.Context, LProgram, GreetingBindings, neThreaded);
      except
        on E: ENyxSchedule do
        begin
          LFailed := True;
        end;
      end;
      Check(LFailed, 'Browser native-thread policy refuses explicitly');
      {$endif}
    end;
    10:
    begin
      {$ifndef PAS2JS}

      if GWorker.Entered.WaitFor(0) <> wrSignaled then
      begin
        Exit;
      end;
      Check(True, 'The native program has entered an actual worker thread');
      GWorker.Cancel;
      {$endif}
    end;
    11:
    begin
      {$ifndef PAS2JS}

      if (GWorker.Observed.WaitFor(0) <> wrSignaled) or not Idle then
      begin
        Exit;
      end;
      Check(GRun.Phase = nqpCancelled, 'Entered native execution observes cooperative caller cancellation');
      RetainedFrame;
      {$endif}
      LProgram := CompileNyxProgram('begin Caption := Source; Count := 0; PreviewWidth := 240; ' +
        'SetTitle(''not applied'') end.', GreetingContract.WithRoutine(
          NyxProgramPropose(NyxProgramSymbol('SetTitle'), NyxProgramCapability(npcStateWrite))
            .Argument(NyxProgramSymbol('Title'), NyxProgramType(npkText))));
      LFailed := False;
      try
        TNyxProgramExecution.Submit(GApp.Context, LProgram, GreetingBindings);
      except
        on E: ENyxProgram do
        begin
          LFailed := E.Diagnostic.Kind = npdType;
        end;
      end;
      Check(LFailed, 'Scalar-state courier refuses host proposals instead of ignoring/executing them');
      RetainedFrame;
      LProgram := CompileNyxProgram('begin Caption := ReadGreeting(); Count := 0; PreviewWidth := 240 end.',
        GreetingContract.WithRoutine(NyxProgramQuery(NyxProgramSymbol('ReadGreeting'),
          NyxProgramType(npkText))));
      LQueries := TReentrantValue.Create;
      GRetiredDuringConversion := False;
      GRun := TNyxProgramExecution.Submit(GApp.Context, LProgram, GreetingBindings,
        NyxProgramRun.Queries(LQueries));
    end;
    12:
    begin

      if not Idle then
      begin
        Exit;
      end;
      Check(GRetiredDuringConversion and (GRun.Phase in [nqpCancelled, nqpDiscarded]),
        'Reentrant scalar conversion retires the page before final result admission / phase ' +
        IntToStr(Ord(GRun.Phase)) + ' / ' + GRun.Failure);
      RetainedFrame;
      GApp.ShowPage('home');
      Check(TNyxCodec.Encode(GDocument) = GBefore, 'Runtime execution never rewrites the authored document/default behavior');
      GFinished := True;
    end;
  end;
  Inc(GStage);
end;

procedure Retire;
begin

  if GRun <> nil then
  begin
    GRun.Cancel;
  end;

  if GFeedback <> nil then
  begin
    GFeedback.Cancel;
  end;
  FreeAndNil(GApp);
  FreeAndNil(GOther);
  GRun := nil;
  GFeedback := nil;
  {$ifndef PAS2JS}
  GWorkerLease := nil;
  GWorker := nil;
  {$endif}
  FreeAndNil(GDocument);
end;

{$ifdef PAS2JS}
procedure Drive;
begin
  try
    Step;

    if GFinished then
    begin
      document.body.setAttribute('data-program-controls', 'passed');
      document.body.setAttribute('data-program-controls-checks', IntToStr(GChecks));
      Retire;
      Exit;
    end;

    if TJSDate.now - GStarted > 120000 then
    begin
      raise Exception.Create('Program controls deadline at stage ' + IntToStr(GStage));
    end;
    window.setTimeout(@Drive, 30);
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-program-controls', 'failed');
      document.body.setAttribute('data-program-controls-error', E.Message);
      Retire;
    end;
  end;
end;
{$endif}

begin
  GChecks := 0;
  GStage := 0;
  try
    RegisterNyxCallback(NyxHandler('TProgramRunHandler'), TProgramRunHandler);
    GDocument := ProgramDocument;
    GBefore := TNyxCodec.Encode(GDocument);
    {$ifdef PAS2JS}
    GHost := TJSHTMLElement(document.createElement('div'));
    GOtherHost := TJSHTMLElement(document.createElement('div'));
    document.body.appendChild(GHost);
    document.body.appendChild(GOtherHost);
    GOtherHost.style.setProperty('display', 'none');
    GApp := TNyxBrowserApplication.Create;
    GOther := TNyxBrowserApplication.Create;
    GApp.Run(GDocument, GHost);
    GOther.Run(GDocument, GOtherHost);
    GStarted := TJSDate.now;
    Drive;
    {$else}
    Application.Initialize;
    GApp := TNyxLCLApplication.Create;
    GOther := TNyxLCLApplication.Create;
    GApp.Mount(GDocument);
    GOther.Mount(GDocument);
    GApp.Window.SetBounds(10, 10, 760, 1000);
    GApp.Window.Show;
    GStarted := GetTickCount64;
    while not GFinished do
    begin
      CheckSynchronize;
      Application.ProcessMessages;
      Step;

      if GetTickCount64 - GStarted > 120000 then
      begin
        raise Exception.Create('Native program controls deadline at stage ' + IntToStr(GStage));
      end;
      Sleep(1);
    end;
    Retire;
    WriteLn('PASS ', GChecks, ' actual Pascal-program application controls');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-program-controls', 'failed');
      document.body.setAttribute('data-program-controls-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
      Retire;
    end;
  end;
end.
