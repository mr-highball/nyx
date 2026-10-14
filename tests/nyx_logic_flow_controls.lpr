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
program nyx_logic_flow_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.codec, nyx.state,
  nyx.flow.types, nyx.flow, nyx.logic, nyx.flow.execution, nyx.linq.execution,
  nyx.events, nyx.behavior, nyx.scheduler, nyx.application.context,
  nyx.test.logic.fixture, nyx.test.logic.document
  {$ifdef NYX_LOGIC_SAVED_COMPANION}, nyx.generated.view, nyx.flow.declarations{$endif}
  {$ifdef PAS2JS}, JS, Web, nyx.application.browser
  {$else}, Interfaces, Classes, Forms, Controls, StdCtrls, nyx.widgets.lcl,
  Graphics, nyx.application.lcl{$endif};

type
  { Callback owns a weak application capability and immutable graph, no controls
    or renderer. Multiple registrations share ordinary Nyx event ordering. }
  TRunGraph = class(TInterfacedObject, INyxEventCallback)
  public
    Context: INyxApplicationContext;
    Graph: INyxFlowGraph;
    LastRun: INyxQueryRun;
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
  end;
  TAfterRun = class(TInterfacedObject, INyxEventCallback)
  public
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
  end;
  { Runs on the actual selected scheduler policy; cancellation inside an
    operation must suppress every mapped result, including worker delivery. }
  TCancellingLogic = class(TInterfacedObject, INyxLogicOperation)
  public
    function GetDescriptor: TNyxLogicDescriptor;
    function GetConfiguration: TNyxDataValue;
    function Invoke(const AInputs: TNyxFlowValues;
      const ACancellation: INyxCancellationScope): TNyxFlowValues;
  end;
  TRetiringAssignment = class(TInterfacedObject, INyxFlowAssignment)
  public
    function GetTarget: TNyxText;
    function GetKind: TNyxStateKind;
    function GetSource: TNyxFlowEndpoint;
  end;

var
  GDocument: TNyxDocument;
  GGraph: INyxFlowGraph;
  GActor: TRunGraph;
  GCallback: INyxEventCallback;
  GFirst: INyxEventSubscription;
  GSecond: INyxEventSubscription;
  GRun: INyxQueryRun;
  GOtherRun: INyxQueryRun;
  GBefore: TNyxText;
  GExpectedPretty: TNyxText;
  GExpectedCompact: TNyxText;
  GRevision: Integer;
  GChecks: Integer;
  GStage: Integer;
  GClicks: Integer;
  GAfterClicks: Integer;
  GFinished: Boolean;
  {$ifdef PAS2JS}
  GApp: TNyxBrowserApplication;
  GOther: TNyxBrowserApplication;
  GHost: TJSHTMLElement;
  GOtherHost: TJSHTMLElement;
  GMemo: TJSHTMLTextAreaElement;
  GStarted: Double;
  {$else}
  GApp: TNyxLCLApplication;
  GOther: TNyxLCLApplication;
  GMemo: TMemo;
  GStarted: QWord;
  {$endif}

procedure Check(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxFlow.Create('Actual Logic application: ' + AMessage);
  end;
  Inc(GChecks);
end;

function Submit(const AContext: INyxApplicationContext;
  const ASource, AHeading: TNyxText; APolicy: TNyxExecutionPolicy): INyxQueryRun;
begin
  Result := TNyxFlowApplication.Submit(AContext, GGraph, [
    NyxFlowInput(LogicEndpoint('parse', nlpText), NyxFlowText(ASource)),
    NyxFlowInput(LogicEndpoint('summary', nlpLeft), NyxFlowText(AHeading))], [
    NyxFlowAssign(NyxTextState('pretty'), LogicEndpoint('pretty', nlpText)),
    NyxFlowAssign(NyxTextState('compact'), LogicEndpoint('compact', nlpText)),
    NyxFlowAssign(NyxTextState('summary'), LogicEndpoint('summary', nlpText))], APolicy);
end;

procedure TRunGraph.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin
  Inc(GClicks);
  LastRun := Submit(Context, Context.State.GetValue(NyxTextState('source')), 'Profile', neUIQueue);
end;

procedure TAfterRun.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin
  Check(GActor.LastRun <> nil, 'second ordered callback observes the graph submission');
  Inc(GAfterClicks);
end;

function TCancellingLogic.GetDescriptor: TNyxLogicDescriptor;
begin
  Result := NyxLogicDescriptor(NyxLogicKind('qualification-cancel'), 'Cancel inside work',
    'Qualification-only cooperative cancellation.', nlgFlow)
    .Output(NyxLogicPort(nlpText), nftText, 'Output that must not publish');
end;

function TRetiringAssignment.GetTarget: TNyxText;
begin
  Result := 'pretty';
end;

function TRetiringAssignment.GetKind: TNyxStateKind;
begin
  Result := nskText;
end;

function TRetiringAssignment.GetSource: TNyxFlowEndpoint;
begin
  FreeAndNil(GOther);
  Result := LogicEndpoint('pretty', nlpText);
end;

function TCancellingLogic.GetConfiguration: TNyxDataValue;
begin
  Result := NyxObject([]);
end;

function TCancellingLogic.Invoke(const AInputs: TNyxFlowValues;
  const ACancellation: INyxCancellationScope): TNyxFlowValues;
begin
  ACancellation.Cancel;
  Result := NyxFlowValues.WithValue(NyxLogicPort(nlpText), NyxFlowText('Do not publish'));
end;

function Settled(const ARun: INyxQueryRun): Boolean;
begin
  Result := (ARun = nil) or (ARun.Phase in
    [nqpSucceeded, nqpDiscarded, nqpCancelled, nqpFailed]);
end;

function PrettyText: TNyxText;
begin
  {$ifdef PAS2JS}
  Result := GApp.View.ElementFor('pretty-code').textContent;
  {$else}
  Result := TNyxText(TMemo(GApp.View.ControlFor('pretty-code')).Text);
  {$endif}
end;

procedure EditSource(const ASource: TNyxText);
begin
  {$ifdef PAS2JS}
  GMemo.value := ASource;
  GMemo.dispatchEvent(TJSEvent.new('input'));
  {$else}
  GMemo.Text := ASource;
  Application.ProcessMessages;
  {$endif}
  Check(GApp.State.GetValue(NyxTextState('source')) = ASource, 'physical input supplies captured graph source');
end;

procedure ClickRun;
begin
  {$ifdef PAS2JS}
  TJSHTMLButtonElement(GApp.View.ElementFor('run-button')).click;
  {$else}
  TNyxLCLButton(GApp.View.ControlFor('run-button')).Click;
  {$endif}
  GRun := GActor.LastRun;
  Check((GClicks = GAfterClicks) and (GFirst.LastExecution.Status = nesSucceeded),
    'ordinary multiple callbacks remain ordered and successful');
end;

procedure Step;
var
  LPolicy: TNyxExecutionPolicy;
  LRefused: Boolean;
  LCancelGraph: INyxFlowGraph;
  LRetiringContext: INyxApplicationContext;
  {$ifndef PAS2JS}
  LBitmap: TBitmap;
  LPNG: TPortableNetworkGraphic;
  {$endif}
begin

  if not Settled(GRun) or not Settled(GOtherRun) then
  begin
    Exit;
  end;
  case GStage of
    0:
      begin
        {$ifdef PAS2JS}
        GMemo := TJSHTMLTextAreaElement(GApp.View.InputFor('source-memo'));
        LPolicy := neAsynchronous;
        LRefused := False;
        try
          Submit(GApp.Context, LogicExample, 'Profile', neThreaded);
        except
          on E: ENyxSchedule do
          begin
            LRefused := True;
          end;
        end;
        Check(LRefused, 'browser refuses a fabricated worker thread');
        {$else}
        GMemo := TMemo(GApp.View.InputFor('source-memo'));
        LPolicy := neThreaded;
        {$endif}
        EditSource('{"profile":{"name":"Updated profile","amount":9007199254740993}}');
        GRevision := GApp.State.Revision;
        ClickRun;
        GOtherRun := Submit(GOther.Context, LogicExample, 'Independent', LPolicy);
      end;
    1:
      begin
        Check((GRun.Phase = nqpSucceeded) and (GOtherRun.Phase = nqpSucceeded),
          'UI and native-worker/browser-event-loop graphs publish completely');
        Check(GApp.State.Revision = GRevision + 1, 'three graph outputs publish through one atomic state revision');
        GExpectedPretty := GApp.State.GetValue(NyxTextState('pretty'));
        GExpectedCompact := GApp.State.GetValue(NyxTextState('compact'));
        Check(Pos('9007199254740993', GExpectedPretty) > 0, 'exact JSON numbers reach real application output');
        Check(TNyxDataValue.ParseJSON(PrettyText).ToJSON =
          TNyxDataValue.ParseJSON(GExpectedPretty).ToJSON, 'actual code control displays complete JSON output');
        Check(GOther.State.GetValue(NyxTextState('summary')) <>
          GApp.State.GetValue(NyxTextState('summary')), 'independent applications never share mutable result state');
        {$ifdef PAS2JS}
        Check(GApp.View.InputFor('source-memo') = GMemo, 'graph publication retains browser source editor');
        {$else}
        Check(GApp.View.InputFor('source-memo') = GMemo, 'graph publication retains native source editor');
        {$endif}
        EditSource('{ malformed');
        GRevision := GApp.State.Revision;
        ClickRun;
      end;
    2:
      begin
        Check((GRun.Phase = nqpFailed) and (Pos('parse', GRun.Failure) > 0),
          'runtime parse failure reports its operation');
        Check((GApp.State.Revision = GRevision) and
          (GApp.State.GetValue(NyxTextState('pretty')) = GExpectedPretty) and
          (GApp.State.GetValue(NyxTextState('compact')) = GExpectedCompact),
          'failed graph preserves every previous mapped output');
        GRun := Submit(GApp.Context, LogicExample, 'Cancelled', neUIQueue);
        GRun.Cancel;
      end;
    3:
      begin
        Check(GRun.Phase = nqpCancelled, 'pending graph cancellation revokes publication');
        Check(GApp.State.Revision = GRevision, 'cancelled graph changes no application state');
        LCancelGraph := NewNyxFlowGraph.WithStep(NyxFlowStep('cancel'),
          TCancellingLogic.Create).Build;
        {$ifdef PAS2JS}
        LPolicy := neAsynchronous;
        {$else}
        LPolicy := neThreaded;
        {$endif}
        GRun := TNyxFlowApplication.Submit(GApp.Context, LCancelGraph, [], [
          NyxFlowAssign(NyxTextState('pretty'), LogicEndpoint('cancel', nlpText)),
          NyxFlowAssign(NyxTextState('compact'), LogicEndpoint('cancel', nlpText))], LPolicy);
      end;
    4:
      begin
        Check(GRun.Phase = nqpCancelled, 'entered operation cancellation reaches the scheduler run');
        Check((GApp.State.Revision = GRevision) and
          (GApp.State.GetValue(NyxTextState('pretty')) = GExpectedPretty) and
          (GApp.State.GetValue(NyxTextState('compact')) = GExpectedCompact),
          'entered cancellation publishes neither output');
        GRun := Submit(GApp.Context, LogicExample, 'Stale', neUIQueue);
        GApp.State.SetValue(NyxTextState('guard'), 'A newer edit');
        GRevision := GApp.State.Revision;
      end;
    5:
      begin
        Check(GRun.Phase = nqpFailed, 'stale destination revision refuses complete delivery');
        Check((GApp.State.Revision = GRevision) and
          (GApp.State.GetValue(NyxTextState('pretty')) = GExpectedPretty),
          'stale work cannot overwrite current control state');
        GRun := Submit(GApp.Context, LogicExample, 'Retired', neUIQueue);
        GApp.ShowPage('other');
      end;
    6:
      begin
        Check(GRun.Phase in [nqpCancelled, nqpDiscarded], 'navigation revokes queued graph delivery');
        Check(GApp.State.GetValue(NyxTextState('pretty')) = GExpectedPretty,
          'retired graph output never enters a replacement view');
        Check(TNyxCodec.Encode(GDocument) = GBefore, 'graph runtimes do not mutate authored defaults or design');
        GApp.ShowPage('home');
        GFirst.Cancel;
        GSecond.Cancel;
        GFirst := GApp.View.Events.On(NyxControlEvents('run-button'), ntClick).Subscribe(GCallback);
        GSecond := GApp.View.Events.On(NyxControlEvents('run-button'), ntClick).Subscribe(TAfterRun.Create);
        {$ifdef PAS2JS}
        GMemo := TJSHTMLTextAreaElement(GApp.View.InputFor('source-memo'));
        {$else}
        GMemo := TMemo(GApp.View.InputFor('source-memo'));
        {$endif}
        EditSource(LogicExample);
        ClickRun;
      end;
    7:
      begin
        Check(GRun.Phase = nqpSucceeded, 'new page generation accepts its own fresh graph run');
        LRetiringContext := GOther.Context;
        LRefused := False;
        try
          TNyxFlowApplication.Submit(LRetiringContext, GGraph, [
            NyxFlowInput(LogicEndpoint('parse', nlpText), NyxFlowText(LogicExample)),
            NyxFlowInput(LogicEndpoint('summary', nlpLeft), NyxFlowText('Retired capture'))],
            [TRetiringAssignment.Create]);
        except
          on E: ENyxFlow do
          begin
            LRefused := True;
          end;
        end;
        Check(LRefused and not LRetiringContext.Active,
          'creator getter retirement refuses before dereferencing freed application state');
        {$ifdef PAS2JS}
        document.body.setAttribute('data-logic-controls-stage', 'complete');
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
        GFinished := True;
      end;
  end;
  Inc(GStage);
end;

procedure Retire;
begin

  if GFirst <> nil then
  begin
    GFirst.Cancel;
  end;

  if GSecond <> nil then
  begin
    GSecond.Cancel;
  end;
  FreeAndNil(GApp);
  FreeAndNil(GOther);
  GCallback := nil;
  GActor := nil;
  GRun := nil;
  GOtherRun := nil;
  GFirst := nil;
  GSecond := nil;
  GGraph := nil;
  FreeAndNil(GDocument);
end;

{$ifdef PAS2JS}
procedure Drive;
begin
  try

    if TJSDate.now - GStarted > 30000 then
    begin
      raise ENyxFlow.Create('Actual graph application deadline at stage ' + IntToStr(GStage));
    end;
    Step;

    if GFinished then
    begin
      document.body.setAttribute('data-logic-controls', 'passed');
      document.body.setAttribute('data-logic-controls-checks', IntToStr(GChecks));
      { Keep this ordinary fresh-generation application visible for review.
        The retired previous generation was checked before this remount. }
      Exit;
    end;
    window.setTimeout(@Drive, 20);
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-logic-controls', 'failed');
      document.body.setAttribute('data-logic-controls-error', E.Message);
      Retire;
    end;
  end;
end;
{$endif}

begin
  try
    {$ifdef NYX_LOGIC_SAVED_COMPANION}
    GDocument := BuildNyxDocument;
    GGraph := GDocument.Flows.Definition(NyxFlowRef('Profile 🐾')).Build;
    {$else}
    GDocument := LogicDocument;
    GGraph := LogicFixture;
    {$endif}
    GBefore := TNyxCodec.Encode(GDocument);
    {$ifdef PAS2JS}
    GHost := TJSHTMLElement(document.createElement('div'));
    GOtherHost := TJSHTMLElement(document.createElement('div'));
    document.body.appendChild(GHost);
    GOtherHost.style.setProperty('display', 'none');
    document.body.appendChild(GOtherHost);
    GApp := TNyxBrowserApplication.Create;
    GOther := TNyxBrowserApplication.Create;
    GApp.Run(GDocument, GHost);
    GOther.Run(GDocument, GOtherHost);
    {$else}
    Application.Initialize;
    GApp := TNyxLCLApplication.Create;
    GOther := TNyxLCLApplication.Create;
    GApp.Mount(GDocument);
    GOther.Mount(GDocument);
    GApp.Window.Show;
    {$endif}
    GActor := TRunGraph.Create;
    GCallback := GActor;
    GActor.Context := GApp.Context;
    GActor.Graph := GGraph;
    GFirst := GApp.View.Events.On(NyxControlEvents('run-button'), ntClick).Subscribe(GCallback);
    GSecond := GApp.View.Events.On(NyxControlEvents('run-button'), ntClick).Subscribe(TAfterRun.Create);
    {$ifdef PAS2JS}
    GStarted := TJSDate.now;
    Drive;
    {$else}
    GStarted := GetTickCount64;
    while not GFinished do
    begin
      CheckSynchronize;
      Application.ProcessMessages;

      if GetTickCount64 - GStarted > 30000 then
      begin
        raise ENyxFlow.Create('Actual graph application deadline at stage ' + IntToStr(GStage));
      end;
      Step;
      Sleep(1);
    end;
    Retire;
    WriteLn('PASS ', GChecks, ' actual Logic application checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-logic-controls', 'failed');
      document.body.setAttribute('data-logic-controls-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
      Retire;
    end;
  end;
end.
