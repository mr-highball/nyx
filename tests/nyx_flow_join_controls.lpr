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
program nyx_flow_join_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.state, nyx.model, nyx.codec,
  nyx.test.flow.joins, nyx.logic.joins, nyx.generated.view,
  nyx.flow, nyx.flow.types, nyx.flow.declarations, nyx.logic,
  nyx.flow.execution, nyx.linq.execution, nyx.scheduler
  {$ifdef PAS2JS}, JS, Web, nyx.application.browser
  {$else}, Interfaces, Classes, Forms, StdCtrls, Graphics,
    nyx.widgets.lcl, nyx.application.lcl{$endif};

var
  GDocument: TNyxDocument;
  GExpected: TNyxDocument;
  GBefore: TNyxText;
  GChecks: Integer;
  GStage: Integer;
  GFinished: Boolean;
  GResultBefore: TNyxText;
  GSourceBefore: TNyxText;
  GRun: INyxQueryRun;
  {$ifdef PAS2JS}
  GApp: TNyxBrowserApplication;
  GOther: TNyxBrowserApplication;
  GHost: TJSHTMLElement;
  GOtherHost: TJSHTMLElement;
  GStarted: Double;
  {$else}
  GApp: TNyxLCLApplication;
  GOther: TNyxLCLApplication;
  GStarted: QWord;
  {$endif}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure Click;
begin
  {$ifdef PAS2JS}
  GApp.View.ElementFor('run-button').click;
  {$else}
  TNyxLCLButton(GApp.View.ControlFor('run-button')).Click;
  {$endif}
end;


procedure Input(const AText: TNyxText);
begin
  {$ifdef PAS2JS}
  TJSHTMLTextAreaElement(GApp.View.InputFor('primary-memo')).value := AText;
  GApp.View.InputFor('primary-memo').dispatchEvent(TJSEvent.new('input'));
  {$else}
  TMemo(GApp.View.InputFor('primary-memo')).Text := AText;
  {$endif}
end;

procedure Toggle(const AName: TNyxText; AValue: Boolean);
begin
  {$ifdef PAS2JS}
  TJSHTMLInputElement(GApp.View.InputFor(AName + '-toggle')).checked := AValue;
  GApp.View.InputFor(AName + '-toggle').dispatchEvent(TJSEvent.new('change'));
  {$else}
  TCheckBox(GApp.View.InputFor(AName + '-toggle')).Checked := AValue;
  {$endif}
  Check(GApp.State.GetValue(NyxBooleanState('use-' + AName)) = AValue,
    'Actual checkbox input publishes typed Boolean state');
end;

function Idle: Boolean;
begin
  Result := GApp.FlowRoutes.Report(0).Field('active').AsInteger = 0;
end;

procedure SaveFrame;
begin
  GResultBefore := GApp.State.GetValue(NyxTextState('result'));
  GSourceBefore := GApp.State.GetValue(NyxTextState('source'));
end;

procedure RetainedFrame;
begin
  Check((GApp.State.GetValue(NyxTextState('result')) = GResultBefore) and
    (GApp.State.GetValue(NyxTextState('source')) = GSourceBefore),
    'Failed/skipped/cancelled/retired branches retain the complete previous output frame');
end;

function WorkerGraph: INyxFlowGraph;
begin
  Result := JoinGraph(nfjFirstAvailable);
end;

procedure SubmitWorker;
begin
  GRun := TNyxFlowApplication.Submit(GApp.Context, WorkerGraph, [
    NyxFlowInput(JoinEndpoint('primary-gate', 'enabled'), NyxFlowBoolean(True)),
    NyxFlowInput(JoinEndpoint('fallback-gate', 'enabled'), NyxFlowBoolean(True)),
    NyxFlowInput(JoinEndpoint('primary', 'text'), NyxFlowText('{"message":"Worker primary"}')),
    NyxFlowInput(JoinEndpoint('fallback', 'text'), NyxFlowText('{"message":"Worker fallback"}'))], [
    NyxFlowAssign(NyxTextState('result'), JoinEndpoint('format', 'text')),
    NyxFlowAssign(NyxTextState('source'), JoinEndpoint('choice', 'source'))],
    {$ifdef PAS2JS}neAsynchronous{$else}neThreaded{$endif});
end;

procedure Step;
{$ifndef PAS2JS}
var
  LBitmap: TBitmap;
  LPNG: TPortableNetworkGraphic;
{$endif}
begin
  case GStage of
    0:
    begin
      Check(GApp.FlowRoutes.Count = 1, 'Ordinary application mounts the exact saved conditional route');
      Click;
    end;
    1:
    begin

      if not Idle then
      begin
        Exit;
      end;
      Check((GApp.State.GetValue(NyxTextState('result')) = '{"message":"Primary"}') and
        (GApp.State.GetValue(NyxTextState('source')) = 'primary'), 'Exactly one selected branch publishes value and source');
      Check(GOther.State.GetValue(NyxTextState('source')) = 'No branch selected yet.',
        'Independent application instances own independent live outputs');
      Toggle('primary', False);
      Toggle('fallback', True);
      Click;
    end;
    2:
    begin

      if not Idle then
      begin
        Exit;
      end;
      Check((GApp.State.GetValue(NyxTextState('result')) = '{"message":"Fallback"}') and
        (GApp.State.GetValue(NyxTextState('source')) = 'fallback'), 'Skipped primary allows fallback to execute');
      {$ifdef PAS2JS}
      Check(GApp.View.ElementFor('source-label').textContent = 'fallback',
        'Actual label displays selected branch reference');
      {$else}
      Check(TLabel(GApp.View.ControlFor('source-label')).Caption = 'fallback',
        'Actual label displays selected branch reference');
      {$endif}
      SaveFrame;
      Toggle('primary', True);
      Click;
    end;
    3:
    begin

      if not Idle then
      begin
        Exit;
      end;
      Check((GApp.FlowRoutes.Report(0).Field('failed').AsInteger = 1) and
        (GApp.FlowRoutes.Report(0).Field('failure').AsText <> ''), 'Ambiguous branches expose a useful route failure');
      RetainedFrame;
      Toggle('primary', False);
      Toggle('fallback', False);
      Click;
    end;
    4:
    begin

      if not Idle then
      begin
        Exit;
      end;
      RetainedFrame;
      Check(GApp.FlowRoutes.Report(0).Field('failed').AsInteger = 1,
        'No selected branch is a successful skip, not a failed run');
      Toggle('primary', True);
      Input('{invalid selected branch');
      Click;
    end;
    5:
    begin

      if not Idle then
      begin
        Exit;
      end;
      Check(GApp.FlowRoutes.Report(0).Field('failed').AsInteger = 2,
        'Selected branch error is never interpreted as availability fallback');
      RetainedFrame;
      Input('null');
      Check(GApp.State.GetValue(NyxTextState('primary')) = 'null', 'Actual memo edit reaches runtime state');
      Click;
    end;
    6:
    begin

      if not Idle then
      begin
        Exit;
      end;
      Check((GApp.State.GetValue(NyxTextState('result')) = 'null') and
        (GApp.State.GetValue(NyxTextState('source')) = 'primary'), 'Present JSON null executes downstream output');
      SubmitWorker;
    end;
    7:
    begin

      if GRun.Phase in [nqpPending, nqpComputing, nqpAwaitingUI] then
      begin
        Exit;
      end;
      Check((GRun.Phase = nqpSucceeded) and
        (GApp.State.GetValue(NyxTextState('result')) = '{"message":"Worker primary"}') and
        (GApp.State.GetValue(NyxTextState('source')) = 'primary'),
        'Native worker/browser loop uses declared priority and publishes only through UI');
      SaveFrame;
      {$ifdef PAS2JS}
      GApp.View.ElementFor('result-title').scrollIntoView(True);
      document.body.setAttribute('data-capture-checkpoint', 'joined-branches');
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
    8:
    begin
      {$ifdef PAS2JS}

      if document.body.getAttribute('data-capture-observed') <> 'joined-branches' then
      begin
        Exit;
      end;
      {$endif}
      SubmitWorker;
      GRun.Cancel;
    end;
    9:
    begin

      if GRun.Phase in [nqpPending, nqpComputing, nqpAwaitingUI] then
      begin
        Exit;
      end;
      RetainedFrame;
      Click;
      GApp.ShowPage('other');
    end;
    10:
    begin
      RetainedFrame;
      GApp.ShowPage('home');
      Click;
    end;
    11:
    begin

      if not Idle then
      begin
        Exit;
      end;
      Check((GApp.State.GetValue(NyxTextState('result')) = 'null') and
        (GApp.State.GetValue(NyxTextState('source')) = 'primary'), 'Fresh view generation can execute the saved graph again');
      Check(TNyxCodec.Encode(GDocument) = GBefore, 'Execution retains exact saved defaults, graph and routes');
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
  FreeAndNil(GApp);
  FreeAndNil(GOther);
  GRun := nil;
  FreeAndNil(GDocument);
end;

{$ifdef PAS2JS}
procedure Drive;
begin
  try

    if TJSDate.now - GStarted > 60000 then
    begin
      raise Exception.Create('Branch application deadline at stage ' + IntToStr(GStage));
    end;
    Step;

    if GFinished then
    begin
      document.body.setAttribute('data-join-controls', 'passed');
      document.body.setAttribute('data-join-controls-checks', IntToStr(GChecks));
      Retire;
      Exit;
    end;
    window.setTimeout(@Drive, 30);
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-join-controls', 'failed');
      document.body.setAttribute('data-join-controls-error', E.Message);
      Retire;
    end;
  end;
end;
{$endif}

begin
  try
    GDocument := BuildNyxDocument;
    GExpected := JoinDocument;
    try
      Check(TNyxCodec.Encode(GDocument) = TNyxCodec.Encode(GExpected),
        'Compiled semantic-authored source reconstructs the exact whole branch application');
    finally
      FreeAndNil(GExpected);
    end;
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
    GApp.Window.SetBounds(10, 10, 760, 1050);
    GApp.Window.Show;
    GStarted := GetTickCount64;
    while not GFinished do
    begin
      CheckSynchronize;
      Application.ProcessMessages;
      Step;

      if GetTickCount64 - GStarted > 60000 then
      begin
        raise Exception.Create('Native branch application deadline at stage ' + IntToStr(GStage));
      end;
      Sleep(1);
    end;
    Retire;
    WriteLn('PASS ', GChecks, ' compiled branch application checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-join-controls', 'failed');
      document.body.setAttribute('data-join-controls-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
      Retire;
    end;
  end;
end.
