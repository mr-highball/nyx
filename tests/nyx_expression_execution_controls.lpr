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

program nyx_expression_execution_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.state, nyx.expressions,
  nyx.expressions.execution, nyx.linq.execution, nyx.application.context,
  nyx.scheduler, nyx.model, nyx.codec, nyx.binding.types, nyx.generated.view
  {$ifdef PAS2JS}, JS, Web, nyx.application.browser
  {$else}, Interfaces, Classes, Forms, Controls, StdCtrls, nyx.application.lcl{$endif};

type
  { Extension getters run during UI admission, not on a worker. This deliberate
    navigation tests revocation before the captured raw state identity is used. }
  TReentrantAssignment = class(TInterfacedObject, INyxExpressionAssignment)
  public
    function GetName: TNyxText;
    function GetExpression: INyxExpression;
  end;

var
  {$ifdef PAS2JS}
  GApp: TNyxBrowserApplication;
  GOther: TNyxBrowserApplication;
  GHost: TJSHTMLElement;
  GOtherHost: TJSHTMLElement;
  GLabel: TJSHTMLElement;
  GNotes: TJSHTMLTextAreaElement;
  GStarted: Double;
  {$else}
  GApp: TNyxLCLApplication;
  GOther: TNyxLCLApplication;
  GLabel: TControl;
  GNotes: TMemo;
  GStarted: QWord;
  {$endif}
  GDocument: TNyxDocument;
  GAuthored: TNyxText;
  GContext: INyxApplicationContext;
  GRun: INyxQueryRun;
  GOtherRun: INyxQueryRun;
  GStage: Integer;
  GChecks: Integer;
  GRevision: Integer;
  GFinished: Boolean;

function TReentrantAssignment.GetName: TNyxText;
begin
  Result := 'quantity';
end;

function TReentrantAssignment.GetExpression: INyxExpression;
begin
  GApp.ShowPage('review');
  Result := NyxIntegerExpression(99);
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxExpression.Create('Expression execution: ' + AReason);
  end;
  Inc(GChecks);
end;

function Caption: TNyxText;
begin
  {$ifdef PAS2JS}
  Result := GApp.View.ElementFor('order-total').textContent;
  {$else}
  Result := TNyxText(TLabel(GApp.View.ControlFor('order-total')).Caption);
  {$endif}
end;

function Enabled: Boolean;
begin
  {$ifdef PAS2JS}
  Result := not TJSHTMLButtonElement(GApp.View.ElementFor('order-submit')).disabled;
  {$else}
  Result := GApp.View.ControlFor('order-submit').Enabled;
  {$endif}
end;

function Settled(const ARun: INyxQueryRun): Boolean;
begin
  Result := (ARun = nil) or (ARun.Phase in
    [nqpSucceeded, nqpDiscarded, nqpCancelled, nqpFailed]);
end;

function Calculate(APolicy: TNyxExecutionPolicy): INyxQueryRun;
begin
  Result := TNyxExpressionExecution.Submit(GApp.Context, [
    NyxCompute(NyxNumberState('price'),
      NyxNumberExpression(NyxNumberState('price')).Plus(NyxNumberExpression(1))),
    NyxCompute(NyxIntegerState('quantity'),
      NyxIntegerExpression(NyxIntegerState('quantity')).Times(NyxIntegerExpression(2))),
    NyxCompute(NyxBooleanState('ready'), NyxBooleanExpression(False))], APolicy);
end;

procedure Step;
var
  LRefused: Boolean;
  LPolicy: TNyxExecutionPolicy;
  LReentrant: INyxExpressionAssignment;
begin

  if not Settled(GRun) or not Settled(GOtherRun) then
  begin
    Exit;
  end;
  case GStage of
    0:
      begin
        Check(Caption = 'Total: 40', 'initial computed caption reaches the actual control');
        Check(Enabled, 'initial condition reaches the actual button');
        {$ifdef PAS2JS}
        GLabel := GApp.View.ElementFor('order-total');
        GNotes := TJSHTMLTextAreaElement(GApp.View.InputFor('order-notes'));
        GNotes.focus;
        GNotes.value := 'Unfinished worker notes';
        LPolicy := neAsynchronous;
        LRefused := False;
        try
          Calculate(neThreaded);
        except
          on ENyxSchedule do
          begin
            LRefused := True;
          end;
        end;
        Check(LRefused, 'browser refuses real-worker policy before publication');
        {$else}
        GLabel := GApp.View.ControlFor('order-total');
        GNotes := TMemo(GApp.View.InputFor('order-notes'));
        GNotes.SetFocus;
        GNotes.Text := 'Unfinished worker notes';
        LPolicy := neThreaded;
        {$endif}
        GRevision := GApp.State.Revision;
        GRun := Calculate(LPolicy);
      end;
    1:
      begin
        Check(GRun.Phase = nqpSucceeded, 'complete expression run delivers on UI');
        Check((Caption = 'Total: 83.5') and not Enabled,
          'one worker/event-loop batch updates actual calculated caption and button');
        Check(GApp.State.Revision = GRevision + 1, 'multiple outputs publish one state revision');
        {$ifdef PAS2JS}
        Check((GApp.View.ElementFor('order-total') = GLabel) and
          (GApp.View.InputFor('order-notes') = GNotes) and
          (GNotes.value = 'Unfinished worker notes') and (document.activeElement = GNotes),
          'actual browser control identity, draft and focus survive publication');
        {$else}
        Check((GApp.View.ControlFor('order-total') = GLabel) and
          (GApp.View.InputFor('order-notes') = GNotes) and
          (GNotes.Text = 'Unfinished worker notes') and GNotes.Focused,
          'actual native control identity, draft and focus survive publication');
        {$endif}
        Check(GOther.State.GetValue(NyxIntegerState('quantity')) = 3,
          'another runtime remains independent');
        GRevision := GApp.State.Revision;
        GRun := TNyxExpressionExecution.Submit(GApp.Context, [
          NyxCompute(NyxIntegerState('quantity'), NyxIntegerExpression(8)),
          NyxCompute(NyxNumberState('price'), NyxNumberExpression(1)
            .DividedBy(NyxNumberExpression(0)))]);
      end;
    2:
      begin
        Check((GRun.Phase = nqpFailed) and (GRun.Failure <> ''),
          'arithmetic failure has a retained terminal diagnostic');
        Check((GApp.State.Revision = GRevision) and (Caption = 'Total: 83.5'),
          'late expression failure publishes no earlier output or control change');
        GRun := TNyxExpressionExecution.Submit(GApp.Context, [
          NyxCompute(NyxIntegerState('quantity'), NyxIntegerExpression(9))]);
        GApp.State.SetValue(NyxNumberState('tax'), 4);
        GRevision := GApp.State.Revision;
      end;
    3:
      begin
        Check((GRun.Phase = nqpFailed) and (GApp.State.Revision = GRevision) and
          (GApp.State.GetValue(NyxIntegerState('quantity')) = 6),
          'stale queued result retains the newer state exactly');
        GRun := TNyxExpressionExecution.Submit(GApp.Context, [
          NyxCompute(NyxIntegerState('quantity'), NyxIntegerExpression(10))]);
        GRun.Cancel;
      end;
    4:
      begin
        Check((GRun.Phase = nqpCancelled) and
          (GApp.State.GetValue(NyxIntegerState('quantity')) = 6),
          'cancelled result cannot publish');
        GRun := TNyxExpressionExecution.Submit(GApp.Context, [
          NyxCompute(NyxIntegerState('quantity'), NyxIntegerExpression(11))]);
        GApp.ShowPage('review');
      end;
    5:
      begin
        Check((GRun.Phase = nqpCancelled) and
          (GApp.State.GetValue(NyxIntegerState('quantity')) = 6),
          'page generation retires queued computation/delivery');
        GApp.ShowPage('home');
        GRevision := GApp.State.Revision;
        GRun := TNyxExpressionExecution.Submit(GApp.Context, [
          NyxCompute(NyxIntegerState('quantity'), NyxIntegerExpression(7)),
          NyxCompute(NyxNumberState('price'),
            NyxIntegerExpression(NyxIntegerState('quantity')).AsNumber)]);
      end;
    6:
      begin
        Check((GRun.Phase = nqpSucceeded) and
          (GApp.State.GetValue(NyxIntegerState('quantity')) = 7) and
          (GApp.State.GetValue(NyxNumberState('price')) = 6),
          'group reads one immutable initial frame, never previous outputs');
        LRefused := False;
        try
          TNyxExpressionExecution.Submit(GApp.Context, [
            NyxCompute(NyxIntegerState('quantity'), NyxIntegerExpression(1)),
            NyxCompute(NyxIntegerState('quantity'), NyxIntegerExpression(2))]);
        except
          on ENyxExpression do
          begin
            LRefused := True;
          end;
        end;
        Check(LRefused and (GApp.State.Revision = GRevision + 1),
          'duplicate destinations refuse before queue admission');
        {$ifdef PAS2JS}
        LPolicy := neAsynchronous;
        {$else}
        LPolicy := neThreaded;
        {$endif}
        GRevision := GApp.State.Revision;
        GRun := TNyxExpressionExecution.Submit(GApp.Context, [
          NyxCompute(NyxIntegerState('quantity'), NyxIntegerExpression(13))], LPolicy);
        GOtherRun := TNyxExpressionExecution.Submit(GApp.Context, [
          NyxCompute(NyxIntegerState('quantity'), NyxIntegerExpression(14))], LPolicy);
      end;
    7:
      begin
        Check(((GRun.Phase = nqpSucceeded) and (GOtherRun.Phase = nqpFailed)) or
          ((GRun.Phase = nqpFailed) and (GOtherRun.Phase = nqpSucceeded)),
          'overlapping immutable runs allow one commit and refuse obsolete delivery');
        Check((GApp.State.Revision = GRevision + 1) and
          (GApp.State.GetValue(NyxIntegerState('quantity')) in [13, 14]),
          'overlap publishes exactly one complete result');
        GOtherRun := nil;
        LRefused := False;
        LReentrant := TReentrantAssignment.Create;
        try
          TNyxExpressionExecution.Submit(GApp.Context, [LReentrant]);
        except
          on ENyxExpression do
          begin
            LRefused := True;
          end;
        end;
        Check(LRefused and (GApp.State.Revision = GRevision + 1),
          'reentrant extension navigation refuses before submitting captured work');
        GApp.ShowPage('home');
        GContext := GApp.Context;
        GRun := TNyxExpressionExecution.Submit(GContext, [
          NyxCompute(NyxIntegerState('quantity'), NyxIntegerExpression(12))]);
        FreeAndNil(GApp);
      end;
    8:
      begin
        Check(not GContext.Active and (GRun.Phase = nqpCancelled),
          'application teardown revokes retained context and queued output');
        Check(TNyxCodec.Encode(GDocument) = GAuthored,
          'runtime workers never mutate authored defaults or design');
        GFinished := True;
      end;
  end;
  Inc(GStage);
end;

procedure Retire;
begin
  FreeAndNil(GApp);
  FreeAndNil(GOther);
  GContext := nil;
  GRun := nil;
  GOtherRun := nil;
  FreeAndNil(GDocument);
end;

{$ifdef PAS2JS}
procedure Drive;
begin
  try

    if TJSDate.now - GStarted > 60000 then
    begin
      raise Exception.Create('Expression execution timeout at stage ' + IntToStr(GStage));
    end;
    Step;

    if GFinished then
    begin
      Retire;
      document.body.setAttribute('data-expression-execution', 'passed');
      document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' actual expression execution checks';
      Exit;
    end;
    window.setTimeout(@Drive, 20);
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-expression-execution', 'failed');
      document.body.setAttribute('data-expression-error', LException.Message);
      Retire;
    end;
  end;
end;
{$endif}

begin
  try
    GDocument := BuildNyxDocument;
    GDocument.Find('order-total').SetBinding(TNyxBindingSpec.Computed(bpText,
      NyxTextExpression('Total: ').Append(
        NyxNumberExpression(NyxNumberState('price')).Times(
          NyxIntegerExpression(NyxIntegerState('quantity')).AsNumber)
          .Plus(NyxNumberExpression(NyxNumberState('tax'))).AsText)));
    GDocument.Find('order-submit').SetBinding(TNyxBindingSpec.Computed(bpEnabled,
      NyxBooleanExpression(NyxBooleanState('ready'))));
    GAuthored := TNyxCodec.Encode(GDocument);
    {$ifdef PAS2JS}
    GHost := TJSHTMLElement(document.createElement('div'));
    GOtherHost := TJSHTMLElement(document.createElement('div'));
    document.body.appendChild(GHost);
    document.body.appendChild(GOtherHost);
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
    GOther.Window.Show;
    GApp.Window.Show;
    GStarted := GetTickCount64;
    while not GFinished do
    begin
      CheckSynchronize;
      Application.ProcessMessages;

      if GetTickCount64 - GStarted > 60000 then
      begin
        raise Exception.Create('Expression execution timeout at stage ' + IntToStr(GStage));
      end;
      Step;
      Sleep(1);
    end;
    Retire;
    WriteLn('PASS ', GChecks, ' actual expression execution checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-expression-execution', 'failed');
      document.body.setAttribute('data-expression-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
      Retire;
    end;
  end;
end.
