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
program nyx_regex_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.state, nyx.model, nyx.codec,
  nyx.collections, nyx.data.rows, nyx.test.logic.regex, nyx.logic.regex, nyx.regex.types, nyx.generated.view,
  nyx.flow, nyx.flow.types, nyx.flow.declarations, nyx.logic,
  nyx.flow.execution, nyx.linq.execution, nyx.scheduler
  {$ifdef PAS2JS}, JS, Web, nyx.application.browser
  {$else}, Interfaces, Classes, Forms, StdCtrls, Grids, Graphics,
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
  GRowsBefore: INyxCollectionSnapshot;
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
  TJSHTMLTextAreaElement(GApp.View.InputFor('people-memo')).value := AText;
  GApp.View.InputFor('people-memo').dispatchEvent(TJSEvent.new('input'));
  {$else}
  TMemo(GApp.View.InputFor('people-memo')).Text := AText;
  {$endif}
end;

function Cell(AColumn, ARow: Integer): TNyxText;
{$ifdef PAS2JS}
var
  LCells: TJSNodeList;
{$endif}
begin
  {$ifdef PAS2JS}
  LCells := GApp.View.ElementFor('names-table').querySelectorAll('[role="gridcell"]');
  Result := LCells.item(ARow * 3 + AColumn).textContent;
  {$else}
  Result := TNyxText(TStringGrid(GApp.View.ControlFor('names-table')).Cells[AColumn, ARow + 1]);
  {$endif}
end;

function Idle: Boolean;
begin
  Result := GApp.FlowRoutes.Report(0).Field('active').AsInteger = 0;
end;

procedure SaveFrame;
begin
  GResultBefore := GApp.State.GetValue(NyxTextState('formatted'));
  GSourceBefore := GApp.State.GetValue(NyxTextState('captures'));
  GRowsBefore := GApp.Collections.Collection(NyxCollection('names')).Snapshot;
end;

procedure RetainedFrame;
var
  LNow: INyxCollectionSnapshot;
  LIndex: Integer;
begin
  LNow := GApp.Collections.Collection(NyxCollection('names')).Snapshot;
  Check((GApp.State.GetValue(NyxTextState('formatted')) = GResultBefore) and
    (GApp.State.GetValue(NyxTextState('captures')) = GSourceBefore) and
    (LNow.Count = GRowsBefore.Count) and (LNow.Revision = GRowsBefore.Revision),
    'Failure/cancellation/retirement retain the complete scalar and Table frame');
  for LIndex := 0 to LNow.Count - 1 do
  begin
    Check(LNow.ItemAt(LIndex).SameItem(GRowsBefore.ItemAt(LIndex)), 'Retained Table row remains exact');
  end;
end;

procedure SubmitWorker(const AText: TNyxText; AWork: Integer);
var
  LOptions: TNyxRegexOptions;
  LGraph: INyxFlowGraph;
begin
  LOptions := RegexOptions.Limits(NyxRegexLimits.WorkSteps(AWork));
  LGraph := NewNyxFlowGraph
    .WithStep(NyxFlowStep('replace'), NewNyxRegexReplace(LOptions, RegexNameReplacement, nruError))
    .WithStep(NyxFlowStep('extract'), NewNyxRegexExtract(LOptions,
      NyxRegexCapture(NyxRegexGroup('last')), nrxSkip)).Build;
  GRun := TNyxFlowApplication.Submit(GApp.Context, LGraph, [
    NyxFlowInput(RegexEndpoint('replace', NyxRegexPort(nrpInput)), NyxFlowText(AText)),
    NyxFlowInput(RegexEndpoint('extract', NyxRegexPort(nrpInput)), NyxFlowText(AText))], [
    NyxFlowAssign(NyxTextState('formatted'), RegexEndpoint('replace', NyxRegexPort(nrpValue)))], [
    NyxFlowAssignRows(NyxCollection('names'), RegexEndpoint('extract', NyxRegexPort(nrpValue)), RegexRows)],
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
      Check(GApp.FlowRoutes.Count = 1, 'Ordinary application mounts the exact saved regex route');
      Click;
    end;
    1:
    begin

      if not Idle then
      begin
        Exit;
      end;
      Check((GApp.Collections.Collection(NyxCollection('names')).Snapshot.Count = 2) and
        (Cell(0, 0) = 'Lovelace') and (Cell(0, 1) = 'Hopper'),
        'Actual ordinary Table displays extracted captures');
      Check((Cell(1, 0) = '4') and (Cell(2, 0) = '8'), 'Table spans retain scalar start and length');
      Check(GApp.State.GetValue(NyxTextState('formatted')) = 'Lovelace, Ada' + #10 + 'Hopper, Grace',
        'Saved structural capture template formats the complete mailing list');
      Check(GOther.Collections.Collection(NyxCollection('names')).Snapshot.Count = 0,
        'Independent application instances own independent live results');
      Input('Jane O''Neil' + #10 + 'Alan Turing');
    end;
    2:
    begin
      Check(Pos('Jane O''Neil', GApp.State.GetValue(NyxTextState('people'))) > 0,
        'Actual memo input reaches exact runtime text');
      Click;
    end;
    3:
    begin

      if not Idle then
      begin
        Exit;
      end;
      Check((Cell(0, 0) = 'O''Neil') and (Cell(0, 1) = 'Turing'),
        'Live names with apostrophes reach typed extracted rows');
      Check(Pos('O''Neil, Jane', GApp.State.GetValue(NyxTextState('formatted'))) > 0,
        'Same live input reaches structural replacement output');
      Input('SingleName');
      Click;
    end;
    4:
    begin

      if not Idle then
      begin
        Exit;
      end;
      Check((GApp.Collections.Collection(NyxCollection('names')).Snapshot.Count = 0) and
        (GApp.State.GetValue(NyxTextState('formatted')) = 'SingleName'),
        'A genuine no-match clears extracted rows and retains unmatched input text');
      Input(RegexPeople);
      Click;
    end;
    5:
    begin

      if not Idle then
      begin
        Exit;
      end;
      SaveFrame;
      SubmitWorker('Barbara Liskov', 5);
    end;
    6:
    begin

      if GRun.Phase in [nqpPending, nqpComputing, nqpAwaitingUI] then
      begin
        Exit;
      end;
      Check(GRun.Phase = nqpFailed, 'An exhausted matching budget is an inspectable failure, never no-match');
      RetainedFrame;
      SubmitWorker('Barbara Liskov', 8000000);
    end;
    7:
    begin

      if GRun.Phase in [nqpPending, nqpComputing, nqpAwaitingUI] then
      begin
        Exit;
      end;
      Check((GRun.Phase = nqpSucceeded) and
        (GApp.State.GetValue(NyxTextState('formatted')) = 'Liskov, Barbara') and
        (Cell(0, 0) = 'Liskov') and (Cell(1, 0) = '8') and (Cell(2, 0) = '6'),
        'Native worker/browser loop publishes exact text and typed Table rows through the UI courier');
      SaveFrame;
      {$ifdef PAS2JS}
      GApp.View.ElementFor('formatted-title').scrollIntoView(True);
      document.body.setAttribute('data-capture-checkpoint', 'regex-results');
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

      if document.body.getAttribute('data-capture-observed') <> 'regex-results' then
      begin
        Exit;
      end;
      {$endif}
      SubmitWorker('New Result', 8000000);
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
      Check((GApp.Collections.Collection(NyxCollection('names')).Snapshot.Count = 2) and
        (Cell(0, 0) = 'Lovelace') and (Cell(0, 1) = 'Hopper'), 'Fresh page generation reactivates the saved regex workflow');
      Check(TNyxCodec.Encode(GDocument) = GBefore, 'Execution retains the exact saved regex/default/route document');
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
  GRowsBefore := nil;
  FreeAndNil(GDocument);
end;

{$ifdef PAS2JS}
procedure Drive;
begin
  try

    if TJSDate.now - GStarted > 60000 then
    begin
      raise Exception.Create('Regex application deadline at stage ' + IntToStr(GStage));
    end;
    Step;

    if GFinished then
    begin
      document.body.setAttribute('data-regex-controls', 'passed');
      document.body.setAttribute('data-regex-controls-checks', IntToStr(GChecks));
      Retire;
      Exit;
    end;
    window.setTimeout(@Drive, 30);
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-regex-controls', 'failed');
      document.body.setAttribute('data-regex-controls-error', E.Message);
      Retire;
    end;
  end;
end;
{$endif}

begin
  try
    GDocument := BuildNyxDocument;
    GExpected := RegexDocument;
    try
      Check(TNyxCodec.Encode(GDocument) = TNyxCodec.Encode(GExpected),
        'Compiled semantic-authored source reconstructs the exact whole regex application');
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
        raise Exception.Create('Native regex application deadline at stage ' + IntToStr(GStage));
      end;
      Sleep(1);
    end;
    Retire;
    WriteLn('PASS ', GChecks, ' compiled regex application checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-regex-controls', 'failed');
      document.body.setAttribute('data-regex-controls-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
      Retire;
    end;
  end;
end.
