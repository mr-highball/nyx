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
program nyx_logic_relations_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.state, nyx.model, nyx.codec,
  nyx.collections, nyx.test.logic.relations, nyx.generated.view,
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
  GOrdersBefore: INyxCollectionSnapshot;
  GRegionsBefore: INyxCollectionSnapshot;
  GTotalBefore: TNyxText;
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
  TJSHTMLTextAreaElement(GApp.View.InputFor('customers-memo')).value := AText;
  GApp.View.InputFor('customers-memo').dispatchEvent(TJSEvent.new('input'));
  {$else}
  TMemo(GApp.View.InputFor('customers-memo')).Text := AText;
  {$endif}
end;

function Cell(const ATable: TNyxText; AColumns, AColumn, ARow: Integer): TNyxText;
{$ifdef PAS2JS}
var
  LCells: TJSNodeList;
{$endif}
begin
  {$ifdef PAS2JS}
  LCells := GApp.View.ElementFor(ATable).querySelectorAll('[role="gridcell"]');
  Result := LCells.item(ARow * AColumns + AColumn).textContent;
  {$else}
  Result := TNyxText(TStringGrid(GApp.View.ControlFor(ATable)).Cells[AColumn, ARow + 1]);
  {$endif}
end;

function Idle: Boolean;
begin
  Result := GApp.FlowRoutes.Report(0).Field('active').AsInteger = 0;
end;

function SameRows(const ALeft, ARight: INyxCollectionSnapshot): Boolean;
var
  LIndex: Integer;
begin
  Result := (ALeft.Count = ARight.Count) and (ALeft.Revision = ARight.Revision);

  if not Result then
  begin
    Exit;
  end;
  for LIndex := 0 to ALeft.Count - 1 do
  begin

    if not ALeft.ItemAt(LIndex).SameItem(ARight.ItemAt(LIndex)) then
    begin
      Exit(False);
    end;
  end;
end;

procedure SaveFrame;
begin
  GOrdersBefore := GApp.Collections.Collection(NyxCollection('orders')).Snapshot;
  GRegionsBefore := GApp.Collections.Collection(NyxCollection('regions')).Snapshot;
  GTotalBefore := GApp.State.GetValue(NyxTextState('total'));
end;

procedure RetainedFrame;
begin
  Check(SameRows(GOrdersBefore, GApp.Collections.Collection(NyxCollection('orders')).Snapshot) and
    SameRows(GRegionsBefore, GApp.Collections.Collection(NyxCollection('regions')).Snapshot) and
    (GApp.State.GetValue(NyxTextState('total')) = GTotalBefore),
    'Rejected run retains both complete collections/revisions and scalar output');
end;

procedure Step;
var
  LOrders: INyxCollectionSnapshot;
  LRegions: INyxCollectionSnapshot;
  {$ifndef PAS2JS}
  LBitmap: TBitmap;
  LPNG: TPortableNetworkGraphic;
  {$endif}
begin
  case GStage of
    0:
    begin
      Check(GApp.FlowRoutes.Count = 1, 'Ordinary application mounts the complete saved relation route');
      Click;
    end;
    1:
    begin

      if not Idle then
      begin
        Exit;
      end;
      LOrders := GApp.Collections.Collection(NyxCollection('orders')).Snapshot;
      LRegions := GApp.Collections.Collection(NyxCollection('regions')).Snapshot;
      Check((LOrders.Count = 3) and (LRegions.Count = 2), 'Distinct/join/group produce both bound collections');
      Check((LOrders.ItemAt(0).Ref.ID = 'order-1') and
        (LOrders.ItemAt(1).GetValue(NyxTextField('customer')) = 'Grace') and
        (LOrders.ItemAt(2).GetValue(NyxTextField('customer')) = 'Ada'),
        'Stable first customer and left order are retained');
      Check((LRegions.ItemAt(0).Ref.ID = 'West') and
        (LRegions.ItemAt(0).GetValue(NyxIntegerField('count')) = 2) and
        (LRegions.ItemAt(1).Ref.ID = 'East'), 'Grouped rows retain first-region order and real counts');
      Check(GApp.State.GetValue(NyxTextState('total')) = '9007199254740993.3',
        'Exact total excludes unmatched orders and exceeds IEEE integer precision');
      Check((Cell('orders-table', 3, 1, 0) = 'Ada') and
        (Cell('orders-table', 3, 2, 2) = '9007199254740993') and
        (Cell('regions-table', 2, 0, 0) = 'West') and (Cell('regions-table', 2, 1, 0) = '2'),
        'Actual ordinary Tables display both transformed outputs and exact values');
      Check((GOther.Collections.Collection(NyxCollection('orders')).Snapshot.Count = 0) and
        (GOther.Collections.Collection(NyxCollection('regions')).Snapshot.Count = 0),
        'Independent application instances retain independent live collections');
      Input(StringReplace(RelationCustomers, '"Grace"', '"Grace Hopper"', [rfReplaceAll]));
      Check(Pos('Grace Hopper', GApp.State.GetValue(NyxTextState('customers'))) > 0,
        'Actual memo input reaches accepted runtime state');
      Click;
    end;
    2:
    begin

      if not Idle then
      begin
        Exit;
      end;
      Check(Cell('orders-table', 3, 1, 1) = 'Grace Hopper', 'New dataset updates the joined bound Table');
      SaveFrame;
      Input('[{"id":1,"name":"Wrong key family","region":"West"}]');
      Click;
    end;
    3:
    begin

      if not Idle then
      begin
        Exit;
      end;
      Check((GApp.FlowRoutes.Report(0).Field('failed').AsInteger = 1) and
        (GApp.FlowRoutes.Report(0).Field('failure').AsText <> ''), 'Wrong key family is inspectable');
      RetainedFrame;
      Input(RelationCustomers);
      Click;
      GApp.FlowRoutes.Cancel(0);
    end;
    4:
    begin

      if not Idle then
      begin
        Exit;
      end;
      RetainedFrame;
      Click;
      GApp.ShowPage('other');
    end;
    5:
    begin
      RetainedFrame;
      GApp.ShowPage('home');
      Click;
    end;
    6:
    begin

      if not Idle then
      begin
        Exit;
      end;
      Check((Cell('orders-table', 3, 1, 1) = 'Grace') and
        (GApp.State.GetValue(NyxTextState('total')) = '9007199254740993.3'),
        'Fresh generation rebuilds useful rows and exact total');
      Check(TNyxCodec.Encode(GDocument) = GBefore, 'Execution never changes saved graph/defaults/source meaning');
      {$ifdef PAS2JS}
      GApp.View.ElementFor('result-title').scrollIntoView(True);
      document.body.setAttribute('data-capture-checkpoint', 'sales-report');
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

      if document.body.getAttribute('data-capture-observed') <> 'sales-report' then
      begin
        Exit;
      end;
      {$endif}
      GRun := TNyxFlowApplication.Submit(GApp.Context,
        GDocument.Flows.Definition(NyxFlowRef('Sales')).Build, [
          NyxFlowInput(RelationEndpoint('orders', nlpText), NyxFlowText(RelationOrders)),
          NyxFlowInput(RelationEndpoint('customers', nlpText), NyxFlowText(
            StringReplace(RelationCustomers, '"Grace"', '"Worker Grace"', [rfReplaceAll])))], [
          NyxFlowAssign(NyxTextState('total'), RelationEndpoint('format-total', nlpText))], [
          NyxFlowAssignRows(NyxCollection('orders'), RelationEndpoint('join', nlpValue), RelationOrderRows),
          NyxFlowAssignRows(NyxCollection('regions'), RelationEndpoint('groups', nlpValue), RelationGroupRows)],
          {$ifdef PAS2JS}neAsynchronous{$else}neThreaded{$endif});
    end;
    8:
    begin

      if GRun.Phase in [nqpPending, nqpComputing, nqpAwaitingUI] then
      begin
        Exit;
      end;
      Check((GRun.Phase = nqpSucceeded) and (Cell('orders-table', 3, 1, 1) = 'Worker Grace') and
        (GApp.State.GetValue(NyxTextState('total')) = '9007199254740993.3'),
        'Native worker/browser event-loop relations publish real rows and exact scalar on UI');
      SaveFrame;
      GRun := TNyxFlowApplication.Submit(GApp.Context,
        GDocument.Flows.Definition(NyxFlowRef('Sales')).Build, [
          NyxFlowInput(RelationEndpoint('orders', nlpText), NyxFlowText(RelationOrders)),
          NyxFlowInput(RelationEndpoint('customers', nlpText), NyxFlowText(RelationCustomers))], [
          NyxFlowAssign(NyxTextState('total'), RelationEndpoint('format-total', nlpText))], [
          NyxFlowAssignRows(NyxCollection('orders'), RelationEndpoint('join', nlpValue), RelationOrderRows)],
          {$ifdef PAS2JS}neAsynchronous{$else}neThreaded{$endif});
      GRun.Cancel;
    end;
    9:
    begin

      if GRun.Phase in [nqpPending, nqpComputing, nqpAwaitingUI] then
      begin
        Exit;
      end;
      RetainedFrame;
      Check(TNyxCodec.Encode(GDocument) = GBefore, 'Worker execution retains the exact saved document');
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
  GOrdersBefore := nil;
  GRegionsBefore := nil;
  FreeAndNil(GDocument);
end;

{$ifdef PAS2JS}
procedure Drive;
begin
  try

    if TJSDate.now - GStarted > 60000 then
    begin
      raise Exception.Create('Relation application deadline at stage ' + IntToStr(GStage));
    end;
    Step;

    if GFinished then
    begin
      document.body.setAttribute('data-relation-controls', 'passed');
      document.body.setAttribute('data-relation-controls-checks', IntToStr(GChecks));
      Retire;
      Exit;
    end;
    window.setTimeout(@Drive, 30);
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-relation-controls', 'failed');
      document.body.setAttribute('data-relation-controls-error', E.Message);
      Retire;
    end;
  end;
end;
{$endif}

begin
  try
    GDocument := BuildNyxDocument;
    GExpected := RelationDocument;
    try
      Check(TNyxCodec.Encode(GDocument) = TNyxCodec.Encode(GExpected),
        'Compiled semantic-authored source reconstructs the exact complete application');
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
        raise Exception.Create('Native relation application deadline at stage ' + IntToStr(GStage));
      end;
      Sleep(1);
    end;
    Retire;
    WriteLn('PASS ', GChecks, ' compiled relational Table application checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-relation-controls', 'failed');
      document.body.setAttribute('data-relation-controls-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
      Retire;
    end;
  end;
end.
