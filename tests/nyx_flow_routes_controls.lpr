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


program nyx_flow_routes_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.state, nyx.model, nyx.codec,
  nyx.events, nyx.behavior, nyx.scheduler, nyx.flow.routing,
  nyx.flow.routes, nyx.flow.route.model, nyx.flow.types, nyx.flow.declarations,
  nyx.flow, nyx.logic, nyx.controls, nyx.test.logic.fixture, nyx.test.flow.routes
  {$ifdef NYX_ROUTED_COMPANION}, nyx.generated.view{$endif}
  {$ifdef PAS2JS}, JS, Web, nyx.application.browser
  {$else}, Interfaces, Classes, Forms, Controls, StdCtrls, Graphics,
  nyx.widgets.lcl, nyx.application.lcl{$endif};

type
  TOrdinary = class(TNyxEventCallback)
  public
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;
  TRetiringResolver = class(TInterfacedObject, INyxLogicResolver)
  public
    function Resolve(const AKind: TNyxLogicKindRef;
      const AConfiguration: TNyxDataValue): INyxLogicOperation;
  end;

var
  GDocument: TNyxDocument;
  GOtherDocument: TNyxDocument;
  GBefore: TNyxText;
  GChecks: Integer;
  GStage: Integer;
  GCallbacks: Integer;
  GFinished: Boolean;
  GRevision: Integer;
  GBindings: INyxFlowRouteBinding;
  GOtherBindings: INyxFlowRouteBinding;
  GCallback: INyxEventSubscription;
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

procedure Check(AValue: Boolean; const AMessage: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Actual saved routes: ' + AMessage);
  end;
  Inc(GChecks);
end;

function RetiringDescriptor: TNyxLogicDescriptor;
begin
  Result := NyxLogicDescriptor(NyxLogicKind('retire-owner'), 'Retire owner',
    'Qualification of resolver lifetime and application retirement.', nlgFlow)
    .Output(NyxFlowPort('text'), nftText, 'Must never publish');
end;

function TRetiringResolver.Resolve(const AKind: TNyxLogicKindRef;
  const AConfiguration: TNyxDataValue): INyxLogicOperation;
begin

  if AKind.Name <> 'retire-owner' then
  begin
    Exit(NyxLogicFromData(AKind, AConfiguration));
  end;
  FreeAndNil(GOther);
  Result := NewNyxLogicDeclaration(RetiringDescriptor, AConfiguration);
end;

procedure TOrdinary.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin
  Inc(GCallbacks);
end;

procedure Click;
begin
  {$ifdef PAS2JS}
  TJSHTMLButtonElement(GApp.View.ElementFor('run-button')).click;
  {$else}
  TNyxLCLButton(GApp.View.ControlFor('run-button')).Click;
  {$endif}
end;

procedure Edit(const AValue: TNyxText);
begin
  {$ifdef PAS2JS}
  TJSHTMLTextAreaElement(GApp.View.InputFor('source-memo')).value := AValue;
  GApp.View.InputFor('source-memo').dispatchEvent(TJSEvent.new('input'));
  {$else}
  TMemo(GApp.View.InputFor('source-memo')).Text := AValue;
  {$endif}
end;

function Idle: Boolean;
var
  LIndex: Integer;
begin
  Result := True;
  for LIndex := 0 to GApp.FlowRoutes.Count - 1 do
  begin
    Result := Result and (GApp.FlowRoutes.Report(LIndex).Field('active').AsInteger = 0);
  end;
end;

procedure ClickOther;
begin
  {$ifdef PAS2JS}
  TJSHTMLButtonElement(GOther.View.ElementFor('run-button')).click;
  {$else}
  TNyxLCLButton(GOther.View.ControlFor('run-button')).Click;
  {$endif}
end;

procedure Step;
var
  LBefore: TNyxText;
  {$ifndef PAS2JS}
  LBitmap: TBitmap;
  LPNG: TPortableNetworkGraphic;
  {$endif}
begin
  case GStage of
    0:
      begin
        Check(GApp.FlowRoutes.Count >= 2, 'ordinary mount attaches saved field/button routes without callback glue');
        GRevision := GApp.State.Revision;
        Click;
      end;
    1:
      begin

        if not Idle then
        begin
          Exit;
        end;
        Check(GApp.State.Revision = GRevision + 1, 'button outputs publish once as an atomic state revision');
        Check(Pos('English demo', GApp.State.GetValue(NyxTextState('pretty'))) > 0,
          'actual button executes saved JSON chain');
        Check(GOther.State.GetValue(NyxTextState('pretty')) <> GApp.State.GetValue(NyxTextState('pretty')),
          'independent applications retain independent state');
        {$ifdef PAS2JS}
        Check(Pos('English demo', GApp.View.ElementFor('pretty-code').textContent) > 0,
          'browser code output displays bound graph result');
        {$else}
        Check(Pos('English demo', TNyxText(TMemo(GApp.View.ControlFor('pretty-code')).Text)) > 0,
          'LCL code output displays bound graph result');
        {$endif}
        Edit('{"profile":{"name":"Live route","exact":9007199254740993}}');
      end;
    2:
      begin

        if not Idle then
        begin
          Exit;
        end;
        Check(Pos('Live route', GApp.State.GetValue(NyxTextState('pretty'))) > 0,
          'physical memo input supplies accepted event value to graph');
        Check(Pos('9007199254740993', GApp.State.GetValue(NyxTextState('pretty'))) > 0,
          'exact JSON number survives actual routed input/output');
        Check(Pos('Live profile', GApp.State.GetValue(NyxTextState('summary'))) = 1,
          'field event uses its own constant mapping');
        GCallback := GApp.View.Events.On(NyxControlEvents('run-button'), ntClick).Subscribe(TOrdinary.Create);
        GRevision := GApp.State.Revision;
        Click;
        Check(GCallbacks = 1, 'ordinary callback still executes with exclusive route refusal');
        Check((GApp.State.Revision = GRevision) and
          (Pos('Exclusive', GApp.FlowRoutes.Report(1).Field('failure').AsText) > 0),
          'late callback registration cannot silently duplicate exclusive route behavior');
        GCallback.Cancel;
        Edit('{ invalid JSON');
      end;
    3:
      begin

        if not Idle then
        begin
          Exit;
        end;
        Check(Pos('Live route', GApp.State.GetValue(NyxTextState('pretty'))) > 0,
          'failed operation retains all previous outputs');
        Check(GApp.FlowRoutes.Report(0).Field('failure').AsText <> '',
          'operation failure remains inspectable');
        Edit('{"profile":{"name":"Superseded"}}');
        Edit('{"profile":{"name":"Latest route"}}');
      end;
    4:
      begin

        if not Idle then
        begin
          Exit;
        end;
        Check(Pos('Latest route', GApp.State.GetValue(NyxTextState('pretty'))) > 0,
          'latest policy publishes only the newest captured input');
        Click;
        GRevision := GApp.State.Revision;
        GApp.State.SetValue(NyxTextState('guard'), 'Intervening user edit');
      end;
    5:
      begin

        if not Idle then
        begin
          Exit;
        end;
        Check(GApp.State.Revision = GRevision + 1, 'stale graph never overwrites an intervening state revision');
        Edit('{"profile":{"name":"Retired view"}}');
        LBefore := GApp.State.GetValue(NyxTextState('pretty'));
        GApp.ShowPage('other');
        Check(GApp.State.GetValue(NyxTextState('pretty')) = LBefore, 'navigation retains accepted output');
      end;
    6:
      begin

        if not Idle then
        begin
          Exit;
        end;
        Check(Pos('Retired view', GApp.State.GetValue(NyxTextState('pretty'))) = 0,
          'retired generation cannot deliver queued output');
        GApp.ShowPage('home');
        Edit('{"profile":{"name":"Fresh generation"}}');
      end;
    7:
      begin

        if not Idle then
        begin
          Exit;
        end;
        Check(Pos('Fresh generation', GApp.State.GetValue(NyxTextState('pretty'))) > 0,
          'same saved attachment works after navigation to a fresh generation');
        Check(TNyxCodec.Encode(GDocument) = GBefore, 'runtime routes never mutate saved declarations/defaults');
        GBindings := GApp.FlowRoutes;
        GBindings.Close;
        GRevision := GApp.State.Revision;
        Click;
        Check((GApp.State.Revision = GRevision) and GBindings.Report(0).Field('closed').AsBoolean,
          'explicit close revokes all routes without borrowing retired controls');
        {$ifndef PAS2JS}

        if ParamCount > 0 then
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
        ClickOther;
        ClickOther;
        Check((GOther.FlowRoutes.Report(1).Field('started').AsInteger = 1) and
          (Pos('capacity', GOther.FlowRoutes.Report(1).Field('failure').AsText) > 0),
          'Every has finite explicit capacity and refuses overload');
        Check((GOther.FlowRoutes.Report(2).Field('started').AsInteger = 1) and
          (GOther.FlowRoutes.Report(2).Field('skipped').AsInteger = 1),
          'IgnoreWhileRunning skips a second event on a no-output action route');
      end;
    8:
      begin

        if (GOther.FlowRoutes.Report(1).Field('active').AsInteger <> 0) or
          (GOther.FlowRoutes.Report(2).Field('active').AsInteger <> 0) then
        begin
          Exit;
        end;
        Check(Pos('English demo', GOther.State.GetValue(NyxTextState('pretty'))) > 0,
          'bounded Every run still publishes its first exact inputs');
        Check(GOther.FlowRoutes.Report(2).Field('failure').AsText = '',
          'no-output action route completes through generation-guarded publication');
        ClickOther;
        GOther.FlowRoutes.Cancel(1);
        GOther.FlowRoutes.Cancel(2);
        Check((GOther.FlowRoutes.Report(1).Field('active').AsInteger = 0) and
          (GOther.FlowRoutes.Report(2).Field('active').AsInteger = 0),
          'explicit cancellation retires exact owned runs');
        GOtherBindings := GOther.FlowRoutes;
        {$ifdef PAS2JS}
        TJSHTMLButtonElement(GOther.View.ElementFor('retire-button')).click;
        {$else}
        TNyxLCLButton(GOther.View.ControlFor('retire-button')).Click;
        {$endif}
        Check((GOther = nil) and GOtherBindings.Report(3).Field('closed').AsBoolean,
          'creator resolution may retire the application without retaining or dereferencing its host');
        Check(Pos('owner changed', GOtherBindings.Report(3).Field('failure').AsText) > 0,
          'retired resolver invocation refuses before any state capture or work submission');
      end;
    9:
      begin
        { Retired native couriers and browser promises release their captured
          graph/inputs on subsequent real UI turns, not during Cancel itself. }
      end;
    10:
      begin
        GFinished := True;
      end;
  end;
  Inc(GStage);
end;

procedure Retire;
begin
  FreeAndNil(GApp);
  FreeAndNil(GOther);
  GCallback := nil;
  GBindings := nil;
  GOtherBindings := nil;
  FreeAndNil(GDocument);
  FreeAndNil(GOtherDocument);
end;

{$ifdef PAS2JS}
procedure Drive;
begin
  try

    if TJSDate.now - GStarted > 30000 then
    begin
      raise Exception.Create('Routed application deadline at stage ' + IntToStr(GStage));
    end;
    Step;

    if GFinished then
    begin
      document.body.setAttribute('data-flow-route-controls', 'passed');
      document.body.setAttribute('data-flow-route-controls-checks', IntToStr(GChecks));
      Exit;
    end;
    window.setTimeout(@Drive, 20);
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-flow-route-controls', 'failed');
      document.body.setAttribute('data-flow-route-controls-error', E.Message);
      Retire;
    end;
  end;
end;
{$endif}

begin
  try
    {$ifdef NYX_ROUTED_COMPANION}
    GDocument := BuildNyxDocument;
    {$else}
    GDocument := RoutedLogicDocument;
    {$endif}
    GOtherDocument := RoutedLogicDocument;
    SetNyxFlowRoutes(GOtherDocument.Find('run-button'), NewNyxFlowRoutes
      .WithRoute(ProfileRoute(nfcmAfterDispatch).Concurrent(nfrEvery).MaximumRuns(1))
      .WithRoute(NyxFlowRoute(NyxFlowRouteID('Action only'), NyxFlowRef('Profile'),
        ntClick, nfcmBeforeCallbacks)
        .InputState(LogicEndpoint('parse', nlpText), NyxTextState('source'))
        .InputConstant(LogicEndpoint('summary', nlpLeft), NyxFlowText('No output'))
        .Concurrent(nfrIgnoreWhileRunning)));
    GOtherDocument.Flows.Define(NyxFlowRef('Retire'), NewNyxFlowGraph.WithStep(
      NyxFlowStep('retire'), NewNyxLogicDeclaration(RetiringDescriptor, NyxObject([]))).Build);
    GOtherDocument.Pages[0].Add(NewNyxButton('retire-button').WithText('Retire test host'));
    SetNyxFlowRoutes(GOtherDocument.Find('retire-button'), NewNyxFlowRoutes.WithRoute(
      NyxFlowRoute(NyxFlowRouteID('Retire safely'), NyxFlowRef('Retire'), ntClick, nfcmExclusive)
        .OutputState(NyxFlowEndpoint(NyxFlowStep('retire'), NyxFlowPort('text')), NyxTextState('pretty'))));
    GBefore := TNyxCodec.Encode(GDocument);
    {$ifdef PAS2JS}
    GHost := TJSHTMLElement(document.createElement('div'));
    GOtherHost := TJSHTMLElement(document.createElement('div'));
    document.body.appendChild(GHost);
    document.body.appendChild(GOtherHost);
    GOtherHost.style.setProperty('display', 'none');
    GApp := TNyxBrowserApplication.Create;
    GOther := TNyxBrowserApplication.Create;
    GOther.ConfigureLogic(TRetiringResolver.Create);
    GApp.Run(GDocument, GHost);
    GOther.Run(GOtherDocument, GOtherHost);
    GStarted := TJSDate.now;
    Drive;
    {$else}
    Application.Initialize;
    GApp := TNyxLCLApplication.Create;
    GOther := TNyxLCLApplication.Create;
    GOther.ConfigureLogic(TRetiringResolver.Create);
    GApp.Mount(GDocument);
    GOther.Mount(GOtherDocument);
    GApp.Window.Show;
    GStarted := GetTickCount64;
    while not GFinished do
    begin
      CheckSynchronize;
      Application.ProcessMessages;

      if GetTickCount64 - GStarted > 30000 then
      begin
        raise Exception.Create('Routed application deadline at stage ' + IntToStr(GStage));
      end;
      Step;
      Sleep(1);
    end;
    Retire;
    WriteLn('PASS ', GChecks, ' actual saved route application checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-flow-route-controls', 'failed');
      document.body.setAttribute('data-flow-route-controls-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
      Retire;
    end;
  end;
end.
