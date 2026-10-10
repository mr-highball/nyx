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
program nyx_runtime_recovery_panel_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifdef PAS2JS}JS, Web, nyx.studio.browser,
  {$else}Interfaces, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls, nyx.test.capture.lcl,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls, nyx.events, nyx.behavior,
  nyx.theme, nyx.operation.panel, nyx.view.recovery, nyx.studio.recovery.status,
  nyx.studio.session, nyx.studio.projects, nyx.studio.view, nyx.studio.section.views;

type
  { Borrows only the current mounted lookup forest during synchronous delivery.
    Records actual typed control actions; it supplies no compiler or fake worker. }
  TPanelScenario = class
  public
    Views: TNyxStudioSectionViews;
    Calls: Integer;
    DisplayRetries: Integer;
    LastAction: TNyxOperationAction;
    procedure Changed(ANode: TNyxNode; const AEvent: TNyxEventInfo);
  end;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxModel.Create('Recovery panel qualification: ' + AReason);
  end;
  Inc(GChecks);
  WriteLn('PASS ', GChecks, ' / ', AReason);
end;

procedure TPanelScenario.Changed(ANode: TNyxNode; const AEvent: TNyxEventInfo);
var
  LAction: TNyxOperationAction;
begin

  if NyxViewRecoveryAction(ANode, AEvent, NyxStudioDisplayRecoveryID) and
    (Views.Root.Find(ANode.ID) = ANode) then
  begin
    Inc(DisplayRetries);
  end;

  if (AEvent.Trigger = ntClick) and
    NyxOperationPanelAction(ANode, AEvent, NyxStudioRuntimeRecoveryID, LAction) and
    (Views.Root.Find(ANode.ID) = ANode) then
  begin
    Inc(Calls);
    LastAction := LAction;
  end;
end;

function Wire(const APhase: TNyxText; APending: Boolean;
  AAccepted, AUnits, ASessions: Integer): TNyxDataValue;
begin
  Result := NyxObject([NyxField('state', NyxData(APhase)),
    NyxField('pending', NyxData(APending)), NyxField('accepted', NyxData(AAccepted)),
    NyxField('units', NyxData(AUnits)), NyxField('sessions', NyxData(ASessions))]);
end;

procedure Refuse(const AValue: TNyxDataValue; const AReason: TNyxText);
var
  LRefused: Boolean;
  LStatus: TNyxRuntimeRecoveryStatus;
begin
  LRefused := False;
  try
    LStatus := DecodeNyxRuntimeRecoveryStatus(AValue);
    { A legitimate return is deliberately observed; it is never a refusal. }
    LRefused := LStatus.Sessions < 0;
  except
    on ENyxModel do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, AReason);
end;

procedure ContractChecks;
var
  LStatus: TNyxRuntimeRecoveryStatus;
  LPanel: INyxPanel;
  LState: TNyxOperationPresentation;
  LBefore: TNyxText;
  LRefused: Boolean;
  LAction: TNyxOperationAction;
begin
  LStatus := DecodeNyxRuntimeRecoveryStatus(Wire('awaiting-execution', True, 0, 459, 9));
  Check(LStatus.Pending and (LStatus.Units = 459) and (LStatus.Sessions = 9),
    'readiness admits the complete bounded registry without reading source');
  LStatus := DecodeNyxRuntimeRecoveryStatus(Wire('published', False, 2, 2, 9));
  Check((LStatus.Phase = nrpPublished) and not LStatus.Pending,
    'published readiness requires every unit');
  LStatus := DecodeNyxRuntimeRecoveryStatus(Wire('cancelled', True, 1, 2, 9));
  Check(LStatus.Pending and (LStatus.Phase = nrpCancelled),
    'cancelled readiness retains the guarded input');
  Refuse(Wire('unknown', True, 0, 2, 1), 'unknown phase refuses');
  Refuse(Wire('not-required', True, 0, 0, 0), 'missing checkpoint cannot be pending');
  Refuse(Wire('awaiting-execution', False, 0, 2, 1), 'awaiting input cannot advertise readiness');
  Refuse(Wire('published', False, 1, 2, 1), 'partial units cannot advertise publication');
  Refuse(Wire('published', True, 2, 2, 1), 'published owners cannot remain pending');
  Refuse(Wire('cancelled', True, 0, 460, 9), 'oversized source registry refuses');
  Refuse(Wire('cancelled', True, -1, 2, 1), 'negative admission count refuses');

  LState := TNyxOperationPresentation.New('Restore shared projects', nopWaiting,
    'Review compiler settings before executing saved Pascal.')
    .Progress(0, 2).Actions([noaStart, noaConfigure]);
  LPanel := NewNyxOperationPanel('contract-operation', LState);
  Check(not NyxOperationPanelAction(LPanel.Node.Find(
    NyxOperationActionID('contract-operation', noaCancel)), 'contract-operation', LAction),
    'hidden action cannot become a command');
  LPanel.Node.Find('contract-operation-actions').Configure.Visible(False).Done;
  Check(not NyxOperationPanelAction(LPanel.Node.Find(
    NyxOperationActionID('contract-operation', noaStart)), 'contract-operation', LAction),
    'creator-hidden action group cannot dispatch a command');
  LPanel.Node.Find('contract-operation-actions').Configure.Visible(True).Done;
  LPanel.Node.Add(NewNyxBadge('creator-extension').WithText('Creator supplied detail'));
  LBefore := LPanel.Node.Find('contract-operation-title').Prop('text');
  LPanel.Node.Find('contract-operation-actions').Remove(
    LPanel.Node.Find(NyxOperationActionID('contract-operation', noaRetry)));
  LRefused := False;
  try
    RestoreNyxOperationPanel(LPanel.Node,
      TNyxOperationPresentation.New('Must not replace title', nopRunning, ''));
  except
    on ENyxModel do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LPanel.Node.Find('contract-operation-title').Prop('text') = LBefore) and
    (LPanel.Node.Find('creator-extension') <> nil),
    'malformed fixed parts refuse before changing presentation or extensions');
end;

{$ifdef PAS2JS}
procedure Pause(AResolve, AReject: TJSPromiseResolver);
begin
  window.setTimeout(
    procedure
    begin
      AResolve(True);
    end, 15);
end;
{$endif}

procedure Idle(AViews: TNyxStudioSectionViews); {$ifdef PAS2JS}async;{$endif}
var
  LTurns: Integer;
begin
  LTurns := 0;
  repeat
    {$ifdef PAS2JS}
    await(TJSPromise.resolve(TJSPromise.new(@Pause)));
    {$else}
    Application.ProcessMessages;
    Sleep(10);
    {$endif}
    Inc(LTurns);

    if LTurns > 2000 then
    begin
      raise ENyxModel.Create('Recovery panel input did not settle');
    end;
  until not AViews.Dispatching;
end;

procedure Run; {$ifdef PAS2JS}async;{$endif}
var
  LSession: TNyxStudioSession;
  LViews: TNyxStudioSectionViews;
  LScenario: TPanelScenario;
  LShell: TNyxDocument;
  LNext: TNyxDocument;
  LTheme: TNyxTheme;
  LState: TNyxStudioViewState;
  LPair: TNyxText;
  LForeign: INyxPanel;
  LDispatch: TNyxDispatch;
  {$ifdef PAS2JS}
  LHost: TJSHTMLElement;
  LStyle: TJSHTMLElement;
  {$else}
  LHost: TPanel;
  LWindow: TForm;
  {$endif}
begin
  ContractChecks;
  LSession := TNyxStudioSession.Create;
  LTheme := TNyxTheme.Create;
  LViews := TNyxStudioSectionViews.Create(LTheme);
  LScenario := TPanelScenario.Create;
  LScenario.Views := LViews;
  LViews.OnEvent := LScenario.Changed;
  LShell := nil;
  LNext := nil;
  {$ifdef PAS2JS}
  LStyle := TJSHTMLElement(document.createElement('style'));
  LStyle.textContent := NyxStudioBrowserCSS;
  document.head.appendChild(LStyle);
  LHost := TJSHTMLElement(document.createElement('main'));
  document.body.appendChild(LHost);
  {$else}
  Application.Initialize;
  LWindow := TForm.CreateNew(nil);
  LWindow.Caption := 'Nyx shared project recovery';
  LWindow.SetBounds(20, 20, 1240, 820);
  LHost := TPanel.Create(LWindow);
  LHost.BevelOuter := bvNone;
  LHost.Parent := LWindow;
  LHost.Align := alClient;
  LWindow.Show;
  {$endif}
  try
    LPair := EncodeNyxProject(LSession.ProjectSnapshot);
    LState := DefaultNyxStudioViewState;
    LState.DisplayRecovery := TNyxViewRecovery.Failed('The accepted display needs refreshing.');
    LState.RuntimeRecovery := TNyxOperationPresentation.New('Shared projects waiting',
      nopWaiting, 'Configure the browser compiler, then recover saved projects and history. ' +
      TNyxText('Starting recovery executes their accepted Pascal constructors. Local designing remains available.'))
      .Progress(0, 2).Actions([noaStart, noaCancel, noaConfigure, noaCheck]);
    LShell := BuildNyxStudioView(LSession, LState);
    {$ifndef PAS2JS}
    { Match ordinary native Studio's logical shell allocation. Compilation alone
      does not establish a host extent; browser uses its viewport sizing adapter. }
    LShell.Pages[0].Configure.Height(LHost.ClientHeight).Done;
    {$endif}
    LViews.Render(LShell, LShell.Pages[0], LHost);
    {$ifdef PAS2JS}await(Idle(LViews));{$else}Idle(LViews);{$endif}
    {$ifdef PAS2JS}
    LViews.ControlFor(NyxOperationActionID(NyxStudioRuntimeRecoveryID, noaStart)).click;
    {$else}
    TButton(LViews.ControlFor(NyxOperationActionID(NyxStudioRuntimeRecoveryID, noaStart))).Click;
    {$endif}
    {$ifdef PAS2JS}await(Idle(LViews));{$else}Idle(LViews);{$endif}
    Check((LScenario.Calls = 1) and (LScenario.LastAction = noaStart),
      'actual Nyx button delivers the typed Start action');
    {$ifdef PAS2JS}
    LViews.ControlFor(NyxViewRecoveryRetryID(NyxStudioDisplayRecoveryID)).click;
    {$else}
    TButton(LViews.ControlFor(NyxViewRecoveryRetryID(NyxStudioDisplayRecoveryID))).Click;
    {$endif}
    {$ifdef PAS2JS}await(Idle(LViews));{$else}Idle(LViews);{$endif}
    Check(LScenario.DisplayRetries = 1,
      'existing display-recovery compound also resolves the actual clicked retry part');
    LForeign := NewNyxOperationPanel(NyxStudioRuntimeRecoveryID, LState.RuntimeRecovery);
    LDispatch := DispatchNyxBehavior(LForeign.Node.Find(NyxOperationActionID(
      NyxStudioRuntimeRecoveryID, noaStart)), ntClick);
    LScenario.Changed(LDispatch.Source, LDispatch.Info);
    Check(LScenario.Calls = 1, 'same-ID foreign compound cannot command the mounted owner');

    LState.OutputVisible := True;
    LState.DisplayRecovery := TNyxViewRecovery.Ready;
    LState.RecoveryCompilerConfiguration := True;
    LNext := BuildNyxStudioView(LSession, LState);
    {$ifndef PAS2JS}
    LNext.Pages[0].Configure.Height(LHost.ClientHeight).Done;
    {$endif}
    LViews.Render(LNext, LNext.Pages[0], LHost);
    {$ifdef PAS2JS}await(Idle(LViews));{$else}Idle(LViews);{$endif}
    Check((LViews.Root.Find('output-pas2js') <> nil) and (LState.OutputTarget = '') and
      (EncodeNyxProject(LSession.ProjectSnapshot) = LPair),
      'browser compiler configuration is reachable without selecting an output or editing a pair');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-recovery-panel-desktop', 'ready');
    {$else}
    LWindow.Repaint;
    SaveNyxNativeCapture(LWindow, IncludeTrailingPathDelimiter(ParamStr(1)) +
      'native-desktop.png', ncmPrint);
    {$endif}

    LNext.Free;
    LNext := nil;
    LState.Compact := True;
    LState.DetailsExpanded := False;
    LState.OutputVisible := False;
    LState.RuntimeRecovery := TNyxOperationPresentation.New('Recovering shared projects',
      nopRunning, 'Executing saved Pascal. Projects: 9').Progress(1, 2).Actions([noaCancel]);
    LNext := BuildNyxStudioView(LSession, LState);
    {$ifdef PAS2JS}
    LHost.setAttribute('data-nyx-studio-compact', 'true');
    {$else}
    LWindow.SetBounds(20, 20, 420, 820);
    LNext.Pages[0].Configure.Height(LHost.ClientHeight).Done;
    {$endif}
    LViews.Render(LNext, LNext.Pages[0], LHost);
    {$ifdef PAS2JS}await(Idle(LViews));{$else}Idle(LViews);{$endif}
    Check(LViews.Root.Find('studio-details-label').Prop('text') = 'Recovering shared projects',
      'collapsed compact details still surface recovery progress');
    Check((EncodeNyxProject(LSession.ProjectSnapshot) = LPair) and not LSession.CanUndo,
      'progress and form-factor changes do not create project history');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-recovery-panel-compact', 'ready');
    document.body.setAttribute('data-recovery-panel-checks', IntToStr(GChecks));
    {$else}
    LWindow.Repaint;
    SaveNyxNativeCapture(LWindow, IncludeTrailingPathDelimiter(ParamStr(1)) +
      'native-compact.png', ncmPrint);
    {$endif}
    LNext.Free;
    LNext := nil;
    LState.DetailsExpanded := True;
    LState.DetailsPercent := 42;
    LNext := BuildNyxStudioView(LSession, LState);
    {$ifndef PAS2JS}
    LNext.Pages[0].Configure.Height(LHost.ClientHeight).Done;
    {$endif}
    LViews.Render(LNext, LNext.Pages[0], LHost);
    {$ifdef PAS2JS}await(Idle(LViews));{$else}Idle(LViews);{$endif}
    {$ifdef PAS2JS}
    Check((TJSHTMLProgressElement(LViews.ControlFor(NyxStudioRuntimeRecoveryID +
      TNyxText('-progress'))).value = 1) and
      (TJSHTMLProgressElement(LViews.ControlFor(NyxStudioRuntimeRecoveryID +
      TNyxText('-progress'))).max = 2), 'actual progress control shows completed source units');
    LViews.ControlFor(NyxOperationActionID(NyxStudioRuntimeRecoveryID, noaCancel)).click;
    {$else}
    Check((TProgressBar(LViews.ControlFor(NyxStudioRuntimeRecoveryID +
      TNyxText('-progress'))).Position = 1) and
      (TProgressBar(LViews.ControlFor(NyxStudioRuntimeRecoveryID +
      TNyxText('-progress'))).Max = 2), 'actual progress control shows completed source units');
    TButton(LViews.ControlFor(NyxOperationActionID(NyxStudioRuntimeRecoveryID, noaCancel))).Click;
    {$endif}
    {$ifdef PAS2JS}await(Idle(LViews));{$else}Idle(LViews);{$endif}
    Check((LScenario.Calls = 2) and (LScenario.LastAction = noaCancel),
      'expanded compact panel delivers the typed Cancel action');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-recovery-panel-expanded', 'ready');
    document.body.setAttribute('data-recovery-panel-checks', IntToStr(GChecks));
    document.body.setAttribute('data-test-result', 'passed');
    {$else}
    LWindow.Repaint;
    SaveNyxNativeCapture(LWindow, IncludeTrailingPathDelimiter(ParamStr(1)) +
      'native-compact-expanded.png', ncmPrint);
    {$endif}
    WriteLn('PASS ', GChecks, ' shared recovery status and actual panel checks');
  finally
    LScenario.Views := nil;
    LViews.Free;
    LScenario.Free;
    LNext.Free;
    LShell.Free;
    LSession.Free;
    LTheme.Free;
    {$ifdef PAS2JS}
    LHost.remove;
    LStyle.remove;
    {$else}
    LWindow.Free;
    {$endif}
  end;
end;

{$ifdef PAS2JS}
procedure Start; async;
begin
  try
    await(Run);
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-test-result', 'failed');
      document.body.setAttribute('data-recovery-panel-error', LException.Message);
    end;
  end;
end;
{$endif}

begin
  {$ifdef PAS2JS}Start;{$else}
  try

    if ParamCount <> 1 then
    begin
      raise ENyxModel.Create('Supply one owned recovery-panel evidence directory');
    end;
    ForceDirectories(ParamStr(1));
    Run;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
  {$endif}
end.
