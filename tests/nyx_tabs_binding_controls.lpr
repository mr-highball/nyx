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

program nyx_tabs_binding_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.tabs,
  nyx.state, nyx.behavior, nyx.binding.types, nyx.codec, nyx.codegen, nyx.source
  {$ifdef PAS2JS}, JS, Web, nyx.render.browser
  {$else}, Interfaces, Forms, Controls, StdCtrls, nyx.render.lcl, nyx.tabs.lcl{$endif};

type
  { The probe borrows the mounted renderer only during Run. Validators verify
    that physical presentation still shows the old page before admission. }
  TTabBindingJourney = class
  private
    FDocument: TNyxDocument;
    FChecks: Integer;
    FCalls: Integer;
    FReject: Boolean;
    FObservedBefore: Boolean;
    FFailNotification: Boolean;
    FSubscription: TNyxStateSubscription;
    {$ifdef PAS2JS}
    FRenderer: TNyxBrowserRenderer;
    FHost: TJSHTMLElement;
    {$else}
    FRenderer: TNyxLCLRenderer;
    FHost: TForm;
    {$endif}
    procedure Check(ACondition: Boolean; const AReason: TNyxText);
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure Validate(ACandidate: TNyxState; AChanges: TNyxStateChanges);
    procedure Observe(AState: TNyxState; AChanges: TNyxStateChanges);
    procedure Click(const AInstance, ATab: TNyxText);
    function Selected(const AInstance, ATab: TNyxText): Boolean;
  public
    procedure Run;
    destructor Destroy; override;
  end;
  {$ifndef PAS2JS}
  TButtonAccess = class(TButton);
  {$endif}

procedure TTabBindingJourney.Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Tab binding: ' + AReason);
  end;
  Inc(FChecks);
end;

function TTabBindingJourney.Selected(const AInstance, ATab: TNyxText): Boolean;
{$ifndef PAS2JS}
var
  LHost: TNyxLCLTabHost;
{$endif}
begin
  {$ifdef PAS2JS}
  Result := FHost.querySelector('[data-nyx-tab="' + AInstance + '/' + ATab + '"]')
    .getAttribute('aria-selected') = 'true';
  {$else}
  LHost := TNyxLCLTabHost(FRenderer.ControlFor(AInstance + '/tab-recipe'));
  Result := (LHost.State.SelectedIndex >= 0) and
    (LHost.State.Entry(LHost.State.SelectedIndex).SourceReference.ID = ATab);
  Check(LHost.PageHost(LHost.State.SelectedIndex).PageControl.ActivePage =
    LHost.PageHost(LHost.State.SelectedIndex), 'native page matches shared selection');
  {$endif}
end;

procedure TTabBindingJourney.Click(const AInstance, ATab: TNyxText);
{$ifndef PAS2JS}
var
  LHost: TNyxLCLTabHost;
  LIndex: Integer;
{$endif}
begin
  {$ifdef PAS2JS}
  TJSHTMLButtonElement(FHost.querySelector('[data-nyx-tab="' + AInstance + '/' + ATab + '"]')).click;
  {$else}
  LHost := TNyxLCLTabHost(FRenderer.ControlFor(AInstance + '/tab-recipe'));
  LIndex := LHost.State.IndexOf(NyxControl(AInstance + '/' + ATab));
  TButtonAccess(LHost.Header(LIndex)).Click;
  {$endif}
end;

procedure TTabBindingJourney.Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin

  if (AEvent.Trigger = ntChange) and (ANode.SourceID = 'tab-recipe') then
  begin
    Inc(FCalls);
    Check(AEvent.HasValue and AEvent.Changed and
      (AEvent.Value.AsText = FRenderer.State.GetValue(NyxTextState('first.selection'))),
      'callback observes the committed local identity');
    Check(Selected('first', AEvent.Value.AsText), 'callback observes accepted presentation');
  end;
end;

procedure TTabBindingJourney.Observe(AState: TNyxState; AChanges: TNyxStateChanges);
begin
  { A borrowed validator needs a normal receipt; notification owns no state. }

  if FFailNotification then
  begin
    raise ENyxState.Create('Accepted selection notification failed');
  end;
end;

procedure TTabBindingJourney.Validate(ACandidate: TNyxState; AChanges: TNyxStateChanges);
begin

  if FReject then
  begin
    FObservedBefore := Selected('first', 'details') and
      (FRenderer.State.GetValue(NyxTextState('first.selection')) = 'details');
    raise ENyxState.Create('The application rejected this selection');
  end;
end;

procedure TTabBindingJourney.Run;
var
  LTabs: INyxTabs;
  LTab: INyxTab;
  LFirst: INyxComponent;
  LSecond: INyxComponent;
  LBefore: TNyxText;
  LRevision: Integer;
  LCalls: Integer;
  LRefused: Boolean;
  {$ifdef PAS2JS}
  LMemoIdentity: TJSHTMLElement;
  {$else}
  LMemoIdentity: TControl;
  {$endif}
  LWorkspace: TNyxSourceWorkspace;
  LCandidate: TNyxDocument;
  LGenerated: TNyxText;
begin
  FDocument := TNyxDocument.Create;
  FDocument.State.SetValue(NyxTextState('first.selection'), NyxTabValue(NyxControl('details')))
    .SetValue(NyxTextState('second.selection'), NyxTabValue(NyxControl('plan')))
    .SetValue(NyxBooleanState('first.readonly'), False)
    .SetValue(NyxBooleanState('plan.enabled'), True)
    .SetValue(NyxTextState('plan.caption'), 'Plan');
  LTabs := NewNyxTabs('tab-recipe');
  LTabs.Configure.Text('Project pages').Height(260).Done;
  LTab := NewNyxTab('plan');
  LTab.Configure.Text('Plan').Done;
  LTab.Binds.Enabled(NyxBooleanState('plan.enabled')).Text(NyxTextState('plan.caption')).Done;
  LTab.Add(NewNyxMemo('notes').Configure.Text('Working notes').Value('Retained notes').Done);
  LTabs.Add(LTab);
  LTabs.Add(NewNyxTab('details').Configure.Text('Details').Done);
  LTabs.Add(NewNyxTab('restricted').Configure.Text('Unavailable').Enabled(False).Done);
  SetNyxTabSelection(LTabs, NyxControl('plan'));
  FDocument.AddComponent(LTabs);
  FDocument.AddPage(NewNyxPage('home').Configure.Layout(nlColumn).Gap(12).Done);
  LFirst := NewNyxComponent('first');
  LFirst.Configure.Component(NyxComponent('tab-recipe')).Done;
  LFirst.Binds.Value(NyxTextState('first.selection')).ReadOnly(NyxBooleanState('first.readonly')).Done;
  LSecond := NewNyxComponent('second');
  LSecond.Configure.Component(NyxComponent('tab-recipe')).Done;
  LSecond.Binds.Value(NyxTextState('second.selection'), bdFromState).Done;
  FDocument.Pages[0].Add(LFirst).Add(LSecond);
  LTab := nil;
  LTabs := nil;
  LFirst := nil;
  LSecond := nil;
  LBefore := TNyxCodec.Encode(FDocument);
  { The crafted reference syntax must survive ordinary source admission. }
  LGenerated := TNyxCodegen.Generate(FDocument);
  Check(Pos('Value(NyxTabValue(NyxControl(''plan'')))', LGenerated) > 0,
    'generated selection uses a distinct typed reference');
  LWorkspace := TNyxSourceWorkspace.Create;
  LCandidate := nil;
  try
    LCandidate := LWorkspace.Candidate(FDocument, LGenerated);
    Check(TNyxCodec.Encode(LCandidate) = LBefore, 'typed selection source round-trip is exact');
    FreeAndNil(LCandidate);
    LRefused := False;
    try
      LCandidate := LWorkspace.Candidate(FDocument, StringReplace(LGenerated,
        'NyxTabValue(NyxControl(''plan''))', 'NyxTabValue(''plan'')', []));
    except
      on ENyxSource do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (TNyxCodec.Encode(FDocument) = LBefore),
      'raw text cannot impersonate a typed tab reference during source admission');
  finally
    LCandidate.Free;
    LWorkspace.Free;
  end;
  {$ifdef PAS2JS}
  FHost := TJSHTMLElement(document.createElement('div'));
  document.body.appendChild(FHost);
  FRenderer := TNyxBrowserRenderer.Create;
  FRenderer.OnEvent := @Event;
  {$else}
  FHost := TForm.Create(nil);
  FHost.SetBounds(0, 0, 700, 700);
  FRenderer := TNyxLCLRenderer.Create;
  FRenderer.OnEvent := Event;
  {$endif}
  FRenderer.Render(FDocument, FDocument.Pages[0], FHost);
  {$ifndef PAS2JS}
  FHost.Show;
  Application.ProcessMessages;
  {$endif}
  Check(Selected('first', 'details') and Selected('second', 'plan'),
    'bound defaults independently select reusable pages');
  {$ifdef PAS2JS}
  LMemoIdentity := FRenderer.InputFor('first/notes');
  TJSHTMLTextAreaElement(LMemoIdentity).value := 'Unfinished notes';
  {$else}
  LMemoIdentity := FRenderer.ControlFor('first/notes');
  TMemo(LMemoIdentity).Text := 'Unfinished notes';
  {$endif}
  FRenderer.State.SetValue(NyxTextState('first.selection'), NyxTabValue(NyxControl('plan')));
  Check(Selected('first', 'plan') and Selected('second', 'plan'),
    'programmatic selection changes only its own bound instance');
  Check(FCalls = 0, 'programmatic synchronization does not impersonate user input');
  Click('first', 'details');
  Check((FRenderer.State.GetValue(NyxTextState('first.selection')) = 'details') and
    Selected('first', 'details') and (FCalls = 1), 'actual header publishes one two-way change');
  Click('first', 'details');
  Check(FCalls = 1, 'unchanged activation does not notify again');
  FSubscription := FRenderer.State.Subscribe(Observe, Validate);
  FReject := True;
  LRevision := FRenderer.State.Revision;
  Click('first', 'plan');
  Check(FObservedBefore and Selected('first', 'details') and
    (FRenderer.State.Revision = LRevision) and (FCalls = 1),
    'application rejection precedes selection/presentation and preserves revision');
  Check(FRenderer.LastBindingError <> '', 'rejected input supplies an actionable binding error');
  FReject := False;
  FRenderer.State.SetValue(NyxBooleanState('first.readonly'), True);
  LRevision := FRenderer.State.Revision;
  Click('first', 'plan');
  Check(Selected('first', 'details') and (FRenderer.State.Revision = LRevision) and
    (FCalls = 1), 'read-only refusal preserves the accepted page and value');
  {$ifdef PAS2JS}
  TJSHTMLElement(FHost.querySelector('[data-nyx-tab="first/plan"]')).focus;
  Check(document.activeElement = FHost.querySelector('[data-nyx-tab="first/details"]'),
    'rejected automatic focus restores the accepted header without changing selection');
  {$else}
  TNyxLCLTabHost(FRenderer.ControlFor('first/tab-recipe')).Header(0).SetFocus;
  Check(TNyxLCLTabHost(FRenderer.ControlFor('first/tab-recipe')).Header(1).Focused,
    'rejected automatic focus restores the accepted header without changing selection');
  {$endif}
  FRenderer.State.SetValue(NyxBooleanState('first.readonly'), False);
  LRevision := FRenderer.State.Revision;
  LRefused := False;
  try
    FRenderer.State.Apply([
      NyxStateValue(NyxTextState('first.selection'), NyxTabValue(NyxControl('plan'))),
      NyxStateValue(NyxTextState('second.selection'), NyxTabValue(NyxControl('restricted')))], LRevision);
  except
    on LException: Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (FRenderer.State.Revision = LRevision) and
    Selected('first', 'details') and Selected('second', 'plan'),
    'one unavailable page refuses the complete multi-state candidate');
  Click('second', 'details');
  Check(Selected('second', 'plan') and (FRenderer.State.Revision = LRevision),
    'from-state binding refuses a user selection atomically');
  LCalls := FCalls;
  Click('first', 'plan');
  Check(Selected('first', 'plan') and (FCalls = LCalls + 1) and
    (FRenderer.LastBindingError = ''), 'next accepted change clears rejection diagnostics');
  LCalls := FCalls;
  FRenderer.State.Apply([
    NyxStateValue(NyxTextState('first.selection'), NyxTabValue(NyxControl('details'))),
    NyxStateValue(NyxTextState('second.selection'), NyxTabValue(NyxControl('details'))),
    NyxStateValue(NyxBooleanState('plan.enabled'), False),
    NyxStateValue(NyxTextState('plan.caption'), 'Planning')], FRenderer.State.Revision);
  Check(Selected('first', 'details') and Selected('second', 'details') and
    (FCalls = LCalls), 'selection and child availability publish in one complete batch');
  {$ifdef PAS2JS}
  Check(TJSHTMLButtonElement(FHost.querySelector('[data-nyx-tab="first/plan"]')).disabled and
    (FHost.querySelector('[data-nyx-tab="first/plan"]').textContent = 'Planning'),
    'actual header reflects live availability and caption without reconstruction');
  {$else}
  Check(not TNyxLCLTabHost(FRenderer.ControlFor('first/tab-recipe')).Header(0).Enabled and
    (TNyxLCLTabHost(FRenderer.ControlFor('first/tab-recipe')).Header(0).Caption = 'Planning'),
    'actual header reflects live availability and caption without reconstruction');
  {$endif}
  LRevision := FRenderer.State.Revision;
  Click('first', 'plan');
  Check(Selected('first', 'details') and (FRenderer.State.Revision = LRevision),
    'a newly disabled header cannot issue a stale selection');
  FRenderer.State.SetValue(NyxBooleanState('plan.enabled'), True);
  Click('first', 'plan');
  Check(Selected('first', 'plan') and (FCalls = LCalls + 1),
    'reenabled retained header can publish its next selection');
  LCalls := FCalls;
  FFailNotification := True;
  Click('first', 'details');
  Check(Selected('first', 'details') and
    (FRenderer.State.GetValue(NyxTextState('first.selection')) = 'details') and
    (FRenderer.LastBindingFailure = nbfNotificationFailed) and (FCalls = LCalls),
    'failed post-commit notification reports accepted state without rollback or duplicate input');
  FFailNotification := False;
  Click('first', 'plan');
  Check(Selected('first', 'plan') and (FCalls = LCalls + 1) and
    (FRenderer.LastBindingFailure = nbfNone), 'later accepted input recovers the existing host');
  {$ifdef PAS2JS}
  Check((FRenderer.InputFor('first/notes') = LMemoIdentity) and
    (TJSHTMLTextAreaElement(LMemoIdentity).value = 'Unfinished notes'),
    'selection retains the actual memo and its unfinished input');
  {$else}
  Check((FRenderer.ControlFor('first/notes') = LMemoIdentity) and
    (TMemo(LMemoIdentity).Text = 'Unfinished notes'),
    'selection retains the actual memo and its unfinished input');
  {$endif}
  Check(TNyxCodec.Encode(FDocument) = LBefore, 'runtime interaction preserves exact authored defaults');
  FSubscription.Disconnect;
  FSubscription.Free;
  FSubscription := nil;
  LCalls := FCalls;
  FRenderer.Render(FDocument, FDocument.Pages[0], FHost, True);
  LRevision := FRenderer.State.Revision;
  Click('first', 'plan');
  Check(Selected('first', 'plan'), 'design preview can inspect another retained page locally');
  Check((FRenderer.State.Revision = LRevision) and (FCalls = LCalls) and
    (FRenderer.State.GetValue(NyxTextState('first.selection')) = 'details') and
    (TNyxCodec.Encode(FDocument) = LBefore),
    'design preview does not execute bound application edits or callbacks');
  {$ifdef PAS2JS}
  document.body.setAttribute('data-nyx-tabs-binding', 'passed');
  document.body.setAttribute('data-nyx-tabs-binding-checks', IntToStr(FChecks));
  {$endif}
  WriteLn('PASS ', FChecks, ' actual tab binding checks');
end;

destructor TTabBindingJourney.Destroy;
begin

  if FSubscription <> nil then
  begin
    FSubscription.Disconnect;
    FSubscription.Free;
    FSubscription := nil;
  end;
  FRenderer.Free;
  {$ifdef PAS2JS}

  if FHost <> nil then
  begin
    FHost.remove;
  end;
  {$else}
  FHost.Free;
  {$endif}
  FDocument.Free;
  inherited Destroy;
end;

var
  LJourney: TTabBindingJourney;
begin
  LJourney := nil;
  try
    {$ifndef PAS2JS}Application.Initialize;{$endif}
    LJourney := TTabBindingJourney.Create;
    LJourney.Run;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-nyx-tabs-binding', 'failed');
      document.body.textContent := 'FAIL ' + LException.Message;
      {$else}
      DumpExceptionBackTrace(Output);
      {$endif}
      ExitCode := 1;
    end;
  end;
  LJourney.Free;
end.
