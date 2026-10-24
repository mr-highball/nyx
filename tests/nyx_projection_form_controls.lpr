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
program nyx_projection_form_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifndef PAS2JS}Interfaces, Classes, Forms, Controls, StdCtrls,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls, nyx.contract,
  nyx.behavior, nyx.events, nyx.scheduler, nyx.state, nyx.codec,
  nyx.publication, nyx.projection.refresh, nyx.content.mount
  {$ifdef PAS2JS}, JS, Web, nyx.render.browser
  {$else}, nyx.render.lcl{$endif};

type
  {$ifdef PAS2JS}
  { Real DOM renderer; all input and publication use ordinary public adapters. }
  TRenderer = TNyxBrowserRenderer;
  { Borrowed field/action face, valid only while its exact renderer is mounted. }
  TFace = TJSHTMLElement;
  {$else}
  { Real LCL renderer; the fixture retains controls and allocated window handles. }
  TRenderer = TNyxLCLRenderer;
  { Borrowed native control, never freed independently of its renderer. }
  TFace = TControl;
  {$endif}

  { A real deferred child has no renderer or callback receiver ownership edge. }
  TChild = class(TInterfacedObject, INyxWork)
    procedure Execute(const AExecution: INyxExecution);
  end;

  { The router owns this callback through its token. Its original execution
    context and child are independently retained after physical dispatch ends. }
  TPosting = class(TNyxEventCallback)
  public
    Scheduler: INyxScheduler;
    Child: INyxExecution;
    Context: INyxExecution;
    Calls: Integer;
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;

var
  GChecks: Integer;
  GChildRuns: Integer;

const
  CFormOwner = 'test.form-owner';

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxModel.Create('Form controls: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure TChild.Execute(const AExecution: INyxExecution);
begin
  Inc(GChildRuns);
end;

procedure TPosting.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  Inc(Calls);
  Context := AExecution;
  Child := Scheduler.PostUI(TChild.Create, AExecution);
end;

{$ifdef PAS2JS}
function Tick: TJSPromise;
begin
  Result := TJSPromise.new(procedure(AResolve, AReject: TJSPromiseResolver)
    begin
      window.setTimeout(procedure
        begin
          AResolve(Undefined);
        end, 0);
    end);
end;
{$endif}

function TextOf(AFace: TFace): TNyxText;
begin
  {$ifdef PAS2JS}
  Result := TJSHTMLInputElement(AFace).value;
  {$else}
  Result := TCustomEdit(AFace).Text;
  {$endif}
end;

procedure Draft(AFace: TFace; const AText: TNyxText);
begin
  {$ifdef PAS2JS}
  TJSHTMLInputElement(AFace).value := AText;
  AFace.dispatchEvent(TJSEvent.new('input'));
  {$else}
  TCustomEdit(AFace).Text := AText;
  {$endif}
end;

function Fixture(ABound: Boolean): TNyxDocument;
var
  LPage: INyxPage;
  LForm: INyxCard;
  LInput: INyxInput;
begin
  Result := TNyxDocument.Create;
  try
    LPage := NewNyxPage('forms');
    Result.AddPage(LPage);
    LForm := NewNyxCard('first-form');
    LForm.Node.SetProp(CFormOwner, 'first owner');
    LPage.Add(LForm);
    LInput := NewNyxInput('first-entry');
    LInput.Configure.Text('Amount').InputType(niNumber).Value(7.0).Done;
    LInput.Contract.Value(NyxNumberDomain.Range(0, 100));
    LForm.Add(LInput);
    LForm.Add(NewNyxButton('post').WithText('Queue work'));
    LForm := NewNyxCard('other-form');
    LForm.Node.SetProp(CFormOwner, 'other owner');
    LPage.Add(LForm);
    LInput := NewNyxInput('other-entry');
    LInput.Configure.Text('Independent amount').InputType(niNumber).Value(9.0).Done;
    LInput.Contract.Value(NyxNumberDomain.Range(0, 100));
    LForm.Add(LInput);

    if ABound then
    begin
      Result.State.SetValue(NyxNumberState('amount'), 7.0);
      Result.Find('first-entry').Binds.Value(NyxNumberState('amount')).Done;
    end;
  except
    Result.Free;
    raise;
  end;
end;

{$ifdef PAS2JS}
function RunCase(ABound: Boolean): JSValue; async;
{$else}
procedure RunCase(ABound: Boolean);
{$endif}
var
  LDocument: TNyxDocument;
  LCandidate: TNyxDocument;
  LStore: TNyxState;
  LRenderer: TRenderer;
  LFirst: TFace;
  LOther: TFace;
  LInputs: TNyxContentFaceStates;
  LForms: TNyxProjectionFormContexts;
  LPublication: INyxPreparedPublication;
  LPosting: TPosting;
  LCallback: INyxEventCallback;
  LToken: INyxEventSubscription;
  LRoot: TNyxNode;
  LBefore: TNyxText;
  LRevision: Integer;
  LIndex: Integer;
  LExpected: TNyxText;
  {$ifdef PAS2JS}
  LHost: TJSHTMLElement;
  {$else}
  LHost: TForm;
  LFirstHandle: PtrUInt;
  LOtherHandle: PtrUInt;
  {$endif}
begin
  {$ifdef PAS2JS}Result := Undefined;{$endif}
  LDocument := Fixture(ABound);
  LCandidate := nil;
  LStore := nil;
  LRenderer := TRenderer.Create;
  LPosting := TPosting.Create;
  LCallback := LPosting;
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('div'));
  LHost.style.setProperty('width', 'min(720px, 100%)');
  LHost.style.setProperty('height', '540px');
  document.body.appendChild(LHost);
  {$else}
  LHost := TForm.CreateNew(nil);
  LHost.SetBounds(20, 20, 720, 540);
  LHost.Show;
  {$endif}
  try

    if ABound then
    begin
      LStore := LDocument.State.Clone;
    end;
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost, False, LStore);

    if ABound then
    begin
      LStore.SetValue(NyxNumberState('amount'), 8.0);
    end;
    LFirst := LRenderer.InputFor('first-entry');
    LOther := LRenderer.InputFor('other-entry');
    LRoot := LRenderer.Root;
    LBefore := TNyxCodec.Encode(LDocument);
    LExpected := '7';

    if ABound then
    begin
      LExpected := '8';
    end;
    {$ifdef PAS2JS}
    LOther.focus;
    {$else}
    TWinControl(LOther).SetFocus;
    Check(TWinControl(LFirst).HandleAllocated and TWinControl(LOther).HandleAllocated,
      'both real input windows are allocated before preparation');
    LFirstHandle := PtrUInt(TWinControl(LFirst).Handle);
    LOtherHandle := PtrUInt(TWinControl(LOther).Handle);
    {$endif}
    Draft(LFirst, '12.75');
    Draft(LOther, '23.5');
    Check((TextOf(LFirst) = '12.75') and
      (LRenderer.Root.Find('first-entry').Prop('value') = LExpected),
      'numeric draft is physically separate from its current accepted value');
    Check(LRenderer.CaptureInteraction(LInputs), 'idle inputs expose copied continuity');
    SetLength(LForms, 1);
    LForms[0] := TNyxProjectionFormContext.ForForm('first-form', 'first-form',
      [NyxExtension(CFormOwner)]);
    LCandidate := LDocument.Clone;
    LCandidate.Find('first-form').SetProp(CFormOwner, 'second owner / 🌙');
    Check(not LRenderer.TryRefresh(LCandidate, LCandidate.Pages[0], False),
      'ordinary scalar refresh does not admit a changed form owner');
    LPosting.Scheduler := LRenderer.Events.Scheduler;
    LToken := LRenderer.Events.On(NyxControlEvents('post', niRuntime), ntClick)
      .Subscribe(LCallback);
    {$ifdef PAS2JS}
    LRenderer.ElementFor('post').click;
    {$else}
    TButton(LRenderer.ControlFor('post')).Click;
    {$endif}
    Check((LPosting.Calls = 1) and (LPosting.Child.Status = nesPending),
      'real physical callback leaves an explicit child queued');
    LPublication := LRenderer.TryPrepareFormRefresh(LCandidate, LCandidate.Pages[0], LForms);
    Check((LPublication = nil) and (TextOf(LFirst) = '12.75') and LToken.Active and
      (LPosting.Child.Status = nesPending), 'busy form preview refuses without canceling work or drafts');
    for LIndex := 1 to 200 do
    begin
      {$ifdef PAS2JS}await(Tick);{$else}CheckSynchronize(10); Application.ProcessMessages;{$endif}

      if LPosting.Child.Status = nesSucceeded then
      begin
        Break;
      end;
    end;
    Check(LPosting.Child.Status = nesSucceeded, 'the refused preview leaves child work executable');
    LRevision := LRenderer.Events.ViewRevision;
    LPublication := LRenderer.TryPrepareFormRefresh(LCandidate, LCandidate.Pages[0], LForms);
    Check((LPublication <> nil) and (LRenderer.Root = LRoot) and
      (LRenderer.InputFor('first-entry') = LFirst) and
      (LRenderer.InputFor('other-entry') = LOther), 'prepared form preview retains actual root and controls');
    Check((TextOf(LFirst) = LExpected) and (TextOf(LOther) = '23.5') and LToken.Active and
      (LRenderer.Events.ViewRevision = LRevision), 'only the changed form resets before reversible admission');
    Check(not LRenderer.CaptureInteraction(LInputs), 'unpublished preview fences ordinary continuity admission');
    LPublication.Retire;
    LPublication := nil;
    Check((TextOf(LFirst) = '12.75') and (TextOf(LOther) = '23.5') and
      (LRenderer.Root.Find('first-form').Prop(CFormOwner) = 'first owner') and
      LToken.Active and (LRenderer.Events.ViewRevision = LRevision),
      'abandonment restores exact old owner, drafts and event generation');
    LPublication := LRenderer.TryPrepareFormRefresh(LCandidate, LCandidate.Pages[0], LForms);
    Check(LPublication <> nil, 'the same source is admissible after rollback');
    LPublication := LRenderer.TryPrepareFormRefresh(LCandidate, LCandidate.Pages[0], LForms);
    Check((LPublication = nil) and (TextOf(LFirst) = '12.75') and
      (TextOf(LOther) = '23.5') and LToken.Active and
      (LRenderer.Events.ViewRevision = LRevision),
      'same-destination preview replacement decides busy before releasing its old guard');
    LPublication := LRenderer.TryPrepareFormRefresh(LCandidate, LCandidate.Pages[0], LForms);
    Check(LPublication <> nil, 'a fresh caller turn can prepare after aliased refusal restores the old form');
    PublishNyxGroup([LPublication]);
    LPublication := nil;
    Check((LRenderer.Root = LRoot) and (LRenderer.InputFor('first-entry') = LFirst) and
      (TextOf(LFirst) = LExpected) and (TextOf(LOther) = '23.5') and LToken.Active and
      LPosting.Context.Cancelled and
      (LRenderer.Events.ViewRevision = LRevision + 1), 'installation retains controls and retires old event authority');
    {$ifndef PAS2JS}
    Check((PtrUInt(TWinControl(LFirst).Handle) = LFirstHandle) and
      (PtrUInt(TWinControl(LOther).Handle) = LOtherHandle),
      'preview, rollback and commit retain both exact native windows');
    {$endif}
    { Registrations live until explicit Cancel. View publication retires old
      executions, while this same registration must receive a fresh context. }
    {$ifdef PAS2JS}
    LRenderer.ElementFor('post').click;
    {$else}
    TButton(LRenderer.ControlFor('post')).Click;
    {$endif}
    Check(LPosting.Calls = 2, 'the retained physical button enters its new event generation');
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'runtime preview never changes authored defaults/source');

    if ABound then
    begin
      Check(LStore.GetValue(NyxNumberState('amount')) = 8.0,
        'form reset reads current independent state without writing its default');
      LStore.SetValue(NyxNumberState('amount'), 11.0);
      Check(TextOf(LFirst) = '11', 'the retained live binding still follows subsequent store writes');
    end;
  finally

    if LPublication <> nil then
    begin
      LPublication.Retire;
    end;
    LPublication := nil;

    if LToken <> nil then
    begin
      LToken.Cancel;
    end;
    LToken := nil;
    LCallback := nil;
    LRenderer.Free;
    LStore.Free;
    LCandidate.Free;
    LDocument.Free;
    {$ifdef PAS2JS}LHost.remove;{$else}LHost.Free;{$endif}
  end;
end;

{$ifdef PAS2JS}
function Run: JSValue; async;
{$else}
procedure Run;
{$endif}
begin
  {$ifdef PAS2JS}Result := Undefined;{$else}Application.Initialize;{$endif}
  try
    {$ifdef PAS2JS}
    await(RunCase(False));
    await(RunCase(True));
    {$else}
    RunCase(False);
    RunCase(True);
    {$endif}
    WriteLn('PASS ', GChecks, ' actual retained form checks');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-form-controls', 'passed');
    document.body.setAttribute('data-form-controls-checks', IntToStr(GChecks));
    {$endif}
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-form-controls', 'failed');
      document.body.setAttribute('data-form-controls-error', LException.Message);
      {$else}DumpExceptionBackTrace(Output); ExitCode := 1;{$endif}
    end;
  end;
end;

begin
  Run;
end.
