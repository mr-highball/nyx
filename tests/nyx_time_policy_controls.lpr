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

program nyx_time_policy_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifdef PAS2JS}JS, Web, nyx.render.browser,{$else}
  Interfaces, Classes, Forms, Controls, StdCtrls, nyx.render.lcl,
  nyx.studio.lcl,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.times.editor, nyx.contract,
  nyx.model, nyx.codec, nyx.generated.time, nyx.studio.projects,
  nyx.studio.session, nyx.studio.sourcejobs, nyx.studio.view,
  nyx.behavior, nyx.studio.inspector, nyx.controls, nyx.schema;

type
  {$ifdef PAS2JS}
  TRenderer = TNyxBrowserRenderer;
  THost = TJSHTMLElement;
  {$else}
  TRenderer = TNyxLCLRenderer;
  THost = TForm;
  TEditAccess = class(TCustomEdit);
  TControlAccess = class(TControl);
  {$endif}
  { Real public Inspector controls enter the ordinary independent paired source
    queue. No accepted project is edited directly by these field changes. Hosts
    belong only to this fixture; active editor/MCP state is never replaced. }
  TClockReview = class
  private
    FHost: THost;
    FRenderer: TRenderer;
    FSession: TNyxStudioSession;
    FCommands: TNyxSourceCommands;
    FShell: TNyxDocument;
    FStage: Integer;
    FError: TNyxText;
    FRefusal: TNyxText;
    FBefore: TNyxText;
    FAfter: TNyxText;
    procedure Refresh;
    procedure Changed(AState: TNyxSourceCommandState; const AMessage: TNyxText);
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure Change(AField: TNyxTimeDomainEditorField; const AValue: TNyxText);
    procedure Click(AField: TNyxTimeDomainEditorField);
    function Pair: TNyxText;
  public
    constructor Create(const ASource: TNyxText);
    destructor Destroy; override;
    function Step: Boolean;
  end;

var
  GReview: TClockReview;
  GChecks: Integer;
  {$ifdef PAS2JS}
  GRequest: TJSXMLHttpRequest;
  GStarted: Double;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Clock Inspector: ' + AReason);
  end;
  Inc(GChecks);
end;

{$ifndef PAS2JS}
{ Export the exact accepted pair from the physical source-queue consumer. The
  independent reconstruction program compiles these bytes rather than asking
  the generator for another builder that could hide source publication errors. }
procedure SaveAccepted(const AName, AText: TNyxText);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(2)) + AName,
    fmCreate);
  try

    if AText <> '' then
    begin
      LStream.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LStream.Free;
  end;
end;
{$endif}

{ Qualify the reusable value snapshot independently of Studio's controllers.
  Copies must outlive the source form, preserve incomplete Unicode text, park
  while absent, and refuse stale owners/baselines or a same-ID wrong-kind field
  without publishing even the first value. These checks run on both compilers. }
{$ifndef NYX_CLOCK_DRAFT_BASELINE}
procedure CheckDraftSnapshot(AContract: TNyxContract; const ADomain: TNyxValueDomain);
var
  LDraft: TNyxTimeDomainEditorDraft;
  LCopy: TNyxTimeDomainEditorDraft;
  LEditor: INyxCard;
  LAbsent: TNyxNode;
  LChoices: TNyxText;
  LBefore: TNyxText;
begin
  LDraft := Default(TNyxTimeDomainEditorDraft);
  LCopy := Default(TNyxTimeDomainEditorDraft);
  LAbsent := TNyxNode.Create(nkColumn, 'parked-inspector');
  LChoices := '23:00' + #10 + TNyxText('Unfinished note: é U0001f680');
  try
    LEditor := NewNyxTimeDomainEditor('draft-form', NyxControl('clock-owner'),
      AContract, ADomain);
    LEditor.Node.Find(NyxTimeDomainEditorFieldID('draft-form', ntfMilliseconds))
      .Configure.Value('1.5').Done;
    LEditor.Node.Find(NyxTimeDomainEditorFieldID('draft-form', ntfChoices))
      .Configure.Value(LChoices).Done;
    LDraft.Capture('draft-form', LEditor.Node);
    LCopy := LDraft;
    LEditor.Node.Find(NyxTimeDomainEditorFieldID('draft-form', ntfMilliseconds))
      .Configure.Value('500').Done;
    LDraft.Capture('draft-form', LEditor.Node);
    LEditor := nil;
    LCopy.Capture('draft-form', LAbsent);
    Check(not LCopy.Restore(LAbsent), 'An absent form parks copied input');
    LEditor := NewNyxTimeDomainEditor('draft-form', NyxControl('clock-owner'),
      AContract, ADomain);
    Check(LCopy.Restore(LEditor.Node), 'A draft outlives its original form');
    Check((LEditor.Node.Find(NyxTimeDomainEditorFieldID('draft-form', ntfMilliseconds))
      .Prop('value') = '1.5') and
      (LEditor.Node.Find(NyxTimeDomainEditorFieldID('draft-form', ntfChoices))
      .Prop('value') = LChoices), 'Copies preserve independent invalid and supplementary text');
    Check(LDraft.Restore(LEditor.Node) and
      (LEditor.Node.Find(NyxTimeDomainEditorFieldID('draft-form', ntfMilliseconds))
      .Prop('value') = '500'), 'Later capture cannot mutate an earlier snapshot');
    LEditor := NewNyxTimeDomainEditor('draft-form', NyxControl('another-owner'),
      AContract, ADomain);
    Check(not LDraft.Restore(LEditor.Node), 'Changed owner retires a copied draft');
    LEditor := NewNyxTimeDomainEditor('draft-form', NyxControl('clock-owner'),
      AContract, ADomain);
    Check(not LDraft.Restore(LEditor.Node), 'Retired owner input cannot reappear');
    LDraft := LCopy;
    LEditor := NewNyxTimeDomainEditor('draft-form', NyxControl('clock-owner'),
      AContract, NyxTimeDomain.AnyStep.Definition);
    Check(not LDraft.Restore(LEditor.Node), 'Changed effective domain retires inherited draft input');
    LDraft := LCopy;
    LEditor := NewNyxTimeDomainEditor('draft-form', NyxControl('clock-owner'),
      AContract, ADomain);
    LBefore := LEditor.Node.Find(NyxTimeDomainEditorFieldID('draft-form', ntfMilliseconds))
      .Prop('value');
    LEditor.Node.Remove(LEditor.Node.Find(NyxTimeDomainEditorFieldID('draft-form', ntfChoices)));
    LEditor.Add(NewNyxInput(NyxTimeDomainEditorFieldID('draft-form', ntfChoices)));
    Check(not LDraft.Restore(LEditor.Node) and
      (LEditor.Node.Find(NyxTimeDomainEditorFieldID('draft-form', ntfMilliseconds))
      .Prop('value') = LBefore), 'Wrong-kind field refuses before any partial restoration');
    LCopy.Clear;
    Check(not LCopy.Restore(LEditor.Node), 'Explicit project retirement clears all draft input');
  finally
    LEditor := nil;
    LAbsent.Free;
  end;
end;
{$endif}

constructor TClockReview.Create(const ASource: TNyxText);
var
  LDocument: TNyxDocument;
begin
  inherited Create;
  LDocument := BuildNyxDocument;
  try
    FSession := TNyxStudioSession.Create(NyxProjectPair(TNyxCodec.Encode(LDocument), ASource));
  finally
    LDocument.Free;
  end;
  FSession.Select('start-time');
  {$ifndef NYX_CLOCK_DRAFT_BASELINE}
  CheckDraftSnapshot(FSession.Selected.Contract, NyxNodeValueDomain(FSession.Selected));
  {$endif}
  FCommands := TNyxSourceCommands.Create(FSession, {$ifdef PAS2JS}@{$endif}Changed);
  FRenderer := TRenderer.Create;
  FRenderer.OnEvent := {$ifdef PAS2JS}@{$endif}Event;
  {$ifdef PAS2JS}
  FHost := TJSHTMLElement(document.createElement('main'));
  FHost.style.cssText := 'max-width:640px;';
  document.body.appendChild(FHost);
  {$else}
  FHost := TForm.CreateNew(nil);
  FHost.SetBounds(24, 24, 620, 840);
  FHost.Show;
  {$endif}
  Refresh;
end;

destructor TClockReview.Destroy;
begin
  FCommands.Free;
  FRenderer.Free;
  FShell.Free;
  FSession.Free;
  {$ifdef PAS2JS}
  FHost.remove;
  {$else}
  FHost.Free;
  {$endif}
  inherited Destroy;
end;

function TClockReview.Pair: TNyxText;
begin
  Result := EncodeNyxProject(FSession.ProjectSnapshot);
end;

procedure TClockReview.Refresh;
var
  LShell: TNyxDocument;
  LState: TNyxStudioViewState;
begin
  LState := DefaultNyxStudioViewState;
  LState.InspectorTab := nitProperties;
  LShell := BuildNyxStudioView(FSession, LState);
  try
    Check(LShell.Find('inspector-time-domain') <> nil,
      'ordinary Studio composes the public clock editor');
    FRenderer.Render(LShell, LShell.Find('inspector-time-domain'), FHost);
    FreeAndNil(FShell);
    FShell := LShell;
    LShell := nil;
  finally
    LShell.Free;
  end;
end;

procedure TClockReview.Changed(AState: TNyxSourceCommandState; const AMessage: TNyxText);
begin

  if AState in [nssFailed, nssRejected, nssStale] then
  begin
    FError := AMessage;
  end;

  if AState = nssApplied then
  begin
    Refresh;
  end;
end;

procedure TClockReview.Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin
  try
    FCommands.Route(ANode, AEvent.Trigger, FRenderer.Root);
  except
    on LException: ENyxContract do
    begin
      { Like Studio's controller, surface a capture refusal without enqueueing
        invalid fields or replacing the accepted design/source. }
      FRefusal := LException.Message;
    end;
  end;
end;

procedure TClockReview.Change(AField: TNyxTimeDomainEditorField; const AValue: TNyxText);
var
  LID: TNyxText;
  {$ifdef PAS2JS}
  LInput: TJSHTMLElement;
  {$else}
  LInput: TControl;
  {$endif}
begin
  LID := NyxTimeDomainEditorFieldID('inspector-time-domain', AField);
  LInput := FRenderer.InputFor(LID);
  Check(LInput <> nil, 'actual clock-policy field is mounted');
  {$ifdef PAS2JS}
  TJSHTMLInputElement(LInput).value := AValue;
  LInput.dispatchEvent(TJSEvent.new('change'));
  {$else}

  if LInput is TComboBox then
  begin
    TComboBox(LInput).ItemIndex := TComboBox(LInput).Items.IndexOf(AValue);
    TComboBox(LInput).OnChange(LInput);
  end
  else
  begin
    TCustomEdit(LInput).Text := AValue;

    if AField in [ntfMinimum, ntfMaximum] then
    begin
      TEditAccess(LInput).OnEditingDone(LInput);
    end;
  end;
  {$endif}
  Check(FRenderer.Root.Find(LID).Prop('value') = AValue,
    'physical field retains its exact proposal');
end;

procedure TClockReview.Click(AField: TNyxTimeDomainEditorField);
var
  LID: TNyxText;
begin
  LID := NyxTimeDomainEditorFieldID('inspector-time-domain', AField);
  {$ifdef PAS2JS}
  FRenderer.ElementFor(LID).click;
  {$else}
  TControlAccess(FRenderer.ControlFor(LID)).Click;
  {$endif}
end;

function TClockReview.Step: Boolean;
var
  LDomain: TNyxValueDomain;
begin
  Result := False;

  if FError <> '' then
  begin
    raise Exception.Create(FError);
  end;

  if FCommands.Busy then
  begin
    Exit;
  end;
  case FStage of
    0:
      begin
        FBefore := Pair;
        Change(ntfMinimum, '23:00:00.000');
        Change(ntfMaximum, '01:00:00.000');
        Change(ntfStepMode, NyxTimeDomainEditorStepName(ntsMilliseconds));
        Change(ntfMilliseconds, '500');
        Check(Pair = FBefore, 'field proposals preserve accepted design/source/history');
        FStage := 1;
        Click(ntfApply);
      end;
    1:
      begin
        Check(FCommands.State = nssApplied, 'actual Apply uses the independent paired queue');
        Check(FSession.Selected.Contract.FindValue(LDomain) and LDomain.ClockTime and
          (LDomain.TimeStepMilliseconds = 500), 'the authored owner receives the typed clock policy');
        Check((LDomain.ToData.Field('min').AsText = '23:00:00.000') and
          (LDomain.ToData.Field('max').AsText = '01:00:00.000'),
          'overnight bounds retain millisecond precision');
        Check(Pos('.StepMilliseconds(500)', FSession.Source) > 0,
          'near-real-time source uses crafted typed authoring');
        FAfter := Pair;
        {$ifndef PAS2JS}
        SaveAccepted('nyx.generated.time.pas', FSession.Source);
        SaveAccepted('design.nyx.json', FSession.Save);
        {$endif}
        FSession.Undo;
        Check(Pair = FBefore, 'one Undo restores the exact paired original');
        FSession.Redo;
        Check(Pair = FAfter, 'one Redo restores the exact authored pair');
        Refresh;
        Change(ntfMilliseconds, '1.5');
        FStage := 2;
        Click(ntfApply);
      end;
    2:
      begin
        Check((Pair = FAfter) and not FCommands.Busy and (FRefusal <> ''),
          'fractional step refuses without changing the accepted pair');
        Refresh;
        FStage := 3;
        Click(ntfInherit);
      end;
    3:
      begin
        Check(not FSession.Selected.Contract.FindValue(LDomain),
          'actual Restore removes only the local value declaration');
        Check(Pair = FBefore, 'restoring inheritance retains the exact original compound contract');
        Result := True;
      end;
  else
    raise Exception.Create('Unknown clock Inspector qualification stage');
  end;
end;

{$ifndef PAS2JS}
{ Exercise the complete ordinary controller, rather than a form-shaped shell.
  These are local owned projects. Actual controls must retain unfinished input
  through repaint, panel/viewport changes and a refused Apply; only the paired
  queue may publish a policy, with one exact ordinary Undo/Redo checkpoint. }
procedure RunNativeStudio(const ASource: TNyxText);
var
  LStudio: TNyxNativeStudio;
  LForm: TForm;
  LDocument: TNyxDocument;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LCode: TControl;
  LDomain: TNyxValueDomain;
  LChoices: TNyxText;

  procedure Ready;
  var
    LStarted: QWord;
  begin
    LStarted := GetTickCount64;
    repeat
      Application.ProcessMessages;
      CheckSynchronize;

      if GetTickCount64 - LStarted > 30000 then
      begin
        raise Exception.Create('Ordinary clock Studio did not finish: ' + LStudio.Status);
      end;
      Sleep(1);
    until not LStudio.PresentationPending and not LStudio.SourceCommands.Busy;
  end;

  function Input(AField: TNyxTimeDomainEditorField): TControl;
  begin
    Result := LStudio.ShellView.InputFor(
      NyxTimeDomainEditorFieldID('inspector-time-domain', AField));

    if Result = nil then
    begin
      raise Exception.Create('The ordinary clock Inspector field is not mounted');
    end;
  end;

  procedure Change(AField: TNyxTimeDomainEditorField; const AValue: TNyxText);
  var
    LInput: TControl;
  begin
    LInput := Input(AField);

    if LInput is TComboBox then
    begin
      TComboBox(LInput).ItemIndex := TComboBox(LInput).Items.IndexOf(AValue);
      TComboBox(LInput).OnChange(LInput);
    end
    else
    begin
      TCustomEdit(LInput).Text := AValue;

      if AField in [ntfMinimum, ntfMaximum] then
      begin
        TEditAccess(LInput).OnEditingDone(LInput);
      end;
    end;
  end;

  procedure Button(const AID: TNyxText);
  begin
    TControlAccess(LStudio.ShellView.ControlFor(AID)).Click;
    Ready;
  end;

  procedure DraftRetained(const AReason: TNyxText);
  var
    LActual: TNyxDataValue;
  begin
    LActual := NyxObject([
      NyxField('minimum', NyxData(TNyxText(TCustomEdit(Input(ntfMinimum)).Text))),
      NyxField('maximum', NyxData(TNyxText(TCustomEdit(Input(ntfMaximum)).Text))),
      NyxField('stepMode', NyxData(TNyxText(TComboBox(Input(ntfStepMode)).Text))),
      NyxField('milliseconds', NyxData(TNyxText(TCustomEdit(Input(ntfMilliseconds)).Text))),
      NyxField('choices', NyxData(TNyxText(TCustomEdit(Input(ntfChoices)).Text)))]);
    Check((TCustomEdit(Input(ntfMinimum)).Text = '23:00:00.000') and
      (TCustomEdit(Input(ntfMaximum)).Text = '01:00:00.000') and
      (TComboBox(Input(ntfStepMode)).Text = NyxTimeDomainEditorStepName(ntsMilliseconds)) and
      (TCustomEdit(Input(ntfMilliseconds)).Text = '1.5') and
      (TNyxText(TCustomEdit(Input(ntfChoices)).Text) = LChoices),
      AReason + ' / actual ' + LActual.ToJSON);
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore,
      'Unsubmitted constraints retain the exact accepted pair');
  end;

begin
  LForm := TForm.CreateNew(nil);
  LStudio := nil;
  try
    LForm.SetBounds(20, 20, 1280, 940);
    LForm.Show;
    LStudio := TNyxNativeStudio.Create(LForm,
      IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(2))) + 'clock-studio-projects');
    LDocument := BuildNyxDocument;
    try
      LStudio.LoadProject(NyxProjectPair(TNyxCodec.Encode(LDocument), ASource));
    finally
      LDocument.Free;
    end;
    LStudio.Session.Select('start-time');
    LStudio.Run;
    Ready;
    Check(LStudio.ShellView.Root.Find('inspector-time-domain') <> nil,
      'Full ordinary Studio mounts the public clock-constraint editor');
    LBefore := EncodeNyxProject(LStudio.Session.ProjectSnapshot);
    LChoices := '23:00:00.000' + #10 + '00:30:00.000' + #10 + '(empty)';
    Change(ntfMinimum, '23:00:00.000');
    Change(ntfMaximum, '01:00:00.000');
    Change(ntfStepMode, NyxTimeDomainEditorStepName(ntsMilliseconds));
    Change(ntfMilliseconds, '1.5');
    Change(ntfChoices, LChoices);
    { Native memo APIs expose CRLF. Compare the exact observed physical draft
      across repaint, rather than assuming the LF source notation survives the
      widget's initial assignment. The portable snapshot check keeps exact LF. }
    LChoices := TNyxText(TCustomEdit(Input(ntfChoices)).Text);
    Button('action-code');
    LCode := LStudio.CodeView.InputFor('studio-code');
    DraftRetained('Opening Pascal retains all five unfinished clock fields');
    Button(NyxInspectorEventsID);
    Button(NyxInspectorPropertiesID);
    DraftRetained('Events/Properties navigation retains the parked clock draft');
    LForm.ClientWidth := 390;
    Ready;
    Button('action-panel-inspector');
    DraftRetained('Compact viewport allocation retains exact clock proposals');
    Button(NyxTimeDomainEditorFieldID('inspector-time-domain', ntfApply));
    Check(not LStudio.SourceCommands.Busy, 'Invalid Apply does not enqueue a policy');
    DraftRetained('Refused Apply leaves invalid text available for correction');
    Change(ntfMilliseconds, '500');
    Button(NyxTimeDomainEditorFieldID('inspector-time-domain', ntfApply));
    Check((LStudio.SourceCommands.State = nssApplied) and
      LStudio.Session.Selected.Contract.FindValue(LDomain) and LDomain.ClockTime and
      (LDomain.TimeStepMilliseconds = 500) and (LDomain.ToData.Field('choices').Count = 3),
      'Ordinary controller admits the corrected complete typed clock policy');
    { Compact Inspector parks the center/source workspace. Check the published
      code when that ordinary pane is visible again, retaining its exact control
      rather than treating a parked widget's old paint as an active source view. }
    Button('action-panel-design');
    Check((LStudio.CodeView.InputFor('studio-code') = LCode) and
      (Pos('.StepMilliseconds(500)', TMemo(LCode).Text) > 0),
      'The retained Pascal editor reflects the paired typed policy');
    Button('action-panel-inspector');
    LAfter := EncodeNyxProject(LStudio.Session.ProjectSnapshot);
    Button('action-undo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore,
      'Ordinary Undo restores the exact pre-draft accepted pair');
    Button('action-redo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LAfter,
      'Ordinary Redo restores the exact corrected policy pair');
    Change(ntfMilliseconds, '2.5');
    LStudio.Session.Select('earliest-time');
    LStudio.RequestRefresh;
    Ready;
    Check(TCustomEdit(Input(ntfMinimum)).Text = '08:30',
      'Changed selection never receives another owner clock draft');
    LStudio.Session.Select('start-time');
    LStudio.RequestRefresh;
    Ready;
    Check(TCustomEdit(Input(ntfMilliseconds)).Text = '500',
      'Retired owner draft does not replay when selection returns');
    Change(ntfMilliseconds, '3.5');
    LDocument := BuildNyxDocument;
    try
      LStudio.LoadProject(NyxProjectPair(TNyxCodec.Encode(LDocument), ASource));
    finally
      LDocument.Free;
    end;
    LStudio.Session.Select('start-time');
    Ready;
    Check(TCustomEdit(Input(ntfMilliseconds)).Text = '1500',
      'Project replacement retires a same-named owner draft');
  finally
    LStudio.Free;
    LForm.Free;
  end;
  WriteLn('PASS ', GChecks, ' clock-policy checks including full ordinary native Studio');
end;
{$endif}

{$ifdef PAS2JS}
procedure Tick;
begin
  try

    if window.performance.now - GStarted > 30000 then
    begin
      raise Exception.Create('Clock Inspector qualification timed out');
    end;

    if GReview.Step then
    begin
      document.body.setAttribute('data-time-policy', 'passed');
      document.body.setAttribute('data-time-policy-checks', IntToStr(GChecks));
      Exit;
    end;
    window.setTimeout(@Tick, 10);
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-time-policy', 'failed');
      document.body.setAttribute('data-time-policy-error', LException.Message);
    end;
  end;
end;
{$endif}

var
  {$ifndef PAS2JS}
  LStream: TFileStream;
  LSource: TNyxText;
  LStarted: QWord;
  LDone: Boolean;
  {$endif}
begin
  {$ifdef PAS2JS}
  GRequest := TJSXMLHttpRequest.new;
  GRequest.open('GET', 'seed.pas.txt', True);
  GRequest.onload := function(AEvent: TJSProgressEvent): Boolean
    begin
      Result := True;
      try

        if GRequest.status <> 200 then
        begin
          raise Exception.Create('Exact clock companion HTTP source is unavailable');
        end;
        GReview := TClockReview.Create(GRequest.responseText);
        GStarted := window.performance.now;
        Tick;
      except
        on LException: Exception do
        begin
          document.body.setAttribute('data-time-policy', 'failed');
          document.body.setAttribute('data-time-policy-error', LException.Message);
        end;
      end;
    end;
  GRequest.send;
  {$else}
  try

    if ParamCount <> 2 then
    begin
      raise Exception.Create('Supply exact clock source and an owned export directory');
    end;
    ForceDirectories(ParamStr(2));
    Application.Initialize;
    LStream := TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyWrite);
    try
      SetLength(LSource, LStream.Size);

      if LSource <> '' then
      begin
        LStream.ReadBuffer(LSource[1], Length(LSource));
      end;
    finally
      LStream.Free;
    end;
    GReview := TClockReview.Create(LSource);
    LStarted := GetTickCount64;
    repeat
      Application.ProcessMessages;
      LDone := GReview.Step;

      if not LDone then
      begin
        Sleep(5);
      end;

      if GetTickCount64 - LStarted > 30000 then
      begin
        raise Exception.Create('Native clock Inspector qualification timed out');
      end;
    until LDone;
    WriteLn('PASS ', GChecks, ' actual native clock Inspector/queue checks');
    FreeAndNil(GReview);
    RunNativeStudio(LSource);
  except
    on LException: Exception do
    begin
      WriteLn('FAIL after ', GChecks, ' / ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
  GReview.Free;
  {$endif}
end.
