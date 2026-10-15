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

program nyx_save_activation_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, nyx.text, nyx.types, nyx.bytes, nyx.data, nyx.model,
  nyx.controls, nyx.behavior, nyx.events, nyx.scheduler, nyx.confirmation,
  nyx.files.save, nyx.files.save.activation
  {$ifdef PAS2JS}, Web{$endif};

type
  { Controlled confirmation exercises shared ownership only. It renders no UI;
    actual target confirmation input is qualified by the separate consumer. }
  TPrompt = class(TNyxConfirmationPresenter)
  public
    Opens: Integer;
    Closes: Integer;
    procedure Present(const AOptions: TNyxConfirmationOptions; const AFocusID: TNyxText); override;
    procedure Conceal; override;
    function GetEvents: INyxEvents; override;
    procedure Choose(ASave: Boolean);
  end;

  { Factory owns retained test observations of independent presentations, never
    a request or payload. Its snapshots remain valid after request retirement. }
  TPrompts = class(TInterfacedObject, INyxSaveActivationPrompts)
  public
    Current: TPrompt;
    Last: INyxConfirmation;
    Count: Integer;
    Reuse: Boolean;
    function CreatePrompt(const AOptions: TNyxFileSaveOptions;
      AByteCount: Integer): INyxConfirmation;
    function PromptOptions(const AOptions: TNyxFileSaveOptions): TNyxConfirmationOptions;
  end;

  { Shared-base request retains copied test bytes and honors receiver revocation.
    It grants no operating-system or browser file authority. }
  TRequest = class(TNyxFileSaveRequest)
  public
    procedure Send(AOutcome: TNyxFileSaveOutcome);
  end;

  { Substituted host can request fresh activation a fixed number of times.
    Complete delivery checks its captured payload; no real disk claim is made. }
  THost = class(TInterfacedObject, INyxFileSaveHost)
  public
    Refusals: Integer;
    Calls: Integer;
    Last: TNyxBytes;
    function BeginSave(const AOptions: TNyxFileSaveOptions; const ABytes: TNyxBytes;
      const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
      AReply: TNyxFileSaveReply): INyxFileSaveRequest;
  end;

  { Borrowed receiver outlives each test request; it records complete receipts. }
  TReply = class
  public
    Calls: Integer;
    Last: TNyxFileSaveResult;
    procedure Receive(const AResult: TNyxFileSaveResult);
  end;

  { Delegates to an actual scheduler but refuses the second delayed admission.
    This fault verifies that losing a retirement watch cannot strand a prompt,
    borrowed reply or pending self lease. Execution uses real parent UI turns;
    only the next delayed admission is deliberately refused. }
  TRejectingScheduler = class(TInterfacedObject, INyxScheduler, INyxSchedulerDelays,
    INyxSchedulerTurns)
  public
    Parent: INyxScheduler;
    Delays: Integer;
    First: INyxExecution;
    RefuseAfter: Integer;
    function GetCapabilities: TNyxSchedulerCapabilities;
    procedure RequireUI;
    procedure Admit(APolicy: TNyxExecutionPolicy);
    function Submit(const AWork: INyxWork; APolicy: TNyxExecutionPolicy): INyxExecution;
    function PostUI(const AWork: INyxWork; const AParent: INyxExecution = nil): INyxExecution;
    procedure Shutdown;
    function PostAfter(const AWork: INyxWork; ADelayMilliseconds: Integer): INyxExecution;
    function GetCanYieldUI: Boolean;
    function PostTurn(const AWork: INyxWork): INyxExecution;
  end;

var
  GChecks: Integer;
  GScheduler: INyxScheduler;
  GShutdownPrompt: INyxConfirmation;
  GFinalReceiver: TReply;
  GRejectedReceiver: TReply;
  GRejected: TRejectingScheduler;
  GRejectedLease: INyxScheduler;
  GRejectedPrompt: INyxConfirmation;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure TPrompt.Present(const AOptions: TNyxConfirmationOptions; const AFocusID: TNyxText);
begin
  Inc(Opens);
end;

procedure TPrompt.Conceal;
begin
  Inc(Closes);
end;

function TPrompt.GetEvents: INyxEvents;
begin
  Result := nil;
end;

procedure TPrompt.Choose(ASave: Boolean);
var
  LEvent: TNyxEventInfo;
begin
  LEvent := Default(TNyxEventInfo);
  LEvent.Value := NyxNull;
  LEvent.Details := NyxNull;
  LEvent.Trigger := ntNamed;
  LEvent.Name := NyxSemantic(nseCancel);

  if ASave then
  begin
    LEvent.Name := NyxSemantic(nseConfirm);
  end;
  LEvent.SourceID := GetContent.ID;
  LEvent.OriginID := GetContent.ID;
  LEvent.TargetID := GetContent.ID;
  Accept(LEvent);
end;

function TPrompts.PromptOptions(const AOptions: TNyxFileSaveOptions): TNyxConfirmationOptions;
begin
  Result := NyxConfirmation('Ready to save').Sizing(ncsContent);
end;

function TPrompts.CreatePrompt(const AOptions: TNyxFileSaveOptions;
  AByteCount: Integer): INyxConfirmation;
begin
  Inc(Count);

  if Reuse then
  begin
    Exit(Last);
  end;
  Current := TPrompt.Create(NewNyxSaveActivationContent('prepared-' +
    TNyxText(IntToStr(Count)), AOptions, AByteCount));
  Result := Current;
  Last := Result;
end;

procedure TRequest.Send(AOutcome: TNyxFileSaveOutcome);
var
  LCount: Integer;
begin
  LCount := 0;

  if AOutcome in [nfsoWritten, nfsoDownloadRequested] then
  begin
    LCount := Length(FBytes);
  end;
  Complete(AOutcome, FOptions.SuggestedName, LCount);
end;

function THost.BeginSave(const AOptions: TNyxFileSaveOptions;
  const ABytes: TNyxBytes; const AScheduler: INyxScheduler;
  const ACancellation: INyxCancellationScope; AReply: TNyxFileSaveReply): INyxFileSaveRequest;
var
  LRequest: TRequest;
  LIndex: Integer;
begin
  Inc(Calls);
  Last := nil;
  SetLength(Last, Length(ABytes));
  for LIndex := 0 to High(ABytes) do
  begin
    Last[LIndex] := ABytes[LIndex];
  end;
  LRequest := TRequest.Create(AOptions, ABytes, AScheduler, ACancellation, AReply);
  Result := LRequest;

  if Refusals > 0 then
  begin
    Dec(Refusals);
    LRequest.Send(nfsoActivationRequired);
  end
  else
  begin
    LRequest.Send(nfsoWritten);
  end;
end;

procedure TReply.Receive(const AResult: TNyxFileSaveResult);
begin
  Inc(Calls);
  Last := AResult;
end;

function TRejectingScheduler.GetCapabilities: TNyxSchedulerCapabilities;
begin
  Result := Parent.Capabilities;
end;

procedure TRejectingScheduler.RequireUI;
begin
  Parent.RequireUI;
end;

procedure TRejectingScheduler.Admit(APolicy: TNyxExecutionPolicy);
begin
  Parent.Admit(APolicy);
end;

function TRejectingScheduler.Submit(const AWork: INyxWork;
  APolicy: TNyxExecutionPolicy): INyxExecution;
begin
  Result := Parent.Submit(AWork, APolicy);
end;

function TRejectingScheduler.PostUI(const AWork: INyxWork;
  const AParent: INyxExecution): INyxExecution;
begin
  Result := Parent.PostUI(AWork, AParent);
end;

procedure TRejectingScheduler.Shutdown;
begin
  Parent.Shutdown;
end;

function TRejectingScheduler.PostAfter(const AWork: INyxWork;
  ADelayMilliseconds: Integer): INyxExecution;
begin
  Inc(Delays);

  if Delays > RefuseAfter then
  begin
    raise ENyxSchedule.Create('Controlled delayed admission refusal');
  end;
  Result := NyxSchedulerDelays(Parent).PostAfter(AWork, ADelayMilliseconds);
  First := Result;
end;

function TRejectingScheduler.GetCanYieldUI: Boolean;
begin
  Result := NyxSchedulerTurns(Parent).CanYieldUI;
end;

function TRejectingScheduler.PostTurn(const AWork: INyxWork): INyxExecution;
begin
  Result := NyxSchedulerTurns(Parent).PostTurn(AWork);
end;

procedure Run;
const
  CExact: TNyxText = 'Prepared 🌙 / 漢字' + #0;
var
  LHost: THost;
  LHostLease: INyxFileSaveHost;
  LPrompts: TPrompts;
  LPromptsLease: INyxSaveActivationPrompts;
  LActivated: INyxFileSaveHost;
  LRequest: INyxFileSaveRequest;
  LOther: INyxFileSaveRequest;
  LReceiver: TReply;
  LSecond: TReply;
  LScope: INyxCancellationScope;
  LBytes: TNyxBytes;
  LOptions: TNyxFileSaveOptions;
  LOld: TPrompt;
  LOldLease: INyxConfirmation;
  LRefused: TRejectingScheduler;
  LRefusedLease: INyxScheduler;
  LPromptCount: Integer;
begin
  GScheduler := NewNyxScheduler;
  LHost := THost.Create;
  LHostLease := LHost;
  LPrompts := TPrompts.Create;
  LPromptsLease := LPrompts;
  LReceiver := TReply.Create;
  LSecond := TReply.Create;
  try
    LOptions := NyxFileSaveOptions('prepared.txt');
    LBytes := NyxEncodeUTF8(CExact);
    LActivated := NewNyxActivatedFileSaveHost(LHostLease, LPromptsLease);
    LRequest := LActivated.BeginSave(LOptions, LBytes, GScheduler, nil, LReceiver.Receive);
    Check((LReceiver.Calls = 1) and (LHost.Calls = 1) and (LPrompts.Count = 0),
      'already sufficient activation passes through without a Nyx prompt');
    Check(LReceiver.Last.Outcome = nfsoWritten, 'pass-through preserves host outcome');
    LRequest := nil;

    LHost.Refusals := 2;
    LRequest := LActivated.BeginSave(LOptions, LBytes, GScheduler, nil, LReceiver.Receive);
    Check((LReceiver.Calls = 1) and LPrompts.Last.IsOpen and (LHost.Calls = 2),
      'activation refusal preserves pending output behind a public confirmation');
    LBytes[0] := 0;
    LPrompts.Current.Choose(True);
    Check((LReceiver.Calls = 1) and LPrompts.Last.IsOpen and (LPrompts.Current.Opens = 2),
      'a repeated activation refusal reuses the independent confirmation safely');
    LPrompts.Current.Choose(True);
    Check((LReceiver.Calls = 2) and not LPrompts.Last.IsOpen and (LHost.Calls = 4),
      'fresh Save completes once and closes the presentation');
    Check(NyxDecodeUTF8(LHost.Last) = CExact, 'retry owns exact original bytes independently of caller mutation');
    LPrompts.Current.Choose(True);
    Check(LReceiver.Calls = 2, 'retired prompt cannot dispatch another save');
    LRequest := nil;

    LActivated := NewNyxActivatedFileSaveHost(LHostLease, LPromptsLease, nsapAlwaysPrompt);
    LRequest := LActivated.BeginSave(LOptions, nil, GScheduler, nil, LReceiver.Receive);
    Check(LPrompts.Last.IsOpen and (LHost.Calls = 4), 'always-prompt policy dispatches no file before choice');
    LPrompts.Current.Choose(False);
    Check((LReceiver.Calls = 3) and (LReceiver.Last.Outcome = nfsoCancelled) and
      (LHost.Calls = 4), 'user Cancel returns a usable receipt without invoking the host');
    LRequest := nil;

    LScope := NewNyxCancellationScope;
    LRequest := LActivated.BeginSave(LOptions, nil, GScheduler, LScope, LReceiver.Receive);
    LScope.Cancel;
    LPrompts.Current.Choose(True);
    Check((LReceiver.Calls = 3) and (LHost.Calls = 4) and not LPrompts.Last.IsOpen,
      'scope retirement suppresses a stale Save action and closes its presentation');
    LRequest := nil;

    LRequest := LActivated.BeginSave(LOptions, nil, GScheduler, nil, LReceiver.Receive);
    LRequest.Cancel;
    Check(not LPrompts.Last.IsOpen and (LReceiver.Calls = 3), 'explicit request cancellation silently closes the prompt');
    LRequest := nil;

    LRequest := LActivated.BeginSave(LOptions, NyxEncodeUTF8('first'), GScheduler, nil, LReceiver.Receive);
    LOld := LPrompts.Current;
    LOldLease := LPrompts.Last;
    LOther := LActivated.BeginSave(LOptions.Suggest('other.txt'), NyxEncodeUTF8('second'),
      GScheduler, nil, LSecond.Receive);
    Check(LOldLease.IsOpen and LPrompts.Last.IsOpen, 'two requests have independently open presentations');
    LPrompts.Current.Choose(True);
    Check((LSecond.Calls = 1) and (LSecond.Last.Name = 'other.txt') and
      (NyxDecodeUTF8(LHost.Last) = 'second'), 'second request publishes its own exact payload first');
    LOld.Choose(True);
    Check((LReceiver.Calls = 4) and (NyxDecodeUTF8(LHost.Last) = 'first') and
      not LOldLease.IsOpen, 'first request remains independent after another completes');
    LRequest := nil;
    LOther := nil;
    LOldLease := nil;

    LRequest := LActivated.BeginSave(LOptions, nil, GScheduler, nil, LReceiver.Receive);
    LPrompts.Reuse := True;
    LOther := LActivated.BeginSave(LOptions, nil, GScheduler, nil, LSecond.Receive);
    Check((LSecond.Calls = 2) and (LSecond.Last.Outcome = nfsoFailed) and LPrompts.Last.IsOpen,
      'reused creator presentation refuses before touching another pending prompt');
    LPrompts.Reuse := False;
    LRequest.Cancel;
    LRequest := nil;
    LOther := nil;

    LRefused := TRejectingScheduler.Create;
    LRefusedLease := LRefused;
    LRefused.Parent := GScheduler;
    LPromptCount := LPrompts.Count;
    LOther := LActivated.BeginSave(LOptions, nil, LRefusedLease, nil, LSecond.Receive);
    Check((LSecond.Calls = 3) and (LSecond.Last.Outcome = nfsoFailed) and
      (LPrompts.Count = LPromptCount),
      'initial watch admission refusal resolves failure before presenting any UI');
    LOther := nil;
    LRefusedLease := nil;

    GRejected := TRejectingScheduler.Create;
    GRejectedLease := GRejected;
    GRejected.Parent := NewNyxScheduler;
    GRejected.RefuseAfter := 1;
    GRejectedReceiver := TReply.Create;
    LOther := LActivated.BeginSave(LOptions, nil, GRejectedLease, nil, GRejectedReceiver.Receive);
    GRejectedPrompt := LPrompts.Last;
    LOther := nil;

    LRequest := LActivated.BeginSave(LOptions, nil, GScheduler, nil, LReceiver.Receive);
    GShutdownPrompt := LPrompts.Last;
    GScheduler.Shutdown;
    LRequest := nil;
    GFinalReceiver := LReceiver;
    LReceiver := nil;
  finally
    LReceiver.Free;
    LSecond.Free;
  end;
end;

procedure Finish;
begin
  Check(not GShutdownPrompt.IsOpen, 'skipped scheduler observation closes its owned pending prompt');
  Check(GFinalReceiver.Calls = 4, 'shutdown never invokes the borrowed completion receiver');
  Check(not GRejectedPrompt.IsOpen and (GRejectedReceiver.Calls = 1) and
    (GRejectedReceiver.Last.Outcome = nfsoFailed),
    'failed next-watch admission closes the prompt and resolves the waiting save as failure');
  Check((GRejected.First <> nil) and (GRejected.First.Status = nesFailed) and
    (GRejected.First.Failure <> ''),
    'actual scheduler retains the failed observation diagnostic');
  GRejected.Shutdown;
  GRejectedPrompt := nil;
  GRejectedLease := nil;
  FreeAndNil(GRejectedReceiver);
  FreeAndNil(GFinalReceiver);
  GShutdownPrompt := nil;
  GScheduler := nil;
  {$ifdef PAS2JS}
  document.body.setAttribute('data-save-activation', 'passed');
  document.body.setAttribute('data-save-activation-checks', IntToStr(GChecks));
  {$else}
  WriteLn('PASS ', GChecks, ' shared save activation checks / controlled presentation and host');
  {$endif}
end;

{$ifdef PAS2JS}
procedure FinishBrowser;
begin
  try
    Finish;
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-save-activation', 'failed');
      document.body.setAttribute('data-save-activation-error', E.Message);
    end;
  end;
end;
{$endif}

{$ifndef PAS2JS}
var
  LStarted: QWord;
{$endif}
begin
  try
    Run;
    {$ifdef PAS2JS}
    window.setTimeout(@FinishBrowser, 150);
    {$else}
    LStarted := GetTickCount64;
    repeat
      CheckSynchronize;
      Sleep(1);
    until GetTickCount64 - LStarted > 150;
    Finish;
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-save-activation', 'failed');
      document.body.setAttribute('data-save-activation-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
