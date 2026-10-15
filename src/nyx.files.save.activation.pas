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

unit nyx.files.save.activation;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.bytes, nyx.controls, nyx.confirmation, nyx.files.save;

type
  { Runtime-only choice: recover only an ActivationRequired receipt, or always
    ask through Nyx first. Neither policy changes file permissions or save mode. }
  TNyxSaveActivationPolicy = (nsapWhenRequired, nsapAlwaysPrompt);

  { Managed target/creator seam creating an independent Nyx confirmation for
    each pending save. The factory receives suggestions/count only, never bytes
    or file authority. Its returned presenter owns its content and target view.
    Factories must not strongly retain their owning decorator or its requests;
    that would create a caller-owned reference cycle. Presentation options keep
    title, geometry and initial focus customizable through ordinary Nyx types. }
  INyxSaveActivationPrompts = interface(IInterface)
    ['{60CAFF21-C6E4-43D0-8855-940000000005}']
    function CreatePrompt(const AOptions: TNyxFileSaveOptions;
      AByteCount: Integer): INyxConfirmation;
    function PromptOptions(const AOptions: TNyxFileSaveOptions): TNyxConfirmationOptions;
  end;

{ Compose a runtime host with a fresh user-action presentation. Prepared bytes
  remain private to one request while its graph waits. Save retries directly in
  the sequential confirmation callback; cancel/retirement never dispatch a file.
  The underlying host still decides availability, activation and actual outcome.
  No serialized option or signal can create this runtime authority. }
function NewNyxActivatedFileSaveHost(const AHost: INyxFileSaveHost;
  const APrompts: INyxSaveActivationPrompts;
  APolicy: TNyxSaveActivationPolicy = nsapWhenRequired): INyxFileSaveHost;

{ Independently owned, extensible public Nyx recipe for default target factories.
  Named parts retain ordinary confirmation contracts. No payload or host is
  attached; callers may customize the template before creating a presenter. }
function NewNyxSaveActivationContent(const AID: TNyxText;
  const AOptions: TNyxFileSaveOptions; AByteCount: Integer): INyxConfirmationDialog;

implementation

uses
  SysUtils, nyx.types, nyx.model, nyx.behavior, nyx.events, nyx.scheduler;

type
  { Closed request phase gates duplicate/reentrant replies and UI actions.
    Completion/cancellation revokes receivers before target objects retire. }
  TActivationPhase = (sapInitial, sapSaving, sapPrompt, sapRetired);

  { Forward declaration for weak per-attempt/prompt callbacks below. The base
    owns the detached payload and pending lease, never an application or tree. }
  TActivatedRequest = class;

  { Borrowed per-attempt receiver, retained by its request and active call stack.
    Disconnect precedes cancelling the host token; proper hosts then relinquish
    their borrowed method. An inline duplicate sees its revoked Owner. }
  TAttempt = class(TInterfacedObject)
  public
    Owner: TActivatedRequest;
    procedure Reply(const AResult: TNyxFileSaveResult);
  end;

  { Prompt event adapter with a weak request pointer. The request owns a managed
    lease alongside its subscription, revoking Owner before subscription cancel. }
  TChoice = class(TNyxEventCallback)
  public
    Owner: TActivatedRequest;
    Save: Boolean;
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

  { A delayed UI observation owns a short request lease. Admitted but skipped
    work cancels the request, including shutdown. Work posts at most one next
    observation; the request does not own the work, so no permanent cycle forms. }
  TWatch = class(TInterfacedObject, INyxWork)
  public
    Owner: TActivatedRequest;
    Lease: INyxFileSaveRequest;
    Entered: Boolean;
    Queued: Boolean;
    destructor Destroy; override;
    procedure Execute(const AExecution: INyxExecution);
  end;

  { Independent UI-only wrapper operation. Owns the runtime host/factory, one
    prompt, its weak subscriptions and the current host request. It can preserve
    prepared bytes across an activation refusal without rerunning computation. }
  TActivatedRequest = class(TNyxFileSaveRequest)
  private
    FHost: INyxFileSaveHost;
    FPrompts: INyxSaveActivationPrompts;
    FPolicy: TNyxSaveActivationPolicy;
    FPhase: TActivationPhase;
    FInner: INyxFileSaveRequest;
    FAttempt: TAttempt;
    FAttemptLease: IInterface;
    FPrompt: INyxConfirmation;
    FSaveChoice: TChoice;
    FCancelChoice: TChoice;
    FSaveLease: INyxEventCallback;
    FCancelLease: INyxEventCallback;
    FSaveSubscription: INyxEventSubscription;
    FCancelSubscription: INyxEventSubscription;
    FWatching: Boolean;
    FPresenting: Boolean;
    procedure ClearAttempt;
    procedure ClearChoices;
    procedure SaveNow;
    procedure Received(const AResult: TNyxFileSaveResult);
    procedure Ask;
    procedure Choose(ASave: Boolean);
    procedure Watch;
  protected
    procedure EndHost; override;
  public
    constructor Create(const AOptions: TNyxFileSaveOptions; const ABytes: TNyxBytes;
      const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
      AReply: TNyxFileSaveReply; const AHost: INyxFileSaveHost;
      const APrompts: INyxSaveActivationPrompts; APolicy: TNyxSaveActivationPolicy);
    procedure Start;
  end;

  { Reference-counted immutable runtime decorator. Each call creates its own
    pending payload/prompt; this host retains no active request or application. }
  TActivatedHost = class(TInterfacedObject, INyxFileSaveHost)
  public
    Host: INyxFileSaveHost;
    Prompts: INyxSaveActivationPrompts;
    Policy: TNyxSaveActivationPolicy;
    function BeginSave(const AOptions: TNyxFileSaveOptions; const ABytes: TNyxBytes;
      const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
      AReply: TNyxFileSaveReply): INyxFileSaveRequest;
  end;

function NewNyxSaveActivationContent(const AID: TNyxText;
  const AOptions: TNyxFileSaveOptions; AByteCount: Integer): INyxConfirmationDialog;
begin
  AOptions.Validate;

  if (AByteCount < 0) or (AByteCount > AOptions.ByteLimit) then
  begin
    raise ENyxModel.Create('Prepared save bytes exceed their options');
  end;
  Result := NewNyxConfirmationDialog(AID);
  Result.TitleHeading.WithText('Your file is ready');
  Result.DescriptionLabel.WithText('Save ' + AOptions.SuggestedName + ' (' +
    TNyxText(IntToStr(AByteCount)) + ' bytes). Choose Save now to continue.');
  Result.ActionsConfirmButton.WithText('Save now');
  Result.ActionsCancelButton.WithText('Cancel');
  Result.ActionsCancelButton.Configure.Variant(nvDefault).Done;
end;

procedure TAttempt.Reply(const AResult: TNyxFileSaveResult);
var
  LLease: IInterface;
  LOwner: TActivatedRequest;
begin
  LLease := Self;
  LOwner := Owner;

  if LOwner <> nil then
  begin
    LOwner.Received(AResult);
  end;
end;

procedure TChoice.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
var
  LOwner: TActivatedRequest;
begin
  LOwner := Owner;

  if LOwner <> nil then
  begin
    LOwner.Choose(Save);
  end;
end;

destructor TWatch.Destroy;
begin

  if Queued and not Entered and (Lease <> nil) then
  begin
    Lease.Cancel;
  end;
  inherited Destroy;
end;

procedure TWatch.Execute(const AExecution: INyxExecution);
begin
  Entered := True;
  Owner.FWatching := False;

  try

    if not AExecution.Cancelled then
    begin
      Owner.Watch;
    end
    else
    begin
      Lease.Cancel;
    end;
  except
    { Losing a live observation must resolve the waiting graph as failure, while
      scope retirement remains silent. Preserve the scheduler failure receipt. }
    Owner.Complete(nfsoFailed, Owner.FOptions.SuggestedName, 0,
      'The save activation observation could not continue');
    raise;
  end;
end;

constructor TActivatedRequest.Create(const AOptions: TNyxFileSaveOptions;
  const ABytes: TNyxBytes; const AScheduler: INyxScheduler;
  const ACancellation: INyxCancellationScope; AReply: TNyxFileSaveReply;
  const AHost: INyxFileSaveHost; const APrompts: INyxSaveActivationPrompts;
  APolicy: TNyxSaveActivationPolicy);
begin
  inherited Create(AOptions, ABytes, AScheduler, ACancellation, AReply);
  FHost := AHost;
  FPrompts := APrompts;
  FPolicy := APolicy;
end;

procedure TActivatedRequest.ClearAttempt;
var
  LInner: INyxFileSaveRequest;
begin

  if FAttempt <> nil then
  begin
    FAttempt.Owner := nil;
  end;
  LInner := FInner;
  FInner := nil;

  if LInner <> nil then
  begin
    LInner.Cancel;
  end;
  FAttempt := nil;
  FAttemptLease := nil;
end;

procedure TActivatedRequest.ClearChoices;
begin

  if FSaveChoice <> nil then
  begin
    FSaveChoice.Owner := nil;
  end;

  if FCancelChoice <> nil then
  begin
    FCancelChoice.Owner := nil;
  end;

  if FSaveSubscription <> nil then
  begin
    FSaveSubscription.Cancel;
  end;

  if FCancelSubscription <> nil then
  begin
    FCancelSubscription.Cancel;
  end;
  FSaveSubscription := nil;
  FCancelSubscription := nil;
  FSaveChoice := nil;
  FCancelChoice := nil;
  FSaveLease := nil;
  FCancelLease := nil;
end;

procedure TActivatedRequest.EndHost;
var
  LPrompt: INyxConfirmation;
begin
  FPhase := sapRetired;
  ClearAttempt;
  ClearChoices;

  if not FPresenting then
  begin
    LPrompt := FPrompt;
    FPrompt := nil;

    if LPrompt <> nil then
    begin
      LPrompt.Close;
    end;
  end;
end;

procedure TActivatedRequest.Watch;
var
  LWatch: TWatch;
  LWork: INyxWork;
  LDelays: INyxSchedulerDelays;
begin

  if not Pending then
  begin
    Cancel;
    Exit;
  end;

  if FWatching then
  begin
    Exit;
  end;
  { Admit optional capability before creating a lease-bearing courier. Failed
    initial admission must report failure, not silently cancel its receiver. }
  LDelays := NyxSchedulerDelays(FScheduler);
  LWatch := TWatch.Create;
  LWork := LWatch;
  LWatch.Owner := Self;
  LWatch.Lease := Self;
  LDelays.PostAfter(LWork, 50);
  LWatch.Queued := True;
  FWatching := True;
end;

procedure TActivatedRequest.Ask;
var
  LPrompt: INyxConfirmation;
  LOptions: TNyxConfirmationOptions;
begin
  ClearAttempt;
  FPhase := sapPrompt;
  Watch;

  if not Pending then
  begin
    Cancel;
    Exit;
  end;

  if FPrompt = nil then
  begin
    LPrompt := FPrompts.CreatePrompt(FOptions.Copy, Length(FBytes));

    if LPrompt = nil then
    begin
      raise ENyxModel.Create('Save activation requires a presentation');
    end;

    if LPrompt.IsOpen or (LPrompt.Result <> ncrNotOpened) then
    begin
      { A borrowed/reused presenter is not this request's independent resource.
        Refuse before subscribing or closing somebody else's presentation. }
      raise ENyxModel.Create('Save activation requires a fresh independent presentation');
    end;

    if not Pending then
    begin
      LPrompt.Close;
      Exit;
    end;
    FPrompt := LPrompt;
    FSaveChoice := TChoice.Create;
    FSaveLease := FSaveChoice;
    FSaveChoice.Owner := Self;
    FSaveChoice.Save := True;
    FCancelChoice := TChoice.Create;
    FCancelLease := FCancelChoice;
    FCancelChoice.Owner := Self;
    FSaveSubscription := FPrompt.OnConfirm.Policy(neSequential).Subscribe(FSaveLease);
    FCancelSubscription := FPrompt.OnCancel.Policy(neSequential).Subscribe(FCancelLease);
  end;
  LOptions := FPrompts.PromptOptions(FOptions.Copy);

  if not Pending then
  begin
    Cancel;
    Exit;
  end;
  LPrompt := FPrompt;
  FPresenting := True;
  try
    LPrompt.Open(LOptions);
  finally
    FPresenting := False;
  end;

  if not Pending then
  begin
    EndHost;
  end;
end;

procedure TActivatedRequest.Received(const AResult: TNyxFileSaveResult);
var
  LLease: INyxFileSaveRequest;
begin
  LLease := Self;

  if (FPhase <> sapSaving) or not Pending then
  begin

    if not Pending then
    begin
      Cancel;
    end;
    Exit;
  end;
  try
    ValidateNyxFileSaveResult(AResult, Length(FBytes));
    ClearAttempt;

    if AResult.Outcome = nfsoActivationRequired then
    begin
      Ask;
    end
    else
    begin
      Complete(AResult.Outcome, AResult.Name, AResult.ByteCount, AResult.Error);
    end;
  except
    Complete(nfsoFailed, FOptions.SuggestedName, 0,
      'The save host or activation presentation could not complete');
  end;
end;

procedure TActivatedRequest.SaveNow;
var
  LAttemptLease: IInterface;
  LAttempt: TAttempt;
  LRequest: INyxFileSaveRequest;
begin

  if not Pending then
  begin
    Cancel;
    Exit;
  end;
  FPhase := sapSaving;
  LAttempt := TAttempt.Create;
  LAttemptLease := LAttempt;
  LAttempt.Owner := Self;
  FAttempt := LAttempt;
  FAttemptLease := LAttemptLease;
  try
    { This call stays in the fresh synchronous Nyx confirmation callback. }
    LRequest := FHost.BeginSave(FOptions.Copy, FBytes, FScheduler, FScope, LAttempt.Reply);

    if Pending and (FPhase = sapSaving) then
    begin

      if LRequest = nil then
      begin
        raise ENyxModel.Create('Pending save host returned no cancellation token');
      end;
      FInner := LRequest;
    end
    else if LRequest <> nil then
    begin
      LRequest.Cancel;
    end;
  except
    Complete(nfsoFailed, FOptions.SuggestedName, 0,
      'The save host could not begin its operation');
  end;
end;

procedure TActivatedRequest.Choose(ASave: Boolean);
var
  LLease: INyxFileSaveRequest;
begin
  FScheduler.RequireUI;
  LLease := Self;

  if (FPhase <> sapPrompt) or not Pending then
  begin

    if not Pending then
    begin
      Cancel;
    end;
    Exit;
  end;

  if ASave then
  begin
    SaveNow;
  end
  else
  begin
    Complete(nfsoCancelled, FOptions.SuggestedName, 0);
  end;
end;

procedure TActivatedRequest.Start;
var
  LLease: INyxFileSaveRequest;
begin
  FScheduler.RequireUI;
  LLease := Self;
  RetainPending;
  try

    if FPolicy = nsapAlwaysPrompt then
    begin
      Ask;
    end
    else
    begin
      SaveNow;
    end;
  except
    Complete(nfsoFailed, FOptions.SuggestedName, 0,
      'The save activation request could not start');
  end;
end;

function TActivatedHost.BeginSave(const AOptions: TNyxFileSaveOptions;
  const ABytes: TNyxBytes; const AScheduler: INyxScheduler;
  const ACancellation: INyxCancellationScope;
  AReply: TNyxFileSaveReply): INyxFileSaveRequest;
var
  LRequest: TActivatedRequest;
begin
  LRequest := TActivatedRequest.Create(AOptions, ABytes, AScheduler,
    ACancellation, AReply, Host, Prompts, Policy);
  Result := LRequest;
  LRequest.Start;
end;

function NewNyxActivatedFileSaveHost(const AHost: INyxFileSaveHost;
  const APrompts: INyxSaveActivationPrompts;
  APolicy: TNyxSaveActivationPolicy): INyxFileSaveHost;
var
  LHost: TActivatedHost;
begin

  if (AHost = nil) or (APrompts = nil) or
    (Ord(APolicy) < Ord(Low(TNyxSaveActivationPolicy))) or
    (Ord(APolicy) > Ord(High(TNyxSaveActivationPolicy))) then
  begin
    raise ENyxModel.Create('Save activation requires a host, prompt factory and known policy');
  end;
  LHost := TActivatedHost.Create;
  Result := LHost;
  LHost.Host := AHost;
  LHost.Prompts := APrompts;
  LHost.Policy := APolicy;
end;

end.
