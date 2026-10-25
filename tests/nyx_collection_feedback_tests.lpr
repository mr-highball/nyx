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
program nyx_collection_feedback_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.state, nyx.scheduler, nyx.application.context,
  nyx.collections, nyx.collections.registry, nyx.collections.view,
  nyx.collections.view.types, nyx.collections.query, nyx.collections.inspection,
  nyx.collections.feedback
  {$ifdef PAS2JS}, Web{$endif};

type
  { Explicit host owns scalar state and its weak context resolvers. Validation
    probes publication refusal/reentrancy; resolver counts prove retired calls
    do not enter the former host. No physical target is claimed here. }
  THost = class
  public
    State: TNyxState;
    Scope: INyxCancellationScope;
    Store: INyxCollection;
    Reject: Boolean;
    Mutate: Boolean;
    FailNotify: Boolean;
    ResolverCalls: Integer;
    function GetState: TNyxState;
    function GetCollections: INyxCollections;
    function GetGeneration: INyxCancellationScope;
    procedure Validate(ACandidate: TNyxState; AChanges: TNyxStateChanges);
    procedure Changed(AState: TNyxState; AChanges: TNyxStateChanges);
  end;

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create('Feedback: ' + AReason);
  end;
end;

function THost.GetState: TNyxState;
begin
  Inc(ResolverCalls);
  Result := State;
end;

function THost.GetCollections: INyxCollections;
begin
  Inc(ResolverCalls);
  Result := nil;
end;

function THost.GetGeneration: INyxCancellationScope;
begin
  Inc(ResolverCalls);
  Result := Scope;
end;

procedure THost.Validate(ACandidate: TNyxState; AChanges: TNyxStateChanges);
begin

  if Reject then
  begin
    raise ENyxState.Create('Expected feedback validation refusal');
  end;

  if Mutate then
  begin
    Mutate := False;
    Store.Insert(Store.Snapshot.Count, NyxCollectionItem(NyxItem(Store.Snapshot.Key, 'during-validation'))
      .WithValue(NyxTextField('name'), 'New while validating'));
  end;
end;

procedure THost.Changed(AState: TNyxState; AChanges: TNyxStateChanges);
begin

  if FailNotify then
  begin
    raise ENyxState.Create('Expected post-commit receiver failure');
  end;
end;

procedure Run;
const
  CExact: TNyxText = 'A careful failure 🌙' + #0 + #13#10 + 'retry later';
  CLoading: TNyxText = 'Loading 🌙';
var
  LHost: THost;
  LLease: INyxApplicationContextLease;
  LView: INyxCollectionView;
  LInspection: INyxCollectionInspection;
  LFeedback: INyxCollectionFeedback;
  LParts: TNyxCollectionFeedbackParts;
  LText: TNyxCollectionFeedbackText;
  LCopy: TNyxCollectionFeedbackText;
  LBefore: INyxCollectionSnapshot;
  LObservation: TNyxCollectionFeedbackObservation;
  LToken: TNyxStateSubscription;
  LRevision: Integer;
  LCalls: Integer;
  LRefused: Boolean;
begin
  LHost := THost.Create;
  LToken := nil;
  try
    LHost.State := TNyxState.Create;
    LHost.Scope := NewNyxCancellationScope;
    LHost.Store := NewNyxCollection(NyxCollection('feedback'),
      NyxCollectionSchema.Text(NyxTextField('name'), ''), []);
    LParts := NyxCollectionFeedbackParts('records-feedback');
    LParts.Define(LHost.State);
    LText := TNyxCollectionFeedbackText.Defaults;
    LCopy := LText.WithHeading(ncfkLoading, CLoading);
    Check(LText.Heading(ncfkLoading) <> LCopy.Heading(ncfkLoading),
      'localizable text builders do not mutate an earlier policy');
    LLease := NewNyxApplicationContext(NewNyxScheduler, LHost.GetState,
      LHost.GetCollections, LHost.GetGeneration);
    LView := NewNyxCollectionView(LHost.Store, NyxCollectionView(LHost.Store.Snapshot.Key)
      .Column(NyxTextField('name'), 'Name'), cpTable);
    LInspection := NyxCollectionInspection(LView);
    LInspection.ConfigureInspection(NyxInspectionPolicy);
    LFeedback := BindNyxCollectionFeedback(LLease.Context, LView, LParts, LCopy,
      [ncfaRetry, ncfaReset, ncfaResume]);
    Check((LFeedback.Capture.Kind = ncfkEmpty) and LFeedback.Capture.CanRetry,
      'a ready empty source has a distinct retryable presentation');
    LBefore := LHost.Store.Snapshot;
    LFeedback.Loading;
    Check((LFeedback.Capture.Kind = ncfkLoading) and LFeedback.Capture.Busy and
      (LHost.State.GetValue(LParts.Caption) = CLoading) and (LHost.Store.Snapshot = LBefore),
      'loading atomically binds a custom heading/activity without changing source');
    LHost.Store.Insert(0, NyxCollectionItem(NyxItem(LHost.Store.Snapshot.Key, 'first'))
      .WithValue(NyxTextField('name'), 'First'));
    LFeedback.Ready;
    Check((LFeedback.Capture.Kind = ncfkReady) and
      (LHost.State.GetValue(LParts.PageRows) = 1) and not LFeedback.Capture.Busy,
      'loaded data and numeric count bindings become ready');
    LFeedback.Refreshing;
    Check((LFeedback.Capture.Kind = ncfkRefreshing) and (LView.Snapshot.Count = 1),
      'refreshing keeps the accepted displayed records');
    LFeedback.Failed(CExact);
    Check((LFeedback.Capture.Kind = ncfkFailed) and LFeedback.Capture.CanRetry and
      (LFeedback.Capture.Diagnostic = CExact) and (LView.Snapshot.Count = 1),
      'failure preserves exact supplementary/NUL/line-ending diagnostics and accepted records');
    LFeedback.Ready;
    LView.ConfigureQuery(NyxCollectionQuery.Where(NyxWhere(NyxTextField('name')).EqualTo('Missing')));
    Check((LFeedback.Capture.Kind = ncfkFilteredEmpty) and LFeedback.Capture.CanReset and
      not LFeedback.Capture.CanRetry and (LFeedback.Capture.SourceRows = 1),
      'filtered-empty differs from an empty source and offers only its permitted reset');
    LFeedback.Actions([ncfaRetry, ncfaResume]);
    Check(not LFeedback.Capture.CanReset and (LFeedback.Capture.Kind = ncfkFilteredEmpty),
      'caller can withdraw a reset affordance without clearing another surface filter');
    LFeedback.Actions([ncfaRetry, ncfaReset, ncfaResume]);
    Check(LFeedback.Capture.CanReset, 'typed action choices can be restored independently of membership');
    LView.ConfigureQuery(Default(TNyxCollectionQuery));
    LInspection.PauseInspection;
    LHost.Store.Insert(1, NyxCollectionItem(NyxItem(LHost.Store.Snapshot.Key, 'second'))
      .WithValue(NyxTextField('name'), 'Second'));
    LObservation := LFeedback.Capture;
    Check((LObservation.Kind = ncfkPaused) and LObservation.CanResume and
      (LObservation.SourceRows = 1) and (LObservation.PendingPublications = 1) and
      (LObservation.IncomingRevision > LObservation.DisplayRevision),
      'paused feedback counts its displayed source and reports incoming progress separately');
    LFeedback.Failed(CExact);
    Check((LFeedback.Capture.Kind = ncfkPaused) and
      (LFeedback.Capture.Activity = ncaFailed) and LFeedback.Capture.CanRetry and
      LFeedback.Capture.CanResume and (LFeedback.Capture.Diagnostic = CExact),
      'a producer failure remains visible while pause and resume authority stay independent');
    LInspection.ResumeInspection;
    Check((LFeedback.Capture.Kind = ncfkFailed) and (LFeedback.Capture.SourceRows = 2),
      'resume adopts current counts without inventing a successful retry');
    LFeedback.Ready;
    LToken := LHost.State.Subscribe(LHost.Changed, LHost.Validate);
    LHost.Reject := True;
    LRevision := LHost.State.Revision;
    LObservation := LFeedback.Capture;
    LRefused := False;
    try
      LFeedback.Loading;
    except
      on LException: ENyxState do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LHost.State.Revision = LRevision) and
      (LFeedback.Capture.Serial = LObservation.Serial) and (LFeedback.Capture.Activity = ncaReady),
      'validation veto leaves both scalar state and cached feedback facts unchanged');
    LHost.Reject := False;
    LHost.Mutate := True;
    LRefused := False;
    try
      LFeedback.Loading;
    except
      on LException: ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LHost.State.Revision = LRevision) and (LHost.Store.Snapshot.Count = 3),
      'a reentrant view change refuses mixed feedback facts before scalar installation');
    LFeedback.Refresh;
    Check((LFeedback.Capture.SourceRows = 3) and (LFeedback.Capture.Activity = ncaReady),
      'explicit refresh reconciles current view facts after reentrant refusal');
    LHost.FailNotify := True;
    LRefused := False;
    try
      LFeedback.Failed('Committed failure presentation');
    except
      on LException: ENyxStateNotification do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LFeedback.Capture.Kind = ncfkFailed) and
      (LHost.State.GetValue(LParts.Caption) = LCopy.Heading(ncfkFailed)),
      'post-commit receiver failure reports failure while retaining installed metadata and scalar state');
    LHost.FailNotify := False;
    LFeedback.Ready;
    LLease.Close;
    LCalls := LHost.ResolverCalls;
    LHost.Store.Insert(3, NyxCollectionItem(NyxItem(LHost.Store.Snapshot.Key, 'after-close')));
    Check(not LFeedback.Capture.Active and (LHost.ResolverCalls = LCalls),
      'retired context notification never enters a former borrowed state resolver');
    LFeedback.Close;
    LRefused := False;
    try
      LFeedback.Loading;
    except
      on LException: ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and not LFeedback.Capture.Active, 'closed feedback cannot regain publication authority');
  finally
    LToken.Free;
    LFeedback := nil;
    LInspection := nil;
    LView := nil;
    LLease := nil;
    LHost.Scope.Cancel;
    LHost.State.Free;
    LHost.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-feedback-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' collection feedback checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-feedback-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
