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

program nyx_popover_open_lifetime_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifndef PAS2JS}{$ifdef UNIX}cthreads,{$endif}{$endif}
  SysUtils,
  nyx.text,
  nyx.types,
  nyx.root.types,
  nyx.model,
  nyx.controls,
  nyx.behavior,
  nyx.events,
  nyx.scheduler,
  nyx.popover,
  nyx.collections,
  nyx.collections.view,
  nyx.collections.view.types,
  nyx.collections.arrangement,
  nyx.collections.query.controls,
  nyx.collections.mount,
  nyx.collections.arrangement.workbench,
  nyx.test.collections.view
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

type
  { This concrete presenter exercises the actual shared Open/Finish/nesting
    lifecycle. Only the borrowed physical presentation boundary is instrumented;
    it does not claim DOM/LCL focus, physical dismissal or painting coverage. }
  TTestPopover = class;

  { Synchronous UI hooks borrow the entered presenter only. Test receivers live
    longer than every presenter that references them and own no subscription. }
  TPresentationHook = procedure(APresenter: TTestPopover) of object;

  { The caller owns these counters beyond all presenter and token releases.
    Destruction proves that a retained open token owns no presenter or router. }
  TPresentationCounts = class
  public
    Presents: Integer;
    Conceals: Integer;
    Destroyed: Integer;
  end;

  { An ordinary independent document/router presenter with an observable native
    or browser-neutral physical boundary. A failed Present deliberately leaves a
    partial host visible so the real base lifecycle must attempt its cleanup. }
  TTestPopover = class(TNyxPopoverPresenter)
  private
    FRouter: INyxEvents;
    FCounts: TPresentationCounts;
    FPhysicalOpen: Boolean;
  protected
    procedure Present(const AOptions: TNyxPopoverOptions;
      const AFocusID: TNyxText); override;
    procedure Conceal(ARestoreFocus: Boolean); override;
    function CanNestWithin(AParent: TNyxPopoverPresenter): Boolean; override;
  public
    OnPresent: TPresentationHook;
    OnConceal: TPresentationHook;
    FailPresent: Boolean;
    FailConceal: Boolean;
    { A target may retire its physical lease during a legitimate cleanup hook. }
    RetireOnConceal: Boolean;
    CaptureHiddenDuringPresent: Boolean;
    CaptureHiddenDuringConceal: Boolean;
    constructor Create(ADocument: TNyxDocument; ACounts: TPresentationCounts);
    destructor Destroy; override;
    function GetEvents: INyxEvents; override;
    procedure RetirePresentation;
    property PhysicalOpen: Boolean read FPhysicalOpen;
  end;

  { Shared cleanup receiver retains immutable token values, never presenters.
    Parent/Child are fixture-borrowed UI aliases used only for synchronous
    reentry tests; their owning interfaces outlive this receiver's callbacks. }
  TCleanupReview = class
  public
    Tokens: array of INyxPopoverOpenLifetime;
    Parent: TTestPopover;
    Child: TTestPopover;
    Observed: Integer;
    ReopenRefused: Boolean;
    procedure Concealing(APresenter: TTestPopover);
    procedure CloseParentDuringPresent(APresenter: TTestPopover);
  end;

  { Completion holds only borrowed test owners. The subscription is cancelled
    before these aliases disappear, avoiding the application presenter cycle
    forbidden by the public popover contract. Exceptions are recorded explicitly
    because an event scheduler may retain a failed execution rather than raise. }
  TReopenCompletion = class(TNyxEventCallback)
  public
    Popup: TTestPopover;
    Previous: INyxPopoverOpenLifetime;
    Replacement: INyxPopoverOpenLifetime;
    Calls: Integer;
    Reopen: Boolean;
    Failure: TNyxText;
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;

  { This deliberately reentrant UI-only test scope models an underlying final
    cancellation getter that silently closes/reopens the actual presentation.
    It is not a worker-safe popup wrapper: the test reads its retained pure
    token AFTER this last outward getter, as the intended final fence must. }
  TReopeningCancellation = class(TInterfacedObject, INyxCancellationScope)
  private
    FPresentation: INyxPopover;
    FCancelled: Boolean;
  public
    constructor Create(const APresentation: INyxPopover);
    procedure Cancel;
    function GetCancelled: Boolean;
  end;

  { Controlled readonly input facts isolate generation composition. These flags
    do not simulate a mounted table or prove physical input suspension; the
    actual shared presenter and its real event router own the open/close side. }
  TQueryInputFacts = class(TInterfacedObject, INyxCollectionInputLifetime)
  public
    InputReady: Boolean;
    InputEpoch: Integer;
    function GetCurrent: Boolean;
    function GetReady: Boolean;
    function GetRetired: Boolean;
    function GetEpoch: Integer;
  end;

  { Only construction is exercised here. The real shared mount owns its view,
    input admission and query controller; there is no pretend physical widget. }
  TConstructionMount = class(TNyxCollectionMountBase)
  public
    Detachments: Integer;
  protected
    procedure RenderDataset; override;
    procedure DetachTarget; override;
  end;

  { Two adversarial callbacks distinguish recursive admission from teardown
    during the exact same actual shared construction path. }
  TConstructionCallback = (ccRecursive, ccDisconnect);

  { The fixture owns this factory and its borrowed mount alias through every
    callback. The accepted mount may retain the factory, so the alias is weak
    and Revoke clears it before any owner retires. No form is needed to qualify
    constructor admission; an accidental request for one refuses explicitly.
    One injected post-revocation failure proves independent cleanup continues. }
  TConstructionFactory = class(TInterfacedObject,
    INyxQueryFormPresenterFactory, INyxCollectionQueryInputFactory)
  public
    Mount: TConstructionMount;
    Router: INyxEvents;
    Input: INyxCollectionInputLifetime;
    Kind: TConstructionCallback;
    Calls: Integer;
    FormCalls: Integer;
    RecursiveRefused: Boolean;
    Revoked: Boolean;
    FailRevoke: Boolean;
    procedure ConfigureInput(const AInput: INyxCollectionInputLifetime);
    function CreateForm(AColumn: Integer; ADocument: TNyxDocument;
      const ARoot: TNyxRootRef): INyxQueryFormPresenter;
    procedure Revoke;
  end;

  {$ifndef PAS2JS}
  { The worker owns only a readonly retained token. No UI, router, presenter,
    document or scheduler is read; the UI joins it before changing its inputs. }
  TOpenLifetimeWorker = class(TThread)
  private
    FToken: INyxPopoverOpenLifetime;
    FExpected: Boolean;
  protected
    procedure Execute; override;
  public
    Passed: Boolean;
    Failure: TNyxText;
    constructor Create(const AToken: INyxPopoverOpenLifetime; AExpected: Boolean);
  end;
  {$endif}

var
  GChecks: Integer;

function TQueryInputFacts.GetCurrent: Boolean;
begin
  Result := True;
end;

function TQueryInputFacts.GetReady: Boolean;
begin
  Result := InputReady;
end;

function TQueryInputFacts.GetRetired: Boolean;
begin
  Result := False;
end;

function TQueryInputFacts.GetEpoch: Integer;
begin
  Result := InputEpoch;
end;

procedure TConstructionMount.RenderDataset;
begin
  { This consumer qualifies shared construction only, not target painting. }
end;

procedure TConstructionMount.DetachTarget;
begin
  Inc(Detachments);
end;

procedure TConstructionFactory.ConfigureInput(
  const AInput: INyxCollectionInputLifetime);
begin
  Inc(Calls);
  Input := AInput;

  if Kind = ccDisconnect then
  begin
    Mount.Disconnect;
    Exit;
  end;
  try
    Mount.ConfigureQueries(Self, Router, NyxControlEvents('construction-table'));
  except
    on ENyxCollection do
    begin
      RecursiveRefused := True;
    end;
  end;
end;

function TConstructionFactory.CreateForm(AColumn: Integer;
  ADocument: TNyxDocument; const ARoot: TNyxRootRef): INyxQueryFormPresenter;
begin
  Result := nil;
  Inc(FormCalls);
  raise ENyxCollection.Create('Construction must not open a physical query form');
end;

procedure TConstructionFactory.Revoke;
begin
  Revoked := True;
  Mount := nil;
  Router := nil;
  Input := nil;

  if FailRevoke then
  begin
    FailRevoke := False;
    raise ENyxCollection.Create('Injected factory retirement failure');
  end;
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxModel.Create('Popover open lifetime: ' + AReason);
  end;
  Inc(GChecks);
end;

function Admission(const APopup: INyxPopover): INyxPopoverOpenAdmission;
begin
  Result := nil;
  Check(Supports(APopup, INyxPopoverOpenAdmission, Result),
    'shared presenter advertises the optional open-lifetime facet');
end;

function Capture(const APopup: INyxPopover): INyxPopoverOpenLifetime;
var
  LAdmission: INyxPopoverOpenAdmission;
begin
  LAdmission := Admission(APopup);
  Result := LAdmission.GetOpenLifetime;
end;

function NewPopup(ACounts: TPresentationCounts;
  out ARaw: TTestPopover): INyxPopover;
var
  LDocument: TNyxDocument;
  LPage: INyxPage;
begin
  LDocument := TNyxDocument.Create;
  try
    LPage := NewNyxPage('lifetime-page');
    LPage.Add(NewNyxLabel('lifetime-label').WithText('Retained lifetime / 🌙'));
    LDocument.AddPage(LPage);
    ARaw := TTestPopover.Create(LDocument, ACounts);
    Result := ARaw;
  finally
    LDocument.Free;
  end;
end;

constructor TTestPopover.Create(ADocument: TNyxDocument;
  ACounts: TPresentationCounts);
begin
  FCounts := ACounts;
  inherited Create(ADocument, NyxPageRoot('lifetime-page'));
  FRouter := NewNyxEvents;
end;

destructor TTestPopover.Destroy;
begin
  { Real platform subclasses revoke their contextual family before freeing
    target resources. This test descendant observes that same target seam. }
  RetireNesting;
  FPhysicalOpen := False;

  if FRouter <> nil then
  begin
    FRouter.Close;
  end;
  FRouter := nil;

  if FCounts <> nil then
  begin
    Inc(FCounts.Destroyed);
  end;
  inherited Destroy;
end;

function TTestPopover.GetEvents: INyxEvents;
begin
  Result := FRouter;
end;

procedure TTestPopover.RetirePresentation;
begin
  { This is the real protected target-retirement seam, not a token setter.
    Platform adapters use it before disposing their physical presentation. }
  RetireNesting;
  FPhysicalOpen := False;
end;

function TTestPopover.CanNestWithin(AParent: TNyxPopoverPresenter): Boolean;
begin
  Result := AParent is TTestPopover;
end;

procedure TTestPopover.Present(const AOptions: TNyxPopoverOptions;
  const AFocusID: TNyxText);
begin
  Inc(FCounts.Presents);
  FPhysicalOpen := True;
  CaptureHiddenDuringPresent := Capture(Self) = nil;

  if Assigned(OnPresent) then
  begin
    OnPresent(Self);
  end;

  if FailPresent then
  begin
    raise ENyxModel.Create('Expected partial Present failure');
  end;
end;

procedure TTestPopover.Conceal(ARestoreFocus: Boolean);
begin
  Inc(FCounts.Conceals);
  FPhysicalOpen := False;
  CaptureHiddenDuringConceal := Capture(Self) = nil;

  if RetireOnConceal then
  begin
    RetirePresentation;
  end;

  if Assigned(OnConceal) then
  begin
    OnConceal(Self);
  end;

  if FailConceal then
  begin
    raise ENyxModel.Create('Expected secondary Conceal failure');
  end;
end;

procedure TCleanupReview.Concealing(APresenter: TTestPopover);
var
  LIndex: Integer;
begin
  Inc(Observed);
  for LIndex := 0 to High(Tokens) do
  begin
    Check((Tokens[LIndex] <> nil) and not Tokens[LIndex].Current,
      'complete captured family is unavailable BEFORE the first child cleanup callback');
  end;
  Check(not APresenter.IsOpen and (Capture(APresenter) = nil),
    'cleanup cannot capture an open or transitioning presentation');

  if (Observed = 1) and (Parent <> nil) and (Child <> nil) then
  begin
    try
      Child.Open(NyxPopover('Cannot reopen inside the closing parent'));
    except
      on ENyxModel do
      begin
        ReopenRefused := True;
      end;
    end;
  end;
end;

procedure TCleanupReview.CloseParentDuringPresent(APresenter: TTestPopover);
begin
  Parent.Close;
end;

procedure TReopenCompletion.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  try
    Inc(Calls);
    Check(NyxPopoverDismissReason(AEvent) = nprOutsidePress,
      'Dismiss retains its actual typed completion reason');
    Check((Previous <> nil) and not Previous.Current and not Popup.IsOpen and
      (Capture(Popup) = nil),
      'actual Dismiss revokes its token before the first completion callback');

    if Reopen then
    begin
      Popup.Open(NyxPopover('Reopened by actual completion'));
      Replacement := Capture(Popup);
      Check((Replacement <> nil) and Replacement.Current and
        (Replacement <> Previous) and not Previous.Current,
        'completion reopening creates new authority without reviving the old token');
    end;
  except
    on LException: Exception do
    begin
      Failure := TNyxText(LException.Message);
    end;
  end;
end;

constructor TReopeningCancellation.Create(const APresentation: INyxPopover);
begin
  inherited Create;
  FPresentation := APresentation;
end;

procedure TReopeningCancellation.Cancel;
begin
  FCancelled := True;
end;

function TReopeningCancellation.GetCancelled: Boolean;
var
  LPresentation: INyxPopover;
begin
  LPresentation := FPresentation;
  LPresentation.Close;
  LPresentation.Open(NyxPopover('Reopened inside final external getter'));
  Result := FCancelled;
end;

{$ifndef PAS2JS}
constructor TOpenLifetimeWorker.Create(const AToken: INyxPopoverOpenLifetime;
  AExpected: Boolean);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FToken := AToken;
  FExpected := AExpected;
end;

procedure TOpenLifetimeWorker.Execute;
var
  LIndex: Integer;
begin
  try
    for LIndex := 1 to 512 do
    begin

      if (FToken = nil) or (FToken.Current <> FExpected) then
      begin
        raise Exception.Create('Readonly retained token changed unexpectedly');
      end;
    end;
    Passed := True;
  except
    on LException: Exception do
    begin
      Failure := TNyxText(LException.Message);
    end;
  end;
end;
{$endif}

procedure CheckWorker(const AToken: INyxPopoverOpenLifetime; AExpected: Boolean);
{$ifndef PAS2JS}
var
  LWorker: TOpenLifetimeWorker;
{$endif}
begin
  {$ifdef PAS2JS}
  Check((AToken <> nil) and (AToken.Current = AExpected),
    'retained browser token has the expected pure readonly state');
  {$else}
  LWorker := TOpenLifetimeWorker.Create(AToken, AExpected);
  try
    LWorker.Start;
    LWorker.WaitFor;
    Check(LWorker.Passed and (LWorker.Failure = ''),
      'retained token supports actual worker reads without a UI owner: ' + LWorker.Failure);
  finally
    LWorker.Free;
  end;
  {$endif}
end;

procedure SinglePresentationCases;
var
  LCounts: TPresentationCounts;
  LRaw: TTestPopover;
  LPopup: INyxPopover;
  LPresentation: INyxPopover;
  LAdmission: INyxPopoverOpenAdmission;
  LOriginal: INyxPopoverOpenLifetime;
  LReplacement: INyxPopoverOpenLifetime;
  LCompletion: TReopenCompletion;
  LCompletionLease: INyxEventCallback;
  LSubscription: INyxEventSubscription;
  LScope: INyxCancellationScope;
  LCancelled: Boolean;
  LRefused: Boolean;
begin
  LCounts := TPresentationCounts.Create;
  try
    LPopup := NewPopup(LCounts, LRaw);
    LAdmission := Admission(LPopup);
    Check(LAdmission.GetOpenLifetime = nil, 'closed presenter exposes no open lease');
    LCompletion := TReopenCompletion.Create;
    LCompletionLease := LCompletion;
    LCompletion.Popup := LRaw;
    LSubscription := LPopup.OnDismiss.Subscribe(LCompletionLease);
    LPopup.Open(NyxPopover('First open'));
    LOriginal := LAdmission.GetOpenLifetime;
    Check((LOriginal <> nil) and LOriginal.Current and LRaw.PhysicalOpen and
      LRaw.CaptureHiddenDuringPresent,
      'successful Present activates a token only after target presentation returns');
    Check(LAdmission.GetOpenLifetime = LOriginal,
      'repeated capture during one open returns its exact existing token');
    CheckWorker(LOriginal, True);
    LCompletion.Previous := LOriginal;
    { This is the actual platform presentation facade, without an arrangement
      wrapper. Silent raw Close must be visible through the token itself. }
    LPresentation := LPopup;
    LPresentation.Close;
    Check(not LOriginal.Current and not LPopup.IsOpen and not LRaw.PhysicalOpen and
      LRaw.CaptureHiddenDuringConceal and (LCompletion.Calls = 0),
      'raw Presentation.Close silently revokes before Conceal and completion');
    LPopup.Close;
    Check((LCounts.Conceals = 1) and not LOriginal.Current,
      'repeated Close is idempotent and cannot reactivate the old token');
    LPopup.Open(NyxPopover('Second open'));
    LReplacement := Capture(LPopup);
    Check((LReplacement <> nil) and LReplacement.Current and
      (LReplacement <> LOriginal) and not LOriginal.Current,
      'close/reopen ABA creates a distinct token with terminal old authority');
    LCompletion.Previous := LReplacement;
    LCompletion.Reopen := True;
    LPopup.Dismiss(nprOutsidePress);
    Check((LCompletion.Calls = 1) and (LCompletion.Failure = '') and LPopup.IsOpen and
      (LCompletion.Replacement <> nil) and LCompletion.Replacement.Current and
      not LReplacement.Current,
      'physical dismissal callback reentry keeps only the newly opened token current');
    LOriginal := LCompletion.Replacement;
    LScope := TReopeningCancellation.Create(LPresentation);
    LCancelled := LScope.Cancelled;
    Check(not LCancelled and LPopup.IsOpen and not LOriginal.Current and
      Capture(LPopup).Current,
      'final outward cancellation getter cannot conceal actual close/reopen behind IsOpen');
    LReplacement := Capture(LPopup);
    LSubscription.Cancel;
    LSubscription := nil;
    LCompletionLease := nil;
    LScope := nil;
    LRaw.RetirePresentation;
    Check(not LReplacement.Current and not LPopup.IsOpen and
      (LAdmission.GetOpenLifetime = nil) and (LCounts.Destroyed = 0),
      'target retirement revokes authority while a retained provider still exists');
    LRefused := False;
    try
      LPopup.Open(NyxPopover('Retired presentation cannot reopen'));
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and not LReplacement.Current,
      'retirement cannot be reversed by a later Open on the same presenter');
    LAdmission := nil;
    LPresentation := nil;
    LPopup := nil;
    Check((LCounts.Destroyed = 1) and not LReplacement.Current,
      'retained tokens do not keep their presenter alive after actual retirement');
    CheckWorker(LReplacement, False);
    LPopup := NewPopup(LCounts, LRaw);
    LPopup.Open(NyxPopover('Target retires during concealment'));
    LReplacement := Capture(LPopup);
    LRaw.RetireOnConceal := True;
    LPopup.Close;
    Check(not LReplacement.Current and not LPopup.IsOpen and
      (Capture(LPopup) = nil),
      'target retirement inside Conceal remains terminal after Finish returns');
    LRefused := False;
    try
      LPopup.Within(nil);
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'retained retired presenter refuses Within without a dead lease read');
    LPopup := nil;
    Check(LCounts.Destroyed = 2,
      'retirement during cleanup leaves no presenter retained by token authority');
  finally

    if LSubscription <> nil then
    begin
      LSubscription.Cancel;
    end;
    LSubscription := nil;
    LCompletionLease := nil;
    LScope := nil;
    LAdmission := nil;
    LPresentation := nil;
    LPopup := nil;
    LCounts.Free;
  end;
end;

procedure NestedPresentationCases;
var
  LCounts: TPresentationCounts;
  LParentRaw: TTestPopover;
  LChildRaw: TTestPopover;
  LGrandchildRaw: TTestPopover;
  LSiblingRaw: TTestPopover;
  LParent: INyxPopover;
  LChild: INyxPopover;
  LGrandchild: INyxPopover;
  LSibling: INyxPopover;
  LReview: TCleanupReview;
  LOldParent: INyxPopoverOpenLifetime;
  LNewParent: INyxPopoverOpenLifetime;
  LNewChild: INyxPopoverOpenLifetime;
begin
  LCounts := TPresentationCounts.Create;
  LReview := TCleanupReview.Create;
  try
    LParent := NewPopup(LCounts, LParentRaw);
    LChild := NewPopup(LCounts, LChildRaw);
    LGrandchild := NewPopup(LCounts, LGrandchildRaw);
    LSibling := NewPopup(LCounts, LSiblingRaw);
    LParent.Open(NyxPopover('Parent'));
    LChild.Within(LParent);
    LChild.Open(NyxPopover('Child'));
    LGrandchild.Within(LChild);
    LGrandchild.Open(NyxPopover('Grandchild'));
    LSibling.Within(LParent);
    LSibling.Open(NyxPopover('Sibling'));
    SetLength(LReview.Tokens, 4);
    LReview.Tokens[0] := Capture(LParent);
    LReview.Tokens[1] := Capture(LChild);
    LReview.Tokens[2] := Capture(LGrandchild);
    LReview.Tokens[3] := Capture(LSibling);
    LOldParent := LReview.Tokens[0];
    LReview.Parent := LParentRaw;
    LReview.Child := LChildRaw;
    LParentRaw.OnConceal := LReview.Concealing;
    LChildRaw.OnConceal := LReview.Concealing;
    LGrandchildRaw.OnConceal := LReview.Concealing;
    LSiblingRaw.OnConceal := LReview.Concealing;
    LParent.Close;
    Check((LReview.Observed = 4) and LReview.ReopenRefused and
      not LParent.IsOpen and not LChild.IsOpen and
      not LGrandchild.IsOpen and not LSibling.IsOpen,
      'nested close revokes the entire family before cleanup and refuses closing-parent reentry');
    LParentRaw.OnConceal := nil;
    LChildRaw.OnConceal := nil;
    LGrandchildRaw.OnConceal := nil;
    LSiblingRaw.OnConceal := nil;
    LParent.Open(NyxPopover('Fresh parent'));
    LChild.Open(NyxPopover('Fresh child'));
    LNewParent := Capture(LParent);
    LNewChild := Capture(LChild);
    Check(LNewParent.Current and LNewChild.Current and not LOldParent.Current and
      not LReview.Tokens[1].Current and not LReview.Tokens[2].Current and
      not LReview.Tokens[3].Current,
      'parent reopen does not revive descendants captured against its old per-open token');
    LParent := nil;
    Check((LCounts.Destroyed = 1) and not LNewParent.Current and not LNewChild.Current and
      not LChild.IsOpen,
      'child and its token do not own the parent; parent retirement revokes the fresh child');
    CheckWorker(LNewChild, False);
  finally

    if LParent <> nil then
    begin
      LParentRaw.OnConceal := nil;
    end;

    if LChild <> nil then
    begin
      LChildRaw.OnConceal := nil;
    end;

    if LGrandchild <> nil then
    begin
      LGrandchildRaw.OnConceal := nil;
    end;

    if LSibling <> nil then
    begin
      LSiblingRaw.OnConceal := nil;
    end;
    LGrandchild := nil;
    LSibling := nil;
    LChild := nil;
    LParent := nil;
    LReview.Free;
    LCounts.Free;
  end;
end;

procedure FailedPresentationCases;
var
  LCounts: TPresentationCounts;
  LRaw: TTestPopover;
  LParentRaw: TTestPopover;
  LPopup: INyxPopover;
  LParent: INyxPopover;
  LAdmission: INyxPopoverOpenAdmission;
  LReview: TCleanupReview;
  LRefused: Boolean;
begin
  LCounts := TPresentationCounts.Create;
  LReview := TCleanupReview.Create;
  try
    LPopup := NewPopup(LCounts, LRaw);
    LAdmission := Admission(LPopup);
    LRaw.FailPresent := True;
    LRefused := False;
    try
      LPopup.Open(NyxPopover('Partial presentation fails'));
    except
      on LException: ENyxModel do
      begin
        LRefused := Pos('Expected partial Present failure', LException.Message) > 0;
      end;
    end;
    Check(LRefused and not LPopup.IsOpen and not LRaw.PhysicalOpen and
      (LAdmission.GetOpenLifetime = nil) and (LCounts.Conceals = 1),
      'failed Present cleans its partial host and publishes no successful open token');
    LRaw.FailConceal := True;
    LRefused := False;
    try
      LPopup.Open(NyxPopover('Both presentation and cleanup fail'));
    except
      on LException: ENyxModel do
      begin
        LRefused := Pos('Expected partial Present failure', LException.Message) > 0;
      end;
    end;
    Check(LRefused and not LPopup.IsOpen and not LRaw.PhysicalOpen and
      (LAdmission.GetOpenLifetime = nil) and (LCounts.Conceals = 2),
      'secondary failed-Present cleanup cannot replace the primary failure or publish authority');
    LRaw.FailPresent := False;
    LRaw.FailConceal := False;
    LPopup.Open(NyxPopover('Successful retry'));
    Check(LPopup.IsOpen and Capture(LPopup).Current,
      'failed presentation leaves no stuck transition preventing a later valid open');
    LPopup.Close;
    LParent := NewPopup(LCounts, LParentRaw);
    LParent.Open(NyxPopover('Parent can close inside child Present'));
    LPopup.Within(LParent);
    LReview.Parent := LParentRaw;
    LRaw.OnPresent := LReview.CloseParentDuringPresent;
    LRefused := False;
    try
      LPopup.Open(NyxPopover('Present loses its original parent'));
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and not LPopup.IsOpen and not LRaw.PhysicalOpen and
      not LParent.IsOpen and (LAdmission.GetOpenLifetime = nil),
      'parent retirement inside Present refuses child adoption and cleans its partial target');
    LRaw.OnPresent := nil;
  finally

    if LPopup <> nil then
    begin
      LRaw.OnPresent := nil;
    end;
    LAdmission := nil;
    LPopup := nil;
    LParent := nil;
    LReview.Free;
    LCounts.Free;
  end;
end;

{ The forwarding surface must combine the real successful-open identity with
  its original router scope and independent input epoch. No wrapper Close hook,
  replacement router or second scheduler participates in these checks. }
procedure QueryGenerationCases;
var
  LCounts: TPresentationCounts;
  LRaw: TTestPopover;
  LPopup: INyxPopover;
  LAdmission: INyxPopoverOpenAdmission;
  LInputObject: TQueryInputFacts;
  LInput: INyxCollectionInputLifetime;
  LMain: INyxEvents;
  LForm: INyxEvents;
  LUnderlying: INyxCancellationScope;
  LMainGeneration: INyxCancellationScope;
  LPrevious: INyxCancellationScope;
  LCurrent: INyxCancellationScope;
  LReady: INyxCancellationReadiness;
  LRefused: Boolean;
begin
  LCounts := TPresentationCounts.Create;
  try
    LPopup := NewPopup(LCounts, LRaw);
    LAdmission := Admission(LPopup);
    LInputObject := TQueryInputFacts.Create;
    LInput := LInputObject;
    LInputObject.InputReady := True;
    LInputObject.InputEpoch := 1;
    LUnderlying := NyxEventGeneration(LPopup.Events);
    LMain := NewNyxArrangementQueryEvents(LPopup.Events, LInput);
    LMainGeneration := NyxEventGeneration(LMain);
    Check(not LMainGeneration.Cancelled,
      'main query capture does not require a form to be open');
    LForm := NewNyxArrangementQueryEvents(LPopup.Events, LInput, LAdmission);
    Check((LForm.Scheduler = LPopup.Events.Scheduler) and
      (LForm.ViewRevision = LPopup.Events.ViewRevision),
      'form forwarding retains the actual router scheduler and revision');
    LRefused := False;
    try
      LCurrent := NyxEventGeneration(LForm);
    except
      on ENyxSchedule do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'closed form refuses query generation capture');
    LPopup.Open(NyxPopover('Query lifetime'));
    LPrevious := NyxEventGeneration(LForm);
    Check(not LPrevious.Cancelled,
      'successful presentation admits a query generation');
    Check(Supports(LPrevious, INyxCancellationReadiness, LReady),
      'captured form generation exposes callback-free final readiness');
    LReady.CheckReady;
    LPopup.Close;
    LRefused := False;
    try
      LReady.CheckReady;
    except
      on ENyxSchedule do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused,
      'final readiness observes raw Close before any later Cancelled sample');
    Check(LPrevious.Cancelled and not LUnderlying.Cancelled and
      not LMainGeneration.Cancelled,
      'raw silent Close cancels only the captured form authority');
    LPopup.Open(NyxPopover('Reopened query lifetime'));
    LCurrent := NyxEventGeneration(LForm);
    Check(LPrevious.Cancelled and not LCurrent.Cancelled,
      'reopening admits fresh queries without reviving the old form generation');
    Check(Supports(LCurrent, INyxCancellationReadiness, LReady),
      'reopened generation retains its own final readiness');
    Inc(LInputObject.InputEpoch);
    LRefused := False;
    try
      LReady.CheckReady;
    except
      on ENyxSchedule do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused,
      'final readiness observes an input epoch change without an outward cancellation getter');
    Check(LCurrent.Cancelled and LMainGeneration.Cancelled and
      not LUnderlying.Cancelled,
      'input epoch change cancels captured main/form queries without cancelling the router');
    LPrevious := NyxEventGeneration(LForm);
    LInputObject.InputReady := False;
    LRefused := False;
    try
      LCurrent := NyxEventGeneration(LForm);
    except
      on ENyxSchedule do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and LPrevious.Cancelled,
      'input suspension refuses fresh capture and retires the admitted generation');
    Inc(LInputObject.InputEpoch);
    LInputObject.InputReady := True;
    LCurrent := NyxEventGeneration(LForm);
    Check(LPrevious.Cancelled and not LCurrent.Cancelled,
      'restored Ready cannot revive the earlier input incarnation');
    LCurrent.Cancel;
    Check(LCurrent.Cancelled and not LUnderlying.Cancelled,
      'explicit decorated cancellation never cancels the actual event router');
    LCurrent := NyxEventGeneration(LForm);
    Check(not LCurrent.Cancelled,
      'independently captured form generations retain independent cancellation');
    LRefused := False;
    try
      NewNyxArrangementQueryEvents(LPopup.Events, LInput, nil);
    except
      on ENyxSchedule do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'form overload refuses a missing actual admission provider');
    LPopup.Close;
    LForm := nil;
    LMain := nil;
    LAdmission := nil;
    LPopup := nil;
    Check((LCounts.Destroyed = 1) and LCurrent.Cancelled,
      'retained query generations own no presenter after forwarding surfaces release');
  finally
    LReady := nil;
    LCurrent := nil;
    LPrevious := nil;
    LMainGeneration := nil;
    LUnderlying := nil;
    LForm := nil;
    LMain := nil;
    LAdmission := nil;
    LPopup := nil;
    LInput := nil;
    LCounts.Free;
  end;
end;

{ Use the existing portable collection seed. A real controller is constructed
  against its actual view/session; the only adversarial boundary is the caller's
  supported input-factory callback, which can reenter or disconnect its mount. }
procedure QueryConstructionCases;
var
  LKind: TConstructionCallback;
  LView: INyxCollectionView;
  LMount: INyxCollectionMount;
  LRaw: TConstructionMount;
  LFactory: INyxQueryFormPresenterFactory;
  LFactoryObject: TConstructionFactory;
  LRouter: INyxEvents;
  LSource: INyxCollectionSnapshot;
  LQueries: INyxCollectionQueryWorkbench;
  LRefused: Boolean;
  LInput: INyxCollectionInputLifetime;
begin
  for LKind := Low(TConstructionCallback) to High(TConstructionCallback) do
  begin
    LView := NewNyxCollectionView(CreateNyxViewCollection,
      NyxTestTableViewSpec.TableLayout(NyxTableLayout), cpTable);
    LSource := LView.Store.Snapshot;
    LRaw := TConstructionMount.Create(LView);
    LMount := LRaw;
    LRaw.Activate;
    LRouter := NewNyxEvents;
    LFactoryObject := TConstructionFactory.Create;
    LFactory := LFactoryObject;
    LFactoryObject.Mount := LRaw;
    LFactoryObject.Router := LRouter;
    LFactoryObject.Kind := LKind;
    try
      LRefused := False;
      try
        LRaw.ConfigureQueries(LFactory, LRouter,
          NyxControlEvents('construction-table'));
      except
        on ENyxCollection do
        begin
          LRefused := True;
        end;
      end;

      if LKind = ccRecursive then
      begin
        LQueries := LRaw.GetQueryWorkbench;
        Check(not LRefused and LFactoryObject.RecursiveRefused and
          (LFactoryObject.Calls = 1) and (LQueries <> nil) and LQueries.Connected,
          'recursive input-factory setup refuses while the original real controller installs once');
        Check(not LFactoryObject.Revoked and (LFactoryObject.FormCalls = 0),
          'refused recursive admission neither revokes the accepted factory nor opens a form');
        LRefused := False;
        try
          LRaw.ConfigureQueries(LFactory, LRouter,
            NyxControlEvents('construction-table'));
        except
          on ENyxCollection do
          begin
            LRefused := True;
          end;
        end;
        Check(LRefused and not LFactoryObject.Revoked and LQueries.Connected,
          'a later duplicate setup preserves the accepted original owner');
        LInput := LRaw.GetInputLifetime;
        LFactoryObject.FailRevoke := True;
        LRefused := False;
        try
          LMount.Disconnect;
        except
          on ENyxCollection do
          begin
            LRefused := True;
          end;
        end;
        Check(LRefused and not LMount.Connected and LInput.Retired and
          not LQueries.Connected and (LRaw.Detachments = 1),
          'throwing factory revocation still retires input controller and actual target');
        LMount.Disconnect;
        Check((LRaw.Detachments = 1) and (LRaw.GetQueryWorkbench = nil),
          'completed teardown remains idempotent after its reported factory failure');
      end
      else
      begin
        Check(LRefused and not LMount.Connected and
          (LRaw.GetQueryWorkbench = nil) and LFactoryObject.Revoked and
          (LFactoryObject.Calls = 1),
          'teardown inside factory configuration safely refuses and revokes its partial attempt');
      end;
      Check(LView.Store.Snapshot = LSource,
        'query construction and reentry refusal preserve the exact accepted collection data');
    finally
      LMount.Disconnect;
      LQueries := nil;
      LMount := nil;
      LFactory := nil;
      LRouter.Close;
      LRouter := nil;
      LSource := nil;
      LView := nil;
    end;
  end;
end;

begin
  try
    SinglePresentationCases;
    NestedPresentationCases;
    FailedPresentationCases;
    QueryGenerationCases;
    QueryConstructionCases;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' popover open-lifetime checks';
    document.body.setAttribute('data-popover-open-lifetime', 'passed');
    document.body.setAttribute('data-popover-open-lifetime-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' popover open-lifetime checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-popover-open-lifetime', 'failed');
      document.body.setAttribute('data-popover-open-lifetime-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
