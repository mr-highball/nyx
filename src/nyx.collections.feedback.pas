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
unit nyx.collections.feedback;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.state, nyx.collections, nyx.collections.view,
  nyx.application.context;

type
  { Application-owned producer activity, independent of displayed membership or
    inspection pause. Signals do not fetch, assign, cancel or roll back data. }
  TNyxCollectionActivity = (ncaReady, ncaLoading, ncaRefreshing, ncaFailed);

  { Primary presentation, with pause taking precedence while Activity and the
    diagnostic continue to describe the producer independently. }
  TNyxCollectionFeedbackKind = (ncfkReady, ncfkLoading, ncfkRefreshing,
    ncfkEmpty, ncfkFilteredEmpty, ncfkFailed, ncfkPaused);

  { Actions are opt-in affordances. The application owns the actual Retry/Reset
    callbacks and their source/query authority; this binding performs no action. }
  TNyxCollectionFeedbackAction = (ncfaRetry, ncfaReset, ncfaResume);

  { A copied capability selection does not grant a query owner or producer lease. }
  TNyxCollectionFeedbackActions = set of TNyxCollectionFeedbackAction;

  { Bounded detached status, safe to retain after Close. Counts refer to the
    displayed source, filtered result and page, including during pause. No source,
    view, widget, context, callback or subscription is retained in this value. }
  TNyxCollectionFeedbackObservation = record
    Active: Boolean;
    Serial: Integer;
    Kind: TNyxCollectionFeedbackKind;
    Activity: TNyxCollectionActivity;
    SourceRows: Integer;
    MatchedRows: Integer;
    PageRows: Integer;
    DisplayRevision: Integer;
    IncomingRevision: Integer;
    PendingPublications: Integer;
    Diagnostic: TNyxText;
    Busy: Boolean;
    CanRetry: Boolean;
    CanReset: Boolean;
    CanResume: Boolean;
  end;

  { Copied localizable headings/action captions. Controls remain ordinary Nyx
    parts: authors may instead bind them to resources or computed expressions.
    Copy explicitly detaches the static text vector on native and pas2js. }
  TNyxCollectionFeedbackText = record
  private
    FHeadings: array[TNyxCollectionFeedbackKind] of TNyxText;
    FRetry: TNyxText;
    FReset: TNyxText;
    FResume: TNyxText;
  public
    class function Defaults: TNyxCollectionFeedbackText; static;
    function Copy: TNyxCollectionFeedbackText;
    function Heading(AKind: TNyxCollectionFeedbackKind): TNyxText;
    function WithHeading(AKind: TNyxCollectionFeedbackKind;
      const AText: TNyxText): TNyxCollectionFeedbackText;
    function RetryButton(const AText: TNyxText): TNyxCollectionFeedbackText;
    function ResetButton(const AText: TNyxText): TNyxCollectionFeedbackText;
    function ResumeButton(const AText: TNyxText): TNyxCollectionFeedbackText;
    property RetryCaption: TNyxText read FRetry;
    property ResetCaption: TNyxText read FReset;
    property ResumeCaption: TNyxText read FResume;
  end;

  { Open named-part identities and typed scalar bindings. Define on the authored
    document before mounting. Numeric count states let callers replace the default
    detail with their own localized/resource-backed expression, atomically. }
  TNyxCollectionFeedbackParts = record
  private
    FName: TNyxText;
  public
    procedure Validate;
    procedure Define(AState: TNyxState);
    function Caption: TNyxTextStateRef;
    function Detail: TNyxTextStateRef;
    function Busy: TNyxBooleanStateRef;
    function RetryVisible: TNyxBooleanStateRef;
    function ResetVisible: TNyxBooleanStateRef;
    function ResumeVisible: TNyxBooleanStateRef;
    function SourceRows: TNyxIntegerStateRef;
    function MatchedRows: TNyxIntegerStateRef;
    function PageRows: TNyxIntegerStateRef;
    function RetryID: TNyxText;
    function ResetID: TNyxText;
    function ResumeID: TNyxText;
    function ProgressID: TNyxText;
    property RootID: TNyxText read FName;
  end;

  { UI-owner feedback binding. It observes the existing flat List/Table view and
    writes only its declared scalar state, never source/query/selection/history.
    Close before application disposal. Tokens have weak receivers; neither this
    binding nor its context retains a renderer/router/document/widget.
    Capture returns copied cached facts. Producer completion/cancellation must
    still use the application's existing scheduler/generation/revision rules.
    Actions changes available affordances through the same atomic publication;
    capture observations on UI before handing detached values to workers. }
  INyxCollectionFeedback = interface(IInterface)
    ['{53A38F4F-7106-4AD2-87DC-914D65A09F91}']
    function Loading: INyxCollectionFeedback;
    function Refreshing: INyxCollectionFeedback;
    function Ready: INyxCollectionFeedback;
    function Failed(const ADiagnostic: TNyxText): INyxCollectionFeedback;
    function Actions(AAllowed: TNyxCollectionFeedbackActions): INyxCollectionFeedback;
    procedure Refresh;
    function Capture: TNyxCollectionFeedbackObservation;
    procedure Close;
  end;

function NyxCollectionFeedbackParts(const AName: TNyxText): TNyxCollectionFeedbackParts;
function BindNyxCollectionFeedback(const AContext: INyxApplicationContext;
  const AView: INyxCollectionView; const AParts: TNyxCollectionFeedbackParts;
  const AText: TNyxCollectionFeedbackText;
  AActions: TNyxCollectionFeedbackActions = [ncfaResume]): INyxCollectionFeedback;

implementation

uses SysUtils, nyx.editing, nyx.scheduler, nyx.collections.inspection,
  nyx.collections.paging, nyx.collections.view.types, nyx.publication;

type
  { Owns context/generation and the ordinary view, with explicitly disconnected
    weak subscriptions. One prepared scalar publication installs all labels,
    counts and affordances before any observer runs. Reentrant view changes
    during validation refuse that candidate; Refresh reconciles afterward. }
  TFeedback = class(TInterfacedObject, INyxCollectionFeedback)
  private
    FContext: INyxApplicationContext;
    FGeneration: INyxCancellationScope;
    FScheduler: INyxScheduler;
    FView: INyxCollectionView;
    FInspection: INyxCollectionInspection;
    FDataToken: INyxCollectionViewSubscription;
    FStatusToken: INyxCollectionViewSubscription;
    FParts: TNyxCollectionFeedbackParts;
    FText: TNyxCollectionFeedbackText;
    FActions: TNyxCollectionFeedbackActions;
    FObservation: TNyxCollectionFeedbackObservation;
    FViewSerial: Integer;
    FSerialExhausted: Boolean;
    FPublishing: Boolean;
    FClosed: Boolean;
    function Active: Boolean;
    procedure Changed;
    procedure DataChanged(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
    procedure StatusChanged(const AStatus: TNyxCollectionInspectionObservation;
      ACause: TNyxCollectionInspectionCause);
    procedure Present(AActivity: TNyxCollectionActivity; const ADiagnostic: TNyxText;
      AActions: TNyxCollectionFeedbackActions);
  public
    constructor Create(const AContext: INyxApplicationContext;
      const AView: INyxCollectionView; const AParts: TNyxCollectionFeedbackParts;
      const AText: TNyxCollectionFeedbackText; AActions: TNyxCollectionFeedbackActions);
    destructor Destroy; override;
    function Loading: INyxCollectionFeedback;
    function Refreshing: INyxCollectionFeedback;
    function Ready: INyxCollectionFeedback;
    function Failed(const ADiagnostic: TNyxText): INyxCollectionFeedback;
    function Actions(AAllowed: TNyxCollectionFeedbackActions): INyxCollectionFeedback;
    procedure Refresh;
    function Capture: TNyxCollectionFeedbackObservation;
    procedure Close;
  end;

class function TNyxCollectionFeedbackText.Defaults: TNyxCollectionFeedbackText;
begin
  Result := Default(TNyxCollectionFeedbackText);
  Result.FHeadings[ncfkReady] := 'Records are ready';
  Result.FHeadings[ncfkLoading] := 'Loading records…';
  Result.FHeadings[ncfkRefreshing] := 'Refreshing records…';
  Result.FHeadings[ncfkEmpty] := 'No records yet';
  Result.FHeadings[ncfkFilteredEmpty] := 'No matching records';
  Result.FHeadings[ncfkFailed] := 'Could not update records';
  Result.FHeadings[ncfkPaused] := 'Inspection is paused';
  Result.FRetry := 'Try again';
  Result.FReset := 'Reset this filter';
  Result.FResume := 'Resume latest';
end;

function TNyxCollectionFeedbackText.Copy: TNyxCollectionFeedbackText;
var
  LKind: TNyxCollectionFeedbackKind;
begin
  Result := Default(TNyxCollectionFeedbackText);
  for LKind := Low(TNyxCollectionFeedbackKind) to High(TNyxCollectionFeedbackKind) do
  begin
    Result.FHeadings[LKind] := FHeadings[LKind];
  end;
  Result.FRetry := FRetry;
  Result.FReset := FReset;
  Result.FResume := FResume;
end;

function TNyxCollectionFeedbackText.Heading(AKind: TNyxCollectionFeedbackKind): TNyxText;
begin
  Result := FHeadings[AKind];
end;

function TNyxCollectionFeedbackText.WithHeading(AKind: TNyxCollectionFeedbackKind;
  const AText: TNyxText): TNyxCollectionFeedbackText;
begin
  NyxTextScalarCount(AText);
  Result := Copy;
  Result.FHeadings[AKind] := AText;
end;

function TNyxCollectionFeedbackText.RetryButton(const AText: TNyxText): TNyxCollectionFeedbackText;
begin
  NyxTextScalarCount(AText);
  Result := Copy;
  Result.FRetry := AText;
end;

function TNyxCollectionFeedbackText.ResetButton(const AText: TNyxText): TNyxCollectionFeedbackText;
begin
  NyxTextScalarCount(AText);
  Result := Copy;
  Result.FReset := AText;
end;

function TNyxCollectionFeedbackText.ResumeButton(const AText: TNyxText): TNyxCollectionFeedbackText;
begin
  NyxTextScalarCount(AText);
  Result := Copy;
  Result.FResume := AText;
end;

function NyxCollectionFeedbackParts(const AName: TNyxText): TNyxCollectionFeedbackParts;
begin
  Result.FName := NyxCollection(AName).Name;
end;

procedure TNyxCollectionFeedbackParts.Validate;
begin
  NyxCollection(FName);
end;

function TNyxCollectionFeedbackParts.Caption: TNyxTextStateRef;
begin
  Result := NyxTextState(FName + '-caption');
end;

function TNyxCollectionFeedbackParts.Detail: TNyxTextStateRef;
begin
  Result := NyxTextState(FName + '-detail');
end;

function TNyxCollectionFeedbackParts.Busy: TNyxBooleanStateRef;
begin
  Result := NyxBooleanState(FName + '-busy');
end;

function TNyxCollectionFeedbackParts.RetryVisible: TNyxBooleanStateRef;
begin
  Result := NyxBooleanState(FName + '-retry-visible');
end;

function TNyxCollectionFeedbackParts.ResetVisible: TNyxBooleanStateRef;
begin
  Result := NyxBooleanState(FName + '-reset-visible');
end;

function TNyxCollectionFeedbackParts.ResumeVisible: TNyxBooleanStateRef;
begin
  Result := NyxBooleanState(FName + '-resume-visible');
end;

function TNyxCollectionFeedbackParts.SourceRows: TNyxIntegerStateRef;
begin
  Result := NyxIntegerState(FName + '-source-rows');
end;

function TNyxCollectionFeedbackParts.MatchedRows: TNyxIntegerStateRef;
begin
  Result := NyxIntegerState(FName + '-matched-rows');
end;

function TNyxCollectionFeedbackParts.PageRows: TNyxIntegerStateRef;
begin
  Result := NyxIntegerState(FName + '-page-rows');
end;

function TNyxCollectionFeedbackParts.RetryID: TNyxText;
begin
  Result := FName + '-retry';
end;

function TNyxCollectionFeedbackParts.ResetID: TNyxText;
begin
  Result := FName + '-reset';
end;

function TNyxCollectionFeedbackParts.ResumeID: TNyxText;
begin
  Result := FName + '-resume';
end;

function TNyxCollectionFeedbackParts.ProgressID: TNyxText;
begin
  Result := FName + '-progress';
end;

procedure TNyxCollectionFeedbackParts.Define(AState: TNyxState);
begin
  Validate;

  if AState = nil then
  begin
    raise ENyxCollection.Create('Feedback requires its authored scalar state');
  end;
  AState.Apply([NyxStateValue(Caption, TNyxCollectionFeedbackText.Defaults.Heading(ncfkReady)),
    NyxStateValue(Detail, ''), NyxStateValue(Busy, False),
    NyxStateValue(RetryVisible, False), NyxStateValue(ResetVisible, False),
    NyxStateValue(ResumeVisible, False), NyxStateValue(SourceRows, 0),
    NyxStateValue(MatchedRows, 0), NyxStateValue(PageRows, 0)]);
end;

constructor TFeedback.Create(const AContext: INyxApplicationContext;
  const AView: INyxCollectionView; const AParts: TNyxCollectionFeedbackParts;
  const AText: TNyxCollectionFeedbackText; AActions: TNyxCollectionFeedbackActions);
begin
  inherited Create;
  AParts.Validate;

  if (AContext = nil) or not AContext.Active or (AView = nil) then
  begin
    raise ENyxCollection.Create('Feedback requires its active context and view');
  end;
  FContext := AContext;
  FScheduler := AContext.Scheduler;
  FScheduler.RequireUI;
  FGeneration := AContext.Generation;
  FView := AView;

  if not (AView.Projection in [cpList, cpTable]) then
  begin
    raise ENyxCollection.Create('Feedback currently supports flat Lists and Tables');
  end;
  FInspection := NyxCollectionInspection(AView);
  FParts := AParts;
  FText := AText.Copy;
  FActions := AActions;
  if FInspection <> nil then
  begin
    FStatusToken := FInspection.SubscribeInspection(StatusChanged);
  end
  else
  begin
    FDataToken := AView.Subscribe(DataChanged);
  end;
  Refresh;
end;

destructor TFeedback.Destroy;
begin
  Close;
  inherited Destroy;
end;

function TFeedback.Active: Boolean;
begin
  Result := not FClosed and (FContext <> nil) and (FGeneration <> nil) and
    not FGeneration.Cancelled and FContext.Active;
end;

procedure TFeedback.Close;
begin

  if FScheduler <> nil then
  begin
    FScheduler.RequireUI;
  end;
  FClosed := True;
  FDataToken := nil;
  FStatusToken := nil;
  FInspection := nil;
  FView := nil;
  FContext := nil;
  FGeneration := nil;
end;

procedure TFeedback.Changed;
var
  LLease: INyxCollectionFeedback;
begin
  LLease := Self;

  if not Active then
  begin
    Exit;
  end;

  if FViewSerial = High(Integer) then
  begin
    FSerialExhausted := True;
  end
  else
  begin
    Inc(FViewSerial);
  end;

  if not FPublishing then
  begin
    Refresh;
  end;
end;

procedure TFeedback.DataChanged(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
begin
  Changed;
end;

procedure TFeedback.StatusChanged(const AStatus: TNyxCollectionInspectionObservation;
  ACause: TNyxCollectionInspectionCause);
begin
  { Owned inspection receivers observe data and inspection-only notifications.
    Subscribe once rather than rereading counts through two callback routes. }
  Changed;
end;

procedure TFeedback.Present(AActivity: TNyxCollectionActivity; const ADiagnostic: TNyxText;
  AActions: TNyxCollectionFeedbackActions);
var
  LLease: INyxCollectionFeedback;
  LView: INyxCollectionView;
  LInspection: INyxCollectionInspection;
  LPages: INyxCollectionPages;
  LFrame: INyxCollectionSnapshot;
  LSource: INyxCollectionSnapshot;
  LStatus: TNyxCollectionInspectionObservation;
  LNext: TNyxCollectionFeedbackObservation;
  LPlan: INyxPreparedPublication;
  LContext: INyxApplicationContext;
  LState: TNyxState;
  LDetail: TNyxText;
  LSerial: Integer;

  procedure RequireCurrent;
  begin

    if not Active or FSerialExhausted or (FViewSerial <> LSerial) or (FView <> LView) then
    begin
      raise ENyxCollection.Create('Feedback changed or retired during publication; refresh current facts');
    end;
  end;

begin
  LLease := Self;
  FScheduler.RequireUI;

  if not Active or FPublishing then
  begin
    raise ENyxCollection.Create('Feedback is retired or already publishing');
  end;
  NyxTextScalarCount(ADiagnostic);
  LSerial := FViewSerial;
  LView := FView;
  LInspection := FInspection;
  LContext := FContext;
  FPublishing := True;
  try
    LNext := Default(TNyxCollectionFeedbackObservation);
    LNext.Active := True;
    LNext.Activity := AActivity;
    LNext.Diagnostic := ADiagnostic;
    LFrame := LView.Snapshot;
    RequireCurrent;
    LStatus := Default(TNyxCollectionInspectionObservation);

    if LInspection <> nil then
    begin
      LSource := LInspection.GetDisplayedSource;
      RequireCurrent;
      LStatus := LInspection.CaptureInspection;
    end
    else
    begin
      LSource := LView.Store.Snapshot;
    end;
    RequireCurrent;
    LPages := NyxCollectionPages(LView);
    RequireCurrent;
    LNext.MatchedRows := LPages.Window.Total;
    RequireCurrent;
    LNext.SourceRows := LSource.Count;
    LNext.PageRows := LFrame.Count;
    LNext.DisplayRevision := LFrame.Revision;
    LNext.IncomingRevision := LSource.Revision;

    if LInspection <> nil then
    begin
      LNext.IncomingRevision := LStatus.IncomingRevision;
      LNext.PendingPublications := LStatus.PendingPublications;
    end;
    RequireCurrent;
    LNext.Busy := AActivity in [ncaLoading, ncaRefreshing];

    if (LInspection <> nil) and (LStatus.Mode = ncimPaused) then
    begin
      LNext.Kind := ncfkPaused;
    end
    else if AActivity = ncaLoading then
    begin
      LNext.Kind := ncfkLoading;
    end
    else if AActivity = ncaRefreshing then
    begin
      LNext.Kind := ncfkRefreshing;
    end
    else if AActivity = ncaFailed then
    begin
      LNext.Kind := ncfkFailed;
    end
    else if LNext.SourceRows = 0 then
    begin
      LNext.Kind := ncfkEmpty;
    end
    else if LNext.MatchedRows = 0 then
    begin
      LNext.Kind := ncfkFilteredEmpty;
    end
    else
    begin
      LNext.Kind := ncfkReady;
    end;
    LNext.CanRetry := (ncfaRetry in AActions) and
      ((AActivity = ncaFailed) or (LNext.Kind = ncfkEmpty));
    LNext.CanReset := (ncfaReset in AActions) and
      (LNext.SourceRows > 0) and (LNext.MatchedRows = 0);
    LNext.CanResume := (ncfaResume in AActions) and (LNext.Kind = ncfkPaused);

    if FObservation.Serial = High(Integer) then
    begin
      raise ENyxCollection.Create('Feedback observation serial is exhausted');
    end;
    LNext.Serial := FObservation.Serial + 1;
    LDetail := TNyxText(IntToStr(LNext.PageRows)) + ' displayed / ' +
      TNyxText(IntToStr(LNext.MatchedRows)) + ' matched / ' +
      TNyxText(IntToStr(LNext.SourceRows)) + ' total';

    if LNext.Kind = ncfkPaused then
    begin
      LDetail := LDetail + ' / ' + TNyxText(IntToStr(LNext.PendingPublications)) + ' pending updates';

      case AActivity of
        ncaLoading:
          LDetail := LDetail + '. ' + FText.Heading(ncfkLoading);
        ncaRefreshing:
          LDetail := LDetail + '. ' + FText.Heading(ncfkRefreshing);
        ncaFailed:
          LDetail := LDetail + '. ' + FText.Heading(ncfkFailed);
        ncaReady:
          begin
            { Pause alone does not imply that the producer is busy or failed. }
          end;
      end;
    end;

    if ADiagnostic <> '' then
    begin
      LDetail := LDetail + '. ' + ADiagnostic;
    end;
    RequireCurrent;
    LState := LContext.State;
    RequireCurrent;
    LPlan := LState.PrepareApply([
      NyxStateValue(FParts.Caption, FText.Heading(LNext.Kind)),
      NyxStateValue(FParts.Detail, LDetail), NyxStateValue(FParts.Busy, LNext.Busy),
      NyxStateValue(FParts.RetryVisible, LNext.CanRetry),
      NyxStateValue(FParts.ResetVisible, LNext.CanReset),
      NyxStateValue(FParts.ResumeVisible, LNext.CanResume),
      NyxStateValue(FParts.SourceRows, LNext.SourceRows),
      NyxStateValue(FParts.MatchedRows, LNext.MatchedRows),
      NyxStateValue(FParts.PageRows, LNext.PageRows)]);
    try
      LPlan.Validate;
      RequireCurrent;
      LPlan.Install;
      FObservation := LNext;
      FActions := AActions;
      LPlan.Notify;
    finally
      LPlan.Retire;
    end;
  finally
    FPublishing := False;
  end;
end;

function TFeedback.Loading: INyxCollectionFeedback;
begin
  Result := Self;
  Present(ncaLoading, '', FActions);
end;

function TFeedback.Refreshing: INyxCollectionFeedback;
begin
  Result := Self;
  Present(ncaRefreshing, '', FActions);
end;

function TFeedback.Ready: INyxCollectionFeedback;
begin
  Result := Self;
  Present(ncaReady, '', FActions);
end;

function TFeedback.Failed(const ADiagnostic: TNyxText): INyxCollectionFeedback;
begin
  Result := Self;
  Present(ncaFailed, ADiagnostic, FActions);
end;

function TFeedback.Actions(AAllowed: TNyxCollectionFeedbackActions): INyxCollectionFeedback;
begin
  Result := Self;
  Present(FObservation.Activity, FObservation.Diagnostic, AAllowed);
end;

procedure TFeedback.Refresh;
begin
  Present(FObservation.Activity, FObservation.Diagnostic, FActions);
end;

function TFeedback.Capture: TNyxCollectionFeedbackObservation;
begin
  FScheduler.RequireUI;
  Result := FObservation;
  Result.Active := Active;
end;

function BindNyxCollectionFeedback(const AContext: INyxApplicationContext;
  const AView: INyxCollectionView; const AParts: TNyxCollectionFeedbackParts;
  const AText: TNyxCollectionFeedbackText;
  AActions: TNyxCollectionFeedbackActions): INyxCollectionFeedback;
begin
  Result := TFeedback.Create(AContext, AView, AParts, AText, AActions);
end;

end.
