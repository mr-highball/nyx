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

unit nyx.collections.query.workbench;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.events, nyx.collections.view, nyx.collections.query.controls;

{ Construct under interface ownership before any outward initialization. The
  supplied router is the accepted parent router; no generation is captured while
  a hidden renderer candidate is being built. Every real input captures its own. }
function NewNyxCollectionQueryWorkbench(const AView: INyxCollectionView;
  const AEvents: INyxEvents; const ATarget: TNyxEventTarget;
  const APresenters: INyxQueryFormPresenterFactory;
  const ADesignID: TNyxText = ''): INyxCollectionQueryWorkbench;
{ One explicit inspection surface for a List or Table. The existing session,
  command negotiation, cancellation and presenter lifetimes remain authoritative.
  Only exact removal and deliberate global resets are admitted in this purpose;
  table headers retain their independent filter/sort permissions. }
function NewNyxCollectionQueryDetails(const AView: INyxCollectionView;
  const AEvents: INyxEvents; const ATarget: TNyxEventTarget;
  const APresenters: INyxQueryFormPresenterFactory;
  const ADesignID: TNyxText = ''): INyxCollectionQueryWorkbench;

implementation

uses
  SysUtils, nyx.types, nyx.data, nyx.state, nyx.model,
  nyx.root.types, nyx.controls, nyx.behavior, nyx.scheduler, nyx.publication,
  nyx.popover.types, nyx.collections, nyx.collections.query,
  nyx.collections.query.policy, nyx.collections.view.types,
  nyx.collections.query.session, nyx.collections.query.editor, nyx.menu.editor;

const
  CPage = 'nyx-query-workbench-page';
  CStatus = 'nyx-query-workbench-status';
  CCounts = 'nyx-query-workbench-counts';
  CClearOwner = 'nyx-query-workbench-clear-owner';
  CClearAll = 'nyx-query-workbench-clear-all';
  CResetSort = 'nyx-query-workbench-reset-sort';
  CResetAll = 'nyx-query-workbench-reset-all';
  COpenSort = 'nyx-query-workbench-open-sort';
  CReload = 'nyx-query-workbench-reload';
  CChipPrefix = 'nyx-query-workbench-chip-';
  { The presenter anchor uses a negative ordinal for whole-surface content.
    This distinct internal sentinel never denotes a source column. }
  CDetailsColumn = -2;

type
  { Closed host purpose keeps inspection authority separate from header policy. }
  TWorkbenchPurpose = (wpTableControls, wpQueryDetails);
  { Forward declaration for inert callback ports, never a retained controller. }
  TWorkbench = class;
  { Only this inert weak port crosses callbacks/queued work. Revocation precedes
    any target cleanup. Serial is UI-owned and monotonic, including no-op Clear. }
  TWorkbenchPort = class(TInterfacedObject)
  public
    Owner: TWorkbench;
    Active: Boolean;
    Enabled: Boolean;
    Serial: Integer;
  end;
  { Exact form incarnation. A retained old popup cannot deliver into its newer
    replacement even when every logical ID and typed value remains identical. }
  TFormPort = class(TInterfacedObject)
  public
    Parent: TWorkbenchPort;
    ParentLease: IInterface;
    Active: Boolean;
  end;
  { Existing scheduler work owns copied values and inert leases only. Worker
    entry posts those values to UI before reading any controller/view/presenter. }
  TQueryWork = class(TInterfacedObject, INyxWork)
  public
    Port: TWorkbenchPort;
    PortLease: IInterface;
    Form: TFormPort;
    FormLease: IInterface;
    Serial: Integer;
    Generation: INyxCancellationScope;
    FormGeneration: INyxCancellationScope;
    Scheduler: INyxScheduler;
    OnUI: Boolean;
    Structural: Boolean;
    Column: Integer;
    Command: TNyxQueryWorkbenchCommand;
    Query: TNyxCollectionQuery;
    Edits: TNyxQuerySessionEdits;
    Capture: INyxCollectionQuerySessionSnapshot;
    procedure Execute(const AExecution: INyxExecution);
    function CloneForUI: INyxWork;
  end;
  { Last validator samples generic cancellation getters. Its final fence reads
    owned controller flags and captured optional flags-only cancellation facets,
    after every model validator. No application callback or mutable UI/model
    getter runs during Install/CheckReady. Arranged main/form generations both
    expose these facets; legacy alternative routers remain source compatible. }
  TCommandFence = class(TInterfacedObject, INyxPreparedPublication, INyxPublicationReadiness)
  private
    FPort: TWorkbenchPort;
    FPortLease: IInterface;
    FForm: TFormPort;
    FFormLease: IInterface;
    FSerial: Integer;
    FGeneration: INyxCancellationScope;
    FFormGeneration: INyxCancellationScope;
    FGenerationReady: INyxCancellationReadiness;
    FFormReady: INyxCancellationReadiness;
    FReady: Boolean;
    FInstalled: Boolean;
    procedure Flags;
  public
    constructor Create(AWork: TQueryWork);
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
    property Installed: Boolean read FInstalled;
  end;
  { Event observations borrow the controller through its revocable port. They
    never own a popup/controller cycle and never replace ordinary registrations. }
  TFormObserver = class(TNyxEventObserver)
  private
    FForm: TFormPort;
    FLease: IInterface;
  public
    constructor Create(AForm: TFormPort; const ALease: IInterface);
    procedure Observe(const AEvent: TNyxEventInfo; ACallbackCount: Integer); override;
  end;
  { Managed command surface. Its view/session/actor are model leases; target and
    form access stay behind weak factory/presenter interfaces. }
  TWorkbench = class(TInterfacedObject, INyxCollectionQueryWorkbench,
    INyxCollectionQueryDetails)
  private
    FPurpose: TWorkbenchPurpose;
    FDetailsOwner: TNyxQueryOwnerID;
    FView: INyxCollectionView;
    FEvents: INyxEvents;
    FTarget: TNyxEventTarget;
    FDesignID: TNyxText;
    FScheduler: INyxScheduler;
    FSession: INyxCollectionQuerySession;
    FActor: INyxCollectionQueryOwner;
    FPresenters: INyxQueryFormPresenterFactory;
    FPresenter: INyxQueryFormPresenter;
    FPort: TWorkbenchPort;
    FPortLease: IInterface;
    FForm: TFormPort;
    FFormLease: IInterface;
    FFormCapture: INyxCollectionQuerySessionSnapshot;
    FFormBaseline: TNyxText;
    FColumn: Integer;
    FFormObservation: TNyxQueryWorkbenchObservation;
    FSubscriptions: array of INyxEventSubscription;
    FExecutions: array of INyxExecution;
    FViewToken: INyxCollectionViewSubscription;
    FDisconnected: Boolean;
    procedure Initialize(const AView: INyxCollectionView; const AEvents: INyxEvents;
      const ATarget: TNyxEventTarget; const APresenters: INyxQueryFormPresenterFactory;
      const ADesignID: TNyxText; APurpose: TWorkbenchPurpose);
    function Advance(AJoin: Boolean = True): Integer;
    procedure RequireActive;
    procedure RequireFormOpen(AForm: TFormPort);
    procedure CheckColumn(AColumn: Integer; AFilter: Boolean);
    procedure CheckSort(const AQuery: TNyxCollectionQuery);
    procedure CheckEdits(const AEdits: TNyxQuerySessionEdits;
      const ACapture: INyxCollectionQuerySessionSnapshot);
    function SortKey(AColumn: Integer; ADirection: TNyxSortDirection): TNyxCollectionQuery;
    procedure Status(const AText: TNyxText);
    procedure RetireForm;
    procedure OpenForm(AColumn: Integer; const AQuery: TNyxCollectionQuery;
      const ACapture: INyxCollectionQuerySessionSnapshot);
    procedure CaptureAction(AForm: TFormPort; const AEvent: TNyxEventInfo);
    procedure ExecuteWork(AWork: TQueryWork);
    procedure Publish(AWork: TQueryWork);
    procedure Submit(AWork: TQueryWork; APolicy: TNyxExecutionPolicy);
    procedure Direct(ACommand: TNyxQueryWorkbenchCommand; AColumn: Integer;
      const AEdits: TNyxQuerySessionEdits);
    procedure Changed(const AView: INyxCollectionView; const AChanges: INyxCollectionChanges);
    function EventInfo(AKind: TNyxQueryWorkbenchEvent;
      const AChange: TNyxQueryWorkbenchChange): TNyxEventInfo;
  public
    destructor Destroy; override;
    procedure OpenColumn(AColumn: Integer);
    procedure OpenSort;
    procedure OpenDetails;
    procedure OpenDetailsFor(const ALocalOwner: TNyxQueryOwnerID);
    procedure ReloadDraft;
    procedure SortColumn(AColumn: Integer; ADirection: TNyxSortDirection;
      AAppend: Boolean = False);
    procedure ClearColumn(AColumn: Integer);
    procedure ClearOwner;
    procedure ClearAllFilters;
    procedure ResetSort;
    procedure ResetAll;
    procedure RemoveFilter(const AOwner: TNyxQueryOwnerID; const AChannel: TNyxQueryChannelRef);
    procedure SetEnabled(AEnabled: Boolean);
    function GetConnected: Boolean;
    function GetObservation: TNyxQueryWorkbenchObservation;
    function GetPresenter: INyxQueryFormPresenter;
    procedure Revoke;
    procedure Disconnect;
  end;

{ A bounded readable description traverses the admitted predicate metadata. It
  never evaluates rows or assigns meaning to the open contribution channel. }
function PredicateCaption(const APredicate: INyxCollectionPredicate;
  const ASpec: TNyxCollectionViewSpec): TNyxText;
const
  CComparison: array[TNyxQueryComparison] of TNyxText =
    ('equals', 'does not equal', 'is less than', 'is at most', 'is greater than',
     'is at least', 'contains', 'starts with', 'ends with');
var
  LIndex, LPosition, LScalarCount, LLast: Integer;
  LScalar: Integer;
  LTitle, LValue, LJoin: TNyxText;
  LExpected: TNyxStateValue;
begin

  if APredicate = nil then
  begin
    Exit('No filter');
  end;
  case APredicate.Kind of
    nqpField:
      begin
        LTitle := APredicate.FieldName;
        for LIndex := 0 to ASpec.Count - 1 do
        begin

          if ASpec.ColumnAt(LIndex).FieldName = APredicate.FieldName then
          begin
            LTitle := ASpec.ColumnAt(LIndex).Title;
            Break;
          end;
        end;
        LExpected := APredicate.Expected;
        case LExpected.Kind of
          nskText: LValue := TNyxText('“') + LExpected.TextValue + TNyxText('”');
          nskBoolean:
            begin

              if LExpected.BooleanValue then
              begin
                LValue := 'true';
              end
              else
              begin
                LValue := 'false';
              end;
            end;
          nskInteger: LValue := TNyxText(IntToStr(LExpected.IntegerValue));
          nskNumber: LValue := LExpected.NumberText;
        end;
        Result := LTitle + ' ' + CComparison[APredicate.Comparison] + ' ' + LValue;
      end;
    nqpNot: Result := 'Not (' + PredicateCaption(APredicate.Child(0), ASpec) + ')';
    nqpAll, nqpAny:
      begin
        LJoin := ' and ';

        if APredicate.Kind = nqpAny then
        begin
          LJoin := ' or ';
        end;
        Result := '';
        for LIndex := 0 to APredicate.ChildCount - 1 do
        begin

          if LIndex > 0 then
          begin
            Result := Result + LJoin;
          end;
          Result := Result + '(' + PredicateCaption(APredicate.Child(LIndex), ASpec) + ')';

          if Length(Result) > 720 then
          begin
            Break;
          end;
        end;
      end;
  end;

  { Count decoded scalars, preserving UTF-8 bytes natively and surrogate pairs
    in the browser. Only the first 181 scalars need inspection. }
  LPosition := 1;
  LScalarCount := 0;
  LLast := 0;
  while (LPosition <= Length(Result)) and (LScalarCount <= 180) do
  begin

    if not NyxNextScalar(Result, LPosition, LScalar) then
    begin
      raise ENyxCollection.Create('Query predicate caption is not valid Unicode');
    end;
    Inc(LScalarCount);

    if LScalarCount = 177 then
    begin
      LLast := LPosition - 1;
    end;
  end;

  if LScalarCount > 180 then
  begin
    Result := Copy(Result, 1, LLast) + TNyxText('…');
  end;
end;

function TQueryWork.CloneForUI: INyxWork;
var
  LNext: TQueryWork;
begin
  LNext := TQueryWork.Create;
  Result := LNext;
  LNext.Port := Port;
  LNext.PortLease := PortLease;
  LNext.Form := Form;
  LNext.FormLease := FormLease;
  LNext.Serial := Serial;
  LNext.Generation := Generation;
  LNext.FormGeneration := FormGeneration;
  LNext.Scheduler := Scheduler;
  LNext.OnUI := True;
  LNext.Structural := Structural;
  LNext.Column := Column;
  LNext.Command := Command;
  LNext.Query := Query.Copy;
  LNext.Edits := Edits.Copy;
  LNext.Capture := Capture;
end;

procedure TQueryWork.Execute(const AExecution: INyxExecution);
var
  LWork: INyxWork;
  LOwner: TWorkbench;
  LOwnerLease: INyxCollectionQueryWorkbench;
  LCancelled: Boolean;
begin

  if not OnUI then
  begin
    LWork := CloneForUI;
    Scheduler.PostUI(LWork, AExecution);
    Exit;
  end;
  Scheduler.RequireUI;

  if (Port = nil) or not Port.Active or not Port.Enabled or
    (Port.Serial <> Serial) or (Port.Owner = nil) or
    ((Form <> nil) and not Form.Active) then
  begin
    Exit;
  end;
  LCancelled := AExecution.Cancelled or Generation.Cancelled;

  if FormGeneration <> nil then
  begin
    LCancelled := FormGeneration.Cancelled or LCancelled;
  end;

  if LCancelled or (Port = nil) or not Port.Active or not Port.Enabled or
    (Port.Serial <> Serial) or (Port.Owner = nil) or
    ((Form <> nil) and not Form.Active) then
  begin
    Exit;
  end;
  LOwner := Port.Owner;
  LOwnerLease := LOwner;

  if not Port.Active or (Port.Owner <> LOwner) or (Port.Serial <> Serial) then
  begin
    Exit;
  end;
  try
    LOwner.ExecuteWork(Self);
  except
    on E: Exception do
    begin

      if Port.Active and (Port.Owner = LOwner) then
      begin
        LOwner.Status(TNyxText(E.Message));
      end;
      raise;
    end;
  end;
end;

constructor TCommandFence.Create(AWork: TQueryWork);
var
  LGenerationReady: INyxCancellationReadiness;
  LFormReady: INyxCancellationReadiness;
begin
  inherited Create;
  FPort := AWork.Port;
  FPortLease := AWork.PortLease;
  FForm := AWork.Form;
  FFormLease := AWork.FormLease;
  FSerial := AWork.Serial;
  FGeneration := AWork.Generation;
  FFormGeneration := AWork.FormGeneration;

  if FGeneration <> nil then
  begin
    Supports(FGeneration, INyxCancellationReadiness, LGenerationReady);
    FGenerationReady := LGenerationReady;
  end;

  if FFormGeneration <> nil then
  begin
    Supports(FFormGeneration, INyxCancellationReadiness, LFormReady);
    FFormReady := LFormReady;
  end;
  { A pas2js QI out argument is a temporary owning lease. Assign each retained
    field normally from a local, giving the fence its own independent reference.
    Passing a field directly can release the main/form generation when the fence
    retires, even while its router still owns and later cancels that generation. }
end;

procedure TCommandFence.Flags;
begin

  if (FPort = nil) or not FPort.Active or not FPort.Enabled or
    (FPort.Owner = nil) or (FPort.Serial <> FSerial) or
    ((FForm <> nil) and not FForm.Active) then
  begin
    raise ENyxCollection.Create('Query command was superseded or its form retired');
  end;
end;

procedure TCommandFence.Validate;
var
  LCancelled: Boolean;
  LOwner: TWorkbench;
  LLease: INyxCollectionQueryWorkbench;
begin
  Flags;

  if FForm <> nil then
  begin
    LOwner := FPort.Owner;
    LLease := LOwner;
    LOwner.RequireFormOpen(FForm);
    Flags;
  end;
  LCancelled := FGeneration.Cancelled;

  if FFormGeneration <> nil then
  begin
    LCancelled := FFormGeneration.Cancelled or LCancelled;
  end;
  Flags;

  if LCancelled then
  begin
    raise ENyxCollection.Create('Query command generation was cancelled');
  end;
  FReady := True;
end;

procedure TWorkbench.RequireFormOpen(AForm: TFormPort);
var
  LPresenter: INyxQueryFormPresenter;
  LOpen: Boolean;
begin

  if AForm = nil then
  begin
    Exit;
  end;

  if not GetConnected or (FForm <> AForm) or not AForm.Active or (FPresenter = nil) then
  begin
    raise ENyxCollection.Create('Query form has retired');
  end;
  LPresenter := FPresenter;
  LOpen := LPresenter.IsOpen;

  if not GetConnected or (FForm <> AForm) or not AForm.Active or
    (FPresenter <> LPresenter) or not LOpen then
  begin
    raise ENyxCollection.Create('Query form has closed or retired');
  end;
end;

procedure TCommandFence.CheckReady;
begin
  Flags;

  if not FReady then
  begin
    raise ENyxCollection.Create('Query command is not ready');
  end;

  if FGenerationReady <> nil then
  begin
    FGenerationReady.CheckReady;
  end;

  if FFormReady <> nil then
  begin
    FFormReady.CheckReady;
  end;
  Flags;
end;

procedure TCommandFence.Install;
begin
  FInstalled := True;
end;

procedure TCommandFence.Notify;
begin
end;

procedure TCommandFence.Retire;
begin
end;

constructor TFormObserver.Create(AForm: TFormPort; const ALease: IInterface);
begin
  inherited Create;
  FForm := AForm;
  FLease := ALease;
end;

procedure TFormObserver.Observe(const AEvent: TNyxEventInfo; ACallbackCount: Integer);
var
  LOwner: TWorkbench;
  LLease: INyxCollectionQueryWorkbench;
begin

  if not FForm.Active or not FForm.Parent.Active or not FForm.Parent.Enabled or
    (FForm.Parent.Owner = nil) then
  begin
    Exit;
  end;
  LOwner := FForm.Parent.Owner;
  LLease := LOwner;
  LOwner.CaptureAction(FForm, AEvent);
end;

procedure TWorkbench.Initialize(const AView: INyxCollectionView;
  const AEvents: INyxEvents; const ATarget: TNyxEventTarget;
  const APresenters: INyxQueryFormPresenterFactory; const ADesignID: TNyxText;
  APurpose: TWorkbenchPurpose);
begin
  FPurpose := APurpose;
  FView := AView;
  FEvents := AEvents;
  FPresenters := APresenters;
  FTarget := ATarget;
  FDesignID := ADesignID;

  if (FView = nil) or (FEvents = nil) or (FPresenters = nil) or
    not (FView.Projection in [cpList, cpTable]) or
    ((FPurpose = wpTableControls) and (FView.Projection <> cpTable)) or (FTarget.ID = '') then
  begin
    raise ENyxCollection.Create('Query workbench requires an exact table, router and presenter port');
  end;
  FScheduler := FEvents.Scheduler;
  FScheduler.RequireUI;
  FSession := NyxCollectionQuerySession(FView);

  if FPurpose = wpQueryDetails then
  begin
    FActor := FSession.AcquireOwner(NyxQueryOrigin('query-detail-controls'), 'Query details');
  end
  else
  begin
    FActor := FSession.AcquireOwner(NyxQueryOrigin('table-query-controls'), 'Table query controls');
  end;
  FPort := TWorkbenchPort.Create;
  FPortLease := FPort;
  FPort.Owner := Self;
  FPort.Active := True;
  FPort.Enabled := True;
  FColumn := -1;
  FViewToken := FView.Subscribe(Changed);
end;

destructor TWorkbench.Destroy;
begin
  try
    Disconnect;
  except
    { Explicit Disconnect reports cleanup errors; destruction cannot revive a
      receiver or abandon the remaining independently owned model leases. }
  end;
  inherited Destroy;
end;

function TWorkbench.GetConnected: Boolean;
begin
  { Raw query takeover retires an actor generation, not its still mounted table.
    Only a fresh user intention may join the new session with another actor. }
  Result := (FPort <> nil) and FPort.Active and (FPort.Owner = Self) and
    not FDisconnected and (FView <> nil);
end;

procedure TWorkbench.RequireActive;
var
  LScheduler: INyxScheduler;
  LView: INyxCollectionView;
  LSpec: TNyxCollectionViewSpec;
begin
  LScheduler := FScheduler;
  LView := FView;

  if not GetConnected or not FPort.Enabled or (LScheduler = nil) then
  begin
    raise ENyxCollection.Create('Query controls are disconnected or disabled');
  end;
  LScheduler.RequireUI;

  if not GetConnected or not FPort.Enabled or (FView <> LView) then
  begin
    raise ENyxCollection.Create('Query controls retired during UI admission');
  end;
  LSpec := LView.Spec;

  if not GetConnected or not FPort.Enabled or (FView <> LView) then
  begin
    raise ENyxCollection.Create('Query controls retired during policy admission');
  end;

  if (FPurpose = wpTableControls) and not LSpec.TableQueryPolicy.IsEnabled then
  begin
    raise ENyxCollection.Create('Table query controls are disabled by policy');
  end;
end;

function TWorkbench.Advance(AJoin: Boolean): Integer;
var
  LSerial: Integer;
  LActor, LNext: INyxCollectionQueryOwner;
  LConnected: Boolean;
begin

  if not GetConnected or not FPort.Enabled then
  begin
    raise ENyxCollection.Create('Query controls are disconnected or disabled');
  end;

  if FPort.Serial = High(Integer) then
  begin
    raise ENyxCollection.Create('Query command intent exhausted');
  end;
  Inc(FPort.Serial);
  LSerial := FPort.Serial;
  RequireActive;
  LActor := FActor;
  LConnected := (LActor <> nil);

  if LConnected then
  begin
    LConnected := LActor.Connected;
  end;

  if not GetConnected or (FPort.Serial <> LSerial) then
  begin
    raise ENyxCollection.Create('Query intention was superseded during admission');
  end;

  if not LConnected then
  begin

    if not AJoin then
    begin
      raise ENyxCollection.Create('This form belongs to a retired query session. Reopen it.');
    end;
    RetireForm;
    RequireActive;
    if FPurpose = wpQueryDetails then
    begin
      LNext := FSession.AcquireOwner(NyxQueryOrigin('query-detail-controls'), 'Query details');
    end
    else
    begin
      LNext := FSession.AcquireOwner(NyxQueryOrigin('table-query-controls'), 'Table query controls');
    end;

    if not GetConnected or (FPort.Serial <> LSerial) then
    begin
      LNext.Revoke;
      raise ENyxCollection.Create('Query owner acquisition was superseded');
    end;
    FActor := LNext;
  end;
  Result := LSerial;
end;

procedure TWorkbench.CheckColumn(AColumn: Integer; AFilter: Boolean);
var
  LSpec: TNyxCollectionViewSpec;
begin
  RequireActive;

  if FPurpose = wpQueryDetails then
  begin
    raise ENyxCollection.Create('Query details do not grant a column editor');
  end;
  LSpec := FView.Spec;

  if (AColumn < 0) or (AColumn >= LSpec.Count) then
  begin
    raise ENyxCollection.Create('Query column is not in this table');
  end;

  if AFilter and (not LSpec.TableQueryPolicy.FilterEnabled or
    not LSpec.ColumnAt(AColumn).QueryPolicy.FilterEnabled) then
  begin
    raise ENyxCollection.Create('Column filtering is disabled');
  end;

  if not AFilter and (not LSpec.TableQueryPolicy.SortEnabled or
    not LSpec.ColumnAt(AColumn).QueryPolicy.SortEnabled) then
  begin
    raise ENyxCollection.Create('Column sorting is disabled');
  end;
end;

function TWorkbench.GetObservation: TNyxQueryWorkbenchObservation;
var
  LLease: INyxCollectionQueryWorkbench;
begin
  LLease := Self;

  if not GetConnected then
  begin
    raise ENyxCollection.Create('Disconnected query controls have no live observation');
  end;
  Result := TNyxQueryWorkbenchObservation.Read(FView);
end;

function TWorkbench.GetPresenter: INyxQueryFormPresenter;
begin
  Result := FPresenter;
end;

procedure TWorkbench.Status(const AText: TNyxText);
var
  LPresenter: INyxQueryFormPresenter;
begin
  LPresenter := FPresenter;

  if (FPort <> nil) and FPort.Active and (LPresenter <> nil) then
  begin
    LPresenter.SetStatus(AText);
  end;
end;

procedure TWorkbench.Changed(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
var
  LLease: INyxCollectionQueryWorkbench;
  LObservation: TNyxQueryWorkbenchObservation;
begin
  LLease := Self;

  if not GetConnected then
  begin
    Exit;
  end;
  LObservation := GetObservation;
  Status(TNyxText(IntToStr(LObservation.MatchedRows)) + ' matching source rows / ' +
    TNyxText(IntToStr(LObservation.SourceRows)) + ' source rows. Drafts are unchanged.');
end;

procedure TWorkbench.RetireForm;
var
  LPresenter: INyxQueryFormPresenter;
  LTokens: array of INyxEventSubscription;
  LIndex: Integer;
  LError: TNyxText;
begin

  if FForm <> nil then
  begin
    FForm.Active := False;
  end;
  LPresenter := FPresenter;
  FPresenter := nil;
  LTokens := FSubscriptions;
  FSubscriptions := nil;
  FForm := nil;
  FFormLease := nil;
  FFormCapture := nil;
  LError := '';
  for LIndex := 0 to High(LTokens) do
  begin
    try
      LTokens[LIndex].Cancel;
    except
      on E: Exception do
      begin

        if LError = '' then
        begin
          LError := TNyxText(E.Message);
        end;
      end;
    end;
  end;

  if LPresenter <> nil then
  begin
    try
      LPresenter.Disconnect;
    except
      on E: Exception do
      begin

        if LError = '' then
        begin
          LError := TNyxText(E.Message);
        end;
      end;
    end;
  end;

  if LError <> '' then
  begin
    raise ENyxCollection.Create(LError);
  end;
end;

procedure TWorkbench.OpenForm(AColumn: Integer; const AQuery: TNyxCollectionQuery;
  const ACapture: INyxCollectionQuerySessionSnapshot);
var
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LEditor: INyxCard;
  LLabel: INyxLabel;
  LOptions: TNyxQueryEditorOptions;
  LObservation: TNyxQueryWorkbenchObservation;
  LObservations: INyxEventObservation;
  LObserver: INyxEventObserver;
  LPresenter: INyxQueryFormPresenter;
  LForm: TFormPort;
  LFormLease: IInterface;
  LTokens: array of INyxEventSubscription;
  LIndex: Integer;
  LLease: INyxCollectionQueryWorkbench;
  LSerial: Integer;
  LNewBaseline: TNyxText;
  LAdopted: Boolean;
  LView: INyxCollectionView;
  LFactory: INyxQueryFormPresenterFactory;
  LSpec: TNyxCollectionViewSpec;
  LStore: INyxCollection;
  LSchema: TNyxCollectionSchema;
  LTarget: TNyxEventTarget;
  LPopover: TNyxPopoverOptions;
  LCaption: TNyxText;
  LContribution: TNyxQueryContribution;
  LDetailsOwner: TNyxQueryOwnerID;

  procedure Current;
  begin

    if not GetConnected or not FPort.Enabled or (FPort.Serial <> LSerial) or
      (FView <> LView) or (FPresenters <> LFactory) then
    begin
      raise ENyxCollection.Create('Query form opening retired during admission');
    end;
  end;

  procedure ObserveNodes(ANode: TNyxNode);
  var
    LChild, LCount: Integer;
    LTrigger: TNyxTrigger;
    LToken: INyxEventSubscription;
  begin

    if ANode.ProjectionKind = NyxKindName(nkButton) then
    begin
      LTrigger := ntClick;
    end
    else
    begin
      LTrigger := ntChange;
    end;

    if ANode.ProjectionKind = NyxKindName(nkButton) then
    begin
      LToken := LObservations.Observe(NyxControlEvents(ANode.ID, niRuntime),
        LTrigger, LObserver, nopBeforeCallbacks, NyxEvent(''));
      LCount := Length(LTokens);
      SetLength(LTokens, LCount + 1);
      LTokens[LCount] := LToken;
    end
    else if (ANode.ProjectionKind = NyxKindName(nkInput)) or
      (ANode.ProjectionKind = NyxKindName(nkSelect)) or
      (ANode.ProjectionKind = NyxKindName(nkCheckbox)) then
    begin
      LToken := LObservations.Observe(NyxControlEvents(ANode.ID, niRuntime),
        LTrigger, LObserver, nopBeforeCallbacks, NyxEvent(''));
      LCount := Length(LTokens);
      SetLength(LTokens, LCount + 1);
      LTokens[LCount] := LToken;
    end;
    for LChild := 0 to ANode.Count - 1 do
    begin
      ObserveNodes(ANode.Children[LChild]);
    end;
  end;

  procedure Button(const AID, ACaption: TNyxText);
  var
    LButton: INyxButton;
  begin
    LButton := NewNyxButton(AID);
    LButton.Configure.Text(ACaption).AccessibleName(ACaption).Done;

    if AColumn = CDetailsColumn then
    begin
      LButton.Configure.PartName(NyxPart(AID)).Done;
    end;
    LEditor.Add(LButton);
  end;

begin
  LLease := Self;
  RequireActive;
  LSerial := FPort.Serial;
  LView := FView;
  LFactory := FPresenters;
  LTarget := FTarget;
  LDetailsOwner := FDetailsOwner;
  LSpec := LView.Spec;
  Current;
  LStore := LView.Store;
  Current;
  LSchema := LStore.Snapshot.Schema;
  Current;
  LAdopted := False;
  LForm := nil;
  LOptions := NyxQueryEditorOptions.SortKeys;

  if AColumn >= 0 then
  begin
    CheckColumn(AColumn, True);
    LOptions := NyxQueryEditorOptions.ColumnFilter(AColumn);
  end;
  LDocument := TNyxDocument.Create;
  try
    LDocument.State.SetValue(NyxQueryFormStatusState, '');
    LPage := NewNyxPage(CPage);
    LPage.Configure.Padding(8).Gap(8).Done;

    if AColumn = CDetailsColumn then
    begin
      LEditor := NewNyxCard(NyxQueryWorkbenchEditorID);
      LEditor.Configure.Gap(10).Padding(12).Done;
      LEditor.Node.SetProp(NyxMenuFormOwnerKey, LTarget.ID).
        SetProp(NyxMenuFormBaselineKey, NyxQueryEditorBaseline(LSchema, LSpec));
      LEditor.Add(NewNyxHeading(NyxQueryWorkbenchEditorID + '-title').WithText('Filters and sorting'));
      LEditor.Add(NewNyxLabel(NyxQueryWorkbenchEditorID + '-help').WithText(
        'Inspect the rules captured when this panel opened. Reload to see current rules. ' +
        'Query changes leave control entries intact; a toolbar Clear resets its own entries.'));
    end
    else
    begin
      LEditor := NewNyxQueryEditor(NyxQueryWorkbenchEditorID, NyxControl(LTarget.ID),
        LSchema, LSpec, AQuery, LOptions);
    end;
    LEditor.Configure.PartName(NyxPart('editor')).Done;
    LObservation := GetObservation;
    Current;
    LEditor.Add(NewNyxLabel(CCounts).Configure.Text(
      TNyxText(IntToStr(LObservation.MatchedRows)) + ' matching source rows / ' +
      TNyxText(IntToStr(LObservation.SourceRows)) + ' source rows').Done);

    if LObservation.MatchedRows = 0 then
    begin

      if LObservation.SourceRows = 0 then
      begin
        LEditor.Add(NewNyxLabel(CCounts + '-empty').Configure.Text(
          'No source rows are available.').Done);
      end
      else
      begin
        LEditor.Add(NewNyxLabel(CCounts + '-empty').Configure.Text(
          'No matching rows. Clear user filters or review authored rules; source rows are intact.').Done);
      end;
    end;
    for LIndex := 0 to LObservation.Count - 1 do
    begin
      LContribution := LObservation.ContributionAt(LIndex);
      LCaption := 'Remove ' +
        PredicateCaption(NyxPredicateFromData(LObservation.ContributionAt(LIndex).Filter),
          LSpec) + TNyxText(' — ') + LContribution.Owner.Caption;

      if AColumn = CDetailsColumn then
      begin

        if not LContribution.Owner.Connected then
        begin
          LCaption := LCaption + ' (retained from earlier activity)';
        end
        else if LDetailsOwner.Defined and (LDetailsOwner.Value = LContribution.Owner.ID.Value) then
        begin
          LCaption := LCaption + ' (this toolbar)';
        end
        else
        begin
          LCaption := LCaption + ' (another control, active when opened)';
        end;
      end;
      Button(CChipPrefix + TNyxText(IntToStr(LIndex)), LCaption);
    end;

    if AColumn = CDetailsColumn then
    begin

      if LObservation.HasUserSort then
      begin
        LCaption := 'Shared sorting belongs to ' + LObservation.SortOwner.Caption;

        if not LObservation.SortOwner.Connected then
        begin
          LCaption := LCaption + ' (retained from earlier activity).';
        end
        else if LDetailsOwner.Defined and
          (LDetailsOwner.Value = LObservation.SortOwner.ID.Value) then
        begin
          LCaption := LCaption + ' (this toolbar).';
        end
        else
        begin
          LCaption := LCaption + ' (another control, active when opened).';
        end;
      end
      else
      begin
        LCaption := 'Original ordering; no user sort is active.';
      end;
      LEditor.Add(NewNyxLabel(CCounts + '-sort').WithText(LCaption));
    end;

    if AColumn <> CDetailsColumn then
    begin
      Button(CClearOwner, 'Clear this surface');
    end;
    Button(CClearAll, 'Clear all user filters');
    Button(CResetSort, 'Reset shared sorting');
    Button(CResetAll, 'Reset all user filters and sorting');

    if AColumn <> CDetailsColumn then
    begin
      Button(COpenSort, 'Edit shared sorting');
    end;
    Button(CReload, 'Discard draft and reload current rules');
    LLabel := NewNyxLabel(CStatus);
    LLabel.Binds.Text(NyxQueryFormStatusState).Done;
    LEditor.Add(LLabel);
    LPage.Add(LEditor);
    LDocument.AddPage(LPage);
    LNewBaseline := LEditor.Node.Prop(NyxMenuFormBaselineKey);
    { -2 is controller-local purpose, not a physical column. Existing target
      factories use -1 for a whole collection, including List anchors. }

    if AColumn = CDetailsColumn then
    begin
      LPresenter := LFactory.CreateForm(-1, LDocument, NyxPageRoot(CPage));
    end
    else
    begin
      LPresenter := LFactory.CreateForm(AColumn, LDocument, NyxPageRoot(CPage));
    end;
    Current;
    RequireActive;

    if (FPort.Serial <> LSerial) or (LPresenter = nil) then
    begin
      raise ENyxCollection.Create('Query form creation was superseded');
    end;
    LForm := TFormPort.Create;
    LFormLease := LForm;
    LForm.Parent := FPort;
    LForm.ParentLease := FPortLease;
    LForm.Active := False;
    LObserver := TFormObserver.Create(LForm, LFormLease);

    if not Supports(LPresenter.Events, INyxEventObservation, LObservations) then
    begin
      raise ENyxCollection.Create('Query form router requires ordinary input observation');
    end;
    ObserveNodes(LPage.Node);
    LPopover := NyxPopover('Filter and sort').Size(480, 660).Focus(NyxPart('editor/field'));

    if AColumn = CDetailsColumn then
    begin
      LPopover := NyxPopover('Filters and sorting').Size(480, 660).
        Focus(NyxPart('editor/' + CClearAll));
    end;
    LPresenter.Show(LPopover);
    RequireActive;

    if FPort.Serial <> LSerial then
    begin
      raise ENyxCollection.Create('Query form opening was superseded');
    end;
    RetireForm;
    RequireActive;

    if FPort.Serial <> LSerial then
    begin
      raise ENyxCollection.Create('Query form retirement was superseded');
    end;
    FPresenter := LPresenter;
    FForm := LForm;
    FFormLease := LFormLease;
    FFormCapture := ACapture;
    FSubscriptions := LTokens;
    FFormBaseline := LNewBaseline;
    FColumn := AColumn;
    FFormObservation := LObservation.Copy;
    LForm.Active := True;
    LAdopted := True;
  finally
    LDocument.Free;

    if not LAdopted then
    begin

      if LForm <> nil then
      begin
        LForm.Active := False;
      end;
      for LIndex := 0 to High(LTokens) do
      begin
        try
          LTokens[LIndex].Cancel;
        except
          { Preserve the original admission error and attempt every cleanup. }
        end;
      end;

      if LPresenter <> nil then
      begin
        try
          LPresenter.Disconnect;
        except
          { The rejected target is independently revoked, never installed. }
        end;
      end;
    end;
  end;
  { Closing the former host may restore its invoker after candidate admission.
    Reapply explicit focus only to this still accepted form; focus callbacks may
    reenter, so this is the final outward call and no receiver access follows. }

  if GetConnected and FPort.Enabled and (FPort.Serial = LSerial) and
    (FForm = LForm) and LForm.Active and (FPresenter = LPresenter) then
  begin
    LPresenter.Show(LPopover);
  end;
end;

procedure TWorkbench.OpenColumn(AColumn: Integer);
var
  LSnapshot: INyxCollectionQuerySessionSnapshot;
  LQuery: TNyxCollectionQuery;
  LIndex: Integer;
  LContribution: TNyxQueryContribution;
  LPredicate: INyxCollectionPredicate;
  LLease: INyxCollectionQueryWorkbench;
  LSession: INyxCollectionQuerySession;
  LActor: INyxCollectionQueryOwner;
  LView: INyxCollectionView;
  LSpec: TNyxCollectionViewSpec;
  LSerial, LActorID: Integer;
begin
  LLease := Self;
  LSerial := Advance;
  LSession := FSession;
  LActor := FActor;
  LView := FView;
  CheckColumn(AColumn, True);

  if (FPresenter <> nil) and (FForm <> nil) and FForm.Active and (FColumn = AColumn) then
  begin
    FPresenter.Show(NyxPopover('Filter column').Size(480, 660).Focus(NyxPart('editor/field')));
    Exit;
  end;
  LSnapshot := LSession.Capture;

  if not GetConnected or (FPort.Serial <> LSerial) then
  begin
    raise ENyxCollection.Create('Column form was superseded during capture');
  end;
  LActorID := LActor.ID.Value;
  LSpec := LView.Spec;

  if not GetConnected or (FPort.Serial <> LSerial) then
  begin
    raise ENyxCollection.Create('Column form was superseded during policy read');
  end;
  LQuery := NyxCollectionQuery;
  for LIndex := 0 to LSnapshot.Count - 1 do
  begin
    LContribution := LSnapshot.ContributionAt(LIndex);

    if (LContribution.Owner.ID.Value = LActorID) and
      (LContribution.Channel.Name = LSpec.ColumnAt(AColumn).FieldName) then
    begin
      LPredicate := NyxPredicateFromData(LContribution.Filter);
      LQuery := LQuery.Where(LPredicate);
    end;
  end;
  OpenForm(AColumn, LQuery, LSnapshot);
end;

procedure TWorkbench.OpenSort;
var
  LSnapshot: INyxCollectionQuerySessionSnapshot;
  LQuery: TNyxCollectionQuery;
  LLease: INyxCollectionQueryWorkbench;
  LSession: INyxCollectionQuerySession;
  LView: INyxCollectionView;
  LSerial: Integer;
begin
  LLease := Self;

  if FPurpose = wpQueryDetails then
  begin
    raise ENyxCollection.Create('Query details do not grant a sort editor');
  end;
  LSerial := Advance;
  LSession := FSession;
  LView := FView;

  if not LView.Spec.TableQueryPolicy.SortEnabled then
  begin
    raise ENyxCollection.Create('Table sorting is disabled');
  end;
  LSnapshot := LSession.Capture;

  if not GetConnected or (FPort.Serial <> LSerial) then
  begin
    raise ENyxCollection.Create('Sort form was superseded during capture');
  end;
  LQuery := NyxCollectionQuery;

  if LSnapshot.HasUserSort then
  begin
    LQuery := LSnapshot.UserSort;
  end;
  OpenForm(-1, LQuery, LSnapshot);
end;

procedure TWorkbench.OpenDetails;
begin
  OpenDetailsFor(Default(TNyxQueryOwnerID));
end;

procedure TWorkbench.OpenDetailsFor(const ALocalOwner: TNyxQueryOwnerID);
var
  LLease: INyxCollectionQueryWorkbench;
  LSession: INyxCollectionQuerySession;
  LSnapshot: INyxCollectionQuerySessionSnapshot;
  LSerial: Integer;
begin
  LLease := Self;
  LSerial := Advance;
  FDetailsOwner := ALocalOwner;
  LSession := FSession;
  LSnapshot := LSession.Capture;

  if not GetConnected or (FPort.Serial <> LSerial) then
  begin
    raise ENyxCollection.Create('Query details were superseded during capture');
  end;
  OpenForm(CDetailsColumn, NyxCollectionQuery, LSnapshot);
end;

procedure TWorkbench.ReloadDraft;
var
  LLease: INyxCollectionQueryWorkbench;
  LSession: INyxCollectionQuerySession;
  LActor: INyxCollectionQueryOwner;
  LView: INyxCollectionView;
  LSnapshot: INyxCollectionQuerySessionSnapshot;
  LSpec: TNyxCollectionViewSpec;
  LQuery: TNyxCollectionQuery;
  LEntry: TNyxQueryContribution;
  LSerial, LColumn, LOwnerID, LIndex: Integer;
begin
  LLease := Self;

  if FForm = nil then
  begin
    raise ENyxCollection.Create('Open a query form before explicitly reloading its draft');
  end;
  LColumn := FColumn;
  LSerial := Advance;
  LSession := FSession;
  LActor := FActor;
  LView := FView;
  LSnapshot := LSession.Capture;
  LOwnerID := LActor.ID.Value;
  LSpec := LView.Spec;

  if not GetConnected or (FPort.Serial <> LSerial) then
  begin
    raise ENyxCollection.Create('Draft reload was superseded while reading current rules');
  end;
  LQuery := NyxCollectionQuery;

  if LColumn = CDetailsColumn then
  begin
    OpenForm(CDetailsColumn, LQuery, LSnapshot);
    Exit;
  end;

  if LColumn >= 0 then
  begin
    CheckColumn(LColumn, True);
    for LIndex := 0 to LSnapshot.Count - 1 do
    begin
      LEntry := LSnapshot.ContributionAt(LIndex);

      if (LEntry.Owner.ID.Value = LOwnerID) and
        (LEntry.Channel.Name = LSpec.ColumnAt(LColumn).FieldName) then
      begin
        LQuery := LQuery.Where(NyxPredicateFromData(LEntry.Filter));
      end;
    end;
  end
  else if LSnapshot.HasUserSort then
  begin
    LQuery := LSnapshot.UserSort;
  end;
  { OpenForm stages and shows a complete candidate before revoking the old
    form. A failed candidate leaves the original presenter's text/capture intact. }
  OpenForm(LColumn, LQuery, LSnapshot);
end;

function TWorkbench.SortKey(AColumn: Integer; ADirection: TNyxSortDirection): TNyxCollectionQuery;
var
  LColumn: TNyxCollectionColumn;
  LView: INyxCollectionView;
begin
  LView := FView;
  CheckColumn(AColumn, False);
  LColumn := LView.Spec.ColumnAt(AColumn);
  RequireActive;
  Result := NyxCollectionQuery;
  case LColumn.Kind of
    nskText: Result := Result.OrderBy(NyxTextField(LColumn.FieldName), ADirection, LColumn.QueryPolicy.TextMode);
    nskBoolean: Result := Result.OrderBy(NyxBooleanField(LColumn.FieldName), ADirection);
    nskInteger: Result := Result.OrderBy(NyxIntegerField(LColumn.FieldName), ADirection);
    nskNumber: Result := Result.OrderBy(NyxNumberField(LColumn.FieldName), ADirection);
  end;
end;

procedure TWorkbench.CheckSort(const AQuery: TNyxCollectionQuery);
var
  LOptions: TNyxQueryEditorOptions;
  LEditor: INyxCard;
  LView: INyxCollectionView;
  LSpec: TNyxCollectionViewSpec;
  LSchema: TNyxCollectionSchema;
  LTarget: TNyxEventTarget;
begin
  { The reusable editor owns the existing typed policy admission, including
    fields, families, duplicate keys, single/multiple and text comparison. }
  LOptions := NyxQueryEditorOptions.SortKeys;
  LView := FView;
  LTarget := FTarget;
  LSpec := LView.Spec;
  LSchema := LView.Store.Snapshot.Schema;
  RequireActive;
  LEditor := NewNyxQueryEditor(NyxQueryWorkbenchEditorID, NyxControl(LTarget.ID),
    LSchema, LSpec, AQuery, LOptions);
end;

procedure TWorkbench.CheckEdits(const AEdits: TNyxQuerySessionEdits;
  const ACapture: INyxCollectionQuerySessionSnapshot);
var
  LEdit: TNyxQuerySessionEdit;
  LSpec: TNyxCollectionViewSpec;
  LIndex, LColumn, LContribution: Integer;
  LEditor: INyxCard;
  LValue: TNyxQueryContribution;
  LView: INyxCollectionView;
  LActor: INyxCollectionQueryOwner;
begin
  LView := FView;
  LActor := FActor;
  LSpec := LView.Spec;
  RequireActive;

  if FPurpose = wpQueryDetails then
  begin
    for LIndex := 0 to AEdits.Count - 1 do
    begin

      if not (AEdits.EditAt(LIndex).Kind in [nqseRemoveFilter,
        nqseClearAllFilters, nqseResetSort, nqseResetAll]) then
      begin
        raise ENyxCollection.Create('Query details allow captured removal and explicit global reset only');
      end;
    end;
    Exit;
  end;
  for LIndex := 0 to AEdits.Count - 1 do
  begin
    LEdit := AEdits.EditAt(LIndex);
    case LEdit.Kind of
      nqseFilter, nqseClearFilter:
      begin
        LColumn := 0;
        while (LColumn < LSpec.Count) and
          (LSpec.ColumnAt(LColumn).FieldName <> LEdit.Channel.Name) do
        begin
          Inc(LColumn);
        end;
        CheckColumn(LColumn, True);

        if LEdit.Kind = nqseFilter then
        begin
          LEditor := NewNyxQueryEditor(NyxQueryWorkbenchEditorID, NyxControl(FTarget.ID),
            LView.Store.Snapshot.Schema, LSpec, LEdit.Query,
            NyxQueryEditorOptions.ColumnFilter(LColumn));
        end;
      end;
      nqseSortBy: CheckSort(LEdit.Query);
      nqseResetSort:
      begin

        if not LSpec.TableQueryPolicy.SortEnabled then
        begin
          raise ENyxCollection.Create('Table sorting is disabled');
        end;
      end;
      nqseClearAllFilters, nqseRemoveFilter:
      begin

        if not LSpec.TableQueryPolicy.FilterEnabled then
        begin
          raise ENyxCollection.Create('Table filtering is disabled');
        end;
      end;
      nqseResetAll:
      begin

        if not LSpec.TableQueryPolicy.FilterEnabled or not LSpec.TableQueryPolicy.SortEnabled then
        begin
          raise ENyxCollection.Create('A complete table query reset is disabled');
        end;
      end;
      nqseClearOwner:
      begin
        for LContribution := 0 to ACapture.Count - 1 do
        begin
          LValue := ACapture.ContributionAt(LContribution);

          if (LValue.Owner.ID.Value = LActor.ID.Value) and not LSpec.TableQueryPolicy.FilterEnabled then
          begin
            raise ENyxCollection.Create('This surface cannot withdraw filters while filtering is disabled');
          end;
        end;

        if ACapture.HasUserSort and (ACapture.SortOwner.ID.Value = LActor.ID.Value) and
          not LSpec.TableQueryPolicy.SortEnabled then
        begin
          raise ENyxCollection.Create('This surface cannot reset sorting while sorting is disabled');
        end;
      end;
    end;
  end;
end;

procedure TWorkbench.SortColumn(AColumn: Integer; ADirection: TNyxSortDirection;
  AAppend: Boolean);
var
  LQuery, LKey: TNyxCollectionQuery;
  LData: TNyxDataValue;
  LValues: array of TNyxDataValue;
  LIndex, LCount: Integer;
  LLease: INyxCollectionQueryWorkbench;
  LCapture: INyxCollectionQuerySessionSnapshot;
begin
  LLease := Self;
  Advance;
  LKey := SortKey(AColumn, ADirection);
  LQuery := LKey;

  if AAppend then
  begin

    if FView.Spec.TableQueryPolicy.Mode <> nqsmMultiple then
    begin
      raise ENyxCollection.Create('Multiple sort keys are disabled');
    end;
    LCapture := FSession.Capture;
    LQuery := NyxCollectionQuery;

    if LCapture.HasUserSort then
    begin
      LQuery := LCapture.UserSort;
    end;
    SetLength(LValues, LQuery.SortCount + 1);
    LCount := 0;
    for LIndex := 0 to LQuery.SortCount - 1 do
    begin

      if LQuery.SortAt(LIndex).FieldName <> LKey.SortAt(0).FieldName then
      begin
        LValues[LCount] := LQuery.SortAt(LIndex).ToData;
        Inc(LCount);
      end;
    end;
    LValues[LCount] := LKey.SortAt(0).ToData;
    SetLength(LValues, LCount + 1);
    LData := NyxObject([NyxField('version', NyxData(1)), NyxField('filter', NyxNull),
      NyxField('order', NyxArray(LValues))]);
    LQuery := TNyxCollectionQuery.FromData(LData);
  end;
  CheckSort(LQuery);
  Direct(nqwcSortColumn, AColumn, NyxQuerySessionEdits.SortBy(LQuery));
end;

procedure TWorkbench.ClearColumn(AColumn: Integer);
var
  LLease: INyxCollectionQueryWorkbench;
begin
  LLease := Self;
  Advance;
  CheckColumn(AColumn, True);
  Direct(nqwcClearColumn, AColumn, NyxQuerySessionEdits.ClearFilter(
    NyxQueryChannel(FView.Spec.ColumnAt(AColumn).FieldName)));
end;

procedure TWorkbench.ClearOwner;
begin
  Direct(nqwcClearOwner, -1, NyxQuerySessionEdits.ClearOwner);
end;

procedure TWorkbench.ClearAllFilters;
begin
  Direct(nqwcClearAllFilters, -1, NyxQuerySessionEdits.ClearAllFilters);
end;

procedure TWorkbench.ResetSort;
begin
  Direct(nqwcResetSort, -1, NyxQuerySessionEdits.ResetSort);
end;

procedure TWorkbench.ResetAll;
begin
  Direct(nqwcResetAll, -1, NyxQuerySessionEdits.ResetAll);
end;

procedure TWorkbench.RemoveFilter(const AOwner: TNyxQueryOwnerID;
  const AChannel: TNyxQueryChannelRef);
begin
  Direct(nqwcRemoveFilter, -1, NyxQuerySessionEdits.RemoveFilter(AOwner, AChannel));
end;

procedure TWorkbench.Direct(ACommand: TNyxQueryWorkbenchCommand; AColumn: Integer;
  const AEdits: TNyxQuerySessionEdits);
var
  LWork: TQueryWork;
  LLease: INyxWork;
  LSelf: INyxCollectionQueryWorkbench;
  LEvents: INyxEvents;
  LSession: INyxCollectionQuerySession;
  LScheduler: INyxScheduler;
begin
  LSelf := Self;
  Advance;
  LEvents := FEvents;
  LSession := FSession;
  LScheduler := FScheduler;
  LWork := TQueryWork.Create;
  LLease := LWork;
  LWork.Port := FPort;
  LWork.PortLease := FPortLease;
  LWork.Serial := FPort.Serial;
  LWork.Generation := NyxEventGeneration(LEvents);

  if not GetConnected or (LWork.Serial <> FPort.Serial) then
  begin
    raise ENyxCollection.Create('Query command retired while capturing its generation');
  end;
  LWork.Scheduler := LScheduler;
  LWork.OnUI := True;
  LWork.Column := AColumn;
  LWork.Command := ACommand;
  LWork.Edits := AEdits.Copy;
  LWork.Capture := LSession.Capture;

  if not GetConnected or (LWork.Serial <> FPort.Serial) then
  begin
    raise ENyxCollection.Create('Query command retired while capturing its session');
  end;
  Publish(LWork);
end;

procedure TWorkbench.Submit(AWork: TQueryWork; APolicy: TNyxExecutionPolicy);
var
  LWork: INyxWork;
  LExecution: INyxExecution;
  LPort: TWorkbenchPort;
  LPortLease: IInterface;
  LIndex, LCount: Integer;
  LScheduler: INyxScheduler;
  LPrevious: TNyxExecutions;
begin
  LWork := AWork;
  LPort := FPort;
  LPortLease := FPortLease;
  LScheduler := FScheduler;
  LPrevious := FExecutions;
  AWork.OnUI := APolicy in [neSequential, neUIQueue];
  LExecution := LScheduler.Submit(LWork, APolicy);

  if not LPort.Active or (LPort.Owner <> Self) then
  begin
    LExecution.Cancel;
    Exit;
  end;
  LCount := 0;
  for LIndex := 0 to High(LPrevious) do
  begin

    if LPrevious[LIndex].Status in [nesPending, nesRunning] then
    begin
      LPrevious[LCount] := LPrevious[LIndex];
      Inc(LCount);
    end;
  end;

  if not LPort.Active or (LPort.Owner <> Self) then
  begin
    LExecution.Cancel;
    Exit;
  end;
  SetLength(LPrevious, LCount + 1);
  LPrevious[LCount] := LExecution;
  FExecutions := LPrevious;
end;

procedure TWorkbench.CaptureAction(AForm: TFormPort; const AEvent: TNyxEventInfo);
var
  LWork: TQueryWork;
  LWorkLease: INyxWork;
  LChange: TNyxQueryEditorChange;
  LAction: TNyxText;
  LIndex: Integer;
  LObservation: TNyxQueryWorkbenchObservation;
  LPresenter: INyxQueryFormPresenter;
  LLease: INyxCollectionQueryWorkbench;
  LEvents, LFormEvents: INyxEvents;
  LView: INyxCollectionView;
  LScheduler: INyxScheduler;
  LBaseline: TNyxText;
  LTarget: TNyxEventTarget;
  LPolicy: TNyxExecutionPolicy;

  procedure Current;
  begin

    if not GetConnected or (FForm <> AForm) or not AForm.Active or
      (LWork.Serial <> FPort.Serial) then
    begin
      raise ENyxCollection.Create('Query input was superseded during capture');
    end;
  end;
begin
  LLease := Self;

  if (AForm <> FForm) or not AForm.Active then
  begin
    Exit;
  end;
  LPresenter := FPresenter;
  try
    Advance(False);

    if AEvent.Trigger = ntChange then
    begin
      Exit;
    end;
    LAction := AEvent.OriginID;

    if LAction = COpenSort then
    begin
      OpenSort;
      Exit;
    end;

    if LAction = CReload then
    begin
      ReloadDraft;
      Exit;
    end;
    LWork := TQueryWork.Create;
    LWorkLease := LWork;
    LWork.Port := FPort;
    LWork.PortLease := FPortLease;
    LWork.Form := AForm;
    LWork.FormLease := FFormLease;
    LWork.Serial := FPort.Serial;
    LEvents := FEvents;
    LView := FView;
    LScheduler := FScheduler;
    LBaseline := FFormBaseline;
    LTarget := FTarget;
    LObservation := FFormObservation.Copy;
    LWork.Generation := NyxEventGeneration(LEvents);
    Current;
    LFormEvents := LPresenter.Events;
    Current;
    LWork.FormGeneration := NyxEventGeneration(LFormEvents);
    Current;
    LWork.Scheduler := LScheduler;
    LWork.Column := FColumn;

    if LWork.Column = CDetailsColumn then
    begin
      LWork.Column := -1;
    end;
    LWork.Capture := FFormCapture;
    LWork.Command := nqwcApplyFilter;

    if LAction = CClearOwner then
    begin
      LWork.Command := nqwcClearOwner;
      LWork.Edits := NyxQuerySessionEdits.ClearOwner;
    end
    else if LAction = CClearAll then
    begin
      LWork.Command := nqwcClearAllFilters;
      LWork.Edits := NyxQuerySessionEdits.ClearAllFilters;
    end
    else if LAction = CResetSort then
    begin
      LWork.Command := nqwcResetSort;
      LWork.Edits := NyxQuerySessionEdits.ResetSort;
    end
    else if LAction = CResetAll then
    begin
      LWork.Command := nqwcResetAll;
      LWork.Edits := NyxQuerySessionEdits.ResetAll;
    end
    else if Pos(CChipPrefix, LAction) = 1 then
    begin
      LIndex := StrToInt(Copy(LAction, Length(CChipPrefix) + 1, MaxInt));
      LWork.Command := nqwcRemoveFilter;
      LWork.Edits := NyxQuerySessionEdits.RemoveFilter(
        LObservation.ContributionAt(LIndex).Owner.ID, LObservation.ContributionAt(LIndex).Channel);
    end
    else
    begin

      if not LPresenter.Capture(LAction, LChange) then
      begin
        Exit;
      end;
      Current;

      if (LChange.Owner.ID <> LTarget.ID) or (LChange.Baseline <> LBaseline) then
      begin
        raise ENyxCollection.Create('Query form belongs to a different target or baseline');
      end;
      LWork.Query := LChange.Query.Copy;
      LWork.Structural := LAction <> NyxQueryEditorActionID(NyxQueryWorkbenchEditorID, nqeSave);

      if not LWork.Structural then
      begin

        if LWork.Column >= 0 then
        begin

          if LChange.Query.Filter = nil then
          begin
            LWork.Edits := NyxQuerySessionEdits.ClearFilter(NyxQueryChannel(LView.Spec.ColumnAt(LWork.Column).FieldName));
          end
          else
          begin
            LWork.Edits := NyxQuerySessionEdits.Filter(
              NyxQueryChannel(LView.Spec.ColumnAt(LWork.Column).FieldName), LChange.Query.Filter);
          end;
        end
        else
        begin
          LWork.Command := nqwcApplySort;
          LWork.Edits := NyxQuerySessionEdits.SortBy(LChange.Query);
        end;
      end;
    end;
    RequireActive;
    Current;
    LPolicy := LFormEvents.On(NyxControlEvents(LAction, niRuntime), ntClick).ExecutionPolicy;
    Current;
    Submit(LWork, LPolicy);
  except
    on E: Exception do
    begin

      if (FForm = AForm) and AForm.Active and FPort.Active then
      begin
        Status(TNyxText(E.Message));
      end;
    end;
  end;
end;

procedure TWorkbench.ExecuteWork(AWork: TQueryWork);
begin
  RequireActive;
  RequireFormOpen(AWork.Form);

  if (AWork.Serial <> FPort.Serial) or
    ((AWork.Form <> nil) and ((AWork.Form <> FForm) or not AWork.Form.Active)) then
  begin
    raise ENyxCollection.Create('Query form command was superseded');
  end;

  if AWork.Structural then
  begin
    OpenForm(AWork.Column, AWork.Query, AWork.Capture);
  end
  else
  begin
    Publish(AWork);
  end;
end;

function TWorkbench.EventInfo(AKind: TNyxQueryWorkbenchEvent;
  const AChange: TNyxQueryWorkbenchChange): TNyxEventInfo;
begin
  Result := Default(TNyxEventInfo);
  { Event copies own a constructed scalar value even when HasValue is false;
    this producer carries only the independent structured Details payload. }
  Result.Value := NyxNull;
  Result.Trigger := ntNamed;
  Result.Name := NyxQueryWorkbenchEvent(AKind);
  Result.SourceID := FTarget.ID;
  Result.OriginID := FTarget.ID;
  Result.TargetID := FTarget.ID;
  Result.HasDetails := True;
  Result.Details := AChange.ToData;
  Result.Changed := AKind = nqweChanged;
end;

procedure TWorkbench.Publish(AWork: TQueryWork);
var
  LLease: INyxCollectionQueryWorkbench;
  LStage, LFence: INyxPreparedPublication;
  LFenceObject: TCommandFence;
  LBefore, LAfter: TNyxQueryWorkbenchObservation;
  LChange: TNyxQueryWorkbenchChange;
  LActor: TNyxQueryOwnerInfo;
  LStream: INyxEventStream;
  LExecutions: TNyxExecutions;
  LInfo: TNyxEventInfo;
  LIndex: Integer;
  LNotification: TNyxText;
  LCancelled: Boolean;
  LActorLease: INyxCollectionQueryOwner;
  LSession: INyxCollectionQuerySession;
  LEvents: INyxEvents;
  LView: INyxCollectionView;
  LDesign: TNyxText;
  LTarget: TNyxEventTarget;
  LError: TNyxText;
  LNegotiated: Boolean;

  procedure Current;
  begin

    if not GetConnected or not FPort.Enabled or (FPort.Serial <> AWork.Serial) or
      (FActor <> LActorLease) or (FSession <> LSession) or (FEvents <> LEvents) or
      (FView <> LView) or ((AWork.Form <> nil) and not AWork.Form.Active) then
    begin
      raise ENyxCollection.Create('Query command retired during admission');
    end;
  end;

  procedure CancelOutstanding;
  var
    LCancel: Integer;
  begin
    for LCancel := 0 to High(LExecutions) do
    begin
      try

        if LExecutions[LCancel].Status in [nesPending, nesRunning] then
        begin
          LExecutions[LCancel].Cancel;
        end;
      except
        { Every independent invocation is attempted; retain the original query
          refusal rather than hiding it behind a cancellation cleanup error. }
      end;
    end;
  end;

begin
  LLease := Self;
  LActorLease := FActor;
  LSession := FSession;
  LEvents := FEvents;
  LView := FView;
  LDesign := FDesignID;
  LTarget := FTarget;
  LNegotiated := False;
  RequireActive;
  Current;
  LActor.ID := LActorLease.ID;
  Current;
  LActor.Origin := LActorLease.Origin;
  Current;
  LActor.Caption := LActorLease.Caption;
  Current;
  LActor.Connected := LActorLease.Connected;
  Current;
  CheckEdits(AWork.Edits, AWork.Capture);
  Current;
  LFenceObject := TCommandFence.Create(AWork);
  LFence := LFenceObject;
  try
    LStage := LSession.PrepareSession(AWork.Capture, LActorLease, AWork.Edits);
    Current;
    LBefore := TNyxQueryWorkbenchObservation.Read(LView);
    Current;
    LChange := TNyxQueryWorkbenchChange.Proposal(AWork.Command, AWork.Column, LActor, LBefore, AWork.Edits);
    LStream := LEvents.OnNamed(LTarget, NyxQueryWorkbenchEvent(nqweBeforeChange));
    Current;

    if LStream.ExecutionPolicy <> neSequential then
    begin
      raise ENyxCollection.Create('Before-query callbacks require sequential execution');
    end;
    Current;
    LInfo := EventInfo(nqweBeforeChange, LChange);
    LExecutions := LEvents.Dispatch(LInfo, LDesign, LDesign);
    Current;
    LCancelled := False;
    LError := '';
    for LIndex := 0 to High(LExecutions) do
    begin

      if LExecutions[LIndex].Status = nesFailed then
      begin
        LError := LExecutions[LIndex].Failure;
      end;
      LCancelled := LCancelled or LExecutions[LIndex].Cancelled;

      if not (LExecutions[LIndex].Status in [nesSucceeded, nesCancelled]) then
      begin
        LError := 'Before-query callbacks must complete synchronously';
      end;
    end;
    Current;

    if LError <> '' then
    begin
      raise ENyxCollection.Create(LError);
    end;

    if LCancelled then
    begin
      Status('Query change cancelled. Draft retained.');
      Exit;
    end;
    LNegotiated := True;
    LNotification := '';
    try
      { The command fence is the LAST validator: generic cancellation getters
        complete after query admission, before callback-free final fences. }
      PublishNyxGroup([LStage, LFence]);
    except
      on E: ENyxPublicationNotification do
      begin
        LNotification := TNyxText(E.Message);
      end;
    end;

    if LFenceObject.Installed and FPort.Active then
    begin
      LAfter := TNyxQueryWorkbenchObservation.Read(LView);

      if not LBefore.SamePolicy(LAfter) then
      begin
        LChange := LChange.Committed(LAfter);
        LInfo := EventInfo(nqweChanged, LChange);
        LEvents.Dispatch(LInfo, LDesign, LDesign);
      end;

      if FPort.Active then
      begin
        Status('Query applied. ' + TNyxText(IntToStr(LAfter.MatchedRows)) +
          ' matching source rows / ' + TNyxText(IntToStr(LAfter.SourceRows)) + ' source rows.');
      end;
      { Accepted commands discard this form's exact launch capture. A new Open
        observes current chips/policy; refused or cancelled drafts remain live. }

      if FPort.Active and (FPort.Serial = AWork.Serial) then
      begin
        RetireForm;
      end;
    end;

    if LNotification <> '' then
    begin
      raise ENyxPublicationNotification.CreateReceiverFailure(LNotification);
    end;
  finally

    if not LNegotiated then
    begin
      CancelOutstanding;
    end;

    if LStage <> nil then
    begin
      LStage.Retire;
    end;
    LFence.Retire;
  end;
end;

procedure TWorkbench.SetEnabled(AEnabled: Boolean);
var
  LLease: INyxCollectionQueryWorkbench;
  LScheduler: INyxScheduler;
begin
  LLease := Self;

  if not GetConnected then
  begin
    Exit;
  end;
  LScheduler := FScheduler;
  LScheduler.RequireUI;

  if not GetConnected then
  begin
    Exit;
  end;
  FPort.Enabled := AEnabled;

  if not AEnabled then
  begin

    if FPort.Serial < High(Integer) then
    begin
      Inc(FPort.Serial);
    end;
    RetireForm;
  end;
end;

procedure TWorkbench.Revoke;
begin

  if FPort <> nil then
  begin
    FPort.Active := False;
    FPort.Enabled := False;
    FPort.Owner := nil;
  end;

  if FForm <> nil then
  begin
    FForm.Active := False;
  end;
end;

procedure TWorkbench.Disconnect;
var
  LActor: INyxCollectionQueryOwner;
  LPresenters: INyxQueryFormPresenterFactory;
  LToken: INyxCollectionViewSubscription;
  LExecutions: array of INyxExecution;
  LIndex: Integer;
  LError: TNyxText;
begin

  if FDisconnected then
  begin
    Exit;
  end;
  FDisconnected := True;
  Revoke;
  LActor := FActor;
  LPresenters := FPresenters;
  LToken := FViewToken;
  LExecutions := FExecutions;
  FExecutions := nil;
  FActor := nil;
  FPresenters := nil;
  FViewToken := nil;
  LError := '';
  try

    if LActor <> nil then
    begin
      LActor.Revoke;
    end;
  except
    on E: Exception do LError := TNyxText(E.Message);
  end;
  try

    if LPresenters <> nil then
    begin
      LPresenters.Revoke;
    end;
  except
    on E: Exception do
    begin

      if LError = '' then LError := TNyxText(E.Message);
    end;
  end;
  try
    RetireForm;
  except
    on E: Exception do
    begin

      if LError = '' then LError := TNyxText(E.Message);
    end;
  end;
  try

    if LToken <> nil then
    begin
      LToken.Disconnect;
    end;
  except
    on E: Exception do
    begin

      if LError = '' then LError := TNyxText(E.Message);
    end;
  end;
  for LIndex := 0 to High(LExecutions) do
  begin
    try
      LExecutions[LIndex].Cancel;
    except
      on E: Exception do
      begin

        if LError = '' then LError := TNyxText(E.Message);
      end;
    end;
  end;
  FSession := nil;
  FView := nil;
  FEvents := nil;
  FScheduler := nil;

  if LError <> '' then
  begin
    raise ENyxCollection.Create(LError);
  end;
end;

function NewNyxCollectionQueryWorkbench(const AView: INyxCollectionView;
  const AEvents: INyxEvents; const ATarget: TNyxEventTarget;
  const APresenters: INyxQueryFormPresenterFactory;
  const ADesignID: TNyxText): INyxCollectionQueryWorkbench;
var
  LOwner: TWorkbench;
begin
  LOwner := TWorkbench.Create;
  Result := LOwner;
  LOwner.Initialize(AView, AEvents, ATarget, APresenters, ADesignID, wpTableControls);
end;

function NewNyxCollectionQueryDetails(const AView: INyxCollectionView;
  const AEvents: INyxEvents; const ATarget: TNyxEventTarget;
  const APresenters: INyxQueryFormPresenterFactory;
  const ADesignID: TNyxText): INyxCollectionQueryWorkbench;
var
  LOwner: TWorkbench;
begin
  LOwner := TWorkbench.Create;
  Result := LOwner;
  LOwner.Initialize(AView, AEvents, ATarget, APresenters, ADesignID, wpQueryDetails);
end;

end.
