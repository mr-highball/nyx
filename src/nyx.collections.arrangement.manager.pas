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

{ Shared ordinary column-manager controller. The view owns preferences; target
  presenters own physical forms. Queued work carries detached values and weak,
  revocable controller/form ports rather than keeping a mount or form alive. }
unit nyx.collections.arrangement.manager;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.events, nyx.collections.view, nyx.collections.arrangement.presenter;

{ Main events must already carry the exact target's input lifetime. No input
  generation is captured during construction of a hidden renderer candidate.
  The target revokes this manager before freeing the borrowed fit receiver. }
function NewNyxColumnArrangementManager(const AView: INyxCollectionView;
  const AEvents: INyxEvents;
  const APresenters: INyxColumnArrangementPresenterFactory;
  APrepareFit: TNyxColumnArrangementFitPrepare): INyxColumnArrangementManager;

implementation

uses
  SysUtils, nyx.text, nyx.types, nyx.state, nyx.model, nyx.controls,
  nyx.root.types, nyx.behavior, nyx.scheduler, nyx.publication,
  nyx.popover.types, nyx.collections, nyx.collections.arrangement,
  nyx.collections.arrangement.controls, nyx.collections.view.types;

const
  CPage = 'nyx-column-arrangement-page';
  CStatus = 'nyx-column-arrangement-status-label';

type
  { Forward declaration joins private weak ports without exposing an owner in a
    public contract or introducing a mount/controller reference cycle. }
  TArrangementManager = class;

  { UI-owned intent lifetime. Retirement is flags-only and precedes all outward
    cleanup. Serial never wraps; equal-value/no-op intentions supersede older
    work just as actual preference changes do. Retained work owns this port. }
  TManagerPort = class(TInterfacedObject)
  public
    Owner: TArrangementManager;
    Active: Boolean;
    Enabled: Boolean;
    Serial: Integer;
  end;

  { Exact form incarnation. Old forms and their captured observers retain only
    this inactive port after replacement, never the controller or its successor. }
  TManagerForm = class(TInterfacedObject)
  public
    Parent: TManagerPort;
    ParentLease: IInterface;
    Active: Boolean;
  end;

  { Presentation-only commands share the existing original input/form proof.
    Closing preserves physical form drafts and never prepares a model change. }
  TManagerCommand = (nmcColumns, nmcReload, nmcClose);

  { Immutable command payload after capture. Execution is always queued to the
    existing UI scheduler, after the originating dispatch has submitted ordinary
    callbacks. No node, physical presenter or mutable model is carried here. }
  TManagerWork = class(TInterfacedObject, INyxWork)
  public
    Port: TManagerPort;
    PortLease: IInterface;
    Form: TManagerForm;
    FormLease: IInterface;
    Serial: Integer;
    Scheduler: INyxScheduler;
    Generation: INyxCancellationScope;
    GenerationReady: INyxCancellationReadiness;
    FormGeneration: INyxCancellationScope;
    FormReady: INyxCancellationReadiness;
    Capture: INyxCollectionColumnsCapture;
    Change: TNyxColumnArrangementChange;
    Draft: TNyxColumnArrangementDraft;
    Command: TManagerCommand;
    procedure Execute(const AExecution: INyxExecution);
  end;

  { Original command admission participates in the SAME publication as the
    existing view preparation. Validate samples arbitrary cancellation getters
    before a nested Fit metric validator; final readiness calls only certified
    cancellation facts plus these local ports after all model validators. }
  TManagerFence = class(TInterfacedObject, INyxPreparedPublication,
    INyxPublicationReadiness, INyxCancellationReadiness)
  private
    FPort: TManagerPort;
    FPortLease: IInterface;
    FForm: TManagerForm;
    FFormLease: IInterface;
    FSerial: Integer;
    FGeneration: INyxCancellationScope;
    FGenerationReady: INyxCancellationReadiness;
    FFormGeneration: INyxCancellationScope;
    FFormReady: INyxCancellationReadiness;
    FValidated: Boolean;
    FInstalled: Boolean;
    FRetired: Boolean;
    procedure Flags;
  public
    constructor Create(AWork: TManagerWork);
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
    property Installed: Boolean read FInstalled;
  end;

  { Existing event observation captures the real synchronized mounted tree.
    It never replaces user subscriptions or strongly retains its controller. }
  TManagerObserver = class(TNyxEventObserver)
  private
    FForm: TManagerForm;
    FLease: IInterface;
  public
    constructor Create(AForm: TManagerForm; const ALease: IInterface);
    procedure Observe(const AEvent: TNyxEventInfo; ACallbackCount: Integer); override;
  end;

  { Per-table manager joins the view's optional columns facet. Factory and fit
    ports are exact-target dependencies; logical retirement severs the weak fit
    callback before physical cleanup. Captures/drafts are independent values. }
  TArrangementManager = class(TInterfacedObject, INyxColumnArrangementManager)
  private
    FView: INyxCollectionView;
    FColumns: INyxCollectionColumns;
    FEvents: INyxEvents;
    FScheduler: INyxScheduler;
    FFactory: INyxColumnArrangementPresenterFactory;
    FPrepareFit: TNyxColumnArrangementFitPrepare;
    FPort: TManagerPort;
    FPortLease: IInterface;
    FPresenter: INyxColumnArrangementPresenter;
    FForm: TManagerForm;
    FFormLease: IInterface;
    FSubscriptions: array of INyxEventSubscription;
    FExecutions: array of INyxExecution;
    FCapture: INyxCollectionColumnsCapture;
    FObservation: TNyxColumnLayoutObservation;
    FDisconnected: Boolean;
    procedure Initialize(const AView: INyxCollectionView; const AEvents: INyxEvents;
      const APresenters: INyxColumnArrangementPresenterFactory;
      APrepareFit: TNyxColumnArrangementFitPrepare);
    procedure RequireActive;
    function Advance: Integer;
    procedure RequireCurrent(ASerial: Integer);
    procedure Status(const AText: TNyxText);
    procedure RetireForm;
    procedure BuildForm(const ACapture: INyxCollectionColumnsCapture;
      const AObservation: TNyxColumnLayoutObservation;
      const ADraft: TNyxColumnArrangementDraft; ASerial: Integer;
      const AProof: INyxCancellationReadiness);
    procedure CaptureAction(AForm: TManagerForm; const AEvent: TNyxEventInfo);
    procedure ExecuteWork(AWork: TManagerWork);
    procedure RequireWork(AWork: TManagerWork);
    procedure Submit(AWork: TManagerWork);
    procedure CaptureMain(out AGeneration: INyxCancellationScope;
      out AReady: INyxCancellationReadiness; ASerial: Integer);
  public
    destructor Destroy; override;
    procedure Open;
    procedure ReloadDraft;
    procedure SetEnabled(AEnabled: Boolean);
    function GetConnected: Boolean;
    function GetObservation: TNyxColumnLayoutObservation;
    function GetPresenter: INyxColumnArrangementPresenter;
    procedure Revoke;
    procedure Disconnect;
  end;

constructor TManagerFence.Create(AWork: TManagerWork);
begin
  inherited Create;
  FPort := AWork.Port;
  FPortLease := AWork.PortLease;
  FForm := AWork.Form;
  FFormLease := AWork.FormLease;
  FSerial := AWork.Serial;
  FGeneration := AWork.Generation;
  FGenerationReady := AWork.GenerationReady;
  FFormGeneration := AWork.FormGeneration;
  FFormReady := AWork.FormReady;
end;

procedure TManagerFence.Flags;
begin

  if FRetired or (FPort = nil) or not FPort.Active or not FPort.Enabled or
    (FPort.Owner = nil) or (FPort.Serial <> FSerial) or (FForm = nil) or
    not FForm.Active or (FForm.Parent <> FPort) then
  begin
    raise ENyxCollection.Create('Column command was superseded or its form retired');
  end;
end;

procedure TManagerFence.Validate;
var
  LCancelled: Boolean;
begin
  Flags;

  if FValidated or (FGeneration = nil) or (FFormGeneration = nil) or
    (FGenerationReady = nil) or (FFormReady = nil) then
  begin
    raise ENyxCollection.Create('Column command requires original certified generations');
  end;
  LCancelled := FGeneration.Cancelled;
  LCancelled := FFormGeneration.Cancelled or LCancelled;
  Flags;

  if LCancelled then
  begin
    raise ENyxCollection.Create('Column command generation was cancelled');
  end;
  FValidated := True;
  CheckReady;
end;

procedure TManagerFence.CheckReady;
begin
  Flags;

  if not FValidated or FInstalled then
  begin
    raise ENyxCollection.Create('Column command is not ready for publication');
  end;
  FGenerationReady.CheckReady;
  FFormReady.CheckReady;
  Flags;
end;

procedure TManagerFence.Install;
begin
  FInstalled := True;
end;

procedure TManagerFence.Notify;
begin
end;

procedure TManagerFence.Retire;
begin
  FRetired := True;
end;

procedure TManagerWork.Execute(const AExecution: INyxExecution);
var
  LOwner: TArrangementManager;
  LOwnerLease: INyxColumnArrangementManager;
  LCancelled: Boolean;
begin
  Scheduler.RequireUI;

  if (Port = nil) or not Port.Active or not Port.Enabled or
    (Port.Serial <> Serial) or (Port.Owner = nil) or
    (Form = nil) or not Form.Active then
  begin
    Exit;
  end;
  LCancelled := AExecution.Cancelled or Generation.Cancelled;
  LCancelled := FormGeneration.Cancelled or LCancelled;

  if LCancelled or not Port.Active or not Port.Enabled or
    (Port.Serial <> Serial) or (Port.Owner = nil) or not Form.Active then
  begin
    Exit;
  end;
  LOwner := Port.Owner;
  LOwnerLease := LOwner;
  try
    LOwner.RequireWork(Self);
    LOwner.ExecuteWork(Self);
  except
    on E: Exception do
    begin

      if Port.Active and (Port.Owner = LOwner) and (Port.Serial = Serial) and
        Form.Active then
      begin
        LOwner.Status(TNyxText(E.Message));
      end;
      raise;
    end;
  end;
end;

constructor TManagerObserver.Create(AForm: TManagerForm; const ALease: IInterface);
begin
  inherited Create;
  FForm := AForm;
  FLease := ALease;
end;

procedure TManagerObserver.Observe(const AEvent: TNyxEventInfo;
  ACallbackCount: Integer);
var
  LOwner: TArrangementManager;
  LLease: INyxColumnArrangementManager;
begin

  if not FForm.Active or not FForm.Parent.Active or
    not FForm.Parent.Enabled or (FForm.Parent.Owner = nil) then
  begin
    Exit;
  end;
  LOwner := FForm.Parent.Owner;
  LLease := LOwner;
  LOwner.CaptureAction(FForm, AEvent);
end;

procedure TArrangementManager.Initialize(const AView: INyxCollectionView;
  const AEvents: INyxEvents;
  const APresenters: INyxColumnArrangementPresenterFactory;
  APrepareFit: TNyxColumnArrangementFitPrepare);
var
  LSpec: TNyxCollectionViewSpec;
begin
  FView := AView;
  FEvents := AEvents;
  FFactory := APresenters;
  FPrepareFit := APrepareFit;
  FPort := TManagerPort.Create;
  FPortLease := FPort;
  FPort.Owner := Self;
  FPort.Active := True;
  FPort.Enabled := True;

  if (FView = nil) or (FEvents = nil) or (FFactory = nil) then
  begin
    raise ENyxCollection.Create('Column manager requires its exact table, router and presenter factory');
  end;
  FScheduler := FEvents.Scheduler;
  FScheduler.RequireUI;
  LSpec := FView.Spec;

  if (FView.Projection <> cpTable) or not LSpec.HasLayoutControls then
  begin
    raise ENyxCollection.Create('Column manager requires authored table arrangement controls');
  end;
  FColumns := NyxCollectionColumns(FView);
end;

destructor TArrangementManager.Destroy;
begin
  try
    Disconnect;
  except
    { Explicit Disconnect reports cleanup failures. Destruction must not revive
      observers or abandon the remaining independent target cleanup. }
  end;
  inherited Destroy;
end;

function TArrangementManager.GetConnected: Boolean;
begin
  Result := (FPort <> nil) and FPort.Active and (FPort.Owner = Self) and
    not FDisconnected and (FView <> nil) and (FColumns <> nil);
end;

procedure TArrangementManager.RequireActive;
var
  LScheduler: INyxScheduler;
begin
  LScheduler := FScheduler;

  if not GetConnected or not FPort.Enabled or (LScheduler = nil) then
  begin
    raise ENyxCollection.Create('Column manager is disconnected or disabled');
  end;
  LScheduler.RequireUI;

  if not GetConnected or not FPort.Enabled then
  begin
    raise ENyxCollection.Create('Column manager retired during UI admission');
  end;
end;

procedure TArrangementManager.RequireCurrent(ASerial: Integer);
begin

  if not GetConnected or not FPort.Enabled or (FPort.Serial <> ASerial) then
  begin
    raise ENyxCollection.Create('Column-manager intention was superseded');
  end;
end;

function TArrangementManager.Advance: Integer;
begin
  RequireActive;

  if FPort.Serial = High(Integer) then
  begin
    raise ENyxCollection.Create('Column-manager intention serial is exhausted');
  end;
  Inc(FPort.Serial);
  Result := FPort.Serial;
end;

procedure TArrangementManager.CaptureMain(out AGeneration: INyxCancellationScope;
  out AReady: INyxCancellationReadiness; ASerial: Integer);
var
  LEvents: INyxEvents;
begin
  LEvents := FEvents;
  AGeneration := NyxEventGeneration(LEvents);
  RequireCurrent(ASerial);

  if (AGeneration = nil) or
    not Supports(AGeneration, INyxCancellationReadiness, AReady) then
  begin
    raise ENyxCollection.Create('Column manager requires certified target input readiness');
  end;
  RequireCurrent(ASerial);

  if AGeneration.Cancelled then
  begin
    raise ENyxCollection.Create('Column-manager target generation was cancelled');
  end;
  RequireCurrent(ASerial);
  AReady.CheckReady;
  RequireCurrent(ASerial);
end;

function TArrangementManager.GetObservation: TNyxColumnLayoutObservation;
var
  LLease: INyxColumnArrangementManager;
  LColumns: INyxCollectionColumns;
  LCapture: INyxCollectionColumnsCapture;
begin
  LLease := Self;
  RequireActive;
  LColumns := FColumns;
  LCapture := LColumns.CaptureColumns;
  RequireActive;
  Result := TNyxColumnLayoutObservation.Read(LCapture);
  RequireActive;
end;

function TArrangementManager.GetPresenter: INyxColumnArrangementPresenter;
begin
  Result := FPresenter;
end;

procedure TArrangementManager.Status(const AText: TNyxText);
var
  LPresenter: INyxColumnArrangementPresenter;
begin
  LPresenter := FPresenter;

  if GetConnected and (FForm <> nil) and FForm.Active and
    (LPresenter <> nil) then
  begin
    { Last outward operation; status synchronization can retire the mount. }
    LPresenter.SetStatus(AText);
  end;
end;

procedure TArrangementManager.RetireForm;
var
  LPresenter: INyxColumnArrangementPresenter;
  LTokens: array of INyxEventSubscription;
  LIndex: Integer;
  LError: TNyxText;
begin

  if FForm <> nil then
  begin
    FForm.Active := False;
  end;
  LPresenter := FPresenter;
  LTokens := FSubscriptions;
  FPresenter := nil;
  FSubscriptions := nil;
  FForm := nil;
  FFormLease := nil;
  FCapture := nil;
  FObservation := Default(TNyxColumnLayoutObservation);
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

procedure TArrangementManager.BuildForm(const ACapture: INyxCollectionColumnsCapture;
  const AObservation: TNyxColumnLayoutObservation;
  const ADraft: TNyxColumnArrangementDraft; ASerial: Integer;
  const AProof: INyxCancellationReadiness);
var
  LLease: INyxColumnArrangementManager;
  LCapture: INyxCollectionColumnsCapture;
  LObservation: TNyxColumnLayoutObservation;
  LDraft: TNyxColumnArrangementDraft;
  LFactory: INyxColumnArrangementPresenterFactory;
  LPresenter: INyxColumnArrangementPresenter;
  LForm: TManagerForm;
  LFormLease: IInterface;
  LTokens: array of INyxEventSubscription;
  LObserver: INyxEventObserver;
  LEvents: INyxEvents;
  LObservations: INyxEventObservation;
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LEditor: INyxCard;
  LLabel: INyxLabel;
  LIndex: Integer;
  LAdopted: Boolean;

  procedure Current;
  begin
    RequireCurrent(ASerial);

    if FFactory <> LFactory then
    begin
      raise ENyxCollection.Create('Column presenter factory retired during construction');
    end;
    AProof.CheckReady;
    RequireCurrent(ASerial);
  end;

  procedure ObserveNodes(ANode: TNyxNode);
  var
    LChild: Integer;
    LCount: Integer;
    LToken: INyxEventSubscription;
    LTrigger: TNyxTrigger;
    LObserve: Boolean;
  begin
    LObserve := True;

    if ANode.ProjectionKind = NyxKindName(nkButton) then
    begin
      LTrigger := ntClick;
    end
    else if (ANode.ProjectionKind = NyxKindName(nkInput)) or
      (ANode.ProjectionKind = NyxKindName(nkSelect)) then
    begin
      LTrigger := ntChange;
    end
    else
    begin
      LTrigger := ntClick;
      LObserve := False;
    end;

    if LObserve then
    begin
      LToken := LObservations.Observe(NyxControlEvents(ANode.ID, niRuntime),
        LTrigger, LObserver, nopBeforeCallbacks, NyxEvent(''));
      LCount := Length(LTokens);
      SetLength(LTokens, LCount + 1);
      LTokens[LCount] := LToken;
      Current;
    end;
    for LChild := 0 to ANode.Count - 1 do
    begin
      ObserveNodes(ANode.Children[LChild]);
    end;
  end;

begin
  LLease := Self;
  LCapture := ACapture;
  LObservation := AObservation.Copy;
  LDraft := ADraft.Copy;
  LFactory := FFactory;
  LForm := nil;
  LAdopted := False;
  Current;
  LDocument := TNyxDocument.Create;
  try
    LDocument.State.SetValue(NyxTextState(NyxColumnArrangementStatusState), '');
    LPage := NewNyxPage(CPage);
    LPage.Configure.Padding(6).Gap(8).Done;
    LEditor := NewNyxColumnArrangementEditor(NyxColumnArrangementEditorID,
      LObservation, LDraft);
    LEditor.Configure.PartName(NyxPart('editor')).Done;
    { Focus an explicit non-mutating dismissal ahead of the field controls.
      Queued observation below preserves ordinary callbacks and popup admission. }
    LEditor.Insert(2, NewNyxButton(NyxColumnArrangementCloseID).Configure
      .Text('Close').PartName(NyxPart('close')).Done);
    LEditor.Add(NewNyxButton(NyxColumnArrangementReloadID).Configure
      .Text('Discard draft and reload current columns').Done);
    LLabel := NewNyxLabel(CStatus);
    LLabel.Binds.Text(NyxTextState(NyxColumnArrangementStatusState)).Done;
    LEditor.Add(LLabel);
    LPage.Add(LEditor);
    LDocument.AddPage(LPage);
    LPresenter := LFactory.CreatePresenter(LDocument, NyxPageRoot(CPage));
    Current;

    if LPresenter = nil then
    begin
      raise ENyxCollection.Create('Column presenter factory returned no form');
    end;
    LForm := TManagerForm.Create;
    LFormLease := LForm;
    LForm.Parent := FPort;
    LForm.ParentLease := FPortLease;
    LObserver := TManagerObserver.Create(LForm, LFormLease);
    LEvents := LPresenter.Events;
    Current;

    if not Supports(LEvents, INyxEventObservation, LObservations) then
    begin
      raise ENyxCollection.Create('Column form requires ordinary event observation');
    end;
    Current;
    ObserveNodes(LPage.Node);
    LPresenter.Show(NyxPopover('Arrange columns').Size(520, 680)
      .Focus(NyxPart('editor/close')));
    Current;
    RetireForm;
    Current;
    FPresenter := LPresenter;
    FForm := LForm;
    FFormLease := LFormLease;
    FSubscriptions := LTokens;
    FCapture := LCapture;
    FObservation := LObservation.Copy;
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
          { Preserve construction/refusal, but still detach every receiver. }
        end;
      end;

      if LPresenter <> nil then
      begin
        try
          LPresenter.Disconnect;
        except
          { Rejected physical candidates never replace the primary error. }
        end;
      end;
    end;
  end;

  if GetConnected and FPort.Enabled and (FPort.Serial = ASerial) and
    (FPresenter = LPresenter) and (FForm = LForm) and LForm.Active then
  begin
    { Former-host cleanup may restore focus. Repeat only for the exact accepted
      presenter; target Show handles an already-open host without new authority. }
    LPresenter.Show(NyxPopover('Arrange columns').Size(520, 680)
      .Focus(NyxPart('editor/close')));
  end;
end;

procedure TArrangementManager.Open;
var
  LLease: INyxColumnArrangementManager;
  LPresenter: INyxColumnArrangementPresenter;
  LColumns: INyxCollectionColumns;
  LCapture: INyxCollectionColumnsCapture;
  LObservation: TNyxColumnLayoutObservation;
  LGeneration: INyxCancellationScope;
  LReady: INyxCancellationReadiness;
  LForm: TManagerForm;
  LFormLease: IInterface;
  LStale: Boolean;
  LSerial: Integer;
begin
  LLease := Self;
  LSerial := Advance;
  CaptureMain(LGeneration, LReady, LSerial);
  LPresenter := FPresenter;

  if (LPresenter <> nil) and (FForm <> nil) and FForm.Active then
  begin
    LForm := FForm;
    LFormLease := FFormLease;
    { A closed form deliberately retains its raw draft and original baseline.
      External resize/configuration must be visible on reopen without silently
      replacing unfinished input or borrowing fresh authority for its actions.
      Show may dispatch focus callbacks, so observe AFTER it under the original
      form/main proof. Include the serial so an equal-value round trip warns. }
    LPresenter.Show(NyxPopover('Arrange columns').Size(520, 680)
      .Focus(NyxPart('editor/close')));
    RequireCurrent(LSerial);
    LReady.CheckReady;
    RequireCurrent(LSerial);

    if (FPresenter <> LPresenter) or (FForm <> LForm) or not LForm.Active then
    begin
      raise ENyxCollection.Create('Column-manager form changed during presentation');
    end;
    LColumns := FColumns;
    LCapture := LColumns.CaptureColumns;
    RequireCurrent(LSerial);
    LObservation := TNyxColumnLayoutObservation.Read(LCapture);
    RequireCurrent(LSerial);
    LReady.CheckReady;
    RequireCurrent(LSerial);

    if (FPresenter <> LPresenter) or (FForm <> LForm) or not LForm.Active then
    begin
      raise ENyxCollection.Create('Column-manager form changed during reopen');
    end;
    LStale := NyxColumnArrangementBaseline(LObservation) <>
      NyxColumnArrangementBaseline(FObservation);

    if LStale then
    begin
      { Last outward operation. Status belongs only to this retained presenter;
        synchronization may retire it, and must never target a successor. }
      LPresenter.SetStatus('Columns changed while this panel was closed. ' +
        'Your draft is retained; reload current columns before applying it.');
    end;
    Exit;
  end;
  LColumns := FColumns;
  LCapture := LColumns.CaptureColumns;
  RequireCurrent(LSerial);
  LObservation := TNyxColumnLayoutObservation.Read(LCapture);
  RequireCurrent(LSerial);
  LReady.CheckReady;
  BuildForm(LCapture, LObservation, Default(TNyxColumnArrangementDraft),
    LSerial, LReady);
end;

procedure TArrangementManager.ReloadDraft;
var
  LLease: INyxColumnArrangementManager;
  LColumns: INyxCollectionColumns;
  LCapture: INyxCollectionColumnsCapture;
  LObservation: TNyxColumnLayoutObservation;
  LGeneration: INyxCancellationScope;
  LReady: INyxCancellationReadiness;
  LSerial: Integer;
begin
  LLease := Self;
  LSerial := Advance;
  CaptureMain(LGeneration, LReady, LSerial);
  LColumns := FColumns;
  LCapture := LColumns.CaptureColumns;
  RequireCurrent(LSerial);
  LObservation := TNyxColumnLayoutObservation.Read(LCapture);
  RequireCurrent(LSerial);
  LReady.CheckReady;
  BuildForm(LCapture, LObservation, Default(TNyxColumnArrangementDraft),
    LSerial, LReady);
end;

procedure TArrangementManager.CaptureAction(AForm: TManagerForm;
  const AEvent: TNyxEventInfo);
var
  LLease: INyxColumnArrangementManager;
  LPresenter: INyxColumnArrangementPresenter;
  LWork: TManagerWork;
  LWorkLease: INyxWork;
  LFormEvents: INyxEvents;
  LRoot: TNyxNode;
  LEditor: TNyxNode;
  LButton: TNyxNode;
  LObservation: TNyxColumnLayoutObservation;
  LAction: TNyxText;
begin
  LLease := Self;

  if not GetConnected or (FForm <> AForm) or not AForm.Active then
  begin
    Exit;
  end;
  LPresenter := FPresenter;
  try
    LWork := TManagerWork.Create;
    LWorkLease := LWork;
    LWork.Serial := Advance;

    if AEvent.Trigger = ntChange then
    begin
      Exit;
    end;
    LWork.Port := FPort;
    LWork.PortLease := FPortLease;
    LWork.Form := AForm;
    LWork.FormLease := FFormLease;
    LWork.Scheduler := FScheduler;
    LWork.Capture := FCapture;
    LObservation := FObservation.Copy;
    CaptureMain(LWork.Generation, LWork.GenerationReady, LWork.Serial);
    LFormEvents := LPresenter.Events;
    RequireCurrent(LWork.Serial);
    LWork.FormGeneration := NyxEventGeneration(LFormEvents);
    RequireCurrent(LWork.Serial);

    if (LWork.FormGeneration = nil) or not Supports(LWork.FormGeneration,
      INyxCancellationReadiness, LWork.FormReady) then
    begin
      raise ENyxCollection.Create('Column form requires its actual open/readiness generation');
    end;
    RequireWork(LWork);
    LAction := AEvent.OriginID;
    LRoot := LPresenter.CaptureRoot;
    RequireWork(LWork);

    if LRoot = nil then
    begin
      raise ENyxCollection.Create('Column form has no current mounted root');
    end;
    { Tree capture is synchronous and callback-free. Do not borrow this root
      across any presenter, router, scheduler or model-interface getter. }
    LEditor := LRoot.Find(NyxColumnArrangementEditorID);
    LButton := LRoot.Find(LAction);

    if (LEditor = nil) or (LButton = nil) then
    begin
      raise ENyxCollection.Create('Column form action is no longer mounted');
    end;
    LWork.Draft := CaptureNyxColumnArrangementDraft(LEditor, LRoot, LObservation);
    LWork.Command := nmcColumns;

    if LButton.SourceID = NyxColumnArrangementReloadID then
    begin
      LWork.Command := nmcReload;
    end
    else if LButton.SourceID = NyxColumnArrangementCloseID then
    begin
      LWork.Command := nmcClose;
    end;

    if (LWork.Command = nmcColumns) and not CaptureNyxColumnArrangementEditor(LButton,
      LRoot, LObservation, LWork.Change) then
    begin
      Exit;
    end;
    RequireWork(LWork);
    Submit(LWork);
  except
    on E: Exception do
    begin

      if GetConnected and (FForm = AForm) and AForm.Active and
        (FPresenter = LPresenter) then
      begin
        Status(TNyxText(E.Message));
      end;
    end;
  end;
end;

procedure TArrangementManager.RequireWork(AWork: TManagerWork);
begin
  RequireCurrent(AWork.Serial);

  if (FForm <> AWork.Form) or not AWork.Form.Active or
    (FPort <> AWork.Port) or (AWork.GenerationReady = nil) or
    (AWork.FormReady = nil) then
  begin
    raise ENyxCollection.Create('Column-manager form command is no longer current');
  end;
  AWork.GenerationReady.CheckReady;
  AWork.FormReady.CheckReady;
  RequireCurrent(AWork.Serial);

  if (FForm <> AWork.Form) or not AWork.Form.Active then
  begin
    raise ENyxCollection.Create('Column-manager form retired during admission');
  end;
end;

procedure TArrangementManager.Submit(AWork: TManagerWork);
var
  LWork: INyxWork;
  LExecution: INyxExecution;
  LScheduler: INyxScheduler;
  LPort: TManagerPort;
  LPortLease: IInterface;
  LPrevious: array of INyxExecution;
  LIndex: Integer;
  LCount: Integer;
begin
  LWork := AWork;
  LScheduler := FScheduler;
  LPort := FPort;
  LPortLease := FPortLease;
  LPrevious := FExecutions;
  LExecution := LScheduler.PostUI(LWork);

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

procedure TArrangementManager.ExecuteWork(AWork: TManagerWork);
var
  LLease: INyxColumnArrangementManager;
  LColumns: INyxCollectionColumns;
  LPresenter: INyxColumnArrangementPresenter;
  LCapture: INyxCollectionColumnsCapture;
  LObservation: TNyxColumnLayoutObservation;
  LDraft: TNyxColumnArrangementDraft;
  LPrepareFit: TNyxColumnArrangementFitPrepare;
  LMeasurement: INyxColumnFitMeasurement;
  LStage: INyxPreparedPublication;
  LFence: INyxPreparedPublication;
  LFenceObject: TManagerFence;
  LProof: INyxCancellationReadiness;
  LNotification: TNyxText;
  LInstalledWidth: Integer;
begin
  LLease := Self;
  RequireWork(AWork);
  LColumns := FColumns;
  LPrepareFit := FPrepareFit;
  LFenceObject := TManagerFence.Create(AWork);
  LFence := LFenceObject;
  try

    if AWork.Command = nmcClose then
    begin
      { Validate the original queued intention after ordinary event callbacks.
        Retiring/reopening that actual popup before this point refuses the old
        close; a delayed operation can never dismiss a successor presentation. }
      LFenceObject.Validate;
      RequireWork(AWork);
      LPresenter := FPresenter;
      LFenceObject.CheckReady;
      LPresenter.Close;
      Exit;
    end;

    if AWork.Command = nmcReload then
    begin
      LFenceObject.Validate;
      { The OLD form proves the reload before capture. Showing its explicit
        replacement may intentionally dismiss that form; subsequent candidate
        admission keeps the ORIGINAL main input proof, not a newly captured one. }
      LProof := AWork.GenerationReady;
      LCapture := LColumns.CaptureColumns;
      RequireWork(AWork);
      LObservation := TNyxColumnLayoutObservation.Read(LCapture);
      RequireWork(AWork);
      BuildForm(LCapture, LObservation, Default(TNyxColumnArrangementDraft),
        AWork.Serial, LProof);
      Exit;
    end;

    if AWork.Change.RequiresMeasurement then
    begin

      if not Assigned(LPrepareFit) then
      begin
        raise ENyxCollection.Create('This table target does not expose measured column fit');
      end;
      { The borrowed receiver is invoked only after the last pure original
        command check; retirement clears its port before freeing the mount. }
      RequireWork(AWork);
      LStage := LPrepareFit(AWork.Capture, AWork.Change.Field, LMeasurement);
      RequireWork(AWork);

      if (LStage = nil) or (LMeasurement = nil) then
      begin
        raise ENyxCollection.Create('Column fit returned no admitted measurement/preparation');
      end;
      LInstalledWidth := LMeasurement.Width;
      RequireWork(AWork);
    end
    else
    begin
      LStage := LColumns.PrepareColumns(AWork.Capture, AWork.Change.Preferences);
      RequireWork(AWork);
    end;
    LNotification := '';
    try
      { Arbitrary generation getters validate FIRST. The nested model/Fit stage
        then validates its source/metric and every captured pure fence runs again
        after all validators, before either participant installs. }
      PublishNyxGroup([LFence, LStage]);
    except
      on E: ENyxPublicationNotification do
      begin
        LNotification := TNyxText(E.Message);
      end;
    end;

    if LFenceObject.Installed and GetConnected and FPort.Enabled and
      (FPort.Serial = AWork.Serial) and (FForm = AWork.Form) and AWork.Form.Active then
    begin
      try
        LCapture := LColumns.CaptureColumns;
        RequireWork(AWork);
        LObservation := TNyxColumnLayoutObservation.Read(LCapture);
        RequireWork(AWork);

        if AWork.Change.RequiresMeasurement then
        begin
          LDraft := AWork.Draft.AfterMeasuredFit(AWork.Change, LInstalledWidth,
            LObservation);
        end
        else
        begin
          LDraft := AWork.Draft.AfterAccepted(AWork.Change, LObservation);
        end;
        { This is the exact accepted continuation, not a new input intention.
          Rebase's full expected preferences/spec/+0-or-1 serial rejects later
          reentrant layout changes even when they returned to equal bytes. }
        BuildForm(LCapture, LObservation, LDraft, AWork.Serial,
          AWork.GenerationReady);
      except
        on E: Exception do
        begin
          { Model publication already committed. A failed UI continuation must
            retain that classification, and an earlier notification failure
            remains primary instead of being replaced by cleanup/admission. }

          if LNotification = '' then
          begin
            LNotification := TNyxText(E.Message);
          end;
        end;
      end;
    end;

    if LNotification <> '' then
    begin
      raise ENyxPublicationNotification.CreateReceiverFailure(LNotification);
    end;
  finally

    if LStage <> nil then
    begin
      LStage.Retire;
    end;
    LFence.Retire;
  end;
end;

procedure TArrangementManager.SetEnabled(AEnabled: Boolean);
var
  LLease: INyxColumnArrangementManager;
  LPresenter: INyxColumnArrangementPresenter;
  LScheduler: INyxScheduler;
begin
  LLease := Self;
  LScheduler := FScheduler;

  if not GetConnected or (FPort.Enabled = AEnabled) then
  begin
    Exit;
  end;
  LScheduler.RequireUI;

  if not GetConnected then
  begin
    Exit;
  end;
  FPort.Enabled := AEnabled;

  if FPort.Serial < High(Integer) then
  begin
    Inc(FPort.Serial);
  end;
  LPresenter := FPresenter;

  if not AEnabled and (LPresenter <> nil) then
  begin
    LPresenter.Close;
  end;
end;

procedure TArrangementManager.Revoke;
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
  FPrepareFit := nil;
end;

procedure TArrangementManager.Disconnect;
var
  LFactory: INyxColumnArrangementPresenterFactory;
  LPresenter: INyxColumnArrangementPresenter;
  LTokens: array of INyxEventSubscription;
  LExecutions: array of INyxExecution;
  LView: INyxCollectionView;
  LColumns: INyxCollectionColumns;
  LEvents: INyxEvents;
  LScheduler: INyxScheduler;
  LCapture: INyxCollectionColumnsCapture;
  LIndex: Integer;
  LError: TNyxText;
begin

  if FDisconnected then
  begin
    Exit;
  end;
  FDisconnected := True;
  Revoke;
  { Detach the complete original cohort before the first cancellation/factory/
    presenter callout. Entered commands retain this controller, and no tail may
    look up a newer form through fields changed by cleanup callbacks. }
  LFactory := FFactory;
  LPresenter := FPresenter;
  LTokens := FSubscriptions;
  LExecutions := FExecutions;
  LView := FView;
  LColumns := FColumns;
  LEvents := FEvents;
  LScheduler := FScheduler;
  LCapture := FCapture;
  FFactory := nil;
  FPresenter := nil;
  FSubscriptions := nil;
  FExecutions := nil;
  FForm := nil;
  FFormLease := nil;
  FView := nil;
  FColumns := nil;
  FEvents := nil;
  FScheduler := nil;
  FCapture := nil;
  FObservation := Default(TNyxColumnLayoutObservation);
  LError := '';

  if LFactory <> nil then
  begin
    try
      LFactory.Revoke;
    except
      on E: Exception do
      begin
        LError := TNyxText(E.Message);
      end;
    end;
  end;
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
  for LIndex := 0 to High(LExecutions) do
  begin
    try
      LExecutions[LIndex].Cancel;
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

function NewNyxColumnArrangementManager(const AView: INyxCollectionView;
  const AEvents: INyxEvents;
  const APresenters: INyxColumnArrangementPresenterFactory;
  APrepareFit: TNyxColumnArrangementFitPrepare): INyxColumnArrangementManager;
var
  LOwner: TArrangementManager;
  LView: INyxCollectionView;
  LEvents: INyxEvents;
  LFactory: INyxColumnArrangementPresenterFactory;
begin
  { Retain all caller slots before initialization can enter custom getters. }
  LView := AView;
  LEvents := AEvents;
  LFactory := APresenters;
  LOwner := TArrangementManager.Create;
  Result := LOwner;
  LOwner.Initialize(LView, LEvents, LFactory, APrepareFit);
end;

end.
