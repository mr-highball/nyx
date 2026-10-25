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
unit nyx.collections.range.actions;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.collections, nyx.collections.arrangement, nyx.collections.clipboard,
  nyx.collections.range, nyx.scheduler;

type
  { A newer selection/clear/retirement during an outward getter defeats the
    entered reservation. Mounted input may claim this refused default without
    reporting a malformed-data diagnostic or replacing the newer selection. }
  ENyxCollectionRangeStale = class(ENyxCollectionRange);

  { One accepted range generation owns immutable source/page snapshots and the
    exact bounded RAW rectangle, never the live store, view, mount or editor.
    Current/Readiness describe only selection lifetime. A command must ALSO
    compare the original source/page objects, column observation and input
    epoch against its mounted owner; this token cannot grant fresh UI access.
    Retaining the immutable data after retirement is safe. Readiness performs
    synchronized flags-only reads and is suitable for a detached host scope. }
  INyxCollectionCellRangeIntent = interface(IInterface)
    ['{D08A0885-447F-4B69-9F48-38D745EB5889}']
    function GetCapture: INyxCollectionCellRangeCapture;
    function GetSource: INyxCollectionSnapshot;
    function GetPage: INyxCollectionSnapshot;
    function GetReadiness: INyxCancellationReadiness;
    function GetCurrent: Boolean;
    property Capture: INyxCollectionCellRangeCapture read GetCapture;
    property Source: INyxCollectionSnapshot read GetSource;
    property Page: INyxCollectionSnapshot read GetPage;
    property Readiness: INyxCancellationReadiness read GetReadiness;
    property Current: Boolean read GetCurrent;
  end;

  { Explicit cell selection is independent of ordinary row selection. All
    methods are UI-owner operations; the mounted caller admits its scheduler
    and exact input BEFORE entry. This owner schedules nothing and invokes no
    observer. Select reserves intent before snapshot getters, admits a complete
    rectangle, then installs without callouts. A nested Select/Clear/Revoke
    prevents that older reservation from replacing the newer choice.

    Failed admission preserves the previous accepted range. Successful select
    and Clear retire the previous generation before releasing any old snapshot;
    Revoke is terminal. These operations cancel receiver authority only and
    cannot undo a clipboard write already entered on the platform. }
  INyxCollectionCellRangeSelection = interface(IInterface)
    ['{C7416955-174E-4378-9FE0-CCDD9B670903}']
    function Select(const ASource, APage: INyxCollectionSnapshot;
      const AVisibleFields: array of TNyxColumnIdentity;
      const AAnchor, AFocus: TNyxCollectionCellIdentity; AColumnSerial: Integer;
      AHeaders: Boolean; ALineEnding: TNyxCollectionCopyLineEnding;
      const ALimits: TNyxCollectionClipboardLimits): INyxCollectionCellRangeIntent;
    function GetSelection: INyxCollectionCellRangeIntent;
    procedure Clear;
    procedure Revoke;
    property Selection: INyxCollectionCellRangeIntent read GetSelection;
  end;

{ Create an inactive, independent selection owner. No source getters, scheduler
  work, input binding or platform clipboard operation occurs at construction. }
function NewNyxCollectionCellRangeSelection: INyxCollectionCellRangeSelection;

implementation

uses SysUtils, nyx.collections.range.navigation, nyx.collections.range.pointer,
  nyx.collections.range.stroke, nyx.publication;

type
  { The concrete intent is private so the selection owner can retire its known
    canonical cancellation flag without querying an arbitrary public provider. }
  TRangeIntent = class(TInterfacedObject, INyxCollectionCellRangeIntent)
  private
    FCapture: INyxCollectionCellRangeCapture;
    FSource: INyxCollectionSnapshot;
    FPage: INyxCollectionSnapshot;
    FScope: INyxCancellationScope;
    FReady: INyxCancellationReadiness;
  public
    constructor Create(const ACapture: INyxCollectionCellRangeCapture;
      const ASource, APage: INyxCollectionSnapshot);
    procedure Retire;
    function GetCapture: INyxCollectionCellRangeCapture;
    function GetSource: INyxCollectionSnapshot;
    function GetPage: INyxCollectionSnapshot;
    function GetReadiness: INyxCancellationReadiness;
    function GetCurrent: Boolean;
  end;

  { Prepared selection owns this UI controller independently. The controller
    never retains a preparation, so abandoning one cannot create a cycle. }
  TRangeSelection = class;

  { Single original request and its fully admitted candidate. Old/new immutable
    values stay leased through flags-only installation; abandonment cancels only
    the uninstalled candidate. No model or physical host participates here. }
  TRangePublication = class(TInterfacedObject, INyxPreparedPublication,
    INyxPublicationReadiness)
  private
    FOwner: TRangeSelection;
    FOwnerLease: INyxCollectionCellRangeSelection;
    FPrevious: INyxCollectionCellRangeIntent;
    FPreviousObject: TRangeIntent;
    FCandidate: INyxCollectionCellRangeIntent;
    FCandidateObject: TRangeIntent;
    FRequest: Integer;
    FValidated: Boolean;
    FInstalled: Boolean;
    FRetired: Boolean;
    procedure RequireCurrent;
    procedure DoRetire;
  public
    destructor Destroy; override;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

  { Only the SAME canonical range publication can back this private envelope.
    The selection owner never retains it. Claiming completion before readiness
    prevents consumed/reentrant attempts from installing the original candidate
    later; retained proposal values contain no selection or mounted authority. }
  TPointerCommand = class(TInterfacedObject, INyxCollectionCellRangePointerCommand,
    INyxCollectionCellRangePointerPreview)
  private
    FProposal: INyxCollectionRangePointer;
    FPlan: INyxPreparedPublication;
    FObject: TRangePublication;
    FCompleted: Boolean;
    FRetired: Boolean;
    function PlanCurrent: Boolean;
    procedure DoRetire;
  public
    destructor Destroy; override;
    function GetProposal: INyxCollectionRangePointer;
    function GetPreview: INyxCollectionCellRangeCapture;
    function GetCurrent: Boolean;
    function Complete(AAllowDefault: Boolean): Boolean;
    procedure Retire;
  end;

  { The UI owner retains exactly one accepted intent. The borrowed concrete
    pointer is valid only while FSelection owns it. Intent serial is separate
    from accepted lifetime: failed work cannot revoke the previous selection,
    but a nested newer reservation makes the older operation refuse. }
  TRangeSelection = class(TInterfacedObject, INyxCollectionCellRangeSelection,
    INyxCollectionCellRangePreparation, INyxCollectionCellRangePointerPreparation)
  private
    FSelection: INyxCollectionCellRangeIntent;
    FSelected: TRangeIntent;
    FIntent: Integer;
    FRevoked: Boolean;
    procedure ClearInternal(ARevoke: Boolean);
    procedure InstallSelection(const ACandidate: INyxCollectionCellRangeIntent;
      ACandidateObject: TRangeIntent);
  public
    destructor Destroy; override;
    function Select(const ASource, APage: INyxCollectionSnapshot;
      const AVisibleFields: array of TNyxColumnIdentity;
      const AAnchor, AFocus: TNyxCollectionCellIdentity; AColumnSerial: Integer;
      AHeaders: Boolean; ALineEnding: TNyxCollectionCopyLineEnding;
      const ALimits: TNyxCollectionClipboardLimits): INyxCollectionCellRangeIntent;
    function GetSelection: INyxCollectionCellRangeIntent;
    procedure Clear;
    procedure Revoke;
    function PrepareNavigation(const ANavigation: INyxCollectionRangeNavigation): INyxPreparedPublication;
    function PreparePointer(const ASource, APage: INyxCollectionSnapshot;
      const AVisibleFields: array of TNyxColumnIdentity;
      const AOrigin, ATarget: TNyxCollectionCellIdentity;
      const APrevious: INyxCollectionCellRangeIntent;
      AAction: TNyxCollectionRangePointerAction; AColumnSerial: Integer;
      AHeaders: Boolean; ALineEnding: TNyxCollectionCopyLineEnding;
      const ALimits: TNyxCollectionClipboardLimits): INyxCollectionCellRangePointerCommand;
  end;

constructor TRangeIntent.Create(const ACapture: INyxCollectionCellRangeCapture;
  const ASource, APage: INyxCollectionSnapshot);
var
  LReady: INyxCancellationReadiness;
begin
  inherited Create;
  FCapture := ACapture;
  FSource := ASource;
  FPage := APage;
  FScope := NewNyxCancellationScope;

  if not Supports(FScope, INyxCancellationReadiness, LReady) then
  begin
    raise ENyxCollectionRange.Create('Cell range requires canonical cancellation readiness');
  end;
  FReady := LReady;
end;

procedure TRangeIntent.Retire;
begin
  FScope.Cancel;
end;

function TRangeIntent.GetCapture: INyxCollectionCellRangeCapture;
begin
  Result := FCapture;
end;

function TRangeIntent.GetSource: INyxCollectionSnapshot;
begin
  Result := FSource;
end;

function TRangeIntent.GetPage: INyxCollectionSnapshot;
begin
  Result := FPage;
end;

function TRangeIntent.GetReadiness: INyxCancellationReadiness;
begin
  Result := FReady;
end;

function TRangeIntent.GetCurrent: Boolean;
begin
  Result := not FScope.Cancelled;
end;

destructor TRangePublication.Destroy;
begin
  DoRetire;
  inherited Destroy;
end;

procedure TRangePublication.RequireCurrent;
begin

  if FRetired or FInstalled or (FOwner = nil) or FOwner.FRevoked or
    (FRequest >= High(Integer)) or (FOwner.FIntent <> FRequest) or
    (FOwner.FSelection <> FPrevious) or (FOwner.FSelected <> FPreviousObject) then
  begin
    raise ENyxCollectionRangeStale.Create('Prepared cell range request is no longer current');
  end;

  if FPreviousObject <> nil then
  begin
    FPreviousObject.FReady.CheckReady;
  end;
end;

procedure TRangePublication.Validate;
begin
  RequireCurrent;
  FValidated := True;
end;

procedure TRangePublication.CheckReady;
begin

  if not FValidated then
  begin
    raise ENyxCollectionRange.Create('Prepared cell range was not validated');
  end;
  RequireCurrent;
end;

procedure TRangePublication.Install;
begin

  if not FValidated or FRetired or FInstalled then
  begin
    Exit;
  end;
  { The coordinator's final readiness check already admitted these exact
    private flags. Old/candidate leases survive this nonthrowing assignment. }
  FOwner.InstallSelection(FCandidate, FCandidateObject);
  FInstalled := True;
end;

procedure TRangePublication.Notify;
begin
  { Independent cell selection has no model/view observers or host effects. }
end;

procedure TRangePublication.DoRetire;
var
  LOwner: INyxCollectionCellRangeSelection;
  LPrevious: INyxCollectionCellRangeIntent;
  LCandidate: INyxCollectionCellRangeIntent;
begin

  if FRetired then
  begin
    Exit;
  end;
  FRetired := True;
  LOwner := FOwnerLease;
  LPrevious := FPrevious;
  LCandidate := FCandidate;

  if not FInstalled and (FCandidateObject <> nil) then
  begin
    FCandidateObject.Retire;
  end;
  FOwner := nil;
  FOwnerLease := nil;
  FPrevious := nil;
  FPreviousObject := nil;
  FCandidate := nil;
  FCandidateObject := nil;
  { Snapshot finalizers can reenter their application. Detach all duties first,
    and retain the controller until both independent value leases are gone. }
  LPrevious := nil;
  LCandidate := nil;
  LOwner := nil;
end;

procedure TRangePublication.Retire;
var
  LLease: INyxPreparedPublication;
begin
  LLease := Self;
  DoRetire;
end;

destructor TPointerCommand.Destroy;
begin
  DoRetire;
  inherited Destroy;
end;

function TPointerCommand.GetProposal: INyxCollectionRangePointer;
begin
  Result := FProposal;
end;

function TPointerCommand.GetPreview: INyxCollectionCellRangeCapture;
begin
  Result := nil;

  if GetCurrent and (FObject.FCandidate <> nil) then
  begin
    Result := FObject.FCandidate.Capture;
  end;
end;

function TPointerCommand.PlanCurrent: Boolean;
begin
  Result := False;

  if FRetired or (FPlan = nil) or (FObject = nil) then
  begin
    Exit;
  end;
  try
    { FObject is our private canonical publication, with flags-only readiness.
      No arbitrary extension or application callback runs during completion. }
    FObject.CheckReady;
    Result := True;
  except
    on E: ENyxCollectionRangeStale do
    begin
      { A newer selection/preparation or retirement keeps its original result. }
    end;
  end;
end;

function TPointerCommand.GetCurrent: Boolean;
begin
  Result := not FCompleted and PlanCurrent;
end;

function TPointerCommand.Complete(AAllowDefault: Boolean): Boolean;
var
  LLease: INyxCollectionCellRangePointerCommand;
begin
  LLease := Self;
  Result := False;

  if FCompleted then
  begin
    Exit;
  end;
  FCompleted := True;

  if not AAllowDefault or not PlanCurrent then
  begin
    Exit;
  end;
  FObject.Install;
  FObject.Notify;
  Result := FObject.FInstalled;
end;

procedure TPointerCommand.DoRetire;
var
  LPlan: INyxPreparedPublication;
begin

  if FRetired then
  begin
    Exit;
  end;
  FRetired := True;
  LPlan := FPlan;
  FObject := nil;
  FPlan := nil;

  if LPlan <> nil then
  begin
    { Last callout: snapshot finalizers may release caller-held envelopes or
      enter another selection; all duties were detached before retirement. }
    LPlan.Retire;
  end;
end;

procedure TPointerCommand.Retire;
var
  LLease: INyxCollectionCellRangePointerCommand;
begin
  LLease := Self;
  DoRetire;
end;

destructor TRangeSelection.Destroy;
begin
  { Final release must not create a new Self interface. The canonical flag is
    retired and all fields detached before any old foreign snapshot finalizes. }
  ClearInternal(True);
  inherited Destroy;
end;

procedure TRangeSelection.ClearInternal(ARevoke: Boolean);
var
  LPrevious: INyxCollectionCellRangeIntent;
  LSelected: TRangeIntent;
begin
  FRevoked := FRevoked or ARevoke;

  if FIntent < High(Integer) then
  begin
    Inc(FIntent);
  end;
  LPrevious := FSelection;
  LSelected := FSelected;
  FSelected := nil;
  FSelection := nil;

  if LSelected <> nil then
  begin
    LSelected.Retire;
  end;
  { LPrevious releases only after the last Self access. Its payload is inert
    before caller-owned snapshot destructors can reenter this selection. }
end;

procedure TRangeSelection.Clear;
var
  LLease: INyxCollectionCellRangeSelection;
begin
  LLease := Self;
  ClearInternal(False);
end;

procedure TRangeSelection.Revoke;
var
  LLease: INyxCollectionCellRangeSelection;
begin
  LLease := Self;
  ClearInternal(True);
end;

function TRangeSelection.GetSelection: INyxCollectionCellRangeIntent;
begin
  Result := FSelection;
end;

function TRangeSelection.Select(const ASource, APage: INyxCollectionSnapshot;
  const AVisibleFields: array of TNyxColumnIdentity;
  const AAnchor, AFocus: TNyxCollectionCellIdentity; AColumnSerial: Integer;
  AHeaders: Boolean; ALineEnding: TNyxCollectionCopyLineEnding;
  const ALimits: TNyxCollectionClipboardLimits): INyxCollectionCellRangeIntent;
var
  LLease: INyxCollectionCellRangeSelection;
  LSource: INyxCollectionSnapshot;
  LPage: INyxCollectionSnapshot;
  LCapture: INyxCollectionCellRangeCapture;
  LCandidate: TRangeIntent;
  LCandidateLease: INyxCollectionCellRangeIntent;
  LPrevious: INyxCollectionCellRangeIntent;
  LIntent: Integer;
begin
  LLease := Self;
  LSource := ASource;
  LPage := APage;

  if FRevoked or (FIntent >= High(Integer) - 1) then
  begin
    raise ENyxCollectionRange.Create('Cell range selection is retired or saturated');
  end;
  Inc(FIntent);
  LIntent := FIntent;
  { The pure producer copies all caller fields/records before its first getter.
    Its complete output is installed only if this SAME reservation survives. }
  LCapture := NewNyxCollectionCellRange(LSource, LPage, AVisibleFields,
    AAnchor, AFocus, AColumnSerial, AHeaders, ALineEnding, ALimits);
  LCandidate := TRangeIntent.Create(LCapture, LSource, LPage);
  LCandidateLease := LCandidate;

  if FRevoked or (FIntent <> LIntent) then
  begin
    LCandidate.Retire;
    raise ENyxCollectionRangeStale.Create('Cell range reservation was replaced during capture');
  end;
  LPrevious := FSelection;
  InstallSelection(LCandidateLease, LCandidate);
  { Final-only result assignment also preserves a caller's earlier interface
    when admission fails. No caller-owned interface is released mid-install. }
  Result := LCandidateLease;
end;

procedure TRangeSelection.InstallSelection(const ACandidate: INyxCollectionCellRangeIntent;
  ACandidateObject: TRangeIntent);
begin

  if FSelected <> nil then
  begin
    FSelected.Retire;
  end;
  FSelection := ACandidate;
  FSelected := ACandidateObject;
end;

function TRangeSelection.PrepareNavigation(
  const ANavigation: INyxCollectionRangeNavigation): INyxPreparedPublication;
var
  LLease: INyxCollectionCellRangeSelection;
  LNavigation: INyxCollectionRangeNavigation;
  LNormalized: INyxCollectionRangeNavigation;
  LPageNavigation: INyxCollectionRangePageNavigation;
  LOriginal: INyxCollectionCellRangeIntent;
  LPrevious: INyxCollectionCellRangeIntent;
  LPreviousObject: TRangeIntent;
  LSource: INyxCollectionSnapshot;
  LPage: INyxCollectionSnapshot;
  LFields: TNyxCollectionClipboardFields;
  LOrigin: TNyxCollectionCellIdentity;
  LAnchor: TNyxCollectionCellIdentity;
  LFocus: TNyxCollectionCellIdentity;
  LKey: TNyxCollectionRangeKey;
  LLimits: TNyxCollectionClipboardLimits;
  LLines: TNyxCollectionCopyLineEnding;
  LHeaders: Boolean;
  LHasPage: Boolean;
  LSerial: Integer;
  LCount: Integer;
  LIndex: Integer;
  LRequest: Integer;
  LPageRows: Integer;
  LCapture: INyxCollectionCellRangeCapture;
  LCandidate: TRangeIntent;
  LCandidateLease: INyxCollectionCellRangeIntent;
  LPublication: TRangePublication;
  LPublicationLease: INyxPreparedPublication;

  procedure RequireReservation;
  begin

    if FRevoked or (FIntent <> LRequest) or (FSelection <> LOriginal) then
    begin
      raise ENyxCollectionRangeStale.Create('Cell range preparation was replaced during capture');
    end;
  end;

begin
  LLease := Self;

  if FRevoked or (FIntent >= High(Integer) - 1) then
  begin
    raise ENyxCollectionRange.Create('Cell range selection is retired or saturated');
  end;
  Inc(FIntent);
  LRequest := FIntent;
  LOriginal := FSelection;
  LPreviousObject := FSelected;
  LNavigation := ANavigation;

  if LNavigation = nil then
  begin
    raise ENyxCollectionRange.Create('Cell range preparation requires its original movement');
  end;
  LPrevious := LNavigation.Previous;
  RequireReservation;

  if LPrevious <> LOriginal then
  begin
    raise ENyxCollectionRangeStale.Create('Cell range movement belongs to another accepted selection');
  end;
  LKey := LNavigation.Key;
  RequireReservation;
  LCandidate := nil;
  LCandidateLease := nil;

  if LKey = ncrkClear then
  begin

    if LOriginal = nil then
    begin
      raise ENyxCollectionRange.Create('Prepared Escape requires its original selected cells');
    end;
  end
  else
  begin
    LPageRows := 1;

    if LKey in [ncrkExtendPageUp, ncrkExtendPageDown] then
    begin
      { QueryInterface and the extension getter are both outward calls. Claim
        the original selection request before them and fence each return. }
      LHasPage := Supports(LNavigation, INyxCollectionRangePageNavigation, LPageNavigation);
      RequireReservation;

      if not LHasPage then
      begin
        raise ENyxCollectionRange.Create('Page movement requires its original viewport step');
      end;
      LPageRows := LPageNavigation.PageRows;
      RequireReservation;

      if LPageRows < 1 then
      begin
        raise ENyxCollectionRange.Create('Page movement has an invalid viewport step');
      end;
    end;
    LSource := LNavigation.Source;
    RequireReservation;
    LPage := LNavigation.Page;
    RequireReservation;
    LOrigin := LNavigation.Origin;
    RequireReservation;
    LAnchor := LNavigation.Anchor;
    RequireReservation;
    LFocus := LNavigation.Focus;
    RequireReservation;
    LSerial := LNavigation.ColumnSerial;
    RequireReservation;
    LCount := LNavigation.FieldCount;
    RequireReservation;

    if (LCount < 1) or (LCount > NyxMaximumCollectionFields) then
    begin
      raise ENyxCollectionRange.Create('Prepared movement visible fields exceed the column limit');
    end;
    SetLength(LFields, LCount);
    for LIndex := 0 to High(LFields) do
    begin
      LFields[LIndex] := LNavigation.FieldAt(LIndex);
      RequireReservation;
    end;
    LHeaders := LNavigation.IncludeHeaders;
    RequireReservation;
    LLines := LNavigation.LineEnding;
    RequireReservation;
    LLimits := LNavigation.Limits;
    RequireReservation;

    if (LOriginal <> nil) and ((LOriginal.Source <> LSource) or
      (LOriginal.Page <> LPage) or not LOrigin.Same(LOriginal.Capture.Focus) or
      not LAnchor.Same(LOriginal.Capture.Anchor) or
      (LOriginal.Capture.ColumnSerial <> LSerial)) then
    begin
      raise ENyxCollectionRangeStale.Create('Prepared movement cannot retarget its original cell range');
    end;

    if (LOriginal = nil) and not LOrigin.Same(LAnchor) then
    begin
      raise ENyxCollectionRange.Create('Initial range movement must retain its exact origin anchor');
    end;
    { Alternate proposal implementations are extension callouts, not trusted
      navigation engines. Re-admit the key with the shared arithmetic before
      accepting its claimed endpoints or complete RAW candidate. }
    LNormalized := NewNyxCollectionRangeNavigation(LSource, LPage, LFields,
      LOrigin, LOriginal, LKey, LSerial, LHeaders, LLines, LLimits, LPageRows);
    RequireReservation;

    if not LNormalized.Anchor.Same(LAnchor) or not LNormalized.Focus.Same(LFocus) then
    begin
      raise ENyxCollectionRange.Create('Prepared movement endpoints do not match its typed key');
    end;
    LCapture := NewNyxCollectionCellRange(LSource, LPage, LFields, LAnchor, LFocus,
      LSerial, LHeaders, LLines, LLimits);
    RequireReservation;
    LCandidate := TRangeIntent.Create(LCapture, LSource, LPage);
    LCandidateLease := LCandidate;
    RequireReservation;
  end;
  LPublication := TRangePublication.Create;
  LPublicationLease := LPublication;
  LPublication.FOwner := Self;
  LPublication.FOwnerLease := LLease;
  LPublication.FPrevious := LOriginal;
  LPublication.FPreviousObject := LPreviousObject;
  LPublication.FCandidate := LCandidateLease;
  LPublication.FCandidateObject := LCandidate;
  LPublication.FRequest := LRequest;
  Result := LPublicationLease;
end;

function TRangeSelection.PreparePointer(const ASource, APage: INyxCollectionSnapshot;
  const AVisibleFields: array of TNyxColumnIdentity;
  const AOrigin, ATarget: TNyxCollectionCellIdentity;
  const APrevious: INyxCollectionCellRangeIntent;
  AAction: TNyxCollectionRangePointerAction; AColumnSerial: Integer;
  AHeaders: Boolean; ALineEnding: TNyxCollectionCopyLineEnding;
  const ALimits: TNyxCollectionClipboardLimits): INyxCollectionCellRangePointerCommand;
var
  LLease: INyxCollectionCellRangeSelection;
  LOriginal: INyxCollectionCellRangeIntent;
  LPreviousObject: TRangeIntent;
  LProposal: INyxCollectionRangePointer;
  LFields: TNyxCollectionClipboardFields;
  LRequest: Integer;
  LIndex: Integer;
  LCapture: INyxCollectionCellRangeCapture;
  LCandidate: TRangeIntent;
  LCandidateLease: INyxCollectionCellRangeIntent;
  LPublication: TRangePublication;
  LPublicationLease: INyxPreparedPublication;
  LCommand: TPointerCommand;
  LCommandLease: INyxCollectionCellRangePointerCommand;

  procedure RequireReservation;
  begin

    if FRevoked or (FIntent <> LRequest) or (FSelection <> LOriginal) then
    begin
      raise ENyxCollectionRangeStale.Create('Pointer range preparation was replaced during capture');
    end;
  end;

begin
  LLease := Self;

  if FRevoked or (FIntent >= High(Integer) - 1) then
  begin
    raise ENyxCollectionRange.Create('Cell range selection is retired or saturated');
  end;
  Inc(FIntent);
  LRequest := FIntent;
  LOriginal := FSelection;
  LPreviousObject := FSelected;

  if APrevious <> LOriginal then
  begin
    raise ENyxCollectionRangeStale.Create('Pointer range belongs to another original selection');
  end;
  { Constructor copies arguments before its first immutable-source getter.
    The owner request above defeats nested nil-selection ABA as well as a
    genuinely newer accepted range; no late recapture can borrow its authority. }
  LProposal := NewNyxCollectionRangePointer(ASource, APage, AVisibleFields,
    AOrigin, ATarget, LOriginal, AAction, AColumnSerial, AHeaders, ALineEnding, ALimits);
  RequireReservation;
  SetLength(LFields, LProposal.FieldCount);
  for LIndex := 0 to High(LFields) do
  begin
    LFields[LIndex] := LProposal.FieldAt(LIndex);
  end;
  LCapture := NewNyxCollectionCellRange(LProposal.Source, LProposal.Page, LFields,
    LProposal.Anchor, LProposal.Focus, LProposal.ColumnSerial,
    LProposal.IncludeHeaders, LProposal.LineEnding, LProposal.Limits);
  RequireReservation;
  LCandidate := TRangeIntent.Create(LCapture, LProposal.Source, LProposal.Page);
  LCandidateLease := LCandidate;
  RequireReservation;
  LPublication := TRangePublication.Create;
  LPublicationLease := LPublication;
  LPublication.FOwner := Self;
  LPublication.FOwnerLease := LLease;
  LPublication.FPrevious := LOriginal;
  LPublication.FPreviousObject := LPreviousObject;
  LPublication.FCandidate := LCandidateLease;
  LPublication.FCandidateObject := LCandidate;
  LPublication.FRequest := LRequest;
  LPublication.Validate;
  LCommand := TPointerCommand.Create;
  LCommandLease := LCommand;
  LCommand.FProposal := LProposal;
  LCommand.FPlan := LPublicationLease;
  LCommand.FObject := LPublication;
  Result := LCommandLease;
end;

function NewNyxCollectionCellRangeSelection: INyxCollectionCellRangeSelection;
begin
  Result := TRangeSelection.Create;
end;

end.
