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

unit nyx.popover;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.types, nyx.root.types, nyx.model, nyx.controls, nyx.popover.types,
  nyx.state, nyx.resources, nyx.behavior, nyx.events, nyx.collections.view, nyx.collections.bindings;

const
  npsBelow = nyx.popover.types.npsBelow;
  npsAbove = nyx.popover.types.npsAbove;
  npsRight = nyx.popover.types.npsRight;
  npsLeft = nyx.popover.types.npsLeft;
  npaStart = nyx.popover.types.npaStart;
  npaCenter = nyx.popover.types.npaCenter;
  npaEnd = nyx.popover.types.npaEnd;
  npzContent = nyx.popover.types.npzContent;
  npzFixed = nyx.popover.types.npzFixed;
  npdEscape = nyx.popover.types.npdEscape;
  npdOutsidePress = nyx.popover.types.npdOutsidePress;
  nprNone = nyx.popover.types.nprNone;
  nprClose = nyx.popover.types.nprClose;
  nprEscape = nyx.popover.types.nprEscape;
  nprOutsidePress = nyx.popover.types.nprOutsidePress;
  nprAnchorUnavailable = nyx.popover.types.nprAnchorUnavailable;
  nprAction = nyx.popover.types.nprAction;

type
  { Compatibility aliases retain one pure policy implementation. }
  TNyxPopoverSide = nyx.popover.types.TNyxPopoverSide;
  TNyxPopoverAlignment = nyx.popover.types.TNyxPopoverAlignment;
  TNyxPopoverSizing = nyx.popover.types.TNyxPopoverSizing;
  TNyxPopoverDismissal = nyx.popover.types.TNyxPopoverDismissal;
  TNyxPopoverDismissals = nyx.popover.types.TNyxPopoverDismissals;
  TNyxPopoverReason = nyx.popover.types.TNyxPopoverReason;
  TNyxPopoverRect = nyx.popover.types.TNyxPopoverRect;
  TNyxPopoverOptions = nyx.popover.types.TNyxPopoverOptions;

  { Owns a complete independent document snapshot, its selected page/reusable
    root and independent scalar/collection runtime stores. Content is specialized
    through RetainNyxControl; configure it BEFORE the first successful Open.
    Mounted content is then stable. Close/reopen retains controls, unbound drafts,
    bindings and subscriptions. Retained content handles outlive the presenter.

    Events is the ordinary mounted Nyx router. OnDismiss is a separate ordered,
    multi-registration completion stream using the same scheduler contract.
    Dismiss closes before callbacks; a callback may reopen/release safely.
    LastReason carries a strong enum; completion uses nseDismiss, with no string
    policy or inferred callback name. Close is silent for this presenter and
    idempotent; contextual descendants report anchor loss. Destruction retires
    the complete declared family without completion notifications.
    All presentation methods require the UI thread. Do not register a callback
    that strongly retains its own presenter, creating an application cycle. }
  { Version-two native interface identity accompanies the added fluent method.
    Rebuild native adapter/packages together; never expose a changed vtable under
    the old GUID. Document/resource wire versions are independent and unchanged. }
  INyxPopover = interface(IInterface)
    ['{B2672EF9-496D-4EC9-8006-131026000001}']
    function GetContent: INyxControl;
    function GetEvents: INyxEvents;
    function GetState: TNyxState;
    function GetOpen: Boolean;
    function GetLastReason: TNyxPopoverReason;
    function OnDismiss: INyxEventStream;
    { Declare a contextual child before opening it. The target adapter verifies
      that its anchor belongs to the parent's presented family. Nil detaches a
      closed child. Open/transitioning members, cycles and incompatible adapters
      refuse before changing the relation. The relation owns weak lifetime leases,
      never either presenter or its document; retain the returned interface. }
    function Within(const AParent: INyxPopover): INyxPopover;
    procedure Open(const AOptions: TNyxPopoverOptions);
    procedure Close;
    procedure Dismiss(AReason: TNyxPopoverReason = nprAction);
    property Content: INyxControl read GetContent;
    property Events: INyxEvents read GetEvents;
    { Borrowed independent runtime store; never free it or retain past owner.
      Its changes are validated by the mounted renderer, not saved defaults. }
    property State: TNyxState read GetState;
    property IsOpen: Boolean read GetOpen;
    property LastReason: TNyxPopoverReason read GetLastReason;
  end;

  { Readonly authority for ONE successful presentation. Close, dismissal,
    retirement or loss of its original parent presentation permanently revokes
    it; reopening creates a different token. Retained tokens own no presenter,
    document, router or physical host. Current permits native worker reads and
    has no application callbacks; it grants no permission to access UI there. }
  INyxPopoverOpenLifetime = interface(IInterface)
    ['{A9341D89-6280-45E9-9B62-792707A4770C}']
    function GetCurrent: Boolean;
    property Current: Boolean read GetCurrent;
  end;

  { Optional UI-thread capture, preserving the existing INyxPopover identity and
    vtable. Closed, transitioning and retired presenters return nil. Capture the
    actual host's token before deferring work; querying IsOpen after reopening
    cannot establish the identity of the original presentation. }
  INyxPopoverOpenAdmission = interface(IInterface)
    ['{6BA2F41D-264F-4F30-9B8F-2C26F574BDDA}']
    function GetOpenLifetime: INyxPopoverOpenLifetime;
  end;

  { Shared lifecycle owner, completed below after its weak nesting seam. }
  TNyxPopoverPresenter = class;
  { Borrowed synchronous descendants; no element extends a presenter's life. }
  TNyxPopoverPresenters = array of TNyxPopoverPresenter;
  { Internal revocable implementation lease, following the menu lifetime seam.
    Managed holders keep this small lease alive, not its borrowed presenter.
    Retirement clears Owner before target views disappear. }
  TNyxPopoverLease = class(TInterfacedObject)
  public
    Owner: TNyxPopoverPresenter;
  end;
  { Optional implementation seam for the shared nesting coordinator. Custom
    presenters may inherit the coordinator or expose this same revocable lease;
    an implementation without the seam refuses Within rather than borrowing an
    unqualified owner. The lease is borrowed for this call only. }
  INyxPopoverNesting = interface(IInterface)
    ['{3A9B011A-48CE-4504-9400-131026000001}']
    function NestingLease: TNyxPopoverLease;
  end;

  { Target seam. Subclasses borrow no original document or content, disconnect
    input/timers and release renderers before inherited destruction. A canonical
    owner-free token follows each successful Open, with immutable ancestry and
    terminal revocation before physical cleanup can call application code. }
  TNyxPopoverPresenter = class(TInterfacedObject, INyxPopover, INyxPopoverNesting,
    INyxPopoverOpenAdmission)
  private
    FDocument: TNyxDocument;
    FContent: INyxControl;
    FState: TNyxState;
    FCollections: INyxCollectionBindings;
    FCompletion: INyxEvents;
    FOpen: Boolean;
    FTransition: Boolean;
    FMounted: Boolean;
    FLastReason: TNyxPopoverReason;
    FLease: TNyxPopoverLease;
    FLeaseOwner: IInterface;
    FParentLease: TNyxPopoverLease;
    FParentOwner: IInterface;
    FChildren: array of TNyxPopoverLease;
    FChildOwners: array of IInterface;
    FOpenLifetime: INyxPopoverOpenLifetime;
    { Borrowed private implementation, retained only by FOpenLifetime. No token
      holds a backreference. UI lifecycle writes use this canonical object. }
    FOpenObject: TObject;
    procedure RevokeOpenLifetime;
    procedure RemoveChild(ALease: TNyxPopoverLease);
    procedure DetachParent;
  protected
    procedure PrepareContent;
    procedure Present(const AOptions: TNyxPopoverOptions;
      const AFocusID: TNyxText); virtual; abstract;
    procedure Conceal(ARestoreFocus: Boolean); virtual; abstract;
    procedure Finish(AReason: TNyxPopoverReason; ANotify: Boolean);
    { Physical ancestry admission is adapter-owned; shared lifetime/closure is
      independent of DOM and LCL. The default refuses an unknown target adapter. }
    function CanNestWithin(AParent: TNyxPopoverPresenter): Boolean; virtual;
    { Borrow open descendant owners only during synchronous target containment.
      The returned array owns no presenter and must not cross an event callback. }
    function NestedPresenters: TNyxPopoverPresenters;
    { Close descendants before hiding or retiring their anchor view. Reopening
      into this closing parent refuses; callbacks may move to a different parent. }
    procedure CloseNested(ANotify: Boolean = True);
    { Target subclasses call this before releasing their hosts. It revokes the
      owner lease and silently closes descendants; repeated retirement is safe. }
    procedure RetireNesting;
    property Document: TNyxDocument read FDocument;
    property Collections: INyxCollectionBindings read FCollections;
    property Mounted: Boolean read FMounted write FMounted;
  public
    { Nil documents and missing exact roots refuse before a physical host opens.
      The supplied document can be destroyed immediately after construction. }
    constructor Create(ADocument: TNyxDocument; const ARoot: TNyxRootRef);
    destructor Destroy; override;
    function GetContent: INyxControl;
    function GetEvents: INyxEvents; virtual; abstract;
    function GetState: TNyxState;
    function GetOpen: Boolean;
    { Nonvirtual canonical capture: ancestor admission must not call an
      alternative override capable of replacing the nesting context. }
    function GetOpenLifetime: INyxPopoverOpenLifetime;
    function GetLastReason: TNyxPopoverReason;
    function OnDismiss: INyxEventStream;
    function NestingLease: TNyxPopoverLease;
    function Within(const AParent: INyxPopover): INyxPopover;
    procedure Open(const AOptions: TNyxPopoverOptions);
    procedure Close;
    procedure Dismiss(AReason: TNyxPopoverReason = nprAction);
    property IsOpen: Boolean read GetOpen;
    property State: TNyxState read GetState;
  end;

function NyxPopover(const ATitle: TNyxText): TNyxPopoverOptions;
{ Read the immutable typed reason from this presenter's owned completion
  snapshot. Unlike LastReason it survives reopening, queues and retirement.
  Foreign/non-completion payloads refuse; malformed numeric values also retain
  the ordinary data contract's exact-integer admission. }
function NyxPopoverDismissReason(const AEvent: TNyxEventInfo): TNyxPopoverReason;
function NyxPopoverRect(ALeft, ATop, AWidth, AHeight: Integer): TNyxPopoverRect;
{ Prefer the requested side. Flip only when the opposite side offers more space
  and the requested side cannot fit. Clamp both axes to the viewport margin.
  Tiny viewports reduce the margin rather than producing an invalid rectangle. }
function PlaceNyxPopover(const AAnchor, AViewport: TNyxPopoverRect;
  const AOptions: TNyxPopoverOptions): TNyxPopoverRect;

implementation

uses
  SysUtils, nyx.composition, nyx.data;

const
  CReasonField: TNyxText = 'popover-reason';

type
  { Closed presentation phases; ordinal storage is confined to the native
    atomic boundary and never becomes a public or persisted numeric policy. }
  TPopoverOpenPhase = (popPreparing, popActive, popRetired);

  { Immutable parent proof plus one typed phase. Only the UI owner writes;
    native retained readers perform atomic
    loads and inspect this canonical token ancestry without touching presenters.
    Activation cannot revive a token retired by a reentrant target callback. }
  TPopoverOpenToken = class(TInterfacedObject, INyxPopoverOpenLifetime)
  private
    FParent: INyxPopoverOpenLifetime;
    FPhase: LongInt;
    function ReadPhase: TPopoverOpenPhase;
  public
    constructor Create(const AParent: INyxPopoverOpenLifetime);
    procedure Activate;
    procedure Revoke;
    function GetCurrent: Boolean;
  end;

constructor TPopoverOpenToken.Create(const AParent: INyxPopoverOpenLifetime);
begin
  inherited Create;
  FParent := AParent;
  FPhase := Ord(popPreparing);
end;

function TPopoverOpenToken.ReadPhase: TPopoverOpenPhase;
begin
  {$ifdef PAS2JS}
  Result := TPopoverOpenPhase(FPhase);
  {$else}
  Result := TPopoverOpenPhase(InterlockedCompareExchange(FPhase,
    Ord(popPreparing), Ord(popPreparing)));
  {$endif}
end;

procedure TPopoverOpenToken.Activate;
begin
  {$ifdef PAS2JS}

  if TPopoverOpenPhase(FPhase) = popPreparing then
  begin
    FPhase := Ord(popActive);
  end;
  {$else}
  InterlockedCompareExchange(FPhase, Ord(popActive), Ord(popPreparing));
  {$endif}
end;

procedure TPopoverOpenToken.Revoke;
begin
  {$ifdef PAS2JS}
  FPhase := Ord(popRetired);
  {$else}
  InterlockedExchange(FPhase, Ord(popRetired));
  {$endif}
end;

function TPopoverOpenToken.GetCurrent: Boolean;
begin
  Result := ReadPhase = popActive;

  if Result and (FParent <> nil) then
  begin
    Result := FParent.Current and (ReadPhase = popActive);
  end;
end;

procedure TNyxPopoverPresenter.RevokeOpenLifetime;
var
  LToken: TPopoverOpenToken;
begin
  LToken := TPopoverOpenToken(FOpenObject);

  if LToken <> nil then
  begin
    LToken.Revoke;
  end;
  FOpenObject := nil;
  FOpenLifetime := nil;
end;

function NyxPopoverDismissReason(const AEvent: TNyxEventInfo): TNyxPopoverReason;
var
  LValue: TNyxDataValue;
  LReason: Integer;
begin

  if not AEvent.IsNamed(NyxSemantic(nseDismiss)) or not AEvent.HasDetails or
    (AEvent.Details.Kind <> ndObject) or (AEvent.Details.Count <> 1) or
    (AEvent.Details.Key(0) <> CReasonField) then
  begin
    raise ENyxModel.Create('Event is not a popover completion snapshot');
  end;
  LValue := AEvent.Details.Field(CReasonField);

  if LValue.Kind <> ndNumber then
  begin
    raise ENyxModel.Create('Popover completion reason requires an enum ordinal');
  end;
  LReason := LValue.AsInteger;

  if (LReason < Ord(nprEscape)) or (LReason > Ord(nprAction)) then
  begin
    raise ENyxModel.Create('Unknown popover completion reason');
  end;
  Result := TNyxPopoverReason(LReason);
end;

function NyxPopoverRect(ALeft, ATop, AWidth, AHeight: Integer): TNyxPopoverRect;
begin
  Result := nyx.popover.types.NyxPopoverRect(ALeft, ATop, AWidth, AHeight);
end;

function NyxPopover(const ATitle: TNyxText): TNyxPopoverOptions;
begin
  Result := nyx.popover.types.NyxPopover(ATitle);
end;

function PlaceNyxPopover(const AAnchor, AViewport: TNyxPopoverRect;
  const AOptions: TNyxPopoverOptions): TNyxPopoverRect;
begin
  Result := nyx.popover.types.PlaceNyxPopover(AAnchor, AViewport, AOptions);
end;

constructor TNyxPopoverPresenter.Create(ADocument: TNyxDocument;
  const ARoot: TNyxRootRef);
begin
  inherited Create;

  if (ADocument = nil) or (ADocument.FindRoot(ARoot) = nil) then
  begin
    raise ENyxModel.Create('Popover requires an exact document root');
  end;
  FDocument := ADocument.Clone;
  FLease := TNyxPopoverLease.Create;
  FLease.Owner := Self;
  FLeaseOwner := FLease;
  FContent := RetainNyxControl(FDocument.FindRoot(ARoot));
  FState := FDocument.State.Clone;
  FCompletion := NewNyxEvents;
end;

destructor TNyxPopoverPresenter.Destroy;
begin
  RetireNesting;

  if FCompletion <> nil then
  begin
    FCompletion.Close;
  end;
  FCollections := nil;
  FCompletion := nil;
  FContent := nil;
  FState.Free;
  FDocument.Free;
  inherited Destroy;
end;

function TNyxPopoverPresenter.NestingLease: TNyxPopoverLease;
begin
  Result := FLease;
end;

function TNyxPopoverPresenter.CanNestWithin(AParent: TNyxPopoverPresenter): Boolean;
begin
  Result := False;
end;

procedure TNyxPopoverPresenter.RemoveChild(ALease: TNyxPopoverLease);
var
  LIndex: Integer;
  LMove: Integer;
begin
  for LIndex := 0 to High(FChildren) do
  begin

    if FChildren[LIndex] = ALease then
    begin
      for LMove := LIndex + 1 to High(FChildren) do
      begin
        FChildren[LMove - 1] := FChildren[LMove];
        FChildOwners[LMove - 1] := FChildOwners[LMove];
      end;
      SetLength(FChildren, Length(FChildren) - 1);
      SetLength(FChildOwners, Length(FChildOwners) - 1);
      Exit;
    end;
  end;
end;

procedure TNyxPopoverPresenter.DetachParent;
begin

  if (FParentLease <> nil) and (FParentLease.Owner <> nil) then
  begin
    FParentLease.Owner.RemoveChild(FLease);
  end;
  FParentLease := nil;
  FParentOwner := nil;
end;

function TNyxPopoverPresenter.Within(const AParent: INyxPopover): INyxPopover;
var
  LNesting: INyxPopoverNesting;
  LParent: TNyxPopoverPresenter;
  LAncestor: TNyxPopoverPresenter;
  LDepth: Integer;
  LCount: Integer;
  LChildren: array of TNyxPopoverLease;
  LOwners: array of IInterface;
begin
  FCompletion.Scheduler.RequireUI;

  if FOpen or FTransition or (FLease = nil) or (FLease.Owner = nil) then
  begin
    raise ENyxModel.Create('Nesting requires a closed stable popover');
  end;
  LParent := nil;

  if AParent <> nil then
  begin

    if not Supports(AParent, INyxPopoverNesting, LNesting) or
      (LNesting.NestingLease = nil) or (LNesting.NestingLease.Owner = nil) then
    begin
      raise ENyxModel.Create('Parent popover has no live nesting contract');
    end;
    LParent := LNesting.NestingLease.Owner;
    LAncestor := LParent;
    LDepth := 0;
    while LAncestor <> nil do
    begin
      Inc(LDepth);

      if (LAncestor = Self) or (LDepth > 64) then
      begin
        raise ENyxModel.Create('Popover nesting refuses cycles or more than 64 ancestors');
      end;
      if LAncestor.FParentLease <> nil then
      begin
        LAncestor := LAncestor.FParentLease.Owner;
      end
      else
      begin
        LAncestor := nil;
      end;
    end;

    if not LParent.IsOpen or LParent.FTransition or not CanNestWithin(LParent) then
    begin
      raise ENyxModel.Create('Nested popover anchor requires an open compatible parent');
    end;

    if LParent.FLease = FParentLease then
    begin
      Exit(Self);
    end;

    if Length(LParent.FChildren) >= 256 then
    begin
      raise ENyxModel.Create('A popover supports at most 256 contextual children');
    end;
    { Allocate a detached registration before leaving the accepted parent. Even
      allocation failure must not leave mismatched lease/owner arrays behind. }
    LCount := Length(LParent.FChildren);
    LChildren := Copy(LParent.FChildren, 0, LCount);
    LOwners := Copy(LParent.FChildOwners, 0, LCount);
    SetLength(LChildren, LCount + 1);
    SetLength(LOwners, LCount + 1);
    LChildren[LCount] := FLease;
    LOwners[LCount] := FLeaseOwner;
  end;
  { All relation/physical admission precedes changing either graph. Weak leases
    retain no presenter, so reciprocal graph bookkeeping cannot form a cycle. }
  DetachParent;

  if LParent <> nil then
  begin
    FParentLease := LParent.FLease;
    FParentOwner := FParentLease;
    LParent.FChildren := LChildren;
    LParent.FChildOwners := LOwners;
  end;
  Result := Self;
end;

function TNyxPopoverPresenter.NestedPresenters: TNyxPopoverPresenters;
var
  LIndex: Integer;
  LNested: TNyxPopoverPresenter;
  LCount: Integer;
begin
  Result := nil;
  for LIndex := 0 to High(FChildren) do
  begin
    LNested := FChildren[LIndex].Owner;

    if (LNested <> nil) and (LNested.FParentLease = FLease) and LNested.IsOpen then
    begin
      LCount := Length(Result);
      SetLength(Result, LCount + 1);
      Result[LCount] := LNested;
    end;
  end;
end;

procedure TNyxPopoverPresenter.CloseNested(ANotify: Boolean);
var
  LLeases: array of TNyxPopoverLease;
  LOwners: array of IInterface;
  LIndex: Integer;
  LChild: INyxPopover;
  LFailure: TNyxText;
begin
  { A completion may release/reparent any later member. Detached lease owners
    preserve snapshot slots without extending presenter lifetimes. Recheck every
    live owner/relation immediately before closing it, from newest to oldest. }
  LLeases := Copy(FChildren, 0, Length(FChildren));
  LOwners := Copy(FChildOwners, 0, Length(FChildOwners));

  if Length(LOwners) <> Length(LLeases) then
  begin
    raise ENyxModel.Create('Popover nesting ownership snapshot is inconsistent');
  end;
  LFailure := '';
  for LIndex := High(LLeases) downto 0 do
  begin

    if (LOwners[LIndex] <> nil) and (LLeases[LIndex].Owner <> nil) and
      (LLeases[LIndex].Owner.FParentLease = FLease) then
    begin
      LChild := LLeases[LIndex].Owner;
      try

        if LChild.IsOpen then
        begin
          LLeases[LIndex].Owner.Finish(nprAnchorUnavailable, ANotify);
        end;
      except
        on LException: Exception do
        begin

          if LFailure = '' then
          begin
            LFailure := LException.Message;
          end;
        end;
      end;
      LChild := nil;
    end;
  end;
  if LFailure <> '' then
  begin
    raise ENyxModel.Create('Nested popover closed with a callback failure: ' + LFailure);
  end;
end;

procedure TNyxPopoverPresenter.RetireNesting;
begin
  { Invalidate the complete token ancestry before descendant concealment can
    reenter a scheduler, event receiver or retained parent admission provider. }
  RevokeOpenLifetime;

  if (FLease = nil) or (FLease.Owner = nil) then
  begin
    Exit;
  end;
  FOpen := False;
  FTransition := True;
  FLease.Owner := nil;
  try
    CloseNested(False);
  finally
    DetachParent;
    FChildren := nil;
    FChildOwners := nil;
    FLease := nil;
    FLeaseOwner := nil;
  end;
end;

procedure TNyxPopoverPresenter.PrepareContent;
var
  LPrototype: TNyxNode;
begin
  FDocument.Validate;
  LPrototype := RealizeNyxView(FDocument, FContent.Node);
  try
    FCollections := NewNyxCollectionBindings(LPrototype,
      NewNyxCollectionContext(FDocument.Collections, LPrototype.ResourceContext));
  finally
    LPrototype.Free;
  end;
end;

function TNyxPopoverPresenter.GetContent: INyxControl;
begin
  Result := FContent;
end;

function TNyxPopoverPresenter.GetState: TNyxState;
begin
  Result := FState;
end;

function TNyxPopoverPresenter.GetOpen: Boolean;
begin
  Result := FOpen;
end;

function TNyxPopoverPresenter.GetOpenLifetime: INyxPopoverOpenLifetime;
begin
  Result := nil;

  if FOpen and not FTransition and (FLease <> nil) and
    (FLease.Owner <> nil) then
  begin
    Result := FOpenLifetime;

    if (Result <> nil) and not Result.Current then
    begin
      Result := nil;
    end;
  end;
end;

function TNyxPopoverPresenter.GetLastReason: TNyxPopoverReason;
begin
  Result := FLastReason;
end;

function TNyxPopoverPresenter.OnDismiss: INyxEventStream;
begin
  Result := FCompletion.OnNamed(NyxCompoundEvents(FContent.ID), NyxSemantic(nseDismiss));
end;

procedure TNyxPopoverPresenter.Open(const AOptions: TNyxPopoverOptions);
var
  LKeepAlive: INyxPopover;
  LFocusID: TNyxText;
  LParent: TNyxPopoverLease;
  LParentOwner: IInterface;
  LParentOpen: INyxPopoverOpenLifetime;
  LToken: TPopoverOpenToken;
  LTokenOwner: INyxPopoverOpenLifetime;
  LPresented: Boolean;

  function ContextCurrent: Boolean;
  begin
    Result := (FLease <> nil) and (FLease.Owner = Self) and
      (FParentLease = LParent);

    if Result and (LParent <> nil) then
    begin
      Result := (LParent.Owner <> nil) and (LParentOpen <> nil) and
        LParentOpen.Current;
    end;
  end;
begin
  LKeepAlive := Self;
  FCompletion.Scheduler.RequireUI;

  if FOpen or FTransition or (FLease = nil) or (FLease.Owner = nil) then
  begin
    raise ENyxModel.Create('Popover is open or changing focus');
  end;
  AOptions.Validate;
  LParent := FParentLease;
  LParentOwner := FParentOwner;
  LParentOpen := nil;

  if (LParent <> nil) and (LParent.Owner <> nil) then
  begin
    { The nonvirtual base getter captures canonical successful-open identity.
      Retain only the parent's inert nesting lease and owner-free token. }
    LParentOpen := LParent.Owner.GetOpenLifetime;
  end;

  if not ContextCurrent then
  begin
    raise ENyxModel.Create('Nested popover parent is closed or retired');
  end;
  LFocusID := '';

  if AOptions.InitialFocus.Name <> '' then
  begin
    LFocusID := FContent.Part(AOptions.InitialFocus).ID;
  end;

  LToken := TPopoverOpenToken.Create(LParentOpen);
  LTokenOwner := LToken;
  FOpenLifetime := LTokenOwner;
  FOpenObject := LToken;
  LPresented := False;
  FTransition := True;
  try
    try

      if not FMounted then
      begin
        PrepareContent;
      end;

      if not ContextCurrent then
      begin
        raise ENyxModel.Create('Popover context changed while preparing');
      end;
      LPresented := True;
      Present(AOptions, LFocusID);

      if not ContextCurrent then
      begin
        raise ENyxModel.Create('Popover context changed while presenting');
      end;
      FMounted := True;
      FLastReason := nprNone;
      LToken.Activate;
      FOpen := True;
    except
      LToken.Revoke;
      RevokeOpenLifetime;

      if LPresented then
      begin
        try
          Conceal(False);
        except
          { A partial physical host still receives cleanup, but secondary
            cleanup failure cannot replace the original presentation error. }
        end;
      end;
      raise;
    end;
  finally
    { A target callback may have retired this still-retained presenter. Keep
      retirement terminal; an Open tail must never clear that transition fence. }

    if (FLease <> nil) and (FLease.Owner <> nil) then
    begin
      FTransition := False;
    end;
  end;
  LKeepAlive.GetOpen;
end;

procedure TNyxPopoverPresenter.Finish(AReason: TNyxPopoverReason; ANotify: Boolean);
var
  LKeepAlive: INyxPopover;
  LEvents: INyxEvents;
  LEvent: TNyxEventInfo;
begin
  LKeepAlive := Self;
  FCompletion.Scheduler.RequireUI;

  if FTransition then
  begin
    raise ENyxModel.Create('Popover is changing focus');
  end;

  if not FOpen then
  begin
    Exit;
  end;
  LEvents := FCompletion;
  LEvent := Default(TNyxEventInfo);
  LEvent.Value := NyxNull;
  LEvent.Details := NyxNull;
  LEvent.HasDetails := True;
  LEvent.Details := NyxObject([NyxField(CReasonField, NyxData(Ord(AReason)))]);
  LEvent.Trigger := ntNamed;
  LEvent.Name := NyxSemantic(nseDismiss);
  LEvent.SourceID := FContent.ID;
  LEvent.OriginID := FContent.ID;
  LEvent.TargetID := FContent.ID;
  RevokeOpenLifetime;
  FOpen := False;
  FLastReason := AReason;
  FTransition := True;
  try
    { An outside press must reach its original destination without focus theft.
      Anchor retirement cannot restore an unavailable invoker. }
    try
      CloseNested;
    finally
      Conceal(not (AReason in [nprOutsidePress, nprAnchorUnavailable]));
    end;
  finally

    if (FLease <> nil) and (FLease.Owner <> nil) then
    begin
      FTransition := False;
    end;
  end;

  if ANotify then
  begin
    LEvents.Dispatch(LEvent, LEvent.OriginID, LEvent.SourceID);
  end;
  LKeepAlive.GetOpen;
end;

procedure TNyxPopoverPresenter.Close;
begin
  Finish(nprClose, False);
end;

procedure TNyxPopoverPresenter.Dismiss(AReason: TNyxPopoverReason);
begin

  if not (AReason in [nprEscape, nprOutsidePress, nprAnchorUnavailable, nprAction]) then
  begin
    raise ENyxModel.Create('Invalid popover dismissal reason');
  end;
  Finish(AReason, True);
end;

end.
