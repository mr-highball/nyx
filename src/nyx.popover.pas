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
  nyx.state, nyx.behavior, nyx.events, nyx.collections.view, nyx.collections.bindings;

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
    policy or inferred callback name. Close is silent and idempotent.
    All presentation methods require the UI thread. Do not register a callback
    that strongly retains its own presenter, creating an application cycle. }
  INyxPopover = interface(IInterface)
    ['{B2672EF9-496D-4EC9-8006-061026000001}']
    function GetContent: INyxControl;
    function GetEvents: INyxEvents;
    function GetState: TNyxState;
    function GetOpen: Boolean;
    function GetLastReason: TNyxPopoverReason;
    function OnDismiss: INyxEventStream;
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

  { Target seam. Subclasses borrow no original document or content, disconnect
    input/timers and release renderers before inherited destruction. }
  TNyxPopoverPresenter = class(TInterfacedObject, INyxPopover)
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
  protected
    procedure PrepareContent;
    procedure Present(const AOptions: TNyxPopoverOptions;
      const AFocusID: TNyxText); virtual; abstract;
    procedure Conceal(ARestoreFocus: Boolean); virtual; abstract;
    procedure Finish(AReason: TNyxPopoverReason; ANotify: Boolean);
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
    function GetLastReason: TNyxPopoverReason;
    function OnDismiss: INyxEventStream;
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
  nyx.composition, nyx.data;

const
  CReasonField: TNyxText = 'popover-reason';

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
  FContent := RetainNyxControl(FDocument.FindRoot(ARoot));
  FState := FDocument.State.Clone;
  FCompletion := NewNyxEvents;
end;

destructor TNyxPopoverPresenter.Destroy;
begin

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

procedure TNyxPopoverPresenter.PrepareContent;
var
  LPrototype: TNyxNode;
begin
  FDocument.Validate;
  LPrototype := RealizeNyxView(FDocument, FContent.Node);
  try
    FCollections := NewNyxCollectionBindings(LPrototype,
      NewNyxCollectionContext(FDocument.Collections));
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
begin
  LKeepAlive := Self;
  FCompletion.Scheduler.RequireUI;

  if FOpen or FTransition then
  begin
    raise ENyxModel.Create('Popover is open or changing focus');
  end;
  AOptions.Validate;
  LFocusID := '';

  if AOptions.InitialFocus.Name <> '' then
  begin
    LFocusID := FContent.Part(AOptions.InitialFocus).ID;
  end;

  if not FMounted then
  begin
    PrepareContent;
  end;
  FTransition := True;
  try
    Present(AOptions, LFocusID);
    FMounted := True;
    FLastReason := nprNone;
    FOpen := True;
  finally
    FTransition := False;
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
  FOpen := False;
  FLastReason := AReason;
  FTransition := True;
  try
    { An outside press must reach its original destination without focus theft.
      Anchor retirement cannot restore an unavailable invoker. }
    Conceal(not (AReason in [nprOutsidePress, nprAnchorUnavailable]));
  finally
    FTransition := False;
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
