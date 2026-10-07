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
  nyx.text, nyx.types, nyx.root.types, nyx.model, nyx.controls,
  nyx.state, nyx.behavior, nyx.events, nyx.collections.view, nyx.collections.bindings;

type
  TNyxPopoverSide = (npsBelow, npsAbove, npsRight, npsLeft);
  TNyxPopoverAlignment = (npaStart, npaCenter, npaEnd);
  TNyxPopoverSizing = (npzContent, npzFixed);
  TNyxPopoverDismissal = (npdEscape, npdOutsidePress);
  TNyxPopoverDismissals = set of TNyxPopoverDismissal;
  { Anchor retirement always closes: keeping an orphaned presentation is unsafe.
    Programmatic Close is silent; Dismiss publishes an ordered completion. }
  TNyxPopoverReason = (nprNone, nprClose, nprEscape, nprOutsidePress,
    nprAnchorUnavailable, nprAction);

  { Adapter-independent rectangle in logical pixels. Coordinates are bounded
    to +/-1,000,000 and extents to 0..1,000,000; checked 32-bit arithmetic stays
    exact on both supported compilers. Rectangles are detached record values. }
  TNyxPopoverRect = record
    Left: Integer;
    Top: Integer;
    Width: Integer;
    Height: Integer;
    procedure Validate;
  end;

  { Immutable fluent geometry, focus and dismissal policy. Width/height are
    allocation requests, capped by the available viewport with a safety margin.
    InitialFocus is optional. Without it the adapter preserves invoker focus.
    Escape/outside press are enabled by default; anchor retirement is mandatory. }
  TNyxPopoverOptions = record
  private
    FTitle: TNyxText;
    FSide: TNyxPopoverSide;
    FAlignment: TNyxPopoverAlignment;
    FSizing: TNyxPopoverSizing;
    FWidth: Integer;
    FHeight: Integer;
    FGap: Integer;
    FMargin: Integer;
    FFocus: TNyxPartRef;
    FDismissals: TNyxPopoverDismissals;
  public
    function Placement(ASide: TNyxPopoverSide;
      AAlignment: TNyxPopoverAlignment = npaStart): TNyxPopoverOptions;
    function Size(AWidth, AHeight: Integer): TNyxPopoverOptions;
    { Content is the default: height follows the mounted Nyx view up to the Size
      cap. Fixed allocates the requested height, also capped by the viewport. }
    function Sizing(AValue: TNyxPopoverSizing): TNyxPopoverOptions;
    function Spacing(AGap, AMargin: Integer): TNyxPopoverOptions;
    function Focus(const APart: TNyxPartRef): TNyxPopoverOptions;
    function DismissOn(AValues: TNyxPopoverDismissals): TNyxPopoverOptions;
    procedure Validate;
    property Title: TNyxText read FTitle;
    property Side: TNyxPopoverSide read FSide;
    property Alignment: TNyxPopoverAlignment read FAlignment;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property SizeMode: TNyxPopoverSizing read FSizing;
    property Gap: Integer read FGap;
    property Margin: Integer read FMargin;
    property InitialFocus: TNyxPartRef read FFocus;
    property Dismissals: TNyxPopoverDismissals read FDismissals;
  end;

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
  Math, nyx.composition, nyx.data;

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

procedure TNyxPopoverRect.Validate;
begin

  if (Left < -1000000) or (Left > 1000000) or
    (Top < -1000000) or (Top > 1000000) or
    (Width < 0) or (Width > 1000000) or
    (Height < 0) or (Height > 1000000) or
    (Left + Width > 1000000) or (Top + Height > 1000000) then
  begin
    raise ENyxModel.Create('Popover rectangle is outside logical pixel bounds');
  end;
end;

function NyxPopoverRect(ALeft, ATop, AWidth, AHeight: Integer): TNyxPopoverRect;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Width := AWidth;
  Result.Height := AHeight;
  Result.Validate;
end;

function NyxPopover(const ATitle: TNyxText): TNyxPopoverOptions;
begin
  Result := Default(TNyxPopoverOptions);
  Result.FTitle := ATitle;
  Result.FWidth := 360;
  Result.FHeight := 240;
  Result.FGap := 8;
  Result.FMargin := 12;
  Result.FDismissals := [npdEscape, npdOutsidePress];
end;

procedure TNyxPopoverOptions.Validate;
begin

  if (FSide < Low(TNyxPopoverSide)) or (FSide > High(TNyxPopoverSide)) or
    (FAlignment < Low(TNyxPopoverAlignment)) or
    (FAlignment > High(TNyxPopoverAlignment)) or
    (FSizing < Low(TNyxPopoverSizing)) or (FSizing > High(TNyxPopoverSizing)) or
    (FWidth < 16) or (FWidth > 16384) or
    (FHeight < 16) or (FHeight > 16384) or
    (FGap < 0) or (FGap > 4096) or (FMargin < 0) or (FMargin > 4096) then
  begin
    raise ENyxModel.Create('Invalid popover geometry or placement policy');
  end;
end;

function TNyxPopoverOptions.Placement(ASide: TNyxPopoverSide;
  AAlignment: TNyxPopoverAlignment): TNyxPopoverOptions;
begin
  Result := Self;
  Result.FSide := ASide;
  Result.FAlignment := AAlignment;
  Result.Validate;
end;

function TNyxPopoverOptions.Size(AWidth, AHeight: Integer): TNyxPopoverOptions;
begin
  Result := Self;
  Result.FWidth := AWidth;
  Result.FHeight := AHeight;
  Result.Validate;
end;

function TNyxPopoverOptions.Spacing(AGap, AMargin: Integer): TNyxPopoverOptions;
begin
  Result := Self;
  Result.FGap := AGap;
  Result.FMargin := AMargin;
  Result.Validate;
end;

function TNyxPopoverOptions.Sizing(AValue: TNyxPopoverSizing): TNyxPopoverOptions;
begin
  Result := Self;
  Result.FSizing := AValue;
  Result.Validate;
end;

function TNyxPopoverOptions.Focus(const APart: TNyxPartRef): TNyxPopoverOptions;
begin
  Result := Self;
  Result.FFocus := APart;
end;

function TNyxPopoverOptions.DismissOn(AValues: TNyxPopoverDismissals): TNyxPopoverOptions;
begin
  Result := Self;
  Result.FDismissals := AValues;
end;

function PlaceNyxPopover(const AAnchor, AViewport: TNyxPopoverRect;
  const AOptions: TNyxPopoverOptions): TNyxPopoverRect;
var
  LMargin: Integer;
  LLeft: Integer;
  LTop: Integer;
  LRight: Integer;
  LBottom: Integer;
  LBefore: Integer;
  LAfter: Integer;
  LSide: TNyxPopoverSide;
begin
  AAnchor.Validate;
  AViewport.Validate;
  AOptions.Validate;

  if (AViewport.Width = 0) or (AViewport.Height = 0) then
  begin
    raise ENyxModel.Create('Popover requires a nonempty viewport');
  end;
  LMargin := Min(AOptions.Margin, (Min(AViewport.Width, AViewport.Height) - 1) div 2);
  LLeft := AViewport.Left + LMargin;
  LTop := AViewport.Top + LMargin;
  LRight := AViewport.Left + AViewport.Width - LMargin;
  LBottom := AViewport.Top + AViewport.Height - LMargin;
  Result.Width := Min(AOptions.Width, LRight - LLeft);
  Result.Height := Min(AOptions.Height, LBottom - LTop);
  LSide := AOptions.Side;

  if LSide in [npsBelow, npsAbove] then
  begin
    LBefore := AAnchor.Top - AOptions.Gap - LTop;
    LAfter := LBottom - AAnchor.Top - AAnchor.Height - AOptions.Gap;

    if (LSide = npsBelow) and (LAfter < Result.Height) and (LBefore > LAfter) then
    begin
      LSide := npsAbove;
    end
    else if (LSide = npsAbove) and (LBefore < Result.Height) and (LAfter > LBefore) then
    begin
      LSide := npsBelow;
    end;
    Result.Left := AAnchor.Left;
    case AOptions.Alignment of
      npaStart:
        begin
          { Start is already anchored. }
        end;
      npaCenter:
        begin
          Result.Left := AAnchor.Left + (AAnchor.Width - Result.Width) div 2;
        end;
      npaEnd:
        begin
          Result.Left := AAnchor.Left + AAnchor.Width - Result.Width;
        end;
    end;

    if LSide = npsBelow then
    begin
      Result.Top := AAnchor.Top + AAnchor.Height + AOptions.Gap;
    end
    else
    begin
      Result.Top := AAnchor.Top - AOptions.Gap - Result.Height;
    end;
  end
  else
  begin
    LBefore := AAnchor.Left - AOptions.Gap - LLeft;
    LAfter := LRight - AAnchor.Left - AAnchor.Width - AOptions.Gap;

    if (LSide = npsRight) and (LAfter < Result.Width) and (LBefore > LAfter) then
    begin
      LSide := npsLeft;
    end
    else if (LSide = npsLeft) and (LBefore < Result.Width) and (LAfter > LBefore) then
    begin
      LSide := npsRight;
    end;
    Result.Top := AAnchor.Top;
    case AOptions.Alignment of
      npaStart:
        begin
          { Start is already anchored. }
        end;
      npaCenter:
        begin
          Result.Top := AAnchor.Top + (AAnchor.Height - Result.Height) div 2;
        end;
      npaEnd:
        begin
          Result.Top := AAnchor.Top + AAnchor.Height - Result.Height;
        end;
    end;

    if LSide = npsRight then
    begin
      Result.Left := AAnchor.Left + AAnchor.Width + AOptions.Gap;
    end
    else
    begin
      Result.Left := AAnchor.Left - AOptions.Gap - Result.Width;
    end;
  end;
  Result.Left := Max(LLeft, Min(Result.Left, LRight - Result.Width));
  Result.Top := Max(LTop, Min(Result.Top, LBottom - Result.Height));
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
