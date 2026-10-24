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

{ Lifetime decoration for an arranged table's existing query router.
  Streams, dispatch, policies and scheduling remain owned by the actual router.
  Captured cancellation gains the mount's independent input epoch and, for a
  form, its actual successful-open lifetime. }
unit nyx.collections.arrangement.workbench;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.events, nyx.collections.mount, nyx.popover;

{ Private adapter for table query work. Pass the exact readonly lifetime supplied
  by the mount, whose getters are callback-free and native-worker-safe. Creation
  does not capture a generation or require installed input, so staged renderers
  can construct a hidden workbench. GetGeneration later requires actual Ready.
  The original generation must expose flags-only cancellation readiness; generic
  external Cancelled getters remain outside final publication admission.

  This is a query-specific decorator, not a replacement application router. It
  forwards INyxEvents and preserves optional ordinary observation when present.
  Prepared view/observation editing belongs to the original renderer/router and
  is deliberately not exposed through this private query surface. Form events
  use the overload taking their actual popover admission provider: each captured
  generation then belongs to one successful presentation, including silent Close.
  The provider is retained by the forwarding surface, never by a generation. }
function NewNyxArrangementQueryEvents(const AEvents: INyxEvents;
  const AInput: INyxCollectionInputLifetime): INyxEvents; overload;
{ A form keeps its original router and target presenter facets. This decorator
  changes only generation capture: a closed, transitioning or retired popover
  refuses capture, and close/reopen never revives an earlier generation. Pass
  the real presenter's optional admission facet, not a wrapper's IsOpen flag.
  Retained tokens contain no presenter/widget reference and permit worker reads.
  A nil provider refuses here rather than silently creating an unguarded form. }
function NewNyxArrangementQueryEvents(const AEvents: INyxEvents;
  const AInput: INyxCollectionInputLifetime;
  const AOpenAdmission: INyxPopoverOpenAdmission): INyxEvents; overload;

implementation

uses
  SysUtils, nyx.text, nyx.types, nyx.behavior, nyx.gestures, nyx.scheduler;

type
  { A generation retains its original cancellation scope and independent readonly
    input facts and optional original open token only. It owns no router, mount,
    view, model, presenter or widget.
    Its own atomic cancellation is terminal; withdrawing this captured authority
    never calls Cancel on the original scope or changes router registrations. }
  TArrangementQueryGeneration = class(TInterfacedObject, INyxCancellationScope,
    INyxCancellationReadiness)
  private
    FInner: INyxCancellationScope;
    FInnerReady: INyxCancellationReadiness;
    FInput: INyxCollectionInputLifetime;
    FOpen: INyxPopoverOpenLifetime;
    FEpoch: Integer;
    FCancelled: LongInt;
  public
    constructor Create(const AInner: INyxCancellationScope;
      const AInput: INyxCollectionInputLifetime; AEpoch: Integer;
      const AOpen: INyxPopoverOpenLifetime;
      const AInnerReady: INyxCancellationReadiness);
    procedure Cancel;
    function GetCancelled: Boolean;
    procedure CheckReady;
  end;

  { Explicit forwarding keeps INyxEvents and INyxEventGeneration on this same
    interface owner on FPC and pas2js. Delegating INyxEvents through implements
    would need a separate proof that QI cannot return the undecorated router.
    Each forward retains the actual router across callbacks, then touches no
    wrapper state. No secondary subscription, dispatch or scheduler is created. }
  TArrangementQueryEvents = class(TInterfacedObject, INyxEvents,
    INyxEventGeneration)
  protected
    FInner: INyxEvents;
    FInput: INyxCollectionInputLifetime;
    FOpenAdmission: INyxPopoverOpenAdmission;
  public
    constructor Create(const AInner: INyxEvents;
      const AInput: INyxCollectionInputLifetime;
      const AOpenAdmission: INyxPopoverOpenAdmission);
    function GetGeneration: INyxCancellationScope;
    function On(const ATarget: TNyxEventTarget;
      ATrigger: TNyxTrigger): INyxEventStream;
    function OnNamed(const ATarget: TNyxEventTarget;
      const AName: TNyxEventRef): INyxEventStream;
    function OnAfterEnter(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnAfterExit(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnKeyDown(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnKeyUp(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnBeforeKeyDown(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnAfterKeyDown(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnKeyPress(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnBeforeKeyPress(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnAfterKeyPress(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnBeforeKeyUp(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnAfterKeyUp(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnBeforeTextInput(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnTextInput(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnAfterTextInput(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnBeforeEdit(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnCompositionStart(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnCompositionUpdate(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnCompositionEnd(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnTextSelectionChange(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnPointerCancel(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnPointerCapture(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnPointerCaptureLost(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnDragStart(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnDrag(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnDragEnter(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnDragOver(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnDragExit(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnDrop(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnDragEnd(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnDoubleClick(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnPointerDown(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnPointerUp(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnPointerMove(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnPointerEnter(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnPointerExit(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnContextMenu(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnBeforeWheel(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnWheel(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnAfterWheel(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnScroll(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnScrollEnd(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnSelectionChange(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnImageLoading(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnImageReady(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnImageError(const ATarget: TNyxEventTarget): INyxEventStream;
    function OnImageCleared(const ATarget: TNyxEventTarget): INyxEventStream;
    function HasSubscribers(ATrigger: TNyxTrigger): Boolean;
    function Dispatch(const AEvent: TNyxEventInfo;
      const AOriginDesignID, ASourceDesignID: TNyxText): TNyxExecutions; reintroduce;
    function DispatchGuarded(const AEvent: TNyxEventInfo;
      const AOriginDesignID, ASourceDesignID: TNyxText;
      const ADelivery: INyxCancellationScope): TNyxExecutions;
    function DispatchInput(const AEvent: TNyxEventInfo;
      const AOriginDesignID, ASourceDesignID: TNyxText;
      out AConsumed: Boolean): TNyxExecutions;
    function DispatchGesture(const AEvent: TNyxEventInfo;
      const AOriginDesignID, ASourceDesignID: TNyxText;
      const ADecision: INyxGestureDecision): TNyxExecutions;
    procedure CancelPending;
    procedure Close;
    function GetScheduler: INyxScheduler;
    function GetViewRevision: Integer;
  end;

  { Advertise ordinary observation only when the real router supplies it.
    Returned subscriptions and their policies retain their original identities. }
  TObservedArrangementQueryEvents = class(TArrangementQueryEvents,
    INyxEventObservation)
  private
    FObservation: INyxEventObservation;
  public
    constructor Create(const AInner: INyxEvents;
      const AInput: INyxCollectionInputLifetime;
      const AObservation: INyxEventObservation;
      const AOpenAdmission: INyxPopoverOpenAdmission);
    function Observe(const ATarget: TNyxEventTarget; ATrigger: TNyxTrigger;
      const AObserver: INyxEventObserver; APhase: TNyxObservationPhase;
      const AName: TNyxEventRef): INyxEventSubscription;
  end;

function ReadQueryCancelled(var AFlag: LongInt): Boolean;
begin
  {$ifdef PAS2JS}
  Result := AFlag <> 0;
  {$else}
  Result := InterlockedCompareExchange(AFlag, 0, 0) <> 0;
  {$endif}
end;

constructor TArrangementQueryGeneration.Create(
  const AInner: INyxCancellationScope;
  const AInput: INyxCollectionInputLifetime; AEpoch: Integer;
  const AOpen: INyxPopoverOpenLifetime;
  const AInnerReady: INyxCancellationReadiness);
begin
  inherited Create;
  FInner := AInner;
  FInnerReady := AInnerReady;
  FInput := AInput;
  FOpen := AOpen;
  FEpoch := AEpoch;
end;

procedure TArrangementQueryGeneration.Cancel;
begin
  {$ifdef PAS2JS}
  FCancelled := 1;
  {$else}
  InterlockedExchange(FCancelled, 1);
  {$endif}
end;

function TArrangementQueryGeneration.GetCancelled: Boolean;
var
  LKeepAlive: INyxCancellationScope;
  LInner: INyxCancellationScope;
  LInput: INyxCollectionInputLifetime;
  LOpen: INyxPopoverOpenLifetime;
  LCancelled: Boolean;
begin

  if ReadQueryCancelled(FCancelled) then
  begin
    Exit(True);
  end;
  LKeepAlive := Self;
  LInner := FInner;
  LInput := FInput;
  LOpen := FOpen;
  try
    { A custom scope can synchronously suspend/resume its table or close/reopen
      its form while answering. Inspect the original open token AFTER this
      call, then finish with the mount's callback-free canonical input facts. }
    LCancelled := LInner.Cancelled;

    if (LOpen <> nil) and not LOpen.Current then
    begin
      LCancelled := True;
    end;
  except
    Cancel;
    raise;
  end;

  if LCancelled or ReadQueryCancelled(FCancelled) or not LInput.Ready or
    (LInput.Epoch <> FEpoch) or (FEpoch = High(Integer)) then
  begin
    Cancel;
  end;
  Result := ReadQueryCancelled(FCancelled);
end;

procedure TArrangementQueryGeneration.CheckReady;
begin
  try
    { This is the captured explicit flags-only contract, never the arbitrary
      original Cancelled getter. It may only inspect retained cancellation facts. }

    if FInnerReady = nil then
    begin
      raise ENyxSchedule.Create('Query generation has no final readiness contract');
    end;
    FInnerReady.CheckReady;

    if (FOpen <> nil) and not FOpen.Current then
    begin
      Cancel;
    end;

    if not FInput.Ready or (FEpoch < 1) or (FEpoch = High(Integer)) or
      (FInput.Epoch <> FEpoch) then
    begin
      Cancel;
    end;

    if ReadQueryCancelled(FCancelled) then
    begin
      raise ENyxSchedule.Create('Query input or presentation is no longer ready');
    end;
  except
    Cancel;
    raise;
  end;
end;

constructor TArrangementQueryEvents.Create(const AInner: INyxEvents;
  const AInput: INyxCollectionInputLifetime;
  const AOpenAdmission: INyxPopoverOpenAdmission);
begin
  inherited Create;
  FInner := AInner;
  FInput := AInput;
  FOpenAdmission := AOpenAdmission;
end;

function TArrangementQueryEvents.GetGeneration: INyxCancellationScope;
var
  LKeepAlive: INyxEvents;
  LInner: INyxEvents;
  LInput: INyxCollectionInputLifetime;
  LScope: INyxCancellationScope;
  LAdmission: INyxPopoverOpenAdmission;
  LOpen: INyxPopoverOpenLifetime;
  LReady: INyxCancellationReadiness;
  LEpoch: Integer;
begin
  LKeepAlive := Self;
  LInner := FInner;
  LInput := FInput;
  LAdmission := FOpenAdmission;
  LEpoch := LInput.Epoch;

  if (LEpoch < 1) or (LEpoch = High(Integer)) or not LInput.Ready then
  begin
    raise ENyxSchedule.Create('Table input is not ready for query capture');
  end;
  LOpen := nil;

  if LAdmission <> nil then
  begin
    LOpen := LAdmission.GetOpenLifetime;

    if LOpen = nil then
    begin
      raise ENyxSchedule.Create('Query form is not open for capture');
    end;
  end;
  { Capture the ACTUAL router generation. Never substitute an unbound scope or
    recapture against a later input epoch after the getter has changed ownership. }
  LScope := NyxEventGeneration(LInner);

  if (LScope = nil) or not Supports(LScope, INyxCancellationReadiness, LReady) then
  begin
    raise ENyxSchedule.Create('Arranged queries require final generation readiness');
  end;

  if (LOpen <> nil) and not LOpen.Current then
  begin
    raise ENyxSchedule.Create('Query form changed during capture');
  end;

  if (LScope = nil) or not LInput.Ready or (LInput.Epoch <> LEpoch) then
  begin
    raise ENyxSchedule.Create('Table input changed during query capture');
  end;
  Result := TArrangementQueryGeneration.Create(LScope, LInput, LEpoch, LOpen,
    LReady);
end;

function TArrangementQueryEvents.On(const ATarget: TNyxEventTarget;
  ATrigger: TNyxTrigger): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.On(ATarget, ATrigger);
end;

function TArrangementQueryEvents.OnNamed(const ATarget: TNyxEventTarget;
  const AName: TNyxEventRef): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnNamed(ATarget, AName);
end;

function TArrangementQueryEvents.OnAfterEnter(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnAfterEnter(ATarget);
end;

function TArrangementQueryEvents.OnAfterExit(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnAfterExit(ATarget);
end;

function TArrangementQueryEvents.OnKeyDown(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnKeyDown(ATarget);
end;

function TArrangementQueryEvents.OnKeyUp(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnKeyUp(ATarget);
end;

function TArrangementQueryEvents.OnBeforeKeyDown(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnBeforeKeyDown(ATarget);
end;

function TArrangementQueryEvents.OnAfterKeyDown(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnAfterKeyDown(ATarget);
end;

function TArrangementQueryEvents.OnKeyPress(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnKeyPress(ATarget);
end;

function TArrangementQueryEvents.OnBeforeKeyPress(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnBeforeKeyPress(ATarget);
end;

function TArrangementQueryEvents.OnAfterKeyPress(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnAfterKeyPress(ATarget);
end;

function TArrangementQueryEvents.OnBeforeKeyUp(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnBeforeKeyUp(ATarget);
end;

function TArrangementQueryEvents.OnAfterKeyUp(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnAfterKeyUp(ATarget);
end;

function TArrangementQueryEvents.OnBeforeTextInput(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnBeforeTextInput(ATarget);
end;

function TArrangementQueryEvents.OnTextInput(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnTextInput(ATarget);
end;

function TArrangementQueryEvents.OnAfterTextInput(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnAfterTextInput(ATarget);
end;

function TArrangementQueryEvents.OnBeforeEdit(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnBeforeEdit(ATarget);
end;

function TArrangementQueryEvents.OnCompositionStart(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnCompositionStart(ATarget);
end;

function TArrangementQueryEvents.OnCompositionUpdate(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnCompositionUpdate(ATarget);
end;

function TArrangementQueryEvents.OnCompositionEnd(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnCompositionEnd(ATarget);
end;

function TArrangementQueryEvents.OnTextSelectionChange(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnTextSelectionChange(ATarget);
end;

function TArrangementQueryEvents.OnPointerCancel(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnPointerCancel(ATarget);
end;

function TArrangementQueryEvents.OnPointerCapture(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnPointerCapture(ATarget);
end;

function TArrangementQueryEvents.OnPointerCaptureLost(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnPointerCaptureLost(ATarget);
end;

function TArrangementQueryEvents.OnDragStart(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnDragStart(ATarget);
end;

function TArrangementQueryEvents.OnDrag(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnDrag(ATarget);
end;

function TArrangementQueryEvents.OnDragEnter(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnDragEnter(ATarget);
end;

function TArrangementQueryEvents.OnDragOver(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnDragOver(ATarget);
end;

function TArrangementQueryEvents.OnDragExit(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnDragExit(ATarget);
end;

function TArrangementQueryEvents.OnDrop(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnDrop(ATarget);
end;

function TArrangementQueryEvents.OnDragEnd(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnDragEnd(ATarget);
end;

function TArrangementQueryEvents.OnDoubleClick(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnDoubleClick(ATarget);
end;

function TArrangementQueryEvents.OnPointerDown(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnPointerDown(ATarget);
end;

function TArrangementQueryEvents.OnPointerUp(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnPointerUp(ATarget);
end;

function TArrangementQueryEvents.OnPointerMove(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnPointerMove(ATarget);
end;

function TArrangementQueryEvents.OnPointerEnter(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnPointerEnter(ATarget);
end;

function TArrangementQueryEvents.OnPointerExit(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnPointerExit(ATarget);
end;

function TArrangementQueryEvents.OnContextMenu(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnContextMenu(ATarget);
end;

function TArrangementQueryEvents.OnBeforeWheel(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnBeforeWheel(ATarget);
end;

function TArrangementQueryEvents.OnWheel(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnWheel(ATarget);
end;

function TArrangementQueryEvents.OnAfterWheel(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnAfterWheel(ATarget);
end;

function TArrangementQueryEvents.OnScroll(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnScroll(ATarget);
end;

function TArrangementQueryEvents.OnScrollEnd(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnScrollEnd(ATarget);
end;

function TArrangementQueryEvents.OnSelectionChange(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnSelectionChange(ATarget);
end;

function TArrangementQueryEvents.OnImageLoading(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnImageLoading(ATarget);
end;

function TArrangementQueryEvents.OnImageReady(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnImageReady(ATarget);
end;

function TArrangementQueryEvents.OnImageError(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnImageError(ATarget);
end;

function TArrangementQueryEvents.OnImageCleared(
  const ATarget: TNyxEventTarget): INyxEventStream;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.OnImageCleared(ATarget);
end;

function TArrangementQueryEvents.HasSubscribers(ATrigger: TNyxTrigger): Boolean;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.HasSubscribers(ATrigger);
end;

function TArrangementQueryEvents.Dispatch(const AEvent: TNyxEventInfo;
  const AOriginDesignID, ASourceDesignID: TNyxText): TNyxExecutions;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.Dispatch(AEvent, AOriginDesignID, ASourceDesignID);
end;

function TArrangementQueryEvents.DispatchGuarded(const AEvent: TNyxEventInfo;
  const AOriginDesignID, ASourceDesignID: TNyxText;
  const ADelivery: INyxCancellationScope): TNyxExecutions;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.DispatchGuarded(AEvent, AOriginDesignID, ASourceDesignID, ADelivery);
end;

function TArrangementQueryEvents.DispatchInput(const AEvent: TNyxEventInfo;
  const AOriginDesignID, ASourceDesignID: TNyxText;
  out AConsumed: Boolean): TNyxExecutions;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.DispatchInput(AEvent, AOriginDesignID, ASourceDesignID, AConsumed);
end;

function TArrangementQueryEvents.DispatchGesture(const AEvent: TNyxEventInfo;
  const AOriginDesignID, ASourceDesignID: TNyxText;
  const ADecision: INyxGestureDecision): TNyxExecutions;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.DispatchGesture(AEvent, AOriginDesignID, ASourceDesignID, ADecision);
end;

procedure TArrangementQueryEvents.CancelPending;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  LInner.CancelPending;
end;

procedure TArrangementQueryEvents.Close;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  LInner.Close;
end;

function TArrangementQueryEvents.GetScheduler: INyxScheduler;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.Scheduler;
end;

function TArrangementQueryEvents.GetViewRevision: Integer;
var
  LInner: INyxEvents;
begin
  LInner := FInner;
  Result := LInner.ViewRevision;
end;

constructor TObservedArrangementQueryEvents.Create(const AInner: INyxEvents;
  const AInput: INyxCollectionInputLifetime;
  const AObservation: INyxEventObservation;
  const AOpenAdmission: INyxPopoverOpenAdmission);
begin
  inherited Create(AInner, AInput, AOpenAdmission);
  FObservation := AObservation;
end;

function TObservedArrangementQueryEvents.Observe(const ATarget: TNyxEventTarget;
  ATrigger: TNyxTrigger; const AObserver: INyxEventObserver;
  APhase: TNyxObservationPhase; const AName: TNyxEventRef): INyxEventSubscription;
var
  LObservation: INyxEventObservation;
begin
  LObservation := FObservation;
  Result := LObservation.Observe(ATarget, ATrigger, AObserver, APhase, AName);
end;

function CreateArrangementQueryEvents(const AEvents: INyxEvents;
  const AInput: INyxCollectionInputLifetime;
  const AOpenAdmission: INyxPopoverOpenAdmission): INyxEvents;
var
  LInner: INyxEvents;
  LInput: INyxCollectionInputLifetime;
  LObservation: INyxEventObservation;
  LGeneration: INyxEventGeneration;
  LAdmission: INyxPopoverOpenAdmission;
begin
  { const interface arguments can alias caller slots. An alternative router's
    QueryInterface may release or replace those slots, so retain the exact
    arguments BEFORE the first outward capability query. }
  LInner := AEvents;
  LInput := AInput;
  LAdmission := AOpenAdmission;

  if (LInner = nil) or (LInput = nil) then
  begin
    raise ENyxSchedule.Create('Arranged queries require an actual router and input lifetime');
  end;

  if not Supports(LInner, INyxEventGeneration, LGeneration) then
  begin
    raise ENyxSchedule.Create('Arranged queries require router generation support');
  end;

  if Supports(LInner, INyxEventObservation, LObservation) then
  begin
    Result := TObservedArrangementQueryEvents.Create(LInner, LInput, LObservation,
      LAdmission);
  end
  else
  begin
    Result := TArrangementQueryEvents.Create(LInner, LInput, LAdmission);
  end;
end;

function NewNyxArrangementQueryEvents(const AEvents: INyxEvents;
  const AInput: INyxCollectionInputLifetime): INyxEvents;
begin
  Result := CreateArrangementQueryEvents(AEvents, AInput, nil);
end;

function NewNyxArrangementQueryEvents(const AEvents: INyxEvents;
  const AInput: INyxCollectionInputLifetime;
  const AOpenAdmission: INyxPopoverOpenAdmission): INyxEvents;
begin

  if AOpenAdmission = nil then
  begin
    raise ENyxSchedule.Create('Query forms require their actual open admission');
  end;
  Result := CreateArrangementQueryEvents(AEvents, AInput, AOpenAdmission);
end;

end.
