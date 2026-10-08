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
unit nyx.image.events;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text, nyx.types, nyx.model, nyx.behavior, nyx.scheduler,
  nyx.events, nyx.image.lifecycle;

type
  { Prepared, owned delivery has no node/widget/renderer reference. Preparation
    runs before any callback; delivery may replace or destroy the whole view.
    The retained router, image peer and immutable payload remain independent.
    Adapters prepare their complete pending batch before invoking anything. }
  TNyxImageDelivery = class
  private
    FEvents: INyxEvents;
    FLifecycle: INyxImageLifecycle;
    FScope: INyxCancellationScope;
    FEvent: TNyxEventInfo;
    FOriginDesignID: TNyxText;
    FSourceDesignID: TNyxText;
    FRevision: Integer;
  public
    constructor Create(const AEvents: INyxEvents; AOrigin: TNyxNode;
      const ALifecycle: INyxImageLifecycle; const ASnapshot: TNyxImageSnapshot;
      const AScope: INyxCancellationScope);
    procedure Deliver;
    function Active: Boolean;
  end;
  TNyxImageDeliveries = array of TNyxImageDelivery;

  { One FIFO pump per view. A cached load can arrive from a different browser
    task source while the first notification's timer is clamped. Append to one
    pending pump, rather than scheduling independent timers that can overtake.
    Enqueue adopts all deliveries, pruning revoked generations. The queued work
    retains this independent queue; it retains no renderer/control/model. }
  INyxImageDeliveryQueue = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001019000003}']
    procedure Enqueue(const AEvents: INyxEvents; const ADeliveries: TNyxImageDeliveries);
  end;

function NewNyxImageDeliveryQueue: INyxImageDeliveryQueue;

{ Consume the bounded pending phases and append owned semantic deliveries.
  Borrows the origin only while preparing. Both target adapters use this route. }
procedure PrepareNyxImageDeliveries(const AEvents: INyxEvents; AOrigin: TNyxNode;
  const ALifecycle: INyxImageLifecycle; var ADeliveries: TNyxImageDeliveries);
{ Always retires every prepared delivery, including skipped/cancelled siblings.
  The array is private to this invocation; callbacks cannot mutate it. }
procedure DeliverNyxImages(const ADeliveries: TNyxImageDeliveries);

implementation

type
  TNyxImageDeliveryQueue = class(TInterfacedObject, INyxWork, INyxImageDeliveryQueue)
  private
    FDeliveries: TNyxImageDeliveries;
    FQueued: Boolean;
  public
    procedure Enqueue(const AEvents: INyxEvents; const ADeliveries: TNyxImageDeliveries);
    procedure Execute(const AExecution: INyxExecution);
    destructor Destroy; override;
  end;

procedure TNyxImageDeliveryQueue.Execute(const AExecution: INyxExecution);
var
  LDeliveries: TNyxImageDeliveries;
begin
  LDeliveries := FDeliveries;
  FDeliveries := nil;
  FQueued := False;
  DeliverNyxImages(LDeliveries);
end;

destructor TNyxImageDeliveryQueue.Destroy;
var
  LIndex: Integer;
begin
  for LIndex := 0 to Length(FDeliveries) - 1 do
  begin
    FDeliveries[LIndex].Free;
  end;
  inherited Destroy;
end;

procedure TNyxImageDeliveryQueue.Enqueue(const AEvents: INyxEvents;
  const ADeliveries: TNyxImageDeliveries);
var
  LWork: INyxWork;
  LIndex: Integer;
  LKept: TNyxImageDeliveries;
begin
  LKept := nil;
  for LIndex := 0 to Length(FDeliveries) - 1 do
  begin

    if FDeliveries[LIndex].Active then
    begin
      SetLength(LKept, Length(LKept) + 1);
      LKept[High(LKept)] := FDeliveries[LIndex];
    end
    else
    begin
      FDeliveries[LIndex].Free;
    end;
  end;
  FDeliveries := LKept;
  for LIndex := 0 to Length(ADeliveries) - 1 do
  begin
    SetLength(FDeliveries, Length(FDeliveries) + 1);
    FDeliveries[High(FDeliveries)] := ADeliveries[LIndex];
  end;

  if FQueued or (Length(FDeliveries) = 0) then
  begin
    Exit;
  end;
  FQueued := True;
  LWork := Self;
  try
    AEvents.Scheduler.Submit(LWork, neUIQueue);
  except
    FQueued := False;
    for LIndex := 0 to Length(FDeliveries) - 1 do
    begin
      FDeliveries[LIndex].Free;
    end;
    FDeliveries := nil;
    raise;
  end;
end;

function NewNyxImageDeliveryQueue: INyxImageDeliveryQueue;
begin
  Result := TNyxImageDeliveryQueue.Create;
end;

constructor TNyxImageDelivery.Create(const AEvents: INyxEvents; AOrigin: TNyxNode;
  const ALifecycle: INyxImageLifecycle; const ASnapshot: TNyxImageSnapshot;
  const AScope: INyxCancellationScope);
const
  CTriggers: array[TNyxImagePhase] of TNyxTrigger =
    (ntImageCleared, ntImageLoading, ntImageReady, ntImageError, ntImageCleared);
var
  LDispatch: TNyxDispatch;
begin
  inherited Create;
  FEvents := AEvents;
  FLifecycle := ALifecycle;
  FScope := AScope;
  FRevision := AEvents.ViewRevision;
  LDispatch := DispatchNyxBehavior(AOrigin, CTriggers[ASnapshot.Phase]);
  FEvent := LDispatch.Info;
  FEvent.HasImage := True;
  FEvent.Image := ASnapshot;
  FOriginDesignID := AOrigin.DesignID;
  FSourceDesignID := LDispatch.Source.DesignID;
end;

function TNyxImageDelivery.Active: Boolean;
begin
  Result := not FScope.Cancelled and (FEvents.ViewRevision = FRevision) and
    (FEvent.Name.Name <> '');
end;

procedure TNyxImageDelivery.Deliver;
var
  LExecutions: TNyxExecutions;
begin

  if not Active then
  begin
    Exit;
  end;
  LExecutions := FEvents.DispatchGuarded(FEvent, FOriginDesignID,
    FSourceDesignID, FScope);
  FLifecycle.Track(FEvent.Image.Request, LExecutions);
end;

procedure PrepareNyxImageDeliveries(const AEvents: INyxEvents; AOrigin: TNyxNode;
  const ALifecycle: INyxImageLifecycle; var ADeliveries: TNyxImageDeliveries);
const
  CTriggers: array[TNyxImagePhase] of TNyxTrigger =
    (ntImageCleared, ntImageLoading, ntImageReady, ntImageError, ntImageCleared);
var
  LSnapshot: TNyxImageSnapshot;
  LScope: INyxCancellationScope;
  LDelivery: TNyxImageDelivery;
begin

  if ALifecycle = nil then
  begin
    Exit;
  end;
  while ALifecycle.Take(LSnapshot, LScope) do
  begin

    if not AEvents.HasSubscribers(CTriggers[LSnapshot.Phase]) then
    begin
      { An unused image must not create a deferred courier or retain a router
        until a UI host pumps again. Named streams are included by this query. }
      Continue;
    end;
    LDelivery := TNyxImageDelivery.Create(AEvents, AOrigin, ALifecycle, LSnapshot, LScope);
    try
      SetLength(ADeliveries, Length(ADeliveries) + 1);
      ADeliveries[High(ADeliveries)] := LDelivery;
    except
      LDelivery.Free;
      raise;
    end;
  end;
end;

procedure DeliverNyxImages(const ADeliveries: TNyxImageDeliveries);
var
  LIndex: Integer;
begin
  try
    for LIndex := 0 to Length(ADeliveries) - 1 do
    begin
      ADeliveries[LIndex].Deliver;
    end;
  finally
    for LIndex := 0 to Length(ADeliveries) - 1 do
    begin
      ADeliveries[LIndex].Free;
    end;
  end;
end;

end.
