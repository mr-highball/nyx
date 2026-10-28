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
unit nyx.viewport.context;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.scheduler;

type
  { A bad extent, retired writer or invalid observation refuses before mutation. }
  ENyxViewportContext = class(Exception);

  { Copied presentation viewport in logical pixels, independent of a section's
    layout allocation and of named container measurements. Default is unknown;
    zero width/height is an actual observation. Revisions advance on changes only. }
  TNyxViewportContextSnapshot = record
  private
    FDefined: Boolean;
    FWidth: Double;
    FHeight: Double;
    FRevision: Integer;
  public
    function SameExtent(const AOther: TNyxViewportContextSnapshot): Boolean;
    property Defined: Boolean read FDefined;
    property Width: Double read FWidth;
    property Height: Double read FHeight;
    property Revision: Integer read FRevision;
  end;

  { The receiver is borrowed until cancellation or publisher retirement. The
    payload owns values only. Notifications run on the owning UI scheduler;
    renderers queue presentation work rather than changing another view inline. }
  TNyxViewportChanged = procedure(const AViewport: TNyxViewportContextSnapshot) of object;

  { Independent observation lease. Cancel is idempotent and disconnects its
    borrowed receiver immediately, including during another receiver's callback.
    Retaining this token never retains a source, renderer, document or host. }
  INyxViewportSubscription = interface(IInterface)
    ['{FFB09F05-F8E1-4793-B858-83D3857E6D71}']
    function GetActive: Boolean;
    procedure Cancel;
    property Active: Boolean read GetActive;
  end;

  { Read-only shared presentation space. Each consumer gets its own subscription;
    subscribing never replaces another receiver and sends no inline notification.
    Read Snapshot after subscribing to obtain the current value. After retirement,
    Active is false and the last copied snapshot remains readable. All operations
    belong to the source UI scheduler. No target or authored model is retained. }
  INyxViewportContext = interface(IInterface)
    ['{4689C25C-5292-48B5-BB45-E2691D5FB337}']
    function GetActive: Boolean;
    function GetSnapshot: TNyxViewportContextSnapshot;
    function Subscribe(AChanged: TNyxViewportChanged): INyxViewportSubscription;
    property Active: Boolean read GetActive;
    property Snapshot: TNyxViewportContextSnapshot read GetSnapshot;
  end;

  { The sole writer lease. Releasing it retires the independently retained read
    port and cancels all borrowed receivers. Publish validates finite/nonnegative
    dimensions before admission. Equal extents are silent. An observer failure
    does not undo an admitted observation: remaining current observers run, then
    the first failure is rethrown. Reentrant publication supersedes older delivery. }
  INyxViewportContextOwner = interface(IInterface)
    ['{37371368-5BCC-46E9-B30A-1721A2A204B5}']
    function GetContext: INyxViewportContext;
    procedure Publish(AWidth, AHeight: Double);
    procedure Retire;
    property Context: INyxViewportContext read GetContext;
  end;

{ UI-thread ownership is explicit even before the first viewport is observed. }
function NewNyxViewportContext(const AScheduler: INyxScheduler): INyxViewportContextOwner;

implementation

uses Math{$ifdef PAS2JS}, JS{$endif};

type
  { A subscription borrows this source until the source retires/removes it. }
  TViewportPort = class;

  { Private delivery capability kept by the source; public consumers can only
    inspect/cancel their lease. Snapshot copies retain entered deliveries safely. }
  IViewportObserver = interface(INyxViewportSubscription)
    ['{1D6520FD-56C4-45F7-A9B1-05AEBB47BB11}']
    procedure Deliver(const AViewport: TNyxViewportContextSnapshot);
    procedure Retire;
  end;

  { Owned registration order. Detached delivery arrays share managed leases,
    while cancellation removes source membership without invalidating that copy. }
  TViewportObservers = array of IViewportObserver;

  { No strong reverse edge to the source. Cancel clears the borrowed callback
    before removing source membership; its local self-lease survives that release. }
  TViewportRegistration = class(TInterfacedObject, INyxViewportSubscription,
    IViewportObserver)
  private
    FSource: TViewportPort;
    FScheduler: INyxScheduler;
    FChanged: TNyxViewportChanged;
  public
    constructor Create(ASource: TViewportPort; const AScheduler: INyxScheduler;
      AChanged: TNyxViewportChanged);
    destructor Destroy; override;
    function GetActive: Boolean;
    procedure Cancel;
    procedure Retire;
    procedure Deliver(const AViewport: TNyxViewportContextSnapshot);
  end;

  { The caller owns this cancellation lease; the source owns only registration
    state. Releasing the last public lease therefore cancels immediately without
    a source/token reference cycle or an orphaned borrowed receiver. }
  TViewportSubscription = class(TInterfacedObject, INyxViewportSubscription)
  private
    FObserver: IViewportObserver;
  public
    constructor Create(const AObserver: IViewportObserver);
    destructor Destroy; override;
    function GetActive: Boolean;
    procedure Cancel;
  end;

  { Values and registrations only. The separate writer retires this port before
    releasing its own lease, so readers cannot keep borrowed callbacks active. }
  TViewportPort = class(TInterfacedObject, INyxViewportContext)
  private
    FScheduler: INyxScheduler;
    FActive: Boolean;
    FSnapshot: TNyxViewportContextSnapshot;
    FObservers: TViewportObservers;
  public
    constructor Create(const AScheduler: INyxScheduler);
    destructor Destroy; override;
    function GetActive: Boolean;
    function GetSnapshot: TNyxViewportContextSnapshot;
    function Subscribe(AChanged: TNyxViewportChanged): INyxViewportSubscription;
    procedure Remove(const AObserver: IViewportObserver);
    procedure Publish(AWidth, AHeight: Double);
    procedure Retire;
  end;

  { Owns exactly one mutable port. Reader references do not prolong this writer's
    lifetime; destruction revokes receivers while preserving their copied values. }
  TViewportOwner = class(TInterfacedObject, INyxViewportContextOwner)
  private
    FPort: TViewportPort;
    FContext: INyxViewportContext;
  public
    constructor Create(const AScheduler: INyxScheduler);
    destructor Destroy; override;
    function GetContext: INyxViewportContext;
    procedure Publish(AWidth, AHeight: Double);
    procedure Retire;
  end;

function TNyxViewportContextSnapshot.SameExtent(const AOther: TNyxViewportContextSnapshot): Boolean;
begin
  Result := (Defined = AOther.Defined) and (Width = AOther.Width) and
    (Height = AOther.Height);
end;

constructor TViewportRegistration.Create(ASource: TViewportPort;
  const AScheduler: INyxScheduler; AChanged: TNyxViewportChanged);
begin
  inherited Create;
  FSource := ASource;
  FScheduler := AScheduler;
  FChanged := AChanged;
end;

destructor TViewportRegistration.Destroy;
begin
  Retire;
  inherited Destroy;
end;

function TViewportRegistration.GetActive: Boolean;
begin
  FScheduler.RequireUI;
  Result := FSource <> nil;
end;

procedure TViewportRegistration.Cancel;
var
  LLease: IViewportObserver;
  LSource: TViewportPort;
begin

  if FSource = nil then
  begin
    Exit;
  end;
  FScheduler.RequireUI;
  LLease := Self;
  LSource := FSource;
  Retire;
  LSource.Remove(LLease);
end;

procedure TViewportRegistration.Retire;
begin
  FSource := nil;
  FChanged := nil;
end;

procedure TViewportRegistration.Deliver(const AViewport: TNyxViewportContextSnapshot);
var
  LChanged: TNyxViewportChanged;
begin
  FScheduler.RequireUI;

  if FSource = nil then
  begin
    Exit;
  end;
  LChanged := FChanged;
  LChanged(AViewport);
end;

constructor TViewportSubscription.Create(const AObserver: IViewportObserver);
begin
  inherited Create;
  FObserver := AObserver;
end;

destructor TViewportSubscription.Destroy;
begin
  Cancel;
  FObserver := nil;
  inherited Destroy;
end;

function TViewportSubscription.GetActive: Boolean;
begin
  Result := FObserver.Active;
end;

procedure TViewportSubscription.Cancel;
begin
  FObserver.Cancel;
end;

constructor TViewportPort.Create(const AScheduler: INyxScheduler);
begin
  inherited Create;

  if AScheduler = nil then
  begin
    raise ENyxViewportContext.Create('A viewport context requires its UI scheduler');
  end;
  AScheduler.RequireUI;
  FScheduler := AScheduler;
  FActive := True;
end;

destructor TViewportPort.Destroy;
begin
  Retire;
  inherited Destroy;
end;

function TViewportPort.GetActive: Boolean;
begin
  FScheduler.RequireUI;
  Result := FActive;
end;

function TViewportPort.GetSnapshot: TNyxViewportContextSnapshot;
begin
  FScheduler.RequireUI;
  Result := FSnapshot;
end;

function TViewportPort.Subscribe(AChanged: TNyxViewportChanged): INyxViewportSubscription;
var
  LObserver: IViewportObserver;
  LToken: INyxViewportSubscription;
  LCount: Integer;
begin
  FScheduler.RequireUI;

  if not FActive or not Assigned(AChanged) then
  begin
    raise ENyxViewportContext.Create('Viewport observation requires a live source and receiver');
  end;
  LObserver := TViewportRegistration.Create(Self, FScheduler, AChanged);
  LToken := TViewportSubscription.Create(LObserver);
  LCount := Length(FObservers);
  SetLength(FObservers, LCount + 1);
  FObservers[LCount] := LObserver;
  Result := LToken;
end;

procedure TViewportPort.Remove(const AObserver: IViewportObserver);
var
  LIndex: Integer;
  LMove: Integer;
begin
  FScheduler.RequireUI;
  for LIndex := 0 to High(FObservers) do
  begin

    if FObservers[LIndex] = AObserver then
    begin
      for LMove := LIndex to High(FObservers) - 1 do
      begin
        FObservers[LMove] := FObservers[LMove + 1];
      end;
      SetLength(FObservers, Length(FObservers) - 1);
      Exit;
    end;
  end;
end;

procedure TViewportPort.Publish(AWidth, AHeight: Double);
var
  LLease: INyxViewportContext;
  LNext: TNyxViewportContextSnapshot;
  LObservers: TViewportObservers;
  LIndex: Integer;
  LFailed: Boolean;
  {$ifdef PAS2JS}
  LFailure: JSValue;
  {$else}
  LFailure: Exception;
  {$endif}
begin
  FScheduler.RequireUI;

  if not FActive then
  begin
    raise ENyxViewportContext.Create('The viewport writer is retired');
  end;

  if IsNan(AWidth) or IsInfinite(AWidth) or (AWidth < 0) or
    (AWidth > 2147483647.0) or IsNan(AHeight) or IsInfinite(AHeight) or
    (AHeight < 0) or (AHeight > 2147483647.0) then
  begin
    raise ENyxViewportContext.Create('Viewport dimensions must be finite nonnegative logical pixels');
  end;
  LNext := FSnapshot;
  LNext.FDefined := True;
  LNext.FWidth := AWidth;
  LNext.FHeight := AHeight;

  if LNext.SameExtent(FSnapshot) then
  begin
    Exit;
  end;

  if FSnapshot.Revision = MaxInt then
  begin
    raise ENyxViewportContext.Create('The viewport revision limit was reached');
  end;
  Inc(LNext.FRevision);
  LObservers := Copy(FObservers, 0, Length(FObservers));
  LLease := Self;
  FSnapshot := LNext;
  LFailed := False;
  {$ifdef PAS2JS}LFailure := Undefined;{$else}LFailure := nil;{$endif}
  for LIndex := 0 to High(LObservers) do
  begin

    if not FActive or (FSnapshot.Revision <> LNext.Revision) then
    begin
      Break;
    end;
    try
      LObservers[LIndex].Deliver(LNext);
    except

      if not LFailed then
      begin
        LFailed := True;
        {$ifdef PAS2JS}
        LFailure := JSExceptValue;
        {$else}
        LFailure := Exception(AcquireExceptionObject);
        {$endif}
      end;
    end;
  end;

  if LFailed then
  begin
    {$ifdef PAS2JS}
    asm
      throw LFailure;
    end;
    {$else}
    raise LFailure;
    {$endif}
  end;
end;

procedure TViewportPort.Retire;
var
  LObservers: TViewportObservers;
  LIndex: Integer;
begin

  if not FActive then
  begin
    Exit;
  end;
  FScheduler.RequireUI;
  FActive := False;
  LObservers := FObservers;
  FObservers := nil;
  for LIndex := 0 to High(LObservers) do
  begin
    LObservers[LIndex].Retire;
  end;
end;

constructor TViewportOwner.Create(const AScheduler: INyxScheduler);
begin
  inherited Create;
  FPort := TViewportPort.Create(AScheduler);
  FContext := FPort;
end;

destructor TViewportOwner.Destroy;
begin
  Retire;
  FPort := nil;
  FContext := nil;
  inherited Destroy;
end;

function TViewportOwner.GetContext: INyxViewportContext;
begin
  Result := FContext;
end;

procedure TViewportOwner.Publish(AWidth, AHeight: Double);
begin
  FPort.Publish(AWidth, AHeight);
end;

procedure TViewportOwner.Retire;
begin

  if FPort <> nil then
  begin
    FPort.Retire;
  end;
end;

function NewNyxViewportContext(const AScheduler: INyxScheduler): INyxViewportContextOwner;
begin
  Result := TViewportOwner.Create(AScheduler);
end;

end.
