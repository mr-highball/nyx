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

type
  { Forward scope owner is retained by each lease/guard through its public
    interface; the private pointer only accesses synchronized local flags. }
  TWorkScope = class;

  { One counted admission. Its immutable owner lease prevents a cycle: the
    scope never retains this object. Active is accessed only under Scope.Lock. }
  TWorkLease = class(TInterfacedObject, INyxWorkLease)
  private
    FScopeLease: INyxWorkScope;
    FScope: TWorkScope;
    FActive: Boolean;
  public
    constructor Create(AScope: TWorkScope);
    destructor Destroy; override;
    procedure Retire;
  end;

  { Reversible idle reservation with immutable owner lifetime. Active is
    protected by the scope lock, including repeated explicit retirement. }
  TWorkPause = class(TInterfacedObject, INyxWorkPause)
  private
    FScopeLease: INyxWorkScope;
    FScope: TWorkScope;
    FActive: Boolean;
  public
    constructor Create(AScope: TWorkScope);
    destructor Destroy; override;
    procedure CheckReady;
    procedure Retire;
  end;

  { Count/pause/closure share one native lock, making idle admission atomic
    against concurrent child submissions. No interface release or caller code
    executes under this lock. The browser has the same state machine without
    a native critical section. Closed scopes keep admitting neither work nor
    previews, while existing leases still retire normally. }
  TWorkScope = class(TInterfacedObject, INyxWorkScope)
  private
    FCount: Integer;
    FPaused: Boolean;
    FClosed: Boolean;
    {$ifndef PAS2JS}
    FLock: TRTLCriticalSection;
    {$endif}
    procedure Lock;
    procedure Unlock;
    procedure RetireWork(AWork: TWorkLease);
    procedure RetirePause(APause: TWorkPause);
    procedure CheckPause(APause: TWorkPause);
  public
    constructor Create;
    destructor Destroy; override;
    function Acquire: INyxWorkLease;
    function TryPause: INyxWorkPause;
    procedure Close;
  end;

constructor TWorkLease.Create(AScope: TWorkScope);
begin
  inherited Create;
  FScopeLease := AScope;
  FScope := AScope;
end;

destructor TWorkLease.Destroy;
begin
  Retire;
  inherited Destroy;
end;

procedure TWorkLease.Retire;
begin
  FScope.RetireWork(Self);
end;

constructor TWorkPause.Create(AScope: TWorkScope);
begin
  inherited Create;
  FScopeLease := AScope;
  FScope := AScope;
end;

destructor TWorkPause.Destroy;
begin
  Retire;
  inherited Destroy;
end;

procedure TWorkPause.Retire;
begin
  FScope.RetirePause(Self);
end;

procedure TWorkPause.CheckReady;
begin
  FScope.CheckPause(Self);
end;

constructor TWorkScope.Create;
begin
  inherited Create;
  {$ifndef PAS2JS}
  InitCriticalSection(FLock);
  {$endif}
end;

destructor TWorkScope.Destroy;
begin
  {$ifndef PAS2JS}
  DoneCriticalSection(FLock);
  {$endif}
  inherited Destroy;
end;

procedure TWorkScope.Lock;
begin
  {$ifndef PAS2JS}
  EnterCriticalSection(FLock);
  {$endif}
end;

procedure TWorkScope.Unlock;
begin
  {$ifndef PAS2JS}
  LeaveCriticalSection(FLock);
  {$endif}
end;

function TWorkScope.Acquire: INyxWorkLease;
var
  LWork: TWorkLease;
  LLease: INyxWorkLease;
begin
  { Allocate and retain before the lock; final assignment invokes no user code. }
  LWork := TWorkLease.Create(Self);
  LLease := LWork;
  Lock;
  try

    if FClosed or FPaused or (FCount = High(Integer)) then
    begin
      raise ENyxSchedule.Create('Work scope is paused, closed or saturated');
    end;
    Inc(FCount);
    LWork.FActive := True;
  finally
    Unlock;
  end;
  Result := LLease;
end;

function TWorkScope.TryPause: INyxWorkPause;
var
  LPause: TWorkPause;
  LLease: INyxWorkPause;
  LAccepted: Boolean;
begin
  { Do not clear Result before admission: FPC may alias it to the caller's
    existing guard. Releasing that guard here would manufacture idle admission
    while this very call was entered with an active pause. }
  LAccepted := False;
  LPause := TWorkPause.Create(Self);
  LLease := LPause;
  Lock;
  try

    if not FClosed and not FPaused and (FCount = 0) then
    begin
      FPaused := True;
      LPause.FActive := True;
      LAccepted := True;
    end;
  finally
    Unlock;
  end;

  if LAccepted then
  begin
    Result := LLease;
  end
  else
  begin
    Result := nil;
  end;
end;

procedure TWorkScope.Close;
begin
  Lock;
  try
    FClosed := True;
  finally
    Unlock;
  end;
end;

procedure TWorkScope.RetireWork(AWork: TWorkLease);
begin
  Lock;
  try

    if AWork.FActive then
    begin
      AWork.FActive := False;
      Dec(FCount);
    end;
  finally
    Unlock;
  end;
end;

procedure TWorkScope.RetirePause(APause: TWorkPause);
begin
  Lock;
  try

    if APause.FActive then
    begin
      APause.FActive := False;
      FPaused := False;
    end;
  finally
    Unlock;
  end;
end;

procedure TWorkScope.CheckPause(APause: TWorkPause);
begin
  Lock;
  try

    if not APause.FActive or not FPaused or FClosed then
    begin
      raise ENyxSchedule.Create('Work preview pause is no longer ready');
    end;
  finally
    Unlock;
  end;
end;

function NewNyxWorkScope: INyxWorkScope;
begin
  Result := TWorkScope.Create;
end;
