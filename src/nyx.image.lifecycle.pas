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
unit nyx.image.lifecycle;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.images, nyx.scheduler;

type
  { IDs belong to one mounted image face, not a document or global resource.
    A replaced face starts its own sequence. Compare with origin identity and
    view lifetime when retaining snapshots across navigation. Zero is absent. }
  TNyxImageRequestID = type Integer;
  TNyxImagePhase = (nipIdle, nipLoading, nipReady, nipFailed, nipCleared);
  TNyxImageFailure = (nifNone, nifUnavailable, nifDecode);

  { Immutable owned observation. Ready means the target accepted a decoded
    image with positive natural dimensions; it is not portable pixel integrity.
    Native dimensions follow its decoder, browser dimensions its image request.
    Loading/failure/clear have zero dimensions. Failure text is owned diagnostic
    text, never a behavior selector. No node, renderer, widget or event is held. }
  TNyxImageSnapshot = record
  private
    { Managed marker stays absent in legacy partially initialized event records. }
    FMarker: TNyxText;
    FRequest: TNyxImageRequestID;
    FSource: TNyxImageSource;
    FPhase: TNyxImagePhase;
    FWidth: Integer;
    FHeight: Integer;
    FFailure: TNyxImageFailure;
    FMessage: TNyxText;
  public
    function Defined: Boolean;
    property Request: TNyxImageRequestID read FRequest;
    property Source: TNyxImageSource read FSource;
    property Phase: TNyxImagePhase read FPhase;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Failure: TNyxImageFailure read FFailure;
    property Message: TNyxText read FMessage;
  end;
  TNyxImageExecutions = array of INyxExecution;

  { Adapter-owned request peer. All methods run on the UI thread; immutable
    snapshots and the cancellation scope may survive in native worker work.
    Start cancels older delivery before returning, clears older signals, and
    queues Loading (or Cleared for NoImage). Ready/Fail accept only the active
    loading request. Old/duplicate completions return False without mutation.
    At most Loading plus one terminal signal await accepted view publication.
    Close revokes delivery and pending work, retaining no platform/model handle. }
  INyxImageLifecycle = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001019000001}']
    function Start(const ASource: TNyxImageSource): TNyxImageRequestID;
    function Ready(ARequest: TNyxImageRequestID; AWidth, AHeight: Integer): Boolean;
    function Fail(ARequest: TNyxImageRequestID; AFailure: TNyxImageFailure;
      const AMessage: TNyxText): Boolean;
    function Take(out ASnapshot: TNyxImageSnapshot;
      out AScope: INyxCancellationScope): Boolean;
    procedure Track(ARequest: TNyxImageRequestID;
      const AExecutions: TNyxImageExecutions);
    procedure Close;
    function GetCurrent: TNyxImageSnapshot;
    property Current: TNyxImageSnapshot read GetCurrent;
  end;

function NewNyxImageLifecycle: INyxImageLifecycle;

implementation

type
  TNyxImageLifecycle = class(TInterfacedObject, INyxImageLifecycle)
  private
    FCurrent: TNyxImageSnapshot;
    FScope: INyxCancellationScope;
    FPending: array[0..1] of TNyxImageSnapshot;
    FCount: Integer;
    FExecutions: TNyxImageExecutions;
    FClosed: Boolean;
    procedure CancelDelivery;
    procedure Queue;
  public
    function Start(const ASource: TNyxImageSource): TNyxImageRequestID;
    function Ready(ARequest: TNyxImageRequestID; AWidth, AHeight: Integer): Boolean;
    function Fail(ARequest: TNyxImageRequestID; AFailure: TNyxImageFailure;
      const AMessage: TNyxText): Boolean;
    function Take(out ASnapshot: TNyxImageSnapshot;
      out AScope: INyxCancellationScope): Boolean;
    procedure Track(ARequest: TNyxImageRequestID;
      const AExecutions: TNyxImageExecutions);
    procedure Close;
    function GetCurrent: TNyxImageSnapshot;
    destructor Destroy; override;
  end;

function TNyxImageSnapshot.Defined: Boolean;
begin
  Result := FMarker = 'image-request';
end;

procedure TNyxImageLifecycle.CancelDelivery;
var
  LIndex: Integer;
begin

  if FScope <> nil then
  begin
    FScope.Cancel;
  end;
  for LIndex := 0 to Length(FExecutions) - 1 do
  begin
    FExecutions[LIndex].Cancel;
  end;
  FExecutions := nil;
  FScope := nil;
  FCount := 0;
  FPending[0] := Default(TNyxImageSnapshot);
  FPending[1] := Default(TNyxImageSnapshot);
end;

procedure TNyxImageLifecycle.Queue;
begin

  if FCount = Length(FPending) then
  begin
    raise ENyxImage.Create('Image request notification capacity exceeded');
  end;
  FPending[FCount] := FCurrent;
  Inc(FCount);
end;

function TNyxImageLifecycle.Start(const ASource: TNyxImageSource): TNyxImageRequestID;
var
  LScope: INyxCancellationScope;
begin

  if FClosed or (FCurrent.Request = High(Integer)) then
  begin
    raise ENyxImage.Create('Image request lifetime is closed or exhausted');
  end;
  LScope := NewNyxCancellationScope;
  CancelDelivery;
  FScope := LScope;
  FCurrent.FMarker := 'image-request';
  Inc(FCurrent.FRequest);
  FCurrent.FSource := ASource;
  FCurrent.FWidth := 0;
  FCurrent.FHeight := 0;
  FCurrent.FFailure := nifNone;
  FCurrent.FMessage := '';

  if ASource.Kind = nisEmpty then
  begin
    FCurrent.FPhase := nipCleared;
  end
  else
  begin
    FCurrent.FPhase := nipLoading;
  end;
  Queue;
  Result := FCurrent.Request;
end;

function TNyxImageLifecycle.Ready(ARequest: TNyxImageRequestID;
  AWidth, AHeight: Integer): Boolean;
begin
  Result := not FClosed and (ARequest = FCurrent.Request) and
    (FCurrent.Phase = nipLoading);

  if not Result then
  begin
    Exit;
  end;

  if (AWidth <= 0) or (AHeight <= 0) then
  begin
    raise ENyxImage.Create('Ready image dimensions must be positive');
  end;
  FCurrent.FWidth := AWidth;
  FCurrent.FHeight := AHeight;
  FCurrent.FPhase := nipReady;
  Queue;
end;

function TNyxImageLifecycle.Fail(ARequest: TNyxImageRequestID;
  AFailure: TNyxImageFailure; const AMessage: TNyxText): Boolean;
begin
  Result := not FClosed and (ARequest = FCurrent.Request) and
    (FCurrent.Phase = nipLoading);

  if not Result then
  begin
    Exit;
  end;

  if (AFailure = nifNone) or (AMessage = '') then
  begin
    raise ENyxImage.Create('Failed image requires a typed failure and diagnostic');
  end;
  FCurrent.FFailure := AFailure;
  FCurrent.FMessage := AMessage;
  FCurrent.FPhase := nipFailed;
  Queue;
end;

function TNyxImageLifecycle.Take(out ASnapshot: TNyxImageSnapshot;
  out AScope: INyxCancellationScope): Boolean;
begin
  ASnapshot := Default(TNyxImageSnapshot);
  AScope := nil;
  Result := not FClosed and (FCount > 0);

  if Result then
  begin
    ASnapshot := FPending[0];
    AScope := FScope;
    FPending[0] := FPending[1];
    FPending[1] := Default(TNyxImageSnapshot);
    Dec(FCount);
  end;
end;

procedure TNyxImageLifecycle.Track(ARequest: TNyxImageRequestID;
  const AExecutions: TNyxImageExecutions);
var
  LIndex: Integer;
  LKept: TNyxImageExecutions;

  procedure Keep(const AExecution: INyxExecution);
  begin

    if AExecution.Status in [nesPending, nesRunning] then
    begin
      SetLength(LKept, Length(LKept) + 1);
      LKept[High(LKept)] := AExecution;
    end;
  end;

begin

  if FClosed or (ARequest <> FCurrent.Request) then
  begin
    { A sequential handler may already have replaced this request before the
      router returns its tokens. Cancel those returned tokens too. }
    for LIndex := 0 to Length(AExecutions) - 1 do
    begin
      AExecutions[LIndex].Cancel;
    end;
    Exit;
  end;
  LKept := nil;
  for LIndex := 0 to Length(FExecutions) - 1 do
  begin
    Keep(FExecutions[LIndex]);
  end;
  for LIndex := 0 to Length(AExecutions) - 1 do
  begin
    Keep(AExecutions[LIndex]);
  end;
  FExecutions := LKept;
end;

procedure TNyxImageLifecycle.Close;
begin
  FClosed := True;
  CancelDelivery;
end;

destructor TNyxImageLifecycle.Destroy;
begin
  Close;
  inherited Destroy;
end;

function TNyxImageLifecycle.GetCurrent: TNyxImageSnapshot;
begin
  Result := FCurrent;
end;

function NewNyxImageLifecycle: INyxImageLifecycle;
begin
  Result := TNyxImageLifecycle.Create;
end;

end.
