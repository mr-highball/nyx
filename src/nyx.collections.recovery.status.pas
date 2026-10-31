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
unit nyx.collections.recovery.status;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.collections, nyx.collections.recovery;

type
  { Bounded presentation facts for one mount. Held describes its physical map,
    independently of historical journal refusals. No draft, snapshot, widget,
    callback, journal or mount is retained. These facts grant no write authority. }
  TNyxCollectionRecoveryStatus = record
    Active: Boolean;
    Held: Boolean;
    InputReady: Boolean;
    Journal: TNyxDraftRecoveryObservation;
  end;

  { UI-owner receiver; retain/disconnect the token before freeing the receiver.
    Presentation state may be updated here. Reentrant observations coalesce to
    the latest complete status and never recursively enter a receiver. }
  TNyxCollectionRecoveryObserver = procedure(
    const AStatus: TNyxCollectionRecoveryStatus) of object;

  { Optional portable mount facet. Notifications follow completed refresh tails,
    including composition-delayed capacity holds and explicit successful retry.
    Capture is bounded; it never reads a live editor or creates a journal. }
  INyxCollectionRecoveryNotifications = interface(IInterface)
    ['{2C15EE90-338C-49EC-B09F-897B5F6392F1}']
    function CaptureRecoveryStatus: TNyxCollectionRecoveryStatus;
    function SubscribeRecoveryStatus(AObserver: TNyxCollectionRecoveryObserver):
      INyxCollectionSubscription;
  end;

  { Independently owned implementation helper. The mount retains this writer
    privately and forwards its readonly facet; subscriptions cannot QI back to
    mount/writer authority. Retire revokes borrowed receivers before teardown. }
  INyxCollectionRecoveryStatusWriter = interface(INyxCollectionRecoveryNotifications)
    ['{6AAE483E-DAFC-4CC7-888E-245469404E5D}']
    procedure Publish(const AStatus: TNyxCollectionRecoveryStatus);
    procedure Retire;
  end;

function NewNyxCollectionRecoveryStatus: INyxCollectionRecoveryStatusWriter;

implementation

uses SysUtils, nyx.text
  {$ifdef PAS2JS}, JS{$endif};

type
  { Writer-owned channel; tokens borrow it and cannot keep its owner alive. }
  TRecoveryStatus = class;
  { A caller-owned lease clears its borrowed callback before removal. The channel
    revokes both pointers before destruction, so retained tokens are inert. }
  TRecoveryStatusToken = class(TInterfacedObject, INyxCollectionSubscription)
  private
    FOwner: TRecoveryStatus;
    FObserver: TNyxCollectionRecoveryObserver;
  public
    destructor Destroy; override;
    function GetConnected: Boolean;
    procedure Disconnect;
  end;

  { Owns cached scalar observations and borrowed token addresses only. Entered
    dispatch retains channel/token leases; a receiver can disconnect or retire
    every owner without invalidating the remaining dispatch frame. }
  TRecoveryStatus = class(TInterfacedObject, INyxCollectionRecoveryNotifications,
    INyxCollectionRecoveryStatusWriter)
  private
    FActive: Boolean;
    FNotifying: Boolean;
    FStatus: TNyxCollectionRecoveryStatus;
    FTokens: array of TRecoveryStatusToken;
    procedure Remove(AToken: TRecoveryStatusToken);
  public
    constructor Create;
    destructor Destroy; override;
    function CaptureRecoveryStatus: TNyxCollectionRecoveryStatus;
    function SubscribeRecoveryStatus(AObserver: TNyxCollectionRecoveryObserver):
      INyxCollectionSubscription;
    procedure Publish(const AStatus: TNyxCollectionRecoveryStatus);
    procedure Retire;
  end;

function SameStatus(const ALeft, ARight: TNyxCollectionRecoveryStatus): Boolean;
begin
  Result := (ALeft.Active = ARight.Active) and (ALeft.Held = ARight.Held) and
    (ALeft.InputReady = ARight.InputReady) and
    (ALeft.Journal.Active = ARight.Journal.Active) and
    (ALeft.Journal.Revision = ARight.Journal.Revision) and
    (ALeft.Journal.Count = ARight.Journal.Count) and
    (ALeft.Journal.RetainedBytes = ARight.Journal.RetainedBytes) and
    (ALeft.Journal.BytesSaturated = ARight.Journal.BytesSaturated) and
    (ALeft.Journal.Warning = ARight.Journal.Warning);
end;

constructor TRecoveryStatus.Create;
begin
  inherited Create;
  FActive := True;
end;

destructor TRecoveryStatus.Destroy;
begin
  Retire;
  inherited Destroy;
end;

function TRecoveryStatus.CaptureRecoveryStatus: TNyxCollectionRecoveryStatus;
begin
  Result := FStatus;
end;

function TRecoveryStatus.SubscribeRecoveryStatus(AObserver: TNyxCollectionRecoveryObserver):
  INyxCollectionSubscription;
var
  LToken: TRecoveryStatusToken;
begin

  if not FActive or FNotifying or not Assigned(AObserver) or
    (Length(FTokens) >= NyxMaximumCollectionSubscriptions) then
  begin
    raise ENyxCollection.Create('Recovery status needs an active owner and a receiver');
  end;
  LToken := TRecoveryStatusToken.Create;
  Result := LToken;
  SetLength(FTokens, Length(FTokens) + 1);
  LToken.FOwner := Self;
  LToken.FObserver := AObserver;
  FTokens[High(FTokens)] := LToken;
end;

procedure TRecoveryStatus.Remove(AToken: TRecoveryStatusToken);
var
  LIndex: Integer;
  LMove: Integer;
begin
  for LIndex := 0 to High(FTokens) do
  begin

    if FTokens[LIndex] = AToken then
    begin
      for LMove := LIndex to High(FTokens) - 1 do
      begin
        FTokens[LMove] := FTokens[LMove + 1];
      end;
      SetLength(FTokens, Length(FTokens) - 1);
      Exit;
    end;
  end;
end;

procedure TRecoveryStatus.Retire;
var
  LIndex: Integer;
begin
  FActive := False;
  FStatus.Active := False;
  FStatus.Held := False;
  FStatus.InputReady := False;
  FStatus.Journal.Active := False;
  for LIndex := 0 to High(FTokens) do
  begin
    FTokens[LIndex].FOwner := nil;
    FTokens[LIndex].FObserver := nil;
  end;
  FTokens := nil;
end;

procedure TRecoveryStatus.Publish(const AStatus: TNyxCollectionRecoveryStatus);
var
  LLease: IInterface;
  LTokens: array of TRecoveryStatusToken;
  LLeases: array of INyxCollectionSubscription;
  LSnapshot: TNyxCollectionRecoveryStatus;
  LToken: TRecoveryStatusToken;
  LIndex: Integer;
  LFailed: Boolean;
  LError: TNyxText;
  {$ifdef PAS2JS}
  LRaw: Boolean;
  LValue: JSValue;
  {$endif}
begin
  LLease := Self;

  if not FActive or SameStatus(FStatus, AStatus) then
  begin
    Exit;
  end;
  FStatus := AStatus;

  if FNotifying then
  begin
    Exit;
  end;
  LFailed := False;
  {$ifdef PAS2JS}LRaw := False;{$endif}
  FNotifying := True;
  try
    repeat
      LSnapshot := FStatus;
      LTokens := Copy(FTokens, 0, Length(FTokens));
      SetLength(LLeases, Length(LTokens));
      for LIndex := 0 to High(LTokens) do
      begin
        LLeases[LIndex] := LTokens[LIndex];
      end;
      for LIndex := 0 to High(LTokens) do
      begin
        LToken := LTokens[LIndex];

        if FActive and (LToken.FOwner = Self) and Assigned(LToken.FObserver) then
        begin
          try
            LToken.FObserver(LSnapshot);
          except
            {$ifdef PAS2JS}

            if not LFailed then
            begin
              LValue := JSExceptValue;
              LRaw := not isClassInstance(LValue) or not (TObject(LValue) is Exception);

              if not LRaw then
              begin
                LError := Exception(LValue).Message;
              end;
            end;
            LFailed := True;
            {$else}
            on LException: Exception do
            begin

              if not LFailed then
              begin
                LError := TNyxText(LException.Message);
              end;
              LFailed := True;
            end;
            {$endif}
          end;
        end;
      end;
    until not FActive or SameStatus(LSnapshot, FStatus);
  finally
    FNotifying := False;
  end;

  if LFailed then
  begin
    {$ifdef PAS2JS}

    if LRaw then
    begin
      asm throw LValue; end;
    end;
    {$endif}
    raise ENyxCollectionNotification.Create('Recovery status committed; observer failed: ' + LError);
  end;
end;

destructor TRecoveryStatusToken.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

function TRecoveryStatusToken.GetConnected: Boolean;
begin
  Result := (FOwner <> nil) and FOwner.FActive and Assigned(FObserver);
end;

procedure TRecoveryStatusToken.Disconnect;
var
  LOwner: TRecoveryStatus;
begin
  LOwner := FOwner;
  FOwner := nil;
  FObserver := nil;

  if LOwner <> nil then
  begin
    LOwner.Remove(Self);
  end;
end;

function NewNyxCollectionRecoveryStatus: INyxCollectionRecoveryStatusWriter;
begin
  Result := TRecoveryStatus.Create;
end;

end.
