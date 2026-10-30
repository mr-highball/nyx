{ Nyx. Copyright (c) 2020 mr-highball. MIT license; see LICENSE.
  Included by the recovery data owner. Only the capture writer owns this
  optional channel; journal and subscription owner pointers are borrowed. }

type
  { Retains the first observer failure until independent receivers finish.
    Foreign browser thrown values keep their exact identity, including
    undefined; native/Pascal diagnostics retain their portable UTF-8 text. }
  TRecoveryObserverFailure = record
    Failed: Boolean;
    Message: TNyxText;
    {$ifdef PAS2JS}
    Raw: Boolean;
    Value: JSValue;
    {$endif}
  end;

procedure CaptureRecoveryObserverFailure(var AFailure: TRecoveryObserverFailure;
  {$ifdef PAS2JS}AValue: JSValue{$else}AException: Exception{$endif});
{$ifdef PAS2JS}
var
  LException: Exception;
{$endif}
begin

  if AFailure.Failed then
  begin
    Exit;
  end;
  AFailure.Failed := True;
  {$ifdef PAS2JS}

  if not isClassInstance(AValue) or not (TObject(AValue) is Exception) then
  begin
    AFailure.Raw := True;
    AFailure.Value := AValue;
    Exit;
  end;
  LException := Exception(AValue);
  AFailure.Message := LException.Message;
  {$else}

  if AException is ENyxState then
  begin
    AFailure.Message := RawByteString(AException.Message);
    SetCodePage(RawByteString(AFailure.Message), CP_UTF8, False);
  end
  else
  begin
    AFailure.Message := TNyxText(AException.Message);
  end;
  {$endif}
end;

constructor TRecoveryChanges.Create;
begin
  inherited Create;
  FActive := True;
end;

destructor TRecoveryChanges.Destroy;
begin
  Stop;
  inherited Destroy;
end;

function TRecoveryChanges.Subscribe(AObserver: TNyxDraftRecoveryObserver):
  INyxCollectionSubscription;
var
  LToken: TRecoverySubscription;
begin

  if not FActive or FNotifying or not Assigned(AObserver) or
    (Length(FTokens) >= NyxMaximumCollectionSubscriptions) then
  begin
    raise ENyxCollection.Create('Recovery observation requires an active unreserved writer and a receiver');
  end;
  LToken := TRecoverySubscription.Create;
  Result := LToken;
  SetLength(FTokens, Length(FTokens) + 1);
  LToken.FOwner := Self;
  LToken.FObserver := AObserver;
  FTokens[High(FTokens)] := LToken;
end;

procedure TRecoveryChanges.Remove(AToken: TRecoverySubscription);
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

procedure TRecoveryChanges.Stop;
var
  LIndex: Integer;
begin
  FActive := False;
  for LIndex := 0 to High(FTokens) do
  begin
    FTokens[LIndex].FOwner := nil;
    FTokens[LIndex].FObserver := nil;
  end;
  FTokens := nil;
end;

procedure TRecoveryChanges.Publish(const AObservation: TNyxDraftRecoveryObservation);
var
  LLease: IInterface;
  LTokens: array of TRecoverySubscription;
  LLeases: array of INyxCollectionSubscription;
  LToken: TRecoverySubscription;
  LIndex: Integer;
  LFailure: TRecoveryObserverFailure;
begin
  LLease := Self;

  if not FActive then
  begin
    Exit;
  end;
  LTokens := System.Copy(FTokens, 0, Length(FTokens));
  SetLength(LLeases, Length(LTokens));
  for LIndex := 0 to High(LTokens) do
  begin
    LLeases[LIndex] := LTokens[LIndex];
  end;
  LFailure := Default(TRecoveryObserverFailure);
  FNotifying := True;
  try
    for LIndex := 0 to High(LTokens) do
    begin
      LToken := LTokens[LIndex];

      if FActive and (LToken.FOwner = Self) and Assigned(LToken.FObserver) then
      begin
        try
          LToken.FObserver(AObservation);
        except
          {$ifdef PAS2JS}CaptureRecoveryObserverFailure(LFailure, JSExceptValue);
          {$else}on E: Exception do CaptureRecoveryObserverFailure(LFailure, E);{$endif}
        end;
      end;
    end;
  finally
    FNotifying := False;
  end;

  if LFailure.Failed then
  begin
    {$ifdef PAS2JS}

    if LFailure.Raw then
    begin
      asm throw LFailure.Value; end;
    end;
    {$endif}
    raise ENyxCollectionNotification.Create('Recovery membership committed; observer failed: ' +
      LFailure.Message);
  end;
end;

destructor TRecoverySubscription.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

function TRecoverySubscription.GetConnected: Boolean;
begin
  Result := (FOwner <> nil) and FOwner.FActive and Assigned(FObserver);
end;

procedure TRecoverySubscription.Disconnect;
var
  LOwner: TRecoveryChanges;
begin
  LOwner := FOwner;
  FOwner := nil;
  FObserver := nil;

  if LOwner <> nil then
  begin
    LOwner.Remove(Self);
  end;
end;

procedure TJournal.NotifyMembership;
var
  LLease: INyxCollectionDraftRecovery;
  LChanges: TRecoveryChanges;
  LChangesLease: IInterface;
  LObservation: TNyxDraftRecoveryObservation;
begin
  LLease := Self;
  LChanges := FChanges;

  if (LChanges = nil) or (Length(LChanges.FTokens) = 0) then
  begin
    Exit;
  end;
  LChangesLease := LChanges;
  LObservation := Capture;
  LChanges.Publish(LObservation);
end;

function TWriter.Subscribe(AObserver: TNyxDraftRecoveryObserver): INyxCollectionSubscription;
begin

  if not FJournal.FActive then
  begin
    raise ENyxCollection.Create('A retired recovery writer cannot add an observer');
  end;
  Result := FChanges.Subscribe(AObserver);
end;
