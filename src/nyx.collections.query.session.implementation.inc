{ Private implementation of the optional shared query session. All values are
  retained by the existing TView/TQueryPreparation owners. This include adds no
  store, query evaluator, widget, scheduler or application lifetime. }

function CopyQueryContribution(const AValue: TNyxQueryContribution): TNyxQueryContribution;
begin
  Result := AValue;
  Result.Filter := AValue.Filter.Copy;
end;

function SameQueryOwner(const ALeft, ARight: TNyxQueryOwnerInfo): Boolean;
begin
  { Connected is capture-time presentation. Revocation is an independent live
    flag, never a mutation of already accepted canonical query metadata. }
  Result := (ALeft.ID.Value = ARight.ID.Value) and
    (ALeft.Origin.Name = ARight.Origin.Name) and (ALeft.Caption = ARight.Caption);
end;

function QueryOwnerPresentation(const AInfo: TNyxQueryOwnerInfo;
  ALife: TQueryOwnerLife): TNyxQueryOwnerInfo;
begin
  Result := AInfo;
  Result.Connected := (ALife <> nil) and ALife.Active and
    (ALife.Scope <> nil) and ALife.Scope.Active;
end;

procedure TQuerySessionState.Initialize(const ABaseline: TNyxCollectionQuery);
begin
  Scope := TQueryScope.Create;
  ScopeLease := Scope;
  Scope.Active := True;
  Baseline := ABaseline.Copy;
  Effective := Baseline.Copy;
end;

procedure TQuerySessionState.Assign(AOther: TQuerySessionState);
var
  LIndex: Integer;
begin
  Scope := AOther.Scope;
  ScopeLease := AOther.ScopeLease;
  Baseline := AOther.Baseline.Copy;
  Effective := AOther.Effective.Copy;
  HasUserSort := AOther.HasUserSort;
  UserSort := AOther.UserSort.Copy;
  SortOwner := AOther.SortOwner;
  SortLife := AOther.SortLife;
  SortLifeLease := AOther.SortLifeLease;
  SetLength(Contributions, Length(AOther.Contributions));
  SetLength(OwnerFlags, Length(Contributions));
  SetLength(OwnerLives, Length(Contributions));
  for LIndex := 0 to High(Contributions) do
  begin
    Contributions[LIndex] := CopyQueryContribution(AOther.Contributions[LIndex]);
    OwnerFlags[LIndex] := AOther.OwnerFlags[LIndex];
    OwnerLives[LIndex] := AOther.OwnerLives[LIndex];
  end;
end;

procedure TQuerySessionState.RemoveAt(AIndex: Integer);
var
  LIndex: Integer;
begin
  for LIndex := AIndex to Length(Contributions) - 2 do
  begin
    Contributions[LIndex] := Contributions[LIndex + 1];
    OwnerFlags[LIndex] := OwnerFlags[LIndex + 1];
    OwnerLives[LIndex] := OwnerLives[LIndex + 1];
  end;
  SetLength(Contributions, Length(Contributions) - 1);
  SetLength(OwnerFlags, Length(Contributions));
  SetLength(OwnerLives, Length(Contributions));
end;

procedure TQuerySessionState.Compose;
var
  LFilters: array of INyxCollectionPredicate;
  LFilter: INyxCollectionPredicate;
  LIndex: Integer;

  { Balance the existing binary AND construct. A long palette of independent
    leaf filters must not consume one depth level for every user contribution.
    The existing node/depth/byte limits still admit or refuse the complete tree. }
  function Joined(AFirst, ALast: Integer): INyxCollectionPredicate;
  var
    LMiddle: Integer;
    LLeft: INyxCollectionPredicate;
    LRight: INyxCollectionPredicate;
  begin

    if AFirst = ALast then
    begin
      Exit(LFilters[AFirst]);
    end;
    LMiddle := AFirst + (ALast - AFirst) div 2;
    LLeft := Joined(AFirst, LMiddle);
    LRight := Joined(LMiddle + 1, ALast);
    Result := LLeft.AndAlso(LRight);
  end;

begin
  Effective := Baseline.Copy;
  LFilter := Baseline.Filter;

  if LFilter <> nil then
  begin
    SetLength(LFilters, 1);
    LFilters[0] := LFilter;
  end;
  for LIndex := 0 to High(Contributions) do
  begin
    SetLength(LFilters, Length(LFilters) + 1);
    LFilters[High(LFilters)] := NyxPredicateFromData(Contributions[LIndex].Filter);
  end;

  if Length(LFilters) > 0 then
  begin
    Effective := Effective.Where(Joined(0, High(LFilters)));
  end;

  if HasUserSort then
  begin
    Effective := Effective.Unsorted;
    for LIndex := 0 to UserSort.SortCount - 1 do
    begin

      if LIndex = 0 then
      begin
        Effective := Effective.OrderBy(UserSort.SortAt(LIndex));
      end
      else
      begin
        Effective := Effective.ThenBy(UserSort.SortAt(LIndex));
      end;
    end;
  end;
end;

procedure TQuerySessionState.Validate(const ASchema: TNyxCollectionSchema);
var
  LIndex: Integer;
  LFilter: INyxCollectionPredicate;
begin
  { Validate even a baseline sort currently overridden by user sorting. Reset
    must never expose an unadmitted hidden policy imported from an extension. }
  Baseline.Validate(ASchema);
  UserSort.Validate(ASchema);
  for LIndex := 0 to High(Contributions) do
  begin
    LFilter := NyxPredicateFromData(Contributions[LIndex].Filter);
    LFilter.Validate(ASchema);
  end;
  Effective.Validate(ASchema);
end;

function TQuerySessionState.SameValues(AOther: TQuerySessionState): Boolean;
var
  LIndex: Integer;
begin
  Result := False;

  if (Scope <> AOther.Scope) or (HasUserSort <> AOther.HasUserSort) or
    (Length(Contributions) <> Length(AOther.Contributions)) or
    (Baseline.ToData.ToJSON <> AOther.Baseline.ToData.ToJSON) then
  begin
    Exit;
  end;

  if HasUserSort and (not SameQueryOwner(SortOwner, AOther.SortOwner) or
    (UserSort.ToData.ToJSON <> AOther.UserSort.ToData.ToJSON)) then
  begin
    Exit;
  end;
  for LIndex := 0 to High(Contributions) do
  begin

    if not SameQueryOwner(Contributions[LIndex].Owner, AOther.Contributions[LIndex].Owner) or
      (Contributions[LIndex].Channel.Name <> AOther.Contributions[LIndex].Channel.Name) or
      (Contributions[LIndex].Filter.ToJSON <> AOther.Contributions[LIndex].Filter.ToJSON) then
    begin
      Exit;
    end;
  end;
  Result := True;
end;

procedure TQuerySessionState.Apply(const AOwner: TNyxQueryOwnerInfo;
  ALife: TQueryOwnerLife; const ALifeLease: IInterface;
  const AEdits: TNyxQuerySessionEdits);
var
  LEdit: TNyxQuerySessionEdit;
  LQuery: TNyxCollectionQuery;
  LFilter: INyxCollectionPredicate;
  LIndex: Integer;
  LMatch: Integer;
  LEditIndex: Integer;
  LOwnerID: Integer;

  procedure ResetOrder;
  begin
    HasUserSort := False;
    UserSort := NyxCollectionQuery;
    SortOwner := Default(TNyxQueryOwnerInfo);
    SortLife := nil;
    SortLifeLease := nil;
  end;

begin

  if (AEdits.Count < 1) or (AEdits.Count > NyxMaximumQuerySessionEdits) then
  begin
    raise ENyxCollection.Create('A query session command requires a bounded nonempty edit batch');
  end;
  for LEditIndex := 0 to AEdits.Count - 1 do
  begin
    LEdit := AEdits.EditAt(LEditIndex);
    LOwnerID := AOwner.ID.Value;

    if LEdit.Kind = nqseRemoveFilter then
    begin
      LOwnerID := LEdit.OwnerID.Value;
    end;
    case LEdit.Kind of
      nqseFilter, nqseClearFilter, nqseRemoveFilter:
      begin
        { Admit even an absent Clear channel; an undefined typed reference must
          not become an accidental global reset. IDs are data, not authority. }
        NyxQueryChannel(LEdit.Channel.Name);

        if LOwnerID < 1 then
        begin
          raise ENyxCollection.Create('A query filter requires an owner identity');
        end;
        LMatch := -1;
        for LIndex := 0 to High(Contributions) do
        begin

          if (Contributions[LIndex].Owner.ID.Value = LOwnerID) and
            (Contributions[LIndex].Channel.Name = LEdit.Channel.Name) then
          begin
            LMatch := LIndex;
            Break;
          end;
        end;

        if LEdit.Kind = nqseFilter then
        begin
          LQuery := LEdit.Query;
          LFilter := LQuery.Filter;

          if (LFilter = nil) or (LQuery.SortCount <> 0) then
          begin
            raise ENyxCollection.Create('A filter contribution requires only a typed predicate');
          end;

          if LMatch < 0 then
          begin

            if Length(Contributions) >= NyxMaximumQuerySessionContributions then
            begin
              raise ENyxCollection.Create('Query session contribution budget exceeded');
            end;
            LMatch := Length(Contributions);
            SetLength(Contributions, LMatch + 1);
            SetLength(OwnerFlags, LMatch + 1);
            SetLength(OwnerLives, LMatch + 1);
          end;
          Contributions[LMatch].Owner := AOwner;
          Contributions[LMatch].Channel := LEdit.Channel;
          Contributions[LMatch].Filter := LFilter.ToData;
          OwnerFlags[LMatch] := ALife;
          OwnerLives[LMatch] := ALifeLease;
        end
        else
        begin

          if LMatch >= 0 then
          begin
            RemoveAt(LMatch);
          end;
        end;
      end;
      nqseSortBy:
      begin
        LQuery := LEdit.Query;
        LFilter := LQuery.Filter;

        if LFilter <> nil then
        begin
          raise ENyxCollection.Create('Shared sort requires a sort-only query');
        end;
        HasUserSort := True;
        UserSort := LQuery.Copy;
        SortOwner := AOwner;
        SortLife := ALife;
        SortLifeLease := ALifeLease;
      end;
      nqseResetSort: ResetOrder;
      nqseClearOwner:
      begin
        for LIndex := High(Contributions) downto 0 do
        begin

          if Contributions[LIndex].Owner.ID.Value = LOwnerID then
          begin
            RemoveAt(LIndex);
          end;
        end;

        if HasUserSort and (SortOwner.ID.Value = LOwnerID) then
        begin
          ResetOrder;
        end;
      end;
      nqseClearAllFilters, nqseResetAll:
      begin
        Contributions := nil;
        OwnerFlags := nil;
        OwnerLives := nil;

        if LEdit.Kind = nqseResetAll then
        begin
          ResetOrder;
        end;
      end;
    end;
  end;
end;

function TQuerySessionState.Import(
  const ASource: INyxCollectionQuerySessionSnapshot): Integer;
var
  LCount: Integer;
  LIndex: Integer;
  LPrevious: Integer;
  LInfo: TNyxQueryOwnerInfo;
  LOwners: array of TNyxQueryOwnerInfo;

  function ImportedOwner(const AInfo: TNyxQueryOwnerInfo): TNyxQueryOwnerInfo;
  var
    LOwner: Integer;
  begin

    if not AInfo.ID.Defined then
    begin
      raise ENyxCollection.Create('Imported query metadata requires an owner ID');
    end;
    NyxQueryOrigin(AInfo.Origin.Name);
    NyxCollection(AInfo.Caption);
    for LOwner := 0 to High(LOwners) do
    begin

      if LOwners[LOwner].ID.Value = AInfo.ID.Value then
      begin

        if (LOwners[LOwner].Origin.Name <> AInfo.Origin.Name) or
          (LOwners[LOwner].Caption <> AInfo.Caption) then
        begin
          raise ENyxCollection.Create('Imported owner identity has conflicting metadata');
        end;
        Result := AInfo;
        Result.ID := NyxQueryOwnerID(LOwner + 1);
        Result.Connected := False;
        Exit;
      end;
    end;

    if Length(LOwners) >= NyxMaximumQuerySessionOwners then
    begin
      raise ENyxCollection.Create('Imported query owner budget exceeded');
    end;
    SetLength(LOwners, Length(LOwners) + 1);
    LOwners[High(LOwners)] := AInfo;
    Result := AInfo;
    Result.ID := NyxQueryOwnerID(Length(LOwners));
    Result.Connected := False;
  end;

begin

  if ASource = nil then
  begin
    raise ENyxCollection.Create('Query metadata import requires a snapshot');
  end;
  LCount := ASource.Count;

  if (LCount < 0) or (LCount > NyxMaximumQuerySessionContributions) then
  begin
    raise ENyxCollection.Create('Imported query contribution budget exceeded');
  end;
  Initialize(ASource.Baseline);
  SetLength(Contributions, LCount);
  SetLength(OwnerFlags, LCount);
  SetLength(OwnerLives, LCount);
  for LIndex := 0 to LCount - 1 do
  begin
    Contributions[LIndex] := CopyQueryContribution(ASource.ContributionAt(LIndex));
    NyxQueryChannel(Contributions[LIndex].Channel.Name);
    Contributions[LIndex].Owner := ImportedOwner(Contributions[LIndex].Owner);
    { Normalize closed metadata before it can enter the target's canonical state. }
    Contributions[LIndex].Filter := NyxPredicateFromData(Contributions[LIndex].Filter).ToData;
    for LPrevious := 0 to LIndex - 1 do
    begin

      if (Contributions[LPrevious].Owner.ID.Value = Contributions[LIndex].Owner.ID.Value) and
        (Contributions[LPrevious].Channel.Name = Contributions[LIndex].Channel.Name) then
      begin
        raise ENyxCollection.Create('Imported query repeats an owner/channel');
      end;
    end;
  end;
  HasUserSort := ASource.HasUserSort;

  if HasUserSort then
  begin
    LInfo := ASource.SortOwner;
    SortOwner := ImportedOwner(LInfo);
    UserSort := ASource.UserSort;

    if UserSort.Filter <> nil then
    begin
      raise ENyxCollection.Create('Imported shared order contains a filter');
    end;
  end;
  Result := Length(LOwners);
end;

destructor TQueryOwnerLease.Destroy;
begin
  Revoke;
  inherited Destroy;
end;

procedure TQueryOwnerLease.Revoke;
begin

  if Life <> nil then
  begin
    Life.Active := False;
  end;
end;

function TQueryOwnerLease.GetConnected: Boolean;
begin
  Result := (Life <> nil) and Life.Active and (Life.Scope <> nil) and Life.Scope.Active;
end;

function TQueryOwnerLease.GetID: TNyxQueryOwnerID;
begin
  Result := Info.ID;
end;

function TQueryOwnerLease.GetOrigin: TNyxQueryOriginRef;
begin
  Result := Info.Origin;
end;

function TQueryOwnerLease.GetCaption: TNyxText;
begin
  Result := Info.Caption;
end;

function TQueryOwnerLease.OwnerLease: TQueryOwnerLease;
begin
  Result := Self;
end;

constructor TQuerySessionSnapshot.Create(AState: TQuerySessionState;
  const AStateLease, AIdentity: IInterface; AIntent: Integer);
var
  LIndex: Integer;
begin
  inherited Create;
  FState := AState;
  FStateLease := AStateLease;
  FIdentity := AIdentity;
  FIntent := AIntent;
  FSortOwner := QueryOwnerPresentation(FState.SortOwner, FState.SortLife);
  SetLength(FContributions, Length(FState.Contributions));
  for LIndex := 0 to High(FContributions) do
  begin
    FContributions[LIndex] := CopyQueryContribution(FState.Contributions[LIndex]);
    FContributions[LIndex].Owner := QueryOwnerPresentation(
      FContributions[LIndex].Owner, FState.OwnerFlags[LIndex]);
  end;
end;

function TQuerySessionSnapshot.GetBaseline: TNyxCollectionQuery;
begin
  Result := FState.Baseline.Copy;
end;

function TQuerySessionSnapshot.GetEffective: TNyxCollectionQuery;
begin
  Result := FState.Effective.Copy;
end;

function TQuerySessionSnapshot.GetHasUserSort: Boolean;
begin
  Result := FState.HasUserSort;
end;

function TQuerySessionSnapshot.GetUserSort: TNyxCollectionQuery;
begin
  Result := FState.UserSort.Copy;
end;

function TQuerySessionSnapshot.GetSortOwner: TNyxQueryOwnerInfo;
begin
  Result := FSortOwner;
end;

function TQuerySessionSnapshot.GetCount: Integer;
begin
  Result := Length(FContributions);
end;

function TQuerySessionSnapshot.ContributionAt(AIndex: Integer): TNyxQueryContribution;
begin

  if (AIndex < 0) or (AIndex >= Length(FContributions)) then
  begin
    raise ENyxCollection.Create('Query contribution index out of range');
  end;
  Result := CopyQueryContribution(FContributions[AIndex]);
end;

function TQuerySessionSnapshot.Identity: IInterface;
begin
  Result := FIdentity;
end;

function TQuerySessionSnapshot.Intent: Integer;
begin
  Result := FIntent;
end;

function TQuerySessionSnapshot.Session: TQuerySessionState;
begin
  Result := FState;
end;

function NyxCollectionQuerySession(const AView: INyxCollectionView): INyxCollectionQuerySession;
var
  LSession: INyxCollectionQuerySession;
begin
  LSession := nil;

  if (AView = nil) or not Supports(AView, INyxCollectionQuerySession, LSession) then
  begin
    raise ENyxCollection.Create('Collection view does not support query sessions');
  end;
  Result := LSession;
end;

function PrepareNyxCollectionQueryTransfer(const AFormer,
  ANext: INyxCollectionView): INyxPreparedPublication;
var
  LFormer: INyxCollectionView;
  LNext: INyxCollectionView;
  LFormerSession: INyxCollectionQuerySession;
  LNextSession: INyxCollectionQuerySession;
  LSource: INyxCollectionQuerySessionSnapshot;
  LTarget: INyxCollectionQuerySessionSnapshot;
  LAtomic: INyxAtomicCollection;
begin
  Result := nil;
  LFormer := AFormer;
  LNext := ANext;

  if (LFormer = nil) or (LNext = nil) or (LFormer = LNext) then
  begin
    raise ENyxCollection.Create('Query transfer requires two independent compatible views');
  end;

  if (LFormer.Store <> LNext.Store) or (LFormer.Projection <> LNext.Projection) or
    (LFormer.Spec.ToData.ToJSON <> LNext.Spec.ToData.ToJSON) then
  begin
    raise ENyxCollection.Create('Query transfer requires the same store, projection and specification');
  end;

  { Legacy non-atomic stores keep their original immediate query-copy path.
    Presence of the optional view facet alone cannot promise a source veto. }

  if not Supports(LNext.Store, INyxAtomicCollection, LAtomic) then
  begin
    Exit;
  end;

  if not Supports(LFormer, INyxCollectionQuerySession, LFormerSession) or
    not Supports(LNext, INyxCollectionQuerySession, LNextSession) then
  begin
    Exit;
  end;
  LSource := LFormerSession.Capture;
  LTarget := LNextSession.Capture;
  Result := LNextSession.PrepareImport(LTarget, LSource);
end;

function TView.Capture: INyxCollectionQuerySessionSnapshot;
begin
  Result := TQuerySessionSnapshot.Create(FQuerySession, FQuerySessionLease,
    FQueryIdentity, FQueryIntent);
end;

function TView.AcquireOwner(const AOrigin: TNyxQueryOriginRef;
  const ALabel: TNyxText): INyxCollectionQueryOwner;
var
  LLease: INyxCollectionView;
  LOrigin: TNyxQueryOriginRef;
  LLabel: TNyxText;
  LIndex: Integer;
  LCount: Integer;
  LOwner: TQueryOwnerLease;
  LLife: TQueryOwnerLife;
begin
  LLease := Self;
  LOrigin := NyxQueryOrigin(AOrigin.Name);
  LLabel := NyxCollection(ALabel).Name;
  Writable;
  { The alternate Busy getter may have installed a reservation or acquired a
    peer token. Recheck canonical fields and count the current live ports. }

  if FNotifying or (FPrepared <> nil) or (FQueryPreparation <> nil) then
  begin
    raise ENyxCollection.Create('Cannot acquire a query owner during publication');
  end;
  LCount := 0;
  for LIndex := 0 to High(FQueryOwnerFlags) do
  begin

    if (FQueryOwnerFlags[LIndex] <> nil) and FQueryOwnerFlags[LIndex].Active and
      (FQueryOwnerFlags[LIndex].Scope <> nil) and FQueryOwnerFlags[LIndex].Scope.Active then
    begin
      FQueryOwnerFlags[LCount] := FQueryOwnerFlags[LIndex];
      FQueryOwnerLives[LCount] := FQueryOwnerLives[LIndex];
      Inc(LCount);
    end;
  end;
  SetLength(FQueryOwnerFlags, LCount);
  SetLength(FQueryOwnerLives, LCount);

  if (LCount >= NyxMaximumQuerySessionOwners) or (FNextQueryOwner = High(Integer)) then
  begin
    raise ENyxCollection.Create('Query session owner budget exhausted');
  end;
  LOwner := TQueryOwnerLease.Create;
  Result := LOwner;
  LLife := TQueryOwnerLife.Create;
  LOwner.LifeLease := LLife;
  LOwner.Life := LLife;
  LLife.Scope := FQuerySession.Scope;
  LLife.ScopeLease := FQuerySession.ScopeLease;
  LLife.Active := True;
  LOwner.Info.ID := NyxQueryOwnerID(FNextQueryOwner + 1);
  LOwner.Info.Origin := LOrigin;
  LOwner.Info.Caption := LLabel;
  LOwner.Info.Connected := True;
  SetLength(FQueryOwnerFlags, LCount + 1);
  SetLength(FQueryOwnerLives, LCount + 1);
  FQueryOwnerFlags[LCount] := LLife;
  FQueryOwnerLives[LCount] := LOwner.LifeLease;
  Inc(FNextQueryOwner);
end;

procedure TQueryPreparation.RequireSessionCapture(
  const ACapture: INyxCollectionQuerySessionSnapshot);
var
  LCapture: INyxCollectionQuerySessionSnapshot;
  LProof: IQuerySessionProof;
begin
  LCapture := ACapture;

  if (LCapture = nil) or not Supports(LCapture, IQuerySessionProof, LProof) then
  begin
    raise ENyxCollection.Create('Query command requires its exact session snapshot');
  end;

  if (LProof.Identity <> FOwner.FQueryIdentity) or (LProof.Intent <> FIntent) or
    (LProof.Session <> FBeforeSession) then
  begin
    raise ENyxCollection.Create('Query session snapshot is foreign or stale');
  end;
  LProof := nil;
  LCapture := nil;
  RequireCurrent;
end;

procedure TQueryPreparation.RequireSessionOwner(const AOwner: INyxCollectionQueryOwner);
var
  LProof: IQueryOwnerProof;
  LOwner: INyxCollectionQueryOwner;
  LToken: TQueryOwnerLease;
begin
  LOwner := AOwner;

  if (LOwner = nil) or not Supports(LOwner, IQueryOwnerProof, LProof) then
  begin
    raise ENyxCollection.Create('Query command requires an exact live owner lease');
  end;
  LToken := LProof.OwnerLease;

  if (LToken = nil) or (LToken.Life = nil) then
  begin
    raise ENyxCollection.Create('Query owner proof has no lifetime port');
  end;
  FCommandInfo := LToken.Info;
  FCommandLife := LToken.Life;
  FCommandLifeLease := LToken.LifeLease;
  FHasCommandOwner := True;
  LProof := nil;
  LOwner := nil;
  RequireCurrent;
end;

procedure TQueryPreparation.SetSession(AState: TQuerySessionState;
  const ALease: IInterface);
begin
  AState.Compose;
  FSession := AState;
  FSessionLease := ALease;
  FQuery := AState.Effective.Copy;
  FChanged := FQuery.ToData.ToJSON <> FBeforeQuery.ToData.ToJSON;
  FSessionChanged := not AState.SameValues(FBeforeSession);

  if (FChanged or FSessionChanged) and (FIntent = High(Integer)) then
  begin
    raise ENyxCollection.Create('Collection query intent is exhausted');
  end;
  RequireCurrent;
end;

function TView.PrepareSession(const ACapture: INyxCollectionQuerySessionSnapshot;
  const AOwner: INyxCollectionQueryOwner;
  const AEdits: TNyxQuerySessionEdits): INyxPreparedPublication;
var
  LCapture: INyxCollectionQuerySessionSnapshot;
  LOwner: INyxCollectionQueryOwner;
  LEdits: TNyxQuerySessionEdits;
  LStage: TQueryPreparation;
  LState: TQuerySessionState;
  LStateLease: IInterface;
begin
  LCapture := ACapture;
  LOwner := AOwner;
  LEdits := AEdits.Copy;
  LStage := TQueryPreparation.Create;
  Result := LStage;
  LStage.Initialize(Self, CaptureQuery, FQuery);
  LStage.RequireSessionCapture(LCapture);
  LStage.RequireSessionOwner(LOwner);
  LState := TQuerySessionState.Create;
  LStateLease := LState;
  LState.Assign(FQuerySession);
  LState.Apply(LStage.FCommandInfo, LStage.FCommandLife,
    LStage.FCommandLifeLease, LEdits);
  LStage.SetSession(LState, LStateLease);
end;

function TView.PrepareRebase(const ACapture: INyxCollectionQuerySessionSnapshot;
  const AOwner: INyxCollectionQueryOwner;
  const AAuthoredSpec: TNyxCollectionViewSpec): INyxPreparedPublication;
var
  LCapture: INyxCollectionQuerySessionSnapshot;
  LOwner: INyxCollectionQueryOwner;
  LSpec: TNyxCollectionViewSpec;
  LStage: TQueryPreparation;
  LState: TQuerySessionState;
  LStateLease: IInterface;
begin
  LCapture := ACapture;
  LOwner := AOwner;
  LSpec := AAuthoredSpec.Copy;
  LStage := TQueryPreparation.Create;
  Result := LStage;
  LStage.Initialize(Self, CaptureQuery, FQuery);
  LStage.RequireSessionCapture(LCapture);
  LStage.RequireSessionOwner(LOwner);

  if LSpec.ToData.ToJSON <> FSpec.ToData.ToJSON then
  begin
    raise ENyxCollection.Create('Authored query rebase requires the exact complete view specification');
  end;
  LState := TQuerySessionState.Create;
  LStateLease := LState;
  LState.Assign(FQuerySession);
  LState.Baseline := LSpec.QueryPolicy;
  LStage.SetSession(LState, LStateLease);
end;

function TView.PrepareImport(const ATargetCapture,
  ASourceSnapshot: INyxCollectionQuerySessionSnapshot): INyxPreparedPublication;
var
  LTarget: INyxCollectionQuerySessionSnapshot;
  LSource: INyxCollectionQuerySessionSnapshot;
  LStage: TQueryPreparation;
  LState: TQuerySessionState;
  LStateLease: IInterface;
begin
  LTarget := ATargetCapture;
  LSource := ASourceSnapshot;
  LStage := TQueryPreparation.Create;
  Result := LStage;
  LStage.Initialize(Self, CaptureQuery, FQuery);
  LStage.RequireSessionCapture(LTarget);
  LState := TQuerySessionState.Create;
  LStateLease := LState;
  LStage.FNextOwner := LState.Import(LSource);
  LStage.SetSession(LState, LStateLease);
end;
