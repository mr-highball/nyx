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

function SameInputProperties(AFirst, ASecond: TNyxStrings): Boolean;
var
  LIndex: Integer;
begin
  Result := False;

  if AFirst.Count <> ASecond.Count then
  begin
    Exit;
  end;
  for LIndex := 0 to AFirst.Count - 1 do
  begin

    if AFirst[LIndex] <> ASecond[LIndex] then
    begin
      Exit;
    end;
  end;
  Result := True;
end;

{ A declared integral binding preserves its narrower family within a numeric
  domain. Resource/computed declarations remain observations, never writable
  state authority. An absent domain grants no generic scalar command. }
function InputScalarKind(ANode: TNyxNode): TNyxStateKind;
var
  LDomain: TNyxValueDomain;
  LIndex: Integer;
  LSpec: TNyxBindingSpec;
begin
  LDomain := NyxNodeValueDomain(ANode);

  if not LDomain.Defined then
  begin
    raise ENyxState.Create('Input capture requires a declared scalar value');
  end;
  Result := LDomain.Kind;
  for LIndex := 0 to ANode.BindingCount - 1 do
  begin
    LSpec := ANode.Bindings[LIndex];

    if not LSpec.Cleared and (LSpec.Target = bpValue) then
    begin
      Result := LSpec.ValueKind;
      Exit;
    end;
  end;
end;

function TBindingResourcePort.GetConnected: Boolean;
begin
  Result := (FOwner <> nil) and (FOwner.FSubscription <> nil) and
    FOwner.FSubscription.Connected;
end;

function TBindingResourcePort.Capture(
  const AControls: array of TNyxControlRef): INyxInputCapture;
begin

  if not GetConnected or not FOwner.RefreshReady then
  begin
    raise ENyxState.Create('Input capture requires active idle bindings');
  end;
  Result := TBindingInputCapture.Create(FOwner, AControls);
end;

function TBindingResourcePort.Prepare(const ACapture: INyxInputCapture;
  const AValues: array of TNyxInputValue): INyxPreparedPublication;
var
  LProjection: TBindingInputPreparation;
  LLease: INyxPreparedPublication;
begin

  if not GetConnected or not FOwner.RefreshReady then
  begin
    raise ENyxState.Create('Input preparation requires active idle bindings');
  end;
  LProjection := TBindingInputPreparation.Create(FOwner, ACapture, AValues);
  LLease := LProjection;
  { Both participants belong to the existing publication owner. State observers
    and target sync each receive their attempt after every outer query installs;
    the first notification failure is preserved by the ordinary group. }
  Result := PrepareNyxGroup([LProjection.FStateStage, LLease]);
end;

constructor TBindingInputCapture.Create(AOwner: TNyxLiveBindings;
  const AControls: array of TNyxControlRef);
var
  LIndex: Integer;
  LPrevious: Integer;
  LBinding: Integer;
  LNode: TNyxNode;
begin
  inherited Create;

  if (Length(AControls) = 0) or
    (Length(AControls) > NyxMaximumInputValues) then
  begin
    raise ENyxState.Create('Input capture requires 1..64 distinct controls');
  end;
  FPort := AOwner.FResourcePort as IBindingResourcePort;
  FSerial := AOwner.FInputSerial;
  FStateRevision := AOwner.FState.Revision;
  SetLength(FControls, Length(AControls));
  SetLength(FPresent, Length(AControls));
  SetLength(FValues, Length(AControls));
  SetLength(FKinds, Length(AControls));
  SetLength(FProps, Length(AControls));
  SetLength(FContracts, Length(AControls));
  SetLength(FBindings, Length(AControls));
  SetLength(FPolicies, Length(AControls));
  for LIndex := 0 to High(AControls) do
  begin

    if AControls[LIndex].ID = '' then
    begin
      raise ENyxState.Create('Input capture requires an exact runtime identity');
    end;
    for LPrevious := 0 to LIndex - 1 do
    begin

      if AControls[LPrevious].ID = AControls[LIndex].ID then
      begin
        raise ENyxState.Create('Input capture contains a duplicate role');
      end;
    end;
    LNode := AOwner.FRoot.Find(AControls[LIndex].ID);

    if LNode = nil then
    begin
      raise ENyxState.Create('Input capture cannot find its mounted control');
    end;
    FControls[LIndex].ID := AControls[LIndex].ID;
    FKinds[LIndex] := InputScalarKind(LNode);
    FProps[LIndex] := TNyxStrings.Create;
    FProps[LIndex].Assign(LNode.Props);
    FPresent[LIndex] := LNode.Props.IndexOfName(NyxAttributeName(atValue)) >= 0;

    if FPresent[LIndex] then
    begin
      FValues[LIndex] := ReadScalar(LNode.Prop(NyxAttributeName(atValue)), FKinds[LIndex]);
    end;
    FContracts[LIndex] := LNode.Contract.Snapshot.ToJSON;
    SetLength(FBindings[LIndex], LNode.BindingCount);
    for LBinding := 0 to LNode.BindingCount - 1 do
    begin
      FBindings[LIndex][LBinding] := LNode.Bindings[LBinding].Copy;
    end;
    FPolicies[LIndex] := NyxInteractionPolicy(LNode);
  end;

  if Assigned(AOwner.FOnInputCapture) then
  begin
    FPhysical := AOwner.FOnInputCapture(AControls);

    if FPhysical = nil then
    begin
      raise ENyxState.Create('The target refused physical input capture');
    end;
  end;
  RequireCurrent(AOwner);
end;

destructor TBindingInputCapture.Destroy;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FProps) do
  begin
    FProps[LIndex].Free;
  end;
  inherited Destroy;
end;

procedure TBindingInputCapture.CheckIndex(AIndex: Integer);
begin

  if (AIndex < 0) or (AIndex >= Length(FControls)) then
  begin
    raise ENyxState.Create('Input capture index is out of range');
  end;
end;

function TBindingInputCapture.SnapshotObject: TObject;
begin
  Result := Self;
end;

function TBindingInputCapture.GetCount: Integer;
begin
  Result := Length(FControls);
end;

function TBindingInputCapture.ControlAt(AIndex: Integer): TNyxControlRef;
begin
  CheckIndex(AIndex);
  Result.ID := FControls[AIndex].ID;
end;

function TBindingInputCapture.HasValueAt(AIndex: Integer): Boolean;
begin
  CheckIndex(AIndex);
  Result := FPresent[AIndex];
end;

function TBindingInputCapture.ValueAt(AIndex: Integer): TNyxStateValue;
begin
  CheckIndex(AIndex);

  if not FPresent[AIndex] then
  begin
    raise ENyxState.Create('Captured input has no explicit scalar value');
  end;
  Result := FValues[AIndex].Copy;
end;

procedure TBindingInputCapture.RequireCurrent(AOwner: TNyxLiveBindings);
var
  LIndex: Integer;
  LBinding: Integer;
  LNode: TNyxNode;
  LPolicy: TNyxInteractionPolicy;
begin

  if (AOwner = nil) or (FPort.Owner <> AOwner) or
    (AOwner.FSubscription = nil) or not AOwner.FSubscription.Connected then
  begin
    raise ENyxState.Create('Input capture belongs to a retired or foreign view');
  end;

  if (AOwner.FInputSerial <> FSerial) or
    (AOwner.FState.Revision <> FStateRevision) then
  begin
    raise ENyxState.Create('Input capture is stale');
  end;
  for LIndex := 0 to High(FControls) do
  begin
    LNode := AOwner.FRoot.Find(FControls[LIndex].ID);

    if (LNode = nil) or not SameInputProperties(FProps[LIndex], LNode.Props) or
      (LNode.BindingCount <> Length(FBindings[LIndex])) or
      (LNode.Contract.Snapshot.ToJSON <> FContracts[LIndex]) or
      (InputScalarKind(LNode) <> FKinds[LIndex]) then
    begin
      raise ENyxState.Create('Input role or contract changed since capture');
    end;
    for LBinding := 0 to LNode.BindingCount - 1 do
    begin

      if not LNode.Bindings[LBinding].Same(FBindings[LIndex][LBinding]) then
      begin
        raise ENyxState.Create('Input binding changed since capture');
      end;
    end;

    if FPresent[LIndex] and not
      ReadScalar(LNode.Prop(NyxAttributeName(atValue)), FKinds[LIndex]).SameValue(FValues[LIndex]) then
    begin
      raise ENyxState.Create('Input projection changed since capture');
    end;
    { Runtime constraint getters are pure, synchronous facts by their public
      policy contract. No handler, validator or expression is executed here. }
    LPolicy := NyxInteractionPolicy(LNode);

    if (LPolicy.Enabled <> FPolicies[LIndex].Enabled) or
      (LPolicy.Visible <> FPolicies[LIndex].Visible) or
      (LPolicy.ReadOnly <> FPolicies[LIndex].ReadOnly) then
    begin
      raise ENyxState.Create('Input interaction policy changed since capture');
    end;
  end;

  if FPort.Owner <> AOwner then
  begin
    raise ENyxState.Create('Input owner retired during capture admission');
  end;

  if FPhysical <> nil then
  begin
    FPhysical.CheckReady;
  end;
end;

{ Compare canonical owned projection facts without constructing another tree.
  A later validator cannot silently change an unrelated field, binding or domain
  and have the detached whole-view projection overwrite that newer meaning. }
function SameInputBaseline(AExisting, ABefore: TNyxNode): Boolean;
var
  LIndex: Integer;
begin
  Result := False;

  if (AExisting.ID <> ABefore.ID) or (AExisting.Kind <> ABefore.Kind) or
    (AExisting.DesignID <> ABefore.DesignID) or
    (AExisting.Count <> ABefore.Count) or
    not SameInputProperties(AExisting.Props, ABefore.Props) or
    (AExisting.BindingCount <> ABefore.BindingCount) or
    (AExisting.Contract.Snapshot.ToJSON <> ABefore.Contract.Snapshot.ToJSON) then
  begin
    Exit;
  end;
  for LIndex := 0 to AExisting.BindingCount - 1 do
  begin

    if not AExisting.Bindings[LIndex].Same(ABefore.Bindings[LIndex]) then
    begin
      Exit;
    end;
  end;
  for LIndex := 0 to AExisting.Count - 1 do
  begin

    if not SameInputBaseline(AExisting.Children[LIndex], ABefore.Children[LIndex]) then
    begin
      Exit;
    end;
  end;
  Result := True;
end;

constructor TBindingInputPreparation.Create(AOwner: TNyxLiveBindings;
  const ACapture: INyxInputCapture; const AValues: array of TNyxInputValue);
var
  LProof: IBindingInputProof;
  LObject: TObject;
  LCapture: TBindingInputCapture;
  LIndex: Integer;
  LPrevious: Integer;
  LSlot: Integer;
  LBinding: Integer;
  LAssignment: Integer;
  LNode: TNyxNode;
  LValue: TNyxStateValue;
  LWire: TNyxText;
  LProjected: TNyxText;
  LSpec: TNyxBindingSpec;
  LState: TNyxState;
  LAssignments: array of TNyxStateAssignment;

  procedure CheckRanges(ANode: TNyxNode);
  var
    LChild: Integer;
  begin
    ValidateNyxDateRangeValues(ANode);
    for LChild := 0 to ANode.Count - 1 do
    begin
      CheckRanges(ANode.Children[LChild]);
    end;
  end;

begin
  inherited Create;
  LAssignments := nil;

  if (Length(AValues) = 0) or (Length(AValues) > NyxMaximumInputValues) or
    (ACapture = nil) or not Supports(ACapture, IBindingInputProof, LProof) then
  begin
    raise ENyxState.Create('Input preparation requires a canonical capture and 1..64 values');
  end;
  LObject := LProof.SnapshotObject;

  if not (LObject is TBindingInputCapture) or
    ((TBindingInputCapture(LObject) as INyxInputCapture) <> ACapture) then
  begin
    raise ENyxState.Create('Foreign input snapshot proof');
  end;
  LCapture := TBindingInputCapture(LObject);
  LCapture.RequireCurrent(AOwner);

  if not AOwner.RefreshReady or (AOwner.FInputSerial = High(Integer)) then
  begin
    raise ENyxState.Create('Input preparation requires an idle, unexhausted owner');
  end;
  FPort := AOwner.FResourcePort as IBindingResourcePort;
  FCapture := ACapture;
  FSnapshot := LCapture;
  AOwner.FResourcePreparation := Self;
  FBefore := AOwner.FRoot.CloneRuntimeProjection;
  FCandidate := AOwner.FRoot.CloneRuntimeProjection;
  SetLength(FResetControls, Length(AValues));
  for LIndex := 0 to High(AValues) do
  begin
    FResetControls[LIndex].ID := AValues[LIndex].Control.ID;
    LSlot := -1;
    for LPrevious := 0 to LCapture.GetCount - 1 do
    begin

      if LCapture.FControls[LPrevious].ID = AValues[LIndex].Control.ID then
      begin
        LSlot := LPrevious;
        Break;
      end;
    end;

    if LSlot < 0 then
    begin
      raise ENyxState.Create('Input assignment was not captured in this scope');
    end;
    for LPrevious := 0 to LIndex - 1 do
    begin

      if AValues[LPrevious].Control.ID = AValues[LIndex].Control.ID then
      begin
        raise ENyxState.Create('Input assignment contains a duplicate control');
      end;
    end;
    LNode := AOwner.FRoot.Find(AValues[LIndex].Control.ID);

    if not NyxInteractionPolicy(LNode).CanEditValue then
    begin
      raise ENyxInputAssignmentRefused.Create(nirkInteraction, AValues[LIndex].Control,
        'The input is read-only, disabled, or hidden.');
    end;
    LValue := AValues[LIndex].Value;

    if (LValue.Kind <> LCapture.FKinds[LSlot]) and not
      ((LValue.Kind = nskInteger) and (LCapture.FKinds[LSlot] = nskNumber)) then
    begin
      raise ENyxState.Create('Input assignment has the wrong scalar family');
    end;
    LWire := ScalarText(LValue);
    LProjected := InputProjectionText(LNode, LValue);
    for LBinding := 0 to LNode.BindingCount - 1 do
    begin
      LSpec := LNode.Bindings[LBinding];

      if LSpec.Cleared or (LSpec.Target <> bpValue) then
      begin
        Continue;
      end;

      if (LSpec.Source <> bsState) or (LSpec.Direction <> bdTwoWay) then
      begin

        if LNode.Prop(NyxAttributeName(atValue)) <> LProjected then
        begin
          raise ENyxInputAssignmentRefused.Create(nirkProjection, AValues[LIndex].Control,
            'The value is supplied by a one-way or resource binding.');
        end;
        Continue;
      end;
      LValue := ReadScalar(LWire, LSpec.ValueKind);
      LAssignment := 0;
      while (LAssignment < Length(LAssignments)) and
        (LAssignments[LAssignment].Key <> LSpec.StateName) do
      begin
        Inc(LAssignment);
      end;

      if LAssignment < Length(LAssignments) then
      begin

        if not LAssignments[LAssignment].Value.SameValue(LValue) then
        begin
          raise ENyxInputAssignmentRefused.Create(nirkConflictingState, AValues[LIndex].Control,
            'These inputs require different values for the same bound state.');
        end;
      end
      else
      begin
        SetLength(LAssignments, LAssignment + 1);
        LAssignments[LAssignment] := NyxStateAssign(LSpec.StateName, LValue);
      end;
    end;
    FCandidate.Find(AValues[LIndex].Control.ID).SetProp(NyxAttributeName(atValue), LProjected);
  end;
  LCapture.RequireCurrent(AOwner);
  { The existing state stage reserves its exact revision before expressions and
    whole-view domain validation. Its candidate remains private to that owner;
    an independently cloned state supplies the same admitted values here. }
  FStateStage := AOwner.FState.PrepareApply(LAssignments, LCapture.FStateRevision);
  AOwner.FCommandRoot := FCandidate;
  LState := AOwner.FState.Clone;
  try
    LState.Apply(LAssignments);
    ApplyNyxBindings(FCandidate, LState);
    { Bounds may themselves be bound to another value in this same batch.
      Admit the complete prospective projection, not yesterday's minimum. }
    for LIndex := 0 to High(AValues) do
    begin
      LNode := FCandidate.Find(AValues[LIndex].Control.ID);
      CheckScalarRange(LNode, ReadScalar(LNode.Prop(NyxAttributeName(atValue)),
        InputScalarKind(LNode)));
    end;
    CheckRanges(FCandidate);
  finally
    LState.Free;
  end;
  RequireOwner;
end;

destructor TBindingInputPreparation.Destroy;
begin
  Retire;
  inherited Destroy;
end;

procedure TBindingInputPreparation.RequireOwner;
var
  LOwner: TNyxLiveBindings;
begin

  if FRetired or (FPort = nil) then
  begin
    raise ENyxState.Create('Input preparation is retired');
  end;
  LOwner := FPort.Owner;

  if (LOwner = nil) or (LOwner.FResourcePreparation <> Self) then
  begin
    raise ENyxState.Create('Input preparation lost its live view');
  end;

  if FInstalled then
  begin
    raise ENyxState.Create('Input preparation is already installed');
  end;
  FSnapshot.RequireCurrent(LOwner);

  if not SameInputBaseline(LOwner.FRoot, FBefore) then
  begin
    raise ENyxState.Create('Input baseline changed during prepared admission');
  end;
end;

procedure TBindingInputPreparation.Validate;
begin
  RequireOwner;
  FValidated := True;
end;

procedure TBindingInputPreparation.CheckReady;
begin
  RequireOwner;

  if not FValidated then
  begin
    raise ENyxState.Create('Input preparation has not been validated');
  end;
end;

procedure TBindingInputPreparation.Install;
begin
  { Every outer participant has validated and fenced. Preserve both the live
    node identity and the displaced property storage until retirement. }
  FPort.Owner.FRoot.ExchangeResourceProjection(FCandidate);
  FPort.Owner.FCommandRoot := nil;
  Inc(FPort.Owner.FInputSerial);
  FInstalled := True;
end;

procedure TBindingInputPreparation.Notify;
var
  LOwner: TNyxLiveBindings;
begin

  if not FInstalled or FRetired or FNotified then
  begin
    Exit;
  end;
  FNotified := True;
  LOwner := FPort.Owner;

  if LOwner <> nil then
  begin

    if Assigned(LOwner.FOnInputSync) then
    begin
      LOwner.FOnInputSync(FResetControls);
    end
    else
    begin
      LOwner.Synchronize;
    end;
  end;
end;

procedure TBindingInputPreparation.Retire;
var
  LOwner: TNyxLiveBindings;
begin

  if FRetired then
  begin
    Exit;
  end;
  FRetired := True;

  if FPort <> nil then
  begin
    LOwner := FPort.Owner;

    if (LOwner <> nil) and (LOwner.FResourcePreparation = Self) then
    begin

      if LOwner.FCommandRoot = FCandidate then
      begin
        LOwner.FCommandRoot := nil;
      end;
      LOwner.FResourcePreparation := nil;
    end;
  end;
  FreeAndNil(FCandidate);
  FreeAndNil(FBefore);
  FStateStage := nil;
  FSnapshot := nil;
  FCapture := nil;
  FPort := nil;
end;
