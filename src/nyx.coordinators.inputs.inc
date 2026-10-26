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

function TCoordinatorScope.CollectionTarget(const AReference: TNyxControlRef): TNyxControlRef;
var
  LLease: INyxCoordinatorProjection;
  LPrefix: TNyxText;
  LIndex: Integer;
  LNode: TNyxNode;
begin
  LLease := Self;
  CheckLive;

  if FSealed then
  begin
    raise ENyxCoordinator.Create('Collection targets must be discovered before Build');
  end;
  LPrefix := '';
  for LIndex := Length(FRootNode.ID) downto 1 do
  begin

    if FRootNode.ID[LIndex] = '/' then
    begin
      LPrefix := Copy(FRootNode.ID, 1, LIndex - 1);
      Break;
    end;
  end;
  { Same expansion-prefix rule as the existing collection pager. Source slashes
    are escaped by NyxQualifiedID; no global fallback can capture a peer instance. }
  LNode := FTreeNode.Find(NyxQualifiedID(LPrefix, AReference.ID));

  if (LNode = nil) or not LNode.CollectionView.Defined or
    not (NyxCollectionProjectionForNode(LNode) in [cpList, cpTable]) then
  begin
    raise ENyxCoordinator.Create('Collection target is missing, foreign or not a List/Table');
  end;
  LIndex := AddRole(LNode, '');
  FRoles[LIndex].ExternalCollection := FRootNode.Find(LNode.ID) <> LNode;
  Result.ID := LNode.ID;
  CheckNodes;
end;

procedure TCoordinatorScope.ReserveInput(const ARole: TNyxControlRef);
var
  LLease: INyxCoordinatorProjection;
  LRole: TCoordinatorRole;
  LHost: INyxCoordinatorInputHost;
  LIndex: Integer;
  LCount: Integer;
begin
  LLease := Self;
  CheckNodes;

  if FSealed then
  begin
    raise ENyxCoordinator.Create('Input roles must be reserved before Build');
  end;
  LRole := FRoles[RoleIndex(ARole)];

  if LRole.ExternalCollection or not NyxNodeValueDomain(LRole.Node).Defined then
  begin
    raise ENyxCoordinator.Create('Only a local declared scalar role grants input authority');
  end;

  if not Supports(FTarget, INyxCoordinatorInputHost, LHost) then
  begin
    raise ENyxCoordinator.Create('This host does not provide scoped scalar commands');
  end;
  FInputHost := LHost;
  LCount := 0;
  for LIndex := 0 to High(FRoles) do
  begin

    if FRoles[LIndex].InputClaimed then
    begin
      Inc(LCount);
    end;
  end;

  if not LRole.InputClaimed and (LCount >= NyxMaximumInputValues) then
  begin
    raise ENyxCoordinator.Create('Too many reserved scalar roles');
  end;
  LRole.InputClaimed := True;
  CheckNodes;
end;

function TCoordinatorScope.ValueDomain(const ARole: TNyxControlRef): TNyxValueDomain;
var
  LLease: INyxCoordinatorProjection;
  LRole: TCoordinatorRole;
begin
  LLease := Self;
  CheckNodes;
  LRole := FRoles[RoleIndex(ARole)];

  if LRole.ExternalCollection then
  begin
    raise ENyxCoordinator.Create('Collection discovery grants no scalar inspection');
  end;
  Result := NyxNodeValueDomain(LRole.Node).Copy;
end;

function TCoordinatorScope.StoredValue(const ARole: TNyxControlRef;
  out AValue: TNyxStateValue): Boolean;
var
  LLease: INyxCoordinatorProjection;
  LRole: TCoordinatorRole;
  LDomain: TNyxValueDomain;
  LData: TNyxDataValue;
begin
  LLease := Self;
  CheckNodes;
  AValue := TNyxStateValue.FromText('');
  LRole := FRoles[RoleIndex(ARole)];
  LDomain := ValueDomain(ARole);

  if not LDomain.Defined then
  begin
    raise ENyxCoordinator.Create('The role has no scalar value domain');
  end;
  Result := LRole.Node.Props.IndexOfName(NyxAttributeName(atValue)) >= 0;

  if not Result then
  begin
    Exit;
  end;
  LData := LDomain.ReadWire(LRole.Node.Prop(NyxAttributeName(atValue)));
  case LDomain.Kind of
    nskText:
      begin
        AValue := TNyxStateValue.FromText(LData.AsText);
      end;
    nskBoolean:
      begin
        AValue := TNyxStateValue.FromBoolean(LData.AsBoolean);
      end;
    nskInteger:
      begin
        AValue := TNyxStateValue.FromInteger(LData.AsInteger);
      end;
    nskNumber:
      begin
        AValue := TNyxStateValue.FromNumber(LData.AsNumber);
      end;
  end;
end;

function TCoordinatorScope.InputCommands: INyxInputCommands;
begin
  CheckLive;

  if not Supports(FPort, INyxInputCommands, Result) then
  begin
    raise ENyxCoordinator.Create('The scalar scope port has no input commands');
  end;
end;

procedure TCoordinatorScope.CheckInput(const AControl: TNyxControlRef);
begin

  if not FRoles[RoleIndex(AControl)].InputClaimed then
  begin
    raise ENyxCoordinator.Create('This scalar role was not reserved by the coordinator');
  end;
end;

function TCoordinatorScope.LiveInputs: INyxInputCommands;
var
  LHost: INyxCoordinatorInputHost;
begin
  CheckLive(True);
  CheckNodes;

  if not Supports(FInputHost, INyxCoordinatorInputHost, LHost) then
  begin
    raise ENyxCoordinatorCancelled.Create('The coordinator has no live scalar host');
  end;
  Result := LHost.InputsFor(FTreeNode);
  CheckLive(True);

  if (Result = nil) or not Result.Connected then
  begin
    raise ENyxCoordinatorCancelled.Create('The exact coordinator tree has no adopted input owner');
  end;
end;

function TCoordinatorPort.GetConnected: Boolean;
var
  LLease: INyxCoordinatorProjection;
begin
  Result := False;
  LLease := Acquire;

  if (LLease = nil) or (FOwner = nil) then
  begin
    Exit;
  end;
  try
    Result := FOwner.LiveInputs <> nil;
  except
    on ENyxCoordinator do
    begin
      Result := False;
    end;
  end;
end;

function TCoordinatorPort.Capture(
  const AControls: array of TNyxControlRef): INyxInputCapture;
var
  LLease: INyxCoordinatorProjection;
  LOwner: TCoordinatorScope;
  LCommands: INyxInputCommands;
  LIndex: Integer;
  LCapture: TScopedInputCapture;
begin
  LLease := Acquire;
  LOwner := FOwner;

  if (LLease = nil) or (LOwner = nil) then
  begin
    raise ENyxCoordinatorCancelled.Create('Scalar coordinator is retired');
  end;
  LCommands := LOwner.LiveInputs;
  for LIndex := 0 to High(AControls) do
  begin
    LOwner.CheckInput(AControls[LIndex]);
  end;
  LCapture := TScopedInputCapture.Create;
  Result := LCapture;
  LCapture.Port := LOwner.FPort;
  LCapture.Commands := LCommands;
  LCapture.Inner := LCommands.Capture(AControls);
  LOwner.CheckNodes;
end;

function TCoordinatorPort.Prepare(const ACapture: INyxInputCapture;
  const AValues: array of TNyxInputValue): INyxPreparedPublication;
var
  LLease: INyxCoordinatorProjection;
  LCaptureLease: INyxInputCapture;
  LProof: IScopedInputProof;
  LObject: TObject;
  LCapture: TScopedInputCapture;
  LOwner: TCoordinatorScope;
  LCommands: INyxInputCommands;
  LInner: INyxPreparedPublication;
  LIndex: Integer;
begin
  LLease := Acquire;
  LOwner := FOwner;
  LCaptureLease := ACapture;

  if (LLease = nil) or (LOwner = nil) or
    not Supports(LCaptureLease, IScopedInputProof, LProof) then
  begin
    raise ENyxCoordinatorCancelled.Create('Scalar preparation requires this live scope capture');
  end;
  LObject := LProof.SnapshotObject;

  if not (LObject is TScopedInputCapture) or
    ((TScopedInputCapture(LObject) as INyxInputCapture) <> LCaptureLease) then
  begin
    raise ENyxCoordinator.Create('Foreign scoped input proof');
  end;
  LCapture := TScopedInputCapture(LObject);
  LCommands := LOwner.LiveInputs;

  if (LCapture.Port <> LOwner.FPort) or (LCapture.Commands <> LCommands) then
  begin
    raise ENyxCoordinatorCancelled.Create('Scalar capture belongs to another scope or view');
  end;
  for LIndex := 0 to High(AValues) do
  begin
    LOwner.CheckInput(AValues[LIndex].Control);
  end;
  LInner := LCommands.Prepare(LCapture.Inner, AValues);
  LOwner.CheckNodes;
  Result := TScopedInputStage.Create(LOwner.FPort, LCommands, LInner);
end;

function TScopedInputCapture.SnapshotObject: TObject;
begin
  Result := Self;
end;

function TScopedInputCapture.GetCount: Integer;
begin
  Result := Inner.Count;
end;

function TScopedInputCapture.ControlAt(AIndex: Integer): TNyxControlRef;
begin
  Result.ID := Inner.ControlAt(AIndex).ID;
end;

function TScopedInputCapture.HasValueAt(AIndex: Integer): Boolean;
begin
  Result := Inner.HasValueAt(AIndex);
end;

function TScopedInputCapture.ValueAt(AIndex: Integer): TNyxStateValue;
begin
  Result := Inner.ValueAt(AIndex).Copy;
end;

constructor TScopedInputStage.Create(const APort: ICoordinatorPort;
  const ACommands: INyxInputCommands; const AInner: INyxPreparedPublication);
begin
  inherited Create;
  FPort := APort;
  FCommands := ACommands;
  FInner := AInner;

  if not Supports(FInner, INyxPublicationReadiness, FFence) then
  begin
    raise ENyxCoordinator.Create('Scalar publication has no final readiness');
  end;
  CheckScope;
end;

destructor TScopedInputStage.Destroy;
begin
  Retire;
  inherited Destroy;
end;

procedure TScopedInputStage.CheckScope;
var
  LLease: INyxCoordinatorProjection;
  LOwner: TCoordinatorScope;
begin

  if FRetired then
  begin
    raise ENyxCoordinatorCancelled.Create('Scalar scope publication is retired');
  end;
  LLease := FPort.Acquire;
  LOwner := FPort.Owner;

  if (LLease = nil) or (LOwner = nil) or (LOwner.LiveInputs <> FCommands) then
  begin
    raise ENyxCoordinatorCancelled.Create('Scalar coordinator scope is no longer current');
  end;
end;

procedure TScopedInputStage.Validate;
begin
  CheckScope;
  FInner.Validate;
  CheckScope;
end;

procedure TScopedInputStage.CheckReady;
begin
  CheckScope;
  FFence.CheckReady;
end;

procedure TScopedInputStage.Install;
begin
  FInner.Install;
end;

procedure TScopedInputStage.Notify;
begin
  FInner.Notify;
end;

procedure TScopedInputStage.Retire;
var
  LInner: INyxPreparedPublication;
begin

  if FRetired then
  begin
    Exit;
  end;
  FRetired := True;
  LInner := FInner;
  FInner := nil;
  FFence := nil;
  FPort := nil;
  FCommands := nil;

  if LInner <> nil then
  begin
    LInner.Retire;
  end;
end;
