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

function TControlWires.PortID(AIndex: Integer): TNyxText;
begin
  Result := FID + '-port-' + TNyxText(IntToStr(AIndex));
end;

function TControlWires.FormFor(ANode: TNyxNode): TNyxNode;
begin
  Result := ANode;
  while (Result <> nil) and not Result.Extensions.Has(NyxExtension(CWireForm)) do
  begin
    Result := Result.Parent;
  end;

  if (Result <> nil) and
    (Result.Extensions.Value(NyxExtension(CWireForm)).AsInteger <> FEpoch) then
  begin
    Result := nil;
  end;
end;

function TControlWires.BuildControl(const AControl: TNyxControlRef): INyxRow;
var
  LIndex: Integer;
  LButton: INyxButton;
  LCaption: TNyxText;
  LControlIndex: Integer;
begin
  RequireActive;

  if FGraphMode then
  begin
    raise ENyxFlow.Create('Return to control connections before composing control strips');
  end;
  LControlIndex := -1;
  for LIndex := 0 to High(FControls) do
  begin

    if FControls[LIndex].ID = AControl.ID then
    begin
      LControlIndex := LIndex;
      Break;
    end;
  end;

  if LControlIndex < 0 then
  begin
    raise ENyxFlow.Create('Choose an exact discovered control for its connector face');
  end;
  Result := NewNyxRow(FID + '-control-' + TNyxText(IntToStr(LControlIndex)));
  Result.Configure.Padding(0).Gap(4).Wrap(nfwWrap).Done;
  Result.Node.Extensions.SetValue(NyxExtension(CWireForm), NyxData(FEpoch));
  for LIndex := 0 to High(FPorts) do
  begin

    if (FPorts[LIndex].Kind <> ntwpkControl) or
      (FPorts[LIndex].ControlPort.Control.ID <> AControl.ID) then
    begin
      Continue;
    end;
    LCaption := 'Out';

    if FPorts[LIndex].Direction = nfdInput then
    begin
      LCaption := 'In';
    end;
    LButton := NewNyxButton(PortID(LIndex));
    LButton.Configure.Text(LCaption).Width(44).Height(32)
      .AccessibleName(AControl.ID + ': ' + FPorts[LIndex].Title +
        ' / ' + NyxFlowTypeName(FPorts[LIndex].ValueType))
      .Hint(FPorts[LIndex].Title + ' / ' + NyxFlowTypeName(FPorts[LIndex].ValueType))
      .TouchBehavior(ntbNone)
      .Enabled(not GetStale).Done;

    if (LIndex = FSource) or (LIndex = FTarget) then
    begin
      LButton.Configure.Variant(nvPrimary).Done;
    end;
    Result.Add(LButton);
  end;
end;

function TControlWires.BuildPipeline: INyxCard;
var
  LIndex: Integer;
  LFlows: INyxFlowDeclarations;
  LList: TNyxText;
  LButton: INyxButton;
  LColumns: INyxRow;
  LInputs: INyxColumn;
  LOutputs: INyxColumn;
  LActions: INyxRow;
  LMoreActions: INyxRow;
  LOptions: INyxDisclosure;
begin

  if FGraphMode then
  begin
    Exit(BuildGraph);
  end;
  RequireActive;
  Result := NewNyxCard(FID + '-pipeline');
  Result.Configure.Padding(12).Gap(8).Done;
  Result.Node.Extensions.SetValue(NyxExtension(CWireForm), NyxData(FEpoch));
  LList := FFlow.Name;

  if LList = '' then
  begin
    LList := 'Control connections';
  end;
  Result.Add(NewNyxButton(FID + '-title').Configure.Text(LList).PartName(NyxPart('move'))
    .Hint('Drag to move this panel. Arrow keys move 8 pixels, Shift+arrows 32; Home restores its position.')
    .TouchBehavior(ntbNone).Done);
  LOptions := NewNyxDisclosure(FID + '-options').WithText('Connection options');
  LOptions.Configure.Value(FOptionsExpanded).Surface(False).Padding(0).Gap(8).Done;
  LOptions.BodyColumn.Add(NewNyxLabel(FID + '-help').WithText(
    'Drag matching ports, or choose Out and In with Tab and Enter. Stage wires, then Apply.'));
  LFlows := FFlowEditing.RuntimeFlows;
  LList := '';
  for LIndex := 0 to LFlows.Count - 1 do
  begin

    if LIndex > 0 then
    begin
      LList := LList + #10;
    end;
    LList := LList + LFlows.Reference(LIndex).Name;
  end;
  LOptions.BodyColumn.Add(NewNyxSelect(FID + '-flow').Configure.Text('Pipeline')
    .Items(LList).Value(FFlow.Name).Enabled(not GetStale).Done);
  LColumns := NewNyxRow(FID + '-ports');
  LColumns.Configure.Gap(8).Wrap(nfwWrap).Done;
  LInputs := NewNyxColumn(FID + '-inputs');
  LOutputs := NewNyxColumn(FID + '-outputs');
  LInputs.Configure.Gap(4).Done;
  LOutputs.Configure.Gap(4).Done;
  LInputs.Add(NewNyxLabel(FID + '-input-title').WithText('Pipeline inputs'));
  LOutputs.Add(NewNyxLabel(FID + '-output-title').WithText('Pipeline outputs'));
  for LIndex := 0 to High(FPorts) do
  begin

    if FPorts[LIndex].Kind <> ntwpkPipeline then
    begin
      Continue;
    end;
    LButton := NewNyxButton(PortID(LIndex));
    LList := FPorts[LIndex].Endpoint.Port.Name;

    if FDefinition.StepCount > 1 then
    begin
      LList := FPorts[LIndex].Title;
    end;
    LButton.Configure.Text(LList + ' / ' + NyxFlowTypeName(FPorts[LIndex].ValueType))
      .Hint(FPorts[LIndex].Title + ': ' + FPorts[LIndex].Description)
      .TouchBehavior(ntbNone)
      .Enabled(not GetStale).Done;

    if (LIndex = FSource) or (LIndex = FTarget) then
    begin
      LButton.Configure.Variant(nvPrimary).Done;
    end;

    if FPorts[LIndex].Direction = nfdInput then
    begin
      LInputs.Add(LButton);
    end
    else
    begin
      LOutputs.Add(LButton);
    end;
  end;
  LColumns.Add(LInputs);
  LColumns.Add(LOutputs);
  Result.Add(LColumns);
  Result.Add(NewNyxButton(ModeControl.ID).WithText('Show operation nodes'));
  LOptions.BodyColumn.Add(NewNyxInput(FID + '-name').Configure.Text('Route name').Value(FName).Done);
  LOptions.BodyColumn.Add(NewNyxSwitch(FID + '-confirm').Configure.Text(
    'Confirm replacing, removing, or discarding the selected work').Value(FConfirmed).Done);
  LActions := NewNyxRow(FID + '-actions');
  LActions.Configure.Gap(4).Wrap(nfwWrap).Done;
  LActions.Add(NewNyxButton(FID + '-connect').WithText('Stage selected'));
  LActions.Add(NewNyxButton(FID + '-apply').Configure.Text('Apply route').Variant(nvPrimary).Done);
  Result.Add(LActions);
  LMoreActions := NewNyxRow(FID + '-more-actions');
  LMoreActions.Configure.Gap(4).Wrap(nfwWrap).Done;
  LMoreActions.Add(NewNyxButton(FID + '-reload').WithText('Reload'));
  LMoreActions.Add(NewNyxButton(FID + '-remove-applied').Configure.Text('Remove applied route')
    .Enabled(FAppliedID.Name <> '').Done);
  LOptions.BodyColumn.Add(LMoreActions);
  for LIndex := 0 to High(FWires) do
  begin
    LButton := NewNyxButton(FID + '-wire-' + TNyxText(IntToStr(LIndex))).WithText(
      FPorts[FWires[LIndex].Source].Title + ' -> ' + FPorts[FWires[LIndex].Target].Title);

    if LIndex = FSelectedWire then
    begin
      LButton.Configure.Variant(nvPrimary).Done;
    end;
    LOptions.BodyColumn.Add(LButton);
  end;
  LOptions.BodyColumn.Add(NewNyxButton(FID + '-remove-staged').Configure.Text('Remove selected staged wire')
    .Enabled(FSelectedWire >= 0).Done);
  Result.Add(LOptions);

  if GetStale then
  begin
    Result.Add(NewNyxLabel(FID + '-stale').WithText(
      'The canvas changed. Your proposal is retained; confirm discarding it before Reload.'));
  end;
  Result.Add(NewNyxLabel(FID + '-message').WithText(FMessage));
end;

function TControlWires.Handle(ANode: TNyxNode; const AEvent: TNyxEventInfo;
  const AResponse: INyxGestureResponse): Boolean;
var
  LLease: INyxToolControlWires;
  LIndex: Integer;
  LCompatible: Boolean;
  LForm: TNyxNode;
  LOptions: TNyxNode;
begin
  RequireActive;
  LLease := Self;
  Result := False;

  LForm := FormFor(ANode);

  if LForm = nil then
  begin
    Exit;
  end;

  if FGraphMode then
  begin
    Exit(HandleGraph(ANode, AEvent));
  end;
  { Default compound actions can report their semantic owner while ANode still
    addresses the actual header. Capture presentation from the admitted form,
    independently of which event transport reported the disclosure change. }
  LOptions := LForm.Find(FID + '-options');

  if LOptions <> nil then
  begin
    FOptionsExpanded := LOptions.Prop('value') = 'true';
  end;
  try

    if (AEvent.Trigger = ntChange) and (ANode.ID = FID + '-name') then
    begin
      FName := ANode.Prop('value');
      FDirty := True;
      Exit;
    end;

    if (AEvent.Trigger = ntChange) and (ANode.ID = FID + '-confirm') then
    begin
      FConfirmed := ANode.Prop('value') = 'true';
      Exit;
    end;

    if (AEvent.Trigger = ntChange) and (ANode.ID = FID + '-flow') then
    begin
      SelectFlow(NyxFlowRef(ANode.Prop('value')), FConfirmed);
      Exit(True);
    end;
    for LIndex := 0 to High(FPorts) do
    begin

      if ANode.ID <> PortID(LIndex) then
      begin
        Continue;
      end;
      RequireCurrent;

      if AEvent.Trigger = ntClick then
      begin

        if FPorts[LIndex].Direction = nfdOutput then
        begin
          FSource := LIndex;
        end
        else
        begin
          FTarget := LIndex;
        end;
        FConfirmed := False;
        Exit(True);
      end;

      if not AEvent.HasDrag or (AResponse = nil) then
      begin
        Exit;
      end;

      if (AEvent.Trigger = ntDragStart) and
        (FPorts[LIndex].Direction = nfdOutput) and AResponse.CanRequest(ngcOfferDrag) then
      begin
        FSource := LIndex;
        FDragSource := LIndex;
        FDrag := True;
        AResponse.OfferDrag(NyxTransferCustom(NyxTransferFormat(CWireTransfer),
          FDragToken), [ndoLink]);
        Exit;
      end;

      if AEvent.Trigger = ntDragEnd then
      begin
        FDrag := False;
        Exit(True);
      end;
      LCompatible := FDrag and (FPorts[LIndex].Direction = nfdInput) and
        (FPorts[FDragSource].Kind <> FPorts[LIndex].Kind) and
        (FPorts[FDragSource].ValueType = FPorts[LIndex].ValueType) and
        AEvent.Drag.Transfer.HasFormat(NyxTransferFormat(CWireTransfer)) and
        (ndoLink in AEvent.Drag.Allowed) and AResponse.CanRequest(ngcAcceptDrop);

      if LCompatible and (AEvent.Trigger in [ntDragEnter, ntDragOver]) then
      begin
        AResponse.AcceptDrop(ndoLink);
        Exit;
      end;

      if LCompatible and (AEvent.Trigger = ntDrop) and AEvent.Drag.Transfer.Readable and
        (AEvent.Drag.Transfer.TextFor(NyxTransferFormat(CWireTransfer)) = FDragToken) then
      begin
        FTarget := LIndex;
        FDrag := False;
        Connect(FDragSource, LIndex, FConfirmed);
        AResponse.AcceptDrop(ndoLink);
        Exit(True);
      end;
      Exit;
    end;

    if AEvent.Trigger <> ntClick then
    begin
      Exit;
    end;

    if ANode.ID = FID + '-graph-begin' then
    begin
      BeginGraphEditing;
    end
    else if ANode.ID = FID + '-connect' then
    begin
      Connect(FSource, FTarget, FConfirmed);
    end
    else if ANode.ID = FID + '-apply' then
    begin
      Apply(NyxFlowRouteID(FName), FConfirmed);
    end
    else if ANode.ID = FID + '-reload' then
    begin
      Reload(FConfirmed);
    end
    else if ANode.ID = FID + '-remove-applied' then
    begin
      RemoveApplied(FConfirmed);
    end
    else if ANode.ID = FID + '-remove-staged' then
    begin
      Remove(FSelectedWire, FConfirmed);
    end
    else
    begin
      for LIndex := 0 to High(FWires) do
      begin

        if ANode.ID = FID + '-wire-' + TNyxText(IntToStr(LIndex)) then
        begin
          FSelectedWire := LIndex;
          FConfirmed := False;
          Exit(True);
        end;
      end;
      Exit;
    end;
    Result := True;
  except
    on E: Exception do
    begin
      FMessage := E.Message;
      Result := True;
    end;
  end;
end;
