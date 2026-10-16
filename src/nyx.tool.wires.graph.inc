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
{ Internal node wiring stages into the Inspector's one Logic proposal. Controller
  fields below are only copied presentation/selection state, never a second draft. }

function TControlWires.NodeDefinition: INyxFlowDefinition;
begin
  Result := FDefinition;

  if FGraphMode then
  begin
    Result := FGraphDefinition;
  end;
end;

procedure TControlWires.ConfigureGraphHost(const AHost: INyxToolGraphHost);
begin
  RequireActive;

  if FGraphMode then
  begin
    raise ENyxFlow.Create('Leave node connection editing before replacing its proposal host');
  end;
  FGraphHost := AHost;
end;

function TControlWires.GetGraphEditing: Boolean;
begin
  RequireActive;
  Result := FGraphMode;
end;

procedure TControlWires.AcceptGraphFrame(const AFrame: TNyxToolGraphFrame);
var
  LDefinition: INyxFlowDefinition;
  LPorts: array of TNyxToolWirePort;
  LOffsets: array of Integer;
  LDescriptor: TNyxLogicDescriptor;
  LPort: TNyxFlowPort;
  LCount: Integer;
  LStep: Integer;
  LIndex: Integer;
begin

  if FEpoch = High(Integer) then
  begin
    raise ENyxFlow.Create('Connector form version capacity exhausted');
  end;
  LDefinition := NyxFlowDefinitionFromData(AFrame.Definition);
  LCount := 0;
  for LStep := 0 to LDefinition.StepCount - 1 do
  begin
    Inc(LCount, LDefinition.Descriptor(LStep).PortCount);
  end;

  if LCount > FOptions.MaximumPorts then
  begin
    raise ENyxFlow.Create('The complete graph exceeds this connector port budget');
  end;
  SetLength(LPorts, LCount);
  SetLength(LOffsets, LDefinition.StepCount);
  LCount := 0;
  for LStep := 0 to LDefinition.StepCount - 1 do
  begin
    LDescriptor := LDefinition.Descriptor(LStep);
    LOffsets[LStep] := LCount;
    for LIndex := 0 to LDescriptor.PortCount - 1 do
    begin
      LPort := LDescriptor.Port(LIndex);
      LPorts[LCount].Kind := ntwpkPipeline;
      LPorts[LCount].Endpoint := NyxFlowEndpoint(LDefinition.Step(LStep), LPort.Reference);
      LPorts[LCount].Direction := LPort.Direction;
      LPorts[LCount].ValueType := LPort.ValueType;
      LPorts[LCount].Title := LPort.Reference.Name;
      LPorts[LCount].Description := LPort.Description;
      Inc(LCount);
    end;
  end;
  FGraphFrame := AFrame;
  FGraphDefinition := LDefinition;
  FGraphPorts := LPorts;
  FGraphOffsets := LOffsets;
  FSource := -1;
  FTarget := -1;
  FConfirmed := False;
  FSelectedWire := -1;
  FMessage := '';
  Inc(FEpoch);
end;

procedure TControlWires.BeginGraphEditing;
var
  LFrame: TNyxToolGraphFrame;
  LLease: INyxToolControlWires;
begin
  RequireActive;
  LLease := Self;

  if FGraphHost = nil then
  begin
    raise ENyxFlow.Create('This host does not expose shared Logic proposal editing');
  end;

  if FDirty then
  begin
    raise ENyxFlow.Create('Apply or explicitly discard control wires before editing internal connections');
  end;

  if not FGraphMode then
  begin
    RequireCurrent;
  end;
  LFrame := FGraphHost.OpenGraph(FFlow);
  AcceptGraphFrame(LFrame);
  FGraphMode := True;
end;

procedure TControlWires.EndGraphEditing;
var
  LLease: INyxToolControlWires;
begin
  RequireActive;
  LLease := Self;

  if not FItem.Defined then
  begin
    raise ENyxFlow.Create('This graph has no control-item mode; hide its canvas presentation instead');
  end;
  { Returning to route presentation retains the proposal in its existing Logic
    owner. Capture current runtime metadata before retiring this display mode. }
  Snapshot(FFlow);
  FGraphMode := False;
  FGraphDefinition := nil;
  FGraphPorts := nil;
  FGraphOffsets := nil;
end;

procedure TControlWires.RequireGraphCurrent;
var
  LCurrent: TNyxToolGraphFrame;
begin
  RequireActive;

  if not FGraphMode or (FGraphHost = nil) then
  begin
    raise ENyxFlow.Create('Open internal node connections first');
  end;
  LCurrent := FGraphHost.ReadGraph(FFlow);

  if LCurrent.Stale or (LCurrent.ProposalVersion <> FGraphFrame.ProposalVersion) or
    (LCurrent.SceneRevision <> FGraphFrame.SceneRevision) or
    (LCurrent.FlowRevision <> FGraphFrame.FlowRevision) or
    (LCurrent.Definition.ToJSON <> FGraphFrame.Definition.ToJSON) then
  begin
    raise ENyxFlow.Create('The shared Logic proposal changed. Reopen its current connections.');
  end;

  if LCurrent.FieldsPending then
  begin
    raise ENyxFlow.Create('Stage or explicitly discard unfinished Logic fields before editing node connections');
  end;

  if FEpoch = High(Integer) then
  begin
    raise ENyxFlow.Create('Connector form version capacity exhausted');
  end;
end;

procedure TControlWires.ConnectGraph(ASource, ATarget: Integer; AReplace: Boolean);
var
  LSource: TNyxToolWirePort;
  LTarget: TNyxToolWirePort;
  LWire: TNyxToolPipelineWire;
  LChanges: TNyxFlowChanges;
  LIndex: Integer;
  LLease: INyxToolControlWires;
begin
  RequireGraphCurrent;
  LLease := Self;
  LSource := Port(ASource);
  LTarget := Port(ATarget);

  if (LSource.Direction <> nfdOutput) or (LTarget.Direction <> nfdInput) or
    (LSource.ValueType <> LTarget.ValueType) then
  begin
    raise ENyxFlow.Create('Connect an operation output to an input of the exact same value family');
  end;
  LChanges := nil;
  for LIndex := 0 to GetNodeWireCount - 1 do
  begin
    LWire := NodeWire(LIndex);

    if not LWire.Target.Same(LTarget.Endpoint) then
    begin
      Continue;
    end;

    if LWire.Source.Same(LSource.Endpoint) then
    begin
      Exit;
    end;

    if not AReplace then
    begin
      raise ENyxFlow.Create('Acknowledge replacing the existing internal input connection');
    end;
    SetLength(LChanges, 1);
    LChanges[0] := NyxDisconnectFlow(FFlow, LWire.Source, LWire.Target);
    Break;
  end;
  SetLength(LChanges, Length(LChanges) + 1);
  LChanges[High(LChanges)] := NyxConnectFlow(FFlow, LSource.Endpoint, LTarget.Endpoint);
  AcceptGraphFrame(FGraphHost.StageGraph(FGraphFrame, LChanges));
end;

procedure TControlWires.RemoveGraphWire(AIndex: Integer; AConfirmed: Boolean);
var
  LWire: TNyxToolPipelineWire;
  LLease: INyxToolControlWires;
begin
  RequireGraphCurrent;
  LLease := Self;

  if not AConfirmed then
  begin
    raise ENyxFlow.Create('Acknowledge removing this internal connection');
  end;
  LWire := NodeWire(AIndex);
  AcceptGraphFrame(FGraphHost.StageGraph(FGraphFrame,
    [NyxDisconnectFlow(FFlow, LWire.Source, LWire.Target)]));
end;

procedure TControlWires.ApplyGraph;
var
  LFrame: TNyxToolGraphFrame;
  LLease: INyxToolControlWires;
begin
  RequireGraphCurrent;
  LLease := Self;
  { Admission and execution stay separate. Invalid whole routes keep the shared
    proposal intact; only the prepared owner can publish a working graph. }
  LFrame := FGraphHost.ApplyGraph(FGraphFrame);
  AcceptGraphFrame(LFrame);
end;

function TControlWires.BuildGraph: INyxCard;
var
  LIndex: Integer;
  LWire: TNyxToolPipelineWire;
  LStatus: TNyxText;
  LActions: INyxRow;
begin
  RequireActive;
  Result := NewNyxCard(FID);
  Result.Configure.Padding(12).Gap(8).WidthSizing(nsFill).Done;
  StampForm(Result.Node);
  Result.Add(NewNyxButton(FID + '-move').Configure.Text('Internal connections')
    .PartName(NyxPart('move')).TouchBehavior(ntbNone).Done);
  Result.Add(NewNyxLabel(FID + '-graph-help').WithText(
    'Choose an output and an input, or drag between them. Logic shares this proposal.'));
  LStatus := 'No unapplied graph changes';

  if FGraphFrame.Pending then
  begin
    LStatus := 'Shared Logic proposal pending';
  end;

  if GetStale then
  begin
    LStatus := 'Shared Logic changed; reopen current connections';
  end;
  Result.Add(NewNyxBadge(FID + '-graph-status').WithText(LStatus));
  Result.Add(NewNyxSwitch(FID + '-graph-confirm').Configure
    .Text('Confirm destructive changes').Value(FConfirmed)
    .Hint('Allows replacing or removing a connection, or discarding unfinished Logic when reloading.').Done);
  LActions := NewNyxRow(FID + '-graph-actions');
  LActions.Configure.Wrap(nfwWrap).Gap(8).Done;
  LActions.Add(NewNyxButton(FID + '-graph-connect').Configure
    .Text('Stage connection').Enabled((FSource >= 0) and (FTarget >= 0)).Done);
  LActions.Add(NewNyxButton(FID + '-graph-apply').Configure
    .Text('Apply live graph').Enabled(FGraphFrame.Pending and not GetStale).Done);
  Result.Add(LActions);
  for LIndex := 0 to GetNodeWireCount - 1 do
  begin
    LWire := NodeWire(LIndex);
    Result.Add(NewNyxButton(FID + '-graph-wire-' + TNyxText(IntToStr(LIndex)))
      .WithText(LWire.Source.Step.Name + '.' + LWire.Source.Port.Name + ' -> ' +
        LWire.Target.Step.Name + '.' + LWire.Target.Port.Name));
  end;
  Result.Add(NewNyxButton(FID + '-graph-remove').Configure
    .Text('Remove selected connection').Enabled(FSelectedWire >= 0).Done);
  Result.Add(NewNyxButton(FID + '-graph-current').WithText('Reopen current proposal'));
  Result.Add(NewNyxButton(FID + '-graph-reload').Configure.Text('Reload runtime graph')
    .Enabled(FFlowEditing.RuntimeFlows.Contains(FFlow)).Done);
  Result.Add(NewNyxButton(ModeControl.ID).WithText('Show operation nodes'));

  if FItem.Defined then
  begin
    Result.Add(NewNyxButton(FID + '-graph-end').Configure.Text('Back to control connections')
      .Hint('Unapplied internal connections remain in Logic until applied or explicitly discarded.').Done);
  end;
  Result.Add(NewNyxLabel(FID + '-message').WithText(FMessage));
end;

function TControlWires.HandleGraph(ANode: TNyxNode;
  const AEvent: TNyxEventInfo): Boolean;
var
  LIndex: Integer;
begin
  Result := False;
  try

    if (ANode.ID = FID + '-graph-confirm') and (AEvent.Trigger = ntChange) then
    begin
      FConfirmed := ANode.Prop('value') = 'true';
      Exit;
    end;

    if AEvent.Trigger <> ntClick then
    begin
      Exit;
    end;
    for LIndex := 0 to GetPortCount - 1 do
    begin

      if ANode.ID = PortControl(LIndex).ID then
      begin
        SelectPort(LIndex);
        Exit(True);
      end;
    end;

    if ANode.ID = FID + '-graph-connect' then
    begin
      StageSelected(FSource, FTarget);
    end
    else if ANode.ID = FID + '-graph-apply' then
    begin
      ApplyGraph;
    end
    else if ANode.ID = FID + '-graph-remove' then
    begin
      RemoveGraphWire(FSelectedWire, FConfirmed);
    end
    else if ANode.ID = FID + '-graph-current' then
    begin
      BeginGraphEditing;
    end
    else if ANode.ID = FID + '-graph-reload' then
    begin
      Reload(FConfirmed);
    end
    else if ANode.ID = FID + '-graph-end' then
    begin
      EndGraphEditing;
    end
    else
    begin
      for LIndex := 0 to GetNodeWireCount - 1 do
      begin

        if ANode.ID = FID + '-graph-wire-' + TNyxText(IntToStr(LIndex)) then
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
