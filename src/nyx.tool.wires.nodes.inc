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
{ Shared node faces reuse exact discovery identities and controller epochs.
  Only inert saved descriptors/wires are inspected; no creator operation runs. }

procedure TControlWires.ValidateFaces(AMaximumNodes, AMaximumPorts: Integer);
var
  LIndex: Integer;
  LPorts: Integer;
begin
  RequireActive;

  if (AMaximumNodes < 1) or (AMaximumNodes > 2047) or
    (AMaximumPorts < 1) or (AMaximumPorts > 65536) then
  begin
    raise ENyxFlow.Create('Node presentation requires explicit bounded face and port budgets');
  end;

  if GetNodeCount > AMaximumNodes then
  begin
    raise ENyxFlow.Create('The complete pipeline exceeds the selected node-face budget');
  end;
  LPorts := 0;
  for LIndex := 0 to GetNodeCount - 1 do
  begin
    Inc(LPorts, NodeDescriptor(LIndex).PortCount);

    if LPorts > AMaximumPorts then
    begin
      raise ENyxFlow.Create('The complete pipeline exceeds the selected displayed-port budget');
    end;
  end;
end;

function TControlWires.GetNodeFlow: TNyxFlowRef;
begin
  RequireActive;
  Result := FFlow;
end;

function TControlWires.GetNodeCount: Integer;
begin
  RequireActive;
  Result := 0;

  if NodeDefinition <> nil then
  begin
    Result := NodeDefinition.StepCount;
  end;
end;

procedure TControlWires.CheckNode(AIndex: Integer);
begin
  RequireActive;

  if (AIndex < 0) or (AIndex >= GetNodeCount) then
  begin
    raise ENyxFlow.Create('Choose an existing captured pipeline step');
  end;
end;

function TControlWires.NodeStep(AIndex: Integer): TNyxFlowStepRef;
begin
  CheckNode(AIndex);
  Result := NodeDefinition.Step(AIndex);
end;

function TControlWires.NodeDescriptor(AIndex: Integer): TNyxLogicDescriptor;
begin
  CheckNode(AIndex);
  Result := NodeDefinition.Descriptor(AIndex).Copy;
end;

function TControlWires.NodeControl(AIndex: Integer): TNyxControlRef;
begin
  CheckNode(AIndex);
  Result := NyxControl(FID + '-step-' + TNyxText(IntToStr(AIndex)));
end;

function TControlWires.NodePort(AIndex, APort: Integer): TNyxToolPipelinePort;
var
  LIndex: Integer;
begin
  Result := Default(TNyxToolPipelinePort);
  Result.Endpoint.Step := NodeStep(AIndex);
  Result.Metadata := NodeDescriptor(AIndex).Port(APort);
  Result.Endpoint.Port := Result.Metadata.Reference;
  Result.RoutePort := -1;
  Result.DisplayControl := NyxControl(FID + '-linked-' + TNyxText(IntToStr(AIndex)) +
    '-' + TNyxText(IntToStr(APort)));

  if FGraphMode then
  begin
    Result.RoutePort := FGraphOffsets[AIndex] + APort;
    Result.DisplayControl := PortControl(Result.RoutePort);
    Exit;
  end;
  for LIndex := 0 to GetPortCount - 1 do
  begin

    if (Port(LIndex).Kind = ntwpkPipeline) and Port(LIndex).Endpoint.Same(Result.Endpoint) then
    begin
      Result.RoutePort := LIndex;
      Result.DisplayControl := PortControl(LIndex);
      Break;
    end;
  end;
end;

function TControlWires.GetNodeWireCount: Integer;
begin
  RequireActive;
  Result := 0;

  if NodeDefinition <> nil then
  begin
    Result := NodeDefinition.WireCount;
  end;
end;

function TControlWires.NodeWire(AIndex: Integer): TNyxToolPipelineWire;
var
  LWire: TNyxDataValue;
begin
  RequireActive;

  if (AIndex < 0) or (AIndex >= GetNodeWireCount) then
  begin
    raise ENyxFlow.Create('Choose an existing internal pipeline wire');
  end;
  { The saved wire vector is the explicit persistence boundary. Endpoint names
    become distinct typed references before any geometry or interaction uses them. }

  if FGraphMode then
  begin
    LWire := FGraphFrame.Definition.Field('wires').Item(AIndex);
  end
  else
  begin
    LWire := FNodeWires.Item(AIndex);
  end;
  Result.Source := NyxFlowEndpoint(NyxFlowStep(LWire.Field('source').Field('step').AsText),
    NyxFlowPort(LWire.Field('source').Field('port').AsText));
  Result.Target := NyxFlowEndpoint(NyxFlowStep(LWire.Field('target').Field('step').AsText),
    NyxFlowPort(LWire.Field('target').Field('port').AsText));
end;

function TControlWires.ModeControl: TNyxControlRef;
begin
  RequireActive;
  Result := NyxControl(FID + '-node-mode');
end;

function TControlWires.BuildNode(AIndex: Integer): INyxCard;
var
  LStep: TNyxFlowStepRef;
  LDescriptor: TNyxLogicDescriptor;
  LPort: TNyxToolPipelinePort;
  LButton: INyxButton;
  LColumns: INyxRow;
  LInputs: INyxColumn;
  LOutputs: INyxColumn;
  LIndex: Integer;
  LHint: TNyxText;
begin
  LStep := NodeStep(AIndex);
  LDescriptor := NodeDescriptor(AIndex);
  Result := NewNyxCard(NodeControl(AIndex).ID);
  Result.Configure.Padding(12).Gap(8).WidthSizing(nsFill).Done;
  Result.Node.Extensions.SetValue(NyxExtension(CWireForm), NyxData(FEpoch));
  Result.Add(NewNyxButton(Result.ID + '-move').Configure
    .Text(LStep.Name).PartName(NyxPart('move')).TouchBehavior(ntbNone)
    .Hint('Drag to move this operation. Arrow keys move 8 world units; Shift moves 32. Home restores it.').Done);
  Result.Add(NewNyxBadge(Result.ID + '-kind').WithText(LDescriptor.Title));
  Result.Add(NewNyxLabel(Result.ID + '-help').WithText(LDescriptor.Description));
  LColumns := NewNyxRow(Result.ID + '-ports');
  LColumns.Configure.Gap(8).Wrap(nfwNoWrap).Done;
  LInputs := NewNyxColumn(Result.ID + '-inputs');
  LOutputs := NewNyxColumn(Result.ID + '-outputs');
  LInputs.Configure.Gap(4).Width(124).WidthSizing(nsAutomatic).Done;
  LOutputs.Configure.Gap(4).Width(124).WidthSizing(nsAutomatic).Done;
  for LIndex := 0 to LDescriptor.PortCount - 1 do
  begin
    LPort := NodePort(AIndex, LIndex);
    LHint := LPort.Metadata.Description;

    if LPort.RoutePort < 0 then
    begin
      LHint := LHint + ' Connected inside the pipeline; edit that connection in Logic.';
    end;
    LButton := NewNyxButton(LPort.DisplayControl.ID);
    LButton.Configure.Text(LPort.Metadata.Reference.Name + ' / ' +
      NyxFlowTypeName(LPort.Metadata.ValueType)).Hint(LHint).TouchBehavior(ntbNone)
      .Enabled((LPort.RoutePort >= 0) and not GetStale).Done;

    if (LPort.RoutePort >= 0) and
      ((LPort.RoutePort = FSource) or (LPort.RoutePort = FTarget)) then
    begin
      LButton.Configure.Variant(nvPrimary).Done;
    end;

    if LPort.Metadata.Direction = nfdInput then
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
end;

function TControlWires.BuildNodeToolbar: INyxCard;
begin

  if FGraphMode then
  begin
    Exit(BuildGraph);
  end;
  Result := BuildPipeline;
  { Remove the combined port subtree before adopting per-step cards. Hiding it
    would retain duplicate identities for the real route port buttons. }
  Result.Node.Remove(Result.Node.Find(FID + '-ports'));
  Result.Node.Find(ModeControl.ID).Configure.Text('Use combined pipeline panel').Done;

  if FGraphHost <> nil then
  begin
    Result.Add(NewNyxButton(FID + '-graph-begin').WithText('Edit internal connections'));
  end;
end;
