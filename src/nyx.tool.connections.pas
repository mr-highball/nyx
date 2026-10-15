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

unit nyx.tool.connections;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.data,
  nyx.behavior, nyx.events, nyx.gestures, nyx.canvas.workspace,
  nyx.flow.types, nyx.flow.declarations, nyx.flow.editing;

type
  { Replacement is explicit and distinct from Connect; an occupied input never
    silently loses its old producer. Removal/replacement require acknowledgement. }
  TNyxToolConnectionAction = (ntcaConnect, ntcaReplace, ntcaRemove);

  { UI-only controller for an independent runtime workspace's saved graph ports.
    Build returns a fresh ordinary Nyx form, suitable for a tool inspector or a
    creator host. The controller retains its view, never its renderer or widgets.
    Disconnect before the borrowed application store/host retires. No control in
    the returned tree retains this controller. Changes use the existing graph
    admission and prepared route replacement; document defaults/history stay
    untouched. Export RuntimeFlows through the workspace's optional capability.
    Full control-command ports and canvas-anchored wire chrome remain separate. }
  INyxToolConnectionEditor = interface(IInterface)
    ['{3ED49869-B3A4-4472-8B18-91DB02CC1500}']
    procedure Disconnect;
    function Build: INyxCard;
    function Handle(ANode: TNyxNode; const AEvent: TNyxEventInfo;
      const AResponse: INyxGestureResponse = nil): Boolean;
    procedure SelectFlow(const AReference: TNyxFlowRef);
    procedure Apply(const ASource, ATarget: TNyxFlowEndpoint;
      AAction: TNyxToolConnectionAction; AConfirmed: Boolean = False);
  end;

{ Create one UI-owned independent editor. AView must provide runtime scene and
  flow-editing capabilities; unsupported/retired views refuse before use. AID
  supplies ordinary form identity, never executable source or host authority. }
function NewNyxToolConnectionEditor(const AID: TNyxText;
  const AView: INyxCanvasWorkspaceView): INyxToolConnectionEditor;

implementation

uses Math, nyx.canvas.scene, nyx.flow, nyx.scheduler, nyx.state, nyx.logic.catalog;

const
  CForm = 'nyx.tool-connections';
  CTransfer = 'application/x-nyx-connection';
  CPageSize = 12;

type
  { Detached endpoint metadata. No control, document, resolver or host capability
    travels in a port choice or drag transfer. Indices address only this snapshot. }
  TPortChoice = record
    Endpoint: TNyxFlowEndpoint;
    Port: TNyxFlowPort;
  end;

  { Owns snapshot/selection and a managed view lease. All returned form roots
    have independent parent ownership. A local drag session is revoked on catalog
    change/disconnect; imported MIME text cannot manufacture a session or a port. }
  TConnectionEditor = class(TInterfacedObject, INyxToolConnectionEditor)
  private
    FID: TNyxText;
    FView: INyxCanvasWorkspaceView;
    FRuntime: INyxCanvasRuntimeView;
    FEditing: INyxCanvasFlowEditing;
    FFlows: INyxFlowDeclarations;
    FFlow: TNyxFlowRef;
    FDefinition: INyxFlowDefinition;
    FBaseline: TNyxDataValue;
    FPorts: array of TPortChoice;
    FSource: Integer;
    FTarget: Integer;
    FOffset: Integer;
    FWireOffset: Integer;
    FEpoch: Integer;
    FScene: Integer;
    FFlowRevision: Integer;
    FConfirmed: Boolean;
    FDrag: Boolean;
    FDragSource: Integer;
    FDragToken: TNyxText;
    FMessage: TNyxText;
    procedure RequireActive;
    procedure Snapshot;
    function IndexOf(const AEndpoint: TNyxFlowEndpoint): Integer;
    function Wire(AIndex: Integer): TNyxFlowWire;
    function FormFor(ANode: TNyxNode): TNyxNode;
  public
    constructor Create(const AID: TNyxText; const AView: INyxCanvasWorkspaceView);
    procedure Disconnect;
    function Build: INyxCard;
    function Handle(ANode: TNyxNode; const AEvent: TNyxEventInfo;
      const AResponse: INyxGestureResponse): Boolean;
    procedure SelectFlow(const AReference: TNyxFlowRef);
    procedure Apply(const ASource, ATarget: TNyxFlowEndpoint;
      AAction: TNyxToolConnectionAction; AConfirmed: Boolean);
  end;

var
  { UI-owned process-local identity only; a transfer carries no execution grant.
    A live local drag and exact captured graph/revisions are required separately. }
  GConnectionSerial: Integer;

constructor TConnectionEditor.Create(const AID: TNyxText;
  const AView: INyxCanvasWorkspaceView);
var
  LRuntime: INyxCanvasRuntimeView;
  LEditing: INyxCanvasFlowEditing;
begin
  inherited Create;
  FID := AID;
  FView := AView;

  if (FView = nil) or not Supports(FView, INyxCanvasRuntimeView, LRuntime) or
    not Supports(FView, INyxCanvasFlowEditing, LEditing) then
  begin
    raise ENyxCanvasScene.Create('Tool connections require an editable runtime workspace');
  end;
  FRuntime := LRuntime;
  FEditing := LEditing;

  if GConnectionSerial = High(Integer) then
  begin
    raise ENyxCanvasScene.Create('Tool connection identity capacity exhausted');
  end;
  Inc(GConnectionSerial);
  FDragToken := TNyxText(IntToStr(GConnectionSerial));
  FSource := -1;
  FTarget := -1;
  Snapshot;
end;

procedure TConnectionEditor.RequireActive;
begin

  if (FView = nil) or not FView.Connected then
  begin
    raise ENyxCanvasScene.Create('Tool connection editor is retired');
  end;
end;

procedure TConnectionEditor.Disconnect;
begin
  FDrag := False;
  FView := nil;
  FRuntime := nil;
  FEditing := nil;
end;

procedure TConnectionEditor.Snapshot;
var
  LBaseline: TNyxDataValue;
  LDescriptor: TNyxLogicDescriptor;
  LIndex: Integer;
  LPart: Integer;
  LCount: Integer;
begin
  RequireActive;

  { Port metadata is immutable at this owning graph revision. Inspector-only
    selection/page changes must not reserialize and rediscover the whole graph.
    A selected graph change clears Baseline explicitly; any admitted source edit
    advances FlowRevision even when its bytes happen to remain the same. }

  if FBaseline.Defined and (FFlows <> nil) and
    (FEditing.FlowRevision = FFlowRevision) then
  begin
    FScene := FRuntime.Scene.Revision;
    Exit;
  end;
  FFlowRevision := FEditing.FlowRevision;
  FFlows := FEditing.RuntimeFlows;

  if not FFlow.Defined or not FFlows.Contains(FFlow) then
  begin
    FFlow := Default(TNyxFlowRef);

    if FFlows.Count > 0 then
    begin
      FFlow := FFlows.Reference(0);
    end;
  end;
  LBaseline := NyxObject([NyxField('exists', NyxData(False)), NyxField('definition', NyxNull)]);

  if FFlow.Defined then
  begin
    LBaseline := NyxFlowBaseline(FFlows, FFlow);
  end;

  if not FBaseline.Defined or (LBaseline.ToJSON <> FBaseline.ToJSON) then
  begin
    FSource := -1;
    FTarget := -1;
    FOffset := 0;
    FWireOffset := 0;
    FConfirmed := False;
    FDrag := False;
  end;
  FBaseline := LBaseline;
  FDefinition := nil;
  FPorts := nil;

  if FFlow.Defined then
  begin
    FDefinition := FFlows.Definition(FFlow);
    for LIndex := 0 to FDefinition.StepCount - 1 do
    begin
      LDescriptor := FDefinition.Descriptor(LIndex);
      for LPart := 0 to LDescriptor.PortCount - 1 do
      begin
        LCount := Length(FPorts);
        SetLength(FPorts, LCount + 1);
        FPorts[LCount].Endpoint := NyxFlowEndpoint(FDefinition.Step(LIndex),
          LDescriptor.Port(LPart).Reference);
        FPorts[LCount].Port := LDescriptor.Port(LPart);
      end;
    end;
  end;
  FScene := FRuntime.Scene.Revision;

  if FEditing.FlowRevision <> FFlowRevision then
  begin
    raise ENyxFlow.Create('Graph changed during connection snapshot');
  end;
end;

function TConnectionEditor.IndexOf(const AEndpoint: TNyxFlowEndpoint): Integer;
begin
  for Result := 0 to High(FPorts) do
  begin

    if FPorts[Result].Endpoint.Same(AEndpoint) then
    begin
      Exit;
    end;
  end;
  Result := -1;
end;

function TConnectionEditor.Wire(AIndex: Integer): TNyxFlowWire;
var
  LWire: TNyxDataValue;
begin
  { ToData is the explicit graph persistence boundary; construct typed endpoint
    records immediately instead of interpreting display labels as behavior. }
  LWire := FBaseline.Field('definition').Field('wires').Item(AIndex);
  Result.Source := NyxFlowEndpoint(NyxFlowStep(LWire.Field('source').Field('step').AsText),
    NyxFlowPort(LWire.Field('source').Field('port').AsText));
  Result.Target := NyxFlowEndpoint(NyxFlowStep(LWire.Field('target').Field('step').AsText),
    NyxFlowPort(LWire.Field('target').Field('port').AsText));
end;

procedure TConnectionEditor.SelectFlow(const AReference: TNyxFlowRef);
begin
  RequireActive;

  if not AReference.Defined or not FEditing.RuntimeFlows.Contains(AReference) then
  begin
    raise ENyxFlow.Create('Choose an existing runtime graph');
  end;
  FFlow := AReference;
  FBaseline := Default(TNyxDataValue);
  FMessage := '';
  Snapshot;
end;

procedure TConnectionEditor.Apply(const ASource, ATarget: TNyxFlowEndpoint;
  AAction: TNyxToolConnectionAction; AConfirmed: Boolean);
var
  LSource: Integer;
  LTarget: Integer;
  LChanges: array of TNyxFlowChange;
  LWire: TNyxFlowWire;
  LIndex: Integer;
  LOccupied: Boolean;
begin
  RequireActive;

  if not (AAction in [ntcaConnect, ntcaReplace, ntcaRemove]) then
  begin
    raise ENyxFlow.Create('Unknown typed connection action');
  end;

  if (AAction <> ntcaConnect) and not AConfirmed then
  begin
    raise ENyxFlow.Create('Review and acknowledge connection removal before applying it');
  end;
  LSource := IndexOf(ASource);
  LTarget := IndexOf(ATarget);

  if (LSource < 0) or (LTarget < 0) then
  begin
    raise ENyxFlow.Create('Choose exact ports from this graph snapshot');
  end;

  if (FPorts[LSource].Port.Direction <> nfdOutput) or
    (FPorts[LTarget].Port.Direction <> nfdInput) or
    (FPorts[LSource].Port.ValueType <> FPorts[LTarget].Port.ValueType) then
  begin
    raise ENyxFlow.Create('Connections require an output and input of the same type');
  end;
  LOccupied := False;
  SetLength(LChanges, 0);
  for LIndex := 0 to FDefinition.WireCount - 1 do
  begin
    LWire := Wire(LIndex);

    if LWire.Target.Same(ATarget) then
    begin
      LOccupied := True;

      if AAction = ntcaReplace then
      begin
        SetLength(LChanges, 1);
        LChanges[0] := NyxDisconnectFlow(FFlow, LWire.Source, LWire.Target).Expect(FBaseline);
      end;
      Break;
    end;
  end;

  if (AAction = ntcaConnect) and LOccupied then
  begin
    raise ENyxFlow.Create('Input already has a producer; explicitly confirm Replace incoming wire');
  end;

  if (AAction = ntcaReplace) and not LOccupied then
  begin
    raise ENyxFlow.Create('No incoming wire to replace; use Connect selected');
  end;
  SetLength(LChanges, Length(LChanges) + 1);

  if AAction = ntcaRemove then
  begin
    LChanges[High(LChanges)] := NyxDisconnectFlow(FFlow, ASource, ATarget).Expect(FBaseline);
  end
  else
  begin
    LChanges[High(LChanges)] := NyxConnectFlow(FFlow, ASource, ATarget).Expect(FBaseline);
  end;
  FEditing.ApplyFlowChanges(LChanges, FScene, FFlowRevision);
  FConfirmed := False;
  FDrag := False;
  FMessage := 'Connection applied. Work using the changed graph was cancelled.';
  Snapshot;
end;

function TConnectionEditor.Build: INyxCard;
var
  LList: TNyxText;
  LIndex: Integer;
  LRow: INyxCard;
  LButton: INyxButton;
  LActions: INyxRow;
  LWire: TNyxFlowWire;
  LCaption: TNyxText;
begin
  Snapshot;
  FDrag := False;

  if FEpoch = High(Integer) then
  begin
    raise ENyxFlow.Create('Connection form revision capacity exhausted');
  end;
  Inc(FEpoch);
  Result := NewNyxCard(FID);
  Result.Configure.Padding(12).Gap(10).Done;
  Result.Node.Extensions.SetValue(NyxExtension(CForm), NyxData(FEpoch));
  Result.Add(NewNyxHeading(FID + '-title').WithText('Connections'));
  Result.Add(NewNyxLabel(FID + '-help').WithText(
    'Drag an output to a compatible input, or choose both with Tab and Enter. Changes affect this running workspace.'));
  LList := '';
  for LIndex := 0 to FFlows.Count - 1 do
  begin

    if LIndex > 0 then
    begin
      LList := LList + #10;
    end;
    LList := LList + FFlows.Reference(LIndex).Name;
  end;
  Result.Add(NewNyxSelect(FID + '-graph').Configure.Text('Graph')
    .Items(LList).Value(FFlow.Name).Enabled(FFlow.Defined).Done);
  LCaption := 'Choose an output';

  if FSource >= 0 then
  begin
    LCaption := FPorts[FSource].Endpoint.Step.Name + ' / ' + FPorts[FSource].Endpoint.Port.Name;
  end;
  LCaption := LCaption + '  ->  ';

  if FTarget >= 0 then
  begin
    LCaption := LCaption + FPorts[FTarget].Endpoint.Step.Name + ' / ' + FPorts[FTarget].Endpoint.Port.Name;
  end
  else
  begin
    LCaption := LCaption + 'choose an input';
  end;
  Result.Add(NewNyxLabel(FID + '-selected').WithText(LCaption));
  for LIndex := FOffset to Min(High(FPorts), FOffset + CPageSize - 1) do
  begin
    LRow := NewNyxCard(FID + '-row-' + TNyxText(IntToStr(LIndex)));
    LRow.Configure.Padding(8).Gap(4).Done;
    LCaption := 'Input';

    if FPorts[LIndex].Port.Direction = nfdOutput then
    begin
      LCaption := 'Output';
    end;
    LButton := NewNyxButton(FID + '-port-' + TNyxText(IntToStr(LIndex)));
    LButton.Configure.Text(LCaption + ' / ' + FPorts[LIndex].Endpoint.Step.Name +
      ' / ' + FPorts[LIndex].Endpoint.Port.Name + ' / ' +
      NyxFlowTypeName(FPorts[LIndex].Port.ValueType))
      .DragSource(FPorts[LIndex].Port.Direction = nfdOutput)
      .DropTarget(FPorts[LIndex].Port.Direction = nfdInput)
      .Hint(FPorts[LIndex].Port.Description).Done;
    LRow.Add(LButton);
    LRow.Add(NewNyxLabel(LRow.ID + '-help').WithText(FPorts[LIndex].Port.Description));
    Result.Add(LRow);
  end;
  LActions := NewNyxRow(FID + '-port-pages');
  LActions.Add(NewNyxButton(FID + '-previous').Configure.Text('Previous ports')
    .Enabled(FOffset > 0).Done);
  LActions.Add(NewNyxButton(FID + '-next').Configure.Text('Next ports')
    .Enabled(FOffset + CPageSize < Length(FPorts)).Done);
  Result.Add(LActions);
  Result.Add(NewNyxSwitch(FID + '-confirm').Configure.Text(
    'Confirm replacing or removing the selected wire')
    .Value(FConfirmed).Done);
  LActions := NewNyxRow(FID + '-actions');
  LActions.Add(NewNyxButton(FID + '-connect').WithText('Connect selected'));
  LActions.Add(NewNyxButton(FID + '-replace').WithText('Replace incoming wire'));
  LActions.Add(NewNyxButton(FID + '-remove').WithText('Remove selected wire'));
  Result.Add(LActions);
  Result.Add(NewNyxLabel(FID + '-message').WithText(FMessage));

  if FDefinition <> nil then
  begin
    Result.Add(NewNyxHeading(FID + '-wires-title').WithText('Existing wires'));
    for LIndex := FWireOffset to Min(FDefinition.WireCount - 1, FWireOffset + CPageSize - 1) do
    begin
      LWire := Wire(LIndex);
      Result.Add(NewNyxButton(FID + '-wire-' + TNyxText(IntToStr(LIndex))).WithText(
        LWire.Source.Step.Name + ' / ' + LWire.Source.Port.Name + ' -> ' +
        LWire.Target.Step.Name + ' / ' + LWire.Target.Port.Name));
    end;
    LActions := NewNyxRow(FID + '-wire-pages');
    LActions.Add(NewNyxButton(FID + '-wires-previous').Configure.Text('Previous wires')
      .Enabled(FWireOffset > 0).Done);
    LActions.Add(NewNyxButton(FID + '-wires-next').Configure.Text('Next wires')
      .Enabled(FWireOffset + CPageSize < FDefinition.WireCount).Done);
    Result.Add(LActions);
  end;
end;

function TConnectionEditor.FormFor(ANode: TNyxNode): TNyxNode;
begin
  Result := ANode;
  while (Result <> nil) and (Result.ID <> FID) do
  begin
    Result := Result.Parent;
  end;

  if (Result = nil) or not Result.Extensions.Has(NyxExtension(CForm)) or
    (Result.Extensions.Value(NyxExtension(CForm)).AsInteger <> FEpoch) then
  begin
    Result := nil;
  end;
end;

function TConnectionEditor.Handle(ANode: TNyxNode; const AEvent: TNyxEventInfo;
  const AResponse: INyxGestureResponse): Boolean;
var
  LLease: INyxToolConnectionEditor;
  LIndex: Integer;
  LAction: TNyxToolConnectionAction;
  LWire: TNyxFlowWire;
  LCompatible: Boolean;
begin
  LLease := Self;
  RequireActive;
  Result := False;

  if (ANode = nil) or (FormFor(ANode) = nil) then
  begin
    Exit;
  end;
  try

    if (ANode.ID = FID + '-graph') and (AEvent.Trigger = ntChange) then
    begin
      SelectFlow(NyxFlowRef(ANode.Prop(NyxAttributeName(atValue))));
      Exit(True);
    end;

    if (ANode.ID = FID + '-confirm') and (AEvent.Trigger = ntChange) and
      AEvent.HasValue and (AEvent.ValueKind = nskBoolean) then
    begin
      FConfirmed := AEvent.Value.AsBoolean;
      Exit(True);
    end;
    for LIndex := 0 to High(FPorts) do
    begin

      if ANode.ID <> FID + '-port-' + TNyxText(IntToStr(LIndex)) then
      begin
        Continue;
      end;

      if AEvent.Trigger = ntClick then
      begin

        if FPorts[LIndex].Port.Direction = nfdOutput then
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
        (FPorts[LIndex].Port.Direction = nfdOutput) and AResponse.CanRequest(ngcOfferDrag) then
      begin
        FSource := LIndex;
        FDragSource := LIndex;
        FDrag := True;
        AResponse.OfferDrag(NyxTransferCustom(NyxTransferFormat(CTransfer), FDragToken), [ndoLink]);
        Exit;
      end;

      if AEvent.Trigger = ntDragEnd then
      begin
        FDrag := False;
        Exit(True);
      end;
      LCompatible := FDrag and (FPorts[LIndex].Port.Direction = nfdInput) and
        (FPorts[FDragSource].Port.ValueType = FPorts[LIndex].Port.ValueType) and
        AEvent.Drag.Transfer.HasFormat(NyxTransferFormat(CTransfer)) and
        (ndoLink in AEvent.Drag.Allowed) and AResponse.CanRequest(ngcAcceptDrop);

      if LCompatible and (AEvent.Trigger in [ntDragEnter, ntDragOver]) then
      begin
        AResponse.AcceptDrop(ndoLink);
        Exit;
      end;

      if LCompatible and (AEvent.Trigger = ntDrop) and AEvent.Drag.Transfer.Readable and
        (AEvent.Drag.Transfer.TextFor(NyxTransferFormat(CTransfer)) = FDragToken) then
      begin
        FTarget := LIndex;
        FDrag := False;
        Apply(FPorts[FDragSource].Endpoint, FPorts[FTarget].Endpoint, ntcaConnect, False);
        AResponse.AcceptDrop(ndoLink);
        Exit(True);
      end;
      Exit;
    end;

    if AEvent.Trigger <> ntClick then
    begin
      Exit;
    end;

    if ANode.ID = FID + '-previous' then
    begin
      FOffset := Max(0, FOffset - CPageSize);
    end
    else if ANode.ID = FID + '-next' then
    begin
      FOffset := Min(Max(0, Length(FPorts) - 1), FOffset + CPageSize);
    end
    else if ANode.ID = FID + '-wires-previous' then
    begin
      FWireOffset := Max(0, FWireOffset - CPageSize);
    end
    else if (ANode.ID = FID + '-wires-next') and (FDefinition <> nil) then
    begin
      FWireOffset := Min(Max(0, FDefinition.WireCount - 1), FWireOffset + CPageSize);
    end
    else if (ANode.ID = FID + '-connect') or (ANode.ID = FID + '-replace') or
      (ANode.ID = FID + '-remove') then
    begin

      if (FSource < 0) or (FTarget < 0) then
      begin
        raise ENyxFlow.Create('Choose both an output and an input');
      end;
      LAction := ntcaConnect;

      if ANode.ID = FID + '-replace' then
      begin
        LAction := ntcaReplace;
      end
      else if ANode.ID = FID + '-remove' then
      begin
        LAction := ntcaRemove;
      end;
      Apply(FPorts[FSource].Endpoint, FPorts[FTarget].Endpoint, LAction, FConfirmed);
    end
    else
    begin

      if FDefinition <> nil then
      begin
        for LIndex := FWireOffset to Min(FDefinition.WireCount - 1, FWireOffset + CPageSize - 1) do
        begin

          if ANode.ID = FID + '-wire-' + TNyxText(IntToStr(LIndex)) then
          begin
            LWire := Wire(LIndex);
            FSource := IndexOf(LWire.Source);
            FTarget := IndexOf(LWire.Target);
            FConfirmed := False;
            Exit(True);
          end;
        end;
      end;
      Exit;
    end;
    Result := True;
  except
    on E: Exception do
    begin
      FDrag := False;
      FMessage := TNyxText(E.Message);
      Result := True;
    end;
  end;
end;

function NewNyxToolConnectionEditor(const AID: TNyxText;
  const AView: INyxCanvasWorkspaceView): INyxToolConnectionEditor;
begin
  Result := TConnectionEditor.Create(AID, AView);
end;

end.
