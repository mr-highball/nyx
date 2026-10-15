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
{ Bounded semantic discovery only; all mutations use the existing paired
  nyx_transaction boundary and public typed Flow changes. }
function TNyxAgentSession.LogicQuery(const AArguments: TNyxDataValue): TNyxDataValue;
var
  LScope: TNyxText;
  LOffset: Integer;
  LLimit: Integer;
  LIndex: Integer;
  LPosition: Integer;
  LCount: Integer;
  LTotal: Integer;
  LTextTotal: Integer;
  LTextOffset: Integer;
  LTextLimit: Integer;
  LStep: Integer;
  LFound: Integer;
  LCatalog: INyxLogicCatalog;
  LDefinition: INyxFlowDefinition;
  LDescriptor: TNyxLogicDescriptor;
  LPort: TNyxFlowPort;
  LItems: array of TNyxDataValue;
  LLabels: array of TNyxDataValue;
  LText: TNyxText;
  LReference: TNyxFlowRef;
  LRoutes: INyxFlowRoutes;
  LRoute: TNyxFlowRoute;
  LRouteData: TNyxDataValue;
  LConditions: TNyxDataValue;
begin
  LScope := TextArgument(AArguments, 'scope', 'graphs');

  if (LScope = 'programs') or (Pos('program-', LScope) = 1) then
  begin
    Exit(ProgramQuery(AArguments));
  end;
  NyxAgentFields(AArguments, '|scope|flow|step|id|route|offset|limit|query|expectedRevision|textOffset|textLimit|');

  if NyxAgentHas(AArguments, 'expectedRevision') then
  begin
    RequireRevision(AArguments);
  end;
  LScope := TextArgument(AArguments, 'scope', 'graphs');
  LOffset := IntegerArgument(AArguments, 'offset', 0, 0, 100000);
  LLimit := IntegerArgument(AArguments, 'limit', 8, 1, 32);
  LTextOffset := IntegerArgument(AArguments, 'textOffset', 0, 0, 10000000);
  LTextLimit := IntegerArgument(AArguments, 'textLimit', 512, 1, 1024);
  SetLength(LItems, LLimit);
  LCount := 0;
  LTotal := 0;

  if LScope = 'catalog' then
  begin
    LCatalog := NewNyxLogicCatalog;
    for LIndex := 0 to LCatalog.Count - 1 do
    begin

      if not LCatalog.Matches(LIndex, TextArgument(AArguments, 'query')) then
      begin
        Continue;
      end;

      if (LTotal >= LOffset) and (LCount < LLimit) then
      begin
        LDescriptor := LCatalog.Descriptor(LIndex);
        SetLength(LLabels, LDescriptor.LabelCount);
        for LPosition := 0 to High(LLabels) do
        begin
          LLabels[LPosition] := NyxData(LDescriptor.LabelAt(LPosition));
        end;
        LItems[LCount] := NyxObject([
          NyxField('kind', NyxData(LDescriptor.Kind.Name)),
          NyxField('title', NyxData(CaptionText(LDescriptor.Title, 256))),
          NyxField('description', NyxData(CaptionText(LDescriptor.Description, 512))),
          NyxField('group', NyxData(NyxLogicGroupName(LDescriptor.Group))),
          NyxField('labels', NyxArray(LLabels)),
          NyxField('configuration', LCatalog.Configuration(LIndex))]);
        Inc(LCount);
      end;
      Inc(LTotal);
    end;
  end
  else if (LScope = 'routes') or (LScope = 'route-inputs') or
    (LScope = 'route-outputs') or (LScope = 'route-row-outputs') then
  begin
    LRoutes := NyxControlFlowRoutes(FSession.Document.Find(TextArgument(AArguments, 'id')));

    if LScope = 'routes' then
    begin
      LTotal := LRoutes.Count;
      for LIndex := LOffset to LTotal - 1 do
      begin

        if LCount >= LLimit then
        begin
          Break;
        end;
        LRoute := LRoutes.Route(LIndex);
        LItems[LCount] := NyxObject([NyxField('route', NyxData(LRoute.ID.Name)),
          NyxField('flow', NyxData(LRoute.Flow.Name)),
          NyxField('trigger', NyxData(NyxTriggerName(LRoute.Trigger))),
          NyxField('event', NyxData(LRoute.EventName.Name)),
          NyxField('callbacks', NyxData(Ord(LRoute.CallbackMode))),
          NyxField('policy', NyxData(Ord(LRoute.Policy))),
          NyxField('concurrency', NyxData(Ord(LRoute.Concurrency))),
          NyxField('enabled', NyxData(LRoute.IsEnabled)),
          NyxField('maximumRuns', NyxData(LRoute.RunLimit)),
          NyxField('inputs', NyxData(LRoute.InputCount)),
          NyxField('outputs', NyxData(LRoute.OutputCount)),
          NyxField('rowOutputs', NyxData(LRoute.RowOutputCount))]);
        Inc(LCount);
      end;
    end
    else
    begin
      LFound := -1;
      for LIndex := 0 to LRoutes.Count - 1 do
      begin

        if LRoutes.Route(LIndex).ID.Name = TextArgument(AArguments, 'route') then
        begin
          LFound := LIndex;
          Break;
        end;
      end;

      if LFound < 0 then
      begin
        raise ENyxModel.Create('Choose an exact local Logic route');
      end;
      LRouteData := LRoutes.Route(LFound).ToData.Field('inputs');

      if LScope = 'route-outputs' then
      begin
        LRouteData := LRoutes.Route(LFound).ToData.Field('outputs');
      end;

      if LScope = 'route-row-outputs' then
      begin
        LRouteData := NyxArray([]);

        if LRoutes.Route(LFound).RowOutputCount > 0 then
        begin
          LRouteData := LRoutes.Route(LFound).ToData.Field('rowOutputs');
        end;
      end;
      LTotal := LRouteData.Count;
      for LIndex := LOffset to LTotal - 1 do
      begin

        if LCount >= LLimit then
        begin
          Break;
        end;
        { Constants can be large resources/data. Publish only source/destination
          facts here; exact values use bounded Unicode scalar text windows. }
        LText := TextSpan(LRouteData.Item(LIndex).ToJSON, LTextOffset, LTextLimit, LTextTotal);
        LItems[LCount] := NyxObject([NyxField('index', NyxData(LIndex)),
          NyxField('textOffset', NyxData(LTextOffset)),
          NyxField('totalScalars', NyxData(LTextTotal)),
          NyxField('text', NyxData(LText))]);
        Inc(LCount);
      end;
    end;
  end
  else if LScope = 'graphs' then
  begin
    LTotal := FSession.Document.Flows.Count;
    for LIndex := LOffset to LTotal - 1 do
    begin

      if LCount >= LLimit then
      begin
        Break;
      end;
      LReference := FSession.Document.Flows.Reference(LIndex);
      LDefinition := FSession.Document.Flows.Definition(LReference);
      LItems[LCount] := NyxObject([NyxField('flow', NyxData(LReference.Name)),
        NyxField('steps', NyxData(LDefinition.StepCount)), NyxField('wires', NyxData(LDefinition.WireCount))]);
      Inc(LCount);
    end;
  end
  else
  begin
    LReference := NyxFlowRef(TextArgument(AArguments, 'flow'));
    LDefinition := FSession.Document.Flows.Definition(LReference);
    LStep := -1;

    if (LScope = 'ports') or (LScope = 'configuration') then
    begin
      LText := TextArgument(AArguments, 'step');
      for LIndex := 0 to LDefinition.StepCount - 1 do
      begin

        if LDefinition.Step(LIndex).Name = LText then
        begin
          LStep := LIndex;
          Break;
        end;
      end;

      if LStep < 0 then
      begin
        raise ENyxModel.Create('Choose an exact saved Logic step');
      end;
      LDescriptor := LDefinition.Descriptor(LStep);
    end;

    if LScope = 'configuration' then
    begin
      LText := TextSpan(LDefinition.Configuration(LStep).ToJSON,
        LTextOffset, LTextLimit, LTextTotal);
      Exit(NyxObject([NyxField('revision', NyxData(FRevision)),
        NyxField('flow', NyxData(LReference.Name)), NyxField('step', NyxData(LDefinition.Step(LStep).Name)),
        NyxField('text', NyxData(LText)), NyxField('textOffset', NyxData(LTextOffset)),
        NyxField('totalScalars', NyxData(LTextTotal)),
        NyxField('truncated', NyxData((LTextOffset > 0) or (LTextTotal > LTextOffset + LTextLimit)))]));
    end;

    if LScope = 'steps' then
    begin
      LTotal := LDefinition.StepCount;
    end
    else if LScope = 'wires' then
    begin
      LTotal := LDefinition.WireCount;
    end
    else if LScope = 'conditions' then
    begin
      LConditions := NyxArray([]);

      if LDefinition.ToData.Field('version').AsInteger in [2, 3] then
      begin
        LConditions := LDefinition.ToData.Field('conditions');
      end;
      LTotal := LConditions.Count;
    end
    else if LScope = 'ports' then
    begin
      LTotal := LDescriptor.PortCount;
    end
    else
    begin
      raise ENyxModel.Create('Choose catalog, graphs, steps, ports, wires, conditions or configuration');
    end;
    for LIndex := LOffset to LTotal - 1 do
    begin

      if LCount >= LLimit then
      begin
        Break;
      end;

      if LScope = 'steps' then
      begin
        LDescriptor := LDefinition.Descriptor(LIndex);
        LItems[LCount] := NyxObject([NyxField('step', NyxData(LDefinition.Step(LIndex).Name)),
          NyxField('kind', NyxData(LDescriptor.Kind.Name)),
          NyxField('title', NyxData(CaptionText(LDescriptor.Title, 256))),
          NyxField('description', NyxData(CaptionText(LDescriptor.Description, 512))),
          NyxField('ports', NyxData(LDescriptor.PortCount))]);
      end
      else if LScope = 'wires' then
      begin
        LItems[LCount] := LDefinition.ToData.Field('wires').Item(LIndex);
      end
      else if LScope = 'conditions' then
      begin
        LItems[LCount] := LConditions.Item(LIndex);
      end
      else
      begin
        LPort := LDescriptor.Port(LIndex);
        LText := 'input';

        if LPort.Direction = nfdOutput then
        begin
          LText := 'output';
        end;
        LItems[LCount] := NyxObject([NyxField('port', NyxData(LPort.Reference.Name)),
          NyxField('direction', NyxData(LText)),
          NyxField('type', NyxData(NyxFlowTypeName(LPort.ValueType))),
          NyxField('description', NyxData(CaptionText(LPort.Description, 512)))]);

        if LPort.Optional then
        begin
          LItems[LCount] := NyxObject([NyxField('port', NyxData(LPort.Reference.Name)),
            NyxField('direction', NyxData(LText)),
            NyxField('type', NyxData(NyxFlowTypeName(LPort.ValueType))),
            NyxField('description', NyxData(CaptionText(LPort.Description, 512))),
            NyxField('optional', NyxData(True))]);
        end;
      end;
      Inc(LCount);
    end;
  end;
  SetLength(LItems, LCount);
  Result := NyxObject([NyxField('revision', NyxData(FRevision)),
    NyxField('scope', NyxData(LScope)), NyxField('total', NyxData(LTotal)),
    NyxField('offset', NyxData(LOffset)), NyxField('items', NyxArray(LItems))]);
end;
