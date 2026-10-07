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

{ Private design-command preparation/publication for nyx.studio.session.
  Only immutable tickets cross processors. Existing session methods remain the
  single authoring implementation; a worker invokes them on independent owners. }

type
  TPreparedDesign = class(TInterfacedObject, INyxPreparedDesign, INyxSchemaAction)
  private
    FRequest: TNyxStudioDesignRequest;
    FDocument: TNyxDocument;
    FWorkspace: TNyxSourceWorkspace;
    FSource: TNyxText;
    FDesign: TNyxText;
    FSnapshot: TNyxText;
    FDiagnostic: TNyxSourceDiagnostic;
    FSelection: TNyxText;
    FView: TNyxText;
    FNextID: Integer;
    FAddedHandler: TNyxHandlerRef;
  public
    destructor Destroy; override;
    procedure Execute;
    function Matches(const ARequest: TNyxStudioDesignRequest): Boolean;
    function GetSource: TNyxText;
    function GetDesign: TNyxText;
    function GetDiagnostic: TNyxSourceDiagnostic;
    function GetSchemaRevision: Integer;
    function GetSelection: TNyxText;
    function GetView: TNyxText;
    function GetNextID: Integer;
    function GetAddedHandler: TNyxHandlerRef;
    function ToData: TNyxDataValue;
    procedure Take(var ADocument: TNyxDocument; var AWorkspace: TNyxSourceWorkspace);
  end;

procedure ValidateEventIntent(const AIntent: TNyxStudioEventIntent);
begin

  if (Ord(AIntent.Action) < Ord(Low(TNyxStudioEventAction))) or
    (Ord(AIntent.Action) > Ord(High(TNyxStudioEventAction))) or
    (Ord(AIntent.Policy) < Ord(Low(TNyxExecutionPolicy))) or
    (Ord(AIntent.Policy) > Ord(High(TNyxExecutionPolicy))) then
  begin
    raise ENyxModel.Create('Callback intent contains an unknown closed choice');
  end;

  if (AIntent.Trigger = ntNamed) <> (AIntent.Name.Name <> '') then
  begin
    raise ENyxModel.Create('Named callback intent requires its exact event reference');
  end;

  if not NyxIsRuntimeTrigger(AIntent.Trigger) and (AIntent.Trigger <> ntNamed) then
  begin
    raise ENyxModel.Create('Callback intent requires a published runtime event');
  end;

  if AIntent.Action = seaRemove then
  begin

    if (AIntent.ID.Name = '') or (AIntent.Handler.Name = '') then
    begin
      raise ENyxModel.Create('Removal requires the exact reviewed registration and handler');
    end;
  end
  else if (AIntent.ID.Name <> '') or (AIntent.Handler.Name <> '') then
  begin
    raise ENyxModel.Create('Only removal intent names an existing registration');
  end;

  if (AIntent.Action <> seaPolicy) and (AIntent.Policy <> neSequential) then
  begin
    raise ENyxModel.Create('Only policy intent selects an execution policy');
  end;
end;

function HasEventRegistration(ANode: TNyxNode; const AIntent: TNyxStudioEventIntent): Boolean;
var
  LEvents: TNyxAuthoredEventInfos;
  LIndex: Integer;
  LCallback: Integer;
begin
  Result := False;

  if ANode = nil then
  begin
    Exit;
  end;
  LEvents := NyxAuthoredEvents(ANode);
  for LIndex := 0 to High(LEvents) do
  begin

    if (LEvents[LIndex].Trigger <> AIntent.Trigger) or
      (LEvents[LIndex].Name.Name <> AIntent.Name.Name) then
    begin
      Continue;
    end;
    for LCallback := 0 to High(LEvents[LIndex].Callbacks) do
    begin

      if (LEvents[LIndex].Callbacks[LCallback].ID.Name = AIntent.ID.Name) and
        (LEvents[LIndex].Callbacks[LCallback].Handler.Name = AIntent.Handler.Name) then
      begin
        Exit(True);
      end;
    end;
  end;
end;

procedure TNyxStudioSession.ApplyEventIntent(const AIntent: TNyxStudioEventIntent;
  out AHandler: TNyxHandlerRef; out ALine: Integer);
var
  LMetadata: TNyxEventSchemas;
  LIndex: Integer;
  LSupported: Boolean;
  LProjection: TNyxNode;
begin
  AHandler := Default(TNyxHandlerRef);
  ALine := 0;
  ValidateEventIntent(AIntent);
  LMetadata := NyxEventsMetadata(Selected, Document);
  LSupported := False;
  for LIndex := 0 to High(LMetadata) do
  begin
    LSupported := LSupported or ((LMetadata[LIndex].Trigger = AIntent.Trigger) and
      (LMetadata[LIndex].Name.Name = AIntent.Name.Name));
  end;

  if not LSupported then
  begin
    raise ENyxModel.Create('The selected component no longer publishes this callback');
  end;

  if AIntent.Action = seaRemove then
  begin
    LProjection := SelectedProjection;
    try

      if not HasEventRegistration(LProjection, AIntent) then
      begin
        raise ENyxModel.Create('This registration changed; review its new removal warning');
      end;
    finally
      LProjection.Free;
    end;
  end;
  case AIntent.Action of
    seaAdd:
      begin

        if AIntent.Trigger = ntNamed then
        begin
          AHandler := AddCallback(AIntent.Name, ALine);
        end
        else
        begin
          AHandler := AddCallback(AIntent.Trigger, ALine);
        end;
      end;
    seaPolicy:
      begin

        if AIntent.Trigger = ntNamed then
        begin
          SetCallbackPolicy(AIntent.Name, AIntent.Policy);
        end
        else
        begin
          SetCallbackPolicy(AIntent.Trigger, AIntent.Policy);
        end;
      end;
    seaRemove:
      begin

        if AIntent.Trigger = ntNamed then
        begin
          RemoveCallback(AIntent.Name, AIntent.ID);
        end
        else
        begin
          RemoveCallback(AIntent.Trigger, AIntent.ID);
        end;
      end;
  end;
end;

function TNyxStudioPendingDesign.EventPolicy(const AOwner: TNyxText;
  ATrigger: TNyxTrigger; const AName: TNyxEventRef;
  out APolicy: TNyxExecutionPolicy): Boolean;
var
  LIndex: Integer;
begin
  APolicy := neSequential;
  for LIndex := High(Events) downto 0 do
  begin

    if (Events[LIndex].Owner = AOwner) and (Events[LIndex].Intent.Trigger = ATrigger) and
      (Events[LIndex].Intent.Name.Name = AName.Name) and
      (Events[LIndex].Intent.Action = seaPolicy) then
    begin
      APolicy := Events[LIndex].Intent.Policy;
      Exit(True);
    end;
  end;
  Result := False;
end;

function TNyxStudioPendingDesign.EventLocked(const AOwner: TNyxText;
  ATrigger: TNyxTrigger; const AName: TNyxEventRef): Boolean;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(Events) do
  begin

    if (Events[LIndex].Owner = AOwner) and (Events[LIndex].Intent.Trigger = ATrigger) and
      (Events[LIndex].Intent.Name.Name = AName.Name) and
      (Events[LIndex].Intent.Action = seaRemove) then
    begin
      Exit(True);
    end;
  end;
  Result := False;
end;

function TNyxStudioPendingDesign.PropertyValue(const ASelection, AKey: TNyxText;
  out AValue: TNyxText): Boolean;
var
  LIndex: Integer;
begin
  AValue := '';
  for LIndex := High(Fields) downto 0 do
  begin

    if (Fields[LIndex].Selection = ASelection) and (Fields[LIndex].Key = AKey) then
    begin
      AValue := Fields[LIndex].Value;
      Exit(True);
    end;
  end;
  Result := False;
end;

function TNyxStudioPendingDesign.ApplyCanvasValues(ARoot: TNyxNode;
  const AView: TNyxText): Boolean;
var
  LIndex: Integer;
  LField: TNyxNode;
begin
  Result := False;

  if ARoot = nil then
  begin
    Exit;
  end;

  if not ARoot.IsRealized then
  begin
    raise ENyxModel.Create('Pending canvas values require an independent realized presentation');
  end;
  for LIndex := 0 to High(CanvasValues) do
  begin

    if CanvasValues[LIndex].View <> AView then
    begin
      Continue;
    end;
    LField := ARoot.Find(CanvasValues[LIndex].RuntimeID);

    if (LField <> nil) and (LField.DesignID = CanvasValues[LIndex].Owner) and
      (LField.Prop(NyxAttributeName(atValue)) <> CanvasValues[LIndex].Value) then
    begin
      { This is unadmitted adapter text, not a typed authored default. Numeric
        partial input must remain visible until its queued admission completes. }
      LField.SetProp(NyxAttributeName(atValue), CanvasValues[LIndex].Value);
      Result := True;
    end;
  end;
end;

function PendingStateField(const AFields: array of TNyxStudioPendingField;
  const AName: TNyxText; AKind: TNyxStateKind; out AValue: TNyxText): Boolean;
var
  LIndex: Integer;
begin
  AValue := '';
  for LIndex := High(AFields) downto 0 do
  begin

    if (AFields[LIndex].Selection = AName) and
      (AFields[LIndex].Key = NyxStateKindName(AKind)) then
    begin
      AValue := AFields[LIndex].Value;
      Exit(True);
    end;
  end;
  Result := False;
end;

function TNyxStudioPendingDesign.StateValue(const AName: TNyxText;
  AKind: TNyxStateKind; out AValue: TNyxText): Boolean;
begin
  Result := PendingStateField(StateValues, AName, AKind, AValue);
end;

function TNyxStudioPendingDesign.StateName(const AName: TNyxText;
  AKind: TNyxStateKind; out AValue: TNyxText): Boolean;
begin
  Result := PendingStateField(StateNames, AName, AKind, AValue);
end;

function TNyxStudioPendingDesign.StateEditorInput(const AName: TNyxText;
  AKind: TNyxStateKind; out AInput: TNyxStudioStateInput): Boolean;
var
  LIndex: Integer;
begin
  AInput := ssiText;
  for LIndex := High(StateValues) downto 0 do
  begin

    if (StateValues[LIndex].Selection = AName) and
      (StateValues[LIndex].Key = NyxStateKindName(AKind)) then
    begin
      AInput := StateValues[LIndex].StateInput;
      Exit(True);
    end;
  end;
  Result := False;
end;

function TNyxStudioPendingDesign.StateLocked(const AName: TNyxText): Boolean;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(RenamingStates) do
  begin

    if RenamingStates[LIndex] = AName then
    begin
      Exit(True);
    end;
  end;
  Result := False;
end;

function TNyxStudioPendingDesign.Binding(const AOwner: TNyxText;
  ATarget: TNyxBindingProperty; out ASpec: TNyxBindingSpec;
  out AInherit: Boolean): Boolean;
var
  LIndex: Integer;
begin
  ASpec := TNyxBindingSpec.Clear(ATarget);
  AInherit := False;
  for LIndex := High(Bindings) downto 0 do
  begin

    if (Bindings[LIndex].Owner = AOwner) and (Bindings[LIndex].Spec.Target = ATarget) then
    begin
      ASpec := Bindings[LIndex].Spec.Copy;
      AInherit := Bindings[LIndex].Inherit;
      Exit(True);
    end;
  end;
  Result := False;
end;

function DesignBindingData(const AEdit: TNyxStudioDesignEdit): TNyxDataValue;
begin
  Result := NyxNull;

  if AEdit.Action in [sdaSetBinding, sdaInheritBinding] then
  begin
    AEdit.Binding.Validate;
    Result := NyxObject([
      NyxField('target', NyxData(Ord(AEdit.Binding.Target))),
      NyxField('name', NyxData(AEdit.Binding.StateName)),
      NyxField('kind', NyxData(Ord(AEdit.Binding.ValueKind))),
      NyxField('direction', NyxData(Ord(AEdit.Binding.Direction))),
      NyxField('cleared', NyxData(AEdit.Binding.Cleared))]);
  end;
end;

function DesignEventData(const AEdit: TNyxStudioDesignEdit): TNyxDataValue;
begin
  Result := NyxNull;

  if AEdit.Action = sdaEvent then
  begin
    ValidateEventIntent(AEdit.Event);
    Result := NyxObject([
      NyxField('action', NyxData(Ord(AEdit.Event.Action))),
      NyxField('trigger', NyxData(Ord(AEdit.Event.Trigger))),
      NyxField('name', NyxData(AEdit.Event.Name.Name)),
      NyxField('policy', NyxData(Ord(AEdit.Event.Policy))),
      NyxField('id', NyxData(AEdit.Event.ID.Name)),
      NyxField('handler', NyxData(AEdit.Event.Handler.Name))]);
  end;
end;

function TNyxStudioDesignRequest.ToData: TNyxDataValue;
var
  LCollection: TNyxDataValue;
  LPlacement: TNyxDataValue;
  LResize: TNyxDataValue;
  LPresentation: TNyxDataValue;
  LPosition: TNyxDataValue;
  LEdit: TNyxDataValue;
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LCount: Integer;
  LVersion: Integer;
begin
  LCollection := NyxNull;
  LPlacement := NyxNull;
  LResize := NyxNull;
  LPresentation := NyxNull;
  LPosition := NyxNull;

  if FEdit.Action = sdaCollection then
  begin
    LCollection := FEdit.Collection.ToData;
  end;

  if FEdit.Action = sdaPlacement then
  begin
    LPlacement := FEdit.Placement.ToData;
  end;

  if FEdit.Action = sdaResize then
  begin
    LResize := FEdit.Resize.ToData;
  end;

  if FEdit.Action = sdaPresentation then
  begin
    LPresentation := FEdit.Presentation.ToData;
  end;

  if FEdit.Action = sdaPosition then
  begin
    LPosition := FEdit.Position.ToData;
  end;
  LEdit := NyxObject([
      NyxField('action', NyxData(Ord(FEdit.Action))),
      NyxField('selection', NyxData(FEdit.Selection)), NyxField('view', NyxData(FEdit.View)),
      NyxField('name', NyxData(FEdit.Name)), NyxField('value', NyxData(FEdit.Value)),
      NyxField('direction', NyxData(Ord(FEdit.Direction))),
      NyxField('platform', NyxData(Ord(FEdit.Platform))),
      NyxField('stateInput', NyxData(Ord(FEdit.StateInput))),
      NyxField('binding', DesignBindingData(FEdit)),
      NyxField('event', DesignEventData(FEdit)),
      NyxField('collection', LCollection), NyxField('placement', LPlacement),
      NyxField('resize', LResize), NyxField('presentation', LPresentation),
      NyxField('position', LPosition)]);
  LVersion := 9;

  if FEdit.Action in [sdaContent, sdaValueDomain, sdaMenu] then
  begin
    { Existing operations retain their deployed v9 shape. Appended recipe,
      value-policy and menu intents need new envelopes; old helpers stay compatible. }
    LVersion := 10;
    { Cache the method result before using it as an index. The installed pas2js
      compiler mis-emits an implicit parameterless record call inside brackets. }
    LCount := LEdit.Count;
    SetLength(LFields, LCount + 1);
    for LIndex := 0 to LCount - 1 do
    begin
      LFields[LIndex] := NyxField(LEdit.Key(LIndex), LEdit.Field(LEdit.Key(LIndex)));
    end;

    if FEdit.Action = sdaMenu then
    begin
      { Only the new menu intent needs v12; deployed envelopes keep their shape. }
      LVersion := 12;
      LFields[LCount] := NyxField('menu', NyxObject([
        NyxField('command', FEdit.Menu.ToData),
        NyxField('baseline', NyxData(FEdit.MenuBaseline))]));
    end
    else if FEdit.Action = sdaValueDomain then
    begin
      { New policy commands alone use v11. Existing deployed envelopes retain
        their exact shape; no absent policy payload is appended to other intents. }
      LVersion := 11;
      LFields[LCount] := NyxField('valueDomain', NyxObject([
        NyxField('command', FEdit.ValueDomain.ToData),
        NyxField('baseline', NyxData(FEdit.ValueDomainBaseline))]));
    end
    else
    begin
      LFields[LCount] := NyxField('content', NyxObject([
        NyxField('command', FEdit.Content.ToData),
        NyxField('baseline', NyxData(FEdit.ContentBaseline))]));
    end;
    LEdit := NyxObject(LFields);
  end;
  Result := NyxObject([
    NyxField('version', NyxData(LVersion)), NyxField('owner', NyxData(FOwner)),
    NyxField('generation', NyxData(FGeneration)), NyxField('nextId', NyxData(FNextID)),
    NyxField('schemaRevision', NyxData(FSchemaRevision)),
    NyxField('pair', NyxData(EncodeNyxProject(FPair))), NyxField('edit', LEdit)]);
end;

function TNyxStudioDesignRequest.SameRequest(const AOther: TNyxStudioDesignRequest): Boolean;
begin
  { Compare values directly natively; JSON encoding is only the browser worker
    boundary, never an extra full-project encoding during ordinary UI delivery. }
  Result := (FOwner = AOther.FOwner) and (FGeneration = AOther.FGeneration) and
    (FNextID = AOther.FNextID) and (FSchemaRevision = AOther.FSchemaRevision) and
    (FPair.Design = AOther.FPair.Design) and (FPair.Source = AOther.FPair.Source) and
    (FPair.Pending = AOther.FPair.Pending) and (FPair.Draft = AOther.FPair.Draft) and
    (FPair.DraftBase = AOther.FPair.DraftBase) and (FEdit.Action = AOther.FEdit.Action) and
    (FEdit.Selection = AOther.FEdit.Selection) and (FEdit.View = AOther.FEdit.View) and
    (FEdit.Name = AOther.FEdit.Name) and (FEdit.Value = AOther.FEdit.Value) and
    (FEdit.Direction = AOther.FEdit.Direction) and (FEdit.Platform = AOther.FEdit.Platform) and
    (FEdit.StateInput = AOther.FEdit.StateInput) and
    (FEdit.Binding.Target = AOther.FEdit.Binding.Target) and
    (FEdit.Binding.StateName = AOther.FEdit.Binding.StateName) and
    (FEdit.Binding.ValueKind = AOther.FEdit.Binding.ValueKind) and
    (FEdit.Binding.Direction = AOther.FEdit.Binding.Direction) and
    (FEdit.Binding.Cleared = AOther.FEdit.Binding.Cleared) and
    (FEdit.Event.Action = AOther.FEdit.Event.Action) and
    (FEdit.Event.Trigger = AOther.FEdit.Event.Trigger) and
    (FEdit.Event.Name.Name = AOther.FEdit.Event.Name.Name) and
    (FEdit.Event.Policy = AOther.FEdit.Event.Policy) and
    (FEdit.Event.ID.Name = AOther.FEdit.Event.ID.Name) and
    (FEdit.Event.Handler.Name = AOther.FEdit.Event.Handler.Name);

  if Result and (FEdit.Action = sdaCollection) then
  begin
    Result := FEdit.Collection.SameIntent(AOther.FEdit.Collection);
  end;

  if Result and (FEdit.Action = sdaPlacement) then
  begin
    Result := FEdit.Placement.SameChange(AOther.FEdit.Placement);
  end;

  if Result and (FEdit.Action = sdaResize) then
  begin
    Result := FEdit.Resize.SameChange(AOther.FEdit.Resize);
  end;

  if Result and (FEdit.Action = sdaPresentation) then
  begin
    Result := FEdit.Presentation.Same(AOther.FEdit.Presentation);
  end;

  if Result and (FEdit.Action = sdaPosition) then
  begin
    Result := FEdit.Position.SameChange(AOther.FEdit.Position);
  end;

  if Result and (FEdit.Action = sdaContent) then
  begin
    Result := FEdit.Content.Same(AOther.FEdit.Content) and
      (FEdit.ContentBaseline = AOther.FEdit.ContentBaseline);
  end;

  if Result and (FEdit.Action = sdaValueDomain) then
  begin
    Result := FEdit.ValueDomain.Same(AOther.FEdit.ValueDomain) and
      (FEdit.ValueDomainBaseline = AOther.FEdit.ValueDomainBaseline);
  end;

  if Result and (FEdit.Action = sdaMenu) then
  begin
    Result := FEdit.Menu.Same(AOther.FEdit.Menu) and
      (FEdit.MenuBaseline = AOther.FEdit.MenuBaseline);
  end;
end;

function ReadNyxStudioDesignRequest(const AData: TNyxDataValue): TNyxStudioDesignRequest;
var
  LEdit: TNyxDataValue;
  LAction: Integer;
  LDirection: Integer;
  LPlatform: Integer;
  LInput: Integer;
  LBinding: TNyxDataValue;
  LTarget: Integer;
  LKind: Integer;
  LFlow: Integer;
  LEvent: TNyxDataValue;
  LTrigger: Integer;
  LEventAction: Integer;
  LPolicy: Integer;
  LVersion: Integer;
  LContent: TNyxDataValue;
begin
  LVersion := AData.Field('version').AsInteger;

  if (AData.Kind <> ndObject) or (AData.Count <> 7) or
    ((LVersion <> 5) and (LVersion <> 6) and (LVersion <> 7) and (LVersion <> 8) and
    (LVersion <> 9) and (LVersion <> 10) and (LVersion <> 11) and (LVersion <> 12)) then
  begin
    raise ENyxModel.Create('Design processor requires its exact versioned ticket');
  end;
  Result := Default(TNyxStudioDesignRequest);
  Result.FOwner := AData.Field('owner').AsText;
  Result.FGeneration := AData.Field('generation').AsInteger;
  Result.FNextID := AData.Field('nextId').AsInteger;
  Result.FSchemaRevision := AData.Field('schemaRevision').AsInteger;
  Result.FPair := DecodeNyxProject(AData.Field('pair').AsText);
  LEdit := AData.Field('edit');

  if (Result.FOwner = '') or (Result.FGeneration < 0) or (Result.FNextID < 0) or
    (Result.FSchemaRevision < 0) or (LEdit.Kind <> ndObject) or
    ((LVersion = 5) and (LEdit.Count <> 11)) or
    ((LVersion = 6) and (LEdit.Count <> 12)) or
    ((LVersion = 7) and (LEdit.Count <> 13)) or
    ((LVersion = 8) and (LEdit.Count <> 14)) or
    ((LVersion = 9) and (LEdit.Count <> 15)) or
    ((LVersion in [10, 11, 12]) and (LEdit.Count <> 16)) then
  begin
    raise ENyxModel.Create('Design processor ticket has invalid identity or intent');
  end;
  LAction := LEdit.Field('action').AsInteger;

  if (LAction < Ord(Low(TNyxStudioDesignAction))) or
    (LAction > Ord(High(TNyxStudioDesignAction))) or
    ((LVersion = 5) and (LAction > Ord(sdaCollection))) or
    ((LVersion = 6) and (LAction > Ord(sdaPlacement))) or
    ((LVersion = 7) and (LAction > Ord(sdaResize))) or
    ((LVersion = 8) and (LAction > Ord(sdaPresentation))) or
    ((LVersion = 9) and (LAction > Ord(sdaPosition))) or
    ((LVersion = 10) and (LAction > Ord(sdaContent))) or
    ((LVersion = 11) and (LAction <> Ord(sdaValueDomain))) or
    ((LVersion = 12) and (LAction <> Ord(sdaMenu))) then
  begin
    raise ENyxModel.Create('Design processor action is outside its closed contract');
  end;
  Result.FEdit.Action := TNyxStudioDesignAction(LAction);
  Result.FEdit.Selection := LEdit.Field('selection').AsText;
  Result.FEdit.View := LEdit.Field('view').AsText;
  Result.FEdit.Name := LEdit.Field('name').AsText;
  Result.FEdit.Value := LEdit.Field('value').AsText;
  LDirection := LEdit.Field('direction').AsInteger;

  if (LDirection < Ord(Low(TNyxStudioMoveDirection))) or
    (LDirection > Ord(High(TNyxStudioMoveDirection))) then
  begin
    raise ENyxModel.Create('Design processor move is outside its closed contract');
  end;
  Result.FEdit.Direction := TNyxStudioMoveDirection(LDirection);
  LPlatform := LEdit.Field('platform').AsInteger;

  if (LPlatform < Ord(Low(TNyxPlatform))) or (LPlatform > Ord(High(TNyxPlatform))) then
  begin
    raise ENyxModel.Create('Design processor platform is outside its closed contract');
  end;
  Result.FEdit.Platform := TNyxPlatform(LPlatform);
  LInput := LEdit.Field('stateInput').AsInteger;

  if (LInput < Ord(Low(TNyxStudioStateInput))) or
    (LInput > Ord(High(TNyxStudioStateInput))) then
  begin
    raise ENyxModel.Create('Design processor state notation is outside its closed contract');
  end;
  Result.FEdit.StateInput := TNyxStudioStateInput(LInput);
  LBinding := LEdit.Field('binding');

  if Result.FEdit.Action in [sdaSetBinding, sdaInheritBinding] then
  begin

    if (LBinding.Kind <> ndObject) or (LBinding.Count <> 5) then
    begin
      raise ENyxModel.Create('Binding intent requires its exact typed descriptor');
    end;
    LTarget := LBinding.Field('target').AsInteger;
    LKind := LBinding.Field('kind').AsInteger;
    LFlow := LBinding.Field('direction').AsInteger;

    if (LTarget < Ord(Low(TNyxBindingProperty))) or (LTarget > Ord(High(TNyxBindingProperty))) or
      (LKind < Ord(Low(TNyxStateKind))) or (LKind > Ord(High(TNyxStateKind))) or
      (LFlow < Ord(Low(TNyxBindingDirection))) or (LFlow > Ord(High(TNyxBindingDirection))) then
    begin
      raise ENyxModel.Create('Binding intent contains an unknown enum choice');
    end;

    if LBinding.Field('cleared').AsBoolean then
    begin

      if (LBinding.Field('name').AsText <> '') or (LKind <> Ord(nskText)) or
        (LFlow <> Ord(bdFromState)) then
      begin
        raise ENyxModel.Create('Cleared binding intent cannot contain a state reference');
      end;
      Result.FEdit.Binding := TNyxBindingSpec.Clear(TNyxBindingProperty(LTarget));
    end
    else
    begin
      Result.FEdit.Binding := TNyxBindingSpec.Bound(TNyxBindingProperty(LTarget),
        LBinding.Field('name').AsText, TNyxStateKind(LKind), TNyxBindingDirection(LFlow));
    end;

    if (Result.FEdit.Action = sdaInheritBinding) and not Result.FEdit.Binding.Cleared then
    begin
      raise ENyxModel.Create('Inherit intent requires a target without a state reference');
    end;
  end
  else if LBinding.Kind <> ndNull then
  begin
    raise ENyxModel.Create('Only binding intent contains a binding descriptor');
  end;
  Result.FEdit.FCanvasContext.FOwner := Result.FOwner;
  Result.FEdit.FCanvasContext.FGeneration := Result.FGeneration;
  LEvent := LEdit.Field('event');

  if Result.FEdit.Action = sdaEvent then
  begin

    if (LEvent.Kind <> ndObject) or (LEvent.Count <> 6) then
    begin
      raise ENyxModel.Create('Callback intent requires its exact typed descriptor');
    end;
    LEventAction := LEvent.Field('action').AsInteger;
    LTrigger := LEvent.Field('trigger').AsInteger;
    LPolicy := LEvent.Field('policy').AsInteger;

    if (LEventAction < Ord(Low(TNyxStudioEventAction))) or
      (LEventAction > Ord(High(TNyxStudioEventAction))) or
      (LTrigger < Ord(Low(TNyxTrigger))) or (LTrigger > Ord(High(TNyxTrigger))) or
      (LPolicy < Ord(Low(TNyxExecutionPolicy))) or (LPolicy > Ord(High(TNyxExecutionPolicy))) then
    begin
      raise ENyxModel.Create('Callback intent contains an unknown enum choice');
    end;
    Result.FEdit.Event.Action := TNyxStudioEventAction(LEventAction);
    Result.FEdit.Event.Trigger := TNyxTrigger(LTrigger);
    Result.FEdit.Event.Policy := TNyxExecutionPolicy(LPolicy);

    if LEvent.Field('name').AsText <> '' then
    begin
      Result.FEdit.Event.Name := NyxEvent(LEvent.Field('name').AsText);
    end;

    if LEvent.Field('id').AsText <> '' then
    begin
      Result.FEdit.Event.ID := NyxCallbackID(LEvent.Field('id').AsText);
    end;

    if LEvent.Field('handler').AsText <> '' then
    begin
      Result.FEdit.Event.Handler := NyxHandler(LEvent.Field('handler').AsText);
    end;
    ValidateEventIntent(Result.FEdit.Event);
  end
  else if LEvent.Kind <> ndNull then
  begin
    raise ENyxModel.Create('Only callback intent contains an event descriptor');
  end;

  if not (Result.FEdit.Action in [sdaSetStateDefault, sdaCreateStateDefault,
    sdaRenameStateDefault, sdaRemoveStateDefault]) and (Result.FEdit.StateInput <> ssiText) then
  begin
    raise ENyxModel.Create('Only state intent selects scalar editor notation');
  end;

  if ((Result.FEdit.Action = sdaCanvasValue) and (Result.FEdit.Platform = npfAny)) or
    ((Result.FEdit.Action <> sdaCanvasValue) and (Result.FEdit.Platform <> npfAny)) then
  begin
    raise ENyxModel.Create('Only canvas intent selects a concrete replay platform');
  end;

  if Result.FEdit.Action = sdaCollection then
  begin
    Result.FEdit.Collection := TNyxStudioCollectionIntent.FromData(LEdit.Field('collection'));

    if NyxStudioCollectionViewAction(Result.FEdit.Collection.Action) and
      ((Result.FEdit.Selection = '') or (Result.FEdit.View = '')) then
    begin
      raise ENyxCollection.Create('Collection view intent requires its captured owner/view');
    end;

    if not NyxStudioCollectionViewAction(Result.FEdit.Collection.Action) and
      ((Result.FEdit.Selection <> '') or (Result.FEdit.View <> '')) then
    begin
      raise ENyxCollection.Create('Document collection data cannot borrow selected-view ownership');
    end;
  end
  else if LEdit.Field('collection').Kind <> ndNull then
  begin
    raise ENyxCollection.Create('Only collection intent carries collection payload');
  end;

  if Result.FEdit.Action = sdaPlacement then
  begin
    Result.FEdit.Placement := TNyxPlacementChange.FromData(LEdit.Field('placement'));
  end
  else if (LVersion >= 6) and (LEdit.Field('placement').Kind <> ndNull) then
  begin
    raise ENyxModel.Create('Only placement intent carries a placement command');
  end;

  if Result.FEdit.Action = sdaResize then
  begin
    Result.FEdit.Resize := TNyxResizeChange.FromData(LEdit.Field('resize'));

    if Result.FEdit.Resize.Control.ID <> Result.FEdit.Selection then
    begin
      raise ENyxModel.Create('Resize payload and captured selected owner differ');
    end;
  end
  else if (LVersion >= 7) and (LEdit.Field('resize').Kind <> ndNull) then
  begin
    raise ENyxModel.Create('Only resize intent carries a resize command');
  end;

  if Result.FEdit.Action = sdaPresentation then
  begin
    Result.FEdit.Presentation := TNyxPresentationEdit.FromData(LEdit.Field('presentation'));

    if (Result.FEdit.Presentation.Control.ID <> '') and
      (Result.FEdit.Presentation.Control.ID <> Result.FEdit.Selection) then
    begin
      raise ENyxModel.Create('Presentation override and captured selected owner differ');
    end;
  end
  else if (LVersion >= 8) and (LEdit.Field('presentation').Kind <> ndNull) then
  begin
    raise ENyxModel.Create('Only presentation intent carries a presentation command');
  end;

  if Result.FEdit.Action = sdaPosition then
  begin
    Result.FEdit.Position := TNyxPositionChange.FromData(LEdit.Field('position'));

    if Result.FEdit.Position.Control.ID <> Result.FEdit.Selection then
    begin
      raise ENyxModel.Create('Position payload and captured selected owner differ');
    end;
  end
  else if (LVersion >= 9) and (LEdit.Field('position').Kind <> ndNull) then
  begin
    raise ENyxModel.Create('Only position intent carries a position command');
  end;

  if Result.FEdit.Action = sdaContent then
  begin
    LContent := LEdit.Field('content');

    if (LContent.Kind <> ndObject) or (LContent.Count <> 2) then
    begin
      raise ENyxModel.Create('Content intent requires its exact command and baseline');
    end;
    Result.FEdit.Content := TNyxContentEdit.FromData(LContent.Field('command'));
    Result.FEdit.ContentBaseline := LContent.Field('baseline').AsText;

    if (Result.FEdit.Content.Control.ID <> Result.FEdit.Selection) or
      (Result.FEdit.ContentBaseline = '') then
    begin
      raise ENyxModel.Create('Content payload requires its exact captured owner and baseline');
    end;
  end
  else if (LVersion = 10) and (LEdit.Field('content').Kind <> ndNull) then
  begin
    raise ENyxModel.Create('Only content intent carries a recipe command');
  end;

  if Result.FEdit.Action = sdaValueDomain then
  begin
    LContent := LEdit.Field('valueDomain');

    if (LContent.Kind <> ndObject) or (LContent.Count <> 2) then
    begin
      raise ENyxContract.Create('Value-domain intent requires exact command and baseline');
    end;
    Result.FEdit.ValueDomain := TNyxValueDomainEdit.FromData(LContent.Field('command'));
    Result.FEdit.ValueDomainBaseline := LContent.Field('baseline').AsText;

    if (Result.FEdit.ValueDomain.Control.ID <> Result.FEdit.Selection) or
      (Result.FEdit.ValueDomainBaseline = '') then
    begin
      raise ENyxContract.Create('Value-domain intent requires the exact captured owner/baseline');
    end;
  end;

  if Result.FEdit.Action = sdaMenu then
  begin
    LContent := LEdit.Field('menu');

    if (LContent.Kind <> ndObject) or (LContent.Count <> 2) then
    begin
      raise ENyxModel.Create('Menu intent requires its exact command and baseline');
    end;
    Result.FEdit.Menu := TNyxMenuEdit.FromData(LContent.Field('command'));
    Result.FEdit.MenuBaseline := LContent.Field('baseline').AsText;

    if (Result.FEdit.Selection = '') or (Result.FEdit.MenuBaseline = '') or
      ((Result.FEdit.Menu.Control.ID <> '') and
      (Result.FEdit.Menu.Control.ID <> Result.FEdit.Selection)) then
    begin
      raise ENyxModel.Create('Menu intent requires its captured owner and baseline');
    end;
  end;
end;

destructor TPreparedDesign.Destroy;
begin
  FWorkspace.Free;
  FDocument.Free;
  inherited Destroy;
end;

procedure TPreparedDesign.Execute;
var
  LSession: TNyxStudioSession;
  LStateValue: TNyxStateValue;
  LLine: Integer;
begin
  { Strict paired admission also verifies handwritten frame ownership. It runs
    entirely on the isolated processor, under its captured creator environment.
    The cost is deliberately kept off the UI rather than trusting a mutable
    accepted session or weakening file/source admission. }
  LSession := TNyxStudioSession.Create(FRequest.FPair);
  try
    LSession.FNextID := FRequest.FNextID;

    if FRequest.FEdit.View <> '' then
    begin
      LSession.Activate(FRequest.FEdit.View);
    end;

    if FRequest.FEdit.Selection <> '' then
    begin
      LSession.Select(FRequest.FEdit.Selection);
    end;

    if FRequest.FEdit.Action in [sdaSetStateDefault, sdaRenameStateDefault,
      sdaRemoveStateDefault] then
    begin
      LStateValue := LSession.Document.State.Value(FRequest.FEdit.Name);

      if LStateValue.Kind <> NyxStudioStateInputKind(FRequest.FEdit.StateInput) then
      begin
        raise ENyxState.Create('Captured state identity changed its scalar family');
      end;
    end;
    case FRequest.FEdit.Action of
      sdaProperty: LSession.SetProperty(FRequest.FEdit.Name, FRequest.FEdit.Value);
      sdaAddKind: LSession.AddKind(FRequest.FEdit.Name);
      sdaDelete: LSession.DeleteSelected;
      sdaDuplicate: LSession.DuplicateSelected;
      sdaMove:
        begin

          if FRequest.FEdit.Direction = nmdPrevious then
          begin
            LSession.MoveSelected(-1);
          end
          else
          begin
            LSession.MoveSelected(1);
          end;
        end;
      sdaTitle: LSession.SetTitle(FRequest.FEdit.Value);
      sdaAddPage: LSession.AddPage;
      sdaCreateComponent: LSession.CreateComponent;
      sdaAddInstance: LSession.AddComponentInstance(FRequest.FEdit.Name);
      sdaCustomizePart: LSession.CustomizePart(FRequest.FEdit.Name);
      sdaPlacement: LSession.Place(FRequest.FEdit.Placement);
      sdaResize: LSession.Resize(FRequest.FEdit.Resize);
      sdaPosition: LSession.Position(FRequest.FEdit.Position);
      sdaPresentation:
        LSession.ApplyPatch(ReadNyxDesignPatch(NyxArray([FRequest.FEdit.Presentation.ToData])));
      sdaContent:
        begin

          if LSession.Selected.Content.ToData.ToJSON <> FRequest.FEdit.ContentBaseline then
          begin
            raise ENyxModel.Create('Content choices changed since this editor was captured');
          end;
          LSession.ApplyPatch(ReadNyxDesignPatch(NyxArray([FRequest.FEdit.Content.ToData])));
        end;
      sdaValueDomain:
        begin

          if NyxStudioValueDomainBaseline(LSession.Document,
            FRequest.FEdit.ValueDomain.Control) <> FRequest.FEdit.ValueDomainBaseline then
          begin
            raise ENyxContract.Create('Value constraints changed since this editor was captured');
          end;
          LSession.ApplyPatch(NyxValueDomainPatch([FRequest.FEdit.ValueDomain]));
        end;
      sdaMenu:
        begin

          if FRequest.FEdit.Menu.IsMenuBar then
          begin

            if NyxMenuBarEditorBaseline(LSession.Document,
              NyxControl(FRequest.FEdit.Selection)) <> FRequest.FEdit.MenuBaseline then
            begin
              raise ENyxModel.Create('Menu bar choices changed since this editor was captured');
            end;
          end
          else
          begin

            if NyxMenuEditorBaseline(LSession.Document, NyxControl(FRequest.FEdit.Selection)) <>
              FRequest.FEdit.MenuBaseline then
            begin
              raise ENyxModel.Create('Menu choices changed since this editor was captured');
            end;
          end;
          LSession.ApplyPatch(NyxMenuPatch([FRequest.FEdit.Menu]));
        end;
      sdaCanvasValue: LSession.SetCapturedCanvasValue(FRequest.FEdit);
      sdaSetStateDefault:
        LSession.SetStateValues([NyxStateAssign(FRequest.FEdit.Name,
          ParseNyxStudioStateInput(FRequest.FEdit.StateInput, FRequest.FEdit.Value))]);
      sdaCreateStateDefault:
        LSession.CreateState(FRequest.FEdit.Name,
          ParseNyxStudioStateInput(FRequest.FEdit.StateInput, FRequest.FEdit.Value));
      sdaRenameStateDefault:
        LSession.RenameState(FRequest.FEdit.Name, FRequest.FEdit.Value);
      sdaRemoveStateDefault: LSession.RemoveState(FRequest.FEdit.Name);
      sdaSetBinding: LSession.SetBinding(FRequest.FEdit.Binding);
      sdaInheritBinding: LSession.InheritBinding(FRequest.FEdit.Binding.Target);
      sdaEvent: LSession.ApplyEventIntent(FRequest.FEdit.Event, FAddedHandler, LLine);
      sdaCollection:
        begin
          LSession.ApplyCollectionIntent(FRequest.FEdit.Collection);
        end;
    end;
    FSource := LSession.Source;
    FDesign := LSession.FSourceWorkspace.Capture.Design;
    FSnapshot := LSession.FSourceWorkspace.Snapshot;
    FSelection := LSession.SelectedID;
    FView := LSession.ActiveViewID;
    FNextID := LSession.FNextID;
    FDocument := LSession.FDocument;
    LSession.FDocument := nil;
    FWorkspace := LSession.FSourceWorkspace;
    LSession.FSourceWorkspace := nil;
  finally
    LSession.Free;
  end;
end;

function TPreparedDesign.Matches(const ARequest: TNyxStudioDesignRequest): Boolean;
begin
  Result := FRequest.SameRequest(ARequest);
end;

function TPreparedDesign.GetSource: TNyxText;
begin
  Result := FSource;
end;

function TPreparedDesign.GetDesign: TNyxText;
begin
  Result := FDesign;
end;

function TPreparedDesign.GetDiagnostic: TNyxSourceDiagnostic;
begin
  Result := FDiagnostic;
end;

function TPreparedDesign.GetSchemaRevision: Integer;
begin
  Result := FRequest.SchemaRevision;
end;

function TPreparedDesign.GetSelection: TNyxText;
begin
  Result := FSelection;
end;

function TPreparedDesign.GetView: TNyxText;
begin
  Result := FView;
end;

function TPreparedDesign.GetNextID: Integer;
begin
  Result := FNextID;
end;

function TPreparedDesign.GetAddedHandler: TNyxHandlerRef;
begin
  Result := FAddedHandler;
end;

function TPreparedDesign.ToData: TNyxDataValue;
begin
  Result := NyxObject([
    NyxField('version', NyxData(2)), NyxField('request', FRequest.ToData),
    NyxField('selection', NyxData(FSelection)), NyxField('view', NyxData(FView)),
    NyxField('nextId', NyxData(FNextID)),
    NyxField('addedHandler', NyxData(FAddedHandler.Name)),
    NyxField('pair', NyxObject([
      NyxField('version', NyxData(1)), NyxField('source', NyxData(FSource)),
      NyxField('design', NyxData(FDesign)), NyxField('workspace', NyxData(FSnapshot)),
      NyxField('schemaRevision', NyxData(GetSchemaRevision)),
      NyxField('diagnostic', NyxObject([
        NyxField('defined', NyxData(FDiagnostic.Defined)),
        NyxField('message', NyxData(FDiagnostic.Message)),
        NyxField('line', NyxData(FDiagnostic.Line)),
        NyxField('column', NyxData(FDiagnostic.Column))]))]))]);
end;

procedure TPreparedDesign.Take(var ADocument: TNyxDocument;
  var AWorkspace: TNyxSourceWorkspace);
begin

  if (ADocument <> nil) or (AWorkspace <> nil) or FDiagnostic.Defined or
    (FDocument = nil) or (FWorkspace = nil) then
  begin
    raise ENyxModel.Create('Design transfer requires empty destinations and one admitted pair');
  end;
  ADocument := FDocument;
  AWorkspace := FWorkspace;
  FDocument := nil;
  FWorkspace := nil;
end;

function PrepareNyxStudioDesign(const ARequest: TNyxStudioDesignRequest;
  const ASchemas: INyxSchemaSnapshot): INyxPreparedDesign;
var
  LOwner: TPreparedDesign;
  LAction: INyxSchemaAction;
begin

  if (ASchemas = nil) or (ASchemas.Revision <> ARequest.SchemaRevision) then
  begin
    raise ENyxModel.Create('Design preparation requires its captured creator environment');
  end;
  LOwner := TPreparedDesign.Create;
  Result := LOwner;
  LOwner.FRequest := ARequest;
  LAction := LOwner;
  try
    ASchemas.Execute(LAction);
  except
    on LException: Exception do
    begin
      LOwner.FDiagnostic.Defined := True;
      LOwner.FDiagnostic.Message := LException.Message;

      if LException is ENyxSource then
      begin
        LOwner.FDiagnostic.Message := ENyxSource(LException).DiagnosticText;
        LOwner.FDiagnostic.Line := ENyxSource(LException).Line;
        LOwner.FDiagnostic.Column := ENyxSource(LException).Column;
      end;
      FreeAndNil(LOwner.FDocument);
      FreeAndNil(LOwner.FWorkspace);
      LOwner.FSource := '';
      LOwner.FDesign := '';
      LOwner.FSnapshot := '';
      LOwner.FAddedHandler := Default(TNyxHandlerRef);
    end;
  end;
end;

function ReceiveNyxPreparedDesign(const AData: TNyxDataValue;
  const ARequest: TNyxStudioDesignRequest;
  const ASchemas: INyxSchemaSnapshot): INyxPreparedDesign;
var
  LOwner: TPreparedDesign;
  LPair: TNyxDataValue;
  LPrepared: INyxPreparedSource;
  LEvent: TNyxStudioEventIntent;
begin

  if (AData.Kind <> ndObject) or (AData.Count <> 7) or
    (AData.Field('version').AsInteger <> 2) or
    not ReadNyxStudioDesignRequest(AData.Field('request')).SameRequest(ARequest) then
  begin
    raise ENyxModel.Create('Design worker reply differs from its dispatched ticket');
  end;
  LPair := AData.Field('pair');
  LPrepared := ReceiveNyxPreparedSource(LPair, LPair.Field('source').AsText, ASchemas);
  LOwner := TPreparedDesign.Create;
  Result := LOwner;
  LOwner.FRequest := ARequest;
  LOwner.FSource := LPrepared.Source;
  LOwner.FDesign := LPrepared.Design;
  LOwner.FDiagnostic := LPrepared.Diagnostic;
  LOwner.FSnapshot := LPair.Field('workspace').AsText;
  LOwner.FSelection := AData.Field('selection').AsText;
  LOwner.FView := AData.Field('view').AsText;
  LOwner.FNextID := AData.Field('nextId').AsInteger;

  if AData.Field('addedHandler').AsText <> '' then
  begin
    LOwner.FAddedHandler := NyxHandler(AData.Field('addedHandler').AsText);
  end;

  if ((ARequest.FEdit.Action <> sdaEvent) or
    (ARequest.FEdit.Event.Action <> seaAdd) or LPrepared.Diagnostic.Defined) and
    (LOwner.FAddedHandler.Name <> '') then
  begin
    raise ENyxModel.Create('Only an admitted callback addition returns a handler');
  end;

  if not LPrepared.Diagnostic.Defined then
  begin
    LPrepared.Take(LOwner.FDocument, LOwner.FWorkspace);

    if (ARequest.FEdit.Action = sdaEvent) and (ARequest.FEdit.Event.Action = seaAdd) then
    begin
      LEvent := ARequest.FEdit.Event;
      LEvent.Handler := LOwner.FAddedHandler;
      LEvent.ID := Default(TNyxCallbackRef);

      if LEvent.Handler.Name <> '' then
      begin
        LEvent.ID := NyxCallbackID(LEvent.Handler.Name);
      end;
      { A receipt cannot name an older helper or a callback on another owner.
        Paired source admission already qualifies the implementation boundary. }

      if (LEvent.Handler.Name = '') or
        (Pos(LowerCase(LEvent.Handler.Name), LowerCase(ARequest.FPair.Source)) > 0) then
      begin
        raise ENyxModel.Create('Callback addition receipt cannot reuse an existing helper');
      end;

      if not HasEventRegistration(LOwner.FDocument.Find(ARequest.FEdit.Selection), LEvent) then
      begin
        raise ENyxModel.Create('Callback addition receipt differs from its admitted registration');
      end;
    end;

    if (LOwner.FNextID < ARequest.FNextID) or
      ((LOwner.FSelection <> '') and (LOwner.FDocument.Find(LOwner.FSelection) = nil)) or
      ((LOwner.FView <> '') and (LOwner.FDocument.Find(LOwner.FView) = nil)) then
    begin
      raise ENyxModel.Create('Design worker reply has invalid presentation identities');
    end;
  end;
end;

function TNyxStudioSession.CommandContext: TNyxStudioCommandContext;
begin
  Result := Default(TNyxStudioCommandContext);
  Result.FOwner := FSourceIdentity;
  Result.FGeneration := FSourceGeneration;
end;

function TNyxStudioSession.MatchesCommandContext(
  const AContext: TNyxStudioCommandContext): Boolean;
begin
  Result := (AContext.FOwner = FSourceIdentity) and
    (AContext.FGeneration = FSourceGeneration);
end;

function TNyxStudioSession.PrepareDesignRequest(const AEdit: TNyxStudioDesignEdit;
  ASchemaRevision: Integer): TNyxStudioDesignRequest;
begin

  if (AEdit.Action in [sdaCanvasValue, sdaPlacement, sdaResize, sdaPosition]) and
    not MatchesCommandContext(AEdit.CanvasContext) then
  begin
    raise ENyxModel.Create('Captured canvas intent belongs to an earlier session or project load');
  end;

  if AEdit.Action = sdaPosition then
  begin
    AEdit.Position.ToData;

    if AEdit.Position.Control.ID <> AEdit.Selection then
    begin
      raise ENyxModel.Create('Position payload and captured selected owner differ');
    end;
  end;

  if AEdit.Action = sdaResize then
  begin
    AEdit.Resize.ToData;

    if AEdit.Resize.Control.ID <> AEdit.Selection then
    begin
      raise ENyxModel.Create('Resize payload and captured selected owner differ');
    end;
  end;

  if AEdit.Action = sdaPresentation then
  begin

    if not AEdit.Presentation.Defined or ((AEdit.Presentation.Control.ID <> '') and
      (AEdit.Presentation.Control.ID <> AEdit.Selection)) then
    begin
      raise ENyxModel.Create('Presentation intent requires its exact captured selected owner');
    end;
  end;

  if AEdit.Action = sdaValueDomain then
  begin

    if not AEdit.ValueDomain.Defined or
      (AEdit.ValueDomain.Control.ID <> AEdit.Selection) or
      (FDocument.Find(AEdit.Selection) = nil) or (AEdit.ValueDomainBaseline = '') then
    begin
      raise ENyxContract.Create('Value-domain intent requires its captured owner/baseline');
    end;
  end;

  if AEdit.Action = sdaContent then
  begin

    if not AEdit.Content.Defined or (AEdit.Content.Control.ID <> AEdit.Selection) or
      (FDocument.Find(AEdit.Selection) = nil) or
      (AEdit.ContentBaseline = '') then
    begin
      raise ENyxModel.Create('Content intent requires its exact captured owner and baseline');
    end;
    { Earlier queued jobs may legitimately have changed this registry. The
      independent processor compares the mounted baseline against its fresh
      pair and returns a normal rejected diagnostic; staleness is not a failed
      scheduler/worker transport and must not be reported as one. }
  end;

  if AEdit.Action = sdaMenu then
  begin

    if not AEdit.Menu.Defined or (AEdit.MenuBaseline = '') or
      (FDocument.Find(AEdit.Selection) = nil) or
      ((AEdit.Menu.Control.ID <> '') and (AEdit.Menu.Control.ID <> AEdit.Selection)) then
    begin
      raise ENyxModel.Create('Menu intent requires its captured owner and baseline');
    end;
  end;

  if AEdit.Action = sdaCollection then
  begin
    AEdit.Collection.Validate;

    if NyxStudioCollectionViewAction(AEdit.Collection.Action) <>
      ((AEdit.Selection <> '') and (AEdit.View <> '')) then
    begin
      raise ENyxCollection.Create('Collection view/data ownership does not match its captured intent');
    end;

    if not NyxStudioCollectionViewAction(AEdit.Collection.Action) and
      ((AEdit.Selection <> '') or (AEdit.View <> '')) then
    begin
      raise ENyxCollection.Create('Document collection data cannot borrow selected-view ownership');
    end;
  end;
  Result := Default(TNyxStudioDesignRequest);
  Result.FOwner := FSourceIdentity;
  Result.FGeneration := FSourceGeneration;
  Result.FNextID := FNextID;
  Result.FSchemaRevision := ASchemaRevision;
  Result.FPair := ProjectSnapshot;
  Result.FEdit := AEdit;
  Result.FEdit.Binding := AEdit.Binding.Copy;
end;

function TNyxStudioSession.CompleteDesignRequest(const ARequest: TNyxStudioDesignRequest;
  const APrepared: INyxPreparedDesign): TNyxSourceCompletion;
var
  LCurrent: TNyxProjectPair;
  LCheckpoint: TNyxSourceCheckpoint;
  LCandidate: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LPublication: TSourcePairPublication;
  LAction: INyxSchemaAction;
  LFollowSelection: Boolean;
  LFollowView: Boolean;
  {$ifdef NYX_STUDIO_PROFILE}
  LPhaseStarted: QWord;
  {$endif}
begin
  Result := nscStale;

  if (ARequest.FOwner <> FSourceIdentity) or
    (ARequest.FGeneration <> FSourceGeneration) or
    (ARequest.SchemaRevision <> NyxSchemaRevision) then
  begin
    Exit;
  end;
  {$ifdef NYX_STUDIO_PROFILE}LPhaseStarted := GetTickCount64;{$endif}
  LCurrent := ProjectSnapshot;
  {$ifdef NYX_STUDIO_PROFILE}
  WriteLn('studio-ui,command-current-snapshot,', GetTickCount64 - LPhaseStarted);
  LPhaseStarted := GetTickCount64;
  {$endif}
  LCheckpoint := FSourceWorkspace.Capture;
  {$ifdef NYX_STUDIO_PROFILE}
  WriteLn('studio-ui,command-capture,', GetTickCount64 - LPhaseStarted);
  {$endif}

  if (LCurrent.Design <> ARequest.FPair.Design) or
    (LCurrent.Source <> ARequest.FPair.Source) or
    (LCurrent.Pending <> ARequest.FPair.Pending) or
    (LCurrent.Draft <> ARequest.FPair.Draft) or
    (LCurrent.DraftBase <> ARequest.FPair.DraftBase) then
  begin
    Exit;
  end;

  if (APrepared = nil) or not APrepared.Matches(ARequest) or
    (APrepared.SchemaRevision <> ARequest.SchemaRevision) then
  begin
    raise ENyxModel.Create('Design completion does not match its captured command');
  end;

  if APrepared.Diagnostic.Defined then
  begin
    Exit(nscRejected);
  end;

  if (APrepared.Design = LCurrent.Design) and (APrepared.Source = LCurrent.Source) then
  begin
    Exit(nscUnchanged);
  end;
  LCandidate := nil;
  LWorkspace := nil;
  LFollowSelection := FSelectedID = ARequest.FEdit.Selection;
  LFollowView := FActiveViewID = ARequest.FEdit.View;
  try
    APrepared.Take(LCandidate, LWorkspace);
    LPublication := TSourcePairPublication.Create;
    LAction := LPublication;
    LPublication.Session := Self;
    LPublication.Checkpoint := LCheckpoint;
    LPublication.Document := LCandidate;
    LPublication.Workspace := LWorkspace;
    LCandidate := nil;
    LWorkspace := nil;

    {$ifdef NYX_STUDIO_PROFILE}LPhaseStarted := GetTickCount64;{$endif}
    if not CommitNyxSchemaRevision(ARequest.SchemaRevision, LAction) then
    begin
      Exit;
    end;
    {$ifdef NYX_STUDIO_PROFILE}
    WriteLn('studio-ui,command-publish,', GetTickCount64 - LPhaseStarted);
    {$endif}

    if APrepared.NextID > FNextID then
    begin
      FNextID := APrepared.NextID;
    end;
    { Preserve a pending handwritten draft with its ORIGINAL base. The new
      design/source is admitted together; a divergent draft stays visibly stale
      instead of being discarded or silently rebased onto generated text. }

    if LFollowView then
    begin
      FActiveViewID := APrepared.View;
    end;

    if LFollowSelection then
    begin
      FSelectedID := APrepared.Selection;
    end;

    if FDocument.Find(FSelectedID) = nil then
    begin
      FSelectedID := FActiveViewID;
    end;
    Result := nscApplied;
  finally
    LCandidate.Free;
    LWorkspace.Free;
  end;
end;
