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
begin
  Result := NyxObject([
    NyxField('version', NyxData(4)), NyxField('owner', NyxData(FOwner)),
    NyxField('generation', NyxData(FGeneration)), NyxField('nextId', NyxData(FNextID)),
    NyxField('schemaRevision', NyxData(FSchemaRevision)),
    NyxField('pair', NyxData(EncodeNyxProject(FPair))),
    NyxField('edit', NyxObject([
      NyxField('action', NyxData(Ord(FEdit.Action))),
      NyxField('selection', NyxData(FEdit.Selection)), NyxField('view', NyxData(FEdit.View)),
      NyxField('name', NyxData(FEdit.Name)), NyxField('value', NyxData(FEdit.Value)),
      NyxField('direction', NyxData(Ord(FEdit.Direction))),
      NyxField('platform', NyxData(Ord(FEdit.Platform))),
      NyxField('stateInput', NyxData(Ord(FEdit.StateInput))),
      NyxField('binding', DesignBindingData(FEdit)),
      NyxField('event', DesignEventData(FEdit))]))]);
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
begin

  if (AData.Kind <> ndObject) or (AData.Count <> 7) or
    (AData.Field('version').AsInteger <> 4) then
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
    (Result.FSchemaRevision < 0) or (LEdit.Kind <> ndObject) or (LEdit.Count <> 10) then
  begin
    raise ENyxModel.Create('Design processor ticket has invalid identity or intent');
  end;
  LAction := LEdit.Field('action').AsInteger;

  if (LAction < Ord(Low(TNyxStudioDesignAction))) or
    (LAction > Ord(High(TNyxStudioDesignAction))) then
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

  if (AEdit.Action = sdaCanvasValue) and not MatchesCommandContext(AEdit.CanvasContext) then
  begin
    raise ENyxModel.Create('Captured canvas intent belongs to an earlier session or project load');
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
