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

unit nyx.flow.controls.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.data, nyx.model, nyx.controls, nyx.types,
  nyx.flow.routes;

type
  { Specialized managed compound form. Named parts remain ordinary Nyx controls,
    retained independently when read. Missing/retyped parts raise; no part keeps
    a document, runtime or reverse reference to its owning form. Creators may
    customize styling/layout through these typed parts without widget handles. }
  INyxControlRouteEditor = interface(INyxCard)
    ['{8F33AB17-671D-43DA-B4AE-9C732512A7B4}']
    function GetEventSelect: INyxSelect;
    function GetGraphSelect: INyxSelect;
    function GetNameInput: INyxInput;
    function GetInputSelect: INyxSelect;
    function GetOutputSelect: INyxSelect;
    function GetDestinationSelect: INyxSelect;
    function GetApplyButton: INyxButton;
    property EventSelect: INyxSelect read GetEventSelect;
    property GraphSelect: INyxSelect read GetGraphSelect;
    property NameInput: INyxInput read GetNameInput;
    property InputSelect: INyxSelect read GetInputSelect;
    property OutputSelect: INyxSelect read GetOutputSelect;
    property DestinationSelect: INyxSelect read GetDestinationSelect;
    property ApplyButton: INyxButton read GetApplyButton;
  end;

  { Detached complete route-set proposal for one authored control. The owner
    publishes it through ordinary paired history, never by mutating a live view.
    Revision and scoped baseline reject stale forms. No tree, store, callback,
    program implementation or host capability is retained in this value. }
  TNyxControlRouteEditorChange = record
    Owner: TNyxControlRef;
    Revision: Integer;
    RoutesData: TNyxDataValue;
  end;

  { Independent unfinished route fields and staged mappings. Invalid scalar/path
    text remains editable. Restoration admits the complete matching context and
    copied field payload before writing any form value. Explicitly saved routes
    and pending Pascal remain owned by the application's ordinary command path. }
  TNyxControlRouteEditorDraft = record
  private
    FData: TNyxDataValue;
  public
    procedure Clear;
    function Defined: Boolean;
    procedure Capture(const AID: TNyxText; ARoot: TNyxNode);
    function CanRestore(const AID: TNyxText; ARoot: TNyxNode): Boolean;
    function Restore(const AID: TNyxText; ARoot: TNyxNode): Boolean;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxControlRouteEditorDraft; static;
  end;

{ Public Nyx event/graph/input/output form for one authored control. The model is
  borrowed only during each call. Choices own inert schemas, never executable
  resolvers. Output choices name actual direct state bindings or explicit state;
  computed/resource projections do not silently become writable control inputs. }
function NewNyxControlRouteEditor(const AID: TNyxText; ADocument: TNyxDocument;
  const AOwner: TNyxControlRef; ARevision: Integer): INyxControlRouteEditor;
{ Local loading/mapping/path choices only. True requests ordinary renderer Sync.
  Refusal raises before changing staged data. Apply/removal return False here:
  Capture below supplies their single undoable document proposal. }
function HandleNyxControlRouteEditor(ANode, ARoot: TNyxNode;
  ADocument: TNyxDocument; ARevision: Integer): Boolean;
{ Complete graph/event/state/collection admission precedes a detached proposal.
  Existing routes and row mappings survive. Removal and replacing an unrelated
  same-name route require acknowledgement. No application code is executed. }
function CaptureNyxControlRouteEditor(ANode, ARoot: TNyxNode;
  ADocument: TNyxDocument; ARevision: Integer;
  out AChange: TNyxControlRouteEditorChange): Boolean;

implementation

uses nyx.schema, nyx.contract, nyx.event.payload, nyx.binding.types, nyx.state,
  nyx.flow.types, nyx.flow.declarations, nyx.flow.route.model, nyx.resources,
  nyx.bytes, nyx.scheduler, nyx.root.types, nyx.logic.catalog, nyx.disclosure;

const
  CEditor = 'nyx.control-route.editor';
  CWorking = 'nyx.control-route.working';
  CPorts = 'nyx.control-route.ports';
  CPath = 'nyx.control-route.path';
  CInitial = 'nyx.control-route.initial';
  CSections: array[0..2] of TNyxText = ('execution', 'input-section', 'output-section');
  CFields: array[0..16] of TNyxText = ('route', 'name', 'graph', 'event',
    'callbacks', 'policy', 'concurrency', 'limit', 'enabled', 'input', 'source',
    'state', 'constant', 'constant-boolean', 'output', 'destination', 'confirm');
  CCallbacks: array[TNyxFlowCallbackMode] of TNyxText =
    ('Exclusive (refuse ordinary callbacks)', 'Before ordinary callbacks', 'After ordinary callbacks');
  CPolicies: array[TNyxExecutionPolicy] of TNyxText =
    ('Sequential', 'Asynchronous', 'UI queue', 'Threaded');
  CConcurrency: array[TNyxFlowConcurrency] of TNyxText =
    ('Latest wins', 'Every invocation', 'Ignore while running');
  CSources: array[TNyxFlowInputSource] of TNyxText =
    ('Constant', 'Existing state/control binding', 'Event value', 'Event details', 'Invocation signal');

type
  { Default portable form implementation. The inherited managed card owns its
    independently allocated children; typed getters retain exact named parts.
    Model/schema construction stays in the factory and no renderer is retained. }
  TControlRouteCard = class(TNyxCard, INyxControlRouteEditor)
  private
    function RetainedPart(const AName: TNyxText): INyxNode;
  public
    function GetEventSelect: INyxSelect;
    function GetGraphSelect: INyxSelect;
    function GetNameInput: INyxInput;
    function GetInputSelect: INyxSelect;
    function GetOutputSelect: INyxSelect;
    function GetDestinationSelect: INyxSelect;
    function GetApplyButton: INyxButton;
  end;

  { Transient metadata accumulator, initialized with Default before use. Doubling
    capacity avoids copying every prior choice on each append. Finish returns
    an independent immutable vector; no document or runtime owner is retained. }
  TRouteChoices = record
    Values: array of TNyxDataValue;
    Count: Integer;
    procedure Add(const AValue: TNyxDataValue; const ATitle: TNyxText);
    function Finish: TNyxDataValue;
  end;

function TControlRouteCard.RetainedPart(const AName: TNyxText): INyxNode;
var
  LNode: TNyxNode;
begin
  LNode := Node.Find(Node.ID + '-' + AName);

  if LNode = nil then
  begin
    raise ENyxFlow.Create('Missing control route part: ' + AName);
  end;
  Result := RetainNyxControl(LNode);
end;

function TControlRouteCard.GetEventSelect: INyxSelect;
begin
  Result := RetainedPart('event') as INyxSelect;
end;

function TControlRouteCard.GetGraphSelect: INyxSelect;
begin
  Result := RetainedPart('graph') as INyxSelect;
end;

function TControlRouteCard.GetNameInput: INyxInput;
begin
  Result := RetainedPart('name') as INyxInput;
end;

function TControlRouteCard.GetInputSelect: INyxSelect;
begin
  Result := RetainedPart('input') as INyxSelect;
end;

function TControlRouteCard.GetOutputSelect: INyxSelect;
begin
  Result := RetainedPart('output') as INyxSelect;
end;

function TControlRouteCard.GetDestinationSelect: INyxSelect;
begin
  Result := RetainedPart('destination') as INyxSelect;
end;

function TControlRouteCard.GetApplyButton: INyxButton;
begin
  Result := RetainedPart('apply') as INyxButton;
end;

function Field(AForm: TNyxNode; const AName: TNyxText): TNyxNode;
begin
  Result := AForm.Find(AForm.ID + '-' + AName);

  if Result = nil then
  begin
    raise ENyxFlow.Create('The control route form is incomplete');
  end;
end;

function Value(AForm: TNyxNode; const AName: TNyxText): TNyxText;
begin
  Result := Field(AForm, AName).Prop(NyxAttributeName(atValue));
end;

function Flag(AForm: TNyxNode; const AName: TNyxText): Boolean;
var
  LValue: TNyxText;
begin
  LValue := Value(AForm, AName);

  if (LValue <> 'true') and (LValue <> 'false') then
  begin
    raise ENyxFlow.Create('Route switches require Boolean values');
  end;
  Result := LValue = 'true';
end;

{ Only inert form/persistence dictionaries use field strings. Public changes
  carry typed routes, endpoints, state references and closed policy enums. }
function Put(const AData: TNyxDataValue; const AName: TNyxText;
  const AValue: TNyxDataValue): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LCount: Integer;
  LIndex: Integer;
  LFound: Boolean;
begin
  { Materialize Count before indexing. pas2js can otherwise emit a record's
    no-argument method itself as the subscript instead of invoking that method. }
  LCount := AData.Count;
  SetLength(LFields, LCount + 1);
  LFound := False;
  for LIndex := 0 to LCount - 1 do
  begin
    LFields[LIndex] := NyxField(AData.Key(LIndex), AData.Field(AData.Key(LIndex)));

    if AData.Key(LIndex) = AName then
    begin
      LFields[LIndex] := NyxField(AName, AValue);
      LFound := True;
    end;
  end;

  if LFound then
  begin
    SetLength(LFields, LCount);
  end
  else
  begin
    LFields[LCount] := NyxField(AName, AValue);
  end;
  Result := NyxObject(LFields);
end;

function Caption(const AText: TNyxText): TNyxText;
var
  LOffset: Integer;
  LScalar: Integer;
  LCount: Integer;
begin
  Result := '';
  LOffset := 1;
  LCount := 0;
  while (LOffset <= Length(AText)) and (LCount < 80) do
  begin

    if not NyxNextScalar(AText, LOffset, LScalar) then
    begin
      raise ENyxFlow.Create('Route captions require valid Unicode');
    end;

    if (LScalar < 32) or ((LScalar >= 127) and (LScalar <= 159)) then
    begin
      Result := Result + ' ';
    end
    else
    begin
      Result := Result + NyxScalarText(LScalar);
    end;
    Inc(LCount);
  end;
end;

procedure TRouteChoices.Add(const AValue: TNyxDataValue; const ATitle: TNyxText);
var
  LCapacity: Integer;
begin

  if Count = High(Integer) then
  begin
    raise ENyxFlow.Create('Route choice capacity exhausted');
  end;

  if Count = Length(Values) then
  begin
    LCapacity := 16;

    if Length(Values) > High(Integer) div 2 then
    begin
      LCapacity := High(Integer);
    end
    else if Length(Values) > 0 then
    begin
      LCapacity := Length(Values) * 2;
    end;
    SetLength(Values, LCapacity);
  end;
  Values[Count] := Put(AValue, 'label',
    NyxData(TNyxText(IntToStr(Count + 1)) + '. ' + Caption(ATitle)));
  Inc(Count);
end;

function TRouteChoices.Finish: TNyxDataValue;
var
  LItems: array of TNyxDataValue;
  LIndex: Integer;
begin
  SetLength(LItems, Count);
  for LIndex := 0 to Count - 1 do
  begin
    LItems[LIndex] := Values[LIndex];
  end;
  Result := NyxArray(LItems);
end;

function Items(const AValues: TNyxDataValue): TNyxText;
var
  LIndex: Integer;
begin
  Result := '';
  for LIndex := 0 to AValues.Count - 1 do
  begin

    if LIndex > 0 then
    begin
      Result := Result + #10;
    end;
    Result := Result + AValues.Item(LIndex).Field('label').AsText;
  end;
end;

function First(const AValues: TNyxDataValue): TNyxText;
begin
  Result := '';

  if AValues.Count > 0 then
  begin
    Result := AValues.Item(0).Field('label').AsText;
  end;
end;

function Choice(const AValues: TNyxDataValue; const ALabel: TNyxText): TNyxDataValue;
var
  LIndex: Integer;
begin
  for LIndex := 0 to AValues.Count - 1 do
  begin

    if AValues.Item(LIndex).Field('label').AsText = ALabel then
    begin
      Exit(AValues.Item(LIndex));
    end;
  end;
  raise ENyxFlow.Create('Choose an exact available route value');
end;

function EnumIndex(const AValue: TNyxText; const AChoices: array of TNyxText): Integer;
begin
  for Result := 0 to High(AChoices) do
  begin

    if AChoices[Result] = AValue then
    begin
      Exit;
    end;
  end;
  raise ENyxFlow.Create('Choose a typed route policy');
end;

function EnumItems(const AChoices: array of TNyxText): TNyxText;
var
  LIndex: Integer;
begin
  Result := '';
  for LIndex := 0 to High(AChoices) do
  begin

    if LIndex > 0 then
    begin
      Result := Result + #10;
    end;
    Result := Result + AChoices[LIndex];
  end;
end;

function Form(ANode, ARoot: TNyxNode): TNyxNode;
begin
  Result := ANode;
  while Result <> nil do
  begin

    if Result.Extensions.Has(NyxExtension(CEditor)) then
    begin

      if (ARoot = nil) or (ARoot.Find(Result.ID) <> Result) then
      begin
        Exit(nil);
      end;
      Exit;
    end;
    Result := Result.Parent;
  end;
end;

function Snapshot(ADocument: TNyxDocument; const AOwner: TNyxControlRef): TNyxDataValue;
var
  LControl: TNyxNode;
  LEvents: TNyxEventSchemas;
  LEventData: TRouteChoices;
  LGraphs: TRouteChoices;
  LStates: TRouteChoices;
  LBindingChoices: TRouteChoices;
  LDestinations: TRouteChoices;
  LBindings: TNyxDataValue;
  LStateData: TNyxDataValue;
  LPorts: TRouteChoices;
  LDefinition: INyxFlowDefinition;
  LDescriptor: TNyxLogicDescriptor;
  LPort: TNyxFlowPort;
  LStep: Integer;
  LPart: Integer;
  LDomain: TNyxValueDomain;
  LValueNode: TNyxNode;
  LFamily: Integer;
  LDetails: Boolean;
  LIndex: Integer;
  LReference: TNyxFlowRef;
  LName: TNyxText;
  LTitle: TNyxText;
  LStateKind: TNyxStateKind;

  procedure Bindings(ANode: TNyxNode);
  var
    LBinding: TNyxBindingSpec;
    LPart: Integer;
  begin
    for LPart := 0 to ANode.BindingCount - 1 do
    begin
      LBinding := ANode.Bindings[LPart];

      if LBinding.Cleared or (LBinding.Source <> bsState) then
      begin
        Continue;
      end;
      LBindingChoices.Add(NyxObject([NyxField('control', NyxData(ANode.ID)),
        NyxField('property', NyxData(Ord(LBinding.Target))),
        NyxField('state', NyxData(LBinding.StateName)),
        NyxField('kind', NyxData(Ord(LBinding.ValueKind)))]),
        ANode.ID + ' / ' + NyxBindingPropertyTitle(LBinding.Target));
    end;
    for LPart := 0 to ANode.Count - 1 do
    begin
      Bindings(ANode.Children[LPart]);
    end;
  end;

begin

  if ADocument = nil then
  begin
    raise ENyxFlow.Create('Control route authoring requires a document');
  end;
  LControl := ADocument.Find(AOwner.ID);

  if LControl = nil then
  begin
    raise ENyxFlow.Create('Choose an existing authored event source');
  end;
  LEventData := Default(TRouteChoices);
  LGraphs := Default(TRouteChoices);
  LStates := Default(TRouteChoices);
  LBindingChoices := Default(TRouteChoices);
  LDestinations := Default(TRouteChoices);
  LEvents := NyxEventsMetadata(LControl, ADocument);
  for LIndex := 0 to High(LEvents) do
  begin
    LDomain := NyxNoDomain;
    LDetails := False;

    if LEvents[LIndex].Payload.Defined then
    begin

      if LEvents[LIndex].Payload.Kind = nepScalar then
      begin
        LDomain := LEvents[LIndex].Payload.Domain;
      end
      else if LEvents[LIndex].Payload.Kind = nepData then
      begin
        LDetails := True;
      end;
    end
    else if LEvents[LIndex].Trigger <> ntNamed then
    begin
      ResolveNyxEventValueContract(LControl, LControl, LControl,
        LEvents[LIndex].Trigger, LValueNode, LDomain);
    end;
    LFamily := -1;

    if LDomain.Defined then
    begin
      LFamily := Ord(NyxFlowStateType(LDomain.Kind));
    end;
    LTitle := NyxTriggerTitle(LEvents[LIndex].Trigger);

    if LEvents[LIndex].Trigger = ntNamed then
    begin
      LTitle := LEvents[LIndex].Name.Name;
    end;
    LEventData.Add(NyxObject([
      NyxField('trigger', NyxData(Ord(LEvents[LIndex].Trigger))),
      NyxField('event', NyxData(LEvents[LIndex].Name.Name)),
      NyxField('scalar', NyxData(LFamily)), NyxField('details', NyxData(LDetails))]), LTitle);
  end;
  for LIndex := 0 to ADocument.Flows.Count - 1 do
  begin
    LReference := ADocument.Flows.Reference(LIndex);
    LDefinition := ADocument.Flows.Definition(LReference);
    LPorts := Default(TRouteChoices);
    for LStep := 0 to LDefinition.StepCount - 1 do
    begin
      LDescriptor := LDefinition.Descriptor(LStep);
      for LPart := 0 to LDescriptor.PortCount - 1 do
      begin
        LPort := LDescriptor.Port(LPart);
        LPorts.Add(NyxObject([
          NyxField('step', NyxData(LDefinition.Step(LStep).Name)),
          NyxField('port', NyxData(LPort.Reference.Name)),
          NyxField('type', NyxData(Ord(LPort.ValueType))),
          NyxField('direction', NyxData(Ord(LPort.Direction))),
          NyxField('optional', NyxData(LPort.Optional)),
          NyxField('description', NyxData(LPort.Description))]),
          LDefinition.Step(LStep).Name + ' / ' + LPort.Reference.Name + ' / ' + NyxFlowTypeName(LPort.ValueType));
      end;
    end;
    { Discovery needs only exact signatures/wires. Do not retain every graph's
      potentially large constants, programs or creator configuration in a form. }
    LGraphs.Add(NyxObject([NyxField('name', NyxData(LReference.Name)),
      NyxField('ports', LPorts.Finish), NyxField('wires', LDefinition.ToData.Field('wires'))]), LReference.Name);
  end;
  for LIndex := 0 to ADocument.State.Count - 1 do
  begin
    LName := ADocument.State.Key(LIndex);
    LStateKind := ADocument.State.Value(LName).Kind;
    LStates.Add(NyxObject([NyxField('state', NyxData(LName)),
      NyxField('kind', NyxData(Ord(LStateKind)))]), 'State / ' + LName);
  end;
  for LIndex := 0 to ADocument.Count - 1 do
  begin
    Bindings(ADocument.Pages[LIndex]);
  end;
  for LIndex := 0 to ADocument.ComponentCount - 1 do
  begin
    Bindings(ADocument.Components[LIndex]);
  end;
  LStateData := LStates.Finish;
  LBindings := LBindingChoices.Finish;
  for LIndex := 0 to LBindings.Count - 1 do
  begin
    LTitle := LBindings.Item(LIndex).Field('control').AsText +
      ' / ' + NyxBindingPropertyTitle(TNyxBindingProperty(LBindings.Item(LIndex).Field('property').AsInteger));
    LStates.Add(LBindings.Item(LIndex), LTitle);
    LDestinations.Add(LBindings.Item(LIndex), LTitle);
  end;
  for LIndex := 0 to LStateData.Count - 1 do
  begin
    LDestinations.Add(LStateData.Item(LIndex), 'State / ' + LStateData.Item(LIndex).Field('state').AsText);
  end;
  Result := NyxObject([NyxField('owner', NyxData(AOwner.ID)),
    NyxField('routes', NyxControlFlowRoutes(LControl).ToData),
    NyxField('events', LEventData.Finish), NyxField('graphs', LGraphs.Finish),
    NyxField('states', LStates.Finish), NyxField('destinations', LDestinations.Finish)]);
end;

function Context(AForm: TNyxNode): TNyxDataValue;
begin
  Result := AForm.Extensions.Value(NyxExtension(CEditor)).Field('snapshot');
end;

procedure CheckContext(AForm: TNyxNode; ADocument: TNyxDocument; ARevision: Integer);
var
  LContext: TNyxDataValue;
begin
  LContext := AForm.Extensions.Value(NyxExtension(CEditor));

  if (LContext.Field('revision').AsInteger <> ARevision) or
    (Snapshot(ADocument, NyxControl(LContext.Field('snapshot').Field('owner').AsText)).ToJSON <>
      LContext.Field('snapshot').ToJSON) then
  begin
    raise ENyxFlow.Create('Reopen this component before changing its event routes');
  end;
end;

function SelectedGraph(AForm: TNyxNode): TNyxDataValue;
begin
  Result := Choice(Context(AForm).Field('graphs'), Value(AForm, 'graph'));
end;

function Endpoint(const AData: TNyxDataValue): TNyxFlowEndpoint;
begin
  Result := NyxFlowEndpoint(NyxFlowStep(AData.Field('step').AsText),
    NyxFlowPort(AData.Field('port').AsText));
end;

function Port(AForm: TNyxNode; AInput: Boolean): TNyxDataValue;
var
  LName: TNyxText;
begin
  LName := 'output';

  if AInput then
  begin
    LName := 'input';
  end;
  Result := Choice(AForm.Extensions.Value(NyxExtension(CPorts)).Field(LName), Value(AForm, LName));
end;

procedure RefreshPorts(AForm: TNyxNode);
var
  LGraph: TNyxDataValue;
  LMetadata: TNyxDataValue;
  LData: TNyxDataValue;
  LInputs: TRouteChoices;
  LOutputs: TRouteChoices;
  LInputData: TNyxDataValue;
  LOutputData: TNyxDataValue;
  LWires: TNyxDataValue;
  LEndpoint: TNyxFlowEndpoint;
  LIndex: Integer;
  LWire: Integer;
  LWired: Boolean;
begin
  LInputs := Default(TRouteChoices);
  LOutputs := Default(TRouteChoices);

  if Value(AForm, 'graph') <> '' then
  begin
    LGraph := SelectedGraph(AForm);
    LWires := LGraph.Field('wires');
    LMetadata := LGraph.Field('ports');
    for LIndex := 0 to LMetadata.Count - 1 do
    begin
      LData := LMetadata.Item(LIndex);
      LEndpoint := Endpoint(LData);
      LWired := False;
      for LWire := 0 to LWires.Count - 1 do
      begin
        LWired := LWired or Endpoint(LWires.Item(LWire).Field('target')).Same(LEndpoint);
      end;

      if LData.Field('direction').AsInteger = Ord(nfdOutput) then
      begin
        LOutputs.Add(LData, LEndpoint.Step.Name + ' / ' + LEndpoint.Port.Name +
          ' / ' + NyxFlowTypeName(TNyxFlowType(LData.Field('type').AsInteger)));
      end
      else if not LWired then
      begin
        LInputs.Add(LData, LEndpoint.Step.Name + ' / ' + LEndpoint.Port.Name +
          ' / ' + NyxFlowTypeName(TNyxFlowType(LData.Field('type').AsInteger)));
      end;
    end;
  end;
  LInputData := LInputs.Finish;
  LOutputData := LOutputs.Finish;
  AForm.Extensions.SetValue(NyxExtension(CPorts),
    NyxObject([NyxField('input', LInputData), NyxField('output', LOutputData)]));
  Field(AForm, 'input').Configure.Items(Items(LInputData)).Value(First(LInputData))
    .Enabled(LInputs.Count > 0).Done;
  Field(AForm, 'output').Configure.Items(Items(LOutputData)).Value(First(LOutputData))
    .Enabled(LOutputs.Count > 0).Done;
end;

{ Mapping construction must not require a finished route name/run-limit field.
  Those independent drafts are admitted together only by CurrentRoute/Apply. }
function MappingRoute(AForm: TNyxNode): TNyxFlowRoute;
begin
  Result := NyxFlowRoute(NyxFlowRouteID('Route mapping'),
    NyxFlowRef(SelectedGraph(AForm).Field('name').AsText), ntClick, nfcmAfterDispatch);
end;

function Header(AForm: TNyxNode): TNyxFlowRoute;
var
  LEvent: TNyxDataValue;
  LMode: TNyxFlowCallbackMode;
  LTrigger: TNyxTrigger;
  LLimit: Integer;
begin
  LEvent := Choice(Context(AForm).Field('events'), Value(AForm, 'event'));
  LMode := TNyxFlowCallbackMode(EnumIndex(Value(AForm, 'callbacks'), CCallbacks));
  LTrigger := TNyxTrigger(LEvent.Field('trigger').AsInteger);

  if LTrigger = ntNamed then
  begin
    Result := NyxNamedFlowRoute(NyxFlowRouteID(Value(AForm, 'name')),
      NyxFlowRef(SelectedGraph(AForm).Field('name').AsText),
      NyxNamedEvent(LEvent.Field('event').AsText), LMode);
  end
  else
  begin
    Result := NyxFlowRoute(NyxFlowRouteID(Value(AForm, 'name')),
      NyxFlowRef(SelectedGraph(AForm).Field('name').AsText), LTrigger, LMode);
  end;

  if not TryStrToInt(String(Value(AForm, 'limit')), LLimit) then
  begin
    raise ENyxFlow.Create('Maximum active runs requires an integer');
  end;
  Result := Result.Using(TNyxExecutionPolicy(EnumIndex(Value(AForm, 'policy'), CPolicies)))
    .Concurrent(TNyxFlowConcurrency(EnumIndex(Value(AForm, 'concurrency'), CConcurrency)))
    .Enabled(Flag(AForm, 'enabled')).MaximumRuns(LLimit);
end;

function CurrentRoute(AForm: TNyxNode): TNyxFlowRoute;
var
  LData: TNyxDataValue;
  LWorking: TNyxDataValue;
  LIndex: Integer;
begin
  LData := Header(AForm).ToData;
  LWorking := AForm.Extensions.Value(NyxExtension(CWorking));
  for LIndex := 0 to LWorking.Count - 1 do
  begin

    if (LWorking.Key(LIndex) = 'inputs') or (LWorking.Key(LIndex) = 'outputs') or
      (LWorking.Key(LIndex) = 'rowOutputs') then
    begin
      LData := Put(LData, LWorking.Key(LIndex), LWorking.Field(LWorking.Key(LIndex)));
    end;
  end;
  Result := TNyxFlowRoute.FromData(LData);
end;

procedure ShowMappings(AForm: TNyxNode);
var
  LWorking: TNyxDataValue;
begin
  LWorking := AForm.Extensions.Value(NyxExtension(CWorking));
  Field(AForm, 'summary').Configure.Text(
    'Mapped inputs: ' + TNyxText(IntToStr(LWorking.Field('inputs').Count)) +
    ' / state outputs: ' + TNyxText(IntToStr(LWorking.Field('outputs').Count)) +
    ' / Table outputs: ' + TNyxText(IntToStr(TNyxFlowRoute.FromData(LWorking).RowOutputCount)) +
    '. Apply publishes the complete route.').Done;
end;

{ Replace/remove one exact mapping in the inert form snapshot. The mapping
  itself is first constructed through typed route methods; full graph/control
  admission still belongs to Capture and the ordinary paired command. }
function Map(const AValues, AEntry: TNyxDataValue; AInput, ARemove: Boolean): TNyxDataValue;
var
  LItems: array of TNyxDataValue;
  LIndex: Integer;
  LCount: Integer;
  LFound: Boolean;
  LMatch: Boolean;
begin
  SetLength(LItems, AValues.Count + 1);
  LCount := 0;
  LFound := False;
  for LIndex := 0 to AValues.Count - 1 do
  begin

    if AInput then
    begin
      LMatch := Endpoint(AValues.Item(LIndex).Field('target')).Same(Endpoint(AEntry.Field('target')));
    end
    else
    begin
      LMatch := AValues.Item(LIndex).Field('state').AsText = AEntry.Field('state').AsText;
    end;

    if LMatch then
    begin
      LFound := True;

      if ARemove then
      begin
        Continue;
      end;
      LItems[LCount] := AEntry;
    end
    else
    begin
      LItems[LCount] := AValues.Item(LIndex);
    end;
    Inc(LCount);
  end;

  if not LFound then
  begin

    if ARemove then
    begin
      raise ENyxFlow.Create('The selected mapping does not exist');
    end;
    LItems[LCount] := AEntry;
    Inc(LCount);
  end;
  SetLength(LItems, LCount);
  Result := NyxArray(LItems);
end;

procedure MapInput(AForm: TNyxNode; ARemove: Boolean);
var
  LRoute: TNyxFlowRoute;
  LPort: TNyxDataValue;
  LSource: TNyxFlowInputSource;
  LType: TNyxFlowType;
  LEvent: TNyxDataValue;
  LState: TNyxDataValue;
  LTarget: TNyxFlowEndpoint;
  LInteger: Integer;
  LWorking: TNyxDataValue;
  LPath: TNyxResourcePath;
begin
  LPort := Port(AForm, True);
  LType := TNyxFlowType(LPort.Field('type').AsInteger);
  LTarget := Endpoint(LPort);
  LWorking := AForm.Extensions.Value(NyxExtension(CWorking));

  if ARemove then
  begin

    if not Flag(AForm, 'confirm') then
    begin
      raise ENyxFlow.Create('Confirm mapping removal before changing this route');
    end;
    LWorking := Put(LWorking, 'inputs', Map(LWorking.Field('inputs'),
      NyxObject([NyxField('target', LPort)]), True, True));
  end
  else
  begin
    LRoute := MappingRoute(AForm);
    LSource := TNyxFlowInputSource(EnumIndex(Value(AForm, 'source'), CSources));
    LEvent := Choice(Context(AForm).Field('events'), Value(AForm, 'event'));
    case LSource of
      nfisEventValue:
        begin

          if LEvent.Field('scalar').AsInteger <> Ord(LType) then
          begin
            raise ENyxFlow.Create('This event has no matching scalar value; choose state or an explicit transform');
          end;
          LRoute := LRoute.InputEventValue(LTarget, LType);
        end;
      nfisEventDetails:
        begin

          if not LEvent.Field('details').AsBoolean or (LType <> nftJSON) then
          begin
            raise ENyxFlow.Create('Structured event details require a declared JSON payload and JSON input');
          end;
          LPath := TNyxResourcePath.FromData(AForm.Extensions.Value(NyxExtension(CPath)));
          LRoute := LRoute.InputEventDetails(LTarget, LPath);
        end;
      nfisSignal:
        begin

          if LType <> nftSignal then
          begin
            raise ENyxFlow.Create('Invocation signals require a Signal input');
          end;
          LRoute := LRoute.InputSignal(LTarget);
        end;
      nfisState:
        begin
          LState := Choice(Context(AForm).Field('states'), Value(AForm, 'state'));

          if Ord(NyxFlowStateType(TNyxStateKind(LState.Field('kind').AsInteger))) <> Ord(LType) then
          begin
            raise ENyxFlow.Create('The chosen state/control binding has a different scalar family');
          end;
          case TNyxStateKind(LState.Field('kind').AsInteger) of
            nskText: LRoute := LRoute.InputState(LTarget, NyxTextState(LState.Field('state').AsText));
            nskBoolean: LRoute := LRoute.InputState(LTarget, NyxBooleanState(LState.Field('state').AsText));
            nskInteger: LRoute := LRoute.InputState(LTarget, NyxIntegerState(LState.Field('state').AsText));
            nskNumber: LRoute := LRoute.InputState(LTarget, NyxNumberState(LState.Field('state').AsText));
          end;
        end;
      nfisConstant:
        begin
          case LType of
            nftText: LRoute := LRoute.InputConstant(LTarget, NyxFlowText(Value(AForm, 'constant')));
            nftBoolean: LRoute := LRoute.InputConstant(LTarget, NyxFlowBoolean(Flag(AForm, 'constant-boolean')));
            nftInteger:
              begin

                if not TryStrToInt(String(Value(AForm, 'constant')), LInteger) then
                begin
                  raise ENyxFlow.Create('The constant requires an exact portable integer');
                end;
                LRoute := LRoute.InputConstant(LTarget, NyxFlowInteger(LInteger));
              end;
            nftNumber: LRoute := LRoute.InputConstant(LTarget, NyxFlowNumber(NyxDecimal(Value(AForm, 'constant'))));
            nftJSON: LRoute := LRoute.InputConstant(LTarget, NyxFlowJSON(TNyxDataValue.ParseJSON(Value(AForm, 'constant'))));
            nftBytes: LRoute := LRoute.InputConstant(LTarget, NyxFlowBytes(NyxDecodeBase64(Value(AForm, 'constant'))));
            nftSignal: LRoute := LRoute.InputConstant(LTarget, NyxFlowSignal);
          end;
        end;
    end;
    LWorking := Put(LWorking, 'inputs', Map(LWorking.Field('inputs'),
      LRoute.ToData.Field('inputs').Item(0), True, False));
  end;
  AForm.Extensions.SetValue(NyxExtension(CWorking), LWorking);
  Field(AForm, 'confirm').Configure.Value(False).Done;
  ShowMappings(AForm);
end;

procedure MapOutput(AForm: TNyxNode; ARemove: Boolean);
var
  LPort: TNyxDataValue;
  LState: TNyxDataValue;
  LSource: TNyxFlowEndpoint;
  LRoute: TNyxFlowRoute;
  LWorking: TNyxDataValue;
begin
  LPort := Port(AForm, False);
  LState := Choice(Context(AForm).Field('destinations'), Value(AForm, 'destination'));

  if Ord(NyxFlowStateType(TNyxStateKind(LState.Field('kind').AsInteger))) <>
    LPort.Field('type').AsInteger then
  begin
    raise ENyxFlow.Create('Output and destination require the same scalar family; JSON/bytes need explicit transforms');
  end;
  LSource := Endpoint(LPort);
  LRoute := MappingRoute(AForm);
  case TNyxStateKind(LState.Field('kind').AsInteger) of
    nskText: LRoute := LRoute.OutputState(LSource, NyxTextState(LState.Field('state').AsText));
    nskBoolean: LRoute := LRoute.OutputState(LSource, NyxBooleanState(LState.Field('state').AsText));
    nskInteger: LRoute := LRoute.OutputState(LSource, NyxIntegerState(LState.Field('state').AsText));
    nskNumber: LRoute := LRoute.OutputState(LSource, NyxNumberState(LState.Field('state').AsText));
  end;

  if ARemove and not Flag(AForm, 'confirm') then
  begin
    raise ENyxFlow.Create('Confirm mapping removal before changing this route');
  end;
  LWorking := AForm.Extensions.Value(NyxExtension(CWorking));
  LWorking := Put(LWorking, 'outputs', Map(LWorking.Field('outputs'),
    LRoute.ToData.Field('outputs').Item(0), False, ARemove));
  AForm.Extensions.SetValue(NyxExtension(CWorking), LWorking);
  Field(AForm, 'confirm').Configure.Value(False).Done;
  ShowMappings(AForm);
end;

procedure LoadRoute(AForm: TNyxNode);
var
  LRoutes: INyxFlowRoutes;
  LRoute: TNyxFlowRoute;
  LEvents: TNyxDataValue;
  LGraphs: TNyxDataValue;
  LIndex: Integer;
  LLabel: TNyxText;
begin
  LRoutes := NyxFlowRoutesFromData(Context(AForm).Field('routes'));
  LLabel := Value(AForm, 'route');

  if LLabel = 'New route' then
  begin
    Field(AForm, 'name').Configure.Value('New route').Done;
    Field(AForm, 'callbacks').Configure.Value(CCallbacks[nfcmAfterDispatch]).Done;
    Field(AForm, 'policy').Configure.Value(CPolicies[neUIQueue]).Done;
    Field(AForm, 'concurrency').Configure.Value(CConcurrency[nfrLatest]).Done;
    Field(AForm, 'enabled').Configure.Value(True).Done;
    Field(AForm, 'limit').Configure.Value('8').Done;
    AForm.Extensions.SetValue(NyxExtension(CWorking), Header(AForm).ToData);
  end
  else
  begin
    LIndex := 0;
    while LIndex < LRoutes.Count do
    begin

      if LLabel = TNyxText(IntToStr(LIndex + 1)) + '. ' + Caption(LRoutes.Route(LIndex).ID.Name) then
      begin
        Break;
      end;
      Inc(LIndex);
    end;

    if LIndex = LRoutes.Count then
    begin
      raise ENyxFlow.Create('Choose an exact saved route');
    end;
    LRoute := LRoutes.Route(LIndex);
    Field(AForm, 'name').Configure.Value(LRoute.ID.Name).Done;
    Field(AForm, 'callbacks').Configure.Value(CCallbacks[LRoute.CallbackMode]).Done;
    Field(AForm, 'policy').Configure.Value(CPolicies[LRoute.Policy]).Done;
    Field(AForm, 'concurrency').Configure.Value(CConcurrency[LRoute.Concurrency]).Done;
    Field(AForm, 'enabled').Configure.Value(LRoute.IsEnabled).Done;
    Field(AForm, 'limit').Configure.Value(TNyxText(IntToStr(LRoute.RunLimit))).Done;
    LEvents := Context(AForm).Field('events');
    for LIndex := 0 to LEvents.Count - 1 do
    begin

      if (LEvents.Item(LIndex).Field('trigger').AsInteger = Ord(LRoute.Trigger)) and
        (LEvents.Item(LIndex).Field('event').AsText = LRoute.EventName.Name) then
      begin
        Field(AForm, 'event').Configure.Value(LEvents.Item(LIndex).Field('label').AsText).Done;
      end;
    end;
    LGraphs := Context(AForm).Field('graphs');
    for LIndex := 0 to LGraphs.Count - 1 do
    begin

      if LGraphs.Item(LIndex).Field('name').AsText = LRoute.Flow.Name then
      begin
        Field(AForm, 'graph').Configure.Value(LGraphs.Item(LIndex).Field('label').AsText).Done;
      end;
    end;
    AForm.Extensions.SetValue(NyxExtension(CWorking), LRoute.ToData);
  end;
  RefreshPorts(AForm);
  Field(AForm, 'confirm').Configure.Value(False).Done;
  ShowMappings(AForm);
end;

function FieldValues(AForm: TNyxNode): TNyxDataValue;
var
  LFields: array[0..High(CFields)] of TNyxDataField;
  LIndex: Integer;
begin
  for LIndex := 0 to High(CFields) do
  begin
    LFields[LIndex] := NyxField(CFields[LIndex], NyxData(Value(AForm, CFields[LIndex])));
  end;
  Result := NyxObject(LFields);
end;

{ Keep advanced payload controls available without occupying the default event
  → script → control workflow. Values remain owned while a field is hidden. }
procedure ShowSourceFields(AForm: TNyxNode);
var
  LSource: TNyxFlowInputSource;
  LType: Integer;
  LDetails: Boolean;
begin
  LSource := TNyxFlowInputSource(EnumIndex(Value(AForm, 'source'), CSources));
  LType := -1;

  if Value(AForm, 'input') <> '' then
  begin
    LType := Port(AForm, True).Field('type').AsInteger;
  end;
  LDetails := LSource = nfisEventDetails;
  Field(AForm, 'state').Configure.Visible(LSource = nfisState).Done;
  Field(AForm, 'constant').Configure.Visible((LSource = nfisConstant) and
    (LType <> Ord(nftBoolean)) and (LType <> Ord(nftSignal))).Done;
  Field(AForm, 'constant-boolean').Configure.Visible((LSource = nfisConstant) and
    (LType = Ord(nftBoolean))).Done;
  Field(AForm, 'path-part').Configure.Visible(LDetails).Done;
  Field(AForm, 'path-kind').Configure.Visible(LDetails).Done;
  Field(AForm, 'path-actions').Configure.Visible(LDetails).Done;
  Field(AForm, 'path-preview').Configure.Visible(LDetails).Done;
end;

function NewNyxControlRouteEditor(const AID: TNyxText; ADocument: TNyxDocument;
  const AOwner: TNyxControlRef; ARevision: Integer): INyxControlRouteEditor;
var
  LSnapshot: TNyxDataValue;
  LRoutes: INyxFlowRoutes;
  LItems: TNyxText;
  LIndex: Integer;
  LEnabled: Boolean;
  LForm: TNyxNode;
  LActions: INyxRow;
  LFooter: INyxNode;
  LChild: Integer;

  procedure Section(const AName, ATitle: TNyxText; AOpen: Boolean;
    const ANames: array of TNyxText);
  var
    LSection: INyxDisclosure;
    LNode: TNyxNode;
    LPart: Integer;
    LChild: Integer;
  begin
    LSection := NewNyxDisclosure(AID + '-' + AName).WithText(ATitle);
    LSection.Configure.Value(AOpen).Surface(False).Padding(0).Gap(10).Done;
    Result.Add(LSection);
    for LPart := 0 to High(ANames) do
    begin
      LNode := Result.Node.Find(AID + '-' + ANames[LPart]);

      if (LNode = nil) or (LNode.Parent <> Result.Node) then
      begin
        raise ENyxFlow.Create('Route sections require their exact independently owned controls');
      end;
      for LChild := 0 to Result.Node.Count - 1 do
      begin

        if Result.Node.Children[LChild] = LNode then
        begin
          Result.Node.Extract(LChild);
          Break;
        end;
      end;
      LSection.BodyColumn.Node.Add(LNode);
    end;
  end;

  procedure Select(const AName, ATitle, AItems, AValue: TNyxText);
  begin
    Result.Add(NewNyxSelect(AID + '-' + AName).Configure.Text(ATitle)
      .Items(AItems).Value(AValue).Enabled(LEnabled).Done);
  end;

begin

  if AID = '' then
  begin
    raise ENyxFlow.Create('A control route editor requires a non-empty form identity');
  end;
  LSnapshot := Snapshot(ADocument, AOwner);
  LEnabled := (LSnapshot.Field('graphs').Count > 0) and (LSnapshot.Field('events').Count > 0);
  Result := TControlRouteCard.Create(AID);
  LForm := Result.Node;
  LForm.Extensions.SetValue(NyxExtension(CEditor), NyxObject([
    NyxField('revision', NyxData(ARevision)), NyxField('snapshot', LSnapshot)]));
  LForm.Extensions.SetValue(NyxExtension(CPath), NyxResourcePath.ToData);
  Result.Configure.Padding(16).Gap(10).Done;
  Result.Add(NewNyxHeading(AID + '-title').WithText('Component event routes'));
  Result.Add(NewNyxLabel(AID + '-help').WithText(
    'Connect this component''s event to a graph or Pascal script. Stage mappings, then Apply once.'));

  if not LEnabled then
  begin
    Result.Add(NewNyxLabel(AID + '-unavailable').WithText(
      'Add a Logic graph and choose a component with events before creating a route.'));
  end;
  LRoutes := NyxFlowRoutesFromData(LSnapshot.Field('routes'));
  LItems := 'New route';
  for LIndex := 0 to LRoutes.Count - 1 do
  begin
    LItems := LItems + #10 + TNyxText(IntToStr(LIndex + 1)) + '. ' + Caption(LRoutes.Route(LIndex).ID.Name);
  end;
  Select('route', 'Saved route', LItems, 'New route');
  Result.Add(NewNyxInput(AID + '-name').Configure.Text('Route name').Value('New route')
    .Enabled(LEnabled).Done);
  Select('event', 'Component event', Items(LSnapshot.Field('events')), First(LSnapshot.Field('events')));
  Select('graph', 'Graph / script pipeline', Items(LSnapshot.Field('graphs')), First(LSnapshot.Field('graphs')));
  Select('callbacks', 'Ordinary callbacks', EnumItems(CCallbacks), CCallbacks[nfcmAfterDispatch]);
  Select('policy', 'Execution policy', EnumItems(CPolicies), CPolicies[neUIQueue]);
  Select('concurrency', 'While work is running', EnumItems(CConcurrency), CConcurrency[nfrLatest]);
  Result.Add(NewNyxInput(AID + '-limit').Configure.Text('Maximum active runs').Value('8').Done);
  Result.Add(NewNyxSwitch(AID + '-enabled').Configure.Text('Enable this route').Value(True).Done);
  Result.Add(NewNyxHeading(AID + '-inputs-title').WithText('Graph inputs'));
  Select('input', 'Unwired graph input', '', '');
  Select('source', 'Value comes from', EnumItems(CSources), CSources[nfisEventValue]);
  Select('state', 'Existing state / control binding', Items(LSnapshot.Field('states')), First(LSnapshot.Field('states')));
  Result.Add(NewNyxMemo(AID + '-constant').Configure.Text('Constant (text, number, JSON or Base64 bytes)')
    .Value('').Height(90).Done);
  Result.Add(NewNyxSwitch(AID + '-constant-boolean').Configure.Text('Boolean constant').Value(False).Done);
  Result.Add(NewNyxInput(AID + '-path-part').Configure.Text('Event details path segment').Value('').Done);
  Select('path-kind', 'Path segment kind', 'Field' + #10 + 'Item', 'Field');
  LActions := NewNyxRow(AID + '-path-actions');
  LActions.Configure.Wrap(nfwWrap).Gap(8).Done;
  LActions.Add(NewNyxButton(AID + '-path-add').WithText('Add path segment'));
  LActions.Add(NewNyxButton(AID + '-path-clear').WithText('Use complete event data'));
  Result.Add(LActions);
  Result.Add(NewNyxLabel(AID + '-path-preview').WithText('Event details: complete data'));
  LActions := NewNyxRow(AID + '-input-actions');
  LActions.Configure.Wrap(nfwWrap).Gap(8).Done;
  LActions.Add(NewNyxButton(AID + '-map-input').Configure.Text('Map input').Enabled(LEnabled).Done);
  LActions.Add(NewNyxButton(AID + '-unmap-input').Configure.Text('Remove input mapping').Enabled(LEnabled).Done);
  Result.Add(LActions);
  Result.Add(NewNyxHeading(AID + '-outputs-title').WithText('Control updates'));
  Select('output', 'Graph output', '', '');
  Select('destination', 'Existing control binding / state', Items(LSnapshot.Field('destinations')),
    First(LSnapshot.Field('destinations')));
  Result.Add(NewNyxLabel(AID + '-binding-help').WithText(
    'A state write updates every control bound to it. Create new bindings in the Bindings inspector. JSON/bytes need an explicit transform.'));
  LActions := NewNyxRow(AID + '-output-actions');
  LActions.Configure.Wrap(nfwWrap).Gap(8).Done;
  LActions.Add(NewNyxButton(AID + '-map-output').Configure.Text('Map output').Enabled(LEnabled).Done);
  LActions.Add(NewNyxButton(AID + '-unmap-output').Configure.Text('Remove output mapping').Enabled(LEnabled).Done);
  Result.Add(LActions);
  Result.Add(NewNyxLabel(AID + '-summary').WithText(''));
  Result.Add(NewNyxSwitch(AID + '-confirm').Configure.Text('Confirm mapping/route removal or replacement').Value(False).Done);
  LActions := NewNyxRow(AID + '-actions');
  LActions.Configure.Wrap(nfwWrap).Gap(8).Done;
  LActions.Add(NewNyxButton(AID + '-apply').Configure.Text('Apply event route')
    .Variant(nvPrimary).Enabled(LEnabled).Done);
  LActions.Add(NewNyxButton(AID + '-remove').Configure.Text('Remove saved route').Enabled(LRoutes.Count > 0).Done);
  LActions.Add(NewNyxButton(AID + '-reset-mappings').Configure.Text('Clear staged mappings').Enabled(LEnabled).Done);
  Result.Add(LActions);

  if LEnabled then
  begin
    LForm.Extensions.SetValue(NyxExtension(CWorking), Header(LForm).ToData);
  end
  else
  begin
    LForm.Extensions.SetValue(NyxExtension(CWorking), NyxNull);
  end;
  RefreshPorts(LForm);

  if LEnabled then
  begin
    ShowMappings(LForm);
  end;
  Section('execution', 'Execution and ordinary callbacks', False,
    ['callbacks', 'policy', 'concurrency', 'limit', 'enabled']);
  Section('input-section', 'Input mappings', True,
    ['inputs-title', 'input', 'source', 'state', 'constant', 'constant-boolean',
     'path-part', 'path-kind', 'path-actions', 'path-preview', 'input-actions']);
  Section('output-section', 'Control and state outputs', True,
    ['outputs-title', 'output', 'destination', 'binding-help', 'output-actions']);
  { Place the common footer after the collapsible sections without replacing
    its controls. Sections adopt only their listed independent children. }
  for LIndex := 0 to 2 do
  begin
    LItems := 'summary';

    if LIndex = 1 then
    begin
      LItems := 'confirm';
    end
    else if LIndex = 2 then
    begin
      LItems := 'actions';
    end;
    LFooter := RetainNyxControl(Field(LForm, LItems));
    for LChild := 0 to LForm.Count - 1 do
    begin

      if LForm.Children[LChild] = LFooter.Node then
      begin
        LForm.Extract(LChild);
        Break;
      end;
    end;
    LForm.Add(LFooter);
  end;
  ShowSourceFields(LForm);
  LForm.Extensions.SetValue(NyxExtension(CInitial), NyxObject([
    NyxField('values', FieldValues(LForm)),
    NyxField('sections', NyxObject([
      NyxField('execution', NyxData(False)),
      NyxField('input-section', NyxData(True)),
      NyxField('output-section', NyxData(True))])),
    NyxField('working', LForm.Extensions.Value(NyxExtension(CWorking)))]));
end;

function HandleNyxControlRouteEditor(ANode, ARoot: TNyxNode;
  ADocument: TNyxDocument; ARevision: Integer): Boolean;
var
  LForm: TNyxNode;
  LID: TNyxText;
  LPath: TNyxResourcePath;
  LIndex: Integer;
  LData: TNyxDataValue;
  LKnown: Boolean;
  LSection: TNyxNode;
  LBody: TNyxNode;
  LCaption: TNyxText;
  LRoute: TNyxFlowRoute;
begin
  Result := False;
  LForm := Form(ANode, ARoot);

  if LForm = nil then
  begin
    Exit;
  end;
  LID := ANode.ID;

  if NyxDisclosureHeader(ANode, LSection, LBody, LCaption) and
    (LSection.Parent = LForm) then
  begin
    { The public disclosure already admitted its toggle before notification.
      Consume that local event so Studio synchronizes this owned form rather
      than rebuilding it with closed defaults and discarding the user's input. }
    CheckContext(LForm, ADocument, ARevision);
    Exit(True);
  end;

  if (LID = LForm.ID + '-apply') or (LID = LForm.ID + '-remove') then
  begin
    Exit;
  end;
  LKnown := (LID = LForm.ID + '-map-input') or (LID = LForm.ID + '-unmap-input') or
    (LID = LForm.ID + '-map-output') or (LID = LForm.ID + '-unmap-output') or
    (LID = LForm.ID + '-path-add') or (LID = LForm.ID + '-path-clear') or
    (LID = LForm.ID + '-path-part') or (LID = LForm.ID + '-path-kind') or
    (LID = LForm.ID + '-reset-mappings');
  for LIndex := 0 to High(CFields) do
  begin
    LKnown := LKnown or (LID = LForm.ID + '-' + CFields[LIndex]);
  end;

  if not LKnown then
  begin
    Exit;
  end;
  CheckContext(LForm, ADocument, ARevision);

  if LID = LForm.ID + '-route' then
  begin
    LoadRoute(LForm);
  end
  else if LID = LForm.ID + '-graph' then
  begin
    { Keep staged mappings, including Table outputs, across pipeline choices.
      Whole candidate admission reports incompatible endpoints. Only an explicit
      confirmed reset below clears them. Name/number drafts remain independent. }
    LData := Put(LForm.Extensions.Value(NyxExtension(CWorking)), 'flow',
      SelectedGraph(LForm).Field('name'));
    LForm.Extensions.SetValue(NyxExtension(CWorking), LData);
    RefreshPorts(LForm);
    Field(LForm, 'confirm').Configure.Value(False).Done;
    ShowMappings(LForm);
  end
  else if LID = LForm.ID + '-reset-mappings' then
  begin

    if not Flag(LForm, 'confirm') then
    begin
      raise ENyxFlow.Create('Confirm clearing all staged scalar and Table mappings');
    end;
    LRoute := TNyxFlowRoute.FromData(LForm.Extensions.Value(NyxExtension(CWorking)));
    while LRoute.RowOutputCount > 0 do
    begin
      LRoute := LRoute.WithoutRows(LRoute.RowOutput(0).Collection);
    end;
    LData := Put(Put(LRoute.ToData, 'inputs', NyxArray([])), 'outputs', NyxArray([]));
    LForm.Extensions.SetValue(NyxExtension(CWorking), TNyxFlowRoute.FromData(LData).ToData);
    Field(LForm, 'confirm').Configure.Value(False).Done;
    ShowMappings(LForm);
  end
  else if (LID = LForm.ID + '-map-input') or (LID = LForm.ID + '-unmap-input') then
  begin
    MapInput(LForm, LID = LForm.ID + '-unmap-input');
  end
  else if (LID = LForm.ID + '-map-output') or (LID = LForm.ID + '-unmap-output') then
  begin
    MapOutput(LForm, LID = LForm.ID + '-unmap-output');
  end
  else if LID = LForm.ID + '-path-add' then
  begin
    LPath := TNyxResourcePath.FromData(LForm.Extensions.Value(NyxExtension(CPath)));

    if Value(LForm, 'path-kind') = 'Field' then
    begin
      LPath := LPath.Field(Value(LForm, 'path-part'));
    end
    else if Value(LForm, 'path-kind') = 'Item' then
    begin

      if not TryStrToInt(String(Value(LForm, 'path-part')), LIndex) or (LIndex < 0) then
      begin
        raise ENyxFlow.Create('A path Item requires a non-negative integer');
      end;
      LPath := LPath.Item(LIndex);
    end
    else
    begin
      raise ENyxFlow.Create('Choose a Field or Item path segment');
    end;
    LForm.Extensions.SetValue(NyxExtension(CPath), LPath.ToData);
    Field(LForm, 'path-preview').Configure.Text('Event details path: ' + LPath.ToData.ToJSON).Done;
  end
  else if LID = LForm.ID + '-path-clear' then
  begin
    LForm.Extensions.SetValue(NyxExtension(CPath), NyxResourcePath.ToData);
    Field(LForm, 'path-preview').Configure.Text('Event details: complete data').Done;
  end;
  ShowSourceFields(LForm);
  Result := True;
end;

function CaptureNyxControlRouteEditor(ANode, ARoot: TNyxNode;
  ADocument: TNyxDocument; ARevision: Integer;
  out AChange: TNyxControlRouteEditorChange): Boolean;
var
  LForm: TNyxNode;
  LRoutes: INyxFlowRoutes;
  LRoute: TNyxFlowRoute;
  LCandidate: TNyxDocument;
  LIndex: Integer;
  LLoadedIndex: Integer;
  LReplacement: INyxFlowRoutes;
  LLoaded: TNyxText;
begin
  AChange := Default(TNyxControlRouteEditorChange);
  LForm := Form(ANode, ARoot);
  Result := (LForm <> nil) and ((ANode.ID = LForm.ID + '-apply') or (ANode.ID = LForm.ID + '-remove'));

  if not Result then
  begin
    Exit;
  end;
  CheckContext(LForm, ADocument, ARevision);
  LRoutes := NyxFlowRoutesFromData(Context(LForm).Field('routes'));

  if ANode.ID = LForm.ID + '-remove' then
  begin

    if not Flag(LForm, 'confirm') then
    begin
      raise ENyxFlow.Create('Confirm removal of this saved event route');
    end;
    LLoaded := Value(LForm, 'route');
    LIndex := 0;
    while LIndex < LRoutes.Count do
    begin

      if LLoaded = TNyxText(IntToStr(LIndex + 1)) + '. ' + Caption(LRoutes.Route(LIndex).ID.Name) then
      begin
        Break;
      end;
      Inc(LIndex);
    end;

    if LIndex = LRoutes.Count then
    begin
      raise ENyxFlow.Create('Load the exact saved route to remove');
    end;
    LRoutes := LRoutes.WithoutRoute(LRoutes.Route(LIndex).ID);
  end
  else
  begin
    LRoute := CurrentRoute(LForm);
    LLoadedIndex := -1;
    for LIndex := 0 to LRoutes.Count - 1 do
    begin

      if Value(LForm, 'route') = TNyxText(IntToStr(LIndex + 1)) + '. ' +
        Caption(LRoutes.Route(LIndex).ID.Name) then
      begin
        LLoadedIndex := LIndex;
      end;
    end;
    for LIndex := 0 to LRoutes.Count - 1 do
    begin

      if (LRoutes.Route(LIndex).ID.Name = LRoute.ID.Name) and
        (LLoadedIndex <> LIndex) and
        not Flag(LForm, 'confirm') then
      begin
        raise ENyxFlow.Create('Load the existing same-name route, or confirm its complete replacement');
      end;
    end;
    if LLoadedIndex < 0 then
    begin
      LRoutes := LRoutes.WithRoute(LRoute);
    end
    else
    begin
      { Renaming edits the loaded slot; it must not leave a second active route
        behind. A confirmed collision replaces that other exact name. Preserve
        the registration order and every unrelated route/mapping verbatim. }
      LReplacement := NewNyxFlowRoutes;
      for LIndex := 0 to LRoutes.Count - 1 do
      begin

        if LIndex = LLoadedIndex then
        begin
          LReplacement := LReplacement.WithRoute(LRoute);
        end
        else if LRoutes.Route(LIndex).ID.Name <> LRoute.ID.Name then
        begin
          LReplacement := LReplacement.WithRoute(LRoutes.Route(LIndex));
        end;
      end;
      LRoutes := LReplacement;
    end;
  end;
  AChange.Owner := NyxControl(Context(LForm).Field('owner').AsText);
  LCandidate := ADocument.Clone;
  try
    SetNyxFlowRoutes(LCandidate.Find(AChange.Owner.ID), LRoutes);
    LCandidate.Validate;
  finally
    LCandidate.Free;
  end;
  AChange.Revision := ARevision;
  AChange.RoutesData := LRoutes.ToData;
end;

procedure TNyxControlRouteEditorDraft.Clear;
begin
  FData := NyxNull;
end;

function TNyxControlRouteEditorDraft.Defined: Boolean;
begin
  Result := FData.Defined and (FData.Kind <> ndNull);
end;

{ Presentation state belongs to the unfinished form, never the authored route.
  Keep actual Boolean disclosure contracts through close/reopen and recovery. }
function SectionValues(AForm: TNyxNode): TNyxDataValue;
begin
  Result := NyxObject([
    NyxField('execution', NyxData(NyxDisclosureExpanded(Field(AForm, 'execution')))),
    NyxField('input-section', NyxData(NyxDisclosureExpanded(Field(AForm, 'input-section')))),
    NyxField('output-section', NyxData(NyxDisclosureExpanded(Field(AForm, 'output-section'))))]);
end;

procedure TNyxControlRouteEditorDraft.Capture(const AID: TNyxText; ARoot: TNyxNode);
var
  LForm: TNyxNode;
  LValues: TNyxDataValue;
begin

  if ARoot = nil then
  begin
    Exit;
  end;
  LForm := ARoot.Find(AID);

  if (LForm = nil) or not LForm.Extensions.Has(NyxExtension(CEditor)) then
  begin
    Exit;
  end;
  LValues := FieldValues(LForm);

  if (LValues.ToJSON = LForm.Extensions.Value(NyxExtension(CInitial)).Field('values').ToJSON) and
    (SectionValues(LForm).ToJSON =
      LForm.Extensions.Value(NyxExtension(CInitial)).Field('sections').ToJSON) and
    (LForm.Extensions.Value(NyxExtension(CWorking)).ToJSON =
      LForm.Extensions.Value(NyxExtension(CInitial)).Field('working').ToJSON) and
    (LForm.Extensions.Value(NyxExtension(CPath)).Count = 0) and
    (Value(LForm, 'path-part') = '') and (Value(LForm, 'path-kind') = 'Field') then
  begin
    Clear;
    Exit;
  end;
  FData := NyxObject([NyxField('version', NyxData(2)),
    NyxField('context', LForm.Extensions.Value(NyxExtension(CEditor))),
    NyxField('values', LValues), NyxField('sections', SectionValues(LForm)),
    NyxField('working', LForm.Extensions.Value(NyxExtension(CWorking))),
    NyxField('path', LForm.Extensions.Value(NyxExtension(CPath))),
    NyxField('path-part', NyxData(Value(LForm, 'path-part'))),
    NyxField('path-kind', NyxData(Value(LForm, 'path-kind')))]);
end;

{ No external callback is entered while restoring model fields. Callers first
  stage this operation against a detached form, including all dependent choices. }
procedure RestoreFields(ATarget: TNyxNode; const AData: TNyxDataValue);
var
  LFieldIndex: Integer;
  LFieldValue: TNyxText;
begin
  EnumIndex(AData.Field('values').Field('callbacks').AsText, CCallbacks);
  EnumIndex(AData.Field('values').Field('policy').AsText, CPolicies);
  EnumIndex(AData.Field('values').Field('concurrency').AsText, CConcurrency);
  EnumIndex(AData.Field('values').Field('source').AsText, CSources);
  Field(ATarget, 'graph').Configure.Value(AData.Field('values').Field('graph').AsText).Done;
  RefreshPorts(ATarget);
  for LFieldIndex := 0 to High(CFields) do
  begin
    LFieldValue := AData.Field('values').Field(CFields[LFieldIndex]).AsText;

    if (CFields[LFieldIndex] = 'enabled') or (CFields[LFieldIndex] = 'confirm') or
      (CFields[LFieldIndex] = 'constant-boolean') then
    begin
      Field(ATarget, CFields[LFieldIndex]).Configure.Value(LFieldValue = 'true').Done;
    end
    else
    begin
      Field(ATarget, CFields[LFieldIndex]).Configure.Value(LFieldValue).Done;
    end;
  end;
  Field(ATarget, 'path-part').Configure.Value(AData.Field('path-part').AsText).Done;
  Field(ATarget, 'path-kind').Configure.Value(AData.Field('path-kind').AsText).Done;
  ATarget.Extensions.SetValue(NyxExtension(CWorking), AData.Field('working'));
  ATarget.Extensions.SetValue(NyxExtension(CPath), AData.Field('path'));
  Field(ATarget, 'path-preview').Configure.Text('Event details path: ' + AData.Field('path').ToJSON).Done;

  if AData.Field('working').Kind <> ndNull then
  begin
    ShowMappings(ATarget);
  end;
  ShowSourceFields(ATarget);

  if AData.Field('version').AsInteger = 2 then
  begin
    for LFieldIndex := 0 to High(CSections) do
    begin
      Field(ATarget, CSections[LFieldIndex]).Configure
        .Value(AData.Field('sections').Field(CSections[LFieldIndex]).AsBoolean).Done;
    end;
  end;
end;

function TNyxControlRouteEditorDraft.CanRestore(const AID: TNyxText; ARoot: TNyxNode): Boolean;
var
  LForm: TNyxNode;
  LStaged: TNyxNode;
begin
  Result := False;

  if not Defined or (ARoot = nil) then
  begin
    Exit;
  end;
  LForm := ARoot.Find(AID);

  if (LForm = nil) or not LForm.Extensions.Has(NyxExtension(CEditor)) or
    (LForm.Extensions.Value(NyxExtension(CEditor)).ToJSON <> FData.Field('context').ToJSON) then
  begin
    Exit;
  end;
  LStaged := LForm.Clone;
  try
    try
      RestoreFields(LStaged, FData);
      Result := True;
    except
      on E: ENyxFlow do
      begin
        { Preserve the complete stored draft while refusing obsolete choices.
          The parent Logic form can preflight this before restoring its fields. }
        Result := False;
      end;
    end;
  finally
    LStaged.Free;
  end;
end;

function TNyxControlRouteEditorDraft.Restore(const AID: TNyxText; ARoot: TNyxNode): Boolean;
begin
  Result := CanRestore(AID, ARoot);

  if Result then
  begin
    RestoreFields(ARoot.Find(AID), FData);
  end;
end;

function TNyxControlRouteEditorDraft.ToData: TNyxDataValue;
begin
  Result := NyxNull;

  if Defined then
  begin
    Result := FData;
  end;
end;

class function TNyxControlRouteEditorDraft.FromData(const AData: TNyxDataValue): TNyxControlRouteEditorDraft;
var
  LIndex: Integer;
  LValue: TNyxText;
begin
  Result := Default(TNyxControlRouteEditorDraft);

  if AData.Kind = ndNull then
  begin
    Exit;
  end;

  if AData.Field('version').AsInteger = 1 then
  begin
    ValidateNyxFlowFields(AData, ['version', 'context', 'values', 'working', 'path', 'path-part', 'path-kind']);
  end
  else if AData.Field('version').AsInteger = 2 then
  begin
    ValidateNyxFlowFields(AData, ['version', 'context', 'values', 'working', 'path', 'path-part', 'path-kind', 'sections']);
    ValidateNyxFlowFields(AData.Field('sections'), CSections);
    for LIndex := 0 to High(CSections) do
    begin
      AData.Field('sections').Field(CSections[LIndex]).AsBoolean;
    end;
  end
  else
  begin
    raise ENyxFlow.Create('Unknown control route draft version');
  end;
  ValidateNyxFlowFields(AData.Field('context'), ['revision', 'snapshot']);
  AData.Field('context').Field('revision').AsInteger;
  ValidateNyxFlowFields(AData.Field('context').Field('snapshot'),
    ['owner', 'routes', 'events', 'graphs', 'states', 'destinations']);
  NyxControl(AData.Field('context').Field('snapshot').Field('owner').AsText);
  NyxFlowRoutesFromData(AData.Field('context').Field('snapshot').Field('routes'));
  ValidateNyxFlowFields(AData.Field('values'), CFields);
  for LIndex := 0 to High(CFields) do
  begin
    LValue := AData.Field('values').Field(CFields[LIndex]).AsText;

    if (CFields[LIndex] = 'enabled') or (CFields[LIndex] = 'confirm') or
      (CFields[LIndex] = 'constant-boolean') then
    begin
      if (LValue <> 'true') and (LValue <> 'false') then
      begin
        raise ENyxFlow.Create('Stored route draft switches require Boolean values');
      end;
    end;
  end;
  TNyxResourcePath.FromData(AData.Field('path'));

  if AData.Field('working').Kind <> ndNull then
  begin
    TNyxFlowRoute.FromData(AData.Field('working'));
  end;
  AData.Field('path-part').AsText;
  AData.Field('path-kind').AsText;
  Result.FData := AData.Copy;
end;

end.
