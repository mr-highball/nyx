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
unit nyx.flow.routes;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.types, nyx.state, nyx.resources,
  nyx.scheduler, nyx.flow.types, nyx.flow.declarations, nyx.collections,
  nyx.collections.registry, nyx.data.rows;

const
  NyxFlowRoutesKey = 'nyx.flow-routes';
  NyxMaximumControlFlowRoutes = 32;
  NyxMaximumFlowRouteMappings = 256;

type
  { A route identity is distinct from a graph, step, port, state or handler name. }
  TNyxFlowRouteRef = record
  private
    FName: TNyxText;
  public
    property Name: TNyxText read FName;
  end;
  { Exclusive refuses matching ordinary callbacks. Before/After refer to dispatch
    invocation/submission, never worker completion. No stream policy is changed. }
  TNyxFlowCallbackMode = (nfcmExclusive, nfcmBeforeCallbacks, nfcmAfterDispatch);
  TNyxFlowConcurrency = (nfrLatest, nfrEvery, nfrIgnoreWhileRunning);
  TNyxFlowInputSource = (nfisConstant, nfisState, nfisEventValue, nfisEventDetails, nfisSignal);

  { Copied source/destination facts. There are no interfaces in portable records.
    ConstantData is a typed Flow-value wire frame only for nfisConstant; DetailsPath
    belongs only to nfisEventDetails. State names are open exact references. }
  TNyxFlowRouteInput = record
    Target: TNyxFlowEndpoint;
    Source: TNyxFlowInputSource;
    ValueType: TNyxFlowType;
    StateName: TNyxText;
    ConstantData: TNyxDataValue;
    DetailsPath: TNyxResourcePath;
  end;
  TNyxFlowRouteOutput = record
    Source: TNyxFlowEndpoint;
    StateName: TNyxText;
    StateKind: TNyxStateKind;
  end;
  { Owned JSON-to-application-collection intent. The recipe carries no runtime
    store or transport authority. Scalar output access/count remain unchanged. }
  TNyxFlowRouteRowsOutput = record
    Source: TNyxFlowEndpoint;
    Collection: TNyxCollectionRef;
    Rows: TNyxDataRows;
  end;

  { Immutable event-to-graph intent. Every fluent operation returns an independent
    value. State is sampled on the UI thread when this route observes its event;
    owned event values/details are snapshots. Workers receive no control/model.
    Inputs must cover every unwired graph input exactly; outputs explicitly map
    to existing scalar state, never implicitly convert JSON/bytes into captions. }
  TNyxFlowRoute = record
  private
    FData: TNyxDataValue;
    function WithField(const AName: TNyxText; const AValue: TNyxDataValue): TNyxFlowRoute;
    function AddInput(const AInput: TNyxFlowRouteInput): TNyxFlowRoute;
    function InputStateName(const ATarget: TNyxFlowEndpoint;
      const AName: TNyxText; AKind: TNyxStateKind): TNyxFlowRoute;
    function OutputStateName(const ASource: TNyxFlowEndpoint;
      const AName: TNyxText; AKind: TNyxStateKind): TNyxFlowRoute;
    function GetID: TNyxFlowRouteRef;
    function GetFlow: TNyxFlowRef;
    function GetTrigger: TNyxTrigger;
    function GetEvent: TNyxEventRef;
    function GetCallbackMode: TNyxFlowCallbackMode;
    function GetPolicy: TNyxExecutionPolicy;
    function GetConcurrency: TNyxFlowConcurrency;
    function GetEnabled: Boolean;
    function GetMaximumRuns: Integer;
    function GetInputCount: Integer;
    function GetOutputCount: Integer;
    function GetRowOutputCount: Integer;
  public
    function InputState(const ATarget: TNyxFlowEndpoint; const AState: TNyxTextStateRef): TNyxFlowRoute; overload;
    function InputState(const ATarget: TNyxFlowEndpoint; const AState: TNyxBooleanStateRef): TNyxFlowRoute; overload;
    function InputState(const ATarget: TNyxFlowEndpoint; const AState: TNyxIntegerStateRef): TNyxFlowRoute; overload;
    function InputState(const ATarget: TNyxFlowEndpoint; const AState: TNyxNumberStateRef): TNyxFlowRoute; overload;
    function InputConstant(const ATarget: TNyxFlowEndpoint; const AValue: TNyxFlowValue): TNyxFlowRoute;
    function InputEventValue(const ATarget: TNyxFlowEndpoint; AType: TNyxFlowType): TNyxFlowRoute;
    function InputEventDetails(const ATarget: TNyxFlowEndpoint;
      const APath: TNyxResourcePath): TNyxFlowRoute;
    function InputSignal(const ATarget: TNyxFlowEndpoint): TNyxFlowRoute;
    function OutputState(const ASource: TNyxFlowEndpoint; const AState: TNyxTextStateRef): TNyxFlowRoute; overload;
    function OutputState(const ASource: TNyxFlowEndpoint; const AState: TNyxBooleanStateRef): TNyxFlowRoute; overload;
    function OutputState(const ASource: TNyxFlowEndpoint; const AState: TNyxIntegerStateRef): TNyxFlowRoute; overload;
    function OutputState(const ASource: TNyxFlowEndpoint; const AState: TNyxNumberStateRef): TNyxFlowRoute; overload;
    { The JSON output covers the complete existing collection schema by name/
      family. Runtime revisions/generation guard coordinated state/row delivery. }
    function OutputRows(const ASource: TNyxFlowEndpoint;
      const ACollection: TNyxCollectionRef; const ARows: TNyxDataRows): TNyxFlowRoute;
    { Explicit replacement preserves the destination's existing position. New
      destinations append; OutputRows still refuses accidental duplicates. }
    function WithRows(const ASource: TNyxFlowEndpoint;
      const ACollection: TNyxCollectionRef; const ARows: TNyxDataRows): TNyxFlowRoute;
    { Removing the last row output restores the original scalar-only encoding.
      An absent destination leaves this immutable route unchanged. }
    function WithoutRows(const ACollection: TNyxCollectionRef): TNyxFlowRoute;
    function Using(APolicy: TNyxExecutionPolicy): TNyxFlowRoute;
    function Concurrent(AValue: TNyxFlowConcurrency): TNyxFlowRoute;
    function Enabled(AValue: Boolean): TNyxFlowRoute;
    { Finite active-run admission, independently tunable from graph byte budgets.
      Latest cancels superseded runs; Every refuses at this capacity rather than
      silently dropping work. IgnoreWhileRunning explicitly skips a busy route. }
    function MaximumRuns(AValue: Integer): TNyxFlowRoute;
    function Input(AIndex: Integer): TNyxFlowRouteInput;
    function Output(AIndex: Integer): TNyxFlowRouteOutput;
    function RowOutput(AIndex: Integer): TNyxFlowRouteRowsOutput;
    procedure Validate;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxFlowRoute; static;
    property ID: TNyxFlowRouteRef read GetID;
    property Flow: TNyxFlowRef read GetFlow;
    property Trigger: TNyxTrigger read GetTrigger;
    property EventName: TNyxEventRef read GetEvent;
    property CallbackMode: TNyxFlowCallbackMode read GetCallbackMode;
    property Policy: TNyxExecutionPolicy read GetPolicy;
    property Concurrency: TNyxFlowConcurrency read GetConcurrency;
    property IsEnabled: Boolean read GetEnabled;
    property RunLimit: Integer read GetMaximumRuns;
    property InputCount: Integer read GetInputCount;
    property OutputCount: Integer read GetOutputCount;
    property RowOutputCount: Integer read GetRowOutputCount;
  end;

  { Immutable per-control route set. A missing local declaration inherits; an
    explicitly empty set masks inheritance. WithRoute replaces one exact ID in
    place. Retained sets own no document, runtime, callback or implementation. }
  INyxFlowRoutes = interface(IInterface)
    ['{D1411014-6120-4C84-A708-000000000014}']
    function GetCount: Integer;
    function Route(AIndex: Integer): TNyxFlowRoute;
    function WithRoute(const ARoute: TNyxFlowRoute): INyxFlowRoutes;
    function WithoutRoute(const AID: TNyxFlowRouteRef): INyxFlowRoutes;
    function ToData: TNyxDataValue;
    property Count: Integer read GetCount;
  end;

function NyxFlowRouteID(const AName: TNyxText): TNyxFlowRouteRef;
function NyxFlowRoute(const AID: TNyxFlowRouteRef; const AFlow: TNyxFlowRef;
  ATrigger: TNyxTrigger; ACallbacks: TNyxFlowCallbackMode): TNyxFlowRoute;
function NyxNamedFlowRoute(const AID: TNyxFlowRouteRef; const AFlow: TNyxFlowRef;
  const AEvent: TNyxEventRef; ACallbacks: TNyxFlowCallbackMode): TNyxFlowRoute;
function NewNyxFlowRoutes: INyxFlowRoutes;
function NyxFlowRoutesFromData(const AData: TNyxDataValue): INyxFlowRoutes;
{ Pure signature/default admission. Does not build executable creator operations,
  evaluate graphs or borrow runtime state. The owning model separately checks
  whether the exact control supports the chosen physical/named event. }
procedure ValidateNyxFlowRoute(const ARoute: TNyxFlowRoute;
  const ADefinition: INyxFlowDefinition; AState: TNyxState;
  const ACollections: INyxCollectionDefaults = nil);
function NyxFlowStateType(AKind: TNyxStateKind): TNyxFlowType;
function NyxFlowStateKind(AType: TNyxFlowType): TNyxStateKind;

implementation

type
  TRoutes = class(TInterfacedObject, INyxFlowRoutes)
  public
    Data: TNyxDataValue;
    function GetCount: Integer;
    function Route(AIndex: Integer): TNyxFlowRoute;
    function WithRoute(const ARoute: TNyxFlowRoute): INyxFlowRoutes;
    function WithoutRoute(const AID: TNyxFlowRouteRef): INyxFlowRoutes;
    function ToData: TNyxDataValue;
    property Count: Integer read GetCount;
  end;

function NyxFlowStateType(AKind: TNyxStateKind): TNyxFlowType;
const
  CTypes: array[TNyxStateKind] of TNyxFlowType = (nftText, nftBoolean, nftInteger, nftNumber);
begin

  if (Ord(AKind) < Ord(Low(TNyxStateKind))) or (Ord(AKind) > Ord(High(TNyxStateKind))) then
  begin
    raise ENyxFlow.Create('Unknown scalar state family');
  end;
  Result := CTypes[AKind];
end;

function NyxFlowStateKind(AType: TNyxFlowType): TNyxStateKind;
begin
  case AType of
    nftText: Result := nskText;
    nftBoolean: Result := nskBoolean;
    nftInteger: Result := nskInteger;
    nftNumber: Result := nskNumber;
  else
    raise ENyxFlow.Create('Only scalar values have a state family');
  end;
end;

function EndpointData(const AValue: TNyxFlowEndpoint): TNyxDataValue;
begin
  AValue.Validate;
  Result := NyxObject([NyxField('step', NyxData(AValue.Step.Name)),
    NyxField('port', NyxData(AValue.Port.Name))]);
end;

function ReadEndpoint(const AValue: TNyxDataValue): TNyxFlowEndpoint;
begin
  ValidateNyxFlowFields(AValue, ['step', 'port']);
  Result := NyxFlowEndpoint(NyxFlowStep(AValue.Field('step').AsText),
    NyxFlowPort(AValue.Field('port').AsText));
end;

function NyxFlowRouteID(const AName: TNyxText): TNyxFlowRouteRef;
begin
  Result.FName := NyxFlowStep(AName).Name;
end;

function NyxFlowRoute(const AID: TNyxFlowRouteRef; const AFlow: TNyxFlowRef;
  ATrigger: TNyxTrigger; ACallbacks: TNyxFlowCallbackMode): TNyxFlowRoute;
begin

  if not NyxIsRuntimeTrigger(ATrigger) or
    (Ord(ACallbacks) < Ord(Low(TNyxFlowCallbackMode))) or
    (Ord(ACallbacks) > Ord(High(TNyxFlowCallbackMode))) then
  begin
    raise ENyxFlow.Create('A route requires a physical runtime trigger and explicit callback mode');
  end;
  Result.FData := NyxObject([NyxField('id', NyxData(NyxFlowRouteID(AID.Name).Name)),
    NyxField('flow', NyxData(NyxFlowRef(AFlow.Name).Name)),
    NyxField('trigger', NyxData(NyxTriggerName(ATrigger))), NyxField('event', NyxData('')),
    NyxField('callbacks', NyxData(Ord(ACallbacks))), NyxField('policy', NyxData(Ord(neUIQueue))),
    NyxField('concurrency', NyxData(Ord(nfrLatest))), NyxField('enabled', NyxData(True)),
    NyxField('maximumRuns', NyxData(8)), NyxField('inputs', NyxArray([])),
    NyxField('outputs', NyxArray([]))]);
end;

function NyxNamedFlowRoute(const AID: TNyxFlowRouteRef; const AFlow: TNyxFlowRef;
  const AEvent: TNyxEventRef; ACallbacks: TNyxFlowCallbackMode): TNyxFlowRoute;
begin
  Result := NyxFlowRoute(AID, AFlow, ntClick, ACallbacks);
  Result := Result.WithField('trigger', NyxData(NyxTriggerName(ntNamed)));
  Result := Result.WithField('event', NyxData(NyxNamedEvent(AEvent.Name).Name));
end;

function TNyxFlowRoute.WithField(const AName: TNyxText;
  const AValue: TNyxDataValue): TNyxFlowRoute;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LFound: Boolean;
begin
  LFound := False;
  SetLength(LFields, FData.Count);
  for LIndex := 0 to High(LFields) do
  begin
    LFields[LIndex] := NyxField(FData.Key(LIndex), FData.Field(FData.Key(LIndex)));

    if LFields[LIndex].Name = AName then
    begin
      LFields[LIndex].Value := AValue;
      LFound := True;
    end;
  end;

  if not LFound then
  begin
    SetLength(LFields, Length(LFields) + 1);
    LFields[High(LFields)] := NyxField(AName, AValue);
  end;
  Result.FData := NyxObject(LFields);
end;

function TNyxFlowRoute.GetID: TNyxFlowRouteRef;
begin
  Result := NyxFlowRouteID(FData.Field('id').AsText);
end;

function TNyxFlowRoute.GetFlow: TNyxFlowRef;
begin
  Result := NyxFlowRef(FData.Field('flow').AsText);
end;

function TNyxFlowRoute.GetTrigger: TNyxTrigger;
begin

  if not TryNyxTrigger(FData.Field('trigger').AsText, Result) then
  begin
    raise ENyxFlow.Create('Unknown route trigger');
  end;
end;

function TNyxFlowRoute.GetEvent: TNyxEventRef;
begin
  Result := NyxEvent(FData.Field('event').AsText);
end;

function TNyxFlowRoute.GetCallbackMode: TNyxFlowCallbackMode;
begin
  Result := TNyxFlowCallbackMode(FData.Field('callbacks').AsInteger);
end;

function TNyxFlowRoute.GetPolicy: TNyxExecutionPolicy;
begin
  Result := TNyxExecutionPolicy(FData.Field('policy').AsInteger);
end;

function TNyxFlowRoute.GetConcurrency: TNyxFlowConcurrency;
begin
  Result := TNyxFlowConcurrency(FData.Field('concurrency').AsInteger);
end;

function TNyxFlowRoute.GetEnabled: Boolean;
begin
  Result := FData.Field('enabled').AsBoolean;
end;

function TNyxFlowRoute.GetMaximumRuns: Integer;
begin
  Result := FData.Field('maximumRuns').AsInteger;
end;

function TNyxFlowRoute.GetInputCount: Integer;
begin
  Result := FData.Field('inputs').Count;
end;

function TNyxFlowRoute.GetOutputCount: Integer;
begin
  Result := FData.Field('outputs').Count;
end;

function TNyxFlowRoute.GetRowOutputCount: Integer;
var
  LIndex: Integer;
begin
  Result := 0;
  for LIndex := 0 to FData.Count - 1 do
  begin

    if FData.Key(LIndex) = 'rowOutputs' then
    begin
      Exit(FData.Field('rowOutputs').Count);
    end;
  end;
end;

function TNyxFlowRoute.Using(APolicy: TNyxExecutionPolicy): TNyxFlowRoute;
begin

  if (Ord(APolicy) < Ord(Low(TNyxExecutionPolicy))) or
    (Ord(APolicy) > Ord(High(TNyxExecutionPolicy))) then
  begin
    raise ENyxFlow.Create('Unknown route execution policy');
  end;
  Result := WithField('policy', NyxData(Ord(APolicy)));
end;

function TNyxFlowRoute.Concurrent(AValue: TNyxFlowConcurrency): TNyxFlowRoute;
begin

  if (Ord(AValue) < Ord(Low(TNyxFlowConcurrency))) or
    (Ord(AValue) > Ord(High(TNyxFlowConcurrency))) then
  begin
    raise ENyxFlow.Create('Unknown route concurrency policy');
  end;
  Result := WithField('concurrency', NyxData(Ord(AValue)));
end;

function TNyxFlowRoute.Enabled(AValue: Boolean): TNyxFlowRoute;
begin
  Result := WithField('enabled', NyxData(AValue));
end;

function TNyxFlowRoute.MaximumRuns(AValue: Integer): TNyxFlowRoute;
begin

  if (AValue < 1) or (AValue > 64) then
  begin
    raise ENyxFlow.Create('Route active-run capacity must be 1..64');
  end;
  Result := WithField('maximumRuns', NyxData(AValue));
end;

function TNyxFlowRoute.AddInput(const AInput: TNyxFlowRouteInput): TNyxFlowRoute;
var
  LItems: array of TNyxDataValue;
  LIndex: Integer;
  LValue: TNyxDataValue;
begin

  if InputCount >= NyxMaximumFlowRouteMappings then
  begin
    raise ENyxFlow.Create('Route input mapping capacity exceeded');
  end;
  SetLength(LItems, InputCount + 1);
  for LIndex := 0 to InputCount - 1 do
  begin

    if Input(LIndex).Target.Same(AInput.Target) then
    begin
      raise ENyxFlow.Create('Map each route input once');
    end;
    LItems[LIndex] := FData.Field('inputs').Item(LIndex);
  end;
  case AInput.Source of
    nfisConstant: LValue := AInput.ConstantData;
    nfisState: LValue := NyxData(AInput.StateName);
    nfisEventDetails: LValue := AInput.DetailsPath.ToData;
    nfisEventValue: LValue := NyxData(Ord(AInput.ValueType));
    nfisSignal: LValue := NyxNull;
  end;
  LItems[High(LItems)] := NyxObject([NyxField('target', EndpointData(AInput.Target)),
    NyxField('source', NyxData(Ord(AInput.Source))), NyxField('type', NyxData(Ord(AInput.ValueType))),
    NyxField('value', LValue)]);
  Result := WithField('inputs', NyxArray(LItems));
end;

function TNyxFlowRoute.InputStateName(const ATarget: TNyxFlowEndpoint;
  const AName: TNyxText; AKind: TNyxStateKind): TNyxFlowRoute;
var
  LInput: TNyxFlowRouteInput;
begin
  LInput := Default(TNyxFlowRouteInput);
  LInput.Target := ATarget;
  LInput.Source := nfisState;
  { The typed factory validates scalar names without interpreting punctuation. }
  LInput.StateName := NyxTextState(AName).Name;
  LInput.ValueType := NyxFlowStateType(AKind);
  Result := AddInput(LInput);
end;

function TNyxFlowRoute.InputConstant(const ATarget: TNyxFlowEndpoint;
  const AValue: TNyxFlowValue): TNyxFlowRoute;
var
  LInput: TNyxFlowRouteInput;
begin
  LInput := Default(TNyxFlowRouteInput);
  LInput.Target := ATarget;
  LInput.Source := nfisConstant;
  LInput.ValueType := AValue.ValueType;
  LInput.ConstantData := AValue.ToData;
  Result := AddInput(LInput);
end;

function TNyxFlowRoute.InputEventValue(const ATarget: TNyxFlowEndpoint;
  AType: TNyxFlowType): TNyxFlowRoute;
var
  LInput: TNyxFlowRouteInput;
begin
  NyxFlowStateKind(AType);
  LInput := Default(TNyxFlowRouteInput);
  LInput.Target := ATarget;
  LInput.Source := nfisEventValue;
  LInput.ValueType := AType;
  Result := AddInput(LInput);
end;

function TNyxFlowRoute.InputEventDetails(const ATarget: TNyxFlowEndpoint;
  const APath: TNyxResourcePath): TNyxFlowRoute;
var
  LInput: TNyxFlowRouteInput;
begin
  LInput := Default(TNyxFlowRouteInput);
  LInput.Target := ATarget;
  LInput.Source := nfisEventDetails;
  LInput.ValueType := nftJSON;
  LInput.DetailsPath := TNyxResourcePath.FromData(APath.ToData);
  Result := AddInput(LInput);
end;

function TNyxFlowRoute.InputSignal(const ATarget: TNyxFlowEndpoint): TNyxFlowRoute;
var
  LInput: TNyxFlowRouteInput;
begin
  LInput := Default(TNyxFlowRouteInput);
  LInput.Target := ATarget;
  LInput.Source := nfisSignal;
  LInput.ValueType := nftSignal;
  Result := AddInput(LInput);
end;

function TNyxFlowRoute.OutputStateName(const ASource: TNyxFlowEndpoint;
  const AName: TNyxText; AKind: TNyxStateKind): TNyxFlowRoute;
var
  LItems: array of TNyxDataValue;
  LIndex: Integer;
begin

  if OutputCount + RowOutputCount >= NyxMaximumFlowRouteMappings then
  begin
    raise ENyxFlow.Create('Route output mapping capacity exceeded');
  end;
  NyxFlowStateType(AKind);
  NyxTextState(AName);
  SetLength(LItems, OutputCount + 1);
  for LIndex := 0 to OutputCount - 1 do
  begin

    if Output(LIndex).StateName = AName then
    begin
      raise ENyxFlow.Create('Map each state destination once per route');
    end;
    LItems[LIndex] := FData.Field('outputs').Item(LIndex);
  end;
  LItems[High(LItems)] := NyxObject([NyxField('source', EndpointData(ASource)),
    NyxField('state', NyxData(AName)), NyxField('kind', NyxData(Ord(AKind)))]);
  Result := WithField('outputs', NyxArray(LItems));
end;

function TNyxFlowRoute.OutputRows(const ASource: TNyxFlowEndpoint;
  const ACollection: TNyxCollectionRef; const ARows: TNyxDataRows): TNyxFlowRoute;
var
  LItems: array of TNyxDataValue;
  LIndex: Integer;
  LKey: TNyxCollectionRef;
begin
  LKey := NyxCollection(ACollection.Name);
  ARows.Validate;
  ASource.Validate;

  if OutputCount + RowOutputCount >= NyxMaximumFlowRouteMappings then
  begin
    raise ENyxFlow.Create('Route output mapping capacity exceeded');
  end;
  SetLength(LItems, RowOutputCount + 1);
  for LIndex := 0 to RowOutputCount - 1 do
  begin

    if RowOutput(LIndex).Collection.Name = LKey.Name then
    begin
      raise ENyxFlow.Create('Map each collection destination once per route');
    end;
    LItems[LIndex] := FData.Field('rowOutputs').Item(LIndex);
  end;
  LItems[High(LItems)] := NyxObject([NyxField('source', EndpointData(ASource)),
    NyxField('collection', NyxData(LKey.Name)), NyxField('rows', ARows.ToData)]);
  Result := WithField('rowOutputs', NyxArray(LItems));
end;

function TNyxFlowRoute.WithRows(const ASource: TNyxFlowEndpoint;
  const ACollection: TNyxCollectionRef; const ARows: TNyxDataRows): TNyxFlowRoute;
var
  LNew: TNyxFlowRoute;
  LItems: array of TNyxDataValue;
  LValue: TNyxDataValue;
  LIndex: Integer;
begin
  LNew := WithoutRows(ACollection).OutputRows(ASource, ACollection, ARows);

  if LNew.RowOutputCount <> RowOutputCount then
  begin
    Exit(LNew);
  end;
  LValue := LNew.FData.Field('rowOutputs').Item(LNew.RowOutputCount - 1);
  SetLength(LItems, RowOutputCount);
  for LIndex := 0 to High(LItems) do
  begin
    LItems[LIndex] := FData.Field('rowOutputs').Item(LIndex);

    if RowOutput(LIndex).Collection.Name = ACollection.Name then
    begin
      LItems[LIndex] := LValue;
    end;
  end;
  Result := WithField('rowOutputs', NyxArray(LItems));
end;

function TNyxFlowRoute.WithoutRows(const ACollection: TNyxCollectionRef): TNyxFlowRoute;
var
  LKey: TNyxCollectionRef;
  LItems: array of TNyxDataValue;
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LCount: Integer;
begin
  LKey := NyxCollection(ACollection.Name);
  Result := Self;
  SetLength(LItems, RowOutputCount);
  LCount := 0;
  for LIndex := 0 to RowOutputCount - 1 do
  begin

    if RowOutput(LIndex).Collection.Name <> LKey.Name then
    begin
      LItems[LCount] := FData.Field('rowOutputs').Item(LIndex);
      Inc(LCount);
    end;
  end;

  if LCount = RowOutputCount then
  begin
    Exit;
  end;
  SetLength(LItems, LCount);

  if LCount > 0 then
  begin
    Exit(WithField('rowOutputs', NyxArray(LItems)));
  end;
  SetLength(LFields, FData.Count - 1);
  LCount := 0;
  for LIndex := 0 to FData.Count - 1 do
  begin

    if FData.Key(LIndex) <> 'rowOutputs' then
    begin
      LFields[LCount] := NyxField(FData.Key(LIndex), FData.Field(FData.Key(LIndex)));
      Inc(LCount);
    end;
  end;
  Result.FData := NyxObject(LFields);
end;

function TNyxFlowRoute.Input(AIndex: Integer): TNyxFlowRouteInput;
var
  LValue: TNyxDataValue;
begin
  Result := Default(TNyxFlowRouteInput);
  LValue := FData.Field('inputs').Item(AIndex);
  Result.Target := ReadEndpoint(LValue.Field('target'));
  Result.Source := TNyxFlowInputSource(LValue.Field('source').AsInteger);
  Result.ValueType := TNyxFlowType(LValue.Field('type').AsInteger);
  case Result.Source of
    nfisConstant: Result.ConstantData := LValue.Field('value');
    nfisState: Result.StateName := LValue.Field('value').AsText;
    nfisEventDetails: Result.DetailsPath := TNyxResourcePath.FromData(LValue.Field('value'));
    nfisEventValue, nfisSignal:
      begin
        { These sources have no retained state/constant/path value. }
      end;
  end;
end;

function TNyxFlowRoute.Output(AIndex: Integer): TNyxFlowRouteOutput;
var
  LValue: TNyxDataValue;
begin
  LValue := FData.Field('outputs').Item(AIndex);
  Result.Source := ReadEndpoint(LValue.Field('source'));
  Result.StateName := LValue.Field('state').AsText;
  Result.StateKind := TNyxStateKind(LValue.Field('kind').AsInteger);
end;

function TNyxFlowRoute.RowOutput(AIndex: Integer): TNyxFlowRouteRowsOutput;
var
  LValue: TNyxDataValue;
begin
  LValue := FData.Field('rowOutputs').Item(AIndex);
  Result.Source := ReadEndpoint(LValue.Field('source'));
  Result.Collection := NyxCollection(LValue.Field('collection').AsText);
  Result.Rows := TNyxDataRows.FromData(LValue.Field('rows'));
end;

procedure TNyxFlowRoute.Validate;
var
  LTrigger: TNyxTrigger;
begin
  if FData.Count = 12 then
  begin
    ValidateNyxFlowFields(FData, ['id', 'flow', 'trigger', 'event', 'callbacks',
      'policy', 'concurrency', 'enabled', 'maximumRuns', 'inputs', 'outputs', 'rowOutputs']);

    if (FData.Field('rowOutputs').Kind <> ndArray) or (RowOutputCount = 0) then
    begin
      raise ENyxFlow.Create('Saved row outputs require a nonempty typed array');
    end;
  end
  else
  begin
    { Old scalar-only descriptors retain their complete original wire bytes. }
    ValidateNyxFlowFields(FData, ['id', 'flow', 'trigger', 'event', 'callbacks',
      'policy', 'concurrency', 'enabled', 'maximumRuns', 'inputs', 'outputs']);
  end;
  GetID;
  GetFlow;
  LTrigger := Trigger;

  if LTrigger = ntNamed then
  begin
    NyxNamedEvent(EventName.Name);
  end
  else if not NyxIsRuntimeTrigger(LTrigger) or (EventName.Name <> '') then
  begin
    raise ENyxFlow.Create('Invalid saved route trigger');
  end;

  if (FData.Field('callbacks').AsInteger < Ord(Low(TNyxFlowCallbackMode))) or
    (FData.Field('callbacks').AsInteger > Ord(High(TNyxFlowCallbackMode))) then
  begin
    raise ENyxFlow.Create('Unknown saved callback mode');
  end;
  Using(Policy);
  Concurrent(Concurrency);
  MaximumRuns(RunLimit);
  GetEnabled;

  if (FData.Field('inputs').Kind <> ndArray) or (FData.Field('outputs').Kind <> ndArray) or
    (InputCount > NyxMaximumFlowRouteMappings) or
    (OutputCount + RowOutputCount > NyxMaximumFlowRouteMappings) then
  begin
    raise ENyxFlow.Create('Route mappings exceed admitted array capacities');
  end;
end;

function TNyxFlowRoute.ToData: TNyxDataValue;
begin
  Validate;
  Result := FData.Copy;
end;

class function TNyxFlowRoute.FromData(const AData: TNyxDataValue): TNyxFlowRoute;
var
  LBase: TNyxFlowRoute;
  LTrigger: TNyxTrigger;
  LInputs: TNyxDataValue;
  LOutputs: TNyxDataValue;
  LItem: TNyxDataValue;
  LInput: TNyxFlowRouteInput;
  LIndex: Integer;
  LType: Integer;
  LSource: Integer;
  LKind: Integer;
begin
  LBase.FData := AData.Copy;
  LBase.Validate;
  LTrigger := LBase.Trigger;

  if LTrigger = ntNamed then
  begin
    Result := NyxNamedFlowRoute(LBase.ID, LBase.Flow, LBase.EventName, LBase.CallbackMode);
  end
  else
  begin
    Result := NyxFlowRoute(LBase.ID, LBase.Flow, LTrigger, LBase.CallbackMode);
  end;
  Result := Result.Using(LBase.Policy).Concurrent(LBase.Concurrency)
    .Enabled(LBase.IsEnabled).MaximumRuns(LBase.RunLimit);
  LInputs := AData.Field('inputs');
  LOutputs := AData.Field('outputs');
  for LIndex := 0 to LInputs.Count - 1 do
  begin
    LItem := LInputs.Item(LIndex);
    ValidateNyxFlowFields(LItem, ['target', 'source', 'type', 'value']);
    LSource := LItem.Field('source').AsInteger;
    LType := LItem.Field('type').AsInteger;

    if (LSource < Ord(Low(TNyxFlowInputSource))) or (LSource > Ord(High(TNyxFlowInputSource))) or
      (LType < Ord(Low(TNyxFlowType))) or (LType > Ord(High(TNyxFlowType))) then
    begin
      raise ENyxFlow.Create('Unknown route input source/family');
    end;
    LInput := Default(TNyxFlowRouteInput);
    LInput.Target := ReadEndpoint(LItem.Field('target'));
    LInput.Source := TNyxFlowInputSource(LSource);
    LInput.ValueType := TNyxFlowType(LType);
    case LInput.Source of
      nfisConstant:
        begin

          if TNyxFlowValue.FromData(LItem.Field('value')).ValueType <> LInput.ValueType then
          begin
            raise ENyxFlow.Create('Route constant has a conflicting family');
          end;
          LInput.ConstantData := LItem.Field('value');
        end;
      nfisState:
        begin
          NyxFlowStateKind(LInput.ValueType);
          LInput.StateName := NyxTextState(LItem.Field('value').AsText).Name;
        end;
      nfisEventValue:
        begin
          NyxFlowStateKind(LInput.ValueType);

          if LItem.Field('value').AsInteger <> LType then
          begin
            raise ENyxFlow.Create('Event scalar family disagrees with its declaration');
          end;
        end;
      nfisEventDetails:
        begin

          if LInput.ValueType <> nftJSON then
          begin
            raise ENyxFlow.Create('Structured event data requires JSON input');
          end;
          LInput.DetailsPath := TNyxResourcePath.FromData(LItem.Field('value'));
        end;
      nfisSignal:
        begin

          if (LInput.ValueType <> nftSignal) or (LItem.Field('value').Kind <> ndNull) then
          begin
            raise ENyxFlow.Create('A signal cannot contain a fabricated scalar payload');
          end;
        end;
    end;
    Result := Result.AddInput(LInput);
  end;
  for LIndex := 0 to LOutputs.Count - 1 do
  begin
    LItem := LOutputs.Item(LIndex);
    ValidateNyxFlowFields(LItem, ['source', 'state', 'kind']);
    LKind := LItem.Field('kind').AsInteger;

    if (LKind < Ord(Low(TNyxStateKind))) or (LKind > Ord(High(TNyxStateKind))) then
    begin
      raise ENyxFlow.Create('Unknown output state family');
    end;
    Result := Result.OutputStateName(ReadEndpoint(LItem.Field('source')),
      LItem.Field('state').AsText, TNyxStateKind(LKind));
  end;
  for LIndex := 0 to LBase.RowOutputCount - 1 do
  begin
    LItem := AData.Field('rowOutputs').Item(LIndex);
    ValidateNyxFlowFields(LItem, ['source', 'collection', 'rows']);
    Result := Result.OutputRows(ReadEndpoint(LItem.Field('source')),
      NyxCollection(LItem.Field('collection').AsText), TNyxDataRows.FromData(LItem.Field('rows')));
  end;
end;

function TNyxFlowRoute.InputState(const ATarget: TNyxFlowEndpoint; const AState: TNyxTextStateRef): TNyxFlowRoute;
begin
  Result := InputStateName(ATarget, AState.Name, nskText);
end;

function TNyxFlowRoute.InputState(const ATarget: TNyxFlowEndpoint; const AState: TNyxBooleanStateRef): TNyxFlowRoute;
begin
  Result := InputStateName(ATarget, AState.Name, nskBoolean);
end;

function TNyxFlowRoute.InputState(const ATarget: TNyxFlowEndpoint; const AState: TNyxIntegerStateRef): TNyxFlowRoute;
begin
  Result := InputStateName(ATarget, AState.Name, nskInteger);
end;

function TNyxFlowRoute.InputState(const ATarget: TNyxFlowEndpoint; const AState: TNyxNumberStateRef): TNyxFlowRoute;
begin
  Result := InputStateName(ATarget, AState.Name, nskNumber);
end;

function TNyxFlowRoute.OutputState(const ASource: TNyxFlowEndpoint; const AState: TNyxTextStateRef): TNyxFlowRoute;
begin
  Result := OutputStateName(ASource, AState.Name, nskText);
end;

function TNyxFlowRoute.OutputState(const ASource: TNyxFlowEndpoint; const AState: TNyxBooleanStateRef): TNyxFlowRoute;
begin
  Result := OutputStateName(ASource, AState.Name, nskBoolean);
end;

function TNyxFlowRoute.OutputState(const ASource: TNyxFlowEndpoint; const AState: TNyxIntegerStateRef): TNyxFlowRoute;
begin
  Result := OutputStateName(ASource, AState.Name, nskInteger);
end;

function TNyxFlowRoute.OutputState(const ASource: TNyxFlowEndpoint; const AState: TNyxNumberStateRef): TNyxFlowRoute;
begin
  Result := OutputStateName(ASource, AState.Name, nskNumber);
end;

function NewNyxFlowRoutes: INyxFlowRoutes;
var
  LOwner: TRoutes;
begin
  LOwner := TRoutes.Create;
  Result := LOwner;
  LOwner.Data := NyxArray([]);
end;

function TRoutes.GetCount: Integer;
begin
  Result := Data.Count;
end;

function TRoutes.Route(AIndex: Integer): TNyxFlowRoute;
begin
  { The private set admits every entry at construction. Return its immutable
    snapshot directly; repeatedly rebuilding all fluent mappings here turns
    whole-document source validation into needless quadratic allocation. }
  Result.FData := Data.Item(AIndex);
end;

function TRoutes.WithRoute(const ARoute: TNyxFlowRoute): INyxFlowRoutes;
var
  LItems: array of TNyxDataValue;
  LIndex: Integer;
  LPosition: Integer;
  LOwner: TRoutes;
begin
  ARoute.Validate;
  SetLength(LItems, Count);
  LPosition := -1;
  for LIndex := 0 to Count - 1 do
  begin
    LItems[LIndex] := Data.Item(LIndex);

    if Route(LIndex).ID.Name = ARoute.ID.Name then
    begin
      LPosition := LIndex;
    end;
  end;

  if LPosition < 0 then
  begin

    if Count >= NyxMaximumControlFlowRoutes then
    begin
      raise ENyxFlow.Create('Control route capacity exceeded');
    end;
    LPosition := Length(LItems);
    SetLength(LItems, LPosition + 1);
  end;
  LItems[LPosition] := ARoute.ToData;
  LOwner := TRoutes.Create;
  Result := LOwner;
  LOwner.Data := NyxArray(LItems);
end;

function TRoutes.WithoutRoute(const AID: TNyxFlowRouteRef): INyxFlowRoutes;
var
  LItems: array of TNyxDataValue;
  LIndex: Integer;
  LCount: Integer;
  LOwner: TRoutes;
begin
  NyxFlowRouteID(AID.Name);
  LCount := 0;
  SetLength(LItems, Count);
  for LIndex := 0 to Count - 1 do
  begin

    if Route(LIndex).ID.Name = AID.Name then
    begin
      Continue;
    end;
    LItems[LCount] := Data.Item(LIndex);
    Inc(LCount);
  end;

  if LCount = Count then
  begin
    raise ENyxFlow.Create('The exact control route does not exist');
  end;
  SetLength(LItems, LCount);
  LOwner := TRoutes.Create;
  Result := LOwner;
  LOwner.Data := NyxArray(LItems);
end;

function TRoutes.ToData: TNyxDataValue;
begin
  Result := NyxObject([NyxField('version', NyxData(1)), NyxField('routes', Data)]);
end;

function NyxFlowRoutesFromData(const AData: TNyxDataValue): INyxFlowRoutes;
var
  LIndex: Integer;
  LRoutes: TNyxDataValue;
  LBefore: Integer;
begin
  ValidateNyxFlowFields(AData, ['version', 'routes']);
  LRoutes := AData.Field('routes');

  if (AData.Field('version').AsInteger <> 1) or (LRoutes.Kind <> ndArray) or
    (LRoutes.Count > NyxMaximumControlFlowRoutes) then
  begin
    raise ENyxFlow.Create('Unsupported or oversized control route set');
  end;
  Result := NewNyxFlowRoutes;
  for LIndex := 0 to LRoutes.Count - 1 do
  begin
    LBefore := Result.Count;
    Result := Result.WithRoute(TNyxFlowRoute.FromData(LRoutes.Item(LIndex)));

    if Result.Count <> LBefore + 1 then
    begin
      raise ENyxFlow.Create('Control route identities must be distinct');
    end;
  end;
end;

procedure ValidateNyxFlowRoute(const ARoute: TNyxFlowRoute;
  const ADefinition: INyxFlowDefinition; AState: TNyxState;
  const ACollections: INyxCollectionDefaults);
var
  LIndex: Integer;
  LPart: Integer;
  LInput: TNyxFlowRouteInput;
  LOutput: TNyxFlowRouteOutput;
  LRowsOutput: TNyxFlowRouteRowsOutput;
  LDescriptor: TNyxLogicDescriptor;
  LPort: TNyxFlowPort;
  LEndpoint: TNyxFlowEndpoint;
  LWires: TNyxDataValue;
  LWired: Boolean;
  LMapped: Boolean;
  LWire: Integer;

  function Port(const AEndpoint: TNyxFlowEndpoint): TNyxFlowPort;
  var
    LStep: Integer;
    LPart: Integer;
    LSchema: TNyxLogicDescriptor;
  begin
    for LStep := 0 to ADefinition.StepCount - 1 do
    begin

      if ADefinition.Step(LStep).Name = AEndpoint.Step.Name then
      begin
        LSchema := ADefinition.Descriptor(LStep);
        for LPart := 0 to LSchema.PortCount - 1 do
        begin

          if LSchema.Port(LPart).Reference.Name = AEndpoint.Port.Name then
          begin
            Exit(LSchema.Port(LPart));
          end;
        end;
      end;
    end;
    raise ENyxFlow.Create('Route references a missing graph step/port');
  end;

begin
  ARoute.Validate;

  if (ADefinition = nil) or (AState = nil) then
  begin
    raise ENyxFlow.Create('Route validation requires graph declarations and authored state');
  end;
  LWires := ADefinition.ToData.Field('wires');
  for LIndex := 0 to ARoute.InputCount - 1 do
  begin
    LInput := ARoute.Input(LIndex);
    LPort := Port(LInput.Target);

    if (LPort.Direction <> nfdInput) or (LPort.ValueType <> LInput.ValueType) then
    begin
      raise ENyxFlow.Create('Route input direction/family disagrees with its graph port');
    end;
    for LWire := 0 to LWires.Count - 1 do
    begin

      if ReadEndpoint(LWires.Item(LWire).Field('target')).Same(LInput.Target) then
      begin
        raise ENyxFlow.Create('A wired graph input cannot also receive route input');
      end;
    end;

    if (LInput.Source = nfisState) and
      (NyxFlowStateType(AState.Value(LInput.StateName).Kind) <> LInput.ValueType) then
    begin
      raise ENyxFlow.Create('Route state input family disagrees with its declaration');
    end;
  end;
  for LIndex := 0 to ADefinition.StepCount - 1 do
  begin
    LDescriptor := ADefinition.Descriptor(LIndex);
    for LPart := 0 to LDescriptor.PortCount - 1 do
    begin
      LPort := LDescriptor.Port(LPart);

      if LPort.Direction <> nfdInput then
      begin
        Continue;
      end;
      LEndpoint := NyxFlowEndpoint(ADefinition.Step(LIndex), LPort.Reference);
      LWired := False;
      LMapped := False;
      for LWire := 0 to LWires.Count - 1 do
      begin
        LWired := LWired or ReadEndpoint(LWires.Item(LWire).Field('target')).Same(LEndpoint);
      end;
      for LWire := 0 to ARoute.InputCount - 1 do
      begin
        LMapped := LMapped or ARoute.Input(LWire).Target.Same(LEndpoint);
      end;

      if not LWired and not LMapped then
      begin
        raise ENyxFlow.Create('Route must supply every unwired graph input');
      end;
    end;
  end;
  for LIndex := 0 to ARoute.OutputCount - 1 do
  begin
    LOutput := ARoute.Output(LIndex);
    LPort := Port(LOutput.Source);

    if (LPort.Direction <> nfdOutput) or
      (LPort.ValueType <> NyxFlowStateType(LOutput.StateKind)) or
      (AState.Value(LOutput.StateName).Kind <> LOutput.StateKind) then
    begin
      raise ENyxFlow.Create('Route state output direction/family disagrees with its declaration');
    end;
  end;
  for LIndex := 0 to ARoute.RowOutputCount - 1 do
  begin
    LRowsOutput := ARoute.RowOutput(LIndex);
    LPort := Port(LRowsOutput.Source);

    if (LPort.Direction <> nfdOutput) or (LPort.ValueType <> nftJSON) then
    begin
      raise ENyxFlow.Create('Route rows require a JSON output port');
    end;

    if (ACollections = nil) or not ACollections.Has(LRowsOutput.Collection) then
    begin
      raise ENyxFlow.Create('Route rows require an existing application collection');
    end;
    LRowsOutput.Rows.MatchSchema(ACollections.Snapshot(LRowsOutput.Collection).Schema);
  end;
end;

end.
