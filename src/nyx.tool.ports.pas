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


unit nyx.tool.ports;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.data, nyx.flow.types,
  nyx.canvas.types, nyx.binding.types;

type
  { Signals and their optional value/details are separate outputs. Bound state
    inputs describe real existing direct projections; expression/resource views
    do not fabricate writable state or silently convert payload families. }
  TNyxToolPortRole = (ntprEventSignal, ntprEventValue, ntprEventDetails, ntprBoundState);

  { A port address includes its scene item and exact effective control identity.
    Reusing a template in two items does not alias their event routers. These
    copied values retain no tree, context, state, callback or executable creator. }
  TNyxToolControlPort = record
    Item: TNyxCanvasItemRef;
    Control: TNyxControlRef;
    Role: TNyxToolPortRole;
    Direction: TNyxFlowDirection;
    ValueType: TNyxFlowType;
    Trigger: TNyxTrigger;
    EventName: TNyxEventRef;
    PropertyTarget: TNyxBindingProperty;
    StateName: TNyxText;
    PayloadOptional: Boolean;
    Title: TNyxText;
    Description: TNyxText;
    function ToData: TNyxDataValue;
  end;

  { Immutable exact-control discovery. UI/agents page the snapshot instead of
    acquiring the entire document. Returned records are detached on pas2js too.
    Only declared scalar values/details are advertised; physical keyboard,
    pointer/editing snapshots require their own explicit future projection. }
  INyxToolControlPorts = interface(IInterface)
    ['{D1411014-6120-4C84-A708-000000000018}']
    function GetCount: Integer;
    function Port(AIndex: Integer): TNyxToolControlPort;
    function Matches(AIndex: Integer; const AQuery: TNyxText): Boolean;
    property Count: Integer read GetCount;
  end;

function NyxToolControlPorts(const AItem: TNyxCanvasItemRef; AControl: TNyxNode;
  ADocument: TNyxDocument): INyxToolControlPorts;

implementation

uses nyx.schema, nyx.contract, nyx.event.payload, nyx.flow.routes, nyx.logic.catalog;

type
  TPorts = class(TInterfacedObject, INyxToolControlPorts)
  public
    Values: array of TNyxToolControlPort;
    procedure Add(const APort: TNyxToolControlPort);
    function GetCount: Integer;
    function Port(AIndex: Integer): TNyxToolControlPort;
    function Matches(AIndex: Integer; const AQuery: TNyxText): Boolean;
  end;

function TNyxToolControlPort.ToData: TNyxDataValue;
begin
  Result := NyxObject([NyxField('item', NyxData(Item.Name)),
    NyxField('control', NyxData(Control.ID)), NyxField('role', NyxData(Ord(Role))),
    NyxField('direction', NyxData(Ord(Direction))), NyxField('type', NyxData(Ord(ValueType))),
    NyxField('trigger', NyxData(NyxTriggerName(Trigger))),
    NyxField('event', NyxData(EventName.Name)),
    NyxField('property', NyxData(NyxBindingPropertyName(PropertyTarget))),
    NyxField('state', NyxData(StateName)), NyxField('optional', NyxData(PayloadOptional)),
    NyxField('title', NyxData(Title)), NyxField('description', NyxData(Description))]);
end;

procedure TPorts.Add(const APort: TNyxToolControlPort);
var
  LCount: Integer;
begin
  LCount := Length(Values);
  SetLength(Values, LCount + 1);
  Values[LCount] := APort;
end;

function TPorts.GetCount: Integer;
begin
  Result := Length(Values);
end;

function TPorts.Port(AIndex: Integer): TNyxToolControlPort;
begin

  if (AIndex < 0) or (AIndex >= Length(Values)) then
  begin
    raise ENyxModel.Create('Tool control port index is out of bounds');
  end;
  { Explicit field copies prevent a returned mutable pas2js record from changing
    the catalog's earlier snapshot. No array Copy/record alias is relied upon. }
  Result.Item := Values[AIndex].Item;
  Result.Control := Values[AIndex].Control;
  Result.Role := Values[AIndex].Role;
  Result.Direction := Values[AIndex].Direction;
  Result.ValueType := Values[AIndex].ValueType;
  Result.Trigger := Values[AIndex].Trigger;
  Result.EventName := Values[AIndex].EventName;
  Result.PropertyTarget := Values[AIndex].PropertyTarget;
  Result.StateName := Values[AIndex].StateName;
  Result.PayloadOptional := Values[AIndex].PayloadOptional;
  Result.Title := Values[AIndex].Title;
  Result.Description := Values[AIndex].Description;
end;

function TPorts.Matches(AIndex: Integer; const AQuery: TNyxText): Boolean;
var
  LPort: TNyxToolControlPort;
  LText: TNyxText;
  LQuery: TNyxText;
begin
  LPort := Port(AIndex);
  LText := LowerCase(LPort.Title + ' ' + LPort.Description + ' ' +
    NyxFlowTypeName(LPort.ValueType) + ' ' + LPort.StateName);
  LQuery := LowerCase(Trim(AQuery));
  Result := (LQuery = '') or (Pos(LQuery, LText) > 0);
end;

function NyxToolControlPorts(const AItem: TNyxCanvasItemRef; AControl: TNyxNode;
  ADocument: TNyxDocument): INyxToolControlPorts;
var
  LOwner: TPorts;
  LMetadata: TNyxEventSchemas;
  LIndex: Integer;
  LPort: TNyxToolControlPort;
  LValueNode: TNyxNode;
  LDomain: TNyxValueDomain;
  LSpec: TNyxBindingSpec;
begin

  if not AItem.Defined or (AControl = nil) or (ADocument = nil) then
  begin
    raise ENyxModel.Create('Tool port discovery requires an exact item, control and context');
  end;
  LOwner := TPorts.Create;
  Result := LOwner;
  LMetadata := NyxEventsMetadata(AControl, ADocument);
  for LIndex := 0 to High(LMetadata) do
  begin
    LPort := Default(TNyxToolControlPort);
    LPort.Item := AItem;
    LPort.Control := NyxControl(AControl.ID);
    LPort.Role := ntprEventSignal;
    LPort.Direction := nfdOutput;
    LPort.ValueType := nftSignal;
    LPort.Trigger := LMetadata[LIndex].Trigger;
    LPort.EventName := LMetadata[LIndex].Name;
    LPort.Title := NyxTriggerTitle(LPort.Trigger);

    if LPort.Trigger = ntNamed then
    begin
      LPort.Title := LPort.EventName.Name;
    end;
    LPort.Description := 'Invocation signal. ' + LMetadata[LIndex].Description;
    LOwner.Add(LPort);
    LDomain := NyxNoDomain;

    if LMetadata[LIndex].Payload.Defined then
    begin

      if LMetadata[LIndex].Payload.Kind = nepScalar then
      begin
        LDomain := LMetadata[LIndex].Payload.Domain;
      end
      else if LMetadata[LIndex].Payload.Kind = nepData then
      begin
        LPort.Role := ntprEventDetails;
        LPort.ValueType := nftJSON;
        LPort.Title := LPort.Title + ' details';
        LPort.PayloadOptional := LMetadata[LIndex].PayloadOptional;
        LPort.Description := 'Owned structured event data; choose explicit Field/Item projections.';
        LOwner.Add(LPort);
      end;
    end
    else if LPort.Trigger <> ntNamed then
    begin
      ResolveNyxEventValueContract(AControl, AControl, AControl, LPort.Trigger,
        LValueNode, LDomain);
    end;

    if LDomain.Defined then
    begin
      LPort.Role := ntprEventValue;
      LPort.ValueType := NyxFlowStateType(LDomain.Kind);
      LPort.Title := LPort.Title + ' value';
      LPort.PayloadOptional := True;
      LPort.Description := 'Accepted scalar snapshot. Missing/mistyped invocation values refuse before graph work.';
      LOwner.Add(LPort);
    end;
  end;
  for LIndex := 0 to AControl.BindingCount - 1 do
  begin
    LSpec := AControl.Bindings[LIndex];

    if LSpec.Cleared or (LSpec.Source <> bsState) then
    begin
      Continue;
    end;
    LPort := Default(TNyxToolControlPort);
    LPort.Item := AItem;
    LPort.Control := NyxControl(AControl.ID);
    LPort.Role := ntprBoundState;
    LPort.Direction := nfdInput;
    LPort.ValueType := NyxFlowStateType(LSpec.ValueKind);
    LPort.PropertyTarget := LSpec.Target;
    LPort.StateName := LSpec.StateName;
    LPort.Title := NyxBindingPropertyTitle(LSpec.Target);
    LPort.Description := 'Write existing state ' + LSpec.StateName +
      ' to update this direct projection through ordinary atomic bindings.';
    LOwner.Add(LPort);
  end;
end;

end.
