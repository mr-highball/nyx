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
unit nyx.flow.editing;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.flow.types, nyx.flow, nyx.flow.declarations,
  nyx.logic;

type
  { Closed semantic edit choices; conditions change graph dependencies without
    granting runtime host authority. Existing ordinal values remain stable. }
  TNyxFlowChangeKind = (nfcDefine, nfcRemove, nfcAddStep, nfcReplaceStep,
    nfcRemoveStep, nfcConnect, nfcDisconnect, nfcSetCondition, nfcClearCondition);
  { Immutable portable intent, containing schema/configuration rather than an
    executable operation. Expect captures an exact original declaration (or
    absence); all guards in a group compare the original registry, never an
    intermediate. Default records refuse. No document/view/host is retained. }
  TNyxFlowChange = record
  private
    FDefined: Boolean;
    FKind: TNyxFlowChangeKind;
    FReference: TNyxFlowRef;
    FData: TNyxDataValue;
    FExpected: TNyxDataValue;
  public
    function Expect(const ABaseline: TNyxDataValue): TNyxFlowChange;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxFlowChange; static;
    property Defined: Boolean read FDefined;
    property Kind: TNyxFlowChangeKind read FKind;
    property Reference: TNyxFlowRef read FReference;
  end;

{ Construct typed edits. Replacing a step retains its declaration position and
  exact ID; removing a connected step refuses unless detach was explicitly chosen.
  Detachment includes data wires and Boolean condition dependencies.
  Connect never silently replaces an occupied input. Disconnect requires that
  exact wire. Whole grouped admission may repair an intermediate graph. }
function NyxDefineFlow(const AReference: TNyxFlowRef;
  const ADefinition: INyxFlowDefinition): TNyxFlowChange;
function NyxRemoveFlow(const AReference: TNyxFlowRef): TNyxFlowChange;
function NyxAddFlowStep(const AReference: TNyxFlowRef; const AStep: TNyxFlowStepRef;
  const AOperation: INyxLogicOperation): TNyxFlowChange;
function NyxReplaceFlowStep(const AReference: TNyxFlowRef; const AStep: TNyxFlowStepRef;
  const AOperation: INyxLogicOperation): TNyxFlowChange;
function NyxRemoveFlowStep(const AReference: TNyxFlowRef; const AStep: TNyxFlowStepRef;
  ADetachWires: Boolean = False): TNyxFlowChange;
function NyxConnectFlow(const AReference: TNyxFlowRef;
  const ASource, ATarget: TNyxFlowEndpoint): TNyxFlowChange;
function NyxDisconnectFlow(const AReference: TNyxFlowRef;
  const ASource, ATarget: TNyxFlowEndpoint): TNyxFlowChange;
{ Set replaces one exact step condition; clear is idempotent. Complete grouped
  admission checks Boolean output identity and all data/condition cycles. }
function NyxSetFlowCondition(const AReference: TNyxFlowRef; const AStep: TNyxFlowStepRef;
  const ACondition: TNyxFlowEndpoint; AExpected: Boolean = True): TNyxFlowChange;
function NyxClearFlowCondition(const AReference: TNyxFlowRef;
  const AStep: TNyxFlowStepRef): TNyxFlowChange;
{ One exact entry, never the entire document. Absence is an explicit baseline. }
function NyxFlowBaseline(const ARegistry: INyxFlowDeclarations;
  const AReference: TNyxFlowRef): TNyxDataValue;
{ Returns independently owned membership. All changes stage before complete graph
  admission: cycles/types/schema conflicts may be repaired within one group.
  Any failure leaves ARegistry byte-for-byte unchanged; creators never execute. }
function EditNyxFlows(const ARegistry: INyxFlowDeclarations;
  const AChanges: array of TNyxFlowChange): INyxFlowDeclarations;

implementation

const
  CNames: array[TNyxFlowChangeKind] of TNyxText =
    ('flow-define', 'flow-remove', 'flow-step-add', 'flow-step-set',
     'flow-step-remove', 'flow-connect', 'flow-disconnect',
     'flow-condition-set', 'flow-condition-clear');

function NyxRemoveFlow(const AReference: TNyxFlowRef): TNyxFlowChange;
begin
  Result := Default(TNyxFlowChange);
  Result.FDefined := True;
  Result.FKind := nfcRemove;
  Result.FReference := NyxFlowRef(AReference.Name);
  Result.FData := NyxObject([]);
end;

function NyxDefineFlow(const AReference: TNyxFlowRef;
  const ADefinition: INyxFlowDefinition): TNyxFlowChange;
var
  LDefinition: INyxFlowDefinition;
begin

  if ADefinition = nil then
  begin
    raise ENyxFlow.Create('A saved graph definition is required');
  end;
  Result := NyxRemoveFlow(AReference);
  LDefinition := ADefinition;
  Result.FKind := nfcDefine;
  Result.FData := NyxObject([NyxField('definition',
    NyxFlowDefinitionFromData(LDefinition.ToData).ToData)]);
end;

function NyxAddFlowStep(const AReference: TNyxFlowRef; const AStep: TNyxFlowStepRef;
  const AOperation: INyxLogicOperation): TNyxFlowChange;
var
  LDescriptor: TNyxLogicDescriptor;
  LConfiguration: TNyxDataValue;
  LOperation: INyxLogicOperation;
  LStep: TNyxFlowStepRef;
begin

  if AOperation = nil then
  begin
    raise ENyxFlow.Create('A typed Logic operation is required');
  end;
  Result := NyxRemoveFlow(AReference);
  LStep := NyxFlowStep(AStep.Name);
  { Retain the input before creator getters: a const interface can alias a caller
    variable which a getter clears. Capture both values before releasing the lease. }
  LOperation := AOperation;
  LDescriptor := TNyxLogicDescriptor.FromData(LOperation.Descriptor.ToData);
  LConfiguration := LOperation.Configuration.Copy;
  Result.FKind := nfcAddStep;
  Result.FData := NyxObject([NyxField('step', NyxData(LStep.Name)),
    NyxField('descriptor', LDescriptor.ToData),
    NyxField('configuration', LConfiguration)]);
end;

function NyxReplaceFlowStep(const AReference: TNyxFlowRef; const AStep: TNyxFlowStepRef;
  const AOperation: INyxLogicOperation): TNyxFlowChange;
begin
  Result := NyxAddFlowStep(AReference, AStep, AOperation);
  Result.FKind := nfcReplaceStep;
end;

function NyxRemoveFlowStep(const AReference: TNyxFlowRef; const AStep: TNyxFlowStepRef;
  ADetachWires: Boolean): TNyxFlowChange;
begin
  Result := NyxRemoveFlow(AReference);
  Result.FKind := nfcRemoveStep;
  Result.FData := NyxObject([NyxField('step', NyxData(NyxFlowStep(AStep.Name).Name)),
    NyxField('detachWires', NyxData(ADetachWires))]);
end;

function EndpointData(const AEndpoint: TNyxFlowEndpoint): TNyxDataValue;
begin
  AEndpoint.Validate;
  Result := NyxObject([NyxField('step', NyxData(AEndpoint.Step.Name)),
    NyxField('port', NyxData(AEndpoint.Port.Name))]);
end;

function ReadEndpoint(const AData: TNyxDataValue): TNyxFlowEndpoint;
begin
  ValidateNyxFlowFields(AData, ['step', 'port']);
  Result := NyxFlowEndpoint(NyxFlowStep(AData.Field('step').AsText),
    NyxFlowPort(AData.Field('port').AsText));
end;

function NyxConnectFlow(const AReference: TNyxFlowRef;
  const ASource, ATarget: TNyxFlowEndpoint): TNyxFlowChange;
begin
  Result := NyxRemoveFlow(AReference);
  Result.FKind := nfcConnect;
  Result.FData := NyxObject([NyxField('source', EndpointData(ASource)),
    NyxField('target', EndpointData(ATarget))]);
end;

function NyxDisconnectFlow(const AReference: TNyxFlowRef;
  const ASource, ATarget: TNyxFlowEndpoint): TNyxFlowChange;
begin
  Result := NyxConnectFlow(AReference, ASource, ATarget);
  Result.FKind := nfcDisconnect;
end;

function NyxClearFlowCondition(const AReference: TNyxFlowRef;
  const AStep: TNyxFlowStepRef): TNyxFlowChange;
begin
  Result := NyxRemoveFlow(AReference);
  Result.FKind := nfcClearCondition;
  Result.FData := NyxObject([NyxField('step', NyxData(NyxFlowStep(AStep.Name).Name))]);
end;

function NyxSetFlowCondition(const AReference: TNyxFlowRef; const AStep: TNyxFlowStepRef;
  const ACondition: TNyxFlowEndpoint; AExpected: Boolean): TNyxFlowChange;
begin
  ACondition.Validate;
  Result := NyxClearFlowCondition(AReference, AStep);
  Result.FKind := nfcSetCondition;
  Result.FData := NyxObject([NyxField('step', NyxData(AStep.Name)),
    NyxField('source', EndpointData(ACondition)), NyxField('equals', NyxData(AExpected))]);
end;

function NyxFlowBaseline(const ARegistry: INyxFlowDeclarations;
  const AReference: TNyxFlowRef): TNyxDataValue;
var
  LData: TNyxDataValue;
  LPresent: Boolean;
begin

  if ARegistry = nil then
  begin
    raise ENyxFlow.Create('An owned graph registry is required');
  end;
  LPresent := ARegistry.Contains(AReference);
  LData := NyxNull;

  if LPresent then
  begin
    LData := ARegistry.Definition(AReference).ToData;
  end;
  Result := NyxObject([NyxField('exists', NyxData(LPresent)), NyxField('definition', LData)]);
end;

function TNyxFlowChange.Expect(const ABaseline: TNyxDataValue): TNyxFlowChange;
begin
  ValidateNyxFlowFields(ABaseline, ['exists', 'definition']);

  if ABaseline.Field('exists').AsBoolean then
  begin
    NyxFlowDefinitionFromData(ABaseline.Field('definition'));
  end
  else if ABaseline.Field('definition').Kind <> ndNull then
  begin
    raise ENyxFlow.Create('An absent graph baseline must contain null');
  end;
  Result := Self;
  Result.FExpected := ABaseline.Copy;
end;

function TNyxFlowChange.ToData: TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin

  if not FDefined then
  begin
    raise ENyxFlow.Create('A default graph change is absent');
  end;
  SetLength(LFields, FData.Count + 2);
  LFields[0] := NyxField('op', NyxData(CNames[FKind]));
  LFields[1] := NyxField('flow', NyxData(FReference.Name));
  for LIndex := 0 to FData.Count - 1 do
  begin
    LFields[LIndex + 2] := NyxField(FData.Key(LIndex), FData.Field(FData.Key(LIndex)));
  end;

  if FExpected.Defined then
  begin
    SetLength(LFields, Length(LFields) + 1);
    LFields[High(LFields)] := NyxField('expected', FExpected);
  end;
  Result := NyxObject(LFields);
end;

class function TNyxFlowChange.FromData(const AData: TNyxDataValue): TNyxFlowChange;
var
  LName: TNyxText;
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LCount: Integer;
  LExpected: TNyxDataValue;
  LPayload: TNyxDataValue;
  LKind: TNyxFlowChangeKind;
  LFound: Boolean;
begin
  LExpected := Default(TNyxDataValue);
  SetLength(LFields, AData.Count);
  LCount := 0;
  for LIndex := 0 to AData.Count - 1 do
  begin
    LName := AData.Key(LIndex);

    if LName = 'expected' then
    begin
      LExpected := AData.Field(LName);
    end
    else
    begin
      LFields[LCount] := NyxField(LName, AData.Field(LName));
      Inc(LCount);
    end;
  end;
  SetLength(LFields, LCount);
  LPayload := NyxObject(LFields);
  LName := LPayload.Field('op').AsText;
  Result := NyxRemoveFlow(NyxFlowRef(LPayload.Field('flow').AsText));
  LFound := False;
  for LKind := Low(TNyxFlowChangeKind) to High(TNyxFlowChangeKind) do
  begin

    if LName = CNames[LKind] then
    begin
      Result.FKind := LKind;
      LFound := True;
      Break;
    end;
  end;

  if not LFound then
  begin
    raise ENyxFlow.Create('Unknown typed graph operation');
  end;
  case Result.Kind of
    nfcSetCondition:
      begin
        ValidateNyxFlowFields(LPayload, ['op', 'flow', 'step', 'source', 'equals']);
        Result := NyxSetFlowCondition(Result.Reference, NyxFlowStep(LPayload.Field('step').AsText),
          ReadEndpoint(LPayload.Field('source')), LPayload.Field('equals').AsBoolean);
      end;
    nfcClearCondition:
      begin
        ValidateNyxFlowFields(LPayload, ['op', 'flow', 'step']);
        Result := NyxClearFlowCondition(Result.Reference, NyxFlowStep(LPayload.Field('step').AsText));
      end;
    nfcDefine:
      begin
        ValidateNyxFlowFields(LPayload, ['op', 'flow', 'definition']);
        Result := NyxDefineFlow(Result.Reference,
          NyxFlowDefinitionFromData(LPayload.Field('definition')));
      end;
    nfcRemove:
      begin
        ValidateNyxFlowFields(LPayload, ['op', 'flow']);
      end;
    nfcAddStep, nfcReplaceStep:
      begin
        LFound := False;
        for LIndex := 0 to LPayload.Count - 1 do
        begin
          LFound := LFound or (LPayload.Key(LIndex) = 'kind');
        end;

        if LFound then
        begin
          { Closed built-ins need no repeated schema in a semantic edit. Unknown
            creators must supply an explicit declaration, never host code. }
          ValidateNyxFlowFields(LPayload, ['op', 'flow', 'step', 'kind', 'configuration']);
          Result := NyxAddFlowStep(Result.Reference, NyxFlowStep(LPayload.Field('step').AsText),
            NyxLogicFromData(NyxLogicKind(LPayload.Field('kind').AsText),
              LPayload.Field('configuration')));
        end
        else
        begin
          ValidateNyxFlowFields(LPayload, ['op', 'flow', 'step', 'descriptor', 'configuration']);
          Result := NyxAddFlowStep(Result.Reference, NyxFlowStep(LPayload.Field('step').AsText),
            NewNyxLogicDeclaration(TNyxLogicDescriptor.FromData(LPayload.Field('descriptor')),
              LPayload.Field('configuration')));
        end;

        if LName = 'flow-step-set' then
        begin
          Result.FKind := nfcReplaceStep;
        end;
      end;
    nfcRemoveStep:
      begin
        ValidateNyxFlowFields(LPayload, ['op', 'flow', 'step', 'detachWires']);
        Result := NyxRemoveFlowStep(Result.Reference, NyxFlowStep(LPayload.Field('step').AsText),
          LPayload.Field('detachWires').AsBoolean);
      end;
    nfcConnect, nfcDisconnect:
      begin
        ValidateNyxFlowFields(LPayload, ['op', 'flow', 'source', 'target']);
        Result := NyxConnectFlow(Result.Reference, ReadEndpoint(LPayload.Field('source')),
          ReadEndpoint(LPayload.Field('target')));

        if LName = 'flow-disconnect' then
        begin
          Result.FKind := nfcDisconnect;
        end;
      end;
  end;

  if LExpected.Defined then
  begin
    Result := Result.Expect(LExpected);
  end;
end;

function ReplaceField(const AObject: TNyxDataValue; const AName: TNyxText;
  const AValue: TNyxDataValue): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin
  SetLength(LFields, AObject.Count);
  for LIndex := 0 to High(LFields) do
  begin
    LFields[LIndex] := NyxField(AObject.Key(LIndex), AObject.Field(AObject.Key(LIndex)));

    if LFields[LIndex].Name = AName then
    begin
      LFields[LIndex].Value := AValue;
    end;
  end;
  Result := NyxObject(LFields);
end;

{ Canonical graph encoding. Optional inputs require version three; otherwise
  conditions require version two, and removing both restores legacy bytes.
  Staged descriptors already have closed port fields. No operation is resolved. }
function WithConditions(const AData, AConditions: TNyxDataValue): TNyxDataValue;
var
  LVersion: Integer;
  LSteps: TNyxDataValue;
  LPorts: TNyxDataValue;
  LIndex: Integer;
  LPart: Integer;
begin
  LVersion := 2;
  LSteps := AData.Field('steps');
  for LIndex := 0 to LSteps.Count - 1 do
  begin
    LPorts := LSteps.Item(LIndex).Field('descriptor').Field('ports');
    for LPart := 0 to LPorts.Count - 1 do
    begin

      if LPorts.Item(LPart).Count = 5 then
      begin
        LVersion := 3;
      end;
    end;
  end;

  if (AConditions.Count = 0) and (LVersion <> 3) then
  begin
    Exit(NyxObject([NyxField('version', NyxData(1)), NyxField('limits', AData.Field('limits')),
      NyxField('steps', AData.Field('steps')), NyxField('wires', AData.Field('wires'))]));
  end;
  Result := NyxObject([NyxField('version', NyxData(LVersion)), NyxField('limits', AData.Field('limits')),
    NyxField('steps', AData.Field('steps')), NyxField('wires', AData.Field('wires')),
    NyxField('conditions', AConditions)]);
end;

{ Only private temporary data is allowed to have an incomplete graph. No raw
  intermediate is published to a registry, document, operation or caller. }
function ChangeData(const AData: TNyxDataValue; const AChange: TNyxFlowChange): TNyxDataValue;
var
  LSteps: TNyxDataValue;
  LWires: TNyxDataValue;
  LItems: array of TNyxDataValue;
  LIndex: Integer;
  LCount: Integer;
  LFound: Boolean;
  LMatches: Boolean;
  LStep: TNyxText;
  LWire: TNyxDataValue;
  LDescriptor: TNyxLogicDescriptor;
  LConditions: TNyxDataValue;
  LConditionEntry: TNyxDataValue;
begin
  Result := AData;
  LSteps := AData.Field('steps');
  LWires := AData.Field('wires');
  LConditions := NyxArray([]);

  if AData.Field('version').AsInteger in [2, 3] then
  begin
    LConditions := AData.Field('conditions');
  end;

  if AChange.Kind in [nfcSetCondition, nfcClearCondition] then
  begin
    LStep := AChange.FData.Field('step').AsText;
    LConditionEntry := NyxNull;

    if AChange.Kind = nfcSetCondition then
    begin
      { The transaction's expected field belongs to its exact baseline guard.
        Keep the Boolean comparison in a separate closed equals field. }
      LConditionEntry := NyxObject([NyxField('step', NyxData(LStep)),
        NyxField('source', AChange.FData.Field('source')),
        NyxField('expected', AChange.FData.Field('equals'))]);
    end;
    LFound := False;
    for LIndex := 0 to LSteps.Count - 1 do
    begin
      LFound := LFound or (LSteps.Item(LIndex).Field('id').AsText = LStep);
    end;

    if not LFound then
    begin
      raise ENyxFlow.Create('A condition edit requires an existing graph step');
    end;
    SetLength(LItems, LConditions.Count + 1);
    LCount := 0;
    LFound := False;
    for LIndex := 0 to LConditions.Count - 1 do
    begin

      if LConditions.Item(LIndex).Field('step').AsText = LStep then
      begin
        LFound := True;

        if AChange.Kind = nfcClearCondition then
        begin
          Continue;
        end;
        LItems[LCount] := LConditionEntry;
      end
      else
      begin
        LItems[LCount] := LConditions.Item(LIndex);
      end;
      Inc(LCount);
    end;

    if not LFound and (AChange.Kind = nfcSetCondition) then
    begin
      LItems[LCount] := LConditionEntry;
      Inc(LCount);
    end;
    SetLength(LItems, LCount);
    Exit(WithConditions(Result, NyxArray(LItems)));
  end;

  if AChange.Kind in [nfcAddStep, nfcReplaceStep, nfcRemoveStep] then
  begin
    LStep := AChange.FData.Field('step').AsText;
    LFound := False;
    SetLength(LItems, LSteps.Count + 1);
    LCount := 0;
    for LIndex := 0 to LSteps.Count - 1 do
    begin
      LMatches := LSteps.Item(LIndex).Field('id').AsText = LStep;

      if LMatches then
      begin
        LFound := True;
      end;

      if LMatches and (AChange.Kind in [nfcReplaceStep, nfcRemoveStep]) then
      begin

        if AChange.Kind = nfcRemoveStep then
        begin
          Continue;
        end;
        LDescriptor := TNyxLogicDescriptor.FromData(AChange.FData.Field('descriptor'));
        LItems[LCount] := NyxObject([NyxField('id', NyxData(LStep)),
          NyxField('kind', NyxData(LDescriptor.Kind.Name)),
          NyxField('descriptor', LDescriptor.ToData),
          NyxField('configuration', AChange.FData.Field('configuration'))]);
      end
      else
      begin
        LItems[LCount] := LSteps.Item(LIndex);
      end;
      Inc(LCount);
    end;

    if (LFound and (AChange.Kind = nfcAddStep)) or
      (not LFound and (AChange.Kind <> nfcAddStep)) then
    begin
      raise ENyxFlow.Create('Graph step identity is occupied or missing');
    end;

    if AChange.Kind = nfcAddStep then
    begin
      LDescriptor := TNyxLogicDescriptor.FromData(AChange.FData.Field('descriptor'));
      LItems[LCount] := NyxObject([NyxField('id', NyxData(LStep)),
        NyxField('kind', NyxData(LDescriptor.Kind.Name)),
        NyxField('descriptor', LDescriptor.ToData),
        NyxField('configuration', AChange.FData.Field('configuration'))]);
      Inc(LCount);
    end;
    SetLength(LItems, LCount);
    Result := ReplaceField(Result, 'steps', NyxArray(LItems));

    if AChange.Kind <> nfcRemoveStep then
    begin
      Result := WithConditions(Result, LConditions);
      Exit;
    end;
    SetLength(LItems, LWires.Count);
    LCount := 0;
    for LIndex := 0 to LWires.Count - 1 do
    begin
      LWire := LWires.Item(LIndex);
      LMatches := (LWire.Field('source').Field('step').AsText = LStep) or
        (LWire.Field('target').Field('step').AsText = LStep);

      if LMatches then
      begin

        if not AChange.FData.Field('detachWires').AsBoolean then
        begin
          raise ENyxFlow.Create('Remove the step connections first or explicitly detach them');
        end;
        Continue;
      end;
      LItems[LCount] := LWire;
      Inc(LCount);
    end;
  end
  else
  begin
    SetLength(LItems, LWires.Count + 1);
    LCount := 0;
    LFound := False;
    for LIndex := 0 to LWires.Count - 1 do
    begin
      LWire := LWires.Item(LIndex);
      LMatches := (LWire.Field('source').ToJSON = AChange.FData.Field('source').ToJSON) and
        (LWire.Field('target').ToJSON = AChange.FData.Field('target').ToJSON);

      if LMatches then
      begin
        LFound := True;

        if AChange.Kind = nfcDisconnect then
        begin
          Continue;
        end;
      end;
      LItems[LCount] := LWire;
      Inc(LCount);
    end;

    if (LFound and (AChange.Kind = nfcConnect)) or
      (not LFound and (AChange.Kind = nfcDisconnect)) then
    begin
      raise ENyxFlow.Create('Graph connection is already present or missing');
    end;

    if AChange.Kind = nfcConnect then
    begin
      LItems[LCount] := AChange.FData;
      Inc(LCount);
    end;
  end;
  SetLength(LItems, LCount);
  Result := ReplaceField(Result, 'wires', NyxArray(LItems));

  if (AChange.Kind = nfcRemoveStep) and (LConditions.Count > 0) then
  begin
    SetLength(LItems, LConditions.Count);
    LCount := 0;
    for LIndex := 0 to LConditions.Count - 1 do
    begin
      LWire := LConditions.Item(LIndex);

      if (LWire.Field('step').AsText = LStep) or
        (LWire.Field('source').Field('step').AsText = LStep) then
      begin

        if not AChange.FData.Field('detachWires').AsBoolean then
        begin
          raise ENyxFlow.Create('Remove condition dependencies first or explicitly detach them');
        end;
        Continue;
      end;
      LItems[LCount] := LWire;
      Inc(LCount);
    end;
    SetLength(LItems, LCount);
    LConditions := NyxArray(LItems);
  end;
  Result := WithConditions(Result, LConditions);
end;

function EditNyxFlows(const ARegistry: INyxFlowDeclarations;
  const AChanges: array of TNyxFlowChange): INyxFlowDeclarations;
type
  TEntry = record
    Reference: TNyxFlowRef;
    Data: TNyxDataValue;
    Present: Boolean;
  end;
var
  LEntries: array of TEntry;
  LChange: TNyxFlowChange;
  LIndex: Integer;
  LPosition: Integer;
  LFound: Integer;
begin

  if (ARegistry = nil) or (Length(AChanges) < 1) or (Length(AChanges) > 64) then
  begin
    raise ENyxFlow.Create('A grouped graph edit requires 1..64 typed changes');
  end;
  SetLength(LEntries, ARegistry.Count);
  for LIndex := 0 to ARegistry.Count - 1 do
  begin
    LEntries[LIndex].Reference := ARegistry.Reference(LIndex);
    LEntries[LIndex].Data := ARegistry.Definition(LEntries[LIndex].Reference).ToData;
    LEntries[LIndex].Present := True;
  end;
  for LIndex := 0 to High(AChanges) do
  begin
    LChange := AChanges[LIndex];
    LChange.ToData;

    if LChange.FExpected.Defined and
      (NyxFlowBaseline(ARegistry, LChange.Reference).ToJSON <> LChange.FExpected.ToJSON) then
    begin
      raise ENyxFlow.Create('Graph changed since this editor proposal was captured');
    end;
    LFound := -1;
    for LPosition := 0 to High(LEntries) do
    begin

      if LEntries[LPosition].Present and
        (LEntries[LPosition].Reference.Name = LChange.Reference.Name) then
      begin
        LFound := LPosition;
        Break;
      end;
    end;

    if LChange.Kind = nfcDefine then
    begin

      if LFound < 0 then
      begin
        LFound := Length(LEntries);
        SetLength(LEntries, LFound + 1);
        LEntries[LFound].Reference := LChange.Reference;
      end;
      LEntries[LFound].Present := True;
      LEntries[LFound].Data := LChange.FData.Field('definition');
    end
    else
    begin

      if LFound < 0 then
      begin
        raise ENyxFlow.Create('The named graph no longer exists');
      end;

      if LChange.Kind = nfcRemove then
      begin
        LEntries[LFound].Present := False;
      end
      else
      begin
        LEntries[LFound].Data := ChangeData(LEntries[LFound].Data, LChange);
      end;
    end;
  end;
  Result := NewNyxFlowDeclarations;
  for LIndex := 0 to High(LEntries) do
  begin

    if LEntries[LIndex].Present then
    begin
      Result.Define(LEntries[LIndex].Reference,
        NyxFlowDefinitionFromData(LEntries[LIndex].Data));
    end;
  end;
end;

end.
