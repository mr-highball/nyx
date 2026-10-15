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
unit nyx.flow.declarations;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.bytes, nyx.flow.types, nyx.flow, nyx.logic;

const
  NyxFlowsWireField = 'flows';
  NyxMaximumFlowDefinitions = 64;
  NyxMaximumFlowDefinitionBytes = 1048576;

type
  { A graph's application name is distinct from a step/port/kind identity. }
  TNyxFlowRef = record
  private
    FName: TNyxText;
  public
    property Name: TNyxText read FName;
    function Defined: Boolean;
  end;

  { Immutable saved graph, containing only admitted typed schema/configuration.
    No executable operation, resolver, scheduler, application or document is
    retained. Build explicitly reconstructs a fresh runtime through built-in or
    caller-supplied authority; import/clone/validation never executes creators. }
  INyxFlowDefinition = interface(IInterface)
    ['{D1411014-6120-4C84-A708-000000000011}']
    function GetStepCount: Integer;
    function GetWireCount: Integer;
    function Step(AIndex: Integer): TNyxFlowStepRef;
    function Descriptor(AIndex: Integer): TNyxLogicDescriptor;
    function Configuration(AIndex: Integer): TNyxDataValue;
    function ToData: TNyxDataValue;
    function Build(const AResolver: INyxLogicResolver = nil): INyxFlowGraph;
    property StepCount: Integer read GetStepCount;
    property WireCount: Integer read GetWireCount;
  end;

  { Document-owned membership, sharing immutable definitions on Clone. Define
    normalizes completely before replacement; a replacement retains its position.
    Retained registries/definitions may outlive the document without backreferences.
    Unknown creator declarations stay admitted data, never implicit host code. }
  INyxFlowDeclarations = interface(IInterface)
    ['{D1411014-6120-4C84-A708-000000000012}']
    function Define(const AReference: TNyxFlowRef;
      const AGraph: INyxFlowGraph): INyxFlowDeclarations; overload;
    function Define(const AReference: TNyxFlowRef;
      const ADefinition: INyxFlowDefinition): INyxFlowDeclarations; overload;
    function Remove(const AReference: TNyxFlowRef): INyxFlowDeclarations;
    function Contains(const AReference: TNyxFlowRef): Boolean;
    function Definition(const AReference: TNyxFlowRef): INyxFlowDefinition;
    function Reference(AIndex: Integer): TNyxFlowRef;
    function GetCount: Integer;
    function Clone: INyxFlowDeclarations;
    function ToData: TNyxDataValue;
    property Count: Integer read GetCount;
  end;

function NyxFlowRef(const AName: TNyxText): TNyxFlowRef;
{ Builds only inert signature declarations for inspection/typed derivation.
  Evaluating them refuses; executable implementations and host authority are
  resolved separately through Definition.Build with an explicit runtime resolver. }
function NyxFlowSignatureGraph(const ADefinition: INyxFlowDefinition): INyxFlowGraph;
function NyxFlowDefinitionFromData(const AData: TNyxDataValue): INyxFlowDefinition;
function NewNyxFlowDeclarations: INyxFlowDeclarations;
function NyxFlowDeclarationsFromData(const AData: TNyxDataValue): INyxFlowDeclarations;

implementation

uses nyx.json;

type
  { This private resolver supplies inert declarations only. It validates known
    built-in configuration/signatures through their pure constructors, without
    retaining those implementations or invoking any runtime operation. }
  TDeclarationResolver = class(TInterfacedObject, INyxLogicResolver)
  public
    Steps: TNyxDataValue;
    function Resolve(const AKind: TNyxLogicKindRef;
      const AConfiguration: TNyxDataValue): INyxLogicOperation;
  end;

  { Immutable admitted data plus private typed read snapshots. Full graph/port/
    condition validation still runs before construction; getters do not reparse
    the complete step vector. Copies expose no mutable vector or runtime host. }
  TDefinition = class(TInterfacedObject, INyxFlowDefinition)
  private
    FData: TNyxDataValue;
    FSteps: array of TNyxFlowStepRef;
    FDescriptors: array of TNyxLogicDescriptor;
    FConfigurations: array of TNyxDataValue;
    FWireCount: Integer;
    procedure CheckStep(AIndex: Integer);
  public
    constructor Create(const AAdmittedData: TNyxDataValue);
    function GetStepCount: Integer;
    function GetWireCount: Integer;
    function Step(AIndex: Integer): TNyxFlowStepRef;
    function Descriptor(AIndex: Integer): TNyxLogicDescriptor;
    function Configuration(AIndex: Integer): TNyxDataValue;
    function ToData: TNyxDataValue;
    function Build(const AResolver: INyxLogicResolver): INyxFlowGraph;
  end;

  { Ordered membership owns immutable admitted definitions. Foreign snapshots
    normalize under the reentry guard; Store publishes only a complete local
    candidate. Clones share immutable definitions, never executable authority. }
  TDeclarations = class(TInterfacedObject, INyxFlowDeclarations)
  private
    FReferences: array of TNyxFlowRef;
    FDefinitions: array of INyxFlowDefinition;
    FAdmitting: Boolean;
    function IndexOf(const AReference: TNyxFlowRef): Integer;
    { Only this unit's completely admitted definitions reach the final commit.
      Public foreign definitions still normalize through the full boundary. }
    function Store(const AReference: TNyxFlowRef; AIndex: Integer;
      const ADefinition: INyxFlowDefinition): INyxFlowDeclarations;
  public
    function Define(const AReference: TNyxFlowRef;
      const AGraph: INyxFlowGraph): INyxFlowDeclarations; overload;
    function Define(const AReference: TNyxFlowRef;
      const ADefinition: INyxFlowDefinition): INyxFlowDeclarations; overload;
    function Remove(const AReference: TNyxFlowRef): INyxFlowDeclarations;
    function Contains(const AReference: TNyxFlowRef): Boolean;
    function Definition(const AReference: TNyxFlowRef): INyxFlowDefinition;
    function Reference(AIndex: Integer): TNyxFlowRef;
    function GetCount: Integer;
    function Clone: INyxFlowDeclarations;
    function ToData: TNyxDataValue;
  end;

function NyxFlowRef(const AName: TNyxText): TNyxFlowRef;
begin
  Result.FName := NyxFlowStep(AName).Name;
end;

function TNyxFlowRef.Defined: Boolean;
begin
  Result := FName <> '';
end;

function TDeclarationResolver.Resolve(const AKind: TNyxLogicKindRef;
  const AConfiguration: TNyxDataValue): INyxLogicOperation;
var
  LIndex: Integer;
  LStep: TNyxDataValue;
  LDescriptor: TNyxLogicDescriptor;
  LSignature: TNyxText;
  LFound: Boolean;
  LKnown: INyxLogicOperation;
begin
  LSignature := '';
  LFound := False;
  for LIndex := 0 to Steps.Count - 1 do
  begin
    LStep := Steps.Item(LIndex);

    if (LStep.Field('kind').AsText <> AKind.Name) or
      (LStep.Field('configuration').ToJSON <> AConfiguration.ToJSON) then
    begin
      Continue;
    end;
    LDescriptor := TNyxLogicDescriptor.FromData(LStep.Field('descriptor'));

    if LFound and (LSignature <> LDescriptor.ToData.ToJSON) then
    begin
      raise ENyxFlow.Create('One Logic kind/configuration cannot declare conflicting signatures');
    end;
    LFound := True;
    LSignature := LDescriptor.ToData.ToJSON;
  end;

  if not LFound then
  begin
    raise ENyxFlow.Create('Saved Logic declaration is missing');
  end;

  if (AKind.Name = 'json-parse') or (AKind.Name = 'json-format') or
    (AKind.Name = 'json-select') or (AKind.Name = 'text-join') or
    (AKind.Name = 'json-set') or (AKind.Name = 'json-remove') or
    (AKind.Name = 'json-merge') or (AKind.Name = 'json-group') or
    (AKind.Name = 'json-distinct') or (AKind.Name = 'json-join') or
    (AKind.Name = 'json-aggregate') or (AKind.Name = 'flow-join') or
    (AKind.Name = 'regex-match') or (AKind.Name = 'regex-replace') or
    (AKind.Name = 'regex-extract') then
  begin
    LKnown := NyxLogicFromData(AKind, AConfiguration);

    if LKnown.Descriptor.ToData.ToJSON <> LSignature then
    begin
      raise ENyxFlow.Create('Saved built-in Logic signature differs from its typed contract');
    end;
  end;
  Result := NewNyxLogicDeclaration(LDescriptor, AConfiguration);
end;

function NyxFlowSignatureGraph(const ADefinition: INyxFlowDefinition): INyxFlowGraph;
var
  LData: TNyxDataValue;
  LResolver: TDeclarationResolver;
  LAuthority: INyxLogicResolver;
begin

  if ADefinition = nil then
  begin
    raise ENyxFlow.Create('Signature graph requires an admitted definition');
  end;
  LData := ADefinition.ToData;
  LResolver := TDeclarationResolver.Create;
  LAuthority := LResolver;
  LResolver.Steps := LData.Field('steps');
  Result := NyxFlowGraphFromData(LData, LAuthority);
end;

function NyxFlowDefinitionFromData(const AData: TNyxDataValue): INyxFlowDefinition;
var
  LResolver: TDeclarationResolver;
  LAuthority: INyxLogicResolver;
  LGraph: INyxFlowGraph;
  LDefinition: TDefinition;
begin

  if NyxUTF8ByteCount(AData.ToJSON) > NyxMaximumFlowDefinitionBytes then
  begin
    raise ENyxFlow.Create('Saved graph exceeds its UTF-8 byte boundary');
  end;
  LResolver := TDeclarationResolver.Create;
  LAuthority := LResolver;
  LResolver.Steps := AData.Field('steps');
  LGraph := NyxFlowGraphFromData(AData, LAuthority);
  LDefinition := TDefinition.Create(LGraph.ToData);
  Result := LDefinition;
end;

constructor TDefinition.Create(const AAdmittedData: TNyxDataValue);
var
  LItems: TNyxDataValue;
  LItem: TNyxDataValue;
  LIndex: Integer;
begin
  inherited Create;
  FData := AAdmittedData.Copy;
  LItems := FData.Field('steps');
  SetLength(FSteps, LItems.Count);
  SetLength(FDescriptors, LItems.Count);
  SetLength(FConfigurations, LItems.Count);
  for LIndex := 0 to LItems.Count - 1 do
  begin
    LItem := LItems.Item(LIndex);
    FSteps[LIndex] := NyxFlowStep(LItem.Field('id').AsText);
    FDescriptors[LIndex] := TNyxLogicDescriptor.FromData(LItem.Field('descriptor'));
    FConfigurations[LIndex] := LItem.Field('configuration');
  end;
  FWireCount := FData.Field('wires').Count;
end;

procedure TDefinition.CheckStep(AIndex: Integer);
begin

  if (AIndex < 0) or (AIndex >= Length(FSteps)) then
  begin
    { Preserve the original data-access boundary's public failure family. }
    raise ENyxJSON.Create('Data item index out of range');
  end;
end;

function TDefinition.GetStepCount: Integer;
begin
  Result := Length(FSteps);
end;

function TDefinition.GetWireCount: Integer;
begin
  Result := FWireCount;
end;

function TDefinition.Step(AIndex: Integer): TNyxFlowStepRef;
begin
  CheckStep(AIndex);
  Result := FSteps[AIndex];
end;

function TDefinition.Descriptor(AIndex: Integer): TNyxLogicDescriptor;
begin
  CheckStep(AIndex);
  Result := FDescriptors[AIndex].Copy;
end;

function TDefinition.Configuration(AIndex: Integer): TNyxDataValue;
begin
  CheckStep(AIndex);
  Result := FConfigurations[AIndex].Copy;
end;

function TDefinition.ToData: TNyxDataValue;
begin
  Result := FData.Copy;
end;

function TDefinition.Build(const AResolver: INyxLogicResolver): INyxFlowGraph;
begin
  Result := NyxFlowGraphFromData(FData, AResolver);
end;

function NewNyxFlowDeclarations: INyxFlowDeclarations;
begin
  Result := TDeclarations.Create;
end;

function TDeclarations.IndexOf(const AReference: TNyxFlowRef): Integer;
begin
  NyxFlowRef(AReference.Name);
  for Result := 0 to High(FReferences) do
  begin

    if FReferences[Result].Name = AReference.Name then
    begin
      Exit;
    end;
  end;
  Result := -1;
end;

function TDeclarations.GetCount: Integer;
begin
  Result := Length(FReferences);
end;

function TDeclarations.Contains(const AReference: TNyxFlowRef): Boolean;
begin
  Result := IndexOf(AReference) >= 0;
end;

function TDeclarations.Define(const AReference: TNyxFlowRef;
  const AGraph: INyxFlowGraph): INyxFlowDeclarations;
var
  LDefinition: INyxFlowDefinition;
  LGraph: INyxFlowGraph;
begin

  if FAdmitting then
  begin
    raise ENyxFlow.Create('Saved graph registry admission cannot reenter');
  end;

  if AGraph = nil then
  begin
    raise ENyxFlow.Create('Saved graph cannot be absent');
  end;
  FAdmitting := True;
  LGraph := AGraph;
  try
    LDefinition := NyxFlowDefinitionFromData(LGraph.ToData);
  finally
    FAdmitting := False;
  end;
  { This definition was just completely admitted by our own constructor. A
    second public normalization reparsed the same graph without adding a guard. }
  Result := Store(AReference, IndexOf(AReference), LDefinition);
end;

function TDeclarations.Define(const AReference: TNyxFlowRef;
  const ADefinition: INyxFlowDefinition): INyxFlowDeclarations;
var
  LIndex: Integer;
  LDefinition: INyxFlowDefinition;
  LInput: INyxFlowDefinition;
begin

  if FAdmitting then
  begin
    raise ENyxFlow.Create('Saved graph registry admission cannot reenter');
  end;
  LIndex := IndexOf(AReference);

  if ADefinition = nil then
  begin
    raise ENyxFlow.Create('Saved graph definition cannot be absent');
  end;
  FAdmitting := True;
  LInput := ADefinition;
  try
    LDefinition := NyxFlowDefinitionFromData(LInput.ToData);
  finally
    FAdmitting := False;
  end;
  Result := Store(AReference, LIndex, LDefinition);
end;

function TDeclarations.Store(const AReference: TNyxFlowRef; AIndex: Integer;
  const ADefinition: INyxFlowDefinition): INyxFlowDeclarations;
var
  LIndex: Integer;
begin
  LIndex := AIndex;

  if LIndex < 0 then
  begin

    if Length(FReferences) >= NyxMaximumFlowDefinitions then
    begin
      raise ENyxFlow.Create('Saved graph registry capacity exceeded');
    end;
    LIndex := Length(FReferences);
    SetLength(FReferences, LIndex + 1);
    SetLength(FDefinitions, LIndex + 1);
    FReferences[LIndex] := AReference;
  end;
  FDefinitions[LIndex] := ADefinition;
  Result := Self;
end;

function TDeclarations.Remove(const AReference: TNyxFlowRef): INyxFlowDeclarations;
var
  LIndex: Integer;
begin

  if FAdmitting then
  begin
    raise ENyxFlow.Create('Saved graph registry admission cannot reenter');
  end;
  LIndex := IndexOf(AReference);

  if LIndex < 0 then
  begin
    raise ENyxFlow.Create('Saved graph reference is not declared');
  end;
  while LIndex < High(FReferences) do
  begin
    FReferences[LIndex] := FReferences[LIndex + 1];
    FDefinitions[LIndex] := FDefinitions[LIndex + 1];
    Inc(LIndex);
  end;
  SetLength(FReferences, Length(FReferences) - 1);
  SetLength(FDefinitions, Length(FDefinitions) - 1);
  Result := Self;
end;

function TDeclarations.Definition(const AReference: TNyxFlowRef): INyxFlowDefinition;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(AReference);

  if LIndex < 0 then
  begin
    raise ENyxFlow.Create('Saved graph reference is not declared');
  end;
  Result := FDefinitions[LIndex];
end;

function TDeclarations.Reference(AIndex: Integer): TNyxFlowRef;
begin

  if (AIndex < 0) or (AIndex >= Length(FReferences)) then
  begin
    raise ENyxFlow.Create('Saved graph registry index out of range');
  end;
  Result := FReferences[AIndex];
end;

function TDeclarations.Clone: INyxFlowDeclarations;
var
  LCopy: TDeclarations;
begin
  LCopy := TDeclarations.Create;
  Result := LCopy;
  LCopy.FReferences := Copy(FReferences, 0, Length(FReferences));
  LCopy.FDefinitions := Copy(FDefinitions, 0, Length(FDefinitions));
end;

function TDeclarations.ToData: TNyxDataValue;
var
  LValues: array of TNyxDataValue;
  LIndex: Integer;
begin
  SetLength(LValues, Length(FReferences));
  for LIndex := 0 to High(FReferences) do
  begin
    LValues[LIndex] := NyxObject([NyxField('name', NyxData(FReferences[LIndex].Name)),
      NyxField('definition', FDefinitions[LIndex].ToData)]);
  end;
  Result := NyxArray(LValues);
end;

function NyxFlowDeclarationsFromData(const AData: TNyxDataValue): INyxFlowDeclarations;
var
  LIndex: Integer;
  LEntry: TNyxDataValue;
  LReference: TNyxFlowRef;
begin

  if (AData.Kind <> ndArray) or (AData.Count > NyxMaximumFlowDefinitions) then
  begin
    raise ENyxFlow.Create('Saved graph registry requires a bounded array');
  end;
  Result := NewNyxFlowDeclarations;
  for LIndex := 0 to AData.Count - 1 do
  begin
    LEntry := AData.Item(LIndex);
    ValidateNyxFlowFields(LEntry, ['name', 'definition']);
    LReference := NyxFlowRef(LEntry.Field('name').AsText);

    if Result.Contains(LReference) then
    begin
      raise ENyxFlow.Create('Saved graph references must be distinct');
    end;
    Result.Define(LReference, NyxFlowDefinitionFromData(LEntry.Field('definition')));
  end;
end;

end.
