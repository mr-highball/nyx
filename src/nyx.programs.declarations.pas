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

unit nyx.programs.declarations;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.data, nyx.programs.types, nyx.programs.contract,
  nyx.programs;

const
  NyxProgramsWireField = 'programs';
  NyxMaximumProgramDefinitions = 64;
  NyxMaximumProgramDefinitionBytes = 1048576;

type
  { Exact project-local name, distinct from a Pascal symbol, control or graph.
    Unicode names are case sensitive, bounded and retain authored whitespace. }
  TNyxProgramRef = record
  private
    FName: TNyxText;
  public
    property Name: TNyxText read FName;
    function Defined: Boolean;
  end;

  { Admitted immutable source/contract/limits. Compilation validates every body
    but never runs it. Compile returns immutable managed code with no grants,
    services, application or runtime state; each invocation remains independent.
    Definitions can outlive their document without a backreference. }
  INyxProgramDefinition = interface(IInterface)
    ['{E291C5CB-6B87-4E85-A8E6-000000000210}']
    function GetSource: TNyxText;
    function GetContract: TNyxProgramContract;
    function GetLimits: TNyxProgramLimits;
    function Compile: INyxProgram;
    function ToData: TNyxDataValue;
    property Source: TNyxText read GetSource;
    property Contract: TNyxProgramContract read GetContract;
    property Limits: TNyxProgramLimits read GetLimits;
  end;

  { Single-authoring-thread document membership with independent Clone vectors.
    Immutable admitted definitions/code may be shared. Foreign definitions are
    normalized under a reentry guard before one publication. Invalid programs,
    duplicates on import, budgets and callbacks preserve the accepted registry.
    No grants, query objects, handles or scheduler enter persisted metadata. }
  INyxProgramDeclarations = interface(IInterface)
    ['{E291C5CB-6B87-4E85-A8E6-000000000211}']
    function Define(const AReference: TNyxProgramRef;
      const ADefinition: INyxProgramDefinition): INyxProgramDeclarations;
    function Remove(const AReference: TNyxProgramRef): INyxProgramDeclarations;
    function Contains(const AReference: TNyxProgramRef): Boolean;
    function Definition(const AReference: TNyxProgramRef): INyxProgramDefinition;
    function Reference(AIndex: Integer): TNyxProgramRef;
    function GetCount: Integer;
    function Clone: INyxProgramDeclarations;
    function ToData: TNyxDataValue;
    property Count: Integer read GetCount;
  end;

function NyxProgramRef(const AName: TNyxText): TNyxProgramRef;
{ Convenience defaults do not grant capabilities. Both factories parse/type-
  check before returning; source errors retain structured UTF-8 diagnostics. }
function NewNyxProgramDefinition(const ASource: TNyxText;
  const AContract: TNyxProgramContract): INyxProgramDefinition; overload;
function NewNyxProgramDefinition(const ASource: TNyxText;
  const AContract: TNyxProgramContract;
  const ALimits: TNyxProgramLimits): INyxProgramDefinition; overload;
function NyxProgramDefinitionFromData(const AData: TNyxDataValue): INyxProgramDefinition;
function NewNyxProgramDeclarations: INyxProgramDeclarations;
function NyxProgramDeclarationsFromData(const AData: TNyxDataValue): INyxProgramDeclarations;

implementation

uses nyx.bytes, nyx.flow.types;

type
  { Private immutable code owner. The compiled service has independent per-call
    frames and no runtime authority; caching it avoids repeated Clone parsing. }
  TProgramDefinition = class(TInterfacedObject, INyxProgramDefinition)
  public
    ProgramValue: INyxProgram;
    function GetSource: TNyxText;
    function GetContract: TNyxProgramContract;
    function GetLimits: TNyxProgramLimits;
    function Compile: INyxProgram;
    function ToData: TNyxDataValue;
  end;

  { Private mutable registry. Parallel vectors contain no COM record fields;
    each derived membership stages allocation before updating accepted fields. }
  TProgramDeclarations = class(TInterfacedObject, INyxProgramDeclarations)
  private
    FNames: array of TNyxProgramRef;
    FDefinitions: array of INyxProgramDefinition;
    FSizes: array of TNyxTextBytes;
    FBytes: TNyxTextBytes;
    FAdmitting: Boolean;
    function IndexOf(const AReference: TNyxProgramRef): Integer;
    procedure RequireMutation;
  public
    function Define(const AReference: TNyxProgramRef;
      const ADefinition: INyxProgramDefinition): INyxProgramDeclarations;
    function Remove(const AReference: TNyxProgramRef): INyxProgramDeclarations;
    function Contains(const AReference: TNyxProgramRef): Boolean;
    function Definition(const AReference: TNyxProgramRef): INyxProgramDefinition;
    function Reference(AIndex: Integer): TNyxProgramRef;
    function GetCount: Integer;
    function Clone: INyxProgramDeclarations;
    function ToData: TNyxDataValue;
  end;

procedure Require(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxProgram.Create(npdType, Default(TNyxProgramSpan), AMessage);
  end;
end;

procedure RequireBudget(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxProgram.Create(npdLimit, Default(TNyxProgramSpan), AMessage);
  end;
end;

function NyxProgramRef(const AName: TNyxText): TNyxProgramRef;
begin
  { Open graph names already enforce the shared Unicode/control/length rules.
    The new record remains a distinct nominal type with no executable meaning. }
  Result.FName := NyxFlowStep(AName).Name;
end;

function TNyxProgramRef.Defined: Boolean;
begin
  Result := FName <> '';
end;

function NewNyxProgramDefinition(const ASource: TNyxText;
  const AContract: TNyxProgramContract): INyxProgramDefinition;
begin
  Result := NewNyxProgramDefinition(ASource, AContract, NyxProgramLimits);
end;

function NewNyxProgramDefinition(const ASource: TNyxText;
  const AContract: TNyxProgramContract;
  const ALimits: TNyxProgramLimits): INyxProgramDefinition;
var
  LDefinition: TProgramDefinition;
  LProgram: INyxProgram;
begin
  LProgram := CompileNyxProgram(ASource, AContract, ALimits);
  LDefinition := TProgramDefinition.Create;
  Result := LDefinition;
  LDefinition.ProgramValue := LProgram;
end;

function NyxProgramDefinitionFromData(const AData: TNyxDataValue): INyxProgramDefinition;
var
  LDefinition: TProgramDefinition;
  LProgram: INyxProgram;
begin
  LProgram := NyxProgramFromData(AData);
  LDefinition := TProgramDefinition.Create;
  Result := LDefinition;
  LDefinition.ProgramValue := LProgram;
end;

function TProgramDefinition.GetSource: TNyxText;
begin
  Result := ProgramValue.Source;
end;

function TProgramDefinition.GetContract: TNyxProgramContract;
begin
  Result := ProgramValue.Contract;
end;

function TProgramDefinition.GetLimits: TNyxProgramLimits;
begin
  Result := ProgramValue.Limits;
end;

function TProgramDefinition.Compile: INyxProgram;
begin
  Result := ProgramValue;
end;

function TProgramDefinition.ToData: TNyxDataValue;
begin
  Result := ProgramValue.ToData;
end;

function NewNyxProgramDeclarations: INyxProgramDeclarations;
begin
  Result := TProgramDeclarations.Create;
end;

procedure TProgramDeclarations.RequireMutation;
begin
  Require(not FAdmitting, 'Program declaration mutation cannot reenter admission');
end;

function TProgramDeclarations.IndexOf(const AReference: TNyxProgramRef): Integer;
var
  LIndex: Integer;
begin
  Require(AReference.Defined, 'Construct a project program reference first');
  for LIndex := 0 to High(FNames) do
  begin

    if FNames[LIndex].Name = AReference.Name then
    begin
      Exit(LIndex);
    end;
  end;
  Result := -1;
end;

function TProgramDeclarations.Define(const AReference: TNyxProgramRef;
  const ADefinition: INyxProgramDefinition): INyxProgramDeclarations;
var
  LKeepAlive: INyxProgramDeclarations;
  LInput: INyxProgramDefinition;
  LReference: TNyxProgramRef;
  LDefinition: INyxProgramDefinition;
  LNames: array of TNyxProgramRef;
  LDefinitions: array of INyxProgramDefinition;
  LSizes: array of TNyxTextBytes;
  LIndex: Integer;
  LPosition: Integer;
  LCount: Integer;
  LBytes: TNyxTextBytes;
  LSize: TNyxTextBytes;
begin
  LKeepAlive := Self;
  RequireMutation;
  Require(ADefinition <> nil, 'A saved program requires an admitted definition');
  LReference := NyxProgramRef(AReference.Name);
  LPosition := IndexOf(LReference);
  RequireBudget((LPosition >= 0) or (Length(FNames) < NyxMaximumProgramDefinitions),
    'Program declaration membership exceeds its bound');
  FAdmitting := True;
  try
    { Always normalize foreign data before changing any accepted vector. Compile
      validates source, not runtime execution; imported metadata grants nothing. }
    { Const inputs can alias caller variables. Capture the name before creator
      callouts and retain the interface while ToData may retire those variables.
      Retire that borrowed creator before publication, under the same guard. }
    LInput := ADefinition;
    LDefinition := NyxProgramDefinitionFromData(LInput.ToData);
    LInput := nil;
    { Count the encoded entry, including escaped open names. Two conservative
      separator/bracket bytes per entry keep every admitted export importable. }
    LSize := NyxUTF8ByteCount(NyxObject([
      NyxField('name', NyxData(LReference.Name)),
      NyxField('program', LDefinition.ToData)]).ToJSON) + 2;
    LBytes := FBytes;

    if LPosition >= 0 then
    begin
      LBytes := LBytes - FSizes[LPosition];
    end;
    RequireBudget(LSize <= NyxMaximumProgramDefinitionBytes - LBytes,
      'Program declaration bytes exceed the project bound');
    LCount := Length(FNames);

    if LPosition < 0 then
    begin
      LPosition := LCount;
      Inc(LCount);
    end;
    SetLength(LNames, LCount);
    SetLength(LDefinitions, LCount);
    SetLength(LSizes, LCount);
    for LIndex := 0 to High(FNames) do
    begin
      LNames[LIndex] := FNames[LIndex];
      LDefinitions[LIndex] := FDefinitions[LIndex];
      LSizes[LIndex] := FSizes[LIndex];
    end;
    LNames[LPosition] := LReference;
    LDefinitions[LPosition] := LDefinition;
    LSizes[LPosition] := LSize;
    FNames := LNames;
    FDefinitions := LDefinitions;
    FSizes := LSizes;
    FBytes := LBytes + LSize;
  finally
    FAdmitting := False;
  end;
  Result := Self;
end;

function TProgramDeclarations.Remove(const AReference: TNyxProgramRef): INyxProgramDeclarations;
var
  LPosition: Integer;
  LIndex: Integer;
  LTarget: Integer;
  LNames: array of TNyxProgramRef;
  LDefinitions: array of INyxProgramDefinition;
  LSizes: array of TNyxTextBytes;
begin
  RequireMutation;
  LPosition := IndexOf(AReference);
  Require(LPosition >= 0, 'The project program is unavailable');
  SetLength(LNames, Length(FNames) - 1);
  SetLength(LDefinitions, Length(FNames) - 1);
  SetLength(LSizes, Length(FNames) - 1);
  LTarget := 0;
  for LIndex := 0 to High(FNames) do
  begin

    if LIndex <> LPosition then
    begin
      LNames[LTarget] := FNames[LIndex];
      LDefinitions[LTarget] := FDefinitions[LIndex];
      LSizes[LTarget] := FSizes[LIndex];
      Inc(LTarget);
    end;
  end;
  FBytes := FBytes - FSizes[LPosition];
  FNames := LNames;
  FDefinitions := LDefinitions;
  FSizes := LSizes;
  Result := Self;
end;

function TProgramDeclarations.Contains(const AReference: TNyxProgramRef): Boolean;
begin
  Result := IndexOf(AReference) >= 0;
end;

function TProgramDeclarations.Definition(const AReference: TNyxProgramRef): INyxProgramDefinition;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(AReference);
  Require(LIndex >= 0, 'The project program is unavailable');
  Result := FDefinitions[LIndex];
end;

function TProgramDeclarations.Reference(AIndex: Integer): TNyxProgramRef;
begin
  Require((AIndex >= 0) and (AIndex < Length(FNames)), 'Program declaration index is unavailable');
  Result := FNames[AIndex];
end;

function TProgramDeclarations.GetCount: Integer;
begin
  Result := Length(FNames);
end;

function TProgramDeclarations.Clone: INyxProgramDeclarations;
var
  LCopy: TProgramDeclarations;
  LIndex: Integer;
begin
  LCopy := TProgramDeclarations.Create;
  Result := LCopy;
  SetLength(LCopy.FNames, Length(FNames));
  SetLength(LCopy.FDefinitions, Length(FNames));
  SetLength(LCopy.FSizes, Length(FNames));
  for LIndex := 0 to High(FNames) do
  begin
    LCopy.FNames[LIndex] := FNames[LIndex];
    LCopy.FDefinitions[LIndex] := FDefinitions[LIndex];
    LCopy.FSizes[LIndex] := FSizes[LIndex];
  end;
  LCopy.FBytes := FBytes;
end;

function TProgramDeclarations.ToData: TNyxDataValue;
var
  LItems: array of TNyxDataValue;
  LIndex: Integer;
begin
  SetLength(LItems, Length(FNames));
  for LIndex := 0 to High(FNames) do
  begin
    LItems[LIndex] := NyxObject([NyxField('name', NyxData(FNames[LIndex].Name)),
      NyxField('program', FDefinitions[LIndex].ToData)]);
  end;
  Result := NyxArray(LItems);
end;

function NyxProgramDeclarationsFromData(const AData: TNyxDataValue): INyxProgramDeclarations;
var
  LItem: TNyxDataValue;
  LReference: TNyxProgramRef;
  LIndex: Integer;
begin
  Require(AData.Kind = ndArray, 'Program declarations require an array');
  RequireBudget(AData.Count <= NyxMaximumProgramDefinitions,
    'Program declarations require bounded membership');
  RequireBudget(NyxUTF8ByteCount(AData.ToJSON) <= NyxMaximumProgramDefinitionBytes,
    'Program declaration input exceeds the project byte bound');
  Result := NewNyxProgramDeclarations;
  for LIndex := 0 to AData.Count - 1 do
  begin
    LItem := AData.Item(LIndex);
    ValidateNyxFlowFields(LItem, ['name', 'program']);
    LReference := NyxProgramRef(LItem.Field('name').AsText);
    Require(not Result.Contains(LReference), 'Duplicate project program declaration');
    Result.Define(LReference, NyxProgramDefinitionFromData(LItem.Field('program')));
  end;
end;

end.
