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



unit nyx.programs.contract;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.data, nyx.programs.types, nyx.scheduler;

type
  { Declared immutable invocation inputs versus required program outputs. Inputs
    are read-only script globals; outputs are published only in a complete result. }
  TNyxProgramDirection = (npvInput, npvOutput);

  { One named typed boundary value. Metadata owns no runtime application value.
    Source variables cannot change its declared type or readonly input direction. }
  TNyxProgramPort = record
    Name: TNyxProgramSymbolRef;
    ValueType: TNyxProgramType;
    Direction: TNyxProgramDirection;
  end;

  { Host routine parameter copied into an immutable signature. Positional calls
    have exact families; compiler-controlled Integer-to-Double widening is the
    sole numeric promotion. Opaque handles retain their declared named type. }
  TNyxProgramParameter = record
    Name: TNyxProgramSymbolRef;
    ValueType: TNyxProgramType;
  end;

  { Queries read an explicit frozen host snapshot. Proposals append typed calls
    to an invocation-owned journal and return Void; they never call a live host. }
  TNyxProgramRoutineKind = (nprQuery, nprProposal);

  { Declarative host signature, never an executable callback or permission.
    Fluent Argument detaches parameters. A proposal requires a capability;
    a query may be explicitly pure without one. Executing requires caller grants
    for every referenced capability, including unreachable compiled branches. }
  TNyxProgramRoutine = record
  private
    FDefined: Boolean;
    FName: TNyxProgramSymbolRef;
    FKind: TNyxProgramRoutineKind;
    FResult: TNyxProgramType;
    FCapability: TNyxProgramCapabilityRef;
    FParameters: array of TNyxProgramParameter;
  public
    function Argument(const AName: TNyxProgramSymbolRef;
      const AType: TNyxProgramType): TNyxProgramRoutine;
    function Parameter(AIndex: Integer): TNyxProgramParameter;
    function ParameterCount: Integer;
    function Copy: TNyxProgramRoutine;
    procedure Validate;
    function ToData: TNyxDataValue;
    { Import checks the parameter count before allocating its owned vector.
      The convenience overload uses the default program parameter limit. }
    class function FromData(const AData: TNyxDataValue): TNyxProgramRoutine; overload; static;
    class function FromData(const AData: TNyxDataValue;
      AMaximumParameters: Integer): TNyxProgramRoutine; overload; static;
    property Defined: Boolean read FDefined;
    property Name: TNyxProgramSymbolRef read FName;
    property Kind: TNyxProgramRoutineKind read FKind;
    property ResultType: TNyxProgramType read FResult;
    property Capability: TNyxProgramCapabilityRef read FCapability;
  end;

  { Independently owned immutable public vocabulary for compilation. Signatures
    and input/output types persist without runtime grants, query objects, handles
    or applications. Local derivation copies vectors on both compiler targets. }
  TNyxProgramContract = record
  private
    FDefined: Boolean;
    FPorts: array of TNyxProgramPort;
    FRoutines: array of TNyxProgramRoutine;
    function AddPort(const AName: TNyxProgramSymbolRef;
      const AType: TNyxProgramType; ADirection: TNyxProgramDirection): TNyxProgramContract;
    procedure AdmitName(const AName: TNyxProgramSymbolRef);
  public
    function Input(const AName: TNyxProgramSymbolRef;
      const AType: TNyxProgramType): TNyxProgramContract;
    function Output(const AName: TNyxProgramSymbolRef;
      const AType: TNyxProgramType): TNyxProgramContract;
    function WithRoutine(const ARoutine: TNyxProgramRoutine): TNyxProgramContract;
    function Port(AIndex: Integer): TNyxProgramPort;
    function PortCount: Integer;
    function Routine(AIndex: Integer): TNyxProgramRoutine;
    function RoutineCount: Integer;
    function Copy: TNyxProgramContract;
    procedure Validate;
    { Admit metadata counts before any deep compiler copy. This checks the
      caller's budgets without borrowing or copying runtime capabilities. }
    procedure ValidateLimits(const ALimits: TNyxProgramLimits);
    function ToData: TNyxDataValue;
    { Count admission precedes vector allocation. The default overload uses
      NyxProgramLimits; declaration import supplies its own persisted limits. }
    class function FromData(const AData: TNyxDataValue): TNyxProgramContract; overload; static;
    class function FromData(const AData: TNyxDataValue;
      const ALimits: TNyxProgramLimits): TNyxProgramContract; overload; static;
    property Defined: Boolean read FDefined;
  end;

  { Caller-owned exact capability grants. No persistence/import API turns design
    metadata into grants. Allow derives an independent set and never widens the
    scope of the separately injected host snapshot or opaque handles. }
  TNyxProgramGrants = record
  private
    FDefined: Boolean;
    FItems: array of TNyxProgramCapabilityRef;
  public
    function Allow(const ACapability: TNyxProgramCapabilityRef): TNyxProgramGrants;
    function Contains(const ACapability: TNyxProgramCapabilityRef): Boolean;
    function Count: Integer;
    function Item(AIndex: Integer): TNyxProgramCapabilityRef;
    property Defined: Boolean read FDefined;
  end;

  { Ordered managed immutable runtime arguments. Construction detaches the
    vector; Item returns an immutable managed value, whose data getters detach. }
  INyxProgramArguments = interface(IInterface)
    ['{9D69BC4B-085E-45F1-A9B0-000000000108}']
    function Count: Integer;
    function Item(AIndex: Integer): INyxProgramValue;
    function Copy: INyxProgramArguments;
  end;

  { Managed immutable named invocation/result frame. WithValue derives an
    independent frame and refuses duplicate names. Nil is unassigned; normal
    factories always return a complete initialized frame, including empty. }
  INyxProgramValues = interface(IInterface)
    ['{9D69BC4B-085E-45F1-A9B0-000000000109}']
    function WithValue(const AName: TNyxProgramSymbolRef;
      const AValue: INyxProgramValue): INyxProgramValues;
    function Has(const AName: TNyxProgramSymbolRef): Boolean;
    function Value(const AName: TNyxProgramSymbolRef): INyxProgramValue;
    function BindingName(AIndex: Integer): TNyxProgramSymbolRef;
    function Item(AIndex: Integer): INyxProgramValue;
    function Count: Integer;
    function Copy: INyxProgramValues;
  end;

  { Explicit frozen, independently retained host query snapshot. Implementations
    must be side-effect free, bounded and safe on the selected execution thread.
    Queries return only declared values/opaque handles. They cannot commit edits,
    retain borrowed interpreter arguments or fabricate authority from text. }
  INyxProgramQueries = interface(IInterface)
    ['{9D69BC4B-085E-45F1-A9B0-000000000103}']
    function Query(const ARoutine: TNyxProgramSymbolRef;
      const AArguments: INyxProgramArguments): INyxProgramValue;
  end;

  { Managed immutable proposed host call with exact source provenance and
    retained immutable arguments. Metadata grants no permission or commit token.
    Publication belongs to an owning revision/generation-guarded adapter. }
  INyxProgramProposal = interface(IInterface)
    ['{9D69BC4B-085E-45F1-A9B0-000000000110}']
    function Routine: TNyxProgramRoutine;
    function Arguments: INyxProgramArguments;
    function GetSpan: TNyxProgramSpan;
    property Span: TNyxProgramSpan read GetSpan;
  end;

  { Managed immutable per-call runtime services. Fresh options grant nothing
    and retain no host, scope or clock. Derivation creates a new managed context;
    these runtime capabilities never enter persisted program declarations. }
  INyxProgramRunOptions = interface(IInterface)
    ['{9D69BC4B-085E-45F1-A9B0-000000000111}']
    function Grants(const AGrants: TNyxProgramGrants): INyxProgramRunOptions;
    function Queries(const AQueries: INyxProgramQueries): INyxProgramRunOptions;
    function Cancellation(const AScope: INyxCancellationScope): INyxProgramRunOptions;
    function Clock(const AClock: INyxProgramClock): INyxProgramRunOptions;
    procedure Validate;
    function GetPermissions: TNyxProgramGrants;
    function GetQuerySnapshot: INyxProgramQueries;
    function GetCancellationScope: INyxCancellationScope;
    function GetTimeSource: INyxProgramClock;
    property Permissions: TNyxProgramGrants read GetPermissions;
    property QuerySnapshot: INyxProgramQueries read GetQuerySnapshot;
    property CancellationScope: INyxCancellationScope read GetCancellationScope;
    property TimeSource: INyxProgramClock read GetTimeSource;
  end;

function NyxProgramContract: TNyxProgramContract;
function NyxProgramQuery(const AName: TNyxProgramSymbolRef;
  const AResult: TNyxProgramType): TNyxProgramRoutine; overload;
function NyxProgramQuery(const AName: TNyxProgramSymbolRef;
  const AResult: TNyxProgramType;
  const ACapability: TNyxProgramCapabilityRef): TNyxProgramRoutine; overload;
function NyxProgramPropose(const AName: TNyxProgramSymbolRef;
  const ACapability: TNyxProgramCapabilityRef): TNyxProgramRoutine;
function NyxProgramGrants: TNyxProgramGrants;
function NyxProgramArguments(const AValues: array of INyxProgramValue): INyxProgramArguments;
function NyxProgramValues: INyxProgramValues; overload;
{ Bulk construction validates unique Pascal symbols with an owned index and
  captures the immutable value vector once, avoiding repeated prefix copying. }
function NyxProgramValues(const ANames: array of TNyxProgramSymbolRef;
  const AValues: array of INyxProgramValue): INyxProgramValues; overload;
function NyxProgramProposal(const ARoutine: TNyxProgramRoutine;
  const AArguments: INyxProgramArguments; const ASpan: TNyxProgramSpan): INyxProgramProposal;
function NyxProgramRun: INyxProgramRunOptions;

implementation

uses nyx.flow.types, nyx.text.index;

type
  { Default owned immutable argument vector. It retains managed values and
    publishes no writable array; ordinary interface retirement releases them. }
  TFrozenArguments = class(TInterfacedObject, INyxProgramArguments)
  private
    FItems: array of INyxProgramValue;
  public
    function Count: Integer;
    function Item(AIndex: Integer): INyxProgramValue;
    function Copy: INyxProgramArguments;
  end;

  { Default owned immutable named frame. Parallel private vectors avoid putting
    COM interfaces in Pascal records, unsupported by pas2js, on either target. }
  TFrozenValues = class(TInterfacedObject, INyxProgramValues)
  private
    FNames: array of TNyxProgramSymbolRef;
    FItems: array of INyxProgramValue;
  public
    function WithValue(const AName: TNyxProgramSymbolRef;
      const AValue: INyxProgramValue): INyxProgramValues;
    function Has(const AName: TNyxProgramSymbolRef): Boolean;
    function Value(const AName: TNyxProgramSymbolRef): INyxProgramValue;
    function BindingName(AIndex: Integer): TNyxProgramSymbolRef;
    function Item(AIndex: Integer): INyxProgramValue;
    function Count: Integer;
    function Copy: INyxProgramValues;
  end;

  { Default complete proposed call. It retains only signature metadata and
    managed immutable arguments; never a query service or application context. }
  TFrozenProposal = class(TInterfacedObject, INyxProgramProposal)
  private
    FRoutine: TNyxProgramRoutine;
    FArguments: INyxProgramArguments;
    FSpan: TNyxProgramSpan;
  public
    function Routine: TNyxProgramRoutine;
    function Arguments: INyxProgramArguments;
    function GetSpan: TNyxProgramSpan;
  end;

  { Default immutable runtime options owner. Each fluent derivation retains
    service leases independently, with no record-level interface workaround. }
  TProgramOptions = class(TInterfacedObject, INyxProgramRunOptions)
  private
    FGrants: TNyxProgramGrants;
    FQueries: INyxProgramQueries;
    FScope: INyxCancellationScope;
    FClock: INyxProgramClock;
    function Clone: TProgramOptions;
  public
    function Grants(const AGrants: TNyxProgramGrants): INyxProgramRunOptions;
    function Queries(const AQueries: INyxProgramQueries): INyxProgramRunOptions;
    function Cancellation(const AScope: INyxCancellationScope): INyxProgramRunOptions;
    function Clock(const AClock: INyxProgramClock): INyxProgramRunOptions;
    procedure Validate;
    function GetPermissions: TNyxProgramGrants;
    function GetQuerySnapshot: INyxProgramQueries;
    function GetCancellationScope: INyxCancellationScope;
    function GetTimeSource: INyxProgramClock;
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

procedure CheckIndex(AIndex, ACount: Integer);
begin
  Require((AIndex >= 0) and (AIndex < ACount), 'Program contract/frame index is out of range');
end;

function NyxProgramQuery(const AName: TNyxProgramSymbolRef;
  const AResult: TNyxProgramType): TNyxProgramRoutine;
begin
  Result := NyxProgramQuery(AName, AResult, Default(TNyxProgramCapabilityRef));
end;

function NyxProgramQuery(const AName: TNyxProgramSymbolRef;
  const AResult: TNyxProgramType;
  const ACapability: TNyxProgramCapabilityRef): TNyxProgramRoutine;
begin
  Result := Default(TNyxProgramRoutine);
  Result.FDefined := True;
  Result.FName := NyxProgramSymbol(AName.Name);
  Result.FKind := nprQuery;
  Result.FResult := AResult;
  Result.FCapability := ACapability;
  Result.Validate;
end;

function NyxProgramPropose(const AName: TNyxProgramSymbolRef;
  const ACapability: TNyxProgramCapabilityRef): TNyxProgramRoutine;
begin
  Result := Default(TNyxProgramRoutine);
  Result.FDefined := True;
  Result.FName := NyxProgramSymbol(AName.Name);
  Result.FKind := nprProposal;
  Result.FResult := NyxProgramType(npkVoid);
  Result.FCapability := ACapability;
  Result.Validate;
end;

procedure TNyxProgramRoutine.Validate;
var
  LIndex: Integer;
  LNames: TNyxTextIndex;
  LKey: TNyxText;
begin
  Require(FDefined and FName.Defined and FResult.Defined, 'Program routines need a name and result type');
  Require(((FKind = nprQuery) and (FResult.Kind <> npkVoid)) or
    ((FKind = nprProposal) and (FResult.Kind = npkVoid) and FCapability.Defined),
    'Queries return a value; proposals require a capability and return Void');
  LNames := TNyxTextIndex.Create;
  try
    for LIndex := 0 to High(FParameters) do
    begin
      Require(FParameters[LIndex].Name.Defined and FParameters[LIndex].ValueType.Defined and
        (FParameters[LIndex].ValueType.Kind <> npkVoid), 'Routine parameters need exact non-Void types');
      LKey := NyxProgramIdentifierKey(FParameters[LIndex].Name.Name);
      Require(LNames.IndexOf(LKey) < 0, 'Routine parameter names must be unique');
      LNames.AddFirst(LKey, LIndex);
    end;
  finally
    LNames.Free;
  end;
end;

function TNyxProgramRoutine.Copy: TNyxProgramRoutine;
var
  LIndex: Integer;
begin
  Result := Default(TNyxProgramRoutine);
  Result.FDefined := FDefined;
  Result.FName := FName;
  Result.FKind := FKind;
  Result.FResult := FResult;
  Result.FCapability := FCapability;
  SetLength(Result.FParameters, Length(FParameters));
  for LIndex := 0 to High(FParameters) do
  begin
    Result.FParameters[LIndex] := FParameters[LIndex];
  end;
end;

function TNyxProgramRoutine.Argument(const AName: TNyxProgramSymbolRef;
  const AType: TNyxProgramType): TNyxProgramRoutine;
var
  LCount: Integer;
begin
  Result := Copy;
  LCount := Length(Result.FParameters);
  SetLength(Result.FParameters, LCount + 1);
  Result.FParameters[LCount].Name := NyxProgramSymbol(AName.Name);
  Result.FParameters[LCount].ValueType := AType;
  Result.Validate;
end;

function TNyxProgramRoutine.Parameter(AIndex: Integer): TNyxProgramParameter;
begin
  CheckIndex(AIndex, Length(FParameters));
  Result := FParameters[AIndex];
end;

function TNyxProgramRoutine.ParameterCount: Integer;
begin
  Result := Length(FParameters);
end;

function TNyxProgramRoutine.ToData: TNyxDataValue;
var
  LItems: array of TNyxDataValue;
  LIndex: Integer;
begin
  Validate;
  SetLength(LItems, Length(FParameters));
  for LIndex := 0 to High(FParameters) do
  begin
    LItems[LIndex] := NyxObject([NyxField('name', NyxData(FParameters[LIndex].Name.Name)),
      NyxField('type', FParameters[LIndex].ValueType.ToData)]);
  end;
  Result := NyxObject([NyxField('name', NyxData(FName.Name)), NyxField('kind', NyxData(Ord(FKind))),
    NyxField('result', FResult.ToData), NyxField('capability', NyxData(FCapability.Name)),
    NyxField('parameters', NyxArray(LItems))]);
end;

class function TNyxProgramRoutine.FromData(const AData: TNyxDataValue): TNyxProgramRoutine;
begin
  Result := FromData(AData, NyxProgramLimits.MaximumParameters);
end;

class function TNyxProgramRoutine.FromData(const AData: TNyxDataValue;
  AMaximumParameters: Integer): TNyxProgramRoutine;
var
  LItems: TNyxDataValue;
  LItem: TNyxDataValue;
  LIndex: Integer;
  LCapability: TNyxProgramCapabilityRef;
  LType: TNyxProgramType;
begin
  Require(AMaximumParameters > 0, 'Program parameter limits must be positive');
  ValidateNyxFlowFields(AData, ['name', 'kind', 'result', 'capability', 'parameters']);
  LItems := AData.Field('parameters');
  Require(LItems.Kind = ndArray, 'Program routine parameters require an array');
  RequireBudget(LItems.Count <= AMaximumParameters, 'Host parameter vocabulary exceeds its budget');
  LCapability := Default(TNyxProgramCapabilityRef);

  if AData.Field('capability').AsText <> '' then
  begin
    LCapability := NyxProgramCapability(AData.Field('capability').AsText);
  end;
  LType := TNyxProgramType.FromData(AData.Field('result'));
  case AData.Field('kind').AsInteger of
    0: Result := NyxProgramQuery(NyxProgramSymbol(AData.Field('name').AsText), LType, LCapability);
    1:
    begin
      Require(LType.Kind = npkVoid, 'A persisted proposal must return Void');
      Result := NyxProgramPropose(NyxProgramSymbol(AData.Field('name').AsText), LCapability);
    end;
    else
    begin
      Require(False, 'Unknown program host routine kind');
      Result := Default(TNyxProgramRoutine);
    end;
  end;
  SetLength(Result.FParameters, LItems.Count);
  for LIndex := 0 to LItems.Count - 1 do
  begin
    LItem := LItems.Item(LIndex);
    ValidateNyxFlowFields(LItem, ['name', 'type']);
    Result.FParameters[LIndex].Name := NyxProgramSymbol(LItem.Field('name').AsText);
    Result.FParameters[LIndex].ValueType := TNyxProgramType.FromData(LItem.Field('type'));
  end;
  Result.Validate;
end;

function NyxProgramContract: TNyxProgramContract;
begin
  Result := Default(TNyxProgramContract);
  Result.FDefined := True;
end;

procedure TNyxProgramContract.AdmitName(const AName: TNyxProgramSymbolRef);
var
  LIndex: Integer;
begin
  Require(FDefined and AName.Defined, 'Program contracts and names must be initialized');
  for LIndex := 0 to High(FPorts) do
  begin
    Require(not AName.Same(FPorts[LIndex].Name), 'Duplicate program boundary symbol');
  end;
  for LIndex := 0 to High(FRoutines) do
  begin
    Require(not AName.Same(FRoutines[LIndex].Name), 'Duplicate program boundary symbol');
  end;
end;

function TNyxProgramContract.AddPort(const AName: TNyxProgramSymbolRef;
  const AType: TNyxProgramType; ADirection: TNyxProgramDirection): TNyxProgramContract;
var
  LCount: Integer;
begin
  AdmitName(AName);
  Require(AType.Defined and (AType.Kind <> npkVoid), 'Program ports need a non-Void type');
  Result := Copy;
  LCount := Length(FPorts);
  SetLength(Result.FPorts, LCount + 1);
  Result.FPorts[LCount].Name := AName;
  Result.FPorts[LCount].ValueType := AType;
  Result.FPorts[LCount].Direction := ADirection;
end;

function TNyxProgramContract.Input(const AName: TNyxProgramSymbolRef;
  const AType: TNyxProgramType): TNyxProgramContract;
begin
  Result := AddPort(AName, AType, npvInput);
end;

function TNyxProgramContract.Output(const AName: TNyxProgramSymbolRef;
  const AType: TNyxProgramType): TNyxProgramContract;
begin
  Result := AddPort(AName, AType, npvOutput);
end;

function TNyxProgramContract.WithRoutine(const ARoutine: TNyxProgramRoutine): TNyxProgramContract;
var
  LCount: Integer;
begin
  ARoutine.Validate;
  AdmitName(ARoutine.Name);
  Result := Copy;
  LCount := Length(FRoutines);
  SetLength(Result.FRoutines, LCount + 1);
  Result.FRoutines[LCount] := ARoutine.Copy;
end;

function TNyxProgramContract.Copy: TNyxProgramContract;
var
  LIndex: Integer;
begin
  Result := Default(TNyxProgramContract);
  Result.FDefined := FDefined;
  SetLength(Result.FPorts, Length(FPorts));
  for LIndex := 0 to High(FPorts) do
  begin
    Result.FPorts[LIndex] := FPorts[LIndex];
  end;
  SetLength(Result.FRoutines, Length(FRoutines));
  for LIndex := 0 to High(FRoutines) do
  begin
    Result.FRoutines[LIndex] := FRoutines[LIndex].Copy;
  end;
end;

function TNyxProgramContract.Port(AIndex: Integer): TNyxProgramPort;
begin
  CheckIndex(AIndex, Length(FPorts));
  Result := FPorts[AIndex];
end;

function TNyxProgramContract.PortCount: Integer;
begin
  Result := Length(FPorts);
end;

function TNyxProgramContract.Routine(AIndex: Integer): TNyxProgramRoutine;
begin
  CheckIndex(AIndex, Length(FRoutines));
  Result := FRoutines[AIndex].Copy;
end;

function TNyxProgramContract.RoutineCount: Integer;
begin
  Result := Length(FRoutines);
end;

procedure TNyxProgramContract.Validate;
var
  LIndex: Integer;
begin
  Require(FDefined, 'An uninitialized program contract refuses');
  for LIndex := 0 to High(FRoutines) do
  begin
    FRoutines[LIndex].Validate;
  end;
end;

procedure TNyxProgramContract.ValidateLimits(const ALimits: TNyxProgramLimits);
var
  LIndex: Integer;
begin
  ALimits.Validate;
  Require(FDefined, 'An uninitialized program contract refuses');
  RequireBudget(Length(FPorts) <= ALimits.MaximumSymbols, 'Program ports exceed the symbol budget');
  RequireBudget(Length(FRoutines) <= ALimits.MaximumRoutines, 'Host routine vocabulary exceeds its budget');
  for LIndex := 0 to High(FRoutines) do
  begin
    RequireBudget(FRoutines[LIndex].ParameterCount <= ALimits.MaximumParameters,
      'Host parameter vocabulary exceeds its budget');
  end;
  Validate;
end;

function TNyxProgramContract.ToData: TNyxDataValue;
var
  LPorts: array of TNyxDataValue;
  LRoutines: array of TNyxDataValue;
  LIndex: Integer;
begin
  Validate;
  SetLength(LPorts, Length(FPorts));
  SetLength(LRoutines, Length(FRoutines));
  for LIndex := 0 to High(FPorts) do
  begin
    LPorts[LIndex] := NyxObject([NyxField('name', NyxData(FPorts[LIndex].Name.Name)),
      NyxField('type', FPorts[LIndex].ValueType.ToData),
      NyxField('direction', NyxData(Ord(FPorts[LIndex].Direction)))]);
  end;
  for LIndex := 0 to High(FRoutines) do
  begin
    LRoutines[LIndex] := FRoutines[LIndex].ToData;
  end;
  Result := NyxObject([NyxField('version', NyxData(1)), NyxField('ports', NyxArray(LPorts)),
    NyxField('routines', NyxArray(LRoutines))]);
end;

class function TNyxProgramContract.FromData(const AData: TNyxDataValue): TNyxProgramContract;
begin
  Result := FromData(AData, NyxProgramLimits);
end;

class function TNyxProgramContract.FromData(const AData: TNyxDataValue;
  const ALimits: TNyxProgramLimits): TNyxProgramContract;
var
  LItems: TNyxDataValue;
  LRoutines: TNyxDataValue;
  LItem: TNyxDataValue;
  LType: TNyxProgramType;
  LName: TNyxProgramSymbolRef;
  LIndex: Integer;
  LNames: TNyxTextIndex;
  LKey: TNyxText;
begin
  ALimits.Validate;
  ValidateNyxFlowFields(AData, ['version', 'ports', 'routines']);
  Require(AData.Field('version').AsInteger = 1, 'Unsupported program contract version');
  LItems := AData.Field('ports');
  LRoutines := AData.Field('routines');
  Require(LItems.Kind = ndArray, 'Program ports require an array');
  Require(LRoutines.Kind = ndArray, 'Program routines require an array');
  RequireBudget(LItems.Count <= ALimits.MaximumSymbols, 'Program ports exceed the symbol budget');
  RequireBudget(LRoutines.Count <= ALimits.MaximumRoutines, 'Host routine vocabulary exceeds its budget');
  Result := NyxProgramContract;
  SetLength(Result.FPorts, LItems.Count);
  SetLength(Result.FRoutines, LRoutines.Count);
  LNames := TNyxTextIndex.Create;
  try
    for LIndex := 0 to LItems.Count - 1 do
    begin
      LItem := LItems.Item(LIndex);
      ValidateNyxFlowFields(LItem, ['name', 'type', 'direction']);
      LName := NyxProgramSymbol(LItem.Field('name').AsText);
      LType := TNyxProgramType.FromData(LItem.Field('type'));
      Require(LType.Kind <> npkVoid, 'Program ports need a non-Void type');
      LKey := NyxProgramIdentifierKey(LName.Name);
      Require(LNames.IndexOf(LKey) < 0, 'Duplicate program boundary symbol');
      LNames.AddFirst(LKey, LIndex);
      Result.FPorts[LIndex].Name := LName;
      Result.FPorts[LIndex].ValueType := LType;
      case LItem.Field('direction').AsInteger of
        0: Result.FPorts[LIndex].Direction := npvInput;
        1: Result.FPorts[LIndex].Direction := npvOutput;
        else Require(False, 'Unknown program port direction');
      end;
    end;
    for LIndex := 0 to LRoutines.Count - 1 do
    begin
      Result.FRoutines[LIndex] := TNyxProgramRoutine.FromData(
        LRoutines.Item(LIndex), ALimits.MaximumParameters);
      LKey := NyxProgramIdentifierKey(Result.FRoutines[LIndex].Name.Name);
      Require(LNames.IndexOf(LKey) < 0, 'Duplicate program boundary symbol');
      { The index owns only membership; original vectors retain declaration order. }
      LNames.AddFirst(LKey, LIndex);
    end;
  finally
    LNames.Free;
  end;
end;

function NyxProgramGrants: TNyxProgramGrants;
begin
  Result := Default(TNyxProgramGrants);
  Result.FDefined := True;
end;

function TNyxProgramGrants.Contains(const ACapability: TNyxProgramCapabilityRef): Boolean;
var
  LIndex: Integer;
begin
  Require(FDefined and ACapability.Defined, 'Capability checks require initialized grants and reference');
  for LIndex := 0 to High(FItems) do
  begin

    if FItems[LIndex].Same(ACapability) then
    begin
      Exit(True);
    end;
  end;
  Result := False;
end;

function TNyxProgramGrants.Allow(const ACapability: TNyxProgramCapabilityRef): TNyxProgramGrants;
var
  LIndex: Integer;
  LCount: Integer;
begin
  if Contains(ACapability) then
  begin
    Exit(Self);
  end;
  Result := NyxProgramGrants;
  LCount := Length(FItems) + 1;
  SetLength(Result.FItems, LCount);
  for LIndex := 0 to High(FItems) do
  begin
    Result.FItems[LIndex] := FItems[LIndex];
  end;
  Result.FItems[LCount - 1] := ACapability;
end;

function TNyxProgramGrants.Count: Integer;
begin
  Require(FDefined, 'Uninitialized program grants refuse');
  Result := Length(FItems);
end;

function TNyxProgramGrants.Item(AIndex: Integer): TNyxProgramCapabilityRef;
begin
  CheckIndex(AIndex, Length(FItems));
  Result := FItems[AIndex];
end;


function NyxProgramArguments(const AValues: array of INyxProgramValue): INyxProgramArguments;
var
  LValue: TFrozenArguments;
  LIndex: Integer;
begin
  LValue := TFrozenArguments.Create;
  Result := LValue;
  SetLength(LValue.FItems, Length(AValues));
  for LIndex := 0 to High(AValues) do
  begin
    Require(AValues[LIndex] <> nil, 'Undefined program argument');
    LValue.FItems[LIndex] := AValues[LIndex];
  end;
end;

function TFrozenArguments.Count: Integer;
begin
  Result := Length(FItems);
end;

function TFrozenArguments.Item(AIndex: Integer): INyxProgramValue;
begin
  CheckIndex(AIndex, Length(FItems));
  Result := FItems[AIndex];
end;

function TFrozenArguments.Copy: INyxProgramArguments;
begin
  Result := Self;
end;

function NyxProgramValues: INyxProgramValues;
begin
  Result := TFrozenValues.Create;
end;

function NyxProgramValues(const ANames: array of TNyxProgramSymbolRef;
  const AValues: array of INyxProgramValue): INyxProgramValues;
var
  LValue: TFrozenValues;
  LNames: TNyxTextIndex;
  LIndex: Integer;
  LKey: TNyxText;
begin
  Require(Length(ANames) = Length(AValues), 'Program names and values must have equal counts');
  LValue := TFrozenValues.Create;
  Result := LValue;
  LNames := TNyxTextIndex.Create;
  try
    SetLength(LValue.FNames, Length(ANames));
    SetLength(LValue.FItems, Length(ANames));
    for LIndex := 0 to High(ANames) do
    begin
      LKey := NyxProgramIdentifierKey(ANames[LIndex].Name);
      Require((AValues[LIndex] <> nil) and (LNames.IndexOf(LKey) < 0),
        'Program frames require unique defined values');
      LNames.AddFirst(LKey, LIndex);
      LValue.FNames[LIndex] := ANames[LIndex];
      LValue.FItems[LIndex] := AValues[LIndex];
    end;
  finally
    LNames.Free;
  end;
end;

function TFrozenValues.Copy: INyxProgramValues;
begin
  Result := Self;
end;

function TFrozenValues.Has(const AName: TNyxProgramSymbolRef): Boolean;
var
  LIndex: Integer;
begin
  Require(AName.Defined, 'Program names must be initialized');
  for LIndex := 0 to High(FNames) do
  begin

    if FNames[LIndex].Same(AName) then
    begin
      Exit(True);
    end;
  end;
  Result := False;
end;

function TFrozenValues.WithValue(const AName: TNyxProgramSymbolRef;
  const AValue: INyxProgramValue): INyxProgramValues;
var
  LValue: TFrozenValues;
  LCount: Integer;
  LIndex: Integer;
begin
  Require((AValue <> nil) and not Has(AName), 'Program frames require unique defined values');
  LValue := TFrozenValues.Create;
  Result := LValue;
  LCount := Length(FItems);
  SetLength(LValue.FItems, LCount + 1);
  SetLength(LValue.FNames, LCount + 1);
  for LIndex := 0 to LCount - 1 do
  begin
    LValue.FItems[LIndex] := FItems[LIndex];
    LValue.FNames[LIndex] := FNames[LIndex];
  end;
  LValue.FNames[LCount] := AName;
  LValue.FItems[LCount] := AValue;
end;

function TFrozenValues.Value(const AName: TNyxProgramSymbolRef): INyxProgramValue;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FItems) do
  begin

    if FNames[LIndex].Same(AName) then
    begin
      Exit(FItems[LIndex]);
    end;
  end;
  Require(False, 'Missing program boundary value: ' + AName.Name);
  Result := nil;
end;

function TFrozenValues.BindingName(AIndex: Integer): TNyxProgramSymbolRef;
begin
  CheckIndex(AIndex, Length(FNames));
  Result := FNames[AIndex];
end;

function TFrozenValues.Item(AIndex: Integer): INyxProgramValue;
begin
  CheckIndex(AIndex, Length(FItems));
  Result := FItems[AIndex];
end;

function TFrozenValues.Count: Integer;
begin
  Result := Length(FItems);
end;

function NyxProgramProposal(const ARoutine: TNyxProgramRoutine;
  const AArguments: INyxProgramArguments; const ASpan: TNyxProgramSpan): INyxProgramProposal;
var
  LValue: TFrozenProposal;
  LIndex: Integer;
begin
  ARoutine.Validate;
  Require(AArguments <> nil, 'Proposed calls need an owned argument frame');
  Require((ARoutine.Kind = nprProposal) and (AArguments.Count = ARoutine.ParameterCount),
    'Proposed calls require their exact declared proposal signature');
  for LIndex := 0 to AArguments.Count - 1 do
  begin
    Require(AArguments.Item(LIndex).ValueType.Same(ARoutine.Parameter(LIndex).ValueType),
      'Proposed call argument has the wrong type');
  end;
  LValue := TFrozenProposal.Create;
  Result := LValue;
  LValue.FRoutine := ARoutine.Copy;
  LValue.FArguments := AArguments;
  LValue.FSpan := ASpan;
end;

function TFrozenProposal.Routine: TNyxProgramRoutine;
begin
  Result := FRoutine.Copy;
end;

function TFrozenProposal.Arguments: INyxProgramArguments;
begin
  Result := FArguments;
end;

function TFrozenProposal.GetSpan: TNyxProgramSpan;
begin
  Result := FSpan;
end;

function NyxProgramRun: INyxProgramRunOptions;
var
  LValue: TProgramOptions;
begin
  LValue := TProgramOptions.Create;
  Result := LValue;
  LValue.FGrants := NyxProgramGrants;
end;

function TProgramOptions.Clone: TProgramOptions;
begin
  Result := TProgramOptions.Create;
  Result.FGrants := FGrants;
  Result.FQueries := FQueries;
  Result.FScope := FScope;
  Result.FClock := FClock;
end;

procedure TProgramOptions.Validate;
begin
  Require(FGrants.Defined, 'Program runtime grants must be initialized');
end;

function TProgramOptions.GetPermissions: TNyxProgramGrants;
begin
  Result := FGrants;
end;

function TProgramOptions.GetQuerySnapshot: INyxProgramQueries;
begin
  Result := FQueries;
end;

function TProgramOptions.GetCancellationScope: INyxCancellationScope;
begin
  Result := FScope;
end;

function TProgramOptions.GetTimeSource: INyxProgramClock;
begin
  Result := FClock;
end;

function TProgramOptions.Grants(const AGrants: TNyxProgramGrants): INyxProgramRunOptions;
var
  LValue: TProgramOptions;
begin
  Require(AGrants.Defined, 'Program grants must be initialized');
  LValue := Clone;
  Result := LValue;
  LValue.FGrants := AGrants;
end;

function TProgramOptions.Queries(const AQueries: INyxProgramQueries): INyxProgramRunOptions;
var
  LValue: TProgramOptions;
begin
  LValue := Clone;
  Result := LValue;
  LValue.FQueries := AQueries;
end;

function TProgramOptions.Cancellation(const AScope: INyxCancellationScope): INyxProgramRunOptions;
var
  LValue: TProgramOptions;
begin
  LValue := Clone;
  Result := LValue;
  LValue.FScope := AScope;
end;

function TProgramOptions.Clock(const AClock: INyxProgramClock): INyxProgramRunOptions;
var
  LValue: TProgramOptions;
begin
  LValue := Clone;
  Result := LValue;
  LValue.FClock := AClock;
end;

end.
