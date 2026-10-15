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

unit nyx.logic.programs;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.data, nyx.logic, nyx.flow, nyx.flow.types,
  nyx.flow.declarations, nyx.programs.types, nyx.programs.contract,
  nyx.programs.declarations, nyx.scheduler;

const
  NyxProgramLogicKind = 'pascal-program';

type
  { Program Number is finite Double; graph Number retains exact decimal data.
    PreserveDecimal checks the complete decimal value through that projection,
    including exponent/trailing-zero equivalence. Approximate explicitly permits
    precision loss. Neither policy changes untouched JSON/byte payloads. }
  TNyxProgramNumberConversion = (npncPreserveDecimal, npncApproximate);

  { Specialized immutable named-program operation. The contract overload is an
    inert declaration for authoring/import; the definition overload compiles an
    independently owned pure implementation. Numbers derives a separate recipe.
    No program name, imported contract or graph grants host access. Scalar/JSON/
    byte ports map exactly; opaque handles require an owning scene adapter.
    Each invocation owns its frames and shares only immutable compiled code. }
  INyxProgramLogic = interface(INyxLogicOperation)
    ['{C6794FAD-7D87-4345-B8C1-B0D90DC348A7}']
    function GetProgramReference: TNyxProgramRef;
    function GetContract: TNyxProgramContract;
    function GetNumberConversion: TNyxProgramNumberConversion;
    function Numbers(AConversion: TNyxProgramNumberConversion): INyxProgramLogic;
    property ProgramReference: TNyxProgramRef read GetProgramReference;
    property Contract: TNyxProgramContract read GetContract;
    property NumberConversion: TNyxProgramNumberConversion read GetNumberConversion;
  end;

{ Inspection only; valid scene-handle ports return False rather than acquiring
  scene authority. Invalid contract metadata still raises its ordinary error. }
function NyxProgramLogicContractSupported(const AContract: TNyxProgramContract): Boolean;
{ Copy a typed reference/signature as inert authoring intent. Invalid metadata
  raises ENyxFlow/its contract error; no definition is compiled or executed. }
function NewNyxProgramLogic(const AReference: TNyxProgramRef;
  const AContract: TNyxProgramContract): INyxProgramLogic; overload;
{ Normalize the definition and compile its exact source into independently owned
  immutable code. Program diagnostics propagate; required capabilities/proposals
  refuse this pure adapter. Invocation still requires a separate explicit call. }
function NewNyxProgramLogic(const AReference: TNyxProgramRef;
  const ADefinition: INyxProgramDefinition): INyxProgramLogic; overload;
{ Strict persistence boundary reconstructs inert declarations only. }
function NyxProgramLogicFromData(const AData: TNyxDataValue): INyxProgramLogic;
{ Capture independent program membership and chain existing explicit host
  authority. Resolution checks the exact saved contract and compiles each resolved
  step during graph construction, granting no query/proposal capabilities. It owns no
  document/application and remains safe after their retirement. }
function NewNyxProgramLogicResolver(const APrograms: INyxProgramDeclarations;
  const ANext: INyxLogicResolver = nil): INyxLogicResolver;
{ Complete candidate validation uses metadata only, never Compile/Run or a
  creator resolver. Missing programs and changed signatures refuse atomically. }
procedure ValidateNyxProgramLogicReferences(const APrograms: INyxProgramDeclarations;
  const AFlows: INyxFlowDeclarations);

implementation

uses SysUtils, nyx.programs;

type
  { Owned immutable declaration or compiled pure operation. A nil Code is
    deliberate authoring data; Invoke refuses before returning any output. }
  TProgramLogic = class(TNyxLogicOperation, INyxProgramLogic)
  private
    FReference: TNyxProgramRef;
    FContract: TNyxProgramContract;
    FCode: INyxProgram;
    FNumbers: TNyxProgramNumberConversion;
  protected
    function Execute(const AInputs: TNyxFlowValues;
      const ACancellation: INyxCancellationScope): TNyxFlowValues; override;
  public
    constructor Create(const AReference: TNyxProgramRef;
      const AContract: TNyxProgramContract; const ACode: INyxProgram;
      ANumbers: TNyxProgramNumberConversion);
    function GetProgramReference: TNyxProgramRef;
    function GetContract: TNyxProgramContract;
    function GetNumberConversion: TNyxProgramNumberConversion;
    function Numbers(AConversion: TNyxProgramNumberConversion): INyxProgramLogic;
  end;

  { Membership is cloned at attachment; compiled operations never retain this
    resolver. Next is explicit runtime authority, independently kept alive through
    callouts. Neither import nor descriptor discovery constructs this resolver. }
  TProgramResolver = class(TInterfacedObject, INyxLogicResolver)
  public
    Programs: INyxProgramDeclarations;
    Next: INyxLogicResolver;
    function Resolve(const AKind: TNyxLogicKindRef;
      const AConfiguration: TNyxDataValue): INyxLogicOperation;
  end;

procedure Require(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxFlow.Create(AMessage);
  end;
end;

function NyxProgramLogicContractSupported(const AContract: TNyxProgramContract): Boolean;
var
  LIndex: Integer;
begin
  AContract.Validate;
  Result := True;
  for LIndex := 0 to AContract.PortCount - 1 do
  begin

    if AContract.Port(LIndex).ValueType.Kind in [npkVoid, npkHandle] then
    begin
      Exit(False);
    end;
  end;
end;

function FlowType(AKind: TNyxProgramKind): TNyxFlowType;
begin
  case AKind of
    npkText: Result := nftText;
    npkBoolean: Result := nftBoolean;
    npkInteger: Result := nftInteger;
    npkNumber: Result := nftNumber;
    npkJSON: Result := nftJSON;
    npkBytes: Result := nftBytes;
  else
    raise ENyxFlow.Create('Opaque program ports require their owning host adapter');
  end;
end;

constructor TProgramLogic.Create(const AReference: TNyxProgramRef;
  const AContract: TNyxProgramContract; const ACode: INyxProgram;
  ANumbers: TNyxProgramNumberConversion);
var
  LIndex: Integer;
  LPort: TNyxProgramPort;
begin
  inherited Create;
  FReference := NyxProgramRef(AReference.Name);
  FContract := AContract.Copy;
  Require(NyxProgramLogicContractSupported(FContract),
    'Opaque program ports require their owning host adapter');
  Require((Ord(ANumbers) >= Ord(Low(TNyxProgramNumberConversion))) and
    (Ord(ANumbers) <= Ord(High(TNyxProgramNumberConversion))),
    'Unknown program number conversion');
  FNumbers := ANumbers;
  FCode := ACode;
  FConfiguration := NyxObject([
    NyxField('version', NyxData(1)), NyxField('program', NyxData(FReference.Name)),
    NyxField('contract', FContract.ToData), NyxField('numbers', NyxData(Ord(FNumbers)))]);
  FDescriptor := NyxLogicDescriptor(NyxLogicKind(NyxProgramLogicKind),
    'Pascal program: ' + FReference.Name,
    'Run a named typed Pascal program with owned inputs and complete graph outputs.',
    nlgFlow).Labelled('pascal').Labelled('script').Labelled('program');
  for LIndex := 0 to FContract.PortCount - 1 do
  begin
    LPort := FContract.Port(LIndex);

    if LPort.Direction = npvInput then
    begin
      FDescriptor := FDescriptor.Input(NyxFlowPort(LPort.Name.Name),
        FlowType(LPort.ValueType.Kind), 'Program input: ' + LPort.ValueType.DisplayName);
    end
    else
    begin
      FDescriptor := FDescriptor.Output(NyxFlowPort(LPort.Name.Name),
        FlowType(LPort.ValueType.Kind), 'Program output: ' + LPort.ValueType.DisplayName);
    end;
  end;
end;

function NewNyxProgramLogic(const AReference: TNyxProgramRef;
  const AContract: TNyxProgramContract): INyxProgramLogic;
begin
  Result := TProgramLogic.Create(AReference, AContract, nil, npncPreserveDecimal);
end;

function NewNyxProgramLogic(const AReference: TNyxProgramRef;
  const ADefinition: INyxProgramDefinition): INyxProgramLogic;
var
  LReference: TNyxProgramRef;
  LDefinition: INyxProgramDefinition;
  LCode: INyxProgram;
begin
  LReference := NyxProgramRef(AReference.Name);
  LDefinition := ADefinition;
  Require(LDefinition <> nil, 'Program Logic requires an explicit definition');
  LDefinition := NyxProgramDefinitionFromData(LDefinition.ToData);
  LCode := LDefinition.Compile;
  Require(not LCode.MayPropose and (LCode.CapabilityCount = 0),
    'This program requires explicit owning host/query authority, unavailable in pure Logic');
  Result := TProgramLogic.Create(LReference, LCode.Contract, LCode, npncPreserveDecimal);
end;

function TProgramLogic.GetProgramReference: TNyxProgramRef;
begin
  Result := FReference;
end;

function TProgramLogic.GetContract: TNyxProgramContract;
begin
  Result := FContract.Copy;
end;

function TProgramLogic.GetNumberConversion: TNyxProgramNumberConversion;
begin
  Result := FNumbers;
end;

function TProgramLogic.Numbers(AConversion: TNyxProgramNumberConversion): INyxProgramLogic;
var
  LKeepAlive: INyxProgramLogic;
begin
  LKeepAlive := Self;
  Result := TProgramLogic.Create(FReference, FContract, FCode, AConversion);
end;

{ Canonical finite-decimal comparison for the explicit Double input projection.
  It never converts a decimal through binary arithmetic for equality. Digits stay
  owned text; exponent arithmetic is bounded by source length plus the finite
  Double domain. Nonzero values with larger exponents cannot round-trip. }
function CanonicalNumber(const AText: TNyxText): TNyxText;
var
  LText: TNyxText;
  LSign: TNyxText;
  LDigits: TNyxText;
  LExponent: Integer;
  LFraction: Integer;
  LPosition: Integer;
  LTrailing: Integer;
  LEnd: Integer;
begin
  LText := AText;
  LSign := '';

  if LText[1] = '-' then
  begin
    LSign := '-';
    Delete(LText, 1, 1);
  end;
  LPosition := Pos('e', LText);

  if LPosition = 0 then
  begin
    LPosition := Pos('E', LText);
  end;
  LExponent := 0;

  if LPosition > 0 then
  begin

    if not TryStrToInt(Copy(LText, LPosition + 1, MaxInt), LExponent) then
    begin
      Exit('');
    end;
    LText := Copy(LText, 1, LPosition - 1);
  end;
  LPosition := Pos('.', LText);
  LFraction := 0;

  if LPosition > 0 then
  begin
    LFraction := Length(LText) - LPosition;
    Delete(LText, LPosition, 1);
  end;
  LPosition := 1;
  while (LPosition < Length(LText)) and (LText[LPosition] = '0') do
  begin
    Inc(LPosition);
  end;
  LDigits := Copy(LText, LPosition, MaxInt);

  if LDigits = '0' then
  begin
    Exit('0');
  end;

  if (LExponent < -Length(AText) - 1000) or (LExponent > Length(AText) + 1000) then
  begin
    Exit('');
  end;
  LEnd := Length(LDigits);
  while (LEnd > 1) and (LDigits[LEnd] = '0') do
  begin
    Dec(LEnd);
  end;
  LTrailing := Length(LDigits) - LEnd;
  Result := LSign + Copy(LDigits, 1, LEnd) + 'e' +
    TNyxText(IntToStr(LExponent - LFraction + LTrailing));
end;

function ProgramInput(const AValue: TNyxFlowValue;
  ANumbers: TNyxProgramNumberConversion): INyxProgramValue;
var
  LNumber: Double;
  LProjected: TNyxText;
  LOriginal: TNyxText;
begin
  case AValue.ValueType of
    nftText: Result := NyxProgramValue(AValue.AsText);
    nftBoolean: Result := NyxProgramValue(AValue.AsBoolean);
    nftInteger: Result := NyxProgramValue(AValue.AsInteger);
    nftJSON: Result := NyxProgramValue(AValue.AsJSON);
    nftBytes: Result := NyxProgramValue(AValue.AsBytes);
    nftNumber:
    begin
      LNumber := NyxData(AValue.AsNumber).AsNumber;

      if ANumbers = npncPreserveDecimal then
      begin
        LOriginal := CanonicalNumber(AValue.AsNumber.Text);
        LProjected := CanonicalNumber(NyxData(LNumber).ToJSON);
        Require((LOriginal <> '') and (LOriginal = LProjected),
          'Decimal input loses precision as Double; select approximate conversion explicitly');
      end;
      Result := NyxProgramValue(LNumber);
    end;
  else
    raise ENyxFlow.Create('Program input does not accept an invocation Signal');
  end;
end;

function FlowOutput(const AValue: INyxProgramValue): TNyxFlowValue;
begin
  case AValue.ValueType.Kind of
    npkText: Result := NyxFlowText(AValue.AsText);
    npkBoolean: Result := NyxFlowBoolean(AValue.AsBoolean);
    npkInteger: Result := NyxFlowInteger(AValue.AsInteger);
    npkNumber: Result := NyxFlowNumber(NyxData(AValue.AsNumber).AsDecimal);
    npkJSON: Result := NyxFlowJSON(AValue.AsJSON);
    npkBytes: Result := NyxFlowBytes(AValue.AsBytes);
  else
    raise ENyxFlow.Create('Program output needs its owning host adapter');
  end;
end;

function TProgramLogic.Execute(const AInputs: TNyxFlowValues;
  const ACancellation: INyxCancellationScope): TNyxFlowValues;
var
  LInputs: INyxProgramValues;
  LResult: INyxProgramResult;
  LPort: TNyxProgramPort;
  LIndex: Integer;
begin
  Require(FCode <> nil, 'Saved program intent requires its explicit runtime program resolver');
  LInputs := NyxProgramValues;
  for LIndex := 0 to FContract.PortCount - 1 do
  begin
    LPort := FContract.Port(LIndex);

    if LPort.Direction = npvInput then
    begin
      LInputs := LInputs.WithValue(LPort.Name,
        ProgramInput(AInputs.Value(NyxFlowPort(LPort.Name.Name)), FNumbers));
    end;
  end;
  try
    LResult := FCode.Run(LInputs, NyxProgramRun.Cancellation(ACancellation));
  except
    on E: ENyxProgram do
    begin

      if E.Diagnostic.Kind = npdCancelled then
      begin
        raise ENyxFlowCancelled.Create('Pascal program invocation cancelled');
      end;
      raise;
    end;
  end;
  Require(LResult.ProposalCount = 0, 'Program host proposals need their owning transaction adapter');
  Result := NyxFlowValues;
  for LIndex := 0 to FContract.PortCount - 1 do
  begin
    LPort := FContract.Port(LIndex);

    if LPort.Direction = npvOutput then
    begin
      Result := Result.WithValue(NyxFlowPort(LPort.Name.Name),
        FlowOutput(LResult.Outputs.Value(LPort.Name)));
    end;
  end;
end;

function NyxProgramLogicFromData(const AData: TNyxDataValue): INyxProgramLogic;
var
  LNumbers: Integer;
begin
  ValidateNyxFlowFields(AData, ['version', 'program', 'contract', 'numbers']);
  Require(AData.Field('version').AsInteger = 1, 'Unknown program Logic version');
  LNumbers := AData.Field('numbers').AsInteger;
  Require((LNumbers >= Ord(Low(TNyxProgramNumberConversion))) and
    (LNumbers <= Ord(High(TNyxProgramNumberConversion))), 'Unknown program number conversion');
  Result := NewNyxProgramLogic(NyxProgramRef(AData.Field('program').AsText),
    TNyxProgramContract.FromData(AData.Field('contract')))
    .Numbers(TNyxProgramNumberConversion(LNumbers));
end;

function NewNyxProgramLogicResolver(const APrograms: INyxProgramDeclarations;
  const ANext: INyxLogicResolver): INyxLogicResolver;
var
  LPrograms: INyxProgramDeclarations;
  LNext: INyxLogicResolver;
  LResolver: TProgramResolver;
begin
  LPrograms := APrograms;
  LNext := ANext;
  Require(LPrograms <> nil, 'Program Logic resolver requires explicit declarations');
  LResolver := TProgramResolver.Create;
  Result := LResolver;
  LResolver.Programs := LPrograms.Clone;
  Require(LResolver.Programs <> nil, 'Program declarations did not supply independent membership');
  LResolver.Next := LNext;
end;

function TProgramResolver.Resolve(const AKind: TNyxLogicKindRef;
  const AConfiguration: TNyxDataValue): INyxLogicOperation;
var
  LKeepAlive: INyxLogicResolver;
  LIntent: INyxProgramLogic;
  LDefinition: INyxProgramDefinition;
begin
  LKeepAlive := Self;

  if AKind.Name <> NyxProgramLogicKind then
  begin

    if Next <> nil then
    begin
      Exit(Next.Resolve(AKind, AConfiguration));
    end;
    Exit(NyxLogicFromData(AKind, AConfiguration));
  end;
  LIntent := NyxProgramLogicFromData(AConfiguration);
  LDefinition := Programs.Definition(LIntent.ProgramReference);
  Require(LDefinition.Contract.ToData.ToJSON = LIntent.Contract.ToData.ToJSON,
    'Saved program Logic contract differs from its named definition');
  Result := NewNyxProgramLogic(LIntent.ProgramReference, LDefinition)
    .Numbers(LIntent.NumberConversion);
end;

procedure ValidateNyxProgramLogicReferences(const APrograms: INyxProgramDeclarations;
  const AFlows: INyxFlowDeclarations);
var
  LPrograms: INyxProgramDeclarations;
  LFlows: INyxFlowDeclarations;
  LGraph: INyxFlowDefinition;
  LOperation: INyxProgramLogic;
  LDefinition: INyxProgramDefinition;
  LIndex: Integer;
  LStep: Integer;
begin
  LPrograms := APrograms;
  LFlows := AFlows;
  Require((LPrograms <> nil) and (LFlows <> nil),
    'Program reference validation requires both owned registries');
  for LIndex := 0 to LFlows.Count - 1 do
  begin
    LGraph := LFlows.Definition(LFlows.Reference(LIndex));
    for LStep := 0 to LGraph.StepCount - 1 do
    begin

      if LGraph.Descriptor(LStep).Kind.Name <> NyxProgramLogicKind then
      begin
        Continue;
      end;
      LOperation := NyxProgramLogicFromData(LGraph.Configuration(LStep));
      Require(LOperation.Descriptor.ToData.ToJSON = LGraph.Descriptor(LStep).ToData.ToJSON,
        'Saved program Logic descriptor differs from its typed configuration');
      LDefinition := LPrograms.Definition(LOperation.ProgramReference);
      Require(LDefinition.Contract.ToData.ToJSON = LOperation.Contract.ToData.ToJSON,
        'Saved program Logic contract differs from its named definition');
    end;
  end;
end;

end.
