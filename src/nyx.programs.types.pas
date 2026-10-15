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



unit nyx.programs.types;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.state;

type
  { Closed portable value families. Handles are opaque capabilities with a
    separately declared Pascal type name; ordinary text never becomes a handle.
    Void is a defined procedure result, distinct from an uninitialized value. }
  TNyxProgramKind = (npkVoid, npkBoolean, npkInteger, npkNumber, npkText,
    npkJSON, npkBytes, npkHandle);

  { Case-insensitive ASCII Pascal identifier, retaining its authored spelling.
    These names select declared symbols, never files, units or host authority.
    Unicode remains unrestricted in admitted string/data values. }
  TNyxProgramSymbolRef = record
  private
    FName: TNyxText;
  public
    property Name: TNyxText read FName;
    function Defined: Boolean;
    function Same(const AOther: TNyxProgramSymbolRef): Boolean;
  end;

  { Exact creator capability identity. Built-in names have a closed enum helper;
    extensions use distinct validated references. Merely spelling a capability
    in source or metadata cannot grant it to an invocation. }
  TNyxProgramCapabilityRef = record
  private
    FName: TNyxText;
  public
    property Name: TNyxText read FName;
    function Defined: Boolean;
    function Same(const AOther: TNyxProgramCapabilityRef): Boolean;
  end;

  { Shared host capability vocabulary. A grant remains scoped by the injected
    snapshot/adapter; SceneEdit does not imply access to every application scene. }
  TNyxProgramCapability = (npcStateRead, npcStateWrite, npcSceneRead,
    npcSceneEdit, npcCameraRead, npcCameraEdit, npcResourceRead);

  { Exact scalar family or opaque handle type. A default record is undefined.
    Handle names compare as Pascal identifiers; their object identity remains
    private to the explicit host implementation, never recoverable from text. }
  TNyxProgramType = record
  private
    FDefined: Boolean;
    FKind: TNyxProgramKind;
    FName: TNyxProgramSymbolRef;
  public
    property Defined: Boolean read FDefined;
    property Kind: TNyxProgramKind read FKind;
    property HandleName: TNyxProgramSymbolRef read FName;
    function Same(const AOther: TNyxProgramType): Boolean;
    function DisplayName: TNyxText;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxProgramType; static;
  end;

  { Host-owned opaque value. Implementations expose only a declared type through
    this interface and must use immutable or revocable worker-safe state.
    Application adapters must not retain live document/view ownership cycles.
    The interpreter can copy/pass a handle but cannot fabricate or dereference it. }
  INyxProgramHandle = interface(IInterface)
    ['{9D69BC4B-085E-45F1-A9B0-000000000101}']
    function GetValueType: TNyxProgramType;
    property ValueType: TNyxProgramType read GetValueType;
  end;

  { Managed immutable runtime value; nil is unassigned. Scalar semantics reuse
    signed 32-bit Nyx state, finite Double and exact strict Unicode. JSON/byte
    getters detach their storage; Copy retains this immutable value. Opaque
    handles retain only their explicit managed capability, never fabricated text. }
  INyxProgramValue = interface(IInterface)
    ['{9D69BC4B-085E-45F1-A9B0-000000000107}']
    function GetValueType: TNyxProgramType;
    function Defined: Boolean;
    function AsBoolean: Boolean;
    function AsInteger: Integer;
    function AsNumber: Double;
    function AsText: TNyxText;
    function AsJSON: TNyxDataValue;
    function AsBytes: TNyxBytes;
    function AsHandle: INyxProgramHandle;
    function AsState: TNyxStateValue;
    function Copy: INyxProgramValue;
    { Conservative owned payload/header bytes, not the host's exact heap size. }
    function ByteSize: TNyxTextBytes;
    property ValueType: TNyxProgramType read GetValueType;
  end;

  { Immutable zero-based UTF-8 byte range, independent of target storage units.
    Line/column are one-based scalar coordinates; CRLF is one line break.
    An EOF diagnostic may have zero byte length. Default spans are undefined. }
  TNyxProgramSpan = record
  private
    FDefined: Boolean;
    FStart: Integer;
    FCount: Integer;
    FLine: Integer;
    FColumn: Integer;
  public
    property Defined: Boolean read FDefined;
    property Start: Integer read FStart;
    property Count: Integer read FCount;
    property Line: Integer read FLine;
    property Column: Integer read FColumn;
  end;

  { Closed diagnostic category. Rejected syntax/types/capabilities, bounded-work
    exhaustion and cancellation remain distinct from successful program output. }
  TNyxProgramDiagnosticKind = (npdSyntax, npdType, npdName, npdLimit,
    npdCapability, npdRuntime, npdCancelled, npdStale);

  { Detached failure metadata. Source text and owner are not retained here;
    callers associate this span with the exact compiled program/source version. }
  TNyxProgramDiagnostic = record
    Kind: TNyxProgramDiagnosticKind;
    Span: TNyxProgramSpan;
    Message: TNyxText;
  end;

  { Exact portable error with structured metadata. Native Message stores UTF-8
    bytes directly; no system ANSI conversion rewrites creator diagnostics. }
  ENyxProgram = class(Exception)
  private
    FDiagnostic: TNyxProgramDiagnostic;
  public
    constructor Create(AKind: TNyxProgramDiagnosticKind;
      const ASpan: TNyxProgramSpan; const AMessage: TNyxText); reintroduce;
    property Diagnostic: TNyxProgramDiagnostic read FDiagnostic;
  end;

  { Caller-tunable compile/run quotas. Nesting has a physical 256-level parser
    ceiling; all other counts are positive signed Integers. Milliseconds zero
    explicitly disables elapsed-time checking; positive deadlines need an
    injected monotonic clock. Allocation is cumulative conservative retention
    accounting, not a process heap cap. Default records refuse. }
  TNyxProgramLimits = record
  private
    FDefined: Boolean;
    FSource: Integer;
    FSymbols: Integer;
    FCode: Integer;
    FRoutines: Integer;
    FParameters: Integer;
    FNesting: Integer;
    FSteps: Integer;
    FCalls: Integer;
    FStack: Integer;
    FValue: Integer;
    FAllocation: Integer;
    FResult: Integer;
    FProposals: Integer;
    FMilliseconds: Integer;
  public
    function SourceBytes(AValue: Integer): TNyxProgramLimits;
    function Symbols(AValue: Integer): TNyxProgramLimits;
    function Instructions(AValue: Integer): TNyxProgramLimits;
    function Routines(AValue: Integer): TNyxProgramLimits;
    function Parameters(AValue: Integer): TNyxProgramLimits;
    function Nesting(AValue: Integer): TNyxProgramLimits;
    function Steps(AValue: Integer): TNyxProgramLimits;
    function CallDepth(AValue: Integer): TNyxProgramLimits;
    function StackValues(AValue: Integer): TNyxProgramLimits;
    function ValueBytes(AValue: Integer): TNyxProgramLimits;
    function AllocationBytes(AValue: Integer): TNyxProgramLimits;
    function ResultBytes(AValue: Integer): TNyxProgramLimits;
    function Proposals(AValue: Integer): TNyxProgramLimits;
    function Milliseconds(AValue: Integer): TNyxProgramLimits;
    procedure Validate;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxProgramLimits; static;
    property MaximumSourceBytes: Integer read FSource;
    property MaximumSymbols: Integer read FSymbols;
    property MaximumInstructions: Integer read FCode;
    property MaximumRoutines: Integer read FRoutines;
    property MaximumParameters: Integer read FParameters;
    property MaximumNesting: Integer read FNesting;
    property MaximumSteps: Integer read FSteps;
    property MaximumCallDepth: Integer read FCalls;
    property MaximumStackValues: Integer read FStack;
    property MaximumValueBytes: Integer read FValue;
    property MaximumAllocationBytes: Integer read FAllocation;
    property MaximumResultBytes: Integer read FResult;
    property MaximumProposals: Integer read FProposals;
    property MaximumMilliseconds: Integer read FMilliseconds;
  end;

  { Read-only monotonic elapsed-time source in milliseconds. Clock objects must
    be safe on the invocation's execution thread. The runtime rejects nonfinite
    or backwards observations; a wall clock is not a substitute. }
  INyxProgramClock = interface(IInterface)
    ['{9D69BC4B-085E-45F1-A9B0-000000000102}']
    function Milliseconds: Double;
  end;

function NyxProgramSymbol(const AName: TNyxText): TNyxProgramSymbolRef;
function NyxProgramCapability(const AName: TNyxText): TNyxProgramCapabilityRef; overload;
function NyxProgramCapability(AKind: TNyxProgramCapability): TNyxProgramCapabilityRef; overload;
function NyxProgramType(AKind: TNyxProgramKind): TNyxProgramType;
function NyxProgramHandleType(const AName: TNyxProgramSymbolRef): TNyxProgramType;
function NyxProgramValue(AValue: Boolean): INyxProgramValue; overload;
function NyxProgramValue(AValue: Integer): INyxProgramValue; overload;
function NyxProgramValue(AValue: Double): INyxProgramValue; overload;
function NyxProgramValue(const AValue: TNyxText): INyxProgramValue; overload;
function NyxProgramValue(const AValue: TNyxDataValue): INyxProgramValue; overload;
function NyxProgramValue(const AValue: TNyxBytes): INyxProgramValue; overload;
function NyxProgramValue(const AValue: INyxProgramHandle): INyxProgramValue; overload;
function NyxProgramValue(const AValue: TNyxStateValue): INyxProgramValue; overload;
function NyxProgramNothing: INyxProgramValue;
function NyxProgramSpan(AStart, ACount, ALine, AColumn: Integer): TNyxProgramSpan;
function NyxProgramLimits: TNyxProgramLimits;
{ ASCII identifier folding only. Never use this routine to rewrite user text,
  persisted application names or exact capability identities. }
function NyxProgramIdentifierKey(const AName: TNyxText): TNyxText;

implementation

uses nyx.flow.types;

type
  { Default immutable managed value. Construction owns copied data/bytes or a
    retained explicit handle. Public interfaces expose no mutation or concrete
    class dependency; native reference counting and browser interfaces agree. }
  TProgramValue = class(TInterfacedObject, INyxProgramValue)
  private
    FType: TNyxProgramType;
    FScalar: TNyxStateValue;
    FData: TNyxDataValue;
    FBytes: TNyxBytes;
    FHandle: INyxProgramHandle;
    procedure Require(AKind: TNyxProgramKind);
  public
    function GetValueType: TNyxProgramType;
    function Defined: Boolean;
    function AsBoolean: Boolean;
    function AsInteger: Integer;
    function AsNumber: Double;
    function AsText: TNyxText;
    function AsJSON: TNyxDataValue;
    function AsBytes: TNyxBytes;
    function AsHandle: INyxProgramHandle;
    function AsState: TNyxStateValue;
    function Copy: INyxProgramValue;
    function ByteSize: TNyxTextBytes;
  end;


procedure Refuse(const AMessage: TNyxText);
begin
  raise ENyxProgram.Create(npdType, Default(TNyxProgramSpan), AMessage);
end;

constructor ENyxProgram.Create(AKind: TNyxProgramDiagnosticKind;
  const ASpan: TNyxProgramSpan; const AMessage: TNyxText);
begin
  inherited Create('');
  {$ifdef PAS2JS}Message := AMessage;{$else}Message := RawByteString(AMessage);{$endif}
  FDiagnostic.Kind := AKind;
  FDiagnostic.Span := ASpan;
  FDiagnostic.Message := AMessage;
end;

function NyxProgramIdentifierKey(const AName: TNyxText): TNyxText;
var
  LIndex: Integer;
  LScalar: Integer;
begin
  NyxProgramSymbol(AName);
  Result := '';
  for LIndex := 1 to Length(AName) do
  begin
    LScalar := Ord(AName[LIndex]);

    if (LScalar >= Ord('A')) and (LScalar <= Ord('Z')) then
    begin
      Inc(LScalar, 32);
    end;
    Result := Result + NyxScalarText(LScalar);
  end;
end;

function NyxProgramSymbol(const AName: TNyxText): TNyxProgramSymbolRef;
var
  LIndex: Integer;
  LChar: Integer;
begin

  if (Length(AName) < 1) or (Length(AName) > 128) then
  begin
    Refuse('Program identifiers require one to 128 ASCII characters');
  end;
  for LIndex := 1 to Length(AName) do
  begin
    LChar := Ord(AName[LIndex]);

    if not (((LChar >= Ord('A')) and (LChar <= Ord('Z'))) or
      ((LChar >= Ord('a')) and (LChar <= Ord('z'))) or (LChar = Ord('_'))) and
      not ((LIndex > 1) and (LChar >= Ord('0')) and (LChar <= Ord('9'))) then
    begin
      Refuse('Program identifiers use ASCII letters/underscore followed by letters/digits/underscore');
    end;
  end;
  Result.FName := AName;
end;

function TNyxProgramSymbolRef.Defined: Boolean;
begin
  Result := FName <> '';
end;

function TNyxProgramSymbolRef.Same(const AOther: TNyxProgramSymbolRef): Boolean;
begin
  Result := Defined and AOther.Defined and
    (NyxProgramIdentifierKey(FName) = NyxProgramIdentifierKey(AOther.FName));
end;

function NyxProgramCapability(const AName: TNyxText): TNyxProgramCapabilityRef;
var
  LIndex: Integer;
  LScalar: Integer;
  LCount: Integer;
begin
  LIndex := 1;
  LCount := 0;
  while LIndex <= Length(AName) do
  begin

    if not NyxNextScalar(AName, LIndex, LScalar) or NyxScalarWhitespace(LScalar) or (LScalar < 33) or
      ((LScalar >= 127) and (LScalar <= 159)) then
    begin
      Refuse('Capability references require exact non-whitespace Unicode names');
    end;
    Inc(LCount);
  end;

  if (LCount < 1) or (LCount > 128) then
  begin
    Refuse('Capability references require one to 128 scalars');
  end;
  Result.FName := AName;
end;

function NyxProgramCapability(AKind: TNyxProgramCapability): TNyxProgramCapabilityRef;
const
  CNames: array[TNyxProgramCapability] of TNyxText =
    ('state.read', 'state.write', 'scene.read', 'scene.edit', 'camera.read',
     'camera.edit', 'resource.read');
begin

  if (Ord(AKind) < Ord(Low(TNyxProgramCapability))) or
    (Ord(AKind) > Ord(High(TNyxProgramCapability))) then
  begin
    Refuse('Unknown built-in program capability');
  end;
  Result := NyxProgramCapability(CNames[AKind]);
end;

function TNyxProgramCapabilityRef.Defined: Boolean;
begin
  Result := FName <> '';
end;

function TNyxProgramCapabilityRef.Same(const AOther: TNyxProgramCapabilityRef): Boolean;
begin
  Result := Defined and AOther.Defined and (FName = AOther.FName);
end;

function NyxProgramType(AKind: TNyxProgramKind): TNyxProgramType;
begin

  if (Ord(AKind) < Ord(Low(TNyxProgramKind))) or
    (Ord(AKind) > Ord(High(TNyxProgramKind))) or (AKind = npkHandle) then
  begin
    Refuse('A handle requires its own explicit declared type name');
  end;
  Result := Default(TNyxProgramType);
  Result.FDefined := True;
  Result.FKind := AKind;
end;

function NyxProgramHandleType(const AName: TNyxProgramSymbolRef): TNyxProgramType;
begin
  Result := Default(TNyxProgramType);
  Result.FName := NyxProgramSymbol(AName.Name);
  Result.FDefined := True;
  Result.FKind := npkHandle;
end;

function TNyxProgramType.Same(const AOther: TNyxProgramType): Boolean;
begin
  Result := FDefined and AOther.FDefined and (FKind = AOther.FKind);

  if Result and (FKind = npkHandle) then
  begin
    Result := FName.Same(AOther.FName);
  end;
end;

function TNyxProgramType.DisplayName: TNyxText;
const
  CNames: array[TNyxProgramKind] of TNyxText =
    ('Void', 'Boolean', 'Integer', 'Double', 'String', 'JSON', 'Bytes', 'Handle');
begin

  if not FDefined then
  begin
    Exit('Undefined');
  end;

  if FKind = npkHandle then
  begin
    Exit(FName.Name);
  end;
  Result := CNames[FKind];
end;

function TNyxProgramType.ToData: TNyxDataValue;
begin

  if not FDefined then
  begin
    Refuse('An undefined program type cannot be persisted');
  end;
  Result := NyxObject([NyxField('kind', NyxData(Ord(FKind))),
    NyxField('name', NyxData(FName.Name))]);
end;

class function TNyxProgramType.FromData(const AData: TNyxDataValue): TNyxProgramType;
var
  LKind: Integer;
begin
  ValidateNyxFlowFields(AData, ['kind', 'name']);
  LKind := AData.Field('kind').AsInteger;

  if (LKind < Ord(Low(TNyxProgramKind))) or (LKind > Ord(High(TNyxProgramKind))) then
  begin
    Refuse('Unknown persisted program value family');
  end;

  if LKind = Ord(npkHandle) then
  begin
    Exit(NyxProgramHandleType(NyxProgramSymbol(AData.Field('name').AsText)));
  end;

  if AData.Field('name').AsText <> '' then
  begin
    Refuse('Only handle types can carry a type name');
  end;
  Result := NyxProgramType(TNyxProgramKind(LKind));
end;


procedure TProgramValue.Require(AKind: TNyxProgramKind);
begin

  if not FType.Defined or (FType.Kind <> AKind) then
  begin
    Refuse('Program value has a different exact family');
  end;
end;

function TProgramValue.GetValueType: TNyxProgramType;
begin
  Result := FType;
end;

function TProgramValue.Defined: Boolean;
begin
  Result := FType.Defined;
end;

function ScalarValue(const AValue: TNyxStateValue; AKind: TNyxProgramKind): INyxProgramValue;
var
  LValue: TProgramValue;
begin
  LValue := TProgramValue.Create;
  Result := LValue;
  LValue.FType := NyxProgramType(AKind);
  LValue.FScalar := AValue.Copy;
end;

function NyxProgramValue(AValue: Boolean): INyxProgramValue;
begin
  Result := ScalarValue(TNyxStateValue.FromBoolean(AValue), npkBoolean);
end;

function NyxProgramValue(AValue: Integer): INyxProgramValue;
begin
  Result := ScalarValue(TNyxStateValue.FromInteger(AValue), npkInteger);
end;

function NyxProgramValue(AValue: Double): INyxProgramValue;
begin
  Result := ScalarValue(TNyxStateValue.FromNumber(AValue), npkNumber);
end;

function NyxProgramValue(const AValue: TNyxText): INyxProgramValue;
begin
  Result := ScalarValue(TNyxStateValue.FromText(AValue), npkText);
end;

function NyxProgramValue(const AValue: TNyxDataValue): INyxProgramValue;
var
  LValue: TProgramValue;
begin

  if not AValue.Defined then
  begin
    Refuse('Program JSON requires an admitted value, including explicit null');
  end;
  LValue := TProgramValue.Create;
  Result := LValue;
  LValue.FType := NyxProgramType(npkJSON);
  LValue.FData := AValue.Copy;
end;

function NyxProgramValue(const AValue: TNyxBytes): INyxProgramValue;
var
  LValue: TProgramValue;
begin
  LValue := TProgramValue.Create;
  Result := LValue;
  LValue.FType := NyxProgramType(npkBytes);
  LValue.FBytes := System.Copy(AValue, 0, Length(AValue));
end;

function NyxProgramValue(const AValue: INyxProgramHandle): INyxProgramValue;
var
  LValue: TProgramValue;
  LType: TNyxProgramType;
begin

  if AValue = nil then
  begin
    Refuse('An opaque handle must be supplied explicitly');
  end;
  LType := AValue.ValueType;

  if not LType.Defined or (LType.Kind <> npkHandle) then
  begin
    Refuse('An opaque handle requires its explicit declared type');
  end;
  LValue := TProgramValue.Create;
  Result := LValue;
  LValue.FType := LType;
  LValue.FHandle := AValue;
end;

function NyxProgramValue(const AValue: TNyxStateValue): INyxProgramValue;
begin
  AValue.Validate;
  case AValue.Kind of
    nskText: Result := NyxProgramValue(AValue.TextValue);
    nskBoolean: Result := NyxProgramValue(AValue.BooleanValue);
    nskInteger: Result := NyxProgramValue(AValue.IntegerValue);
    nskNumber: Result := NyxProgramValue(AValue.NumberValue);
  end;
end;

function NyxProgramNothing: INyxProgramValue;
var
  LValue: TProgramValue;
begin
  LValue := TProgramValue.Create;
  Result := LValue;
  LValue.FType := NyxProgramType(npkVoid);
end;

function TProgramValue.AsBoolean: Boolean;
begin
  Require(npkBoolean);
  Result := FScalar.BooleanValue;
end;

function TProgramValue.AsInteger: Integer;
begin
  Require(npkInteger);
  Result := FScalar.IntegerValue;
end;

function TProgramValue.AsNumber: Double;
begin
  Require(npkNumber);
  Result := FScalar.NumberValue;
end;

function TProgramValue.AsText: TNyxText;
begin
  Require(npkText);
  Result := FScalar.TextValue;
end;

function TProgramValue.AsJSON: TNyxDataValue;
begin
  Require(npkJSON);
  Result := FData.Copy;
end;

function TProgramValue.AsBytes: TNyxBytes;
begin
  Require(npkBytes);
  Result := System.Copy(FBytes, 0, Length(FBytes));
end;

function TProgramValue.AsHandle: INyxProgramHandle;
begin
  Require(npkHandle);
  Result := FHandle;
end;

function TProgramValue.AsState: TNyxStateValue;
begin

  if not (FType.Kind in [npkBoolean, npkInteger, npkNumber, npkText]) then
  begin
    Refuse('This program value is not scalar application state');
  end;
  Result := FScalar.Copy;
end;

function TProgramValue.Copy: INyxProgramValue;
begin
  Result := Self;
end;

function TProgramValue.ByteSize: TNyxTextBytes;
begin
  Result := 32;
  case FType.Kind of
    npkVoid, npkBoolean, npkInteger, npkNumber: ;
    npkText: Result := Result + NyxUTF8ByteCount(FScalar.TextValue);
    npkJSON: Result := Result + NyxUTF8ByteCount(FData.ToJSON);
    npkBytes: Result := Result + Length(FBytes);
    npkHandle: Result := Result + NyxUTF8ByteCount(FType.HandleName.Name);
  end;
end;

function NyxProgramSpan(AStart, ACount, ALine, AColumn: Integer): TNyxProgramSpan;
begin

  if (AStart < 0) or (ACount < 0) or (ACount > High(Integer) - AStart) or
    (ALine < 1) or (AColumn < 1) then
  begin
    Refuse('Program spans require bounded UTF-8 coordinates');
  end;
  Result := Default(TNyxProgramSpan);
  Result.FDefined := True;
  Result.FStart := AStart;
  Result.FCount := ACount;
  Result.FLine := ALine;
  Result.FColumn := AColumn;
end;

function NyxProgramLimits: TNyxProgramLimits;
begin
  Result := Default(TNyxProgramLimits);
  Result.FDefined := True;
  Result.FSource := 262144;
  Result.FSymbols := 4096;
  Result.FCode := 65536;
  Result.FRoutines := 256;
  Result.FParameters := 64;
  Result.FNesting := 64;
  Result.FSteps := 1000000;
  Result.FCalls := 64;
  Result.FStack := 4096;
  Result.FValue := 1048576;
  Result.FAllocation := 64 * 1024 * 1024;
  Result.FResult := 16 * 1024 * 1024;
  Result.FProposals := 1024;
end;

procedure TNyxProgramLimits.Validate;
begin

  if not FDefined or (FSource < 1) or (FSource = High(Integer)) or
    (FSymbols < 1) or (FCode < 1) or (FRoutines < 1) or (FParameters < 1) or
    (FNesting < 1) or (FNesting > 256) or (FSteps < 1) or (FCalls < 1) or
    (FStack < 1) or (FValue < 1) or (FAllocation < 1) or (FResult < 1) or
    (FProposals < 1) or (FMilliseconds < 0) then
  begin
    Refuse('Program limits require positive quotas and nesting at most 256');
  end;
end;

function TNyxProgramLimits.SourceBytes(AValue: Integer): TNyxProgramLimits;
begin
  Result := Self;
  Result.FSource := AValue;
  Result.Validate;
end;

function TNyxProgramLimits.Symbols(AValue: Integer): TNyxProgramLimits;
begin
  Result := Self;
  Result.FSymbols := AValue;
  Result.Validate;
end;

function TNyxProgramLimits.Instructions(AValue: Integer): TNyxProgramLimits;
begin
  Result := Self;
  Result.FCode := AValue;
  Result.Validate;
end;

function TNyxProgramLimits.Routines(AValue: Integer): TNyxProgramLimits;
begin
  Result := Self;
  Result.FRoutines := AValue;
  Result.Validate;
end;

function TNyxProgramLimits.Parameters(AValue: Integer): TNyxProgramLimits;
begin
  Result := Self;
  Result.FParameters := AValue;
  Result.Validate;
end;

function TNyxProgramLimits.Nesting(AValue: Integer): TNyxProgramLimits;
begin
  Result := Self;
  Result.FNesting := AValue;
  Result.Validate;
end;

function TNyxProgramLimits.Steps(AValue: Integer): TNyxProgramLimits;
begin
  Result := Self;
  Result.FSteps := AValue;
  Result.Validate;
end;

function TNyxProgramLimits.CallDepth(AValue: Integer): TNyxProgramLimits;
begin
  Result := Self;
  Result.FCalls := AValue;
  Result.Validate;
end;

function TNyxProgramLimits.StackValues(AValue: Integer): TNyxProgramLimits;
begin
  Result := Self;
  Result.FStack := AValue;
  Result.Validate;
end;

function TNyxProgramLimits.ValueBytes(AValue: Integer): TNyxProgramLimits;
begin
  Result := Self;
  Result.FValue := AValue;
  Result.Validate;
end;

function TNyxProgramLimits.AllocationBytes(AValue: Integer): TNyxProgramLimits;
begin
  Result := Self;
  Result.FAllocation := AValue;
  Result.Validate;
end;

function TNyxProgramLimits.ResultBytes(AValue: Integer): TNyxProgramLimits;
begin
  Result := Self;
  Result.FResult := AValue;
  Result.Validate;
end;

function TNyxProgramLimits.Proposals(AValue: Integer): TNyxProgramLimits;
begin
  Result := Self;
  Result.FProposals := AValue;
  Result.Validate;
end;

function TNyxProgramLimits.Milliseconds(AValue: Integer): TNyxProgramLimits;
begin
  Result := Self;
  Result.FMilliseconds := AValue;
  Result.Validate;
end;

function TNyxProgramLimits.ToData: TNyxDataValue;
begin
  Validate;
  Result := NyxObject([
    NyxField('source', NyxData(FSource)),
    NyxField('symbols', NyxData(FSymbols)),
    NyxField('instructions', NyxData(FCode)),
    NyxField('routines', NyxData(FRoutines)),
    NyxField('parameters', NyxData(FParameters)),
    NyxField('nesting', NyxData(FNesting)),
    NyxField('steps', NyxData(FSteps)),
    NyxField('calls', NyxData(FCalls)),
    NyxField('stack', NyxData(FStack)),
    NyxField('value', NyxData(FValue)),
    NyxField('allocation', NyxData(FAllocation)),
    NyxField('result', NyxData(FResult)),
    NyxField('proposals', NyxData(FProposals)),
    NyxField('milliseconds', NyxData(FMilliseconds))]);
end;

class function TNyxProgramLimits.FromData(const AData: TNyxDataValue): TNyxProgramLimits;
begin
  ValidateNyxFlowFields(AData, ['source', 'symbols', 'instructions', 'routines', 'parameters', 'nesting', 'steps', 'calls', 'stack', 'value', 'allocation', 'result', 'proposals', 'milliseconds']);
  Result := NyxProgramLimits
    .SourceBytes(AData.Field('source').AsInteger)
    .Symbols(AData.Field('symbols').AsInteger)
    .Instructions(AData.Field('instructions').AsInteger)
    .Routines(AData.Field('routines').AsInteger)
    .Parameters(AData.Field('parameters').AsInteger)
    .Nesting(AData.Field('nesting').AsInteger)
    .Steps(AData.Field('steps').AsInteger)
    .CallDepth(AData.Field('calls').AsInteger)
    .StackValues(AData.Field('stack').AsInteger)
    .ValueBytes(AData.Field('value').AsInteger)
    .AllocationBytes(AData.Field('allocation').AsInteger)
    .ResultBytes(AData.Field('result').AsInteger)
    .Proposals(AData.Field('proposals').AsInteger)
    .Milliseconds(AData.Field('milliseconds').AsInteger);
end;

end.
