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
unit nyx.flow.types;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.bytes;

type
  { Exact portable diagnostics; no ANSI conversion of creator names or failures. }
  ENyxFlow = class(Exception)
  public
    constructor Create(const AMessage: TNyxText); reintroduce;
  end;
  ENyxFlowCancelled = class(ENyxFlow);

  { Closed value families. JSON is structured data, never executable authority.
    Signal is an invocation token, distinct from Boolean/null data. Numeric
    widening/parsing is never implicit; transforms must opt into conversions. }
  TNyxFlowType = (nftText, nftBoolean, nftInteger, nftNumber, nftJSON, nftBytes, nftSignal);
  TNyxFlowDirection = (nfdInput, nfdOutput);
  TNyxLogicAffinity = (nlaPure, nlaUI);
  TNyxLogicGroup = (nlgFiles, nlgJSON, nlgCollections, nlgText, nlgNetwork, nlgData, nlgFlow);

  { Open exact names are distinct references. They admit 1..128 Unicode scalars,
    no C0/C1 controls, and at least one non-whitespace scalar. No delimiter is
    reserved: endpoint identity compares step/port independently. }
  TNyxFlowPortRef = record
  private
    FName: TNyxText;
  public
    property Name: TNyxText read FName;
    function Defined: Boolean;
  end;
  TNyxFlowStepRef = record
  private
    FName: TNyxText;
  public
    property Name: TNyxText read FName;
    function Defined: Boolean;
  end;
  TNyxLogicKindRef = record
  private
    FName: TNyxText;
  public
    property Name: TNyxText read FName;
    function Defined: Boolean;
  end;

  { Independent immutable payload. Bytes use canonical Base64 only inside this
    value/wire boundary; callers receive detached byte arrays. Number keeps the
    admitted exact decimal. AsFloat is its explicit approximate projection. }
  TNyxFlowValue = record
  private
    FType: TNyxFlowType;
    FData: TNyxDataValue;
    procedure Require(AType: TNyxFlowType);
  public
    function GetType: TNyxFlowType;
    function AsText: TNyxText;
    function AsBoolean: Boolean;
    function AsInteger: Integer;
    function AsNumber: TNyxDecimal;
    function AsFloat: Double;
    function AsJSON: TNyxDataValue;
    function AsBytes: TNyxBytes;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxFlowValue; static;
    property ValueType: TNyxFlowType read GetType;
  end;
  TNyxFlowEntry = record
    Port: TNyxFlowPortRef;
    Value: TNyxFlowValue;
  end;

  { Immutable named operation frame. WithValue replaces an exact port while
    copying the vector; earlier frames remain independent. No mutable store,
    callback, scheduler, host authority or UI control belongs to a frame. }
  TNyxFlowValues = record
  private
    FEntries: array of TNyxFlowEntry;
    function IndexOf(const APort: TNyxFlowPortRef): Integer;
    function GetCount: Integer;
  public
    function WithValue(const APort: TNyxFlowPortRef;
      const AValue: TNyxFlowValue): TNyxFlowValues;
    function Has(const APort: TNyxFlowPortRef): Boolean;
    function Value(const APort: TNyxFlowPortRef): TNyxFlowValue;
    function Entry(AIndex: Integer): TNyxFlowEntry;
    property Count: Integer read GetCount;
  end;

  { Copied discoverable port metadata. Inputs accept one producer per invocation;
    outputs may fan out. Required inputs have no implicit null/default coercion. }
  TNyxFlowPort = record
    Reference: TNyxFlowPortRef;
    Direction: TNyxFlowDirection;
    ValueType: TNyxFlowType;
    Description: TNyxText;
  end;
  TNyxFlowEndpoint = record
    Step: TNyxFlowStepRef;
    Port: TNyxFlowPortRef;
    function Same(const AOther: TNyxFlowEndpoint): Boolean;
    procedure Validate;
  end;

  { Immutable creator metadata. Pure means independently owned immutable inputs
    and worker-safe implementation, a creator contract that a flag cannot prove.
    UI affinity requires the submitting UI thread. Labels are exact search data. }
  TNyxLogicDescriptor = record
  private
    FKind: TNyxLogicKindRef;
    FTitle: TNyxText;
    FDescription: TNyxText;
    FGroup: TNyxLogicGroup;
    FAffinity: TNyxLogicAffinity;
    FPorts: array of TNyxFlowPort;
    FLabels: array of TNyxText;
    function AddPort(const APort: TNyxFlowPortRef; ADirection: TNyxFlowDirection;
      AType: TNyxFlowType; const ADescription: TNyxText): TNyxLogicDescriptor;
    function GetCount: Integer;
  public
    function Input(const APort: TNyxFlowPortRef; AType: TNyxFlowType;
      const ADescription: TNyxText): TNyxLogicDescriptor;
    function Output(const APort: TNyxFlowPortRef; AType: TNyxFlowType;
      const ADescription: TNyxText): TNyxLogicDescriptor;
    function Labelled(const ALabel: TNyxText): TNyxLogicDescriptor;
    function Port(AIndex: Integer): TNyxFlowPort;
    function Find(const APort: TNyxFlowPortRef): TNyxFlowPort;
    function LabelCount: Integer;
    function LabelAt(AIndex: Integer): TNyxText;
    procedure Validate;
    function ToData: TNyxDataValue;
    property Kind: TNyxLogicKindRef read FKind;
    property Title: TNyxText read FTitle;
    property Description: TNyxText read FDescription;
    property Group: TNyxLogicGroup read FGroup;
    property Affinity: TNyxLogicAffinity read FAffinity;
    property PortCount: Integer read GetCount;
  end;

  { Caller-tunable finite admission budgets. Counts/bytes are validated before
    graph ownership or operation entry. The run total counts external values and
    every produced value once, independently of fan-out. It is not peak-memory,
    latency or blocking-host-work enforcement. }
  TNyxFlowLimits = record
  private
    FSteps: Integer;
    FWires: Integer;
    FValueBytes: Integer;
    FRunBytes: Integer;
  public
    class function Defaults: TNyxFlowLimits; static;
    function Steps(AValue: Integer): TNyxFlowLimits;
    function Wires(AValue: Integer): TNyxFlowLimits;
    function ValueBytes(AValue: Integer): TNyxFlowLimits;
    function RunBytes(AValue: Integer): TNyxFlowLimits;
    procedure Validate;
    property MaximumSteps: Integer read FSteps;
    property MaximumWires: Integer read FWires;
    property MaximumValueBytes: Integer read FValueBytes;
    property MaximumRunBytes: Integer read FRunBytes;
  end;

function NyxFlowPort(const AName: TNyxText): TNyxFlowPortRef;
function NyxFlowStep(const AName: TNyxText): TNyxFlowStepRef;
function NyxLogicKind(const AName: TNyxText): TNyxLogicKindRef;
function NyxFlowEndpoint(const AStep: TNyxFlowStepRef;
  const APort: TNyxFlowPortRef): TNyxFlowEndpoint;
function NyxFlowValues: TNyxFlowValues;
function NyxFlowText(const AValue: TNyxText): TNyxFlowValue;
function NyxFlowBoolean(AValue: Boolean): TNyxFlowValue;
function NyxFlowInteger(AValue: Integer): TNyxFlowValue;
function NyxFlowNumber(const AValue: TNyxDecimal): TNyxFlowValue;
function NyxFlowJSON(const AValue: TNyxDataValue): TNyxFlowValue;
function NyxFlowBytes(const AValue: TNyxBytes): TNyxFlowValue;
function NyxFlowSignal: TNyxFlowValue;
function NyxLogicDescriptor(const AKind: TNyxLogicKindRef;
  const ATitle, ADescription: TNyxText; AGroup: TNyxLogicGroup;
  AAffinity: TNyxLogicAffinity = nlaPure): TNyxLogicDescriptor;
{ Shared exact closed-shape persistence guard. No unknown fields silently vanish. }
procedure ValidateNyxFlowFields(const AData: TNyxDataValue; const AFields: array of TNyxText);

implementation

constructor ENyxFlow.Create(const AMessage: TNyxText);
{$ifndef PAS2JS}
var
  LMessage: String;
{$endif}
begin
  inherited Create('');
  {$ifdef PAS2JS}
  Message := AMessage;
  {$else}
  RawByteString(LMessage) := RawByteString(AMessage);
  Message := LMessage;
  {$endif}
end;

procedure ValidateName(const AName: TNyxText);
var
  LIndex: Integer;
  LScalar: Integer;
  LCount: Integer;
  LNonspace: Boolean;
begin
  LIndex := 1;
  LCount := 0;
  LNonspace := False;
  while LIndex <= Length(AName) do
  begin

    if not NyxNextScalar(AName, LIndex, LScalar) or (LScalar < 32) or
      ((LScalar >= 127) and (LScalar <= 159)) then
    begin
      raise ENyxFlow.Create('Flow identity contains invalid Unicode/control text');
    end;
    Inc(LCount);
    LNonspace := LNonspace or not NyxScalarWhitespace(LScalar);
  end;

  if not LNonspace or (LCount > 128) then
  begin
    raise ENyxFlow.Create('Flow identity requires 1..128 scalars and nonspace text');
  end;
end;

procedure ValidateNyxFlowFields(const AData: TNyxDataValue; const AFields: array of TNyxText);
var
  LIndex: Integer;
begin

  if (AData.Kind <> ndObject) or (AData.Count <> Length(AFields)) then
  begin
    raise ENyxFlow.Create('Flow object requires its exact closed fields');
  end;
  for LIndex := 0 to High(AFields) do
  begin
    AData.Field(AFields[LIndex]);
  end;
end;

function NyxFlowPort(const AName: TNyxText): TNyxFlowPortRef;
begin
  ValidateName(AName);
  Result.FName := AName;
end;

function NyxFlowStep(const AName: TNyxText): TNyxFlowStepRef;
begin
  ValidateName(AName);
  Result.FName := AName;
end;

function NyxLogicKind(const AName: TNyxText): TNyxLogicKindRef;
begin
  ValidateName(AName);
  Result.FName := AName;
end;

function TNyxFlowPortRef.Defined: Boolean;
begin
  Result := FName <> '';
end;

function TNyxFlowStepRef.Defined: Boolean;
begin
  Result := FName <> '';
end;

function TNyxLogicKindRef.Defined: Boolean;
begin
  Result := FName <> '';
end;

function NyxFlowEndpoint(const AStep: TNyxFlowStepRef;
  const APort: TNyxFlowPortRef): TNyxFlowEndpoint;
begin
  Result.Step := AStep;
  Result.Port := APort;
  Result.Validate;
end;

procedure TNyxFlowEndpoint.Validate;
begin
  ValidateName(Step.Name);
  ValidateName(Port.Name);
end;

function TNyxFlowEndpoint.Same(const AOther: TNyxFlowEndpoint): Boolean;
begin
  Result := (Step.Name = AOther.Step.Name) and (Port.Name = AOther.Port.Name);
end;

function TNyxFlowValue.GetType: TNyxFlowType;
begin
  FData.Validate;
  Result := FType;
end;

procedure TNyxFlowValue.Require(AType: TNyxFlowType);
begin

  if GetType <> AType then
  begin
    raise ENyxFlow.Create('Flow value has a different typed family');
  end;
end;

function NyxFlowText(const AValue: TNyxText): TNyxFlowValue;
begin
  Result.FType := nftText;
  Result.FData := NyxData(AValue);
end;

function NyxFlowBoolean(AValue: Boolean): TNyxFlowValue;
begin
  Result.FType := nftBoolean;
  Result.FData := NyxData(AValue);
end;

function NyxFlowInteger(AValue: Integer): TNyxFlowValue;
begin
  Result.FType := nftInteger;
  Result.FData := NyxData(AValue);
end;

function NyxFlowNumber(const AValue: TNyxDecimal): TNyxFlowValue;
begin
  Result.FType := nftNumber;
  Result.FData := NyxData(AValue);
end;

function NyxFlowJSON(const AValue: TNyxDataValue): TNyxFlowValue;
begin
  Result.FType := nftJSON;
  Result.FData := AValue.Copy;
end;

function NyxFlowBytes(const AValue: TNyxBytes): TNyxFlowValue;
begin
  Result.FType := nftBytes;
  Result.FData := NyxData(NyxEncodeBase64(AValue));
end;

function NyxFlowSignal: TNyxFlowValue;
begin
  Result.FType := nftSignal;
  Result.FData := NyxNull;
end;

function TNyxFlowValue.AsText: TNyxText;
begin
  Require(nftText);
  Result := FData.AsText;
end;

function TNyxFlowValue.AsBoolean: Boolean;
begin
  Require(nftBoolean);
  Result := FData.AsBoolean;
end;

function TNyxFlowValue.AsInteger: Integer;
begin
  Require(nftInteger);
  Result := FData.AsInteger;
end;

function TNyxFlowValue.AsNumber: TNyxDecimal;
begin
  Require(nftNumber);
  Result := FData.AsDecimal;
end;

function TNyxFlowValue.AsFloat: Double;
begin
  Require(nftNumber);
  Result := FData.AsNumber;
end;

function TNyxFlowValue.AsJSON: TNyxDataValue;
begin
  Require(nftJSON);
  Result := FData.Copy;
end;

function TNyxFlowValue.AsBytes: TNyxBytes;
begin
  Require(nftBytes);
  Result := NyxDecodeBase64(FData.AsText);
end;

function TNyxFlowValue.ToData: TNyxDataValue;
begin
  GetType;
  Result := NyxObject([NyxField('type', NyxData(Ord(FType))), NyxField('value', FData)]);
end;

class function TNyxFlowValue.FromData(const AData: TNyxDataValue): TNyxFlowValue;
var
  LType: Integer;
  LValue: TNyxDataValue;
begin
  ValidateNyxFlowFields(AData, ['type', 'value']);
  LType := AData.Field('type').AsInteger;
  LValue := AData.Field('value');
  case LType of
    Ord(nftText): Result := NyxFlowText(LValue.AsText);
    Ord(nftBoolean): Result := NyxFlowBoolean(LValue.AsBoolean);
    Ord(nftInteger): Result := NyxFlowInteger(LValue.AsInteger);
    Ord(nftNumber): Result := NyxFlowNumber(LValue.AsDecimal);
    Ord(nftJSON): Result := NyxFlowJSON(LValue);
    Ord(nftBytes): Result := NyxFlowBytes(NyxDecodeBase64(LValue.AsText));
    Ord(nftSignal):
      begin

        if LValue.Kind <> ndNull then
        begin
          raise ENyxFlow.Create('Signal tokens carry no fabricated payload');
        end;
        Result := NyxFlowSignal;
      end;
  else
    raise ENyxFlow.Create('Unknown flow value family');
  end;
end;

function NyxFlowValues: TNyxFlowValues;
begin
  Result := Default(TNyxFlowValues);
end;

function TNyxFlowValues.GetCount: Integer;
begin
  Result := Length(FEntries);
end;

function TNyxFlowValues.IndexOf(const APort: TNyxFlowPortRef): Integer;
begin
  ValidateName(APort.Name);
  for Result := 0 to High(FEntries) do
  begin

    if FEntries[Result].Port.Name = APort.Name then
    begin
      Exit;
    end;
  end;
  Result := -1;
end;

function TNyxFlowValues.Has(const APort: TNyxFlowPortRef): Boolean;
begin
  Result := IndexOf(APort) >= 0;
end;

function TNyxFlowValues.WithValue(const APort: TNyxFlowPortRef;
  const AValue: TNyxFlowValue): TNyxFlowValues;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(APort);
  AValue.GetType;
  Result := Self;
  Result.FEntries := Copy(FEntries, 0, Length(FEntries));

  if LIndex < 0 then
  begin

    if Length(FEntries) >= 16 then
    begin
      raise ENyxFlow.Create('An operation frame supports at most sixteen ports');
    end;
    LIndex := Length(FEntries);
    SetLength(Result.FEntries, LIndex + 1);
  end;
  Result.FEntries[LIndex].Port := APort;
  Result.FEntries[LIndex].Value := AValue;
end;

function TNyxFlowValues.Value(const APort: TNyxFlowPortRef): TNyxFlowValue;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(APort);

  if LIndex < 0 then
  begin
    raise ENyxFlow.Create('Required flow input is missing: ' + APort.Name);
  end;
  Result := FEntries[LIndex].Value;
end;

function TNyxFlowValues.Entry(AIndex: Integer): TNyxFlowEntry;
begin

  if (AIndex < 0) or (AIndex >= Length(FEntries)) then
  begin
    raise ENyxFlow.Create('Flow frame index out of range');
  end;
  Result := FEntries[AIndex];
end;

function NyxLogicDescriptor(const AKind: TNyxLogicKindRef;
  const ATitle, ADescription: TNyxText; AGroup: TNyxLogicGroup;
  AAffinity: TNyxLogicAffinity): TNyxLogicDescriptor;
begin
  Result := Default(TNyxLogicDescriptor);
  Result.FKind := AKind;
  Result.FTitle := ATitle;
  Result.FDescription := ADescription;
  Result.FGroup := AGroup;
  Result.FAffinity := AAffinity;
  Result.Validate;
end;

function TNyxLogicDescriptor.AddPort(const APort: TNyxFlowPortRef;
  ADirection: TNyxFlowDirection; AType: TNyxFlowType;
  const ADescription: TNyxText): TNyxLogicDescriptor;
var
  LIndex: Integer;
begin
  ValidateName(APort.Name);

  if (ADescription = '') or (NyxUTF8ByteCount(ADescription) > 16384) or
    (Ord(AType) < Ord(Low(TNyxFlowType))) or (Ord(AType) > Ord(High(TNyxFlowType))) then
  begin
    raise ENyxFlow.Create('Unknown port value family');
  end;
  for LIndex := 0 to High(FPorts) do
  begin

    if FPorts[LIndex].Reference.Name = APort.Name then
    begin
      raise ENyxFlow.Create('Logic port names must be distinct');
    end;
  end;

  if Length(FPorts) >= 16 then
  begin
    raise ENyxFlow.Create('An operation supports at most sixteen ports');
  end;
  Result := Self;
  Result.FPorts := Copy(FPorts, 0, Length(FPorts));
  SetLength(Result.FPorts, Length(FPorts) + 1);
  LIndex := High(Result.FPorts);
  Result.FPorts[LIndex].Reference := APort;
  Result.FPorts[LIndex].Direction := ADirection;
  Result.FPorts[LIndex].ValueType := AType;
  Result.FPorts[LIndex].Description := ADescription;
end;

function TNyxLogicDescriptor.Input(const APort: TNyxFlowPortRef; AType: TNyxFlowType;
  const ADescription: TNyxText): TNyxLogicDescriptor;
begin
  Result := AddPort(APort, nfdInput, AType, ADescription);
end;

function TNyxLogicDescriptor.Output(const APort: TNyxFlowPortRef; AType: TNyxFlowType;
  const ADescription: TNyxText): TNyxLogicDescriptor;
begin
  Result := AddPort(APort, nfdOutput, AType, ADescription);
end;

function TNyxLogicDescriptor.Labelled(const ALabel: TNyxText): TNyxLogicDescriptor;
var
  LIndex: Integer;
begin
  ValidateName(ALabel);
  Result := Self;
  for LIndex := 0 to High(FLabels) do
  begin

    if FLabels[LIndex] = ALabel then
    begin
      Exit;
    end;
  end;

  if Length(FLabels) >= 32 then
  begin
    raise ENyxFlow.Create('Logic labels exceed their capacity');
  end;
  Result.FLabels := Copy(FLabels, 0, Length(FLabels));
  SetLength(Result.FLabels, Length(FLabels) + 1);
  Result.FLabels[High(Result.FLabels)] := ALabel;
end;

function TNyxLogicDescriptor.GetCount: Integer;
begin
  Result := Length(FPorts);
end;

function TNyxLogicDescriptor.Port(AIndex: Integer): TNyxFlowPort;
begin

  if (AIndex < 0) or (AIndex >= Length(FPorts)) then
  begin
    raise ENyxFlow.Create('Logic port index out of range');
  end;
  Result := FPorts[AIndex];
end;

function TNyxLogicDescriptor.Find(const APort: TNyxFlowPortRef): TNyxFlowPort;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FPorts) do
  begin

    if FPorts[LIndex].Reference.Name = APort.Name then
    begin
      Exit(FPorts[LIndex]);
    end;
  end;
  raise ENyxFlow.Create('Logic port is not declared: ' + APort.Name);
end;

function TNyxLogicDescriptor.LabelCount: Integer;
begin
  Result := Length(FLabels);
end;

function TNyxLogicDescriptor.LabelAt(AIndex: Integer): TNyxText;
begin

  if (AIndex < 0) or (AIndex >= Length(FLabels)) then
  begin
    raise ENyxFlow.Create('Logic label index out of range');
  end;
  Result := FLabels[AIndex];
end;

procedure TNyxLogicDescriptor.Validate;
begin
  ValidateName(FKind.Name);
  if (FTitle = '') or (FDescription = '') or (NyxUTF8ByteCount(FTitle) > 4096) or
    (NyxUTF8ByteCount(FDescription) > 16384) or
    (Ord(FGroup) < Ord(Low(TNyxLogicGroup))) or
    (Ord(FGroup) > Ord(High(TNyxLogicGroup))) or
    (Ord(FAffinity) < Ord(Low(TNyxLogicAffinity))) or
    (Ord(FAffinity) > Ord(High(TNyxLogicAffinity))) then
  begin
    raise ENyxFlow.Create('Logic metadata requires bounded help, title, group and affinity');
  end;
end;

function TNyxLogicDescriptor.ToData: TNyxDataValue;
var
  LPorts: array of TNyxDataValue;
  LLabels: array of TNyxDataValue;
  LIndex: Integer;
begin
  Validate;
  SetLength(LPorts, Length(FPorts));
  SetLength(LLabels, Length(FLabels));
  for LIndex := 0 to High(FPorts) do
  begin
    LPorts[LIndex] := NyxObject([
      NyxField('name', NyxData(FPorts[LIndex].Reference.Name)),
      NyxField('direction', NyxData(Ord(FPorts[LIndex].Direction))),
      NyxField('type', NyxData(Ord(FPorts[LIndex].ValueType))),
      NyxField('description', NyxData(FPorts[LIndex].Description))]);
  end;
  for LIndex := 0 to High(FLabels) do
  begin
    LLabels[LIndex] := NyxData(FLabels[LIndex]);
  end;
  Result := NyxObject([NyxField('kind', NyxData(FKind.Name)),
    NyxField('title', NyxData(FTitle)), NyxField('description', NyxData(FDescription)),
    NyxField('group', NyxData(Ord(FGroup))), NyxField('affinity', NyxData(Ord(FAffinity))),
    NyxField('ports', NyxArray(LPorts)), NyxField('labels', NyxArray(LLabels))]);
end;

class function TNyxFlowLimits.Defaults: TNyxFlowLimits;
begin
  Result.FSteps := 128;
  Result.FWires := 512;
  Result.FValueBytes := 1048576;
  Result.FRunBytes := 8 * 1048576;
end;

procedure TNyxFlowLimits.Validate;
begin

  if (FSteps < 1) or (FSteps > 4096) or (FWires < 0) or (FWires > 16384) or
    (FValueBytes < 1) or (FValueBytes > 4 * 1048576) or
    (FRunBytes < FValueBytes) or (FRunBytes > 64 * 1048576) then
  begin
    raise ENyxFlow.Create('Flow limits require finite admitted counts/byte budgets');
  end;
end;

function TNyxFlowLimits.Steps(AValue: Integer): TNyxFlowLimits;
begin
  Result := Self;
  Result.FSteps := AValue;
  Result.Validate;
end;

function TNyxFlowLimits.Wires(AValue: Integer): TNyxFlowLimits;
begin
  Result := Self;
  Result.FWires := AValue;
  Result.Validate;
end;

function TNyxFlowLimits.ValueBytes(AValue: Integer): TNyxFlowLimits;
begin
  Result := Self;
  Result.FValueBytes := AValue;
  Result.Validate;
end;

function TNyxFlowLimits.RunBytes(AValue: Integer): TNyxFlowLimits;
begin
  Result := Self;
  Result.FRunBytes := AValue;
  Result.Validate;
end;

end.
