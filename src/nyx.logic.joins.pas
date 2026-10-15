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
unit nyx.logic.joins;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.logic, nyx.flow.types, nyx.scheduler;

type
  { ExactlyOne refuses ambiguity. First/Last use declared input order, never
    completion timing or topological step registration order. All selected
    dependencies finish before the join runs; errors/cancellation are not absence. }
  TNyxFlowJoinSelection = (nfjExactlyOne, nfjFirstAvailable, nfjLastAvailable);

  { Empty joins either skip the whole step/outputs with normal graph provenance
    or fail. No default/null/false/signal is invented for an unavailable branch. }
  TNyxFlowJoinEmpty = (nfjeSkip, nfjeError);

  { Closed discoverable join outputs. Value retains the exact payload family;
    Source is text identifying the selected input reference. }
  TNyxFlowJoinPort = (nfjpValue, nfjpSource);

  { Independent fluent recipe. Input appends one distinct open application port
    reference. One to fourteen alternatives leave room for the two outputs.
    Names value/source are reserved. Copy detaches the vector and every record;
    no operation, runtime, interface or document belongs to these options.
    Default records are invalid; NyxFlowJoinOptions initializes a typed recipe. }
  TNyxFlowJoinOptions = record
  private
    FInitialized: Boolean;
    FType: TNyxFlowType;
    FSelection: TNyxFlowJoinSelection;
    FEmpty: TNyxFlowJoinEmpty;
    FInputs: array of TNyxFlowPortRef;
    procedure ValidateHeader;
    function GetCount: Integer;
  public
    { Append one validated exact reference, refusing duplicates/reserved names or
      the fourteen-input limit before changing this recipe. }
    function Input(const APort: TNyxFlowPortRef): TNyxFlowJoinOptions;
    { Choose ambiguity/priority handling. All active producers still complete;
      this policy does not race, suppress errors or cancel losing branches. }
    function Select(ASelection: TNyxFlowJoinSelection): TNyxFlowJoinOptions;
    { Select whole-step skip provenance or an error when no input is present. }
    function WhenEmpty(AEmpty: TNyxFlowJoinEmpty): TNyxFlowJoinOptions;
    { Return an independent reference in declaration order; invalid indices raise. }
    function Port(AIndex: Integer): TNyxFlowPortRef;
    { Detach the recipe vector; an initialized unfinished zero-input recipe may
      be copied for configuration, but cannot construct or persist an operation. }
    function Copy: TNyxFlowJoinOptions;
    { Admit a complete recipe; raises ENyxFlow without retaining host authority. }
    procedure Validate;
    { Closed persistence boundary. FromData refuses unknown fields/choices,
      invalid names and incomplete recipes; no executable creator is resolved. }
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxFlowJoinOptions; static;
    property ValueType: TNyxFlowType read FType;
    property Selection: TNyxFlowJoinSelection read FSelection;
    property Empty: TNyxFlowJoinEmpty read FEmpty;
    property Count: Integer read GetCount;
  end;

  { Managed pure conditional merge of branch outputs. Options are detached.
    TrySelect returns false only for configured empty-skip, clearing both outputs;
    error/ambiguity/type/unknown-input cases raise without a selected result.
    A present JSON null, false, zero, empty text or empty bytes is a real value.
    Graphs use INyxConditionalLogic to record an empty skip before Invoke; direct
    Invoke on an empty skip refuses because it cannot return declared outputs.
    This is not failure recovery or a first-completion race primitive. }
  INyxFlowJoin = interface(INyxLogicOperation)
    ['{D1411014-6120-4C84-A708-000000000079}']
    function GetOptions: TNyxFlowJoinOptions;
    function TrySelect(const AInputs: TNyxFlowValues; out AValue: TNyxFlowValue;
      out ASource: TNyxFlowPortRef): Boolean;
    property Options: TNyxFlowJoinOptions read GetOptions;
  end;

function NyxFlowJoinOptions(AType: TNyxFlowType): TNyxFlowJoinOptions;
{ Output identity is closed; application-defined input names use NyxFlowPort. }
function NyxFlowJoinPort(APort: TNyxFlowJoinPort): TNyxFlowPortRef;
{ Validate and capture independent immutable options/schema in a managed pure
  operation. An empty recipe refuses before any runtime graph is changed. }
function NewNyxFlowJoin(const AOptions: TNyxFlowJoinOptions): INyxFlowJoin;

implementation

type
  { The immutable implementation owns only copied options/schema/configuration.
    Its admission predicate reads the invocation frame without effects or retained
    receivers. Each selection is local; concurrent pure calls share no cursors. }
  TFlowJoin = class(TNyxLogicOperation, INyxFlowJoin, INyxConditionalLogic)
  private
    FOptions: TNyxFlowJoinOptions;
    function Choose(const AInputs: TNyxFlowValues): Integer;
  protected
    function Execute(const AInputs: TNyxFlowValues;
      const ACancellation: INyxCancellationScope): TNyxFlowValues; override;
  public
    constructor Create(const AOptions: TNyxFlowJoinOptions);
    function GetOptions: TNyxFlowJoinOptions;
    function ShouldInvoke(const AInputs: TNyxFlowValues): Boolean;
    function TrySelect(const AInputs: TNyxFlowValues; out AValue: TNyxFlowValue;
      out ASource: TNyxFlowPortRef): Boolean;
  end;

procedure TNyxFlowJoinOptions.ValidateHeader;
begin

  if not FInitialized or
    (Ord(FType) < Ord(Low(TNyxFlowType))) or (Ord(FType) > Ord(High(TNyxFlowType))) or
    (Ord(FSelection) < Ord(Low(TNyxFlowJoinSelection))) or
    (Ord(FSelection) > Ord(High(TNyxFlowJoinSelection))) or
    (Ord(FEmpty) < Ord(Low(TNyxFlowJoinEmpty))) or (Ord(FEmpty) > Ord(High(TNyxFlowJoinEmpty))) then
  begin
    raise ENyxFlow.Create('Flow join requires initialized closed type/selection/empty choices');
  end;
end;

function NyxFlowJoinOptions(AType: TNyxFlowType): TNyxFlowJoinOptions;
begin
  Result := Default(TNyxFlowJoinOptions);
  Result.FInitialized := True;
  Result.FType := AType;
  Result.FSelection := nfjExactlyOne;
  Result.FEmpty := nfjeSkip;
  Result.ValidateHeader;
end;

function NyxFlowJoinPort(APort: TNyxFlowJoinPort): TNyxFlowPortRef;
const
  CNames: array[TNyxFlowJoinPort] of TNyxText = ('value', 'source');
var
  LIndex: Integer;
begin
  LIndex := Ord(APort);

  if (LIndex < Ord(Low(TNyxFlowJoinPort))) or
    (LIndex > Ord(High(TNyxFlowJoinPort))) then
  begin
    raise ENyxFlow.Create('Unknown flow join output');
  end;
  Result := NyxFlowPort(CNames[APort]);
end;

function TNyxFlowJoinOptions.Copy: TNyxFlowJoinOptions;
var
  LIndex: Integer;
begin
  ValidateHeader;
  Result := Default(TNyxFlowJoinOptions);
  Result.FInitialized := True;
  Result.FType := FType;
  Result.FSelection := FSelection;
  Result.FEmpty := FEmpty;
  SetLength(Result.FInputs, Length(FInputs));
  for LIndex := 0 to High(FInputs) do
  begin
    Result.FInputs[LIndex] := NyxFlowPort(FInputs[LIndex].Name);
  end;
end;

function TNyxFlowJoinOptions.Input(const APort: TNyxFlowPortRef): TNyxFlowJoinOptions;
var
  LReference: TNyxFlowPortRef;
  LIndex: Integer;
begin
  LReference := NyxFlowPort(APort.Name);
  ValidateHeader;

  if (LReference.Name = NyxFlowJoinPort(nfjpValue).Name) or
    (LReference.Name = NyxFlowJoinPort(nfjpSource).Name) or (Length(FInputs) >= 14) then
  begin
    raise ENyxFlow.Create('Flow join supports fourteen alternatives and reserves value/source outputs');
  end;
  for LIndex := 0 to High(FInputs) do
  begin

    if FInputs[LIndex].Name = LReference.Name then
    begin
      raise ENyxFlow.Create('Flow join input references must be distinct');
    end;
  end;
  Result := Copy;
  SetLength(Result.FInputs, Length(FInputs) + 1);
  Result.FInputs[High(Result.FInputs)] := LReference;
end;

function TNyxFlowJoinOptions.Select(ASelection: TNyxFlowJoinSelection): TNyxFlowJoinOptions;
begin
  Result := Copy;
  Result.FSelection := ASelection;
  Result.ValidateHeader;
end;

function TNyxFlowJoinOptions.WhenEmpty(AEmpty: TNyxFlowJoinEmpty): TNyxFlowJoinOptions;
begin
  Result := Copy;
  Result.FEmpty := AEmpty;
  Result.ValidateHeader;
end;

function TNyxFlowJoinOptions.GetCount: Integer;
begin
  ValidateHeader;
  Result := Length(FInputs);
end;

function TNyxFlowJoinOptions.Port(AIndex: Integer): TNyxFlowPortRef;
begin
  ValidateHeader;

  if (AIndex < 0) or (AIndex >= Length(FInputs)) then
  begin
    raise ENyxFlow.Create('Flow join input index out of range');
  end;
  Result := NyxFlowPort(FInputs[AIndex].Name);
end;

procedure TNyxFlowJoinOptions.Validate;
var
  LCopy: TNyxFlowJoinOptions;
  LIndex: Integer;
begin
  ValidateHeader;

  if (Length(FInputs) < 1) or (Length(FInputs) > 14) then
  begin
    raise ENyxFlow.Create('Flow join requires one to fourteen alternatives');
  end;
  LCopy := NyxFlowJoinOptions(FType);
  for LIndex := 0 to High(FInputs) do
  begin
    LCopy := LCopy.Input(FInputs[LIndex]);
  end;
end;

function TNyxFlowJoinOptions.ToData: TNyxDataValue;
var
  LInputs: array of TNyxDataValue;
  LIndex: Integer;
begin
  Validate;
  SetLength(LInputs, Length(FInputs));
  for LIndex := 0 to High(FInputs) do
  begin
    LInputs[LIndex] := NyxData(FInputs[LIndex].Name);
  end;
  Result := NyxObject([NyxField('type', NyxData(Ord(FType))),
    NyxField('selection', NyxData(Ord(FSelection))), NyxField('empty', NyxData(Ord(FEmpty))),
    NyxField('inputs', NyxArray(LInputs))]);
end;

class function TNyxFlowJoinOptions.FromData(const AData: TNyxDataValue): TNyxFlowJoinOptions;
var
  LType: Integer;
  LSelection: Integer;
  LEmpty: Integer;
  LInputs: TNyxDataValue;
  LIndex: Integer;
begin
  ValidateNyxFlowFields(AData, ['type', 'selection', 'empty', 'inputs']);
  LType := AData.Field('type').AsInteger;
  LSelection := AData.Field('selection').AsInteger;
  LEmpty := AData.Field('empty').AsInteger;

  if (LType < Ord(Low(TNyxFlowType))) or (LType > Ord(High(TNyxFlowType))) or
    (LSelection < Ord(Low(TNyxFlowJoinSelection))) or (LSelection > Ord(High(TNyxFlowJoinSelection))) or
    (LEmpty < Ord(Low(TNyxFlowJoinEmpty))) or (LEmpty > Ord(High(TNyxFlowJoinEmpty))) then
  begin
    raise ENyxFlow.Create('Unknown persisted flow join choices');
  end;
  Result := NyxFlowJoinOptions(TNyxFlowType(LType))
    .Select(TNyxFlowJoinSelection(LSelection)).WhenEmpty(TNyxFlowJoinEmpty(LEmpty));
  LInputs := AData.Field('inputs');

  if (LInputs.Kind <> ndArray) or (LInputs.Count < 1) or (LInputs.Count > 14) then
  begin
    raise ENyxFlow.Create('Flow join requires one to fourteen input references');
  end;
  for LIndex := 0 to LInputs.Count - 1 do
  begin
    Result := Result.Input(NyxFlowPort(LInputs.Item(LIndex).AsText));
  end;
  Result.Validate;
end;

constructor TFlowJoin.Create(const AOptions: TNyxFlowJoinOptions);
var
  LIndex: Integer;
begin
  inherited Create;
  AOptions.Validate;
  FOptions := AOptions.Copy;
  FConfiguration := FOptions.ToData;
  FDescriptor := NyxLogicDescriptor(NyxLogicKind('flow-join'), 'Join conditional branches',
    'Rejoin available branches with exact payload types, declared priority and explicit empty/ambiguity policies.',
    nlgFlow).Labelled('branch').Labelled('merge').Labelled('conditional').Labelled('flow');
  for LIndex := 0 to FOptions.Count - 1 do
  begin
    FDescriptor := FDescriptor.OptionalInput(FOptions.Port(LIndex), FOptions.ValueType, 'Optional branch value');
  end;
  FDescriptor := FDescriptor.Output(NyxFlowJoinPort(nfjpValue), FOptions.ValueType, 'Selected exact branch value')
    .Output(NyxFlowJoinPort(nfjpSource), nftText, 'Selected input reference');
end;

function TFlowJoin.GetOptions: TNyxFlowJoinOptions;
begin
  Result := FOptions.Copy;
end;

function TFlowJoin.Choose(const AInputs: TNyxFlowValues): Integer;
var
  LIndex: Integer;
begin
  ValidateNyxLogicFrame(FDescriptor, nfdInput, AInputs);
  Result := -1;
  for LIndex := 0 to FOptions.Count - 1 do
  begin

    if not AInputs.Has(FOptions.Port(LIndex)) then
    begin
      Continue;
    end;

    if Result >= 0 then
    begin

      if FOptions.Selection = nfjExactlyOne then
      begin
        raise ENyxFlow.Create('Flow join requires exactly one available branch');
      end;

      if FOptions.Selection = nfjFirstAvailable then
      begin
        Continue;
      end;
    end;
    Result := LIndex;
  end;

  if (Result < 0) and (FOptions.Empty = nfjeError) then
  begin
    raise ENyxFlow.Create('Flow join has no available branch');
  end;
end;

function TFlowJoin.ShouldInvoke(const AInputs: TNyxFlowValues): Boolean;
begin
  Result := Choose(AInputs) >= 0;
end;

function TFlowJoin.TrySelect(const AInputs: TNyxFlowValues; out AValue: TNyxFlowValue;
  out ASource: TNyxFlowPortRef): Boolean;
var
  LIndex: Integer;
begin
  AValue := Default(TNyxFlowValue);
  ASource := Default(TNyxFlowPortRef);
  LIndex := Choose(AInputs);
  Result := LIndex >= 0;

  if Result then
  begin
    ASource := FOptions.Port(LIndex);
    AValue := AInputs.Value(ASource);
  end;
end;

function TFlowJoin.Execute(const AInputs: TNyxFlowValues;
  const ACancellation: INyxCancellationScope): TNyxFlowValues;
var
  LValue: TNyxFlowValue;
  LSource: TNyxFlowPortRef;
begin

  if not TrySelect(AInputs, LValue, LSource) then
  begin
    raise ENyxFlow.Create('Empty join has no output; use TrySelect or graph skip provenance');
  end;
  Result := NyxFlowValues.WithValue(NyxFlowJoinPort(nfjpValue), LValue)
    .WithValue(NyxFlowJoinPort(nfjpSource), NyxFlowText(LSource.Name));
end;

function NewNyxFlowJoin(const AOptions: TNyxFlowJoinOptions): INyxFlowJoin;
begin
  Result := TFlowJoin.Create(AOptions);
end;

end.
