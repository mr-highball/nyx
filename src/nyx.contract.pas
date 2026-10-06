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

unit nyx.contract;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  SysUtils,
  nyx.text,
  nyx.dates,
  nyx.types,
  nyx.data,
  nyx.state;

const
  { One explicitly owned node extension. Keeping the declaration in the existing
    version-1 extension envelope preserves other application fields and avoids
    a new flattened wire-field collision. The payload has its own schema version. }
  NyxContractKey: TNyxText = 'nyx.contract';
  NyxMaximumContractFields = 128;
  NyxMaximumDomainChoices = 128;

type
  { Domain rejection is also state admission rejection. Existing application
    validators can catch ENyxState while more specific callers distinguish
    contract failures without parsing a message. }
  ENyxContract = class(ENyxState);

  { Immutable scalar specification. Default records are invalid; NyxNoDomain is
    the explicit absence of a value contract. ToData/FromData are the descriptor
    boundary. Typed factories below are the normal handwritten authoring API.
    Copies own JSON text, without shared mutable arrays or borrowed containers. }
  TNyxValueDomain = record
  private
    FData: TNyxDataValue;
    FKind: TNyxStateKind;
    function GetDefined: Boolean;
    function GetKind: TNyxStateKind;
    function GetCalendarDate: Boolean;
  public
    class function FromData(const AData: TNyxDataValue): TNyxValueDomain; static;
    function ToData: TNyxDataValue;
    function Copy: TNyxValueDomain;
    procedure Validate;
    { Decode exact control wire text; never guess Boolean/numeric meaning from
      a caption. Integer spelling follows the portable control boundary;
      numbers are finite Doubles, text/choice equality remains exact Unicode. }
    function ReadWire(const AText: TNyxText): TNyxDataValue;
    procedure Admit(const AValue: TNyxDataValue);
    property Defined: Boolean read GetDefined;
    property Kind: TNyxStateKind read GetKind;
    { Date domains still bind exact text stores; this closed format adds calendar
      admission instead of introducing a locale-dependent state representation. }
    property CalendarDate: Boolean read GetCalendarDate;
  end;

  { Distinct builder families keep range/choice arguments typed. Every fluent
    operation returns a new immutable specification, retaining its baseline. }
  TNyxTextDomain = record
  private
    FDomain: TNyxValueDomain;
  public
    function Choices(const AValues: array of TNyxText): TNyxTextDomain; overload;
    { Typed date choices require CalendarDate/NyxDateDomain; a no-date entry is
      an explicit optional choice. Every value is copied into the specification. }
    function Choices(const AValues: array of TNyxCalendarDate): TNyxTextDomain; overload;
    { Return a new canonical calendar specification. Empty is an optional date;
      choices and date ranges still admit only actual Gregorian days. }
    function CalendarDate: TNyxTextDomain;
    { Inclusive bounds require defined dates in ascending order. Empty values
      remain admitted; required-value policy belongs to application validation. }
    function Range(const AMinimum, AMaximum: TNyxCalendarDate): TNyxTextDomain;
    function Definition: TNyxValueDomain;
  end;

  TNyxBooleanDomain = record
  private
    FDomain: TNyxValueDomain;
  public
    function Choices(const AValues: array of Boolean): TNyxBooleanDomain;
    function Definition: TNyxValueDomain;
  end;

  TNyxIntegerDomain = record
  private
    FDomain: TNyxValueDomain;
  public
    function Range(AMinimum, AMaximum: Integer): TNyxIntegerDomain;
    function Choices(const AValues: array of Integer): TNyxIntegerDomain;
    function Definition: TNyxValueDomain;
  end;

  TNyxNumberDomain = record
  private
    FDomain: TNyxValueDomain;
  public
    function Range(AMinimum, AMaximum: Double): TNyxNumberDomain;
    function Choices(const AValues: array of Double): TNyxNumberDomain;
    function Definition: TNyxValueDomain;
  end;

  TNyxEventValueSource = (nvsNone, nvsTarget, nvsOrigin, nvsSource, nvsPart);
  TNyxEventValueRef = record
  private
    FSource: TNyxEventValueSource;
    FPart: TNyxText;
    FMarker: TNyxText;
  public
    procedure Validate;
    function Copy: TNyxEventValueRef;
    property Source: TNyxEventValueSource read FSource;
    property Part: TNyxText read FPart;
  end;

  TNyxFieldContract = record
    Part: TNyxPartRef;
    Domain: TNyxValueDomain;
  end;
  TNyxEventContract = record
    Trigger: TNyxTrigger;
    ValueSource: TNyxEventValueRef;
    Domain: TNyxValueDomain;
  end;

  { Node-owned fluent facade borrowing that node's extension store. Never free
    or retain it after its node. Readers return independent specifications;
    updates validate a complete candidate before replacing the single owned
    extension. Clone/recipe/reusable overlays retain existing store ownership.
    Field/event overrides replace whole matching declarations, preserving order. }
  TNyxContract = class
  private
    FExtensions: TNyxExtensions;
    FKey: TNyxExtensionRef;
    { The extension store is externally editable at its explicit boundary.
      Compare immutable snapshots before using this cache; never trust a node
      revision that an extension edit could bypass. Cached readers own values. }
    FReadSnapshot: TNyxDataValue;
    FHasReadSnapshot: Boolean;
    FHasValue: Boolean;
    FValue: TNyxValueDomain;
    FFields: array of TNyxFieldContract;
    FEvents: array of TNyxEventContract;
    procedure Refresh;
    procedure Publish(const AData: TNyxDataValue);
    procedure PutField(const APart: TNyxPartRef; const ADomain: TNyxValueDomain);
    procedure PutEvent(ATrigger: TNyxTrigger; const ASource: TNyxEventValueRef;
      const ADomain: TNyxValueDomain);
  public
    constructor Create(AExtensions: TNyxExtensions);
    function Snapshot: TNyxDataValue;
    procedure Validate;
    procedure Assign(ASource: TNyxContract);
    function FindValue(out ADomain: TNyxValueDomain): Boolean;
    function FieldCount: Integer;
    function FieldAt(AIndex: Integer): TNyxFieldContract;
    function EventCount: Integer;
    function EventAt(AIndex: Integer): TNyxEventContract;
    function FindEvent(ATrigger: TNyxTrigger; out AEvent: TNyxEventContract): Boolean;
    function NoValue: TNyxContract;
    function Signal(ATrigger: TNyxTrigger): TNyxContract;
    { Explicit descriptor/import boundary. Default generated source emits the
      typed factories; noncanonical admitted descriptors retain exact data here. }
    function Metadata(const AData: TNyxDataValue): TNyxContract;
    function Value(const ADomain: TNyxTextDomain): TNyxContract; overload;
    function Field(const APart: TNyxPartRef; const ADomain: TNyxTextDomain): TNyxContract; overload;
    function On(ATrigger: TNyxTrigger; const ASource: TNyxEventValueRef;
      const ADomain: TNyxTextDomain): TNyxContract; overload;
    function Value(const ADomain: TNyxBooleanDomain): TNyxContract; overload;
    function Field(const APart: TNyxPartRef; const ADomain: TNyxBooleanDomain): TNyxContract; overload;
    function On(ATrigger: TNyxTrigger; const ASource: TNyxEventValueRef;
      const ADomain: TNyxBooleanDomain): TNyxContract; overload;
    function Value(const ADomain: TNyxIntegerDomain): TNyxContract; overload;
    function Field(const APart: TNyxPartRef; const ADomain: TNyxIntegerDomain): TNyxContract; overload;
    function On(ATrigger: TNyxTrigger; const ASource: TNyxEventValueRef;
      const ADomain: TNyxIntegerDomain): TNyxContract; overload;
    function Value(const ADomain: TNyxNumberDomain): TNyxContract; overload;
    function Field(const APart: TNyxPartRef; const ADomain: TNyxNumberDomain): TNyxContract; overload;
    function On(ATrigger: TNyxTrigger; const ASource: TNyxEventValueRef;
      const ADomain: TNyxNumberDomain): TNyxContract; overload;
  end;

{ Explicit descriptor helpers for schema/codec integration. Domain enum choices
  are closed; extension registration never patches the built-in control enum. }
function NyxNoDomain: TNyxValueDomain;
function NyxScalarDomain(AKind: TNyxStateKind): TNyxValueDomain;
function NyxTextDomain: TNyxTextDomain;
function NyxDateDomain: TNyxTextDomain; overload;
{ Enrich an existing text specification without dropping choices or bounds.
  Physical date projections also use this for legacy text declarations. }
function NyxDateDomain(const ABase: TNyxValueDomain): TNyxTextDomain; overload;
function NyxBooleanDomain: TNyxBooleanDomain;
function NyxIntegerDomain: TNyxIntegerDomain;
function NyxNumberDomain: TNyxNumberDomain;
function NyxNoEventValue: TNyxEventValueRef;
function NyxTargetValue: TNyxEventValueRef;
function NyxOriginValue: TNyxEventValueRef;
function NyxSourceValue: TNyxEventValueRef;
function NyxPartValue(const APart: TNyxPartRef): TNyxEventValueRef;

implementation

const
  CValueSourceNames: array[TNyxEventValueSource] of TNyxText =
    ('none', 'target', 'origin', 'source', 'part');

function HasField(const AData: TNyxDataValue; const AName: TNyxText): Boolean;
var
  LIndex: Integer;
begin
  Result := False;
  for LIndex := 0 to AData.Count - 1 do
  begin

    if AData.Key(LIndex) = AName then
    begin
      Exit(True);
    end;
  end;
end;

procedure CheckMembers(const AData: TNyxDataValue; const ANames: TNyxText);
var
  LIndex: Integer;
begin

  if AData.Kind <> ndObject then
  begin
    raise ENyxContract.Create('Contract descriptor requires an object');
  end;
  for LIndex := 0 to AData.Count - 1 do
  begin

    if (Pos('|', AData.Key(LIndex)) > 0) or
      (Pos('|' + AData.Key(LIndex) + '|', ANames) = 0) then
    begin
      raise ENyxContract.Create('Unknown contract descriptor member: ' + AData.Key(LIndex));
    end;
  end;
end;

function ReplaceField(const AData: TNyxDataValue; const AName: TNyxText;
  const AValue: TNyxDataValue): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LFound: Boolean;
begin
  SetLength(LFields, AData.Count);
  LFound := False;
  for LIndex := 0 to AData.Count - 1 do
  begin
    LFields[LIndex] := NyxField(AData.Key(LIndex), AData.Field(AData.Key(LIndex)));

    if AData.Key(LIndex) = AName then
    begin
      LFields[LIndex] := NyxField(AName, AValue);
      LFound := True;
    end;
  end;

  if not LFound then
  begin
    LIndex := Length(LFields);
    SetLength(LFields, LIndex + 1);
    LFields[LIndex] := NyxField(AName, AValue);
  end;
  Result := NyxObject(LFields);
end;

function NyxNoDomain: TNyxValueDomain;
begin
  Result.FData := NyxNull;
  Result.FKind := nskText;
end;

function NyxScalarDomain(AKind: TNyxStateKind): TNyxValueDomain;
begin
  Result := TNyxValueDomain.FromData(NyxObject([
    NyxField('type', NyxData(NyxStateKindName(AKind)))
  ]));
end;

function TNyxValueDomain.GetDefined: Boolean;
begin
  FData.Validate;
  Result := FData.Kind <> ndNull;
end;

function TNyxValueDomain.GetKind: TNyxStateKind;
begin

  if not Defined then
  begin
    raise ENyxContract.Create('No scalar value is declared');
  end;
  Result := FKind;
end;

function TNyxValueDomain.GetCalendarDate: Boolean;
begin
  Result := Defined and HasField(FData, 'format') and
    (FData.Field('format').AsText = 'date');
end;

class function TNyxValueDomain.FromData(const AData: TNyxDataValue): TNyxValueDomain;
var
  LKind: TNyxStateKind;
  LName: TNyxText;
  LFound: Boolean;
begin
  Result.FData := AData.Copy;
  Result.FKind := nskText;
  LFound := not Result.Defined;

  if Result.Defined then
  begin
    LName := AData.Field('type').AsText;
  end;
  for LKind := Low(TNyxStateKind) to High(TNyxStateKind) do
  begin

    if Result.Defined and (NyxStateKindName(LKind) = LName) then
    begin
      Result.FKind := LKind;
      LFound := True;
      Break;
    end;
  end;

  if not LFound then
  begin
    raise ENyxContract.Create('Unknown scalar domain type: ' + LName);
  end;
  Result.Validate;
end;

function TNyxValueDomain.ToData: TNyxDataValue;
begin
  { Private immutable data was completely admitted by FromData. Copying it
    cannot change a choice/range, so readers need only the construction guard. }
  Result := FData.Copy;
end;

function TNyxValueDomain.Copy: TNyxValueDomain;
begin
  FData.Validate;
  Result := Self;
end;

function SameDomainScalar(AKind: TNyxStateKind;
  const ALeft, ARight: TNyxDataValue): Boolean;
begin
  { Decimal spelling belongs to persistence, not numeric choice membership.
    In particular 1, 1.0 and 1e0 designate the same admitted Double. }

  if AKind = nskNumber then
  begin
    Exit(ALeft.AsNumber = ARight.AsNumber);
  end;
  Result := ALeft.ToJSON = ARight.ToJSON;
end;

procedure TNyxValueDomain.Validate;
var
  LKind: TNyxStateKind;
  LChoices: TNyxDataValue;
  LIndex: Integer;
  LPrevious: Integer;
  LMinimumDate: TNyxCalendarDate;
  LMaximumDate: TNyxCalendarDate;
begin
  FData.Validate;

  if not Defined then
  begin
    Exit;
  end;
  CheckMembers(FData, '|type|min|max|choices|format|');
  LKind := Kind;

  if HasField(FData, 'format') and
    ((LKind <> nskText) or (FData.Field('format').AsText <> 'date')) then
  begin
    raise ENyxContract.Create('Only text domains support the canonical date format');
  end;

  if HasField(FData, 'min') <> HasField(FData, 'max') then
  begin
    raise ENyxContract.Create('Domain range requires both bounds');
  end;

  if HasField(FData, 'min') then
  begin

    if CalendarDate then
    begin

      if not TryNyxDate(FData.Field('min').AsText, LMinimumDate) or
        not TryNyxDate(FData.Field('max').AsText, LMaximumDate) or
        not LMinimumDate.Defined or not LMaximumDate.Defined or
        (LMinimumDate.Compare(LMaximumDate) > 0) then
      begin
        raise ENyxContract.Create('Calendar range requires ascending defined dates');
      end;
    end
    else
    begin

      if not (LKind in [nskInteger, nskNumber]) then
      begin
        raise ENyxContract.Create('Only numeric or calendar domains have ranges');
      end;

      if LKind = nskInteger then
      begin
        FData.Field('min').AsInteger;
        FData.Field('max').AsInteger;
      end;

      if FData.Field('min').AsNumber > FData.Field('max').AsNumber then
      begin
        raise ENyxContract.Create('Domain minimum exceeds maximum');
      end;
    end;
  end;

  if HasField(FData, 'choices') then
  begin
    LChoices := FData.Field('choices');

    if (LChoices.Kind <> ndArray) or (LChoices.Count = 0) or
      (LChoices.Count > NyxMaximumDomainChoices) then
    begin
      raise ENyxContract.Create('Domain choices require 1..128 values');
    end;
    for LIndex := 0 to LChoices.Count - 1 do
    begin
      { Admit below checks the scalar/range and membership. Each choice is a
        member by construction; duplicates are a separate contract error. }
      Admit(LChoices.Item(LIndex));
      for LPrevious := 0 to LIndex - 1 do
      begin

        if SameDomainScalar(LKind, LChoices.Item(LPrevious), LChoices.Item(LIndex)) then
        begin
          raise ENyxContract.Create('Duplicate domain choice');
        end;
      end;
    end;
  end;
end;

procedure TNyxValueDomain.Admit(const AValue: TNyxDataValue);
var
  LChoices: TNyxDataValue;
  LIndex: Integer;
  LFound: Boolean;
  LNumber: Double;
  LDate: TNyxCalendarDate;
begin
  AValue.Validate;
  LDate := NyxNoDate;
  case Kind of
    nskText:
      begin
        AValue.AsText;

        if CalendarDate and not TryNyxDate(AValue.AsText, LDate) then
        begin
          raise ENyxContract.Create('Value requires a valid calendar YYYY-MM-DD date');
        end;
      end;
    nskBoolean:
      begin
        AValue.AsBoolean;
      end;
    nskInteger:
      begin
        AValue.AsInteger;
      end;
    nskNumber:
      begin
        AValue.AsNumber;
      end;
  end;

  if HasField(FData, 'min') then
  begin

    if CalendarDate then
    begin
      { Empty is deliberately independent of the inclusive calendar bounds. }

      if LDate.Defined and
        ((LDate.Compare(TNyxCalendarDate.FromText(FData.Field('min').AsText)) < 0) or
        (LDate.Compare(TNyxCalendarDate.FromText(FData.Field('max').AsText)) > 0)) then
      begin
        raise ENyxContract.Create('Date is outside its declared calendar range');
      end;
    end
    else
    begin
      LNumber := AValue.AsNumber;

      if (LNumber < FData.Field('min').AsNumber) or
        (LNumber > FData.Field('max').AsNumber) then
      begin
        raise ENyxContract.Create('Value is outside its declared domain range');
      end;
    end;
  end;

  if HasField(FData, 'choices') then
  begin
    LChoices := FData.Field('choices');
    LFound := False;
    for LIndex := 0 to LChoices.Count - 1 do
    begin

      if SameDomainScalar(Kind, LChoices.Item(LIndex), AValue) then
      begin
        LFound := True;
        Break;
      end;
    end;

    if not LFound then
    begin
      raise ENyxContract.Create('Value is outside its declared domain choices');
    end;
  end;
end;

function TNyxValueDomain.ReadWire(const AText: TNyxText): TNyxDataValue;
var
  LInteger: Integer;
  LNumber: Double;
begin
  case Kind of
    nskText:
      begin
        Result := NyxData(AText);
      end;
    nskBoolean:
      begin

        if (AText <> 'true') and (AText <> 'false') then
        begin
          raise ENyxContract.Create('Boolean domain requires true or false');
        end;
        Result := NyxData(AText = 'true');
      end;
    nskInteger:
      begin

        if not TryNyxStateInteger(AText, LInteger) then
        begin
          raise ENyxContract.Create('Integer domain requires a complete signed integer');
        end;
        Result := NyxData(LInteger);
      end;
    nskNumber:
      begin

        if not TryNyxStateNumber(AText, LNumber) then
        begin
          raise ENyxContract.Create('Number domain requires a complete finite number');
        end;
        Result := NyxData(LNumber);
      end;
  end;
  Admit(Result);
end;

function WithBounds(const ADomain: TNyxValueDomain;
  const AMinimum, AMaximum: TNyxDataValue): TNyxValueDomain;
begin
  Result := TNyxValueDomain.FromData(ReplaceField(
    ReplaceField(ADomain.ToData, 'min', AMinimum), 'max', AMaximum));
end;

function WithChoices(const ADomain: TNyxValueDomain;
  const AChoices: TNyxDataValue): TNyxValueDomain;
begin
  Result := TNyxValueDomain.FromData(ReplaceField(ADomain.ToData, 'choices', AChoices));
end;

procedure CheckPart(const APart: TNyxText);
begin

  if (APart = '') or (APart = '.') or (APart[1] = '/') or
    (APart[Length(APart)] = '/') or (Pos('//', APart) > 0) then
  begin
    raise ENyxContract.Create('Contract field requires a named part path');
  end;
end;

function EventValue(ASource: TNyxEventValueSource; const APart: TNyxText): TNyxEventValueRef;
begin
  Result.FSource := ASource;
  Result.FPart := APart;
  Result.FMarker := 'nyx.event-value/1';
  Result.Validate;
end;

function NyxNoEventValue: TNyxEventValueRef;
begin
  Result := EventValue(nvsNone, '');
end;

function NyxTargetValue: TNyxEventValueRef;
begin
  Result := EventValue(nvsTarget, '');
end;

function NyxOriginValue: TNyxEventValueRef;
begin
  Result := EventValue(nvsOrigin, '');
end;

function NyxSourceValue: TNyxEventValueRef;
begin
  Result := EventValue(nvsSource, '');
end;

function NyxPartValue(const APart: TNyxPartRef): TNyxEventValueRef;
begin
  Result := EventValue(nvsPart, APart.Name);
end;

procedure TNyxEventValueRef.Validate;
begin

  if (FMarker <> 'nyx.event-value/1') or
    not (FSource in [nvsNone, nvsTarget, nvsOrigin, nvsSource, nvsPart]) then
  begin
    raise ENyxContract.Create('Construct a typed event value reference before use');
  end;

  if FSource = nvsPart then
  begin
    CheckPart(FPart);
  end
  else if FPart <> '' then
  begin
    raise ENyxContract.Create('Only a named-part event value has a part path');
  end;
end;

function TNyxEventValueRef.Copy: TNyxEventValueRef;
begin
  Validate;
  Result.FSource := FSource;
  Result.FPart := FPart;
  Result.FMarker := FMarker;
end;

function DecodeEvent(const AData: TNyxDataValue): TNyxEventContract;
var
  LSource: TNyxEventValueSource;
  LName: TNyxText;
  LPart: TNyxText;
begin
  CheckMembers(AData, '|trigger|source|part|domain|');
  LName := AData.Field('trigger').AsText;

  if not TryNyxTrigger(LName, Result.Trigger) or
    not NyxIsRuntimeTrigger(Result.Trigger) then
  begin
    raise ENyxContract.Create('Event contract requires a runtime trigger');
  end;
  LName := AData.Field('source').AsText;
  LPart := '';

  if HasField(AData, 'part') then
  begin
    LPart := AData.Field('part').AsText;
  end;
  for LSource := Low(TNyxEventValueSource) to High(TNyxEventValueSource) do
  begin

    if CValueSourceNames[LSource] = LName then
    begin
      Result.ValueSource := EventValue(LSource, LPart);
      Result.Domain := TNyxValueDomain.FromData(AData.Field('domain'));

      if (LSource = nvsNone) <> not Result.Domain.Defined then
      begin
        raise ENyxContract.Create('Signals have no domain; value events require one');
      end;
      Exit;
    end;
  end;
  raise ENyxContract.Create('Unknown event value source');
end;

procedure ValidateSpecification(const AData: TNyxDataValue);
var
  LEntries: TNyxDataValue;
  LEntry: TNyxDataValue;
  LDomain: TNyxValueDomain;
  LEvent: TNyxEventContract;
  LIndex: Integer;
  LPrevious: Integer;
begin
  CheckMembers(AData, '|version|value|fields|events|');

  if AData.Field('version').AsInteger <> 1 then
  begin
    raise ENyxContract.Create('Unsupported component contract version');
  end;

  if HasField(AData, 'value') then
  begin
    LDomain := TNyxValueDomain.FromData(AData.Field('value'));
  end;

  if HasField(AData, 'fields') then
  begin
    LEntries := AData.Field('fields');

    if (LEntries.Kind <> ndArray) or (LEntries.Count > NyxMaximumContractFields) then
    begin
      raise ENyxContract.Create('Component field contract exceeds its array budget');
    end;
    for LIndex := 0 to LEntries.Count - 1 do
    begin
      LEntry := LEntries.Item(LIndex);
      CheckMembers(LEntry, '|part|domain|');
      CheckPart(LEntry.Field('part').AsText);
      LDomain := TNyxValueDomain.FromData(LEntry.Field('domain'));

      if not LDomain.Defined then
      begin
        raise ENyxContract.Create('Named field requires a scalar domain');
      end;
      for LPrevious := 0 to LIndex - 1 do
      begin

        if LEntries.Item(LPrevious).Field('part').AsText = LEntry.Field('part').AsText then
        begin
          raise ENyxContract.Create('Duplicate named field contract');
        end;
      end;
    end;
  end;

  if HasField(AData, 'events') then
  begin
    LEntries := AData.Field('events');

    { One declaration per canonical trigger bounds the descriptor. The registry
      grows with supported families; its original four-trigger ceiling would
      reject valid independent contracts for the new phases. Runtime admission
      and duplicate checks below still reject designer or repeated identities. }

    if (LEntries.Kind <> ndArray) or (LEntries.Count > Ord(High(TNyxTrigger)) + 1) then
    begin
      raise ENyxContract.Create('Component event contract exceeds the canonical trigger registry');
    end;
    for LIndex := 0 to LEntries.Count - 1 do
    begin
      LEvent := DecodeEvent(LEntries.Item(LIndex));
      for LPrevious := 0 to LIndex - 1 do
      begin

        if LEntries.Item(LPrevious).Field('trigger').AsText = NyxTriggerName(LEvent.Trigger) then
        begin
          raise ENyxContract.Create('Duplicate runtime trigger contract');
        end;
      end;
    end;
  end;
end;

constructor TNyxContract.Create(AExtensions: TNyxExtensions);
begin
  inherited Create;

  if (AExtensions = nil) or (AExtensions.Scope <> nesNode) then
  begin
    raise ENyxContract.Create('Component contract requires its node extension store');
  end;
  FExtensions := AExtensions;
  FKey := NyxExtension(NyxContractKey);
end;

function TNyxContract.Snapshot: TNyxDataValue;
begin

  if FExtensions.Has(FKey) then
  begin
    Result := FExtensions.Value(FKey);
  end
  else
  begin
    Result := NyxObject([NyxField('version', NyxData(1))]);
  end;
end;

procedure TNyxContract.Validate;
begin

  if not FExtensions.Has(FKey) then
  begin
    Exit;
  end;
  Refresh;
end;

procedure TNyxContract.Refresh;
var
  LSnapshot: TNyxDataValue;
  LEntries: TNyxDataValue;
  LEntry: TNyxDataValue;
  LIndex: Integer;
  LFields: array of TNyxFieldContract;
  LEvents: array of TNyxEventContract;
  LHasValue: Boolean;
  LValue: TNyxValueDomain;
begin
  LSnapshot := Snapshot;

  if FHasReadSnapshot and (FReadSnapshot.ToJSON = LSnapshot.ToJSON) then
  begin
    Exit;
  end;
  ValidateSpecification(LSnapshot);
  LHasValue := HasField(LSnapshot, 'value');
  LValue := NyxNoDomain;

  if LHasValue then
  begin
    LValue := TNyxValueDomain.FromData(LSnapshot.Field('value'));
  end;
  LFields := nil;

  if HasField(LSnapshot, 'fields') then
  begin
    LEntries := LSnapshot.Field('fields');
    SetLength(LFields, LEntries.Count);
    for LIndex := 0 to High(LFields) do
    begin
      LEntry := LEntries.Item(LIndex);
      LFields[LIndex].Part := NyxPart(LEntry.Field('part').AsText);
      LFields[LIndex].Domain := TNyxValueDomain.FromData(LEntry.Field('domain'));
    end;
  end;
  LEvents := nil;

  if HasField(LSnapshot, 'events') then
  begin
    LEntries := LSnapshot.Field('events');
    SetLength(LEvents, LEntries.Count);
    for LIndex := 0 to High(LEvents) do
    begin
      LEvents[LIndex] := DecodeEvent(LEntries.Item(LIndex));
    end;
  end;
  { Publish the read cache only after the whole current namespace is admitted.
    A failed external metadata edit can neither poison prior snapshots nor make
    an old contract appear current. Returning records never exposes these arrays. }
  FFields := LFields;
  FEvents := LEvents;
  FHasValue := LHasValue;
  FValue := LValue;
  FReadSnapshot := LSnapshot.Copy;
  FHasReadSnapshot := True;
end;

procedure TNyxContract.Publish(const AData: TNyxDataValue);
begin
  ValidateSpecification(AData);
  FExtensions.SetValue(FKey, AData);
end;

procedure TNyxContract.Assign(ASource: TNyxContract);
begin

  if ASource = nil then
  begin
    raise ENyxContract.Create('Contract assignment requires a source');
  end;
  Publish(ASource.Snapshot);
end;

function TNyxContract.FindValue(out ADomain: TNyxValueDomain): Boolean;
begin
  ADomain := NyxNoDomain;
  Result := False;

  if not FExtensions.Has(FKey) then
  begin
    Exit;
  end;
  Refresh;
  Result := FHasValue;

  if Result then
  begin
    ADomain := FValue.Copy;
  end;
end;

function TNyxContract.FieldCount: Integer;
begin
  Result := 0;

  if not FExtensions.Has(FKey) then
  begin
    Exit;
  end;
  Refresh;
  Result := Length(FFields);
end;

function TNyxContract.FieldAt(AIndex: Integer): TNyxFieldContract;
begin
  Refresh;

  if (AIndex < 0) or (AIndex >= Length(FFields)) then
  begin
    raise ENyxContract.Create('Field contract index out of range');
  end;
  Result.Part := FFields[AIndex].Part;
  Result.Domain := FFields[AIndex].Domain.Copy;
end;

function TNyxContract.EventCount: Integer;
begin
  Result := 0;

  if not FExtensions.Has(FKey) then
  begin
    Exit;
  end;
  Refresh;
  Result := Length(FEvents);
end;

function TNyxContract.EventAt(AIndex: Integer): TNyxEventContract;
begin
  Refresh;

  if (AIndex < 0) or (AIndex >= Length(FEvents)) then
  begin
    raise ENyxContract.Create('Event contract index out of range');
  end;
  Result.Trigger := FEvents[AIndex].Trigger;
  Result.ValueSource := FEvents[AIndex].ValueSource.Copy;
  Result.Domain := FEvents[AIndex].Domain.Copy;
end;

function TNyxContract.FindEvent(ATrigger: TNyxTrigger;
  out AEvent: TNyxEventContract): Boolean;
var
  LIndex: Integer;
begin
  AEvent.Trigger := ATrigger;
  AEvent.ValueSource := NyxNoEventValue;
  AEvent.Domain := NyxNoDomain;
  for LIndex := 0 to EventCount - 1 do
  begin
    AEvent := EventAt(LIndex);

    if AEvent.Trigger = ATrigger then
    begin
      Exit(True);
    end;
  end;
  Result := False;
end;

procedure TNyxContract.PutField(const APart: TNyxPartRef; const ADomain: TNyxValueDomain);
var
  LData: TNyxDataValue;
  LFields: TNyxDataValue;
  LItems: array of TNyxDataValue;
  LCount: Integer;
  LIndex: Integer;
  LTarget: Integer;
begin
  CheckPart(APart.Name);
  LData := Snapshot;
  LCount := FieldCount;
  SetLength(LItems, LCount);
  LTarget := LCount;

  if LCount > 0 then
  begin
    LFields := LData.Field('fields');
    for LIndex := 0 to LCount - 1 do
    begin
      LItems[LIndex] := LFields.Item(LIndex);

      if LItems[LIndex].Field('part').AsText = APart.Name then
      begin
        LTarget := LIndex;
      end;
    end;
  end;

  if LTarget = LCount then
  begin
    SetLength(LItems, LCount + 1);
  end;
  LItems[LTarget] := NyxObject([
    NyxField('part', NyxData(APart.Name)), NyxField('domain', ADomain.ToData)
  ]);
  Publish(ReplaceField(LData, 'fields', NyxArray(LItems)));
end;

procedure TNyxContract.PutEvent(ATrigger: TNyxTrigger;
  const ASource: TNyxEventValueRef; const ADomain: TNyxValueDomain);
var
  LData: TNyxDataValue;
  LEvents: TNyxDataValue;
  LEntry: TNyxDataValue;
  LItems: array of TNyxDataValue;
  LCount: Integer;
  LIndex: Integer;
  LTarget: Integer;
begin
  ASource.Validate;

  if ATrigger in [ntDesignSelect, ntDesignValue] then
  begin
    raise ENyxContract.Create('Event contract requires a runtime trigger');
  end;
  LData := Snapshot;
  LCount := EventCount;
  SetLength(LItems, LCount);
  LTarget := LCount;

  if LCount > 0 then
  begin
    LEvents := LData.Field('events');
    for LIndex := 0 to LCount - 1 do
    begin
      LItems[LIndex] := LEvents.Item(LIndex);

      if LItems[LIndex].Field('trigger').AsText = NyxTriggerName(ATrigger) then
      begin
        LTarget := LIndex;
      end;
    end;
  end;

  if LTarget = LCount then
  begin
    SetLength(LItems, LCount + 1);
  end;
  LEntry := NyxObject([
    NyxField('trigger', NyxData(NyxTriggerName(ATrigger))),
    NyxField('source', NyxData(CValueSourceNames[ASource.Source])),
    NyxField('domain', ADomain.ToData)
  ]);

  if ASource.Source = nvsPart then
  begin
    LEntry := ReplaceField(LEntry, 'part', NyxData(ASource.Part));
  end;
  LItems[LTarget] := LEntry;
  Publish(ReplaceField(LData, 'events', NyxArray(LItems)));
end;

function TNyxContract.NoValue: TNyxContract;
begin
  Publish(ReplaceField(Snapshot, 'value', NyxNull));
  Result := Self;
end;

function TNyxContract.Signal(ATrigger: TNyxTrigger): TNyxContract;
begin
  PutEvent(ATrigger, NyxNoEventValue, NyxNoDomain);
  Result := Self;
end;

function TNyxContract.Metadata(const AData: TNyxDataValue): TNyxContract;
begin
  Publish(AData);
  Result := Self;
end;

function NyxTextDomain: TNyxTextDomain;
begin
  Result.FDomain := NyxScalarDomain(nskText);
end;

function NyxDateDomain: TNyxTextDomain;
begin
  Result := NyxTextDomain.CalendarDate;
end;

function NyxDateDomain(const ABase: TNyxValueDomain): TNyxTextDomain;
begin
  Result.FDomain := ABase.Copy;
  Result := Result.CalendarDate;
end;

function TNyxTextDomain.CalendarDate: TNyxTextDomain;
begin

  if FDomain.Kind <> nskText then
  begin
    raise ENyxContract.Create('Calendar format requires a text domain');
  end;
  Result.FDomain := TNyxValueDomain.FromData(ReplaceField(
    FDomain.ToData, 'format', NyxData('date')));
end;

function TNyxTextDomain.Range(const AMinimum, AMaximum: TNyxCalendarDate): TNyxTextDomain;
var
  LData: TNyxDataValue;
begin

  if not FDomain.CalendarDate or not AMinimum.Defined or not AMaximum.Defined then
  begin
    raise ENyxContract.Create('Date range requires a calendar domain and defined bounds');
  end;
  LData := ReplaceField(FDomain.ToData, 'min', NyxData(AMinimum.ToText));
  LData := ReplaceField(LData, 'max', NyxData(AMaximum.ToText));
  Result.FDomain := TNyxValueDomain.FromData(LData);
end;

function TNyxTextDomain.Definition: TNyxValueDomain;
begin
  Result := FDomain.Copy;
end;

function TNyxTextDomain.Choices(const AValues: array of TNyxText): TNyxTextDomain;
var
  LItems: array of TNyxDataValue;
  LIndex: Integer;
begin
  SetLength(LItems, Length(AValues));
  for LIndex := 0 to High(AValues) do
  begin
    LItems[LIndex] := NyxData(AValues[LIndex]);
  end;
  Result.FDomain := WithChoices(FDomain, NyxArray(LItems));
end;

function TNyxTextDomain.Choices(const AValues: array of TNyxCalendarDate): TNyxTextDomain;
var
  LItems: array of TNyxDataValue;
  LIndex: Integer;
begin

  if not FDomain.CalendarDate then
  begin
    raise ENyxContract.Create('Typed date choices require a calendar domain');
  end;
  SetLength(LItems, Length(AValues));
  for LIndex := 0 to High(AValues) do
  begin
    LItems[LIndex] := NyxData(AValues[LIndex].ToText);
  end;
  Result.FDomain := WithChoices(FDomain, NyxArray(LItems));
end;

function TNyxContract.Value(const ADomain: TNyxTextDomain): TNyxContract;
begin
  Publish(ReplaceField(Snapshot, 'value', ADomain.Definition.ToData));
  Result := Self;
end;

function TNyxContract.Field(const APart: TNyxPartRef;
  const ADomain: TNyxTextDomain): TNyxContract;
begin
  PutField(APart, ADomain.Definition);
  Result := Self;
end;

function TNyxContract.On(ATrigger: TNyxTrigger; const ASource: TNyxEventValueRef;
  const ADomain: TNyxTextDomain): TNyxContract;
begin
  PutEvent(ATrigger, ASource, ADomain.Definition);
  Result := Self;
end;

function NyxBooleanDomain: TNyxBooleanDomain;
begin
  Result.FDomain := NyxScalarDomain(nskBoolean);
end;

function TNyxBooleanDomain.Definition: TNyxValueDomain;
begin
  Result := FDomain.Copy;
end;

function TNyxBooleanDomain.Choices(const AValues: array of Boolean): TNyxBooleanDomain;
var
  LItems: array of TNyxDataValue;
  LIndex: Integer;
begin
  SetLength(LItems, Length(AValues));
  for LIndex := 0 to High(AValues) do
  begin
    LItems[LIndex] := NyxData(AValues[LIndex]);
  end;
  Result.FDomain := WithChoices(FDomain, NyxArray(LItems));
end;

function TNyxContract.Value(const ADomain: TNyxBooleanDomain): TNyxContract;
begin
  Publish(ReplaceField(Snapshot, 'value', ADomain.Definition.ToData));
  Result := Self;
end;

function TNyxContract.Field(const APart: TNyxPartRef;
  const ADomain: TNyxBooleanDomain): TNyxContract;
begin
  PutField(APart, ADomain.Definition);
  Result := Self;
end;

function TNyxContract.On(ATrigger: TNyxTrigger; const ASource: TNyxEventValueRef;
  const ADomain: TNyxBooleanDomain): TNyxContract;
begin
  PutEvent(ATrigger, ASource, ADomain.Definition);
  Result := Self;
end;

function NyxIntegerDomain: TNyxIntegerDomain;
begin
  Result.FDomain := NyxScalarDomain(nskInteger);
end;

function TNyxIntegerDomain.Definition: TNyxValueDomain;
begin
  Result := FDomain.Copy;
end;

function TNyxIntegerDomain.Range(AMinimum, AMaximum: Integer): TNyxIntegerDomain;
begin
  Result.FDomain := WithBounds(FDomain, NyxData(AMinimum), NyxData(AMaximum));
end;

function TNyxIntegerDomain.Choices(const AValues: array of Integer): TNyxIntegerDomain;
var
  LItems: array of TNyxDataValue;
  LIndex: Integer;
begin
  SetLength(LItems, Length(AValues));
  for LIndex := 0 to High(AValues) do
  begin
    LItems[LIndex] := NyxData(AValues[LIndex]);
  end;
  Result.FDomain := WithChoices(FDomain, NyxArray(LItems));
end;

function TNyxContract.Value(const ADomain: TNyxIntegerDomain): TNyxContract;
begin
  Publish(ReplaceField(Snapshot, 'value', ADomain.Definition.ToData));
  Result := Self;
end;

function TNyxContract.Field(const APart: TNyxPartRef;
  const ADomain: TNyxIntegerDomain): TNyxContract;
begin
  PutField(APart, ADomain.Definition);
  Result := Self;
end;

function TNyxContract.On(ATrigger: TNyxTrigger; const ASource: TNyxEventValueRef;
  const ADomain: TNyxIntegerDomain): TNyxContract;
begin
  PutEvent(ATrigger, ASource, ADomain.Definition);
  Result := Self;
end;

function NyxNumberDomain: TNyxNumberDomain;
begin
  Result.FDomain := NyxScalarDomain(nskNumber);
end;

function TNyxNumberDomain.Definition: TNyxValueDomain;
begin
  Result := FDomain.Copy;
end;

function TNyxNumberDomain.Range(AMinimum, AMaximum: Double): TNyxNumberDomain;
begin
  Result.FDomain := WithBounds(FDomain, NyxData(AMinimum), NyxData(AMaximum));
end;

function TNyxNumberDomain.Choices(const AValues: array of Double): TNyxNumberDomain;
var
  LItems: array of TNyxDataValue;
  LIndex: Integer;
begin
  SetLength(LItems, Length(AValues));
  for LIndex := 0 to High(AValues) do
  begin
    LItems[LIndex] := NyxData(AValues[LIndex]);
  end;
  Result.FDomain := WithChoices(FDomain, NyxArray(LItems));
end;

function TNyxContract.Value(const ADomain: TNyxNumberDomain): TNyxContract;
begin
  Publish(ReplaceField(Snapshot, 'value', ADomain.Definition.ToData));
  Result := Self;
end;

function TNyxContract.Field(const APart: TNyxPartRef;
  const ADomain: TNyxNumberDomain): TNyxContract;
begin
  PutField(APart, ADomain.Definition);
  Result := Self;
end;

function TNyxContract.On(ATrigger: TNyxTrigger; const ASource: TNyxEventValueRef;
  const ADomain: TNyxNumberDomain): TNyxContract;
begin
  PutEvent(ATrigger, ASource, ADomain.Definition);
  Result := Self;
end;

end.
