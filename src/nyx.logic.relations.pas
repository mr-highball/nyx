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
unit nyx.logic.relations;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.logic, nyx.logic.collections,
  nyx.numbers.decimal, nyx.flow.types, nyx.scheduler;

type
  { Null is either refused or a separate equality class. Match never coerces
    null into empty text/false/zero, and explicitly matches null across joins.
    Missing paths and other scalar families always refuse. }
  TNyxJSONNullKeys = (njnReject, njnMatch);

  { Inner/left/full preserve left encounter order and right match order; full
    appends unmatched right rows. Right mirrors left, driven by right order.
    Duplicate keys produce all pairs, bounded by the shared output byte budget. }
  TNyxJSONJoinKind = (njjInner, njjLeft, njjRight, njjFull);

  { Sum/average use exact 96-bit decimal inputs and a wide accumulator; requested
    rounding occurs once at the result. Min/max retain the first tied input's
    original numeric spelling. Count counts rows and requires the root path. }
  TNyxJSONAggregation = (njaSum, njaAverage, njaMinimum, njaMaximum, njaCount);

  { Empty sum/count are zero. Empty average/min/max explicitly return JSON null
    or refuse the complete result. JSON null inside numeric input is not skipped. }
  TNyxJSONEmptyAggregate = (njaeNull, njaeError);

  { Immutable grouped rows. Output is an ordered array of objects with key
    (first original key), items (complete original rows) and count. Typed key
    equality follows the existing Text/Boolean/Decimal collection families.
    No operation retains a query, document, store or UI receiver between calls. }
  INyxJSONGroup = interface(INyxLogicOperation)
    ['{D1411014-6120-4C84-A708-000000000074}']
    function Group(const AItems: TNyxDataValue): TNyxDataValue;
    function GetPath: TNyxDataPath;
    function GetKeyKind: TNyxJSONKeyKind;
    function GetNullKeys: TNyxJSONNullKeys;
    property Path: TNyxDataPath read GetPath;
    property KeyKind: TNyxJSONKeyKind read GetKeyKind;
    property NullKeys: TNyxJSONNullKeys read GetNullKeys;
  end;

  { First complete row per equal typed path key, in first encounter order.
    Every input key is validated, including singleton/empty-result cases.
    Configuration and results are detached immutable values. }
  INyxJSONDistinct = interface(INyxLogicOperation)
    ['{D1411014-6120-4C84-A708-000000000075}']
    function Distinct(const AItems: TNyxDataValue): TNyxDataValue;
    function GetPath: TNyxDataPath;
    function GetKeyKind: TNyxJSONKeyKind;
    function GetNullKeys: TNyxJSONNullKeys;
    property Path: TNyxDataPath read GetPath;
    property KeyKind: TNyxJSONKeyKind read GetKeyKind;
    property NullKeys: TNyxJSONNullKeys read GetNullKeys;
  end;

  { Independent left/right key paths and explicit join/null policy. Every key
    on both inputs is admitted before joining, even when the other array is empty.
    Results have left/right JSON payloads and hasLeft/hasRight Boolean presence;
    absent payloads use null, distinguishable from a present JSON null.
    Graph inputs are nlpLeft/nlpRight; nlpValue is the complete result array. }
  INyxJSONJoin = interface(INyxLogicOperation)
    ['{D1411014-6120-4C84-A708-000000000076}']
    function Join(const ALeft, ARight: TNyxDataValue): TNyxDataValue;
    function GetLeftPath: TNyxDataPath;
    function GetRightPath: TNyxDataPath;
    function GetKeyKind: TNyxJSONKeyKind;
    function GetJoinKind: TNyxJSONJoinKind;
    function GetNullKeys: TNyxJSONNullKeys;
    property LeftPath: TNyxDataPath read GetLeftPath;
    property RightPath: TNyxDataPath read GetRightPath;
    property KeyKind: TNyxJSONKeyKind read GetKeyKind;
    property JoinKind: TNyxJSONJoinKind read GetJoinKind;
    property NullKeys: TNyxJSONNullKeys read GetNullKeys;
  end;

  { One exact scalar result from a JSON array. Numeric operations select Path;
    Count requires root and counts complete rows, without projecting a value.
    Sum/average share Nyx LINQ decimal arithmetic, never an implicit Double.
    Graph input nlpJSON produces one JSON number/null at nlpValue. }
  INyxJSONAggregate = interface(INyxLogicOperation)
    ['{D1411014-6120-4C84-A708-000000000077}']
    function Aggregate(const AItems: TNyxDataValue): TNyxDataValue;
    function GetPath: TNyxDataPath;
    function GetAggregation: TNyxJSONAggregation;
    function GetRounding: TNyxDecimalRounding;
    function GetEmpty: TNyxJSONEmptyAggregate;
    property Path: TNyxDataPath read GetPath;
    property Aggregation: TNyxJSONAggregation read GetAggregation;
    property Rounding: TNyxDecimalRounding read GetRounding;
    property Empty: TNyxJSONEmptyAggregate read GetEmpty;
  end;

function NewNyxJSONGroup(const APath: TNyxDataPath; AKind: TNyxJSONKeyKind;
  ANulls: TNyxJSONNullKeys = njnReject): INyxJSONGroup;
function NewNyxJSONDistinct(const APath: TNyxDataPath; AKind: TNyxJSONKeyKind;
  ANulls: TNyxJSONNullKeys = njnReject): INyxJSONDistinct;
function NewNyxJSONJoin(const ALeftPath, ARightPath: TNyxDataPath;
  AKind: TNyxJSONKeyKind; AJoin: TNyxJSONJoinKind = njjInner;
  ANulls: TNyxJSONNullKeys = njnReject): INyxJSONJoin;
function NewNyxJSONAggregate(const APath: TNyxDataPath; AAggregation: TNyxJSONAggregation;
  ARounding: TNyxDecimalRounding = ndrExact;
  AEmpty: TNyxJSONEmptyAggregate = njaeNull): INyxJSONAggregate;
{ Closed data boundary: known kinds validate their complete fields and enums;
  unknown kinds return nil. Reconstructs pure behavior, never external authority. }
function NyxRelationLogicFromData(const AKind: TNyxLogicKindRef;
  const AConfiguration: TNyxDataValue): INyxLogicOperation;

implementation

uses
  nyx.json, nyx.bytes, nyx.linq, nyx.linq.grouping, nyx.linq.joins,
  nyx.linq.projection, nyx.linq.aggregates, nyx.linq.decimal;

type
  { Per-invocation immutable row. Key admission happens once, retaining its raw
    spelling beside an internal normalized equality key. No interface fields or
    caller-owned arrays; explicit field copies avoid pas2js record aliasing. }
  TKeyedJSON = record
    Value: TNyxDataValue;
    Key: TNyxDataValue;
    Canonical: TNyxText;
  end;

  { Internal sequence aliases keep generic expansion on both compilers typed. }
  TKeyedSequence = INyxSequence<TKeyedJSON>;
  { Owned JSON result sequence, materialized under the shared output budget. }
  TDataSequence = INyxSequence<TNyxDataValue>;

  { Invocation-scoped cached key projection; no path re-evaluation during hashing. }
  TKeySelector = class(TInterfacedObject, INyxSelector<TKeyedJSON, TNyxText>)
  public
    Scope: INyxCancellationScope;
    function Project(const AValue: TKeyedJSON): TNyxText;
  end;

  { Group elements explicitly copy immutable record fields for both compilers. }
  TIdentity = class(TInterfacedObject, INyxSelector<TKeyedJSON, TKeyedJSON>)
  public
    Scope: INyxCancellationScope;
    function Project(const AValue: TKeyedJSON): TKeyedJSON;
  end;

  { Distinct/group materialization returns original whole rows, not internal keys. }
  TValueSelector = class(TInterfacedObject, INyxSelector<TKeyedJSON, TNyxDataValue>)
  public
    Scope: INyxCancellationScope;
    function Project(const AValue: TKeyedJSON): TNyxDataValue;
  end;

  { Typed cached-key equality; its text hash treats embedded NUL as significant.
    Scope belongs to one invocation and no callback retains its operation. }
  TKeyEquality = class(TInterfacedObject, INyxEquality<TKeyedJSON>)
  public
    Scope: INyxCancellationScope;
    Text: INyxEquality<TNyxText>;
    function Equal(const ALeft, ARight: TKeyedJSON): Boolean;
    function Hash(const AValue: TKeyedJSON): Cardinal;
  end;

  { Pure immutable path/family policy shared by group/distinct implementations. }
  TKeyedOperation = class(TNyxLogicOperation)
  protected
    FPath: TNyxDataPath;
    FKind: TNyxJSONKeyKind;
    FNulls: TNyxJSONNullKeys;
    function Apply(const AItems: TNyxDataValue;
      const AScope: INyxCancellationScope): TNyxDataValue;
    function Transform(const AItems: TKeyedSequence;
      const AScope: INyxCancellationScope): TNyxDataValue; virtual; abstract;
    function Execute(const AInputs: TNyxFlowValues;
      const ACancellation: INyxCancellationScope): TNyxFlowValues; override;
  public
    constructor Create(const APath: TNyxDataPath; AKind: TNyxJSONKeyKind;
      ANulls: TNyxJSONNullKeys; const AName, ATitle, AHelp: TNyxText);
    function GetPath: TNyxDataPath;
    function GetKeyKind: TNyxJSONKeyKind;
    function GetNullKeys: TNyxJSONNullKeys;
  end;

  { Managed grouping delegates lookup/equality/order to Nyx LINQ. }
  TJSONGroup = class(TKeyedOperation, INyxJSONGroup)
  protected
    function Transform(const AItems: TKeyedSequence;
      const AScope: INyxCancellationScope): TNyxDataValue; override;
  public
    function Group(const AItems: TNyxDataValue): TNyxDataValue;
  end;

  { Managed first-row distinct delegates the set index to Nyx LINQ. }
  TJSONDistinct = class(TKeyedOperation, INyxJSONDistinct)
  protected
    function Transform(const AItems: TKeyedSequence;
      const AScope: INyxCancellationScope): TNyxDataValue; override;
  public
    function Distinct(const AItems: TNyxDataValue): TNyxDataValue;
  end;

  { Inner joins always have two partners; no absence sentinel reaches callers. }
  TInnerProjection = class(TInterfacedObject,
    INyxSelector<INyxPairValue<TKeyedJSON, TKeyedJSON>, TNyxDataValue>)
  public
    Scope: INyxCancellationScope;
    function Project(const AValue: INyxPairValue<TKeyedJSON, TKeyedJSON>): TNyxDataValue;
  end;

  { Outer joins explicitly inspect partner presence before reading a payload. }
  TOuterProjection = class(TInterfacedObject,
    INyxSelector<INyxJoinValue<TKeyedJSON, TKeyedJSON>, TNyxDataValue>)
  public
    Scope: INyxCancellationScope;
    function Project(const AValue: INyxJoinValue<TKeyedJSON, TKeyedJSON>): TNyxDataValue;
  end;

  { Immutable join configuration; each call owns its cached rows and LINQ lookup. }
  TJSONJoin = class(TNyxLogicOperation, INyxJSONJoin)
  private
    FLeft: TNyxDataPath;
    FRight: TNyxDataPath;
    FKind: TNyxJSONKeyKind;
    FJoin: TNyxJSONJoinKind;
    FNulls: TNyxJSONNullKeys;
    function Apply(const ALeft, ARight: TNyxDataValue;
      const AScope: INyxCancellationScope): TNyxDataValue;
  protected
    function Execute(const AInputs: TNyxFlowValues;
      const ACancellation: INyxCancellationScope): TNyxFlowValues; override;
  public
    constructor Create(const ALeft, ARight: TNyxDataPath; AKind: TNyxJSONKeyKind;
      AJoin: TNyxJSONJoinKind; ANulls: TNyxJSONNullKeys);
    function Join(const ALeft, ARight: TNyxDataValue): TNyxDataValue;
    function GetLeftPath: TNyxDataPath;
    function GetRightPath: TNyxDataPath;
    function GetKeyKind: TNyxJSONKeyKind;
    function GetJoinKind: TNyxJSONJoinKind;
    function GetNullKeys: TNyxJSONNullKeys;
  end;

  { Numeric selector owns only a path and invocation token. Decimal conversion
    is exact; empty/null/missing/wrong-family values are never coerced to zero. }
  TDecimalSelector = class(TInterfacedObject, INyxSelector<TNyxDataValue, TNyxDecimalNumber>)
  public
    Path: TNyxDataPath;
    Scope: INyxCancellationScope;
    function Project(const AValue: TNyxDataValue): TNyxDecimalNumber;
  end;

  { Immutable aggregate policy; invocation-local queries/accumulators retire on
    success, cancellation or exception. No live collection/store is retained. }
  TJSONAggregate = class(TNyxLogicOperation, INyxJSONAggregate)
  private
    FPath: TNyxDataPath;
    FAggregation: TNyxJSONAggregation;
    FRounding: TNyxDecimalRounding;
    FEmpty: TNyxJSONEmptyAggregate;
    function Apply(const AItems: TNyxDataValue;
      const AScope: INyxCancellationScope): TNyxDataValue;
  protected
    function Execute(const AInputs: TNyxFlowValues;
      const ACancellation: INyxCancellationScope): TNyxFlowValues; override;
  public
    constructor Create(const APath: TNyxDataPath; AAggregation: TNyxJSONAggregation;
      ARounding: TNyxDecimalRounding; AEmpty: TNyxJSONEmptyAggregate);
    function Aggregate(const AItems: TNyxDataValue): TNyxDataValue;
    function GetPath: TNyxDataPath;
    function GetAggregation: TNyxJSONAggregation;
    function GetRounding: TNyxDecimalRounding;
    function GetEmpty: TNyxJSONEmptyAggregate;
  end;

procedure CheckCancellation(const AScope: INyxCancellationScope);
begin

  if (AScope <> nil) and AScope.Cancelled then
  begin
    raise ENyxFlowCancelled.Create('JSON relation invocation cancelled');
  end;
end;

procedure Choice(AValue, AMaximum: Integer; const AName: TNyxText);
begin

  if (AValue < 0) or (AValue > AMaximum) then
  begin
    raise ENyxFlow.Create('Unknown JSON relation ' + AName);
  end;
end;

procedure RequireArray(const AItems: TNyxDataValue);
begin
  AItems.Validate;

  if AItems.Kind <> ndArray then
  begin
    raise ENyxFlow.Create('JSON relation input must be an array');
  end;
end;

function CopyKeyed(const AValue: TKeyedJSON): TKeyedJSON;
begin
  Result.Value := AValue.Value.Copy;
  Result.Key := AValue.Key.Copy;
  Result.Canonical := AValue.Canonical;
end;

{ Internal tagged text is an equality key, never an output or behavioral string.
  Normalized decimals ensure 1/1.0/1e0 hash equally; tags keep explicit null apart
  from every ordinary text value, including empty, NUL and tag-looking strings. }
function KeyText(const AKey: TNyxDataValue; AKind: TNyxJSONKeyKind;
  ANulls: TNyxJSONNullKeys): TNyxText;
var
  LDecimal: TNyxDecimalNumber;
begin

  if AKey.Kind = ndNull then
  begin

    if ANulls = njnReject then
    begin
      raise ENyxFlow.Create('JSON relation key is null; choose explicit null matching');
    end;
    Exit(#0);
  end;
  case AKind of
    njkText: Result := AKey.AsText;
    njkBoolean: Result := AKey.ToJSON;
    njkDecimal:
    begin
      LDecimal := TNyxDecimalNumber.FromDecimal(AKey.AsDecimal);
      Result := LDecimal.ToText;
    end;
  end;

  if ((AKind = njkBoolean) and (AKey.Kind <> ndBoolean)) then
  begin
    raise ENyxFlow.Create('JSON relation key differs from its declared Boolean family');
  end;
  Result := TNyxText(#1) + Result;
end;

function Capture(const AItems: TNyxDataValue; const APath: TNyxDataPath;
  AKind: TNyxJSONKeyKind; ANulls: TNyxJSONNullKeys;
  const AScope: INyxCancellationScope): TKeyedSequence;
var
  LRows: array of TKeyedJSON;
  LIndex: Integer;
  LCount: Integer;
begin
  RequireArray(AItems);
  LCount := AItems.Count;
  SetLength(LRows, LCount);
  for LIndex := 0 to LCount - 1 do
  begin
    CheckCancellation(AScope);
    LRows[LIndex].Value := AItems.Item(LIndex);
    LRows[LIndex].Key := APath.Select(LRows[LIndex].Value);
    LRows[LIndex].Canonical := KeyText(LRows[LIndex].Key, AKind, ANulls);
  end;
  CheckCancellation(AScope);
  Result := TNyxSequence<TKeyedJSON>.From(LRows);
end;

{ Count bytes as results arrive. A Cartesian join cannot allocate an unbounded
  candidate before discovering the final JSON budget. Arithmetic is guarded
  before append; ordinary NyxArray admission still checks the whole result. }
procedure Append(var AValues: TNyxSequenceArray<TNyxDataValue>; var ACount, ABytes: Integer;
  const AValue: TNyxDataValue; const AScope: INyxCancellationScope);
var
  LBytes: Integer;
begin
  CheckCancellation(AScope);
  LBytes := NyxUTF8ByteCount(AValue.ToJSON) + Ord(ACount > 0);

  if LBytes > NyxMaximumJSONBytes - ABytes then
  begin
    raise ENyxFlow.Create('JSON relation output exceeds the shared byte budget');
  end;

  if ACount = Length(AValues) then
  begin
    SetLength(AValues, ACount + 64);
  end;
  AValues[ACount] := AValue.Copy;
  Inc(ACount);
  Inc(ABytes, LBytes);
end;

function Materialize(const ASource: TDataSequence;
  const AScope: INyxCancellationScope): TNyxDataValue;
var
  LValues: TNyxSequenceArray<TNyxDataValue>;
  LCursor: INyxEnumerator<TNyxDataValue>;
  LCount: Integer;
  LBytes: Integer;
begin
  LValues := nil;
  LCount := 0;
  LBytes := 2;
  LCursor := ASource.GetEnumerator;
  while LCursor.MoveNext do
  begin
    Append(LValues, LCount, LBytes, LCursor.Current, AScope);
  end;
  CheckCancellation(AScope);
  SetLength(LValues, LCount);
  Result := NyxArray(LValues);
end;

function TKeySelector.Project(const AValue: TKeyedJSON): TNyxText;
begin
  CheckCancellation(Scope);
  Result := AValue.Canonical;
end;

function TIdentity.Project(const AValue: TKeyedJSON): TKeyedJSON;
begin
  CheckCancellation(Scope);
  Result := CopyKeyed(AValue);
end;

function TValueSelector.Project(const AValue: TKeyedJSON): TNyxDataValue;
begin
  CheckCancellation(Scope);
  Result := AValue.Value.Copy;
end;

function TKeyEquality.Equal(const ALeft, ARight: TKeyedJSON): Boolean;
begin
  CheckCancellation(Scope);
  Result := Text.Equal(ALeft.Canonical, ARight.Canonical);
end;

function TKeyEquality.Hash(const AValue: TKeyedJSON): Cardinal;
begin
  CheckCancellation(Scope);
  Result := Text.Hash(AValue.Canonical);
end;

constructor TKeyedOperation.Create(const APath: TNyxDataPath; AKind: TNyxJSONKeyKind;
  ANulls: TNyxJSONNullKeys; const AName, ATitle, AHelp: TNyxText);
begin
  inherited Create;
  Choice(Ord(AKind), Ord(High(TNyxJSONKeyKind)), 'key family');
  Choice(Ord(ANulls), Ord(High(TNyxJSONNullKeys)), 'null-key policy');
  FPath := TNyxDataPath.FromData(APath.ToData);
  FKind := AKind;
  FNulls := ANulls;
  FConfiguration := NyxObject([NyxField('path', FPath.ToData),
    NyxField('kind', NyxData(Ord(FKind))), NyxField('nulls', NyxData(Ord(FNulls)))]);
  FDescriptor := NyxLogicDescriptor(NyxLogicKind(AName), ATitle, AHelp, nlgCollections)
    .Input(NyxLogicPort(nlpJSON), nftJSON, 'Complete input array')
    .Output(NyxLogicPort(nlpValue), nftJSON, 'Complete transformed array')
    .Labelled('collection').Labelled('key').Labelled('data');
end;

function TKeyedOperation.GetPath: TNyxDataPath;
begin
  Result := FPath.Copy;
end;

function TKeyedOperation.GetKeyKind: TNyxJSONKeyKind;
begin
  Result := FKind;
end;

function TKeyedOperation.GetNullKeys: TNyxJSONNullKeys;
begin
  Result := FNulls;
end;

function TKeyedOperation.Apply(const AItems: TNyxDataValue;
  const AScope: INyxCancellationScope): TNyxDataValue;
begin
  Result := Transform(Capture(AItems, FPath, FKind, FNulls, AScope), AScope);
end;

function TKeyedOperation.Execute(const AInputs: TNyxFlowValues;
  const ACancellation: INyxCancellationScope): TNyxFlowValues;
begin
  Result := NyxFlowValues.WithValue(NyxLogicPort(nlpValue),
    NyxFlowJSON(Apply(AInputs.Value(NyxLogicPort(nlpJSON)).AsJSON, ACancellation)));
end;

function TJSONGroup.Group(const AItems: TNyxDataValue): TNyxDataValue;
begin
  Result := Apply(AItems, nil);
end;

function TJSONGroup.Transform(const AItems: TKeyedSequence;
  const AScope: INyxCancellationScope): TNyxDataValue;
var
  LKey: TKeySelector;
  LKeyPort: INyxSelector<TKeyedJSON, TNyxText>;
  LIdentity: TIdentity;
  LIdentityPort: INyxSelector<TKeyedJSON, TKeyedJSON>;
  LValue: TValueSelector;
  LValuePort: INyxSelector<TKeyedJSON, TNyxDataValue>;
  LLookup: INyxLookup<TNyxText, TKeyedJSON>;
  LGroup: INyxGroup<TNyxText, TKeyedJSON>;
  LRows: TNyxSequenceArray<TNyxDataValue>;
  LIndex: Integer;
  LCount: Integer;
  LBytes: Integer;
  LFirst: TKeyedJSON;
  LRow: TNyxDataValue;
begin
  LKey := TKeySelector.Create;
  LKeyPort := LKey;
  LKey.Scope := AScope;
  LIdentity := TIdentity.Create;
  LIdentityPort := LIdentity;
  LIdentity.Scope := AScope;
  LValue := TValueSelector.Create;
  LValuePort := LValue;
  LValue.Scope := AScope;
  LLookup := TNyxLookup<TKeyedJSON, TNyxText, TKeyedJSON>.From(AItems,
    LKeyPort, LIdentityPort, NewNyxTextEquality);
  LRows := nil;
  LCount := 0;
  LBytes := 2;
  for LIndex := 0 to LLookup.Count - 1 do
  begin
    CheckCancellation(AScope);
    LGroup := LLookup.GroupAt(LIndex);
    LFirst := LGroup.Values.First;
    LRow := NyxObject([NyxField('key', LFirst.Key),
      NyxField('items', Materialize(TNyxSelect<TKeyedJSON, TNyxDataValue>.From(LGroup.Values, LValuePort), AScope)),
      NyxField('count', NyxData(LGroup.Count))]);
    Append(LRows, LCount, LBytes, LRow, AScope);
  end;
  SetLength(LRows, LCount);
  Result := NyxArray(LRows);
end;

function TJSONDistinct.Distinct(const AItems: TNyxDataValue): TNyxDataValue;
begin
  Result := Apply(AItems, nil);
end;

function TJSONDistinct.Transform(const AItems: TKeyedSequence;
  const AScope: INyxCancellationScope): TNyxDataValue;
var
  LEquality: TKeyEquality;
  LEqualityPort: INyxEquality<TKeyedJSON>;
  LValue: TValueSelector;
  LValuePort: INyxSelector<TKeyedJSON, TNyxDataValue>;
begin
  LEquality := TKeyEquality.Create;
  LEqualityPort := LEquality;
  LEquality.Scope := AScope;
  LEquality.Text := NewNyxTextEquality;
  LValue := TValueSelector.Create;
  LValuePort := LValue;
  LValue.Scope := AScope;
  Result := Materialize(TNyxSelect<TKeyedJSON, TNyxDataValue>.From(
    AItems.Distinct(LEqualityPort), LValuePort), AScope);
end;

function JoinRow(const ALeft, ARight: TNyxDataValue; AHasLeft, AHasRight: Boolean): TNyxDataValue;
begin
  Result := NyxObject([NyxField('left', ALeft), NyxField('right', ARight),
    NyxField('hasLeft', NyxData(AHasLeft)), NyxField('hasRight', NyxData(AHasRight))]);
end;

function TInnerProjection.Project(const AValue: INyxPairValue<TKeyedJSON, TKeyedJSON>): TNyxDataValue;
begin
  CheckCancellation(Scope);
  Result := JoinRow(AValue.Outer.Value, AValue.Inner.Value, True, True);
end;

function TOuterProjection.Project(const AValue: INyxJoinValue<TKeyedJSON, TKeyedJSON>): TNyxDataValue;
var
  LLeft: TNyxDataValue;
  LRight: TNyxDataValue;
begin
  CheckCancellation(Scope);
  LLeft := NyxNull;
  LRight := NyxNull;

  if AValue.HasLeft then
  begin
    LLeft := AValue.Left.Value.Copy;
  end;

  if AValue.HasRight then
  begin
    LRight := AValue.Right.Value.Copy;
  end;
  Result := JoinRow(LLeft, LRight, AValue.HasLeft, AValue.HasRight);
end;

constructor TJSONJoin.Create(const ALeft, ARight: TNyxDataPath; AKind: TNyxJSONKeyKind;
  AJoin: TNyxJSONJoinKind; ANulls: TNyxJSONNullKeys);
begin
  inherited Create;
  Choice(Ord(AKind), Ord(High(TNyxJSONKeyKind)), 'key family');
  Choice(Ord(AJoin), Ord(High(TNyxJSONJoinKind)), 'join kind');
  Choice(Ord(ANulls), Ord(High(TNyxJSONNullKeys)), 'null-key policy');
  FLeft := TNyxDataPath.FromData(ALeft.ToData);
  FRight := TNyxDataPath.FromData(ARight.ToData);
  FKind := AKind;
  FJoin := AJoin;
  FNulls := ANulls;
  FConfiguration := NyxObject([NyxField('left', FLeft.ToData), NyxField('right', FRight.ToData),
    NyxField('kind', NyxData(Ord(FKind))), NyxField('join', NyxData(Ord(FJoin))),
    NyxField('nulls', NyxData(Ord(FNulls)))]);
  FDescriptor := NyxLogicDescriptor(NyxLogicKind('json-join'), 'Join JSON arrays',
    'Match two typed key paths; choose inner/left/right/full and explicit null matching.', nlgCollections)
    .Input(NyxLogicPort(nlpLeft), nftJSON, 'Complete left array')
    .Input(NyxLogicPort(nlpRight), nftJSON, 'Complete right array')
    .Output(NyxLogicPort(nlpValue), nftJSON, 'Paired rows with explicit partner presence')
    .Labelled('relate').Labelled('lookup').Labelled('join').Labelled('collection');
end;

function TJSONJoin.GetLeftPath: TNyxDataPath;
begin
  Result := FLeft.Copy;
end;

function TJSONJoin.GetRightPath: TNyxDataPath;
begin
  Result := FRight.Copy;
end;

function TJSONJoin.GetKeyKind: TNyxJSONKeyKind;
begin
  Result := FKind;
end;

function TJSONJoin.GetJoinKind: TNyxJSONJoinKind;
begin
  Result := FJoin;
end;

function TJSONJoin.GetNullKeys: TNyxJSONNullKeys;
begin
  Result := FNulls;
end;

function TJSONJoin.Apply(const ALeft, ARight: TNyxDataValue;
  const AScope: INyxCancellationScope): TNyxDataValue;
var
  LLeft: TKeyedSequence;
  LRight: TKeyedSequence;
  LKey: TKeySelector;
  LKeyPort: INyxSelector<TKeyedJSON, TNyxText>;
  LInner: TInnerProjection;
  LInnerPort: INyxSelector<INyxPairValue<TKeyedJSON, TKeyedJSON>, TNyxDataValue>;
  LOuter: TOuterProjection;
  LOuterPort: INyxSelector<INyxJoinValue<TKeyedJSON, TKeyedJSON>, TNyxDataValue>;
  LResult: TDataSequence;
  LEquality: INyxEquality<TNyxText>;
begin
  LLeft := Capture(ALeft, FLeft, FKind, FNulls, AScope);
  LRight := Capture(ARight, FRight, FKind, FNulls, AScope);
  LKey := TKeySelector.Create;
  LKeyPort := LKey;
  LKey.Scope := AScope;
  LEquality := NewNyxTextEquality;

  if FJoin = njjInner then
  begin
    LInner := TInnerProjection.Create;
    LInnerPort := LInner;
    LInner.Scope := AScope;
    LResult := TNyxJoin<TKeyedJSON, TKeyedJSON, TNyxText, TNyxDataValue>.Inner(
      LLeft, LRight, LKeyPort, LKeyPort, LInnerPort, LEquality);
  end
  else
  begin
    LOuter := TOuterProjection.Create;
    LOuterPort := LOuter;
    LOuter.Scope := AScope;
    case FJoin of
      njjLeft: LResult := TNyxJoin<TKeyedJSON, TKeyedJSON, TNyxText, TNyxDataValue>.Left(
        LLeft, LRight, LKeyPort, LKeyPort, LOuterPort, LEquality);
      njjRight: LResult := TNyxJoin<TKeyedJSON, TKeyedJSON, TNyxText, TNyxDataValue>.Right(
        LLeft, LRight, LKeyPort, LKeyPort, LOuterPort, LEquality);
      njjFull: LResult := TNyxJoin<TKeyedJSON, TKeyedJSON, TNyxText, TNyxDataValue>.Full(
        LLeft, LRight, LKeyPort, LKeyPort, LOuterPort, LEquality);
      else
      begin
        raise ENyxFlow.Create('JSON join configuration changed after admission');
      end;
    end;
  end;
  Result := Materialize(LResult, AScope);
end;

function TJSONJoin.Join(const ALeft, ARight: TNyxDataValue): TNyxDataValue;
begin
  Result := Apply(ALeft, ARight, nil);
end;

function TJSONJoin.Execute(const AInputs: TNyxFlowValues;
  const ACancellation: INyxCancellationScope): TNyxFlowValues;
begin
  Result := NyxFlowValues.WithValue(NyxLogicPort(nlpValue), NyxFlowJSON(Apply(
    AInputs.Value(NyxLogicPort(nlpLeft)).AsJSON, AInputs.Value(NyxLogicPort(nlpRight)).AsJSON,
    ACancellation)));
end;

function TDecimalSelector.Project(const AValue: TNyxDataValue): TNyxDecimalNumber;
begin
  CheckCancellation(Scope);
  Result := TNyxDecimalNumber.FromDecimal(Path.Select(AValue).AsDecimal);
end;

constructor TJSONAggregate.Create(const APath: TNyxDataPath; AAggregation: TNyxJSONAggregation;
  ARounding: TNyxDecimalRounding; AEmpty: TNyxJSONEmptyAggregate);
begin
  inherited Create;
  Choice(Ord(AAggregation), Ord(High(TNyxJSONAggregation)), 'aggregate');
  Choice(Ord(ARounding), Ord(High(TNyxDecimalRounding)), 'rounding');
  Choice(Ord(AEmpty), Ord(High(TNyxJSONEmptyAggregate)), 'empty policy');
  FPath := TNyxDataPath.FromData(APath.ToData);
  FAggregation := AAggregation;
  FRounding := ARounding;
  FEmpty := AEmpty;

  if (FAggregation = njaCount) and (FPath.ToData.Count <> 0) then
  begin
    raise ENyxFlow.Create('JSON Count counts complete rows and requires the root path');
  end;
  FConfiguration := NyxObject([NyxField('path', FPath.ToData),
    NyxField('aggregation', NyxData(Ord(FAggregation))), NyxField('rounding', NyxData(Ord(FRounding))),
    NyxField('empty', NyxData(Ord(FEmpty)))]);
  FDescriptor := NyxLogicDescriptor(NyxLogicKind('json-aggregate'), 'Summarize JSON array',
    'Count rows or calculate exact decimal sum/average/minimum/maximum with explicit empty and rounding policies.',
    nlgCollections).Input(NyxLogicPort(nlpJSON), nftJSON, 'Complete input array')
    .Output(NyxLogicPort(nlpValue), nftJSON, 'Exact number or explicit empty null')
    .Labelled('sum').Labelled('average').Labelled('count').Labelled('aggregate').Labelled('summary');
end;

function TJSONAggregate.GetPath: TNyxDataPath;
begin
  Result := FPath.Copy;
end;

function TJSONAggregate.GetAggregation: TNyxJSONAggregation;
begin
  Result := FAggregation;
end;

function TJSONAggregate.GetRounding: TNyxDecimalRounding;
begin
  Result := FRounding;
end;

function TJSONAggregate.GetEmpty: TNyxJSONEmptyAggregate;
begin
  Result := FEmpty;
end;

function TJSONAggregate.Apply(const AItems: TNyxDataValue;
  const AScope: INyxCancellationScope): TNyxDataValue;
var
  LValues: array of TNyxDataValue;
  LIndex: Integer;
  LCount: Integer;
  LSource: TDataSequence;
  LSelector: TDecimalSelector;
  LSelectPort: INyxSelector<TNyxDataValue, TNyxDecimalNumber>;
  LNumbers: INyxSequence<TNyxDecimalNumber>;
  LNumber: TNyxDecimalNumber;
  LSelected: TNyxDataValue;
begin
  RequireArray(AItems);
  CheckCancellation(AScope);
  LCount := AItems.Count;

  if FAggregation = njaCount then
  begin
    Exit(NyxData(LCount));
  end;

  if LCount = 0 then
  begin

    if FAggregation = njaSum then
    begin
      Exit(NyxData(0));
    end;

    if FEmpty = njaeError then
    begin
      raise ENyxFlow.Create('JSON aggregate requires a nonempty array');
    end;
    Exit(NyxNull);
  end;
  SetLength(LValues, LCount);
  for LIndex := 0 to LCount - 1 do
  begin
    CheckCancellation(AScope);
    LValues[LIndex] := AItems.Item(LIndex);
  end;
  LSource := TNyxSequence<TNyxDataValue>.From(LValues);
  LSelector := TDecimalSelector.Create;
  LSelectPort := LSelector;
  LSelector.Path := FPath.Copy;
  LSelector.Scope := AScope;
  case FAggregation of
    njaSum, njaAverage:
    begin
      LNumbers := TNyxSelect<TNyxDataValue, TNyxDecimalNumber>.From(LSource, LSelectPort);

      if FAggregation = njaSum then
      begin
        LNumber := TNyxDecimalAggregate.Sum(LNumbers, FRounding);
      end
      else
      begin
        LNumber := TNyxDecimalAggregate.Average(LNumbers, FRounding);
      end;
      Result := NyxData(LNumber.ToDecimal);
    end;
    njaMinimum, njaMaximum:
    begin

      if FAggregation = njaMinimum then
      begin
        LSelected := TNyxExtremaBy<TNyxDataValue, TNyxDecimalNumber>.MinBy(
          LSource, LSelectPort, NewNyxDecimalComparer);
      end
      else
      begin
        LSelected := TNyxExtremaBy<TNyxDataValue, TNyxDecimalNumber>.MaxBy(
          LSource, LSelectPort, NewNyxDecimalComparer);
      end;
      Result := FPath.Select(LSelected);
    end;
    else
    begin
      raise ENyxFlow.Create('JSON aggregate configuration changed after admission');
    end;
  end;
  CheckCancellation(AScope);
end;

function TJSONAggregate.Aggregate(const AItems: TNyxDataValue): TNyxDataValue;
begin
  Result := Apply(AItems, nil);
end;

function TJSONAggregate.Execute(const AInputs: TNyxFlowValues;
  const ACancellation: INyxCancellationScope): TNyxFlowValues;
begin
  Result := NyxFlowValues.WithValue(NyxLogicPort(nlpValue),
    NyxFlowJSON(Apply(AInputs.Value(NyxLogicPort(nlpJSON)).AsJSON, ACancellation)));
end;

function NewNyxJSONGroup(const APath: TNyxDataPath; AKind: TNyxJSONKeyKind;
  ANulls: TNyxJSONNullKeys): INyxJSONGroup;
begin
  Result := TJSONGroup.Create(APath, AKind, ANulls, 'json-group', 'Group JSON rows',
    'Collect equal typed keys into ordered key/items/count groups, retaining original rows.');
end;

function NewNyxJSONDistinct(const APath: TNyxDataPath; AKind: TNyxJSONKeyKind;
  ANulls: TNyxJSONNullKeys): INyxJSONDistinct;
begin
  Result := TJSONDistinct.Create(APath, AKind, ANulls, 'json-distinct', 'Distinct JSON rows',
    'Keep the first complete row for each equal typed key, preserving encounter order.');
end;

function NewNyxJSONJoin(const ALeftPath, ARightPath: TNyxDataPath;
  AKind: TNyxJSONKeyKind; AJoin: TNyxJSONJoinKind; ANulls: TNyxJSONNullKeys): INyxJSONJoin;
begin
  Result := TJSONJoin.Create(ALeftPath, ARightPath, AKind, AJoin, ANulls);
end;

function NewNyxJSONAggregate(const APath: TNyxDataPath; AAggregation: TNyxJSONAggregation;
  ARounding: TNyxDecimalRounding; AEmpty: TNyxJSONEmptyAggregate): INyxJSONAggregate;
begin
  Result := TJSONAggregate.Create(APath, AAggregation, ARounding, AEmpty);
end;

function NyxRelationLogicFromData(const AKind: TNyxLogicKindRef;
  const AConfiguration: TNyxDataValue): INyxLogicOperation;
var
  LKind: Integer;
  LNulls: Integer;
  LJoin: Integer;
  LAggregation: Integer;
  LRounding: Integer;
  LEmpty: Integer;
begin
  Result := nil;

  if (AKind.Name = 'json-group') or (AKind.Name = 'json-distinct') then
  begin
    ValidateNyxFlowFields(AConfiguration, ['path', 'kind', 'nulls']);
    LKind := AConfiguration.Field('kind').AsInteger;
    LNulls := AConfiguration.Field('nulls').AsInteger;
    Choice(LKind, Ord(High(TNyxJSONKeyKind)), 'key family');
    Choice(LNulls, Ord(High(TNyxJSONNullKeys)), 'null-key policy');

    if AKind.Name = 'json-group' then
    begin
      Result := NewNyxJSONGroup(TNyxDataPath.FromData(AConfiguration.Field('path')),
        TNyxJSONKeyKind(LKind), TNyxJSONNullKeys(LNulls));
    end
    else
    begin
      Result := NewNyxJSONDistinct(TNyxDataPath.FromData(AConfiguration.Field('path')),
        TNyxJSONKeyKind(LKind), TNyxJSONNullKeys(LNulls));
    end;
  end
  else if AKind.Name = 'json-join' then
  begin
    ValidateNyxFlowFields(AConfiguration, ['left', 'right', 'kind', 'join', 'nulls']);
    LKind := AConfiguration.Field('kind').AsInteger;
    LNulls := AConfiguration.Field('nulls').AsInteger;
    LJoin := AConfiguration.Field('join').AsInteger;
    Choice(LKind, Ord(High(TNyxJSONKeyKind)), 'key family');
    Choice(LNulls, Ord(High(TNyxJSONNullKeys)), 'null-key policy');
    Choice(LJoin, Ord(High(TNyxJSONJoinKind)), 'join kind');
    Result := NewNyxJSONJoin(TNyxDataPath.FromData(AConfiguration.Field('left')),
      TNyxDataPath.FromData(AConfiguration.Field('right')), TNyxJSONKeyKind(LKind),
      TNyxJSONJoinKind(LJoin), TNyxJSONNullKeys(LNulls));
  end
  else if AKind.Name = 'json-aggregate' then
  begin
    ValidateNyxFlowFields(AConfiguration, ['path', 'aggregation', 'rounding', 'empty']);
    LAggregation := AConfiguration.Field('aggregation').AsInteger;
    LRounding := AConfiguration.Field('rounding').AsInteger;
    LEmpty := AConfiguration.Field('empty').AsInteger;
    Choice(LAggregation, Ord(High(TNyxJSONAggregation)), 'aggregate');
    Choice(LRounding, Ord(High(TNyxDecimalRounding)), 'rounding');
    Choice(LEmpty, Ord(High(TNyxJSONEmptyAggregate)), 'empty policy');
    Result := NewNyxJSONAggregate(TNyxDataPath.FromData(AConfiguration.Field('path')),
      TNyxJSONAggregation(LAggregation), TNyxDecimalRounding(LRounding), TNyxJSONEmptyAggregate(LEmpty));
  end;
end;

end.
