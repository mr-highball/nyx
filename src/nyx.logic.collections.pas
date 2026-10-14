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


unit nyx.logic.collections;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.logic, nyx.flow.types, nyx.scheduler;

type
  { Closed scalar comparisons. Equal/NotEqual support null; ordering requires
    equal scalar families. Text search is ordinal and case-sensitive. Numeric
    comparisons use Nyx's exact 96-bit decimal domain (scale 0..28), refusing
    overflow/precision loss. Original JSON number spellings remain untouched. }
  TNyxJSONComparison = (njcEqual, njcNotEqual, njcLess, njcAtMost, njcGreater,
    njcAtLeast, njcContains, njcStartsWith, njcEndsWith);
  TNyxJSONKeyKind = (njkText, njkBoolean, njkDecimal);
  TNyxJSONDirection = (njdAscending, njdDescending);

  { Immutable managed collection components. Direct calls and graph execution
    use the same LINQ pipeline. Every input must be an array; missing paths,
    heterogeneous keys and invalid scalar domains refuse the complete result.
    They retain copied configuration only, never a document/store/host. }
  INyxJSONFilter = interface(INyxLogicOperation)
    ['{D1411014-6120-4C84-A708-000000000022}']
    function Filter(const AItems: TNyxDataValue): TNyxDataValue;
    function GetPath: TNyxDataPath;
    function GetComparison: TNyxJSONComparison;
    function GetExpected: TNyxDataValue;
    property Path: TNyxDataPath read GetPath;
    property Comparison: TNyxJSONComparison read GetComparison;
    property Expected: TNyxDataValue read GetExpected;
  end;
  INyxJSONSort = interface(INyxLogicOperation)
    ['{D1411014-6120-4C84-A708-000000000023}']
    function Sort(const AItems: TNyxDataValue): TNyxDataValue;
    function GetPath: TNyxDataPath;
    function GetKeyKind: TNyxJSONKeyKind;
    function GetDirection: TNyxJSONDirection;
    property Path: TNyxDataPath read GetPath;
    property KeyKind: TNyxJSONKeyKind read GetKeyKind;
    property Direction: TNyxJSONDirection read GetDirection;
  end;
  INyxJSONMap = interface(INyxLogicOperation)
    ['{D1411014-6120-4C84-A708-000000000024}']
    { Select one typed path from every item. Use the root path for identity;
      structured results remain structured rather than being formatted as text. }
    function Map(const AItems: TNyxDataValue): TNyxDataValue;
    function GetPath: TNyxDataPath;
    property Path: TNyxDataPath read GetPath;
  end;
  INyxJSONSlice = interface(INyxLogicOperation)
    ['{D1411014-6120-4C84-A708-000000000025}']
    function Slice(const AItems: TNyxDataValue): TNyxDataValue;
    function GetOffset: Integer;
    function GetCount: Integer;
    property Offset: Integer read GetOffset;
    property Count: Integer read GetCount;
  end;

function NewNyxJSONFilter(const APath: TNyxDataPath;
  AComparison: TNyxJSONComparison; const AExpected: TNyxDataValue): INyxJSONFilter;
{ Stable ordering retains source order for equal keys, including numerically
  equal values with different decimal spellings. All keys are admitted before
  ordering, even a singleton input whose comparer would otherwise never run. }
function NewNyxJSONSort(const APath: TNyxDataPath; AKind: TNyxJSONKeyKind;
  ADirection: TNyxJSONDirection = njdAscending): INyxJSONSort;
function NewNyxJSONMap(const APath: TNyxDataPath): INyxJSONMap;
{ Offset/count are nonnegative signed 32-bit values. A zero count returns an
  empty array, but still requires a valid array input. No hidden row limit. }
function NewNyxJSONSlice(AOffset, ACount: Integer): INyxJSONSlice;
{ Closed persistence boundary used by ordinary graph recovery. Returns nil for
  another operation kind, and refuses malformed known configuration. }
function NyxCollectionLogicFromData(const AKind: TNyxLogicKindRef;
  const AConfiguration: TNyxDataValue): INyxLogicOperation;

implementation

uses nyx.linq, nyx.numbers.decimal;

type
  TDataSequence = INyxSequence<TNyxDataValue>;
  TDataArray = TNyxSequenceArray<TNyxDataValue>;

  { Per-invocation callbacks own only copied values and a cancellation token.
    No callback retains its operation, query, UI or live collection. }
  TPathSelector = class(TInterfacedObject, INyxSelector<TNyxDataValue, TNyxDataValue>)
  public
    Path: TNyxDataPath;
    Cancellation: INyxCancellationScope;
    function Project(const AValue: TNyxDataValue): TNyxDataValue;
  end;
  TScalarComparer = class(TInterfacedObject, INyxComparer<TNyxDataValue>)
  public
    Kind: TNyxJSONKeyKind;
    Cancellation: INyxCancellationScope;
    function Compare(const ALeft, ARight: TNyxDataValue): Integer;
  end;
  TFilterPredicate = class(TInterfacedObject, INyxPredicate<TNyxDataValue>)
  public
    Path: TNyxDataPath;
    Comparison: TNyxJSONComparison;
    Expected: TNyxDataValue;
    Cancellation: INyxCancellationScope;
    function Test(const AValue: TNyxDataValue): Boolean;
  end;
  TCollectionOperation = class(TNyxLogicOperation)
  protected
    procedure Describe(const AKind, ATitle, AHelp: TNyxText);
    function Transform(const AItems: TNyxDataValue;
      const ACancellation: INyxCancellationScope): TNyxDataValue; virtual; abstract;
    function Execute(const AInputs: TNyxFlowValues;
      const ACancellation: INyxCancellationScope): TNyxFlowValues; override;
  end;
  TJSONFilter = class(TCollectionOperation, INyxJSONFilter)
  private
    FPath: TNyxDataPath;
    FComparison: TNyxJSONComparison;
    FExpected: TNyxDataValue;
  protected
    function Transform(const AItems: TNyxDataValue;
      const ACancellation: INyxCancellationScope): TNyxDataValue; override;
  public
    constructor Create(const APath: TNyxDataPath; AComparison: TNyxJSONComparison;
      const AExpected: TNyxDataValue);
    function Filter(const AItems: TNyxDataValue): TNyxDataValue;
    function GetPath: TNyxDataPath;
    function GetComparison: TNyxJSONComparison;
    function GetExpected: TNyxDataValue;
  end;
  TJSONSort = class(TCollectionOperation, INyxJSONSort)
  private
    FPath: TNyxDataPath;
    FKind: TNyxJSONKeyKind;
    FDirection: TNyxJSONDirection;
  protected
    function Transform(const AItems: TNyxDataValue;
      const ACancellation: INyxCancellationScope): TNyxDataValue; override;
  public
    constructor Create(const APath: TNyxDataPath; AKind: TNyxJSONKeyKind;
      ADirection: TNyxJSONDirection);
    function Sort(const AItems: TNyxDataValue): TNyxDataValue;
    function GetPath: TNyxDataPath;
    function GetKeyKind: TNyxJSONKeyKind;
    function GetDirection: TNyxJSONDirection;
  end;
  TJSONMap = class(TCollectionOperation, INyxJSONMap)
  private
    FPath: TNyxDataPath;
  protected
    function Transform(const AItems: TNyxDataValue;
      const ACancellation: INyxCancellationScope): TNyxDataValue; override;
  public
    constructor Create(const APath: TNyxDataPath);
    function Map(const AItems: TNyxDataValue): TNyxDataValue;
    function GetPath: TNyxDataPath;
  end;
  TJSONSlice = class(TCollectionOperation, INyxJSONSlice)
  private
    FOffset: Integer;
    FCount: Integer;
  protected
    function Transform(const AItems: TNyxDataValue;
      const ACancellation: INyxCancellationScope): TNyxDataValue; override;
  public
    constructor Create(AOffset, ACount: Integer);
    function Slice(const AItems: TNyxDataValue): TNyxDataValue;
    function GetOffset: Integer;
    function GetCount: Integer;
  end;

procedure CheckCancellation(const AScope: INyxCancellationScope);
begin

  if (AScope <> nil) and AScope.Cancelled then
  begin
    raise ENyxFlowCancelled.Create('JSON collection invocation cancelled');
  end;
end;

function ScalarKind(const AValue: TNyxDataValue): TNyxJSONKeyKind;
begin
  case AValue.Kind of
    ndText: Result := njkText;
    ndBoolean: Result := njkBoolean;
    ndNumber: Result := njkDecimal;
    else
    begin
      raise ENyxFlow.Create('JSON collection key must have a declared scalar family');
    end;
  end;
end;

procedure ValidateKey(const AValue: TNyxDataValue; AKind: TNyxJSONKeyKind);
var
  LDecimal: TNyxDecimalNumber;
begin

  if ScalarKind(AValue) <> AKind then
  begin
    raise ENyxFlow.Create('JSON collection key family differs from its declared family');
  end;

  if AKind = njkDecimal then
  begin
    LDecimal := TNyxDecimalNumber.FromDecimal(AValue.AsDecimal);
    { The explicit exact conversion above validates even a singleton key. }
    LDecimal.ToDecimal;
  end;
end;

function CompareScalar(const ALeft, ARight: TNyxDataValue;
  AKind: TNyxJSONKeyKind): Integer;
var
  LText: INyxComparer<TNyxText>;
  LBoolean: INyxComparer<Boolean>;
  LLeft: TNyxDecimalNumber;
  LRight: TNyxDecimalNumber;
begin
  ValidateKey(ALeft, AKind);
  ValidateKey(ARight, AKind);
  case AKind of
    njkText:
    begin
      LText := NewNyxTextComparer;
      Result := LText.Compare(ALeft.AsText, ARight.AsText);
    end;
    njkBoolean:
    begin
      LBoolean := NewNyxBooleanComparer;
      Result := LBoolean.Compare(ALeft.AsBoolean, ARight.AsBoolean);
    end;
    njkDecimal:
    begin
      LLeft := TNyxDecimalNumber.FromDecimal(ALeft.AsDecimal);
      LRight := TNyxDecimalNumber.FromDecimal(ARight.AsDecimal);
      Result := LLeft.CompareTo(LRight);
    end;
  end;
end;

function TPathSelector.Project(const AValue: TNyxDataValue): TNyxDataValue;
begin
  CheckCancellation(Cancellation);
  Result := Path.Select(AValue);
end;

function TScalarComparer.Compare(const ALeft, ARight: TNyxDataValue): Integer;
begin
  CheckCancellation(Cancellation);
  Result := CompareScalar(ALeft, ARight, Kind);
end;

function TFilterPredicate.Test(const AValue: TNyxDataValue): Boolean;
var
  LActual: TNyxDataValue;
  LCompare: Integer;
  LText: TNyxText;
  LExpected: TNyxText;
begin
  CheckCancellation(Cancellation);
  LActual := Path.Select(AValue);

  if Expected.Kind = ndNull then
  begin
    Result := LActual.Kind = ndNull;

    if Comparison = njcNotEqual then
    begin
      Result := not Result;
    end;
    Exit;
  end;
  { Null is a real family. It is not an empty text/zero/False conversion. }

  if LActual.Kind <> Expected.Kind then
  begin
    raise ENyxFlow.Create('JSON filter selected value has a different scalar family');
  end;

  if Comparison in [njcContains, njcStartsWith, njcEndsWith] then
  begin
    LText := LActual.AsText;
    LExpected := Expected.AsText;

    if LExpected = '' then
    begin
      Exit(True);
    end;
    case Comparison of
      njcContains: Result := Pos(LExpected, LText) > 0;
      njcStartsWith: Result := Copy(LText, 1, Length(LExpected)) = LExpected;
      njcEndsWith: Result := Copy(LText, Length(LText) - Length(LExpected) + 1,
        Length(LExpected)) = LExpected;
      else Result := False;
    end;
    Exit;
  end;
  LCompare := CompareScalar(LActual, Expected, ScalarKind(Expected));
  case Comparison of
    njcEqual: Result := LCompare = 0;
    njcNotEqual: Result := LCompare <> 0;
    njcLess: Result := LCompare < 0;
    njcAtMost: Result := LCompare <= 0;
    njcGreater: Result := LCompare > 0;
    njcAtLeast: Result := LCompare >= 0;
    else Result := False;
  end;
end;

function Capture(const AItems: TNyxDataValue;
  const ACancellation: INyxCancellationScope): TDataSequence;
var
  LItems: TDataArray;
  LIndex: Integer;
  LCount: Integer;
begin

  if AItems.Kind <> ndArray then
  begin
    raise ENyxFlow.Create('JSON collection input must be an array');
  end;
  LCount := AItems.Count;
  SetLength(LItems, LCount);
  for LIndex := 0 to LCount - 1 do
  begin
    CheckCancellation(ACancellation);
    LItems[LIndex] := AItems.Item(LIndex);
  end;
  Result := TNyxSequence<TNyxDataValue>.From(LItems);
end;

function Materialize(const ASequence: TDataSequence;
  const ACancellation: INyxCancellationScope): TNyxDataValue;
var
  LValues: TDataArray;
  LCursor: INyxEnumerator<TNyxDataValue>;
  LCount: Integer;
  LCapacity: Integer;
begin
  LCursor := ASequence.GetEnumerator;
  LCount := 0;
  LCapacity := 0;
  LValues := nil;
  while LCursor.MoveNext do
  begin
    CheckCancellation(ACancellation);

    if LCount = LCapacity then
    begin
      LCapacity := LCapacity + 64;
      SetLength(LValues, LCapacity);
    end;
    LValues[LCount] := LCursor.Current.Copy;
    Inc(LCount);
  end;
  CheckCancellation(ACancellation);
  SetLength(LValues, LCount);
  Result := NyxArray(LValues);
end;

procedure TCollectionOperation.Describe(const AKind, ATitle, AHelp: TNyxText);
begin
  FDescriptor := NyxLogicDescriptor(NyxLogicKind(AKind), ATitle, AHelp, nlgCollections)
    .Input(NyxLogicPort(nlpJSON), nftJSON, 'Owned JSON array')
    .Output(NyxLogicPort(nlpValue), nftJSON, 'Complete transformed JSON array')
    .Labelled('collection').Labelled('array').Labelled('query').Labelled('data');
end;

function TCollectionOperation.Execute(const AInputs: TNyxFlowValues;
  const ACancellation: INyxCancellationScope): TNyxFlowValues;
begin
  Result := NyxFlowValues.WithValue(NyxLogicPort(nlpValue), NyxFlowJSON(
    Transform(AInputs.Value(NyxLogicPort(nlpJSON)).AsJSON, ACancellation)));
end;

constructor TJSONFilter.Create(const APath: TNyxDataPath;
  AComparison: TNyxJSONComparison; const AExpected: TNyxDataValue);
begin
  inherited Create;

  if (Ord(AComparison) < Ord(Low(TNyxJSONComparison))) or
    (Ord(AComparison) > Ord(High(TNyxJSONComparison))) then
  begin
    raise ENyxFlow.Create('Unknown JSON filter comparison');
  end;

  if AExpected.Kind = ndNull then
  begin

    if not (AComparison in [njcEqual, njcNotEqual]) then
    begin
      raise ENyxFlow.Create('Null supports JSON filter equality only');
    end;
  end
  else
  begin
    ValidateKey(AExpected, ScalarKind(AExpected));
  end;

  if (AComparison in [njcContains, njcStartsWith, njcEndsWith]) and
    (AExpected.Kind <> ndText) then
  begin
    raise ENyxFlow.Create('JSON text search requires a text expected value');
  end;
  FPath := TNyxDataPath.FromData(APath.ToData);
  FComparison := AComparison;
  FExpected := AExpected.Copy;
  FConfiguration := NyxObject([NyxField('path', FPath.ToData),
    NyxField('comparison', NyxData(Ord(FComparison))), NyxField('expected', FExpected)]);
  Describe('json-filter', 'Filter JSON array',
    'Keep array items matching a typed scalar path comparison; no implicit conversions.');
end;

function TJSONFilter.GetPath: TNyxDataPath;
begin
  Result := TNyxDataPath.FromData(FPath.ToData);
end;

function TJSONFilter.GetComparison: TNyxJSONComparison;
begin
  Result := FComparison;
end;

function TJSONFilter.GetExpected: TNyxDataValue;
begin
  Result := FExpected.Copy;
end;

function TJSONFilter.Filter(const AItems: TNyxDataValue): TNyxDataValue;
begin
  Result := Transform(AItems, nil);
end;

function TJSONFilter.Transform(const AItems: TNyxDataValue;
  const ACancellation: INyxCancellationScope): TNyxDataValue;
var
  LPredicate: TFilterPredicate;
  LPort: INyxPredicate<TNyxDataValue>;
  LSource: TDataSequence;
begin
  LSource := Capture(AItems, ACancellation);
  LPredicate := TFilterPredicate.Create;
  LPort := LPredicate;
  LPredicate.Path := FPath;
  LPredicate.Comparison := FComparison;
  LPredicate.Expected := FExpected.Copy;
  LPredicate.Cancellation := ACancellation;
  Result := Materialize(LSource.Where(LPort), ACancellation);
end;

constructor TJSONSort.Create(const APath: TNyxDataPath; AKind: TNyxJSONKeyKind;
  ADirection: TNyxJSONDirection);
begin
  inherited Create;

  if (Ord(AKind) < Ord(Low(TNyxJSONKeyKind))) or
    (Ord(AKind) > Ord(High(TNyxJSONKeyKind))) or
    (Ord(ADirection) < Ord(Low(TNyxJSONDirection))) or
    (Ord(ADirection) > Ord(High(TNyxJSONDirection))) then
  begin
    raise ENyxFlow.Create('Unknown JSON sort family or direction');
  end;
  FPath := TNyxDataPath.FromData(APath.ToData);
  FKind := AKind;
  FDirection := ADirection;
  FConfiguration := NyxObject([NyxField('path', FPath.ToData),
    NyxField('kind', NyxData(Ord(FKind))), NyxField('direction', NyxData(Ord(FDirection)))]);
  Describe('json-sort', 'Sort JSON array',
    'Stable typed path ordering. Decimal keys use exact 96-bit values; equal keys retain input order.');
end;

function TJSONSort.GetPath: TNyxDataPath;
begin
  Result := TNyxDataPath.FromData(FPath.ToData);
end;

function TJSONSort.GetKeyKind: TNyxJSONKeyKind;
begin
  Result := FKind;
end;

function TJSONSort.GetDirection: TNyxJSONDirection;
begin
  Result := FDirection;
end;

function TJSONSort.Sort(const AItems: TNyxDataValue): TNyxDataValue;
begin
  Result := Transform(AItems, nil);
end;

function TJSONSort.Transform(const AItems: TNyxDataValue;
  const ACancellation: INyxCancellationScope): TNyxDataValue;
var
  LSource: TDataSequence;
  LSelector: TPathSelector;
  LSelectPort: INyxSelector<TNyxDataValue, TNyxDataValue>;
  LComparer: TScalarComparer;
  LComparePort: INyxComparer<TNyxDataValue>;
  LCursor: INyxEnumerator<TNyxDataValue>;
  LDirection: TNyxOrderDirection;
begin
  LSource := Capture(AItems, ACancellation);
  LSelector := TPathSelector.Create;
  LSelectPort := LSelector;
  LSelector.Path := FPath;
  LSelector.Cancellation := ACancellation;
  LComparer := TScalarComparer.Create;
  LComparePort := LComparer;
  LComparer.Kind := FKind;
  LComparer.Cancellation := ACancellation;
  { Preflight every key, including singleton arrays, before sorting. The LINQ
    ordering then owns independently prepared keys and stable tie handling. }
  LCursor := LSource.GetEnumerator;
  while LCursor.MoveNext do
  begin
    ValidateKey(LSelector.Project(LCursor.Current), FKind);
  end;
  LDirection := nodAscending;

  if FDirection = njdDescending then
  begin
    LDirection := nodDescending;
  end;
  Result := Materialize(LSource.OrderBy(TNyxKeyOrder<TNyxDataValue, TNyxDataValue>
    .By(LSelectPort, LComparePort, LDirection)), ACancellation);
end;

constructor TJSONMap.Create(const APath: TNyxDataPath);
begin
  inherited Create;
  FPath := TNyxDataPath.FromData(APath.ToData);
  FConfiguration := NyxObject([NyxField('path', FPath.ToData)]);
  Describe('json-map', 'Map JSON array',
    'Select the same typed Field/Item path from every array item, preserving structured values.');
end;

function TJSONMap.GetPath: TNyxDataPath;
begin
  Result := TNyxDataPath.FromData(FPath.ToData);
end;

function TJSONMap.Map(const AItems: TNyxDataValue): TNyxDataValue;
begin
  Result := Transform(AItems, nil);
end;

function TJSONMap.Transform(const AItems: TNyxDataValue;
  const ACancellation: INyxCancellationScope): TNyxDataValue;
var
  LSelector: TPathSelector;
  LPort: INyxSelector<TNyxDataValue, TNyxDataValue>;
  LSource: TDataSequence;
begin
  LSource := Capture(AItems, ACancellation);
  LSelector := TPathSelector.Create;
  LPort := LSelector;
  LSelector.Path := FPath;
  LSelector.Cancellation := ACancellation;
  Result := Materialize(LSource.Select(LPort), ACancellation);
end;

constructor TJSONSlice.Create(AOffset, ACount: Integer);
begin
  inherited Create;

  if (AOffset < 0) or (ACount < 0) then
  begin
    raise ENyxFlow.Create('JSON pagination offset/count must be nonnegative');
  end;
  FOffset := AOffset;
  FCount := ACount;
  FConfiguration := NyxObject([NyxField('offset', NyxData(FOffset)),
    NyxField('count', NyxData(FCount))]);
  Describe('json-slice', 'Page JSON array',
    'Skip an explicit number of items and take a bounded page, retaining source order.');
end;

function TJSONSlice.GetOffset: Integer;
begin
  Result := FOffset;
end;

function TJSONSlice.GetCount: Integer;
begin
  Result := FCount;
end;

function TJSONSlice.Slice(const AItems: TNyxDataValue): TNyxDataValue;
begin
  Result := Transform(AItems, nil);
end;

function TJSONSlice.Transform(const AItems: TNyxDataValue;
  const ACancellation: INyxCancellationScope): TNyxDataValue;
begin
  Result := Materialize(Capture(AItems, ACancellation).Skip(FOffset).Take(FCount), ACancellation);
end;

function NewNyxJSONFilter(const APath: TNyxDataPath; AComparison: TNyxJSONComparison;
  const AExpected: TNyxDataValue): INyxJSONFilter;
begin
  Result := TJSONFilter.Create(APath, AComparison, AExpected);
end;

function NewNyxJSONSort(const APath: TNyxDataPath; AKind: TNyxJSONKeyKind;
  ADirection: TNyxJSONDirection): INyxJSONSort;
begin
  Result := TJSONSort.Create(APath, AKind, ADirection);
end;

function NewNyxJSONMap(const APath: TNyxDataPath): INyxJSONMap;
begin
  Result := TJSONMap.Create(APath);
end;

function NewNyxJSONSlice(AOffset, ACount: Integer): INyxJSONSlice;
begin
  Result := TJSONSlice.Create(AOffset, ACount);
end;

function NyxCollectionLogicFromData(const AKind: TNyxLogicKindRef;
  const AConfiguration: TNyxDataValue): INyxLogicOperation;
var
  LComparison: Integer;
  LKind: Integer;
  LDirection: Integer;
begin
  Result := nil;

  if AKind.Name = 'json-filter' then
  begin
    ValidateNyxFlowFields(AConfiguration, ['path', 'comparison', 'expected']);
    LComparison := AConfiguration.Field('comparison').AsInteger;

    if (LComparison < Ord(Low(TNyxJSONComparison))) or
      (LComparison > Ord(High(TNyxJSONComparison))) then
    begin
      raise ENyxFlow.Create('Unknown JSON filter comparison');
    end;
    Result := NewNyxJSONFilter(TNyxDataPath.FromData(AConfiguration.Field('path')),
      TNyxJSONComparison(LComparison), AConfiguration.Field('expected'));
  end
  else if AKind.Name = 'json-sort' then
  begin
    ValidateNyxFlowFields(AConfiguration, ['path', 'kind', 'direction']);
    LKind := AConfiguration.Field('kind').AsInteger;
    LDirection := AConfiguration.Field('direction').AsInteger;

    if (LKind < Ord(Low(TNyxJSONKeyKind))) or (LKind > Ord(High(TNyxJSONKeyKind))) or
      (LDirection < Ord(Low(TNyxJSONDirection))) or (LDirection > Ord(High(TNyxJSONDirection))) then
    begin
      raise ENyxFlow.Create('Unknown JSON sort family or direction');
    end;
    Result := NewNyxJSONSort(TNyxDataPath.FromData(AConfiguration.Field('path')),
      TNyxJSONKeyKind(LKind), TNyxJSONDirection(LDirection));
  end
  else if AKind.Name = 'json-map' then
  begin
    ValidateNyxFlowFields(AConfiguration, ['path']);
    Result := NewNyxJSONMap(TNyxDataPath.FromData(AConfiguration.Field('path')));
  end
  else if AKind.Name = 'json-slice' then
  begin
    ValidateNyxFlowFields(AConfiguration, ['offset', 'count']);
    Result := NewNyxJSONSlice(AConfiguration.Field('offset').AsInteger,
      AConfiguration.Field('count').AsInteger);
  end;
end;

end.
