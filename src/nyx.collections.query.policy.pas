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

unit nyx.collections.query.policy;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.state, nyx.collections.query;

type
  { Multiple admits an explicitly ordered vector; it does not make a plain
    header click append a key. Single admits one user key at a time. Authored
    baseline ordering remains under the existing query/session owner. }
  TNyxQuerySortMode = (nqsmSingle, nqsmMultiple);
  TNyxQueryComparisons = set of TNyxQueryComparison;

  { Immutable authoring choices for one declared scalar column. Absence preserves
    the library default; NyxColumnQuery creates an explicit enabled choice.
    Operators narrows the field family's portable operations, never coerces a
    field into another family. Empty text is an ordinary exact text value;
    collection schemas currently require a correctly typed value for every
    declared field, so no invented SQL-null comparison is exposed here.
    No view, source, callback, UI control or user-session draft is retained. }
  TNyxColumnQueryPolicy = record
  private
    FDefined: Boolean;
    FFiltering: Boolean;
    FSorting: Boolean;
    FHasOperators: Boolean;
    FOperators: TNyxQueryComparisons;
    FTextMode: TNyxQueryTextComparison;
    function GetComparisonCount: Integer;
    procedure Check;
  public
    function Filtering(AEnabled: Boolean): TNyxColumnQueryPolicy;
    function Sorting(AEnabled: Boolean): TNyxColumnQueryPolicy;
    function Operators(const AValues: array of TNyxQueryComparison): TNyxColumnQueryPolicy;
    function TextComparison(AMode: TNyxQueryTextComparison): TNyxColumnQueryPolicy;
    function Allows(AKind: TNyxStateKind; AComparison: TNyxQueryComparison): Boolean;
    function ComparisonAt(AIndex: Integer): TNyxQueryComparison;
    procedure Validate(AKind: TNyxStateKind);
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxColumnQueryPolicy; static;
    property Defined: Boolean read FDefined;
    property FilterEnabled: Boolean read FFiltering;
    property SortEnabled: Boolean read FSorting;
    property HasOperators: Boolean read FHasOperators;
    property ComparisonCount: Integer read GetComparisonCount;
    property TextMode: TNyxQueryTextComparison read FTextMode;
  end;

  { Table-wide command admission, independent of the authored baseline query.
    Disabling controls never clears an existing query or withdraws another
    surface's contributions. Column restrictions further narrow these flags.
    Runtime preferences are not saved back into this copied policy. }
  TNyxTableQueryPolicy = record
  private
    FDefined: Boolean;
    FEnabled: Boolean;
    FFiltering: Boolean;
    FSorting: Boolean;
    FMode: TNyxQuerySortMode;
  public
    function Enabled(AValue: Boolean): TNyxTableQueryPolicy;
    function Filtering(AValue: Boolean): TNyxTableQueryPolicy;
    function Sorting(AValue: Boolean): TNyxTableQueryPolicy;
    function SortMode(AMode: TNyxQuerySortMode): TNyxTableQueryPolicy;
    procedure Validate;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxTableQueryPolicy; static;
    property Defined: Boolean read FDefined;
    property IsEnabled: Boolean read FEnabled;
    property FilterEnabled: Boolean read FFiltering;
    property SortEnabled: Boolean read FSorting;
    property Mode: TNyxQuerySortMode read FMode;
  end;

function NyxColumnQuery: TNyxColumnQueryPolicy;
function NyxTableQuery: TNyxTableQueryPolicy;
{ The existing query evaluator defines these exact scalar families. This pure
  helper is shared by form admission; it is not another query evaluator. }
function NyxQueryFamilyComparisons(AKind: TNyxStateKind): TNyxQueryComparisons;

implementation

const
  CComparisonNames: array[TNyxQueryComparison] of TNyxText =
    ('equal', 'notEqual', 'less', 'atMost', 'greater', 'atLeast',
     'contains', 'startsWith', 'endsWith');
  CTextNames: array[TNyxQueryTextComparison] of TNyxText = ('exact', 'asciiInsensitive');
  CSortNames: array[TNyxQuerySortMode] of TNyxText = ('single', 'multiple');

function NyxQueryFamilyComparisons(AKind: TNyxStateKind): TNyxQueryComparisons;
begin

  if (Ord(AKind) < Ord(Low(TNyxStateKind))) or
    (Ord(AKind) > Ord(High(TNyxStateKind))) then
  begin
    raise EArgumentException.Create('Unknown column query scalar family');
  end;
  Result := [];
  case AKind of
    nskText: Result := [nqcEqual, nqcNotEqual, nqcContains, nqcStartsWith, nqcEndsWith];
    nskBoolean: Result := [nqcEqual, nqcNotEqual];
    nskInteger, nskNumber: Result := [nqcEqual, nqcNotEqual, nqcLess,
      nqcAtMost, nqcGreater, nqcAtLeast];
  end;
end;

function NyxColumnQuery: TNyxColumnQueryPolicy;
begin
  Result := Default(TNyxColumnQueryPolicy);
  Result.FDefined := True;
  Result.FFiltering := True;
  Result.FSorting := True;
  Result.FTextMode := nqtExact;
end;

procedure TNyxColumnQueryPolicy.Check;
begin

  if not FDefined then
  begin
    raise EArgumentException.Create('Create a column query policy before configuring it');
  end;

  if (Ord(FTextMode) < Ord(Low(TNyxQueryTextComparison))) or
    (Ord(FTextMode) > Ord(High(TNyxQueryTextComparison))) then
  begin
    raise EArgumentException.Create('Unknown column text comparison');
  end;

  if FHasOperators and FFiltering and (FOperators = []) then
  begin
    raise EArgumentException.Create('An enabled column filter requires an allowed comparison');
  end;
end;

function TNyxColumnQueryPolicy.Filtering(AEnabled: Boolean): TNyxColumnQueryPolicy;
begin
  Check;
  Result := Self;
  Result.FFiltering := AEnabled;
  Result.Check;
end;

function TNyxColumnQueryPolicy.Sorting(AEnabled: Boolean): TNyxColumnQueryPolicy;
begin
  Check;
  Result := Self;
  Result.FSorting := AEnabled;
end;

function TNyxColumnQueryPolicy.Operators(
  const AValues: array of TNyxQueryComparison): TNyxColumnQueryPolicy;
var
  LIndex: Integer;
begin
  Check;
  Result := Self;
  Result.FHasOperators := True;
  Result.FOperators := [];

  if Length(AValues) > Ord(High(TNyxQueryComparison)) + 1 then
  begin
    raise EArgumentException.Create('Too many column filter operators');
  end;
  for LIndex := 0 to High(AValues) do
  begin

    if (Ord(AValues[LIndex]) < Ord(Low(TNyxQueryComparison))) or
      (Ord(AValues[LIndex]) > Ord(High(TNyxQueryComparison))) or
      (AValues[LIndex] in Result.FOperators) then
    begin
      raise EArgumentException.Create('Column query operators must be distinct admitted enum values');
    end;
    Include(Result.FOperators, AValues[LIndex]);
  end;
  Result.Check;
end;

function TNyxColumnQueryPolicy.TextComparison(
  AMode: TNyxQueryTextComparison): TNyxColumnQueryPolicy;
begin
  Check;
  Result := Self;
  Result.FTextMode := AMode;
  Result.Check;
end;

procedure TNyxColumnQueryPolicy.Validate(AKind: TNyxStateKind);
var
  LFamily: TNyxQueryComparisons;
begin
  Check;
  LFamily := NyxQueryFamilyComparisons(AKind);

  if FHasOperators and (FOperators - LFamily <> []) then
  begin
    raise EArgumentException.Create('Column filter operator does not apply to its scalar family');
  end;

  if (AKind <> nskText) and (FTextMode <> nqtExact) then
  begin
    raise EArgumentException.Create('Text comparison only applies to text columns');
  end;
end;

function TNyxColumnQueryPolicy.Allows(AKind: TNyxStateKind;
  AComparison: TNyxQueryComparison): Boolean;
begin
  Validate(AKind);
  Result := FFiltering and (AComparison in NyxQueryFamilyComparisons(AKind)) and
    (not FHasOperators or (AComparison in FOperators));
end;

function TNyxColumnQueryPolicy.GetComparisonCount: Integer;
var
  LValue: TNyxQueryComparison;
begin
  Result := 0;
  for LValue := Low(TNyxQueryComparison) to High(TNyxQueryComparison) do
  begin

    if LValue in FOperators then
    begin
      Inc(Result);
    end;
  end;
end;

function TNyxColumnQueryPolicy.ComparisonAt(AIndex: Integer): TNyxQueryComparison;
var
  LValue: TNyxQueryComparison;
begin

  if (AIndex < 0) or (AIndex >= ComparisonCount) then
  begin
    raise EArgumentOutOfRangeException.Create('Column query comparison index is outside its explicit list');
  end;
  for LValue := Low(TNyxQueryComparison) to High(TNyxQueryComparison) do
  begin

    if LValue in FOperators then
    begin

      if AIndex = 0 then
      begin
        Exit(LValue);
      end;
      Dec(AIndex);
    end;
  end;
  Result := nqcEqual;
end;

function TNyxColumnQueryPolicy.ToData: TNyxDataValue;
var
  LOperators: TNyxDataValue;
  LItems: array of TNyxDataValue;
  LIndex: Integer;
begin

  if not FDefined then
  begin
    Exit(NyxNull);
  end;
  Check;
  LOperators := NyxNull;

  if FHasOperators then
  begin
    SetLength(LItems, ComparisonCount);
    for LIndex := 0 to High(LItems) do
    begin
      LItems[LIndex] := NyxData(CComparisonNames[ComparisonAt(LIndex)]);
    end;
    LOperators := NyxArray(LItems);
  end;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('filtering', NyxData(FFiltering)), NyxField('sorting', NyxData(FSorting)),
    NyxField('operators', LOperators), NyxField('textComparison', NyxData(CTextNames[FTextMode]))]);
end;

procedure RequireShape(const AData: TNyxDataValue; const AKeys: TNyxText; ACount: Integer);
var
  LIndex: Integer;
begin
  AData.Validate;

  if (AData.Kind <> ndObject) or (AData.Count <> ACount) or
    (AData.Field('version').AsInteger <> 1) then
  begin
    raise EArgumentException.Create('Query controls require a complete version-one policy');
  end;
  for LIndex := 0 to AData.Count - 1 do
  begin

    if Pos('|' + AData.Key(LIndex) + '|', AKeys) = 0 then
    begin
      raise EArgumentException.Create('Unknown query control policy member');
    end;
  end;
end;

class function TNyxColumnQueryPolicy.FromData(const AData: TNyxDataValue): TNyxColumnQueryPolicy;
var
  LValues: array of TNyxQueryComparison;
  LOperators: TNyxDataValue;
  LIndex: Integer;
  LValue: TNyxQueryComparison;
  LFound: Boolean;
  LText: TNyxText;
begin
  Result := Default(TNyxColumnQueryPolicy);

  if AData.Kind = ndNull then
  begin
    Exit;
  end;
  RequireShape(AData, '|version|filtering|sorting|operators|textComparison|', 5);
  Result := NyxColumnQuery.Filtering(AData.Field('filtering').AsBoolean)
    .Sorting(AData.Field('sorting').AsBoolean);
  LText := AData.Field('textComparison').AsText;

  if LText = CTextNames[nqtExact] then
  begin
    Result := Result.TextComparison(nqtExact);
  end
  else if LText = CTextNames[nqtAsciiInsensitive] then
  begin
    Result := Result.TextComparison(nqtAsciiInsensitive);
  end
  else
  begin
    raise EArgumentException.Create('Unknown saved column text comparison');
  end;
  LOperators := AData.Field('operators');

  if LOperators.Kind = ndNull then
  begin
    Exit;
  end;

  if (LOperators.Kind <> ndArray) or (LOperators.Count > Ord(High(TNyxQueryComparison)) + 1) then
  begin
    raise EArgumentException.Create('Column operators require a bounded typed list');
  end;
  SetLength(LValues, LOperators.Count);
  for LIndex := 0 to High(LValues) do
  begin
    LText := LOperators.Item(LIndex).AsText;
    LFound := False;
    for LValue := Low(TNyxQueryComparison) to High(TNyxQueryComparison) do
    begin

      if LText = CComparisonNames[LValue] then
      begin
        LValues[LIndex] := LValue;
        LFound := True;
        Break;
      end;
    end;

    if not LFound then
    begin
      raise EArgumentException.Create('Unknown saved column filter operator');
    end;
  end;
  Result := Result.Operators(LValues);
end;

function NyxTableQuery: TNyxTableQueryPolicy;
begin
  Result := Default(TNyxTableQueryPolicy);
  Result.FDefined := True;
  Result.FEnabled := True;
  Result.FFiltering := True;
  Result.FSorting := True;
  Result.FMode := nqsmMultiple;
end;

procedure TNyxTableQueryPolicy.Validate;
begin

  if not FDefined or (Ord(FMode) < Ord(Low(TNyxQuerySortMode))) or
    (Ord(FMode) > Ord(High(TNyxQuerySortMode))) then
  begin
    raise EArgumentException.Create('Create an admitted table query policy before configuring it');
  end;
end;

function TNyxTableQueryPolicy.Enabled(AValue: Boolean): TNyxTableQueryPolicy;
begin
  Validate;
  Result := Self;
  Result.FEnabled := AValue;
end;

function TNyxTableQueryPolicy.Filtering(AValue: Boolean): TNyxTableQueryPolicy;
begin
  Validate;
  Result := Self;
  Result.FFiltering := AValue;
end;

function TNyxTableQueryPolicy.Sorting(AValue: Boolean): TNyxTableQueryPolicy;
begin
  Validate;
  Result := Self;
  Result.FSorting := AValue;
end;

function TNyxTableQueryPolicy.SortMode(AMode: TNyxQuerySortMode): TNyxTableQueryPolicy;
begin
  Validate;
  Result := Self;
  Result.FMode := AMode;
  Result.Validate;
end;

function TNyxTableQueryPolicy.ToData: TNyxDataValue;
begin

  if not FDefined then
  begin
    Exit(NyxNull);
  end;
  Validate;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('enabled', NyxData(FEnabled)), NyxField('filtering', NyxData(FFiltering)),
    NyxField('sorting', NyxData(FSorting)), NyxField('sortMode', NyxData(CSortNames[FMode]))]);
end;

class function TNyxTableQueryPolicy.FromData(const AData: TNyxDataValue): TNyxTableQueryPolicy;
var
  LMode: TNyxText;
begin
  Result := Default(TNyxTableQueryPolicy);

  if AData.Kind = ndNull then
  begin
    Exit;
  end;
  RequireShape(AData, '|version|enabled|filtering|sorting|sortMode|', 5);
  Result := NyxTableQuery.Enabled(AData.Field('enabled').AsBoolean)
    .Filtering(AData.Field('filtering').AsBoolean).Sorting(AData.Field('sorting').AsBoolean);
  LMode := AData.Field('sortMode').AsText;

  if LMode = CSortNames[nqsmSingle] then
  begin
    Result := Result.SortMode(nqsmSingle);
  end
  else if LMode = CSortNames[nqsmMultiple] then
  begin
    Result := Result.SortMode(nqsmMultiple);
  end
  else
  begin
    raise EArgumentException.Create('Unknown saved query sort mode');
  end;
end;

end.
