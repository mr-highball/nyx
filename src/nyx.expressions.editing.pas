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
unit nyx.expressions.editing;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.state, nyx.expressions;

type
  { An owned child-index path, independent of node addresses or a live editor.
    Root is empty; only indices 0..2 can occur in the closed expression grammar.
    Actual arity/depth is checked against the exact normalized tree on access. }
  TNyxExpressionPath = record
  private
    FIndices: array of Integer;
    function GetCount: Integer;
    function GetIndex(AIndex: Integer): Integer;
  public
    function Child(AIndex: Integer): TNyxExpressionPath;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxExpressionPath; static;
    property Count: Integer read GetCount;
    property Indices[AIndex: Integer]: Integer read GetIndex;
  end;

{ Empty path selects the root. Both functions admit an independent normalized
  tree; no caller-supplied mutable implementation is retained. }
function NyxExpressionPath(const AIndices: array of Integer): TNyxExpressionPath;
function NyxExpressionAt(const AExpression: INyxExpression;
  const APath: TNyxExpressionPath): INyxExpression;
{ Family-preserving reference rename. Every matching occurrence is replaced,
  including lazy branches; untouched literals/order/comparison remain exact.
  Conflicting target families refuse before returning any modified expression. }
function RenameNyxExpressionState(const AExpression: INyxExpression;
  const AFrom, ATo: TNyxTextStateRef): INyxExpression; overload;
function RenameNyxExpressionState(const AExpression: INyxExpression;
  const AFrom, ATo: TNyxBooleanStateRef): INyxExpression; overload;
function RenameNyxExpressionState(const AExpression: INyxExpression;
  const AFrom, ATo: TNyxIntegerStateRef): INyxExpression; overload;
function RenameNyxExpressionState(const AExpression: INyxExpression;
  const AFrom, ATo: TNyxNumberStateRef): INyxExpression; overload;
{ Recursive strict expression schema under one named definition. Callers merge
  this definition into their root $defs when embedding its $ref. Decoder still
  enforces whole-tree budgets, scalar families and dependency consistency. }
function NyxExpressionSchemaDefinition: TNyxDataValue;

implementation

function TNyxExpressionPath.GetCount: Integer;
begin
  Result := Length(FIndices);
end;

function TNyxExpressionPath.GetIndex(AIndex: Integer): Integer;
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ENyxExpression.Create('Expression path index is outside the owned path');
  end;
  Result := FIndices[AIndex];
end;

function NyxExpressionPath(const AIndices: array of Integer): TNyxExpressionPath;
var
  LIndex: Integer;
begin
  Result := Default(TNyxExpressionPath);

  if Length(AIndices) >= NyxMaximumExpressionDepth then
  begin
    raise ENyxExpression.Create('Expression path exceeds tree depth');
  end;
  SetLength(Result.FIndices, Length(AIndices));
  for LIndex := 0 to High(AIndices) do
  begin

    if (AIndices[LIndex] < 0) or (AIndices[LIndex] > 2) then
    begin
      raise ENyxExpression.Create('Expression child index must be zero, one or two');
    end;
    Result.FIndices[LIndex] := AIndices[LIndex];
  end;
end;

function TNyxExpressionPath.Child(AIndex: Integer): TNyxExpressionPath;
var
  LIndices: array of Integer;
  LIndex: Integer;
begin
  SetLength(LIndices, Count + 1);
  for LIndex := 0 to Count - 1 do
  begin
    LIndices[LIndex] := Indices[LIndex];
  end;
  LIndices[Count] := AIndex;
  Result := NyxExpressionPath(LIndices);
end;

function TNyxExpressionPath.ToData: TNyxDataValue;
var
  LValues: array of TNyxDataValue;
  LIndex: Integer;
begin
  SetLength(LValues, Count);
  for LIndex := 0 to Count - 1 do
  begin
    LValues[LIndex] := NyxData(Indices[LIndex]);
  end;
  Result := NyxArray(LValues);
end;

class function TNyxExpressionPath.FromData(const AData: TNyxDataValue): TNyxExpressionPath;
var
  LIndices: array of Integer;
  LIndex: Integer;
begin

  if (AData.Kind <> ndArray) or (AData.Count >= NyxMaximumExpressionDepth) then
  begin
    raise ENyxExpression.Create('Expression path requires a bounded child-index array');
  end;
  SetLength(LIndices, AData.Count);
  for LIndex := 0 to High(LIndices) do
  begin
    LIndices[LIndex] := AData.Item(LIndex).AsInteger;
  end;
  Result := NyxExpressionPath(LIndices);
end;

function NyxExpressionAt(const AExpression: INyxExpression;
  const APath: TNyxExpressionPath): INyxExpression;
var
  LIndex: Integer;
begin

  if AExpression = nil then
  begin
    raise ENyxExpression.Create('Expression inspection requires its owned tree');
  end;
  Result := NyxExpressionFromData(AExpression.ToData);
  for LIndex := 0 to APath.Count - 1 do
  begin
    Result := Result.Child(APath.Indices[LIndex]);
  end;
end;

function Rename(const AExpression: INyxExpression;
  const AFrom, ATo: TNyxText; AKind: TNyxStateKind): INyxExpression;
var
  LExpression: INyxExpression;
  LDependency: TNyxExpressionDependency;
  LIndex: Integer;

  function Rebuild(const AData: TNyxDataValue): TNyxDataValue;
  var
    LFields: array of TNyxDataField;
    LChildren: array of TNyxDataValue;
    LField: Integer;
    LChild: Integer;
    LValue: TNyxDataValue;
  begin
    SetLength(LFields, AData.Count);
    for LField := 0 to AData.Count - 1 do
    begin
      LValue := AData.Field(AData.Key(LField));

      if (AData.Key(LField) = 'state') and (LValue.AsText = AFrom) then
      begin
        LValue := NyxData(ATo);
      end;

      if AData.Key(LField) = 'args' then
      begin
        SetLength(LChildren, LValue.Count);
        for LChild := 0 to High(LChildren) do
        begin
          LChildren[LChild] := Rebuild(LValue.Item(LChild));
        end;
        LValue := NyxArray(LChildren);
      end;
      LFields[LField] := NyxField(AData.Key(LField), LValue);
    end;
    Result := NyxObject(LFields);
  end;

begin

  if AExpression = nil then
  begin
    raise ENyxExpression.Create('Reference rename requires its typed expression');
  end;
  LExpression := NyxExpressionFromData(AExpression.ToData);
  for LIndex := 0 to LExpression.DependencyCount - 1 do
  begin
    LDependency := LExpression.Dependency(LIndex);

    if (LDependency.Name = AFrom) and (LDependency.Kind <> AKind) then
    begin
      raise ENyxExpression.Create('Expression reference rename must retain its scalar family');
    end;
  end;
  Result := NyxExpressionFromData(Rebuild(LExpression.ToData));
end;

function RenameNyxExpressionState(const AExpression: INyxExpression;
  const AFrom, ATo: TNyxTextStateRef): INyxExpression;
begin
  Result := Rename(AExpression, AFrom.Name, ATo.Name, nskText);
end;

function RenameNyxExpressionState(const AExpression: INyxExpression;
  const AFrom, ATo: TNyxBooleanStateRef): INyxExpression;
begin
  Result := Rename(AExpression, AFrom.Name, ATo.Name, nskBoolean);
end;

function RenameNyxExpressionState(const AExpression: INyxExpression;
  const AFrom, ATo: TNyxIntegerStateRef): INyxExpression;
begin
  Result := Rename(AExpression, AFrom.Name, ATo.Name, nskInteger);
end;

function RenameNyxExpressionState(const AExpression: INyxExpression;
  const AFrom, ATo: TNyxNumberStateRef): INyxExpression;
begin
  Result := Rename(AExpression, AFrom.Name, ATo.Name, nskNumber);
end;

function NyxExpressionSchemaDefinition: TNyxDataValue;
const
  CPrimitive: array[TNyxStateKind] of TNyxText =
    ('string', 'boolean', 'integer', 'number');
var
  LBranches: array of TNyxDataValue;
  LKinds: array of TNyxDataValue;
  LKind: TNyxStateKind;
  LValue: TNyxDataValue;

  function Shape(const AOperation: TNyxDataValue; const AExtra: TNyxDataField;
    const ARequired: TNyxText): TNyxDataValue;
  begin
    Result := NyxObject([
      NyxField('type', NyxData('object')),
      NyxField('properties', NyxObject([
        NyxField('version', NyxObject([NyxField('const', NyxData(1))])),
        NyxField('kind', NyxObject([NyxField('enum', NyxArray(LKinds))])),
        NyxField('op', AOperation), AExtra])),
      NyxField('required', NyxArray([NyxData('version'), NyxData('kind'),
        NyxData('op'), NyxData(ARequired)])),
      NyxField('additionalProperties', NyxData(False))]);
  end;

  function Arguments(ACount: Integer): TNyxDataValue;
  begin
    Result := NyxObject([
      NyxField('type', NyxData('array')),
      NyxField('minItems', NyxData(ACount)),
      NyxField('maxItems', NyxData(ACount)),
      NyxField('items', NyxObject([
        NyxField('$ref', NyxData('#/$defs/NyxExpression'))]))]);
  end;

begin
  SetLength(LBranches, 9);
  SetLength(LKinds, Ord(High(TNyxStateKind)) + 1);
  for LKind := Low(TNyxStateKind) to High(TNyxStateKind) do
  begin
    LKinds[Ord(LKind)] := NyxData(NyxStateKindName(LKind));
    LValue := NyxObject([NyxField('type', NyxData(CPrimitive[LKind]))]);

    if LKind = nskInteger then
    begin
      LValue := NyxObject([
        NyxField('type', NyxData('integer')),
        NyxField('minimum', NyxData(Low(Integer))),
        NyxField('maximum', NyxData(High(Integer)))]);
    end;

    if LKind = nskText then
    begin
      LValue := NyxObject([
        NyxField('type', NyxData('string')),
        NyxField('maxLength', NyxData(NyxMaximumExpressionTextScalars))]);
    end;
    LBranches[Ord(LKind)] := NyxObject([
      NyxField('type', NyxData('object')),
      NyxField('properties', NyxObject([
        NyxField('version', NyxObject([NyxField('const', NyxData(1))])),
        NyxField('kind', NyxObject([NyxField('const', NyxData(NyxStateKindName(LKind)))])),
        NyxField('op', NyxObject([NyxField('const', NyxData('literal'))])),
        NyxField('value', LValue)])),
      NyxField('required', NyxArray([NyxData('version'), NyxData('kind'),
        NyxData('op'), NyxData('value')])),
      NyxField('additionalProperties', NyxData(False))]);
  end;
  LBranches[4] := Shape(NyxObject([NyxField('const', NyxData('state'))]),
    NyxField('state', NyxObject([NyxField('type', NyxData('string')),
      NyxField('minLength', NyxData(1))])), 'state');
  LBranches[5] := Shape(NyxObject([NyxField('enum', NyxArray([
    NyxData('not'), NyxData('negate'), NyxData('length'), NyxData('text'), NyxData('number')]))]),
    NyxField('args', Arguments(1)), 'args');
  LBranches[6] := Shape(NyxObject([NyxField('enum', NyxArray([
    NyxData('and'), NyxData('or'), NyxData('equal'), NyxData('not-equal'),
    NyxData('less'), NyxData('at-most'), NyxData('greater'), NyxData('at-least'),
    NyxData('add'), NyxData('subtract'), NyxData('multiply'), NyxData('divide'),
    NyxData('modulo'), NyxData('concat')]))]), NyxField('args', Arguments(2)), 'args');
  LBranches[7] := Shape(NyxObject([NyxField('const', NyxData('choose'))]),
    NyxField('args', Arguments(3)), 'args');
  LBranches[8] := NyxObject([
    NyxField('type', NyxData('object')),
    NyxField('properties', NyxObject([
      NyxField('version', NyxObject([NyxField('const', NyxData(1))])),
      NyxField('kind', NyxObject([NyxField('const', NyxData('boolean'))])),
      NyxField('op', NyxObject([NyxField('enum', NyxArray([
        NyxData('equal'), NyxData('not-equal'), NyxData('contains'),
        NyxData('starts-with'), NyxData('ends-with')]))])),
      NyxField('args', Arguments(2)),
      NyxField('comparison', NyxObject([NyxField('enum', NyxArray([
        NyxData('exact'), NyxData('ascii-insensitive')]))]))])),
    NyxField('required', NyxArray([NyxData('version'), NyxData('kind'),
      NyxData('op'), NyxData('args'), NyxData('comparison')])),
    NyxField('additionalProperties', NyxData(False))]);
  Result := NyxObject([NyxField('oneOf', NyxArray(LBranches))]);
end;

end.
