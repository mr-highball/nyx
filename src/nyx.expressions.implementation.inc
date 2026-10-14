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

{ Private implementation included by nyx.expressions. All accepted expression
  families share one admission/evaluation engine; specialized wrappers keep
  invalid authoring combinations out of Pascal calls. No renderer is involved. }

type
  IExpressionNode = interface(INyxExpression)
    ['{AF627AE8-40CC-49C0-8681-E5A8F1FC1007}']
    function Compute(const AInputs: INyxExpressionInputs): TNyxStateValue;
  end;
  TExpressionInputs = class(TInterfacedObject, INyxExpressionInputs)
  private
    FIndex: TNyxTextIndex;
    FValues: array of TNyxStateValue;
    function ReadValue(const AName: TNyxText; AKind: TNyxStateKind): TNyxStateValue;
  public
    constructor Create(AState: TNyxState);
    destructor Destroy; override;
    function ReadText(const ARef: TNyxTextStateRef): TNyxText;
    function ReadBoolean(const ARef: TNyxBooleanStateRef): Boolean;
    function ReadInteger(const ARef: TNyxIntegerStateRef): Integer;
    function ReadNumber(const ARef: TNyxNumberStateRef): Double;
    function GetCount: Integer;
  end;
  TExpressionNode = class(TInterfacedObject, INyxExpression, IExpressionNode)
  private
    FKind: TNyxStateKind;
    FOperator: TNyxExpressionOperator;
    FChildren: array of INyxExpression;
    FDependencies: array of TNyxExpressionDependency;
    FScalar: TNyxStateValue;
    FName: TNyxText;
    FComparison: TNyxExpressionTextComparison;
    FData: TNyxDataValue;
    FNodeCount: Integer;
    FDepth: Integer;
    procedure AdmitShape;
    procedure CaptureDependencies;
  public
    constructor Create(const AData: TNyxDataValue; var ACount: Integer; ADepth: Integer);
    function GetKind: TNyxStateKind;
    function GetOperator: TNyxExpressionOperator;
    function GetNodeCount: Integer;
    function GetDepth: Integer;
    function GetChildCount: Integer;
    function Child(AIndex: Integer): INyxExpression;
    function GetDependencyCount: Integer;
    function Dependency(AIndex: Integer): TNyxExpressionDependency;
    procedure ValidateInputs(const AInputs: INyxExpressionInputs);
    function Compute(const AInputs: INyxExpressionInputs): TNyxStateValue;
    function Value(const AInputs: INyxExpressionInputs): TNyxStateValue;
    function ToData: TNyxDataValue;
  end;
  TBooleanExpression = class(TExpressionNode, INyxBooleanExpression)
  public
    function Evaluate(const AInputs: INyxExpressionInputs): Boolean;
    function AndAlso(const AOther: INyxBooleanExpression): INyxBooleanExpression;
    function OrElse(const AOther: INyxBooleanExpression): INyxBooleanExpression;
    function EqualTo(const AOther: INyxBooleanExpression): INyxBooleanExpression;
    function NotEqualTo(const AOther: INyxBooleanExpression): INyxBooleanExpression;
    function Negated: INyxBooleanExpression;
    function AsText: INyxTextExpression;
  end;
  TTextExpression = class(TExpressionNode, INyxTextExpression)
  public
    function Evaluate(const AInputs: INyxExpressionInputs): TNyxText;
    function Append(const AOther: INyxTextExpression): INyxTextExpression;
    function EqualTo(const AOther: INyxTextExpression;
      AComparison: TNyxExpressionTextComparison): INyxBooleanExpression;
    function NotEqualTo(const AOther: INyxTextExpression;
      AComparison: TNyxExpressionTextComparison): INyxBooleanExpression;
    function Contains(const AOther: INyxTextExpression;
      AComparison: TNyxExpressionTextComparison): INyxBooleanExpression;
    function StartsWith(const AOther: INyxTextExpression;
      AComparison: TNyxExpressionTextComparison): INyxBooleanExpression;
    function EndsWith(const AOther: INyxTextExpression;
      AComparison: TNyxExpressionTextComparison): INyxBooleanExpression;
    function Length: INyxIntegerExpression;
  end;
  TIntegerExpression = class(TExpressionNode, INyxIntegerExpression)
  public
    function Evaluate(const AInputs: INyxExpressionInputs): Integer;
    function Plus(const AOther: INyxIntegerExpression): INyxIntegerExpression;
    function Minus(const AOther: INyxIntegerExpression): INyxIntegerExpression;
    function Times(const AOther: INyxIntegerExpression): INyxIntegerExpression;
    function DividedBy(const AOther: INyxIntegerExpression): INyxIntegerExpression;
    function Modulo(const AOther: INyxIntegerExpression): INyxIntegerExpression;
    function EqualTo(const AOther: INyxIntegerExpression): INyxBooleanExpression;
    function NotEqualTo(const AOther: INyxIntegerExpression): INyxBooleanExpression;
    function LessThan(const AOther: INyxIntegerExpression): INyxBooleanExpression;
    function AtMost(const AOther: INyxIntegerExpression): INyxBooleanExpression;
    function GreaterThan(const AOther: INyxIntegerExpression): INyxBooleanExpression;
    function AtLeast(const AOther: INyxIntegerExpression): INyxBooleanExpression;
    function Negated: INyxIntegerExpression;
    function AsText: INyxTextExpression;
    function AsNumber: INyxNumberExpression;
  end;
  TNumberExpression = class(TExpressionNode, INyxNumberExpression)
  public
    function Evaluate(const AInputs: INyxExpressionInputs): Double;
    function Plus(const AOther: INyxNumberExpression): INyxNumberExpression;
    function Minus(const AOther: INyxNumberExpression): INyxNumberExpression;
    function Times(const AOther: INyxNumberExpression): INyxNumberExpression;
    function DividedBy(const AOther: INyxNumberExpression): INyxNumberExpression;
    function EqualTo(const AOther: INyxNumberExpression): INyxBooleanExpression;
    function NotEqualTo(const AOther: INyxNumberExpression): INyxBooleanExpression;
    function LessThan(const AOther: INyxNumberExpression): INyxBooleanExpression;
    function AtMost(const AOther: INyxNumberExpression): INyxBooleanExpression;
    function GreaterThan(const AOther: INyxNumberExpression): INyxBooleanExpression;
    function AtLeast(const AOther: INyxNumberExpression): INyxBooleanExpression;
    function Negated: INyxNumberExpression;
    function AsText: INyxTextExpression;
  end;

const
  COperators: array[TNyxExpressionOperator] of TNyxText =
    ('literal', 'state', 'not', 'and', 'or', 'equal', 'not-equal', 'less',
      'at-most', 'greater', 'at-least', 'add', 'subtract', 'multiply', 'divide',
      'modulo', 'negate', 'concat', 'contains', 'starts-with', 'ends-with',
      'length', 'text', 'number', 'choose');

function NyxExpressionOperatorName(AOperator: TNyxExpressionOperator): TNyxText;
begin

  if (Ord(AOperator) < Ord(Low(TNyxExpressionOperator))) or
    (Ord(AOperator) > Ord(High(TNyxExpressionOperator))) then
  begin
    raise ENyxExpression.Create('Unknown expression operator');
  end;
  Result := COperators[AOperator];
end;

function NyxExpressionTextComparisonName(
  AComparison: TNyxExpressionTextComparison): TNyxText;
const
  CNames: array[TNyxExpressionTextComparison] of TNyxText = ('exact', 'ascii-insensitive');
begin

  if (Ord(AComparison) < Ord(Low(TNyxExpressionTextComparison))) or
    (Ord(AComparison) > Ord(High(TNyxExpressionTextComparison))) then
  begin
    raise ENyxExpression.Create('Unknown expression text comparison');
  end;
  Result := CNames[AComparison];
end;

function ReadKind(const AName: TNyxText): TNyxStateKind;
var
  LKind: TNyxStateKind;
begin
  for LKind := Low(TNyxStateKind) to High(TNyxStateKind) do
  begin

    if AName = NyxStateKindName(LKind) then
    begin
      Exit(LKind);
    end;
  end;
  raise ENyxExpression.Create('Unknown expression scalar kind');
end;

function ReadOperator(const AName: TNyxText): TNyxExpressionOperator;
var
  LOperator: TNyxExpressionOperator;
begin
  for LOperator := Low(TNyxExpressionOperator) to High(TNyxExpressionOperator) do
  begin

    if AName = COperators[LOperator] then
    begin
      Exit(LOperator);
    end;
  end;
  raise ENyxExpression.Create('Unknown expression operator');
end;

function ReadComparison(const AName: TNyxText): TNyxExpressionTextComparison;
begin

  if AName = 'exact' then
  begin
    Exit(netExact);
  end;

  if AName = 'ascii-insensitive' then
  begin
    Exit(netAsciiInsensitive);
  end;
  raise ENyxExpression.Create('Unknown expression text comparison');
end;

function ScalarCount(const AText: TNyxText): Integer;
var
  LOffset: Integer;
  LScalar: Integer;
begin
  Result := 0;
  LOffset := 1;
  while LOffset <= System.Length(AText) do
  begin

    if not NyxNextScalar(AText, LOffset, LScalar) then
    begin
      raise ENyxExpression.Create('Expression text contains malformed Unicode');
    end;
    Inc(Result);

    if Result > NyxMaximumExpressionTextScalars then
    begin
      raise ENyxExpression.Create('Expression text exceeds its scalar budget');
    end;
  end;
end;

procedure CheckWireBudget(const AText: TNyxText);
var
  LOffset: Integer;
  LScalar: Integer;
  LBytes: Integer;
begin
  LOffset := 1;
  LBytes := 0;
  while LOffset <= System.Length(AText) do
  begin

    if not NyxNextScalar(AText, LOffset, LScalar) then
    begin
      raise ENyxExpression.Create('Expression descriptor contains malformed Unicode');
    end;
    Inc(LBytes);

    if LScalar > $7f then
    begin
      Inc(LBytes);
    end;

    if LScalar > $7ff then
    begin
      Inc(LBytes);
    end;

    if LScalar > $ffff then
    begin
      Inc(LBytes);
    end;

    if LBytes > NyxMaximumExpressionBytes then
    begin
      raise ENyxExpression.Create('Expression descriptor exceeds its UTF-8 byte budget');
    end;
  end;
end;

procedure CheckMembers(const AData: TNyxDataValue; const AAllowed: TNyxText);
var
  LIndex: Integer;
begin

  if AData.Kind <> ndObject then
  begin
    raise ENyxExpression.Create('Expression requires an object descriptor');
  end;
  for LIndex := 0 to AData.Count - 1 do
  begin

    if Pos('|' + AData.Key(LIndex) + '|', AAllowed) = 0 then
    begin
      raise ENyxExpression.Create('Unknown expression member: ' + AData.Key(LIndex));
    end;
  end;
end;

function HasMember(const AData: TNyxDataValue; const AName: TNyxText): Boolean;
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

function StateName(const AName: TNyxText; AKind: TNyxStateKind): TNyxText;
begin
  case AKind of
    nskText:
      begin
        Result := NyxTextState(AName).Name;
      end;
    nskBoolean:
      begin
        Result := NyxBooleanState(AName).Name;
      end;
    nskInteger:
      begin
        Result := NyxIntegerState(AName).Name;
      end;
    nskNumber:
      begin
        Result := NyxNumberState(AName).Name;
      end;
  end;
end;

function ReadScalar(const AData: TNyxDataValue; AKind: TNyxStateKind): TNyxStateValue;
begin
  case AKind of
    nskText:
      begin
        ScalarCount(AData.AsText);
        Result := TNyxStateValue.FromText(AData.AsText);
      end;
    nskBoolean:
      begin
        Result := TNyxStateValue.FromBoolean(AData.AsBoolean);
      end;
    nskInteger:
      begin
        Result := TNyxStateValue.FromInteger(AData.AsInteger);
      end;
    nskNumber:
      begin
        Result := TNyxStateValue.FromNumber(AData.AsNumber);
      end;
  end;
end;

function ScalarData(const AValue: TNyxStateValue): TNyxDataValue;
begin
  case AValue.Kind of
    nskText:
      begin
        Result := NyxData(AValue.TextValue);
      end;
    nskBoolean:
      begin
        Result := NyxData(AValue.BooleanValue);
      end;
    nskInteger:
      begin
        Result := NyxData(AValue.IntegerValue);
      end;
    nskNumber:
      begin
        Result := NyxData(AValue.NumberValue);
      end;
  end;
end;

function InputValue(const AInputs: INyxExpressionInputs;
  const AName: TNyxText; AKind: TNyxStateKind): TNyxStateValue;
begin

  if AInputs = nil then
  begin
    raise ENyxExpression.Create('Expression state reference requires an input snapshot');
  end;
  case AKind of
    nskText:
      begin
        Result := TNyxStateValue.FromText(AInputs.ReadText(NyxTextState(AName)));
      end;
    nskBoolean:
      begin
        Result := TNyxStateValue.FromBoolean(AInputs.ReadBoolean(NyxBooleanState(AName)));
      end;
    nskInteger:
      begin
        Result := TNyxStateValue.FromInteger(AInputs.ReadInteger(NyxIntegerState(AName)));
      end;
    nskNumber:
      begin
        Result := TNyxStateValue.FromNumber(AInputs.ReadNumber(NyxNumberState(AName)));
      end;
  end;
end;

function Build(const AData: TNyxDataValue; var ACount: Integer;
  ADepth: Integer): INyxExpression; forward;

constructor TExpressionNode.Create(const AData: TNyxDataValue;
  var ACount: Integer; ADepth: Integer);
var
  LArgs: TNyxDataValue;
  LIndex: Integer;
  LValues: array of TNyxDataValue;
  LFields: array of TNyxDataField;
begin
  inherited Create;

  if ADepth > NyxMaximumExpressionDepth then
  begin
    raise ENyxExpression.Create('Expression exceeds its nesting budget');
  end;
  Inc(ACount);

  if ACount > NyxMaximumExpressionNodes then
  begin
    raise ENyxExpression.Create('Expression exceeds its node budget');
  end;

  if AData.Field('version').AsInteger <> 1 then
  begin
    raise ENyxExpression.Create('Unknown expression descriptor version');
  end;
  FKind := ReadKind(AData.Field('kind').AsText);
  FOperator := ReadOperator(AData.Field('op').AsText);
  FNodeCount := 1;
  FDepth := 1;
  FComparison := netExact;
  case FOperator of
    neoLiteral:
      begin
        CheckMembers(AData, '|version|kind|op|value|');
        FScalar := ReadScalar(AData.Field('value'), FKind);
      end;
    neoState:
      begin
        CheckMembers(AData, '|version|kind|op|state|');
        FName := StateName(AData.Field('state').AsText, FKind);
      end;
  else
    begin
      CheckMembers(AData, '|version|kind|op|args|comparison|');
      LArgs := AData.Field('args');

      if (LArgs.Kind <> ndArray) or (LArgs.Count > 3) then
      begin
        raise ENyxExpression.Create('Expression operands require a bounded array');
      end;
      SetLength(FChildren, LArgs.Count);
      for LIndex := 0 to LArgs.Count - 1 do
      begin
        FChildren[LIndex] := Build(LArgs.Item(LIndex), ACount, ADepth + 1);
        Inc(FNodeCount, FChildren[LIndex].NodeCount);

        if FChildren[LIndex].Depth + 1 > FDepth then
        begin
          FDepth := FChildren[LIndex].Depth + 1;
        end;
      end;

      if HasMember(AData, 'comparison') then
      begin

        if not (FOperator in [neoEqual, neoNotEqual, neoContains, neoStartsWith, neoEndsWith]) or
          (System.Length(FChildren) <> 2) or (FChildren[0].Kind <> nskText) then
        begin
          raise ENyxExpression.Create('Text comparison belongs only to textual predicates');
        end;
        FComparison := ReadComparison(AData.Field('comparison').AsText);
      end;
    end;
  end;
  AdmitShape;
  CaptureDependencies;
  SetLength(LFields, 4);
  LFields[0] := NyxField('version', NyxData(1));
  LFields[1] := NyxField('kind', NyxData(NyxStateKindName(FKind)));
  LFields[2] := NyxField('op', NyxData(COperators[FOperator]));
  case FOperator of
    neoLiteral:
      begin
        LFields[3] := NyxField('value', ScalarData(FScalar));
      end;
    neoState:
      begin
        LFields[3] := NyxField('state', NyxData(FName));
      end;
  else
    begin
      SetLength(LValues, System.Length(FChildren));
      for LIndex := 0 to High(FChildren) do
      begin
        LValues[LIndex] := FChildren[LIndex].ToData;
      end;
      LFields[3] := NyxField('args', NyxArray(LValues));

      if (FOperator in [neoEqual, neoNotEqual, neoContains, neoStartsWith, neoEndsWith]) and
        (FChildren[0].Kind = nskText) then
      begin
        SetLength(LFields, 5);
        LFields[4] := NyxField('comparison', NyxData(NyxExpressionTextComparisonName(FComparison)));
      end;
    end;
  end;
  FData := NyxObject(LFields);
end;

procedure TExpressionNode.AdmitShape;

  procedure Arity(AExpected: Integer);
  begin

    if System.Length(FChildren) <> AExpected then
    begin
      raise ENyxExpression.Create('Wrong operand count for ' + COperators[FOperator]);
    end;
  end;

  procedure Kind(AActual, AExpected: TNyxStateKind);
  begin

    if AActual <> AExpected then
    begin
      raise ENyxExpression.Create('Wrong scalar family for ' + COperators[FOperator]);
    end;
  end;

  procedure SameOperands;
  begin
    Arity(2);
    Kind(FChildren[0].Kind, FChildren[1].Kind);
  end;

begin
  case FOperator of
    neoLiteral, neoState:
      begin
        Arity(0);
      end;
    neoNot:
      begin
        Arity(1);
        Kind(FKind, nskBoolean);
        Kind(FChildren[0].Kind, nskBoolean);
      end;
    neoAnd, neoOr:
      begin
        SameOperands;
        Kind(FKind, nskBoolean);
        Kind(FChildren[0].Kind, nskBoolean);
      end;
    neoEqual, neoNotEqual:
      begin
        SameOperands;
        Kind(FKind, nskBoolean);
      end;
    neoLess, neoAtMost, neoGreater, neoAtLeast:
      begin
        SameOperands;
        Kind(FKind, nskBoolean);

        if not (FChildren[0].Kind in [nskInteger, nskNumber]) then
        begin
          raise ENyxExpression.Create('Ordered comparison requires a numeric family');
        end;
      end;
    neoAdd, neoSubtract, neoMultiply, neoDivide, neoModulo:
      begin
        SameOperands;
        Kind(FChildren[0].Kind, FKind);

        if not (FKind in [nskInteger, nskNumber]) or
          ((FOperator = neoModulo) and (FKind <> nskInteger)) then
        begin
          raise ENyxExpression.Create('Arithmetic requires the declared numeric family');
        end;
      end;
    neoNegate:
      begin
        Arity(1);
        Kind(FChildren[0].Kind, FKind);

        if not (FKind in [nskInteger, nskNumber]) then
        begin
          raise ENyxExpression.Create('Negation requires a numeric family');
        end;
      end;
    neoConcat:
      begin
        SameOperands;
        Kind(FKind, nskText);
        Kind(FChildren[0].Kind, nskText);
      end;
    neoContains, neoStartsWith, neoEndsWith:
      begin
        SameOperands;
        Kind(FKind, nskBoolean);
        Kind(FChildren[0].Kind, nskText);
      end;
    neoLength:
      begin
        Arity(1);
        Kind(FKind, nskInteger);
        Kind(FChildren[0].Kind, nskText);
      end;
    neoText:
      begin
        Arity(1);
        Kind(FKind, nskText);

        if FChildren[0].Kind = nskText then
        begin
          raise ENyxExpression.Create('Text projection requires an explicit non-text scalar');
        end;
      end;
    neoNumber:
      begin
        Arity(1);
        Kind(FKind, nskNumber);
        Kind(FChildren[0].Kind, nskInteger);
      end;
    neoChoose:
      begin
        Arity(3);
        Kind(FChildren[0].Kind, nskBoolean);
        Kind(FChildren[1].Kind, FKind);
        Kind(FChildren[2].Kind, FKind);
      end;
  end;
end;

procedure TExpressionNode.CaptureDependencies;
var
  LSeen: TNyxTextIndex;
  LChild: Integer;
  LIndex: Integer;
  LDependency: TNyxExpressionDependency;

  procedure Add(const AValue: TNyxExpressionDependency);
  var
    LExisting: Integer;
    LCount: Integer;
  begin
    LExisting := LSeen.IndexOf(AValue.Name);

    if LExisting >= 0 then
    begin

      if FDependencies[LExisting].Kind <> AValue.Kind then
      begin
        raise ENyxExpression.Create('Expression uses conflicting types for state: ' + AValue.Name);
      end;
      Exit;
    end;
    LCount := System.Length(FDependencies);
    SetLength(FDependencies, LCount + 1);
    FDependencies[LCount].Name := AValue.Name;
    FDependencies[LCount].Kind := AValue.Kind;
    LSeen.AddFirst(AValue.Name, LCount);
  end;

begin
  LSeen := TNyxTextIndex.Create;
  try

    if FOperator = neoState then
    begin
      LDependency.Name := FName;
      LDependency.Kind := FKind;
      Add(LDependency);
    end;
    for LChild := 0 to High(FChildren) do
    begin
      for LIndex := 0 to FChildren[LChild].DependencyCount - 1 do
      begin
        Add(FChildren[LChild].Dependency(LIndex));
      end;
    end;
  finally
    LSeen.Free;
  end;
end;

function Build(const AData: TNyxDataValue; var ACount: Integer;
  ADepth: Integer): INyxExpression;
begin
  case ReadKind(AData.Field('kind').AsText) of
    nskBoolean:
      begin
        Result := TBooleanExpression.Create(AData, ACount, ADepth);
      end;
    nskText:
      begin
        Result := TTextExpression.Create(AData, ACount, ADepth);
      end;
    nskInteger:
      begin
        Result := TIntegerExpression.Create(AData, ACount, ADepth);
      end;
    nskNumber:
      begin
        Result := TNumberExpression.Create(AData, ACount, ADepth);
      end;
  end;
end;

function NyxExpressionFromData(const AData: TNyxDataValue): INyxExpression;
var
  LCount: Integer;
begin
  {$ifdef NYX_EXPRESSION_PROFILE}Inc(GExpressionProfile.Admissions);{$endif}
  LCount := 0;
  try
    CheckWireBudget(AData.ToJSON);
    Result := Build(AData, LCount, 1);
  except
    on LException: Exception do
    begin
      raise ENyxExpression.Create('Expression admission: ' + TNyxText(LException.Message));
    end;
  end;
end;

function TExpressionNode.GetKind: TNyxStateKind;
begin
  Result := FKind;
end;

function TExpressionNode.GetOperator: TNyxExpressionOperator;
begin
  Result := FOperator;
end;

function TExpressionNode.GetNodeCount: Integer;
begin
  Result := FNodeCount;
end;

function TExpressionNode.GetDepth: Integer;
begin
  Result := FDepth;
end;

function TExpressionNode.GetChildCount: Integer;
begin
  Result := System.Length(FChildren);
end;

function TExpressionNode.Child(AIndex: Integer): INyxExpression;
begin

  if (AIndex < 0) or (AIndex >= System.Length(FChildren)) then
  begin
    raise ENyxExpression.Create('Expression child index is out of range');
  end;
  Result := FChildren[AIndex];
end;

function TExpressionNode.GetDependencyCount: Integer;
begin
  Result := System.Length(FDependencies);
end;

function TExpressionNode.Dependency(AIndex: Integer): TNyxExpressionDependency;
begin

  if (AIndex < 0) or (AIndex >= System.Length(FDependencies)) then
  begin
    raise ENyxExpression.Create('Expression dependency index is out of range');
  end;
  Result.Name := FDependencies[AIndex].Name;
  Result.Kind := FDependencies[AIndex].Kind;
end;

function TExpressionNode.ToData: TNyxDataValue;
begin
  Result := FData.Copy;
end;

procedure TExpressionNode.ValidateInputs(const AInputs: INyxExpressionInputs);
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FDependencies) do
  begin
    InputValue(AInputs, FDependencies[LIndex].Name, FDependencies[LIndex].Kind);
  end;
end;

function TExpressionNode.Value(const AInputs: INyxExpressionInputs): TNyxStateValue;
begin
  {$ifdef NYX_EXPRESSION_PROFILE}Inc(GExpressionProfile.Evaluations);{$endif}
  try
    ValidateInputs(AInputs);
    Result := Compute(AInputs);

    if Result.Kind = nskText then
    begin
      ScalarCount(Result.TextValue);
    end;
  except
    on LException: Exception do
    begin
      raise ENyxExpression.Create('Expression evaluation: ' + TNyxText(LException.Message));
    end;
  end;
end;

constructor TExpressionInputs.Create(AState: TNyxState);
var
  LIndex: Integer;
begin
  inherited Create;
  {$ifdef NYX_EXPRESSION_PROFILE}Inc(GExpressionProfile.InputFrames);{$endif}

  if AState = nil then
  begin
    raise ENyxExpression.Create('Expression inputs require an owning state store');
  end;
  FIndex := TNyxTextIndex.Create;
  SetLength(FValues, AState.Count);
  for LIndex := 0 to AState.Count - 1 do
  begin
    FIndex.AddFirst(AState.Key(LIndex), LIndex);
    FValues[LIndex] := AState.Value(AState.Key(LIndex)).Copy;
  end;
end;

destructor TExpressionInputs.Destroy;
begin
  FIndex.Free;
  inherited Destroy;
end;

function TExpressionInputs.ReadValue(const AName: TNyxText;
  AKind: TNyxStateKind): TNyxStateValue;
var
  LIndex: Integer;
begin
  LIndex := FIndex.IndexOf(AName);

  if LIndex < 0 then
  begin
    raise ENyxExpression.Create('Expression state is missing: ' + AName);
  end;

  if FValues[LIndex].Kind <> AKind then
  begin
    raise ENyxExpression.Create('Expression state has the wrong kind: ' + AName);
  end;
  Result := FValues[LIndex].Copy;
end;

function TExpressionInputs.ReadText(const ARef: TNyxTextStateRef): TNyxText;
begin
  Result := ReadValue(ARef.Name, nskText).TextValue;
end;

function TExpressionInputs.ReadBoolean(const ARef: TNyxBooleanStateRef): Boolean;
begin
  Result := ReadValue(ARef.Name, nskBoolean).BooleanValue;
end;

function TExpressionInputs.ReadInteger(const ARef: TNyxIntegerStateRef): Integer;
begin
  Result := ReadValue(ARef.Name, nskInteger).IntegerValue;
end;

function TExpressionInputs.ReadNumber(const ARef: TNyxNumberStateRef): Double;
begin
  Result := ReadValue(ARef.Name, nskNumber).NumberValue;
end;

function TExpressionInputs.GetCount: Integer;
begin
  Result := System.Length(FValues);
end;

function CaptureNyxExpressionInputs(AState: TNyxState): INyxExpressionInputs;
begin
  Result := TExpressionInputs.Create(AState);
end;

function CheckedInteger(AValue: Double): TNyxStateValue;
begin

  if IsNan(AValue) or IsInfinite(AValue) or
    (AValue < -2147483648.0) or (AValue > 2147483647.0) then
  begin
    raise ENyxExpression.Create('Expression integer arithmetic overflow');
  end;
  Result := TNyxStateValue.FromInteger(Integer(Trunc(AValue)));
end;

function Numeric(const AValue: TNyxStateValue): Double;
begin
  if AValue.Kind = nskInteger then
  begin
    Exit(AValue.IntegerValue);
  end;
  Result := AValue.NumberValue;
end;

function FoldASCII(const AText: TNyxText): TNyxText;
var
  LIndex: Integer;
begin
  Result := AText;
  for LIndex := 1 to System.Length(Result) do
  begin

    if (Result[LIndex] >= 'A') and (Result[LIndex] <= 'Z') then
    begin
      Result[LIndex] := Chr(Ord(Result[LIndex]) + 32);
    end;
  end;
end;

function TExpressionNode.Compute(const AInputs: INyxExpressionInputs): TNyxStateValue;
var
  LLeft: TNyxStateValue;
  LRight: TNyxStateValue;
  LNumber: Double;
  LOther: Double;
  LResult: Double;
  LText: TNyxText;
  LSearch: TNyxText;
  LBoolean: Boolean;
  LOrder: Integer;

  function Operand(AIndex: Integer): TNyxStateValue;
  begin
    Result := (FChildren[AIndex] as IExpressionNode).Compute(AInputs);
  end;

begin

  if FOperator = neoLiteral then
  begin
    Exit(FScalar.Copy);
  end;

  if FOperator = neoState then
  begin
    Exit(InputValue(AInputs, FName, FKind));
  end;
  LLeft := Operand(0);
  case FOperator of
    neoNot:
      begin
        Exit(TNyxStateValue.FromBoolean(not LLeft.BooleanValue));
      end;
    neoAnd:
      begin

        if not LLeft.BooleanValue then
        begin
          Exit(TNyxStateValue.FromBoolean(False));
        end;
        Exit(Operand(1));
      end;
    neoOr:
      begin

        if LLeft.BooleanValue then
        begin
          Exit(TNyxStateValue.FromBoolean(True));
        end;
        Exit(Operand(1));
      end;
    neoChoose:
      begin

        if LLeft.BooleanValue then
        begin
          Exit(Operand(1));
        end;
        Exit(Operand(2));
      end;
    neoLength:
      begin
        Exit(TNyxStateValue.FromInteger(ScalarCount(LLeft.TextValue)));
      end;
    neoText:
      begin
        case LLeft.Kind of
          nskBoolean:
            begin
              LText := 'false';

              if LLeft.BooleanValue then
              begin
                LText := 'true';
              end;
            end;
          nskInteger:
            begin
              LText := TNyxText(IntToStr(LLeft.IntegerValue));
            end;
          nskNumber:
            begin
              LText := LLeft.NumberText;
            end;
        else
          raise ENyxExpression.Create('Unsupported expression text projection');
        end;
        Exit(TNyxStateValue.FromText(LText));
      end;
    neoNumber:
      begin
        Exit(TNyxStateValue.FromNumber(LLeft.IntegerValue));
      end;
    neoNegate:
      begin
        LResult := -Numeric(LLeft);

        if FKind = nskInteger then
        begin
          Exit(CheckedInteger(LResult));
        end;
        Exit(TNyxStateValue.FromNumber(LResult));
      end;
  else
    begin
      { The remaining admitted operators consume both scalar operands below. }
    end;
  end;
  LRight := Operand(1);

  if (FOperator in [neoConcat, neoContains, neoStartsWith, neoEndsWith]) or
    ((FOperator in [neoEqual, neoNotEqual]) and (LLeft.Kind = nskText)) then
  begin
    LText := LLeft.TextValue;
    LSearch := LRight.TextValue;

    if FOperator = neoConcat then
    begin

      if ScalarCount(LText) > NyxMaximumExpressionTextScalars - ScalarCount(LSearch) then
      begin
        raise ENyxExpression.Create('Expression concatenation exceeds its scalar budget');
      end;
      Exit(TNyxStateValue.FromText(LText + LSearch));
    end;

    if FComparison = netAsciiInsensitive then
    begin
      LText := FoldASCII(LText);
      LSearch := FoldASCII(LSearch);
    end;
    case FOperator of
      neoEqual:
        begin
          LBoolean := LText = LSearch;
        end;
      neoNotEqual:
        begin
          LBoolean := LText <> LSearch;
        end;
      neoContains:
        begin
          LBoolean := (LSearch = '') or (Pos(LSearch, LText) > 0);
        end;
      neoStartsWith:
        begin
          LBoolean := Copy(LText, 1, System.Length(LSearch)) = LSearch;
        end;
      neoEndsWith:
        begin
          LBoolean := (System.Length(LSearch) <= System.Length(LText)) and
            (Copy(LText, System.Length(LText) - System.Length(LSearch) + 1,
              System.Length(LSearch)) = LSearch);
        end;
    else
      raise ENyxExpression.Create('Unsupported expression text operator');
    end;
    Exit(TNyxStateValue.FromBoolean(LBoolean));
  end;

  if (FOperator in [neoEqual, neoNotEqual]) and (LLeft.Kind = nskBoolean) then
  begin
    LBoolean := LLeft.BooleanValue = LRight.BooleanValue;

    if FOperator = neoNotEqual then
    begin
      LBoolean := not LBoolean;
    end;
    Exit(TNyxStateValue.FromBoolean(LBoolean));
  end;
  LNumber := Numeric(LLeft);
  LOther := Numeric(LRight);

  if FOperator in [neoEqual, neoNotEqual, neoLess, neoAtMost, neoGreater, neoAtLeast] then
  begin
    LOrder := 0;

    if LNumber < LOther then
    begin
      LOrder := -1;
    end;

    if LNumber > LOther then
    begin
      LOrder := 1;
    end;
    case FOperator of
      neoEqual:
        begin
          LBoolean := LOrder = 0;
        end;
      neoNotEqual:
        begin
          LBoolean := LOrder <> 0;
        end;
      neoLess:
        begin
          LBoolean := LOrder < 0;
        end;
      neoAtMost:
        begin
          LBoolean := LOrder <= 0;
        end;
      neoGreater:
        begin
          LBoolean := LOrder > 0;
        end;
      neoAtLeast:
        begin
          LBoolean := LOrder >= 0;
        end;
    else
      raise ENyxExpression.Create('Unsupported numeric comparison');
    end;
    Exit(TNyxStateValue.FromBoolean(LBoolean));
  end;
  case FOperator of
    neoAdd:
      begin
        LResult := LNumber + LOther;
      end;
    neoSubtract:
      begin
        LResult := LNumber - LOther;
      end;
    neoMultiply:
      begin
        LResult := LNumber * LOther;
      end;
    neoDivide, neoModulo:
      begin

        if LOther = 0 then
        begin
          raise ENyxExpression.Create('Expression division by zero');
        end;
        LResult := LNumber / LOther;

        if FKind = nskInteger then
        begin
          LResult := Trunc(LResult);
        end;

        if FOperator = neoModulo then
        begin
          LResult := LNumber - LResult * LOther;
        end;
      end;
  else
    raise ENyxExpression.Create('Unsupported expression arithmetic operator');
  end;

  if FKind = nskInteger then
  begin
    Exit(CheckedInteger(LResult));
  end;
  Result := TNyxStateValue.FromNumber(LResult);
end;

function Compose(AOperator: TNyxExpressionOperator; AKind: TNyxStateKind;
  const AOperands: array of INyxExpression;
  AComparison: TNyxExpressionTextComparison = netExact): INyxExpression;
var
  LValues: array of TNyxDataValue;
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin
  SetLength(LValues, System.Length(AOperands));
  for LIndex := 0 to High(AOperands) do
  begin

    if AOperands[LIndex] = nil then
    begin
      raise ENyxExpression.Create('Expression requires every typed operand');
    end;
    LValues[LIndex] := AOperands[LIndex].ToData;
  end;
  SetLength(LFields, 4);
  LFields[0] := NyxField('version', NyxData(1));
  LFields[1] := NyxField('kind', NyxData(NyxStateKindName(AKind)));
  LFields[2] := NyxField('op', NyxData(NyxExpressionOperatorName(AOperator)));
  LFields[3] := NyxField('args', NyxArray(LValues));

  if (System.Length(AOperands) = 2) and (AOperands[0].Kind = nskText) and
    (AOperator in [neoEqual, neoNotEqual, neoContains, neoStartsWith, neoEndsWith]) then
  begin
    SetLength(LFields, 5);
    LFields[4] := NyxField('comparison', NyxData(NyxExpressionTextComparisonName(AComparison)));
  end;
  Result := NyxExpressionFromData(NyxObject(LFields));
end;

function Literal(const AValue: TNyxStateValue): INyxExpression;
begin
  Result := NyxExpressionFromData(NyxObject([
    NyxField('version', NyxData(1)),
    NyxField('kind', NyxData(NyxStateKindName(AValue.Kind))),
    NyxField('op', NyxData(COperators[neoLiteral])),
    NyxField('value', ScalarData(AValue))]));
end;

function Reference(const AName: TNyxText; AKind: TNyxStateKind): INyxExpression;
begin
  Result := NyxExpressionFromData(NyxObject([
    NyxField('version', NyxData(1)),
    NyxField('kind', NyxData(NyxStateKindName(AKind))),
    NyxField('op', NyxData(COperators[neoState])),
    NyxField('state', NyxData(StateName(AName, AKind)))]));
end;

function TBooleanExpression.Evaluate(const AInputs: INyxExpressionInputs): Boolean;
begin
  Result := Value(AInputs).BooleanValue;
end;

function TBooleanExpression.AndAlso(const AOther: INyxBooleanExpression): INyxBooleanExpression;
begin
  Result := Compose(neoAnd, nskBoolean,
    [Self as INyxExpression, AOther]) as INyxBooleanExpression;
end;

function TBooleanExpression.OrElse(const AOther: INyxBooleanExpression): INyxBooleanExpression;
begin
  Result := Compose(neoOr, nskBoolean,
    [Self as INyxExpression, AOther]) as INyxBooleanExpression;
end;

function TBooleanExpression.EqualTo(const AOther: INyxBooleanExpression): INyxBooleanExpression;
begin
  Result := Compose(neoEqual, nskBoolean,
    [Self as INyxExpression, AOther]) as INyxBooleanExpression;
end;

function TBooleanExpression.NotEqualTo(const AOther: INyxBooleanExpression): INyxBooleanExpression;
begin
  Result := Compose(neoNotEqual, nskBoolean,
    [Self as INyxExpression, AOther]) as INyxBooleanExpression;
end;

function TBooleanExpression.Negated: INyxBooleanExpression;
begin
  Result := Compose(neoNot, nskBoolean,
    [Self as INyxExpression]) as INyxBooleanExpression;
end;

function TBooleanExpression.AsText: INyxTextExpression;
begin
  Result := Compose(neoText, nskText,
    [Self as INyxExpression]) as INyxTextExpression;
end;

function NyxBooleanExpression(AValue: Boolean): INyxBooleanExpression;
begin
  Result := Literal(TNyxStateValue.FromBoolean(AValue)) as INyxBooleanExpression;
end;

function NyxBooleanExpression(const ARef: TNyxBooleanStateRef): INyxBooleanExpression;
begin
  Result := Reference(ARef.Name, nskBoolean) as INyxBooleanExpression;
end;

function NyxChoose(const ACondition: INyxBooleanExpression;
  const AThen, AElse: INyxBooleanExpression): INyxBooleanExpression;
begin
  Result := Compose(neoChoose, nskBoolean, [ACondition, AThen, AElse]) as INyxBooleanExpression;
end;

function TTextExpression.Evaluate(const AInputs: INyxExpressionInputs): TNyxText;
begin
  Result := Value(AInputs).TextValue;
end;

function TTextExpression.Append(const AOther: INyxTextExpression): INyxTextExpression;
begin
  Result := Compose(neoConcat, nskText,
    [Self as INyxExpression, AOther]) as INyxTextExpression;
end;

function TTextExpression.EqualTo(const AOther: INyxTextExpression;
  AComparison: TNyxExpressionTextComparison): INyxBooleanExpression;
begin
  Result := Compose(neoEqual, nskBoolean,
    [Self as INyxExpression, AOther], AComparison) as INyxBooleanExpression;
end;

function TTextExpression.NotEqualTo(const AOther: INyxTextExpression;
  AComparison: TNyxExpressionTextComparison): INyxBooleanExpression;
begin
  Result := Compose(neoNotEqual, nskBoolean,
    [Self as INyxExpression, AOther], AComparison) as INyxBooleanExpression;
end;

function TTextExpression.Contains(const AOther: INyxTextExpression;
  AComparison: TNyxExpressionTextComparison): INyxBooleanExpression;
begin
  Result := Compose(neoContains, nskBoolean,
    [Self as INyxExpression, AOther], AComparison) as INyxBooleanExpression;
end;

function TTextExpression.StartsWith(const AOther: INyxTextExpression;
  AComparison: TNyxExpressionTextComparison): INyxBooleanExpression;
begin
  Result := Compose(neoStartsWith, nskBoolean,
    [Self as INyxExpression, AOther], AComparison) as INyxBooleanExpression;
end;

function TTextExpression.EndsWith(const AOther: INyxTextExpression;
  AComparison: TNyxExpressionTextComparison): INyxBooleanExpression;
begin
  Result := Compose(neoEndsWith, nskBoolean,
    [Self as INyxExpression, AOther], AComparison) as INyxBooleanExpression;
end;

function TTextExpression.Length: INyxIntegerExpression;
begin
  Result := Compose(neoLength, nskInteger,
    [Self as INyxExpression]) as INyxIntegerExpression;
end;

function NyxTextExpression(const AValue: TNyxText): INyxTextExpression;
begin
  Result := Literal(TNyxStateValue.FromText(AValue)) as INyxTextExpression;
end;

function NyxTextExpression(const ARef: TNyxTextStateRef): INyxTextExpression;
begin
  Result := Reference(ARef.Name, nskText) as INyxTextExpression;
end;

function NyxChoose(const ACondition: INyxBooleanExpression;
  const AThen, AElse: INyxTextExpression): INyxTextExpression;
begin
  Result := Compose(neoChoose, nskText, [ACondition, AThen, AElse]) as INyxTextExpression;
end;

function TIntegerExpression.Evaluate(const AInputs: INyxExpressionInputs): Integer;
begin
  Result := Value(AInputs).IntegerValue;
end;

function TIntegerExpression.Plus(const AOther: INyxIntegerExpression): INyxIntegerExpression;
begin
  Result := Compose(neoAdd, nskInteger,
    [Self as INyxExpression, AOther]) as INyxIntegerExpression;
end;

function TIntegerExpression.Minus(const AOther: INyxIntegerExpression): INyxIntegerExpression;
begin
  Result := Compose(neoSubtract, nskInteger,
    [Self as INyxExpression, AOther]) as INyxIntegerExpression;
end;

function TIntegerExpression.Times(const AOther: INyxIntegerExpression): INyxIntegerExpression;
begin
  Result := Compose(neoMultiply, nskInteger,
    [Self as INyxExpression, AOther]) as INyxIntegerExpression;
end;

function TIntegerExpression.DividedBy(const AOther: INyxIntegerExpression): INyxIntegerExpression;
begin
  Result := Compose(neoDivide, nskInteger,
    [Self as INyxExpression, AOther]) as INyxIntegerExpression;
end;

function TIntegerExpression.Modulo(const AOther: INyxIntegerExpression): INyxIntegerExpression;
begin
  Result := Compose(neoModulo, nskInteger,
    [Self as INyxExpression, AOther]) as INyxIntegerExpression;
end;

function TIntegerExpression.EqualTo(const AOther: INyxIntegerExpression): INyxBooleanExpression;
begin
  Result := Compose(neoEqual, nskBoolean,
    [Self as INyxExpression, AOther]) as INyxBooleanExpression;
end;

function TIntegerExpression.NotEqualTo(const AOther: INyxIntegerExpression): INyxBooleanExpression;
begin
  Result := Compose(neoNotEqual, nskBoolean,
    [Self as INyxExpression, AOther]) as INyxBooleanExpression;
end;

function TIntegerExpression.LessThan(const AOther: INyxIntegerExpression): INyxBooleanExpression;
begin
  Result := Compose(neoLess, nskBoolean,
    [Self as INyxExpression, AOther]) as INyxBooleanExpression;
end;

function TIntegerExpression.AtMost(const AOther: INyxIntegerExpression): INyxBooleanExpression;
begin
  Result := Compose(neoAtMost, nskBoolean,
    [Self as INyxExpression, AOther]) as INyxBooleanExpression;
end;

function TIntegerExpression.GreaterThan(const AOther: INyxIntegerExpression): INyxBooleanExpression;
begin
  Result := Compose(neoGreater, nskBoolean,
    [Self as INyxExpression, AOther]) as INyxBooleanExpression;
end;

function TIntegerExpression.AtLeast(const AOther: INyxIntegerExpression): INyxBooleanExpression;
begin
  Result := Compose(neoAtLeast, nskBoolean,
    [Self as INyxExpression, AOther]) as INyxBooleanExpression;
end;

function TIntegerExpression.Negated: INyxIntegerExpression;
begin
  Result := Compose(neoNegate, nskInteger,
    [Self as INyxExpression]) as INyxIntegerExpression;
end;

function TIntegerExpression.AsText: INyxTextExpression;
begin
  Result := Compose(neoText, nskText,
    [Self as INyxExpression]) as INyxTextExpression;
end;

function TIntegerExpression.AsNumber: INyxNumberExpression;
begin
  Result := Compose(neoNumber, nskNumber,
    [Self as INyxExpression]) as INyxNumberExpression;
end;

function NyxIntegerExpression(AValue: Integer): INyxIntegerExpression;
begin
  Result := Literal(TNyxStateValue.FromInteger(AValue)) as INyxIntegerExpression;
end;

function NyxIntegerExpression(const ARef: TNyxIntegerStateRef): INyxIntegerExpression;
begin
  Result := Reference(ARef.Name, nskInteger) as INyxIntegerExpression;
end;

function NyxChoose(const ACondition: INyxBooleanExpression;
  const AThen, AElse: INyxIntegerExpression): INyxIntegerExpression;
begin
  Result := Compose(neoChoose, nskInteger, [ACondition, AThen, AElse]) as INyxIntegerExpression;
end;

function TNumberExpression.Evaluate(const AInputs: INyxExpressionInputs): Double;
begin
  Result := Value(AInputs).NumberValue;
end;

function TNumberExpression.Plus(const AOther: INyxNumberExpression): INyxNumberExpression;
begin
  Result := Compose(neoAdd, nskNumber,
    [Self as INyxExpression, AOther]) as INyxNumberExpression;
end;

function TNumberExpression.Minus(const AOther: INyxNumberExpression): INyxNumberExpression;
begin
  Result := Compose(neoSubtract, nskNumber,
    [Self as INyxExpression, AOther]) as INyxNumberExpression;
end;

function TNumberExpression.Times(const AOther: INyxNumberExpression): INyxNumberExpression;
begin
  Result := Compose(neoMultiply, nskNumber,
    [Self as INyxExpression, AOther]) as INyxNumberExpression;
end;

function TNumberExpression.DividedBy(const AOther: INyxNumberExpression): INyxNumberExpression;
begin
  Result := Compose(neoDivide, nskNumber,
    [Self as INyxExpression, AOther]) as INyxNumberExpression;
end;

function TNumberExpression.EqualTo(const AOther: INyxNumberExpression): INyxBooleanExpression;
begin
  Result := Compose(neoEqual, nskBoolean,
    [Self as INyxExpression, AOther]) as INyxBooleanExpression;
end;

function TNumberExpression.NotEqualTo(const AOther: INyxNumberExpression): INyxBooleanExpression;
begin
  Result := Compose(neoNotEqual, nskBoolean,
    [Self as INyxExpression, AOther]) as INyxBooleanExpression;
end;

function TNumberExpression.LessThan(const AOther: INyxNumberExpression): INyxBooleanExpression;
begin
  Result := Compose(neoLess, nskBoolean,
    [Self as INyxExpression, AOther]) as INyxBooleanExpression;
end;

function TNumberExpression.AtMost(const AOther: INyxNumberExpression): INyxBooleanExpression;
begin
  Result := Compose(neoAtMost, nskBoolean,
    [Self as INyxExpression, AOther]) as INyxBooleanExpression;
end;

function TNumberExpression.GreaterThan(const AOther: INyxNumberExpression): INyxBooleanExpression;
begin
  Result := Compose(neoGreater, nskBoolean,
    [Self as INyxExpression, AOther]) as INyxBooleanExpression;
end;

function TNumberExpression.AtLeast(const AOther: INyxNumberExpression): INyxBooleanExpression;
begin
  Result := Compose(neoAtLeast, nskBoolean,
    [Self as INyxExpression, AOther]) as INyxBooleanExpression;
end;

function TNumberExpression.Negated: INyxNumberExpression;
begin
  Result := Compose(neoNegate, nskNumber,
    [Self as INyxExpression]) as INyxNumberExpression;
end;

function TNumberExpression.AsText: INyxTextExpression;
begin
  Result := Compose(neoText, nskText,
    [Self as INyxExpression]) as INyxTextExpression;
end;

function NyxNumberExpression(AValue: Double): INyxNumberExpression;
begin
  Result := Literal(TNyxStateValue.FromNumber(AValue)) as INyxNumberExpression;
end;

function NyxNumberExpression(const ARef: TNyxNumberStateRef): INyxNumberExpression;
begin
  Result := Reference(ARef.Name, nskNumber) as INyxNumberExpression;
end;

function NyxChoose(const ACondition: INyxBooleanExpression;
  const AThen, AElse: INyxNumberExpression): INyxNumberExpression;
begin
  Result := Compose(neoChoose, nskNumber, [ACondition, AThen, AElse]) as INyxNumberExpression;
end;
