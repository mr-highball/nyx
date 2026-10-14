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

unit nyx.expressions;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.state, nyx.data;

const
  NyxMaximumExpressionNodes = 256;
  NyxMaximumExpressionDepth = 32;
  NyxMaximumExpressionBytes = 262144;
  NyxMaximumExpressionTextScalars = 262144;

type
  ENyxExpression = class(ENyxState);
  { Exact Unicode by default. ASCII folding changes A..Z only; no ambient
    locale, normalization or platform collation changes expression meaning. }
  TNyxExpressionTextComparison = (netExact, netAsciiInsensitive);
  { Closed descriptor vocabulary. These enum values never select arbitrary
    Pascal routines. Source/code generators use the specialized fluent methods. }
  TNyxExpressionOperator = (neoLiteral, neoState, neoNot, neoAnd, neoOr,
    neoEqual, neoNotEqual, neoLess, neoAtMost, neoGreater, neoAtLeast,
    neoAdd, neoSubtract, neoMultiply, neoDivide, neoModulo, neoNegate,
    neoConcat, neoContains, neoStartsWith, neoEndsWith, neoLength,
    neoText, neoNumber, neoChoose);

  { Copied inspection metadata, not an untyped authoring reference. Names are
    exact; expression construction uses the existing distinct state-ref types. }
  TNyxExpressionDependency = record
    Name: TNyxText;
    Kind: TNyxStateKind;
  end;

  { An immutable input frame owns admitted scalar copies independently of a
    runtime store, document and controls. It can outlive its source and be read
    by worker threads. Implementations must supply stable, exact typed values;
    missing/wrong-kind references raise rather than supplying implicit defaults. }
  INyxExpressionInputs = interface
    ['{AF627AE8-40CC-49C0-8681-E5A8F1FC1001}']
    function ReadText(const ARef: TNyxTextStateRef): TNyxText;
    function ReadBoolean(const ARef: TNyxBooleanStateRef): Boolean;
    function ReadInteger(const ARef: TNyxIntegerStateRef): Integer;
    function ReadNumber(const ARef: TNyxNumberStateRef): Double;
    function GetCount: Integer;
    property Count: Integer read GetCount;
  end;

  INyxExpression = interface;
  INyxBooleanExpression = interface;
  INyxTextExpression = interface;
  INyxIntegerExpression = interface;
  INyxNumberExpression = interface;

  { Immutable reference-counted expression. Trees own expressions and scalar
    descriptors, never inputs, stores, nodes, callbacks or target handles.
    ToData returns an independent strict version-one snapshot. Dependencies are
    unique in first-use order; conflicting types for one exact name refuse.
    ValidateInputs checks ALL dependencies even in a branch not evaluated.
    Value performs that admission before lazy Boolean/conditional evaluation.
    Integer overflow, division by zero and nonfinite results raise; evaluation
    publishes nothing and never changes the supplied immutable frame. }
  INyxExpression = interface
    ['{AF627AE8-40CC-49C0-8681-E5A8F1FC1002}']
    function GetKind: TNyxStateKind;
    function GetOperator: TNyxExpressionOperator;
    function GetNodeCount: Integer;
    function GetDepth: Integer;
    function GetChildCount: Integer;
    function Child(AIndex: Integer): INyxExpression;
    function GetDependencyCount: Integer;
    function Dependency(AIndex: Integer): TNyxExpressionDependency;
    procedure ValidateInputs(const AInputs: INyxExpressionInputs);
    function Value(const AInputs: INyxExpressionInputs): TNyxStateValue;
    function ToData: TNyxDataValue;
    property Kind: TNyxStateKind read GetKind;
    property OperatorKind: TNyxExpressionOperator read GetOperator;
    property NodeCount: Integer read GetNodeCount;
    property Depth: Integer read GetDepth;
    property ChildCount: Integer read GetChildCount;
    property DependencyCount: Integer read GetDependencyCount;
  end;

  { Boolean composition evaluates left first and short-circuits the right.
    Choosing a branch is explicit through the typed NyxChoose overloads. }
  INyxBooleanExpression = interface(INyxExpression)
    ['{AF627AE8-40CC-49C0-8681-E5A8F1FC1003}']
    function Evaluate(const AInputs: INyxExpressionInputs): Boolean;
    function AndAlso(const AOther: INyxBooleanExpression): INyxBooleanExpression;
    function OrElse(const AOther: INyxBooleanExpression): INyxBooleanExpression;
    function Negated: INyxBooleanExpression;
    function EqualTo(const AOther: INyxBooleanExpression): INyxBooleanExpression;
    function NotEqualTo(const AOther: INyxBooleanExpression): INyxBooleanExpression;
    function AsText: INyxTextExpression;
  end;

  { Concatenation is explicit, with bounded exact Unicode results. Length counts
    Unicode scalars on native and browser, including supplementary characters.
    Search is textual only; strings never become identifiers or executable code. }
  INyxTextExpression = interface(INyxExpression)
    ['{AF627AE8-40CC-49C0-8681-E5A8F1FC1004}']
    function Evaluate(const AInputs: INyxExpressionInputs): TNyxText;
    function Append(const AOther: INyxTextExpression): INyxTextExpression;
    function EqualTo(const AOther: INyxTextExpression;
      AComparison: TNyxExpressionTextComparison = netExact): INyxBooleanExpression;
    function NotEqualTo(const AOther: INyxTextExpression;
      AComparison: TNyxExpressionTextComparison = netExact): INyxBooleanExpression;
    function Contains(const AOther: INyxTextExpression;
      AComparison: TNyxExpressionTextComparison = netExact): INyxBooleanExpression;
    function StartsWith(const AOther: INyxTextExpression;
      AComparison: TNyxExpressionTextComparison = netExact): INyxBooleanExpression;
    function EndsWith(const AOther: INyxTextExpression;
      AComparison: TNyxExpressionTextComparison = netExact): INyxBooleanExpression;
    function Length: INyxIntegerExpression;
  end;

  { Signed 32-bit arithmetic is checked on both targets. DividedBy truncates
    toward zero, like Pascal div; Modulo follows that quotient. Use AsNumber
    deliberately when a fractional result is wanted. AsText is locale-independent. }
  INyxIntegerExpression = interface(INyxExpression)
    ['{AF627AE8-40CC-49C0-8681-E5A8F1FC1005}']
    function Evaluate(const AInputs: INyxExpressionInputs): Integer;
    function Plus(const AOther: INyxIntegerExpression): INyxIntegerExpression;
    function Minus(const AOther: INyxIntegerExpression): INyxIntegerExpression;
    function Times(const AOther: INyxIntegerExpression): INyxIntegerExpression;
    function DividedBy(const AOther: INyxIntegerExpression): INyxIntegerExpression;
    function Modulo(const AOther: INyxIntegerExpression): INyxIntegerExpression;
    function Negated: INyxIntegerExpression;
    function EqualTo(const AOther: INyxIntegerExpression): INyxBooleanExpression;
    function NotEqualTo(const AOther: INyxIntegerExpression): INyxBooleanExpression;
    function LessThan(const AOther: INyxIntegerExpression): INyxBooleanExpression;
    function AtMost(const AOther: INyxIntegerExpression): INyxBooleanExpression;
    function GreaterThan(const AOther: INyxIntegerExpression): INyxBooleanExpression;
    function AtLeast(const AOther: INyxIntegerExpression): INyxBooleanExpression;
    function AsText: INyxTextExpression;
    function AsNumber: INyxNumberExpression;
  end;

  { Finite IEEE Double operations store each intermediate in the portable scalar
    contract; no native extended-precision expression becomes persistent state.
    Arithmetic failure refuses evaluation; signed zero follows state canonicalization. }
  INyxNumberExpression = interface(INyxExpression)
    ['{AF627AE8-40CC-49C0-8681-E5A8F1FC1006}']
    function Evaluate(const AInputs: INyxExpressionInputs): Double;
    function Plus(const AOther: INyxNumberExpression): INyxNumberExpression;
    function Minus(const AOther: INyxNumberExpression): INyxNumberExpression;
    function Times(const AOther: INyxNumberExpression): INyxNumberExpression;
    function DividedBy(const AOther: INyxNumberExpression): INyxNumberExpression;
    function Negated: INyxNumberExpression;
    function EqualTo(const AOther: INyxNumberExpression): INyxBooleanExpression;
    function NotEqualTo(const AOther: INyxNumberExpression): INyxBooleanExpression;
    function LessThan(const AOther: INyxNumberExpression): INyxBooleanExpression;
    function AtMost(const AOther: INyxNumberExpression): INyxBooleanExpression;
    function GreaterThan(const AOther: INyxNumberExpression): INyxBooleanExpression;
    function AtLeast(const AOther: INyxNumberExpression): INyxBooleanExpression;
    function AsText: INyxTextExpression;
  end;

{ Capture on the source store's owning thread. The store remains borrowed only
  for this synchronous copy; callers serialize its access as usual. The resulting
  managed immutable frame owns no subscription/store and supports worker reads. }
function CaptureNyxExpressionInputs(AState: TNyxState): INyxExpressionInputs;

{ Literal and reference overloads never coerce another scalar family. }
function NyxBooleanExpression(AValue: Boolean): INyxBooleanExpression; overload;
function NyxBooleanExpression(const ARef: TNyxBooleanStateRef): INyxBooleanExpression; overload;
function NyxTextExpression(const AValue: TNyxText): INyxTextExpression; overload;
function NyxTextExpression(const ARef: TNyxTextStateRef): INyxTextExpression; overload;
function NyxIntegerExpression(AValue: Integer): INyxIntegerExpression; overload;
function NyxIntegerExpression(const ARef: TNyxIntegerStateRef): INyxIntegerExpression; overload;
function NyxNumberExpression(AValue: Double): INyxNumberExpression; overload;
function NyxNumberExpression(const ARef: TNyxNumberStateRef): INyxNumberExpression; overload;

{ Only the chosen branch is evaluated, after complete dependency/type admission.
  Both branch interfaces must have the same Pascal family. Nil operands refuse. }
function NyxChoose(const ACondition: INyxBooleanExpression;
  const AThen, AElse: INyxBooleanExpression): INyxBooleanExpression; overload;
function NyxChoose(const ACondition: INyxBooleanExpression;
  const AThen, AElse: INyxTextExpression): INyxTextExpression; overload;
function NyxChoose(const ACondition: INyxBooleanExpression;
  const AThen, AElse: INyxIntegerExpression): INyxIntegerExpression; overload;
function NyxChoose(const ACondition: INyxBooleanExpression;
  const AThen, AElse: INyxNumberExpression): INyxNumberExpression; overload;

{ Explicit persistence/extension boundaries. Decode normalizes alternate
  implementations to an independent immutable tree, validates every closed
  member/type/arity and refuses unknown versions/operators before returning. }
function NyxExpressionFromData(const AData: TNyxDataValue): INyxExpression;
function NyxExpressionOperatorName(AOperator: TNyxExpressionOperator): TNyxText;
function NyxExpressionTextComparisonName(AComparison: TNyxExpressionTextComparison): TNyxText;

implementation

uses
  Math, nyx.text.index;

{$include nyx.expressions.implementation.inc}

end.
