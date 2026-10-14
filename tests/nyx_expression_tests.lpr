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

program nyx_expression_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, {$ifdef PAS2JS}Web,{$endif}
  nyx.text, nyx.state, nyx.data, nyx.expressions;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Typed expressions: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure Refuses(const AExpression: INyxExpression; const AInputs: INyxExpressionInputs;
  const AReason: TNyxText);
var
  LRefused: Boolean;
begin
  LRefused := False;
  try
    AExpression.Value(AInputs);
  except
    on ENyxExpression do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, AReason);
end;

procedure RefusesWire(const AData: TNyxDataValue; const AReason: TNyxText);
var
  LRefused: Boolean;
begin
  LRefused := False;
  try
    NyxExpressionFromData(AData);
  except
    on ENyxExpression do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, AReason);
end;

procedure Verify;
var
  LState: TNyxState;
  LInputs: INyxExpressionInputs;
  LNext: INyxExpressionInputs;
  LCondition: INyxBooleanExpression;
  LCount: INyxIntegerExpression;
  LPrice: INyxNumberExpression;
  LCaption: INyxTextExpression;
  LRoundTrip: INyxExpression;
  LFailing: INyxIntegerExpression;
  LTree: INyxBooleanExpression;
  LDependency: TNyxExpressionDependency;
  LWire: TNyxDataValue;
  LBefore: TNyxText;
  LText: TNyxText;
  LIndex: Integer;
  LRefused: Boolean;
  LLeft: Integer;
  LRight: Integer;
begin
  LState := TNyxState.Create;
  try
    LState.SetValue(NyxBooleanState('ready'), True)
      .SetValue(NyxIntegerState('quantity'), 3)
      .SetValue(NyxNumberState('price'), 12.5)
      .SetValue(NyxNumberState('tax'), 2.5)
      .SetValue(NyxTextState('title'), TNyxText('Notes 🌙'));
    LInputs := CaptureNyxExpressionInputs(LState);
    Check(LInputs.Count = 5, 'input snapshot owns the full typed state frame');
    LCount := NyxIntegerExpression(NyxIntegerState('quantity'));
    LCondition := NyxBooleanExpression(NyxBooleanState('ready'))
      .AndAlso(LCount.AtLeast(NyxIntegerExpression(2)));
    Check(LCondition.Evaluate(LInputs), 'conditions compose multiple strongly typed keys');
    LPrice := NyxNumberExpression(NyxNumberState('price'))
      .Times(LCount.AsNumber)
      .Plus(NyxNumberExpression(NyxNumberState('tax')));
    Check(LPrice.Evaluate(LInputs) = 40, 'explicit integer promotion supplies a calculated total');
    LCaption := NyxTextExpression(NyxTextState('title'))
      .Append(NyxTextExpression(' / '))
      .Append(LCount.AsText);
    Check(LCaption.Evaluate(LInputs) = TNyxText('Notes 🌙 / 3'),
      'captions combine exact supplementary text and explicit scalar projection');
    Check(NyxChoose(LCondition, NyxTextExpression('Ready'), NyxTextExpression('Waiting'))
      .Evaluate(LInputs) = 'Ready', 'typed conditional text chooses its admitted branch');
    Check(NyxBooleanExpression(False).Negated.Evaluate(nil), 'literal negation needs no state');
    Check(NyxBooleanExpression(False).OrElse(NyxBooleanExpression(True)).Evaluate(nil),
      'Boolean alternative uses its right operand');
    Check(NyxBooleanExpression(True).EqualTo(NyxBooleanExpression(True)).Evaluate(nil),
      'Boolean equality retains its family');
    Check(NyxBooleanExpression(True).NotEqualTo(NyxBooleanExpression(False)).Evaluate(nil),
      'Boolean inequality retains its family');
    Check(NyxBooleanExpression(False).AsText.Evaluate(nil) = 'false',
      'Boolean caption projection is explicit and locale independent');
    Check(NyxTextExpression(TNyxText('A🌙é')).Length.Evaluate(nil) = 4,
      'length counts supplementary and combining Unicode scalars');
    Check(NyxTextExpression('HELLO').EqualTo(NyxTextExpression('hello'), netAsciiInsensitive)
      .Evaluate(nil), 'ASCII folding is an explicit typed choice');
    Check(not NyxTextExpression(TNyxText('É')).EqualTo(NyxTextExpression(TNyxText('é')),
      netAsciiInsensitive).Evaluate(nil), 'ASCII folding preserves non-ASCII distinctions');
    Check(NyxTextExpression('alpha beta').Contains(NyxTextExpression('BETA'), netAsciiInsensitive)
      .Evaluate(nil), 'typed text containment honors the declared comparison');
    Check(NyxTextExpression('alpha beta').StartsWith(NyxTextExpression('alpha')).Evaluate(nil),
      'typed prefix search');
    Check(NyxTextExpression('alpha beta').EndsWith(NyxTextExpression('beta')).Evaluate(nil),
      'typed suffix search');
    Check(NyxTextExpression('text').EndsWith(NyxTextExpression('')).Evaluate(nil),
      'empty suffix matches exact text');
    Check(not NyxTextExpression('text').EndsWith(NyxTextExpression('longer text')).Evaluate(nil),
      'longer suffix refuses a match without invalid offsets');
    Check(NyxTextExpression('text').NotEqualTo(NyxTextExpression('other')).Evaluate(nil),
      'text inequality');
    Check(NyxIntegerExpression(6).Plus(NyxIntegerExpression(2)).Minus(NyxIntegerExpression(1))
      .Times(NyxIntegerExpression(3)).Evaluate(nil) = 21, 'integer arithmetic remains fluent');
    Check(NyxIntegerExpression(7).Negated.Evaluate(nil) = -7, 'checked integer negation');
    Check(NyxNumberExpression(2.5).Negated.Evaluate(nil) = -2.5, 'finite numeric negation');
    Check(NyxNumberExpression(2.5).Plus(NyxNumberExpression(1.5)).Minus(NyxNumberExpression(1.0))
      .DividedBy(NyxNumberExpression(2.0)).Evaluate(nil) = 1.5, 'number arithmetic retains fractions');
    Check(NyxIntegerExpression(2).LessThan(NyxIntegerExpression(3)).Evaluate(nil) and
      NyxIntegerExpression(3).GreaterThan(NyxIntegerExpression(2)).Evaluate(nil) and
      NyxIntegerExpression(3).AtMost(NyxIntegerExpression(3)).Evaluate(nil),
      'integer ordering compares values without subtracting unchecked bounds');
    Check(NyxNumberExpression(3.0).AtLeast(NyxNumberExpression(3.0)).Evaluate(nil) and
      NyxNumberExpression(3.0).NotEqualTo(NyxNumberExpression(2.0)).Evaluate(nil),
      'number comparisons remain exact within their Double contract');
    for LLeft := -9 to 9 do
    begin
      for LRight := -5 to 5 do
      begin

        if LRight <> 0 then
        begin
          Check(NyxIntegerExpression(LLeft).DividedBy(NyxIntegerExpression(LRight))
            .Evaluate(nil) = LLeft div LRight, 'integer division agrees with Pascal signed truncation');
          Check(NyxIntegerExpression(LLeft).Modulo(NyxIntegerExpression(LRight))
            .Evaluate(nil) = LLeft mod LRight, 'remainder agrees with Pascal signed arithmetic');
        end;
      end;
    end;
    Refuses(NyxIntegerExpression(High(Integer)).Plus(NyxIntegerExpression(1)), nil,
      'integer addition overflow refuses');
    Refuses(NyxIntegerExpression(Low(Integer)).Negated, nil, 'minimum integer negation refuses');
    Refuses(NyxIntegerExpression(High(Integer)).Times(NyxIntegerExpression(2)), nil,
      'integer multiplication overflow refuses');
    Refuses(NyxIntegerExpression(Low(Integer)).DividedBy(NyxIntegerExpression(-1)), nil,
      'minimum integer division overflow refuses');
    Refuses(NyxIntegerExpression(7).Modulo(NyxIntegerExpression(0)), nil,
      'zero-divisor remainder refuses');
    Refuses(NyxNumberExpression(7.0).DividedBy(NyxNumberExpression(0.0)), nil,
      'zero-divisor number refuses');
    Refuses(NyxNumberExpression(1.7e308).Times(NyxNumberExpression(2.0)), nil,
      'nonfinite numeric result refuses');
    LFailing := NyxIntegerExpression(1).DividedBy(NyxIntegerExpression(0));
    Check(not NyxBooleanExpression(False).AndAlso(LFailing.EqualTo(NyxIntegerExpression(0)))
      .Evaluate(nil), 'Boolean short circuit skips arithmetic in an unused operand');
    Check(NyxChoose(NyxBooleanExpression(False), LFailing, NyxIntegerExpression(42))
      .Evaluate(nil) = 42, 'conditional evaluation skips arithmetic in the unused branch');
    Refuses(NyxBooleanExpression(False).AndAlso(
      NyxBooleanExpression(NyxBooleanState('missing'))), LInputs,
      'all dependencies admit even when Boolean evaluation could skip a branch');
    Refuses(NyxChoose(NyxBooleanExpression(False),
      NyxIntegerExpression(NyxIntegerState('missing')), NyxIntegerExpression(42)), LInputs,
      'unused conditional dependencies cannot hide missing state');
    Refuses(NyxBooleanExpression(NyxBooleanState('quantity')), LInputs,
      'a snapshot refuses a wrong-kind state reference');
    LRefused := False;
    try
      NyxBooleanExpression(NyxBooleanState('same')).AndAlso(
        NyxIntegerExpression(NyxIntegerState('same')).GreaterThan(NyxIntegerExpression(0)));
    except
      on ENyxExpression do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'conflicting typed dependency declarations refuse construction');
    Check((LCondition.DependencyCount = 2) and (LCondition.Dependency(0).Name = 'ready') and
      (LCondition.Dependency(1).Name = 'quantity'), 'dependencies preserve unique first-use order');
    LDependency := LCondition.Dependency(0);
    LDependency.Name := 'caller-changed';
    Check((LDependency.Name = 'caller-changed') and (LCondition.Dependency(0).Name = 'ready'),
      'returned dependency metadata cannot alias the tree');
    LWire := LCaption.ToData;
    LBefore := LWire.ToJSON;
    LRoundTrip := NyxExpressionFromData(TNyxDataValue.ParseJSON(LBefore));
    Check((LRoundTrip.Kind = nskText) and (LRoundTrip.ToData.ToJSON = LBefore) and
      ((LRoundTrip as INyxTextExpression).Evaluate(LInputs) = LCaption.Evaluate(LInputs)),
      'versioned wire preserves exact tree meaning and Unicode');
    RefusesWire(TNyxDataValue.ParseJSON('{"version":2,"kind":"text","op":"literal","value":"x"}'),
      'unknown descriptor version refuses');
    RefusesWire(TNyxDataValue.ParseJSON('{"version":1,"kind":"integer","op":"literal","value":"3"}'),
      'wire strings do not become numeric behavior');
    RefusesWire(TNyxDataValue.ParseJSON('{"version":1,"kind":"boolean","op":"execute","value":true}'),
      'unknown operator cannot execute application code');
    RefusesWire(TNyxDataValue.ParseJSON('{"version":1,"kind":"boolean","op":"literal","value":true,"extra":1}'),
      'unknown descriptor fields refuse');
    RefusesWire(TNyxDataValue.ParseJSON('{"version":1,"kind":"boolean","op":"not","args":[]}'),
      'wrong operand arity refuses');
    LTree := NyxBooleanExpression(False);
    for LIndex := 2 to NyxMaximumExpressionDepth do
    begin
      LTree := LTree.Negated;
    end;
    LBefore := LTree.ToData.ToJSON;
    LRefused := False;
    try
      LTree.Negated;
    except
      on ENyxExpression do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LTree.Depth = NyxMaximumExpressionDepth) and
      (LTree.ToData.ToJSON = LBefore), 'depth refusal preserves the accepted expression');
    LTree := NyxBooleanExpression(True);
    for LIndex := 1 to 7 do
    begin
      LTree := LTree.AndAlso(LTree);
    end;
    LRefused := False;
    try
      LTree.AndAlso(NyxBooleanExpression(True));
    except
      on ENyxExpression do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LTree.NodeCount = 255) and LTree.Evaluate(nil),
      'node-budget refusal preserves a complete accepted Boolean tree');
    LText := TNyxText('🌙');
    for LIndex := 1 to 16 do
    begin
      LText := LText + LText;
    end;
    LRefused := False;
    try
      NyxTextExpression(LText);
    except
      on ENyxExpression do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'wire budget measures UTF-8 bytes rather than browser UTF-16 units');
    LState.SetValue(NyxIntegerState('quantity'), 7);
    LNext := CaptureNyxExpressionInputs(LState);
    Check((LCount.Evaluate(LInputs) = 3) and (LCount.Evaluate(LNext) = 7),
      'independent snapshots do not follow later runtime changes');
    FreeAndNil(LState);
    Check(LCaption.Evaluate(LInputs) = TNyxText('Notes 🌙 / 3'),
      'expression/input owners outlive their source store');
  finally
    LInputs := nil;
    LNext := nil;
    LCondition := nil;
    LCount := nil;
    LPrice := nil;
    LCaption := nil;
    LRoundTrip := nil;
    LFailing := nil;
    LTree := nil;
    LState.Free;
  end;
end;

begin
  try
    Verify;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-nyx-expressions', 'passed');
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' typed expression checks';
    {$else}
    WriteLn('PASS ', GChecks, ' typed expression checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-nyx-expressions', 'failed');
      document.body.textContent := 'FAIL ' + LException.Message;
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
