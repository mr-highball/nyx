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

program nyx_expression_plan_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.state, nyx.expressions,
  nyx.binding.types, nyx.model
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Immutable binding plans: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure Verify;
var
  LNode: TNyxNode;
  LCopy: TNyxNode;
  LPlan: INyxExpression;
  LVisible: INyxExpression;
  LEnabled: INyxExpression;
  LSpec: TNyxBindingSpec;
  LRefused: Boolean;
begin
  LNode := TNyxNode.Create(nkLabel, 'planned-label');
  LCopy := nil;
  try
    LNode.SetBinding(TNyxBindingSpec.Computed(bpText, NyxTextExpression(TNyxText('Plan 🌙'))));
    LPlan := LNode.BindingExpression(bpText);
    Check(LNode.BindingExpression(bpText) = LPlan, 'repeated access retains the same immutable admitted plan');
    LCopy := LNode.Clone;
    Check(LCopy.BindingExpression(bpText) = LPlan,
      'clone shares the immutable tree while owning independent descriptor vectors');
    LCopy.SetBinding(TNyxBindingSpec.Computed(bpText, NyxTextExpression('Changed copy')));
    Check(((LCopy.BindingExpression(bpText) as INyxTextExpression).Evaluate(nil) = 'Changed copy') and
      ((LNode.BindingExpression(bpText) as INyxTextExpression).Evaluate(nil) = TNyxText('Plan 🌙')),
      'replacement cannot mutate the original node or retained plan');
    FreeAndNil(LCopy);
    LNode.SetBinding(TNyxBindingSpec.Computed(bpVisible, NyxBooleanExpression(True)));
    LVisible := LNode.BindingExpression(bpVisible);
    LNode.SetBinding(TNyxBindingSpec.Computed(bpEnabled, NyxBooleanExpression(False)));
    LEnabled := LNode.BindingExpression(bpEnabled);
    LNode.RemoveBinding(bpVisible);
    Check((LNode.BindingCount = 2) and (LNode.Bindings[0].Target = bpText) and
      (LNode.Bindings[1].Target = bpEnabled) and
      (LNode.BindingExpression(bpEnabled) = LEnabled),
      'middle removal preserves exact descriptor/plan alignment and order');
    Check((LVisible as INyxBooleanExpression).Evaluate(nil),
      'removed plan remains independently usable while retained');
    LNode.SetBinding(TNyxBindingSpec.Clear(bpText));
    LRefused := False;
    try
      LNode.BindingExpression(bpText);
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and ((LPlan as INyxTextExpression).Evaluate(nil) = TNyxText('Plan 🌙')),
      'clearing retires node access without changing the retained expression');
    LNode.SetBinding(TNyxBindingSpec.Bound(bpText, 'caption', nskText, bdFromState));
    LRefused := False;
    try
      LNode.BindingExpression(bpText);
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'direct replacement cannot expose an obsolete computed plan');
    LNode.RemoveBinding(bpText);
    Check((LNode.BindingCount = 1) and (LNode.BindingExpression(bpEnabled) = LEnabled),
      'first removal preserves the remaining exact plan');
    LNode.RemoveBinding(bpEnabled);
    Check(LNode.BindingCount = 0, 'last removal retires both vectors');
    LRefused := False;
    try
      LNode.SetBinding(TNyxBindingSpec.Computed(bpEnabled, NyxTextExpression('Wrong family')));
    except
      on ENyxState do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LNode.BindingCount = 0), 'failed static admission leaves node vectors unchanged');
    LSpec := TNyxBindingSpec.Computed(bpText, NyxTextExpression('Owned descriptor'));
    LSpec.Copy.Validate;
    LNode.SetBinding(LSpec.Copy);
    Check((LNode.BindingExpression(bpText) as INyxTextExpression).Evaluate(nil) = 'Owned descriptor',
      'copied private admission proof retains the exact normalized family');
    FreeAndNil(LNode);
    Check(((LPlan as INyxTextExpression).Evaluate(nil) = TNyxText('Plan 🌙')) and
      not (LEnabled as INyxBooleanExpression).Evaluate(nil),
      'retained immutable plans outlive all node/clone owners without backreferences');
  finally
    LCopy.Free;
    LNode.Free;
  end;
end;

begin
  try
    Verify;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-expression-plans', 'passed');
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' immutable binding plan checks';
    {$else}
    WriteLn('PASS ', GChecks, ' immutable binding plan checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-expression-plans', 'failed');
      document.body.setAttribute('data-expression-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
