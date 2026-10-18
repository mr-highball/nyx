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
program nyx_pascal_constant_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.scheduler, nyx.pascal.lexing, nyx.pascal.context,
  nyx.pascal.files, nyx.pascal.translation, nyx.pascal.syntax,
  nyx.pascal.syntax.translated, nyx.pascal.binding, nyx.pascal.constants,
  nyx.pascal.values, nyx.pascal.conditions
  {$ifndef PAS2JS}, nyx.pascal.files.native, nyx.pascal.modules,
  constant.meaning.fixture, shift.meaning.fixture, mask.meaning.fixture{$endif};

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
end;

function Facts(AWordBytes: Integer = 2): INyxPascalCompilerContext;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi).CompleteDefines(True)
    .WithOption('H', ppkTrue);
  Result := NyxPascalWithCapturedSize(Result, NyxPascalSizeReference('Word'),
    NyxPascalSizeFact(pssKnown, ppmDelphi, ppkTrue, AWordBytes));
  Result := NyxPascalWithCapturedSize(Result, NyxPascalSizeReference('Int64'),
    NyxPascalSizeFact(pssKnown, ppmDelphi, ppkTrue, 8));
  Result := NyxPascalWithCapturedSize(Result, NyxPascalSizeReference('QWord'),
    NyxPascalSizeFact(pssKnown, ppmDelphi, ppkTrue, 8));
end;

function View(const AText: TNyxText; const AContext: INyxPascalCompilerContext): INyxPascalTranslatedSyntax;
begin
  Result := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(
    NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('case.pas'), AText),
    NyxPascalPhysicalFile('case.pas'), AContext, TNyxPascalTranslationOptions.Defaults),
    TNyxPascalSyntaxOptions.Defaults);
end;

function Site(const AText: TNyxText; const AContext: INyxPascalCompilerContext): INyxPascalBindingSite;
var
  LWorld: INyxPascalSemanticWorld;
  LView: INyxPascalTranslatedSyntax;
begin
  LView := View(AText, AContext);
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LView);
  Result := LWorld.At(0, 0, LView.Syntax.Lexical.Scalars);
end;

function Evaluate(const ASite: INyxPascalBindingSite; const AText: TNyxText): INyxPascalConstantResult;
begin
  Result := PlanNyxPascalConstant(AText, TNyxPascalConstantOptions.Defaults)
    .Evaluate(ASite, TNyxPascalConstantOptions.Defaults);
end;

procedure Number(const ASite: INyxPascalBindingSite; const AExpression, AExpected: TNyxText);
var
  LResult: INyxPascalConstantResult;
begin
  LResult := Evaluate(ASite, AExpression);
  WriteLn('constant / ', AExpression, ' / kind=', Ord(LResult.Value.Kind),
    ' issue=', Ord(LResult.Issue), ' related=', LResult.RelatedName);
  Check((LResult.Issue = pbiNone) and (LResult.Value.Kind = pvvInteger) and
    (LResult.Value.Number.ToText = AExpected), 'Exact constant value: ' + String(AExpression));
end;

procedure Truth(const ASite: INyxPascalBindingSite; const AExpression: TNyxText; AExpected: Boolean);
var
  LResult: INyxPascalConstantResult;
begin
  LResult := Evaluate(ASite, AExpression);
  Check((LResult.Issue = pbiNone) and (LResult.Value.Kind = pvvBoolean) and
    (LResult.Value.BooleanResult = AExpected), 'Exact Boolean value: ' + String(AExpression));
end;

procedure ValuesAndScopes;
const
  CSource: TNyxText = 'unit ValueCase; interface ' +
    'const Seed=7; Answer=Seed*6; FutureTrap=FutureValue; FutureValue=99; ' +
    'type TBand=-2..42; TAlias=type TBand; TChoice=(first=3,middle,last=9); ' +
    'const Width=High(TAlias)-Low(TAlias)+1; EnumOrdinal=Ord(High(TChoice)); ' +
    'var ErrorCode:Word; implementation procedure Scope; const Seed=2; Local=Seed+1; ' +
    'begin end; end.';
var
  LSite: INyxPascalBindingSite;
  LLocal: INyxPascalBindingSite;
  LResult: INyxPascalConstantResult;
  LAt: Integer;
  LDeclaration: TNyxPascalSyntaxDeclaration;
  LPlan: INyxPascalConstantPlan;
  LWorld: INyxPascalSemanticWorld;
  LOther: INyxPascalTranslatedSyntax;
  LConditional: TNyxPascalConstantOptions;
begin
  LSite := Site(CSource, Facts);
  Number(LSite, 'Answer', '42');
  Number(LSite, 'Width', '45');
  Number(LSite, 'EnumOrdinal', '9');
  Number(LSite, 'Ord(middle)', '4');
  Number(LSite, 'Low(TBand)', '-2');
  Number(LSite, 'High(TAlias)', '42');
  Number(LSite, 'High(ErrorCode)', '65535');
  Number(LSite, 'Low(Int64)', '-9223372036854775808');
  Number(LSite, 'High(QWord)', '18446744073709551615');
  Number(LSite, '9007199254740993+2', '9007199254740995');
  Number(LSite, '$ffffffffffffffff', '-1');
  Number(LSite, '%101010', '42');
  Number(LSite, '&52', '42');
  Number(LSite, '2+5*8', '42');
  Number(LSite, '1 shl 5', '32');
  Number(LSite, '128 shr 2', '32');
  Number(LSite, '1 << 5', '32');
  Number(LSite, '128 >> 2', '32');
  Number(LSite, '1 shl 5 + 1', '33');
  Number(LSite, '128 shr 2 div 2', '16');
  Number(LSite, '9007199254740993 shl 1', '18014398509481986');
  Number(LSite, '1 shl 63', '-9223372036854775808');
  Number(LSite, '(1 shl 63) shl 1', '0');
  Number(LSite, '(-1) shr 1', '9223372036854775807');
  Number(LSite, '(-1) shr 63', '1');
  Number(LSite, 'High(QWord) shr 63', '1');
  Number(LSite, 'High(QWord) shl 1', '18446744073709551614');
  Number(LSite, 'High(Word) shl 48', '18446462598732840960');
  Number(LSite, '(1 shl 5) shl 0', '32');
  Number(LSite, '-19 div 3', '-6');
  Number(LSite, '-19 mod 3', '-1');
  Number(LSite, '19 div -3', '-6');
  Number(LSite, '19 mod -3', '1');
  Number(LSite, '18446744073709551615 div 3', '6148914691236517205');
  LConditional := TNyxPascalConstantOptions.Defaults.InDialect(pcdFPCConditional);
  LResult := PlanNyxPascalConstant('$ffffffffffffffff', LConditional).Evaluate(LSite, LConditional);
  Check((LResult.Issue = pbiNone) and (LResult.Value.Number.ToText = TNyxText('-1')),
    'FPC conditional radix literals retain their independently verified signed interpretation');
  LResult := PlanNyxPascalConstant('Ord(High(QWord))', LConditional).Evaluate(LSite, LConditional);
  Check((LResult.Issue = pbiNone) and (LResult.Value.Number.ToText = TNyxText('-1')),
    'FPC conditional Ord preserves its signed projection independently of an unsigned High result');
  Check(not PlanNyxPascalConstant('Low(TBand)=-2', LConditional).SyntaxSupported,
    'The unsupported FPC LOW conditional cannot borrow ordinary Pascal intrinsic meaning');
  Truth(LSite, 'Answer=42', True);
  Truth(LSite, '(High(TBand)=Answer) and not false', True);
  Truth(LSite, 'defined(ABSENT) or declared(Answer)', True);
  LResult := Evaluate(LSite, 'Answer');
  Check((LResult.DependencyCount = 2) and (LResult.WorkUnits > 0),
    'Declaration evaluation retains exact distinct source dependencies and work');
  LSite := nil;
  Check(LResult.Dependency(0).Declaration.Name = TNyxText('Answer'),
    'Result navigation owns its original world after caller retirement');
  LSite := Site(CSource, Facts);
  for LAt := 0 to LSite.World.ModuleAt(0).Syntax.DeclarationCount - 1 do
  begin
    LDeclaration := LSite.World.ModuleAt(0).Syntax.Declaration(LAt);

    if LDeclaration.Name = TNyxText('Local') then
    begin
      LLocal := LSite.World.At(0, LDeclaration.Scope, LDeclaration.ExpressionSpan.Start);
    end;
  end;
  Check(LLocal <> nil, 'The unchanged local constant has an actual source scope');
  Number(LLocal, 'Local', '3');
  LPlan := PlanNyxPascalConstant('Answer', TNyxPascalConstantOptions.Defaults);
  Number(LLocal, LPlan.SourceText, '42');
  LResult := Evaluate(LSite, 'FutureTrap');
  Check((LResult.Value.Kind = pvvUnknown) and (LResult.Issue <> pbiNone),
    'Later constants cannot donate values to an earlier declaration site');
  LResult := Evaluate(LSite, 'High(TChoice)');
  Check((LResult.Issue = pbiNone) and (LResult.Value.Kind = pvvOrdinal),
    'An enum bound preserves ordinal intent independently of numeric projection');
  LResult := Evaluate(LSite, 'High(TChoice)=9');
  Check((LResult.Value.Kind = pvvUnknown) and (LResult.Issue <> pbiNone),
    'An enum value cannot silently enter integer comparison without qualified nominal rules');
  LSite := Site('unit Shadow; interface type Word=-5..5; var Value:Word; implementation end.', Facts);
  Number(LSite, 'High(Value)', '5');
  LSite := Site('unit WrongFacts; interface var Value:Word; implementation end.', Facts(3));
  LResult := Evaluate(LSite, 'High(Value)');
  Check((LResult.Value.Kind = pvvUnknown) and (LResult.Issue = pbiCompilerContext),
    'Inconsistent primitive measurement cannot invent an ordinal bound');
  LSite := Site('unit MissingFacts; interface var Value:Word; implementation end.',
    NewNyxPascalCompilerContext(ppcFPC, TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi));
  LResult := Evaluate(LSite, 'High(Value)');
  Check((LResult.Value.Kind = pvvUnknown) and (LResult.Issue = pbiCompilerContext),
    'An uncaptured primitive bound remains unknown even when its family name is recognized');
  LSite := Site('unit InlineRange; interface var Value:-3..6; implementation end.', Facts);
  Number(LSite, 'High(Value)-Low(Value)', '9');
  LSite := Site('unit Independent; interface const Answer=13; implementation end.', Facts);
  LResult := LPlan.Evaluate(LSite, TNyxPascalConstantOptions.Defaults);
  Check((LResult.Issue = pbiNone) and (LResult.Value.Number.ToText = TNyxText('13')),
    'A reusable immutable plan binds to each new source world without cached values');
  LOther := View('unit OtherValues; interface const Seed=21; Answer=Seed*2; implementation end.', Facts);
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LOther)
    .WithModule(View('unit Consumer; interface uses OtherValues; const Seed=999; implementation end.', Facts));
  LSite := LWorld.At(1, 0, LWorld.ModuleAt(1).Syntax.Lexical.Scalars);
  Number(LSite, 'OtherValues.Answer', '42');
  LSite := Site('unit Physical; interface { A supplementary clue: 🧵 } const SourceValue=42; implementation end.', Facts);
  LResult := Evaluate(LSite, 'SourceValue');
  Check((LResult.DependencyCount = 1) and
    (LResult.Dependency(0).World.ModuleAt(0).Source.Files.TextAt(0) =
      TNyxText('unit Physical; interface { A supplementary clue: 🧵 } const SourceValue=42; implementation end.')) and
    (LResult.Dependency(0).World.ModuleAt(0).Syntax.Lexical.Slice(
      LResult.Dependency(0).Declaration.NameSpan) = TNyxText('SourceValue')),
    'Exact original supplementary text and declaration navigation remain source-owned');
end;

procedure ShiftContracts;
const
  CUnknown: array[0..7] of TNyxText = ('1 shl -1', '1 shl 64', '1 shr 65',
    '1 shl 9007199254740993', 'true shl 1', '1 shl true', 'first shl 1',
    '1 shr first');
var
  LSite: INyxPascalBindingSite;
  LResult: INyxPascalConstantResult;
  LValue: TNyxPascalConstantValue;
  LOriginal: TNyxPascalConstantValue;
  LAt: Integer;
begin
  LSite := Site('unit ShiftGuard; interface type TChoice=(first,last); implementation end.', Facts);
  for LAt := Low(CUnknown) to High(CUnknown) do
  begin
    LResult := Evaluate(LSite, CUnknown[LAt]);
    Check((LResult.Value.Kind = pvvUnknown) and (LResult.Issue <> pbiNone),
      'Undefined shifts and incompatible operands refuse: ' + String(CUnknown[LAt]));
  end;
  LOriginal := TNyxPascalConstantValue.IntegerValue(-1);
  LValue := LOriginal.Shifted64(psdRight, TNyxPascalConstantValue.IntegerValue(1).AsUnsigned);
  Check((LValue.Number.ToText = TNyxText('9223372036854775807')) and
    not LValue.IsUnsigned and (LOriginal.Number.ToText = TNyxText('-1')),
    'The count does not donate signedness or mutate the original record');
  LOriginal := TNyxPascalConstantValue.IntegerValue(1).AsUnsigned;
  LValue := LOriginal.Shifted64(psdLeft, TNyxPascalConstantValue.IntegerValue(63));
  Check(LValue.IsUnsigned and (LValue.Number.ToText = TNyxText('9223372036854775808')) and
    (LOriginal.Number.ToText = TNyxText('1')), 'Explicit unsigned-64 intent survives a high-bit shift independently');
  LValue := LValue.Shifted64(psdRight, TNyxPascalConstantValue.IntegerValue(63));
  Check(LValue.IsUnsigned and (LValue.Number.ToText = TNyxText('1')),
    'Small results retain explicit unsigned-64 value intent');
  LSite := Site('unit Uncaptured; interface implementation end.',
    NewNyxPascalCompilerContext(ppcFPC, TNyxPascalCompilerVersion.Create(0, 0, 0), ppmDelphi));
  LResult := Evaluate(LSite, '1 shl 5');
  Check((LResult.Value.Kind = pvvUnknown) and (LResult.Issue = pbiCompilerContext),
    'A missing compiler version cannot borrow this build host''s constant-folding domain');
  LSite := Site('unit OtherCompiler; interface implementation end.',
    NewNyxPascalCompilerContext(ppcPas2JS, TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi));
  LResult := Evaluate(LSite, '1 shl 5');
  Check((LResult.Value.Kind = pvvUnknown) and (LResult.Issue = pbiCompilerContext),
    'Another source compiler requires its own qualified shift semantics');
  Check(not PlanNyxPascalConstant('1 << 5', TNyxPascalConstantOptions.Defaults
    .InDialect(pcdFPCConditional)).SyntaxSupported,
    'Ordinary Pascal symbolic shifts do not expand FPC conditional grammar');
end;

procedure IntegerMasks;
var
  LSite: INyxPascalBindingSite;
  LResult: INyxPascalConstantResult;
  LOriginal: TNyxPascalConstantValue;
  LValue: TNyxPascalConstantValue;
  LBit: TNyxPascalConstantValue;
  LAll: TNyxPascalConstantValue;
  LAt: Integer;
begin
  LSite := Site('unit MaskValues; interface implementation end.', Facts);
  Number(LSite, '63 and 42', '42');
  Number(LSite, '32 or 10', '42');
  Number(LSite, '63 xor 21', '42');
  Number(LSite, '(-1) and 42', '42');
  Number(LSite, '(-64) or 42', '-22');
  Number(LSite, '(-1) xor 42', '-43');
  Number(LSite, '(416 + (1 shl 10 - 1) shl 10) and -32', '1047968');
  Number(LSite, 'High(QWord) and High(QWord)', '18446744073709551615');
  Number(LSite, 'High(QWord) or 0', '18446744073709551615');
  Number(LSite, 'High(QWord) xor High(QWord)', '0');
  Number(LSite, '$8000000000000001', '-9223372036854775807');
  Number(LSite, '$8000000000000001 and $f000000000000003', '-9223372036854775807');
  Number(LSite, '9223372036854775809 and 17293822569102704643', '9223372036854775809');
  Number(LSite, '9007199254740993 xor 1', '9007199254740992');
  Number(LSite, '(-1) xor (-1)', '0');
  LOriginal := TNyxPascalConstantValue.IntegerValue(42);
  LValue := LOriginal.Complemented64;
  Check((LValue.Number.ToText = TNyxText('-43')) and not LValue.IsUnsigned and
    (LOriginal.Number.ToText = TNyxText('42')), 'Explicit signed complement preserves its independent operand');
  Check(LValue.Complemented64.Number.ToText = TNyxText('42'),
    'Double explicit complement restores the exact signed pattern');
  LValue := TNyxPascalConstantValue.IntegerValue(1).AsUnsigned.Complemented64;
  Check(LValue.IsUnsigned and (LValue.Number.ToText = TNyxText('18446744073709551614')),
    'Explicit unsigned complement retains all 64 bits beyond browser native operators');
  LValue := LValue.Bitwise64(pbwAnd, TNyxPascalConstantValue.IntegerValue(0));
  Check(LValue.IsUnsigned and (LValue.Number.ToText = TNyxText('0')),
    'A cleared result still retains explicit unsigned-64 intent');
  LAll := NyxPascalIntegerToken('$ffffffffffffffff', 16);
  for LAt := 0 to 63 do
  begin
    LBit := TNyxPascalConstantValue.IntegerValue(1).AsUnsigned.Shifted64(psdLeft,
      TNyxPascalConstantValue.IntegerValue(LAt));
    Check(LBit.Bitwise64(pbwAnd, LAll).Number.ToText = LBit.Number.ToText,
      'Every bit position survives the all-ones identity across byte boundaries');
    Check(LBit.Bitwise64(pbwXor, LBit).Number.ToText = TNyxText('0'),
      'Every bit position cancels under self XOR');
    Check(LBit.Bitwise64(pbwOr, TNyxPascalConstantValue.IntegerValue(0)).Number.ToText =
      LBit.Number.ToText, 'Every bit position survives the zero OR identity');
    Check(LBit.Complemented64.Complemented64.Number.ToText = LBit.Number.ToText,
      'Complement involution preserves every explicit unsigned bit position');
  end;
  Check(TNyxPascalConstantValue.BooleanValue(True).Complemented64.Kind = pvvUnknown,
    'Explicit integer complement cannot silently accept a Boolean');
  LValue := NyxPascalIntegerToken('$ffffffffffffffff', 16);
  Check(LValue.IsUnsigned and (LValue.Number.ToText = TNyxText('18446744073709551615')),
    'Raw integer-token interchange remains mathematical independently of compiler source interpretation');
  LResult := Evaluate(LSite, 'not 1');
  Check((LResult.Value.Kind = pvvUnknown) and (LResult.Issue <> pbiNone),
    'Source NOT retains its separate width/promotion prerequisite');
  LSite := Site('unit OtherMasks; interface implementation end.',
    NewNyxPascalCompilerContext(ppcPas2JS, TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi));
  LResult := Evaluate(LSite, '63 and 42');
  Check((LResult.Value.Kind = pvvUnknown) and (LResult.Issue = pbiCompilerContext),
    'Integer masking cannot borrow another compiler family from the analyzer host');
  LResult := Evaluate(LSite, '$ffffffffffffffff');
  Check((LResult.Value.Kind = pvvUnknown) and (LResult.Issue = pbiCompilerContext),
    'A high-bit source radix token requires its actual compiler authority');
  LSite := Site('unit MissingMaskAuthority; interface implementation end.',
    NewNyxPascalCompilerContext(ppcFPC, TNyxPascalCompilerVersion.Create(0, 0, 0), ppmDelphi));
  LResult := Evaluate(LSite, '63 and 42');
  Check((LResult.Value.Kind = pvvUnknown) and (LResult.Issue = pbiCompilerContext),
    'An unknown compiler version cannot acquire integer pattern semantics');
end;

procedure Refusals;
var
  LSite: INyxPascalBindingSite;
  LResult: INyxPascalConstantResult;
  LPlan: INyxPascalConstantPlan;
  LCancel: INyxCancellationScope;
  LRejected: Boolean;
  LIndex: Integer;
  LValue: TNyxPascalConstantValue;
  LConditional: TNyxPascalConstantOptions;
const
  CUnknown: array[0..15] of TNyxText = ('Loop', 'Variable', '1 div 0',
    '18446744073709551615+1', '-9223372036854775808-1',
    'High(QWord) and (-1)', 'not 1', 'Ord(true)+High(Word)+High(QWord)', 'Missing',
    '(High(QWord)-High(QWord))-1', '9223372036854775807+1',
    '-9223372036854775808 div -1', '-1<High(QWord)', '-Low(Int64)',
    'High(QWord) or (-1)', 'High(QWord) xor (-1)');
begin
  LSite := Site('unit Refusal; interface const Loop=Loop+1; ' +
    'var Variable:Word; implementation end.', Facts);
  LResult := Evaluate(LSite, 'Loop');
  Check((LResult.Value.Kind = pvvUnknown) and (LResult.Issue = pbiRecursiveType),
    'A declaration cycle is detected before recursive expansion can continue');
  for LIndex := Low(CUnknown) to High(CUnknown) do
  begin
    LResult := Evaluate(LSite, CUnknown[LIndex]);
    Check((LResult.Value.Kind = pvvUnknown) and (LResult.Issue <> pbiNone),
      'Unknowns and unsafe operations remain explicit: ' + String(CUnknown[LIndex]));
  end;
  LResult := Evaluate(LSite, 'High(QWord)-High(QWord)');
  Check((LResult.Issue = pbiNone) and LResult.Value.IsUnsigned and
    (LResult.Value.Number.ToText = TNyxText('0')),
    'Unsigned intent survives small results and protects later arithmetic from silent underflow');
  Check(not PlanNyxPascalConstant('RunAnything()', TNyxPascalConstantOptions.Defaults).SyntaxSupported,
    'An ordinary call is never interpreted as an intrinsic or executed');
  Check(not PlanNyxPascalConstant('007=7', TNyxPascalConstantOptions.Defaults
    .InDialect(pcdFPCConditional)).SyntaxSupported,
    'Noncanonical directive decimal rules retain the previous compiler-context guard');
  Check(not PlanNyxPascalConstant('defined(&ABSENT)', TNyxPascalConstantOptions.Defaults).SyntaxSupported,
    'Escaped compiler-symbol syntax is not recast as a verified define');
  LRejected := False;
  LValue := TNyxPascalConstantValue.BooleanValue(True);
  try
    LValue.Number;
  except
    on ENyxPascalValue do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Typed value getters refuse incompatible kinds');
  LRejected := False;
  try
    LResult.Dependency(999);
  except
    on ENyxPascalBinding do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Dependency getters enforce their exact result bounds');
  LPlan := PlanNyxPascalConstant('High(Word)', TNyxPascalConstantOptions.Defaults);
  LResult := nil;
  LRejected := False;
  try
    LResult := LPlan.Evaluate(LSite, TNyxPascalConstantOptions.Defaults.UpToSteps(1));
  except
    on ENyxPascalConstantLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LResult = nil), 'Work exhaustion publishes no partial value or dependencies');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LRejected := False;
  try
    LResult := LPlan.Evaluate(LSite, TNyxPascalConstantOptions.Defaults, LCancel);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LResult = nil), 'Cancelled evaluation publishes no partial value');
  LSite := Site('unit Deep; interface const A=3; B=A+1; C=B+1; implementation end.', Facts);
  LPlan := PlanNyxPascalConstant('C', TNyxPascalConstantOptions.Defaults);
  LRejected := False;
  try
    LResult := LPlan.Evaluate(LSite, TNyxPascalConstantOptions.Defaults.UpToDepth(2));
  except
    on ENyxPascalConstantLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Nested declaration evaluation shares one depth ceiling');
  LConditional := TNyxPascalConstantOptions.Defaults.InDialect(pcdFPCConditional);
  LSite := Site('unit OtherCompiler; interface implementation end.',
    NewNyxPascalCompilerContext(ppcPas2JS, TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi));
  LResult := PlanNyxPascalConstant('1=1', LConditional).Evaluate(LSite, LConditional);
  Check((LResult.Issue = pbiCompilerContext) and (LResult.Value.Kind = pvvUnknown),
    'FPC conditional semantics cannot be attributed to another compiler family');
  LRejected := False;
  try
    LPlan.Evaluate(LSite, LConditional);
  except
    on ENyxPascalBinding do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Evaluation cannot reinterpret an already parsed plan in another dialect');
end;

procedure OrderedConditions;
const
  CSource: TNyxText = 'unit Ordered; interface const Seed=7; Answer=Seed*6; ' +
    'type TBand=-2..42; var ErrorCode:Word; ' +
    '{$if High(TBand)=Answer}{$define RANGE_OK}{$endif}' +
    '{$if High(ErrorCode)=65535}{$define PRIMITIVE_OK}{$endif}' +
    '{$if 9007199254740993+2=9007199254740995}{$define EXACT_OK}{$endif}' +
    'implementation end.';
var
  LFiles: INyxPascalFileCatalog;
  LResult: INyxPascalSemanticTranslation;
  LContext: INyxPascalCompilerContext;
  LWorld: INyxPascalSemanticWorld;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('ordered.pas'), CSource);
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults);
  LResult := NewNyxPascalSemanticTranslation(LFiles, NyxPascalPhysicalFile('ordered.pas'), Facts,
    LWorld, TNyxPascalTranslationOptions.Defaults, TNyxPascalSyntaxOptions.Defaults,
    TNyxPascalConditionalOptions.Defaults);
  Check(LResult.Source.Resolved and not LResult.Pending and (LResult.Answers = 3),
    'Ordinary ordered translation consumes declaration-bound and wide exact constant conditions');
  LContext := LResult.Source.State(LResult.Source.StateCount - 1);
  Check((LContext.Symbol(NyxPascalDefine('RANGE_OK')).IsDefined = ppkTrue) and
    (LContext.Symbol(NyxPascalDefine('PRIMITIVE_OK')).IsDefined = ppkTrue) and
    (LContext.Symbol(NyxPascalDefine('EXACT_OK')).IsDefined = ppkTrue),
    'Exact known conditions replay at their original sites and publish ordinary branch state');
  Check(LResult.Source.Files.TextAt(0) = CSource, 'Condition refinement preserves every authored scalar');
  LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('low.pas'),
    'unit UnsupportedLow; interface type TBand=-2..42; ' +
    '{$if Low(TBand)=-2}{$define MUST_NOT_ADMIT}{$endif}implementation end.');
  LResult := NewNyxPascalSemanticTranslation(LFiles, NyxPascalPhysicalFile('low.pas'), Facts,
    LWorld, TNyxPascalTranslationOptions.Defaults, TNyxPascalSyntaxOptions.Defaults,
    TNyxPascalConditionalOptions.Defaults);
  Check(LResult.Pending and not LResult.Source.Resolved and (LResult.Answers = 0),
    'Ordinary refinement retains unsupported conditional dialects without publishing their branch');
end;

{$ifndef PAS2JS}
procedure WholeCompiledCompanion(const ARepo: TNyxText);
var
  LCapture: INyxPascalDirectoryCapture;
  LView: INyxPascalTranslatedSyntax;
  LWorld: INyxPascalSemanticWorld;
  LSite: INyxPascalBindingSite;
  LSemantic: INyxPascalSemanticTranslation;
begin
  Check(NativeConstantMeaning(StrToInt('45')) = 42,
    'The whole authored companion executes its compiler-evaluated values');
  LCapture := CaptureNyxPascalSourceDirectory(ARepo + '/tests', NewNyxPascalFileCatalog(pfcExact),
    NewNyxPascalModuleCatalog, TNyxPascalDirectoryOptions.Defaults.OnlyFile('constant.meaning.fixture.pas'));
  LView := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LCapture.Files,
    LCapture.Modules.Entry(0).Root, Facts, TNyxPascalTranslationOptions.Defaults),
    TNyxPascalSyntaxOptions.Defaults);
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LView);
  LSite := LWorld.At(0, 0, LView.Syntax.Lexical.Scalars);
  Number(LSite, 'Answer', '42');
  Number(LSite, 'LargeExact', '9007199254740993');
  Number(LSite, 'RangeWidth', '45');
  Number(LSite, 'LastOrdinal', '9');
  Number(LSite, 'NegativeQuotient', '-6');
  Number(LSite, 'NegativeRemainder', '-1');
  Check(NativeShiftMeaning, 'The exact whole object/record companion executes its compiler-folded shifts');
  LCapture := CaptureNyxPascalSourceDirectory(ARepo + '/tests', NewNyxPascalFileCatalog(pfcExact),
    NewNyxPascalModuleCatalog, TNyxPascalDirectoryOptions.Defaults.OnlyFile('shift.meaning.fixture.pas'));
  LSemantic := NewNyxPascalSemanticTranslation(LCapture.Files, LCapture.Modules.Entry(0).Root,
    Facts, NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults),
    TNyxPascalTranslationOptions.Defaults, TNyxPascalSyntaxOptions.Defaults,
    TNyxPascalConditionalOptions.Defaults);
  WriteLn('whole shift companion / answers=', LSemantic.Answers, ' pending=', LSemantic.Pending,
    ' issue=', Ord(LSemantic.BindingIssue), ' ', LSemantic.RelatedName);
  Check(LSemantic.Source.Resolved and not LSemantic.Pending and (LSemantic.Answers = 6),
    'Unfinished object/record prefixes resolve local shifts without future declarations or synthetic closings');
  LView := NewNyxPascalTranslatedSyntax(LSemantic.Source, TNyxPascalSyntaxOptions.Defaults);
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LView);
  LSite := LWorld.At(0, 0, LView.Syntax.Lexical.Scalars);
  Number(LSite, 'TShiftedObject.Quantum', '32');
  Number(LSite, 'TShiftedRecord.Quantum', '8');
  Number(LSite, 'TShiftedObject.Wide', '18014398509481986');
  Number(LSite, 'Wrapped', '-9223372036854775808');
  Number(LSite, 'ZeroFilled', '9223372036854775807');
  Check(NativeMaskMeaning, 'The whole FPC companion independently executes signed and unsigned source masks');
  LCapture := CaptureNyxPascalSourceDirectory(ARepo + '/tests', NewNyxPascalFileCatalog(pfcExact),
    NewNyxPascalModuleCatalog, TNyxPascalDirectoryOptions.Defaults.OnlyFile('mask.meaning.fixture.pas'));
  LSemantic := NewNyxPascalSemanticTranslation(LCapture.Files, LCapture.Modules.Entry(0).Root,
    Facts, NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults),
    TNyxPascalTranslationOptions.Defaults, TNyxPascalSyntaxOptions.Defaults,
    TNyxPascalConditionalOptions.Defaults);
  Check(LSemantic.Source.Resolved and not LSemantic.Pending and (LSemantic.Answers = 6),
    'Unfinished object/record masks and source radix values resolve from exact preceding declarations');
  LView := NewNyxPascalTranslatedSyntax(LSemantic.Source, TNyxPascalSyntaxOptions.Defaults);
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LView);
  LSite := LWorld.At(0, 0, LView.Syntax.Lexical.Scalars);
  Number(LSite, 'TMaskedObject.Maximum', '1047968');
  Number(LSite, 'TMaskedRecord.Maximum', '48');
  Number(LSite, 'Wide', '9007199254740992');
  Number(LSite, 'Pattern', '-9223372036854775807');
  Number(LSite, 'UnsignedMask', '18446744073709551615');
  Number(LSite, 'DecimalPattern', '9223372036854775809');
end;
{$endif}

begin
  try
    ValuesAndScopes;
    ShiftContracts;
    IntegerMasks;
    Refusals;
    OrderedConditions;
    {$ifndef PAS2JS}
    Check(ParamCount = 1, 'Supply the repository root for the whole original companion');
    WholeCompiledCompanion(TNyxText(ParamStr(1)));
    {$endif}
    WriteLn('PASS ', GChecks, ' exact source constant and ordinal checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
