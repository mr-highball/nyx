{ Source-owned IEEE expressions retain compiler-node checking chronology and
  detached value proofs. Native whole-source cases independently qualify types,
  special values and relations; portable queries never execute authored code. }
program nyx_pascal_ieee_binding_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses SysUtils, Classes, nyx.text, nyx.editing, nyx.scheduler, nyx.pascal.work,
  nyx.pascal.reals, nyx.pascal.ieee, nyx.pascal.context, nyx.pascal.files,
  nyx.pascal.translation, nyx.pascal.lexing, nyx.pascal.syntax, nyx.pascal.syntax.translated,
  nyx.pascal.binding, nyx.pascal.conditions, nyx.pascal.values
  {$ifndef PAS2JS}, ieee.binding.fixture{$endif};

{$ifndef PAS2JS}
type
  { Immutable world/site inputs are shared; each reader owns its query and
    cancellation/work budget. Only an error string returns after joining. }
  TBindingIEEEReader = class(TThread)
  public
    World: INyxPascalSemanticWorld;
    Site: INyxPascalBindingSite;
    Window: TNyxPascalSpan;
    ExpectedKind: TNyxPascalIEEEKind;
    ErrorText: String;
    procedure Execute; override;
  end;
{$endif}

var GChecks: Integer; GInclude: TNyxText;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then raise Exception.Create(AReason);
end;

function Facts(APrecision: TNyxPascalBinaryPrecision = pbpX87Extended80): INyxPascalCompilerContext;
begin
  Result := NyxPascalWithConfiguredFPU(NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3,3,1),ppmDelphi).CompleteDefines(True).WithOption('H',ppkTrue)
    .WithSymbol(NyxPascalDefine('CPUI386'),ppdDefined)
    .WithSymbol(NyxPascalDefine('FPUX87'),ppdDefined),NyxPascalFPU('X87'));
  { This explicit pristine test fact qualifies the native fixture's ABI guard.
    It is not retagged source-local data or a new native preparation probe. }
  Result := NyxPascalWithCapturedSize(Result,NyxPascalSizeReference('Extended'),
    NyxPascalSizeFact(pssKnown,ppmDelphi,ppkTrue,10));

  if APrecision <> pbpUnknown then
    Result := NyxPascalWithRealParser(Result,TNyxPascalRealParserPolicy.NearestEven(APrecision));
end;

function Root(const AOmit: TNyxText = ''; ACpu: Boolean = True): INyxPascalCompilerContext;
const CNames: array[0..10] of TNyxText = ('Single','Double','Extended','Boolean','ShortInt','Byte','SmallInt','LongInt','LongWord','Int64','QWord');
  CSizes: array[0..10] of Integer = (4,8,10,1,1,1,2,4,4,8,8);
var LAt: Integer;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,TNyxPascalCompilerVersion.Create(3,3,1),
    ppmFPC,pcsoSystemBootstrap).CompleteDefines(True).WithOption('H',ppkFalse);

  if ACpu then Result := Result.WithSymbol(NyxPascalDefine('CPUI386'),ppdDefined)
    .WithSymbol(NyxPascalDefine('CPUINT32'),ppdDefined);
  for LAt := 0 to High(CNames) do
  begin

    if NyxPascalASCIIKey(CNames[LAt]) = NyxPascalASCIIKey(AOmit) then Continue;
    Result := NyxPascalWithCapturedSize(Result,NyxPascalSizeReference(CNames[LAt]),
      NyxPascalSizeFact(pssKnown,ppmFPC,ppkFalse,CSizes[LAt]));
  end;
end;

function View(const AName, AText: TNyxText;
  const AContext: INyxPascalCompilerContext): INyxPascalTranslatedSyntax;
begin
  Result := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile(AName),AText),NyxPascalPhysicalFile(AName),AContext,
    TNyxPascalTranslationOptions.Defaults),TNyxPascalSyntaxOptions.Defaults);
end;

function WorldFor(const ASource: TNyxText; const AProfile, ARoot: INyxPascalCompilerContext;
  ASteps: Integer = 1000000): INyxPascalSemanticWorld;
var LContext: INyxPascalCompilerContext; LBase: INyxPascalSemanticWorld;
  LSystem, LObjPas, LTarget, LTypeInfo, LMath, LSysUtils: INyxPascalTranslatedSyntax;
  LSemantic: INyxPascalSemanticTranslation; LOptions: TNyxPascalBindingOptions;
begin
  LContext := NyxPascalWithBootstrapContext(AProfile,ARoot);
  LOptions := TNyxPascalBindingOptions.Defaults.WithImplicitScopes(pispCompilerUnits).UpToSteps(ASteps);
  LSystem := View('system.pas','unit System;interface implementation end.',LContext);
  LObjPas := View('objpas.pas','unit ObjPas;interface implementation end.',LContext);
  LTypeInfo := View('typinfo.pas','unit TypInfo;interface implementation end.',LContext);
  LMath := View('math.pas','unit Math;interface implementation end.',LContext);
  LSysUtils := View('sysutils.pas','unit SysUtils;interface implementation end.',LContext);
  LBase := NewNyxPascalSemanticWorld(LOptions).WithModule(LSystem).WithModule(LObjPas)
    .WithModule(LTypeInfo).WithModule(LMath).WithModule(LSysUtils);
  { Resolve the original ABI guard through the existing ordered source/constant
    owner. A plain translator cannot give SIZEOF semantic authority by itself. }
  LSemantic := NewNyxPascalSemanticTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('source.pas'),ASource).WithFile(NyxPascalPhysicalFile('ieee.binding.cases.inc'),GInclude),NyxPascalPhysicalFile('source.pas'),LContext,LBase,
    TNyxPascalTranslationOptions.Defaults,TNyxPascalSyntaxOptions.Defaults,TNyxPascalConditionalOptions.Defaults);
  LTarget := NewNyxPascalTranslatedSyntax(LSemantic.Source,TNyxPascalSyntaxOptions.Defaults);
  Result := NewNyxPascalSemanticWorld(LOptions).WithModule(LSystem).WithModule(LObjPas).WithModule(LTarget)
    .WithModule(LTypeInfo).WithModule(LMath).WithModule(LSysUtils);
end;

function ExpressionSite(const AWorld: INyxPascalSemanticWorld; const ASource, AMarker,
  AExpression: TNyxText; out AWindow: TNyxPascalSpan): INyxPascalBindingSite;
var LAt, LStart, LFinish, LPhysicalStart, LPhysicalFinish, LCaret: Integer;
  LView: INyxPascalTranslatedSyntax; LOrigin: TNyxPascalSyntaxOrigin;
begin
  LView := AWorld.ModuleAt(2); LAt := Pos(AMarker,ASource);

  if not LView.Source.Resolved then
  begin
    WriteLn('literal source diagnostics / count=',LView.Source.DiagnosticCount);
    for LStart := 0 to LView.Source.DiagnosticCount-1 do
    begin

      if LStart = 3 then Break;
      WriteLn(String(LView.Source.Diagnostic(LStart).MessageText));
    end;
  end;
  Check(LAt > 0,'The complete original source retains its unique marker');
  Inc(LAt,Length(AMarker));
  while (LAt <= Length(ASource)) and (ASource[LAt] in [#9,#10,#13,' ']) do Inc(LAt);
  Check(Copy(ASource,LAt,Length(AExpression)) = AExpression,'The original literal/expression spelling stays exact');
  LPhysicalStart := NyxTextScalarCount(Copy(ASource,1,LAt-1));
  LPhysicalFinish := LPhysicalStart+NyxTextScalarCount(AExpression);
  LStart := -1; LFinish := -1; LCaret := -1;
  for LAt := 0 to LView.OriginCount-1 do
  begin
    LOrigin := LView.Origin(LAt);

    if (LOrigin.Occurrence <> 0) or (LOrigin.PhysicalSpan.Start < LPhysicalStart) then Continue;

    if LOrigin.PhysicalSpan.Finish <= LPhysicalFinish then
    begin

      if LStart < 0 then LStart := LOrigin.VirtualSpan.Start;
      LFinish := LOrigin.VirtualSpan.Finish;
    end
    else
    begin LCaret := LOrigin.VirtualSpan.Start; Break; end;
  end;
  Check((LStart >= 0) and (LFinish > LStart) and (LCaret >= LFinish),
    'The complete code-token union and following captured site own the query');
  AWindow := NyxPascalSpan(LStart,LFinish-LStart);
  Result := AWorld.At(2,LView.Syntax.ScopeAt(LStart),LCaret);
end;

function Query(const ASource, AMarker, AExpression: TNyxText;
  const AProfile, ARoot: INyxPascalCompilerContext): INyxPascalResolution;
var LWorld: INyxPascalSemanticWorld; LSite: INyxPascalBindingSite;
  LWindow: TNyxPascalSpan; LQueries: INyxPascalExpressionQueries; LAccess: TNyxPascalMemberAccess;
begin
  LWorld := WorldFor(ASource,AProfile,ARoot);
  LSite := ExpressionSite(LWorld,ASource,AMarker,AExpression,LWindow);
  Supports(LWorld,INyxPascalExpressionQueries,LQueries);
  Result := LQueries.ExpressionType(LSite,LWindow,LAccess);
end;

function Companion(const AExpression: TNyxText; const ASettings: TNyxText = '{$minfpconstprec 32}'): TNyxText;
begin
  Result := 'unit U;{$mode delphi}{$modeswitch implicitfunctionspecialization+}'+ASettings+
    'interface function Keep<T>(const AValue:T):T;implementation procedure Run;begin Observe('+
    AExpression+');end;end.';
end;

function Profile: INyxPascalCompilerContext;
begin
  Result := NyxPascalWithRealEvaluation(Facts.WithSymbol(NyxPascalDefine('CPUINT32'),ppdDefined),TNyxPascalRealEvaluationPolicy.NearestEven(pbpX87Extended80));
  Result := NyxPascalWithCapturedSize(Result,NyxPascalSizeReference('LongInt'),NyxPascalSizeFact(pssKnown,ppmDelphi,ppkTrue,4));
  Result := NyxPascalWithCapturedSize(Result,NyxPascalSizeReference('Int64'),NyxPascalSizeFact(pssKnown,ppmDelphi,ppkTrue,8));
  Result := NyxPascalWithCapturedSize(Result,NyxPascalSizeReference('QWord'),NyxPascalSizeFact(pssKnown,ppmDelphi,ppkTrue,8));
end;

procedure ControlledContract;
var LAnswer: INyxPascalResolution; LIEEE: INyxPascalIEEEConstant;
  LReal: INyxPascalRealConstant; LScalar: INyxPascalConstantExpression;
  LSource: TNyxText;

  procedure ValueCase(const AExpression: TNyxText; AKind: TNyxPascalIEEEKind;
    ANegative: Boolean = False; const AChecking: TNyxText = 'OFF');
  begin
    LSource := Companion(AExpression,'{$minfpconstprec 64}{$IEEEERRORS '+AChecking+'}');
    LAnswer := Query(LSource,'Observe(',AExpression,Profile,Root);
    Check((LAnswer.Status = pbsBound) and Supports(LAnswer.BoundType,INyxPascalIEEEConstant,LIEEE),
      'An admitted expression supplies a typed IEEE proof: '+String(AExpression));
    Check(LIEEE.IEEEValue.Kind = AKind,'The admitted special-value kind stays exact');

    if AKind <> pifNaN then
      Check(LIEEE.IEEEValue.Negative = ANegative,'An admitted sign survives folding and conversion');
    Check(Supports(LAnswer.BoundType,INyxPascalRealConstant,LReal) = (AKind = pifFinite),
      'The old finite-only capability never supplies infinity or NaN');
  end;

  procedure Refusal(const AExpression, AChecking: TNyxText; AIssue: TNyxPascalBindingIssue);
  begin
    LSource := Companion(AExpression,'{$minfpconstprec 64}'+AChecking);
    LAnswer := Query(LSource,'Observe(',AExpression,Profile,Root);
    Check((LAnswer.Status <> pbsBound) and (LAnswer.Issue = AIssue),
      'The exact checking context refuses this compiler-node outcome: '+String(AExpression));
  end;

begin
  ValueCase('1.0 / 0.0',pifInfinity);
  ValueCase('-1.0 / 0.0',pifInfinity,True);
  ValueCase('0.0 / 0.0',pifNaN);
  ValueCase('Single(1.0e300)',pifInfinity);
  ValueCase('Single(-1.0e300)',pifInfinity,True,'ON');
  ValueCase('Single(-1.0e-300)',pifFinite,True,'ON');
  Refusal('1.0 / 0.0','{$IEEEERRORS ON}',pbiUnsupportedType);
  Refusal('0.0 / 0.0','{$IEEEERRORS ON}',pbiUnsupportedType);
  Refusal('Single(1.0e300)','{$IEEEERRORS ON}',pbiUnsupportedType);
  Refusal('1.0 / 0.0','',pbiCompilerContext);
  Refusal('Single(1.0e300)','',pbiCompilerContext);
  Refusal('1.0 / 0.0 {$IEEEERRORS OFF}','{$IEEEERRORS ON}',pbiUnsupportedType);
  Refusal('1.0 / {$IEEEERRORS ON}0.0','{$IEEEERRORS OFF}',pbiUnsupportedType);
  Refusal('Single(1.0 / 0.0,2.0)','{$IEEEERRORS OFF}',pbiUnsupportedType);
  ValueCase('1.0 / {$IEEEERRORS OFF}0.0',pifInfinity,False,'ON');
  ValueCase('1.0 / 0.0 {$IEEEERRORS OFF}{$ifopt H+}{$endif}',pifInfinity,False,'ON');
  ValueCase('1.0 / 0.0 {$IEEEERRORS OFF}{$ifopt H-}{$endif}',pifInfinity,False,'ON');
  ValueCase('1.0 / 0.0 {$IEEEERRORS ON}',pifInfinity);

  LSource := 'unit U;{$mode delphi}{$minfpconstprec 64}{$IEEEERRORS OFF}interface '+
    'const PInf=1.0/0.0;NInf=-1.0/0.0;QNaN=0.0/0.0;'+
    '{$IEEEERRORS ON}implementation procedure Run;begin Observe(-NInf);end;end.';
  LAnswer := Query(LSource,'Observe(','-NInf',Profile,Root);
  Check((LAnswer.Status = pbsBound) and Supports(LAnswer.BoundType,INyxPascalIEEEConstant,LIEEE) and
    (LIEEE.IEEEValue.Kind = pifInfinity) and not LIEEE.IEEEValue.Negative,
    'Unary negation reuses the admitted negative node without inventing a second range check');
  LSource := StringReplace(String(LSource),'Observe(-NInf)','Observe(PInf)',[]);
  LAnswer := Query(LSource,'Observe(','PInf',Profile,Root);
  Check((LAnswer.Status <> pbsBound) and (LAnswer.Issue = pbiUnsupportedType),
    'A positive-infinity reference has its own checked use-site node');

  LSource := Companion('Keep(1.0 / 0.0)','{$minfpconstprec 64}{$IEEEERRORS OFF}');
  LAnswer := Query(LSource,'Observe(','Keep(1.0 / 0.0)',Profile,Root);
  Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType.Builtin = pbbExtended) and
    not Supports(LAnswer.BoundType,INyxPascalIEEEConstant,LIEEE) and
    not Supports(LAnswer.BoundType,INyxPascalRealConstant,LReal) and
    not Supports(LAnswer.BoundType,INyxPascalConstantExpression,LScalar),
    'Generic return typing consumes nominal identity without inheriting an argument value');
end;

procedure LiteralRangeContract;
var LSource: TNyxText; LAnswer: INyxPascalResolution;
  LIEEE: INyxPascalIEEEConstant; LSourceLiteral: INyxPascalIEEERealLiteral;
  LFiniteLiteral: INyxPascalRealLiteral; LFinite: INyxPascalRealConstant;
  LLiteral: INyxPascalLiteralType;

  procedure Accepted(const AExpression, ASettings: TNyxText;
    AKind: TNyxPascalIEEEKind; ANegative: Boolean; ABuiltin: TNyxPascalBuiltin);
  begin
    LSource := Companion(AExpression,'{$minfpconstprec 32}'+ASettings);
    LAnswer := Query(LSource,'Observe(',AExpression,Profile,Root);
    Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType.Builtin = ABuiltin) and
      Supports(LAnswer.BoundType,INyxPascalIEEEConstant,LIEEE),
      'The qualified source literal retains compiler type and admitted value: '+String(AExpression));
    Check((LIEEE.IEEEValue.Kind = AKind) and (LIEEE.IEEEValue.Negative = ANegative),
      'Literal overflow/underflow preserves the actual native kind and sign');
    Check(Supports(LAnswer.BoundType,INyxPascalRealLiteral,LFiniteLiteral) = (AKind = pifFinite),
      'The original finite literal capability cannot advertise an infinity');
    Check(Supports(LAnswer.BoundType,INyxPascalRealConstant,LFinite) = (AKind = pifFinite),
      'The original finite constant capability cannot advertise an infinity');
  end;

  procedure Refused(const AExpression, ASettings: TNyxText; AIssue: TNyxPascalBindingIssue);
  begin
    LSource := Companion(AExpression,'{$minfpconstprec 32}'+ASettings);
    LAnswer := Query(LSource,'Observe(',AExpression,Profile,Root);
    Check((LAnswer.Status <> pbsBound) and (LAnswer.Issue = AIssue),
      'Literal construction retains unsigned-node and pending-switch checks: '+String(AExpression));
  end;

begin
  Accepted('1.0e9999','{$IEEEERRORS OFF}',pifInfinity,False,pbbSingle);
  Check(Supports(LAnswer.BoundType,INyxPascalIEEERealLiteral,LSourceLiteral) and
    (LSourceLiteral.SourceValue.Spelling = '1.0e9999') and
    not LSourceLiteral.SourceValue.IsZero and
    (LSourceLiteral.ParsedIEEEValue.Kind = pifInfinity) and
    (LSourceLiteral.ParsedIEEEValue.Precision = pbpX87Extended80),
    'Exact source mathematics stays distinct from the admitted parser infinity');
  Check(Supports(LAnswer.BoundType,INyxPascalLiteralType,LLiteral) and
    (LLiteral.LiteralKind = plkReal) and (LLiteral.Value.Kind = pvvUnknown),
    'The ordinary literal contract supplies no invented scalar value');
  Accepted('-1.0e9999','{$IEEEERRORS OFF}',pifInfinity,True,pbbSingle);
  Accepted('1.0e9999','{$IEEEERRORS OFF}{$minfpconstprec 64}',pifInfinity,False,pbbDouble);
  Accepted('1.0e-9999','{$IEEEERRORS ON}',pifFinite,False,pbbSingle);
  Accepted('-1.0e-9999','{$IEEEERRORS ON}',pifFinite,True,pbbSingle);
  Check(Supports(LAnswer.BoundType,INyxPascalIEEERealLiteral,LSourceLiteral) and
    LSourceLiteral.SourceValue.Negative and not LSourceLiteral.SourceValue.IsZero and
    LSourceLiteral.ParsedIEEEValue.IsZero and
    LSourceLiteral.ParsedIEEEValue.FiniteValue.Inexact,
    'A signed inexact parser zero retains its nonzero exact source separately');
  Accepted('-1.0e-9999','{$IEEEERRORS ON}{$minfpconstprec 64}',pifFinite,True,pbbDouble);
  Accepted('1.0e9999 {$IEEEERRORS ON}','{$IEEEERRORS OFF}',pifInfinity,False,pbbSingle);
  Accepted('{$IEEEERRORS OFF}(1.0e9999)','{$IEEEERRORS ON}',pifInfinity,False,pbbSingle);
  Accepted('{$IEEEERRORS OFF}-1.0e9999','{$IEEEERRORS ON}',pifInfinity,True,pbbSingle);
  Accepted('- {$IEEEERRORS ON}1.0e9999','{$IEEEERRORS OFF}',pifInfinity,True,pbbSingle);
  Accepted('{$IEEEERRORS OFF}{$ifopt H+}{$endif}1.0e9999','{$IEEEERRORS ON}',pifInfinity,False,pbbSingle);
  Refused('1.0e9999','',pbiCompilerContext);
  Refused('1.0e9999','{$IEEEERRORS ON}',pbiUnsupportedType);
  Refused('-1.0e9999','{$IEEEERRORS ON}',pbiUnsupportedType);
  Refused('{$IEEEERRORS OFF}1.0e9999','{$IEEEERRORS ON}',pbiUnsupportedType);
  Refused('- {$IEEEERRORS OFF}1.0e9999','{$IEEEERRORS ON}',pbiUnsupportedType);
  Refused('{$IEEEERRORS ON}-1.0e9999','{$IEEEERRORS OFF}',pbiUnsupportedType);
  Refused('Double(1.0e9999 {$IEEEERRORS ON})','{$IEEEERRORS OFF}',pbiUnsupportedType);
  Accepted('Double(-1.0e9999 {$IEEEERRORS ON})','{$IEEEERRORS OFF}',pifInfinity,True,pbbDouble);
  Refused('1.'+TNyxText(StringOfChar('0',254))+'1','{$IEEEERRORS OFF}',pbiUnsupportedType);
  LSource := Companion('Keep(1.0e9999)','{$minfpconstprec 32}{$IEEEERRORS OFF}');
  LAnswer := Query(LSource,'Observe(','Keep(1.0e9999)',Profile,Root);
  Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType.Builtin = pbbSingle) and
    not Supports(LAnswer.BoundType,INyxPascalIEEEConstant,LIEEE) and
    not Supports(LAnswer.BoundType,INyxPascalIEEERealLiteral,LSourceLiteral),
    'A generic result inherits neither an overflowing argument value nor its source proof');
end;

procedure LiteralRangeWorkContract;
var LSource, LExpression: TNyxText; LWorld: INyxPascalSemanticWorld;
  LSite: INyxPascalBindingSite; LQueries: INyxPascalExpressionQueries;
  LWindow: TNyxPascalSpan; LAccess: TNyxPascalMemberAccess;
  LAnswer: INyxPascalResolution; LBudget: INyxPascalAnalysisBudget;
  LSourceLiteral: INyxPascalIEEERealLiteral; LRetained: TNyxPascalIEEEValue;
  LAt, LPass, LFit: Integer; LExpected: TNyxPascalIEEEKind;
begin
  for LPass := 0 to 1 do
  begin
    LExpression := '1.0e9999'; LExpected := pifInfinity;

    if LPass = 1 then begin LExpression := '-1.0e-9999'; LExpected := pifFinite; end;
    LSource := Companion(LExpression,'{$minfpconstprec 32}{$IEEEERRORS OFF}');
    LWorld := WorldFor(LSource,Profile,Root);
    LSite := ExpressionSite(LWorld,LSource,'Observe(',LExpression,LWindow);
    Supports(LWorld,INyxPascalExpressionQueries,LQueries); LFit := -1;
    for LAt := 1 to 20000 do
    begin
      LAnswer := nil; LBudget := NewNyxPascalAnalysisBudget(LAt);
      try
        try
          LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess,LBudget);
          Check((LAnswer.Status = pbsBound) and
            Supports(LAnswer.BoundType,INyxPascalIEEERealLiteral,LSourceLiteral),
            'The first sufficient allowance publishes the complete source-literal proof');
          LRetained := LSourceLiteral.ParsedIEEEValue; LFit := LAt;
        except
          on ENyxPascalAnalysisWorkLimit do Check((LAnswer = nil) and (LBudget.Receipt.Total <= LAt),
            'Every smaller allowance refuses atomically within its unchanged parent budget');
        end;
      finally LBudget.Retire; end;

      if LFit > 0 then Break;
    end;
    Check((LFit > 1) and (LRetained.Kind = LExpected) and
      (LRetained.Negative = (LPass = 1)),
      'Retired work budgets leave truthful detached literal values');
    WriteLn('real literal range work / ',String(LExpression),' first fit=',LFit,
      ' every smaller allowance refused');
  end;
end;

procedure WorkContract;
var LSource: TNyxText; LWorld: INyxPascalSemanticWorld; LSite: INyxPascalBindingSite;
  LWindow: TNyxPascalSpan; LQueries: INyxPascalExpressionQueries; LAccess: TNyxPascalMemberAccess;
  LAnswer: INyxPascalResolution; LBudget: INyxPascalAnalysisBudget;
  LAt, LFit: Integer; LRejected: Boolean; LCancel: INyxCancellationScope;
  LIEEE: INyxPascalIEEEConstant; LRetained: TNyxPascalIEEEValue;
begin
  LSource := Companion('Single(1.0 / 0.0) * 0.0','{$minfpconstprec 64}{$IEEEERRORS OFF}');
  LWorld := WorldFor(LSource,Profile,Root);
  LSite := ExpressionSite(LWorld,LSource,'Observe(','Single(1.0 / 0.0) * 0.0',LWindow);
  Supports(LWorld,INyxPascalExpressionQueries,LQueries); LFit := -1;
  LRetained := TNyxPascalIEEEValue.Unknown;
  for LAt := 1 to 20000 do
  begin
    LAnswer := nil; LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      try
        LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess,LBudget);
        Check((LAnswer.Status = pbsBound) and Supports(LAnswer.BoundType,INyxPascalIEEEConstant,LIEEE) and
          (LIEEE.IEEEValue.Kind = pifNaN),'The first sufficient allowance publishes the full special-value expression');
        LRetained := LIEEE.IEEEValue; LFit := LAt;
      except
        on ENyxPascalAnalysisWorkLimit do Check((LAnswer = nil) and (LBudget.Receipt.Total <= LAt),
          'Every smaller allowance refuses without partial special-value state');
      end;
    finally LBudget.Retire; end;

    if LFit > 0 then Break;
  end;
  Check((LFit > 1) and (LRetained.Kind = pifNaN),
    'Detached special-value evidence survives retirement without a mutable query lease');
  WriteLn('IEEE binding work / first fit=',LFit,' every smaller allowance refused');
  LCancel := NewNyxCancellationScope; LCancel.Cancel;
  LAnswer := nil; LRejected := False;
  try LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess,LCancel);
  except on ENyxPascalAnalysisCancelled do LRejected := True; end;
  Check(LRejected and (LAnswer = nil),'Plain cancellation publishes no IEEE expression answer');
end;

{$ifndef PAS2JS}
function Load(const APath: String): TNyxText;
var LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath,fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Result,LStream.Size);

    if Length(Result) > 0 then LStream.ReadBuffer(Result[1],Length(Result));
  finally LStream.Free; end;
end;

function TypeName(const AType: INyxPascalSemanticType): TNyxText;
var LType: INyxPascalSemanticType; LLiteral: INyxPascalLiteralType;
  LScalar: INyxPascalConstantExpression;
begin
  LType := AType;

  if Supports(LType,INyxPascalLiteralType,LLiteral) then LType := LLiteral.SpecializationType;

  if Supports(LType,INyxPascalConstantExpression,LScalar) then LType := LScalar.ResultType;

  if (LType.Kind in [pstAlias,pstDistinct]) and (LType.Definition <> nil) then
    Exit(LType.Definition.Declaration.Name);
  case LType.Builtin of
    pbbSingle: Result := 'Single'; pbbDouble: Result := 'Double'; pbbExtended: Result := 'Extended';
    pbbBoolean: Result := 'Boolean';
  else Result := '';
  end;
end;

procedure NativeSourceContract;
var LRows, LFields: TStringList; LSource, LText, LMarker, LExpression, LDescription: TNyxText;
  LRow, LAt, LFinish, LComma, LPhysical, LPhysicalFinish, LOccurrence: Integer;
  LFirst, LLast, LCaret: Integer; LOrigin: TNyxPascalSyntaxOrigin;
  LWorld: INyxPascalSemanticWorld; LView: INyxPascalTranslatedSyntax;
  LQueries: INyxPascalExpressionQueries; LAnswer: INyxPascalResolution;
  LAccess: TNyxPascalMemberAccess; LReal: INyxPascalRealConstant;
  LIEEE: INyxPascalIEEEConstant; LScalar: INyxPascalConstantExpression;
  LValue: TNyxPascalIEEEValue;
begin
  LSource := Load('tests/ieee.binding.fixture.pas'); GInclude := Load('tests/ieee.binding.cases.inc');
  LWorld := WorldFor(LSource,Profile,Root); LView := LWorld.ModuleAt(2);
  Check(LView.Source.Resolved and (LView.Source.OccurrenceCount = 5),
    'The whole native witness retains both ABI guards and four original include occurrences');
  Supports(LWorld,INyxPascalExpressionQueries,LQueries);
  LRows := TStringList.Create; LFields := TStringList.Create;
  try
    LRows.Text := String(NativeIEEEBindings);
    Check((LRows.Count = 191) and (NativeIEEEBindingCalls = 191),
      'Every original native expression executes at its authored checking/precision site');
    LFields.Delimiter := '|'; LFields.StrictDelimiter := True;
    for LRow := 0 to LRows.Count-1 do
    begin
      LFields.DelimitedText := LRows[LRow]; LText := GInclude;
      LOccurrence := LRow div 43+1;

      if LRow >= 172 then begin LOccurrence := 0; LText := LSource; end;
      LMarker := 'Emit('''+TNyxText(LFields[1])+''',';
      LAt := Pos(LMarker,LText);

      if (LRow >= 172) and (LFields[0] = 'checked') then
        LAt := Pos(LMarker,Copy(LText,Pos('procedure Checked;',LText),Length(LText)))+
          Pos('procedure Checked;',LText)-1;
      Check(LAt > 0,'The original expression retains its unique source marker');
      LComma := 0;
      while LComma < 2 do
      begin

        if LText[LAt] = ',' then Inc(LComma);
        Inc(LAt);
      end;
      while LText[LAt] in [#9,#10,#13,' '] do Inc(LAt);
      LFinish := LAt;
      while LText[LFinish] <> ';' do Inc(LFinish);
      LExpression := Copy(LText,LAt,LFinish-LAt-1);
      LPhysical := NyxTextScalarCount(Copy(LText,1,LAt-1));
      LPhysicalFinish := LPhysical+NyxTextScalarCount(LExpression);
      LFirst := -1; LLast := -1; LCaret := -1;
      for LAt := 0 to LView.OriginCount-1 do
      begin
        LOrigin := LView.Origin(LAt);

        if (LOrigin.Occurrence <> LOccurrence) or (LOrigin.PhysicalSpan.Start < LPhysical) then Continue;

        if LOrigin.PhysicalSpan.Finish <= LPhysicalFinish then
        begin

          if LFirst < 0 then LFirst := LOrigin.VirtualSpan.Start;
          LLast := LOrigin.VirtualSpan.Finish;
        end
        else begin LCaret := LOrigin.VirtualSpan.Start; Break; end;
      end;
      Check((LFirst >= 0) and (LLast > LFirst) and (LCaret >= LLast),
        'The exact physical include occurrence owns the complete expression token union');
      LAnswer := LQueries.ExpressionType(LWorld.At(2,LView.Syntax.ScopeAt(LFirst),LCaret),
        NyxPascalSpan(LFirst,LLast-LFirst),LAccess);
      WriteLn('native IEEE binding / ',LFields[0],'/',LFields[1],' status=',Ord(LAnswer.Status),
        ' issue=',Ord(LAnswer.Issue),' related=',String(LAnswer.RelatedName));
      Check((LAnswer.Status = pbsBound) and (TypeName(LAnswer.BoundType) = TNyxText(LFields[3])),
        'The unchanged native expression retains its actual compiler-inferred type');

      if LFields[3] = 'TAmount' then
        Check((LAnswer.BoundType.Kind = pstDistinct) and (LAnswer.BoundType.Definition <> nil),
          'Special-value decoration retains its authored distinct declaration');

      if LFields[2] = '0' then
        Check(not Supports(LAnswer.BoundType,INyxPascalIEEEConstant,LIEEE) and
          not Supports(LAnswer.BoundType,INyxPascalRealConstant,LReal) and
          (not Supports(LAnswer.BoundType,INyxPascalConstantExpression,LScalar) or
          (LScalar.ConstantValue.Kind = pvvUnknown)),
          'Runtime and generic results supply no fabricated IEEE or scalar evidence')
      else if LFields[3] = 'Boolean' then
        Check(Supports(LAnswer.BoundType,INyxPascalConstantExpression,LScalar) and
          (LScalar.ConstantValue.Kind = pvvBoolean) and
          (Ord(LScalar.ConstantValue.BooleanResult) = StrToInt(LFields[5])),
          'The native IEEE relation agrees, including unordered NaN')
      else
      begin
        Check(Supports(LAnswer.BoundType,INyxPascalIEEEConstant,LIEEE),'An admitted real expression supplies IEEE evidence');
        LValue := LIEEE.IEEEValue;
        case LValue.Kind of
          pifFinite: LDescription := 'finite'; pifInfinity: LDescription := 'infinity';
          pifNaN: LDescription := 'nan';
        else LDescription := 'unknown';
        end;
        Check(LDescription = TNyxText(LFields[4]),'The represented native special-value meaning agrees');

        if LValue.Kind <> pifNaN then
          Check(Ord(LValue.Negative) = StrToInt(LFields[5]),'The actual native sign agrees, including finite signed zero');
        Check(Supports(LAnswer.BoundType,INyxPascalRealConstant,LReal) = (LValue.Kind = pifFinite),
          'The original finite-only proof remains truthful at every native source site');
      end;
    end;
    Check(NativeIEEEBindingCalls = 191,'Semantic queries never invoke the independently compiled native bodies');
  finally LFields.Free; LRows.Free; GInclude := ''; end;
end;

procedure TBindingIEEEReader.Execute;
var LQueries: INyxPascalExpressionQueries; LAnswer: INyxPascalResolution;
  LIEEE: INyxPascalIEEEConstant; LAccess: TNyxPascalMemberAccess; LBudget: INyxPascalAnalysisBudget;
begin
  LBudget := NewNyxPascalAnalysisBudget(20000);
  try
    try
      Supports(World,INyxPascalExpressionQueries,LQueries);
      LAnswer := LQueries.ExpressionType(Site,Window,LAccess,LBudget);

      if (LAnswer.Status <> pbsBound) or not Supports(LAnswer.BoundType,INyxPascalIEEEConstant,LIEEE) or
        (LIEEE.IEEEValue.Kind <> ExpectedKind) then raise Exception.Create('Concurrent IEEE expression differs');
    except on E: Exception do ErrorText := E.Message; end;
  finally LBudget.Retire; end;
end;

procedure ConcurrentContract;
var LSource: TNyxText; LWorld: INyxPascalSemanticWorld; LSite: INyxPascalBindingSite;
  LWindow: TNyxPascalSpan; LReaders: array of TBindingIEEEReader; LAt, LPass: Integer;
  LExpression: TNyxText; LKind: TNyxPascalIEEEKind;
begin
  for LPass := 0 to 1 do
  begin
    LExpression := 'Single(1.0 / 0.0) * 0.0'; LKind := pifNaN;

    if LPass = 1 then begin LExpression := '1.0e9999'; LKind := pifInfinity; end;
    LSource := Companion(LExpression,'{$minfpconstprec 64}{$IEEEERRORS OFF}');
    LWorld := WorldFor(LSource,Profile,Root);
    LSite := ExpressionSite(LWorld,LSource,'Observe(',LExpression,LWindow);
    SetLength(LReaders,64);
    try
      for LAt := 0 to High(LReaders) do
      begin
        LReaders[LAt] := TBindingIEEEReader.Create(True); LReaders[LAt].FreeOnTerminate := False;
        LReaders[LAt].World := LWorld; LReaders[LAt].Site := LSite; LReaders[LAt].Window := LWindow;
        LReaders[LAt].ExpectedKind := LKind; LReaders[LAt].Start;
      end;
      for LAt := 0 to High(LReaders) do
      begin LReaders[LAt].WaitFor; Check(LReaders[LAt].ErrorText = '',LReaders[LAt].ErrorText); end;
    finally
      for LAt := 0 to High(LReaders) do
      begin

        if LReaders[LAt] <> nil then
        begin LReaders[LAt].WaitFor; LReaders[LAt].Free; LReaders[LAt] := nil; end;
      end;
    end;
    WriteLn('IEEE binding readers / 64 joined');
  end;
end;
{$endif}

begin
  ControlledContract; LiteralRangeContract; LiteralRangeWorkContract; WorkContract;
  {$ifndef PAS2JS}NativeSourceContract; ConcurrentContract;{$endif}
  WriteLn('PASS IEEE binding ',GChecks,' checks');
end.
