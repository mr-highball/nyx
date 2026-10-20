{ Exact abstract IEEE semantics, authored checking intent and qualified cast
  underflow have separate evidence. Native values/relations are compiler-folded;
  portable arithmetic uses no host float or claimed NaN payload. }
program nyx_pascal_ieee_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses SysUtils, Classes, nyx.text, nyx.editing, nyx.scheduler, nyx.pascal.work,
  nyx.pascal.values, nyx.pascal.reals, nyx.pascal.ieee, nyx.pascal.context, nyx.pascal.files,
  nyx.pascal.translation, nyx.pascal.lexing, nyx.pascal.syntax, nyx.pascal.syntax.translated,
  nyx.pascal.binding, nyx.pascal.conditions, nyx.pascal.constants
  {$ifndef PAS2JS}, ieee.values.fixture{$endif};

{$ifndef PAS2JS}
type
  { Each reader owns its arithmetic budget/workspace and publishes only text
    after the main thread joins it. No mutable numeric work is shared. }
  TIEEEReader = class(TThread)
  public
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
    .WithFile(NyxPascalPhysicalFile('source.pas'),ASource).WithFile(NyxPascalPhysicalFile('real.coercion.cases.inc'),GInclude),NyxPascalPhysicalFile('source.pas'),LContext,LBase,
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

function Rounded(const AText: TNyxText): TNyxPascalBinaryReal;
var LSource: TNyxPascalRealValue;
begin

  if (AText <> '') and (AText[1] = '-') then
    LSource := NyxPascalRealToken(Copy(AText,2,Length(AText)-1)).Negated
  else LSource := NyxPascalRealToken(AText);
  Result := NyxPascalRoundRealToEven(LSource,pbpX87Extended80);
end;

function Number(const AText: TNyxText): TNyxPascalIEEEValue;
begin
  Result := TNyxPascalIEEEValue.Finite(Rounded(AText));
end;

{$ifndef PAS2JS}
type
  { Independent expected mathematical value or relation for one native label.
    Only detached scalar/string records participate; no interface is embedded. }
  TIEEECase = record
    LabelText: TNyxText;
    Value: TNyxPascalIEEEValue;
    IsRelation, RelationValue: Boolean;
  end;

var GCases: array of TIEEECase;

procedure Add(const ALabel: TNyxText; const AValue: TNyxPascalIEEEValue);
var LAt: Integer;
begin
  LAt := Length(GCases); SetLength(GCases,LAt+1); GCases[LAt].LabelText := ALabel; GCases[LAt].Value := AValue;
end;

procedure AddRelation(const ALabel: TNyxText; AValue: Boolean);
var LAt: Integer;
begin
  LAt := Length(GCases); SetLength(GCases,LAt+1); GCases[LAt].LabelText := ALabel;
  GCases[LAt].IsRelation := True; GCases[LAt].RelationValue := AValue;
end;
{$endif}

procedure Vectors;
var P, N, Q, LValue: TNyxPascalIEEEValue; LPrecision: TNyxPascalBinaryPrecision;
  LRejected: Boolean; LFinite: TNyxPascalBinaryReal;
begin
  for LPrecision := pbpBinary32 to pbpBinary128 do
  begin
    P := TNyxPascalIEEEValue.Infinity(LPrecision); N := P.Negated;
    Q := TNyxPascalIEEEValue.NotANumber(LPrecision);
    Check((P.Compare(N) = pieoGreater) and (N.Compare(P) = pieoLess) and
      (P.Compare(P) = pieoEqual),'Signed infinities retain exact order');
    Check((P.Compare(Number('1.0')) = pieoGreater) and (N.Compare(Number('-1.0')) = pieoLess),
      'Infinities compare beyond every finite represented value');
    Check((Q.Compare(Q) = pieoUnordered) and (Q.Compare(P) = pieoUnordered) and
      (TNyxPascalIEEEValue.Unknown.Compare(Q) = pieoUnknown),'NaN unordered and unknown remain distinct');
    Check(P.Operate(proAdd,N,LPrecision).Kind = pifNaN,'Opposite infinities add to NaN');
    Check(P.Operate(proSubtract,P,LPrecision).Kind = pifNaN,'Equal infinities subtract to NaN');
    Check(P.Operate(proMultiply,Number('0.0'),LPrecision).Kind = pifNaN,'Zero times infinity is NaN');
    Check(P.Operate(proDivide,P,LPrecision).Kind = pifNaN,'Infinity divided by infinity is NaN');
    LValue := Number('-1.0').Operate(proDivide,P,LPrecision);
    Check(LValue.IsZero and LValue.Negative,'Finite division by infinity retains signed zero');
    LValue := Number('-1.0').Operate(proDivide,Number('0.0'),LPrecision);
    Check((LValue.Kind = pifInfinity) and LValue.Negative,'Nonzero finite division by zero produces signed infinity');
    Check(Number('0.0').Operate(proDivide,Number('0.0'),LPrecision).Kind = pifNaN,'Zero over zero produces NaN');
    Check(P.RoundToEven(LPrecision).Kind = pifInfinity,'Explicit format conversion retains infinity');
    Check(Q.Negated.RoundToEven(LPrecision).Kind = pifNaN,'NaN has abstract unordered meaning across negation and conversion');
  end;
  LValue := Number('1.0e300').RoundToEven(pbpBinary32);
  Check((LValue.Kind = pifInfinity) and not LValue.Negative,'Only explicit IEEE projection interprets finite overflow as infinity');
  LFinite := Rounded('1.0e300').RoundToEven(pbpBinary32);
  Check(TNyxPascalIEEEValue.Finite(LFinite).Kind = pifUnknown,'Wrapping a finite error cannot silently admit a special value');
  Q := TNyxPascalIEEEValue.NotANumber(pbpBinary64); LRejected := False;
  try Q.Negative; except on ENyxPascalValue do LRejected := True; end;
  Check(LRejected,'NaN supplies no fabricated sign or payload');
  LRejected := False;
  try LFinite := Q.FiniteValue; except on ENyxPascalValue do LRejected := True; end;
  Check(LRejected,'NaN supplies no finite significand');
  Check(TNyxPascalIEEEValue.Infinity(pbpUnknown).Kind = pifUnknown,'Unknown precision cannot borrow a host format');
  Check(Number('-0.0').Compare(Number('0.0')) = pieoEqual,'Signed finite zeros compare equally');
  {$ifndef PAS2JS}
  P := TNyxPascalIEEEValue.Infinity(pbpBinary64); N := P.Negated; Q := TNyxPascalIEEEValue.NotANumber(pbpBinary64);
  Add('positive',P); Add('negative',N); Add('nan',Q);
  Add('add-positive',P.Operate(proAdd,Number('2.0'),pbpBinary64));
  Add('add-negative',N.Operate(proAdd,Number('2.0'),pbpBinary64));
  Add('add-opposite',P.Operate(proAdd,N,pbpBinary64));
  Add('subtract-same',P.Operate(proSubtract,P,pbpBinary64));
  Add('multiply-zero',P.Operate(proMultiply,Number('0.0'),pbpBinary64));
  Add('multiply-negative',P.Operate(proMultiply,Number('-2.0'),pbpBinary64));
  Add('multiply-two-negative',N.Operate(proMultiply,Number('-2.0'),pbpBinary64));
  Add('divide-infinities',P.Operate(proDivide,P,pbpBinary64));
  Add('divide-finite',Number('1.0').Operate(proDivide,P,pbpBinary64));
  Add('divide-negative',Number('-1.0').Operate(proDivide,P,pbpBinary64));
  Add('divide-negative-zero',Number('0.0').Operate(proDivide,N,pbpBinary64));
  Add('infinity-divide',N.Operate(proDivide,Number('2.0'),pbpBinary64));
  Add('nan-add',Q.Operate(proAdd,Number('1.0'),pbpBinary64));
  Add('cast-infinity',P.RoundToEven(pbpBinary32)); Add('cast-nan',Q.RoundToEven(pbpBinary32));
  Add('cast-overflow',Number('1.0e300').RoundToEven(pbpBinary32));
  Add('cast-underflow',Number('1.0e-300').RoundToEven(pbpBinary32));
  Add('cast-negative-underflow',Number('-1.0e-300').RoundToEven(pbpBinary32));
  AddRelation('nan-equal',Q.Compare(Q) = pieoEqual); AddRelation('nan-not-equal',Q.Compare(Q) <> pieoEqual);
  AddRelation('nan-less',Q.Compare(Number('1.0')) = pieoLess);
  AddRelation('nan-less-equal',Q.Compare(Number('1.0')) in [pieoLess,pieoEqual]);
  AddRelation('nan-greater',Q.Compare(Number('1.0')) = pieoGreater);
  AddRelation('nan-greater-equal',Q.Compare(Number('1.0')) in [pieoGreater,pieoEqual]);
  AddRelation('infinity-equal',P.Compare(P) = pieoEqual); AddRelation('infinity-less',N.Compare(P) = pieoLess);
  AddRelation('infinity-greater',P.Compare(Number('1.0')) = pieoGreater);
  {$endif}
end;

function StorageIdentity(AValue: TNyxPascalKnowledge): TNyxText;
var LContext: INyxPascalCompilerContext; LView, LSystem: INyxPascalTranslatedSyntax;
  LWorld: INyxPascalSemanticWorld; LResult: INyxPascalConstantResult;
  LIdentity: INyxPascalConstantStorageIdentity;
begin
  LContext := NyxPascalWithCompilerPolicies(Profile,
    NyxPascalCompilerPolicies(Profile).RequestIEEEErrors(AValue));
  LContext := NyxPascalWithCapturedSize(LContext,NyxPascalSizeReference('Byte'),
    NyxPascalSizeFact(pssKnown,ppmDelphi,ppkTrue,1));
  LView := View('layout.pas','unit Layout;interface {$packrecords 1}type TPacket=record B:Byte;end;implementation end.',LContext);
  LSystem := View('system.pas','unit System;interface implementation end.',LContext);
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LView).WithModule(LSystem);
  LResult := PlanNyxPascalConstant('sizeof(TPacket)',TNyxPascalConstantOptions.Defaults)
    .Evaluate(LWorld.At(0,0,LView.Syntax.Lexical.Scalars),TNyxPascalConstantOptions.Defaults);
  Check(Supports(LResult,INyxPascalConstantStorageIdentity,LIdentity),'Existing source storage receipts retain local IEEE intent');
  Result := LIdentity.StorageKey.Value;
end;

procedure PolicyContract;
var LBase, LOn, LOff: INyxPascalCompilerContext; LPolicies: TNyxPascalCompilerPolicies;
  LView: INyxPascalTranslatedSyntax; LAt: Integer; LToken: TNyxPascalTranslatedToken;
  LName: TNyxText; LOnSeen, LOffSeen, LRestored: Boolean; LAnswer: INyxPascalResolution;
  LReal: INyxPascalRealConstant; LSource, LUnknownKey, LOnKey, LOffKey: TNyxText;
begin
  LBase := Profile;
  Check(NyxPascalCompilerPolicies(LBase).IEEEErrorsRequested = ppkUnknown,'Unknown supplies no initial IEEE checking flag');
  LOn := NyxPascalWithCompilerPolicies(LBase,NyxPascalCompilerPolicies(LBase).RequestIEEEErrors(ppkTrue));
  LOff := NyxPascalWithCompilerPolicies(LBase,NyxPascalCompilerPolicies(LBase).RequestIEEEErrors(ppkFalse));
  Check(not LOn.SameFacts(LOff) and not LBase.SameFacts(LOn),'IEEE intent participates in exact context identity');
  Check(CaptureNyxPascalCompilerContext(LOff).SameFacts(LOff),'Capture retains authored IEEE intent');
  Check(NyxPascalCompilerPolicies(LOff.InMode(ppmObjFPC)).IEEEErrorsRequested = ppkFalse,
    'The inspected FPC mode handler retains this local flag rather than inventing a mode default');
  Check(NyxPascalCompilerPolicies(LOff.UnknownState).IEEEErrorsRequested = ppkUnknown,
    'Unknown effects revoke authored IEEE knowledge');
  Check(NyxPascalCompilerPolicies(RestoreNyxPascalLocalSettings(LOn,LOff)).IEEEErrorsRequested = ppkFalse,
    'Local settings restore saved IEEE intent');
  LView := View('policy.pas','unit U;{$mode delphi}interface {$IEEEERRORS OFF}var A:Integer;'+
    '{$push}{$IEEEERRORS ON}B:Integer;{$pop}C:Integer;implementation end.',LBase);
  Check(LView.Source.Resolved,'The original IEEE directive belongs to the shared source translator');
  LOnSeen := False; LOffSeen := False; LRestored := False;
  for LAt := 0 to LView.Source.Count-1 do
  begin
    LToken := LView.Source.Token(LAt);

    if LView.Source.PhysicalToken(LAt).Kind <> ptkWord then Continue;
    LName := LView.Source.Text(LAt); LPolicies := NyxPascalCompilerPolicies(LView.Source.State(LToken.State));

    if LName = 'A' then LOffSeen := LPolicies.IEEEErrorsRequested = ppkFalse;

    if LName = 'B' then LOnSeen := LPolicies.IEEEErrorsRequested = ppkTrue;

    if LName = 'C' then LRestored := LPolicies.IEEEErrorsRequested = ppkFalse;
  end;
  Check(LOffSeen and LOnSeen and LRestored,'Ordered source states retain local OFF/ON/restored OFF independently of R/Q');
  LUnknownKey := StorageIdentity(ppkUnknown); LOnKey := StorageIdentity(ppkTrue); LOffKey := StorageIdentity(ppkFalse);
  Check((LUnknownKey <> LOnKey) and (LOnKey <> LOffKey) and (LUnknownKey = StorageIdentity(ppkUnknown)),
    'Tagged IEEE identity distinguishes known requests while preserving unspecified identity');
  LView := View('inactive.pas','unit U;{$mode delphi}interface {$IEEEERRORS OFF}'+
    '{$ifdef ABSENT}{$IEEEERRORS ON}{$endif}var A:Integer;implementation end.',LBase);
  Check(LView.Source.Resolved and
    (NyxPascalCompilerPolicies(LView.Source.State(LView.Source.Occurrence(0).ExitState)).IEEEErrorsRequested = ppkFalse),
    'A known inactive branch cannot change IEEE intent');
  LView := View('malformed.pas','unit U;{$mode delphi}interface {$IEEEERRORS SOMEDAY}implementation end.',LBase);
  Check(not LView.Source.Resolved,'Unknown IEEE directive operands retain explicit source refusal');
  LSource := Companion('Single(-1.0e-300)','{$minfpconstprec 64}{$IEEEERRORS ON}');
  LAnswer := Query(LSource,'Observe(','Single(-1.0e-300)',LBase,Root);
  Check((LAnswer.Status = pbsBound) and Supports(LAnswer.BoundType,INyxPascalRealConstant,LReal) and
    LReal.RealValue.IsZero and LReal.RealValue.Negative,'Qualified real-cast underflow retains negative zero with IEEE errors enabled');
  LSource := Companion('Single(1.0e-300)','{$minfpconstprec 64}{$IEEEERRORS OFF}');
  LAnswer := Query(LSource,'Observe(','Single(1.0e-300)',LBase,Root);
  Check((LAnswer.Status = pbsBound) and Supports(LAnswer.BoundType,INyxPascalRealConstant,LReal) and
    LReal.RealValue.IsZero and not LReal.RealValue.Negative,'Qualified positive underflow remains finite zero with IEEE errors disabled');
end;

procedure WorkContract;
var LBudget: INyxPascalAnalysisBudget; LCancel: INyxCancellationScope;
  LValue, LRetained, LLeft, LRight: TNyxPascalIEEEValue;
  LAt, LFit: Integer; LRejected: Boolean;
begin
  LLeft := Number('1.0'); LRight := Number('3.0'); LFit := -1;
  for LAt := 1 to 20000 do
  begin
    LValue := TNyxPascalIEEEValue.Unknown; LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      try
        LValue := LLeft.Operate(proDivide,LRight,pbpBinary64,LBudget);
        Check(LValue.Kind = pifFinite,'The first sufficient IEEE allowance publishes a complete finite result');
        LRetained := LValue; LFit := LAt;
      except
        on ENyxPascalAnalysisWorkLimit do Check((LValue.Kind = pifUnknown) and (LBudget.Receipt.Total <= LAt),
          'Every smaller IEEE allowance refuses without a partial result');
      end;
    finally LBudget.Retire; end;

    if LFit > 0 then Break;
  end;
  Check((LFit > 1) and (LRetained.RoundToEven(pbpBinary32).Kind = pifFinite),'An IEEE value retains no retired scope');
  WriteLn('IEEE work / first fit=',LFit,' every smaller allowance refused');
  LCancel := NewNyxCancellationScope; LCancel.Cancel;
  LValue := TNyxPascalIEEEValue.Unknown; LRejected := False;
  try LValue := LLeft.Operate(proDivide,LRight,pbpBinary64,LCancel);
  except on ENyxPascalAnalysisCancelled do LRejected := True; end;
  Check(LRejected and (LValue.Kind = pifUnknown),'Ordinary cancellation publishes no IEEE result');
end;

{$ifndef PAS2JS}
procedure NativeContract;
var LRows, LFields: TStringList; LAt: Integer; LExpected, LSign: TNyxText;
begin
  LRows := TStringList.Create; LFields := TStringList.Create;
  try
    LRows.Text := String(NativeIEEEValues); LFields.Delimiter := '|'; LFields.StrictDelimiter := True;
    Check((LRows.Count = 30) and (Length(GCases) = 30),'All independent compiler values and relations are observed');
    for LAt := 0 to High(GCases) do
    begin
      LFields.DelimitedText := LRows[LAt]; LExpected := ''; LSign := '-';

      if GCases[LAt].IsRelation then
      begin LExpected := 'relation'; LSign := TNyxText(IntToStr(Ord(GCases[LAt].RelationValue))); end
      else
      begin
        case GCases[LAt].Value.Kind of
          pifFinite: LExpected := 'finite';
          pifInfinity: LExpected := 'infinity';
          pifNaN: LExpected := 'nan';
        else raise Exception.Create('An independent IEEE expected value is unknown'); end;

        if GCases[LAt].Value.Kind <> pifNaN then
        begin
          LSign := '0';

          if GCases[LAt].Value.Negative then LSign := '1';
        end;
      end;
      Check((LFields.Count = 3) and (TNyxText(LFields[0]) = GCases[LAt].LabelText) and
        (TNyxText(LFields[1]) = LExpected) and (TNyxText(LFields[2]) = LSign),
        'Actual compiler special-value/relation semantics differ: '+LRows[LAt]);
    end;
  finally LFields.Free; LRows.Free; end;
end;

procedure TIEEEReader.Execute;
var LValue: TNyxPascalIEEEValue; LBudget: INyxPascalAnalysisBudget;
begin
  LBudget := NewNyxPascalAnalysisBudget(20000);
  try
    try
      LValue := TNyxPascalIEEEValue.Infinity(pbpBinary64).Operate(proMultiply,Number('0.0'),pbpBinary64,LBudget);

      if LValue.Kind <> pifNaN then raise Exception.Create('Concurrent special-value semantics differ');
    except on E: Exception do ErrorText := E.Message; end;
  finally LBudget.Retire; end;
end;

procedure ConcurrentContract;
var LReaders: array of TIEEEReader; LAt: Integer;
begin
  SetLength(LReaders,64);
  try
    for LAt := 0 to High(LReaders) do
    begin LReaders[LAt] := TIEEEReader.Create(True); LReaders[LAt].FreeOnTerminate := False; LReaders[LAt].Start; end;
    for LAt := 0 to High(LReaders) do
    begin LReaders[LAt].WaitFor; Check(LReaders[LAt].ErrorText = '',LReaders[LAt].ErrorText); end;
  finally
    for LAt := 0 to High(LReaders) do
    begin

      if LReaders[LAt] <> nil then begin LReaders[LAt].WaitFor; LReaders[LAt].Free; end;
    end;
  end;
  WriteLn('IEEE readers / 64 joined');
end;
{$endif}

begin
  Vectors; PolicyContract; WorkContract;
  {$ifndef PAS2JS}NativeContract; ConcurrentContract;{$endif}
  WriteLn('PASS IEEE ',GChecks,' checks');
end.
