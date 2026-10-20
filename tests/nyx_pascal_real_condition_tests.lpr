{ FPC conditional mathematics is distinct from ordinary Pascal source typing.
  Complete native directives supply independent truth, while source declarations
  keep their own rounding, dependencies and admission. No authored body executes
  during semantic translation or constant-plan evaluation. }
program nyx_pascal_real_condition_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses SysUtils, Classes, nyx.text, nyx.scheduler, nyx.pascal.work,
  nyx.pascal.reals, nyx.pascal.ieee, nyx.pascal.context, nyx.pascal.files,
  nyx.pascal.translation, nyx.pascal.lexing, nyx.pascal.syntax, nyx.pascal.syntax.translated,
  nyx.pascal.binding, nyx.pascal.conditions, nyx.pascal.constants, nyx.pascal.values
  {$ifndef PAS2JS}, nyx.editing, real.condition.fixture{$endif};

{$ifndef PAS2JS}
type
  { Immutable world/site inputs are shared; each reader owns its query and
    cancellation/work budget. Only an error string returns after joining. }
  TRealConditionReader = class(TThread)
  public
    Site: INyxPascalBindingSite;
    Plan: INyxPascalConstantPlan;
    ErrorText: String;
    procedure Execute; override;
  end;
{$endif}

var GChecks: Integer; GInclude: TNyxText; GLast: INyxPascalSemanticTranslation;

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
    .WithFile(NyxPascalPhysicalFile('source.pas'),ASource).WithFile(NyxPascalPhysicalFile('real.condition.cases.inc'),GInclude),NyxPascalPhysicalFile('source.pas'),LContext,LBase,
    TNyxPascalTranslationOptions.Defaults,TNyxPascalSyntaxOptions.Defaults,TNyxPascalConditionalOptions.Defaults);
  GLast := LSemantic;
  LTarget := NewNyxPascalTranslatedSyntax(LSemantic.Source,TNyxPascalSyntaxOptions.Defaults);
  Result := NewNyxPascalSemanticWorld(LOptions).WithModule(LSystem).WithModule(LObjPas).WithModule(LTarget)
    .WithModule(LTypeInfo).WithModule(LMath).WithModule(LSysUtils);
end;

function Profile: INyxPascalCompilerContext;
begin
  Result := NyxPascalWithRealEvaluation(Facts.WithSymbol(NyxPascalDefine('CPUINT32'),ppdDefined),TNyxPascalRealEvaluationPolicy.NearestEven(pbpX87Extended80));
  Result := NyxPascalWithCapturedSize(Result,NyxPascalSizeReference('LongInt'),NyxPascalSizeFact(pssKnown,ppmDelphi,ppkTrue,4));
  Result := NyxPascalWithCapturedSize(Result,NyxPascalSizeReference('Int64'),NyxPascalSizeFact(pssKnown,ppmDelphi,ppkTrue,8));
  Result := NyxPascalWithCapturedSize(Result,NyxPascalSizeReference('QWord'),NyxPascalSizeFact(pssKnown,ppmDelphi,ppkTrue,8));
end;

function Options: TNyxPascalConstantOptions;
begin
  Result := TNyxPascalConstantOptions.Defaults.InDialect(pcdFPCConditional);
end;

function Source: TNyxText;
begin
  Result := 'unit U;{$mode delphi}{$minfpconstprec 32}{$excessprecision off}{$IEEEERRORS OFF}'+
    'interface const SingleValue=Single(0.1);SumOfSingles=SingleValue+SingleValue;'+
    'SingleAlias=SingleValue;PositiveFlag=SingleValue>0.0;PInf=1.0/0.0;QNaN=0.0/0.0;'+
    '{$IEEEERRORS ON}implementation end.';
end;

function SiteFor(const AText: TNyxText; const AProfile: INyxPascalCompilerContext): INyxPascalBindingSite;
var LWorld: INyxPascalSemanticWorld; LView: INyxPascalTranslatedSyntax;
begin
  LWorld := WorldFor(AText,AProfile,Root); LView := LWorld.ModuleAt(2);
  Result := LWorld.At(2,0,LView.Syntax.Lexical.Scalars);
end;

function Evaluate(const ASite: INyxPascalBindingSite; const AText: TNyxText): INyxPascalConstantResult;
begin
  Result := PlanNyxPascalConstant(AText,Options).Evaluate(ASite,Options);
end;

procedure ControlledContract;
var LSite: INyxPascalBindingSite; LResult: INyxPascalConstantResult;
  LIEEE: INyxPascalIEEEConstant; LAt: Integer; LFound, LAlias: Boolean;

  procedure Truth(const AText: TNyxText; AExpected: Boolean);
  begin
    LResult := Evaluate(LSite,AText);
    WriteLn('conditional value / ',String(AText),' issue=',Ord(LResult.Issue),' related=',String(LResult.RelatedName));
    Check((LResult.Issue = pbiNone) and (LResult.Value.Kind = pvvBoolean) and
      (LResult.Value.BooleanResult = AExpected),'The qualified conditional truth agrees: '+String(AText));
    Check(Supports(LResult,INyxPascalIEEEConstant,LIEEE) and (LIEEE.IEEEValue.Kind = pifUnknown),
      'A Boolean answer carries no disguised real value');
  end;

begin
  LSite := SiteFor(Source,Profile);
  Truth('0.1+0.2=0.3',True);
  Truth('16777217=16777216.0',False);
  Truth('(16777216.0+1.0)-16777216.0=1.0',True);
  Truth('SingleValue=0.1',False);
  Truth('SumOfSingles=0.2',False);
  Truth('SingleAlias=SingleValue',True);
  Truth('PositiveFlag',True);
  Truth('PInf>0.0',True);
  Truth('QNaN<>QNaN',True);
  Truth('QNaN=QNaN',False);
  Truth('1.0/0.0>0.0',True);
  Truth('0.0/0.0<>0.0/0.0',True);
  Truth('True+1.5=2.5',True);
  Truth('18446744073709551616>0.0',True);
  Truth('1.0e-9999=0.0',True);
  Truth('1.0e9999>0.0',True);
  Truth('True or Missing',True);
  Truth('False and Missing',False);
  LResult := Evaluate(LSite,'SingleAlias=SingleValue');
  LFound := False; LAlias := False;
  for LAt := 0 to LResult.DependencyCount-1 do
  begin

    if LResult.Dependency(LAt).Declaration.Name = 'SingleValue' then LFound := True;

    if LResult.Dependency(LAt).Declaration.Name = 'SingleAlias' then LAlias := True;
  end;
  Check(LFound and LAlias,'Source-owned real aliases retain their transitive declaration dependencies');
  LResult := Evaluate(LSite,'1.0/0.0');
  Check((LResult.Issue = pbiNone) and (LResult.Value.Kind = pvvUnknown) and
    Supports(LResult,INyxPascalIEEEConstant,LIEEE) and (LIEEE.IEEEValue.Kind = pifInfinity),
    'A standalone real result uses the optional existing IEEE capability');
  LSite := nil;
  Check((LIEEE.IEEEValue.Kind = pifInfinity) and not LIEEE.IEEEValue.Negative,
    'Detached conditional evidence survives caller/world retirement');
end;

procedure RefusalContract;
var LSite: INyxPascalBindingSite; LResult: INyxPascalConstantResult;
  LContext: INyxPascalCompilerContext; LIEEE: INyxPascalIEEEConstant;
  LAt: Integer; LCases: array[0..3] of TNyxText;
  LWorld: INyxPascalSemanticWorld;
begin
  LCases[0] := '1.5 and True'; LCases[1] := '1.5 div 1';
  LCases[2] := 'Ord(1.5)'; LCases[3] := 'RunAnything(1.5)';
  LSite := SiteFor(Source,Profile);
  for LAt := 0 to High(LCases) do
  begin
    LResult := Evaluate(LSite,LCases[LAt]);
    Check((LResult.Issue <> pbiNone) and (LResult.Value.Kind = pvvUnknown) and
      Supports(LResult,INyxPascalIEEEConstant,LIEEE) and (LIEEE.IEEEValue.Kind = pifUnknown),
      'Malformed or unsupported conditional meaning publishes no partial channel');
  end;
  Check(not PlanNyxPascalConstant('+1.5>0',Options).SyntaxSupported,
    'Conditional unary plus cannot borrow ordinary Pascal grammar');
  Check(not PlanNyxPascalConstant('1.5+2.5',TNyxPascalConstantOptions.Defaults).SyntaxSupported,
    'Ordinary real expressions remain source-bound through the existing binder');
  LContext := NyxPascalWithRealParser(Profile,TNyxPascalRealParserPolicy.Unknown);
  LSite := SiteFor('unit U;interface implementation end.',LContext);
  LResult := Evaluate(LSite,'1.5<2.0');
  Check((LResult.Issue = pbiCompilerContext) and (LResult.RelatedName = 'REALPARSER'),
    'A target FPU or explicit evaluator cannot invent compiler-host parsing');
  LResult := Evaluate(LSite,'False and (1.5<2.0)');
  Check((LResult.Issue = pbiNone) and not LResult.Value.BooleanResult,
    'A known short-circuited branch does not request unused parser facts');
  LContext := NyxPascalWithRealEvaluation(Profile,TNyxPascalRealEvaluationPolicy.Unknown);
  LSite := SiteFor('unit U;interface implementation end.',LContext);
  LResult := Evaluate(LSite,'1.5<2.0');
  Check((LResult.Issue = pbiCompilerContext) and (LResult.RelatedName = 'REALEVALUATION'),
    'Parsing facts do not invent host conversion/evaluation facts');
  LSite := SiteFor('unit U;{$minfpconstprec 32}{$IEEEERRORS OFF}interface const PInf=1.0/0.0;'+
    '{$IEEEERRORS ON}BadAlias=PInf;implementation end.',Profile);
  LResult := Evaluate(LSite,'BadAlias>0.0');
  Check((LResult.Issue <> pbiNone) and (LResult.Value.Kind = pvvUnknown),
    'A conditional cannot rehabilitate a rejected ordinary alias initializer');
  LSite := SiteFor('unit U;interface implementation end.',Profile);
  LResult := Evaluate(LSite,TNyxText(StringOfChar('1',256))+'.0>0.0');
  Check((LResult.Issue <> pbiNone) and (LResult.Value.Kind = pvvUnknown),
    'Long caller tokens refuse rather than silently truncating to the compiler short-string input');
  LWorld := WorldFor('unit U;interface {$if 1.5}const Wrong=1;{$endif}implementation end.',Profile,Root);
  Check(GLast.Pending and not GLast.Source.Resolved and (GLast.BindingIssue = pbiUnsupportedType),
    'A real value cannot masquerade as an accepted Boolean directive');
  Check(LWorld.ModuleAt(2).Source = GLast.Source,'A refused condition retains the ordinary source model');
  for LAt := 0 to 1 do
  begin
    LContext := NewNyxPascalCompilerContext(ppcFPC,TNyxPascalCompilerVersion.Create(3,2,2),ppmDelphi);

    if LAt = 1 then
      LContext := NewNyxPascalCompilerContext(ppcPas2JS,TNyxPascalCompilerVersion.Create(3,3,1),ppmDelphi);
    LContext := NyxPascalWithRealEvaluation(NyxPascalWithRealParser(LContext,
      TNyxPascalRealParserPolicy.NearestEven(pbpX87Extended80)),
      TNyxPascalRealEvaluationPolicy.NearestEven(pbpX87Extended80));
    LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults)
      .WithModule(View('other.pas','unit Other;interface implementation end.',LContext));
    LSite := LWorld.At(0,0,LWorld.ModuleAt(0).Syntax.Lexical.Scalars);
    LResult := Evaluate(LSite,'1.5<2.0');
    Check((LResult.Issue = pbiCompilerContext) and (LResult.Value.Kind = pvvUnknown),
      'Another compiler family/version cannot borrow this qualified preprocessor contract');
  end;
end;

procedure RangeContract;
var LPrecision: TNyxPascalBinaryPrecision; LBudget: INyxPascalAnalysisBudget;
  LValue, LExpected: TNyxPascalBinaryReal; LSource: TNyxPascalRealValue; LOrder: Integer;
begin
  for LPrecision := pbpBinary32 to pbpBinary128 do
  begin
    LBudget := NewNyxPascalAnalysisBudget(TNyxPascalConstantOptions.Defaults.MaximumSteps);
    try
      LValue := NyxPascalRoundRealToEven(NyxPascalRealToken('1.0e9999',LBudget),LPrecision,LBudget);
      Check(LValue.Status = rrsOverflow,'A proven outer decimal bound supplies finite-format overflow');
      LSource := NyxPascalRealToken('1.0e-9999',LBudget).Negated;
      LValue := NyxPascalRoundRealToEven(LSource,LPrecision,LBudget);
      Check((LValue.Status = rrsFinite) and LValue.IsZero and LValue.Negative and LValue.Inexact,
        'A proven tiny negative value retains signed inexact zero in every mathematical format');
      Check(LBudget.Receipt.Total < 1000,'Certain outer bounds avoid large power construction within the original budget');
    finally LBudget.Retire; end;
    LBudget := NewNyxPascalAnalysisBudget(TNyxPascalConstantOptions.Defaults.MaximumSteps);
    try
      LSource := NyxPascalRealToken('1'+TNyxText(StringOfChar('0',200))+'1e-200',LBudget);
      LValue := NyxPascalRoundRealToEven(LSource,LPrecision,LBudget);
      LExpected := NyxPascalRoundRealToEven(NyxPascalRealToken('10.0',LBudget),LPrecision,LBudget);
      Check(LValue.TryCompare(LExpected,LOrder,LBudget) and (LOrder = 0),
        'Decimal order includes a compensating long coefficient rather than judging its exponent alone');
    finally LBudget.Retire; end;
  end;
end;

procedure WorkContract;
var LSite: INyxPascalBindingSite; LPlan: INyxPascalConstantPlan;
  LResult: INyxPascalConstantResult; LBudget: INyxPascalAnalysisBudget;
  LAt, LFit: Integer; LCancel: INyxCancellationScope; LRejected: Boolean;
begin
  LSite := SiteFor(Source,Profile);
  LPlan := PlanNyxPascalConstant('(SingleAlias+0.1)>0.0',Options); LFit := -1;
  for LAt := 1 to 20000 do
  begin
    LResult := nil; LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      try
        LResult := LPlan.Evaluate(LSite,Options,LBudget);
        Check((LResult.Issue = pbiNone) and (LResult.Value.Kind = pvvBoolean) and LResult.Value.BooleanResult,
          'The first sufficient budget publishes the complete source/conditional result');
        LFit := LAt;
      except
        on ENyxPascalAnalysisWorkLimit do Check((LResult = nil) and (LBudget.Receipt.Total <= LAt),
          'Every smaller budget refuses atomically without either value channel');
      end;
    finally LBudget.Retire; end;

    if LFit > 0 then Break;
  end;
  Check(LFit > 1,'Real conditional work remains within the existing limits');
  WriteLn('real condition work / first fit=',LFit,' every smaller allowance refused');
  LCancel := NewNyxCancellationScope; LCancel.Cancel; LResult := nil; LRejected := False;
  try LResult := LPlan.Evaluate(LSite,Options,LCancel);
  except on ENyxPascalAnalysisCancelled do LRejected := True; end;
  Check(LRejected and (LResult = nil),'Ordinary cancellation publishes no conditional numeric result');
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

procedure NativeSourceContract;
var LSource, LText, LMarker: TNyxText; LWorld: INyxPascalSemanticWorld;
  LRows, LFields: TStringList; LRow, LOccurrence, LAt, LStart, LFinish, LBranch: Integer;
  LStarts: array[0..1] of Integer;
  LExpected, LSeen, LActive: Boolean; LToken: TNyxPascalTranslatedToken; LPhysical: TNyxPascalToken;
begin
  LSource := Load('tests/real.condition.fixture.pas');
  GInclude := Load('tests/real.condition.cases.inc');
  LWorld := WorldFor(LSource,Profile,Root);
  Check(LWorld.ModuleAt(2).Source = GLast.Source,'The ordinary world consumes the actual ordered translation');
  Check(GLast.Source.Resolved and not GLast.Pending,'Every original native directive resolves through ordered semantic prefixes');
  Check(GLast.Answers < TNyxPascalConditionalOptions.Defaults.MaximumPasses,
    'The complete original unit fits its unchanged conditional pass ceiling');
  Check((GLast.Source.OccurrenceCount = 3) and (GLast.Source.Files.TextAt(0) = LSource),
    'The complete original source and both repeated include occurrences remain exact');
  LRows := TStringList.Create; LFields := TStringList.Create;
  try
    LRows.Text := String(NativeRealConditions);
    Check((LRows.Count = 57) and (NativeRealConditionCalls = 57),
      'The independently compiled unit executes all original condition branches');
    LFields.Delimiter := '|'; LFields.StrictDelimiter := True;
    for LRow := 0 to LRows.Count-1 do
    begin
      LFields.DelimitedText := LRows[LRow]; LText := GInclude; LOccurrence := LRow div 28+1;

      if LRow = 56 then begin LText := LSource; LOccurrence := 0; end;
      LExpected := LFields[2] = '1';
      LMarker := 'Emit('''+TNyxText(LFields[1])+''',';
      LStarts[0] := Pos(LMarker,LText);
      Check(LStarts[0] > 0,'Each native result retains its original physical branch marker');
      LFinish := Pos(LMarker,Copy(LText,LStarts[0]+Length(LMarker),Length(LText)));
      Check(LFinish > 0,'Both branches stay original source, never supplied expected expressions');
      LStarts[1] := LStarts[0]+Length(LMarker)+LFinish-1;
      for LBranch := 0 to 1 do
      begin
        LStart := LStarts[LBranch]; LFinish := LStart;
        while LText[LFinish] <> ';' do Inc(LFinish);
        LStart := NyxTextScalarCount(Copy(LText,1,LStart-1));
        LFinish := NyxTextScalarCount(Copy(LText,1,LFinish)); LSeen := False;
        LActive := LExpected;

        if LBranch = 1 then LActive := not LActive;
        for LAt := 0 to GLast.Source.Count-1 do
        begin
          LToken := GLast.Source.Token(LAt); LPhysical := GLast.Source.PhysicalToken(LAt);

          if (LToken.Occurrence = LOccurrence) and (LPhysical.Span.Start >= LStart) and
            (LPhysical.Span.Finish <= LFinish) and (LPhysical.Kind = ptkWord) then
          begin
            LSeen := True;
            Check((LActive and (LToken.Activity = ppkTrue)) or
              (not LActive and (LToken.Activity = ppkFalse)),
              'Actual native truth selects one branch and rejects its exact original opposite');
          end;
        end;
        Check(LSeen,'Both native branch alternatives retain their physical source tokens');
      end;
      WriteLn('native real condition / ',LFields[0],'/',LFields[1],'=',LFields[2]);
    end;
    Check(NativeRealConditionCalls = 57,'Ordered semantic reads never execute the compiled witness bodies');
    WriteLn('real condition original / answers=',GLast.Answers,' passes=',GLast.Passes);
  finally LFields.Free; LRows.Free; GInclude := ''; end;
end;

procedure TRealConditionReader.Execute;
var LResult: INyxPascalConstantResult; LBudget: INyxPascalAnalysisBudget;
begin
  LBudget := NewNyxPascalAnalysisBudget(20000);
  try
    try
      LResult := Plan.Evaluate(Site,Options,LBudget);

      if (LResult.Issue <> pbiNone) or (LResult.Value.Kind <> pvvBoolean) or
        not LResult.Value.BooleanResult then raise Exception.Create('Concurrent conditional result differs');
    except on E: Exception do ErrorText := E.Message; end;
  finally LBudget.Retire; end;
end;

procedure ConcurrentContract;
var LSite: INyxPascalBindingSite; LPlan: INyxPascalConstantPlan;
  LReaders: array of TRealConditionReader; LAt: Integer;
begin
  LSite := SiteFor(Source,Profile); LPlan := PlanNyxPascalConstant('(SingleAlias+0.1)>0.0',Options);
  SetLength(LReaders,64);
  try
    for LAt := 0 to High(LReaders) do
    begin
      LReaders[LAt] := TRealConditionReader.Create(True); LReaders[LAt].FreeOnTerminate := False;
      LReaders[LAt].Site := LSite; LReaders[LAt].Plan := LPlan;
      LReaders[LAt].Start;
    end;
    for LAt := 0 to High(LReaders) do
    begin LReaders[LAt].WaitFor; Check(LReaders[LAt].ErrorText = '',LReaders[LAt].ErrorText); end;
  finally
    for LAt := 0 to High(LReaders) do
    begin

      if LReaders[LAt] <> nil then begin LReaders[LAt].WaitFor; LReaders[LAt].Free; end;
    end;
  end;
  WriteLn('real condition readers / 64 joined');
end;
{$endif}

begin
  ControlledContract; RefusalContract; RangeContract; WorkContract;
  {$ifndef PAS2JS}NativeSourceContract; ConcurrentContract;{$endif}
  WriteLn('PASS real conditions ',GChecks,' checks');
end.
