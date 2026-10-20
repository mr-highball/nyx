{ Whole original native expression/include windows qualify real constant
  resolution. Explicit compiler-host evaluation evidence stays independent of
  parser, target and source switches. No query invokes a witness body. }
program nyx_pascal_real_declaration_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses SysUtils, Classes, nyx.text, nyx.editing, nyx.scheduler, nyx.pascal.work,
  nyx.pascal.reals, nyx.pascal.context, nyx.pascal.files,
  nyx.pascal.translation, nyx.pascal.lexing, nyx.pascal.syntax, nyx.pascal.syntax.translated,
  nyx.pascal.binding, nyx.pascal.conditions
  {$ifndef PAS2JS}, nyx.pascal.values, real.declarations.fixture{$endif};

{$ifndef PAS2JS}
type
  { A reader retains an immutable world/site, owns its query budget and publishes
    text only after the main thread joins it. Work integers are never shared. }
  TDeclarationReader = class(TThread)
  public
    World: INyxPascalSemanticWorld;
    Site: INyxPascalBindingSite;
    Window: TNyxPascalSpan;
    ErrorText: String;
    procedure Execute; override;
  end;
{$endif}

var GChecks: Integer; GInclude, GConstants: TNyxText;

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
    .WithFile(NyxPascalPhysicalFile('source.pas'),ASource).WithFile(NyxPascalPhysicalFile('real.declarations.cases.inc'),GInclude).WithFile(NyxPascalPhysicalFile('real.declarations.constants.inc'),GConstants),NyxPascalPhysicalFile('source.pas'),LContext,LBase,
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

function TypeName(const AType: INyxPascalSemanticType): TNyxText;
var LType: INyxPascalSemanticType; LValue: INyxPascalConstantExpression;
begin
  Result := '';

  if AType = nil then Exit;
  LType := AType;

  if Supports(LType,INyxPascalConstantExpression,LValue) then LType := LValue.ResultType;

  if (LType.Kind in [pstAlias,pstDistinct]) and (LType.Definition <> nil) then
    Exit(LType.Definition.Declaration.Name);
  case LType.Builtin of
    pbbSingle: Result := 'Single'; pbbDouble: Result := 'Double'; pbbExtended: Result := 'Extended';
    pbbBoolean: Result := 'Boolean';
  else Result := '';
  end;
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

procedure ControlledContract;
var LSource: TNyxText; LAnswer: INyxPascalResolution; LReal: INyxPascalRealConstant;
  LLiteral: INyxPascalRealLiteral;
  LWorld: INyxPascalSemanticWorld; LView: INyxPascalTranslatedSyntax;
begin
  LSource := 'unit U;{$mode delphi}{$minfpconstprec 32}interface const Spacing=1.25;'+
    '{$minfpconstprec 64}implementation procedure Run;begin Observe(Spacing);end;end.';
  LAnswer := Query(LSource,'Observe(','Spacing',Profile,Root);
  WriteLn('controlled named real / status=',Ord(LAnswer.Status),' issue=',Ord(LAnswer.Issue),' related=',String(LAnswer.RelatedName));
  Check((LAnswer.Status = pbsBound) and (TypeName(LAnswer.BoundType) = 'Single') and
    Supports(LAnswer.BoundType,INyxPascalRealConstant,LReal) and LReal.RealValue.SameValue(Rounded('1.25')),
    'A later use cannot change the constant declaration minimum or value');
  Check(not Supports(LAnswer.BoundType,INyxPascalRealLiteral,LLiteral),
    'A named constant reference does not impersonate an authored numeric token');
  LSource := 'unit U;{$mode delphi}{$minfpconstprec 32}interface const Spacing=1.25;'+
    'implementation procedure Run;const Spacing=2.5;begin Observe(Spacing);end;end.';
  LAnswer := Query(LSource,'Observe(','Spacing',Profile,Root);
  Check((LAnswer.Status = pbsBound) and Supports(LAnswer.BoundType,INyxPascalRealConstant,LReal) and
    LReal.RealValue.SameValue(Rounded('2.5')),'The local declaration scope retains ordinary constant shadowing');
  LSource := 'unit U;{$mode delphi}{$minfpconstprec 32}interface const First=Later;Later=1.25;'+
    'implementation procedure Run;begin Observe(First);end;end.';
  LAnswer := Query(LSource,'Observe(','First',Profile,Root);
  Check(LAnswer.Status <> pbsBound,'A later declaration cannot supply an earlier initializer name');
  LSource := 'unit U;{$mode delphi}{$minfpconstprec 32}interface const Broken=Broken;'+
    'implementation procedure Run;begin Observe(Broken);end;end.';
  LAnswer := Query(LSource,'Observe(','Broken',Profile,Root);
  Check((LAnswer.Status = pbsUnresolved) and (LAnswer.Issue = pbiRecursiveType),
    'A self reference refuses through the bounded declaration value path');
  LSource := Companion('Double(Single(0.1))'); LAnswer := Query(LSource,'Observe(','Double(Single(0.1))',Profile,Root);
  Check((LAnswer.Status = pbsBound) and Supports(LAnswer.BoundType,INyxPascalRealConstant,LReal) and
    LReal.RealValue.SameValue(Rounded('0.1').RoundToEven(pbpBinary32)),
    'A wider explicit cast cannot recover bits lost by the nested Single cast');
  LSource := Companion('Keep(Single(1.25))'); LAnswer := Query(LSource,'Observe(','Keep(Single(1.25))',Profile,Root);
  Check((LAnswer.Status = pbsBound) and (TypeName(LAnswer.BoundType) = 'Single') and
    not Supports(LAnswer.BoundType,INyxPascalRealConstant,LReal),'A generic return cannot inherit its cast argument value');
  LSource := 'unit U;{$mode delphi}{$minfpconstprec 32}interface const First=1.25;'+
    '{$unknown_effect}const Second=First;implementation procedure Run;begin Observe(Second);end;end.';
  LWorld := WorldFor(LSource,Profile,Root); LView := LWorld.ModuleAt(2);
  LAnswer := LWorld.Lookup(LWorld.At(2,0,LView.Syntax.Lexical.Scalars),NyxPascalBindingName('Second'));
  Check(not LView.Source.Resolved and (LAnswer.Status <> pbsBound),
    'Unknown effects refuse the source projection and cannot publish a following declaration value');
  LSource := Companion('Single(1.25)'); LAnswer := Query(LSource,'Observe(','Single(1.25)',
    NyxPascalWithRealEvaluation(Profile,TNyxPascalRealEvaluationPolicy.Unknown),Root);
  Check((LAnswer.Status = pbsBound) and Supports(LAnswer.BoundType,INyxPascalRealConstant,LReal),
    'A represented-real conversion needs no invented integer/host arithmetic intermediate');
  LSource := Companion('Single(2)'); LAnswer := Query(LSource,'Observe(','Single(2)',
    NyxPascalWithRealEvaluation(Profile,TNyxPascalRealEvaluationPolicy.Unknown),Root);
  Check((LAnswer.Status <> pbsBound) and (LAnswer.RelatedName = 'REALEVALUATION'),
    'An integer cast cannot invent its compiler-host conversion precision');
  LSource := Companion('Single(1.25,2.5)'); LAnswer := Query(LSource,'Observe(','Single(1.25,2.5)',Profile,Root);
  Check(LAnswer.Status <> pbsBound,'A numeric type cast still requires exactly one complete expression');
end;

procedure WorkContract;
var LSource: TNyxText; LWorld: INyxPascalSemanticWorld; LSite: INyxPascalBindingSite;
  LWindow: TNyxPascalSpan; LQueries: INyxPascalExpressionQueries; LAccess: TNyxPascalMemberAccess;
  LAnswer: INyxPascalResolution; LBudget: INyxPascalAnalysisBudget;
  LAt, LFit: Integer; LRejected: Boolean; LCancel: INyxCancellationScope;
begin
  LSource := 'unit U;{$mode delphi}{$minfpconstprec 32}interface const Spacing=1.25;'+
    'implementation procedure Run;begin Observe(Single(Spacing) + 2);end;end.';
  LWorld := WorldFor(LSource,Profile,Root);
  LSite := ExpressionSite(LWorld,LSource,'Observe(','Single(Spacing) + 2',LWindow);
  Supports(LWorld,INyxPascalExpressionQueries,LQueries); LFit := -1;
  for LAt := 1 to 20000 do
  begin
    LAnswer := nil; LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      try
        LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess,LBudget);
        Check((LAnswer.Status = pbsBound) and (TypeName(LAnswer.BoundType) = 'Single'),
          'First sufficient allowance publishes the full declaration/cast expression');
        LFit := LAt;
      except
        on ENyxPascalAnalysisWorkLimit do Check((LAnswer = nil) and (LBudget.Receipt.Total <= LAt),
          'Every smaller allowance refuses without partial declaration/cast state');
      end;
    finally LBudget.Retire; end;

    if LFit > 0 then Break;
  end;
  Check(LFit > 1,'Declaration/cast work remains inside the existing query budget');
  WriteLn('real declaration work / first fit=',LFit,' every smaller allowance refused');
  LCancel := NewNyxCancellationScope; LCancel.Cancel;
  LAnswer := nil; LRejected := False;
  try LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess,LCancel);
  except on ENyxPascalAnalysisCancelled do LRejected := True; end;
  Check(LRejected and (LAnswer = nil),'Plain cancellation publishes no named/cast value');
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
var LRows, LFields: TStringList; LSource, LMarker, LExpression, LDescription: TNyxText;
  LRow, LAt, LFinish, LComma, LPhysical, LPhysicalFinish, LOccurrence: Integer;
  LFirst, LLast, LCaret: Integer; LOrigin: TNyxPascalSyntaxOrigin;
  LWorld: INyxPascalSemanticWorld; LView: INyxPascalTranslatedSyntax;
  LQueries: INyxPascalExpressionQueries; LAnswer: INyxPascalResolution;
  LAccess: TNyxPascalMemberAccess; LReal: INyxPascalRealConstant;
  LScalar: INyxPascalConstantExpression;
begin
  LSource := Load('tests/real.declarations.fixture.pas'); GInclude := Load('tests/real.declarations.cases.inc'); GConstants := Load('tests/real.declarations.constants.inc');
  LWorld := WorldFor(LSource,Profile,Root); LView := LWorld.ModuleAt(2);
  Check(LView.Source.Resolved and (LView.Source.OccurrenceCount = 11),
    'The whole compiler oracle retains its ABI guards and five original case includes');
  Supports(LWorld,INyxPascalExpressionQueries,LQueries);
  LRows := TStringList.Create; LFields := TStringList.Create;
  try
    LRows.Text := String(NativeRealDeclarations);
    Check((LRows.Count = 100) and (NativeRealDeclarationCalls = 100),'All original native cases execute at all five source policy sites');
    LFields.Delimiter := '|'; LFields.StrictDelimiter := True;
    for LRow := 0 to LRows.Count-1 do
    begin
      LFields.DelimitedText := LRows[LRow]; LOccurrence := (LRow div 20+1)*2;
      LMarker := 'Emit('''+TNyxText(LFields[1])+''',';

      if LFields[3] = 'Boolean' then LMarker := 'EmitRelation('''+TNyxText(LFields[1])+''',';
      LAt := Pos(LMarker,GInclude); Check(LAt > 0,'The included expression retains its unique authored marker');
      LComma := 0;
      while LComma < 2 do
      begin

        if GInclude[LAt] = ',' then Inc(LComma);
        Inc(LAt);
      end;
      while GInclude[LAt] in [#9,#10,#13,' '] do Inc(LAt);
      LFinish := LAt;
      while GInclude[LFinish] <> ';' do Inc(LFinish);
      LExpression := Copy(GInclude,LAt,LFinish-LAt-1);
      LPhysical := NyxTextScalarCount(Copy(GInclude,1,LAt-1));
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
      Check((LFirst >= 0) and (LLast > LFirst) and (LCaret >= LLast),'The exact include occurrence owns the complete original expression');
      LAnswer := LQueries.ExpressionType(LWorld.At(2,LView.Syntax.ScopeAt(LFirst),LCaret),
        NyxPascalSpan(LFirst,LLast-LFirst),LAccess);
      WriteLn('native declaration binding / ',LFields[0],'/',LFields[1],' status=',Ord(LAnswer.Status),
        ' issue=',Ord(LAnswer.Issue),' related=',String(LAnswer.RelatedName));
      Check((LAnswer.Status = pbsBound) and (TypeName(LAnswer.BoundType) = TNyxText(LFields[3])),
        'The unchanged native expression retains its actual inferred compiler type');
      if LFields[3] = 'TAmount' then
        Check((LAnswer.BoundType.Kind = pstDistinct) and (LAnswer.BoundType.Definition <> nil) and
          (LAnswer.BoundType.Definition.Declaration.Name = 'TAmount'),
          'A constant cast retains ordinary distinct declaration identity as well as generic result identity');

      if LFields[2] = '0' then
        Check(not Supports(LAnswer.BoundType,INyxPascalRealConstant,LReal) and
          (not Supports(LAnswer.BoundType,INyxPascalConstantExpression,LScalar) or
          (LScalar.ConstantValue.Kind = pvvUnknown)),'Runtime variables retain no fabricated scalar/real value')
      else if LFields[3] = 'Boolean' then
        Check(Supports(LAnswer.BoundType,INyxPascalConstantExpression,LScalar) and
          (LScalar.ConstantValue.Kind = pvvBoolean) and
          (Ord(LScalar.ConstantValue.BooleanResult) = StrToInt(LFields[4])),
          'Actual native comparison agrees after integer/common-real conversion')
      else
      begin
        Check(Supports(LAnswer.BoundType,INyxPascalRealConstant,LReal),'An admitted real constant retains its represented value');
        LDescription := '0';

        if LReal.RealValue.Negative then LDescription := '1';
        LDescription := LDescription+'|'+LReal.RealValue.Significand+'|'+TNyxText(IntToStr(LReal.RealValue.BinaryExponent));
        Check(LDescription = TNyxText(LFields[4]+'|'+LFields[5]+'|'+LFields[6]),
          'The original native expression retains its actual rounded compiler value');
      end;
    end;
    Check(NativeRealDeclarationCalls = 100,'Semantic queries never execute the native witness body');
  finally LFields.Free; LRows.Free; GInclude := ''; GConstants := ''; end;
end;

procedure TDeclarationReader.Execute;
var LQueries: INyxPascalExpressionQueries; LAnswer: INyxPascalResolution;
  LReal: INyxPascalRealConstant; LAccess: TNyxPascalMemberAccess; LBudget: INyxPascalAnalysisBudget;
begin
  LBudget := NewNyxPascalAnalysisBudget(20000);
  try
    try
      Supports(World,INyxPascalExpressionQueries,LQueries); LAnswer := LQueries.ExpressionType(Site,Window,LAccess,LBudget);

      if (LAnswer.Status <> pbsBound) or not Supports(LAnswer.BoundType,INyxPascalRealConstant,LReal) or
        not LReal.RealValue.SameValue(Rounded('3.25')) then raise Exception.Create('Concurrent mixed constant differs');
    except on E: Exception do ErrorText := E.Message; end;
  finally LBudget.Retire; end;
end;

procedure ConcurrentContract;
var LSource: TNyxText; LWorld: INyxPascalSemanticWorld; LSite: INyxPascalBindingSite;
  LWindow: TNyxPascalSpan; LReaders: array of TDeclarationReader; LAt: Integer;
begin
  LSource := Companion('Single(1.25) + 2'); LWorld := WorldFor(LSource,Profile,Root);
  LSite := ExpressionSite(LWorld,LSource,'Observe(','Single(1.25) + 2',LWindow); SetLength(LReaders,64);
  try
    for LAt := 0 to High(LReaders) do
    begin
      LReaders[LAt] := TDeclarationReader.Create(True); LReaders[LAt].FreeOnTerminate := False;
      LReaders[LAt].World := LWorld; LReaders[LAt].Site := LSite; LReaders[LAt].Window := LWindow; LReaders[LAt].Start;
    end;
    for LAt := 0 to High(LReaders) do
    begin LReaders[LAt].WaitFor; Check(LReaders[LAt].ErrorText = '',LReaders[LAt].ErrorText); end;
  finally
    for LAt := 0 to High(LReaders) do
    begin

      if LReaders[LAt] <> nil then begin LReaders[LAt].WaitFor; LReaders[LAt].Free; end;
    end;
  end;
  WriteLn('real declaration readers / 64 joined');
end;
{$endif}

begin
  ControlledContract; WorkContract;
  {$ifndef PAS2JS}NativeSourceContract; ConcurrentContract;{$endif}
  WriteLn('PASS real declarations ',GChecks,' checks');
end.
