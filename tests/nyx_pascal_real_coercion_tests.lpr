{ Whole original native expression/include windows qualify real constant
  resolution. Explicit compiler-host evaluation evidence stays independent of
  parser, target and source switches. No query invokes a witness body. }
program nyx_pascal_real_coercion_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses SysUtils, Classes, nyx.text, nyx.editing, nyx.scheduler, nyx.pascal.work,
  nyx.pascal.values, nyx.pascal.reals, nyx.pascal.context, nyx.pascal.files,
  nyx.pascal.translation, nyx.pascal.lexing, nyx.pascal.syntax, nyx.pascal.syntax.translated,
  nyx.pascal.binding, nyx.pascal.conditions
  {$ifndef PAS2JS}, real.coercion.fixture{$endif};

{$ifndef PAS2JS}
type
  { A reader retains an immutable world/site, owns its query budget and publishes
    text only after the main thread joins it. Work integers are never shared. }
  TCoercionReader = class(TThread)
  public
    World: INyxPascalSemanticWorld;
    Site: INyxPascalBindingSite;
    Window: TNyxPascalSpan;
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

function TypeName(const AType: INyxPascalSemanticType): TNyxText;
begin
  Result := '';

  if AType = nil then Exit;
  case AType.Builtin of
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
  LScalar: INyxPascalConstantExpression; LLiteral: INyxPascalRealLiteral;
begin
  LSource := Companion('-(1.25)'); LAnswer := Query(LSource,'Observe(','-(1.25)',Facts,Root);
  Check((LAnswer.Status = pbsBound) and (TypeName(LAnswer.BoundType) = 'Single') and
    Supports(LAnswer.BoundType,INyxPascalRealConstant,LReal) and LReal.RealValue.Negative,
    'Unary constants retain operand type and exact sign independently of host arithmetic policy');
  Check(not Supports(LAnswer.BoundType,INyxPascalRealLiteral,LLiteral),
    'A parenthesized unary expression cannot impersonate an original numeric token');
  LSource := Companion('16777217 = 16777216.0','{$minfpconstprec 32}{$excessprecision off}');
  LAnswer := Query(LSource,'Observe(','16777217 = 16777216.0',Profile,Root);
  Check((LAnswer.Status = pbsBound) and Supports(LAnswer.BoundType,INyxPascalConstantExpression,LScalar) and
    LScalar.ConstantValue.BooleanResult,'Mixed comparison converts the integer to the common Single first');
  LSource := Companion('16777217 = 16777216.0','{$minfpconstprec 64}{$excessprecision off}');
  LAnswer := Query(LSource,'Observe(','16777217 = 16777216.0',Profile,Root);
  Check((LAnswer.Status = pbsBound) and Supports(LAnswer.BoundType,INyxPascalConstantExpression,LScalar) and
    not LScalar.ConstantValue.BooleanResult,'The same source numbers compare differently under a Double minimum');
  LSource := Companion('Keep(1.25 + 2)'); LAnswer := Query(LSource,'Observe(','Keep(1.25 + 2)',Profile,Root);
  Check((LAnswer.Status = pbsBound) and (TypeName(LAnswer.BoundType) = 'Single') and
    not Supports(LAnswer.BoundType,INyxPascalRealConstant,LReal) and
    not Supports(LAnswer.BoundType,INyxPascalConstantExpression,LScalar),
    'Generic return types inherit neither real nor scalar argument proofs');
  LSource := 'unit U;{$mode delphi}{$minfpconstprec 32}interface var V:Single;'+
    'implementation procedure Run;begin Observe(V + 1.25);end;end.';
  LAnswer := Query(LSource,'Observe(','V + 1.25',Facts,Root);
  Check((LAnswer.Status = pbsBound) and not Supports(LAnswer.BoundType,INyxPascalRealConstant,LReal),
    'Runtime arithmetic needs no compiler-host folding fact and fabricates no constant');
  LSource := Companion('1.25 + 2'); LAnswer := Query(LSource,'Observe(','1.25 + 2',Facts,Root);
  Check((LAnswer.Status = pbsUnresolved) and (LAnswer.RelatedName = 'REALEVALUATION'),
    'Constant conversion cannot borrow a compiler-host precision from parser facts');
  LAnswer := Query(LSource,'Observe(','1.25 + 2',Profile,Root('ShortInt'));
  Check((LAnswer.Status <> pbsBound) and (LAnswer.RelatedName = 'shortint'),
    'Missing integer primitive facts retain the original required compiler-root measurement');
  LSource := Companion('1 / 3'); LAnswer := Query(LSource,'Observe(','1 / 3',
    NyxPascalWithRealEvaluation(Profile,TNyxPascalRealEvaluationPolicy.Unknown),Root);
  Check((LAnswer.Status = pbsBound) and not Supports(LAnswer.BoundType,INyxPascalRealConstant,LReal),
    'Without host folding evidence the prior integer-division type answer remains value-unknown');
end;

procedure WorkContract;
var LSource: TNyxText; LWorld: INyxPascalSemanticWorld; LSite: INyxPascalBindingSite;
  LWindow: TNyxPascalSpan; LQueries: INyxPascalExpressionQueries; LAccess: TNyxPascalMemberAccess;
  LAnswer: INyxPascalResolution; LValue: TNyxPascalBinaryReal;
  LBudget: INyxPascalAnalysisBudget; LCancel: INyxCancellationScope;
  LAt, LFit: Integer; LRejected: Boolean; LInteger: TNyxPascalConstantValue;
begin
  LSource := Companion('1.25 + 2'); LWorld := WorldFor(LSource,Profile,Root);
  LSite := ExpressionSite(LWorld,LSource,'Observe(','1.25 + 2',LWindow);
  Supports(LWorld,INyxPascalExpressionQueries,LQueries); LFit := -1;
  for LAt := 1 to 20000 do
  begin
    LAnswer := nil; LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      try
        LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess,LBudget);
        Check((LAnswer.Status = pbsBound) and (TypeName(LAnswer.BoundType) = 'Single'),
          'First sufficient allowance publishes the whole mixed expression');
        LFit := LAt;
      except
        on ENyxPascalAnalysisWorkLimit do Check((LAnswer = nil) and (LBudget.Receipt.Total <= LAt),
          'Every smaller allowance refuses atomically without exceeding parent work');
      end;
    finally LBudget.Retire; end;

    if LFit > 0 then Break;
  end;
  Check(LFit > 1,'Mixed conversion charges the existing expression budget');
  WriteLn('real coercion binding work / first fit=',LFit,' every smaller allowance refused');
  LInteger := NyxPascalIntegerToken('18446744073709551615',10); LFit := -1;
  for LAt := 1 to 20000 do
  begin
    LValue := TNyxPascalBinaryReal.Unknown; LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      try
        LValue := NyxPascalRoundIntegerToRealEven(LInteger,pbpX87Extended80,LBudget);
        Check(LValue.SameValue(Rounded('18446744073709551615.0')) and not LValue.Inexact,
          'The complete unsigned-64 value converts exactly to a 64-bit significand');
        LFit := LAt;
      except
        on ENyxPascalAnalysisWorkLimit do Check((LValue.Status = rrsUnknown) and (LBudget.Receipt.Total <= LAt),
          'Every smaller conversion allowance releases work without a partial value');
      end;
    finally LBudget.Retire; end;

    if LFit > 0 then Break;
  end;
  Check(LFit > 1,'Integer conversion charges its complete exact decimal/binary work');
  WriteLn('integer real conversion work / first fit=',LFit,' every smaller allowance refused');
  Check(NyxPascalRoundIntegerToRealEven(TNyxPascalConstantValue.BooleanValue(True),pbpBinary32).Status = rrsUnknown,
    'A Boolean is not silently promoted into an integer conversion');
  Check(NyxPascalRoundIntegerToRealEven(TNyxPascalConstantValue.IntegerValue(1).AsOrdinal,pbpBinary32).Status = rrsUnknown,
    'A nominal enum ordinal needs its explicit integer projection');
  LCancel := NewNyxCancellationScope; LCancel.Cancel;
  LValue := TNyxPascalBinaryReal.Unknown; LRejected := False;
  try LValue := NyxPascalRoundIntegerToRealEven(LInteger,pbpBinary32,LCancel);
  except on ENyxPascalAnalysisCancelled do LRejected := True; end;
  Check(LRejected and (LValue.Status = rrsUnknown),'Plain cancellation publishes no converted integer');
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
  LSource := Load('tests/real.coercion.fixture.pas'); GInclude := Load('tests/real.coercion.cases.inc');
  LWorld := WorldFor(LSource,Profile,Root); LView := LWorld.ModuleAt(2);
  Check(LView.Source.Resolved and (LView.Source.OccurrenceCount = 6),
    'The whole compiler oracle retains its ABI guards and five original case includes');
  Supports(LWorld,INyxPascalExpressionQueries,LQueries);
  LRows := TStringList.Create; LFields := TStringList.Create;
  try
    LRows.Text := String(NativeRealCoercions);
    Check((LRows.Count = 140) and (NativeRealCoercionCalls = 140),'All original native cases execute at all five source policy sites');
    LFields.Delimiter := '|'; LFields.StrictDelimiter := True;
    for LRow := 0 to LRows.Count-1 do
    begin
      LFields.DelimitedText := LRows[LRow]; LOccurrence := LRow div 28+1;
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
      WriteLn('native coercion binding / ',LFields[0],'/',LFields[1],' status=',Ord(LAnswer.Status),
        ' issue=',Ord(LAnswer.Issue),' related=',String(LAnswer.RelatedName));
      Check((LAnswer.Status = pbsBound) and (TypeName(LAnswer.BoundType) = TNyxText(LFields[3])),
        'The unchanged native expression retains its actual inferred compiler type');

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
    Check(NativeRealCoercionCalls = 140,'Semantic queries never execute the native witness body');
  finally LFields.Free; LRows.Free; GInclude := ''; end;
end;

procedure TCoercionReader.Execute;
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
  LWindow: TNyxPascalSpan; LReaders: array of TCoercionReader; LAt: Integer;
begin
  LSource := Companion('1.25 + 2'); LWorld := WorldFor(LSource,Profile,Root);
  LSite := ExpressionSite(LWorld,LSource,'Observe(','1.25 + 2',LWindow); SetLength(LReaders,64);
  try
    for LAt := 0 to High(LReaders) do
    begin
      LReaders[LAt] := TCoercionReader.Create(True); LReaders[LAt].FreeOnTerminate := False;
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
  WriteLn('real coercion readers / 64 joined');
end;
{$endif}

begin
  ControlledContract; WorkContract;
  {$ifndef PAS2JS}NativeSourceContract; ConcurrentContract;{$endif}
  WriteLn('PASS real coercion ',GChecks,' checks');
end.
