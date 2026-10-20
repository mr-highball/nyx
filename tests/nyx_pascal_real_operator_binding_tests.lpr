{ Whole original native expression/include windows qualify real constant
  resolution. Explicit compiler-host evaluation evidence stays independent of
  parser, target and source switches. No query invokes a witness body. }
program nyx_pascal_real_operator_binding_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses SysUtils, Classes, nyx.text, nyx.editing, nyx.scheduler, nyx.pascal.work,
  nyx.pascal.reals, nyx.pascal.context, nyx.pascal.files,
  nyx.pascal.translation, nyx.pascal.lexing, nyx.pascal.syntax, nyx.pascal.syntax.translated,
  nyx.pascal.binding, nyx.pascal.conditions, nyx.pascal.constants
  {$ifndef PAS2JS}, real.operations.fixture{$endif};

{$ifndef PAS2JS}
type
  { A reader retains an immutable world/site, owns its query budget and publishes
    text only after the main thread joins it. Work integers are never shared. }
  TRealOperatorReader = class(TThread)
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
const CNames: array[0..3] of TNyxText = ('Single','Double','Extended','Boolean');
  CSizes: array[0..3] of Integer = (4,8,10,1);
var LAt: Integer;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,TNyxPascalCompilerVersion.Create(3,3,1),
    ppmFPC,pcsoSystemBootstrap).CompleteDefines(True).WithOption('H',ppkFalse);

  if ACpu then Result := Result.WithSymbol(NyxPascalDefine('CPUI386'),ppdDefined);
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
    .WithFile(NyxPascalPhysicalFile('source.pas'),ASource).WithFile(NyxPascalPhysicalFile('real.operations.cases.inc'),GInclude),NyxPascalPhysicalFile('source.pas'),LContext,LBase,
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
  Result := NyxPascalWithRealEvaluation(Facts,TNyxPascalRealEvaluationPolicy.NearestEven(pbpX87Extended80));
end;

function Rounded(const AText: TNyxText): TNyxPascalBinaryReal;
var LSource: TNyxPascalRealValue;
begin

  if (AText <> '') and (AText[1] = '-') then
    LSource := NyxPascalRealToken(Copy(AText,2,Length(AText)-1)).Negated
  else LSource := NyxPascalRealToken(AText);
  Result := NyxPascalRoundRealToEven(LSource,pbpX87Extended80);
end;

procedure Expect(const AExpression, ASettings, AType, AValue: TNyxText);
var LSource: TNyxText; LAnswer: INyxPascalResolution; LReal: INyxPascalRealConstant;
  LLiteral: INyxPascalRealLiteral;
begin
  LSource := Companion(AExpression,ASettings);
  LAnswer := Query(LSource,'Observe(',AExpression,Profile,Root);
  WriteLn('typed real operation / ',String(AExpression),' status=',Ord(LAnswer.Status),
    ' issue=',Ord(LAnswer.Issue),' related=',String(LAnswer.RelatedName));
  Check((LAnswer.Status = pbsBound) and (TypeName(LAnswer.BoundType) = AType),
    'The complete expression retains its compiler result type');
  Check(Supports(LAnswer.BoundType,INyxPascalRealConstant,LReal) and
    LReal.RealValue.SameValue(Rounded(AValue)),'The expression retains its independently represented finite value');
  Check(not Supports(LAnswer.BoundType,INyxPascalRealLiteral,LLiteral),
    'A folded expression cannot pretend to be an original source real token');
end;

procedure ControlledContract;
var LSource: TNyxText; LAnswer: INyxPascalResolution; LReal: INyxPascalRealConstant;
begin
  Expect('1.25 + 2.5','{$minfpconstprec 32}{$excessprecision off}','Single','3.75');
  Expect('1.25 + 2.5','{$minfpconstprec 64}{$excessprecision off}','Double','3.75');
  Expect('1.25 + 2.5','{$minfpconstprec 32}{$excessprecision on}','Extended','3.75');
  Expect('(1.25 + 2.5) * 2.0','{$minfpconstprec 32}{$excessprecision off}','Single','7.5');
  Expect('(16777216.0 + 1.0) - 16777216.0','{$minfpconstprec 32}{$excessprecision off}','Single','0.0');
  Expect('(16777216.0 + 1.0) - 16777216.0','{$minfpconstprec 32}{$excessprecision on}','Extended','1.0');
  LSource := Companion('1.25 + 2.5');
  LAnswer := Query(LSource,'Observe(','1.25 + 2.5',Facts,Root);
  Check((LAnswer.Status = pbsUnresolved) and (LAnswer.RelatedName = 'REALEVALUATION'),
    'Parser evidence alone supplies no compiler-host folding precision');
  LSource := Companion('1.25 = 1.25'); LAnswer := Query(LSource,'Observe(','1.25 = 1.25',Facts,Root);
  WriteLn('comparison context / status=',Ord(LAnswer.Status),' issue=',Ord(LAnswer.Issue),
    ' related=',String(LAnswer.RelatedName),' type=',String(TypeName(LAnswer.BoundType)));
  Check((LAnswer.Status = pbsBound) and (TypeName(LAnswer.BoundType) = 'Boolean') and
    not Supports(LAnswer.BoundType,INyxPascalRealConstant,LReal),
    'Comparisons need represented operands, not invented host arithmetic or a real-valued result');
  LSource := Companion('Keep(1.25 + 2.5)','{$minfpconstprec 32}{$excessprecision off}');
  LAnswer := Query(LSource,'Observe(','Keep(1.25 + 2.5)',Profile,Root);
  Check((LAnswer.Status = pbsBound) and (TypeName(LAnswer.BoundType) = 'Single') and
    not Supports(LAnswer.BoundType,INyxPascalRealConstant,LReal),
    'A generic call inherits the result type without inheriting its argument constant');
  LSource := Companion('1.0 / 0.0'); LAnswer := Query(LSource,'Observe(','1.0 / 0.0',Profile,Root);
  Check((LAnswer.Status = pbsUnresolved) and (LAnswer.Issue = pbiCompilerContext) and
    (LAnswer.RelatedName = 'IEEEERRORS'),
    'Zero division requires the exact source-node checking fact rather than fabricated admission');
  LSource := Companion('1.0e300 * 1.0e300'); LAnswer := Query(LSource,'Observe(',
    '1.0e300 * 1.0e300',Profile,Root);
  Check((LAnswer.Status = pbsBound) and Supports(LAnswer.BoundType,INyxPascalRealConstant,LReal),
    'A large finite compiler-host result remains within its actual extended range');
  LSource := Companion('1.0e4000 * 1.0e4000'); LAnswer := Query(LSource,'Observe(',
    '1.0e4000 * 1.0e4000',Profile,Root);
  Check((LAnswer.Status = pbsUnresolved) and (LAnswer.Issue = pbiCompilerContext) and
    (LAnswer.RelatedName = 'IEEEERRORS'),
    'Positive overflow requires the exact source-node checking fact rather than inferred admission');
end;

function StorageIdentity(const APolicy: TNyxPascalRealEvaluationPolicy): TNyxText;
var LContext: INyxPascalCompilerContext; LFiles: INyxPascalFileCatalog;
  LView, LSystem: INyxPascalTranslatedSyntax; LWorld: INyxPascalSemanticWorld;
  LResult: INyxPascalConstantResult; LIdentity: INyxPascalConstantStorageIdentity;
begin
  LContext := NyxPascalWithRealEvaluation(NyxPascalWithCapturedSize(Facts,
    NyxPascalSizeReference('Byte'),NyxPascalSizeFact(pssKnown,ppmDelphi,ppkTrue,1)),APolicy);
  LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('layout.pas'),
    'unit Layout;interface {$packrecords 1}type TPacket=record Value:Byte;end;implementation end.')
    .WithFile(NyxPascalPhysicalFile('system.pas'),'unit System;interface implementation end.');
  LView := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFiles,NyxPascalPhysicalFile('layout.pas'),
    LContext,TNyxPascalTranslationOptions.Defaults),TNyxPascalSyntaxOptions.Defaults);
  LSystem := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFiles,NyxPascalPhysicalFile('system.pas'),
    LContext,TNyxPascalTranslationOptions.Defaults),TNyxPascalSyntaxOptions.Defaults);
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LView).WithModule(LSystem);
  LResult := PlanNyxPascalConstant('sizeof(TPacket)',TNyxPascalConstantOptions.Defaults)
    .Evaluate(LWorld.At(0,0,LView.Syntax.Lexical.Scalars),TNyxPascalConstantOptions.Defaults);
  Check(Supports(LResult,INyxPascalConstantStorageIdentity,LIdentity),
    'Existing storage identity retains independent compiler-host evidence');
  Result := LIdentity.StorageKey.Value;
end;

procedure PolicyContract;
var LBase, L80, L64: INyxPascalCompilerContext; LPolicy: TNyxPascalRealEvaluationPolicy;
  LUnknownKey, L80Key, L64Key: TNyxText;
begin
  LBase := Facts; L80 := Profile;
  L64 := NyxPascalWithRealEvaluation(LBase,TNyxPascalRealEvaluationPolicy.NearestEven(pbpBinary64));
  Check(not NyxPascalRealEvaluation(LBase).Known,'A parser/target context grants no compiler-host evaluation fact');
  Check(not LBase.SameFacts(L80) and not L80.SameFacts(L64),
    'Independent evaluation algorithm/precision participate in exact context identity');
  Check(CaptureNyxPascalCompilerContext(L80).SameFacts(L80),'Capture retains explicit evaluation evidence');
  LPolicy := NyxPascalRealEvaluation(L80);
  Check(NyxPascalRealEvaluation(L80.InMode(ppmObjFPC)).SamePolicy(LPolicy),
    'Source mode cannot retarget compiler-host arithmetic');
  Check(NyxPascalRealEvaluation(L80.UnknownState).SamePolicy(LPolicy),
    'Unknown source revokes source knowledge independently of immutable host evidence');
  Check(NyxPascalRealEvaluation(RestoreNyxPascalLocalSettings(L80,L64)).SamePolicy(LPolicy),
    'Local restore cannot substitute a different initial compiler host');
  Check(NyxPascalRealParser(L80).SamePolicy(NyxPascalRealParser(LBase)),
    'Installing evaluation evidence cannot change independent parser facts');
  LUnknownKey := StorageIdentity(TNyxPascalRealEvaluationPolicy.Unknown);
  L80Key := StorageIdentity(LPolicy);
  L64Key := StorageIdentity(NyxPascalRealEvaluation(L64));
  Check((LUnknownKey <> L80Key) and (L80Key <> L64Key) and
    (LUnknownKey = StorageIdentity(TNyxPascalRealEvaluationPolicy.Unknown)),
    'Tagged evaluation identity distinguishes known profiles and preserves unknown identity');
end;

procedure WorkContract;
var LSource: TNyxText; LWorld: INyxPascalSemanticWorld; LSite: INyxPascalBindingSite;
  LWindow: TNyxPascalSpan; LQueries: INyxPascalExpressionQueries; LAccess: TNyxPascalMemberAccess;
  LAnswer, LRetained: INyxPascalResolution; LReal: INyxPascalRealConstant;
  LBudget: INyxPascalAnalysisBudget; LCancel: INyxCancellationScope;
  LAt, LFit: Integer; LRejected: Boolean;
begin
  LSource := Companion('1.0 / 3.0','{$minfpconstprec 32}{$excessprecision off}');
  LWorld := WorldFor(LSource,Profile,Root); LSite := ExpressionSite(LWorld,LSource,'Observe(','1.0 / 3.0',LWindow);
  Supports(LWorld,INyxPascalExpressionQueries,LQueries); LFit := -1;
  for LAt := 1 to 20000 do
  begin
    LAnswer := nil; LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      try
        LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess,LBudget);
        Check((LAnswer.Status = pbsBound) and (TypeName(LAnswer.BoundType) = 'Single'),
          'First sufficient allowance publishes the complete constant expression');
        LRetained := LAnswer; LFit := LAt;
      except
        on ENyxPascalAnalysisWorkLimit do Check((LAnswer = nil) and (LBudget.Receipt.Total <= LAt),
          'Every smaller allowance refuses without a partial expression or excess parent work');
      end;
    finally LBudget.Retire; end;

    if LFit > 0 then Break;
  end;
  Check((LFit > 1) and Supports(LRetained.BoundType,INyxPascalRealConstant,LReal) and
    (LReal.RealValue.Status = rrsFinite),'A published expression retains no retired work scope');
  WriteLn('real operator binding work / first fit=',LFit,' every smaller allowance refused');
  LCancel := NewNyxCancellationScope; LCancel.Cancel;
  LAnswer := nil; LRejected := False;
  try LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess,LCancel);
  except on ENyxPascalAnalysisCancelled do LRejected := True; end;
  Check(LRejected and (LAnswer = nil),'Plain cancellation publishes no expression proof');
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
var LRows, LFields: TStringList; LSource, LMarker, LExpression: TNyxText;
  LRow, LAt, LFinish, LComma, LPhysical, LPhysicalFinish, LOccurrence: Integer;
  LFirst, LLast, LCaret: Integer; LOrigin: TNyxPascalSyntaxOrigin;
  LWorld: INyxPascalSemanticWorld; LView: INyxPascalTranslatedSyntax;
  LQueries: INyxPascalExpressionQueries; LAnswer: INyxPascalResolution;
  LAccess: TNyxPascalMemberAccess; LReal: INyxPascalRealConstant; LDescription: TNyxText;
begin
  LSource := Load('tests/real.operations.fixture.pas'); GInclude := Load('tests/real.operations.cases.inc');
  LWorld := WorldFor(LSource,Profile,Root); LView := LWorld.ModuleAt(2);
  Check(LView.Source.Resolved and (LView.Source.OccurrenceCount = 6),
    'The whole original compiler oracle retains all five exact case includes and its ABI guard');
  Supports(LWorld,INyxPascalExpressionQueries,LQueries);
  LRows := TStringList.Create; LFields := TStringList.Create;
  try
    LRows.Text := String(NativeRealOperationObservations);
    Check((LRows.Count = 115) and (NativeRealOperationCalls = 115),'The independent complete expression oracle executed all cases');
    LFields.Delimiter := '|'; LFields.StrictDelimiter := True;
    for LRow := 0 to LRows.Count-1 do
    begin
      LFields.DelimitedText := LRows[LRow]; LOccurrence := LRow div 23+1;
      LMarker := 'Emit('''+TNyxText(LFields[1])+''',';

      if StrToInt(LFields[2]) >= 4 then LMarker := 'EmitRelation('''+TNyxText(LFields[1])+''',';
      LAt := Pos(LMarker,GInclude); Check(LAt > 0,'The original included expression retains its independent marker');
      LComma := 0;
      while LComma < 4 do
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
      Check((LFirst >= 0) and (LLast > LFirst) and (LCaret >= LLast),
        'The exact included code-token union owns this expression query');
      LAnswer := LQueries.ExpressionType(LWorld.At(2,LView.Syntax.ScopeAt(LFirst),LCaret),
        NyxPascalSpan(LFirst,LLast-LFirst),LAccess);
      WriteLn('native expression binding / ',LFields[0],'/',LFields[1],' status=',Ord(LAnswer.Status),
        ' issue=',Ord(LAnswer.Issue),' related=',String(LAnswer.RelatedName));
      Check((LAnswer.Status = pbsBound) and (TypeName(LAnswer.BoundType) = TNyxText(LFields[5])),
        'The unchanged original expression agrees with the actual inferred native type');

      if StrToInt(LFields[2]) < 4 then
      begin
        Check(Supports(LAnswer.BoundType,INyxPascalRealConstant,LReal),'An admitted real expression retains a finite constant proof');
        LDescription := '0';

        if LReal.RealValue.Negative then LDescription := '1';
        LDescription := LDescription+'|'+LReal.RealValue.Significand+'|'+TNyxText(IntToStr(LReal.RealValue.BinaryExponent));
        Check(LDescription = TNyxText(LFields[6]+'|'+LFields[7]+'|'+LFields[8]),
          'The unchanged original expression agrees with the actual native value bits');
      end;
    end;
    Check(NativeRealOperationCalls = 115,'Semantic queries never invoke the native compiler witness');
  finally LFields.Free; LRows.Free; GInclude := ''; end;
end;

procedure TRealOperatorReader.Execute;
var LQueries: INyxPascalExpressionQueries; LAnswer: INyxPascalResolution;
  LReal: INyxPascalRealConstant; LAccess: TNyxPascalMemberAccess; LBudget: INyxPascalAnalysisBudget;
begin
  LBudget := NewNyxPascalAnalysisBudget(20000);
  try
    try
      Supports(World,INyxPascalExpressionQueries,LQueries);
      LAnswer := LQueries.ExpressionType(Site,Window,LAccess,LBudget);

      if (LAnswer.Status <> pbsBound) or not Supports(LAnswer.BoundType,INyxPascalRealConstant,LReal) or
        not LReal.RealValue.SameValue(Rounded('3.75')) then
        raise Exception.Create('Concurrent real constant expression differs');
    except on E: Exception do ErrorText := E.Message; end;
  finally LBudget.Retire; end;
end;

procedure ConcurrentContract;
var LSource: TNyxText; LWorld: INyxPascalSemanticWorld; LSite: INyxPascalBindingSite;
  LWindow: TNyxPascalSpan; LReaders: array of TRealOperatorReader; LAt: Integer;
begin
  LSource := Companion('1.25 + 2.5'); LWorld := WorldFor(LSource,Profile,Root);
  LSite := ExpressionSite(LWorld,LSource,'Observe(','1.25 + 2.5',LWindow); SetLength(LReaders,64);
  try
    for LAt := 0 to High(LReaders) do
    begin
      LReaders[LAt] := TRealOperatorReader.Create(True); LReaders[LAt].FreeOnTerminate := False;
      LReaders[LAt].World := LWorld; LReaders[LAt].Site := LSite; LReaders[LAt].Window := LWindow;
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
  WriteLn('real operator binding readers / 64 joined');
end;
{$endif}

begin
  PolicyContract; ControlledContract; WorkContract;
  {$ifndef PAS2JS}NativeSourceContract; ConcurrentContract;{$endif}
  WriteLn('PASS real operator binding ',GChecks,' checks');
end.
