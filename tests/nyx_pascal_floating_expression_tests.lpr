{ Native expressions remain an independent oracle. Semantic queries use each
  original whole expression window, including nested code and directive timing. }
program nyx_pascal_floating_expression_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses SysUtils, Classes, nyx.text, nyx.editing, nyx.scheduler, nyx.pascal.work,
  nyx.pascal.context, nyx.pascal.files, nyx.pascal.translation, nyx.pascal.lexing,
  nyx.pascal.syntax, nyx.pascal.syntax.translated, nyx.pascal.binding, nyx.pascal.requirements
  {$ifndef PAS2JS}, floating.expressions.fixture{$endif};

type
  { Expected nominal results are independent of their shared representation. }
  TExpectedFloat = (efSingle,efDouble,efExtended,efCurrency,efComp,efReal,efNamed,efBoolean);
  { Controlled browser companions use explicit authored source settings. These
    do not supply measurements or infer the configured compiler's defaults. }
  TFloatGroup = (fgX87Off,fgX87On,fgSSE2Off,fgSSE2On);
  { Value-owned witness correspondence; strings are actual Pascal source, not
    product behavior selectors. No interface is stored in a record. }
  TFloatCase = record
    LabelText, ExpressionText, SetupText, SuffixText: TNyxText;
    Expected: TExpectedFloat;
    Group: TFloatGroup;
  end;
  { A bounded dynamic vector owns only the fixture's fixed observation rows. }
  TFloatCases = array of TFloatCase;
  {$ifndef PAS2JS}
  { Each reader owns its evaluation frames over shared immutable source/root
    data. Main joins before observing outcomes or retiring any shared lease. }
  TFloatingExpressionReader = class(TThread)
  public
    World: INyxPascalSemanticWorld;
    Site: INyxPascalBindingSite;
    Window: TNyxPascalSpan;
    ErrorText: String;
    procedure Execute; override;
  end;
  {$endif}

const
  CNames: array[TExpectedFloat] of TNyxText =
    ('Single','Double','Extended','Currency','Comp','Real','TNamedFloat','Boolean');
  CMode = '{$mode delphi}{$H+}{$codepage utf8}{$R+}{$modeswitch implicitfunctionspecialization+}';
  CDeclarations = 'interface type TNamedFloat=type Double;var F32:Single;F64:Double;F80:Extended;'+
    'Money:Currency;Integral:Comp;RealValue:Real;NamedValue:TNamedFloat;I32:LongInt;'+
    'function KeepFloat<T>(const AValue:T):T;implementation ';

var
  GChecks: Integer;
  GCases: TFloatCases;
  {$ifndef PAS2JS}GFixture: TNyxText;{$endif}

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then raise Exception.Create(AReason);
end;

procedure Add(const ALabel, AExpression: TNyxText; AExpected: TExpectedFloat;
  AGroup: TFloatGroup = fgX87Off; const ASetup: TNyxText = ''; const ASuffix: TNyxText = '');
var LAt: Integer;
begin
  LAt := Length(GCases); SetLength(GCases,LAt+1);
  GCases[LAt].LabelText := ALabel; GCases[LAt].ExpressionText := AExpression;
  GCases[LAt].Expected := AExpected; GCases[LAt].Group := AGroup;
  GCases[LAt].SetupText := ASetup; GCases[LAt].SuffixText := ASuffix;
end;

procedure Cases;
begin
  Add('off-single','F32 + F32',efSingle);
  Add('off-mixed','F32 + F64',efDouble);
  Add('off-reversed','F64 + F32',efDouble);
  Add('off-real-left','RealValue + F64',efReal);
  Add('off-real-right','F64 + RealValue',efDouble);
  Add('off-named-left','NamedValue + F64',efNamed);
  Add('off-float-int','F32 + I32',efSingle);
  Add('off-int-float','I32 + F32',efSingle);
  Add('off-comp-pair','Integral + Integral',efExtended);
  Add('off-comp-int','Integral + I32',efComp);
  Add('off-currency-mixed','Money + F64',efCurrency);
  Add('off-currency-divide','Money / Integral',efCurrency);
  Add('off-integer-divide','I32 / I32',efExtended);
  Add('off-negative','-F32',efSingle);
  Add('off-positive','+F32',efSingle);
  Add('off-compare','F32 < F64',efBoolean);
  Add('off-mixed-compare','I32 < F64',efBoolean);
  Add('off-currency-compare','Money > Integral',efBoolean);
  Add('off-chain','F32 + F32 * F64',efDouble);
  Add('off-call','KeepFloat(F32 + F32) + F32',efSingle);
  Add('on-single','F32 + F32',efExtended,fgX87On);
  Add('on-mixed','F32 + F64',efExtended,fgX87On);
  Add('on-float-int','F32 + I32',efSingle,fgX87On);
  Add('on-real','RealValue + F64',efExtended,fgX87On);
  Add('on-currency','Money * F64',efCurrency,fgX87On);
  Add('on-comp-pair','Integral + Integral',efExtended,fgX87On);
  Add('on-comp-int','Integral + I32',efComp,fgX87On);
  Add('on-negative','-F32',efExtended,fgX87On);
  Add('on-positive','+F32',efSingle,fgX87On);
  Add('on-integer-divide','I32 / I32',efExtended,fgX87On);
  Add('sse-single-divide','F32 / F32',efSingle,fgSSE2Off);
  Add('sse-integer-divide','I32 / I32',efDouble,fgSSE2Off);
  Add('sse-float-int','F32 + I32',efSingle,fgSSE2Off);
  Add('sse-real-negative','-RealValue',efReal,fgSSE2Off);
  Add('sse-on-single','F32 + F32',efExtended,fgSSE2On);
  Add('sse-on-integer','I32 / I32',efDouble,fgSSE2On);
  Add('sse-on-negative','-F32',efExtended,fgSSE2On);
  Add('sse-on-positive','+F32',efSingle,fgSSE2On);
  Add('precision-before-right','F32 + {$excessprecision on} F32',efExtended);
  Add('precision-after-right','F32 + F32',efSingle,fgX87Off,'',' {$excessprecision on}');
  Add('precision-unary-tail','-F32',efSingle,fgX87Off,'',' {$excessprecision on}');
  Add('precision-grouped','(F32 + F32) + {$excessprecision on} F32',efExtended);
  Add('fpu-before-right','I32 / {$fputype SSE2} I32',efDouble);
  Add('fpu-after-right','I32 / I32',efDouble,fgX87Off,'',' {$fputype SSE2}');
  Add('fpu-whole-expression','I32 / I32 + {$fputype SSE2} F32',efDouble);
  Add('fpu-grouped-expression','(I32 / I32) + {$fputype SSE2} F32',efExtended);
  Add('precision-ifopt-flush','F32 + F32',efExtended,fgSSE2Off,'',
    ' {$excessprecision on}{$ifopt R+}{$endif}');
  Add('precision-pop-tail','F32 + F32',efExtended,fgSSE2Off,
    '{$push}{$excessprecision on}',' {$pop}');
  Add('precision-after-pop','F32 + F32',efSingle,fgSSE2Off);
  Add('precision-macro-tail','FLOATPAIR',efSingle,fgSSE2Off,
    '{$macro on}{$define FLOATPAIR:=F32 + F32}',' {$excessprecision on}');
end;

function Root(const AOmit: TNyxText = ''): INyxPascalCompilerContext;
const CTypes: array[0..6] of TNyxText = ('Single','Double','Extended','Currency','Comp','LongInt','Boolean');
  CSizes: array[0..6] of Integer = (4,8,10,8,8,4,1);
var LAt: Integer;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,TNyxPascalCompilerVersion.Create(3,3,1),
    ppmFPC,pcsoSystemBootstrap).WithOption('H',ppkFalse).CompleteDefines(True)
    .WithSymbol(NyxPascalDefine('CPUI386'),ppdDefined);
  for LAt := 0 to High(CTypes) do
  begin

    if NyxPascalASCIIKey(CTypes[LAt]) = NyxPascalASCIIKey(AOmit) then Continue;
    Result := NyxPascalWithCapturedSize(Result,NyxPascalSizeReference(CTypes[LAt]),
      NyxPascalSizeFact(pssKnown,ppmFPC,ppkFalse,CSizes[LAt]));
  end;
  Result := NyxPascalWithCapturedSize(Result,NyxPascalSizeReference('Real'),
    NyxPascalSizeFact(pssUndeclared,ppmFPC,ppkFalse));
end;

function Facts: INyxPascalCompilerContext;
begin
  Result := NyxPascalWithConfiguredFPU(NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3,3,1),ppmDelphi).CompleteDefines(True)
    .WithSymbol(NyxPascalDefine('CPUI386'),ppdDefined)
    .WithSymbol(NyxPascalDefine('FPUX87'),ppdDefined).WithOption('H',ppkTrue),NyxPascalFPU('X87'));
end;

function View(const AName, AText: TNyxText;
  const AContext: INyxPascalCompilerContext): INyxPascalTranslatedSyntax;
begin
  Result := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile(AName),AText),NyxPascalPhysicalFile(AName),AContext,
    TNyxPascalTranslationOptions.Defaults),TNyxPascalSyntaxOptions.Defaults);
end;

function WorldFor(const ASource: TNyxText; const ARoot: INyxPascalCompilerContext;
  const AProfile: INyxPascalCompilerContext = nil): INyxPascalSemanticWorld;
var LContext: INyxPascalCompilerContext;
begin
  LContext := AProfile;

  if LContext = nil then LContext := Facts;
  LContext := NyxPascalWithBootstrapContext(LContext,ARoot);
  Result := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults.WithImplicitScopes(pispCompilerUnits))
    .WithModule(View('system.pas','unit System;{$mode fpc}interface type Real=type Double;implementation end.',LContext))
    .WithModule(View('objpas.pas','unit ObjPas;{$mode fpc}interface implementation end.',LContext))
    .WithModule(View('floating.expressions.fixture.pas',ASource,LContext));
end;

function Companion(const ACase: TFloatCase): TNyxText;
var LSettings: TNyxText;
begin
  case ACase.Group of
    fgX87Off: LSettings := '{$fputype X87}{$excessprecision off}';
    fgX87On: LSettings := '{$fputype X87}{$excessprecision on}';
    fgSSE2Off: LSettings := '{$fputype SSE2}{$excessprecision off}';
    fgSSE2On: LSettings := '{$fputype SSE2}{$excessprecision on}';
  end;
  Result := 'unit floating.expressions.fixture;'+CMode+CDeclarations+LSettings+
    'procedure Run;begin '+ACase.SetupText+'Emit('''+ACase.LabelText+''', Observe('+
    ACase.ExpressionText+ACase.SuffixText+'));end;end.';
end;

function CaseSite(const AWorld: INyxPascalSemanticWorld; const ACase: TFloatCase;
  out AWindow: TNyxPascalSpan): INyxPascalBindingSite;
var LView: INyxPascalTranslatedSyntax; LText, LMarker: TNyxText;
  LAt, LStart, LFinish, LPhysicalStart, LPhysicalFinish, LOriginIndex, LCaret: Integer;
  LOrigin: TNyxPascalSyntaxOrigin;
begin
  LView := AWorld.ModuleAt(2);
  LText := NyxPascalOccurrenceLexical(LView.Source,0).Source;
  LMarker := 'Emit('''+ACase.LabelText+''', Observe(';
  LAt := Pos(LMarker,LText);
  Check(LAt > 0,'The original complete fixture retains its unique expression label');
  Inc(LAt,Length(LMarker));
  Check(Copy(LText,LAt,Length(ACase.ExpressionText)) = ACase.ExpressionText,
    'The label owns the exact expression input, including authored directives');
  LPhysicalStart := NyxTextScalarCount(Copy(LText,1,LAt-1));
  LPhysicalFinish := LPhysicalStart+NyxTextScalarCount(ACase.ExpressionText);
  LStart := -1; LFinish := -1;
  { Own the complete code-token union. Removed trailing directives/trivia have
    no cursor affinity; a macro's complete physical span owns all expansions. }
  for LOriginIndex := 0 to LView.OriginCount-1 do
  begin
    LOrigin := LView.Origin(LOriginIndex);

    if (LOrigin.Occurrence <> 0) or (LOrigin.PhysicalSpan.Start < LPhysicalStart) or
      (LOrigin.PhysicalSpan.Finish > LPhysicalFinish) then Continue;

    if LStart < 0 then LStart := LOrigin.VirtualSpan.Start;
    LFinish := LOrigin.VirtualSpan.Finish;
  end;
  Check((LStart >= 0) and (LFinish > LStart),'Both original boundaries map to the complete analysis window');
  AWindow := NyxPascalSpan(LStart,LFinish-LStart);
  LCaret := -1;
  for LOriginIndex := 0 to LView.OriginCount-1 do
  begin
    LOrigin := LView.Origin(LOriginIndex);

    if LOrigin.VirtualSpan.Start >= LFinish then
    begin
      LCaret := LOrigin.VirtualSpan.Start;
      Break;
    end;
  end;
  Check(LCaret >= LFinish,'The query site uses an explicitly captured following token, not a synthetic separator');
  Result := AWorld.At(2,LView.Syntax.ScopeAt(LStart),LCaret);
end;

function TypeName(const AType: INyxPascalSemanticType): TNyxText;
var LType: INyxPascalSemanticType; LAt: Integer;
begin
  Result := ''; LType := AType;
  for LAt := 0 to 63 do
  begin

    if LType = nil then Exit;

    if LType.Kind <> pstAlias then Break;
    LType := LType.Target;
  end;

  if LType.Kind = pstDistinct then Exit(LType.Definition.Declaration.Name);
  case LType.Builtin of
    pbbSingle: Result := 'Single'; pbbDouble: Result := 'Double';
    pbbExtended: Result := 'Extended'; pbbCurrency: Result := 'Currency';
    pbbComp: Result := 'Comp'; pbbBoolean: Result := 'Boolean';
  else Result := 'unsupported'; end;
end;

procedure ExpressionContract;
var LWorld: INyxPascalSemanticWorld; LAt: Integer; LSite: INyxPascalBindingSite;
  LWindow: TNyxPascalSpan; LQueries: INyxPascalExpressionQueries;
  LAnswer: INyxPascalResolution; LAccess: TNyxPascalMemberAccess;
begin
  {$ifndef PAS2JS}LWorld := WorldFor(GFixture,Root);{$endif}
  for LAt := 0 to High(GCases) do
  begin
    {$ifdef PAS2JS}LWorld := WorldFor(Companion(GCases[LAt]),Root);{$endif}
    LSite := CaseSite(LWorld,GCases[LAt],LWindow);
    Supports(LWorld,INyxPascalExpressionQueries,LQueries);
    LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess);
    WriteLn('floating expression / ',String(GCases[LAt].LabelText),' status=',Ord(LAnswer.Status),
      ' issue=',Ord(LAnswer.Issue),' related=',String(LAnswer.RelatedName));
    Check((LAnswer.Status = pbsBound) and (TypeName(LAnswer.BoundType) = CNames[GCases[LAt].Expected]),
      'The original floating expression retains the native nominal result: '+String(GCases[LAt].LabelText));
  end;
end;

function Query(const ACase: TFloatCase; const ASource: TNyxText;
  const ARoot: INyxPascalCompilerContext; const AProfile: INyxPascalCompilerContext = nil): INyxPascalResolution;
var LWorld: INyxPascalSemanticWorld; LSite: INyxPascalBindingSite; LWindow: TNyxPascalSpan;
  LQueries: INyxPascalExpressionQueries; LAccess: TNyxPascalMemberAccess;
begin
  LWorld := WorldFor(ASource,ARoot,AProfile);
  LSite := CaseSite(LWorld,ACase,LWindow);
  Supports(LWorld,INyxPascalExpressionQueries,LQueries);
  Result := LQueries.ExpressionType(LSite,LWindow,LAccess);
end;

procedure RefusalContract;
var LCase: TFloatCase; LAnswer: INyxPascalResolution; LNeeds: INyxPascalCompilerRequirements;
  LSource: TNyxText; LProfile: INyxPascalCompilerContext;
begin
  LCase := GCases[0];
  LAnswer := Query(LCase,Companion(LCase),nil);
  Check(LAnswer.Status <> pbsBound,'Unqualified symbolic floating names cannot grant arithmetic semantics');
  LSource := TNyxText(StringReplace(String(Companion(LCase)),'{$excessprecision off}','',[]));
  LAnswer := Query(LCase,LSource,Root);
  Check((LAnswer.Status = pbsBound) and (TypeName(LAnswer.BoundType) = TNyxText('Extended')),
    'The same authored Delphi header now has an independently qualified precision effect');
  LSource := TNyxText(StringReplace(String(LSource),'{$mode delphi}','',[]));
  LAnswer := Query(LCase,LSource,Root);
  Check((LAnswer.Status = pbsUnresolved) and (LAnswer.Issue = pbiCompilerContext) and
    (LAnswer.RelatedName = TNyxText('EXCESSPRECISION')),
    'Without an authored mode or captured request, startup precision remains an explicit context gap');
  LCase := GCases[6];
  LSource := TNyxText(StringReplace(String(Companion(LCase)),'{$excessprecision off}','',[]));
  LAnswer := Query(LCase,LSource,Root);
  Check((LAnswer.Status = pbsBound) and (TypeName(LAnswer.BoundType) = TNyxText('Single')),
    'A precision-independent mixed operation does not invent or demand a default');
  LCase := GCases[20];
  LAnswer := Query(LCase,Companion(LCase),Root('Extended'));
  LNeeds := CaptureNyxPascalRequirements(LAnswer);
  Check((LAnswer.Status = pbsUnresolved) and (LNeeds.RequirementCount = 1) and
    (LNeeds.RequirementAt(0).Reference.Name = TNyxText('extended')) and
    (LNeeds.RequirementAt(0).ContextState = -1),
    'A missing canonical result retains the existing exact compiler-root requirement');
  LCase := GCases[12];
  LSource := TNyxText(StringReplace(String(Companion(LCase)),'{$fputype X87}','{$fputype NONE}',[]));
  LAnswer := Query(LCase,LSource,Root);
  Check(LAnswer.Status <> pbsBound,'FPU NONE cannot admit real division');
  LSource := TNyxText(StringReplace(String(Companion(LCase)),'{$fputype X87}','',[]));
  LProfile := NewNyxPascalCompilerContext(ppcFPC,TNyxPascalCompilerVersion.Create(3,3,1),ppmDelphi)
    .CompleteDefines(True).WithSymbol(NyxPascalDefine('CPUI386'),ppdDefined);
  LAnswer := Query(LCase,LSource,Root,LProfile);
  Check((LAnswer.Status = pbsUnresolved) and (LAnswer.RelatedName = TNyxText('FPUTYPE')),
    'An unknown current selection cannot borrow a host instruction default');
  LSource := TNyxText(StringReplace(String(Companion(LCase)),'{$excessprecision off}',
    '{$excessprecision off}{$undef FPUX87}{$define FPUSSE2}',[]));
  LAnswer := Query(LCase,LSource,Root);
  Check((LAnswer.Status = pbsBound) and (TypeName(LAnswer.BoundType) = TNyxText('Extended')),
    'Authored selector macros cannot retarget division');
  LCase := GCases[0]; LCase.ExpressionText := 'F32 and F32';
  LAnswer := Query(LCase,Companion(LCase),Root);
  Check(LAnswer.Status <> pbsBound,'Floating operands do not gain ordinal bitwise operators');
  LCase.ExpressionText := '1.5 + 2.5';
  LAnswer := Query(LCase,Companion(LCase),Root);
  Check(LAnswer.Status <> pbsBound,'Real literal meaning stays explicit until its separate qualified value contract');
end;

procedure WorkContract;
var LWorld: INyxPascalSemanticWorld; LSite: INyxPascalBindingSite; LWindow: TNyxPascalSpan;
  LQueries: INyxPascalExpressionQueries; LBudget: INyxPascalAnalysisBudget;
  LAnswer, LRetained: INyxPascalResolution; LAccess: TNyxPascalMemberAccess;
  LAt, LFit: Integer; LCancelled: INyxCancellationScope; LRejected: Boolean;
begin
  LWorld := WorldFor({$ifdef PAS2JS}Companion(GCases[1]){$else}GFixture{$endif},Root);
  LSite := CaseSite(LWorld,GCases[1],LWindow);
  Supports(LWorld,INyxPascalExpressionQueries,LQueries);
  LFit := -1;
  for LAt := 1 to 20000 do
  begin
    LAnswer := nil;
    LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      try
        LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess,LBudget);
        Check((LAnswer.Status = pbsBound) and (TypeName(LAnswer.BoundType) = TNyxText('Double')),
          'The first sufficient work allowance publishes the complete floating result');
        LRetained := LAnswer; LFit := LAt;
      except
        on ENyxPascalAnalysisWorkLimit do
          Check((LAnswer = nil) and (LBudget.Receipt.Total <= LAt),'Every smaller allowance refuses atomically');
      end;
    finally LBudget.Retire; end;

    if LFit > 0 then Break;
  end;
  Check(LFit > 1,'Floating operations have a bounded positive first fit');
  Check(TypeName(LRetained.BoundType) = TNyxText('Double'),'Published results retain no retired execution scope');
  WriteLn('floating expression work / first fit=',LFit,' every smaller allowance refused');
  LCancelled := NewNyxCancellationScope; LCancelled.Cancel;
  LAnswer := nil; LRejected := False;
  try LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess,LCancelled);
  except on ENyxPascalAnalysisCancelled do LRejected := True; end;
  Check(LRejected and (LAnswer = nil),'Cancellation publishes no partial arithmetic result');
end;

function ChainWork(ACount: Integer): Integer;
var LCase: TFloatCase; LAt: Integer; LWorld: INyxPascalSemanticWorld;
  LSite: INyxPascalBindingSite; LWindow: TNyxPascalSpan; LQueries: INyxPascalExpressionQueries;
  LBudget: INyxPascalAnalysisBudget; LAnswer: INyxPascalResolution; LAccess: TNyxPascalMemberAccess;
begin
  LCase := GCases[12]; LCase.ExpressionText := 'I32';
  for LAt := 1 to ACount do LCase.ExpressionText := LCase.ExpressionText+' / I32';
  LWorld := WorldFor(Companion(LCase),Root); LSite := CaseSite(LWorld,LCase,LWindow);
  Supports(LWorld,INyxPascalExpressionQueries,LQueries);
  LBudget := NewNyxPascalAnalysisBudget(100000);
  try
    LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess,LBudget);
    Check((LAnswer.Status = pbsBound) and (TypeName(LAnswer.BoundType) = TNyxText('Extended')),
      'A division chain retains its single expression-level instruction selection');
    Result := LBudget.Receipt.Total;
  finally LBudget.Retire; end;
end;

procedure ScalingContract;
var LSmall, LLarge: Integer;
begin
  LSmall := ChainWork(20); LLarge := ChainWork(40);
  Check((LLarge > LSmall) and (LLarge < LSmall*3),'Doubling a flat chain does not cause quadratic lookahead work');
  WriteLn('floating chain work / 20=',LSmall,' 40=',LLarge);
end;

{$ifndef PAS2JS}
procedure TFloatingExpressionReader.Execute;
var LQueries: INyxPascalExpressionQueries; LAt: Integer; LAnswer: INyxPascalResolution;
  LAccess: TNyxPascalMemberAccess;
begin
  try
    Supports(World,INyxPascalExpressionQueries,LQueries);
    for LAt := 0 to 15 do
    begin
      LAnswer := LQueries.ExpressionType(Site,Window,LAccess);

      if (LAnswer.Status <> pbsBound) or (TypeName(LAnswer.BoundType) <> TNyxText('Double')) then
        raise Exception.Create('Concurrent source-activated result differs');
    end;
  except on E: Exception do ErrorText := E.ClassName+': '+E.Message; end;
end;

procedure ConcurrentContract;
var LThreads: array[0..3] of TFloatingExpressionReader; LAt: Integer;
  LWorld: INyxPascalSemanticWorld; LSite: INyxPascalBindingSite; LWindow: TNyxPascalSpan;
begin
  LWorld := WorldFor(GFixture,Root); LSite := CaseSite(LWorld,GCases[44],LWindow);
  for LAt := 0 to High(LThreads) do
  begin
    LThreads[LAt] := TFloatingExpressionReader.Create(True);
    LThreads[LAt].World := LWorld; LThreads[LAt].Site := LSite; LThreads[LAt].Window := LWindow;
    LThreads[LAt].Start;
  end;
  for LAt := 0 to High(LThreads) do LThreads[LAt].WaitFor;
  try
    for LAt := 0 to High(LThreads) do Check(LThreads[LAt].ErrorText = '',LThreads[LAt].ErrorText);
  finally for LAt := 0 to High(LThreads) do LThreads[LAt].Free; end;
  Check((NativeFloatingExpressionCalls = Length(GCases)) and (NativeFloatingKeepCalls = 1),
    'All source/concurrent queries invoke no witness body');
  WriteLn('native concurrent floating expression readers / 64 joined');
end;

procedure NativeWitness;
var LText: TNyxText; LAt: Integer; LStream: TFileStream;
begin
  LText := TNyxText(NativeFloatingExpressions);
  Write(LText);
  Check(NativeFloatingExpressionCalls = Length(GCases),'Every independent whole native observation executes once');
  Check(NativeFloatingKeepCalls = 1,'The nested generic body executes independently once');
  for LAt := 0 to High(GCases) do
    Check(Pos(GCases[LAt].LabelText+'='+CNames[GCases[LAt].Expected]+#10,LText) > 0,
      'Independent native result differs: '+String(GCases[LAt].LabelText));
  LStream := TFileStream.Create('tests/floating.expressions.fixture.pas',fmOpenRead or fmShareDenyWrite);
  try
    SetLength(GFixture,LStream.Size);

    if Length(GFixture) > 0 then LStream.ReadBuffer(GFixture[1],Length(GFixture));
  finally LStream.Free; end;
end;
{$endif}

begin
  try
    Cases;
    {$ifndef PAS2JS}NativeWitness;{$endif}
    ExpressionContract;
    RefusalContract;
    WorkContract;
    ScalingContract;
    {$ifndef PAS2JS}ConcurrentContract;{$endif}
    WriteLn('PASS ',GChecks,' floating expression checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ',E.ClassName,': ',E.Message);
      ExitCode := 1;
    end;
  end;
end.
