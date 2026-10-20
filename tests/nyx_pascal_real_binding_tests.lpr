{ Whole native inputs qualify literal inference with an explicit parser-profile
  contract. Source decimals, parser-rounded values and primitive identities are
  inspected separately; no query executes a witness or infers a host format. }
program nyx_pascal_real_binding_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses SysUtils, Classes, nyx.text, nyx.editing, nyx.scheduler, nyx.pascal.work,
  nyx.pascal.values, nyx.pascal.reals, nyx.pascal.context, nyx.pascal.files,
  nyx.pascal.translation, nyx.pascal.lexing, nyx.pascal.syntax, nyx.pascal.syntax.translated,
  nyx.pascal.binding, nyx.pascal.requirements, nyx.pascal.conditions, nyx.pascal.constants
  {$ifndef PAS2JS}, real.literals.fixture, binary.real.fixture{$endif};

{$ifndef PAS2JS}
type
  { Each reader owns its query and work allowance over one shared immutable
    source world. It publishes only text after Main joins every worker. }
  TRealBindingReader = class(TThread)
  public
    World: INyxPascalSemanticWorld;
    Site: INyxPascalBindingSite;
    Window: TNyxPascalSpan;
    ErrorText: String;
    procedure Execute; override;
  end;
{$endif}

var GChecks: Integer;

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
const CNames: array[0..2] of TNyxText = ('Single','Double','Extended');
  CSizes: array[0..2] of Integer = (4,8,10);
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
    .WithFile(NyxPascalPhysicalFile('source.pas'),ASource),NyxPascalPhysicalFile('source.pas'),LContext,LBase,
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
  else Result := '';
  end;
end;

procedure PolicyContract;
var LBase, L80, L64, LCapture: INyxPascalCompilerContext;
begin
  LBase := Facts(pbpUnknown); L80 := Facts; L64 := Facts(pbpBinary64);
  Check(not NyxPascalRealParser(LBase).Known,'No host or target observation supplies a parser algorithm');
  Check(not LBase.SameFacts(L80) and not L80.SameFacts(L64),'Parser algorithm/precision participate in exact context identity');
  LCapture := CaptureNyxPascalCompilerContext(L80);
  Check(LCapture.SameFacts(L80),'Capture retains explicit parser-profile evidence');
  Check(NyxPascalRealParser(L80.InMode(ppmObjFPC)).SamePolicy(NyxPascalRealParser(L80)),
    'Source mode changes retain the compiler-parser contract');
  Check(NyxPascalRealParser(L80.UnknownState).SamePolicy(NyxPascalRealParser(L80)),
    'Unknown source effects do not erase immutable initial compiler metadata');
  Check(NyxPascalRealParser(RestoreNyxPascalLocalSettings(L80,L64)).Precision = pbpX87Extended80,
    'Local restore cannot install a saved context parser from another profile');
end;

procedure ControlledContract;
var LSource: TNyxText; LAnswer: INyxPascalResolution;
  LLiteral: INyxPascalLiteralType; LReal: INyxPascalRealLiteral;
  LNeeds: INyxPascalCompilerRequirements;
  LProfile: INyxPascalCompilerContext;
begin
  LSource := Companion('1.25'); LAnswer := Query(LSource,'Observe(','1.25',Facts,Root);
  Check((LAnswer.Status = pbsBound) and (TypeName(LAnswer.BoundType) = 'Single'),
    'Explicit minimum32 and parser80 choose the compiler-root Single');
  Check(Supports(LAnswer.BoundType,INyxPascalLiteralType,LLiteral) and (LLiteral.LiteralKind = plkReal) and
    (LLiteral.Value.Kind = pvvUnknown),'Integer/Boolean evaluators receive no false real constant value');
  Check(Supports(LAnswer.BoundType,INyxPascalRealLiteral,LReal) and
    (LReal.SourceValue.Spelling = '1.25') and (LReal.SourceValue.Coefficient = '125') and
    (LReal.ParsedValue.Significand = '101') and (LReal.ParsedValue.BinaryExponent = -2),
    'Specialized literal metadata preserves exact source and parser-rounded values separately');
  LSource := Companion('1.00000000000000000000001');
  LAnswer := Query(LSource,'Observe(','1.00000000000000000000001',Facts,Root);
  Check((LAnswer.Status = pbsBound) and (TypeName(LAnswer.BoundType) = 'Single'),
    'A rounded parser result can fit Single despite nonrepresentable exact source');
  LSource := Companion('0.1','{$minfpconstprec 64}');
  LAnswer := Query(LSource,'Observe(','0.1',Facts,Root);
  Check((LAnswer.Status = pbsBound) and (TypeName(LAnswer.BoundType) = 'Extended'),
    'Parser80 recurring decimals retain the target best-real result');
  LAnswer := Query(LSource,'Observe(','0.1',Facts(pbpBinary64),Root);
  Check((LAnswer.Status = pbsBound) and (TypeName(LAnswer.BoundType) = 'Double'),
    'An explicitly different parser profile changes meaning without changing target defines');
  LSource := Companion('Keep(1.25)');
  LAnswer := Query(LSource,'Observe(','Keep(1.25)',Facts,Root);
  Check((LAnswer.Status = pbsBound) and (TypeName(LAnswer.BoundType) = 'Single') and
    not Supports(LAnswer.BoundType,INyxPascalRealLiteral,LReal),
    'Implicit generic deduction consumes the specialized type and returns no argument-value proof');
  LSource := Companion('1.25'); LAnswer := Query(LSource,'Observe(','1.25',Facts(pbpUnknown),Root);
  Check((LAnswer.Status = pbsUnresolved) and (LAnswer.Issue = pbiCompilerContext) and
    (LAnswer.RelatedName = 'REALPARSER'),'Unknown parser behavior remains an explicit compiler-context gap');
  LAnswer := Query(LSource,'Observe(','1.25',Facts,Root('Single'));
  LNeeds := CaptureNyxPascalRequirements(LAnswer);
  Check((LAnswer.Status = pbsUnresolved) and (LNeeds.RequirementCount = 1) and
    (LNeeds.RequirementAt(0).Reference.Name = 'single'),
    'Missing selected primitive representation retains the existing exact typed requirement');
  LSource := Companion('1.25','');
  LAnswer := Query(LSource,'Observe(','1.25',Facts,Root);
  Check((LAnswer.Status = pbsUnresolved) and (LAnswer.RelatedName = 'MINFPCONSTPREC'),
    'A selected mode or parser contract cannot invent a startup minimum');
  LSource := Companion('1.25','{$minfpconstprec 32}{$fputype NONE}');
  LAnswer := Query(LSource,'Observe(','1.25',Facts,Root);
  Check((LAnswer.Status = pbsUnresolved) and (LAnswer.RelatedName = 'FPUTYPE'),
    'An inactive FPU cannot admit even an exact real literal');
  LSource := Companion('1.25');
  LProfile := NyxPascalWithConfiguredFPU(Facts,NyxPascalFPU('future'));
  LAnswer := Query(LSource,'Observe(','1.25',LProfile,Root);
  Check((LAnswer.Status = pbsUnresolved) and (LAnswer.RelatedName = 'FPUTYPE'),
    'Open selector atoms cannot fabricate supported FPC30301 target instructions');
  LAnswer := Query(LSource,'Observe(','1.25',Facts,Root('',False));
  Check((LAnswer.Status = pbsUnresolved) and (LAnswer.Issue = pbiCompilerContext),
    'A parser profile cannot invent an unobserved target primitive family');
  LSource := Companion('1.0e9999');
  LAnswer := Query(LSource,'Observe(','1.0e9999',Facts,Root);
  Check(LAnswer.Status <> pbsBound,'Mathematical overflow is not fabricated compiler literal admission');
  LSource := Companion('1.25 + 2.5'); LAnswer := Query(LSource,'Observe(','1.25 + 2.5',Facts,Root);
  Check(LAnswer.Status <> pbsBound,'Real constant operators do not acquire unqualified runtime rules');
  LSource := Companion('-(1.25)'); LAnswer := Query(LSource,'Observe(','-(1.25)',Facts,Root);
  Check(LAnswer.Status <> pbsBound,'Parenthesized constant negation retains separate folding qualification');
end;

function StorageIdentity(APrecision: TNyxPascalBinaryPrecision): TNyxText;
var LContext: INyxPascalCompilerContext; LFiles: INyxPascalFileCatalog;
  LView, LSystem: INyxPascalTranslatedSyntax; LWorld: INyxPascalSemanticWorld;
  LResult: INyxPascalConstantResult; LIdentity: INyxPascalConstantStorageIdentity;
begin
  LContext := NyxPascalWithCapturedSize(Facts(APrecision),NyxPascalSizeReference('Byte'),
    NyxPascalSizeFact(pssKnown,ppmDelphi,ppkTrue,1));
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
    'Source/storage identities retain the explicit parser contract');
  Result := LIdentity.StorageKey.Value;
end;

procedure StorageContract;
var LUnknown, L64, L80: TNyxText;
begin
  LUnknown := StorageIdentity(pbpUnknown); L64 := StorageIdentity(pbpBinary64);
  L80 := StorageIdentity(pbpX87Extended80);
  Check((LUnknown <> L64) and (LUnknown <> L80) and (L64 <> L80),
    'Parser behavior and precision cannot collide in structural context identity');
  Check((Pos('real-parser-policy-v1',LUnknown) = 0) and (Pos('real-parser-policy-v1',L80) > 0),
    'Unspecified contexts retain the prior key form while known parser policies are tagged');
end;

procedure WorkContract;
var LSource: TNyxText; LWorld: INyxPascalSemanticWorld; LSite: INyxPascalBindingSite;
  LQueries: INyxPascalExpressionQueries; LWindow: TNyxPascalSpan; LAccess: TNyxPascalMemberAccess;
  LAnswer, LRetained: INyxPascalResolution; LBudget: INyxPascalAnalysisBudget;
  LAt, LFit: Integer; LRejected: Boolean;
  LCancel: INyxCancellationScope;
begin
  LSource := Companion('0.1'); LWorld := WorldFor(LSource,Facts,Root);
  LSite := ExpressionSite(LWorld,LSource,'Observe(','0.1',LWindow);
  Supports(LWorld,INyxPascalExpressionQueries,LQueries); LFit := -1;
  for LAt := 1 to 20000 do
  begin
    LAnswer := nil; LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      try
        LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess,LBudget);
        Check((LAnswer.Status = pbsBound) and (TypeName(LAnswer.BoundType) = 'Extended'),
          'First sufficient allowance publishes complete literal typing');
        LRetained := LAnswer; LFit := LAt;
      except
        on ENyxPascalAnalysisWorkLimit do Check((LAnswer = nil) and (LBudget.Receipt.Total <= LAt),
          'Every smaller allowance refuses without a partial literal or exceeded parent budget');
      end;
    finally LBudget.Retire; end;

    if LFit > 0 then Break;
  end;
  Check((LFit > 1) and (LRetained.BoundType <> nil),'Published literal types retain no retired execution scope');
  WriteLn('real literal binding work / first fit=',LFit,' every smaller allowance refused');
  LSource := Companion('1.0e4000'); LWorld := WorldFor(LSource,Facts,Root,100000);
  LSite := ExpressionSite(LWorld,LSource,'Observe(','1.0e4000',LWindow);
  Supports(LWorld,INyxPascalExpressionQueries,LQueries); LAnswer := nil; LRejected := False;
  try LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess);
  except on ENyxPascalAnalysisWorkLimit do LRejected := True; end;
  Check(LRejected and (LAnswer = nil),'Absent an aggregate budget, the existing default semantic ceiling still bounds arithmetic');
  LCancel := NewNyxCancellationScope; LCancel.Cancel;
  LAnswer := nil; LRejected := False;
  try LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess,LCancel);
  except on ENyxPascalAnalysisCancelled do LRejected := True; end;
  Check(LRejected and (LAnswer = nil),'Plain cancellation publishes no partial literal result');
end;

{$ifndef PAS2JS}
procedure TRealBindingReader.Execute;
var LQueries: INyxPascalExpressionQueries; LAnswer: INyxPascalResolution;
  LReal: INyxPascalRealLiteral; LAccess: TNyxPascalMemberAccess; LBudget: INyxPascalAnalysisBudget;
begin
  LBudget := NewNyxPascalAnalysisBudget(10000);
  try
    try
      Supports(World,INyxPascalExpressionQueries,LQueries);
      LAnswer := LQueries.ExpressionType(Site,Window,LAccess,LBudget);

      if (LAnswer.Status <> pbsBound) or not Supports(LAnswer.BoundType,INyxPascalRealLiteral,LReal) or
        (LReal.SourceValue.Spelling <> '0.1') or (LReal.ParsedValue.Precision <> pbpX87Extended80) then
        ErrorText := 'Concurrent literal result differs';
    except on E: Exception do ErrorText := E.ClassName+': '+E.Message; end;
  finally LBudget.Retire; end;
end;

procedure ConcurrentContract;
var LReaders: array[0..63] of TRealBindingReader; LAt: Integer;
  LWorld: INyxPascalSemanticWorld; LSite: INyxPascalBindingSite; LWindow: TNyxPascalSpan; LSource: TNyxText;
begin
  LSource := Companion('0.1'); LWorld := WorldFor(LSource,Facts,Root);
  LSite := ExpressionSite(LWorld,LSource,'Observe(','0.1',LWindow);
  for LAt := 0 to High(LReaders) do LReaders[LAt] := nil;
  try
    for LAt := 0 to High(LReaders) do
    begin
      LReaders[LAt] := TRealBindingReader.Create(True); LReaders[LAt].World := LWorld;
      LReaders[LAt].Site := LSite; LReaders[LAt].Window := LWindow;
    end;
    for LAt := 0 to High(LReaders) do LReaders[LAt].Start;
    for LAt := 0 to High(LReaders) do LReaders[LAt].WaitFor;
    for LAt := 0 to High(LReaders) do Check(LReaders[LAt].ErrorText = '',LReaders[LAt].ErrorText);
  finally
    for LAt := 0 to High(LReaders) do LReaders[LAt].Free;
  end;
  Check((NativeRealLiteralCalls = 17) and (NativeBinaryRealCalls = 23),'Concurrent source queries invoke no native body');
  WriteLn('native concurrent real literal readers / 64 joined');
end;

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
var LRows, LFields: TStringList; LText, LSource, LMarker, LExpression, LName: TNyxText;
  LAt: Integer; LAnswer: INyxPascalResolution;
begin
  LRows := TStringList.Create; LFields := TStringList.Create;
  try
    LText := TNyxText(NativeRealLiterals); LRows.Text := String(LText);
    LSource := Load('tests/real.literals.fixture.pas');
    Check((LRows.Count = 17) and (NativeRealLiteralCalls = 17),'The complete seventeen-observation witness stays original');
    for LAt := 0 to LRows.Count-1 do
    begin
      LName := TNyxText(Copy(LRows[LAt],1,Pos('=',LRows[LAt])-1));
      LMarker := 'Emit('''+LName+''', Observe(';
      LExpression := '1.25';

      if LName = 'min32-recurring' then LExpression := '0.1';

      if LName = 'min64-recurring' then LExpression := '0.1';

      if LName = 'default-recurring' then LExpression := '0.1';

      if (LName = 'min32-over24bits') or (LName = 'min64-over24bits') then LExpression := '16777217.0';

      if LName = 'min64-over53bits' then LExpression := '9007199254740993.0';

      if LName = 'min32-negative' then LExpression := '-1.25';

      if LName = 'min32-positive' then LExpression := '+1.25';

      if LName = 'min32-exponent' then LExpression := '15.0e+1';

      if LName = 'min32-tiny' then LExpression := '1.0e-40';
      LAnswer := Query(LSource,LMarker,LExpression,Facts,Root);
      WriteLn('original real literal / ',String(LName),' status=',Ord(LAnswer.Status),' issue=',Ord(LAnswer.Issue),
        ' related=',String(LAnswer.RelatedName));
      Check((LAnswer.Status = pbsBound) and
        (TypeName(LAnswer.BoundType) = TNyxText(Copy(LRows[LAt],Pos('=',LRows[LAt])+1,Length(LRows[LAt])))),
        'Original native literal deduction differs: '+String(LName));
    end;
    LText := TNyxText(NativeBinaryRealObservations); LRows.Text := String(LText);
    LSource := Load('tests/binary.real.fixture.pas');
    Check((LRows.Count = 23) and (NativeBinaryRealCalls = 23),'The complete binary witness retains every actual literal');
    LFields.Delimiter := '|'; LFields.StrictDelimiter := True;
    for LAt := 0 to LRows.Count-1 do
    begin
      LFields.DelimitedText := LRows[LAt];
      LMarker := 'Emit('''+TNyxText(LFields[0])+''','''+TNyxText(LFields[1])+''',';
      LAnswer := Query(LSource,LMarker,TNyxText(LFields[1]),Facts,Root);
      WriteLn('original binary literal / ',LFields[0],' status=',Ord(LAnswer.Status),' issue=',Ord(LAnswer.Issue),
        ' related=',String(LAnswer.RelatedName));
      Check((LAnswer.Status = pbsBound) and (TypeName(LAnswer.BoundType) = TNyxText(LFields[2])),
        'Original compiler-rounded literal deduction differs: '+LFields[0]);
    end;
    Check((NativeRealLiteralCalls = 17) and (NativeBinaryRealCalls = 23),'Source queries invoke no native witness body');
  finally LFields.Free; LRows.Free; end;
end;
{$endif}

begin
  try
    PolicyContract;
    ControlledContract;
    StorageContract;
    WorkContract;
    {$ifndef PAS2JS}NativeSourceContract; ConcurrentContract;{$endif}
    WriteLn('PASS ',GChecks,' qualified real literal binding checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ',E.ClassName,': ',E.Message);
      ExitCode := 1;
    end;
  end;
end.
