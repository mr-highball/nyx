{ Whole native RTTI/overload evidence qualifies representation separately from
  exact declaration identity. Controlled roots exercise provenance and refusal. }
program nyx_pascal_floating_identity_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses SysUtils, Classes, nyx.text, nyx.editing, nyx.scheduler, nyx.pascal.work,
  nyx.pascal.context, nyx.pascal.files, nyx.pascal.translation,
  nyx.pascal.lexing, nyx.pascal.syntax, nyx.pascal.syntax.translated,
  nyx.pascal.binding, nyx.pascal.requirements
  {$ifndef PAS2JS}, floating.identity.fixture{$endif};

{$ifndef PAS2JS}
type
  { Each reader owns its resolution over shared immutable compiler/source data.
    Main joins every reader before inspecting outcomes or retiring the world. }
  TFloatingReader = class(TThread)
  public
    World: INyxPascalSemanticWorld;
    Site: INyxPascalBindingSite;
    Window: TNyxPascalSpan;
    ErrorText: String;
    procedure Execute; override;
  end;
{$endif}

const
  CMode = '{$mode delphi}{$H+}{$codepage utf8}{$modeswitch implicitfunctionspecialization+}';
  CPortable = 'unit floating.identity.fixture;'+CMode+'interface '+
    'type TDoubleAlias=Double;TDoubleDistinct=type Double;'+
    'function SameFloating<T>(var ALeft,ARight:T):T;implementation end.';
  CNames: array[0..4] of TNyxText = ('Single','Double','Extended','Currency','Comp');
  CBytes: array[0..4] of Integer = (4,8,10,8,8);
  CFormats: array[0..4] of TNyxPascalFloatingFormat =
    (pffBinary32,pffBinary64,pffX87Extended80,pffCurrency64,pffComp64);

var GChecks: Integer; GFixture: TNyxText;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then raise Exception.Create(AReason);
end;

function Root(ADoubleBytes: Integer = 8): INyxPascalCompilerContext;
var LAt: Integer;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,TNyxPascalCompilerVersion.Create(3,3,1),
    ppmFPC,pcsoSystemBootstrap).WithOption('H',ppkFalse).CompleteDefines(True)
    .WithSymbol(NyxPascalDefine('CPUI386'),ppdDefined);
  for LAt := 0 to High(CNames) do
  begin

    if (LAt = 1) and (ADoubleBytes = 0) then Continue;

    if LAt = 1 then
      Result := NyxPascalWithCapturedSize(Result,NyxPascalSizeReference(CNames[LAt]),
        NyxPascalSizeFact(pssKnown,ppmFPC,ppkFalse,ADoubleBytes))
    else
      Result := NyxPascalWithCapturedSize(Result,NyxPascalSizeReference(CNames[LAt]),
        NyxPascalSizeFact(pssKnown,ppmFPC,ppkFalse,CBytes[LAt]));
  end;
  Result := NyxPascalWithCapturedSize(Result,NyxPascalSizeReference('Real'),
    NyxPascalSizeFact(pssUndeclared,ppmFPC,ppkFalse));
end;

function Facts: INyxPascalCompilerContext;
begin
  Result := NyxPascalWithConfiguredFPU(NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3,3,1),ppmDelphi).WithOption('H',ppkTrue)
    .CompleteDefines(True).WithSymbol(NyxPascalDefine('CPUI386'),ppdDefined)
    .WithSymbol(NyxPascalDefine('FPUX87'),ppdDefined),NyxPascalFPU('X87'));
end;

function View(const AName, AText: TNyxText;
  const AContext: INyxPascalCompilerContext): INyxPascalTranslatedSyntax;
begin
  Result := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile(AName),AText),NyxPascalPhysicalFile(AName),AContext,
    TNyxPascalTranslationOptions.Defaults),TNyxPascalSyntaxOptions.Defaults);
end;

function WorldFor(const AExpression: TNyxText; const ARoot: INyxPascalCompilerContext;
  const AExtra: TNyxText = ''; const ASystemDeclarations: TNyxText = 'Real=type Double;'):
  INyxPascalSemanticWorld;
var LContext: INyxPascalCompilerContext;
begin
  LContext := NyxPascalWithBootstrapContext(Facts,ARoot);
  Result := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults.WithImplicitScopes(pispCompilerUnits))
    .WithModule(View('system.pas','unit System;{$mode fpc}interface type '+ASystemDeclarations+
      'implementation end.',LContext))
    .WithModule(View('objpas.pas','unit ObjPas;{$mode fpc}interface implementation end.',LContext))
    .WithModule(View('floating.identity.fixture.pas',GFixture,LContext))
    .WithModule(View('consumer.pas','unit Consumer;'+CMode+'interface uses floating.identity.fixture;'+
      'var F32:Single;F64:Double;F80:Extended;Money:Currency;Integral:Comp;'+
      'AliasValue:TDoubleAlias;DistinctValue:TDoubleDistinct;RealValue:Real;'+
      'implementation procedure Run;begin '+AExtra+AExpression+';end;end.',LContext));
end;

function Site(const AWorld: INyxPascalSemanticWorld; const AExpression: TNyxText;
  out AWindow: TNyxPascalSpan): INyxPascalBindingSite;
var LView: INyxPascalTranslatedSyntax; LText: TNyxText; LAt, LStart, LFinish: Integer;
begin
  LView := AWorld.ModuleAt(3);
  LText := NyxPascalOccurrenceLexical(LView.Source,0).Source;
  LAt := Pos(AExpression+';end;end.',LText);
  Check(LAt > 0,'The expression retains its complete physical occurrence');
  LStart := NyxTextScalarCount(Copy(LText,1,LAt-1));
  LFinish := LStart+NyxTextScalarCount(AExpression);
  LStart := LView.VirtualAt(0,LStart);
  LFinish := LView.VirtualAt(0,LFinish);
  Check((LStart >= 0) and (LFinish > LStart),'Both analysis boundaries retain the whole original expression');
  AWindow := NyxPascalSpan(LStart,LFinish-LStart);
  Result := AWorld.At(3,LView.Syntax.ScopeAt(LStart),LFinish);
end;

function Query(const AExpression: TNyxText; const ARoot: INyxPascalCompilerContext;
  const AExtra: TNyxText = ''; const ASystemDeclarations: TNyxText = 'Real=type Double;'):
  INyxPascalResolution;
var
  LWorld: INyxPascalSemanticWorld;
  LSite: INyxPascalBindingSite;
  LWindow: TNyxPascalSpan;
  LQueries: INyxPascalExpressionQueries;
  LAccess: TNyxPascalMemberAccess;
begin
  LWorld := WorldFor(AExpression,ARoot,AExtra,ASystemDeclarations);
  LSite := Site(LWorld,AExpression,LWindow);
  Supports(LWorld,INyxPascalExpressionQueries,LQueries);
  Result := LQueries.ExpressionType(LSite,LWindow,LAccess);
end;

function Leaf(const AType: INyxPascalSemanticType): INyxPascalSemanticType;
var LAt: Integer;
begin
  Result := AType;
  for LAt := 0 to 63 do
  begin

    if (Result = nil) or not (Result.Kind in [pstAlias,pstDistinct]) then Exit;
    Result := Result.Target;
  end;
  raise Exception.Create('Unexpected cyclic type evidence');
end;

procedure RepresentationContract;
const CVariables: array[0..4] of TNyxText = ('F32','F64','F80','Money','Integral');
var
  LAt: Integer;
  LAnswer: INyxPascalResolution;
  LFormat: INyxPascalFloatingIdentity;
  LIntrinsic: INyxPascalIntrinsicIdentity;
begin
  for LAt := 0 to High(CVariables) do
  begin
    LAnswer := Query(CVariables[LAt],Root);
    Check((LAnswer.Status = pbsBound) and Supports(LAnswer.BoundType,INyxPascalFloatingIdentity,LFormat),
      'A qualified compiler-root primitive carries typed floating representation');
    Check((LFormat.FloatingFormat = CFormats[LAt]) and (LFormat.StorageBytes = CBytes[LAt]),
      'The qualified compiler definition agrees with the independent native RTTI/storage witness');
    Check(Supports(LAnswer.BoundType,INyxPascalIntrinsicIdentity,LIntrinsic) and
      (LIntrinsic.IntrinsicOrigin = pioSystemBootstrap) and
      (LIntrinsic.IntrinsicReference.Name = NyxPascalSizeReference(CNames[LAt]).Name),
      'Representation retains its original root owner and nominal primitive reference');
  end;
  LAnswer := Query('F64',Root,'{$fputype SSE2}');
  Check((LAnswer.Status = pbsBound) and Supports(LAnswer.BoundType,INyxPascalFloatingIdentity,LFormat) and
    (LFormat.FloatingFormat = pffBinary64),'Source instruction selection cannot retarget the compiler-root Double definition');
end;

procedure NominalContract;
var LAnswer: INyxPascalResolution; LFormat: INyxPascalFloatingIdentity;
begin
  LAnswer := Query('AliasValue',Root);
  Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType.Kind = pstAlias),
    'An authored transparent alias remains visible in the public type shape');
  Check(Supports(Leaf(LAnswer.BoundType),INyxPascalFloatingIdentity,LFormat) and
    (LFormat.FloatingFormat = pffBinary64),'An alias reaches its original qualified representation');
  LAnswer := Query('DistinctValue',Root);
  Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType.Kind = pstDistinct) and
    (LAnswer.BoundType.Definition <> nil),'A distinct wrapper keeps its actual declaration');
  Check(Supports(Leaf(LAnswer.BoundType),INyxPascalFloatingIdentity,LFormat) and
    (LFormat.FloatingFormat = pffBinary64),'A distinct wrapper can share storage without sharing nominal identity');
  LAnswer := Query('RealValue',Root);
  Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType.Kind = pstDistinct) and
    (LAnswer.BoundType.Definition.Declaration.Name = TNyxText('Real')),
    'The RTL Real declaration is not replaced by a fabricated primitive alias');
  Check(Supports(Leaf(LAnswer.BoundType),INyxPascalFloatingIdentity,LFormat) and
    (LFormat.FloatingFormat = pffBinary64),'Real retains the distinct declaration and the underlying Double representation');
  LAnswer := Query('SameFloating<Double>(F64,AliasValue)',Root);
  Check((LAnswer.Status = pbsBound) and Supports(Leaf(LAnswer.BoundType),INyxPascalFloatingIdentity,LFormat),
    'Explicit generic application retains the qualified floating identity');
  LAnswer := Query('SameFloating(F64,F64)',Root);
  Check((LAnswer.Status = pbsBound) and Supports(Leaf(LAnswer.BoundType),INyxPascalFloatingIdentity,LFormat),
    'Repeated primitive arguments retain the same qualified identity');
  LAnswer := Query('SameFloating(F64,AliasValue)',Root);
  Check((LAnswer.Status = pbsBound) and Supports(Leaf(LAnswer.BoundType),INyxPascalFloatingIdentity,LFormat),
    'Repeated generic deduction accepts the transparent Double alias');
  LAnswer := Query('SameFloating(DistinctValue,DistinctValue)',Root);
  Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType.Kind = pstDistinct),
    'Repeated generic deduction retains a distinct actual type');
  LAnswer := Query('SameFloating(F64,DistinctValue)',Root);
  Check(LAnswer.Status <> pbsBound,'A shared representation cannot collapse independently declared generic arguments');
  LAnswer := Query('SameFloating(F64,RealValue)',Root);
  Check(LAnswer.Status <> pbsBound,'The RTL Real/Double distinction also survives deduction');
  LAnswer := Query('F32',Root,'','Real=type Double;Single=Double;');
  Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType.Kind = pstAlias) and
    Supports(Leaf(LAnswer.BoundType),INyxPascalFloatingIdentity,LFormat) and
    (LFormat.FloatingFormat = pffBinary64),'An authored System alias cannot borrow the pre-alias Single observation');
end;

procedure RefusalContract;
var
  LAnswer: INyxPascalResolution;
  LFormat: INyxPascalFloatingIdentity;
  LNeeds: INyxPascalCompilerRequirements;
  LRoot: INyxPascalCompilerContext;
begin
  LAnswer := Query('F64',Root(0));
  LNeeds := CaptureNyxPascalRequirements(LAnswer);
  Check((LAnswer.Status = pbsUnresolved) and (LAnswer.Issue = pbiCompilerContext) and
    (LNeeds.RequirementCount = 1) and (LNeeds.RequirementAt(0).Reference.Name = TNyxText('double')),
    'Missing primitive evidence remains an exact typed requirement, without an extra fixed probe');
  LAnswer := Query('F64',Root(4));
  Check((LAnswer.Status = pbsUnresolved) and (LAnswer.Issue = pbiCompilerContext),
    'A root measurement contradicting the qualified compiler definition refuses');
  LRoot := Root.WithSymbol(NyxPascalDefine('CPUI386'),ppdUndefined);
  LAnswer := Query('F64',LRoot);
  Check((LAnswer.Status = pbsBound) and not Supports(LAnswer.BoundType,INyxPascalFloatingIdentity,LFormat),
    'Equal name and byte size without a qualified target establish no floating format');
  LAnswer := Query('F64',nil);
  Check((LAnswer.Status = pbsBound) and not Supports(LAnswer.BoundType,INyxPascalFloatingIdentity,LFormat),
    'Ordinary symbolic System lookup cannot impersonate a compiler-root observation');
  LRoot := NyxPascalWithCapturedSize(Root,NyxPascalSizeReference('Double'),
    NyxPascalSizeFact(pssUndeclared,ppmFPC,ppkFalse));
  LAnswer := Query('F64',LRoot);
  Check(LAnswer.Status <> pbsBound,'An observed absent primitive cannot gain a representation from its spelling');
  LAnswer := Query('F64',Root,'','Real=type Double;Double=record Value:LongInt;end;');
  Check((LAnswer.Status = pbsBound) and not Supports(Leaf(LAnswer.BoundType),INyxPascalFloatingIdentity,LFormat),
    'A source declaration shadow keeps its shape instead of borrowing root Double metadata');
end;

procedure WorkContract;
var
  LWorld: INyxPascalSemanticWorld;
  LSite: INyxPascalBindingSite;
  LWindow: TNyxPascalSpan;
  LQueries: INyxPascalExpressionQueries;
  LAnswer, LRetained: INyxPascalResolution;
  LBudget: INyxPascalAnalysisBudget;
  LAccess: TNyxPascalMemberAccess;
  LFit, LAt: Integer;
  LCancel: INyxCancellationScope;
  LRejected: Boolean;
  LFormat: INyxPascalFloatingIdentity;
begin
  LWorld := WorldFor('F64',Root);
  LSite := Site(LWorld,'F64',LWindow);
  Supports(LWorld,INyxPascalExpressionQueries,LQueries);
  LFit := -1;
  for LAt := 1 to 20000 do
  begin
    LAnswer := nil;
    LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      try
        LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess,LBudget);
        Check((LAnswer.Status = pbsBound) and Supports(LAnswer.BoundType,INyxPascalFloatingIdentity,LFormat),
          'The first sufficient work allowance publishes the whole typed result');
        LRetained := LAnswer;
        LFit := LAt;
      except
        on ENyxPascalAnalysisWorkLimit do
          Check((LAnswer = nil) and (LBudget.Receipt.Total <= LAt),'Every smaller work allowance refuses atomically');
      end;
    finally
      LBudget.Retire;
    end;

    if LFit > 0 then Break;
  end;
  Check(LFit > 1,'Floating identity has a bounded positive first fit');
  Check(Supports(LRetained.BoundType,INyxPascalFloatingIdentity,LFormat) and
    (LFormat.FloatingFormat = pffBinary64),'Published representation retains no retired query budget');
  WriteLn('floating identity work / first fit=',LFit,' every smaller allowance refused');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LAnswer := nil;
  LRejected := False;
  try
    LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess,LCancel);
  except
    on ENyxPascalAnalysisCancelled do LRejected := True;
  end;
  Check(LRejected and (LAnswer = nil),'Cancellation publishes no partial type metadata');
end;

{$ifndef PAS2JS}
procedure NativeWitness;
var LText: TNyxText; LStream: TFileStream;
begin
  LText := TNyxText(NativeFloatingObservations);
  Write(LText);
  Check(NativeFloatingCalls = 9,'Nine whole native generic calls execute independently of semantic queries');
  Check(Pos('Single/ftSingle/4'+#10,LText) > 0,'Native single has its own RTTI format');
  Check(Pos('Double/ftDouble/8'+#10,LText) > 0,'Native double has its own RTTI format');
  Check(Pos('Extended/ftExtended/10'+#10,LText) > 0,'Native extended is independently qualified');
  Check(Pos('Currency/ftCurr/8'+#10,LText) > 0,'Currency remains different from a same-sized binary double');
  Check(Pos('Comp/ftComp/8'+#10,LText) > 0,'Comp remains different from Currency and binary double');
  Check(Pos('Real/ftDouble/8'+#10,LText) > 0,'Real shares representation without erasing its declared name');
  Check(Pos('TDoubleDistinct/ftDouble/8'+#10,LText) > 0,'A user distinct type also retains its declared identity');
  Check(Pos('routes=123'+#10,LText) > 0,'The compiler accepts independent Double/Real/distinct overloads');
  LStream := TFileStream.Create('tests/floating.identity.fixture.pas',fmOpenRead or fmShareDenyWrite);
  try
    SetLength(GFixture,LStream.Size);

    if Length(GFixture) > 0 then LStream.ReadBuffer(GFixture[1],Length(GFixture));
  finally LStream.Free; end;
end;

procedure TFloatingReader.Execute;
var LAt: Integer; LQueries: INyxPascalExpressionQueries; LAnswer: INyxPascalResolution;
  LAccess: TNyxPascalMemberAccess; LFormat: INyxPascalFloatingIdentity;
begin
  try
    Supports(World,INyxPascalExpressionQueries,LQueries);
    for LAt := 0 to 15 do
    begin
      LAnswer := LQueries.ExpressionType(Site,Window,LAccess);

      if (LAnswer.Status <> pbsBound) or not Supports(LAnswer.BoundType,INyxPascalFloatingIdentity,LFormat) or
        (LFormat.FloatingFormat <> pffBinary64) then raise Exception.Create('Concurrent floating identity differs');
    end;
  except on E: Exception do ErrorText := E.ClassName+': '+E.Message; end;
end;

procedure ConcurrentContract;
var LThreads: array[0..3] of TFloatingReader; LAt: Integer;
  LWorld: INyxPascalSemanticWorld; LSite: INyxPascalBindingSite; LWindow: TNyxPascalSpan;
begin
  LWorld := WorldFor('F64',Root);
  LSite := Site(LWorld,'F64',LWindow);
  for LAt := 0 to High(LThreads) do
  begin
    LThreads[LAt] := TFloatingReader.Create(True);
    LThreads[LAt].World := LWorld;
    LThreads[LAt].Site := LSite;
    LThreads[LAt].Window := LWindow;
    LThreads[LAt].Start;
  end;
  for LAt := 0 to High(LThreads) do LThreads[LAt].WaitFor;
  try
    for LAt := 0 to High(LThreads) do Check(LThreads[LAt].ErrorText = '',LThreads[LAt].ErrorText);
  finally
    for LAt := 0 to High(LThreads) do LThreads[LAt].Free;
  end;
  Check(NativeFloatingCalls = 9,'Concurrent source queries invoke no native witness body');
  WriteLn('native concurrent floating readers / 64 joined');
end;
{$endif}

begin
  try
    GFixture := CPortable;
    {$ifndef PAS2JS}NativeWitness;{$endif}
    RepresentationContract;
    NominalContract;
    RefusalContract;
    WorkContract;
    {$ifndef PAS2JS}ConcurrentContract;{$endif}
    WriteLn('PASS ',GChecks,' floating representation and identity checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ',E.ClassName,': ',E.Message);
      ExitCode := 1;
    end;
  end;
end.
