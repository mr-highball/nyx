{ Whole native expressions independently qualify portable numeric contracts.
  Queries inspect source without executing a body or borrowing work ownership. }
program nyx_pascal_numeric_expression_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses SysUtils, Classes, nyx.text, nyx.editing, nyx.scheduler, nyx.pascal.work,
  nyx.pascal.files, nyx.pascal.context, nyx.pascal.lexing, nyx.pascal.translation,
  nyx.pascal.syntax, nyx.pascal.syntax.translated, nyx.pascal.binding
  {$ifndef PAS2JS}, numeric.expressions.fixture{$endif};

type
  { A row ties an authored expression to the compiler's separate RTTI label.
    None retains an unsupported rule, never a fabricated successful result. }
  TNumericCase = record
    LabelText, ExpressionText: TNyxText;
    Expected: TNyxPascalBuiltin;
  end;
  {$ifndef PAS2JS}
  { Concurrent queries share immutable source/type ownership only. Main joins
    every reader before inspecting results; no query shares a mutable budget. }
  TNumericReader = class(TThread)
  public
    Shared: INyxPascalSemanticWorld;
    Site: INyxPascalBindingSite;
    Window: TNyxPascalSpan;
    ErrorText: String;
    procedure Execute; override;
  end;
  {$endif}

const
  CMode = '{$mode delphi}{$H+}{$codepage utf8}{$modeswitch implicitfunctionspecialization+}';
  CPortable = 'unit numeric.expressions.fixture;'+CMode+'interface '+
    'function ObserveNumeric<T>(const AValue:T):T;function ResetNumeric<T>(const AValue:T):T;'+
    'function NumericValue:LongInt;implementation end.';
  { Forty-two outer observations plus two nested ObserveNumeric calls. }
  CNativeInvocations = 44;
  CCases: array[0..41] of TNumericCase = (
    (LabelText:'constant-add';ExpressionText:'1+2';Expected:pbbShortInt),
    (LabelText:'constant-byte-boundary';ExpressionText:'127+1';Expected:pbbByte),
    (LabelText:'constant-word-boundary';ExpressionText:'32767+1';Expected:pbbWord),
    (LabelText:'constant-long-boundary';ExpressionText:'2147483647+1';Expected:pbbLongWord),
    (LabelText:'constant-multiply';ExpressionText:'6*7';Expected:pbbShortInt),
    (LabelText:'constant-div';ExpressionText:'84 div 2';Expected:pbbShortInt),
    (LabelText:'constant-mod';ExpressionText:'85 mod 43';Expected:pbbShortInt),
    (LabelText:'constant-shift';ExpressionText:'1 shl 6';Expected:pbbShortInt),
    (LabelText:'constant-bitwise';ExpressionText:'$ff and $0f';Expected:pbbShortInt),
    (LabelText:'constant-not';ExpressionText:'not 1';Expected:pbbLongInt),
    (LabelText:'constant-real';ExpressionText:'1.5+2.5';Expected:pbbNone),
    (LabelText:'short-add';ExpressionText:'S8+S8';Expected:pbbLongInt),
    (LabelText:'byte-add';ExpressionText:'U8+U8';Expected:pbbLongWord),
    (LabelText:'small-add';ExpressionText:'S16+S16';Expected:pbbLongInt),
    (LabelText:'word-add';ExpressionText:'U16+U16';Expected:pbbLongWord),
    (LabelText:'small-byte';ExpressionText:'S8+U8';Expected:pbbLongInt),
    (LabelText:'long-add';ExpressionText:'S32+S32';Expected:pbbLongInt),
    (LabelText:'unsigned-add';ExpressionText:'U32+U32';Expected:pbbLongWord),
    (LabelText:'mixed-long';ExpressionText:'S32+U32';Expected:pbbInt64),
    (LabelText:'int64-long';ExpressionText:'S64+S32';Expected:pbbInt64),
    (LabelText:'int64-unsigned';ExpressionText:'S64+U64';Expected:pbbInt64),
    (LabelText:'qword-long';ExpressionText:'U64+S32';Expected:pbbQWord),
    (LabelText:'short-negative';ExpressionText:'-S8';Expected:pbbLongInt),
    (LabelText:'unsigned-negative';ExpressionText:'-U32';Expected:pbbLongInt),
    (LabelText:'byte-not';ExpressionText:'not U8';Expected:pbbByte),
    (LabelText:'byte-shift';ExpressionText:'U8 shl U8';Expected:pbbLongWord),
    (LabelText:'native-add';ExpressionText:'NI+NI';Expected:pbbLongInt),
    (LabelText:'native-unsigned';ExpressionText:'NU+NU';Expected:pbbNativeUInt),
    (LabelText:'single-add';ExpressionText:'F32+F32';Expected:pbbNone),
    (LabelText:'single-double';ExpressionText:'F32+F64';Expected:pbbNone),
    (LabelText:'integer-divide';ExpressionText:'S32/S32';Expected:pbbNone),
    (LabelText:'integer-multiply';ExpressionText:'S32*S32';Expected:pbbLongInt),
    (LabelText:'word-bitwise';ExpressionText:'U16 or U8';Expected:pbbWord),
    (LabelText:'runtime-div';ExpressionText:'U16 div U8';Expected:pbbLongWord),
    (LabelText:'runtime-mod';ExpressionText:'S64 mod S32';Expected:pbbInt64),
    (LabelText:'runtime-compare';ExpressionText:'S32 < U32';Expected:pbbBoolean),
    (LabelText:'function-barrier';ExpressionText:'ResetNumeric(1+2)+1';Expected:pbbLongInt),
    (LabelText:'returned-byte-add';ExpressionText:'ObserveNumeric(127+1)+1';Expected:pbbLongInt),
    (LabelText:'returned-small-add';ExpressionText:'ObserveNumeric((127+1)*2)+1';Expected:pbbLongInt),
    (LabelText:'routine-less';ExpressionText:'NumericValue < U32';Expected:pbbBoolean),
    (LabelText:'routine-less-equal';ExpressionText:'NumericValue <= U32';Expected:pbbBoolean),
    (LabelText:'qualified-routine-less';ExpressionText:'numeric.expressions.fixture.NumericValue < U32';Expected:pbbBoolean));
  CNames: array[TNyxPascalBuiltin] of TNyxText = ('','Integer','Byte','Word','SmallInt',
    'ShortInt','LongInt','LongWord','Int64','QWord','NativeInt','NativeUInt','Boolean',
    'ByteBool','WordBool','LongBool','Char','AnsiChar','WideChar','Single','Double',
    'Extended','Real','Currency','Pointer','AnsiString','UTF8String','UnicodeString','WideString','ShortString','Comp');

var
  GChecks: Integer;
  GFixture: TNyxText;
  {$ifndef PAS2JS}GObservations: TNyxText;{$endif}

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then raise Exception.Create(AReason);
end;

function Facts(AALUBits: Integer = 32; ANativeSizes: Boolean = True): INyxPascalCompilerContext;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,TNyxPascalCompilerVersion.Create(3,3,1),ppmDelphi)
    .CompleteDefines(True).WithOption('H',ppkTrue);

  if AALUBits <> 0 then Result := Result.WithSymbol(NyxPascalDefine('CPUINT'+TNyxText(IntToStr(AALUBits))),ppdDefined);

  if ANativeSizes then
  begin
    { Native uses actual independent widths; browser compiles controlled input
      without asserting browser execution or a browser-native Pascal width. }
    Result := NyxPascalWithCapturedSize(Result,NyxPascalSizeReference('NativeInt'),
      NyxPascalSizeFact(pssKnown,ppmDelphi,ppkTrue,{$ifdef PAS2JS}4{$else}SizeOf(NativeInt){$endif}));
    Result := NyxPascalWithCapturedSize(Result,NyxPascalSizeReference('NativeUInt'),
      NyxPascalSizeFact(pssKnown,ppmDelphi,ppkTrue,{$ifdef PAS2JS}4{$else}SizeOf(NativeUInt){$endif}));
  end;
end;

function View(const AName, AText: TNyxText; const AContext: INyxPascalCompilerContext): INyxPascalTranslatedSyntax;
begin
  Result := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile(AName),AText),NyxPascalPhysicalFile(AName),AContext,
    TNyxPascalTranslationOptions.Defaults),TNyxPascalSyntaxOptions.Defaults);
end;

function WorldFor(const AExpression: TNyxText; const AContext: INyxPascalCompilerContext;
  const AExtra: TNyxText = ''): INyxPascalSemanticWorld;
begin
  Result := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults)
    .WithModule(View('numeric.expressions.fixture.pas',GFixture,AContext))
    .WithModule(View('consumer.pas','unit Consumer;'+CMode+'interface uses numeric.expressions.fixture;'+
      'type TDistinctNumeric=type LongInt;var S8:ShortInt;U8:Byte;S16:SmallInt;U16:Word;'+
      'S32:LongInt;U32:LongWord;S64:Int64;U64:QWord;NI:NativeInt;NU:NativeUInt;F32:Single;F64:Double;'+
      'DistinctNumber:TDistinctNumeric;implementation procedure Run;begin '+AExtra+AExpression+';end;end.',AContext));
end;

function Site(const AWorld: INyxPascalSemanticWorld; const AExpression: TNyxText;
  out AWindow: TNyxPascalSpan): INyxPascalBindingSite;
var
  LView: INyxPascalTranslatedSyntax;
  LText: TNyxText;
  LAt, LStart, LFinish: Integer;
begin
  LView := AWorld.ModuleAt(1);
  LText := NyxPascalOccurrenceLexical(LView.Source,0).Source;
  LAt := Pos(AExpression+';end;end.',LText);
  Check(LAt > 0,'The expression retains its physical occurrence');
  LStart := NyxTextScalarCount(Copy(LText,1,LAt-1));
  LFinish := LStart+NyxTextScalarCount(AExpression);
  LStart := LView.VirtualAt(0,LStart);
  LFinish := LView.VirtualAt(0,LFinish);
  Check((LStart >= 0) and (LFinish > LStart),'Its entire projection remains available');
  AWindow := NyxPascalSpan(LStart,LFinish-LStart);
  Result := AWorld.At(1,LView.Syntax.ScopeAt(LStart),LFinish);
end;

function Resolve(const AExpression: TNyxText; const AContext: INyxPascalCompilerContext;
  const AExtra: TNyxText = ''): INyxPascalResolution;
var
  LWorld: INyxPascalSemanticWorld;
  LSite: INyxPascalBindingSite;
  LWindow: TNyxPascalSpan;
  LQueries: INyxPascalExpressionQueries;
  LAccess: TNyxPascalMemberAccess;
begin
  LWorld := WorldFor(AExpression,AContext,AExtra);
  LSite := Site(LWorld,AExpression,LWindow);
  Supports(LWorld,INyxPascalExpressionQueries,LQueries);
  Result := LQueries.ExpressionType(LSite,LWindow,LAccess);
end;

procedure Contract(const AExpression: TNyxText; AExpected: TNyxPascalBuiltin);
var
  LAnswer: INyxPascalResolution;
  LLiteral: INyxPascalLiteralType;
begin
  LAnswer := Resolve(AExpression,Facts);

  if AExpected = pbbNone then
    Check((LAnswer.Status <> pbsBound) and (LAnswer.BoundType = nil),'Unsupported numeric meaning stays explicit: '+String(AExpression))
  else
  begin
    Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType <> nil),
      'Numeric result must bind: '+String(AExpression)+' / '+String(LAnswer.MessageText));
    Check(LAnswer.BoundType.Builtin = AExpected,'Exact numeric result: '+String(AExpression)+' / '+
      String(CNames[LAnswer.BoundType.Builtin])+' expected '+String(CNames[AExpected]));
    Check(not Supports(LAnswer.BoundType,INyxPascalLiteralType,LLiteral),'An expression is not misreported as a source literal');
  end;
end;

procedure NumericContracts;
var
  LAt: Integer;
begin
  for LAt := 0 to High(CCases) do
  begin
    {$ifndef PAS2JS}
    Check(Pos(CCases[LAt].LabelText+'=',GObservations) > 0,'Every rule retains its native observation');

    if CCases[LAt].Expected <> pbbNone then
      Check(Pos(CCases[LAt].LabelText+'='+CNames[CCases[LAt].Expected]+#10,GObservations) > 0,
        'The independent compiler confirms '+String(CCases[LAt].LabelText));
    {$endif}
    Contract(CCases[LAt].ExpressionText,CCases[LAt].Expected);
    Contract('ObserveNumeric('+CCases[LAt].ExpressionText+')',CCases[LAt].Expected);
  end;
  Contract('(1+2)*14',pbbShortInt);
  Contract('ObserveNumeric(1+2)+1',pbbLongInt);
  Contract('ObserveNumeric(127+1)+1',pbbLongInt);
  Contract('DistinctNumber+S32',pbbNone);
  Contract('1 div 0',pbbNone);
  Contract('1 shl -1',pbbNone);
  Contract('1+',pbbNone);
  Contract('18446744073709551615+1',pbbNone);
  Contract('NumericValue<LongInt>()',pbbNone);
end;

procedure ContextContracts;
var
  LAnswer: INyxPascalResolution;
  LFacts: INyxPascalCompilerContext;
begin
  LAnswer := Resolve('S8+S8',Facts(0));
  Check((LAnswer.Status = pbsUnresolved) and (LAnswer.Issue = pbiCompilerContext),'Missing ALU facts never borrow the analyzer width');
  LFacts := Facts.WithSymbol(NyxPascalDefine('CPUINT64'),ppdDefined);
  LAnswer := Resolve('S8+S8',LFacts);
  Check((LAnswer.Status = pbsUnresolved) and (LAnswer.Issue = pbiCompilerContext),'Contradictory initial ALU claims refuse');
  LAnswer := Resolve('NI+NI',Facts(32,False));
  Check((LAnswer.Status = pbsUnresolved) and (LAnswer.Issue = pbiCompilerContext),'Native widths require separate primitive evidence');
  LAnswer := Resolve('S8+S8',Facts,'{$undef CPUINT32}{$define CPUINT64}');
  Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType.Builtin = pbbLongInt),'Authored defines cannot change the compiler ALU width');
end;

procedure OwnershipAndWork;
const CExpression = 'ObserveNumeric((127+1)*2)+1';
var
  LWorld: INyxPascalSemanticWorld;
  LSite: INyxPascalBindingSite;
  LWindow: TNyxPascalSpan;
  LQueries: INyxPascalExpressionQueries;
  LAccess: TNyxPascalMemberAccess;
  LAnswer, LRetained: INyxPascalResolution;
  LBudget: INyxPascalAnalysisBudget;
  LCancel: INyxCancellationScope;
  LAt, LFit: Integer;
  LRefused: Boolean;
begin
  LWorld := WorldFor(CExpression,Facts);
  LSite := Site(LWorld,CExpression,LWindow);
  Supports(LWorld,INyxPascalExpressionQueries,LQueries);
  LFit := -1;
  for LAt := 1 to 20000 do
  begin
    LAnswer := nil;
    LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      try
        LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess,LBudget);
        Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType.Builtin = pbbLongInt),'First fit publishes the whole numeric result');
        LFit := LAt;
        LRetained := LAnswer;
      except
        on ENyxPascalAnalysisWorkLimit do
          Check((LAnswer = nil) and (LBudget.Receipt.Total <= LAt),'Every smaller allowance refuses atomically');
      end;
    finally
      LBudget.Retire;
    end;

    if LFit > 0 then Break;
  end;
  Check(LFit > 1,'Numeric inference has a finite positive first fit');
  WriteLn('numeric expression work / first fit=',LFit,' every smaller allowance refused');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LAnswer := nil;
  LRefused := False;
  try
    LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess,LCancel);
  except
    on ENyxPascalAnalysisCancelled do LRefused := True;
  end;
  Check(LRefused and (LAnswer = nil),'Cancellation publishes no partial numeric proof');
  Check((LRetained.BoundType.Builtin = pbbLongInt) and (LRetained.BoundType.World = LWorld),
    'A result remains exact after budget/query retirement');
end;

{$ifndef PAS2JS}
procedure TNumericReader.Execute;
var
  LQueries: INyxPascalExpressionQueries;
  LAnswer: INyxPascalResolution;
  LAccess: TNyxPascalMemberAccess;
begin
  try
    Supports(Shared,INyxPascalExpressionQueries,LQueries);
    LAnswer := LQueries.ExpressionType(Site,Window,LAccess);

    if (LAnswer.Status <> pbsBound) or (LAnswer.BoundType.Builtin <> pbbLongInt) then
      raise Exception.Create('Concurrent numeric result differs');
  except
    on E: Exception do ErrorText := E.ClassName+': '+E.Message;
  end;
end;

procedure Concurrent;
const CExpression = 'ObserveNumeric((127+1)*2)+1';
var
  LWorld: INyxPascalSemanticWorld;
  LSite: INyxPascalBindingSite;
  LWindow: TNyxPascalSpan;
  LReaders: array[0..63] of TNumericReader;
  LAt: Integer;
begin
  LWorld := WorldFor(CExpression,Facts);
  LSite := Site(LWorld,CExpression,LWindow);
  for LAt := 0 to High(LReaders) do
  begin
    LReaders[LAt] := TNumericReader.Create(True);
    LReaders[LAt].Shared := LWorld;
    LReaders[LAt].Site := LSite;
    LReaders[LAt].Window := LWindow;
  end;
  try
    for LAt := 0 to High(LReaders) do LReaders[LAt].Start;
    for LAt := 0 to High(LReaders) do LReaders[LAt].WaitFor;
    for LAt := 0 to High(LReaders) do Check(LReaders[LAt].ErrorText = '',LReaders[LAt].ErrorText);
  finally
    for LAt := 0 to High(LReaders) do LReaders[LAt].Free;
  end;
  WriteLn('native concurrent numeric readers / 64 joined');
end;

procedure NativeWitness;
var
  LStream: TFileStream;
begin
  {$ifndef CPUINT32}
  raise Exception.Create('The retained RTTI matrix requires its independently compiled CPUINT32 witness');
  {$endif}
  RunNumericWitness;
  GObservations := TNyxText(NumericObservations);
  Write(GObservations);
  Check(NumericInvocations = CNativeInvocations,'Every outer and nested native expression executed independently');
  Check(NumericBarrierValue = 1,'ResetNumeric changes the input value');
  Check(NumericSourceInvocations = 3,'The whole native routine comparisons invoke real source bodies');
  LStream := TFileStream.Create('tests/numeric.expressions.fixture.pas',fmOpenRead or fmShareDenyWrite);
  try
    SetLength(GFixture,LStream.Size);

    if Length(GFixture) > 0 then LStream.ReadBuffer(GFixture[1],Length(GFixture));
  finally
    LStream.Free;
  end;
  WriteLn('native numeric witness / invocations=',NumericInvocations,' barrier=',NumericBarrierValue);
end;
{$endif}

begin
  try
    GFixture := CPortable;
    {$ifndef PAS2JS}NativeWitness;{$endif}
    NumericContracts;
    ContextContracts;
    OwnershipAndWork;
    {$ifndef PAS2JS}
    Concurrent;
    Check(NumericInvocations = CNativeInvocations,'Queries never execute source/generic bodies');
    Check(NumericSourceInvocations = 3,'Queries never execute routine comparison bodies');
    {$endif}
    WriteLn('PASS ',GChecks,' numeric expression checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ',E.ClassName,': ',E.Message);
      ExitCode := 1;
    end;
  end;
end.
