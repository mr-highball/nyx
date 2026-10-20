{ Exact decimal source evidence is independent of native RTTI observations.
  The whole native witness qualifies only the configured compiler's literal
  deductions and policy timing; this test never infers those through host floats. }
program nyx_pascal_real_value_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses SysUtils, Classes, nyx.text, nyx.scheduler, nyx.pascal.values,
  nyx.pascal.reals, nyx.pascal.context, nyx.pascal.files, nyx.pascal.translation,
  nyx.pascal.lexing, nyx.pascal.work, nyx.pascal.syntax, nyx.pascal.syntax.translated,
  nyx.pascal.binding, nyx.pascal.constants {$ifndef PAS2JS}, real.literals.fixture{$endif};

{$ifndef PAS2JS}
type
  { Each reader owns a fresh budget and detached value over immutable text.
    Main joins every thread before inspecting results or freeing any reader. }
  TRealReader = class(TThread)
  public
    ErrorText: String;
    procedure Execute; override;
  end;
{$endif}

const CHuge = '9999999999999999999999999999999999999999';
var GChecks: Integer; GFixture: TNyxText;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then raise Exception.Create(AReason);
end;

procedure Exact(const ASource, ACoefficient, AExponent: TNyxText);
var LValue: TNyxPascalRealValue;
begin
  LValue := NyxPascalRealToken(ASource);
  Check(LValue.Known and not LValue.Negative and (LValue.Spelling = ASource) and
    (LValue.Coefficient = ACoefficient) and (LValue.Exponent = AExponent),
    'Exact real value differs: '+String(ASource));
end;

procedure ValueContract;
var LValue, LOther, LUnknown: TNyxPascalRealValue; LExponent: Integer; LAt: Integer;
begin
  Exact('1.25','125','-2');
  Exact('123.4500','12345','-2');
  Exact('0001.2500E+0002','125','0');
  Exact('000.0012300e-0002','123','-7');
  Exact('123000e-2','123','1');
  Exact('0.0e'+CHuge,'0','0');
  Exact('000.000e-'+CHuge,'0','0');
  Exact('1e9007199254740993','1','9007199254740993');
  Exact('10e'+CHuge,'1','10000000000000000000000000000000000000000');
  Exact('100e-'+CHuge,'1','-9999999999999999999999999999999999999997');
  Exact('0.001e-'+CHuge,'1','-10000000000000000000000000000000000000002');
  Exact('1.0e-0','1','0');
  Exact('123456789012345678901234567890.0123456789',
    '1234567890123456789012345678900123456789','-10');
  LUnknown := TNyxPascalRealValue.Unknown;
  Check(not LUnknown.Known and not LUnknown.IsZero and not LUnknown.SameValue(LUnknown),
    'Unknown grants no source value or equality proof');
  Check(not LUnknown.Negated.Known and not LUnknown.TryExponent(LExponent) and (LExponent = 0),
    'Unknown sign/projection remains unknown');
  LValue := NyxPascalRealToken('001.2500'); LOther := NyxPascalRealToken('125e-2');
  Check(LValue.SameValue(LOther) and (LValue.Spelling <> LOther.Spelling),
    'Mathematical equality preserves distinct exact source spellings');
  Check(not LValue.Negated.SameValue(LOther) and LValue.Negated.Negated.SameValue(LOther),
    'Unary signs remain distinct from the unsigned token');
  LValue := NyxPascalRealToken('0.0'); LOther := LValue.Negated;
  Check(LOther.IsZero and LOther.Negative and LValue.SameValue(LOther) and not LValue.Negative,
    'Negative zero retains its sign without mutating the original value');
  LValue := NyxPascalRealToken('1.0e2147483647');
  Check(LValue.TryExponent(LExponent) and (LExponent = High(Integer)),'Maximum signed exponent projects exactly');
  LValue := NyxPascalRealToken('1.0e-2147483648');
  Check(LValue.TryExponent(LExponent) and (LExponent = Low(Integer)),'Minimum signed exponent projects without positive overflow');
  LValue := NyxPascalRealToken('1.0e2147483648');
  Check(not LValue.TryExponent(LExponent) and (LExponent = 0),'Positive exponent overflow leaves no partial projection');
  LValue := NyxPascalRealToken('1.0e-2147483649');
  Check(not LValue.TryExponent(LExponent) and (LExponent = 0),'Negative exponent overflow leaves no partial projection');
  { Scale shifts test exact equality over many normalizations, independently
    of the exponent's arbitrary precision and without an approximate oracle. }
  LValue := NyxPascalRealToken('1.25');
  for LAt := 0 to 80 do
  begin
    LOther := NyxPascalRealToken('125'+TNyxText(StringOfChar('0',LAt))+'e-'+TNyxText(IntToStr(LAt+2)));
    Check(LOther.SameValue(LValue),'Equivalent decimal scales keep exactly the same value');
  end;
end;

procedure MalformedContract;
const CBad: array[0..16] of TNyxText = ('','1','1.','1e','1e+','1e-','1.2e3x',
  '+1.0','-1.0','$1.0','1_000.0',' 1.0','1.0 ','NaN','Infinity','1..2','.25');
var LAt: Integer; LValue: TNyxPascalRealValue; LRejected: Boolean;
begin
  for LAt := 0 to High(CBad) do
  begin
    LValue := TNyxPascalRealValue.Unknown; LRejected := False;
    try LValue := NyxPascalRealToken(CBad[LAt]);
    except on ENyxPascalValue do LRejected := True; end;
    Check(LRejected and not LValue.Known,'Malformed/foreign real spelling publishes no value');
  end;
end;

function Facts: INyxPascalCompilerContext;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,TNyxPascalCompilerVersion.Create(3,3,1),ppmDelphi)
    .CompleteDefines(True);
end;

function Translate(const AText: TNyxText;
  const AContext: INyxPascalCompilerContext = nil): INyxPascalTranslation;
var LContext: INyxPascalCompilerContext;
begin
  LContext := AContext;

  if LContext = nil then LContext := Facts;
  Result := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('real.pas'),AText),NyxPascalPhysicalFile('real.pas'),
    LContext,TNyxPascalTranslationOptions.Defaults);
end;

function Marker(const ASource: INyxPascalTranslation; const AName: TNyxText): Integer;
var LAt: Integer;
begin
  for LAt := 0 to ASource.Count-1 do
  begin

    if ASource.Text(LAt) = AName then Exit(LAt);
  end;
  raise Exception.Create('Missing real policy marker: '+String(AName));
end;

function PolicyAt(const ASource: INyxPascalTranslation; const AName: TNyxText): TNyxPascalRealPrecisionRequest;
begin
  Result := NyxPascalCompilerPolicies(ASource.State(ASource.Token(Marker(ASource,AName)).State))
    .MinimumRealPrecisionRequested;
end;

procedure PolicyContract;
var LBase, L32, L64, LDefault, LCapture: INyxPascalCompilerContext;
  LSource, LPrefix: INyxPascalTranslation; LAt, LBefore: Integer;
  LFiles: INyxPascalFileCatalog;
begin
  LBase := Facts;
  Check(NyxPascalCompilerPolicies(LBase).MinimumRealPrecisionRequested = prprUnknown,
    'Selected mode supplies no minimum-constant startup observation');
  L32 := NyxPascalWithCompilerPolicies(LBase,TNyxPascalCompilerPolicies.Unknown.RequestMinimumRealPrecision(prprBinary32));
  L64 := NyxPascalWithCompilerPolicies(L32,NyxPascalCompilerPolicies(L32).RequestMinimumRealPrecision(prprBinary64));
  LDefault := NyxPascalWithCompilerPolicies(LBase,TNyxPascalCompilerPolicies.Unknown.RequestMinimumRealPrecision(prprDefault));
  Check(not L32.SameFacts(L64) and not L32.SameFacts(LDefault),'Distinct authored minimum requests keep distinct identities');
  LCapture := CaptureNyxPascalCompilerContext(L64);
  Check(LCapture.SameFacts(L64),'Complete capture retains minimum precision');
  Check(RestoreNyxPascalLocalSettings(L64,L32).SameFacts(L64),'Local restore retains the current module minimum');
  Check(NyxPascalCompilerPolicies(L64.InMode(ppmDelphi)).MinimumRealPrecisionRequested = prprBinary64,
    'Qualified MODE retains an existing module request without inventing a default');
  Check(NyxPascalCompilerPolicies(L64.UnknownState).MinimumRealPrecisionRequested = prprUnknown,
    'Unknown effects revoke minimum precision');
  Check(LCapture.SameFacts(L64) and LBase.SameFacts(Facts),'Fluent changes retain earlier immutable captures');
  LSource := Translate(GFixture);
  Check(LSource.Resolved,'The complete native literal fixture has a resolved source timeline');
  Check(PolicyAt(LSource,'HeaderMinimum') = prprBinary64,'Header MODE retains the earlier minimum request');
  Check(PolicyAt(LSource,'Minimum32') = prprBinary32,'Explicit minimum32 remains authored');
  Check(PolicyAt(LSource,'Minimum64') = prprBinary64,'Decimal leading zeroes retain the native setting');
  Check(PolicyAt(LSource,'AfterPop') = prprBinary64,'The module minimum survives POP');
  Check(PolicyAt(LSource,'MinimumDefault') = prprDefault,'DEFAULT retains its separate authored intent');
  LBefore := Marker(LSource,'''following-setting''');
  LAt := LBefore;
  while LAt < LSource.Count do
  begin

    if (LSource.PhysicalToken(LAt).Kind = ptkNumber) and LSource.PhysicalToken(LAt).RealNumber then Break;
    Inc(LAt);
  end;
  Check((LAt < LSource.Count) and (NyxPascalCompilerPolicies(LSource.State(LSource.Token(LAt).State))
    .MinimumRealPrecisionRequested = prprDefault),'The literal owns its setting before a following directive');
  LPrefix := NewNyxPascalTranslationPrefix(LSource,Marker(LSource,'Minimum32'),TNyxPascalTranslationOptions.Defaults);
  Check(NyxPascalCompilerPolicies(LPrefix.State(LPrefix.StateCount-1)).MinimumRealPrecisionRequested = prprBinary32,
    'Prefix capture excludes later minimum changes');
  for LAt := 0 to LSource.Count-1 do
  begin

    if (LSource.PhysicalToken(LAt).Kind <> ptkNumber) or not LSource.PhysicalToken(LAt).RealNumber then Continue;
    Check(NyxPascalRealToken(LSource.Text(LAt)).Known,'Every complete original native real token retains exact decimal evidence');
  end;
  LSource := Translate('{$minfpconstprec 32}{$ifdef ABSENT}{$minfpconstprec 64}{$endif}Marker');
  Check(LSource.Resolved and (PolicyAt(LSource,'Marker') = prprBinary32),'Inactive requests do not change the module minimum');
  LSource := Translate('{$minfpconstprec 32}{$if MAYBE}{$minfpconstprec 64}{$endif}Marker');
  Check(not LSource.Resolved and (PolicyAt(LSource,'Marker') = prprUnknown),'Uncertain participation revokes minimum knowledge');
  LSource := Translate('{$minfpconstprec 80}Marker');
  Check(not LSource.Resolved and (PolicyAt(LSource,'Marker') = prprUnknown),'Unsupported minimum80 remains a real compiler-policy error');
  LSource := Translate('{$minfpconstprec 64bits}Marker');
  Check(not LSource.Resolved,'Trailing identifier text is not a valid numeric prefix');
  LSource := Translate('{$minfpconstprec +32}Marker');
  Check(not LSource.Resolved,'A sign is not readid numeric spelling');
  LSource := Translate('{$minfpconstprec defaulted}Marker');
  Check(not LSource.Resolved,'DEFAULT is not an arbitrary identifier prefix');
  LSource := Translate('{$minfpconstprec 32}{$unknownpolicy}Marker');
  Check(not LSource.Resolved and (PolicyAt(LSource,'Marker') = prprUnknown),'Unknown source effects revoke the module request');
  LSource := Translate('{$minfpconstprec 32}Marker',NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3,2,2),ppmDelphi).CompleteDefines(True));
  Check(not LSource.Resolved,'Unqualified compiler versions cannot inherit the scanner rule');
  LSource := Translate('{$minfpconstprec 32}Marker',NewNyxPascalCompilerContext(ppcPas2JS,
    TNyxPascalCompilerVersion.Create(3,3,1),ppmDelphi).CompleteDefines(True));
  Check(not LSource.Resolved,'Another compiler family cannot inherit the literal policy');
  LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('main.pas'),
    '{$minfpconstprec 32}{$include value.inc}After').WithFile(NyxPascalPhysicalFile('value.inc'),
    '{$minfpconstprec 64}Included');
  LSource := NewNyxPascalTranslation(LFiles,NyxPascalPhysicalFile('main.pas'),Facts,
    TNyxPascalTranslationOptions.Defaults);
  Check(LSource.Resolved and (PolicyAt(LSource,'Included') = prprBinary64) and
    (PolicyAt(LSource,'After') = prprBinary64),'Includes share the module policy without retagging origins');
end;

function StorageIdentity(AValue: TNyxPascalRealPrecisionRequest): TNyxText;
var LContext: INyxPascalCompilerContext; LFiles: INyxPascalFileCatalog;
  LView, LSystem: INyxPascalTranslatedSyntax; LWorld: INyxPascalSemanticWorld;
  LResult: INyxPascalConstantResult; LIdentity: INyxPascalConstantStorageIdentity;
begin
  LContext := NyxPascalWithCapturedSize(NyxPascalWithCompilerPolicies(Facts.WithOption('H',ppkTrue),
    TNyxPascalCompilerPolicies.Unknown.RequestMinimumRealPrecision(AValue)),NyxPascalSizeReference('Byte'),
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
  Check(Supports(LResult,INyxPascalConstantStorageIdentity,LIdentity),'Storage retains exact minimum-precision context identity');
  Result := LIdentity.StorageKey.Value;
end;

procedure StorageContract;
var LUnknown, LDefault, L32, L64: TNyxText;
begin
  LUnknown := StorageIdentity(prprUnknown); LDefault := StorageIdentity(prprDefault);
  L32 := StorageIdentity(prprBinary32); L64 := StorageIdentity(prprBinary64);
  Check((LUnknown <> LDefault) and (LUnknown <> L32) and (LUnknown <> L64) and
    (LDefault <> L32) and (LDefault <> L64) and (L32 <> L64),
    'Every authored request has a distinct exact storage key');
  Check((Pos('minimum-real-precision-request-v1',LUnknown) = 0) and
    (Pos('minimum-real-precision-request-v1',LDefault) > 0),
    'Unspecified storage identity retains its old form and DEFAULT remains tagged');
end;

procedure PolicyWorkContract;
const CSource = '{$minfpconstprec 32}First{$push}{$minfpconstprec 64}Inside{$pop}After';
var LFiles: INyxPascalFileCatalog; LSource, LRetained: INyxPascalTranslation;
  LBudget: INyxPascalAnalysisBudget; LAt, LFit: Integer; LRejected: Boolean;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('budget.pas'),CSource);
  LFit := -1;
  for LAt := 1 to 10000 do
  begin
    LSource := nil; LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      try
        LSource := NewNyxPascalTranslation(LFiles,NyxPascalPhysicalFile('budget.pas'),Facts,
          TNyxPascalTranslationOptions.Defaults,LBudget);
        Check(LSource.Resolved and (PolicyAt(LSource,'After') = prprBinary64),
          'First sufficient policy allowance publishes complete module history');
        LFit := LAt; LRetained := LSource;
      except
        on ENyxPascalAnalysisWorkLimit do
          Check((LSource = nil) and (LBudget.Receipt.Total <= LAt),'Every smaller policy allowance refuses atomically');
      end;
    finally LBudget.Retire; end;

    if LFit > 0 then Break;
  end;
  Check((LFit > 1) and (PolicyAt(LRetained,'First') = prprBinary32),
    'Published policy retires its work owner while retaining earlier states');
  WriteLn('minimum real precision work / first fit=',LFit,' every smaller allowance refused');
  LRejected := False; LSource := nil;
  try LSource := NewNyxPascalTranslation(LFiles,NyxPascalPhysicalFile('budget.pas'),Facts,
    TNyxPascalTranslationOptions.Defaults.UpToStates(1));
  except on ENyxPascalTranslationLimit do LRejected := True; end;
  Check(LRejected and (LSource = nil),'State quota refusal exposes no partial module policy');
end;

procedure WorkContract;
const CSource = '000123.4500e-99999999999999999999999999999999999';
var LValue, LRetained: TNyxPascalRealValue; LBudget: INyxPascalAnalysisBudget;
  LCancel: INyxCancellationScope; LAt, LFit: Integer; LRejected: Boolean;
begin
  LFit := -1;
  for LAt := 1 to 10000 do
  begin
    LValue := TNyxPascalRealValue.Unknown; LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      try
        LValue := NyxPascalRealToken(CSource,LBudget);
        Check(LValue.Known and (LValue.Coefficient = '12345'),
          'The first sufficient allowance publishes the complete exact value');
        LFit := LAt; LRetained := LValue;
      except
        on ENyxPascalAnalysisWorkLimit do
          Check(not LValue.Known and (LBudget.Receipt.Total <= LAt),
            'Every smaller work allowance refuses without a partial real value');
      end;
    finally LBudget.Retire; end;

    if LFit > 0 then Break;
  end;
  Check((LFit > 1) and LRetained.Known and (LRetained.Spelling = CSource),
    'The value retains no retired execution-budget lease');
  WriteLn('real value work / first fit=',LFit,' every smaller allowance refused');
  LCancel := NewNyxCancellationScope; LCancel.Cancel;
  LValue := TNyxPascalRealValue.Unknown; LRejected := False;
  try LValue := NyxPascalRealToken(CSource,LCancel);
  except on ENyxPascalAnalysisCancelled do LRejected := True; end;
  Check(LRejected and not LValue.Known,'Cancellation publishes no partial decimal evidence');
end;

{$ifndef PAS2JS}
procedure TRealReader.Execute;
var LBudget: INyxPascalAnalysisBudget; LValue: TNyxPascalRealValue;
begin
  LBudget := NewNyxPascalAnalysisBudget(10000);
  try
    try
      LValue := NyxPascalRealToken('0001.2500E+0002',LBudget);

      if not LValue.Known or (LValue.Coefficient <> '125') or (LValue.Exponent <> '0') then
        ErrorText := 'Concurrent exact decimal differs';
    except on E: Exception do ErrorText := E.ClassName+': '+E.Message; end;
  finally LBudget.Retire; end;
end;

procedure ConcurrentContract;
var LReaders: array[0..63] of TRealReader; LAt: Integer;
begin
  for LAt := 0 to High(LReaders) do LReaders[LAt] := nil;
  try
    for LAt := 0 to High(LReaders) do LReaders[LAt] := TRealReader.Create(True);
    for LAt := 0 to High(LReaders) do LReaders[LAt].Start;
    for LAt := 0 to High(LReaders) do LReaders[LAt].WaitFor;
    for LAt := 0 to High(LReaders) do Check(LReaders[LAt].ErrorText = '',LReaders[LAt].ErrorText);
  finally
    for LAt := 0 to High(LReaders) do LReaders[LAt].Free;
  end;
  WriteLn('native concurrent real value readers / 64 joined');
end;

procedure NativeWitness;
const CExpected: array[0..16] of TNyxText = ('header-64=Double','min32-exact=Single',
  'min32-recurring=Extended','min32-over24bits=Extended','min32-negative=Single',
  'min32-positive=Single','min32-exponent=Single','min32-tiny=Extended',
  'min64-exact=Double','min64-recurring=Extended','min64-over24bits=Double',
  'min64-over53bits=Extended','after-pop=Double','default-exact=Single',
  'default-recurring=Extended','following-setting=Single','following-literal=Double');
var LText: TNyxText; LStream: TFileStream; LAt: Integer;
begin
  LText := TNyxText(NativeRealLiterals); Write(LText);
  Check(NativeRealLiteralCalls = 17,'The whole independent native witness executes every observation once');
  for LAt := 0 to High(CExpected) do
    Check(Pos(CExpected[LAt]+#10,LText) > 0,'Independent native deduction differs: '+String(CExpected[LAt]));
  LStream := TFileStream.Create('tests/real.literals.fixture.pas',fmOpenRead or fmShareDenyWrite);
  try
    SetLength(GFixture,LStream.Size);

    if Length(GFixture) > 0 then LStream.ReadBuffer(GFixture[1],Length(GFixture));
  finally LStream.Free; end;
end;
{$endif}

begin
  try
    GFixture := '{$minfpconstprec 64}unit U;{$mode delphi}interface implementation '+
      'procedure HeaderMinimum;begin end;{$minfpconstprec 32}procedure Minimum32;begin end;'+
      '{$push}{$minfpconstprec 00064}procedure Minimum64;begin end;{$pop}procedure AfterPop;begin end;'+
      '{$minfpconstprec default explanatory text}procedure MinimumDefault;begin '+
      'Emit(''following-setting'',Observe(1.25 {$minfpconstprec 64}));end;end.';
    {$ifndef PAS2JS}NativeWitness;{$endif}
    ValueContract;
    MalformedContract;
    PolicyContract;
    StorageContract;
    PolicyWorkContract;
    WorkContract;
    {$ifndef PAS2JS}ConcurrentContract; Check(NativeRealLiteralCalls = 17,'Value/source reads invoke no native body');{$endif}
    WriteLn('PASS ',GChecks,' exact real value and minimum precision checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ',E.ClassName,': ',E.Message);
      ExitCode := 1;
    end;
  end;
end.
