{ Configured profile observations retain their own immutable namespace. Source
  defines, local settings and query replies cannot impersonate that snapshot. }
program nyx_pascal_configured_fpu_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses SysUtils, Classes, nyx.text, nyx.scheduler, nyx.pascal.context,
  nyx.pascal.compilertrace, nyx.pascal.files, nyx.pascal.translation,
  nyx.pascal.work, nyx.pascal.lexing, nyx.pascal.syntax, nyx.pascal.syntax.translated,
  nyx.pascal.binding, nyx.pascal.constants
  {$ifndef PAS2JS}, configured.fpu.fixture{$endif};

const
  CFile: TNyxText = 'probe.pas';
  CPrefix: TNyxText = 'NYX_CONTEXT_fpu_';
  CVersion: TNyxText = '(3101) Macro defined: FPC'#10+
    '(3101) Macro defined: CPUI386'#10+
    '(3103) Macro FPC_VERSION set to 3'#10+
    '(3103) Macro FPC_RELEASE set to 3'#10+
    '(3103) Macro FPC_PATCH set to 1'#10+
    '(3103) Macro FPC_FULLVERSION set to 30301'#10;
  CTarget = 'Free Pascal Compiler version 3.3.1 [qualified] for i386'#10;

type
  { Mutable alternate producer delegates only the original context contract.
    Capture must read/copy its optional selector once and retain no producer. }
  TFPUProducer = class(TInterfacedObject, INyxPascalCompilerContext, INyxPascalContextConfiguredFPU)
  public
    Base: INyxPascalCompilerContext;
    Value: TNyxPascalFPUReference;
    Reads: Integer;
    destructor Destroy; override;
    function Family: TNyxPascalCompilerFamily;
    function Version: TNyxPascalCompilerVersion;
    function Mode: TNyxPascalLanguageMode;
    function DefinesComplete: Boolean;
    function Symbol(const AName: TNyxPascalDefineRef): TNyxPascalDefineValue;
    function Option(AName: Char): TNyxPascalKnowledge;
    function Feature(AFeature: TNyxPascalLanguageFeature): TNyxPascalKnowledge;
    function WithSymbol(const AName: TNyxPascalDefineRef; AKind: TNyxPascalDefineKind;
      AInteger: Integer = 0): INyxPascalCompilerContext;
    function CompleteDefines(AValue: Boolean): INyxPascalCompilerContext;
    function WithOption(AName: Char; AValue: TNyxPascalKnowledge): INyxPascalCompilerContext;
    function WithFeature(AFeature: TNyxPascalLanguageFeature; AValue: TNyxPascalKnowledge): INyxPascalCompilerContext;
    function InMode(AValue: TNyxPascalLanguageMode): INyxPascalCompilerContext;
    function UnknownState: INyxPascalCompilerContext;
    function SameFacts(const AOther: INyxPascalCompilerContext): Boolean;
    function SymbolCount: Integer;
    function SymbolName(AIndex: Integer): TNyxPascalDefineRef;
    function ConfiguredFPU: TNyxPascalFPUReference;
    function WithConfiguredFPU(const AValue: TNyxPascalFPUReference): INyxPascalCompilerContext;
  end;

var GChecks, GDestroyed: Integer;

destructor TFPUProducer.Destroy;
begin
  Inc(GDestroyed);
  inherited Destroy;
end;
function TFPUProducer.Family: TNyxPascalCompilerFamily;
begin Result := Base.Family; end;
function TFPUProducer.Version: TNyxPascalCompilerVersion;
begin Result := Base.Version; end;
function TFPUProducer.Mode: TNyxPascalLanguageMode;
begin Result := Base.Mode; end;
function TFPUProducer.DefinesComplete: Boolean;
begin Result := Base.DefinesComplete; end;
function TFPUProducer.Symbol(const AName: TNyxPascalDefineRef): TNyxPascalDefineValue;
begin Result := Base.Symbol(AName); end;
function TFPUProducer.Option(AName: Char): TNyxPascalKnowledge;
begin Result := Base.Option(AName); end;
function TFPUProducer.Feature(AFeature: TNyxPascalLanguageFeature): TNyxPascalKnowledge;
begin Result := Base.Feature(AFeature); end;
function TFPUProducer.WithSymbol(const AName: TNyxPascalDefineRef; AKind: TNyxPascalDefineKind;
  AInteger: Integer): INyxPascalCompilerContext;
begin Result := Base.WithSymbol(AName,AKind,AInteger); end;
function TFPUProducer.CompleteDefines(AValue: Boolean): INyxPascalCompilerContext;
begin Result := Base.CompleteDefines(AValue); end;
function TFPUProducer.WithOption(AName: Char; AValue: TNyxPascalKnowledge): INyxPascalCompilerContext;
begin Result := Base.WithOption(AName,AValue); end;
function TFPUProducer.WithFeature(AFeature: TNyxPascalLanguageFeature; AValue: TNyxPascalKnowledge): INyxPascalCompilerContext;
begin Result := Base.WithFeature(AFeature,AValue); end;
function TFPUProducer.InMode(AValue: TNyxPascalLanguageMode): INyxPascalCompilerContext;
begin Result := Base.InMode(AValue); end;
function TFPUProducer.UnknownState: INyxPascalCompilerContext;
begin Result := Base.UnknownState; end;
function TFPUProducer.SameFacts(const AOther: INyxPascalCompilerContext): Boolean;
begin Result := NyxPascalWithConfiguredFPU(Base,Value).SameFacts(AOther); end;
function TFPUProducer.SymbolCount: Integer;
begin Result := Base.SymbolCount; end;
function TFPUProducer.SymbolName(AIndex: Integer): TNyxPascalDefineRef;
begin Result := Base.SymbolName(AIndex); end;
function TFPUProducer.ConfiguredFPU: TNyxPascalFPUReference;
begin Inc(Reads); Result := Value; end;
function TFPUProducer.WithConfiguredFPU(const AValue: TNyxPascalFPUReference): INyxPascalCompilerContext;
begin Result := NyxPascalWithConfiguredFPU(Base,AValue); end;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then raise Exception.Create(AReason);
end;

function Facts: INyxPascalCompilerContext;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,TNyxPascalCompilerVersion.Create(3,3,1),ppmDelphi)
    .CompleteDefines(True);
end;

function Message(const AValue: TNyxText): TNyxText;
begin
  Result := CFile+'(4,2) Note: (2025) User defined: '+CPrefix+AValue+#10;
end;

function Probe: INyxPascalCompilerProbe;
begin
  Result := NewNyxFPCCompilerProbe(NyxPascalDefine('FPU'));
end;

function Frame(const AMacros: TNyxText; const ATarget: TNyxText = CTarget): TNyxText;
begin
  { Independently authored packet; no source generator supplies the expected
    reply stream. The native check also consumes a retained real transcript. }
  Result := CVersion+ATarget+AMacros+'(3104) Compiling probe.pas'+#10+
    Message('BEGIN')+Message('D_0')+Message('D_1')+Message('U_2')+Message('U_3')+
    Message('U_4')+Message('U_5')+Message('U_6')+Message('U_7')+
    Message('H_TRUE')+Message('END')+'(1008) 80 lines compiled, 0.1 sec'+#10;
end;

function Capture(const AText: TNyxText): INyxFPCCompilerCapture;
begin
  Result := ReadNyxFPCCompilerCapture(Probe,CFile,AText,0,TNyxPascalCompilerTraceOptions.Defaults);
end;

procedure ContextContract;
var
  LBase, LX87, LChanged, LCaptured, LCurrent: INyxPascalCompilerContext;
  LValue: TNyxPascalFPUReference;
begin
  LBase := Facts;
  Check(not NyxPascalConfiguredFPU(LBase).Known,'No selector is an uncaptured profile, not a default FPU');
  LValue := NyxPascalFPU('X87');
  LX87 := NyxPascalWithConfiguredFPU(LBase,LValue);
  Check((LValue.Name = 'x87') and not NyxPascalConfiguredFPU(LBase).Known,'The typed selector is normalized and copied');
  Check(not LX87.SameFacts(LBase),'A configured selector participates in exact context identity');
  LChanged := LX87.WithSymbol(NyxPascalDefine('FPUX87'),ppdUndefined)
    .WithSymbol(NyxPascalDefine('FPUSSE2'),ppdDefined);
  Check(NyxPascalConfiguredFPU(LChanged).Name = 'x87','Authored define changes cannot retarget the configured FPU');
  LCaptured := CaptureNyxPascalCompilerContext(LX87);
  Check(LCaptured.SameFacts(LX87),'Complete context capture retains configured profile identity');
  Check(NyxPascalConfiguredFPU(LX87.InMode(ppmObjFPC)).Name = 'x87','Mode changes retain initial profile metadata');
  Check(NyxPascalConfiguredFPU(LX87.UnknownState).Name = 'x87','Unknown source effects cannot rewrite earlier profile history');
  LCurrent := NyxPascalWithConfiguredFPU(LBase,NyxPascalFPU('SSE2'));
  LChanged := RestoreNyxPascalLocalSettings(LCurrent,LX87);
  Check(NyxPascalConfiguredFPU(LChanged).Name = 'sse2','Local settings restore cannot import another profile');
  LChanged := NyxPascalWithConfiguredFPU(LX87,Default(TNyxPascalFPUReference));
  Check(not NyxPascalConfiguredFPU(LChanged).Known and (NyxPascalConfiguredFPU(LX87).Name = 'x87'),
    'An explicit independent change can clear a snapshot without mutating its predecessor');
  Check(NyxPascalFPU('68881').Name = '68881','Open typed selector data supports numeric target names without inventing support');
end;

procedure TraceContract;
var
  LCapture, LOther, LMerged: INyxFPCCompilerCapture;
  LRejected: Boolean;
begin
  LCapture := Capture(Frame('(3101) Macro defined: FPUX87'+#10));
  Check((NyxPascalConfiguredFPU(LCapture.Context).Name = 'x87') and
    (NyxPascalConfiguredFPU(LCapture.ObservedDefaults).Name = 'x87'),
    'Qualified target and initial selector observations populate both separate snapshot owners');
  LOther := Capture(Frame('(3101) Macro defined: FPUX87'+#10+'(3101) Macro defined: FPU_MY_FEATURE'+#10));
  Check(NyxPascalConfiguredFPU(LOther.Context).Name = 'x87','An unrelated FPU-prefixed user symbol is not a compiler selector');
  LOther := Capture(Frame('(3101) Macro defined: FPUX87'+#10+'(3101) Macro defined: FPUSSE2'+#10));
  Check(not NyxPascalConfiguredFPU(LOther.Context).Known,'Conflicting selector observations remain unavailable');
  LOther := Capture(Frame('(3101) Macro defined: FPU_CUSTOM'+#10));
  Check(not NyxPascalConfiguredFPU(LOther.Context).Known,'An arbitrary user-defined name cannot manufacture configured hardware');
  LOther := Capture(Frame('(3101) Macro defined: FPUX87'+#10,''));
  Check(not NyxPascalConfiguredFPU(LOther.Context).Known,'A CPU define without the actual compiler target banner is insufficient');
  LOther := Capture(Frame('(3101) Macro defined: FPUX87'+#10,
    'Free Pascal Compiler version 3.3.1 [qualified] for aarch64'+#10));
  Check(not NyxPascalConfiguredFPU(LOther.Context).Known,'An unqualified target cannot borrow the analyzer target or an authored CPU define');
  LOther := Capture(Frame('(3101) Macro defined: FPUX87'+#10,CTarget+CTarget));
  Check(not NyxPascalConfiguredFPU(LOther.Context).Known,'Duplicated target ownership is not selection evidence');
  LMerged := LCapture.Merge(LCapture);
  Check(NyxPascalConfiguredFPU(LMerged.Context).Name = 'x87','Compatible cohorts retain the same configured observation');
  LOther := Capture(Frame('(3101) Macro defined: FPUSSE2'+#10));
  LRejected := False;
  try
    LCapture.Merge(LOther);
  except
    on ENyxPascalContext do LRejected := True;
  end;
  Check(LRejected and (NyxPascalConfiguredFPU(LCapture.Context).Name = 'x87'),
    'Changed compiler-profile anchors refuse atomically');
end;

procedure SourceContract;
var
  LContext: INyxPascalCompilerContext;
  LSource: INyxPascalTranslation;
begin
  LContext := NyxPascalWithConfiguredFPU(Facts,NyxPascalFPU('X87'));
  LSource := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact).WithFile(
    NyxPascalPhysicalFile('source.pas'),'{$undef FPUX87}{$define FPUSSE2}Marker'),
    NyxPascalPhysicalFile('source.pas'),LContext,TNyxPascalTranslationOptions.Defaults);
  LContext := LSource.State(LSource.Token(LSource.Count-1).State);
  Check(LSource.Resolved and (LContext.Symbol(NyxPascalDefine('FPUSSE2')).IsDefined = ppkTrue) and
    (NyxPascalConfiguredFPU(LContext).Name = 'x87'),'Source namespace mutation remains independent of initial configuration');
end;

procedure ProducerContract;
var
  LProducer: TFPUProducer;
  LSource, LCaptured: INyxPascalCompilerContext;
  LBefore: Integer;
begin
  LBefore := GDestroyed;
  LProducer := TFPUProducer.Create;
  LProducer.Base := Facts;
  LProducer.Value := NyxPascalFPU('X87');
  LSource := LProducer;
  LCaptured := CaptureNyxPascalCompilerContext(LSource);
  Check(LProducer.Reads = 1,'Capture freezes the alternate selector exactly once');
  LProducer.Value := NyxPascalFPU('SSE2');
  Check(NyxPascalConfiguredFPU(LCaptured).Name = 'x87','Later alternate-producer mutations cannot change the snapshot');
  LSource := nil;
  {$ifndef PAS2JS}
  Check(GDestroyed = LBefore+1,'The immutable snapshot retains no alternate producer');
  {$endif}
end;

function StorageIdentity(const AName: TNyxText): TNyxText;
var
  LContext: INyxPascalCompilerContext;
  LFiles: INyxPascalFileCatalog;
  LView, LSystem: INyxPascalTranslatedSyntax;
  LWorld: INyxPascalSemanticWorld;
  LReply: INyxPascalConstantResult;
  LIdentity: INyxPascalConstantStorageIdentity;
begin
  LContext := Facts.WithOption('H',ppkTrue);

  if AName <> '' then LContext := NyxPascalWithConfiguredFPU(LContext,NyxPascalFPU(AName));
  LContext := NyxPascalWithCapturedSize(LContext,NyxPascalSizeReference('Byte'),
    NyxPascalSizeFact(pssKnown,ppmDelphi,ppkTrue,1));
  LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('layout.pas'),
    'unit Layout;interface {$packrecords 1}type TPacket=record Value:Byte;end;implementation end.')
    .WithFile(NyxPascalPhysicalFile('system.pas'),'unit System;interface implementation end.');
  LView := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFiles,NyxPascalPhysicalFile('layout.pas'),
    LContext,TNyxPascalTranslationOptions.Defaults),TNyxPascalSyntaxOptions.Defaults);
  LSystem := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFiles,NyxPascalPhysicalFile('system.pas'),
    LContext,TNyxPascalTranslationOptions.Defaults),TNyxPascalSyntaxOptions.Defaults);
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LView).WithModule(LSystem);
  LReply := PlanNyxPascalConstant('sizeof(TPacket)',TNyxPascalConstantOptions.Defaults)
    .Evaluate(LWorld.At(0,0,LView.Syntax.Lexical.Scalars),TNyxPascalConstantOptions.Defaults);
  Check(Supports(LReply,INyxPascalConstantStorageIdentity,LIdentity),'Complete structural evidence retains its profile owner');
  Result := LIdentity.StorageKey.Value;
end;

procedure StorageContract;
var LMissing, LX87, LSSE2: TNyxText;
begin
  LMissing := StorageIdentity('');
  LX87 := StorageIdentity('X87');
  LSSE2 := StorageIdentity('SSE2');
  Check(Pos('configured-fpu-v1',LMissing) = 0,'Uncaptured profiles preserve the previous structural-key form');
  Check((Pos('configured-fpu-v1',LX87) > 0) and (LX87 <> LSSE2) and
    (LMissing <> LX87),'Configured profiles own separate tagged structural identities');
end;

procedure WorkContract;
var
  LCapture, LRetained: INyxFPCCompilerCapture;
  LBudget: INyxPascalAnalysisBudget;
  LCancel: INyxCancellationScope;
  LAt, LFit: Integer;
  LRejected: Boolean;
  LText: TNyxText;
begin
  LText := Frame('(3101) Macro defined: FPUX87'+#10);
  LFit := -1;
  for LAt := 1 to 20000 do
  begin
    LCapture := nil;
    LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      try
        LCapture := ReadNyxFPCCompilerCapture(Probe,CFile,LText,0,TNyxPascalCompilerTraceOptions.Defaults,LBudget);
        Check(NyxPascalConfiguredFPU(LCapture.Context).Name = 'x87','First fit publishes the complete configured snapshot');
        LFit := LAt;
        LRetained := LCapture;
      except
        on ENyxPascalAnalysisWorkLimit do
          Check((LCapture = nil) and (LBudget.Receipt.Total <= LAt),'Every smaller allowance refuses atomically');
      end;
    finally
      LBudget.Retire;
    end;

    if LFit > 0 then Break;
  end;
  Check((LFit > 1) and (NyxPascalConfiguredFPU(LRetained.Context).Name = 'x87'),
    'Snapshot data outlives its retired analysis scope');
  WriteLn('configured FPU work / first fit=',LFit,' every smaller allowance refused');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LCapture := nil;
  LRejected := False;
  try
    LCapture := ReadNyxFPCCompilerCapture(Probe,CFile,LText,0,TNyxPascalCompilerTraceOptions.Defaults,LCancel);
  except
    on ENyxPascalAnalysisCancelled do LRejected := True;
  end;
  Check(LRejected and (LCapture = nil),'Cancellation cannot publish partial FPU metadata');
end;

{$ifndef PAS2JS}
function ReadExact(const APath: String): TNyxText;
var LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath,fmOpenRead or fmShareDenyWrite);
  try
    Check(LStream.Size < 4*1024*1024,'Retained compiler input is bounded');
    SetLength(Result,LStream.Size);

    if Length(Result) > 0 then LStream.ReadBuffer(Result[1],Length(Result));
  finally
    LStream.Free;
  end;
end;

procedure NativeWitness;
var
  LProbe: INyxPascalCompilerProbe;
  LCapture: INyxFPCCompilerCapture;
  LPath: String;
  LText: TNyxText;
begin
  Check(NativeConfiguredFPU = 'x87','The independent whole native witness has its actual initial compiler setting');
  Check(NativeSpoofedDivisionType = 'Extended','Changing FPU macro spelling does not change actual native division behavior');
  Check(NativeConfiguredInvocations = 1,'The native witness executes once');
  Check(ParamCount = 1,'Supply the existing retained profile directory; no new profile job is created');
  LPath := IncludeTrailingPathDelimiter(ParamStr(1));
  LProbe := NewNyxFPCCompilerProbe(NyxPascalDefine('PREPARATION'),ppmDelphi);
  LText := ReadExact(LPath+'nyxcompilercontext.pas');
  Check(LText = LProbe.Source,'The existing control-profile source is byte-exact to its owned rendering with no added queries');
  LCapture := ReadNyxFPCCompilerCapture(LProbe,'nyxcompilercontext.pas',ReadExact(LPath+'compiler-profile.log'),0,
    TNyxPascalCompilerTraceOptions.Defaults);
  Check(NyxPascalConfiguredFPU(LCapture.Context).Name = NativeConfiguredFPU,
    'The retained real compiler transcript supplies the independently observed configured FPU');
  WriteLn('native configured FPU / ',String(NyxPascalConfiguredFPU(LCapture.Context).Name),
    ' actual division after source macro spoof=Extended; retained profile replay only');
end;
{$endif}

begin
  try
    {$ifndef PAS2JS}NativeWitness;{$endif}
    ContextContract;
    TraceContract;
    SourceContract;
    ProducerContract;
    StorageContract;
    WorkContract;
    {$ifndef PAS2JS}
    Check(NativeConfiguredInvocations = 1,'Context and transcript queries execute no application body');
    {$endif}
    WriteLn('PASS ',GChecks,' configured FPU snapshot checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ',E.ClassName,': ',E.Message);
      ExitCode := 1;
    end;
  end;
end.
