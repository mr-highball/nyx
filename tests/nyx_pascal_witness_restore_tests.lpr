{ Controlled source-witness restoration. MIT license, see LICENSE. }
program nyx_pascal_witness_restore_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.bytes, nyx.scheduler, nyx.pascal.context,
  nyx.pascal.files, nyx.pascal.encoding, nyx.pascal.lexing,
  nyx.pascal.syntax, nyx.pascal.syntax.translated, nyx.pascal.translation,
  nyx.pascal.binding, nyx.pascal.conditions, nyx.pascal.requirements,
  nyx.pascal.compilertrace, nyx.pascal.witness, nyx.pascal.work;

type
  { Explicit fixture compiler facts exercise ownership without measuring an ABI.
    Freeze refuses to delegate merging to this temporary alternate producer. }
  TFixtureCapture = class(TInterfacedObject, INyxFPCCompilerCapture)
  public
    Value: INyxPascalCompilerContext;
    function Context: INyxPascalCompilerContext;
    function ObservedDefaults: INyxPascalCompilerContext;
    function Merge(const ANext: INyxFPCCompilerCapture): INyxFPCCompilerCapture;
  end;
  { Counts actual proof cancellation boundaries, including a final cancellation
    and retirement of the caller's last separate scope variable. }
  TProofCancellation = class(TInterfacedObject, INyxCancellationScope)
  public
    Calls, StopAt: Integer;
    ClearCaller, Stopped: Boolean;
    procedure Cancel;
    function GetCancelled: Boolean;
    destructor Destroy; override;
  end;

var
  GChecks: Integer;
  GCallerScope: INyxCancellationScope;
  GScopeRetired: Boolean;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then
    raise Exception.Create(AReason);
end;

function TFixtureCapture.Context: INyxPascalCompilerContext;
begin Result := Value; end;
function TFixtureCapture.ObservedDefaults: INyxPascalCompilerContext;
begin Result := Value; end;
function TFixtureCapture.Merge(const ANext: INyxFPCCompilerCapture): INyxFPCCompilerCapture;
begin
  Result := nil;
  raise Exception.Create('An immutable capture must not use an alternate merger');
end;

procedure TProofCancellation.Cancel;
begin Stopped := True; end;
function TProofCancellation.GetCancelled: Boolean;
begin
  Inc(Calls);

  if ClearCaller then
    GCallerScope := nil;
  Result := Stopped or ((StopAt > 0) and (Calls >= StopAt));
end;
destructor TProofCancellation.Destroy;
begin
  GScopeRetired := True;
  inherited Destroy;
end;

function FixtureOwner(AChanged: Boolean = False): INyxFPCCompilerCapture;
var
  LValue: TFixtureCapture;
  LLease: INyxFPCCompilerCapture;
begin
  LValue := TFixtureCapture.Create;
  LLease := LValue;
  LValue.Value := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmFPC, pcsoSystemBootstrap)
    .WithSymbol(NyxPascalDefine('FPC'), ppdDefined).WithOption('H', ppkFalse);

  if AChanged then
    LValue.Value := LValue.Value.WithOption('R', ppkTrue);
  Result := FreezeNyxFPCCompilerCapture(LLease);
end;

function FixtureFiles: INyxPascalFileCatalog;
begin
  Result := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('fixture/system.pp'),
      'unit System; interface {$mode objfpc}{$include payload.inc} implementation end.')
    .WithFile(NyxPascalPhysicalFile('fixture/payload.inc'),
      'type TPair=packed record First:Byte; Second:Word; end;'#10 +
      'const PairBytes=sizeof(TPair);'#10 +
      '{$if PairBytes>0}const Ready=1;{$endif}')
    .WithFile(NyxPascalPhysicalFile('fixture/spare.inc'), 'const UnusedSibling=1;')
    .WithIncludeDirectory('fixture');
end;

function FixtureRequest(const AFiles: INyxPascalFileCatalog;
  const AOwner: INyxFPCCompilerCapture): INyxPascalCompilerRequirement;
var
  LSource: INyxPascalSemanticTranslation;
  LContext: INyxPascalCompilerContext;
begin
  LContext := NewNyxPascalCompilerContext(ppcFPC, AOwner.Context.Version, ppmFPC)
    .WithOption('H', ppkFalse);
  LContext := NyxPascalWithBootstrapContext(LContext, AOwner.Context);
  LSource := NewNyxPascalSemanticTranslation(AFiles,
    NyxPascalPhysicalFile('fixture/system.pp'), LContext,
    NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults.WithImplicitScopes(pispCompilerUnits)),
    TNyxPascalTranslationOptions.Defaults, TNyxPascalSyntaxOptions.Defaults,
    TNyxPascalConditionalOptions.Defaults);
  Check(LSource.Pending and (CaptureNyxPascalRequirements(LSource).RequirementCount = 1),
    'Controlled complete source supplies one real typed structural requirement');
  Result := CaptureNyxPascalRequirements(LSource).RequirementAt(0);
  Check(Result.Kind = pcrSourceSize, 'The authored record remains a source-size request');
end;

{ source-v1 witnesses describe only their historically supported Unknown initial
  policies. A source-authored MODE may establish current helper lookup intent,
  but neither known initial nor known bootstrap policy can be smuggled into that
  old framing. This is admission testing, with no native compiler measurement. }
procedure HelperPolicyWitnessBoundary;
var
  LFiles: INyxPascalFileCatalog;
  LInitial, LRoot, LContext, LKnown: INyxPascalCompilerContext;
  LView: INyxPascalTranslatedSyntax;
  LPolicy: TNyxPascalKnowledge;
  LLocation: Integer;

  function View(const AContext: INyxPascalCompilerContext): INyxPascalTranslatedSyntax;
  begin
    Result := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFiles,
      NyxPascalPhysicalFile('fixture/system.pp'), AContext,
      TNyxPascalTranslationOptions.Defaults), TNyxPascalSyntaxOptions.Defaults);
  end;

begin
  LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(
    NyxPascalPhysicalFile('fixture/system.pp'),
    'unit System; interface {$mode objfpc} type TPair=record Value:Byte; end; implementation end.');
  LInitial := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmFPC).WithOption('H', ppkFalse);
  LRoot := FixtureOwner.Context;
  LContext := NyxPascalWithBootstrapContext(LInitial, LRoot);
  LView := View(LContext);
  Check(LView.CompleteTranslation and LView.Source.Resolved and NyxPascalSourceWitnessContext(LView),
    'Authored MODE does not change the legacy Unknown initial/bootstrap witness admission');
  for LPolicy := ppkFalse to ppkTrue do
  begin
    for LLocation := 0 to 1 do
    begin
      LKnown := NyxPascalWithCompilerPolicies(LInitial,
        TNyxPascalCompilerPolicies.Unknown.WithMultipleHelpers(LPolicy));

      if LLocation = 0 then
        LContext := NyxPascalWithBootstrapContext(LKnown, LRoot)
      else
        LContext := NyxPascalWithBootstrapContext(LInitial,
          NyxPascalWithCompilerPolicies(LRoot,
            TNyxPascalCompilerPolicies.Unknown.WithMultipleHelpers(LPolicy)));
      LView := View(LContext);
      Check(LView.CompleteTranslation and LView.Source.Resolved and
        not NyxPascalSourceWitnessContext(LView),
        'Known initial or bootstrap helper policy cannot borrow source-v1 witness framing');
    end;
  end;
  Check(NyxPascalSourceWitnessContext(View(NyxPascalWithBootstrapContext(
    LInitial, LRoot))), 'Refused policy variants leave the original witness boundary intact');
end;

function FileText(const AFiles: INyxPascalFileCatalog; const AName: TNyxText): TNyxText;
var
  LBytes: INyxPascalSourceBytes;
begin
  LBytes := NyxPascalSourceBytesAt(AFiles, AFiles.Find(NyxPascalPhysicalFile(AName)));
  Result := NyxDecodeUTF8(LBytes.Slice(NyxPascalByteSpan(0, LBytes.Count)));
end;

function ReplaceText(const AFiles: INyxPascalFileCatalog; const AName,
  ABefore, AAfter: TNyxText): INyxPascalFileCatalog;
var
  LBytes: INyxPascalSourceBytes;
  LText: TNyxText;
  LByteFiles: INyxPascalByteFileCatalog;
begin
  LBytes := NyxPascalSourceBytesAt(AFiles, AFiles.Find(NyxPascalPhysicalFile(AName)));
  LText := FileText(AFiles, AName);
  Check(Pos(ABefore, LText) > 0, 'Negative fixture changes an actual retained byte sequence');
  LText := StringReplace(LText, ABefore, AAfter, []);

  if not Supports(AFiles, INyxPascalByteFileCatalog, LByteFiles) then
    raise Exception.Create('Controlled retained files require their byte capability');
  Result := LByteFiles.WithByteSource(NyxPascalPhysicalFile(AName),
    NewNyxPascalSourceBytes(NyxEncodeUTF8(LText), LBytes.Origin));
end;

function CopyBytes(const AFiles: INyxPascalFileCatalog; ACase: TNyxPascalFileCase;
  AReverse, AOmitLast: Boolean): INyxPascalFileCatalog;
var
  LAt, LIndex: Integer;
  LByteFiles: INyxPascalByteFileCatalog;
begin
  Result := NewNyxPascalFileCatalog(ACase);
  for LAt := 0 to AFiles.Count - 1 do
  begin
    LIndex := LAt;

    if AReverse then
      LIndex := AFiles.Count - LAt - 1;

    if AOmitLast and (LIndex = AFiles.Count - 1) then
      Continue;

    if not Supports(Result, INyxPascalByteFileCatalog, LByteFiles) then
      raise Exception.Create('Controlled copied files require their byte capability');
    Result := LByteFiles.WithByteSource(
      AFiles.FileAt(LIndex), NyxPascalSourceBytesAt(AFiles, LIndex));
  end;
  for LAt := 0 to AFiles.DirectoryCount - 1 do
    Result := Result.WithIncludeDirectory(AFiles.DirectoryAt(LAt));
end;

{ Controlled external note data is parser input, never a native compiler claim.
  The separate adoption consumer must replay the actual archived compiler log. }
function Transcript(const APrefix: TNyxText): TNyxText;
var
  LBit: Integer;
begin
  Result := 'system.pp(1,1) Note: (2025) User defined: ' + APrefix + 'READY'#10 +
    'payload.inc(1,1) Note: (2025) User defined: ' + APrefix + '0_BEGIN'#10;
  for LBit := 0 to 30 do
    Result := Result + 'payload.inc(1,1) Note: (2025) User defined: ' + APrefix +
      '0_BIT_' + TNyxText(IntToStr(LBit)) + '_' +
      TNyxText(IntToStr((3 shr LBit) and 1)) + #10;
  Result := Result + 'payload.inc(1,1) Note: (2025) User defined: ' +
    APrefix + '0_END'#10 + 'payload.inc(1,1) Fatal: (2022) User defined: ' +
    APrefix + 'STOP'#10 + 'Fatal: (1018) Compilation aborted'#10;
end;

procedure Run;
var
  LOwner, LReply: INyxFPCCompilerCapture;
  LFiles, LRetained, LChanged: INyxPascalFileCatalog;
  LRequest, LNextRequest: INyxPascalCompilerRequirement;
  LSource, LNextSource: INyxPascalSourceSizeRequirement;
  LOriginal, LRestored, LRefused: INyxPascalSourceWitness;
  LText, LPrefix: TNyxText;
  LAt, LVariant, LCalls, LFit: Integer;
  LRejected: Boolean;
  LScope: INyxCancellationScope;
  LCounter: TProofCancellation;
  LBudget: INyxPascalAnalysisBudget;
begin
  LOwner := FixtureOwner;
  LFiles := FixtureFiles;
  LRequest := FixtureRequest(LFiles, LOwner);

  if not Supports(LRequest, INyxPascalSourceSizeRequirement, LSource) then
    raise Exception.Create('Controlled typed source-size requirement is unavailable');
  LOriginal := PlanNyxPascalSourceWitness(LRequest, LOwner);
  LRetained := LOriginal.Files;
  LText := FileText(LRetained, 'fixture/system.pp');
  LAt := Pos('NYX_SOURCE_', LText);
  LPrefix := Copy(LText, LAt, Length('NYX_SOURCE_') + 33);
  LRestored := RestoreNyxPascalSourceWitness(LRequest, LOwner, LRetained);
  Check(LRestored.Files.SameFiles(LOriginal.Files),
    'Restoration reconstructs the complete exact witness and original source views');
  LReply := LRestored.Read(Transcript(LPrefix), 1, TNyxPascalCompilerTraceOptions.Defaults);
  Check(NyxPascalCapturedStorage(LReply.Context, LSource.StorageKey).Bytes = 3,
    'Existing Read accepts the complete controlled packet after restoration');
  LChanged := CopyBytes(LRetained, pfcExact, False, False);
  LRestored := RestoreNyxPascalSourceWitness(LRequest, LOwner, LChanged);
  Check(LRestored.Files.SameFiles(LOriginal.Files) and
    (NyxPascalSourceViewAt(LChanged, LChanged.Find(NyxPascalPhysicalFile('fixture/spare.inc'))) = nil),
    'Retained decoding supplies no source authority; reconstructed views belong to current original inputs');

  LNextRequest := FixtureRequest(FixtureFiles, LOwner);

  if not Supports(LNextRequest, INyxPascalSourceSizeRequirement, LNextSource) then
    raise Exception.Create('Reconstructed typed source-size requirement is unavailable');
  Check(LSource.StorageKey.Value <> LNextSource.StorageKey.Value,
    'Reconstructed original catalogs retain distinct process-local requirement keys');
  LRestored := RestoreNyxPascalSourceWitness(LNextRequest, LOwner, LRetained);
  LReply := LRestored.Read(Transcript(LPrefix), 1, TNyxPascalCompilerTraceOptions.Defaults);
  Check((NyxPascalCapturedStorage(LReply.Context, LNextSource.StorageKey).State = psfsKnown) and
    (NyxPascalCapturedStorage(LReply.Context, LSource.StorageKey).State = psfsUnknown),
    'Byte-identical replay answers the current request without transplanting its historical key');

  for LVariant := 0 to 11 do
  begin
    case LVariant of
      0: LChanged := ReplaceText(LRetained, 'fixture/system.pp', 'NYX_SOURCE_', 'NYX_FOREIGN_');
      1: LChanged := ReplaceText(LRetained, 'fixture/system.pp', LPrefix + 'READY}',
        'NYX_SOURCE_Z0000000000000000000000000000000_READY}');
      2: LChanged := ReplaceText(LRetained, 'fixture/system.pp', 'implementation end.',
        'implementation end. {$push}{$notes on}{$note ' + LPrefix + 'READY}{$pop}');
      3: LChanged := ReplaceText(LRetained, 'fixture/payload.inc', 'div 1)', 'div 2)');
      4: LChanged := ReplaceText(LRetained, 'fixture/spare.inc', '=1', '=2');
      5: LChanged := ReplaceText(LRetained, 'fixture/payload.inc', 'First:Byte', 'First:Word');
      6: LChanged := LRetained.WithFile(NyxPascalPhysicalFile('extra.inc'), 'const Extra=1;');
      7: LChanged := CopyBytes(LRetained, pfcExact, False, True);
      8: LChanged := CopyBytes(LRetained, pfcExact, True, False);
      9: LChanged := LRetained.WithIncludeDirectory('other');
      10: LChanged := CopyBytes(LRetained, pfcASCIIInsensitive, False, False);
      11: LChanged := ReplaceText(LRetained, 'fixture/system.pp', LPrefix + 'READY}',
        'NYX_SOURCE_00000000000000000000000000000000_READY}');
    end;
    LRefused := nil;
    LRejected := False;
    try
      LRefused := RestoreNyxPascalSourceWitness(LRequest, LOwner, LChanged);
    except
      on ENyxPascalContext do LRejected := True;
    end;
    Check(LRejected and (LRefused = nil),
      'Changed marker, instrumentation, original sibling, membership or search policy refuses atomically');
  end;

  LRefused := nil;
  LRejected := False;
  try
    LRefused := RestoreNyxPascalSourceWitness(LRequest, FixtureOwner(True), LRetained);
  except
    on ENyxPascalContext do LRejected := True;
  end;
  Check(LRejected and (LRefused = nil), 'A different compiler owner cannot borrow retained source measurements');

  LCounter := TProofCancellation.Create;
  LScope := LCounter;
  LRestored := RestoreNyxPascalSourceWitness(LRequest, LOwner, LRetained, LScope);
  LCalls := LCounter.Calls;
  Check(LCalls > 3, 'Restoration exposes actual cancellation boundaries throughout proof');
  for LVariant := 0 to 2 do
  begin
    LCounter.Calls := 0;
    case LVariant of
      0: LCounter.StopAt := 1;
      1: LCounter.StopAt := LCalls div 2;
      2: LCounter.StopAt := LCalls;
    end;
    LRefused := nil;
    LRejected := False;
    try
      LRefused := RestoreNyxPascalSourceWitness(LRequest, LOwner, LRetained, LScope);
    except
      on ENyxPascalAnalysisCancelled do LRejected := True;
    end;
    Check(LRejected and (LRefused = nil), 'Early, middle and final cancellation retain atomic refusal');
  end;
  LScope := nil;
  LBudget := NewNyxPascalAnalysisBudget(2000000);
  try
    LRestored := RestoreNyxPascalSourceWitness(LRequest, LOwner, LRetained, LBudget);
    LFit := LBudget.Receipt.Total;
    Check(LFit > 1, 'Exact retained-byte proof charges the supplied existing analysis budget');
  finally
    LBudget.Retire;
  end;
  LBudget := NewNyxPascalAnalysisBudget(LFit - 1);
  try
    LRefused := nil;
    LRejected := False;
    try
      LRefused := RestoreNyxPascalSourceWitness(LRequest, LOwner, LRetained, LBudget);
    except
      on ENyxPascalAnalysisWorkLimit do LRejected := True;
    end;
    Check(LRejected and (LRefused = nil), 'Insufficient proof allowance cannot publish a witness');
  finally
    LBudget.Retire;
  end;
  LCounter := TProofCancellation.Create;
  LCounter.ClearCaller := True;
  GScopeRetired := False;
  GCallerScope := LCounter;
  LRestored := RestoreNyxPascalSourceWitness(LRequest, LOwner, LRetained, GCallerScope);
  Check((GCallerScope = nil) and GScopeRetired and (LRestored <> nil),
    'A local lease survives caller-slot retirement and no scope survives the result');
  Check(LFiles.SameFiles(FixtureFiles), 'All controlled original files remain exact');
end;

begin
  try
    Run;
    HelperPolicyWitnessBoundary;
    WriteLn('PASS ', GChecks, ' controlled source-witness restoration checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
