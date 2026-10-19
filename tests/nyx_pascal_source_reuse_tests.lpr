{ Per-provider source reuse through ordinary completion requests. Inputs remain
  immutable project/catalog values; native workers never touch controls. These
  consumers qualify analysis reuse, not live editor adoption or runtime parity. }
program nyx_pascal_source_reuse_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, {$ifndef PAS2JS}Classes,{$endif} nyx.text, nyx.bytes, nyx.scheduler, nyx.editing,
  nyx.pascal.names, nyx.pascal.project, nyx.pascal.drafts, nyx.pascal.sources,
  nyx.pascal.context, nyx.pascal.lexing, nyx.pascal.files, nyx.pascal.binding,
  nyx.pascal.translation, nyx.pascal.modules, nyx.pascal.work,
  nyx.pascal.completion, nyx.pascal.completion.owned;

const
  CMain: TNyxText = 'unit Consumer; {$mode delphi} interface uses Widgets; implementation ' +
    'procedure Run; var Item:TWidget; begin Item.Val; end; end.';
  CExtra: TNyxText = 'unit Extra; interface const Flag=1; implementation end.';
  CMembers: TNyxText = 'Value:LongInt;';

type
  { Caller-owned work/cancellation boundary used to stop inside an actual warm
    source proof. It forwards ordinary admission and retains no provider, source
    or response; the test retires its detached budget after the call joins. }
  TProofStop = class(TInterfacedObject, INyxPascalAnalysisBudget)
  public
    Budget: INyxPascalAnalysisBudget;
    Remaining: Integer;
    Stopped: Boolean;
    procedure Cancel;
    function GetCancelled: Boolean;
    procedure Admit(APhase: TNyxPascalAnalysisPhase; AUnits: Integer = 1);
    function Receipt: TNyxPascalAnalysisReceipt;
    procedure Retire;
  end;
  {$ifndef PAS2JS}
  { A concurrent completion caller owns its project and response, sharing only
    the immutable configuration/bounded source directory through the provider.
    Main joins before inspecting results; no test counters are shared by workers. }
  TQueryReader = class(TThread)
  protected
    procedure Execute; override;
  public
    Provider: INyxPascalCompletionProvider;
    Sources: INyxPascalSourceSnapshot;
    Expected: TNyxText;
    ErrorText: String;
    Iterations: Integer;
  end;
  {$endif}

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not ACondition then raise Exception.Create(AReason);
end;

function Facts: INyxPascalCompilerContext;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi).CompleteDefines(True);
end;

function Options: TNyxOwnedPascalCompletionOptions;
begin
  Result := TNyxOwnedPascalCompletionOptions.Defaults.WithBinding(
    TNyxPascalBindingOptions.Defaults.WithImplicitScopes(pispDeclarationNames));
end;

function Files(AWorldDependent: Boolean = False): INyxPascalFileCatalog;
var
  LText: TNyxText;
  LAt: Integer;
begin
  LText := 'unit Widgets; interface ';

  if AWorldDependent then LText := LText + 'uses Extra; ';
  LText := LText + 'type TWidget=record ';

  if AWorldDependent then
    LText := LText + '{$if declared(Extra.Flag)}Value:LongInt;{$else}Other:LongInt;{$endif}'
  else
    LText := LText + '{$i members.inc}';
  LText := LText + ' end; ';
  { A realistic unchanged dependency has many declarations. Measured logical
    cold/warm work must reflect an avoided parse, not an assumed speedup. }
  for LAt := 1 to 128 do
    LText := LText + 'TUnused' + TNyxText(IntToStr(LAt)) + '=record Field:LongInt; end; ';
  Result := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('widgets.pas'), LText + 'implementation end.')
    .WithFile(NyxPascalPhysicalFile('members.inc'), CMembers);
end;

function Sources: INyxPascalSourceSnapshot;
begin
  Result := CaptureNyxPascalSources(NewNyxPascalProject(NyxPascalUnit('Consumer'), CMain)
    .WithUnit(NyxPascalUnit('Extra'), CExtra)
    .WithInclude(NyxPascalInclude('members.inc'), CMembers));
end;

function RequestOf(const ASource: INyxPascalSourceSnapshot): INyxPascalCompletionRequest;
var
  LText: TNyxText;
  LAt: Integer;
begin
  LText := ASource.Text(TNyxPascalSourceRef.Main);
  LAt := Pos('Item.Val', LText);

  if LAt = 0 then raise Exception.Create('Expected query in the exact main buffer');
  Result := NewNyxPascalCompletionRequest(ASource).At(TNyxPascalSourceRef.Main,
    NyxTextScalarCount(Copy(LText, 1, LAt - 1)) + 8);
end;

function Provider(const AOptions: TNyxOwnedPascalCompletionOptions;
  AWorldDependent: Boolean = False; AIndexed: Boolean = False): INyxPascalCompletionProvider;
var
  LRoots: array of TNyxPascalPhysicalFile;
  LModules: INyxPascalModuleCatalog;
begin

  if AIndexed then
  begin
    LModules := NewNyxPascalModuleCatalog.WithModule(NewNyxPascalModuleSource(
      NyxPascalModule('Widgets'), NyxPascalPhysicalFile('widgets.pas')));
    Result := NewNyxIndexedPascalCompletionProvider(Facts, Files(AWorldDependent), LModules, AOptions);
  end
  else
  begin
    SetLength(LRoots, 1);
    LRoots[0] := NyxPascalPhysicalFile('widgets.pas');
    Result := NewNyxOwnedPascalCompletionProvider(Facts, Files(AWorldDependent), LRoots, AOptions);
  end;
end;

function Reuse(const AReply: INyxPascalCompletionResult): INyxPascalCompletionReuse;
begin
  Check(Supports(AReply, INyxPascalCompletionReuse, Result), 'The request has a detached reuse-work receipt');
end;

function Analysis(const AReply: INyxPascalCompletionResult): INyxPascalCompletionAnalysis;
begin
  Check(Supports(AReply, INyxPascalCompletionAnalysis, Result), 'The request has ordinary detached module analysis');
end;

function Revalidation(const AReply: INyxPascalCompletionResult): INyxPascalCompletionRevalidation;
begin
  Check(Supports(AReply, INyxPascalCompletionRevalidation, Result), 'Current-world semantic revalidation has detached counters');
end;

function Location(const AReply: INyxPascalCompletionResult): INyxOwnedPascalCompletionResult;
begin
  Check(Supports(AReply, INyxOwnedPascalCompletionResult, Result), 'The ready row retains its physical definition source');
end;

procedure Ready(const AReply: INyxPascalCompletionResult; const AName: TNyxText = 'Value');
begin
  Check((AReply.Status = pcsReady) and (AReply.Count = 1),
    'One ready member: ' + String(AReply.MessageText));
  Check(AReply.Item(0).Name = AName, 'The member belongs to this request''s exact world');
end;

function CheckStorage(const AProvider: INyxPascalCompletionProvider;
  const AOptions: TNyxOwnedPascalCompletionOptions): TNyxPascalSourceReuseState;
var
  LStorage: INyxPascalCompletionReuseStorage;
begin
  Check(Supports(AProvider, INyxPascalCompletionReuseStorage, LStorage), 'The provider exposes a detached atomic storage observation');
  Result := LStorage.SourceReuseState;
  Check((Result.Modules >= 0) and (Result.Modules <= AOptions.SourceReuse.MaximumModules) and
    (Result.Tokens >= 0) and (Result.Tokens <= AOptions.SourceReuse.MaximumTokens) and
    (Result.CatalogBytes >= 0) and (Result.CatalogBytes <= AOptions.SourceReuse.MaximumCatalogBytes),
    'Every published retention counter stays inside its independent configured ceiling');
end;

procedure EditsAndLifetime;
var
  LProvider: INyxPascalCompletionProvider;
  LSource, LChanged, LExpanded: INyxPascalSourceSnapshot;
  LCold, LWarm, LReply: INyxPascalCompletionResult;
  LColdWork, LWarmWork: Integer;
begin
  LProvider := Provider(Options);
  LSource := Sources;
  LCold := LProvider.Complete(RequestOf(LSource));
  Ready(LCold);
  Check((Reuse(LCold).ConstructedModules = 3) and (Reuse(LCold).ReusedModules = 0), 'The first request constructs all three roots');
  LWarm := LProvider.Complete(RequestOf(LSource));
  Ready(LWarm);
  Check((Reuse(LWarm).ConstructedModules = 0) and (Reuse(LWarm).ReusedModules = 3), 'A repeated request rebinds every independent module');
  Check(Location(LCold).DefinitionSource(0) = Location(LWarm).DefinitionSource(0), 'Unchanged include navigation shares its exact lexical owner');
  LColdWork := Analysis(LCold).TranslatedTokens;
  LWarmWork := Analysis(LWarm).TranslatedTokens;
  Check((Reuse(LWarm).ReuseWork > 0) and (LWarmWork < LColdWork), 'This unchanged dependency avoids measured logical analysis work');
  WriteLn('completion reuse work / cold=', LColdWork, ' warm=', LWarmWork);
  LChanged := LSource.WithBuffer(TNyxPascalSourceRef.UnitFile(NyxPascalUnit('Extra')),
    'unit Extra; interface const Changed=2; implementation end.');
  LReply := LProvider.Complete(RequestOf(LChanged));
  Ready(LReply);
  Check((Reuse(LReply).ReusedModules = 2) and (Reuse(LReply).ConstructedModules = 1), 'An unrelated root edit rebuilds only that root');
  LChanged := LChanged.WithBuffer(TNyxPascalSourceRef.IncludeFile(NyxPascalInclude('members.inc')), 'ValueChanged:LongInt;');
  LReply := LProvider.Complete(RequestOf(LChanged));
  Ready(LReply, 'ValueChanged');
  Check((Reuse(LReply).ReusedModules = 2) and (Reuse(LReply).ConstructedModules = 1), 'Editing a used include invalidates its consuming root');
  Check(Location(LReply).DefinitionSource(0) <> Location(LCold).DefinitionSource(0), 'A changed include gets a new navigation owner');
  LChanged := LChanged.WithBuffer(TNyxPascalSourceRef.Main, CMain + ' // current root edit');
  LReply := LProvider.Complete(RequestOf(LChanged));
  Ready(LReply, 'ValueChanged');
  Check((Reuse(LReply).ReusedModules = 2) and (Reuse(LReply).ConstructedModules = 1), 'Editing the main root leaves unrelated analysis reusable');
  LExpanded := CaptureNyxPascalSources(NewNyxPascalProject(NyxPascalUnit('Consumer'), CMain)
    .WithUnit(NyxPascalUnit('Extra'), CExtra).WithUnit(NyxPascalUnit('Added'),
      'unit Added; interface implementation end.')
    .WithInclude(NyxPascalInclude('members.inc'), CMembers));
  LReply := LProvider.Complete(RequestOf(LExpanded));
  Ready(LReply);
  Check((Reuse(LReply).ReusedModules = 0) and (Reuse(LReply).ConstructedModules = 4), 'Catalog membership changes invalidate all source proofs');
  LProvider := nil;
  LSource := nil;
  LChanged := nil;
  Check((LCold.Item(0).Name = 'Value') and
    (Location(LCold).DefinitionSource(0).Source = CMembers), 'Earlier navigation remains exact after edits, eviction and provider retirement');
end;

procedure RetentionAndLoads;
var
  LProvider: INyxPascalCompletionProvider;
  LSource: INyxPascalSourceSnapshot;
  LReply: INyxPascalCompletionResult;
  LOptions: TNyxOwnedPascalCompletionOptions;
  LStorage: TNyxPascalSourceReuseState;
  LAt: Integer;
begin
  LSource := Sources;
  for LAt := 0 to 5 do
  begin
    LOptions := Options;
    case LAt of
      0: LOptions := LOptions.WithSourceReuse(TNyxPascalSourceReuseOptions.Defaults.UpToModules(0));
      1: LOptions := LOptions.WithSourceReuse(TNyxPascalSourceReuseOptions.Defaults.UpToModules(1));
      2: LOptions := LOptions.WithSourceReuse(TNyxPascalSourceReuseOptions.Defaults.UpToTokens(1));
      3: LOptions := LOptions.WithSourceReuse(TNyxPascalSourceReuseOptions.Defaults.UpToCatalogBytes(1));
      4: LOptions := LOptions.WithSourceReuse(TNyxPascalSourceReuseOptions.Defaults.UpToTokens(512));
      5: LOptions := LOptions.WithSourceReuse(TNyxPascalSourceReuseOptions.Defaults.UpToCatalogBytes(
        Files.ByteCount + NyxUTF8ByteCount(CMain) + NyxUTF8ByteCount(CExtra)));
    end;
    LProvider := Provider(LOptions);
    LReply := LProvider.Complete(RequestOf(LSource));
    Ready(LReply);
    LReply := LProvider.Complete(RequestOf(LSource));
    Ready(LReply);
    LStorage := CheckStorage(LProvider, LOptions);

    if LAt = 4 then
      Check((Reuse(LReply).ReusedModules = 2) and (Reuse(LReply).ConstructedModules = 1) and
        (LStorage.Modules = 2), 'A token ceiling keeps small roots while declining the larger module')
    else
      Check((Reuse(LReply).ReusedModules = 0) and (Reuse(LReply).ConstructedModules = 3), 'Disabled or exhausted retention remains an ordinary cold request');

    if LAt = 5 then Check(LStorage.Modules = 1, 'Catalog-byte pressure evicts older entries before publication');

    if LAt = 0 then Check(Reuse(LReply).ReuseWork = 0, 'Disabled reuse performs no directory work');
  end;
  LProvider := Provider(Options, False, True);
  LReply := LProvider.Complete(RequestOf(LSource));
  Ready(LReply);
  Check(Analysis(LReply).DependencyLoads = 1, 'The first indexed request loads one logical dependency');
  LReply := LProvider.Complete(RequestOf(LSource));
  Ready(LReply);
  Check((Analysis(LReply).DependencyLoads = 1) and (Reuse(LReply).ReusedModules = 3), 'Warm indexed reuse still charges its logical dependency load');
end;

procedure TProofStop.Cancel;
begin
  Stopped := True;
end;

function TProofStop.GetCancelled: Boolean;
begin
  Result := Stopped;
end;

procedure TProofStop.Admit(APhase: TNyxPascalAnalysisPhase; AUnits: Integer);
begin

  if APhase = papSourceProof then
  begin

    if AUnits > Remaining then
    begin
      Cancel;
      raise ENyxPascalAnalysisCancelled.Create('Stop at an actual source proof boundary');
    end;
    Dec(Remaining, AUnits);
  end;
  Budget.Admit(APhase, AUnits);
end;

function TProofStop.Receipt: TNyxPascalAnalysisReceipt;
begin
  Result := Budget.Receipt;
end;

procedure TProofStop.Retire;
begin
  Stopped := True;
  Budget.Retire;
end;

procedure WorkAndCancellation;
var
  LProvider: INyxPascalCompletionProvider;
  LCancellable: INyxCancellablePascalCompletionProvider;
  LSource: INyxPascalSourceSnapshot;
  LReply: INyxPascalCompletionResult;
  LMetadata: INyxOwnedPascalCompletionResult;
  LBudget: INyxPascalAnalysisBudget;
  LStop: TProofStop;
  LScope: INyxPascalAnalysisBudget;
  LAt, LFit: Integer;
begin
  LProvider := Provider(Options);
  LSource := Sources;
  Ready(LProvider.Complete(RequestOf(LSource)));
  Check(Supports(LProvider, INyxCancellablePascalCompletionProvider, LCancellable), 'The ordinary provider supports caller-owned cancellation/work');
  LBudget := NewNyxPascalAnalysisBudget(100000);
  try
    LReply := LCancellable.CompleteWithCancellation(RequestOf(LSource), LBudget);
    Ready(LReply);
    LFit := LBudget.Receipt.Total;
  finally
    LBudget.Retire;
  end;
  Check((LFit > 1) and (LFit < 12000), 'This warm request has a bounded measured work requirement');
  for LAt := 1 to LFit - 1 do
  begin
    LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      LReply := LCancellable.CompleteWithCancellation(RequestOf(LSource), LBudget);
      Supports(LReply, INyxOwnedPascalCompletionResult, LMetadata);
      Check((LReply.Status = pcsUnavailable) and (LReply.Count = 0) and
        (LMetadata.Gap = pcgBudget) and (LBudget.Receipt.Total <= LAt),
        'Every smaller warm allowance returns no partial completion rows');
    finally
      LBudget.Retire;
    end;
  end;
  LBudget := NewNyxPascalAnalysisBudget(LFit);
  try
    Ready(LCancellable.CompleteWithCancellation(RequestOf(LSource), LBudget));
    Check(LBudget.Receipt.Total = LFit, 'The exact measured allowance succeeds after every smaller refusal');
  finally
    LBudget.Retire;
  end;
  for LAt := 0 to 3 do
  begin
    LStop := TProofStop.Create;
    LScope := LStop;
    LStop.Budget := NewNyxPascalAnalysisBudget(100000);
    LStop.Remaining := LAt * 100;
    try
      LReply := LCancellable.CompleteWithCancellation(RequestOf(LSource), LScope);
      Check((LReply.Status = pcsCancelled) and (LReply.Count = 0), 'Cancellation inside source proof exposes no partial rows');
    finally
      LScope.Retire;
    end;
  end;
  Ready(LProvider.Complete(RequestOf(LSource)));
  Check(Reuse(LProvider.Complete(RequestOf(LSource))).ReusedModules = 3,
    'Refusal and cancellation leave immutable cached entries reusable');
  WriteLn('warm completion work / first fit=', LFit, ' every smaller allowance refused');
end;

procedure WorldRevalidation;
var
  LProvider: INyxPascalCompletionProvider;
  LSource, LChanged: INyxPascalSourceSnapshot;
  LCold, LWarm, LReply: INyxPascalCompletionResult;
  LColdOnly: INyxPascalCompletionProvider;
begin
  LProvider := Provider(Options, True);
  LSource := Sources;
  LCold := LProvider.Complete(RequestOf(LSource));
  Ready(LCold);
  LWarm := LProvider.Complete(RequestOf(LSource));
  Ready(LWarm);
  Check((Reuse(LWarm).ConstructedModules > 0) and (Reuse(LWarm).ReusedModules = 3), 'The initially missing import stays cold, then its completed dependency permits ordered revalidation');
  Check((Revalidation(LWarm).RevalidatedModules = 1) and
    (Revalidation(LWarm).RevalidatedAnswers = 1), 'A reused conditional module checks its answer in the current world');
  Check(Location(LCold).DefinitionSource(0) = Location(LWarm).DefinitionSource(0), 'A successfully revalidated condition preserves its exact original declaration source');
  LChanged := LSource.WithBuffer(TNyxPascalSourceRef.UnitFile(NyxPascalUnit('Extra')),
    'unit Extra; interface const Changed=2; implementation end.');
  LReply := LProvider.Complete(RequestOf(LChanged));
  Check((LReply.Status = pcsReady) and (LReply.Count = 0), 'A changed imported declaration changes conditional exports in the fresh world');
  Check(Revalidation(LReply).RevalidationDeclines > 0, 'Changed or incomplete dependency answers decline the retained module before normal analysis');
  Check(LCold.Count = 1, 'Reconstructing another world leaves the old detached result unchanged');
  LColdOnly := Provider(Options.WithSourceReuse(TNyxPascalSourceReuseOptions.Defaults.WithSemanticReplay(False)), True);
  Ready(LColdOnly.Complete(RequestOf(LSource)));
  LReply := LColdOnly.Complete(RequestOf(LSource));
  Ready(LReply);
  Check((Reuse(LReply).ReusedModules = 2) and (Revalidation(LReply).RevalidatedModules = 0),
    'The caller can disable semantic replay while retaining independent source reuse');
end;

{$ifndef PAS2JS}
procedure TQueryReader.Execute;
var
  LReply: INyxPascalCompletionResult;
  LAt: Integer;
begin
  try
    for LAt := 1 to 32 do
    begin
      LReply := Provider.Complete(RequestOf(Sources));

      if (LReply.Status <> pcsReady) or (LReply.Count <> 1) or (LReply.Item(0).Name <> Expected) then
        raise Exception.Create('Concurrent response lost its request-local declaration');
      Inc(Iterations);
    end;
  except
    on E: Exception do ErrorText := E.Message;
  end;
end;

procedure ConcurrentReaders;
var
  LProvider: INyxPascalCompletionProvider;
  LReaders: array of TQueryReader;
  LAt: Integer;
  LOptions: TNyxOwnedPascalCompletionOptions;
  LStorage: TNyxPascalSourceReuseState;
begin
  LOptions := Options.WithSourceReuse(TNyxPascalSourceReuseOptions.Defaults.UpToModules(4));
  LProvider := Provider(LOptions);
  SetLength(LReaders, 4);
  try
    for LAt := 0 to High(LReaders) do
    begin
      LReaders[LAt] := TQueryReader.Create(True);
      LReaders[LAt].FreeOnTerminate := False;
      LReaders[LAt].Provider := LProvider;
      LReaders[LAt].Expected := 'Value' + TNyxText(IntToStr(LAt));
      LReaders[LAt].Sources := Sources.WithBuffer(TNyxPascalSourceRef.IncludeFile(
        NyxPascalInclude('members.inc')), LReaders[LAt].Expected + ':LongInt;');
      LReaders[LAt].Start;
    end;
    for LAt := 1 to 16 do
      LStorage := CheckStorage(LProvider, LOptions);
    for LAt := 0 to High(LReaders) do
    begin
      LReaders[LAt].WaitFor;
      Check((LReaders[LAt].ErrorText = '') and (LReaders[LAt].Iterations = 32),
        'A concurrent caller finishes with its own declaration: ' + LReaders[LAt].ErrorText);
    end;
  finally
    for LAt := 0 to High(LReaders) do
    begin

      if LReaders[LAt] <> nil then
      begin
        LReaders[LAt].WaitFor;
        LReaders[LAt].Free;
      end;
    end;
  end;
  Ready(LProvider.Complete(RequestOf(Sources)));
  LStorage := CheckStorage(LProvider, LOptions);
  Check(LStorage.Modules > 0, 'The final shared directory remains usable after concurrent eviction');
  WriteLn('native concurrent completion requests / 128 joined');
end;
{$endif}

begin
  try
    EditsAndLifetime;
    RetentionAndLoads;
    WorldRevalidation;
    WorkAndCancellation;
    {$ifndef PAS2JS}ConcurrentReaders;{$endif}
    WriteLn('PASS ', GChecks, ' source reuse completion checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', GChecks, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
