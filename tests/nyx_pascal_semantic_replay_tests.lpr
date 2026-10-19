{ Chronological semantic revalidation uses the ordinary evaluator and current
  input world. These consumers execute no authored source and qualify no editor
  rollout, compiler admission or complete dependency-aware incremental system. }
program nyx_pascal_semantic_replay_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, {$ifndef PAS2JS}Classes,{$endif} nyx.text, nyx.scheduler,
  nyx.pascal.context, nyx.pascal.files, nyx.pascal.lexing, nyx.pascal.translation,
  nyx.pascal.syntax, nyx.pascal.syntax.translated, nyx.pascal.binding,
  nyx.pascal.conditions, nyx.pascal.work;

const
  CRoot: TNyxText = 'unit Consumer; {$mode delphi}{$h+} interface uses Dependency; ' +
    '{$if declared(Dependency.Flag)}const Ready=1;{$endif}{$i query.inc} implementation end.';
  CInclude: TNyxText = '{$push}{$h-}{$if Dependency.Amount=2}' +
    'type TChosen=record Value:LongInt; end;{$endif}{$pop}';
  CDependency: TNyxText = 'unit Dependency; interface const Flag=1; Amount=2; implementation end.';

type
  { Shape-compatible forwarding cannot impersonate the private construction
    owner. The underlying receipt is real; only its provenance wrapper changes. }
  TAlternateReceipt = class(TInterfacedObject, INyxPascalSemanticTranslation)
  public
    Base: INyxPascalSemanticTranslation;
    function Source: INyxPascalTranslation;
    function WorkTokens: Integer;
    function Passes: Integer;
    function Answers: Integer;
    function Pending: Boolean;
    function Origin: TNyxPascalConditionOrigin;
    function BindingIssue: TNyxPascalBindingIssue;
    function RelatedName: TNyxText;
  end;
  { Caller-owned cancellation at an actual fresh binding step, rather than only
    before the factory starts. The scope retains no source, world or receipt. }
  TBindingStop = class(TInterfacedObject, INyxPascalAnalysisBudget)
  public
    Budget: INyxPascalAnalysisBudget;
    Stopped: Boolean;
    procedure Cancel;
    function GetCancelled: Boolean;
    procedure Admit(APhase: TNyxPascalAnalysisPhase; AUnits: Integer = 1);
    function Receipt: TNyxPascalAnalysisReceipt;
    procedure Retire;
  end;
  {$ifndef PAS2JS}
  { Concurrent revalidation owns independent worlds and results while sharing
    only immutable source/query plans. Main joins before reading any counters. }
  TReplayReader = class(TThread)
  protected
    procedure Execute; override;
  public
    Previous: INyxPascalSemanticTranslation;
    Files: INyxPascalFileCatalog;
    World: INyxPascalSemanticWorld;
    ExpectedMatch: Boolean;
    Iterations: Integer;
    ErrorText: String;
  end;
  {$endif}

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then raise Exception.Create(AReason);
end;

function Facts: INyxPascalCompilerContext;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi).CompleteDefines(True);
end;

function Options: TNyxPascalConditionalOptions;
begin
  Result := TNyxPascalConditionalOptions.Defaults.WithReplayCapture(True);
end;

function Files: INyxPascalFileCatalog;
begin
  Result := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('consumer.pas'), CRoot)
    .WithFile(NyxPascalPhysicalFile('query.inc'), CInclude)
    .WithFile(NyxPascalPhysicalFile('dependency.pas'), CDependency)
    .WithFile(NyxPascalPhysicalFile('unrelated.pas'), 'unit Unrelated; interface implementation end.');
end;

function World(const AFiles: INyxPascalFileCatalog): INyxPascalSemanticWorld;
begin
  Result := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults
    .WithImplicitScopes(pispDeclarationNames)).WithModule(NewNyxPascalTranslatedSyntax(
      NewNyxPascalTranslation(AFiles, NyxPascalPhysicalFile('dependency.pas'), Facts,
        TNyxPascalTranslationOptions.Defaults), TNyxPascalSyntaxOptions.Defaults));
end;

function Analyze(const AFiles: INyxPascalFileCatalog;
  const AOptions: TNyxPascalConditionalOptions): INyxPascalSemanticTranslation;
begin
  Result := NewNyxPascalSemanticTranslation(AFiles, NyxPascalPhysicalFile('consumer.pas'),
    Facts, World(AFiles), TNyxPascalTranslationOptions.Defaults,
    TNyxPascalSyntaxOptions.Defaults, AOptions);
  Check(Result.Source.Resolved and not Result.Pending and (Result.Answers = 2),
    'The real source records two successful ordered semantic queries');
end;

function Replay(const AValue: INyxPascalSemanticTranslation): INyxPascalSemanticReplay;
begin
  Check(Supports(AValue, INyxPascalSemanticReplay, Result), 'The result has an optional detached retention receipt');
end;

function TAlternateReceipt.Source: INyxPascalTranslation; begin Result := Base.Source; end;
function TAlternateReceipt.WorkTokens: Integer; begin Result := Base.WorkTokens; end;
function TAlternateReceipt.Passes: Integer; begin Result := Base.Passes; end;
function TAlternateReceipt.Answers: Integer; begin Result := Base.Answers; end;
function TAlternateReceipt.Pending: Boolean; begin Result := Base.Pending; end;
function TAlternateReceipt.Origin: TNyxPascalConditionOrigin; begin Result := Base.Origin; end;
function TAlternateReceipt.BindingIssue: TNyxPascalBindingIssue; begin Result := Base.BindingIssue; end;
function TAlternateReceipt.RelatedName: TNyxText; begin Result := Base.RelatedName; end;

procedure AnswersAndOrigins;
var
  LFiles, LCurrent, LChanged: INyxPascalFileCatalog;
  LOriginal, LNew, LCold, LNegative: INyxPascalSemanticTranslation;
  LOriginalView, LNewView, LPrefixView: INyxPascalTranslatedSyntax;
  LPrefix: INyxPascalTranslation;
  LAlternate: TAlternateReceipt;
  LWrapper: INyxPascalSemanticTranslation;
  LAt: Integer;
begin
  LFiles := Files;
  LOriginal := Analyze(LFiles, Options);
  Check(Replay(LOriginal).ReplayAvailable and (Replay(LOriginal).ReplayCount = 2) and
    (Replay(LOriginal).RevalidatedAnswers = 0), 'The cold receipt retains all sites without pretending they were replayed');
  LCurrent := LFiles.WithFile(NyxPascalPhysicalFile('unrelated.pas'), 'unit Unrelated; interface const Edited=1; implementation end.');
  LNew := RebindNyxPascalSemanticTranslation(LOriginal, LCurrent, World(LCurrent), Options);
  Check((LNew <> nil) and LNew.Source.Resolved and (LNew.Answers = 2) and
    (Replay(LNew).RevalidatedAnswers = 2), 'A current-world rebind checks both answers in order');
  Check(LNew.Source.Files.TextAt(3) = LCurrent.TextAt(3), 'The accepted source sees the current unrelated buffer');
  Check((LNew.Source.Count = LOriginal.Source.Count) and
    (LNew.Source.StateCount = LOriginal.Source.StateCount), 'Effective token/state counts remain exact');
  for LAt := 0 to LOriginal.Source.Count - 1 do
    Check((LNew.Source.Text(LAt) = LOriginal.Source.Text(LAt)) and
      (LNew.Source.Token(LAt).Activity = LOriginal.Source.Token(LAt).Activity),
      'Revalidated answers preserve exact token spelling and conditional participation');
  LOriginalView := NewNyxPascalTranslatedSyntax(LOriginal.Source, TNyxPascalSyntaxOptions.Defaults);
  LNewView := RebindNyxPascalTranslatedSource(LOriginalView, LNew.Source);
  Check((LNewView <> nil) and (LNewView.Syntax = LOriginalView.Syntax), 'The final proven source can share its immutable syntax');
  Check(RebindNyxPascalFrozenProjection(LOriginalView, LNew.Source) = nil,
    'A final full projection cannot impersonate a construction-time prefix');
  LPrefix := NewNyxPascalTranslationPrefix(LOriginal.Source, LOriginal.Source.Count - 1,
    TNyxPascalTranslationOptions.Defaults);
  LPrefixView := NewNyxPascalTranslatedSyntax(LPrefix, TNyxPascalSyntaxOptions.Defaults);
  Check(RebindNyxPascalFrozenProjection(LPrefixView, LNew.Source) = nil,
    'An arbitrary-cut prefix wrapper cannot acquire frozen replay provenance');
  LCold := Analyze(LCurrent, Options);
  Check(RebindNyxPascalTranslatedSource(LOriginalView, LCold.Source) = nil,
    'Equal fresh source values cannot impersonate the private successful rebind link');
  LChanged := LCurrent.WithFile(NyxPascalPhysicalFile('dependency.pas'),
    'unit Dependency; interface const Flag=1; Amount=3; implementation end.');
  Check(RebindNyxPascalSemanticTranslation(LOriginal, LChanged, World(LChanged), Options) = nil,
    'A changed second answer rejects the entire candidate after the first still matches');
  LChanged := LCurrent.WithFile(NyxPascalPhysicalFile('dependency.pas'),
    'unit Dependency; interface const Other=1; Amount=2; implementation end.');
  Check(RebindNyxPascalSemanticTranslation(LOriginal, LChanged, World(LChanged), Options) = nil,
    'A positive declaration answer cannot survive its imported declaration disappearing');
  LChanged := LCurrent.WithFile(NyxPascalPhysicalFile('consumer.pas'),
    'unit Consumer; {$mode delphi}{$h+} interface uses Dependency; ' +
    '{$if not declared(Dependency.Flag)}const Absent=1;{$endif}{$i query.inc} implementation end.');
  LNegative := Analyze(LChanged, Options);
  Check(RebindNyxPascalSemanticTranslation(LNegative, LChanged, World(LChanged), Options) <> nil,
    'A false semantic condition is freshly checked and remains reusable');
  LChanged := LChanged.WithFile(NyxPascalPhysicalFile('dependency.pas'),
    'unit Dependency; interface const Other=1; Amount=2; implementation end.');
  Check(RebindNyxPascalSemanticTranslation(LNegative, LChanged, World(LChanged), Options) = nil,
    'A negative answer cannot survive its previously present declaration disappearing');
  Check(RebindNyxPascalSemanticTranslation(LOriginal, LCurrent,
    NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults), Options) = nil,
    'An uncaptured imported module yields unknown, never a cached answer');
  LChanged := LCurrent.WithFile(NyxPascalPhysicalFile('query.inc'), CInclude + ' ');
  Check(RebindNyxPascalSemanticTranslation(LOriginal, LChanged, World(LChanged), Options) = nil,
    'Changed physical include input rejects replay before semantic queries');
  LAlternate := TAlternateReceipt.Create;
  LWrapper := LAlternate;
  LAlternate.Base := LOriginal;
  Check(RebindNyxPascalSemanticTranslation(LWrapper, LCurrent, World(LCurrent), Options) = nil,
    'Alternate shape-compatible receipts cannot grant private replay authority');
  LNew := RebindNyxPascalSemanticTranslation(LNew, LCurrent, World(LCurrent), Options);
  Check((LNew <> nil) and Replay(LNew).ReplayAvailable, 'An in-place second-generation receipt preserves its input lease and new prefix lineage');
  LOriginal := nil;
  LWrapper := nil;
  LOriginalView := nil;
  LFiles := nil;
  Check(LNew.Source.Resolved and (Replay(LNew).RevalidatedAnswers = 2), 'Revalidated source/plans outlive earlier receipts and worlds');
end;

procedure Retention;
var
  LFiles: INyxPascalFileCatalog;
  LSource: INyxPascalSemanticTranslation;
  LAt: Integer;
  LOptions: TNyxPascalConditionalOptions;
  LTokens, LBytes: Integer;
begin
  LFiles := Files;
  LSource := Analyze(LFiles, Options);
  LTokens := Replay(LSource).ReplayTokens;
  LBytes := Replay(LSource).ReplayCatalogBytes;
  for LAt := 0 to 4 do
  begin
    LOptions := Options;
    case LAt of
      0: LOptions := LOptions.WithReplayCapture(False);
      1: LOptions := LOptions.UpToReplayTokens(1);
      2: LOptions := LOptions.UpToReplayCatalogBytes(1);
      3: LOptions := LOptions.UpToReplayTokens(LTokens - 1);
      4: LOptions := LOptions.UpToReplayCatalogBytes(LBytes - 1);
    end;
    LSource := Analyze(LFiles, LOptions);
    Check(not Replay(LSource).ReplayAvailable and (Replay(LSource).ReplayCount = 0) and
      (Replay(LSource).ReplayTokens = 0) and (Replay(LSource).ReplayCatalogBytes = 0),
      'Declined retention drops the whole replay capability while preserving normal source meaning');
    Check(RebindNyxPascalSemanticTranslation(LSource, LFiles, World(LFiles), Options) = nil,
      'No partial or disabled recording can be promoted to replay authority');
  end;
  LFiles := LFiles.WithFile(NyxPascalPhysicalFile('consumer.pas'),
    'unit Consumer; interface uses Dependency; ' +
    '{$if declared(Dependency.Flag)}const First=1;{$endif}' +
    'type TUnknown=record Value:LongInt; end; ' +
    '{$if sizeof(TUnknown)>0}const Guessed=1;{$endif} implementation end.');
  LSource := NewNyxPascalSemanticTranslation(LFiles, NyxPascalPhysicalFile('consumer.pas'),
    Facts, World(LFiles), TNyxPascalTranslationOptions.Defaults,
    TNyxPascalSyntaxOptions.Defaults, Options);
  Check(LSource.Pending and not LSource.Source.Resolved and not Replay(LSource).ReplayAvailable,
    'An unknown structural measurement cannot turn earlier recorded answers into a complete replay');
  Check(RebindNyxPascalSemanticTranslation(LSource, LFiles, World(LFiles), Options) = nil,
    'Pending compiler/storage authority stays on the normal explicit fulfillment path');
end;

procedure InPlaceInputs;
var
  LFiles: INyxPascalFileCatalog;
  LOnly: INyxPascalTranslation;
  LView: INyxPascalTranslatedSyntax;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('lone.pas'),
    'unit Lone; interface const Value=1; implementation end.');
  LOnly := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('lone.pas'), Facts,
    TNyxPascalTranslationOptions.Defaults);
  LOnly := RebindNyxPascalTranslation(LOnly, LFiles);
  Check((LOnly <> nil) and LOnly.Resolved, 'A sole source interface can be rebound into its own destination');
  LView := NewNyxPascalTranslatedSyntax(LOnly, TNyxPascalSyntaxOptions.Defaults);
  LOnly := nil;
  LView := RebindNyxPascalTranslatedSyntax(LView, LFiles);
  Check((LView <> nil) and LView.CompleteTranslation, 'A sole projection interface can be rebound in place without retiring its input early');
end;

procedure TBindingStop.Cancel; begin Stopped := True; end;
function TBindingStop.GetCancelled: Boolean; begin Result := Stopped; end;
procedure TBindingStop.Admit(APhase: TNyxPascalAnalysisPhase; AUnits: Integer);
begin

  if APhase = papBindingStep then
  begin
    Cancel;
    raise ENyxPascalAnalysisCancelled.Create('Stop at fresh binding, before accepting a replayed answer');
  end;
  Budget.Admit(APhase, AUnits);
end;
function TBindingStop.Receipt: TNyxPascalAnalysisReceipt; begin Result := Budget.Receipt; end;
procedure TBindingStop.Retire; begin Stopped := True; Budget.Retire; end;

procedure WorkAndCancellation;
var
  LFiles: INyxPascalFileCatalog;
  LWorld: INyxPascalSemanticWorld;
  LOriginal, LAttempt, LKeep: INyxPascalSemanticTranslation;
  LBudget, LScope: INyxPascalAnalysisBudget;
  LStop: TBindingStop;
  LAt, LFit: Integer;
  LCancelled: Boolean;
begin
  LFiles := Files;
  LWorld := World(LFiles);
  LOriginal := Analyze(LFiles, Options);
  LFit := -1;
  for LAt := 1 to 10000 do
  begin
    LAttempt := nil;
    LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      try
        LAttempt := RebindNyxPascalSemanticTranslation(LOriginal, LFiles, LWorld, Options, LBudget);
        Check(LAttempt <> nil, 'A sufficient allowance publishes the complete revalidated source');
        Check((LBudget.Receipt.Used(papSourceToken) = 0) and
          (LBudget.Receipt.Used(papSyntaxToken) = 0) and (LBudget.Receipt.Used(papMetadataNode) = 0),
          'Replay translates no source tokens and reconstructs no syntax/metadata');
        LKeep := LAttempt;
        LFit := LAt;
      except
        on ENyxPascalAnalysisWorkLimit do
          Check((LAttempt = nil) and (LBudget.Receipt.Total <= LAt), 'Every smaller replay allowance refuses atomically');
      end;
    finally
      LBudget.Retire;
    end;

    if LFit > 0 then Break;
  end;
  Check(LFit > 1, 'Replay has a finite measured first fit');
  WriteLn('ordered semantic source work / cold=', LOriginal.WorkTokens, ' replay=', LKeep.WorkTokens);
  WriteLn('ordered semantic replay work / first fit=', LFit, ' every smaller allowance refused');
  Check(LKeep.Source.Resolved, 'Retiring work does not retire the accepted source');
  LStop := TBindingStop.Create;
  LScope := LStop;
  LStop.Budget := NewNyxPascalAnalysisBudget(100000);
  LCancelled := False;
  LAttempt := nil;
  try
    try
      LAttempt := RebindNyxPascalSemanticTranslation(LOriginal, LFiles, LWorld, Options, LScope);
    except
      on ENyxPascalAnalysisCancelled do LCancelled := True;
    end;
    Check(LCancelled and (LAttempt = nil), 'Cancellation inside fresh binding publishes no partial answer/source');
  finally
    LScope.Retire;
  end;
  LAttempt := RebindNyxPascalSemanticTranslation(LOriginal, LFiles, LWorld, Options);
  Check((LAttempt <> nil) and Replay(LOriginal).ReplayAvailable, 'Refusal/cancellation cannot mutate the prior replay source');
end;

{$ifndef PAS2JS}
procedure TReplayReader.Execute;
var
  LReply: INyxPascalSemanticTranslation;
  LAt: Integer;
begin
  try
    for LAt := 1 to 24 do
    begin
      LReply := RebindNyxPascalSemanticTranslation(Previous, Files, World, Options);

      if (LReply <> nil) <> ExpectedMatch then
        raise Exception.Create('A concurrent query borrowed another world''s conditional answer');
      Inc(Iterations);
    end;
  except
    on E: Exception do ErrorText := E.Message;
  end;
end;

procedure ConcurrentReaders;
var
  LFiles: INyxPascalFileCatalog;
  LOriginal: INyxPascalSemanticTranslation;
  LReaders: array of TReplayReader;
  LAt: Integer;
begin
  LFiles := Files;
  LOriginal := Analyze(LFiles, Options);
  SetLength(LReaders, 4);
  try
    for LAt := 0 to High(LReaders) do
    begin
      LReaders[LAt] := TReplayReader.Create(True);
      LReaders[LAt].FreeOnTerminate := False;
      LReaders[LAt].Previous := LOriginal;
      LReaders[LAt].ExpectedMatch := LAt mod 2 = 0;
      LReaders[LAt].Files := LFiles;

      if not LReaders[LAt].ExpectedMatch then
        LReaders[LAt].Files := LFiles.WithFile(NyxPascalPhysicalFile('dependency.pas'),
          'unit Dependency; interface const Flag=1; Amount=3; implementation end.');
      LReaders[LAt].World := World(LReaders[LAt].Files);
      LReaders[LAt].Start;
    end;
    for LAt := 0 to High(LReaders) do
    begin
      LReaders[LAt].WaitFor;
      Check((LReaders[LAt].Iterations = 24) and (LReaders[LAt].ErrorText = ''),
        'Concurrent ordered replay keeps its own world: ' + LReaders[LAt].ErrorText);
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
  WriteLn('native concurrent semantic revalidations / 96 joined');
end;
{$endif}

begin
  try
    AnswersAndOrigins;
    Retention;
    InPlaceInputs;
    WorkAndCancellation;
    {$ifndef PAS2JS}ConcurrentReaders;{$endif}
    WriteLn('PASS ', GChecks, ' ordered semantic replay checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', GChecks, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
