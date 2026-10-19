{ Constructor call contracts must keep the actual receiver and original method
  origins. Binding never evaluates an argument or executes a constructor. }
program nyx_pascal_constructor_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Classes, nyx.text, nyx.editing, nyx.scheduler, nyx.pascal.work, nyx.pascal.lexing,
  nyx.pascal.context, nyx.pascal.files, nyx.pascal.syntax,
  nyx.pascal.syntax.translated, nyx.pascal.translation, nyx.pascal.binding,
  nyx.pascal.names, nyx.pascal.project, nyx.pascal.drafts, nyx.pascal.sources,
  nyx.pascal.completion, nyx.pascal.completion.owned
  {$ifndef PAS2JS}, construction.contracts.fixture{$endif};

type
  { Each cancellation boundary belongs to one query, independently of source,
    world or result ownership. Counted reads also permit deterministic late stops. }
  TQueryStop = class(TInterfacedObject, INyxCancellationScope)
  public
    Reads, StopAt: Integer;
    Stopped: Boolean;
    procedure Cancel;
    function GetCancelled: Boolean;
  end;
  {$ifndef PAS2JS}
  { Independent queries share only a published world. Main joins before reading
    the worker's outcome or releasing that world's immutable receiver values. }
  TConstructorReader = class(TThread)
  protected
    procedure Execute; override;
  public
    Shared: INyxPascalSemanticWorld;
    Site: INyxPascalBindingSite;
    Window: TNyxPascalSpan;
    ErrorText: String;
  end;
  {$endif}

const
  { Portable declaration input for browser consumers. Native qualification
    replaces it with the complete handwritten compiler unit, including bodies. }
  CPortableFixture: TNyxText = 'unit construction.contracts.fixture; interface type ' +
    'TReceiverBadge=class public Value:LongInt; constructor Create; overload; ' +
    'constructor Create(AValue:LongInt); overload; constructor Create(AValue:Boolean); overload; ' +
    'function WithValue(AValue:LongInt):TReceiverBadge; end; ' +
    'TReceiverSpecial=class(TReceiverBadge) Accent:LongInt; end; TReceiverAlias=TReceiverSpecial; ' +
    'TReceiverClass=class of TReceiverBadge; TReceiverBox<T>=class Value:T; constructor Create(const AValue:T); end; ' +
    'TReceiverBadgeBox=class(TReceiverBox<TReceiverBadge>) Extra:LongInt; end; ' +
    'TReceiverPoint=record Value:LongInt; constructor Create(AValue:LongInt); end; ' +
    'TReceiverLegacy=object Value:LongInt; constructor Init(AValue:LongInt); end; ' +
    'TReceiverStartup=class class constructor Start; class destructor Stop; end; ' +
    'function ReceiverClass:TReceiverClass; implementation end.';

var
  GChecks: Integer;
  GFixture: TNyxText;

procedure TQueryStop.Cancel;
begin
  Stopped := True;
end;

function TQueryStop.GetCancelled: Boolean;
begin
  Inc(Reads);
  Result := Stopped or ((StopAt > 0) and (Reads >= StopAt));
end;

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

function View(const AFile, AText: TNyxText;
  const AContext: INyxPascalCompilerContext = nil): INyxPascalTranslatedSyntax;
var
  LContext: INyxPascalCompilerContext;
begin
  LContext := AContext;

  if LContext = nil then LContext := Facts;
  Result := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(
    NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile(AFile), AText),
    NyxPascalPhysicalFile(AFile), LContext, TNyxPascalTranslationOptions.Defaults),
    TNyxPascalSyntaxOptions.Defaults);
end;

function WorldFor(const AExpression: TNyxText;
  const AContext: INyxPascalCompilerContext = nil): INyxPascalSemanticWorld;
begin
  Result := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults)
    .WithModule(View('system.pas', 'unit System; interface type TObject=class end; implementation end.', AContext))
    .WithModule(View('construction.contracts.fixture.pas', GFixture, AContext))
    .WithModule(View('use.pas', 'unit ConstructorUse; interface uses construction.contracts.fixture; ' +
      'var Meta:TReceiverClass; Badge:TReceiverBadge; Legacy:TReceiverLegacy; ' +
      'implementation procedure Run; begin ' + AExpression + '; end; end.', AContext));
end;

function ExpressionSite(const AWorld: INyxPascalSemanticWorld; const AExpression: TNyxText;
  out AWindow: TNyxPascalSpan): INyxPascalBindingSite;
var
  LSource: TNyxText;
  LStart, LFinish: Integer;
  LView: INyxPascalTranslatedSyntax;
  LPhysical: INyxPascalLexicalSnapshot;
begin
  LView := AWorld.ModuleAt(AWorld.Count - 1);
  LPhysical := NyxPascalOccurrenceLexical(LView.Source, 0);
  LSource := LPhysical.Slice(NyxPascalSpan(0, LPhysical.Scalars));
  LStart := Length(LSource) - Length(AExpression);
  while (LStart >= 0) and (Copy(LSource, LStart + 1, Length(AExpression)) <> AExpression) do Dec(LStart);
  Check(LStart >= 0, 'Expression belongs to the current module');
  LFinish := NyxTextScalarCount(Copy(LSource, 1, LStart + Length(AExpression)));
  LStart := NyxTextScalarCount(Copy(LSource, 1, LStart));
  LFinish := LView.VirtualAt(0, LFinish);
  LStart := LView.VirtualAt(0, LStart);
  Check((LStart >= 0) and (LFinish > LStart), 'Expression maps to its current semantic projection');
  AWindow := NyxPascalSpan(LStart, LFinish - LStart);
  Result := AWorld.At(AWorld.Count - 1, LView.Syntax.ScopeAt(LStart), LFinish);
end;

function Resolve(const AWorld: INyxPascalSemanticWorld; const AExpression: TNyxText;
  out AAccess: TNyxPascalMemberAccess): INyxPascalResolution;
var
  LQueries: INyxPascalExpressionQueries;
  LSite: INyxPascalBindingSite;
  LWindow: TNyxPascalSpan;
begin
  LSite := ExpressionSite(AWorld, AExpression, LWindow);
  Supports(AWorld, INyxPascalExpressionQueries, LQueries);
  Result := LQueries.ExpressionType(LSite, LWindow, AAccess);
end;

function Terminal(const AType: INyxPascalSemanticType): INyxPascalSemanticType;
begin
  Result := AType;
  while (Result <> nil) and (Result.Kind = pstAlias) do Result := Result.Target;
end;

procedure Contract(const AExpression, AExpected: TNyxText);
var
  LWorld: INyxPascalSemanticWorld;
  LFound: INyxPascalResolution;
  LType: INyxPascalSemanticType;
  LAccess: TNyxPascalMemberAccess;
begin
  LWorld := WorldFor(AExpression);
  LFound := Resolve(LWorld, AExpression, LAccess);
  Check((LFound.Status = pbsBound) and (LFound.BoundType <> nil),
    'Expected a constructor result for ' + String(AExpression) + ': ' + String(LFound.MessageText));
  LType := Terminal(LFound.BoundType);
  Check((LType.Kind = pstDeclared) and (LType.Definition <> nil) and
    (LType.Definition.Declaration.Name = AExpected) and (LAccess = pmaInstance),
    'The actual receiver controls the constructor result: ' + String(AExpression));
end;

procedure ScalarContract(const AExpression: TNyxText);
var
  LWorld: INyxPascalSemanticWorld;
  LFound: INyxPascalResolution;
  LType: INyxPascalSemanticType;
  LAccess: TNyxPascalMemberAccess;
begin
  LWorld := WorldFor(AExpression);
  LFound := Resolve(LWorld, AExpression, LAccess);
  LType := Terminal(LFound.BoundType);
  Check((LFound.Status = pbsBound) and (LType <> nil) and
    (LType.Builtin = pbbLongInt) and (LAccess = pmaInstance),
    'A constructor chain reaches the original typed field: ' + String(AExpression));
end;

procedure Refusal(const AExpression: TNyxText);
var
  LWorld: INyxPascalSemanticWorld;
  LFound: INyxPascalResolution;
  LAccess: TNyxPascalMemberAccess;
begin
  LWorld := WorldFor(AExpression);
  LFound := Resolve(LWorld, AExpression, LAccess);
  Check((LFound.Status <> pbsBound) and (LFound.BoundType = nil),
    'Unsupported construction cannot invent a value: ' + String(AExpression));
end;

procedure ContextRefusal;
const
  CExpression: TNyxText = 'TReceiverSpecial.Create(42)';
var
  LWorld: INyxPascalSemanticWorld;
  LFound: INyxPascalResolution;
  LAccess: TNyxPascalMemberAccess;
begin
  LWorld := WorldFor(CExpression, NewNyxPascalCompilerContext(ppcUnknown,
    TNyxPascalCompilerVersion.Create(0, 0, 0), ppmUnknown).CompleteDefines(True));
  LFound := Resolve(LWorld, CExpression, LAccess);
  Check((LFound.Status <> pbsBound) and (LFound.BoundType = nil),
    'An unknown compiler grants no qualified constructor result');
  LWorld := WorldFor(CExpression, NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 2, 2), ppmDelphi).CompleteDefines(True));
  LFound := Resolve(LWorld, CExpression, LAccess);
  Check((LFound.Status <> pbsBound) and (LFound.BoundType = nil),
    'An unqualified compiler version retains its explicit context gap');
end;

procedure OwnershipAndWork;
const
  CExpression: TNyxText = 'TReceiverBadgeBox.Create(Badge).Value.WithValue(42)';
var
  LWorld, LChanged: INyxPascalSemanticWorld;
  LSite: INyxPascalBindingSite;
  LWindow: TNyxPascalSpan;
  LQueries: INyxPascalExpressionQueries;
  LAttempt, LRetained, LMethod: INyxPascalResolution;
  LBudget: INyxPascalAnalysisBudget;
  LAccess: TNyxPascalMemberAccess;
  LAt, LFit: Integer;
  LStop: TQueryStop;
  LCancel: INyxCancellationScope;
  LRefused: Boolean;
begin
  LWorld := WorldFor(CExpression);
  LSite := ExpressionSite(LWorld, CExpression, LWindow);
  Supports(LWorld, INyxPascalExpressionQueries, LQueries);
  LFit := -1;
  for LAt := 1 to 5000 do
  begin
    LAttempt := nil;
    LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      try
        LAttempt := LQueries.ExpressionType(LSite, LWindow, LAccess, LBudget);
        Check((LAttempt.Status = pbsBound) and (LAttempt.BoundType <> nil), 'A sufficient current allowance resolves the constructor chain');
        LRetained := LAttempt;
        LFit := LAt;
      except
        on ENyxPascalAnalysisWorkLimit do
          Check((LAttempt = nil) and (LBudget.Receipt.Total <= LAt), 'Every smaller constructor allowance refuses atomically');
      end;
    finally
      LBudget.Retire;
    end;

    if LFit > 0 then Break;
  end;
  Check(LFit > 1, 'The constructor query has a finite bounded first fit');
  WriteLn('constructor contract work / first fit=', LFit, ' every smaller allowance refused');
  for LAt := 1 to 80 do
  begin
    LStop := TQueryStop.Create;
    LCancel := LStop;
    LStop.StopAt := LAt;
    LAttempt := nil;
    LRefused := False;
    try
      LAttempt := LQueries.ExpressionType(LSite, LWindow, LAccess, LCancel);
    except
      on ENyxPascalAnalysisCancelled do LRefused := True;
    end;
    Check(LRefused and (LAttempt = nil), 'Cancellation at each admitted early boundary publishes no constructor result');
  end;
  WriteLn('constructor cancellation / 80 boundaries refused');
  LMethod := LWorld.Lookup(LSite, NyxPascalBindingName('TReceiverSpecial.Create'));
  Check(LMethod.Status = pbsOverloads, 'All original constructor overloads remain visible');
  for LAt := 0 to LMethod.Count - 1 do
    Check((LMethod.SymbolAt(LAt).Declaration.RoutineKind = prkConstructor) and
      (LWorld.ModuleAt(LMethod.SymbolAt(LAt).ModuleIndex).Source.Files
        .FileAt(LMethod.SymbolAt(LAt).Origin.FileIndex).Name = TNyxText('construction.contracts.fixture.pas')),
      'Receiver ownership retains each original constructor declaration and physical source');
  LChanged := LWorld.WithModule(View('use.pas', 'unit ConstructorUse; interface implementation end.'));
  LAttempt := nil;
  LRefused := False;
  Supports(LChanged, INyxPascalExpressionQueries, LQueries);
  try
    LAttempt := LQueries.ExpressionType(LSite, LWindow, LAccess);
  except
    on ENyxPascalBinding do LRefused := True;
  end;
  Check(LRefused and (LAttempt = nil), 'A changed world refuses an earlier constructor site');
  LChanged := nil;
  LQueries := nil;
  LWorld := nil;
  Check((LRetained.World = LSite.World) and (Terminal(LRetained.BoundType).Definition.Declaration.Name = TNyxText('TReceiverBadge')),
    'Retained constructor result types survive budget and caller-world retirement');
end;

procedure CompletionPath(const AExpression, AExpected: TNyxText);
var
  LFiles: INyxPascalFileCatalog;
  LRoots: array of TNyxPascalPhysicalFile;
  LProvider: INyxPascalCompletionProvider;
  LSources, LPreview: INyxPascalSourceSnapshot;
  LRequest: INyxPascalCompletionRequest;
  LResult: INyxPascalCompletionResult;
  LMetadata: INyxOwnedPascalCompletionResult;
  LText: TNyxText;
  LStart, LCaret, LNewCaret: Integer;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('sdk/system.pas'), 'unit System; interface type TObject=class end; implementation end.')
    .WithFile(NyxPascalPhysicalFile('sdk/construction.contracts.fixture.pas'), GFixture);
  SetLength(LRoots, 2);
  LRoots[0] := NyxPascalPhysicalFile('sdk/system.pas');
  LRoots[1] := NyxPascalPhysicalFile('sdk/construction.contracts.fixture.pas');
  LProvider := NewNyxOwnedPascalCompletionProvider(Facts, LFiles, LRoots,
    TNyxOwnedPascalCompletionOptions.Defaults);
  LText := 'unit Consumer; interface uses construction.contracts.fixture; var Badge:TReceiverBadge; ' +
    'implementation procedure Run; begin ' + AExpression;
  LSources := CaptureNyxPascalSources(NewNyxPascalProject(NyxPascalUnit('Consumer'), LText));
  LCaret := NyxTextScalarCount(LText);
  LRequest := NewNyxPascalCompletionRequest(LSources).At(TNyxPascalSourceRef.Main, LCaret);
  LResult := LProvider.Complete(LRequest);
  Check((LResult.Status = pcsReady) and (LResult.Count = 1) and (LResult.Total = 1),
    'Ordinary completion follows the constructor receiver: ' + String(AExpression) + ' / ' + String(LResult.MessageText));
  Check(LResult.Item(0).Name = AExpected, 'Constructor completion retains the specialized member name');
  Supports(LResult, INyxOwnedPascalCompletionResult, LMetadata);
  LProvider := nil;
  Check((LMetadata.Definition(0).FileReference.Name = TNyxText('sdk/construction.contracts.fixture.pas')) and
    (LMetadata.DefinitionSource(0).Slice(LMetadata.Definition(0).Span) = AExpected),
    'Detached completion navigates the exact original field after provider retirement');
  LPreview := LResult.Preview(0, LSources, LNewCaret);
  LStart := NyxTextScalarCount(LResult.ExpectedText);
  Check((LNewCaret = LCaret + NyxTextScalarCount(AExpected) - LStart) and
    (LPreview.Text(TNyxPascalSourceRef.Main) = Copy(LText, 1, Length(LText) - Length(LResult.ExpectedText)) + AExpected),
    'Detached insertion preserves the complete unfinished constructor chain');
end;

{$ifndef PAS2JS}
procedure TConstructorReader.Execute;
var
  LFound: INyxPascalResolution;
  LQueries: INyxPascalExpressionQueries;
  LAccess: TNyxPascalMemberAccess;
  LAt: Integer;
begin
  try
    Supports(Shared, INyxPascalExpressionQueries, LQueries);
    for LAt := 1 to 16 do
    begin
      LFound := LQueries.ExpressionType(Site, Window, LAccess);

      if (LFound.Status <> pbsBound) or (LFound.BoundType = nil) or
        (Terminal(LFound.BoundType).Definition.Declaration.Name <> TNyxText('TReceiverBadge')) then
        raise Exception.Create('Concurrent receiver type changed');
    end;
  except
    on E: Exception do ErrorText := E.Message;
  end;
end;

procedure ConcurrentReaders;
const
  CExpression: TNyxText = 'TReceiverBadgeBox.Create(Badge).Value';
var
  LWorld: INyxPascalSemanticWorld;
  LSite: INyxPascalBindingSite;
  LWindow: TNyxPascalSpan;
  LReaders: array[0..3] of TConstructorReader;
  LAt: Integer;
begin
  LWorld := WorldFor(CExpression);
  LSite := ExpressionSite(LWorld, CExpression, LWindow);
  for LAt := 0 to High(LReaders) do
  begin
    LReaders[LAt] := TConstructorReader.Create(True);
    LReaders[LAt].Shared := LWorld;
    LReaders[LAt].Site := LSite;
    LReaders[LAt].Window := LWindow;
  end;
  try
    for LAt := 0 to High(LReaders) do LReaders[LAt].Start;
    for LAt := 0 to High(LReaders) do LReaders[LAt].WaitFor;
    for LAt := 0 to High(LReaders) do Check(LReaders[LAt].ErrorText = '', 'Joined constructor reader: ' + LReaders[LAt].ErrorText);
  finally
    for LAt := 0 to High(LReaders) do LReaders[LAt].Free;
  end;
  WriteLn('native concurrent constructor queries / 64 joined');
end;
{$endif}

begin
  try
    GFixture := CPortableFixture;
    {$ifndef PAS2JS}
    Check(CompilerConstructorValue = 42, 'The handwritten fixture executes with the pinned native compiler');
    WriteLn('native constructor fixture / value=42 invocations=', ConstructorInvocations);
    with TStringList.Create do
    try
      LoadFromFile('tests/construction.contracts.fixture.pas');
      GFixture := TNyxText(Text);
    finally
      Free;
    end;
    {$endif}
    Contract('TReceiverBadge.Create(42)', 'TReceiverBadge');
    Contract('TReceiverSpecial.Create', 'TReceiverSpecial');
    Contract('TReceiverSpecial.Create().WithValue(42)', 'TReceiverBadge');
    Contract('TReceiverSpecial.Create(42)', 'TReceiverSpecial');
    Contract('TReceiverAlias.Create(42)', 'TReceiverSpecial');
    Contract('Meta.Create(42)', 'TReceiverBadge');
    Contract('ReceiverClass().Create(42)', 'TReceiverBadge');
    Contract('TReceiverPoint.Create(42)', 'TReceiverPoint');
    Contract('TReceiverBox<TReceiverBadge>.Create(Badge)', 'TReceiverBox');
    Contract('TReceiverBadgeBox.Create(Badge)', 'TReceiverBadgeBox');
    ScalarContract('TReceiverSpecial.Create.Accent');
    ScalarContract('(TReceiverSpecial.Create(42)).Accent');
    ScalarContract('TReceiverBadgeBox.Create(Badge).Value.Value');
    ScalarContract('TReceiverBox<TReceiverBadge>.Create(Badge).Value.Value');
    ScalarContract('TReceiverPoint.Create(42).Value');
    Refusal('TReceiverStartup.Start()');
    Refusal('TReceiverStartup.Stop()');
    Refusal('Legacy.Init(42)');
    Refusal('TReceiverLegacy.Init(42)');
    Refusal('Badge.Destroy()');
    Refusal('TReceiverSpecial.Create(,)');
    Refusal('TReceiverSpecial.Create(42, )');
    Refusal('TReceiverBox<>.Create(Badge)');
    Refusal('TReceiverBox<Missing>.Create(Badge)');
    ContextRefusal;
    OwnershipAndWork;
    CompletionPath('TReceiverSpecial.Create(42).Ac', 'Accent');
    CompletionPath('TReceiverSpecial.Create.Ac', 'Accent');
    CompletionPath('TReceiverBadgeBox.Create(Badge).Value.Val', 'Value');
    CompletionPath('TReceiverBox<TReceiverBadge>.Create(Badge).Value.Val', 'Value');
    {$ifndef PAS2JS}
    ConcurrentReaders;
    Check(ConstructorInvocations = 7, 'Source analysis never invokes the real constructor bodies');
    {$endif}
    WriteLn('PASS ', GChecks, ' constructor receiver contract checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', GChecks, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
