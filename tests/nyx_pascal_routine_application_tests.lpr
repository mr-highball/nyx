{ Explicit routine type arguments must flow through declared return contracts,
  ordinary completion and original source origins without evaluating arguments. }
program nyx_pascal_routine_application_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses SysUtils, Classes, nyx.text, nyx.editing, nyx.scheduler, nyx.pascal.work, nyx.pascal.names,
  nyx.pascal.files, nyx.pascal.context, nyx.pascal.lexing,
  nyx.pascal.translation, nyx.pascal.syntax, nyx.pascal.syntax.translated,
  nyx.pascal.binding, nyx.pascal.project, nyx.pascal.drafts, nyx.pascal.sources,
  nyx.pascal.completion, nyx.pascal.completion.owned
  {$ifndef PAS2JS}, application.generics.fixture{$endif};

{$ifndef PAS2JS}
type
  { Every thread owns its query state and result; only the immutable world/site
    and scalar window are shared. Main joins before inspecting outcome fields. }
  TApplicationReader = class(TThread)
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
  { Browser consumers use portable declaration input. Native qualification
    replaces it with the complete handwritten compiled unit, including bodies. }
  CPortableFixture: TNyxText = 'unit application.generics.fixture; interface type ' +
    'TAppliedBadge=class Value:LongInt; function WithValue(AValue:LongInt):TAppliedBadge; ' +
    'function Choose<T>(const AValue:T):T; end; TAppliedBox<T>=class Value:T; end; ' +
    'TAppliedCalls=class class function Keep<T>(const AValue:T):T; static; end; ' +
    'IAppliedValue=interface function ReadValue:LongInt; end; ' +
    'function KeepValue<T>(const AValue:T):T; function BoxValue<T>(const AValue:T):TAppliedBox<T>; ' +
    'function KeepBadge<T:TAppliedBadge>(const AValue:T):T; ' +
    'function RetypeValue<T,U>(const AValue:T;const AOther:U):TAppliedBox<U>; ' +
    'function CommonValue<T>(const AValue:T;AMarker:LongInt):T;overload; ' +
    'function CommonValue<T>(const AValue:T;AMarker:Boolean):T;overload; ' +
    'function DifferentValue<T>(const AValue:T;AMarker:LongInt):T;overload; ' +
    'function DifferentValue<T>(const AValue:T;AMarker:Boolean):TAppliedBox<T>;overload; implementation end.';

var
  GChecks: Integer;
  GFixture: TNyxText;

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

function WorldFor(const AExpression: TNyxText; AFunctionOwner: Boolean = False;
  const AExtraTypes: TNyxText = ''; const AContext: INyxPascalCompilerContext = nil): INyxPascalSemanticWorld;
var
  LBody: TNyxText;
begin
  LBody := 'procedure Run;';

  if AFunctionOwner then LBody := 'function Run:TAppliedBadge;';
  Result := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults)
    .WithModule(View('system.pas', 'unit System; interface type TObject=class end; ' +
      'TInterfacedObject=class(TObject) end; implementation end.', AContext))
    .WithModule(View('application.generics.fixture.pas', GFixture, AContext))
    .WithModule(View('use.pas', 'unit ApplicationUse; interface uses application.generics.fixture; ' +
      AExtraTypes + ' ' +
      'var Badge:TAppliedBadge; Box:TAppliedBox<TAppliedBadge>; Shared:IAppliedValue; implementation ' + LBody + ' begin ' +
      AExpression + '; end; end.', AContext));
end;

function ExpressionSite(const AWorld: INyxPascalSemanticWorld; const AExpression: TNyxText;
  out AWindow: TNyxPascalSpan): INyxPascalBindingSite;
var
  LView: INyxPascalTranslatedSyntax;
  LPhysical: INyxPascalLexicalSnapshot;
  LText: TNyxText;
  LStart, LFinish: Integer;
begin
  LView := AWorld.ModuleAt(AWorld.Count - 1);
  LPhysical := NyxPascalOccurrenceLexical(LView.Source, 0);
  LText := LPhysical.Slice(NyxPascalSpan(0, LPhysical.Scalars));
  LStart := Length(LText) - Length(AExpression);
  while (LStart >= 0) and (Copy(LText, LStart + 1, Length(AExpression)) <> AExpression) do Dec(LStart);
  Check(LStart >= 0, 'Expression belongs to the current owning file');
  LFinish := LView.VirtualAt(0, NyxTextScalarCount(Copy(LText, 1, LStart + Length(AExpression))));
  LStart := LView.VirtualAt(0, NyxTextScalarCount(Copy(LText, 1, LStart)));
  Check((LStart >= 0) and (LFinish > LStart), 'Exact argument/call boundaries map into the current semantic view');
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
    'Explicit generic application resolves: ' + String(AExpression) + ' / ' + String(LFound.MessageText));
  LType := LFound.BoundType;
  while (LType <> nil) and (LType.Kind = pstAlias) do LType := LType.Target;
  Check((LType <> nil) and (LType.Definition <> nil) and
    (LType.Definition.Declaration.Name = AExpected) and (LAccess = pmaInstance),
    'The explicit argument retains its exact declared result contract');
end;

procedure Refusal(const AExpression: TNyxText; AFunctionOwner: Boolean = False);
var
  LWorld: INyxPascalSemanticWorld;
  LFound: INyxPascalResolution;
  LAccess: TNyxPascalMemberAccess;
begin
  LWorld := WorldFor(AExpression, AFunctionOwner);
  LFound := Resolve(LWorld, AExpression, LAccess);
  Check((LFound.Status <> pbsBound) and (LFound.BoundType = nil),
    'Unsupported application cannot invent a result: ' + String(AExpression));
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
  LType := LFound.BoundType;
  while (LType <> nil) and (LType.Kind = pstAlias) do LType := LType.Target;
  Check((LFound.Status = pbsBound) and (LType <> nil) and (LType.Builtin = pbbLongInt),
    'A generic call chain reaches its original scalar field: ' + String(AExpression));
end;

procedure CompletionPath(const AExpression, AExpected: TNyxText);
var
  LProvider: INyxPascalCompletionProvider;
  LFiles: INyxPascalFileCatalog;
  LRoots: array of TNyxPascalPhysicalFile;
  LSources, LPreview: INyxPascalSourceSnapshot;
  LRequest: INyxPascalCompletionRequest;
  LReply: INyxPascalCompletionResult;
  LOrigins: INyxOwnedPascalCompletionResult;
  LText: TNyxText;
  LCaret, LNewCaret: Integer;
  LMissing: INyxPascalCompletionProvider;
  LRefusal: INyxPascalCompletionResult;
  LGap: INyxOwnedPascalCompletionResult;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('sdk/system.pas'), 'unit System; interface type TObject=class end; ' +
      'TInterfacedObject=class(TObject) end; implementation end.')
    .WithFile(NyxPascalPhysicalFile('sdk/application.generics.fixture.pas'), GFixture);
  SetLength(LRoots, 2);
  LRoots[0] := NyxPascalPhysicalFile('sdk/system.pas');
  LRoots[1] := NyxPascalPhysicalFile('sdk/application.generics.fixture.pas');
  { This small authored fixture has no complete captured RTL. Match the direct
    world's explicit declaration-name policy; default captured-scope refusal is
    qualified separately below instead of inventing missing ObjPas exports. }
  LProvider := NewNyxOwnedPascalCompletionProvider(Facts, LFiles, LRoots,
    TNyxOwnedPascalCompletionOptions.Defaults.WithBinding(
      TNyxPascalBindingOptions.Defaults.WithImplicitScopes(pispDeclarationNames)));
  LText := 'unit Consumer; interface uses application.generics.fixture; ' +
    'var Badge:TAppliedBadge; Box:TAppliedBox<TAppliedBadge>; implementation procedure Run; begin ' + AExpression;
  LSources := CaptureNyxPascalSources(NewNyxPascalProject(NyxPascalUnit('Consumer'), LText));
  LCaret := NyxTextScalarCount(LText);
  LRequest := NewNyxPascalCompletionRequest(LSources).At(TNyxPascalSourceRef.Main, LCaret);

  if Pos('LongInt', AExpression) > 0 then
  begin
    LMissing := NewNyxOwnedPascalCompletionProvider(Facts, LFiles, LRoots, TNyxOwnedPascalCompletionOptions.Defaults);
    LRefusal := LMissing.Complete(LRequest);
    Supports(LRefusal, INyxOwnedPascalCompletionResult, LGap);
    Check((LRefusal.Status = pcsUnavailable) and (LRefusal.Count = 0) and
      (LGap.BindingIssue = pbiMissingModule) and (LGap.RelatedName = TNyxText('ObjPas')),
      'Default captured-scope policy retains the real missing implicit-unit gap instead of manufacturing RTL exports');
  end;
  LReply := LProvider.Complete(LRequest);
  Check((LReply.Status = pcsReady) and (LReply.Count = 1) and (LReply.Total = 1),
    'Ordinary completion follows an explicit routine application: ' + String(AExpression) + ' / ' + String(LReply.MessageText));
  Check(LReply.Item(0).Name = AExpected, 'The application preserves its specialized member name');
  Supports(LReply, INyxOwnedPascalCompletionResult, LOrigins);
  LProvider := nil;
  Check((LOrigins.Definition(0).FileReference.Name = TNyxText('sdk/application.generics.fixture.pas')) and
    (LOrigins.DefinitionSource(0).Slice(LOrigins.Definition(0).Span) = AExpected),
    'Detached completion retains exact original method/field navigation after provider retirement');
  LPreview := LReply.Preview(0, LSources, LNewCaret);
  Check((LNewCaret = LCaret + NyxTextScalarCount(AExpected) - NyxTextScalarCount(LReply.ExpectedText)) and
    (LPreview.Text(TNyxPascalSourceRef.Main) = Copy(LText, 1, Length(LText) - Length(LReply.ExpectedText)) + AExpected),
    'Exact insertion changes only the final member prefix in the unfinished chain');
end;

procedure ContextAndCompilerRefusal;
const
  CExpression: TNyxText = 'KeepValue<TAppliedBadge>(Badge)';
  CNested: TNyxText = 'TUnsupported<TAppliedBadge>.Map<LongInt>(42)';
var
  LWorld: INyxPascalSemanticWorld;
  LFound: INyxPascalResolution;
  LAccess: TNyxPascalMemberAccess;
begin
  LWorld := WorldFor(CNested, False, 'type TUnsupported<T>=class ' +
    'class function Map<U>(const AValue:U):U;static;end;');
  LFound := Resolve(LWorld, CNested, LAccess);
  Check((LFound.Status <> pbsBound) and (LFound.BoundType = nil),
    'The actual compiler-rejected nested generic declaration grants no call result');
  LWorld := WorldFor(CExpression, False, '', NewNyxPascalCompilerContext(ppcUnknown,
    TNyxPascalCompilerVersion.Create(0, 0, 0), ppmUnknown).CompleteDefines(True));
  LFound := Resolve(LWorld, CExpression, LAccess);
  Check((LFound.Status <> pbsBound) and (LFound.BoundType = nil), 'Unknown compiler context grants no routine application');
  LWorld := WorldFor(CExpression, False, '', NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 2, 2), ppmDelphi).CompleteDefines(True));
  LFound := Resolve(LWorld, CExpression, LAccess);
  Check((LFound.Status <> pbsBound) and (LFound.BoundType = nil), 'An unqualified version does not inherit application eligibility');
end;

procedure OwnershipAndWork;
const
  CExpression: TNyxText = 'KeepValue<TAppliedBadge>(Badge).Choose<TAppliedBox<TAppliedBadge>>(Box).Value.WithValue(42)';
var
  LWorld, LChanged: INyxPascalSemanticWorld;
  LQueries: INyxPascalExpressionQueries;
  LSite: INyxPascalBindingSite;
  LWindow: TNyxPascalSpan;
  LAttempt, LRetained: INyxPascalResolution;
  LBudget: INyxPascalAnalysisBudget;
  LCancel: INyxCancellationScope;
  LAccess: TNyxPascalMemberAccess;
  LAt, LFit: Integer;
  LRefused: Boolean;
begin
  LWorld := WorldFor(CExpression);
  LSite := ExpressionSite(LWorld, CExpression, LWindow);
  Supports(LWorld, INyxPascalExpressionQueries, LQueries);
  LFit := -1;
  for LAt := 1 to 10000 do
  begin
    LAttempt := nil;
    LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      try
        LAttempt := LQueries.ExpressionType(LSite, LWindow, LAccess, LBudget);
        Check((LAttempt.Status = pbsBound) and (LAttempt.BoundType <> nil), 'First sufficient application allowance resolves the complete current chain');
        LRetained := LAttempt;
        LFit := LAt;
      except
        on ENyxPascalAnalysisWorkLimit do
          Check((LAttempt = nil) and (LBudget.Receipt.Total <= LAt), 'Every smaller application allowance refuses atomically');
      end;
    finally
      LBudget.Retire;
    end;

    if LFit > 0 then Break;
  end;
  Check(LFit > 1, 'A generic routine query has a finite bounded first fit');
  WriteLn('generic application work / first fit=', LFit, ' every smaller allowance refused');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LAttempt := nil;
  LRefused := False;
  try
    LAttempt := LQueries.ExpressionType(LSite, LWindow, LAccess, LCancel);
  except
    on ENyxPascalAnalysisCancelled do LRefused := True;
  end;
  Check(LRefused and (LAttempt = nil), 'Cancelled application queries publish no type/frame');
  LChanged := LWorld.WithModule(View('use.pas', 'unit ApplicationUse; interface implementation end.'));
  Supports(LChanged, INyxPascalExpressionQueries, LQueries);
  LAttempt := nil;
  LRefused := False;
  try
    LAttempt := LQueries.ExpressionType(LSite, LWindow, LAccess);
  except
    on ENyxPascalBinding do LRefused := True;
  end;
  Check(LRefused and (LAttempt = nil), 'A replacement world refuses an older application site');
  Check(LRetained.World = LSite.World, 'Retained frames/types keep their original immutable world after work retirement');
end;

{$ifndef PAS2JS}
procedure TApplicationReader.Execute;
var
  LQueries: INyxPascalExpressionQueries;
  LFound: INyxPascalResolution;
  LAccess: TNyxPascalMemberAccess;
  LAt: Integer;
  LType: INyxPascalSemanticType;
begin
  try
    Supports(Shared, INyxPascalExpressionQueries, LQueries);
    for LAt := 1 to 16 do
    begin
      LFound := LQueries.ExpressionType(Site, Window, LAccess);

      if (LFound.Status <> pbsBound) or (LFound.BoundType = nil) then
        raise Exception.Create('Concurrent application lost its frame');
      LType := LFound.BoundType;
      while (LType <> nil) and (LType.Kind = pstAlias) do LType := LType.Target;

      if (LType = nil) or (LType.Definition = nil) or
        (LType.Definition.Declaration.Name <> TNyxText('TAppliedBadge')) then
        raise Exception.Create('Concurrent application changed its concrete argument');
    end;
  except
    on E: Exception do ErrorText := E.Message;
  end;
end;

procedure ConcurrentReaders;
const
  CExpression: TNyxText = 'BoxValue<TAppliedBadge>(Badge).Value';
var
  LReaders: array[0..3] of TApplicationReader;
  LWorld: INyxPascalSemanticWorld;
  LSite: INyxPascalBindingSite;
  LWindow: TNyxPascalSpan;
  LAt: Integer;
begin
  LWorld := WorldFor(CExpression);
  LSite := ExpressionSite(LWorld, CExpression, LWindow);
  for LAt := 0 to High(LReaders) do
  begin
    LReaders[LAt] := TApplicationReader.Create(True);
    LReaders[LAt].Shared := LWorld;
    LReaders[LAt].Site := LSite;
    LReaders[LAt].Window := LWindow;
  end;
  try
    for LAt := 0 to High(LReaders) do LReaders[LAt].Start;
    for LAt := 0 to High(LReaders) do LReaders[LAt].WaitFor;
    for LAt := 0 to High(LReaders) do Check(LReaders[LAt].ErrorText = '', 'Joined generic application reader: ' + LReaders[LAt].ErrorText);
  finally
    for LAt := 0 to High(LReaders) do LReaders[LAt].Free;
  end;
  WriteLn('native concurrent generic applications / 64 joined');
end;
{$endif}

begin
  try
    GFixture := CPortableFixture;
    {$ifndef PAS2JS}
    Check(CompilerApplicationValue = 42, 'The complete handwritten fixture executes its real generic calls to 42');
    WriteLn('native generic application fixture / value=42 invocations=', ApplicationInvocations);
    with TStringList.Create do
    try
      LoadFromFile('tests/application.generics.fixture.pas');
      GFixture := TNyxText(Text);
    finally
      Free;
    end;
    {$endif}
    Contract('KeepValue<TAppliedBadge>(Badge)', 'TAppliedBadge');
    Contract('BoxValue<TAppliedBadge>(Badge)', 'TAppliedBox');
    Contract('KeepBadge<TAppliedBadge>(Badge)', 'TAppliedBadge');
    Contract('TAppliedCalls.Keep<TAppliedBadge>(Badge)', 'TAppliedBadge');
    Contract('Badge.Choose<TAppliedBadge>(Badge)', 'TAppliedBadge');
    Contract('KeepValue<IAppliedValue>(Shared)', 'IAppliedValue');
    Contract('CommonValue<TAppliedBadge>(Badge, True)', 'TAppliedBadge');
    Contract('RetypeValue<LongInt, TAppliedBadge>(42, Badge)', 'TAppliedBox');
    Contract('application.generics.fixture.KeepValue<TAppliedBadge>(Badge)', 'TAppliedBadge');
    Contract('KeepValue<TAppliedBadge>(Badge).Choose<TAppliedBox<TAppliedBadge>>(Box)', 'TAppliedBox');
    ScalarContract('BoxValue<TAppliedBadge>(Badge).Value.Value');
    ScalarContract('RetypeValue<LongInt, TAppliedBadge>(42, Badge).Value.Value');
    Refusal('KeepValue<>(Badge)');
    Refusal('KeepValue<TAppliedBadge, LongInt>(Badge)');
    Refusal('RetypeValue<TAppliedBadge>(Badge)');
    Refusal('KeepValue<Badge>(Badge)');
    Refusal('KeepValue<Result>(Badge)', True);
    Refusal('KeepBadge<LongInt>(42)');
    Refusal('KeepValue<Missing>(Badge)');
    Refusal('KeepValue<TAppliedBadge><TAppliedBadge>(Badge)');
    Refusal('KeepValue<TAppliedBox<TAppliedBadge>>(Box');
    Refusal('DifferentValue<TAppliedBadge>(Badge, True)');
    ContextAndCompilerRefusal;
    OwnershipAndWork;
    CompletionPath('KeepValue<TAppliedBadge>(Badge).Val', 'Value');
    CompletionPath('BoxValue<TAppliedBadge>(Badge).Value.WithV', 'WithValue');
    CompletionPath('RetypeValue<LongInt, TAppliedBadge>(42, Badge).Value.Val', 'Value');
    CompletionPath('Badge.Choose<TAppliedBox<TAppliedBadge>>(Box).Value.Val', 'Value');
    {$ifndef PAS2JS}
    ConcurrentReaders;
    Check(ApplicationInvocations = 12, 'Semantic inspection invokes no generic body or argument');
    {$endif}
    WriteLn('PASS ', GChecks, ' explicit generic routine application checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', GChecks, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
