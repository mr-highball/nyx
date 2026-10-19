{ Implicit calls require exact source policy, declared argument correspondence
  and ordinary completion ownership. Source queries never invoke the fixture. }
program nyx_pascal_inference_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses SysUtils, Classes, nyx.text, nyx.scheduler, nyx.editing, nyx.pascal.work,
  nyx.pascal.files, nyx.pascal.context, nyx.pascal.lexing, nyx.pascal.translation,
  nyx.pascal.syntax, nyx.pascal.syntax.translated, nyx.pascal.binding,
  nyx.pascal.names, nyx.pascal.drafts, nyx.pascal.project, nyx.pascal.sources,
  nyx.pascal.completion, nyx.pascal.completion.owned
  {$ifndef PAS2JS}, inference.contracts.fixture{$endif};

{$ifndef PAS2JS}
type
  { Each worker creates private deduction frames/results over a shared immutable
    world and source window. Main joins before reading any worker outcome. }
  TInferenceReader = class(TThread)
  public
    Shared: INyxPascalSemanticWorld;
    Site: INyxPascalBindingSite;
    Window: TNyxPascalSpan;
    ErrorText: String;
    procedure Execute; override;
  end;
{$endif}

const
  CPortable: TNyxText = 'unit inference.contracts.fixture;{$mode delphi}' +
    '{$modeswitch implicitfunctionspecialization+}interface type TInferredBadge=class Value:LongInt;end;' +
    'TInferredBox<T>=class Value:T;end;TInferredStep<T>=function(AValue:T):T;' +
    'TInferredMethods=class class function Keep<T>(const AValue:T):T;static;' +
    'function Box<T>(const AValue:T):TInferredBox<T>;end;' +
    'function KeepValue<T>(const AValue:T):T;function BoxValue<T>(const AValue:T):TInferredBox<T>;' +
    'function TakeValue<T>(const ABox:TInferredBox<T>):T;' +
    'function PairValue<T,U>(const AValue:T;const AOther:U):TInferredBox<U>;' +
    'function ConstrainedValue<T:TInferredBadge>(const AValue:T):T;' +
    'function OptionalValue<T>(const AValue:T;AMarker:LongInt=0):T;' +
    'function CommonValue<T>(const AValue:T;AMarker:LongInt):T;overload;' +
    'function CommonValue<T>(const AValue:T;AMarker:Boolean):T;overload;' +
    'function CallbackValue<T>(const AStep:TInferredStep<T>):TInferredStep<T>;' +
    'function RepeatedValue<T>(const ALeft,ARight:T):T;implementation end.';
  CVariables: TNyxText = 'var Badge:TInferredBadge;Box:TInferredBox<TInferredBadge>;' +
    'Methods:TInferredMethods;Number:LongInt;Step:TInferredStep<TInferredBadge>;';
  CModeOn = '{$modeswitch implicitfunctionspecialization+}';
  CModeOff = '{$modeswitch implicitfunctionspecialization-}';

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

function Translation(const AText: TNyxText; const AContext: INyxPascalCompilerContext = nil;
  const ACancel: INyxCancellationScope = nil): INyxPascalTranslation;
var
  LContext: INyxPascalCompilerContext;
begin
  LContext := AContext;

  if LContext = nil then LContext := Facts;
  Result := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('mode.pas'), AText), NyxPascalPhysicalFile('mode.pas'),
    LContext, TNyxPascalTranslationOptions.Defaults, ACancel);
end;

function AtMarker(const ASource: INyxPascalTranslation): TNyxPascalKnowledge;
var
  LAt: Integer;
begin
  for LAt := 0 to ASource.Count - 1 do
  begin

    if (ASource.PhysicalToken(LAt).Kind = ptkWord) and (ASource.Text(LAt) = 'Marker') then
      Exit(ASource.State(ASource.Token(LAt).State).Feature(ppfImplicitFunctionSpecialization));
  end;
  raise Exception.Create('Missing exact source policy marker');
end;

procedure ModeContracts;
var
  LOn, LOff, LCopy: INyxPascalCompilerContext;
  LSource: INyxPascalTranslation;
  LAt: Integer;
begin
  LOn := Facts.WithFeature(ppfImplicitFunctionSpecialization, ppkTrue);
  LOff := Facts.WithFeature(ppfImplicitFunctionSpecialization, ppkFalse);
  LCopy := CaptureNyxPascalCompilerContext(LOn);
  Check(not LOn.SameFacts(LOff) and LCopy.SameFacts(LOn), 'Capture/identity retain the additional typed policy independently');
  Check(Facts.Feature(ppfImplicitFunctionSpecialization) = ppkUnknown, 'Unobserved defaults remain unknown');
  Check(LOn.InMode(ppmDelphi).Feature(ppfImplicitFunctionSpecialization) = ppkUnknown, 'Mode reset does not invent a default');
  Check(RestoreNyxPascalLocalSettings(LOff, LOn).Feature(ppfImplicitFunctionSpecialization) = ppkFalse,
    'Local settings do not restore the module switch');
  LSource := Translation(CModeOn+'unit Modes;interface var Marker:LongInt;implementation end.');

  if AtMarker(LSource) <> ppkTrue then
  begin
    WriteLn('leading switch / resolved=',LSource.Resolved,' value=',Ord(AtMarker(LSource)));
    for LAt := 0 to LSource.DiagnosticCount-1 do WriteLn(String(LSource.Diagnostic(LAt).MessageText));
  end;
  Check(AtMarker(LSource) = ppkTrue, 'Leading module switch is effective');
  Check(AtMarker(Translation('unit Modes;'+CModeOn+'interface var Marker:LongInt;implementation end.')) = ppkTrue, 'Unit header switch is effective');
  Check(AtMarker(Translation('unit Modes;'+CModeOff+'interface '+CModeOn+'var Marker:LongInt;implementation end.')) = ppkTrue,
    'Immediate interface lookahead retains the scanner global window');
  LSource := Translation('unit Modes;'+CModeOff+'interface var Earlier:LongInt;'+CModeOn+'var Marker:LongInt;implementation end.');
  Check(LSource.Resolved and (AtMarker(LSource) = ppkFalse), 'Later interface directive is retained but ignored');
  Check(AtMarker(Translation('unit Modes;'+CModeOff+'interface type IView=interface '+CModeOn+
    'end;var Marker:LongInt;implementation end.')) = ppkFalse, 'Interface type keywords cannot reopen the unit header');
  Check(AtMarker(Translation('unit Modes;'+CModeOn+'{$push}'+CModeOff+'{$pop}interface var Marker:LongInt;implementation end.')) = ppkFalse,
    'PUSH/POP retains the current module switch, matching the compiler refusal');
  Check(AtMarker(Translation('unit Modes;'+CModeOn+'interface var Earlier:LongInt;implementation '+CModeOff+
    'var Marker:LongInt;end.')) = ppkTrue, 'Implementation switch cannot revoke an earlier effective policy');
  Check(AtMarker(Translation('program Modes;'+CModeOn+'var Marker:LongInt;begin end.')) = ppkTrue,
    'Immediate program-header lookahead remains global');
  Check(AtMarker(Translation(CModeOff+'program Modes;var Earlier:LongInt;'+CModeOn+'var Marker:LongInt;begin end.')) = ppkFalse,
    'Later program declarations cannot reopen the global window');
  LSource := Translation('unit Modes;'+CModeOff+'{$ifdef ABSENT}'+CModeOn+'{$endif}interface var Marker:LongInt;implementation end.');
  Check(AtMarker(LSource) = ppkFalse, 'Known inactive switches have no effect');
  LSource := Translation('unit Modes;'+CModeOff+'{$ifdef UNKNOWN}'+CModeOn+'{$endif}interface var Marker:LongInt;implementation end.',
    NewNyxPascalCompilerContext(ppcFPC,TNyxPascalCompilerVersion.Create(3,3,1),ppmDelphi));
  Check(not LSource.Resolved and (AtMarker(LSource) = ppkUnknown), 'Conditional uncertainty never grants implicit specialization');
end;

function View(const AName, AText: TNyxText; const AContext: INyxPascalCompilerContext): INyxPascalTranslatedSyntax;
begin
  Result := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile(AName), AText), NyxPascalPhysicalFile(AName), AContext,
    TNyxPascalTranslationOptions.Defaults), TNyxPascalSyntaxOptions.Defaults);
end;

function WorldFor(const AExpression: TNyxText; const AMode: TNyxText = CModeOn;
  const AContext: INyxPascalCompilerContext = nil; ACandidates: Integer = 50): INyxPascalSemanticWorld;
var
  LContext: INyxPascalCompilerContext;
begin
  LContext := AContext;

  if LContext = nil then LContext := Facts;
  Result := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults.UpToCandidates(ACandidates))
    .WithModule(View('system.pas','unit System;interface type TObject=class end;implementation end.',LContext))
    .WithModule(View('inference.contracts.fixture.pas',GFixture,LContext))
    .WithModule(View('consumer.pas','unit Consumer;'+AMode+'interface uses inference.contracts.fixture;'+
      CVariables+'implementation procedure Run;begin '+AExpression+';end;end.',LContext));
end;

function Site(const AWorld: INyxPascalSemanticWorld; const AExpression: TNyxText;
  out AWindow: TNyxPascalSpan): INyxPascalBindingSite;
var
  LView: INyxPascalTranslatedSyntax;
  LPhysical: INyxPascalLexicalSnapshot;
  LText: TNyxText;
  LAt, LStart, LFinish: Integer;
begin
  LView := AWorld.ModuleAt(2);
  LPhysical := NyxPascalOccurrenceLexical(LView.Source,0);
  LText := LPhysical.Slice(NyxPascalSpan(0,LPhysical.Scalars));
  LAt := Pos(AExpression,LText);
  Check(LAt > 0,'The expression retains its exact physical occurrence');
  LStart := NyxTextScalarCount(Copy(LText,1,LAt-1));
  LFinish := LStart+NyxTextScalarCount(AExpression);
  LStart := LView.VirtualAt(0,LStart);
  { The following authored semicolon supplies an exact context-bearing caret.
    Adding one to the previous token can land in a synthetic projection gap. }
  LFinish := LView.VirtualAt(0,LFinish);
  Check((LStart >= 0) and (LFinish > LStart),'The complete expression maps to its virtual source');
  AWindow := NyxPascalSpan(LStart,LFinish-LStart);
  Result := AWorld.At(2,LView.Syntax.ScopeAt(LStart),LFinish);
end;

function Resolve(const AWorld: INyxPascalSemanticWorld; const AExpression: TNyxText): INyxPascalResolution;
var
  LQueries: INyxPascalExpressionQueries;
  LSite: INyxPascalBindingSite;
  LWindow: TNyxPascalSpan;
  LAccess: TNyxPascalMemberAccess;
begin
  LSite := Site(AWorld,AExpression,LWindow);
  Check(Supports(AWorld,INyxPascalExpressionQueries,LQueries),'Ordinary expression queries own deduction');
  Result := LQueries.ExpressionType(LSite,LWindow,LAccess);
end;

procedure Contract(const AExpression: TNyxText);
var
  LWorld: INyxPascalSemanticWorld;
  LAnswer: INyxPascalResolution;
  LProbe: INyxPascalResolution;
  LType: INyxPascalSemanticType;
begin
  LWorld := WorldFor(AExpression);
  LAnswer := Resolve(LWorld,AExpression);

  if (LAnswer.Status <> pbsBound) and (AExpression = 'BoxValue(Badge).Value') then
  begin
    LProbe := Resolve(WorldFor('BoxValue(Badge)'),'BoxValue(Badge)');
    WriteLn('constructed inferred call / status=',Ord(LProbe.Status),' issue=',Ord(LProbe.Issue));
    LProbe := Resolve(WorldFor('BoxValue<TInferredBadge>(Badge).Value'),'BoxValue<TInferredBadge>(Badge).Value');
    WriteLn('constructed explicit chain / status=',Ord(LProbe.Status),' issue=',Ord(LProbe.Issue));
  end;
  Check(LAnswer.Status = pbsBound,'Inferred result binds: '+String(AExpression)+' / '+String(LAnswer.MessageText));
  LType := LAnswer.BoundType;
  while (LType <> nil) and (LType.Kind = pstAlias) do LType := LType.Target;
  Check((LType <> nil) and (LType.Kind = pstDeclared) and
    (LType.Definition.Declaration.Name = 'TInferredBadge'),'The inferred chain retains the exact badge contract');
end;

procedure Refusal(const AExpression: TNyxText; const AMode: TNyxText = CModeOn;
  const AContext: INyxPascalCompilerContext = nil);
var
  LAnswer: INyxPascalResolution;
begin
  LAnswer := Resolve(WorldFor(AExpression,AMode,AContext),AExpression);
  Check((LAnswer.Status <> pbsBound) and (LAnswer.BoundType = nil),'Unsupported deduction publishes no guessed type: '+String(AExpression));
end;

procedure OwnershipAndWork;
const
  CExpression = 'KeepValue(PairValue(Number,Badge).Value)';
var
  LWorld, LOther: INyxPascalSemanticWorld;
  LQueries: INyxPascalExpressionQueries;
  LSite: INyxPascalBindingSite;
  LWindow: TNyxPascalSpan;
  LAccess: TNyxPascalMemberAccess;
  LAnswer, LRetained: INyxPascalResolution;
  LBudget: INyxPascalAnalysisBudget;
  LCancel: INyxCancellationScope;
  LAt, LFit: Integer;
  LRefused: Boolean;
begin
  LWorld := WorldFor(CExpression);
  LSite := Site(LWorld,CExpression,LWindow);
  Supports(LWorld,INyxPascalExpressionQueries,LQueries);
  LFit := -1;
  for LAt := 1 to 10000 do
  begin
    LAnswer := nil;
    LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      try
        LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess,LBudget);
        Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType <> nil), 'The first sufficient allowance captures the complete inferred result');
        LRetained := LAnswer;
        LFit := LAt;
      except
        on ENyxPascalAnalysisWorkLimit do
          Check((LAnswer = nil) and (LBudget.Receipt.Total <= LAt),'Every smaller inference allowance refuses atomically');
      end;
    finally
      LBudget.Retire;
    end;

    if LFit > 0 then Break;
  end;
  Check(LFit > 1,'Inference has a finite positive first fit');
  WriteLn('implicit inference work / first fit=',LFit,' every smaller allowance refused');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LAnswer := nil;
  LRefused := False;
  try
    LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess,LCancel);
  except
    on ENyxPascalAnalysisCancelled do LRefused := True;
  end;
  Check(LRefused and (LAnswer = nil),'Cancelled deduction publishes no frame or result');
  LOther := WorldFor(CExpression);
  Supports(LOther,INyxPascalExpressionQueries,LQueries);
  LAnswer := nil;
  LRefused := False;
  try
    LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess);
  except
    on ENyxPascalBinding do LRefused := True;
  end;
  Check(LRefused and (LAnswer = nil),'Replacement worlds cannot retag earlier inference sites');
  Check(LRetained.World = LSite.World,'Retained inference frames outlive their retired work budget');
  LWorld := WorldFor('KeepValue(Badge,0,0)',CModeOn,nil,2);
  LAnswer := nil;
  LRefused := False;
  try
    LAnswer := Resolve(LWorld,'KeepValue(Badge,0,0)');
  except
    on ENyxPascalBindingLimit do LRefused := True;
  end;
  Check(LRefused and (LAnswer = nil),'Argument capacity never truncates an inferred call');
end;

{$ifndef PAS2JS}
procedure TInferenceReader.Execute;
var
  LQueries: INyxPascalExpressionQueries;
  LAnswer: INyxPascalResolution;
  LType: INyxPascalSemanticType;
  LAccess: TNyxPascalMemberAccess;
  LAt: Integer;
begin
  try

    if not Supports(Shared,INyxPascalExpressionQueries,LQueries) then
      raise Exception.Create('Missing concurrent inference capability');
    for LAt := 1 to 16 do
    begin
      LAnswer := LQueries.ExpressionType(Site,Window,LAccess);

      if LAnswer.Status <> pbsBound then raise Exception.Create('Concurrent deduction failed');
      LType := LAnswer.BoundType;
      while (LType <> nil) and (LType.Kind = pstAlias) do LType := LType.Target;

      if (LType = nil) or (LType.Definition = nil) or
        (LType.Definition.Declaration.Name <> 'TInferredBadge') then
        raise Exception.Create('Concurrent inference lost its exact declared result');
    end;
  except
    on E: Exception do ErrorText := E.Message;
  end;
end;

procedure Concurrent;
const
  CExpression = 'KeepValue(PairValue(Number,Badge).Value)';
var
  LWorld: INyxPascalSemanticWorld;
  LSite: INyxPascalBindingSite;
  LWindow: TNyxPascalSpan;
  LReaders: array[0..3] of TInferenceReader;
  LAt: Integer;
begin
  LWorld := WorldFor(CExpression);
  LSite := Site(LWorld,CExpression,LWindow);
  for LAt := 0 to High(LReaders) do
  begin
    LReaders[LAt] := TInferenceReader.Create(True);
    LReaders[LAt].Shared := LWorld;
    LReaders[LAt].Site := LSite;
    LReaders[LAt].Window := LWindow;
  end;
  try
    for LAt := 0 to High(LReaders) do LReaders[LAt].Start;
    for LAt := 0 to High(LReaders) do LReaders[LAt].WaitFor;
    for LAt := 0 to High(LReaders) do
      Check(LReaders[LAt].ErrorText = '', 'Concurrent inference reader: '+LReaders[LAt].ErrorText);
  finally
    for LAt := 0 to High(LReaders) do LReaders[LAt].Free;
  end;
  WriteLn('native concurrent implicit inference / 64 joined');
end;
{$endif}

procedure IncludedPolicyAndHeaderOwnership;
var
  LFiles: INyxPascalFileCatalog;
  LSource: INyxPascalTranslation;
  LWorld: INyxPascalSemanticWorld;
  LAnswer: INyxPascalResolution;
  LAt: Integer;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('mode.pas'),'unit Modes;'+CModeOff+
      '{$i flags.inc}interface var Earlier:LongInt;{$define LATE}{$i flags.inc}var Marker:LongInt;implementation end.')
    .WithFile(NyxPascalPhysicalFile('flags.inc'),'{$ifdef LATE}'+CModeOff+'{$else}'+CModeOn+'{$endif}');
  LSource := NewNyxPascalTranslation(LFiles,NyxPascalPhysicalFile('mode.pas'),Facts,
    TNyxPascalTranslationOptions.Defaults);
  Check(LSource.Resolved and (LSource.OccurrenceCount = 3) and (AtMarker(LSource) = ppkTrue),
    'Repeated include occurrences retain their actual global/late contexts');
  LSource := Translation('unit Modes;{$macro on}{$define HEADER:=interface}'+CModeOff+
    'HEADER var Earlier:LongInt;'+CModeOn+'var Marker:LongInt;implementation end.');
  Check(LSource.Resolved and (AtMarker(LSource) = ppkFalse), 'Effective macro keywords retain the original module boundary');
  {$ifndef PAS2JS}
  { This exact body belongs to the whole compiled fixture. Its header can use
    a formal without giving an unapplied body a guessed lexical environment. }
  LWorld := WorldFor('KeepValue(Badge)');
  LAt := Pos('Result := TInferredBox<T>.Create;',GFixture);
  Check(LAt > 0,'The body refusal uses an exact compiler-owned source occurrence');
  LAt := NyxTextScalarCount(Copy(GFixture,1,LAt-1))+Length('Result := ');
  LAt := LWorld.ModuleAt(1).VirtualAt(0,LAt);
  Check(LAt >= 0,'The body type name retains its exact virtual origin');
  LAnswer := LWorld.Lookup(LWorld.At(1,LWorld.ModuleAt(1).Syntax.ScopeAt(LAt),LAt),
    NyxPascalBindingName('TInferredBox'));
  Check((LAnswer.Status <> pbsBound) and (LAnswer.Issue = pbiGenericContext) and
    (LAnswer.BoundType = nil),'Header lookup permission never escapes into an unapplied generic body');
  {$endif}
end;

procedure ModeWork;
const
  CText = 'unit Modes;'+CModeOn+'interface var Marker:LongInt;implementation end.';
var
  LBudget: INyxPascalAnalysisBudget;
  LSource, LRetained: INyxPascalTranslation;
  LAt, LFit: Integer;
begin
  LFit := -1;
  for LAt := 1 to 10000 do
  begin
    LSource := nil;
    LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      try
        LSource := Translation(CText,nil,LBudget);
        Check(LSource.Resolved and (AtMarker(LSource) = ppkTrue), 'Sufficient mode proof work retains the complete policy');
        Check(LBudget.Receipt.Used(papSourceProof) >= 4,'Prior header token proof is admitted through the existing work owner');
        LRetained := LSource;
        LFit := LAt;
      except
        on ENyxPascalAnalysisWorkLimit do
          Check((LSource = nil) and (LBudget.Receipt.Total <= LAt),'Every smaller header proof allowance refuses atomically');
      end;
    finally
      LBudget.Retire;
    end;

    if LFit > 0 then Break;
  end;
  Check((LFit > 1) and (AtMarker(LRetained) = ppkTrue),'Published source policy survives work retirement without a retained execution scope');
  WriteLn('implicit mode work / first fit=',LFit,' every smaller allowance refused');
end;

procedure CompletionPath(const AExpression: TNyxText);
var
  LFiles: INyxPascalFileCatalog;
  LRoots: array of TNyxPascalPhysicalFile;
  LProvider: INyxPascalCompletionProvider;
  LSources, LPreview: INyxPascalSourceSnapshot;
  LReply: INyxPascalCompletionResult;
  LOrigins: INyxOwnedPascalCompletionResult;
  LText: TNyxText;
  LCaret, LNewCaret: Integer;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('sdk/system.pas'),'unit System;interface type TObject=class end;implementation end.')
    .WithFile(NyxPascalPhysicalFile('sdk/inference.contracts.fixture.pas'),GFixture);
  SetLength(LRoots,2);
  LRoots[0] := NyxPascalPhysicalFile('sdk/system.pas');
  LRoots[1] := NyxPascalPhysicalFile('sdk/inference.contracts.fixture.pas');
  { Explicit small-model policy; default captured-SDK completeness is neither
    claimed nor repaired with invented ObjPas exports. }
  LProvider := NewNyxOwnedPascalCompletionProvider(Facts,LFiles,LRoots,
    TNyxOwnedPascalCompletionOptions.Defaults.WithBinding(
      TNyxPascalBindingOptions.Defaults.WithImplicitScopes(pispDeclarationNames)));
  LText := 'unit Consumer;'+CModeOn+'interface uses inference.contracts.fixture;'+CVariables+
    'implementation procedure Run;begin '+AExpression;
  LSources := CaptureNyxPascalSources(NewNyxPascalProject(NyxPascalUnit('Consumer'),LText));
  LCaret := NyxTextScalarCount(LText);
  LReply := LProvider.Complete(NewNyxPascalCompletionRequest(LSources).At(TNyxPascalSourceRef.Main,LCaret));
  Check((LReply.Status = pcsReady) and (LReply.Count = 1) and (LReply.Total = 1),
    'Ordinary completion follows deduction: '+String(AExpression)+' / '+String(LReply.MessageText));
  Check(LReply.Item(0).Name = 'Value','The specialized field remains the actual source declaration');
  Supports(LReply,INyxOwnedPascalCompletionResult,LOrigins);
  LProvider := nil;
  Check((LOrigins.Definition(0).FileReference.Name = 'sdk/inference.contracts.fixture.pas') and
    (LOrigins.DefinitionSource(0).Slice(LOrigins.Definition(0).Span) = 'Value'),
    'Exact physical navigation survives provider retirement');
  LPreview := LReply.Preview(0,LSources,LNewCaret);
  Check((LNewCaret = LCaret+2) and (LPreview.Text(TNyxPascalSourceRef.Main) = LText+'ue'),
    'Detached insertion changes only the final member prefix');
end;

begin
  try
    GFixture := CPortable;
    {$ifndef PAS2JS}
    with TStringList.Create do
    try
      LoadFromFile('tests/inference.contracts.fixture.pas');
      GFixture := TNyxText(Text);
    finally
      Free;
    end;
    Check(NativeInferenceValue = 42,'The native compiler executes legal implicit calls to 42');
    Check(InferenceInvocations = 12,'All twelve independent generic invocations execute exactly once');
    WriteLn('native inference fixture / value=42 invocations=',InferenceInvocations);
    {$endif}
    ModeContracts;
    IncludedPolicyAndHeaderOwnership;
    ModeWork;
    Contract('KeepValue(Badge)');
    Contract('BoxValue(Badge).Value');
    Contract('TakeValue(Box)');
    Contract('PairValue(Number,Badge).Value');
    Contract('TInferredMethods.Keep(Badge)');
    Contract('Methods.Box(Badge).Value');
    Contract('ConstrainedValue(Badge)');
    Contract('OptionalValue(Badge)');
    Contract('CommonValue(Badge,True)');
    Contract('CallbackValue(Step)(Badge)');
    Contract('RepeatedValue(Badge,Badge)');
    Contract('KeepValue(PairValue(Number,Badge).Value)');
    Contract('KeepValue(PairValue<LongInt,TInferredBadge>(Number,Badge).Value)');
    { Literal inference now has its own native-witness qualification. Reals
      still retain the original explicit missing-type refusal here. }
    Refusal('KeepValue(1.5)');
    Refusal('ConstrainedValue(Number)');
    Refusal('RepeatedValue(Badge,Number)');
    Refusal('KeepValue(TInferredBadge)');
    Refusal('KeepValue(Badge,Number)');
    Refusal('KeepValue()');
    Refusal('KeepValue(Badge,)');
    Refusal('KeepValue(Badge+Badge)');
    Refusal('PairValue(Number,Badge');
    Refusal('KeepValue(Badge)',CModeOff);
    Refusal('KeepValue(Badge)','');
    Refusal('KeepValue(Badge)',CModeOn,Facts.WithSymbol(NyxPascalDefine('NYX_INFERENCE_AFTER_POP'),ppdDefined));
    CompletionPath('KeepValue(Badge).Val');
    CompletionPath('TakeValue(Box).Val');
    CompletionPath('CallbackValue(Step)(Badge).Val');
    OwnershipAndWork;
    {$ifndef PAS2JS}
    Concurrent;
    Check(InferenceInvocations = 12,'Semantic queries and completion never invoke application code');
    {$endif}
    WriteLn('PASS ',GChecks,' implicit routine inference checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL / ',E.Message);
      {$ifndef PAS2JS}
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$else}
      raise;
      {$endif}
    end;
  end;
end.
