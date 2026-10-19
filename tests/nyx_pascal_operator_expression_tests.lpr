{ Operator result contracts and source references retain exact world, parameter,
  precedence and origin ownership. No semantic query invokes an operator body. }
program nyx_pascal_operator_expression_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses SysUtils, Classes, nyx.text, nyx.editing, nyx.scheduler, nyx.pascal.work,
  nyx.pascal.files, nyx.pascal.context, nyx.pascal.lexing, nyx.pascal.translation,
  nyx.pascal.syntax, nyx.pascal.syntax.translated, nyx.pascal.binding,
  nyx.pascal.names, nyx.pascal.drafts, nyx.pascal.project, nyx.pascal.sources,
  nyx.pascal.completion, nyx.pascal.completion.owned
  {$ifndef PAS2JS}, operator.expressions.fixture{$endif};

{$ifndef PAS2JS}
type
  { Independent operator candidate sets and substitution frames share only
    immutable source/site values. Main joins before reading worker outcomes. }
  TOperatorReader = class(TThread)
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
  CPortable = 'unit operator.expressions.fixture;'+CMode+'interface type '+
    'TOperatorBadge=class Value:LongInt;end;TOtherOperatorBadge=class Value:LongInt;end;'+
    'TOperatorValue=record public Badge:TOperatorBadge;'+
    'class operator +(const ALeft,ARight:TOperatorValue):TOperatorBadge;'+
    'class operator +(const ALeft:TOperatorValue;ARight:LongInt):TOtherOperatorBadge;'+
    'class operator *(const ALeft,ARight:TOperatorValue):TOperatorValue;'+
    'class operator -(const AValue:TOperatorValue):TOperatorValue;'+
    'class operator =(const ALeft,ARight:TOperatorValue):Boolean;end;'+
    'function KeepOperator<T>(const AValue:T):T;implementation end.';

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
  Result := NewNyxPascalCompilerContext(ppcFPC,TNyxPascalCompilerVersion.Create(3,3,1),ppmDelphi)
    .CompleteDefines(True);
end;

function View(const AName, AText: TNyxText): INyxPascalTranslatedSyntax;
begin
  Result := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile(AName),AText),NyxPascalPhysicalFile(AName),Facts,
    TNyxPascalTranslationOptions.Defaults),TNyxPascalSyntaxOptions.Defaults);
end;

function WorldFor(const AExpression: TNyxText; const AMode: TNyxText = CMode;
  const AExtra: TNyxText = ''; ACandidates: Integer = 50; ADepth: Integer = 64): INyxPascalSemanticWorld;
begin
  Result := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults.UpToCandidates(ACandidates).UpToDepth(ADepth))
    .WithModule(View('operator.expressions.fixture.pas',GFixture))
    .WithModule(View('consumer.pas','unit Consumer;'+AMode+'interface uses operator.expressions.fixture;'+
      'var Left,Right:TOperatorValue;Number:LongInt;Enabled,Ready:Boolean;'+AExtra+
      'implementation procedure Run;begin '+AExpression+';end;end.'));
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
  LAt := Pos('begin '+AExpression,LText);
  Check(LAt > 0,'The expression has its exact physical occurrence');
  LStart := NyxTextScalarCount(Copy(LText,1,LAt+5));
  LFinish := LStart+NyxTextScalarCount(AExpression);
  LStart := LView.VirtualAt(0,LStart);
  LFinish := LView.VirtualAt(0,LFinish);
  Check((LStart >= 0) and (LFinish > LStart),'The entire expression retains its original projection');
  AWindow := NyxPascalSpan(LStart,LFinish-LStart);
  Result := AWorld.At(1,LView.Syntax.ScopeAt(LStart),LFinish);
end;

function ResolveIn(const AWorld: INyxPascalSemanticWorld; const AExpression: TNyxText): INyxPascalResolution;
var
  LSite: INyxPascalBindingSite;
  LWindow: TNyxPascalSpan;
  LAccess: TNyxPascalMemberAccess;
  LQueries: INyxPascalExpressionQueries;
begin
  LSite := Site(AWorld,AExpression,LWindow);
  Supports(AWorld,INyxPascalExpressionQueries,LQueries);
  Result := LQueries.ExpressionType(LSite,LWindow,LAccess);
end;

procedure Contract(const AExpression: TNyxText; const AExpected: TNyxText; const AMode: TNyxText = CMode);
var
  LWorld: INyxPascalSemanticWorld;
  LSite: INyxPascalBindingSite;
  LWindow: TNyxPascalSpan;
  LAccess: TNyxPascalMemberAccess;
  LQueries: INyxPascalExpressionQueries;
  LAnswer: INyxPascalResolution;
begin
  LWorld := WorldFor(AExpression,AMode);
  LSite := Site(LWorld,AExpression,LWindow);
  Supports(LWorld,INyxPascalExpressionQueries,LQueries);
  LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess);
  Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType <> nil),
    'The source expression retains its declared result: '+String(AExpression)+' / '+String(LAnswer.MessageText));
  Check((LAnswer.BoundType.Definition <> nil) and
    (LAnswer.BoundType.Definition.Declaration.Name = AExpected),'The exact named result is preserved');
end;

procedure ExpressionContracts;
var
  LAnswer: INyxPascalResolution;
  LLiteral: INyxPascalLiteralType;
begin
  Contract('(Left + Right)','TOperatorBadge');
  Contract('(Left * Right + Right)','TOperatorBadge');
  Contract('(-Left + Right)','TOperatorBadge');
  Contract('(Left + Number)','TOtherOperatorBadge');
  Contract('(@Left)^.Badge','TOperatorBadge',CMode+'{$T+}');
  Contract('(@Left.Badge)^','TOperatorBadge',CMode+'{$T+}');
  Contract('KeepOperator(Left + Right)','TOperatorBadge');
  Contract('KeepOperator(Left * Right + Right)','TOperatorBadge');
  LAnswer := ResolveIn(WorldFor('Left = Right'),'Left = Right');
  Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType.Builtin = pbbBoolean),
    'A declared equality result retains its real Boolean contract');
  LAnswer := ResolveIn(WorldFor('(Enabled and not Ready) or (Ready xor Enabled)'),
    '(Enabled and not Ready) or (Ready xor Enabled)');
  Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType.Builtin = pbbBoolean),
    'Qualified Boolean operators retain Boolean result identity');
  LAnswer := ResolveIn(WorldFor('True and False'),'True and False');
  Check((LAnswer.Status = pbsBound) and not Supports(LAnswer.BoundType,INyxPascalLiteralType,LLiteral),
    'An operator result does not inherit an operand literal value');
  LAnswer := ResolveIn(WorldFor('@Left',CMode+'{$T+}'),'@Left');
  Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType.Kind = pstPointer) and
    (LAnswer.BoundType.Target.Definition.Declaration.Name = 'TOperatorValue'),
    'T+ describes the exact pointed-to storage type without obtaining an address');
  LAnswer := ResolveIn(WorldFor('@Left',CMode+'{$T-}'),'@Left');
  Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType.Builtin = pbbPointer),
    'T- retains a compiler pointer without fabricating a pointed-to type');
  LAnswer := ResolveIn(WorldFor('@Left'),'@Left');
  Check((LAnswer.Status = pbsUnresolved) and (LAnswer.Issue = pbiCompilerContext),
    'An unobserved typed-address setting cannot be guessed');
end;

procedure Refusal(const AExpression: TNyxText; const AMode: TNyxText = CMode);
var
  LAnswer: INyxPascalResolution;
begin
  LAnswer := ResolveIn(WorldFor(AExpression,AMode),AExpression);
  Check((LAnswer.Status <> pbsBound) and (LAnswer.BoundType = nil),
    'Incomplete/unsupported operator or reference meaning refuses: '+String(AExpression));
end;

procedure OwnershipAndWork;
const
  CExpression = 'KeepOperator(Left * Right + Right)';
var
  LWorld, LOther: INyxPascalSemanticWorld;
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
  LWorld := WorldFor(CExpression);
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
        Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType.Definition <> nil),
          'The first sufficient allowance publishes the whole operator/generic result');
        LFit := LAt;
        LRetained := LAnswer;
      except
        on ENyxPascalAnalysisWorkLimit do
          Check((LAnswer = nil) and (LBudget.Receipt.Total <= LAt),
            'Every smaller allowance refuses without partial operator candidates');
      end;
    finally
      LBudget.Retire;
    end;

    if LFit > 0 then Break;
  end;
  Check(LFit > 1,'Operator resolution has a finite positive first fit');
  WriteLn('operator expression work / first fit=',LFit,' every smaller allowance refused');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LAnswer := nil;
  LRefused := False;
  try
    LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess,LCancel);
  except
    on ENyxPascalAnalysisCancelled do LRefused := True;
  end;
  Check(LRefused and (LAnswer = nil),'Cancelled operator resolution publishes no result');
  LOther := WorldFor(CExpression);
  Supports(LOther,INyxPascalExpressionQueries,LQueries);
  LAnswer := nil;
  LRefused := False;
  try
    LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess);
  except
    on ENyxPascalBinding do LRefused := True;
  end;
  Check(LRefused and (LAnswer = nil),'An operator site cannot be retagged to a replacement world');
  Check(LRetained.World = LWorld,'A retained result outlives its retired execution budget');
end;

procedure CompletionPath;
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
    .WithFile(NyxPascalPhysicalFile('sdk/operator.expressions.fixture.pas'),GFixture);
  SetLength(LRoots,2);
  LRoots[0] := NyxPascalPhysicalFile('sdk/system.pas');
  LRoots[1] := NyxPascalPhysicalFile('sdk/operator.expressions.fixture.pas');
  LProvider := NewNyxOwnedPascalCompletionProvider(Facts,LFiles,LRoots,
    TNyxOwnedPascalCompletionOptions.Defaults.WithBinding(
      TNyxPascalBindingOptions.Defaults.WithImplicitScopes(pispDeclarationNames)));
  LText := 'unit Consumer;'+CMode+'interface uses operator.expressions.fixture;var Left,Right:TOperatorValue;'+
    'implementation procedure Run;begin (Left * Right + Right).Val';
  LSources := CaptureNyxPascalSources(NewNyxPascalProject(NyxPascalUnit('Consumer'),LText));
  LCaret := NyxTextScalarCount(LText);
  LReply := LProvider.Complete(NewNyxPascalCompletionRequest(LSources).At(TNyxPascalSourceRef.Main,LCaret));
  Check((LReply.Status = pcsReady) and (LReply.Count = 1) and (LReply.Item(0).Name = 'Value'),
    'Ordinary completion follows the operator result / '+String(LReply.MessageText));
  Supports(LReply,INyxOwnedPascalCompletionResult,LOrigins);
  LProvider := nil;
  Check((LOrigins.Definition(0).FileReference.Name = 'sdk/operator.expressions.fixture.pas') and
    (LOrigins.DefinitionSource(0).Slice(LOrigins.Definition(0).Span) = 'Value'),
    'Exact operator-result member navigation survives provider retirement');
  LPreview := LReply.Preview(0,LSources,LNewCaret);
  Check((LNewCaret = LCaret+2) and (LPreview.Text(TNyxPascalSourceRef.Main) = LText+'ue'),
    'Detached insertion changes only the requested member prefix');
end;

{$ifndef PAS2JS}
procedure TOperatorReader.Execute;
var
  LQueries: INyxPascalExpressionQueries;
  LAnswer: INyxPascalResolution;
  LAccess: TNyxPascalMemberAccess;
  LAt: Integer;
begin
  try
    Supports(Shared,INyxPascalExpressionQueries,LQueries);
    for LAt := 1 to 16 do
    begin
      LAnswer := LQueries.ExpressionType(Site,Window,LAccess);

      if (LAnswer.Status <> pbsBound) or (LAnswer.BoundType.Definition = nil) or
        (LAnswer.BoundType.Definition.Declaration.Name <> 'TOperatorBadge') then
        raise Exception.Create('Concurrent operator resolution lost its exact result contract');
    end;
  except
    on E: Exception do ErrorText := E.Message;
  end;
end;

procedure Concurrent;
const
  CExpression = 'KeepOperator(Left * Right + Right)';
var
  LWorld: INyxPascalSemanticWorld;
  LSite: INyxPascalBindingSite;
  LWindow: TNyxPascalSpan;
  LReaders: array[0..3] of TOperatorReader;
  LAt: Integer;
begin
  LWorld := WorldFor(CExpression);
  LSite := Site(LWorld,CExpression,LWindow);
  for LAt := 0 to High(LReaders) do
  begin
    LReaders[LAt] := TOperatorReader.Create(True);
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
  WriteLn('native concurrent operator readers / 64 joined');
end;

procedure NativeWitness;
var
  LStream: TFileStream;
begin
  Check(NativeOperatorValue = 42,'Whole native operator and reference expressions execute to 42');
  Check(OperatorInvocations = 9,'Nine declared operator/generic bodies execute in the independent witness');
  LStream := TFileStream.Create('tests/operator.expressions.fixture.pas',fmOpenRead or fmShareDenyWrite);
  try
    SetLength(GFixture,LStream.Size);

    if Length(GFixture) > 0 then LStream.ReadBuffer(GFixture[1],Length(GFixture));
  finally
    LStream.Free;
  end;
  WriteLn('native operator witness / value=42 invocations=',OperatorInvocations);
end;
{$endif}

begin
  try
    GFixture := CPortable;
    {$ifndef PAS2JS}NativeWitness;{$endif}
    ExpressionContracts;
    Refusal('Left +');
    Refusal('(Left + Right');
    Refusal('Left = Right = Left');
    Refusal('Left + 1');
    Refusal('@TOperatorValue',CMode+'{$T+}');
    Refusal('@42',CMode+'{$T+}');
    Refusal('(@Left)^',CMode+'{$T-}');
    OwnershipAndWork;
    CompletionPath;
    {$ifndef PAS2JS}
    Concurrent;
    Check(OperatorInvocations = 9,'Semantic operator/address reads never execute application code');
    {$endif}
    WriteLn('PASS ',GChecks,' operator expression checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ',E.ClassName,': ',E.Message);
      ExitCode := 1;
    end;
  end;
end.
