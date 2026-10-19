{ Ordinary argument syntax and formal-dependent types are separate obligations.
  The native application witness runs once; source queries never execute it. }
program nyx_pascal_argument_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses SysUtils, Classes, nyx.text, nyx.editing, nyx.scheduler, nyx.pascal.work,
  nyx.pascal.files, nyx.pascal.context, nyx.pascal.lexing, nyx.pascal.translation,
  nyx.pascal.syntax, nyx.pascal.syntax.translated, nyx.pascal.binding,
  nyx.pascal.names, nyx.pascal.drafts, nyx.pascal.project, nyx.pascal.sources,
  nyx.pascal.completion, nyx.pascal.completion.owned
  {$ifndef PAS2JS}, argument.contracts.fixture{$endif};

{$ifndef PAS2JS}
type
  { Readers share only immutable source/world/site values. Every argument
    answer vector, syntax walk and generic frame belongs to its own query. }
  TArgumentReader = class(TThread)
  public
    Shared: INyxPascalSemanticWorld;
    Site: INyxPascalBindingSite;
    Window: TNyxPascalSpan;
    ErrorText: String;
    procedure Execute; override;
  end;
{$endif}

const
  CMode = '{$mode delphi}{$H+}{$modeswitch implicitfunctionspecialization+}';
  CPortable = 'unit argument.contracts.fixture;'+CMode+'interface type '+
    'TArgumentBadge=class Value:LongInt;end;TArgumentBox<T>=class Value:T;end;'+
    'function WithText<T>(const AValue:T;const AText:UnicodeString):TArgumentBox<T>;'+
    'function WithNumbers<T>(const AValue:T;AReal:Double;APointer:Pointer):TArgumentBox<T>;'+
    'function WithFlag<T>(const AValue:T;AFlag:Boolean):TArgumentBox<T>;'+
    'function NeedBoth<T,U>(const AValue:T;const AOther:U):TArgumentBox<U>;implementation end.';

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
    .WithModule(View('argument.contracts.fixture.pas',GFixture))
    .WithModule(View('consumer.pas','unit Consumer;'+AMode+'interface uses argument.contracts.fixture;'+
      'var Badge:TArgumentBadge;'+AExtra+'implementation procedure Run;begin '+AExpression+';end;end.'));
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
  Check(LAt > 0,'The argument call has its exact physical source occurrence');
  LStart := NyxTextScalarCount(Copy(LText,1,LAt+5));
  LFinish := LStart+NyxTextScalarCount(AExpression);
  LStart := LView.VirtualAt(0,LStart);
  LFinish := LView.VirtualAt(0,LFinish);
  Check((LStart >= 0) and (LFinish > LStart),'The whole call maps to its original projection');
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

function Resolve(const AExpression: TNyxText): INyxPascalResolution;
begin
  Result := ResolveIn(WorldFor(AExpression),AExpression);
end;

procedure Contract(const AExpression: TNyxText);
var
  LAnswer: INyxPascalResolution;
begin
  LAnswer := Resolve(AExpression);
  Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType <> nil),
    'An ordinary argument cannot erase the deduced result: '+String(AExpression)+' / '+String(LAnswer.MessageText));
  Check((LAnswer.BoundType.Definition <> nil) and
    (LAnswer.BoundType.Definition.Declaration.Name = 'TArgumentBadge'),
    'The fluent result keeps the actual named badge contract');
end;

procedure Refusal(const AExpression: TNyxText);
var
  LAnswer: INyxPascalResolution;
begin
  LAnswer := Resolve(AExpression);
  Check((LAnswer.Status <> pbsBound) and (LAnswer.BoundType = nil),
    'Malformed or type-dependent opaque arguments refuse: '+String(AExpression));
end;

procedure ArgumentContracts;
var
  LAnswer: INyxPascalResolution;
  LName: TNyxText;
begin
  Contract('WithText(Badge,''λ'').Value');
  Contract('WithText(Badge, ((''a'' + #0 + ''b''))).Value');
  Contract('WithNumbers(Badge, -(1.5 + 2.5), nil).Value');
  Contract('WithFlag(Badge,(1 < 2) and not False).Value');
  Contract('WithText(Badge,''a''#0''λ'').Value');
  Contract('WithText(Badge,(''a''+''b'')+(''c''+''d'')).Value');
  Contract('WithText(NeedBoth<TArgumentBadge,TArgumentBadge>(Badge,Badge).Value,''λ'').Value');
  Contract('WithNumbers(Badge,1 / 0,nil).Value');
  Contract('WithNumbers(Badge,1 shl (2+3),((nil))).Value');
  LAnswer := Resolve('NeedBoth(Badge,''λ'').Value');
  Check((LAnswer.Status = pbsUnresolved) and (LAnswer.Issue = pbiCompilerContext) and
    (LAnswer.BoundType = nil),'A required type preserves its original compiler-context refusal');
  LAnswer := ResolveIn(WorldFor('NeedBoth(Badge,''λ'').Value',CMode+'{$codepage utf8}'),
    'NeedBoth(Badge,''λ'').Value');
  Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType.Builtin = pbbWideChar),
    'Explicit encoding evidence still permits an actually required literal type');
  Refusal('NeedBoth(Badge,1+2).Value');
  Refusal('WithText(Badge,)');
  Refusal('WithText(Badge,(''a''+)).Value');
  Refusal('WithText(Badge,''a'' ''b'').Value');
  Refusal('WithText(Badge,(''a'',''b'')).Value');
  Refusal('WithText(Badge,(''a''+''b'']).Value');
  Refusal('WithText(Badge,).Value');
  Refusal('WithText(Badge,''a''+).Value');
  Refusal('WithNumbers(Badge,1 < 2 < 3,nil).Value');
  Refusal('WithNumbers(Badge,1,,nil).Value');
  Refusal('WithNumbers(Badge,1,nil,0).Value');
  LName := 'true'+TNyxText(StringOfChar('x',123));
  LAnswer := ResolveIn(WorldFor(LName,CMode,'var '+LName+':TArgumentBadge;'),LName);
  Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType.Definition <> nil) and
    (LAnswer.BoundType.Definition.Declaration.Name = 'TArgumentBadge'),
    'A long identifier with a Boolean prefix retains its declared type');
  LAnswer := ResolveIn(WorldFor('False',CMode,'var False:TArgumentBadge;'),'False');
  Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType.Definition <> nil),
    'A shadowed Boolean spelling remains an ordinary declaration');
end;

procedure OwnershipAndWork;
const
  CExpression = 'WithText(Badge, ((''λ'' + #0 + ''b''))).Value';
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
  for LAt := 1 to 10000 do
  begin
    LAnswer := nil;
    LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      try
        LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess,LBudget);
        Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType.Definition <> nil),
          'The first sufficient allowance publishes the complete named result');
        LFit := LAt;
        LRetained := LAnswer;
      except
        on ENyxPascalAnalysisWorkLimit do
          Check((LAnswer = nil) and (LBudget.Receipt.Total <= LAt),
            'Every smaller allowance refuses without an escaped argument plan');
      end;
    finally
      LBudget.Retire;
    end;

    if LFit > 0 then Break;
  end;
  Check(LFit > 1,'Argument syntax and deduction have a finite positive first fit');
  WriteLn('argument work / first fit=',LFit,' every smaller allowance refused');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LRefused := False;
  LAnswer := nil;
  try
    LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess,LCancel);
  except
    on ENyxPascalAnalysisCancelled do LRefused := True;
  end;
  Check(LRefused and (LAnswer = nil),'Cancelled argument parsing publishes no result');
  LOther := WorldFor(CExpression);
  Supports(LOther,INyxPascalExpressionQueries,LQueries);
  LRefused := False;
  LAnswer := nil;
  try
    LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess);
  except
    on ENyxPascalBinding do LRefused := True;
  end;
  Check(LRefused and (LAnswer = nil),'A replacement world cannot adopt an earlier argument site');
  Check(LRetained.World = LWorld,'Retained results outlive their retired execution budget');
  LWorld := WorldFor(CExpression,CMode,'',1);
  LRefused := False;
  try
    LAnswer := ResolveIn(LWorld,CExpression);
  except
    on ENyxPascalBindingLimit do LRefused := True;
  end;
  Check(LRefused,'The existing candidate limit also bounds argument vectors');
  LWorld := WorldFor(CExpression,CMode,'',50,2);
  LRefused := False;
  try
    LAnswer := ResolveIn(LWorld,CExpression);
  except
    on ENyxPascalBindingLimit do LRefused := True;
  end;
  Check(LRefused,'Nested opaque groups retain the existing recursion ceiling');
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
    .WithFile(NyxPascalPhysicalFile('sdk/argument.contracts.fixture.pas'),GFixture);
  SetLength(LRoots,2);
  LRoots[0] := NyxPascalPhysicalFile('sdk/system.pas');
  LRoots[1] := NyxPascalPhysicalFile('sdk/argument.contracts.fixture.pas');
  LProvider := NewNyxOwnedPascalCompletionProvider(Facts,LFiles,LRoots,
    TNyxOwnedPascalCompletionOptions.Defaults.WithBinding(
      TNyxPascalBindingOptions.Defaults.WithImplicitScopes(pispDeclarationNames)));
  LText := 'unit Consumer;'+CMode+'interface uses argument.contracts.fixture;var Badge:TArgumentBadge;'+
    'implementation procedure Run;begin WithText(Badge,''λ'').Value.Val';
  LSources := CaptureNyxPascalSources(NewNyxPascalProject(NyxPascalUnit('Consumer'),LText));
  LCaret := NyxTextScalarCount(LText);
  LReply := LProvider.Complete(NewNyxPascalCompletionRequest(LSources).At(TNyxPascalSourceRef.Main,LCaret));
  Check((LReply.Status = pcsReady) and (LReply.Count = 1) and (LReply.Item(0).Name = 'Value'),
    'Ordinary completion retains the independent generic result / '+String(LReply.MessageText));
  Supports(LReply,INyxOwnedPascalCompletionResult,LOrigins);
  LProvider := nil;
  Check((LOrigins.Definition(0).FileReference.Name = 'sdk/argument.contracts.fixture.pas') and
    (LOrigins.DefinitionSource(0).Slice(LOrigins.Definition(0).Span) = 'Value'),
    'Navigation retains the original field source after provider retirement');
  LPreview := LReply.Preview(0,LSources,LNewCaret);
  Check((LNewCaret = LCaret+2) and (LPreview.Text(TNyxPascalSourceRef.Main) = LText+'ue'),
    'Detached insertion changes only the requested fluent member prefix');
end;

{$ifndef PAS2JS}
procedure TArgumentReader.Execute;
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
        (LAnswer.BoundType.Definition.Declaration.Name <> 'TArgumentBadge') then
        raise Exception.Create('Concurrent opaque arguments lost their named result contract');
    end;
  except
    on E: Exception do ErrorText := E.Message;
  end;
end;

procedure Concurrent;
const
  CExpression = 'WithText(Badge, ((''λ'' + #0 + ''b''))).Value';
var
  LWorld: INyxPascalSemanticWorld;
  LSite: INyxPascalBindingSite;
  LWindow: TNyxPascalSpan;
  LReaders: array[0..3] of TArgumentReader;
  LAt: Integer;
begin
  LWorld := WorldFor(CExpression);
  LSite := Site(LWorld,CExpression,LWindow);
  for LAt := 0 to High(LReaders) do
  begin
    LReaders[LAt] := TArgumentReader.Create(True);
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
  WriteLn('native concurrent argument readers / 64 joined');
end;

procedure NativeWitness;
var
  LStream: TFileStream;
begin
  Check(NativeArgumentValue = 42,'Whole native calls execute to 42');
  Check(ArgumentInvocations = 4,'The compiler witness invokes four independent generic calls');
  LStream := TFileStream.Create('tests/argument.contracts.fixture.pas',fmOpenRead or fmShareDenyWrite);
  try
    SetLength(GFixture,LStream.Size);

    if Length(GFixture) > 0 then LStream.ReadBuffer(GFixture[1],Length(GFixture));
  finally
    LStream.Free;
  end;
  WriteLn('native argument witness / value=42 invocations=',ArgumentInvocations);
end;
{$endif}

begin
  try
    GFixture := CPortable;
    {$ifndef PAS2JS}NativeWitness;{$endif}
    ArgumentContracts;
    OwnershipAndWork;
    CompletionPath;
    {$ifndef PAS2JS}
    Concurrent;
    Check(ArgumentInvocations = 4,'Source queries never execute application arguments or generic bodies');
    {$endif}
    WriteLn('PASS ',GChecks,' formal-dependent argument checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ',E.ClassName,': ',E.Message);
      ExitCode := 1;
    end;
  end;
end.
