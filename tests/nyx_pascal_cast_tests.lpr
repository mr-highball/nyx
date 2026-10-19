{ nyx
  Copyright (c) 2020 mr-highball

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
}
program nyx_pascal_cast_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Classes, nyx.text, nyx.scheduler, nyx.editing, nyx.pascal.lexing,
  nyx.pascal.context, nyx.pascal.files, nyx.pascal.syntax, nyx.pascal.syntax.translated,
  nyx.pascal.translation, nyx.pascal.binding, nyx.pascal.work, nyx.pascal.names,
  nyx.pascal.project, nyx.pascal.drafts, nyx.pascal.sources, nyx.pascal.completion, nyx.pascal.completion.owned
  {$ifndef PAS2JS}, cast.contracts.fixture{$endif};

const
  CDeclarations: TNyxText = 'unit Chain; interface type ' +
    'TBadge=class Value:LongInt; function WithValue(A:LongInt):TBadge; ' +
    'function Choose(A:LongInt):TBadge; overload; function Choose(A:Boolean):TBadge; overload; ' +
    'class function Build:TBadge; static; end; TOther=class Other:LongInt; end; ' +
    'TBox<T>=class Value:T; function ReadValue:T; end; ' +
    'PBadge=^TBadge; TRows=array of TBadge; TGrid=array[0..1,0..1] of TBadge; ' +
    'TNested=array of TRows; TFactory=function:TBadge; ' +
    'var Badge:TBadge; Address:PBadge; Rows:TRows; Grid:TGrid; Nested:TNested; ' +
    'Box:TBox<TBadge>; Factory:TFactory; ' +
    'function NewBadge:TBadge; function Vary(A:LongInt):TBadge; overload; ' +
    'function Vary(A:Boolean):TOther; overload; procedure NoResult; ';

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

function View(const AName, AText: TNyxText): INyxPascalTranslatedSyntax;
begin
  Result := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile(AName), AText), NyxPascalPhysicalFile(AName), Facts,
    TNyxPascalTranslationOptions.Defaults), TNyxPascalSyntaxOptions.Defaults);
end;

function EmptyWorld: INyxPascalSemanticWorld;
begin
  Result := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults)
    .WithModule(View('system.pas', 'unit System; interface type TObject=class end; implementation end.'));
end;

function WorldFor(const AExpression: TNyxText): INyxPascalSemanticWorld;
begin
  Result := EmptyWorld.WithModule(View('chain.pas', CDeclarations +
    'implementation procedure Run; begin ' + AExpression + '; end; end.'));
end;

function Window(const AWorld: INyxPascalSemanticWorld; const AExpression: TNyxText): TNyxPascalSpan;
var
  LLex: INyxPascalLexicalSnapshot;
  LView: INyxPascalTranslatedSyntax;
  LText: TNyxText;
  LAt, LStart, LFinish: Integer;
begin
  LView := AWorld.ModuleAt(AWorld.Count - 1);
  LLex := NyxPascalOccurrenceLexical(LView.Source, 0);
  LText := LLex.Slice(NyxPascalSpan(0, LLex.Scalars));
  { The body's expression is last: a short name may also occur inside an earlier
    declared type such as TBadge. Never cut that unrelated identifier's token. }
  LAt := Length(LText) - Length(AExpression) + 1;
  while (LAt > 0) and (Copy(LText, LAt, Length(AExpression)) <> AExpression) do Dec(LAt);
  Check(LAt > 0, 'The complete expression is present in its owning source view');
  LAt := NyxTextScalarCount(Copy(LText, 1, LAt - 1));
  LStart := LView.VirtualAt(0, LAt);
  LFinish := LView.VirtualAt(0, LAt + NyxTextScalarCount(AExpression));
  Check((LStart >= 0) and (LFinish > LStart), 'Both original expression boundaries map into the owning semantic view');
  Result := NyxPascalSpan(LStart, LFinish - LStart);
end;

function Site(const AWorld: INyxPascalSemanticWorld; const AWindow: TNyxPascalSpan): INyxPascalBindingSite;
begin
  Result := AWorld.At(AWorld.Count - 1,
    AWorld.ModuleAt(AWorld.Count - 1).Syntax.ScopeAt(AWindow.Finish), AWindow.Finish);
end;

function TerminalType(const AType: INyxPascalSemanticType): INyxPascalSemanticType;
begin
  Result := AType;
  while (Result <> nil) and (Result.Kind in [pstAlias, pstDistinct]) do Result := Result.Target;
end;

function Resolve(const AWorld: INyxPascalSemanticWorld; const AExpression: TNyxText;
  out AAccess: TNyxPascalMemberAccess): INyxPascalResolution;
var
  LQueries: INyxPascalExpressionQueries;
  LWindow: TNyxPascalSpan;
begin
  Check(Supports(AWorld, INyxPascalExpressionQueries, LQueries), 'World exposes optional source-owned expression contracts');
  LWindow := Window(AWorld, AExpression);
  Result := LQueries.ExpressionType(Site(AWorld, LWindow), LWindow, AAccess);
end;

procedure Shape(const AExpression: TNyxText; const AName: TNyxText = 'TBadge';
  AExpectedAccess: TNyxPascalMemberAccess = pmaInstance);
var
  LWorld: INyxPascalSemanticWorld;
  LResult: INyxPascalResolution;
  LType: INyxPascalSemanticType;
  LAccess: TNyxPascalMemberAccess;
begin
  LWorld := WorldFor(AExpression);
  LResult := Resolve(LWorld, AExpression, LAccess);
  Check((LResult.Status = pbsBound) and (LResult.BoundType <> nil),
    'Expected a declared postfix contract: ' + String(AExpression) + ' / ' + String(LResult.MessageText));
  LType := TerminalType(LResult.BoundType);
  Check((LType.Definition <> nil) and (LType.Definition.Declaration.Name = AName) and
    (LAccess = AExpectedAccess), 'Postfix contract keeps the exact declared type and access role');
end;

procedure Refusal(const AExpression: TNyxText; AIssue: TNyxPascalBindingIssue = pbiUnsupportedType);
var
  LWorld: INyxPascalSemanticWorld;
  LResult: INyxPascalResolution;
  LAccess: TNyxPascalMemberAccess;
begin
  LWorld := WorldFor(AExpression);
  LResult := Resolve(LWorld, AExpression, LAccess);
  Check((LResult.Status <> pbsBound) and (LResult.Issue = AIssue) and
    (LResult.BoundType = nil) and (LResult.Count = 0),
    'An unsupported/uncertain postfix chain publishes no invented result: ' + String(AExpression) +
    ' / ' + String(LResult.MessageText));
end;

procedure Contracts;
var
  LWorld: INyxPascalSemanticWorld;
  LAnswer: INyxPascalResolution;
  LAccess: TNyxPascalMemberAccess;
begin
  Shape('TBadge(Badge)');
  Shape('TBadge(NewBadge()).WithValue(42)');
  Shape('(Badge as TBadge)');
  Shape('(NewBadge() as TBadge).WithValue(42)');
  Shape('(TBox<TBadge>(Badge)).Value');
  Shape('(Badge as TBox<TBadge>).Value');
  Shape('PBadge(Address)^');
  Shape('TRows(Rows)[0]');
  Shape('Chain.TBadge(Badge)');
  Refusal('TBadge()');
  Refusal('TBadge(Badge,Badge)');
  Refusal('(Badge as PBadge)');
  Refusal('(Badge as TRows)');
  Refusal('(TBadge as TBadge)');
  Refusal('(Address as TBadge)');
  Refusal('TBadge(Badge)()');
  Refusal('(Badge as Missing)', pbiMissingName);
  Refusal('(Badge as TBox<>)');
  Refusal('TBox<TBadge(Badge)');
  Refusal('Badge()');
  LWorld := WorldFor('LongInt(1)');
  LAnswer := Resolve(LWorld, 'LongInt(1)', LAccess);
  Check((LAnswer.Status = pbsBound) and (TerminalType(LAnswer.BoundType).Builtin = pbbLongInt),
    'An unshadowed language intrinsic retains its declared cast target');
  LWorld := EmptyWorld.WithModule(View('chain.pas', CDeclarations +
    'implementation function Work:LongInt; begin Result(0); end; end.'));
  LAnswer := Resolve(LWorld, 'Result(0)', LAccess);
  Check((LAnswer.Status = pbsUnresolved) and (LAnswer.BoundType = nil),
    'An implicit scalar Result is a value, never an intrinsic type-cast name');
  LWorld := EmptyWorld.WithModule(View('chain.pas', CDeclarations +
    'implementation function Work:TBadge; begin (Badge as Result); end; end.'));
  LAnswer := Resolve(LWorld, '(Badge as Result)', LAccess);
  Check((LAnswer.Status <> pbsBound) and (LAnswer.BoundType = nil),
    'An implicit class Result is a value, never the named target of AS');
  LWorld := EmptyWorld.WithModule(View('chain.pas', CDeclarations +
    'implementation function Work:TBadge; begin TBox<Result>(Badge); end; end.'));
  LAnswer := Resolve(LWorld, 'TBox<Result>(Badge)', LAccess);
  Check((LAnswer.Status <> pbsBound) and (LAnswer.BoundType = nil),
    'A generic cast argument must name a type rather than implicit result storage');
end;
procedure OwnershipAndWork;
const
  CExpression: TNyxText = '(Box as TBox<TBadge>).ReadValue().WithValue(42)';
var
  LWorld, LChanged: INyxPascalSemanticWorld;
  LQueries: INyxPascalExpressionQueries;
  LWindow: TNyxPascalSpan;
  LSite: INyxPascalBindingSite;
  LAttempt, LRetained: INyxPascalResolution;
  LBudget: INyxPascalAnalysisBudget;
  LCancel: INyxCancellationScope;
  LAccess: TNyxPascalMemberAccess;
  LAt, LFit: Integer;
  LRefused: Boolean;
begin
  LWorld := WorldFor(CExpression);
  LWindow := Window(LWorld, CExpression);
  LSite := Site(LWorld, LWindow);
  Supports(LWorld, INyxPascalExpressionQueries, LQueries);
  LFit := -1;
  for LAt := 1 to 3000 do
  begin
    LAttempt := nil;
    LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      try
        LAttempt := LQueries.ExpressionType(LSite, LWindow, LAccess, LBudget);
        Check((LAttempt.Status = pbsBound) and (LAttempt.BoundType <> nil), 'First sufficient expression budget binds the actual chain');
        LRetained := LAttempt;
        LFit := LAt;
      except
        on ENyxPascalAnalysisWorkLimit do
          Check((LAttempt = nil) and (LBudget.Receipt.Total <= LAt), 'Smaller expression allowance refuses atomically');
      end;
    finally
      LBudget.Retire;
    end;

    if LFit > 0 then Break;
  end;
  Check(LFit > 1, 'Actual expression work has a finite first fit');
  WriteLn('cast work / first fit=', LFit, ' every smaller allowance refused');
  Check(LWorld.Member(LRetained.BoundType, LSite, NyxPascalBindingName('WithValue')).Status = pbsBound,
    'Retained expression types remain usable after budget retirement');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LAttempt := nil;
  LRefused := False;
  try
    LAttempt := LQueries.ExpressionType(LSite, LWindow, LAccess, LCancel);
  except
    on ENyxPascalAnalysisCancelled do LRefused := True;
  end;
  Check(LRefused and (LAttempt = nil), 'Cancelled expression scans publish no result');
  LChanged := LWorld.WithModule(View('chain.pas', 'unit Chain; interface implementation end.'));
  Supports(LChanged, INyxPascalExpressionQueries, LQueries);
  LRefused := False;
  try
    LQueries.ExpressionType(LSite, LWindow, LAccess);
  except
    on ENyxPascalBinding do LRefused := True;
  end;
  Check(LRefused, 'A replacement world refuses the old expression site');
  Supports(LWorld, INyxPascalExpressionQueries, LQueries);
  LAt := LWorld.ModuleAt(1).Syntax.Lexical.TokenAt(LWindow.Start);
  while LWorld.ModuleAt(1).Syntax.Lexical.Token(LAt).Kind <> ptkWord do Inc(LAt);
  LAt := LWorld.ModuleAt(1).Syntax.Lexical.Token(LAt).Span.Start + 1;
  LAttempt := LQueries.ExpressionType(LSite, NyxPascalSpan(LAt, LWindow.Finish - LAt), LAccess);
  Check((LAttempt.Status = pbsUnresolved) and (LAttempt.BoundType = nil), 'A window cut inside its first token cannot resolve a tail');
end;

procedure CompletionPath;
var
  LFiles: INyxPascalFileCatalog;
  LRoots: array of TNyxPascalPhysicalFile;
  LProvider: INyxPascalCompletionProvider;
  LSources, LPreview: INyxPascalSourceSnapshot;
  LRequest: INyxPascalCompletionRequest;
  LResult: INyxPascalCompletionResult;
  LMetadata: INyxOwnedPascalCompletionResult;
  LText, LExpression: TNyxText;
  LAt: Integer;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('sdk/system.pas'), 'unit System; interface type TObject=class end; implementation end.')
    .WithFile(NyxPascalPhysicalFile('sdk/chain.pas'), CDeclarations + 'implementation end.');
  SetLength(LRoots, 2);
  LRoots[0] := NyxPascalPhysicalFile('sdk/system.pas');
  LRoots[1] := NyxPascalPhysicalFile('sdk/chain.pas');
  LProvider := NewNyxOwnedPascalCompletionProvider(Facts, LFiles, LRoots, TNyxOwnedPascalCompletionOptions.Defaults);
  LExpression := '(TBox<TBadge>(Box)).ReadValue().WithV';
  LText := 'unit Consumer; interface uses Chain; implementation procedure Run; begin ' + LExpression + '; end; end.';
  LSources := CaptureNyxPascalSources(NewNyxPascalProject(NyxPascalUnit('Consumer'), LText));
  LAt := Pos(LExpression, LText);
  LRequest := NewNyxPascalCompletionRequest(LSources).At(TNyxPascalSourceRef.Main,
    NyxTextScalarCount(Copy(LText, 1, LAt - 1)) + NyxTextScalarCount(LExpression));
  LResult := LProvider.Complete(LRequest);
  Check((LResult.Status = pcsReady) and (LResult.Count = 1) and (LResult.Total = 1),
    'The ordinary completion provider follows the complete fluent receiver: ' + String(LResult.MessageText));
  Check((LResult.Item(0).Name = TNyxText('WithValue')) and (LResult.ExpectedText = TNyxText('WithV')),
    'Call/group punctuation cannot change the exact member replacement');
  Supports(LResult, INyxOwnedPascalCompletionResult, LMetadata);
  LProvider := nil;
  Check((LMetadata.Definition(0).FileReference.Name = TNyxText('sdk/chain.pas')) and
    (LMetadata.DefinitionSource(0).Slice(LMetadata.Definition(0).Span) = TNyxText('WithValue')),
    'Fluent suggestions retain their original declaration token after provider retirement');
  LPreview := LResult.Preview(0, LSources, LAt);
  Check((LAt = LRequest.Caret + 4) and
    (Pos('ReadValue().WithValue;', LPreview.Text(TNyxPascalSourceRef.Main)) > 0),
    'The detached insertion preserves the exact preceding fluent expression');
end;

{$ifndef PAS2JS}
procedure WholeNativeUnit;
var
  LText: TStringList;
  LWorld: INyxPascalSemanticWorld;
  LAnswer: INyxPascalResolution;
  LAccess: TNyxPascalMemberAccess;
  LBefore: LongInt;
begin
  Check(CompilerCastValue = 42, 'The independent compiler executes explicit/checked/generic/pointer casts to 42');
  LBefore := CastOperandCalls;
  LText := TStringList.Create;
  try
    LText.LoadFromFile('tests/cast.contracts.fixture.pas');
    LWorld := EmptyWorld.WithModule(View('cast.contracts.fixture.pas', TNyxText(LText.Text)))
      .WithModule(View('observer.pas', 'unit Observer; interface uses cast.contracts.fixture; var Current:TObject; ' +
        'implementation procedure Run; begin (Current as TCastBox<TCastValue>).ReadItem().ReadValue(); end; end.'));
  finally
    LText.Free;
  end;
  LAnswer := Resolve(LWorld, '(Current as TCastBox<TCastValue>).ReadItem().ReadValue()', LAccess);
  Check((LAnswer.Status = pbsBound) and (TerminalType(LAnswer.BoundType).Builtin = pbbLongInt),
    'The unchanged whole compiled source supplies the generic checked-cast result');
  Check(CastOperandCalls = LBefore, 'Source queries execute no operand, checked assertion or callback');
  LWorld := LWorld.WithModule(View('observer.pas', 'unit Observer; interface uses cast.contracts.fixture; var Current:TObject; ' +
    'implementation procedure Run; begin (Current as ICastValue).ReadValue(); end; end.'));
  LAnswer := Resolve(LWorld, '(Current as ICastValue).ReadValue()', LAccess);
  Check((LAnswer.Status = pbsBound) and (TerminalType(LAnswer.BoundType).Builtin = pbbLongInt),
    'The actual named interface retains its result contract after a checked assertion');
  Check(CastOperandCalls = LBefore, 'Interface narrowing still executes no runtime query or operand');
end;
{$endif}

begin
  try
    Contracts;
    OwnershipAndWork;
    CompletionPath;
    {$ifndef PAS2JS}WholeNativeUnit;{$endif}
    WriteLn('PASS ', GChecks, ' source-owned cast contract checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
