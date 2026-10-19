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
program nyx_pascal_indexed_property_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Classes, nyx.text, nyx.scheduler, nyx.editing, nyx.pascal.lexing, nyx.pascal.lexing.append,
  nyx.pascal.context, nyx.pascal.files, nyx.pascal.syntax, nyx.pascal.syntax.translated,
  nyx.pascal.translation, nyx.pascal.binding, nyx.pascal.work, nyx.pascal.names,
  nyx.pascal.project, nyx.pascal.drafts, nyx.pascal.sources, nyx.pascal.completion, nyx.pascal.completion.owned
  {$ifndef PAS2JS}, indexed.properties.fixture{$endif};

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
    'function Vary(A:Boolean):TOther; overload; procedure NoResult; ' +
    'type TBag<T>=class function Read(I:LongInt):T; procedure Put(I:LongInt; V:T); ' +
    'property Items[I:LongInt]:T read Read write Put; default; property WriteOnly[I:LongInt]:T write Put; end; ' +
    'TChild=class(TBag<TBadge>) end; THidden=class(TChild) function Number(I:LongInt):LongInt; ' +
    'property Items[I:LongInt]:LongInt read Number; end; ' +
    'TAlternate=class(TChild) function OtherItem(I:LongInt):TRows; property Other[I:LongInt]:TRows read OtherItem; default; end; ' +
    'TMatrix=class function Cell(I,J:LongInt):TBadge; property Cells[I,J:LongInt]:TBadge read Cell; default; end; ' +
    'TRecord=record function Read(I:LongInt):TBadge; property Items[I:LongInt]:TBadge read Read; default; end; ' +
    'IList=interface function Read(I:LongInt):TBadge; property Items[I:LongInt]:TBadge read Read; default; end; ' +
    'TBadDefaults=class function Read(I:LongInt):TBadge; property First[I:LongInt]:TBadge read Read; default; ' +
    'property Second[I:LongInt]:TBadge read Read; default; end; TAlias=TBag<TBadge>; ' +
    'var Bag:TBag<TBadge>; Child:TChild; Hidden:THidden; Alternate:TAlternate; Matrix:TMatrix; Rec:TRecord; List:IList; AliasedBag:TAlias; Bad:TBadDefaults; ';

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
  LSyntax: INyxPascalSyntaxSnapshot;
  LOwner, LProperty, LParameter: Integer;
  LDecl: TNyxPascalSyntaxDeclaration;
  LHints: INyxPascalHintSyntax;
begin
  Shape('Bag.Items[0]');
  Shape('Bag[0]');
  Shape('(Bag)[0]');
  Shape('Child[0]');
  Shape('Hidden[0]');
  Shape('Matrix.Cells[1,2]');
  Shape('Matrix[1,2]');
  Shape('Rec[0]');
  Shape('List[0]');
  Shape('AliasedBag[0]');
  Shape('Alternate[0][1]');
  Shape('Alternate.Other[0][1]');
  Shape('Bag.Items[0].WithValue(42)');
  Refusal('Bag.Items.Value');
  Refusal('Bag.Items');
  Refusal('Bag.Items[]');
  Refusal('Bag.Items[0,1]');
  Refusal('Matrix[0]');
  Refusal('Matrix[0,1,2]');
  Refusal('Bag.WriteOnly[0]');
  Refusal('TChild[0]', pbiWrongMemberKind);
  Refusal('Bad[0]', pbiAmbiguousName);
  LWorld := WorldFor('Hidden.Items[0]');
  LAnswer := Resolve(LWorld, 'Hidden.Items[0]', LAccess);
  Check((LAnswer.Status = pbsBound) and (TerminalType(LAnswer.BoundType).Builtin = pbbLongInt),
    'Named lookup follows hiding while inherited default lookup retains its original declaration');
  LSyntax := View('metadata.pas', 'unit Metadata; interface type TBag=class function Read(I,J:LongInt):LongInt; ' +
    'property Items[const I,J:LongInt]:LongInt read Read deprecated ''Use Current''; default; ' +
    'property Count:LongInt read Count default 7; end; implementation end.').Syntax;
  Check(LSyntax.DiagnosticCount = 0, 'Property parameter metadata retains complete declaration syntax');
  LOwner := LSyntax.Named(0, 'TBag');
  LProperty := LSyntax.Named(LSyntax.Declaration(LOwner).NestedScope, 'Items');
  LDecl := LSyntax.Declaration(LProperty);
  Check(LDecl.DefaultProperty and (LDecl.PropertyParameters.Length > 0) and (LDecl.PropertyAccess = [pspaRead]),
    'A default member directive is separate from property default values');
  Check(Supports(LSyntax, INyxPascalHintSyntax, LHints) and (LDecl.FirstHint >= 0) and
    (LHints.HintAt(LDecl.FirstHint).Kind = phkDeprecated), 'Accessor metadata leaves trailing hints with their existing typed owner');
  Check(LSyntax.Scope(LDecl.NestedScope).Kind = psskPropertyParameters, 'Property parameters own a distinct lexical scope');
  LParameter := LSyntax.Named(LDecl.NestedScope, 'J');
  Check((LParameter >= 0) and (LSyntax.Declaration(LParameter).ParameterMode = pspmConst),
    'Grouped parameters keep independent names and passing modes');
  LProperty := LSyntax.Named(LSyntax.Declaration(LOwner).NestedScope, 'Count');
  Check(not LSyntax.Declaration(LProperty).DefaultProperty, 'A scalar default value does not select unnamed indexing');
end;

procedure BoundariesAndCheckpoint;
var
  LWorld: INyxPascalSemanticWorld;
  LAnswer: INyxPascalResolution;
  LAccess: TNyxPascalMemberAccess;
  LLex, LAppended: INyxPascalLexicalSnapshot;
  LBefore, LAfter: INyxPascalSyntaxSnapshot;
  LOld, LNew: TNyxPascalSyntaxDeclaration;
  LAt: Integer;
begin
  LWorld := EmptyWorld.WithModule(View('chain.pas', 'unit Chain; interface type TBag=class ' +
    'strict private function Read(I:LongInt):TBag; property Items[I:LongInt]:TBag read Read; default; ' +
    'end; var Outside:TBag; implementation procedure Run; begin Outside[0]; end; end.'));
  LAnswer := Resolve(LWorld, 'Outside[0]', LAccess);
  Check((LAnswer.Status = pbsInaccessible) and (LAnswer.BoundType = nil), 'Default indexing preserves strict-private access');
  LWorld := EmptyWorld.WithModule(View('chain.pas', 'unit Chain; interface type TBag=class ' +
    'function Read(I:LongInt):TBag; property Items[I:]:TBag read Read; default; end; ' +
    'var Broken:TBag; implementation procedure Run; begin Broken[0]; end; end.'));
  LAnswer := Resolve(LWorld, 'Broken[0]', LAccess);
  Check((LAnswer.Status = pbsUnresolved) and (LAnswer.BoundType = nil), 'A missing index parameter type grants no result');
  LLex := NewNyxPascalAppendLexical('unit Partial; interface type TBag=class ' +
    'property Items[I:LongInt:LongInt read Read; default;', TNyxPascalLexicalOptions.Defaults);
  LBefore := NewNyxPascalSyntaxFromLexical(LLex, TNyxPascalSyntaxOptions.Defaults);
  for LAt := 0 to LBefore.DeclarationCount - 1 do
  begin
    LOld := LBefore.Declaration(LAt);

    if LOld.Kind = psdkProperty then
      Check((psdfIncomplete in LOld.Flags) and not LBefore.Scope(LOld.NestedScope).Complete,
        'An unclosed property parameter list retains incomplete scope/declaration state');
  end;
  LLex := NewNyxPascalAppendLexical('unit Retained; interface type TBag=class function Read(I:LongInt):LongInt; ' +
    'property Items[I:LongInt]:LongInt read Read; default; end; implementation ', TNyxPascalLexicalOptions.Defaults);
  LBefore := NewNyxPascalSyntaxFromLexical(LLex, TNyxPascalSyntaxOptions.Defaults);
  LAppended := AppendNyxPascalLexical(LLex, 'end.');
  LAfter := AppendNyxPascalSyntax(LBefore, LAppended, TNyxPascalSyntaxOptions.Defaults);
  Check(LAfter.DiagnosticCount = 0, 'An appended property checkpoint completes without diagnostics');
  for LAt := 0 to LBefore.DeclarationCount - 1 do
  begin
    LOld := LBefore.Declaration(LAt);
    LNew := LAfter.Declaration(LAt);
    Check((LOld.PropertyParameters.Start = LNew.PropertyParameters.Start) and
      (LOld.PropertyParameters.Length = LNew.PropertyParameters.Length) and
      (LOld.PropertyAccess = LNew.PropertyAccess) and (LOld.DefaultProperty = LNew.DefaultProperty),
      'Checkpoint append retains exact property metadata without changing the earlier reader');
  end;
end;
procedure OwnershipAndWork;
const
  CExpression: TNyxText = '(Child[0]).WithValue(42)';
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
  WriteLn('property index work / first fit=', LFit, ' every smaller allowance refused');
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
  LExpression := '(Hidden[0]).WithV';
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
    (Pos('(Hidden[0]).WithValue;', LPreview.Text(TNyxPascalSourceRef.Main)) > 0),
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
  Check(IndexedPropertyProof = 42, 'Independent native named/default/generic/inherited/record property indexing returns 42');
  LBefore := IndexGetterCalls;
  LText := TStringList.Create;
  try
    LText.LoadFromFile('tests/indexed.properties.fixture.pas');
    LWorld := EmptyWorld.WithModule(View('indexed.properties.fixture.pas', TNyxText(LText.Text)))
      .WithModule(View('observer.pas', 'unit Observer; interface uses indexed.properties.fixture; var Hidden:TIndexHidden; ' +
        'implementation procedure Run; begin Hidden[0].Value; end; end.'));
  finally
    LText.Free;
  end;
  LAnswer := Resolve(LWorld, 'Hidden[0].Value', LAccess);
  Check((LAnswer.Status = pbsBound) and (TerminalType(LAnswer.BoundType).Builtin = pbbLongInt),
    'The unchanged whole compiled source supplies the original inherited default element');
  Check(IndexGetterCalls = LBefore, 'Source inspection never executes a getter or setter');
end;
{$endif}

begin
  try
    Contracts;
    BoundariesAndCheckpoint;
    OwnershipAndWork;
    CompletionPath;
    {$ifndef PAS2JS}WholeNativeUnit;{$endif}
    WriteLn('PASS ', GChecks, ' indexed/default property checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
