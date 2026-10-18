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
program nyx_pascal_open_scope_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Classes, nyx.text, nyx.bytes, nyx.editing, nyx.scheduler, nyx.pascal.context,
  nyx.pascal.lexing, nyx.pascal.files, nyx.pascal.translation, nyx.pascal.syntax,
  nyx.pascal.syntax.translated, nyx.pascal.binding, nyx.pascal.conditions,
  nyx.pascal.names, nyx.pascal.project, nyx.pascal.drafts, nyx.pascal.sources, nyx.pascal.completion,
  nyx.pascal.completion.owned, open.scope.fixture;

const
  CSystem: TNyxText = 'unit System; interface type TObject=class end; implementation end.';
  CKnown: TNyxText = 'unit Known; interface type TBase=class public type ' +
    'TInherited=record Value:Integer; end; ' +
    'TFactory=class public type TItem=record Value:Integer; end; end; end; implementation end.';
var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
end;

function Facts: INyxPascalCompilerContext;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi).CompleteDefines(True);
end;

function View(const AFile, AText: TNyxText): INyxPascalTranslatedSyntax;
begin
  Result := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(
    NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile(AFile), AText),
    NyxPascalPhysicalFile(AFile), Facts, TNyxPascalTranslationOptions.Defaults),
    TNyxPascalSyntaxOptions.Defaults);
end;

{ Controlled portable modules qualify lookup rules. The separate actual RTL
  consumer retains real compiler roots; these fixtures do not replace SDK input. }
function World(const AText: TNyxText): INyxPascalSemanticWorld;
begin
  Result := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults)
    .WithModule(View('system.pas', CSystem)).WithModule(View('known.pas', CKnown))
    .WithModule(View('consumer.pas', AText));
end;

function AtText(const AWorld: INyxPascalSemanticWorld; ACaret: Integer): INyxPascalBindingSite;
var
  LScope: Integer;
begin
  LScope := AWorld.ModuleAt(2).Syntax.ScopeAt(ACaret);
  Result := AWorld.At(2, LScope, ACaret);
end;

function EndLookup(const AText, AName: TNyxText): INyxPascalResolution;
var
  LWorld: INyxPascalSemanticWorld;
begin
  LWorld := World(AText);
  Result := LWorld.Lookup(AtText(LWorld, LWorld.ModuleAt(2).Syntax.Lexical.Scalars),
    NyxPascalBindingName(AName));
end;

procedure LexicalNames;
const
  CPrefix: TNyxText = 'unit Consumer; interface uses Known; type TGlobal=Byte; ' +
    'TOpen=class(TBase) public type TLocal=TInherited; ';
var
  LWorld: INyxPascalSemanticWorld;
  LAnswer: INyxPascalResolution;
  LSite: INyxPascalBindingSite;
  LAt: Integer;
  LText: TNyxText;
  LBody: TNyxText;
  LRejected: Boolean;
  LCancel: INyxCancellationScope;
begin
  LAnswer := EndLookup(CPrefix, 'TGlobal');
  Check((LAnswer.Status = pbsBound) and (LAnswer.SymbolAt(0).Declaration.Scope = 0),
    'A complete captured ancestor can prove absence before lookup reaches the outer declaration');
  LAnswer := EndLookup(CPrefix, 'TInherited');
  Check((LAnswer.Status = pbsBound) and (LAnswer.SymbolAt(0).ModuleIndex = 1),
    'An inherited nested name remains available in the unfinished descendant');
  LAnswer := EndLookup(CPrefix, 'TLocal');
  Check((LAnswer.Status = pbsBound) and (LAnswer.SymbolAt(0).ModuleIndex = 2),
    'A prior own nested declaration still takes precedence');
  LAnswer := EndLookup(CPrefix, 'TOpen');
  Check((LAnswer.Status = pbsBound) and (LAnswer.Count = 1),
    'The enclosing declaration retains its exact source identity');
  Check(LAnswer.World.TypeOfSymbol(LAnswer.SymbolAt(0)).Issue = pbiIncompleteSource,
    'Lexical lookup grants no complete receiver type to the open class');
  for LAt := 0 to 1 do
  begin
    LText := CPrefix + '{query}';
    LBody := LText + 'type TGlobal=Word; ';

    if LAt = 1 then
    begin
      LBody := LBody + 'end; implementation end.';
    end;
    LWorld := World(LBody);
    LSite := AtText(LWorld, NyxTextScalarCount(LText));
    LAnswer := LWorld.Lookup(LSite, NyxPascalBindingName('TGlobal'));
    Check((LAnswer.Status = pbsBound) and (LAnswer.SymbolAt(0).Declaration.Scope = 0),
      'Later own nested types cannot be donated by open or completed receiver lookup');
  end;
  LAnswer := EndLookup('unit Consumer; interface type TOuter=class public type ' +
    'TOuterName=Byte; TInner=class ', 'TOuterName');
  Check((LAnswer.Status = pbsBound) and (LAnswer.SymbolAt(0).Declaration.Name = TNyxText('TOuterName')),
    'Nested traversal searches each enclosing member owner exactly once');
  LAnswer := EndLookup('unit Consumer; interface type TGlobal=Byte; TOpen=class(TAbsent) ', 'TGlobal');
  WriteLn('missing ancestor / status=', Ord(LAnswer.Status), ' issue=', Ord(LAnswer.Issue),
    ' related=', LAnswer.RelatedName);
  Check((LAnswer.Status = pbsUnresolved) and (LAnswer.Issue = pbiMissingType),
    'An explicitly named missing ancestor cannot masquerade as the end of inheritance');
  LAnswer := EndLookup('unit Consumer; interface uses Missing; type TGlobal=Byte; TOpen=class(Missing.TBase) ', 'TGlobal');
  Check((LAnswer.Status = pbsUnresolved) and (LAnswer.Issue = pbiMissingModule),
    'An uncaptured ancestor module refuses instead of selecting an outer name');
  LAnswer := EndLookup('unit Consumer; interface type TGlobal=Byte; TOpen=class(TAbsent ', 'TGlobal');
  Check((LAnswer.Status = pbsUnresolved) and (LAnswer.Issue = pbiIncompleteSource),
    'An unfinished ancestor header supplies no absence evidence');
  LAnswer := EndLookup('unit Consumer; interface type TGlobal=Byte; TOpen<T>=class ', 'TGlobal');
  Check((LAnswer.Status = pbsUnresolved) and (LAnswer.Issue = pbiGenericContext),
    'An unbound generic environment retains its existing explicit refusal');
  LAnswer := EndLookup('unit Consumer; interface type TGlobal=Byte; TOpen=class Bad @; ', 'TGlobal');
  Check((LAnswer.Status = pbsUnresolved) and
    (LAnswer.Issue in [pbiIncompleteSource, pbiUnsupportedScope]),
    'Malformed earlier member syntax cannot prove an outer name is unshadowed');
  LWorld := World(CPrefix);
  LSite := AtText(LWorld, LWorld.ModuleAt(2).Syntax.Lexical.Scalars);
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LRejected := False;
  try
    LWorld.Lookup(LSite, NyxPascalBindingName('TGlobal'), LCancel);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Cancellation publishes no partial lexical result');
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults.UpToSteps(1))
    .WithModule(LWorld.ModuleAt(0)).WithModule(LWorld.ModuleAt(1)).WithModule(LWorld.ModuleAt(2));
  LRejected := False;
  try
    LWorld.Lookup(AtText(LWorld, LWorld.ModuleAt(2).Syntax.Lexical.Scalars),
      NyxPascalBindingName('TGlobal'));
  except
    on ENyxPascalBindingLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'A bounded lookup refuses without returning partial inherited candidates');
end;

procedure OrdinaryCompletion;
const
  CPrefix: TNyxText = 'unit Consumer; interface uses Known; type ' +
    'TOpen=class(TBase) function Build: TFactory.TI';
var
  LFiles: INyxPascalFileCatalog;
  LRoots: array of TNyxPascalPhysicalFile;
  LProvider: INyxPascalCompletionProvider;
  LSources: INyxPascalSourceSnapshot;
  LReply: INyxPascalCompletionResult;
  LMetadata: INyxOwnedPascalCompletionResult;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('system.pas'), CSystem)
    .WithFile(NyxPascalPhysicalFile('known.pas'), CKnown)
    .WithFile(NyxPascalPhysicalFile('objpas.pas'), 'unit ObjPas; interface implementation end.');
  SetLength(LRoots, 3);
  LRoots[0] := NyxPascalPhysicalFile('system.pas');
  LRoots[1] := NyxPascalPhysicalFile('known.pas');
  LRoots[2] := NyxPascalPhysicalFile('objpas.pas');
  LProvider := NewNyxOwnedPascalCompletionProvider(Facts, LFiles, LRoots,
    TNyxOwnedPascalCompletionOptions.Defaults);
  LSources := CaptureNyxPascalSources(NewNyxPascalProject(NyxPascalUnit('Consumer'),
    'unit Consumer; interface implementation end.')).WithBuffer(TNyxPascalSourceRef.Main, CPrefix);
  LReply := LProvider.Complete(NewNyxPascalCompletionRequest(LSources)
    .At(TNyxPascalSourceRef.Main, NyxTextScalarCount(CPrefix)).Take(1));
  Supports(LReply, INyxOwnedPascalCompletionResult, LMetadata);
  WriteLn('open completion / status=', Ord(LReply.Status), ' count=', LReply.Count,
    ' message=', LReply.MessageText);
  Check((LReply.Status = pcsReady) and (LReply.Count = 1) and
    (LReply.Item(0).Name = TNyxText('TItem')), 'Ordinary completion follows the inherited factory from an open method header');
  Check((LMetadata.Definition(0).FileReference.Name = TNyxText('known.pas')) and
    (LMetadata.DefinitionSource(0).Slice(LMetadata.Definition(0).Span) = TNyxText('TItem')),
    'Detached navigation retains the exact captured inherited nested declaration');
  Check(LSources.Text(TNyxPascalSourceRef.Main) = CPrefix,
    'Completion preserves every unfinished source scalar');
end;

procedure SourceCondition;
const
  CText: TNyxText = 'unit System; interface type Char=AnsiChar; TObject=class ' +
    '{$if sizeof(Char)=1}const CorrectChar=1;{$else}{$fatal Wrong alias}{$endif}';
var
  LContext: INyxPascalCompilerContext;
  LRoot: INyxPascalCompilerContext;
  LResult: INyxPascalSemanticTranslation;
begin
  LRoot := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmFPC, pcsoSystemBootstrap)
    .WithOption('H', ppkFalse);
  LRoot := NyxPascalWithCapturedSize(LRoot, NyxPascalSizeReference('AnsiChar'),
    NyxPascalSizeFact(pssKnown, ppmFPC, ppkFalse, 1));
  LContext := NyxPascalWithBootstrapContext(Facts, LRoot);
  LResult := NewNyxPascalSemanticTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('system.pas'), CText), NyxPascalPhysicalFile('system.pas'),
    LContext, NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults
      .WithImplicitScopes(pispCompilerUnits)), TNyxPascalTranslationOptions.Defaults,
    TNyxPascalSyntaxOptions.Defaults, TNyxPascalConditionalOptions.Defaults);
  Check(not LResult.Pending and LResult.Source.Resolved and (LResult.Answers = 1),
    'Ordered semantic conditions can reach an outer source alias from the compiler root open class');
  Check(LResult.Source.Files.TextAt(0) = CText,
    'The conditional consumer invents no closing syntax or SDK declaration');
  LResult := NewNyxPascalSemanticTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('consumer.pas'), 'unit Consumer; interface type TOuter=Byte; ' +
      'TOpen=class(TAbsent) {$if declared(TOuter)}const MustNotGuess=1;{$endif}'),
    NyxPascalPhysicalFile('consumer.pas'), Facts, World('unit Consumer; interface implementation end.'),
    TNyxPascalTranslationOptions.Defaults, TNyxPascalSyntaxOptions.Defaults,
    TNyxPascalConditionalOptions.Defaults);
  Check(LResult.Pending and (LResult.BindingIssue = pbiMissingType),
    'The ordinary conditional reader retains an empty open body at the exact EOF caret');
end;

{$ifndef PAS2JS}
{ Consume the exact whole authored unit already compiled into this native test.
  This byte-only reader does not rewrite its syntax or manufacture a closed copy. }
procedure WholeAuthoredSource(const ARoot: TNyxText);
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
  LText: TNyxText;
  LResult: INyxPascalSemanticTranslation;
  LView: INyxPascalTranslatedSyntax;
  LWorld: INyxPascalSemanticWorld;
  LAnswer: INyxPascalResolution;
  LType: INyxPascalSemanticType;
  LAt: Integer;
  LScope: Integer;
begin
  LStream := TFileStream.Create(String(ARoot + '/tests/open.scope.fixture.pas'), fmOpenRead or fmShareDenyWrite);
  try
    Check((LStream.Size > 0) and (LStream.Size < 1024 * 1024), 'The authored input has a bounded exact byte packet');
    SetLength(LBytes, LStream.Size);
    LStream.ReadBuffer(LBytes[0], Length(LBytes));
  finally
    LStream.Free;
  end;
  LText := NyxDecodeUTF8(LBytes);
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults)
    .WithModule(View('system.pas', CSystem));
  LResult := NewNyxPascalSemanticTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('open.scope.fixture.pas'), LText),
    NyxPascalPhysicalFile('open.scope.fixture.pas'), Facts, LWorld,
    TNyxPascalTranslationOptions.Defaults, TNyxPascalSyntaxOptions.Defaults,
    TNyxPascalConditionalOptions.Defaults);
  Check(LResult.Source.Resolved and not LResult.Pending and (LResult.Answers = 1),
    'The whole unchanged authored conditional resolves inside its open descendant prefix');
  Check(LResult.Source.Files.TextAt(0) = LText, 'The whole source consumer preserves every original scalar');
  LView := NewNyxPascalTranslatedSyntax(LResult.Source, TNyxPascalSyntaxOptions.Defaults);
  Check(LView.Syntax.DiagnosticCount = 0, 'The exact compiled companion retains complete declaration syntax');
  LWorld := LWorld.WithModule(LView);
  LAt := LView.Syntax.Named(0, 'TScopeDescendant');
  LScope := LView.Syntax.Declaration(LAt).NestedScope;
  LAt := LView.Syntax.Named(LScope, 'Earlier');
  LAnswer := LWorld.TypeOfSymbol(LWorld.Symbol(1, LAt));
  LType := LAnswer.BoundType;
  while (LType <> nil) and (LType.Kind in [pstAlias, pstDistinct]) do
  begin
    LType := LType.Target;
  end;
  Check((LAnswer.Status = pbsBound) and (LType <> nil) and (LType.Builtin = pbbByte),
    'Whole-source field binding agrees with the compiler before the later shadow');
  LAt := LView.Syntax.Named(LScope, 'TOrderedValue');
  LAnswer := LWorld.TypeOfSymbol(LWorld.Symbol(1, LAt));
  LType := LAnswer.BoundType;
  while (LType <> nil) and (LType.Kind in [pstAlias, pstDistinct]) do
  begin
    LType := LType.Target;
  end;
  Check((LAnswer.Status = pbsBound) and (LType <> nil) and (LType.Builtin = pbbWord),
    'The later nested alias retains a separate exact source identity');
end;
{$endif}

begin
  try
    Check(RunOpenScopeFixture = 42, 'Whole compiled authored inheritance/member behavior returns 42');
    {$ifndef PAS2JS}
    { Native ABI sizes are compiler-oracle evidence, never JavaScript layout. }
    Check((EarlierScopeSize = 1) and (LaterScopeSize = 2),
      'The configured compiler keeps prior global and later nested alias layouts independent');
    {$endif}
    LexicalNames;
    OrdinaryCompletion;
    SourceCondition;
    {$ifndef PAS2JS}
    Check(ParamCount = 1, 'Supply repository root for the exact compiled authored source');
    WholeAuthoredSource(TNyxText(ParamStr(1)));
    {$endif}
    WriteLn('PASS ', GChecks, ' unfinished enclosing scope and completion checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
