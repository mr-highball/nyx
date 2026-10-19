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
program nyx_pascal_binding_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Classes, nyx.text, nyx.scheduler, nyx.pascal.context,
  nyx.pascal.lexing, nyx.pascal.files, nyx.pascal.translation, nyx.pascal.syntax,
  nyx.pascal.syntax.translated, nyx.pascal.binding,
  binding.fixture.base, binding.fixture.child;

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
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi)
    .WithSymbol(NyxPascalDefine('FPC'), ppdDefined)
    .WithSymbol(NyxPascalDefine('FPC_FULLVERSION'), ppdInteger, 30301)
    .CompleteDefines(True);
end;

function View(const AName, AText: TNyxText): INyxPascalTranslatedSyntax;
var
  LFiles: INyxPascalFileCatalog;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile(AName), AText);
  Result := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFiles,
    NyxPascalPhysicalFile(AName), Facts, TNyxPascalTranslationOptions.Defaults),
    TNyxPascalSyntaxOptions.Defaults);
end;

function EndSite(const AWorld: INyxPascalSemanticWorld; AModule: Integer): INyxPascalBindingSite;
begin
  Result := AWorld.At(AModule, 0, AWorld.ModuleAt(AModule).Syntax.Lexical.Scalars);
end;

function Lookup(const AWorld: INyxPascalSemanticWorld; AModule: Integer;
  const AName: TNyxText): INyxPascalResolution;
begin
  Result := AWorld.Lookup(EndSite(AWorld, AModule), NyxPascalBindingName(AName));
end;

function NamedSymbol(const AWorld: INyxPascalSemanticWorld; AModule: Integer;
  const AName: TNyxText): INyxPascalBoundSymbol;
var
  LResult: INyxPascalResolution;
begin
  LResult := Lookup(AWorld, AModule, AName);
  Check((LResult.Status = pbsBound) and (LResult.Count = 1),
    'An exact supported name binds to one declaration: ' + String(AName));
  Result := LResult.SymbolAt(0);
end;

function Shape(const AWorld: INyxPascalSemanticWorld;
  const ASymbol: INyxPascalBoundSymbol): INyxPascalSemanticType;
var
  LResult: INyxPascalResolution;
begin
  LResult := AWorld.TypeOfSymbol(ASymbol);
  Check((LResult.Status = pbsBound) and (LResult.BoundType <> nil),
    'An explicit supported declaration has a bound type: ' + String(ASymbol.Declaration.Name) +
    ' / ' + String(LResult.MessageText));
  Result := LResult.BoundType;
end;

function TerminalType(const AType: INyxPascalSemanticType): INyxPascalSemanticType;
begin
  Result := AType;
  while Result.Kind in [pstAlias, pstDistinct] do
  begin
    Result := Result.Target;
  end;
end;

{ Executable lifecycle scopes bind through their own module, never through a
  preceding routine's locals. Unsupported inline declarations retain the same
  refusal as other executable constructs, without fabricating a shadow. }
procedure LifecycleBinding;
var
  LText: TNyxText;
  LView: INyxPascalTranslatedSyntax;
  LWorld: INyxPascalSemanticWorld;
  LResult: INyxPascalResolution;
  LAt: Integer;
  LScope: Integer;
begin
  LText := 'unit Phases; interface var Shared:Integer; implementation var PrivateValue:Integer; ' +
    'procedure Previous; var Shared:Byte; begin Shared:=0; end; ' +
    'initialization begin Shared:=42; end; finalization PrivateValue:=0; end.';
  LView := View('phases.pas', LText);
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LView);
  LAt := Pos(TNyxText('Shared:=42'), LText) - 1;
  LScope := LView.Syntax.ScopeAt(LAt);
  LResult := LWorld.Lookup(LWorld.At(0, LScope, LAt), NyxPascalBindingName('Shared'));
  Check((LResult.Status = pbsBound) and (LResult.Count = 1) and
    (LResult.SymbolAt(0).DeclarationIndex = LView.Syntax.Named(0, 'Shared')),
    'Initialization binds the module variable rather than a previous routine local');
  LAt := Pos(TNyxText('PrivateValue:=0'), LText) - 1;
  LScope := LView.Syntax.ScopeAt(LAt);
  LResult := LWorld.Lookup(LWorld.At(0, LScope, LAt), NyxPascalBindingName('PrivateValue'));
  Check((LResult.Status = pbsBound) and (LResult.Count = 1) and
    (LResult.SymbolAt(0).Declaration.Visibility = psavImplementation),
    'Finalization can bind its own implementation variable');
  LText := 'unit BadPhase; interface var Shared:Integer; implementation ' +
    'initialization var Shared:Byte; Shared:=1; end.';
  LView := View('badphase.pas', LText);
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LView);
  LAt := Pos(TNyxText('Shared:=1'), LText) - 1;
  LResult := LWorld.Lookup(LWorld.At(0, LView.Syntax.ScopeAt(LAt), LAt), NyxPascalBindingName('Shared'));
  Check((LResult.Status = pbsUnresolved) and (LResult.Issue = pbiUnsupportedScope),
    'An unsupported inline shadow cannot grant false outer-scope lookup');
end;

{ Typed procedural requests survive ordinary semantic binding and aliases.
  The caller's captured global default is a separate fact and is never replaced
  by a platform-specific request authored on one declaration. }
procedure WinapiBinding;
var
  LContext: INyxPascalCompilerContext;
  LBefore: INyxPascalCompilerContext;
  LView: INyxPascalTranslatedSyntax;
  LWorld: INyxPascalSemanticWorld;
  LType: INyxPascalSemanticType;
  LFiles: INyxPascalFileCatalog;
begin
  LContext := NyxPascalWithCompilerPolicies(Facts,
    TNyxPascalCompilerPolicies.Unknown.CallBy(pccCdecl));
  LBefore := CaptureNyxPascalCompilerContext(LContext);
  LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('abibindings.pas'),
    'unit ABIBindings; interface type TResult=record end; ' +
    'TCallback=function(AValue:TResult):TResult; winapi; ' +
    'var Direct:function(AValue:TResult):TResult; winapi; Named:TCallback; implementation end.');
  LView := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFiles,
    NyxPascalPhysicalFile('abibindings.pas'), LContext, TNyxPascalTranslationOptions.Defaults),
    TNyxPascalSyntaxOptions.Defaults);
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LView);
  LType := Shape(LWorld, NamedSymbol(LWorld, 0, 'Direct'));
  Check((LType.Kind = pstFunction) and
    (LType.Syntax.Node(LType.SyntaxNode).CallingConvention = prcWinapi),
    'An inline callback retains the symbolic Winapi request through ordinary type binding');
  Check((LType.ParameterCount = 1) and (LType.ParameterName(0) = TNyxText('AValue')),
    'Convention binding preserves the original parameter signature');
  LType := TerminalType(Shape(LWorld, NamedSymbol(LWorld, 0, 'Named')));
  Check((LType.Kind = pstFunction) and
    (LType.Syntax.Node(LType.SyntaxNode).CallingConvention = prcWinapi),
    'A named callback alias retains the same typed request');
  Check(LContext.SameFacts(LBefore) and
    (NyxPascalCompilerPolicies(LView.Source.State(LView.Source.StateCount - 1)).CallingConvention = pccCdecl),
    'Authored routine conventions do not mutate or replace captured global calling policy');
end;

procedure Basic;
var
  LWorld: INyxPascalSemanticWorld;
  LChanged: INyxPascalSemanticWorld;
  LSmall: INyxPascalSemanticWorld;
  LResult: INyxPascalResolution;
  LOld: INyxPascalBoundSymbol;
  LType: INyxPascalSemanticType;
  LSite: INyxPascalBindingSite;
  LAt: Integer;
  LScope: Integer;
  LRejected: Boolean;
  LCancel: INyxCancellationScope;
begin
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults)
    .WithModule(View('system.pas',
      'unit System; {$mode delphi} interface type TObject=class end; IInterface=interface end; implementation end.'))
    .WithModule(View('left.pas',
      'unit Left; {$mode delphi} interface type TCollision=record LeftValue: Integer; end; implementation var Internal: Integer; end.'))
    .WithModule(View('right.pas',
      'unit Right; {$mode delphi} interface type TCollision=record RightValue: Integer; end; implementation end.'))
    .WithModule(View('consumer.pas',
      'unit Consumer; {$mode delphi} interface uses Left, Right; ' +
      'type TAlias=Right.TCollision; TDistinct=type Integer; ' +
      'TCallback=function(const A,B:Integer): Boolean of object; ' +
      'var Current:TAlias; implementation ' +
      'procedure Run; var Shared: Integer; procedure Nested(Shared: TAlias); var Counter: Integer; ' +
      'begin Counter:=0 end; begin Shared:=1; Nested(Current) end; end.'));
  LResult := Lookup(LWorld, 3, 'TCollision');
  Check((LResult.Status = pbsBound) and (LResult.SymbolAt(0).ModuleIndex = 2),
    'Last directly used unit wins without importing another unit transitively');
  LResult := Lookup(LWorld, 3, 'Left.TCollision');
  Check((LResult.Status = pbsBound) and (LResult.SymbolAt(0).ModuleIndex = 1),
    'Explicit unit qualification bypasses a later same-named declaration');
  Check(Lookup(LWorld, 3, 'Left.Internal').Status = pbsMissing,
    'Implementation-only globals are not unit exports');
  Check(Lookup(LWorld, 1, 'Left.Internal').Status = pbsBound,
    'Own-unit qualification retains implementation visibility at the caller position');
  Check(Lookup(LWorld, 3, 'System.Integer').BoundType.Builtin = pbbInteger,
    'Qualified language intrinsics retain automatic System meaning');
  LType := Shape(LWorld, NamedSymbol(LWorld, 3, 'Current'));
  Check((LType.Kind = pstAlias) and
    (TerminalType(LType).Definition.ModuleIndex = 2),
    'Variable types follow transparent qualified aliases into their actual declaration');
  LResult := LWorld.Member(LType, EndSite(LWorld, 3), NyxPascalBindingName('RightValue'));
  Check((LResult.Status = pbsBound) and (LResult.SymbolAt(0).Declaration.Kind = psdkField),
    'A record instance exposes its bound field rather than a same-named global');
  Check(LWorld.Member(LType, EndSite(LWorld, 3), NyxPascalBindingName('RightValue'),
    pmaType).Issue = pbiWrongMemberKind, 'A type receiver cannot acquire an instance field');
  LType := Shape(LWorld, NamedSymbol(LWorld, 3, 'TDistinct'));
  Check(LType.Kind = pstDistinct, 'Distinct alias identity is not silently erased');
  LType := TerminalType(Shape(LWorld, NamedSymbol(LWorld, 3, 'TCallback')));
  Check((LType.Kind = pstFunction) and (LType.ParameterCount = 2) and
    (LType.ParameterName(0) = TNyxText('A')) and
    (LType.ParameterMode(1) = pspmConst) and
    (TerminalType(LType.ParameterType(0)).Builtin = pbbInteger) and
    (LType.Target.Builtin = pbbBoolean),
    'Bound callback signatures retain exact grouped parameter names/modes and result');

  LAt := LWorld.ModuleAt(3).Syntax.Named(0, 'Run');
  LScope := LWorld.ModuleAt(3).Syntax.Declaration(LAt).NestedScope;
  LAt := LWorld.ModuleAt(3).Syntax.Named(LScope, 'Nested');
  LScope := LWorld.ModuleAt(3).Syntax.Declaration(LAt).NestedScope;
  LSite := LWorld.At(3, LScope, LWorld.ModuleAt(3).Syntax.Scope(LScope).Span.Finish - 1);
  LResult := LWorld.Lookup(LSite, NyxPascalBindingName('Shared'));
  Check((LResult.Status = pbsBound) and
    (LResult.SymbolAt(0).Declaration.Kind = psdkParameter),
    'Nested parameter shadowing wins over the outer variable');
  Check(TerminalType(Shape(LWorld, LResult.SymbolAt(0))).Definition.ModuleIndex = 2,
    'The shadowing parameter retains its own aliased type');

  LChanged := LWorld.WithModule(View('consumer.pas',
    'unit Consumer; {$mode delphi} interface uses Left, MissingLater; implementation end.'));
  LResult := Lookup(LChanged, 3, 'TCollision');
  Check((LResult.Issue = pbiMissingModule) and
    (LResult.RelatedName = TNyxText('MissingLater')),
    'An uncaptured later use blocks an earlier candidate instead of guessing');
  Check(Lookup(LChanged, 3, 'Left.TCollision').Status = pbsBound,
    'Explicit known-unit context remains queryable alongside an uncaptured later import');
  LOld := NamedSymbol(LWorld, 3, 'Current');
  LRejected := False;
  try
    LChanged.TypeOfSymbol(LOld);
  except
    on ENyxPascalBinding do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (Shape(LWorld, LOld) <> nil),
    'Replacement worlds refuse old handles while the retained source world survives');
  LRejected := False;
  try
    LChanged.Lookup(EndSite(LWorld, 3), NyxPascalBindingName('TCollision'));
  except
    on ENyxPascalBinding do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Caller scope handles cannot silently rebase to another source world');
  LRejected := False;
  try
    LChanged.Member(LType, EndSite(LChanged, 3), NyxPascalBindingName('Value'));
  except
    on ENyxPascalBinding do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Semantic receiver types retain the same exact-world guard');

  LChanged := LWorld.WithModule(View('generic.pas',
    'unit Generic; {$mode delphi} interface type T=record Value:Integer; end; ' +
    'TBox<T>=class public Value:T; end; var Box:TBox<Integer>; implementation end.'));
  LResult := LChanged.TypeOfSymbol(NamedSymbol(LChanged, 4, 'Box'));
  Check((LResult.Status = pbsBound) and (LResult.BoundType.Kind = pstDeclared),
    'A concrete generic argument binds its supported declared receiver');
  LAt := LChanged.ModuleAt(4).Syntax.Named(0, 'TBox');
  LScope := LChanged.ModuleAt(4).Syntax.Declaration(LAt).NestedScope;
  LResult := LChanged.Lookup(LChanged.At(4, LScope,
    LChanged.ModuleAt(4).Syntax.Scope(LScope).Span.Finish - 1), NyxPascalBindingName('T'));
  Check(LResult.Issue = pbiGenericContext,
    'An unresolved generic placeholder cannot bind to a same-named outer type');

  LChanged := LWorld.WithModule(View('cyca.pas',
    'unit CycleA; {$mode delphi} interface uses CycleB; type TA=CycleB.TB; implementation end.'))
    .WithModule(View('cycb.pas',
    'unit CycleB; {$mode delphi} interface uses CycleA; type TB=CycleA.TA; implementation end.'));
  Check(LChanged.TypeOfSymbol(NamedSymbol(LChanged, 4, 'TA')).Issue = pbiRecursiveType,
    'Recursive imported aliases return an explicit diagnostic without retaining a type cycle');
  LSmall := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults.UpToSteps(1))
    .WithModule(LWorld.ModuleAt(1));
  LRejected := False;
  try
    LSmall.Lookup(EndSite(LSmall, 0), NyxPascalBindingName('TCollision'));
  except
    on ENyxPascalBindingLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Step budgets refuse the query instead of returning truncated context');
  LSmall := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults.UpToCandidates(1))
    .WithModule(View('overload.pas',
      'unit Overloaded; {$mode delphi} interface procedure Work(A:Integer); overload; ' +
      'procedure Work(A:Boolean); overload; implementation end.'));
  LRejected := False;
  try
    LSmall.Lookup(EndSite(LSmall, 0), NyxPascalBindingName('Work'));
  except
    on ENyxPascalBindingLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Candidate budgets never silently discard an overload alternative');
  LSmall := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults)
    .WithModule(LSmall.ModuleAt(0));
  Check(Lookup(LSmall, 0, 'Work').Status = pbsOverloads,
    'Overloads remain an explicit family without pretending to choose a call');
  LSmall := LSmall.WithModule(View('ambiguous.pas',
    'unit Ambiguous; {$mode delphi} interface type TThing=record end; TThing=record end; implementation end.'));
  Check(Lookup(LSmall, 1, 'TThing').Status = pbsAmbiguous,
    'Conflicting nonroutine declarations are not reduced to the first name');
  LSmall := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults.UpToModules(1))
    .WithModule(LWorld.ModuleAt(1));
  LRejected := False;
  try
    LSmall.WithModule(LWorld.ModuleAt(2));
  except
    on ENyxPascalBindingLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LSmall.Count = 1), 'Module limits preserve the original immutable world');
  LSmall := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults.UpToDepth(2))
    .WithModule(View('depth.pas',
      'unit Depth; {$mode delphi} interface type TAlias=Integer; implementation end.'));
  LOld := NamedSymbol(LSmall, 0, 'TAlias');
  LRejected := False;
  try
    LSmall.TypeOfSymbol(LOld);
  except
    on ENyxPascalBindingLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Recursion budgets cover qualified-name and alias binding stages');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LRejected := False;
  try
    LWorld.Lookup(EndSite(LWorld, 3), NyxPascalBindingName('Current'), LCancel);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Cancellation refuses without publishing a semantic response');
end;

{$ifndef PAS2JS}
function ReadText(const APath: String): TNyxText;
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Result, LStream.Size);

    if Length(Result) > 0 then
    begin
      LStream.ReadBuffer(Result[1], Length(Result));
    end;
  finally
    LStream.Free;
  end;
end;

procedure CompiledFixtures;
var
  LWorld: INyxPascalSemanticWorld;
  LType: INyxPascalSemanticType;
  LResult: INyxPascalResolution;
  LSite: INyxPascalBindingSite;
  LAt: Integer;
  LScope: Integer;
  LNative: TBindingChild;
begin

  if ParamCount = 0 then
  begin
    Exit;
  end;
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults)
    .WithModule(View('system.pas',
      'unit System; {$mode delphi} interface type TObject=class end; IInterface=interface end; implementation end.'))
    .WithModule(View('binding.fixture.base.pas',
      ReadText(IncludeTrailingPathDelimiter(ParamStr(1)) + 'tests/binding.fixture.base.pas')))
    .WithModule(View('binding.fixture.child.pas',
      ReadText(IncludeTrailingPathDelimiter(ParamStr(1)) + 'tests/binding.fixture.child.pas')))
    .WithModule(View('observer.pas',
      'unit Observer; {$mode delphi} interface uses binding.fixture.base, binding.fixture.child; ' +
      'var Current:TBindingAlias; implementation end.'));
  LType := Shape(LWorld, NamedSymbol(LWorld, 3, 'Current'));
  LResult := LWorld.Member(LType, EndSite(LWorld, 3), NyxPascalBindingName('WithValue'));
  Check((LResult.Status = pbsBound) and
    (TerminalType(Shape(LWorld, LResult.SymbolAt(0))).Definition.Declaration.Name = TNyxText('TBindingBase')),
    'Compiled fluent return types bind back to their declaring class');
  Check(LWorld.Member(LType, EndSite(LWorld, 3), NyxPascalBindingName('FUnitSecret')).Status = pbsInaccessible,
    'Another unit cannot acquire unit-private fields');
  Check(LWorld.Member(LType, EndSite(LWorld, 1), NyxPascalBindingName('FUnitSecret')).Status = pbsBound,
    'The defining unit can read ordinary private fields');
  Check(LWorld.Member(LType, EndSite(LWorld, 1), NyxPascalBindingName('FSecret')).Status = pbsInaccessible,
    'Strict private fields remain unavailable outside owning methods even in the same unit');
  LAt := LWorld.ModuleAt(1).Syntax.Named(0, 'RevealSecret');
  LScope := LWorld.ModuleAt(1).Syntax.Declaration(LAt).NestedScope;
  LSite := LWorld.At(1, LScope, LWorld.ModuleAt(1).Syntax.Scope(LScope).Span.Finish - 1);
  Check(LWorld.Member(LType, LSite, NyxPascalBindingName('FSecret')).Status = pbsBound,
    'The actual owning method retains its strict-private declaration access');
  LAt := LWorld.ModuleAt(2).Syntax.Named(0, 'ProbeProtected');
  LScope := LWorld.ModuleAt(2).Syntax.Declaration(LAt).NestedScope;
  LSite := LWorld.At(2, LScope, LWorld.ModuleAt(2).Syntax.Scope(LScope).Span.Finish - 1);
  Check(LWorld.Member(LType, LSite, NyxPascalBindingName('FProtected')).Status = pbsBound,
    'Cross-unit descendants can access protected members');
  Check(LWorld.Member(LType, LSite, NyxPascalBindingName('FStrictProtected')).Status = pbsBound,
    'Cross-unit descendants retain strict-protected access');
  Check(LWorld.Member(LType, EndSite(LWorld, 1), NyxPascalBindingName('FStrictProtected')).Status = pbsInaccessible,
    'Strict protected is not a unit-wide privilege');
  Check(LWorld.Member(LType, LSite, NyxPascalBindingName('FSecret')).Status = pbsInaccessible,
    'A descendant never acquires strict-private base fields');
  LType := Shape(LWorld, NamedSymbol(LWorld, 1, 'TBindingForwardAlias'));
  Check(TerminalType(LType).Definition.Declaration.Name = TNyxText('TBindingForward'),
    'A visible forward identity resolves to its later completed definition');
  LNative := TBindingChild.Create;
  try
    Check((LNative.ProbeProtected = 28) and (BindingSameUnitSecret(LNative) = 1) and
      (LNative.RevealSecret = 2) and (LNative.WithValue(23) = LNative),
      'The same actual compiled fixture units execute their permitted visibility/fluent behavior');
  finally
    LNative.Free;
  end;
end;

procedure ActualControls;
const
  CFiles: array[0..5] of String = ('nyx.controls.pas', 'nyx.controls.facades.inc',
    'nyx.controls.types.inc', 'nyx.controls.factories.inc', 'nyx.controls.implementation.inc',
    'nyx.controls.facades.implementation.inc');
var
  LFiles: INyxPascalFileCatalog;
  LView: INyxPascalTranslatedSyntax;
  LWorld: INyxPascalSemanticWorld;
  LBadge: INyxPascalBoundSymbol;
  LType: INyxPascalSemanticType;
  LResult: INyxPascalResolution;
  LOrigin: TNyxPascalSyntaxOrigin;
  LAt: Integer;
begin

  if ParamCount = 0 then
  begin
    Exit;
  end;
  LFiles := NewNyxPascalFileCatalog(pfcExact);
  for LAt := Low(CFiles) to High(CFiles) do
  begin
    LFiles := LFiles.WithFile(NyxPascalPhysicalFile(TNyxText(CFiles[LAt])),
      ReadText(IncludeTrailingPathDelimiter(ParamStr(1)) + 'src/' + CFiles[LAt]));
  end;
  LView := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFiles,
    NyxPascalPhysicalFile('nyx.controls.pas'), Facts, TNyxPascalTranslationOptions.Defaults),
    TNyxPascalSyntaxOptions.Defaults);
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LView);
  LResult := LWorld.Lookup(LWorld.At(0, 0, LWorld.InterfaceBoundary(0)),
    NyxPascalBindingName('INyxBadge'));
  Check((LResult.Status = pbsBound) and (LResult.Count = 1), 'Real SDK forward/full badge declarations unify by type identity');
  LBadge := LResult.SymbolAt(0);
  LType := Shape(LWorld, LBadge);
  LResult := LWorld.Member(LType, LWorld.At(0, 0, LWorld.InterfaceBoundary(0)), NyxPascalBindingName('WithText'));
  Check((LResult.Status = pbsBound) and (LResult.Count = 1),
    'The real badge interface resolves its own specialized fluent member');
  Check(TerminalType(Shape(LWorld, LResult.SymbolAt(0))).Definition.DeclarationIndex = LBadge.DeclarationIndex,
    'Real fluent member result binding returns the same badge declaration identity');
  LOrigin := LResult.SymbolAt(0).Origin;
  Check(LView.Source.Files.FileAt(LOrigin.FileIndex).Name = TNyxText('nyx.controls.types.inc'),
    'Bound SDK member definitions retain their exact original include');
  LResult := LWorld.Member(LType, LWorld.At(0, 0, LWorld.InterfaceBoundary(0)), NyxPascalBindingName('GetContent'));
  Check((LResult.Status = pbsBound) and
    (LResult.SymbolAt(0).Origin.FileIndex = 0), 'Real inherited members resolve through INyxControl into the main physical unit');
  WriteLn('actual SDK / real badge identity, own/inherited members and specialized result binding');
end;
{$endif}

begin
  try
    Basic;
    LifecycleBinding;
    WinapiBinding;
    {$ifndef PAS2JS}
    CompiledFixtures;
    ActualControls;
    {$endif}
    WriteLn('PASS ', GChecks, ' scoped Pascal binding checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL / ', LException.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
