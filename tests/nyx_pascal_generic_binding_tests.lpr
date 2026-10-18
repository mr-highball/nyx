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
program nyx_pascal_generic_binding_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses SysUtils, Classes, nyx.text, nyx.scheduler, nyx.pascal.context,
  nyx.pascal.lexing, nyx.pascal.files, nyx.pascal.translation, nyx.pascal.syntax,
  nyx.pascal.syntax.translated, nyx.pascal.binding, binding.fixture.generics
  {$ifndef PAS2JS}, nyx.controls{$endif};
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

function View(const AName, AText: TNyxText): INyxPascalTranslatedSyntax;
var
  LFiles: INyxPascalFileCatalog;
  LFacts: INyxPascalCompilerContext;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile(AName), AText);
  LFacts := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi)
    .WithSymbol(NyxPascalDefine('FPC'), ppdDefined)
    .WithSymbol(NyxPascalDefine('FPC_FULLVERSION'), ppdInteger, 30301)
    .CompleteDefines(True);
  Result := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFiles,
    NyxPascalPhysicalFile(AName), LFacts, TNyxPascalTranslationOptions.Defaults),
    TNyxPascalSyntaxOptions.Defaults);
end;

function Site(const AWorld: INyxPascalSemanticWorld): INyxPascalBindingSite;
begin
  Result := AWorld.At(AWorld.Count - 1, 0,
    AWorld.ModuleAt(AWorld.Count - 1).Syntax.Lexical.Scalars);
end;

function TerminalType(const AType: INyxPascalSemanticType): INyxPascalSemanticType;
begin
  Result := AType;
  while (Result <> nil) and (Result.Kind in [pstAlias, pstDistinct]) do
  begin
    Result := Result.Target;
  end;
end;

function Bound(const AResult: INyxPascalResolution): INyxPascalSemanticType;
begin
  Check((AResult.Status = pbsBound) and (AResult.BoundType <> nil),
    'Expected a concrete type: ' + String(AResult.MessageText));
  Result := AResult.BoundType;
end;

function NamedType(const AWorld: INyxPascalSemanticWorld;
  const AName: TNyxText): INyxPascalSemanticType;
var
  LResult: INyxPascalResolution;
begin
  LResult := AWorld.Lookup(Site(AWorld), NyxPascalBindingName(AName));
  Check((LResult.Status = pbsBound) and (LResult.Count = 1),
    'Expected one named declaration: ' + String(AName));
  Result := Bound(AWorld.TypeOfSymbol(LResult.SymbolAt(0)));
end;

function MemberType(const AWorld: INyxPascalSemanticWorld;
  const AReceiver: INyxPascalSemanticType; const AName: TNyxText): INyxPascalSemanticType;
var
  LResult: INyxPascalResolution;
begin
  LResult := AWorld.Member(AReceiver, Site(AWorld), NyxPascalBindingName(AName));
  Check((LResult.Status = pbsBound) and (LResult.Count = 1),
    'Expected one member: ' + String(AName) + ' / ' + String(LResult.MessageText));
  Result := Bound(AWorld.TypeOfSymbol(LResult.SymbolAt(0)));
end;

function EmptyWorld: INyxPascalSemanticWorld;
begin
  Result := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults)
    .WithModule(View('system.pas',
      'unit System; {$mode delphi} interface type TObject=class end; ' +
      'IInterface=interface end; implementation end.'));
end;

procedure Substitution;
var
  LWorld: INyxPascalSemanticWorld;
  LOld: INyxPascalSemanticWorld;
  LInteger: INyxPascalSemanticType;
  LBoolean: INyxPascalSemanticType;
  LType: INyxPascalSemanticType;
  LResult: INyxPascalResolution;
  LSymbol: INyxPascalBoundSymbol;
  LCapability: INyxPascalSubstitutedSymbol;
  LRejected: Boolean;
begin
  LWorld := EmptyWorld.WithModule(View('templates.pas',
    'unit Templates; {$mode delphi} interface type ' +
    'T=record Outer:Integer; end; ' +
    'TBox<T>=class public Value:T; function Read:T; ' +
    'function Keep:TBox<T>; function AsNumbers:TBox<Integer>; type TItems=array of T; ' +
    'TMap=function(const A:T):T of object; end; ' +
    'TChild<U>=class(TBox<U>) public Extra:U; end; ' +
    'TSequence<V>=array of V; ' +
    'TRecursive<R>=TRecursive<R>; ' +
    'TPair<A,B>=record Left:A; Right:B; end; ' +
    'TOuter<A>=class public type TInner<B>=record Left:A; Right:B; end; end; ' +
    'TBad<C:class>=class end; ' +
    'implementation end.'))
    .WithModule(View('use.pas',
    'unit Consumer; {$mode delphi} interface uses Templates; ' +
    'type T=Boolean; TNumbers=TSequence<Integer>; ' +
    'var Numbers:TBox<Integer>; Flags:TBox<Boolean>; ' +
    'Qualified:Templates.TBox<Boolean>; Cycle:TRecursive<Integer>; ' +
    'Child:TChild<Integer>; Nested:TBox<TBox<Boolean>>; ' +
    'Pair:TPair<Integer,Boolean>; Few:TPair<Integer>; ' +
    'NestedPair:TOuter<Integer>.TInner<Boolean>; ' +
    'type TNestedAlias=TBox<Integer>.TItems; ' +
    'var ' +
    'Many:TBox<Integer,Boolean>; Restricted:TBad<Integer>; ' +
    'implementation end.'));
  LInteger := NamedType(LWorld, 'Numbers');
  LBoolean := NamedType(LWorld, 'Flags');
  Check(TerminalType(MemberType(LWorld, LInteger, 'Value')).Builtin = pbbInteger,
    'A field substitutes its concrete receiver argument rather than an outer T');
  Check(TerminalType(MemberType(LWorld, LBoolean, 'Read')).Builtin = pbbBoolean,
    'Separate specializations of the same declaration never share argument state');
  Check(TerminalType(MemberType(LWorld, LInteger, 'Keep')).Definition.Declaration.Name =
    TNyxText('TBox'), 'A fluent self result keeps the declared generic identity');
  LType := MemberType(LWorld, MemberType(LWorld, LBoolean, 'Keep'), 'Value');
  Check(TerminalType(LType).Builtin = pbbBoolean,
    'A fluent self result retains its actual argument for the next member query');
  LType := MemberType(LWorld, MemberType(LWorld, LBoolean, 'AsNumbers'), 'Value');
  Check(TerminalType(LType).Builtin = pbbInteger,
    'An explicitly different self specialization replaces the current concrete frame');
  LType := MemberType(LWorld, NamedType(LWorld, 'Qualified'), 'Value');
  Check(TerminalType(LType).Builtin = pbbBoolean,
    'Qualified generic prototypes bind their concrete arguments through the same owner');
  LType := MemberType(LWorld, LBoolean, 'TItems');
  Check((TerminalType(LType).Kind = pstArray) and
    (TerminalType(TerminalType(LType).Target).Builtin = pbbBoolean),
    'Nested aliases bind through the enclosing concrete frame');
  LType := TerminalType(MemberType(LWorld, LBoolean, 'TMap'));
  Check((LType.Kind = pstFunction) and (LType.ParameterCount = 1) and
    (TerminalType(LType.ParameterType(0)).Builtin = pbbBoolean) and
    (TerminalType(LType.Target).Builtin = pbbBoolean),
    'Nested callable parameter and result types receive the same substitution');
  LType := NamedType(LWorld, 'Child');
  Check(TerminalType(MemberType(LWorld, LType, 'Value')).Builtin = pbbInteger,
    'Generic inheritance binds the ancestor arguments through its own frame');
  LType := MemberType(LWorld, NamedType(LWorld, 'Nested'), 'Value');
  Check(TerminalType(MemberType(LWorld, LType, 'Value')).Builtin = pbbBoolean,
    'Nested constructed arguments preserve both levels of concrete identity');
  LType := NamedType(LWorld, 'Pair');
  Check((TerminalType(MemberType(LWorld, LType, 'Left')).Builtin = pbbInteger) and
    (TerminalType(MemberType(LWorld, LType, 'Right')).Builtin = pbbBoolean),
    'Multiple placeholders bind independently and preserve their order');
  LType := TerminalType(NamedType(LWorld, 'TNumbers'));
  Check((LType.Kind = pstArray) and
    (TerminalType(LType.Target).Builtin = pbbInteger),
    'Generic aliases retain their own parameter scope outside a member body');
  LResult := LWorld.Lookup(Site(LWorld), NyxPascalBindingName('NestedPair'));
  Check(LWorld.TypeOfSymbol(LResult.SymbolAt(0)).Issue = pbiGenericContext,
    'Compiler-unsupported nested generic declarations refuse explicitly');
  LType := TerminalType(NamedType(LWorld, 'TNestedAlias'));
  Check(TerminalType(LType.Target).Builtin = pbbInteger,
    'Qualification through a concrete receiver retains its nested alias context');
  LResult := LWorld.Lookup(Site(LWorld), NyxPascalBindingName('Few'));
  Check(LWorld.TypeOfSymbol(LResult.SymbolAt(0)).Issue = pbiGenericArity,
    'Too few arguments refuse without partial substitution');
  LResult := LWorld.Lookup(Site(LWorld), NyxPascalBindingName('Many'));
  Check(LWorld.TypeOfSymbol(LResult.SymbolAt(0)).Issue = pbiGenericArity,
    'Too many arguments refuse without discarding extras');
  LResult := LWorld.Lookup(Site(LWorld), NyxPascalBindingName('Restricted'));
  Check(LWorld.TypeOfSymbol(LResult.SymbolAt(0)).Issue = pbiGenericConstraint,
    'Unsupported constraints never certify an invalid concrete specialization');
  LResult := LWorld.Lookup(Site(LWorld), NyxPascalBindingName('Cycle'));
  Check(LWorld.TypeOfSymbol(LResult.SymbolAt(0)).Issue = pbiRecursiveType,
    'Generic alias cycles refuse without retaining recursive type graphs');
  LResult := LWorld.Member(LBoolean, Site(LWorld), NyxPascalBindingName('Value'));
  LSymbol := LResult.SymbolAt(0);
  Check(Supports(LSymbol, INyxPascalSubstitutedSymbol, LCapability) and
    (LCapability.Environment.Count = 1) and
    (LCapability.Environment.Name(0) = TNyxText('T')) and
    (LCapability.Environment.Argument(0).Builtin = pbbBoolean),
    'The returned member exposes immutable concrete context between queries');
  Check(LWorld.ModuleAt(LSymbol.ModuleIndex).Source.Files.FileAt(LSymbol.Origin.FileIndex).Name =
    TNyxText('templates.pas'), 'Substitution preserves the original physical definition');
  LOld := LWorld;
  LWorld := LWorld.WithModule(View('extra.pas',
    'unit Extra; {$mode delphi} interface implementation end.'));
  LRejected := False;
  try
    LWorld.TypeOfSymbol(LSymbol);
  except
    on ENyxPascalBinding do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'A specialized member retains the exact-world handle guard');
  Check(TerminalType(Bound(LOld.TypeOfSymbol(LSymbol))).Builtin = pbbBoolean,
    'The retained old world and argument frame remain usable after replacement');
end;

procedure MemberPages;
var
  LWorld: INyxPascalSemanticWorld;
  LType: INyxPascalSemanticType;
  LQueries: INyxPascalMemberQueries;
  LPage: INyxPascalMemberPage;
  LNext: INyxPascalMemberPage;
  LCancel: INyxCancellationScope;
  LRejected: Boolean;
begin
  LWorld := EmptyWorld.WithModule(View('base.pas',
    'unit Base; {$mode delphi} interface type TBase<T>=class ' +
    'private ItemHidden:T; public ItemTwo:T; function Act(A:T):T; overload; end; ' +
    'implementation end.'))
    .WithModule(View('child.pas',
    'unit Child; {$mode delphi} interface uses Base; ' +
    'type TChild<T>=class(TBase<T>) public ItemOne:T; ' +
    'function Act(A:Boolean):Boolean; overload; end; ' +
    'var Current:TChild<Integer>; implementation end.'));
  LType := NamedType(LWorld, 'Current');
  Check(Supports(LWorld, INyxPascalMemberQueries, LQueries),
    'The owned world exposes optional bounded member queries');
  LPage := LQueries.Members(LType, Site(LWorld),
    TNyxPascalMemberQuery.Defaults.Matching('iTeM').Take(1));
  Check((LPage.ResultSet.Status = pbsBound) and (LPage.ResultSet.Count = 1) and
    (LPage.ResultSet.SymbolAt(0).Declaration.Name = TNyxText('ItemOne')) and
    LPage.HasMore and (LPage.NextOffset = 1),
    'Prefix matching is case-insensitive and first page reports a checked continuation');
  LNext := LQueries.Members(LType, Site(LWorld),
    TNyxPascalMemberQuery.Defaults.Matching('item').Skip(LPage.NextOffset).Take(1));
  Check((LNext.ResultSet.Status = pbsBound) and (LNext.ResultSet.Count = 1) and
    (LNext.ResultSet.SymbolAt(0).Declaration.Name = TNyxText('ItemTwo')) and
    not LNext.HasMore and (LNext.NextOffset = 2),
    'Visible row offsets exclude inaccessible ancestor fields and preserve ordering');
  Check(TerminalType(Bound(LWorld.TypeOfSymbol(LNext.ResultSet.SymbolAt(0)))).Builtin = pbbInteger,
    'Paged inherited candidates retain concrete arguments for a later type query');
  LPage := LQueries.Members(LType, Site(LWorld),
    TNyxPascalMemberQuery.Defaults.Matching('act').Take(2));
  Check((LPage.ResultSet.Count = 2) and not LPage.HasMore and
    (TerminalType(Bound(LWorld.TypeOfSymbol(LPage.ResultSet.SymbolAt(0)))).Builtin = pbbBoolean) and
    (TerminalType(Bound(LWorld.TypeOfSymbol(LPage.ResultSet.SymbolAt(1)))).Builtin = pbbInteger),
    'Overload candidates keep their independent own and ancestor result types');
  LPage := LQueries.Members(LType, Site(LWorld),
    TNyxPascalMemberQuery.Defaults.Matching('item').ForAccess(pmaType));
  Check((LPage.ResultSet.Status = pbsBound) and (LPage.ResultSet.Count = 0) and not LPage.HasMore,
    'A type receiver excludes ordinary instance fields');
  LPage := LQueries.Members(LType, Site(LWorld),
    TNyxPascalMemberQuery.Defaults.Matching('nothing').Skip(500));
  Check((LPage.ResultSet.Status = pbsBound) and (LPage.ResultSet.Count = 0) and
    not LPage.HasMore, 'An empty bounded query has no invented candidates');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LRejected := False;
  try
    LQueries.Members(LType, Site(LWorld), TNyxPascalMemberQuery.Defaults, LCancel);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Member pages honor cancellation before scanning');
  LRejected := False;
  try
    TNyxPascalMemberQuery.Defaults.Take(51);
  except
    on ENyxPascalBinding do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Page requests cannot bypass the 50-row response limit');
  LRejected := False;
  try
    TNyxPascalMemberQuery.Defaults.Skip(High(Integer));
  except
    on ENyxPascalBinding do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'A continuation offset cannot overflow when rows are appended');
  LWorld := EmptyWorld.WithModule(View('missing.pas',
    'unit MissingContext; {$mode delphi} interface uses Uncaptured; ' +
    'type TThing=class(TUnknown) public Item:Integer; end; ' +
    'var Current:TThing; implementation end.'));
  LType := NamedType(LWorld, 'Current');
  Supports(LWorld, INyxPascalMemberQueries, LQueries);
  LPage := LQueries.Members(LType, Site(LWorld),
    TNyxPascalMemberQuery.Defaults.Matching('item'));
  Check((LPage.ResultSet.Status = pbsUnresolved) and
    (LPage.ResultSet.Issue = pbiMissingModule) and
    (LPage.ResultSet.RelatedName = TNyxText('Uncaptured')) and
    (LPage.ResultSet.Count = 0) and not LPage.HasMore,
    'An uncaptured ancestor refuses the whole page instead of claiming a complete list');
  LWorld := EmptyWorld.WithModule(View('ambiguous.pas',
    'unit Ambiguous; {$mode delphi} interface type ' +
    'TBase=record end; TBase=record end; ' +
    'TThing=class(TBase) public Item:Integer; end; ' +
    'var Current:TThing; implementation end.'));
  LType := NamedType(LWorld, 'Current');
  Supports(LWorld, INyxPascalMemberQueries, LQueries);
  LPage := LQueries.Members(LType, Site(LWorld), TNyxPascalMemberQuery.Defaults);
  Check((LPage.ResultSet.Status = pbsAmbiguous) and (LPage.ResultSet.Count = 0) and
    not LPage.HasMore, 'Ambiguous ancestor declarations never leak as member rows');
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults.UpToCandidates(1))
    .WithModule(View('small.pas',
      'unit Small; {$mode delphi} interface type TData=record Item:Integer; end; ' +
      'var Current:TData; implementation end.'));
  LType := NamedType(LWorld, 'Current');
  Supports(LWorld, INyxPascalMemberQueries, LQueries);
  LRejected := False;
  try
    LQueries.Members(LType, Site(LWorld), TNyxPascalMemberQuery.Defaults.Take(2));
  except
    on ENyxPascalBindingLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'A page cannot exceed a stricter source world candidate budget');
  LPage := LQueries.Members(LType, Site(LWorld), TNyxPascalMemberQuery.Defaults.Take(1));
  Check((LPage.ResultSet.Count = 1) and not LPage.HasMore,
    'A request within the stricter world budget still returns its exact member');
end;

{$ifndef PAS2JS}
function ReadText(const APath: String): TNyxText;
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Result, LStream.Size);

    if LStream.Size > 0 then
    begin
      LStream.ReadBuffer(Result[1], Length(Result));
    end;
  finally
    LStream.Free;
  end;
end;

procedure CompiledFixture;
var
  LWorld: INyxPascalSemanticWorld;
  LType: INyxPascalSemanticType;
  LInteger: TBindingDerived<Integer>;
  LBoolean: TBindingBox<Boolean>;
  LPair: TBindingPair<Integer, Boolean>;
  LItems: TBindingSequence<Integer>;
  LNested: TBindingOuter<Integer>.TInner;
begin
  Check(ParamCount = 1, 'Native qualification requires the exact repository input root');
  LWorld := EmptyWorld.WithModule(View('binding.fixture.generics.pas',
    ReadText(IncludeTrailingPathDelimiter(ParamStr(1)) + 'tests/binding.fixture.generics.pas')))
    .WithModule(View('observer.pas',
      'unit Observer; {$mode delphi} interface uses binding.fixture.generics; ' +
      'var Current:TBindingDerived<Integer>; Flags:TBindingBox<Boolean>; ' +
      'Items:TBindingSequence<Integer>; ' +
      'Nested:TBindingOuter<Integer>.TInner; implementation end.'));
  LType := NamedType(LWorld, 'Current');
  Check(TerminalType(MemberType(LWorld, LType, 'Value')).Builtin = pbbInteger,
    'The exact compiled generic property resolves through its actual ancestor');
  Check(TerminalType(MemberType(LWorld, LType, 'ReadValue')).Builtin = pbbInteger,
    'The exact compiled method result resolves through its actual ancestor');
  LType := TerminalType(MemberType(LWorld, NamedType(LWorld, 'Flags'), 'TMapper'));
  Check((LType.Kind = pstFunction) and
    (TerminalType(LType.ParameterType(0)).Builtin = pbbBoolean) and
    (TerminalType(LType.Target).Builtin = pbbBoolean),
    'The compiler-captured nested callable binds its actual signature');
  LType := TerminalType(NamedType(LWorld, 'Items'));
  Check(TerminalType(LType.Target).Builtin = pbbInteger,
    'The exact compiled generic alias resolves its element');
  LType := NamedType(LWorld, 'Nested');
  Check(TerminalType(MemberType(LWorld, LType, 'OuterValue')).Builtin = pbbInteger,
    'The compiler-captured nested record retains its enclosing concrete frame');
  LInteger := TBindingDerived<Integer>.Create;
  LBoolean := TBindingBox<Boolean>.Create;
  try
    LInteger.Put(73);
    LInteger.Extra := 19;
    LBoolean.Value := True;
    LPair.Left := LInteger.ReadValue;
    LPair.Right := LBoolean.ReadValue;
    SetLength(LItems, 1);
    LItems[0] := LPair.Left;
    LNested.OuterValue := LPair.Left;
    Check((LInteger.Value = 73) and (LInteger.Extra = 19) and
      (LInteger.SelfType = LInteger) and LPair.Right and (LItems[0] = 73) and
      (LNested.OuterValue = 73),
      'Actual FPC constructors, properties, generic methods, records and aliases agree');
  finally
    LBoolean.Free;
    LInteger.Free;
  end;
end;

procedure ActualBadge;
const
  CFiles: array[0..5] of String = ('nyx.controls.pas', 'nyx.controls.facades.inc',
    'nyx.controls.types.inc', 'nyx.controls.factories.inc', 'nyx.controls.implementation.inc',
    'nyx.controls.facades.implementation.inc');
var
  LFiles: INyxPascalFileCatalog;
  LFacts: INyxPascalCompilerContext;
  LView: INyxPascalTranslatedSyntax;
  LWorld: INyxPascalSemanticWorld;
  LType: INyxPascalSemanticType;
  LBadgeType: INyxPascalSemanticType;
  LBox: TBindingBox<INyxBadge>;
  LBadge: INyxBadge;
  LAt: Integer;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact);
  for LAt := Low(CFiles) to High(CFiles) do
  begin
    LFiles := LFiles.WithFile(NyxPascalPhysicalFile(TNyxText(CFiles[LAt])),
      ReadText(IncludeTrailingPathDelimiter(ParamStr(1)) + 'src/' + CFiles[LAt]));
  end;
  LFacts := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi)
    .WithSymbol(NyxPascalDefine('FPC'), ppdDefined)
    .WithSymbol(NyxPascalDefine('FPC_FULLVERSION'), ppdInteger, 30301)
    .CompleteDefines(True);
  LView := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFiles,
    NyxPascalPhysicalFile('nyx.controls.pas'), LFacts, TNyxPascalTranslationOptions.Defaults),
    TNyxPascalSyntaxOptions.Defaults);
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults)
    .WithModule(LView)
    .WithModule(View('binding.fixture.generics.pas',
      ReadText(IncludeTrailingPathDelimiter(ParamStr(1)) + 'tests/binding.fixture.generics.pas')))
    .WithModule(View('badge.consumer.pas',
      'unit BadgeConsumer; {$mode delphi} interface uses binding.fixture.generics, nyx.controls; ' +
      'var Current:TBindingBox<INyxBadge>; implementation end.'));
  LType := NamedType(LWorld, 'Current');
  LBadgeType := TerminalType(MemberType(LWorld, LType, 'Value'));
  Check((LBadgeType.Kind = pstDeclared) and
    (LBadgeType.Definition.Declaration.Name = TNyxText('INyxBadge')),
    'A real SDK interface survives generic property substitution as its own type');
  LType := TerminalType(MemberType(LWorld, LBadgeType, 'WithText'));
  Check((LType.Definition.DeclarationIndex = LBadgeType.Definition.DeclarationIndex) and
    (LType.Definition.ModuleIndex = LBadgeType.Definition.ModuleIndex),
    'Generic continuation preserves the actual specialized badge fluent result identity');
  Check(LView.Source.Files.FileAt(LBadgeType.Definition.Origin.FileIndex).Name =
    TNyxText('nyx.controls.types.inc'), 'The generic result still navigates into the physical SDK include');
  LBox := TBindingBox<INyxBadge>.Create;
  try
    LBox.Put(NewNyxBadge('generic-badge').WithText('Ready'));
    LBadge := LBox.ReadValue.WithText('Concrete and reusable');
    Check((LBadge = LBox.Value) and (LBadge.Text = TNyxText('Concrete and reusable')),
      'The actual compiler and Nyx control agree with the resolved generic continuation');
  finally
    LBadge := nil;
    LBox.Free;
  end;
end;
{$endif}

begin
  try
    Substitution;
    MemberPages;
    {$ifndef PAS2JS}
    CompiledFixture;
    ActualBadge;
    {$endif}
    WriteLn('PASS ', GChecks, ' concrete generic binding checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL / ', LException.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
