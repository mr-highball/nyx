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
program nyx_pascal_interface_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Classes, nyx.text, nyx.scheduler, nyx.pascal.context,
  nyx.pascal.lexing, nyx.pascal.files,
  nyx.pascal.translation, nyx.pascal.syntax, nyx.pascal.syntax.translated,
  nyx.pascal.binding, nyx.pascal.work
  {$ifndef PAS2JS}, interfaces.style.fixture{$endif};

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then raise Exception.Create(AReason);
end;

{ No implicit style is supplied unless the caller has an actual observation. }
function Facts(AStyle: TNyxPascalInterfaceStyle = pifsUnknown): INyxPascalCompilerContext;
begin
  Result := NyxPascalWithCompilerPolicies(NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi).CompleteDefines(True)
    .WithSymbol(NyxPascalDefine('MAYBE'), ppdUnknown),
    TNyxPascalCompilerPolicies.Unknown.UseInterfaces(AStyle));
end;

function Translate(const AText: TNyxText; AInitial: TNyxPascalInterfaceStyle = pifsUnknown;
  const APart: TNyxText = ''): INyxPascalTranslation;
begin
  Result := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('main.pas'), AText)
    .WithFile(NyxPascalPhysicalFile('part.inc'), APart), NyxPascalPhysicalFile('main.pas'),
    Facts(AInitial), TNyxPascalTranslationOptions.Defaults);
end;

function Marker(const ASource: INyxPascalTranslation; const AName: TNyxText): Integer;
var
  LAt: Integer;
begin
  for LAt := 0 to ASource.Count - 1 do
  begin

    if ASource.Text(LAt) = AName then Exit(LAt);
  end;
  raise Exception.Create('Missing original interface-style marker');
end;

function StyleAt(const ASource: INyxPascalTranslation; const AName: TNyxText): TNyxPascalInterfaceStyle;
begin
  Result := NyxPascalCompilerPolicies(ASource.State(ASource.Token(Marker(ASource, AName)).State)).InterfaceStyle;
end;

procedure PolicyValues;
var
  LUnknown, LCom, LRaw, LRestored: INyxPascalCompilerContext;
  LPolicy: TNyxPascalCompilerPolicies;
begin
  LUnknown := Facts;
  LCom := Facts(pifsCOM);
  LRaw := Facts(pifsCORBA);
  LPolicy := TNyxPascalCompilerPolicies.Unknown;
  Check(LPolicy.InterfaceStyle = pifsUnknown, 'Zero policy does not guess a default');
  Check(LPolicy.UseInterfaces(pifsCOM).InterfaceStyle = pifsCOM, 'Fluent COM request is typed');
  Check(LPolicy.InterfaceStyle = pifsUnknown, 'Policy copies do not mutate their parent');
  Check(CaptureNyxPascalCompilerContext(LCom).SameFacts(LCom), 'Detached capture retains interface style');
  Check(not LCom.SameFacts(LRaw) and not LUnknown.SameFacts(LCom), 'Styles participate in exact context facts');
  Check(not NyxPascalCompilerPolicies(LCom).SamePolicies(NyxPascalCompilerPolicies(LRaw)),
    'COM and CORBA policy identities differ');
  Check(not NyxPascalCompilerPolicies(LUnknown).SamePolicies(NyxPascalCompilerPolicies(LCom)),
    'Unknown is a distinct policy identity, not an implicit COM value');
  Check(NyxPascalCompilerPolicies(LCom.UnknownState).InterfaceStyle = pifsUnknown,
    'Unknown effects revoke captured interface knowledge');
  Check(NyxPascalCompilerPolicies(LRaw.InMode(ppmObjFPC)).InterfaceStyle = pifsCORBA,
    'The inspected FPC mode handler retains interface style');
  LRestored := RestoreNyxPascalLocalSettings(LRaw, LCom);
  Check(NyxPascalCompilerPolicies(LRestored).InterfaceStyle = pifsCORBA,
    'Local POP retains the current module style, not the saved COM style');
  LRestored := NyxPascalWithCompilerPolicies(NewNyxPascalCompilerContext(ppcPas2JS,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi), LPolicy.UseInterfaces(pifsCOM));
  Check(NyxPascalCompilerPolicies(LRestored.InMode(ppmObjFPC)).InterfaceStyle = pifsUnknown,
    'An unqualified compiler family does not borrow FPC mode lifetime');
end;

procedure SourceLifetime;
var
  LSource, LPrefix: INyxPascalTranslation;
  LSettings: INyxPascalTranslationSettings;
  LAt, LState: Integer;
begin
  LSource := Translate('{$interfaces com} ComMarker {$push}{$I part.inc}' +
    ' AfterInclude {$pop} AfterPop {$mode objfpc} AfterMode {$interfaces default} AfterDefault',
    pifsCORBA, '{$interfaces corba} IncludedMarker');
  Check(LSource.Resolved and (LSource.DiagnosticCount = 0), 'Complete style directives translate without hidden effects');
  Check(StyleAt(LSource, 'ComMarker') = pifsCOM, 'Explicit COM is retained at original tokens');
  Check((StyleAt(LSource, 'IncludedMarker') = pifsCORBA) and
    (StyleAt(LSource, 'AfterInclude') = pifsCORBA), 'An included request changes its textual parent');
  Check(StyleAt(LSource, 'AfterPop') = pifsCORBA, 'POP does not restore the old interface type');
  Check(StyleAt(LSource, 'AfterMode') = pifsCORBA, 'MODE retains the inspected module setting');
  Check(StyleAt(LSource, 'AfterDefault') = pifsCORBA, 'DEFAULT restores the supplied initial value');
  Check(LSource.Token(Marker(LSource, 'IncludedMarker')).Occurrence <> 0,
    'The included interface setting keeps its physical occurrence');
  Supports(LSource, INyxPascalTranslationSettings, LSettings);
  LState := LSource.Token(Marker(LSource, 'IncludedMarker')).State;
  Check((LSettings.SettingsDepth(LState) = 1) and
    (NyxPascalCompilerPolicies(LSettings.SavedSettings(LState, 0)).InterfaceStyle = pifsCOM),
    'The historical saved frame still records the original COM value');
  LAt := Marker(LSource, 'AfterInclude');
  LPrefix := NewNyxPascalTranslationPrefix(LSource, LAt + 1, TNyxPascalTranslationOptions.Defaults);
  LSource := nil;
  Check(StyleAt(LPrefix, 'AfterInclude') = pifsCORBA, 'A bounded prefix owns earlier style history after source retirement');
  LSource := Translate('{$interfaces corba} BeforeDefault {$interfaces default} UnknownDefault');
  Check(StyleAt(LSource, 'UnknownDefault') = pifsUnknown, 'DEFAULT cannot manufacture an uncaptured initial value');
  LSource := Translate('{$if false}{$interfaces corba}{$endif} InactiveMarker', pifsCOM);
  Check(StyleAt(LSource, 'InactiveMarker') = pifsCOM, 'Inactive directives do not change an active style');
  LSource := Translate('{$if defined(MAYBE)}{$interfaces corba}{$endif} UncertainMarker', pifsCOM);
  Check(not LSource.Resolved and (StyleAt(LSource, 'UncertainMarker') = pifsUnknown),
    'Conditional uncertainty cannot select one interface contract');
  LSource := Translate('{$interfaces banana} InvalidMarker', pifsCOM);
  Check(not LSource.Resolved and (StyleAt(LSource, 'InvalidMarker') = pifsUnknown),
    'Malformed requests remain visible and revoke knowledge');
  LSource := Translate('{$madeup effect} UnsupportedMarker', pifsCOM);
  Check(not LSource.Resolved and (StyleAt(LSource, 'UnsupportedMarker') = pifsUnknown),
    'An unknown directive cannot preserve stale interface knowledge');
end;

function View(const AName, AText: TNyxText;
  AInitial: TNyxPascalInterfaceStyle = pifsUnknown): INyxPascalTranslatedSyntax;
begin
  Result := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile(AName), AText), NyxPascalPhysicalFile(AName), Facts(AInitial),
    TNyxPascalTranslationOptions.Defaults), TNyxPascalSyntaxOptions.Defaults);
end;

{ A caller-owned declaration model names the actual FPC COM root. It supplies
  no runtime ABI or measured target layout and no fabricated initial default. }
function EmptyWorld: INyxPascalSemanticWorld;
begin
  Result := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults)
    .WithModule(View('system.pas', 'unit System; interface type TObject=class end; ' +
    'IUnknown=interface function _AddRef:LongInt; function _Release:LongInt; end; ' +
    'IInterface=IUnknown; TInterfacedObject=class(TObject) end; implementation end.'));
end;

function Site(const AWorld: INyxPascalSemanticWorld): INyxPascalBindingSite;
begin
  Result := AWorld.At(AWorld.Count - 1, 0, AWorld.ModuleAt(AWorld.Count - 1).Syntax.Lexical.Scalars);
end;

function Bound(const AResult: INyxPascalResolution): INyxPascalSemanticType;
begin
  Check((AResult.Status = pbsBound) and (AResult.BoundType <> nil),
    'Expected a bound interface type: ' + String(AResult.MessageText));
  Result := AResult.BoundType;
end;

function NamedType(const AWorld: INyxPascalSemanticWorld; const AName: TNyxText): INyxPascalSemanticType;
var
  LFound: INyxPascalResolution;
begin
  LFound := AWorld.Lookup(Site(AWorld), NyxPascalBindingName(AName));
  Check((LFound.Status = pbsBound) and (LFound.Count = 1), 'Expected one original declaration: ' + String(AName));
  Result := Bound(AWorld.TypeOfSymbol(LFound.SymbolAt(0)));
end;

function TerminalType(const AType: INyxPascalSemanticType): INyxPascalSemanticType;
begin
  Result := AType;
  while (Result <> nil) and (Result.Kind in [pstAlias, pstDistinct]) do Result := Result.Target;
end;

procedure CheckMember(const AWorld: INyxPascalSemanticWorld; const AType: INyxPascalSemanticType;
  const AName: TNyxText; AStatus: TNyxPascalBindingStatus);
var
  LResult: INyxPascalResolution;
begin
  LResult := AWorld.Member(AType, Site(AWorld), NyxPascalBindingName(AName));
  Check(LResult.Status = AStatus, 'Member ' + String(AName) + ' status: ' + String(LResult.MessageText));

  if AStatus <> pbsBound then Check(LResult.Count = 0, 'A refused or absent member publishes no candidate');
end;

function InterfaceWorld: INyxPascalSemanticWorld;
begin
  Result := EmptyWorld.WithModule(View('contracts.pas',
    'unit Contracts; interface type IUnknownStyle=interface function ReadValue:LongInt; end; ' +
    '{$interfaces com} ICom=interface function ReadValue:LongInt; end; ' +
    'IAtKeyword={$interfaces corba} interface function ReadValue:LongInt; end; ' +
    'IRaw=interface function ReadValue:LongInt; end; ' +
    'IExplicit=interface(ICom) end; {$interfaces com} ' +
    'TBox<T:ICom>=class Item:T; end; TUnknownBox<T:IUnknownStyle>=class Item:T; end; ' +
    'TNeedsRoot<T:IUnknown>=class Item:T; end; var Yes:TNeedsRoot<ICom>; ' +
    'No:TNeedsRoot<IRaw>; implementation end.'));
end;

procedure ImplicitRoots;
var
  LWorld: INyxPascalSemanticWorld;
  LCom, LRaw, LUnknown, LFormal: INyxPascalSemanticType;
  LQueries: INyxPascalMemberQueries;
  LPage: INyxPascalMemberPage;
  LFound: INyxPascalResolution;
  LParameter: INyxPascalFormalParameter;
  LDecl, LAt: Integer;
begin
  LWorld := InterfaceWorld;
  LCom := NamedType(LWorld, 'ICom');
  LRaw := NamedType(LWorld, 'IRaw');
  LUnknown := NamedType(LWorld, 'IUnknownStyle');
  CheckMember(LWorld, LCom, '_AddRef', pbsBound);
  CheckMember(LWorld, LRaw, '_AddRef', pbsMissing);
  CheckMember(LWorld, LUnknown, '_AddRef', pbsUnresolved);
  CheckMember(LWorld, NamedType(LWorld, 'IAtKeyword'), '_AddRef', pbsMissing);
  CheckMember(LWorld, NamedType(LWorld, 'IExplicit'), '_AddRef', pbsBound);
  CheckMember(LWorld, LUnknown, 'ReadValue', pbsBound);
  LFound := LWorld.Member(LUnknown, Site(LWorld), NyxPascalBindingName('ReadValue'));
  Check(TerminalType(Bound(LWorld.TypeOfSymbol(LFound.SymbolAt(0)))).Builtin = pbbLongInt,
    'A name absent from every possible root can resolve in the outer scope without guessing COM');
  Supports(LWorld, INyxPascalMemberQueries, LQueries);
  LPage := LQueries.Members(LCom, Site(LWorld), TNyxPascalMemberQuery.Defaults.Matching('_'));
  Check((LPage.ResultSet.Status = pbsBound) and (LPage.ResultSet.Count = 2) and not LPage.HasMore,
    'Known COM pages include both exact System root members');
  LPage := LQueries.Members(LRaw, Site(LWorld), TNyxPascalMemberQuery.Defaults);
  Check((LPage.ResultSet.Status = pbsBound) and (LPage.ResultSet.Count = 1),
    'CORBA pages are complete without COM donation');
  LPage := LQueries.Members(LUnknown, Site(LWorld), TNyxPascalMemberQuery.Defaults);
  Check((LPage.ResultSet.Status = pbsUnresolved) and (LPage.ResultSet.Count = 0) and not LPage.HasMore,
    'An unknown implicit parent refuses the complete page atomically');
  LFound := LWorld.Lookup(Site(LWorld), NyxPascalBindingName('Yes'));
  Bound(LWorld.TypeOfSymbol(LFound.SymbolAt(0)));
  LFound := LWorld.Lookup(Site(LWorld), NyxPascalBindingName('No'));
  Check(LWorld.TypeOfSymbol(LFound.SymbolAt(0)).Status = pbsUnresolved,
    'A CORBA interface cannot satisfy a COM root constraint');
  LDecl := -1;
  for LAt := 0 to LWorld.ModuleAt(1).Syntax.DeclarationCount - 1 do
  begin

    if (LWorld.ModuleAt(1).Syntax.Declaration(LAt).Name = TNyxText('Item')) and
      (LWorld.ModuleAt(1).Syntax.Scope(LWorld.ModuleAt(1).Syntax.Declaration(LAt).Scope).Name = TNyxText('TBox')) then LDecl := LAt;
  end;
  Check(LDecl >= 0, 'The source-owned bounded formal field is present');
  LFormal := Bound(LWorld.TypeOfSymbol(LWorld.Symbol(1, LDecl)));
  Check(Supports(LFormal, INyxPascalFormalParameter, LParameter) and (LParameter.BoundCount = 1),
    'A formal retains its exact interface guarantee');
  CheckMember(LWorld, LFormal, '_Release', pbsBound);
  LPage := LQueries.Members(LFormal, Site(LWorld), TNyxPascalMemberQuery.Defaults.Matching('_'));
  Check((LPage.ResultSet.Status = pbsBound) and (LPage.ResultSet.Count = 2),
    'Formal member pages use the same captured COM root');
  LDecl := -1;
  for LAt := 0 to LWorld.ModuleAt(1).Syntax.DeclarationCount - 1 do
  begin

    if (LWorld.ModuleAt(1).Syntax.Declaration(LAt).Name = TNyxText('Item')) and
      (LWorld.ModuleAt(1).Syntax.Scope(LWorld.ModuleAt(1).Syntax.Declaration(LAt).Scope).Name = TNyxText('TUnknownBox')) then LDecl := LAt;
  end;
  Check(LDecl >= 0, 'The unknown-style formal has its own source owner');
  LFormal := Bound(LWorld.TypeOfSymbol(LWorld.Symbol(1, LDecl)));
  CheckMember(LWorld, LFormal, 'ReadValue', pbsBound);
  CheckMember(LWorld, LFormal, '_AddRef', pbsUnresolved);
  LPage := LQueries.Members(LFormal, Site(LWorld), TNyxPascalMemberQuery.Defaults);
  Check((LPage.ResultSet.Status = pbsUnresolved) and (LPage.ResultSet.Count = 0) and not LPage.HasMore,
    'Unknown-style formal pages retain atomic refusal without dropping the own-member guarantee');
end;

{ Exhaust every smaller allowance rather than choosing an arbitrary passing
  budget. No partial page may escape; returned symbols outlive budget retirement. }
procedure AtomicWork;
var
  LWorld, LChanged: INyxPascalSemanticWorld;
  LType: INyxPascalSemanticType;
  LQueries: INyxPascalMemberQueries;
  LAttempt, LAccepted: INyxPascalMemberPage;
  LBudget: INyxPascalAnalysisBudget;
  LCancel: INyxCancellationScope;
  LAt, LFirst: Integer;
  LRefused: Boolean;
begin
  LWorld := InterfaceWorld;
  LType := NamedType(LWorld, 'ICom');
  Supports(LWorld, INyxPascalMemberQueries, LQueries);
  LFirst := -1;
  for LAt := 1 to 2000 do
  begin
    LAttempt := nil;
    LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      try
        LAttempt := LQueries.Members(LType, Site(LWorld), TNyxPascalMemberQuery.Defaults, LBudget);
        LFirst := LAt;
        LAccepted := LAttempt;
      except
        on ENyxPascalAnalysisWorkLimit do Check((LAttempt = nil) and (LBudget.Receipt.Total <= LAt),
          'A smaller root-traversal budget publishes no partial page');
      end;
    finally
      LBudget.Retire;
    end;

    if LFirst >= 0 then Break;
  end;
  Check((LFirst > 1) and (LAccepted.ResultSet.Status = pbsBound) and (LAccepted.ResultSet.Count = 3),
    'Actual COM member traversal has a finite budgeted first fit');
  Check(TerminalType(Bound(LWorld.TypeOfSymbol(LAccepted.ResultSet.SymbolAt(0)))).Builtin = pbbLongInt,
    'Accepted member symbols remain usable after work retirement');
  WriteLn('interface work / first fit=', LFirst, ' every smaller allowance refused');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LAttempt := nil;
  LRefused := False;
  try
    LAttempt := LQueries.Members(LType, Site(LWorld), TNyxPascalMemberQuery.Defaults, LCancel);
  except
    on ENyxPascalAnalysisCancelled do LRefused := True;
  end;
  Check(LRefused and (LAttempt = nil), 'Cancellation refuses without a partial page');
  LChanged := LWorld.WithModule(View('contracts.pas', 'unit Contracts; interface type ICom=interface end; implementation end.'));
  LRefused := False;
  try
    LChanged.Member(LType, Site(LChanged), NyxPascalBindingName('_AddRef'));
  except
    on ENyxPascalBinding do LRefused := True;
  end;
  Check(LRefused, 'Source replacement rejects the old-world interface receiver');
  CheckMember(LWorld, LType, '_AddRef', pbsBound);
end;

{$ifndef PAS2JS}
procedure WholeNativeUnit;
var
  LText: TStringList;
  LWorld: INyxPascalSemanticWorld;
  LObserved: TNyxPascalInterfaceStyle;
begin
  Check(CompilerInterfaceStyleValue = 42, 'Independent native raw/COM calls and refcounts execute to 42');
  LObserved := pifsUnknown;
  case CompilerInitialInterfaceStyle of
    1: LObserved := pifsCOM;
    2: LObserved := pifsCORBA;
  end;
  Check(LObserved <> pifsUnknown, 'The native compiler supplies an observed initial style');
  LText := TStringList.Create;
  try
    LText.LoadFromFile('tests/interfaces.style.fixture.pas');
    LWorld := EmptyWorld.WithModule(View('interfaces.style.fixture.pas', TNyxText(LText.Text), LObserved));
  finally
    LText.Free;
  end;
  CheckMember(LWorld, NamedType(LWorld, 'IModeRetained'), '_AddRef', pbsMissing);
  CheckMember(LWorld, NamedType(LWorld, 'IWithinPush'), '_AddRef', pbsMissing);
  CheckMember(LWorld, NamedType(LWorld, 'IAfterPop'), '_AddRef', pbsMissing);
  CheckMember(LWorld, NamedType(LWorld, 'IComValue'), '_AddRef', pbsBound);

  if LObserved = pifsCOM then
  begin
    CheckMember(LWorld, NamedType(LWorld, 'IInitialDefault'), '_AddRef', pbsBound);
    CheckMember(LWorld, NamedType(LWorld, 'IRestoredDefault'), '_AddRef', pbsBound);
  end
  else
  begin
    CheckMember(LWorld, NamedType(LWorld, 'IInitialDefault'), '_AddRef', pbsMissing);
    CheckMember(LWorld, NamedType(LWorld, 'IRestoredDefault'), '_AddRef', pbsMissing);
  end;
  WriteLn('native interface oracle / initial=', Ord(LObserved), ' runtime=42');
end;
{$endif}

begin
  try
    PolicyValues;
    SourceLifetime;
    ImplicitRoots;
    AtomicWork;
    {$ifndef PAS2JS}WholeNativeUnit;{$endif}
    WriteLn('PASS ', GChecks, ' captured interface style checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
