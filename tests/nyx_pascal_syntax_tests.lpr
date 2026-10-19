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
program nyx_pascal_syntax_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Classes, nyx.text, nyx.editing, nyx.scheduler,
  nyx.pascal.lexing, nyx.pascal.syntax, nyx.bytes, nyx.pascal.context,
  nyx.pascal.files, nyx.pascal.translation, nyx.pascal.syntax.translated
  {$ifndef PAS2JS}, Process, module.body.fixture{$endif};

const
  CSource: TNyxText = 'unit Demo.Tools;' + #13#10 +
    '{$mode delphi}{$H+}{$codepage utf8}' + #10 +
    'interface uses SysUtils, Data.Types in ''data.pas'';' + #10 +
    'type TBase = class' + #10 +
    'strict private FSecret: Integer;' + #10 +
    'public function Render(const AText: String): String; virtual;' + #10 +
    'property Caption: String read FCaption write FCaption; end;' + #10 +
    'TWidget<T: class> = class(TBase)' + #10 +
    'private FItems: array of T;' + #10 +
    'public procedure Append(constref AItem: T); overload;' + #10 +
    'procedure Append(const AItem: T; ACount: Integer); overload;' + #10 +
    'property Items[Index: Integer]: T read GetItem; default; end;' + #10 +
    'TAlias = TWidget<TBase>;' + #10 +
    'TCallback = reference to function(const AValue: TAlias): TBase;' + #10 +
    'TChoice = (choiceFirst, choiceLast = 4);' + #10 +
    'TVector = array[0..2] of Integer; PWidget = ^TAlias;' + #10 +
    'TAccess = class(TBase);' + #10 +
    'var Global: TAlias;' + #10 +
    'const Greeting: String = ''English qualification 🚀'';' + #10 +
    'implementation' + #13#10 +
    'function TBase.Render(const AText: String): String;' + #10 +
    'var Local: TBase;' + #10 +
    'procedure Nested(var Local: TAlias);' + #10 +
    'var Deep: TWidget<TBase>;' + #10 +
    'begin Deep.Append(Local); end;' + #10 +
    'begin Result := AText; end;' + #10 +
    'end.';

var
  GChecks: Integer;
  GCorpusFiles: Integer;
  GCorpusDeclarations: Integer;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
end;

function Entry(const ASyntax: INyxPascalSyntaxSnapshot; AScope: Integer;
  const AName: TNyxText): Integer;
begin
  Result := ASyntax.Named(AScope, AName);

  if Result < 0 then
  begin
    raise Exception.Create('Missing declared syntax name ' + AName + ' in scope ' + IntToStr(AScope));
  end;
end;

function TypeText(const ASyntax: INyxPascalSyntaxSnapshot; AEntry: Integer): TNyxText;
begin
  Result := ASyntax.Lexical.Slice(ASyntax.Declaration(AEntry).TypeSpan);
end;

function HasIssue(const ASyntax: INyxPascalSyntaxSnapshot;
  AIssue: TNyxPascalSyntaxIssue): Boolean;
var
  LAt: Integer;
begin
  Result := False;
  for LAt := 0 to ASyntax.DiagnosticCount - 1 do
  begin

    if ASyntax.Diagnostic(LAt).Issue = AIssue then
    begin
      Exit(True);
    end;
  end;
end;

procedure ValidateSnapshot(const ASyntax: INyxPascalSyntaxSnapshot);
var
  LAt: Integer;
  LEntry: TNyxPascalSyntaxDeclaration;
  LScope: TNyxPascalSyntaxScope;
  LParent: Integer;
  LCount: Integer;
begin
  for LAt := 0 to ASyntax.ScopeCount - 1 do
  begin
    LScope := ASyntax.Scope(LAt);
    Check((LScope.Parent < LAt) and (LScope.Parent >= -1) and
      (LScope.Span.Finish <= ASyntax.Lexical.Scalars),
      'Scope containment is acyclic and physically bounded');
    LParent := LScope.FirstDeclaration;
    LCount := 0;
    while LParent >= 0 do
    begin
      LEntry := ASyntax.Declaration(LParent);
      Check((LEntry.Scope = LAt) and (LEntry.NameSpan.Finish <= ASyntax.Lexical.Scalars) and
        (LEntry.TypeSpan.Finish <= ASyntax.Lexical.Scalars) and
        (LEntry.HeaderSpan.Finish <= ASyntax.Lexical.Scalars),
        'Scoped declarations retain exact physical ranges');
      LParent := LEntry.Next;
      Inc(LCount);

      if LCount > ASyntax.DeclarationCount then
      begin
        raise Exception.Create('Declaration link cycle');
      end;
    end;
    Check(LCount = LScope.DeclarationCount, 'Bounded scope traversal retains every local row');
  end;
end;

procedure Core;
var
  LSyntax: INyxPascalSyntaxSnapshot;
  LOther: INyxPascalSyntaxSnapshot;
  LBase: Integer;
  LWidget: Integer;
  LMembers: Integer;
  LRender: Integer;
  LRoutine: Integer;
  LNested: Integer;
  LInner: Integer;
  LAt: Integer;
  LOffset: Integer;
  LDecl: TNyxPascalSyntaxDeclaration;
  LOptions: TNyxPascalSyntaxOptions;
  LRejected: Boolean;
  LCancel: INyxCancellationScope;
begin
  LSyntax := NewNyxPascalSyntaxSnapshot(CSource);
  ValidateSnapshot(LSyntax);
  for LAt := 0 to LSyntax.DiagnosticCount - 1 do
  begin
    WriteLn('syntax diagnostic / ', LSyntax.Diagnostic(LAt).Span.Start, ' / ',
      LSyntax.Diagnostic(LAt).MessageText);
  end;
  Check((LSyntax.DiagnosticCount = 0) and (LSyntax.Lexical.DiagnosticCount = 0),
    'The declared class/generic/property/nested-routine input needs no recovery');
  Check(LSyntax.Scope(0).Name = TNyxText('Demo.Tools'), 'The physical module namespace is retained');
  Check((LSyntax.ImportCount = 2) and (LSyntax.ImportAt(1).Name = TNyxText('Data.Types')),
    'Uses namespaces remain data without acquiring a filesystem path');
  LBase := Entry(LSyntax, 0, 'TBase');
  LMembers := LSyntax.Declaration(LBase).NestedScope;
  Check(LSyntax.Declaration(LBase).TypeForm = pstfClass, 'A class retains its surface construction');
  LDecl := LSyntax.Declaration(Entry(LSyntax, LMembers, 'FSecret'));
  Check((LDecl.Visibility = psavStrictPrivate) and (TypeText(LSyntax,
    Entry(LSyntax, LMembers, 'FSecret')) = TNyxText('Integer')), 'Field type and strict access remain explicit');
  LRender := Entry(LSyntax, LMembers, 'Render');
  LRoutine := LSyntax.Declaration(LRender).NestedScope;
  Check((TypeText(LSyntax, LRender) = TNyxText('String')) and
    (LSyntax.Declaration(Entry(LSyntax, LRoutine, 'AText')).ParameterMode = pspmConst),
    'Routine result and parameter mode retain exact type syntax');
  Check(TypeText(LSyntax, Entry(LSyntax, LMembers, 'Caption')) = TNyxText('String'),
    'Property accessors do not become part of the declared type');
  LWidget := Entry(LSyntax, 0, 'TWidget');
  LMembers := LSyntax.Declaration(LWidget).NestedScope;
  Check(LSyntax.Lexical.Slice(LSyntax.Declaration(LWidget).GenericSpan) = TNyxText('<T: class>'),
    'Generic constraints retain their exact source expression');
  Check(LSyntax.Lexical.Slice(LSyntax.Declaration(LWidget).AncestorSpan) = TNyxText('(TBase)'),
    'Declared ancestors remain syntax rather than assumed resolved types');
  Check(TypeText(LSyntax, Entry(LSyntax, LMembers, 'FItems')) = TNyxText('array of T'),
    'Array-valued fields retain their complete type expression');
  LAt := Entry(LSyntax, LMembers, 'Append');
  Check((LSyntax.Declaration(LAt).NextNamed >= 0) and
    (psdfOverload in LSyntax.Declaration(LAt).Flags), 'Overload declarations retain distinct linked signatures');
  Check(LSyntax.Declaration(Entry(LSyntax, LSyntax.Declaration(LAt).NestedScope,
    'AItem')).ParameterMode = pspmConstRef, 'Nestedvar-compatible passing syntax is not flattened to value mode');
  Check(TypeText(LSyntax, Entry(LSyntax, LMembers, 'Items')) = TNyxText('T'),
    'Indexed default property type remains separate from index/accessor syntax');
  Check((LSyntax.Declaration(Entry(LSyntax, 0, 'TAlias')).TypeForm = pstfNamed) and
    (TypeText(LSyntax, Entry(LSyntax, 0, 'TAlias')) = TNyxText('TWidget<TBase>')),
    'Alias/generic specialization syntax is not erased');
  Check(LSyntax.Declaration(Entry(LSyntax, 0, 'TCallback')).TypeForm = pstfRoutine,
    'Reference-to procedural type syntax remains classified');
  Check(LSyntax.Declaration(Entry(LSyntax, 0, 'PWidget')).TypeForm = pstfPointer,
    'Pointer construction remains distinct from a named alias');
  Check(LSyntax.Declaration(Entry(LSyntax, 0, 'choiceLast')).Kind = psdkEnumValue,
    'Enum entries remain independently discoverable');
  Check(LSyntax.Scope(LSyntax.Declaration(Entry(LSyntax, 0, 'TAccess')).NestedScope).DeclarationCount = 0,
    'An empty Delphi descendant does not swallow later declarations');
  LRender := Entry(LSyntax, 0, 'Render');
  LRoutine := LSyntax.Declaration(LRender).NestedScope;
  Check(LSyntax.Declaration(LRender).OwnerName = TNyxText('TBase'),
    'A qualified implementation keeps its short name and explicit owning type');
  Check(TypeText(LSyntax, Entry(LSyntax, LRoutine, 'Local')) = TNyxText('TBase'),
    'Outer local type syntax remains retained');
  LNested := Entry(LSyntax, LRoutine, 'Nested');
  LInner := LSyntax.Declaration(LNested).NestedScope;
  Check((LSyntax.Declaration(Entry(LSyntax, LInner, 'Local')).Kind = psdkParameter) and
    (TypeText(LSyntax, Entry(LSyntax, LInner, 'Local')) = TNyxText('TAlias')) and
    (TypeText(LSyntax, Entry(LSyntax, LRoutine, 'Local')) = TNyxText('TBase')),
    'Shadowed parameter/local declarations remain in separate scopes');
  LOffset := LSyntax.Declaration(Entry(LSyntax, LInner, 'Deep')).NameSpan.Start;
  Check(LSyntax.ScopeAt(LOffset) = LInner, 'Caret context reaches the innermost nested routine');
  Check(LSyntax.Named(0, 'Missing') = -1, 'Absent names are not invented');

  LOther := NewNyxPascalSyntaxSnapshot('unit Recovery; interface var Good: Integer;' +
    TNyxText(' const Broken = ''unfinished') + #10 + 'var Later: String;');
  Check((LOther.Named(0, 'Good') >= 0) and (LOther.Named(0, 'Later') >= 0) and
    (LOther.Lexical.DiagnosticCount = 1), 'Recovery preserves declarations after an unfinished quoted value');
  LOther := NewNyxPascalSyntaxSnapshot('unit Unfinished; interface type TOpen = class public FCount: Integer;');
  LAt := Entry(LOther, 0, 'TOpen');
  LMembers := LOther.Declaration(LAt).NestedScope;
  Check((psdfIncomplete in LOther.Declaration(LAt).Flags) and not LOther.Scope(LMembers).Complete and
    (LOther.ScopeAt(LOther.Lexical.Scalars) = LMembers),
    'An unfinished type retains its open scope at EOF without claiming completeness');
  LOther := NewNyxPascalSyntaxSnapshot('unit Conditional; interface type {$ifdef FEATURE}' +
    'TChoice = Integer; {$else} TChoice = String; {$endif} {$I+} {$I parts.inc} implementation end.');
  LAt := Entry(LOther, 0, 'TChoice');
  Check((psdfConditional in LOther.Declaration(LAt).Flags) and
    (LOther.Declaration(LAt).NextNamed >= 0) and
    HasIssue(LOther, psiConditionalContextRequired) and HasIssue(LOther, psiIncludeContextRequired),
    'Unresolved conditional branches and include context are explicit, never silently selected');
  LOptions := TNyxPascalSyntaxOptions.Defaults;
  LOther := NewNyxPascalSyntaxSnapshot('TThing = interface function Value: Integer; end;',
    LOptions.Fragment(psfTypes));
  LAt := Entry(LOther, 0, 'TThing');
  Check((LOther.DiagnosticCount = 0) and
    (TypeText(LOther, Entry(LOther, LOther.Declaration(LAt).NestedScope, 'Value')) = TNyxText('Integer')),
    'An explicit type-fragment context supports reusable include analysis');
  LOther := NewNyxPascalSyntaxSnapshot('procedure Click; FCount: Integer;',
    LOptions.Fragment(psfMembers));
  LAt := Entry(LOther, 0, 'Click');
  Check((LOther.DiagnosticCount = 0) and (LOther.Scope(0).Kind = psskMembers) and
    (LOther.Named(0, 'FCount') >= 0) and LOther.Scope(LOther.Declaration(LAt).NestedScope).Complete,
    'Explicit member fragments do not invent routine implementations');
  LOther := NewNyxPascalSyntaxSnapshot('procedure Click; var Count: Integer;',
    LOptions.Fragment(psfInterfaceDeclarations));
  Check((LOther.DiagnosticCount = 0) and (LOther.Named(0, 'Count') >= 0),
    'Interface include fragments retain prototypes separately from following variables');
  LOther := NewNyxPascalSyntaxSnapshot('unit MissingType; interface type TBroken = ; TGood = Integer;');
  Check((psdfIncomplete in LOther.Declaration(Entry(LOther, 0, 'TBroken')).Flags) and
    (TypeText(LOther, Entry(LOther, 0, 'TGood')) = TNyxText('Integer')) and
    HasIssue(LOther, psiExpectedName), 'A missing alias expression stays incomplete and preserves later types');
  LOther := NewNyxPascalSyntaxSnapshot('unit Lambda; interface implementation procedure Run; begin F := function: Integer begin Result := 1 end; end; end.');
  LAt := Entry(LOther, 0, 'Run');
  Check((psdfUnsupported in LOther.Declaration(LAt).Flags) and
    LOther.Scope(LOther.Declaration(LAt).NestedScope).UnsupportedSyntax,
    'Unmodeled anonymous scopes are explicit uncertainty rather than false outer-scope resolution');
  LOther := NewNyxPascalSyntaxSnapshot('unit InlineLocal; interface implementation procedure Run; begin var X: Integer; X := 1; end; end.');
  LAt := Entry(LOther, 0, 'Run');
  Check((psdfUnsupported in LOther.Declaration(LAt).Flags) and
    LOther.Scope(LOther.Declaration(LAt).NestedScope).UnsupportedSyntax,
    'Skipped inline declarations cannot grant outer-variable binding authority');
  Check(LOther.Scope(0).Complete, 'A closed unit retains its end-dot boundary independently of grammar coverage');
  LOther := NewNyxPascalSyntaxSnapshot('unit OpenRoutine; interface implementation procedure Work; var X: Integer; begin X := 1;');
  LAt := Entry(LOther, 0, 'Work');
  Check((psdfIncomplete in LOther.Declaration(LAt).Flags) and
    not LOther.Scope(LOther.Declaration(LAt).NestedScope).Complete,
    'An unfinished implementation cannot acquire a closed routine scope');
  Check(not LOther.Scope(0).Complete, 'An unfinished module cannot acquire a closed root boundary');

  LRejected := False;
  try
    NewNyxPascalSyntaxSnapshot('unit Quota; interface var A,B: Integer;', LOptions.UpToDeclarations(1));
  except
    on ENyxPascalSyntaxLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LSyntax.Lexical.Source = CSource), 'Declaration quota refuses without altering an older snapshot');
  LRejected := False;
  try
    NewNyxPascalSyntaxSnapshot('unit Quota; interface type T = class end;', LOptions.UpToScopes(1));
  except
    on ENyxPascalSyntaxLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Scope quota is independent of source recovery');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LRejected := False;
  try
    NewNyxPascalSyntaxSnapshot(CSource, LOptions, LCancel);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Cancellation publishes no syntax candidate');
end;

{ Query typed executable regions through the existing immutable scope surface.
  No body token is promoted to a declaration, and closed spans grant no runtime
  or full statement/expression meaning. }
procedure ModuleBodies;
const
  CBroken: array[0..9] of TNyxText = (
    'unit U; interface implementation initialization repeat X:=1; end.',
    'unit U; interface implementation initialization X:=F(1]; end.',
    'unit U; interface implementation initialization X:=F(1; finalization end.',
    'unit U; interface implementation initialization begin finalization end.',
    'unit U; interface implementation initialization initialization end.',
    'unit U; interface implementation finalization initialization end.',
    'unit U; interface implementation initialization finally X:=1; end.',
    'unit U; interface implementation initialization try X:=1; except finally end; end.',
    'unit U; interface implementation initialization var X:Integer; end.',
    'unit U; interface implementation initialization try X:=1; end; end.');
var
  LText: TNyxText;
  LSyntax: INyxPascalSyntaxSnapshot;
  LScope: TNyxPascalSyntaxScope;
  LAt: Integer;
  LInit: Integer;
  LFinal: Integer;
  LIndex: Integer;
  LFailed: Boolean;
  LPartial: INyxPascalSyntaxSnapshot;
  LCancel: INyxCancellationScope;
begin
  Check((Ord(psskBlock) = 3) and (Ord(psskInitialization) = 4) and (Ord(psskFinalization) = 5),
    'Appended module lifecycle roles preserve the original scope ordinals');
  LText := 'unit Phases; interface var Value:Integer; implementation ' +
    'initialization repeat Inc(Value); until Value=2; ' +
    'case Value of 2:begin try Value:=40; finally Inc(Value,2); end; end; end; ' +
    'finalization Value:=0; end.';
  LSyntax := NewNyxPascalSyntaxSnapshot(LText);
  Check((LSyntax.DiagnosticCount = 0) and LSyntax.Scope(0).Complete,
    'Both lifecycle regions retain closed module structure');
  Check((LSyntax.DeclarationCount = 1) and (LSyntax.Declaration(0).Name = TNyxText('Value')),
    'Executable references and assignments do not become module declarations');
  LInit := -1;
  LFinal := -1;
  for LAt := 1 to LSyntax.ScopeCount - 1 do
  begin
    LScope := LSyntax.Scope(LAt);

    if LScope.Kind = psskInitialization then LInit := LAt;

    if LScope.Kind = psskFinalization then LFinal := LAt;
    Check(LScope.Complete and not LScope.UnsupportedSyntax, 'Every valid executable delimiter scope is complete');
  end;
  Check((LInit >= 0) and (LFinal > LInit), 'Initialization and finalization remain distinct ordered scopes');
  Check((LSyntax.Scope(LInit).Parent = 0) and (LSyntax.Scope(LFinal).Parent = 0),
    'Both lifecycle regions belong to their module rather than a preceding routine');
  Check(Copy(LSyntax.Lexical.Slice(LSyntax.Scope(LInit).Span), 1, 14) = TNyxText('initialization'),
    'The initialization scope begins at its original keyword');
  Check((LSyntax.Scope(LInit).Span.Finish <= LSyntax.Scope(LFinal).Span.Start) and
    (LSyntax.ScopeAt(Pos(TNyxText('Value:=0'), LText) - 1) = LFinal),
    'Adjacent lifecycle scopes keep nonoverlapping ranges and exact containment');
  ValidateSnapshot(LSyntax);
  LSyntax := NewNyxPascalSyntaxSnapshot('unit OldStyle; interface implementation begin X:=42; end.');
  Check((LSyntax.DiagnosticCount = 0) and LSyntax.Scope(0).Complete and
    (LSyntax.Scope(1).Kind = psskInitialization), 'The old unit BEGIN spelling retains initialization ownership');
  LSyntax := NewNyxPascalSyntaxSnapshot('program Plain; begin X:=42; end.');
  Check((LSyntax.DiagnosticCount = 0) and (LSyntax.Scope(1).Kind = psskBlock),
    'An ordinary program body retains its existing block role');
  LSyntax := NewNyxPascalSyntaxSnapshot('unit Empty; interface implementation initialization finalization end.');
  Check((LSyntax.DiagnosticCount = 0) and (LSyntax.ScopeCount = 3),
    'Empty lifecycle regions preserve distinct source boundaries');
  LSyntax := NewNyxPascalSyntaxSnapshot('unit Names; interface var &finalization:Integer; ' +
    'implementation initialization &finalization:=1; WriteLn(''initialization end finally''); ' +
    'asm nop end; end.');
  Check((LSyntax.DiagnosticCount = 0) and (LSyntax.DeclarationCount = 1) and
    LSyntax.Scope(0).Complete, 'Escaped names, quoted text and assembly do not impersonate lifecycle boundaries');
  for LIndex := 0 to High(CBroken) do
  begin
    LSyntax := NewNyxPascalSyntaxSnapshot(CBroken[LIndex]);
    Check(LSyntax.DiagnosticCount > 0, 'Malformed lifecycle syntax retains explicit diagnostic '+IntToStr(LIndex));
    Check(LSyntax.Named(0, 'X') < 0, 'Malformed executable text cannot fabricate a module variable');
  end;
  LSyntax := NewNyxPascalSyntaxSnapshot('program Wrong; initialization X:=1; end.');
  Check((LSyntax.DiagnosticCount > 0) and (LSyntax.ScopeCount = 1),
    'A program header grants no unit lifecycle authority');
  LSyntax := NewNyxPascalSyntaxSnapshot('unit Missing; interface initialization X:=1; end.');
  Check((LSyntax.DiagnosticCount > 0) and (LSyntax.ScopeCount = 1),
    'A lifecycle region requires the actual implementation boundary');
  LSyntax := NewNyxPascalSyntaxSnapshot('initialization X:=1; end.',
    TNyxPascalSyntaxOptions.Defaults.Fragment(psfDeclarations));
  Check((LSyntax.DiagnosticCount > 0) and (LSyntax.ScopeCount = 1),
    'A declaration fragment grants no inferred unit context');
  LSyntax := NewNyxPascalSyntaxSnapshot('unit U; interface implementation initialization repeat until; end.');
  Check(LSyntax.DiagnosticCount > 0, 'An empty UNTIL condition remains a real syntax gap');
  LPartial := nil;
  LFailed := False;
  try
    LPartial := NewNyxPascalSyntaxSnapshot(LText, TNyxPascalSyntaxOptions.Defaults.UpToScopes(2));
  except
    on ENyxPascalSyntaxLimit do LFailed := True;
  end;
  Check(LFailed and (LPartial = nil), 'Lifecycle scopes share the existing atomic scope quota');
  LFailed := False;
  try
    LPartial := NewNyxPascalSyntaxSnapshot(LText, TNyxPascalSyntaxOptions.Defaults.UpToDepth(1));
  except
    on ENyxPascalSyntaxLimit do LFailed := True;
  end;
  Check(LFailed and (LPartial = nil), 'Nested lifecycle constructs share the existing depth quota');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LFailed := False;
  try
    LPartial := NewNyxPascalSyntaxSnapshot(LText, TNyxPascalSyntaxOptions.Defaults, LCancel);
  except
    on ENyxPascalAnalysisCancelled do LFailed := True;
  end;
  Check(LFailed and (LPartial = nil), 'Cancelled lifecycle analysis publishes no partial snapshot');
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

{ Observe the same whole compiler-admitted unit/include, including exact physical
  scope origins. A fresh child supplies actual finalization evidence after its
  ordinary program body exits; it does not touch a host, UI or project file. }
procedure NativeModuleBodies;
var
  LFiles: INyxPascalFileCatalog;
  LBytes: TNyxBytes;
  LStream: TFileStream;
  LName: TNyxText;
  LView: INyxPascalTranslatedSyntax;
  LOrigin: TNyxPascalSyntaxOrigin;
  LAt: Integer;
  LPhases: Integer;
  LOutput: String;
begin
  Check(InitializedPhaseValue = 42, 'The independently compiled unit initializes before the consumer');
  Check(RunCommand(ParamStr(0), ['--module-phase-child'], LOutput, [poStderrToOutPut]),
    'The lifecycle observation child exits normally');
  Check((Pos('NYX_PHASE_INIT=42', LOutput) > 0) and
    (Pos('NYX_PHASE_FINAL=42', LOutput) > Pos('NYX_PHASE_INIT=42', LOutput)) and
    (Pos('0 unfreed memory blocks : 0', LOutput) > 0),
    'The child observes actual initialization before finalization with a clean native heap');

  if ParamCount = 0 then
  begin
    Exit;
  end;
  LFiles := NewNyxPascalFileCatalog(pfcExact);
  for LAt := 0 to 1 do
  begin
    LName := 'module.body.fixture.pas';

    if LAt = 1 then LName := 'module.body.fixture.inc';
    LStream := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(1)) + 'tests/' + String(LName),
      fmOpenRead or fmShareDenyWrite);
    try
      Check((LStream.Size > 0) and (LStream.Size < 65536), 'The complete lifecycle source packet is bounded');
      SetLength(LBytes, LStream.Size);
      LStream.ReadBuffer(LBytes[0], Length(LBytes));
    finally
      LStream.Free;
    end;
    LFiles := LFiles.WithFile(NyxPascalPhysicalFile(LName), NyxDecodeUTF8(LBytes));
  end;
  LView := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFiles,
    NyxPascalPhysicalFile('module.body.fixture.pas'), NewNyxPascalCompilerContext(ppcFPC,
      TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi).CompleteDefines(True),
    TNyxPascalTranslationOptions.Defaults), TNyxPascalSyntaxOptions.Defaults);
  Check(LView.CompleteTranslation and (LView.Syntax.DiagnosticCount = 0) and LView.Syntax.Scope(0).Complete,
    'The whole compiled unit/include retain resolved translation and complete scope boundaries');
  LPhases := 0;
  for LAt := 1 to LView.Syntax.ScopeCount - 1 do
  begin

    if LView.Syntax.Scope(LAt).Kind in [psskInitialization, psskFinalization] then
    begin
      LOrigin := LView.Origin(LView.OriginAt(LView.Syntax.Scope(LAt).Span.Start));
      Check(LView.Source.Files.FileAt(LView.Source.Occurrence(LOrigin.Occurrence).FileIndex).Name =
        TNyxText('module.body.fixture.inc'), 'Lifecycle navigation retains the original physical include');
      Inc(LPhases);
    end;
  end;
  Check(LPhases = 2, 'The original compiled include retains exactly two lifecycle regions');
end;

procedure Corpus;
const
  CFiles: array[0..4] of String = ('nyx.controls.pas', 'nyx.controls.types.inc',
    'nyx.controls.facades.inc', 'nyx.scheduler.pas', 'nyx.pascal.lexing.pas');
var
  LFile: String;
  LSyntax: INyxPascalSyntaxSnapshot;
  LOptions: TNyxPascalSyntaxOptions;
  LAt: Integer;
  LBadge: Integer;
  LMembers: Integer;
begin

  if ParamCount = 0 then
  begin
    Exit;
  end;
  for LFile in CFiles do
  begin
    LOptions := TNyxPascalSyntaxOptions.Defaults;

    if ExtractFileExt(LFile) = '.inc' then
    begin
      LOptions := LOptions.Fragment(psfTypes);
    end;
    LSyntax := NewNyxPascalSyntaxSnapshot(
      ReadText(IncludeTrailingPathDelimiter(ParamStr(1)) + 'src/' + LFile), LOptions);
    ValidateSnapshot(LSyntax);
    Check(LSyntax.DeclarationCount > 0, 'Actual owned Pascal declarations are retained');
    WriteLn('corpus / ', LFile, ' / ', LSyntax.DeclarationCount, ' declarations / ',
      LSyntax.DiagnosticCount, ' explicit syntax/context diagnostics');
    Inc(GCorpusFiles);
    Inc(GCorpusDeclarations, LSyntax.DeclarationCount);

    if LFile = 'nyx.controls.types.inc' then
    begin
      LBadge := Entry(LSyntax, 0, 'INyxBadge');
      while (LBadge >= 0) and (LSyntax.Declaration(LBadge).NestedScope < 0) do
      begin
        LBadge := LSyntax.Declaration(LBadge).NextNamed;
      end;
      Check(LBadge >= 0, 'The actual specialized badge interface has a full declared scope');
      LMembers := LSyntax.Declaration(LBadge).NestedScope;
      LAt := Entry(LSyntax, LMembers, 'WithText');
      Check(TypeText(LSyntax, LAt) = TNyxText('INyxBadge'),
        'The real badge fluent method preserves its specialized interface result');
      LMembers := LSyntax.Declaration(LAt).NestedScope;
      Check(TypeText(LSyntax, Entry(LSyntax, LMembers, 'AText')) = TNyxText('TNyxText'),
        'The real method parameter retains the portable text contract');
    end;
  end;
end;
{$endif}

begin
  {$ifndef PAS2JS}
  if (ParamCount = 1) and (ParamStr(1) = '--module-phase-child') then
  begin
    WriteLn('NYX_PHASE_INIT=', InitializedPhaseValue);
    TracePhases := True;
  end
  else
  {$endif}
  try
    Core;
    ModuleBodies;
    {$ifndef PAS2JS}NativeModuleBodies; Corpus;{$endif}
    WriteLn('PASS ', GChecks, ' owned syntax/scope checks; ', GCorpusFiles,
      ' actual inputs / ', GCorpusDeclarations, ' declarations');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL / ', LException.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
