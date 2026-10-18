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
  nyx.pascal.lexing, nyx.pascal.syntax;

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
  try
    Core;
    {$ifndef PAS2JS}Corpus;{$endif}
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
