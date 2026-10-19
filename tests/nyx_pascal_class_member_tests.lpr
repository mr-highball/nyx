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
program nyx_pascal_class_member_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Classes, nyx.text, nyx.bytes, nyx.editing, nyx.scheduler,
  nyx.pascal.lexing, nyx.pascal.context, nyx.pascal.files, nyx.pascal.translation,
  nyx.pascal.syntax, nyx.pascal.syntax.translated, nyx.pascal.types,
  nyx.pascal.binding, nyx.pascal.names, nyx.pascal.project, nyx.pascal.drafts,
  nyx.pascal.sources, nyx.pascal.completion, nyx.pascal.completion.owned
  {$ifndef PAS2JS}, member.classes.fixture, member.generics.fixture,
  member.threadvars.fixture{$endif};

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

function View(const AText: TNyxText; const AFile: TNyxText = 'case.pas'): INyxPascalTranslatedSyntax;
begin
  Result := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(
    NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile(AFile), AText),
    NyxPascalPhysicalFile(AFile), Facts, TNyxPascalTranslationOptions.Defaults),
    TNyxPascalSyntaxOptions.Defaults);
end;

procedure RolesAndBinding;
const
  CText: TNyxText = 'unit Members; interface type TState=record Count:LongInt; ' +
    'class function GetEmpty:TState; static; class property Empty:TState read GetEmpty; end; ' +
    'TSettings=class public class var First,Second:LongInt; Third:LongInt; ' +
    'class property State:TState read GetState; ' +
    'public InstanceField:LongInt; property InstanceOnly:LongInt read InstanceField; ' +
    'generic class function Echo<T>(const Value:T):T; static; ' +
    'generic class procedure CopyValue<T>(const Value:T; out Other:T); static; ' +
    'end; implementation end.';
var
  LView: INyxPascalTranslatedSyntax;
  LSyntax: INyxPascalSyntaxSnapshot;
  LWorld: INyxPascalSemanticWorld;
  LType: INyxPascalSemanticType;
  LSite: INyxPascalBindingSite;
  LAnswer: INyxPascalResolution;
  LClass: Integer;
  LScope: Integer;
  LAt: Integer;
  LNames: array[0..4] of TNyxText;
  LNode: TNyxPascalSyntaxDeclaration;
  LCaller: INyxPascalTranslatedSyntax;
  LImportedWorld: INyxPascalSemanticWorld;
begin
  LView := View(CText);
  LSyntax := LView.Syntax;
  Check(LView.CompleteTranslation and (LSyntax.DiagnosticCount = 0),
    'The class-member family retains complete declared syntax');
  LClass := LSyntax.Named(0, 'TSettings');
  LScope := LSyntax.Declaration(LClass).NestedScope;
  LNames[0] := 'First';
  LNames[1] := 'Second';
  LNames[2] := 'Third';
  LNames[3] := 'Echo';
  LNames[4] := 'CopyValue';
  for LAt := 0 to 2 do
  begin
    LNode := LSyntax.Declaration(LSyntax.Named(LScope, LNames[LAt]));
    Check((LNode.Kind = psdkField) and (psdfClassMember in LNode.Flags),
      'Every grouped and continued class-var declaration retains class storage intent');
  end;
  LNode := LSyntax.Declaration(LSyntax.Named(LScope, 'State'));
  Check((LNode.Kind = psdkProperty) and (psdfClassMember in LNode.Flags),
    'A class property keeps its ordinary property role and explicit class modifier');
  LNode := LSyntax.Declaration(LSyntax.Named(LScope, 'InstanceField'));
  Check((LNode.Kind = psdkField) and not (psdfClassMember in LNode.Flags),
    'A property terminates the class-var section before later instance fields');
  LNode := LSyntax.Declaration(LSyntax.Named(LScope, 'InstanceOnly'));
  Check(not (psdfClassMember in LNode.Flags), 'Class property intent cannot leak into an instance property');
  for LAt := 3 to 4 do
  begin
    LNode := LSyntax.Declaration(LSyntax.Named(LScope, LNames[LAt]));
    Check((LNode.Kind = psdkRoutine) and (psdfClassMember in LNode.Flags) and
      (LNode.GenericSpan.Length > 0) and (LNode.NestedScope >= 0),
      'Generic-class functions and procedures retain their own generic signature and parameter scope');
  end;
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LView);
  LSite := LWorld.At(0, 0, LSyntax.Lexical.Scalars);
  LAnswer := LWorld.TypeOfSymbol(LWorld.Symbol(0, LClass));
  Check(LAnswer.Status = pbsBound, 'The declared owner still binds through the ordinary type path');
  LType := LAnswer.BoundType;
  LAnswer := LWorld.Member(LType, LSite, NyxPascalBindingName('State'), pmaType);
  Check((LAnswer.Status = pbsBound) and (LAnswer.SymbolAt(0).Declaration.Kind = psdkProperty),
    'Type-member lookup admits the class property');
  LAnswer := LWorld.TypeOfSymbol(LAnswer.SymbolAt(0));
  Check((LAnswer.Issue = pbiMissingModule) and (LAnswer.RelatedName = TNyxText('System')),
    'An uncaptured implicit ancestor cannot prove the outer property type is unshadowed');
  { This explicit small base is a controlled scope oracle. The separately run
    unchanged RTL consumer owns actual SDK meaning and does not use this stub. }
  LWorld := LWorld.WithModule(View('unit System; interface type TObject=class end; implementation end.', 'system.pas'));
  LSite := LWorld.At(0, 0, LSyntax.Lexical.Scalars);
  LAnswer := LWorld.TypeOfSymbol(LWorld.Symbol(0, LClass));
  LType := LAnswer.BoundType;
  LAnswer := LWorld.Member(LType, LSite, NyxPascalBindingName('State'), pmaType);
  LAnswer := LWorld.TypeOfSymbol(LAnswer.SymbolAt(0));
  Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType.Kind = pstDeclared),
    'The class property returns its declared record type');
  LAnswer := LWorld.Member(LAnswer.BoundType, LSite, NyxPascalBindingName('Count'));
  Check((LAnswer.Status = pbsBound) and (LAnswer.SymbolAt(0).Declaration.Name = TNyxText('Count')),
    'Member lookup continues through the class property result');
  LAnswer := LWorld.Member(LType, LSite, NyxPascalBindingName('InstanceOnly'), pmaType);
  Check(LAnswer.Issue = pbiWrongMemberKind, 'Type access cannot donate an instance property');
  LAnswer := LWorld.Member(LType, LSite, NyxPascalBindingName('Third'), pmaType);
  Check(LAnswer.Status = pbsBound, 'A continued class-var declaration remains accessible through its type');
  LAnswer := LWorld.Member(LType, LSite, NyxPascalBindingName('Echo'), pmaType);
  Check((LAnswer.Status = pbsBound) and (LAnswer.SymbolAt(0).Declaration.GenericSpan.Length > 0),
    'Type-member lookup preserves the generic class method declaration');
  Check(LWorld.TypeOfSymbol(LAnswer.SymbolAt(0)).Issue = pbiGenericContext,
    'An unspecialized method cannot borrow a guessed generic argument');
  LAnswer := LWorld.TypeOfSymbol(LWorld.Symbol(0, LSyntax.Named(0, 'TState')));
  LAnswer := LWorld.Member(LAnswer.BoundType, LSite, NyxPascalBindingName('Empty'), pmaType);
  Check((LAnswer.Status = pbsBound) and (psdfClassMember in LAnswer.SymbolAt(0).Declaration.Flags),
    'Record-owned class properties use the same ordinary typed member boundary');
  LCaller := View('unit Caller; interface uses Members; implementation end.', 'caller.pas');
  LImportedWorld := LWorld.WithModule(LCaller);
  LAnswer := LImportedWorld.Lookup(LImportedWorld.At(2, 0, LCaller.Syntax.Lexical.Scalars),
    NyxPascalBindingName('TSettings.State.Count'));
  Check((LAnswer.Status = pbsBound) and (LAnswer.SymbolAt(0).Declaration.Name = TNyxText('Count')),
    'Dotted receivers resolve an imported type and its class-property result');
  LCaller := View('unit Caller; interface uses Members, Missing; implementation end.', 'caller.pas');
  LImportedWorld := LImportedWorld.WithModule(LCaller);
  LAnswer := LImportedWorld.Lookup(LImportedWorld.At(2, 0, LCaller.Syntax.Lexical.Scalars),
    NyxPascalBindingName('TSettings.State'));
  Check((LAnswer.Issue = pbiMissingModule) and (LAnswer.RelatedName = TNyxText('Missing')),
    'An uncaptured later import cannot be skipped to donate an earlier receiver');
  LAnswer := LImportedWorld.Lookup(LImportedWorld.At(2, 0, LCaller.Syntax.Lexical.Scalars),
    NyxPascalBindingName('Members.TSettings.State'));
  Check((LAnswer.Status = pbsBound) and (LAnswer.SymbolAt(0).Declaration.Kind = psdkProperty),
    'An explicitly visible captured unit namespace retains its established authority');
  LCaller := View('unit Caller; interface uses Members; implementation procedure Read; ' +
    'var TSettings:TState; begin end;end.', 'caller.pas');
  LImportedWorld := LImportedWorld.WithModule(LCaller);
  LScope := LCaller.Syntax.Declaration(LCaller.Syntax.Named(0, 'Read')).NestedScope;
  LAnswer := LImportedWorld.Lookup(LImportedWorld.At(2, LScope, LCaller.Syntax.Scope(LScope).Span.Finish - 1),
    NyxPascalBindingName('TSettings.Count'));
  Check((LAnswer.Status = pbsBound) and (LAnswer.SymbolAt(0).Declaration.Kind = psdkField),
    'A local receiver still shadows the imported class before qualified fallback');
end;

procedure ThreadStorageRoles;
const
  CText: TNyxText = 'unit Storage; interface threadvar GlobalFirst,GlobalSecond:LongInt; ' +
    'GlobalThird:LongInt; var Ordinary:LongInt; type TStore=class public ' +
    'class threadvar First,Second:LongInt; Third:LongInt; class var Shared:LongInt; ' +
    'class threadvar Other:LongInt; const Offset=2; public InstanceField:LongInt; ' +
    'class procedure Touch; static; end; implementation end.';
var
  LView: INyxPascalTranslatedSyntax;
  LSyntax: INyxPascalSyntaxSnapshot;
  LWorld: INyxPascalSemanticWorld;
  LSite: INyxPascalBindingSite;
  LType: INyxPascalSemanticType;
  LAnswer: INyxPascalResolution;
  LNode: TNyxPascalSyntaxDeclaration;
  LNames: array[0..2] of TNyxText;
  LAt, LScope, LClass: Integer;
begin
  LView := View(CText);
  LSyntax := LView.Syntax;
  Check(LSyntax.DiagnosticCount = 0, 'Global and class threadvar sections retain complete declaration grammar');
  LNames[0] := 'GlobalFirst'; LNames[1] := 'GlobalSecond'; LNames[2] := 'GlobalThird';
  for LAt := 0 to 2 do
  begin
    LNode := LSyntax.Declaration(LSyntax.Named(0, LNames[LAt]));
    Check((LNode.Kind = psdkVariable) and (psdfThreadLocal in LNode.Flags) and
      not (psdfClassMember in LNode.Flags), 'Grouped/continued global threadvars retain thread-local storage without a class owner');
  end;
  LNode := LSyntax.Declaration(LSyntax.Named(0, 'Ordinary'));
  Check(not (psdfThreadLocal in LNode.Flags), 'An ordinary var section ends global thread-local storage intent');
  LClass := LSyntax.Named(0, 'TStore');
  LScope := LSyntax.Declaration(LClass).NestedScope;
  LNames[0] := 'First'; LNames[1] := 'Second'; LNames[2] := 'Third';
  for LAt := 0 to 2 do
  begin
    LNode := LSyntax.Declaration(LSyntax.Named(LScope, LNames[LAt]));
    Check((LNode.Kind = psdkField) and (LNode.Flags * [psdfClassMember, psdfThreadLocal] =
      [psdfClassMember, psdfThreadLocal]), 'Every grouped/continued class threadvar retains both independent flags');
  end;
  LNode := LSyntax.Declaration(LSyntax.Named(LScope, 'Shared'));
  Check((psdfClassMember in LNode.Flags) and not (psdfThreadLocal in LNode.Flags),
    'A following class var retains shared class storage without thread-local leakage');
  LNode := LSyntax.Declaration(LSyntax.Named(LScope, 'Offset'));
  Check((LNode.Kind = psdkConstant) and not (psdfThreadLocal in LNode.Flags),
    'A following constant remains owned by the intact class scope');
  LNode := LSyntax.Declaration(LSyntax.Named(LScope, 'InstanceField'));
  Check((LNode.Flags * [psdfClassMember, psdfThreadLocal]) = [],
    'Visibility restores ordinary instance-member intent');
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LView);
  LSite := LWorld.At(0, 0, LSyntax.Lexical.Scalars);
  LAnswer := LWorld.TypeOfSymbol(LWorld.Symbol(0, LClass));
  LType := LAnswer.BoundType;
  LAnswer := LWorld.Member(LType, LSite, NyxPascalBindingName('Third'), pmaType);
  Check((LAnswer.Status = pbsBound) and (psdfThreadLocal in LAnswer.SymbolAt(0).Declaration.Flags),
    'Type-member binding preserves continued thread-local class fields');
  LAnswer := LWorld.Member(LType, LSite, NyxPascalBindingName('InstanceField'), pmaType);
  Check(LAnswer.Issue = pbiWrongMemberKind, 'Thread-local sections cannot authorize later instance access through the type');
  LSyntax := NewNyxPascalSyntaxSnapshot('unit Broken; interface class threadvar Bad:LongInt; implementation end.');
  Check((LSyntax.DiagnosticCount > 0) and (LSyntax.Named(0, 'Bad') < 0),
    'A unit-level class threadvar cannot fabricate a member scope');
  LSyntax := NewNyxPascalSyntaxSnapshot('class threadvar First:LongInt; Next:LongInt; class var Shared:LongInt;',
    TNyxPascalSyntaxOptions.Defaults.Fragment(psfMembers));
  LNode := LSyntax.Declaration(LSyntax.Named(0, 'Next'));
  Check((LSyntax.DiagnosticCount = 0) and
    (LNode.Flags * [psdfClassMember, psdfThreadLocal] = [psdfClassMember, psdfThreadLocal]),
    'Explicit member fragments retain continued class thread-local section state');
end;

procedure RecoveryAndLimits;
var
  LSyntax: INyxPascalSyntaxSnapshot;
  LScope: Integer;
  LNode: TNyxPascalSyntaxDeclaration;
  LCancel: INyxCancellationScope;
  LRefused: Boolean;
begin
  LSyntax := NewNyxPascalSyntaxSnapshot('unit Access; interface type TSettings=class ' +
    'public class var Shared:LongInt; private InstanceField:LongInt; ' +
    'public class property Value:LongInt read Shared; end; implementation end.');
  LScope := LSyntax.Declaration(LSyntax.Named(0, 'TSettings')).NestedScope;
  LNode := LSyntax.Declaration(LSyntax.Named(LScope, 'InstanceField'));
  Check((LSyntax.DiagnosticCount = 0) and (LNode.Visibility = psavPrivate) and
    not (psdfClassMember in LNode.Flags), 'Visibility starts a new instance-member section');
  LSyntax := NewNyxPascalSyntaxSnapshot('unit Bad; interface class property Value:LongInt read Missing; implementation end.');
  Check((LSyntax.DiagnosticCount > 0) and (LSyntax.Named(0, 'Value') < 0),
    'A unit-level class property cannot fabricate a member owner');
  LSyntax := NewNyxPascalSyntaxSnapshot('unit Bad; interface type TSettings=class ' +
    'class generic function Wrong<T>:T; class property ; end; implementation end.');
  LScope := LSyntax.Declaration(LSyntax.Named(0, 'TSettings')).NestedScope;
  Check((LSyntax.DiagnosticCount > 0) and (LSyntax.Named(LScope, 'Wrong') < 0),
    'Reversed generic/class and missing property names retain explicit diagnostics');
  LSyntax := nil;
  LRefused := False;
  try
    LSyntax := NewNyxPascalSyntaxSnapshot('unit Limited; interface type TSettings=class ' +
      'class property Value:LongInt read Missing; end; implementation end.',
      TNyxPascalSyntaxOptions.Defaults.UpToDeclarations(1));
  except
    on ENyxPascalSyntaxLimit do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LSyntax = nil), 'Class properties obey the existing node quota without partial publication');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LRefused := False;
  try
    LSyntax := NewNyxPascalSyntaxSnapshot('unit Cancelled; interface type TSettings=class ' +
      'class property Value:LongInt read Missing; end; implementation end.',
      TNyxPascalSyntaxOptions.Defaults, LCancel);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LSyntax = nil), 'Cancellation admits no partial class-member index');
end;

{$ifndef PAS2JS}
function ReadSource(const APath: TNyxText): TNyxText;
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LStream := TFileStream.Create(String(APath), fmOpenRead or fmShareDenyWrite);
  try
    Check((LStream.Size > 0) and (LStream.Size < 1024 * 1024), 'The compiled companion has a bounded original byte packet');
    SetLength(LBytes, LStream.Size);
    LStream.ReadBuffer(LBytes[0], Length(LBytes));
  finally
    LStream.Free;
  end;
  Result := NyxDecodeUTF8(LBytes);
end;

procedure WholeCompiledSource(const ARoot: TNyxText);
var
  LFiles: INyxPascalFileCatalog;
  LView: INyxPascalTranslatedSyntax;
  LGenericView: INyxPascalTranslatedSyntax;
  LWorld: INyxPascalSemanticWorld;
  LOwner: Integer;
  LScope: Integer;
  LAt: Integer;
  LProvider: INyxPascalCompletionProvider;
  LSources: INyxPascalSourceSnapshot;
  LReply: INyxPascalCompletionResult;
  LMetadata: INyxOwnedPascalCompletionResult;
  LValueReply: INyxPascalCompletionResult;
  LValueMetadata: INyxOwnedPascalCompletionResult;
  LValueSources: INyxPascalSourceSnapshot;
  LPreview: INyxPascalSourceSnapshot;
  LNewCaret: Integer;
  LRoots: array of TNyxPascalPhysicalFile;
  LPrefix: TNyxText;
  LText: TNyxText;
  LInclude: TNyxText;
  LGeneric: TNyxText;
  LThreadView: INyxPascalTranslatedSyntax;
  LThreadText, LThreadInclude: TNyxText;
  LNode: TNyxPascalSyntaxDeclaration;
  LThreadAnswer: INyxPascalResolution;
begin
  Check((ClassMemberValue = 42) and (GenericMemberValue = 42),
    'The actual compiler executes class properties/storage and both generic-class method specializations');
  Check(ThreadStorageValue = 42,
    'Actual native threadvars start independently, preserve the caller and share ordinary class storage after join');
  LThreadText := ReadSource(ARoot + '/tests/member.threadvars.fixture.pas');
  LThreadInclude := ReadSource(ARoot + '/tests/member.threadvars.fixture.inc');
  LThreadView := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(
    NewNyxPascalFileCatalog(pfcExact)
      .WithFile(NyxPascalPhysicalFile('member.threadvars.fixture.pas'), LThreadText)
      .WithFile(NyxPascalPhysicalFile('member.threadvars.fixture.inc'), LThreadInclude),
    NyxPascalPhysicalFile('member.threadvars.fixture.pas'), Facts,
    TNyxPascalTranslationOptions.Defaults), TNyxPascalSyntaxOptions.Defaults);
  Check(LThreadView.CompleteTranslation and (LThreadView.Syntax.DiagnosticCount = 0),
    'The same whole compiled unit/include retains complete thread-local declaration syntax');
  LOwner := LThreadView.Syntax.Named(0, 'TThreadLocalSettings');
  LScope := LThreadView.Syntax.Declaration(LOwner).NestedScope;
  LNode := LThreadView.Syntax.Declaration(LThreadView.Syntax.Named(LScope, 'LocalThird'));
  Check((LNode.Flags * [psdfClassMember, psdfThreadLocal]) = [psdfClassMember, psdfThreadLocal],
    'The whole compiled continued field preserves both class and thread storage');
  LAt := LThreadView.OriginAt(LNode.NameSpan.Start);
  Check((LAt >= 0) and (LThreadView.Source.Files.FileAt(LThreadView.Origin(LAt).FileIndex).Name =
    'member.threadvars.fixture.inc'), 'Thread-local field navigation keeps its original include origin');
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LThreadView);
  LThreadAnswer := LWorld.TypeOfSymbol(LWorld.Symbol(0, LOwner));
  LThreadAnswer := LWorld.Member(LThreadAnswer.BoundType,
    LWorld.At(0, 0, LThreadView.Syntax.Lexical.Scalars), NyxPascalBindingName('LocalThird'), pmaType);
  Check((LThreadAnswer.Status = pbsBound) and
    (psdfThreadLocal in LThreadAnswer.SymbolAt(0).Declaration.Flags),
    'Actual compiled continued threadvars bind through their declaring type with the original storage intent');
  LText := ReadSource(ARoot + '/tests/member.classes.fixture.pas');
  LInclude := ReadSource(ARoot + '/tests/member.classes.fixture.inc');
  LGeneric := ReadSource(ARoot + '/tests/member.generics.fixture.pas');
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('member.classes.fixture.pas'), LText)
    .WithFile(NyxPascalPhysicalFile('member.classes.fixture.inc'), LInclude)
    .WithFile(NyxPascalPhysicalFile('member.generics.fixture.pas'), LGeneric);
  LView := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFiles,
    NyxPascalPhysicalFile('member.classes.fixture.pas'), Facts, TNyxPascalTranslationOptions.Defaults),
    TNyxPascalSyntaxOptions.Defaults);
  Check(LView.CompleteTranslation and (LView.Syntax.DiagnosticCount = 0),
    'The unchanged whole class-property unit/include has complete declaration syntax');
  LGenericView := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFiles,
    NyxPascalPhysicalFile('member.generics.fixture.pas'), Facts, TNyxPascalTranslationOptions.Defaults),
    TNyxPascalSyntaxOptions.Defaults);
  Check(LGenericView.CompleteTranslation and (LGenericView.Syntax.DiagnosticCount = 0),
    'The unchanged whole generic-method unit has complete declaration syntax');
  LOwner := LGenericView.Syntax.Named(0, 'TGenericFactory');
  LScope := LGenericView.Syntax.Declaration(LOwner).NestedScope;
  Check((LGenericView.Syntax.Named(LScope, 'Echo') >= 0) and
    (LGenericView.Syntax.Named(LScope, 'CopyValue') >= 0),
    'Actual generic-class signatures remain owned by their declared class');
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LView);
  LOwner := LView.Syntax.Named(0, 'TCounterSettings');
  Check(LWorld.TypeOfSymbol(LWorld.Symbol(0, LOwner)).Status = pbsBound,
    'The actual class owner binds without a substituted fixture declaration');
  LFiles := LFiles.WithFile(NyxPascalPhysicalFile('system.pas'),
    'unit System; interface type TObject=class end; implementation end.')
    .WithFile(NyxPascalPhysicalFile('objpas.pas'), 'unit ObjPas; interface implementation end.');
  SetLength(LRoots, 3);
  LRoots[0] := NyxPascalPhysicalFile('system.pas');
  LRoots[1] := NyxPascalPhysicalFile('objpas.pas');
  LRoots[2] := NyxPascalPhysicalFile('member.classes.fixture.pas');
  LProvider := NewNyxOwnedPascalCompletionProvider(Facts, LFiles, LRoots, TNyxOwnedPascalCompletionOptions.Defaults);
  LPrefix := 'unit Consumer; interface uses member.classes.fixture; implementation procedure Inspect; begin TCounterSettings.St';
  LSources := CaptureNyxPascalSources(NewNyxPascalProject(NyxPascalUnit('Consumer'),
    'unit Consumer; interface implementation end.')).WithBuffer(TNyxPascalSourceRef.Main, LPrefix);
  LReply := LProvider.Complete(NewNyxPascalCompletionRequest(LSources)
    .At(TNyxPascalSourceRef.Main, NyxTextScalarCount(LPrefix)).Take(1));
  Check((LReply.Status = pcsReady) and (LReply.Count = 1) and
    (LReply.Item(0).Name = TNyxText('State')) and (LReply.Item(0).Kind = pckProperty),
    'Ordinary completion offers the actual class property with its specialized role');
  Supports(LReply, INyxOwnedPascalCompletionResult, LMetadata);
  Check((LMetadata.Definition(0).FileReference.Name = TNyxText('member.classes.fixture.inc')) and
    (LMetadata.DefinitionSource(0).Slice(LMetadata.Definition(0).Span) = TNyxText('State')),
    'Class-property navigation retains its exact included name token');
  LPreview := LReply.Preview(0, LSources, LNewCaret);
  Check((LPreview.Text(TNyxPascalSourceRef.Main) = LPrefix + TNyxText('ate')) and
    (LSources.Text(TNyxPascalSourceRef.Main) = LPrefix) and
    (LNewCaret = NyxTextScalarCount(LPrefix) + 3),
    'Class-property insertion is exact and detached from the original source');
  LValueSources := LSources.WithBuffer(TNyxPascalSourceRef.Main,
    LPrefix + TNyxText('ate.Co'));
  LValueReply := LProvider.Complete(NewNyxPascalCompletionRequest(LValueSources)
    .At(TNyxPascalSourceRef.Main, NyxTextScalarCount(LPrefix) + 6).Take(1));
  Supports(LValueReply, INyxOwnedPascalCompletionResult, LValueMetadata);
  WriteLn('actual class-property result / status=', Ord(LValueReply.Status),
    ' gap=', Ord(LValueMetadata.Gap), ' issue=', Ord(LValueMetadata.BindingIssue),
    ' related=', LValueMetadata.RelatedName, ' message=', LValueReply.MessageText);
  Check((LValueReply.Status = pcsReady) and (LValueReply.Count = 1) and
    (LValueReply.Item(0).Name = TNyxText('Count')) and (LValueReply.Item(0).Kind = pckField),
    'Ordinary completion follows the actual class property into its record result');
  Check((LValueMetadata.Definition(0).FileReference.Name = TNyxText('member.classes.fixture.pas')) and
    (LValueMetadata.DefinitionSource(0).Slice(LValueMetadata.Definition(0).Span) = TNyxText('Count')),
    'Result-member navigation retains the actual field definition');
  LProvider := nil;
  Check(LMetadata.DefinitionSource(0).Slice(LMetadata.Definition(0).Span) = TNyxText('State'),
    'Detached property navigation survives provider retirement');
  for LAt := 0 to LView.Source.Files.Count - 1 do
  begin
    Check(LView.Source.Files.TextAt(LAt) = LFiles.TextAt(LAt),
      'Analysis preserves each exact compiled source/include packet');
  end;
end;
{$endif}

begin
  try
    RolesAndBinding;
    ThreadStorageRoles;
    RecoveryAndLimits;
    {$ifndef PAS2JS}
    Check(ParamCount = 1, 'Supply the repository root for exact compiled companions');
    WholeCompiledSource(TNyxText(ParamStr(1)));
    {$endif}
    WriteLn('PASS ', GChecks, ' class-member family and completion checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
