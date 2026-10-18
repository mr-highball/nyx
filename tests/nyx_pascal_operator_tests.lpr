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
program nyx_pascal_operator_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.scheduler, nyx.pascal.lexing, nyx.pascal.context,
  nyx.pascal.files, nyx.pascal.translation, nyx.pascal.syntax,
  nyx.pascal.syntax.translated, nyx.pascal.binding;

const
  CSource: TNyxText = 'unit OperatorShapes; {$mode delphi} interface type ' +
    'TVector=record Value:LongInt; private ' +
    'class operator -(const A:TVector):TVector; public ' +
    'function Add(A:LongInt):LongInt; ' +
    'class operator +(const A,B:TVector):TVector; ' +
    'class operator Add(const A:TVector; B:LongInt):TVector; ' +
    'class operator Explicit(const A:TVector):LongInt; ' +
    'class operator :=(A:LongInt):TVector; end; implementation end.';

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

{ Controlled source meaning uses the low-level symbolic compatibility policy.
  No synthetic System/ObjPas exports stand in for actual compiler SDK meaning. }
function View(const ASource: TNyxText): INyxPascalTranslatedSyntax;
var
  LFacts: INyxPascalCompilerContext;
begin
  LFacts := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi).CompleteDefines(True);
  Result := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(
    NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('source.pas'), ASource),
    NyxPascalPhysicalFile('source.pas'), LFacts, TNyxPascalTranslationOptions.Defaults),
    TNyxPascalSyntaxOptions.Defaults);
end;

function EndSite(const AWorld: INyxPascalSemanticWorld; AModule: Integer): INyxPascalBindingSite;
begin
  Result := AWorld.At(AModule, 0, AWorld.ModuleAt(AModule).Syntax.Lexical.Scalars);
end;

procedure FamiliesAndGuards;
var
  LView: INyxPascalTranslatedSyntax;
  LWorld: INyxPascalSemanticWorld;
  LOther: INyxPascalSemanticWorld;
  LType: INyxPascalSemanticType;
  LSite: INyxPascalBindingSite;
  LSyntax: INyxPascalOperatorSyntax;
  LQueries: INyxPascalOperatorQueries;
  LMembers: INyxPascalMemberQueries;
  LReply: INyxPascalResolution;
  LPage: INyxPascalMemberPage;
  LAt: Integer;
  LScope: Integer;
  LCancel: INyxCancellationScope;
  LRejected: Boolean;
begin
  LView := View(CSource);
  Check(LView.CompleteTranslation and (LView.Syntax.DiagnosticCount = 0),
    'Supported operator declarations retain complete surrounding syntax');
  Check(Supports(LView.Syntax, INyxPascalOperatorSyntax, LSyntax),
    'Existing snapshots expose an optional typed operator index');
  LScope := LView.Syntax.Declaration(LView.Syntax.Named(0, 'TVector')).NestedScope;
  LAt := LSyntax.NamedOperator(LScope, pokAdd);
  Check((LAt >= 0) and (LView.Syntax.Declaration(LAt).Name = TNyxText('+')) and
    (LView.Syntax.Declaration(LAt).NextNamed >= 0),
    'Symbolic and named additions retain one typed family with original spellings');
  Check((LView.Syntax.Declaration(LAt).OperatorKind = pokAdd) and
    (LView.Syntax.Declaration(LView.Syntax.Named(LScope, 'Add')).OperatorKind = pokNone),
    'A method named Add stays independent of the operator family');
  Check(LView.Syntax.Named(LScope, 'operator#' + TNyxText(IntToStr(Ord(pokAdd)))) < 0,
    'Internal operator index keys never escape through the ordinary name contract');
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LView);
  LSite := EndSite(LWorld, 0);
  LReply := LWorld.Lookup(LSite, NyxPascalBindingName('TVector'));
  LType := LWorld.TypeOfSymbol(LReply.SymbolAt(0)).BoundType;
  Check(Supports(LWorld, INyxPascalOperatorQueries, LQueries),
    'The original world retains an optional typed operator-query capability');
  LReply := LQueries.OperatorMembers(LType, LSite, pokAdd);
  Check((LReply.Status = pbsOverloads) and (LReply.Count = 2),
    'Operator lookup retains alternatives without selecting an expression call');
  Check((LReply.SymbolAt(0).Declaration.OperatorKind = pokAdd) and
    (LWorld.TypeOfSymbol(LReply.SymbolAt(0)).BoundType.Definition.Declaration.Name = TNyxText('TVector')),
    'Existing bound-symbol/result consumers retain typed operator identity');
  LReply := LWorld.Member(LType, LSite, NyxPascalBindingName('Add'));
  Check((LReply.Status = pbsBound) and (LReply.Count = 1) and
    (LReply.SymbolAt(0).Declaration.OperatorKind = pokNone),
    'Ordinary exact member lookup receives only the actual method');
  Supports(LWorld, INyxPascalMemberQueries, LMembers);
  LPage := LMembers.Members(LType, LSite, TNyxPascalMemberQuery.Defaults.Matching('A'));
  Check((LPage.ResultSet.Count = 1) and
    (LPage.ResultSet.SymbolAt(0).Declaration.Name = TNyxText('Add')),
    'Ordinary completion pages do not acquire operator spellings');
  LReply := LQueries.OperatorMembers(LType, LSite, pokExplicit);
  Check((LReply.Count = 1) and (LReply.SymbolAt(0).Declaration.OperatorKind = pokExplicit),
    'Explicit conversion identity is separate from implicit assignment');
  LOther := LWorld.WithModule(View('unit Caller; interface implementation end.'));
  Supports(LOther, INyxPascalOperatorQueries, LQueries);
  LReply := LOther.Lookup(EndSite(LOther, 0), NyxPascalBindingName('TVector'));
  LType := LOther.TypeOfSymbol(LReply.SymbolAt(0)).BoundType;
  LReply := LQueries.OperatorMembers(LType, EndSite(LOther, 1), pokSubtract);
  Check((LReply.Status = pbsInaccessible) and (LReply.Issue = pbiVisibility),
    'Typed operator lookup retains ordinary private visibility');
  LReply := nil;
  LRejected := False;
  try
    LReply := LQueries.OperatorMembers(LType, LSite, pokAdd);
  except
    on ENyxPascalBinding do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LReply = nil), 'A stale source-world site cannot query current operator meanings');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LRejected := False;
  try
    LReply := LQueries.OperatorMembers(LType, EndSite(LOther, 0), pokAdd, LCancel);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Operator queries keep explicit cancellation ownership');
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults.UpToCandidates(1)).WithModule(LView);
  LReply := LWorld.Lookup(EndSite(LWorld, 0), NyxPascalBindingName('TVector'));
  LType := LWorld.TypeOfSymbol(LReply.SymbolAt(0)).BoundType;
  Supports(LWorld, INyxPascalOperatorQueries, LQueries);
  LReply := nil;
  LRejected := False;
  try
    LReply := LQueries.OperatorMembers(LType, EndSite(LWorld, 0), pokAdd);
  except
    on ENyxPascalBindingLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LReply = nil), 'Overload limits refuse atomically rather than truncating alternatives');
  LRejected := False;
  try
    LReply := LQueries.OperatorMembers(LType, EndSite(LWorld, 0), pokNone);
  except
    on ENyxPascalBinding do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Operator queries require an explicit supported closed family');
  LView := View('unit UnknownOperators; interface type TUnsafe=record ' +
    'class operator Wiggle(A:TUnsafe):TUnsafe; end; implementation end.');
  LScope := LView.Syntax.Declaration(LView.Syntax.Named(0, 'TUnsafe')).NestedScope;
  Check((LView.Syntax.DiagnosticCount > 0) and LView.Syntax.Scope(LScope).UnsupportedSyntax and
    (LView.Syntax.Named(LScope, 'Wiggle') < 0),
    'An unknown named operator retains explicit unsafe scope instead of an ordinary method');
  LView := View('unit UnknownPunctuation; interface type TUnsafe=record ' +
    'class operator @(A:TUnsafe):TUnsafe; end; implementation end.');
  LScope := LView.Syntax.Declaration(LView.Syntax.Named(0, 'TUnsafe')).NestedScope;
  Check((LView.Syntax.DiagnosticCount > 0) and LView.Syntax.Scope(LScope).UnsupportedSyntax,
    'Unknown operator punctuation cannot grant a complete candidate scope');
end;

begin
  try
    FamiliesAndGuards;
    WriteLn('PASS ', GChecks, ' typed operator declaration/query checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
