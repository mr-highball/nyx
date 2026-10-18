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
program nyx_pascal_scopes_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, {$ifndef PAS2JS}Classes,{$endif}
  nyx.text, nyx.bytes, nyx.pascal.context, nyx.pascal.files,
  nyx.pascal.translation, nyx.pascal.syntax, nyx.pascal.syntax.translated,
  nyx.pascal.binding, nyx.pascal.completion.owned;

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

function Find(const AWorld: INyxPascalSemanticWorld; AModule: Integer;
  const AName: TNyxText): INyxPascalResolution;
begin
  Result := AWorld.Lookup(EndSite(AWorld, AModule), NyxPascalBindingName(AName));
end;

function ScopeWorld: INyxPascalSemanticWorld;
begin
  { These small source models isolate lookup order. They are deliberately not
    substituted for a configured SDK or advertised as real RTL source capture. }
  Result := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults
    .WithImplicitScopes(pispCompilerUnits))
    .WithModule(View('system.pas', 'unit System; {$mode fpc} interface type Integer=SmallInt; implementation end.'))
    .WithModule(View('objpas.pas', 'unit ObjPas; {$mode objfpc} interface type Integer=LongInt; implementation end.'));
end;

function Terminal(const AWorld: INyxPascalSemanticWorld;
  const AResult: INyxPascalResolution): INyxPascalSemanticType;
var
  LAnswer: INyxPascalResolution;
begin
  LAnswer := AResult;

  if (LAnswer.Status = pbsBound) and (LAnswer.BoundType = nil) then
  begin
    LAnswer := AWorld.TypeOfSymbol(LAnswer.SymbolAt(0));
  end;
  Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType <> nil), 'The supported source type binds');
  Result := LAnswer.BoundType;
  while Result.Kind in [pstAlias, pstDistinct] do
  begin
    Result := Result.Target;
  end;
end;

procedure ScopeOrder;
var
  LWorld: INyxPascalSemanticWorld;
  LMissing: INyxPascalSemanticWorld;
  LAnswer: INyxPascalResolution;
  LType: INyxPascalSemanticType;
  LIdentity: INyxPascalIntrinsicIdentity;
begin
  Check(TNyxOwnedPascalCompletionOptions.Defaults.Binding.ImplicitScopes = pispCompilerUnits,
    'The owned engine defaults to compiler scopes rather than symbolic name fallback');
  LWorld := ScopeWorld.WithModule(View('consumer.pas',
    'unit Consumer; {$mode delphi} interface implementation end.'));
  LAnswer := Find(LWorld, 2, 'Integer');
  Check((LAnswer.Status = pbsBound) and (LAnswer.SymbolAt(0).ModuleIndex = 1),
    'An unqualified Delphi name binds the actual captured ObjPas declaration before System');
  LType := Terminal(LWorld, LAnswer);
  Check((LType.Builtin = pbbLongInt) and Supports(LType, INyxPascalIntrinsicIdentity, LIdentity) and
    (LIdentity.IntrinsicOrigin = pioSystem) and
    (LIdentity.IntrinsicReference.Name = TNyxText('system.longint')) and
    (LIdentity.IntrinsicContext.Mode = ppmFPC),
    'The implicit alias retains its actual terminal System target and declaring compiler context');
  LType := Terminal(LWorld, Find(LWorld, 2, 'System.Integer'));
  Check((LType.Builtin = pbbSmallInt) and Supports(LType, INyxPascalIntrinsicIdentity, LIdentity) and
    (LIdentity.IntrinsicReference.Name = TNyxText('system.smallint')) and
    (LIdentity.IntrinsicContext.Mode = ppmFPC),
    'Explicit System qualification retains the independent source alias and context');
  LAnswer := Find(LWorld, 2, 'ObjPas.Integer');
  Check((LAnswer.Status = pbsBound) and (LAnswer.SymbolAt(0).ModuleIndex = 1),
    'A compiler implicit unit is also visible through explicit qualification');
  LMissing := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults
    .WithImplicitScopes(pispCompilerUnits)).WithModule(LWorld.ModuleAt(0)).WithModule(LWorld.ModuleAt(2));
  LAnswer := Find(LMissing, 1, 'Integer');
  Check((LAnswer.Status = pbsUnresolved) and (LAnswer.Issue = pbiMissingModule) and
    (LAnswer.RelatedName = TNyxText('ObjPas')),
    'Missing implicit ObjPas refuses instead of falling through to a different System declaration');
  LAnswer := Find(LMissing, 1, 'System.Integer');
  Check((LAnswer.Status = pbsBound) and (LAnswer.SymbolAt(0).ModuleIndex = 0),
    'An explicit known namespace does not depend on an unrelated implicit unit');
  LWorld := LWorld.WithModule(View('consumer.pas',
    'unit Consumer; {$mode fpc} interface implementation end.'));
  Check(Find(LWorld, 2, 'Integer').SymbolAt(0).ModuleIndex = 0,
    'FPC mode does not import ObjPas merely because a world happens to contain it');
  LWorld := LWorld.WithModule(View('consumer.pas',
    'unit Consumer; {$mode delphi} interface type Integer=Byte; implementation end.'));
  Check(Find(LWorld, 2, 'Integer').SymbolAt(0).ModuleIndex = 2,
    'A source-local declaration precedes the entire implicit chain');
  LType := Terminal(LWorld, Find(LWorld, 2, 'Integer'));
  Check(LType.Builtin = pbbByte, 'A local alias follows its declared target instead of a same-spelled pristine measurement');
end;

procedure Provenance;
var
  LWorld: INyxPascalSemanticWorld;
  LLeft: INyxPascalIntrinsicIdentity;
  LRight: INyxPascalIntrinsicIdentity;
  LType: INyxPascalSemanticType;
begin
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults)
    .WithModule(View('legacy.pas', 'unit Legacy; {$mode delphi} interface implementation end.'));
  LType := Terminal(LWorld, Find(LWorld, 0, 'Integer'));
  Check(Supports(LType, INyxPascalIntrinsicIdentity, LLeft), 'A symbolic intrinsic exposes typed provenance');
  LType := Terminal(LWorld, Find(LWorld, 0, 'System.Integer'));
  Check(Supports(LType, INyxPascalIntrinsicIdentity, LRight) and
    (LLeft.IntrinsicOrigin <> LRight.IntrinsicOrigin) and
    (LLeft.IntrinsicReference.Name <> LRight.IntrinsicReference.Name),
    'The legacy categories preserve distinct unqualified and System identities without claiming equal types');
end;

{$ifndef PAS2JS}
function ReadText(const AName: String): TNyxText;
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LStream := TFileStream.Create(AName, fmOpenRead or fmShareDenyWrite);
  try
    Check((LStream.Size > 0) and (LStream.Size <= 16384), 'Only the exact bounded source fixture is read');
    SetLength(LBytes, LStream.Size);
    LStream.ReadBuffer(LBytes[0], Length(LBytes));
    Result := NyxDecodeUTF8(LBytes);
  finally
    LStream.Free;
  end;
end;

procedure ActualImports;
var
  LWorld: INyxPascalSemanticWorld;
  LAnswer: INyxPascalResolution;
  LType: INyxPascalSemanticType;
begin
  Check(ParamCount = 2, 'Supply repository and actual FPC source root for source inspection');
  LWorld := ScopeWorld
    .WithModule(View('first.pas', ReadText(IncludeTrailingPathDelimiter(ParamStr(1)) + 'tests/nyx.scope.first.pas')))
    .WithModule(View('last.pas', ReadText(IncludeTrailingPathDelimiter(ParamStr(1)) + 'tests/nyx.scope.last.pas')))
    .WithModule(View('consumer.pas', 'unit Consumer; {$mode delphi} interface uses nyx.scope.first, nyx.scope.last; implementation end.'));
  LAnswer := Find(LWorld, 4, 'Integer');
  Check((LAnswer.Status = pbsBound) and (LAnswer.SymbolAt(0).ModuleIndex = 3),
    'The same whole sources compiled by the independent oracle obey last explicit import precedence');
  LType := Terminal(LWorld, LAnswer);
  Check(LType.Builtin = pbbQWord, 'The last source alias targets the independently compiler-observed eight-byte category');
  LAnswer := Find(LWorld, 4, 'nyx.scope.first.Integer');
  Check((LAnswer.Status = pbsBound) and (LAnswer.SymbolAt(0).ModuleIndex = 2),
    'Explicit dotted unit qualification preserves the first imported owner');
end;

procedure ActualObjPas;
var
  LFiles: INyxPascalFileCatalog;
  LView: INyxPascalTranslatedSyntax;
  LWorld: INyxPascalSemanticWorld;
  LContext: INyxPascalCompilerContext;
  LAnswer: INyxPascalResolution;
  LText: TNyxText;
begin
  { This is deliberately unconfigured source inspection. Compiler facts are not
    borrowed from the executable's own conditional symbols or invented defaults. }
  LContext := NewNyxPascalCompilerContext(ppcUnknown,
    TNyxPascalCompilerVersion.Create(0, 0, 0), ppmUnknown);
  LText := ReadText(IncludeTrailingPathDelimiter(ParamStr(2)) + 'rtl/objpas/objpas.pp');
  LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('rtl/objpas.pp'), LText);
  LView := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFiles,
    NyxPascalPhysicalFile('rtl/objpas.pp'), LContext, TNyxPascalTranslationOptions.Defaults),
    TNyxPascalSyntaxOptions.Defaults);
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults
    .WithImplicitScopes(pispCompilerUnits)).WithModule(LView);
  LAnswer := Find(LWorld, 0, 'Integer');
  WriteLn('actual ObjPas / declarations=', LView.Syntax.DeclarationCount,
    ' complete=', LView.CompleteTranslation, ' status=', Ord(LAnswer.Status),
    ' translation diagnostics=', LView.Source.DiagnosticCount,
    ' syntax diagnostics=', LView.Syntax.DiagnosticCount,
    ' Integer issue=', Ord(LAnswer.Issue));
  Check((NyxPascalOccurrenceLexical(LView.Source, 0).Source = LText) and
    (LView.Source.DiagnosticCount > 0) and not LView.CompleteTranslation and
    (LAnswer.Status = pbsUnresolved),
    'Unconfigured actual ObjPas retains its whole source and refuses unavailable export meaning');
end;
{$endif}

begin
  try
    ScopeOrder;
    Provenance;
    {$ifndef PAS2JS}
    ActualImports;
    ActualObjPas;
    {$endif}
    WriteLn('PASS ', GChecks, ' implicit source scope and intrinsic identity checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
