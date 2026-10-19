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
program nyx_pascal_bootstrap_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.pascal.context, nyx.pascal.compilertrace,
  nyx.pascal.files, nyx.pascal.translation, nyx.pascal.syntax,
  nyx.pascal.syntax.translated, nyx.pascal.binding, nyx.pascal.conditions;

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

function SourceFacts: INyxPascalCompilerContext;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmFPC).CompleteDefines(True);
end;

{ Explicit controlled metadata exercises ownership/binding algorithms only.
  The separate native consumer measures the configured compiler before aliasing. }
function Bootstrap: INyxPascalCompilerContext;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmFPC, pcsoSystemBootstrap)
    .WithOption('H', ppkFalse);
  Result := NyxPascalWithCapturedSize(Result, NyxPascalSizeReference('AnsiChar'),
    NyxPascalSizeFact(pssKnown, ppmFPC, ppkFalse, 1));
  Result := NyxPascalWithCapturedSize(Result, NyxPascalSizeReference('WideChar'),
    NyxPascalSizeFact(pssKnown, ppmFPC, ppkFalse, 2));
  Result := NyxPascalWithCapturedSize(Result, NyxPascalSizeReference('Comp'),
    NyxPascalSizeFact(pssKnown, ppmFPC, ppkFalse, 8));
  Result := NyxPascalWithCapturedSize(Result, NyxPascalSizeReference('Char'),
    NyxPascalSizeFact(pssUndeclared, ppmFPC, ppkFalse));
end;

function Analyze(const AText: TNyxText; const AContext: INyxPascalCompilerContext;
  const AWorld: INyxPascalSemanticWorld): INyxPascalSemanticTranslation;
begin
  Result := NewNyxPascalSemanticTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('controlled.pas'), AText), NyxPascalPhysicalFile('controlled.pas'),
    AContext, AWorld, TNyxPascalTranslationOptions.Defaults,
    TNyxPascalSyntaxOptions.Defaults, TNyxPascalConditionalOptions.Defaults);
end;

procedure ContextOwnership;
var
  LRoot: INyxPascalCompilerContext;
  LSource: INyxPascalCompilerContext;
  LChanged: INyxPascalCompilerContext;
  LProbe: INyxPascalCompilerProbe;
  LRejected: Boolean;
begin
  LRoot := Bootstrap;
  LSource := NyxPascalWithBootstrapContext(SourceFacts, LRoot);
  Check(not SourceFacts.SameFacts(LSource), 'Compiler-root identity participates in source equality');
  Check(CaptureNyxPascalCompilerContext(LSource).SameFacts(LSource),
    'Freezing retains the detached root context exactly');
  LChanged := LSource.InMode(ppmDelphi).WithOption('H', ppkTrue);
  Check(NyxPascalBootstrapContext(LChanged).SameFacts(LRoot) and
    (NyxPascalBootstrapContext(LChanged).Mode = ppmFPC),
    'Authored mode and string policy never reassign compiler-root identity');
  Check(NyxPascalBootstrapContext(LChanged.UnknownState) = nil,
    'Unknown conditional state drops compiler-root authority with other facts');
  Check(NyxPascalWithBootstrapContext(LSource, nil).SameFacts(SourceFacts),
    'Explicit detachment restores the original independent source facts');
  LRejected := False;
  try
    NyxPascalWithBootstrapContext(LRoot, LSource);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Nested/reverse attachment refuses before retaining a cycle');
  LRejected := False;
  try
    NyxPascalWithBootstrapContext(NewNyxPascalCompilerContext(ppcFPC,
      TNyxPascalCompilerVersion.Create(3, 2, 2), ppmFPC), LRoot);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Different compiler versions cannot share primitive evidence');
  LProbe := NewNyxFPCSystemBootstrapProbe(NyxPascalDefine('PORTABLE_ROOT'));
  LProbe := NyxFPCProbeSizeOf(LProbe, NyxPascalSizeReference('AnsiChar'));
  Check((NyxFPCProbeScope(CaptureNyxFPCCompilerProbe(LProbe)) = pcsoSystemBootstrap) and
    (Pos('unit System;', LProbe.Source) = 1) and (Pos('type', LProbe.Source) > 0),
    'Detached bootstrap requests preserve their explicit compiler-root source');
  LRejected := False;
  try
    NyxFPCProbeSizeOf(LProbe, NyxPascalSizeReference('System.Char'));
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Authored System exports cannot enter pre-alias measurements');
end;

procedure ScopedBinding;
const
  CSystem: TNyxText = 'unit System; {$mode objfpc}{$H+} interface ' +
    '{$if declared(Char)}{$fatal Future alias leaked}{$endif}' +
    'type {$if declared(AnsiChar)}Char=AnsiChar;{$else}AnsiChar=Char;{$endif}' +
    '{$if declared(Comp)}TComp=Comp;{$else}{$fatal Comp missing}{$endif}' +
    '{$if sizeof(Char)=1}{$define CORRECT_CHAR}{$else}{$fatal Wrong Char}{$endif}' +
    'implementation end.';
var
  LContext: INyxPascalCompilerContext;
  LResult: INyxPascalSemanticTranslation;
  LWorld: INyxPascalSemanticWorld;
  LView: INyxPascalTranslatedSyntax;
  LAnswer: INyxPascalResolution;
  LIdentity: INyxPascalIntrinsicIdentity;
begin
  LContext := NyxPascalWithBootstrapContext(SourceFacts, Bootstrap);
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults
    .WithImplicitScopes(pispCompilerUnits));
  LResult := Analyze(CSystem, LContext, LWorld);
  WriteLn('controlled root / answers=', LResult.Answers, ' pending=', LResult.Pending,
    ' issue=', Ord(LResult.BindingIssue), ' ', LResult.RelatedName);
  Check(LResult.Source.Resolved and not LResult.Pending and (LResult.Answers = 4),
    'Actual source ordering chooses the primitive branch, then binds its Char alias');
  Check(LResult.Source.Files.TextAt(0) = CSystem, 'Conditional refinement preserves the complete source');
  Check(LResult.Source.State(LResult.Source.StateCount - 1).Symbol(
    NyxPascalDefine('CORRECT_CHAR')).IsDefined = ppkTrue,
    'Source ObjFPC/H+ changes retain independent FPC/H- primitive sizes');
  LView := NewNyxPascalTranslatedSyntax(LResult.Source, TNyxPascalSyntaxOptions.Defaults);
  LWorld := LWorld.WithModule(LView);
  LAnswer := LWorld.Lookup(LWorld.At(0, 0, LView.Syntax.Lexical.Scalars),
    NyxPascalBindingName('System.AnsiChar'));
  Check((LAnswer.Status = pbsBound) and Supports(LAnswer.BoundType,
    INyxPascalIntrinsicIdentity, LIdentity) and
    (LIdentity.IntrinsicOrigin = pioSystemBootstrap) and
    (LIdentity.IntrinsicReference.Name = TNyxText('ansichar')) and
    (NyxPascalCompilerScope(LIdentity.IntrinsicContext) = pcsoSystemBootstrap),
    'Terminal primitive identity retains its measured compiler-root namespace');
  LResult := Analyze('unit Consumer; interface ' +
    '{$if sizeof(System.Char)=1}{$define QUALIFIED_OK}{$endif}implementation end.',
    LContext, LWorld);
  Check(LResult.Source.Resolved and not LResult.Pending,
    'Qualified authored System aliases unwrap to the measured primitive');
  LResult := Analyze('unit Consumer; interface type Char=packed record A:AnsiChar; end;' +
    '{$if sizeof(Char)=1}{$define MUST_NOT_GUESS}{$endif}implementation end.', LContext, LWorld);
  Check(LResult.Pending and (LResult.BindingIssue = pbiCompilerContext) and
    not LResult.Source.Resolved and
    (LResult.Source.State(LResult.Source.StateCount - 1).Symbol(
      NyxPascalDefine('MUST_NOT_GUESS')).IsDefined = ppkUnknown),
    'A local record shadow cannot borrow compiler or System Char measurements');
  LResult := Analyze('unit Consumer; interface {$if declared(Double)}{$endif}implementation end.',
    LContext, LWorld);
  Check(LResult.Pending and (LResult.BindingIssue = pbiCompilerContext),
    'An unqueried primitive remains unknown rather than guessed present');
  LResult := Analyze('unit Consumer; interface uses MissingLater;' +
    '{$if sizeof(System.Char)=1}{$endif}implementation end.', LContext, LWorld);
  { Qualified System bypasses unrelated imports by language rule; unqualified
    lookup must retain the later import's ability to shadow that same name. }
  Check(LResult.Source.Resolved, 'Explicit qualification follows the named captured owner');
  LResult := Analyze('unit Consumer; interface uses MissingLater;' +
    '{$if sizeof(Char)=1}{$endif}implementation end.', LContext, LWorld);
  Check(LResult.Pending and (LResult.BindingIssue = pbiMissingModule),
    'A missing later import refuses before primitive fallback');
end;

begin
  try
    ContextOwnership;
    ScopedBinding;
    WriteLn('PASS ', GChecks, ' compiler-root ownership and source-binding checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
