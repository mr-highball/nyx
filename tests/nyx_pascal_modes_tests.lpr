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
program nyx_pascal_modes_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.scheduler, nyx.pascal.context, nyx.pascal.lexing,
  nyx.pascal.files, nyx.pascal.translation, nyx.pascal.syntax,
  nyx.pascal.syntax.translated;

type
  { Construction visitor with no retained source. Destruction proves that a
    published snapshot releases its observer rather than forming a lifetime loop. }
  TPolicyVisitor = class(TInterfacedObject, INyxPascalLexicalObserver)
  public
    destructor Destroy; override;
    procedure AfterToken(AIndex: Integer; const AToken: TNyxPascalToken;
      const AText: TNyxText; var AAllowNestedComments: Boolean);
  end;
  { Cancellation occurs during one long post-directive comment. No source or
    translation survives through this borrowed polling scope. }
  TCommentCancellation = class(TInterfacedObject, INyxCancellationScope)
  private
    FPolls: Integer;
  public
    procedure Cancel;
    function GetCancelled: Boolean;
  end;

var
  GChecks: Integer;
  GVisitorsDestroyed: Integer;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
end;

destructor TPolicyVisitor.Destroy;
begin
  Inc(GVisitorsDestroyed);
  inherited Destroy;
end;

procedure TPolicyVisitor.AfterToken(AIndex: Integer; const AToken: TNyxPascalToken;
  const AText: TNyxText; var AAllowNestedComments: Boolean);
begin

  if AToken.Kind = ptkDirective then
  begin
    AAllowNestedComments := AText = TNyxText('{$nestedcomments on}');
  end;
end;

procedure TCommentCancellation.Cancel;
begin
  FPolls := High(Integer) - 1;
end;

function TCommentCancellation.GetCancelled: Boolean;
begin
  Inc(FPolls);
  Result := FPolls > 20;
end;

function Facts: INyxPascalCompilerContext;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi)
    .WithSymbol(NyxPascalDefine('FPC'), ppdDefined)
    .WithFeature(ppfNestedComments, ppkFalse).CompleteDefines(True);
end;

function Translate(const AText: TNyxText): INyxPascalTranslation;
begin
  Result := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('main.pas'), AText),
    NyxPascalPhysicalFile('main.pas'), Facts, TNyxPascalTranslationOptions.Defaults);
end;

function WordIndex(const ASource: INyxPascalTranslation; const AWord: TNyxText): Integer;
var
  LAt: Integer;
begin
  for LAt := 0 to ASource.Count - 1 do
  begin

    if (ASource.PhysicalToken(LAt).Kind = ptkWord) and (ASource.Text(LAt) = AWord) then
    begin
      Exit(LAt);
    end;
  end;
  raise Exception.Create('Missing authored word: ' + AWord);
end;

function StateAt(const ASource: INyxPascalTranslation;
  const AWord: TNyxText): INyxPascalCompilerContext;
begin
  Result := ASource.State(ASource.Token(WordIndex(ASource, AWord)).State);
end;

procedure PolicyAndModes;
var
  LVisitor: INyxPascalLexicalObserver;
  LLexical: INyxPascalLexicalSnapshot;
  LSource: INyxPascalTranslation;
  LState: INyxPascalCompilerContext;
  LAt: Integer;
  LJoined: TNyxText;
begin
  LVisitor := TPolicyVisitor.Create;
  LLexical := NewNyxPascalContextualLexicalSnapshot(
    '{$nestedcomments on}{outer {inner} tail} EndName',
    TNyxPascalLexicalOptions.Defaults, LVisitor);
  LVisitor := nil;
  Check((GVisitorsDestroyed = 1) and (LLexical.DiagnosticCount = 0),
    'Contextual publication releases the visitor and keeps its completed tokens');
  Check(not LLexical.Options.AllowNestedComments,
    'Snapshot options describe the initial policy, never a fictitious uniform final mode');
  LJoined := '';
  for LAt := 0 to LLexical.Count - 1 do
  begin
    LJoined := LJoined + LLexical.Text(LAt);
  end;
  Check(LJoined = LLexical.Source, 'Dynamic lexing preserves every original storage unit');

  LSource := Translate('{$mode fpc}{outer {inner} tail} FPCSite');
  Check(LSource.Resolved, 'Known FPC source mode translates without guessed exports');
  LState := StateAt(LSource, 'FPCSite');
  Check((LState.Mode = ppmFPC) and (LState.Feature(ppfNestedComments) = ppkTrue) and
    (LState.Option('H') = ppkFalse) and (LState.Feature(ppfOutParameters) = ppkFalse),
    'FPC mode records its qualified comment, short-string and OUT defaults');
  LSource := Translate('{$mode objfpc}ObjSite {$modeswitch out-}OutOff {$modeswitch out}OutOn');
  LState := StateAt(LSource, 'ObjSite');
  Check((LState.Mode = ppmObjFPC) and (LState.Feature(ppfNestedComments) = ppkTrue) and
    (LState.Feature(ppfOutParameters) = ppkTrue) and
    (LState.Symbol(NyxPascalDefine('FPC_OBJFPC')).IsDefined = ppkTrue) and
    (LState.Symbol(NyxPascalDefine('FPC_DELPHI')).IsDefined = ppkFalse),
    'ObjFPC source mode updates the exact mode define and permits OUT parameters');
  Check((StateAt(LSource, 'OutOff').Feature(ppfOutParameters) = ppkFalse) and
    (StateAt(LSource, 'OutOn').Feature(ppfOutParameters) = ppkTrue),
    'Explicit OUT switches retain typed ordered facts');
  LSource := Translate('{$mode delphiunicode}UnicodeSite');
  LState := StateAt(LSource, 'UnicodeSite');
  Check((LState.Mode = ppmDelphiUnicode) and (LState.Option('H') = ppkTrue) and
    (LState.Feature(ppfUnicodeStrings) = ppkTrue) and
    (LState.Symbol(NyxPascalDefine('FPC_DELPHI')).IsDefined = ppkTrue),
    'DelphiUnicode records Unicode defaults with the actual Delphi mode define');
  LSource := Translate('{$mode tp}TPSite');
  LState := StateAt(LSource, 'TPSite');
  Check((LState.Option('H') = ppkFalse) and (LState.Feature(ppfNestedComments) = ppkFalse) and
    (LState.Symbol(NyxPascalDefine('FPC_TP')).IsDefined = ppkTrue),
    'TP restores nonnested comments and its own mode identity');
  LSource := Translate('{$mode delphi}DelphiSite');
  LState := StateAt(LSource, 'DelphiSite');
  Check((LState.Option('H') = ppkTrue) and (LState.Feature(ppfUnicodeStrings) = ppkFalse) and
    (LState.Feature(ppfAnonymousFunctions) = ppkUnknown) and
    (LState.Feature(ppfNestedProcVars) = ppkUnknown),
    'Mode reset revokes unsupported/version-sensitive feature knowledge');
  LSource := Translate('{$nestedcomments on}{outer {$nestedcomments off} still comment}' +
    'AfterComment ''{$mode objfpc}'' AfterString');
  Check(LSource.Resolved and
    (StateAt(LSource, 'AfterComment').Feature(ppfNestedComments) = ppkTrue) and
    (StateAt(LSource, 'AfterString').Mode = ppmDelphi),
    'Directive-looking text inside comments/strings does not mutate compiler policy');
end;

procedure IncludesAndOrigins;
var
  LFiles: INyxPascalFileCatalog;
  LSource: INyxPascalTranslation;
  LView: INyxPascalTranslatedSyntax;
  LFirst: INyxPascalLexicalSnapshot;
  LSecond: INyxPascalLexicalSnapshot;
  LOrigin: TNyxPascalSyntaxOrigin;
  LAt: Integer;
  LBefore: Integer;
  LRejected: Boolean;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('main.pas'),
      'unit Main; interface {$I enter.inc}{outer {inner} tail}'#13#10 +
      '{$I shared.inc}{$I leave.inc}{$I shared.inc} implementation end.')
    .WithFile(NyxPascalPhysicalFile('enter.inc'), '{$mode objfpc}')
    .WithFile(NyxPascalPhysicalFile('leave.inc'), '{$nestedcomments off}')
    .WithFile(NyxPascalPhysicalFile('shared.inc'),
      '{ outer { inner } // 🚀 }'#10'var SharedValue: Integer;');
  LSource := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('main.pas'), Facts,
    TNyxPascalTranslationOptions.Defaults);
  Check(LSource.Resolved and (LSource.OccurrenceCount = 5),
    'An included mode controls the next parent comment and repeated include visits');
  LFirst := NyxPascalOccurrenceLexical(LSource, 2);
  LSecond := NyxPascalOccurrenceLexical(LSource, 4);
  Check(LFirst.Options.AllowNestedComments and not LSecond.Options.AllowNestedComments and
    (LFirst.Count <> LSecond.Count),
    'Repeated physical bytes retain independent lexical policy and token ordinals');
  LRejected := False;
  try
    LSource.Physical(LFiles.Find(NyxPascalPhysicalFile('shared.inc')));
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Legacy file-only access refuses an ambiguous occurrence');
  LView := NewNyxPascalTranslatedSyntax(LSource, TNyxPascalSyntaxOptions.Defaults);
  Check(LView.CompleteTranslation, 'Known contextual tokens reach the declared syntax projection');
  LBefore := GChecks;
  for LAt := 0 to LView.OriginCount - 1 do
  begin
    LOrigin := LView.Origin(LAt);
    Check((NyxPascalOccurrenceLexical(LSource, LOrigin.Occurrence).Slice(LOrigin.PhysicalSpan) =
      LView.Syntax.Lexical.Slice(LOrigin.VirtualSpan)) and
      (LView.VirtualAt(LOrigin.Occurrence, LOrigin.PhysicalSpan.Start) = LOrigin.VirtualSpan.Start),
      'Every syntax origin round trips through its exact repeated include context');
  end;
  Check(GChecks > LBefore, 'The origin check traverses actual projected declarations');
  Check(LSource.PhysicalToken(WordIndex(LSource, 'implementation')).Line = 2,
    'CRLF physical positions survive include-driven policy changes');
  LSource := nil;
  LView := nil;
  Check((LFirst.Source = LSecond.Source) and (LFirst.Token(0).Kind = ptkComment),
    'Detached occurrence snapshots remain valid after their translation is released');
end;

procedure Refusals;
var
  LSource: INyxPascalTranslation;
  LFiles: INyxPascalFileCatalog;
  LView: INyxPascalTranslatedSyntax;
  LRejected: Boolean;
  LAt: Integer;
  LHasPolicyIssue: Boolean;
  LHasUnbalanced: Boolean;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('main.pas'),
    'unit Main; interface {$ifdef MAYBE}{$nestedcomments on}{$endif} Tail');
  LSource := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('main.pas'),
    Facts.CompleteDefines(False), TNyxPascalTranslationOptions.Defaults);
  LHasPolicyIssue := False;
  LHasUnbalanced := False;
  for LAt := 0 to LSource.DiagnosticCount - 1 do
  begin
    LHasPolicyIssue := LHasPolicyIssue or (LSource.Diagnostic(LAt).Issue = ptiLexicalPolicyChange);
    LHasUnbalanced := LHasUnbalanced or (LSource.Diagnostic(LAt).Issue = ptiUnbalancedConditional);
  end;
  LView := NewNyxPascalTranslatedSyntax(LSource, TNyxPascalSyntaxOptions.Defaults);
  Check(not LSource.Resolved and LHasPolicyIssue and not LHasUnbalanced and
    not LView.CompleteTranslation and
    (LSource.Token(WordIndex(LSource, 'Tail')).Activity = ppkUnknown),
    'Unknown lexical branches stop safe syntax without manufacturing an unmatched conditional');
  LSource := Translate('{$mode objfpc}{$mode delphi}AfterInvalidMode');
  Check(not LSource.Resolved and
    (LSource.Token(WordIndex(LSource, 'AfterInvalidMode')).Activity = ppkUnknown),
    'A second source mode refuses instead of claiming a compiler-valid transition');
  LFiles := LFiles.WithFile(NyxPascalPhysicalFile('main.pas'), '{$mode objfpc}UnknownCompilerTail');
  LSource := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('main.pas'),
    NewNyxPascalCompilerContext(ppcUnknown,
      TNyxPascalCompilerVersion.Create(0, 0, 0), ppmUnknown),
    TNyxPascalTranslationOptions.Defaults);
  Check(not LSource.Resolved and
    (LSource.Token(WordIndex(LSource, 'UnknownCompilerTail')).Activity = ppkUnknown),
    'FPC-specific mode defaults are never inferred for an unknown compiler family');
  LSource := Translate('{$ifdef MISSING}{$mode objfpc}{$I missing.inc}{$endif}' +
    'Ready {$modeswitch nestedcomments+}{outer {inner} tail}' +
    '{$modeswitch nestedcomments-}Finished');
  Check(LSource.Resolved and (LSource.OccurrenceCount = 1) and
    (StateAt(LSource, 'Ready').Mode = ppmDelphi) and
    (StateAt(LSource, 'Finished').Feature(ppfNestedComments) = ppkFalse),
    'Inactive mode/include effects are skipped; active +/- switches remain ordered');
  LRejected := False;
  try
    NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
      .WithFile(NyxPascalPhysicalFile('main.pas'), '{$mode objfpc}{one {two} three}'),
      NyxPascalPhysicalFile('main.pas'), Facts,
      TNyxPascalTranslationOptions.Defaults.Lexical(
        TNyxPascalLexicalOptions.Defaults.UpToCommentDepth(1)));
  except
    on ENyxPascalLexicalLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Dynamically enabled nesting retains the original lexical depth quota');
  LRejected := False;
  try
    NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
      .WithFile(NyxPascalPhysicalFile('main.pas'),
        '{$mode objfpc}{' + TNyxText(StringOfChar('x', 65536)) + '}'),
      NyxPascalPhysicalFile('main.pas'), Facts, TNyxPascalTranslationOptions.Defaults,
      TCommentCancellation.Create);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Cancellation inside a long contextual token aborts unpublished construction');
end;

begin
  try
    PolicyAndModes;
    IncludesAndOrigins;
    Refusals;
    WriteLn('PASS ', GChecks, ' ordered lexical mode/include checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
