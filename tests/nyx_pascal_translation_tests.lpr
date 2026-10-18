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
program nyx_pascal_translation_tests;

{$mode delphi}{$H+}{$codepage utf8}
{$I nyx.callback.features.inc}

uses SysUtils, Classes, nyx.text, nyx.scheduler, nyx.pascal.lexing,
  nyx.pascal.context, nyx.pascal.files, nyx.pascal.translation,
  nyx.pascal.files.project,
  nyx.pascal.sources, nyx.pascal.project, nyx.pascal.names, nyx.pascal.drafts;

const
  {$ifdef NYX_HAS_INLINE_CALLBACKS}
  CCompilerInline = True;
  {$else}
  CCompilerInline = False;
  {$endif}
  {$if (FPC_FULLVERSION >= 30301) and defined(FPC)}
  CCompilerModernNative = True;
  {$else}
  CCompilerModernNative = False;
  {$endif}

type
  { Controlled cancellation checks entry and work inside an ordinary long token.
    The scope owns only a poll counter, never a translation or source owner. }
  TPolledCancellation = class(TInterfacedObject, INyxCancellationScope)
  private
    FCancelled: Boolean;
  public
    Polls: Integer;
    procedure Cancel;
    function GetCancelled: Boolean;
  end;

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

procedure TPolledCancellation.Cancel;
begin
  FCancelled := True;
end;

function TPolledCancellation.GetCancelled: Boolean;
begin
  Inc(Polls);
  Result := FCancelled or (Polls > 7);
end;

function NativeFacts(AVersion: Integer = 30301): INyxPascalCompilerContext;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(AVersion div 10000,
      (AVersion div 100) mod 100, AVersion mod 100), ppmDelphi)
    .WithSymbol(NyxPascalDefine('FPC'), ppdDefined)
    .WithSymbol(NyxPascalDefine('FPC_FULLVERSION'), ppdInteger, AVersion)
    .WithOption('H', ppkTrue)
    .CompleteDefines(True);
end;

function WordAt(const ATranslation: INyxPascalTranslation; const AText: TNyxText;
  AOrdinal: Integer = 0): Integer;
var
  LAt: Integer;
begin
  for LAt := 0 to ATranslation.Count - 1 do
  begin

    if (ATranslation.PhysicalToken(LAt).Kind = ptkWord) and
      (ATranslation.Text(LAt) = AText) then
    begin

      if AOrdinal = 0 then
      begin
        Exit(LAt);
      end;
      Dec(AOrdinal);
    end;
  end;
  raise Exception.Create('Missing translated word: ' + AText);
end;

function HasIssue(const ATranslation: INyxPascalTranslation;
  AIssue: TNyxPascalTranslationIssue): Boolean;
var
  LAt: Integer;
begin
  for LAt := 0 to ATranslation.DiagnosticCount - 1 do
  begin

    if ATranslation.Diagnostic(LAt).Issue = AIssue then
    begin
      Exit(True);
    end;
  end;
  Result := False;
end;

procedure Validate(const ATranslation: INyxPascalTranslation);
var
  LAt: Integer;
  LToken: TNyxPascalTranslatedToken;
  LOccurrence: TNyxPascalInclusion;
  LPhysical: INyxPascalLexicalSnapshot;
begin
  Check(ATranslation.OccurrenceCount > 0, 'Every translation has an explicit root occurrence');
  for LAt := 0 to ATranslation.Count - 1 do
  begin
    LToken := ATranslation.Token(LAt);
    LOccurrence := ATranslation.Occurrence(LToken.Occurrence);
    LPhysical := ATranslation.Physical(LOccurrence.FileIndex);
    Check((LToken.State >= 0) and (LToken.State < ATranslation.StateCount) and
      (ATranslation.Text(LAt) = LPhysical.Text(LToken.PhysicalToken)) and
      (ATranslation.PhysicalToken(LAt).Span.Finish <= LPhysical.Scalars),
      'Physical positions, exact token text and state identity remain snapshot-local');
  end;
  for LAt := 0 to ATranslation.OccurrenceCount - 1 do
  begin
    LOccurrence := ATranslation.Occurrence(LAt);
    Check((LOccurrence.Parent < LAt) and (LOccurrence.Parent >= -1) and
      (LOccurrence.FirstToken >= 0) and
      (LOccurrence.TokenCount <= ATranslation.Count - LOccurrence.FirstToken) and
      (LOccurrence.EntryState <= LOccurrence.ExitState) and
      (LOccurrence.ExitState < ATranslation.StateCount),
      'Inclusion tree and state/token intervals remain bounded and acyclic');
  end;
end;

procedure Contexts;
var
  LOpen: INyxPascalCompilerContext;
  LClosed: INyxPascalCompilerContext;
  LOther: INyxPascalCompilerContext;
  LResult: TNyxPascalConditionResult;
  LRejected: Boolean;
begin
  LOpen := NativeFacts.CompleteDefines(False);
  LClosed := LOpen.CompleteDefines(True);
  Check((LOpen.Symbol(NyxPascalDefine('UNSEEN')).IsDefined = ppkUnknown) and
    (LClosed.Symbol(NyxPascalDefine('UNSEEN')).IsDefined = ppkFalse),
    'Missing capture is distinct from explicitly complete-set absence');
  LOther := LClosed.WithSymbol(NyxPascalDefine('unseen'), ppdUnknown);
  Check((LOther.Symbol(NyxPascalDefine('UNSEEN')).IsDefined = ppkUnknown) and
    (LClosed.Symbol(NyxPascalDefine('UNSEEN')).IsDefined = ppkFalse),
    'Explicit unknown survives complete capture without mutating its baseline');
  Check(not LOpen.WithSymbol(NyxPascalDefine('FirstUnknown'), ppdUnknown).SameFacts(
    LOpen.WithSymbol(NyxPascalDefine('OtherUnknown'), ppdUnknown)),
    'Different unknown symbol names cannot alias exact context identity');
  Check((LClosed.Version.FullNumber = 30301) and (LClosed.Option('h') = ppkTrue) and
    (LClosed.InMode(ppmDelphi).Option('H') = ppkUnknown) and
    (LClosed.Option('H') = ppkTrue), 'Version/short-option/mode resets retain independent facts');
  LResult := AnalyzeNyxPascalCondition(LClosed, 'defined(FPC) and (FPC_FULLVERSION >= 30301)');
  Check((LResult.Truth = ppkTrue) and (LResult.Issue = pciNone),
    'Captured compiler version and define condition resolve without a host compiler');
  {$ifndef PAS2JS}
  Check((LResult.Truth = ppkTrue) = CCompilerModernNative,
    'The representative version condition agrees with this actual native compilation');
  {$endif}
  Check(AnalyzeNyxPascalCondition(LClosed, 'not defined(PAS2JS)').Truth = ppkTrue,
    'Undefined symbols remain known absent only in the sealed set');
  Check(AnalyzeNyxPascalCondition(LOpen, 'defined(UNSEEN)').Truth = ppkUnknown,
    'Open capture refuses to invent a condition result');
  Check(AnalyzeNyxPascalCondition(LOpen, 'defined(UNSEEN) and (1 = 2)').Truth = ppkFalse,
    'Three-valued Boolean logic retains decisive false knowledge');
  Check(AnalyzeNyxPascalCondition(LClosed, '($ff >= &77) and (%10 <> 3)').Truth = ppkTrue,
    'Exact base spellings compare without machine-locale conversion');
  Check(AnalyzeNyxPascalCondition(LClosed, '1 or 0 and 0').Truth = ppkTrue,
    'Pascal Boolean precedence preserves and before or');
  Check(AnalyzeNyxPascalCondition(LClosed, '(1 = 1) xor (1 = 2)').Truth = ppkTrue,
    'Boolean xor remains distinct from integer bit operations');
  Check(AnalyzeNyxPascalCondition(LClosed, 'declared(TThing)').Truth = ppkUnknown,
    'A semantic compiler predicate cannot be answered from define facts');
  Check(AnalyzeNyxPascalCondition(LClosed, 'not 30301').Truth = ppkUnknown,
    'Unsupported integer bit operations cannot become a Boolean guess');
  Check(AnalyzeNyxPascalCondition(LClosed, '00').Truth = ppkUnknown,
    'Noncanonical conditional literals cannot be normalized into a guessed Boolean');
  Check(AnalyzeNyxPascalCondition(LClosed, 'defined(&FPC)').Truth = ppkUnknown,
    'Escaped Pascal identifiers cannot masquerade as compiler-symbol syntax');
  Check(AnalyzeNyxPascalCondition(LClosed, 'FPC_FULLVERSION + 1 > 3').Truth = ppkUnknown,
    'Unimplemented arithmetic refuses instead of returning a partial parse');
  Check(AnalyzeNyxPascalCondition(LClosed, '(defined(FPC)').Truth = ppkUnknown,
    'Unfinished expressions remain unresolved');
  LRejected := False;
  try
    NyxPascalDefine('invalid/name');
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Compiler symbols remain distinct validated open names');
end;

procedure Files;
var
  LFiles: INyxPascalFileCatalog;
  LOther: INyxPascalFileCatalog;
  LSources: INyxPascalSourceSnapshot;
  LProject: INyxPascalProject;
  LRejected: Boolean;
  LText: TNyxText;
begin
  LText := TNyxText('packed 🐾') + #0 + #13#10;
  LFiles := NewNyxPascalFileCatalog(pfcASCIIInsensitive)
    .WithFile(NyxPascalPhysicalFile('src/main.pas'), 'unit Main;')
    .WithFile(NyxPascalPhysicalFile('src/shared.inc'), LText)
    .WithFile(NyxPascalPhysicalFile('fallback/shared.inc'), 'fallback')
    .WithIncludeDirectory('fallback');
  Check((LFiles.ResolveInclude(NyxPascalPhysicalFile('src/main.pas'), 'SHARED.inc') = 1) and
    (LFiles.ResolveInclude(NyxPascalPhysicalFile('other/main.pas'), 'shared.inc') = 2),
    'Explicit case policy and ordered current-directory/search lookup remain captured');
  Check(LFiles.ResolveInclude(NyxPascalPhysicalFile('src/sub/main.pas'), '../shared.inc') = 1,
    'Relative parents normalize within the virtual capture root');
  LOther := LFiles.WithFile(NyxPascalPhysicalFile('src/shared.inc'), 'replacement');
  Check((LFiles.TextAt(1) = LText) and (LOther.TextAt(1) = TNyxText('replacement')) and
    not LFiles.SameFiles(LOther) and LFiles.SameFiles(CaptureNyxPascalFiles(LFiles)),
    'Exact packed text and old captures survive persistent replacement/freeze');
  LRejected := False;
  try
    LFiles.WithFile(NyxPascalPhysicalFile('SRC/shared.inc'), '');
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Captured case aliases refuse atomically');
  LRejected := False;
  try
    NewNyxPascalFileCatalog(pfcExact).ResolveInclude(
      NyxPascalPhysicalFile('main.pas'), '../outside.inc');
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'An include refuses when every candidate escapes the virtual root');
  LOther := LFiles.WithIncludeDirectory('').WithIncludeDirectory('deep/search')
    .WithFile(NyxPascalPhysicalFile('deep/Only.inc'), 'searched');
  Check(LOther.ResolveInclude(NyxPascalPhysicalFile('src/main.pas'), '../Only.inc') = 3,
    'A root-escaping candidate cannot hide a later contained search-directory match');
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('A.inc'), 'first')
    .WithFile(NyxPascalPhysicalFile('a.inc'), 'second');
  Check(LFiles.Count = 2, 'Case-sensitive captures retain distinct source names');
  LProject := NewNyxPascalProject(NyxPascalUnit('captured'), 'unit captured; interface implementation end.')
    .WithInclude(NyxPascalInclude('parts.inc'), 'accepted');
  LSources := CaptureNyxPascalSources(LProject).WithBuffer(
    TNyxPascalSourceRef.IncludeFile(NyxPascalInclude('parts.inc')), LText);
  LFiles := CaptureNyxPascalProjectFiles(LSources, pfcExact);
  Check((LFiles.TextAt(1) = LText) and (LSources.AcceptedText(
    TNyxPascalSourceRef.IncludeFile(NyxPascalInclude('parts.inc'))) = TNyxText('accepted')) and
    (NyxPascalProjectPhysicalFile(LSources, TNyxPascalSourceRef.Main).Name = TNyxText('captured.pas')),
    'Project buffer capture preserves unfinished bytes and independent accepted ownership');
end;

procedure Translation;
const
  CRoot = 'unit Main; interface ' +
    '{$if defined(FPC) and (FPC_FULLVERSION >= 30301)} Modern {$else} Legacy {$endif} ' +
    '{$I parts.inc} AfterInclude ' +
    '{$undef FEATURE} {$I parts.inc} ' +
    '{$ifopt H+} LongStrings {$else} ShortStrings {$endif} ' +
    '{$ifdef NEVER} {$include uncaptured.inc} {$define LEAK} {$endif} ' +
    '{$ifdef LEAK} Leaked {$elseif defined(FPC)} Clean {$else} Unselected {$endif} ' +
    'implementation end.';
  CPart = '{$ifdef FEATURE} FeatureOn {$else} FeatureOff {$endif} {$define FROM_PART}';
var
  LFiles: INyxPascalFileCatalog;
  LContext: INyxPascalCompilerContext;
  LTranslation: INyxPascalTranslation;
  LUnknown: INyxPascalTranslation;
  LOptions: TNyxPascalTranslationOptions;
  LAt: Integer;
  LFirst: Integer;
  LSecond: Integer;
  LRejected: Boolean;
  LCancel: INyxCancellationScope;
  LPoller: TPolledCancellation;
begin
  LOptions := TNyxPascalTranslationOptions.Defaults;
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('main.pas'), CRoot)
    .WithFile(NyxPascalPhysicalFile('parts.inc'), CPart);
  LContext := NativeFacts.WithSymbol(NyxPascalDefine('FEATURE'), ppdDefined);
  LTranslation := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('main.pas'), LContext, LOptions);
  Validate(LTranslation);
  Check(LTranslation.Resolved and (LTranslation.DiagnosticCount = 0) and
    (LTranslation.OccurrenceCount = 3), 'Known condition/include context resolves without an external compiler');
  Check((LTranslation.Token(WordAt(LTranslation, 'Modern')).Activity = ppkTrue) and
    (LTranslation.Token(WordAt(LTranslation, 'Legacy')).Activity = ppkFalse) and
    (LTranslation.Token(WordAt(LTranslation, 'Clean')).Activity = ppkTrue) and
    (LTranslation.Token(WordAt(LTranslation, 'Leaked')).Activity = ppkFalse),
    'Conditional and elseif selection preserve inactive source without leaked side effects');
  LFirst := WordAt(LTranslation, 'FeatureOn', 0);
  LSecond := WordAt(LTranslation, 'FeatureOn', 1);
  Check((LTranslation.Token(LFirst).Activity = ppkTrue) and
    (LTranslation.Token(LSecond).Activity = ppkFalse) and
    (LTranslation.Token(LFirst).Occurrence <> LTranslation.Token(LSecond).Occurrence) and
    (LTranslation.PhysicalToken(LFirst).Span.Start = LTranslation.PhysicalToken(LSecond).Span.Start),
    'Repeated includes keep exact physical spans with independent state/occurrence identity');
  LAt := WordAt(LTranslation, 'AfterInclude');
  Check((LTranslation.State(LTranslation.Token(LAt).State).Symbol(
    NyxPascalDefine('FROM_PART')).IsDefined = ppkTrue) and
    (LContext.Symbol(NyxPascalDefine('FROM_PART')).IsDefined = ppkFalse),
    'Textual include writes flow back to the caller context without mutating its initial snapshot');
  Check(LTranslation.Token(WordAt(LTranslation, 'LongStrings')).Activity = ppkTrue,
    'Captured short-option state drives ifopt');
  Check(LTranslation.PhysicalToken(LTranslation.Count - 1).Kind = ptkEOF,
    'Only the explicit root owns the translated EOF');
  LFiles := LFiles.WithFile(NyxPascalPhysicalFile('main.pas'),
    '{$I ''assets/diagram-🐾.inc''} {$I-}{$ifopt I-}SafeIO{$endif}')
    .WithFile(NyxPascalPhysicalFile('assets/diagram-🐾.inc'), TNyxText('{packed 🐾') + #0 + '}');
  LUnknown := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('main.pas'), NativeFacts, LOptions);
  Check(LUnknown.Resolved and (LUnknown.OccurrenceCount = 2) and
    (LUnknown.Files.TextAt(LUnknown.Occurrence(1).FileIndex) = TNyxText('{packed 🐾') + #0 + '}') and
    (LUnknown.Token(WordAt(LUnknown, 'SafeIO')).Activity = ppkTrue),
    'Quoted Unicode include names/payloads remain exact and I- means IO checking');
  Check(not LTranslation.Files.SameFiles(LFiles),
    'A later file capture cannot silently replace an older translation identity');
  LFiles := LFiles.WithFile(NyxPascalPhysicalFile('main.pas'), '{$I opening.inc}Inside{$endif}After')
    .WithFile(NyxPascalPhysicalFile('opening.inc'), '{$ifdef FPC}Opening');
  LUnknown := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('main.pas'), NativeFacts, LOptions);
  Check(LUnknown.Resolved and
    (LUnknown.Token(WordAt(LUnknown, 'Inside')).Activity = ppkTrue),
    'Textual conditional frames can open in an include and close in its owner');

  LFiles := LFiles.WithFile(NyxPascalPhysicalFile('main.pas'),
    '{$ifdef UNCERTAIN}{$define Changed}{$else}{$undef Changed}{$endif}' +
    '{$ifdef Changed}Maybe{$else}Alternative{$endif} Always');
  LUnknown := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('main.pas'),
    NativeFacts.CompleteDefines(False), LOptions);
  Validate(LUnknown);
  Check(not LUnknown.Resolved and HasIssue(LUnknown, ptiUnknownCondition) and
    (LUnknown.Token(WordAt(LUnknown, 'Maybe')).Activity = ppkUnknown) and
    (LUnknown.Token(WordAt(LUnknown, 'Alternative')).Activity = ppkUnknown) and
    (LUnknown.Token(WordAt(LUnknown, 'Always')).Activity = ppkTrue),
    'Unknown branches retain both alternatives and conservatively revoke changed facts');

  LFiles := LFiles.WithFile(NyxPascalPhysicalFile('main.pas'),
    '{$I unavailable.inc}{$define UNSAFE_AFTER_GAP}{$ifdef FPC}Uncertain{$endif}');
  LUnknown := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('main.pas'), NativeFacts, LOptions);
  Check(HasIssue(LUnknown, ptiMissingInclude) and
    (LUnknown.Token(WordAt(LUnknown, 'Uncertain')).Activity = ppkUnknown) and
    (LUnknown.State(LUnknown.StateCount - 1).Symbol(
      NyxPascalDefine('UNSAFE_AFTER_GAP')).IsDefined = ppkUnknown) and
    not HasIssue(LUnknown, ptiUnbalancedConditional),
    'Uncaptured includes revoke later lexical interpretation as well as symbol certainty');

  LFiles := LFiles.WithFile(NyxPascalPhysicalFile('main.pas'), '{$I loop.inc}')
    .WithFile(NyxPascalPhysicalFile('loop.inc'), '{$I loop.inc}');
  LUnknown := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('main.pas'), NativeFacts, LOptions);
  Check(HasIssue(LUnknown, ptiIncludeCycle), 'Unproductive include recursion becomes an explicit context gap');
  LFiles := LFiles.WithFile(NyxPascalPhysicalFile('loop.inc'),
    '{$ifndef GUARDED}{$define GUARDED}{$I loop.inc}{$endif} Guarded');
  LUnknown := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('main.pas'), NativeFacts, LOptions);
  Check(LUnknown.Resolved and (LUnknown.OccurrenceCount = 3),
    'A state-changing include guard may reenter once before its inactive recursion stops');

  LFiles := LFiles.WithFile(NyxPascalPhysicalFile('main.pas'),
    '{$mode delphi}{$H+}{$modeswitch nestedprocvars}{$modeswitch anonymousfunctions}' +
    '{$ifopt H+}Configured{$endif}');
  LUnknown := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('main.pas'), NativeFacts, LOptions);
  LAt := WordAt(LUnknown, 'Configured');
  Check(LUnknown.Resolved and
    (LUnknown.State(LUnknown.Token(LAt).State).Feature(ppfAnonymousFunctions) = ppkTrue) and
    (LUnknown.State(LUnknown.Token(LAt).State).Feature(ppfNestedProcVars) = ppkTrue),
    'Known Delphi source configuration retains typed callback features and option updates');
  LFiles := LFiles.WithFile(NyxPascalPhysicalFile('main.pas'),
    '{$nestedcomments on} Following {$define UNSAFE} Tail');
  LUnknown := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('main.pas'), NativeFacts, LOptions);
  Check(HasIssue(LUnknown, ptiLexicalPolicyChange) and
    (LUnknown.Token(WordAt(LUnknown, 'Following')).Activity = ppkUnknown) and
    (LUnknown.State(LUnknown.StateCount - 1).Symbol(NyxPascalDefine('UNSAFE')).IsDefined = ppkUnknown),
    'Unsupported lexical transitions stop interpreting the remaining directives');
  LFiles := LFiles.WithFile(NyxPascalPhysicalFile('main.pas'), '{$else} Unbalanced');
  LUnknown := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('main.pas'), NativeFacts, LOptions);
  Check(HasIssue(LUnknown, ptiUnbalancedConditional), 'Unmatched branch boundaries remain visible');
  LFiles := LFiles.WithFile(NyxPascalPhysicalFile('main.pas'),
    '{$ifdef FPC}NativePossibility{$else}OtherPossibility{$endif}OrdinarySource');
  LUnknown := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('main.pas'),
    NewNyxPascalCompilerContext(ppcUnknown,
      TNyxPascalCompilerVersion.Create(0, 0, 0), ppmUnknown), LOptions);
  Check((LUnknown.InitialContext.Family = ppcUnknown) and not LUnknown.Resolved and
    (LUnknown.Token(WordAt(LUnknown, 'NativePossibility')).Activity = ppkUnknown) and
    (LUnknown.Token(WordAt(LUnknown, 'OrdinarySource')).Activity = ppkTrue),
    'Unconfigured output/compiler context retains source inspection with explicit unknown branches');
  LFiles := LFiles.WithFile(NyxPascalPhysicalFile('main.pas'),
    '{$ifdef invalid/name}Maybe{$else}Other{$endif}AfterBroken');
  LUnknown := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('main.pas'), NativeFacts, LOptions);
  Check(HasIssue(LUnknown, ptiUnknownCondition) and
    not HasIssue(LUnknown, ptiUnbalancedConditional) and
    (LUnknown.Token(WordAt(LUnknown, 'Maybe')).Activity = ppkUnknown) and
    (LUnknown.Token(WordAt(LUnknown, 'Other')).Activity = ppkUnknown) and
    (LUnknown.Token(WordAt(LUnknown, 'AfterBroken')).Activity = ppkTrue),
    'A malformed condition retains its own frame without consuming another boundary');

  LRejected := False;
  try
    NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('main.pas'), NativeFacts, LOptions.UpToTokens(1));
  except
    on ENyxPascalTranslationLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and LTranslation.Resolved, 'Token budget refuses atomically without altering an older translation');
  LFiles := LFiles.WithFile(NyxPascalPhysicalFile('main.pas'), '{$define A}A');
  LRejected := False;
  try
    NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('main.pas'), NativeFacts, LOptions.UpToStates(1));
  except
    on ENyxPascalTranslationLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Compiler-state budgets are independent from token budgets');
  LFiles := LFiles.WithFile(NyxPascalPhysicalFile('main.pas'), '{$I leaf.inc}')
    .WithFile(NyxPascalPhysicalFile('leaf.inc'), 'Leaf');
  LRejected := False;
  try
    NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('main.pas'), NativeFacts,
      LOptions.UpToIncludeDepth(1));
  except
    on ENyxPascalTranslationLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Include depth refuses before recursive stack ownership grows');
  LRejected := False;
  try
    NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('main.pas'), NativeFacts,
      LOptions.UpToOccurrences(1));
  except
    on ENyxPascalTranslationLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Occurrence budget counts the root and repeated inclusions independently');
  LRejected := False;
  try
    NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('main.pas'), NativeFacts,
      LOptions.UpToExpandedBytes(Length('{$I leaf.inc}')));
  except
    on ENyxPascalTranslationLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Expanded byte limits include the physical source of included fragments');
  LRejected := False;
  try
    NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('main.pas'),
      NativeFacts.WithFeature(ppfNestedComments, ppkTrue), LOptions);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Conflicting captured lexical facts refuse before analysis');
  LFiles := LFiles.WithFile(NyxPascalPhysicalFile('main.pas'), '? ?');
  LRejected := False;
  try
    NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('main.pas'), NativeFacts,
      LOptions.UpToDiagnostics(1));
  except
    on ENyxPascalTranslationLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Excess diagnostics refuse without silently discarding context gaps');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LRejected := False;
  try
    NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('main.pas'), NativeFacts, LOptions, LCancel);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Pre-cancellation publishes no translated candidate');
  LFiles := LFiles.WithFile(NyxPascalPhysicalFile('main.pas'), '{' + TNyxText(StringOfChar('x', 32768)) + '}');
  LPoller := TPolledCancellation.Create;
  LCancel := LPoller;
  LRejected := False;
  try
    NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('main.pas'), NativeFacts, LOptions, LCancel);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LPoller.Polls > 7), 'Cancellation is observed inside a long physical token');
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

procedure ActualCallbackGate;
var
  LFiles: INyxPascalFileCatalog;
  LTranslation: INyxPascalTranslation;
  LContext: INyxPascalCompilerContext;
  LFinal: INyxPascalCompilerContext;
begin

  if ParamCount = 0 then
  begin
    Exit;
  end;
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('callback-owner.pas'),
      '{$mode delphi}{$I nyx.callback.features.inc} CallbackSite')
    .WithFile(NyxPascalPhysicalFile('nyx.callback.features.inc'),
      ReadText(IncludeTrailingPathDelimiter(ParamStr(1)) + 'src/nyx.callback.features.inc'));
  LTranslation := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('callback-owner.pas'),
    NativeFacts, TNyxPascalTranslationOptions.Defaults);
  Validate(LTranslation);
  LFinal := LTranslation.State(LTranslation.Token(WordAt(LTranslation, 'CallbackSite')).State);
  Check(LTranslation.Resolved and
    ((LFinal.Symbol(NyxPascalDefine('NYX_HAS_INLINE_CALLBACKS')).IsDefined = ppkTrue) = CCompilerInline) and
    (LFinal.Feature(ppfNestedProcVars) = ppkTrue) and
    (LFinal.Feature(ppfFunctionReferences) = ppkTrue) and
    (LFinal.Feature(ppfAnonymousFunctions) = ppkTrue),
    'The actual current Nyx callback include agrees with native compiler gates and typed features');
  LTranslation := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('callback-owner.pas'),
    NativeFacts(30202), TNyxPascalTranslationOptions.Defaults);
  LFinal := LTranslation.State(LTranslation.StateCount - 1);
  Check(LTranslation.Resolved and
    (LFinal.Symbol(NyxPascalDefine('NYX_HAS_INLINE_CALLBACKS')).IsDefined = ppkFalse) and
    (LFinal.Feature(ppfNestedProcVars) = ppkTrue) and
    (LFinal.Feature(ppfAnonymousFunctions) = ppkUnknown),
    'A captured older compiler version retains nested callbacks without inventing inline support');
  LContext := NewNyxPascalCompilerContext(ppcPas2JS,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi)
    .WithSymbol(NyxPascalDefine('PAS2JS'), ppdDefined)
    .WithSymbol(NyxPascalDefine('PAS2JS_FULLVERSION'), ppdInteger, 30301)
    .CompleteDefines(True);
  LTranslation := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('callback-owner.pas'),
    LContext, TNyxPascalTranslationOptions.Defaults);
  LFinal := LTranslation.State(LTranslation.StateCount - 1);
  Check(LTranslation.Resolved and
    (LFinal.Feature(ppfAnonymousFunctions) = ppkTrue) and
    (LFinal.Feature(ppfNestedProcVars) = ppkUnknown),
    'A captured browser profile chooses its own include path independently of the native analyzer build');
  LTranslation := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('callback-owner.pas'),
    LContext.WithSymbol(NyxPascalDefine('NYX_DISABLE_INLINE_CALLBACKS'), ppdDefined),
    TNyxPascalTranslationOptions.Defaults);
  Check(LTranslation.Resolved and
    (LTranslation.State(LTranslation.StateCount - 1).Symbol(
      NyxPascalDefine('NYX_HAS_INLINE_CALLBACKS')).IsDefined = ppkFalse),
    'The real include retains the caller override for disabling inline callbacks');
end;
{$endif}

begin
  try
    Contexts;
    Files;
    Translation;
    {$ifndef PAS2JS}
    ActualCallbackGate;
    {$endif}
    WriteLn('PASS ', GChecks, ' captured-context/translation checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL / ', LException.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
