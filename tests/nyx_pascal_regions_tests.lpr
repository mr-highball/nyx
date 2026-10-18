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
program nyx_pascal_regions_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Classes, nyx.text, nyx.editing, nyx.scheduler, nyx.pascal.lexing,
  nyx.pascal.context, nyx.pascal.files, nyx.pascal.translation,
  nyx.pascal.syntax, nyx.pascal.syntax.translated, lexical.fixture;

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

{ Each fixture selects its compiler family independently of the executing host.
  FPC-scanner observations cannot silently become pas2js compiler knowledge. }
function Translate(const AText: TNyxText; const AInclude: TNyxText = '';
  AFamily: TNyxPascalCompilerFamily = ppcFPC): INyxPascalTranslation;
var
  LFiles: INyxPascalFileCatalog;
  LContext: INyxPascalCompilerContext;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('main.pas'), AText)
    .WithFile(NyxPascalPhysicalFile('body.inc'), AInclude);
  LContext := NewNyxPascalCompilerContext(AFamily,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi)
    .WithOption('I', ppkTrue).WithOption('H', ppkTrue)
    .WithSymbol(NyxPascalDefine('NO'), ppdUndefined);
  Result := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('main.pas'),
    LContext, TNyxPascalTranslationOptions.Defaults);
end;

function StateAt(const ASource: INyxPascalTranslation; const AWord: TNyxText): INyxPascalCompilerContext;
var
  LAt: Integer;
begin
  for LAt := 0 to ASource.Count - 1 do
  begin

    if ASource.Text(LAt) = AWord then
    begin
      Exit(ASource.State(ASource.Token(LAt).State));
    end;
  end;
  raise Exception.Create('Expected an original marker token');
end;

procedure Switches;
var
  LSource: INyxPascalTranslation;
  LState: INyxPascalCompilerContext;
  LAt: Integer;
begin
  Check(CGroupedIO and CGroupedOverflow and CGroupedStrings and CGroupedRange and CGroupedVar,
    'Actual compilation accepts all five switches in the original grouped spelling');
  LSource := Translate('{$push}{$I-,Q-,H-,R-,V-} Changed {$pop} Restored');
  LState := StateAt(LSource, 'Changed');
  Check(LSource.Resolved and (LSource.OccurrenceCount = 1) and
    (LState.Option('I') = ppkFalse) and (LState.Option('Q') = ppkFalse) and
    (LState.Option('H') = ppkFalse) and (LState.Option('R') = ppkFalse) and
    (LState.Option('V') = ppkFalse), 'Grouped I switches are settings, never include filenames');
  Check((StateAt(LSource, 'Restored').Option('I') = ppkTrue) and
    (StateAt(LSource, 'Restored').Option('H') = ppkTrue), 'PUSH/POP retains the pre-group context');
  LSource := Translate('{$i-,q+,h-,i+} Ordered');
  Check(LSource.Resolved and (StateAt(LSource, 'Ordered').Option('I') = ppkTrue) and
    (StateAt(LSource, 'Ordered').Option('Q') = ppkTrue), 'Case and repeated immediate-comma switches retain authored order');
  LSource := Translate('{$I body.inc} Parent', '{$H-,I-} Included');
  Check(LSource.Resolved and (LSource.OccurrenceCount = 2) and
    (StateAt(LSource, 'Parent').Option('H') = ppkFalse), 'Ordinary includes still propagate grouped options');
  LSource := Translate('{$ifdef NO}{$I-,H-}{$endif} Unchanged');
  Check(LSource.Resolved and (StateAt(LSource, 'Unchanged').Option('I') = ppkTrue),
    'Known inactive groups leave the selected context unchanged');
  LSource := Translate('{$if UNKNOWN}{$I-,H-}{$endif} Uncertain');
  Check(not LSource.Resolved and (StateAt(LSource, 'Uncertain').Option('I') = ppkUnknown),
    'Unknown activity revokes switch knowledge without selecting a branch');
  LSource := Translate('{$I-,H-,} InvalidTail');
  Check(not LSource.Resolved and (LSource.Diagnostic(0).Issue = ptiMalformedDirective),
    'A malformed trailing group refuses as a whole');
  for LAt := 0 to LSource.StateCount - 1 do
  begin

    if LSource.State(LAt).Option('I') = ppkFalse then
    begin
      raise Exception.Create('Malformed group published a partial prefix');
    end;
  end;
  Check(True, 'No historical context publishes the rejected prefix');
  LSource := Translate('{$H-,A+} Unsupported');
  Check(not LSource.Resolved and (LSource.DiagnosticCount > 0),
    'Mode-dependent alignment cannot masquerade as a captured Boolean option');
end;

{ Compare source interpretation with independent compiled switch observations.
  Exact annotations remain in physical tokens; unknown branches and include/
  saved-setting lifetimes still use the ordinary immutable translation route. }
procedure AnnotatedSwitches;
var
  LSource: INyxPascalTranslation;
  LPrefix: INyxPascalTranslation;
  LText: TNyxText;
  LJoined: TNyxText;
  LAt: Integer;
  LMarker: Integer;
begin
  {$ifndef PAS2JS}
  Check(CAnnotatedOverflow and CAnnotatedStrings and CCommentCommaIgnored and
    CSpaceEndsGroup and CSpaceCommaIgnored and CAdjacentTailIgnored and
    CAdjacentStringsUnchanged and CAnnotatedGroupIO and CAnnotatedGroupOverflow and
    CAnnotatedGroupTailIgnored, 'Actual compiled IFOPT observes annotation and immediate-comma boundaries');
  {$endif}
  LText := '{$Q+}{$push}{$Q- no overflow checking } Changed {$pop} Restored';
  LSource := Translate(LText);
  Check(LSource.Resolved and (StateAt(LSource, 'Changed').Option('Q') = ppkFalse) and
    (StateAt(LSource, 'Restored').Option('Q') = ppkTrue),
    'The exact RTL annotation applies Q- and PUSH/POP restores its prior value');
  LJoined := '';
  LMarker := -1;
  for LAt := 0 to LSource.Count - 1 do
  begin
    LJoined := LJoined + LSource.Text(LAt);

    if LSource.Text(LAt) = 'Changed' then
    begin
      LMarker := LAt;
    end;
  end;
  Check((LJoined = LText) and (LMarker >= 0), 'All original annotation and source bytes remain exact');
  LPrefix := NewNyxPascalTranslationPrefix(LSource, LMarker + 1,
    TNyxPascalTranslationOptions.Defaults);
  LSource := nil;
  Check(StateAt(LPrefix, 'Changed').Option('Q') = ppkFalse,
    'A detached bounded prefix retains its annotated context and source lease');
  LSource := Translate('{$Q- note, H- and I- are prose} Marker');
  Check(LSource.Resolved and (StateAt(LSource, 'Marker').Option('Q') = ppkFalse) and
    (StateAt(LSource, 'Marker').Option('H') = ppkTrue) and
    (StateAt(LSource, 'Marker').Option('I') = ppkTrue), 'A comma inside explanatory text cannot resume a switch group');
  LSource := Translate('{$I- ,H-} Marker');
  Check(LSource.Resolved and (StateAt(LSource, 'Marker').Option('I') = ppkFalse) and
    (StateAt(LSource, 'Marker').Option('H') = ppkTrue), 'Whitespace before a comma already ended the group');
  LSource := Translate('{$I-H-} Marker');
  Check(LSource.Resolved and (StateAt(LSource, 'Marker').Option('I') = ppkFalse) and
    (StateAt(LSource, 'Marker').Option('H') = ppkTrue), 'An adjacent non-comma tail stays comment text');
  LSource := Translate('{$I-,Q- note, H-} Marker');
  Check(LSource.Resolved and (StateAt(LSource, 'Marker').Option('I') = ppkFalse) and
    (StateAt(LSource, 'Marker').Option('Q') = ppkFalse) and
    (StateAt(LSource, 'Marker').Option('H') = ppkTrue), 'A complete group may end with an annotation');
  LSource := Translate('{$I-,' + #9 + 'H-} Invalid');
  Check(not LSource.Resolved and (LSource.Diagnostic(0).Issue = ptiMalformedDirective),
    'Whitespace after a committed comma is not another short switch');
  for LAt := 0 to LSource.StateCount - 1 do
  begin

    if LSource.State(LAt).Option('I') = ppkFalse then
    begin
      raise Exception.Create('An invalid whitespace group published its prefix');
    end;
  end;
  Check(True, 'A malformed committed group retains atomic historical states');
  LSource := Translate('{$I-,H} Invalid');
  Check(not LSource.Resolved and (LSource.Diagnostic(0).Issue = ptiMalformedDirective),
    'A committed final letter still requires its sign');
  LSource := Translate('{$ifdef NO}{$I- annotation, H-}{$endif} Marker');
  Check(LSource.Resolved and (StateAt(LSource, 'Marker').Option('I') = ppkTrue),
    'Known inactive annotations have no setting effect');
  LSource := Translate('{$if UNKNOWN}{$I- annotation, H-}{$endif} Marker');
  Check(not LSource.Resolved and (StateAt(LSource, 'Marker').Option('I') = ppkUnknown) and
    (StateAt(LSource, 'Marker').Option('H') = ppkTrue), 'Unknown activity affects the switch, never names in its annotation');
  LSource := Translate('{$I body.inc} Parent', '{$I- note, H-} Included');
  Check(LSource.Resolved and (LSource.OccurrenceCount = 2) and
    (StateAt(LSource, 'Parent').Option('I') = ppkFalse) and
    (StateAt(LSource, 'Parent').Option('H') = ppkTrue), 'An annotated I switch is not an include and its value propagates outward');
  LSource := Translate('(*$Q- ' + NyxScalarText($1F680) + ', H-*) Marker');
  Check(LSource.Resolved and (StateAt(LSource, 'Marker').Option('Q') = ppkFalse) and
    (StateAt(LSource, 'Marker').Option('H') = ppkTrue), 'Alternative comment delimiters and supplementary annotation text retain the same boundary');
  LSource := Translate('{$Q- annotation} Marker', '', ppcPas2JS);
  Check(not LSource.Resolved and (LSource.Diagnostic(0).Issue = ptiMalformedDirective),
    'A different compiler family cannot inherit FPC annotation knowledge');
  LSource := Translate('{$I-, H-} Marker', '', ppcPas2JS);
  Check(LSource.Resolved and (StateAt(LSource, 'Marker').Option('I') = ppkFalse) and
    (StateAt(LSource, 'Marker').Option('H') = ppkFalse), 'Other families retain their existing strict separated-group contract');
end;

procedure Regions;
var
  LLexical: INyxPascalLexicalSnapshot;
  LSource: INyxPascalTranslation;
  LSyntax: INyxPascalTranslatedSyntax;
  LText: TNyxText;
  LJoined: TNyxText;
  LAt: Integer;
  LFound: Boolean;
  LCancel: INyxCancellationScope;
  LRefused: Boolean;
begin
  Check(TwiceLexicalValue(21) = 42, 'The configured native compiler executes the actual assembly operands');
  LText := 'asm movl %eax,%ecx; db "end",''end''; endbr32; // end' + #13#10 +
    '{end} (*end*) movl $1,%eax end; const Binary=%101;';
  LLexical := NewNyxPascalLexicalSnapshot(LText);
  Check(LLexical.DiagnosticCount = 0, 'Opaque assembly operands never become Pascal literal errors');
  LJoined := '';
  LFound := False;
  for LAt := 0 to LLexical.Count - 1 do
  begin
    LJoined := LJoined + LLexical.Text(LAt);

    if LLexical.Text(LAt) = 'Binary' then
    begin
      LFound := (LLexical.Token(LAt).Kind = ptkWord) and
        (LLexical.Token(LAt).Region = plrPascal);
    end;
  end;
  Check((LJoined = LText) and (LLexical.Scalars = NyxTextScalarCount(LText)) and LFound,
    'Exact assembly trivia, line endings and following Pascal token positions survive');
  LLexical := NewNyxPascalLexicalSnapshot('asm db "quoted\" end"; end;');
  Check(LLexical.DiagnosticCount = 0, 'An escaped assembly quote cannot expose a false END');
  LLexical := NewNyxPascalLexicalSnapshot('const Wrong = %eax;');
  Check(LLexical.DiagnosticCount > 0, 'Pascal outside ASM still rejects invalid binary digits');
  LLexical := NewNyxPascalLexicalSnapshot('asm movl %eax,%ecx');
  Check((LLexical.DiagnosticCount = 1) and
    (LLexical.Diagnostic(0).Issue = pliUnterminatedAssembly), 'An unfinished standalone assembly region remains explicit');
  LSource := Translate('procedure Run; begin asm {$I body.inc} end; end;', 'movl %eax,%ecx');
  LSyntax := NewNyxPascalTranslatedSyntax(LSource, TNyxPascalSyntaxOptions.Defaults);
  Check(LSource.Resolved and (LSource.OccurrenceCount = 2) and
    (LSyntax.Syntax.DiagnosticCount = 0), 'Included operands share the live region and preserve the ordinary body scope');
  LSource := Translate('procedure Run; begin {$I body.inc} movl %eax,%ecx end; end;', 'asm');
  Check(LSource.Resolved, 'A textual include may open assembly for its caller');
  LSource := Translate('procedure Run; begin asm movl %eax,%ecx {$I body.inc} end;', 'end;');
  Check(LSource.Resolved, 'A textual include may close its caller assembly region');
  LSource := Translate('{$ifdef NO}asm movl %eax,%ecx{$endif} const Good=%101;');
  Check(LSource.Resolved and (StateAt(LSource, 'Good').Option('I') = ppkTrue),
    'An inactive unclosed ASM cannot alter the selected Pascal envelope');
  LSource := Translate('asm {$ifdef NO}end{$else}movl %eax,%ecx{$endif} end;');
  Check(LSource.Resolved, 'An inactive END cannot terminate the selected assembly envelope');
  LSource := Translate('{$ifdef NO}asm{$elseif 1=1}asm movl %eax,%ecx' +
    '{$else}end{$endif} end; const After=%1;');
  Check(LSource.Resolved, 'ELSEIF selects one outgoing region while later discarded branches stay isolated');
  LSource := Translate('{$ifdef NO}asm {$ifdef NO}end{$endif}movl %eax,%ecx{$endif} const After=%1;');
  Check(LSource.Resolved, 'Nested inactive conditionals restore their own physical entry region');
  LSource := Translate('{$if UNKNOWN}asm{$endif} movl %eax,%ecx end;');
  Check(not LSource.Resolved, 'Unknown conditional region changes never establish resolved source');
  LSource := Translate('asm movl %eax,%ecx');
  Check(not LSource.Resolved and (LSource.DiagnosticCount = 1),
    'The translator diagnoses one complete-root assembly EOF rather than include fragments');
  LLexical := NewNyxPascalLexicalSnapshot(TNyxText('asm db "Rocket 🚀" end [''EAX'']; LabelAfter'));
  LAt := LLexical.TokenAt(NyxTextScalarCount(TNyxText('asm db "Rocket 🚀" end [''EAX'']; ')));
  Check((LLexical.DiagnosticCount = 0) and (LLexical.Text(LAt) = TNyxText('LabelAfter')) and
    (LLexical.Token(LAt).Region = plrPascal), 'Supplementary text and trailing changed-register lists retain scalar positions');
  LRefused := False;
  try
    LLexical := NewNyxPascalLexicalSnapshot(LText, TNyxPascalLexicalOptions.Defaults.UpToTokens(4));
  except
    on ENyxPascalLexicalLimit do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'Opaque assembly still obeys the original token quota atomically');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LRefused := False;
  try
    LLexical := NewNyxPascalLexicalSnapshot(LText, TNyxPascalLexicalOptions.Defaults, LCancel);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'Region scanning preserves atomic cooperative cancellation');
end;

begin
  try
    Switches;
    AnnotatedSwitches;
    Regions;
    WriteLn('PASS ', GChecks, ' compiler switch and assembly region checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
