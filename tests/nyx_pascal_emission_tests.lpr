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
program nyx_pascal_emission_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.bytes, nyx.scheduler, nyx.pascal.lexing,
  nyx.pascal.context, nyx.pascal.files, nyx.pascal.translation
  {$ifndef PAS2JS}, Classes{$endif};

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

{ Source knowledge is explicit and unrelated to the executing compiler's flags. }
function Facts: INyxPascalCompilerContext;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi).CompleteDefines(True)
    .WithSymbol(NyxPascalDefine('MAYBE'), ppdUnknown)
    .WithSymbol(NyxPascalDefine('NO'), ppdUndefined);
end;

function Translate(const AText: TNyxText; const APart: TNyxText = '';
  const AContext: INyxPascalCompilerContext = nil): INyxPascalTranslation;
var
  LContext: INyxPascalCompilerContext;
begin
  LContext := AContext;

  if LContext = nil then
  begin
    LContext := Facts;
  end;
  Result := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('main.pas'), AText)
    .WithFile(NyxPascalPhysicalFile('part.inc'), APart), NyxPascalPhysicalFile('main.pas'),
    LContext, TNyxPascalTranslationOptions.Defaults);
end;

function MarkerState(const ASource: INyxPascalTranslation;
  const AName: TNyxText): Integer;
var
  LAt: Integer;
begin
  for LAt := 0 to ASource.Count - 1 do
  begin

    if ASource.Text(LAt) = AName then
    begin
      Exit(ASource.Token(LAt).State);
    end;
  end;
  raise Exception.Create('Missing original marker');
end;

function EmissionAt(const ASource: INyxPascalTranslation; const AName: TNyxText;
  AChannel: TNyxPascalDiagnosticChannel): TNyxPascalKnowledge;
begin
  Result := NyxPascalCompilerPolicies(ASource.State(MarkerState(ASource, AName)))
    .DiagnosticEmission(AChannel);
end;

{ Plain scalar policy copies, complete snapshots and saved settings must retain
  independent category requests without interpreting per-warning actions. }
procedure Policies;
var
  LBefore: TNyxPascalCompilerPolicies;
  LAfter: TNyxPascalCompilerPolicies;
  LBase: INyxPascalCompilerContext;
  LChanged: INyxPascalCompilerContext;
  LRestored: INyxPascalCompilerContext;
begin
  LBefore := TNyxPascalCompilerPolicies.Unknown;
  Check((LBefore.DiagnosticEmission(pdcWarnings) = ppkUnknown) and
    (LBefore.DiagnosticEmission(pdcHints) = ppkUnknown) and
    (LBefore.DiagnosticEmission(pdcNotes) = ppkUnknown), 'Zero initialization fabricates no verbosity defaults');
  LBefore := LBefore.EmitDiagnostics(pdcWarnings, ppkFalse)
    .EmitDiagnostics(pdcHints, ppkTrue).EmitDiagnostics(pdcNotes, ppkFalse);
  LAfter := LBefore.EmitDiagnostics(pdcWarnings, ppkTrue);
  Check((LBefore.DiagnosticEmission(pdcWarnings) = ppkFalse) and
    (LAfter.DiagnosticEmission(pdcWarnings) = ppkTrue) and
    (LAfter.DiagnosticEmission(pdcHints) = ppkTrue), 'Scalar fluent copies have independent category values');
  Check(not LBefore.SamePolicies(LAfter), 'Category requests participate in exact policy identity');
  LBase := NyxPascalWithCompilerPolicies(Facts, LBefore);
  Check(CaptureNyxPascalCompilerContext(LBase).SameFacts(LBase), 'Complete capture retains every category');
  LChanged := NyxPascalWithCompilerPolicies(LBase, LAfter);
  Check(not LBase.SameFacts(LChanged), 'Exact context identity includes category requests');
  LChanged := LChanged.InMode(ppmObjFPC);
  Check(NyxPascalCompilerPolicies(LChanged).SamePolicies(LAfter),
    'Mode changes retain diagnostic verbosity without inferring language defaults');
  LChanged := LChanged.WithSymbol(NyxPascalDefine('LATER'), ppdDefined);
  LChanged := NyxPascalWithWarning(LChanged, NyxPascalWarningNumber(3018), pwaError);
  LRestored := RestoreNyxPascalLocalSettings(LChanged, LBase);
  Check((NyxPascalCompilerPolicies(LRestored).DiagnosticEmission(pdcWarnings) = ppkFalse) and
    (LRestored.Symbol(NyxPascalDefine('LATER')).IsDefined = ppkTrue), 'Local restore preserves later definitions and saved categories');
  Check((NyxPascalWarnings(LRestored).WarningCount = 0) and
    (NyxPascalWarnings(LChanged).WarningCount = 1), 'Category and per-message restoration retain independent inputs');
  Check(NyxPascalCompilerPolicies(LBase.UnknownState).DiagnosticEmission(pdcHints) = ppkUnknown,
    'Unknown side effects revoke category knowledge');
  Check(NyxPascalCompilerPolicies(CaptureNyxPascalCompilerContext(Facts))
    .DiagnosticEmission(pdcWarnings) = ppkUnknown, 'Uncaptured compiler defaults remain unknown');
end;

{ Includes, historical frames and detached prefixes qualify source meaning and
  ownership rather than mirroring only the policy setter's implementation. }
procedure SourceAndReceipts;
var
  LSource: INyxPascalTranslation;
  LPrefix: INyxPascalTranslation;
  LRows: INyxPascalEmissionDirectives;
  LPrefixRows: INyxPascalEmissionDirectives;
  LSettings: INyxPascalTranslationSettings;
  LBase: INyxPascalCompilerContext;
  LPolicies: TNyxPascalCompilerPolicies;
  LRow: TNyxPascalEmissionDirective;
  LInside: Integer;
  LRejected: Boolean;
  LText: TNyxText;
begin
  LPolicies := TNyxPascalCompilerPolicies.Unknown.EmitDiagnostics(pdcWarnings, ppkTrue)
    .EmitDiagnostics(pdcHints, ppkTrue).EmitDiagnostics(pdcNotes, ppkTrue);
  LBase := NyxPascalWithCompilerPolicies(Facts, LPolicies);
  LSource := Translate('Before {$push}{$i part.inc} Inside {$pop} After',
    '{$warnings off explanation, on stays text}{$hints-}{$notes off}', LBase);
  Check(LSource.Resolved and Supports(LSource, INyxPascalEmissionDirectives, LRows) and
    (LRows.EmissionDirectiveCount = 3), 'All categories retain independent included source receipts');
  Check((EmissionAt(LSource, 'Inside', pdcWarnings) = ppkFalse) and
    (EmissionAt(LSource, 'Inside', pdcHints) = ppkFalse) and
    (EmissionAt(LSource, 'Inside', pdcNotes) = ppkFalse), 'Included category toggles affect their parent context');
  Check((EmissionAt(LSource, 'Before', pdcWarnings) = ppkTrue) and
    (EmissionAt(LSource, 'After', pdcWarnings) = ppkTrue) and
    (EmissionAt(LSource, 'After', pdcNotes) = ppkTrue), 'PUSH/POP preserves original and restored category values');
  Supports(LSource, INyxPascalTranslationSettings, LSettings);
  LInside := MarkerState(LSource, 'Inside');
  Check((LSettings.SettingsDepth(LInside) = 1) and
    (NyxPascalCompilerPolicies(LSettings.SavedSettings(LInside, 0))
      .DiagnosticEmission(pdcHints) = ppkTrue), 'Historical saved frames retain their original verbosity');
  LRow := LRows.EmissionDirectiveAt(0);
  Check((LRow.Channel = pdcWarnings) and not LRow.Enabled and (LRow.Activity = ppkTrue) and
    (LRow.Origin.FileReference.Name = TNyxText('part.inc')) and
    (NyxPascalOccurrenceLexical(LSource, LRow.Origin.Occurrence).Slice(LRow.Origin.Span) =
      TNyxText('{$warnings off explanation, on stays text}')), 'Authored emission retains the complete original annotation and occurrence');
  Check(NyxPascalCompilerPolicies(LSource.State(LRow.State)).DiagnosticEmission(pdcWarnings) = ppkTrue,
    'Receipt state precedes the authored toggle');
  LPrefix := NewNyxPascalTranslationPrefix(LSource, LRows.EmissionDirectiveAt(1).Origin.PrefixCount,
    TNyxPascalTranslationOptions.Defaults);
  Check(Supports(LPrefix, INyxPascalEmissionDirectives, LPrefixRows) and
    (LPrefixRows.EmissionDirectiveCount = 1), 'A bounded prefix excludes its current and later emission requests');
  LSource := nil;
  LSettings := nil;
  LRows := nil;
  Check((LPrefixRows.EmissionDirectiveAt(0).Origin.FileReference.Name = TNyxText('part.inc')) and
    (LPrefixRows.EmissionDirectiveAt(0).Channel = pdcWarnings), 'Detached prefix receipts preserve original ownership');
  LRejected := False;
  try
    LPrefixRows.EmissionDirectiveAt(1);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Emission receipt ordinals refuse outside the retained prefix');
  LRejected := False;
  try
    LPrefixRows.EmissionDirectiveAt(-1);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Negative emission ordinals refuse before an array access');
  LText := '{$warnings off ' + NyxScalarText($1F680) + ', notes stay unchanged}';
  LSource := Translate(LText + TNyxText(' Marker'));
  Supports(LSource, INyxPascalEmissionDirectives, LRows);
  LRow := LRows.EmissionDirectiveAt(0);
  Check(LSource.Resolved and (NyxPascalOccurrenceLexical(LSource, 0).Slice(LRow.Origin.Span) = LText) and
    (EmissionAt(LSource, 'Marker', pdcWarnings) = ppkFalse) and
    (EmissionAt(LSource, 'Marker', pdcNotes) = ppkUnknown), 'Supplementary annotations retain exact scalar spans and unrelated unknown defaults');
  LSource := Translate('{$i part.inc}{$i part.inc} Marker', '{$notes off}');
  Supports(LSource, INyxPascalEmissionDirectives, LRows);
  Check(LSource.Resolved and (LRows.EmissionDirectiveCount = 2) and
    (LRows.EmissionDirectiveAt(0).Origin.Occurrence <> LRows.EmissionDirectiveAt(1).Origin.Occurrence),
    'Repeated includes retain separate authored occurrences even for equal requests');
  LSource := Translate('{$warnings off}{$mode objfpc} Marker');
  Check(LSource.Resolved and (EmissionAt(LSource, 'Marker', pdcWarnings) = ppkFalse),
    'An actual source mode directive retains prior category intent');
  LSource := Translate('{$warnings on}{$i part.inc} Marker', '{$warnings off}');
  Check(LSource.Resolved and (EmissionAt(LSource, 'Marker', pdcWarnings) = ppkFalse),
    'Ordinary include propagation does not invent an implicit restore');
end;

{ Uncertainty and malformed/unsupported source must remain visible even when the
  authored compiler warning category is off. Construction failures publish nil. }
procedure RefusalAndActivity;
var
  LSource: INyxPascalTranslation;
  LRows: INyxPascalEmissionDirectives;
  LCancel: INyxCancellationScope;
  LRejected: Boolean;
begin
  LSource := Translate('{$warnings on}{$hints on}{$if MAYBE}{$warnings off}{$endif} Marker');
  Supports(LSource, INyxPascalEmissionDirectives, LRows);
  Check(not LSource.Resolved and (EmissionAt(LSource, 'Marker', pdcWarnings) = ppkUnknown) and
    (EmissionAt(LSource, 'Marker', pdcHints) = ppkTrue), 'Uncertain activity revokes only the written category');
  Check((LRows.EmissionDirectiveAt(2).Activity = ppkUnknown) and
    not LRows.EmissionDirectiveAt(2).Enabled, 'Uncertainty retains the authored disabled request separately');
  LSource := Translate('{$ifdef NO}{$warnings offjunk}{$endif} Marker');
  Supports(LSource, INyxPascalEmissionDirectives, LRows);
  Check(LSource.Resolved and (LRows.EmissionDirectiveCount = 0), 'Inactive malformed text creates no category effect or receipt');
  LSource := Translate('{$warnings offjunk} Marker');
  Supports(LSource, INyxPascalEmissionDirectives, LRows);
  Check(not LSource.Resolved and (LRows.EmissionDirectiveCount = 0) and
    (LSource.Diagnostic(0).Issue = ptiMalformedDirective), 'A complete unsupported identifier cannot masquerade as OFF');
  LSource := Translate('{$notes default} Marker');
  Check(not LSource.Resolved and (LSource.Diagnostic(0).Issue = ptiMalformedDirective),
    'DEFAULT cannot fabricate a captured verbosity default');
  LSource := Translate('{$warnings off}{$unknown_feature} Marker');
  Check(not LSource.Resolved and (LSource.DiagnosticCount > 0) and
    (EmissionAt(LSource, 'Marker', pdcWarnings) = ppkUnknown), 'Compiler verbosity cannot suppress analyzer diagnostics or unknown effects');
  LSource := Translate('{$warnings off} Marker', '', NewNyxPascalCompilerContext(ppcPas2JS,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi));
  Check(not LSource.Resolved and (LSource.Diagnostic(0).Issue = ptiUnsupportedDirective),
    'A different compiler family cannot inherit FPC source-toggle semantics');
  LSource := nil;
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LRejected := False;
  try
    LSource := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
      .WithFile(NyxPascalPhysicalFile('main.pas'), '{$warnings off}'),
      NyxPascalPhysicalFile('main.pas'), Facts, TNyxPascalTranslationOptions.Defaults, LCancel);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LSource = nil), 'Cancelled category construction publishes no partial translation');
  LRejected := False;
  try
    LSource := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
      .WithFile(NyxPascalPhysicalFile('main.pas'), '{$warnings off}'),
      NyxPascalPhysicalFile('main.pas'), Facts, TNyxPascalTranslationOptions.Defaults.UpToStates(1));
  except
    on ENyxPascalTranslationLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LSource = nil), 'State quotas refuse before publishing a partial category history');
end;

{$ifndef PAS2JS}
{ Consume the independently compiled fixture's exact UTF-8 transcript. The shell
  only orchestrates compilation; these semantic assertions live in Pascal. The
  fixture itself is never executed, and its one visible warning is deliberate. }
procedure NativeOracle(const APath: String);
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
  LText: TNyxText;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try
    Check((LStream.Size > 0) and (LStream.Size < 1024 * 1024), 'Independent compiler transcript is bounded');
    SetLength(LBytes, LStream.Size);
    LStream.ReadBuffer(LBytes[0], Length(LBytes));
  finally
    LStream.Free;
  end;
  LText := NyxDecodeUTF8(LBytes);
  Check(Pos('NYX_EMISSION_HIDDEN_', LText) = 0, 'Actual FPC suppresses all categories and retains suppression across a mode change');
  Check((Pos('NYX_EMISSION_VISIBLE_WARNING', LText) > 0) and
    (Pos('NYX_EMISSION_VISIBLE_HINT', LText) > 0) and
    (Pos('NYX_EMISSION_VISIBLE_NOTE', LText) > 0), 'Actual FPC POP restores all three deliberately visible categories');
end;
{$endif}

begin
  try
    Policies;
    SourceAndReceipts;
    RefusalAndActivity;
    {$ifndef PAS2JS}
    Check(ParamCount = 1, 'Supply the independently compiled native fixture transcript');
    NativeOracle(ParamStr(1));
    {$endif}
    WriteLn('PASS ', GChecks, ' typed diagnostic emission checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
