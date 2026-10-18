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
program nyx_pascal_policy_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.pascal.context, nyx.pascal.files,
  nyx.pascal.lexing, nyx.pascal.translation, nyx.pascal.macros;

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
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi)
    .CompleteDefines(True).WithSymbol(NyxPascalDefine('UNKNOWN'), ppdUnknown);
end;

procedure ValuesAndRestore;
var
  LBefore: INyxPascalCompilerContext;
  LSaved: INyxPascalCompilerContext;
  LCurrent: INyxPascalCompilerContext;
  LRestored: INyxPascalCompilerContext;
  LPolicies: TNyxPascalCompilerPolicies;
  LRejected: Boolean;
begin
  LBefore := Facts;
  LPolicies := TNyxPascalCompilerPolicies.Unknown.WithInlining(ppkFalse)
    .AlignRecords(pra1).StoreEnums(pes1).CallBy(pccCdecl);
  LSaved := NyxPascalWithCompilerPolicies(LBefore, LPolicies)
    .WithFeature(ppfReplacementMacros, ppkTrue).WithFeature(ppfNestedProcVars, ppkFalse)
    .WithFeature(ppfScopedEnums, ppkFalse).WithOption('D', ppkFalse)
    .WithSymbol(NyxPascalDefine('VALUE'), ppdInteger, 1);
  LSaved := NyxPascalWithLocalSwitch(LSaved, plsLongStrings, ppkTrue);
  LSaved := NyxPascalWithLocalSwitch(LSaved, plsRangeChecks, ppkFalse);
  Check(NyxPascalCompilerPolicies(LBefore).RecordAlignment = praUnknown,
    'Fluent policy changes preserve the parent context');
  Check(CaptureNyxPascalCompilerContext(LSaved).SameFacts(LSaved),
    'Complete context capture preserves optional policies');
  Check(not LBefore.SameFacts(LSaved), 'Policy changes participate in exact snapshot identity');
  Check(NyxPascalCompilerPolicies(LSaved.UnknownState).RecordAlignment = praUnknown,
    'Unknown effects revoke layout policy rather than inventing target defaults');
  Check(NyxPascalCompilerPolicies(LSaved.InMode(ppmObjFPC)).CallingConvention = pccUnknown,
    'An unqualified mode reset cannot retain a previous calling default');
  LCurrent := NyxPascalWithCompilerPolicies(LSaved,
    TNyxPascalCompilerPolicies.Unknown.WithInlining(ppkTrue).AlignRecords(pra8)
      .StoreEnums(pes4).CallBy(pccStdCall))
    .WithOption('H', ppkFalse).WithOption('R', ppkTrue).WithOption('D', ppkTrue)
    .WithFeature(ppfScopedEnums, ppkTrue).WithFeature(ppfNestedProcVars, ppkTrue)
    .WithFeature(ppfReplacementMacros, ppkFalse)
    .WithSymbol(NyxPascalDefine('VALUE'), ppdInteger, 2)
    .WithSymbol(NyxPascalDefine('ADDED'), ppdDefined);
  LRestored := RestoreNyxPascalLocalSettings(LCurrent, LSaved);
  Check(NyxPascalCompilerPolicies(LRestored).SamePolicies(LPolicies),
    'Local restore recovers exact symbolic layout/inlining/calling policies');
  Check((LRestored.Option('H') = ppkTrue) and (LRestored.Option('R') = ppkFalse),
    'Local short switches recover their saved values');
  Check(LRestored.Feature(ppfScopedEnums) = ppkFalse, 'Scoped enumeration policy is local');
  Check((LRestored.Feature(ppfReplacementMacros) = ppkFalse) and
    (LRestored.Feature(ppfNestedProcVars) = ppkTrue) and (LRestored.Option('D') = ppkTrue),
    'Global macro/mode/debug facts survive local restoration');
  Check((LRestored.Symbol(NyxPascalDefine('VALUE')).IntegerValue = 2) and
    (LRestored.Symbol(NyxPascalDefine('ADDED')).IsDefined = ppkTrue),
    'Definitions made after PUSH survive POP');
  Check((NyxPascalCompilerPolicies(LCurrent).RecordAlignment = pra8) and
    (LSaved.Symbol(NyxPascalDefine('VALUE')).IntegerValue = 1),
    'Restoration mutates neither input snapshot');
  LRejected := False;
  LRestored := nil;
  try
    LRestored := RestoreNyxPascalLocalSettings(LCurrent,
      NewNyxPascalCompilerContext(ppcPas2JS, TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi));
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LRestored = nil), 'A foreign compiler frame refuses without a partial restored result');
end;

function Translate(const AText: TNyxText; const APart: TNyxText = ''): INyxPascalTranslation;
begin
  Result := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('main.pas'), AText)
    .WithFile(NyxPascalPhysicalFile('part.inc'), APart), NyxPascalPhysicalFile('main.pas'),
    Facts, TNyxPascalTranslationOptions.Defaults);
end;

function StateAt(const ASource: INyxPascalTranslation; const AWord: TNyxText): Integer;
var
  LAt: Integer;
begin
  for LAt := 0 to ASource.Count - 1 do
  begin

    if ASource.Text(LAt) = AWord then
    begin
      Exit(ASource.Token(LAt).State);
    end;
  end;
  raise Exception.Create('Expected authored state marker was not captured');
end;

procedure SourceScopes;
const
  CMain: TNyxText = 'unit Main; interface {$macro on}{$define VALUE:=11}' +
    '{$packrecords 1}{$packenum 1}{$calling cdecl}{$inline off}{$H+}{$R-}' +
    '{$scopedenums off}{$push}{$I part.inc} var InsideMarker:Integer;' +
    '{$pop} var AfterMarker:Integer; implementation end.';
  CPart: TNyxText = '{$packrecords 8}{$packenum 4}{$calling stdcall}{$inline on}' +
    '{$H-}{$R+}{$scopedenums on}{$define VALUE:=13}{$macro off}';
var
  LSource: INyxPascalTranslation;
  LSettings: INyxPascalTranslationSettings;
  LMacros: INyxPascalTranslationMacros;
  LInside: Integer;
  LAfter: Integer;
  LState: INyxPascalCompilerContext;
  LPolicy: TNyxPascalCompilerPolicies;
  LRejected: Boolean;
begin
  LSource := Translate(CMain, CPart);
  Check(LSource.Resolved, 'A complete cross-include settings scope remains interpretable');
  Supports(LSource, INyxPascalTranslationSettings, LSettings);
  Supports(LSource, INyxPascalTranslationMacros, LMacros);
  LInside := StateAt(LSource, 'InsideMarker');
  LAfter := StateAt(LSource, 'AfterMarker');
  Check((LSettings.SettingsDepth(LInside) = 1) and (LSettings.SettingsDepth(LAfter) = 0),
    'Historical stacks keep independent shape before and after POP');
  LPolicy := NyxPascalCompilerPolicies(LSource.State(LInside));
  Check((LPolicy.RecordAlignment = pra8) and (LPolicy.EnumStorage = pes4) and
    (LPolicy.CallingConvention = pccStdCall) and (LPolicy.Inlining = ppkTrue),
    'An included body supplies all supported typed local policies');
  LPolicy := NyxPascalCompilerPolicies(LSource.State(LAfter));
  Check((LPolicy.RecordAlignment = pra1) and (LPolicy.EnumStorage = pes1) and
    (LPolicy.CallingConvention = pccCdecl) and (LPolicy.Inlining = ppkFalse),
    'POP restores the parent policy set after the included body');
  LState := LSource.State(LAfter);
  Check((LState.Option('H') = ppkTrue) and (LState.Option('R') = ppkFalse) and
    (LState.Feature(ppfScopedEnums) = ppkFalse), 'Local short and enum policies restore together');
  Check((LState.Feature(ppfReplacementMacros) = ppkFalse) and
    (LMacros.StateMacros(LAfter).Definition(NyxPascalDefine('VALUE')).Replacement = TNyxText('13')),
    'Global macro enablement and replacement edits survive source POP');
  Check(NyxPascalCompilerPolicies(LSettings.SavedSettings(LInside, 0)).RecordAlignment = pra1,
    'Retained frames expose the exact saved immutable context');
  Check((LSettings.SettingsOrigin(LInside, 0).Occurrence = 0) and
    (NyxPascalOccurrenceLexical(LSource, 0).Slice(LSettings.SettingsOrigin(LInside, 0).Span) = TNyxText('{$push}')),
    'Stack entries retain their physical PUSH origin');
  LRejected := False;
  try
    LSettings.SavedSettings(LAfter, 0);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'A popped frame is not exposed by a later historical state');
  LSource := nil;
  Check(LSettings.SettingsDepth(LInside) = 1, 'The optional state owner remains valid after its other interface retires');
  LSource := Translate('unit Main; interface var BeforeMarker:Integer; {$push}' +
    'var SameMarker:Integer; {$pop} var EndMarker:Integer; implementation end.');
  Supports(LSource, INyxPascalTranslationSettings, LSettings);
  Check(LSource.Resolved and (StateAt(LSource, 'BeforeMarker') <> StateAt(LSource, 'SameMarker')) and
    LSource.State(StateAt(LSource, 'BeforeMarker')).SameFacts(LSource.State(StateAt(LSource, 'SameMarker'))),
    'A changed stack cannot coalesce states with identical current option facts');
  LSource := Translate('unit Main; interface {$push}{$I part.inc} var Marker:Integer; implementation end.', '{$pop}');
  Supports(LSource, INyxPascalTranslationSettings, LSettings);
  Check(LSource.Resolved and (LSettings.SettingsDepth(StateAt(LSource, 'Marker')) = 0),
    'A nested include can consume a settings frame opened by its caller');
  LSource := Translate('unit Main; {$push}{$modeswitch nestedprocvars}{$pop} interface var Marker:Integer; implementation end.');
  Check(LSource.Resolved and
    (LSource.State(StateAt(LSource, 'Marker')).Feature(ppfNestedProcVars) = ppkTrue),
    'Global grammar switches survive the local frame, matching the compiler oracle');
end;

procedure AliasesAndRefusals;
var
  LSource: INyxPascalTranslation;
  LSettings: INyxPascalTranslationSettings;
  LPolicy: TNyxPascalCompilerPolicies;
  LRejected: Boolean;
begin
  LSource := Translate('unit Main; interface {$ALIGN 4}{$A8}{$MINENUMSIZE 2}{$Z+}' +
    '{$rangechecks on}{$overflowchecks off}{$calling default}{$inline on} var Marker:Integer; implementation end.');
  LPolicy := NyxPascalCompilerPolicies(LSource.State(StateAt(LSource, 'Marker')));
  Check(LSource.Resolved and (LPolicy.RecordAlignment = pra8) and (LPolicy.EnumStorage = pes4) and
    (LPolicy.CallingConvention = pccDefault), 'Documented aliases preserve explicit symbolic policy choices');
  Check((LSource.State(StateAt(LSource, 'Marker')).Option('R') = ppkTrue) and
    (LSource.State(StateAt(LSource, 'Marker')).Option('Q') = ppkFalse),
    'Long Boolean spellings share the exact short-switch facts used by IFOPT');
  LSource := Translate('unit Main; interface {$packrecords C}{$packenum default} var Marker:Integer; implementation end.');
  LPolicy := NyxPascalCompilerPolicies(LSource.State(StateAt(LSource, 'Marker')));
  Check(LSource.Resolved and (LPolicy.RecordAlignment = praC) and (LPolicy.EnumStorage = pesDefault),
    'Target-dependent C/default intent is retained without invented byte sizes');
  LSource := Translate('unit Main; interface {$packrecords 4}{$if UNKNOWN}{$packrecords 8}{$endif}' +
    'var Marker:Integer; implementation end.');
  Check(not LSource.Resolved and
    (NyxPascalCompilerPolicies(LSource.State(StateAt(LSource, 'Marker'))).RecordAlignment = praUnknown),
    'An uncertain write revokes only its policy meaning instead of selecting a branch');
  LSource := Translate('unit Main; interface {$if UNKNOWN}{$push}{$endif} implementation end.');
  Check(not LSource.Resolved and (LSource.Diagnostic(1).Issue = ptiCompilerSettings),
    'An uncertain stack shape stops certainty rather than guessing a matching POP');
  LSource := Translate('unit Main; interface {$pop} implementation end.');
  Check(not LSource.Resolved and (LSource.Diagnostic(0).Issue = ptiCompilerSettings),
    'A missing inherited settings frame is a context gap');
  LSource := Translate('unit Main; interface {$push} implementation end.');
  Supports(LSource, INyxPascalTranslationSettings, LSettings);
  Check(not LSource.Resolved and (LSettings.SettingsDepth(LSource.StateCount - 1) = 1) and
    (LSource.Diagnostic(0).Issue = ptiCompilerSettings),
    'A stack continuing beyond the unit is retained explicitly for its future compiler owner');
  LSource := Translate('unit Main; interface {$packrecords 3} implementation end.');
  Check(not LSource.Resolved and (LSource.Diagnostic(0).Issue = ptiMalformedDirective),
    'Invalid source policy values cannot enter a captured typed state');
  LSource := Translate('unit Main; interface {$packrecords 4}{$A+} implementation end.');
  Check(not LSource.Resolved and (LSource.Diagnostic(0).Issue = ptiCompilerSettings),
    'Unqualified mode-dependent alignment aliases cannot retain a stale numeric policy');
  LSource := Translate('unit Main; interface {$push}{$I missing.inc} implementation end.');
  Supports(LSource, INyxPascalTranslationSettings, LSettings);
  Check(not LSettings.SettingsKnown(LSource.StateCount - 1),
    'An unavailable include revokes knowledge of the stack shape');
  LRejected := False;
  try
    LSettings.SettingsDepth(LSource.StateCount - 1);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Prior captured frames cannot masquerade as the current stack after unknown effects');
end;

procedure Budgets;
var
  LFiles: INyxPascalFileCatalog;
  LSource: INyxPascalTranslation;
  LRejected: Boolean;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('main.pas'),
    'unit Main; interface {$push}{$push}{$pop}{$pop} implementation end.');
  LRejected := False;
  try
    LSource := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('main.pas'), Facts,
      TNyxPascalTranslationOptions.Defaults.UpToIncludeDepth(1));
  except
    on ENyxPascalTranslationLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LSource = nil), 'Settings nesting quotas refuse without publishing a partial translation');
  LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('main.pas'),
    'unit Main; interface {$push}{$pop} implementation end.');
  LRejected := False;
  try
    LSource := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('main.pas'), Facts,
      TNyxPascalTranslationOptions.Defaults.UpToStates(2));
  except
    on ENyxPascalTranslationLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LSource = nil), 'Stack-only state transitions cannot bypass the retained-state budget');
end;

begin
  try
    ValuesAndRestore;
    SourceScopes;
    AliasesAndRefusals;
    Budgets;
    WriteLn('PASS ', GChecks, ' typed compiler policy checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
