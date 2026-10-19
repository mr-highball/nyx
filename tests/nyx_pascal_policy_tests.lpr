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

uses SysUtils, Classes, nyx.text, nyx.bytes, nyx.scheduler, nyx.pascal.context,
  nyx.pascal.files, nyx.pascal.lexing, nyx.pascal.translation, nyx.pascal.macros,
  nyx.pascal.syntax, nyx.pascal.syntax.translated, codegen.fixture
  {$ifndef PAS2JS}, pointer.policy.fixture{$endif};

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

procedure SetAndModulePolicies;
var
  LSource: INyxPascalTranslation;
  LBefore: INyxPascalCompilerContext;
  LPolicies: TNyxPascalCompilerPolicies;
begin
  LBefore := NyxPascalWithCompilerPolicies(Facts,
    TNyxPascalCompilerPolicies.Unknown.PackSets(psp1).RequestSmartLinking(ppkFalse));
  Check(CaptureNyxPascalCompilerContext(LBefore).SameFacts(LBefore),
    'Set allocation and smart-link intent survive detached context capture');
  Check(not LBefore.SameFacts(NyxPascalWithCompilerPolicies(LBefore,
    NyxPascalCompilerPolicies(LBefore).PackSets(psp8))),
    'Set packing participates in compiler context identity');
  LPolicies := NyxPascalCompilerPolicies(LBefore.InMode(ppmObjFPC));
  Check((LPolicies.SetPacking = pspUnknown) and
    (LPolicies.SmartLinkingRequested = ppkFalse),
    'Mode reset revokes local packing while retaining the module-level link request');
  LSource := Translate('unit Main; interface {$packset 1}{$smartlink off}' +
    '{$push}{$packset 8}{$smartlink on}{$modeswitch class-} type TInside=Byte;' +
    '{$pop} type TAfter=Byte; implementation end.');
  Check(LSource.Resolved, 'Supported set/module policies do not poison later source participation');
  LPolicies := NyxPascalCompilerPolicies(LSource.State(StateAt(LSource, 'TInside')));
  Check((LPolicies.SetPacking = psp8) and (LPolicies.SmartLinkingRequested = ppkTrue),
    'The inner settings frame retains both exact authored policies');
  LPolicies := NyxPascalCompilerPolicies(LSource.State(StateAt(LSource, 'TAfter')));
  Check((LPolicies.SetPacking = psp1) and (LPolicies.SmartLinkingRequested = ppkTrue) and
    (LSource.State(StateAt(LSource, 'TAfter')).Feature(ppfClasses) = ppkFalse),
    'POP restores set packing while preserving global class and module-link intent');
  LSource := Translate('unit Main; {$mode objfpc} interface type TAfter=class end; implementation end.');
  Check(LSource.Resolved and
    (LSource.State(StateAt(LSource, 'TAfter')).Feature(ppfClasses) = ppkTrue),
    'Supported FPC object mode captures its class-model default');
  LSource := Translate('unit Main; interface {$if defined(UNKNOWN)}{$packset 2}' +
    '{$smartlink on}{$endif} type TAfter=Byte; implementation end.');
  LPolicies := NyxPascalCompilerPolicies(LSource.State(StateAt(LSource, 'TAfter')));
  Check(not LSource.Resolved and (LPolicies.SetPacking = pspUnknown) and
    (LPolicies.SmartLinkingRequested = ppkUnknown),
    'Conditional set/link changes retain uncertainty');
  LSource := Translate('unit Main; interface {$packset 16} type TAfter=Byte; implementation end.');
  Check(not LSource.Resolved and (LSource.DiagnosticCount = 1),
    'Unsupported set allocation refuses without accepting a guessed layout');
end;

{ Source intents are qualified separately from effective routine allocation.
  No global context field may leak the last register request into another body. }
procedure CodeGenerationPolicies;
var
  LSource: INyxPascalTranslation;
  LPrefix: INyxPascalTranslation;
  LIntents: INyxPascalRegisterDirectives;
  LPrefixIntents: INyxPascalRegisterDirectives;
  LIntent: TNyxPascalRegisterDirective;
  LSaved: INyxPascalCompilerContext;
  LChanged: INyxPascalCompilerContext;
  LCancel: INyxCancellationScope;
  LRefused: Boolean;
begin
  Check(CompilerPolicyValue = 33, 'Whole native input executes Intel/AT&T, actual POP and floating-point code');
  LSaved := NyxPascalWithCompilerPolicies(Facts,
    TNyxPascalCompilerPolicies.Unknown.ReadAssemblyBy(parATT));
  LChanged := NyxPascalWithCompilerPolicies(LSaved,
    NyxPascalCompilerPolicies(LSaved).ReadAssemblyBy(parIntel));
  Check(not LSaved.SameFacts(LChanged) and CaptureNyxPascalCompilerContext(LSaved).SameFacts(LSaved),
    'Reader changes affect exact immutable identity and survive capture');
  Check(NyxPascalCompilerPolicies(RestoreNyxPascalLocalSettings(LChanged, LSaved)).AssemblerReader = parATT,
    'Local restoration restores reader intent rather than keeping the later style');
  Check((NyxPascalCompilerPolicies(LSaved.UnknownState).AssemblerReader = parUnknown) and
    (NyxPascalCompilerPolicies(Facts).AssemblerReader = parUnknown), 'Unknown/default input never invents an assembler reader');
  LSource := Translate('{$asmmode att}{$push}{$I part.inc} Inside {$pop} Outside', '{$asmmode intel}');
  Check(LSource.Resolved and
    (NyxPascalCompilerPolicies(LSource.State(StateAt(LSource, 'Inside'))).AssemblerReader = parIntel) and
    (NyxPascalCompilerPolicies(LSource.State(StateAt(LSource, 'Outside'))).AssemblerReader = parATT),
    'Textual includes and PUSH/POP share exact reader policy');
  LSource := Translate('{$asmmode default} Marker');
  Check(LSource.Resolved and
    (NyxPascalCompilerPolicies(LSource.State(StateAt(LSource, 'Marker'))).AssemblerReader = parDefault),
    'Default reader retains configured-target intent');
  LSource := Translate('{$asmmode att}{$if UNKNOWN}{$asmmode intel}{$endif} Marker');
  Check(not LSource.Resolved and
    (NyxPascalCompilerPolicies(LSource.State(StateAt(LSource, 'Marker'))).AssemblerReader = parUnknown),
    'Uncertain reader changes revoke that policy without selecting a style');
  LSource := Translate('asm {$asmmode att} end;');
  Check(not LSource.Resolved, 'Reader changes inside ASM refuse instead of claiming instruction semantics');
  LSource := Translate('{$asmmode invented}');
  Check(not LSource.Resolved, 'An unsupported reader cannot enter the policy set');
  LSource := Translate('{$maxfpuregisters 0} {$I part.inc} {$I part.inc} ' +
    '{$if UNKNOWN}{$maxfpuregisters default}{$endif} Marker', '{$maxfpuregisters 7}');
  Supports(LSource, INyxPascalRegisterDirectives, LIntents);
  Check((LIntents <> nil) and (LIntents.RegisterDirectiveCount = 4), 'Exact register requests are retained as bounded immutable source intents');
  LIntent := LIntents.RegisterDirectiveAt(0);
  Check((LIntent.Policy = pfr0) and (LIntent.Activity = ppkTrue) and
    (LIntent.Origin.Occurrence = 0) and (LIntent.Origin.PrefixCount = 0) and
    (LIntent.State = LSource.Token(0).State) and
    (LSource.PhysicalToken(0).Span.Start = LIntent.Origin.Span.Start),
    'A zero register request retains exact original token, pre-state and authored activity');
  Check((LIntents.RegisterDirectiveAt(1).Origin.Occurrence <> LIntents.RegisterDirectiveAt(2).Origin.Occurrence) and
    (LIntents.RegisterDirectiveAt(1).Policy = pfr7) and
    (LIntents.RegisterDirectiveAt(2).Policy = pfr7), 'Repeated includes retain independent directive occurrences');
  Check((LIntents.RegisterDirectiveAt(3).Activity = ppkUnknown) and
    (LIntents.RegisterDirectiveAt(3).Policy = pfrDefault),
    'An uncertain request remains authored intent rather than effective allocation');
  LPrefix := NewNyxPascalTranslationPrefix(LSource,
    LIntents.RegisterDirectiveAt(2).Origin.PrefixCount, TNyxPascalTranslationOptions.Defaults);
  Supports(LPrefix, INyxPascalRegisterDirectives, LPrefixIntents);
  Check(LPrefixIntents.RegisterDirectiveCount = 2, 'A source prefix exposes only earlier original requests');
  LSource := nil;
  Check((LIntents.RegisterDirectiveCount = 4) and (LPrefixIntents.RegisterDirectiveCount = 2),
    'Detached full and prefix leases keep their immutable source ownership');
  LRefused := False;
  try
    LIntent := LIntents.RegisterDirectiveAt(4);
  except
    on ENyxPascalContext do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'Checked intent ordinals never expose an uninitialized value');
  LSource := Translate('{$ifdef NEVER}{$maxfpuregisters bad}{$endif}{$maxfpuregisters normal}');
  Supports(LSource, INyxPascalRegisterDirectives, LIntents);
  Check(LSource.Resolved and (LIntents.RegisterDirectiveCount = 1) and
    (LIntents.RegisterDirectiveAt(0).Policy = pfrDefault),
    'Inactive requests are skipped; NORMAL retains default intent');
  LSource := Translate('{$maxfpuregisters 8}');
  Supports(LSource, INyxPascalRegisterDirectives, LIntents);
  Check(LSource.Resolved and (LIntents.RegisterDirectiveAt(0).Policy = pfr8),
    'The explicit FPC 3.3.1 context admits its qualified eight-register request');
  LSource := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('old.pas'), '{$maxfpuregisters 8}'),
    NyxPascalPhysicalFile('old.pas'), NewNyxPascalCompilerContext(ppcFPC,
      TNyxPascalCompilerVersion.Create(3, 2, 2), ppmDelphi), TNyxPascalTranslationOptions.Defaults);
  Supports(LSource, INyxPascalRegisterDirectives, LIntents);
  Check(not LSource.Resolved and (LIntents.RegisterDirectiveCount = 0),
    'The older explicit context never borrows this analyzer compiler eight-register extension');
  LSource := Translate('{$maxfpuregisters 0007}');
  Supports(LSource, INyxPascalRegisterDirectives, LIntents);
  Check(LSource.Resolved and (LIntents.RegisterDirectiveAt(0).Policy = pfr7),
    'Decimal leading zeroes preserve their exact numeric intent');
  LSource := Translate('{$maxfpuregisters 999999999999999999999999999999}');
  Supports(LSource, INyxPascalRegisterDirectives, LIntents);
  Check(not LSource.Resolved and (LIntents.RegisterDirectiveCount = 0),
    'An out-of-domain count refuses before overflow or publication');
  LSource := Translate('{$maxfpuregisters 7 extra}');
  Check(not LSource.Resolved, 'A malformed directive tail is not silently accepted');
  LSource := Translate('{$maxfpuregisters 0}{$push}{$maxfpuregisters 7}{$pop}');
  Supports(LSource, INyxPascalRegisterDirectives, LIntents);
  Check(LSource.Resolved and (LIntents.RegisterDirectiveCount = 2) and
    (LIntents.RegisterDirectiveAt(1).Policy = pfr7),
    'POP does not synthesize a restored FPU request that the compiler settings stack does not own');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LRefused := False;
  try
    LPrefix := NewNyxPascalTranslationPrefix(LPrefix, 0, TNyxPascalTranslationOptions.Defaults, LCancel);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'Intent prefix capture preserves cooperative cancellation');
end;

{$ifndef PAS2JS}
{ Read the exact whole compiled fixture through a bounded byte boundary. The
  syntax consumer sees the same directives and function bodies as native FPC. }
procedure WholeCodeGenerationInput;
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
  LText: TNyxText;
  LSource: INyxPascalTranslation;
  LView: INyxPascalTranslatedSyntax;
  LIntents: INyxPascalRegisterDirectives;
begin

  if ParamCount = 0 then
  begin
    Exit;
  end;
  LStream := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(1)) +
    'tests/codegen.fixture.pas', fmOpenRead or fmShareDenyWrite);
  try

    if (LStream.Size < 1) or (LStream.Size > 65536) then
    begin
      raise Exception.Create('The whole handwritten fixture exceeds its input budget');
    end;
    SetLength(LBytes, LStream.Size);
    LStream.ReadBuffer(LBytes[0], Length(LBytes));
  finally
    LStream.Free;
  end;
  LText := NyxDecodeUTF8(LBytes);
  LSource := Translate(LText);
  Supports(LSource, INyxPascalRegisterDirectives, LIntents);
  LView := NewNyxPascalTranslatedSyntax(LSource, TNyxPascalSyntaxOptions.Defaults);
  Check(LSource.Resolved and (LIntents.RegisterDirectiveCount = 4) and
    (LView.Syntax.DiagnosticCount = 0) and (LView.Syntax.DeclarationCount >= 5),
    'The whole original compiled source passes ordinary translation and declaration/body syntax');
  Check((LIntents.RegisterDirectiveAt(0).Policy = pfr0) and
    (LIntents.RegisterDirectiveAt(1).Policy = pfr7) and
    (LIntents.RegisterDirectiveAt(2).Policy = pfrDefault) and
    (LIntents.RegisterDirectiveAt(3).Policy = pfr8), 'Whole input retains each routine request without a global effective limit');
end;
{$endif}

function PointerRequestAt(const ASource: INyxPascalTranslation;
  const AName: TNyxText): TNyxPascalPointerCheckingRequest;
begin
  Result := NyxPascalCompilerPolicies(ASource.State(StateAt(ASource, AName))).PointerCheckingRequested;
end;

{$push}{$R-}
{ Deliberately outside the enum domain, as an alternate input might supply.
  Range checking is disabled only while creating this validator control. }
function InvalidPointerRequest: TNyxPascalPointerCheckingRequest;
var
  LOrdinal: Integer;
begin
  LOrdinal := Ord(High(TNyxPascalPointerCheckingRequest)) + 1;
  Result := TNyxPascalPointerCheckingRequest(LOrdinal);
end;
{$pop}

procedure PointerPolicyValues;
var
  LRequest, LOther: TNyxPascalPointerCheckingRequest;
  LPolicies, LOtherPolicies: TNyxPascalCompilerPolicies;
  LBase, LContext, LSaved, LRestored: INyxPascalCompilerContext;
  LRejected: Boolean;
begin
  LBase := Facts;
  Check(NyxPascalCompilerPolicies(LBase).PointerCheckingRequested = ppcrUnknown,
    'Missing pointer-check capture never becomes an observed compiler default');
  for LRequest := Low(TNyxPascalPointerCheckingRequest) to High(TNyxPascalPointerCheckingRequest) do
  begin
    LPolicies := TNyxPascalCompilerPolicies.Unknown.RequestPointerChecking(LRequest);
    LContext := NyxPascalWithCompilerPolicies(LBase, LPolicies);
    Check((NyxPascalCompilerPolicies(CaptureNyxPascalCompilerContext(LContext)).PointerCheckingRequested = LRequest)
      and CaptureNyxPascalCompilerContext(LContext).SameFacts(LContext),
      'Owned context capture preserves the exact typed pointer request');
    Check(NyxPascalCompilerPolicies(LContext.UnknownState).PointerCheckingRequested = ppcrUnknown,
      'Unknown source effects revoke pointer-check knowledge');
    Check(NyxPascalCompilerPolicies(LContext.InMode(ppmObjFPC)).PointerCheckingRequested = LRequest,
      'Inspected FPC source modes retain pointer intent independently of mode defaults');
    for LOther := Low(TNyxPascalPointerCheckingRequest) to High(TNyxPascalPointerCheckingRequest) do
    begin
      LOtherPolicies := TNyxPascalCompilerPolicies.Unknown.RequestPointerChecking(LOther);
      Check((LPolicies.SamePolicies(LOtherPolicies) = (LRequest = LOther)) and
        (LContext.SameFacts(NyxPascalWithCompilerPolicies(LBase, LOtherPolicies)) = (LRequest = LOther)),
        'Default, off, on and missing pointer capture have distinct exact identities');
    end;
  end;
  LSaved := NyxPascalWithCompilerPolicies(LBase,
    TNyxPascalCompilerPolicies.Unknown.RequestPointerChecking(ppcrDefault));
  LContext := NyxPascalWithCompilerPolicies(LSaved,
    TNyxPascalCompilerPolicies.Unknown.RequestPointerChecking(ppcrOn).RequestGoto(ppkTrue));
  LRestored := RestoreNyxPascalLocalSettings(LContext, LSaved);
  Check((NyxPascalCompilerPolicies(LRestored).PointerCheckingRequested = ppcrDefault) and
    (NyxPascalCompilerPolicies(LRestored).GotoRequested = ppkTrue) and
    (NyxPascalCompilerPolicies(LContext).PointerCheckingRequested = ppcrOn),
    'Local restore recovers pointer intent without restoring module GOTO or mutating either input');
  LContext := NyxPascalWithCompilerPolicies(NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 0), ppmDelphi),
    TNyxPascalCompilerPolicies.Unknown.RequestPointerChecking(ppcrOff));
  Check(NyxPascalCompilerPolicies(LContext.InMode(ppmObjFPC)).PointerCheckingRequested = ppcrUnknown,
    'Other compiler versions retain conservative mode-reset behavior');
  LPolicies := TNyxPascalCompilerPolicies.Unknown;
  LRejected := False;
  try
    LOtherPolicies := LPolicies.RequestPointerChecking(InvalidPointerRequest);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LPolicies.PointerCheckingRequested = ppcrUnknown),
    'Invalid pointer enum values refuse without changing the caller policy');
end;

procedure PointerPolicySources;
var
  LSource: INyxPascalTranslation;
  LSaved: INyxPascalTranslationSettings;
  LAt: Integer;
begin
  LSource := Translate('unit PointerRequests; interface ' +
    '{$checkpointer ON} const EnabledMarker=1; {$push}{$checkpointer DEFAULT} const DefaultMarker=2; ' +
    '{$include part.inc}{$pop} const RestoredMarker=3; ' +
    '{$if 0}{$checkpointer off}{$endif} const InactiveMarker=4; ' +
    '{$if UNKNOWN}{$checkpointer off}{$endif} const UncertainMarker=5; implementation end.',
    '{$push}{$checkpointer-} const IncludedOff=6; {$checkpointer*} const IncludedDefault=7; ' +
    '{$pop} const AfterIncluded=8;');
  Check((PointerRequestAt(LSource, 'EnabledMarker') = ppcrOn) and
    (PointerRequestAt(LSource, 'DefaultMarker') = ppcrDefault),
    'Source ON and DEFAULT retain authored requests without resolving a target flag');
  Check((PointerRequestAt(LSource, 'IncludedOff') = ppcrOff) and
    (PointerRequestAt(LSource, 'IncludedDefault') = ppcrDefault) and
    (PointerRequestAt(LSource, 'AfterIncluded') = ppcrDefault) and
    (LSource.OccurrenceCount = 2), 'Nested include settings retain minus/star requests and independent POP scopes');
  Check((PointerRequestAt(LSource, 'RestoredMarker') = ppcrOn) and
    (PointerRequestAt(LSource, 'InactiveMarker') = ppcrOn) and
    (PointerRequestAt(LSource, 'UncertainMarker') = ppcrUnknown) and not LSource.Resolved,
    'Inactive writes preserve pointer intent and uncertain participation revokes knowledge');
  Supports(LSource, INyxPascalTranslationSettings, LSaved);
  LAt := StateAt(LSource, 'IncludedOff');
  Check((LSaved.SettingsDepth(LAt) = 2) and
    (NyxPascalCompilerPolicies(LSaved.SavedSettings(LAt, 1)).PointerCheckingRequested = ppcrDefault) and
    (LSaved.SettingsOrigin(LAt, 1).Occurrence = 1),
    'Saved pointer policy and original include PUSH origin survive publication');
  LSource := Translate('unit PointerRequests; {$checkpointer+} interface const PlusMarker=1; ' +
    '{$checkpointer OFF annotation} const OffMarker=2; {$checkpointer DEFAULT annotation} ' +
    'const ResetMarker=3; implementation end.');
  Check(LSource.Resolved and (PointerRequestAt(LSource, 'PlusMarker') = ppcrOn) and
    (PointerRequestAt(LSource, 'OffMarker') = ppcrOff) and
    (PointerRequestAt(LSource, 'ResetMarker') = ppcrDefault),
    'Inspected pointer spellings preserve the exact directive comments after their value');
  LSource := Translate('unit PointerRequests; interface {$checkpointer maybe} const BadMarker=1; implementation end.');
  Check(not LSource.Resolved and (PointerRequestAt(LSource, 'BadMarker') = ppcrUnknown),
    'Malformed pointer requests remain visible and revoke source facts');
  LSource := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('other.pas'), 'unit Other; {$checkpointer off} interface end.'),
    NyxPascalPhysicalFile('other.pas'), NewNyxPascalCompilerContext(ppcPas2JS,
      TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi), TNyxPascalTranslationOptions.Defaults);
  Check(not LSource.Resolved, 'Pointer-check directive recognition does not grant another compiler family FPC behavior');
end;

{$ifndef PAS2JS}
function ReadPointerPolicyInput(const AName: String): TNyxText;
var
  LFile: TFileStream;
  LBytes: TNyxBytes;
begin
  LFile := TFileStream.Create(ParamStr(1) + '/tests/' + AName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(LBytes, LFile.Size);

    if Length(LBytes) > 0 then
    begin
      LFile.ReadBuffer(LBytes[0], Length(LBytes));
    end;
    Result := NyxDecodeUTF8(LBytes);
  finally
    LFile.Free;
  end;
end;

procedure WholePointerPolicyInput;
var
  LSource: INyxPascalTranslation;
  LView: INyxPascalTranslatedSyntax;
begin
  Check(PointerPolicyFixtureValue = 42, 'The independent compiled pointer-policy fixture body remains callable');
  LSource := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('pointer.policy.fixture.pas'), ReadPointerPolicyInput('pointer.policy.fixture.pas'))
    .WithFile(NyxPascalPhysicalFile('pointer.policy.fixture.inc'), ReadPointerPolicyInput('pointer.policy.fixture.inc')),
    NyxPascalPhysicalFile('pointer.policy.fixture.pas'), Facts, TNyxPascalTranslationOptions.Defaults);
  LView := NewNyxPascalTranslatedSyntax(LSource, TNyxPascalSyntaxOptions.Defaults);
  Check(LSource.Resolved and (LSource.OccurrenceCount = 2) and (LView.Syntax.DiagnosticCount = 0),
    'The whole independently compiled unit/include retains ordinary declaration and body syntax');
  Check((PointerRequestAt(LSource, 'PointerBefore') = ppcrOff) and
    (PointerRequestAt(LSource, 'PointerDefault') = ppcrDefault) and
    (PointerRequestAt(LSource, 'PointerIncludedOff') = ppcrOff) and
    (PointerRequestAt(LSource, 'PointerIncludedDefault') = ppcrDefault) and
    (PointerRequestAt(LSource, 'PointerAfterInclude') = ppcrDefault) and
    (PointerRequestAt(LSource, 'PointerRestored') = ppcrOff),
    'The whole compiler input retains mode-local and nested include request histories');
end;
{$endif}

begin
  try
    ValuesAndRestore;
    SourceScopes;
    AliasesAndRefusals;
    Budgets;
    SetAndModulePolicies;
    CodeGenerationPolicies;
    PointerPolicyValues;
    PointerPolicySources;
    {$ifndef PAS2JS}WholeCodeGenerationInput;{$endif}
    {$ifndef PAS2JS}WholePointerPolicyInput;{$endif}
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
