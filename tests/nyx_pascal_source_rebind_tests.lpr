{ Dependency-checked translation rebinding. These are input/storage proofs, not
  semantic-world independence receipt, not cache integration or editor adoption. }
program nyx_pascal_source_rebind_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.bytes, nyx.scheduler, nyx.pascal.lexing,
  nyx.pascal.context, nyx.pascal.files, nyx.pascal.encoding,
  nyx.pascal.translation, nyx.pascal.work, nyx.pascal.syntax,
  nyx.pascal.syntax.translated, nyx.pascal.types, nyx.pascal.binding,
  nyx.pascal.conditions;

const
  CRoot: TNyxText = 'unit Leaf; {$mode delphi}{$h+}{$macro on}{$define FieldType:=LongInt} ' +
    'interface type TValue=record {$i fields.inc} end; ' +
    '{$push}{$h-} procedure Run(const Value:String); {$pop} implementation end.';
  CFields: TNyxText = 'Value:FieldType;';

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then raise Exception.Create(AReason);
end;

function Facts: INyxPascalCompilerContext;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi).CompleteDefines(True);
end;

function Files: INyxPascalFileCatalog;
begin
  Result := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('leaf.pas'), CRoot)
    .WithFile(NyxPascalPhysicalFile('fields.inc'), CFields)
    .WithFile(NyxPascalPhysicalFile('unrelated.pas'), 'unit Unrelated; interface implementation end.');
end;

function Source(const AFiles: INyxPascalFileCatalog): INyxPascalTranslation;
begin
  Result := NewNyxPascalTranslation(AFiles, NyxPascalPhysicalFile('leaf.pas'),
    Facts, TNyxPascalTranslationOptions.Defaults);
  Check(Result.Resolved, 'The complete root/include/settings/macro source resolves');
end;

procedure SameTranslation(const AOld, ANew: INyxPascalTranslation);
var
  LAt, LLevel: Integer;
  LOldToken, LNewToken: TNyxPascalTranslatedToken;
  LOldSettings, LNewSettings: INyxPascalTranslationSettings;
  LOldMacros, LNewMacros: INyxPascalTranslationMacros;
begin
  Check((AOld.Count = ANew.Count) and (AOld.StateCount = ANew.StateCount) and
    (AOld.OccurrenceCount = ANew.OccurrenceCount) and (AOld.DiagnosticCount = ANew.DiagnosticCount),
    'Every published row count remains exact');
  for LAt := 0 to AOld.Count - 1 do
  begin
    LOldToken := AOld.Token(LAt);
    LNewToken := ANew.Token(LAt);
    Check((AOld.Text(LAt) = ANew.Text(LAt)) and
      (LOldToken.Occurrence = LNewToken.Occurrence) and (LOldToken.State = LNewToken.State) and
      (LOldToken.PhysicalToken = LNewToken.PhysicalToken) and
      (LOldToken.Activity = LNewToken.Activity) and (LOldToken.Expansion = LNewToken.Expansion),
      'Effective tokens keep their physical occurrence, state, activity and macro origin');
  end;
  Supports(AOld, INyxPascalTranslationSettings, LOldSettings);
  Supports(ANew, INyxPascalTranslationSettings, LNewSettings);
  Supports(AOld, INyxPascalTranslationMacros, LOldMacros);
  Supports(ANew, INyxPascalTranslationMacros, LNewMacros);
  for LAt := 0 to AOld.StateCount - 1 do
  begin
    Check(AOld.State(LAt).SameFacts(ANew.State(LAt)) and
      (LOldSettings.SettingsKnown(LAt) = LNewSettings.SettingsKnown(LAt)) and
      (LOldSettings.SettingsDepth(LAt) = LNewSettings.SettingsDepth(LAt)),
      'Compiler facts and local stack shape remain exact');
    Check(LOldMacros.StateMacros(LAt).SameMacros(LNewMacros.StateMacros(LAt)),
      'Each state keeps its exact immutable macro environment');
    for LLevel := 0 to LOldSettings.SettingsDepth(LAt) - 1 do
      Check(LOldSettings.SavedSettings(LAt, LLevel).SameFacts(LNewSettings.SavedSettings(LAt, LLevel)),
        'Saved historical settings remain independently readable');
  end;
  for LAt := 0 to AOld.OccurrenceCount - 1 do
    Check(NyxPascalOccurrenceLexical(AOld, LAt) = NyxPascalOccurrenceLexical(ANew, LAt),
      'Unchanged occurrences share their exact completed lexical owner');
end;

procedure InputsAndLifetime;
var
  LFiles, LCurrent, LChanged: INyxPascalFileCatalog;
  LOriginal, LRebound, LAttempt, LPrefix: INyxPascalTranslation;
  LByteCatalog: INyxPascalByteFileCatalog;
  LBytes: INyxPascalSourceBytes;
  LView: INyxPascalSourceView;
begin
  LFiles := Files;
  LOriginal := Source(LFiles);
  LCurrent := LFiles.WithFile(NyxPascalPhysicalFile('unrelated.pas'), 'unit Unrelated; interface var Edited:LongInt; implementation end.');
  LRebound := RebindNyxPascalTranslation(LOriginal, LCurrent);
  Check(LRebound <> nil, 'An unrelated edit can retain the root and include');
  Check(LRebound.Files.TextAt(2) = LCurrent.TextAt(2), 'The rebound source exposes current unrelated content');
  Check(LOriginal.Files.TextAt(2) = LFiles.TextAt(2), 'The old source retains its original project catalog');
  SameTranslation(LOriginal, LRebound);
  LChanged := LCurrent.WithFile(NyxPascalPhysicalFile('fields.inc'), 'Other:FieldType;');
  Check(RebindNyxPascalTranslation(LOriginal, LChanged) = nil, 'A changed used include invalidates reuse');
  LChanged := LCurrent.WithFile(NyxPascalPhysicalFile('leaf.pas'), CRoot + ' ');
  Check(RebindNyxPascalTranslation(LOriginal, LChanged) = nil, 'Changed root trivia is still changed input');
  Check(RebindNyxPascalTranslation(LOriginal, LCurrent.WithIncludeDirectory('other')) = nil,
    'Include search changes invalidate reuse even when this include still exists');
  Check(RebindNyxPascalTranslation(LOriginal, LCurrent.WithFile(NyxPascalPhysicalFile('extra.pas'), 'unit Extra; interface implementation end.')) = nil,
    'Membership changes cannot borrow a prior resolution/search proof');
  LPrefix := NewNyxPascalTranslationPrefix(LOriginal, LOriginal.Count - 1, TNyxPascalTranslationOptions.Defaults);
  Check(RebindNyxPascalTranslation(LPrefix, LCurrent) = nil, 'A prefix wrapper cannot impersonate a published full translation');
  LBytes := NewNyxPascalSourceBytes(NyxEncodeUTF8(CFields));
  LView := DecodeNyxPascalSource(LBytes, NyxPascalUTF8Codec);
  Supports(LCurrent, INyxPascalByteFileCatalog, LByteCatalog);
  LChanged := LByteCatalog.WithByteSource(NyxPascalPhysicalFile('fields.inc'), LBytes, LView);
  Check(LChanged.TextAt(1) = LCurrent.TextAt(1), 'The changed provenance keeps identical readable text');
  Check(RebindNyxPascalTranslation(LOriginal, LChanged) = nil, 'Equal text cannot replace original byte-origin identity');
  LOriginal := nil;
  LFiles := nil;
  LCurrent := nil;
  Check(LRebound.Count > 0, 'Rebound rows outlive the original source and caller catalog');
  LAttempt := RebindNyxPascalTranslation(LRebound, LRebound.Files);
  Check((LAttempt <> nil) and (LAttempt.Text(0) = LRebound.Text(0)), 'A published rebound source can itself be safely rebound');
end;

procedure WorkAndCancellation;
var
  LFiles, LCurrent: INyxPascalFileCatalog;
  LSource, LAttempt, LRetained: INyxPascalTranslation;
  LBudget: INyxPascalAnalysisBudget;
  LCancel: INyxCancellationScope;
  LAt, LFit: Integer;
  LRefused: Boolean;
begin
  LFiles := Files;
  LSource := Source(LFiles);
  LCurrent := LFiles.WithFile(NyxPascalPhysicalFile('unrelated.pas'), 'unit Unrelated; interface var Changed:Byte; implementation end.');
  LFit := -1;
  for LAt := 1 to 3000 do
  begin
    LAttempt := nil;
    LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      try
        LAttempt := RebindNyxPascalTranslation(LSource, LCurrent, LBudget);
        Check(LAttempt <> nil, 'A sufficient proof allowance publishes the complete rebound source');
        LRetained := LAttempt;
        LFit := LAt;
      except
        on ENyxPascalAnalysisWorkLimit do
          Check((LAttempt = nil) and (LBudget.Receipt.Total <= LAt), 'Every smaller proof allowance refuses atomically');
      end;
    finally
      LBudget.Retire;
    end;

    if LFit > 0 then Break;
  end;
  Check(LFit > 1, 'Rebinding has a finite admitted first fit');
  WriteLn('source rebind work / first fit=', LFit, ' every smaller allowance refused');
  Check(LRetained.Text(0) = LSource.Text(0), 'Retiring proof work does not retire the resulting source');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LAttempt := nil;
  LRefused := False;
  try
    LAttempt := RebindNyxPascalTranslation(LSource, LCurrent, LCancel);
  except
    on ENyxPascalAnalysisCancelled do LRefused := True;
  end;
  Check(LRefused and (LAttempt = nil), 'Cancellation publishes no partial input rebind');
  Check(LSource.Resolved and (LSource.Files.TextAt(2) = LFiles.TextAt(2)), 'Failed proof leaves the prior source unchanged');
end;

procedure ConditionDependencies;
var
  LResult: INyxPascalSemanticTranslation;
  LDependency: INyxPascalSemanticDependencies;
  LWorld: INyxPascalSemanticWorld;

  function Analyze(const AText: TNyxText;
    const AWorld: INyxPascalSemanticWorld): INyxPascalSemanticTranslation;
  begin
    Result := NewNyxPascalSemanticTranslation(NewNyxPascalFileCatalog(pfcExact)
      .WithFile(NyxPascalPhysicalFile('leaf.pas'), AText),
      NyxPascalPhysicalFile('leaf.pas'), Facts, AWorld,
      TNyxPascalTranslationOptions.Defaults, TNyxPascalSyntaxOptions.Defaults,
      TNyxPascalConditionalOptions.Defaults);
  end;
begin
  LResult := Analyze('unit Leaf; interface type TReady=record Value:LongInt; end; implementation end.', nil);
  Check(Supports(LResult, INyxPascalSemanticDependencies, LDependency) and
    not LDependency.ConsultedWorld and LResult.Source.Resolved,
    'A complete source without semantic conditions explicitly records world independence');
  LResult := Analyze('unit Leaf; interface {$if 1=1}const Yes=1;{$endif} implementation end.', nil);
  Check(Supports(LResult, INyxPascalSemanticDependencies, LDependency) and
    not LDependency.ConsultedWorld and LResult.Source.Resolved,
    'A context-only scalar condition does not acquire semantic world authority');
  LResult := Analyze('unit Leaf; interface type TReady=record end; ' +
    '{$if declared(TReady)}const Yes=1;{$endif} implementation end.', nil);
  Check(Supports(LResult, INyxPascalSemanticDependencies, LDependency) and
    LDependency.ConsultedWorld and not LResult.Pending and (LResult.Answers = 1),
    'Even a successful local declaration condition records its world consultation');
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults
    .WithImplicitScopes(pispDeclarationNames));
  LResult := Analyze('unit Leaf; interface {$if declared(TAbsent)}const No=1;{$endif} implementation end.', LWorld);
  Check(Supports(LResult, INyxPascalSemanticDependencies, LDependency) and
    LDependency.ConsultedWorld and not LResult.Pending and (LResult.Answers = 1),
    'A successfully answered absence query still depends on the supplied namespace');
  LResult := Analyze('unit Leaf; interface {$if declared(TAbsent)}const No=1;{$endif} implementation end.', nil);
  Check(Supports(LResult, INyxPascalSemanticDependencies, LDependency) and
    LDependency.ConsultedWorld and LResult.Pending and (LResult.Answers = 0),
    'An unanswered query is world-dependent even though it accepted no source answer');
  LResult := nil;
  LWorld := nil;
  Check(LDependency.ConsultedWorld, 'The copied dependency receipt outlives its input world');
end;

procedure ProjectionStorage;
var
  LFiles, LCurrent: INyxPascalFileCatalog;
  LOld, LNew, LCold, LAttempt: INyxPascalTranslatedSyntax;
  LOldOrigin, LNewOrigin: TNyxPascalSyntaxOrigin;
  LOldDecl, LNewDecl: TNyxPascalSyntaxDeclaration;
  LBudget: INyxPascalAnalysisBudget;
  LParsed: INyxPascalTypeSyntax;
  LAt, LFit: Integer;
begin
  LFiles := Files;
  LCurrent := LFiles.WithFile(NyxPascalPhysicalFile('unrelated.pas'), 'unit Unrelated; interface var NewText:LongInt; implementation end.');
  LOld := NewNyxPascalTranslatedSyntax(Source(LFiles), TNyxPascalSyntaxOptions.Defaults);
  LFit := -1;
  for LAt := 1 to 3000 do
  begin
    LAttempt := nil;
    LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      try
        LAttempt := RebindNyxPascalTranslatedSyntax(LOld, LCurrent, LBudget);
        Check(LAttempt <> nil, 'A sufficient projection proof publishes a complete view');
        Check((LBudget.Receipt.Used(papSyntaxToken) = 0) and
          (LBudget.Receipt.Used(papMetadataNode) = 0), 'Rebinding performs no declaration parse or metadata reconstruction');
        LNew := LAttempt;
        LFit := LAt;
      except
        on ENyxPascalAnalysisWorkLimit do
          Check((LAttempt = nil) and (LBudget.Receipt.Total <= LAt), 'Each smaller projection proof refuses atomically');
      end;
    finally
      LBudget.Retire;
    end;

    if LFit > 0 then Break;
  end;
  Check(LFit > 1, 'Projection input rebinding has a finite first fit');
  WriteLn('source projection rebind work / first fit=', LFit, ' every smaller allowance refused');
  Check((LNew.Syntax = LOld.Syntax) and (LNew.OriginCount = LOld.OriginCount),
    'A successful rebind shares its exact immutable syntax and origin rows');
  Check(LNew.Source.Files.TextAt(2) = LCurrent.TextAt(2), 'The shared projection exposes the current unrelated buffer');
  for LAt := 0 to LOld.OriginCount - 1 do
  begin
    LOldOrigin := LOld.Origin(LAt);
    LNewOrigin := LNew.Origin(LAt);
    Check((LOldOrigin.VirtualSpan.Start = LNewOrigin.VirtualSpan.Start) and
      (LOldOrigin.PhysicalSpan.Start = LNewOrigin.PhysicalSpan.Start) and
      (LOldOrigin.FileIndex = LNewOrigin.FileIndex) and (LOldOrigin.State = LNewOrigin.State) and
      (LOldOrigin.Occurrence = LNewOrigin.Occurrence) and (LOldOrigin.Expansion = LNewOrigin.Expansion),
      'Every shared origin retains its exact physical/include/macro identity');
  end;
  LCold := NewNyxPascalTranslatedSyntax(Source(LCurrent), TNyxPascalSyntaxOptions.Defaults);
  Check(LCold.Syntax.DeclarationCount = LNew.Syntax.DeclarationCount, 'Cold and rebound declaration counts agree');
  for LAt := 0 to LCold.Syntax.DeclarationCount - 1 do
  begin
    LOldDecl := LCold.Syntax.Declaration(LAt);
    LNewDecl := LNew.Syntax.Declaration(LAt);
    Check((LOldDecl.Name = LNewDecl.Name) and (LOldDecl.Kind = LNewDecl.Kind) and
      (LOldDecl.NameSpan.Start = LNewDecl.NameSpan.Start) and (LOldDecl.TypeSpan.Start = LNewDecl.TypeSpan.Start),
      'Cold reconstruction and storage reuse retain the same declaration coordinates');

    if LNewDecl.Kind = psdkField then
    begin
      LParsed := LNew.TypeOfDeclaration(LAt, TNyxPascalTypeOptions.Defaults);
      Check(LParsed.Complete, 'A new source-free type memo is usable on the rebound projection');
    end;
  end;
  LOld := nil;
  LFiles := nil;
  Check(LNew.Syntax.DeclarationCount > 0, 'Shared metadata outlives the previous projection');
end;

begin
  try
    InputsAndLifetime;
    WorkAndCancellation;
    ConditionDependencies;
    ProjectionStorage;
    WriteLn('PASS ', GChecks, ' source catalog rebind checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', GChecks, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
