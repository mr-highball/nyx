{ Current lexical/projection revisions compare against independent whole parses.
  The new leading-row proof grants no translation state or semantic world reuse. }
program nyx_pascal_revision_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, {$ifndef PAS2JS}Classes,{$endif} nyx.text, nyx.scheduler,
  nyx.pascal.lexing, nyx.pascal.lexing.append, nyx.pascal.syntax,
  nyx.pascal.context, nyx.pascal.files, nyx.pascal.translation,
  nyx.pascal.syntax.translated, nyx.pascal.work;

type
  { Cancellation inside real comparison work. This scope owns no source or
    parser and stops before any linked candidate can be published. }
  TRevisionStop = class(TInterfacedObject, INyxCancellationScope)
  public
    Polls: Integer;
    Stopped: Boolean;
    procedure Cancel;
    function GetCancelled: Boolean;
  end;
  {$ifndef PAS2JS}
  { Native readers share immutable old/current sources only. Every revision and
    parser result is local; main joins before reading counters/error messages. }
  TRevisionReader = class(TThread)
  protected
    procedure Execute; override;
  public
    Previous: INyxPascalTranslatedSyntax;
    Current: INyxPascalTranslation;
    Expected: TNyxText;
    Iterations: Integer;
    ErrorText: String;
  end;
  {$endif}

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then raise Exception.Create(AReason);
end;

{ Independent full public metadata observation, retained verbatim from the maintained checkpoint consumer. }
function SnapshotText(const ASource: INyxPascalSyntaxSnapshot): TNyxText;
var
  LParts: TNyxStrings;
  LAt: Integer;
  LScope: TNyxPascalSyntaxScope;
  LEntry: TNyxPascalSyntaxDeclaration;
  LFlag: TNyxPascalSyntaxFlag;
  LLink: TNyxPascalLinkageFlag;
  LVariants: INyxPascalVariantSyntax;
  LHints: INyxPascalHintSyntax;
  LLabels: INyxPascalStatementLabelSyntax;
  LIntrinsics: INyxPascalIntrinsicSyntax;
  LStructures: INyxPascalStructuredSyntax;
  LOperators: INyxPascalOperatorSyntax;

  procedure Number(AValue: Integer);
  begin
    LParts.Add(TNyxText(IntToStr(AValue)) + ';');
  end;

  procedure Text(const AText: TNyxText);
  begin
    LParts.Add(TNyxText(IntToStr(Length(AText))) + ':' + AText);
  end;

  procedure Span(const ASpan: TNyxPascalSpan);
  begin
    Number(ASpan.Start);
    Number(ASpan.Length);
  end;

begin
  LParts := TNyxStrings.Create;
  try
    Text(ASource.Lexical.Source);
    Number(ASource.ScopeCount);
    for LAt := 0 to ASource.ScopeCount - 1 do
    begin
      LScope := ASource.Scope(LAt);
      Text(LScope.Name);
      Number(Ord(LScope.Kind));
      Number(LScope.Parent);
      Number(LScope.OwnerDeclaration);
      Number(LScope.FirstDeclaration);
      Number(LScope.DeclarationCount);
      Span(LScope.Span);
      Number(Ord(LScope.Complete));
      Number(Ord(LScope.UnsupportedSyntax));
    end;
    Supports(ASource, INyxPascalStructuredSyntax, LStructures);
    Supports(ASource, INyxPascalOperatorSyntax, LOperators);
    Number(ASource.DeclarationCount);
    for LAt := 0 to ASource.DeclarationCount - 1 do
    begin
      LEntry := ASource.Declaration(LAt);
      Text(LEntry.Name);
      Text(LEntry.OwnerName);
      Number(Ord(LEntry.Kind));
      Number(Ord(LEntry.RoutineKind));
      Span(LEntry.PropertyParameters);
      Number(Ord(pspaRead in LEntry.PropertyAccess));
      Number(Ord(pspaWrite in LEntry.PropertyAccess));
      Number(Ord(LEntry.DefaultProperty));
      Number(Ord(LEntry.OperatorKind));
      Number(LEntry.VariantArm);
      Number(LEntry.FirstHint);
      Number(Ord(LEntry.Anonymous));
      Number(Ord(LEntry.TypeForm));
      Number(Ord(LEntry.Visibility));
      Number(Ord(LEntry.ParameterMode));
      Number(Ord(LEntry.CallingConvention));
      Span(LEntry.CallingConventionSpan);
      for LFlag := Low(TNyxPascalSyntaxFlag) to High(TNyxPascalSyntaxFlag) do
      begin
        Number(Ord(LFlag in LEntry.Flags));
      end;
      for LLink := Low(TNyxPascalLinkageFlag) to High(TNyxPascalLinkageFlag) do
      begin
        Number(Ord(LLink in LEntry.Linkage));
      end;
      Span(LEntry.LinkageSpan);
      Number(LEntry.FirstIntrinsic);
      Span(LEntry.NameSpan);
      Span(LEntry.HeaderSpan);
      Span(LEntry.TypeSpan);
      Span(LEntry.AncestorSpan);
      Span(LEntry.GenericSpan);
      Span(LEntry.ExpressionSpan);
      Number(LEntry.Scope);
      Number(LEntry.NestedScope);
      Number(LEntry.Next);
      Number(LEntry.NextNamed);
      Number(ASource.Named(LEntry.Scope, LEntry.Name));
      Number(LStructures.TypeAt(LEntry.TypeSpan.Start));

      if LEntry.OperatorKind in [Succ(pokUnknown)..High(TNyxPascalOperatorKind)] then
      begin
        Number(LOperators.NamedOperator(LEntry.Scope, LEntry.OperatorKind));
      end;
    end;
    Number(ASource.ImportCount);
    for LAt := 0 to ASource.ImportCount - 1 do
    begin
      Text(ASource.ImportAt(LAt).Name);
      Span(ASource.ImportAt(LAt).Span);
      Number(ASource.ImportAt(LAt).Scope);
      Number(Ord(ASource.ImportAt(LAt).Conditional));
    end;
    Number(ASource.DiagnosticCount);
    for LAt := 0 to ASource.DiagnosticCount - 1 do
    begin
      Number(Ord(ASource.Diagnostic(LAt).Issue));
      Span(ASource.Diagnostic(LAt).Span);
      Text(ASource.Diagnostic(LAt).MessageText);
    end;
    Supports(ASource, INyxPascalVariantSyntax, LVariants);
    Number(LVariants.SelectorCount);
    for LAt := 0 to LVariants.SelectorCount - 1 do
    begin
      Number(LVariants.SelectorAt(LAt).RecordScope);
      Number(LVariants.SelectorAt(LAt).ParentArm);
      Number(LVariants.SelectorAt(LAt).TagDeclaration);
      Span(LVariants.SelectorAt(LAt).TypeSpan);
      Span(LVariants.SelectorAt(LAt).Span);
      Number(LVariants.SelectorAt(LAt).FirstArm);
      Number(Ord(LVariants.SelectorAt(LAt).Complete));
    end;
    Number(LVariants.ArmCount);
    for LAt := 0 to LVariants.ArmCount - 1 do
    begin
      Number(LVariants.ArmAt(LAt).SelectorIndex);
      Span(LVariants.ArmAt(LAt).Span);
      Number(LVariants.ArmAt(LAt).FirstLabel);
      Number(LVariants.ArmAt(LAt).NextArm);
      Number(Ord(LVariants.ArmAt(LAt).Complete));
    end;
    Number(LVariants.LabelCount);
    for LAt := 0 to LVariants.LabelCount - 1 do
    begin
      Number(LVariants.LabelAt(LAt).ArmIndex);
      Span(LVariants.LabelAt(LAt).Span);
      Number(LVariants.LabelAt(LAt).NextLabel);
    end;
    Supports(ASource, INyxPascalHintSyntax, LHints);
    Number(LHints.HintCount);
    for LAt := 0 to LHints.HintCount - 1 do
    begin
      Number(LHints.HintAt(LAt).DeclarationIndex);
      Number(Ord(LHints.HintAt(LAt).Kind));
      Span(LHints.HintAt(LAt).Span);
      Span(LHints.HintAt(LAt).MessageSpan);
      Number(LHints.HintAt(LAt).NextHint);
    end;
    Supports(ASource, INyxPascalStatementLabelSyntax, LLabels);
    Number(LLabels.StatementLabelCount);
    for LAt := 0 to LLabels.StatementLabelCount - 1 do
    begin
      Number(Ord(LLabels.StatementLabelAt(LAt).Kind));
      Number(LLabels.StatementLabelAt(LAt).DeclarationIndex);
      Text(LLabels.StatementLabelAt(LAt).Name);
      Number(LLabels.StatementLabelAt(LAt).Scope);
      Span(LLabels.StatementLabelAt(LAt).NameSpan);
      Span(LLabels.StatementLabelAt(LAt).SectionSpan);
      Number(Ord(LLabels.StatementLabelAt(LAt).Complete));
      Number(Ord(LLabels.StatementLabelAt(LAt).Conditional));
    end;
    Supports(ASource, INyxPascalIntrinsicSyntax, LIntrinsics);
    Number(LIntrinsics.IntrinsicCount);
    for LAt := 0 to LIntrinsics.IntrinsicCount - 1 do
    begin
      Number(LIntrinsics.IntrinsicAt(LAt).DeclarationIndex);
      Number(Ord(LIntrinsics.IntrinsicAt(LAt).Kind));
      Span(LIntrinsics.IntrinsicAt(LAt).Span);
      Span(LIntrinsics.IntrinsicAt(LAt).KeySpan);
      Span(LIntrinsics.IntrinsicAt(LAt).OperandSpan);
      Number(LIntrinsics.IntrinsicAt(LAt).NextIntrinsic);
      Number(Ord(LIntrinsics.IntrinsicAt(LAt).Complete));
    end;
    Result := LParts.Join;
  finally
    LParts.Free;
  end;
end;



procedure TRevisionStop.Cancel; begin Stopped := True; end;
function TRevisionStop.GetCancelled: Boolean;
begin
  Inc(Polls);
  Result := Stopped or (Polls >= 12);
end;

function Arena(const AText: TNyxText): INyxPascalLexicalSnapshot;
begin
  Result := NewNyxPascalAppendLexical(AText,
    TNyxPascalLexicalOptions.Defaults.NestedComments(True).UnicodeIdentifiers(True));
end;

function Facts: INyxPascalCompilerContext;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi).CompleteDefines(True);
end;

function Source(const AText: TNyxText): INyxPascalTranslation;
begin
  Result := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('edit.pas'), AText), NyxPascalPhysicalFile('edit.pas'),
    Facts, TNyxPascalTranslationOptions.Defaults);
end;

function View(const AText: TNyxText): INyxPascalTranslatedSyntax;
begin
  Result := NewNyxPascalTranslatedSyntax(Source(AText), TNyxPascalSyntaxOptions.Defaults);
end;

function Prefix: TNyxText;
var
  LAt: Integer;
begin
  Result := 'unit Edit; {$h+} interface const ';
  for LAt := 1 to 96 do Result := Result + 'C' + TNyxText(IntToStr(LAt)) + '=' + TNyxText(IntToStr(LAt)) + '; ';
  Result := Result + 'type TStable=record Value:String; end; implementation ';
end;

function ProgramText(AValue: Integer): TNyxText;
begin
  Result := Prefix + 'procedure Run; var Marker:LongInt; begin Marker:=' +
    TNyxText(IntToStr(AValue)) + '; end; end.';
end;

procedure SameLexical(const ACurrent, ALinked: INyxPascalLexicalSnapshot);
var
  LAt: Integer;
  LLeft, LRight: TNyxPascalToken;
begin
  Check((ACurrent.Source = ALinked.Source) and (ACurrent.Count = ALinked.Count) and
    (ACurrent.Scalars = ALinked.Scalars) and (ACurrent.DiagnosticCount = ALinked.DiagnosticCount),
    'The linked value owns the exact current source/counts/diagnostics');
  for LAt := 0 to ACurrent.Count - 1 do
  begin
    LLeft := ACurrent.Token(LAt);
    LRight := ALinked.Token(LAt);
    Check((ACurrent.Text(LAt) = ALinked.Text(LAt)) and
      (LLeft.Span.Start = LRight.Span.Start) and (LLeft.Span.Length = LRight.Span.Length) and
      (LLeft.Kind = LRight.Kind) and (LLeft.Word = LRight.Word) and (LLeft.Symbol = LRight.Symbol) and
      (LLeft.NumberBase = LRight.NumberBase) and (LLeft.RealNumber = LRight.RealNumber) and
      (LLeft.Line = LRight.Line) and (LLeft.Column = LRight.Column) and
      (LLeft.Complete = LRight.Complete) and (LLeft.Escaped = LRight.Escaped) and
      (LLeft.Region = LRight.Region), 'Every current token payload/coordinate remains exact');
  end;
  for LAt := 0 to ACurrent.DiagnosticCount - 1 do
    Check((ACurrent.Diagnostic(LAt).Issue = ALinked.Diagnostic(LAt).Issue) and
      (ACurrent.Diagnostic(LAt).Span.Start = ALinked.Diagnostic(LAt).Span.Start) and
      (ACurrent.Diagnostic(LAt).Span.Length = ALinked.Diagnostic(LAt).Span.Length),
      'Current recovery diagnostics survive linking unchanged');
end;

procedure LexicalEdits;
  procedure Compare(const ABefore, AAfter: TNyxText);
  var
    LBefore, LCurrent, LLinked: INyxPascalLexicalSnapshot;
    LWork: INyxPascalLexicalAppendWork;
    LRetained, LAt: Integer;
  begin
    LBefore := Arena(ABefore);
    LCurrent := Arena(AAfter);
    LLinked := LinkNyxPascalLexicalRevision(LBefore, LCurrent);
    Check(LLinked <> nil, 'Owned equal-policy completed arenas can establish revision continuity');
    SameLexical(LCurrent, LLinked);
    LRetained := NyxPascalRetainedLexicalTokens(LBefore, LLinked);
    Check((LRetained >= 0) and (LRetained <= LBefore.Count) and (LRetained <= LCurrent.Count),
      'The direct-predecessor proof stays within both exact token collections');
    for LAt := 0 to LRetained - 1 do
      Check((LBefore.Text(LAt) = LLinked.Text(LAt)) and
        (LBefore.Token(LAt).Span.Start = LLinked.Token(LAt).Span.Start), 'Every donated leading token remains unchanged');
    Supports(LLinked, INyxPascalLexicalAppendWork, LWork);
    Check((LWork.LexedTokens = 0) and (LWork.CopiedTokens = 0) and
      (LWork.SharedTokens = LCurrent.Count), 'Linking leases current rows without reporting a fabricated scan');
    LBefore := nil;
    LCurrent := nil;
    Check(LLinked.Source = AAfter, 'The linked value outlives both snapshot owners');
  end;
begin
  Compare('', 'unit Edit; interface implementation end.');
  Compare('name', 'name_more');
  Compare('name_more', 'name');
  Compare('x := 1.;', 'x := 1..3;');
  Compare('x' + #13, 'x' + #13#10);
  Compare('{unfinished', '{finished} Value');
  Compare('''unfinished', '''finished''');
  Compare('asm mov eax, 1 end;', 'asm mov eax, 2 end;');
  Compare(TNyxText('Name := ''hello 😀'';'), TNyxText('Name := ''hello 🚀'';'));
  Compare('{$if Flag} First {$endif}', '{$if Flag} Second {$endif}');
  Compare('unit Edit; interface implementation end.', '');
end;

procedure ParserAndOrigins;
var
  LBefore, LRevised, LCold, LWhole: INyxPascalTranslatedSyntax;
  LCurrent: INyxPascalTranslation;
  LWork: INyxPascalSyntaxWork;
  LRevision: INyxPascalProjectionRevisionWork;
  LIdentity: IInterface;
  LAt: Integer;
  LText: TNyxText;
  LDeclaration: TNyxPascalSyntaxDeclaration;
  LBudget: INyxPascalAnalysisBudget;
  LColdWork, LRevisedWork: Integer;
begin
  LBefore := View(ProgramText(1));
  LIdentity := NyxPascalDeclarationIdentity(LBefore.Syntax, LBefore.Syntax.Named(0, 'C1'));
  LCurrent := Source(ProgramText(2000));
  LBudget := NewNyxPascalAnalysisBudget(1000000);
  try
    LRevised := ReviseNyxPascalTranslatedSyntax(LBefore, LCurrent, TNyxPascalSyntaxOptions.Defaults, LBudget);
    LRevisedWork := LBudget.Receipt.Total;
  finally
    LBudget.Retire;
  end;
  LBudget := NewNyxPascalAnalysisBudget(1000000);
  try
    LCold := NewNyxPascalTranslatedSyntax(LCurrent, TNyxPascalSyntaxOptions.Defaults, LBudget);
    LColdWork := LBudget.Receipt.Total;
  finally
    LBudget.Retire;
  end;
  WriteLn('late projection revision work / cold=', LColdWork, ' revised=', LRevisedWork);
  Check((LRevised <> nil) and (SnapshotText(LRevised.Syntax) = SnapshotText(LCold.Syntax)),
    'Revised current-source parsing equals every independent public metadata table');
  Supports(LRevised.Syntax, INyxPascalSyntaxWork, LWork);
  Check(LWork.ReusedDeclarations > 0, 'A late body edit restores the parser''s independently safe checkpoint');
  Check(NyxPascalDeclarationIdentity(LRevised.Syntax, LRevised.Syntax.Named(0, 'C1')) = LIdentity,
    'Earlier declarations retain their exact immutable metadata identity');
  Supports(LRevised, INyxPascalProjectionRevisionWork, LRevision);
  Check((LRevision.RevisionRetainedTokens > 0) and not LRevision.WholeSyntaxShared,
    'Revision distinguishes a proved prefix from a completely unchanged logical input');
  LCurrent := Source('// a current physical heading' + #13#10 + ProgramText(1));
  LWhole := ReviseNyxPascalTranslatedSyntax(LBefore, LCurrent, TNyxPascalSyntaxOptions.Defaults);
  Supports(LWhole, INyxPascalProjectionRevisionWork, LRevision);
  Check(LRevision.WholeSyntaxShared and (LWhole.Syntax = LBefore.Syntax),
    'Changed physical trivia can preserve the entire proven logical syntax');
  Check((LWhole.Source = LCurrent) and (LWhole.Origin(0).PhysicalSpan.Start > LBefore.Origin(0).PhysicalSpan.Start),
    'Shared syntax still maps through freshly constructed current physical origins');
  LText := TNyxText(StringReplace(String(ProgramText(1)), '{$h+}', '{$h-}', []));
  LCurrent := Source(LText);
  LRevised := ReviseNyxPascalTranslatedSyntax(LBefore, LCurrent, TNyxPascalSyntaxOptions.Defaults);
  Supports(LRevised, INyxPascalProjectionRevisionWork, LRevision);
  LDeclaration := LRevised.Syntax.Declaration(LRevised.Syntax.Named(0, 'TStable'));
  LDeclaration := LRevised.Syntax.Declaration(LRevised.Syntax.Named(LDeclaration.NestedScope, 'Value'));
  LAt := LRevised.OriginAt(LDeclaration.TypeSpan.Start);
  Check(LRevision.WholeSyntaxShared and
    (LRevised.Source.State(LRevised.Origin(LAt).State).Option('H') = ppkFalse),
    'Identical logical syntax binds through the new physical compiler-state origin');
  for LAt := 0 to LWhole.OriginCount - 1 do
    Check(LWhole.Source.Files.FileAt(LWhole.Origin(LAt).FileIndex).Name = 'edit.pas',
      'Each current origin keeps its real physical file owner');
  LCurrent := Source('unit Edit; interface const C1=999; implementation end.');
  LRevised := ReviseNyxPascalTranslatedSyntax(LBefore, LCurrent, TNyxPascalSyntaxOptions.Defaults);
  LCold := NewNyxPascalTranslatedSyntax(LCurrent, TNyxPascalSyntaxOptions.Defaults);
  Check(SnapshotText(LRevised.Syntax) = SnapshotText(LCold.Syntax), 'An early edit uses exact ordinary parsing when the last checkpoint is unsafe');
  Supports(LRevised.Syntax, INyxPascalSyntaxWork, LWork);
  Check(LWork.ReusedDeclarations = 0, 'No later parser checkpoint crosses an unproved earlier edit');
  LWhole := ReviseNyxPascalTranslatedSyntax(LWhole, Source(ProgramText(1)), TNyxPascalSyntaxOptions.Defaults);
  Check((LWhole <> nil) and LWhole.CompleteTranslation, 'An in-place next-generation revision keeps its input alive');
end;

procedure BudgetsAndRefusals;
var
  LBefore, LCurrent, LAttempt: INyxPascalLexicalSnapshot;
  LBudget: INyxPascalAnalysisBudget;
  LCancel: INyxCancellationScope;
  LAt, LFit: Integer;
  LCancelled: Boolean;
begin
  LBefore := Arena('const Value=1;');
  LCurrent := Arena('const Value=2;');
  LFit := -1;
  for LAt := 1 to 1000 do
  begin
    LBudget := NewNyxPascalAnalysisBudget(LAt);
    LAttempt := nil;
    try
      try
        LAttempt := LinkNyxPascalLexicalRevision(LBefore, LCurrent, LBudget);
        Check(LAttempt <> nil, 'A sufficient comparison allowance publishes the whole current linked value');
        LFit := LAt;
      except
        on ENyxPascalAnalysisWorkLimit do
          Check((LAttempt = nil) and (LBudget.Receipt.Total <= LAt), 'Every smaller revision comparison allowance refuses atomically');
      end;
    finally
      LBudget.Retire;
    end;

    if LFit > 0 then Break;
  end;
  Check(LFit > 1, 'Lexical revision linking has a finite admitted first fit');
  WriteLn('lexical revision work / first fit=', LFit, ' every smaller allowance refused');
  LCancel := TRevisionStop.Create;
  LAttempt := nil;
  LCancelled := False;
  try
    LAttempt := LinkNyxPascalLexicalRevision(LBefore, LCurrent, LCancel);
  except
    on ENyxPascalAnalysisCancelled do LCancelled := True;
  end;
  Check(LCancelled and (LAttempt = nil), 'Mid-comparison cancellation publishes no partial link');
  Check(LinkNyxPascalLexicalRevision(LBefore, NewNyxPascalLexicalSnapshot('const Value=2;')) = nil,
    'A physical snapshot without owned fixed-policy arena storage confers no revision authority');
  Check(LinkNyxPascalLexicalRevision(LBefore, NewNyxPascalAppendLexical('const Value=2;',
    TNyxPascalLexicalOptions.Defaults)) = nil, 'Changed lexical policies cannot donate a predecessor proof');
  Check(NyxPascalRetainedLexicalTokens(LBefore, NewNyxPascalLexicalSnapshot('const Value=2;')) = -1,
    'The parser authority helper independently rejects foreign lexical producers');
  LCurrent := LinkNyxPascalLexicalRevision(LBefore, LCurrent);
  Check(LCurrent.Source = 'const Value=2;', 'A sole current snapshot can be linked into its own destination');
end;

procedure ProjectionAdmission;
const
  CBefore: TNyxText = 'unit Edit; interface const Stable=1; implementation procedure Run; begin Work(1); end; end.';
  CAfter: TNyxText = 'unit Edit; interface const Stable=1; implementation procedure Run; begin Work(222); end; end.';
var
  LBefore, LAttempt, LKeep: INyxPascalTranslatedSyntax;
  LCurrent: INyxPascalTranslation;
  LBudget: INyxPascalAnalysisBudget;
  LAt, LFit: Integer;
begin
  LBefore := View(CBefore);
  LCurrent := Source(CAfter);
  LFit := -1;
  for LAt := 1 to 5000 do
  begin
    LAttempt := nil;
    LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      try
        LAttempt := ReviseNyxPascalTranslatedSyntax(LBefore, LCurrent, TNyxPascalSyntaxOptions.Defaults, LBudget);
        Check(LAttempt <> nil, 'A sufficient projection allowance publishes a complete current view');
        LKeep := LAttempt;
        LFit := LAt;
      except
        on ENyxPascalAnalysisWorkLimit do
          Check((LAttempt = nil) and (LBudget.Receipt.Total <= LAt), 'Every smaller complete projection allowance refuses without a partial view');
      end;
    finally
      LBudget.Retire;
    end;

    if LFit > 0 then Break;
  end;
  Check(LFit > 1, 'Current projection revision has a finite admitted first fit');
  Check(SnapshotText(LKeep.Syntax) = SnapshotText(View(CAfter).Syntax), 'The budget-qualified revised view equals complete independent parsing');
  Check(LBefore.Source.Files.TextAt(0) = CBefore, 'Refused revisions never alter earlier source ownership');
  WriteLn('projection revision work / first fit=', LFit, ' every smaller allowance refused');
end;

{$ifndef PAS2JS}
procedure TRevisionReader.Execute;
var
  LAt: Integer;
  LView: INyxPascalTranslatedSyntax;
begin
  try
    for LAt := 1 to 16 do
    begin
      LView := ReviseNyxPascalTranslatedSyntax(Previous, Current, TNyxPascalSyntaxOptions.Defaults);

      if (LView = nil) or (SnapshotText(LView.Syntax) <> Expected) then
        raise Exception.Create('Concurrent revision lost its current source metadata');
      Inc(Iterations);
    end;
  except
    on E: Exception do ErrorText := E.Message;
  end;
end;

procedure ConcurrentReaders;
var
  LBefore: INyxPascalTranslatedSyntax;
  LReaders: array of TRevisionReader;
  LAt: Integer;
begin
  LBefore := View(ProgramText(1));
  SetLength(LReaders, 4);
  try
    for LAt := 0 to High(LReaders) do
    begin
      LReaders[LAt] := TRevisionReader.Create(True);
      LReaders[LAt].FreeOnTerminate := False;
      LReaders[LAt].Previous := LBefore;
      LReaders[LAt].Current := Source(ProgramText(LAt + 2));
      LReaders[LAt].Expected := SnapshotText(NewNyxPascalTranslatedSyntax(
        LReaders[LAt].Current, TNyxPascalSyntaxOptions.Defaults).Syntax);
      LReaders[LAt].Start;
    end;
    for LAt := 0 to High(LReaders) do
    begin
      LReaders[LAt].WaitFor;
      Check((LReaders[LAt].Iterations = 16) and (LReaders[LAt].ErrorText = ''),
        'Concurrent immutable metadata revisions finish independently: ' + LReaders[LAt].ErrorText);
    end;
  finally
    for LAt := 0 to High(LReaders) do
    begin

      if LReaders[LAt] <> nil then
      begin
        LReaders[LAt].WaitFor;
        LReaders[LAt].Free;
      end;
    end;
  end;
  WriteLn('native concurrent projection revisions / 64 joined');
end;
{$endif}

begin
  try
    LexicalEdits;
    ParserAndOrigins;
    BudgetsAndRefusals;
    ProjectionAdmission;
    {$ifndef PAS2JS}ConcurrentReaders;{$endif}
    WriteLn('PASS ', GChecks, ' current-source revision checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', GChecks, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
