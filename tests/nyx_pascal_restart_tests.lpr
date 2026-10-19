{ Earlier parser/filter restarts must equal independent whole construction.
  Bounded history changes reuse opportunities, never the resulting language facts. }
program nyx_pascal_restart_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, {$ifndef PAS2JS}Classes,{$endif} nyx.text, nyx.scheduler,
  nyx.pascal.lexing, nyx.pascal.lexing.append, nyx.pascal.syntax,
  nyx.pascal.work;

type
  { A deterministic cooperative stop owns no parser or source. Different poll
    positions exercise both history selection and private restoration work. }
  TRestartStop = class(TInterfacedObject, INyxCancellationScope)
  public
    Polls, StopAfter: Integer;
    Stopped: Boolean;
    procedure Cancel;
    function GetCancelled: Boolean;
  end;
  {$ifndef PAS2JS}
  { Each native reader owns its revision result while sharing immutable input
    owners. Main joins every reader before inspecting its result or retiring it. }
  TRestartReader = class(TThread)
  protected
    procedure Execute; override;
  public
    Previous: INyxPascalSyntaxSnapshot;
    Current: INyxPascalLexicalSnapshot;
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

{ Independent public metadata serializer, retained verbatim from the maintained revision consumer. }
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

procedure TRestartStop.Cancel;
begin
  Stopped := True;
end;

function TRestartStop.GetCancelled: Boolean;
begin
  Inc(Polls);
  Result := Stopped or (Polls >= StopAfter);
end;

function Arena(const AText: TNyxText): INyxPascalLexicalSnapshot;
begin
  Result := NewNyxPascalAppendLexical(AText, TNyxPascalLexicalOptions.Defaults);
end;

function Whole(const AText: TNyxText): INyxPascalSyntaxSnapshot;
begin
  Result := NewNyxPascalSyntaxFromLexical(Arena(AText), TNyxPascalSyntaxOptions.Defaults);
end;

function Revised(const APrevious: INyxPascalSyntaxSnapshot; const AText: TNyxText;
  const AExecution: INyxCancellationScope = nil): INyxPascalSyntaxSnapshot;
var
  LCurrent, LLinked: INyxPascalLexicalSnapshot;
begin
  LCurrent := Arena(AText);
  LLinked := LinkNyxPascalLexicalRevision(APrevious.Lexical, LCurrent, AExecution);
  Result := AppendNyxPascalSyntax(APrevious, LLinked, TNyxPascalSyntaxOptions.Defaults, AExecution);
end;

function Fixture(ACount: Integer): TNyxText;
var
  LAt: Integer;
begin
  Result := 'unit Restart; interface const ';
  for LAt := 1 to ACount do
    Result := Result + 'C' + TNyxText(IntToStr(LAt)) + '=' + TNyxText(IntToStr(LAt)) + '; ';
  Result := Result + 'type TChoice=record case Kind:Byte of 0:(Value:Integer); 1:(Other:Byte); end; ' +
    'TBox=class private FValue:Integer; public property Value:Integer read FValue write FValue; end; ' +
    'procedure Run; implementation procedure Run; label Again; var X:Integer; begin Again: X:=1; end; end.';
end;

function ChangeConstant(const AText: TNyxText; AIndex, AValue: Integer): TNyxText;
var
  LName: TNyxText;
  LStart, LFinish: Integer;
begin
  LName := 'C' + TNyxText(IntToStr(AIndex)) + '=';
  LStart := Pos(LName, AText);

  if LStart = 0 then raise Exception.Create('Fixture constant not found');
  Inc(LStart, Length(LName));
  LFinish := LStart;
  while (LFinish <= Length(AText)) and (AText[LFinish] <> ';') do Inc(LFinish);
  Result := Copy(AText, 1, LStart - 1) + TNyxText(IntToStr(AValue)) +
    Copy(AText, LFinish, Length(AText));
end;

procedure Bounded(const ASource: INyxPascalSyntaxSnapshot);
var
  LHistory: INyxPascalSyntaxRestartWork;
begin
  Check(Supports(ASource, INyxPascalSyntaxRestartWork, LHistory), 'Owned snapshots expose detached history work');
  Check((LHistory.ParserHistoryCount >= 0) and (LHistory.ParserHistoryCount <= 32) and
    (LHistory.FilterHistoryCount >= 0) and (LHistory.FilterHistoryCount <= 32),
    'Both flat restart directories retain at most 32 historical points');
  Check((LHistory.ParserCheckpointReads <= 33) and (LHistory.FilterCheckpointReads <= 33),
    'Selection visits at most the bounded history and separate latest point');
end;

procedure MiddleEdits;
const
  CPositions: array[0..12] of Integer = (1, 2, 3, 5, 17, 32, 63, 96, 128, 192, 256, 383, 384);
var
  LText, LBefore, LChanged: TNyxText;
  LBase, LNext, LCold: INyxPascalSyntaxSnapshot;
  LWork, LColdWork: INyxPascalSyntaxWork;
  LFilter: INyxPascalSyntaxFilterWork;
  LHistory: INyxPascalSyntaxRestartWork;
  LIdentity: IInterface;
  LAt, LPosition: Integer;
  LBudget: INyxPascalAnalysisBudget;
  LColdUnits, LRevisedUnits: Integer;
begin
  LText := Fixture(384);
  LBase := Whole(LText);
  LBefore := SnapshotText(LBase);
  LIdentity := NyxPascalDeclarationIdentity(LBase, LBase.Named(0, 'C1'));
  Bounded(LBase);
  for LAt := Low(CPositions) to High(CPositions) do
  begin
    LPosition := CPositions[LAt];
    LChanged := ChangeConstant(LText, LPosition, 9001);
    LNext := Revised(LBase, LChanged);
    LCold := Whole(LChanged);
    Check(SnapshotText(LNext) = SnapshotText(LCold), 'Every middle/early/late edit equals independent complete metadata');
    Bounded(LNext);
    Supports(LNext, INyxPascalSyntaxWork, LWork);
    Supports(LCold, INyxPascalSyntaxWork, LColdWork);
    Supports(LNext, INyxPascalSyntaxFilterWork, LFilter);
    Supports(LNext, INyxPascalSyntaxRestartWork, LHistory);
    Check(LWork.ReusedDeclarations < LPosition, 'A parser checkpoint never crosses the changed declaration');

    if LPosition >= 17 then
    begin
      Check((LWork.ReusedDeclarations > 0) and (LWork.RestartToken > 0), 'Earlier genuine parser checkpoints preserve completed declarations');
      Check(LFilter.ReusedFilterTokens > 0, 'Earlier filter checkpoints skip a proved lexical prefix');
      Check(LHistory.ParserCheckpointReads > 1, 'Middle edits select earlier history when the latest point is unsafe');
      Check(NyxPascalDeclarationIdentity(LNext, LNext.Named(0, 'C1')) = LIdentity,
        'A restored earlier declaration retains its immutable metadata identity');
      Check(LWork.ParserTokenReads < LColdWork.ParserTokenReads, 'Restoration actually avoids earlier parser reads');
    end;
  end;
  Check(SnapshotText(LBase) = LBefore, 'All sibling revisions leave the complete predecessor exact');
  LChanged := ChangeConstant(LText, 256, 9001);
  LBudget := NewNyxPascalAnalysisBudget(2000000);
  LCold := NewNyxPascalSyntaxFromLexical(Arena(LChanged), TNyxPascalSyntaxOptions.Defaults, LBudget);
  LColdUnits := LBudget.Receipt.Total;
  LBudget.Retire;
  LBudget := NewNyxPascalAnalysisBudget(2000000);
  LNext := Revised(LBase, LChanged, LBudget);
  LRevisedUnits := LBudget.Receipt.Total;
  LBudget.Retire;
  Check((LRevisedUnits < LColdUnits) and (SnapshotText(LNext) = SnapshotText(LCold)),
    'Measured revision includes prefix comparison/history work and still saves real admitted parser/filter work');
  WriteLn('middle source restart work / cold=', LColdUnits, ' revised=', LRevisedUnits);
end;

procedure BranchesAndRetirement;
var
  LText, LLeftText, LRightText, LBefore, LRightBefore: TNyxText;
  LBase, LLeft, LRight, LNext: INyxPascalSyntaxSnapshot;
  LAt: Integer;
begin
  LText := Fixture(384);
  LBase := Whole(LText);
  LBefore := SnapshotText(LBase);
  LLeftText := ChangeConstant(LText, 288, 701);
  LRightText := ChangeConstant(LText, 320, 702);
  LLeft := Revised(LBase, LLeftText);
  LRight := Revised(LBase, LRightText);
  LRightBefore := SnapshotText(LRight);
  Check(SnapshotText(LBase) = LBefore, 'Creating independent middle-edit branches preserves their ancestor');
  LBase := nil;
  for LAt := 0 to 3 do
  begin
    LLeftText := ChangeConstant(LLeftText, 200 - LAt * 48, 703 + LAt);
    LNext := Revised(LLeft, LLeftText);
    Check(SnapshotText(LNext) = SnapshotText(Whole(LLeftText)), 'Earlier edits remain exact through successive retired generations');
    LLeft := nil;
    LLeft := LNext;
    LNext := nil;
    Bounded(LLeft);
    Check(SnapshotText(LRight) = LRightBefore, 'A sibling keeps all metadata and indexes through ancestor retirement');
  end;
  Check(SnapshotText(LLeft) = SnapshotText(Whole(LLeftText)), 'The newest history owns everything needed after old snapshot retirement');
end;

procedure Recovery;
const
  CText: TNyxText = 'unit Recovery; interface const A=1; B=2; C=3; type T=record ' +
    'case Tag:Byte of 0:(X:Integer); 1:(Y:Byte); end; procedure Run; ' +
    'implementation procedure Run; label Again; begin Again: end; end.';
var
  LBase, LNext, LCold: INyxPascalSyntaxSnapshot;
  LVariants: INyxPascalVariantSyntax;
  LText, LChanged: TNyxText;
  LAt, LMode: Integer;
begin
  LCold := Whole('unit Missing; interface type T=record case Byte of :(X:Integer); end; implementation end.');
  Check(Supports(LCold, INyxPascalVariantSyntax, LVariants) and (LVariants.ArmCount > 0) and
    (LVariants.ArmAt(0).Span.Length = 0) and not LVariants.ArmAt(0).Complete and
    (LCold.DiagnosticCount > 0), 'A missing variant label keeps an empty incomplete arm and an explicit diagnostic');
  LBase := Whole(CText);
  for LAt := 1 to Length(CText) do
  begin
    for LMode := 0 to 2 do
    begin
      case LMode of
        0: LChanged := Copy(CText, 1, LAt - 1) + Copy(CText, LAt + 1, Length(CText));
        1: LChanged := Copy(CText, 1, LAt - 1) + ' ' + Copy(CText, LAt, Length(CText));
        2: LChanged := Copy(CText, 1, LAt - 1) + '''' + Copy(CText, LAt + 1, Length(CText));
      end;
      try
        LCold := Whole(LChanged);
        LNext := Revised(LBase, LChanged);
        Check(SnapshotText(LNext) = SnapshotText(LCold), 'Single-character edit/recovery at every source position keeps all current metadata exact');
        Bounded(LNext);
      except
        WriteLn('single-character fixture / scalar=', LAt, ' mode=', LMode);
        {$ifndef PAS2JS}DumpExceptionBackTrace(Output);{$endif}
        raise;
      end;
    end;
  end;
  LText := TNyxText(StringReplace(String(Fixture(96)), 'interface const',
    'interface {$ifdef Feature} const Broken=;', []));
  LText := TNyxText(StringReplace(String(LText), 'implementation procedure Run;',
    '{$include values.inc} {$endif} implementation procedure Run;', []));
  LBase := Whole(LText);
  LChanged := ChangeConstant(LText, 64, 9001);
  LNext := Revised(LBase, LChanged);
  Check(SnapshotText(LNext) = SnapshotText(Whole(LChanged)), 'Restored raw conditional depth and earlier parser/filter diagnostics remain exact');
  LChanged := TNyxText(StringReplace(String(LChanged), '{$endif}', '{$else}', []));
  LNext := Revised(LNext, LChanged);
  Check(SnapshotText(LNext) = SnapshotText(Whole(LChanged)), 'Changed conditional closure replaces provisional filter diagnostics');
  LChanged := '{hello ' + TNyxText('😀') + '}' + #13#10 + LChanged;
  LNext := Revised(LNext, LChanged);
  Check(SnapshotText(LNext) = SnapshotText(Whole(LChanged)), 'A shifted Unicode/CRLF prefix grants no stale scalar coordinate authority');
end;

procedure FragmentRestarts;
var
  LFragment: TNyxPascalSyntaxFragment;
  LOptions: TNyxPascalSyntaxOptions;
  LText, LChanged, LName: TNyxText;
  LBase, LNext, LCold: INyxPascalSyntaxSnapshot;
  LCurrent, LLinked: INyxPascalLexicalSnapshot;
  LWork: INyxPascalSyntaxWork;
  LAt: Integer;
begin
  for LFragment := psfDeclarations to psfInterfaceDeclarations do
  begin
    LOptions := TNyxPascalSyntaxOptions.Defaults.Fragment(LFragment);
    LText := '';

    if LFragment = psfMembers then LText := 'private '
    else if LFragment <> psfTypes then LText := 'const ';
    for LAt := 1 to 64 do
    begin
      LName := TNyxText(IntToStr(LAt));
      case LFragment of
        psfTypes: LText := LText + 'T' + LName + '=record Value:Integer; end; ';
        psfMembers: LText := LText + 'F' + LName + ':Integer; ';
      else
        LText := LText + 'C' + LName + '=' + LName + '; ';
      end;
    end;
    case LFragment of
      psfTypes: LChanged := TNyxText(StringReplace(String(LText),
        'T40=record Value:Integer;', 'T40=record Value:Byte;', []));
      psfMembers: LChanged := TNyxText(StringReplace(String(LText), 'F40:Integer;', 'F40:Byte;', []));
    else
      LChanged := ChangeConstant(LText, 40, 9001);
    end;
    LBase := NewNyxPascalSyntaxFromLexical(Arena(LText), LOptions);
    LCurrent := Arena(LChanged);
    LLinked := LinkNyxPascalLexicalRevision(LBase.Lexical, LCurrent);
    LNext := AppendNyxPascalSyntax(LBase, LLinked, LOptions);
    LCold := NewNyxPascalSyntaxFromLexical(LCurrent, LOptions);
    Check(SnapshotText(LNext) = SnapshotText(LCold), 'Explicit type/member/declaration fragment grammar resumes with its original caller role');
    Supports(LNext, INyxPascalSyntaxWork, LWork);
    Check(LWork.ReusedDeclarations > 0, 'Fragment histories actually restore completed earlier metadata');
    Bounded(LNext);
  end;
end;

procedure AtomicRefusal;
var
  LBase, LCandidate: INyxPascalSyntaxSnapshot;
  LText, LBefore, LExpected: TNyxText;
  LBudget: INyxPascalAnalysisBudget;
  LStop: TRestartStop;
  LScope: INyxCancellationScope;
  LAt, LFit, LCancelled, LCompleted: Integer;
  LRefused: Boolean;
begin
  LText := Fixture(5);
  LBase := Whole(LText);
  LBefore := SnapshotText(LBase);
  LText := ChangeConstant(LText, 3, 9001);
  LExpected := SnapshotText(Whole(LText));
  LFit := -1;
  for LAt := 1 to 20000 do
  begin
    LCandidate := nil;
    LBudget := NewNyxPascalAnalysisBudget(LAt);
    LRefused := False;
    try
      LCandidate := Revised(LBase, LText, LBudget);
    except
      on ENyxPascalAnalysisWorkLimit do LRefused := True;
    end;

    if not LRefused then
    begin
      Check(SnapshotText(LCandidate) = LExpected, 'First admitted restart returns the independent whole result');
      LFit := LAt;
      LBudget.Retire;
      Break;
    end;
    Check((LCandidate = nil) and (LBudget.Receipt.Total <= LAt), 'Every smaller allowance refuses before publishing a partial history or result');
    LBudget.Retire;
  end;
  Check((LFit > 1) and (SnapshotText(LBase) = LBefore), 'Exhaustive budget refusal preserves exact predecessor metadata');
  WriteLn('historical restart work / first fit=', LFit, ' every smaller allowance refused');
  LCancelled := 0;
  LCompleted := 0;
  for LAt := 1 to 80 do
  begin
    LCandidate := nil;
    LStop := TRestartStop.Create;
    LStop.StopAfter := LAt;
    LScope := LStop;
    try
      LCandidate := Revised(LBase, LText, LScope);
      Check(SnapshotText(LCandidate) = LExpected, 'A completed cancellation probe remains exact');
      Inc(LCompleted);
    except
      on ENyxPascalAnalysisCancelled do
      begin
        Check(LCandidate = nil, 'Cancellation at each tested work poll publishes no partial candidate');
        Inc(LCancelled);
      end;
    end;
    Check(SnapshotText(LBase) = LBefore, 'Interrupted history selection/restoration never changes its predecessor');
  end;
  Check(LCancelled > 10, 'Cancellation reaches multiple actual link/filter/parser work positions');
  WriteLn('historical restart cancellation / refused=', LCancelled, ' completed=', LCompleted);
  LStop := TRestartStop.Create;
  LStop.StopAfter := 1000000;
  LScope := LStop;
  LCandidate := Revised(LBase, LText, LScope);
  LScope.Cancel;
  Check(SnapshotText(LCandidate) = LExpected, 'Published history/metadata owns no live cancellation or execution dependency');
end;

{$ifndef PAS2JS}
procedure TRestartReader.Execute;
var
  LAt: Integer;
  LLinked: INyxPascalLexicalSnapshot;
  LNext: INyxPascalSyntaxSnapshot;
begin
  try
    for LAt := 1 to 8 do
    begin
      LLinked := LinkNyxPascalLexicalRevision(Previous.Lexical, Current);
      LNext := AppendNyxPascalSyntax(Previous, LLinked, TNyxPascalSyntaxOptions.Defaults);

      if SnapshotText(LNext) <> Expected then raise Exception.Create('Concurrent history restoration lost current metadata');
      Inc(Iterations);
    end;
  except
    on E: Exception do ErrorText := E.Message;
  end;
end;

procedure ConcurrentReaders;
var
  LBase: INyxPascalSyntaxSnapshot;
  LReaders: array of TRestartReader;
  LText, LChanged: TNyxText;
  LAt: Integer;
begin
  LText := Fixture(192);
  LBase := Whole(LText);
  SetLength(LReaders, 4);
  try
    for LAt := 0 to High(LReaders) do
    begin
      LChanged := ChangeConstant(LText, 48 + LAt * 32, 9001 + LAt);
      LReaders[LAt] := TRestartReader.Create(True);
      LReaders[LAt].FreeOnTerminate := False;
      LReaders[LAt].Previous := LBase;
      LReaders[LAt].Current := Arena(LChanged);
      LReaders[LAt].Expected := SnapshotText(Whole(LChanged));
      LReaders[LAt].Start;
    end;
    for LAt := 0 to High(LReaders) do
    begin
      LReaders[LAt].WaitFor;
      Check((LReaders[LAt].Iterations = 8) and (LReaders[LAt].ErrorText = ''),
        'Concurrent readers own independent middle-edit restarts: ' + LReaders[LAt].ErrorText);
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
  WriteLn('native concurrent historical restarts / 32 joined');
end;
{$endif}

begin
  try
    MiddleEdits;
    BranchesAndRetirement;
    Recovery;
    FragmentRestarts;
    AtomicRefusal;
    {$ifndef PAS2JS}ConcurrentReaders;{$endif}
    WriteLn('PASS ', GChecks, ' earlier parser/filter restart checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', GChecks, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
