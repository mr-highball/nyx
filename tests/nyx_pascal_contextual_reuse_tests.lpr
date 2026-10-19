{ Physical row donation must run current callbacks and equal fresh current
  construction. Immutable input identity grants no previous compiler meaning. }
program nyx_pascal_contextual_reuse_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, {$ifndef PAS2JS}Classes,{$endif} nyx.text, nyx.bytes, nyx.scheduler, nyx.editing,
  nyx.pascal.lexing, nyx.pascal.lexing.append, nyx.pascal.work,
  nyx.pascal.encoding, nyx.pascal.files, nyx.pascal.context,
  nyx.pascal.translation, nyx.pascal.syntax, nyx.pascal.syntax.translated,
  nyx.pascal.conditions, nyx.pascal.binding, nyx.pascal.names,
  nyx.pascal.project, nyx.pascal.drafts, nyx.pascal.sources,
  nyx.pascal.files.project, nyx.pascal.completion, nyx.pascal.completion.owned;

type
  { Alternate mutable text is copied once, never trusted as an input owner. }
  TInputSpy = class(TInterfacedObject, INyxPascalLexicalInput)
  public
    Value: TNyxText;
    Reads: Integer;
    function Text: TNyxText;
  end;
  { Alternate inspection callbacks may fail after earlier files were copied.
    Their buffer inputs must acquire fresh ownership only on complete capture;
    an in-place failed capture must preserve the original provider reference. }
  TSourceSpy = class(TInterfacedObject, INyxPascalSourceSnapshot)
  public
    Current: INyxPascalSourceSnapshot;
    Reads, FailAt: Integer;
    procedure Read;
    function Kind: TNyxPascalSourceSnapshotKind;
    function MainUnit: TNyxPascalUnitRef;
    function Limits: TNyxPascalProjectLimits;
    function Count: Integer;
    function FileReference(AIndex: Integer): TNyxPascalSourceRef;
    function Text(const AFile: TNyxPascalSourceRef): TNyxText;
    function AcceptedText(const AFile: TNyxPascalSourceRef): TNyxText;
    function WithBuffer(const AFile: TNyxPascalSourceRef; const AText: TNyxText): INyxPascalSourceSnapshot;
  end;
  { Deterministic cancellation scope owns no source/reader; callbacks may cancel
    it at ordinary rows or EOF before any complete snapshot can publish. }
  TStop = class(TInterfacedObject, INyxCancellationScope)
  public
    Stopped: Boolean;
    procedure Cancel;
    function GetCancelled: Boolean;
  end;
  { Fresh callback owner for each scan. Captures are revocable reader borrows;
    frozen prefixes remain independent. Transcript includes every current row. }
  TObserver = class(TInterfacedObject, INyxPascalLexicalObserver,
    INyxPascalLexicalConstructionObserver)
  public
    Transcript: TNyxText;
    Count, FreezeAt, CancelAt, FailAt: Integer;
    Capture: INyxPascalLexicalPrefixCapture;
    Frozen: INyxPascalLexicalPrefix;
    Cancellation: INyxCancellationScope;
    Mutate: TInputSpy;
    constructor Create;
    procedure BeginRead(const ACapture: INyxPascalLexicalPrefixCapture);
    procedure AfterToken(AIndex: Integer; const AToken: TNyxPascalToken;
      const AText: TNyxText; var AAllowNestedComments: Boolean); virtual;
  end;
  { Current region authority can diverge then converge at an explicit physical
    directive. This is a lexical policy fixture, not compiler/assembly admission. }
  TRegionObserver = class(TObserver, INyxPascalLexicalRegionObserver)
  public
    Region: TNyxPascalLexicalRegion;
    function InitialRegion: TNyxPascalLexicalRegion;
    function NextRegion: TNyxPascalLexicalRegion;
    procedure AfterToken(AIndex: Integer; const AToken: TNyxPascalToken;
      const AText: TNyxText; var AAllowNestedComments: Boolean); override;
  end;
  {$ifndef PAS2JS}
  { Readers share immutable input/donor only. Each callback/result is local and
    main joins before inspecting counters or retiring worker leases. }
  TReplayReader = class(TThread)
  protected
    procedure Execute; override;
  public
    Input: INyxPascalLexicalInput;
    Previous: INyxPascalLexicalSnapshot;
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

function TInputSpy.Text: TNyxText;
begin Inc(Reads); Result := Value; end;
procedure TSourceSpy.Read;
begin
  Inc(Reads);

  if (FailAt > 0) and (Reads >= FailAt) then raise ENyxPascalProject.Create('Source spy refused');
end;
function TSourceSpy.Kind: TNyxPascalSourceSnapshotKind;
begin Read; Result := Current.Kind; end;
function TSourceSpy.MainUnit: TNyxPascalUnitRef;
begin Read; Result := Current.MainUnit; end;
function TSourceSpy.Limits: TNyxPascalProjectLimits;
begin Read; Result := Current.Limits; end;
function TSourceSpy.Count: Integer;
begin Read; Result := Current.Count; end;
function TSourceSpy.FileReference(AIndex: Integer): TNyxPascalSourceRef;
begin Read; Result := Current.FileReference(AIndex); end;
function TSourceSpy.Text(const AFile: TNyxPascalSourceRef): TNyxText;
begin Read; Result := Current.Text(AFile); end;
function TSourceSpy.AcceptedText(const AFile: TNyxPascalSourceRef): TNyxText;
begin Read; Result := Current.AcceptedText(AFile); end;
function TSourceSpy.WithBuffer(const AFile: TNyxPascalSourceRef; const AText: TNyxText): INyxPascalSourceSnapshot;
begin Read; Result := Current.WithBuffer(AFile, AText); end;
procedure TStop.Cancel;
begin Stopped := True; end;
function TStop.GetCancelled: Boolean;
begin Result := Stopped; end;

constructor TObserver.Create;
begin
  inherited Create;
  FreezeAt := -1;
  CancelAt := -1;
  FailAt := -1;
end;

procedure TObserver.BeginRead(const ACapture: INyxPascalLexicalPrefixCapture);
begin
  Capture := ACapture;

  if Mutate <> nil then Mutate.Value := 'changed after capture';
end;

procedure TObserver.AfterToken(AIndex: Integer; const AToken: TNyxPascalToken;
  const AText: TNyxText; var AAllowNestedComments: Boolean);
begin
  Inc(Count);
  Transcript := Transcript + TNyxText(IntToStr(AIndex)) + ':' + AText + '|';

  if AText = '{$nestedcomments on}' then AAllowNestedComments := True;

  if AText = '{$nestedcomments off}' then AAllowNestedComments := False;

  if AIndex = FreezeAt then Frozen := Capture.Freeze;

  if (AIndex = CancelAt) and (Cancellation <> nil) then Cancellation.Cancel;

  if AIndex = FailAt then raise ENyxPascalLexical.Create('Observer refused this row');
end;

function TRegionObserver.InitialRegion: TNyxPascalLexicalRegion;
begin Result := Region; end;
function TRegionObserver.NextRegion: TNyxPascalLexicalRegion;
begin Result := Region; end;
procedure TRegionObserver.AfterToken(AIndex: Integer; const AToken: TNyxPascalToken;
  const AText: TNyxText; var AAllowNestedComments: Boolean);
begin
  inherited AfterToken(AIndex, AToken, AText, AAllowNestedComments);

  if (AText = '{$assembly}') or (AToken.Word = pkwAsm) then Region := plrAssembly;

  if (AText = '{$pascal}') or (AToken.Word = pkwEnd) then Region := plrPascal;
end;

procedure SameLexical(const ACold, ARevised: INyxPascalLexicalSnapshot);
var
  LAt: Integer;
  LLeft, LRight: TNyxPascalToken;
begin
  Check((ACold.Source = ARevised.Source) and (ACold.Count = ARevised.Count) and
    (ACold.Scalars = ARevised.Scalars) and (ACold.DiagnosticCount = ARevised.DiagnosticCount),
    'Current source, Unicode length, tokens and diagnostic counts equal fresh construction');
  for LAt := 0 to ACold.Count - 1 do
  begin
    LLeft := ACold.Token(LAt); LRight := ARevised.Token(LAt);
    Check((ACold.Text(LAt) = ARevised.Text(LAt)) and
      (LLeft.Kind = LRight.Kind) and (LLeft.Word = LRight.Word) and
      (LLeft.Symbol = LRight.Symbol) and (LLeft.NumberBase = LRight.NumberBase) and
      (LLeft.RealNumber = LRight.RealNumber) and (LLeft.Escaped = LRight.Escaped) and
      (LLeft.Complete = LRight.Complete) and (LLeft.Region = LRight.Region) and
      (LLeft.Line = LRight.Line) and (LLeft.Column = LRight.Column) and
      (LLeft.Span.Start = LRight.Span.Start) and (LLeft.Span.Length = LRight.Span.Length),
      'Every current token payload, exact spelling and coordinate equals fresh scanning');
  end;
  for LAt := 0 to ACold.DiagnosticCount - 1 do
    Check((ACold.Diagnostic(LAt).Issue = ARevised.Diagnostic(LAt).Issue) and
      (ACold.Diagnostic(LAt).Span.Start = ARevised.Diagnostic(LAt).Span.Start) and
      (ACold.Diagnostic(LAt).Span.Length = ARevised.Diagnostic(LAt).Span.Length),
      'Current recovery diagnostics are reconstructed exactly');
end;

function Scan(const AInput: INyxPascalLexicalInput; const AObserver: INyxPascalLexicalObserver;
  const AOptions: TNyxPascalLexicalOptions; const APrevious: INyxPascalLexicalSnapshot = nil;
  const AExecution: INyxCancellationScope = nil): INyxPascalLexicalSnapshot;
begin
  Result := NewNyxPascalContextualLexicalSnapshot(AInput, AOptions, AObserver, AExecution, APrevious);
end;

procedure PhysicalRows;
const
  CText: TNyxText = 'unit Example; interface const Text=''Hello 😀'';' + #13#10 +
    'Value=42; implementation end.' + #13;
var
  LInput, LOther: INyxPascalLexicalInput;
  LCold, LNext, LRejected: INyxPascalLexicalSnapshot;
  LOldObserver, LObserver: TObserver;
  LOldLease, LLease: INyxPascalLexicalObserver;
  LWork: INyxPascalContextualLexicalWork;
  LBudget: INyxPascalAnalysisBudget;
  LView: INyxPascalSourceView;
  LBefore: TNyxText;
  LAt, LFit: Integer;
  LRefused: Boolean;
begin
  LInput := NewNyxPascalLexicalInput(CText);
  LView := NyxPascalSourceViewFromLexicalInput(LInput);
  Check((NyxPascalSourceLexicalInput(LView) = LInput) and (LView.Text = CText) and
    LView.Original.SameBytes(NewNyxPascalSourceBytes(NyxEncodeUTF8(CText), pboAuthoringUTF8)),
    'The source bridge preserves exact Unicode/CRLF and canonical authoring byte provenance');
  LOldObserver := TObserver.Create; LOldLease := LOldObserver;
  LCold := Scan(LInput, LOldLease, TNyxPascalLexicalOptions.Defaults);
  LBefore := LOldObserver.Transcript;
  LObserver := TObserver.Create; LLease := LObserver; LObserver.FreezeAt := 3;
  LNext := Scan(LInput, LLease, TNyxPascalLexicalOptions.Defaults, LCold);
  SameLexical(LCold, LNext);
  Supports(LNext, INyxPascalContextualLexicalWork, LWork);
  Check((LWork.ScannedTokens = 0) and (LWork.ReusedTokens = LCold.Count) and
    (LWork.CandidateReads = LCold.Count), 'Exact input/context replays all physical rows, including EOF, without classifying them again');
  Check((LObserver.Transcript = LBefore) and (LObserver.Count = LCold.Count), 'Every current callback executes once in original physical order');
  Check((LObserver.Frozen.Count = 4) and (LObserver.Frozen.ScannedScalars = LCold.Token(3).Span.Finish),
    'A callback freeze contains only current rows already emitted');
  LRefused := False;
  try LObserver.Capture.Freeze; except on ENyxPascalLexical do LRefused := True; end;
  Check(LRefused, 'The current capture borrow retires after replay just as after scanning');

  LOther := NewNyxPascalLexicalInput(CText);
  LObserver := TObserver.Create; LLease := LObserver;
  LNext := Scan(LOther, LLease, TNyxPascalLexicalOptions.Defaults, LCold);
  Supports(LNext, INyxPascalContextualLexicalWork, LWork);
  Check((LWork.ReusedTokens = 0) and (LWork.ScannedTokens = LNext.Count), 'Equal text from another owner cannot donate physical rows');
  LObserver := TObserver.Create; LLease := LObserver;
  LNext := Scan(LInput, LLease, TNyxPascalLexicalOptions.Defaults.UnicodeIdentifiers(True), LCold);
  Supports(LNext, INyxPascalContextualLexicalWork, LWork);
  Check(LWork.ReusedTokens = 0, 'Changed fixed identifier policy declines donation');
  LObserver := TObserver.Create; LLease := LObserver;
  LNext := Scan(LInput, LLease, TNyxPascalLexicalOptions.Defaults,
    NewNyxPascalAppendLexical(CText, TNyxPascalLexicalOptions.Defaults));
  Supports(LNext, INyxPascalContextualLexicalWork, LWork);
  Check(LWork.ReusedTokens = 0, 'Another producer cannot grant completed contextual row authority');

  LFit := -1;
  for LAt := 1 to 1000 do
  begin
    LObserver := TObserver.Create; LLease := LObserver;
    LBudget := NewNyxPascalAnalysisBudget(LAt);
    LRejected := nil;
    LRefused := False;
    try LRejected := Scan(LInput, LLease, TNyxPascalLexicalOptions.Defaults, LCold, LBudget);
    except on ENyxPascalAnalysisWorkLimit do LRefused := True; end;

    if not LRefused then
    begin
      SameLexical(LCold, LRejected);
      LFit := LAt;
      LBudget.Retire;
      Break;
    end;
    Check((LRejected = nil) and (LBudget.Receipt.Total <= LAt), 'Every smaller reuse allowance refuses without publishing');
    LBudget.Retire;
  end;
  Check((LFit = LCold.Count + 1) and (LOldObserver.Transcript = LBefore),
    'Source proof plus every physical copy remain charged; the previous snapshot/callback never changes');
  WriteLn('contextual physical reuse / rows=', LCold.Count, ' scans=0 first-fit=', LFit);
end;

procedure ChangedContexts;
var
  LInput: INyxPascalLexicalInput;
  LOld, LCold, LNext: INyxPascalLexicalSnapshot;
  LObserver, LColdObserver: TObserver;
  LRegion, LColdRegion: TRegionObserver;
  LLease, LColdLease: INyxPascalLexicalObserver;
  LWork: INyxPascalContextualLexicalWork;
begin
  LInput := NewNyxPascalLexicalInput('{ outer { inner } tail } {$nestedcomments on} {yes { nested }} value:=1;');
  LObserver := TObserver.Create; LLease := LObserver;
  LOld := Scan(LInput, LLease, TNyxPascalLexicalOptions.Defaults);
  LColdObserver := TObserver.Create; LColdLease := LColdObserver;
  LCold := Scan(LInput, LColdLease, TNyxPascalLexicalOptions.Defaults.NestedComments(True));
  LObserver := TObserver.Create; LLease := LObserver;
  LNext := Scan(LInput, LLease, TNyxPascalLexicalOptions.Defaults.NestedComments(True), LOld);
  SameLexical(LCold, LNext);
  Supports(LNext, INyxPascalContextualLexicalWork, LWork);
  Check((LWork.ScannedTokens > 0) and (LWork.ReusedTokens > 0) and
    (LObserver.Transcript = LColdObserver.Transcript), 'Different nesting scans affected rows and resynchronizes only at exact current boundaries');

  LInput := NewNyxPascalLexicalInput('foo eax+1; {$pascal} value:=3;');
  LRegion := TRegionObserver.Create; LLease := LRegion; LRegion.Region := plrPascal;
  LOld := Scan(LInput, LLease, TNyxPascalLexicalOptions.Defaults);
  LColdRegion := TRegionObserver.Create; LColdLease := LColdRegion; LColdRegion.Region := plrAssembly;
  LCold := Scan(LInput, LColdLease, TNyxPascalLexicalOptions.Defaults);
  LRegion := TRegionObserver.Create; LLease := LRegion; LRegion.Region := plrAssembly;
  LNext := Scan(LInput, LLease, TNyxPascalLexicalOptions.Defaults, LOld);
  SameLexical(LCold, LNext);
  Supports(LNext, INyxPascalContextualLexicalWork, LWork);
  Check((LWork.ScannedTokens > 0) and (LWork.ReusedTokens > 0) and
    (LRegion.Transcript = LColdRegion.Transcript), 'Changed assembly context rescans before the current region converges');

  LInput := NewNyxPascalLexicalInput('a:=1e+; ' + '''unfinished' + #13#10 + 'asm mov eax,1');
  LObserver := TObserver.Create; LLease := LObserver;
  LOld := Scan(LInput, LLease, TNyxPascalLexicalOptions.Defaults);
  LObserver := TObserver.Create; LLease := LObserver;
  LNext := Scan(LInput, LLease, TNyxPascalLexicalOptions.Defaults, LOld);
  SameLexical(LOld, LNext);
  Supports(LNext, INyxPascalContextualLexicalWork, LWork);
  Check((LOld.DiagnosticCount >= 3) and (LWork.ScannedTokens > 0) and (LWork.ReusedTokens > 0),
    'Recovery/assembly EOF diagnostics are reconstructed rather than dropped by reuse');
end;

procedure CancellationAndInput;
var
  LSpy: TInputSpy;
  LInput: INyxPascalLexicalInput;
  LOld, LNext: INyxPascalLexicalSnapshot;
  LObserver: TObserver;
  LLease: INyxPascalLexicalObserver;
  LStop: INyxCancellationScope;
  LMode: Integer;
  LRefused: Boolean;
begin
  LSpy := TInputSpy.Create; LInput := LSpy; LSpy.Value := 'stable:=42;';
  LObserver := TObserver.Create; LLease := LObserver;
  LStop := TStop.Create; LStop.Cancel;
  LNext := nil;
  LRefused := False;
  try LNext := Scan(LInput, LLease, TNyxPascalLexicalOptions.Defaults, nil, LStop);
  except on ENyxPascalAnalysisCancelled do LRefused := True; end;
  Check(LRefused and (LNext = nil) and (LSpy.Reads = 0), 'Entry cancellation precedes alternate input callbacks');
  LObserver := TObserver.Create; LLease := LObserver; LObserver.Mutate := LSpy;
  LOld := Scan(LInput, LLease, TNyxPascalLexicalOptions.Defaults);
  Check((LOld.Source = TNyxText('stable:=42;')) and (LSpy.Reads = 1),
    'Alternate text freezes once before callbacks can mutate its provider');
  LInput := NewNyxPascalLexicalInput('stable:=42;');
  LObserver := TObserver.Create; LLease := LObserver;
  LOld := Scan(LInput, LLease, TNyxPascalLexicalOptions.Defaults);
  for LMode := 0 to 2 do
  begin
    LObserver := TObserver.Create; LLease := LObserver;
    LStop := TStop.Create; LObserver.Cancellation := LStop;

    if LMode = 0 then LObserver.CancelAt := 0
    else if LMode = 1 then LObserver.CancelAt := LOld.Count - 1
    else LObserver.FailAt := 2;
    LNext := nil;
    LRefused := False;
    try LNext := Scan(LInput, LLease, TNyxPascalLexicalOptions.Defaults, LOld, LStop);
    except
      on ENyxPascalAnalysisCancelled do LRefused := LMode < 2;
      on E: ENyxPascalLexical do LRefused := (LMode = 2) and (E.Message = 'Observer refused this row');
    end;
    Check(LRefused and (LNext = nil) and (LOld.Source = LInput.Text),
      'Callback cancellation at ordinary rows/EOF and callback refusal publish no partial snapshot');
  end;
  LObserver := TObserver.Create; LLease := LObserver;
  LOld := Scan(LInput, LLease, TNyxPascalLexicalOptions.Defaults, LOld);
  Check(LOld.Count > 1, 'In-place replay keeps its donor lease alive through publication');
end;

function Facts: INyxPascalCompilerContext;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi).CompleteDefines(True);
end;

procedure SameTranslation(const ACold, ARevised: INyxPascalTranslation);
var
  LAt, LLevel: Integer;
  LLeft, LRight: TNyxPascalTranslatedToken;
  LInclusion, LOther: TNyxPascalInclusion;
  LSettings, LOtherSettings: INyxPascalTranslationSettings;
  LMacros, LOtherMacros: INyxPascalTranslationMacros;
begin
  Check(ACold.Files.SameFiles(ARevised.Files) and (ACold.Resolved = ARevised.Resolved) and
    (ACold.Count = ARevised.Count) and (ACold.OccurrenceCount = ARevised.OccurrenceCount) and
    (ACold.StateCount = ARevised.StateCount) and (ACold.DiagnosticCount = ARevised.DiagnosticCount),
    'Current translation keeps exact inputs, state shape, origins and diagnostics');
  for LAt := 0 to ACold.Count - 1 do
  begin
    LLeft := ACold.Token(LAt); LRight := ARevised.Token(LAt);
    Check((ACold.Text(LAt) = ARevised.Text(LAt)) and
      (NyxPascalEffectiveText(ACold, LAt) = NyxPascalEffectiveText(ARevised, LAt)) and
      (LLeft.Occurrence = LRight.Occurrence) and (LLeft.PhysicalToken = LRight.PhysicalToken) and
      (LLeft.State = LRight.State) and (LLeft.Activity = LRight.Activity) and
      (LLeft.Expansion = LRight.Expansion) and (LLeft.ExpansionToken = LRight.ExpansionToken) and
      (LLeft.Expanded = LRight.Expanded), 'Every translated/effective token uses its current occurrence, compiler state and macro expansion');
  end;
  Supports(ACold, INyxPascalTranslationSettings, LSettings);
  Supports(ARevised, INyxPascalTranslationSettings, LOtherSettings);
  Supports(ACold, INyxPascalTranslationMacros, LMacros);
  Supports(ARevised, INyxPascalTranslationMacros, LOtherMacros);
  for LAt := 0 to ACold.StateCount - 1 do
  begin
    Check(ACold.State(LAt).SameFacts(ARevised.State(LAt)) and
      (LSettings.SettingsKnown(LAt) = LOtherSettings.SettingsKnown(LAt)) and
      (LSettings.SettingsDepth(LAt) = LOtherSettings.SettingsDepth(LAt)) and
      LMacros.StateMacros(LAt).SameMacros(LOtherMacros.StateMacros(LAt)),
      'Current compiler/macro/settings states equal independent fresh evaluation');
    for LLevel := 0 to LSettings.SettingsDepth(LAt) - 1 do
      Check(LSettings.SavedSettings(LAt, LLevel).SameFacts(LOtherSettings.SavedSettings(LAt, LLevel)),
        'Saved local compiler settings are reconstructed too');
  end;
  for LAt := 0 to ACold.OccurrenceCount - 1 do
  begin
    LInclusion := ACold.Occurrence(LAt); LOther := ARevised.Occurrence(LAt);
    Check((LInclusion.FileIndex = LOther.FileIndex) and (LInclusion.Parent = LOther.Parent) and
      (LInclusion.IncludeSpan.Start = LOther.IncludeSpan.Start) and
      (LInclusion.IncludeSpan.Length = LOther.IncludeSpan.Length) and
      (LInclusion.FirstToken = LOther.FirstToken) and (LInclusion.TokenCount = LOther.TokenCount) and
      (LInclusion.EntryState = LOther.EntryState) and (LInclusion.ExitState = LOther.ExitState),
      'Repeated includes retain exact current occurrence/entry/exit boundaries');
    SameLexical(NyxPascalOccurrenceLexical(ACold, LAt), NyxPascalOccurrenceLexical(ARevised, LAt));
  end;
  for LAt := 0 to ACold.DiagnosticCount - 1 do
    Check((ACold.Diagnostic(LAt).Issue = ARevised.Diagnostic(LAt).Issue) and
      (ACold.Diagnostic(LAt).Occurrence = ARevised.Diagnostic(LAt).Occurrence) and
      (ACold.Diagnostic(LAt).Span.Start = ARevised.Diagnostic(LAt).Span.Start) and
      (ACold.Diagnostic(LAt).Span.Length = ARevised.Diagnostic(LAt).Span.Length) and
      (ACold.Diagnostic(LAt).MessageText = ARevised.Diagnostic(LAt).MessageText), 'Current translation diagnostics remain exact');
  Check(SnapshotText(NewNyxPascalTranslatedSyntax(ACold, TNyxPascalSyntaxOptions.Defaults).Syntax) =
    SnapshotText(NewNyxPascalTranslatedSyntax(ARevised, TNyxPascalSyntaxOptions.Defaults).Syntax),
    'Every public parser metadata/index family equals independent fresh current construction');
end;

procedure TranslationAndIncludes;
var
  LFiles, LChanged: INyxPascalFileCatalog;
  LOld, LCold, LNext: INyxPascalTranslation;
  LWork: INyxPascalTranslationLexicalWork;
  LContext: INyxPascalCompilerContext;
begin
  LContext := Facts;
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('main.pas'), 'unit Main; interface {$define First}{$i values.inc} implementation end.')
    .WithFile(NyxPascalPhysicalFile('values.inc'), '{$push}{$warnings off}{$ifdef First}const FirstValue=1;{$else}const SecondValue=2;{$endif}{$pop}');
  LOld := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('main.pas'), LContext, TNyxPascalTranslationOptions.Defaults);
  LChanged := LFiles.WithFile(NyxPascalPhysicalFile('main.pas'), 'unit Main; interface {$undef First}{$i values.inc} implementation end.');
  LCold := NewNyxPascalTranslation(LChanged, NyxPascalPhysicalFile('main.pas'), LContext, TNyxPascalTranslationOptions.Defaults);
  LNext := ReviseNyxPascalTranslation(LOld, LChanged, NyxPascalPhysicalFile('main.pas'), LContext, TNyxPascalTranslationOptions.Defaults);
  SameTranslation(LCold, LNext);
  Supports(LNext, INyxPascalTranslationLexicalWork, LWork);
  Check(LNext.Resolved and (LWork.ReusedLexicalTokens > 0) and (LWork.ScannedLexicalTokens > 0),
    'A changed root reuses unchanged include rows while reevaluating its changed define/conditional meaning');
  LFiles := LChanged.WithFile(NyxPascalPhysicalFile('main.pas'),
    'unit Main; interface {$define First}{$i values.inc}{$undef First}{$i values.inc} implementation end.');
  LCold := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('main.pas'), LContext, TNyxPascalTranslationOptions.Defaults);
  LNext := ReviseNyxPascalTranslation(LOld, LFiles, NyxPascalPhysicalFile('main.pas'), LContext, TNyxPascalTranslationOptions.Defaults);
  SameTranslation(LCold, LNext);
  Supports(LCold, INyxPascalTranslationLexicalWork, LWork);
  Check(LWork.ReusedLexicalTokens > 0, 'Repeated occurrences in one construction can reuse physical rows with independent current state');
  LOld := nil;
  LFiles := nil;
  Check(LNext.OccurrenceCount = 3, 'Current physical rows outlive the donor and preserve both independent include visits');
end;

procedure IncludeSideEffects;
var
  LFiles, LChanged: INyxPascalFileCatalog;
  LOld, LCold, LNext: INyxPascalTranslation;
  LWork: INyxPascalTranslationLexicalWork;
  LAt: Integer;
  LExpandedTwo: Boolean;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('main.pas'), 'unit Main; interface {$i settings.inc}' +
      '{ outer { inner } ''tail }' + #13#10 +
      '{$nestedcomments on} const Value=42; implementation end.')
    .WithFile(NyxPascalPhysicalFile('settings.inc'), '{$nestedcomments off}');
  LOld := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('main.pas'), Facts, TNyxPascalTranslationOptions.Defaults);
  LChanged := LFiles.WithFile(NyxPascalPhysicalFile('settings.inc'), '{$nestedcomments on}');
  LCold := NewNyxPascalTranslation(LChanged, NyxPascalPhysicalFile('main.pas'), Facts, TNyxPascalTranslationOptions.Defaults);
  LNext := ReviseNyxPascalTranslation(LOld, LChanged, NyxPascalPhysicalFile('main.pas'), Facts, TNyxPascalTranslationOptions.Defaults);
  SameTranslation(LCold, LNext);
  Supports(LNext, INyxPascalTranslationLexicalWork, LWork);
  Check(LNext.Resolved and not LOld.Resolved and (LWork.ScannedLexicalTokens > 0) and
    (LWork.ReusedLexicalTokens > 0), 'An edited child changes parent comment scanning and removes obsolete lexical diagnostics');

  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('main.pas'), 'unit Main; interface {$macro on}{$define NUMBER:=1}{$i values.inc} implementation end.')
    .WithFile(NyxPascalPhysicalFile('values.inc'), 'const Expanded=NUMBER;');
  LOld := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('main.pas'), Facts, TNyxPascalTranslationOptions.Defaults);
  LChanged := LFiles.WithFile(NyxPascalPhysicalFile('main.pas'),
    'unit Main; interface {$macro on}{$define NUMBER:=2}{$i values.inc} implementation end.');
  LCold := NewNyxPascalTranslation(LChanged, NyxPascalPhysicalFile('main.pas'), Facts, TNyxPascalTranslationOptions.Defaults);
  LNext := ReviseNyxPascalTranslation(LOld, LChanged, NyxPascalPhysicalFile('main.pas'), Facts, TNyxPascalTranslationOptions.Defaults);
  SameTranslation(LCold, LNext);
  LExpandedTwo := False;
  for LAt := 0 to LNext.Count - 1 do
  begin

    if LNext.Token(LAt).Expanded and (NyxPascalEffectiveText(LNext, LAt) = '2') then LExpandedTwo := True;
  end;
  Check(LNext.Resolved and LExpandedTwo, 'Reused physical words expand through the current macro environment');
end;

function World(AFlag: Boolean): INyxPascalSemanticWorld;
var
  LText: TNyxText;
  LSource: INyxPascalTranslation;
begin
  LText := 'unit Widgets; interface const ';

  if AFlag then LText := LText + 'Flag=1;' else LText := LText + 'Other=1;';
  LText := LText + ' implementation end.';
  LSource := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('widgets.pas'), LText), NyxPascalPhysicalFile('widgets.pas'),
    Facts, TNyxPascalTranslationOptions.Defaults);
  Result := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults.WithImplicitScopes(pispDeclarationNames))
    .WithModule(NewNyxPascalTranslatedSyntax(LSource, TNyxPascalSyntaxOptions.Defaults));
end;

procedure CurrentSemanticWorld;
var
  LFiles: INyxPascalFileCatalog;
  LOld, LCold, LNext: INyxPascalSemanticTranslation;
  LWork: INyxPascalTranslationLexicalWork;
  LView: INyxPascalTranslatedSyntax;
  LWorld: INyxPascalSemanticWorld;
  LBudget: INyxPascalAnalysisBudget;
  LRejected: Boolean;
  LAt, LFit: Integer;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('main.pas'), 'unit Main; interface uses Widgets; {$i choice.inc} implementation end.')
    .WithFile(NyxPascalPhysicalFile('choice.inc'), '{$if declared(Widgets.Flag)}const FirstChoice=1;{$else}const SecondChoice=2;{$endif}');
  LOld := NewNyxPascalSemanticTranslation(LFiles, NyxPascalPhysicalFile('main.pas'), Facts, World(True),
    TNyxPascalTranslationOptions.Defaults, TNyxPascalSyntaxOptions.Defaults, TNyxPascalConditionalOptions.Defaults);
  Check(not LOld.Pending and LOld.Source.Resolved and (LOld.Answers = 1), 'The previous source really answered its imported-symbol condition');
  LCold := NewNyxPascalSemanticTranslation(LFiles, NyxPascalPhysicalFile('main.pas'), Facts, World(False),
    TNyxPascalTranslationOptions.Defaults, TNyxPascalSyntaxOptions.Defaults, TNyxPascalConditionalOptions.Defaults);
  LNext := ReviseNyxPascalSemanticTranslation(LOld.Source, LFiles, NyxPascalPhysicalFile('main.pas'), Facts, World(False),
    TNyxPascalTranslationOptions.Defaults, TNyxPascalSyntaxOptions.Defaults, TNyxPascalConditionalOptions.Defaults);
  SameTranslation(LCold.Source, LNext.Source);
  Supports(LNext.Source, INyxPascalTranslationLexicalWork, LWork);
  Check(not LNext.Pending and LNext.Source.Resolved and (LNext.Answers = 1) and
    (LWork.ReusedLexicalTokens > 0) and (LWork.ScannedLexicalTokens = 0),
    'Every physical row can replay while the imported-symbol answer is freshly reevaluated');
  LView := NewNyxPascalTranslatedSyntax(LNext.Source, TNyxPascalSyntaxOptions.Defaults);
  Check((LView.Syntax.Named(0, 'SecondChoice') >= 0) and (LView.Syntax.Named(0, 'FirstChoice') < 0),
    'The new current-world branch wins; previous semantic answers never cross lexical donation');
  LWorld := World(False);
  LFit := -1;
  for LAt := 1 to 10000 do
  begin
    LBudget := NewNyxPascalAnalysisBudget(LAt);
    LNext := nil;
    LRejected := False;
    try
      LNext := ReviseNyxPascalSemanticTranslation(LOld.Source, LFiles, NyxPascalPhysicalFile('main.pas'), Facts, LWorld,
        TNyxPascalTranslationOptions.Defaults, TNyxPascalSyntaxOptions.Defaults, TNyxPascalConditionalOptions.Defaults, LBudget);
    except
      on ENyxPascalConditionalWorkLimit do LRejected := True;
      on ENyxPascalAnalysisWorkLimit do LRejected := True;
    end;

    if not LRejected then
    begin
      Check(not LNext.Pending and LNext.Source.Resolved and (LNext.Answers = 1),
        'The first admitted nested-budget construction returns the complete current-world answer');
      LFit := LAt;
      LBudget.Retire;
      Break;
    end;
    Check((LNext = nil) and (LBudget.Receipt.Total <= LAt), 'Every smaller semantic-revision allowance refuses atomically without a cold retry');
    LBudget.Retire;
  end;
  Check(LFit > 1, 'Physical reuse and current semantic work remain inside the caller aggregate budget');
  WriteLn('contextual semantic reuse / first-fit=', LFit, ' every smaller allowance refused');
end;

procedure AlternateSourceCapture;
var
  LSpy: TSourceSpy;
  LExternal, LOriginal, LCaptured: INyxPascalSourceSnapshot;
  LFile: TNyxPascalSourceRef;
  LRejected: Boolean;
begin
  LFile := TNyxPascalSourceRef.IncludeFile(NyxPascalInclude('fields.inc'));
  LSpy := TSourceSpy.Create;
  LExternal := LSpy;
  LOriginal := LExternal;
  LSpy.Current := CaptureNyxPascalSources(NewNyxPascalProject(NyxPascalUnit('Input'),
    'unit Input; interface implementation end.').WithInclude(NyxPascalInclude('fields.inc'), 'Value:LongInt;'));
  Check(NyxPascalSourcesLexicalInput(LExternal, LFile) = nil, 'An alternate source snapshot supplies no sealed per-buffer input identity');
  LSpy.FailAt := 8;
  LRejected := False;
  try LExternal := CaptureNyxPascalSources(LExternal);
  except on ENyxPascalProject do LRejected := True; end;
  Check(LRejected and (LExternal = LOriginal), 'Late in-place source capture failure preserves its exact original provider');
  LSpy.FailAt := 0;
  LCaptured := CaptureNyxPascalSources(LExternal);
  Check((NyxPascalSourcesLexicalInput(LCaptured, LFile) <> nil) and
    (NyxPascalSourcesLexicalInput(LCaptured, LFile) <> NyxPascalSourcesLexicalInput(LSpy.Current, LFile)),
    'A fully validated alternate snapshot receives new immutable text owners rather than borrowed identities');
  LSpy.Current := LSpy.Current.WithBuffer(LFile, 'Different:Byte;');
  Check(LCaptured.Text(LFile) = TNyxText('Value:LongInt;'), 'Later provider edits cannot change captured text or its lexical owner');
end;

procedure ProjectBuffersAndProvider;
const
  CMain: TNyxText = 'unit Consumer; interface type TWidget=record {$i fields.inc} end; implementation ' +
    'procedure Run; var Item:TWidget; begin Item.Val; end; end.';
var
  LSources, LChanged, LCaptured: INyxPascalSourceSnapshot;
  LInput: INyxPascalLexicalInput;
  LRoots: array of TNyxPascalPhysicalFile;
  LProvider, LColdProvider: INyxPascalCompletionProvider;
  LReply, LColdReply: INyxPascalCompletionResult;
  LWork: INyxPascalCompletionLexicalWork;
  LOptions: TNyxOwnedPascalCompletionOptions;
  LRequest: INyxPascalCompletionRequest;
  LFiles: INyxPascalFileCatalog;
  LAt: Integer;
begin
  LSources := CaptureNyxPascalSources(NewNyxPascalProject(NyxPascalUnit('Consumer'), CMain)
    .WithInclude(NyxPascalInclude('fields.inc'), 'Value:LongInt;'));
  LInput := NyxPascalSourcesLexicalInput(LSources, TNyxPascalSourceRef.IncludeFile(NyxPascalInclude('fields.inc')));
  LChanged := LSources.WithBuffer(TNyxPascalSourceRef.Main, CMain + ' ');
  LCaptured := CaptureNyxPascalSources(LChanged);
  Check(NyxPascalSourcesLexicalInput(LCaptured, TNyxPascalSourceRef.IncludeFile(NyxPascalInclude('fields.inc'))) = LInput,
    'Other-file edits and detached source capture preserve the unchanged buffer owner');
  LCaptured := CaptureNyxPascalSources(LCaptured);
  Check(LCaptured.Text(TNyxPascalSourceRef.Main) = CMain + ' ', 'In-place source capture keeps the complete current input alive');
  LFiles := CaptureNyxPascalProjectFiles(LCaptured, pfcExact);
  Check(NyxPascalSourceLexicalInput(NyxPascalSourceViewAt(LFiles,
    LFiles.Find(NyxPascalPhysicalFile('fields.inc')))) = LInput,
    'The ordinary project-file bridge preserves the unchanged buffer owner in its decoded view');
  LOptions := TNyxOwnedPascalCompletionOptions.Defaults.WithBinding(
    TNyxPascalBindingOptions.Defaults.WithImplicitScopes(pispDeclarationNames));
  SetLength(LRoots, 0);
  LProvider := NewNyxOwnedPascalCompletionProvider(Facts, NewNyxPascalFileCatalog(pfcExact), LRoots, LOptions);
  LAt := Pos('Item.Val', CMain);
  LRequest := NewNyxPascalCompletionRequest(LSources).At(TNyxPascalSourceRef.Main,
    NyxTextScalarCount(Copy(CMain, 1, LAt - 1)) + 8);
  LReply := LProvider.Complete(LRequest);
  Check((LReply.Status = pcsReady) and (LReply.Count = 1), 'The ordinary provider first constructs the project and its include');
  LRequest := NewNyxPascalCompletionRequest(LChanged).At(TNyxPascalSourceRef.Main,
    NyxTextScalarCount(Copy(CMain, 1, LAt - 1)) + 8);
  LReply := LProvider.Complete(LRequest);
  Supports(LReply, INyxPascalCompletionLexicalWork, LWork);
  Check((LReply.Status = pcsReady) and (LReply.Count = 1) and (LReply.Item(0).Name = TNyxText('Value')) and
    (LWork.ReusedLexicalTokens > 0), 'Ordinary edited-project completion actually donates unchanged project include rows');
  WriteLn('edited project physical work / scanned=', LWork.ScannedLexicalTokens,
    ' reused=', LWork.ReusedLexicalTokens, ' reads=', LWork.LexicalCandidateReads);
  LOptions := LOptions.WithSourceReuse(LOptions.SourceReuse.WithLexicalReuse(False));
  LColdProvider := NewNyxOwnedPascalCompletionProvider(Facts, NewNyxPascalFileCatalog(pfcExact), LRoots, LOptions);
  LColdReply := LColdProvider.Complete(LRequest);
  Supports(LColdReply, INyxPascalCompletionLexicalWork, LWork);
  Check((LColdReply.Status = pcsReady) and (LColdReply.Count = LReply.Count) and
    (LColdReply.Item(0).Name = LReply.Item(0).Name) and (LWork.ReusedLexicalTokens = 0),
    'Disabling previous-source physical reuse preserves the independent completion answer');
end;

{$ifndef PAS2JS}
procedure TReplayReader.Execute;
var
  LAt: Integer;
  LObserver: TObserver;
  LLease: INyxPascalLexicalObserver;
  LNext: INyxPascalLexicalSnapshot;
  LWork: INyxPascalContextualLexicalWork;
begin
  try
    for LAt := 1 to 16 do
    begin
      LObserver := TObserver.Create; LLease := LObserver;
      LNext := Scan(Input, LLease, TNyxPascalLexicalOptions.Defaults, Previous);
      Supports(LNext, INyxPascalContextualLexicalWork, LWork);

      if (LNext.Source <> Input.Text) or (LWork.ScannedTokens <> 0) or
        (LWork.ReusedTokens <> LNext.Count) or (LObserver.Count <> LNext.Count) then
        raise Exception.Create('Concurrent physical replay lost current row/callback identity');
      Inc(Iterations);
    end;
  except on E: Exception do ErrorText := E.Message; end;
end;

procedure ConcurrentReaders;
var
  LInput: INyxPascalLexicalInput;
  LOld: INyxPascalLexicalSnapshot;
  LObserver: TObserver;
  LLease: INyxPascalLexicalObserver;
  LReaders: array of TReplayReader;
  LAt: Integer;
begin
  LInput := NewNyxPascalLexicalInput('unit Parallel; interface const Value=42; implementation end.');
  LObserver := TObserver.Create; LLease := LObserver;
  LOld := Scan(LInput, LLease, TNyxPascalLexicalOptions.Defaults);
  SetLength(LReaders, 4);
  try
    for LAt := 0 to High(LReaders) do
    begin
      LReaders[LAt] := TReplayReader.Create(True);
      LReaders[LAt].FreeOnTerminate := False;
      LReaders[LAt].Input := LInput;
      LReaders[LAt].Previous := LOld;
      LReaders[LAt].Start;
    end;
    for LAt := 0 to High(LReaders) do
    begin
      LReaders[LAt].WaitFor;
      Check((LReaders[LAt].Iterations = 16) and (LReaders[LAt].ErrorText = ''),
        'Concurrent readers finish with independent current callbacks: ' + LReaders[LAt].ErrorText);
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
  WriteLn('native concurrent contextual replays / 64 joined');
end;
{$endif}

begin
  try
    PhysicalRows;
    ChangedContexts;
    CancellationAndInput;
    TranslationAndIncludes;
    IncludeSideEffects;
    CurrentSemanticWorld;
    AlternateSourceCapture;
    ProjectBuffersAndProvider;
    {$ifndef PAS2JS}ConcurrentReaders;{$endif}
    WriteLn('PASS ', GChecks, ' current contextual token reuse checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', GChecks, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
