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
program nyx_pascal_work_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.scheduler, nyx.pascal.work, nyx.pascal.lexing,
  nyx.pascal.context, nyx.pascal.files, nyx.pascal.translation,
  nyx.pascal.syntax, nyx.pascal.syntax.translated, nyx.pascal.types,
  nyx.pascal.binding, nyx.pascal.conditions;

type
  { Test-owned cancellation lease with an observable retirement. No parser or
    work is retained; destruction proves published values release this parent. }
  TParentScope = class(TInterfacedObject, INyxCancellationScope)
  private
    FCancelled: Boolean;
  public
    destructor Destroy; override;
    procedure Cancel;
    function GetCancelled: Boolean;
  end;
  { Caller execution policy cancels immediately before a selected real boundary.
    Its private numeric budget owns no source/world. Nested conditional budgets
    must preserve this caller policy rather than silently replacing it. }
  TPhaseScope = class(TInterfacedObject, INyxCancellationScope, INyxPascalAnalysisBudget)
  private
    FInner: INyxPascalAnalysisBudget;
    FPhase: TNyxPascalAnalysisPhase;
  public
    constructor Create(APhase: TNyxPascalAnalysisPhase);
    procedure Cancel;
    function GetCancelled: Boolean;
    procedure Admit(APhase: TNyxPascalAnalysisPhase; AUnits: Integer);
    function Receipt: TNyxPascalAnalysisReceipt;
    procedure Retire;
  end;

var
  GChecks: Integer;
  GParentsRetired: Integer;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
end;

destructor TParentScope.Destroy;
begin
  Inc(GParentsRetired);
  inherited Destroy;
end;

procedure TParentScope.Cancel;
begin
  FCancelled := True;
end;

function TParentScope.GetCancelled: Boolean;
begin
  Result := FCancelled;
end;

constructor TPhaseScope.Create(APhase: TNyxPascalAnalysisPhase);
begin
  inherited Create;
  FPhase := APhase;
  FInner := NewNyxPascalAnalysisBudget(1000000);
end;

procedure TPhaseScope.Cancel;
begin
  FInner.Cancel;
end;

function TPhaseScope.GetCancelled: Boolean;
begin
  Result := FInner.Cancelled;
end;

procedure TPhaseScope.Admit(APhase: TNyxPascalAnalysisPhase; AUnits: Integer);
begin

  if APhase = FPhase then
  begin
    FInner.Cancel;
  end;
  FInner.Admit(APhase, AUnits);
end;

function TPhaseScope.Receipt: TNyxPascalAnalysisReceipt;
begin
  Result := FInner.Receipt;
end;

procedure TPhaseScope.Retire;
begin
  FInner.Retire;
end;

function Facts: INyxPascalCompilerContext;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi).CompleteDefines(True);
end;

function Analyze(const AFiles: INyxPascalFileCatalog; AMaximum: Integer;
  const AScope: INyxCancellationScope = nil): INyxPascalSemanticTranslation;
begin
  Result := NewNyxPascalSemanticTranslation(AFiles, NyxPascalPhysicalFile('work.pas'),
    Facts, NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults),
    TNyxPascalTranslationOptions.Defaults, TNyxPascalSyntaxOptions.Defaults,
    TNyxPascalConditionalOptions.Defaults.UpToTokens(AMaximum), AScope);
end;

procedure BudgetOwnership;
var
  LParent: INyxCancellationScope;
  LBudget: INyxPascalAnalysisBudget;
  LOuter: INyxPascalAnalysisBudget;
  LCopy: TNyxPascalAnalysisReceipt;
  LRejected: Boolean;
begin
  LParent := TParentScope.Create;
  LBudget := NewNyxPascalAnalysisBudget(High(Integer), LParent);
  LParent := nil;
  LBudget.Admit(papSyntaxToken, High(Integer) - 1);
  LCopy := LBudget.Receipt;
  LBudget.Admit(papBindingStep);
  LRejected := False;
  try
    LBudget.Admit(papTypeToken);
  except
    on LLimit: ENyxPascalAnalysisWorkLimit do
    begin
      LRejected := (LLimit.WorkBefore = High(Integer)) and (LLimit.Requested = 1) and
        (LLimit.Maximum = High(Integer)) and (LLimit.Phase = papTypeToken);
    end;
  end;
  Check(LRejected and (LBudget.Receipt.Total = High(Integer)),
    'Overflow-safe exact admission rejects before incrementing the exhausted budget');
  Check((LCopy.Total = High(Integer) - 1) and (LCopy.Used(papBindingStep) = 0),
    'Detached receipts do not alias subsequent phase counters');
  LBudget.Retire;
  Check(GParentsRetired = 1, 'Retirement releases the outward parent lease immediately');
  LRejected := False;
  try
    LBudget.Admit(papSyntaxToken, 0);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Even zero-work admission cannot revive a retired operation');

  LOuter := NewNyxPascalAnalysisBudget(3);
  LBudget := NewNyxPascalAnalysisBudget(8, LOuter);
  LBudget.Admit(papWorldToken, 3);
  LRejected := False;
  try
    LBudget.Admit(papSyntaxToken);
  except
    on ENyxPascalAnalysisWorkLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LBudget.Receipt.Total = 3) and (LOuter.Receipt.Total = 3),
    'Nested budgets enforce the caller ceiling without partially admitting the child');
  LBudget.Cancel;
  Check(LBudget.Cancelled and not LOuter.Cancelled, 'Local cancellation preserves the parent generation');
  LBudget.Retire;
  LOuter.Retire;
end;

procedure SemanticBoundaries;
const
  CSource: TNyxText = 'unit Work; interface const Amount=2; ' +
    '{$if Amount=2}const Ready=1;{$endif} implementation end.';
var
  LFiles: INyxPascalFileCatalog;
  LBaseline: INyxPascalSemanticTranslation;
  LResult: INyxPascalSemanticTranslation;
  LDetailed: INyxPascalSemanticAnalysisWork;
  LCompatibility: INyxPascalSemanticWork;
  LReceipt: TNyxPascalAnalysisReceipt;
  LPhase: TNyxPascalAnalysisPhase;
  LSeen: set of TNyxPascalAnalysisPhase;
  LLimit: Integer;
  LSum: Integer;
  LRejected: Boolean;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('work.pas'), CSource);
  LBaseline := Analyze(LFiles, 1000000);
  Check(LBaseline.Source.Resolved and (LBaseline.Answers = 1),
    'The real constant condition completes through ordinary source/world binding');
  Check(Supports(LBaseline, INyxPascalSemanticAnalysisWork, LDetailed) and
    Supports(LBaseline, INyxPascalSemanticWork, LCompatibility),
    'The published result exposes detached detailed and compatible work receipts');
  LReceipt := LDetailed.AnalysisWork;
  LSum := 0;
  for LPhase := Low(TNyxPascalAnalysisPhase) to High(TNyxPascalAnalysisPhase) do
  begin
    Inc(LSum, LReceipt.Used(LPhase));
  end;
  Check((LSum = LReceipt.Total) and (LSum = LBaseline.WorkTokens) and
    (LCompatibility.TranslationTokens = LBaseline.Source.Count) and
    (LSum = LCompatibility.TranslationTokens + LCompatibility.PrefixTokens +
      LCompatibility.ConstantSteps), 'Exact phase totals retain one emitted-token identity and no double charging');
  { Physical source scanning emits its own final rows once. Prepared projection
    atoms avoid another classification pass but still admit every produced
    virtual row; they are not a free copy or a guessed work discount. }
  Check((LReceipt.Used(papFrozenRow) > 0) and (LReceipt.Used(papProjectionToken) > 0) and
    (LReceipt.Used(papLexicalToken) > 0) and (LReceipt.Used(papLexicalRow) > 0) and
    (LReceipt.Used(papFilterToken) > 0) and (LReceipt.Used(papSyntaxToken) > 0) and
    (LReceipt.Used(papMetadataNode) > 0) and (LReceipt.Used(papMetadataValue) > 0) and
    (LReceipt.Used(papWorldToken) > 0) and (LReceipt.Used(papWorldEntry) > 0) and
    (LReceipt.Used(papBindingStep) > 0) and (LReceipt.Used(papConstantPlan) > 0) and
    (LReceipt.Used(papConstantStep) > 0),
    'Ordinary construction charges actual scans, copies, metadata, binding and evaluation without duplicate lexical imports');
  LResult := Analyze(LFiles, LSum);
  Check(LResult.Source.Resolved and (LResult.WorkTokens = LSum),
    'The exact measured cold-construction allowance succeeds without a guessed discount');
  LSeen := [];
  for LLimit := 1 to LSum - 1 do
  begin
    LResult := nil;
    LRejected := False;
    try
      LResult := Analyze(LFiles, LLimit);
    except
      on LError: ENyxPascalConditionalWorkLimit do
      begin
        LRejected := LError.HasAnalysisPhase and (LError.Maximum = LLimit) and
          (LError.WorkBefore <= LLimit) and
          (LError.Requested > LLimit - LError.WorkBefore);
        Include(LSeen, LError.AnalysisPhase);
      end;
    end;
    Check(LRejected and (LResult = nil),
      'Every smaller allowance refuses atomically at its actual next operation');
  end;
  for LPhase := Low(TNyxPascalAnalysisPhase) to High(TNyxPascalAnalysisPhase) do
  begin

    if LReceipt.Used(LPhase) > 0 then
    begin
      Check(LPhase in LSeen, 'Every exercised phase is independently interruptible before its first admitted operation');
    end;
  end;
  Check((LFiles.TextAt(0) = CSource) and (LBaseline.Source.Files.TextAt(0) = CSource),
    'Exhaustive refusal leaves captured source and the earlier successful result exact');
  WriteLn('cold semantic admitted units / ', LSum, ' / exhaustive smaller limits / ', LSum - 1);
end;

procedure CancellationBoundaries;
const
  CSource: TNyxText = 'unit Work; interface const Amount=2; ' +
    '{$if Amount=2}const Ready=1;{$endif} implementation end.';
var
  LFiles: INyxPascalFileCatalog;
  LResult: INyxPascalSemanticTranslation;
  LScope: INyxPascalAnalysisBudget;
  LPhase: TNyxPascalAnalysisPhase;
  LRejected: Boolean;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('work.pas'), CSource);
  for LPhase in [papFrozenRow, papProjectionToken, papLexicalToken, papFilterToken,
    papMetadataNode, papWorldToken, papBindingStep, papConstantStep] do
  begin
    LScope := TPhaseScope.Create(LPhase);
    LResult := nil;
    LRejected := False;
    try
      LResult := Analyze(LFiles, 1000000, LScope);
    except
      on ENyxPascalAnalysisCancelled do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LResult = nil) and (LScope.Receipt.Used(LPhase) = 0),
      'Caller cancellation propagates through nested admission before the selected phase executes');
    LScope.Retire;
  end;
end;

procedure TypedQueryOwnership;
const
  CSource: TNyxText = 'unit Work; interface type TPayload=record Count:LongInt; ' +
    'end; PPayload=^TPayload; var Value:PPayload; implementation end.';
var
  LFiles: INyxPascalFileCatalog;
  LView: INyxPascalTranslatedSyntax;
  LWorld: INyxPascalSemanticWorld;
  LBudget: INyxPascalAnalysisBudget;
  LType: INyxPascalTypeSyntax;
  LResolution: INyxPascalResolution;
  LDeclaration: Integer;
  LCold: TNyxPascalAnalysisReceipt;
  LWarm: TNyxPascalAnalysisReceipt;
  LBefore: TNyxPascalAnalysisReceipt;
  LRejected: Boolean;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('work.pas'), CSource);
  LBudget := NewNyxPascalAnalysisBudget(1000000);
  LView := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFiles,
    NyxPascalPhysicalFile('work.pas'), Facts, TNyxPascalTranslationOptions.Defaults,
    LBudget), TNyxPascalSyntaxOptions.Defaults, LBudget);
  LWorld := WithNyxPascalSemanticModule(NewNyxPascalSemanticWorld(
    TNyxPascalBindingOptions.Defaults), LView, LBudget);
  LDeclaration := LView.Syntax.Named(0, 'TPayload');
  LBefore := LBudget.Receipt;
  LType := LView.TypeOfDeclaration(LDeclaration, TNyxPascalTypeOptions.Defaults, LBudget);
  LCold := LBudget.Receipt;
  Check(LType.Complete and (LCold.Used(papTypeToken) > LBefore.Used(papTypeToken)) and
    (LCold.Used(papTypeNode) > LBefore.Used(papTypeNode)) and
    (LCold.Used(papTypeProof) > LBefore.Used(papTypeProof)),
    'Cold type parsing admits its actual reads, nodes and source-free proof preparation');
  LType := LView.TypeOfDeclaration(LDeclaration, TNyxPascalTypeOptions.Defaults, LBudget);
  LWarm := LBudget.Receipt;
  Check((LWarm.Used(papTypeToken) = LCold.Used(papTypeToken)) and
    (LWarm.Used(papTypeNode) = LCold.Used(papTypeNode)) and
    (LWarm.Used(papTypeProof) > LCold.Used(papTypeProof)) and
    (LWarm.Total - LCold.Total < LCold.Total - LBefore.Total),
    'Validated reuse avoids real grammar work while still admitting its current-source proof');
  LResolution := LWorld.TypeOfSymbol(LWorld.Symbol(0,
    LView.Syntax.Named(0, 'Value')), LBudget);
  Check((LResolution.Issue = pbiNone) and
    (LBudget.Receipt.Used(papBindingStep) > 0), 'Ordinary bound type resolution shares the caller work ceiling');
  LBudget.Retire;
  Check(LType.Complete and (LType.Lexical.Source <> '') and (LWorld.Count = 1) and
    (LResolution.Issue = pbiNone), 'Published type/world/results remain readable after their execution retires');
  LBudget := NewNyxPascalAnalysisBudget(1);
  LType := nil;
  LRejected := False;
  try
    LType := LView.TypeOfDeclaration(LDeclaration, TNyxPascalTypeOptions.Defaults, LBudget);
  except
    on ENyxPascalAnalysisWorkLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LType = nil), 'A populated memo cannot bypass a smaller caller budget');
  LBudget.Retire;
end;

begin
  try
    BudgetOwnership;
    SemanticBoundaries;
    CancellationBoundaries;
    TypedQueryOwnership;
    WriteLn('PASS ', GChecks, ' Pascal analysis work admission checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      ExitCode := 1;
    end;
  end;
end.
