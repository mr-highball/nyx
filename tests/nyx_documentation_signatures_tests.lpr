{ Source-owned signatures and shared pre-copy admission. MIT, see LICENSE. }
program nyx_documentation_signatures_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.bytes, nyx.scheduler, nyx.pascal.lexing,
  nyx.pascal.syntax, nyx.pascal.types, nyx.pascal.work, nyx.documentation,
  nyx.documentation.typeexpressions, nyx.documentation.signatures,
  nyx.test.documentation {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

type
  { Mutable lexical supplier never supplies token or Slice authority. Counters
    and a second-read text/cancellation change exercise final bounded agreement. }
  TLexicalSupplier = class(TInterfacedObject, INyxPascalLexicalSnapshot)
  public
    Value: INyxPascalLexicalSnapshot;
    ChangedSource: TNyxText;
    Cancellation: INyxCancellationScope;
    SourceReads, SliceReads, TokenReads: Integer;
    destructor Destroy; override;
    function Source: TNyxText;
    function Options: TNyxPascalLexicalOptions;
    function Scalars: Integer;
    function Count: Integer;
    function Token(AIndex: Integer): TNyxPascalToken;
    function Text(AIndex: Integer): TNyxText;
    function Identifier(AIndex: Integer): TNyxText;
    function TokenAt(AOffset: Integer): Integer;
    function Slice(const ASpan: TNyxPascalSpan): TNyxText;
    function DiagnosticCount: Integer;
    function Diagnostic(AIndex: Integer): TNyxPascalLexicalDiagnostic;
  end;
  { Alternate syntax is observed, not cast into checked ownership. Record changes
    come from another real owned parse so fixture code fabricates no private span
    or declaration layout. All supplied ordinals remain snapshot-local. }
  TSyntaxSupplier = class(TInterfacedObject, INyxPascalSyntaxSnapshot)
  public
    Value, Changed: INyxPascalSyntaxSnapshot;
    LexicalValue: INyxPascalLexicalSnapshot;
    Reads: array of Integer;
    CountOverride, MutationOrdinal, MutationAt, TotalReads: Integer;
    constructor Create(const AText: TNyxText);
    destructor Destroy; override;
    function Lexical: INyxPascalLexicalSnapshot;
    function ScopeCount: Integer;
    function Scope(AIndex: Integer): TNyxPascalSyntaxScope;
    function ScopeAt(AOffset: Integer): Integer;
    function DeclarationCount: Integer;
    function Declaration(AIndex: Integer): TNyxPascalSyntaxDeclaration;
    function Named(AScope: Integer; const AName: TNyxText): Integer;
    function ImportCount: Integer;
    function ImportAt(AIndex: Integer): TNyxPascalSyntaxImport;
    function DiagnosticCount: Integer;
    function Diagnostic(AIndex: Integer): TNyxPascalSyntaxDiagnostic;
  end;
  { A mutable outward copy allowance proves exact UTF-8 pre-admission, including
    when the child's larger local default would otherwise accept another copy. }
  TOuterCopy = class(TInterfacedObject, INyxDocumentationTypeAdmission)
  public
    Bytes: Integer;
    Work, MaximumWork: Integer;
    Calls, CancelAt: Integer;
    CancelOnSyntax: Boolean;
    SyntaxCancellations: Integer;
    Cancellation: INyxCancellationScope;
    procedure AdmitTypeWork(APhase: TNyxPascalAnalysisPhase; AUnits: Integer);
    function RemainingTypeBytes: Integer;
  end;
  { Observed parent cancellation becomes true on one chosen read. Replaying the
    successful call's last observation targets cancellation after its private
    substring copy, without depending on a hard-coded implementation counter. }
  TObservedCancellation = class(TInterfacedObject, INyxCancellationScope)
  public
    Reads, CancelAt: Integer;
    CancelledExplicitly: Boolean;
    procedure Cancel;
    function GetCancelled: Boolean;
  end;

var
  GChecks, GDestroyed: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not AValue then
    raise Exception.Create(AReason);
end;

function Construction: INyxDocumentationTypeConstruction;
begin
  Result := NewNyxDocumentationTypeConstruction(
    TNyxDocumentationTypeExpressionLimits.Defaults.Bytes(NyxDocumentationMaximumBytes));
end;

function HeaderSyntax(const AText: TNyxText): INyxPascalSyntaxSnapshot;
begin
  Result := NewNyxPascalSyntaxSnapshot(AText,
    TNyxPascalSyntaxOptions.Defaults.Fragment(psfInterfaceDeclarations));
end;

constructor TSyntaxSupplier.Create(const AText: TNyxText);
begin
  inherited Create;
  Value := HeaderSyntax(AText);
  SetLength(Reads, Value.DeclarationCount);
  CountOverride := -2;
  MutationOrdinal := -1;
  MutationAt := 2;
end;

destructor TSyntaxSupplier.Destroy;
begin
  Inc(GDestroyed);
  inherited Destroy;
end;

function TSyntaxSupplier.Lexical: INyxPascalLexicalSnapshot;
begin
  Result := LexicalValue;

  if Result = nil then
    Result := Value.Lexical;
end;

function TSyntaxSupplier.ScopeCount: Integer;
begin
  Result := Value.ScopeCount;
end;

function TSyntaxSupplier.Scope(AIndex: Integer): TNyxPascalSyntaxScope;
begin
  Result := Value.Scope(AIndex);
end;

function TSyntaxSupplier.ScopeAt(AOffset: Integer): Integer;
begin
  Result := Value.ScopeAt(AOffset);
end;

function TSyntaxSupplier.DeclarationCount: Integer;
begin
  Result := CountOverride;

  if Result = -2 then
    Result := Value.DeclarationCount;
end;

function TSyntaxSupplier.Declaration(AIndex: Integer): TNyxPascalSyntaxDeclaration;
begin
  Inc(TotalReads);
  Inc(Reads[AIndex]);

  if (AIndex = MutationOrdinal) and (Reads[AIndex] >= MutationAt) then
    Result := Changed.Declaration(AIndex)
  else
    Result := Value.Declaration(AIndex);
end;

function TSyntaxSupplier.Named(AScope: Integer; const AName: TNyxText): Integer;
begin
  Result := Value.Named(AScope, AName);
end;

function TSyntaxSupplier.ImportCount: Integer;
begin
  Result := Value.ImportCount;
end;

function TSyntaxSupplier.ImportAt(AIndex: Integer): TNyxPascalSyntaxImport;
begin
  Result := Value.ImportAt(AIndex);
end;

function TSyntaxSupplier.DiagnosticCount: Integer;
begin
  Result := Value.DiagnosticCount;
end;

function TSyntaxSupplier.Diagnostic(AIndex: Integer): TNyxPascalSyntaxDiagnostic;
begin
  Result := Value.Diagnostic(AIndex);
end;

destructor TLexicalSupplier.Destroy;
begin
  Inc(GDestroyed);
  inherited Destroy;
end;

function TLexicalSupplier.Source: TNyxText;
begin
  Inc(SourceReads);

  if (SourceReads > 1) and (Cancellation <> nil) then
    Cancellation.Cancel;

  if (SourceReads > 1) and (ChangedSource <> '') then
    Result := ChangedSource
  else
    Result := Value.Source;
end;

function TLexicalSupplier.Options: TNyxPascalLexicalOptions;
begin
  Result := Value.Options;
end;

function TLexicalSupplier.Scalars: Integer;
begin
  Result := Value.Scalars;
end;

function TLexicalSupplier.Count: Integer;
begin
  Result := Value.Count;
end;

function TLexicalSupplier.Token(AIndex: Integer): TNyxPascalToken;
begin
  Inc(TokenReads);
  Result := Value.Token(AIndex);
end;

function TLexicalSupplier.Text(AIndex: Integer): TNyxText;
begin
  Result := Value.Text(AIndex);
end;

function TLexicalSupplier.Identifier(AIndex: Integer): TNyxText;
begin
  Result := Value.Identifier(AIndex);
end;

function TLexicalSupplier.TokenAt(AOffset: Integer): Integer;
begin
  Result := Value.TokenAt(AOffset);
end;

function TLexicalSupplier.Slice(const ASpan: TNyxPascalSpan): TNyxText;
begin
  Inc(SliceReads);
  Result := Value.Slice(ASpan);
end;

function TLexicalSupplier.DiagnosticCount: Integer;
begin
  Result := Value.DiagnosticCount;
end;

function TLexicalSupplier.Diagnostic(AIndex: Integer): TNyxPascalLexicalDiagnostic;
begin
  Result := Value.Diagnostic(AIndex);
end;

procedure TOuterCopy.AdmitTypeWork(APhase: TNyxPascalAnalysisPhase; AUnits: Integer);
begin

  if (Ord(APhase) < Ord(Low(TNyxPascalAnalysisPhase))) or
    (Ord(APhase) > Ord(High(TNyxPascalAnalysisPhase))) then
    raise EArgumentException.Create('Unknown test admission phase');

  if (MaximumWork > 0) and (AUnits > MaximumWork - Work) then
    raise ENyxDocumentationTypeExpression.Create('Outer signature work allowance exhausted');
  Inc(Work, AUnits);
  Inc(Calls);

  if (Cancellation <> nil) and (CancelAt > 0) and (Calls >= CancelAt) then
    Cancellation.Cancel;

  if (Cancellation <> nil) and CancelOnSyntax and (APhase = papSyntaxToken) then
  begin
    Inc(SyntaxCancellations);
    Cancellation.Cancel;
  end;
end;

function TOuterCopy.RemainingTypeBytes: Integer;
begin
  Result := Bytes;
end;

procedure TObservedCancellation.Cancel;
begin
  CancelledExplicitly := True;
end;

function TObservedCancellation.GetCancelled: Boolean;
begin
  Inc(Reads);
  Result := CancelledExplicitly or ((CancelAt > 0) and (Reads >= CancelAt));
end;

procedure WindowCases;
var
  LScope: INyxDocumentationTypeConstruction;
  LOuter: TOuterCopy;
  LOuterLease: INyxDocumentationTypeAdmission;
  LText, LCopied: TNyxText;
  LRefused: Boolean;
  LParent: TObservedCancellation;
  LParentLease: INyxCancellationScope;
  LFinalObservation: Integer;
begin
  LText := 'A' + NyxScalarText($1F680) + #0 + 'Z';
  LScope := NewNyxDocumentationTypeConstruction(
    TNyxDocumentationTypeExpressionLimits.Defaults.Bytes(5));
  try
    LCopied := CopyNyxDocumentationTypeTextWindow(LText, NyxPascalSpan(1, 2), LScope);
    Check((LCopied = NyxScalarText($1F680) + #0) and (NyxUTF8ByteCount(LCopied) = 5),
      'Copy uses scalar coordinates and exact UTF-8 admission for supplementary/NUL text');
    LRefused := False;
    try
      LCopied := CopyNyxDocumentationTypeTextWindow(LText, NyxPascalSpan(3, 1), LScope);
    except
      on E: ENyxDocumentationTypeExpression do LRefused := True;
    end;
    Check(LRefused, 'A second window shares the first window remaining byte allowance');
  finally
    LScope.Retire;
  end;
  LOuter := TOuterCopy.Create;
  LOuterLease := LOuter;
  LOuter.Bytes := 4;
  LScope := NewNyxDocumentationTypeConstruction(
    TNyxDocumentationTypeExpressionLimits.Defaults, nil, LOuterLease);
  try
    LRefused := False;
    try
      LCopied := CopyNyxDocumentationTypeTextWindow(LText, NyxPascalSpan(1, 2), LScope);
    except
      on E: ENyxDocumentationTypeExpression do LRefused := True;
    end;
    Check(LRefused and (LOuter.Work > 0), 'Outer copy policy refuses the five-byte window before its copy');
    LOuter.Bytes := 5;
    Check(CopyNyxDocumentationTypeTextWindow(LText, NyxPascalSpan(1, 2), LScope) =
      NyxScalarText($1F680) + #0, 'A refused byte reservation does not consume storage allowance');
  finally
    LScope.Retire;
  end;
  LScope := NewNyxDocumentationTypeConstruction(
    TNyxDocumentationTypeExpressionLimits.Defaults.Work(3));
  try
    LRefused := False;
    try
      LCopied := CopyNyxDocumentationTypeTextWindow('prefix-value', NyxPascalSpan(7, 5), LScope);
    except
      on E: ENyxDocumentationTypeExpression do LRefused := True;
    end;
    Check(LRefused and (LScope.Receipt.Total = 3), 'Prefix scanning consumes actual shared work and refuses atomically');
  finally
    LScope.Retire;
  end;
  LScope := Construction;
  try
    LRefused := False;
    try
      LCopied := CopyNyxDocumentationTypeTextWindow('tiny', NyxPascalSpan(3, 2), LScope);
    except
      on E: ENyxDocumentationTypeExpression do LRefused := True;
    end;
    Check(LRefused, 'An out-of-source scalar window cannot be silently truncated');
    Check(CopyNyxDocumentationTypeTextWindow('tiny', NyxPascalSpan(4, 0), LScope) = '',
      'An exact empty EOF window is admitted');
    LScope.Cancel;
    LRefused := False;
    try
      LCopied := CopyNyxDocumentationTypeTextWindow('tiny', NyxPascalSpan(0, 1), LScope);
    except
      on E: ENyxDocumentationTypeExpressionCancelled do LRefused := True;
    end;
    Check(LRefused, 'Window copying honors cancellation before touching source');
  finally
    LScope.Retire;
  end;
  LParent := TObservedCancellation.Create;
  LParentLease := LParent;
  LScope := NewNyxDocumentationTypeConstruction(
    TNyxDocumentationTypeExpressionLimits.Defaults, LParentLease);
  try
    Check(CopyNyxDocumentationTypeTextWindow('source', NyxPascalSpan(0, 6), LScope) = 'source',
      'Observed cancellation baseline copies its selected window');
    LFinalObservation := LParent.Reads;
  finally
    LScope.Retire;
  end;
  LParent := TObservedCancellation.Create;
  LParentLease := LParent;
  LParent.CancelAt := LFinalObservation;
  LScope := NewNyxDocumentationTypeConstruction(
    TNyxDocumentationTypeExpressionLimits.Defaults, LParentLease);
  try
    LCopied := '';
    LRefused := False;
    try
      LCopied := CopyNyxDocumentationTypeTextWindow('source', NyxPascalSpan(0, 6), LScope);
    except
      on E: ENyxDocumentationTypeExpressionCancelled do LRefused := True;
    end;
    Check(LRefused and (LCopied = '') and (LParent.Reads = LFinalObservation),
      'Final cancellation after private copying cannot publish a hidden managed result');
  finally
    LScope.Retire;
  end;
end;

procedure SignatureCases;
var
  LScope: INyxDocumentationTypeConstruction;
  LSyntax: INyxPascalSyntaxSnapshot;
  LSignature: INyxDocumentationRichSignature;
  LParameter: INyxDocumentationParameterDeclaration;
  LBefore: Integer;
begin
  LSyntax := HeaderSyntax('function Mix(const A, B: array of Core.TItem; var C: Integer; ' +
    'out D: Core.TResult; constref E: Core.TBox<Integer>; ' +
    '&type: String = ''left;middle:right''): Core.TAnswer;');
  LScope := Construction;
  try
    LSignature := CaptureNyxDocumentationRoutineSignature(LSyntax, LSyntax.Named(0, 'Mix'), LScope);
    Check(LSignature.ParameterCount = 6, 'Grouped parameters retain original logical order');
    Check((LSignature.ParameterAt(0).Name = 'A') and (LSignature.ParameterAt(1).Name = 'B') and
      (LSignature.ParameterAt(2).Passing = ndpVar) and (LSignature.ParameterAt(3).Passing = ndpOut) and
      (LSignature.ParameterAt(4).Passing = ndpConstRef), 'Value/const/var/out/constref modes retain their declared intent');
    LParameter := LSignature.ParameterAt(5);
    Check((LParameter.Name = 'type') and (LParameter.Passing = ndpValue) and LParameter.HasDefault and
      (LParameter.DefaultPascal = '''left;middle:right'''), 'Escaped logical name and exact punctuation default survive capture');
    Check((LSignature.ParameterAt(0).TypeUse.Expression.Text = 'array of Core.TItem') and
      (LSignature.ParameterAt(0).TypeUse.Expression.Occurrence = ndtoParameter) and
      (LSignature.ParameterAt(0).TypeUse.BindingCount = 1) and
      not LSignature.ParameterAt(0).TypeUse.BindingAt(0).Defined, 'Parameter shapes retain qualified syntax and explicit unresolved slots');
    Check((LSignature.ResultUse.Expression.Text = 'Core.TAnswer') and
      (LSignature.ResultUse.Expression.Occurrence = ndtoResult) and
      not LSignature.ResultUse.Reference.Defined, 'A named result remains an expression use without a guessed global identity');
    LBefore := LScope.Receipt.Total;
    LScope.Spend;
    Check(LScope.Receipt.Total = LBefore + 1, 'Extractor does not retire the caller shared construction');
  finally
    LScope.Retire;
  end;
  LSyntax := nil;
  Check((LSignature.ParameterAt(0).TypeUse.Expression.Syntax.Lexical.Source = 'array of Core.TItem') and
    (LSignature.ResultUse.Expression.Syntax.Lexical.Source = 'Core.TAnswer'),
    'Published signatures keep small detached type windows after source and admission retire');
  LScope := Construction;
  try
    LSyntax := HeaderSyntax('procedure Tick;');
    LSignature := CaptureNyxDocumentationRoutineSignature(LSyntax, LSyntax.Named(0, 'Tick'), LScope);
    Check((LSignature.ParameterCount = 0) and (LSignature.ResultUse = nil), 'Parameterless procedure retains no invented result');
    LSyntax := HeaderSyntax('function Callback(const AHandler: procedure(Value: Integer) of object; ' +
      'Count: Integer = Ord('';'')): Boolean;');
    LSignature := CaptureNyxDocumentationRoutineSignature(LSyntax, LSyntax.Named(0, 'Callback'), LScope);
    Check((LSignature.ParameterAt(0).TypeUse.Expression.Syntax.Node(
      LSignature.ParameterAt(0).TypeUse.Expression.Syntax.Root).Kind = ptxProcedure) and
      (LSignature.ParameterAt(1).DefaultPascal = 'Ord('';'')'), 'Procedural shape and nested constant-expression defaults remain inert syntax');
  finally
    LScope.Retire;
  end;
end;

procedure RefusalCases;
const
  CBad: array[0..6] of TNyxText = ('procedure Untyped(var A);',
    'function Missing: ;', 'function Generic<T>(A: T): T;',
    'procedure Broken(A: Integer = );', 'constructor Create(A: Integer);',
    'procedure Conditional(A: {$ifdef MAYBE}Integer{$else}String{$endif});',
    'procedure Embedded(A: record Value: Integer; end);');
var
  LScope: INyxDocumentationTypeConstruction;
  LSyntax: INyxPascalSyntaxSnapshot;
  LSignature: INyxDocumentationRichSignature;
  LAt: Integer;
  LRefused: Boolean;
  LText: TNyxText;
begin
  for LAt := Low(CBad) to High(CBad) do
  begin
    LSyntax := HeaderSyntax(CBad[LAt]);
    LScope := Construction;
    LSignature := nil;
    try
      LRefused := False;
      try
        LSignature := CaptureNyxDocumentationRoutineSignature(LSyntax, 0, LScope);
      except
        on E: ENyxDocumentation do LRefused := True;
        on E: ENyxDocumentationTypeExpression do LRefused := True;
      end;
      Check(LRefused and (LSignature = nil), 'Unsupported/uncertain signature refuses without a partial result');
    finally
      LScope.Retire;
    end;
  end;
  LScope := Construction;
  LSignature := nil;
  try
    LRefused := False;
    try
      LSignature := CaptureNyxDocumentationRoutineSignature(nil, 0, LScope);
    except
      on E: ENyxDocumentationSignature do LRefused := True;
    end;
    Check(LRefused and (LSignature = nil), 'Nil source supplies no declaration authority');
  finally
    LScope.Retire;
  end;
  LText := 'procedure TooMany(';
  for LAt := 1 to NyxDocumentationMaximumParameters + 1 do
  begin

    if LAt > 1 then
      LText := LText + '; ';
    LText := LText + 'A' + IntToStr(LAt) + ': Integer';
  end;
  LSyntax := HeaderSyntax(LText + ');');
  LScope := Construction;
  try
    LRefused := False;
    try
      LSignature := CaptureNyxDocumentationRoutineSignature(LSyntax, 0, LScope);
    except
      on E: ENyxDocumentationSignature do LRefused := True;
    end;
    Check(LRefused, 'One more than the public parameter ceiling refuses the complete signature');
  finally
    LScope.Retire;
  end;
end;

procedure AggregateCases;
var
  LSource: INyxPascalSyntaxSnapshot;
  LCounted: TSyntaxSupplier;
  LScope: INyxDocumentationTypeConstruction;
  LFirst, LSecond: INyxDocumentationRichSignature;
  LWork, LRows: Integer;
  LRefused: Boolean;
  LOuter: TOuterCopy;
  LOuterLease: INyxDocumentationTypeAdmission;
  LFinalAdmission: Integer;
begin
  LCounted := TSyntaxSupplier.Create('function Repeated(const Items: array of Core.TItem): Core.TResult;');
  LSource := LCounted;
  LOuter := TOuterCopy.Create;
  LOuterLease := LOuter;
  LOuter.Bytes := NyxDocumentationMaximumBytes;
  LScope := NewNyxDocumentationTypeConstruction(
    TNyxDocumentationTypeExpressionLimits.Defaults, nil, LOuterLease);
  try
    LFirst := CaptureNyxDocumentationRoutineSignature(LSource, 0, LScope);
    { Retained receipt-vector capacity can make a later capture cheaper. Apply
      the caller's actual remaining allowance instead of assuming equal costs. }
    LOuter.MaximumWork := LOuter.Work + 1;
    LWork := LScope.Receipt.Total;
    LRows := LCounted.TotalReads;
    LSecond := nil;
    LRefused := False;
    try
      LSecond := CaptureNyxDocumentationRoutineSignature(LSource, 0, LScope);
    except
      on E: ENyxDocumentationTypeExpression do LRefused := True;
    end;
    Check(LRefused and (LSecond = nil) and (LFirst.ParameterCount = 1),
      'Two captures share one work ceiling and preserve the earlier immutable result on refusal');
    Check((LOuter.Work = LOuter.MaximumWork) and (LScope.Receipt.Total = LWork + 1) and
      (LCounted.TotalReads = LRows), 'Shared remaining work refuses before another supplier row without resetting either receipt');
  finally
    LScope.Retire;
  end;
  LOuter := TOuterCopy.Create;
  LOuterLease := LOuter;
  LOuter.Bytes := NyxDocumentationMaximumBytes;
  LScope := NewNyxDocumentationTypeConstruction(
    TNyxDocumentationTypeExpressionLimits.Defaults, nil, LOuterLease);
  try
    LFirst := CaptureNyxDocumentationRoutineSignature(LSource, 0, LScope);
    LFinalAdmission := LOuter.Calls;
  finally
    LScope.Retire;
  end;
  LOuter := TOuterCopy.Create;
  LOuterLease := LOuter;
  LOuter.Bytes := NyxDocumentationMaximumBytes;
  LOuter.CancelAt := LFinalAdmission;
  LScope := NewNyxDocumentationTypeConstruction(
    TNyxDocumentationTypeExpressionLimits.Defaults, nil, LOuterLease);
  LOuter.Cancellation := LScope;
  try
    LSecond := nil;
    LRefused := False;
    try
      LSecond := CaptureNyxDocumentationRoutineSignature(LSource, 0, LScope);
    except
      on E: ENyxDocumentationTypeExpressionCancelled do LRefused := True;
    end;
    Check(LRefused and (LSecond = nil) and (LOuter.Calls = LFinalAdmission),
      'Cancellation caused by final outward admission cannot publish a complete-looking signature');
  finally
    LOuter.Cancellation := nil;
    LScope.Retire;
  end;
  LOuter := TOuterCopy.Create;
  LOuterLease := LOuter;
  LOuter.Bytes := NyxDocumentationMaximumBytes;
  LOuter.CancelOnSyntax := True;
  LScope := NewNyxDocumentationTypeConstruction(
    TNyxDocumentationTypeExpressionLimits.Defaults, nil, LOuterLease);
  LOuter.Cancellation := LScope;
  try
    LSecond := nil;
    LRefused := False;
    try
      LSecond := CaptureNyxDocumentationRoutineSignature(LSource, 0, LScope);
    except
      on E: ENyxDocumentationTypeExpressionCancelled do LRefused := True;
    end;
    Check(LRefused and (LSecond = nil) and (LOuter.SyntaxCancellations = 1),
      'Cancellation during owned declaration parsing retains the pure expression cancellation contract');
  finally
    LOuter.Cancellation := nil;
    LScope.Retire;
  end;
end;

procedure AlternateCases;
var
  LScope: INyxDocumentationTypeConstruction;
  LSource: TSyntaxSupplier;
  LSourceLease: INyxPascalSyntaxSnapshot;
  LLexical: TLexicalSupplier;
  LSignature: INyxDocumentationRichSignature;
  LRefused: Boolean;
  LAt, LBefore: Integer;
begin
  for LAt := 0 to 5 do
  begin
    LSource := TSyntaxSupplier.Create('function Pick(A: Integer): Boolean;');
    LSourceLease := LSource;
    LLexical := TLexicalSupplier.Create;
    LLexical.Value := LSource.Value.Lexical;
    LSource.LexicalValue := LLexical;
    LScope := Construction;
    LSignature := nil;
    LBefore := GDestroyed;
    try
      case LAt of
        1: begin
          LSource.Changed := HeaderSyntax('function Pick(A: Integer): Core.TNumber;');
          Check(LSource.Changed.Declaration(0).TypeSpan.Length <>
            LSource.Value.Declaration(0).TypeSpan.Length,
            'Late declaration fixture changes a relevant observed result span');
          LSource.MutationOrdinal := 0;
        end;
        2: begin
          LSource.Changed := HeaderSyntax('function Pick(B: Integer): Boolean;');
          LSource.MutationOrdinal := 1;
        end;
        3: LLexical.ChangedSource := 'function Pick(A: Integer): TNumber;';
        4: LLexical.Cancellation := LScope;
        5: LSource.CountOverride := -1;
      end;
      LRefused := False;
      try
        LSignature := CaptureNyxDocumentationRoutineSignature(LSourceLease, 0, LScope);
      except
        on E: ENyxDocumentation do LRefused := True;
        on E: ENyxDocumentationTypeExpression do LRefused := True;
      end;
      Check((LAt = 0) <> LRefused, 'Stable alternate succeeds; late row/text/cancellation and invalid counts refuse, case ' + IntToStr(LAt));
      Check((LLexical.SliceReads = 0) and (LLexical.TokenReads = 0),
        'Alternate token/Slice observations never supply checked header authority');

      if LAt = 5 then
        Check((LSource.TotalReads = 0) and (LLexical.SourceReads = 0),
          'Invalid count refuses before selected row or lexical-source getters');

      if LAt > 0 then
        Check(LSignature = nil, 'Alternate refusal publishes no partial signature');
    finally
      LScope.Retire;
      LSourceLease := nil;
    end;
    Check(GDestroyed = LBefore + 2, 'Result retains neither alternate syntax nor lexical supplier');
  end;
  LSource := TSyntaxSupplier.Create('function Pick(A: Integer): Boolean;');
  LSourceLease := LSource;
  LScope := NewNyxDocumentationTypeConstruction(
    TNyxDocumentationTypeExpressionLimits.Defaults.Work(1));
  try
    LRefused := False;
    try
      LSignature := CaptureNyxDocumentationRoutineSignature(LSourceLease, 0, LScope);
    except
      on E: ENyxDocumentationTypeExpression do LRefused := True;
    end;
    Check(LRefused and (LSource.TotalReads = 0), 'Small work policy refuses before the first source row');
  finally
    LScope.Retire;
  end;
end;

{$ifndef PAS2JS}
function RepositoryArgument: String;
begin

  if ParamCount <> 1 then
    raise EArgumentException.Create('Usage: nyx_documentation_signatures_tests <repository-root>');
  Result := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(1)));

  if not DirectoryExists(Result) or not FileExists(Result + 'PROJECT.md') or
    not FileExists(Result + 'src' + PathDelim + 'nyx.contract.pas') or
    not FileExists(Result + 'src' + PathDelim + 'nyx.data.pas') then
    raise EArgumentException.Create('Repository root requires PROJECT.md and actual contract/data source');
end;

function SourceSyntax(const APath: String): INyxPascalSyntaxSnapshot;
var
  LStream: TMemoryStream;
  LBytes: TNyxBytes;
  LOptions: TNyxPascalSyntaxOptions;
begin
  Result := nil;
  LOptions := TNyxPascalSyntaxOptions.Defaults;
  LStream := TMemoryStream.Create;
  try
    LStream.LoadFromFile(APath);

    if LStream.Size > LOptions.LexicalOptions.MaximumSourceBytes then
      raise Exception.Create('Actual source exceeds existing lexical allowance');
    SetLength(LBytes, Integer(LStream.Size));

    if Length(LBytes) > 0 then
      Move(LStream.Memory^, LBytes[0], Length(LBytes));
    Result := NewNyxPascalSyntaxSnapshot(NyxDecodeUTF8(LBytes), LOptions);
  finally
    LStream.Free;
  end;
end;

procedure ActualSignature(const ASource: INyxPascalSyntaxSnapshot; ARoutine: Integer;
  const AName, AParameter: TNyxText);
var
  LScope: INyxDocumentationTypeConstruction;
  LSignature: INyxDocumentationRichSignature;
  LDeclaration: TNyxPascalSyntaxDeclaration;
  LTypeText, LResultText: TNyxText;
begin
  LDeclaration := ASource.Declaration(ARoutine);
  Check((LDeclaration.Name = AName) and (LDeclaration.RoutineKind = prkFunction),
    'Mandatory actual source contains the selected declared function');
  LTypeText := ASource.Lexical.Slice(ASource.Declaration(
    ASource.Named(LDeclaration.NestedScope, AParameter)).TypeSpan);
  LResultText := ASource.Lexical.Slice(LDeclaration.TypeSpan);
  LScope := Construction;
  try
    LSignature := CaptureNyxDocumentationRoutineSignature(ASource, ARoutine, LScope);
  finally
    LScope.Retire;
  end;
  Check((LSignature.ParameterCount = 1) and (LSignature.ParameterAt(0).Name = AParameter) and
    (LSignature.ParameterAt(0).Passing = ndpConst) and
    (LSignature.ParameterAt(0).TypeUse.Expression.Text = LTypeText),
    'Actual parameter order/mode/type comes through the public extractor');
  Check((LSignature.ResultUse.Expression.Text = LResultText) and
    (LSignature.ResultUse.Expression.Occurrence = ndtoResult) and
    (LSignature.ParameterAt(0).TypeUse.BindingCount = 1) and
    not LSignature.ParameterAt(0).TypeUse.BindingAt(0).Defined and
    (LSignature.ResultUse.BindingCount = 1) and not LSignature.ResultUse.BindingAt(0).Defined,
    'Actual parameter/result names stay exact unbound syntax rather than invented core bindings');
end;

procedure ActualSources(const ARepository: String);
const
  COwners: array[0..4] of TNyxText = ('TNyxTextDomain', 'TNyxTimeDomain',
    'TNyxBooleanDomain', 'TNyxIntegerDomain', 'TNyxNumberDomain');
var
  LSyntax: INyxPascalSyntaxSnapshot;
  LAt, LOwner, LRoutine, LCount: Integer;
begin
  LSyntax := SourceSyntax(ARepository + 'src' + PathDelim + 'nyx.contract.pas');
  LCount := 0;
  for LAt := Low(COwners) to High(COwners) do
  begin
    LOwner := LSyntax.Named(0, COwners[LAt]);
    Check(LOwner >= 0, 'Mandatory actual Choices owner exists');
    LRoutine := LSyntax.Named(LSyntax.Declaration(LOwner).NestedScope, 'Choices');
    while LRoutine >= 0 do
    begin
      ActualSignature(LSyntax, LRoutine, 'Choices', 'AValues');
      Inc(LCount);
      LRoutine := LSyntax.Declaration(LRoutine).NextNamed;
    end;
  end;
  Check(LCount = 7, 'All seven public actual Choices overloads are extracted without skips');
  LSyntax := SourceSyntax(ARepository + 'src' + PathDelim + 'nyx.data.pas');
  LRoutine := LSyntax.Named(0, 'NyxObject');
  Check(LRoutine >= 0, 'Mandatory actual NyxObject declaration exists');
  ActualSignature(LSyntax, LRoutine, 'NyxObject', 'AFields');
  LRoutine := LSyntax.Named(0, 'NyxArray');
  Check(LRoutine >= 0, 'Mandatory actual NyxArray declaration exists');
  ActualSignature(LSyntax, LRoutine, 'NyxArray', 'AItems');
end;

var
  LRepository: String;
{$endif}
begin
  try
    {$ifndef PAS2JS}
    LRepository := RepositoryArgument;
    {$endif}
    WindowCases;
    SignatureCases;
    RefusalCases;
    AggregateCases;
    AlternateCases;
    {$ifndef PAS2JS}
    ActualSources(LRepository);
    {$endif}
    Check((DocumentationControlsConstructed = 0) and (DocumentationCallbacksConstructed = 0) and
      (DocumentationCallbacksInvoked = 0) and (DocumentationGlobalFactoryCalls = 0) and
      (DocumentationGlobalRoutineCalls = 0), 'Signature inspection executes no documented class, callback, factory or example');
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' portable source signature checks';
    {$else}
    WriteLn('PASS ', GChecks, ' source signature checks / all9 actual signatures extracted');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + E.Message;
      {$else}
      WriteLn(StdErr, E.ClassName, ': ', E.Message, ' at check count ', GChecks);
      DumpExceptionBackTrace(StdErr);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
