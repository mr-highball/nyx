{ Atomic source-signature batches and admitted Unicode window reuse. MIT. }
program nyx_documentation_signaturebatch_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.bytes, nyx.scheduler, nyx.pascal.lexing,
  nyx.pascal.syntax, nyx.pascal.work, nyx.documentation,
  nyx.documentation.typeexpressions, nyx.documentation.signatures,
  nyx.test.documentation {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

type
  { Snapshot-local ordinals preserve the caller's selected order. They are not
    persistent names or authority to register incomplete unit declarations. }
  TOrdinals = array of Integer;
  { One bounded adversarial change is chosen explicitly, without fabricated
    private declaration fields or a replacement parser in this consumer. }
  TBatchFault = (bfNone, bfEarlierRoutine, bfEarlierParameter, bfSource,
    bfCancelSource, bfCount);
  { Lexical source changes only at its second whole-value observation. Token
    and Slice counters prove that a supplier does not grant lexical authority. }
  TLexicalProxy = class(TInterfacedObject, INyxPascalLexicalSnapshot)
  public
    Value: INyxPascalLexicalSnapshot;
    Fault: TBatchFault;
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
  { An alternate source delegates all real syntax to the existing parser. A
    later selected routine activates a different earlier row from another owned
    parse, testing the batch's final sibling receipts rather than span guesses. }
  TSyntaxProxy = class(TInterfacedObject, INyxPascalSyntaxSnapshot)
  public
    Value, Changed: INyxPascalSyntaxSnapshot;
    LexicalValue: INyxPascalLexicalSnapshot;
    Fault: TBatchFault;
    TargetOrdinal, TriggerOrdinal, DeclarationReads: Integer;
    Activated: Boolean;
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
  { An external aggregate byte/work policy can revoke the current construction
    on a measured admission. Disposal proves no result retains the policy. }
  TOuter = class(TInterfacedObject, INyxDocumentationTypeAdmission)
  public
    Bytes, Calls, CancelAt, MutateAt, RetireAt: Integer;
    Selection: TOrdinals;
    Windows: INyxDocumentationTextWindows;
    Cancellation: INyxCancellationScope;
    destructor Destroy; override;
    procedure AdmitTypeWork(APhase: TNyxPascalAnalysisPhase; AUnits: Integer);
    function RemainingTypeBytes: Integer;
  end;

const
  CHeaders: TNyxText = 'function One(A: Integer): Boolean;' + #10 +
    'function Two(B: Integer): Boolean;';

var
  GChecks, GDisposed, GPolicies: Integer;
  {$ifndef PAS2JS}
  GActual: Integer;
  {$endif}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not AValue then
    raise Exception.Create(AReason);
end;

function Syntax(const AText: TNyxText): INyxPascalSyntaxSnapshot;
begin
  Result := NewNyxPascalSyntaxSnapshot(AText,
    TNyxPascalSyntaxOptions.Defaults.Fragment(psfInterfaceDeclarations));
end;

function Construction: INyxDocumentationTypeConstruction;
begin
  Result := NewNyxDocumentationTypeConstruction(
    TNyxDocumentationTypeExpressionLimits.Defaults.Bytes(NyxDocumentationMaximumBytes));
end;

destructor TLexicalProxy.Destroy;
begin
  Inc(GDisposed);
  inherited Destroy;
end;

function TLexicalProxy.Source: TNyxText;
begin
  Inc(SourceReads);
  Result := Value.Source;

  if (Fault = bfSource) and (SourceReads >= 2) then
    Result := Result + ' ';

  if (Fault = bfCancelSource) and (Cancellation <> nil) then
    Cancellation.Cancel;
end;

function TLexicalProxy.Options: TNyxPascalLexicalOptions;
begin
  Result := Value.Options;
end;

function TLexicalProxy.Scalars: Integer;
begin
  Result := Value.Scalars;
end;

function TLexicalProxy.Count: Integer;
begin
  Result := Value.Count;
end;

function TLexicalProxy.Token(AIndex: Integer): TNyxPascalToken;
begin
  Inc(TokenReads);
  Result := Value.Token(AIndex);
end;

function TLexicalProxy.Text(AIndex: Integer): TNyxText;
begin
  Result := Value.Text(AIndex);
end;

function TLexicalProxy.Identifier(AIndex: Integer): TNyxText;
begin
  Result := Value.Identifier(AIndex);
end;

function TLexicalProxy.TokenAt(AOffset: Integer): Integer;
begin
  Inc(TokenReads);
  Result := Value.TokenAt(AOffset);
end;

function TLexicalProxy.Slice(const ASpan: TNyxPascalSpan): TNyxText;
begin
  Inc(SliceReads);
  Result := Value.Slice(ASpan);
end;

function TLexicalProxy.DiagnosticCount: Integer;
begin
  Result := Value.DiagnosticCount;
end;

function TLexicalProxy.Diagnostic(AIndex: Integer): TNyxPascalLexicalDiagnostic;
begin
  Result := Value.Diagnostic(AIndex);
end;

destructor TSyntaxProxy.Destroy;
begin
  Inc(GDisposed);
  inherited Destroy;
end;

function TSyntaxProxy.Lexical: INyxPascalLexicalSnapshot;
begin
  Result := LexicalValue;
end;

function TSyntaxProxy.ScopeCount: Integer;
begin
  Result := Value.ScopeCount;
end;

function TSyntaxProxy.Scope(AIndex: Integer): TNyxPascalSyntaxScope;
begin
  Result := Value.Scope(AIndex);
end;

function TSyntaxProxy.ScopeAt(AOffset: Integer): Integer;
begin
  Result := Value.ScopeAt(AOffset);
end;

function TSyntaxProxy.DeclarationCount: Integer;
begin
  Result := Value.DeclarationCount;

  if Fault = bfCount then
    Result := -1;
end;

function TSyntaxProxy.Declaration(AIndex: Integer): TNyxPascalSyntaxDeclaration;
begin
  Inc(DeclarationReads);

  if AIndex = TriggerOrdinal then
    Activated := True;

  if Activated and (AIndex = TargetOrdinal) and
    (Fault in [bfEarlierRoutine, bfEarlierParameter]) then
    Result := Changed.Declaration(AIndex)
  else
    Result := Value.Declaration(AIndex);
end;

function TSyntaxProxy.Named(AScope: Integer; const AName: TNyxText): Integer;
begin
  Result := Value.Named(AScope, AName);
end;

function TSyntaxProxy.ImportCount: Integer;
begin
  Result := Value.ImportCount;
end;

function TSyntaxProxy.ImportAt(AIndex: Integer): TNyxPascalSyntaxImport;
begin
  Result := Value.ImportAt(AIndex);
end;

function TSyntaxProxy.DiagnosticCount: Integer;
begin
  Result := Value.DiagnosticCount;
end;

function TSyntaxProxy.Diagnostic(AIndex: Integer): TNyxPascalSyntaxDiagnostic;
begin
  Result := Value.Diagnostic(AIndex);
end;

destructor TOuter.Destroy;
begin
  Inc(GPolicies);
  inherited Destroy;
end;

procedure TOuter.AdmitTypeWork(APhase: TNyxPascalAnalysisPhase; AUnits: Integer);
begin
  Inc(Calls);

  if (CancelAt > 0) and (Calls >= CancelAt) and (Cancellation <> nil) then
    Cancellation.Cancel;

  if (MutateAt > 0) and (Calls >= MutateAt) and (Length(Selection) = 2) then
    Selection[1] := Selection[0];

  if (RetireAt > 0) and (Calls >= RetireAt) and (Windows <> nil) then
    Windows.Retire;
end;

function TOuter.RemainingTypeBytes: Integer;
begin
  Result := Bytes;
end;

procedure WindowCases;
var
  LScope: INyxDocumentationTypeConstruction;
  LWindows: INyxDocumentationTextWindows;
  LText, LCopy: TNyxText;
  LAt: Integer;
  LRefused: Boolean;
  LOuter: TOuter;
  LOuterLease: INyxDocumentationTypeAdmission;
begin
  LText := 'A' + NyxScalarText($1F680) + #0 + 'Z';
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults.Bytes(5));
  LWindows := NewNyxDocumentationTextWindows(LText, LScope);
  try
    Check(LWindows.CopyWindow(NyxPascalSpan(1, 2)) = NyxScalarText($1F680) + #0,
      'A window uses scalar coordinates and exact supplementary/NUL bytes');
    LRefused := False;
    LCopy := '';
    try LCopy := LWindows.CopyWindow(NyxPascalSpan(1, 2));
    except on EArgumentException do LRefused := True; end;
    Check(LRefused and (LCopy = ''), 'Repeating a checked position still consumes actual copied-byte allowance');
  finally LWindows.Retire; LScope.Retire; end;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults.Work(5000));
  LWindows := NewNyxDocumentationTextWindows(TNyxText(StringOfChar('.', 4096)) + NyxScalarText($1F680) + 'Z', LScope);
  try
    for LAt := 1 to 40 do
      Check(LWindows.CopyWindow(NyxPascalSpan(4096, 1)) = NyxScalarText($1F680),
        'Repeated Unicode windows reuse checked positions within the unchanged shared work ceiling');
    Check(LWindows.CopyWindow(NyxPascalSpan(4097, 1)) = 'Z', 'A forward window resumes after the supplementary character');
    Check(LWindows.CopyWindow(NyxPascalSpan(0, 1)) = '.', 'A backwards window returns the correct earlier text');
    LRefused := False;
    try LCopy := LWindows.CopyWindow(NyxPascalSpan(4096, 1));
    except on EArgumentException do LRefused := True; end;
    Check(LRefused, 'After moving backwards the necessary prefix rescan still spends actual remaining work');
  finally LWindows.Retire; LScope.Retire; end;
  LOuter := TOuter.Create;
  LOuter.Bytes := 4;
  LOuterLease := LOuter;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults, nil, LOuterLease);
  LWindows := NewNyxDocumentationTextWindows(LText, LScope);
  try
    LRefused := False;
    try LCopy := LWindows.CopyWindow(NyxPascalSpan(1, 2));
    except on EArgumentException do LRefused := True; end;
    Check(LRefused, 'External remaining bytes reject before the five-byte window is allocated');
    LOuter.Bytes := 5;
    Check(LWindows.CopyWindow(NyxPascalSpan(1, 2)) = NyxScalarText($1F680) + #0,
      'A refused reserve neither advances checkpoint nor consumes copied bytes');
  finally LWindows.Retire; LScope.Retire; end;
  LScope := Construction;
  LText := 'tiny';
  LWindows := NewNyxDocumentationTextWindows(LText, LScope);
  LText := 'a different value';
  try
    Check(LWindows.CopyWindow(NyxPascalSpan(0, 4)) = 'tiny',
      'Changing the caller variable cannot invalidate checkpoints over the captured text value');
    LRefused := False;
    try LCopy := LWindows.CopyWindow(NyxPascalSpan(3, 2));
    except on EArgumentException do LRefused := True; end;
    Check(LRefused, 'Out-of-source windows refuse rather than truncate');
    Check(LWindows.CopyWindow(NyxPascalSpan(4, 0)) = '', 'An exact empty EOF remains valid');
    LWindows.Retire;
    LRefused := False;
    try LCopy := LWindows.CopyWindow(NyxPascalSpan(0, 1));
    except on ENyxDocumentationTypeExpressionCancelled do LRefused := True; end;
    Check(LRefused, 'Retired windows cannot revive their source or construction');
  finally LWindows.Retire; LScope.Retire; end;
  LScope := Construction;
  LWindows := NewNyxDocumentationTextWindows('source', LScope);
  try
    LScope.Retire;
    LRefused := False;
    try LCopy := LWindows.CopyWindow(NyxPascalSpan(0, 1));
    except on ENyxDocumentationTypeExpressionCancelled do LRefused := True; end;
    Check(LRefused, 'A retained cursor cannot read through a retired construction');
  finally LWindows.Retire; LScope.Retire; end;
  LOuter := TOuter.Create;
  LOuter.Bytes := NyxDocumentationMaximumBytes;
  LOuterLease := LOuter;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults, nil, LOuterLease);
  LWindows := NewNyxDocumentationTextWindows('source', LScope);
  LOuter.Windows := LWindows;
  LOuter.RetireAt := LOuter.Calls + 1;
  LScope := nil;
  try
    LCopy := '';
    LRefused := False;
    try LCopy := LWindows.CopyWindow(NyxPascalSpan(0, 1));
    except on ENyxDocumentationTypeExpressionCancelled do LRefused := True; end;
    Check(LRefused and (LCopy = ''), 'Reentrant retirement cannot destroy the active construction borrow or publish a copy');
  finally LOuter.Windows := nil; LWindows.Retire; end;
end;

procedure BatchCases;
var
  LSource: INyxPascalSyntaxSnapshot;
  LScope: INyxDocumentationTypeConstruction;
  LValues, LPrior: TNyxDocumentationRoutineSignatures;
  LSingle, LRetainedSingle: INyxDocumentationRichSignature;
  LOrdinals: TOrdinals;
  LAt: Integer;
  LText: TNyxText;
  LRefused: Boolean;
begin
  LSource := Syntax(CHeaders);
  LScope := Construction;
  try
    LValues := CaptureNyxDocumentationRoutineSignatures(LSource, [LSource.Named(0, 'Two'), LSource.Named(0, 'One')], LScope);
    Check((Length(LValues) = 2) and (LValues[0].ParameterAt(0).Name = 'B') and
      (LValues[1].ParameterAt(0).Name = 'A'), 'Batch keeps caller order even for backwards source selection');
    LPrior := LValues;
    LValues := nil;
    LRefused := False;
    try LValues := CaptureNyxDocumentationRoutineSignatures(LSource, [0, 0], LScope);
    except on EArgumentException do LRefused := True; end;
    Check(LRefused and (Length(LValues) = 0) and (LPrior[0].ParameterAt(0).Name = 'B'),
      'Duplicate selection refuses atomically and preserves earlier detached readers');
    LRefused := False;
    try LValues := CaptureNyxDocumentationRoutineSignatures(LSource, [], LScope);
    except on EArgumentException do LRefused := True; end;
    Check(LRefused and (Length(LValues) = 0), 'An empty batch has no partial success meaning');
    SetLength(LOrdinals, NyxDocumentationMaximumMembers + 1);
    LRefused := False;
    try LValues := CaptureNyxDocumentationRoutineSignatures(LSource, LOrdinals, LScope);
    except on EArgumentException do LRefused := True; end;
    Check(LRefused and (Length(LValues) = 0), 'Batch size retains the existing member ceiling');
  finally LScope.Retire; end;
  LSource := Syntax('function Good: Integer;' + #10 + 'procedure Bad(var A);');
  LScope := Construction;
  try
    LValues := nil;
    LRefused := False;
    try LValues := CaptureNyxDocumentationRoutineSignatures(LSource, [0, 1], LScope);
    except on EArgumentException do LRefused := True; end;
    Check(LRefused and (Length(LValues) = 0), 'A later untyped parameter cannot expose an earlier partial batch');
  finally LScope.Retire; end;
  LText := '{' + TNyxText(StringOfChar('x', 32768)) + '}' + #10;
  for LAt := 0 to 31 do
    LText := LText + 'function F' + IntToStr(LAt) + '(const A: Integer): Boolean;' + #10;
  LSource := Syntax(LText);
  SetLength(LOrdinals, 32);
  for LAt := 0 to High(LOrdinals) do
    LOrdinals[LAt] := LSource.Named(0, 'F' + IntToStr(LAt));
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults.Work(500000));
  try
    LValues := CaptureNyxDocumentationRoutineSignatures(LSource, LOrdinals, LScope);
    Check((Length(LValues) = 32) and (LScope.Receipt.Total <= 500000),
      'Complete ordered batch fits one unchanged shared allowance despite a long source prefix');
  finally LScope.Retire; end;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults.Work(500000));
  try
    LRefused := False;
    try
      for LAt := 0 to High(LOrdinals) do
      begin
        LSingle := CaptureNyxDocumentationRoutineSignature(LSource, LOrdinals[LAt], LScope);
        { A failed native managed-result assignment may clear its receiving
          variable. Retain each completed reader independently before retry. }
        LRetainedSingle := LSingle;
      end;
    except on EArgumentException do LRefused := True; end;
    Check(LRefused, 'Independent legacy captures honestly exhaust that same allowance by repeating prefix scans');
    Check((LValues[31].ParameterAt(0).Name = 'A') and (LRetainedSingle <> nil) and
      (LRetainedSingle.ParameterAt(0).Name = 'A'),
      'Refusal leaves complete batch and earlier legacy results independently readable');
  finally LScope.Retire; end;
end;

function ProxyBatch(AFault: TBatchFault; ACancelAt: Integer;
  out ACalls, ASourceReads, ADeclarationReads: Integer): TNyxDocumentationRoutineSignatures;
var
  LSource: TSyntaxProxy;
  LLexical: TLexicalProxy;
  LSourceLease: INyxPascalSyntaxSnapshot;
  LOuter: TOuter;
  LOuterLease: INyxDocumentationTypeAdmission;
  LScope: INyxDocumentationTypeConstruction;
  LOne, LTwo: Integer;
begin
  Result := nil;
  LSource := TSyntaxProxy.Create;
  LSource.Value := Syntax(CHeaders);
  LSource.Fault := AFault;
  LSourceLease := LSource;
  LLexical := TLexicalProxy.Create;
  LLexical.Value := LSource.Value.Lexical;
  LLexical.Fault := AFault;
  LSource.LexicalValue := LLexical;
  LOne := LSource.Value.Named(0, 'One');
  LTwo := LSource.Value.Named(0, 'Two');
  LSource.TriggerOrdinal := LTwo;
  LSource.TargetOrdinal := LOne;

  if AFault = bfEarlierRoutine then
    LSource.Changed := Syntax('function Won(A: Integer): Boolean;' + #10 + 'function Two(B: Integer): Boolean;')
  else if AFault = bfEarlierParameter then
  begin
    LSource.Changed := Syntax('function One(C: Integer): Boolean;' + #10 + 'function Two(B: Integer): Boolean;');
    LSource.TargetOrdinal := LSource.Value.Named(LSource.Value.Declaration(LOne).NestedScope, 'A');
  end;
  LOuter := TOuter.Create;
  LOuter.Bytes := NyxDocumentationMaximumBytes;
  LOuter.CancelAt := ACancelAt;
  LOuterLease := LOuter;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults, nil, LOuterLease);
  LOuter.Cancellation := LScope;
  LLexical.Cancellation := LScope;
  try
    Result := CaptureNyxDocumentationRoutineSignatures(LSourceLease, [LOne, LTwo], LScope);
  finally
    ACalls := LOuter.Calls;
    ASourceReads := LLexical.SourceReads;
    ADeclarationReads := LSource.DeclarationReads;
    Check((LLexical.SliceReads = 0) and (LLexical.TokenReads = 0),
      'An alternate supplier never supplies trusted token or slice authority');
    LLexical.Cancellation := nil;
    LOuter.Cancellation := nil;
    LScope.Retire;
  end;
end;

procedure AlternateCases;
var
  LFault: TBatchFault;
  LCalls, LSourceReads, LDeclarationReads, LLast, LBefore, LPolicies: Integer;
  LValues: TNyxDocumentationRoutineSignatures;
  LRefused: Boolean;
begin
  LBefore := GDisposed;
  LPolicies := GPolicies;
  LValues := ProxyBatch(bfNone, 0, LCalls, LSourceReads, LDeclarationReads);
  LLast := LCalls;
  Check((Length(LValues) = 2) and (LSourceReads = 2), 'Stable alternate source is observed as two complete source values');
  Check((GDisposed = LBefore + 2) and (GPolicies = LPolicies + 1),
    'Published batch retains neither source/lexical supplier nor cancellation/admission policy');
  for LFault := bfEarlierRoutine to bfCount do
  begin
    LValues := nil;
    LRefused := False;
    try LValues := ProxyBatch(LFault, 0, LCalls, LSourceReads, LDeclarationReads);
    except on EArgumentException do LRefused := True; end;
    Check(LRefused and (Length(LValues) = 0), 'Changed earlier row/parameter/full source/cancellation/count refuses the whole batch ' + IntToStr(Ord(LFault)));

    if LFault = bfCancelSource then
      Check((LSourceReads = 1) and (LDeclarationReads = 0), 'Cancelling the first source getter prevents all selected declaration reads');

    if LFault = bfCount then
      Check((LSourceReads = 0) and (LDeclarationReads = 0), 'Invalid ordinal extent refuses before source or declaration copying');
  end;
  LValues := nil;
  LRefused := False;
  try LValues := ProxyBatch(bfNone, LLast, LCalls, LSourceReads, LDeclarationReads);
  except on EArgumentException do LRefused := True; end;
  Check(LRefused and (Length(LValues) = 0) and (LCalls = LLast),
    'Final outward admission cancellation prevents publishing an otherwise complete vector');
end;

procedure SelectionCases;
var
  LSource: INyxPascalSyntaxSnapshot;
  LSelection: TOrdinals;
  LOuter: TOuter;
  LOuterLease: INyxDocumentationTypeAdmission;
  LScope: INyxDocumentationTypeConstruction;
  LValues: TNyxDocumentationRoutineSignatures;
  LAt, LLast: Integer;
  LRefused: Boolean;
begin
  LSource := Syntax(CHeaders);
  LLast := 0;
  for LAt := 0 to 1 do
  begin
    SetLength(LSelection, 2);
    LSelection[0] := LSource.Named(0, 'One');
    LSelection[1] := LSource.Named(0, 'Two');
    LOuter := TOuter.Create;
    LOuter.Bytes := NyxDocumentationMaximumBytes;
    LOuter.Selection := LSelection;

    if LAt = 1 then
      LOuter.MutateAt := LLast;
    LOuterLease := LOuter;
    LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults, nil, LOuterLease);
    try
      LValues := nil;
      LRefused := False;
      try LValues := CaptureNyxDocumentationRoutineSignatures(LSource, LSelection, LScope);
      except on EArgumentException do LRefused := True; end;

      if LAt = 0 then
      begin
        Check(not LRefused and (Length(LValues) = 2), 'Stable caller backing array retains its exact two-member selection');
        LLast := LOuter.Calls;
      end
      else
        Check(LRefused and (Length(LValues) = 0) and (LSelection[0] = LSelection[1]),
          'Last outward admission cannot swap caller ordinals into a duplicate successful batch');
    finally
      LOuter.Selection := nil;
      LScope.Retire;
    end;
  end;
end;

{$ifndef PAS2JS}
function RepositoryArgument: String;
begin

  if ParamCount <> 1 then
    raise EArgumentException.Create('Usage: nyx_documentation_signaturebatch_tests <repository-root>');
  Result := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(1)));

  if not DirectoryExists(Result) or not FileExists(Result + 'PROJECT.md') or
    not FileExists(Result + 'src' + PathDelim + 'nyx.controls.pas') or
    not FileExists(Result + 'src' + PathDelim + 'nyx.controls.factories.inc') then
    raise EArgumentException.Create('Repository root requires actual controls/factory source');
end;

function SourceSyntax(const APath: String; AFragment: TNyxPascalSyntaxFragment): INyxPascalSyntaxSnapshot;
var
  LStream: TMemoryStream;
  LBytes: TNyxBytes;
  LOptions: TNyxPascalSyntaxOptions;
begin
  LOptions := TNyxPascalSyntaxOptions.Defaults.Fragment(AFragment);
  LStream := TMemoryStream.Create;
  try
    LStream.LoadFromFile(APath);

    if LStream.Size > LOptions.LexicalOptions.MaximumSourceBytes then
      raise Exception.Create('Actual source exceeds existing lexical allowance');
    SetLength(LBytes, Integer(LStream.Size));

    if Length(LBytes) > 0 then
      Move(LStream.Memory^, LBytes[0], Length(LBytes));
    Result := NewNyxPascalSyntaxSnapshot(NyxDecodeUTF8(LBytes), LOptions);
  finally LStream.Free; end;
end;

procedure ActualBatch(const ASource: INyxPascalSyntaxSnapshot; AExpected: Integer);
var
  LOrdinals: TOrdinals;
  LAt, LCount: Integer;
  LDeclaration: TNyxPascalSyntaxDeclaration;
  LScope: INyxDocumentationTypeConstruction;
  LValues: TNyxDocumentationRoutineSignatures;
begin
  SetLength(LOrdinals, AExpected);
  LCount := 0;
  for LAt := 0 to ASource.DeclarationCount - 1 do
  begin
    LDeclaration := ASource.Declaration(LAt);

    if (LDeclaration.Scope = 0) and (LDeclaration.Kind = psdkRoutine) and
      (LDeclaration.Visibility in [psavInterface, psavUnspecified]) then
    begin
      Check(LCount < AExpected, 'Actual public source has no silently skipped extra global');
      LOrdinals[LCount] := LAt;
      Inc(LCount);
    end;
  end;
  Check(LCount = AExpected, 'All actual public globals are selected under their physical source owner');
  LScope := Construction;
  try
    LValues := CaptureNyxDocumentationRoutineSignatures(ASource, LOrdinals, LScope);
    Check(Length(LValues) = AExpected, 'Actual source selection publishes one complete signature batch');
    for LAt := 0 to High(LValues) do
    begin
      LDeclaration := ASource.Declaration(LOrdinals[LAt]);

      if LDeclaration.RoutineKind = prkFunction then
        Check((LValues[LAt].ResultUse <> nil) and
          (LValues[LAt].ResultUse.Expression.Text = ASource.Lexical.Slice(LDeclaration.TypeSpan)),
          'Actual function result stays exact unbound source syntax')
      else
        Check(LValues[LAt].ResultUse = nil, 'Actual procedure acquires no invented result');
      Inc(GActual);
    end;
  finally LScope.Retire; end;
end;

procedure ActualSources(const ARepository: String);
begin
  ActualBatch(SourceSyntax(ARepository + 'src' + PathDelim + 'nyx.controls.factories.inc', psfInterfaceDeclarations), 81);
  ActualBatch(SourceSyntax(ARepository + 'src' + PathDelim + 'nyx.controls.pas', psfModule), 8);
  Check(GActual = 89, 'All81 generated and8 handwritten globals retain their two original physical source owners');
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
    BatchCases;
    AlternateCases;
    SelectionCases;
    {$ifndef PAS2JS}
    ActualSources(LRepository);
    {$endif}
    Check((DocumentationControlsConstructed = 0) and (DocumentationCallbacksConstructed = 0) and
      (DocumentationCallbacksInvoked = 0) and (DocumentationGlobalFactoryCalls = 0) and
      (DocumentationGlobalRoutineCalls = 0), 'Source inspection executes no documented control/callback/factory/example');
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' portable signature batch checks';
    {$else}
    WriteLn('PASS ', GChecks, ' signature batch checks / all89 actual global signatures captured');
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
