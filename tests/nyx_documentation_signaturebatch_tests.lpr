{ Atomic source-signature batches and admitted Unicode window reuse. MIT. }
program nyx_documentation_signaturebatch_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.bytes, nyx.scheduler, nyx.pascal.lexing,
  nyx.pascal.syntax, nyx.pascal.work, nyx.pascal.files,
  nyx.pascal.context, nyx.pascal.translation, nyx.pascal.syntax.translated,
  nyx.pascal.types, nyx.documentation,
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
  { A session policy changes only caller-owned handles or shared input elements
    at a measured admission. It supplies no syntax/source authority, and its
    destruction proves that detached signatures retain no outward policy. }
  TSessionAdmission = class(TInterfacedObject, INyxDocumentationTypeAdmission)
  public
    Used, Trigger, AvailableBytes, Action: Integer;
    RemainingCalls, RemainingAction, WorkAtRemaining: Integer;
    destructor Destroy; override;
    procedure AdmitTypeWork(APhase: TNyxPascalAnalysisPhase; AUnits: Integer);
    function RemainingTypeBytes: Integer;
  end;
  { The same immutable facts behind a different ordinary interface cannot earn
    the private owner's session path. Getter counters prove early refusal. }
  TSessionViewDelegate = class(TInterfacedObject, INyxPascalTranslatedSyntax)
  public
    Value: INyxPascalTranslatedSyntax;
    GetterReads: Integer;
    function Source: INyxPascalTranslation;
    function Syntax: INyxPascalSyntaxSnapshot;
    function CompleteTranslation: Boolean;
    function BlockedAt: Integer;
    function OriginCount: Integer;
    function Origin(AIndex: Integer): TNyxPascalSyntaxOrigin;
    function OriginAt(AVirtualOffset: Integer): Integer;
    function VirtualAt(AOccurrence, APhysicalOffset: Integer): Integer;
    function Part(AIndex: Integer; const ASpan: TNyxPascalSpan): TNyxPascalSyntaxOrigin;
    function TypeOfDeclaration(AIndex: Integer; const AOptions: TNyxPascalTypeOptions;
      const ACancellation: INyxCancellationScope): INyxPascalTypeSyntax;
  end;

const
  CHeaders: TNyxText = 'function One(A: Integer): Boolean;' + #10 +
    'function Two(B: Integer): Boolean;';

var
  GChecks, GDisposed, GPolicies: Integer;
  GSourceScope: INyxDocumentationTypeConstruction;
  GSignatureSource: INyxDocumentationSignatureSource;
  GSignatureOrdinals: TNyxDocumentationRoutineOrdinals;
  GPolicyDestroyed: Integer;
  {$ifndef PAS2JS}
  GActual: Integer;
  {$endif}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not AValue then
    raise Exception.Create(AReason);
end;

function TSessionViewDelegate.Source: INyxPascalTranslation;
begin Inc(GetterReads); Result := Value.Source; end;
function TSessionViewDelegate.Syntax: INyxPascalSyntaxSnapshot;
begin Inc(GetterReads); Result := Value.Syntax; end;
function TSessionViewDelegate.CompleteTranslation: Boolean;
begin Inc(GetterReads); Result := Value.CompleteTranslation; end;
function TSessionViewDelegate.BlockedAt: Integer;
begin Inc(GetterReads); Result := Value.BlockedAt; end;
function TSessionViewDelegate.OriginCount: Integer;
begin Inc(GetterReads); Result := Value.OriginCount; end;
function TSessionViewDelegate.Origin(AIndex: Integer): TNyxPascalSyntaxOrigin;
begin Inc(GetterReads); Result := Value.Origin(AIndex); end;
function TSessionViewDelegate.OriginAt(AVirtualOffset: Integer): Integer;
begin Inc(GetterReads); Result := Value.OriginAt(AVirtualOffset); end;
function TSessionViewDelegate.VirtualAt(AOccurrence, APhysicalOffset: Integer): Integer;
begin Inc(GetterReads); Result := Value.VirtualAt(AOccurrence, APhysicalOffset); end;
function TSessionViewDelegate.Part(AIndex: Integer; const ASpan: TNyxPascalSpan): TNyxPascalSyntaxOrigin;
begin Inc(GetterReads); Result := Value.Part(AIndex, ASpan); end;
function TSessionViewDelegate.TypeOfDeclaration(AIndex: Integer; const AOptions: TNyxPascalTypeOptions;
  const ACancellation: INyxCancellationScope): INyxPascalTypeSyntax;
begin Inc(GetterReads); Result := Value.TypeOfDeclaration(AIndex, AOptions, ACancellation); end;

destructor TSessionAdmission.Destroy;
begin
  Inc(GPolicyDestroyed);
  inherited Destroy;
end;

procedure TSessionAdmission.AdmitTypeWork(APhase: TNyxPascalAnalysisPhase; AUnits: Integer);
begin

  if (Ord(APhase) < Ord(Low(TNyxPascalAnalysisPhase))) or
    (Ord(APhase) > Ord(High(TNyxPascalAnalysisPhase))) or (AUnits < 1) then
    raise Exception.Create('A session policy received an invalid work phase');
  Inc(Used, AUnits);

  if (Trigger > 0) and (Used >= Trigger) then
  begin
    Trigger := 0;
    case Action of
      3: GSourceScope := nil;
      4: GSignatureOrdinals[0] := -1;
      5: GSignatureSource.Retire;
      6: GSignatureOrdinals := nil;
      7: GSourceScope.Retire;
      8: GSignatureSource := nil;
      else begin end;
    end;
  end;
end;

function TSessionAdmission.RemainingTypeBytes: Integer;
var
  LAction: Integer;
begin
  Inc(RemainingCalls);
  LAction := RemainingAction;
  RemainingAction := 0;
  case LAction of
    1: GSourceScope.Retire;
    2: GSourceScope.AdmitText('1234');
    else begin end;
  end;

  if LAction <> 0 then
    WorkAtRemaining := Used;

  if AvailableBytes > 0 then
    Result := AvailableBytes
  else
    Result := 4 * 1024 * 1024;
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

procedure PortableSignatureSource;
var
  LFiles: INyxPascalFileCatalog;
  LContext: INyxPascalCompilerContext;
  LView: INyxPascalTranslatedSyntax;
  LAlternate: TSessionViewDelegate;
  LAlternateView: INyxPascalTranslatedSyntax;
  LSession: INyxDocumentationSignatureSource;
  LScope: INyxDocumentationTypeConstruction;
  LSelection: TNyxDocumentationRoutineOrdinals;
  LValues, LPrevious: TNyxDocumentationRoutineSignatures;
  LText: TNyxText;
  LFailure: Boolean;
  LPolicy: TSessionAdmission;
  LAdmission: INyxDocumentationTypeAdmission;
  LAt, LWork, LLimit, LAction, LDestroyed: Integer;
begin
  LText := 'unit SignatureSource; interface type ';
  for LAt := 0 to 399 do
    LText := LText + 'TPadding' + TNyxText(IntToStr(LAt)) + ' = record end; ';
  LText := LText + 'procedure First(const A: TNumber = 0); ' +
    'function Second(var B: TNumber): TNumber; implementation end.';
  LContext := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi);
  LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(
    NyxPascalPhysicalFile('SignatureSource.pas'), LText);
  LView := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFiles,
    NyxPascalPhysicalFile('SignatureSource.pas'), LContext,
    TNyxPascalTranslationOptions.Defaults), TNyxPascalSyntaxOptions.Defaults);
  Check(LView.CompleteTranslation and (LView.Syntax.DiagnosticCount = 0),
    'The session prefix witness is complete owned translated declaration syntax');
  LLimit := LView.Syntax.Lexical.Scalars * 3;
  SetLength(LSelection, 1);
  LScope := NewNyxDocumentationTypeConstruction(
    TNyxDocumentationTypeExpressionLimits.Defaults.Work(LLimit));
  try
    LSession := NewNyxDocumentationSignatureSource(LView, LScope);
    LSelection[0] := LView.Syntax.Named(0, 'First');
    LValues := LSession.CaptureDeclared(LSelection);
    LPrevious := LValues;
    LSelection[0] := LView.Syntax.Named(0, 'Second');
    LValues := LSession.CaptureDeclared(LSelection);
    Check((Length(LValues) = 1) and (LValues[0].ParameterCount = 1) and
      (LValues[0].ParameterAt(0).Name = 'B') and
      (LValues[0].ParameterAt(0).Passing = ndpVar) and
      (LValues[0].ResultUse.Expression.Text = 'TNumber') and
      (LPrevious[0].ParameterAt(0).Name = 'A') and
      LPrevious[0].ParameterAt(0).HasDefault and
      (LPrevious[0].ParameterAt(0).DefaultPascal = '0'),
      'One checked session preserves original parameter/default/result shape across separate selections');
    Check(LScope.Receipt.Total < LLimit,
      'A shared source cursor fits three source extents without repeated complete-unit validation');
    LSession.Retire;
    LFailure := False;
    try
      LSession.CaptureDeclared(LSelection);
    except
      on ENyxDocumentationTypeExpressionCancelled do LFailure := True;
    end;
    Check(LFailure and (LPrevious[0].ParameterAt(0).Name = 'A'),
      'Retirement refuses new captures and leaves detached prior signatures readable');
  finally

    if LSession <> nil then
      LSession.Retire;
    LSession := nil;
    LScope.Retire;
  end;
  LScope := NewNyxDocumentationTypeConstruction(
    TNyxDocumentationTypeExpressionLimits.Defaults.Work(LLimit));
  LFailure := False;
  try
    try
      LSelection[0] := LView.Syntax.Named(0, 'First');
      CaptureNyxDocumentationDeclaredRoutineSignatures(LView.Syntax, LSelection, LScope);
      LSelection[0] := LView.Syntax.Named(0, 'Second');
      CaptureNyxDocumentationDeclaredRoutineSignatures(LView.Syntax, LSelection, LScope);
    except
      on ENyxDocumentationTypeExpression do LFailure := True;
      on ENyxPascalAnalysisWorkLimit do LFailure := True;
    end;
  finally
    LScope.Retire;
  end;
  Check(LFailure, 'The unchanged independent snapshot batches still charge repeated full-source agreement');
  LAlternate := TSessionViewDelegate.Create;
  LAlternateView := LAlternate;
  LAlternate.Value := LView;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults);
  LFailure := False;
  try
    try
      NewNyxDocumentationSignatureSource(LAlternateView, LScope);
    except
      on ENyxDocumentationSignature do LFailure := True;
    end;
  finally
    LScope.Retire;
  end;
  Check(LFailure and (LAlternate.GetterReads = 0),
    'A delegating view cannot borrow the owned session fast path or reach source getters');
  LSelection[0] := LView.Syntax.Named(0, 'First');
  LPolicy := TSessionAdmission.Create;
  LAdmission := LPolicy;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults, nil, LAdmission);
  try
    LSession := NewNyxDocumentationSignatureSource(LView, LScope);
    LValues := LSession.CaptureDeclared(LSelection);
    LWork := LPolicy.Used;
  finally

    if LSession <> nil then
      LSession.Retire;
    LSession := nil;
    LScope.Retire;
  end;
  for LAction := 4 to 8 do
  begin
    GSignatureOrdinals := Copy(LSelection);
    LPolicy := TSessionAdmission.Create;
    LAdmission := LPolicy;
    LPolicy.Trigger := LWork;
    LPolicy.Action := LAction;
    GSourceScope := NewNyxDocumentationTypeConstruction(
      TNyxDocumentationTypeExpressionLimits.Defaults, nil, LAdmission);
    LScope := GSourceScope;
    GSignatureSource := NewNyxDocumentationSignatureSource(LView, LScope);
    LSession := GSignatureSource;

    if LAction = 8 then
      LSession := nil;
    LFailure := False;
    LValues := nil;
    try
      try
        LValues := GSignatureSource.CaptureDeclared(GSignatureOrdinals);
      except
        on ENyxDocumentationSignature do LFailure := True;
        on ENyxDocumentationTypeExpressionCancelled do LFailure := True;
      end;
      Check((LPolicy.Trigger = 0) and
        (((LAction in [4, 5, 7]) and LFailure and (LValues = nil)) or
          ((LAction in [6, 8]) and not LFailure and (Length(LValues) = 1))),
        'Last-admission mutation/retirement refuses; released caller handles retain in-flight value ownership, action=' + IntToStr(LAction));
    finally
      GSignatureSource := nil;

      if LSession <> nil then
        LSession.Retire;
      LSession := nil;
      GSignatureOrdinals := nil;
      GSourceScope := nil;
      LScope.Retire;
      LScope := nil;
    end;
  end;
  LDestroyed := GPolicyDestroyed;
  LAdmission := nil;
  Check((GPolicyDestroyed = LDestroyed + 1) and
    (LPrevious[0].ParameterAt(0).DefaultPascal = '0'),
    'Session retirement releases outward policy while published signatures keep only detached fields');
  LPolicy := TSessionAdmission.Create;
  LAdmission := LPolicy;
  LPolicy.Trigger := 1;
  LPolicy.Action := 3;
  GSourceScope := NewNyxDocumentationTypeConstruction(
    TNyxDocumentationTypeExpressionLimits.Defaults, nil, LAdmission);
  LSession := NewNyxDocumentationSignatureSource(LView, GSourceScope);
  try
    LValues := LSession.CaptureDeclared(LSelection);
    Check((GSourceScope = nil) and (Length(LValues) = 1),
      'The session factory leases construction before admission can release its last caller slot');
  finally

    if LSession <> nil then
      LSession.Retire;
    LSession := nil;
  end;
  LPolicy := TSessionAdmission.Create;
  LAdmission := LPolicy;
  LPolicy.AvailableBytes := 1;
  LScope := NewNyxDocumentationTypeConstruction(
    TNyxDocumentationTypeExpressionLimits.Defaults, nil, LAdmission);
  try
    LSession := NewNyxDocumentationSignatureSource(LView, LScope);
    LFailure := False;
    LValues := nil;
    try
      LValues := LSession.CaptureDeclared(LSelection);
    except
      on ENyxDocumentationTypeExpression do LFailure := True;
    end;
    Check(LFailure and (LValues = nil),
      'Owned syntax still requires remaining byte admission for each selected type window');
  finally

    if LSession <> nil then
      LSession.Retire;
    LSession := nil;
    LScope.Retire;
  end;
  for LAction := 1 to 2 do
  begin
    LPolicy := TSessionAdmission.Create;
    LAdmission := LPolicy;
    LPolicy.RemainingAction := LAction;
    GSourceScope := NewNyxDocumentationTypeConstruction(
      TNyxDocumentationTypeExpressionLimits.Defaults.Bytes(8), nil, LAdmission);
    LScope := GSourceScope;
    LSession := NewNyxDocumentationSignatureSource(LView, LScope);
    LFailure := False;
    LValues := nil;
    try
      try
        LValues := LSession.CaptureDeclared(LSelection);
      except
        on ENyxDocumentationTypeExpressionCancelled do LFailure := LAction = 1;
        on ENyxDocumentationTypeExpression do LFailure := LAction = 2;
      end;
      Check(LFailure and (LValues = nil) and (LPolicy.RemainingAction = 0) and
        (LPolicy.RemainingCalls >= LAction) and (LPolicy.WorkAtRemaining > 0) and
        (LPolicy.Used = LPolicy.WorkAtRemaining),
        'Remaining-byte retirement or reentrant local reservation refuses before later type work, action=' + IntToStr(LAction));
    finally
      LSession.Retire;
      LSession := nil;
      GSourceScope := nil;
      LScope.Retire;
    end;
  end;
  SetLength(LSelection, 2);
  LSelection[1] := LSelection[0];
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults);
  try
    LSession := NewNyxDocumentationSignatureSource(LView, LScope);
    LFailure := False;
    try
      LSession.CaptureDeclared(LSelection);
    except
      on ENyxDocumentationSignature do LFailure := True;
    end;
    Check(LFailure, 'Duplicate owned-source selections refuse without publishing a partial vector');
    LSelection[1] := LView.Syntax.Named(0, 'TPadding0');
    LFailure := False;
    LValues := nil;
    try
      LValues := LSession.CaptureDeclared(LSelection);
    except
      on ENyxDocumentationSignature do LFailure := True;
    end;
    Check(LFailure and (LValues = nil) and
      (LPrevious[0].ParameterAt(0).DefaultPascal = '0'),
      'A later non-routine refuses the whole session selection while detached prior success remains readable');
  finally

    if LSession <> nil then
      LSession.Retire;
    LSession := nil;
    LScope.Retire;
  end;
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
    not FileExists(Result + 'src' + PathDelim + 'nyx.controls.types.inc') or
    not FileExists(Result + 'src' + PathDelim + 'nyx.controls.facades.inc') or
    not FileExists(Result + 'src' + PathDelim + 'nyx.controls.implementation.inc') or
    not FileExists(Result + 'src' + PathDelim + 'nyx.controls.facades.implementation.inc') or
    not FileExists(Result + 'src' + PathDelim + 'nyx.controls.factories.inc') then
    raise EArgumentException.Create('Repository root requires actual controls and all five literal includes');
end;

function SourceText(const APath: String): TNyxText;
var
  LStream: TMemoryStream;
  LBytes: TNyxBytes;
  LOptions: TNyxPascalLexicalOptions;
begin
  LOptions := TNyxPascalLexicalOptions.Defaults;
  LStream := TMemoryStream.Create;
  try
    LStream.LoadFromFile(APath);

    if LStream.Size > LOptions.MaximumSourceBytes then
      raise Exception.Create('Actual source exceeds existing lexical allowance');
    SetLength(LBytes, Integer(LStream.Size));

    if Length(LBytes) > 0 then
      Move(LStream.Memory^, LBytes[0], Length(LBytes));
    Result := NyxDecodeUTF8(LBytes);
  finally LStream.Free; end;
end;

function SourceSyntax(const APath: String; AFragment: TNyxPascalSyntaxFragment): INyxPascalSyntaxSnapshot;
begin
  Result := NewNyxPascalSyntaxSnapshot(SourceText(APath),
    TNyxPascalSyntaxOptions.Defaults.Fragment(AFragment));
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

procedure ActualSignatureSource(const ARepository: TNyxText);
var
  LFiles: INyxPascalFileCatalog;
  LContext: INyxPascalCompilerContext;
  LView: INyxPascalTranslatedSyntax;
  LSyntax: INyxPascalSyntaxSnapshot;
  LSession: INyxDocumentationSignatureSource;
  LScope: INyxDocumentationTypeConstruction;
  LSelection: TNyxDocumentationRoutineOrdinals;
  LValues: TNyxDocumentationRoutineSignatures;
  LOwner, LRoutine: TNyxPascalSyntaxDeclaration;
  LModule: TNyxPascalSyntaxScope;
  LAt, LCount, LGroup, LGroups, LRoutines, LGlobals, LPublicTypes,
    LRead, LWork: Integer;
  procedure AddFile(const AName: TNyxText);
  begin
    LFiles := LFiles.WithFile(NyxPascalPhysicalFile(AName),
      SourceText(ExcludeTrailingPathDelimiter(String(ARepository)) + PathDelim +
        'src' + PathDelim + String(AName)));
  end;
  procedure SelectScope(AScope: Integer);
  var
    LBefore: Integer;
  begin
    LModule := LSyntax.Scope(AScope);
    Check(LModule.Complete and not LModule.UnsupportedSyntax,
      'Every selected actual controls signature scope must be complete');
    LSelection := nil;
    LAt := LModule.FirstDeclaration;
    LRead := 0;
    while LAt <> -1 do
    begin
      Inc(LRead);
      Check(LRead <= LModule.DeclarationCount,
        'Actual public scope links remain within their complete declared count');
      LRoutine := LSyntax.Declaration(LAt);

      if (LRoutine.Kind = psdkRoutine) and
        (LRoutine.Visibility in [psavUnspecified, psavInterface, psavPublic,
          psavPublished, psavAutomated]) then
      begin
        LCount := Length(LSelection);
        Check(LCount < NyxDocumentationMaximumMembers,
          'No actual public signature is omitted to fit the member ceiling');
        SetLength(LSelection, LCount + 1);
        LSelection[LCount] := LAt;
      end;
      LAt := LRoutine.Next;
    end;

    if Length(LSelection) = 0 then
      Exit;
    LValues := LSession.CaptureDeclared(LSelection);
    Check(Length(LValues) = Length(LSelection),
      'Each complete actual public callable group publishes atomically');
    for LBefore := 0 to High(LSelection) do
    begin
      LRoutine := LSyntax.Declaration(LSelection[LBefore]);

      if LRoutine.TypeSpan.Length <> 0 then
        Check((LValues[LBefore].ResultUse <> nil) and
          (LValues[LBefore].ResultUse.Expression.Text = LSyntax.Lexical.Slice(LRoutine.TypeSpan)),
          'The actual result retains its exact unbound projected type window')
      else
        Check(LValues[LBefore].ResultUse = nil,
          'A result-free actual callable acquires no invented type');
    end;
    Inc(LGroups);
    Inc(LRoutines, Length(LSelection));

    if AScope = 0 then
      LGlobals := Length(LSelection)
    else
      Inc(LPublicTypes);
  end;
begin
  { These are the real unit's five literal include owners, not fabricated unit
    fragments or a reduced declaration list. Import/type binding is deliberately
    outside this signature-only witness and remains required for the package. }
  LFiles := NewNyxPascalFileCatalog(pfcExact);
  AddFile('nyx.controls.pas');
  AddFile('nyx.controls.types.inc');
  AddFile('nyx.controls.facades.inc');
  AddFile('nyx.controls.factories.inc');
  AddFile('nyx.controls.implementation.inc');
  AddFile('nyx.controls.facades.implementation.inc');
  LContext := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi).WithSymbol(NyxPascalDefine('PAS2JS'), ppdUndefined);
  LView := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFiles,
    NyxPascalPhysicalFile('nyx.controls.pas'), LContext,
    TNyxPascalTranslationOptions.Defaults), TNyxPascalSyntaxOptions.Defaults);
  Check(LView.Source.Resolved and LView.CompleteTranslation and
    (LView.Source.DiagnosticCount = 0) and (LView.Syntax.DiagnosticCount = 0),
    'Actual controls and all five original includes produce one complete translated signature source');
  LSyntax := LView.Syntax;
  LGroups := 0;
  LRoutines := 0;
  LGlobals := 0;
  LPublicTypes := 0;
  LValues := nil;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults);
  try
    LSession := NewNyxDocumentationSignatureSource(LView, LScope);
    SelectScope(0);
    for LGroup := 0 to LSyntax.DeclarationCount - 1 do
    begin
      LOwner := LSyntax.Declaration(LGroup);

      if (LOwner.Kind = psdkType) and (LOwner.Scope = 0) and
        (LOwner.NestedScope >= 0) and
        (LOwner.Visibility in [psavUnspecified, psavInterface, psavPublic,
          psavPublished, psavAutomated]) then
        SelectScope(LOwner.NestedScope);
    end;
    LWork := LScope.Receipt.Total;
    Check((LGlobals = 89) and (LPublicTypes >= 81) and (LRoutines > LGlobals) and
      (LWork <= TNyxDocumentationTypeExpressionLimits.Defaults.MaximumWork),
      'All89 globals and every actual public type callable group share one unchanged default allowance');
    LSession.Retire;
    LSession := nil;
  finally

    if LSession <> nil then
      LSession.Retire;
    LScope.Retire;
  end;
  Check((Length(LValues) > 0) and (LValues[0].ParameterCount >= 0),
    'Actual detached signatures remain readable after source-session and construction retirement');
  WriteLn('actual controls signature source / globals=', LGlobals,
    ' public type groups=', LPublicTypes, ' routines=', LRoutines,
    ' groups=', LGroups, ' shared work=', LWork);
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
    PortableSignatureSource;
    {$ifndef PAS2JS}
    ActualSources(LRepository);
    ActualSignatureSource(TNyxText(LRepository));
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
