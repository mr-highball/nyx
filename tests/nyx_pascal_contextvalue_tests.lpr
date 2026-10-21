{ Exact owned context values; controlled facts grant no compiler authority. MIT. }
program nyx_pascal_contextvalue_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.scheduler, nyx.pascal.context, nyx.pascal.lexing,
  nyx.pascal.reals, nyx.pascal.work {$ifdef PAS2JS}, Web{$endif};

type
  { Private test framing distinguishes numeric fields from borrowed text and
    refuses its own byte ceiling before a field is appended. It is not a public
    context decoder or a claim of compiler-backed measurements. }
  TValues = class(TInterfacedObject, INyxPascalCompilerContextValues)
  public
    Data: TNyxText;
    Fields, Maximum, CancelAt, DropAt: Integer;
    Cancellation: INyxCancellationScope;
    procedure Before;
    procedure Append(const AValue: TNyxText);
    procedure IntegerValue(AValue: Integer);
    procedure TextValue(const AValue: TNyxText);
  end;
  { An ordinary forwarding context exposes valid facts but owns a separate
    interface. Value-owner admission refuses it before reading any getter.
    The pure syntax selector may inspect its family/version; a controlled getter
    then replaces caller inputs to qualify the selector's local input leases. }
  TDelegate = class(TInterfacedObject, INyxPascalCompilerContext)
  public
    Value: INyxPascalCompilerContext;
    Reads: Integer;
    ReplaceOperandOnFamily: Boolean;
    destructor Destroy; override;
    function Family: TNyxPascalCompilerFamily;
    function Version: TNyxPascalCompilerVersion;
    function Mode: TNyxPascalLanguageMode;
    function DefinesComplete: Boolean;
    function Symbol(const AName: TNyxPascalDefineRef): TNyxPascalDefineValue;
    function Option(AName: Char): TNyxPascalKnowledge;
    function Feature(AFeature: TNyxPascalLanguageFeature): TNyxPascalKnowledge;
    function WithSymbol(const AName: TNyxPascalDefineRef;
      AKind: TNyxPascalDefineKind; AInteger: Integer): INyxPascalCompilerContext;
    function CompleteDefines(AValue: Boolean): INyxPascalCompilerContext;
    function WithOption(AName: Char; AValue: TNyxPascalKnowledge): INyxPascalCompilerContext;
    function WithFeature(AFeature: TNyxPascalLanguageFeature;
      AValue: TNyxPascalKnowledge): INyxPascalCompilerContext;
    function InMode(AValue: TNyxPascalLanguageMode): INyxPascalCompilerContext;
    function UnknownState: INyxPascalCompilerContext;
    function SameFacts(const AOther: INyxPascalCompilerContext): Boolean;
    function SymbolCount: Integer;
    function SymbolName(AIndex: Integer): TNyxPascalDefineRef;
  end;

var
  GChecks: Integer;
  GCallerContext: INyxPascalCompilerContext;
  GCallerReceiver: INyxPascalCompilerContextValues;
  GCallerOperand: TNyxText;
  GRetiredDelegates, GLastDelegateReads: Integer;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then
    raise Exception.Create(AReason);
end;

procedure TValues.Before;
begin
  Inc(Fields);

  if Fields = DropAt then
  begin
    GCallerContext := nil;
    GCallerReceiver := nil;
  end;

  if (Fields = CancelAt) and (Cancellation <> nil) then
    Cancellation.Cancel;
end;

procedure TValues.Append(const AValue: TNyxText);
begin

  if (Maximum > 0) and (Length(AValue) > Maximum - Length(Data)) then
    raise ERangeError.Create('Receiver byte allowance exhausted');
  Data := Data + AValue;
end;

procedure TValues.IntegerValue(AValue: Integer);
begin
  Before;
  Append('i' + TNyxText(IntToStr(AValue)) + ';');
end;

procedure TValues.TextValue(const AValue: TNyxText);
begin
  Before;
  { Every context key/name is canonical ASCII, so storage units equal bytes. }
  Append('t' + TNyxText(IntToStr(Length(AValue))) + ':' + AValue + ';');
end;

destructor TDelegate.Destroy;
begin
  Inc(GRetiredDelegates);
  GLastDelegateReads := Reads;
  inherited Destroy;
end;

function TDelegate.Family: TNyxPascalCompilerFamily;
begin
  Inc(Reads);

  if ReplaceOperandOnFamily then
  begin
    GCallerOperand := 'Changed';
    GCallerContext := nil;
  end;
  Result := Value.Family;
end;
function TDelegate.Version: TNyxPascalCompilerVersion;
begin Inc(Reads); Result := Value.Version; end;
function TDelegate.Mode: TNyxPascalLanguageMode;
begin Inc(Reads); Result := Value.Mode; end;
function TDelegate.DefinesComplete: Boolean;
begin Inc(Reads); Result := Value.DefinesComplete; end;
function TDelegate.Symbol(const AName: TNyxPascalDefineRef): TNyxPascalDefineValue;
begin Inc(Reads); Result := Value.Symbol(AName); end;
function TDelegate.Option(AName: Char): TNyxPascalKnowledge;
begin Inc(Reads); Result := Value.Option(AName); end;
function TDelegate.Feature(AFeature: TNyxPascalLanguageFeature): TNyxPascalKnowledge;
begin Inc(Reads); Result := Value.Feature(AFeature); end;
function TDelegate.WithSymbol(const AName: TNyxPascalDefineRef;
  AKind: TNyxPascalDefineKind; AInteger: Integer): INyxPascalCompilerContext;
begin Result := Value.WithSymbol(AName, AKind, AInteger); end;
function TDelegate.CompleteDefines(AValue: Boolean): INyxPascalCompilerContext;
begin Result := Value.CompleteDefines(AValue); end;
function TDelegate.WithOption(AName: Char; AValue: TNyxPascalKnowledge): INyxPascalCompilerContext;
begin Result := Value.WithOption(AName, AValue); end;
function TDelegate.WithFeature(AFeature: TNyxPascalLanguageFeature;
  AValue: TNyxPascalKnowledge): INyxPascalCompilerContext;
begin Result := Value.WithFeature(AFeature, AValue); end;
function TDelegate.InMode(AValue: TNyxPascalLanguageMode): INyxPascalCompilerContext;
begin Result := Value.InMode(AValue); end;
function TDelegate.UnknownState: INyxPascalCompilerContext;
begin Result := Value.UnknownState; end;
function TDelegate.SameFacts(const AOther: INyxPascalCompilerContext): Boolean;
begin Inc(Reads); Result := Value.SameFacts(AOther); end;
function TDelegate.SymbolCount: Integer;
begin Inc(Reads); Result := Value.SymbolCount; end;
function TDelegate.SymbolName(AIndex: Integer): TNyxPascalDefineRef;
begin Inc(Reads); Result := Value.SymbolName(AIndex); end;

function Content(const AContext: INyxPascalCompilerContext;
  const AExecution: INyxCancellationScope = nil): TNyxText;
var
  LValues: TValues;
  LLease: INyxPascalCompilerContextValues;
begin
  LValues := TValues.Create;
  LLease := LValues;
  VisitNyxPascalCompilerContextValues(AContext, LLease, AExecution);
  Result := LValues.Data;
end;

procedure Different(const ABase, AChanged: INyxPascalCompilerContext);
begin
  Check(not ABase.SameFacts(AChanged), 'Controlled variants differ under the existing exact context owner');
  Check(Content(ABase) <> Content(AChanged), 'Context values preserve that independent fact difference');
end;

{ The fixed pre-extension stream is a compatibility oracle, not another context
  encoder. Its empty policies/options/features were retained before this field
  existed. Known helper policy must distinguish both values, every owning scope
  and exact work, without changing that absent-policy stream or its allowance. }
procedure HelperPolicyValues;
const
  CLegacy: TNyxText =
    'i1;i1;i30301;i3;i0;i0;t0:;' +
    'i0;i0;i0;i0;i1;t0:;i0;' +
    'i0;i0;i0;i0;i0;i0;i0;i0;i0;i0;i0;i0;i0;i0;i0;i0;i0;' +
    'i0;i0;i0;i0;i0;i0;' +
    'i26;i0;i0;i0;i0;i0;i0;i0;i0;i0;i0;i0;i0;i0;' +
    'i0;i0;i0;i0;i0;i0;i0;i0;i0;i0;i0;i0;i0;' +
    'i13;i0;i0;i0;i0;i0;i0;i0;i0;i0;i0;i0;i0;i0;';
var
  LBase, LCurrent, LSaved, LRestored, LRoot, LNested: INyxPascalCompilerContext;
  LPolicy, LSavedPolicy: TNyxPascalKnowledge;
  LMode: TNyxPascalLanguageMode;
  LValues: TValues;
  LReceiver: INyxPascalCompilerContextValues;
  LBudget: INyxPascalAnalysisBudget;
  LScope: INyxCancellationScope;
  LText, LPrevious: TNyxText;
  LFailed: Boolean;
begin
  LBase := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi);
  Check(Content(LBase) = CLegacy,
    'An absent helper policy preserves the literal pre-extension value stream');
  LBudget := NewNyxPascalAnalysisBudget(78);
  try
    Check((Content(LBase, LBudget) = CLegacy) and (LBudget.Receipt.Total = 78),
      'The old absent-policy stream retains its exact 78-unit work allowance');
  finally
    LBudget.Retire;
  end;
  for LPolicy := Low(TNyxPascalKnowledge) to High(TNyxPascalKnowledge) do
  begin
    LCurrent := NyxPascalWithCompilerPolicies(LBase,
      TNyxPascalCompilerPolicies.Unknown.WithMultipleHelpers(LPolicy));
    Check(NyxPascalCompilerPolicies(LBase).MultipleHelpers = ppkUnknown,
      'An independent policy copy cannot modify its original context');
    Check(LCurrent.SameFacts(CaptureNyxPascalCompilerContext(LCurrent)) and
      (Content(LCurrent) = Content(CaptureNyxPascalCompilerContext(LCurrent))),
      'Detached capture retains exact helper policy and context values');
    Check(NyxPascalCompilerPolicies(LCurrent.WithOption('R', ppkTrue)).MultipleHelpers = LPolicy,
      'Unrelated immutable context updates retain the independent helper policy');
    Check(NyxPascalCompilerPolicies(LCurrent.UnknownState).MultipleHelpers = ppkUnknown,
      'An unknown compiler effect revokes helper-policy knowledge');
    for LMode := Low(TNyxPascalLanguageMode) to High(TNyxPascalLanguageMode) do
      Check(NyxPascalCompilerPolicies(LCurrent.InMode(LMode)).MultipleHelpers = ppkUnknown,
        'Public InMode supplies no compiler-backed helper default');
    LText := Content(LCurrent);
    LValues := TValues.Create;
    LReceiver := LValues;
    LBudget := NewNyxPascalAnalysisBudget(99);
    try
      VisitNyxPascalCompilerContextValues(LCurrent, LReceiver, LBudget);

      if LPolicy = ppkUnknown then
        Check((LText = CLegacy) and (LValues.Fields = 78) and
          (LBudget.Receipt.Total = 78), 'Explicit Unknown remains byte- and work-compatible')
      else
      begin
        Different(LBase, LCurrent);
        Check((Pos('t19:multiple-helpers-v1;i' + TNyxText(IntToStr(Ord(LPolicy))) + ';',
          LText) > 0) and (LValues.Fields = 80) and (LBudget.Receipt.Total = 99),
          'Each known value has its own tagged fields and pays their full work');
      end;
    finally
      LBudget.Retire;
    end;

    if LPolicy <> ppkUnknown then
    begin
      LPrevious := CLegacy;
      LFailed := False;
      LBudget := NewNyxPascalAnalysisBudget(98);
      try
        try
          LPrevious := Content(LCurrent, LBudget);
        except
          on ENyxPascalAnalysisWorkLimit do LFailed := True;
        end;
        Check(LFailed and (LPrevious = CLegacy),
          'One less than the known-policy allowance refuses without replacing a published value');
      finally
        LBudget.Retire;
      end;
      LValues := TValues.Create;
      LReceiver := LValues;
      LScope := NewNyxCancellationScope;
      LValues.Cancellation := LScope;
      LValues.CancelAt := 32;
      LFailed := False;
      try
        VisitNyxPascalCompilerContextValues(LCurrent, LReceiver, LScope);
      except
        on ENyxPascalAnalysisCancelled do LFailed := True;
      end;
      Check(LFailed and (LValues.Fields = 32),
        'Cancellation inside the optional policy tag refuses before its numeric value callback');
    end;
    for LSavedPolicy := Low(TNyxPascalKnowledge) to High(TNyxPascalKnowledge) do
    begin
      LSaved := NyxPascalWithCompilerPolicies(LBase.WithOption('R', ppkFalse),
        TNyxPascalCompilerPolicies.Unknown.WithMultipleHelpers(LSavedPolicy));
      LRestored := RestoreNyxPascalLocalSettings(LCurrent.WithOption('R', ppkTrue), LSaved);
      Check((NyxPascalCompilerPolicies(LRestored).MultipleHelpers = LPolicy) and
        (LRestored.Option('R') = ppkFalse),
        'POP restores local options while retaining current global helper knowledge, including Unknown');
    end;
    LRoot := NewNyxPascalCompilerContext(ppcFPC,
      TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi, pcsoSystemBootstrap);
    LNested := NyxPascalWithBootstrapContext(LBase,
      NyxPascalWithCompilerPolicies(LRoot,
        TNyxPascalCompilerPolicies.Unknown.WithMultipleHelpers(LPolicy)));
    Check(NyxPascalCompilerPolicies(NyxPascalBootstrapContext(
      CaptureNyxPascalCompilerContext(LNested))).MultipleHelpers = LPolicy,
      'Bootstrap policy survives its independently owned recursive capture');

    if LPolicy <> ppkUnknown then
    begin
      Different(NyxPascalWithBootstrapContext(LBase, LRoot), LNested);
      Different(NyxPascalWithBootstrapContext(LCurrent, LRoot), LNested);
    end;
  end;
  Different(NyxPascalWithCompilerPolicies(LBase,
    TNyxPascalCompilerPolicies.Unknown.WithMultipleHelpers(ppkFalse)),
    NyxPascalWithCompilerPolicies(LBase,
      TNyxPascalCompilerPolicies.Unknown.WithMultipleHelpers(ppkTrue)));
end;

procedure Run;
var
  LBase, LRoot, LOther, LCaptured: INyxPascalCompilerContext;
  LPolicies: TNyxPascalCompilerPolicies;
  LFeature: TNyxPascalLanguageFeature;
  LChannel: TNyxPascalDiagnosticChannel;
  LAlignment: TNyxPascalCodeAlignmentRequest;
  LBudget: INyxPascalAnalysisBudget;
  LScope: INyxCancellationScope;
  LValues: TValues;
  LReceiver: INyxPascalCompilerContextValues;
  LDelegate: TDelegate;
  LAlternate: INyxPascalCompilerContext;
  LText, LPrevious: TNyxText;
  LFailed: Boolean;
  LUsed, LOption, LFields: Integer;
begin
  { These are authored qualification values. No compiler process is launched. }
  LBase := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi);
  Different(LBase, NewNyxPascalCompilerContext(ppcPas2JS,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi));
  Different(LBase, NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 2), ppmDelphi));
  Different(LBase, LBase.InMode(ppmObjFPC));
  Different(LBase, LBase.CompleteDefines(True));
  Different(LBase, LBase.WithSymbol(NyxPascalDefine('UNKNOWN'), ppdUnknown));
  LOther := LBase.WithSymbol(NyxPascalDefine('A'), ppdDefined)
    .WithSymbol(NyxPascalDefine('B'), ppdInteger, -1);
  Different(LOther, LBase.WithSymbol(NyxPascalDefine('B'), ppdInteger, -1)
    .WithSymbol(NyxPascalDefine('A'), ppdDefined));
  for LOption := Ord('A') to Ord('Z') do
    Different(LBase, LBase.WithOption(Chr(LOption), ppkFalse));
  for LFeature := Low(TNyxPascalLanguageFeature) to High(TNyxPascalLanguageFeature) do
    Different(LBase, LBase.WithFeature(LFeature, ppkTrue));
  LPolicies := TNyxPascalCompilerPolicies.Unknown;
  for LChannel := Low(TNyxPascalDiagnosticChannel) to High(TNyxPascalDiagnosticChannel) do
    Different(LBase, NyxPascalWithCompilerPolicies(LBase,
      LPolicies.EmitDiagnostics(LChannel, ppkFalse)));
  Different(LBase, NyxPascalWithCompilerPolicies(LBase, LPolicies.AlignRecords(pra8)));
  Different(LBase, NyxPascalWithCompilerPolicies(LBase, LPolicies.WithInlining(ppkTrue)));
  Different(LBase, NyxPascalWithCompilerPolicies(LBase, LPolicies.StoreEnums(pes2)));
  Different(LBase, NyxPascalWithCompilerPolicies(LBase, LPolicies.PackSets(pspFixed)));
  Different(LBase, NyxPascalWithCompilerPolicies(LBase, LPolicies.CallBy(pccCdecl)));
  Different(LBase, NyxPascalWithCompilerPolicies(LBase, LPolicies.RequestSmartLinking(ppkFalse)));
  Different(LBase, NyxPascalWithCompilerPolicies(LBase, LPolicies.RequestGoto(ppkTrue)));
  Different(LBase, NyxPascalWithCompilerPolicies(LBase, LPolicies.ReadAssemblyBy(parATT)));
  Different(LBase, NyxPascalWithCompilerPolicies(LBase, LPolicies.RequestPointerChecking(ppcrDefault)));
  Different(LBase, NyxPascalWithCompilerPolicies(LBase, LPolicies.UseInterfaces(pifsCORBA)));
  Different(LBase, NyxPascalWithCompilerPolicies(LBase, LPolicies.RequestExcessPrecision(ppkFalse)));
  Different(LBase, NyxPascalWithCompilerPolicies(LBase, LPolicies.ApplyModeExcessPrecision(ppkTrue)));
  Different(LBase, NyxPascalWithCompilerPolicies(LBase,
    LPolicies.RequestMinimumRealPrecision(prprBinary32)));
  Different(LBase, NyxPascalWithCompilerPolicies(LBase, LPolicies.RequestIEEEErrors(ppkFalse)));
  Different(LBase, NyxPascalWithWarning(LBase, NyxPascalWarningNumber(3018), pwaOff));
  Different(LBase, NyxPascalWithWarning(LBase, NyxPascalWarningName('deprecated'), pwaError, ppkUnknown));
  LAlignment := Default(TNyxPascalCodeAlignmentRequest).Add(pcaProcedure, 16).Add(pcaRecordMinimum, -1);
  Different(LBase, NyxPascalWithCodeAlignment(LBase, LAlignment, ppkUnknown));
  Different(LBase, NyxPascalWithOptimization(LBase,
    TNyxPascalOptimizationRequest.Select(pomInitial)));
  Different(LBase, NyxPascalWithOptimization(LBase,
    TNyxPascalOptimizationRequest.Toggle(pofFastMath, False), ppkUnknown));
  Different(LBase, NyxPascalWithCapturedSize(LBase, NyxPascalSizeReference('Integer'),
    NyxPascalSizeFact(pssKnown, ppmDelphi, ppkTrue, 4)));
  Different(LBase, NyxPascalWithCapturedSize(LBase, NyxPascalSizeReference('Absent'),
    NyxPascalSizeFact(pssUndeclared, ppmDelphi, ppkTrue)));
  Different(LBase, NyxPascalWithCapturedStorage(LBase, NyxPascalStorageKey('nyx.storage.1|value'),
    NyxPascalStorageFact(psfsKnown, 0)));
  Different(LBase, NyxPascalWithConfiguredFPU(LBase, NyxPascalFPU('X87')));
  Different(LBase, NyxPascalWithRealParser(LBase,
    TNyxPascalRealParserPolicy.NearestEven(pbpBinary64)));
  Different(LBase, NyxPascalWithRealEvaluation(LBase,
    TNyxPascalRealEvaluationPolicy.NearestEven(pbpX87Extended80)));
  { The context constructor already records Configured source intent. Use a
    different declared state; do not claim the unchanged value is a difference. }
  Different(LBase, NyxPascalWithSourceFPU(LBase, TNyxPascalSourceFPUSelection.Unknown));
  LRoot := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi, pcsoSystemBootstrap);
  Different(LBase, LRoot);
  LOther := NyxPascalWithBootstrapContext(LBase, LRoot);
  Different(LBase, LOther);
  Different(LOther, NyxPascalWithBootstrapContext(LBase,
    LRoot.WithSymbol(NyxPascalDefine('ROOT'), ppdInteger, 42)));
  LCaptured := CaptureNyxPascalCompilerContext(LOther);
  Check(LOther.SameFacts(LCaptured) and (Content(LOther) = Content(LCaptured)),
    'Equal detached captures retain exact context values, independently of object identity');

  LText := Content(LOther);
  LBudget := NewNyxPascalAnalysisBudget(100000);
  Check(Content(LOther, LBudget) = LText, 'Work admission does not alter the immutable value sequence');
  LUsed := LBudget.Receipt.Total;
  LBudget.Retire;
  LBudget := NewNyxPascalAnalysisBudget(LUsed);
  Check(Content(LOther, LBudget) = LText, 'An exact existing analysis allowance fits the same value traversal');
  LBudget.Retire;
  LBudget := NewNyxPascalAnalysisBudget(LUsed - 1);
  LPrevious := LText;
  LFailed := False;
  try
    LPrevious := Content(LOther, LBudget);
  except
    on ENyxPascalAnalysisWorkLimit do LFailed := True;
  end;
  Check(LFailed and (LPrevious = LText), 'Late admission refusal cannot replace the caller previously published value');
  LBudget.Retire;

  LValues := TValues.Create;
  LReceiver := LValues;
  LDelegate := TDelegate.Create;
  LAlternate := LDelegate;
  LDelegate.Value := LBase;
  LFailed := False;
  try
    VisitNyxPascalCompilerContextValues(LAlternate, LReceiver);
  except
    on ENyxPascalContext do LFailed := True;
  end;
  Check(LFailed and (LDelegate.Reads = 0) and (LValues.Fields = 0),
    'Alternate contexts are refused before any context getter or receiver call');
  LFailed := False;
  try
    VisitNyxPascalCompilerContextValues(nil, LReceiver);
  except
    on ENyxPascalContext do LFailed := True;
  end;
  Check(LFailed and (LValues.Fields = 0), 'Missing context cannot begin an output stream');
  LFailed := False;
  try
    VisitNyxPascalCompilerContextValues(LBase, nil);
  except
    on ENyxPascalContext do LFailed := True;
  end;
  Check(LFailed, 'A receiver is an explicit visitation requirement');

  LValues.Maximum := 1;
  LFailed := False;
  try
    VisitNyxPascalCompilerContextValues(LBase, LReceiver);
  except
    on ERangeError do LFailed := True;
  end;
  Check(LFailed and (LValues.Data = ''), 'Receiver byte refusal propagates without a copied field');
  LValues.Maximum := 0;
  LValues.Fields := 0;
  LScope := NewNyxCancellationScope;
  LScope.Cancel;
  LFailed := False;
  try
    VisitNyxPascalCompilerContextValues(LBase, LReceiver, LScope);
  except
    on ENyxPascalAnalysisCancelled do LFailed := True;
  end;
  Check(LFailed and (LValues.Fields = 0), 'Pre-cancellation refuses before receiver output');

  LScope := NewNyxCancellationScope;
  LValues.Cancellation := LScope;
  LValues.CancelAt := 1;
  LFailed := False;
  try
    VisitNyxPascalCompilerContextValues(LBase, LReceiver, LScope);
  except
    on ENyxPascalAnalysisCancelled do LFailed := True;
  end;
  Check(LFailed and (LValues.Fields = 1), 'Receiver cancellation is observed before the next field');

  LValues := TValues.Create;
  LReceiver := LValues;
  VisitNyxPascalCompilerContextValues(LBase, LReceiver);
  LFields := LValues.Fields;
  LValues.Data := '';
  LValues.Fields := 0;
  LScope := NewNyxCancellationScope;
  LValues.Cancellation := LScope;
  LValues.CancelAt := LFields;
  LFailed := False;
  try
    VisitNyxPascalCompilerContextValues(LBase, LReceiver, LScope);
  except
    on ENyxPascalAnalysisCancelled do LFailed := True;
  end;
  Check(LFailed and (LValues.Fields = LFields), 'Last-field cancellation still refuses visitation completion');

  LValues := TValues.Create;
  LReceiver := LValues;
  LValues.DropAt := 1;
  GCallerContext := LBase;
  GCallerReceiver := LReceiver;
  VisitNyxPascalCompilerContextValues(GCallerContext, GCallerReceiver);
  Check((GCallerContext = nil) and (GCallerReceiver = nil) and (LValues.Data = Content(LBase)),
    'Local context and receiver leases survive caller-variable release during the first callback');
  Check(Content(LBase) = Content(NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi)),
    'All traversals and refusals leave the original immutable context exact');
end;

{ Exercise both selected grammar and strict fallback with borrowed caller
  slots. No extra interface local keeps the delegate alive during the call:
  clearing that slot in Family requires the selector's own context lease. }
procedure ConditionalOperandLeases;
var
  LName: TNyxPascalDefineRef;
  LCase, LRetired: Integer;
  LFailed: Boolean;

  procedure Prepare;
  var
    LDelegate: TDelegate;
  begin
    { Let the class-to-interface conversion temporaries retire before the
      outer call. Only the caller's explicit context slot remains afterward. }
    LDelegate := TDelegate.Create;
    GCallerContext := LDelegate;
    LDelegate.ReplaceOperandOnFamily := True;

    if LCase = 0 then
      LDelegate.Value := NewNyxPascalCompilerContext(ppcFPC,
        TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi)
    else
      LDelegate.Value := NewNyxPascalCompilerContext(ppcFPC,
        TNyxPascalCompilerVersion.Create(3, 3, 2), ppmDelphi);
  end;

begin
  for LCase := 0 to 2 do
  begin
    LRetired := GRetiredDelegates;
    Prepare;
    GCallerOperand := 'Original';

    if LCase <> 1 then
      GCallerOperand := GCallerOperand + '(annotation)';
    LName := Default(TNyxPascalDefineRef);
    LFailed := False;
    try
      try
        LName := NyxPascalConditionalDefine(GCallerContext, GCallerOperand);
      except
        on ENyxPascalContext do LFailed := True;
      end;
      Check((GCallerContext = nil) and (GCallerOperand = 'Changed'),
        'The alternate context really releases and replaces both caller slots');
      Check((GRetiredDelegates = LRetired + 1) and (GLastDelegateReads = 2),
        'The syntax selector leases the context through both observations and retires it on return');

      if LCase = 2 then
        Check(LFailed, 'Strict fallback validates the original malformed operand after caller mutation')
      else
        Check(not LFailed and (LName.Name = 'original'),
          'Both grammar paths select the retained original text after caller mutation');
    finally
      GCallerContext := nil;
      GCallerOperand := '';
    end;
  end;
end;

begin
  try
    Run;
    HelperPolicyValues;
    ConditionalOperandLeases;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' context value checks';
    {$else}
    WriteLn('PASS ', GChecks, ' context value checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + E.Message;
      {$else}
      WriteLn(StdErr, E.ClassName, ': ', E.Message, ' at check ', GChecks);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
