{ Portable qualification of source-owned implicit routine receivers/results.
  The separate native fixture is an independent compiler witness, not a parser
  fixture rewritten to make Nyx's expected answer easier to obtain. }
program nyx_pascal_method_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Classes, nyx.text, nyx.scheduler, nyx.editing, nyx.pascal.lexing,
  nyx.pascal.lexing.append, nyx.pascal.context, nyx.pascal.files, nyx.pascal.syntax, nyx.pascal.syntax.translated,
  nyx.pascal.translation, nyx.pascal.binding, nyx.pascal.work, nyx.pascal.names,
  nyx.pascal.project, nyx.pascal.drafts, nyx.pascal.sources, nyx.pascal.completion, nyx.pascal.completion.owned
  {$ifndef PAS2JS}, method.receivers.fixture{$endif};

const
  CHeader: TNyxText = 'unit Methods; interface type TBadge=class ' +
    'Value:LongInt; class var Shared:LongInt; constructor Create; destructor Destroy; ' +
    'function Work:TBadge; class function Read:TBadge; class function Detached:TBadge; static; ' +
    'end; TBox<T>=class Value:T; function Echo:TBox<T>; function ReadValue:T; end; ' +
    'TValue=record Value:LongInt; function Echo:TValue; end; ' +
    'TOld=object Value:LongInt; function Echo:TOld; end; implementation ';
  CBody: TNyxText =
    'constructor TBadge.Create; begin Self{constructor}; end; ' +
    'destructor TBadge.Destroy; begin Self{destructor}; end; ' +
    'function TBadge.Work:TBadge; ' +
    'procedure Capture; begin Self{nested-self}; Result{outer-result}; end; ' +
    'function Own:LongInt; begin Result{inner-result}; Self{inner-self}; end; ' +
    'begin Self{instance}; Result{result}; Self.Value{field}; Result.Value{result-field}; end; ' +
    'class function TBadge.Read:TBadge; begin Self{class}; Self.Shared{class-field}; Result{class-result}; end; ' +
    'class function TBadge.Detached:TBadge; begin Self{static}; Result{static-result}; end; ' +
    'function TBox<T>.Echo:TBox<T>; begin Self{generic}; Result{generic-result}; Self.Value{generic-field}; end; ' +
    'function TBox<T>.ReadValue:T; begin Result{formal-result}; end; ' +
    'function TValue.Echo:TValue; begin Self{record}; Result{record-result}; end; ' +
    'function TOld.Echo:TOld; begin Self{object}; Result{object-result}; end; end.';

var
  GChecks: Integer;
  GWorld: INyxPascalSemanticWorld;
  GText: TNyxText;

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

function View(const AName, AText: TNyxText; const AContext: INyxPascalCompilerContext): INyxPascalTranslatedSyntax;
begin
  Result := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile(AName), AText), NyxPascalPhysicalFile(AName), AContext,
    TNyxPascalTranslationOptions.Defaults), TNyxPascalSyntaxOptions.Defaults);
end;

procedure Build(const AText: TNyxText; const AContext: INyxPascalCompilerContext);
begin
  GText := AText;
  GWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults)
    .WithModule(View('system.pas', 'unit System; interface type TObject=class end; implementation end.', AContext))
    .WithModule(View('methods.pas', AText, AContext));
end;

function Site(const AMarker: TNyxText): INyxPascalBindingSite;
var
  LAt: Integer;
begin
  LAt := Pos('{' + AMarker + '}', GText);
  Check(LAt > 0, 'The query has an exact source marker: ' + String(AMarker));
  LAt := NyxTextScalarCount(Copy(GText, 1, LAt - 1));
  { Marker comments have no projected token. Map the final identifier scalar,
    then move to its exact virtual end rather than asking a comment for origin. }
  LAt := GWorld.ModuleAt(1).VirtualAt(0, LAt - 1);
  Check(LAt >= 0, 'The query maps to its original occurrence: ' + String(AMarker));
  Inc(LAt);
  Result := GWorld.At(1, GWorld.ModuleAt(1).Syntax.ScopeAt(LAt), LAt);
end;

function TerminalType(const AType: INyxPascalSemanticType): INyxPascalSemanticType;
begin
  Result := AType;
  while (Result <> nil) and (Result.Kind in [pstAlias, pstDistinct]) do Result := Result.Target;
end;

procedure Declared(const AName, AMarker, ATypeName: TNyxText; AClass: Boolean = False);
var
  LAnswer: INyxPascalResolution;
  LType: INyxPascalSemanticType;
begin
  LAnswer := GWorld.Lookup(Site(AMarker), NyxPascalBindingName(AName));
  Check(LAnswer.Status = pbsBound, 'The implicit expression binds: ' + String(AMarker) +
    ' status=' + IntToStr(Ord(LAnswer.Status)) + ' issue=' + IntToStr(Ord(LAnswer.Issue)));
  Check((LAnswer.Count = 0) and (LAnswer.BoundType <> nil), 'Implicit storage never fabricates a declaration');
  LType := TerminalType(LAnswer.BoundType);

  if AClass then
  begin
    Check(LType.Kind = pstClassOf, 'A class method Self is a class reference');
    LType := TerminalType(LType.Target);
  end;
  Check((LType.Kind = pstDeclared) and (LType.Definition.Declaration.Name = ATypeName),
    'The implicit type keeps its exact declared owner: ' + String(AMarker));
end;

procedure Contracts;
var
  LAnswer: INyxPascalResolution;
  LFormal: INyxPascalFormalParameter;
  LAt, LStatic: Integer;
  LDecl: TNyxPascalSyntaxDeclaration;
begin
  Build(CHeader + CBody, Facts);
  Check(GWorld.ModuleAt(1).Syntax.DiagnosticCount = 0, 'Complete method syntax has no declaration diagnostics');
  Declared('Self', 'instance', 'TBadge');
  Declared('Result', 'result', 'TBadge');
  Declared('Self', 'constructor', 'TBadge');
  Declared('Self', 'destructor', 'TBadge');
  Declared('Self', 'nested-self', 'TBadge');
  Declared('Self', 'inner-self', 'TBadge');
  Declared('Result', 'outer-result', 'TBadge');
  Declared('Self', 'class', 'TBadge', True);
  Declared('Result', 'class-result', 'TBadge');
  Declared('Result', 'static-result', 'TBadge');
  Declared('Self', 'record', 'TValue');
  Declared('Result', 'record-result', 'TValue');
  Declared('Self', 'object', 'TOld');
  Declared('Result', 'object-result', 'TOld');
  Declared('Self', 'generic', 'TBox');
  Declared('Result', 'generic-result', 'TBox');
  LAnswer := GWorld.Lookup(Site('field'), NyxPascalBindingName('Self.Value'));
  Check((LAnswer.Status = pbsBound) and (LAnswer.Count = 1) and
    (LAnswer.SymbolAt(0).Declaration.Name = TNyxText('Value')), 'Qualified Self follows the existing member binder');
  LAnswer := GWorld.Lookup(Site('result-field'), NyxPascalBindingName('Result.Value'));
  Check((LAnswer.Status = pbsBound) and (LAnswer.Count = 1), 'Qualified Result follows the same typed member path');
  LAnswer := GWorld.Lookup(Site('class-field'), NyxPascalBindingName('Self.Shared'));
  Check((LAnswer.Status = pbsBound) and (LAnswer.Count = 1), 'A class reference exposes class storage');
  LAnswer := GWorld.Lookup(Site('class'), NyxPascalBindingName('Self.Value'));
  Check(LAnswer.Status <> pbsBound, 'Class Self does not donate instance fields');
  LAnswer := GWorld.Lookup(Site('static'), NyxPascalBindingName('Self'));
  Check((LAnswer.Status <> pbsBound) and (LAnswer.BoundType = nil), 'STATIC is recovered from the declaration, not guessed from the implementation');
  LAnswer := GWorld.Lookup(Site('inner-result'), NyxPascalBindingName('Result'));
  Check((LAnswer.Status = pbsBound) and (TerminalType(LAnswer.BoundType).Builtin = pbbLongInt),
    'A nested function owns its result instead of capturing the outer one');
  LAnswer := GWorld.Lookup(Site('formal-result'), NyxPascalBindingName('Result'));
  Check((LAnswer.Status = pbsBound) and Supports(LAnswer.BoundType, INyxPascalFormalParameter, LFormal) and
    (LFormal.Name = TNyxText('T')), 'A generic method result preserves its exact formal parameter');
  LAnswer := GWorld.Lookup(Site('generic-field'), NyxPascalBindingName('Self.Value'));
  LAnswer := GWorld.TypeOfSymbol(LAnswer.SymbolAt(0));
  Check((LAnswer.Status = pbsBound) and Supports(LAnswer.BoundType, INyxPascalFormalParameter, LFormal),
    'A generic Self carries its owner environment into fields');
  LStatic := 0;
  for LAt := 0 to GWorld.ModuleAt(1).Syntax.DeclarationCount - 1 do
  begin
    LDecl := GWorld.ModuleAt(1).Syntax.Declaration(LAt);

    if psdfStatic in LDecl.Flags then Inc(LStatic);

    if LDecl.Kind <> psdkRoutine then
    begin
      Check(LDecl.RoutineKind = prkNone, 'Non-routines have no implicit routine role');
    end;
  end;
  Check(LStatic = 1, 'Only the authored STATIC declaration retains its typed modifier');
end;

procedure UnfinishedAndRefused;
var
  LAnswer: INyxPascalResolution;
  LAt: Integer;
begin
  Build(CHeader + 'function TBadge.Work:TBadge; begin Self{unfinished-self}; Result{unfinished-result}', Facts);
  Declared('Self', 'unfinished-self', 'TBadge');
  Declared('Result', 'unfinished-result', 'TBadge');
  Build('unit Methods; interface implementation procedure Run; begin Result{procedure}; Self{global}; end; end.', Facts);
  LAnswer := GWorld.Lookup(Site('procedure'), NyxPascalBindingName('Result'));
  Check((LAnswer.Status <> pbsBound) and (LAnswer.BoundType = nil), 'An ordinary procedure has no function result');
  LAnswer := GWorld.Lookup(Site('global'), NyxPascalBindingName('Self'));
  Check((LAnswer.Status <> pbsBound) and (LAnswer.BoundType = nil), 'A free routine has no method receiver');
  Build(CHeader + 'function TBadge.Work:TBadge; begin {$modeswitch result-} Result{disabled}; end; end.', Facts);
  LAt := Pos('{disabled}', GText) - 2;
  Check(not GWorld.ModuleAt(1).CompleteTranslation and (GWorld.ModuleAt(1).VirtualAt(0, LAt) < 0),
    'An unmodeled result switch refuses projection rather than borrowing an old mode');
  Build(CHeader + 'function TBadge.Work:TBadge; begin Result{unknown}; end; end.',
    NewNyxPascalCompilerContext(ppcUnknown, TNyxPascalCompilerVersion.Create(0, 0, 0), ppmDelphi));
  LAnswer := GWorld.Lookup(Site('unknown'), NyxPascalBindingName('Result'));
  Check((LAnswer.Status = pbsUnresolved) and (LAnswer.BoundType = nil), 'An unknown compiler grants no implicit result semantics');
  Build('unit Methods; interface type TBadge=class function Mix(A:LongInt):TBadge; overload; ' +
    'class function Mix(A:Boolean):TBadge; static; overload; end; implementation ' +
    'function TBadge.Mix(A:LongInt):TBadge; begin Self{mixed}; end; end.', Facts);
  LAnswer := GWorld.Lookup(Site('mixed'), NyxPascalBindingName('Self'));
  Check((LAnswer.Status = pbsUnresolved) and (LAnswer.Issue = pbiOverloadContext),
    'Disagreeing overload receiver contracts need signature matching, never a guessed Self');
  Build('unit Methods; interface type TBadge=class class constructor Start; class destructor Stop; end; ' +
    'implementation class constructor TBadge.Start; begin Self{class-constructor}; end; ' +
    'class destructor TBadge.Stop; begin Self{class-destructor}; end; end.', Facts);
  LAnswer := GWorld.Lookup(Site('class-constructor'), NyxPascalBindingName('Self'));
  Check((LAnswer.Status <> pbsBound) and (LAnswer.BoundType = nil),
    'Class initialization has no metaclass or instance Self');
  LAnswer := GWorld.Lookup(Site('class-destructor'), NyxPascalBindingName('Self'));
  Check((LAnswer.Status <> pbsBound) and (LAnswer.BoundType = nil),
    'Class finalization has no metaclass or instance Self');
end;

procedure CompletionPath;
var
  LFiles: INyxPascalFileCatalog;
  LRoots: array of TNyxPascalPhysicalFile;
  LProvider: INyxPascalCompletionProvider;
  LSources, LPreview: INyxPascalSourceSnapshot;
  LRequest: INyxPascalCompletionRequest;
  LAnswer: INyxPascalCompletionResult;
  LMetadata: INyxOwnedPascalCompletionResult;
  LText: TNyxText;
  LAt, LNewCaret: Integer;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('sdk/system.pas'),
    'unit System; interface type TObject=class end; implementation end.');
  SetLength(LRoots, 1);
  LRoots[0] := NyxPascalPhysicalFile('sdk/system.pas');
  LProvider := NewNyxOwnedPascalCompletionProvider(Facts, LFiles, LRoots, TNyxOwnedPascalCompletionOptions.Defaults);
  LText := CHeader + 'function TBadge.Work:TBadge; begin Result.Val';
  LSources := CaptureNyxPascalSources(NewNyxPascalProject(NyxPascalUnit('Methods'), LText));
  LAt := NyxTextScalarCount(LText);
  LRequest := NewNyxPascalCompletionRequest(LSources).At(TNyxPascalSourceRef.Main, LAt);
  LAnswer := LProvider.Complete(LRequest);
  Check((LAnswer.Status = pcsReady) and (LAnswer.Count = 1) and (LAnswer.Total = 1),
    'Ordinary completion follows Result inside an unfinished method: ' + String(LAnswer.MessageText));
  Check((LAnswer.Item(0).Name = TNyxText('Value')) and (LAnswer.ExpectedText = TNyxText('Val')),
    'The existing insertion contract replaces only the member prefix');
  Supports(LAnswer, INyxOwnedPascalCompletionResult, LMetadata);
  LProvider := nil;
  Check(LMetadata.DefinitionSource(0).Slice(LMetadata.Definition(0).Span) = TNyxText('Value'),
    'Member navigation retains a real declaration after provider retirement');
  LPreview := LAnswer.Preview(0, LSources, LNewCaret);
  Check((LNewCaret = LAt + 2) and (LPreview.Text(TNyxPascalSourceRef.Main) = LText + 'ue'),
    'Detached insertion preserves the exact unfinished method');
end;

procedure OwnershipAndWork;
var
  LSite: INyxPascalBindingSite;
  LAttempt, LRetained: INyxPascalResolution;
  LBudget: INyxPascalAnalysisBudget;
  LCancel: INyxCancellationScope;
  LChanged, LOriginal: INyxPascalSemanticWorld;
  LAt, LFit: Integer;
  LRefused: Boolean;
begin
  Build(CHeader + CBody, Facts);
  LSite := Site('generic-field');
  LOriginal := GWorld;
  LFit := -1;
  for LAt := 1 to 3000 do
  begin
    LAttempt := nil;
    LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      try
        LAttempt := GWorld.Lookup(LSite, NyxPascalBindingName('Self.Value'), LBudget);
        Check((LAttempt.Status = pbsBound) and (LAttempt.Count = 1), 'A sufficient allowance binds the actual generic member');
        LRetained := LAttempt;
        LFit := LAt;
      except
        on ENyxPascalAnalysisWorkLimit do
          Check((LAttempt = nil) and (LBudget.Receipt.Total <= LAt), 'Each smaller allowance refuses without publishing a partial answer');
      end;
    finally
      LBudget.Retire;
    end;

    if LFit > 0 then Break;
  end;
  Check(LFit > 1, 'Implicit receiver analysis has a bounded first fit');
  WriteLn('method receiver work / first fit=', LFit, ' every smaller allowance refused');
  Check(GWorld.TypeOfSymbol(LRetained.SymbolAt(0)).Status = pbsBound,
    'Retiring a work allowance does not retire the retained source symbol');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LAttempt := nil;
  LRefused := False;
  try
    LAttempt := GWorld.Lookup(LSite, NyxPascalBindingName('Self.Value'), LCancel);
  except
    on ENyxPascalAnalysisCancelled do LRefused := True;
  end;
  Check(LRefused and (LAttempt = nil), 'Cancellation publishes no implicit binding');
  LChanged := GWorld.WithModule(View('methods.pas', 'unit Methods; interface implementation end.', Facts));
  LRefused := False;
  try
    LChanged.Lookup(LSite, NyxPascalBindingName('Self.Value'));
  except
    on ENyxPascalBinding do LRefused := True;
  end;
  Check(LRefused, 'A replacement source world refuses a stale method site');
  Check(LOriginal.Lookup(LSite, NyxPascalBindingName('Self.Value')).Status = pbsBound,
    'Replacing a module leaves the original immutable method world usable');
end;

procedure CheckpointRoles;
const
  CPrefix: TNyxText = 'unit Roles; interface type TBadge=class ' +
    'constructor Create; destructor Destroy; class function Build:TBadge; static; end; ' +
    'implementation function Read:LongInt; begin Result:=42; end; ';
var
  LLex, LAppended: INyxPascalLexicalSnapshot;
  LBefore, LAfter: INyxPascalSyntaxSnapshot;
  LAt: Integer;
  LOld, LNew: TNyxPascalSyntaxDeclaration;
begin
  LLex := NewNyxPascalAppendLexical(CPrefix, TNyxPascalLexicalOptions.Defaults);
  LBefore := NewNyxPascalSyntaxFromLexical(LLex, TNyxPascalSyntaxOptions.Defaults);
  LAppended := AppendNyxPascalLexical(LLex, 'end.');
  LAfter := AppendNyxPascalSyntax(LBefore, LAppended, TNyxPascalSyntaxOptions.Defaults);
  Check(LAfter.DiagnosticCount = 0, 'A completed checkpoint retains valid routine syntax');
  for LAt := 0 to LBefore.DeclarationCount - 1 do
  begin
    LOld := LBefore.Declaration(LAt);
    LNew := LAfter.Declaration(LAt);
    Check((LOld.RoutineKind = LNew.RoutineKind) and (LOld.Flags = LNew.Flags),
      'Appended metadata preserves the exact typed routine role and STATIC bit');
  end;
end;

{$ifndef PAS2JS}
procedure NativeWitness;
var
  LText: TStringList;
  LAnswer: INyxPascalResolution;
  LSite: INyxPascalBindingSite;
  LAt: Integer;
  LFormal: INyxPascalFormalParameter;

  procedure Locate(const AExactText: TNyxText; ANameLength: Integer);
  begin
    LAt := Pos(AExactText, GText);
    Check(LAt > 0, 'The whole compiled unit retains the exact method expression');
    LAt := GWorld.ModuleAt(1).VirtualAt(0,
      NyxTextScalarCount(Copy(GText, 1, LAt - 1)) + ANameLength - 1) + 1;
    Check(LAt > 0, 'The native expression maps to its original source occurrence');
    LSite := GWorld.At(1, GWorld.ModuleAt(1).Syntax.ScopeAt(LAt), LAt);
  end;
begin
  Check(MethodReceiverProof = 42, 'The independently compiled whole Pascal unit executes to 42');
  LText := TStringList.Create;
  try
    LText.LoadFromFile('tests/method.receivers.fixture.pas');
    Build(TNyxText(LText.Text), Facts);
    Check(GWorld.ModuleAt(1).Syntax.DiagnosticCount = 0, 'The original compiled unit has no recovered declaration error');
    Locate('Self.Value := AValue', 4);
    LAnswer := GWorld.Lookup(LSite, NyxPascalBindingName('Self.Value'));
    Check((LAnswer.Status = pbsBound) and (LAnswer.SymbolAt(0).Declaration.Name = TNyxText('Value')),
      'Constructor binding uses the unchanged compiled source');
    Locate('Result.Value := Self.Value + 1', 6);
    LAnswer := GWorld.Lookup(LSite, NyxPascalBindingName('Result'));
    Check((LAnswer.Status = pbsBound) and
      (LAnswer.BoundType.Definition.Declaration.Name = TNyxText('TMethodBadge')),
      'The whole native nested procedure captures its outer function result');
    Locate('Self.Shared', 4);
    LAnswer := GWorld.Lookup(LSite, NyxPascalBindingName('Self.Shared'));
    Check((LAnswer.Status = pbsBound) and (LAnswer.SymbolAt(0).Declaration.Name = TNyxText('Shared')),
      'Class receiver binding uses the original compiled method body');
    Locate('Result := Self.Value;', 6);
    LAnswer := GWorld.Lookup(LSite, NyxPascalBindingName('Result'));
    Check((LAnswer.Status = pbsBound) and Supports(LAnswer.BoundType, INyxPascalFormalParameter, LFormal) and
      (LFormal.DeclarationOwner.Declaration.Name = TNyxText('TMethodBox')),
      'The whole native generic method keeps its declared parameter owner');
  finally
    LText.Free;
  end;
end;
{$endif}

begin
  try
    Contracts;
    UnfinishedAndRefused;
    CompletionPath;
    OwnershipAndWork;
    CheckpointRoles;
    {$ifndef PAS2JS}NativeWitness;{$endif}
    GWorld := nil;
    WriteLn('PASS ', GChecks, ' implicit routine receiver/result checks');
  except
    on E: Exception do
    begin
      GWorld := nil;
      WriteLn('FAIL ', GChecks, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
