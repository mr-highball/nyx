{ Source signatures retain authored parameter/default ownership independently
  of routine execution, argument inference and compiler overload selection. }
program nyx_pascal_signature_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses SysUtils, Classes, nyx.text, nyx.scheduler, nyx.pascal.work, nyx.pascal.names,
  nyx.pascal.files, nyx.pascal.context, nyx.pascal.lexing, nyx.pascal.translation,
  nyx.pascal.syntax, nyx.pascal.syntax.translated, nyx.pascal.binding
  {$ifndef PAS2JS}, signature.contracts.fixture{$endif};

{$ifndef PAS2JS}
type
  { Readers share immutable input only; each signature query owns its vectors
    and work state, and all threads join before main observes their outcomes. }
  TSignatureReader = class(TThread)
  public
    Shared: INyxPascalSemanticWorld;
    Routine: INyxPascalBoundSymbol;
    ErrorText: String;
    procedure Execute; override;
  end;
{$endif}

const
  CPortable: TNyxText = 'unit signature.contracts.fixture; interface type ' +
    'TSignatureStep=function(AValue:LongInt):LongInt; TSignatureBadge=class ' +
    'constructor Create(AValue:LongInt=42); function Combine(const ALeft,ARight:LongInt;AExtra:LongInt=0):LongInt;' +
    'function Call(AValue:LongInt;AStep:TSignatureStep=nil):LongInt; ' +
    'class procedure Touch(var AValue;const AInput;out AResult:LongInt;constref AOffset:LongInt);static;end;' +
    'TSignatureBox<T>=class function Replace(const AValue:T;ACommit:Boolean=True):T;end;' +
    'function Grouped(AFirst,ASecond:LongInt):LongInt;' +
    'function Describe(const AText:AnsiString=''A;B)'';AMarker:LongInt=7):LongInt;' +
    'function Identity<T>(const AValue:T):T;implementation end.';

var
  GChecks: Integer;
  GFixture: TNyxText;

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

function View(const AName, AText: TNyxText): INyxPascalTranslatedSyntax;
begin
  Result := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(
    NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile(AName), AText),
    NyxPascalPhysicalFile(AName), Facts, TNyxPascalTranslationOptions.Defaults),
    TNyxPascalSyntaxOptions.Defaults);
end;

function WorldFor(const AText: TNyxText; ACandidates: Integer = 50): INyxPascalSemanticWorld;
begin
  Result := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults.UpToCandidates(ACandidates))
    .WithModule(View('system.pas', 'unit System;interface type TObject=class end;implementation end.'))
    .WithModule(View('signature.pas', AText));
end;

function Declaration(const AWorld: INyxPascalSemanticWorld; const AName: TNyxText;
  AKind: TNyxPascalSyntaxDeclarationKind = psdkRoutine): INyxPascalBoundSymbol;
var
  LAt: Integer;
  LDecl: TNyxPascalSyntaxDeclaration;
begin
  Result := nil;
  for LAt := 0 to AWorld.ModuleAt(1).Syntax.DeclarationCount - 1 do
  begin
    LDecl := AWorld.ModuleAt(1).Syntax.Declaration(LAt);

    if (LDecl.Kind = AKind) and (LDecl.Name = AName) then
      Exit(AWorld.Symbol(1, LAt));
  end;
  raise Exception.Create('Missing exact source declaration: ' + String(AName));
end;

function QuerySignature(const AWorld: INyxPascalSemanticWorld;
  const ARoutine: INyxPascalBoundSymbol; const ACancel: INyxCancellationScope = nil): INyxPascalResolution;
var
  LQueries: INyxPascalRoutineQueries;
begin
  Check(Supports(AWorld, INyxPascalRoutineQueries, LQueries), 'World exposes optional source signature queries');
  Result := LQueries.RoutineSignature(ARoutine, ACancel);
end;

function BoundSignature(const AWorld: INyxPascalSemanticWorld; const AName: TNyxText;
  ACount, ARequired: Integer): INyxPascalSemanticType;
var
  LAnswer: INyxPascalResolution;
  LSource: INyxPascalDeclaredSignature;
begin
  LAnswer := QuerySignature(AWorld, Declaration(AWorld, AName));
  Check(LAnswer.Status = pbsBound, 'Signature binds: ' + String(AName) + ' issue=' + IntToStr(Ord(LAnswer.Issue)));
  Result := LAnswer.BoundType;
  Check((Result <> nil) and (Result.ParameterCount = ACount), 'Complete parameter order/count: ' + String(AName));
  Check(Supports(Result, INyxPascalDeclaredSignature, LSource) and
    (LSource.RequiredParameterCount = ARequired), 'Required/trailing-default count: ' + String(AName));
end;

function Terminal(const AType: INyxPascalSemanticType): INyxPascalSemanticType;
begin
  Result := AType;
  while (Result <> nil) and (Result.Kind in [pstAlias, pstDistinct]) do Result := Result.Target;
end;

procedure Contracts;
var
  LWorld: INyxPascalSemanticWorld;
  LType, LCallback: INyxPascalSemanticType;
  LSource: INyxPascalDeclaredSignature;
  LFormal: INyxPascalFormalParameter;
  LAt: Integer;
  LParameter: INyxPascalBoundSymbol;
begin
  LWorld := WorldFor(GFixture);
  LType := BoundSignature(LWorld, 'Combine', 3, 2);
  Check((LType.Kind = pstFunction) and (Terminal(LType.Target).Builtin = pbbLongInt), 'Function result remains its declared type');
  Check((LType.ParameterName(0) = 'ALeft') and (LType.ParameterName(1) = 'ARight'), 'Grouped names preserve authored order');
  Check((LType.ParameterMode(0) = pspmConst) and (LType.ParameterMode(1) = pspmConst), 'Grouped modes remain independent entries');
  Supports(LType, INyxPascalDeclaredSignature, LSource);
  LParameter := LSource.ParameterDeclaration(2);
  Check((psdfDefaultArgument in LParameter.Declaration.Flags) and
    (LWorld.ModuleAt(1).Syntax.Lexical.Slice(LParameter.Declaration.ExpressionSpan) = '0'), 'Default expression is an exact source span');
  Check(LWorld.ModuleAt(1).Source.Files.FileAt(LParameter.Origin.FileIndex).Name = 'signature.pas',
    'Parameters retain their physical declaration file');
  LType := BoundSignature(LWorld, 'Create', 1, 0);
  Check((LType.Kind = pstProcedure) and (LType.Target = nil), 'Constructor signature has no fabricated declared return type');
  LType := BoundSignature(LWorld, 'Touch', 4, 4);
  Check((LType.Kind = pstProcedure) and (LType.ParameterType(0).Kind = pstUntyped) and
    (LType.ParameterType(1).Kind = pstUntyped), 'Untyped var/const parameters remain explicitly untyped');
  Check((LType.ParameterMode(0) = pspmVar) and (LType.ParameterMode(1) = pspmConst) and
    (LType.ParameterMode(2) = pspmOut) and (LType.ParameterMode(3) = pspmConstRef), 'Every authored passing mode is retained');
  LType := BoundSignature(LWorld, 'Call', 2, 1);
  LCallback := Terminal(LType.ParameterType(1));
  Check((LCallback.Kind = pstFunction) and (LCallback.ParameterCount = 1) and
    (Terminal(LCallback.Target).Builtin = pbbLongInt), 'Callback aliases retain their complete nested signature');
  LType := BoundSignature(LWorld, 'Describe', 2, 0);
  Supports(LType, INyxPascalDeclaredSignature, LSource);
  Check(LWorld.ModuleAt(1).Syntax.Lexical.Slice(LSource.ParameterDeclaration(0).Declaration.ExpressionSpan) = '''A;B)''', 'Delimiters inside strings never split defaults');
  LType := BoundSignature(LWorld, 'Grouped', 2, 2);
  Check(LType.ParameterName(1) = 'ASecond', 'Global grouped parameters retain their second name');
  LType := BoundSignature(LWorld, 'Identity', 1, 1);
  Check(Supports(LType.Target, INyxPascalFormalParameter, LFormal), 'Unapplied generic result retains its formal identity');
  Check((LFormal.Name = 'T') and (LFormal.DeclarationOwner.Declaration.Name = 'Identity'), 'Routine formal retains its original owner');
  Check(Supports(LType.ParameterType(0), INyxPascalFormalParameter, LFormal), 'Generic argument retains a formal rather than an invented concrete type');
  for LAt := 0 to LType.ParameterCount - 1 do
  begin
    Supports(LType, INyxPascalDeclaredSignature, LSource);
    Check(LSource.ParameterDeclaration(LAt).World = LWorld, 'Parameter symbols retain the exact world');
  end;
end;

procedure ReceiversIncludesAndLimits;
const
  CInclude: TNyxText = 'procedure IncludedWork(const AText:AnsiString=''Hello'';AValue:LongInt=42);';
var
  LWorld: INyxPascalSemanticWorld;
  LAnswer, LValue: INyxPascalResolution;
  LSource: INyxPascalDeclaredSignature;
  LView: INyxPascalTranslatedSyntax;
  LFiles: INyxPascalFileCatalog;
  LOrigin: TNyxPascalSyntaxOrigin;
  LParameter: INyxPascalBoundSymbol;
  LType: INyxPascalSemanticType;
  LRefused: Boolean;
  LAt, LSeen: Integer;
begin
  LWorld := WorldFor(GFixture).WithModule(View('use.pas', 'unit Consumer;interface uses ' +
    'signature.contracts.fixture;var Box:TSignatureBox<TSignatureBadge>;implementation end.'));
  LAnswer := LWorld.Lookup(LWorld.At(2, 0, LWorld.ModuleAt(2).Syntax.Lexical.Scalars), NyxPascalBindingName('Box'));
  Check((LAnswer.Status = pbsBound) and (LAnswer.Count = 1), 'The caller supplies an actual constructed receiver');
  LValue := LWorld.TypeOfSymbol(LAnswer.SymbolAt(0));
  LAnswer := LWorld.Member(LValue.BoundType, LWorld.At(2, 0,
    LWorld.ModuleAt(2).Syntax.Lexical.Scalars), NyxPascalBindingName('Replace'), pmaInstance);
  Check((LAnswer.Status = pbsBound) and (LAnswer.Count = 1), 'The original receiver method remains independently owned');
  LValue := QuerySignature(LWorld, LAnswer.SymbolAt(0));
  Check(LValue.Status = pbsBound, 'Constructed receiver signature binds');
  LType := Terminal(LValue.BoundType.Target);
  Check((LType.Kind = pstDeclared) and (LType.Definition.Declaration.Name = 'TSignatureBadge'), 'Receiver frame substitutes the return type');
  LType := Terminal(LValue.BoundType.ParameterType(0));
  Check((LType.Kind = pstDeclared) and (LType.Definition.Declaration.Name = 'TSignatureBadge'), 'The same frame substitutes the parameter type');
  Supports(LValue.BoundType, INyxPascalDeclaredSignature, LSource);
  Check((LSource.RequiredParameterCount = 1) and
    (psdfDefaultArgument in LSource.ParameterDeclaration(1).Declaration.Flags), 'Receiver substitution retains the independent Boolean default');

  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('included.pas'), 'unit Included;interface {$i signature.inc} implementation end.')
    .WithFile(NyxPascalPhysicalFile('signature.inc'), CInclude);
  LView := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFiles,
    NyxPascalPhysicalFile('included.pas'), Facts, TNyxPascalTranslationOptions.Defaults), TNyxPascalSyntaxOptions.Defaults);
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults)
    .WithModule(View('system.pas', 'unit System;interface implementation end.')).WithModule(LView);
  LType := BoundSignature(LWorld, 'IncludedWork', 2, 0);
  Supports(LType, INyxPascalDeclaredSignature, LSource);
  LParameter := LSource.ParameterDeclaration(0);
  LOrigin := LView.Part(LView.OriginAt(LParameter.Declaration.ExpressionSpan.Start), LParameter.Declaration.ExpressionSpan);
  Check(LView.Source.Files.FileAt(LOrigin.FileIndex).Name = 'signature.inc', 'Default navigation retains its physical include');
  Check(Copy(LView.Source.Files.TextAt(LOrigin.FileIndex), LOrigin.PhysicalSpan.Start + 1,
    LOrigin.PhysicalSpan.Length) = '''Hello''', 'ASCII include navigation retains the exact original token');
  LWorld := nil;
  LView := nil;
  LFiles := nil;
  Check(LSource.ParameterDeclaration(0).World = LType.World, 'Retained signature parameters keep their original include world alive');
  LRefused := False;
  try
    LSource.ParameterDeclaration(2);
  except
    on ENyxPascalBinding do LRefused := True;
  end;
  Check(LRefused, 'Parameter declaration getters reject an out-of-range ordinal');

  LWorld := WorldFor(GFixture, 2);
  LAnswer := nil;
  LRefused := False;
  try
    LAnswer := QuerySignature(LWorld, Declaration(LWorld, 'Combine'));
  except
    on ENyxPascalBindingLimit do LRefused := True;
  end;
  Check(LRefused and (LAnswer = nil), 'Parameter candidate limits refuse the entire signature without truncation');
  LWorld := WorldFor('unit Overloads;interface function Pick(A:LongInt):LongInt;overload;' +
    'function Pick(A:LongInt;B:LongInt=0):LongInt;overload;implementation end.');
  LSeen := 0;
  for LAt := 0 to LWorld.ModuleAt(1).Syntax.DeclarationCount - 1 do
  begin

    if LWorld.ModuleAt(1).Syntax.Declaration(LAt).Name <> 'Pick' then Continue;
    LAnswer := QuerySignature(LWorld, LWorld.Symbol(1, LAt));
    Inc(LSeen);
    Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType.ParameterCount = LSeen), 'Each exact overload retains its own complete signature');
  end;
  Check(LSeen = 2, 'Both overload declarations were inspected without selecting a call');
end;

procedure RefusalsAndEditing;
const
  CPrefix: TNyxText = 'unit Editing;interface implementation ';
var
  LWorld, LOther: INyxPascalSemanticWorld;
  LAnswer, LRetained: INyxPascalResolution;
  LQueries: INyxPascalRoutineQueries;
  LSymbol: INyxPascalBoundSymbol;
  LCancel: INyxCancellationScope;
  LRefused: Boolean;
begin
  LWorld := WorldFor(CPrefix + 'function Draft(AValue:LongInt=42):LongInt;begin');
  LRetained := QuerySignature(LWorld, Declaration(LWorld, 'Draft'));
  Check((LRetained.Status = pbsBound) and (LRetained.BoundType.ParameterCount = 1), 'A complete header survives an unfinished body');
  LWorld := WorldFor(CPrefix + 'function Bad(AValue:LongInt=):LongInt;begin end;end.');
  LAnswer := QuerySignature(LWorld, Declaration(LWorld, 'Bad'));
  Check((LAnswer.Status <> pbsBound) and (LAnswer.BoundType = nil), 'An explicit missing default refuses without a partial signature');
  LWorld := WorldFor(CPrefix + 'procedure Bad(AValue:LongInt=1;ARequired:LongInt);begin end;end.');
  LAnswer := QuerySignature(LWorld, Declaration(LWorld, 'Bad'));
  Check((LAnswer.Status <> pbsBound) and (LAnswer.BoundType = nil), 'Nontrailing defaults cannot invent a required count');
  LWorld := WorldFor(CPrefix + 'procedure Bad(AValue);begin end;end.');
  LAnswer := QuerySignature(LWorld, Declaration(LWorld, 'Bad'));
  Check((LAnswer.Status <> pbsBound) and (LAnswer.BoundType = nil), 'Untyped value parameters cannot invent a type');
  LWorld := WorldFor(GFixture);
  LSymbol := Declaration(LWorld, 'Combine');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LAnswer := nil;
  LRefused := False;
  try
    LAnswer := QuerySignature(LWorld, LSymbol, LCancel);
  except
    on ENyxPascalAnalysisCancelled do LRefused := True;
  end;
  Check(LRefused and (LAnswer = nil), 'Cancelled queries publish no signature');
  LOther := WorldFor(GFixture);
  Supports(LOther, INyxPascalRoutineQueries, LQueries);
  LAnswer := nil;
  LRefused := False;
  try
    LAnswer := LQueries.RoutineSignature(LSymbol);
  except
    on ENyxPascalBinding do LRefused := True;
  end;
  Check(LRefused and (LAnswer = nil), 'Foreign-world routine symbols cannot be retagged');
  Check((LRetained.World <> LWorld) and (LRetained.BoundType.ParameterName(0) = 'AValue'), 'Retained signatures outlive their caller world references');
end;

procedure WorkAndSources;
var
  LWorld: INyxPascalSemanticWorld;
  LSymbol: INyxPascalBoundSymbol;
  LQueries: INyxPascalRoutineQueries;
  LAnswer: INyxPascalResolution;
  LBudget: INyxPascalAnalysisBudget;
  LAt, LFit: Integer;
begin
  LWorld := WorldFor(GFixture);
  LSymbol := Declaration(LWorld, 'Combine');
  Supports(LWorld, INyxPascalRoutineQueries, LQueries);
  LFit := -1;
  for LAt := 1 to 10000 do
  begin
    LAnswer := nil;
    LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      try
        LAnswer := LQueries.RoutineSignature(LSymbol, LBudget);
        Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType.ParameterCount = 3), 'First sufficient allowance captures the complete signature');
        LFit := LAt;
      except
        on ENyxPascalAnalysisWorkLimit do
          Check((LAnswer = nil) and (LBudget.Receipt.Total <= LAt), 'Every smaller allowance refuses atomically');
      end;
    finally
      LBudget.Retire;
    end;

    if LFit > 0 then Break;
  end;
  Check(LFit > 1, 'Signature work has a finite positive first fit');
  WriteLn('declared signature work / first fit=', LFit, ' every smaller allowance refused');
end;

{$ifndef PAS2JS}
procedure TSignatureReader.Execute;
var
  LQueries: INyxPascalRoutineQueries;
  LAnswer: INyxPascalResolution;
  LAt: Integer;
begin
  try

    if not Supports(Shared, INyxPascalRoutineQueries, LQueries) then
      raise Exception.Create('Missing concurrent signature capability');
    for LAt := 1 to 16 do
    begin
      LAnswer := LQueries.RoutineSignature(Routine);

      if (LAnswer.Status <> pbsBound) or (LAnswer.BoundType.ParameterCount <> 3) or
        (LAnswer.BoundType.ParameterName(2) <> 'AExtra') then
        raise Exception.Create('Concurrent signature lost its complete source contract');
    end;
  except
    on E: Exception do ErrorText := E.Message;
  end;
end;

procedure Concurrent;
var
  LWorld: INyxPascalSemanticWorld;
  LSymbol: INyxPascalBoundSymbol;
  LReaders: array[0..3] of TSignatureReader;
  LAt: Integer;
begin
  LWorld := WorldFor(GFixture);
  LSymbol := Declaration(LWorld, 'Combine');
  for LAt := 0 to High(LReaders) do
  begin
    LReaders[LAt] := TSignatureReader.Create(True);
    LReaders[LAt].Shared := LWorld;
    LReaders[LAt].Routine := LSymbol;
  end;
  try
    for LAt := 0 to High(LReaders) do LReaders[LAt].Start;
    for LAt := 0 to High(LReaders) do LReaders[LAt].WaitFor;
    for LAt := 0 to High(LReaders) do
      Check(LReaders[LAt].ErrorText = '', 'Concurrent signature reader: ' + LReaders[LAt].ErrorText);
  finally
    for LAt := 0 to High(LReaders) do LReaders[LAt].Free;
  end;
  WriteLn('native concurrent declared signatures / 64 joined');
end;
{$endif}

begin
  try
    GFixture := CPortable;
    {$ifndef PAS2JS}
    with TStringList.Create do
    try
      LoadFromFile('tests/signature.contracts.fixture.pas');
      GFixture := TNyxText(Text);
    finally
      Free;
    end;
    Check(SignatureFixtureValue = 42, 'The independent native compiler executes the whole handwritten fixture');
    Check(SignatureInvocations = 8, 'Native invocation count establishes an independent execution witness');
    WriteLn('native signature fixture / value=42 invocations=', SignatureInvocations);
    {$endif}
    Contracts;
    ReceiversIncludesAndLimits;
    RefusalsAndEditing;
    WorkAndSources;
    {$ifndef PAS2JS}
    Concurrent;
    Check(SignatureInvocations = 8, 'Signature inspection never invokes application code');
    {$endif}
    WriteLn('PASS ', GChecks, ' source-owned routine signature checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL / ', E.Message);
      {$ifndef PAS2JS}
      DumpExceptionBackTrace(Output);
      Halt(1);
      {$else}
      raise;
      {$endif}
    end;
  end;
end.
