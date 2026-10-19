{ The whole native compiler witness is independent of later semantic queries.
  Portable resolver checks are added around these exact authored expressions. }
program nyx_pascal_literal_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses SysUtils, Classes, nyx.text, nyx.editing, nyx.scheduler, nyx.pascal.work,
  nyx.pascal.files, nyx.pascal.context, nyx.pascal.lexing, nyx.pascal.translation,
  nyx.pascal.syntax, nyx.pascal.syntax.translated, nyx.pascal.binding,
  nyx.pascal.values, nyx.pascal.names, nyx.pascal.drafts, nyx.pascal.project, nyx.pascal.sources,
  nyx.pascal.completion, nyx.pascal.completion.owned
  {$ifndef PAS2JS}, literal.contracts.fixture{$endif};

{$ifndef PAS2JS}
type
  { Independent readers share only frozen source/world/site values. Each
    expression query owns its decoder, unit vector and generic frame. }
  TLiteralReader = class(TThread)
  public
    Shared: INyxPascalSemanticWorld;
    Site: INyxPascalBindingSite;
    Window: TNyxPascalSpan;
    ErrorText: String;
    procedure Execute; override;
  end;
{$endif}

const
  CMode = '{$mode delphi}{$H+}{$codepage utf8}{$modeswitch unicodestrings-}' +
    '{$modeswitch implicitfunctionspecialization+}';
  CUnicodeMode = '{$mode delphiunicode}{$H+}{$codepage utf8}{$modeswitch unicodestrings+}' +
    '{$modeswitch implicitfunctionspecialization+}';
  CPortable = 'unit literal.contracts.fixture;' + CMode +
    'interface type TLiteralBox<T>=class Value:T;end;'+
    'function EchoLiteral<T>(const AValue:T):T;function BoxLiteral<T>(const AValue:T):TLiteralBox<T>;implementation end.';

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
  Result := NewNyxPascalCompilerContext(ppcFPC,TNyxPascalCompilerVersion.Create(3,3,1),ppmDelphi)
    .CompleteDefines(True);
  {$ifdef NYX_LITERAL_UNICODE}
  Result := Result.WithSymbol(NyxPascalDefine('NYX_LITERAL_UNICODE'),ppdDefined);
  {$endif}
end;

function View(const AName, AText: TNyxText; const AContext: INyxPascalCompilerContext): INyxPascalTranslatedSyntax;
begin
  Result := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile(AName),AText),NyxPascalPhysicalFile(AName),AContext,
    TNyxPascalTranslationOptions.Defaults),TNyxPascalSyntaxOptions.Defaults);
end;

function WorldFor(const AExpression: TNyxText; const AMode: TNyxText = CMode;
  const AExtra: TNyxText = ''; const AContext: INyxPascalCompilerContext = nil): INyxPascalSemanticWorld;
var
  LContext: INyxPascalCompilerContext;
begin
  LContext := AContext;

  if LContext = nil then LContext := Facts;
  Result := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults)
    .WithModule(View('literal.contracts.fixture.pas',GFixture,LContext))
    .WithModule(View('consumer.pas','unit Consumer;'+AMode+
      'interface uses literal.contracts.fixture;'+AExtra+'implementation procedure Run;begin '+
      AExpression+';end;end.',LContext));
end;

function Site(const AWorld: INyxPascalSemanticWorld; const AExpression: TNyxText;
  out AWindow: TNyxPascalSpan): INyxPascalBindingSite;
var
  LView: INyxPascalTranslatedSyntax;
  LText: TNyxText;
  LAt, LStart, LFinish, LOriginAt, LPhysicalStart: Integer;
  LOrigin: TNyxPascalSyntaxOrigin;
begin
  LView := AWorld.ModuleAt(1);
  LText := NyxPascalOccurrenceLexical(LView.Source,0).Source;
  LAt := Pos('begin '+AExpression,LText);
  Check(LAt > 0,'The expression is present in its exact physical file');
  Inc(LAt,6);
  LStart := NyxTextScalarCount(Copy(LText,1,LAt-1));
  LPhysicalStart := LStart;
  LFinish := LStart + NyxTextScalarCount(AExpression);
  LStart := LView.VirtualAt(0,LStart);
  LFinish := LView.VirtualAt(0,LFinish);

  if LStart < 0 then
  begin
    { Nonlinear expansion origins deliberately have no linear VirtualAt.
      Select the first exact invocation row, retaining the real semicolon
      boundary for the complete expansion's analysis window. }
    for LOriginAt := 0 to LView.OriginCount - 1 do
    begin
      LOrigin := LView.Origin(LOriginAt);

      if (LOrigin.Occurrence = 0) and (LOrigin.PhysicalSpan.Start = LPhysicalStart) and
        (LOrigin.Expansion >= 0) then
      begin
        LStart := LOrigin.VirtualSpan.Start;
        Break;
      end;
    end;
  end;
  Check((LStart >= 0) and (LFinish > LStart),'The entire expression maps to retained source: '+String(AExpression));
  AWindow := NyxPascalSpan(LStart,LFinish-LStart);
  Result := AWorld.At(1,LView.Syntax.ScopeAt(LStart),LFinish);
end;

function Resolve(const AWorld: INyxPascalSemanticWorld; const AExpression: TNyxText): INyxPascalResolution;
var
  LSite: INyxPascalBindingSite;
  LWindow: TNyxPascalSpan;
  LQueries: INyxPascalExpressionQueries;
  LAccess: TNyxPascalMemberAccess;
begin
  LSite := Site(AWorld,AExpression,LWindow);
  Check(Supports(AWorld,INyxPascalExpressionQueries,LQueries),'Literal typing uses ordinary expression queries');
  Result := LQueries.ExpressionType(LSite,LWindow,LAccess);
end;

function BuiltinName(AValue: TNyxPascalBuiltin): TNyxText;
const
  CNames: array[TNyxPascalBuiltin] of TNyxText = ('','Integer','Byte','Word','SmallInt',
    'ShortInt','LongInt','LongWord','Int64','QWord','NativeInt','NativeUInt','Boolean',
    'ByteBool','WordBool','LongBool','Char','AnsiChar','WideChar','Single','Double',
    'Extended','Real','Currency','Pointer','AnsiString','UTF8String','UnicodeString',
    'WideString','ShortString','Comp');
begin
  Result := CNames[AValue];
end;

procedure Contract(const AExpression, AExpected: TNyxText; const AMode: TNyxText = CMode);
var
  LAnswer: INyxPascalResolution;
  LLiteral: INyxPascalLiteralType;
begin
  LAnswer := Resolve(WorldFor('EchoLiteral('+AExpression+')',AMode),'EchoLiteral('+AExpression+')');
  Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType <> nil),
    'Literal deduction binds '+String(AExpression)+' / '+String(LAnswer.MessageText));
  Check(BuiltinName(LAnswer.BoundType.Builtin) = AExpected,
    'Native witness type for '+String(AExpression)+' / got '+String(BuiltinName(LAnswer.BoundType.Builtin)));
  Check(not Supports(LAnswer.BoundType,INyxPascalLiteralType,LLiteral),
    'A function result type does not claim the argument literal value');
end;

procedure Refusal(const AExpression: TNyxText; const AMode: TNyxText = CMode;
  const AContext: INyxPascalCompilerContext = nil);
var
  LAnswer: INyxPascalResolution;
begin
  LAnswer := Resolve(WorldFor(AExpression,AMode,'',AContext),AExpression);
  Check((LAnswer.Status <> pbsBound) and (LAnswer.BoundType = nil),
    'Unsupported expression publishes no guessed literal type: '+String(AExpression));
end;

procedure RawContracts;
var
  LWorld: INyxPascalSemanticWorld;
  LAnswer: INyxPascalResolution;
  LLiteral: INyxPascalLiteralType;
  LUnits: TNyxText;
begin
  LWorld := WorldFor('18446744073709551615');
  LAnswer := Resolve(LWorld,'18446744073709551615');
  Check(Supports(LAnswer.BoundType,INyxPascalLiteralType,LLiteral),'Exact integer retains optional source evidence');
  Check((LLiteral.LiteralKind = plkInteger) and
    (LLiteral.Value.Number.ToText = '18446744073709551615'),'Unsigned 64-bit value never passes through a browser Number');
  Check((LLiteral.ModuleIndex = 1) and (LLiteral.Context.Family = ppcFPC) and
    (LLiteral.Span.Length = 20),'Literal identity retains its source module, context and span');
  LAnswer := Resolve(WorldFor('-9223372036854775808'),'-9223372036854775808');
  Check(Supports(LAnswer.BoundType,INyxPascalLiteralType,LLiteral) and
    (LLiteral.Value.Number.ToText = '-9223372036854775808'),'Signed lower boundary is exact');
  LAnswer := Resolve(WorldFor('''a''#0''b'''),'''a''#0''b''');
  Check((LAnswer.Status = pbsBound) and Supports(LAnswer.BoundType,INyxPascalLiteralType,LLiteral),
    'Contiguous source fragments form one literal despite projection separators');
  Check((LAnswer.BoundType.Kind = pstArray) and (LAnswer.BoundType.Target.Builtin = pbbAnsiChar) and
    (LLiteral.UnitCount = 3) and (LLiteral.CodeUnit(0) = 97) and
    (LLiteral.CodeUnit(1) = 0) and (LLiteral.CodeUnit(2) = 98),
    'Raw constant array and embedded NUL stay distinct from specialization type');
  LAnswer := Resolve(WorldFor('''😀'''),'''😀''');
  Check(Supports(LAnswer.BoundType,INyxPascalLiteralType,LLiteral) and LLiteral.Wide and
    (LLiteral.UnitCount = 2) and (LLiteral.CodeUnit(0) = $d83d) and
    (LLiteral.CodeUnit(1) = $de00),'Astral source scalar retains its two UTF-16 units');
  LAnswer := Resolve(WorldFor('#$D800'),'#$D800');
  Check(Supports(LAnswer.BoundType,INyxPascalLiteralType,LLiteral) and LLiteral.Wide and
    (LLiteral.CodeUnit(0) = $d800),'A numeric surrogate stays an exact code unit');
  LUnits := '''a''''b''';
  LAnswer := Resolve(WorldFor(LUnits),LUnits);
  Check(Supports(LAnswer.BoundType,INyxPascalLiteralType,LLiteral) and
    (LLiteral.UnitCount = 3) and (LLiteral.CodeUnit(1) = 39),'Doubled quotes decode once');
  try
    LLiteral.CodeUnit(-1);
    Check(False,'Invalid code-unit access must refuse');
  except
    on ENyxPascalBinding do Check(True,'Invalid code-unit access refuses');
  end;
  LAnswer := Resolve(WorldFor('True',CMode,'var True:LongInt;'),'True');
  Check((LAnswer.Status = pbsBound) and not Supports(LAnswer.BoundType,INyxPascalLiteralType,LLiteral),
    'A visible True declaration retains ordinary lookup ownership');
  LAnswer := Resolve(WorldFor('42',CMode,'type ShortInt=class end;'),'42');
  Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType.Builtin = pbbShortInt),
    'A local primitive-spelling declaration cannot retag a compiler literal');
  LWorld := WorldFor('''é''',CMode+'{$i absent.inc}{$mode delphi}{$H+}');
  Check(not LWorld.ModuleAt(1).CompleteTranslation,
    'An unavailable include cannot manufacture a later literal projection or context');
  Refusal('''a'' ''b''');
  Refusal('18446744073709551616');
  Refusal('-9223372036854775809');
  Refusal('1.5');
  Refusal('nil');
  Refusal('42 + 1');
  Refusal('EchoLiteral(''ab'')','{$H+}{$modeswitch implicitfunctionspecialization+}');
  Refusal('42','',NewNyxPascalCompilerContext(ppcPas2JS,TNyxPascalCompilerVersion.Create(3,3,1),ppmDelphi));
  Contract('42','ShortInt');
  Contract('''ab''','AnsiString');
  Contract('''ab''','UnicodeString',CUnicodeMode);
  Contract('''ab''','ShortString',CMode+'{$H-}');
end;

procedure OwnershipAndWork;
const
  CExpression = 'EchoLiteral(''a''#0''😀''#256)';
var
  LWorld, LOther: INyxPascalSemanticWorld;
  LSite: INyxPascalBindingSite;
  LWindow: TNyxPascalSpan;
  LQueries: INyxPascalExpressionQueries;
  LAccess: TNyxPascalMemberAccess;
  LAnswer, LRetained: INyxPascalResolution;
  LBudget: INyxPascalAnalysisBudget;
  LCancel: INyxCancellationScope;
  LAt, LFit: Integer;
  LRefused: Boolean;
begin
  LWorld := WorldFor(CExpression);
  LSite := Site(LWorld,CExpression,LWindow);
  Supports(LWorld,INyxPascalExpressionQueries,LQueries);
  LFit := -1;
  for LAt := 1 to 10000 do
  begin
    LAnswer := nil;
    LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      try
        LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess,LBudget);
        Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType.Builtin = pbbUnicodeString),
          'First sufficient allowance publishes the entire literal-derived type');
        LRetained := LAnswer;
        LFit := LAt;
      except
        on ENyxPascalAnalysisWorkLimit do
          Check((LAnswer = nil) and (LBudget.Receipt.Total <= LAt),
            'Every smaller allowance refuses without a partial type or escaped decoder');
      end;
    finally
      LBudget.Retire;
    end;

    if LFit > 0 then Break;
  end;
  Check(LFit > 1,'Literal decoding and specialization have a finite positive first fit');
  WriteLn('literal work / first fit=',LFit,' every smaller allowance refused');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LAnswer := nil;
  LRefused := False;
  try
    LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess,LCancel);
  except
    on ENyxPascalAnalysisCancelled do LRefused := True;
  end;
  Check(LRefused and (LAnswer = nil),'Cancelled literal work publishes no result');
  LOther := WorldFor(CExpression);
  Supports(LOther,INyxPascalExpressionQueries,LQueries);
  LAnswer := nil;
  LRefused := False;
  try
    LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess);
  except
    on ENyxPascalBinding do LRefused := True;
  end;
  Check(LRefused and (LAnswer = nil),'An earlier literal site cannot be retagged to another world');
  LQueries := nil;
  LWorld := nil;
  LSite := nil;
  Check((LRetained.World <> nil) and (LRetained.BoundType.Builtin = pbbUnicodeString),
    'A retained result outlives retired execution and local source leases');
end;

procedure IncludedAndExpandedOrigins;
const
  CExpression = '''a''#0''b''';
var
  LFiles: INyxPascalFileCatalog;
  LView: INyxPascalTranslatedSyntax;
  LWorld: INyxPascalSemanticWorld;
  LQueries: INyxPascalExpressionQueries;
  LSite: INyxPascalBindingSite;
  LAnswer: INyxPascalResolution;
  LLiteral: INyxPascalLiteralType;
  LAccess: TNyxPascalMemberAccess;
  LStart, LFinish, LOrigin: Integer;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('consumer.pas'),'unit Consumer;'+CMode+
      'interface implementation procedure Run;begin {$i payload.inc}end;end.')
    .WithFile(NyxPascalPhysicalFile('payload.inc'),CExpression+';');
  LView := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFiles,
    NyxPascalPhysicalFile('consumer.pas'),Facts,TNyxPascalTranslationOptions.Defaults),
    TNyxPascalSyntaxOptions.Defaults);
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults)
    .WithModule(View('literal.contracts.fixture.pas',GFixture,Facts)).WithModule(LView);
  LStart := LView.VirtualAt(1,0);
  LFinish := LView.VirtualAt(1,NyxTextScalarCount(CExpression));
  LSite := LWorld.At(1,LView.Syntax.ScopeAt(LStart),LFinish);
  Supports(LWorld,INyxPascalExpressionQueries,LQueries);
  LAnswer := LQueries.ExpressionType(LSite,NyxPascalSpan(LStart,LFinish-LStart),LAccess);
  Check((LAnswer.Status = pbsBound) and Supports(LAnswer.BoundType,INyxPascalLiteralType,LLiteral),
    'Contiguous included fragments retain literal identity');
  LOrigin := LView.OriginAt(LLiteral.Span.Start);
  Check((LView.Origin(LOrigin).Occurrence = 1) and (LView.Origin(LOrigin).PhysicalSpan.Start = 0) and
    (LLiteral.CodeUnit(1) = 0),'Literal metadata maps to the exact included occurrence');
  LAnswer := Resolve(WorldFor('CAPTION',CMode+'{$macro on}{$define CAPTION := ''a''#0''b''}'),'CAPTION');
  Check((LAnswer.Status = pbsBound) and Supports(LAnswer.BoundType,INyxPascalLiteralType,LLiteral) and
    (LLiteral.UnitCount = 3) and (LLiteral.CodeUnit(1) = 0),'Macro fragments use effective adjacency, preserving their invocation origin');
end;

procedure CompletionPath;
var
  LFiles: INyxPascalFileCatalog;
  LRoots: array of TNyxPascalPhysicalFile;
  LProvider: INyxPascalCompletionProvider;
  LSources, LPreview: INyxPascalSourceSnapshot;
  LReply: INyxPascalCompletionResult;
  LOrigins: INyxOwnedPascalCompletionResult;
  LText: TNyxText;
  LCaret, LNewCaret: Integer;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('sdk/system.pas'),'unit System;interface type TObject=class end;implementation end.')
    .WithFile(NyxPascalPhysicalFile('sdk/literal.contracts.fixture.pas'),GFixture);
  SetLength(LRoots,2);
  LRoots[0] := NyxPascalPhysicalFile('sdk/system.pas');
  LRoots[1] := NyxPascalPhysicalFile('sdk/literal.contracts.fixture.pas');
  { This declared-source companion explicitly selects the small-model policy.
    It does not claim a complete default SDK or manufacture ObjPas exports. }
  LProvider := NewNyxOwnedPascalCompletionProvider(Facts,LFiles,LRoots,
    TNyxOwnedPascalCompletionOptions.Defaults.WithBinding(
      TNyxPascalBindingOptions.Defaults.WithImplicitScopes(pispDeclarationNames)));
  LText := 'unit Consumer;'+CMode+'interface uses literal.contracts.fixture;'+
    'implementation procedure Run;begin BoxLiteral(42).Val';
  LSources := CaptureNyxPascalSources(NewNyxPascalProject(NyxPascalUnit('Consumer'),LText));
  LCaret := NyxTextScalarCount(LText);
  LReply := LProvider.Complete(NewNyxPascalCompletionRequest(LSources).At(TNyxPascalSourceRef.Main,LCaret));
  Check((LReply.Status = pcsReady) and (LReply.Count = 1) and (LReply.Item(0).Name = 'Value'),
    'Ordinary completion follows the literal-inferred constructed result / '+String(LReply.MessageText));
  Supports(LReply,INyxOwnedPascalCompletionResult,LOrigins);
  LProvider := nil;
  Check((LOrigins.Definition(0).FileReference.Name = 'sdk/literal.contracts.fixture.pas') and
    (LOrigins.DefinitionSource(0).Slice(LOrigins.Definition(0).Span) = 'Value'),
    'Member navigation retains exact physical source after provider retirement');
  LPreview := LReply.Preview(0,LSources,LNewCaret);
  Check((LNewCaret = LCaret+2) and (LPreview.Text(TNyxPascalSourceRef.Main) = LText+'ue'),
    'Detached completion insertion changes only the authored member prefix');
end;

{$ifndef PAS2JS}
procedure TLiteralReader.Execute;
var
  LQueries: INyxPascalExpressionQueries;
  LAnswer: INyxPascalResolution;
  LLiteral: INyxPascalLiteralType;
  LAccess: TNyxPascalMemberAccess;
  LAt: Integer;
begin
  try
    Supports(Shared,INyxPascalExpressionQueries,LQueries);
    for LAt := 1 to 16 do
    begin
      LAnswer := LQueries.ExpressionType(Site,Window,LAccess);

      if (LAnswer.Status <> pbsBound) or not Supports(LAnswer.BoundType,INyxPascalLiteralType,LLiteral) or
        not LLiteral.Wide or (LLiteral.UnitCount <> 4) or (LLiteral.CodeUnit(1) <> $d83d) then
        raise Exception.Create('Concurrent literal decoder lost its exact source code units');
    end;
  except
    on E: Exception do ErrorText := E.Message;
  end;
end;

procedure Concurrent;
const
  CExpression = '''a''#128512#0';
var
  LWorld: INyxPascalSemanticWorld;
  LSite: INyxPascalBindingSite;
  LWindow: TNyxPascalSpan;
  LReaders: array[0..3] of TLiteralReader;
  LAt: Integer;
begin
  LWorld := WorldFor(CExpression);
  LSite := Site(LWorld,CExpression,LWindow);
  for LAt := 0 to High(LReaders) do
  begin
    LReaders[LAt] := TLiteralReader.Create(True);
    LReaders[LAt].Shared := LWorld;
    LReaders[LAt].Site := LSite;
    LReaders[LAt].Window := LWindow;
  end;
  try
    for LAt := 0 to High(LReaders) do LReaders[LAt].Start;
    for LAt := 0 to High(LReaders) do LReaders[LAt].WaitFor;
    for LAt := 0 to High(LReaders) do Check(LReaders[LAt].ErrorText = '',LReaders[LAt].ErrorText);
  finally
    for LAt := 0 to High(LReaders) do LReaders[LAt].Free;
  end;
  WriteLn('native concurrent literal decoders / 64 joined');
end;

procedure NativeWitness;
var
  LObserved: TStringList;
  LStream: TFileStream;
  LLineAt, LLineEnd, LLabelAt, LCallAt, LEndAt, LCompared: Integer;
  LLabel, LExpression, LExpected, LMode, LRest, LLine: TNyxText;
  LValue: UnicodeString;
  LAnswer: INyxPascalResolution;
  LLiteral: INyxPascalLiteralType;
  LUnitAt: Integer;
begin
  RunLiteralWitness;
  Check(NativeLiteralBoxValue = 42,'A native inferred constructed result executes to 42');
  Write(LiteralObservations);
  { Read bytes directly into UTF8String. Routing source through an ANSI
    TStringList would reinterpret non-ASCII bytes under the host codepage. }
  LStream := TFileStream.Create('tests/literal.contracts.fixture.pas',fmOpenRead or fmShareDenyWrite);
  try
    SetLength(GFixture,LStream.Size);

    if Length(GFixture) > 0 then LStream.ReadBuffer(GFixture[1],Length(GFixture));
  finally
    LStream.Free;
  end;
  LObserved := TStringList.Create;
  try
    LObserved.Text := String(LiteralObservations);
    LCompared := 0;
    LLineAt := 1;
    while LLineAt <= Length(GFixture) do
    begin
      LLineEnd := LLineAt;
      while (LLineEnd <= Length(GFixture)) and (GFixture[LLineEnd] <> #10) do Inc(LLineEnd);
      LLine := Copy(GFixture,LLineAt,LLineEnd-LLineAt);
      LLineAt := LLineEnd+1;
      LLabelAt := Pos('GLabel := ''',LLine);
      LCallAt := Pos('EchoLiteral(',LLine);

      if (LLabelAt = 0) or (LCallAt = 0) then Continue;
      LRest := Copy(LLine,LLabelAt+11,Length(LLine));
      LLabel := Copy(LRest,1,Pos('''',LRest)-1);
      LExpected := TNyxText(LObserved.Values[String(LLabel)]);

      if (LExpected = '') or (LLabel = 'real') then Continue;
      LEndAt := Pos(');',LLine);
      LExpression := Copy(LLine,LCallAt+12,LEndAt-LCallAt-12);
      LMode := CMode;
      {$ifdef NYX_LITERAL_UNICODE}LMode := CUnicodeMode;{$endif}

      if (LLabel = 'short-string') or (LLabel = 'short-wide-string') then LMode := LMode+'{$H-}';
      Contract(LExpression,LExpected,LMode);
      Inc(LCompared);
    end;
    Check((LCompared = 53) and (LiteralInvocations = 54),
      'Every qualified observation is compared, and source reads never execute the template');
    LValue := NativeLiteralCodeUnits;
    LAnswer := Resolve(WorldFor('#195#169#256#0#$D800'),'#195#169#256#0#$D800');
    Check(Supports(LAnswer.BoundType,INyxPascalLiteralType,LLiteral) and
      (LLiteral.UnitCount = Length(LValue)),'Compiler materialization agrees with the decoded unit count');
    for LUnitAt := 1 to Length(LValue) do
      Check(LLiteral.CodeUnit(LUnitAt-1) = Ord(LValue[LUnitAt]),
        'Compiler materialization agrees with every retained byte/wide/NUL/surrogate unit');
    WriteLn('native literal witness / invocations=',LiteralInvocations,' compared=',LCompared);
  finally
    LObserved.Free;
  end;
end;
{$endif}

begin
  try
    GFixture := CPortable;
    {$ifndef PAS2JS}NativeWitness;{$endif}
    RawContracts;
    IncludedAndExpandedOrigins;
    CompletionPath;
    OwnershipAndWork;
    {$ifndef PAS2JS}Concurrent;{$endif}
    WriteLn('PASS ',GChecks,' source-owned literal checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ',E.ClassName,': ',E.Message);
      ExitCode := 1;
    end;
  end;
end.
