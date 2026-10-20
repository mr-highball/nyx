{ Source string semantics are checked against complete independently compiled
  Delphi/DelphiUnicode witnesses. Portable queries inspect original inputs and
  detached values; they never link or execute those authored expressions. }
program nyx_pascal_string_expression_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses SysUtils, Classes, nyx.text, nyx.editing, nyx.scheduler, nyx.pascal.work,
  nyx.pascal.context, nyx.pascal.files, nyx.pascal.translation, nyx.pascal.lexing,
  nyx.pascal.syntax, nyx.pascal.syntax.translated, nyx.pascal.binding, nyx.pascal.values;

{$ifndef PAS2JS}
type
  { Each reader owns its query/budget and publishes an error after joining.
    Only immutable semantic inputs and value capabilities cross threads. }
  TStringExpressionReader = class(TThread)
  public
    World: INyxPascalSemanticWorld;
    Site: INyxPascalBindingSite;
    Window: TNyxPascalSpan;
    ErrorText: String;
    procedure Execute; override;
  end;
{$endif}

const
  CExpression = '''a''#0 + #256 + #$D800';
  CMode = '{$mode delphi}{$H+}{$codepage utf8}{$modeswitch implicitfunctionspecialization+}';

var GChecks: Integer; GInclude: TNyxText;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then raise Exception.Create(AReason);
end;

function Facts(AUnicode: Boolean = False): INyxPascalCompilerContext;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,TNyxPascalCompilerVersion.Create(3,3,1),ppmDelphi)
    .CompleteDefines(True);

  if AUnicode then Result := Result.WithSymbol(NyxPascalDefine('NYX_STRING_UNICODE'),ppdDefined);
end;

function Root: INyxPascalCompilerContext;
const CNames: array[0..8] of TNyxText = ('AnsiChar','WideChar','ShortString','AnsiString',
  'UTF8String','UnicodeString','WideString','Boolean','LongInt');
  CSizes: array[0..8] of Integer = (1,2,256,4,4,4,4,1,4);
var LAt: Integer;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,TNyxPascalCompilerVersion.Create(3,3,1),
    ppmFPC,pcsoSystemBootstrap).CompleteDefines(True).WithOption('H',ppkFalse);
  for LAt := 0 to High(CNames) do
    Result := NyxPascalWithCapturedSize(Result,NyxPascalSizeReference(CNames[LAt]),
      NyxPascalSizeFact(pssKnown,ppmFPC,ppkFalse,CSizes[LAt]));
end;

function View(const AName, AText: TNyxText;
  const AContext: INyxPascalCompilerContext): INyxPascalTranslatedSyntax;
begin
  Result := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile(AName),AText)
    .WithFile(NyxPascalPhysicalFile('string.expression.cases.inc'),GInclude),
    NyxPascalPhysicalFile(AName),AContext,TNyxPascalTranslationOptions.Defaults),
    TNyxPascalSyntaxOptions.Defaults);
end;

function WorldFor(const ASource: TNyxText; AUnicode: Boolean = False;
  ASteps: Integer = 1000000): INyxPascalSemanticWorld;
var LContext: INyxPascalCompilerContext;
begin
  LContext := NyxPascalWithBootstrapContext(Facts(AUnicode),Root);
  Result := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults
    .WithImplicitScopes(pispCompilerUnits).UpToSteps(ASteps))
    .WithModule(View('system.pas','unit System;interface implementation end.',LContext))
    .WithModule(View('objpas.pas','unit ObjPas;interface implementation end.',LContext))
    .WithModule(View('source.pas',ASource,LContext))
    .WithModule(View('sysutils.pas','unit SysUtils;interface implementation end.',LContext))
    .WithModule(View('typinfo.pas','unit TypInfo;interface implementation end.',LContext));
end;

function Companion(const AExpression: TNyxText; const ASettings: TNyxText = ''): TNyxText;
begin
  Result := 'unit U;'+CMode+ASettings+'interface '+
    'const Prefix=''Hello'';Letter=''A'';Caption=Prefix+'', world'';'+
    'Typed:UnicodeString=''Stored'';function Keep<T>(const AValue:T):T;'+
    'var A:AnsiString;W:UnicodeString;I:LongInt;Small:String[3];implementation procedure Run;begin Observe('+
    AExpression+');end;end.';
end;

function Site(const AWorld: INyxPascalSemanticWorld; const AText: TNyxText;
  AStart, AFinish, AOccurrence: Integer; out AWindow: TNyxPascalSpan): INyxPascalBindingSite;
var LAt, LFirst, LLast, LCaret, LPhysical, LEnd: Integer;
  LView: INyxPascalTranslatedSyntax; LOrigin: TNyxPascalSyntaxOrigin;
begin
  LPhysical := NyxTextScalarCount(Copy(AText,1,AStart-1));
  LEnd := LPhysical+NyxTextScalarCount(Copy(AText,AStart,AFinish-AStart));
  LView := AWorld.ModuleAt(2); LFirst := -1; LLast := -1; LCaret := -1;
  for LAt := 0 to LView.OriginCount-1 do
  begin
    LOrigin := LView.Origin(LAt);

    if (LOrigin.Occurrence <> AOccurrence) or (LOrigin.PhysicalSpan.Start < LPhysical) then Continue;

    if LOrigin.PhysicalSpan.Finish <= LEnd then
    begin

      if LFirst < 0 then LFirst := LOrigin.VirtualSpan.Start;
      LLast := LOrigin.VirtualSpan.Finish;
    end
    else begin LCaret := LOrigin.VirtualSpan.Start; Break; end;
  end;
  Check((LFirst >= 0) and (LLast > LFirst) and (LCaret >= LLast),
    'The exact physical occurrence owns the complete original code-token union');
  AWindow := NyxPascalSpan(LFirst,LLast-LFirst);
  Result := AWorld.At(2,LView.Syntax.ScopeAt(LFirst),LCaret);
end;

function ExpressionSite(const AWorld: INyxPascalSemanticWorld;
  const ASource, AExpression: TNyxText; out AWindow: TNyxPascalSpan): INyxPascalBindingSite;
var LAt: Integer;
begin
  LAt := Pos('Observe(',ASource);
  Check(LAt > 0,'The whole source keeps its unique query marker'); Inc(LAt,8);
  Check(Copy(ASource,LAt,Length(AExpression)) = AExpression,'The expression text stays byte-exact');
  Result := Site(AWorld,ASource,LAt,LAt+Length(AExpression),0,AWindow);
end;

function QuerySource(const ASource, AExpression: TNyxText): INyxPascalResolution;
var LWorld: INyxPascalSemanticWorld; LQueries: INyxPascalExpressionQueries;
  LSite: INyxPascalBindingSite; LWindow: TNyxPascalSpan;
  LAccess: TNyxPascalMemberAccess;
begin
  LWorld := WorldFor(ASource);
  LSite := ExpressionSite(LWorld,ASource,AExpression,LWindow);
  Supports(LWorld,INyxPascalExpressionQueries,LQueries);
  Result := LQueries.ExpressionType(LSite,LWindow,LAccess);
end;

function Query(const AExpression: TNyxText; const ASettings: TNyxText = ''): INyxPascalResolution;
begin
  Result := QuerySource(Companion(AExpression,ASettings),AExpression);
end;

procedure ControlledContract;
var LAnswer: INyxPascalResolution; LString: INyxPascalStringConstant; LSource: TNyxText;
  LLiteral: INyxPascalLiteralType; LScalar: INyxPascalConstantExpression;
  LRejected: Boolean;
begin
  LAnswer := Query(CExpression);
  Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType.Builtin = pbbUnicodeString) and
    Supports(LAnswer.BoundType,INyxPascalStringConstant,LString),'A complete constant string expression carries typed units');
  Check((LString.UnitCount = 4) and LString.Wide and (LString.Encoding = psceUTF16) and
    (LString.CodeUnit(0) = 97) and (LString.CodeUnit(1) = 0) and
    (LString.CodeUnit(2) = 256) and (LString.CodeUnit(3) = $d800),
    'Embedded NUL and lone surrogate units survive exact concatenation');
  Check(not Supports(LAnswer.BoundType,INyxPascalLiteralType,LLiteral),
    'A folded expression does not pretend to be an original source literal');
  LRejected := False;
  try LString.CodeUnit(-1); except on ERangeError do LRejected := True; end;
  Check(LRejected,'Published unit indexing checks its lower boundary');
  LRejected := False;
  try LString.CodeUnit(LString.UnitCount); except on ERangeError do LRejected := True; end;
  Check(LRejected,'Published unit indexing checks its upper boundary');
  LAnswer := Query('''a''#0 < ''a''#1');
  Check((LAnswer.Status = pbsBound) and Supports(LAnswer.BoundType,INyxPascalConstantExpression,LScalar) and
    LScalar.ConstantValue.BooleanResult,'Comparison is length-aware and includes NUL');
  LAnswer := Query('A + ''!''');
  Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType.Builtin = pbbAnsiString) and
    not Supports(LAnswer.BoundType,INyxPascalStringConstant,LString),'Runtime data retains type without fabricated text');
  LAnswer := Query('Keep(''ab'' + ''cd'')');
  Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType.Builtin = pbbAnsiString) and
    not Supports(LAnswer.BoundType,INyxPascalStringConstant,LString),'Generic returns inherit type without argument units');
  LAnswer := Query('Caption + ''!''');
  Check((LAnswer.Status = pbsBound) and Supports(LAnswer.BoundType,INyxPascalStringConstant,LString) and
    (LString.UnitCount = 13),'Named source constants retain their complete original initializer');
  LAnswer := Query('Typed + ''!''');
  Check((LAnswer.Status = pbsBound) and not Supports(LAnswer.BoundType,INyxPascalStringConstant,LString),
    'Typed constant storage supplies no immutable-value proof');
  LAnswer := Query('''ab'' - ''cd''');
  Check(LAnswer.Status <> pbsBound,'Unsupported string operators do not acquire invented arithmetic');
  LAnswer := Query('''ab'' + I');
  Check((LAnswer.Status <> pbsBound) and (LAnswer.Issue = pbiUnsupportedType),
    'A resolved integer does not silently become a character or formatted text');
  LSource := StringReplace(String(Companion('Caption + ''!''')),'procedure Run;begin',
    'procedure Run;var Prefix:AnsiString;begin',[]);
  LAnswer := QuerySource(LSource,'Caption + ''!''');
  Check((LAnswer.Status = pbsBound) and Supports(LAnswer.BoundType,INyxPascalStringConstant,LString) and
    (LString.UnitCount = 13),'A use-site local shadow cannot rebind the declaration initializer');
  LSource := StringReplace(String(Companion('Prefix + ''!''')),'procedure Run;begin',
    'procedure Run;var Prefix:AnsiString;begin',[]);
  LAnswer := QuerySource(LSource,'Prefix + ''!''');
  Check((LAnswer.Status = pbsBound) and not Supports(LAnswer.BoundType,INyxPascalStringConstant,LString),
    'An ordinary local variable shadows a global constant without inheriting its value');
  LSource := StringReplace(String(Companion('Cycle')),'const Prefix=',
    'const Cycle=Cycle+''x'';Prefix=',[]);
  LAnswer := QuerySource(LSource,'Cycle');
  Check((LAnswer.Status <> pbsBound) and (LAnswer.Issue = pbiRecursiveType),
    'Recursive declaration strings refuse without a partial concatenation');
  LAnswer := Query('Small + ''!''');
  Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType.Kind = pstBuiltin) and
    (LAnswer.BoundType.Builtin = pbbShortString),
    'ShortString concatenation returns the canonical 255-character type, not a bounded operand');
  LAnswer := Query('Letter');
  Check((LAnswer.Status = pbsBound) and Supports(LAnswer.BoundType,INyxPascalStringConstant,LString) and
    (LString.Form = pscfCharacter) and (LString.CodeUnit(0) = 65),
    'A named character has detached ordinal-unit evidence');
  Check(Supports(LAnswer.BoundType,INyxPascalConstantExpression,LScalar) and
    (LScalar.ConstantValue.Kind = pvvUnknown),
    'The original scalar expression interface retains its integer/Boolean-only contract');
end;

procedure WorkContract;
var LSource: TNyxText; LWorld: INyxPascalSemanticWorld; LSite: INyxPascalBindingSite;
  LWindow: TNyxPascalSpan; LQueries: INyxPascalExpressionQueries; LAccess: TNyxPascalMemberAccess;
  LAnswer: INyxPascalResolution; LValue: INyxPascalStringConstant;
  LBudget: INyxPascalAnalysisBudget; LAt, LFit: Integer; LCancel: INyxCancellationScope; LRefused: Boolean;
begin
  LSource := Companion(CExpression); LWorld := WorldFor(LSource);
  LSite := ExpressionSite(LWorld,LSource,CExpression,LWindow);
  Supports(LWorld,INyxPascalExpressionQueries,LQueries); LFit := -1;
  for LAt := 1 to 20000 do
  begin
    LAnswer := nil; LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      try
        LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess,LBudget);
        Check((LAnswer.Status = pbsBound) and Supports(LAnswer.BoundType,INyxPascalStringConstant,LValue) and
          (LValue.UnitCount = 4),'The first sufficient allowance publishes complete detached text');
        LFit := LAt;
      except
        on ENyxPascalAnalysisWorkLimit do Check((LAnswer = nil) and (LBudget.Receipt.Total <= LAt),
          'Every smaller allowance refuses atomically within its unchanged parent budget');
      end;
    finally LBudget.Retire; end;

    if LFit > 0 then Break;
  end;
  Check((LFit > 1) and (LValue.CodeUnit(3) = $d800),'Values survive work-budget retirement');
  WriteLn('string expression work / first fit=',LFit,' every smaller allowance refused');
  LCancel := NewNyxCancellationScope; LCancel.Cancel; LAnswer := nil; LRefused := False;
  try LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess,LCancel);
  except on ENyxPascalAnalysisCancelled do LRefused := True; end;
  Check(LRefused and (LAnswer = nil),'Plain cancellation publishes no string answer');
  LWorld := WorldFor(LSource,False,20);
  LSite := ExpressionSite(LWorld,LSource,CExpression,LWindow);
  Supports(LWorld,INyxPascalExpressionQueries,LQueries); LAnswer := nil; LRefused := False;
  try LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess);
  except on ENyxPascalBindingLimit do LRefused := True; end;
  Check(LRefused and (LAnswer = nil),'The local semantic ceiling still refuses without an aggregate scope');
end;

{$ifndef PAS2JS}
function Load(const APath: String): TNyxText;
var LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath,fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Result,LStream.Size);

    if Length(Result) > 0 then LStream.ReadBuffer(Result[1],Length(Result));
  finally LStream.Free; end;
end;

function TypeName(const AType: INyxPascalSemanticType): TNyxText;
var LType: INyxPascalSemanticType; LExpression: INyxPascalConstantExpression;
begin
  LType := AType;

  if LType = nil then Exit('');

  if Supports(LType,INyxPascalConstantExpression,LExpression) then LType := LExpression.ResultType;

  if (LType.Kind in [pstAlias,pstDistinct]) and (LType.Definition <> nil) then Exit(LType.Definition.Declaration.Name);
  case LType.Builtin of
    pbbAnsiString: Result := 'AnsiString'; pbbUTF8String: Result := 'UTF8String';
    pbbUnicodeString: Result := 'UnicodeString'; pbbWideString: Result := 'WideString';
    pbbShortString: Result := 'ShortString'; pbbBoolean: Result := 'Boolean';
  else Result := '';
  end;
end;

procedure NativeContract;
var LSource, LMarker, LDescription: TNyxText;
  LRows, LFields: TStringList; LMode, LRow, LAt, LStart, LEnd, LComma: Integer;
  LWorld: INyxPascalSemanticWorld; LQueries: INyxPascalExpressionQueries;
  LSite: INyxPascalBindingSite; LWindow: TNyxPascalSpan; LAnswer: INyxPascalResolution;
  LAccess: TNyxPascalMemberAccess; LString: INyxPascalStringConstant; LScalar: INyxPascalConstantExpression;
begin
  Check(ParamCount = 2,'Supply both independently compiled whole native witness outputs');
  LSource := Load('tests/strings.expressions.fixture.pas'); GInclude := Load('tests/string.expression.cases.inc');
  LRows := TStringList.Create; LFields := TStringList.Create;
  try
    LFields.Delimiter := '|'; LFields.StrictDelimiter := True;
    for LMode := 0 to 1 do
    begin
      Check(Load(ParamStr(LMode+1)+'.capacity') = '4|256',
        'The actual compiler distinguishes String[3] storage from its full ShortString result');
      LWorld := WorldFor(LSource,LMode = 1);
      Check(LWorld.ModuleAt(2).Source.Resolved and (LWorld.ModuleAt(2).Source.OccurrenceCount = 3),
        'The complete source retains both original precision-independent include occurrences');
      Supports(LWorld,INyxPascalExpressionQueries,LQueries);
      LRows.Text := String(Load(ParamStr(LMode+1)));
      Check(LRows.Count = 108,'The actual native compiler output retains every whole-source expression');
      for LRow := 0 to LRows.Count-1 do
      begin
        LFields.DelimitedText := LRows[LRow]; Check(LFields.Count = 5,'Native output has a complete typed observation');
        LMarker := 'Emit('''+TNyxText(LFields[1])+''',';
        LAt := Pos(LMarker,GInclude); Check(LAt > 0,'The original include retains its unique expression marker');
        LComma := 0;
        while LComma < 2 do
        begin

          if GInclude[LAt] = ',' then Inc(LComma);
          Inc(LAt);
        end;
        while GInclude[LAt] in [#9,#10,#13,' '] do Inc(LAt);
        LStart := LAt; LEnd := LAt;
        while GInclude[LEnd] <> ';' do Inc(LEnd);
        Dec(LEnd);
        LSite := Site(LWorld,GInclude,LStart,LEnd,LRow div 54+1,LWindow);
        LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess);
        WriteLn('native string expression / mode=',LMode,' ',LFields[0],'/',LFields[1],
          ' status=',Ord(LAnswer.Status),' issue=',Ord(LAnswer.Issue),' type=',String(TypeName(LAnswer.BoundType)));
        Check((LAnswer.Status = pbsBound) and (TypeName(LAnswer.BoundType) = TNyxText(LFields[3])),
          'The original expression retains its actual independently inferred native type');

        if LFields[2] = '0' then
          Check(not Supports(LAnswer.BoundType,INyxPascalStringConstant,LString) and
            (not Supports(LAnswer.BoundType,INyxPascalConstantExpression,LScalar) or
              (LScalar.ConstantValue.Kind = pvvUnknown)),'Runtime/generic/storage results carry no fabricated value')
        else if LFields[3] = 'Boolean' then
          Check(Supports(LAnswer.BoundType,INyxPascalConstantExpression,LScalar) and
            (LScalar.ConstantValue.Kind = pvvBoolean) and
            ('relation,'+IntToStr(Ord(LScalar.ConstantValue.BooleanResult)) = LFields[4]),
            'The actual native string relation agrees, including embedded zero units')
        else
        begin
          Check(Supports(LAnswer.BoundType,INyxPascalStringConstant,LString),'A complete constant supplies code-unit evidence');
          LDescription := 'text';
          for LAt := 0 to LString.UnitCount-1 do LDescription := LDescription+','+TNyxText(IntToStr(LString.CodeUnit(LAt)));
          Check(LDescription = TNyxText(LFields[4]),'The actual native UTF-16 value agrees without normalization');
        end;
      end;
    end;
  finally LRows.Free; LFields.Free; GInclude := ''; end;
end;

procedure TStringExpressionReader.Execute;
var LQueries: INyxPascalExpressionQueries; LAnswer: INyxPascalResolution;
  LAccess: TNyxPascalMemberAccess; LValue: INyxPascalStringConstant; LBudget: INyxPascalAnalysisBudget;
begin
  LBudget := NewNyxPascalAnalysisBudget(20000);
  try
    try
      Supports(World,INyxPascalExpressionQueries,LQueries);
      LAnswer := LQueries.ExpressionType(Site,Window,LAccess,LBudget);

      if (LAnswer.Status <> pbsBound) or not Supports(LAnswer.BoundType,INyxPascalStringConstant,LValue) or
        (LValue.UnitCount <> 4) or (LValue.CodeUnit(3) <> $d800) then raise Exception.Create('Concurrent string value differs');
    except on E: Exception do ErrorText := E.Message; end;
  finally LBudget.Retire; end;
end;

procedure ConcurrentContract;
var LSource: TNyxText; LWorld: INyxPascalSemanticWorld; LSite: INyxPascalBindingSite;
  LWindow: TNyxPascalSpan; LReaders: array of TStringExpressionReader; LAt: Integer;
begin
  LSource := Companion(CExpression); LWorld := WorldFor(LSource);
  LSite := ExpressionSite(LWorld,LSource,CExpression,LWindow); SetLength(LReaders,64);
  try
    for LAt := 0 to High(LReaders) do
    begin
      LReaders[LAt] := TStringExpressionReader.Create(True); LReaders[LAt].FreeOnTerminate := False;
      LReaders[LAt].World := LWorld; LReaders[LAt].Site := LSite; LReaders[LAt].Window := LWindow; LReaders[LAt].Start;
    end;
    for LAt := 0 to High(LReaders) do
    begin LReaders[LAt].WaitFor; Check(LReaders[LAt].ErrorText = '',LReaders[LAt].ErrorText); end;
  finally
    for LAt := 0 to High(LReaders) do
    begin

      if LReaders[LAt] <> nil then begin LReaders[LAt].WaitFor; LReaders[LAt].Free; end;
    end;
  end;
  WriteLn('string expression readers / 64 joined');
end;
{$endif}

begin
  ControlledContract; WorkContract;
  {$ifndef PAS2JS}NativeContract; ConcurrentContract;{$endif}
  WriteLn('PASS string expressions ',GChecks,' checks');
end.
