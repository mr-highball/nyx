{ Pointer compatibility is checked against complete compiled Pascal inputs.
  The resolver reads their original declarations/includes; it never links the
  witness or executes an authored operator while answering an editor query. }
program nyx_pascal_pointer_identity_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses SysUtils, Classes, nyx.text, nyx.editing, nyx.scheduler, nyx.pascal.work,
  nyx.pascal.context, nyx.pascal.files, nyx.pascal.translation, nyx.pascal.lexing,
  nyx.pascal.syntax, nyx.pascal.syntax.translated, nyx.pascal.binding;

{$ifndef PAS2JS}
type
  { A reader owns its query and budget. Only immutable source/site handles are
    shared; the main thread joins every reader before inspecting its outcome. }
  TPointerReader = class(TThread)
  public
    World: INyxPascalSemanticWorld;
    Site: INyxPascalBindingSite;
    Window: TNyxPascalSpan;
    ErrorText: String;
    procedure Execute; override;
  end;
{$endif}

const
  CMode = '{$mode delphi}{$H+}{$codepage utf8}';
  CExpression = '@Value + Selector';

var GChecks: Integer; GInclude: TNyxText;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then raise Exception.Create(AReason);
end;

function Facts(AUnicode: Boolean = False; ATarget: Boolean = True): INyxPascalCompilerContext;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,TNyxPascalCompilerVersion.Create(3,3,1),ppmDelphi)
    .CompleteDefines(True);

  if ATarget then Result := Result.WithSymbol(NyxPascalDefine('CPUI386'),ppdDefined);

  if AUnicode then Result := Result.WithSymbol(NyxPascalDefine('NYX_POINTER_UNICODE'),ppdDefined);
end;

function View(const AName, AText: TNyxText;
  const AContext: INyxPascalCompilerContext): INyxPascalTranslatedSyntax;
begin
  Result := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile(AName),AText)
    .WithFile(NyxPascalPhysicalFile('pointer.identity.cases.inc'),GInclude),
    NyxPascalPhysicalFile(AName),AContext,TNyxPascalTranslationOptions.Defaults),
    TNyxPascalSyntaxOptions.Defaults);
end;

function WorldFor(const ASource: TNyxText; AUnicode: Boolean = False;
  ATarget: Boolean = True; ASteps: Integer = 100000;
  const ASymbol: TNyxText = ''): INyxPascalSemanticWorld;
var LContext: INyxPascalCompilerContext;
begin
  LContext := Facts(AUnicode,ATarget);

  if ASymbol <> '' then LContext := LContext.WithSymbol(NyxPascalDefine(ASymbol),ppdDefined);
  Result := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults.UpToSteps(ASteps))
    .WithModule(View('system.pas','unit System;interface implementation end.',LContext))
    .WithModule(View('objpas.pas','unit ObjPas;interface implementation end.',LContext))
    .WithModule(View('source.pas',ASource,LContext))
    .WithModule(View('sysutils.pas','unit SysUtils;interface implementation end.',LContext))
    .WithModule(View('typinfo.pas','unit TypInfo;interface implementation end.',LContext));
end;

function Companion(const AExpression: TNyxText; const AExtra: TNyxText = ''): TNyxText;
begin
  Result := 'unit U;'+CMode+'{$T+}interface type '+
    'TValue=record N:LongInt;end;PValue=^TValue;'+
    'TResult=record N:LongInt;end;TSelector=record '+
    'class operator +(const A:PValue;const B:TSelector):TResult;end;'+AExtra+
    'var Value:TValue;Selector:TSelector;implementation procedure Run;begin Observe('+
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
    'The complete physical expression retains its exact include occurrence');
  AWindow := NyxPascalSpan(LFirst,LLast-LFirst);
  Result := AWorld.At(2,LView.Syntax.ScopeAt(LFirst),LCaret);
end;

function ExpressionSite(const AWorld: INyxPascalSemanticWorld;
  const ASource, AExpression: TNyxText; out AWindow: TNyxPascalSpan): INyxPascalBindingSite;
var LAt: Integer;
begin
  LAt := Pos('Observe(',ASource);
  Check(LAt > 0,'The complete companion retains its unique query marker'); Inc(LAt,8);
  Result := Site(AWorld,ASource,LAt,LAt+Length(AExpression),0,AWindow);
end;

function TypeName(const AType: INyxPascalSemanticType): TNyxText;
begin
  Result := '';

  if (AType <> nil) and (AType.Definition <> nil) then Result := AType.Definition.Declaration.Name;
end;

procedure ControlledContract;
var LSource: TNyxText; LWorld: INyxPascalSemanticWorld; LQueries: INyxPascalExpressionQueries;
  LSite: INyxPascalBindingSite; LWindow: TNyxPascalSpan;
  LAnswer: INyxPascalResolution; LAccess: TNyxPascalMemberAccess;
begin
  LSource := Companion(CExpression); LWorld := WorldFor(LSource);
  LSite := ExpressionSite(LWorld,LSource,CExpression,LWindow);
  Supports(LWorld,INyxPascalExpressionQueries,LQueries);
  LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess);
  Check((LAnswer.Status = pbsBound) and (TypeName(LAnswer.BoundType) = 'TResult'),
    'A typed address matches a declared pointer through qualified pointee equality');
  LWorld := WorldFor(LSource,False,False);
  LSite := ExpressionSite(LWorld,LSource,CExpression,LWindow);
  Supports(LWorld,INyxPascalExpressionQueries,LQueries);
  LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess);
  Check((LAnswer.Status <> pbsBound) and (LAnswer.BoundType = nil),
    'Unknown target representation cannot grant a pointer conversion');
  LSource := Companion('@Other + Selector','TOther=record N:LongInt;end;var Other:TOther;');
  LWorld := WorldFor(LSource); LSite := ExpressionSite(LWorld,LSource,'@Other + Selector',LWindow);
  Supports(LWorld,INyxPascalExpressionQueries,LQueries);
  LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess);
  Check((LAnswer.Status <> pbsBound) and (LAnswer.BoundType = nil),
    'The same record storage shape supplies no pointer compatibility');
  LSource := StringReplace(String(Companion(CExpression)),'PValue=^TValue;',
    '{$undef CPUI386}PValue=^TValue;{$define CPUI386}',[]);
  LWorld := WorldFor(LSource); LSite := ExpressionSite(LWorld,LSource,CExpression,LWindow);
  Supports(LWorld,INyxPascalExpressionQueries,LQueries);
  LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess);
  Check((LAnswer.Status = pbsBound) and (TypeName(LAnswer.BoundType) = 'TResult'),
    'Masking a source CPU symbol does not change the initial compiler target');
  LSource := StringReplace(String(Companion(CExpression)),'interface type',
    '{$define CPUI386}interface type',[]);
  LWorld := WorldFor(LSource,False,False); LSite := ExpressionSite(LWorld,LSource,CExpression,LWindow);
  Supports(LWorld,INyxPascalExpressionQueries,LQueries);
  LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess);
  Check((LAnswer.Status <> pbsBound) and (LAnswer.BoundType = nil),
    'An authored CPU symbol cannot manufacture an absent initial target observation');
end;

procedure WorkContract;
var LSource: TNyxText; LWorld: INyxPascalSemanticWorld; LQueries: INyxPascalExpressionQueries;
  LSite: INyxPascalBindingSite; LWindow: TNyxPascalSpan; LAccess: TNyxPascalMemberAccess;
  LAnswer, LRetained: INyxPascalResolution; LBudget: INyxPascalAnalysisBudget;
  LAt, LFit: Integer; LCancel: INyxCancellationScope; LRefused: Boolean;
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
        Check((LAnswer.Status = pbsBound) and (TypeName(LAnswer.BoundType) = 'TResult'),
          'The first admitted query publishes the complete selected result');
        LRetained := LAnswer; LFit := LAt;
      except
        on ENyxPascalAnalysisWorkLimit do Check((LAnswer = nil) and (LBudget.Receipt.Total <= LAt),
          'Every smaller parent allowance refuses atomically within its budget');
      end;
    finally LBudget.Retire; end;

    if LFit > 0 then Break;
  end;
  Check((LFit > 1) and (TypeName(LRetained.BoundType) = 'TResult'),
    'The immutable answer survives retirement of the caller budget');
  WriteLn('pointer work / first fit=',LFit,' every smaller allowance refused');
  LCancel := NewNyxCancellationScope; LCancel.Cancel; LAnswer := nil; LRefused := False;
  try LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess,LCancel);
  except on ENyxPascalAnalysisCancelled do LRefused := True; end;
  Check(LRefused and (LAnswer = nil),'Cancelled pointer matching publishes no answer');
  LWorld := WorldFor(LSource,False,True,20);
  LSite := ExpressionSite(LWorld,LSource,CExpression,LWindow);
  Supports(LWorld,INyxPascalExpressionQueries,LQueries); LAnswer := nil; LRefused := False;
  try LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess);
  except on ENyxPascalBindingLimit do LRefused := True; end;
  Check(LRefused and (LAnswer = nil),'The local semantic ceiling still refuses atomically');
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

procedure NativeContract;
var LSource, LMarker: TNyxText; LRows, LFields: TStringList;
  LMode, LRow, LAt, LStart, LEnd: Integer;
  LWorld: INyxPascalSemanticWorld; LQueries: INyxPascalExpressionQueries;
  LSite: INyxPascalBindingSite; LWindow: TNyxPascalSpan; LAnswer: INyxPascalResolution;
  LAccess: TNyxPascalMemberAccess;
begin
  Check(ParamCount = 2,'Supply both whole independently compiled native outputs');
  LSource := Load('tests/pointers.identity.fixture.pas'); GInclude := Load('tests/pointer.identity.cases.inc');
  LRows := TStringList.Create; LFields := TStringList.Create;
  try
    LFields.Delimiter := '|'; LFields.StrictDelimiter := True;
    for LMode := 0 to 1 do
    begin
      LWorld := WorldFor(LSource,LMode = 1);
      Check(LWorld.ModuleAt(2).Source.Resolved and (LWorld.ModuleAt(2).Source.OccurrenceCount = 4),
        'The entire unit retains typed/untyped/masked original include occurrences');
      Supports(LWorld,INyxPascalExpressionQueries,LQueries);
      LRows.Text := String(Load(ParamStr(LMode+1)));
      Check(LRows.Count = 93,'Native output retains every actual selected overload');
      for LRow := 0 to LRows.Count-1 do
      begin
        LFields.DelimitedText := LRows[LRow]; Check(LFields.Count = 4,'Native selection output is complete');
        LMarker := 'Emit('''+TNyxText(LFields[1])+''',';
        LAt := Pos(LMarker,GInclude); Check(LAt > 0,'The original include retains the native expression');
        Inc(LAt,Length(LMarker));
        while GInclude[LAt] in [#9,#10,#13,' '] do Inc(LAt);
        LStart := LAt; LEnd := LAt;
        while GInclude[LEnd] <> ';' do Inc(LEnd);
        Dec(LEnd);
        LSite := Site(LWorld,GInclude,LStart,LEnd,LRow div 31+1,LWindow);
        LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess);
        WriteLn('native pointer / mode=',LMode,' ',LFields[0],'/',LFields[1],
          ' status=',Ord(LAnswer.Status),' issue=',Ord(LAnswer.Issue),' type=',String(TypeName(LAnswer.BoundType)));
        Check((LAnswer.Status = pbsBound) and (TypeName(LAnswer.BoundType) = TNyxText(LFields[2])),
          'The source expression retains its independently selected native result');
      end;
    end;
  finally LRows.Free; LFields.Free; GInclude := ''; end;
end;

procedure NominalContract;
const CLabels: array[0..1] of TNyxText = ('generic-widget','generic-other');
  CArguments: array[0..1] of TNyxText = ('TWidget','TOtherWidget');
var LSource, LMarker: TNyxText; LCase, LStart, LEnd: Integer;
  LWorld: INyxPascalSemanticWorld; LQueries: INyxPascalExpressionQueries;
  LSite: INyxPascalBindingSite; LWindow: TNyxPascalSpan;
  LAnswer: INyxPascalResolution; LAccess: TNyxPascalMemberAccess;
  LType: INyxPascalSemanticType; LSubstituted: INyxPascalSubstitutedSymbol;
begin
  LSource := Load('tests/pointers.identity.fixture.pas'); GInclude := Load('tests/pointer.identity.cases.inc');
  try
    LWorld := WorldFor(LSource); Supports(LWorld,INyxPascalExpressionQueries,LQueries);
    for LCase := 0 to High(CLabels) do
    begin
      LMarker := 'Emit('''+CLabels[LCase]+''','; LStart := Pos(LMarker,GInclude);
      Check(LStart > 0,'The original pointer argument retains its exact source');
      Inc(LStart,Length(LMarker));
      while GInclude[LStart] in [#9,#10,#13,' '] do Inc(LStart);
      LEnd := LStart;
      while GInclude[LEnd] <> ' ' do Inc(LEnd);
      LSite := Site(LWorld,GInclude,LStart,LEnd,1,LWindow);
      LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess);
      Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType.Kind = pstAlias),
        'Pointer compatibility does not erase the declared source-facing alias');
      LType := LAnswer.BoundType;
      while LType.Kind = pstAlias do LType := LType.Target;
      Check((LType.Kind = pstPointer) and (LType.Target.Definition <> nil) and
        Supports(LType.Target.Definition,INyxPascalSubstitutedSymbol,LSubstituted) and
        (LSubstituted.Environment <> nil) and (LSubstituted.Environment.Count = 1) and
        (TypeName(LSubstituted.Environment.Argument(0)) = CArguments[LCase]),
        'The original generic argument remains exact despite compiler pointer equivalence');
    end;
    LWorld := nil; LQueries := nil; LSite := nil;
    Check(TypeName(LSubstituted.Environment.Argument(0)) = 'TOtherWidget',
      'Detached nominal provenance survives retirement of query-owner handles');
  finally GInclude := ''; end;
end;

procedure AmbiguityContract;
const CLabels: array[0..1] of TNyxText = ('ambiguous-separate','ambiguous-address');
  CSymbols: array[0..1] of TNyxText = ('NYX_POINTER_AMBIGUITY','NYX_POINTER_AMBIGUITY_ADDRESS');
var LSource, LMarker: TNyxText; LMode, LCase, LStart, LEnd: Integer;
  LWorld: INyxPascalSemanticWorld; LQueries: INyxPascalExpressionQueries;
  LSite: INyxPascalBindingSite; LWindow: TNyxPascalSpan;
  LAnswer: INyxPascalResolution; LAccess: TNyxPascalMemberAccess;
begin
  LSource := Load('tests/pointers.identity.fixture.pas'); GInclude := Load('tests/pointer.identity.cases.inc');
  try
    for LMode := 0 to 1 do
      for LCase := 0 to High(CLabels) do
      begin
        LWorld := WorldFor(LSource,LMode = 1,True,100000,CSymbols[LCase]);
        Check(LWorld.ModuleAt(2).Source.Resolved,'The independently refused native input translates completely');
        LMarker := 'Emit('''+CLabels[LCase]+''',';
        LStart := Pos(LMarker,LSource); Check(LStart > 0,'The complete native unit retains its ambiguity');
        Inc(LStart,Length(LMarker));
        while LSource[LStart] in [#9,#10,#13,' '] do Inc(LStart);
        LEnd := LStart;
        while LSource[LEnd] <> ';' do Inc(LEnd);
        Dec(LEnd); LSite := Site(LWorld,LSource,LStart,LEnd,0,LWindow);
        Supports(LWorld,INyxPascalExpressionQueries,LQueries);
        LAnswer := LQueries.ExpressionType(LSite,LWindow,LAccess);
        Check((LAnswer.Status = pbsUnresolved) and (LAnswer.Issue = pbiOverloadContext) and
          (LAnswer.BoundType = nil),'Equal-ranked unique pointer overloads refuse without a partial result');
      end;
  finally GInclude := ''; end;
end;

procedure TPointerReader.Execute;
var LQueries: INyxPascalExpressionQueries; LAnswer: INyxPascalResolution;
  LAccess: TNyxPascalMemberAccess; LBudget: INyxPascalAnalysisBudget;
begin
  LBudget := NewNyxPascalAnalysisBudget(20000);
  try
    try
      Supports(World,INyxPascalExpressionQueries,LQueries);
      LAnswer := LQueries.ExpressionType(Site,Window,LAccess,LBudget);

      if (LAnswer.Status <> pbsBound) or (TypeName(LAnswer.BoundType) <> 'TResult') then
        raise Exception.Create('Concurrent pointer selection differs');
    except on E: Exception do ErrorText := E.Message; end;
  finally LBudget.Retire; end;
end;

procedure ConcurrentContract;
var LSource: TNyxText; LWorld: INyxPascalSemanticWorld; LSite: INyxPascalBindingSite;
  LWindow: TNyxPascalSpan; LReaders: array of TPointerReader; LAt: Integer;
begin
  LSource := Companion(CExpression); LWorld := WorldFor(LSource);
  LSite := ExpressionSite(LWorld,LSource,CExpression,LWindow); SetLength(LReaders,64);
  try
    for LAt := 0 to High(LReaders) do
    begin
      LReaders[LAt] := TPointerReader.Create(True); LReaders[LAt].FreeOnTerminate := False;
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
  WriteLn('pointer readers / 64 joined');
end;
{$endif}

begin
  ControlledContract; WorkContract;
  {$ifndef PAS2JS}NativeContract; NominalContract; AmbiguityContract; ConcurrentContract;{$endif}
  WriteLn('PASS pointer identity ',GChecks,' checks');
end.
