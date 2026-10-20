{ Authored header effects are checked against whole independently compiled
  sources. Selected mode alone remains insufficient evidence of startup defaults;
  ignored late MODE directives retain their original token and exact pre-state. }
program nyx_pascal_precision_header_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses SysUtils, Classes, nyx.text, nyx.editing, nyx.scheduler, nyx.pascal.work,
  nyx.pascal.context, nyx.pascal.files, nyx.pascal.translation, nyx.pascal.lexing,
  nyx.pascal.syntax, nyx.pascal.syntax.translated, nyx.pascal.binding,
  nyx.pascal.constants {$ifndef PAS2JS}, precision.header.fixture{$endif};

var GChecks: Integer; GFixture, GIgnored: TNyxText;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then raise Exception.Create(AReason);
end;

function Facts: INyxPascalCompilerContext;
begin
  Result := NyxPascalWithConfiguredFPU(NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3,3,1),ppmDelphi).CompleteDefines(True)
    .WithSymbol(NyxPascalDefine('CPUI386'),ppdDefined)
    .WithSymbol(NyxPascalDefine('FPUX87'),ppdDefined),NyxPascalFPU('X87'));
end;

function Root: INyxPascalCompilerContext;
const CNames: array[0..2] of TNyxText = ('Single','Extended','LongInt');
  CSizes: array[0..2] of Integer = (4,10,4);
var LAt: Integer;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,TNyxPascalCompilerVersion.Create(3,3,1),
    ppmFPC,pcsoSystemBootstrap).CompleteDefines(True)
    .WithSymbol(NyxPascalDefine('CPUI386'),ppdDefined);
  for LAt := 0 to High(CNames) do
    Result := NyxPascalWithCapturedSize(Result,NyxPascalSizeReference(CNames[LAt]),
      NyxPascalSizeFact(pssKnown,ppmFPC,ppkFalse,CSizes[LAt]));
end;

function Translate(const AText: TNyxText;
  const AContext: INyxPascalCompilerContext = nil): INyxPascalTranslation;
var LContext: INyxPascalCompilerContext;
begin
  LContext := AContext;

  if LContext = nil then LContext := Facts;
  Result := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('header.pas'),AText),NyxPascalPhysicalFile('header.pas'),
    LContext,TNyxPascalTranslationOptions.Defaults);
end;

function Marker(const ASource: INyxPascalTranslation; const AName: TNyxText): Integer;
var LAt: Integer;
begin
  for LAt := 0 to ASource.Count-1 do
  begin

    if ASource.Text(LAt) = AName then Exit(LAt);
  end;
  raise Exception.Create('Missing source marker: '+String(AName));
end;

function PolicyAt(const ASource: INyxPascalTranslation; const AName: TNyxText): TNyxPascalCompilerPolicies;
begin
  Result := NyxPascalCompilerPolicies(ASource.State(ASource.Token(Marker(ASource,AName)).State));
end;

procedure ContextContract;
var LBase, LMode, LRequest, LCapture: INyxPascalCompilerContext;
  LPolicy: TNyxPascalCompilerPolicies;
begin
  LBase := Facts;
  Check(NyxPascalCompilerPolicies(LBase).ExcessPrecisionSetting = ppkUnknown,
    'Selected Delphi mode supplies no uncaptured startup default');
  LPolicy := NyxPascalCompilerPolicies(LBase).ApplyModeExcessPrecision(ppkTrue);
  LMode := NyxPascalWithCompilerPolicies(LBase,LPolicy);
  Check((LPolicy.ExcessPrecisionRequested = ppkUnknown) and
    (LPolicy.ModeExcessPrecision = ppkTrue) and (LPolicy.ExcessPrecisionSetting = ppkTrue),
    'A qualified mode effect is separate from an explicit request');
  LRequest := NyxPascalWithCompilerPolicies(LBase,
    TNyxPascalCompilerPolicies.Unknown.RequestExcessPrecision(ppkTrue));
  Check(not LMode.SameFacts(LRequest),'Equal values with different provenance retain distinct exact identities');
  LCapture := CaptureNyxPascalCompilerContext(LMode);
  Check(LCapture.SameFacts(LMode),'Capture retains mode-derived precision');
  LRequest := NyxPascalWithCompilerPolicies(LMode,LPolicy.RequestExcessPrecision(ppkFalse));
  Check((NyxPascalCompilerPolicies(LRequest).ModeExcessPrecision = ppkUnknown) and
    (NyxPascalCompilerPolicies(LRequest).ExcessPrecisionSetting = ppkFalse),
    'An explicit OFF supersedes the prior mode effect');
  Check(LPolicy.RequestExcessPrecision(ppkUnknown).ExcessPrecisionSetting = ppkUnknown,
    'Uncertain requests cannot fall back to an earlier mode default');
  LRequest := RestoreNyxPascalLocalSettings(LRequest,LMode);
  Check(LRequest.SameFacts(LMode),'Local restore recovers the saved value and its provenance');
  Check(NyxPascalCompilerPolicies(LMode.UnknownState).ExcessPrecisionSetting = ppkUnknown,
    'Unknown effects revoke both precision channels');
  Check(NyxPascalCompilerPolicies(LMode.InMode(ppmDelphi)).ExcessPrecisionSetting = ppkUnknown,
    'InMode alone has no authored-header proof');
  Check(LCapture.SameFacts(LMode) and LBase.SameFacts(Facts),'Fluent updates leave earlier contexts unchanged');
end;

procedure SourceContract;
var LSource, LPrefix: INyxPascalTranslation; LPolicy: TNyxPascalCompilerPolicies;
  LAt, LIgnored: Integer; LFiles: INyxPascalFileCatalog;
begin
  LSource := Translate(GFixture);
  Check(LSource.Resolved,'The whole native header fixture translates without an unknown effect');
  LPolicy := PolicyAt(LSource,'HeaderDefault');
  Check((LPolicy.ExcessPrecisionRequested = ppkUnknown) and
    (LPolicy.ModeExcessPrecision = ppkTrue),'Header MODE supersedes the earlier explicit OFF');
  LPolicy := PolicyAt(LSource,'ExplicitOff');
  Check((LPolicy.ExcessPrecisionRequested = ppkFalse) and
    (LPolicy.ModeExcessPrecision = ppkUnknown),'Later explicit settings keep their authored provenance');
  LPolicy := PolicyAt(LSource,'RestoredHeader');
  Check((LPolicy.ExcessPrecisionRequested = ppkUnknown) and
    (LPolicy.ModeExcessPrecision = ppkTrue),'POP restores the saved mode effect');
  LPrefix := NewNyxPascalTranslationPrefix(LSource,Marker(LSource,'HeaderDefault'),
    TNyxPascalTranslationOptions.Defaults);
  Check(NyxPascalCompilerPolicies(LPrefix.State(LPrefix.StateCount-1)).ModeExcessPrecision = ppkTrue,
    'Prefix capture cannot inherit a later explicit request');
  LSource := Translate(GIgnored); LIgnored := 0;
  Check(LSource.Resolved,'Known ignored global switches retain a resolved source timeline');
  for LAt := 0 to LSource.Count-2 do
  begin

    if LSource.Text(LAt) <> '{$mode objfpc}' then Continue;
    Inc(LIgnored);
    Check(LSource.Token(LAt).State = LSource.Token(LAt+1).State,
      'Ignored MODE retains its physical occurrence and exact unchanged state');
  end;
  Check((LIgnored = 2) and (LSource.State(LSource.StateCount-1).Mode = ppmDelphi),
    'Late modes neither retarget the dialect nor trip the header-only repeat rule');
  LSource := Translate('unit U;interface var V:Single;{$mode delphi}Marker implementation end.');
  Check(LSource.Resolved and (PolicyAt(LSource,'Marker').ExcessPrecisionSetting = ppkUnknown),
    'A first but late MODE cannot invent a default');
  LSource := Translate('{$mode delphi}{$mode delphi}unit U;interface implementation end.');
  Check(not LSource.Resolved,'A genuinely repeated legal header MODE still refuses');
  LSource := Translate('{$ifdef ABSENT}{$mode delphi}{$endif}Marker');
  Check(LSource.Resolved and (PolicyAt(LSource,'Marker').ExcessPrecisionSetting = ppkUnknown),
    'An inactive header MODE has no setting effect');
  LSource := Translate('{$if MAYBE}{$mode delphi}{$endif}Marker');
  Check(not LSource.Resolved and (PolicyAt(LSource,'Marker').ExcessPrecisionSetting = ppkUnknown),
    'Unknown conditional participation supplies no mode default');
  LSource := Translate('{$mode delphi}Marker',NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3,2,2),ppmDelphi).CompleteDefines(True));
  Check(PolicyAt(LSource,'Marker').ExcessPrecisionSetting = ppkUnknown,
    'Another compiler version cannot inherit this qualified mode effect');
  LSource := Translate('{$mode delphi}Marker',NewNyxPascalCompilerContext(ppcPas2JS,
    TNyxPascalCompilerVersion.Create(3,3,1),ppmDelphi).CompleteDefines(True));
  Check(PolicyAt(LSource,'Marker').ExcessPrecisionSetting = ppkUnknown,
    'Another compiler family cannot inherit the FPC default');
  LSource := Translate('{$mode delphi}{$unknownpolicy}Marker');
  Check(not LSource.Resolved and (PolicyAt(LSource,'Marker').ExcessPrecisionSetting = ppkUnknown),
    'An unknown source effect revokes the authored mode default');
  LSource := Translate('{$mode delphi}{$if MAYBE}{$excessprecision off}{$endif}Marker');
  Check(not LSource.Resolved and (PolicyAt(LSource,'Marker').ExcessPrecisionSetting = ppkUnknown),
    'Uncertain later requests cannot revive an earlier header setting');
  LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('unit.pas'),
    'unit U;{$include mode.inc}interface Marker implementation end.')
    .WithFile(NyxPascalPhysicalFile('mode.inc'),'{$mode delphi}');
  LSource := NewNyxPascalTranslation(LFiles,NyxPascalPhysicalFile('unit.pas'),Facts,
    TNyxPascalTranslationOptions.Defaults);
  Check(LSource.Resolved and (PolicyAt(LSource,'Marker').ModeExcessPrecision = ppkTrue),
    'Captured includes share the actual module header boundary');
end;

function View(const AName, ASource: TNyxText): INyxPascalTranslatedSyntax;
begin
  Result := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile(AName),ASource),NyxPascalPhysicalFile(AName),
    NyxPascalWithBootstrapContext(Facts,Root),TNyxPascalTranslationOptions.Defaults),
    TNyxPascalSyntaxOptions.Defaults);
end;

procedure ExpressionContract(const ASource, ALabel, AExpression, AExpected: TNyxText);
var LWorld: INyxPascalSemanticWorld; LView: INyxPascalTranslatedSyntax;
  LAt, LStart, LFinish, LPhysicalStart, LPhysicalFinish, LCaret: Integer;
  LOrigin: TNyxPascalSyntaxOrigin; LQueries: INyxPascalExpressionQueries;
  LAnswer: INyxPascalResolution; LAccess: TNyxPascalMemberAccess;
begin
  LView := View('header.pas',ASource);
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults.WithImplicitScopes(pispCompilerUnits))
    .WithModule(View('system.pas','unit System;interface implementation end.'))
    .WithModule(View('objpas.pas','unit ObjPas;interface implementation end.')).WithModule(LView)
    { The native witness imports RTTI for its independent observation body.
      These queries resolve only its numeric operands; no RTTI member is inferred. }
    .WithModule(View('typinfo.pas','unit TypInfo;interface implementation end.'));
  LAt := Pos('Emit('''+ALabel+''', Observe(',ASource);
  Check(LAt > 0,'The original source contains the independent observation label');
  Inc(LAt,Length('Emit('''+ALabel+''', Observe('));
  Check(Copy(ASource,LAt,Length(AExpression)) = AExpression,'The complete original expression stays exact');
  LPhysicalStart := NyxTextScalarCount(Copy(ASource,1,LAt-1));
  LPhysicalFinish := LPhysicalStart+NyxTextScalarCount(AExpression);
  LStart := -1; LFinish := -1; LCaret := -1;
  for LAt := 0 to LView.OriginCount-1 do
  begin
    LOrigin := LView.Origin(LAt);

    if (LOrigin.Occurrence <> 0) or (LOrigin.PhysicalSpan.Start < LPhysicalStart) then Continue;

    if LOrigin.PhysicalSpan.Finish <= LPhysicalFinish then
    begin

      if LStart < 0 then LStart := LOrigin.VirtualSpan.Start;
      LFinish := LOrigin.VirtualSpan.Finish;
    end
    else
    begin
      LCaret := LOrigin.VirtualSpan.Start;
      Break;
    end;
  end;
  Check((LStart >= 0) and (LFinish > LStart) and (LCaret >= LFinish),
    'Captured original code tokens own the query window and following site');
  Supports(LWorld,INyxPascalExpressionQueries,LQueries);
  LAnswer := LQueries.ExpressionType(LWorld.At(2,LView.Syntax.ScopeAt(LStart),LCaret),
    NyxPascalSpan(LStart,LFinish-LStart),LAccess);
  WriteLn('header expression / ',String(ALabel),' status=',Ord(LAnswer.Status),
    ' issue=',Ord(LAnswer.Issue),' related=',String(LAnswer.RelatedName));
  Check((LAnswer.Status = pbsBound) and (LAnswer.BoundType <> nil) and
    (((AExpected = 'Single') and (LAnswer.BoundType.Builtin = pbbSingle)) or
    ((AExpected = 'Extended') and (LAnswer.BoundType.Builtin = pbbExtended))),
    'The original expression agrees with native inferred type: '+String(ALabel));
end;

function StorageIdentity(AModeEffect: Boolean): TNyxText;
var LContext: INyxPascalCompilerContext; LPolicy: TNyxPascalCompilerPolicies;
  LFiles: INyxPascalFileCatalog; LView, LSystem: INyxPascalTranslatedSyntax;
  LWorld: INyxPascalSemanticWorld; LResult: INyxPascalConstantResult;
  LIdentity: INyxPascalConstantStorageIdentity;
begin
  LPolicy := TNyxPascalCompilerPolicies.Unknown;

  if AModeEffect then LPolicy := LPolicy.ApplyModeExcessPrecision(ppkTrue)
  else LPolicy := LPolicy.RequestExcessPrecision(ppkTrue);
  LContext := NyxPascalWithCapturedSize(NyxPascalWithCompilerPolicies(Facts.WithOption('H',ppkTrue),
    LPolicy),NyxPascalSizeReference('Byte'),NyxPascalSizeFact(pssKnown,ppmDelphi,ppkTrue,1));
  LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('layout.pas'),
    'unit Layout;interface {$packrecords 1}type TPacket=record Value:Byte;end;implementation end.')
    .WithFile(NyxPascalPhysicalFile('system.pas'),'unit System;interface implementation end.');
  LView := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFiles,NyxPascalPhysicalFile('layout.pas'),
    LContext,TNyxPascalTranslationOptions.Defaults),TNyxPascalSyntaxOptions.Defaults);
  LSystem := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFiles,NyxPascalPhysicalFile('system.pas'),
    LContext,TNyxPascalTranslationOptions.Defaults),TNyxPascalSyntaxOptions.Defaults);
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LView).WithModule(LSystem);
  LResult := PlanNyxPascalConstant('sizeof(TPacket)',TNyxPascalConstantOptions.Defaults)
    .Evaluate(LWorld.At(0,0,LView.Syntax.Lexical.Scalars),TNyxPascalConstantOptions.Defaults);
  Check(Supports(LResult,INyxPascalConstantStorageIdentity,LIdentity),
    'The shape retains exact context identity with either precision provenance');
  Result := LIdentity.StorageKey.Value;
end;

procedure StorageContract;
var LMode, LRequest: TNyxText;
begin
  LMode := StorageIdentity(True); LRequest := StorageIdentity(False);
  Check((LMode <> LRequest) and (Pos('mode-excess-precision-v1',LMode) > 0) and
    (Pos('excess-precision-request-v1',LMode) = 0) and
    (Pos('mode-excess-precision-v1',LRequest) = 0),
    'Tagged mode effects cannot collide with explicit precision requests');
end;

procedure WorkContract;
const CText = 'unit U;{$mode delphi}interface var V:Single;{$mode objfpc}Marker implementation end.';
var LFiles: INyxPascalFileCatalog; LSource, LRetained: INyxPascalTranslation;
  LBudget: INyxPascalAnalysisBudget; LCancel: INyxCancellationScope;
  LAt, LFit: Integer; LRejected: Boolean;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('budget.pas'),CText);
  LFit := -1;
  for LAt := 1 to 20000 do
  begin
    LSource := nil; LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      try
        LSource := NewNyxPascalTranslation(LFiles,NyxPascalPhysicalFile('budget.pas'),Facts,
          TNyxPascalTranslationOptions.Defaults,LBudget);
        Check(LSource.Resolved and (PolicyAt(LSource,'Marker').ModeExcessPrecision = ppkTrue),
          'First sufficient allowance publishes both complete header proofs');
        LFit := LAt; LRetained := LSource;
      except
        on ENyxPascalAnalysisWorkLimit do
          Check((LSource = nil) and (LBudget.Receipt.Total <= LAt),
            'Every smaller allowance refuses atomically within its admitted work');
      end;
    finally LBudget.Retire; end;

    if LFit > 0 then Break;
  end;
  Check((LFit > 1) and (PolicyAt(LRetained,'Marker').ExcessPrecisionSetting = ppkTrue),
    'Published header history remains usable after its execution budget retires');
  WriteLn('header precision work / first fit=',LFit,' every smaller allowance refused');
  LSource := nil; LCancel := NewNyxCancellationScope; LCancel.Cancel; LRejected := False;
  try
    LSource := NewNyxPascalTranslation(LFiles,NyxPascalPhysicalFile('budget.pas'),Facts,
      TNyxPascalTranslationOptions.Defaults,LCancel);
  except
    on ENyxPascalAnalysisCancelled do LRejected := True;
  end;
  Check(LRejected and (LSource = nil),'Cancellation publishes no partial mode effect');
  LSource := nil; LRejected := False;
  try
    LSource := NewNyxPascalTranslation(LFiles,NyxPascalPhysicalFile('budget.pas'),Facts,
      TNyxPascalTranslationOptions.Defaults.UpToStates(1));
  except
    on ENyxPascalTranslationLimit do LRejected := True;
  end;
  Check(LRejected and (LSource = nil),'State quotas remain atomic with the added provenance');
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

procedure NativeWitness;
var LText: TNyxText;
begin
  LText := TNyxText(NativeHeaderPrecision); Write(LText);
  Check((NativeHeaderPrecisionCalls = 6) and (Pos('header-add=Extended'+#10,LText) > 0) and
    (Pos('header-negative=Extended'+#10,LText) > 0) and (Pos('header-positive=Single'+#10,LText) > 0) and
    (Pos('header-mixed=Single'+#10,LText) > 0) and (Pos('explicit-off=Single'+#10,LText) > 0) and
    (Pos('restored-header=Extended'+#10,LText) > 0),'The whole independent native header witness must match');
  Check(ParamCount = 2,'The independent ignored-MODE compiler and native logs are required');
  LText := Load(ParamStr(1));
  Check((Pos('Misplaced global compiler switch, ignored',LText) > 0) and
    (Pos('2 warning(s) issued',LText) > 0),'The separate compiler oracle retains both expected warnings');
  LText := Load(ParamStr(2));
  Check((Pos('ignored-late-mode=Single',LText) > 0) and (Pos('ignored-pending-mode=Single',LText) > 0) and
    (Pos('after-pending=Extended',LText) > 0) and (Pos('0 unfreed memory blocks : 0',LText) > 0),
    'Ignored MODE neither changes dialect nor flushes pending precision');
  GFixture := Load('tests/precision.header.fixture.pas');
  GIgnored := Load('tests/precision.header.ignored.fixture.lpr');
end;
{$endif}

begin
  try
    { Browser consumers compile controlled semantic companions only. They do
      not execute the native witnesses or infer pas2js floating defaults. }
    GFixture := '{$excessprecision off}unit U;{$mode delphi}interface var F32:Single;I32:LongInt;'+
      'implementation procedure HeaderDefault;begin Emit(''header-add'', Observe(F32 + F32));'+
      'Emit(''header-negative'', Observe(-F32));Emit(''header-positive'', Observe(+F32));'+
      'Emit(''header-mixed'', Observe(I32 + F32));end;{$push}{$excessprecision off}'+
      'procedure ExplicitOff;begin Emit(''explicit-off'', Observe(F32 + F32));end;{$pop}'+
      'procedure RestoredHeader;begin Emit(''restored-header'', Observe(F32 + F32));end;end.';
    GIgnored := 'program P;{$mode delphi}{$excessprecision off}var F32:Single;begin '+
      'Emit(''ignored-late-mode'', Observe(F32 + F32 {$mode objfpc}));'+
      'Emit(''ignored-pending-mode'', Observe(F32 + F32 {$excessprecision on}{$mode objfpc}));'+
      'Emit(''after-pending'', Observe(F32 + F32));end.';
    {$ifndef PAS2JS}NativeWitness;{$endif}
    ContextContract;
    SourceContract;
    StorageContract;
    WorkContract;
    ExpressionContract(GFixture,'header-add','F32 + F32','Extended');
    ExpressionContract(GFixture,'header-negative','-F32','Extended');
    ExpressionContract(GFixture,'header-positive','+F32','Single');
    ExpressionContract(GFixture,'header-mixed','I32 + F32','Single');
    ExpressionContract(GFixture,'explicit-off','F32 + F32','Single');
    ExpressionContract(GFixture,'restored-header','F32 + F32','Extended');
    ExpressionContract(GIgnored,'ignored-late-mode','F32 + F32 {$mode objfpc}','Single');
    ExpressionContract(GIgnored,'ignored-pending-mode','F32 + F32 {$excessprecision on}{$mode objfpc}','Single');
    ExpressionContract(GIgnored,'after-pending','F32 + F32','Extended');
    {$ifndef PAS2JS}Check(NativeHeaderPrecisionCalls = 6,'Source queries invoke no native body');{$endif}
    WriteLn('PASS ',GChecks,' authored header precision checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ',E.ClassName,': ',E.Message);
      ExitCode := 1;
    end;
  end;
end.
