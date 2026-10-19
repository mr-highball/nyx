{ Source FPU selection remains a distinct context namespace from configuration
  and from spoofable source defines. Native observations qualify actual rules. }
program nyx_pascal_source_fpu_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses SysUtils, Classes, nyx.text, nyx.pascal.context, nyx.pascal.files,
  nyx.pascal.translation, nyx.pascal.macros, nyx.pascal.work, nyx.scheduler,
  nyx.pascal.lexing, nyx.pascal.syntax, nyx.pascal.syntax.translated,
  nyx.pascal.binding, nyx.pascal.constants
  {$ifndef PAS2JS}, source.fpu.fixture{$endif};

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
  Result := NyxPascalWithConfiguredFPU(NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3,3,1),ppmDelphi).CompleteDefines(True)
    .WithSymbol(NyxPascalDefine('CPUI386'),ppdDefined)
    .WithSymbol(NyxPascalDefine('FPUX87'),ppdDefined),NyxPascalFPU('X87'));
end;

function Translate(const AText: TNyxText;
  const AContext: INyxPascalCompilerContext = nil): INyxPascalTranslation;
var LContext: INyxPascalCompilerContext;
begin
  LContext := AContext;

  if LContext = nil then LContext := Facts;
  Result := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('selection.pas'),AText),NyxPascalPhysicalFile('selection.pas'),
    LContext,TNyxPascalTranslationOptions.Defaults);
end;

function MarkerToken(const ASource: INyxPascalTranslation; const AName: TNyxText): Integer;
var LAt: Integer;
begin
  for LAt := 0 to ASource.Count-1 do
  begin

    if ASource.Text(LAt) = AName then Exit(LAt);
  end;
  raise Exception.Create('Missing source FPU marker: '+String(AName));
end;

function ContextAt(const ASource: INyxPascalTranslation; const AName: TNyxText): INyxPascalCompilerContext;
begin
  Result := ASource.State(ASource.Token(MarkerToken(ASource,AName)).State);
end;

procedure ContextContract;
var LBase, LSelected, LCaptured, LCurrent: INyxPascalCompilerContext;
begin
  LBase := Facts;
  Check((NyxPascalSourceFPU(LBase).Kind = pfsConfigured) and
    (NyxPascalSelectedFPU(LBase).Name = 'x87'),'An untouched owned context uses its initial snapshot');
  LSelected := NyxPascalWithSourceFPU(LBase,TNyxPascalSourceFPUSelection.Select(NyxPascalFPU('SSE2')));
  Check((NyxPascalSelectedFPU(LSelected).Name = 'sse2') and
    (NyxPascalConfiguredFPU(LSelected).Name = 'x87'),'A typed source selection cannot rewrite the initial profile');
  Check((NyxPascalSourceFPU(LBase).Kind = pfsConfigured) and
    not LBase.SameFacts(LSelected),'Fluent selection is copied and participates in exact context identity');
  LCaptured := CaptureNyxPascalCompilerContext(LSelected);
  Check(LCaptured.SameFacts(LSelected),'Complete capture retains the selection history');
  Check(NyxPascalSelectedFPU(RestoreNyxPascalLocalSettings(LSelected,LBase)).Name = 'sse2',
    'Local settings restore retains the current source selection');
  Check(NyxPascalSelectedFPU(LSelected.InMode(ppmObjFPC)).Name = 'sse2','A mode request does not select another FPU');
  LCurrent := LSelected.UnknownState;
  Check((NyxPascalConfiguredFPU(LCurrent).Name = 'x87') and not NyxPascalSelectedFPU(LCurrent).Known,
    'Unknown effects retain initial history without falling back to that earlier selection');
  Check(not LCurrent.SameFacts(LBase),'Unknown history differs from untouched configured history');
  LCurrent := NyxPascalWithConfiguredFPU(LSelected,NyxPascalFPU('AVX'));
  Check(NyxPascalSelectedFPU(LCurrent).Name = 'sse2','An explicit source selector remains independent of configuration updates');
  Check(NyxPascalSelectedFPU(LCaptured).Name = 'sse2','Earlier captured histories stay immutable');
end;

procedure SourceContract;
var LSource, LPrefix: INyxPascalTranslation;
  LContext: INyxPascalCompilerContext;
begin
  LSource := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('source.fpu.fixture.pas'),GFixture),
    NyxPascalPhysicalFile('source.fpu.fixture.pas'),Facts,TNyxPascalTranslationOptions.Defaults);
  Check(LSource.Resolved,'The complete compiler-accepted source FPU witness retains its selection history');
  Check(NyxPascalSelectedFPU(ContextAt(LSource,'AtX87')).Name = 'x87','The whole original X87 procedure keeps its selection');
  Check(NyxPascalSelectedFPU(ContextAt(LSource,'AtSSE2')).Name = 'sse2','The whole original SSE2 procedure keeps its selection');
  Check(NyxPascalSelectedFPU(ContextAt(LSource,'AfterPop')).Name = 'sse2','POP retains the actual source FPU selection');
  Check(NyxPascalSelectedFPU(ContextAt(LSource,'AfterDefine')).Name = 'x87','Authored defines do not impersonate selection');
  LContext := ContextAt(LSource,'AfterReassert');
  Check((NyxPascalSelectedFPU(LContext).Name = 'x87') and
    (NyxPascalConfiguredFPU(LContext).Name = 'x87'),'Reassertion retains separate initial and source histories');
  Check((LContext.Symbol(NyxPascalDefine('FPUX87')).IsDefined = ppkTrue) and
    (LContext.Symbol(NyxPascalDefine('FPUSSE2')).IsDefined = ppkTrue),'Only the actual old/new selector symbols are changed');
  LPrefix := NewNyxPascalTranslationPrefix(LSource,MarkerToken(LSource,'AtSSE2'),
    TNyxPascalTranslationOptions.Defaults);
  Check(LPrefix.Resolved and (NyxPascalSelectedFPU(LPrefix.State(LPrefix.StateCount-1)).Name = 'sse2'),
    'An owned bounded prefix excludes later reassertions');
end;

procedure RefusalAndLifetime;
var
  LSource: INyxPascalTranslation;
  LFiles: INyxPascalFileCatalog;
  LContext: INyxPascalCompilerContext;
  LCancel: INyxCancellationScope;
  LRejected: Boolean;
begin
  LSource := Translate('{$fputype SSE2}{$ifdef ABSENT}{$fputype X87}{$endif}Marker');
  Check(LSource.Resolved and (NyxPascalSelectedFPU(ContextAt(LSource,'Marker')).Name = 'sse2'),
    'Inactive selection does not change the current FPU');
  LSource := Translate('{$if MAYBE}{$fputype SSE2}{$endif}Marker');
  LContext := ContextAt(LSource,'Marker');
  Check(not LSource.Resolved and not NyxPascalSelectedFPU(LContext).Known,
    'Uncertain participation cannot retain a definite selection');
  Check((LContext.Symbol(NyxPascalDefine('FPUX87')).IsDefined = ppkUnknown) and
    (LContext.Symbol(NyxPascalDefine('FPUSSE2')).IsDefined = ppkUnknown),'Both conditional selector effects remain unknown');
  LSource := Translate('{$if MAYBE}{$fputype SSE2}{$endif}{$fputype X87}Marker');
  Check(not LSource.Resolved and not NyxPascalSelectedFPU(ContextAt(LSource,'Marker')).Known,
    'An unknown old selector refuses reconstruction instead of guessing which symbol to clear');
  LSource := Translate('{$fputype SSE2}{$unknownpolicy}{$fputype X87}Marker');
  Check(not LSource.Resolved and not NyxPascalSelectedFPU(ContextAt(LSource,'Marker')).Known,
    'A blocked source cannot recover a selector by borrowing initial history');
  LSource := Translate('{$fputype SSE2 trailing prose}Marker');
  Check(not LSource.Resolved and not NyxPascalSelectedFPU(ContextAt(LSource,'Marker')).Known,
    'The complete FPUTYPE argument is required');
  LSource := Translate('{$fputype FUTURE}Marker');
  Check(not LSource.Resolved,'Open selector references do not invent compiler support');
  LSource := Translate('{$fputype SSE2}Marker',NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3,3,1),ppmDelphi).CompleteDefines(True)
    .WithSymbol(NyxPascalDefine('CPUI386'),ppdDefined).WithSymbol(NyxPascalDefine('FPUX87'),ppdDefined));
  Check(not LSource.Resolved,'Source macro spelling cannot replace a missing configured profile');
  LSource := Translate('{$fputype SSE2}Marker',Facts.WithSymbol(NyxPascalDefine('CPUI386'),ppdUndefined));
  Check(not LSource.Resolved,'A configured selector alone cannot invent a qualified target');
  LSource := Translate('{$fputype SSE2}Marker',NyxPascalWithConfiguredFPU(
    NewNyxPascalCompilerContext(ppcFPC,TNyxPascalCompilerVersion.Create(3,2,2),ppmDelphi)
    .CompleteDefines(True).WithSymbol(NyxPascalDefine('CPUI386'),ppdDefined),NyxPascalFPU('X87')));
  Check(not LSource.Resolved,'Unqualified compiler versions retain refusal');
  LSource := Translate('{$fputype SSE2}Marker',NyxPascalWithConfiguredFPU(
    NewNyxPascalCompilerContext(ppcPas2JS,TNyxPascalCompilerVersion.Create(3,3,1),ppmDelphi)
    .CompleteDefines(True).WithSymbol(NyxPascalDefine('CPUI386'),ppdDefined),NyxPascalFPU('X87')));
  Check(not LSource.Resolved,'Another compiler family cannot inherit FPC selector semantics');
  LSource := Translate('{$undef CPUI386}{$define FPUSSE2}{$fputype SSE2}Marker');
  Check(LSource.Resolved and (NyxPascalSelectedFPU(ContextAt(LSource,'Marker')).Name = 'sse2'),
    'Authored target/selector defines cannot retarget the immutable initial qualification');
  LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('main.pas'),
    '{$include part.inc}After').WithFile(NyxPascalPhysicalFile('part.inc'),'{$fputype SSE2}Included');
  LSource := NewNyxPascalTranslation(LFiles,NyxPascalPhysicalFile('main.pas'),Facts,TNyxPascalTranslationOptions.Defaults);
  Check(LSource.Resolved and (NyxPascalSelectedFPU(ContextAt(LSource,'Included')).Name = 'sse2') and
    (NyxPascalSelectedFPU(ContextAt(LSource,'After')).Name = 'sse2'),'Physical includes share exact selection chronology');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LSource := nil;
  LRejected := False;
  try
    LSource := NewNyxPascalTranslation(LFiles,NyxPascalPhysicalFile('main.pas'),Facts,
      TNyxPascalTranslationOptions.Defaults,LCancel);
  except
    on ENyxPascalAnalysisCancelled do LRejected := True;
  end;
  Check(LRejected and (LSource = nil),'Cancellation publishes no partial source selection');
end;

procedure MacroContract;
var
  LSource: INyxPascalTranslation;
  LMacros: INyxPascalTranslationMacros;
  LEnvironment: INyxPascalMacroEnvironment;
begin
  LSource := Translate('{$macro on}{$define FPUSSE2:=77}{$fputype SSE2}First '+
    '{$fputype X87}Second {$fputype SSE2}Third');
  Check(LSource.Resolved and Supports(LSource,INyxPascalTranslationMacros,LMacros),
    'Selector changes retain an owned replacement environment');
  LEnvironment := LMacros.StateMacros(LSource.Token(MarkerToken(LSource,'First')).State);
  Check(LEnvironment.Has(NyxPascalDefine('FPUSSE2')) and
    (LEnvironment.Definition(NyxPascalDefine('FPUSSE2')).Replacement = '77'),
    'Defining the new selector preserves its existing replacement buffer');
  LEnvironment := LMacros.StateMacros(LSource.Token(MarkerToken(LSource,'Second')).State);
  Check(not LEnvironment.Has(NyxPascalDefine('FPUSSE2')),'Undefining the old selector clears its replacement buffer');
  LEnvironment := LMacros.StateMacros(LSource.Token(MarkerToken(LSource,'Third')).State);
  Check(not LEnvironment.Has(NyxPascalDefine('FPUSSE2')),
    'Later redefinition cannot resurrect a retired old replacement buffer');
  LSource := Translate('{$macro on}{$define FPUX87:=77}{$fputype X87}Marker');
  Supports(LSource,INyxPascalTranslationMacros,LMacros);
  Check(not LMacros.StateMacros(LSource.Token(MarkerToken(LSource,'Marker')).State)
    .Has(NyxPascalDefine('FPUX87')),'Reasserting the same selector still clears its old replacement first');
end;

function StorageIdentity(const ASelection: TNyxPascalSourceFPUSelection): TNyxText;
var
  LContext: INyxPascalCompilerContext;
  LFiles: INyxPascalFileCatalog;
  LView, LSystem: INyxPascalTranslatedSyntax;
  LWorld: INyxPascalSemanticWorld;
  LResult: INyxPascalConstantResult;
  LIdentity: INyxPascalConstantStorageIdentity;
begin
  LContext := NyxPascalWithSourceFPU(Facts.WithOption('H',ppkTrue),ASelection);
  LContext := NyxPascalWithCapturedSize(LContext,NyxPascalSizeReference('Byte'),
    NyxPascalSizeFact(pssKnown,ppmDelphi,ppkTrue,1));
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
  Check(Supports(LResult,INyxPascalConstantStorageIdentity,LIdentity),'A bound shape retains its context identity');
  Result := LIdentity.StorageKey.Value;
end;

procedure StorageContract;
var LConfigured, LUnknown, LX87, LSSE2: TNyxText;
begin
  LConfigured := StorageIdentity(TNyxPascalSourceFPUSelection.Configured);
  LUnknown := StorageIdentity(TNyxPascalSourceFPUSelection.Unknown);
  LX87 := StorageIdentity(TNyxPascalSourceFPUSelection.Select(NyxPascalFPU('X87')));
  LSSE2 := StorageIdentity(TNyxPascalSourceFPUSelection.Select(NyxPascalFPU('SSE2')));
  Check(Pos('source-fpu-selection-v1',LConfigured) = 0,'Untouched configured history keeps the preceding identity form');
  Check((LConfigured <> LUnknown) and (LConfigured <> LX87) and (LX87 <> LSSE2) and
    (LUnknown <> LX87) and (LUnknown <> LSSE2),'Unknown, configured and selected histories cannot collide');
  Check((Pos('source-fpu-selection-v1',LUnknown) > 0) and
    (Pos('source-fpu-selection-v1',LSSE2) > 0),'Unknown and explicit source selections have tagged identities');
end;

procedure WorkContract;
const CText = '{$fputype SSE2}First{$push}{$fputype X87}Inside{$pop}After';
var
  LFiles: INyxPascalFileCatalog;
  LSource, LRetained: INyxPascalTranslation;
  LBudget: INyxPascalAnalysisBudget;
  LAt, LFit: Integer;
  LRejected: Boolean;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('budget.pas'),CText);
  LFit := -1;
  for LAt := 1 to 20000 do
  begin
    LSource := nil;
    LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      try
        LSource := NewNyxPascalTranslation(LFiles,NyxPascalPhysicalFile('budget.pas'),Facts,
          TNyxPascalTranslationOptions.Defaults,LBudget);
        Check(LSource.Resolved and (NyxPascalSelectedFPU(ContextAt(LSource,'Inside')).Name = 'x87') and
          (NyxPascalSelectedFPU(ContextAt(LSource,'After')).Name = 'x87'),
          'First sufficient work allowance publishes the complete selection history');
        LFit := LAt;
        LRetained := LSource;
      except
        on ENyxPascalAnalysisWorkLimit do
          Check((LSource = nil) and (LBudget.Receipt.Total <= LAt),'Every smaller work allowance refuses atomically');
      end;
    finally
      LBudget.Retire;
    end;

    if LFit > 0 then Break;
  end;
  Check(LFit > 1,'Selection translation has a bounded positive first fit');
  Check(NyxPascalSelectedFPU(ContextAt(LRetained,'After')).Name = 'x87','Published contexts retire their execution budget');
  WriteLn('source FPU work / first fit=',LFit,' every smaller allowance refused');
  LSource := nil;
  LRejected := False;
  try
    LSource := NewNyxPascalTranslation(LFiles,NyxPascalPhysicalFile('budget.pas'),Facts,
      TNyxPascalTranslationOptions.Defaults.UpToStates(2));
  except
    on ENyxPascalTranslationLimit do LRejected := True;
  end;
  Check(LRejected and (LSource = nil),'State limits refuse without a partial source selection');
end;

{$ifndef PAS2JS}
procedure NativeWitness;
var
  LText: TNyxText;
  LStream: TFileStream;
begin
  LText := TNyxText(NativeSourceFPUObservations);
  Write(LText);
  Check(NativeSourceFPUCalls = 5,'Five whole native expressions execute independently');
  Check(Pos('x87=Extended'+#10,LText) > 0,'Explicit X87 selects native extended division');
  Check(Pos('sse2=Double'+#10,LText) > 0,'Explicit SSE2 selects native double division');
  Check(Pos('after-pop=Double'+#10,LText) > 0,'PUSH/POP does not restore the FPU selection');
  Check(Pos('after-define=Extended'+#10,LText) > 0,'Authored define spelling cannot change selected instructions');
  Check(Pos('after-reassert=Extended'+#10,LText) > 0,'An explicit directive can reassert the actual selection');
  Check((Pos('x87-define=true'+#10,LText) > 0) and (Pos('sse2-define=true'+#10,LText) > 0) and
    (Pos('old-x87-define=false'+#10,LText) > 0) and (Pos('after-pop-sse2=true'+#10,LText) > 0),
    'Native selection updates exactly its old/new selector symbols');
  Check(Pos('unrelated-define-retained=true'+#10,LText) > 0,'Unrelated authored selector defines remain separate');
  LStream := TFileStream.Create('tests/source.fpu.fixture.pas',fmOpenRead or fmShareDenyWrite);
  try
    SetLength(GFixture,LStream.Size);

    if Length(GFixture) > 0 then LStream.ReadBuffer(GFixture[1],Length(GFixture));
  finally
    LStream.Free;
  end;
end;
{$endif}

begin
  try
    GFixture := 'unit source.fpu.fixture;{$mode delphi}interface implementation '+
      '{$fputype X87}procedure AtX87;begin end;{$push}{$fputype SSE2}procedure AtSSE2;begin end;'+
      '{$pop}procedure AfterPop;begin end;{$fputype X87}{$undef FPUX87}{$define FPUSSE2}'+
      'procedure AfterDefine;begin end;{$fputype X87}procedure AfterReassert;begin end;end.';
    {$ifndef PAS2JS}NativeWitness;{$endif}
    ContextContract;
    SourceContract;
    RefusalAndLifetime;
    MacroContract;
    StorageContract;
    WorkContract;
    WriteLn('PASS ',GChecks,' source FPU selection checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ',E.ClassName,': ',E.Message);
      ExitCode := 1;
    end;
  end;
end.
