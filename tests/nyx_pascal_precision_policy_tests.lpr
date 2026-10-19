{ The whole compiler witness is independent of source-policy interpretation.
  Source contexts record intent without asserting a target floating format. }
program nyx_pascal_precision_policy_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses SysUtils, Classes, nyx.text, nyx.scheduler, nyx.pascal.files, nyx.pascal.context,
  nyx.pascal.translation, nyx.pascal.work, nyx.pascal.lexing, nyx.pascal.syntax,
  nyx.pascal.syntax.translated, nyx.pascal.binding, nyx.pascal.constants
  {$ifndef PAS2JS}, precision.policy.fixture{$endif};

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
end;

function Translate(const AText: TNyxText;
  const AContext: INyxPascalCompilerContext = nil): INyxPascalTranslation;
var
  LContext: INyxPascalCompilerContext;
begin
  LContext := AContext;

  if LContext = nil then LContext := Facts;
  Result := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('policy.pas'),AText),NyxPascalPhysicalFile('policy.pas'),
    LContext,TNyxPascalTranslationOptions.Defaults);
end;

function MarkerToken(const ASource: INyxPascalTranslation; const AName: TNyxText): Integer;
var LAt: Integer;
begin
  for LAt := 0 to ASource.Count-1 do
  begin

    if ASource.Text(LAt) = AName then Exit(LAt);
  end;
  raise Exception.Create('Missing original policy marker: '+String(AName));
end;

function PrecisionAt(const ASource: INyxPascalTranslation; const AName: TNyxText): TNyxPascalKnowledge;
begin
  Result := NyxPascalCompilerPolicies(ASource.State(ASource.Token(MarkerToken(ASource,AName)).State))
    .ExcessPrecisionRequested;
end;

procedure ContextContract;
var
  LBase, LOn, LOff, LCaptured, LRestored: INyxPascalCompilerContext;
  LPolicy: TNyxPascalCompilerPolicies;
begin
  LBase := Facts;
  LPolicy := NyxPascalCompilerPolicies(LBase);
  Check(LPolicy.ExcessPrecisionRequested = ppkUnknown,'Uncaptured policy is not a compiler default');
  LOn := NyxPascalWithCompilerPolicies(LBase,LPolicy.RequestExcessPrecision(ppkTrue));
  LOff := NyxPascalWithCompilerPolicies(LOn,NyxPascalCompilerPolicies(LOn).RequestExcessPrecision(ppkFalse));
  Check((LPolicy.ExcessPrecisionRequested = ppkUnknown) and
    (NyxPascalCompilerPolicies(LOn).ExcessPrecisionRequested = ppkTrue),'Fluent changes copy the original policy');
  Check(not LOn.SameFacts(LOff) and not LBase.SameFacts(LOff),'Exact context identity includes the precision request');
  LCaptured := CaptureNyxPascalCompilerContext(LOn);
  Check(LCaptured.SameFacts(LOn),'Complete capture retains the policy');
  LRestored := RestoreNyxPascalLocalSettings(LOff,LOn);
  Check(NyxPascalCompilerPolicies(LRestored).ExcessPrecisionRequested = ppkTrue,'Local restore recovers the saved value');
  Check(NyxPascalCompilerPolicies(LOn.UnknownState).ExcessPrecisionRequested = ppkUnknown,'Unknown effects revoke precision knowledge');
  Check(NyxPascalCompilerPolicies(LOn.InMode(ppmObjFPC)).ExcessPrecisionRequested = ppkUnknown,
    'Mode changes do not invent an effective precision default');
  Check(NyxPascalCompilerPolicies(LOn.InMode(ppmDelphi)).ExcessPrecisionRequested = ppkUnknown,
    'Even a repeated mode request needs its own effective-setting qualification');
  Check(NyxPascalCompilerPolicies(LCaptured).ExcessPrecisionRequested = ppkTrue,'Earlier captures stay immutable');
end;

procedure SourceContract;
var
  LSource, LPrefix: INyxPascalTranslation;
begin
  LSource := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('precision.policy.fixture.pas'),GFixture),
    NyxPascalPhysicalFile('precision.policy.fixture.pas'),Facts,TNyxPascalTranslationOptions.Defaults);
  Check(LSource.Resolved,'The whole legal precision witness must retain its source policies');
  Check(PrecisionAt(LSource,'ObserveOff') = ppkFalse,'The original OFF procedure retains its request');
  Check(PrecisionAt(LSource,'ObserveOn') = ppkTrue,'The original ON procedure retains its request');
  Check(PrecisionAt(LSource,'ObserveRestored') = ppkFalse,'POP restores the original local request');
  LPrefix := NewNyxPascalTranslationPrefix(LSource,MarkerToken(LSource,'ObserveOn'),
    TNyxPascalTranslationOptions.Defaults);
  Check(LPrefix.Resolved and (NyxPascalCompilerPolicies(LPrefix.State(LPrefix.StateCount-1))
    .ExcessPrecisionRequested = ppkTrue),'A bounded prefix excludes the later POP instead of inheriting final policy');
end;

procedure RefusalAndLifetime;
var
  LSource: INyxPascalTranslation;
  LFiles: INyxPascalFileCatalog;
  LCancel: INyxCancellationScope;
  LRejected: Boolean;
begin
  LSource := Translate('{$excessprecision on}{$ifdef ABSENT}{$excessprecision off}{$endif}Marker');
  Check(LSource.Resolved and (PrecisionAt(LSource,'Marker') = ppkTrue),'Inactive directives do not alter precision');
  LSource := Translate('{$excessprecision on}{$if MAYBE}{$excessprecision off}{$endif}Marker');
  Check(not LSource.Resolved and (PrecisionAt(LSource,'Marker') = ppkUnknown),'Uncertain participation revokes knowledge');
  LSource := Translate('{$excessprecision+}{$push}{$excessprecision off explanatory text}Inside{$pop}After');
  Check(LSource.Resolved and (PrecisionAt(LSource,'Inside') = ppkFalse) and
    (PrecisionAt(LSource,'After') = ppkTrue),'The qualified FPC Boolean reader preserves signs, comments and local lifetime');
  LSource := Translate('{$excessprecision on}{$excessprecision offline}Marker');
  Check(not LSource.Resolved and (PrecisionAt(LSource,'Marker') = ppkUnknown),'An invalid identifier is not an OFF prefix');
  LSource := Translate('{$excessprecision on}{$unknownpolicy}Marker');
  Check(not LSource.Resolved and (PrecisionAt(LSource,'Marker') = ppkUnknown),'An unknown effect cannot preserve precision');
  LSource := Translate('{$excessprecision on}Marker',NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3,2,2),ppmDelphi).CompleteDefines(True));
  Check(not LSource.Resolved,'Unqualified compiler versions retain refusal');
  LSource := Translate('{$excessprecision on}Marker',NewNyxPascalCompilerContext(ppcPas2JS,
    TNyxPascalCompilerVersion.Create(3,3,1),ppmDelphi).CompleteDefines(True));
  Check(not LSource.Resolved,'Another compiler family cannot inherit FPC policy semantics');
  LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('main.pas'),
    '{$excessprecision on}{$include part.inc}After').WithFile(NyxPascalPhysicalFile('part.inc'),
    '{$excessprecision off}Included');
  LSource := NewNyxPascalTranslation(LFiles,NyxPascalPhysicalFile('main.pas'),Facts,TNyxPascalTranslationOptions.Defaults);
  Check(LSource.Resolved and (PrecisionAt(LSource,'Included') = ppkFalse) and
    (PrecisionAt(LSource,'After') = ppkFalse),'Textual includes share the precise local request chronology');
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
  Check(LRejected and (LSource = nil),'Cancellation publishes no partial source policy');
end;

function StorageIdentity(AValue: TNyxPascalKnowledge): TNyxText;
var
  LContext: INyxPascalCompilerContext;
  LFiles: INyxPascalFileCatalog;
  LView, LSystem: INyxPascalTranslatedSyntax;
  LWorld: INyxPascalSemanticWorld;
  LResult: INyxPascalConstantResult;
  LIdentity: INyxPascalConstantStorageIdentity;
begin
  LContext := NyxPascalWithCompilerPolicies(Facts.WithOption('H',ppkTrue),
    TNyxPascalCompilerPolicies.Unknown.RequestExcessPrecision(AValue));
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
  WriteLn('precision storage / issue=',Ord(LResult.Issue),' related=',String(LResult.RelatedName));
  Check(Supports(LResult,INyxPascalConstantStorageIdentity,LIdentity),'A bound shape retains its complete context identity');
  Result := LIdentity.StorageKey.Value;
end;

procedure StorageContract;
var LUnknown, LOn, LOff: TNyxText;
begin
  LUnknown := StorageIdentity(ppkUnknown);
  LOn := StorageIdentity(ppkTrue);
  LOff := StorageIdentity(ppkFalse);
  Check(Pos('excess-precision-request-v1',LUnknown) = 0,'Unspecified contexts keep their existing identity form');
  Check((LUnknown <> LOn) and (LOn <> LOff) and (LOff <> LUnknown),'Known precision values cannot collide in storage identity');
  Check((Pos('excess-precision-request-v1',LOn) > 0) and
    (Pos('excess-precision-request-v1',LOff) > 0),'Each known policy has a tagged identity boundary');
end;

procedure WorkContract;
const CText = '{$excessprecision on}First{$push}{$excessprecision off}Inside{$pop}After';
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
        Check(LSource.Resolved and (PrecisionAt(LSource,'Inside') = ppkFalse) and
          (PrecisionAt(LSource,'After') = ppkTrue),'First sufficient allowance publishes the complete source history');
        LFit := LAt;
        LRetained := LSource;
      except
        on ENyxPascalAnalysisWorkLimit do
          Check((LSource = nil) and (LBudget.Receipt.Total <= LAt),'Every smaller allowance refuses without a partial policy');
      end;
    finally
      LBudget.Retire;
    end;

    if LFit > 0 then Break;
  end;
  Check(LFit > 1,'Policy translation has a bounded positive first fit');
  Check(PrecisionAt(LRetained,'After') = ppkTrue,'The published context retires its execution budget');
  WriteLn('precision policy work / first fit=',LFit,' every smaller allowance refused');
  LSource := nil;
  LRejected := False;
  try
    LSource := NewNyxPascalTranslation(LFiles,NyxPascalPhysicalFile('budget.pas'),Facts,
      TNyxPascalTranslationOptions.Defaults.UpToStates(2));
  except
    on ENyxPascalTranslationLimit do LRejected := True;
  end;
  Check(LRejected and (LSource = nil),'State limits refuse without a partial precision history');
end;

{$ifndef PAS2JS}
procedure NativeWitness;
var
  LObserved: TNyxText;
  LStream: TFileStream;
begin
  LObserved := TNyxText(NativePrecisionObservations);
  Write(LObserved);
  Check(NativePrecisionInvocations = 4,'Every native policy observation executed once');
  Check(Pos('off-single=Single'+#10,LObserved) > 0,'OFF keeps the native Single result');
  Check(Pos('off-mixed=Double'+#10,LObserved) > 0,'OFF retains the wider mixed input type');
  Check(Pos('on-single=Extended'+#10,LObserved) > 0,'ON uses the independent native best-real result');
  Check(Pos('restored-single=Single'+#10,LObserved) > 0,'POP restores the earlier local precision policy');
  LStream := TFileStream.Create('tests/precision.policy.fixture.pas',fmOpenRead or fmShareDenyWrite);
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
    GFixture := 'unit precision.policy.fixture;{$mode delphi}interface implementation '+
      '{$excessprecision off}procedure ObserveOff;begin end;{$push}{$excessprecision on}'+
      'procedure ObserveOn;begin end;{$pop}procedure ObserveRestored;begin end;end.';
    {$ifndef PAS2JS}NativeWitness;{$endif}
    ContextContract;
    SourceContract;
    RefusalAndLifetime;
    StorageContract;
    WorkContract;
    WriteLn('PASS ',GChecks,' excess-precision policy checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ',E.ClassName,': ',E.Message);
      ExitCode := 1;
    end;
  end;
end.
