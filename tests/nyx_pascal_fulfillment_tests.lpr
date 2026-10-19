{ nyx
  Copyright (c) 2020 mr-highball

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
}
program nyx_pascal_fulfillment_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.bytes, nyx.scheduler, nyx.pascal.context, nyx.pascal.files,
  nyx.pascal.lexing,
  nyx.pascal.translation, nyx.pascal.syntax, nyx.pascal.syntax.translated,
  nyx.pascal.binding, nyx.pascal.constants, nyx.pascal.values,
  nyx.pascal.requirements, nyx.pascal.compilertrace, nyx.pascal.modules,
  nyx.pascal.fulfillment, nyx.pascal.conditions, nyx.pascal.witness,
  nyx.pascal.encoding, nyx.pascal.work
  {$ifndef PAS2JS}, LazFileUtils, nyx.studio.pascalprofiles{$endif};

type
  { Synthetic portable input only. Production factories freeze both fact/default
    packets and must never call this producer's untrusted Merge implementation. }
  TCapture = class(TInterfacedObject, INyxFPCCompilerCapture)
  public
    Facts: INyxPascalCompilerContext;
    Defaults: INyxPascalCompilerContext;
    function Context: INyxPascalCompilerContext;
    function ObservedDefaults: INyxPascalCompilerContext;
    function Merge(const ANext: INyxFPCCompilerCapture): INyxFPCCompilerCapture;
  end;
  { Mutable response vector qualifies complete capture and duplicate-job removal.
    Interface leases live in a class-owned dynamic array, never in records. }
  TRequests = class(TInterfacedObject, INyxPascalCompilerRequirements)
  public
    Values: array of INyxPascalCompilerRequirement;
    function RequirementCount: Integer;
    function RequirementAt(AIndex: Integer): INyxPascalCompilerRequirement;
  end;
  { Captured two-field source with shared and unrelated module roots. Missing
    requests come from ordinary constant binding, not an English diagnostic or
    fixed host probe list. All fields are outward immutable interface leases. }
  TFixture = class
  public
    Files: INyxPascalFileCatalog;
    Modules: INyxPascalModuleCatalog;
    Root: INyxPascalCompilerContext;
    View: INyxPascalTranslatedSyntax;
    World: INyxPascalSemanticWorld;
    Requests: INyxPascalCompilerRequirements;
    constructor Create(const AContext: INyxPascalCompilerContext);
  end;
  {$ifndef PAS2JS}
  { Independent compiled packed-layout oracle. The source witness compiles the
    whole corresponding declarations from captured include bytes; these types
    make native comparisons independent of parser-supplied numeric sizes. }
  TPackedPair = packed record
    First: Byte;
    Second: Word;
  end;
  { A second authored layout requires revalidation of the earlier observation
    in the same compiler job before any new source receipt can be published. }
  TPackedEnvelope = packed record
    Pair: TPackedPair;
    Last: Byte;
  end;
  {$endif}

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
end;

function TCapture.Context: INyxPascalCompilerContext;
begin
  Result := Facts;
end;

function TCapture.ObservedDefaults: INyxPascalCompilerContext;
begin
  Result := Defaults;
end;

function TCapture.Merge(const ANext: INyxFPCCompilerCapture): INyxFPCCompilerCapture;
begin
  Result := nil;
  raise Exception.Create('Frozen capture operations cannot delegate to a mutable producer');
end;

function Capture(const AFacts, ADefaults: INyxPascalCompilerContext): INyxFPCCompilerCapture;
var
  LValue: TCapture;
  LOwner: INyxFPCCompilerCapture;
begin
  LValue := TCapture.Create;
  LOwner := LValue;
  LValue.Facts := AFacts;
  LValue.Defaults := ADefaults;
  Result := FreezeNyxFPCCompilerCapture(LOwner);
end;

function TRequests.RequirementCount: Integer;
begin
  Result := Length(Values);
end;

function TRequests.RequirementAt(AIndex: Integer): INyxPascalCompilerRequirement;
begin
  Result := Values[AIndex];
end;

constructor TFixture.Create(const AContext: INyxPascalCompilerContext);
var
  LProfile: INyxPascalCompilerContext;
  LPeer: INyxPascalCompilerContext;
  LOther: INyxPascalCompilerContext;
  LValue: INyxPascalConstantResult;
  LRequests: TRequests;
  LAt: Integer;
begin
  inherited Create;
  Root := AContext;
  LProfile := NewNyxPascalCompilerContext(ppcFPC, Root.Version, ppmFPC).WithOption('H', ppkFalse);
  LProfile := NyxPascalWithBootstrapContext(LProfile, Root);
  LPeer := NewNyxPascalCompilerContext(ppcFPC, Root.Version, ppmDelphi).WithOption('H', ppkTrue);
  LPeer := NyxPascalWithBootstrapContext(LPeer, Root);
  LOther := NyxPascalWithBootstrapContext(LPeer,
    Root.WithSymbol(NyxPascalDefine('DISTINCT_ROOT'), ppdDefined));
  Files := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('system.pas'),
      'unit System; interface var A:Word; B:Longint; implementation end.')
    .WithFile(NyxPascalPhysicalFile('peer.pas'), 'unit Peer; interface implementation end.')
    .WithFile(NyxPascalPhysicalFile('other.pas'), 'unit Other; interface implementation end.');
  Modules := NewNyxPascalModuleCatalog
    .WithModule(NewNyxPascalModuleSource(NyxPascalModule('System'), NyxPascalPhysicalFile('system.pas'))
      .InContext(LProfile, TNyxPascalTranslationOptions.Defaults))
    .WithModule(NewNyxPascalModuleSource(NyxPascalModule('Peer'), NyxPascalPhysicalFile('peer.pas'))
      .InContext(LPeer, TNyxPascalTranslationOptions.Defaults))
    .WithModule(NewNyxPascalModuleSource(NyxPascalModule('Other'), NyxPascalPhysicalFile('other.pas'))
      .InContext(LOther, TNyxPascalTranslationOptions.Defaults));
  View := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(Files, Modules.Entry(0).Root,
    LProfile, TNyxPascalTranslationOptions.Defaults), TNyxPascalSyntaxOptions.Defaults);
  World := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults
    .WithImplicitScopes(pispCompilerUnits)).WithModule(View);
  LRequests := TRequests.Create;
  Requests := LRequests;
  SetLength(LRequests.Values, 3);
  for LAt := 0 to 1 do
  begin
    LValue := PlanNyxPascalConstant('High(' + Chr(Ord('A') + LAt) + ')',
      TNyxPascalConstantOptions.Defaults).Evaluate(World.At(0, 0, View.Syntax.Lexical.Scalars),
      TNyxPascalConstantOptions.Defaults);
    LRequests.Values[LAt] := CaptureNyxPascalRequirements(LValue).RequirementAt(0);
  end;
  LRequests.Values[2] := LRequests.Values[0];
end;

function SourceFiles: INyxPascalFileCatalog;
begin
  Result := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('fixture/system.pp'),
      'unit System; interface {$mode objfpc}{$include payload.inc} implementation end.')
    .WithFile(NyxPascalPhysicalFile('fixture/payload.inc'),
      'type TPair=packed record First:Byte; Second:Word; end;'#10 +
      'const PairBytes=sizeof(TPair);'#10 +
      '{$packrecords 8}{$macro on}{$define UNRELATED_NAME:=Byte}'#10 +
      '{$if PairBytes>0}const FirstReady=1;{$endif}'#10 +
      'type TEnvelope=packed record Pair:TPair; Last:Byte; end;'#10 +
      'const EnvelopeBytes=sizeof(TEnvelope);'#10 +
      '{$if EnvelopeBytes>0}const SecondReady=1;{$endif}');
end;

function SourceModules(const AOwner: INyxFPCCompilerCapture): INyxPascalModuleCatalog;
var LContext: INyxPascalCompilerContext;
begin
  LContext := NewNyxPascalCompilerContext(ppcFPC, AOwner.Context.Version, ppmFPC)
    .WithOption('H', ppkFalse);
  LContext := NyxPascalWithBootstrapContext(LContext, AOwner.Context);
  Result := NewNyxPascalModuleCatalog.WithModule(NewNyxPascalModuleSource(
    NyxPascalModule('System'), NyxPascalPhysicalFile('fixture/system.pp'))
    .InContext(LContext, TNyxPascalTranslationOptions.Defaults));
end;

function SourceQuery(const AFiles: INyxPascalFileCatalog;
  const AModules: INyxPascalModuleCatalog;
  const AWorld: INyxPascalSemanticWorld): INyxPascalSemanticTranslation;
begin
  Result := NewNyxPascalSemanticTranslation(AFiles, AModules.Entry(0).Root,
    AModules.Entry(0).Context, AWorld, AModules.Entry(0).Translation,
    TNyxPascalSyntaxOptions.Defaults, TNyxPascalConditionalOptions.Defaults);
end;

{ Controlled external transcript input exercises parser refusal, separately from
  the actual native compiler below. The test never labels these values native. }
function SourceTranscript(const AWitness: INyxPascalSourceWitness;
  AFirst, ASecond: Integer; ACount: Integer): TNyxText;
var
  LRoot, LPrefix, LOwner, LRowText: TNyxText;
  LAt, LRow, LBit, LValue: Integer;
begin
  LRoot := NyxDecodeUTF8(NyxPascalSourceBytesAt(AWitness.Files,
    AWitness.Files.Find(AWitness.Root)).Slice(NyxPascalByteSpan(0,
      NyxPascalSourceBytesAt(AWitness.Files, AWitness.Files.Find(AWitness.Root)).Count)));
  LAt := Pos('NYX_SOURCE_', LRoot);
  LPrefix := Copy(LRoot, LAt, Length('NYX_SOURCE_') + 32 + 1);
  Result := 'system.pp(1,1) Note: (2025) User defined: ' + LPrefix + 'READY'#10;
  for LRow := 0 to ACount - 1 do
  begin
    LRowText := TNyxText(IntToStr(LRow));
    LOwner := 'payload.inc(1,1) Note: (2025) User defined: ' + LPrefix + LRowText;
    Result := Result + LOwner + '_BEGIN'#10;
    LValue := AFirst;

    if LRow > 0 then LValue := ASecond;
    for LBit := 0 to 30 do
      Result := Result + LOwner + '_BIT_' + TNyxText(IntToStr(LBit)) + '_' +
        TNyxText(IntToStr((LValue shr LBit) and 1)) + #10;
    Result := Result + LOwner + '_END'#10;
  end;
  Result := Result + 'payload.inc(1,1) Fatal: (2022) User defined: ' + LPrefix + 'STOP'#10 +
    'Fatal: (1018) Compilation aborted'#10;
end;

procedure BindingCorrespondence;
const
  CText: TNyxText = 'unit System; interface type TObject=class end; ' +
    'TData=record Value:Byte; end; TAlias=TData; const Earlier=sizeof(TAlias); ' +
    'type TShadow=class public type TData=record Value:Word; end; const Marker=1; end; ' +
    'TAliasShadow=class public type TData=System.TData; const Marker=1; end; ' +
    'THidden=class strict private type TPrivate=record Value:Byte; end; public const Marker=sizeof(TPrivate); end; ' +
    'TGeneric<T>=class public type TElement=record Value:T; end; const Marker=1; end; ' +
    'implementation end.';
var
  LView: INyxPascalTranslatedSyntax;
  LWorld, LOtherWorld: INyxPascalSemanticWorld;
  LBefore, LAfter, LShadow, LAliasShadow, LGeneric: INyxPascalBindingSite;
  LBudget: INyxPascalAnalysisBudget;
  LCancel: INyxCancellationScope;
  LRejected: Boolean;

  function Inside(const AName: TNyxText): INyxPascalBindingSite;
  var LScope: Integer;
  begin
    LScope := LView.Syntax.Declaration(LView.Syntax.Named(0, AName)).NestedScope;
    Result := LWorld.At(0, LScope, LView.Syntax.Declaration(
      LView.Syntax.Named(LScope, 'Marker')).ExpressionSpan.Start);
  end;

begin
  LView := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(
    NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('identity.pas'), CText),
    NyxPascalPhysicalFile('identity.pas'), NewNyxPascalCompilerContext(ppcFPC,
      TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi).CompleteDefines(True),
    TNyxPascalTranslationOptions.Defaults), TNyxPascalSyntaxOptions.Defaults);
  Check(LView.Syntax.DiagnosticCount = 0, 'The binding correspondence control has complete declared syntax');
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LView);
  LBefore := LWorld.At(0, 0, LView.Syntax.Declaration(LView.Syntax.Named(0, 'Earlier')).ExpressionSpan.Start);
  LAfter := LWorld.At(0, 0, LView.Syntax.Lexical.Scalars);
  Check(NyxPascalSameStorageBinding(LBefore, LAfter, NyxPascalBindingName('TAlias')),
    'The same visible name retains its exact alias and nominal definition chain across sites');
  LShadow := Inside('TShadow');
  Check(not NyxPascalSameStorageBinding(LBefore, LShadow, NyxPascalBindingName('TData')),
    'A later same-spelled type with another definition cannot borrow the earlier layout');
  Check(NyxPascalSameStorageBinding(LBefore, LShadow, NyxPascalBindingName('System.TData')),
    'Explicit namespace visibility can prove the same definition despite local shadowing');
  LAliasShadow := Inside('TAliasShadow');
  Check(not NyxPascalSameStorageBinding(LBefore, LAliasShadow, NyxPascalBindingName('TData')),
    'A different visible alias cannot pass merely because it reaches the same terminal record');
  Check(not NyxPascalSameStorageBinding(Inside('THidden'), LAfter, NyxPascalBindingName('TPrivate')),
    'A name visible only to its earlier declaration scope cannot be emitted at an unrelated current scope');
  Check(not NyxPascalSameStorageBinding(LBefore, LAfter, NyxPascalBindingName('Byte')),
    'Pristine builtins do not acquire declared-layout correspondence');
  LGeneric := Inside('TGeneric');
  Check(not NyxPascalSameStorageBinding(LGeneric, LGeneric, NyxPascalBindingName('TElement')),
    'A generic owner cannot acquire an unspecialized storage correspondence');
  LOtherWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LView);
  Check(not NyxPascalSameStorageBinding(LBefore, LOtherWorld.At(0, 0, LView.Syntax.Lexical.Scalars),
    NyxPascalBindingName('TAlias')), 'Identical ordinals in another world do not establish source identity');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LRejected := False;
  try NyxPascalSameStorageBinding(LBefore, LAfter, NyxPascalBindingName('TAlias'), LCancel);
  except on ENyxPascalAnalysisCancelled do LRejected := True; end;
  Check(LRejected, 'Cancelled binding proof returns no affirmative correspondence');
  LBudget := NewNyxPascalAnalysisBudget(1);
  LRejected := False;
  try
    try NyxPascalSameStorageBinding(LBefore, LAfter, NyxPascalBindingName('TAlias'), LBudget);
    except on ENyxPascalAnalysisWorkLimit do LRejected := True; end;
    Check(LRejected and (LBudget.Receipt.Total = 1), 'Binding correspondence admits before its unchanged work ceiling');
  finally LBudget.Retire; end;
end;

procedure MacroCorrespondence;
var
  LRoot: INyxPascalCompilerContext;
  LOwner: INyxFPCCompilerCapture;
  LFiles: INyxPascalFileCatalog;
  LModules: INyxPascalModuleCatalog;
  LWorld: INyxPascalSemanticWorld;
  LQuery: INyxPascalSemanticTranslation;
  LRequests: INyxPascalCompilerRequirements;
  LAt, LCase: Integer;
  LMacroName, LTail: TNyxText;
  LSafe: Boolean;
begin
  LRoot := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmFPC, pcsoSystemBootstrap)
    .WithSymbol(NyxPascalDefine('FPC'), ppdDefined).WithOption('H', ppkFalse);
  LOwner := Capture(LRoot, LRoot);
  LModules := SourceModules(LOwner);
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults.WithImplicitScopes(pispCompilerUnits));
  for LCase := 0 to 5 do
  begin
    LTail := '';
    case LCase of
      0: LMacroName := 'TPair';
      1: LMacroName := 'sizeof';
      2: LMacroName := 'div';
      3: LMacroName := 'mod';
      4: LMacroName := 'or';
      5: begin LMacroName := 'TPair'; LTail := '{$macro off}'; end;
    end;
    LFiles := SourceFiles.WithFile(NyxPascalPhysicalFile('fixture/payload.inc'),
      'type TPair=packed record First:Byte; end; TChanged=packed record First:Word; end;'#10 +
      'const PairBytes=sizeof(TPair); {$macro on}{$define ' + LMacroName + ':=TChanged}' + LTail + #10 +
      '{$if PairBytes>0}const Ready=1;{$endif}');
    LQuery := SourceQuery(LFiles, LModules, LWorld);
    LRequests := CaptureNyxPascalRequirements(LQuery);
    LSafe := LQuery.Pending;
    for LAt := 0 to LRequests.RequirementCount - 1 do
    begin

      if LCase < 5 then LSafe := LSafe and (LRequests.RequirementAt(LAt).Kind <> pcrSourceSize)
      else LSafe := LSafe and (LRequests.RequirementAt(LAt).Kind = pcrSourceSize);
    end;

    if LCase = 5 then LSafe := LSafe and (LRequests.RequirementCount = 1);
    Check(LSafe, 'Current captured macro activity governs safe native reference/intrinsic/operator spelling');
  end;
end;

procedure SourceWitnessPortable;
var
  LFiles, LChanged: INyxPascalFileCatalog;
  LModules: INyxPascalModuleCatalog;
  LWorld: INyxPascalSemanticWorld;
  LQuery: INyxPascalSemanticTranslation;
  LRoot: INyxPascalCompilerContext;
  LOwner, LReply: INyxFPCCompilerCapture;
  LRequest: INyxPascalCompilerRequirement;
  LSource: INyxPascalSourceSizeRequirement;
  LWitness: INyxPascalSourceWitness;
  LPlan: INyxPascalCompilerFulfillmentPlan;
  LApplied: INyxPascalCompilerFulfillment;
  LChangedModules: INyxPascalModuleCatalog;
  LChangedContext: INyxPascalCompilerContext;
  LBudget: INyxPascalAnalysisBudget;
  LOptions: TNyxPascalCompilerTraceOptions;
  LCancel: INyxCancellationScope;
  LText, LBad: TNyxText;
  LVariant: Integer;
  LRejected: Boolean;
begin
  LRoot := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmFPC, pcsoSystemBootstrap)
    .WithSymbol(NyxPascalDefine('FPC'), ppdDefined).WithOption('H', ppkFalse);
  LOwner := Capture(LRoot, LRoot);
  LFiles := SourceFiles;
  Check(NyxPascalFileCatalogIdentity(LFiles) = NyxPascalFileCatalogIdentity(CaptureNyxPascalFiles(LFiles)),
    'Detached owned captures preserve immutable input identity');
  LChanged := LFiles.WithFile(NyxPascalPhysicalFile('extra.pas'), 'unit Extra; interface implementation end.');
  Check(NyxPascalFileCatalogIdentity(LFiles) <> NyxPascalFileCatalogIdentity(LChanged),
    'A real input edit obtains a distinct identity without changing its predecessor');
  LModules := SourceModules(LOwner);
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults.WithImplicitScopes(pispCompilerUnits));
  LQuery := SourceQuery(LFiles, LModules, LWorld);
  Check(LQuery.Pending and (CaptureNyxPascalRequirements(LQuery).RequirementCount = 1), 'The first authored layout emits one typed source request');
  LRequest := CaptureNyxPascalRequirements(LQuery).RequirementAt(0);
  Check((LRequest.Kind = pcrSourceSize) and (LRequest.Reference.Name = 'tpair') and
    (LRequest.ContextState = -1), 'Source layout does not become a pristine primitive-name request');
  Supports(LRequest, INyxPascalSourceSizeRequirement, LSource);
  Check(NyxPascalCapturedSize(LOwner.Context, LRequest.Reference).State = pssUnknown,
    'No authored source name is inserted into the primitive namespace');
  LWitness := PlanNyxPascalSourceWitness(LRequest, LOwner);
  Check(not LWitness.Files.SameFiles(LFiles) and LFiles.SameFiles(SourceFiles),
    'Closed witness instrumentation leaves all original captured bytes exact');
  LOptions := TNyxPascalCompilerTraceOptions.Defaults;
  LText := SourceTranscript(LWitness, 3, 0, 1);
  LReply := LWitness.Read(LText, 1, LOptions);
  Check(NyxPascalCapturedStorage(LReply.Context, LSource.StorageKey).Bytes = 3,
    'A complete controlled source packet publishes its one structural value');
  for LVariant := 0 to 8 do
  begin
    LBad := LText;
    case LVariant of
      0: LBad := StringReplace(LText, '_BIT_0_1', '_BIT_1_1', []);
      1: LBad := StringReplace(LText, 'payload.inc(', 'foreign.inc(', []);
      2: LBad := StringReplace(LText, 'Fatal: (1018) Compilation aborted', '', []);
      3: LBad := StringReplace(LText, 'STOP', 'GUARD', []);
      4: LBad := StringReplace(LText, 'Note: (2025)', 'Hint: (2025)', []);
      5: LBad := StringReplace(LText, '(1,1)', '(0,1)', []);
      6: LBad := LText + 'Error: unexpected compiler failure'#10;
      7: LBad := StringReplace(LText, '_END', '_BEGIN', []);
      8: LBad := StringReplace(LText, '_BIT_30_0', '_BIT_30_2', []);
    end;
    LRejected := False;
    try LWitness.Read(LBad, 1, LOptions);
    except on ENyxPascalContext do LRejected := True; end;
    Check(LRejected, 'Malformed/foreign/incomplete source transcript refuses atomically');
  end;
  LRejected := False;
  try LWitness.Read(LText, 0, LOptions);
  except on ENyxPascalContext do LRejected := True; end;
  Check(LRejected, 'A successful exit cannot stand in for the exact intentional terminal status');
  LRejected := False;
  try LWitness.Read(LText, 1, LOptions.UpToBytes(1));
  except on ENyxPascalContext do LRejected := True; end;
  Check(LRejected, 'Transcript byte limits refuse without truncation');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LRejected := False;
  try LWitness.Read(LText, 1, LOptions, LCancel);
  except on ENyxPascalAnalysisCancelled do LRejected := True; end;
  Check(LRejected, 'Cancelled transcript interpretation publishes no reply');
  LBudget := NewNyxPascalAnalysisBudget(1);
  LRejected := False;
  try
    try NewNyxPascalSourceSizeRequirement(LRequest.Reference, LRequest.Source, LSource.Origin, LBudget);
    except on ENyxPascalAnalysisWorkLimit do LRejected := True; end;
    Check(LRejected and (LBudget.Receipt.Total = 1),
      'Source identity construction admits work before refusing its unchanged aggregate limit');
  finally LBudget.Retire; end;
  LPlan := PlanNyxPascalCompilerFulfillment(LFiles, LModules, LQuery,
    NewNyxPascalCompilerCaptures.WithCapture(LOwner), 1);
  LApplied := LPlan.ApplyReplies(NewNyxPascalCompilerCaptures.WithCapture(LReply));
  Check((LApplied.Jobs = 1) and (NyxPascalCapturedStorage(LOwner.Context, LSource.StorageKey).State = psfsUnknown),
    'One-fact application updates detached modules and preserves the original capture');
  LQuery := SourceQuery(LFiles, LApplied.Modules, LWorld);
  Check(LQuery.Pending and (LQuery.Answers = 1) and (CaptureNyxPascalRequirements(LQuery).RequirementCount = 1),
    'The unchanged world replay consumes the first source receipt and reaches the next layout');
  LOwner := LApplied.Captures.CaptureAt(0);
  LWitness := PlanNyxPascalSourceWitness(CaptureNyxPascalRequirements(LQuery).RequirementAt(0), LOwner);
  LRejected := False;
  try LWitness.Read(SourceTranscript(LWitness, 9, 4, 2), 1, LOptions);
  except on ENyxPascalContext do LRejected := True; end;
  Check(LRejected, 'The second native witness must revalidate the previous value in the same job');
  LReply := LWitness.Read(SourceTranscript(LWitness, 3, 4, 2), 1, LOptions);
  LPlan := PlanNyxPascalCompilerFulfillment(LFiles, LApplied.Modules, LQuery,
    LApplied.Captures, 1);
  LApplied := LPlan.ApplyReplies(NewNyxPascalCompilerCaptures.WithCapture(LReply));
  LQuery := SourceQuery(LFiles, LApplied.Modules, LWorld);
  Check(not LQuery.Pending and LQuery.Source.Resolved and (LQuery.Answers = 2),
    'Two exact source receipts resolve the whole captured conditional input');
  LChangedContext := NyxPascalWithBootstrapContext(LApplied.Modules.Entry(0).Context,
    NyxPascalBootstrapContext(LApplied.Modules.Entry(0).Context).WithOption('R', ppkTrue));
  LChangedModules := LApplied.Modules.WithModule(LApplied.Modules.Entry(0).InContext(
    LChangedContext, TNyxPascalTranslationOptions.Defaults));
  LQuery := SourceQuery(LFiles, LChangedModules, LWorld);
  Check(LQuery.Pending and (LQuery.Answers = 0), 'Changed compiler-root switches cannot reuse earlier source measurements');
  LChangedContext := LApplied.Modules.Entry(0).Context.WithOption('R', ppkTrue);
  LChangedModules := LApplied.Modules.WithModule(LApplied.Modules.Entry(0).InContext(
    LChangedContext, TNyxPascalTranslationOptions.Defaults));
  LQuery := SourceQuery(LFiles, LChangedModules, LWorld);
  Check(LQuery.Pending and (LQuery.Answers = 0), 'Changed source initial switches cannot reuse earlier source measurements');
  LQuery := SourceQuery(LChanged, LApplied.Modules, LWorld);
  Check(LQuery.Pending and (LQuery.Answers = 0), 'Changed input membership cannot consume an earlier source receipt');
  LRejected := False;
  try PlanNyxPascalCompilerFulfillment(LChanged, LModules, NewNyxPascalRequirements(LRequest),
    NewNyxPascalCompilerCaptures.WithCapture(Capture(LRoot, LRoot)), 1);
  except on ENyxPascalContext do LRejected := True; end;
  Check(LRejected, 'Source request preflight rejects changed files before any native effect');
end;

procedure Portable;
var
  LRoot: INyxPascalCompilerContext;
  LCaptures: INyxPascalCompilerCaptures;
  LReplies: INyxPascalCompilerCaptures;
  LPlan: INyxPascalCompilerFulfillmentPlan;
  LResult: INyxPascalCompilerFulfillment;
  LFixture: TFixture;
  LFact: INyxPascalCompilerContext;
  LRejected: Boolean;
  LCancel: INyxCancellationScope;
begin
  LRoot := NewNyxPascalCompilerContext(ppcFPC, TNyxPascalCompilerVersion.Create(3, 3, 1),
    ppmFPC, pcsoSystemBootstrap).WithOption('H', ppkFalse);
  LFixture := TFixture.Create(LRoot);
  try
    LCaptures := NewNyxPascalCompilerCaptures.WithCapture(Capture(LRoot, LRoot));
    LPlan := PlanNyxPascalCompilerFulfillment(LFixture.Files, LFixture.Modules,
      LFixture.Requests, LCaptures, 2);
    Check((LPlan.Count = 2) and (LFixture.Requests.RequirementCount = 3),
      'Exact duplicate typed requirements share one job without changing the original response');
    Check((LPlan.RequirementAt(0).Reference.Name = NyxPascalSizeReference('Word').Name) and
      (LPlan.RequirementAt(1).Reference.Name = NyxPascalSizeReference('Longint').Name),
      'Binding-derived requests retain source order and exact primitive identity');
    LFact := NyxPascalWithCapturedSize(LRoot, LPlan.RequirementAt(0).Reference,
      NyxPascalSizeFact(pssKnown, ppmFPC, ppkFalse, 2));
    LReplies := NewNyxPascalCompilerCaptures.WithCapture(Capture(LFact, LRoot));
    LFact := NyxPascalWithCapturedSize(LRoot, LPlan.RequirementAt(1).Reference,
      NyxPascalSizeFact(pssKnown, ppmFPC, ppkFalse, 4));
    LReplies := LReplies.WithCapture(Capture(LFact, LRoot));
    LResult := LPlan.ApplyReplies(LReplies);
    Check((LResult.Jobs = 2) and (LResult.Captures.Count = 1),
      'Independent one-fact replies merge into one captured owner atomically');
    Check((NyxPascalCapturedSize(NyxPascalBootstrapContext(LResult.Modules.Entry(0).Context),
      NyxPascalSizeReference('Word')).Bytes = 2) and
      (NyxPascalCapturedSize(NyxPascalBootstrapContext(LResult.Modules.Entry(1).Context),
        NyxPascalSizeReference('Longint')).Bytes = 4),
      'All registered modules sharing the exact old bootstrap packet receive both facts');
    Check((LResult.Modules.Entry(1).Context.Mode = ppmDelphi) and
      LResult.Modules.Entry(2).Context.SameFacts(LFixture.Modules.Entry(2).Context),
      'Per-module modes and unrelated bootstrap owners remain exact');
    Check((LResult.Captures.Find(LRoot) = -1) and
      (LCaptures.Find(LRoot) = 0) and
      (NyxPascalCapturedSize(LRoot, NyxPascalSizeReference('Word')).State = pssUnknown),
      'Output captures advance while original capture/context snapshots remain independent');
    LReplies := NewNyxPascalCompilerCaptures.WithCapture(LReplies.CaptureAt(0))
      .WithCapture(Capture(LFact.WithSymbol(NyxPascalDefine('UNRELATED'), ppdDefined), LRoot));
    LRejected := False;
    try
      LResult := LPlan.ApplyReplies(LReplies);
    except
      on ENyxPascalContext do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LResult.Jobs = 2), 'A late unrelated reply delta preserves the last complete result');
    LRejected := False;
    try
      PlanNyxPascalCompilerFulfillment(LFixture.Files, LFixture.Modules,
        LFixture.Requests, LCaptures, 1);
    except
      on ENyxPascalContext do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'Unique jobs obey the explicit batch allowance before delegation');
    LRejected := False;
    try
      PlanNyxPascalCompilerFulfillment(LFixture.Files, LResult.Modules,
        LFixture.Requests, LCaptures, 2);
    except
      on ENyxPascalContext do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'An old source/capture requirement cannot apply to an advanced module context');
    LRejected := False;
    try
      PlanNyxPascalCompilerFulfillment(LFixture.Files, LFixture.Modules, LFixture.Requests,
        NewNyxPascalCompilerCaptures, 2);
    except
      on ENyxPascalContext do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'An unregistered exact compiler owner refuses before delegated work');
    LCancel := NewNyxCancellationScope;
    LCancel.Cancel;
    LRejected := False;
    try
      LResult := LPlan.ApplyReplies(LReplies, LCancel);
    except
      on ENyxPascalAnalysisCancelled do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LResult.Jobs = 2), 'Cancellation preserves the last complete fulfilled result');
    LPlan := PlanNyxPascalCompilerFulfillment(LFixture.Files, LFixture.Modules, nil, LCaptures, 1);
    Check((LPlan.Count = 0) and (LPlan.ApplyReplies(NewNyxPascalCompilerCaptures).Jobs = 0),
      'An absent request capability is empty, without inferred compiler work');
  finally
    LFixture.Free;
  end;
end;

procedure PristineAndLocal;
var
  LContext: INyxPascalCompilerContext;
  LFiles: INyxPascalFileCatalog;
  LModules: INyxPascalModuleCatalog;
  LView: INyxPascalTranslatedSyntax;
  LWorld: INyxPascalSemanticWorld;
  LValue: INyxPascalConstantResult;
  LCaptures: INyxPascalCompilerCaptures;
  LPlan: INyxPascalCompilerFulfillmentPlan;
  LResult: INyxPascalCompilerFulfillment;
  LRejected: Boolean;
begin
  LContext := NewNyxPascalCompilerContext(ppcFPC, TNyxPascalCompilerVersion.Create(3, 3, 1),
    ppmDelphi).WithOption('H', ppkTrue);
  LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('plain.pas'),
    'unit Plain; interface var Value:Word; implementation end.');
  LModules := NewNyxPascalModuleCatalog.WithModule(NewNyxPascalModuleSource(
    NyxPascalModule('Plain'), NyxPascalPhysicalFile('plain.pas'))
    .InContext(LContext, TNyxPascalTranslationOptions.Defaults));
  LView := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFiles, LModules.Entry(0).Root,
    LContext, TNyxPascalTranslationOptions.Defaults), TNyxPascalSyntaxOptions.Defaults);
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LView);
  LValue := PlanNyxPascalConstant('High(Value)', TNyxPascalConstantOptions.Defaults)
    .Evaluate(LWorld.At(0, 0, LView.Syntax.Lexical.Scalars), TNyxPascalConstantOptions.Defaults);
  LCaptures := NewNyxPascalCompilerCaptures.WithCapture(Capture(LContext, LContext));
  LPlan := PlanNyxPascalCompilerFulfillment(LFiles, LModules, LValue, LCaptures, 1);
  LResult := LPlan.ApplyReplies(NewNyxPascalCompilerCaptures.WithCapture(Capture(
    NyxPascalWithCapturedSize(LContext, NyxPascalSizeReference('Word'),
      NyxPascalSizeFact(pssKnown, ppmDelphi, ppkTrue, 2)), LContext)));
  Check((LResult.Jobs = 1) and (NyxPascalCompilerScope(LResult.Modules.Entry(0).Context) = pcsoPristine),
    'Ordinary pristine owners retain their scope without acquiring bootstrap identity');
  LFiles := LFiles.WithFile(NyxPascalPhysicalFile('plain.pas'),
    'unit Plain; interface {$H-}var Value:Word; implementation end.');
  LView := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFiles, LModules.Entry(0).Root,
    LContext, TNyxPascalTranslationOptions.Defaults), TNyxPascalSyntaxOptions.Defaults);
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LView);
  LValue := PlanNyxPascalConstant('High(Value)', TNyxPascalConstantOptions.Defaults)
    .Evaluate(LWorld.At(0, 0, LView.Syntax.Lexical.Scalars), TNyxPascalConstantOptions.Defaults);
  LRejected := False;
  try
    PlanNyxPascalCompilerFulfillment(LFiles, LModules, LValue, LCaptures, 1);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'A source-local H change cannot be promoted to pristine compiler defaults');
end;

{$ifndef PAS2JS}
procedure NativeSource(const ACompiler, AOutput: TNyxText);
var
  LOwner: INyxFPCCompilerCapture;
  LFiles: INyxPascalFileCatalog;
  LModules: INyxPascalModuleCatalog;
  LCaptures: INyxPascalCompilerCaptures;
  LWorld: INyxPascalSemanticWorld;
  LQuery: INyxPascalSemanticTranslation;
  LResult: INyxPascalCompilerFulfillment;
  LSource: INyxPascalSourceSizeRequirement;
  LAt, LExpected: Integer;
  LRejected: Boolean;
  LBadOwner: INyxFPCCompilerCapture;
begin
  LOwner := CaptureNyxFPCCompilerCapture(ACompiler, AOutput + '/source-owner',
    NewNyxFPCSystemBootstrapProbe(NyxPascalDefine('SOURCE_OWNER')),
    TNyxPascalCompilerTraceOptions.Defaults);
  LFiles := SourceFiles;
  LModules := SourceModules(LOwner);
  LCaptures := NewNyxPascalCompilerCaptures.WithCapture(LOwner);
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults.WithImplicitScopes(pispCompilerUnits));
  for LAt := 0 to 1 do
  begin
    LQuery := SourceQuery(LFiles, LModules, LWorld);
    Check(LQuery.Pending and (CaptureNyxPascalRequirements(LQuery).RequirementCount = 1), 'The real source compiler receives only the current layout request');
    Supports(CaptureNyxPascalRequirements(LQuery).RequirementAt(0), INyxPascalSourceSizeRequirement, LSource);
    LResult := FulfillNyxFPCCompilerRequirements(ACompiler, AOutput + '/source-' + TNyxText(IntToStr(LAt)),
      LFiles, LModules, LQuery, LCaptures, 1, TNyxPascalCompilerTraceOptions.Defaults);
    LExpected := SizeOf(TPackedPair);

    if LAt > 0 then LExpected := SizeOf(TPackedEnvelope);
    Check((LResult.Jobs = 1) and (NyxPascalCapturedStorage(LResult.Captures.CaptureAt(0).Context,
      LSource.StorageKey).Bytes = LExpected), 'Actual source layout agrees with the independent compiled native oracle');
    LModules := LResult.Modules;
    LCaptures := LResult.Captures;
  end;
  LQuery := SourceQuery(LFiles, LModules, LWorld);
  Check(LQuery.Source.Resolved and not LQuery.Pending and (LQuery.Answers = 2),
    'Actual native measurements flow through existing modules into the unchanged source world');
  LBadOwner := Capture(LOwner.Context.WithSymbol(NyxPascalDefine('NYX_TEST_MISSING_COMPILER_INPUT'),
    ppdDefined), LOwner.ObservedDefaults);
  LModules := SourceModules(LBadOwner);
  LQuery := SourceQuery(LFiles, LModules, LWorld);
  LRejected := False;
  try
    FulfillNyxFPCCompilerRequirements(ACompiler, AOutput + '/source-refused-guard',
      LFiles, LModules, LQuery, NewNyxPascalCompilerCaptures.WithCapture(LBadOwner),
      1, TNyxPascalCompilerTraceOptions.Defaults);
  except on ENyxPascalContext do LRejected := True; end;
  Check(LRejected and (NyxPascalCapturedSize(LBadOwner.Context, NyxPascalSizeReference('TPair')).State = pssUnknown),
    'A real mismatched compiler-input guard refuses, joins its compiler and publishes no source/primitive reply');
end;

procedure Native(const ACompiler, AOutput: TNyxText);
var
  LOwner: INyxFPCCompilerCapture;
  LCaptures: INyxPascalCompilerCaptures;
  LResult: INyxPascalCompilerFulfillment;
  LFixture: TFixture;
  LCancel: INyxCancellationScope;
  LRejected: Boolean;
begin
  LOwner := CaptureNyxFPCCompilerCapture(ACompiler, AOutput + '/owner',
    NewNyxFPCSystemBootstrapProbe(NyxPascalDefine('BATCH_OWNER')),
    TNyxPascalCompilerTraceOptions.Defaults);
  LFixture := TFixture.Create(LOwner.Context);
  try
    LCaptures := NewNyxPascalCompilerCaptures.WithCapture(LOwner);
    LRejected := False;
    try
      FulfillNyxFPCCompilerRequirements(ACompiler, AOutput + '/over-limit', LFixture.Files,
        LFixture.Modules, LFixture.Requests, LCaptures, 1, TNyxPascalCompilerTraceOptions.Defaults);
    except
      on ENyxPascalContext do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and not DirectoryExistsUTF8(AOutput + '/over-limit'),
      'Native batch preflight creates no directory when the job allowance is insufficient');
    LCancel := NewNyxCancellationScope;
    LCancel.Cancel;
    LRejected := False;
    try
      FulfillNyxFPCCompilerRequirements(ACompiler, AOutput + '/cancelled', LFixture.Files,
        LFixture.Modules, LFixture.Requests, LCaptures, 2,
        TNyxPascalCompilerTraceOptions.Defaults, LCancel);
    except
      on ENyxPascalAnalysisCancelled do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and not DirectoryExistsUTF8(AOutput + '/cancelled'),
      'Cancelled native preflight creates no batch directory or compiler work');
    LResult := FulfillNyxFPCCompilerRequirements(ACompiler, AOutput + '/facts', LFixture.Files,
      LFixture.Modules, LFixture.Requests, LCaptures, 2, TNyxPascalCompilerTraceOptions.Defaults);
    Check((LResult.Jobs = 2) and DirectoryExistsUTF8(AOutput + '/facts/fact-1') and
      DirectoryExistsUTF8(AOutput + '/facts/fact-2'),
      'The configured native compiler owns exactly two joined primitive jobs');
    Check((NyxPascalCapturedSize(LResult.Captures.CaptureAt(0).Context,
      NyxPascalSizeReference('Word')).Bytes = SizeOf(Word)) and
      (NyxPascalCapturedSize(LResult.Captures.CaptureAt(0).Context,
        NyxPascalSizeReference('Longint')).Bytes = SizeOf(Longint)),
      'Actual bootstrap measurements match the independent compiled native type oracle');
    Check((NyxPascalCapturedSize(LOwner.Context, NyxPascalSizeReference('Longint')).State = pssUnknown) and
      (NyxPascalBootstrapContext(LResult.Modules.Entry(1).Context).SameFacts(
        LResult.Captures.CaptureAt(0).Context)),
      'Native replies propagate to shared owners while retaining the exact previous capture');
    Check(FulfillNyxFPCCompilerRequirements('', AOutput + '/empty', LFixture.Files,
      LFixture.Modules, nil, LCaptures, 1, TNyxPascalCompilerTraceOptions.Defaults).Jobs = 0,
      'An empty native batch works without a configured compiler or target');
    Check(not DirectoryExistsUTF8(AOutput + '/empty'), 'Empty native fulfillment creates no scratch directory');
  finally
    LFixture.Free;
  end;
end;
{$endif}

begin
  try
    Portable;
    PristineAndLocal;
    SourceWitnessPortable;
    BindingCorrespondence;
    MacroCorrespondence;
    {$ifndef PAS2JS}
    Check(ParamCount = 2, 'Supply configured compiler and fresh native scratch root');
    ForceDirectoriesUTF8(ParamStr(2));
    Native(ParamStr(1), ParamStr(2));
    NativeSource(ParamStr(1), ParamStr(2));
    {$endif}
    WriteLn('PASS ', GChecks, ' atomic compiler fulfillment checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
