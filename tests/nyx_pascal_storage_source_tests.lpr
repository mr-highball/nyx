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
program nyx_pascal_storage_source_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.scheduler, nyx.pascal.context, nyx.pascal.lexing,
  nyx.pascal.files, nyx.pascal.translation, nyx.pascal.syntax,
  nyx.pascal.syntax.translated, nyx.pascal.binding, nyx.pascal.constants,
  nyx.pascal.storage, nyx.pascal.values, nyx.pascal.requirements, nyx.pascal.work,
  nyx.pascal.conditions
  {$ifndef PAS2JS}, Classes, nyx.bytes, packing.mode.fixture{$endif};

type
  { Cancellation fixture counts actual reads without owning a result/world.
    A completed storage response must never poll this retired execution again. }
  TCancelAt = class(TInterfacedObject, INyxCancellationScope)
  public
    Calls, StopAt: Integer;
    Stopped: Boolean;
    procedure Cancel;
    function GetCancelled: Boolean;
  end;

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

procedure TCancelAt.Cancel;
begin Stopped := True; end;
function TCancelAt.GetCancelled: Boolean;
begin
  Inc(Calls);
  Result := Stopped or ((StopAt > 0) and (Calls >= StopAt));
end;

function Facts: INyxPascalCompilerContext;
const
  CNames: array[0..3] of String = ('Byte', 'Longint', 'Word', 'Boolean');
  CBytes: array[0..3] of Integer = (1, 4, 2, 1);
var
  LAt: Integer;
begin
  { Explicit fixture facts exercise provenance, not real target ABI capture.
    No production initial probe or native job list is added by these controls. }
  Result := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi).CompleteDefines(True)
    .WithOption('H', ppkTrue).WithFeature(ppfAdvancedRecords, ppkTrue);
  for LAt := Low(CNames) to High(CNames) do
  begin
    Result := NyxPascalWithCapturedSize(Result, NyxPascalSizeReference(CNames[LAt]),
      NyxPascalSizeFact(pssKnown, ppmDelphi, ppkTrue, CBytes[LAt]));
  end;
end;

function Site(const AText: TNyxText;
  const AContext: INyxPascalCompilerContext): INyxPascalBindingSite;
var
  LWorld: INyxPascalSemanticWorld;
  LView, LSystem: INyxPascalTranslatedSyntax;
  LFiles: INyxPascalFileCatalog;
begin
  { This controlled System fixture proves that the class's captured ancestor
    has no shadowing nested names. It is not a production SDK replacement. }
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('layout.pas'), AText)
    .WithFile(NyxPascalPhysicalFile('fixture-system.pas'),
      'unit System; interface type TObject=class end; implementation end.');
  LView := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(
    LFiles,
    NyxPascalPhysicalFile('layout.pas'), AContext, TNyxPascalTranslationOptions.Defaults),
    TNyxPascalSyntaxOptions.Defaults);
  LSystem := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFiles,
    NyxPascalPhysicalFile('fixture-system.pas'), AContext,
    TNyxPascalTranslationOptions.Defaults), TNyxPascalSyntaxOptions.Defaults);
  LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults)
    .WithModule(LView).WithModule(LSystem);
  Result := LWorld.At(0, 0, LView.Syntax.Lexical.Scalars);
end;

function Evaluate(const ASite: INyxPascalBindingSite; const AText: TNyxText;
  const AScope: INyxCancellationScope = nil): INyxPascalConstantResult;
begin
  Result := PlanNyxPascalConstant(AText, TNyxPascalConstantOptions.Defaults)
    .Evaluate(ASite, TNyxPascalConstantOptions.Defaults, AScope);
end;

function Storage(const ASite: INyxPascalBindingSite;
  const AText: TNyxText): INyxPascalConstantStorage;
var
  LAnswer: INyxPascalConstantResult;
begin
  LAnswer := Evaluate(ASite, AText);
  WriteLn('source storage / ', AText, ' / issue=', Ord(LAnswer.Issue),
    ' / related=', LAnswer.RelatedName);

  Check((LAnswer.Value.Kind = pvvUnknown) and (LAnswer.Issue = pbiCompilerContext) and
    Supports(LAnswer, INyxPascalConstantStorage, Result) and (Result.StoragePlan <> nil),
    'A complete bound shape remains pending a distinct compiler layout measurement');
end;

procedure QualifiedFields;
var
  LSite: INyxPascalBindingSite;
  LStorage: INyxPascalConstantStorage;
  LRoot: INyxPascalStorageShape;
  LFailed: Boolean;
begin
  LSite := Site('unit LayoutCase; interface {$packrecords 2} type TOuter=class ' +
    'type TNumber=Longint; TPacket=record Left,Right:TNumber; Flag:Byte; end; end; ' +
    '{$packrecords 8} type TAlias=TOuter.TPacket; implementation end.', Facts);
  LStorage := Storage(LSite, 'sizeof(TAlias)');
  LRoot := LStorage.StoragePlan.Shape;
  Check((LRoot.Kind = pskRecord) and (LRoot.Alignment = psa2) and (LRoot.Count = 3),
    'Record policy comes from the underlying nested declaration, not the alias/caller');
  Check((LRoot.Child(0).Primitive = pspLongint) and
    (LRoot.Child(1).Primitive = pspLongint) and (LRoot.Child(2).Primitive = pspByte),
    'Grouped fields bind the class-local alias independently and retain order');
  Check((LStorage.StorageNodeCount = 4) and
    (LStorage.StorageNode(3) = LRoot) and
    (LStorage.StorageDefinition(3).Declaration.Name = 'TPacket'),
    'The child-first context table ends with the actual record declaration');
  Check((LStorage.StorageIntrinsicOrigin(0) = pioUnqualified) and
    (NyxPascalASCIIKey(LStorage.StorageIntrinsicReference(0).Name) = 'longint') and
    (LStorage.StorageContext(0).Mode = ppmDelphi), 'Primitive binding identity is retained separately');
  LFailed := False;
  try
    LStorage.StorageIntrinsicReference(3);
  except
    on ENyxPascalBinding do LFailed := True;
  end;
  Check(LFailed, 'A record cannot pretend to be a name-keyed primitive measurement');
  LSite := nil;
  Check(LStorage.StorageDefinition(3).World.ModuleAt(
    LStorage.StorageDefinition(3).ModuleIndex).Source.Files.FileAt(
    LStorage.StorageDefinition(3).Origin.FileIndex).Name = 'layout.pas',
    'Retiring the caller leaves exact source navigation available');
end;

procedure ModePackingProvenance;
var
  LMode: TNyxPascalLanguageMode;
  LAlignment: TNyxPascalRecordAlignment;
  LBefore, LAfter: INyxPascalCompilerContext;
  LSite: INyxPascalBindingSite;
  LStorage: INyxPascalConstantStorage;
  LAnswer: INyxPascalConstantResult;
begin
  { A mode reset does change enum/set defaults, but FPC 3.3.1 preserves the
    authored record request. Exercise every symbolic choice without assigning
    any of them a numeric layout or filling an unknown initial/default fact. }
  for LMode := ppmFPC to ppmTP do
  begin
    for LAlignment := Low(TNyxPascalRecordAlignment) to High(TNyxPascalRecordAlignment) do
    begin
      LBefore := NyxPascalWithCompilerPolicies(Facts,
        TNyxPascalCompilerPolicies.Unknown.AlignRecords(LAlignment)
          .StoreEnums(pes2).PackSets(psp8));
      LAfter := LBefore.InMode(LMode);
      Check((LAfter.Mode = LMode) and
        (NyxPascalCompilerPolicies(LAfter).RecordAlignment = LAlignment) and
        (NyxPascalCompilerPolicies(LAfter).EnumStorage = pesUnknown) and
        (NyxPascalCompilerPolicies(LAfter).SetPacking = pspUnknown),
        'A qualified mode transition retains only the independent record packing request');
      Check(CaptureNyxPascalCompilerContext(LAfter).SameFacts(LAfter) and
        (NyxPascalCompilerPolicies(LBefore).RecordAlignment = LAlignment),
        'Mode-copy capture retains exact packing and leaves the original context untouched');
    end;
  end;
  LBefore := NyxPascalWithCompilerPolicies(Facts,
    TNyxPascalCompilerPolicies.Unknown.AlignRecords(pra2));
  Check((NyxPascalCompilerPolicies(LBefore.InMode(ppmUnknown)).RecordAlignment = praUnknown) and
    (NyxPascalCompilerPolicies(LBefore.UnknownState.InMode(ppmDelphi)).RecordAlignment = praUnknown),
    'An unknown mode/effect cannot recover an earlier packing request');
  LBefore := NyxPascalWithCompilerPolicies(NewNyxPascalCompilerContext(ppcPas2JS,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi),
    TNyxPascalCompilerPolicies.Unknown.AlignRecords(pra2));
  Check(NyxPascalCompilerPolicies(LBefore.InMode(ppmObjFPC)).RecordAlignment = praUnknown,
    'FPC source evidence grants no matching pas2js packing semantics');
  LBefore := NyxPascalWithCompilerPolicies(NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 2, 2), ppmDelphi),
    TNyxPascalCompilerPolicies.Unknown.AlignRecords(pra2));
  Check(NyxPascalCompilerPolicies(LBefore.InMode(ppmObjFPC)).RecordAlignment = praUnknown,
    'An unqualified compiler version retains the conservative reset');
  LSite := Site('unit ModePacking; {$packrecords 2}{$mode delphi} interface ' +
    'type TBefore=record Value:Byte; end; {$push}{$packrecords 8} ' +
    'type TInside=record Value:Byte; end; {$pop} ' +
    'type TAfter=record Value:Byte; end; implementation end.', Facts);
  LStorage := Storage(LSite, 'sizeof(TBefore)');
  Check(LStorage.StoragePlan.Shape.Alignment = psa2,
    'A source MODE retains packing authored before it at the record declaration');
  LStorage := Storage(LSite, 'sizeof(TInside)');
  Check(LStorage.StoragePlan.Shape.Alignment = psa8,
    'An inner authored request still overrides the retained outer request');
  LStorage := Storage(LSite, 'sizeof(TAfter)');
  Check(LStorage.StoragePlan.Shape.Alignment = psa2,
    'Local POP restores the exact request inherited across the source MODE');
  LAnswer := Evaluate(Site('unit ModeUnknown; {$mode delphi} interface ' +
    'type TTarget=record Value:Byte; end; implementation end.', Facts), 'sizeof(TTarget)');
  Check((LAnswer.Issue = pbiCompilerContext) and
    Supports(LAnswer, INyxPascalConstantStorage, LStorage) and (LStorage.StoragePlan = nil),
    'A real mode directive never supplies an uncaptured initial record packing policy');
end;

{$ifndef PAS2JS}
{ Analyze the complete exact source also compiled into this native consumer.
  Byte input is bounded and borrowed only during this synchronous inspection. }
procedure WholeModePackingInput;
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
  LStorage: INyxPascalConstantStorage;
begin

  if ParamCount < 1 then
  begin
    raise Exception.Create('Native source-storage qualification needs the repository path');
  end;
  Check(PackingPreserved,
    'The actual native compiler preserves pending PACKRECORDS through MODE');
  LStream := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(1)) +
    'tests/packing.mode.fixture.pas', fmOpenRead or fmShareDenyWrite);
  try

    if (LStream.Size < 1) or (LStream.Size > 65536) then
    begin
      raise Exception.Create('The whole packing fixture exceeds its byte budget');
    end;
    SetLength(LBytes, LStream.Size);
    LStream.ReadBuffer(LBytes[0], Length(LBytes));
  finally
    LStream.Free;
  end;
  LStorage := Storage(Site(NyxDecodeUTF8(LBytes), Facts), 'sizeof(TInheritedPacking)');
  Check((LStorage.StoragePlan.Shape.Alignment = psa1) and
    (LStorage.StoragePlan.Shape.Form = psfOrdinary) and
    (LStorage.StoragePlan.Shape.Count = 2) and
    (LStorage.StorageDefinition(2).Declaration.Name = 'TInheritedPacking'),
    'The whole original compiler input retains its actual record and pre-MODE packing owner');
end;
{$endif}

procedure ArraysAndForms;
var
  LSite: INyxPascalBindingSite;
  LStorage: INyxPascalConstantStorage;
  LRoot, LChild: INyxPascalStorageShape;
begin
  LSite := Site('unit ArrayCase; interface {$packrecords c} const First=-2; Last=1; ' +
    'type TIndex=3..4; TInner=packed record Flag:Byte; Value:Longint; end; ' +
    '{$packrecords 8} type TMatrix=array[(First+1)..(Last+1),TIndex] of TInner; ' +
    'TBits=bitpacked record Left,Right:Boolean; end; TEmpty=record end; ' +
    'TFlags=bitpacked array[0..3,1..2] of Boolean; ' +
    'TInline=record Values:array[1..2] of record Low:Byte; High:Longint; end; end; ' +
    'implementation end.', Facts);
  LStorage := Storage(LSite, 'sizeof(TMatrix)');
  LRoot := LStorage.StoragePlan.Shape;
  Check((LRoot.Kind = pskArray) and (LRoot.ArrayLength = 4) and
    (LRoot.Child(0).Kind = pskArray) and (LRoot.Child(0).ArrayLength = 2),
    'Expression and declaration-bound ordinal dimensions retain their full extents');
  LChild := LRoot.Child(0).Child(0);
  Check((LChild.Form = psfPacked) and (LChild.Alignment = psaC) and
    (LStorage.StorageContext(2).SameFacts(LStorage.StorageContext(3)) = False),
    'Nested record and array retain different declaration contexts');
  LStorage := Storage(LSite, 'sizeof(TBits)');
  Check(LStorage.StoragePlan.Shape.Form = psfBitPacked, 'Bitpacked intent is not flattened into packed');
  LStorage := Storage(LSite, 'sizeof(TEmpty)');
  Check((LStorage.StoragePlan.Shape.Count = 0) and (LStorage.StorageNodeCount = 1),
    'An empty record is complete without a fabricated field');
  LStorage := Storage(LSite, 'sizeof(TFlags)');
  Check((LStorage.StoragePlan.Shape.Form = psfBitPacked) and
    (LStorage.StoragePlan.Shape.Child(0).Form = psfBitPacked),
    'Each dimension retains the authored bitpacked array intent');
  LStorage := Storage(LSite, 'sizeof(TInline)');
  LChild := LStorage.StoragePlan.Shape.Child(0);
  Check((LChild.ArrayLength = 2) and (LChild.Child(0).Kind = pskRecord) and
    (LChild.Child(0).Count = 2), 'Anonymous structured elements retain their actual member owner');
end;

procedure NonStorageMembersAndRootIdentity;
var
  LSite: INyxPascalBindingSite;
  LStorage: INyxPascalConstantStorage;
  LRootFacts, LContext: INyxPascalCompilerContext;
begin
  LSite := Site('unit Members; interface {$packrecords 1} type &bitpacked=Byte; ' +
    'TTarget=record class var Shared:Longint; const Magic=4; ' +
    'type TUnused=record Other:Longint; end; private FValue:&bitpacked; ' +
    'public procedure Change; property Value:Byte read FValue; end; ' +
    'implementation procedure TTarget.Change; begin end; end.', Facts);
  LStorage := Storage(LSite, 'sizeof(TTarget)');
  Check((LStorage.StoragePlan.Shape.Count = 1) and
    (LStorage.StoragePlan.Shape.Child(0).Primitive = pspByte),
    'Private data participates; routines/properties/static data/nested types allocate no instance field');
  Check(LStorage.StorageDefinition(0).Declaration.Name = 'bitpacked',
    'An escaped contextual packing word stays an ordinary bound alias');
  LRootFacts := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmFPC, pcsoSystemBootstrap)
    .WithOption('H', ppkFalse);
  LRootFacts := NyxPascalWithCapturedSize(LRootFacts, NyxPascalSizeReference('Byte'),
    NyxPascalSizeFact(pssKnown, ppmFPC, ppkFalse, 1));
  LContext := NyxPascalWithBootstrapContext(Facts, LRootFacts);
  LSite := Site('unit RootIdentity; interface {$packrecords 1} type ' +
    'TTarget=record Value:System.Byte; end; implementation end.', LContext);
  LStorage := Storage(LSite, 'sizeof(TTarget)');
  Check((LStorage.StorageIntrinsicOrigin(0) = pioSystemBootstrap) and
    (NyxPascalCompilerScope(LStorage.StorageContext(0)) = pcsoSystemBootstrap) and
    (LStorage.StorageContext(0).Mode = ppmFPC) and
    (LStorage.StorageContext(0).Option('H') = ppkFalse) and
    (LStorage.StorageContext(1).Mode = ppmDelphi),
    'A root primitive retains its own mode/H/scope rather than the record caller context');
end;

procedure ConditionalPropagation;
var
  LAt: Integer;
  LExpression: TNyxText;
  LFiles: INyxPascalFileCatalog;
  LSemantic: INyxPascalSemanticTranslation;
  LStorage: INyxPascalConstantStorage;
begin
  for LAt := 0 to 1 do
  begin
    LExpression := 'sizeof(TTarget)<>0';

    if LAt = 1 then
    begin
      LExpression := 'sizeof(TTarget)+1>0';
    end;
    LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('conditional.pas'),
      'unit ConditionalCase; interface {$packrecords 2} type TTarget=record ' +
      'Value:Byte; end; {$if ' + LExpression + '} const ShouldStayUnknown=1; {$endif} ' +
      'implementation end.');
    LSemantic := NewNyxPascalSemanticTranslation(LFiles, NyxPascalPhysicalFile('conditional.pas'),
      Facts, NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults),
      TNyxPascalTranslationOptions.Defaults, TNyxPascalSyntaxOptions.Defaults,
      TNyxPascalConditionalOptions.Defaults);
    Check(LSemantic.Pending and (LSemantic.Answers = 0) and
      (LSemantic.BindingIssue = pbiCompilerContext) and
      Supports(LSemantic, INyxPascalConstantStorage, LStorage) and
      (LStorage.StoragePlan <> nil) and (LStorage.StoragePlan.Shape.Alignment = psa2),
      'Legacy and expression conditions expose the same pending source-bound storage payload');
    Check(not LSemantic.Source.Resolved,
      'A shape cannot answer a conditional or resolve source without a measured layout fact');
  end;
end;

procedure Refusals;
const
  CSources: array[0..5] of TNyxText = (
    'unit Refusal; interface type TTarget=record Value:Byte; end; implementation end.',
    'unit Refusal; interface {$packrecords 1} type TTarget=record case Boolean of ' +
      'False:(Value:Byte); True:(Other:Longint); end; implementation end.',
    'unit Refusal; interface {$packrecords 1} type TTarget=array of Byte; implementation end.',
    'unit Refusal; interface {$packrecords 1} type TTarget=record ' +
      'Value:array[0..sizeof(TTarget)] of Byte; end; implementation end.',
    'unit Refusal; interface {$packrecords 1} const Limit=sizeof(TTarget); ' +
      'type TTarget=record Value:array[0..Limit] of Byte; end; implementation end.',
    'unit Refusal; interface {$packrecords 1} type TTarget=record FixedValue:Byte; ' +
      '{$if Uncaptured} Other:Longint; {$endif} end; implementation end.');
var
  LAt: Integer;
  LAnswer: INyxPascalConstantResult;
  LStorage: INyxPascalConstantStorage;
  LContext: INyxPascalCompilerContext;
  LRequests: INyxPascalCompilerRequirements;
begin
  for LAt := Low(CSources) to High(CSources) do
  begin
    LAnswer := Evaluate(Site(CSources[LAt], Facts), 'sizeof(TTarget)');
    WriteLn('storage refusal / ', LAt, ' / issue=', Ord(LAnswer.Issue),
      ' / related=', LAnswer.RelatedName);
    Check((LAnswer.Value.Kind = pvvUnknown) and
      Supports(LAnswer, INyxPascalConstantStorage, LStorage) and
      (LStorage.StoragePlan = nil) and (LStorage.StorageNodeCount = 0),
      'Unknown packing, variants, dynamic arrays and recursive bounds publish no partial plan');

    if LAt = 0 then
    begin
      Check(LAnswer.Issue = pbiCompilerContext, 'Unknown record packing is not DEFAULT');
    end;

    if LAt = 3 then
    begin
      Check(LAnswer.Issue = pbiRecursiveType, 'SizeOf recursion shares the active source query');
    end;

    if LAt = 4 then
    begin
      Check(LAnswer.Issue = pbiMissingName, 'A bound constant cannot see a later type declaration');
    end;
  end;
  LContext := NyxPascalWithCapturedSize(Facts, NyxPascalSizeReference('Longint'),
    NyxPascalSizeFact(pssUnknown, ppmDelphi, ppkTrue));
  LAnswer := Evaluate(Site('unit Missing; interface {$packrecords 1} ' +
    'type TTarget=record Value:Longint; end; implementation end.', LContext), 'sizeof(TTarget)');
  LRequests := CaptureNyxPascalRequirements(LAnswer);
  Check((LAnswer.Issue = pbiCompilerContext) and (LRequests.RequirementCount = 1) and
    (NyxPascalASCIIKey(LRequests.RequirementAt(0).Reference.Name) = 'longint'),
    'A missing field primitive forwards the existing typed request without a fixed probe');
end;

procedure ExecutionLifetime;
var
  LSite: INyxPascalBindingSite;
  LPlan: INyxPascalConstantPlan;
  LAnswer, LRejected: INyxPascalConstantResult;
  LStorage: INyxPascalConstantStorage;
  LScope: TCancelAt;
  LLease: INyxCancellationScope;
  LBudget: INyxPascalAnalysisBudget;
  LCalls, LAt, LStop: Integer;
  LFailed: Boolean;
begin
  LSite := Site('unit Execution; interface {$packrecords 4} type TTarget=record ' +
    'Values:array[0..2] of Byte; end; implementation end.', Facts);
  LPlan := PlanNyxPascalConstant('sizeof(TTarget)', TNyxPascalConstantOptions.Defaults);
  LAnswer := LPlan.Evaluate(LSite, TNyxPascalConstantOptions.Defaults);
  LScope := TCancelAt.Create;
  LLease := LScope;
  LAnswer := LPlan.Evaluate(LSite, TNyxPascalConstantOptions.Defaults, LLease);
  LCalls := LScope.Calls;
  LScope.Cancel;
  Check(Supports(LAnswer, INyxPascalConstantStorage, LStorage) and
    (LStorage.StoragePlan.Shape.Child(0).ArrayLength = 3) and
    (LStorage.StorageNodeCount = 3) and (LScope.Calls = LCalls),
    'Published storage getters do no work through their retired cancellation scope');
  for LAt := 0 to 3 do
  begin
    LScope := TCancelAt.Create;
    LLease := LScope;
    LStop := 1;

    if LAt > 0 then
    begin
      LStop := (LCalls * LAt) div 3;
    end;
    LScope.StopAt := LStop;
    LRejected := nil;
    LFailed := False;
    try
      LRejected := LPlan.Evaluate(LSite, TNyxPascalConstantOptions.Defaults, LLease);
    except
      on ENyxPascalAnalysisCancelled do LFailed := True;
    end;
    Check(LFailed and (LRejected = nil) and (LStorage.StorageNodeCount = 3),
      'Early/middle/late cancellation refuses the whole query and preserves the prior result');
  end;
  LBudget := NewNyxPascalAnalysisBudget(1);
  LRejected := nil;
  LFailed := False;
  try
    try
      LRejected := LPlan.Evaluate(LSite, TNyxPascalConstantOptions.Defaults, LBudget);
    except
      on ENyxPascalAnalysisWorkLimit do LFailed := True;
    end;
    Check(LFailed and (LRejected = nil) and (LBudget.Receipt.Total <= 1),
      'Aggregate admission refuses before publishing a partially captured storage response');
  finally
    LBudget.Retire;
  end;
end;

begin
  try
    QualifiedFields;
    ModePackingProvenance;
    {$ifndef PAS2JS}WholeModePackingInput;{$endif}
    ArraysAndForms;
    NonStorageMembersAndRootIdentity;
    ConditionalPropagation;
    Refusals;
    ExecutionLifetime;
    WriteLn('PASS ', GChecks, ' declaration-bound storage checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
