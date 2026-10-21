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
  LIdentity: INyxPascalConstantStorageIdentity;
  LContext: INyxPascalCompilerContext;
  LAnswer: INyxPascalConstantResult;
  LText: TNyxText;
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
  LText := NyxDecodeUTF8(LBytes);
  LStorage := Storage(Site(LText, Facts), 'sizeof(TInheritedPacking)');
  Check((LStorage.StoragePlan.Shape.Alignment = psa1) and
    (LStorage.StoragePlan.Shape.Form = psfOrdinary) and
    (LStorage.StoragePlan.Shape.Count = 2) and
    (LStorage.StorageDefinition(2).Declaration.Name = 'TInheritedPacking'),
    'The whole original compiler input retains its actual record and pre-MODE packing owner');
  Check(Supports(LStorage, INyxPascalConstantStorageIdentity, LIdentity),
    'A complete source-bound shape exposes its separate structural receipt');
  LContext := NyxPascalWithCapturedStorage(Facts, LIdentity.StorageKey,
    NyxPascalStorageFact(psfsKnown, PackingMeasuredBytes));
  LAnswer := Evaluate(Site(LText, LContext), 'sizeof(TInheritedPacking)');
  Check((LAnswer.Issue = pbiNone) and (LAnswer.Value.Kind = pvvInteger) and
    (LAnswer.Value.Number.ToText = TNyxText(IntToStr(PackingMeasuredBytes))),
    'The actual compiled handwritten declaration supplies the matching source sizeof value');
  Check(NyxPascalCapturedSize(LContext, NyxPascalSizeReference('TInheritedPacking')).State = pssUnknown,
    'A measured source record never becomes a pristine name-keyed primitive fact');
  LStorage := Storage(Site(LText, Facts), 'sizeof(TInheritedPackingArray)');
  Supports(LStorage, INyxPascalConstantStorageIdentity, LIdentity);
  LContext := NyxPascalWithCapturedStorage(Facts, LIdentity.StorageKey,
    NyxPascalStorageFact(psfsKnown, PackingArrayMeasuredBytes));
  LAnswer := Evaluate(Site(LText, LContext), 'sizeof(TInheritedPackingArray)');
  Check((LAnswer.Issue = pbiNone) and
    (LAnswer.Value.Number.ToText = TNyxText(IntToStr(PackingArrayMeasuredBytes))),
    'The actual compiler supplies the array size without analyzer layout arithmetic');
end;
{$endif}

procedure StructuralEvidence;
const
  CSource: TNyxText = 'unit Measure; interface {$packrecords 1} ' +
    'type TTarget=record Value:Byte; end; implementation end.';
var
  LStorage: INyxPascalConstantStorage;
  LIdentity: INyxPascalConstantStorageIdentity;
  LKey: TNyxPascalStorageKey;
  LContext, LMissing, LOther: INyxPascalCompilerContext;
  LValues: INyxPascalContextStorage;
  LAnswer: INyxPascalConstantResult;
  LRequests: INyxPascalCompilerRequirements;
  LFiles: INyxPascalFileCatalog;
  LSemantic: INyxPascalSemanticTranslation;
  LAt: Integer;
  LExpression: TNyxText;
  LFailed: Boolean;
begin
  LStorage := Storage(Site(CSource, Facts), 'sizeof(TTarget)');
  Check(Supports(LStorage, INyxPascalConstantStorageIdentity, LIdentity),
    'Complete pending storage has a checked detached identity');
  LKey := LIdentity.StorageKey;
  { Controlled metadata exercises consumption, not native host fulfillment.
    The separate whole handwritten input supplies actual compiled evidence. }
  LContext := NyxPascalWithCapturedStorage(Facts, LKey, NyxPascalStorageFact(psfsKnown, 1));
  LAnswer := Evaluate(Site(CSource, LContext), 'sizeof(TTarget)');
  Check((LAnswer.Issue = pbiNone) and (LAnswer.Value.Number.ToText = '1') and
    (LAnswer.DependencyCount > 0), 'A matching receipt answers only after actual source binding');
  Check(CaptureNyxPascalCompilerContext(LContext).SameFacts(LContext) and
    not Facts.SameFacts(LContext), 'Structural evidence participates in detached capture and exact identity');
  Supports(LContext, INyxPascalContextStorage, LValues);
  Check((LValues.StorageCount = 1) and LValues.StorageKey(0).SameKey(LKey) and
    (NyxPascalCapturedStorage(Facts, LKey).State = psfsUnknown),
    'The independent parent context acquires no structural evidence');
  Check((NyxPascalCapturedStorage(LContext.UnknownState, LKey).State = psfsUnknown) and
    (NyxPascalCapturedSize(LContext, NyxPascalSizeReference('TTarget')).State = pssUnknown),
    'Unknown effects revoke structural evidence and user type names stay out of primitive facts');
  LOther := RestoreNyxPascalLocalSettings(LContext.WithOption('R', ppkTrue), Facts);
  Check(NyxPascalCapturedStorage(LOther, LKey).State = psfsKnown,
    'Local settings restoration does not discard independent captured evidence');
  LAnswer := Evaluate(Site(StringReplace(String(CSource), '{$packrecords 1}',
    '{$packrecords 2}', []), LContext), 'sizeof(TTarget)');
  Check((LAnswer.Value.Kind = pvvUnknown) and (LAnswer.Issue = pbiCompilerContext),
    'Different declaration packing cannot reuse a structurally similar receipt');
  LAnswer := Evaluate(Site(StringReplace(String(CSource), '{$packrecords 1}', '', []),
    LContext), 'sizeof(TTarget)');
  Check((LAnswer.Value.Kind = pvvUnknown) and (LAnswer.Issue = pbiCompilerContext),
    'An earlier known receipt cannot supply an uncaptured initial packing policy');
  LAnswer := Evaluate(Site(CSource, LContext.WithOption('R', ppkTrue)), 'sizeof(TTarget)');
  Check(LAnswer.Value.Kind = pvvUnknown, 'Different captured context facts invalidate the exact receipt');
  LOther := NyxPascalWithCapturedStorage(Facts, LKey, NyxPascalStorageFact(psfsOutOfRange));
  LAnswer := Evaluate(Site(CSource, LOther), 'sizeof(TTarget)');
  Check((LAnswer.Value.Kind = pvvUnknown) and (LAnswer.Issue = pbiCompilerContext),
    'An out-of-range observation supplies no truncated source integer');
  LOther := NyxPascalWithCodeAlignment(Facts,
    Default(TNyxPascalCodeAlignmentRequest).Add(pcaRecordMaximum, 8), ppkUnknown);
  LStorage := Storage(Site(CSource, LOther), 'sizeof(TTarget)');
  Supports(LStorage, INyxPascalConstantStorageIdentity, LIdentity);
  LOther := NyxPascalWithCapturedStorage(LOther, LIdentity.StorageKey,
    NyxPascalStorageFact(psfsKnown, 1));
  LAnswer := Evaluate(Site(CSource, LOther), 'sizeof(TTarget)');
  Check((LAnswer.Issue = pbiCompilerContext) and (LAnswer.Value.Kind = pvvUnknown),
    'A receipt cannot select an uncertain CODEALIGN execution branch');
  for LAt := 0 to 1 do
  begin
    LExpression := 'sizeof(TTarget)=1';

    if LAt = 1 then LExpression := 'sizeof(TTarget)+1=2';
    LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('measured.pas'),
      'unit Measure; interface {$packrecords 1} type TTarget=record Value:Byte; end; ' +
      '{$if ' + LExpression + '} const Accepted=1; {$endif} implementation end.');
    LSemantic := NewNyxPascalSemanticTranslation(LFiles, NyxPascalPhysicalFile('measured.pas'),
      LContext, NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults),
      TNyxPascalTranslationOptions.Defaults, TNyxPascalSyntaxOptions.Defaults,
      TNyxPascalConditionalOptions.Defaults);
    Check(not LSemantic.Pending and (LSemantic.Answers = 1) and LSemantic.Source.Resolved,
      'Both legacy and expression condition paths consume the exact structural value');
  end;
  LMissing := NyxPascalWithCapturedSize(Facts, NyxPascalSizeReference('Longint'),
    NyxPascalSizeFact(pssUnknown, ppmDelphi, ppkTrue));
  LStorage := Storage(Site(CSource, LMissing), 'sizeof(TTarget)');
  Supports(LStorage, INyxPascalConstantStorageIdentity, LIdentity);
  LMissing := NyxPascalWithCapturedStorage(LMissing, LIdentity.StorageKey,
    NyxPascalStorageFact(psfsKnown, 1));
  LAnswer := Evaluate(Site(CSource, LMissing), 'sizeof(TTarget)+sizeof(Longint)');
  Supports(LAnswer, INyxPascalCompilerRequirements, LRequests);
  Supports(LAnswer, INyxPascalConstantStorage, LStorage);
  Check((LAnswer.Value.Kind = pvvUnknown) and (LRequests.RequirementCount = 1) and
    (NyxPascalASCIIKey(LRequests.RequirementAt(0).Reference.Name) = 'longint') and
    (LStorage.StoragePlan = nil), 'A known record cannot hide a later missing primitive request');
  LOther := Facts;
  for LAt := 1 to NyxPascalStorageFactLimit do
  begin
    LOther := NyxPascalWithCapturedStorage(LOther,
      NyxPascalStorageKey('nyx.storage.1|fixture-' + TNyxText(IntToStr(LAt))),
      NyxPascalStorageFact(psfsKnown, LAt));
  end;
  LFailed := False;
  try
    LOther := NyxPascalWithCapturedStorage(LOther, LKey, NyxPascalStorageFact(psfsKnown, 1));
  except
    on ENyxPascalContext do LFailed := True;
  end;
  Supports(LOther, INyxPascalContextStorage, LValues);
  Check(LFailed and (LValues.StorageCount = NyxPascalStorageFactLimit),
    'An excess structural receipt refuses atomically without replacing the caller target');
end;

{ Pointer checking does not supply layout facts. It is nevertheless part of an
  exact captured context: a receipt from one request cannot answer another. }
procedure PointerPolicyIdentity;
const
  CSource: TNyxText = 'unit Measure; interface {$packrecords 1} ' +
    'type TTarget=record Value:Byte; end; implementation end.';
var
  LBase, LContext, LMeasured: INyxPascalCompilerContext;
  LStorage: INyxPascalConstantStorage;
  LIdentity: INyxPascalConstantStorageIdentity;
  LAnswer: INyxPascalConstantResult;
  LKeys: array of TNyxPascalStorageKey;
  LRequest, LOther: TNyxPascalPointerCheckingRequest;
begin
  LBase := Facts;
  SetLength(LKeys, Ord(High(TNyxPascalPointerCheckingRequest)) + 1);
  LStorage := Storage(Site(CSource, LBase), 'sizeof(TTarget)');
  Supports(LStorage, INyxPascalConstantStorageIdentity, LIdentity);
  LKeys[0] := LIdentity.StorageKey;
  for LRequest := Low(TNyxPascalPointerCheckingRequest) to High(TNyxPascalPointerCheckingRequest) do
  begin
    LContext := NyxPascalWithCompilerPolicies(LBase,
      NyxPascalCompilerPolicies(LBase).RequestPointerChecking(LRequest));
    LStorage := Storage(Site(CSource, LContext), 'sizeof(TTarget)');
    Supports(LStorage, INyxPascalConstantStorageIdentity, LIdentity);

    if LRequest = ppcrUnknown then
    begin
      Check(LIdentity.StorageKey.SameKey(LKeys[0]),
        'Unspecified pointer requests preserve the existing opaque structural identity');
    end;
    LKeys[Ord(LRequest)] := LIdentity.StorageKey;
    LMeasured := NyxPascalWithCapturedStorage(LContext, LIdentity.StorageKey,
      NyxPascalStorageFact(psfsKnown, 1));
    LAnswer := Evaluate(Site(CSource, LMeasured), 'sizeof(TTarget)');
    Check((LAnswer.Issue = pbiNone) and (LAnswer.Value.Number.ToText = '1'),
      'An explicitly supplied structural receipt still answers its exact pointer-request context');
  end;
  for LRequest := Low(TNyxPascalPointerCheckingRequest) to High(TNyxPascalPointerCheckingRequest) do
  begin
    for LOther := Low(TNyxPascalPointerCheckingRequest) to High(TNyxPascalPointerCheckingRequest) do
    begin
      LContext := NyxPascalWithCompilerPolicies(LBase,
        NyxPascalCompilerPolicies(LBase).RequestPointerChecking(LOther));
      LMeasured := NyxPascalWithCapturedStorage(LContext, LKeys[Ord(LRequest)],
        NyxPascalStorageFact(psfsKnown, 1));
      LAnswer := Evaluate(Site(CSource, LMeasured), 'sizeof(TTarget)');
      Check((LKeys[Ord(LRequest)].SameKey(LKeys[Ord(LOther)]) = (LRequest = LOther)) and
        (((LRequest = LOther) and (LAnswer.Issue = pbiNone) and
          (LAnswer.Value.Number.ToText = '1')) or
         ((LRequest <> LOther) and (LAnswer.Value.Kind = pvvUnknown) and
          (LAnswer.Issue = pbiCompilerContext))),
        'Missing/default/off/on contexts never reuse a different request identity or answer');
    end;
  end;
end;

{ Helper lookup policy grants no storage measurement. Exact measurement reuse
  must still distinguish absent, single-helper and multiple-helper contexts,
  including independently owned bootstrap policy. All sizes here are controlled
  fixture replies rather than a native compiler or layout claim. }
procedure HelperPolicyIdentity;
const
  CSource: TNyxText = 'unit Measure; interface {$packrecords 1} ' +
    'type TTarget=record Value:Byte; end; implementation end.';
var
  LBase, LContext, LMeasured, LRoot: INyxPascalCompilerContext;
  LStorage: INyxPascalConstantStorage;
  LIdentity: INyxPascalConstantStorageIdentity;
  LAnswer: INyxPascalConstantResult;
  LKeys: array[TNyxPascalKnowledge] of TNyxPascalStorageKey;
  LPolicy, LOther: TNyxPascalKnowledge;
  LBootstrap: Boolean;

  function ContextFor(AValue: TNyxPascalKnowledge): INyxPascalCompilerContext;
  begin

    if LBootstrap then
      Result := NyxPascalWithBootstrapContext(LBase,
        NyxPascalWithCompilerPolicies(LRoot,
          TNyxPascalCompilerPolicies.Unknown.WithMultipleHelpers(AValue)))
    else
      Result := NyxPascalWithCompilerPolicies(LBase,
        NyxPascalCompilerPolicies(LBase).WithMultipleHelpers(AValue));
  end;

begin
  LBase := Facts;
  LRoot := NewNyxPascalCompilerContext(ppcFPC,
    LBase.Version, ppmDelphi, pcsoSystemBootstrap).WithOption('H', ppkTrue);
  LRoot := NyxPascalWithCapturedSize(LRoot, NyxPascalSizeReference('Byte'),
    NyxPascalSizeFact(pssKnown, ppmDelphi, ppkTrue, 1));
  for LBootstrap := False to True do
  begin
    for LPolicy := Low(TNyxPascalKnowledge) to High(TNyxPascalKnowledge) do
    begin
      LContext := ContextFor(LPolicy);
      LStorage := Storage(Site(CSource, LContext), 'sizeof(TTarget)');
      Supports(LStorage, INyxPascalConstantStorageIdentity, LIdentity);
      LKeys[LPolicy] := LIdentity.StorageKey;

      if (LPolicy = ppkUnknown) and not LBootstrap then
      begin
        LStorage := Storage(Site(CSource, LBase), 'sizeof(TTarget)');
        Supports(LStorage, INyxPascalConstantStorageIdentity, LIdentity);
        Check(LIdentity.StorageKey.SameKey(LKeys[LPolicy]),
          'An explicit absent helper policy preserves the existing structural request key');
      end;
    end;
    for LPolicy := Low(TNyxPascalKnowledge) to High(TNyxPascalKnowledge) do
    begin
      for LOther := Low(TNyxPascalKnowledge) to High(TNyxPascalKnowledge) do
      begin
        LMeasured := NyxPascalWithCapturedStorage(ContextFor(LOther), LKeys[LPolicy],
          NyxPascalStorageFact(psfsKnown, 1));
        LAnswer := Evaluate(Site(CSource, LMeasured), 'sizeof(TTarget)');
        Check((LKeys[LPolicy].SameKey(LKeys[LOther]) = (LPolicy = LOther)) and
          (((LPolicy = LOther) and (LAnswer.Issue = pbiNone) and
            (LAnswer.Value.Number.ToText = '1')) or
           ((LPolicy <> LOther) and (LAnswer.Value.Kind = pvvUnknown) and
            (LAnswer.Issue = pbiCompilerContext))),
          'A measured storage value answers only the identical outer or bootstrap helper policy');
      end;
    end;
  end;
end;

procedure StructuralBounds;
var
  LContext: INyxPascalCompilerContext;
  LRoot, LOther: INyxPascalCompilerContext;
  LValues: INyxPascalContextStorage;
  LKey: TNyxPascalStorageKey;
  LFact: TNyxPascalStorageFact;
  LAt: Integer;
  LFailed: Boolean;
begin
  LFailed := False;
  try
    LKey := NyxPascalStorageKey('nyx.storage.1|');
  except
    on ENyxPascalContext do LFailed := True;
  end;
  Check(LFailed, 'An empty structural receipt refuses before publication');
  LFailed := False;
  try
    LKey := NyxPascalStorageKey('nyx.storage.1|bad'#10'key');
  except
    on ENyxPascalContext do LFailed := True;
  end;
  Check(LFailed, 'Control characters cannot enter structural identity data');
  LFact := NyxPascalStorageFact(psfsKnown, 0);
  Check((LFact.State = psfsKnown) and (LFact.Bytes = 0),
    'A measured empty record is distinct from an unknown measurement');
  LFailed := False;
  try
    LFact := NyxPascalStorageFact(psfsUnknown, 1);
  except
    on ENyxPascalContext do LFailed := True;
  end;
  Check(LFailed and (LFact.State = psfsKnown) and (LFact.Bytes = 0),
    'An invalid measurement preserves the previous value through native result aliasing');
  LContext := Facts;
  for LAt := 1 to 4 do
  begin
    LKey := NyxPascalStorageKey('nyx.storage.1|' +
      TNyxText(StringOfChar('x', NyxPascalStorageKeyLimit - 15)) + TNyxText(IntToStr(LAt)));
    Supports(LContext, INyxPascalContextStorage, LValues);
    LContext := LValues.WithStorageFact(LKey, LFact);
  end;
  Supports(LContext, INyxPascalContextStorage, LValues);
  Check(LValues.StorageCount = 4, 'Exact key-byte allowance admits four complete maximum receipts');
  LFailed := False;
  try
    LContext := LValues.WithStorageFact(NyxPascalStorageKey('nyx.storage.1|extra'), LFact);
  except
    on ENyxPascalContext do LFailed := True;
  end;
  Supports(LContext, INyxPascalContextStorage, LValues);
  Check(LFailed and (LValues.StorageCount = 4), 'Aggregate key-byte excess refuses atomically');
  LFailed := False;
  try
    LFact := LValues.StorageValueAt(4);
  except
    on ENyxPascalContext do LFailed := True;
  end;
  Check(LFailed and (LFact.Bytes = 0), 'Checked structural ordinals preserve the previous result on refusal');
  LKey := NyxPascalStorageKey('nyx.storage.1|root-overlap');
  LRoot := NyxPascalWithCapturedStorage(NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmFPC, pcsoSystemBootstrap),
    LKey, NyxPascalStorageFact(psfsKnown, 1));
  LContext := NyxPascalWithBootstrapContext(Facts, LRoot);
  LFailed := False;
  try
    LContext := NyxPascalWithCapturedStorage(LContext, LKey, NyxPascalStorageFact(psfsKnown, 2));
  except
    on ENyxPascalContext do LFailed := True;
  end;
  Check(LFailed and (NyxPascalCapturedStorage(LContext, LKey).State = psfsUnknown),
    'A conflicting module receipt cannot override its existing compiler-root observation');
  LOther := NyxPascalWithCapturedStorage(Facts, LKey, NyxPascalStorageFact(psfsKnown, 2));
  LFailed := False;
  try
    LOther := NyxPascalWithBootstrapContext(LOther, LRoot);
  except
    on ENyxPascalContext do LFailed := True;
  end;
  Check(LFailed and (NyxPascalBootstrapContext(LOther) = nil),
    'Attaching a conflicting compiler root refuses without publishing a partial context');
end;

procedure MixedStorageEvidence;
const
  CSource: TNyxText = 'unit MixedEvidence; interface {$packrecords 1} ' +
    'type TTarget=record Value:System.Byte; end; implementation end.';
var
  LRoot, LContext: INyxPascalCompilerContext;
  LStorage: INyxPascalConstantStorage;
  LIdentity: INyxPascalConstantStorageIdentity;
  LAnswer: INyxPascalConstantResult;
begin
  LRoot := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmFPC, pcsoSystemBootstrap)
    .WithOption('H', ppkFalse);
  LRoot := NyxPascalWithCapturedSize(LRoot, NyxPascalSizeReference('Byte'),
    NyxPascalSizeFact(pssKnown, ppmFPC, ppkFalse, 1));
  LContext := NyxPascalWithBootstrapContext(Facts, LRoot);
  LStorage := Storage(Site(CSource, LContext), 'sizeof(TTarget)');
  Supports(LStorage, INyxPascalConstantStorageIdentity, LIdentity);
  LRoot := NyxPascalWithCapturedStorage(LRoot, LIdentity.StorageKey,
    NyxPascalStorageFact(psfsKnown, 1));
  LContext := NyxPascalWithBootstrapContext(Facts, LRoot);
  LAnswer := Evaluate(Site(CSource, LContext), 'sizeof(TTarget)');
  Check((LAnswer.Issue = pbiNone) and (LAnswer.Value.Number.ToText = '1') and
    (LRoot.Mode = ppmFPC) and (LRoot.Option('H') = ppkFalse) and
    (LContext.Mode = ppmDelphi) and (LContext.Option('H') = ppkTrue),
    'Root-owned structural evidence retains mixed per-node mode/H without retagging a primitive');
end;

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
  LIdentity: INyxPascalConstantStorageIdentity;
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
  Check(Supports(LAnswer, INyxPascalConstantStorageIdentity, LIdentity) and
    (Length(LIdentity.StorageKey.Value) > 0) and (LScope.Calls = LCalls),
    'Published identity getters borrow validated values without deferred work or cancellation');
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
    StructuralEvidence;
    PointerPolicyIdentity;
    HelperPolicyIdentity;
    StructuralBounds;
    MixedStorageEvidence;
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
