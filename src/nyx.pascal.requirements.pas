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
unit nyx.pascal.requirements;
{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.bytes, nyx.scheduler, nyx.pascal.work, nyx.pascal.context, nyx.pascal.lexing,
  nyx.pascal.syntax.translated, nyx.pascal.translation, nyx.pascal.files,
  nyx.pascal.encoding;

type
  { Closed compiler evidence request, independent of diagnostic text. A primitive
    size query establishes presence/absence and size in one exact compiler scope;
    it never asks the pristine host to bind an authored alias or record layout. }
  TNyxPascalCompilerRequirementKind = (pcrPrimitiveSize, pcrSourceSize);
  { Immutable missing fact with separate context owner and observing source.
    ContextState=-1 selects the owner's independent bootstrap packet; other
    ordinals select an exact retained translation state. SourceOffset is virtual
    and may be EOF; OriginIndex=-1 honestly reports no mapped physical token.
    All leases point outward to frozen source/context, never an editor or query. }
  INyxPascalCompilerRequirement = interface(IInterface)
    ['{7BA1AE9A-2728-40F8-B575-245022BA2B82}']
    function Kind: TNyxPascalCompilerRequirementKind;
    function Reference: TNyxPascalSizeReference;
    function Context: INyxPascalCompilerContext;
    function OwnerSource: INyxPascalTranslatedSyntax;
    function ContextState: Integer;
    function Source: INyxPascalTranslatedSyntax;
    function SourceOffset: Integer;
    function OriginIndex: Integer;
  end;
  { An exact source-conditional witness request, separate from pristine named
    primitives. Key binds owned inputs, physical occurrence/site and captured
    compiler inputs. Native execution must compile those source bytes in scope;
    this metadata neither supplies a packing default nor grants execution. }
  INyxPascalSourceSizeRequirement = interface(IInterface)
    ['{F62A3BF5-0C92-4263-9029-379135D1082C}']
    function Origin: TNyxPascalConditionOrigin;
    function StorageKey: TNyxPascalStorageKey;
  end;
  { Optional read-only response capability. Exact first-use order and at most
    64 complete requirements are captured; missing capability means no request,
    never implicit permission to infer requirements from an English message. }
  INyxPascalCompilerRequirements = interface(IInterface)
    ['{259C234F-EE06-42DD-9F00-022F43437A1C}']
    function RequirementCount: Integer;
    function RequirementAt(AIndex: Integer): INyxPascalCompilerRequirement;
  end;

{ Freeze context and retain exact immutable source leases. The selected owner
  state must equal that context, and the requested primitive fact must be unknown.
  Invalid provenance/ranges/identity refuses before publishing a requirement. }
function NewNyxPascalPrimitiveRequirement(const AContext: INyxPascalCompilerContext;
  const AReference: TNyxPascalSizeReference; const AOwner, ASource: INyxPascalTranslatedSyntax;
  AContextState, ASourceOffset: Integer): INyxPascalCompilerRequirement;
{ Freeze a supported bound source witness at its actual unresolved condition.
  Compiler-root System owns the first native path; other roots remain explicit
  prerequisites. No user type name enters the primitive-size namespace. }
function NewNyxPascalSourceSizeRequirement(const AReference: TNyxPascalSizeReference;
  const ASource: INyxPascalTranslatedSyntax;
  const AOrigin: TNyxPascalConditionOrigin;
  const ACancellation: INyxCancellationScope = nil): INyxPascalCompilerRequirement;
{ First native source path requires a compiler-root System prefix without
  imported units or authored initial policy overrides. Unknown defaults remain
  unknown and are measured in the actual source; unsupported startup requests
  retain the ordinary structural prerequisite instead of being silently ignored. }
function NyxPascalSourceWitnessContext(const ASource: INyxPascalTranslatedSyntax): Boolean;
{ Check the captured pre-conditional replacement environment before rendering
  a named native expression. An earlier effective name must not be redirected
  by a later macro, nor may the witness's intrinsic/operator words be replaced.
  This inspects captured translation data only, never the host configuration. }
function NyxPascalSourceSizeReferenceSafe(const ASource: INyxPascalTranslatedSyntax;
  const AName: TNyxPascalSizeReference;
  const ACancellation: INyxCancellationScope = nil): Boolean;
{ Freeze an alternate requirement once; no producer/callback survives capture. }
function CaptureNyxPascalRequirement(const ASource: INyxPascalCompilerRequirement): INyxPascalCompilerRequirement;
{ An empty or one-requirement owned response. Nil explicitly means empty. }
function NewNyxPascalRequirements(const ARequirement: INyxPascalCompilerRequirement = nil): INyxPascalCompilerRequirements;
{ Freeze the optional capability of a response. Invalid count/getter/context
  refuses the whole collection; absent/nil capabilities yield an empty snapshot. }
function CaptureNyxPascalRequirements(const AResponse: IInterface): INyxPascalCompilerRequirements;
{ Exact retained pre-token state at an actual origin or complete EOF. -2 means
  no captured state at that virtual position; -1 stays reserved for bootstrap. }
function NyxPascalContextStateAt(const ASource: INyxPascalTranslatedSyntax;
  AOffset: Integer): Integer;

implementation

uses nyx.pascal.macros;

type
  { Forward implementation identity, borrowed only with its ordinary interface. }
  TSourceSizeRequirement = class;
  { Sealed immutable-owner receipt. Alternate requirement providers cannot
    bypass revalidation by returning a structurally similar public capability. }
  ISourceSizeOwner = interface(IInterface)
    ['{C292038B-EBC2-447C-B49F-E6CD9C6F2691}']
    function SourceSizeOwner: TSourceSizeRequirement;
  end;
  { Owns frozen source/context leases and value-only query coordinates. There
    is no source mutation, earlier context chain, host or application callback. }
  TSourceSizeRequirement = class(TInterfacedObject, INyxPascalCompilerRequirement,
    INyxPascalSourceSizeRequirement, ISourceSizeOwner)
  private
    FReference: TNyxPascalSizeReference;
    FSource: INyxPascalTranslatedSyntax;
    FContext: INyxPascalCompilerContext;
    FOrigin: TNyxPascalConditionOrigin;
    FKey: TNyxPascalStorageKey;
  public
    constructor Create(const AReference: TNyxPascalSizeReference;
      const ASource: INyxPascalTranslatedSyntax; const AOrigin: TNyxPascalConditionOrigin;
      const ACancellation: INyxCancellationScope);
    function Kind: TNyxPascalCompilerRequirementKind;
    function Reference: TNyxPascalSizeReference;
    function Context: INyxPascalCompilerContext;
    function OwnerSource: INyxPascalTranslatedSyntax;
    function ContextState: Integer;
    function Source: INyxPascalTranslatedSyntax;
    function SourceOffset: Integer;
    function OriginIndex: Integer;
    function Origin: TNyxPascalConditionOrigin;
    function StorageKey: TNyxPascalStorageKey;
    function SourceSizeOwner: TSourceSizeRequirement;
  end;
  { Owns only immutable snapshots, copied selector and checked scalar indices.
    Source leases have no reverse reference to this requirement or its result. }
  TCompilerRequirement = class(TInterfacedObject, INyxPascalCompilerRequirement)
  private
    FReference: TNyxPascalSizeReference;
    FContext: INyxPascalCompilerContext;
    FOwner: INyxPascalTranslatedSyntax;
    FSource: INyxPascalTranslatedSyntax;
    FState: Integer;
    FOffset: Integer;
    FOrigin: Integer;
  public
    constructor Create(const AContext: INyxPascalCompilerContext;
      const AReference: TNyxPascalSizeReference; const AOwner, ASource: INyxPascalTranslatedSyntax;
      AState, AOffset: Integer);
    function Kind: TNyxPascalCompilerRequirementKind;
    function Reference: TNyxPascalSizeReference;
    function Context: INyxPascalCompilerContext;
    function OwnerSource: INyxPascalTranslatedSyntax;
    function ContextState: Integer;
    function Source: INyxPascalTranslatedSyntax;
    function SourceOffset: Integer;
    function OriginIndex: Integer;
  end;
  { Owns a detached dynamic interface vector, never interface fields in records
    or static arrays. Only factory construction writes entries before publication. }
  TCompilerRequirements = class(TInterfacedObject, INyxPascalCompilerRequirements)
  public
    Values: array of INyxPascalCompilerRequirement;
    function RequirementCount: Integer;
    function RequirementAt(AIndex: Integer): INyxPascalCompilerRequirement;
  end;

function NyxPascalContextStateAt(const ASource: INyxPascalTranslatedSyntax;
  AOffset: Integer): Integer;
var
  LOrigin: Integer;
begin
  Result := -2;

  if (ASource = nil) or (AOffset < 0) or (AOffset > ASource.Syntax.Lexical.Scalars) then
  begin
    Exit;
  end;
  LOrigin := ASource.OriginAt(AOffset);

  if LOrigin >= 0 then
  begin
    Result := ASource.Origin(LOrigin).State;
  end
  else if (AOffset = ASource.Syntax.Lexical.Scalars) and ASource.CompleteTranslation then
  begin
    Result := ASource.Source.Occurrence(0).ExitState;
  end;
end;

constructor TCompilerRequirement.Create(const AContext: INyxPascalCompilerContext;
  const AReference: TNyxPascalSizeReference; const AOwner, ASource: INyxPascalTranslatedSyntax;
  AState, AOffset: Integer);
var
  LExpected: INyxPascalCompilerContext;
begin
  inherited Create;
  FContext := CaptureNyxPascalCompilerContext(AContext);
  FReference := NyxPascalSizeReference(AReference.Name);

  if (AOwner = nil) or (ASource = nil) or (AState < -1) or
    (AState >= AOwner.Source.StateCount) or (AOffset < 0) or
    (AOffset > ASource.Syntax.Lexical.Scalars) or
    (FContext.Family <> ppcFPC) or (FContext.Version.FullNumber = 0) or
    (FContext.Mode = ppmUnknown) or
    (NyxPascalCapturedSize(FContext, FReference).State <> pssUnknown) then
  begin
    raise ENyxPascalContext.Create('A missing primitive request needs exact compatible unknown compiler facts');
  end;

  if AState = -1 then
  begin
    LExpected := NyxPascalBootstrapContext(AOwner.Source.InitialContext);

    if NyxPascalCompilerScope(FContext) <> pcsoSystemBootstrap then
    begin
      raise ENyxPascalContext.Create('A bootstrap requirement cannot acquire pristine-program provenance');
    end;
  end
  else
  begin
    LExpected := AOwner.Source.State(AState);
  end;

  if (LExpected = nil) or not FContext.SameFacts(LExpected) then
  begin
    raise ENyxPascalContext.Create('A missing primitive requirement differs from its owning source context');
  end;
  FOwner := AOwner;
  FSource := ASource;
  FState := AState;
  FOffset := AOffset;
  FOrigin := ASource.OriginAt(AOffset);
end;

function TCompilerRequirement.Kind: TNyxPascalCompilerRequirementKind;
begin
  Result := pcrPrimitiveSize;
end;

function TCompilerRequirement.Reference: TNyxPascalSizeReference;
begin
  Result := FReference;
end;

function TCompilerRequirement.Context: INyxPascalCompilerContext;
begin
  Result := FContext;
end;

function TCompilerRequirement.OwnerSource: INyxPascalTranslatedSyntax;
begin
  Result := FOwner;
end;

function TCompilerRequirement.ContextState: Integer;
begin
  Result := FState;
end;

function TCompilerRequirement.Source: INyxPascalTranslatedSyntax;
begin
  Result := FSource;
end;

function TCompilerRequirement.SourceOffset: Integer;
begin
  Result := FOffset;
end;

function TCompilerRequirement.OriginIndex: Integer;
begin
  Result := FOrigin;
end;

function NewNyxPascalPrimitiveRequirement(const AContext: INyxPascalCompilerContext;
  const AReference: TNyxPascalSizeReference; const AOwner, ASource: INyxPascalTranslatedSyntax;
  AContextState, ASourceOffset: Integer): INyxPascalCompilerRequirement;
begin
  Result := TCompilerRequirement.Create(AContext, AReference, AOwner, ASource,
    AContextState, ASourceOffset);
end;

function CaptureNyxPascalRequirement(const ASource: INyxPascalCompilerRequirement): INyxPascalCompilerRequirement;
var
  LSource: INyxPascalSourceSizeRequirement;
  LOwned: ISourceSizeOwner;
  LObject: TSourceSizeRequirement;
  LRebuilt: INyxPascalCompilerRequirement;
  LRebuiltSource: INyxPascalSourceSizeRequirement;
begin

  if (ASource <> nil) and Supports(ASource, ISourceSizeOwner, LOwned) then
  begin
    LObject := LOwned.SourceSizeOwner;

    if (LObject = nil) or not (LObject is TSourceSizeRequirement) or
      ((LObject as INyxPascalCompilerRequirement) <> ASource) then
    begin
      raise ENyxPascalContext.Create('A source requirement has a foreign immutable owner');
    end;
    Exit(ASource);
  end;

  if (ASource <> nil) and (ASource.Kind = pcrSourceSize) and
    Supports(ASource, INyxPascalSourceSizeRequirement, LSource) then
  begin
    LRebuilt := NewNyxPascalSourceSizeRequirement(ASource.Reference, ASource.Source, LSource.Origin);
    Supports(LRebuilt, INyxPascalSourceSizeRequirement, LRebuiltSource);

    if not LRebuilt.Context.SameFacts(ASource.Context) or
      (ASource.OwnerSource <> ASource.Source) or (ASource.ContextState <> -1) or
      (ASource.OriginIndex <> -1) or (ASource.SourceOffset <> LRebuilt.SourceOffset) or
      not LRebuiltSource.StorageKey.SameKey(LSource.StorageKey) then
      raise ENyxPascalContext.Create('An alternate source request changed its checked owner or key');
    Exit(LRebuilt);
  end;

  if (ASource = nil) or (ASource.Kind <> pcrPrimitiveSize) then
  begin
    raise ENyxPascalContext.Create('Capture requires one supported complete compiler requirement');
  end;
  Result := NewNyxPascalPrimitiveRequirement(ASource.Context, ASource.Reference,
    ASource.OwnerSource, ASource.Source, ASource.ContextState, ASource.SourceOffset);
end;

function NyxPascalSourceSizeReferenceSafe(const ASource: INyxPascalTranslatedSyntax;
  const AName: TNyxPascalSizeReference;
  const ACancellation: INyxCancellationScope): Boolean;
var
  LState, LAt, LStart: Integer;
  LMacros: INyxPascalTranslationMacros;
  LEnvironment: INyxPascalMacroEnvironment;
  LContext: INyxPascalCompilerContext;
  LPart: TNyxText;
begin
  NyxPascalAdmitAnalysis(ACancellation, papConstantStep);

  if (ACancellation <> nil) and ACancellation.Cancelled then
    raise ENyxPascalAnalysisCancelled.Create('Source-size reference inspection cancelled');
  Result := False;

  if ASource = nil then Exit;
  LState := NyxPascalContextStateAt(ASource, ASource.Syntax.Lexical.Scalars);

  if (LState < 0) or not Supports(ASource.Source, INyxPascalTranslationMacros, LMacros) then Exit;
  LContext := ASource.Source.State(LState);

  if LContext.Feature(ppfReplacementMacros) = ppkFalse then Exit(True);
  LEnvironment := LMacros.StateMacros(LState);

  if LEnvironment = nil then Exit;
  NyxPascalAdmitAnalysis(ACancellation, papConstantStep, Length(AName.Name) + 4);

  if LEnvironment.Has(NyxPascalDefine('sizeof')) or LEnvironment.Has(NyxPascalDefine('div')) or
    LEnvironment.Has(NyxPascalDefine('mod')) or LEnvironment.Has(NyxPascalDefine('or')) then Exit;
  LStart := 1;
  for LAt := 1 to Length(AName.Name) + 1 do
  begin

    if (LAt <= Length(AName.Name)) and (AName.Name[LAt] <> '.') then Continue;
    LPart := Copy(AName.Name, LStart, LAt - LStart);

    if (Length(LPart) <= 128) and LEnvironment.Has(NyxPascalDefine(LPart)) then Exit;
    LStart := LAt + 1;
  end;
  Result := True;
end;

function NyxPascalSourceWitnessContext(const ASource: INyxPascalTranslatedSyntax): Boolean;
var
  LInitial, LRoot: INyxPascalCompilerContext;
  LFeature: TNyxPascalLanguageFeature;
begin
  Result := False;

  if (ASource = nil) or (ASource.Syntax.ImportCount <> 0) or
    (NyxPascalASCIIKey(ASource.Syntax.Scope(0).Name) <> 'system') then Exit;
  LInitial := ASource.Source.InitialContext;
  LRoot := NyxPascalBootstrapContext(LInitial);

  if (LRoot = nil) or (LInitial.Family <> ppcFPC) or (LRoot.Family <> ppcFPC) or
    (LInitial.Mode <> ppmFPC) or (LRoot.Mode <> ppmFPC) or
    (LInitial.Version.FullNumber = 0) or (LInitial.Version.FullNumber <> LRoot.Version.FullNumber) or
    LInitial.DefinesComplete or LRoot.DefinesComplete then Exit;
  { A requested initial modeswitch is not supplied by a metadata flag alone.
    The first native path accepts only matching captured startup features;
    authored switches in the original source remain compiler-owned. }
  for LFeature := Low(TNyxPascalLanguageFeature) to High(TNyxPascalLanguageFeature) do
  begin

    if (LInitial.Feature(LFeature) <> ppkUnknown) and
      (LInitial.Feature(LFeature) <> LRoot.Feature(LFeature)) then Exit;
  end;
  Result := NyxPascalCompilerPolicies(LInitial).SamePolicies(TNyxPascalCompilerPolicies.Unknown) and
    NyxPascalCompilerPolicies(LRoot).SamePolicies(TNyxPascalCompilerPolicies.Unknown) and
    (NyxPascalWarnings(LInitial).WarningCount = 0) and (NyxPascalWarnings(LRoot).WarningCount = 0) and
    (NyxPascalCodeAlignments(LInitial).CodeAlignmentCount = 0) and
    (NyxPascalCodeAlignments(LRoot).CodeAlignmentCount = 0) and
    (NyxPascalOptimizations(LInitial).OptimizationCount = 0) and
    (NyxPascalOptimizations(LRoot).OptimizationCount = 0);
end;

constructor TSourceSizeRequirement.Create(const AReference: TNyxPascalSizeReference;
  const ASource: INyxPascalTranslatedSyntax; const AOrigin: TNyxPascalConditionOrigin;
  const ACancellation: INyxCancellationScope);
var
  LAt, LFile, LVisits, LKeyLength: Integer;
  LInput: INyxPascalCompilerContext;
  LName: TNyxPascalDefineRef;
  LValue: TNyxPascalDefineValue;
  LParts: TNyxStrings;
  LView: INyxPascalSourceView;
  LDirective: TNyxText;
  LFeature: TNyxPascalLanguageFeature;

  procedure Part(const AText: TNyxText);
  const CHex: TNyxText = '0123456789ABCDEF';
  var
    LBytes: TNyxBytes;
    LEncoded: TNyxText;
    LByte: Integer;
  begin
    { UTF-8 hex keeps virtual Unicode names printable and independent of the
      native/pas2js string storage representation. Admit before conversion. }
    NyxPascalAdmitAnalysis(ACancellation, papConstantStep, Length(AText) + 1);
    LBytes := NyxEncodeUTF8(AText);
    NyxPascalAdmitAnalysis(ACancellation, papConstantStep, Length(LBytes) * 2 + 16);
    SetLength(LEncoded, Length(LBytes) * 2);
    for LByte := 0 to High(LBytes) do
    begin
      LEncoded[LByte * 2 + 1] := CHex[(LBytes[LByte] shr 4) + 1];
      LEncoded[LByte * 2 + 2] := CHex[(LBytes[LByte] and 15) + 1];
    end;

    if Length(LEncoded) + 16 > 1024 * 1024 - LKeyLength then
    begin
      raise ENyxPascalContext.Create('A source witness identity exceeds its storage-key limit');
    end;
    Inc(LKeyLength, Length(LEncoded) + 16);
    LParts.Add(TNyxText(IntToStr(Length(LEncoded))) + ':' + LEncoded + '|');
  end;
begin
  inherited Create;
  NyxPascalAdmitAnalysis(ACancellation, papConstantStep);
  FReference := NyxPascalSizeReference(AReference.Name);

  if not NyxPascalSourceSizeReferenceSafe(ASource, FReference, ACancellation) then
    raise ENyxPascalContext.Create('A native source-size reference has no unchanged captured macro spelling');

  if not NyxPascalSourceWitnessContext(ASource) or not ASource.Source.Resolved or
    (AOrigin.Occurrence < 0) or (AOrigin.Occurrence >= ASource.Source.OccurrenceCount) or
    (AOrigin.PrefixCount <> ASource.Source.Count) then
  begin
    raise ENyxPascalContext.Create('A source witness needs its exact resolved pre-conditional System prefix');
  end;
  LFile := ASource.Source.Occurrence(AOrigin.Occurrence).FileIndex;

  if (ASource.Source.Files.FileAt(LFile).Name <> AOrigin.FileReference.Name) or
    (AOrigin.Span.Start < 0) or (AOrigin.Span.Length < 1) then
  begin
    raise ENyxPascalContext.Create('A source witness has no captured physical query site');
  end;
  LView := NyxPascalSourceViewAt(ASource.Source.Files, LFile);

  if (LView = nil) or (LView.Kind <> psekUTF8) or (AOrigin.Span.Finish > LView.Scalars) then
    raise ENyxPascalContext.Create('A source witness has no exact readable physical span');
  NyxPascalAdmitAnalysis(ACancellation, papConstantStep, AOrigin.Span.Length * 8 + 1);
  LDirective := NyxPascalASCIIKey(NyxDecodeUTF8(LView.Original.Slice(LView.OriginalSpan(AOrigin.Span))));

  if (Copy(LDirective, 1, 4) <> '{$if') and (Copy(LDirective, 1, 8) <> '{$elseif') and
    (Copy(LDirective, 1, 5) <> '(*$if') and (Copy(LDirective, 1, 9) <> '(*$elseif') then
    raise ENyxPascalContext.Create('A source witness must precede its physical conditional directive');
  LVisits := 0;
  for LAt := 0 to ASource.Source.OccurrenceCount - 1 do
  begin
    NyxPascalAdmitAnalysis(ACancellation, papConstantStep);

    if ASource.Source.Occurrence(LAt).FileIndex = LFile then Inc(LVisits);
  end;

  if LVisits <> 1 then
  begin
    raise ENyxPascalContext.Create('Repeated source witness includes need occurrence-specific native materialization');
  end;
  LInput := ASource.Source.InitialContext;
  FContext := NyxPascalBootstrapContext(LInput);

  if (FContext = nil) or (FContext.Family <> ppcFPC) or
    (FContext.Mode <> ppmFPC) or (FContext.Version.FullNumber = 0) then
  begin
    raise ENyxPascalContext.Create('A source witness needs the exact captured compiler-root owner');
  end;
  FOrigin := AOrigin;
  FSource := ASource;
  LParts := TNyxStrings.Create;
  try
    LKeyLength := 32;
    LParts.Add('nyx.storage.1|source-v1|');
    Part(NyxPascalFileCatalogIdentity(ASource.Source.Files));
    Part(ASource.Source.Files.FileAt(ASource.Source.Occurrence(0).FileIndex).Name);
    Part(AOrigin.FileReference.Name);
    Part(TNyxText(IntToStr(AOrigin.Span.Start)));
    Part(FReference.Name);
    Part(TNyxText(IntToStr(Ord(FContext.Family))));
    Part(TNyxText(IntToStr(FContext.Version.FullNumber)));
    Part(TNyxText(IntToStr(Ord(FContext.Mode))));
    Part('initial-inputs');
    for LAt := Ord('A') to Ord('Z') do
      Part(TNyxText(IntToStr(Ord(LInput.Option(Chr(LAt))))));
    for LFeature := Low(TNyxPascalLanguageFeature) to High(TNyxPascalLanguageFeature) do
      Part(TNyxText(IntToStr(Ord(LInput.Feature(LFeature)))));
    for LAt := 0 to LInput.SymbolCount - 1 do
    begin
      NyxPascalAdmitAnalysis(ACancellation, papConstantStep);
      LName := LInput.SymbolName(LAt);
      LValue := LInput.Symbol(LName);

      if LValue.Kind in [ppdDefined, ppdInteger] then
      begin
        Part(LName.Name); Part(TNyxText(IntToStr(Ord(LValue.Kind))));

        if LValue.Kind = ppdInteger then Part(TNyxText(IntToStr(LValue.IntegerValue)));
      end;
    end;
    Part('compiler-root-inputs');
    for LAt := Ord('A') to Ord('Z') do
      Part(TNyxText(IntToStr(Ord(FContext.Option(Chr(LAt))))));
    for LFeature := Low(TNyxPascalLanguageFeature) to High(TNyxPascalLanguageFeature) do
      Part(TNyxText(IntToStr(Ord(FContext.Feature(LFeature)))));
    { Captured presences/integer values are compiler inputs. Further verified
      absences and primitive measurements refine knowledge without changing
      those inputs or invalidating an earlier native source observation. }
    for LAt := 0 to FContext.SymbolCount - 1 do
    begin
      NyxPascalAdmitAnalysis(ACancellation, papConstantStep);
      LName := FContext.SymbolName(LAt);
      LValue := FContext.Symbol(LName);

      if LValue.Kind in [ppdDefined, ppdInteger] then
      begin
        Part(LName.Name); Part(TNyxText(IntToStr(Ord(LValue.Kind))));

        if LValue.Kind = ppdInteger then Part(TNyxText(IntToStr(LValue.IntegerValue)));
      end;
    end;
    NyxPascalAdmitAnalysis(ACancellation, papConstantStep, LKeyLength);
    FKey := NyxPascalStorageKey(LParts.Join);
  finally
    LParts.Free;
  end;
end;

function TSourceSizeRequirement.Kind: TNyxPascalCompilerRequirementKind;
begin Result := pcrSourceSize; end;
function TSourceSizeRequirement.Reference: TNyxPascalSizeReference;
begin Result := FReference; end;
function TSourceSizeRequirement.Context: INyxPascalCompilerContext;
begin Result := FContext; end;
function TSourceSizeRequirement.OwnerSource: INyxPascalTranslatedSyntax;
begin Result := FSource; end;
function TSourceSizeRequirement.ContextState: Integer;
begin Result := -1; end;
function TSourceSizeRequirement.Source: INyxPascalTranslatedSyntax;
begin Result := FSource; end;
function TSourceSizeRequirement.SourceOffset: Integer;
begin Result := FSource.Syntax.Lexical.Scalars; end;
function TSourceSizeRequirement.OriginIndex: Integer;
begin Result := -1; end;
function TSourceSizeRequirement.Origin: TNyxPascalConditionOrigin;
begin Result := FOrigin; end;
function TSourceSizeRequirement.StorageKey: TNyxPascalStorageKey;
begin Result := FKey; end;
function TSourceSizeRequirement.SourceSizeOwner: TSourceSizeRequirement;
begin Result := Self; end;

function NewNyxPascalSourceSizeRequirement(const AReference: TNyxPascalSizeReference;
  const ASource: INyxPascalTranslatedSyntax;
  const AOrigin: TNyxPascalConditionOrigin;
  const ACancellation: INyxCancellationScope): INyxPascalCompilerRequirement;
begin
  Result := TSourceSizeRequirement.Create(AReference, ASource, AOrigin, ACancellation);
end;

function TCompilerRequirements.RequirementCount: Integer;
begin
  Result := Length(Values);
end;

function TCompilerRequirements.RequirementAt(AIndex: Integer): INyxPascalCompilerRequirement;
begin

  if (AIndex < 0) or (AIndex >= Length(Values)) then
  begin
    raise ENyxPascalContext.Create('A compiler requirement index is outside its captured response');
  end;
  Result := Values[AIndex];
end;

function NewNyxPascalRequirements(const ARequirement: INyxPascalCompilerRequirement): INyxPascalCompilerRequirements;
var
  LResult: TCompilerRequirements;
begin
  LResult := TCompilerRequirements.Create;
  Result := LResult;

  if ARequirement <> nil then
  begin
    SetLength(LResult.Values, 1);
    LResult.Values[0] := CaptureNyxPascalRequirement(ARequirement);
  end;
end;

function CaptureNyxPascalRequirements(const AResponse: IInterface): INyxPascalCompilerRequirements;
var
  LSource: INyxPascalCompilerRequirements;
  LResult: TCompilerRequirements;
  LCount: Integer;
  LAt: Integer;
  LOwner: INyxPascalCompilerRequirements;
begin
  LResult := TCompilerRequirements.Create;
  LOwner := LResult;

  if (AResponse <> nil) and Supports(AResponse, INyxPascalCompilerRequirements, LSource) then
  begin
    LCount := LSource.RequirementCount;

    if (LCount < 0) or (LCount > 64) then
    begin
      raise ENyxPascalContext.Create('Compiler requirement responses exceed their bounded collection');
    end;
    SetLength(LResult.Values, LCount);
    for LAt := 0 to LCount - 1 do
    begin
      LResult.Values[LAt] := CaptureNyxPascalRequirement(LSource.RequirementAt(LAt));
    end;
  end;
  Result := LOwner;
end;

end.
