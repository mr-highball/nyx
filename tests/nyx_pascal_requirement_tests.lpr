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
program nyx_pascal_requirement_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.scheduler, nyx.pascal.context, nyx.pascal.lexing,
  nyx.pascal.files, nyx.pascal.translation, nyx.pascal.syntax,
  nyx.pascal.syntax.translated, nyx.pascal.binding, nyx.pascal.modules,
  nyx.pascal.requirements, nyx.pascal.compilertrace, nyx.pascal.constants,
  nyx.pascal.values, nyx.pascal.conditions
  {$ifndef PAS2JS}, LazFileUtils, nyx.studio.pascalprofiles{$endif};

type
  { Controlled foreign capture producer. Freeze must detach its fields and must
    not invoke this producer's deliberately unavailable Merge implementation. }
  TCaptureProducer = class(TInterfacedObject, INyxFPCCompilerCapture)
  public
    Facts: INyxPascalCompilerContext;
    Defaults: INyxPascalCompilerContext;
    function Context: INyxPascalCompilerContext;
    function ObservedDefaults: INyxPascalCompilerContext;
    function Merge(const ANext: INyxFPCCompilerCapture): INyxFPCCompilerCapture;
  end;
  { Mutable read-only collection producer used to exercise bounded capture and
    atomic bad-getter refusal. Published snapshots must retain no producer. }
  TRequirementProducer = class(TInterfacedObject, INyxPascalCompilerRequirements)
  public
    Count: Integer;
    Item: INyxPascalCompilerRequirement;
    InvalidAt: Integer;
    function RequirementCount: Integer;
    function RequirementAt(AIndex: Integer): INyxPascalCompilerRequirement;
  end;
  { Owns an independent source world and module descriptor. The minimal source
    qualifies the portable request algorithm, not replacement SDK declarations. }
  TSourceFixture = class
  public
    Files: INyxPascalFileCatalog;
    Module: INyxPascalModuleSource;
    View: INyxPascalTranslatedSyntax;
    World: INyxPascalSemanticWorld;
    Site: INyxPascalBindingSite;
    constructor Create(const ARootFacts: INyxPascalCompilerContext);
    function Missing: INyxPascalCompilerRequirement;
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

function TCaptureProducer.Context: INyxPascalCompilerContext;
begin
  Result := Facts;
end;

function TCaptureProducer.ObservedDefaults: INyxPascalCompilerContext;
begin
  Result := Defaults;
end;

function TCaptureProducer.Merge(const ANext: INyxFPCCompilerCapture): INyxFPCCompilerCapture;
begin
  Result := nil;
  raise Exception.Create('A frozen capture must not call a foreign Merge implementation');
end;

function TRequirementProducer.RequirementCount: Integer;
begin
  Result := Count;
end;

function TRequirementProducer.RequirementAt(AIndex: Integer): INyxPascalCompilerRequirement;
begin
  Result := Item;

  if AIndex = InvalidAt then
  begin
    Result := nil;
  end;
end;

function Capture(const AFacts, ADefaults: INyxPascalCompilerContext): INyxFPCCompilerCapture;
var
  LProducer: TCaptureProducer;
  LLease: INyxFPCCompilerCapture;
begin
  LProducer := TCaptureProducer.Create;
  LLease := LProducer;
  LProducer.Facts := AFacts;
  LProducer.Defaults := ADefaults;
  Result := FreezeNyxFPCCompilerCapture(LLease);
end;

function RootDefaults: INyxPascalCompilerContext;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmFPC, pcsoSystemBootstrap)
    .WithOption('H', ppkFalse);
end;

function RootFacts: INyxPascalCompilerContext;
begin
  Result := NyxPascalWithCapturedSize(RootDefaults, NyxPascalSizeReference('Byte'),
    NyxPascalSizeFact(pssKnown, ppmFPC, ppkFalse, 1));
end;

constructor TSourceFixture.Create(const ARootFacts: INyxPascalCompilerContext);
var
  LProfile: INyxPascalCompilerContext;
begin
  inherited Create;
  LProfile := NewNyxPascalCompilerContext(ppcFPC, ARootFacts.Version, ppmFPC)
    .WithOption('H', ppkFalse);
  LProfile := NyxPascalWithBootstrapContext(LProfile, ARootFacts);
  Files := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('system.pas'),
    'unit System; interface var ErrorCode:Word; implementation end.');
  Module := NewNyxPascalModuleSource(NyxPascalModule('System'), NyxPascalPhysicalFile('system.pas'))
    .InContext(LProfile, TNyxPascalTranslationOptions.Defaults);
  View := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(Files, Module.Root,
    Module.Context, Module.Translation), TNyxPascalSyntaxOptions.Defaults);
  World := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults
    .WithImplicitScopes(pispCompilerUnits)).WithModule(View);
  Site := World.At(0, 0, View.Syntax.Lexical.Scalars);
end;

function TSourceFixture.Missing: INyxPascalCompilerRequirement;
var
  LValue: INyxPascalConstantResult;
  LRequests: INyxPascalCompilerRequirements;
begin
  LValue := PlanNyxPascalConstant('High(ErrorCode)', TNyxPascalConstantOptions.Defaults)
    .Evaluate(Site, TNyxPascalConstantOptions.Defaults);
  Check((LValue.Value.Kind = pvvUnknown) and (LValue.Issue = pbiCompilerContext),
    'A missing compiler-root primitive cannot become a guessed ordinal bound');
  Check(Supports(LValue, INyxPascalCompilerRequirements, LRequests) and
    (LRequests.RequirementCount = 1), 'Constant evaluation carries one structured primitive requirement');
  Result := LRequests.RequirementAt(0);
end;

procedure Portable;
var
  LFixture: TSourceFixture;
  LBase: INyxFPCCompilerCapture;
  LFresh: INyxFPCCompilerCapture;
  LReply: INyxFPCCompilerCapture;
  LRequirement: INyxPascalCompilerRequirement;
  LRequests: INyxPascalCompilerRequirements;
  LResolution: INyxPascalResolution;
  LModule: INyxPascalModuleSource;
  LBadModule: INyxPascalModuleSource;
  LContext: INyxPascalCompilerContext;
  LResult: INyxPascalConstantResult;
  LView: INyxPascalTranslatedSyntax;
  LWorld: INyxPascalSemanticWorld;
  LPlan: INyxPascalConstantPlan;
  LRejected: Boolean;
  LProducer: TRequirementProducer;
  LLease: INyxPascalCompilerRequirements;
  LFrozen: INyxPascalCompilerRequirements;
  LSource: INyxPascalFileCatalog;
  LConditional: INyxPascalSemanticTranslation;
  LName: TNyxPascalSizeReference;
  LConsumer: INyxPascalTranslatedSyntax;
begin
  LFixture := TSourceFixture.Create(RootFacts);
  try
    LRequirement := LFixture.Missing;
    LName := LRequirement.Reference;
    Check((LRequirement.Kind = pcrPrimitiveSize) and
      (NyxPascalASCIIKey(LName.Name) = TNyxText('word')) and
      (LRequirement.ContextState = -1) and
      (NyxPascalCompilerScope(LRequirement.Context) = pcsoSystemBootstrap),
      'The typed request identifies root presence/size without diagnostic-text dispatch');
    Check((LRequirement.OwnerSource = LFixture.View) and
      (LRequirement.Source = LFixture.View) and (LRequirement.OriginIndex >= 0),
      'Context owner and actual observing source token remain independently leased');
    Check(LRequirement.Context.SameFacts(RootFacts), 'Request capture preserves all exact prior compiler facts');
    LSource := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('consumer.pas'),
      'unit Consumer; interface var Value:Word; implementation end.');
    LConsumer := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LSource,
      NyxPascalPhysicalFile('consumer.pas'), LFixture.Module.Context,
      LFixture.Module.Translation), TNyxPascalSyntaxOptions.Defaults);
    LWorld := LFixture.World.WithModule(LConsumer);
    LResult := PlanNyxPascalConstant('High(Value)', TNyxPascalConstantOptions.Defaults)
      .Evaluate(LWorld.At(1, 0, LConsumer.Syntax.Lexical.Scalars), TNyxPascalConstantOptions.Defaults);
    LRequests := CaptureNyxPascalRequirements(LResult);
    Check((LRequests.RequirementCount = 1) and
      (LRequests.RequirementAt(0).OwnerSource = LFixture.View) and
      (LRequests.RequirementAt(0).Source = LConsumer),
      'An importing observer cannot become the compiler-root context owner');
    LResolution := LFixture.World.Lookup(LFixture.Site, NyxPascalBindingName('ErrorCode'));
    LResolution := LFixture.World.TypeOfSymbol(LResolution.SymbolAt(0));
    Check(Supports(LResolution, INyxPascalCompilerRequirements, LRequests) and
      (LRequests.RequirementCount = 1), 'Ordinary type binding exposes the same optional requirement capability');
    LBase := Capture(RootFacts, RootDefaults);
    LContext := NyxPascalWithCapturedSize(RootDefaults, LName,
      NyxPascalSizeFact(pssKnown, ppmFPC, ppkFalse, 2))
      .WithSymbol(NyxPascalDefine('UNRELATED_QUERY'), ppdDefined);
    LFresh := Capture(LContext, RootDefaults);
    LReply := MergeNyxFPCSizeCapture(LBase, LFresh, LName);
    Check((NyxPascalCapturedSize(LReply.Context, LName).Bytes = 2) and
      (NyxPascalCapturedSize(LReply.Context, NyxPascalSizeReference('Byte')).Bytes = 1) and
      (LReply.Context.Symbol(NyxPascalDefine('UNRELATED_QUERY')).Kind = ppdUnknown),
      'A coherent reply retains old facts and publishes only the requested fact');
    Check(NyxPascalCapturedSize(LBase.Context, LName).State = pssUnknown,
      'Reply construction leaves its exact previous capture unchanged');
    LModule := ResolveNyxPascalModuleRequirement(LFixture.Files, LFixture.Module, LRequirement, LBase, LReply);
    Check((LModule.Context.Mode = LFixture.Module.Context.Mode) and
      (NyxPascalCapturedSize(NyxPascalBootstrapContext(LModule.Context), LName).Bytes = 2),
      'Fact application updates only the independent compiler-root packet');
    LView := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFixture.Files,
      LModule.Root, LModule.Context, LModule.Translation), TNyxPascalSyntaxOptions.Defaults);
    LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults
      .WithImplicitScopes(pispCompilerUnits)).WithModule(LView);
    LPlan := PlanNyxPascalConstant('High(ErrorCode)', TNyxPascalConstantOptions.Defaults);
    LResult := LPlan.Evaluate(LWorld.At(0, 0, LView.Syntax.Lexical.Scalars), TNyxPascalConstantOptions.Defaults);
    Check((LResult.Issue = pbiNone) and (LResult.Value.Number.ToText = TNyxText('65535')),
      'Existing bound-type evaluation consumes the resolved packet without a special Word path');
    Check(CaptureNyxPascalRequirements(LResult).RequirementCount = 0,
      'A successful value does not retain obsolete missing requirements');
    LSource := LFixture.Files.WithFile(NyxPascalPhysicalFile('system.pas'),
      'unit System; interface var ErrorCode:Word; { changed } implementation end.');
    LRejected := False;
    LBadModule := nil;
    try
      LBadModule := ResolveNyxPascalModuleRequirement(LSource, LFixture.Module, LRequirement, LBase, LReply);
    except
      on ENyxPascalContext do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LBadModule = nil), 'Changed source bytes refuse before publishing a module');
    LRejected := False;
    try
      LBadModule := ResolveNyxPascalModuleRequirement(LFixture.Files, LModule, LRequirement, LBase, LReply);
    except
      on ENyxPascalContext do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LBadModule = nil), 'A prior request refuses after its owner context changes');
    LRejected := False;
    try
      LBadModule := ResolveNyxPascalModuleRequirement(LFixture.Files, LFixture.Module,
        LRequirement, LBase, Capture(LContext, RootDefaults));
    except
      on ENyxPascalContext do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LBadModule = nil), 'A reply with unrelated fact changes cannot widen module application');
    LRejected := False;
    try
      MergeNyxFPCSizeCapture(LBase, Capture(LContext,
        RootDefaults.WithSymbol(NyxPascalDefine('CHANGED_DEFAULT'), ppdDefined)), LName);
    except
      on ENyxPascalContext do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'Different default anchors refuse even when compiler version/mode agree');
    LRejected := False;
    try
      NewNyxPascalPrimitiveRequirement(RootFacts.WithSymbol(NyxPascalDefine('CHANGED'), ppdDefined),
        LName, LFixture.View, LFixture.View, -1, LFixture.Site.Caret);
    except
      on ENyxPascalContext do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'A forged context cannot borrow an unrelated owner source');
    LRejected := False;
    try
      NewNyxPascalPrimitiveRequirement(RootFacts, NyxPascalSizeReference('Byte'),
        LFixture.View, LFixture.View, -1, LFixture.Site.Caret);
    except
      on ENyxPascalContext do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'An already captured fact is not a missing requirement');
    LRequirement := NewNyxPascalPrimitiveRequirement(RootFacts, LName,
      LFixture.View, LFixture.View, -1, LFixture.Site.Caret);
    Check(LRequirement.OriginIndex = -1, 'EOF remains an honest unmapped observing position');
    LProducer := TRequirementProducer.Create;
    LLease := LProducer;
    LProducer.Count := 1;
    LProducer.Item := LRequirement;
    LProducer.InvalidAt := -1;
    LFrozen := CaptureNyxPascalRequirements(LLease);
    LProducer.Item := nil;
    Check((LFrozen.RequirementCount = 1) and
      (LFrozen.RequirementAt(0).Reference.Name = LName.Name), 'Collection capture detaches mutable producers');
    LProducer.Count := 65;
    LRejected := False;
    try
      CaptureNyxPascalRequirements(LLease);
    except
      on ENyxPascalContext do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'Excess requirement collections refuse without truncation');
    LProducer.Count := 2;
    LProducer.Item := LRequirement;
    LProducer.InvalidAt := 1;
    LRequests := nil;
    LRejected := False;
    try
      LRequests := CaptureNyxPascalRequirements(LLease);
    except
      on ENyxPascalContext do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LRequests = nil), 'A failed alternate getter publishes no partial collection');
    LRejected := False;
    try
      LFrozen.RequirementAt(1);
    except
      on ENyxPascalContext do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'Requirement collection getters enforce exact bounds');
    LSource := LFixture.Files.WithFile(NyxPascalPhysicalFile('system.pas'),
      'unit System; interface var ErrorCode:Word; {$if High(ErrorCode)=65535}' +
      'const Ready=1;{$endif}implementation end.');
    LConditional := NewNyxPascalSemanticTranslation(LSource, LFixture.Module.Root,
      LFixture.Module.Context, nil, LFixture.Module.Translation, TNyxPascalSyntaxOptions.Defaults,
      TNyxPascalConditionalOptions.Defaults);
    Check(LConditional.Pending and (CaptureNyxPascalRequirements(LConditional).RequirementCount = 1),
      'Ordered constant conditions propagate exact typed missing facts');
    LSource := LFixture.Files.WithFile(NyxPascalPhysicalFile('system.pas'),
      'unit System; interface {$if sizeof(Word)=2}const Ready=1;{$endif}implementation end.');
    LConditional := NewNyxPascalSemanticTranslation(LSource, LFixture.Module.Root,
      LFixture.Module.Context, nil, LFixture.Module.Translation, TNyxPascalSyntaxOptions.Defaults,
      TNyxPascalConditionalOptions.Defaults);
    Check(LConditional.Pending and (CaptureNyxPascalRequirements(LConditional).RequirementCount = 1),
      'Legacy source-size conditions preserve the same compiler requirement capability');
  finally
    LFixture.Free;
  end;
  Check(LRequirement.Context.SameFacts(RootFacts) and
    (LRequirement.OwnerSource.Source.Files.TextAt(0) =
      TNyxText('unit System; interface var ErrorCode:Word; implementation end.')),
    'Detached requirements retain their exact source/context after fixture retirement');
  LFixture := TSourceFixture.Create(NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(0, 0, 0), ppmFPC, pcsoSystemBootstrap));
  try
    LResult := PlanNyxPascalConstant('High(ErrorCode)', TNyxPascalConstantOptions.Defaults)
      .Evaluate(LFixture.Site, TNyxPascalConstantOptions.Defaults);
    Check((LResult.Issue = pbiCompilerContext) and
      (CaptureNyxPascalRequirements(LResult).RequirementCount = 0),
      'Incomplete compiler metadata retains inspection without creating an invalid host request');
  finally
    LFixture.Free;
  end;
end;

{$ifndef PAS2JS}
procedure Native(const ACompiler, AOutput: TNyxText);
var
  LOwner: INyxFPCCompilerCapture;
  LReply: INyxFPCCompilerCapture;
  LAbsent: INyxFPCCompilerCapture;
  LProbe: INyxPascalCompilerProbe;
  LFixture: TSourceFixture;
  LRequest: INyxPascalCompilerRequirement;
  LModule: INyxPascalModuleSource;
  LValue: INyxPascalConstantResult;
  LWorld: INyxPascalSemanticWorld;
  LView: INyxPascalTranslatedSyntax;
  LRejected: Boolean;
  LCancel: INyxCancellationScope;
  LPristine: INyxFPCCompilerCapture;
  LPristineReply: INyxFPCCompilerCapture;
  LFiles: INyxPascalFileCatalog;
  LSite: INyxPascalBindingSite;
  LRequests: INyxPascalCompilerRequirements;
begin
  Check(not DirectoryExistsUTF8(AOutput) and CreateDirUTF8(AOutput), 'Native fulfillment owns fresh evidence output');
  LProbe := NyxFPCProbeSizeOf(NewNyxFPCSystemBootstrapProbe(NyxPascalDefine('REQUEST_OWNER')),
    NyxPascalSizeReference('Byte'));
  LOwner := CaptureNyxFPCCompilerCapture(ACompiler, AOutput + '/owner', LProbe,
    TNyxPascalCompilerTraceOptions.Defaults);
  LFixture := TSourceFixture.Create(LOwner.Context);
  try
    LRequest := LFixture.Missing;
    LReply := FulfillNyxFPCCompilerRequirement(ACompiler, AOutput + '/required', LRequest,
      LOwner, TNyxPascalCompilerTraceOptions.Defaults);
    Check((NyxPascalCapturedSize(LReply.Context, LRequest.Reference).State = pssKnown) and
      (NyxPascalCapturedSize(LReply.Context, LRequest.Reference).Bytes = 2),
      'The configured compiler resolves the requested primitive in its actual compiler-root scope');
    LModule := ResolveNyxPascalModuleRequirement(LFixture.Files, LFixture.Module, LRequest, LOwner, LReply);
    LView := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFixture.Files,
      LModule.Root, LModule.Context, LModule.Translation), TNyxPascalSyntaxOptions.Defaults);
    LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults
      .WithImplicitScopes(pispCompilerUnits)).WithModule(LView);
    LValue := PlanNyxPascalConstant('High(ErrorCode)', TNyxPascalConstantOptions.Defaults)
      .Evaluate(LWorld.At(0, 0, LView.Syntax.Lexical.Scalars), TNyxPascalConstantOptions.Defaults);
    Check((LValue.Issue = pbiNone) and (LValue.Value.Number.ToText = TNyxText('65535')),
      'Actual measured evidence reaches the ordinary source value consumer');
    LRejected := False;
    try
      FulfillNyxFPCCompilerRequirement(ACompiler, AOutput + '/wrong-owner', LRequest,
        LReply, TNyxPascalCompilerTraceOptions.Defaults);
    except
      on ENyxPascalContext do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and not DirectoryExistsUTF8(AOutput + '/wrong-owner'),
      'Mismatched owner preflight creates no compiler directory');
    LCancel := NewNyxCancellationScope;
    LCancel.Cancel;
    LRejected := False;
    try
      FulfillNyxFPCCompilerRequirement(ACompiler, AOutput + '/cancelled', LRequest,
        LOwner, TNyxPascalCompilerTraceOptions.Defaults, LCancel);
    except
      on ENyxPascalAnalysisCancelled do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and not DirectoryExistsUTF8(AOutput + '/cancelled'),
      'Cancelled native preflight creates no compiler directory or reply');
    LRequest := NewNyxPascalPrimitiveRequirement(LOwner.Context,
      NyxPascalSizeReference('NYX_UNDECLARED_PRIMITIVE'), LFixture.View, LFixture.View,
      -1, LFixture.Site.Caret);
    LAbsent := FulfillNyxFPCCompilerRequirement(ACompiler, AOutput + '/absent', LRequest,
      LOwner, TNyxPascalCompilerTraceOptions.Defaults);
    Check(NyxPascalCapturedSize(LAbsent.Context, LRequest.Reference).State = pssUndeclared,
      'Actual compiler absence remains a typed result without inventing a primitive');
    Check(NyxPascalCapturedSize(LOwner.Context, NyxPascalSizeReference('Word')).State = pssUnknown,
      'Native fulfillment retains the independent previous capture');
    LPristine := CaptureNyxFPCCompilerCapture(ACompiler, AOutput + '/pristine-owner',
      NewNyxFPCCompilerProbe(NyxPascalDefine('PRISTINE_OWNER'), ppmDelphi),
      TNyxPascalCompilerTraceOptions.Defaults);
    LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile('pristine.pas'),
      'unit Pristine; interface var Value:Word; implementation end.');
    LModule := NewNyxPascalModuleSource(NyxPascalModule('Pristine'), NyxPascalPhysicalFile('pristine.pas'))
      .InContext(LPristine.Context, TNyxPascalTranslationOptions.Defaults);
    LView := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFiles, LModule.Root,
      LModule.Context, LModule.Translation), TNyxPascalSyntaxOptions.Defaults);
    LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LView);
    LSite := LWorld.At(0, 0, LView.Syntax.Lexical.Scalars);
    LValue := PlanNyxPascalConstant('High(Value)', TNyxPascalConstantOptions.Defaults)
      .Evaluate(LSite, TNyxPascalConstantOptions.Defaults);
    LRequests := CaptureNyxPascalRequirements(LValue);
    Check((LRequests.RequirementCount = 1) and
      (NyxPascalCompilerScope(LRequests.RequirementAt(0).Context) = pcsoPristine),
      'A pristine missing primitive keeps its independent observed source context');
    LRequest := LRequests.RequirementAt(0);
    LPristineReply := FulfillNyxFPCCompilerRequirement(ACompiler, AOutput + '/pristine-required',
      LRequest, LPristine, TNyxPascalCompilerTraceOptions.Defaults);
    LModule := ResolveNyxPascalModuleRequirement(LFiles, LModule, LRequest, LPristine, LPristineReply);
    Check((NyxPascalCapturedSize(LModule.Context, LRequest.Reference).Bytes = 2) and
      (NyxPascalBootstrapContext(LModule.Context) = nil),
      'Pristine fulfillment does not acquire compiler-root provenance');
    LFiles := LFiles.WithFile(LModule.Root,
      'unit Pristine; {$H-} interface var Value:Word; implementation end.');
    LView := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFiles, LModule.Root,
      LPristine.Context, TNyxPascalTranslationOptions.Defaults), TNyxPascalSyntaxOptions.Defaults);
    LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults).WithModule(LView);
    LValue := PlanNyxPascalConstant('High(Value)', TNyxPascalConstantOptions.Defaults)
      .Evaluate(LWorld.At(0, 0, LView.Syntax.Lexical.Scalars), TNyxPascalConstantOptions.Defaults);
    LRequests := CaptureNyxPascalRequirements(LValue);
    Check((LRequests.RequirementCount = 1) and
      not LRequests.RequirementAt(0).Context.SameFacts(LPristine.Context),
      'A source-local mode effect retains its different captured context');
    LRejected := False;
    try
      FulfillNyxFPCCompilerRequirement(ACompiler, AOutput + '/source-local-retag',
        LRequests.RequirementAt(0), LPristine, TNyxPascalCompilerTraceOptions.Defaults);
    except
      on ENyxPascalContext do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and not DirectoryExistsUTF8(AOutput + '/source-local-retag'),
      'Source-local facts cannot silently request a pristine host measurement');
  finally
    LFixture.Free;
  end;
end;
{$endif}

begin
  try
    Portable;
    {$ifndef PAS2JS}
    Check(ParamCount = 2, 'Supply configured FPC and fresh native evidence output');
    Native(TNyxText(ParamStr(1)), TNyxText(ParamStr(2)));
    {$endif}
    WriteLn('PASS ', GChecks, ' typed missing compiler requirement checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
