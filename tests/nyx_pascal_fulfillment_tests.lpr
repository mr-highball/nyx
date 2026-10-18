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

uses SysUtils, nyx.text, nyx.scheduler, nyx.pascal.context, nyx.pascal.files,
  nyx.pascal.lexing,
  nyx.pascal.translation, nyx.pascal.syntax, nyx.pascal.syntax.translated,
  nyx.pascal.binding, nyx.pascal.constants, nyx.pascal.values,
  nyx.pascal.requirements, nyx.pascal.compilertrace, nyx.pascal.modules,
  nyx.pascal.fulfillment
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
    {$ifndef PAS2JS}
    Check(ParamCount = 2, 'Supply configured compiler and fresh native scratch root');
    ForceDirectoriesUTF8(ParamStr(2));
    Native(ParamStr(1), ParamStr(2));
    {$endif}
    WriteLn('PASS ', GChecks, ' atomic primitive fulfillment checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
