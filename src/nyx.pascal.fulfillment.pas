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
unit nyx.pascal.fulfillment;
{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.scheduler, nyx.pascal.context, nyx.pascal.files, nyx.pascal.modules,
  nyx.pascal.requirements, nyx.pascal.compilertrace;

type
  { Immutable ordered capture vector, bounded to 64. Append freezes an alternate
    producer; checked ordinals retain no mutable host. Repeated identical captures
    may represent separate replies, but identical contexts with conflicting default
    anchors refuse. Source/pristine/bootstrap identity is never retagged. }
  INyxPascalCompilerCaptures = interface(IInterface)
    ['{1A4346C1-2C41-4987-A7C0-E1CF360E9A66}']
    function Count: Integer;
    function CaptureAt(AIndex: Integer): INyxFPCCompilerCapture;
    { Return the first exact context owner, or -1. No semantic equivalence guess. }
    function Find(const AContext: INyxPascalCompilerContext): Integer;
    function WithCapture(const AValue: INyxFPCCompilerCapture): INyxPascalCompilerCaptures;
  end;
  { Atomic fulfilled module/capture result. All source files remain external
    immutable inputs; only exact context deltas change. Shared matching bootstrap
    packets update together without touching unrelated modules or either input.
    Captures and replies are value snapshots, not application execution evidence. }
  INyxPascalCompilerFulfillment = interface(IInterface)
    ['{DA85820C-79B5-4459-BEE7-EA092E623215}']
    function Modules: INyxPascalModuleCatalog;
    function Captures: INyxPascalCompilerCaptures;
    function Replies: INyxPascalCompilerCaptures;
    function Jobs: Integer;
  end;
  { Fully preflighted unique jobs in source request order. Exact duplicate requests
    share one measurement. Plan construction does no I/O; a native host may then
    perform only these jobs. Apply validates every one-fact reply before returning
    any updated module/capture set. Source replay remains explicit and separate. }
  INyxPascalCompilerFulfillmentPlan = interface(IInterface)
    ['{0765E982-B0D4-4223-AD73-9FB1393F729C}']
    function Count: Integer;
    function RequirementAt(AIndex: Integer): INyxPascalCompilerRequirement;
    function OwnerAt(AIndex: Integer): INyxFPCCompilerCapture;
    function ApplyReplies(const AReplies: INyxPascalCompilerCaptures;
      const ACancellation: INyxCancellationScope = nil): INyxPascalCompilerFulfillment;
  end;

{ Empty immutable vector; no compiler, file or default context is inferred. }
function NewNyxPascalCompilerCaptures: INyxPascalCompilerCaptures;
{ Freeze a caller-serialized vector, including checked alternate getters. Nil
  means empty; excess counts and ambiguous default anchors refuse atomically. }
function CaptureNyxPascalCompilerCaptures(
  const ASource: INyxPascalCompilerCaptures): INyxPascalCompilerCaptures;
{ Freeze and preflight every optional typed request against exact captured module
  and compiler owners. Positive explicit job limit; no source-local promotion,
  diagnostic-text parsing or inferred measurement. Limit/identity/cancellation
  failure publishes no plan, allowing a host to refuse before creating scratch. }
function PlanNyxPascalCompilerFulfillment(const AFiles: INyxPascalFileCatalog;
  const AModules: INyxPascalModuleCatalog; const AResponse: IInterface;
  const ACaptures: INyxPascalCompilerCaptures; AMaximumJobs: Integer;
  const ACancellation: INyxCancellationScope = nil): INyxPascalCompilerFulfillmentPlan;

implementation

uses SysUtils, nyx.pascal.lexing, nyx.pascal.syntax.translated;

type
  { Class-owned dynamic interface vector. No interfaces are stored in records or
    static arrays; fluent copies never change a published source vector. }
  TCaptures = class(TInterfacedObject, INyxPascalCompilerCaptures)
  public
    Values: array of INyxFPCCompilerCapture;
    function Count: Integer;
    function CaptureAt(AIndex: Integer): INyxFPCCompilerCapture;
    function Find(const AContext: INyxPascalCompilerContext): Integer;
    function WithCapture(const AValue: INyxFPCCompilerCapture): INyxPascalCompilerCaptures;
  end;
  { Published result owns immutable output snapshots only. There is no reverse
    lease to the construction plan, a compiler process or an editor. }
  TFulfillment = class(TInterfacedObject, INyxPascalCompilerFulfillment)
  public
    ModuleValues: INyxPascalModuleCatalog;
    CaptureValues: INyxPascalCompilerCaptures;
    ReplyValues: INyxPascalCompilerCaptures;
    function Modules: INyxPascalModuleCatalog;
    function Captures: INyxPascalCompilerCaptures;
    function Replies: INyxPascalCompilerCaptures;
    function Jobs: Integer;
  end;
  { Owns frozen inputs and scalar job indices into them. Request/source leases
    point outward to immutable snapshots, never back into this plan. All jobs
    preflight against the unchanged inputs before any delegated external effect. }
  TFulfillmentPlan = class(TInterfacedObject, INyxPascalCompilerFulfillmentPlan)
  private
    FFiles: INyxPascalFileCatalog;
    FModules: INyxPascalModuleCatalog;
    FCaptures: INyxPascalCompilerCaptures;
    FRequirements: array of INyxPascalCompilerRequirement;
    FOwners: array of Integer;
    FModuleRows: array of Integer;
  public
    constructor Create(const AFiles: INyxPascalFileCatalog;
      const AModules: INyxPascalModuleCatalog; const AResponse: IInterface;
      const ACaptures: INyxPascalCompilerCaptures; AMaximumJobs: Integer;
      const ACancellation: INyxCancellationScope);
    function Count: Integer;
    function RequirementAt(AIndex: Integer): INyxPascalCompilerRequirement;
    function OwnerAt(AIndex: Integer): INyxFPCCompilerCapture;
    function ApplyReplies(const AReplies: INyxPascalCompilerCaptures;
      const ACancellation: INyxCancellationScope): INyxPascalCompilerFulfillment;
  end;

procedure CheckCancellation(const AValue: INyxCancellationScope);
begin

  if (AValue <> nil) and AValue.Cancelled then
  begin
    raise ENyxPascalAnalysisCancelled.Create('Primitive fulfillment cancelled');
  end;
end;

function TCaptures.Count: Integer;
begin
  Result := Length(Values);
end;

function TCaptures.CaptureAt(AIndex: Integer): INyxFPCCompilerCapture;
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ENyxPascalContext.Create('A compiler capture index is outside its immutable vector');
  end;
  Result := Values[AIndex];
end;

function TCaptures.Find(const AContext: INyxPascalCompilerContext): Integer;
var
  LAt: Integer;
begin
  Result := -1;

  if AContext = nil then
  begin
    Exit;
  end;
  for LAt := 0 to Count - 1 do
  begin

    if Values[LAt].Context.SameFacts(AContext) then
    begin
      Exit(LAt);
    end;
  end;
end;

function TCaptures.WithCapture(const AValue: INyxFPCCompilerCapture): INyxPascalCompilerCaptures;
var
  LValue: INyxFPCCompilerCapture;
  LCopy: TCaptures;
  LOwner: INyxPascalCompilerCaptures;
  LAt: Integer;
begin
  LValue := FreezeNyxFPCCompilerCapture(AValue);

  if Count >= 64 then
  begin
    raise ENyxPascalContext.Create('Compiler capture vectors are bounded to 64 entries');
  end;
  LAt := Find(LValue.Context);

  if (LAt >= 0) and not Values[LAt].ObservedDefaults.SameFacts(LValue.ObservedDefaults) then
  begin
    raise ENyxPascalContext.Create('Identical compiler facts cannot have ambiguous default anchors');
  end;
  LCopy := TCaptures.Create;
  LOwner := LCopy;
  SetLength(LCopy.Values, Count + 1);
  for LAt := 0 to Count - 1 do
  begin
    LCopy.Values[LAt] := Values[LAt];
  end;
  LCopy.Values[Count] := LValue;
  Result := LOwner;
end;

function NewNyxPascalCompilerCaptures: INyxPascalCompilerCaptures;
begin
  Result := TCaptures.Create;
end;

function CaptureNyxPascalCompilerCaptures(
  const ASource: INyxPascalCompilerCaptures): INyxPascalCompilerCaptures;
var
  LCount: Integer;
  LAt: Integer;
  LValues: INyxPascalCompilerCaptures;
begin
  LValues := NewNyxPascalCompilerCaptures;

  if ASource <> nil then
  begin
    LCount := ASource.Count;

    if (LCount < 0) or (LCount > 64) then
    begin
      raise ENyxPascalContext.Create('An alternate capture vector exceeds its bound');
    end;
    for LAt := 0 to LCount - 1 do
    begin
      LValues := LValues.WithCapture(ASource.CaptureAt(LAt));
    end;
  end;
  Result := LValues;
end;

function TFulfillment.Modules: INyxPascalModuleCatalog;
begin
  Result := ModuleValues;
end;

function TFulfillment.Captures: INyxPascalCompilerCaptures;
begin
  Result := CaptureValues;
end;

function TFulfillment.Replies: INyxPascalCompilerCaptures;
begin
  Result := ReplyValues;
end;

function TFulfillment.Jobs: Integer;
begin
  Result := ReplyValues.Count;
end;

constructor TFulfillmentPlan.Create(const AFiles: INyxPascalFileCatalog;
  const AModules: INyxPascalModuleCatalog; const AResponse: IInterface;
  const ACaptures: INyxPascalCompilerCaptures; AMaximumJobs: Integer;
  const ACancellation: INyxCancellationScope);
var
  LRequests: INyxPascalCompilerRequirements;
  LRequest: INyxPascalCompilerRequirement;
  LModuleAt: Integer;
  LCaptureAt: Integer;
  LAt: Integer;
  LPrevious: Integer;
  LDuplicate: Boolean;
  LSourceKey, LPreviousKey: INyxPascalSourceSizeRequirement;
begin
  inherited Create;
  CheckCancellation(ACancellation);

  if (AMaximumJobs < 1) or (AMaximumJobs > 64) then
  begin
    raise ENyxPascalContext.Create('Use an explicit primitive job limit from one through 64');
  end;
  FFiles := CaptureNyxPascalFiles(AFiles);
  FModules := CaptureNyxPascalModules(AModules, FFiles);
  FCaptures := CaptureNyxPascalCompilerCaptures(ACaptures);
  LRequests := CaptureNyxPascalRequirements(AResponse);
  for LAt := 0 to LRequests.RequirementCount - 1 do
  begin
    CheckCancellation(ACancellation);
    LRequest := LRequests.RequirementAt(LAt);
    LModuleAt := FModules.Find(NyxPascalModule(LRequest.OwnerSource.Syntax.Scope(0).Name));

    if LModuleAt < 0 then
    begin
      raise ENyxPascalContext.Create('A primitive request has no registered captured module owner');
    end;
    CaptureNyxPascalRequirementModule(FFiles, FModules.Entry(LModuleAt), LRequest);
    LCaptureAt := FCaptures.Find(LRequest.Context);

    if LCaptureAt < 0 then
    begin
      raise ENyxPascalContext.Create('A primitive request has no exact registered compiler capture');
    end;
    LDuplicate := False;
    for LPrevious := 0 to Count - 1 do
    begin
      if (FOwners[LPrevious] = LCaptureAt) and
        (FModuleRows[LPrevious] = LModuleAt) and
        (FRequirements[LPrevious].Kind = LRequest.Kind) and
        (FRequirements[LPrevious].Reference.Name = LRequest.Reference.Name) then
      begin

        if LRequest.Kind = pcrSourceSize then
        begin
          Supports(LRequest, INyxPascalSourceSizeRequirement, LSourceKey);
          Supports(FRequirements[LPrevious], INyxPascalSourceSizeRequirement, LPreviousKey);
          LDuplicate := LDuplicate or LSourceKey.StorageKey.SameKey(LPreviousKey.StorageKey);
        end
        else LDuplicate := True;
      end;
    end;

    if LDuplicate then
    begin
      Continue;
    end;

    if Count >= AMaximumJobs then
    begin
      raise ENyxPascalContext.Create('Primitive requests exceed the explicit batch job allowance');
    end;
    LPrevious := Count;
    SetLength(FRequirements, LPrevious + 1);
    SetLength(FOwners, LPrevious + 1);
    SetLength(FModuleRows, LPrevious + 1);
    FRequirements[LPrevious] := LRequest;
    FOwners[LPrevious] := LCaptureAt;
    FModuleRows[LPrevious] := LModuleAt;
  end;
  CheckCancellation(ACancellation);
end;

function TFulfillmentPlan.Count: Integer;
begin
  Result := Length(FRequirements);
end;

function TFulfillmentPlan.RequirementAt(AIndex: Integer): INyxPascalCompilerRequirement;
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ENyxPascalContext.Create('A primitive job index is outside its frozen plan');
  end;
  Result := FRequirements[AIndex];
end;

function TFulfillmentPlan.OwnerAt(AIndex: Integer): INyxFPCCompilerCapture;
begin
  RequirementAt(AIndex);
  Result := FCaptures.CaptureAt(FOwners[AIndex]);
end;

function TFulfillmentPlan.ApplyReplies(const AReplies: INyxPascalCompilerCaptures;
  const ACancellation: INyxCancellationScope): INyxPascalCompilerFulfillment;
var
  LReplies: INyxPascalCompilerCaptures;
  LMerged: array of INyxFPCCompilerCapture;
  LModules: INyxPascalModuleCatalog;
  LCaptures: INyxPascalCompilerCaptures;
  LModule: INyxPascalModuleSource;
  LContext: INyxPascalCompilerContext;
  LRoot: INyxPascalCompilerContext;
  LAt: Integer;
  LCapture: Integer;
  LValue: TFulfillment;
  LOwner: INyxPascalCompilerFulfillment;
begin
  CheckCancellation(ACancellation);
  LReplies := CaptureNyxPascalCompilerCaptures(AReplies);

  if LReplies.Count <> Count then
  begin
    raise ENyxPascalContext.Create('Primitive fulfillment needs one complete reply per frozen job');
  end;
  { Validate every source owner and exact one-fact delta against the unchanged
    plan. A late invalid reply cannot publish any earlier successful update. }
  for LAt := 0 to Count - 1 do
  begin
    CheckCancellation(ACancellation);
    ResolveNyxPascalModuleRequirement(FFiles, FModules.Entry(FModuleRows[LAt]),
      FRequirements[LAt], OwnerAt(LAt), LReplies.CaptureAt(LAt));
  end;
  SetLength(LMerged, FCaptures.Count);
  for LCapture := 0 to FCaptures.Count - 1 do
  begin
    LMerged[LCapture] := FCaptures.CaptureAt(LCapture);
    for LAt := 0 to Count - 1 do
    begin
      CheckCancellation(ACancellation);

      if FCaptures.CaptureAt(LCapture).Context.SameFacts(FRequirements[LAt].Context) then
      begin
        LMerged[LCapture] := LMerged[LCapture].Merge(LReplies.CaptureAt(LAt));
      end;
    end;
  end;
  LModules := FModules;
  for LAt := 0 to FModules.Count - 1 do
  begin
    CheckCancellation(ACancellation);
    LModule := FModules.Entry(LAt);
    LContext := LModule.Context;

    if LContext = nil then
    begin
      Continue;
    end;
    LRoot := NyxPascalBootstrapContext(LContext);
    for LCapture := 0 to FCaptures.Count - 1 do
    begin

      if LContext.SameFacts(FCaptures.CaptureAt(LCapture).Context) then
      begin
        LContext := LMerged[LCapture].Context;
        Break;
      end;

      if (LRoot <> nil) and LRoot.SameFacts(FCaptures.CaptureAt(LCapture).Context) then
      begin
        LContext := NyxPascalWithBootstrapContext(LContext, LMerged[LCapture].Context);
        Break;
      end;
    end;

    if not LModule.Context.SameFacts(LContext) then
    begin
      LModules := LModules.WithModule(LModule.InContext(LContext, LModule.Translation));
    end;
  end;
  LCaptures := NewNyxPascalCompilerCaptures;
  for LCapture := 0 to Length(LMerged) - 1 do
  begin
    CheckCancellation(ACancellation);
    LCaptures := LCaptures.WithCapture(LMerged[LCapture]);
  end;
  LValue := TFulfillment.Create;
  LOwner := LValue;
  LValue.ModuleValues := LModules;
  LValue.CaptureValues := LCaptures;
  LValue.ReplyValues := LReplies;
  CheckCancellation(ACancellation);
  Result := LOwner;
end;

function PlanNyxPascalCompilerFulfillment(const AFiles: INyxPascalFileCatalog;
  const AModules: INyxPascalModuleCatalog; const AResponse: IInterface;
  const ACaptures: INyxPascalCompilerCaptures; AMaximumJobs: Integer;
  const ACancellation: INyxCancellationScope): INyxPascalCompilerFulfillmentPlan;
begin
  Result := TFulfillmentPlan.Create(AFiles, AModules, AResponse, ACaptures,
    AMaximumJobs, ACancellation);
end;

end.
