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
unit nyx.pascal.completion.owned;
{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.scheduler, nyx.pascal.lexing, nyx.pascal.context,
  nyx.pascal.files, nyx.pascal.translation, nyx.pascal.syntax,
  nyx.pascal.binding, nyx.pascal.modules, nyx.pascal.sources, nyx.pascal.completion,
  nyx.pascal.encoding, nyx.pascal.conditions;

type
  { Independent copied analysis budgets. Per-module translation/syntax and
    semantic query limits retain their existing contracts; AggregateTokens caps
    translated tokens across every project/SDK root in this one request.
    No target, path, compiler process or mutable producer is stored here. }
  TNyxOwnedPascalCompletionOptions = record
  private
    FTranslation: TNyxPascalTranslationOptions;
    FSyntax: TNyxPascalSyntaxOptions;
    FBinding: TNyxPascalBindingOptions;
    FConditions: TNyxPascalConditionalOptions;
    FTokens: Integer;
    FReceiverTokens: Integer;
    FDependencyLoads: Integer;
    FDefined: Boolean;
  public
    class function Defaults: TNyxOwnedPascalCompletionOptions; static;
    function WithTranslation(const AValue: TNyxPascalTranslationOptions): TNyxOwnedPascalCompletionOptions;
    function WithSyntax(const AValue: TNyxPascalSyntaxOptions): TNyxOwnedPascalCompletionOptions;
    function WithBinding(const AValue: TNyxPascalBindingOptions): TNyxOwnedPascalCompletionOptions;
    { Bound ordered semantic work within each root as well as AggregateTokens. }
    function WithConditions(const AValue: TNyxPascalConditionalOptions): TNyxOwnedPascalCompletionOptions;
    function UpToTokens(AValue: Integer): TNyxOwnedPascalCompletionOptions;
    function UpToReceiverTokens(AValue: Integer): TNyxOwnedPascalCompletionOptions;
    { Limit successful on-demand module additions across the entire request. }
    function UpToDependencyLoads(AValue: Integer): TNyxOwnedPascalCompletionOptions;
    procedure Validate;
    property Translation: TNyxPascalTranslationOptions read FTranslation;
    property Syntax: TNyxPascalSyntaxOptions read FSyntax;
    property Binding: TNyxPascalBindingOptions read FBinding;
    property Conditions: TNyxPascalConditionalOptions read FConditions;
    property AggregateTokens: Integer read FTokens;
    property ReceiverTokens: Integer read FReceiverTokens;
    property DependencyLoads: Integer read FDependencyLoads;
  end;
  { Closed assistance gaps, independent of compiler diagnostics or admission.
    Binding preserves its own typed issue/related name for focused context capture. }
  TNyxOwnedPascalCompletionGap = (pcgNone, pcgExpression, pcgPhysicalContext,
    pcgRepeatedInclude, pcgSourceIdentity, pcgBinding, pcgBudget, pcgCancelled, pcgInternal);
  { Physical declaration location within an explicit analysis root. These virtual
    names confer no filesystem/edit permission. Repeated inclusion has its exact
    occurrence; Span addresses the original declaration name in scalar units. }
  TNyxPascalCompletionDefinition = record
    Root: TNyxPascalPhysicalFile;
    FileReference: TNyxPascalPhysicalFile;
    Span: TNyxPascalSpan;
    Occurrence: Integer;
  end;
  { Optional in-process definition/gap metadata. The ordinary completion result
    and existing wire schema stay unchanged; codecs do not transport this
    capability. Ready rows retain their exact physical lexical source after the
    provider is released/replaced, so navigation need not reopen a changed file. }
  INyxOwnedPascalCompletionResult = interface(IInterface)
    ['{170458CB-C602-4E28-84F0-E90C2408EE01}']
    function Gap: TNyxOwnedPascalCompletionGap;
    function BindingIssue: TNyxPascalBindingIssue;
    function RelatedName: TNyxText;
    function Definition(AIndex: Integer): TNyxPascalCompletionDefinition;
    function DefinitionSource(AIndex: Integer): INyxPascalLexicalSnapshot;
  end;
  { Optional original-byte navigation for a ready row. The detached readable
    view retains exact bytes/encoding after provider/world retirement. Its origin
    distinguishes external bytes from canonical authoring UTF-8. No wire schema
    or file/process authority follows from these in-process coordinates. }
  INyxPascalCompletionByteOrigins = interface(IInterface)
    ['{170458CB-C602-4E28-84F0-E90C2408EE04}']
    function DefinitionView(AIndex: Integer): INyxPascalSourceView;
    function DefinitionByteSpan(AIndex: Integer): TNyxPascalByteSpan;
  end;
  { Optional synchronous cancellation for worker owners. Cancellation is borrowed
    only during this call and publishes a detached cancelled response, no rows.
    The original provider interface remains usable by existing Studio consumers. }
  INyxCancellablePascalCompletionProvider = interface(IInterface)
    ['{170458CB-C602-4E28-84F0-E90C2408EE02}']
    function CompleteWithCancellation(const ARequest: INyxPascalCompletionRequest;
      const ACancellation: INyxCancellationScope): INyxPascalCompletionResult;
  end;
  { Optional detached analysis receipt, independent of wire schemas and source
    publication. Includes every actually translated project/SDK root in insertion
    order; indexed but unused modules are absent. No world/provider is retained. }
  INyxPascalCompletionAnalysis = interface(IInterface)
    ['{170458CB-C602-4E28-84F0-E90C2408EE03}']
    function ModuleCount: Integer;
    function ModuleName(AIndex: Integer): TNyxPascalModuleRef;
    function ModuleRoot(AIndex: Integer): TNyxPascalPhysicalFile;
    function TranslatedTokens: Integer;
    function DependencyLoads: Integer;
  end;

{ Capture explicit SDK files/roots and compiler facts once. Requests overlay all
  exact project buffers, preserving accepted membership and include ownership.
  The provider reads no disk, starts no compiler, executes no authored code and
  retains no editor. Hosts run synchronous analysis on their scheduler/worker.
  Receivers use source-owned postfix contracts: dotted names, declared fluent
  call results, grouping, pointers and array elements. Calls require every visible
  result to agree; argument admission, casts, helpers and general inference remain
  separate. No authored expression, callback, getter or factory executes.
  Repeated occurrences of the requested include refuse rather than choosing one.
  This factory installs no default provider or Studio configuration. }
function NewNyxOwnedPascalCompletionProvider(const AContext: INyxPascalCompilerContext;
  const AFiles: INyxPascalFileCatalog; const ASDKRoots: array of TNyxPascalPhysicalFile;
  const AOptions: TNyxOwnedPascalCompletionOptions): INyxPascalCompletionProvider;
{ Capture an explicit module index instead of eagerly translating SDK roots.
  Missing-module binding replies load only that indexed captured source, then
  restart semantic queries with fresh exact-world handles. Parsed earlier roots
  and aggregate counters survive retries. Unindexed/partial/context-incompatible
  inputs refuse; nothing searches the filesystem or fabricates export knowledge. }
function NewNyxIndexedPascalCompletionProvider(const AContext: INyxPascalCompilerContext;
  const AFiles: INyxPascalFileCatalog; const AModules: INyxPascalModuleCatalog;
  const AOptions: TNyxOwnedPascalCompletionOptions): INyxPascalCompletionProvider;

implementation

uses SysUtils, nyx.pascal.drafts, nyx.pascal.files.project, nyx.pascal.syntax.translated,
  nyx.pascal.work;

type
  { An internally raised typed refusal; its bounded English message contains no
    host exception/path. The call owner converts it to a no-row detached result. }
  EOwnedGap = class(Exception)
  public
    GapKind: TNyxOwnedPascalCompletionGap;
    Issue: TNyxPascalBindingIssue;
    Related: TNyxText;
    constructor CreateGap(AGap: TNyxOwnedPascalCompletionGap; const AMessage: TNyxText;
      AIssue: TNyxPascalBindingIssue = pbiNone; const ARelated: TNyxText = '');
  end;
  { Value-only row locations, detached from parser/world lifetimes. }
  TDefinitionVector = array of TNyxPascalCompletionDefinition;
  { Value-owned analysis location; interfaces live only in the owning classes. }
  TModuleLocation = record
    Name: TNyxPascalModuleRef;
    Root: TNyxPascalPhysicalFile;
  end;
  { Delegates guarded insertion/preview to the original result owner. Only
    source-location values and typed gap metadata are added, without a cycle. }
  TOwnedResult = class(TInterfacedObject, INyxPascalCompletionResult,
    INyxOwnedPascalCompletionResult, INyxPascalCompletionAnalysis,
    INyxPascalCompletionByteOrigins)
  public
    Base: INyxPascalCompletionResult;
    Definitions: TDefinitionVector;
    DefinitionSources: array of INyxPascalLexicalSnapshot;
    DefinitionViews: array of INyxPascalSourceView;
    Modules: array of TModuleLocation;
    Tokens: Integer;
    Loads: Integer;
    GapValue: TNyxOwnedPascalCompletionGap;
    IssueValue: TNyxPascalBindingIssue;
    RelatedValue: TNyxText;
    function Request: INyxPascalCompletionRequest;
    function Status: TNyxPascalCompletionStatus;
    function MessageText: TNyxText;
    function Count: Integer;
    function Total: Integer;
    function Item(AIndex: Integer): TNyxPascalCompletionItem;
    function ReplaceStart: Integer;
    function ExpectedText: TNyxText;
    function Preview(AIndex: Integer; const ACurrent: INyxPascalSourceSnapshot;
      out ANewCaret: Integer): INyxPascalSourceSnapshot;
    function Gap: TNyxOwnedPascalCompletionGap;
    function BindingIssue: TNyxPascalBindingIssue;
    function RelatedName: TNyxText;
    function Definition(AIndex: Integer): TNyxPascalCompletionDefinition;
    function DefinitionSource(AIndex: Integer): INyxPascalLexicalSnapshot;
    function DefinitionView(AIndex: Integer): INyxPascalSourceView;
    function DefinitionByteSpan(AIndex: Integer): TNyxPascalByteSpan;
    function ModuleCount: Integer;
    function ModuleName(AIndex: Integer): TNyxPascalModuleRef;
    function ModuleRoot(AIndex: Integer): TNyxPascalPhysicalFile;
    function TranslatedTokens: Integer;
    function DependencyLoads: Integer;
  end;
  { Frozen, read-only configuration. Each call owns its own sources/world/counters;
    concurrent readers share no mutable query cache or application state. }
  TOwnedProvider = class(TInterfacedObject, INyxPascalCompletionProvider,
    INyxCancellablePascalCompletionProvider)
  public
    Context: INyxPascalCompilerContext;
    Files: INyxPascalFileCatalog;
    Roots: array of TNyxPascalPhysicalFile;
    Modules: INyxPascalModuleCatalog;
    Options: TNyxOwnedPascalCompletionOptions;
    function Complete(const ARequest: INyxPascalCompletionRequest): INyxPascalCompletionResult;
    function CompleteWithCancellation(const ARequest: INyxPascalCompletionRequest;
      const ACancellation: INyxCancellationScope): INyxPascalCompletionResult;
  end;
  { One synchronous analysis call. Provider is borrowed only until this call joins;
    immutable sources/world and cancellation are retained for that same lifetime.
    No response retains this helper or a mutable host/editor. }
  TAnalysis = class
  private
    FProvider: TOwnedProvider;
    FRequest: INyxPascalCompletionRequest;
    FCancel: INyxCancellationScope;
    FFiles: INyxPascalFileCatalog;
    FWorld: INyxPascalSemanticWorld;
    FBuilding: array of TNyxText;
    FOwner: Integer;
    FTokens: Integer;
    FScan: Integer;
    FLoads: Integer;
    FReceiverReady: Boolean;
    FReceiverExpressionStart: Integer;
    FReceiverPrefix: TNyxText;
    FReceiverExpected: TNyxText;
    FReceiverStart: Integer;
    FReceiverDot: Integer;
    procedure Check;
    procedure Scan;
    procedure AddModule(const AFile: TNyxPascalPhysicalFile; const AExpected: TNyxText;
      const AContext: INyxPascalCompilerContext = nil;
      const ADescriptor: INyxPascalModuleSource = nil; AReplace: Boolean = False);
    procedure BuildWorld;
    procedure RefineWorld;
    function LoadDependency(const AName: TNyxText): Boolean;
    procedure CaptureAnalysis(AResult: TOwnedResult);
    function Previous(const ALexical: INyxPascalLexicalSnapshot; AIndex: Integer): Integer;
    procedure Receiver(const ALexical: INyxPascalLexicalSnapshot;
      out AStart, ADot, AExpressionStart: Integer; out AExpected, APrefix: TNyxText);
    function Caller(const AFile: TNyxPascalPhysicalFile; ADot, AExpressionStart: Integer;
      out AExpressionWindow: TNyxPascalSpan): INyxPascalBindingSite;
    function Run: INyxPascalCompletionResult;
  public
    constructor Create(AProvider: TOwnedProvider;
      const ARequest: INyxPascalCompletionRequest; const ACancellation: INyxCancellationScope);
    function Complete: INyxPascalCompletionResult;
  end;

class function TNyxOwnedPascalCompletionOptions.Defaults: TNyxOwnedPascalCompletionOptions;
begin
  Result.FDefined := True;
  Result.FTranslation := TNyxPascalTranslationOptions.Defaults;
  Result.FSyntax := TNyxPascalSyntaxOptions.Defaults;
  Result.FBinding := TNyxPascalBindingOptions.Defaults.WithImplicitScopes(pispCompilerUnits);
  Result.FConditions := TNyxPascalConditionalOptions.Defaults;
  Result.FTokens := 1000000;
  Result.FReceiverTokens := 128;
  Result.FDependencyLoads := 128;
end;

procedure TNyxOwnedPascalCompletionOptions.Validate;
begin

  if not FDefined or (FTokens < 1) or (FReceiverTokens < 1) or (FDependencyLoads < 1) then
  begin
    raise ENyxPascalBinding.Create('Use explicit positive owned completion budgets');
  end;
  FTranslation.Validate;
  FSyntax.Validate;
  FBinding.Validate;
  FConditions.Validate;
end;

function TNyxOwnedPascalCompletionOptions.WithTranslation(
  const AValue: TNyxPascalTranslationOptions): TNyxOwnedPascalCompletionOptions;
begin
  Result := Self;
  Result.FTranslation := AValue;
  Result.Validate;
end;

function TNyxOwnedPascalCompletionOptions.WithSyntax(
  const AValue: TNyxPascalSyntaxOptions): TNyxOwnedPascalCompletionOptions;
begin
  Result := Self;
  Result.FSyntax := AValue;
  Result.Validate;
end;

function TNyxOwnedPascalCompletionOptions.WithBinding(
  const AValue: TNyxPascalBindingOptions): TNyxOwnedPascalCompletionOptions;
begin
  Result := Self;
  Result.FBinding := AValue;
  Result.Validate;
end;

function TNyxOwnedPascalCompletionOptions.WithConditions(
  const AValue: TNyxPascalConditionalOptions): TNyxOwnedPascalCompletionOptions;
begin
  Result := Self;
  Result.FConditions := AValue;
  Result.Validate;
end;

function TNyxOwnedPascalCompletionOptions.UpToTokens(AValue: Integer): TNyxOwnedPascalCompletionOptions;
begin
  Result := Self;
  Result.FTokens := AValue;
  Result.Validate;
end;

function TNyxOwnedPascalCompletionOptions.UpToReceiverTokens(AValue: Integer): TNyxOwnedPascalCompletionOptions;
begin
  Result := Self;
  Result.FReceiverTokens := AValue;
  Result.Validate;
end;

function TNyxOwnedPascalCompletionOptions.UpToDependencyLoads(AValue: Integer): TNyxOwnedPascalCompletionOptions;
begin
  Result := Self;
  Result.FDependencyLoads := AValue;
  Result.Validate;
end;

constructor EOwnedGap.CreateGap(AGap: TNyxOwnedPascalCompletionGap;
  const AMessage: TNyxText; AIssue: TNyxPascalBindingIssue; const ARelated: TNyxText);
begin
  inherited Create(AMessage);
  GapKind := AGap;
  Issue := AIssue;
  Related := ARelated;
end;

function TOwnedResult.Request: INyxPascalCompletionRequest;
begin
  Result := Base.Request;
end;

function TOwnedResult.Status: TNyxPascalCompletionStatus;
begin
  Result := Base.Status;
end;

function TOwnedResult.MessageText: TNyxText;
begin
  Result := Base.MessageText;
end;

function TOwnedResult.Count: Integer;
begin
  Result := Base.Count;
end;

function TOwnedResult.Total: Integer;
begin
  Result := Base.Total;
end;

function TOwnedResult.Item(AIndex: Integer): TNyxPascalCompletionItem;
begin
  Result := Base.Item(AIndex);
end;

function TOwnedResult.ReplaceStart: Integer;
begin
  Result := Base.ReplaceStart;
end;

function TOwnedResult.ExpectedText: TNyxText;
begin
  Result := Base.ExpectedText;
end;

function TOwnedResult.Preview(AIndex: Integer; const ACurrent: INyxPascalSourceSnapshot;
  out ANewCaret: Integer): INyxPascalSourceSnapshot;
begin
  Result := Base.Preview(AIndex, ACurrent, ANewCaret);
end;

function TOwnedResult.Gap: TNyxOwnedPascalCompletionGap;
begin
  Result := GapValue;
end;

function TOwnedResult.BindingIssue: TNyxPascalBindingIssue;
begin
  Result := IssueValue;
end;

function TOwnedResult.RelatedName: TNyxText;
begin
  Result := RelatedValue;
end;

function TOwnedResult.Definition(AIndex: Integer): TNyxPascalCompletionDefinition;
begin
  Item(AIndex);
  Result := Definitions[AIndex];
end;

function TOwnedResult.DefinitionSource(AIndex: Integer): INyxPascalLexicalSnapshot;
begin
  Item(AIndex);
  Result := DefinitionSources[AIndex];
end;

function TOwnedResult.DefinitionView(AIndex: Integer): INyxPascalSourceView;
begin
  Item(AIndex);
  Result := DefinitionViews[AIndex];
end;

function TOwnedResult.DefinitionByteSpan(AIndex: Integer): TNyxPascalByteSpan;
begin
  Item(AIndex);
  Result := DefinitionViews[AIndex].OriginalSpan(Definitions[AIndex].Span);
end;

function TOwnedResult.ModuleCount: Integer;
begin
  Result := Length(Modules);
end;

function TOwnedResult.ModuleName(AIndex: Integer): TNyxPascalModuleRef;
begin

  if (AIndex < 0) or (AIndex >= Length(Modules)) then
  begin
    raise ENyxPascalContext.Create('Analysis module index is outside its receipt');
  end;
  Result := Modules[AIndex].Name;
end;

function TOwnedResult.ModuleRoot(AIndex: Integer): TNyxPascalPhysicalFile;
begin
  ModuleName(AIndex);
  Result := Modules[AIndex].Root;
end;

function TOwnedResult.TranslatedTokens: Integer;
begin
  Result := Tokens;
end;

function TOwnedResult.DependencyLoads: Integer;
begin
  Result := Loads;
end;

constructor TAnalysis.Create(AProvider: TOwnedProvider;
  const ARequest: INyxPascalCompletionRequest; const ACancellation: INyxCancellationScope);
begin
  inherited Create;
  FProvider := AProvider;
  FRequest := CaptureNyxPascalCompletionRequest(ARequest);
  FCancel := ACancellation;
end;

procedure TAnalysis.Check;
begin

  if (FCancel <> nil) and FCancel.Cancelled then
  begin
    raise ENyxPascalAnalysisCancelled.Create('Owned Pascal completion cancelled');
  end;
end;

procedure TAnalysis.Scan;
begin
  Check;

  if FScan >= FProvider.Options.ReceiverTokens then
  begin
    raise EOwnedGap.CreateGap(pcgBudget, 'The receiver expression exceeds its token budget');
  end;
  Inc(FScan);
end;

procedure TAnalysis.AddModule(const AFile: TNyxPascalPhysicalFile; const AExpected: TNyxText;
  const AContext: INyxPascalCompilerContext; const ADescriptor: INyxPascalModuleSource;
  AReplace: Boolean);
var
  LView: INyxPascalTranslatedSyntax;
  LSource: INyxPascalTranslation;
  LName: TNyxText;
  LFacts: INyxPascalCompilerContext;
  LOptions: TNyxPascalTranslationOptions;
  LConstruction: INyxPascalSemanticTranslation;
  LConditions: TNyxPascalConditionalOptions;
  LAt: Integer;
  LDepth: Integer;
  LRemaining: Integer;
begin
  Check;
  LFacts := AContext;
  LOptions := FProvider.Options.Translation;

  if LFacts = nil then
  begin
    LFacts := FProvider.Context;
  end
  else if ADescriptor <> nil then
  begin
    LOptions := ADescriptor.Translation;
  end;

  if (LFacts.Family <> FProvider.Context.Family) or
    (LFacts.Version.FullNumber <> FProvider.Context.Version.FullNumber) then
  begin
    raise EOwnedGap.CreateGap(pcgBinding, 'This indexed module needs a compatible captured compiler identity',
      pbiCompilerContext, AExpected);
  end;

  if NyxPascalSourceViewAt(FFiles, FFiles.Find(AFile)) = nil then
  begin
    raise EOwnedGap.CreateGap(pcgBinding, 'This captured module needs an explicit readable source encoding',
      pbiCompilerContext, AExpected);
  end;
  for LAt := 0 to High(FBuilding) do
  begin

    if FBuilding[LAt] = AFile.Name then
    begin
      raise EOwnedGap.CreateGap(pcgBinding, 'Conditional imports need a still-constructing source root',
        pbiIncompleteSource, AExpected);
    end;
  end;

  if Length(FBuilding) >= FProvider.Options.Binding.MaximumModules then
  begin
    raise EOwnedGap.CreateGap(pcgBudget, 'Conditional dependency construction exceeds its depth budget');
  end;
  LDepth := Length(FBuilding);
  SetLength(FBuilding, LDepth + 1);
  FBuilding[LDepth] := AFile.Name;
  try
    while True do
    begin
      Check;
      LRemaining := FProvider.Options.AggregateTokens - FTokens;

      if LRemaining < 1 then
      begin
        raise EOwnedGap.CreateGap(pcgBudget, 'Semantic source roots exceed the aggregate token budget');
      end;
      LConditions := FProvider.Options.Conditions;

      if LRemaining < LConditions.MaximumTokens then
      begin
        LConditions := LConditions.UpToTokens(LRemaining);
      end;
      LConstruction := NewNyxPascalSemanticTranslation(FFiles, AFile, LFacts,
        FWorld, LOptions, FProvider.Options.Syntax, LConditions, FCancel);
      Inc(FTokens, LConstruction.WorkTokens);
      LSource := LConstruction.Source;

      if not LConstruction.Pending or (LConstruction.BindingIssue <> pbiMissingModule) or
        (LConstruction.RelatedName = '') then
      begin
        Break;
      end;

      if not LoadDependency(LConstruction.RelatedName) then
      begin
        Break;
      end;
    end;
  finally
    SetLength(FBuilding, LDepth);
  end;
  LView := NewNyxPascalTranslatedSyntax(LSource, FProvider.Options.Syntax, FCancel);
  LName := LView.Syntax.Scope(0).Name;

  if LName = '' then
  begin
    raise EOwnedGap.CreateGap(pcgSourceIdentity, 'This captured root has no declared module identity', pbiNone, AFile.Name);
  end;

  if (AExpected <> '') and (NyxPascalASCIIKey(LName) <> NyxPascalASCIIKey(AExpected)) then
  begin
    raise EOwnedGap.CreateGap(pcgSourceIdentity, 'This declared module differs from its project membership', pbiNone, AFile.Name);
  end;

  LAt := FWorld.FindModule(LName);

  if LAt >= 0 then
  begin

    if not AReplace or (FWorld.ModuleAt(LAt).Source.Files.FileAt(
      FWorld.ModuleAt(LAt).Source.Occurrence(0).FileIndex).Name <> AFile.Name) then
    begin
      raise EOwnedGap.CreateGap(pcgSourceIdentity, 'Two captured roots declare the same module', pbiNone, AFile.Name);
    end;
  end;
  FWorld := WithNyxPascalSemanticModule(FWorld, LView, FCancel);
end;

procedure TAnalysis.BuildWorld;
var
  LAt: Integer;
  LOther: Integer;
  LFile: TNyxPascalPhysicalFile;
  LSource: TNyxPascalSourceRef;
  LProjectRoot: Boolean;
  LExpected: TNyxText;
begin
  Check;

  if FWorld <> nil then
  begin
    Exit;
  end;
  FFiles := FProvider.Files;
  for LAt := 0 to FRequest.Sources.Count - 1 do
  begin
    Check;
    LSource := FRequest.Sources.FileReference(LAt);
    FFiles := FFiles.WithFile(NyxPascalProjectPhysicalFile(FRequest.Sources, LSource),
      FRequest.Sources.Text(LSource));
  end;
  FWorld := NewNyxPascalSemanticWorld(FProvider.Options.Binding);
  for LAt := 0 to High(FProvider.Roots) do
  begin
    Check;
    LProjectRoot := False;
    for LOther := 0 to FRequest.Sources.Count - 1 do
    begin
      Check;
      LSource := FRequest.Sources.FileReference(LOther);

      if LSource.Role <> npsInclude then
      begin
        LFile := NyxPascalProjectPhysicalFile(FRequest.Sources, LSource);
        LProjectRoot := LProjectRoot or
          (FFiles.Find(LFile) = FFiles.Find(FProvider.Roots[LAt]));
      end;
    end;

    if not LProjectRoot then
    begin
      AddModule(FProvider.Roots[LAt], '');
    end;
  end;
  for LAt := 0 to FRequest.Sources.Count - 1 do
  begin
    Check;
    LSource := FRequest.Sources.FileReference(LAt);

    if LSource.Role <> npsInclude then
    begin
      LExpected := NyxPascalProjectUnit(FRequest.Sources, LSource).Name;
      AddModule(NyxPascalProjectPhysicalFile(FRequest.Sources, LSource), LExpected);
    end;
  end;
  RefineWorld;
  LExpected := NyxPascalProjectUnit(FRequest.Sources, FRequest.Owner).Name;
  FOwner := FWorld.FindModule(LExpected);

  if FOwner < 0 then
  begin
    raise EOwnedGap.CreateGap(pcgSourceIdentity, 'The explicit completion owner has no captured module');
  end;
end;

procedure TAnalysis.RefineWorld;
var
  LAt: Integer;
  LPass: Integer;
  LBefore: Integer;
  LAfter: Integer;
  LDescriptorAt: Integer;
  LView: INyxPascalTranslatedSyntax;
  LDescriptor: INyxPascalModuleSource;
  LRoot: TNyxPascalPhysicalFile;
  LName: TNyxText;
  LChanged: Boolean;

  function Unknowns(const ASource: INyxPascalTranslation): Integer;
  var
    LDiagnostic: Integer;
  begin
    Result := 0;
    for LDiagnostic := 0 to ASource.DiagnosticCount - 1 do
    begin

      if ASource.Diagnostic(LDiagnostic).Issue = ptiUnknownCondition then
      begin
        Inc(Result);
      end;
    end;
  end;

begin
  { All supplied roots now exist. Revisit conditional exports without making
    caller root order a prerequisite. Only a reduction in unresolved conditions
    permits another sweep; ordinary token/pass/load budgets still own all work. }
  LPass := 0;
  repeat
    Check;
    LChanged := False;
    Inc(LPass);
    LAt := 0;
    while LAt < FWorld.Count do
    begin
      Check;
      LView := FWorld.ModuleAt(LAt);
      LBefore := Unknowns(LView.Source);

      if LBefore > 0 then
      begin
        LRoot := LView.Source.Files.FileAt(LView.Source.Occurrence(0).FileIndex);
        LName := LView.Syntax.Scope(0).Name;
        LDescriptor := nil;

        if FProvider.Modules <> nil then
        begin
          LDescriptorAt := FProvider.Modules.Find(NyxPascalModule(LName));

          if LDescriptorAt >= 0 then
          begin
            LDescriptor := FProvider.Modules.Entry(LDescriptorAt);

            if LDescriptor.Root.Name <> LRoot.Name then
            begin
              LDescriptor := nil;
            end;
          end;
        end;
        AddModule(LRoot, LName, LView.Source.InitialContext, LDescriptor, True);
        LAfter := Unknowns(FWorld.ModuleAt(LAt).Source);
        LChanged := LChanged or (LAfter < LBefore);
      end;
      Inc(LAt);
    end;

    if LChanged and (LPass >= FProvider.Options.Conditions.MaximumPasses) then
    begin
      raise EOwnedGap.CreateGap(pcgBudget, 'Conditional module refinement exceeds its pass budget');
    end;
  until not LChanged;
end;

function TAnalysis.LoadDependency(const AName: TNyxText): Boolean;
var
  LAt: Integer;
  LSource: INyxPascalModuleSource;
begin
  Check;
  Result := False;

  if (FProvider.Modules = nil) or (FWorld = nil) or (FWorld.FindModule(AName) >= 0) then
  begin
    Exit;
  end;
  LAt := FProvider.Modules.Find(NyxPascalModule(AName));

  if LAt < 0 then
  begin
    Exit;
  end;

  if (FLoads >= FProvider.Options.DependencyLoads) or
    (FWorld.Count >= FProvider.Options.Binding.MaximumModules) then
  begin
    raise EOwnedGap.CreateGap(pcgBudget, 'The query exceeds its indexed dependency-load budget');
  end;
  LSource := FProvider.Modules.Entry(LAt);
  AddModule(LSource.Root, LSource.Name.Name, LSource.Context, LSource);
  Inc(FLoads);

  if Length(FBuilding) = 0 then
  begin
    RefineWorld;
  end;
  Result := True;
end;

procedure TAnalysis.CaptureAnalysis(AResult: TOwnedResult);
var
  LAt: Integer;
  LView: INyxPascalTranslatedSyntax;
begin
  AResult.Tokens := FTokens;
  AResult.Loads := FLoads;

  if FWorld = nil then
  begin
    Exit;
  end;
  SetLength(AResult.Modules, FWorld.Count);
  for LAt := 0 to FWorld.Count - 1 do
  begin
    LView := FWorld.ModuleAt(LAt);
    AResult.Modules[LAt].Name := NyxPascalModule(LView.Syntax.Scope(0).Name);
    AResult.Modules[LAt].Root := LView.Source.Files.FileAt(LView.Source.Occurrence(0).FileIndex);
  end;
end;

function TAnalysis.Previous(const ALexical: INyxPascalLexicalSnapshot; AIndex: Integer): Integer;
begin
  Result := AIndex;
  while Result >= 0 do
  begin
    Scan;

    if not (ALexical.Token(Result).Kind in [ptkWhitespace, ptkComment, ptkDirective]) then
    begin
      Exit;
    end;
    Dec(Result);
  end;
end;

procedure TAnalysis.Receiver(const ALexical: INyxPascalLexicalSnapshot;
  out AStart, ADot, AExpressionStart: Integer; out AExpected, APrefix: TNyxText);
var
  LAt: Integer;
  LToken: TNyxPascalToken;
  LWord: Integer;
  LPrevious: Integer;
  LDepth: Integer;

  { Find the whole preceding postfix receiver. Balanced windows remain opaque;
    source-owned semantic binding below decides what their result can mean. }
  function Tail(AIndex: Integer): Integer;
  var
    LStack: array of TNyxPascalSymbol;
    LCount: Integer;
    LClose: TNyxPascalSymbol;
    LPart: TNyxPascalToken;
    LCall: Boolean;
  begin
    Inc(LDepth);
    try

      if LDepth > FProvider.Options.Binding.MaximumDepth then
        raise EOwnedGap.CreateGap(pcgBudget, 'Receiver nesting exceeds its configured depth');

      if AIndex < 0 then
        raise EOwnedGap.CreateGap(pcgExpression, 'A postfix receiver is incomplete');
      LPart := ALexical.Token(AIndex);

      if LPart.Kind = ptkWord then
      begin
        AExpressionStart := LPart.Span.Start;
        Result := Previous(ALexical, AIndex - 1);
      end
      else if (LPart.Kind = ptkSymbol) and (LPart.Symbol = psoCaret) then
      begin
        Result := Tail(Previous(ALexical, AIndex - 1));
      end
      else if (LPart.Kind = ptkSymbol) and (LPart.Symbol in [psoCloseParen, psoCloseBracket]) then
      begin
        LCall := LPart.Symbol = psoCloseBracket;
        LCount := 1;
        SetLength(LStack, 1);
        LStack[0] := LPart.Symbol;
        while LCount > 0 do
        begin
          AIndex := Previous(ALexical, AIndex - 1);

          if AIndex < 0 then
            raise EOwnedGap.CreateGap(pcgExpression, 'A postfix group is unfinished');
          LPart := ALexical.Token(AIndex);

          if LPart.Kind <> ptkSymbol then Continue;

          if LPart.Symbol in [psoCloseParen, psoCloseBracket] then
          begin

            if LCount >= FProvider.Options.Binding.MaximumDepth then
              raise EOwnedGap.CreateGap(pcgBudget, 'Receiver group nesting exceeds its configured depth');
            SetLength(LStack, LCount + 1);
            LStack[LCount] := LPart.Symbol;
            Inc(LCount);
          end
          else if LPart.Symbol in [psoOpenParen, psoOpenBracket] then
          begin
            LClose := psoCloseParen;

            if LPart.Symbol = psoOpenBracket then LClose := psoCloseBracket;

            if LStack[LCount - 1] <> LClose then
              raise EOwnedGap.CreateGap(pcgExpression, 'Postfix delimiters are crossed');
            Dec(LCount);
          end;
        end;
        AExpressionStart := LPart.Span.Start;
        Result := Previous(ALexical, AIndex - 1);

        if Result >= 0 then
        begin
          LPart := ALexical.Token(Result);
          LCall := LCall or ((LPart.Kind = ptkWord) and (LPart.Word = pkwNone)) or
            ((LPart.Kind = ptkSymbol) and (LPart.Symbol in [psoCloseParen, psoCloseBracket, psoCaret]));
        end;

        if LCall then Result := Tail(Result);
      end
      else raise EOwnedGap.CreateGap(pcgExpression, 'This receiver needs further expression inference');

      if (Result >= 0) and (ALexical.Token(Result).Symbol = psoDot) then
        Result := Tail(Previous(ALexical, Result - 1));
    finally
      Dec(LDepth);
    end;
  end;
begin
  AStart := FRequest.Caret;
  AExpected := '';
  APrefix := '';
  LAt := ALexical.TokenAt(FRequest.Caret);
  LToken := ALexical.Token(LAt);
  LWord := -1;

  if (LToken.Kind = ptkWord) then
  begin
    LWord := LAt;
  end
  else if (LAt > 0) and (ALexical.Token(LAt - 1).Kind = ptkWord) and
    (ALexical.Token(LAt - 1).Span.Finish = FRequest.Caret) then
  begin
    LWord := LAt - 1;
  end
  else if (LToken.Kind in [ptkString, ptkCharacter, ptkComment, ptkDirective]) and
    (LToken.Span.Start < FRequest.Caret) then
  begin
    raise EOwnedGap.CreateGap(pcgExpression, 'Completion caret is inside literal, comment or directive text');
  end;

  if LWord >= 0 then
  begin
    LToken := ALexical.Token(LWord);
    AStart := LToken.Span.Start;
    AExpected := ALexical.Text(LWord);
    APrefix := ALexical.Slice(NyxPascalSpan(AStart, FRequest.Caret - AStart));

    if LToken.Escaped and (APrefix <> '') then
    begin
      Delete(APrefix, 1, 1);
    end;
    LAt := LWord;
  end;
  LAt := Previous(ALexical, LAt - 1);

  if (LAt < 0) or (ALexical.Token(LAt).Symbol <> psoDot) then
  begin
    raise EOwnedGap.CreateGap(pcgExpression, 'This query needs a supported declared receiver before a member dot');
  end;
  ADot := ALexical.Token(LAt).Span.Start;
  LDepth := 0;
  LPrevious := Tail(Previous(ALexical, LAt - 1));

  if (LPrevious >= 0) and (ALexical.Token(LPrevious).Kind = ptkSymbol) and
    (ALexical.Token(LPrevious).Symbol in [psoCloseParen, psoCloseBracket,
      psoCaret, psoAt, psoPlus, psoMinus]) then
    raise EOwnedGap.CreateGap(pcgExpression, 'This receiver prefix needs expression inference');
end;

function TAnalysis.Caller(const AFile: TNyxPascalPhysicalFile; ADot, AExpressionStart: Integer;
  out AExpressionWindow: TNyxPascalSpan): INyxPascalBindingSite;
var
  LView: INyxPascalTranslatedSyntax;
  LAt: Integer;
  LFile: Integer;
  LOffset: Integer;
  LFound: Integer;
  LStart: Integer;
begin
  LView := FWorld.ModuleAt(FOwner);
  LFile := LView.Source.Files.Find(AFile);
  LFound := -1;
  LOffset := -1;
  LStart := -1;
  for LAt := 0 to LView.Source.OccurrenceCount - 1 do
  begin
    Check;

    if LView.Source.Occurrence(LAt).FileIndex = LFile then
    begin

      if LFound >= 0 then
      begin
        raise EOwnedGap.CreateGap(pcgRepeatedInclude, 'This file has multiple occurrences in its owner; an explicit occurrence query is required');
      end;
      LFound := LAt;
      LOffset := LView.VirtualAt(LAt, ADot);
      LStart := LView.VirtualAt(LAt, AExpressionStart);
    end;
  end;

  if (LOffset < 0) or (LStart < 0) or (LStart >= LOffset) then
  begin
    raise EOwnedGap.CreateGap(pcgPhysicalContext, 'The member dot has no known-active physical context in its explicit owner');
  end;
  AExpressionWindow := NyxPascalSpan(LStart, LOffset - LStart);
  Result := FWorld.At(FOwner, LView.Syntax.ScopeAt(LOffset), LOffset);
end;

{ UI detail is bounded by scalar, never by a byte/UTF-16 unit boundary. }
function ShortDetail(const AText: TNyxText): TNyxText;
var
  LAt: Integer;
  LScalar: Integer;
  LCount: Integer;
begin
  LAt := 1;
  LCount := 0;
  while (LAt <= Length(AText)) and (LCount < 2000) do
  begin
    NyxNextScalar(AText, LAt, LScalar);
    Inc(LCount);
  end;
  Result := Copy(AText, 1, LAt - 1);
end;

function CompletionKind(AKind: TNyxPascalSyntaxDeclarationKind): TNyxPascalCompletionKind;
begin
  case AKind of
    psdkType:
      begin
        Result := pckType;
      end;
    psdkRoutine:
      begin
        Result := pckMethod;
      end;
    psdkProperty:
      begin
        Result := pckProperty;
      end;
    psdkField:
      begin
        Result := pckField;
      end;
    psdkVariable:
      begin
        Result := pckVariable;
      end;
    psdkParameter:
      begin
        Result := pckParameter;
      end;
    psdkConstant, psdkEnumValue, psdkResourceString:
      begin
        Result := pckConstant;
      end;
    psdkLabel:
      begin
        { Label navigation needs a statement context. A label cannot be emitted
          as an ordinary expression-member completion row. }
        raise ENyxPascalBinding.Create('A statement label is not a value member');
      end;
  end;
end;

function TAnalysis.Run: INyxPascalCompletionResult;
var
  LFile: TNyxPascalPhysicalFile;
  LLex: INyxPascalLexicalSnapshot;
  LSite: INyxPascalBindingSite;
  LPrefix: TNyxText;
  LExpected: TNyxText;
  LStart: Integer;
  LDot: Integer;
  LBinding: INyxPascalResolution;
  LType: INyxPascalSemanticType;
  LAccess: TNyxPascalMemberAccess;
  LQueries: INyxPascalMemberQueries;
  LPage: INyxPascalMemberPage;
  LTotal: INyxPascalMemberTotal;
  LAt: Integer;
  LSymbol: INyxPascalBoundSymbol;
  LView: INyxPascalTranslatedSyntax;
  LOrigin: TNyxPascalSyntaxOrigin;
  LRows: array of TNyxPascalCompletionItem;
  LResult: TOwnedResult;
  LInsert: TNyxText;
  LNameLex: INyxPascalLexicalSnapshot;
  LExpressions: INyxPascalExpressionQueries;
  LExpressionWindow: TNyxPascalSpan;
begin
  BuildWorld;
  LFile := NyxPascalProjectPhysicalFile(FRequest.Sources, FRequest.FileReference);
  try
    LLex := FWorld.ModuleAt(FOwner).Source.Physical(
      FWorld.ModuleAt(FOwner).Source.Files.Find(LFile));
  except
    on ENyxPascalContext do
    begin
      raise EOwnedGap.CreateGap(pcgPhysicalContext, 'The requested file has no active captured inclusion');
    end;
  end;

  if not FReceiverReady then
  begin
    Receiver(LLex, FReceiverStart, FReceiverDot, FReceiverExpressionStart,
      FReceiverExpected, FReceiverPrefix);
    FReceiverReady := True;
  end;
  LStart := FReceiverStart;
  LDot := FReceiverDot;
  LExpected := FReceiverExpected;
  LPrefix := FReceiverPrefix;
  LSite := Caller(LFile, LDot, FReceiverExpressionStart, LExpressionWindow);
  Supports(FWorld, INyxPascalExpressionQueries, LExpressions);
  LBinding := LExpressions.ExpressionType(LSite, LExpressionWindow, LAccess, FCancel);

  if LBinding.Status <> pbsBound then
  begin
    raise EOwnedGap.CreateGap(pcgBinding, LBinding.MessageText,
      LBinding.Issue, LBinding.RelatedName);
  end;
  LType := LBinding.BoundType;
  Supports(FWorld, INyxPascalMemberQueries, LQueries);
  LPage := LQueries.Members(LType, LSite, TNyxPascalMemberQuery.Defaults
    .Matching(LPrefix).Skip(FRequest.Offset).Take(FRequest.Limit).ForAccess(LAccess).CountAll, FCancel);

  if LPage.ResultSet.Status <> pbsBound then
  begin
    raise EOwnedGap.CreateGap(pcgBinding, LPage.ResultSet.MessageText,
      LPage.ResultSet.Issue, LPage.ResultSet.RelatedName);
  end;
  Supports(LPage, INyxPascalMemberTotal, LTotal);
  LResult := TOwnedResult.Create;
  Result := LResult;
  SetLength(LRows, LPage.ResultSet.Count);
  SetLength(LResult.Definitions, Length(LRows));
  SetLength(LResult.DefinitionSources, Length(LRows));
  SetLength(LResult.DefinitionViews, Length(LRows));
  for LAt := 0 to High(LRows) do
  begin
    Check;
    LSymbol := LPage.ResultSet.SymbolAt(LAt);
    LView := FWorld.ModuleAt(LSymbol.ModuleIndex);
    LInsert := LSymbol.Declaration.Name;
    LNameLex := NewNyxPascalLexicalSnapshot(LInsert);

    if LNameLex.Token(0).Word <> pkwNone then
    begin
      LInsert := '&' + LInsert;
    end;
    LRows[LAt] := NyxPascalCompletion(LSymbol.Declaration.Name, LInsert,
      ShortDetail(LView.Syntax.Lexical.Slice(LSymbol.Declaration.HeaderSpan)),
      CompletionKind(LSymbol.Declaration.Kind));
    LOrigin := LSymbol.Origin;
    LResult.Definitions[LAt].FileReference := LView.Source.Files.FileAt(LOrigin.FileIndex);
    LResult.Definitions[LAt].Root := LView.Source.Files.FileAt(LView.Source.Occurrence(0).FileIndex);
    LResult.Definitions[LAt].Span := LOrigin.PhysicalSpan;
    LResult.Definitions[LAt].Occurrence := LOrigin.Occurrence;
    LResult.DefinitionSources[LAt] := NyxPascalOccurrenceLexical(
      LView.Source, LOrigin.Occurrence);
    LResult.DefinitionViews[LAt] := NyxPascalSourceViewAt(LView.Source.Files, LOrigin.FileIndex);
  end;
  Check;
  LResult.Base := NewNyxPascalCompletionResult(FRequest, pcsReady, '',
    LStart, LExpected, LRows, LTotal.Total);
  CaptureAnalysis(LResult);
end;

function TAnalysis.Complete: INyxPascalCompletionResult;
var
  LReply: TOwnedResult;
begin
  try
    Check;
    while True do
    begin
      try
        Exit(Run);
      except
        on LGap: EOwnedGap do
        begin

          if (LGap.GapKind = pcgBinding) and (LGap.Issue = pbiMissingModule) and
            (LGap.Related <> '') and LoadDependency(LGap.Related) then
          begin
            Continue;
          end;
          raise;
        end;
      end;
    end;
  except
    on LError: EOwnedGap do
    begin
      LReply := TOwnedResult.Create;
      Result := LReply;
      LReply.GapValue := LError.GapKind;
      LReply.IssueValue := LError.Issue;
      LReply.RelatedValue := LError.Related;
      LReply.Base := NewNyxPascalCompletionResult(FRequest, pcsUnavailable,
        ShortDetail(TNyxText(LError.Message) + ' ' + LError.Related), 0, '', [], 0);
    end;
    on ENyxPascalAnalysisCancelled do
    begin
      LReply := TOwnedResult.Create;
      Result := LReply;
      LReply.GapValue := pcgCancelled;
      LReply.Base := NewNyxPascalCompletionResult(FRequest, pcsCancelled,
        'Owned Pascal analysis was cancelled', 0, '', [], 0);
    end;
    on LError: ENyxPascalModuleContext do
    begin
      LReply := TOwnedResult.Create;
      Result := LReply;
      LReply.GapValue := pcgBinding;
      LReply.IssueValue := pbiCompilerContext;
      LReply.RelatedValue := LError.RelatedModule;
      LReply.Base := NewNyxPascalCompletionResult(FRequest, pcsUnavailable,
        TNyxText(LError.Message), 0, '', [], 0);
    end;
    on LError: Exception do
    begin
      LReply := TOwnedResult.Create;
      Result := LReply;
      LReply.GapValue := pcgInternal;

      if (LError is ENyxPascalAnalysisWorkLimit) or
        (LError is ENyxPascalLexicalLimit) or (LError is ENyxPascalBindingLimit) or
        (LError is ENyxPascalTranslationLimit) then
      begin
        LReply.GapValue := pcgBudget;
        LReply.Base := NewNyxPascalCompletionResult(FRequest, pcsUnavailable,
          'Owned Pascal analysis exceeded a configured budget', 0, '', [], 0);
      end
      else
      begin
        LReply.Base := NewNyxPascalCompletionResult(FRequest, pcsFailed,
          'Owned Pascal analysis could not complete this captured query', 0, '', [], 0);
      end;
    end;
  end;
  CaptureAnalysis(LReply);
end;

function TOwnedProvider.Complete(const ARequest: INyxPascalCompletionRequest): INyxPascalCompletionResult;
begin
  Result := CompleteWithCancellation(ARequest, nil);
end;

function TOwnedProvider.CompleteWithCancellation(const ARequest: INyxPascalCompletionRequest;
  const ACancellation: INyxCancellationScope): INyxPascalCompletionResult;
var
  LAnalysis: TAnalysis;
begin
  LAnalysis := TAnalysis.Create(Self, ARequest, ACancellation);
  try
    Result := LAnalysis.Complete;
  finally
    LAnalysis.Free;
  end;
end;

function NewNyxOwnedPascalCompletionProvider(const AContext: INyxPascalCompilerContext;
  const AFiles: INyxPascalFileCatalog; const ASDKRoots: array of TNyxPascalPhysicalFile;
  const AOptions: TNyxOwnedPascalCompletionOptions): INyxPascalCompletionProvider;
var
  LProvider: TOwnedProvider;
  LAt: Integer;
  LOther: Integer;
begin
  AOptions.Validate;
  LProvider := TOwnedProvider.Create;
  Result := LProvider;
  LProvider.Context := CaptureNyxPascalCompilerContext(AContext);
  LProvider.Files := CaptureNyxPascalFiles(AFiles);
  LProvider.Options := AOptions;

  if Length(ASDKRoots) > AOptions.Binding.MaximumModules then
  begin
    raise ENyxPascalBindingLimit.Create('Captured SDK roots exceed the configured module budget');
  end;
  SetLength(LProvider.Roots, Length(ASDKRoots));
  for LAt := 0 to High(ASDKRoots) do
  begin
    LProvider.Roots[LAt] := NyxPascalPhysicalFile(ASDKRoots[LAt].Name);

    if LProvider.Files.Find(LProvider.Roots[LAt]) < 0 then
    begin
      raise ENyxPascalContext.Create('A completion SDK root needs its exact captured source file');
    end;
    for LOther := 0 to LAt - 1 do
    begin

      if LProvider.Files.Find(LProvider.Roots[LAt]) = LProvider.Files.Find(LProvider.Roots[LOther]) then
      begin
        raise ENyxPascalContext.Create('Completion SDK roots must have independent physical identities');
      end;
    end;
  end;
end;

function NewNyxIndexedPascalCompletionProvider(const AContext: INyxPascalCompilerContext;
  const AFiles: INyxPascalFileCatalog; const AModules: INyxPascalModuleCatalog;
  const AOptions: TNyxOwnedPascalCompletionOptions): INyxPascalCompletionProvider;
var
  LProvider: TOwnedProvider;
begin
  AOptions.Validate;
  LProvider := TOwnedProvider.Create;
  Result := LProvider;
  LProvider.Context := CaptureNyxPascalCompilerContext(AContext);
  LProvider.Files := CaptureNyxPascalFiles(AFiles);
  LProvider.Modules := CaptureNyxPascalModules(AModules, LProvider.Files);
  LProvider.Options := AOptions;
end;
end.
