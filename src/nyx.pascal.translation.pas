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
unit nyx.pascal.translation;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.scheduler, nyx.pascal.lexing, nyx.pascal.context, nyx.pascal.files;

type
  { Translation resource refusal is atomic. It differs from a recoverable missing
    include/unknown condition, which remains visible in the returned diagnostics. }
  ENyxPascalTranslationLimit = class(ENyxPascalContext);
  { Explicit captured-context gaps; none is disguised as an inactive branch or a
    successful compiler/type-resolution result. Positions are always physical. }
  TNyxPascalTranslationIssue = (ptiUnknownCondition, ptiMissingInclude,
    ptiIncludeCycle, ptiUnbalancedConditional, ptiUnsupportedDirective,
    ptiMalformedDirective, ptiLexicalIssue, ptiLexicalPolicyChange);
  { Immutable physical diagnostic. Occurrence distinguishes repeated includes;
    text is English guidance with no machine paths or borrowed exceptions. }
  TNyxPascalTranslationDiagnostic = record
  private
    FIssue: TNyxPascalTranslationIssue;
    FOccurrence: Integer;
    FSpan: TNyxPascalSpan;
    FMessage: TNyxText;
  public
    property Issue: TNyxPascalTranslationIssue read FIssue;
    property Occurrence: Integer read FOccurrence;
    property Span: TNyxPascalSpan read FSpan;
    property MessageText: TNyxText read FMessage;
  end;
  { One inclusion, not just one filename. Parent is -1 for the explicit root;
    IncludeSpan belongs to the parent's physical file. Token interval includes
    descendants; entry/exit state indices preserve textual include side effects. }
  TNyxPascalInclusion = record
  private
    FFile: Integer;
    FParent: Integer;
    FIncludeSpan: TNyxPascalSpan;
    FFirst: Integer;
    FCount: Integer;
    FEntry: Integer;
    FExit: Integer;
  public
    property FileIndex: Integer read FFile;
    property Parent: Integer read FParent;
    property IncludeSpan: TNyxPascalSpan read FIncludeSpan;
    property FirstToken: Integer read FFirst;
    property TokenCount: Integer read FCount;
    property EntryState: Integer read FEntry;
    property ExitState: Integer read FExit;
  end;
  { One source token with its inclusion and pre-token compiler state. Activity
    describes conditional participation only, never semantic validity. Physical
    token ordinals belong to their exact occurrence's lexical snapshot, including
    repeated visits with different policies. Records own no interfaces. }
  TNyxPascalTranslatedToken = record
  private
    FOccurrence: Integer;
    FPhysicalToken: Integer;
    FState: Integer;
    FActivity: TNyxPascalKnowledge;
  public
    property Occurrence: Integer read FOccurrence;
    property PhysicalToken: Integer read FPhysicalToken;
    property State: Integer read FState;
    property Activity: TNyxPascalKnowledge read FActivity;
  end;
  { Independent analysis budgets. Expanded bytes count repeated physical reads;
    token budgets include trivia/directives and only the root EOF. No limit
    silently truncates a published token/context/occurrence collection. }
  TNyxPascalTranslationOptions = record
  private
    FDefined: Boolean;
    FLexical: TNyxPascalLexicalOptions;
    FTokens: Integer;
    FBytes: Integer;
    FDepth: Integer;
    FOccurrences: Integer;
    FStates: Integer;
    FDiagnostics: Integer;
  public
    { Explicit defaults: 400000 tokens, 16 MiB expanded input, 64 include/condition
      levels, 4096 occurrences/states and 1024 diagnostics. Zeroed options refuse;
      all fluent setters return validated independent copies. }
    class function Defaults: TNyxPascalTranslationOptions; static;
    { Capture initial lexical policy and immutable per-occurrence budgets.
      Known source directives may change comment nesting between tokens. }
    function Lexical(const AValue: TNyxPascalLexicalOptions): TNyxPascalTranslationOptions;
    { Includes trivia/directives and the root EOF, across repeated includes. }
    function UpToTokens(AValue: Integer): TNyxPascalTranslationOptions;
    { UTF-8 bytes of physical reads, including every repeated inclusion. }
    function UpToExpandedBytes(AValue: Integer): TNyxPascalTranslationOptions;
    { Bounds both recursive inclusions and the conditional frame stack. }
    function UpToIncludeDepth(AValue: Integer): TNyxPascalTranslationOptions;
    { The root counts once; repeated files receive independent occurrences. }
    function UpToOccurrences(AValue: Integer): TNyxPascalTranslationOptions;
    { Limits retained immutable compiler states; adjacent equal states coalesce. }
    function UpToStates(AValue: Integer): TNyxPascalTranslationOptions;
    { Excess unresolved constructs refuse instead of silently dropping guidance. }
    function UpToDiagnostics(AValue: Integer): TNyxPascalTranslationOptions;
    { Undefined or nonpositive budgets raise before capturing any input. }
    procedure Validate;
    property LexicalOptions: TNyxPascalLexicalOptions read FLexical;
    property MaximumTokens: Integer read FTokens;
    property MaximumExpandedBytes: Integer read FBytes;
    property MaximumIncludeDepth: Integer read FDepth;
    property MaximumOccurrences: Integer read FOccurrences;
    property MaximumStates: Integer read FStates;
    property MaximumDiagnostics: Integer read FDiagnostics;
  end;
  { Owns frozen files, compiler facts, lexical caches and include-aware tokens.
    All indices/getters are checked and snapshot-local. Resolved means encountered
    translation constructs have known supported context, not grammar, type binding,
    compiler acceptance or execution. Unknown initial facts remain explicit.
    No mutable provider/editor/filesystem/process survives construction. }
  INyxPascalTranslation = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570C03}']
    function Files: INyxPascalFileCatalog;
    function InitialContext: INyxPascalCompilerContext;
    function Resolved: Boolean;
    function Count: Integer;
    function Token(AIndex: Integer): TNyxPascalTranslatedToken;
    function PhysicalToken(AIndex: Integer): TNyxPascalToken;
    function Text(AIndex: Integer): TNyxText;
    { Legacy file-only access refuses differing occurrence tokenizations/policies.
      Prefer NyxPascalOccurrenceLexical when an origin supplies its occurrence. }
    function Physical(AFile: Integer): INyxPascalLexicalSnapshot;
    function OccurrenceCount: Integer;
    function Occurrence(AIndex: Integer): TNyxPascalInclusion;
    function StateCount: Integer;
    function State(AIndex: Integer): INyxPascalCompilerContext;
    function DiagnosticCount: Integer;
    function Diagnostic(AIndex: Integer): TNyxPascalTranslationDiagnostic;
  end;
  { Optional precise lexical views for repeated textual includes. PhysicalToken
    ordinals belong to this occurrence's snapshot, never another visit of its
    file. Returned snapshots are detached and retain no translation/visitor. }
  INyxPascalTranslationLexicalContexts = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570C04}']
    function OccurrenceLexical(AOccurrence: Integer): INyxPascalLexicalSnapshot;
  end;

{ Read exact occurrence tokens, with legacy file-only fallback for alternate
  translators which have not exposed contextual lexing. Indices remain checked. }
function NyxPascalOccurrenceLexical(const ATranslation: INyxPascalTranslation;
  AOccurrence: Integer): INyxPascalLexicalSnapshot;

{ Freeze caller-serialized alternate inputs once and traverse only captured files.
  Root must name an explicit captured unit/program file; no include owner is guessed.
  Conditional define/option changes flow out of textual includes. Unknown effects
  revoke state knowledge. Inactive includes perform no lookup or state mutation.
  Cancellation is borrowed only during construction; every reader must join before
  its owner edits input files. No compiler application is run by translation. }
function NewNyxPascalTranslation(const AFiles: INyxPascalFileCatalog;
  const ARoot: TNyxPascalPhysicalFile; const AContext: INyxPascalCompilerContext;
  const AOptions: TNyxPascalTranslationOptions;
  const ACancellation: INyxCancellationScope = nil): INyxPascalTranslation;

implementation

uses SysUtils, nyx.bytes;

type
  { File-only compatibility view. The first immutable occurrence is retained;
    differing later policies/tokens revoke ambiguous legacy access. }
  TPhysicalCache = class
    Lexical: INyxPascalLexicalSnapshot;
    Ambiguous: Boolean;
  end;
  { Borrowed only by synchronous token visitors while construction is active. }
  TTranslation = class;
  { One lexical construction's visitor. Owner is borrowed, never reference counted
    back to its translation; recursive includes get independent visitors. }
  TTranslationReader = class(TInterfacedObject, INyxPascalLexicalObserver)
  public
    Owner: TTranslation;
    Occurrence: Integer;
    Included: Boolean;
    procedure AfterToken(AIndex: Integer; const AToken: TNyxPascalToken;
      const AText: TNyxText; var AAllowNestedComments: Boolean);
  end;
  { One conditional frame may cross include boundaries. Taken is three-valued;
    an unknown branch's writes revoke facts rather than merging a guessed path. }
  TConditionalFrame = record
    Parent: TNyxPascalKnowledge;
    Taken: TNyxPascalKnowledge;
    HasElse: Boolean;
    Occurrence: Integer;
    Span: TNyxPascalSpan;
  end;
  { Owns every cache/immutable vector. Construction alone retains cancellation;
    all published references point outward to data, never back to a document. }
  TTranslation = class(TInterfacedObject, INyxPascalTranslation,
    INyxPascalTranslationLexicalContexts)
  private
    FFiles: INyxPascalFileCatalog;
    FInitial: INyxPascalCompilerContext;
    FContext: INyxPascalCompilerContext;
    FOptions: TNyxPascalTranslationOptions;
    FPhysical: array of TPhysicalCache;
    FLexical: array of INyxPascalLexicalSnapshot;
    FTokens: array of TNyxPascalTranslatedToken;
    FOccurrences: array of TNyxPascalInclusion;
    FStates: array of INyxPascalCompilerContext;
    FDiagnostics: array of TNyxPascalTranslationDiagnostic;
    FFrames: array of TConditionalFrame;
    FTokenCount: Integer;
    FOccurrenceCount: Integer;
    FStateCount: Integer;
    FDiagnosticCount: Integer;
    FBytes: Integer;
    FDepth: Integer;
    FActive: TNyxPascalKnowledge;
    FResolved: Boolean;
    FBlocked: Boolean;
    FNested: Boolean;
    FModeSeen: Boolean;
    FCancellation: INyxCancellationScope;
    procedure CheckCancellation;
    procedure Problem(AIssue: TNyxPascalTranslationIssue; AOccurrence: Integer;
      const ASpan: TNyxPascalSpan; const AMessage: TNyxText);
    procedure SaveState;
    procedure AddToken(AOccurrence, AToken: Integer);
    procedure Visit(AFile, AParent: Integer; const AInclude: TNyxPascalSpan);
    procedure Directive(AOccurrence: Integer; const AToken: TNyxPascalToken;
      const AText: TNyxText);
    function Condition(const AKind, AValue: TNyxText): TNyxPascalKnowledge;
    procedure ChangeSymbol(const AName: TNyxText; AKind: TNyxPascalDefineKind);
    procedure ChangeOption(AName: Char; AValue: TNyxPascalKnowledge);
    procedure UnknownEffect;
  public
    constructor Create(const AFiles: INyxPascalFileCatalog;
      const ARoot: TNyxPascalPhysicalFile; const AContext: INyxPascalCompilerContext;
      const AOptions: TNyxPascalTranslationOptions;
      const ACancellation: INyxCancellationScope);
    destructor Destroy; override;
    function Files: INyxPascalFileCatalog;
    function InitialContext: INyxPascalCompilerContext;
    function Resolved: Boolean;
    function Count: Integer;
    function Token(AIndex: Integer): TNyxPascalTranslatedToken;
    function PhysicalToken(AIndex: Integer): TNyxPascalToken;
    function Text(AIndex: Integer): TNyxText;
    function Physical(AFile: Integer): INyxPascalLexicalSnapshot;
    function OccurrenceLexical(AOccurrence: Integer): INyxPascalLexicalSnapshot;
    function OccurrenceCount: Integer;
    function Occurrence(AIndex: Integer): TNyxPascalInclusion;
    function StateCount: Integer;
    function State(AIndex: Integer): INyxPascalCompilerContext;
    function DiagnosticCount: Integer;
    function Diagnostic(AIndex: Integer): TNyxPascalTranslationDiagnostic;
  end;

function Capacity(ACount, AMaximum: Integer): Integer;
begin
  Result := AMaximum;

  if 32 + ACount div 2 <= AMaximum - ACount then
  begin
    Result := ACount + 32 + ACount div 2;
  end;
end;

class function TNyxPascalTranslationOptions.Defaults: TNyxPascalTranslationOptions;
begin
  Result := Default(TNyxPascalTranslationOptions);
  Result.FDefined := True;
  Result.FLexical := TNyxPascalLexicalOptions.Defaults;
  Result.FTokens := 400000;
  Result.FBytes := 16 * 1024 * 1024;
  Result.FDepth := 64;
  Result.FOccurrences := 4096;
  Result.FStates := 4096;
  Result.FDiagnostics := 1024;
end;

procedure TNyxPascalTranslationOptions.Validate;
begin
  FLexical.Validate;

  if not FDefined or (FTokens < 1) or (FBytes < 1) or (FDepth < 1) or
    (FOccurrences < 1) or (FStates < 1) or (FDiagnostics < 1) then
  begin
    raise ENyxPascalContext.Create('Use explicit positive Pascal translation budgets');
  end;
end;

function TNyxPascalTranslationOptions.Lexical(
  const AValue: TNyxPascalLexicalOptions): TNyxPascalTranslationOptions;
begin
  Result := Self;
  Result.FLexical := AValue;
  Result.Validate;
end;

function TNyxPascalTranslationOptions.UpToTokens(AValue: Integer): TNyxPascalTranslationOptions;
begin
  Result := Self;
  Result.FTokens := AValue;
  Result.Validate;
end;

function TNyxPascalTranslationOptions.UpToExpandedBytes(AValue: Integer): TNyxPascalTranslationOptions;
begin
  Result := Self;
  Result.FBytes := AValue;
  Result.Validate;
end;

function TNyxPascalTranslationOptions.UpToIncludeDepth(AValue: Integer): TNyxPascalTranslationOptions;
begin
  Result := Self;
  Result.FDepth := AValue;
  Result.Validate;
end;

function TNyxPascalTranslationOptions.UpToOccurrences(AValue: Integer): TNyxPascalTranslationOptions;
begin
  Result := Self;
  Result.FOccurrences := AValue;
  Result.Validate;
end;

function TNyxPascalTranslationOptions.UpToStates(AValue: Integer): TNyxPascalTranslationOptions;
begin
  Result := Self;
  Result.FStates := AValue;
  Result.Validate;
end;

function TNyxPascalTranslationOptions.UpToDiagnostics(AValue: Integer): TNyxPascalTranslationOptions;
begin
  Result := Self;
  Result.FDiagnostics := AValue;
  Result.Validate;
end;


constructor TTranslation.Create(const AFiles: INyxPascalFileCatalog;
  const ARoot: TNyxPascalPhysicalFile; const AContext: INyxPascalCompilerContext;
  const AOptions: TNyxPascalTranslationOptions;
  const ACancellation: INyxCancellationScope);
var
  LRoot: Integer;
  LAt: Integer;
begin
  inherited Create;
  AOptions.Validate;
  FOptions := AOptions;
  FCancellation := ACancellation;
  CheckCancellation;
  try
    FFiles := CaptureNyxPascalFiles(AFiles);
    FInitial := CaptureNyxPascalCompilerContext(AContext);
    FContext := FInitial;
    FNested := FOptions.LexicalOptions.AllowNestedComments;

    if (FInitial.Feature(ppfNestedComments) <> ppkUnknown) and
      ((FInitial.Feature(ppfNestedComments) = ppkTrue) <>
        FOptions.LexicalOptions.AllowNestedComments) then
    begin
      raise ENyxPascalContext.Create('Captured comment facts disagree with the lexical policy');
    end;
    SetLength(FPhysical, FFiles.Count);
    LRoot := FFiles.Find(ARoot);

    if LRoot < 0 then
    begin
      raise ENyxPascalContext.Create('The explicit translation root was not captured');
    end;
    FResolved := True;
    FActive := ppkTrue;
    SaveState;
    Visit(LRoot, -1, NyxPascalSpan(0, 0));

    if not FBlocked then
    begin
      for LAt := 0 to High(FFrames) do
      begin
        Problem(ptiUnbalancedConditional, FFrames[LAt].Occurrence, FFrames[LAt].Span,
          'Finish this captured conditional block');
      end;
    end;
  finally
    FCancellation := nil;
  end;
end;

destructor TTranslation.Destroy;
var
  LAt: Integer;
begin
  for LAt := 0 to High(FPhysical) do
  begin
    FPhysical[LAt].Free;
  end;
  inherited Destroy;
end;

procedure TTranslation.CheckCancellation;
begin

  if (FCancellation <> nil) and FCancellation.Cancelled then
  begin
    raise ENyxPascalAnalysisCancelled.Create('Pascal translation cancelled');
  end;
end;

procedure TTranslation.Problem(AIssue: TNyxPascalTranslationIssue; AOccurrence: Integer;
  const ASpan: TNyxPascalSpan; const AMessage: TNyxText);
begin
  FResolved := False;

  if FDiagnosticCount >= FOptions.MaximumDiagnostics then
  begin
    raise ENyxPascalTranslationLimit.Create('Pascal translation diagnostics exceed their budget');
  end;

  if FDiagnosticCount = Length(FDiagnostics) then
  begin
    SetLength(FDiagnostics, Capacity(FDiagnosticCount, FOptions.MaximumDiagnostics));
  end;
  FDiagnostics[FDiagnosticCount].FIssue := AIssue;
  FDiagnostics[FDiagnosticCount].FOccurrence := AOccurrence;
  FDiagnostics[FDiagnosticCount].FSpan := ASpan;
  FDiagnostics[FDiagnosticCount].FMessage := AMessage;
  Inc(FDiagnosticCount);
end;

procedure TTranslation.SaveState;
begin

  if (FStateCount > 0) and FStates[FStateCount - 1].SameFacts(FContext) then
  begin
    Exit;
  end;

  if FStateCount >= FOptions.MaximumStates then
  begin
    raise ENyxPascalTranslationLimit.Create('Pascal translation states exceed their budget');
  end;

  if FStateCount = Length(FStates) then
  begin
    SetLength(FStates, Capacity(FStateCount, FOptions.MaximumStates));
  end;
  FStates[FStateCount] := FContext;
  Inc(FStateCount);
end;

procedure TTranslation.AddToken(AOccurrence, AToken: Integer);
begin
  CheckCancellation;

  if FTokenCount >= FOptions.MaximumTokens then
  begin
    raise ENyxPascalTranslationLimit.Create('Pascal translated tokens exceed their budget');
  end;

  if FTokenCount = Length(FTokens) then
  begin
    SetLength(FTokens, Capacity(FTokenCount, FOptions.MaximumTokens));
  end;
  FTokens[FTokenCount].FOccurrence := AOccurrence;
  FTokens[FTokenCount].FPhysicalToken := AToken;
  FTokens[FTokenCount].FState := FStateCount - 1;
  FTokens[FTokenCount].FActivity := FActive;

  if FBlocked then
  begin
    FTokens[FTokenCount].FActivity := ppkUnknown;
  end;
  Inc(FTokenCount);
end;

procedure TTranslation.UnknownEffect;
begin
  { An unknown textual effect may also change comment/string lexing. Keep the
    remaining physical capture for inspection, but do not interpret another
    directive using stale lexical defaults or manufacture unclosed-frame errors. }
  FContext := FContext.UnknownState;
  FBlocked := True;
  SaveState;
end;

procedure TTranslationReader.AfterToken(AIndex: Integer; const AToken: TNyxPascalToken;
  const AText: TNyxText; var AAllowNestedComments: Boolean);
begin

  if not Included or (AToken.Kind <> ptkEOF) then
  begin
    Owner.AddToken(Occurrence, AIndex);

    if (AToken.Kind = ptkDirective) and not Owner.FBlocked then
    begin
      Owner.Directive(Occurrence, AToken, AText);
    end;
  end;
  { Includes share the caller's compiler state. In particular a nested include
    may change the policy used for this reader's very next physical token. }
  AAllowNestedComments := Owner.FNested;
end;

function SameLexicalView(const ALeft, ARight: INyxPascalLexicalSnapshot;
  const ACancellation: INyxCancellationScope): Boolean;
var
  LAt: Integer;
  LLeft: TNyxPascalToken;
  LRight: TNyxPascalToken;
begin
  Result := False;

  if (ALeft.Count <> ARight.Count) or
    (ALeft.Options.AllowNestedComments <> ARight.Options.AllowNestedComments) then
  begin
    Exit;
  end;
  for LAt := 0 to ALeft.Count - 1 do
  begin

    if (ACancellation <> nil) and ACancellation.Cancelled then
    begin
      raise ENyxPascalAnalysisCancelled.Create('Pascal lexical comparison cancelled');
    end;
    LLeft := ALeft.Token(LAt);
    LRight := ARight.Token(LAt);
    { Both views scan the same immutable text with fixed identifier policy.
      Nesting can alter only token boundaries, families and completeness. }

    if (LLeft.Kind <> LRight.Kind) or (LLeft.Span.Start <> LRight.Span.Start) or
      (LLeft.Span.Length <> LRight.Span.Length) or (LLeft.Complete <> LRight.Complete) then
    begin
      Exit;
    end;
  end;
  Result := True;
end;

procedure TTranslation.Visit(AFile, AParent: Integer; const AInclude: TNyxPascalSpan);
var
  LOccurrence: Integer;
  LAt: Integer;
  LAncestor: Integer;
  LBytes: Integer;
  LLexical: INyxPascalLexicalSnapshot;
  LReader: TTranslationReader;
  LObserver: INyxPascalLexicalObserver;
begin
  CheckCancellation;

  if FDepth >= FOptions.MaximumIncludeDepth then
  begin
    raise ENyxPascalTranslationLimit.Create('Pascal include depth exceeds its budget');
  end;

  if FOccurrenceCount >= FOptions.MaximumOccurrences then
  begin
    raise ENyxPascalTranslationLimit.Create('Pascal include occurrences exceed their budget');
  end;
  LAncestor := AParent;
  while LAncestor >= 0 do
  begin

    if (FOccurrences[LAncestor].FileIndex = AFile) and
      FStates[FOccurrences[LAncestor].EntryState].SameFacts(FContext) then
    begin
      Problem(ptiIncludeCycle, AParent, AInclude,
        'This include repeats without changing its captured compiler state');
      UnknownEffect;
      Exit;
    end;
    LAncestor := FOccurrences[LAncestor].Parent;
  end;
  LBytes := NyxUTF8ByteCount(FFiles.TextAt(AFile));

  if LBytes > FOptions.MaximumExpandedBytes - FBytes then
  begin
    raise ENyxPascalTranslationLimit.Create('Expanded Pascal source exceeds its byte budget');
  end;
  Inc(FBytes, LBytes);
  LOccurrence := FOccurrenceCount;

  if LOccurrence = Length(FOccurrences) then
  begin
    SetLength(FOccurrences, Capacity(LOccurrence, FOptions.MaximumOccurrences));
    SetLength(FLexical, Length(FOccurrences));
  end;
  FOccurrences[LOccurrence].FFile := AFile;
  FOccurrences[LOccurrence].FParent := AParent;
  FOccurrences[LOccurrence].FIncludeSpan := AInclude;
  FOccurrences[LOccurrence].FFirst := FTokenCount;
  FOccurrences[LOccurrence].FEntry := FStateCount - 1;
  Inc(FOccurrenceCount);
  Inc(FDepth);
  try

    LReader := TTranslationReader.Create;
    LObserver := LReader;
    LReader.Owner := Self;
    LReader.Occurrence := LOccurrence;
    LReader.Included := AParent >= 0;
    LLexical := NewNyxPascalContextualLexicalSnapshot(FFiles.TextAt(AFile),
      FOptions.LexicalOptions.NestedComments(FNested), LObserver, FCancellation);
    FLexical[LOccurrence] := LLexical;
    for LAt := 0 to LLexical.DiagnosticCount - 1 do
    begin
      Problem(ptiLexicalIssue, LOccurrence, LLexical.Diagnostic(LAt).Span,
        LLexical.Diagnostic(LAt).MessageText);
    end;

    if FPhysical[AFile] = nil then
    begin
      FPhysical[AFile] := TPhysicalCache.Create;
      FPhysical[AFile].Lexical := LLexical;
    end
    else if not FPhysical[AFile].Ambiguous then
    begin
      FPhysical[AFile].Ambiguous := not SameLexicalView(
        FPhysical[AFile].Lexical, LLexical, FCancellation);
    end;
    FOccurrences[LOccurrence].FCount := FTokenCount - FOccurrences[LOccurrence].FFirst;
    FOccurrences[LOccurrence].FExit := FStateCount - 1;
  finally
    Dec(FDepth);
  end;
end;

procedure SplitDirective(const AText: TNyxText; out AName, AValue: TNyxText);
var
  LAt: Integer;
  LStart: Integer;
  LFinish: Integer;
begin
  LAt := 1;
  while (LAt <= Length(AText)) and (AText[LAt] <> '$') do
  begin
    Inc(LAt);
  end;
  Inc(LAt);
  LStart := LAt;
  while (LAt <= Length(AText)) and (AText[LAt] in ['A'..'Z', 'a'..'z']) do
  begin
    Inc(LAt);
  end;
  AName := NyxPascalASCIIKey(Copy(AText, LStart, LAt - LStart));
  LFinish := Length(AText) - 1;

  if (Length(AText) >= 2) and (Copy(AText, Length(AText) - 1, 2) = '*)') then
  begin
    Dec(LFinish);
  end;
  { Trim only Pascal ASCII whitespace in the original storage. ANSI RTL Trim
    can transcode a supplementary-Unicode include name on a native host. }
  while (LAt <= LFinish) and (AText[LAt] in [#9..#13, ' ']) do
  begin
    Inc(LAt);
  end;
  while (LFinish >= LAt) and (AText[LFinish] in [#9..#13, ' ']) do
  begin
    Dec(LFinish);
  end;
  AValue := Copy(AText, LAt, LFinish - LAt + 1);
end;

function TTranslation.Condition(const AKind, AValue: TNyxText): TNyxPascalKnowledge;
var
  LName: TNyxPascalDefineRef;
  LOption: TNyxPascalKnowledge;
begin
  Result := ppkUnknown;
  try

    if (AKind = 'ifdef') or (AKind = 'ifndef') then
    begin
      LName := NyxPascalDefine(AValue);
      Result := FContext.Symbol(LName).IsDefined;

      if AKind = 'ifndef' then
      begin
        Result := NyxPascalNot(Result);
      end;
    end
    else if AKind = 'ifopt' then
    begin

      if (Length(AValue) = 2) and (AValue[2] in ['+', '-']) then
      begin
        LOption := FContext.Option(AValue[1]);
        Result := LOption;

        if AValue[2] = '-' then
        begin
          Result := NyxPascalNot(Result);
        end;
      end;
    end
    else
    begin
      Result := AnalyzeNyxPascalCondition(FContext, AValue).Truth;
    end;
  except
    on ENyxPascalContext do
    begin
      { A malformed authored condition still owns its structural frame. Do not
        accidentally consume the enclosing conditional's endif during recovery. }
      Result := ppkUnknown;
    end;
  end;
end;

procedure TTranslation.ChangeSymbol(const AName: TNyxText; AKind: TNyxPascalDefineKind);
begin

  if FActive = ppkUnknown then
  begin
    AKind := ppdUnknown;
  end;
  FContext := FContext.WithSymbol(NyxPascalDefine(AName), AKind);
  SaveState;
end;

procedure TTranslation.ChangeOption(AName: Char; AValue: TNyxPascalKnowledge);
begin

  if FActive = ppkUnknown then
  begin
    AValue := ppkUnknown;
  end;
  FContext := FContext.WithOption(AName, AValue);
  SaveState;
end;

function ModeNamed(const AName: TNyxText): TNyxPascalLanguageMode;
const
  CNames: array[TNyxPascalLanguageMode] of TNyxText =
    ('', 'fpc', 'objfpc', 'delphi', 'delphiunicode', 'tp', 'macpas', 'iso', 'extendedpascal');
var
  LMode: TNyxPascalLanguageMode;
begin
  Result := ppmUnknown;
  for LMode := Succ(Low(TNyxPascalLanguageMode)) to High(TNyxPascalLanguageMode) do
  begin

    if CNames[LMode] = NyxPascalASCIIKey(AName) then
    begin
      Exit(LMode);
    end;
  end;
end;

function FeatureNamed(const AName: TNyxText; out AFeature: TNyxPascalLanguageFeature): Boolean;
const
  CNames: array[TNyxPascalLanguageFeature] of TNyxText =
    ('advancedrecords', 'nestedprocvars', 'anonymousfunctions', 'functionreferences',
      'typehelpers', 'nestedcomments', 'scopedenums', 'unicodestrings', 'out');
var
  LFeature: TNyxPascalLanguageFeature;
begin
  for LFeature := Low(TNyxPascalLanguageFeature) to High(TNyxPascalLanguageFeature) do
  begin

    if CNames[LFeature] = AName then
    begin
      AFeature := LFeature;
      Exit(True);
    end;
  end;
  Result := False;
end;

procedure TTranslation.Directive(AOccurrence: Integer; const AToken: TNyxPascalToken;
  const AText: TNyxText);
var
  LName: TNyxText;
  LValue: TNyxText;
  LArgument: TNyxText;
  LCondition: TNyxPascalKnowledge;
  LAt: Integer;
  LFrame: TConditionalFrame;
  LMode: TNyxPascalLanguageMode;
  LFeature: TNyxPascalLanguageFeature;
begin
  SplitDirective(AText, LName, LValue);
  try

    if (LName = 'ifdef') or (LName = 'ifndef') or (LName = 'if') or (LName = 'ifopt') then
    begin

      if Length(FFrames) >= FOptions.MaximumIncludeDepth then
      begin
        raise ENyxPascalTranslationLimit.Create('Pascal conditional depth exceeds its budget');
      end;
      LCondition := ppkFalse;

      if FActive <> ppkFalse then
      begin
        LCondition := Condition(LName, LValue);

        if LCondition = ppkUnknown then
        begin
          Problem(ptiUnknownCondition, AOccurrence, AToken.Span,
            'This condition needs additional captured compiler or semantic facts');
        end;
      end;
      LFrame := Default(TConditionalFrame);
      LFrame.Parent := FActive;
      LFrame.Taken := LCondition;
      LFrame.Occurrence := AOccurrence;
      LFrame.Span := AToken.Span;
      LAt := Length(FFrames);
      SetLength(FFrames, LAt + 1);
      FFrames[LAt] := LFrame;
      FActive := NyxPascalAnd(FActive, LCondition);
      Exit;
    end;

    if (LName = 'else') or (LName = 'elseif') or (LName = 'endif') or (LName = 'ifend') then
    begin
      LAt := High(FFrames);

      if LAt < 0 then
      begin
        Problem(ptiUnbalancedConditional, AOccurrence, AToken.Span,
          'This conditional boundary has no opening directive');
        UnknownEffect;
        FActive := ppkUnknown;
        Exit;
      end;
      LFrame := FFrames[LAt];

      if (LName = 'endif') or (LName = 'ifend') then
      begin
        FActive := LFrame.Parent;
        SetLength(FFrames, LAt);
      end
      else
      begin

        if LFrame.HasElse then
        begin
          Problem(ptiUnbalancedConditional, AOccurrence, AToken.Span,
            'A conditional branch cannot follow its final else');
          FActive := ppkUnknown;
          UnknownEffect;
          Exit;
        end;
        LCondition := ppkTrue;

        if LName = 'elseif' then
        begin

          if (LFrame.Parent = ppkFalse) or (LFrame.Taken = ppkTrue) then
          begin
            LCondition := ppkFalse;
          end
          else
          begin
            LCondition := Condition('if', LValue);

            if LCondition = ppkUnknown then
            begin
              Problem(ptiUnknownCondition, AOccurrence, AToken.Span,
                'This alternate condition needs additional captured facts');
            end;
          end;
        end
        else
        begin
          LFrame.HasElse := True;
        end;
        FActive := NyxPascalAnd(LFrame.Parent,
          NyxPascalAnd(NyxPascalNot(LFrame.Taken), LCondition));
        LFrame.Taken := NyxPascalOr(LFrame.Taken, LCondition);
        FFrames[LAt] := LFrame;
      end;
      Exit;
    end;

    if FActive = ppkFalse then
    begin
      Exit;
    end;

    if (LName = 'define') or (LName = 'undef') then
    begin

      if Pos(':=', LValue) > 0 then
      begin
        Problem(ptiUnsupportedDirective, AOccurrence, AToken.Span,
          'Replacement macros require explicit expansion support');
        UnknownEffect;
        FBlocked := True;
      end
      else if LName = 'define' then
      begin
        ChangeSymbol(LValue, ppdDefined);
      end
      else
      begin
        ChangeSymbol(LValue, ppdUndefined);
      end;
      Exit;
    end;

    if (LName = 'include') or ((LName = 'i') and not ((LValue = '+') or (LValue = '-'))) then
    begin
      LArgument := LValue;

      if (Length(LArgument) >= 2) and (LArgument[1] = #39) and
        (LArgument[Length(LArgument)] = #39) then
      begin
        LArgument := Copy(LArgument, 2, Length(LArgument) - 2);
      end;

      if (Pos('%', LArgument) > 0) or (Pos('*', LArgument) > 0) then
      begin
        Problem(ptiUnsupportedDirective, AOccurrence, AToken.Span,
          'Compiler-info substitutions and wildcard includes need explicit capture');
        UnknownEffect;
        Exit;
      end;
      LAt := FFiles.ResolveInclude(FFiles.FileAt(FOccurrences[AOccurrence].FileIndex), LArgument);

      if LAt < 0 then
      begin
        Problem(ptiMissingInclude, AOccurrence, AToken.Span,
          'This include was not present in the captured source search context');
        UnknownEffect;
      end
      else
      begin
        Visit(LAt, AOccurrence, AToken.Span);
      end;
      Exit;
    end;

    if (Length(LName) = 1) and ((LValue = '+') or (LValue = '-')) then
    begin
      LCondition := ppkFalse;

      if LValue = '+' then
      begin
        LCondition := ppkTrue;
      end;
      ChangeOption(LName[1], LCondition);
      Exit;
    end;

    if LName = 'mode' then
    begin
      LMode := ModeNamed(LValue);

      if FModeSeen then
      begin
        Problem(ptiMalformedDirective, AOccurrence, AToken.Span,
          'Only one source MODE directive is supported per compiler module');
        UnknownEffect;
        Exit;
      end;
      FModeSeen := True;

      if (FActive <> ppkTrue) or
        not ((LMode in [ppmDelphi, ppmTP]) or
          ((FContext.Family = ppcFPC) and (LMode in [ppmFPC, ppmObjFPC, ppmDelphiUnicode]))) then
      begin
        Problem(ptiLexicalPolicyChange, AOccurrence, AToken.Span,
          'This conditional or unsupported mode needs verified lexical defaults');
        FBlocked := True;
        UnknownEffect;
      end
      else
      begin
        FContext := FContext.InMode(LMode);
        FNested := LMode in [ppmFPC, ppmObjFPC];
        FContext := FContext.WithFeature(ppfNestedComments, ppkFalse);

        if FNested then
        begin
          FContext := FContext.WithFeature(ppfNestedComments, ppkTrue);
        end;

        if FContext.Family = ppcFPC then
        begin
          { Only independently qualified mode facts are restored after InMode
            revokes options/features. Version-sensitive callback/helper defaults
            remain unknown until explicitly captured or switched in source. }
          FContext := FContext.WithOption('H', ppkFalse)
            .WithFeature(ppfUnicodeStrings, ppkFalse)
            .WithFeature(ppfOutParameters, ppkFalse);

          if LMode in [ppmDelphi, ppmDelphiUnicode] then
          begin
            FContext := FContext.WithOption('H', ppkTrue);
          end;

          if LMode = ppmDelphiUnicode then
          begin
            FContext := FContext.WithFeature(ppfUnicodeStrings, ppkTrue);
          end;

          if LMode in [ppmDelphi, ppmDelphiUnicode, ppmObjFPC] then
          begin
            FContext := FContext.WithFeature(ppfOutParameters, ppkTrue);
          end;
          FContext := FContext.WithSymbol(NyxPascalDefine('FPC_DELPHI'), ppdUndefined)
            .WithSymbol(NyxPascalDefine('FPC_OBJFPC'), ppdUndefined)
            .WithSymbol(NyxPascalDefine('FPC_TP'), ppdUndefined)
            .WithSymbol(NyxPascalDefine('FPC_MACPAS'), ppdUndefined)
            .WithSymbol(NyxPascalDefine('FPC_ISO'), ppdUndefined)
            .WithSymbol(NyxPascalDefine('FPC_EXTENDEDPASCAL'), ppdUndefined);

          if LMode in [ppmDelphi, ppmDelphiUnicode] then
          begin
            FContext := FContext.WithSymbol(NyxPascalDefine('FPC_DELPHI'), ppdDefined);
          end
          else if LMode = ppmTP then
          begin
            FContext := FContext.WithSymbol(NyxPascalDefine('FPC_TP'), ppdDefined);
          end
          else if LMode = ppmObjFPC then
          begin
            FContext := FContext.WithSymbol(NyxPascalDefine('FPC_OBJFPC'), ppdDefined);
          end;
        end;
        SaveState;
      end;
      Exit;
    end;

    if (LName = 'modeswitch') or (LName = 'nestedcomments') or (LName = 'scopedenums') then
    begin
      LArgument := NyxPascalASCIIKey(LValue);
      LCondition := ppkTrue;

      if LName <> 'modeswitch' then
      begin

        if LArgument = 'off' then
        begin
          LCondition := ppkFalse;
        end
        else if LArgument <> 'on' then
        begin
          raise ENyxPascalContext.Create('A Boolean directive needs on or off');
        end;
        LArgument := LName;
      end
      else if (Length(LArgument) > 0) and
        (LArgument[Length(LArgument)] in ['+', '-']) then
      begin

        if LArgument[Length(LArgument)] = '-' then
        begin
          LCondition := ppkFalse;
        end;
        Delete(LArgument, Length(LArgument), 1);
      end;

      if not FeatureNamed(LArgument, LFeature) then
      begin
        Problem(ptiUnsupportedDirective, AOccurrence, AToken.Span,
          'This language feature needs a captured typed policy');
        UnknownEffect;
        Exit;
      end;

      if FActive = ppkUnknown then
      begin
        LCondition := ppkUnknown;
      end;

      if (LFeature = ppfNestedComments) and
        (LCondition = ppkUnknown) then
      begin
        Problem(ptiLexicalPolicyChange, AOccurrence, AToken.Span,
          'Conditional comment termination is unknown; later directives cannot be interpreted');
        UnknownEffect;
        FBlocked := True;
      end
      else
      begin
        FContext := FContext.WithFeature(LFeature, LCondition);

        if LFeature = ppfNestedComments then
        begin
          FNested := LCondition = ppkTrue;
        end;
        SaveState;
      end;
      Exit;
    end;

    if (LName = 'codepage') and (NyxPascalASCIIKey(LValue) = 'utf8') then
    begin
      Exit;
    end;
    Problem(ptiUnsupportedDirective, AOccurrence, AToken.Span,
      'This directive needs explicit compiler-context support');
    UnknownEffect;
  except
    on LError: ENyxPascalTranslationLimit do
    begin
      raise;
    end;
    on LError: ENyxPascalLexicalLimit do
    begin
      raise;
    end;
    on LError: ENyxPascalContext do
    begin
      Problem(ptiMalformedDirective, AOccurrence, AToken.Span,
        'This directive cannot be interpreted in its captured context');
      UnknownEffect;
      FActive := ppkUnknown;
    end;
  end;
end;

function TTranslation.Files: INyxPascalFileCatalog;
begin
  Result := FFiles;
end;

function TTranslation.InitialContext: INyxPascalCompilerContext;
begin
  Result := FInitial;
end;

function TTranslation.Resolved: Boolean;
begin
  Result := FResolved;
end;

function TTranslation.Count: Integer;
begin
  Result := FTokenCount;
end;

function TTranslation.Token(AIndex: Integer): TNyxPascalTranslatedToken;
begin

  if (AIndex < 0) or (AIndex >= FTokenCount) then
  begin
    raise ENyxPascalContext.Create('Translated token index is outside its snapshot');
  end;
  Result := FTokens[AIndex];
end;

function TTranslation.PhysicalToken(AIndex: Integer): TNyxPascalToken;
var
  LToken: TNyxPascalTranslatedToken;
begin
  LToken := Token(AIndex);
  Result := OccurrenceLexical(LToken.Occurrence).Token(LToken.PhysicalToken);
end;

function TTranslation.Text(AIndex: Integer): TNyxText;
var
  LToken: TNyxPascalTranslatedToken;
begin
  LToken := Token(AIndex);
  Result := OccurrenceLexical(LToken.Occurrence).Text(LToken.PhysicalToken);
end;

function TTranslation.Physical(AFile: Integer): INyxPascalLexicalSnapshot;
begin
  FFiles.FileAt(AFile);

  if FPhysical[AFile] = nil then
  begin
    raise ENyxPascalContext.Create('This captured file has no translation occurrence');
  end;

  if FPhysical[AFile].Ambiguous then
  begin
    raise ENyxPascalContext.Create('This file has differing lexical contexts; select its exact occurrence');
  end;
  Result := FPhysical[AFile].Lexical;
end;

function TTranslation.OccurrenceLexical(AOccurrence: Integer): INyxPascalLexicalSnapshot;
begin
  Occurrence(AOccurrence);
  Result := FLexical[AOccurrence];
end;

function NyxPascalOccurrenceLexical(const ATranslation: INyxPascalTranslation;
  AOccurrence: Integer): INyxPascalLexicalSnapshot;
var
  LContexts: INyxPascalTranslationLexicalContexts;
begin

  if ATranslation = nil then
  begin
    raise ENyxPascalContext.Create('A lexical occurrence needs its owning translation');
  end;

  if Supports(ATranslation, INyxPascalTranslationLexicalContexts, LContexts) then
  begin
    Result := LContexts.OccurrenceLexical(AOccurrence);
  end
  else
  begin
    Result := ATranslation.Physical(ATranslation.Occurrence(AOccurrence).FileIndex);
  end;
end;

function TTranslation.OccurrenceCount: Integer;
begin
  Result := FOccurrenceCount;
end;

function TTranslation.Occurrence(AIndex: Integer): TNyxPascalInclusion;
begin

  if (AIndex < 0) or (AIndex >= FOccurrenceCount) then
  begin
    raise ENyxPascalContext.Create('Include occurrence index is outside its translation');
  end;
  Result := FOccurrences[AIndex];
end;

function TTranslation.StateCount: Integer;
begin
  Result := FStateCount;
end;

function TTranslation.State(AIndex: Integer): INyxPascalCompilerContext;
begin

  if (AIndex < 0) or (AIndex >= FStateCount) then
  begin
    raise ENyxPascalContext.Create('Compiler-state index is outside its translation');
  end;
  Result := FStates[AIndex];
end;

function TTranslation.DiagnosticCount: Integer;
begin
  Result := FDiagnosticCount;
end;

function TTranslation.Diagnostic(AIndex: Integer): TNyxPascalTranslationDiagnostic;
begin

  if (AIndex < 0) or (AIndex >= FDiagnosticCount) then
  begin
    raise ENyxPascalContext.Create('Translation diagnostic index is outside its snapshot');
  end;
  Result := FDiagnostics[AIndex];
end;

function NewNyxPascalTranslation(const AFiles: INyxPascalFileCatalog;
  const ARoot: TNyxPascalPhysicalFile; const AContext: INyxPascalCompilerContext;
  const AOptions: TNyxPascalTranslationOptions;
  const ACancellation: INyxCancellationScope): INyxPascalTranslation;
begin
  Result := TTranslation.Create(AFiles, ARoot, AContext, AOptions, ACancellation);
end;

end.
