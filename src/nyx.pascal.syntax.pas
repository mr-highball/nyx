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
unit nyx.pascal.syntax;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.scheduler, nyx.pascal.lexing;

type
  { Scope/entry/diagnostic/depth quotas refuse the complete candidate rather than
    presenting a truncated structure as a usable language index. }
  ENyxPascalSyntaxLimit = class(ENyxPascalLexicalLimit);
  { An explicit physical-file starting context. Includes must receive their
    actual enclosing context; no filename or text heuristic grants that context. }
  TNyxPascalSyntaxFragment = (psfModule, psfDeclarations, psfTypes, psfMembers,
    psfInterfaceDeclarations);
  { Lexical containment, independent of resolved symbols or runtime ownership.
    Routine scopes include parameters/locals. Statement blocks retain boundaries;
    inline declarations and anonymous scopes explicitly require richer analysis. }
  TNyxPascalSyntaxScopeKind = (psskModule, psskMembers, psskRoutine, psskBlock);
  { Declared intent only. A typed variable/property preserves its type expression;
    a routine preserves its complete signature, result, parameters and owner name. }
  TNyxPascalSyntaxDeclarationKind = (psdkType, psdkConstant, psdkVariable,
    psdkField, psdkParameter, psdkProperty, psdkRoutine, psdkEnumValue, psdkResourceString);
  { Surface type construction. Named expressions may still be generic/qualified;
    neither this form nor an ancestor expression claims semantic resolution. }
  TNyxPascalSyntaxTypeForm = (pstfNone, pstfNamed, pstfClass, pstfInterface,
    pstfRecord, pstfObject, pstfEnumeration, pstfArray, pstfSet, pstfPointer,
    pstfRoutine, pstfClassReference);
  { Authored access at this declaration. Strict and unit visibility remain
    distinct; effective inherited accessibility belongs to later binding. }
  TNyxPascalSyntaxVisibility = (psavUnspecified, psavInterface, psavImplementation,
    psavLocal, psavPrivate, psavStrictPrivate, psavProtected, psavStrictProtected,
    psavPublic, psavPublished, psavAutomated);
  { Pascal parameter passing syntax, copied into each grouped parameter entry. }
  TNyxPascalSyntaxParameterMode = (pspmValue, pspmConst, pspmVar, pspmOut, pspmConstRef);
  { Explicit uncertainty and declaration modifiers. Unsupported/recovered scopes
    grant no successful type inference, compilation or source editing authority. }
  TNyxPascalSyntaxFlag = (psdfIncomplete, psdfConditional, psdfForward,
    psdfClassMember, psdfOverload, psdfUnsupported);
  { Value-owned flag set with no mutable storage or target-specific state. }
  TNyxPascalSyntaxFlags = set of TNyxPascalSyntaxFlag;
  { Scoped structural failure/context requirement, separate from lexical issues.
    Consumers must inspect both diagnostic collections, never infer compilation. }
  TNyxPascalSyntaxIssue = (psiExpectedName, psiExpectedSeparator, psiUnclosedScope,
    psiUnsupportedSyntax, psiIncludeContextRequired, psiConditionalContextRequired,
    psiUnterminatedConditional, psiCompilerContextRequired);
  { Independent scalar diagnostic. English guidance is owned; the exact source
    slice remains on its immutable snapshot, never an editor/session borrow. }
  TNyxPascalSyntaxDiagnostic = record
  private
    FIssue: TNyxPascalSyntaxIssue;
    FSpan: TNyxPascalSpan;
    FMessage: TNyxText;
  public
    property Issue: TNyxPascalSyntaxIssue read FIssue;
    property Span: TNyxPascalSpan read FSpan;
    property MessageText: TNyxText read FMessage;
  end;
  { One immutable scope. Parent/owner/declaration links are snapshot-local
    ordinals, never persistent references or permissions. -1 means absent.
    Complete describes closed boundaries, never grammar or compiler acceptance;
    UnsupportedSyntax forbids inferring through a skipped declaration construct. }
  TNyxPascalSyntaxScope = record
  private
    FKind: TNyxPascalSyntaxScopeKind;
    FParent: Integer;
    FOwner: Integer;
    FFirst: Integer;
    FLast: Integer;
    FCount: Integer;
    FSpan: TNyxPascalSpan;
    FName: TNyxText;
    FComplete: Boolean;
    FUnsupported: Boolean;
  public
    property Kind: TNyxPascalSyntaxScopeKind read FKind;
    property Parent: Integer read FParent;
    property OwnerDeclaration: Integer read FOwner;
    property FirstDeclaration: Integer read FFirst;
    property DeclarationCount: Integer read FCount;
    property Span: TNyxPascalSpan read FSpan;
    property Name: TNyxText read FName;
    property Complete: Boolean read FComplete;
    property UnsupportedSyntax: Boolean read FUnsupported;
  end;
  { One physical declaration with exact source ranges. Grouped names have
    independent entries and a shared type/signature range. Linked ordinals allow
    bounded scope/name queries. Records retain no interfaces or mutable arrays. }
  TNyxPascalSyntaxDeclaration = record
  private
    FName: TNyxText;
    FOwnerName: TNyxText;
    FKind: TNyxPascalSyntaxDeclarationKind;
    FForm: TNyxPascalSyntaxTypeForm;
    FVisibility: TNyxPascalSyntaxVisibility;
    FParameterMode: TNyxPascalSyntaxParameterMode;
    FFlags: TNyxPascalSyntaxFlags;
    FNameSpan: TNyxPascalSpan;
    FHeaderSpan: TNyxPascalSpan;
    FTypeSpan: TNyxPascalSpan;
    FAncestorSpan: TNyxPascalSpan;
    FGenericSpan: TNyxPascalSpan;
    FExpressionSpan: TNyxPascalSpan;
    FScope: Integer;
    FNestedScope: Integer;
    FNext: Integer;
    FNextNamed: Integer;
    FLastNamed: Integer;
  public
    property Name: TNyxText read FName;
    property OwnerName: TNyxText read FOwnerName;
    property Kind: TNyxPascalSyntaxDeclarationKind read FKind;
    property TypeForm: TNyxPascalSyntaxTypeForm read FForm;
    property Visibility: TNyxPascalSyntaxVisibility read FVisibility;
    property ParameterMode: TNyxPascalSyntaxParameterMode read FParameterMode;
    property Flags: TNyxPascalSyntaxFlags read FFlags;
    property NameSpan: TNyxPascalSpan read FNameSpan;
    property HeaderSpan: TNyxPascalSpan read FHeaderSpan;
    property TypeSpan: TNyxPascalSpan read FTypeSpan;
    property AncestorSpan: TNyxPascalSpan read FAncestorSpan;
    property GenericSpan: TNyxPascalSpan read FGenericSpan;
    property ExpressionSpan: TNyxPascalSpan read FExpressionSpan;
    property Scope: Integer read FScope;
    property NestedScope: Integer read FNestedScope;
    property Next: Integer read FNext;
    property NextNamed: Integer read FNextNamed;
  end;
  { A declared uses/requires/contains namespace. Optional file expressions remain
    authored text; this value confers no filesystem/import/loading capability. }
  TNyxPascalSyntaxImport = record
  private
    FName: TNyxText;
    FSpan: TNyxPascalSpan;
    FScope: Integer;
    FConditional: Boolean;
  public
    property Name: TNyxText read FName;
    property Span: TNyxPascalSpan read FSpan;
    property Scope: Integer read FScope;
    property Conditional: Boolean read FConditional;
  end;
  { Independent fluent limits and explicit include/fragment starting context.
    A lexer policy is copied, never inferred from the host compiler. }
  TNyxPascalSyntaxOptions = record
  private
    FDefined: Boolean;
    FFragment: TNyxPascalSyntaxFragment;
    FLexical: TNyxPascalLexicalOptions;
    FDeclarations: Integer;
    FScopes: Integer;
    FDepth: Integer;
    FDiagnostics: Integer;
  public
    { Explicit module/lexical defaults and positive independent quotas; zeroed
      options refuse. Setters return validated copies, never shared mutable state. }
    class function Defaults: TNyxPascalSyntaxOptions; static;
    { Select the actual enclosing fragment grammar; filenames grant no context. }
    function Fragment(AValue: TNyxPascalSyntaxFragment): TNyxPascalSyntaxOptions;
    { Copy the complete lexical policy, including Unicode/comment handling. }
    function Lexical(const AValue: TNyxPascalLexicalOptions): TNyxPascalSyntaxOptions;
    { Bounds entries across grouped names, parameters and all nested scopes. }
    function UpToDeclarations(AValue: Integer): TNyxPascalSyntaxOptions;
    { Includes the physical module/fragment scope and routine/member/body scopes. }
    function UpToScopes(AValue: Integer): TNyxPascalSyntaxOptions;
    { Bounds recursive grouping and declaration/body/conditional nesting. }
    function UpToDepth(AValue: Integer): TNyxPascalSyntaxOptions;
    { Excess recovery/context diagnostics refuse the whole candidate. }
    function UpToDiagnostics(AValue: Integer): TNyxPascalSyntaxOptions;
    { Raises ENyxPascalLexical for undefined, invalid-enum or nonpositive policies. }
    procedure Validate;
    property StartingContext: TNyxPascalSyntaxFragment read FFragment;
    property LexicalOptions: TNyxPascalLexicalOptions read FLexical;
    property MaximumDeclarations: Integer read FDeclarations;
    property MaximumScopes: Integer read FScopes;
    property MaximumDepth: Integer read FDepth;
    property MaximumDiagnostics: Integer read FDiagnostics;
  end;
  { Immutable recoverable declared structure over one physical lexical snapshot.
    Types/ancestors/generic arguments remain exact syntax, not bound compiler
    types. Includes/conditions/unsupported constructs remain explicit diagnostics.
    There is no source execution, compiler provenance, editor or document borrow. }
  INyxPascalSyntaxSnapshot = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570B02}']
    function Lexical: INyxPascalLexicalSnapshot;
    { Ordinals are zero-based and checked. Returned records are detached values;
      nested/linked ordinals are meaningful only in this same immutable snapshot. }
    function ScopeCount: Integer;
    function Scope(AIndex: Integer): TNyxPascalSyntaxScope;
    { Deepest physical containment; an unfinished scope also contains EOF.
      This does not resolve conditional branches or imply visibility/binding. }
    function ScopeAt(AOffset: Integer): Integer;
    function DeclarationCount: Integer;
    function Declaration(AIndex: Integer): TNyxPascalSyntaxDeclaration;
    { First same-spelled entry in this exact scope, or -1. NextNamed retains
      overload/forward duplicates; this is declared lookup, not name binding. }
    function Named(AScope: Integer; const AName: TNyxText): Integer;
    function ImportCount: Integer;
    function ImportAt(AIndex: Integer): TNyxPascalSyntaxImport;
    function DiagnosticCount: Integer;
    function Diagnostic(AIndex: Integer): TNyxPascalSyntaxDiagnostic;
  end;

{ Pure analysis on copied text. Unknown/missing syntax retains preceding
  declarations and physical spans with explicit recovery issues. Quotas and
  cancellation refuse the complete candidate; no parser result admits source. }
function NewNyxPascalSyntaxSnapshot(const ASource: TNyxText): INyxPascalSyntaxSnapshot; overload;
function NewNyxPascalSyntaxSnapshot(const ASource: TNyxText;
  const AOptions: TNyxPascalSyntaxOptions;
  const ACancellation: INyxCancellationScope = nil): INyxPascalSyntaxSnapshot; overload;

implementation

uses nyx.text.index;

type
  { Source-independent ordinal vector. Temporary lists stage grouped declarations
    without putting interfaces or shared mutable arrays in published records. }
  TOrdinals = array of Integer;
  { Owned parser builder becomes the immutable snapshot after construction.
    Significant-token ordinals keep lossless trivia on Lexical. No raw source
    provider or cancellation borrow survives publication. }
  TSyntaxSnapshot = class(TInterfacedObject, INyxPascalSyntaxSnapshot)
  private
    FLexical: INyxPascalLexicalSnapshot;
    FOptions: TNyxPascalSyntaxOptions;
    FCancellation: INyxCancellationScope;
    FTokens: TOrdinals;
    FConditional: array of Boolean;
    FScopes: array of TNyxPascalSyntaxScope;
    FDeclarations: array of TNyxPascalSyntaxDeclaration;
    FImports: array of TNyxPascalSyntaxImport;
    FDiagnostics: array of TNyxPascalSyntaxDiagnostic;
    FScopeCount: Integer;
    FDeclarationCount: Integer;
    FImportCount: Integer;
    FDiagnosticCount: Integer;
    FNames: TNyxTextIndex;
    FPosition: Integer;
    FDepth: Integer;
    function Tok(AOffset: Integer = 0): TNyxPascalToken;
    function Word(AWord: TNyxPascalWord; AOffset: Integer = 0): Boolean;
    function Sym(ASymbol: TNyxPascalSymbol; AOffset: Integer = 0): Boolean;
    function EOF: Boolean;
    function Name(AOffset: Integer = 0): TNyxText;
    procedure Step;
    function EndOffset: Integer;
    function SpanFrom(AStart: Integer): TNyxPascalSpan;
    function Range(AStart, AEnd: Integer): TNyxPascalSpan;
    procedure Problem(AIssue: TNyxPascalSyntaxIssue; const AText: TNyxText;
      const ASpan: TNyxPascalSpan);
    procedure Unsupported(AScope: Integer; const AText: TNyxText;
      const ASpan: TNyxPascalSpan);
    procedure Enter;
    procedure Leave;
    function AddScope(AKind: TNyxPascalSyntaxScopeKind; AParent, AOwner: Integer;
      AStart: Integer; const AName: TNyxText): Integer;
    procedure FinishScope(AScope: Integer; AComplete: Boolean);
    function AddDeclaration(AScope: Integer; AKind: TNyxPascalSyntaxDeclarationKind;
      AVisibility: TNyxPascalSyntaxVisibility; AName: Integer): Integer;
    procedure Separator;
    procedure Groups(AOpen, AClose: TNyxPascalSymbol);
    procedure Angles;
    procedure Expression(AStop: TNyxPascalSymbol; AAlsoComma: Boolean = False);
    function Form: TNyxPascalSyntaxTypeForm;
    procedure ParseType(AScope: Integer; AVisibility: TNyxPascalSyntaxVisibility);
    procedure TypeSection(AScope: Integer; AVisibility: TNyxPascalSyntaxVisibility);
    procedure Variables(AScope: Integer; AKind: TNyxPascalSyntaxDeclarationKind;
      AVisibility: TNyxPascalSyntaxVisibility; AClass: Boolean = False);
    procedure Constants(AScope: Integer; AKind: TNyxPascalSyntaxDeclarationKind;
      AVisibility: TNyxPascalSyntaxVisibility);
    procedure Parameters(AScope: Integer);
    procedure Routine(AScope: Integer; AVisibility: TNyxPascalSyntaxVisibility;
      AClass: Boolean = False);
    procedure PropertyDeclaration(AScope: Integer; AVisibility: TNyxPascalSyntaxVisibility);
    procedure Imports(AScope: Integer);
    procedure Declarations(AScope: Integer; AVisibility: TNyxPascalSyntaxVisibility;
      AMembers: Boolean);
    function Block(AScope: Integer): Boolean;
    procedure BuildTokens;
    procedure Build;
  public
    constructor Create(const ASource: TNyxText; const AOptions: TNyxPascalSyntaxOptions;
      const ACancellation: INyxCancellationScope);
    destructor Destroy; override;
    function Lexical: INyxPascalLexicalSnapshot;
    function ScopeCount: Integer;
    function Scope(AIndex: Integer): TNyxPascalSyntaxScope;
    function ScopeAt(AOffset: Integer): Integer;
    function DeclarationCount: Integer;
    function Declaration(AIndex: Integer): TNyxPascalSyntaxDeclaration;
    function Named(AScope: Integer; const AName: TNyxText): Integer;
    function ImportCount: Integer;
    function ImportAt(AIndex: Integer): TNyxPascalSyntaxImport;
    function DiagnosticCount: Integer;
    function Diagnostic(AIndex: Integer): TNyxPascalSyntaxDiagnostic;
  end;

class function TNyxPascalSyntaxOptions.Defaults: TNyxPascalSyntaxOptions;
begin
  Result := Default(TNyxPascalSyntaxOptions);
  Result.FDefined := True;
  Result.FLexical := TNyxPascalLexicalOptions.Defaults;
  Result.FDeclarations := 16384;
  Result.FScopes := 8192;
  Result.FDepth := 128;
  Result.FDiagnostics := 1024;
end;

procedure TNyxPascalSyntaxOptions.Validate;
begin
  FLexical.Validate;

  if not FDefined or (FDeclarations < 1) or (FScopes < 1) or
    (FDepth < 1) or (FDiagnostics < 1) or
    (Ord(FFragment) < Ord(Low(TNyxPascalSyntaxFragment))) or
    (Ord(FFragment) > Ord(High(TNyxPascalSyntaxFragment))) then
  begin
    raise ENyxPascalLexical.Create('Use explicit positive Pascal syntax options');
  end;
end;

function TNyxPascalSyntaxOptions.Fragment(AValue: TNyxPascalSyntaxFragment): TNyxPascalSyntaxOptions;
begin
  Result := Self;
  Result.FFragment := AValue;
  Result.Validate;
end;

function TNyxPascalSyntaxOptions.Lexical(const AValue: TNyxPascalLexicalOptions): TNyxPascalSyntaxOptions;
begin
  Result := Self;
  Result.FLexical := AValue;
  Result.Validate;
end;

function TNyxPascalSyntaxOptions.UpToDeclarations(AValue: Integer): TNyxPascalSyntaxOptions;
begin
  Result := Self;
  Result.FDeclarations := AValue;
  Result.Validate;
end;

function TNyxPascalSyntaxOptions.UpToScopes(AValue: Integer): TNyxPascalSyntaxOptions;
begin
  Result := Self;
  Result.FScopes := AValue;
  Result.Validate;
end;

function TNyxPascalSyntaxOptions.UpToDepth(AValue: Integer): TNyxPascalSyntaxOptions;
begin
  Result := Self;
  Result.FDepth := AValue;
  Result.Validate;
end;

function TNyxPascalSyntaxOptions.UpToDiagnostics(AValue: Integer): TNyxPascalSyntaxOptions;
begin
  Result := Self;
  Result.FDiagnostics := AValue;
  Result.Validate;
end;

constructor TSyntaxSnapshot.Create(const ASource: TNyxText;
  const AOptions: TNyxPascalSyntaxOptions; const ACancellation: INyxCancellationScope);
begin
  inherited Create;
  AOptions.Validate;
  FOptions := AOptions;
  FCancellation := ACancellation;
  FNames := TNyxTextIndex.Create;
  try
    FLexical := NewNyxPascalLexicalSnapshot(ASource, AOptions.LexicalOptions, ACancellation);
    Build;
  finally
    FCancellation := nil;
  end;
end;

destructor TSyntaxSnapshot.Destroy;
begin
  FNames.Free;
  inherited Destroy;
end;

function TSyntaxSnapshot.Tok(AOffset: Integer): TNyxPascalToken;
var
  LAt: Integer;
begin
  LAt := FPosition + AOffset;

  if LAt < 0 then
  begin
    LAt := 0;
  end;

  if LAt >= Length(FTokens) then
  begin
    LAt := High(FTokens);
  end;
  Result := FLexical.Token(FTokens[LAt]);
end;

function TSyntaxSnapshot.Word(AWord: TNyxPascalWord; AOffset: Integer): Boolean;
begin
  Result := (Tok(AOffset).Kind = ptkWord) and (Tok(AOffset).Word = AWord);
end;

function TSyntaxSnapshot.Sym(ASymbol: TNyxPascalSymbol; AOffset: Integer): Boolean;
begin
  Result := (Tok(AOffset).Kind = ptkSymbol) and (Tok(AOffset).Symbol = ASymbol);
end;

function TSyntaxSnapshot.EOF: Boolean;
begin
  Result := Tok.Kind = ptkEOF;
end;

function TSyntaxSnapshot.Name(AOffset: Integer): TNyxText;
begin
  Result := '';

  if Tok(AOffset).Kind = ptkWord then
  begin
    Result := FLexical.Identifier(FTokens[FPosition + AOffset]);
  end;
end;

procedure TSyntaxSnapshot.Step;
begin

  if (FCancellation <> nil) and FCancellation.Cancelled then
  begin
    raise ENyxPascalAnalysisCancelled.Create('Pascal syntax analysis cancelled');
  end;

  if FPosition < High(FTokens) then
  begin
    Inc(FPosition);
  end;
end;

function TSyntaxSnapshot.EndOffset: Integer;
begin
  Result := 0;

  if FPosition > 0 then
  begin
    Result := FLexical.Token(FTokens[FPosition - 1]).Span.Finish;
  end;
end;

function TSyntaxSnapshot.SpanFrom(AStart: Integer): TNyxPascalSpan;
begin
  Result := NyxPascalSpan(AStart, EndOffset - AStart);
end;

function TSyntaxSnapshot.Range(AStart, AEnd: Integer): TNyxPascalSpan;
begin
  Result := NyxPascalSpan(FLexical.Token(FTokens[AStart]).Span.Start, 0);

  if AEnd > AStart then
  begin
    Result := NyxPascalSpan(Result.Start,
      FLexical.Token(FTokens[AEnd - 1]).Span.Finish - Result.Start);
  end;
end;

{ Geometric growth remains within the admitted quota without integer overflow.
  Only the builder mutates vectors; unused capacity never enters public counts. }
function Capacity(ACount, AMaximum: Integer): Integer;
begin
  Result := AMaximum;

  if 16 + ACount div 2 <= AMaximum - ACount then
  begin
    Result := ACount + 16 + ACount div 2;
  end;
end;

procedure TSyntaxSnapshot.Problem(AIssue: TNyxPascalSyntaxIssue; const AText: TNyxText;
  const ASpan: TNyxPascalSpan);
var
  LAt: Integer;
begin
  LAt := FDiagnosticCount;

  if LAt >= FOptions.MaximumDiagnostics then
  begin
    raise ENyxPascalSyntaxLimit.Create('Pascal syntax diagnostics exceed their budget');
  end;

  if LAt = Length(FDiagnostics) then
  begin
    SetLength(FDiagnostics, Capacity(LAt, FOptions.MaximumDiagnostics));
  end;
  FDiagnostics[LAt].FIssue := AIssue;
  FDiagnostics[LAt].FSpan := ASpan;
  FDiagnostics[LAt].FMessage := AText;
  Inc(FDiagnosticCount);
end;

procedure TSyntaxSnapshot.Unsupported(AScope: Integer; const AText: TNyxText;
  const ASpan: TNyxPascalSpan);
var
  LScope: Integer;
begin
  Problem(psiUnsupportedSyntax, AText, ASpan);
  LScope := AScope;
  while LScope >= 0 do
  begin
    FScopes[LScope].FUnsupported := True;

    if FScopes[LScope].OwnerDeclaration >= 0 then
    begin
      Include(FDeclarations[FScopes[LScope].OwnerDeclaration].FFlags, psdfUnsupported);
      Break;
    end;
    LScope := FScopes[LScope].Parent;
  end;
end;

procedure TSyntaxSnapshot.Enter;
begin

  if FDepth >= FOptions.MaximumDepth then
  begin
    raise ENyxPascalSyntaxLimit.Create('Pascal syntax nesting exceeds its budget');
  end;
  Inc(FDepth);
end;

procedure TSyntaxSnapshot.Leave;
begin
  Dec(FDepth);
end;

function TSyntaxSnapshot.AddScope(AKind: TNyxPascalSyntaxScopeKind;
  AParent, AOwner: Integer; AStart: Integer; const AName: TNyxText): Integer;
begin
  Result := FScopeCount;

  if Result >= FOptions.MaximumScopes then
  begin
    raise ENyxPascalSyntaxLimit.Create('Pascal syntax scopes exceed their budget');
  end;

  if Result = Length(FScopes) then
  begin
    SetLength(FScopes, Capacity(Result, FOptions.MaximumScopes));
  end;
  FScopes[Result].FKind := AKind;
  FScopes[Result].FParent := AParent;
  FScopes[Result].FOwner := AOwner;
  FScopes[Result].FFirst := -1;
  FScopes[Result].FLast := -1;
  FScopes[Result].FSpan := NyxPascalSpan(AStart, FLexical.Scalars - AStart);
  FScopes[Result].FName := AName;
  Inc(FScopeCount);
end;

procedure TSyntaxSnapshot.FinishScope(AScope: Integer; AComplete: Boolean);
begin
  FScopes[AScope].FComplete := AComplete;

  if AComplete then
  begin
    FScopes[AScope].FSpan := SpanFrom(FScopes[AScope].Span.Start);
  end;
end;

function TSyntaxSnapshot.AddDeclaration(AScope: Integer;
  AKind: TNyxPascalSyntaxDeclarationKind; AVisibility: TNyxPascalSyntaxVisibility;
  AName: Integer): Integer;
var
  LToken: TNyxPascalToken;
  LKey: TNyxText;
  LFirst: Integer;
begin
  Result := FDeclarationCount;

  if Result >= FOptions.MaximumDeclarations then
  begin
    raise ENyxPascalSyntaxLimit.Create('Pascal declarations exceed their syntax budget');
  end;
  LToken := FLexical.Token(FTokens[AName]);

  if Result = Length(FDeclarations) then
  begin
    SetLength(FDeclarations, Capacity(Result, FOptions.MaximumDeclarations));
  end;
  FDeclarations[Result].FName := FLexical.Identifier(FTokens[AName]);
  FDeclarations[Result].FKind := AKind;
  FDeclarations[Result].FVisibility := AVisibility;
  FDeclarations[Result].FNameSpan := LToken.Span;
  FDeclarations[Result].FScope := AScope;
  FDeclarations[Result].FNestedScope := -1;
  FDeclarations[Result].FNext := -1;
  FDeclarations[Result].FNextNamed := -1;
  FDeclarations[Result].FLastNamed := Result;
  Inc(FDeclarationCount);

  if FConditional[AName] then
  begin
    Include(FDeclarations[Result].FFlags, psdfConditional);
  end;

  if FScopes[AScope].FLast >= 0 then
  begin
    FDeclarations[FScopes[AScope].FLast].FNext := Result;
  end
  else
  begin
    FScopes[AScope].FFirst := Result;
  end;
  FScopes[AScope].FLast := Result;
  Inc(FScopes[AScope].FCount);
  LKey := TNyxText(IntToStr(AScope)) + ':' + NyxPascalASCIIKey(FDeclarations[Result].Name);
  LFirst := FNames.IndexOf(LKey);

  if LFirst < 0 then
  begin
    FNames.AddFirst(LKey, Result);
  end
  else
  begin
    FDeclarations[FDeclarations[LFirst].FLastNamed].FNextNamed := Result;
    FDeclarations[LFirst].FLastNamed := Result;
  end;
end;

procedure TSyntaxSnapshot.Separator;
begin

  if Sym(psoSemicolon) then
  begin
    Step;
  end
  else
  begin
    Problem(psiExpectedSeparator, 'Finish this declaration with a semicolon', Tok.Span);
  end;
end;

procedure TSyntaxSnapshot.Groups(AOpen, AClose: TNyxPascalSymbol);
begin
  Enter;
  try
    Step;
    while not EOF and not Sym(AClose) do
    begin

      if Sym(AOpen) then
      begin
        Groups(AOpen, AClose);
      end
      else if Sym(psoOpenParen) then
      begin
        Groups(psoOpenParen, psoCloseParen);
      end
      else if Sym(psoOpenBracket) then
      begin
        Groups(psoOpenBracket, psoCloseBracket);
      end
      else
      begin
        Step;
      end;
    end;

    if Sym(AClose) then
    begin
      Step;
    end
    else
    begin
      Problem(psiUnclosedScope, 'Finish this grouped Pascal expression', Tok.Span);
    end;
  finally
    Leave;
  end;
end;

procedure TSyntaxSnapshot.Angles;
var
  LDepth: Integer;
begin
  LDepth := 1;
  Step;
  while not EOF and (LDepth > 0) do
  begin

    if Sym(psoLess) then
    begin
      Inc(LDepth);

      if LDepth > FOptions.MaximumDepth then
      begin
        raise ENyxPascalSyntaxLimit.Create('Generic syntax nesting exceeds its budget');
      end;
    end
    else if Sym(psoGreater) then
    begin
      Dec(LDepth);
    end
    else if Sym(psoShiftRight) then
    begin
      Dec(LDepth, 2);
    end;
    Step;
  end;

  if LDepth <> 0 then
  begin
    Problem(psiUnclosedScope, 'Finish this generic parameter or argument list', Tok.Span);
  end;
end;

procedure TSyntaxSnapshot.Expression(AStop: TNyxPascalSymbol; AAlsoComma: Boolean);
begin
  while not EOF and not Sym(AStop) and not (AAlsoComma and Sym(psoComma)) and
    not Word(pkwEnd) and not Word(pkwImplementation) and not Word(pkwBegin) do
  begin

    if not Tok.Complete then
    begin
      { Lexical recovery already identifies the uncertain token. Stop its value
        here so a following declaration is not swallowed as unfinished content. }
      Step;
      Break;
    end;

    if Sym(psoOpenParen) then
    begin
      Groups(psoOpenParen, psoCloseParen);
    end
    else if Sym(psoOpenBracket) or Sym(psoAlternateOpenBracket) then
    begin

      if Sym(psoAlternateOpenBracket) then
      begin
        Groups(psoAlternateOpenBracket, psoAlternateCloseBracket);
      end
      else
      begin
        Groups(psoOpenBracket, psoCloseBracket);
      end;
    end
    else
    begin
      Step;
    end;
  end;
end;

function TSyntaxSnapshot.Form: TNyxPascalSyntaxTypeForm;
begin
  Result := pstfNamed;
  case Tok.Word of
    pkwClass:
      begin
        Result := pstfClass;

        if Word(pkwOf, 1) then
        begin
          Result := pstfClassReference;
        end;
      end;
    pkwInterface, pkwDispinterface:
      begin
        Result := pstfInterface;
      end;
    pkwRecord:
      begin
        Result := pstfRecord;
      end;
    pkwObject:
      begin
        Result := pstfObject;
      end;
    pkwArray:
      begin
        Result := pstfArray;
      end;
    pkwSet:
      begin
        Result := pstfSet;
      end;
    pkwProcedure, pkwFunction, pkwReference:
      begin
        Result := pstfRoutine;
      end;
  else
    begin
      { Other words remain an exact named type expression. }
    end;
  end;

  if Sym(psoCaret) then
  begin
    Result := pstfPointer;
  end
  else if Sym(psoOpenParen) then
  begin
    Result := pstfEnumeration;
  end;
end;

procedure TSyntaxSnapshot.ParseType(AScope: Integer; AVisibility: TNyxPascalSyntaxVisibility);
var
  LEntry: Integer;
  LStart: Integer;
  LTypeStart: Integer;
  LGenericStart: Integer;
  LAncestor: Integer;
  LMemberScope: Integer;
  LEnum: Integer;
  LForm: TNyxPascalSyntaxTypeForm;
begin
  Enter;
  try
    LStart := Tok.Span.Start;
    LEntry := AddDeclaration(AScope, psdkType, AVisibility, FPosition);
    Step;

    if Sym(psoLess) then
    begin
      LGenericStart := FPosition;
      Angles;
      FDeclarations[LEntry].FGenericSpan := Range(LGenericStart, FPosition);
    end;

    if not Sym(psoEqual) then
    begin
      Include(FDeclarations[LEntry].FFlags, psdfIncomplete);
      Problem(psiExpectedSeparator, 'Add an equals sign and type expression', Tok.Span);
      Separator;
      Exit;
    end;
    Step;
    LTypeStart := FPosition;

    if EOF or Sym(psoSemicolon) then
    begin
      Include(FDeclarations[LEntry].FFlags, psdfIncomplete);
      Problem(psiExpectedName, 'Add a type expression after the equals sign', Tok.Span);
      FDeclarations[LEntry].FTypeSpan := Range(LTypeStart, FPosition);
      Separator;
      FDeclarations[LEntry].FHeaderSpan := SpanFrom(LStart);
      Exit;
    end;

    if Word(pkwPacked) then
    begin
      Step;
    end;
    LForm := Form;
    FDeclarations[LEntry].FForm := LForm;

    if LForm in [pstfClass, pstfInterface, pstfRecord, pstfObject] then
    begin
      Step;

      if Word(pkwSealed) or Word(pkwAbstract) then
      begin
        Step;
      end;

      if Sym(psoSemicolon) then
      begin
        Include(FDeclarations[LEntry].FFlags, psdfForward);
      end
      else
      begin

        if Sym(psoOpenParen) then
        begin
          LAncestor := FPosition;
          Groups(psoOpenParen, psoCloseParen);
          FDeclarations[LEntry].FAncestorSpan := Range(LAncestor, FPosition);
        end;
        LMemberScope := AddScope(psskMembers, AScope, LEntry, Tok.Span.Start,
          FDeclarations[LEntry].Name);
        FDeclarations[LEntry].FNestedScope := LMemberScope;

        if (LForm = pstfClass) and Sym(psoSemicolon) then
        begin
          { Delphi's empty descendant spelling class(TAncestor) has no end. }
          FScopes[LMemberScope].FComplete := True;
          FScopes[LMemberScope].FSpan := NyxPascalSpan(Tok.Span.Start, 0);
        end
        else
        begin
          Declarations(LMemberScope, psavUnspecified, True);

          if Word(pkwEnd) then
          begin
            Step;
            FinishScope(LMemberScope, True);
          end
          else
          begin
            Include(FDeclarations[LEntry].FFlags, psdfIncomplete);
            Problem(psiUnclosedScope, 'Finish this structured type with end', Tok.Span);
            FinishScope(LMemberScope, False);
          end;
        end;
      end;
    end
    else if LForm = pstfEnumeration then
    begin
      Step;
      while not EOF and not Sym(psoCloseParen) do
      begin

        if Tok.Kind = ptkWord then
        begin
          LEnum := AddDeclaration(AScope, psdkEnumValue, AVisibility, FPosition);
          Step;
          FDeclarations[LEnum].FTypeSpan := FDeclarations[LEntry].NameSpan;

          if Sym(psoEqual) or Sym(psoAssign) then
          begin
            Step;
            Expression(psoCloseParen, True);
          end;
        end
        else
        begin
          Problem(psiExpectedName, 'An enum entry needs an identifier', Tok.Span);
          Step;
        end;

        if Sym(psoComma) then
        begin
          Step;
        end
        else if not Sym(psoCloseParen) then
        begin
          Break;
        end;
      end;

      if Sym(psoCloseParen) then
      begin
        Step;
      end
      else
      begin
        Include(FDeclarations[LEntry].FFlags, psdfIncomplete);
        Problem(psiUnclosedScope, 'Finish this enumeration', Tok.Span);
      end;
    end
    else
    begin
      Expression(psoSemicolon);
    end;
    FDeclarations[LEntry].FTypeSpan := Range(LTypeStart, FPosition);
    Separator;
    FDeclarations[LEntry].FHeaderSpan := SpanFrom(LStart);
  finally
    Leave;
  end;
end;

function SectionBoundary(AWord: TNyxPascalWord): Boolean;
begin
  Result := AWord in [pkwType, pkwVar, pkwThreadvar, pkwConst, pkwResourcestring,
    pkwProcedure, pkwFunction, pkwConstructor, pkwDestructor, pkwImplementation,
    pkwInterface, pkwBegin, pkwEnd, pkwInitialization, pkwFinalization,
    pkwPrivate, pkwProtected, pkwPublic, pkwPublished, pkwStrict, pkwAutomated,
    pkwClass, pkwProperty, pkwGeneric];
end;

procedure TSyntaxSnapshot.TypeSection(AScope: Integer; AVisibility: TNyxPascalSyntaxVisibility);
var
  LBefore: Integer;
begin
  while not EOF do
  begin

    if Word(pkwGeneric) then
    begin
      Step;
    end
    else if SectionBoundary(Tok.Word) then
    begin
      Break;
    end;

    if Tok.Kind <> ptkWord then
    begin
      Break;
    end;
    LBefore := FPosition;
    ParseType(AScope, AVisibility);

    if FPosition = LBefore then
    begin
      Step;
    end;
  end;
end;

procedure TSyntaxSnapshot.Variables(AScope: Integer; AKind: TNyxPascalSyntaxDeclarationKind;
  AVisibility: TNyxPascalSyntaxVisibility; AClass: Boolean);
var
  LEntries: TOrdinals;
  LEntry: Integer;
  LStart: Integer;
  LType: Integer;
  LExpression: Integer;
  LAt: Integer;
  LTypeSpan: TNyxPascalSpan;
  LExpressionSpan: TNyxPascalSpan;
  LForm: TNyxPascalSyntaxTypeForm;
begin
  LStart := Tok.Span.Start;
  LEntries := nil;
  repeat

    if Tok.Kind <> ptkWord then
    begin
      Problem(psiExpectedName, 'A variable or field needs an identifier', Tok.Span);
      Break;
    end;
    LEntry := AddDeclaration(AScope, AKind, AVisibility, FPosition);
    SetLength(LEntries, Length(LEntries) + 1);
    LEntries[High(LEntries)] := LEntry;

    if AClass then
    begin
      Include(FDeclarations[LEntry].FFlags, psdfClassMember);
    end;
    Step;

    if not Sym(psoComma) then
    begin
      Break;
    end;
    Step;
  until EOF;
  LTypeSpan := NyxPascalSpan(Tok.Span.Start, 0);
  LExpressionSpan := LTypeSpan;
  LForm := pstfNone;

  if Sym(psoColon) then
  begin
    Step;
    LType := FPosition;
    LForm := Form;

    if Word(pkwProcedure) or Word(pkwFunction) or Word(pkwReference) then
    begin
      Expression(psoSemicolon);
    end
    else
    begin
      while not EOF and not Sym(psoSemicolon) and not Sym(psoEqual) and
        not Sym(psoAssign) and not SectionBoundary(Tok.Word) do
      begin

        if Sym(psoOpenParen) then
        begin
          Groups(psoOpenParen, psoCloseParen);
        end
        else if Sym(psoOpenBracket) then
        begin
          Groups(psoOpenBracket, psoCloseBracket);
        end
        else if Sym(psoLess) then
        begin
          Angles;
        end
        else
        begin
          Step;
        end;
      end;
    end;
    LTypeSpan := Range(LType, FPosition);
  end
  else if not Sym(psoAssign) then
  begin
    Problem(psiExpectedSeparator, 'A variable needs a declared type or explicit initializer', Tok.Span);
  end;

  if Sym(psoEqual) or Sym(psoAssign) then
  begin
    Step;
    LExpression := FPosition;
    Expression(psoSemicolon);
    LExpressionSpan := Range(LExpression, FPosition);
  end;
  for LAt := 0 to High(LEntries) do
  begin
    LEntry := LEntries[LAt];
    FDeclarations[LEntry].FTypeSpan := LTypeSpan;
    FDeclarations[LEntry].FExpressionSpan := LExpressionSpan;
    FDeclarations[LEntry].FForm := LForm;

    if (LTypeSpan.Length = 0) and (LExpressionSpan.Length = 0) then
    begin
      Include(FDeclarations[LEntry].FFlags, psdfIncomplete);
    end;
  end;
  Separator;
  for LAt := 0 to High(LEntries) do
  begin
    FDeclarations[LEntries[LAt]].FHeaderSpan := SpanFrom(LStart);
  end;
end;

procedure TSyntaxSnapshot.Constants(AScope: Integer;
  AKind: TNyxPascalSyntaxDeclarationKind; AVisibility: TNyxPascalSyntaxVisibility);
var
  LEntry: Integer;
  LStart: Integer;
  LType: Integer;
  LExpression: Integer;
begin
  LStart := Tok.Span.Start;
  LEntry := AddDeclaration(AScope, AKind, AVisibility, FPosition);
  Step;

  if Sym(psoColon) then
  begin
    Step;
    LType := FPosition;
    while not EOF and not Sym(psoEqual) and not Sym(psoSemicolon) and
      not SectionBoundary(Tok.Word) do
    begin

      if Sym(psoOpenBracket) then
      begin
        Groups(psoOpenBracket, psoCloseBracket);
      end
      else
      begin
        Step;
      end;
    end;
    FDeclarations[LEntry].FTypeSpan := Range(LType, FPosition);
  end;

  if Sym(psoEqual) then
  begin
    Step;
    LExpression := FPosition;
    Expression(psoSemicolon);
    FDeclarations[LEntry].FExpressionSpan := Range(LExpression, FPosition);
  end
  else
  begin
    Include(FDeclarations[LEntry].FFlags, psdfIncomplete);
    Problem(psiExpectedSeparator, 'A constant needs its value expression', Tok.Span);
  end;
  Separator;
  FDeclarations[LEntry].FHeaderSpan := SpanFrom(LStart);
end;

procedure TSyntaxSnapshot.Parameters(AScope: Integer);
var
  LEntries: TOrdinals;
  LEntry: Integer;
  LType: Integer;
  LMode: TNyxPascalSyntaxParameterMode;
  LAt: Integer;
begin
  Step;
  while not EOF and not Sym(psoCloseParen) do
  begin
    LMode := pspmValue;
    case Tok.Word of
      pkwConst:
        begin
          LMode := pspmConst;
        end;
      pkwVar:
        begin
          LMode := pspmVar;
        end;
      pkwOut:
        begin
          LMode := pspmOut;
        end;
      pkwConstref:
        begin
          LMode := pspmConstRef;
        end;
    else
      begin
        { Ordinary parameter names retain value passing. }
      end;
    end;

    if LMode <> pspmValue then
    begin
      Step;
    end;
    LEntries := nil;
    repeat

      if Tok.Kind <> ptkWord then
      begin
        Problem(psiExpectedName, 'A parameter needs an identifier', Tok.Span);
        Break;
      end;
      LEntry := AddDeclaration(AScope, psdkParameter, psavLocal, FPosition);
      FDeclarations[LEntry].FParameterMode := LMode;
      SetLength(LEntries, Length(LEntries) + 1);
      LEntries[High(LEntries)] := LEntry;
      Step;

      if not Sym(psoComma) then
      begin
        Break;
      end;
      Step;
    until EOF;

    if Sym(psoColon) then
    begin
      Step;
      LType := FPosition;
      while not EOF and not Sym(psoSemicolon) and not Sym(psoCloseParen) and not Sym(psoEqual) do
      begin

        if Sym(psoOpenParen) then
        begin
          Groups(psoOpenParen, psoCloseParen);
        end
        else if Sym(psoOpenBracket) then
        begin
          Groups(psoOpenBracket, psoCloseBracket);
        end
        else if Sym(psoLess) then
        begin
          Angles;
        end
        else
        begin
          Step;
        end;
      end;
      for LAt := 0 to High(LEntries) do
      begin
        FDeclarations[LEntries[LAt]].FTypeSpan := Range(LType, FPosition);
      end;
    end;

    if Sym(psoEqual) then
    begin
      Step;
      while not EOF and not Sym(psoSemicolon) and not Sym(psoCloseParen) do
      begin

        if Sym(psoOpenParen) then
        begin
          Groups(psoOpenParen, psoCloseParen);
        end
        else
        begin
          Step;
        end;
      end;
    end;

    if not Sym(psoSemicolon) then
    begin
      Break;
    end;
    Step;
  end;

  if Sym(psoCloseParen) then
  begin
    Step;
  end
  else
  begin
    Problem(psiUnclosedScope, 'Finish this parameter list', Tok.Span);
  end;
end;

function RoutineModifier(AWord: TNyxPascalWord): Boolean;
begin
  Result := AWord in [pkwForward, pkwExternal, pkwAbstract, pkwOverload, pkwOverride,
    pkwVirtual, pkwDynamic, pkwInline, pkwReintroduce, pkwAssembler, pkwCdecl,
    pkwStdcall, pkwPascal, pkwSafecall, pkwRegister, pkwVarargs, pkwNostackframe,
    pkwStatic, pkwDeprecated, pkwPlatform, pkwExperimental, pkwFinal];
end;

procedure TSyntaxSnapshot.Routine(AScope: Integer;
  AVisibility: TNyxPascalSyntaxVisibility; AClass: Boolean);
var
  LEntry: Integer;
  LScope: Integer;
  LStart: Integer;
  LName: Integer;
  LFinalName: Integer;
  LType: Integer;
  LGeneric: Integer;
  LHasBody: Boolean;
  LIsFunction: Boolean;
  LFullName: TNyxText;
  LOwner: TNyxText;
  LGenericSpan: TNyxPascalSpan;
begin
  Enter;
  try
    LStart := Tok.Span.Start;
    LIsFunction := Word(pkwFunction);
    Step;

    if Tok.Kind <> ptkWord then
    begin
      Problem(psiExpectedName, 'A routine needs an identifier', Tok.Span);
      Expression(psoSemicolon);
      Separator;
      Exit;
    end;
    LName := FPosition;
    LFinalName := LName;
    LGenericSpan := NyxPascalSpan(Tok.Span.Start, 0);
    LFullName := Name;
    Step;
    LOwner := '';

    if Sym(psoLess) then
    begin
      LGeneric := FPosition;
      Angles;
      LGenericSpan := Range(LGeneric, FPosition);
    end;
    while Sym(psoDot) and (Tok(1).Kind = ptkWord) do
    begin
      Step;
      LOwner := LFullName;
      LFullName := LFullName + '.' + Name;
      LFinalName := FPosition;
      Step;

      if Sym(psoLess) then
      begin
        LGeneric := FPosition;
        Angles;
        LGenericSpan := Range(LGeneric, FPosition);
      end;
    end;
    LEntry := AddDeclaration(AScope, psdkRoutine, AVisibility, LFinalName);
    FDeclarations[LEntry].FGenericSpan := LGenericSpan;
    FDeclarations[LEntry].FOwnerName := LOwner;
    FDeclarations[LEntry].FNameSpan := Range(LName, FPosition);

    if AClass then
    begin
      Include(FDeclarations[LEntry].FFlags, psdfClassMember);
    end;
    LScope := AddScope(psskRoutine, AScope, LEntry, LStart, LFullName);
    FDeclarations[LEntry].FNestedScope := LScope;

    if Sym(psoOpenParen) then
    begin
      Parameters(LScope);
    end;

    if LIsFunction and Sym(psoColon) then
    begin
      Step;
      LType := FPosition;
      Expression(psoSemicolon);
      FDeclarations[LEntry].FTypeSpan := Range(LType, FPosition);
    end;
    Separator;
    LHasBody := not (AVisibility in [psavInterface]) and
      (FScopes[AScope].Kind <> psskMembers);
    while RoutineModifier(Tok.Word) do
    begin

      if Word(pkwForward) or Word(pkwExternal) or Word(pkwAbstract) then
      begin
        LHasBody := False;
        Include(FDeclarations[LEntry].FFlags, psdfForward);
      end;

      if Word(pkwOverload) then
      begin
        Include(FDeclarations[LEntry].FFlags, psdfOverload);
      end;
      Step;
      Expression(psoSemicolon);
      Separator;
    end;
    FDeclarations[LEntry].FHeaderSpan := SpanFrom(LStart);

    if LHasBody then
    begin
      Declarations(LScope, psavLocal, False);

      if Word(pkwBegin) or Word(pkwAsm) then
      begin

        if not Block(LScope) then
        begin
          Include(FDeclarations[LEntry].FFlags, psdfIncomplete);
        end;
        Separator;
      end
      else
      begin
        Include(FDeclarations[LEntry].FFlags, psdfIncomplete);
        Problem(psiUnclosedScope, 'Finish this routine implementation', Tok.Span);
      end;
    end;
    FinishScope(LScope, not (psdfIncomplete in FDeclarations[LEntry].Flags));
  finally
    Leave;
  end;
end;

procedure TSyntaxSnapshot.PropertyDeclaration(AScope: Integer;
  AVisibility: TNyxPascalSyntaxVisibility);
var
  LStart: Integer;
  LEntry: Integer;
  LType: Integer;
begin
  LStart := Tok.Span.Start;
  Step;

  if Tok.Kind <> ptkWord then
  begin
    Problem(psiExpectedName, 'A property needs an identifier', Tok.Span);
    Expression(psoSemicolon);
    Separator;
    Exit;
  end;
  LEntry := AddDeclaration(AScope, psdkProperty, AVisibility, FPosition);
  Step;

  if Sym(psoOpenBracket) then
  begin
    Groups(psoOpenBracket, psoCloseBracket);
  end;

  if Sym(psoColon) then
  begin
    Step;
    LType := FPosition;
    while not EOF and not Sym(psoSemicolon) and
      not (Tok.Word in [pkwRead, pkwWrite, pkwStored, pkwDefault, pkwNodefault,
        pkwImplements, pkwIndex, pkwDispid]) and not Word(pkwEnd) do
    begin

      if Sym(psoLess) then
      begin
        Angles;
      end
      else
      begin
        Step;
      end;
    end;
    FDeclarations[LEntry].FTypeSpan := Range(LType, FPosition);
    FDeclarations[LEntry].FForm := pstfNamed;
  end;
  Expression(psoSemicolon);
  Separator;

  if Word(pkwDefault) or Word(pkwNodefault) then
  begin
    Step;
    Separator;
  end;
  FDeclarations[LEntry].FHeaderSpan := SpanFrom(LStart);
end;

procedure TSyntaxSnapshot.Imports(AScope: Integer);
var
  LName: TNyxText;
  LStart: Integer;
  LEntry: Integer;
begin
  Step;
  while not EOF and (Tok.Kind = ptkWord) do
  begin
    LStart := FPosition;
    LName := Name;
    Step;
    while Sym(psoDot) and (Tok(1).Kind = ptkWord) do
    begin
      Step;
      LName := LName + '.' + Name;
      Step;
    end;
    LEntry := FImportCount;

    if LEntry >= FOptions.MaximumDeclarations then
    begin
      raise ENyxPascalSyntaxLimit.Create('Pascal import syntax exceeds its budget');
    end;

    if LEntry = Length(FImports) then
    begin
      SetLength(FImports, Capacity(LEntry, FOptions.MaximumDeclarations));
    end;
    FImports[LEntry].FName := LName;
    FImports[LEntry].FSpan := Range(LStart, FPosition);
    FImports[LEntry].FScope := AScope;
    FImports[LEntry].FConditional := FConditional[LStart];
    Inc(FImportCount);

    if Word(pkwIn) then
    begin
      Step;
      Expression(psoSemicolon, True);
    end;

    if not Sym(psoComma) then
    begin
      Break;
    end;
    Step;
  end;
  Separator;
end;

procedure TSyntaxSnapshot.Declarations(AScope: Integer;
  AVisibility: TNyxPascalSyntaxVisibility; AMembers: Boolean);
var
  LSection: TNyxPascalWord;
  LBefore: Integer;
  LClass: Boolean;
  LStrict: Boolean;
begin
  Enter;
  try
    LSection := pkwNone;
    while not EOF and not Word(pkwEnd) and not Word(pkwBegin) and not Word(pkwAsm) do
    begin
      LBefore := FPosition;

      if Word(pkwInterface) then
      begin
        AVisibility := psavInterface;
        LSection := pkwNone;
        Step;
      end
      else if Word(pkwImplementation) then
      begin

        if AScope <> 0 then
        begin
          Break;
        end;
        AVisibility := psavImplementation;
        LSection := pkwNone;
        Step;
      end
      else if Tok.Word in [pkwUses, pkwRequires, pkwContains] then
      begin
        Imports(AScope);
      end
      else if Word(pkwType) then
      begin
        Step;
        TypeSection(AScope, AVisibility);
        LSection := pkwNone;
      end
      else if Tok.Word in [pkwVar, pkwThreadvar, pkwConst, pkwResourcestring] then
      begin
        LSection := Tok.Word;
        Step;
      end
      else if Tok.Word in [pkwPrivate, pkwProtected, pkwPublic, pkwPublished,
        pkwAutomated, pkwStrict] then
      begin
        LStrict := Word(pkwStrict);

        if LStrict then
        begin
          Step;
        end;
        case Tok.Word of
          pkwPrivate:
            begin
              AVisibility := psavPrivate;

              if LStrict then
              begin
                AVisibility := psavStrictPrivate;
              end;
            end;
          pkwProtected:
            begin
              AVisibility := psavProtected;

              if LStrict then
              begin
                AVisibility := psavStrictProtected;
              end;
            end;
          pkwPublic:
            begin
              AVisibility := psavPublic;
            end;
          pkwPublished:
            begin
              AVisibility := psavPublished;
            end;
          pkwAutomated:
            begin
              AVisibility := psavAutomated;
            end;
        else
          begin
            Problem(psiExpectedName, 'Strict visibility needs private or protected', Tok.Span);
          end;
        end;
        Step;
        LSection := pkwNone;
      end
      else if Word(pkwProperty) then
      begin
        PropertyDeclaration(AScope, AVisibility);
        LSection := pkwNone;
      end
      else if Tok.Word in [pkwProcedure, pkwFunction, pkwConstructor, pkwDestructor,
        pkwClass, pkwGeneric] then
      begin
        LClass := Word(pkwClass);

        if LClass or Word(pkwGeneric) then
        begin
          Step;
        end;

        if Word(pkwVar) and LClass then
        begin
          Step;
          Variables(AScope, psdkField, AVisibility, True);
        end
        else if Tok.Word in [pkwProcedure, pkwFunction, pkwConstructor, pkwDestructor] then
        begin
          Routine(AScope, AVisibility, LClass);
        end
        else
        begin
          Unsupported(AScope, 'This class/generic declaration needs richer context', Tok.Span);
          Expression(psoSemicolon);
          Separator;
        end;
        LSection := pkwNone;
      end
      else if Sym(psoOpenBracket) then
      begin
        Groups(psoOpenBracket, psoCloseBracket);
      end
      else if AMembers and Word(pkwCase) then
      begin
        Unsupported(AScope, 'Variant record fields need a dedicated case-field model', Tok.Span);
        Expression(psoSemicolon);
        Separator;
      end
      else if Tok.Kind = ptkWord then
      begin
        case LSection of
          pkwVar, pkwThreadvar:
            begin
              Variables(AScope, psdkVariable, AVisibility);
            end;
          pkwConst:
            begin
              Constants(AScope, psdkConstant, AVisibility);
            end;
          pkwResourcestring:
            begin
              Constants(AScope, psdkResourceString, AVisibility);
            end;
        else

          if AMembers then
          begin
            Variables(AScope, psdkField, AVisibility);
          end
          else
          begin
            Unsupported(AScope, 'This declaration is not yet structurally classified', Tok.Span);
            Expression(psoSemicolon);
            Separator;
          end;
        end;
      end
      else
      begin
        Unsupported(AScope, 'Unrecognized declaration syntax is retained', Tok.Span);
        Step;
      end;

      if FPosition = LBefore then
      begin
        Step;
      end;
    end;
  finally
    Leave;
  end;
end;

function TSyntaxSnapshot.Block(AScope: Integer): Boolean;
var
  LScope: Integer;
  LStart: Integer;
  LDepth: Integer;
  LRepeat: Integer;
begin
  Enter;
  try
    LStart := Tok.Span.Start;
    LScope := AddScope(psskBlock, AScope, -1, LStart, '');
    Step;
    LDepth := 1;
    LRepeat := 0;
    while not EOF and (LDepth > 0) do
    begin

      if Tok.Word in [pkwBegin, pkwTry, pkwCase, pkwAsm] then
      begin
        Inc(LDepth);

        if LDepth > FOptions.MaximumDepth then
        begin
          raise ENyxPascalSyntaxLimit.Create('Pascal body nesting exceeds its budget');
        end;
      end
      else if Word(pkwRepeat) then
      begin
        Inc(LRepeat);
      end
      else if Word(pkwUntil) then
      begin

        if LRepeat > 0 then
        begin
          Dec(LRepeat);
        end;
      end
      else if Word(pkwEnd) then
      begin
        Dec(LDepth);
      end
      else if Tok.Word in [pkwProcedure, pkwFunction] then
      begin
        Unsupported(LScope, 'Anonymous routine scopes need expression-context analysis', Tok.Span);
      end
      else if Tok.Word in [pkwVar, pkwConst, pkwType] then
      begin
        Unsupported(LScope, 'Inline declarations need statement-scope analysis', Tok.Span);
      end;
      Step;
    end;
    FinishScope(LScope, (LDepth = 0) and (LRepeat = 0));
    Result := FScopes[LScope].Complete;

    if not FScopes[LScope].Complete then
    begin
      Problem(psiUnclosedScope, 'Finish this Pascal body', Tok.Span);
    end;
  finally
    Leave;
  end;
end;

function DirectiveWord(const AText: TNyxText): TNyxText;
var
  LAt: Integer;
  LStart: Integer;
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
  Result := NyxPascalASCIIKey(Copy(AText, LStart, LAt - LStart));

  if (Result = 'i') and (LAt <= Length(AText)) and (AText[LAt] in ['+', '-']) then
  begin
    Result := 'iochecks';
  end;
end;

procedure TSyntaxSnapshot.BuildTokens;
var
  LAt: Integer;
  LCount: Integer;
  LDepth: Integer;
  LDirective: TNyxText;
  LToken: TNyxPascalToken;
  LConditionalReported: Boolean;
  LIncludeReported: Boolean;
begin
  SetLength(FTokens, FLexical.Count);
  SetLength(FConditional, FLexical.Count);
  LCount := 0;
  LDepth := 0;
  LConditionalReported := False;
  LIncludeReported := False;
  for LAt := 0 to FLexical.Count - 1 do
  begin

    if (LAt mod 1024 = 0) and (FCancellation <> nil) and FCancellation.Cancelled then
    begin
      raise ENyxPascalAnalysisCancelled.Create('Pascal syntax analysis cancelled');
    end;
    LToken := FLexical.Token(LAt);

    if LToken.Kind = ptkDirective then
    begin
      LDirective := DirectiveWord(FLexical.Text(LAt));

      if (LDirective = 'ifdef') or (LDirective = 'ifndef') or (LDirective = 'if') or
        (LDirective = 'ifopt') then
      begin
        Inc(LDepth);

        if LDepth > FOptions.MaximumDepth then
        begin
          raise ENyxPascalSyntaxLimit.Create('Pascal conditional syntax exceeds its budget');
        end;

        if not LConditionalReported then
        begin
          Problem(psiConditionalContextRequired,
            'Conditional declarations require an explicit compiler context', LToken.Span);
          LConditionalReported := True;
        end;
      end
      else if (LDirective = 'endif') or (LDirective = 'ifend') then
      begin

        if LDepth > 0 then
        begin
          Dec(LDepth);
        end
        else
        begin
          Problem(psiUnterminatedConditional, 'Unmatched conditional terminator', LToken.Span);
        end;
      end
      else if ((LDirective = 'include') or (LDirective = 'i')) and not LIncludeReported then
      begin
        Problem(psiIncludeContextRequired,
          'Captured include expansion is required before binding this unit', LToken.Span);
        LIncludeReported := True;
      end;
    end
    else if not (LToken.Kind in [ptkWhitespace, ptkComment]) then
    begin
      FTokens[LCount] := LAt;
      FConditional[LCount] := LDepth > 0;
      Inc(LCount);
    end;
  end;

  if LDepth > 0 then
  begin
    Problem(psiUnterminatedConditional, 'Finish this conditional compilation block',
      FLexical.Token(FLexical.Count - 1).Span);
  end;
  SetLength(FTokens, LCount);
  SetLength(FConditional, LCount);
end;

procedure TSyntaxSnapshot.Build;
var
  LModule: TNyxText;
  LModuleScope: Integer;
  LStart: Integer;
  LBodyClosed: Boolean;
begin
  BuildTokens;

  if FOptions.StartingContext = psfMembers then
  begin
    LModuleScope := AddScope(psskMembers, -1, -1, 0, '');
  end
  else
  begin
    LModuleScope := AddScope(psskModule, -1, -1, 0, '');
  end;

  if (FOptions.StartingContext = psfModule) and
    (Tok.Word in [pkwUnit, pkwProgram, pkwLibrary, pkwPackage]) then
  begin
    Step;
    LModule := Name;
    Step;
    while Sym(psoDot) and (Tok(1).Kind = ptkWord) do
    begin
      Step;
      LModule := LModule + '.' + Name;
      Step;
    end;
    FScopes[LModuleScope].FName := LModule;

    if Sym(psoOpenParen) then
    begin
      Groups(psoOpenParen, psoCloseParen);
    end;
    Separator;
  end;

  if FOptions.StartingContext = psfTypes then
  begin
    TypeSection(0, psavInterface);
  end
  else if FOptions.StartingContext = psfInterfaceDeclarations then
  begin
    Declarations(0, psavInterface, False);
  end
  else
  begin

    if FOptions.StartingContext = psfMembers then
    begin
      Declarations(0, psavUnspecified, True);
    end
    else
    begin
      Declarations(0, psavImplementation, False);
    end;
  end;

  LBodyClosed := False;

  if Word(pkwBegin) or Word(pkwAsm) then
  begin
    LBodyClosed := Block(0);
  end
  else if Word(pkwInitialization) or Word(pkwFinalization) then
  begin
    { Initialization/finalization statements remain opaque until statement/expression
      scopes are modeled; they must not become fake unit declarations. }
    LStart := Tok.Span.Start;
    Unsupported(0, 'Initialization/finalization bodies need statement analysis',
      NyxPascalSpan(LStart, FLexical.Scalars - LStart));
  end;
  { A physical fragment closes at EOF. A complete module needs its final end-dot
    boundary; skipped initialization statements cannot grant closed structure. }

  if FOptions.StartingContext <> psfModule then
  begin
    FScopes[0].FComplete := EOF;
  end
  else
  begin
    FScopes[0].FComplete :=
      (Word(pkwEnd) and Sym(psoDot, 1) and (Tok(2).Kind = ptkEOF)) or
      (LBodyClosed and Sym(psoDot) and (Tok(1).Kind = ptkEOF));
  end;
end;

function TSyntaxSnapshot.Lexical: INyxPascalLexicalSnapshot;
begin
  Result := FLexical;
end;

function TSyntaxSnapshot.ScopeCount: Integer;
begin
  Result := FScopeCount;
end;

function TSyntaxSnapshot.Scope(AIndex: Integer): TNyxPascalSyntaxScope;
begin

  if (AIndex < 0) or (AIndex >= FScopeCount) then
  begin
    raise ENyxPascalLexical.Create('Pascal scope index is outside its syntax snapshot');
  end;
  Result := FScopes[AIndex];
end;

function TSyntaxSnapshot.ScopeAt(AOffset: Integer): Integer;
var
  LAt: Integer;
  LDepth: Integer;
  LBestDepth: Integer;
  LParent: Integer;
begin

  if (AOffset < 0) or (AOffset > FLexical.Scalars) then
  begin
    raise ENyxPascalLexical.Create('Pascal scope position is outside its syntax snapshot');
  end;
  Result := 0;
  LBestDepth := 0;
  for LAt := 1 to FScopeCount - 1 do
  begin

    if (AOffset >= FScopes[LAt].Span.Start) and
      ((AOffset < FScopes[LAt].Span.Finish) or
        ((AOffset = FLexical.Scalars) and not FScopes[LAt].Complete)) then
    begin
      LDepth := 0;
      LParent := LAt;
      while LParent >= 0 do
      begin
        Inc(LDepth);
        LParent := FScopes[LParent].Parent;
      end;

      if LDepth > LBestDepth then
      begin
        Result := LAt;
        LBestDepth := LDepth;
      end;
    end;
  end;
end;

function TSyntaxSnapshot.DeclarationCount: Integer;
begin
  Result := FDeclarationCount;
end;

function TSyntaxSnapshot.Declaration(AIndex: Integer): TNyxPascalSyntaxDeclaration;
begin

  if (AIndex < 0) or (AIndex >= FDeclarationCount) then
  begin
    raise ENyxPascalLexical.Create('Pascal declaration index is outside its syntax snapshot');
  end;
  Result := FDeclarations[AIndex];
end;

function TSyntaxSnapshot.Named(AScope: Integer; const AName: TNyxText): Integer;
begin
  Scope(AScope);
  Result := FNames.IndexOf(TNyxText(IntToStr(AScope)) + ':' + NyxPascalASCIIKey(AName));
end;

function TSyntaxSnapshot.ImportCount: Integer;
begin
  Result := FImportCount;
end;

function TSyntaxSnapshot.ImportAt(AIndex: Integer): TNyxPascalSyntaxImport;
begin

  if (AIndex < 0) or (AIndex >= FImportCount) then
  begin
    raise ENyxPascalLexical.Create('Pascal import index is outside its syntax snapshot');
  end;
  Result := FImports[AIndex];
end;

function TSyntaxSnapshot.DiagnosticCount: Integer;
begin
  Result := FDiagnosticCount;
end;

function TSyntaxSnapshot.Diagnostic(AIndex: Integer): TNyxPascalSyntaxDiagnostic;
begin

  if (AIndex < 0) or (AIndex >= FDiagnosticCount) then
  begin
    raise ENyxPascalLexical.Create('Pascal syntax diagnostic index is outside its snapshot');
  end;
  Result := FDiagnostics[AIndex];
end;

function NewNyxPascalSyntaxSnapshot(const ASource: TNyxText): INyxPascalSyntaxSnapshot;
begin
  Result := NewNyxPascalSyntaxSnapshot(ASource, TNyxPascalSyntaxOptions.Defaults);
end;

function NewNyxPascalSyntaxSnapshot(const ASource: TNyxText;
  const AOptions: TNyxPascalSyntaxOptions;
  const ACancellation: INyxCancellationScope): INyxPascalSyntaxSnapshot;
begin
  Result := TSyntaxSnapshot.Create(ASource, AOptions, ACancellation);
end;

end.
