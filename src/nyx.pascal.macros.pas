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
unit nyx.pascal.macros;
{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.scheduler, nyx.pascal.lexing, nyx.pascal.context;

type
  { A replacement cannot be interpreted safely. Ordinary source translation may
    retain this as a diagnostic; cancellation and hard quotas remain separate. }
  ENyxPascalMacro = class(ENyxPascalContext);
  { A name re-enters its own expansion ancestry. No partial token list escapes. }
  ENyxPascalMacroCycle = class(ENyxPascalMacro);
  { Explicit byte/token/depth/work limits refuse the complete expansion. }
  ENyxPascalMacroLimit = class(ENyxPascalContext);
  { Source mode expands ordinary identifiers. Conditions protect defined()'s
    name argument, which asks about a symbol rather than its replacement. }
  TNyxPascalMacroUse = (pmuSource, pmuCondition);
  { Value-only definition origin. Occurrence -1 denotes explicit host facts with
    no authored source position; otherwise Span covers the exact replacement. }
  TNyxPascalMacroDefinition = record
    Name: TNyxPascalDefineRef;
    Replacement: TNyxText;
    Occurrence: Integer;
    Span: TNyxPascalSpan;
    Known: Boolean;
  end;
  { One recursive use. Parent is a trace ordinal, -1 at the root. Definition is
    copied metadata; token spans remain relative to this replacement's lexical
    view and can be added to Definition.Span only for an authored definition. }
  TNyxPascalMacroTrace = record
    Definition: TNyxPascalMacroDefinition;
    Parent: Integer;
  end;
  { Detached effective tokens and complete recursive trace. Root tokens have
    trace -1. No caller/environment/cancellation survives construction; retained
    lexical views own exact fragments, never rewritten physical source. }
  INyxPascalMacroExpansion = interface(IInterface)
    ['{B5CF48F3-D4B3-488E-A07F-FFBA148A2101}']
    function Count: Integer;
    function Token(AIndex: Integer): TNyxPascalToken;
    function Text(AIndex: Integer): TNyxText;
    function TokenTrace(AIndex: Integer): Integer;
    function TraceCount: Integer;
    function Trace(AIndex: Integer): TNyxPascalMacroTrace;
    function WorkBytes: Integer;
    function Joined: TNyxText;
  end;
  { Immutable bounded replacement table. Names are case-insensitive, definitions
    remain local to the owning translation unit and later redefinitions do not
    mutate earlier snapshots. No filesystem, compiler or authored code executes. }
  INyxPascalMacroEnvironment = interface(IInterface)
    ['{B5CF48F3-D4B3-488E-A07F-FFBA148A2102}']
    function Has(const AName: TNyxPascalDefineRef): Boolean;
    function Definition(const AName: TNyxPascalDefineRef): TNyxPascalMacroDefinition;
    function WithDefinition(const AValue: TNyxPascalMacroDefinition): INyxPascalMacroEnvironment;
    { Undef removes replacement status. A plain redefinition of an existing
      replacement clears its text but retains replacement status, as FPC does. }
    function Without(const AName: TNyxPascalDefineRef): INyxPascalMacroEnvironment;
    { Retain source evidence while revoking every captured replacement's known
      meaning after an unavailable include or another unknown textual effect. }
    function UnknownState: INyxPascalMacroEnvironment;
    function SameMacros(const AOther: INyxPascalMacroEnvironment): Boolean;
    function Count: Integer;
    function At(AIndex: Integer): TNyxPascalMacroDefinition;
    { Caller supplies positive remaining budgets. Trivia is retained, EOF is not.
      Replacement directives/invalid lexical fragments refuse explicitly until
      their effects can be composed with the owning translation reader. }
    function Expand(const AText: TNyxText; AUse: TNyxPascalMacroUse;
      const ALexical: TNyxPascalLexicalOptions; ABytes, ATokens, ADepth: Integer;
      const ACancellation: INyxCancellationScope = nil): INyxPascalMacroExpansion;
  end;

function NewNyxPascalMacroEnvironment: INyxPascalMacroEnvironment;
{ Owned expansion views may donate detached scanner proof for a complete atom.
  Alternate expansion implementations retain ordinary projection scanning. }
function NyxPascalPreparedMacroToken(const AExpansion: INyxPascalMacroExpansion;
  AIndex: Integer): TNyxPascalPreparedToken;

implementation

uses SysUtils, nyx.text.index, nyx.bytes, nyx.editing;

type
  { Private binding from an expansion ordinal to its owned lexical fragment.
    Returned values retain neither expansion, trace nor source reader. }
  IPreparedExpansion = interface(IInterface)
    ['{D6C0241E-734E-4B8C-B015-532967EDC039}']
    function PreparedToken(AIndex: Integer): TNyxPascalPreparedToken;
  end;
  { Immutable table owns a detached value vector and ordinal index. Construction
    publishes only after validation; strings are managed immutable values. }
  TEnvironment = class(TInterfacedObject, INyxPascalMacroEnvironment)
  private
    FValues: array of TNyxPascalMacroDefinition;
    FNames: TNyxTextIndex;
    function CopyTable: TEnvironment;
  public
    constructor Create;
    destructor Destroy; override;
    function Has(const AName: TNyxPascalDefineRef): Boolean;
    function Definition(const AName: TNyxPascalDefineRef): TNyxPascalMacroDefinition;
    function WithDefinition(const AValue: TNyxPascalMacroDefinition): INyxPascalMacroEnvironment;
    function Without(const AName: TNyxPascalDefineRef): INyxPascalMacroEnvironment;
    function UnknownState: INyxPascalMacroEnvironment;
    function SameMacros(const AOther: INyxPascalMacroEnvironment): Boolean;
    function Count: Integer;
    function At(AIndex: Integer): TNyxPascalMacroDefinition;
    function Expand(const AText: TNyxText; AUse: TNyxPascalMacroUse;
      const ALexical: TNyxPascalLexicalOptions; ABytes, ATokens, ADepth: Integer;
      const ACancellation: INyxCancellationScope): INyxPascalMacroExpansion;
  end;
  { Ordinals into class-owned lexical views avoid interfaces in record vectors. }
  TExpandedToken = record
    View: Integer;
    Token: Integer;
    Trace: Integer;
  end;
  { Construction owns callback leases; publication drops them. Trace/value/token
    vectors and lexical fragments remain detached and have no parent back links. }
  TExpansion = class(TInterfacedObject, INyxPascalMacroExpansion, IPreparedExpansion)
  private
    FEnvironment: INyxPascalMacroEnvironment;
    FCancellation: INyxCancellationScope;
    FUse: TNyxPascalMacroUse;
    FOptions: TNyxPascalLexicalOptions;
    FByteLimit: Integer;
    FTokenLimit: Integer;
    FDepthLimit: Integer;
    FBytes: Integer;
    FCount: Integer;
    FTokens: array of TExpandedToken;
    FViews: array of INyxPascalLexicalSnapshot;
    FTraces: array of TNyxPascalMacroTrace;
    procedure Check;
    procedure Visit(const AText: TNyxText; AParent, ADepth: Integer);
    procedure Append(AView, AToken, ATrace: Integer);
    procedure VerifyJoined;
    function Entry(AIndex: Integer): TExpandedToken;
  public
    procedure Build(const AEnvironment: INyxPascalMacroEnvironment;
      const AText: TNyxText; AUse: TNyxPascalMacroUse;
      const ALexical: TNyxPascalLexicalOptions; ABytes, ATokens, ADepth: Integer;
      const ACancellation: INyxCancellationScope);
    function Count: Integer;
    function Token(AIndex: Integer): TNyxPascalToken;
    function Text(AIndex: Integer): TNyxText;
    function TokenTrace(AIndex: Integer): Integer;
    function TraceCount: Integer;
    function Trace(AIndex: Integer): TNyxPascalMacroTrace;
    function WorkBytes: Integer;
    function Joined: TNyxText;
    function PreparedToken(AIndex: Integer): TNyxPascalPreparedToken;
  end;

constructor TEnvironment.Create;
begin
  inherited Create;
  FNames := TNyxTextIndex.Create;
end;

destructor TEnvironment.Destroy;
begin
  FNames.Free;
  inherited Destroy;
end;

function TEnvironment.CopyTable: TEnvironment;
begin
  Result := TEnvironment.Create;
  Result.FValues := Copy(FValues);
  Result.FNames.Free;
  Result.FNames := FNames.Clone;
end;

function TEnvironment.Has(const AName: TNyxPascalDefineRef): Boolean;
begin
  Result := FNames.IndexOf(NyxPascalASCIIKey(AName.Name)) >= 0;
end;

function TEnvironment.Definition(const AName: TNyxPascalDefineRef): TNyxPascalMacroDefinition;
begin
  Result := At(FNames.IndexOf(NyxPascalASCIIKey(AName.Name)));
end;

function TEnvironment.WithDefinition(const AValue: TNyxPascalMacroDefinition): INyxPascalMacroEnvironment;
var
  LCopy: TEnvironment;
  LOwner: INyxPascalMacroEnvironment;
  LAt: Integer;
  LBytes: Integer;
  LIndex: Integer;
begin
  NyxPascalDefine(AValue.Name.Name);

  if (AValue.Occurrence < -1) or (NyxTextScalarCount(AValue.Replacement) <> AValue.Span.Length) then
  begin
    raise ENyxPascalMacro.Create('A replacement needs its exact scalar origin');
  end;
  LAt := FNames.IndexOf(NyxPascalASCIIKey(AValue.Name.Name));

  if (LAt < 0) and (Length(FValues) >= 2048) then
  begin
    raise ENyxPascalMacroLimit.Create('Replacement names exceed 2048');
  end;
  LBytes := NyxUTF8ByteCount(AValue.Replacement);
  for LIndex := 0 to High(FValues) do
  begin

    if LIndex <> LAt then
    begin

      if NyxUTF8ByteCount(FValues[LIndex].Replacement) > 4 * 1024 * 1024 - LBytes then
      begin
        raise ENyxPascalMacroLimit.Create('Replacement definitions exceed four MiB');
      end;
      Inc(LBytes, NyxUTF8ByteCount(FValues[LIndex].Replacement));
    end;
  end;

  if LBytes > 4 * 1024 * 1024 then
  begin
    raise ENyxPascalMacroLimit.Create('Replacement definitions exceed four MiB');
  end;
  LCopy := CopyTable;
  LOwner := LCopy;

  if LAt < 0 then
  begin
    LAt := Length(LCopy.FValues);
    SetLength(LCopy.FValues, LAt + 1);
    LCopy.FNames.AddFirst(NyxPascalASCIIKey(AValue.Name.Name), LAt);
  end;
  LCopy.FValues[LAt] := AValue;
  Result := LOwner;
end;

function TEnvironment.Without(const AName: TNyxPascalDefineRef): INyxPascalMacroEnvironment;
var
  LAt: Integer;
  LValue: TNyxPascalMacroDefinition;
  LOwner: INyxPascalMacroEnvironment;
  LCopy: TEnvironment;
begin
  LCopy := TEnvironment.Create;
  LOwner := LCopy;
  for LAt := 0 to High(FValues) do
  begin
    LValue := FValues[LAt];

    if NyxPascalASCIIKey(LValue.Name.Name) <> NyxPascalASCIIKey(AName.Name) then
    begin
      LCopy.FNames.AddFirst(NyxPascalASCIIKey(LValue.Name.Name), Length(LCopy.FValues));
      SetLength(LCopy.FValues, Length(LCopy.FValues) + 1);
      LCopy.FValues[High(LCopy.FValues)] := LValue;
    end;
  end;
  Result := LOwner;
end;

function TEnvironment.Count: Integer;
begin
  Result := Length(FValues);
end;

function TEnvironment.UnknownState: INyxPascalMacroEnvironment;
var
  LCopy: TEnvironment;
  LOwner: INyxPascalMacroEnvironment;
  LAt: Integer;
begin
  LCopy := CopyTable;
  LOwner := LCopy;
  for LAt := 0 to High(LCopy.FValues) do
  begin
    LCopy.FValues[LAt].Known := False;
  end;
  Result := LOwner;
end;

function TEnvironment.At(AIndex: Integer): TNyxPascalMacroDefinition;
begin

  if (AIndex < 0) or (AIndex >= Length(FValues)) then
  begin
    raise ENyxPascalMacro.Create('Replacement definition is outside its snapshot');
  end;
  Result := FValues[AIndex];
end;

function TEnvironment.SameMacros(const AOther: INyxPascalMacroEnvironment): Boolean;
var
  LAt: Integer;
  LValue: TNyxPascalMacroDefinition;
begin
  Result := False;

  if (AOther = nil) or (AOther.Count <> Count) then
  begin
    Exit;
  end;
  for LAt := 0 to High(FValues) do
  begin
    LValue := AOther.At(LAt);

    if (LValue.Name.Name <> FValues[LAt].Name.Name) or
      (LValue.Replacement <> FValues[LAt].Replacement) or
      (LValue.Known <> FValues[LAt].Known) or
      (LValue.Occurrence <> FValues[LAt].Occurrence) or
      (LValue.Span.Start <> FValues[LAt].Span.Start) or
      (LValue.Span.Length <> FValues[LAt].Span.Length) then
    begin
      Exit;
    end;
  end;
  Result := True;
end;

procedure TExpansion.Check;
begin

  if (FCancellation <> nil) and FCancellation.Cancelled then
  begin
    raise ENyxPascalAnalysisCancelled.Create('Replacement expansion cancelled');
  end;
end;

procedure TExpansion.Append(AView, AToken, ATrace: Integer);
var
  LCapacity: Integer;
begin

  if FCount >= FTokenLimit then
  begin
    raise ENyxPascalMacroLimit.Create('Replacement tokens exceed their budget');
  end;

  if FCount = Length(FTokens) then
  begin
    LCapacity := FTokenLimit;

    if FCount < FTokenLimit - 256 then
    begin
      LCapacity := FCount + 256;
    end;
    SetLength(FTokens, LCapacity);
  end;
  FTokens[FCount].View := AView;
  FTokens[FCount].Token := AToken;
  FTokens[FCount].Trace := ATrace;
  Inc(FCount);
end;

procedure TExpansion.Visit(const AText: TNyxText; AParent, ADepth: Integer);
var
  LView: INyxPascalLexicalSnapshot;
  LViewIndex: Integer;
  LAt: Integer;
  LTrace: Integer;
  LAncestor: Integer;
  LToken: TNyxPascalToken;
  LName: TNyxPascalDefineRef;
  LDefinition: TNyxPascalMacroDefinition;
  LProtectName: Boolean;
begin
  Check;

  if ADepth > FDepthLimit then
  begin
    raise ENyxPascalMacroLimit.Create('Replacement nesting exceeds its budget');
  end;

  if NyxUTF8ByteCount(AText) > FByteLimit - FBytes then
  begin
    raise ENyxPascalMacroLimit.Create('Replacement work exceeds its byte budget');
  end;
  Inc(FBytes, NyxUTF8ByteCount(AText));
  LView := NewNyxPascalLexicalSnapshot(AText, FOptions, FCancellation);

  if LView.DiagnosticCount > 0 then
  begin
    raise ENyxPascalMacro.Create('A replacement has incomplete or invalid lexical content');
  end;
  LViewIndex := Length(FViews);
  SetLength(FViews, LViewIndex + 1);
  FViews[LViewIndex] := LView;
  LProtectName := False;
  for LAt := 0 to LView.Count - 2 do
  begin
    Check;
    LToken := LView.Token(LAt);

    if LToken.Kind = ptkDirective then
    begin
      raise ENyxPascalMacro.Create('Replacement directives need composition with the owning reader');
    end;

    if (FUse = pmuCondition) and (LToken.Kind = ptkWord) and
      (NyxPascalASCIIKey(LView.Identifier(LAt)) = TNyxText('defined')) then
    begin
      LProtectName := True;
      Append(LViewIndex, LAt, AParent);
      Continue;
    end;

    if LToken.Kind = ptkWord then
    begin

      if LProtectName then
      begin
        LProtectName := False;
      end
      else if not LToken.Escaped and (LToken.Word = pkwNone) then
      begin
        try
          LName := NyxPascalDefine(LView.Identifier(LAt));
        except
          on ENyxPascalContext do
          begin
            Append(LViewIndex, LAt, AParent);
            Continue;
          end;
        end;

        if FEnvironment.Has(LName) then
        begin
          LDefinition := FEnvironment.Definition(LName);

          if not LDefinition.Known then
          begin
            raise ENyxPascalMacro.Create('A conditionally modified replacement is unknown');
          end;
          LAncestor := AParent;
          while LAncestor >= 0 do
          begin

            if NyxPascalASCIIKey(FTraces[LAncestor].Definition.Name.Name) =
              NyxPascalASCIIKey(LName.Name) then
            begin
              raise ENyxPascalMacroCycle.Create('A replacement recursively references its own ancestry');
            end;
            LAncestor := FTraces[LAncestor].Parent;
          end;

          if Length(FTraces) >= FTokenLimit then
          begin
            raise ENyxPascalMacroLimit.Create('Replacement steps exceed their budget');
          end;
          LTrace := Length(FTraces);
          SetLength(FTraces, LTrace + 1);
          FTraces[LTrace].Definition := LDefinition;
          FTraces[LTrace].Parent := AParent;
          Visit(LDefinition.Replacement, LTrace, ADepth + 1);
          Continue;
        end;
      end;
    end;
    Append(LViewIndex, LAt, AParent);
  end;
end;

procedure TExpansion.Build(const AEnvironment: INyxPascalMacroEnvironment;
  const AText: TNyxText; AUse: TNyxPascalMacroUse;
  const ALexical: TNyxPascalLexicalOptions; ABytes, ATokens, ADepth: Integer;
  const ACancellation: INyxCancellationScope);
begin

  if (ABytes < 1) or (ATokens < 1) or (ADepth < 1) then
  begin
    raise ENyxPascalMacroLimit.Create('Replacement expansion needs positive remaining budgets');
  end;
  FEnvironment := AEnvironment;
  FCancellation := ACancellation;
  FUse := AUse;
  FOptions := ALexical;
  FByteLimit := ABytes;
  FTokenLimit := ATokens;
  FDepthLimit := ADepth;
  try
    Visit(AText, -1, 0);
    SetLength(FTokens, FCount);
    VerifyJoined;
    Check;
  finally
    FEnvironment := nil;
    FCancellation := nil;
  end;
end;

procedure TExpansion.VerifyJoined;
var
  LText: TNyxText;
  LView: INyxPascalLexicalSnapshot;
  LAt: Integer;
begin
  LText := Joined;

  if NyxUTF8ByteCount(LText) > FByteLimit - FBytes then
  begin
    raise ENyxPascalMacroLimit.Create('Replacement tokenization exceeds its byte budget');
  end;
  Inc(FBytes, NyxUTF8ByteCount(LText));
  LView := NewNyxPascalLexicalSnapshot(LText, FOptions, FCancellation);

  if (LView.DiagnosticCount > 0) or (LView.Count - 1 <> FCount) then
  begin
    raise ENyxPascalMacro.Create('Replacement boundaries need textual reader composition');
  end;
  for LAt := 0 to FCount - 1 do
  begin
    Check;

    if (LView.Token(LAt).Kind <> Token(LAt).Kind) or (LView.Text(LAt) <> Text(LAt)) then
    begin
      raise ENyxPascalMacro.Create('Replacement boundaries need textual reader composition');
    end;
  end;
end;

function TExpansion.Entry(AIndex: Integer): TExpandedToken;
begin

  if (AIndex < 0) or (AIndex >= FCount) then
  begin
    raise ENyxPascalMacro.Create('Expanded token is outside its snapshot');
  end;
  Result := FTokens[AIndex];
end;

function TExpansion.Count: Integer;
begin
  Result := FCount;
end;

function TExpansion.Token(AIndex: Integer): TNyxPascalToken;
var
  LEntry: TExpandedToken;
begin
  LEntry := Entry(AIndex);
  Result := FViews[LEntry.View].Token(LEntry.Token);
end;

function TExpansion.PreparedToken(AIndex: Integer): TNyxPascalPreparedToken;
var
  LEntry: TExpandedToken;
begin
  LEntry := Entry(AIndex);
  Result := NyxPascalPreparedToken(FViews[LEntry.View], LEntry.Token);
end;

function NyxPascalPreparedMacroToken(const AExpansion: INyxPascalMacroExpansion;
  AIndex: Integer): TNyxPascalPreparedToken;
var
  LOwner: IPreparedExpansion;
begin
  Result := Default(TNyxPascalPreparedToken);

  if Supports(AExpansion, IPreparedExpansion, LOwner) then
  begin
    Result := LOwner.PreparedToken(AIndex);
  end;
end;

function TExpansion.Text(AIndex: Integer): TNyxText;
var
  LEntry: TExpandedToken;
begin
  LEntry := Entry(AIndex);
  Result := FViews[LEntry.View].Text(LEntry.Token);
end;

function TExpansion.TokenTrace(AIndex: Integer): Integer;
begin
  Result := Entry(AIndex).Trace;
end;

function TExpansion.TraceCount: Integer;
begin
  Result := Length(FTraces);
end;

function TExpansion.Trace(AIndex: Integer): TNyxPascalMacroTrace;
begin

  if (AIndex < 0) or (AIndex >= Length(FTraces)) then
  begin
    raise ENyxPascalMacro.Create('Replacement trace is outside its snapshot');
  end;
  Result := FTraces[AIndex];
end;

function TExpansion.WorkBytes: Integer;
begin
  Result := FBytes;
end;

function TExpansion.Joined: TNyxText;
var
  LParts: TNyxStrings;
  LAt: Integer;
begin
  LParts := TNyxStrings.Create;
  try
    for LAt := 0 to FCount - 1 do
    begin
      LParts.Add(Text(LAt));
    end;
    Result := LParts.Join;
  finally
    LParts.Free;
  end;
end;

function TEnvironment.Expand(const AText: TNyxText; AUse: TNyxPascalMacroUse;
  const ALexical: TNyxPascalLexicalOptions; ABytes, ATokens, ADepth: Integer;
  const ACancellation: INyxCancellationScope): INyxPascalMacroExpansion;
var
  LExpansion: TExpansion;
  LOwner: INyxPascalMacroExpansion;
begin
  LExpansion := TExpansion.Create;
  LOwner := LExpansion;
  LExpansion.Build(Self, AText, AUse, ALexical, ABytes, ATokens, ADepth, ACancellation);
  Result := LOwner;
end;

function NewNyxPascalMacroEnvironment: INyxPascalMacroEnvironment;
begin
  Result := TEnvironment.Create;
end;

end.
