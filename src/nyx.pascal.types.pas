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
unit nyx.pascal.types;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.scheduler, nyx.pascal.lexing, nyx.pascal.syntax;

type
  { Quotas refuse the complete type candidate; ordinary incomplete source
    instead produces a detached recoverable syntax tree and diagnostics. }
  ENyxPascalTypeLimit = class(ENyxPascalLexicalLimit);
  { Declared type construction, never resolved compiler identity. Bounds/defaults
    retain exact syntax for later constant binding rather than numeric conversion. }
  TNyxPascalTypeKind = (ptxMissing, ptxName, ptxQualified, ptxGeneric, ptxPointer,
    ptxArray, ptxSet, ptxFile, ptxString, ptxClassOf, ptxProcedure, ptxFunction,
    ptxReference, ptxDistinct, ptxParameterGroup, ptxParameterName, ptxUntyped,
    ptxBound, ptxSubrange, ptxEnumeration, ptxEnumMember, ptxUnsupported);
  { Typed child intent. Ordinals and links are local to one immutable arena;
    they confer neither declaration visibility nor an editing capability. }
  TNyxPascalTypeRole = (ptrRoot, ptrBase, ptrMember, ptrArgument, ptrPointee,
    ptrElement, ptrDimension, ptrLength, ptrTarget, ptrParameter, ptrParameterName,
    ptrParameterType, ptrResult, ptrDefault, ptrLowerBound, ptrUpperBound, ptrEnumMember);
  { Distinct procedural ABI spellings; Default means no convention is authored.
    Platform/compiler eligibility remains part of later semantic qualification. }
  TNyxPascalCallingConvention = (pccDefault, pccRegister, pccCdecl, pccStdcall,
    pccPascal, pccSafecall, pccSyscall, pccVectorcall);
  { Authored packing and procedural qualifiers, independent of inferred defaults. }
  TNyxPascalTypeFlag = (ptfPacked, ptfBitPacked, ptfOfObject, ptfNested, ptfVarargs,
    ptfInline, ptfSpecialize);
  { Value-owned flags. No interfaces/arrays occur in published records. }
  TNyxPascalTypeFlags = set of TNyxPascalTypeFlag;
  { Exact syntax node with parent/child/sibling ordinals (-1 absent). Grouped
    parameter names share their group's type child; tree ownership stays in its
    arena. Span counts scalars in Lexical, which may be an analysis projection. }
  TNyxPascalTypeNode = record
  private
    FKind: TNyxPascalTypeKind;
    FRole: TNyxPascalTypeRole;
    FName: TNyxText;
    FSpan: TNyxPascalSpan;
    FParent: Integer;
    FFirst: Integer;
    FLast: Integer;
    FNext: Integer;
    FMode: TNyxPascalSyntaxParameterMode;
    FConvention: TNyxPascalCallingConvention;
    FFlags: TNyxPascalTypeFlags;
  public
    property Kind: TNyxPascalTypeKind read FKind;
    property Role: TNyxPascalTypeRole read FRole;
    property Name: TNyxText read FName;
    property Span: TNyxPascalSpan read FSpan;
    property Parent: Integer read FParent;
    property FirstChild: Integer read FFirst;
    property NextSibling: Integer read FNext;
    property ParameterMode: TNyxPascalSyntaxParameterMode read FMode;
    property CallingConvention: TNyxPascalCallingConvention read FConvention;
    property Flags: TNyxPascalTypeFlags read FFlags;
  end;
  { Recoverable type failures are distinct from lexical diagnostics and compiler
    admission. A directive needs captured translation, never guessed branch state. }
  TNyxPascalTypeIssue = (ptiExpectedType, ptiExpectedName, ptiExpectedDelimiter,
    ptiUnsupportedType, ptiTrailingSyntax, ptiContextRequired);
  { Detached scalar diagnostic, with stable English guidance and no source borrow. }
  TNyxPascalTypeDiagnostic = record
  private
    FIssue: TNyxPascalTypeIssue;
    FSpan: TNyxPascalSpan;
    FMessage: TNyxText;
  public
    property Issue: TNyxPascalTypeIssue read FIssue;
    property Span: TNyxPascalSpan read FSpan;
    property MessageText: TNyxText read FMessage;
  end;
  { Independent positive node/depth/diagnostic budgets. A zeroed record refuses;
    fluent values retain no interfaces and never modify their baseline. }
  TNyxPascalTypeOptions = record
  private
    FDefined: Boolean;
    FNodes: Integer;
    FDepth: Integer;
    FDiagnostics: Integer;
  public
    { Defaults: 4096 nodes, 128 nested type/group levels, 128 diagnostics. }
    class function Defaults: TNyxPascalTypeOptions; static;
    function UpToNodes(AValue: Integer): TNyxPascalTypeOptions;
    function UpToDepth(AValue: Integer): TNyxPascalTypeOptions;
    function UpToDiagnostics(AValue: Integer): TNyxPascalTypeOptions;
    procedure Validate;
    property MaximumNodes: Integer read FNodes;
    property MaximumDepth: Integer read FDepth;
    property MaximumDiagnostics: Integer read FDiagnostics;
  end;
  { Owns immutable syntax over one exact lexical window. Complete recognizes the
    supported type grammar only; opaque constant expressions are not evaluated,
    names/types are not bound, and no compilation/execution authority is granted.
    Returned records/checked ordinals borrow no editor, file, mutable provider or UI. }
  INyxPascalTypeSyntax = interface(IInterface)
    ['{98349E7C-C0ED-48DE-A518-427A04570D01}']
    function Lexical: INyxPascalLexicalSnapshot;
    function Window: TNyxPascalSpan;
    function Root: Integer;
    function Complete: Boolean;
    function Count: Integer;
    function Node(AIndex: Integer): TNyxPascalTypeNode;
    { First/ordinal child of this role, -1 absent; no inherited/name lookup occurs. }
    function Child(AParent: Integer; ARole: TNyxPascalTypeRole;
      AOrdinal: Integer = 0): Integer;
    function DiagnosticCount: Integer;
    function Diagnostic(AIndex: Integer): TNyxPascalTypeDiagnostic;
  end;

{ Parse an exact scalar window without copying/relexing its text. Bounds cannot
  cut a significant token. Nested generic closers split >> and >= contextually;
  the original lexical tokens/text remain exact. Cancellation is borrowed only
  during construction and quotas never publish a truncated candidate. }
function NewNyxPascalTypeSyntax(const ALexical: INyxPascalLexicalSnapshot;
  const AWindow: TNyxPascalSpan; const AOptions: TNyxPascalTypeOptions;
  const ACancellation: INyxCancellationScope = nil): INyxPascalTypeSyntax;

implementation

type
  { Builder owns an immutable lexical snapshot, local ordinal vectors and a
    temporary cancellation lease. Partial compound symbols are cursor state only;
    no source token is mutated or shared with another parser. }
  TTypeSyntax = class(TInterfacedObject, INyxPascalTypeSyntax)
  private
    FLexical: INyxPascalLexicalSnapshot;
    FWindow: TNyxPascalSpan;
    FOptions: TNyxPascalTypeOptions;
    FTokens: array of Integer;
    FNodes: array of TNyxPascalTypeNode;
    FDiagnostics: array of TNyxPascalTypeDiagnostic;
    FCount: Integer;
    FDiagnosticCount: Integer;
    FRoot: Integer;
    FAt: Integer;
    FSuffix: Boolean;
    FLastOffset: Integer;
    FDepth: Integer;
    FCancellation: INyxCancellationScope;
    procedure CheckCancellation;
    function AtEnd: Boolean;
    function Tok: TNyxPascalToken;
    function Sym(AValue: TNyxPascalSymbol): Boolean;
    function Word(AValue: TNyxPascalWord): Boolean;
    function NameToken: Boolean;
    function Key: TNyxText;
    function Offset: Integer;
    procedure Step;
    function CloseGeneric: Boolean;
    function Add(AKind: TNyxPascalTypeKind; AStart: Integer): Integer;
    procedure Finish(AIndex: Integer);
    procedure Link(AParent, AChild: Integer; ARole: TNyxPascalTypeRole);
    procedure Problem(AIssue: TNyxPascalTypeIssue; const AMessage: TNyxText);
    procedure Expect(AValue: TNyxPascalSymbol; const AMessage: TNyxText);
    function ParseType: Integer;
    function ParseName: Integer;
    function ParseRoutine: Integer;
    procedure Parameters(AParent: Integer);
    function Bound(AStop: TNyxPascalSymbol; AComma, ARange: Boolean): Integer;
    function Convention(out AConvention: TNyxPascalCallingConvention): Boolean;
    procedure Build;
  public
    constructor Create(const ALexical: INyxPascalLexicalSnapshot;
      const AWindow: TNyxPascalSpan; const AOptions: TNyxPascalTypeOptions;
      const ACancellation: INyxCancellationScope);
    function Lexical: INyxPascalLexicalSnapshot;
    function Window: TNyxPascalSpan;
    function Root: Integer;
    function Complete: Boolean;
    function Count: Integer;
    function Node(AIndex: Integer): TNyxPascalTypeNode;
    function Child(AParent: Integer; ARole: TNyxPascalTypeRole;
      AOrdinal: Integer): Integer;
    function DiagnosticCount: Integer;
    function Diagnostic(AIndex: Integer): TNyxPascalTypeDiagnostic;
  end;

function Capacity(ACount, AMaximum: Integer): Integer;
begin
  Result := AMaximum;

  if 16 + ACount div 2 <= AMaximum - ACount then
  begin
    Result := ACount + 16 + ACount div 2;
  end;
end;

class function TNyxPascalTypeOptions.Defaults: TNyxPascalTypeOptions;
begin
  Result := Default(TNyxPascalTypeOptions);
  Result.FDefined := True;
  Result.FNodes := 4096;
  Result.FDepth := 128;
  Result.FDiagnostics := 128;
end;

procedure TNyxPascalTypeOptions.Validate;
begin

  if not FDefined or (FNodes < 1) or (FDepth < 1) or (FDiagnostics < 1) then
  begin
    raise ENyxPascalLexical.Create('Use explicit positive Pascal type syntax budgets');
  end;
end;

function TNyxPascalTypeOptions.UpToNodes(AValue: Integer): TNyxPascalTypeOptions;
begin
  Result := Self;
  Result.FNodes := AValue;
  Result.Validate;
end;

function TNyxPascalTypeOptions.UpToDepth(AValue: Integer): TNyxPascalTypeOptions;
begin
  Result := Self;
  Result.FDepth := AValue;
  Result.Validate;
end;

function TNyxPascalTypeOptions.UpToDiagnostics(AValue: Integer): TNyxPascalTypeOptions;
begin
  Result := Self;
  Result.FDiagnostics := AValue;
  Result.Validate;
end;

constructor TTypeSyntax.Create(const ALexical: INyxPascalLexicalSnapshot;
  const AWindow: TNyxPascalSpan; const AOptions: TNyxPascalTypeOptions;
  const ACancellation: INyxCancellationScope);
begin
  inherited Create;
  AOptions.Validate;

  if (ALexical = nil) or (AWindow.Finish > ALexical.Scalars) then
  begin
    raise ENyxPascalLexical.Create('A type window needs an exact lexical source range');
  end;
  FLexical := ALexical;
  FWindow := AWindow;
  FOptions := AOptions;
  FCancellation := ACancellation;
  try
    CheckCancellation;
    Build;
  finally
    FCancellation := nil;
  end;
end;

procedure TTypeSyntax.CheckCancellation;
begin

  if (FCancellation <> nil) and FCancellation.Cancelled then
  begin
    raise ENyxPascalAnalysisCancelled.Create('Pascal type analysis cancelled');
  end;
end;

function TTypeSyntax.AtEnd: Boolean;
begin
  Result := FAt >= Length(FTokens);
end;

function TTypeSyntax.Tok: TNyxPascalToken;
begin
  Result := FLexical.Token(FLexical.Count - 1);

  if not AtEnd then
  begin
    Result := FLexical.Token(FTokens[FAt]);
  end;
end;

function TTypeSyntax.Offset: Integer;
begin
  Result := FWindow.Finish;

  if not AtEnd then
  begin
    Result := Tok.Span.Start;

    if FSuffix then
    begin
      Inc(Result);
    end;
  end;
end;

function TTypeSyntax.Sym(AValue: TNyxPascalSymbol): Boolean;
begin
  Result := False;

  if AtEnd or (Tok.Kind <> ptkSymbol) then
  begin
    Exit;
  end;

  if not FSuffix then
  begin
    Result := Tok.Symbol = AValue;
  end
  else
  begin
    Result := ((Tok.Symbol = psoShiftRight) and (AValue = psoGreater)) or
      ((Tok.Symbol = psoGreaterEqual) and (AValue = psoEqual));
  end;
end;

function TTypeSyntax.Word(AValue: TNyxPascalWord): Boolean;
begin
  Result := not AtEnd and (Tok.Kind = ptkWord) and (Tok.Word = AValue);
end;

function TTypeSyntax.NameToken: Boolean;
begin
  Result := not AtEnd and (Tok.Kind = ptkWord) and
    ((Tok.Word = pkwNone) or Tok.Escaped);
end;

function TTypeSyntax.Key: TNyxText;
begin
  Result := '';

  if not AtEnd and (Tok.Kind = ptkWord) and not Tok.Escaped then
  begin
    Result := NyxPascalASCIIKey(FLexical.Identifier(FTokens[FAt]));
  end;
end;

procedure TTypeSyntax.Step;
begin
  CheckCancellation;

  if not AtEnd then
  begin
    FLastOffset := Tok.Span.Finish;
    Inc(FAt);
    FSuffix := False;
  end;
end;

function TTypeSyntax.CloseGeneric: Boolean;
begin
  Result := True;

  if Sym(psoGreater) then
  begin
    Step;
  end
  else if not FSuffix and (Sym(psoShiftRight) or Sym(psoGreaterEqual)) then
  begin
    FSuffix := True;
    FLastOffset := Tok.Span.Start + 1;
  end
  else
  begin
    Result := False;
    Problem(ptiExpectedDelimiter, 'Finish this generic argument list with >');
  end;
end;

function TTypeSyntax.Add(AKind: TNyxPascalTypeKind; AStart: Integer): Integer;
begin
  Result := FCount;

  if Result >= FOptions.MaximumNodes then
  begin
    raise ENyxPascalTypeLimit.Create('Pascal type nodes exceed their budget');
  end;

  if Result = Length(FNodes) then
  begin
    SetLength(FNodes, Capacity(Result, FOptions.MaximumNodes));
  end;
  FNodes[Result].FKind := AKind;
  FNodes[Result].FSpan := NyxPascalSpan(AStart, 0);
  FNodes[Result].FParent := -1;
  FNodes[Result].FFirst := -1;
  FNodes[Result].FLast := -1;
  FNodes[Result].FNext := -1;
  Inc(FCount);
end;

procedure TTypeSyntax.Finish(AIndex: Integer);
var
  LEnd: Integer;
begin
  LEnd := FLastOffset;

  if LEnd < FNodes[AIndex].Span.Start then
  begin
    LEnd := FNodes[AIndex].Span.Start;
  end;
  FNodes[AIndex].FSpan := NyxPascalSpan(FNodes[AIndex].Span.Start,
    LEnd - FNodes[AIndex].Span.Start);
end;

procedure TTypeSyntax.Link(AParent, AChild: Integer; ARole: TNyxPascalTypeRole);
begin

  if FNodes[AChild].Parent <> -1 then
  begin
    raise ENyxPascalLexical.Create('A type syntax node cannot acquire two parents');
  end;
  FNodes[AChild].FParent := AParent;
  FNodes[AChild].FRole := ARole;

  if FNodes[AParent].FFirst < 0 then
  begin
    FNodes[AParent].FFirst := AChild;
  end
  else
  begin
    FNodes[FNodes[AParent].FLast].FNext := AChild;
  end;
  FNodes[AParent].FLast := AChild;
end;

procedure TTypeSyntax.Problem(AIssue: TNyxPascalTypeIssue; const AMessage: TNyxText);
begin

  if FDiagnosticCount >= FOptions.MaximumDiagnostics then
  begin
    raise ENyxPascalTypeLimit.Create('Pascal type diagnostics exceed their budget');
  end;

  if FDiagnosticCount = Length(FDiagnostics) then
  begin
    SetLength(FDiagnostics, Capacity(FDiagnosticCount, FOptions.MaximumDiagnostics));
  end;
  FDiagnostics[FDiagnosticCount].FIssue := AIssue;
  FDiagnostics[FDiagnosticCount].FSpan := NyxPascalSpan(Offset, 0);
  FDiagnostics[FDiagnosticCount].FMessage := AMessage;
  Inc(FDiagnosticCount);
end;

procedure TTypeSyntax.Expect(AValue: TNyxPascalSymbol; const AMessage: TNyxText);
begin

  if Sym(AValue) then
  begin
    Step;
  end
  else
  begin
    Problem(ptiExpectedDelimiter, AMessage);
  end;
end;

function TTypeSyntax.ParseName: Integer;
var
  LPart: Integer;
  LCombined: Integer;
  LGeneric: Integer;
  LBefore: Integer;
  LCanSpecialize: Boolean;
begin
  Result := Add(ptxName, Offset);

  if not NameToken and not Word(pkwString) then
  begin
    FNodes[Result].FKind := ptxMissing;
    Problem(ptiExpectedName, 'Add a Pascal type name');
    Exit;
  end;
  FNodes[Result].FName := FLexical.Identifier(FTokens[FAt]);
  Step;
  Finish(Result);
  LCanSpecialize := True;
  while True do
  begin

    if Sym(psoLess) then
    begin

      if not LCanSpecialize then
      begin
        Problem(ptiUnsupportedType, 'A type segment cannot acquire two argument lists');
        Break;
      end;
      LGeneric := Add(ptxGeneric, FNodes[Result].Span.Start);
      Link(LGeneric, Result, ptrBase);
      Step;

      if Sym(psoGreater) or Sym(psoShiftRight) or AtEnd then
      begin
        Problem(ptiExpectedType, 'A generic argument list needs a type');
      end
      else
      begin
        repeat
          LBefore := FAt;
          LPart := ParseType;
          Link(LGeneric, LPart, ptrArgument);

          if not Sym(psoComma) then
          begin
            Break;
          end;
          Step;

          if (FAt = LBefore) or AtEnd then
          begin
            Problem(ptiExpectedType, 'Add the next generic argument type');
            Break;
          end;
        until False;
      end;
      CloseGeneric;
      Finish(LGeneric);
      Result := LGeneric;
      LCanSpecialize := False;
    end
    else if Sym(psoDot) then
    begin
      LCombined := Add(ptxQualified, FNodes[Result].Span.Start);
      Link(LCombined, Result, ptrBase);
      Step;
      LPart := Add(ptxName, Offset);

      if not NameToken then
      begin
        FNodes[LPart].FKind := ptxMissing;
        Problem(ptiExpectedName, 'A qualified type needs a member name');
        Link(LCombined, LPart, ptrMember);
        Finish(LCombined);
        Result := LCombined;
        Break;
      end;
      FNodes[LPart].FName := FLexical.Identifier(FTokens[FAt]);
      Step;
      Finish(LPart);
      Link(LCombined, LPart, ptrMember);
      Finish(LCombined);
      Result := LCombined;
      LCanSpecialize := True;
    end
    else
    begin
      Break;
    end;
  end;
end;

function TTypeSyntax.Bound(AStop: TNyxPascalSymbol; AComma, ARange: Boolean): Integer;
var
  LStack: array of TNyxPascalSymbol;
  LDepth: Integer;
begin
  Result := Add(ptxBound, Offset);
  LDepth := 0;
  while not AtEnd do
  begin

    if (LDepth = 0) and (Sym(AStop) or Sym(psoSemicolon) or Sym(psoCloseParen) or
      (AComma and Sym(psoComma)) or (ARange and Sym(psoRange)) or
      Sym(psoGreater) or Sym(psoShiftRight) or Sym(psoGreaterEqual)) then
    begin
      Break;
    end;

    if Sym(psoOpenParen) or Sym(psoOpenBracket) then
    begin

      if LDepth >= FOptions.MaximumDepth then
      begin
        raise ENyxPascalTypeLimit.Create('Pascal type bound grouping exceeds its budget');
      end;
      SetLength(LStack, LDepth + 1);
      LStack[LDepth] := psoCloseParen;

      if Sym(psoOpenBracket) then
      begin
        LStack[LDepth] := psoCloseBracket;
      end;
      Inc(LDepth);
    end
    else if Sym(psoCloseParen) or Sym(psoCloseBracket) then
    begin

      if (LDepth = 0) or not Sym(LStack[LDepth - 1]) then
      begin
        Problem(ptiExpectedDelimiter, 'This bound has an unmatched delimiter');
        Break;
      end;
      Dec(LDepth);
    end;
    Step;
  end;

  if (FLastOffset < FNodes[Result].Span.Start) or
    (FLastOffset = FNodes[Result].Span.Start) then
  begin
    Problem(ptiExpectedType, 'Add an ordinal bound or constant expression');
  end;

  if LDepth <> 0 then
  begin
    Problem(ptiExpectedDelimiter, 'Finish this bound expression group');
  end;
  Finish(Result);
end;

function TTypeSyntax.Convention(out AConvention: TNyxPascalCallingConvention): Boolean;
const
  CNames: array[TNyxPascalCallingConvention] of TNyxText =
    ('', 'register', 'cdecl', 'stdcall', 'pascal', 'safecall', 'syscall', 'vectorcall');
var
  LValue: TNyxPascalCallingConvention;
begin
  for LValue := Succ(Low(TNyxPascalCallingConvention)) to High(TNyxPascalCallingConvention) do
  begin

    if Key = CNames[LValue] then
    begin
      AConvention := LValue;
      Exit(True);
    end;
  end;
  AConvention := pccDefault;
  Result := False;
end;

procedure TTypeSyntax.Parameters(AParent: Integer);
var
  LGroup: Integer;
  LName: Integer;
  LType: Integer;
  LBefore: Integer;
  LMode: TNyxPascalSyntaxParameterMode;
begin
  Step;
  while not AtEnd and not Sym(psoCloseParen) do
  begin
    LBefore := FAt;
    LMode := pspmValue;
    LGroup := Add(ptxParameterGroup, Offset);
    case Tok.Word of
      pkwConst:
        begin
          LMode := pspmConst;
          Step;
        end;
      pkwConstref:
        begin
          LMode := pspmConstRef;
          Step;
        end;
      pkwVar:
        begin
          LMode := pspmVar;
          Step;
        end;
      pkwOut:
        begin
          LMode := pspmOut;
          Step;
        end;
    else
      begin
        { An ordinary value parameter has no prefix. }
      end;
    end;
    FNodes[LGroup].FMode := LMode;
    repeat
      LName := Add(ptxParameterName, Offset);

      if not NameToken then
      begin
        Problem(ptiExpectedName, 'A parameter group needs a name');
        Link(LGroup, LName, ptrParameterName);
        Break;
      end;
      FNodes[LName].FName := FLexical.Identifier(FTokens[FAt]);
      Step;
      Finish(LName);
      Link(LGroup, LName, ptrParameterName);

      if not Sym(psoComma) then
      begin
        Break;
      end;
      Step;
    until AtEnd;

    if Sym(psoColon) then
    begin
      Step;
      LType := ParseType;
    end
    else if LMode in [pspmConst, pspmVar, pspmOut] then
    begin
      LType := Add(ptxUntyped, Offset);
    end
    else
    begin
      LType := Add(ptxMissing, Offset);
      Problem(ptiExpectedType, 'A value parameter needs its declared type');
    end;
    Link(LGroup, LType, ptrParameterType);

    if Sym(psoEqual) then
    begin
      Step;
      Link(LGroup, Bound(psoCloseParen, False, False), ptrDefault);
    end;
    Finish(LGroup);
    Link(AParent, LGroup, ptrParameter);

    if not Sym(psoSemicolon) then
    begin
      Break;
    end;
    Step;

    if FAt = LBefore then
    begin
      Break;
    end;
  end;
  Expect(psoCloseParen, 'Finish this procedural parameter list with )');
end;

function TTypeSyntax.ParseRoutine: Integer;
var
  LFunction: Boolean;
  LConvention: TNyxPascalCallingConvention;
begin
  LFunction := Word(pkwFunction);
  Result := Add(ptxProcedure, Offset);

  if LFunction then
  begin
    FNodes[Result].FKind := ptxFunction;
  end;
  Step;

  if Sym(psoOpenParen) then
  begin
    Parameters(Result);
  end;

  if LFunction then
  begin
    Expect(psoColon, 'A function type needs a result colon');
    Link(Result, ParseType, ptrResult);
  end;
  while not AtEnd do
  begin

    if Word(pkwOf) then
    begin
      Step;

      if not Word(pkwObject) then
      begin
        Problem(ptiExpectedType, 'A method pointer needs of object');
        Break;
      end;
      Include(FNodes[Result].FFlags, ptfOfObject);
      Step;
    end
    else if Word(pkwIs) then
    begin
      Step;

      if Key <> 'nested' then
      begin
        Problem(ptiUnsupportedType, 'This procedural qualifier needs richer analysis');
        Break;
      end;
      Include(FNodes[Result].FFlags, ptfNested);
      Step;
    end
    else if Convention(LConvention) then
    begin

      if FNodes[Result].FConvention <> pccDefault then
      begin
        Problem(ptiUnsupportedType, 'A procedural type has conflicting calling conventions');
      end;
      FNodes[Result].FConvention := LConvention;
      Step;
    end
    else if Word(pkwVarargs) or Word(pkwInline) then
    begin

      if Word(pkwVarargs) then
      begin
        Include(FNodes[Result].FFlags, ptfVarargs);
      end
      else
      begin
        Include(FNodes[Result].FFlags, ptfInline);
      end;
      Step;
    end
    else if Sym(psoSemicolon) then
    begin
      { A convention follows its signature separator, not an arbitrary following
        declaration. The builder's bounded window remains the owning boundary. }
      Inc(FAt);

      if not AtEnd and (Convention(LConvention) or Word(pkwVarargs) or Word(pkwInline)) then
      begin
        FLastOffset := FLexical.Token(FTokens[FAt - 1]).Span.Finish;
      end
      else
      begin
        Dec(FAt);
        Break;
      end;
    end
    else
    begin
      Break;
    end;
  end;
  Finish(Result);
end;

function TTypeSyntax.ParseType: Integer;
var
  LStart: Integer;
  LChild: Integer;
  LBase: Integer;
  LMember: Integer;
  LPacking: TNyxPascalTypeFlags;
  LSpecialize: Boolean;
begin
  CheckCancellation;

  if FDepth >= FOptions.MaximumDepth then
  begin
    raise ENyxPascalTypeLimit.Create('Pascal type nesting exceeds its budget');
  end;
  Inc(FDepth);
  try
    LStart := Offset;
    LPacking := [];

    if Word(pkwPacked) or (Key = 'bitpacked') then
    begin
      Include(LPacking, ptfPacked);

      if Key = 'bitpacked' then
      begin
        Include(LPacking, ptfBitPacked);
      end;
      Step;
    end;

    if Word(pkwType) or Word(pkwReference) then
    begin
      Result := Add(ptxDistinct, LStart);

      if Word(pkwReference) then
      begin
        FNodes[Result].FKind := ptxReference;
        Step;

        if not Word(pkwTo) then
        begin
          Problem(ptiExpectedType, 'A function reference needs reference to');
        end
        else
        begin
          Step;
        end;

        if not Word(pkwFunction) and not Word(pkwProcedure) then
        begin
          Problem(ptiExpectedType, 'A function reference needs a procedural signature');
        end;
      end
      else
      begin
        Step;
      end;
      Link(Result, ParseType, ptrTarget);
    end
    else if Sym(psoCaret) then
    begin
      Result := Add(ptxPointer, LStart);
      Step;
      Link(Result, ParseType, ptrPointee);
    end
    else if Word(pkwClass) then
    begin
      Result := Add(ptxClassOf, LStart);
      Step;

      if not Word(pkwOf) then
      begin
        FNodes[Result].FKind := ptxUnsupported;
        Problem(ptiUnsupportedType, 'Structured class bodies use their declared member scope');
      end
      else
      begin
        Step;
        Link(Result, ParseType, ptrTarget);
      end;
    end
    else if Word(pkwArray) or Word(pkwSet) or Word(pkwFile) then
    begin
      Result := Add(ptxArray, LStart);

      if Word(pkwSet) then
      begin
        FNodes[Result].FKind := ptxSet;
      end
      else if Word(pkwFile) then
      begin
        FNodes[Result].FKind := ptxFile;
      end;
      Step;

      if Sym(psoOpenBracket) then
      begin

        if FNodes[Result].Kind <> ptxArray then
        begin
          Problem(ptiUnsupportedType, 'Only array types have dimension brackets');
        end;
        Step;
        repeat
          Link(Result, Bound(psoCloseBracket, True, False), ptrDimension);

          if not Sym(psoComma) then
          begin
            Break;
          end;
          Step;
        until AtEnd;
        Expect(psoCloseBracket, 'Finish the array dimensions with ]');
      end;

      if Word(pkwOf) then
      begin
        Step;

        if Word(pkwConst) and (FNodes[Result].Kind = ptxArray) then
        begin
          LChild := Add(ptxUntyped, Offset);
          FNodes[LChild].FName := 'const';
          Step;
          Finish(LChild);
          Link(Result, LChild, ptrElement);
        end
        else
        begin
          Link(Result, ParseType, ptrElement);
        end;
      end
      else if FNodes[Result].Kind <> ptxFile then
      begin
        Problem(ptiExpectedType, 'An array or set needs its element type after of');
      end;
    end
    else if Word(pkwProcedure) or Word(pkwFunction) then
    begin
      Result := ParseRoutine;
    end
    else if Word(pkwString) then
    begin
      Result := Add(ptxString, LStart);
      FNodes[Result].FName := FLexical.Identifier(FTokens[FAt]);
      Step;

      if Sym(psoOpenBracket) then
      begin
        Step;
        Link(Result, Bound(psoCloseBracket, False, False), ptrLength);
        Expect(psoCloseBracket, 'Finish the short-string length with ]');
      end;
    end
    else if Sym(psoOpenParen) then
    begin
      Result := Add(ptxEnumeration, LStart);
      Step;
      repeat
        LMember := Add(ptxEnumMember, Offset);

        if not NameToken then
        begin
          Problem(ptiExpectedName, 'An enumeration needs a member name');
          Link(Result, LMember, ptrEnumMember);
          Break;
        end;
        FNodes[LMember].FName := FLexical.Identifier(FTokens[FAt]);
        Step;

        if Sym(psoAssign) or Sym(psoEqual) then
        begin
          Step;
          Link(LMember, Bound(psoCloseParen, True, False), ptrDefault);
        end;
        Finish(LMember);
        Link(Result, LMember, ptrEnumMember);

        if not Sym(psoComma) then
        begin
          Break;
        end;
        Step;
      until AtEnd;
      Expect(psoCloseParen, 'Finish this enumeration with )');
    end
    else if Word(pkwSpecialize) or NameToken then
    begin
      LSpecialize := Word(pkwSpecialize);

      if LSpecialize then
      begin
        Step;
      end;
      Result := ParseName;

      if LSpecialize then
      begin
        FNodes[Result].FSpan := NyxPascalSpan(LStart, FLastOffset - LStart);
        Include(FNodes[Result].FFlags, ptfSpecialize);
      end;

      if Sym(psoRange) then
      begin
        LBase := Result;
        Result := Add(ptxSubrange, LStart);
        Link(Result, LBase, ptrLowerBound);
        Step;
        Link(Result, Bound(psoCloseBracket, True, False), ptrUpperBound);
      end;
    end
    else if not AtEnd and ((Tok.Kind in [ptkNumber, ptkString, ptkCharacter]) or
      Sym(psoPlus) or Sym(psoMinus)) then
    begin
      LBase := Bound(psoRange, True, True);
      Result := Add(ptxSubrange, LStart);
      Link(Result, LBase, ptrLowerBound);
      Expect(psoRange, 'An ordinal type expression needs a lower..upper range');
      Link(Result, Bound(psoCloseBracket, True, False), ptrUpperBound);
    end
    else
    begin
      Result := Add(ptxMissing, LStart);
      Problem(ptiExpectedType, 'Add a supported Pascal type expression');

      if not AtEnd and not Sym(psoComma) and not Sym(psoCloseParen) and
        not Sym(psoGreater) and not Sym(psoShiftRight) and not Sym(psoSemicolon) then
      begin
        Step;
      end;
    end;
    FNodes[Result].FFlags := FNodes[Result].FFlags + LPacking;
    Finish(Result);
  finally
    Dec(FDepth);
  end;
end;

procedure TTypeSyntax.Build;
var
  LAt: Integer;
  LCount: Integer;
  LToken: TNyxPascalToken;
begin
  LCount := 0;
  LAt := FLexical.TokenAt(FWindow.Start);
  { A small requested type window allocates only its own ordinal interval,
    rather than every subsequent token in a large owning unit. }
  SetLength(FTokens, FLexical.TokenAt(FWindow.Finish) - LAt + 1);
  while LAt < FLexical.Count do
  begin
    CheckCancellation;
    LToken := FLexical.Token(LAt);

    if (LToken.Kind = ptkEOF) or (LToken.Span.Start >= FWindow.Finish) then
    begin
      Break;
    end;

    if not (LToken.Kind in [ptkWhitespace, ptkComment]) or not LToken.Complete then
    begin

      if (LToken.Span.Start < FWindow.Start) or (LToken.Span.Finish > FWindow.Finish) then
      begin
        raise ENyxPascalLexical.Create('A type window cannot cut a significant token');
      end;
      FTokens[LCount] := LAt;
      Inc(LCount);
    end;
    Inc(LAt);
  end;
  SetLength(FTokens, LCount);
  FLastOffset := FWindow.Start;

  for LAt := 0 to High(FTokens) do
  begin

    if FLexical.Token(FTokens[LAt]).Kind = ptkDirective then
    begin
      FAt := LAt;
      Problem(ptiContextRequired, 'Translate conditional/include directives before type analysis');
    end;

    if not FLexical.Token(FTokens[LAt]).Complete or
      (FLexical.Token(FTokens[LAt]).Kind = ptkInvalid) then
    begin
      FAt := LAt;
      Problem(ptiUnsupportedType, 'This type contains an incomplete or invalid lexical token');
    end;
  end;
  FAt := 0;
  FRoot := ParseType;

  if Sym(psoSemicolon) then
  begin
    Step;
  end;

  if not AtEnd then
  begin
    Problem(ptiTrailingSyntax, 'This type has additional unclassified syntax');
  end;
end;

function TTypeSyntax.Lexical: INyxPascalLexicalSnapshot;
begin
  Result := FLexical;
end;

function TTypeSyntax.Window: TNyxPascalSpan;
begin
  Result := FWindow;
end;

function TTypeSyntax.Root: Integer;
begin
  Result := FRoot;
end;

function TTypeSyntax.Complete: Boolean;
begin
  Result := FDiagnosticCount = 0;
end;

function TTypeSyntax.Count: Integer;
begin
  Result := FCount;
end;

function TTypeSyntax.Node(AIndex: Integer): TNyxPascalTypeNode;
begin

  if (AIndex < 0) or (AIndex >= FCount) then
  begin
    raise ENyxPascalLexical.Create('Type node index is outside its immutable syntax');
  end;
  Result := FNodes[AIndex];
end;

function TTypeSyntax.Child(AParent: Integer; ARole: TNyxPascalTypeRole;
  AOrdinal: Integer): Integer;
begin

  if AOrdinal < 0 then
  begin
    raise ENyxPascalLexical.Create('A type child ordinal cannot be negative');
  end;
  Result := Node(AParent).FirstChild;
  while Result >= 0 do
  begin

    if FNodes[Result].Role = ARole then
    begin

      if AOrdinal = 0 then
      begin
        Exit;
      end;
      Dec(AOrdinal);
    end;
    Result := FNodes[Result].NextSibling;
  end;
end;

function TTypeSyntax.DiagnosticCount: Integer;
begin
  Result := FDiagnosticCount;
end;

function TTypeSyntax.Diagnostic(AIndex: Integer): TNyxPascalTypeDiagnostic;
begin

  if (AIndex < 0) or (AIndex >= FDiagnosticCount) then
  begin
    raise ENyxPascalLexical.Create('Type diagnostic index is outside its immutable syntax');
  end;
  Result := FDiagnostics[AIndex];
end;

function NewNyxPascalTypeSyntax(const ALexical: INyxPascalLexicalSnapshot;
  const AWindow: TNyxPascalSpan; const AOptions: TNyxPascalTypeOptions;
  const ACancellation: INyxCancellationScope): INyxPascalTypeSyntax;
begin
  Result := TTypeSyntax.Create(ALexical, AWindow, AOptions, ACancellation);
end;

end.
