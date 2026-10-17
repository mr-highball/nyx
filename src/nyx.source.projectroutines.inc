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

type
  { One copied physical chunk in expanded lexical order. Occurrence distinguishes
    repeated includes of the same leaf, preventing a false contiguous edit span. }
  TRoutineExpansionSpan = record
    FileReference: TNyxPascalSourceRef;
    LogicalStart: Integer;
    PhysicalStart: Integer;
    Count: Integer;
    Occurrence: Integer;
  end;

  { Sparse Unicode cursor, captured every 256 scalars. Physical lookups scan at
    most one block after binary search; routine count cannot multiply full-file
    UTF-8/UTF-16 rescans. Positions are private target string coordinates. }
  TRoutinePhysicalPoint = record
    Position: Integer;
    Offset: Integer;
    Line: Integer;
    Column: Integer;
  end;

  { Owned physical text and sparse cursors. No COM interface is stored inside
    this record, preserving managed-record behavior on both Pascal targets. }
  TRoutinePhysicalFile = record
    FileReference: TNyxPascalSourceRef;
    Text: TNyxText;
    Points: array of TRoutinePhysicalPoint;
  end;

  { Constructor owns its detached expansion scratch, then releases it after
    building the requested routine or declaration catalog. Returned entries
    borrow no source provider; no application, lexer, mutable session or transport
    is retained. }
  TPascalRoutineIndex = class(TInterfacedObject, INyxPascalRoutineIndex,
    INyxPascalSymbolIndex, INyxPascalRoutineReadStatus)
  private
    FOwner: TNyxPascalSourceRef;
    FEntries: array of TNyxPascalRoutineSite;
    FSymbols: array of TNyxPascalSymbolSite;
    FSources: INyxPascalSourceSnapshot;
    FProblemText: TNyxText;
    FProblemLocation: TNyxPascalRoutineLocation;
    FStopped: Boolean;
    FLimits: TNyxPascalRoutineLimits;
    FParts: TNyxStrings;
    FText: TNyxText;
    FSpans: array of TRoutineExpansionSpan;
    FPhysical: array of TRoutinePhysicalFile;
    FStack: array of TNyxPascalSourceRef;
    FOccurrences: Integer;
    FExpandedBytes: Integer;
    FLogicalLength: Integer;
    function PhysicalFile(const AFile: TNyxPascalSourceRef): Integer;
    function PhysicalLocation(const AFile: TNyxPascalSourceRef;
      APosition: Integer): TNyxPascalRoutineLocation;
    function SpanAt(APosition: Integer): Integer;
    function Point(APosition: Integer): TNyxPascalRoutineLocation;
    function PhysicalText(AStart, AFinish: Integer;
      out ALocation: TNyxPascalRoutineLocation): TNyxText;
    procedure Append(const AFile: TNyxPascalSourceRef;
      AOccurrence, AStart, ACount: Integer; const AText: TNyxText);
    procedure Expand(const AFile: TNyxPascalSourceRef);
    procedure RememberFiles(var ASite: TNyxPascalRoutineSite;
      AStart, AFinish: Integer);
    procedure Build;
    procedure BuildSymbols;
  public
    constructor Create(const ASources: INyxPascalSourceSnapshot;
      const AOwner: TNyxPascalSourceRef; const ALimits: TNyxPascalRoutineLimits;
      AForSymbols: Boolean = False);
    function Count: Integer;
    function Item(AIndex: Integer): TNyxPascalRoutineSite;
    function Owner: TNyxPascalSourceRef;
    function SymbolCount: Integer;
    function SymbolAt(AIndex: Integer): TNyxPascalSymbolSite;
    function ProblemText: TNyxText;
    function ProblemLocation: TNyxPascalRoutineLocation;
  end;

class function TNyxPascalRoutineLimits.Defaults: TNyxPascalRoutineLimits;
begin
  Result.FBytes := 32 * 1024 * 1024;
  Result.FDepth := 32;
  Result.FIncludes := 4096;
end;

procedure TNyxPascalRoutineLimits.Validate;
begin

  if (FBytes < 1) or (FBytes > 256 * 1024 * 1024) or
    (FDepth < 1) or (FDepth > 128) or (FIncludes < 1) or (FIncludes > 65536) then
  begin
    raise ENyxModel.Create('Routine browsing needs positive bytes up to 256 MiB, depth 1..128 and 1..65536 includes');
  end;
end;

function TNyxPascalRoutineLimits.UpToExpandedBytes(AValue: Integer): TNyxPascalRoutineLimits;
begin
  Result := Self;
  Result.FBytes := AValue;
  Result.Validate;
end;

function TNyxPascalRoutineLimits.UpToDepth(AValue: Integer): TNyxPascalRoutineLimits;
begin
  Result := Self;
  Result.FDepth := AValue;
  Result.Validate;
end;

function TNyxPascalRoutineLimits.UpToIncludes(AValue: Integer): TNyxPascalRoutineLimits;
begin
  Result := Self;
  Result.FIncludes := AValue;
  Result.Validate;
end;

{ Only actual lexer comment tokens can expand. Quoted text and ordinary comments
  containing convincing directive syntax never become dependency operations. }
function RoutineIncludeName(const AToken: TToken): TNyxText;
var
  LText: TNyxText;
  LBreak: Integer;
  LCommand: TNyxText;
begin
  Result := '';

  if AToken.Kind <> tkComment then
  begin
    Exit;
  end;
  LCommand := ImportDirective(AToken);

  if (LCommand <> 'i') and (LCommand <> 'include') then
  begin
    Exit;
  end;
  LText := Trim(AToken.Text);

  if Copy(LText, 1, 2) = '{$' then
  begin
    LText := Trim(Copy(LText, 3, Length(LText) - 3));
  end
  else
  begin
    LText := Trim(Copy(LText, 4, Length(LText) - 5));
  end;
  LBreak := 1;
  while (LBreak <= Length(LText)) and (LText[LBreak] in ['a'..'z', 'A'..'Z']) do
  begin
    Inc(LBreak);
  end;
  LText := Trim(Copy(LText, LBreak, MaxInt));

  if (LText = '+') or (LText = '-') then
  begin
    Exit;
  end;

  if (Length(LText) >= 2) and (LText[1] in ['''', '"']) and
    (LText[Length(LText)] = LText[1]) then
  begin
    LText := Copy(LText, 2, Length(LText) - 2);
  end;
  Result := NyxPascalInclude(LText).Name;
end;

constructor TPascalRoutineIndex.Create(const ASources: INyxPascalSourceSnapshot;
  const AOwner: TNyxPascalSourceRef; const ALimits: TNyxPascalRoutineLimits;
  AForSymbols: Boolean);
begin
  inherited Create;
  AOwner.Validate;
  ALimits.Validate;

  if AOwner.Role = npsInclude then
  begin
    raise ENyxModel.Create('An include needs an explicit owning main/helper unit');
  end;
  FOwner := AOwner;
  FLimits := ALimits;
  FSources := CaptureNyxPascalSources(ASources);
  FParts := TNyxStrings.Create;
  try
    Expand(FOwner);
    FText := FParts.Join;

    if AForSymbols then
    begin
      BuildSymbols;
    end
    else
    begin
      Build;
    end;
  finally
    FreeAndNil(FParts);
    FSources := nil;
    FText := '';
    FSpans := nil;
    FPhysical := nil;
    FStack := nil;
  end;
end;

procedure TPascalRoutineIndex.Append(const AFile: TNyxPascalSourceRef;
  AOccurrence, AStart, ACount: Integer; const AText: TNyxText);
var
  LText: TNyxText;
  LBytes: Integer;
  LIndex: Integer;
begin

  if ACount = 0 then
  begin
    Exit;
  end;
  LText := Copy(AText, AStart, ACount);
  LBytes := NyxUTF8ByteCount(LText);

  if LBytes > FLimits.FBytes - FExpandedBytes then
  begin
    raise ENyxModel.Create('Expanded routine source exceeds its UTF-8 budget');
  end;
  Inc(FExpandedBytes, LBytes);
  FParts.Add(LText);
  LIndex := Length(FSpans);
  SetLength(FSpans, LIndex + 1);
  FSpans[LIndex].FileReference := AFile;
  FSpans[LIndex].LogicalStart := FLogicalLength + 1;
  FSpans[LIndex].PhysicalStart := AStart;
  FSpans[LIndex].Count := ACount;
  FSpans[LIndex].Occurrence := AOccurrence;
  Inc(FLogicalLength, ACount);
end;

procedure TPascalRoutineIndex.Expand(const AFile: TNyxPascalSourceRef);
var
  LText: TNyxText;
  LTokens: TTokens;
  LName: TNyxText;
  LIndex: Integer;
  LCursor: Integer;
  LOccurrence: Integer;
  LProblem: TNyxSourceDiagnostic;
  LStop: Integer;
begin

  if Length(FStack) >= FLimits.FDepth then
  begin
    raise ENyxModel.Create('Routine include expansion exceeds its depth budget');
  end;
  for LIndex := 0 to High(FStack) do
  begin

    if FStack[LIndex].SameFile(AFile) then
    begin
      raise ENyxModel.Create('Routine include expansion contains a cycle');
    end;
  end;

  if FOccurrences > FLimits.FIncludes then
  begin
    raise ENyxModel.Create('Routine include expansion exceeds its occurrence budget');
  end;
  Inc(FOccurrences);
  LOccurrence := FOccurrences;
  SetLength(FStack, Length(FStack) + 1);
  FStack[High(FStack)] := AFile;
  try
    LText := FSources.Text(AFile);
    LTokens := LexSourceRead(LText, FSources.Kind = pssEditing, LProblem, LStop);
    LCursor := 1;
    for LIndex := 0 to High(LTokens) do
    begin
      LName := RoutineIncludeName(LTokens[LIndex]);

      if LName <> '' then
      begin
        Append(AFile, LOccurrence, LCursor, LTokens[LIndex].Position - LCursor, LText);
        { A directive is lexical separation even without adjacent whitespace.
          Synthetic separators have no physical ownership and cannot become an
          edit boundary. Ordinary body spans still retain their original include
          directives when both boundaries belong to the same occurrence. }
        Append(AFile, 0, 1, 1, ' ');
        Expand(TNyxPascalSourceRef.IncludeFile(NyxPascalInclude(LName)));

        if FStopped then
        begin
          Exit;
        end;
        Append(AFile, 0, 1, 1, ' ');
        LCursor := LTokens[LIndex].Finish;
      end;
    end;
    Append(AFile, LOccurrence, LCursor, LStop - LCursor, LText);

    if LProblem.Defined then
    begin
      FProblemText := LProblem.Message;
      FProblemLocation := PhysicalLocation(AFile, LStop);
      FStopped := True;
    end;
  finally
    SetLength(FStack, Length(FStack) - 1);
  end;
end;

function TPascalRoutineIndex.PhysicalFile(const AFile: TNyxPascalSourceRef): Integer;
var
  LIndex: Integer;
  LCursor: TRoutinePhysicalPoint;
  LScalar: Integer;
  LPoint: Integer;
begin
  for LIndex := 0 to High(FPhysical) do
  begin

    if FPhysical[LIndex].FileReference.SameFile(AFile) then
    begin
      Exit(LIndex);
    end;
  end;
  Result := Length(FPhysical);
  SetLength(FPhysical, Result + 1);
  FPhysical[Result].FileReference := AFile;
  FPhysical[Result].Text := FSources.Text(AFile);
  LCursor := Default(TRoutinePhysicalPoint);
  LCursor.Position := 1;
  LCursor.Line := 1;
  LCursor.Column := 1;
  SetLength(FPhysical[Result].Points, 1);
  FPhysical[Result].Points[0] := LCursor;
  while LCursor.Position <= Length(FPhysical[Result].Text) do
  begin

    if not NyxNextScalar(FPhysical[Result].Text, LCursor.Position, LScalar) then
    begin
      raise ENyxModel.Create('Routine locations require valid Unicode');
    end;
    Inc(LCursor.Offset);

    if LScalar = 10 then
    begin
      Inc(LCursor.Line);
      LCursor.Column := 1;
    end
    else
    begin
      Inc(LCursor.Column);
    end;

    if LCursor.Offset mod 256 = 0 then
    begin
      LPoint := Length(FPhysical[Result].Points);
      SetLength(FPhysical[Result].Points, LPoint + 1);
      FPhysical[Result].Points[LPoint] := LCursor;
    end;
  end;
end;

function TPascalRoutineIndex.PhysicalLocation(const AFile: TNyxPascalSourceRef;
  APosition: Integer): TNyxPascalRoutineLocation;
var
  LFile: Integer;
  LLow: Integer;
  LHigh: Integer;
  LMiddle: Integer;
  LCursor: TRoutinePhysicalPoint;
  LScalar: Integer;
begin
  Result := Default(TNyxPascalRoutineLocation);
  LFile := PhysicalFile(AFile);

  if (APosition < 1) or (APosition > Length(FPhysical[LFile].Text) + 1) then
  begin
    raise ENyxModel.Create('Physical routine position is outside its captured file');
  end;
  LLow := 0;
  LHigh := High(FPhysical[LFile].Points);
  while LLow < LHigh do
  begin
    LMiddle := (LLow + LHigh + 1) div 2;

    if FPhysical[LFile].Points[LMiddle].Position <= APosition then
    begin
      LLow := LMiddle;
    end
    else
    begin
      LHigh := LMiddle - 1;
    end;
  end;
  LCursor := FPhysical[LFile].Points[LLow];
  while LCursor.Position < APosition do
  begin
    NyxNextScalar(FPhysical[LFile].Text, LCursor.Position, LScalar);
    Inc(LCursor.Offset);

    if LScalar = 10 then
    begin
      Inc(LCursor.Line);
      LCursor.Column := 1;
    end
    else
    begin
      Inc(LCursor.Column);
    end;
  end;

  if LCursor.Position <> APosition then
  begin
    raise ENyxModel.Create('Routine location splits a Unicode scalar');
  end;
  Result.FDefined := True;
  Result.FFile := AFile;
  Result.FOffset := LCursor.Offset;
  Result.FLine := LCursor.Line;
  Result.FColumn := LCursor.Column;
end;

function TPascalRoutineIndex.SpanAt(APosition: Integer): Integer;
var
  LLow: Integer;
  LHigh: Integer;
  LMiddle: Integer;
begin

  if (APosition < 1) or (APosition > FLogicalLength) then
  begin
    raise ENyxModel.Create('Expanded routine position is outside its captured text');
  end;
  LLow := 0;
  LHigh := High(FSpans);
  while LLow < LHigh do
  begin
    LMiddle := (LLow + LHigh + 1) div 2;

    if FSpans[LMiddle].LogicalStart <= APosition then
    begin
      LLow := LMiddle;
    end
    else
    begin
      LHigh := LMiddle - 1;
    end;
  end;
  Result := LLow;

  if APosition >= FSpans[Result].LogicalStart + FSpans[Result].Count then
  begin
    raise ENyxModel.Create('Expanded routine position has no source span');
  end;
end;

function TPascalRoutineIndex.Point(APosition: Integer): TNyxPascalRoutineLocation;
var
  LSpan: Integer;
begin
  LSpan := SpanAt(APosition);

  if FSpans[LSpan].Occurrence = 0 then
  begin
    raise ENyxModel.Create('Synthetic expansion trivia has no physical source location');
  end;
  Result := PhysicalLocation(FSpans[LSpan].FileReference,
    FSpans[LSpan].PhysicalStart + APosition - FSpans[LSpan].LogicalStart);
end;

function TPascalRoutineIndex.PhysicalText(AStart, AFinish: Integer;
  out ALocation: TNyxPascalRoutineLocation): TNyxText;
var
  LFirst: Integer;
  LLast: Integer;
  LStart: Integer;
  LFinish: Integer;
  LFile: Integer;
begin
  ALocation := Default(TNyxPascalRoutineLocation);
  Result := Copy(FText, AStart, AFinish - AStart);

  if AFinish <= AStart then
  begin
    Exit;
  end;
  LFirst := SpanAt(AStart);
  LLast := SpanAt(AFinish - 1);

  if (FSpans[LFirst].Occurrence = 0) or
    (FSpans[LFirst].Occurrence <> FSpans[LLast].Occurrence) then
  begin
    Exit;
  end;
  LStart := FSpans[LFirst].PhysicalStart + AStart - FSpans[LFirst].LogicalStart;
  LFinish := FSpans[LLast].PhysicalStart + AFinish - FSpans[LLast].LogicalStart;
  ALocation := PhysicalLocation(FSpans[LFirst].FileReference, LStart);
  LFile := PhysicalFile(ALocation.FileReference);
  Result := Copy(FPhysical[LFile].Text, LStart, LFinish - LStart);
end;

function TNyxPascalRoutineSite.TouchesFile(const AFile: TNyxPascalSourceRef): Boolean;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FSourceFiles) do
  begin

    if FSourceFiles[LIndex].SameFile(AFile) then
    begin
      Exit(True);
    end;
  end;
  Result := False;
end;

procedure TPascalRoutineIndex.RememberFiles(var ASite: TNyxPascalRoutineSite;
  AStart, AFinish: Integer);
var
  LSpan: Integer;
  LCount: Integer;
begin

  if AFinish <= AStart then
  begin
    Exit;
  end;
  LSpan := SpanAt(AStart);
  while (LSpan < Length(FSpans)) and (FSpans[LSpan].LogicalStart < AFinish) do
  begin

    if (FSpans[LSpan].Occurrence <> 0) and
      not ASite.TouchesFile(FSpans[LSpan].FileReference) then
    begin
      LCount := Length(ASite.FSourceFiles);
      SetLength(ASite.FSourceFiles, LCount + 1);
      ASite.FSourceFiles[LCount] := FSpans[LSpan].FileReference;
    end;
    Inc(LSpan);
  end;
end;

{ Calling conventions and other declaration modifiers precede local declarations
  or begin/asm. Keep each complete prefix clause in the immutable signature;
  a body edit must not silently change cdecl/stdcall, export or inline behavior.
  The existing balancer has already delimited this one complete implementation. }
function ProjectRoutineBodyStart(const ASource: TNyxRoutineSource): Integer;
var
  LTokens: TTokens;
  LIndex: Integer;
  LDepth: Integer;
begin
  Result := ASource.FHeaderFinish;
  LTokens := HandlerTokens(ASource.Code);
  LIndex := 0;
  while LIndex < Length(LTokens) do
  begin

    if HandlerWord(LTokens, LIndex, 'begin') or HandlerWord(LTokens, LIndex, 'asm') or
      HandlerWord(LTokens, LIndex, 'var') or HandlerWord(LTokens, LIndex, 'const') or
      HandlerWord(LTokens, LIndex, 'type') or HandlerWord(LTokens, LIndex, 'label') or
      HandlerWord(LTokens, LIndex, 'resourcestring') or
      RoutineOpening(LTokens, LIndex) then
    begin
      Exit;
    end;
    LDepth := 0;
    while LIndex < Length(LTokens) do
    begin

      if HandlerSymbol(LTokens, LIndex, '(') or HandlerSymbol(LTokens, LIndex, '[') then
      begin
        Inc(LDepth);
      end
      else if HandlerSymbol(LTokens, LIndex, ')') or HandlerSymbol(LTokens, LIndex, ']') then
      begin
        Dec(LDepth);
      end;

      if (LDepth < 0) or (LDepth > 256) then
      begin
        raise ENyxModel.Create('Routine declaration modifiers have ambiguous groups');
      end;

      if (LDepth = 0) and HandlerSymbol(LTokens, LIndex, ';') then
      begin
        Result := ASource.FHeaderFinish + LTokens[LIndex].Finish - 1;
        Inc(LIndex);
        Break;
      end;
      Inc(LIndex);
    end;
  end;
end;

procedure TPascalRoutineIndex.Build;
var
  LCatalog: TNyxRoutineCatalog;
  LIndex: Integer;
  LSource: TNyxRoutineSource;
  LSite: TNyxPascalRoutineSite;
  LDeclaration: TNyxRoutineDeclarationSource;
  LManaged: Boolean;
  LBodyStart: Integer;
  LProblem: TNyxSourceDiagnostic;
  LProblemPosition: Integer;
  LLastSpan: Integer;
  LDeclarationText: TNyxText;
begin
  LManaged := (FOwner.Role = npsMain) and (Pos(NyxViewsBegin, FText) > 0);
  LCatalog := ReadRoutineCatalog(FText, LManaged, FSources.Kind = pssEditing, LProblem);

  if LProblem.Defined and (FProblemText = '') then
  begin
    FProblemText := LProblem.Message;
    LProblemPosition := NyxTextPosition(FText, LProblem.Line, LProblem.Column);

    if LProblemPosition <= FLogicalLength then
    begin
      FProblemLocation := Point(LProblemPosition);
    end
    else if Length(FSpans) > 0 then
    begin
      LLastSpan := High(FSpans);

      if FSpans[LLastSpan].Occurrence <> 0 then
      begin
        FProblemLocation := PhysicalLocation(FSpans[LLastSpan].FileReference,
          FSpans[LLastSpan].PhysicalStart + FSpans[LLastSpan].Count);
      end;
    end
    else if FLogicalLength = 0 then
    begin
      FProblemLocation := PhysicalLocation(FOwner, 1);
    end;
  end;
  LDeclarationText := FText;

  if (FSources.Kind = pssEditing) and (LCatalog.Count > 0) then
  begin
    { Counterpart lookup sees the exact complete prefix, never a fabricated
      closing body for an unrelated unfinished routine later in the buffer. }
    LDeclarationText := Copy(FText, 1, LCatalog.Item(LCatalog.Count - 1).FFinish - 1);
  end;
  SetLength(FEntries, LCatalog.Count);
  for LIndex := 0 to LCatalog.Count - 1 do
  begin
    LSource := LCatalog.Item(LIndex);
    LSite := Default(TNyxPascalRoutineSite);
    LSite.FRoutine := LSource.Routine;
    LSite.FKind := LSource.Kind;
    LSite.FDefinition := Point(LSource.FStart);
    LBodyStart := ProjectRoutineBodyStart(LSource);
    LSite.FSignature := PhysicalText(LSource.FStart, LBodyStart,
      LSite.FSignatureLocation);
    LSite.FBody := PhysicalText(LBodyStart, LSource.FFinish,
      LSite.FBodyLocation);
    LSite.FEditable := LSource.Editable and LSite.FBodyLocation.IsDefined;
    LSite.FReason := LSource.Reason;
    RememberFiles(LSite, LSource.FStart, LSource.FFinish);

    if (FOwner.Role = npsMain) and
      (SameText(LSource.Routine.Name, 'BuildNyxDocument') or
        SameText(LSource.Routine.Name, 'BindNyxCallbacks')) then
    begin
      LSite.FEditable := False;
      LSite.FReason := 'Compiler-managed main entry point; use explicit source authoring';
    end;

    if not LSite.FBodyLocation.IsDefined and (LSite.FReason = '') then
    begin
      LSite.FReason := 'Implementation spans different include occurrences; use explicit grouped file ranges';
    end;

    if Pos('.', LSource.Routine.Name) = 0 then
    begin
      try
        { Browsing may read compiler-managed names; editability already follows
          the owning-unit catalog. Legacy helper authoring retains its stricter
          DeclarationGlobal guard through the default managed overload. }
        LDeclaration := DeclarationSite(LDeclarationText, LSource.Routine, False);
        LSite.FVisibility := prvImplementation;

        if LDeclaration.Visibility = rvInterface then
        begin
          LSite.FVisibility := prvInterface;
          LSite.FDeclaration := PhysicalText(LDeclaration.FStart, LDeclaration.FFinish,
            LSite.FDeclarationLocation);
          RememberFiles(LSite, LDeclaration.FStart, LDeclaration.FFinish);

          if not DeclarationSameSignature(LDeclaration.Declaration, LSource.Signature) then
          begin
            LSite.FVisibility := prvUnknown;
            LSite.FEditable := False;
            LSite.FReason := 'Interface and implementation signatures have ambiguous ownership';
          end;
        end;
      except
        on ENyxSource do
        begin
          { An ambiguous/conditional counterpart is unknown; the implemented
            body's independent location still remains useful for inspection. }
          LSite.FVisibility := prvUnknown;
        end;
      end;
    end;
    FEntries[LIndex] := LSite;
  end;
end;

function TPascalRoutineIndex.Count: Integer;
begin
  Result := Length(FEntries);
end;

function TPascalRoutineIndex.Item(AIndex: Integer): TNyxPascalRoutineSite;
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ENyxModel.Create('Routine index is outside its owning source snapshot');
  end;
  Result := FEntries[AIndex];
end;

function TPascalRoutineIndex.Owner: TNyxPascalSourceRef;
begin
  Result := FOwner;
end;

function TPascalRoutineIndex.ProblemText: TNyxText;
begin
  Result := FProblemText;
end;

function TPascalRoutineIndex.ProblemLocation: TNyxPascalRoutineLocation;
begin
  Result := FProblemLocation;
end;

function CaptureNyxPascalRoutineIndex(const AProject: INyxPascalProject;
  const AOwner: TNyxPascalSourceRef;
  const ALimits: TNyxPascalRoutineLimits): INyxPascalRoutineIndex;
begin
  Result := TPascalRoutineIndex.Create(CaptureNyxPascalSources(AProject), AOwner, ALimits);
end;

function CaptureNyxPascalRoutineIndex(const AProject: INyxPascalProject;
  const AOwner: TNyxPascalSourceRef): INyxPascalRoutineIndex;
begin
  Result := CaptureNyxPascalRoutineIndex(AProject, AOwner, TNyxPascalRoutineLimits.Defaults);
end;

function CaptureNyxPascalRoutineIndex(const ASources: INyxPascalSourceSnapshot;
  const AOwner: TNyxPascalSourceRef): INyxPascalRoutineIndex;
begin
  Result := CaptureNyxPascalRoutineIndex(ASources, AOwner, TNyxPascalRoutineLimits.Defaults);
end;

function CaptureNyxPascalRoutineIndex(const ASources: INyxPascalSourceSnapshot;
  const AOwner: TNyxPascalSourceRef;
  const ALimits: TNyxPascalRoutineLimits): INyxPascalRoutineIndex;
begin
  Result := TPascalRoutineIndex.Create(ASources, AOwner, ALimits);
end;
