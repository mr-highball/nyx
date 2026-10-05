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

{ Declaration edits share the existing lexer. They do not resolve Pascal types
  or external-unit callers; admitted companions still need an ordinary build. }

procedure DeclarationGlobal(const ARoutine: TNyxRoutineRef);
begin
  NyxRoutine(ARoutine.Name);

  if Pos('.', ARoutine.Name) > 0 then
  begin
    raise ENyxModel.Create('Unit helper declarations cannot change class-member signatures');
  end;

  if SameText(ARoutine.Name, 'BuildNyxDocument') or SameText(ARoutine.Name, 'BindNyxCallbacks') then
  begin
    raise ENyxModel.Create('Compiler-managed companion infrastructure is not a helper declaration');
  end;
end;

function DeclarationHeaderEnd(const ASource: TNyxText; const ATokens: TTokens;
  AStart: Integer): Integer;
var
  LIndex: Integer;
  LDepth: Integer;
begin
  LDepth := 0;
  for LIndex := AStart + 1 to High(ATokens) do
  begin

    if HandlerSymbol(ATokens, LIndex, '(') or HandlerSymbol(ATokens, LIndex, '[') then
    begin
      Inc(LDepth);
    end
    else if HandlerSymbol(ATokens, LIndex, ')') or HandlerSymbol(ATokens, LIndex, ']') then
    begin
      Dec(LDepth);
    end;

    if (LDepth < 0) or (LDepth > 256) then
    begin
      HandlerFail(ASource, 'Helper signature has ambiguous groups', ATokens, LIndex);
    end;

    if (LDepth = 0) and HandlerSymbol(ATokens, LIndex, ';') then
    begin
      Exit(LIndex);
    end;
  end;
  HandlerFail(ASource, 'Finish the helper signature with a semicolon', ATokens, AStart);
  Result := -1;
end;

function DeclarationSameSignature(const ALeft, ARight: TNyxText): Boolean;
var
  LLeft: TTokens;
  LRight: TTokens;
  LIndex: Integer;
begin
  LLeft := HandlerTokens(ALeft);
  LRight := HandlerTokens(ARight);
  Result := Length(LLeft) = Length(LRight);

  if not Result then
  begin
    Exit;
  end;
  for LIndex := 0 to High(LLeft) do
  begin

    if (LLeft[LIndex].Kind <> LRight[LIndex].Kind) or
      ((LLeft[LIndex].Kind = tkWord) and not SameText(LLeft[LIndex].Text, LRight[LIndex].Text)) or
      ((LLeft[LIndex].Kind <> tkWord) and (LLeft[LIndex].Text <> LRight[LIndex].Text)) then
    begin
      Exit(False);
    end;
  end;
end;

function DeclarationSite(const ASource: TNyxText;
  const ARoutine: TNyxRoutineRef): TNyxRoutineDeclarationSource;
var
  LTokens: TTokens;
  LIndex: Integer;
  LCursor: Integer;
  LFinish: Integer;
  LTypes: Integer;
  LName: TNyxText;
  LFound: Boolean;
begin
  Result := Default(TNyxRoutineDeclarationSource);
  Result.FVisibility := rvImplementation;
  DeclarationGlobal(ARoutine);
  LTokens := HandlerTokens(ASource);
  LIndex := 0;
  while (LIndex < Length(LTokens)) and not HandlerWord(LTokens, LIndex, 'interface') do
  begin
    Inc(LIndex);
  end;
  Inc(LIndex);
  LTypes := 0;
  LFound := False;
  while (LIndex < Length(LTokens)) and not HandlerWord(LTokens, LIndex, 'implementation') do
  begin

    if HandlerTypeBlock(LTokens, LIndex) then
    begin
      Inc(LTypes);
    end
    else if (LTypes > 0) and HandlerWord(LTokens, LIndex, 'end') then
    begin
      Dec(LTypes);
    end
    else if (LTypes = 0) and
      (HandlerWord(LTokens, LIndex, 'procedure') or HandlerWord(LTokens, LIndex, 'function')) and
      (HandlerSymbol(LTokens, LIndex - 1, ';') or HandlerWord(LTokens, LIndex - 1, 'interface')) then
    begin
      LCursor := LIndex + 1;

      if HandlerSymbol(LTokens, LCursor, '&') then
      begin
        Inc(LCursor);
      end;

      if (LCursor >= Length(LTokens)) or (LTokens[LCursor].Kind <> tkWord) then
      begin
        HandlerFail(ASource, 'Interface helper identity is ambiguous', LTokens, LCursor);
      end;
      LName := LTokens[LCursor].Text;
      LFinish := DeclarationHeaderEnd(ASource, LTokens, LIndex);

      if SameText(LName, ARoutine.Name) then
      begin

        if LFound then
        begin
          HandlerFail(ASource, 'Interface helper identity is duplicated or overloaded', LTokens, LIndex);
        end;
        Result.FStart := LTokens[LIndex].Position;
        Result.FFinish := LTokens[LFinish].Finish;
        HandlerNoDirectives(ASource, Result.FStart, Result.FFinish);

        if (LFinish + 1 < Length(LTokens)) and
          not (HandlerWord(LTokens, LFinish + 1, 'function') or
          HandlerWord(LTokens, LFinish + 1, 'procedure') or
          HandlerWord(LTokens, LFinish + 1, 'implementation') or
          HandlerWord(LTokens, LFinish + 1, 'type') or
          HandlerWord(LTokens, LFinish + 1, 'var') or
          HandlerWord(LTokens, LFinish + 1, 'const') or
          HandlerWord(LTokens, LFinish + 1, 'resourcestring')) then
        begin
          HandlerFail(ASource, 'Interface helper modifiers require ordinary Pascal editing',
            LTokens, LFinish + 1);
        end;
        Result.FSignature := Copy(ASource, Result.FStart, Result.FFinish - Result.FStart);
        Result.FVisibility := rvInterface;
        Result.FLine := 1;
        for LCursor := 1 to Result.FStart - 1 do
        begin

          if ASource[LCursor] = #10 then
          begin
            Inc(Result.FLine);
          end;
        end;
        LFound := True;
      end;
      LIndex := LFinish + 1;
      Continue;
    end;
    Inc(LIndex);
  end;
end;

function NyxRoutineDeclaration(AKind: TNyxRoutineKind; const ARoutine: TNyxRoutineRef;
  AVisibility: TNyxRoutineVisibility; const ASignature, AImplementation: TNyxText): TNyxRoutineDeclaration;
var
  LTokens: TTokens;
  LAll: TTokens;
  LHeader: Integer;
  LFinish: Integer;
  LCursor: Integer;
  LName: TNyxText;
  LDefinition: TNyxText;
begin
  Result := Default(TNyxRoutineDeclaration);
  DeclarationGlobal(ARoutine);

  if not ((AKind = nrProcedure) or (AKind = nrFunction)) or
    not ((AVisibility = rvInterface) or (AVisibility = rvImplementation)) then
  begin
    raise ENyxModel.Create('Unit declarations require a procedure/function and typed visibility');
  end;
  LTokens := HandlerTokens(ASignature);

  if (Length(LTokens) < 3) or not RoutineOpening(LTokens, 0) or
    (RoutineKind(LTokens, 0) <> AKind) or HandlerWord(LTokens, 0, 'class') then
  begin
    raise ENyxModel.Create('Helper signature must match its typed routine kind');
  end;
  LHeader := DeclarationHeaderEnd(ASignature, LTokens, 0);

  if (LHeader <> High(LTokens)) or (LTokens[LHeader].Finish <> Length(ASignature) + 1) or
    (LTokens[0].Position <> 1) then
  begin
    raise ENyxModel.Create('Supply one exact helper signature ending at its semicolon');
  end;
  LCursor := 1;

  if HandlerSymbol(LTokens, LCursor, '&') then
  begin
    Inc(LCursor);
  end;

  if (LCursor >= Length(LTokens)) or (LTokens[LCursor].Kind <> tkWord) then
  begin
    raise ENyxModel.Create('Helper signature requires its exact Pascal identity');
  end;
  LName := LTokens[LCursor].Text;

  if not SameText(LName, ARoutine.Name) or HandlerSymbol(LTokens, LCursor + 1, '.') then
  begin
    raise ENyxModel.Create('Helper signature must match its distinct unit routine reference');
  end;
  LDefinition := JoinSourceFrame([ASignature, AImplementation]);
  LAll := HandlerTokens(LDefinition);
  LFinish := HandlerRoutineEnd(LDefinition, LAll, 0, 0, LHeader);

  if (LFinish <> High(LAll)) or (LAll[LHeader].Finish <> Length(ASignature) + 1) then
  begin
    raise ENyxModel.Create('Supply one helper implementation, without sibling declarations');
  end;
  HandlerNoDirectives(LDefinition, 1, Length(LDefinition) + 1);
  for LCursor := LAll[LFinish].Finish to Length(LDefinition) do
  begin

    if not (LDefinition[LCursor] in [#9, #10, #13, ' ']) then
    begin
      raise ENyxModel.Create('Finish helper code at its semicolon with only trailing whitespace');
    end;
  end;
  Result.FRoutine := NyxRoutine(ARoutine.Name);
  Result.FKind := AKind;
  Result.FVisibility := AVisibility;
  Result.FSignature := ASignature;
  Result.FImplementation := AImplementation;
end;

function ReadNyxRoutineDeclaration(const ASource: TNyxText;
  const ARoutine: TNyxRoutineRef): TNyxRoutineDeclarationSource;
var
  LRoutine: TNyxRoutineSource;
begin
  LRoutine := ReadNyxRoutineSource(ASource, ARoutine);
  Result := DeclarationSite(ASource, ARoutine);
  Result.FImplementationSignature := LRoutine.Signature;
  Result.FImplementationLine := LRoutine.Line;

  if (Result.Visibility = rvInterface) and
    not DeclarationSameSignature(Result.Declaration, LRoutine.Signature) then
  begin
    raise ENyxSource.CreateAt('Interface and implementation signatures have ambiguous ownership',
      ASource, Result.FStart);
  end;
end;

function TNyxRoutineDeclarationSource.Text(APart: TNyxDeclarationPart): TNyxText;
begin
  case Ord(APart) of
    Ord(dspInterface):
      Result := FSignature;
    Ord(dspImplementation):
      Result := FImplementationSignature;
  else
    raise ENyxModel.Create('Unknown typed declaration counterpart');
  end;
end;

function TNyxRoutineDeclarationSource.SourceLine(APart: TNyxDeclarationPart): Integer;
begin
  case Ord(APart) of
    Ord(dspInterface):
      Result := FLine;
    Ord(dspImplementation):
      Result := FImplementationLine;
  else
    raise ENyxModel.Create('Unknown typed declaration counterpart');
  end;
end;

function DeclarationAnchor(const ASource: TNyxText; ABefore: Integer): Integer;
var
  LTokens: TTokens;
  LIndex: Integer;
  LCursor: Integer;
  LSameLine: Boolean;
  LConditions: Integer;
  LDirective: TNyxText;
begin
  { Insert after the preceding owned token and its attached inline comments.
    Leading comments/trivia of the following declaration remain beside it. }
  LTokens := Lex(ASource);
  Result := 1;
  for LIndex := 0 to High(LTokens) do
  begin

    if LTokens[LIndex].Position >= ABefore then
    begin
      Break;
    end;

    if LTokens[LIndex].Kind <> tkComment then
    begin
      Result := LTokens[LIndex].Finish;
    end
    else
    begin
      LSameLine := True;
      for LCursor := Result to LTokens[LIndex].Position - 1 do
      begin

        if ASource[LCursor] in [#10, #13] then
        begin
          LSameLine := False;
          Break;
        end;
      end;

      if LSameLine then
      begin
        Result := LTokens[LIndex].Finish;
      end;
    end;
  end;
  { The anchor is between tokens. The routine guard intentionally checks token
    spans; here conditional ownership must be checked at the actual gap. }
  LConditions := 0;
  for LIndex := 0 to High(LTokens) do
  begin

    if LTokens[LIndex].Position >= Result then
    begin
      Break;
    end;
    LDirective := ImportDirective(LTokens[LIndex]);

    if (LDirective = 'if') or (LDirective = 'ifdef') or
      (LDirective = 'ifndef') or (LDirective = 'ifopt') then
    begin
      Inc(LConditions);
    end
    else if (LDirective = 'endif') or (LDirective = 'ifend') then
    begin
      Dec(LConditions);
    end;

    if (LConditions < 0) or (LConditions > 256) then
    begin
      HandlerFail(ASource, 'Helper insertion has unbalanced conditional ownership', LTokens, LIndex);
    end;
  end;

  if LConditions <> 0 then
  begin
    raise ENyxSource.CreateAt('Conditional helper insertion requires ordinary Pascal editing', ASource, Result);
  end;
end;

function AddNyxRoutineDeclaration(const ASource: TNyxText;
  const ADeclaration: TNyxRoutineDeclaration): TNyxText;
var
  LDeclaration: TNyxRoutineDeclaration;
  LCatalog: TNyxRoutineCatalog;
  LSite: TNyxRoutineDeclarationSource;
  LTokens: TTokens;
  LPrefix: TNyxText;
  LBuilder: TNyxText;
  LSuffix: TNyxText;
  LAnchor: Integer;
  LIndex: Integer;
  LBefore: Integer;
  LRoutineTokens: TTokens;
  LTokenIndex: Integer;
begin
  LDeclaration := NyxRoutineDeclaration(ADeclaration.Kind, ADeclaration.Routine,
    ADeclaration.Visibility, ADeclaration.Signature, ADeclaration.Code);
  LCatalog := ReadNyxRoutines(ASource);
  for LIndex := 0 to LCatalog.Count - 1 do
  begin

    if SameText(LCatalog.Item(LIndex).Routine.Name, LDeclaration.Routine.Name) then
    begin
      raise ENyxSource.CreateAt('Helper name already has an implementation/declaration', ASource, 1);
    end;
  end;
  LSite := DeclarationSite(ASource, LDeclaration.Routine);

  if LSite.Visibility = rvInterface then
  begin
    raise ENyxSource.CreateAt('Helper name already has an interface declaration', ASource, LSite.FStart);
  end;
  SplitNyxSourceFrame(ASource, LPrefix, LBuilder, LSuffix);
  LBefore := Length(LPrefix) + 1;

  { Keep successive helper definitions in authored order, before class/managed
    consumers. A possible existing caller can require an earlier safe site.
    This lexical ordering does not pretend to resolve Pascal types or shadows. }
  for LIndex := 0 to LCatalog.Count - 1 do
  begin

    if (Pos('.', LCatalog.Item(LIndex).Routine.Name) > 0) or
      SameText(LCatalog.Item(LIndex).Routine.Name, 'BuildNyxDocument') or
      SameText(LCatalog.Item(LIndex).Routine.Name, 'BindNyxCallbacks') then
    begin

      if LCatalog.FEntries[LIndex].FStart < LBefore then
      begin
        LBefore := LCatalog.FEntries[LIndex].FStart;
      end;
      Break;
    end;
  end;
  for LIndex := 0 to LCatalog.Count - 1 do
  begin
    LRoutineTokens := HandlerTokens(LCatalog.Item(LIndex).Code);
    for LTokenIndex := 0 to High(LRoutineTokens) do
    begin

      if (LRoutineTokens[LTokenIndex].Kind = tkWord) and
        SameText(LRoutineTokens[LTokenIndex].Text, LDeclaration.Routine.Name) and
        (LCatalog.FEntries[LIndex].FStart < LBefore) then
      begin
        LBefore := LCatalog.FEntries[LIndex].FStart;
        Break;
      end;
    end;
  end;
  LAnchor := DeclarationAnchor(ASource, LBefore);
  Result := JoinSourceFrame([Copy(ASource, 1, LAnchor - 1), #10#10,
    LDeclaration.Signature, LDeclaration.Code, Copy(ASource, LAnchor, MaxInt)]);

  if LDeclaration.Visibility = rvInterface then
  begin
    LTokens := HandlerTokens(Result);
    LIndex := 0;
    while (LIndex < Length(LTokens)) and not HandlerWord(LTokens, LIndex, 'implementation') do
    begin
      Inc(LIndex);
    end;

    if LIndex = Length(LTokens) then
    begin
      HandlerFail(Result, 'Helper creation requires an implementation section', LTokens, LIndex);
    end;
    LAnchor := DeclarationAnchor(Result, LTokens[LIndex].Position);
    Result := JoinSourceFrame([Copy(Result, 1, LAnchor - 1), #10#10,
      LDeclaration.Signature, Copy(Result, LAnchor, MaxInt)]);
  end;
  ReadNyxRoutineDeclaration(Result, LDeclaration.Routine);
end;

function ReplaceNyxRoutineDeclaration(const ASource: TNyxText;
  const AReplacement: TNyxRoutineDeclaration;
  const AExpectedSignature, AExpectedImplementation, AExpectedDeclaration: TNyxText): TNyxText;
var
  LReplacement: TNyxRoutineDeclaration;
  LRoutine: TNyxRoutineSource;
  LSite: TNyxRoutineDeclarationSource;
begin
  { Validate a complete replacement before borrowing any old-source offsets.
    This is an explicit signature/body change, never an inferred caller rewrite. }
  LReplacement := NyxRoutineDeclaration(AReplacement.Kind, AReplacement.Routine,
    AReplacement.Visibility, AReplacement.Signature, AReplacement.Code);
  LRoutine := ReadNyxRoutineSource(ASource, LReplacement.Routine);
  LSite := ReadNyxRoutineDeclaration(ASource, LReplacement.Routine);

  if not LRoutine.Editable then
  begin
    raise ENyxSource.CreateAt(LRoutine.Reason, ASource, LRoutine.FStart);
  end;

  if (LRoutine.Signature <> AExpectedSignature) or (LRoutine.Code <> AExpectedImplementation) or
    (LSite.Declaration <> AExpectedDeclaration) then
  begin
    raise ENyxSource.CreateAt('Helper declaration changed; inspect all exact accepted counterparts again',
      ASource, LRoutine.FStart);
  end;

  if LReplacement.Visibility <> LSite.Visibility then
  begin
    raise ENyxSource.CreateAt('Signature editing must retain helper visibility', ASource, LRoutine.FStart);
  end;
  Result := JoinSourceFrame([Copy(ASource, 1, LRoutine.FStart - 1),
    LReplacement.Signature, LReplacement.Code, Copy(ASource, LRoutine.FFinish, MaxInt)]);

  if LSite.Visibility = rvInterface then
  begin
    { Interface spans precede the implementation, so their original offsets
      remain valid after the descending replacement. Surrounding bytes survive. }
    Result := JoinSourceFrame([Copy(Result, 1, LSite.FStart - 1), LReplacement.Signature,
      Copy(Result, LSite.FFinish, MaxInt)]);
  end;

  if Result = ASource then
  begin
    raise ENyxModel.Create('Signature replacement has no source change');
  end;
  ReadNyxRoutineDeclaration(Result, LReplacement.Routine);
end;

function RemoveNyxRoutineDeclaration(const ASource: TNyxText;
  const ARoutine: TNyxRoutineRef;
  const AExpectedSignature, AExpectedImplementation, AExpectedDeclaration: TNyxText): TNyxText;
var
  LRoutine: TNyxRoutineSource;
  LSite: TNyxRoutineDeclarationSource;
  LTokens: TTokens;
  LIndex: Integer;
  LPosition: Integer;
  LDirectiveTokens: TTokens;
  LDirectiveText: TNyxText;
  LDirectiveIndex: Integer;
begin
  DeclarationGlobal(ARoutine);
  LRoutine := ReadNyxRoutineSource(ASource, ARoutine);
  LSite := ReadNyxRoutineDeclaration(ASource, ARoutine);

  if not LRoutine.Editable then
  begin
    raise ENyxSource.CreateAt(LRoutine.Reason, ASource, LRoutine.FStart);
  end;

  if (LRoutine.Signature <> AExpectedSignature) or (LRoutine.Code <> AExpectedImplementation) or
    (LSite.Declaration <> AExpectedDeclaration) then
  begin
    raise ENyxSource.CreateAt('Helper declaration changed; inspect all exact accepted counterparts again',
      ASource, LRoutine.FStart);
  end;
  LTokens := Lex(ASource);
  for LIndex := 0 to High(LTokens) do
  begin
    LPosition := LTokens[LIndex].Position;

    if ((LPosition >= LRoutine.FStart) and (LPosition < LRoutine.FFinish)) or
      ((LSite.FStart > 0) and (LPosition >= LSite.FStart) and (LPosition < LSite.FFinish)) then
    begin
      Continue;
    end;

    if (LTokens[LIndex].Kind = tkWord) and SameText(LTokens[LIndex].Text, ARoutine.Name) then
    begin
      { No type-resolution claim: a shadow/member reference can be ambiguous.
        Refuse rather than erase a possibly retained caller or registration. }
      HandlerFail(ASource, 'Retained source may reference this helper; update callers before removal',
        LTokens, LIndex);
    end;

    if ImportDirective(LTokens[LIndex]) <> '' then
    begin
      LDirectiveText := LTokens[LIndex].Text;

      if Copy(LDirectiveText, 1, 2) = '{$' then
      begin
        LDirectiveText := Copy(LDirectiveText, 3, Length(LDirectiveText) - 3);
      end
      else
      begin
        LDirectiveText := Copy(LDirectiveText, 4, Length(LDirectiveText) - 5);
      end;
      LDirectiveTokens := Lex(LDirectiveText);
      for LDirectiveIndex := 0 to High(LDirectiveTokens) do
      begin

        if (LDirectiveTokens[LDirectiveIndex].Kind = tkWord) and
          SameText(LDirectiveTokens[LDirectiveIndex].Text, ARoutine.Name) then
        begin
          HandlerFail(ASource, 'A retained compiler directive may reference this helper', LTokens, LIndex);
        end;
      end;
    end;
  end;
  Result := JoinSourceFrame([Copy(ASource, 1, LRoutine.FStart - 1),
    Copy(ASource, LRoutine.FFinish, MaxInt)]);

  if LSite.Visibility = rvInterface then
  begin
    { Interface lies before implementation, so its exact offsets still apply. }
    Result := JoinSourceFrame([Copy(Result, 1, LSite.FStart - 1),
      Copy(Result, LSite.FFinish, MaxInt)]);
  end;
end;
