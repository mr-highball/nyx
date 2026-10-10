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

{ Included by nyx.source. The existing lexer/block balancer finds exact Pascal
  boundaries. It never evaluates an expression or asserts execution authority. }

function CustomizationRegion(ADocument: TNyxDocument;
  const ABlocks: TNyxCustomizationBlocks): TNyxText;
begin
  Result := '{ nyx:customizations-begin }' + #10 +
    '{ Visual edits are applied after the original handwritten builder.' + #10 +
    '  Keep helpers and expressions outside this managed customization boundary. }' + #10 +
    TNyxCodegen.CustomizationRoutine(ADocument, ABlocks) + #10 +
    'function BuildNyxDocument: TNyxDocument;' + #10 +
    'begin' + #10 +
    '  Result := BuildNyxOriginalDocument;' + #10 +
    '  try' + #10 + #10 +
    '    if Result = nil then' + #10 +
    '    begin' + #10 +
    '      raise ENyxModel.Create(''The original builder returned no document'');' + #10 +
    '    end;' + #10 +
    '    CustomizeNyxDocument(Result);' + #10 +
    '  except' + #10 +
    '    Result.Free;' + #10 +
    '    Result := nil;' + #10 +
    '    raise;' + #10 +
    '  end;' + #10 +
    'end;' + #10 +
    '{ nyx:customizations-end }';
end;

function CustomizeNyxPropertySource(const ASource: TNyxText;
  ABefore, AAfter: TNyxDocument; AValidateOnly: Boolean = False): TNyxText;
const
  CBlockPrefix: TNyxText = '// nyx:customize ';
var
  LTokens: TTokens;
  LCodeTokens: TTokens;
  LIdentity: TTokens;
  LBeforeBlocks: TNyxCustomizationBlocks;
  LChanges: TNyxCustomizationBlocks;
  LBlocks: TNyxCustomizationBlocks;
  LIndex: Integer;
  LPrior: Integer;
  LBegin: Integer;
  LEnd: Integer;
  LBlock: Integer;
  LBodyStart: Integer;
  LBodyEnd: Integer;
  LBuilder: Integer;
  LBuilderCount: Integer;
  LFinish: Integer;
  LHeader: Integer;
  LInsertion: Integer;
  LDirective: TNyxText;
  LSource: TNyxText;
  LRegion: TNyxText;
  LImports: TNyxImportClause;
  LGeneratedImports: TNyxImportClause;
  LImplementationImports: TNyxImportClause;
  LFound: Boolean;
begin
  NyxCompanionUnitName(ASource);
  LChanges := nil;

  if not AValidateOnly then
  begin
    LChanges := TNyxCodegen.CustomizationChanges(ABefore, AAfter);
  end;

  if (Length(LChanges) = 0) and not AValidateOnly then
  begin
    Exit(ASource);
  end;
  LTokens := Lex(ASource);
  LBegin := -1;
  LEnd := -1;
  LBlock := -1;
  LBeforeBlocks := nil;
  LBodyStart := 0;
  for LIndex := 0 to High(LTokens) do
  begin
    LDirective := ImportDirective(LTokens[LIndex]);

    if (LDirective = 'if') or (LDirective = 'ifdef') or (LDirective = 'ifndef') or
      (LDirective = 'ifopt') or (LDirective = 'else') or (LDirective = 'elseif') or
      (LDirective = 'endif') or (LDirective = 'ifend') then
    begin
      raise ENyxSource.CreateAt('Conditional source needs an explicit customization writer',
        ASource, LTokens[LIndex].Position);
    end;

    if LTokens[LIndex].Kind <> tkComment then
    begin
      Continue;
    end;

    if Trim(LTokens[LIndex].Text) = '{ nyx:customizations-begin }' then
    begin

      if LBegin >= 0 then
      begin
        raise ENyxSource.CreateAt('Duplicate customization boundary', ASource, LTokens[LIndex].Position);
      end;
      LBegin := LIndex;
    end
    else if Trim(LTokens[LIndex].Text) = '{ nyx:customizations-end }' then
    begin

      if (LBegin < 0) or (LEnd >= 0) then
      begin
        raise ENyxSource.CreateAt('Unmatched customization boundary', ASource, LTokens[LIndex].Position);
      end;
      LEnd := LIndex;
    end
    else if (LBegin >= 0) and (LEnd < 0) and
      ((Copy(LTokens[LIndex].Text, 1, Length(CBlockPrefix)) = CBlockPrefix) or
      (Trim(LTokens[LIndex].Text) = '// nyx:customizations-statements-end')) then
    begin

      if LBlock >= 0 then
      begin
        LBodyEnd := LTokens[LIndex].Position;
        LBeforeBlocks[LBlock].Pascal := TrimRight(Copy(ASource,
          LBodyStart, LBodyEnd - LBodyStart)) + #10;
      end;

      if Trim(LTokens[LIndex].Text) = '// nyx:customizations-statements-end' then
      begin
        LBlock := -1;
        Continue;
      end;
      LIdentity := Lex(Copy(LTokens[LIndex].Text, Length(CBlockPrefix) + 1, MaxInt));

      if (Length(LIdentity) <> 2) or (LIdentity[0].Kind <> tkString) or
        (LIdentity[1].Kind <> tkString) or (Length(LBeforeBlocks) >= 4096) then
      begin
        raise ENyxSource.CreateAt('Malformed customization identity', ASource, LTokens[LIndex].Position);
      end;
      for LPrior := 0 to High(LBeforeBlocks) do
      begin

        if (LBeforeBlocks[LPrior].ControlID = LIdentity[0].Text) and
          (LBeforeBlocks[LPrior].PropertyName = LIdentity[1].Text) then
        begin
          raise ENyxSource.CreateAt('Duplicate customization identity', ASource, LTokens[LIndex].Position);
        end;
      end;
      LBlock := Length(LBeforeBlocks);
      SetLength(LBeforeBlocks, LBlock + 1);
      LBeforeBlocks[LBlock].ControlID := LIdentity[0].Text;
      LBeforeBlocks[LBlock].PropertyName := LIdentity[1].Text;
      LBodyStart := LTokens[LIndex].Finish;

      if (LBodyStart <= Length(ASource)) and (ASource[LBodyStart] = #13) then
      begin
        Inc(LBodyStart);
      end;

      if (LBodyStart <= Length(ASource)) and (ASource[LBodyStart] = #10) then
      begin
        Inc(LBodyStart);
      end;
    end;
  end;

  if (LBegin >= 0) <> (LEnd >= 0) then
  begin
    raise ENyxSource.CreateAt('Keep both customization boundaries', ASource, 1);
  end;
  LSource := ASource;
  LInsertion := 0;

  if LBegin >= 0 then
  begin
    LRegion := Copy(ASource, LTokens[LBegin].Position,
      LTokens[LEnd].Finish - LTokens[LBegin].Position);

    if LRegion <> CustomizationRegion(ABefore, LBeforeBlocks) then
    begin
      raise ENyxSource.CreateAt('Customization scaffolding changed; preserve or reconcile it explicitly',
        ASource, LTokens[LBegin].Position);
    end;

    if AValidateOnly then
    begin
      Exit(ASource);
    end;
    LInsertion := LTokens[LBegin].Position;
    LSource := Copy(ASource, 1, LInsertion - 1) +
      Copy(ASource, LTokens[LEnd].Finish, MaxInt);
  end
  else
  begin
    { Only one exported parameterless builder and one implementation are admitted.
      Recursion or other local references would change meaning when renaming;
      refuse them instead of routing them through an invented wrapper. }
    LCodeTokens := HandlerTokens(ASource);
    LBuilder := -1;
    LBuilderCount := 0;
    for LIndex := 0 to High(LCodeTokens) do
    begin

      if HandlerWord(LCodeTokens, LIndex, 'BuildNyxOriginalDocument') or
        HandlerWord(LCodeTokens, LIndex, 'CustomizeNyxDocument') then
      begin
        raise ENyxSource.CreateAt('Customization helper identity already exists', ASource,
          LCodeTokens[LIndex].Position);
      end;

      if not HandlerWord(LCodeTokens, LIndex, 'BuildNyxDocument') then
      begin
        Continue;
      end;
      Inc(LBuilderCount);

      if not HandlerWord(LCodeTokens, LIndex - 1, 'function') or
        not HandlerSymbol(LCodeTokens, LIndex + 1, ':') or
        not HandlerWord(LCodeTokens, LIndex + 2, 'TNyxDocument') or
        not HandlerSymbol(LCodeTokens, LIndex + 3, ';') then
      begin
        raise ENyxSource.CreateAt('Builder references/signatures need explicit reconciliation',
          ASource, LCodeTokens[LIndex].Position);
      end;
      LBuilder := LIndex;
    end;

    if (LBuilderCount <> 2) or (LBuilder < 0) then
    begin
      raise ENyxSource.CreateAt('Require one exported builder and one complete implementation', ASource, 1);
    end;
    LFinish := HandlerRoutineEnd(ASource, LCodeTokens, LBuilder - 1, 0, LHeader);
    HandlerNoDirectives(ASource, LCodeTokens[LBuilder - 1].Position, LCodeTokens[LFinish].Finish);

    if AValidateOnly then
    begin
      Exit(ASource);
    end;
    LSource := Copy(ASource, 1, LCodeTokens[LBuilder].Position - 1) +
      'BuildNyxOriginalDocument' + Copy(ASource, LCodeTokens[LBuilder].Finish, MaxInt);
    LInsertion := LCodeTokens[LFinish].Finish +
      Length('BuildNyxOriginalDocument') - Length(LCodeTokens[LBuilder].Text);
    LSource := Copy(LSource, 1, LInsertion - 1) + #10 + #10 + Copy(LSource, LInsertion, MaxInt);
    Inc(LInsertion, 2);
  end;
  { Replace by semantic identity, retaining every unrelated statement. Repeated
    edits never accumulate another wrapper or stale override for the same key. }
  SetLength(LBlocks, Length(LBeforeBlocks));
  for LIndex := 0 to High(LBeforeBlocks) do
  begin
    LBlocks[LIndex] := LBeforeBlocks[LIndex];
  end;
  for LIndex := 0 to High(LChanges) do
  begin
    LFound := False;
    for LPrior := 0 to High(LBlocks) do
    begin

      if (LBlocks[LPrior].ControlID = LChanges[LIndex].ControlID) and
        (LBlocks[LPrior].PropertyName = LChanges[LIndex].PropertyName) then
      begin
        LBlocks[LPrior] := LChanges[LIndex];
        LFound := True;
        Break;
      end;
    end;

    if not LFound then
    begin
      SetLength(LBlocks, Length(LBlocks) + 1);
      LBlocks[High(LBlocks)] := LChanges[LIndex];
    end;
  end;
  Result := Copy(LSource, 1, LInsertion - 1) + CustomizationRegion(AAfter, LBlocks) +
    Copy(LSource, LInsertion, MaxInt);
  { Add only missing implementation imports, retaining the public signature and
    existing declarations/comments.
    The ordinary generator owns the complete fluent API dependency closure. }
  LGeneratedImports := ReadNyxImports(TNyxCodegen.Generate(AAfter), nisInterface);
  for LIndex := 0 to LGeneratedImports.Count - 1 do
  begin
    LImports := ReadNyxImports(Result, nisInterface);
    LImplementationImports := ReadNyxImports(Result, nisImplementation);
    LFound := False;
    for LPrior := 0 to LImports.Count - 1 do
    begin

      if SameText(LImports.UnitAt(LPrior).Name, LGeneratedImports.UnitAt(LIndex).Name) then
      begin
        LFound := True;
        Break;
      end;
    end;
    for LPrior := 0 to LImplementationImports.Count - 1 do
    begin

      if SameText(LImplementationImports.UnitAt(LPrior).Name, LGeneratedImports.UnitAt(LIndex).Name) then
      begin
        LFound := True;
        Break;
      end;
    end;

    if not LFound then
    begin
      Result := EditNyxImport(Result, nisImplementation, niaAdd, LGeneratedImports.UnitAt(LIndex));
    end;
  end;
  NyxCompanionUnitName(Result);
end;
