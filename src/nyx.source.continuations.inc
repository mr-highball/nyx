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

{ Included by nyx.source. Structural stages have separate Pascal scopes so a
  later removal/recreation cannot invalidate earlier locals, helpers or computed
  values. Freeze completed tree, state and binding stages verbatim; merge only
  the latest scalar-property stage. The retained wire marker 'tree' identifies
  these ordered stages. The sole exported builder runs the retained base once,
  then stages in order. This is source preparation, never execution authority. }

const
  CContinuationBegin: TNyxText = '{ nyx:continuation-begin }';
  CContinuationEnd: TNyxText = '{ nyx:continuation-end }';
  CContinuationHeader: TNyxText = '// nyx:continuation ';
  CContinuationBase: TNyxText = 'BuildNyxContinuationBase';
  CContinuationMaximum = 1024;

type
  TNyxSourceContinuation = record
    Defined: Boolean;
    Start: Integer;
    Finish: Integer;
    Ordinal: Integer;
    Tree: Boolean;
    Blocks: TNyxCustomizationBlocks;
  end;

function ContinuationName(AOrdinal: Integer): TNyxText;
begin
  Result := 'CustomizeNyxContinuation' + TNyxText(IntToStr(AOrdinal));
end;

function ContinuationRoutine(ADocument: TNyxDocument;
  const ABlocks: TNyxCustomizationBlocks; AOrdinal: Integer): TNyxText;
const
  CHeader: TNyxText = 'procedure CustomizeNyxDocument(ADocument: TNyxDocument);';
begin
  Result := TNyxCodegen.CustomizationRoutine(ADocument, ABlocks);

  if Copy(Result, 1, Length(CHeader)) <> CHeader then
  begin
    raise ENyxModel.Create('Customization emitter did not supply its exact routine header');
  end;
  { Splice the admitted ASCII header in TNyxText. Native ANSI RTL replacement
    would convert supplementary source text before it reaches the compiler. }
  Result := 'procedure ' + ContinuationName(AOrdinal) + '(ADocument: TNyxDocument);' +
    Copy(Result, Length(CHeader) + 1, MaxInt);
end;

function ContinuationRegion(ADocument: TNyxDocument;
  const ABlocks: TNyxCustomizationBlocks; AOrdinal: Integer; ATree: Boolean): TNyxText;
var
  LIndex: Integer;
  LKind: TNyxText;
begin
  LKind := 'properties';

  if ATree then
  begin
    LKind := 'tree';
  end;
  Result := CContinuationBegin + #10 + CContinuationHeader + '''' +
    TNyxText(IntToStr(AOrdinal)) + ''' ''' + LKind + '''' + #10 +
    '{ Earlier steps retain their handwritten Pascal and independent local scopes. }' + #10 +
    ContinuationRoutine(ADocument, ABlocks, AOrdinal) + #10 +
    'function BuildNyxDocument: TNyxDocument;' + #10 +
    'begin' + #10 +
    '  Result := ' + CContinuationBase + ';' + #10 +
    '  try' + #10 + #10 +
    '    if Result = nil then' + #10 +
    '    begin' + #10 +
    '      raise ENyxModel.Create(''The original builder returned no document'');' + #10 +
    '    end;' + #10;
  for LIndex := 1 to AOrdinal do
  begin
    Result := Result + '    ' + ContinuationName(LIndex) + '(Result);' + #10;
  end;
  Result := Result +
    '  except' + #10 +
    '    Result.Free;' + #10 +
    '    Result := nil;' + #10 +
    '    raise;' + #10 +
    '  end;' + #10 +
    'end;' + #10 + CContinuationEnd;
end;

function ReadContinuation(const ASource: TNyxText): TNyxSourceContinuation;
const
  CBlockPrefix: TNyxText = '// nyx:customize ';
var
  LTokens: TTokens;
  LValues: TTokens;
  LIndex: Integer;
  LPrior: Integer;
  LBlock: Integer;
  LBody: Integer;
  LHeader: Boolean;
  LInside: Boolean;
  LDirective: TNyxText;
begin
  Result := Default(TNyxSourceContinuation);
  LTokens := Lex(ASource);
  LInside := False;
  LHeader := False;
  LBlock := -1;
  LBody := 0;
  for LIndex := 0 to High(LTokens) do
  begin
    LDirective := ImportDirective(LTokens[LIndex]);

    if (LDirective = 'if') or (LDirective = 'ifdef') or (LDirective = 'ifndef') or
      (LDirective = 'ifopt') or (LDirective = 'else') or (LDirective = 'elseif') or
      (LDirective = 'endif') or (LDirective = 'ifend') then
    begin
      raise ENyxSource.CreateAt('Conditional source needs explicit continuation reconciliation',
        ASource, LTokens[LIndex].Position);
    end;

    if LTokens[LIndex].Kind <> tkComment then
    begin
      Continue;
    end;

    if Trim(LTokens[LIndex].Text) = CContinuationBegin then
    begin

      if Result.Defined then
      begin
        raise ENyxSource.CreateAt('Duplicate active continuation boundary',
          ASource, LTokens[LIndex].Position);
      end;
      Result.Defined := True;
      Result.Start := LTokens[LIndex].Position;
      LInside := True;
      Continue;
    end;

    if Trim(LTokens[LIndex].Text) = CContinuationEnd then
    begin

      if not LInside or not LHeader or (LBlock >= 0) then
      begin
        raise ENyxSource.CreateAt('Unmatched continuation boundary',
          ASource, LTokens[LIndex].Position);
      end;
      Result.Finish := LTokens[LIndex].Finish;
      LInside := False;
      Continue;
    end;

    if not LInside then
    begin
      Continue;
    end;

    if Copy(LTokens[LIndex].Text, 1, Length(CContinuationHeader)) = CContinuationHeader then
    begin
      LValues := Lex(Copy(LTokens[LIndex].Text, Length(CContinuationHeader) + 1, MaxInt));

      if LHeader or (Length(LValues) <> 2) then
      begin
        raise ENyxSource.CreateAt('Invalid continuation identity', ASource, LTokens[LIndex].Position);
      end;

      if (LValues[0].Kind <> tkString) or (LValues[1].Kind <> tkString) or
        not TryNyxInteger(LValues[0].Text, Result.Ordinal) or (Result.Ordinal < 1) or
        (Result.Ordinal > CContinuationMaximum) or
        not ((LValues[1].Text = 'tree') or (LValues[1].Text = 'properties')) then
      begin
        raise ENyxSource.CreateAt('Invalid continuation identity', ASource, LTokens[LIndex].Position);
      end;
      Result.Tree := LValues[1].Text = 'tree';
      LHeader := True;
    end
    else if (Copy(LTokens[LIndex].Text, 1, Length(CBlockPrefix)) = CBlockPrefix) or
      (Trim(LTokens[LIndex].Text) = '// nyx:customizations-statements-end') then
    begin

      if LBlock >= 0 then
      begin
        Result.Blocks[LBlock].Pascal := TrimRight(Copy(ASource,
          LBody, LTokens[LIndex].Position - LBody)) + #10;
      end;

      if Trim(LTokens[LIndex].Text) = '// nyx:customizations-statements-end' then
      begin
        LBlock := -1;
        Continue;
      end;
      LValues := Lex(Copy(LTokens[LIndex].Text, Length(CBlockPrefix) + 1, MaxInt));

      if (Length(LValues) <> 2) or (Length(Result.Blocks) >= 4096) then
      begin
        raise ENyxSource.CreateAt('Invalid continuation statement identity',
          ASource, LTokens[LIndex].Position);
      end;

      if (LValues[0].Kind <> tkString) or (LValues[1].Kind <> tkString) then
      begin
        raise ENyxSource.CreateAt('Invalid continuation statement identity',
          ASource, LTokens[LIndex].Position);
      end;
      for LPrior := 0 to High(Result.Blocks) do
      begin

        if (Result.Blocks[LPrior].ControlID = LValues[0].Text) and
          (Result.Blocks[LPrior].PropertyName = LValues[1].Text) then
        begin
          raise ENyxSource.CreateAt('Duplicate continuation statement',
            ASource, LTokens[LIndex].Position);
        end;
      end;
      LBlock := Length(Result.Blocks);
      SetLength(Result.Blocks, LBlock + 1);
      Result.Blocks[LBlock].ControlID := LValues[0].Text;
      Result.Blocks[LBlock].PropertyName := LValues[1].Text;
      LBody := LTokens[LIndex].Finish;

      if (LBody <= Length(ASource)) and (ASource[LBody] = #13) then
      begin
        Inc(LBody);
      end;

      if (LBody <= Length(ASource)) and (ASource[LBody] = #10) then
      begin
        Inc(LBody);
      end;
    end;
  end;

  if LInside or (Result.Defined and (Result.Finish = 0)) then
  begin
    raise ENyxSource.CreateAt('Keep both continuation boundaries', ASource, 1);
  end;
end;

function ContinuedSource(const ASource: TNyxText; ABefore, AAfter: TNyxDocument;
  const AChanges: TNyxCustomizationBlocks; const ACurrent: TNyxSourceContinuation;
  ATree: Boolean): TNyxText;
var
  LCode: TTokens;
  LIndex: Integer;
  LPrior: Integer;
  LBuilder: Integer;
  LBuilderCount: Integer;
  LFinish: Integer;
  LHeader: Integer;
  LInsertion: Integer;
  LOrdinal: Integer;
  LName: TNyxText;
  LPrefix: TNyxText;
  LSource: TNyxText;
  LBlocks: TNyxCustomizationBlocks;
  LFound: Boolean;
  LImports: TNyxImportClause;

  procedure IncludeUnit(const AUnit: TNyxPascalUnitRef);
  var
    LClause: TNyxImportClause;
    LSection: TNyxImportSection;
    LItem: Integer;
  begin
    for LSection := Low(TNyxImportSection) to High(TNyxImportSection) do
    begin
      LClause := ReadNyxImports(Result, LSection);
      for LItem := 0 to LClause.Count - 1 do
      begin

        if SameText(LClause.UnitAt(LItem).Name, AUnit.Name) then
        begin
          Exit;
        end;
      end;
    end;
    Result := EditNyxImport(Result, nisImplementation, niaAdd, AUnit);
  end;

begin
  LPrefix := '';
  LOrdinal := 1;
  LSource := ASource;
  LBlocks := nil;

  if ACurrent.Defined then
  begin

    if Copy(ASource, ACurrent.Start, ACurrent.Finish - ACurrent.Start) <>
      ContinuationRegion(ABefore, ACurrent.Blocks, ACurrent.Ordinal, ACurrent.Tree) then
    begin
      raise ENyxSource.CreateAt('Continuation scaffolding changed; reconcile it explicitly',
        ASource, ACurrent.Start);
    end;
    LInsertion := ACurrent.Start;
    LSource := Copy(ASource, 1, LInsertion - 1) + Copy(ASource, ACurrent.Finish, MaxInt);
    LOrdinal := ACurrent.Ordinal;

    if ATree or ACurrent.Tree then
    begin
      { A later tree/state/binding change must keep the earlier order and its own
        local types, including controls removed/recreated in subsequent steps. }
      LPrefix := ContinuationRoutine(ABefore, ACurrent.Blocks, ACurrent.Ordinal) + #10;
      Inc(LOrdinal);
    end
    else
    begin
      SetLength(LBlocks, Length(ACurrent.Blocks));
      for LIndex := 0 to High(LBlocks) do
      begin
        LBlocks[LIndex] := ACurrent.Blocks[LIndex];
      end;
    end;
  end
  else
  begin
    CustomizeNyxPropertySource(ASource, ABefore, ABefore, True);
    LCode := HandlerTokens(ASource);
    LBuilder := -1;
    LBuilderCount := 0;
    for LIndex := 0 to High(LCode) do
    begin

      if HandlerWord(LCode, LIndex, CContinuationBase) or
        HandlerWord(LCode, LIndex, ContinuationName(1)) then
      begin
        raise ENyxSource.CreateAt('Continuation helper identity already exists',
          ASource, LCode[LIndex].Position);
      end;

      if HandlerWord(LCode, LIndex, 'BuildNyxDocument') then
      begin
        Inc(LBuilderCount);

        if not HandlerWord(LCode, LIndex - 1, 'function') or
          not HandlerSymbol(LCode, LIndex + 1, ':') or
          not HandlerWord(LCode, LIndex + 2, 'TNyxDocument') or
          not HandlerSymbol(LCode, LIndex + 3, ';') then
        begin
          raise ENyxSource.CreateAt('Builder references need explicit continuation reconciliation',
            ASource, LCode[LIndex].Position);
        end;
        LBuilder := LIndex;
      end;
    end;

    if (LBuilderCount <> 2) or (LBuilder < 0) then
    begin
      raise ENyxSource.CreateAt('Require one exported builder and implementation', ASource, 1);
    end;
    LFinish := HandlerRoutineEnd(ASource, LCode, LBuilder - 1, 0, LHeader);
    LInsertion := LCode[LFinish].Finish + Length(CContinuationBase) -
      Length(LCode[LBuilder].Text);
    LSource := Copy(ASource, 1, LCode[LBuilder].Position - 1) + CContinuationBase +
      Copy(ASource, LCode[LBuilder].Finish, MaxInt);
    LPrefix := #10 + #10;
  end;

  if LOrdinal > CContinuationMaximum then
  begin
    raise ENyxSource.CreateAt('Continuation capacity requires source consolidation', ASource, 1);
  end;
  LName := ContinuationName(LOrdinal);

  if not ACurrent.Defined or (LOrdinal <> ACurrent.Ordinal) then
  begin
    LCode := HandlerTokens(LSource);
    for LIndex := 0 to High(LCode) do
    begin

      if HandlerWord(LCode, LIndex, LName) then
      begin
        raise ENyxSource.CreateAt('Continuation helper identity already exists',
          ASource, LCode[LIndex].Position);
      end;
    end;
  end;
  for LIndex := 0 to High(AChanges) do
  begin
    LFound := False;
    for LPrior := 0 to High(LBlocks) do
    begin

      if (LBlocks[LPrior].ControlID = AChanges[LIndex].ControlID) and
        (LBlocks[LPrior].PropertyName = AChanges[LIndex].PropertyName) then
      begin
        LBlocks[LPrior] := AChanges[LIndex];
        LFound := True;
        Break;
      end;
    end;

    if not LFound then
    begin
      SetLength(LBlocks, Length(LBlocks) + 1);
      LBlocks[High(LBlocks)] := AChanges[LIndex];
    end;
  end;

  if Length(LBlocks) > 4096 then
  begin
    raise ENyxSource.CreateAt('Continuation statement capacity exceeded', ASource, 1);
  end;
  Result := Copy(LSource, 1, LInsertion - 1) + LPrefix +
    ContinuationRegion(AAfter, LBlocks, LOrdinal, ATree) +
    Copy(LSource, LInsertion, MaxInt);
  LImports := ReadNyxImports(TNyxCodegen.Generate(AAfter), nisInterface);
  for LIndex := 0 to LImports.Count - 1 do
  begin
    IncludeUnit(LImports.UnitAt(LIndex));
  end;
  IncludeUnit(NyxPascalUnit('nyx.root.types'));
  NyxCompanionUnitName(Result);
end;

function CustomizeNyxExecutedSource(const ASource: TNyxText;
  ABefore, AAfter: TNyxDocument): TNyxText;
var
  LChanges: TNyxCustomizationBlocks;
  LCurrent: TNyxSourceContinuation;
  LIndex: Integer;
  LTree: Boolean;
begin
  NyxCompanionUnitName(ASource);
  LChanges := TNyxCodegen.CustomizationChanges(ABefore, AAfter);

  if Length(LChanges) = 0 then
  begin
    Exit(ASource);
  end;
  LTree := False;
  for LIndex := 0 to High(LChanges) do
  begin

    if (LChanges[LIndex].PropertyName = NyxCustomizationConstructKey) or
      (LChanges[LIndex].PropertyName = NyxCustomizationTopologyKey) or
      (LChanges[LIndex].PropertyName = NyxCustomizationRemoveKey) or
      (Copy(LChanges[LIndex].PropertyName, 1, Length(NyxCustomizationStatePrefix)) =
        NyxCustomizationStatePrefix) or
      (Copy(LChanges[LIndex].PropertyName, 1, Length(NyxCustomizationBindingPrefix)) =
        NyxCustomizationBindingPrefix) then
    begin
      LTree := True;
    end;
  end;
  LCurrent := ReadContinuation(ASource);

  if LTree or LCurrent.Defined then
  begin
    Result := ContinuedSource(ASource, ABefore, AAfter, LChanges, LCurrent, LTree);
  end
  else
  begin
    Result := CustomizeNyxPropertySource(ASource, ABefore, AAfter);
  end;
end;
