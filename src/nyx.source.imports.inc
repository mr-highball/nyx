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



{ Included by nyx.source. Reuse its admitted Unicode lexer; exact token offsets
  only live with the source being edited. Normal comments are never deleted. }

function NyxPascalUnit(const AName: TNyxText): TNyxPascalUnitRef;
var
  LIndex: Integer;
begin
  for LIndex := 1 to Length(AName) do
  begin

    if not (AName[LIndex] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '.']) then
    begin
      raise ENyxModel.Create('Pascal imports require an ASCII unit namespace');
    end;
  end;
  TNyxCodegen.AdmitUnitName(AName);
  Result.FName := AName;
end;

function TNyxImportClause.GetCount: Integer;
begin
  Result := Length(FEntries);
end;

function TNyxImportClause.UnitAt(AIndex: Integer): TNyxPascalUnitRef;
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ENyxModel.Create('Import index is outside this clause');
  end;
  Result := FEntries[AIndex].FUnit;
end;

function TNyxImportClause.LineAt(AIndex: Integer): Integer;
begin
  UnitAt(AIndex);
  Result := FEntries[AIndex].FLine;
end;

function ImportDirective(const AToken: TToken): TNyxText;
var
  LText: TNyxText;
  LIndex: Integer;
begin
  Result := '';

  if AToken.Kind <> tkComment then
  begin
    Exit;
  end;
  LText := AToken.Text;

  if Copy(LText, 1, 2) = '{$' then
  begin
    LText := Copy(LText, 3, Length(LText) - 3);
  end
  else if Copy(LText, 1, 3) = '(*$' then
  begin
    LText := Copy(LText, 4, Length(LText) - 5);
  end
  else
  begin
    Exit;
  end;
  LText := LowerCase(Trim(LText));
  LIndex := 1;
  while (LIndex <= Length(LText)) and (LText[LIndex] in ['a'..'z']) do
  begin
    Inc(LIndex);
  end;
  Result := Copy(LText, 1, LIndex - 1);

  if Result = '' then
  begin
    Result := '?';
  end;
end;

function ImportSpan(const AToken: TToken): TNyxImportSpan;
begin
  Result.FStart := AToken.Position;
  Result.FFinish := AToken.Finish;
end;

function ReadNyxImports(const ASource: TNyxText;
  ASection: TNyxImportSection): TNyxImportClause;
var
  LTokens: TTokens;
  LSectionName: TNyxText;
  LName: TNyxText;
  LDirective: TNyxText;
  LIndex: Integer;
  LEntry: Integer;
  LPrior: Integer;
  LPart: Integer;
  LConditions: Integer;
  LLine: Integer;
  LLineCursor: Integer;
  LFound: Boolean;

  procedure SkipComments;
  begin
    while (LIndex < Length(LTokens)) and (LTokens[LIndex].Kind = tkComment) do
    begin

      if ImportDirective(LTokens[LIndex]) <> '' then
      begin
        HandlerFail(ASource, 'Import directives require ordinary Pascal editing', LTokens, LIndex);
      end;
      Inc(LIndex);
    end;
  end;

begin
  Result := Default(TNyxImportClause);
  LSectionName := 'interface';

  if ASection = nisImplementation then
  begin
    LSectionName := 'implementation';
  end
  else if ASection <> nisInterface then
  begin
    raise ENyxModel.Create('Unknown Pascal import section');
  end;
  NyxCompanionUnitName(ASource);
  LTokens := Lex(ASource);
  LFound := False;
  LConditions := 0;
  LIndex := 0;
  while LIndex < Length(LTokens) do
  begin
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
      HandlerFail(ASource, 'Import conditional ownership is unbalanced', LTokens, LIndex);
    end;

    if HandlerWord(LTokens, LIndex, LSectionName) then
    begin

      if LConditions <> 0 then
      begin
        HandlerFail(ASource, 'Conditional import sections require ordinary Pascal editing',
          LTokens, LIndex);
      end;
      Result.FAnchor := LTokens[LIndex].Finish;
      Inc(LIndex);
      LFound := True;
      Break;
    end;
    Inc(LIndex);
  end;

  if not LFound then
  begin
    raise ENyxSource.CreateAt('The exact Pascal import section is missing', ASource, 1);
  end;
  SkipComments;

  if not HandlerWord(LTokens, LIndex, 'uses') then
  begin
    Exit;
  end;
  Result.FUses := ImportSpan(LTokens[LIndex]);
  Inc(LIndex);
  LLineCursor := 1;
  LLine := 1;
  repeat
    SkipComments;
    LEntry := Length(Result.FEntries);

    if LEntry >= 256 then
    begin
      HandlerFail(ASource, 'Import clause exceeds 256 namespaces', LTokens, LIndex);
    end;
    SetLength(Result.FEntries, LEntry + 1);
    LName := '';
    repeat

      if (LIndex >= Length(LTokens)) or (LTokens[LIndex].Kind <> tkWord) then
      begin
        HandlerFail(ASource, 'Import requires an ordinary Pascal namespace', LTokens, LIndex);
      end;
      while LLineCursor < LTokens[LIndex].Position do
      begin

        if ASource[LLineCursor] = #10 then
        begin
          Inc(LLine);
        end;
        Inc(LLineCursor);
      end;

      if LName = '' then
      begin
        Result.FEntries[LEntry].FLine := LLine;
      end;
      LName := LName + LTokens[LIndex].Text;
      LPart := Length(Result.FEntries[LEntry].FParts);
      SetLength(Result.FEntries[LEntry].FParts, LPart + 1);
      Result.FEntries[LEntry].FParts[LPart] := ImportSpan(LTokens[LIndex]);
      Inc(LIndex);
      SkipComments;

      if not HandlerSymbol(LTokens, LIndex, '.') then
      begin
        Break;
      end;
      LName := LName + '.';
      Inc(LPart);
      SetLength(Result.FEntries[LEntry].FParts, LPart + 1);
      Result.FEntries[LEntry].FParts[LPart] := ImportSpan(LTokens[LIndex]);
      Inc(LIndex);
      SkipComments;
    until False;
    Result.FEntries[LEntry].FUnit := NyxPascalUnit(LName);
    for LPrior := 0 to LEntry - 1 do
    begin

      if SameText(Result.UnitAt(LPrior).Name, LName) then
      begin
        HandlerFail(ASource, 'Duplicate Pascal import namespace', LTokens, LIndex);
      end;
    end;

    if HandlerSymbol(LTokens, LIndex, ';') then
    begin
      Result.FEnd := ImportSpan(LTokens[LIndex]);
      Break;
    end;

    if not HandlerSymbol(LTokens, LIndex, ',') then
    begin
      HandlerFail(ASource, 'Imports require comma separators and a closing semicolon',
        LTokens, LIndex);
    end;
    SetLength(Result.FCommas, LEntry + 1);
    Result.FCommas[LEntry] := ImportSpan(LTokens[LIndex]);
    Inc(LIndex);
  until False;
end;

function EditNyxImport(const ASource: TNyxText; ASection: TNyxImportSection;
  AAction: TNyxImportAction; const AUnit: TNyxPascalUnitRef): TNyxText;
var
  LClause: TNyxImportClause;
  LVerified: TNyxImportClause;
  LUnit: TNyxPascalUnitRef;
  LIndex: Integer;
  LFound: Integer;
  LParts: Integer;
  LCount: Integer;
  LSeparator: TNyxImportSpan;
  LWhitespace: Boolean;
  LCursor: Integer;

  procedure RemoveSpan(const ASpan: TNyxImportSpan);
  begin
    Result := JoinSourceFrame([Copy(Result, 1, ASpan.FStart - 1),
      Copy(Result, ASpan.FFinish, MaxInt)]);
  end;

  procedure InsertAt(APosition: Integer; const AText: TNyxText);
  begin
    Result := JoinSourceFrame([Copy(Result, 1, APosition - 1), AText,
      Copy(Result, APosition, MaxInt)]);
  end;

begin
  LUnit := NyxPascalUnit(AUnit.Name);
  LClause := ReadNyxImports(ASource, ASection);
  LFound := -1;
  for LIndex := 0 to LClause.Count - 1 do
  begin

    if SameText(LClause.UnitAt(LIndex).Name, LUnit.Name) then
    begin
      LFound := LIndex;
      Break;
    end;
  end;
  Result := ASource;
  LCount := LClause.Count;
  case Ord(AAction) of
    Ord(niaAdd):
      begin

        if LFound >= 0 then
        begin
          raise ENyxModel.Create('The import already exists in this exact section');
        end;

        if LClause.Count = 0 then
        begin
          InsertAt(LClause.FAnchor, #10 + #10 + 'uses' + #10 + '  ' + LUnit.Name + ';');
        end
        else
        begin
          { Descending offsets keep the prior line comment attached to its unit.
            All prior whitespace/comments are retained, including before ;. }
          InsertAt(LClause.FEnd.FStart, #10 + '  ' + LUnit.Name);
          LParts := High(LClause.FEntries[High(LClause.FEntries)].FParts);
          InsertAt(LClause.FEntries[High(LClause.FEntries)].FParts[LParts].FFinish, ',');
        end;
        Inc(LCount);
      end;
    Ord(niaRemove):
      begin

        if LFound < 0 then
        begin
          raise ENyxModel.Create('The import is missing from this exact section');
        end;

        if LClause.Count = 1 then
        begin
          RemoveSpan(LClause.FEnd);
        end
        else if LFound < LClause.Count - 1 then
        begin
          RemoveSpan(LClause.FCommas[LFound]);
        end;
        for LParts := High(LClause.FEntries[LFound].FParts) downto 0 do
        begin
          RemoveSpan(LClause.FEntries[LFound].FParts[LParts]);
        end;

        if LClause.Count = 1 then
        begin
          RemoveSpan(LClause.FUses);
        end
        else if LFound = LClause.Count - 1 then
        begin
          LSeparator := LClause.FCommas[LFound - 1];
          LWhitespace := True;
          for LCursor := LSeparator.FFinish to LClause.FEntries[LFound].FParts[0].FStart - 1 do
          begin

            if not (ASource[LCursor] in [#9, #10, #13, ' ']) then
            begin
              LWhitespace := False;
              Break;
            end;
          end;

          if LWhitespace then
          begin
            { Retire separator indentation with a removed final import. This
              restores a normal Name; line. Comments always retain exact bytes. }
            LSeparator.FFinish := LClause.FEntries[LFound].FParts[0].FStart;
          end;
          RemoveSpan(LSeparator);
        end;
        Dec(LCount);
      end;
  else
    begin
      raise ENyxModel.Create('Unknown Pascal import action');
    end;
  end;
  LVerified := ReadNyxImports(Result, ASection);

  if LVerified.Count <> LCount then
  begin
    raise ENyxModel.Create('Import edit did not preserve its exact lexical clause');
  end;
end;
