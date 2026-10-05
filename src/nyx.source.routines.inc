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

{ General routine discovery shares the callback lexer/block balancer. It does
  not interpret expressions, execute source or guess conditional ownership. }

function NyxRoutine(const AName: TNyxText): TNyxRoutineRef;
var
  LIndex: Integer;
  LFirst: Boolean;
begin

  if (AName = '') or (Length(AName) > 120) then
  begin
    raise ENyxModel.Create('Routine names require 1..120 ASCII characters');
  end;
  LFirst := True;
  for LIndex := 1 to Length(AName) do
  begin

    if AName[LIndex] = '.' then
    begin

      if LFirst then
      begin
        raise ENyxModel.Create('Routine names require complete qualified identifiers');
      end;
      LFirst := True;
    end
    else
    begin

      if not (AName[LIndex] in ['A'..'Z', 'a'..'z', '_', '0'..'9']) or
        (LFirst and (AName[LIndex] in ['0'..'9'])) then
      begin
        raise ENyxModel.Create('Routine names require ASCII Pascal identifiers');
      end;
      LFirst := False;
    end;
  end;

  if LFirst then
  begin
    raise ENyxModel.Create('Routine names cannot end with a qualification dot');
  end;
  Result.FName := AName;
end;

function TNyxRoutineCatalog.GetCount: Integer;
begin
  Result := Length(FEntries);
end;

function TNyxRoutineCatalog.Item(AIndex: Integer): TNyxRoutineSource;
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ENyxModel.Create('Routine index is outside the accepted catalog');
  end;
  Result := FEntries[AIndex];
end;

function RoutineOpening(const ATokens: TTokens; AIndex: Integer): Boolean;
begin
  Result := HandlerWord(ATokens, AIndex, 'procedure') or
    HandlerWord(ATokens, AIndex, 'function') or
    HandlerWord(ATokens, AIndex, 'constructor') or
    HandlerWord(ATokens, AIndex, 'destructor');
end;

function RoutineKind(const ATokens: TTokens; AIndex: Integer): TNyxRoutineKind;
begin
  Result := nrProcedure;

  if HandlerWord(ATokens, AIndex, 'function') then
  begin
    Result := nrFunction;
  end
  else if HandlerWord(ATokens, AIndex, 'constructor') then
  begin
    Result := nrConstructor;
  end
  else if HandlerWord(ATokens, AIndex, 'destructor') then
  begin
    Result := nrDestructor;
  end;
end;

function ReadNyxRoutines(const ASource: TNyxText): TNyxRoutineCatalog;
var
  LTokens: TTokens;
  LPrefix: TNyxText;
  LBuilder: TNyxText;
  LSuffix: TNyxText;
  LName: TNyxText;
  LIndex: Integer;
  LStart: Integer;
  LFinish: Integer;
  LHeader: Integer;
  LCursor: Integer;
  LTypes: Integer;
  LCount: Integer;
  LPrior: Integer;
  LLine: Integer;
  LLineCursor: Integer;
  LEntry: TNyxRoutineSource;
begin
  Result := Default(TNyxRoutineCatalog);
  NyxCompanionUnitName(ASource);
  SplitNyxSourceFrame(ASource, LPrefix, LBuilder, LSuffix);
  LTokens := HandlerTokens(ASource);
  LIndex := 0;
  while (LIndex < Length(LTokens)) and not HandlerWord(LTokens, LIndex, 'implementation') do
  begin
    Inc(LIndex);
  end;

  if LIndex = Length(LTokens) then
  begin
    HandlerFail(ASource, 'Routine discovery requires a companion implementation section', LTokens, LIndex);
  end;
  Inc(LIndex);
  LTypes := 0;
  LCount := 0;
  LLine := 1;
  LLineCursor := 1;
  while LIndex < Length(LTokens) do
  begin

    if (LTypes = 0) and (HandlerWord(LTokens, LIndex, 'initialization') or
      HandlerWord(LTokens, LIndex, 'finalization') or HandlerWord(LTokens, LIndex, 'end')) then
    begin
      Break;
    end;

    if HandlerTypeBlock(LTokens, LIndex) then
    begin
      Inc(LTypes);
    end
    else if (LTypes > 0) and HandlerWord(LTokens, LIndex, 'end') then
    begin
      Dec(LTypes);
    end
    else if (LTypes = 0) and RoutineOpening(LTokens, LIndex) and
      (HandlerSymbol(LTokens, LIndex - 1, ';') or
      HandlerWord(LTokens, LIndex - 1, 'implementation') or
      HandlerWord(LTokens, LIndex - 1, 'class')) then
    begin
      LStart := LIndex;

      if HandlerWord(LTokens, LIndex - 1, 'class') then
      begin
        Dec(LStart);
      end;
      LCursor := LIndex + 1;
      LName := '';
      while LCursor < Length(LTokens) do
      begin

        { Escaping a reserved identifier does not change Pascal identity. Keep
          its authored ampersand in the immutable signature, not the reference. }

        if HandlerSymbol(LTokens, LCursor, '&') then
        begin
          Inc(LCursor);
        end;

        if (LCursor >= Length(LTokens)) or (LTokens[LCursor].Kind <> tkWord) then
        begin
          HandlerFail(ASource, 'Routine identity has ambiguous qualification', LTokens, LCursor);
        end;
        LName := LName + LTokens[LCursor].Text;
        Inc(LCursor);

        if not HandlerSymbol(LTokens, LCursor, '.') then
        begin
          Break;
        end;
        LName := LName + '.';
        Inc(LCursor);
      end;
      LEntry := Default(TNyxRoutineSource);
      LEntry.FRoutine := NyxRoutine(LName);
      LEntry.FKind := RoutineKind(LTokens, LIndex);
      { Nesting=1 lets the same balancer locate forward/external declarations.
        They are discovery-only, never editable implementations. }
      LFinish := HandlerRoutineEnd(ASource, LTokens, LIndex, 1, LHeader);
      LEntry.FStart := LTokens[LStart].Position;
      LEntry.FHeaderFinish := LTokens[LHeader].Finish;
      LEntry.FFinish := LTokens[LFinish].Finish;
      LEntry.FSignature := Copy(ASource, LEntry.FStart,
        LEntry.FHeaderFinish - LEntry.FStart);
      LEntry.FImplementation := Copy(ASource, LEntry.FHeaderFinish,
        LEntry.FFinish - LEntry.FHeaderFinish);
      while LLineCursor < LEntry.FStart do
      begin

        if ASource[LLineCursor] = #10 then
        begin
          Inc(LLine);
        end;
        Inc(LLineCursor);
      end;
      LEntry.FLine := LLine;
      LEntry.FEditable := True;

      if (LEntry.FStart <= Length(LPrefix) + Length(LBuilder)) and
        (LEntry.FFinish > Length(LPrefix)) then
      begin
        LEntry.FReason := 'Managed views belong to the design contract';
      end
      else if SameText(LName, 'BuildNyxDocument') or SameText(LName, 'BindNyxCallbacks') then
      begin
        LEntry.FReason := 'Compiler-managed companion infrastructure';
      end
      else if HandlerWord(LTokens, LHeader + 1, 'forward') or
        HandlerWord(LTokens, LHeader + 1, 'external') then
      begin
        LEntry.FReason := 'No local implementation for this declaration';
      end
      else
      begin
        try
          HandlerNoDirectives(ASource, LEntry.FStart, LEntry.FFinish);
        except
          on LException: ENyxSource do
          begin
            LEntry.FReason := TNyxText(LException.Message);
          end;
        end;
      end;
      LEntry.FEditable := LEntry.FReason = '';

      if LCount = 4096 then
      begin
        HandlerFail(ASource, 'Routine discovery exceeds 4096 entries', LTokens, LIndex);
      end;
      SetLength(Result.FEntries, LCount + 1);
      Result.FEntries[LCount] := LEntry;
      for LPrior := 0 to LCount - 1 do
      begin

        if SameText(Result.FEntries[LPrior].Routine.Name, LName) then
        begin
          Result.FEntries[LPrior].FEditable := False;
          Result.FEntries[LPrior].FReason := 'Overloaded or duplicate routine identity';
          Result.FEntries[LCount].FEditable := False;
          Result.FEntries[LCount].FReason := 'Overloaded or duplicate routine identity';
        end;
      end;
      Inc(LCount);
      LIndex := LFinish + 1;
      Continue;
    end;
    Inc(LIndex);
  end;
end;

function ReadNyxRoutineSource(const ASource: TNyxText;
  const ARoutine: TNyxRoutineRef): TNyxRoutineSource;
var
  LCatalog: TNyxRoutineCatalog;
  LReference: TNyxRoutineRef;
  LIndex: Integer;
  LFound: Boolean;
begin
  Result := Default(TNyxRoutineSource);
  LReference := NyxRoutine(ARoutine.Name);
  LCatalog := ReadNyxRoutines(ASource);
  LFound := False;
  for LIndex := 0 to LCatalog.Count - 1 do
  begin

    if SameText(LCatalog.Item(LIndex).Routine.Name, LReference.Name) then
    begin

      if LFound then
      begin
        raise ENyxSource.CreateAt('Routine identity is overloaded or ambiguous', ASource, 1);
      end;
      Result := LCatalog.Item(LIndex);
      LFound := True;
    end;
  end;

  if not LFound then
  begin
    raise ENyxSource.CreateAt('Routine implementation is absent from the accepted companion', ASource, 1);
  end;
end;

function ReplaceNyxRoutineImplementation(const ASource: TNyxText;
  const ARoutine: TNyxRoutineRef; const AExpected, AImplementation: TNyxText): TNyxText;
var
  LOriginal: TNyxRoutineSource;
  LReplacement: TNyxText;
  LTokens: TTokens;
  LStart: Integer;
  LHeader: Integer;
  LFinish: Integer;
  LIndex: Integer;
begin
  LOriginal := ReadNyxRoutineSource(ASource, ARoutine);

  if not LOriginal.Editable then
  begin
    raise ENyxSource.CreateAt(LOriginal.Reason, ASource, LOriginal.FStart);
  end;

  if LOriginal.Code <> AExpected then
  begin
    raise ENyxSource.CreateAt('Routine changed; inspect its exact accepted text again',
      ASource, LOriginal.FHeaderFinish);
  end;
  LReplacement := JoinSourceFrame([LOriginal.Signature, AImplementation]);
  LTokens := HandlerTokens(LReplacement);
  LStart := 0;

  if HandlerWord(LTokens, 0, 'class') then
  begin
    Inc(LStart);
  end;
  LFinish := HandlerRoutineEnd(LReplacement, LTokens, LStart, 0, LHeader);

  if (LFinish <> High(LTokens)) or
    (LTokens[LHeader].Finish <> Length(LOriginal.Signature) + 1) then
  begin
    raise ENyxSource.CreateAt('Replace only the routine implementation, retaining its signature',
      LReplacement, 1);
  end;
  HandlerNoDirectives(LReplacement, 1, Length(LReplacement) + 1);
  for LIndex := LTokens[LFinish].Finish to Length(LReplacement) do
  begin

    if not (LReplacement[LIndex] in [#9, #10, #13, ' ']) then
    begin
      raise ENyxSource.CreateAt('Finish routine text at end; with only trailing whitespace',
        LReplacement, LIndex);
    end;
  end;
  Result := JoinSourceFrame([Copy(ASource, 1, LOriginal.FHeaderFinish - 1),
    AImplementation, Copy(ASource, LOriginal.FFinish, MaxInt)]);
end;
