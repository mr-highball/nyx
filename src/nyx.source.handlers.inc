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

{ This include shares the workspace's Unicode lexer. It establishes exact
  callback ownership only; expression/type checking remains the compiler's job.
  Do not replace it with substring searches: Pascal literals and comments may
  contain complete, convincing-looking method declarations and block keywords. }
function HandlerTokens(const ASource: TNyxText): TTokens;
var
  LAll: TTokens;
  LIndex: Integer;
  LCount: Integer;
begin
  Result := nil;
  LAll := Lex(ASource);
  SetLength(Result, Length(LAll));
  LCount := 0;
  for LIndex := 0 to High(LAll) do
  begin

    if LAll[LIndex].Kind <> tkComment then
    begin
      Result[LCount] := LAll[LIndex];
      Inc(LCount);
    end;
  end;
  SetLength(Result, LCount);
end;

function HandlerSymbol(const ATokens: TTokens; AIndex: Integer;
  const ASymbol: TNyxText): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < Length(ATokens)) and
    (ATokens[AIndex].Kind = tkSymbol) and (ATokens[AIndex].Text = ASymbol);
end;

function HandlerWord(const ATokens: TTokens; AIndex: Integer;
  const AWord: TNyxText): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < Length(ATokens)) and
    (ATokens[AIndex].Kind = tkWord) and SameText(ATokens[AIndex].Text, AWord);

  if Result and (AIndex > 0) then
  begin
    { Escaped identifiers and member references are not structural keywords. }
    Result := not HandlerSymbol(ATokens, AIndex - 1, '&') and
      not HandlerSymbol(ATokens, AIndex - 1, '.');
  end;
end;

procedure HandlerFail(const ASource, AMessage: TNyxText;
  const ATokens: TTokens; AIndex: Integer);
var
  LPosition: Integer;
begin
  LPosition := Length(ASource) + 1;

  if (AIndex >= 0) and (AIndex < Length(ATokens)) then
  begin
    LPosition := ATokens[AIndex].Position;
  end;
  raise ENyxSource.CreateAt(AMessage, ASource, LPosition);
end;

function HandlerTypeBlock(const ATokens: TTokens; AIndex: Integer): Boolean;
begin
  Result := HandlerWord(ATokens, AIndex, 'record') or
    HandlerWord(ATokens, AIndex, 'object') or HandlerWord(ATokens, AIndex, 'interface') or
    HandlerWord(ATokens, AIndex, 'class');

  if Result and (AIndex + 1 < Length(ATokens)) then
  begin
    Result := not HandlerSymbol(ATokens, AIndex + 1, ';') and
      not HandlerWord(ATokens, AIndex + 1, 'of') and
      not HandlerWord(ATokens, AIndex + 1, 'procedure') and
      not HandlerWord(ATokens, AIndex + 1, 'function') and
      not HandlerWord(ATokens, AIndex + 1, 'constructor') and
      not HandlerWord(ATokens, AIndex + 1, 'destructor') and
      not HandlerWord(ATokens, AIndex + 1, 'operator') and
      not HandlerWord(ATokens, AIndex + 1, 'property') and
      not HandlerWord(ATokens, AIndex + 1, 'var');
  end;
end;

function HandlerBlockEnd(const ASource: TNyxText;
  const ATokens: TTokens; AStart: Integer): Integer;
type
  TBlock = (hbEnd, hbRepeat, hbAssembly);
  TBlockStack = array[0..255] of TBlock;
var
  LStack: TBlockStack;
  LDepth: Integer;
  LIndex: Integer;
  LOpen: Boolean;
  LKind: TBlock;
begin
  { Every opening block publishes a slot before LDepth advances. Initializing
    the fixed stack also makes that invariant explicit to checked compilers. }
  LStack := Default(TBlockStack);
  LDepth := 0;
  LIndex := AStart;
  while LIndex < Length(ATokens) do
  begin
    LOpen := False;
    LKind := hbEnd;

    if (LDepth > 0) and (LStack[LDepth - 1] = hbAssembly) then
    begin
      { Assembly content is opaque to Pascal block balancing. Its end token
        closes only that assembly block; the compiler admits target syntax. }
    end
    else
    begin

      if HandlerTypeBlock(ATokens, LIndex) then
      begin
        { Local types belong in the declaration section. Refuse inline type
          blocks rather than mistake their end for the enclosing method end. }
        HandlerFail(ASource, 'Inline callback type blocks require ordinary Pascal editing', ATokens, LIndex);
      end;
      LOpen := HandlerWord(ATokens, LIndex, 'begin') or
        HandlerWord(ATokens, LIndex, 'case') or HandlerWord(ATokens, LIndex, 'try') or
        HandlerWord(ATokens, LIndex, 'repeat') or HandlerWord(ATokens, LIndex, 'asm');

      if HandlerWord(ATokens, LIndex, 'repeat') then
      begin
        LKind := hbRepeat;
      end
      else if HandlerWord(ATokens, LIndex, 'asm') then
      begin
        LKind := hbAssembly;
      end;
    end;

    if LOpen then
    begin

      if LDepth = Length(LStack) then
      begin
        HandlerFail(ASource, 'Callback block nesting exceeds the editor budget', ATokens, LIndex);
      end;
      LStack[LDepth] := LKind;
      Inc(LDepth);
    end
    else if HandlerWord(ATokens, LIndex, 'end') or HandlerWord(ATokens, LIndex, 'until') then
    begin

      if (LDepth = 0) or
        ((LStack[LDepth - 1] = hbRepeat) <> HandlerWord(ATokens, LIndex, 'until')) then
      begin
        HandlerFail(ASource, 'Callback blocks do not have an unambiguous closing boundary', ATokens, LIndex);
      end;
      Dec(LDepth);

      if LDepth = 0 then
      begin
        Exit(LIndex);
      end;
    end;
    Inc(LIndex);
  end;
  HandlerFail(ASource, 'The callback implementation is missing its closing end', ATokens, AStart);
  Result := -1;
end;

function HandlerRoutineEnd(const ASource: TNyxText; const ATokens: TTokens;
  AStart, ANesting: Integer; out AHeaderEnd: Integer): Integer;
var
  LIndex: Integer;
  LGroups: Integer;
  LTypes: Integer;
  LNestedHeader: Integer;
  LStatementStart: Boolean;
begin

  if ANesting > 32 then
  begin
    HandlerFail(ASource, 'Callback local routine nesting exceeds the editor budget', ATokens, AStart);
  end;
  LIndex := AStart + 1;
  LGroups := 0;
  AHeaderEnd := -1;
  while LIndex < Length(ATokens) do
  begin

    if HandlerSymbol(ATokens, LIndex, '(') or HandlerSymbol(ATokens, LIndex, '[') then
    begin
      Inc(LGroups);
    end
    else if HandlerSymbol(ATokens, LIndex, ')') or HandlerSymbol(ATokens, LIndex, ']') then
    begin
      Dec(LGroups);
    end
    else if HandlerSymbol(ATokens, LIndex, ';') and (LGroups = 0) then
    begin
      AHeaderEnd := LIndex;
      Break;
    end;

    if (LGroups < 0) or (LGroups > 256) then
    begin
      HandlerFail(ASource, 'Callback signature has unbalanced groups', ATokens, LIndex);
    end;
    Inc(LIndex);
  end;

  if AHeaderEnd < 0 then
  begin
    HandlerFail(ASource, 'Callback signature must end with a semicolon', ATokens, AStart);
  end;
  Inc(LIndex);
  LGroups := 0;
  LTypes := 0;
  LStatementStart := True;
  while LIndex < Length(ATokens) do
  begin

    if (LGroups = 0) and (LTypes = 0) and LStatementStart then
    begin

      if HandlerWord(ATokens, LIndex, 'forward') or HandlerWord(ATokens, LIndex, 'external') then
      begin

        if ANesting = 0 then
        begin
          HandlerFail(ASource, 'This callback has no local implementation to edit', ATokens, LIndex);
        end;
        while (LIndex < Length(ATokens)) and not HandlerSymbol(ATokens, LIndex, ';') do
        begin
          Inc(LIndex);
        end;
        Exit(LIndex);
      end;

      if HandlerWord(ATokens, LIndex, 'procedure') or HandlerWord(ATokens, LIndex, 'function') then
      begin

        if HandlerSymbol(ATokens, LIndex + 2, '.') then
        begin
          HandlerFail(ASource, 'A sibling qualified method cannot belong to this callback', ATokens, LIndex);
        end;
        LIndex := HandlerRoutineEnd(ASource, ATokens, LIndex, ANesting + 1, LNestedHeader) + 1;
        Continue;
      end;

      if HandlerWord(ATokens, LIndex, 'begin') or HandlerWord(ATokens, LIndex, 'asm') then
      begin
        Result := HandlerBlockEnd(ASource, ATokens, LIndex) + 1;

        if not HandlerSymbol(ATokens, Result, ';') then
        begin
          HandlerFail(ASource, 'Finish the callback implementation with end;', ATokens, Result);
        end;
        Exit;
      end;

      if HandlerWord(ATokens, LIndex, 'initialization') or
        HandlerWord(ATokens, LIndex, 'finalization') or HandlerWord(ATokens, LIndex, 'end') then
      begin
        HandlerFail(ASource, 'This callback is missing a complete local body', ATokens, LIndex);
      end;
    end;

    if HandlerSymbol(ATokens, LIndex, '(') or HandlerSymbol(ATokens, LIndex, '[') then
    begin
      Inc(LGroups);
    end
    else if HandlerSymbol(ATokens, LIndex, ')') or HandlerSymbol(ATokens, LIndex, ']') then
    begin
      Dec(LGroups);
    end
    else if (LGroups = 0) and HandlerTypeBlock(ATokens, LIndex) then
    begin
      Inc(LTypes);
    end
    else if (LGroups = 0) and HandlerWord(ATokens, LIndex, 'end') then
    begin
      Dec(LTypes);
    end;

    if (LGroups < 0) or (LGroups > 256) or (LTypes < 0) or (LTypes > 256) then
    begin
      HandlerFail(ASource, 'Callback declarations have ambiguous boundaries', ATokens, LIndex);
    end;

    if (LGroups = 0) and (LTypes = 0) then
    begin
      LStatementStart := HandlerSymbol(ATokens, LIndex, ';') or
        HandlerWord(ATokens, LIndex, 'var') or HandlerWord(ATokens, LIndex, 'const') or
        HandlerWord(ATokens, LIndex, 'type');
    end;
    Inc(LIndex);
  end;
  HandlerFail(ASource, 'This callback is missing a complete implementation', ATokens, AStart);
  Result := -1;
end;

procedure HandlerNoDirectives(const ASource: TNyxText; AStart, AFinish: Integer);
var
  LTokens: TTokens;
  LIndex: Integer;
  LBreak: Integer;
  LConditions: Integer;
  LDirective: TNyxText;
begin
  LTokens := Lex(ASource);
  LConditions := 0;
  for LIndex := 0 to High(LTokens) do
  begin

    if LTokens[LIndex].Position >= AFinish then
    begin
      Break;
    end;

    if (LTokens[LIndex].Position >= AStart) and (LConditions <> 0) then
    begin
      raise ENyxSource.CreateAt('Conditional callback implementations require ordinary Pascal editing',
        ASource, LTokens[LIndex].Position);
    end;

    if (LTokens[LIndex].Kind = tkComment) and
      (LTokens[LIndex].Position >= AStart) and (LTokens[LIndex].Position < AFinish) and
      ((Copy(LTokens[LIndex].Text, 1, 2) = '{$') or (Copy(LTokens[LIndex].Text, 1, 3) = '(*$')) then
    begin
      raise ENyxSource.CreateAt('Callback directives require ordinary Pascal editing; semantic ownership is ambiguous',
        ASource, LTokens[LIndex].Position);
    end;
    { A directive outside the method can still make that entire method
      conditional. Track nesting before its first token; do not assume that
      the presence of one lexically complete body proves target ownership. }

    if LTokens[LIndex].Kind <> tkComment then
    begin
      Continue;
    end;
    LDirective := LTokens[LIndex].Text;

    if Copy(LDirective, 1, 2) = '{$' then
    begin
      LDirective := Copy(LDirective, 3, Length(LDirective) - 3);
    end
    else if Copy(LDirective, 1, 3) = '(*$' then
    begin
      LDirective := Copy(LDirective, 4, Length(LDirective) - 5);
    end
    else
    begin
      Continue;
    end;
    LDirective := LowerCase(Trim(LDirective));
    LBreak := 1;
    while (LBreak <= Length(LDirective)) and (LDirective[LBreak] in ['a'..'z']) do
    begin
      Inc(LBreak);
    end;
    LDirective := Copy(LDirective, 1, LBreak - 1);

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
      raise ENyxSource.CreateAt('Callback conditional ownership is unbalanced', ASource,
        LTokens[LIndex].Position);
    end;
  end;
end;

function ReadNyxHandlerSource(const ASource: TNyxText;
  const AHandler: TNyxHandlerRef): TNyxHandlerSource;
var
  LTokens: TTokens;
  LPrefix: TNyxText;
  LBody: TNyxText;
  LSuffix: TNyxText;
  LIndex: Integer;
  LStart: Integer;
  LHeader: Integer;
  LFinish: Integer;
begin
  Result := Default(TNyxHandlerSource);
  Result.FHandler := NyxHandler(AHandler.Name);
  SplitNyxSourceFrame(ASource, LPrefix, LBody, LSuffix);
  LTokens := HandlerTokens(ASource);
  LStart := -1;
  for LIndex := 0 to Length(LTokens) - 4 do
  begin

    if (LTokens[LIndex].Kind = tkWord) and SameText(LTokens[LIndex].Text, 'procedure') and
      (LTokens[LIndex + 1].Kind = tkWord) and SameText(LTokens[LIndex + 1].Text, AHandler.Name) and
      HandlerSymbol(LTokens, LIndex + 2, '.') and (LTokens[LIndex + 3].Kind = tkWord) and
      SameText(LTokens[LIndex + 3].Text, 'Invoke') then
    begin

      if LStart >= 0 then
      begin
        HandlerFail(ASource, 'Callback implementation identity is ambiguous', LTokens, LIndex);
      end;
      LStart := LIndex;
    end;
  end;

  if LStart < 0 then
  begin
    raise ENyxSource.CreateAt('The callback implementation is missing from this source', ASource, 1);
  end;
  LFinish := HandlerRoutineEnd(ASource, LTokens, LStart, 0, LHeader);
  Result.FStart := LTokens[LStart].Position;
  Result.FHeaderFinish := LTokens[LHeader].Finish;
  Result.FFinish := LTokens[LFinish].Finish;

  if (Result.FStart <= Length(LPrefix) + Length(LBody)) and
    (Result.FFinish > Length(LPrefix)) then
  begin
    raise ENyxSource.CreateAt('Callback implementations belong outside managed views', ASource, Result.FStart);
  end;
  HandlerNoDirectives(ASource, Result.FStart, Result.FFinish);
  Result.FSignature := Copy(ASource, Result.FStart, Result.FHeaderFinish - Result.FStart);
  Result.FImplementation := Copy(ASource, Result.FHeaderFinish, Result.FFinish - Result.FHeaderFinish);
  Result.FLine := 1;
  for LIndex := 1 to Result.FStart - 1 do
  begin

    if ASource[LIndex] = #10 then
    begin
      Inc(Result.FLine);
    end;
  end;
end;

function ReplaceNyxHandlerImplementation(const ASource: TNyxText;
  const AHandler: TNyxHandlerRef; const AExpected, AImplementation: TNyxText): TNyxText;
var
  LOriginal: TNyxHandlerSource;
  LReplacement: TNyxText;
  LTokens: TTokens;
  LHeader: Integer;
  LFinish: Integer;
  LIndex: Integer;
begin
  LOriginal := ReadNyxHandlerSource(ASource, AHandler);

  if LOriginal.Code <> AExpected then
  begin
    raise ENyxSource.CreateAt('Callback implementation changed; inspect its exact accepted text again',
      ASource, LOriginal.FHeaderFinish);
  end;
  LReplacement := JoinSourceFrame([LOriginal.Signature, AImplementation]);
  LTokens := HandlerTokens(LReplacement);
  LFinish := HandlerRoutineEnd(LReplacement, LTokens, 0, 0, LHeader);

  if (LFinish <> High(LTokens)) or (LTokens[LHeader].Finish <> Length(LOriginal.Signature) + 1) then
  begin
    raise ENyxSource.CreateAt('Replace only this callback implementation, retaining its signature', LReplacement, 1);
  end;
  HandlerNoDirectives(LReplacement, 1, Length(LReplacement) + 1);
  for LIndex := LTokens[LFinish].Finish to Length(LReplacement) do
  begin

    if not (LReplacement[LIndex] in [#9, #10, #13, ' ']) then
    begin
      { A trailing line comment could swallow the next same-line helper.
        The semicolon closes the editable method; comments belong in its body. }
      raise ENyxSource.CreateAt('Finish callback text at end; with only trailing whitespace',
        LReplacement, LIndex);
    end;
  end;
  Result := JoinSourceFrame([Copy(ASource, 1, LOriginal.FHeaderFinish - 1),
    AImplementation, Copy(ASource, LOriginal.FFinish, MaxInt)]);
end;
