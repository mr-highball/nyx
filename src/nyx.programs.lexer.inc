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



{ Owned scalar lexer for nyx.programs. Source maps preserve byte coordinates on
  both UTF-8/FPC and UTF-16/pas2js without retaining any target editor/control. }

constructor TLexer.Create(const ASource: TNyxText; const ALimits: TNyxProgramLimits);
var
  LStorage: Integer;
  LCount: Integer;
  LByte: Integer;
  LWidth: Integer;
  LScalar: Integer;
  LBefore: Integer;
  LLine: Integer;
  LColumn: Integer;
  LCR: Boolean;
begin
  inherited Create;
  FLimits := ALimits;
  FSource := ASource;

  if Length(ASource) > ALimits.MaximumSourceBytes then
  begin
    raise ENyxProgram.Create(npdLimit, NyxProgramSpan(0, 0, 1, 1),
      'Program source exceeds its UTF-8 byte budget');
  end;
  SetLength(FChars, Length(ASource));
  SetLength(FStorage, Length(ASource) + 1);
  SetLength(FBytes, Length(ASource) + 1);
  SetLength(FLines, Length(ASource) + 1);
  SetLength(FColumns, Length(ASource) + 1);
  LStorage := 1;
  LCount := 0;
  LByte := 0;
  LLine := 1;
  LColumn := 1;
  LCR := False;
  while LStorage <= Length(ASource) do
  begin
    LBefore := LStorage;

    if not NyxNextScalar(ASource, LStorage, LScalar) then
    begin
      raise ENyxProgram.Create(npdSyntax, NyxProgramSpan(LByte, 1, LLine, LColumn),
        'Program source contains malformed Unicode');
    end;
    LWidth := 1;

    if LScalar > $7F then
    begin
      LWidth := 2;
    end;

    if LScalar > $7FF then
    begin
      LWidth := 3;
    end;

    if LScalar > $FFFF then
    begin
      LWidth := 4;
    end;

    if LWidth > ALimits.MaximumSourceBytes - LByte then
    begin
      raise ENyxProgram.Create(npdLimit, NyxProgramSpan(LByte, 0, LLine, LColumn),
        'Program source exceeds its UTF-8 byte budget');
    end;
    FChars[LCount] := LScalar;
    FStorage[LCount] := LBefore;
    FBytes[LCount] := LByte;
    FLines[LCount] := LLine;
    FColumns[LCount] := LColumn;
    Inc(LByte, LWidth);
    Inc(LCount);

    if LScalar = 13 then
    begin
      Inc(LLine);
      LColumn := 1;
    end
    else if LScalar = 10 then
    begin

      if not LCR then
      begin
        Inc(LLine);
      end;
      LColumn := 1;
    end
    else
    begin
      Inc(LColumn);
    end;
    LCR := LScalar = 13;
  end;
  SetLength(FChars, LCount);
  SetLength(FStorage, LCount + 1);
  SetLength(FBytes, LCount + 1);
  SetLength(FLines, LCount + 1);
  SetLength(FColumns, LCount + 1);
  FStorage[LCount] := Length(ASource) + 1;
  FBytes[LCount] := LByte;
  FLines[LCount] := LLine;
  FColumns[LCount] := LColumn;
end;

function TLexer.Peek(AOffset: Integer): Integer;
begin
  Result := -1;

  if (AOffset >= 0) and (AOffset < Length(FChars) - FPosition) then
  begin
    Result := FChars[FPosition + AOffset];
  end;
end;

function TLexer.Take: Integer;
begin
  Result := Peek;

  if Result >= 0 then
  begin
    Inc(FPosition);
  end;
end;

function TLexer.Span(AFirst, ALast: Integer): TNyxProgramSpan;
begin
  Result := NyxProgramSpan(FBytes[AFirst], FBytes[ALast] - FBytes[AFirst],
    FLines[AFirst], FColumns[AFirst]);
end;

procedure TLexer.Fail(AFirst: Integer; const AMessage: TNyxText);
begin
  raise ENyxProgram.Create(npdSyntax, Span(AFirst, FPosition), AMessage);
end;

procedure TLexer.Skip;
var
  LStack: TIntegers;
  LFirst: Integer;
  LKind: Integer;
begin
  LStack := nil;
  while Peek >= 0 do
  begin

    if NyxScalarWhitespace(Peek) then
    begin
      Take;
      Continue;
    end;

    if (Peek = Ord('/')) and (Peek(1) = Ord('/')) then
    begin
      while (Peek >= 0) and (Peek <> 10) and (Peek <> 13) do
      begin
        Take;
      end;
      Continue;
    end;

    if (Peek <> Ord('{')) and not ((Peek = Ord('(')) and (Peek(1) = Ord('*'))) then
    begin
      Exit;
    end;
    LFirst := FPosition;
    while True do
    begin
      LKind := 0;

      if Peek = Ord('{') then
      begin

        if Peek(1) = Ord('$') then
        begin
          Take;
          Take;
          Fail(LFirst, 'Compiler directives cannot change a behavior program');
        end;
        LKind := 1;
        Take;
      end
      else if (Peek = Ord('(')) and (Peek(1) = Ord('*')) then
      begin

        if Peek(2) = Ord('$') then
        begin
          Take;
          Take;
          Take;
          Fail(LFirst, 'Compiler directives cannot change a behavior program');
        end;
        LKind := 2;
        Take;
        Take;
      end;

      if LKind <> 0 then
      begin

        if Length(LStack) >= FLimits.MaximumNesting then
        begin
          raise ENyxProgram.Create(npdLimit, Span(LFirst, FPosition), 'Program comment nesting budget exceeded');
        end;
        SetLength(LStack, Length(LStack) + 1);
        LStack[High(LStack)] := LKind;
        Continue;
      end;

      if Peek < 0 then
      begin
        Fail(LFirst, 'Unterminated Pascal comment');
      end;

      if ((LStack[High(LStack)] = 1) and (Peek = Ord('}'))) or
        ((LStack[High(LStack)] = 2) and (Peek = Ord('*')) and (Peek(1) = Ord(')'))) then
      begin
        LKind := LStack[High(LStack)];
        Take;

        if LKind = 2 then
        begin
          Take;
        end;
        SetLength(LStack, Length(LStack) - 1);

        if Length(LStack) = 0 then
        begin
          Break;
        end;
      end
      else
      begin
        Take;
      end;
    end;
  end;
end;

{ Literal pieces are joined once. Long quoted creator text therefore avoids
  repeated whole-prefix concatenation while retaining supplementary data/NUL. }
function TLexer.ReadText: TNyxText;
var
  LPieces: TNyxStrings;
  LFirst: Integer;
  LStart: Integer;
  LBase: Integer;
  LValue: Integer;
  LDigits: Integer;
  LDigit: Integer;
  LChar: Integer;
begin
  LPieces := TNyxStrings.Create;
  LFirst := FPosition;
  try
    while (Peek = Ord('''')) or (Peek = Ord('#')) do
    begin

      if Take = Ord('''') then
      begin
        LStart := FPosition;
        while True do
        begin

          if (Peek < 0) or (Peek = 10) or (Peek = 13) then
          begin
            Fail(LFirst, 'Quoted Pascal strings must close on the same source line');
          end;

          if Peek = Ord('''') then
          begin
            LPieces.Add(System.Copy(FSource, FStorage[LStart], FStorage[FPosition] - FStorage[LStart]));
            Take;

            if Peek <> Ord('''') then
            begin
              Break;
            end;
            Take;
            LPieces.Add('''');
            LStart := FPosition;
          end
          else
          begin
            Take;
          end;
        end;
      end
      else
      begin
        LBase := 10;

        if Peek = Ord('$') then
        begin
          LBase := 16;
          Take;
        end;
        LValue := 0;
        LDigits := 0;
        while Peek >= 0 do
        begin
          LChar := Peek;
          LDigit := -1;

          if (LChar >= Ord('0')) and (LChar <= Ord('9')) then
          begin
            LDigit := LChar - Ord('0');
          end
          else if (LBase = 16) and (LChar >= Ord('a')) and (LChar <= Ord('f')) then
          begin
            LDigit := LChar - Ord('a') + 10;
          end
          else if (LBase = 16) and (LChar >= Ord('A')) and (LChar <= Ord('F')) then
          begin
            LDigit := LChar - Ord('A') + 10;
          end;

          if (LDigit < 0) or (LDigit >= LBase) then
          begin
            Break;
          end;
          Take;

          if LValue > ($10FFFF - LDigit) div LBase then
          begin
            Fail(LFirst, 'Pascal character escape exceeds Unicode scalar range');
          end;
          LValue := LValue * LBase + LDigit;
          Inc(LDigits);
        end;

        if (LDigits = 0) or ((LValue >= $D800) and (LValue <= $DFFF)) then
        begin
          Fail(LFirst, 'Pascal character escapes require a Unicode scalar');
        end;
        LPieces.Add(NyxScalarText(LValue));
      end;
    end;
    Result := LPieces.Join;
  finally
    LPieces.Free;
  end;
end;

function TLexer.ReadNumber(out AInteger: Boolean): Double;
var
  LFirst: Integer;
  LBase: Integer;
  LDigit: Integer;
  LChar: Integer;
  LCount: Integer;
  LCode: Integer;
  LIndex: Integer;
  LText: TNyxText;
begin
  LFirst := FPosition;
  AInteger := True;
  Result := 0;
  LBase := 10;

  if Peek = Ord('$') then
  begin
    LBase := 16;
    Take;
  end;
  LCount := 0;
  while Peek >= 0 do
  begin
    LChar := Peek;
    LDigit := -1;

    if (LChar >= Ord('0')) and (LChar <= Ord('9')) then
    begin
      LDigit := LChar - Ord('0');
    end
    else if (LBase = 16) and (LChar >= Ord('a')) and (LChar <= Ord('f')) then
    begin
      LDigit := LChar - Ord('a') + 10;
    end
    else if (LBase = 16) and (LChar >= Ord('A')) and (LChar <= Ord('F')) then
    begin
      LDigit := LChar - Ord('A') + 10;
    end;

    if (LDigit < 0) or (LDigit >= LBase) then
    begin
      Break;
    end;
    Take;
    Inc(LCount);
  end;

  if LCount = 0 then
  begin
    Fail(LFirst, 'Numeric literals require digits');
  end;

  if LBase = 10 then
  begin

    if (Peek = Ord('.')) and (Peek(1) <> Ord('.')) then
    begin
      AInteger := False;
      Take;
      while (Peek >= Ord('0')) and (Peek <= Ord('9')) do
      begin
        Take;
      end;
    end;

    if (Peek = Ord('e')) or (Peek = Ord('E')) then
    begin
      AInteger := False;
      Take;

      if (Peek = Ord('+')) or (Peek = Ord('-')) then
      begin
        Take;
      end;
      LCount := 0;
      while (Peek >= Ord('0')) and (Peek <= Ord('9')) do
      begin
        Take;
        Inc(LCount);
      end;

      if LCount = 0 then
      begin
        Fail(LFirst, 'A numeric exponent requires digits');
      end;
    end;
  end;

  if not AInteger then
  begin
    LText := System.Copy(FSource, FStorage[LFirst], FStorage[FPosition] - FStorage[LFirst]);
    try
      Val(LText, Result, LCode);
    except
      on E: EMathError do
      begin
        Fail(LFirst, 'Numeric literal must be a finite invariant Double');
      end;
    end;

    if (LCode <> 0) or IsNan(Result) or IsInfinite(Result) then
    begin
      Fail(LFirst, 'Numeric literal must be a finite invariant Double');
    end;
  end
  else
  begin
    LIndex := LFirst + Ord(LBase = 16);
    while LIndex < FPosition do
    begin
      LChar := FChars[LIndex];
      LDigit := LChar - Ord('0');

      if LChar >= Ord('a') then
      begin
        LDigit := LChar - Ord('a') + 10;
      end
      else if LChar >= Ord('A') then
      begin
        LDigit := LChar - Ord('A') + 10;
      end;

      if Result > (2147483648.0 - LDigit) / LBase then
      begin
        Fail(LFirst, 'Integer literals require signed 32-bit magnitude');
      end;
      Result := Result * LBase + LDigit;
      Inc(LIndex);
    end;
  end;
end;

function TLexer.Next: TToken;
var
  LFirst: Integer;
  LChar: Integer;
  LInteger: Boolean;
begin
  Skip;
  Result := Default(TToken);
  LFirst := FPosition;
  LChar := Peek;

  if LChar < 0 then
  begin
    Result.Kind := ptEOF;
  end
  else if ((LChar >= Ord('a')) and (LChar <= Ord('z'))) or
    ((LChar >= Ord('A')) and (LChar <= Ord('Z'))) or (LChar = Ord('_')) then
  begin
    Result.Kind := ptIdentifier;
    Take;
    while ((Peek >= Ord('a')) and (Peek <= Ord('z'))) or
      ((Peek >= Ord('A')) and (Peek <= Ord('Z'))) or (Peek = Ord('_')) or
      ((Peek >= Ord('0')) and (Peek <= Ord('9'))) do
    begin
      Take;
    end;
    Result.Text := System.Copy(FSource, FStorage[LFirst], FStorage[FPosition] - FStorage[LFirst]);

    if FPosition - LFirst > 128 then
    begin
      Fail(LFirst, 'A Pascal identifier exceeds 128 ASCII characters');
    end;
  end
  else if (LChar = Ord('''')) or (LChar = Ord('#')) then
  begin
    Result.Kind := ptText;
    Result.Text := ReadText;
  end
  else if ((LChar >= Ord('0')) and (LChar <= Ord('9'))) or (LChar = Ord('$')) then
  begin
    Result.Number := ReadNumber(LInteger);
    Result.Kind := ptNumber;

    if LInteger then
    begin
      Result.Kind := ptInteger;
    end;
  end
  else
  begin

    if LChar > 127 then
    begin
      Take;
      Fail(LFirst, 'Pascal identifiers/operators use ASCII; quoted data supports Unicode');
    end;
    Result.Kind := ptSymbol;
    Result.Text := NyxScalarText(Take);

    if ((LChar = Ord(':')) and (Peek = Ord('='))) or
      ((LChar = Ord('<')) and ((Peek = Ord('=')) or (Peek = Ord('>')))) or
      ((LChar = Ord('>')) and (Peek = Ord('='))) or
      ((LChar = Ord('.')) and (Peek = Ord('.'))) then
    begin
      Result.Text := Result.Text + NyxScalarText(Take);
    end;
  end;
  Result.Span := Span(LFirst, FPosition);
end;
