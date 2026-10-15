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


{ Parser/compiler for the shared scalar VM. This include belongs only to
  nyx.regex; borrowed target vectors become immutable before factory publication. }

function RegexLineBreak(AScalar: Integer): Boolean;
begin
  Result := (AScalar = 10) or (AScalar = 13) or (AScalar = $85) or
    (AScalar = $2028) or (AScalar = $2029);
end;

{ Geometric vector growth without signed overflow, respecting the admitted
  maximum even when a caller chooses a tiny or unusually large budget. }
function RegexCapacity(AOld, AMaximum: Integer): Integer;
begin

  if AOld > AMaximum div 2 then
  begin
    Exit(AMaximum);
  end;
  Result := Min(AMaximum, Max(8, AOld * 2));
end;

constructor TCompiler.Create(ATarget: TCompiledRegex);
var
  LIndex: Integer;
  LCount: Integer;
  LScalar: Integer;
begin
  inherited Create;
  FTarget := ATarget;
  SetLength(FPattern, Length(FTarget.FOptions.Pattern));
  LIndex := 1;
  LCount := 0;
  while LIndex <= Length(FTarget.FOptions.Pattern) do
  begin

    if not NyxNextScalar(FTarget.FOptions.Pattern, LIndex, LScalar) then
    begin
      raise ENyxRegex.Create('Malformed regex pattern Unicode');
    end;
    FPattern[LCount] := LScalar;
    Inc(LCount);
  end;
  SetLength(FPattern, LCount);
  FMode.Comparison := FTarget.FOptions.Comparison;
  FMode.Multiline := FTarget.FOptions.IsMultiline;
  FMode.DotAll := FTarget.FOptions.IsDotAll;
  FMode.Expanded := FTarget.FOptions.IsExpanded;
  SetLength(FTarget.FNames, 1);
  FTarget.FNames[0] := '';
end;

procedure TCompiler.Fail(const AReason: TNyxText);
begin
  raise ENyxRegex.Create('Regex scalar ' + TNyxText(IntToStr(FPosition + 1)) + ': ' + AReason);
end;

function TCompiler.Peek: Integer;
begin
  Result := -1;

  if FPosition < Length(FPattern) then
  begin
    Result := FPattern[FPosition];
  end;
end;

function TCompiler.Take: Integer;
begin
  Result := Peek;

  if Result < 0 then
  begin
    Fail('Unexpected end of pattern');
  end;
  Inc(FPosition);
end;

procedure TCompiler.Expect(AValue: Integer);
begin

  if Take <> AValue then
  begin
    Fail('Expected ' + NyxScalarText(AValue));
  end;
end;

{ Quoting is lexical: a following quantifier still belongs to the final quoted
  character. These transitions run only at expression token boundaries, never
  while reading names, character classes or comments. Source offsets stay exact. }
procedure TCompiler.QuoteBoundary;
begin
  while (FPosition + 1 < Length(FPattern)) and (Peek = Ord('\')) do
  begin

    if not FQuoted and (FPattern[FPosition + 1] = Ord('Q')) then
    begin
      Inc(FPosition, 2);
      FQuoted := True;
    end
    else if FQuoted and (FPattern[FPosition + 1] = Ord('E')) then
    begin
      Inc(FPosition, 2);
      FQuoted := False;
    end
    else
    begin
      Break;
    end;
  end;
end;

procedure TCompiler.Space;
begin
  while True do
  begin
    QuoteBoundary;

    if FQuoted or not FMode.Expanded or (Peek < 0) then
    begin
      Exit;
    end;

    if NyxScalarWhitespace(Peek) then
    begin
      Take;
    end
    else if Peek = Ord('#') then
    begin
      while (Peek >= 0) and not RegexLineBreak(Peek) do
      begin
        Take;
      end;
    end
    else
    begin
      Break;
    end;
  end;
end;

procedure TCompiler.Enter;
begin

  if FNesting >= FTarget.FOptions.Budget.MaximumNesting then
  begin
    raise ENyxRegexLimit.Create('Regex nesting budget exceeded');
  end;
  Inc(FNesting);
end;

procedure TCompiler.Leave;
begin
  Dec(FNesting);
end;

function TCompiler.Node(AKind: TNodeKind): Integer;
var
  LCapacity: Integer;
begin

  if FNodeCount >= FTarget.FOptions.Budget.MaximumInstructions then
  begin
    raise ENyxRegexLimit.Create('Regex syntax-node budget exceeded');
  end;

  if FNodeCount = Length(FNodes) then
  begin
    LCapacity := RegexCapacity(FNodeCount, FTarget.FOptions.Budget.MaximumInstructions);
    SetLength(FNodes, LCapacity);
  end;
  Result := FNodeCount;
  Inc(FNodeCount);
  FNodes[Result] := Default(TNode);
  FNodes[Result].Kind := AKind;
  FNodes[Result].Mode := FMode;
end;

procedure TCompiler.Child(AParent, AChild: Integer);
var
  LCount: Integer;
begin
  LCount := FNodes[AParent].ChildCount;

  if LCount = Length(FNodes[AParent].Children) then
  begin
    SetLength(FNodes[AParent].Children,
      RegexCapacity(LCount, FTarget.FOptions.Budget.MaximumInstructions));
  end;
  FNodes[AParent].Children[LCount] := AChild;
  Inc(FNodes[AParent].ChildCount);
end;

function TCompiler.Disjunction: Integer;
var
  LFirst: Integer;
begin
  LFirst := Alternative;
  Result := LFirst;
  Space;

  if Peek = Ord('|') then
  begin
    Result := Node(nkAlternate);
    Child(Result, LFirst);
    while Peek = Ord('|') do
    begin
      Take;
      Child(Result, Alternative);
      Space;
    end;
  end;
end;

function TCompiler.Alternative: Integer;
var
  LFirst: Integer;
begin
  Result := Node(nkConcat);
  Space;
  while (Peek >= 0) and (FQuoted or ((Peek <> Ord('|')) and (Peek <> Ord(')')))) do
  begin
    LFirst := Term;
    Child(Result, LFirst);
    Space;
  end;

  if FNodes[Result].ChildCount = 1 then
  begin
    Result := FNodes[Result].Children[0];
  end
  else if FNodes[Result].ChildCount = 0 then
  begin
    FNodes[Result].Kind := nkEmpty;
  end;
end;

function TCompiler.Decimal: Integer;
var
  LDigit: Integer;
begin

  if (Peek < Ord('0')) or (Peek > Ord('9')) then
  begin
    Fail('Expected a nonnegative decimal count');
  end;
  Result := 0;
  while (Peek >= Ord('0')) and (Peek <= Ord('9')) do
  begin
    LDigit := Take - Ord('0');

    if Result > (High(Integer) - LDigit) div 10 then
    begin
      Fail('Decimal count exceeds signed Integer');
    end;
    Result := Result * 10 + LDigit;
  end;
end;

function TCompiler.Term: Integer;
var
  LAtom: Integer;
  LMin: Integer;
  LMax: Integer;
  LAtomic: Integer;
  LGreedy: Boolean;
  LPossessive: Boolean;
begin
  LAtom := Atom;
  Result := LAtom;
  Space;

  if FQuoted then
  begin
    Exit;
  end;

  if (Peek <> Ord('*')) and (Peek <> Ord('+')) and (Peek <> Ord('?')) and
    (Peek <> Ord('{')) then
  begin
    Exit;
  end;

  if FNodes[LAtom].Kind in [nkStart, nkEnd, nkWord, nkAssert] then
  begin
    Fail('Assertions cannot be quantified directly');
  end;
  LMin := 0;
  LMax := -1;
  case Take of
    Ord('+'): LMin := 1;
    Ord('?'): LMax := 1;
    Ord('{'):
    begin
      LMin := Decimal;
      LMax := LMin;

      if Peek = Ord(',') then
      begin
        Take;
        LMax := -1;

        if Peek <> Ord('}') then
        begin
          LMax := Decimal;
        end;
      end;
      Expect(Ord('}'));

      if (LMax >= 0) and (LMax < LMin) then
      begin
        Fail('Quantifier maximum is below minimum');
      end;
    end;
  end;
  LGreedy := True;
  LPossessive := False;

  if Peek = Ord('?') then
  begin
    Take;
    LGreedy := False;
  end
  else if Peek = Ord('+') then
  begin
    Take;
    LPossessive := True;
  end;
  Result := Node(nkRepeat);
  Child(Result, LAtom);
  FNodes[Result].Minimum := LMin;
  FNodes[Result].Maximum := LMax;
  FNodes[Result].Greedy := LGreedy;

  if LPossessive then
  begin
    LAtomic := Node(nkAtomic);
    Child(LAtomic, Result);
    Result := LAtomic;
  end;
end;

function TCompiler.Atom: Integer;
var
  LScalar: Integer;
  LPart: TPart;
begin
  Space;
  LScalar := Take;

  if FQuoted then
  begin
    Result := Node(nkLiteral);
    FNodes[Result].Value := LScalar;
    Exit;
  end;
  case LScalar of
    Ord('('): Result := Group;
    Ord('['):
    begin
      Result := Node(nkClass);
      FNodes[Result].Value := CharacterClass;
    end;
    Ord('\'): Result := Escape(False, LPart);
    Ord('.'): Result := Node(nkAny);
    Ord('^'): Result := Node(nkStart);
    Ord('$'): Result := Node(nkEnd);
    Ord(')'), Ord(']'), Ord('{'), Ord('}'), Ord('*'), Ord('+'), Ord('?'):
    begin
      Fail('Unexpected regex operator');
      Result := -1;
    end;
    else
    begin
      Result := Node(nkLiteral);
      FNodes[Result].Value := LScalar;
    end;
  end;
end;

function TCompiler.ReadName(AClose: Integer): TNyxText;
begin
  Result := '';
  while (Peek >= 0) and (Peek <> AClose) do
  begin

    if Length(Result) > 512 then
    begin
      Fail('Capture name exceeds its bounded scalar contract');
    end;
    Result := Result + NyxScalarText(Take);
  end;
  Expect(AClose);
  Result := NyxRegexGroup(Result).Name;
end;

function TCompiler.Group: Integer;
var
  LMode: TMode;
  LCapture: Boolean;
  LAssertion: Boolean;
  LBehind: Boolean;
  LNegative: Boolean;
  LAtomic: Boolean;
  LName: TNyxText;
  LGroup: Integer;
  LBody: Integer;
  LPart: Integer;
  LEnable: Boolean;
  LSeen: Boolean;
  LFlag: Integer;
begin
  Enter;
  LMode := FMode;
  LCapture := True;
  LAssertion := False;
  LBehind := False;
  LNegative := False;
  LAtomic := False;
  LName := '';

  if Peek = Ord('?') then
  begin
    Take;
    LCapture := False;
    case Take of
      Ord(':'): ;
      Ord('='): LAssertion := True;
      Ord('!'):
      begin
        LAssertion := True;
        LNegative := True;
      end;
      Ord('>'): LAtomic := True;
      Ord('<'):
      begin

        if (Peek = Ord('=')) or (Peek = Ord('!')) then
        begin
          LAssertion := True;
          LBehind := True;
          LNegative := Take = Ord('!');
        end
        else
        begin
          LCapture := True;
          LName := ReadName(Ord('>'));
        end;
      end;
      Ord(''''):
      begin
        LCapture := True;
        LName := ReadName(Ord(''''));
      end;
      Ord('P'):
      begin
        Expect(Ord('<'));
        LCapture := True;
        LName := ReadName(Ord('>'));
      end;
      Ord('#'):
      begin
        while (Peek >= 0) and (Peek <> Ord(')')) do
        begin
          Take;
        end;
        Expect(Ord(')'));
        Leave;
        Exit(Node(nkEmpty));
      end;
      Ord('i'), Ord('m'), Ord('s'), Ord('x'), Ord('-'):
      begin
        Dec(FPosition);
        LEnable := True;
        LSeen := False;
        while (Peek >= 0) and (Peek <> Ord(':')) and (Peek <> Ord(')')) do
        begin
          LFlag := Take;

          if LFlag = Ord('-') then
          begin

            if not LEnable then
            begin
              Fail('Only one inline flag negation is allowed');
            end;
            LEnable := False;
            Continue;
          end;
          LSeen := True;
          case LFlag of
            Ord('i'):
            begin
              FMode.Comparison := nrcExact;

              if LEnable then
              begin
                FMode.Comparison := nrcUnicode;

                if FTarget.FOptions.Comparison = nrcASCII then
                begin
                  FMode.Comparison := nrcASCII;
                end;
              end;
            end;
            Ord('m'): FMode.Multiline := LEnable;
            Ord('s'): FMode.DotAll := LEnable;
            Ord('x'): FMode.Expanded := LEnable;
            else Fail('Unknown inline flag');
          end;
        end;

        if not LSeen then
        begin
          Fail('An inline flag group requires flags');
        end;

        if Peek = Ord(')') then
        begin
          Take;
          Leave;
          Exit(Node(nkEmpty));
        end;
        Expect(Ord(':'));
      end;
      else Fail('Unsupported group syntax');
    end;
  end;
  LGroup := -1;

  if LCapture then
  begin

    if Length(FTarget.FNames) - 1 >= FTarget.FOptions.Budget.MaximumGroups then
    begin
      raise ENyxRegexLimit.Create('Regex capture-group budget exceeded');
    end;
    for LPart := 1 to High(FTarget.FNames) do
    begin

      if (LName <> '') and (FTarget.FNames[LPart] = LName) then
      begin
        Fail('Named captures must be unique');
      end;
    end;
    LGroup := Length(FTarget.FNames);
    SetLength(FTarget.FNames, LGroup + 1);
    FTarget.FNames[LGroup] := LName;
  end;
  LBody := Disjunction;
  Expect(Ord(')'));
  FMode := LMode;
  Leave;
  Result := LBody;

  if LCapture then
  begin
    Result := Node(nkCapture);
    FNodes[Result].Value := LGroup;
  end
  else if LAssertion then
  begin
    Result := Node(nkAssert);
    FNodes[Result].Negative := LNegative;
    FNodes[Result].Behind := LBehind;
  end
  else if LAtomic then
  begin
    Result := Node(nkAtomic);
  end;

  if Result <> LBody then
  begin
    Child(Result, LBody);
  end;
end;

function TCompiler.HexDigits(ACount: Integer): Integer;
var
  LIndex: Integer;
  LChar: Integer;
  LDigit: Integer;
begin
  Result := 0;
  for LIndex := 1 to ACount do
  begin
    LChar := Take;
    LDigit := -1;

    if (LChar >= Ord('0')) and (LChar <= Ord('9')) then
    begin
      LDigit := LChar - Ord('0');
    end
    else if (LChar >= Ord('a')) and (LChar <= Ord('f')) then
    begin
      LDigit := LChar - Ord('a') + 10;
    end
    else if (LChar >= Ord('A')) and (LChar <= Ord('F')) then
    begin
      LDigit := LChar - Ord('A') + 10;
    end;

    if (LDigit < 0) or (Result > ($10FFFF - LDigit) div 16) then
    begin
      Fail('Invalid Unicode hexadecimal escape');
    end;
    Result := Result * 16 + LDigit;
  end;
end;

function TCompiler.PropertyPart(ANegative: Boolean): TPart;
const
  CLong: array[TNyxUnicodeCategory] of TNyxText =
    ('unassigned', 'uppercaseletter', 'lowercaseletter', 'titlecaseletter',
     'modifierletter', 'otherletter', 'nonspacingmark', 'spacingmark', 'enclosingmark',
     'decimalnumber', 'letternumber', 'othernumber', 'connectorpunctuation',
     'dashpunctuation', 'openpunctuation', 'closepunctuation', 'initialpunctuation',
     'finalpunctuation', 'otherpunctuation', 'mathsymbol', 'currencysymbol',
     'modifiersymbol', 'othersymbol', 'spaceseparator', 'lineseparator',
     'paragraphseparator', 'control', 'format', 'surrogate', 'privateuse');
var
  LName: TNyxText;
  LValue: TNyxText;
  LChar: Integer;
  LCategory: TNyxUnicodeCategory;
  LPart: Integer;
begin
  Result := Default(TPart);
  Result.Kind := cpCategory;
  Result.Negated := ANegative;
  Expect(Ord('{'));
  LName := '';
  while (Peek >= 0) and (Peek <> Ord('}')) do
  begin
    LChar := Take;

    if (LChar = Ord('_')) or (LChar = Ord('-')) or (LChar = Ord(' ')) then
    begin
      Continue;
    end;

    if (LChar >= Ord('A')) and (LChar <= Ord('Z')) then
    begin
      Inc(LChar, 32);
    end;

    if not ((LChar >= Ord('a')) and (LChar <= Ord('z'))) and (LChar <> Ord('=')) then
    begin
      Fail('Unknown Unicode property spelling');
    end;

    if Length(LName) >= 128 then
    begin
      Fail('Unicode property name exceeds 128 characters');
    end;
    LName := LName + NyxScalarText(LChar);
  end;
  Expect(Ord('}'));

  if Copy(LName, 1, 16) = 'generalcategory=' then
  begin
    LName := Copy(LName, 17, MaxInt);
  end
  else if Copy(LName, 1, 3) = 'gc=' then
  begin
    LName := Copy(LName, 4, MaxInt);
  end;
  for LCategory := Low(TNyxUnicodeCategory) to High(TNyxUnicodeCategory) do
  begin
    LValue := NyxUnicodeCategoryName(LCategory);
    LValue := LowerCase(LValue);

    if (LName = LValue) or (LName = CLong[LCategory]) then
    begin
      Result.Categories := [LCategory];
      Exit;
    end;
  end;
  LPart := -1;

  if (LName = 'l') or (LName = 'letter') then
  begin
    Result.Categories := [nucUppercaseLetter..nucOtherLetter];
  end
  else if (LName = 'lc') or (LName = 'casedletter') then
  begin
    Result.Categories := [nucUppercaseLetter..nucTitlecaseLetter];
  end
  else if (LName = 'm') or (LName = 'mark') then
  begin
    Result.Categories := [nucNonspacingMark..nucEnclosingMark];
  end
  else if (LName = 'n') or (LName = 'number') then
  begin
    Result.Categories := [nucDecimalNumber..nucOtherNumber];
  end
  else if (LName = 'p') or (LName = 'punctuation') then
  begin
    Result.Categories := [nucConnectorPunctuation..nucOtherPunctuation];
  end
  else if (LName = 's') or (LName = 'symbol') then
  begin
    Result.Categories := [nucMathSymbol..nucOtherSymbol];
  end
  else if (LName = 'z') or (LName = 'separator') then
  begin
    Result.Categories := [nucSpaceSeparator..nucParagraphSeparator];
  end
  else if (LName = 'c') or (LName = 'other') then
  begin
    Result.Categories := [nucControl..nucPrivateUse] + [nucUnassigned];
  end
  else if LName = 'assigned' then
  begin
    Result.Kind := cpAssigned;
  end
  else if (LName = 'whitespace') or (LName = 'space') then
  begin
    Result.Kind := cpSpace;
    Result.First := 1;
  end
  else if (LName = 'any') or (LName = 'ascii') then
  begin
    Result.Kind := cpRange;
    Result.First := 0;
    Result.Last := $10FFFF;

    if LName = 'ascii' then
    begin
      Result.Last := 127;
    end;
  end
  else
  begin
    LPart := 0;
  end;

  if LPart = 0 then
  begin
    Fail('Unsupported Unicode property: ' + LName);
  end;
end;

function TCompiler.Escape(AInClass: Boolean; out APart: TPart): Integer;
var
  LChar: Integer;
  LScalar: Integer;
  LLow: Integer;
  LCount: Integer;
  LClass: Integer;
  LLiteral: Integer;
  LBraced: Boolean;

  function FinishPart: Integer;
  begin
    Result := -1;

    if not AInClass then
    begin
      LClass := Length(FTarget.FClasses);
      SetLength(FTarget.FClasses, LClass + 1);
      SetLength(FTarget.FClasses[LClass].Parts, 1);
      FTarget.FClasses[LClass].Parts[0] := APart;
      Result := Node(nkClass);
      FNodes[Result].Value := LClass;
    end;
  end;

begin
  APart := Default(TPart);
  APart.Kind := cpRange;
  LChar := Take;
  LBraced := False;
  case LChar of
    Ord('d'), Ord('D'), Ord('s'), Ord('S'), Ord('w'), Ord('W'), Ord('p'), Ord('P'):
    begin
      case LChar of
        Ord('d'), Ord('D'): APart.Kind := cpDigit;
        Ord('s'), Ord('S'): APart.Kind := cpSpace;
        Ord('w'), Ord('W'): APart.Kind := cpWord;
        Ord('p'), Ord('P'): APart := PropertyPart(LChar = Ord('P'));
      end;

      if LChar in [Ord('D'), Ord('S'), Ord('W')] then
      begin
        APart.Negated := True;
      end;
      Exit(FinishPart);
    end;
    Ord('b'), Ord('B'):
    begin

      if not AInClass then
      begin
        Result := Node(nkWord);
        FNodes[Result].Negative := LChar = Ord('B');
        Exit;
      end;

      if LChar = Ord('B') then
      begin
        Fail('Unsupported class escape');
      end;
      LChar := 8;
    end;
    Ord('A'), Ord('z'), Ord('Z'):
    begin

      if AInClass then
      begin
        Fail('Input assertions are unavailable inside classes');
      end;

      if LChar = Ord('A') then
      begin
        Result := Node(nkStart);
        FNodes[Result].Value := 1;
      end
      else
      begin
        Result := Node(nkEnd);
        FNodes[Result].Value := 1;

        if LChar = Ord('Z') then
        begin
          FNodes[Result].Value := 2;
        end;
      end;
      Exit;
    end;
    Ord('1')..Ord('9'):
    begin

      if AInClass then
      begin
        Fail('Numeric backreferences are unavailable inside classes');
      end;
      Dec(FPosition);
      LScalar := Decimal;
      Result := Node(nkBackref);
      FNodes[Result].Value := LScalar;
      Exit;
    end;
    Ord('k'):
    begin

      if AInClass then
      begin
        Fail('Named backreferences are unavailable inside classes');
      end;
      Result := Node(nkBackref);

      if Take <> Ord('<') then
      begin
        Fail('Named backreferences use angle brackets');
      end;
      FNodes[Result].Name := ReadName(Ord('>'));
      Exit;
    end;
    Ord('Q'):
    begin

      if AInClass then
      begin
        Fail('Quoted sequences are unavailable inside classes');
      end;
      Result := Node(nkConcat);
      while Peek >= 0 do
      begin

        if (Peek = Ord('\')) and (FPosition + 1 < Length(FPattern)) and
          (FPattern[FPosition + 1] = Ord('E')) then
        begin
          Inc(FPosition, 2);
          Break;
        end;
        LLiteral := Node(nkLiteral);
        FNodes[LLiteral].Value := Take;
        Child(Result, LLiteral);
      end;
      Exit;
    end;
    Ord('n'): LChar := 10;
    Ord('r'): LChar := 13;
    Ord('t'): LChar := 9;
    Ord('f'): LChar := 12;
    Ord('v'): LChar := 11;
    Ord('a'): LChar := 7;
    Ord('e'): LChar := 27;
    Ord('0'):
    begin

      if (Peek >= Ord('0')) and (Peek <= Ord('9')) then
      begin
        Fail('Octal escapes are not part of the portable grammar');
      end;
      LChar := 0;
    end;
    Ord('x'), Ord('u'):
    begin

      if Peek = Ord('{') then
      begin
        LBraced := True;
        Take;
        LCount := 0;
        LScalar := 0;
        while (Peek >= 0) and (Peek <> Ord('}')) do
        begin

          if LCount >= 6 then
          begin
            Fail('A scalar escape has at most six hexadecimal digits');
          end;
          LLow := HexDigits(1);

          if LScalar > ($10FFFF - LLow) div 16 then
          begin
            Fail('Scalar escape is out of range');
          end;
          LScalar := LScalar * 16 + LLow;
          Inc(LCount);
        end;

        if LCount = 0 then
        begin
          Fail('A scalar escape requires hexadecimal digits');
        end;
        Expect(Ord('}'));
      end
      else
      begin
        LCount := 2;

        if LChar = Ord('u') then
        begin
          LCount := 4;
        end;
        LScalar := HexDigits(LCount);
      end;

      if (LScalar >= $D800) and (LScalar <= $DBFF) and
        (LChar = Ord('u')) and not LBraced then
      begin
        Expect(Ord('\'));
        Expect(Ord('u'));
        LLow := HexDigits(4);

        if (LLow < $DC00) or (LLow > $DFFF) then
        begin
          Fail('High surrogate escape requires its low surrogate');
        end;
        LScalar := $10000 + (LScalar - $D800) * $400 + LLow - $DC00;
      end;

      if (LScalar >= $D800) and (LScalar <= $DFFF) then
      begin
        Fail('An isolated surrogate is not a Unicode scalar');
      end;
      LChar := LScalar;
    end;
    else
    begin

      if ((LChar >= Ord('a')) and (LChar <= Ord('z'))) or
        ((LChar >= Ord('A')) and (LChar <= Ord('Z'))) then
      begin
        Fail('Unsupported alphabetic escape');
      end;
    end;
  end;
  APart.First := LChar;
  APart.Last := LChar;

  if AInClass then
  begin
    Exit(-1);
  end;
  Result := Node(nkLiteral);
  FNodes[Result].Value := LChar;
end;

function TCompiler.CharacterClass: Integer;
var
  LClass: TCharacterClass;
  LPart: TPart;
  LEnd: TPart;
  LCount: Integer;
  LOpCount: Integer;
  LOperator: TSetOperator;
  LIndex: Integer;

  function Member: TPart;
  begin
    Result := Default(TPart);
    Result.Kind := cpRange;

    if Peek = Ord('\') then
    begin
      Take;
      Escape(True, Result);
    end
    else if Peek = Ord('[') then
    begin
      Take;
      Result.Kind := cpNested;
      Result.First := CharacterClass;
    end
    else
    begin
      Result.First := Take;
      Result.Last := Result.First;
    end;
  end;

begin
  Enter;
  LClass := Default(TCharacterClass);
  LCount := 0;
  LOpCount := 0;

  if Peek = Ord('^') then
  begin
    Take;
    LClass.Negated := True;
  end;
  while (Peek >= 0) and (Peek <> Ord(']')) do
  begin

    if (FPosition + 1 < Length(FPattern)) and
      (((Peek = Ord('&')) and (FPattern[FPosition + 1] = Ord('&'))) or
       ((Peek = Ord('-')) and (FPattern[FPosition + 1] = Ord('-')))) then
    begin
      LOperator := csoIntersection;

      if Take = Ord('-') then
      begin
        LOperator := csoSubtraction;
      end;
      Take;

      if LCount = 0 then
      begin
        Fail('A set operation requires a left union');
      end;
      Expect(Ord('['));
      LIndex := CharacterClass;
      SetLength(LClass.Operators, LOpCount + 1);
      LClass.Operators[LOpCount].Kind := LOperator;
      LClass.Operators[LOpCount].ClassIndex := LIndex;
      Inc(LOpCount);
      Continue;
    end;

    if LOpCount > 0 then
    begin
      Fail('Union members must precede explicit nested set operations');
    end;
    LPart := Member;

    if (Peek = Ord('-')) and (FPosition + 1 < Length(FPattern)) and
      (FPattern[FPosition + 1] <> Ord(']')) and (FPattern[FPosition + 1] <> Ord('-')) then
    begin
      Take;
      LEnd := Member;

      if (LPart.Kind <> cpRange) or (LEnd.Kind <> cpRange) or LPart.Negated or
        LEnd.Negated or (LPart.First > LEnd.First) then
      begin
        Fail('A range requires ascending literal scalar endpoints');
      end;
      LPart.Last := LEnd.First;
    end;

    if LCount = Length(LClass.Parts) then
    begin
      SetLength(LClass.Parts, RegexCapacity(LCount, FTarget.FOptions.Budget.MaximumPatternScalars));
    end;
    LClass.Parts[LCount] := LPart;
    Inc(LCount);
  end;
  Expect(Ord(']'));
  SetLength(LClass.Parts, LCount);
  Result := Length(FTarget.FClasses);
  SetLength(FTarget.FClasses, Result + 1);
  FTarget.FClasses[Result] := LClass;
  Leave;
end;

function TCompiler.Width(ANode: Integer): Integer;
var
  LIndex: Integer;
  LWidth: Integer;
begin
  Result := 0;
  case FNodes[ANode].Kind of
    nkLiteral, nkClass, nkAny: Result := 1;
    nkBackref: Result := -1;
    nkCapture, nkAtomic: Result := Width(FNodes[ANode].Children[0]);
    nkRepeat:
    begin

      if FNodes[ANode].Minimum <> FNodes[ANode].Maximum then
      begin
        Exit(-1);
      end;
      LWidth := Width(FNodes[ANode].Children[0]);

      if LWidth < 0 then
      begin
        Exit(-1);
      end;

      if (LWidth > 0) and (FNodes[ANode].Minimum > High(Integer) div LWidth) then
      begin
        Fail('Lookbehind width exceeds Integer');
      end;
      Result := LWidth * FNodes[ANode].Minimum;
    end;
    nkConcat:
    begin
      for LIndex := 0 to FNodes[ANode].ChildCount - 1 do
      begin
        LWidth := Width(FNodes[ANode].Children[LIndex]);

        if LWidth < 0 then
        begin
          Exit(-1);
        end;

        if Result > High(Integer) - LWidth then
        begin
          Fail('Lookbehind width exceeds Integer');
        end;
        Inc(Result, LWidth);
      end;
    end;
    nkAlternate:
    begin
      Result := Width(FNodes[ANode].Children[0]);
      for LIndex := 1 to FNodes[ANode].ChildCount - 1 do
      begin

        if Result <> Width(FNodes[ANode].Children[LIndex]) then
        begin
          Exit(-1);
        end;
      end;
    end;
    nkEmpty, nkStart, nkEnd, nkWord, nkAssert: Result := 0;
  end;
end;

function TCompiler.Emit(AOp: TOp; ANode, ANext: Integer): Integer;
var
  LCapacity: Integer;
begin

  if FCodeCount >= FTarget.FOptions.Budget.MaximumInstructions then
  begin
    raise ENyxRegexLimit.Create('Regex instruction budget exceeded');
  end;

  if FCodeCount = Length(FTarget.FCode) then
  begin
    LCapacity := RegexCapacity(FCodeCount, FTarget.FOptions.Budget.MaximumInstructions);
    SetLength(FTarget.FCode, LCapacity);
  end;
  Result := FCodeCount;
  Inc(FCodeCount);
  FTarget.FCode[Result] := Default(TInstruction);
  FTarget.FCode[Result].Op := AOp;
  FTarget.FCode[Result].X := ANext;

  if ANode >= 0 then
  begin
    FTarget.FCode[Result].Value := FNodes[ANode].Value;
    FTarget.FCode[Result].Minimum := FNodes[ANode].Minimum;
    FTarget.FCode[Result].Maximum := FNodes[ANode].Maximum;
    FTarget.FCode[Result].Greedy := FNodes[ANode].Greedy;
    FTarget.FCode[Result].Negative := FNodes[ANode].Negative;
    FTarget.FCode[Result].Behind := FNodes[ANode].Behind;
    FTarget.FCode[Result].Mode := FNodes[ANode].Mode;
  end;
end;

function TCompiler.Code(ANode, ANext: Integer): Integer;
var
  LIndex: Integer;
  LNext: Integer;
  LBody: Integer;
  LCheck: Integer;
  LEnd: Integer;
  LSlot: Integer;
  LReference: TNyxRegexCaptureRef;
  LPrimitive: Integer;
  LCapture: Integer;
  LOp: TOp;
begin
  Result := ANext;
  case FNodes[ANode].Kind of
    nkEmpty: ;
    nkLiteral: Result := Emit(roChar, ANode, ANext);
    nkClass: Result := Emit(roClass, ANode, ANext);
    nkAny: Result := Emit(roAny, ANode, ANext);
    nkStart: Result := Emit(roStart, ANode, ANext);
    nkEnd: Result := Emit(roEnd, ANode, ANext);
    nkWord: Result := Emit(roWord, ANode, ANext);
    nkBackref:
    begin
      LReference := NyxRegexCapture(FNodes[ANode].Value);

      if FNodes[ANode].Name <> '' then
      begin
        LReference := NyxRegexCapture(NyxRegexGroup(FNodes[ANode].Name));
      end;
      Result := Emit(roBackref, ANode, ANext);
      FTarget.FCode[Result].Value := FTarget.CaptureIndex(LReference);
    end;
    nkConcat:
    begin
      for LIndex := FNodes[ANode].ChildCount - 1 downto 0 do
      begin
        Result := Code(FNodes[ANode].Children[LIndex], Result);
      end;
    end;
    nkAlternate:
    begin
      LIndex := FNodes[ANode].ChildCount - 1;
      Result := Code(FNodes[ANode].Children[LIndex], ANext);
      Dec(LIndex);
      while LIndex >= 0 do
      begin
        LBody := Code(FNodes[ANode].Children[LIndex], ANext);
        LNext := Emit(roSplit, ANode, LBody);
        FTarget.FCode[LNext].Y := Result;
        Result := LNext;
        Dec(LIndex);
      end;
    end;
    nkCapture:
    begin
      LEnd := Emit(roSaveEnd, ANode, ANext);
      LBody := Code(FNodes[ANode].Children[0], LEnd);
      Result := Emit(roSaveStart, ANode, LBody);
    end;
    nkRepeat:
    begin
      LPrimitive := FNodes[ANode].Children[0];
      LCapture := -1;

      if FNodes[LPrimitive].Kind = nkCapture then
      begin
        LCapture := FNodes[LPrimitive].Value;
        LPrimitive := FNodes[LPrimitive].Children[0];
      end;

      if FNodes[ANode].Greedy and
        (FNodes[LPrimitive].Kind in [nkLiteral, nkClass, nkAny]) then
      begin
        LOp := roRepeatAny;

        if FNodes[LPrimitive].Kind = nkLiteral then
        begin
          LOp := roRepeatChar;
        end
        else if FNodes[LPrimitive].Kind = nkClass then
        begin
          LOp := roRepeatClass;
        end;
        Result := Emit(LOp, LPrimitive, ANext);
        FTarget.FCode[Result].Minimum := FNodes[ANode].Minimum;
        FTarget.FCode[Result].Maximum := FNodes[ANode].Maximum;
        FTarget.FCode[Result].Y := LCapture;
        Exit;
      end;
      LSlot := FTarget.FLoops;
      Inc(FTarget.FLoops);
      LEnd := Emit(roRepeatEnd, ANode, -1);
      FTarget.FCode[LEnd].Value := LSlot;
      FTarget.FCode[LEnd].Y := ANext;
      LBody := Code(FNodes[ANode].Children[0], LEnd);
      LCheck := Emit(roRepeatCheck, ANode, LBody);
      FTarget.FCode[LCheck].Value := LSlot;
      FTarget.FCode[LCheck].Y := ANext;
      FTarget.FCode[LEnd].X := LCheck;
      Result := Emit(roRepeatInit, ANode, LCheck);
      FTarget.FCode[Result].Value := LSlot;
    end;
    nkAssert:
    begin
      LBody := Code(FNodes[ANode].Children[0], Emit(roAccept, -1, -1));
      Result := Emit(roAssert, ANode, ANext);
      FTarget.FCode[Result].Y := LBody;

      if FNodes[ANode].Behind then
      begin
        LCheck := Width(FNodes[ANode].Children[0]);

        if LCheck < 0 then
        begin
          Fail('Lookbehind requires one fixed Unicode-scalar width');
        end;
        FTarget.FCode[Result].Minimum := LCheck;
      end;
    end;
    nkAtomic:
    begin
      LSlot := FTarget.FAtomics;
      Inc(FTarget.FAtomics);
      LEnd := Emit(roAtomicEnd, ANode, ANext);
      FTarget.FCode[LEnd].Value := LSlot;
      LBody := Code(FNodes[ANode].Children[0], LEnd);
      Result := Emit(roAtomicStart, ANode, LBody);
      FTarget.FCode[Result].Value := LSlot;
    end;
  end;
end;

procedure TCompiler.Compile;
var
  LRoot: Integer;
  LEnd: Integer;
begin
  LRoot := Disjunction;
  Space;

  if Peek >= 0 then
  begin
    Fail('Unexpected closing group');
  end;
  LEnd := Emit(roAccept, -1, -1);
  FTarget.FEntry := Code(LRoot, LEnd);
  SetLength(FTarget.FCode, FCodeCount);
end;
