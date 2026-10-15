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


{ Invocation-local execution for nyx.regex. No DOM/LCL/host matcher is used. }

procedure TRunner.Tick;
begin

  if (FSteps and 255) = 0 then
  begin

    if (FScope <> nil) and FScope.Cancelled then
    begin
      raise ENyxRegexCancelled.Create('Regex search cancelled');
    end;
  end;

  if FSteps >= FRegex.FOptions.Budget.MaximumWorkSteps then
  begin
    raise ENyxRegexLimit.Create('Regex complete-search work budget exceeded');
  end;
  Inc(FSteps);
end;

procedure TRunner.Charge(ABytes: TNyxTextBytes);
begin

  if (ABytes < 0) or
    (ABytes > FRegex.FOptions.Budget.MaximumResultBytes - FResultBytes) then
  begin
    raise ENyxRegexLimit.Create('Regex complete-result allocation budget exceeded');
  end;
  Inc(FResultBytes, Integer(ABytes));
end;

constructor TRunner.Create(ARegex: TCompiledRegex; const AInput: TNyxText;
  const ACancellation: INyxCancellationScope);
var
  LCapacity: Integer;
  LIndex: Integer;
  LCount: Integer;
  LBefore: Integer;
  LScalar: Integer;
  LBytes: TNyxTextBytes;
begin
  inherited Create;
  FRegex := ARegex;
  FScope := ACancellation;
  FText := AInput;
  Tick;
  LCapacity := Min(Length(AInput), FRegex.FOptions.Budget.MaximumInputScalars);
  SetLength(FScalars, LCapacity);
  SetLength(FOffsets, LCapacity + 1);
  SetLength(FBytes, LCapacity + 1);
  LIndex := 1;
  LCount := 0;
  LBytes := 0;
  while LIndex <= Length(AInput) do
  begin
    Tick;

    if LCount >= FRegex.FOptions.Budget.MaximumInputScalars then
    begin
      raise ENyxRegexLimit.Create('Regex input exceeds its scalar budget');
    end;
    LBefore := LIndex;

    if not NyxNextScalar(AInput, LIndex, LScalar) then
    begin
      raise ENyxRegex.Create('Regex input contains malformed Unicode');
    end;
    FScalars[LCount] := LScalar;
    FOffsets[LCount] := LBefore;
    FBytes[LCount] := LBytes;

    if LScalar <= $7F then
    begin
      Inc(LBytes);
    end
    else if LScalar <= $7FF then
    begin
      Inc(LBytes, 2);
    end
    else if LScalar <= $FFFF then
    begin
      Inc(LBytes, 3);
    end
    else
    begin
      Inc(LBytes, 4);
    end;
    Inc(LCount);
  end;
  SetLength(FScalars, LCount);
  SetLength(FOffsets, LCount + 1);
  SetLength(FBytes, LCount + 1);
  FOffsets[LCount] := Length(AInput) + 1;
  FBytes[LCount] := LBytes;
end;

function TRunner.Initial(AStart: Integer): TState;
var
  LIndex: Integer;
begin
  Result := Default(TState);
  Result.PC := FRegex.FEntry;
  Result.Position := AStart;
  SetLength(Result.Captures, Length(FRegex.FNames) * 2);
  for LIndex := 0 to High(Result.Captures) do
  begin
    Tick;
    Result.Captures[LIndex] := -1;
  end;
  Result.Captures[0] := AStart;
  SetLength(Result.Counters, FRegex.FLoops * 2);
  SetLength(Result.Marks, FRegex.FAtomics);
end;

function TRunner.Clone(const AState: TState): TState;
var
  LIndex: Integer;
begin
  Result := Default(TState);
  Result.PC := AState.PC;
  Result.Position := AState.Position;
  SetLength(Result.Captures, Length(AState.Captures));
  SetLength(Result.Counters, Length(AState.Counters));
  SetLength(Result.Marks, Length(AState.Marks));
  for LIndex := 0 to High(AState.Captures) do
  begin
    Tick;
    Result.Captures[LIndex] := AState.Captures[LIndex];
  end;
  for LIndex := 0 to High(AState.Counters) do
  begin
    Tick;
    Result.Counters[LIndex] := AState.Counters[LIndex];
  end;
  for LIndex := 0 to High(AState.Marks) do
  begin
    Tick;
    Result.Marks[LIndex] := AState.Marks[LIndex];
  end;
end;

function TRunner.Equal(AA, AB: Integer; ACase: TNyxRegexCase): Boolean;
begin
  Tick;

  if ACase = nrcUnicode then
  begin
    AA := NyxSimpleFold(AA);
    AB := NyxSimpleFold(AB);
  end
  else if ACase = nrcASCII then
  begin

    if (AA >= Ord('A')) and (AA <= Ord('Z')) then
    begin
      Inc(AA, 32);
    end;

    if (AB >= Ord('A')) and (AB <= Ord('Z')) then
    begin
      Inc(AB, 32);
    end;
  end;
  Result := AA = AB;
end;

function TRunner.Word(AScalar: Integer): Boolean;
begin

  if FRegex.FOptions.Classes = nrgASCII then
  begin
    Exit(((AScalar >= Ord('a')) and (AScalar <= Ord('z'))) or
      ((AScalar >= Ord('A')) and (AScalar <= Ord('Z'))) or
      ((AScalar >= Ord('0')) and (AScalar <= Ord('9'))) or (AScalar = Ord('_')));
  end;
  Result := NyxScalarWord(AScalar);
end;

function TRunner.LineBreak(AScalar: Integer): Boolean;
begin
  Result := RegexLineBreak(AScalar);
end;

function TRunner.PartMatches(const APart: TPart; AScalar: Integer;
  const AMode: TMode): Boolean;
var
  LValue: Integer;
  LIndex: Integer;

  function Contains(AValue: Integer): Boolean;
  begin
    Tick;
    Result := False;
    case APart.Kind of
      cpRange: Result := (AValue >= APart.First) and (AValue <= APart.Last);
      cpDigit:
      begin
        Result := NyxScalarCategory(AValue) = nucDecimalNumber;

        if FRegex.FOptions.Classes = nrgASCII then
        begin
          Result := (AValue >= Ord('0')) and (AValue <= Ord('9'));
        end;
      end;
      cpSpace:
      begin
        Result := NyxScalarWhitespace(AValue);

        if (FRegex.FOptions.Classes = nrgASCII) and (APart.First <> 1) then
        begin
          Result := ((AValue >= 9) and (AValue <= 13)) or (AValue = 32);
        end;
      end;
      cpWord: Result := Word(AValue);
      cpCategory: Result := NyxScalarCategory(AValue) in APart.Categories;
      cpAssigned: Result := NyxScalarCategory(AValue) <> nucUnassigned;
      cpNested: Result := ClassMatches(APart.First, AValue, AMode);
    end;
  end;

begin
  Result := Contains(AScalar);

  if not Result and (APart.Kind <> cpNested) then
  begin

    if AMode.Comparison = nrcUnicode then
    begin
      LIndex := 1;
      while NyxScalarCaseEquivalent(AScalar, LIndex, LValue) do
      begin
        Inc(LIndex);

        if Contains(LValue) then
        begin
          Result := True;
          Break;
        end;
      end;
    end
    else if AMode.Comparison = nrcASCII then
    begin

      if (AScalar >= Ord('A')) and (AScalar <= Ord('Z')) then
      begin
        Result := Contains(AScalar + 32);
      end
      else if (AScalar >= Ord('a')) and (AScalar <= Ord('z')) then
      begin
        Result := Contains(AScalar - 32);
      end;
    end;
  end;

  if APart.Negated then
  begin
    Result := not Result;
  end;
end;

function TRunner.ClassMatches(AClass, AScalar: Integer; const AMode: TMode): Boolean;
var
  LIndex: Integer;
  LOther: Boolean;
begin
  Tick;
  Result := False;
  for LIndex := 0 to High(FRegex.FClasses[AClass].Parts) do
  begin

    if PartMatches(FRegex.FClasses[AClass].Parts[LIndex], AScalar, AMode) then
    begin
      Result := True;
      Break;
    end;
  end;
  for LIndex := 0 to High(FRegex.FClasses[AClass].Operators) do
  begin

    if not Result then
    begin
      Break;
    end;
    LOther := ClassMatches(FRegex.FClasses[AClass].Operators[LIndex].ClassIndex, AScalar, AMode);

    if FRegex.FClasses[AClass].Operators[LIndex].Kind = csoSubtraction then
    begin
      LOther := not LOther;
    end;
    Result := Result and LOther;
  end;

  if FRegex.FClasses[AClass].Negated then
  begin
    Result := not Result;
  end;
end;

function TRunner.Run(const AInitial: TState; ARequiredEnd, ADepth: Integer;
  out AResult: TState): Boolean;
var
  LState: TState;
  LStack: array of TAlternative;
  LCount: Integer;
  LInstruction: TInstruction;
  LFail: Boolean;
  LOther: TState;
  LAssertionResult: TState;
  LAlternative: TAlternative;
  LPart: TPart;
  LLeft: Boolean;
  LRight: Boolean;
  LFound: Boolean;
  LSlot: Integer;
  LIndex: Integer;
  LStart: Integer;
  LFinish: Integer;
  LLength: Integer;
  LMark: Integer;
  LRequiredEnd: Integer;

  procedure Push(const AFrame: TState; APC: Integer);
  begin

    if FStates >= FRegex.FOptions.Budget.MaximumBacktrackStates then
    begin
      raise ENyxRegexLimit.Create('Regex live backtrack-state budget exceeded');
    end;

    if LCount = Length(LStack) then
    begin
      SetLength(LStack, RegexCapacity(LCount, FRegex.FOptions.Budget.MaximumBacktrackStates));
    end;
    LStack[LCount] := Default(TAlternative);
    LStack[LCount].Frame := Clone(AFrame);
    LStack[LCount].Frame.PC := APC;
    Inc(LCount);
    Inc(FStates);
  end;

  procedure Drop;
  begin
    Dec(LCount);
    Dec(FStates);
    LStack[LCount] := Default(TAlternative);
  end;

  function Pop: Boolean;
  begin
    Result := LCount > 0;

    if not Result then
    begin
      Exit;
    end;
    Tick;
    LAlternative := LStack[LCount - 1];

    if LAlternative.Ranged then
    begin
      LState := Clone(LAlternative.Frame);
      LState.Position := LAlternative.NextPosition;

      if (LAlternative.Group >= 0) and (LState.Position > LAlternative.Frame.Position) then
      begin
        LState.Captures[LAlternative.Group * 2] := LState.Position - 1;
        LState.Captures[LAlternative.Group * 2 + 1] := LState.Position;
      end;

      if LAlternative.NextPosition = LAlternative.Minimum then
      begin
        Drop;
      end
      else
      begin
        Dec(LStack[LCount - 1].NextPosition);
      end;
    end
    else
    begin
      LState := LAlternative.Frame;
      Drop;
    end;
  end;

  function UnitMatches(AOp: TOp; AScalar: Integer): Boolean;
  begin
    Result := False;
    case AOp of
      roChar, roRepeatChar: Result := Equal(AScalar, LInstruction.Value, LInstruction.Mode.Comparison);
      roClass, roRepeatClass: Result := ClassMatches(LInstruction.Value, AScalar, LInstruction.Mode);
      roAny, roRepeatAny: Result := LInstruction.Mode.DotAll or not LineBreak(AScalar);
      roAccept, roSplit, roSaveStart, roSaveEnd, roRepeatInit, roRepeatCheck,
      roRepeatEnd, roStart, roEnd, roWord, roBackref, roAssert, roAtomicStart,
      roAtomicEnd:
      begin
        raise ENyxRegex.Create('Internal regex primitive opcode mismatch');
      end;
    end;
  end;

begin
  AResult := Default(TState);

  if ADepth > FRegex.FOptions.Budget.MaximumNesting then
  begin
    raise ENyxRegexLimit.Create('Regex assertion execution nesting budget exceeded');
  end;
  LState := Clone(AInitial);
  LCount := 0;
  try
    while True do
    begin
      Tick;
      LInstruction := FRegex.FCode[LState.PC];
      LFail := False;
      case LInstruction.Op of
        roAccept:
        begin

          if (ARequiredEnd < 0) or (LState.Position = ARequiredEnd) then
          begin
            AResult := LState;
            Exit(True);
          end;
          LFail := True;
        end;
        roChar, roClass, roAny:
        begin
          LFail := LState.Position >= Length(FScalars);

          if not LFail then
          begin
            LFail := not UnitMatches(LInstruction.Op, FScalars[LState.Position]);
          end;

          if not LFail then
          begin
            Inc(LState.Position);
          end;
          LState.PC := LInstruction.X;
        end;
        roRepeatChar, roRepeatClass, roRepeatAny:
        begin
          LStart := LState.Position;
          while LState.Position < Length(FScalars) do
          begin
            Tick;

            if (LInstruction.Maximum >= 0) and
              (LState.Position - LStart >= LInstruction.Maximum) then
            begin
              Break;
            end;

            if not UnitMatches(LInstruction.Op, FScalars[LState.Position]) then
            begin
              Break;
            end;
            Inc(LState.Position);
          end;
          LLength := LState.Position - LStart;
          LFail := LLength < LInstruction.Minimum;

          if not LFail then
          begin
            { One range snapshot represents every shorter greedy prefix.
              Preserve the pre-loop capture frame for the zero-iteration case. }
            if LLength > LInstruction.Minimum then
            begin
              LOther := LState;
              LOther.Position := LStart;
              Push(LOther, LInstruction.X);
              LStack[LCount - 1].Ranged := True;
              LStack[LCount - 1].Minimum := LStart + LInstruction.Minimum;
              LStack[LCount - 1].NextPosition := LState.Position - 1;
              LStack[LCount - 1].Group := LInstruction.Y;
            end;

            if (LInstruction.Y >= 0) and (LLength > 0) then
            begin
              LState.Captures[LInstruction.Y * 2] := LState.Position - 1;
              LState.Captures[LInstruction.Y * 2 + 1] := LState.Position;
            end;
          end;
          LState.PC := LInstruction.X;
        end;
        roSplit:
        begin
          Push(LState, LInstruction.Y);
          LState.PC := LInstruction.X;
        end;
        roSaveStart:
        begin
          LState.Captures[LInstruction.Value * 2] := LState.Position;
          LState.Captures[LInstruction.Value * 2 + 1] := -1;
          LState.PC := LInstruction.X;
        end;
        roSaveEnd:
        begin
          LState.Captures[LInstruction.Value * 2 + 1] := LState.Position;
          LState.PC := LInstruction.X;
        end;
        roRepeatInit:
        begin
          LState.Counters[LInstruction.Value * 2] := 0;
          LState.Counters[LInstruction.Value * 2 + 1] := -1;
          LState.PC := LInstruction.X;
        end;
        roRepeatCheck:
        begin
          LSlot := LInstruction.Value * 2;

          if LState.Counters[LSlot] < LInstruction.Minimum then
          begin
            LState.Counters[LSlot + 1] := LState.Position;
            LState.PC := LInstruction.X;
          end
          else if (LInstruction.Maximum >= 0) and
            (LState.Counters[LSlot] >= LInstruction.Maximum) then
          begin
            LState.PC := LInstruction.Y;
          end
          else if LInstruction.Greedy then
          begin
            Push(LState, LInstruction.Y);
            LState.Counters[LSlot + 1] := LState.Position;
            LState.PC := LInstruction.X;
          end
          else
          begin
            LOther := Clone(LState);
            LOther.Counters[LSlot + 1] := LOther.Position;
            Push(LOther, LInstruction.X);
            LState.PC := LInstruction.Y;
          end;
        end;
        roRepeatEnd:
        begin
          LSlot := LInstruction.Value * 2;

          if LState.Counters[LSlot] = High(Integer) then
          begin
            raise ENyxRegexLimit.Create('Regex repetition count exceeds Integer');
          end;
          Inc(LState.Counters[LSlot]);
          LState.PC := LInstruction.X;

          if (LState.Position = LState.Counters[LSlot + 1]) and
            (LState.Counters[LSlot] >= LInstruction.Minimum) then
          begin
            LState.PC := LInstruction.Y;
          end;
        end;
        roStart:
        begin
          LFound := LState.Position = 0;

          if not LFound and LInstruction.Mode.Multiline and (LInstruction.Value = 0) then
          begin
            LFound := LineBreak(FScalars[LState.Position - 1]) and
              not ((FScalars[LState.Position - 1] = 13) and
                (LState.Position < Length(FScalars)) and (FScalars[LState.Position] = 10));
          end;
          LFail := not LFound;
          LState.PC := LInstruction.X;
        end;
        roEnd:
        begin
          LFound := LState.Position = Length(FScalars);

          if not LFound and (LInstruction.Value = 2) then
          begin
            LFound := ((LState.Position = Length(FScalars) - 1) and
              LineBreak(FScalars[LState.Position])) or
              ((LState.Position = Length(FScalars) - 2) and
                (FScalars[LState.Position] = 13) and (FScalars[LState.Position + 1] = 10));
          end
          else if not LFound and (LInstruction.Value = 0) and LInstruction.Mode.Multiline then
          begin
            LFound := LineBreak(FScalars[LState.Position]);
          end;

          if LFound and (LState.Position > 0) and
            (LState.Position < Length(FScalars)) and
            (FScalars[LState.Position] = 10) and (FScalars[LState.Position - 1] = 13) then
          begin
            LFound := False;
          end;
          LFail := not LFound;
          LState.PC := LInstruction.X;
        end;
        roWord:
        begin
          LPart := Default(TPart);
          LPart.Kind := cpWord;
          LLeft := False;
          LRight := False;

          if LState.Position > 0 then
          begin
            LLeft := PartMatches(LPart, FScalars[LState.Position - 1], LInstruction.Mode);
          end;

          if LState.Position < Length(FScalars) then
          begin
            LRight := PartMatches(LPart, FScalars[LState.Position], LInstruction.Mode);
          end;
          LFound := LLeft <> LRight;
          LFail := LFound = LInstruction.Negative;
          LState.PC := LInstruction.X;
        end;
        roBackref:
        begin
          LStart := LState.Captures[LInstruction.Value * 2];
          LFinish := LState.Captures[LInstruction.Value * 2 + 1];
          LFail := (LStart < 0) or (LFinish < LStart);

          if not LFail then
          begin
            LLength := LFinish - LStart;
            LFail := LLength > Length(FScalars) - LState.Position;

            if not LFail then
            begin
              for LIndex := 0 to LLength - 1 do
              begin

                if not Equal(FScalars[LStart + LIndex],
                  FScalars[LState.Position + LIndex], LInstruction.Mode.Comparison) then
                begin
                  LFail := True;
                  Break;
                end;
              end;

              if not LFail then
              begin
                Inc(LState.Position, LLength);
              end;
            end;
          end;
          LState.PC := LInstruction.X;
        end;
        roAssert:
        begin
          LFound := False;
          LOther := Clone(LState);
          LOther.PC := LInstruction.Y;
          LRequiredEnd := -1;

          if LInstruction.Behind then
          begin
            LOther.Position := LState.Position - LInstruction.Minimum;
            LRequiredEnd := LState.Position;
          end;

          if LOther.Position >= 0 then
          begin
            LFound := Run(LOther, LRequiredEnd, ADepth + 1, LAssertionResult);
          end;
          LFail := LFound = LInstruction.Negative;

          if LFound and not LInstruction.Negative then
          begin
            LAssertionResult.Position := LState.Position;
            LState := LAssertionResult;
          end;
          LState.PC := LInstruction.X;
        end;
        roAtomicStart:
        begin
          LState.Marks[LInstruction.Value] := LCount;
          LState.PC := LInstruction.X;
        end;
        roAtomicEnd:
        begin
          LMark := LState.Marks[LInstruction.Value];

          if (LMark < 0) or (LMark > LCount) then
          begin
            raise ENyxRegex.Create('Internal regex atomic boundary mismatch');
          end;
          while LCount > LMark do
          begin
            Drop;
          end;
          LState.PC := LInstruction.X;
        end;
      end;

      if LFail and not Pop then
      begin
        Exit(False);
      end;
    end;
  finally
    while LCount > 0 do
    begin
      Drop;
    end;
  end;
end;

function TRunner.Snapshot(const AState: TState; AStart: Integer): TNyxRegexMatch;
var
  LGroups: array of TNyxRegexCapture;
  LIndex: Integer;
  LFirst: Integer;
  LLast: Integer;
  LBytes: TNyxTextBytes;
begin
  Charge(128 + 6 * (FBytes[AState.Position] - FBytes[AStart]));
  SetLength(LGroups, Length(FRegex.FNames));
  for LIndex := 0 to High(LGroups) do
  begin
    Tick;
    LGroups[LIndex] := Default(TNyxRegexCapture);
    LGroups[LIndex].Name := FRegex.FNames[LIndex];
    LFirst := AState.Captures[LIndex * 2];
    LLast := AState.Captures[LIndex * 2 + 1];

    if LIndex = 0 then
    begin
      LFirst := AStart;
      LLast := AState.Position;
    end;
    LBytes := 0;

    if (LFirst >= 0) and (LLast >= LFirst) then
    begin
      LBytes := FBytes[LLast] - FBytes[LFirst];
    end;
    Charge(128 + 6 * LBytes + 6 * NyxUTF8ByteCount(FRegex.FNames[LIndex]));

    if (LFirst >= 0) and (LLast >= LFirst) then
    begin
      LGroups[LIndex].Span := NyxRegexSpan(LFirst, LLast - LFirst);
      LGroups[LIndex].Value := Copy(FText, FOffsets[LFirst], FOffsets[LLast] - FOffsets[LFirst]);
    end;
  end;
  Result := NyxRegexMatch(LGroups);
end;

function TRunner.Find(AFirstOnly: Boolean): TNyxRegexMatches;
var
  LItems: array of TNyxRegexMatch;
  LCount: Integer;
  LPosition: Integer;
  LInitial: TState;
  LResult: TState;
begin
  LItems := nil;
  LCount := 0;
  LPosition := 0;
  while LPosition <= Length(FScalars) do
  begin
    Tick;
    LInitial := Initial(LPosition);

    if not Run(LInitial, -1, 0, LResult) then
    begin
      Inc(LPosition);
      Continue;
    end;

    if LCount >= FRegex.FOptions.Budget.MaximumMatches then
    begin
      raise ENyxRegexLimit.Create('Regex complete-search match count exceeded');
    end;

    if LCount = Length(LItems) then
    begin
      SetLength(LItems, RegexCapacity(LCount, FRegex.FOptions.Budget.MaximumMatches));
    end;
    LItems[LCount] := Snapshot(LResult, LPosition);
    Inc(LCount);

    if AFirstOnly then
    begin
      Break;
    end;

    if LResult.Position = LPosition then
    begin
      Inc(LPosition);
    end
    else
    begin
      LPosition := LResult.Position;
    end;
  end;
  SetLength(LItems, LCount);
  Result := NyxRegexMatches(LItems);
end;

function TRunner.HasMatch: Boolean;
var
  LPosition: Integer;
  LInitial: TState;
  LResult: TState;
begin
  for LPosition := 0 to Length(FScalars) do
  begin
    Tick;
    LInitial := Initial(LPosition);

    if Run(LInitial, -1, 0, LResult) then
    begin
      Exit(True);
    end;
  end;
  Result := False;
end;
