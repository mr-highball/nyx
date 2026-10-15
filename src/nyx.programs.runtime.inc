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



{ Invocation-local interpreter for nyx.programs. No instruction publishes a live
  edit. Only a complete result exposes its detached outputs/proposal journal. }

function TProgramResult.GetOutputs: INyxProgramValues;
begin
  Result := FOutputs.Copy;
end;

function TProgramResult.GetProposalCount: Integer;
begin
  Result := Length(FProposals);
end;

function TProgramResult.Proposal(AIndex: Integer): INyxProgramProposal;
begin

  if (AIndex < 0) or (AIndex >= Length(FProposals)) then
  begin
    raise ENyxProgram.Create(npdName, Default(TNyxProgramSpan), 'Proposal index is out of range');
  end;
  Result := FProposals[AIndex];
end;

function TProgramResult.GetSteps: Integer;
begin
  Result := FSteps;
end;

function TProgramResult.GetAllocatedBytes: Integer;
begin
  Result := FAllocation;
end;

procedure TRunner.Reject(AKind: TNyxProgramDiagnosticKind; const AMessage: TNyxText);
begin
  raise ENyxProgram.Create(AKind, FSpan, AMessage);
end;

procedure TRunner.CheckClock;
var
  LNow: Double;
begin

  if FProgram.FLimits.MaximumMilliseconds = 0 then
  begin
    Exit;
  end;
  LNow := FOptions.TimeSource.Milliseconds;

  if IsNan(LNow) or IsInfinite(LNow) or (LNow < FLastTime) then
  begin
    Reject(npdRuntime, 'Program clock must be finite and monotonic');
  end;
  FLastTime := LNow;

  if LNow - FStarted >= FProgram.FLimits.MaximumMilliseconds then
  begin
    Reject(npdLimit, 'Program elapsed-time budget exceeded');
  end;
end;

procedure TRunner.Tick;
begin

  if (FSteps and 63) = 0 then
  begin

    if (FOptions.CancellationScope <> nil) and FOptions.CancellationScope.Cancelled then
    begin
      Reject(npdCancelled, 'Behavior program cancelled');
    end;
    CheckClock;
  end;

  if FSteps >= FProgram.FLimits.MaximumSteps then
  begin
    Reject(npdLimit, 'Program complete-run step budget exceeded');
  end;
  Inc(FSteps);
end;

procedure TRunner.Charge(ABytes: TNyxTextBytes);
begin

  if (ABytes < 0) or (ABytes > FProgram.FLimits.MaximumAllocationBytes - FAllocation) then
  begin
    Reject(npdLimit, 'Program cumulative allocation budget exceeded');
  end;
  Inc(FAllocation, Integer(ABytes));
end;

procedure TRunner.AdmitValue(const AValue: INyxProgramValue);
var
  LBytes: TNyxTextBytes;
begin

  if (AValue = nil) or not AValue.Defined then
  begin
    Reject(npdRuntime, 'An unassigned value cannot enter program execution');
  end;
  LBytes := AValue.ByteSize;

  if LBytes > FProgram.FLimits.MaximumValueBytes then
  begin
    Reject(npdLimit, 'Program value byte budget exceeded');
  end;
  Charge(LBytes);
end;

procedure TRunner.Push(const AValue: INyxProgramValue);
begin

  if FStackCount >= FProgram.FLimits.MaximumStackValues then
  begin
    Reject(npdLimit, 'Program operand stack budget exceeded');
  end;
  AdmitValue(AValue);

  if FStackCount = Length(FStack) then
  begin
    SetLength(FStack, ProgramCapacity(FStackCount, FProgram.FLimits.MaximumStackValues));
  end;
  FStack[FStackCount] := AValue.Copy;
  Inc(FStackCount);
end;

function TRunner.Pop: INyxProgramValue;
begin

  if FStackCount < 1 then
  begin
    Reject(npdRuntime, 'Internal program operand stack underflow');
  end;
  Dec(FStackCount);
  Result := FStack[FStackCount].Copy;
  FStack[FStackCount] := Default(INyxProgramValue);
end;

function TRunner.FrameFor(AScope: Integer): Integer;
begin
  Result := High(FFrames);
  while (Result >= 0) and (FFrames[Result].Routine <> AScope) do
  begin
    Result := FFrames[Result].StaticParent;
  end;

  if Result < 0 then
  begin
    Reject(npdRuntime, 'A lexical program frame is unavailable');
  end;
end;

function TRunner.Read(AScope, ASlot: Integer): INyxProgramValue;
var
  LFrame: Integer;
begin
  LFrame := FrameFor(AScope);

  if FFrames[LFrame].Values[ASlot] = nil then
  begin
    Reject(npdRuntime, 'Variable read before assignment: ' +
      FProgram.FRoutines[AScope].Variables[ASlot].Name.Name);
  end;
  Result := FFrames[LFrame].Values[ASlot].Copy;
end;

procedure TRunner.Write(AScope, ASlot: Integer; const AValue: INyxProgramValue);
var
  LFrame: Integer;
  LIndex: Integer;
begin
  LFrame := FrameFor(AScope);

  if FProgram.FRoutines[AScope].Variables[ASlot].ReadOnly then
  begin
    Reject(npdRuntime, 'A read-only program value cannot be assigned');
  end;
  for LIndex := 0 to High(FGuards) do
  begin

    if (FGuards[LIndex].Frame = LFrame) and (FGuards[LIndex].Slot = ASlot) then
    begin
      Reject(npdRuntime, 'A captured active For counter cannot be assigned');
    end;
  end;

  if not AValue.ValueType.Same(FProgram.FRoutines[AScope].Variables[ASlot].ValueType) then
  begin
    Reject(npdType, 'A program write changed its declared value family');
  end;
  AdmitValue(AValue);
  FFrames[LFrame].Values[ASlot] := AValue.Copy;
end;

function TRunner.Arguments(ACount: Integer): INyxProgramArguments;
var
  LValues: TValues;
  LIndex: Integer;
begin
  Charge(TNyxTextBytes(ACount) * 32);
  SetLength(LValues, ACount);
  for LIndex := ACount - 1 downto 0 do
  begin
    LValues[LIndex] := Pop;
  end;
  Result := NyxProgramArguments(LValues);
end;

procedure TRunner.Call(ARoutine: Integer; const AArguments: INyxProgramArguments);
var
  LIndex: Integer;
  LFrame: Integer;
  LParent: Integer;
  LValue: INyxProgramValue;
begin

  if Length(FFrames) >= FProgram.FLimits.MaximumCallDepth then
  begin
    Reject(npdLimit, 'Program call depth budget exceeded');
  end;

  if AArguments.Count <> FProgram.FRoutines[ARoutine].Parameters then
  begin
    Reject(npdType, 'Program call arguments changed after compilation');
  end;
  LParent := -1;

  if ARoutine <> 0 then
  begin
    LParent := FrameFor(FProgram.FRoutines[ARoutine].Parent);
  end;
  Charge(64 + TNyxTextBytes(Length(FProgram.FRoutines[ARoutine].Variables)) * 32);
  LFrame := Length(FFrames);
  SetLength(FFrames, LFrame + 1);
  FFrames[LFrame] := TFrame.Create;
  FFrames[LFrame].Routine := ARoutine;
  FFrames[LFrame].StaticParent := LParent;
  FFrames[LFrame].ReturnPC := FPC;
  FFrames[LFrame].OperandBase := FStackCount;
  SetLength(FFrames[LFrame].Values, Length(FProgram.FRoutines[ARoutine].Variables));
  for LIndex := 0 to High(FFrames[LFrame].Values) do
  begin
    LValue := Default(INyxProgramValue);

    if LIndex < AArguments.Count then
    begin
      LValue := AArguments.Item(LIndex);

      if not LValue.ValueType.Same(FProgram.FRoutines[ARoutine].Variables[LIndex].ValueType) then
      begin
        Reject(npdType, 'Program parameter has a different declared type');
      end;
    end
    else if FProgram.FRoutines[ARoutine].Variables[LIndex].Initial <> nil then
    begin
      LValue := FProgram.FRoutines[ARoutine].Variables[LIndex].Initial.Copy;
    end;

    if LValue <> nil then
    begin
      AdmitValue(LValue);
    end;
    FFrames[LFrame].Values[LIndex] := LValue;
  end;
  FPC := FProgram.FRoutines[ARoutine].Entry;
end;

procedure TRunner.Return;
var
  LFrame: Integer;
  LResult: INyxProgramValue;
  LReturnPC: Integer;
begin
  LFrame := High(FFrames);

  if FStackCount <> FFrames[LFrame].OperandBase then
  begin
    Reject(npdRuntime, 'Internal program operand balance mismatch');
  end;
  while (Length(FGuards) > 0) and (FGuards[High(FGuards)].Owner = LFrame) do
  begin
    SetLength(FGuards, Length(FGuards) - 1);
  end;

  if LFrame = 0 then
  begin
    FPC := -1;
    Exit;
  end;
  LResult := Default(INyxProgramValue);

  if FProgram.FRoutines[FFrames[LFrame].Routine].ResultSlot >= 0 then
  begin
    LResult := Read(FFrames[LFrame].Routine,
      FProgram.FRoutines[FFrames[LFrame].Routine].ResultSlot);
  end;
  LReturnPC := FFrames[LFrame].ReturnPC;
  FFrames[LFrame].Free;
  SetLength(FFrames, LFrame);
  FPC := LReturnPC;

  if LResult <> nil then
  begin
    Push(LResult);
  end;
end;

constructor TRunner.Create(AProgram: TCompiledProgram; const AInputs: INyxProgramValues;
  const AOptions: INyxProgramRunOptions);
var
  LIndex: Integer;
  LInputs: Integer;
  LPort: TNyxProgramPort;
  LValue: INyxProgramValue;
begin
  inherited Create;
  FProgram := AProgram;
  FOptions := AOptions;
  FSpan := FProgram.FRoutines[0].Declaration;
  FPC := -1;

  if AOptions = nil then
  begin
    Self.Reject(npdType, 'Behavior runtime options must be initialized');
  end;
  FOptions.Validate;

  if AInputs = nil then
  begin
    Self.Reject(npdType, 'Behavior inputs require an initialized owned frame');
  end;
  for LIndex := 0 to High(FProgram.FCapabilities) do
  begin

    if not FOptions.Permissions.Contains(FProgram.FCapabilities[LIndex]) then
    begin
      FSpan := FProgram.FCapabilitySpans[LIndex];
      Self.Reject(npdCapability, 'Behavior capability was not granted: ' + FProgram.FCapabilities[LIndex].Name);
    end;
  end;

  if FProgram.FNeedsQueries and (FOptions.QuerySnapshot = nil) then
  begin
    Self.Reject(npdCapability, 'Behavior queries require an explicit frozen host snapshot');
  end;

  if FProgram.FLimits.MaximumMilliseconds > 0 then
  begin

    if FOptions.TimeSource = nil then
    begin
      Self.Reject(npdCapability, 'An elapsed-time budget requires an explicit monotonic clock');
    end;
    FStarted := FOptions.TimeSource.Milliseconds;

    if IsNan(FStarted) or IsInfinite(FStarted) then
    begin
      Self.Reject(npdRuntime, 'Program clock must be finite and monotonic');
    end;
    FLastTime := FStarted;
  end;
  Tick;
  Call(0, NyxProgramArguments([]));
  LInputs := 0;
  for LIndex := 0 to FProgram.FContract.PortCount - 1 do
  begin
    LPort := FProgram.FContract.Port(LIndex);

    if LPort.Direction = npvInput then
    begin
      Inc(LInputs);

      if not AInputs.Has(LPort.Name) then
      begin
        Self.Reject(npdName, 'Missing behavior input: ' + LPort.Name.Name);
      end;
      LValue := AInputs.Value(LPort.Name);

      if (LValue = nil) or not LValue.ValueType.Same(LPort.ValueType) then
      begin
        Self.Reject(npdType, 'Behavior input has a different declared family: ' + LPort.Name.Name);
      end;
      AdmitValue(LValue);
      FFrames[0].Values[LIndex] := LValue.Copy;
    end;
  end;

  if AInputs.Count <> LInputs then
  begin
    Self.Reject(npdName, 'Behavior inputs contain undeclared or output-only values');
  end;
end;

{ Numeric widening is explicit at typed call/store boundaries. Binary operators
  may promote Integer operands to finite Double as Pascal arithmetic specifies. }
destructor TRunner.Destroy;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FFrames) do
  begin
    FFrames[LIndex].Free;
  end;
  inherited Destroy;
end;

function ProgramNumber(const AValue: INyxProgramValue): Double;
begin

  if AValue.ValueType.Kind = npkInteger then
  begin
    Exit(AValue.AsInteger);
  end;
  Result := AValue.AsNumber;
end;

function ProgramInteger(AValue: Double; const ASpan: TNyxProgramSpan): INyxProgramValue;
begin

  if IsNan(AValue) or IsInfinite(AValue) or (AValue < -2147483648.0) or
    (AValue > 2147483647.0) or (Frac(AValue) <> 0) then
  begin
    raise ENyxProgram.Create(npdRuntime, ASpan, 'Integer arithmetic overflow');
  end;
  Result := NyxProgramValue(Integer(Trunc(AValue)));
end;

function TRunner.Binary(AKind: TBinary;
  const ALeft, ARight: INyxProgramValue): INyxProgramValue;
var
  LA: Double;
  LB: Double;
  LNumber: Double;
  LCompare: Integer;
  LTextA: TNyxText;
  LTextB: TNyxText;
  LIndexA: Integer;
  LIndexB: Integer;
  LScalarA: Integer;
  LScalarB: Integer;
  LBytes: TNyxTextBytes;
  LBoolean: Boolean;
begin

  if (AKind = pbAdd) and (ALeft.ValueType.Kind = npkText) then
  begin
    LTextA := ALeft.AsText;
    LTextB := ARight.AsText;
    LBytes := TNyxTextBytes(NyxUTF8ByteCount(LTextA)) + NyxUTF8ByteCount(LTextB);

    if LBytes > FProgram.FLimits.MaximumValueBytes - 32 then
    begin
      Reject(npdLimit, 'Concatenated text exceeds the value byte budget');
    end;
    Exit(NyxProgramValue(LTextA + LTextB));
  end;
  LCompare := 0;

  if ALeft.ValueType.Kind = npkText then
  begin
    LTextA := ALeft.AsText;
    LTextB := ARight.AsText;
    LIndexA := 1;
    LIndexB := 1;
    while (LIndexA <= Length(LTextA)) and (LIndexB <= Length(LTextB)) do
    begin
      Tick;
      NyxNextScalar(LTextA, LIndexA, LScalarA);
      NyxNextScalar(LTextB, LIndexB, LScalarB);

      if LScalarA <> LScalarB then
      begin
        LCompare := -1;

        if LScalarA > LScalarB then
        begin
          LCompare := 1;
        end;
        Break;
      end;
    end;

    if LCompare = 0 then
    begin
      LCompare := Ord(LIndexA <= Length(LTextA)) - Ord(LIndexB <= Length(LTextB));
    end;
  end
  else if ALeft.ValueType.Kind = npkBoolean then
  begin
    LCompare := Ord(ALeft.AsBoolean) - Ord(ARight.AsBoolean);
  end
  else if ALeft.ValueType.Kind = npkHandle then
  begin
    LCompare := Ord(ALeft.AsHandle <> ARight.AsHandle);
  end
  else
  begin
    LA := ProgramNumber(ALeft);
    LB := ProgramNumber(ARight);

    if AKind in [pbAdd, pbSubtract, pbMultiply, pbDivide, pbIntegerDivide, pbModulo] then
    begin

      if (AKind in [pbDivide, pbIntegerDivide, pbModulo]) and (LB = 0) then
      begin
        Reject(npdRuntime, 'Division by zero');
      end;
      LNumber := 0;
      case AKind of
        pbAdd: LNumber := LA + LB;
        pbSubtract: LNumber := LA - LB;
        pbMultiply: LNumber := LA * LB;
        pbDivide: LNumber := LA / LB;
        pbIntegerDivide:
        begin

          if (LA = -2147483648.0) and (LB = -1) then
          begin
            Reject(npdRuntime, 'Integer division overflow');
          end;
          LNumber := ALeft.AsInteger div ARight.AsInteger;
        end;
        pbModulo:
        begin

          if not ((LA = -2147483648.0) and (LB = -1)) then
          begin
            LNumber := ALeft.AsInteger mod ARight.AsInteger;
          end;
        end;
        pbEqual, pbNotEqual, pbLess, pbAtMost, pbGreater, pbAtLeast: ;
      end;

      if (AKind <> pbDivide) and (ALeft.ValueType.Kind = npkInteger) and
        (ARight.ValueType.Kind = npkInteger) then
      begin
        Exit(ProgramInteger(LNumber, FSpan));
      end;

      if IsNan(LNumber) or IsInfinite(LNumber) then
      begin
        Reject(npdRuntime, 'Numeric arithmetic must remain finite');
      end;
      Exit(NyxProgramValue(LNumber));
    end;

    if LA < LB then
    begin
      LCompare := -1;
    end
    else if LA > LB then
    begin
      LCompare := 1;
    end;
  end;
  LBoolean := False;
  case AKind of
    pbEqual: LBoolean := LCompare = 0;
    pbNotEqual: LBoolean := LCompare <> 0;
    pbLess: LBoolean := LCompare < 0;
    pbAtMost: LBoolean := LCompare <= 0;
    pbGreater: LBoolean := LCompare > 0;
    pbAtLeast: LBoolean := LCompare >= 0;
    pbAdd, pbSubtract, pbMultiply, pbDivide, pbIntegerDivide, pbModulo:
      Reject(npdType, 'Unsupported program operand families');
  end;
  Result := NyxProgramValue(LBoolean);
end;

function TRunner.Builtin(AKind: TBuiltin;
  const AArguments: INyxProgramArguments): INyxProgramValue;
var
  LValue: INyxProgramValue;
  LOther: INyxProgramValue;
  LText: TNyxText;
  LData: TNyxDataValue;
  LStorage: Integer;
  LBefore: Integer;
  LScalar: Integer;
  LCount: Integer;
  LFirst: Integer;
  LLast: Integer;
  LStart: Integer;
  LTake: Integer;
  LA: Double;
  LB: Double;
begin
  LValue := AArguments.Item(0);
  Result := Default(INyxProgramValue);
  case AKind of
    pfLength:
    begin
      LCount := 0;
      case LValue.ValueType.Kind of
        npkText:
        begin
          LText := LValue.AsText;
          LStorage := 1;
          while LStorage <= Length(LText) do
          begin
            Tick;
            NyxNextScalar(LText, LStorage, LScalar);
            Inc(LCount);
          end;
        end;
        npkBytes: LCount := Length(LValue.AsBytes);
        npkJSON:
        begin
          LData := LValue.AsJSON;

          if not (LData.Kind in [ndArray, ndObject]) then
          begin
            Reject(npdType, 'Length requires a JSON array or object');
          end;
          LCount := LData.Count;
        end;
        npkVoid, npkBoolean, npkInteger, npkNumber, npkHandle:
          Reject(npdType, 'Length requires String, Bytes or JSON container');
      end;
      Result := NyxProgramValue(LCount);
    end;
    pfIntToStr: Result := NyxProgramValue(TNyxText(IntToStr(LValue.AsInteger)));
    pfFloatToStr: Result := NyxProgramValue(NyxStateNumberText(LValue.AsNumber));
    pfCopy, pfTrim:
    begin
      LText := LValue.AsText;
      LStorage := 1;
      LCount := 0;
      LFirst := 0;
      LLast := 0;
      LStart := 1;
      LTake := High(Integer);

      if AKind = pfCopy then
      begin
        LStart := AArguments.Item(1).AsInteger;
        LTake := AArguments.Item(2).AsInteger;

        if (LStart < 1) or (LTake < 0) then
        begin
          Reject(npdRuntime, 'Copy requires a positive scalar start and nonnegative count');
        end;
      end;
      while (LStorage <= Length(LText)) and (LTake > 0) do
      begin
        Tick;
        LBefore := LStorage;
        NyxNextScalar(LText, LStorage, LScalar);
        Inc(LCount);

        if ((AKind = pfTrim) and not NyxScalarWhitespace(LScalar)) or
          ((AKind = pfCopy) and (LCount >= LStart)) then
        begin

          if LFirst = 0 then
          begin
            LFirst := LBefore;
          end;
          LLast := LStorage;

          if AKind = pfCopy then
          begin
            Dec(LTake);
          end;
        end;
      end;
      Result := NyxProgramValue(System.Copy(LText, LFirst, LLast - LFirst));
    end;
    pfAbs, pfMin, pfMax:
    begin
      LA := ProgramNumber(LValue);
      LB := LA;
      LOther := LValue;

      if AKind <> pfAbs then
      begin
        LOther := AArguments.Item(1);
        LB := ProgramNumber(LOther);
      end;

      if AKind = pfAbs then
      begin
        LA := Abs(LA);
      end
      else if ((AKind = pfMin) and (LB < LA)) or
        ((AKind = pfMax) and (LB > LA)) then
      begin
        LA := LB;
      end;

      if (LValue.ValueType.Kind = npkInteger) and (LOther.ValueType.Kind = npkInteger) then
      begin
        Result := ProgramInteger(LA, FSpan);
      end
      else
      begin
        Result := NyxProgramValue(LA);
      end;
    end;
    pfOrd:
    begin

      if LValue.ValueType.Kind = npkBoolean then
      begin
        Result := NyxProgramValue(Ord(LValue.AsBoolean));
      end
      else
      begin
        LText := LValue.AsText;
        LStorage := 1;

        if not NyxNextScalar(LText, LStorage, LScalar) or
          (LStorage <= Length(LText)) then
        begin
          Reject(npdRuntime, 'Ord requires exactly one Unicode scalar');
        end;
        Result := NyxProgramValue(LScalar);
      end;
    end;
    pfChr:
    begin
      LScalar := LValue.AsInteger;

      if (LScalar < 0) or (LScalar > $10FFFF) or ((LScalar >= $D800) and (LScalar <= $DFFF)) then
      begin
        Reject(npdRuntime, 'Chr requires a Unicode scalar');
      end;
      Result := NyxProgramValue(NyxScalarText(LScalar));
    end;
  end;
end;

function TRunner.Finish: INyxProgramResult;
var
  LResult: TProgramResult;
  LPort: TNyxProgramPort;
  LValue: INyxProgramValue;
  LIndex: Integer;
  LBytes: TNyxTextBytes;
  LNames: array of TNyxProgramSymbolRef;
  LValues: TValues;
  LCount: Integer;
begin
  CheckClock;

  if (FOptions.CancellationScope <> nil) and FOptions.CancellationScope.Cancelled then
  begin
    Reject(npdCancelled, 'Behavior program cancelled before result admission');
  end;
  LResult := TProgramResult.Create;
  Result := LResult;
  LCount := 0;
  for LIndex := 0 to FProgram.FContract.PortCount - 1 do
  begin

    if FProgram.FContract.Port(LIndex).Direction = npvOutput then
    begin
      Inc(LCount);
    end;
  end;
  Charge(TNyxTextBytes(LCount) * 64 + TNyxTextBytes(Length(FProposals)) * 16);
  SetLength(LNames, LCount);
  SetLength(LValues, LCount);
  LCount := 0;
  LBytes := FResultBytes;
  for LIndex := 0 to FProgram.FContract.PortCount - 1 do
  begin
    LPort := FProgram.FContract.Port(LIndex);

    if LPort.Direction = npvOutput then
    begin
      Tick;
      LValue := Read(0, LIndex);
      LBytes := LBytes + LValue.ByteSize + Length(LPort.Name.Name);

      if LBytes > FProgram.FLimits.MaximumResultBytes then
      begin
        Reject(npdLimit, 'Program complete result byte budget exceeded');
      end;
      LNames[LCount] := LPort.Name;
      LValues[LCount] := LValue;
      Inc(LCount);
    end;
  end;
  LResult.FOutputs := NyxProgramValues(LNames, LValues);
  SetLength(LResult.FProposals, Length(FProposals));
  for LIndex := 0 to High(FProposals) do
  begin
    Tick;
    LResult.FProposals[LIndex] := FProposals[LIndex];
  end;
  CheckClock;

  if (FOptions.CancellationScope <> nil) and FOptions.CancellationScope.Cancelled then
  begin
    Reject(npdCancelled, 'Behavior program cancelled before complete result publication');
  end;
  LResult.FSteps := FSteps;
  LResult.FAllocation := FAllocation;
end;

function TRunner.Execute: INyxProgramResult;
var
  LInstruction: TInstruction;
  LValue: INyxProgramValue;
  LOther: INyxProgramValue;
  LArgs: INyxProgramArguments;
  LHost: TNyxProgramRoutine;
  LFrame: Integer;
  LIndex: Integer;
  LCount: Integer;
  LNext: Integer;
  LBound: Integer;
  LBytes: TNyxTextBytes;
begin
  while FPC >= 0 do
  begin

    if FPC >= Length(FProgram.FCode) then
    begin
      Reject(npdRuntime, 'Internal program instruction address is out of range');
    end;
    LInstruction := FProgram.FCode[FPC];
    FSpan := LInstruction.Span;
    Tick;
    Inc(FPC);
    try
      case LInstruction.Op of
        poConstant: Push(FProgram.FConstants[LInstruction.A]);
        poRead: Push(Read(LInstruction.A, LInstruction.B));
        poWrite:
        begin
          LValue := Pop;
          Write(LInstruction.A, LInstruction.B, LValue);
        end;
        poDrop: Pop;
        poWiden:
        begin
          LValue := Pop;
          Push(NyxProgramValue(Double(LValue.AsInteger)));
        end;
        poNegate:
        begin
          LValue := Pop;

          if LValue.ValueType.Kind = npkInteger then
          begin
            Push(ProgramInteger(-Double(LValue.AsInteger), FSpan));
          end
          else
          begin
            Push(NyxProgramValue(-LValue.AsNumber));
          end;
        end;
        poNot:
        begin
          LValue := Pop;
          Push(NyxProgramValue(not LValue.AsBoolean));
        end;
        poBinary:
        begin
          LOther := Pop;
          LValue := Pop;
          Push(Binary(TBinary(LInstruction.A), LValue, LOther));
        end;
        poJump: FPC := LInstruction.Target;
        poIfFalse, poIfTrue:
        begin
          LValue := Pop;

          if LValue.AsBoolean = (LInstruction.Op = poIfTrue) then
          begin
            FPC := LInstruction.Target;
          end;
        end;
        poCall:
        begin
          LArgs := Arguments(LInstruction.B);
          Call(LInstruction.A, LArgs);
        end;
        poHost:
        begin
          LArgs := Arguments(LInstruction.B);
          LHost := FProgram.FContract.Routine(LInstruction.A);

          if LHost.Kind = nprQuery then
          begin
            CheckClock;
            try
              LValue := FOptions.QuerySnapshot.Query(LHost.Name, LArgs);
            except
              on E: ENyxProgram do
              begin
                Reject(E.Diagnostic.Kind, E.Diagnostic.Message);
              end;
              on E: Exception do
              begin
                Reject(npdRuntime, 'The declared host query could not complete');
              end;
            end;
            CheckClock;

            if (LValue = nil) or not LValue.ValueType.Same(LHost.ResultType) then
            begin
              Reject(npdType, 'Host query returned a different declared type');
            end;
            Push(LValue);
          end
          else
          begin
            LCount := Length(FProposals);

            if LCount >= FProgram.FLimits.MaximumProposals then
            begin
              Reject(npdLimit, 'Program proposed-call budget exceeded');
            end;
            LBytes := 128 + Length(LHost.Name.Name) + TNyxTextBytes(LHost.ParameterCount) * 64;
            for LIndex := 0 to LArgs.Count - 1 do
            begin
              Tick;
              LBytes := LBytes + LArgs.Item(LIndex).ByteSize;
            end;

            if LBytes > FProgram.FLimits.MaximumResultBytes - FResultBytes then
            begin
              Reject(npdLimit, 'Program complete proposal byte budget exceeded');
            end;
            Charge(LBytes);
            Inc(FResultBytes, Integer(LBytes));
            SetLength(FProposals, LCount + 1);
            FProposals[LCount] := NyxProgramProposal(LHost, LArgs, FSpan);
          end;
        end;
        poBuiltin:
        begin
          LArgs := Arguments(LInstruction.B);
          Push(Builtin(TBuiltin(LInstruction.A), LArgs));
        end;
        poReturn: Return;
        poForLock:
        begin
          LCount := Length(FGuards);
          Charge(16);
          SetLength(FGuards, LCount + 1);
          FGuards[LCount].Owner := High(FFrames);
          FGuards[LCount].Frame := FrameFor(LInstruction.A);
          FGuards[LCount].Slot := LInstruction.B;
        end;
        poForUnlock:
        begin
          LCount := High(FGuards);

          if (LCount < 0) or (FGuards[LCount].Owner <> High(FFrames)) then
          begin
            Reject(npdRuntime, 'Internal program For guard mismatch');
          end;
          SetLength(FGuards, LCount);
        end;
        poForNext:
        begin
          LFrame := FrameFor(LInstruction.A);
          LNext := FFrames[LFrame].Values[LInstruction.B].AsInteger;
          LBound := FFrames[High(FFrames)].Values[LInstruction.C].AsInteger;

          if LNext <> LBound then
          begin
            { The frozen bound and active readonly guard make this step safe at
              both signed extrema; the terminal iteration never increments. }
            Inc(LNext, LInstruction.D);
            LValue := NyxProgramValue(LNext);
            AdmitValue(LValue);
            FFrames[LFrame].Values[LInstruction.B] := LValue;
            FPC := LInstruction.Target;
          end;
        end;
      end;
    except
      on E: ENyxProgram do
      begin
        raise;
      end;
      on E: EMathError do
      begin
        Reject(npdRuntime, 'Program numeric arithmetic overflow');
      end;
    end;
  end;
  Result := Finish;
end;
