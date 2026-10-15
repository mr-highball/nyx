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



{ Compiler/type checking for nyx.programs. All branches and routine bodies are
  admitted before bytecode is published; source text never selects native code. }

function ProgramCapacity(ACount, ALimit: Integer): Integer;
begin
  Result := ALimit;

  if ACount < ALimit div 2 then
  begin
    Result := Max(16, ACount * 2);
    Result := Min(Result, ALimit);
  end;
end;

function ProgramReserved(const AName: TNyxText): Boolean;
const
  CWords: TNyxText = '|program|unit|interface|implementation|initialization|finalization|' +
    'begin|end|var|const|type|procedure|function|forward|if|then|else|while|do|repeat|until|' +
    'for|to|downto|break|continue|exit|not|and|or|div|mod|true|false|nil|uses|array|' +
    'record|class|object|set|of|case|with|try|except|finally|raise|asm|inherited|out|';
begin
  Result := Pos('|' + NyxProgramIdentifierKey(AName) + '|', CWords) > 0;
end;

constructor TCompiler.Create(ATarget: TCompiledProgram);
begin
  inherited Create;
  FTarget := ATarget;
  FLexer := TLexer.Create(ATarget.FSource, ATarget.FLimits);
end;

destructor TCompiler.Destroy;
begin
  FLexer.Free;
  inherited Destroy;
end;

procedure TCompiler.Next;
begin
  FToken := FLexer.Next;
end;

function TCompiler.IsToken(const AText: TNyxText): Boolean;
begin
  Result := (FToken.Kind = ptSymbol) and (FToken.Text = AText);

  if (FToken.Kind = ptIdentifier) and (AText <> '') and
    (((AText[1] >= 'a') and (AText[1] <= 'z')) or
      ((AText[1] >= 'A') and (AText[1] <= 'Z')) or (AText[1] = '_')) then
  begin
    Result := NyxProgramIdentifierKey(FToken.Text) = NyxProgramIdentifierKey(AText);
  end;
end;

function TCompiler.Accept(const AText: TNyxText): Boolean;
begin
  Result := IsToken(AText);

  if Result then
  begin
    Next;
  end;
end;

procedure TCompiler.Expect(const AText: TNyxText);
begin

  if not Accept(AText) then
  begin
    Fail(npdSyntax, 'Expected ' + AText);
  end;
end;

procedure TCompiler.Fail(AKind: TNyxProgramDiagnosticKind; const AMessage: TNyxText);
begin
  raise ENyxProgram.Create(AKind, FToken.Span, AMessage);
end;

procedure TCompiler.Enter;
begin

  if FDepth >= FTarget.FLimits.MaximumNesting then
  begin
    Fail(npdLimit, 'Program parser nesting budget exceeded');
  end;
  Inc(FDepth);
end;

procedure TCompiler.Leave;
begin
  Dec(FDepth);
end;

function TCompiler.Emit(AOp: TOp; const ASpan: TNyxProgramSpan;
  AA: Integer; AB: Integer; AC: Integer; AD: Integer): Integer;
begin

  if FCodeCount >= FTarget.FLimits.MaximumInstructions then
  begin
    Fail(npdLimit, 'Program instruction budget exceeded');
  end;

  if FCodeCount = Length(FTarget.FCode) then
  begin
    SetLength(FTarget.FCode, ProgramCapacity(FCodeCount, FTarget.FLimits.MaximumInstructions));
  end;
  Result := FCodeCount;
  Inc(FCodeCount);
  FTarget.FCode[Result] := Default(TInstruction);
  FTarget.FCode[Result].Op := AOp;
  FTarget.FCode[Result].A := AA;
  FTarget.FCode[Result].B := AB;
  FTarget.FCode[Result].C := AC;
  FTarget.FCode[Result].D := AD;
  FTarget.FCode[Result].Span := ASpan;
end;

function TCompiler.Constant(const AValue: INyxProgramValue;
  const ASpan: TNyxProgramSpan): Integer;
var
  LIndex: Integer;
begin

  if AValue.ByteSize > FTarget.FLimits.MaximumValueBytes then
  begin
    raise ENyxProgram.Create(npdLimit, ASpan, 'Program literal exceeds the value byte budget');
  end;

  if FConstantCount >= FTarget.FLimits.MaximumInstructions then
  begin
    Fail(npdLimit, 'Program constant budget exceeded');
  end;

  if FConstantCount = Length(FTarget.FConstants) then
  begin
    SetLength(FTarget.FConstants, ProgramCapacity(FConstantCount, FTarget.FLimits.MaximumInstructions));
  end;
  LIndex := FConstantCount;
  Inc(FConstantCount);
  FTarget.FConstants[LIndex] := AValue.Copy;
  Result := Emit(poConstant, ASpan, LIndex);
end;

function TCompiler.Variable(const AName: TNyxProgramSymbolRef;
  out AScope, ASlot: Integer): Boolean;
var
  LScope: Integer;
  LIndex: Integer;
begin
  LScope := FCurrent;
  while LScope >= 0 do
  begin

    if (FTarget.FRoutines[LScope].ResultSlot >= 0) and
      FTarget.FRoutines[LScope].Name.Same(AName) then
    begin
      AScope := LScope;
      ASlot := FTarget.FRoutines[LScope].ResultSlot;
      Exit(True);
    end;
    for LIndex := 0 to High(FTarget.FRoutines[LScope].Variables) do
    begin

      if FTarget.FRoutines[LScope].Variables[LIndex].Name.Same(AName) then
      begin
        AScope := LScope;
        ASlot := LIndex;
        Exit(True);
      end;
    end;
    LScope := FTarget.FRoutines[LScope].Parent;
  end;
  AScope := -1;
  ASlot := -1;
  Result := False;
end;

function TCompiler.Routine(const AName: TNyxProgramSymbolRef): Integer;
var
  LScope: Integer;
  LIndex: Integer;
begin
  LScope := FCurrent;
  while LScope >= 0 do
  begin
    for LIndex := 1 to High(FTarget.FRoutines) do
    begin

      if (FTarget.FRoutines[LIndex].Parent = LScope) and
        FTarget.FRoutines[LIndex].Name.Same(AName) then
      begin
        Exit(LIndex);
      end;
    end;
    LScope := FTarget.FRoutines[LScope].Parent;
  end;
  Result := -1;
end;

function TCompiler.Host(const AName: TNyxProgramSymbolRef): Integer;
var
  LIndex: Integer;
begin
  for LIndex := 0 to FTarget.FContract.RoutineCount - 1 do
  begin

    if FTarget.FContract.Routine(LIndex).Name.Same(AName) then
    begin
      Exit(LIndex);
    end;
  end;
  Result := -1;
end;

function TCompiler.Builtin(const AName: TNyxProgramSymbolRef; out AKind: TBuiltin): Boolean;
const
  CNames: array[TBuiltin] of TNyxText =
    ('Length', 'IntToStr', 'FloatToStr', 'Copy', 'Trim', 'Abs', 'Min', 'Max', 'Ord', 'Chr');
var
  LIndex: TBuiltin;
begin
  for LIndex := Low(TBuiltin) to High(TBuiltin) do
  begin

    if AName.Same(NyxProgramSymbol(CNames[LIndex])) then
    begin
      AKind := LIndex;
      Exit(True);
    end;
  end;
  AKind := pfLength;
  Result := False;
end;

function TCompiler.AddVariable(ARoutine: Integer; const AName: TNyxProgramSymbolRef;
  const AType: TNyxProgramType; AReadOnly: Boolean;
  const AInitial: INyxProgramValue): Integer;
var
  LIndex: Integer;
  LKind: TBuiltin;
begin

  if FSymbols >= FTarget.FLimits.MaximumSymbols then
  begin
    Fail(npdLimit, 'Program symbol budget exceeded');
  end;

  if not AType.Defined or (AType.Kind = npkVoid) or ProgramReserved(AName.Name) or
    Builtin(AName, LKind) then
  begin
    Fail(npdName, 'Variables need a nonreserved name and exact non-Void type');
  end;

  if (ARoutine > 0) and AName.Same(FTarget.FRoutines[ARoutine].Name) then
  begin
    Fail(npdName, 'A routine cannot hide its own result identifier with a variable');
  end;
  for LIndex := 0 to High(FTarget.FRoutines[ARoutine].Variables) do
  begin

    if FTarget.FRoutines[ARoutine].Variables[LIndex].Name.Same(AName) then
    begin
      Fail(npdName, 'Duplicate local variable or parameter: ' + AName.Name);
    end;
  end;
  for LIndex := 1 to High(FTarget.FRoutines) do
  begin

    if (FTarget.FRoutines[LIndex].Parent = ARoutine) and
      FTarget.FRoutines[LIndex].Name.Same(AName) then
    begin
      Fail(npdName, 'A local variable conflicts with a routine: ' + AName.Name);
    end;
  end;

  if (ARoutine = 0) and (Host(AName) >= 0) then
  begin
    Fail(npdName, 'A global variable conflicts with its host vocabulary');
  end;
  Result := Length(FTarget.FRoutines[ARoutine].Variables);
  SetLength(FTarget.FRoutines[ARoutine].Variables, Result + 1);
  FTarget.FRoutines[ARoutine].Variables[Result] := TVariable.Create;
  FTarget.FRoutines[ARoutine].Variables[Result].Name := AName;
  FTarget.FRoutines[ARoutine].Variables[Result].ValueType := AType;
  FTarget.FRoutines[ARoutine].Variables[Result].ReadOnly := AReadOnly;
  FTarget.FRoutines[ARoutine].Variables[Result].Initial := AInitial;
  Inc(FSymbols);
end;

function TCompiler.ReadName: TNyxProgramSymbolRef;
begin

  if (FToken.Kind <> ptIdentifier) or ProgramReserved(FToken.Text) then
  begin
    Fail(npdName, 'Expected a nonreserved Pascal identifier');
  end;
  Result := NyxProgramSymbol(FToken.Text);
  Next;
end;

function TCompiler.ReadType: TNyxProgramType;
var
  LName: TNyxProgramSymbolRef;
  LKey: TNyxText;
  LIndex: Integer;
  LPart: Integer;
  LHost: TNyxProgramRoutine;
  LType: TNyxProgramType;
begin
  LName := ReadName;
  LKey := NyxProgramIdentifierKey(LName.Name);

  if LKey = 'boolean' then
  begin
    Exit(NyxProgramType(npkBoolean));
  end;

  if LKey = 'integer' then
  begin
    Exit(NyxProgramType(npkInteger));
  end;

  if (LKey = 'double') or (LKey = 'real') then
  begin
    Exit(NyxProgramType(npkNumber));
  end;

  if (LKey = 'string') or (LKey = 'tnyxtext') then
  begin
    Exit(NyxProgramType(npkText));
  end;

  if LKey = 'json' then
  begin
    Exit(NyxProgramType(npkJSON));
  end;

  if LKey = 'bytes' then
  begin
    Exit(NyxProgramType(npkBytes));
  end;
  for LIndex := 0 to FTarget.FContract.PortCount - 1 do
  begin
    LType := FTarget.FContract.Port(LIndex).ValueType;

    if (LType.Kind = npkHandle) and LName.Same(LType.HandleName) then
    begin
      Exit(LType);
    end;
  end;
  for LIndex := 0 to FTarget.FContract.RoutineCount - 1 do
  begin
    LHost := FTarget.FContract.Routine(LIndex);
    LType := LHost.ResultType;

    if (LType.Kind = npkHandle) and LName.Same(LType.HandleName) then
    begin
      Exit(LType);
    end;
    for LPart := 0 to LHost.ParameterCount - 1 do
    begin
      LType := LHost.Parameter(LPart).ValueType;

      if (LType.Kind = npkHandle) and LName.Same(LType.HandleName) then
      begin
        Exit(LType);
      end;
    end;
  end;
  Fail(npdType, 'Unknown Pascal behavior type: ' + LName.Name);
  Result := Default(TNyxProgramType);
end;

function TCompiler.Literal(ANegative: Boolean): INyxProgramValue;
var
  LNumber: Double;
begin

  if FToken.Kind in [ptInteger, ptNumber] then
  begin
    LNumber := FToken.Number;

    if ANegative then
    begin
      LNumber := -LNumber;
    end;

    if FToken.Kind = ptInteger then
    begin

      if (LNumber < -2147483648.0) or (LNumber > 2147483647.0) then
      begin
        Fail(npdType, 'Integer literal exceeds signed 32-bit range');
      end;
      Result := NyxProgramValue(Integer(Trunc(LNumber)));
    end
    else
    begin
      Result := NyxProgramValue(LNumber);
    end;
  end
  else if not ANegative and (FToken.Kind = ptText) then
  begin
    Result := NyxProgramValue(FToken.Text);
  end
  else if not ANegative and (IsToken('true') or IsToken('false')) then
  begin
    Result := NyxProgramValue(IsToken('true'));
  end
  else
  begin
    Fail(npdSyntax, 'A literal constant requires a typed scalar literal');
    Result := Default(INyxProgramValue);
  end;
  Next;
end;

procedure TCompiler.Assignable(const ASource, ATarget: TNyxProgramType;
  const ASpan: TNyxProgramSpan);
begin

  if ASource.Same(ATarget) then
  begin
    Exit;
  end;

  if (ASource.Kind = npkInteger) and (ATarget.Kind = npkNumber) then
  begin
    Emit(poWiden, ASpan);
    Exit;
  end;
  raise ENyxProgram.Create(npdType, ASpan, 'Cannot assign ' + ASource.DisplayName +
    ' to ' + ATarget.DisplayName);
end;

procedure TCompiler.RequireCapability(const ARoutine: TNyxProgramRoutine;
  const ASpan: TNyxProgramSpan);
var
  LIndex: Integer;
begin

  if ARoutine.Kind = nprQuery then
  begin
    FTarget.FNeedsQueries := True;
  end
  else
  begin
    FTarget.FMayPropose := True;
  end;

  if not ARoutine.Capability.Defined then
  begin
    Exit;
  end;
  for LIndex := 0 to High(FTarget.FCapabilities) do
  begin

    if FTarget.FCapabilities[LIndex].Same(ARoutine.Capability) then
    begin
      Exit;
    end;
  end;
  LIndex := Length(FTarget.FCapabilities);
  SetLength(FTarget.FCapabilities, LIndex + 1);
  SetLength(FTarget.FCapabilitySpans, LIndex + 1);
  FTarget.FCapabilities[LIndex] := ARoutine.Capability;
  FTarget.FCapabilitySpans[LIndex] := ASpan;
end;

function TCompiler.Call(const AName: TNyxProgramSymbolRef;
  const ASpan: TNyxProgramSpan): TNyxProgramType;
var
  LLocal: Integer;
  LHostIndex: Integer;
  LHost: TNyxProgramRoutine;
  LKind: TBuiltin;
  LIsBuiltin: Boolean;
  LExpected: Integer;
  LCount: Integer;
  LType: TNyxProgramType;
  LTypes: array of TNyxProgramType;
  LOpen: Boolean;
begin
  LLocal := Routine(AName);
  LHostIndex := -1;
  LHost := Default(TNyxProgramRoutine);
  LIsBuiltin := False;
  LKind := pfLength;

  if LLocal >= 0 then
  begin
    LExpected := FTarget.FRoutines[LLocal].Parameters;
    Result := FTarget.FRoutines[LLocal].ResultType;
  end
  else
  begin
    LHostIndex := Host(AName);

    if LHostIndex >= 0 then
    begin
      LHost := FTarget.FContract.Routine(LHostIndex);
      LExpected := LHost.ParameterCount;
      Result := LHost.ResultType;
    end
    else
    begin
      LIsBuiltin := Builtin(AName, LKind);

      if not LIsBuiltin then
      begin
        raise ENyxProgram.Create(npdName, ASpan, 'Unknown routine: ' + AName.Name);
      end;
      LExpected := 1;

      if LKind in [pfMin, pfMax] then
      begin
        LExpected := 2;
      end;

      if LKind = pfCopy then
      begin
        LExpected := 3;
      end;
      Result := NyxProgramType(npkVoid);
    end;
  end;
  LCount := 0;
  SetLength(LTypes, LExpected);
  LOpen := Accept('(');

  if LOpen and not IsToken(')') then
  begin
    repeat

      if LCount >= LExpected then
      begin
        Fail(npdType, 'Too many routine arguments');
      end;
      LType := Expression;
      LTypes[LCount] := LType;

      if LLocal >= 0 then
      begin
        Assignable(LType, FTarget.FRoutines[LLocal].Variables[LCount].ValueType, ASpan);
      end
      else if LHostIndex >= 0 then
      begin
        Assignable(LType, LHost.Parameter(LCount).ValueType, ASpan);
      end;
      Inc(LCount);
    until not Accept(',');
  end;

  if LOpen then
  begin
    Expect(')');
  end;

  if LCount <> LExpected then
  begin
    raise ENyxProgram.Create(npdType, ASpan, 'Routine argument count does not match its signature');
  end;

  if LIsBuiltin then
  begin
    case LKind of
      pfLength:
      begin

        if not (LTypes[0].Kind in [npkText, npkBytes, npkJSON]) then
        begin
          Fail(npdType, 'Length requires String, Bytes or JSON container');
        end;
        Result := NyxProgramType(npkInteger);
      end;
      pfIntToStr:
      begin
        Assignable(LTypes[0], NyxProgramType(npkInteger), ASpan);
        Result := NyxProgramType(npkText);
      end;
      pfFloatToStr:
      begin
        Assignable(LTypes[0], NyxProgramType(npkNumber), ASpan);
        Result := NyxProgramType(npkText);
      end;
      pfCopy:
      begin

        if (LTypes[0].Kind <> npkText) or (LTypes[1].Kind <> npkInteger) or
          (LTypes[2].Kind <> npkInteger) then
        begin
          Fail(npdType, 'Copy requires String, Integer start and Integer count');
        end;
        Result := NyxProgramType(npkText);
      end;
      pfTrim:
      begin
        Assignable(LTypes[0], NyxProgramType(npkText), ASpan);
        Result := NyxProgramType(npkText);
      end;
      pfAbs, pfMin, pfMax:
      begin

        if not (LTypes[0].Kind in [npkInteger, npkNumber]) or
          ((LExpected = 2) and not (LTypes[1].Kind in [npkInteger, npkNumber])) then
        begin
          Fail(npdType, 'Numeric builtin requires Integer or Double arguments');
        end;
        Result := LTypes[0];

        if (LExpected = 2) and (LTypes[1].Kind = npkNumber) then
        begin
          Result := LTypes[1];
        end;
      end;
      pfOrd:
      begin

        if not (LTypes[0].Kind in [npkBoolean, npkText]) then
        begin
          Fail(npdType, 'Ord requires Boolean or a one-scalar String');
        end;
        Result := NyxProgramType(npkInteger);
      end;
      pfChr:
      begin
        Assignable(LTypes[0], NyxProgramType(npkInteger), ASpan);
        Result := NyxProgramType(npkText);
      end;
    end;
    Emit(poBuiltin, ASpan, Ord(LKind), LCount);
  end
  else if LLocal >= 0 then
  begin
    Emit(poCall, ASpan, LLocal, LCount);
  end
  else
  begin
    RequireCapability(LHost, ASpan);
    Emit(poHost, ASpan, LHostIndex, LCount);
  end;
end;

function TCompiler.Primary: TNyxProgramType;
var
  LValue: INyxProgramValue;
  LName: TNyxProgramSymbolRef;
  LSpan: TNyxProgramSpan;
  LScope: Integer;
  LSlot: Integer;
begin
  Enter;
  try
    LSpan := FToken.Span;

    if Accept('(') then
    begin
      Result := Expression;
      Expect(')');
      Exit;
    end;

    if (FToken.Kind in [ptInteger, ptNumber, ptText]) or IsToken('true') or IsToken('false') then
    begin
      LValue := Literal;
      Constant(LValue, LSpan);
      Exit(LValue.ValueType);
    end;
    LName := ReadName;

    if not IsToken('(') and Variable(LName, LScope, LSlot) then
    begin
      Emit(poRead, LSpan, LScope, LSlot);
      Result := FTarget.FRoutines[LScope].Variables[LSlot].ValueType;
    end
    else
    begin
      Result := Call(LName, LSpan);
    end;

    if Result.Kind = npkVoid then
    begin
      raise ENyxProgram.Create(npdType, LSpan, 'A procedure cannot supply an expression value');
    end;
  finally
    Leave;
  end;
end;

function TCompiler.Unary: TNyxProgramType;
var
  LSpan: TNyxProgramSpan;
  LValue: INyxProgramValue;
begin
  Enter;
  try
    LSpan := FToken.Span;

    if Accept('-') then
    begin

      if FToken.Kind in [ptInteger, ptNumber] then
      begin
        LValue := Literal(True);
        Constant(LValue, LSpan);
        Exit(LValue.ValueType);
      end;
      Result := Unary;

      if not (Result.Kind in [npkInteger, npkNumber]) then
      begin
        Fail(npdType, 'Unary minus requires Integer or Double');
      end;
      Emit(poNegate, LSpan);
    end
    else if Accept('+') then
    begin
      Result := Unary;

      if not (Result.Kind in [npkInteger, npkNumber]) then
      begin
        Fail(npdType, 'Unary plus requires Integer or Double');
      end;
    end
    else if Accept('not') then
    begin
      Result := Unary;
      Assignable(Result, NyxProgramType(npkBoolean), LSpan);
      Emit(poNot, LSpan);
    end
    else
    begin
      Result := Primary;
    end;
  finally
    Leave;
  end;
end;

function TCompiler.BinaryType(AKind: TBinary;
  const ALeft, ARight: TNyxProgramType): TNyxProgramType;
var
  LNumeric: Boolean;
begin
  LNumeric := (ALeft.Kind in [npkInteger, npkNumber]) and
    (ARight.Kind in [npkInteger, npkNumber]);

  if AKind in [pbEqual, pbNotEqual, pbLess, pbAtMost, pbGreater, pbAtLeast] then
  begin

    if not LNumeric and not (ALeft.Same(ARight) and
      ((ALeft.Kind in [npkText, npkBoolean]) or
       ((ALeft.Kind = npkHandle) and (AKind in [pbEqual, pbNotEqual])))) then
    begin
      Fail(npdType, 'Comparison requires compatible scalar types or equal opaque handle types');
    end;
    Exit(NyxProgramType(npkBoolean));
  end;

  if (AKind = pbAdd) and (ALeft.Kind = npkText) and (ARight.Kind = npkText) then
  begin
    Exit(ALeft);
  end;

  if not LNumeric or ((AKind in [pbIntegerDivide, pbModulo]) and
    ((ALeft.Kind <> npkInteger) or (ARight.Kind <> npkInteger))) then
  begin
    Fail(npdType, 'Arithmetic requires compatible numeric types');
  end;
  Result := NyxProgramType(npkInteger);

  if (AKind = pbDivide) or (ALeft.Kind = npkNumber) or (ARight.Kind = npkNumber) then
  begin
    Result := NyxProgramType(npkNumber);
  end;
end;

function TCompiler.Expression(AMinimum: Integer): TNyxProgramType;
var
  LPrecedence: Integer;
  LKind: TBinary;
  LAnd: Boolean;
  LOr: Boolean;
  LSpan: TNyxProgramSpan;
  LRight: TNyxProgramType;
  LBranch: Integer;
  LEnd: Integer;
begin
  Enter;
  try
    Result := Unary;
    while True do
    begin
      LPrecedence := -1;
      LKind := pbAdd;
      LAnd := IsToken('and');
      LOr := IsToken('or');

      if IsToken('+') or IsToken('-') or LOr then
      begin
        LPrecedence := 1;

        if IsToken('-') then
        begin
          LKind := pbSubtract;
        end;
      end
      else if IsToken('*') or IsToken('/') or IsToken('div') or IsToken('mod') or LAnd then
      begin
        LPrecedence := 2;
        LKind := pbMultiply;

        if IsToken('/') then
        begin
          LKind := pbDivide;
        end
        else if IsToken('div') then
        begin
          LKind := pbIntegerDivide;
        end
        else if IsToken('mod') then
        begin
          LKind := pbModulo;
        end;
      end
      else if IsToken('=') or IsToken('<>') or IsToken('<') or IsToken('<=') or
        IsToken('>') or IsToken('>=') then
      begin
        LPrecedence := 0;
        LKind := pbEqual;

        if IsToken('<>') then
        begin
          LKind := pbNotEqual;
        end
        else if IsToken('<') then
        begin
          LKind := pbLess;
        end
        else if IsToken('<=') then
        begin
          LKind := pbAtMost;
        end
        else if IsToken('>') then
        begin
          LKind := pbGreater;
        end
        else if IsToken('>=') then
        begin
          LKind := pbAtLeast;
        end;
      end;

      if LPrecedence < AMinimum then
      begin
        Break;
      end;
      LSpan := FToken.Span;
      Next;

      if LAnd or LOr then
      begin
        Assignable(Result, NyxProgramType(npkBoolean), LSpan);
        LBranch := Emit(poIfFalse, LSpan);

        if LOr then
        begin
          FTarget.FCode[LBranch].Op := poIfTrue;
        end;
        LRight := Expression(LPrecedence + 1);
        Assignable(LRight, NyxProgramType(npkBoolean), LSpan);
        LEnd := Emit(poJump, LSpan);
        FTarget.FCode[LBranch].Target := FCodeCount;
        Constant(NyxProgramValue(LOr), LSpan);
        FTarget.FCode[LEnd].Target := FCodeCount;
        Result := NyxProgramType(npkBoolean);
      end
      else
      begin
        LRight := Expression(LPrecedence + 1);
        Result := BinaryType(LKind, Result, LRight);
        Emit(poBinary, LSpan, Ord(LKind));
      end;
    end;
  finally
    Leave;
  end;
end;


procedure TCompiler.Writable(AScope, ASlot: Integer);
var
  LIndex: Integer;
begin

  if FTarget.FRoutines[AScope].Variables[ASlot].ReadOnly then
  begin
    Fail(npdType, 'A contract input, constant or const parameter is read-only');
  end;
  for LIndex := 0 to High(FLoops) do
  begin

    if (FLoops[LIndex].Scope = AScope) and (FLoops[LIndex].Slot = ASlot) then
    begin
      Fail(npdType, 'An active For counter is read-only');
    end;
  end;
end;

procedure TCompiler.PushLoop(AScope, ASlot: Integer);
var
  LIndex: Integer;
begin
  LIndex := Length(FLoops);
  SetLength(FLoops, LIndex + 1);
  FLoops[LIndex] := Default(TLoop);
  FLoops[LIndex].Scope := AScope;
  FLoops[LIndex].Slot := ASlot;
end;

procedure TCompiler.EndLoop(ABreak, AContinue: Integer);
var
  LIndex: Integer;
  LLoop: Integer;
begin
  LLoop := High(FLoops);
  for LIndex := 0 to High(FLoops[LLoop].Breaks) do
  begin
    FTarget.FCode[FLoops[LLoop].Breaks[LIndex]].Target := ABreak;
  end;
  for LIndex := 0 to High(FLoops[LLoop].Continues) do
  begin
    FTarget.FCode[FLoops[LLoop].Continues[LIndex]].Target := AContinue;
  end;
  SetLength(FLoops, LLoop);
end;

procedure TCompiler.Statements(const AEnd: TNyxText);
begin
  while not IsToken(AEnd) do
  begin

    if FToken.Kind = ptEOF then
    begin
      Fail(npdSyntax, 'Unterminated Pascal statement block');
    end;
    Statement;

    if not IsToken(AEnd) then
    begin
      Expect(';');
    end
    else
    begin
      Accept(';');
    end;
  end;
end;

procedure TCompiler.Statement;
var
  LName: TNyxProgramSymbolRef;
  LType: TNyxProgramType;
  LSpan: TNyxProgramSpan;
  LScope: Integer;
  LSlot: Integer;
  LStart: Integer;
  LBranch: Integer;
  LEnd: Integer;
  LContinue: Integer;
  LLoop: Integer;
  LIndex: Integer;
  LBound: Integer;
  LDirection: Integer;
begin
  Enter;
  try
    LSpan := FToken.Span;

    if Accept('begin') then
    begin
      Statements('end');
      Expect('end');
      Exit;
    end;

    if IsToken(';') then
    begin
      Exit;
    end;

    if Accept('if') then
    begin
      LType := Expression;
      Assignable(LType, NyxProgramType(npkBoolean), LSpan);
      LBranch := Emit(poIfFalse, LSpan);
      Expect('then');
      Statement;

      if Accept('else') then
      begin
        LEnd := Emit(poJump, LSpan);
        FTarget.FCode[LBranch].Target := FCodeCount;
        Statement;
        FTarget.FCode[LEnd].Target := FCodeCount;
      end
      else
      begin
        FTarget.FCode[LBranch].Target := FCodeCount;
      end;
      Exit;
    end;

    if Accept('while') then
    begin
      LStart := FCodeCount;
      LType := Expression;
      Assignable(LType, NyxProgramType(npkBoolean), LSpan);
      LBranch := Emit(poIfFalse, LSpan);
      Expect('do');
      PushLoop(-1, -1);
      Statement;
      LEnd := Emit(poJump, LSpan);
      FTarget.FCode[LEnd].Target := LStart;
      FTarget.FCode[LBranch].Target := FCodeCount;
      EndLoop(FCodeCount, LStart);
      Exit;
    end;

    if Accept('repeat') then
    begin
      LStart := FCodeCount;
      PushLoop(-1, -1);
      Statements('until');
      Expect('until');
      LContinue := FCodeCount;
      LType := Expression;
      Assignable(LType, NyxProgramType(npkBoolean), LSpan);
      LBranch := Emit(poIfFalse, LSpan);
      FTarget.FCode[LBranch].Target := LStart;
      EndLoop(FCodeCount, LContinue);
      Exit;
    end;

    if Accept('for') then
    begin
      LName := ReadName;

      if not Variable(LName, LScope, LSlot) then
      begin
        Fail(npdName, 'For requires a declared Integer counter');
      end;
      Writable(LScope, LSlot);

      if FTarget.FRoutines[LScope].Variables[LSlot].ValueType.Kind <> npkInteger then
      begin
        Fail(npdType, 'For counters require Integer');
      end;
      Expect(':=');
      LType := Expression;
      Assignable(LType, NyxProgramType(npkInteger), LSpan);
      Emit(poWrite, LSpan, LScope, LSlot);
      LDirection := 1;

      if Accept('downto') then
      begin
        LDirection := -1;
      end
      else
      begin
        Expect('to');
      end;
      LType := Expression;
      Assignable(LType, NyxProgramType(npkInteger), LSpan);

      if FSymbols >= FTarget.FLimits.MaximumSymbols then
      begin
        Fail(npdLimit, 'For end slot exceeds the symbol budget');
      end;
      { Anonymous frozen end slots cannot be named/captured by script source. }
      LBound := Length(FTarget.FRoutines[FCurrent].Variables);
      SetLength(FTarget.FRoutines[FCurrent].Variables, LBound + 1);
      FTarget.FRoutines[FCurrent].Variables[LBound] := TVariable.Create;
      FTarget.FRoutines[FCurrent].Variables[LBound].ValueType := NyxProgramType(npkInteger);
      Inc(FSymbols);
      Emit(poWrite, LSpan, FCurrent, LBound);
      Emit(poRead, LSpan, LScope, LSlot);
      Emit(poRead, LSpan, FCurrent, LBound);
      LIndex := Ord(pbAtMost);

      if LDirection < 0 then
      begin
        LIndex := Ord(pbAtLeast);
      end;
      Emit(poBinary, LSpan, LIndex);
      LBranch := Emit(poIfFalse, LSpan);
      Emit(poForLock, LSpan, LScope, LSlot);
      LStart := FCodeCount;
      Expect('do');
      PushLoop(LScope, LSlot);
      Statement;
      LContinue := FCodeCount;
      LIndex := Emit(poForNext, LSpan, LScope, LSlot, LBound, LDirection);
      FTarget.FCode[LIndex].Target := LStart;
      LEnd := FCodeCount;
      Emit(poForUnlock, LSpan);
      FTarget.FCode[LBranch].Target := FCodeCount;
      EndLoop(LEnd, LContinue);
      Exit;
    end;

    if IsToken('break') or IsToken('continue') then
    begin

      if Length(FLoops) = 0 then
      begin
        Fail(npdSyntax, 'Break/Continue require a containing loop');
      end;
      LLoop := High(FLoops);
      LIndex := Emit(poJump, LSpan);

      if Accept('break') then
      begin
        LEnd := Length(FLoops[LLoop].Breaks);
        SetLength(FLoops[LLoop].Breaks, LEnd + 1);
        FLoops[LLoop].Breaks[LEnd] := LIndex;
      end
      else
      begin
        Expect('continue');
        LEnd := Length(FLoops[LLoop].Continues);
        SetLength(FLoops[LLoop].Continues, LEnd + 1);
        FLoops[LLoop].Continues[LEnd] := LIndex;
      end;
      Exit;
    end;

    if Accept('exit') then
    begin

      if Accept('(') then
      begin

        if not IsToken(')') then
        begin

          if FTarget.FRoutines[FCurrent].ResultSlot < 0 then
          begin
            Fail(npdType, 'Only a function can Exit with a value');
          end;
          LType := Expression;
          Assignable(LType, FTarget.FRoutines[FCurrent].ResultType, LSpan);
          Emit(poWrite, LSpan, FCurrent, FTarget.FRoutines[FCurrent].ResultSlot);
        end;
        Expect(')');
      end;
      Emit(poReturn, LSpan);
      Exit;
    end;
    LName := ReadName;

    if Accept(':=') then
    begin

      if not Variable(LName, LScope, LSlot) then
      begin
        raise ENyxProgram.Create(npdName, LSpan, 'Unknown assignment target: ' + LName.Name);
      end;
      Writable(LScope, LSlot);
      LType := Expression;
      Assignable(LType, FTarget.FRoutines[LScope].Variables[LSlot].ValueType, LSpan);
      Emit(poWrite, LSpan, LScope, LSlot);
    end
    else
    begin
      LType := Call(LName, LSpan);

      if LType.Kind <> npkVoid then
      begin
        Emit(poDrop, LSpan);
      end;
    end;
  finally
    Leave;
  end;
end;

procedure TCompiler.DeclareRoutine;
var
  LParent: Integer;
  LNew: Integer;
  LExisting: Integer;
  LIndex: Integer;
  LPart: Integer;
  LFunction: Boolean;
  LReadOnly: Boolean;
  LName: TNyxProgramSymbolRef;
  LNames: array of TNyxProgramSymbolRef;
  LType: TNyxProgramType;
  LSpan: TNyxProgramSpan;
  LKind: TBuiltin;
  LHeader: TRoutine;
begin
  LParent := FCurrent;
  LSpan := FToken.Span;
  LFunction := Accept('function');

  if not LFunction then
  begin
    Expect('procedure');
  end;
  LName := ReadName;

  if Builtin(LName, LKind) or ((LParent = 0) and (Host(LName) >= 0)) then
  begin
    Fail(npdName, 'Routine name conflicts with the declared host or builtin vocabulary');
  end;
  for LIndex := 0 to High(FTarget.FRoutines[LParent].Variables) do
  begin

    if FTarget.FRoutines[LParent].Variables[LIndex].Name.Same(LName) then
    begin
      Fail(npdName, 'Routine name conflicts with a local variable');
    end;
  end;
  LExisting := -1;
  for LIndex := 1 to High(FTarget.FRoutines) do
  begin

    if (FTarget.FRoutines[LIndex].Parent = LParent) and
      FTarget.FRoutines[LIndex].Name.Same(LName) then
    begin
      LExisting := LIndex;

      if not FTarget.FRoutines[LIndex].Forwarded then
      begin
        Fail(npdName, 'Duplicate routine declaration');
      end;
      Break;
    end;
  end;

  if (LExisting < 0) and (Length(FTarget.FRoutines) >= FTarget.FLimits.MaximumRoutines) then
  begin
    Fail(npdLimit, 'Program routine budget exceeded');
  end;

  if LExisting >= 0 then
  begin
    { A matching forward definition replaces its header, not its logical symbol
      budget. Temporary signature comparison must not double-charge parameters. }
    Dec(FSymbols, Length(FTarget.FRoutines[LExisting].Variables));
  end;
  LNew := Length(FTarget.FRoutines);
  SetLength(FTarget.FRoutines, LNew + 1);
  FTarget.FRoutines[LNew] := Default(TRoutine);
  FTarget.FRoutines[LNew].Name := LName;
  FTarget.FRoutines[LNew].Parent := LParent;
  FTarget.FRoutines[LNew].Entry := -1;
  FTarget.FRoutines[LNew].ResultSlot := -1;
  FTarget.FRoutines[LNew].ResultType := NyxProgramType(npkVoid);
  FTarget.FRoutines[LNew].Declaration := LSpan;
  FCurrent := LNew;

  if Accept('(') then
  begin
    while not IsToken(')') do
    begin
      LReadOnly := Accept('const');

      if IsToken('var') or IsToken('out') then
      begin
        Fail(npdSyntax, 'Behavior parameters pass owned values; var/out aliasing is not supported');
      end;
      LNames := nil;
      repeat
        LName := ReadName;
        SetLength(LNames, Length(LNames) + 1);
        LNames[High(LNames)] := LName;
      until not Accept(',');
      Expect(':');
      LType := ReadType;
      for LIndex := 0 to High(LNames) do
      begin

        if FTarget.FRoutines[LNew].Parameters >= FTarget.FLimits.MaximumParameters then
        begin
          Fail(npdLimit, 'Routine parameter budget exceeded');
        end;
        AddVariable(LNew, LNames[LIndex], LType, LReadOnly, Default(INyxProgramValue));
        Inc(FTarget.FRoutines[LNew].Parameters);
      end;

      if not IsToken(')') then
      begin
        Expect(';');
      end;
    end;
    Expect(')');
  end;

  if LFunction then
  begin
    Expect(':');
    LType := ReadType;
    FTarget.FRoutines[LNew].ResultType := LType;
    FTarget.FRoutines[LNew].ResultSlot := AddVariable(LNew,
      NyxProgramSymbol('Result'), LType, False, Default(INyxProgramValue));
  end;
  Expect(';');

  if LExisting >= 0 then
  begin
    LHeader := FTarget.FRoutines[LNew];

    if (LHeader.Parameters <> FTarget.FRoutines[LExisting].Parameters) or
      not LHeader.ResultType.Same(FTarget.FRoutines[LExisting].ResultType) then
    begin
      Fail(npdType, 'A forward routine definition must retain its exact signature');
    end;
    for LPart := 0 to LHeader.Parameters - 1 do
    begin

      if not LHeader.Variables[LPart].Name.Same(FTarget.FRoutines[LExisting].Variables[LPart].Name) or
        not LHeader.Variables[LPart].ValueType.Same(FTarget.FRoutines[LExisting].Variables[LPart].ValueType) or
        (LHeader.Variables[LPart].ReadOnly <> FTarget.FRoutines[LExisting].Variables[LPart].ReadOnly) then
      begin
        Fail(npdType, 'A forward routine definition must retain its parameters');
      end;
    end;
    for LPart := 0 to High(LHeader.Variables) do
    begin
      LHeader.Variables[LPart].Free;
    end;
    SetLength(FTarget.FRoutines, LNew);
    FCurrent := LExisting;
    LNew := LExisting;
  end;

  if Accept('forward') then
  begin

    if LExisting >= 0 then
    begin
      Fail(npdName, 'A routine can be forwarded only once');
    end;
    FTarget.FRoutines[LNew].Forwarded := True;
    Expect(';');
  end
  else
  begin
    FTarget.FRoutines[LNew].Forwarded := False;
    Block(LNew);
    Expect(';');
  end;
  FCurrent := LParent;
end;

procedure TCompiler.Declarations;
var
  LNames: array of TNyxProgramSymbolRef;
  LName: TNyxProgramSymbolRef;
  LType: TNyxProgramType;
  LValue: INyxProgramValue;
  LDeclared: Boolean;
  LNegative: Boolean;
  LIndex: Integer;
begin
  while IsToken('var') or IsToken('const') or IsToken('function') or IsToken('procedure') do
  begin

    if IsToken('function') or IsToken('procedure') then
    begin
      DeclareRoutine;
      Continue;
    end;

    if Accept('var') then
    begin
      while (FToken.Kind = ptIdentifier) and not ProgramReserved(FToken.Text) do
      begin
        LNames := nil;
        repeat
          LName := ReadName;
          SetLength(LNames, Length(LNames) + 1);
          LNames[High(LNames)] := LName;
        until not Accept(',');
        Expect(':');
        LType := ReadType;
        for LIndex := 0 to High(LNames) do
        begin
          AddVariable(FCurrent, LNames[LIndex], LType, False, Default(INyxProgramValue));
        end;
        Expect(';');
      end;
    end
    else
    begin
      Expect('const');
      while (FToken.Kind = ptIdentifier) and not ProgramReserved(FToken.Text) do
      begin
        LName := ReadName;
        LDeclared := Accept(':');
        LType := Default(TNyxProgramType);

        if LDeclared then
        begin
          LType := ReadType;
        end;
        Expect('=');
        LNegative := Accept('-');

        if not LNegative then
        begin
          Accept('+');
        end;
        LValue := Literal(LNegative);

        if LDeclared then
        begin

          if (LValue.ValueType.Kind = npkInteger) and (LType.Kind = npkNumber) then
          begin
            LValue := NyxProgramValue(Double(LValue.AsInteger));
          end;

          if not LValue.ValueType.Same(LType) then
          begin
            Fail(npdType, 'Literal constant does not match its declared type');
          end;
        end;

        if LValue.ByteSize > FTarget.FLimits.MaximumValueBytes then
        begin
          Fail(npdLimit, 'Constant exceeds its value byte budget');
        end;
        AddVariable(FCurrent, LName, LValue.ValueType, True, LValue);
        Expect(';');
      end;
    end;
  end;
end;

procedure TCompiler.Block(ARoutine: Integer);
var
  LSpan: TNyxProgramSpan;
begin
  Enter;
  try
    FCurrent := ARoutine;
    Declarations;
    FCurrent := ARoutine;
    FTarget.FRoutines[ARoutine].Entry := FCodeCount;
    LSpan := FToken.Span;
    Expect('begin');
    Statements('end');
    Expect('end');
    Emit(poReturn, LSpan);
  finally
    Leave;
  end;
end;

procedure TCompiler.Compile;
var
  LIndex: Integer;
  LPort: TNyxProgramPort;
  LHost: TNyxProgramRoutine;
  LKind: TBuiltin;
begin
  Next;
  SetLength(FTarget.FRoutines, 1);
  FTarget.FRoutines[0] := Default(TRoutine);
  FTarget.FRoutines[0].Name := NyxProgramSymbol('Main');
  FTarget.FRoutines[0].Parent := -1;
  FTarget.FRoutines[0].Entry := -1;
  FTarget.FRoutines[0].ResultSlot := -1;
  FTarget.FRoutines[0].ResultType := NyxProgramType(npkVoid);
  FTarget.FRoutines[0].Declaration := FToken.Span;
  FCurrent := 0;

  if FTarget.FContract.RoutineCount > FTarget.FLimits.MaximumRoutines then
  begin
    Fail(npdLimit, 'Host routine vocabulary exceeds its budget');
  end;
  for LIndex := 0 to FTarget.FContract.RoutineCount - 1 do
  begin
    LHost := FTarget.FContract.Routine(LIndex);

    if ProgramReserved(LHost.Name.Name) or Builtin(LHost.Name, LKind) then
    begin
      Fail(npdName, 'A host routine conflicts with a reserved name');
    end;

    if LHost.ParameterCount > FTarget.FLimits.MaximumParameters then
    begin
      Fail(npdLimit, 'Host parameter vocabulary exceeds its budget');
    end;
  end;
  for LIndex := 0 to FTarget.FContract.PortCount - 1 do
  begin
    LPort := FTarget.FContract.Port(LIndex);
    AddVariable(0, LPort.Name, LPort.ValueType, LPort.Direction = npvInput,
      Default(INyxProgramValue));
  end;

  if Accept('program') then
  begin
    ReadName;
    Expect(';');
  end;
  Block(0);
  Expect('.');

  if FToken.Kind <> ptEOF then
  begin
    Fail(npdSyntax, 'Unexpected content after the program terminator');
  end;
  for LIndex := 1 to High(FTarget.FRoutines) do
  begin

    if FTarget.FRoutines[LIndex].Forwarded or (FTarget.FRoutines[LIndex].Entry < 0) then
    begin
      raise ENyxProgram.Create(npdName, FTarget.FRoutines[LIndex].Declaration,
        'A forward routine has no definition: ' + FTarget.FRoutines[LIndex].Name.Name);
    end;
  end;
  SetLength(FTarget.FCode, FCodeCount);
  SetLength(FTarget.FConstants, FConstantCount);
end;
