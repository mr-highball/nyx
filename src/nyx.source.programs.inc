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

{ Typed behavior declaration parsing. Only source/contract/limits metadata enters
  the candidate document; parsing never supplies runtime grants or runs a program. }

function TConfigurationReader.ProgramConstructor(const AName: TNyxText): TValue;
var
  LArgs: TValues;
  LMethod: TNyxText;
  LCapability: TNyxProgramCapabilityRef;
  LDefinition: INyxProgramDefinition;
  LLimit: Integer;

  procedure Count(AMinimum, AMaximum: Integer);
  begin

    if (Length(LArgs) < AMinimum) or (Length(LArgs) > AMaximum) then
    begin
      Fail('Wrong typed program argument count');
    end;
  end;

  procedure Kind(AIndex: Integer; AKind: TValueKind);
  begin

    if (AIndex >= Length(LArgs)) or (LArgs[AIndex].Kind <> AKind) then
    begin
      Fail('Wrong program argument family');
    end;
  end;

begin
  Result := Default(TValue);
  LArgs := nil;

  if At('(') then
  begin
    LArgs := Arguments(3);
  end;

  if AName = 'nyxprogramcontract' then
  begin
    Count(0, 0);
    Result.Kind := vkProgramContract;
    Result.ProgramContract := NyxProgramContract;
  end
  else if AName = 'nyxcanvasprogramcontract' then
  begin
    Count(0, 0);
    Result.Kind := vkProgramContract;
    Result.ProgramContract := NyxCanvasProgramContract;
  end
  else if AName = 'nyxprogramlimits' then
  begin
    Count(0, 0);
    Result.Kind := vkProgramLimits;
    Result.ProgramLimits := NyxProgramLimits;
  end
  else if (AName = 'nyxprogramref') or (AName = 'nyxprogramsymbol') then
  begin
    Count(1, 1);
    Kind(0, vkText);

    if AName = 'nyxprogramref' then
    begin
      Result.Kind := vkProgramRef;
      Result.Text := NyxProgramRef(LArgs[0].Text).Name;
    end
    else
    begin
      Result.Kind := vkProgramSymbol;
      Result.Text := NyxProgramSymbol(LArgs[0].Text).Name;
    end;
  end
  else if AName = 'nyxprogramtype' then
  begin
    Count(1, 1);
    Kind(0, vkProgramKind);
    Result.Kind := vkProgramType;
    Result.ProgramType := NyxProgramType(TNyxProgramKind(LArgs[0].Ordinal));
  end
  else if AName = 'nyxprogramhandletype' then
  begin
    Count(1, 1);
    Kind(0, vkProgramSymbol);
    Result.Kind := vkProgramType;
    Result.ProgramType := NyxProgramHandleType(NyxProgramSymbol(LArgs[0].Text));
  end
  else if AName = 'nyxprogramcapability' then
  begin
    Count(1, 1);
    Result.Kind := vkProgramCapabilityRef;

    if LArgs[0].Kind = vkProgramCapability then
    begin
      Result.Text := NyxProgramCapability(TNyxProgramCapability(LArgs[0].Ordinal)).Name;
    end
    else
    begin
      Kind(0, vkText);
      Result.Text := NyxProgramCapability(LArgs[0].Text).Name;
    end;
  end
  else if AName = 'nyxprogramquery' then
  begin
    Count(2, 3);
    Kind(0, vkProgramSymbol);
    Kind(1, vkProgramType);
    LCapability := Default(TNyxProgramCapabilityRef);

    if Length(LArgs) = 3 then
    begin
      Kind(2, vkProgramCapabilityRef);
      LCapability := NyxProgramCapability(LArgs[2].Text);
    end;
    Result.Kind := vkProgramRoutine;
    Result.ProgramRoutine := NyxProgramQuery(NyxProgramSymbol(LArgs[0].Text),
      LArgs[1].ProgramType, LCapability);
  end
  else if AName = 'nyxprogrampropose' then
  begin
    Count(2, 2);
    Kind(0, vkProgramSymbol);
    Kind(1, vkProgramCapabilityRef);
    Result.Kind := vkProgramRoutine;
    Result.ProgramRoutine := NyxProgramPropose(NyxProgramSymbol(LArgs[0].Text),
      NyxProgramCapability(LArgs[1].Text));
  end
  else if AName = 'newnyxprogramdefinition' then
  begin
    Count(2, 3);
    Kind(0, vkText);
    Kind(1, vkProgramContract);

    if Length(LArgs) = 3 then
    begin
      Kind(2, vkProgramLimits);
      LDefinition := NewNyxProgramDefinition(LArgs[0].Text, LArgs[1].ProgramContract,
        LArgs[2].ProgramLimits);
    end
    else
    begin
      LDefinition := NewNyxProgramDefinition(LArgs[0].Text, LArgs[1].ProgramContract);
    end;
    Result.Kind := vkProgramDefinition;
    Result.Data := LDefinition.ToData;
  end
  else if AName = 'nyxprogramdefinitionfromdata' then
  begin
    Count(1, 1);
    Kind(0, vkData);
    Result.Kind := vkProgramDefinition;
    Result.Data := NyxProgramDefinitionFromData(LArgs[0].Data).ToData;
  end
  else
  begin
    Fail('Unknown typed program constructor');
  end;

  while At('.') do
  begin
    Inc(FCursor);
    LMethod := LowerCase(FTokens[FCursor].Text);
    Inc(FCursor);
    LArgs := Arguments(2);
    case Result.Kind of
      vkProgramContract:
      begin

        if LMethod = 'withroutine' then
        begin
          Count(1, 1);
          Kind(0, vkProgramRoutine);
          Result.ProgramContract := Result.ProgramContract.WithRoutine(LArgs[0].ProgramRoutine);
        end
        else
        begin
          Count(2, 2);
          Kind(0, vkProgramSymbol);
          Kind(1, vkProgramType);

          if LMethod = 'input' then
          begin
            Result.ProgramContract := Result.ProgramContract.Input(
              NyxProgramSymbol(LArgs[0].Text), LArgs[1].ProgramType);
          end
          else if LMethod = 'output' then
          begin
            Result.ProgramContract := Result.ProgramContract.Output(
              NyxProgramSymbol(LArgs[0].Text), LArgs[1].ProgramType);
          end
          else
          begin
            Fail('Unknown program contract method');
          end;
        end;
      end;
      vkProgramRoutine:
      begin
        Count(2, 2);
        Kind(0, vkProgramSymbol);
        Kind(1, vkProgramType);

        if LMethod <> 'argument' then
        begin
          Fail('A program routine supports typed Argument only');
        end;
        Result.ProgramRoutine := Result.ProgramRoutine.Argument(
          NyxProgramSymbol(LArgs[0].Text), LArgs[1].ProgramType);
      end;
      vkProgramLimits:
      begin
        Count(1, 1);
        Kind(0, vkInteger);
        LLimit := StrToInt(LArgs[0].Text);

        if LMethod = 'sourcebytes' then
        begin
          Result.ProgramLimits := Result.ProgramLimits.SourceBytes(LLimit);
        end
        else if LMethod = 'symbols' then
        begin
          Result.ProgramLimits := Result.ProgramLimits.Symbols(LLimit);
        end
        else if LMethod = 'instructions' then
        begin
          Result.ProgramLimits := Result.ProgramLimits.Instructions(LLimit);
        end
        else if LMethod = 'routines' then
        begin
          Result.ProgramLimits := Result.ProgramLimits.Routines(LLimit);
        end
        else if LMethod = 'parameters' then
        begin
          Result.ProgramLimits := Result.ProgramLimits.Parameters(LLimit);
        end
        else if LMethod = 'nesting' then
        begin
          Result.ProgramLimits := Result.ProgramLimits.Nesting(LLimit);
        end
        else if LMethod = 'steps' then
        begin
          Result.ProgramLimits := Result.ProgramLimits.Steps(LLimit);
        end
        else if LMethod = 'calldepth' then
        begin
          Result.ProgramLimits := Result.ProgramLimits.CallDepth(LLimit);
        end
        else if LMethod = 'stackvalues' then
        begin
          Result.ProgramLimits := Result.ProgramLimits.StackValues(LLimit);
        end
        else if LMethod = 'valuebytes' then
        begin
          Result.ProgramLimits := Result.ProgramLimits.ValueBytes(LLimit);
        end
        else if LMethod = 'allocationbytes' then
        begin
          Result.ProgramLimits := Result.ProgramLimits.AllocationBytes(LLimit);
        end
        else if LMethod = 'resultbytes' then
        begin
          Result.ProgramLimits := Result.ProgramLimits.ResultBytes(LLimit);
        end
        else if LMethod = 'proposals' then
        begin
          Result.ProgramLimits := Result.ProgramLimits.Proposals(LLimit);
        end
        else if LMethod = 'milliseconds' then
        begin
          Result.ProgramLimits := Result.ProgramLimits.Milliseconds(LLimit);
        end
        else
        begin
          Fail('Unknown typed program limit');
        end;
      end;
      else Fail('This program value has no fluent configuration methods');
    end;
  end;
end;

procedure TConfigurationReader.ProgramDefaults;
var
  LArgs: TValues;
  LReference: TNyxProgramRef;
begin
  Inc(FCursor, 3);
  repeat
    Expect('.');
    Expect('Define');
    LArgs := Arguments(2);

    if (Length(LArgs) <> 2) or (LArgs[0].Kind <> vkProgramRef) or
      (LArgs[1].Kind <> vkProgramDefinition) then
    begin
      Fail('Programs.Define requires a project program reference and admitted typed definition');
    end;
    LReference := NyxProgramRef(LArgs[0].Text);

    if FApply then
    begin

      if FDocument.Programs.Contains(LReference) then
      begin
        Fail('Define each saved program once in the managed builder');
      end;
      FDocument.Programs.Define(LReference, NyxProgramDefinitionFromData(LArgs[1].Data));
    end;
  until not At('.');
  Expect(';');
end;
