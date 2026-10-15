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
{ Bounded declaration inspection shares nyx_logic and its existing authority.
  This path never invokes a program, supplies grants or mutates a proposal. }
function TNyxAgentSession.ProgramQuery(const AArguments: TNyxDataValue): TNyxDataValue;
var
  LScope: TNyxText;
  LOffset: Integer;
  LLimit: Integer;
  LTextOffset: Integer;
  LTextLimit: Integer;
  LTextTotal: Integer;
  LTotal: Integer;
  LCount: Integer;
  LIndex: Integer;
  LFound: Integer;
  LReference: TNyxProgramRef;
  LDefinition: INyxProgramDefinition;
  LProgram: INyxProgram;
  LContract: TNyxProgramContract;
  LLimits: TNyxProgramLimits;
  LRoutine: TNyxProgramRoutine;
  LPort: TNyxProgramPort;
  LParameter: TNyxProgramParameter;
  LReceipt: INyxProgramCompilation;
  LData: TNyxDataValue;
  LItems: array of TNyxDataValue;
  LText: TNyxText;

  function Diagnostic(const AValue: TNyxProgramDiagnostic): TNyxDataValue;
  begin
    Result := NyxObject([NyxField('kind', NyxData(Ord(AValue.Kind))),
      NyxField('message', NyxData(CaptionText(AValue.Message, 512))),
      NyxField('byteOffset', NyxData(AValue.Span.Start)),
      NyxField('byteLength', NyxData(AValue.Span.Count)),
      NyxField('line', NyxData(AValue.Span.Line)),
      NyxField('column', NyxData(AValue.Span.Column))]);
  end;

begin
  NyxAgentFields(AArguments, '|scope|program|routine|definition|offset|limit|expectedRevision|textOffset|textLimit|');

  if NyxAgentHas(AArguments, 'expectedRevision') then
  begin
    RequireRevision(AArguments);
  end;
  LScope := TextArgument(AArguments, 'scope');
  LOffset := IntegerArgument(AArguments, 'offset', 0, 0, 100000);
  LLimit := IntegerArgument(AArguments, 'limit', 8, 1, 32);
  LTextOffset := IntegerArgument(AArguments, 'textOffset', 0, 0, 10000000);
  LTextLimit := IntegerArgument(AArguments, 'textLimit', 512, 1, 1024);
  SetLength(LItems, LLimit);
  LCount := 0;
  LTotal := 0;

  if LScope = 'program-check' then
  begin
    { Limits admit metadata before compilation. Errors return detached spans
      against precisely these proposed bytes, never against accepted Pascal. }
    LData := AArguments.Field('definition');
    ValidateNyxFlowFields(LData, ['version', 'source', 'contract', 'limits']);
    try

      if LData.Field('version').AsInteger <> NyxProgramLanguageVersion then
      begin
        raise ENyxModel.Create('Unsupported interpreted program language version');
      end;
      LLimits := TNyxProgramLimits.FromData(LData.Field('limits'));
      LContract := TNyxProgramContract.FromData(LData.Field('contract'), LLimits);
      LReceipt := InspectNyxProgram(LData.Field('source').AsText, LContract, LLimits);
      LTotal := LReceipt.DiagnosticCount;
      for LIndex := LOffset to LTotal - 1 do
      begin

        if LCount >= LLimit then
        begin
          Break;
        end;
        LItems[LCount] := Diagnostic(LReceipt.Diagnostic(LIndex));
        Inc(LCount);
      end;
    except
      on E: ENyxProgram do
      begin
        LTotal := 1;

        if LOffset = 0 then
        begin
          LItems[0] := Diagnostic(E.Diagnostic);
          LCount := 1;
        end;
      end;
    end;
    SetLength(LItems, LCount);
    Exit(NyxObject([NyxField('revision', NyxData(FRevision)),
      NyxField('scope', NyxData(LScope)), NyxField('succeeded', NyxData(LTotal = 0)),
      NyxField('total', NyxData(LTotal)), NyxField('offset', NyxData(LOffset)),
      NyxField('items', NyxArray(LItems))]));
  end;

  if LScope = 'programs' then
  begin
    LTotal := FSession.Document.Programs.Count;
    for LIndex := LOffset to LTotal - 1 do
    begin

      if LCount >= LLimit then
      begin
        Break;
      end;
      LReference := FSession.Document.Programs.Reference(LIndex);
      LDefinition := FSession.Document.Programs.Definition(LReference);
      LItems[LCount] := NyxObject([NyxField('program', NyxData(LReference.Name)),
        NyxField('ports', NyxData(LDefinition.Contract.PortCount)),
        NyxField('routines', NyxData(LDefinition.Contract.RoutineCount)),
        NyxField('capabilities', NyxData(LDefinition.Compile.CapabilityCount))]);
      Inc(LCount);
    end;
  end
  else
  begin
    LReference := NyxProgramRef(TextArgument(AArguments, 'program'));
    LDefinition := FSession.Document.Programs.Definition(LReference);
    LContract := LDefinition.Contract;
    LProgram := LDefinition.Compile;

    if LScope = 'program-source' then
    begin
      LText := TextSpan(LDefinition.Source, LTextOffset, LTextLimit, LTextTotal);
      Exit(NyxObject([NyxField('revision', NyxData(FRevision)),
        NyxField('scope', NyxData(LScope)), NyxField('program', NyxData(LReference.Name)),
        NyxField('textOffset', NyxData(LTextOffset)), NyxField('text', NyxData(LText)),
        NyxField('totalScalars', NyxData(LTextTotal)),
        NyxField('truncated', NyxData((LTextOffset > 0) or (LTextTotal > LTextOffset + LTextLimit)))]));
    end;

    if LScope = 'program-ports' then
    begin
      LTotal := LContract.PortCount;
    end
    else if LScope = 'program-routines' then
    begin
      LTotal := LContract.RoutineCount;
    end
    else if LScope = 'program-limits' then
    begin
      LTotal := 1;
    end
    else if LScope = 'program-capabilities' then
    begin
      LTotal := LProgram.CapabilityCount;
    end
    else if LScope = 'program-routine-arguments' then
    begin
      LFound := -1;
      for LIndex := 0 to LContract.RoutineCount - 1 do
      begin

        if LContract.Routine(LIndex).Name.Same(NyxProgramSymbol(TextArgument(AArguments, 'routine'))) then
        begin
          LFound := LIndex;
          Break;
        end;
      end;

      if LFound < 0 then
      begin
        raise ENyxModel.Create('Choose an exact declared host routine');
      end;
      LRoutine := LContract.Routine(LFound);
      LTotal := LRoutine.ParameterCount;
    end
    else
    begin
      raise ENyxModel.Create('Choose a declared program inspection scope');
    end;
    for LIndex := LOffset to LTotal - 1 do
    begin

      if LCount >= LLimit then
      begin
        Break;
      end;

      if LScope = 'program-ports' then
      begin
        LPort := LContract.Port(LIndex);
        LText := 'input';

        if LPort.Direction = npvOutput then
        begin
          LText := 'output';
        end;
        LData := NyxObject([NyxField('name', NyxData(LPort.Name.Name)),
          NyxField('direction', NyxData(LText)), NyxField('type', LPort.ValueType.ToData)]);
      end
      else if LScope = 'program-routines' then
      begin
        LRoutine := LContract.Routine(LIndex);
        LText := 'query';

        if LRoutine.Kind = nprProposal then
        begin
          LText := 'proposal';
        end;
        LData := NyxObject([NyxField('name', NyxData(LRoutine.Name.Name)),
          NyxField('kind', NyxData(LText)), NyxField('result', LRoutine.ResultType.ToData),
          NyxField('capability', NyxData(LRoutine.Capability.Name)),
          NyxField('arguments', NyxData(LRoutine.ParameterCount))]);
      end
      else if LScope = 'program-routine-arguments' then
      begin
        LParameter := LRoutine.Parameter(LIndex);
        LData := NyxObject([NyxField('name', NyxData(LParameter.Name.Name)),
          NyxField('type', LParameter.ValueType.ToData)]);
      end
      else if LScope = 'program-limits' then
      begin
        LData := LDefinition.Limits.ToData;
      end
      else
      begin
        LData := NyxObject([NyxField('capability', NyxData(LProgram.Capability(LIndex).Name))]);
      end;
      LItems[LCount] := LData;
      Inc(LCount);
    end;
  end;
  SetLength(LItems, LCount);
  Result := NyxObject([NyxField('revision', NyxData(FRevision)),
    NyxField('scope', NyxData(LScope)), NyxField('total', NyxData(LTotal)),
    NyxField('offset', NyxData(LOffset)), NyxField('items', NyxArray(LItems))]);
end;
