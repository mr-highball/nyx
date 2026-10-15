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


{ Crafted typed event routes. JSON appears only for explicit structured constant
  data; closed modes, policies, families and mappings retain public Pascal types. }
function PascalFlowRoutes(const ARoutes: INyxFlowRoutes): TNyxText;
const
  CCallbacks: array[TNyxFlowCallbackMode] of TNyxText =
    ('nfcmExclusive', 'nfcmBeforeCallbacks', 'nfcmAfterDispatch');
  CConcurrency: array[TNyxFlowConcurrency] of TNyxText =
    ('nfrLatest', 'nfrEvery', 'nfrIgnoreWhileRunning');
  CPolicies: array[TNyxExecutionPolicy] of TNyxText =
    ('neSequential', 'neAsynchronous', 'neUIQueue', 'neThreaded');
  CTypes: array[TNyxFlowType] of TNyxText =
    ('nftText', 'nftBoolean', 'nftInteger', 'nftNumber', 'nftJSON', 'nftBytes', 'nftSignal');
  CStates: array[TNyxStateKind] of TNyxText =
    ('NyxTextState', 'NyxBooleanState', 'NyxIntegerState', 'NyxNumberState');
var
  LIndex: Integer;
  LMap: Integer;
  LPart: Integer;
  LRoute: TNyxFlowRoute;
  LInput: TNyxFlowRouteInput;
  LOutput: TNyxFlowRouteOutput;
  LRowsOutput: TNyxFlowRouteRowsOutput;
  LValue: TNyxFlowValue;
  LArgument: TNyxText;
  LPath: TNyxDataValue;

  function PascalBoolean(AValue: Boolean): TNyxText;
  begin
    Result := 'False';

    if AValue then
    begin
      Result := 'True';
    end;
  end;

  function Endpoint(const AValue: TNyxFlowEndpoint): TNyxText;
  const
    CPorts: array[TNyxLogicPort] of TNyxText =
      ('nlpText', 'nlpJSON', 'nlpValue', 'nlpLeft', 'nlpRight');
    CHTTPPorts: array[TNyxHTTPPort] of TNyxText =
      ('nhpTrigger', 'nhpBody', 'nhpStatus', 'nhpHeaders', 'nhpError', 'nhpSucceeded',
       'nhpHeaderVisibility');
  var
    LPort: TNyxLogicPort;
    LHTTPPort: TNyxHTTPPort;
    LExpression: TNyxText;
  begin
    LExpression := 'NyxFlowPort(' + PascalString(AValue.Port.Name) + ')';
    for LPort := Low(TNyxLogicPort) to High(TNyxLogicPort) do
    begin

      if AValue.Port.Name = NyxLogicPort(LPort).Name then
      begin
        LExpression := 'NyxLogicPort(' + CPorts[LPort] + ')';
        Break;
      end;
    end;
    for LHTTPPort := Low(TNyxHTTPPort) to High(TNyxHTTPPort) do
    begin

      if AValue.Port.Name = NyxHTTPPort(LHTTPPort).Name then
      begin
        LExpression := 'NyxHTTPPort(' + CHTTPPorts[LHTTPPort] + ')';
        Break;
      end;
    end;
    Result := 'NyxFlowEndpoint(NyxFlowStep(' + PascalString(AValue.Step.Name) +
      '), ' + LExpression + ')';
  end;

begin
  Result := 'NewNyxFlowRoutes';
  for LIndex := 0 to ARoutes.Count - 1 do
  begin
    LRoute := ARoutes.Route(LIndex);
    Result := Result + #10 + '        .WithRoute(';

    if LRoute.Trigger = ntNamed then
    begin
      Result := Result + 'NyxNamedFlowRoute(';
      LArgument := 'NyxNamedEvent(' + PascalString(LRoute.EventName.Name) + ')';
    end
    else
    begin
      Result := Result + 'NyxFlowRoute(';
      LArgument := NyxTriggerSymbol(LRoute.Trigger);
    end;
    Result := Result + 'NyxFlowRouteID(' + PascalString(LRoute.ID.Name) +
      '), NyxFlowRef(' + PascalString(LRoute.Flow.Name) + '),' + #10 +
      '          ' + LArgument + ', ' + CCallbacks[LRoute.CallbackMode] + ')';
    for LMap := 0 to LRoute.InputCount - 1 do
    begin
      LInput := LRoute.Input(LMap);
      LArgument := Endpoint(LInput.Target);
      case LInput.Source of
        nfisState:
          Result := Result + #10 + '          .InputState(' + LArgument + ', ' +
            CStates[NyxFlowStateKind(LInput.ValueType)] + '(' + PascalString(LInput.StateName) + '))';
        nfisEventValue:
          Result := Result + #10 + '          .InputEventValue(' + LArgument + ', ' +
            CTypes[LInput.ValueType] + ')';
        nfisSignal:
          Result := Result + #10 + '          .InputSignal(' + LArgument + ')';
        nfisEventDetails:
          begin
            Result := Result + #10 + '          .InputEventDetails(' + LArgument + ', NyxDataPath';
            LPath := LInput.DetailsPath.ToData;
            for LPart := 0 to LPath.Count - 1 do
            begin

              if LPath.Item(LPart).Kind = ndText then
              begin
                Result := Result + '.Field(' + PascalString(LPath.Item(LPart).AsText) + ')';
              end
              else
              begin
                Result := Result + '.Item(' + LPath.Item(LPart).ToJSON + ')';
              end;
            end;
            Result := Result + ')';
          end;
        nfisConstant:
          begin
            LValue := TNyxFlowValue.FromData(LInput.ConstantData);
            case LValue.ValueType of
              nftText: LArgument := 'NyxFlowText(' + PascalString(LValue.AsText) + ')';
              nftBoolean: LArgument := 'NyxFlowBoolean(' + PascalBoolean(LValue.AsBoolean) + ')';
              nftInteger: LArgument := 'NyxFlowInteger(' + LValue.ToData.Field('value').ToJSON + ')';
              nftNumber: LArgument := 'NyxFlowNumber(NyxDecimal(' +
                PascalString(LValue.AsNumber.Text) + '))';
              nftJSON: LArgument := 'NyxFlowJSON(TNyxDataValue.ParseJSON(' +
                PascalString(LValue.AsJSON.ToJSON) + '))';
              nftBytes: LArgument := 'TNyxFlowValue.FromData(TNyxDataValue.ParseJSON(' +
                PascalString(LValue.ToData.ToJSON) + '))';
              nftSignal: LArgument := 'NyxFlowSignal';
            end;
            Result := Result + #10 + '          .InputConstant(' + Endpoint(LInput.Target) +
              ', ' + LArgument + ')';
          end;
      end;
    end;
    for LMap := 0 to LRoute.OutputCount - 1 do
    begin
      LOutput := LRoute.Output(LMap);
      Result := Result + #10 + '          .OutputState(' + Endpoint(LOutput.Source) + ', ' +
        CStates[LOutput.StateKind] + '(' + PascalString(LOutput.StateName) + '))';
    end;
    Result := Result + #10 + '          .Using(' + CPolicies[LRoute.Policy] + ')' +
      '.Concurrent(' + CConcurrency[LRoute.Concurrency] + ')' + #10 +
      '          .Enabled(' + PascalBoolean(LRoute.IsEnabled) + ')' +
      '.MaximumRuns(' + TNyxText(IntToStr(LRoute.RunLimit)) + ')';
    for LMap := 0 to LRoute.RowOutputCount - 1 do
    begin
      LRowsOutput := LRoute.RowOutput(LMap);
      Result := Result + #10 + '          .OutputRows(' + Endpoint(LRowsOutput.Source) +
        ', NyxCollection(' + PascalString(LRowsOutput.Collection.Name) + '),' +
        #10 + '            ' + DataRowsExpression(LRowsOutput.Rows) + ')';
    end;
    Result := Result + ')';
  end;
end;
