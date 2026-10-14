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


{ Pure route grammar: configurations, mappings and constants become owned values.
  It never resolves a creator, constructs a host operation or subscribes events. }
function TConfigurationReader.FlowRouteConstructor(const AName: TNyxText): TValue;
var
  LArgs: TValues;
  LMethod: TNyxText;
  LRoutes: INyxFlowRoutes;
  LValue: TNyxFlowValue;
  LState: TNyxStateKind;

  procedure Count(AValue: Integer);
  begin

    if Length(LArgs) <> AValue then
    begin
      Fail('Wrong typed Logic route argument count');
    end;
  end;

  procedure Kind(AIndex: Integer; AKind: TValueKind);
  begin

    if (AIndex >= Length(LArgs)) or (LArgs[AIndex].Kind <> AKind) then
    begin
      Fail('Wrong typed Logic route argument family');
    end;
  end;

begin
  Result := Default(TValue);
  LArgs := nil;

  if AName = 'tnyxflowvalue' then
  begin
    Expect('.');
    Expect('FromData');
    LArgs := Arguments(1);
    Count(1);
    Kind(0, vkData);
    Result.Kind := vkFlowValue;
    Result.Data := TNyxFlowValue.FromData(LArgs[0].Data).ToData;
    Exit;
  end;

  if At('(') then
  begin
    LArgs := Arguments(4);
  end;

  if AName = 'newnyxflowroutes' then
  begin
    Count(0);
    Result.Kind := vkFlowRoutes;
    LRoutes := NewNyxFlowRoutes;
  end
  else if AName = 'nyxflowrouteid' then
  begin
    Count(1);
    Kind(0, vkText);
    Result.Kind := vkFlowRouteID;
    Result.Text := NyxFlowRouteID(LArgs[0].Text).Name;
  end
  else if (AName = 'nyxflowroute') or (AName = 'nyxnamedflowroute') then
  begin
    Count(4);
    Kind(0, vkFlowRouteID);
    Kind(1, vkFlowRef);
    Kind(3, vkFlowCallbackMode);
    Result.Kind := vkFlowRoute;

    if AName = 'nyxflowroute' then
    begin
      Kind(2, vkTrigger);
      Result.FlowRoute := NyxFlowRoute(NyxFlowRouteID(LArgs[0].Text),
        NyxFlowRef(LArgs[1].Text), TNyxTrigger(LArgs[2].Ordinal),
        TNyxFlowCallbackMode(LArgs[3].Ordinal));
    end
    else
    begin
      Kind(2, vkEvent);
      Result.FlowRoute := NyxNamedFlowRoute(NyxFlowRouteID(LArgs[0].Text),
        NyxFlowRef(LArgs[1].Text), NyxNamedEvent(LArgs[2].Text),
        TNyxFlowCallbackMode(LArgs[3].Ordinal));
    end;
  end
  else
  begin
    Result.Kind := vkFlowValue;

    if AName = 'nyxflowsignal' then
    begin
      Count(0);
      LValue := NyxFlowSignal;
    end
    else
    begin
      Count(1);

      if AName = 'nyxflowtext' then
      begin
        Kind(0, vkText);
        LValue := NyxFlowText(LArgs[0].Text);
      end
      else if AName = 'nyxflowboolean' then
      begin
        Kind(0, vkBoolean);
        LValue := NyxFlowBoolean(LArgs[0].Ordinal <> 0);
      end
      else if AName = 'nyxflowinteger' then
      begin
        Kind(0, vkInteger);
        LValue := NyxFlowInteger(StrToInt(LArgs[0].Text));
      end
      else if AName = 'nyxflownumber' then
      begin
        Kind(0, vkDecimal);
        LValue := NyxFlowNumber(NyxDecimal(LArgs[0].Text));
      end
      else
      begin
        Kind(0, vkData);
        LValue := NyxFlowJSON(LArgs[0].Data);
      end;
    end;
    Result.Data := LValue.ToData;
    Exit;
  end;
  while At('.') do
  begin
    Inc(FCursor);
    LMethod := LowerCase(FTokens[FCursor].Text);
    Inc(FCursor);
    LArgs := nil;

    if At('(') then
    begin
      LArgs := Arguments(2);
    end;

    if Result.Kind = vkFlowRoutes then
    begin
      Count(1);

      if LMethod = 'withroute' then
      begin
        Kind(0, vkFlowRoute);
        LRoutes := LRoutes.WithRoute(LArgs[0].FlowRoute);
      end
      else if LMethod = 'withoutroute' then
      begin
        Kind(0, vkFlowRouteID);
        LRoutes := LRoutes.WithoutRoute(NyxFlowRouteID(LArgs[0].Text));
      end
      else
      begin
        Fail('Unsupported typed Logic route set method');
      end;
    end
    else if Result.Kind = vkFlowRoute then
    begin

      if (LMethod = 'inputstate') or (LMethod = 'outputstate') then
      begin
        Count(2);
        Kind(0, vkFlowEndpoint);
        Kind(1, vkStateRef);
        LState := TNyxStateKind(LArgs[1].Ordinal);

        if LMethod = 'inputstate' then
        begin
          case LState of
            nskText: Result.FlowRoute := Result.FlowRoute.InputState(LArgs[0].FlowEndpoint, NyxTextState(LArgs[1].Text));
            nskBoolean: Result.FlowRoute := Result.FlowRoute.InputState(LArgs[0].FlowEndpoint, NyxBooleanState(LArgs[1].Text));
            nskInteger: Result.FlowRoute := Result.FlowRoute.InputState(LArgs[0].FlowEndpoint, NyxIntegerState(LArgs[1].Text));
            nskNumber: Result.FlowRoute := Result.FlowRoute.InputState(LArgs[0].FlowEndpoint, NyxNumberState(LArgs[1].Text));
          end;
        end
        else
        begin
          case LState of
            nskText: Result.FlowRoute := Result.FlowRoute.OutputState(LArgs[0].FlowEndpoint, NyxTextState(LArgs[1].Text));
            nskBoolean: Result.FlowRoute := Result.FlowRoute.OutputState(LArgs[0].FlowEndpoint, NyxBooleanState(LArgs[1].Text));
            nskInteger: Result.FlowRoute := Result.FlowRoute.OutputState(LArgs[0].FlowEndpoint, NyxIntegerState(LArgs[1].Text));
            nskNumber: Result.FlowRoute := Result.FlowRoute.OutputState(LArgs[0].FlowEndpoint, NyxNumberState(LArgs[1].Text));
          end;
        end;
      end
      else if (LMethod = 'inputconstant') or (LMethod = 'inputeventvalue') or
        (LMethod = 'inputeventdetails') or (LMethod = 'inputsignal') then
      begin
        Kind(0, vkFlowEndpoint);

        if LMethod = 'inputsignal' then
        begin
          Count(1);
          Result.FlowRoute := Result.FlowRoute.InputSignal(LArgs[0].FlowEndpoint);
        end
        else
        begin
          Count(2);

          if LMethod = 'inputconstant' then
          begin
            Kind(1, vkFlowValue);
            Result.FlowRoute := Result.FlowRoute.InputConstant(LArgs[0].FlowEndpoint,
              TNyxFlowValue.FromData(LArgs[1].Data));
          end
          else if LMethod = 'inputeventvalue' then
          begin
            Kind(1, vkFlowType);
            Result.FlowRoute := Result.FlowRoute.InputEventValue(LArgs[0].FlowEndpoint,
              TNyxFlowType(LArgs[1].Ordinal));
          end
          else
          begin
            Kind(1, vkResourcePath);
            Result.FlowRoute := Result.FlowRoute.InputEventDetails(LArgs[0].FlowEndpoint,
              LArgs[1].ResourcePath);
          end;
        end;
      end
      else
      begin
        Count(1);

        if LMethod = 'using' then
        begin
          Kind(0, vkPolicy);
          Result.FlowRoute := Result.FlowRoute.Using(TNyxExecutionPolicy(LArgs[0].Ordinal));
        end
        else if LMethod = 'concurrent' then
        begin
          Kind(0, vkFlowConcurrency);
          Result.FlowRoute := Result.FlowRoute.Concurrent(TNyxFlowConcurrency(LArgs[0].Ordinal));
        end
        else if LMethod = 'enabled' then
        begin
          Kind(0, vkBoolean);
          Result.FlowRoute := Result.FlowRoute.Enabled(LArgs[0].Ordinal <> 0);
        end
        else if LMethod = 'maximumruns' then
        begin
          Kind(0, vkInteger);
          Result.FlowRoute := Result.FlowRoute.MaximumRuns(StrToInt(LArgs[0].Text));
        end
        else
        begin
          Fail('Unsupported typed Logic route method');
        end;
      end;
    end
    else
    begin
      Fail('Typed route identity has no fluent methods');
    end;
  end;

  if Result.Kind = vkFlowRoutes then
  begin
    Result.Data := LRoutes.ToData;
  end;
end;
