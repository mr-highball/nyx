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

program nyx_routine_schema;
{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.data, nyx.studio.mcp, nyx.studio.agents;

var
  LTools: TNyxDataValue;
  LTool: TNyxDataValue;
  LModes: TNyxDataValue;
  LMode: TNyxDataValue;
  LChange: TNyxDataValue;
  LIndex: Integer;
  LChecks: Integer;
  LFound: Boolean;

procedure Check(ACondition: Boolean; const AReason: String);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(LChecks);
end;

begin
  try
    { Actual transport discovery builder; no listener or configuration refresh. }
    LTools := NyxStudioMCPTools.Field('tools');
    Check(LTools.Count = 19, 'Focused routine modes extend the existing nineteen tools');
    LFound := False;
    for LIndex := 0 to LTools.Count - 1 do
    begin
      LTool := LTools.Item(LIndex);

      if LTool.Field('name').AsText = 'nyx_pascal' then
      begin
        LFound := True;
        Break;
      end;
    end;
    Check(LFound, 'The actual Pascal tool contains routine capabilities');
    LModes := LTool.Field('inputSchema').Field('oneOf');
    Check(LModes.Count = 7, 'Existing callback/import modes plus three routine modes');
    for LIndex := 0 to LModes.Count - 1 do
    begin
      LMode := LModes.Item(LIndex);
      Check(not LMode.Field('additionalProperties').AsBoolean, 'Every mode is strict');
      Check(NyxAgentHas(LMode.Field('properties'), 'workspace') and
        NyxAgentHas(LMode.Field('properties'), 'review'), 'Context routing remains outer');
      Check(LMode.Field('not').Field('required').Count = 2, 'Mixed contexts refuse');
    end;
    LMode := LModes.Item(4).Field('properties');
    Check((LMode.Field('mode').Field('const').AsText = 'routines') and
      (LMode.Field('limit').Field('maximum').AsInteger = 50), 'Bounded names discovery');
    LMode := LModes.Item(5).Field('properties');
    Check((LMode.Field('mode').Field('const').AsText = 'routine') and
      (LMode.Field('count').Field('maximum').AsInteger = 4096), 'Bounded Unicode implementation window');
    Check(LMode.Field('routine').Field('maxLength').AsInteger = 120, 'Distinct qualified name budget');
    LMode := LModes.Item(6).Field('properties');
    Check(LMode.Field('mode').Field('const').AsText = 'edit-routines', 'Grouped helper editing is advertised');
    LChange := LMode.Field('changes');
    Check((LChange.Field('minItems').AsInteger = 1) and
      (LChange.Field('maxItems').AsInteger = 16), 'Group budget remains sixteen');
    LChange := LChange.Field('items');
    Check(not LChange.Field('additionalProperties').AsBoolean and
      not NyxAgentHas(LChange.Field('properties'), 'workspace') and
      not NyxAgentHas(LChange.Field('properties'), 'review'), 'No per-change context injection');
    Check((LChange.Field('required').Count = 3) and
      (LChange.Field('properties').Field('expected').Field('maxLength').AsInteger = 32768) and
      (LChange.Field('properties').Field('implementation').Field('maxLength').AsInteger = 32768),
      'Exact expected and proposed text have declared per-field budgets');
    WriteLn('PASS ', LChecks, ' actual Pascal routine discovery checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
