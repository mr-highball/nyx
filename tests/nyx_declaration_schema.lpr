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

program nyx_declaration_schema;
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
    Check(LTools.Count = 20, 'Focused routine modes extend the twenty advertised tools');
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
    Check(LModes.Count = 9, 'Existing modes plus declaration counterparts and grouped authoring');
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

    LMode := LModes.Item(7).Field('properties');
    Check((LMode.Field('mode').Field('const').AsText = 'declaration') and
      (LMode.Field('count').Field('maximum').AsInteger = 4096), 'Bounded exact counterpart query');
    Check(LMode.Field('part').Field('enum').Count = 2, 'Signature counterpart is a closed choice');
    LMode := LModes.Item(8).Field('properties');
    Check(LMode.Field('mode').Field('const').AsText = 'edit-declarations', 'Grouped declaration authoring');
    LChange := LMode.Field('changes');
    Check((LChange.Field('maxItems').AsInteger = 16) and
      (LChange.Field('items').Field('oneOf').Count = 4), 'Sixteen typed create/edit/remove/signature operations');
    for LIndex := 0 to 3 do
    begin
      LMode := LChange.Field('items').Field('oneOf').Item(LIndex);
      Check(not LMode.Field('additionalProperties').AsBoolean and
        not NyxAgentHas(LMode.Field('properties'), 'workspace') and
        not NyxAgentHas(LMode.Field('properties'), 'review'), 'Action-specific strict nested schema');
    end;
    LMode := LChange.Field('items').Field('oneOf').Item(0).Field('properties');
    Check((LMode.Field('kind').Field('enum').Count = 2) and
      (LMode.Field('visibility').Field('enum').Count = 2), 'Creation choices are closed Pascal domains');
    LMode := LChange.Field('items').Field('oneOf').Item(2);
    Check(LMode.Field('required').Count = 5, 'Removal acknowledges both exact source counterparts');
    LMode := LChange.Field('items').Field('oneOf').Item(3);
    Check((LMode.Field('properties').Field('op').Field('const').AsText = 'signature') and
      (LMode.Field('required').Count = 9), 'Signature replacement requires complete proposed/accepted counterparts');
    Check((LMode.Field('properties').Field('kind').Field('enum').Count = 2) and
      (LMode.Field('properties').Field('visibility').Field('enum').Count = 2),
      'Signature kind/visibility remain closed Pascal choices');
    Check((LMode.Field('properties').Field('expectedSignature').Field('maxLength').AsInteger = 32768) and
      (LMode.Field('properties').Field('expectedDeclaration').Field('maxLength').AsInteger = 32768),
      'Every acknowledged signature counterpart has the declared fragment budget');
    WriteLn('PASS ', LChecks, ' actual Pascal declaration discovery checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
