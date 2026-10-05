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


program nyx_import_schema;
{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.data, nyx.studio.mcp, nyx.studio.agents;

var
  LTools: TNyxDataValue;
  LTool: TNyxDataValue;
  LVariants: TNyxDataValue;
  LVariant: TNyxDataValue;
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
    { Actual discovery builder only: no server/configuration constructor. }
    LTools := NyxStudioMCPTools.Field('tools');
    Check(LTools.Count = 19, 'Catalog inventory remains nineteen');
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
    Check(LFound, 'Existing Pascal tool advertises imports');
    LVariants := LTool.Field('inputSchema').Field('oneOf');
    Check(LVariants.Count = 4, 'Four focused Pascal modes');
    for LIndex := 0 to LVariants.Count - 1 do
    begin
      LVariant := LVariants.Item(LIndex);
      Check(not LVariant.Field('additionalProperties').AsBoolean,
        'Every outer mode refuses unknown members');
      Check(NyxAgentHas(LVariant.Field('properties'), 'workspace') and
        NyxAgentHas(LVariant.Field('properties'), 'review'), 'Exact context routing remains outer only');
      Check(LVariant.Field('not').Field('required').Count = 2,
        'Mixed workspace/review contexts remain prohibited');
    end;
    LVariant := LVariants.Item(2);
    Check(LVariant.Field('properties').Field('mode').Field('const').AsText = 'imports',
      'Import query is discoverable');
    Check(LVariant.Field('properties').Field('limit').Field('maximum').AsInteger = 50,
      'Query budget is discoverable');
    LVariant := LVariants.Item(3);
    Check(LVariant.Field('properties').Field('mode').Field('const').AsText = 'edit-imports',
      'Grouped import mutation is discoverable');
    LChange := LVariant.Field('properties').Field('changes');
    Check((LChange.Field('minItems').AsInteger = 1) and
      (LChange.Field('maxItems').AsInteger = 32), 'Typed group budget is exact');
    LChange := LChange.Field('items');
    Check(not LChange.Field('additionalProperties').AsBoolean and
      not NyxAgentHas(LChange.Field('properties'), 'workspace') and
      not NyxAgentHas(LChange.Field('properties'), 'review'), 'No nested context/code injection');
    Check((LChange.Field('properties').Field('op').Field('enum').Count = 2) and
      (LChange.Field('properties').Field('section').Field('enum').Count = 2),
      'Action and section choices are closed');
    Check(LChange.Field('properties').Field('unit').Field('maxLength').AsInteger = 120,
      'Unit namespace has the compiler admission budget');
    WriteLn('PASS ', LChecks, ' actual Pascal import discovery checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
