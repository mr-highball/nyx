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
program nyx_choice_schema;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.choices, nyx.studio.agents, nyx.studio.mcp;

var
  GChecks: Integer;
  GTools: TNyxDataValue;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

{ The real discovery schema is pure: constructing no engine/listener avoids
  touching operator credentials, services, projects or network configuration. }
function Tool(const AName: TNyxText): TNyxDataValue;
var
  LIndex: Integer;
begin
  for LIndex := 0 to GTools.Count - 1 do
  begin

    if GTools.Item(LIndex).Field('name').AsText = AName then
    begin
      Exit(GTools.Item(LIndex));
    end;
  end;
  raise Exception.Create('Tool is absent');
end;

procedure Run;
var
  LProperties: TNyxDataValue;
  LOperations: TNyxDataValue;
  LOperation: TNyxDataValue;
  LRow: TNyxDataValue;
  LName: TNyxDataValue;
  LIndex: Integer;
  LSet: Boolean;
  LInherit: Boolean;
begin
  GTools := NyxStudioMCPTools.Field('tools');
  LProperties := Tool('nyx_node').Field('inputSchema').Field('properties');
  Check(LProperties.Field('choices').Field('type').AsText = 'boolean', 'choice inspection is opt-in');
  Check(LProperties.Field('choiceScope').Field('enum').ToJSON =
    NyxArray([NyxData('local'), NyxData('effective')]).ToJSON, 'local and effective scopes are discoverable');
  Check((LProperties.Field('choiceOffset').Field('maximum').AsInteger = NyxMaximumChoiceItems) and
    (LProperties.Field('choiceLimit').Field('maximum').AsInteger = 16), 'row pages are bounded independently');
  LOperations := Tool('nyx_transaction').Field('inputSchema')
    .Field('properties').Field('operations').Field('items').Field('oneOf');
  LSet := False;
  LInherit := False;
  for LIndex := 0 to LOperations.Count - 1 do
  begin
    LOperation := LOperations.Item(LIndex);

    if not NyxAgentHas(LOperation, 'properties') then
    begin
      { Other domains may group their operation alternatives under oneOf.
        This check owns the two explicit choice branches, both required below. }
      Continue;
    end;
    LName := LOperation.Field('properties').Field('op');

    if not NyxAgentHas(LName, 'const') then
    begin
      Continue;
    end;

    if LName.Field('const').AsText = 'choices-set' then
    begin
      LSet := True;
      Check(not LOperation.Field('additionalProperties').AsBoolean and
        (LOperation.Field('required').Count = 3), 'set requires a closed owner/descriptor operation');
      LProperties := LOperation.Field('properties').Field('choices');
      Check(not LProperties.Field('additionalProperties').AsBoolean and
        (LProperties.Field('properties').Field('version').Field('const').AsInteger = 1),
        'descriptor version and members are explicit');
      LProperties := LProperties.Field('properties').Field('items');
      Check(LProperties.Field('maxItems').AsInteger = NyxMaximumChoiceItems,
        'discovery shares the portable choice count allowance');
      LRow := LProperties.Field('items');
      Check(not LRow.Field('additionalProperties').AsBoolean and
        (LRow.Field('required').Count = 2) and
        (LRow.Field('properties').Field('value').Field('type').AsText = 'string') and
        (LRow.Field('properties').Field('caption').Field('type').AsText = 'string'),
        'row values are exact text with closed fields, including empty keys');
    end
    else if LName.Field('const').AsText = 'choices-inherit' then
    begin
      LInherit := True;
      Check(not LOperation.Field('additionalProperties').AsBoolean and
        (LOperation.Field('required').Count = 2), 'inherit has no ambiguous null set');
    end;
  end;
  Check(LSet and LInherit, 'both grouped operations are discoverable');
end;

begin
  try
    Run;
    WriteLn('PASS ', GChecks, ' choice MCP schema checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
