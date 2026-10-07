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

program nyx_date_policy_schema;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.studio.agents, nyx.studio.mcp;

var
  LTools: TNyxDataValue;
  LProperties: TNyxDataValue;
  LOperations: TNyxDataValue;
  LVariants: TNyxDataValue;
  LBranch: TNyxDataValue;
  LIndex: Integer;
  LChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(LChecks);
end;

{ Inspect the real discovery catalog without constructing a protocol engine,
  starting a listener or refreshing credentials. These checks protect an agent's
  discoverable typed boundary; the companion exercises actual atomic admission. }
function Tool(const AName: TNyxText): TNyxDataValue;
var
  LToolIndex: Integer;
begin
  for LToolIndex := 0 to LTools.Count - 1 do
  begin

    if LTools.Item(LToolIndex).Field('name').AsText = AName then
    begin
      Exit(LTools.Item(LToolIndex));
    end;
  end;
  raise Exception.Create('Tool is absent: ' + AName);
end;

function Operation(const AName: TNyxText): TNyxDataValue;
var
  LOperationIndex: Integer;
begin
  for LOperationIndex := 0 to LOperations.Count - 1 do
  begin

    if LOperations.Item(LOperationIndex).Field('properties').Field('op')
      .Field('const').AsText = AName then
    begin
      Exit(LOperations.Item(LOperationIndex));
    end;
  end;
  raise Exception.Create('Operation is absent: ' + AName);
end;

begin
  try
    LTools := NyxStudioMCPTools.Field('tools');
    LProperties := Tool('nyx_node').Field('inputSchema').Field('properties');
    Check(LProperties.Field('valueDomain').Field('type').AsText = 'boolean',
      'Domain inspection is opt-in');
    Check(LProperties.Field('domainScope').Field('enum').ToJSON =
      NyxArray([NyxData('local'), NyxData('effective')]).ToJSON,
      'Local and effective policies are distinct query choices');
    Check((LProperties.Field('domainLimit').Field('maximum').AsInteger = 16) and
      (LProperties.Field('domainOffset').Field('maximum').AsInteger = 128),
      'Domain choices use bounded paging');
    LOperations := Tool('nyx_transaction').Field('inputSchema')
      .Field('properties').Field('operations').Field('items').Field('oneOf');
    LBranch := Operation('value-domain-inherit');
    Check(not LBranch.Field('additionalProperties').AsBoolean and
      (LBranch.Field('required').Count = 2),
      'Inheritance is an exact closed operation, not a null or string policy');
    LBranch := Operation('value-domain-set');
    Check(not LBranch.Field('additionalProperties').AsBoolean and
      (LBranch.Field('required').Count = 3),
      'Setting requires an exact owner and typed domain');
    LVariants := LBranch.Field('properties').Field('domain').Field('oneOf');
    Check(LVariants.Count = 5, 'All scalar families and Gregorian format are discoverable');
    for LIndex := 0 to LVariants.Count - 1 do
    begin
      LBranch := LVariants.Item(LIndex);
      Check(not LBranch.Field('additionalProperties').AsBoolean and
        LBranch.Field('properties').Field('choices').Field('uniqueItems').AsBoolean and
        (LBranch.Field('properties').Field('choices').Field('maxItems').AsInteger = 128),
        'Every family closes unknown fields and bounds exact unique choices');
    end;
    LProperties := LVariants.Item(2).Field('properties');
    Check((LProperties.Field('min').Field('type').AsText = 'integer') and
      (LProperties.Field('min').Field('minimum').AsInteger = Low(Integer)) and
      (LProperties.Field('max').Field('maximum').AsInteger = High(Integer)),
      'Integer constraints retain native signed scalar limits');
    LProperties := LVariants.Item(4).Field('properties');
    Check((LProperties.Field('format').Field('const').AsText = 'date') and
      (LProperties.Field('min').Field('type').AsText = 'string') and
      (LProperties.Field('min').Field('minLength').AsInteger = 10) and
      (LVariants.Item(4).Field('anyOf').Item(1).Field('required').Count = 2),
      'Calendar format discovers canonical paired bounds, with strict Pascal date admission');
    WriteLn('PASS ', LChecks, ' offline value-domain MCP schema checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
