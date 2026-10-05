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



program nyx_agent_collection_schema;
{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.studio.mcp, nyx.studio.agents;

var
  LTools: TNyxDataValue;
  LTool: TNyxDataValue;
  LSchema: TNyxDataValue;
  LVariant: TNyxDataValue;
  LChanges: TNyxDataValue;
  LChange: TNyxDataValue;
  LProperties: TNyxDataValue;
  LDefinition: TNyxDataValue;
  LIndex: Integer;
  LMode: Integer;
  LChecks: Integer;
  LFound: Boolean;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Collection discovery: ' + AReason);
  end;
  Inc(LChecks);
end;

begin
  try
    { This is the actual MCP discovery builder. No server constructor, listener,
      personal configuration or authenticated active project is touched. }
    LTools := NyxStudioMCPTools.Field('tools');
    Check(LTools.Count = 19, 'complete source catalog count');
    LFound := False;
    for LIndex := 0 to LTools.Count - 1 do
    begin
      LTool := LTools.Item(LIndex);

      if LTool.Field('name').AsText = 'nyx_collections' then
      begin
        LFound := True;
        Break;
      end;
    end;
    Check(LFound, 'new focused tool is actually discoverable');
    Check(not LTool.Field('annotations').Field('readOnlyHint').AsBoolean,
      'mixed query/edit tool advertises conservative mutation hint');
    LSchema := LTool.Field('inputSchema').Field('oneOf');
    Check(LSchema.Count = 8, 'all eight closed context/apply modes are discoverable');
    for LMode := 0 to LSchema.Count - 1 do
    begin
      LVariant := LSchema.Item(LMode);
      Check(not LVariant.Field('additionalProperties').AsBoolean and
        NyxAgentHas(LVariant.Field('properties'), 'workspace') and
        NyxAgentHas(LVariant.Field('properties'), 'review'),
        'strict outer project/review routing ' + IntToStr(LMode));

      if LMode = 3 then
      begin
        Check(LVariant.Field('not').Field('anyOf').Count = 2,
          'row/choice and project/review exclusivity both remain composed');
      end
      else
      begin
        Check(LVariant.Field('not').Field('required').Count = 2,
          'outer project/review exclusivity ' + IntToStr(LMode));
      end;
    end;
    LProperties := LSchema.Item(2).Field('properties');
    Check((LProperties.Field('limit').Field('maximum').AsInteger = 20) and
      (LProperties.Field('fields').Field('maxItems').AsInteger = 16) and
      LProperties.Field('fields').Field('uniqueItems').AsBoolean, 'row query budgets');
    Check(LSchema.Item(3).Field('properties').Field('count').Field('maximum').AsInteger = 4096,
      'Unicode scalar window limit');
    Check((LSchema.Item(6).Field('properties').Field('source').Field('enum').Count = 3) and
      (LSchema.Item(6).Field('properties').Field('source').Field('enum').Item(2).AsText = 'restorable'),
      'masked inherited titles expose the same exact-window query contract');
    LProperties := LSchema.Item(7).Field('properties');
    Check((LProperties.Field('changes').Field('maxItems').AsInteger = 32) and
      (LProperties.Field('operationId').Field('maxLength').AsInteger = 120),
      'group and retry identity limits');
    LChanges := LProperties.Field('changes').Field('items').Field('oneOf');
    Check(LChanges.Count = 29, 'named commands and all seventeen ordinary actions');
    LDefinition := LChanges.Item(1).Field('properties').Field('definition').Field('oneOf');
    Check((LDefinition.Count = 4) and
      (LDefinition.Item(0).Field('properties').Field('default').Field('type').AsText = 'string') and
      (LDefinition.Item(2).Field('properties').Field('default').Field('minimum').AsInteger = Low(Integer)),
      'typed named defaults and signed integer constraints');
    Check(LDefinition.Item(0).Field('properties').Field('domain').Field('oneOf').Item(1)
      .Field('properties').Field('choices').Field('maxItems').AsInteger = 128,
      'bounded typed domain choices');
    LDefinition := LChanges.Item(5).Field('properties').Field('spec').Field('oneOf');
    Check((LDefinition.Count = 2) and
      (LDefinition.Item(0).Field('properties').Field('version').Field('const').AsInteger = 1) and
      (LDefinition.Item(1).Field('required').Count = 6) and
      (LDefinition.Item(1).Field('properties').Field('selection').Field('const').AsText = 'multiple'),
      'exact versioned single/multiple fluent view descriptor');
    for LIndex := 0 to LChanges.Count - 1 do
    begin
      LChange := LChanges.Item(LIndex);
      Check(not LChange.Field('additionalProperties').AsBoolean and
        not NyxAgentHas(LChange.Field('properties'), 'workspace') and
        not NyxAgentHas(LChange.Field('properties'), 'review'),
        'nested operation keeps strict ownership ' + IntToStr(LIndex));
    end;
    WriteLn('PASS ', LChecks, ' actual collection MCP discovery checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
