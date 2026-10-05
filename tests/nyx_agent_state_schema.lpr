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

program nyx_agent_state_schema;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.studio.agents, nyx.studio.mcp;

var
  LTools: TNyxDataValue;
  LTool: TNyxDataValue;
  LSchema: TNyxDataValue;
  LVariant: TNyxDataValue;
  LChanges: TNyxDataValue;
  LIndex: Integer;
  LCount: Integer;
  LFound: Boolean;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('State schema: ' + AReason);
  end;
  Inc(LCount);
end;

begin
  try
    { The actual tools/list catalog runs without a listener or any constructor
      that refreshes local Codex credentials. This is offline discovery evidence. }
    LTools := NyxStudioMCPTools.Field('tools');
    Check(LTools.Count = 18, 'staged catalog contains eighteen tools');
    LFound := False;
    for LIndex := 0 to LTools.Count - 1 do
    begin

      if LTools.Item(LIndex).Field('name').AsText = 'nyx_state' then
      begin
        LTool := LTools.Item(LIndex);
        LFound := True;
        Break;
      end;
    end;
    Check(LFound, 'state tool is advertised');
    Check(not LTool.Field('annotations').Field('readOnlyHint').AsBoolean,
      'mixed read/write tool has conservative mutation annotation');
    LSchema := LTool.Field('inputSchema');
    Check(LSchema.Field('oneOf').Count = 4, 'four focused modes');
    for LIndex := 0 to 3 do
    begin
      LVariant := LSchema.Field('oneOf').Item(LIndex);
      Check((LVariant.Field('properties').Field('workspace').Kind = ndObject) and
        (LVariant.Field('properties').Field('review').Kind = ndObject) and
        not LVariant.Field('additionalProperties').AsBoolean,
        'routing is on each closed outer alternative');
    end;
    LChanges := LVariant.Field('properties').Field('changes').Field('items').Field('oneOf');
    Check(LChanges.Count = 7, 'closed scalar and binding operation variants');
    Check(not LChanges.Item(0).Field('additionalProperties').AsBoolean and
      (LChanges.Item(0).Field('properties').Field('value').Field('type').AsText = 'string'),
      'text mutation exposes exact primitive and fields');
    Check((LChanges.Item(2).Field('properties').Field('value').Field('type').AsText = 'integer') and
      (LChanges.Item(2).Field('properties').Field('value').Field('minimum').AsInteger = Low(Integer)),
      'integer mutation exposes signed native range');
    Check(LChanges.Item(5).Field('properties').Field('target').Field('enum').Count = 19,
      'all nineteen binding targets are discoverable');
    Check(not NyxAgentHas(LChanges.Item(5).Field('properties'), 'workspace') and
      not NyxAgentHas(LChanges.Item(5).Field('properties'), 'review'),
      'context routing does not leak into nested mutations');
    for LIndex := 0 to LTools.Count - 1 do
    begin

      if LTools.Item(LIndex).Field('name').AsText = 'nyx_callbacks' then
      begin
        LVariant := LTools.Item(LIndex).Field('inputSchema').Field('oneOf').Item(1);
        Check((LVariant.Field('not').Field('anyOf').Count = 2) and
          (LVariant.Field('not').Field('anyOf').Item(0).Field('anyOf').Count = 2) and
          (LVariant.Field('not').Field('anyOf').Item(1).Field('required').Count = 2),
          'callback review prohibitions and context exclusivity both survive');
      end;
    end;
    WriteLn('PASS ', LCount, ' offline state MCP schema checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
