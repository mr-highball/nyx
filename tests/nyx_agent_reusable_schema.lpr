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

program nyx_agent_reusable_schema;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.data, nyx.studio.mcp, nyx.studio.agents;

var
  LTools, LNode, LTransaction, LVariants, LProperties: TNyxDataValue;
  LIndex, LChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Reusable discovery: ' + AReason);
  end;
  Inc(LChecks);
end;

begin
  try
    { Actual discovery construction without starting any service. This covers
      shared tool routing as well as the closed semantic operation contracts. }
    LTools := NyxStudioMCPTools.Field('tools');
    Check(LTools.Count = 20, 'source catalog remains focused');
    for LIndex := 0 to LTools.Count - 1 do
    begin

      if LTools.Item(LIndex).Field('name').AsText = 'nyx_node' then
      begin
        LNode := LTools.Item(LIndex);
      end;

      if LTools.Item(LIndex).Field('name').AsText = 'nyx_transaction' then
      begin
        LTransaction := LTools.Item(LIndex);
      end;
    end;
    LProperties := LNode.Field('inputSchema').Field('properties');
    Check(LProperties.Field('parts').Field('type').AsText = 'boolean',
      'named-part context is an opt-in Boolean');
    Check((LProperties.Field('partLimit').Field('minimum').AsInteger = 1) and
      (LProperties.Field('partLimit').Field('maximum').AsInteger = 50),
      'named-part response remains bounded');
    Check(NyxAgentHas(LProperties, 'workspace') and NyxAgentHas(LProperties, 'review'),
      'bounded query retains explicit project/review routing');
    LProperties := LTransaction.Field('inputSchema').Field('properties');
    Check(NyxAgentHas(LProperties, 'expectedRevision') and NyxAgentHas(LProperties, 'operationId') and
      NyxAgentHas(LProperties, 'workspace') and NyxAgentHas(LProperties, 'review'),
      'mutation retains revision/receipt/project/review routing');
    LVariants := LProperties.Field('operations').Field('items').Field('oneOf');
    Check(LVariants.Count = 16, 'ordinary, reusable, relative placement and named presentation operations');
    for LIndex := 0 to LVariants.Count - 1 do
    begin
      Check(not LVariants.Item(LIndex).Field('additionalProperties').AsBoolean,
        'operation shape refuses unknown fields: ' + IntToStr(LIndex));
    end;
    Check(LVariants.Item(6).Field('properties').Field('identities').Field('additionalProperties')
      .Field('type').AsText = 'string', 'derivation identities retain exact authored names');
    Check(LVariants.Item(7).Field('properties').Field('index').Field('minimum').AsInteger = 0,
      'explicit instance insertion positions are nonnegative');
    Check(LVariants.Item(8).Field('properties').Field('mode').Field('enum').Count = 5,
      'all five typed part operations are discoverable');
    Check(not NyxAgentHas(LVariants.Item(9).Field('properties'), 'mode'),
      'restoring inheritance cannot smuggle an override mode');
    Check(LVariants.Item(10).Field('properties').Field('placement').Field('enum').Count = 3,
      'relative placement exposes three closed positions');
    Check(LVariants.Item(11).Field('required').Count = 5,
      'new placement requires an exact kind, identity and relative target');
    WriteLn('PASS ', LChecks, ' actual reusable discovery checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
