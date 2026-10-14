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

{ Tool wire names are an explicit discovery boundary. Resolve an exact operation
  independently of schema order; new unrelated operations must not shift a
  reusable assertion onto another contract. Missing/duplicate branches refuse. }
function Operation(const AName: TNyxText): TNyxDataValue;
var
  LBranch: Integer;
  LMatches: Integer;
  LIdentity: TNyxDataValue;
  LChoice: Integer;
  LMatched: Boolean;
begin
  LMatches := 0;
  for LBranch := 0 to LVariants.Count - 1 do
  begin

    LIdentity := LVariants.Item(LBranch).Field('properties').Field('op');
    LMatched := False;

    if NyxAgentHas(LIdentity, 'const') then
    begin
      LMatched := LIdentity.Field('const').AsText = AName;
    end
    else
    begin
      { Some related operations share a closed enum branch, for example
        presentation-use/reset. Match a declared choice, never an arbitrary name. }

      if not NyxAgentHas(LIdentity, 'enum') or (LIdentity.Field('enum').Count = 0) then
      begin
        raise Exception.Create('Reusable discovery operation has no exact identity');
      end;
      for LChoice := 0 to LIdentity.Field('enum').Count - 1 do
      begin

        if LIdentity.Field('enum').Item(LChoice).AsText = AName then
        begin
          LMatched := True;
        end;
      end;
    end;

    if LMatched then
    begin
      Inc(LMatches);
      Result := LVariants.Item(LBranch);
    end;
  end;

  if LMatches <> 1 then
  begin
    raise Exception.Create('Reusable discovery requires one exact operation: ' + AName);
  end;
end;

{ Operation families may be nested oneOf branches. Inspect every concrete shape
  without treating an unrelated family addition/order as a reusable regression. }
function ClosedVariants(const AVariants: TNyxDataValue): TNyxDataValue;
var
  LItems: array of TNyxDataValue;

  procedure Visit(const AValue: TNyxDataValue);
  var
    LIndex: Integer;
  begin

    if NyxAgentHas(AValue, 'oneOf') then
    begin
      for LIndex := 0 to AValue.Field('oneOf').Count - 1 do
      begin
        Visit(AValue.Field('oneOf').Item(LIndex));
      end;
    end
    else
    begin
      SetLength(LItems, Length(LItems) + 1);
      LItems[High(LItems)] := AValue;
    end;
  end;

var
  LIndex: Integer;
begin
  LItems := nil;
  for LIndex := 0 to AVariants.Count - 1 do
  begin
    Visit(AVariants.Item(LIndex));
  end;
  Result := NyxArray(LItems);
end;

begin
  try
    { Actual discovery construction without starting any service. This covers
      shared tool routing as well as the closed semantic operation contracts. }
    LTools := NyxStudioMCPTools.Field('tools');
    Check(LTools.Count = 24, 'current source catalog includes focused Logic discovery');
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
    LVariants := ClosedVariants(LProperties.Field('operations').Field('items').Field('oneOf'));
    Check(LVariants.Count > 0, 'discovery supplies concrete closed operation shapes');
    for LIndex := 0 to LVariants.Count - 1 do
    begin
      Check(not LVariants.Item(LIndex).Field('additionalProperties').AsBoolean,
        'operation shape refuses unknown fields: ' + IntToStr(LIndex));
    end;
    Check(Operation('derive').Field('properties').Field('identities').Field('additionalProperties')
      .Field('type').AsText = 'string', 'derivation identities retain exact authored names');
    Check(Operation('instance').Field('properties').Field('index').Field('minimum').AsInteger = 0,
      'explicit instance insertion positions are nonnegative');
    Check(Operation('override').Field('properties').Field('mode').Field('enum').Count = 5,
      'all five typed part operations are discoverable');
    Check(not NyxAgentHas(Operation('inherit').Field('properties'), 'mode'),
      'restoring inheritance cannot smuggle an override mode');
    Check(Operation('place').Field('properties').Field('placement').Field('enum').Count = 3,
      'relative placement exposes three closed positions');
    Check(Operation('place-new').Field('required').Count = 5,
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
