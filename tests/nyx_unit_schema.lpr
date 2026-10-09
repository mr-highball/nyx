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

program nyx_unit_schema;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.studio.mcp, nyx.studio.agents,
  nyx.studio.projects, nyx.studio.directories, nyx.studio.outputs;

var
  LTools: TNyxDataValue;
  LModes: TNyxDataValue;
  LMode: TNyxDataValue;
  LChange: TNyxDataValue;
  LIndex: Integer;
  LChecks: Integer;
  LFound: Boolean;
  LEngine: TNyxStudioMCP;
  LSeed: TNyxAgentSession;
  LProfile: TNyxOutputConfiguration;
  LClaim: TNyxDataValue;
  LToken: TNyxText;
  LArguments: TNyxDataValue;
  LResult: TNyxDataValue;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LRevision: Integer;
  LPair: TNyxProjectPair;
  LRuntime: TNyxText;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Pascal unit transport: ' + AReason);
  end;
  Inc(LChecks);
end;

function Tool(const AName: TNyxText; const AArguments: TNyxDataValue): TNyxDataValue;
begin
  Result := LEngine.InvokeTool(AName, 'owned-source-connection', 'Scooty source workshop', AArguments);
end;

function Observe: TNyxText;
begin
  Result := LEngine.EditorExchange(LToken, NyxObject([
    NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))])).Field('project').AsText;
end;

begin
  try
    LTools := NyxStudioMCPTools.Field('tools');
    Check(LTools.Count = 22, 'General source modes retain focused tool inventory');
    LFound := False;
    for LIndex := 0 to LTools.Count - 1 do
    begin

      if LTools.Item(LIndex).Field('name').AsText = 'nyx_pascal' then
      begin
        LModes := LTools.Item(LIndex).Field('inputSchema').Field('oneOf');
        LFound := True;
        Break;
      end;
    end;
    Check(LFound and (LModes.Count = 13), 'Two source modes append to existing discovery');
    LMode := LModes.Item(11);
    Check(LMode.Field('properties').Field('mode').Field('const').AsText = 'unit',
      'Bounded unit context is advertised');
    Check(LMode.Field('properties').Field('count').Field('maximum').AsInteger = 4096,
      'Read windows have a scalar limit');
    Check(LMode.Field('properties').Field('expectedRevision').Field('minimum').AsInteger = 1,
      'Reads can pin accepted revision');
    Check(LMode.Field('properties').Field('line').Field('minimum').AsInteger = 1,
      'Compiler/routine source lines map to exact scalar windows');
    Check(LMode.Field('allOf').Item(0).Field('not').Field('required').Count = 2,
      'Line and scalar-offset queries are exclusive');
    LMode := LModes.Item(12);
    Check(LMode.Field('properties').Field('mode').Field('const').AsText = 'edit-unit',
      'One group authoring mode is advertised');
    Check(LMode.Field('required').Count = 4, 'Mutation requires mode, revision, operation and group');
    LChange := LMode.Field('properties').Field('changes');
    Check((LChange.Field('minItems').AsInteger = 1) and
      (LChange.Field('maxItems').AsInteger = 16), 'Mutation group budget is explicit');
    LChange := LChange.Field('items');
    Check((LChange.Field('required').Count = 3) and
      not LChange.Field('additionalProperties').AsBoolean, 'Strict original offset and exact text changes');
    Check(LChange.Field('properties').Field('replacement').Field('maxLength').AsInteger = 262144,
      'Replacement scalar budget is advertised');
    for LIndex := 11 to 12 do
    begin
      LMode := LModes.Item(LIndex);
      Check(not LMode.Field('additionalProperties').AsBoolean, 'Source context is strict');
      Check(NyxAgentHas(LMode.Field('properties'), 'workspace') and
        NyxAgentHas(LMode.Field('properties'), 'review'), 'Existing outer project/review routing applies');
      Check(LMode.Field('not').Field('required').Count = 2, 'Mixed project contexts refuse');
    end;

    { This suspended engine uses a NEW owned test repository. Constructor writes
      configuration only beneath that root; no user enrollment or listener is
      started. The real wrapper still performs paired durable save/rollback. }
    Check(ParamCount = 1, 'Transport qualification needs a fresh owned runtime');
    LRuntime := ParamStr(1);
    Check(not DirectoryExists(LRuntime), 'Existing runtime must not be overwritten');
    LEngine := nil;
    LSeed := TNyxAgentSession.Create;
    LProfile := TNyxOutputConfiguration.Create;
    try
      LEngine := TNyxStudioMCP.Create(TNyxStudioDirectories.ForRepository(LRuntime),
        8648, 8649, LProfile.Encode);
      LClaim := LEngine.ConnectEditor(NyxObject([NyxField('op', NyxData('claim')),
        NyxField('project', LSeed.Exchange(NyxObject([NyxField('op', NyxData('observe')),
          NyxField('after', NyxData(0))])).Field('project')),
        NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
      LToken := LClaim.Field('token').AsText;
      LBefore := Observe;
      LRevision := Tool('nyx_session', NyxObject([])).Field('revision').AsInteger;
      LArguments := NyxObject([NyxField('mode', NyxData('edit-unit')),
        NyxField('expectedRevision', NyxData(LRevision)), NyxField('operationId', NyxData('durable-source')),
        NyxField('changes', NyxArray([NyxObject([NyxField('offset', NyxData(0)),
          NyxField('expected', NyxData('')),
          NyxField('replacement', NyxData('// Durable handwritten note 🌿' + #10))])]))]);
      LResult := Tool('nyx_pascal', LArguments);
      LAfter := Observe;
      LPair := DecodeNyxProject(LAfter);
      Check(Pos('// Durable handwritten note 🌿' + #10, LPair.Source) = 1,
        'Actual wrapper admits exact source proposal');
      Check(LResult.Field('sourceEdit').Field('changes').AsInteger = 1, 'Wrapper returns small receipt');
      LResult := Tool('nyx_pascal', LArguments);
      Check(Observe = LAfter, 'Actual wrapper retry does not replay');
      FreeAndNil(LEngine);

      { Reload the owned checkpoint before connecting any editor. A source edit
        must be durable, and its ordinary paired history must survive recovery. }
      LEngine := TNyxStudioMCP.Create(TNyxStudioDirectories.ForRepository(LRuntime),
        8648, 8649, LProfile.Encode);
      LResult := Tool('nyx_session', NyxObject([]));
      Check(LResult.Field('canUndo').AsBoolean, 'Source mutation persists paired history');
      LRevision := LResult.Field('revision').AsInteger;
      LResult := Tool('nyx_pascal', NyxObject([NyxField('mode', NyxData('unit')),
        NyxField('count', NyxData(36))]));
      Check(Pos('// Durable handwritten note 🌿' + #10, LResult.Field('text').AsText) = 1,
        'Recovered accepted source retains exact supplementary text');
      LResult := Tool('nyx_history', NyxObject([NyxField('expectedRevision', NyxData(LRevision)),
        NyxField('operationId', NyxData('recovered-source-undo')), NyxField('direction', NyxData('undo'))]));
      Check(not LResult.Field('canUndo').AsBoolean and LResult.Field('canRedo').AsBoolean,
        'One recovered Undo restores the prior history boundary');
      LResult := Tool('nyx_pascal', NyxObject([NyxField('mode', NyxData('unit')),
        NyxField('count', NyxData(36))]));
      Check(LResult.Field('text').AsText = Copy(DecodeNyxProject(LBefore).Source, 1, 36),
        'Recovered paired Undo restores the original accepted source window');
    finally
      LEngine.Free;
      LProfile.Free;
      LSeed.Free;
    end;
    WriteLn('PASS ', LChecks, ' source discovery and durable wrapper checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
