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



program nyx_semantic_query_preview_transport;
{$mode delphi}{$H+}{$codepage utf8}
uses
  SysUtils, nyx.text, nyx.data, nyx.collections, nyx.collections.query,
  nyx.studio.mcp, nyx.studio.directories, nyx.studio.outputs,
  nyx.studio.projects, nyx.studio.agents, nyx.test.query.preview;
var
  LEngine: TNyxStudioMCP;
  LProfile: TNyxOutputConfiguration;
  LSeed: TNyxAgentSession;
  LPair: TNyxProjectPair;
  LClaim: TNyxDataValue;
  LToken: TNyxText;
  LBefore: TNyxText;
  LRevision: Integer;
  LTools: TNyxDataValue;
  LModes: TNyxDataValue;
  LProperties: TNyxDataValue;
  LReply: TNyxDataValue;
  LQuery: TNyxCollectionQuery;
  LIndex: Integer;
  LFound: Boolean;
  LChecks: Integer;
  LSections: Integer;
  LRejected: Boolean;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Query preview transport: ' + AReason);
  end;
  Inc(LChecks);
end;

function Observe: TNyxText;
begin
  Result := LEngine.EditorExchange(LToken, NyxObject([
    NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))])).Field('project').AsText;
end;

function Tool(const AArguments: TNyxDataValue): TNyxDataValue;
begin
  Result := LEngine.InvokeTool('nyx_collections', 'owned-preview-connection', 'Scooty', AArguments);
  Check(Observe = LBefore, 'ordinary durable wrapper preserves accepted pair and draft');
end;

begin
  LEngine := nil;
  LProfile := nil;
  LSeed := nil;
  try
    LTools := NyxStudioMCPTools.Field('tools');
    LFound := False;
    for LIndex := 0 to LTools.Count - 1 do
    begin

      if LTools.Item(LIndex).Field('name').AsText = 'nyx_collections' then
      begin
        LModes := LTools.Item(LIndex).Field('inputSchema').Field('oneOf');
        LFound := True;
        Break;
      end;
    end;
    Check(LFound and (LModes.Count = 13), 'three strict preview sections extend existing ten modes');
    LSections := 0;
    for LIndex := 0 to LModes.Count - 1 do
    begin
      LProperties := LModes.Item(LIndex).Field('properties');

      if LProperties.Field('mode').Field('const').AsText = 'preview' then
      begin
        Inc(LSections);
        Check(not LModes.Item(LIndex).Field('additionalProperties').AsBoolean and
          NyxAgentHas(LProperties, 'workspace') and NyxAgentHas(LProperties, 'review'),
          'each closed preview variant keeps outer project routing');
        Check(LModes.Item(LIndex).Field('not').Field('required').Count = 2,
          'project and review are mutually exclusive');
        Check(LProperties.Field('expectedRevision').Field('minimum').AsInteger = 1,
          'every preview section requires a revision');
        Check(LProperties.Field('query').Field('anyOf').Item(1).Field('$ref').AsText =
          '#/$defs/nyxQuery', 'strict typed query definition is shared with mutation');

        if LProperties.Field('section').Field('const').AsText = 'rows' then
        begin
          Check((LProperties.Field('limit').Field('maximum').AsInteger = 20) and
            (LProperties.Field('fields').Field('maxItems').AsInteger = 16), 'row/field budgets');
        end;

        if LProperties.Field('section').Field('const').AsText = 'pascal' then
        begin
          Check(LProperties.Field('count').Field('maximum').AsInteger = 4096,
            'Pascal scalar window budget');
        end;
      end;
    end;
    Check(LSections = 3, 'all three preview sections are actually advertised');
    Check(ParamCount = 1, 'supply a fresh owned suspended runtime');
    Check(not DirectoryExists(ParamStr(1)), 'existing runtime cannot be overwritten');
    LPair := CreateNyxQueryPreviewSeed;
    LPair.Pending := True;
    LPair.DraftBase := LPair.Source;
    LPair.Draft := LPair.Source + TNyxText(#10 + '// Unfinished 🌙');
    LSeed := TNyxAgentSession.Create(LPair);
    LProfile := TNyxOutputConfiguration.Create;
    { Constructor is suspended: no listener/process or user enrollment is started.
      The real tool wrapper owns routing, activity, locking and durable recovery. }
    LEngine := TNyxStudioMCP.Create(TNyxStudioDirectories.ForRepository(ParamStr(1)),
      8648, 8649, LProfile.Encode);
    LClaim := LEngine.ConnectEditor(NyxObject([NyxField('op', NyxData('claim')),
      NyxField('project', LSeed.Exchange(NyxObject([NyxField('op', NyxData('observe')),
        NyxField('after', NyxData(0))])).Field('project')),
      NyxField('selection', NyxData('tasks-table')), NyxField('view', NyxData('home'))]));
    LToken := LClaim.Field('token').AsText;
    LBefore := Observe;
    LRevision := LEngine.InvokeTool('nyx_session', 'owned-preview-connection', 'Scooty',
      NyxObject([])).Field('revision').AsInteger;
    LQuery := NyxCollectionQuery.Where(NyxWhere(NyxIntegerField('priority')).AtLeast(27));
    LReply := Tool(NyxTestQueryPreviewArguments(LRevision, 'summary', LQuery));
    Check(LReply.Field('visibleCount').AsInteger = 3, 'real wrapper returns unapplied counts');
    LReply := Tool(NyxTestQueryPreviewArguments(LRevision, 'rows', LQuery)
      .WithField('limit', NyxData(1)).WithField('fields', NyxArray([NyxData('priority')])));
    Check((LReply.Field('rows').Count = 1) and
      (LReply.Field('rows').Item(0).Field('values').Item(0).Field('value')
        .Field('value').AsInteger = 27), 'wrapper returns only requested field page');
    LReply := Tool(NyxTestQueryPreviewArguments(LRevision, 'pascal', LQuery)
      .WithField('count', NyxData(16)));
    Check((LReply.Field('pascal').Field('text').AsText = 'NyxCollectionQue') and
      (LReply.Field('pascal').Field('nextOffset').AsInteger = 16), 'wrapper retains text window');
    LRejected := False;
    try
      Tool(NyxTestQueryPreviewArguments(LRevision - 1, 'summary', LQuery));
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (Observe = LBefore), 'wrapper stale refusal retains complete pair');
    FreeAndNil(LEngine);
    LEngine := TNyxStudioMCP.Create(TNyxStudioDirectories.ForRepository(ParamStr(1)),
      8648, 8649, LProfile.Encode);
    LClaim := LEngine.ConnectEditor(NyxObject([NyxField('op', NyxData('claim')),
      NyxField('project', NyxData(LBefore)), NyxField('selection', NyxData('tasks-table')),
      NyxField('view', NyxData('home'))]));
    LToken := LClaim.Field('token').AsText;
    Check(Observe = LBefore, 'owned durable recovery retains pending pair after previews');
    WriteLn('PASS ', LChecks, ' real query preview discovery/durable-wrapper checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
  LEngine.Free;
  LProfile.Free;
  LSeed.Free;
end.
