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

program nyx_mcp_http_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.studio.session, nyx.studio.projects,
  nyx.test.mcp.client;

var
  LClient: TNyxMCPTestClient;
  LSession: TNyxStudioSession;
  LValue: TNyxDataValue;
  LArguments: TNyxDataValue;
  LEditor: TNyxDataValue;
  LToken: TNyxText;
  LBase: TNyxText;
  LRevision: Integer;
  LCount: Integer;
  LIndex: Integer;
  LEditingEvents: Integer;
  LDragEvents: Integer;
  LCaptureEvents: Integer;
  LTrigger: TNyxTrigger;
  LEvent: TNyxDataValue;

procedure Check(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(LCount);
end;

function Transaction(const AID, AOperations: TNyxText; ARevision: Integer): TNyxDataValue;
begin
  Result := NyxObject([NyxField('operationId', NyxData(AID)),
    NyxField('expectedRevision', NyxData(ARevision)),
    NyxField('operations', TNyxDataValue.ParseJSON(AOperations))]);
end;

begin
  LClient := nil;
  LSession := nil;
  try
    LBase := ParamStr(1);
    LSession := TNyxStudioSession.Create;
    LEditor := NyxTestEditorExchange(LBase, '/api/agents/connect', '', NyxObject([
      NyxField('op', NyxData('claim')), NyxField('project', NyxData(EncodeNyxProject(LSession.ProjectSnapshot))),
      NyxField('selection', NyxData(LSession.SelectedID)), NyxField('view', NyxData(LSession.ActiveViewID))]));
    LToken := LEditor.Field('token').AsText;
    LRevision := LEditor.Field('state').Field('session').Field('revision').AsInteger;
    LClient := TNyxMCPTestClient.Create(ParamStr(2));
    Check(True, 'Real MCP initialize and initialized notification');
    LValue := LClient.RPC('tools/list', NyxObject([])).Field('result');
    Check(LValue.Field('tools').Count = 12, 'Focused semantic capabilities advertised');
    Check((LValue.Field('tools').Item(11).Field('name').AsText = 'nyx_callbacks') and
      (LValue.Field('tools').Item(11).Field('inputSchema').Field('properties').Field('changes').Field('maxItems').AsInteger = 32),
      'Semantic callback tool advertises bounded batches');
    Check(LValue.Field('tools').Item(7).Field('inputSchema').Field('required').Count = 3,
      'Transaction input schema publishes required guards');
    LClient.Exchange('GET', NyxObject([]), 405);
    Check(True, 'Optional SSE GET explicitly unsupported');
    LClient.Exchange('POST', NyxObject([NyxField('jsonrpc', NyxData('2.0')),
      NyxField('method', NyxData('notifications/initialized'))]), 403, 'http://untrusted.example');
    Check(True, 'Foreign browser Origin refused');
    LValue := LClient.Tool('nyx_session', NyxObject([]));
    Check(not LValue.Field('isError').AsBoolean, 'Typed bounded session context');
    Check(not (Pos('design', LValue.Field('content').Item(0).Field('text').AsText) > 0), 'Session query does not dump design');
    LValue := LClient.Tool('nyx_node', NyxObject([NyxField('id', NyxData('project-name')),
      NyxField('events', NyxData(True)), NyxField('eventLimit', NyxData(50)),
      NyxField('limit', NyxData(1))]));
    LEditingEvents := 0;
    LDragEvents := 0;
    LCaptureEvents := 0;
    for LIndex := 0 to LValue.Field('structuredContent').Field('events').Count - 1 do
    begin
      LEvent := LValue.Field('structuredContent').Field('events').Item(LIndex);

      if TryNyxTrigger(LEvent.Field('trigger').AsText, LTrigger) then
      begin

        if LTrigger in [ntDragStart, ntDrag, ntDragEnter, ntDragOver,
          ntDragExit, ntDrop, ntDragEnd] then
        begin
          Inc(LDragEvents);
          Check((LEvent.Field('contexts').Count = 2) and
            (LEvent.Field('contexts').Item(1).AsText = 'drag'),
            'Actual MCP publishes protected/readable drag context metadata');
        end
        else if LTrigger in [ntPointerCancel, ntPointerCapture, ntPointerCaptureLost] then
        begin
          Inc(LCaptureEvents);
          Check((LEvent.Field('contexts').Count = 1) and
            (LEvent.Field('contexts').Item(0).AsText = 'pointer'),
            'Actual MCP publishes physical capture/cancellation context metadata');
        end;
      end;

      if (LEvent.Field('trigger').AsText = 'before-edit') or
        (LEvent.Field('trigger').AsText = 'composition-start') or
        (LEvent.Field('trigger').AsText = 'composition-update') or
        (LEvent.Field('trigger').AsText = 'composition-end') or
        (LEvent.Field('trigger').AsText = 'text-selection-change') then
      begin
        Inc(LEditingEvents);
        Check((LEvent.Field('contexts').Count = 1) and
          (LEvent.Field('contexts').Item(0).AsText = 'editing'),
          'Actual MCP publishes a bounded editing context declaration');
      end;
    end;
    Check(LEditingEvents = 5, 'Actual MCP discovers all five editing lifecycle events');
    Check((LDragEvents = 7) and (LCaptureEvents = 3),
      'Bounded MCP event query discovers all ten gesture families');
    LArguments := Transaction('http-craft', '[{"op":"create","kind":"row","id":"mcp-row","parent":"home"},' +
      '{"op":"create","kind":"badge","id":"mcp-badge","parent":"mcp-row","properties":{"text":"Crafted 🌙漢字"}},' +
      '{"op":"create","kind":"button","id":"mcp-button","parent":"mcp-row","properties":{"text":"Act"}}]', LRevision);
    LValue := LClient.Tool('nyx_transaction', LArguments);
    Check(not LValue.Field('isError').AsBoolean, 'HTTP grouped creation admitted');
    Inc(LRevision);
    Check(LValue.Field('structuredContent').Field('revision').AsInteger = LRevision, 'Exactly one published revision');
    LValue := LClient.Tool('nyx_transaction', LArguments);
    Check(LValue.Field('structuredContent').Field('revision').AsInteger = LRevision, 'HTTP retry returns original receipt');
    LValue := LClient.Tool('nyx_outline', NyxObject([NyxField('parent', NyxData('mcp-row')),
      NyxField('offset', NyxData(1)), NyxField('limit', NyxData(1))]));
    Check((LValue.Field('structuredContent').Field('items').Count = 1) and
      (LValue.Field('structuredContent').Field('total').AsInteger = 2), 'Paged immediate-child query');
    LValue := LClient.Tool('nyx_node', NyxObject([NyxField('id', NyxData('mcp-button')),
      NyxField('events', NyxData(True)), NyxField('limit', NyxData(1))]));
    Check(LValue.Field('structuredContent').Field('events').Count > 0, 'Published event capabilities inspectable');
    LValue := LClient.Tool('nyx_node', NyxObject([NyxField('id', NyxData('mcp-button')),
      NyxField('events', NyxData(True)), NyxField('eventOffset', NyxData(1)),
      NyxField('eventLimit', NyxData(1)), NyxField('limit', NyxData(1))]));
    Check((LValue.Field('structuredContent').Field('events').Count = 1) and
      (LValue.Field('structuredContent').Field('totalEvents').AsInteger > 1) and
      (LValue.Field('structuredContent').Field('eventOffset').AsInteger = 1),
      'Real MCP transport exposes bounded event pages');
    Check(LValue.Field('structuredContent').Field('events').Item(0).Field('name').AsText = '',
      'Physical event metadata has no invented semantic identity');
    Check((LValue.Field('structuredContent').Field('totalRegistrations').AsInteger = 0) and
      (LValue.Field('structuredContent').Field('registrationOffset').AsInteger = 0) and
      not LValue.Field('structuredContent').Field('registrationsPartial').AsBoolean,
      'Real MCP callback window is explicit and bounded');
    Check(LClient.RPC('tools/list', NyxObject([])).Field('result').Field('tools')
      .Item(2).Field('inputSchema').Field('properties').Field('routeLimit')
      .Field('maximum').AsInteger = 50, 'MCP schema publishes a bounded semantic route window');
    LValue := LClient.Tool('nyx_transaction', Transaction('http-semantic-alias',
      '[{"op":"update","id":"mcp-button","properties":{"emit":"open"}}]', LRevision));
    Check(not LValue.Field('isError').AsBoolean, 'semantic alias is an ordinary undoable editor operation');
    Inc(LRevision);
    LValue := LClient.Tool('nyx_node', NyxObject([NyxField('id', NyxData('mcp-button')),
      NyxField('events', NyxData(True)), NyxField('eventOffset', NyxData(0)),
      NyxField('eventLimit', NyxData(50)), NyxField('routeLimit', NyxData(1))]));
    Check((LValue.Field('structuredContent').Field('totalRoutes').AsInteger = 1) and
      not LValue.Field('structuredContent').Field('routesPartial').AsBoolean,
      'actual MCP discovers exactly one semantic physical route');
    LValue := LClient.Tool('nyx_node', NyxObject([NyxField('id', NyxData('mcp-button')),
      NyxField('events', NyxData(True)), NyxField('eventLimit', NyxData(50)),
      NyxField('routeOffset', NyxData(1)), NyxField('routeLimit', NyxData(1))]));
    Check((LValue.Field('structuredContent').Field('totalRoutes').AsInteger = 1) and
      LValue.Field('structuredContent').Field('routesPartial').AsBoolean,
      'actual MCP marks an omitted route page as partial');
    LValue := LClient.Tool('nyx_history', NyxObject([NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('http-semantic-undo')), NyxField('direction', NyxData('undo'))]));
    Check(not LValue.Field('isError').AsBoolean, 'semantic alias operation is undone as one paired edit');
    Inc(LRevision);
    LValue := LClient.Tool('nyx_node', NyxObject([NyxField('id', NyxData('mcp-badge')),
      NyxField('keys', NyxArray([NyxData('text')])), NyxField('limit', NyxData(1))]));
    LValue := LValue.Field('structuredContent').Field('properties');
    Check((LValue.Count = 1) and
      (LValue.Item(0).Field('meaning').AsText = 'Presentation') and
      (LValue.Item(0).Field('browser').AsText = 'Available') and
      (LValue.Item(0).Field('native').AsText = 'Available') and
      (LValue.Item(0).Field('help').AsText <> ''),
      'bounded real MCP property query includes meaning and target support');
    LValue := LClient.Tool('nyx_transaction', Transaction('http-stale', '[{"op":"delete","id":"mcp-badge"}]', LRevision - 1));
    Check(LValue.Field('isError').AsBoolean and
      (LValue.Field('structuredContent').Field('currentRevision').AsInteger = LRevision), 'Stale mutation refused with current revision');
    LValue := LClient.Tool('nyx_transaction', Transaction('http-types', '[{"op":"title","value":"must roll back"},' +
      '{"op":"update","id":"mcp-button","properties":{"enabled":"false"}}]', LRevision));
    Check(LValue.Field('isError').AsBoolean, 'Wrong Boolean scalar rejects grouped candidate');
    LEditor := NyxTestEditorExchange(LBase, '/api/agents', LToken, NyxObject([NyxField('op', NyxData('observe')),
      NyxField('after', NyxData(0))]));
    Check(LSession.Document.Title = LEditor.Field('session').Field('title').AsText, 'Rejected group retains exact title');
    Check(Pos('Crafted 🌙漢字', DecodeNyxProject(LEditor.Field('project').AsText).Source) > 0, 'Observer receives readable specialized Pascal');
    Check(LEditor.Field('activity').Count > 0, 'Agent activity is visible to operator');
    LValue := LClient.Tool('nyx_transaction', Transaction('http-move', '[{"op":"move","id":"mcp-button","parent":"home","index":0}]', LRevision));
    Check(not LValue.Field('isError').AsBoolean, 'Reparent admitted with explicit index');
    Inc(LRevision);
    LValue := LClient.Tool('nyx_node', NyxObject([NyxField('id', NyxData('mcp-button'))]));
    Check(LValue.Field('structuredContent').Field('node').Field('parent').AsText = 'home', 'Query follows new ownership');
    LValue := LClient.Tool('nyx_transaction', Transaction('http-delete', '[{"op":"delete","id":"mcp-button"}]', LRevision));
    Check(not LValue.Field('isError').AsBoolean, 'Semantic deletion admitted');
    Inc(LRevision);
    LValue := LClient.Tool('nyx_history', NyxObject([NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('http-undo')), NyxField('direction', NyxData('undo'))]));
    Check(not LValue.Field('isError').AsBoolean, 'Ordinary paired undo over MCP');
    Inc(LRevision);
    LValue := LClient.Tool('nyx_node', NyxObject([NyxField('id', NyxData('mcp-button'))]));
    Check(not LValue.Field('isError').AsBoolean, 'Undo restores deleted control');
    NyxTestEditorExchange(LBase, '/api/agents', LToken, NyxObject([NyxField('op', NyxData('configure')),
      NyxField('permission', NyxData('readOnly'))]));
    LValue := LClient.Tool('nyx_transaction', Transaction('http-denied', '[{"op":"title","value":"denied"}]', LRevision));
    Check(LValue.Field('isError').AsBoolean, 'Operator read-only refuses agent editing');
    LValue := LClient.Tool('nyx_components', NyxObject([NyxField('query', NyxData('reply')),
      NyxField('limit', NyxData(2))]));
    Check(LValue.Field('structuredContent').Field('items').Count > 0, 'Intent descriptions searchable under read-only');
    NyxTestEditorExchange(LBase, '/api/agents', LToken, NyxObject([NyxField('op', NyxData('configure')),
      NyxField('permission', NyxData('disabled'))]));
    LValue := LClient.Tool('nyx_session', NyxObject([]));
    Check(LValue.Field('isError').AsBoolean, 'Disabled access refuses inspection');
    NyxTestEditorExchange(LBase, '/api/agents', LToken, NyxObject([NyxField('op', NyxData('configure')),
      NyxField('permission', NyxData('edit'))]));
    LValue := LClient.Tool('nyx_preview', NyxObject([NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('view', NyxData('home')), NyxField('width', NyxData(390)),
      NyxField('height', NyxData(844)), NyxField('capture', NyxData(True))]));

    if LValue.Field('isError').AsBoolean then
    begin
      raise Exception.Create('Actual Nyx preview captured: ' +
        LValue.Field('content').Item(0).Field('text').AsText);
    end;
    Check(not LValue.Field('isError').AsBoolean, 'Actual Nyx preview captured');
    Check((LValue.Field('content').Count = 3) and
      (LValue.Field('content').Item(2).Field('mimeType').AsText = 'image/png'), 'MCP image content is PNG');
    Check(Pos('iVBOR', LValue.Field('content').Item(2).Field('data').AsText) = 1, 'PNG stream encodes exact binary bytes');
    LClient.Close;
    Check(True, 'Transport termination accepted');
    LClient.Free;
    LClient := nil;
    LSession.Free;
    LSession := nil;
    WriteLn('PASS ', LCount, ' real MCP HTTP checks');
  except
    on LException: Exception do
    begin
      LClient.Free;
      LSession.Free;
      WriteLn('FAIL ', LException.Message);
      Halt(1);
    end;
  end;
end.
