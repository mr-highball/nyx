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

program nyx_logic_row_routes_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, nyx.text, nyx.types, nyx.data, nyx.model, nyx.codec,
  nyx.controls, nyx.collections, nyx.data.rows, nyx.resources, nyx.state,
  nyx.flow, nyx.flow.types, nyx.flow.routes, nyx.flow.route.model,
  nyx.flow.declarations, nyx.flow.rows.editor, nyx.logic, nyx.logic.editor,
  nyx.codegen, nyx.source, nyx.test.logic.table,
  nyx.studio.projects, nyx.studio.session, nyx.studio.agents
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AMessage: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

procedure Run;
var
  LDoc: TNyxDocument;
  LParsed: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LRoute: TNyxFlowRoute;
  LLegacy: TNyxFlowRoute;
  LRouteSet: INyxFlowRoutes;
  LSource: TNyxText;
  LPair: TNyxProjectPair;
  LRoot: INyxColumn;
  LForm: INyxCard;
  LLogic: INyxCard;
  LDraft: TNyxLogicEditorDraft;
  LRestored: TNyxLogicEditorDraft;
  LRowsDraft: TNyxFlowRowsEditorDraft;
  LChange: TNyxFlowRowsEditorChange;
  LRejected: Boolean;
  LAgent: TNyxAgentSession;
  LRevision: Integer;
  LArgs: TNyxDataValue;
  LReply: TNyxDataValue;
  LEdited: TNyxFlowRoute;
  {$ifndef PAS2JS}LFile: TFileStream;{$endif}
begin
  LDoc := LogicTableRoutedDocument;
  LParsed := nil;
  LWorkspace := nil;
  LAgent := nil;
  try
    LRoute := NyxControlFlowRoutes(LDoc.Find('load')).Route(0);
    LLegacy := LRoute.WithoutRows(NyxCollection('people'));
    Check((LLegacy.ToData.Count = 11) and (LLegacy.RowOutputCount = 0),
      'removing final row mapping restores complete legacy shape');
    Check(TNyxFlowRoute.FromData(LLegacy.ToData).ToData.ToJSON = LLegacy.ToData.ToJSON,
      'old scalar route bytes round-trip unchanged');
    Check(TNyxFlowRoute.FromData(LRoute.ToData).ToData.ToJSON = LRoute.ToData.ToJSON,
      'new typed row route bytes round-trip exactly');
    Check(LRoute.RowOutput(0).Rows.ToData.ToJSON = LogicTableRows.ToData.ToJSON,
      'saved row recipe is an independent typed snapshot');
    LRejected := False;
    try
      LEdited := LRoute.OutputRows(LRoute.RowOutput(0).Source,
        NyxCollection('people'), LogicTableRows);
    except
      on E: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'duplicate output declaration refuses');

    LEdited := LRoute.OutputRows(LRoute.RowOutput(0).Source,
      NyxCollection('mirror'), LogicTableRows);
    LEdited := LEdited.WithRows(LRoute.RowOutput(0).Source,
      NyxCollection('people'), LogicTableRows.Numbers(nrnpApproximate));
    Check((LEdited.RowOutput(0).Collection.Name = 'people') and
      (LEdited.RowOutput(1).Collection.Name = 'mirror'), 'explicit row replacement preserves order');
    Check(LRoute.RowOutput(0).Rows.NumberPolicy = nrnpCanonical,
      'fluent replacement preserves original recipe and route');

    LSource := TNyxCodegen.Generate(LDoc);
    LParsed := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LParsed) = TNyxCodec.Encode(LDoc), 'generated row route reconstructs exactly');
    Check(LWorkspace.Render(LParsed) = LSource, 'no-op source is byte-exact');
    Check((Pos('.OutputRows(', LSource) > 0) and (Pos('.DecimalText(', LSource) > 0),
      'crafted source exposes specialized typed paths and projections');
    LParsed.Free;
    LParsed := nil;
    FreeAndNil(LWorkspace);
    LPair := NyxProjectPair(TNyxCodec.Encode(LDoc), LSource);

    LRoot := NewNyxColumn('forms');
    LLogic := NewNyxLogicEditor('editor', LDoc, NyxFlowRef('Team'));
    LForm := NewNyxFlowRowsEditor('editor-rows', LDoc, NyxControl('load'), 0);
    LRoot.Add(LLogic).Add(LForm);
    LForm.Node.Find('editor-rows-scope').Configure.Value('Field').Done;
    LForm.Node.Find('editor-rows-field').Configure.Value('name').Done;
    HandleNyxFlowRowsEditor(LForm.Node.Find('editor-rows-clear-path'), LRoot.Node, LDoc, 0);
    LForm.Node.Find('editor-rows-part').Configure.Value('id').Done;
    HandleNyxFlowRowsEditor(LForm.Node.Find('editor-rows-append'), LRoot.Node, LDoc, 0);
    Check(CaptureNyxFlowRowsEditor(LForm.Node.Find('editor-rows-apply'),
      LRoot.Node, LDoc, 0, LChange), 'typed form captures an existing route output');
    Check(TNyxCodec.Encode(LDoc) = LPair.Design, 'presentation/capture never changes accepted design');

    LForm.Node.Find('editor-rows-segment').Configure.Value('Item').Done;
    LForm.Node.Find('editor-rows-part').Configure.Value('unfinished index').Done;
    LRejected := False;
    try
      HandleNyxFlowRowsEditor(LForm.Node.Find('editor-rows-append'), LRoot.Node, LDoc, 0);
    except
      on E: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LForm.Node.Find('editor-rows-part').Prop('value') = 'unfinished index'),
      'malformed path input remains editable');
    LDraft.Capture('editor', LRoot.Node);
    Check(LDraft.ToData.Field('version').AsInteger = 4, 'nested Table draft uses explicit version four');
    LRestored := TNyxLogicEditorDraft.FromData(LDraft.ToData);
    Check(LRestored.ToData.ToJSON = LDraft.ToData.ToJSON, 'unfinished nested draft bytes round-trip');
    LForm.Node.Find('editor-rows-part').Configure.Value('changed').Done;
    Check(LRestored.Restore('editor', LRoot.Node) and
      (LForm.Node.Find('editor-rows-part').Prop('value') = 'unfinished index'),
      'nested draft restores exact unfinished text');
    LRowsDraft.Capture('editor-rows', LRoot.Node);
    LForm := NewNyxFlowRowsEditor('stale', LDoc, NyxControl('load'), 1);
    Check(not LRowsDraft.Restore('stale', LForm.Node), 'different revision refuses draft restoration');
    LForm := nil;

    LRejected := False;
    try
      CaptureNyxFlowRowsEditor(LRoot.Node.Find('editor-rows-remove'), LRoot.Node, LDoc, 0, LChange);
    except
      on E: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'removal requires explicit confirmation');
    LRoot.Node.Find('editor-rows-confirm').Configure.Value(True).Done;
    Check(CaptureNyxFlowRowsEditor(LRoot.Node.Find('editor-rows-remove'),
      LRoot.Node, LDoc, 0, LChange), 'confirmed removal captures one proposal');
    Check(NyxFlowRoutesFromData(LChange.RoutesData).Route(0).RowOutputCount = 0,
      'removal retains scalar inputs/outputs and route identity');
    LRoot.Node.Find('editor-rows-confirm').Configure.Value(False).Done;
    CaptureNyxFlowRowsEditor(LRoot.Node.Find('editor-rows-apply'),
      LRoot.Node, LDoc, 0, LChange);

    LAgent := TNyxAgentSession.Create(LPair);
    LAgent.InheritPermission(apEdit);
    LRevision := LAgent.Revision;
    LReply := LAgent.Call('nyx_logic', 'Scooty row qualification', NyxObject([
      NyxField('scope', NyxData('route-row-outputs')), NyxField('id', NyxData('load')),
      NyxField('route', NyxData('Load team')), NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('textLimit', NyxData(32))]));
    Check((LReply.Field('total').AsInteger = 1) and
      (Length(LReply.Field('items').Item(0).Field('text').AsText) <= 32),
      'semantic row mapping query is a bounded revision-pinned text window');
    LArgs := NyxObject([NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('typed-table-output')),
      NyxField('operations', NyxArray([NyxObject([
        NyxField('op', NyxData('flow-routes-set')), NyxField('id', NyxData('load')),
        NyxField('routes', LChange.RoutesData)])]))]);
    LAgent.Call('nyx_transaction', 'Scooty row qualification', LArgs);
    Check(LAgent.Revision = LRevision + 1, 'row mapping is one guarded paired revision');
    LAgent.Call('nyx_transaction', 'Scooty row qualification', LArgs);
    Check(LAgent.Revision = LRevision + 1, 'exact semantic retry adds no history');
    LReply := LAgent.Call('nyx_logic', 'Scooty row qualification', NyxObject([
      NyxField('scope', NyxData('route-row-outputs')), NyxField('id', NyxData('load')),
      NyxField('route', NyxData('Load team')), NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('textLimit', NyxData(1024))]));
    Check(TNyxDataValue.ParseJSON(LReply.Field('items').Item(0).Field('text').AsText)
      .Field('rows').Field('fields').Item(0).Field('path').Item(0).AsText = 'id',
      'semantic query observes the changed typed name path');
    LAgent.Call('nyx_history', 'Scooty row qualification', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData('row-output-undo')), NyxField('direction', NyxData('undo'))]));
    Check(EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) = EncodeNyxProject(LPair),
      'one semantic Undo restores the exact accepted source/design pair');

    {$ifndef PAS2JS}
    if ParamCount = 1 then
    begin
      ForceDirectories(ParamStr(1));
      LFile := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(1)) +
        'nyx.generated.view.pas', fmCreate);
      try
        LFile.WriteBuffer(LSource[1], Length(LSource));
      finally
        LFile.Free;
      end;
    end;
    {$endif}
  finally
    LRoot := nil;
    LForm := nil;
    LLogic := nil;
    LAgent.Free;
    LWorkspace.Free;
    LParsed.Free;
    LDoc.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-row-routes', 'passed');
    document.body.setAttribute('data-row-routes-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' saved row/source/editor/semantic checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-row-routes', 'failed');
      document.body.setAttribute('data-row-routes-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
