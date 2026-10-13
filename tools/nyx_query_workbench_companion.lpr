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

program nyx_query_workbench_companion;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.codec, nyx.model, nyx.types,
  nyx.binding.types, nyx.scheduler,
  nyx.state, nyx.collections, nyx.collections.view.types, nyx.callbacks, nyx.source,
  nyx.studio.projects, nyx.studio.edits, nyx.studio.stateedits,
  nyx.studio.collectionedits, nyx.studio.callbackedits, nyx.studio.transactions,
  nyx.studio.agents,
  nyx.test.mcp.client;

var
  GClient: TNyxMCPTestClient;
  GReview: TNyxText;
  GRevision: Integer;
  GChecks: Integer;
  GSerial: Integer;
  GEvidence: TNyxText;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function Operation: TNyxText;
begin
  Inc(GSerial);
  Result := 'workbench-' + IntToStr(GSerial);
end;

function Call(const ATool: TNyxText; const AFields: array of TNyxDataField;
  AContext: Boolean = True): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LReply: TNyxDataValue;
begin
  SetLength(LFields, Length(AFields) + Ord(AContext));
  for LIndex := 0 to High(AFields) do
  begin
    LFields[LIndex] := AFields[LIndex];
  end;

  if AContext then
  begin
    LFields[High(LFields)] := NyxField('review', NyxData(GReview));
  end;
  LReply := GClient.Tool(ATool, NyxObject(LFields));
  Check(not LReply.Field('isError').AsBoolean,
    ATool + ' refused: ' + Copy(LReply.ToJSON, 1, 800));
  Result := LReply.Field('structuredContent');
end;

procedure Save(const AName, AText: TNyxText);
var
  LFile: TFileStream;
begin
  LFile := TFileStream.Create(GEvidence + AName, fmCreate);
  try

    if AText <> '' then
    begin
      LFile.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LFile.Free;
  end;
end;

function ReadText(const APath: TNyxText): TNyxText;
var
  LFile: TFileStream;
begin
  LFile := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, LFile.Size);

    if LFile.Size <> 0 then
    begin
      LFile.ReadBuffer(Result[1], Length(Result));
    end;
  finally
    LFile.Free;
  end;
end;

function ExportPair: TNyxProjectPair;
var
  LReply: TNyxDataValue;
  LText: TNyxText;
  LOffset: Integer;
begin
  LText := '';
  LOffset := 0;
  repeat
    LReply := Call('nyx_project', [NyxField('mode', NyxData('export')),
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('part', NyxData('project')),
      NyxField('offset', NyxData(LOffset)), NyxField('count', NyxData(4096))]);
    LText := LText + LReply.Field('text').AsText;
    LOffset := LReply.Field('nextOffset').AsInteger;
  until LOffset = LReply.Field('total').AsInteger;
  Result := DecodeNyxProject(LText);
end;

procedure History(const ADirection: TNyxText);
begin
  Call('nyx_history', [NyxField('direction', NyxData(ADirection)),
    NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData(Operation))]);
  GRevision := Call('nyx_session', []).Field('revision').AsInteger;
end;

procedure AwaitCommit(const AReceipt: TNyxDataValue);
var
  LStatus: TNyxDataValue;
  LStarted: QWord;
begin

  Save('callback-receipt.json', AReceipt.ToJSON);

  if not NyxAgentHas(AReceipt, 'job') then
  begin
    GRevision := Call('nyx_session', []).Field('revision').AsInteger;
    Exit;
  end;
  LStarted := GetTickCount64;
  repeat
    LStatus := Call('nyx_build', [NyxField('mode', NyxData('status')),
      NyxField('job', AReceipt.Field('job')), NyxField('severity', NyxData('error')),
      NyxField('limit', NyxData(6))]);

    if not ((LStatus.Field('state').AsText = 'queued') or
      (LStatus.Field('state').AsText = 'running')) then
    begin
      Break;
    end;

    if GetTickCount64 - LStarted > 120000 then
    begin
      raise Exception.Create('Owning callback build remains unfinished; retain its receipt');
    end;
    Sleep(100);
  until False;
  Save('callback-construction.json', LStatus.ToJSON);
  Check((LStatus.Field('state').AsText = 'succeeded') and
    (LStatus.Field('publication').Field('state').AsText = 'committed'),
    'Actual callback constructor admits one complete paired command');
  GRevision := Call('nyx_session', []).Field('revision').AsInteger;
end;

{ Record installed readiness/diagnostics without changing its compiler profile
  or substituting the current-source tree for the observing release. }
procedure CaptureInstalledBuild;
var
  LOutputs: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LStatus: TNyxDataValue;
  LStarted: QWord;
begin
  { Admission is distinct from compilation. Use the installed semantic build
    capability at the exact admitted revision; never override its source/profile
    to conceal a deployment that predates the example's runtime dependencies. }
  LOutputs := Call('nyx_build', [NyxField('mode', NyxData('outputs'))]);
  Save('installed-output-readiness.json', LOutputs.ToJSON);
  LReceipt := GClient.Tool('nyx_build', NyxObject([
    NyxField('review', NyxData(GReview)), NyxField('mode', NyxData('request')),
    NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData(Operation)),
    NyxField('outputID', LOutputs.Field('outputID')),
    NyxField('target', NyxData('browser')), NyxField('scope', NyxData('application'))]));
  Save('installed-build-request.json', LReceipt.ToJSON);

  if LReceipt.Field('isError').AsBoolean then
  begin
    Exit;
  end;
  LReceipt := LReceipt.Field('structuredContent');
  LStarted := GetTickCount64;
  repeat
    LStatus := Call('nyx_build', [NyxField('mode', NyxData('status')),
      NyxField('job', LReceipt.Field('job')), NyxField('severity', NyxData('error')),
      NyxField('limit', NyxData(6))]);

    if not ((LStatus.Field('state').AsText = 'queued') or
      (LStatus.Field('state').AsText = 'running')) then
    begin
      Break;
    end;

    if GetTickCount64 - LStarted > 120000 then
    begin
      raise Exception.Create('Installed compiler remains unfinished; retain its exact receipt');
    end;
    Sleep(100);
  until False;
  Save('installed-build-status.json', LStatus.ToJSON);
end;

{ Import exactly one pair through reserved UTF-8 bytes, bounded scalar chunks,
  review and the owner/revision ticket. The current review is exclusively ours. }
procedure ImportPair(const APair: TNyxProjectPair);
var
  LPacket: TNyxText;
  LReply: TNyxDataValue;
  LImport: TNyxText;
  LTicket: TNyxText;
  LIndex: Integer;
  LStart: Integer;
  LCount: Integer;
  LOffset: Integer;
  LScalar: Integer;
begin
  LPacket := EncodeNyxProject(APair);
  Save('input.nyx', LPacket);
  LReply := Call('nyx_project', [NyxField('mode', NyxData('begin-import')),
    NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData(Operation)),
    NyxField('bytes', NyxData(NyxUTF8ByteCount(LPacket)))]);
  LImport := LReply.Field('projectImport').Field('import').AsText;
  LIndex := 1;
  LOffset := 0;
  while LIndex <= Length(LPacket) do
  begin
    LStart := LIndex;
    LCount := 0;
    while (LIndex <= Length(LPacket)) and (LCount < 4096) do
    begin
      if not NyxNextScalar(LPacket, LIndex, LScalar) then
      begin
        raise Exception.Create('Owned project contains invalid UTF-8 scalars');
      end;
      Inc(LCount);
    end;
    LReply := Call('nyx_project', [NyxField('mode', NyxData('append-import')),
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData(Operation)),
      NyxField('import', NyxData(LImport)), NyxField('offset', NyxData(LOffset)),
      NyxField('text', NyxData(Copy(LPacket, LStart, LIndex - LStart)))]);
    LOffset := LReply.Field('projectImport').Field('nextOffset').AsInteger;
  end;
  LReply := Call('nyx_project', [NyxField('mode', NyxData('review-import')),
    NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData(Operation)),
    NyxField('import', NyxData(LImport)), NyxField('resolution', NyxData('match')),
    NyxField('admission', NyxData('literal'))]);
  LTicket := LReply.Field('projectImport').Field('reviewID').AsText;
  Check(LTicket <> '', 'Exact paired review returns its owning ticket');
  Call('nyx_project', [NyxField('mode', NyxData('apply')),
    NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData(Operation)),
    NyxField('import', NyxData(LImport)), NyxField('reviewID', NyxData(LTicket))]);
  GRevision := Call('nyx_session', []).Field('revision').AsInteger;
end;

procedure Run;
var
  LPrimary: TNyxDataValue;
  LAfter: TNyxDataValue;
  LLayout: INyxDesignPatch;
  LState: INyxStateBindingPatch;
  LData: INyxCollectionPatch;
  LTransaction: INyxProjectTransaction;
  LBefore: TNyxProjectPair;
  LPair: TNyxProjectPair;
  LUndo: TNyxProjectPair;
  LDocument: TNyxDocument;
  LHandler: TNyxHandlerRef;
  LBody: TNyxHandlerSource;
  LPrefix: TNyxText;
  LBuilder: TNyxText;
  LSuffix: TNyxText;
  LTemplate: TNyxText;
  LOld: TNyxText;
  LIndex: Integer;
  LImports: array[0..10] of TNyxText;
begin
  Check(ParamCount = 3, 'Supply config.toml, a NEW evidence directory and exact behavior template');
  GEvidence := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(2)));
  Check(not DirectoryExists(GEvidence), 'Workbench evidence is new');
  ForceDirectories(GEvidence);
  LTemplate := ReadText(ParamStr(3));
  GClient := TNyxMCPTestClient.Create(ParamStr(1), 'Scooty query workbench');
  try
    LPrimary := Call('nyx_session', [], False);
    GReview := Call('nyx_reviews', [NyxField('mode', NyxData('create')),
      NyxField('base', NyxData('empty')), NyxField('label', NyxData('Task query workbench')),
      NyxField('expectedRevision', LPrimary.Field('revision')),
      NyxField('operationId', NyxData(Operation))], False).Field('review').AsText;
    GRevision := Call('nyx_session', []).Field('revision').AsInteger;
    LLayout := ReadNyxDesignPatch(TNyxDataValue.ParseJSON(
      '[{"op":"create","kind":"page","id":"task-workbench","root":"page","properties":{"padding":24,"gap":16}},' +
      '{"op":"create","kind":"heading","id":"workbench-title","parent":"task-workbench","properties":{"text":"A little room for big ideas"}},' +
      '{"op":"create","kind":"label","id":"workbench-description","parent":"task-workbench","properties":{"text":"Edit task points, choose a filter, and calculate a fresh summary."}},' +
      '{"op":"create","kind":"row","id":"workbench-actions","parent":"task-workbench","properties":{"gap":12}},' +
      '{"op":"create","kind":"checkbox","id":"important-filter","parent":"workbench-actions","properties":{"text":"Important only"}},' +
      '{"op":"create","kind":"button","id":"calculate-summary","parent":"workbench-actions","properties":{"text":"Calculate summary"}},' +
      '{"op":"create","kind":"label","id":"workbench-summary","parent":"task-workbench","properties":{"text":"Choose a filter, then calculate the summary."}},' +
      '{"op":"create","kind":"table","id":"workbench-tasks","parent":"task-workbench","properties":{"height":320,"aria-label":"Task planning table"}},' +
      '{"op":"create","kind":"page","id":"about-workbench","root":"page","properties":{"padding":24}},' +
      '{"op":"create","kind":"heading","id":"about-title","parent":"about-workbench","properties":{"text":"Keep making things"}}]'));
    LState := NyxStateBindingPatch([
      NyxCreateDefault(NyxStateValue(NyxTextState('summary'), 'Choose a filter, then calculate the summary.')),
      NyxCreateDefault(NyxStateValue(NyxBooleanState('important-only'), False)),
      NyxCreateDefault(NyxStateValue(NyxIntegerState('query-request'), 0)),
      NyxBindControl(NyxBindingOwner('workbench-summary'), bpText, NyxTextState('summary'), bdFromState),
      NyxBindControl(NyxBindingOwner('important-filter'), bpValue, NyxBooleanState('important-only'), bdTwoWay)]);
    LData := NyxCollectionPatch([
      NyxDefineCollection(NyxCollection('tasks'),
        NyxCollectionSchema.Text(NyxTextField('caption'), '')
          .Integer(NyxIntegerField('points'), 0).Boolean(NyxBooleanField('important'), False), [
        NyxCollectionItem(NyxItem(NyxCollection('tasks'), 'layout')).WithValue(NyxTextField('caption'), 'Sketch the layout')
          .WithValue(NyxIntegerField('points'), 3).WithValue(NyxBooleanField('important'), True),
        NyxCollectionItem(NyxItem(NyxCollection('tasks'), 'palette')).WithValue(NyxTextField('caption'), 'Choose a palette')
          .WithValue(NyxIntegerField('points'), 5),
        NyxCollectionItem(NyxItem(NyxCollection('tasks'), 'components')).WithValue(NyxTextField('caption'), 'Compose reusable pieces')
          .WithValue(NyxIntegerField('points'), 8).WithValue(NyxBooleanField('important'), True),
        NyxCollectionItem(NyxItem(NyxCollection('tasks'), 'copy')).WithValue(NyxTextField('caption'), 'Write helpful copy')
          .WithValue(NyxIntegerField('points'), 2),
        NyxCollectionItem(NyxItem(NyxCollection('tasks'), 'interaction')).WithValue(NyxTextField('caption'), 'Try the interactions')
          .WithValue(NyxIntegerField('points'), 13).WithValue(NyxBooleanField('important'), True),
        NyxCollectionItem(NyxItem(NyxCollection('tasks'), 'polish')).WithValue(NyxTextField('caption'), 'Polish the details')
          .WithValue(NyxIntegerField('points'), 1)]),
      NyxBindCollection(NyxBindingOwner('workbench-tasks'), cpTable,
        NyxCollectionView(NyxCollection('tasks')).Column(NyxTextField('caption'), 'Task')
          .Column(NyxIntegerField('points'), 'Points', cmEditable)
          .Column(NyxBooleanField('important'), 'Important'))]);
    LTransaction := NyxProjectTransaction([NyxDesignStep(LLayout), NyxStateStep(LState), NyxCollectionStep(LData)]);
    Call('nyx_transaction', [NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData(Operation)), NyxField('operations', LTransaction.ToData)]);
    GRevision := Call('nyx_session', []).Field('revision').AsInteger;
    AwaitCommit(Call('nyx_callbacks', [NyxField('mode', NyxData('apply')),
      NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData(Operation)),
      NyxField('changes', CaptureNyxCallbackBatch(NyxCallbackPatch([
        NyxAddCallback('calculate-summary', NyxCallbackEvent(ntClick)),
        NyxSetCallbackPolicy('calculate-summary', NyxCallbackEvent(ntClick), neSequential)])).ToData)]));
    LBefore := ExportPair;
    LPair := LBefore;
    LDocument := TNyxCodec.Decode(LPair.Design);
    try
      LHandler := NyxAuthoredEvents(LDocument.Find('calculate-summary'))[0].Callbacks[0].Handler;
    finally
      LDocument.Free;
    end;
    LBody := ReadNyxHandlerSource(LPair.Source, LHandler);
    LPair.Source := ReplaceNyxHandlerImplementation(LPair.Source, LHandler, LBody.Code,
      #10 + 'begin' + #10 + '  // Capture runtime inputs on UI; the library owns worker/merger execution.' + #10 +
      '  StartTaskSummary(Context);' + #10 + 'end;');
    LOld := LHandler.Name + ' = class(TNyxEventCallback)';

    if Pos(LOld, LPair.Source) <> 0 then
    begin
      LPair.Source := StringReplace(LPair.Source, LOld,
        LHandler.Name + ' = class(TNyxApplicationEventCallback)', []);
    end;
    LImports[0] := 'nyx.application.context';
    { State and collections already belong to the generated interface imports.
      Pascal forbids repeating them in the implementation uses clause. }
    LImports[1] := 'nyx.linq';
    LImports[2] := 'nyx.linq.collections';
    LImports[3] := 'nyx.linq.aggregates';
    LImports[4] := 'nyx.linq.parallel';
    LImports[5] := 'nyx.linq.pipeline';
    LImports[6] := 'nyx.linq.reductions';
    LImports[7] := 'nyx.linq.execution';
    LImports[8] := 'nyx.numbers.integer64';
    LImports[9] := 'nyx.application.queryfeedback';
    LImports[10] := 'SysUtils';
    for LIndex := 0 to High(LImports) do
    begin
      LPair.Source := EditNyxImport(LPair.Source, nisImplementation, niaAdd, NyxPascalUnit(LImports[LIndex]));
    end;
    SplitNyxSourceFrame(LPair.Source, LPrefix, LBuilder, LSuffix);
    { Split retains the opening managed marker in the prefix. Helpers must be
      outside that frame; otherwise the following template comment consumes
      the marker's line and source admission correctly refuses the document. }
    LIndex := Pos(NyxViewsBegin, LPrefix);
    Check(LIndex > 0, 'The exported managed opening marker is present');
    LPair.Source := Copy(LPrefix, 1, LIndex - 1) + LTemplate + TNyxText(#10) +
      Copy(LPrefix, LIndex, MaxInt) + LBuilder + LSuffix;
    Save('prepared-source.pas', LPair.Source);
    ImportPair(LPair);
    Check(EncodeNyxProject(ExportPair) = EncodeNyxProject(LPair), 'Admitted paired source/design is exact');
    History('undo');
    LUndo := ExportPair;
    Check(EncodeNyxProject(LUndo) = EncodeNyxProject(LBefore), 'One paired Undo restores complete prior source/design');
    History('redo');
    Check(EncodeNyxProject(ExportPair) = EncodeNyxProject(LPair), 'One paired Redo restores exact authored behavior');
    Save('query-workbench.nyx', EncodeNyxProject(LPair));
    Save('nyx.generated.view.pas', LPair.Source);
    Save('design.json', LPair.Design);
    CaptureInstalledBuild;
    Call('nyx_reviews', [NyxField('mode', NyxData('discard')),
      NyxField('review', NyxData(GReview)), NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData(Operation))], False);
    GReview := '';
    LAfter := Call('nyx_session', [], False);
    Check((LPrimary.Field('revision').AsInteger = LAfter.Field('revision').AsInteger) and
      (LPrimary.Field('selection').ToJSON = LAfter.Field('selection').ToJSON),
      'Unrelated primary selection/revision stay exact');
  finally
    { This independent review belongs only to this transport. Retire it on
      preparation refusal too; active/protected user projects are never touched. }

    if GReview <> '' then
    begin
      GRevision := Call('nyx_session', []).Field('revision').AsInteger;
      Call('nyx_reviews', [NyxField('mode', NyxData('discard')),
        NyxField('review', NyxData(GReview)), NyxField('expectedRevision', NyxData(GRevision)),
        NyxField('operationId', NyxData(Operation))], False);
      GReview := '';
    end;
    GClient.Close;
    GClient.Free;
  end;
end;

begin
  try
    Run;
    WriteLn('PASS ', GChecks, ' semantic workbench composition/import/history assertions');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
