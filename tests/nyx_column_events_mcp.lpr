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
program nyx_column_events_mcp;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.state,
  nyx.binding.types, nyx.collections, nyx.collections.columns,
  nyx.collections.view.types, nyx.collections.arrangement,
  nyx.studio.stateedits, nyx.studio.collectionedits, nyx.test.mcp.client;

const
  CField: TNyxText = 'service 🌙';

var
  GClient: TNyxMCPTestClient;
  GDirectory: String;
  GWorkspace: TNyxText;
  GRevision: Integer;
  GChecks: Integer;
  GSerial: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Column MCP: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure Save(const AName: String; const AText: TNyxText);
var
  LFile: TFileStream;
  LBytes: TNyxBytes;
begin
  LBytes := NyxEncodeUTF8(AText);
  LFile := TFileStream.Create(GDirectory + AName, fmCreate);
  try

    if Length(LBytes) > 0 then
    begin
      LFile.WriteBuffer(LBytes[0], Length(LBytes));
    end;
  finally
    LFile.Free;
  end;
end;

function Scoped(const AArguments: TNyxDataValue): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin
  SetLength(LFields, AArguments.Count + 1);
  for LIndex := 0 to AArguments.Count - 1 do
  begin
    LFields[LIndex] := NyxField(AArguments.Key(LIndex), AArguments.Field(AArguments.Key(LIndex)));
  end;
  LFields[High(LFields)] := NyxField('workspace', NyxData(GWorkspace));
  Result := NyxObject(LFields);
end;

function Call(const ATool: TNyxText; const AArguments: TNyxDataValue;
  AScoped: Boolean = True): TNyxDataValue;
var
  LArguments: TNyxDataValue;
  LReply: TNyxDataValue;
  LTrace: String;
  LIndex: Integer;
begin
  LArguments := AArguments;

  if AScoped then
  begin

    if GWorkspace = '' then
    begin
      raise Exception.Create('An owned named project is required before editing');
    end;
    LArguments := Scoped(LArguments);
  end;
  Inc(GSerial);
  LTrace := IntToStr(GSerial);

  if (ATool = 'nyx_build') and (AArguments.Field('mode').AsText = 'status') then
  begin
    { Queue/running polling does not manufacture new assertions or thousands of
      repeated files. Each build separately retains its initial/final receipt. }
    LTrace := 'current-status';
  end;
  Save(LTrace + '-request.json', NyxObject([
    NyxField('tool', NyxData(ATool)), NyxField('arguments', LArguments)]).ToJSON);
  LReply := GClient.Tool(ATool, LArguments);
  Save(LTrace + '-reply.json', LReply.ToJSON);

  if LReply.Field('isError').AsBoolean then
  begin
    raise Exception.Create('Semantic request refused: ' + ATool + ' / ' + LReply.ToJSON);
  end;
  Result := LReply.Field('structuredContent');

  if AScoped then
  begin
    { Configuration/capability reads intentionally have no document revision.
      Field is a strict read, so inspect presence before accessing the member. }
    for LIndex := 0 to Result.Count - 1 do
    begin

      if Result.Key(LIndex) = 'revision' then
      begin
        GRevision := Result.Field('revision').AsInteger;
        Break;
      end;
    end;
  end;
end;

function Mutation(const AIdentity: TNyxText; const AFields: array of TNyxDataField): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin
  SetLength(LFields, Length(AFields) + 2);
  LFields[0] := NyxField('expectedRevision', NyxData(GRevision));
  LFields[1] := NyxField('operationId', NyxData(AIdentity));
  for LIndex := 0 to High(AFields) do
  begin
    LFields[LIndex + 2] := AFields[LIndex];
  end;
  Result := NyxObject(LFields);
end;

{ File acknowledgments coordinate the external documented browser tool with this
  retained semantic transport. They confer no editor authority and contain no
  credentials. The operator inspects the real UI before acknowledging a phase. }
procedure Phase(const APhase, AContinuation: String);
var
  LStarted: QWord;
begin
  Save('phase.json', NyxObject([
    NyxField('phase', NyxData(TNyxText(APhase))),
    NyxField('workspace', NyxData(GWorkspace)), NyxField('revision', NyxData(GRevision)),
    NyxField('checks', NyxData(GChecks))]).ToJSON);
  WriteLn('Column MCP phase: ', APhase);
  Flush(Output);

  if AContinuation = '' then
  begin
    Exit;
  end;
  LStarted := GetTickCount64;
  while not FileExists(GDirectory + AContinuation + '.continue') do
  begin

    if GetTickCount64 - LStarted > 600000 then
    begin
      raise Exception.Create('Browser acknowledgment timed out: ' + AContinuation);
    end;
    Sleep(100);
  end;
end;

function Source: TNyxText;
var
  LLine: Integer;
  LIndex: Integer;
  LRevision: Integer;
  LWindow: TNyxDataValue;
  LLines: TNyxStrings;
begin
  LLine := 1;
  LRevision := GRevision;
  LLines := TNyxStrings.Create;
  try
    repeat
      LWindow := Call('nyx_source', NyxObject([
        NyxField('line', NyxData(LLine)), NyxField('count', NyxData(80))]));
      Check(GRevision = LRevision, 'source windows belong to the same accepted revision');
      for LIndex := 0 to LWindow.Field('lines').Count - 1 do
      begin
        LLines.Add(LWindow.Field('lines').Item(LIndex).AsText);
      end;
      Check((LWindow.Field('lines').Count > 0) and (LLine < 2000),
        'bounded export makes progress through this small accepted application');
      Inc(LLine, LWindow.Field('lines').Count);
    until LLine > LWindow.Field('totalLines').AsInteger;
    Result := LLines.Text;
  finally
    LLines.Free;
  end;
end;

function NodeEvents: TNyxDataValue;
begin
  Result := Call('nyx_node', NyxObject([
    NyxField('id', NyxData('service-table')), NyxField('keys', NyxArray([NyxData('height')])),
    NyxField('events', NyxData(True)), NyxField('eventLimit', NyxData(50))]));
end;

function Registrations(const ANode: TNyxDataValue): TNyxDataValue;
var
  LIndex: Integer;
  LEvent: TNyxDataValue;
  LEvents: TNyxDataValue;
begin
  Result := NyxArray([]);
  LEvents := ANode.Field('registrations').Field('events');
  for LIndex := 0 to LEvents.Count - 1 do
  begin
    LEvent := LEvents.Item(LIndex);

    if LEvent.Field('trigger').AsText = 'columns-changed' then
    begin
      Exit(LEvent.Field('callbacks'));
    end;
  end;
end;

procedure Compose;
var
  LSpec: TNyxCollectionViewSpec;
  LCollection: TNyxDataValue;
  LState: TNyxDataValue;
  LDesign: TNyxDataValue;
  LOperations: array of TNyxDataValue;
  LIndex: Integer;
begin
  LDesign := TNyxDataValue.ParseJSON(
    '[{"op":"title","value":"Arrange your service monitor"},' +
    '{"op":"create","kind":"page","id":"monitor","root":"page","properties":{"padding":16,"gap":12}},' +
    '{"op":"create","kind":"heading","id":"monitor-heading","parent":"monitor","properties":{"text":"Your service monitor"}},' +
    '{"op":"create","kind":"label","id":"column-status-label","parent":"monitor"},' +
    '{"op":"create","kind":"table","id":"service-table","parent":"monitor","properties":{"height":200}},' +
    '{"op":"create","kind":"label","id":"peer-guide","parent":"monitor","properties":{"text":"Independent peer arrangement"}},' +
    '{"op":"create","kind":"table","id":"peer-table","parent":"monitor","properties":{"height":160}}]');
  LSpec := NyxCollectionView(NyxCollection('services'))
    .Column(NyxTextField(CField), 'Service')
      .ColumnSizing(NyxColumnSizing.Fixed(180).Minimum(50).Maximum(600))
    .Column(NyxIntegerField('pid'), 'Process ID')
      .ColumnSizing(NyxColumnSizing.Fixed(100).Minimum(50).Maximum(400))
    .TableLayout(NyxTableLayout);
  LCollection := NyxCollectionPatch([
    NyxDefineCollection(NyxCollection('services'),
      NyxCollectionSchema.Text(NyxTextField(CField), '').Integer(NyxIntegerField('pid'), 0), [
        NyxCollectionItem(NyxItem(NyxCollection('services'), 'gateway'))
          .WithValue(NyxTextField(CField), 'Gateway').WithValue(NyxIntegerField('pid'), 2401),
        NyxCollectionItem(NyxItem(NyxCollection('services'), 'worker'))
          .WithValue(NyxTextField(CField), 'Background worker').WithValue(NyxIntegerField('pid'), 2402)]),
    NyxBindCollection(NyxBindingOwner('service-table'), cpTable, LSpec),
    NyxBindCollection(NyxBindingOwner('peer-table'), cpTable, LSpec)]).ToData;
  LState := NyxStateBindingPatch([
    NyxCreateDefault(NyxStateValue(NyxTextState('column-status'), 'Arrange a column to see its field.')),
    NyxCreateDefault(NyxStateValue(NyxTextState('column-preferences'), '')),
    NyxCreateDefault(NyxStateValue(NyxIntegerState('column-change-count'), 0)),
    NyxBindControl(NyxBindingOwner('column-status-label'), bpText,
      NyxTextState('column-status'), bdFromState)]).ToData;
  SetLength(LOperations, LDesign.Count + 2);
  for LIndex := 0 to LDesign.Count - 1 do
  begin
    LOperations[LIndex] := LDesign.Item(LIndex);
  end;
  LOperations[LDesign.Count] := NyxObject([
    NyxField('op', NyxData('state')), NyxField('changes', LState)]);
  LOperations[LDesign.Count + 1] := NyxObject([
    NyxField('op', NyxData('collections')), NyxField('changes', LCollection)]);
  Call('nyx_transaction', Mutation('columns-compose', [
    NyxField('operations', NyxArray(LOperations))]));
  Call('nyx_select', Mutation('columns-select-table', [
    NyxField('id', NyxData('service-table'))]));
end;

function InspectHandler(const AHandler: TNyxText): TNyxText;
var
  LReply: TNyxDataValue;
begin
  LReply := Call('nyx_pascal', NyxObject([
    NyxField('mode', NyxData('inspect')), NyxField('handler', NyxData(AHandler)),
    NyxField('offset', NyxData(0)), NyxField('count', NyxData(4096))]));
  Check(LReply.Field('nextOffset').AsInteger = LReply.Field('total').AsInteger,
    'the exact short callback body fits one bounded semantic response');
  Result := LReply.Field('text').AsText;
end;

procedure Compile(const ATarget, AOutput: TNyxText; AWaitMilliseconds: QWord);
var
  LReceipt: TNyxDataValue;
  LStatus: TNyxDataValue;
  LStarted: QWord;
  LState: TNyxText;
begin
  LReceipt := Call('nyx_build', Mutation('columns-build-' + ATarget, [
    NyxField('mode', NyxData('request')), NyxField('target', NyxData(ATarget)),
    NyxField('scope', NyxData('application')), NyxField('outputID', NyxData(AOutput))]));
  Save(String(ATarget) + '-receipt.json', LReceipt.ToJSON);
  LStarted := GetTickCount64;
  repeat
    LStatus := Call('nyx_build', NyxObject([
      NyxField('mode', NyxData('status')), NyxField('job', LReceipt.Field('job')),
      NyxField('severity', NyxData('error')), NyxField('limit', NyxData(5))]));
    LState := LStatus.Field('state').AsText;
    Save(String(ATarget) + '-status.json', LStatus.ToJSON);

    if (LState = 'succeeded') or (LState = 'failed') or (LState = 'cancelled') then
    begin
      Break;
    end;

    if GetTickCount64 - LStarted > AWaitMilliseconds then
    begin
      raise Exception.Create('Compilation is still running; inspect its retained job');
    end;
    Sleep(250);
  until False;
  Check((LState = 'succeeded') and LStatus.Field('currentSource').AsBoolean,
    'delegated ' + ATarget + ' application compiler accepts this exact authored pair');
end;

procedure Run;
var
  LPrimary: TNyxDataValue;
  LReply: TNyxDataValue;
  LNode: TNyxDataValue;
  LRegistrations: TNyxDataValue;
  LFirst: TNyxText;
  LSecond: TNyxText;
  LFirstOld: TNyxText;
  LSecondOld: TNyxText;
  LFirstCode: TNyxText;
  LSecondCode: TNyxText;
  LBeforeSource: TNyxText;
  LAfterSource: TNyxText;
  LRevision: Integer;
  LOutput: TNyxText;
begin
  LPrimary := Call('nyx_session', NyxObject([]), False);
  Save('primary-before.json', LPrimary.ToJSON);
  LReply := Call('nyx_workspaces', NyxObject([
    NyxField('mode', NyxData('create')), NyxField('base', NyxData('empty')),
    NyxField('expectedRevision', LPrimary.Field('revision')),
    NyxField('operationId', NyxData('columns-new-workspace')),
    NyxField('label', NyxData('Column event workshop'))]), False);
  GWorkspace := LReply.Field('workspace').AsText;
  LReply := Call('nyx_session', NyxObject([]));
  Check(GRevision = 1, 'an independent ordinary project starts at revision one');
  Phase('workspace-created', 'observer-ready');
  Compose;
  LNode := NodeEvents;
  Save('composed-node.json', LNode.ToJSON);
  Check((Pos('columns-changed', LNode.ToJSON) > 0) and
    (Pos('collection-columns', LNode.ToJSON) > 0),
    'bounded live metadata exposes the real typed column event and payload context');
  Phase('composed', 'ui-added');
  Call('nyx_session', NyxObject([]));
  LNode := NodeEvents;
  LRegistrations := Registrations(LNode);
  Check(LRegistrations.Count = 1, 'ordinary live Studio Add authored one callback');
  LFirst := LRegistrations.Item(0).Field('handler').AsText;
  LBeforeSource := Source;
  Check(Pos('// TODO: implement ' + LFirst + '.', LBeforeSource) > 0,
    'ordinary Add creates the exact navigable TODO in accepted source');
  LRevision := GRevision;
  LReply := Call('nyx_callbacks', Mutation('columns-add-and-policy', [
    NyxField('mode', NyxData('apply')),
    NyxField('changes', TNyxDataValue.ParseJSON(
      '[{"op":"add","id":"service-table","event":{"trigger":"columns-changed"}},' +
      '{"op":"policy","id":"service-table","event":{"trigger":"columns-changed"},"policy":"ui-queue"}]'))]));
  Check(GRevision = LRevision + 1, 'related semantic callback changes publish once');
  LAfterSource := Source;
  LRegistrations := Registrations(NodeEvents);
  Check(LRegistrations.Count = 2, 'live semantic metadata retains both ordered registrations');
  LSecond := LRegistrations.Item(1).Field('handler').AsText;
  Check((LFirst <> LSecond) and (Pos('.On(ntColumnsChanged)', LAfterSource) > 0),
    'crafted source retains specialized control names and the closed typed registration');
  Call('nyx_history', Mutation('columns-undo', [NyxField('direction', NyxData('undo'))]));
  Check((Source = LBeforeSource) and (Registrations(NodeEvents).Count = 1),
    'one Undo restores the exact source and callback metadata before the group');
  Call('nyx_history', Mutation('columns-redo', [NyxField('direction', NyxData('redo'))]));
  Check((Source = LAfterSource) and (Registrations(NodeEvents).Count = 2),
    'Redo restores the exact paired source/metadata group');
  LReply := GClient.Tool('nyx_callbacks', Scoped(NyxObject([
    NyxField('mode', NyxData('apply')), NyxField('expectedRevision', NyxData(LRevision)),
    NyxField('operationId', NyxData('columns-stale')),
    NyxField('changes', TNyxDataValue.ParseJSON(
      '[{"op":"policy","id":"service-table","event":{"trigger":"columns-changed"},"policy":"sequential"}]'))])));
  Save('stale-refusal.json', LReply.ToJSON);
  Check(LReply.Field('isError').AsBoolean, 'stale callback mutation refuses');
  Check(Source = LAfterSource, 'refusal retains the exact accepted source');
  LFirstOld := InspectHandler(LFirst);
  LSecondOld := InspectHandler(LSecond);
  LFirstCode := #10 + 'begin' + #10 + #10 +
    '  if AExecution.Cancelled or not AEvent.CollectionColumns.Defined then' + #10 +
    '  begin' + #10 + '    Exit;' + #10 + '  end;' + #10 +
    '  { Report the accepted typed field through ordinary live application state. }' + #10 +
    '  Context.State.SetValue(NyxTextState(''column-status''),' + #10 +
    '    AEvent.CollectionColumns.ChangeAt(0).Field.Name);' + #10 +
    '  Context.State.SetValue(NyxIntegerState(''column-change-count''),' + #10 +
    '    Context.State.GetValue(NyxIntegerState(''column-change-count'')) + 1);' + #10 + 'end;';
  LSecondCode := #10 + 'begin' + #10 + #10 +
    '  if AExecution.Cancelled or not AEvent.CollectionColumns.Defined then' + #10 +
    '  begin' + #10 + '    Exit;' + #10 + '  end;' + #10 +
    '  { Persist detached preferences, independently of control/widget lifetime. }' + #10 +
    '  Context.State.SetValue(NyxTextState(''column-preferences''),' + #10 +
    '    AEvent.CollectionColumns.After.ToData.ToJSON);' + #10 + 'end;';
  Call('nyx_pascal', Mutation('columns-implement', [
    NyxField('mode', NyxData('apply')), NyxField('changes', NyxArray([
      NyxObject([NyxField('handler', NyxData(LFirst)), NyxField('expected', NyxData(LFirstOld)),
        NyxField('implementation', NyxData(LFirstCode))]),
      NyxObject([NyxField('handler', NyxData(LSecond)), NyxField('expected', NyxData(LSecondOld)),
        NyxField('implementation', NyxData(LSecondCode))])]))]));
  Check((InspectHandler(LFirst) = LFirstCode) and (InspectHandler(LSecond) = LSecondCode),
    'one guarded semantic operation admits both handwritten implementations exactly');
  Save('nyx.generated.view.pas', Source);
  Phase('authored', '');
  LReply := Call('nyx_build', NyxObject([NyxField('mode', NyxData('outputs'))]));
  Save('outputs.json', LReply.ToJSON);
  LOutput := LReply.Field('outputID').AsText;
  Compile('browser', LOutput,
    QWord(LReply.Field('compilerLimits').Field('compilationMilliseconds').AsInteger) + 10000);
  Compile('lcl', LOutput,
    QWord(LReply.Field('compilerLimits').Field('compilationMilliseconds').AsInteger) + 10000);
  Check(Call('nyx_session', NyxObject([]), False).Field('revision').AsInteger =
    LPrimary.Field('revision').AsInteger, 'primary project revision remains untouched');
  Phase('compiled', 'finished');
  Save('result.json', NyxObject([NyxField('checks', NyxData(GChecks)),
    NyxField('workspace', NyxData(GWorkspace)), NyxField('revision', NyxData(GRevision))]).ToJSON);
  WriteLn('PASS ', GChecks, ' owned live column-event MCP checks');
end;

{ Resume only delegated compilation of an already authored owned project after
  a transport/driver failure. This does not replay or claim the authoring checks;
  original request/reply and UI evidence remain in their separate packet. }
procedure ResumeBuild;
var
  LReply: TNyxDataValue;
  LOutput: TNyxText;
begin
  GWorkspace := TNyxText(ParamStr(3));
  Call('nyx_session', NyxObject([]));
  Save('nyx.generated.view.pas', Source);
  LReply := Call('nyx_build', NyxObject([NyxField('mode', NyxData('outputs'))]));
  Save('outputs.json', LReply.ToJSON);
  LOutput := LReply.Field('outputID').AsText;
  { Observe the server's existing compiler allowance plus delivery grace. An
    earlier client timeout never cancels or resubmits the retained job. }
  Compile('browser', LOutput,
    QWord(LReply.Field('compilerLimits').Field('compilationMilliseconds').AsInteger) + 10000);
  Compile('lcl', LOutput,
    QWord(LReply.Field('compilerLimits').Field('compilationMilliseconds').AsInteger) + 10000);
  Phase('compiled', 'finished');
  Save('result.json', NyxObject([NyxField('checks', NyxData(GChecks)),
    NyxField('workspace', NyxData(GWorkspace)), NyxField('revision', NyxData(GRevision)),
    NyxField('mode', NyxData('resume-build'))]).ToJSON);
  WriteLn('PASS ', GChecks, ' resumed column-event compilation checks');
end;

begin
  GClient := nil;
  try

    if not (ParamCount in [2, 3]) then
    begin
      raise Exception.Create('Supply private QA MCP configuration, external evidence directory, ' +
        'and optionally the exact owned workspace to resume compilation only');
    end;
    GDirectory := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(2)));
    ForceDirectories(GDirectory);
    GClient := TNyxMCPTestClient.Create(ParamStr(1), 'Scooty / column event workshop');
    try

      if ParamCount = 3 then
      begin
        ResumeBuild;
      end
      else
      begin
        Run;
      end;
    finally
      try
        { Retire this transport on assertion/timeout failures as well. Ordinary
          owned projects and their retained compiler jobs survive that closure. }
        GClient.Close;
      finally
        GClient.Free;
        GClient := nil;
      end;
    end;
  except
    on LException: Exception do
    begin

      if GDirectory <> '' then
      begin
        Save('failure.txt', TNyxText(LException.Message));
      end;
      WriteLn('FAIL ', TNyxText(LException.Message));
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
