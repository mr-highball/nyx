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

program nyx_grid_companion;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.text, nyx.data, nyx.test.mcp.client,
  nyx.state, nyx.binding.types, nyx.collections, nyx.collections.view.types, nyx.collections.selection,
  nyx.studio.collectionedits, nyx.studio.collectionintent, nyx.studio.stateedits,
  nyx.studio.edits, nyx.studio.transactions, nyx.studio.editorbuild,
  nyx.studio.builds, nyx.studio.preview;

var
  GClient: TNyxMCPTestClient;
  GReview: TNyxText;
  GRevision: Integer;
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function Call(const AName: TNyxText; const AFields: array of TNyxDataField;
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
    LFields[High(LFields)] := NyxField('workspace', NyxData(GReview));
  end;
  LReply := GClient.Tool(AName, NyxObject(LFields));
  Check(not LReply.Field('isError').AsBoolean,
    AName + ' refused: ' + Copy(LReply.ToJSON, 1, 700));
  Result := LReply.Field('structuredContent');
end;

function Source: TNyxText;
var
  LReply: TNyxDataValue;
  LLines: TNyxDataValue;
  LLine: Integer;
  LIndex: Integer;
begin
  Result := '';
  LLine := 1;
  repeat
    LReply := Call('nyx_source', [NyxField('line', NyxData(LLine)),
      NyxField('count', NyxData(80))]);
    Check(LReply.Field('revision').AsInteger = GRevision, 'Exact source revision');
    LLines := LReply.Field('lines');
    Check(LLines.Count > 0, 'Nonempty bounded source window');
    for LIndex := 0 to LLines.Count - 1 do
    begin
      Result := Result + LLines.Item(LIndex).AsText + TNyxText(#10);
    end;
    Inc(LLine, LLines.Count);
  until LLine > LReply.Field('totalLines').AsInteger;
end;

procedure History(const ADirection: TNyxText);
begin
  Call('nyx_history', [NyxField('direction', NyxData(ADirection)),
    NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData('grid-' + ADirection))]);
  GRevision := Call('nyx_session', []).Field('revision').AsInteger;
end;

{ Optional large-table companion stays on the same semantic paired boundary.
  Typed rows/bindings are composed with the normal layout, never injected into a
  renderer by the input fixtures. The disabled twin qualifies forced host input. }
procedure PagingParts(out ALayout: INyxDesignPatch; out AData: INyxCollectionPatch);
var
  LKey: TNyxCollectionRef;
  LRows: array of TNyxCollectionItem;
  LIndex: Integer;
  LSpec: TNyxCollectionViewSpec;
begin
  ALayout := ReadNyxDesignPatch(TNyxDataValue.ParseJSON(
    '[{"op":"create","kind":"page","id":"paging-review","root":"page",' +
    '"properties":{"gap":12,"padding":20}},' +
    '{"op":"create","kind":"heading","id":"paging-title","parent":"paging-review",' +
    '"properties":{"text":"Ideas worth exploring"}},' +
    '{"op":"create","kind":"label","id":"paging-help","parent":"paging-review",' +
    '"properties":{"text":"Page Up and Page Down follow the visible table. Shift extends a selection; Control moves focus."}},' +
    '{"op":"create","kind":"table","id":"paging-table","parent":"paging-review",' +
    '"properties":{"height":240,"aria-label":"Project ideas"}},' +
    '{"op":"create","kind":"button","id":"after-paging-table","parent":"paging-review",' +
    '"properties":{"text":"Keep exploring"}},' +
    '{"op":"create","kind":"table","id":"disabled-paging-table","parent":"paging-review",' +
    '"properties":{"height":80,"enabled":false,"aria-label":"Read-only input qualification"}}]'));
  LKey := NyxCollection('project-ideas');
  SetLength(LRows, 40);
  for LIndex := 0 to High(LRows) do
  begin
    LRows[LIndex] := NyxCollectionItem(NyxItem(LKey, 'idea-' + TNyxText(IntToStr(LIndex + 1))))
      .WithValue(NyxTextField('idea'), 'Explore idea ' + TNyxText(IntToStr(LIndex + 1)))
      .WithValue(NyxIntegerField('priority'), LIndex + 1);
  end;
  LSpec := NyxCollectionView(LKey).Column(NyxTextField('idea'), 'Idea', cmEditable)
    .Column(NyxIntegerField('priority'), 'Priority', cmEditable)
    .Column(NyxTextField('status'), 'Status').Selection(nsmMultiple);
  AData := NyxCollectionPatch([
    NyxDefineCollection(LKey, NyxCollectionSchema.Text(NyxTextField('idea'), '')
      .Integer(NyxIntegerField('priority'), 1).Text(NyxTextField('status'), 'Ready'), LRows),
    NyxBindCollection(NyxBindingOwner('paging-table'), cpTable, LSpec),
    NyxBindCollection(NyxBindingOwner('disabled-paging-table'), cpTable, LSpec)]);
end;

{ Bounded byte I/O is qualification evidence, never an editor import. Existing
  owned exports identify their exact project; machine profiles stay elsewhere. }
function ReadEvidence(const AName: TNyxText; AMaximum: Integer): TNyxText;
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(AName, fmOpenRead or fmShareDenyNone);
  try
    Check((LStream.Size > 0) and (LStream.Size <= AMaximum), 'Owned evidence has an admitted byte length');
    SetLength(Result, LStream.Size);
    LStream.ReadBuffer(Result[1], Length(Result));
  finally
    LStream.Free;
  end;
end;

{ Build the already composed paging view through semantic jobs. This starts no
  server and configures no compiler. Explicit per-machine output readiness is a
  prerequisite only to these builds, never to creating/exporting the design. }
procedure BuildPagingViews;
var
  LBefore: TNyxText;
  LOutput: TNyxDataValue;
  LRequest: TNyxDataValue;
  LStatus: TNyxDataValue;
  LFields: array of TNyxDataField;
  LTarget: TNyxBuildTarget;
  LJob: TNyxBuildJobRef;
  LArtifact: TNyxCompiledArtifact;
  LIndex: Integer;
  LStarted: QWord;
  LStream: TFileStream;
  LText: TNyxText;
  LReady: Boolean;
begin
  GReview := ReadEvidence(IncludeTrailingPathDelimiter(ParamStr(2)) + 'workspace.txt', 120);
  GRevision := Call('nyx_session', []).Field('revision').AsInteger;
  LBefore := Source;
  Check(LBefore = ReadEvidence(IncludeTrailingPathDelimiter(ParamStr(2)) +
    'nyx.generated.view.pas', 4 * 1024 * 1024), 'Builds use the exact existing semantic export');
  LOutput := Call('nyx_build', [NyxField('mode', NyxData('outputs'))]);
  for LTarget := Low(TNyxBuildTarget) to High(TNyxBuildTarget) do
  begin
    LReady := False;
    for LIndex := 0 to LOutput.Field('outputs').Count - 1 do
    begin

      if LOutput.Field('outputs').Item(LIndex).Field('target').AsText = NyxBuildTargetName(LTarget) then
      begin
        LReady := LOutput.Field('outputs').Item(LIndex).Field('ready').AsBoolean;
      end;
    end;
    Check(LReady,
      'Configure this output before compiling its owned paging view');
    LRequest := NewNyxCompilerRequest.Target(LTarget).Scope(bsView)
      .Root(NyxBuildRoot('paging-review')).AtRevision(GRevision)
      .Output(NyxBuildOutput(LOutput.Field('outputID').AsText))
      .Operation(NyxBuildOperation('grid-paging-' + NyxBuildTargetName(LTarget))).Arguments;
    SetLength(LFields, LRequest.Count);
    for LIndex := 0 to High(LFields) do
    begin
      LFields[LIndex] := NyxField(LRequest.Key(LIndex), LRequest.Field(LRequest.Key(LIndex)));
    end;
    LJob := NyxBuildJob(Call('nyx_build', LFields).Field('job').AsText);
    LStarted := GetTickCount64;
    repeat
      LStatus := Call('nyx_build', [NyxField('mode', NyxData('status')),
        NyxField('job', NyxData(LJob.ID)), NyxField('limit', NyxData(20))]);
      LText := LStatus.ToJSON;
      LStream := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(2)) +
        'paging-build-' + NyxBuildTargetName(LTarget) + '.json', fmCreate);
      try
        LStream.WriteBuffer(LText[1], Length(LText));
      finally
        LStream.Free;
      end;

      if ParseNyxBuildJobState(LStatus.Field('state').AsText) in
        [bjsSucceeded, bjsFailed, bjsCancelled] then
      begin
        Break;
      end;
      Sleep(25);
    until GetTickCount64 - LStarted >= 180000;
    LArtifact := AdmitNyxCompiledArtifact(LStatus);
    Check((LArtifact.Job = LJob.ID) and (LArtifact.Target = LTarget) and
      (LStatus.Field('revision').AsInteger = GRevision) and
      (LStatus.Field('view').AsText = 'paging-review') and
      (ParseNyxBuildScope(LStatus.Field('scope').AsText) = bsView),
      'Semantic compiler admits the exact paging root, revision, target and artifact');
    Check((Source = LBefore) and (Call('nyx_session', []).Field('revision').AsInteger = GRevision),
      'Both semantic compiler jobs preserve accepted source and paired history');
  end;
end;

var
  LPrimary: TNyxDataValue;
  LReply: TNyxDataValue;
  LSource: TNyxText;
  LFile: TFileStream;
  LBefore: TNyxText;
  LKey: TNyxCollectionRef;
  LPatch: INyxCollectionPatch;
  LScalar: INyxStateBindingPatch;
  LLayout: INyxDesignPatch;
  LTransaction: INyxProjectTransaction;
  LPagingLayout: INyxDesignPatch;
  LPagingData: INyxCollectionPatch;
begin

  if (ParamCount <> 2) and
    not ((ParamCount = 3) and
      ((ParamStr(3) = '--paging') or (ParamStr(3) = '--build-paging'))) then
  begin
    raise Exception.Create('Use grid companion <explicit config.toml> <source directory> [--paging|--build-paging]');
  end;

  if (ParamCount = 3) and (ParamStr(3) = '--build-paging') then
  begin
    GClient := TNyxMCPTestClient.Create(ParamStr(1), 'Scooty grid compiler');
    try
      BuildPagingViews;
      WriteLn('PASS ', GChecks, ' semantic paging view/compiler checks');
    finally
      GClient.Close;
      GClient.Free;
    end;
    Exit;
  end;

  if DirectoryExists(ParamStr(2)) then
  begin
    raise Exception.Create('Companion destination must be new; retained exports are never replaced');
  end;
  GClient := TNyxMCPTestClient.Create(ParamStr(1), 'Scooty grid companion');
  try
    LPrimary := Call('nyx_session', [], False);
    LReply := Call('nyx_workspaces', [
      NyxField('mode', NyxData('create')),
      NyxField('base', NyxData('empty')),
      NyxField('label', NyxData('A thoughtful work table')),
      NyxField('expectedRevision', LPrimary.Field('revision')),
      NyxField('operationId', NyxData('grid-companion-create'))], False);
    GReview := LReply.Field('workspace').AsText;
    Check(GReview <> '', 'Owned ordinary workspace has an exact identity');
    GRevision := Call('nyx_session', []).Field('revision').AsInteger;
    Check(GRevision = 1, 'New owned workspace begins without composition history');
    LBefore := Source;
    LLayout := ReadNyxDesignPatch(TNyxDataValue.ParseJSON(
        '[{"op":"create","kind":"page","id":"grid-review","root":"page",' +
        '"properties":{"gap":12,"padding":20}},' +
        '{"op":"create","kind":"heading","id":"grid-title","parent":"grid-review",' +
        '"properties":{"text":"A thoughtful work table"}},' +
        '{"op":"create","kind":"label","id":"grid-help","parent":"grid-review",' +
        '"properties":{"text":"Use arrows to move between cells. Enter edits a value; Escape returns to the cell."}},' +
        '{"op":"create","kind":"input","id":"workspace-note","parent":"grid-review",' +
        '"properties":{"text":"Workspace note","placeholder":"A thought for later"}},' +
        '{"op":"create","kind":"table","id":"work-table","parent":"grid-review",' +
        '"properties":{"height":240,"aria-label":"Work items"}},' +
        '{"op":"create","kind":"button","id":"after-table","parent":"grid-review",' +
        '"properties":{"text":"Keep creating"}}]'));
    { The portable paired candidate owns layout, scalar and collection binding
      as one semantic mutation. One Undo restores this new workspace's exact
      initial source, including removal of the newly composed page and data. }
    LKey := NyxCollection('work-items');
    LScalar := NyxStateBindingPatch([
      NyxCreateDefault(NyxStateValue(NyxTextState('workspace-note'), 'Keep crafting.')),
      NyxBindControl(NyxBindingOwner('workspace-note'), bpValue,
        NyxTextState('workspace-note'), bdTwoWay)]);
    LPatch := NyxCollectionPatch([
      NyxDefineCollection(LKey, NyxCollectionSchema.Text(NyxTextField('task'), '')
        .Integer(NyxIntegerField('priority'), 1).Text(NyxTextField('status'), 'Ready'), [
        NyxCollectionItem(NyxItem(LKey, 'plan')).WithValue(NyxTextField('task'), 'Plan the next idea'),
        NyxCollectionItem(NyxItem(LKey, 'design')).WithValue(NyxTextField('task'), 'Sketch the experience')
          .WithValue(NyxIntegerField('priority'), 2),
        NyxCollectionItem(NyxItem(LKey, 'build')).WithValue(NyxTextField('task'), 'Build something useful')
          .WithValue(NyxIntegerField('priority'), 3),
        NyxCollectionItem(NyxItem(LKey, 'share')).WithValue(NyxTextField('task'), 'Share the result')]),
      NyxBindCollection(NyxBindingOwner('work-table'), cpTable,
        NyxCollectionView(LKey).Column(NyxTextField('task'), 'Task', cmEditable)
          .Column(NyxIntegerField('priority'), 'Priority', cmEditable)
          .Column(NyxTextField('status'), 'Status').Selection(nsmMultiple))]);
    LTransaction := NyxProjectTransaction([
      NyxDesignStep(LLayout), NyxStateStep(LScalar), NyxCollectionStep(LPatch)]);

    if ParamCount = 3 then
    begin
      PagingParts(LPagingLayout, LPagingData);
      LTransaction := NyxProjectTransaction([
        NyxDesignStep(LLayout), NyxStateStep(LScalar), NyxCollectionStep(LPatch),
        NyxDesignStep(LPagingLayout), NyxCollectionStep(LPagingData)]);
    end;
    Call('nyx_transaction', [
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData('grid-companion-compose')),
      NyxField('operations', LTransaction.ToData)]);
    GRevision := Call('nyx_session', []).Field('revision').AsInteger;
    LSource := Source;
    Check((Pos('INyxTable', LSource) > 0) and (Pos('INyxInput', LSource) > 0) and
      (Pos('NyxCollectionView', LSource) > 0),
      'Specialized crafted scalar and collection companion');
    History('undo');
    Check(Source = LBefore, 'One paired Undo removes the entire composed layout and data');
    Check(not Call('nyx_session', []).Field('canUndo').AsBoolean,
      'No hidden layout or collection history remains after the one Undo');
    Check(Call('nyx_outline', [NyxField('scope', NyxData('pages'))]).Field('total').AsInteger = 0,
      'Whole-composition Undo removes the new page');
    Check(Call('nyx_collections', [NyxField('mode', NyxData('list'))]).Field('total').AsInteger = 0,
      'Whole-composition Undo removes its data defaults');
    Check(Call('nyx_state', [NyxField('mode', NyxData('defaults'))]).Field('total').AsInteger = 0,
      'Whole-composition Undo removes its scalar defaults');
    History('redo');
    Check(Source = LSource, 'One paired Redo restores the exact companion');
    ForceDirectories(ParamStr(2));
    LFile := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(2)) +
      'nyx.generated.view.pas', fmCreate);
    try
      LFile.WriteBuffer(LSource[1], Length(LSource));
    finally
      LFile.Free;
    end;
    LFile := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(2)) + 'workspace.txt', fmCreate);
    try
      LFile.WriteBuffer(GReview[1], Length(GReview));
    finally
      LFile.Free;
    end;
    { Leave this owned ordinary project reviewable. Closing the connection
      retires presence, not the authored project or its independent history. }
    Check(Call('nyx_session', [], False).Field('revision').AsInteger =
      LPrimary.Field('revision').AsInteger, 'Primary revision remains unchanged');
    WriteLn('PASS ', GChecks, ' semantic companion/source/history checks');
  finally
    try
      GClient.Close;
    finally
      GClient.Free;
    end;
  end;
end.
