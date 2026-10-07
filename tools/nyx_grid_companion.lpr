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
  nyx.state, nyx.collections, nyx.collections.view.types, nyx.collections.selection,
  nyx.studio.collectionedits, nyx.studio.collectionintent, nyx.studio.stateedits;

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

var
  LPrimary: TNyxDataValue;
  LReply: TNyxDataValue;
  LSource: TNyxText;
  LFile: TFileStream;
  LBefore: TNyxText;
  LKey: TNyxCollectionRef;
  LPatch: INyxCollectionPatch;
begin

  if ParamCount <> 2 then
  begin
    raise Exception.Create('Use grid companion <explicit config.toml> <new source directory>');
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
    Call('nyx_transaction', [
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData('grid-companion-layout')),
      NyxField('operations', TNyxDataValue.ParseJSON(
        '[{"op":"create","kind":"page","id":"grid-review","root":"page",' +
        '"properties":{"gap":12,"padding":20}},' +
        '{"op":"create","kind":"heading","id":"grid-title","parent":"grid-review",' +
        '"properties":{"text":"A thoughtful work table"}},' +
        '{"op":"create","kind":"label","id":"grid-help","parent":"grid-review",' +
        '"properties":{"text":"Use arrows to move between cells. Enter edits a value; Escape returns to the cell."}},' +
        '{"op":"create","kind":"table","id":"work-table","parent":"grid-review",' +
        '"properties":{"height":240,"aria-label":"Work items"}},' +
        '{"op":"create","kind":"button","id":"after-table","parent":"grid-review",' +
        '"properties":{"text":"Keep creating"}}]'))]);
    GRevision := Call('nyx_session', []).Field('revision').AsInteger;
    LBefore := Source;
    { Layout creation and collection/default binding currently have separate
      semantic APIs. Each phase is one grouped paired operation; do not claim
      a single whole-composition Undo. Record that existing workflow gap. }
    LKey := NyxCollection('work-items');
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
    Call('nyx_collections', [NyxField('mode', NyxData('apply')),
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData('grid-companion-data')),
      NyxField('changes', LPatch.ToData)]);
    GRevision := Call('nyx_session', []).Field('revision').AsInteger;
    LSource := Source;
    Check((Pos('INyxTable', LSource) > 0) and (Pos('NyxCollectionView', LSource) > 0),
      'Specialized crafted bound-table companion');
    History('undo');
    Check(Source = LBefore, 'One paired Undo restores the exact layout before data binding');
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
