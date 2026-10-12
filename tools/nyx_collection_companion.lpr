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
program nyx_collection_companion;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.text, nyx.data, nyx.types, nyx.collections, nyx.state,
  nyx.collections.view.types,
  nyx.studio.edits, nyx.studio.collectionedits, nyx.studio.stateedits, nyx.studio.transactions,
  nyx.test.mcp.client;

var
  GClient: TNyxMCPTestClient;
  GReview: TNyxText;
  GRevision: Integer;
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

{ The owning authenticated transport is kept from review creation through
  grouped publication, bounded export, paired history and explicit retirement. }
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
    LFields[High(LFields)] := NyxField('review', NyxData(GReview));
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
    Check(LReply.Field('revision').AsInteger = GRevision, 'Export keeps one exact revision');
    LLines := LReply.Field('lines');
    Check(LLines.Count > 0, 'Source window is nonempty');
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
    NyxField('operationId', NyxData('collection-' + ADirection + '-' + IntToStr(GRevision)))]);
  GRevision := Call('nyx_session', []).Field('revision').AsInteger;
end;

procedure Run;
var
  LPrimary: TNyxDataValue;
  LAfterPrimary: TNyxDataValue;
  LEmpty: TNyxText;
  LSource: TNyxText;
  LKey: TNyxCollectionRef;
  LLayout: INyxDesignPatch;
  LData: INyxCollectionPatch;
  LTransaction: INyxProjectTransaction;
  LStream: TFileStream;
begin
  Check(ParamCount = 2, 'Supply explicit config.toml and a new export directory');
  Check(not DirectoryExists(ParamStr(2)), 'Companion export must be new');
  GClient := TNyxMCPTestClient.Create(ParamStr(1), 'Scooty mixed collection companion');
  try
    LPrimary := Call('nyx_session', [], False);
    GReview := Call('nyx_reviews', [NyxField('mode', NyxData('create')),
      NyxField('base', NyxData('empty')), NyxField('label', NyxData('Collections working together')),
      NyxField('expectedRevision', LPrimary.Field('revision')),
      NyxField('operationId', NyxData('collection-companion-create'))], False).Field('review').AsText;
    Check(GReview <> '', 'Independent review has an exact owning reference');
    GRevision := Call('nyx_session', []).Field('revision').AsInteger;
    LEmpty := Source;
    { All three controls share one portable definition; measurements materialize
      their own independent runtime store from its schema, never mutate defaults. }
    LLayout := ReadNyxDesignPatch(TNyxDataValue.ParseJSON(
      '[{"op":"create","kind":"page","id":"measured-view","root":"page","properties":{"padding":20,"gap":12}},' +
      '{"op":"create","kind":"list","id":"measured-list","parent":"measured-view","properties":{"height":220,"aria-label":"Collection captions"}},' +
      '{"op":"create","kind":"table","id":"measured-table","parent":"measured-view","properties":{"height":220,"aria-label":"Collection priorities"}},' +
      '{"op":"create","kind":"tree","id":"measured-tree","parent":"measured-view","properties":{"height":220,"aria-label":"Collection hierarchy"}}]'));
    LKey := NyxCollection('measured');
    LData := NyxCollectionPatch([
      NyxDefineCollection(LKey, NyxCollectionSchema.Text(NyxTextField('caption'), '')
        .Integer(NyxIntegerField('priority'), 0).Text(NyxTextField('parent'), ''), [
        NyxCollectionItem(NyxItem(LKey, 'row-0')).WithValue(NyxTextField('caption'), 'Explore the collection'),
        NyxCollectionItem(NyxItem(LKey, 'row-1')).WithValue(NyxTextField('caption'), 'Plan the next chapter')
          .WithValue(NyxTextField('parent'), 'row-0'),
        NyxCollectionItem(NyxItem(LKey, 'row-2')).WithValue(NyxTextField('caption'), 'Build something useful')
          .WithValue(NyxTextField('parent'), 'row-0')]),
      NyxBindCollection(NyxBindingOwner('measured-list'), cpList,
        NyxCollectionView(LKey).Column(NyxTextField('caption'), 'Item')),
      NyxBindCollection(NyxBindingOwner('measured-table'), cpTable,
        NyxCollectionView(LKey).Column(NyxTextField('caption'), 'Item')
          .Column(NyxIntegerField('priority'), 'Priority', cmEditable)),
      NyxBindCollection(NyxBindingOwner('measured-tree'), cpTree,
        NyxCollectionView(LKey).Column(NyxTextField('caption'), 'Item').Parent(NyxTextField('parent')))]);
    LTransaction := NyxProjectTransaction([NyxDesignStep(LLayout), NyxCollectionStep(LData)]);
    Call('nyx_transaction', [NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData('collection-companion-compose')),
      NyxField('operations', LTransaction.ToData)]);
    GRevision := Call('nyx_session', []).Field('revision').AsInteger;
    LSource := Source;
    Check((Pos('INyxList', LSource) > 0) and (Pos('INyxTable', LSource) > 0) and
      (Pos('INyxTree', LSource) > 0) and
      (Pos('NyxCollectionView', LSource) > 0), 'Crafted specialized collection/source bindings');
    History('undo');
    Check(Source = LEmpty, 'One paired Undo removes the complete companion and defaults');
    Check(not Call('nyx_session', []).Field('canUndo').AsBoolean, 'No hidden composition history');
    History('redo');
    Check(Source = LSource, 'One paired Redo restores exact typed source');
    ForceDirectories(ParamStr(2));
    LStream := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(2)) + 'nyx.generated.view.pas', fmCreate);
    try
      LStream.WriteBuffer(LSource[1], Length(LSource));
    finally
      LStream.Free;
    end;
    Call('nyx_reviews', [NyxField('mode', NyxData('discard')),
      NyxField('review', NyxData(GReview)), NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData('collection-companion-discard'))], False);
    GReview := '';
    LAfterPrimary := Call('nyx_session', [], False);
    Check((LPrimary.Field('revision').AsInteger = LAfterPrimary.Field('revision').AsInteger) and
      (LPrimary.Field('selection').ToJSON = LAfterPrimary.Field('selection').ToJSON),
      'Unrelated primary revision/selection remain unchanged');
  finally
    GClient.Close;
    GClient.Free;
  end;
end;

begin
  try
    Run;
    WriteLn('PASS ', GChecks, ' semantic mixed-collection composition/history checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
