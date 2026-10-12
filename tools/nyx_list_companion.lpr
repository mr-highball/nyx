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
program nyx_list_companion;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.text, nyx.data, nyx.types, nyx.collections, nyx.state,
  nyx.collections.view.types, nyx.collections.selection, nyx.typeahead,
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
    NyxField('operationId', NyxData('list-' + ADirection + '-' + IntToStr(GRevision)))]);
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
  GClient := TNyxMCPTestClient.Create(ParamStr(1), 'Scooty list companion');
  try
    LPrimary := Call('nyx_session', [], False);
    GReview := Call('nyx_reviews', [NyxField('mode', NyxData('create')),
      NyxField('base', NyxData('empty')), NyxField('label', NyxData('Choose your next idea')),
      NyxField('expectedRevision', LPrimary.Field('revision')),
      NyxField('operationId', NyxData('list-companion-create'))], False).Field('review').AsText;
    Check(GReview <> '', 'Independent review has an exact owning reference');
    GRevision := Call('nyx_session', []).Field('revision').AsInteger;
    LEmpty := Source;
    { Explicit component/part identities permit the whole definition, its
      collection and two instances to publish as one ordinary semantic command. }
    LLayout := ReadNyxDesignPatch(TNyxDataValue.ParseJSON(
      '[{"op":"create","kind":"card","id":"idea-card","root":"component","properties":{"padding":20,"gap":12}},' +
      '{"op":"create","kind":"heading","id":"idea-card-title","parent":"idea-card","properties":{"part":"title","text":"Choose your next idea"}},' +
      '{"op":"create","kind":"list","id":"idea-options","parent":"idea-card","properties":{"part":"options","height":240,"aria-label":"Available ideas"}},' +
      '{"op":"create","kind":"button","id":"idea-card-action","parent":"idea-card","properties":{"part":"action","text":"Keep creating"}},' +
      '{"op":"create","kind":"page","id":"list-review","root":"page","properties":{"padding":20,"gap":16}},' +
      '{"op":"instance","component":"idea-card","id":"first-card","parent":"list-review"},' +
      '{"op":"instance","component":"idea-card","id":"second-card","parent":"list-review"}]'));
    LKey := NyxCollection('ideas');
    LData := NyxCollectionPatch([
      NyxDefineCollection(LKey, NyxCollectionSchema.Text(NyxTextField('title'), '')
        .Integer(NyxIntegerField('rank'), 0), [
        NyxCollectionItem(NyxItem(LKey, 'plan')).WithValue(NyxTextField('title'), 'Plan something useful'),
        NyxCollectionItem(NyxItem(LKey, 'sketch')).WithValue(NyxTextField('title'), 'Sketch a new experience'),
        NyxCollectionItem(NyxItem(LKey, 'build')).WithValue(NyxTextField('title'), 'Build the next idea')]),
      NyxBindCollection(NyxBindingOwner('idea-options'), cpList,
        NyxCollectionView(LKey).Column(NyxTextField('title'), 'Idea')
          .Selection(nsmMultiple).Scoped(csInstance).TypeAhead(NyxTypeAhead))]);
    LTransaction := NyxProjectTransaction([NyxDesignStep(LLayout), NyxCollectionStep(LData)]);
    Call('nyx_transaction', [NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData('list-companion-compose')),
      NyxField('operations', LTransaction.ToData)]);
    GRevision := Call('nyx_session', []).Field('revision').AsInteger;
    LSource := Source;
    Check((Pos('INyxList', LSource) > 0) and (Pos('INyxCard', LSource) > 0) and
      (Pos('NyxCollectionView', LSource) > 0), 'Crafted specialized compound/source binding');
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
      NyxField('operationId', NyxData('list-companion-discard'))], False);
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
    WriteLn('PASS ', GChecks, ' semantic reusable-list composition/history checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
