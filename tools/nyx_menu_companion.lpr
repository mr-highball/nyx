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

program nyx_menu_companion;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.text, nyx.data, nyx.test.mcp.client;

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
    NyxField('operationId', NyxData('menu-' + ADirection))]);
  GRevision := Call('nyx_session', []).Field('revision').AsInteger;
end;

var
  LPrimary: TNyxDataValue;
  LReply: TNyxDataValue;
  LSource: TNyxText;
  LFile: TFileStream;
begin

  if ParamCount <> 2 then
  begin
    raise Exception.Create('Use menu companion <explicit config.toml> <source directory>');
  end;
  GClient := TNyxMCPTestClient.Create(ParamStr(1), 'Scooty menu companion');
  try
    LPrimary := Call('nyx_session', [], False);
    LReply := Call('nyx_reviews', [
      NyxField('mode', NyxData('create')),
      NyxField('base', NyxData('empty')),
      NyxField('label', NyxData('Thoughtful actions')),
      NyxField('expectedRevision', LPrimary.Field('revision')),
      NyxField('operationId', NyxData('menu-companion-create'))], False);
    GReview := LReply.Field('review').AsText;
    GRevision := Call('nyx_session', []).Field('revision').AsInteger;
    Call('nyx_transaction', [
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData('menu-companion-compose')),
      NyxField('operations', TNyxDataValue.ParseJSON(
        '[{"op":"create","kind":"column","id":"thoughtful-actions","root":"page",' +
        '"properties":{"layout":"column","gap":4,"padding":8,"cross-alignment":"stretch","compound":true}},' +
        '{"op":"create","kind":"button","id":"menu-cut","parent":"thoughtful-actions",' +
        '"properties":{"text":"Cut","part":"cut","variant":"secondary"}},' +
        '{"op":"create","kind":"button","id":"menu-copy","parent":"thoughtful-actions",' +
        '"properties":{"text":"Copy","part":"copy","variant":"secondary"}},' +
        '{"op":"create","kind":"button","id":"menu-paste","parent":"thoughtful-actions",' +
        '"properties":{"text":"Paste","part":"paste","variant":"secondary"}},' +
        '{"op":"create","kind":"separator","id":"menu-separator","parent":"thoughtful-actions",' +
        '"properties":{"part":"separator","height":1}},' +
        '{"op":"create","kind":"button","id":"menu-guides","parent":"thoughtful-actions",' +
        '"properties":{"text":"Show guides","part":"guides","variant":"secondary"}},' +
        '{"op":"create","kind":"button","id":"menu-comfortable","parent":"thoughtful-actions",' +
        '"properties":{"text":"Comfortable","part":"comfortable","variant":"secondary"}},' +
        '{"op":"create","kind":"button","id":"menu-compact","parent":"thoughtful-actions",' +
        '"properties":{"text":"Compact","part":"compact","variant":"secondary"}},' +
        '{"op":"create","kind":"button","id":"menu-hidden","parent":"thoughtful-actions",' +
        '"properties":{"text":"Archived action","part":"hidden","visible":false}}]'))]);
    GRevision := Call('nyx_session', []).Field('revision').AsInteger;
    LSource := Source;
    Check(Pos('INyxButton', LSource) > 0, 'Specialized crafted companion');
    History('undo');
    Check(Source <> LSource, 'One paired Undo removes the composition');
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
    Call('nyx_reviews', [NyxField('mode', NyxData('discard')),
      NyxField('review', NyxData(GReview)),
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData('menu-companion-discard'))], False);
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
