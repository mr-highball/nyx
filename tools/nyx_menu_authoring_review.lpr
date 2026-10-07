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

program nyx_menu_authoring_review;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, fphttpclient, nyx.text, nyx.data, nyx.types, nyx.root.types,
  nyx.menu.types, nyx.menu.declarations, nyx.studio.edits, nyx.test.mcp.client;

var
  GClient: TNyxMCPTestClient;
  GReview: TNyxText;
  GRevision: Integer;
  GChecks: Integer;
  GAcceptedSource: TNyxText;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function Call(const AName: TNyxText; const AFields: array of TNyxDataField;
  AContext: Boolean = True; ARefuse: Boolean = False): TNyxDataValue;
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
  Check(LReply.Field('isError').AsBoolean = ARefuse,
    AName + ' unexpected admission: ' + Copy(LReply.ToJSON, 1, 700));
  Result := LReply.Field('structuredContent');
end;

procedure Save(const AName, AText: TNyxText);
var
  LFile: TFileStream;
begin
  ForceDirectories(ParamStr(2));
  LFile := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(2)) + AName, fmCreate);
  try

    if AText <> '' then
    begin
      LFile.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LFile.Free;
  end;
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
    Check(LReply.Field('revision').AsInteger = GRevision, 'Bounded source stays at one revision');
    LLines := LReply.Field('lines');
    Check(LLines.Count > 0, 'A source window makes progress');
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
    NyxField('operationId', NyxData('declared-menu-' + ADirection))]);
  GRevision := Call('nyx_session', []).Field('revision').AsInteger;
end;

procedure Build(const ATarget, AScope, AOutput: TNyxText);
var
  LFields: array of TNyxDataField;
  LReply: TNyxDataValue;
  LJob: TNyxText;
  LState: TNyxText;
  LStarted: QWord;
  LHTTP: TFPHTTPClient;
  LBytes: TMemoryStream;
  LCompiled: TNyxText;
begin
  SetLength(LFields, 6 + Ord(AScope <> 'application'));
  LFields[0] := NyxField('mode', NyxData('request'));
  LFields[1] := NyxField('expectedRevision', NyxData(GRevision));
  LFields[2] := NyxField('operationId', NyxData('menu-' + ATarget + '-' + AScope));
  LFields[3] := NyxField('outputID', NyxData(AOutput));
  LFields[4] := NyxField('target', NyxData(ATarget));
  LFields[5] := NyxField('scope', NyxData(AScope));

  if AScope <> 'application' then
  begin
    LFields[6] := NyxField('view', NyxData('home'));
  end;
  LReply := Call('nyx_build', LFields);
  LJob := LReply.Field('job').AsText;
  Check(Call('nyx_build', LFields).Field('job').AsText = LJob,
    'Exact build retry reuses its immutable job');
  LStarted := GetTickCount64;
  repeat
    LReply := Call('nyx_build', [NyxField('mode', NyxData('status')),
      NyxField('job', NyxData(LJob)), NyxField('severity', NyxData('error')),
      NyxField('limit', NyxData(4))]);
    LState := LReply.Field('state').AsText;

    if GetTickCount64 - LStarted > 90000 then
    begin
      raise Exception.Create('Menu compile exceeded its real-clock budget');
    end;
    Sleep(100);
  until (LState = 'succeeded') or (LState = 'failed') or (LState = 'cancelled');
  Save('build-' + ATarget + '-' + AScope + '.json', LReply.ToJSON);
  Check((LState = 'succeeded') and LReply.Field('currentSource').AsBoolean,
    'The actual menu compiler succeeds at the accepted source revision');
  { HTTP bytes establish the real compiler input independently of its status
    flags. The URL is an explicit local fixture host, never a tool-supplied path. }
  LHTTP := TFPHTTPClient.Create(nil);
  LBytes := TMemoryStream.Create;
  try
    LHTTP.Get(ParamStr(3) + '/' + LReply.Field('compiledSource').AsText, LBytes);
    SetLength(LCompiled, LBytes.Size);

    if LBytes.Size <> 0 then
    begin
      Move(LBytes.Memory^, LCompiled[1], LBytes.Size);
    end;
    Check(LCompiled = GAcceptedSource, 'The actual compiler receives the exact exported menu source');
  finally
    LBytes.Free;
    LHTTP.Free;
  end;
  WriteLn('Qualified ', ATarget, '/', AScope);
  Flush(Output);
end;

procedure Run;
var
  LPrimary: TNyxDataValue;
  LReply: TNyxDataValue;
  LOperations: array of TNyxDataValue;
  LCreation: TNyxDataValue;
  LActions: INyxMenuDefinition;
  LDensity: INyxMenuDefinition;
  LBefore: TNyxText;
  LSource: TNyxText;
  LOutput: TNyxText;
  LIndex: Integer;
begin
  LPrimary := Call('nyx_session', [], False);
  LReply := Call('nyx_reviews', [NyxField('mode', NyxData('create')),
    NyxField('base', NyxData('empty')), NyxField('label', NyxData('Saved thoughtful actions')),
    NyxField('expectedRevision', LPrimary.Field('revision')),
    NyxField('operationId', NyxData('declared-menu-create'))], False);
  GReview := LReply.Field('review').AsText;
  GRevision := Call('nyx_session', []).Field('revision').AsInteger;
  LBefore := Source;
  { Raw keys below are the explicit transport boundary. Policy/item meaning is
    authored through the public Pascal types, never a parallel string builder. }
  LCreation := TNyxDataValue.ParseJSON(
    '[{"op":"create","kind":"page","id":"home","root":"page","properties":{"gap":12,"padding":24}},' +
    '{"op":"create","kind":"button","id":"open-actions","parent":"home","properties":{"text":"Actions"}},' +
    '{"op":"create","kind":"column","id":"actions-content","root":"component","properties":{"gap":4,"padding":8,"compound":true}},' +
    '{"op":"create","kind":"button","id":"copy-draft","parent":"actions-content","properties":{"text":"Copy draft","part":"copy"}},' +
    '{"op":"create","kind":"button","id":"show-guides","parent":"actions-content","properties":{"text":"Show guides","part":"guides"}},' +
    '{"op":"create","kind":"separator","id":"action-divider","parent":"actions-content","properties":{"part":"divider","height":1}},' +
    '{"op":"create","kind":"button","id":"choose-density","parent":"actions-content","properties":{"text":"Density","part":"density"}},' +
    '{"op":"create","kind":"column","id":"density-content","root":"component","properties":{"gap":4,"padding":8,"compound":true}},' +
    '{"op":"create","kind":"button","id":"roomy-density","parent":"density-content","properties":{"text":"Roomy","part":"roomy"}},' +
    '{"op":"create","kind":"button","id":"compact-density","parent":"density-content","properties":{"text":"Compact","part":"compact"}}]');
  LActions := NewNyxMenuDefinition(NyxReusableRoot('actions-content'), NyxMenu('Thoughtful actions'))
    .Action(NyxPart('copy'), NyxMenuCommand('copy-draft'))
    .Check(NyxPart('guides'), NyxMenuCommand('show-guides'), True)
    .Separator(NyxPart('divider'))
    .Submenu(NyxPart('density'), NyxMenuRef('density'));
  LDensity := NewNyxMenuDefinition(NyxReusableRoot('density-content'), NyxMenu('Density'))
    .Radio(NyxPart('roomy'), NyxMenuCommand('roomy'), NyxMenuGroup('spacing'), True)
    .Radio(NyxPart('compact'), NyxMenuCommand('compact'), NyxMenuGroup('spacing'), False);
  SetLength(LOperations, LCreation.Count + 3);
  for LIndex := 0 to LCreation.Count - 1 do
  begin
    LOperations[LIndex] := LCreation.Item(LIndex);
  end;
  LOperations[LCreation.Count] := NyxDefineMenu(NyxMenuRef('actions'), LActions).ToData;
  LOperations[LCreation.Count + 1] := NyxDefineMenu(NyxMenuRef('density'), LDensity).ToData;
  LOperations[LCreation.Count + 2] := NyxAttachMenu(NyxControl('open-actions'), NyxMenuRef('actions')).ToData;
  Call('nyx_transaction', [NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData('declared-menu-compose')),
    NyxField('operations', NyxArray(LOperations))]);
  Inc(GRevision);
  LSource := Source;
  Check((Pos('NewNyxMenuDefinition(', LSource) > 0) and
    (Pos('.Menu(NyxMenuRef(', LSource) > 0), 'MCP generates typed crafted menu source');
  LReply := Call('nyx_menus', [NyxField('limit', NyxData(1))]);
  Check((LReply.Field('total').AsInteger = 2) and LReply.Field('hasMore').AsBoolean,
    'Authenticated menu discovery is bounded');
  LReply := Call('nyx_menus', [NyxField('name', NyxData('actions')),
    NyxField('itemOffset', NyxData(3)), NyxField('itemLimit', NyxData(1))]);
  Check((LReply.Field('items').Count = 1) and
    (LReply.Field('items').Item(0).Field('submenu').AsText = 'density'),
    'Authenticated exact menu inspection pages semantic branch meaning');
  Save('menus.json', LReply.ToJSON);
  Call('nyx_transaction', [NyxField('expectedRevision', NyxData(GRevision - 1)),
    NyxField('operationId', NyxData('declared-menu-stale')),
    NyxField('operations', NyxArray([NyxNoMenu(NyxControl('open-actions')).ToData]))], True, True);
  Call('nyx_transaction', [NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData('declared-menu-dangling')),
    NyxField('operations', NyxArray([NyxRemoveMenu(NyxMenuRef('density')).ToData]))], True, True);
  Check(Source = LSource, 'Refused stale/dangling groups retain exact accepted source');
  History('undo');
  Check(Source = LBefore, 'One Undo restores the exact pre-menu source');
  History('redo');
  Check(Source = LSource, 'One Redo restores the exact whole composition');
  Save('nyx.generated.view.pas', LSource);
  GAcceptedSource := LSource;
  LOutput := Call('nyx_build', [NyxField('mode', NyxData('outputs'))]).Field('outputID').AsText;
  Build('browser', 'application', LOutput);
  Build('lcl', 'application', LOutput);
  Build('browser', 'view', LOutput);
  Build('lcl', 'view', LOutput);
  Call('nyx_reviews', [NyxField('mode', NyxData('discard')),
    NyxField('review', NyxData(GReview)), NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData('declared-menu-discard'))], False);
  GReview := '';
  LReply := Call('nyx_session', [], False);
  for LIndex := 0 to LPrimary.Count - 1 do
  begin
    { Activity is deliberately visible to an observing user. Revision, document
      counts, selection, navigation, history and draft/permission stay exact. }

    if LPrimary.Key(LIndex) <> 'activitySequence' then
    begin
      Check(LReply.Field(LPrimary.Key(LIndex)).ToJSON =
        LPrimary.Field(LPrimary.Key(LIndex)).ToJSON,
        'The primary semantic context remains unchanged: ' + LPrimary.Key(LIndex));
    end;
  end;
end;

begin
  GClient := nil;
  try

    if ParamCount <> 3 then
    begin
      raise Exception.Create('Supply isolated config.toml, owned output directory and editor HTTP base');
    end;
    GClient := TNyxMCPTestClient.Create(ParamStr(1), 'Scooty saved menu review');
    try
      Run;
    finally
      GClient.Close;
    end;
    WriteLn('PASS ', GChecks, ' authenticated menu authoring checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
  GClient.Free;
end.
