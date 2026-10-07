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

program nyx_query_companion;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, fphttpclient, nyx.text, nyx.data, nyx.collections,
  nyx.collections.query, nyx.collections.view.types, nyx.studio.collectionintent,
  nyx.studio.collectionedits, nyx.studio.stateedits,
  nyx.test.mcp.client;

var
  GClient: TNyxMCPTestClient;
  GWorkspace: TNyxText;
  GDirectory: TNyxText;
  GRevision: Integer;
  GChecks: Integer;
  GSequence: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

{ This persistent transport only modifies the exact grid workspace exported by
  the companion tool. No primary project, navigation or configuration is claimed.
  Related row/query changes use one ordinary typed paired candidate. }
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
    LFields[High(LFields)] := NyxField('workspace', NyxData(GWorkspace));
  end;
  LReply := GClient.Tool(AName, NyxObject(LFields));
  Check(LReply.Field('isError').AsBoolean = ARefuse,
    AName + ' unexpected admission: ' + Copy(LReply.ToJSON, 1, 900));
  Result := LReply.Field('structuredContent');
end;

procedure Save(const AName, AText: TNyxText);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(GDirectory + AName, fmCreate);
  try

    if AText <> '' then
    begin
      LStream.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LStream.Free;
  end;
end;

function Operation: TNyxText;
begin
  Inc(GSequence);
  Result := 'query-companion-' + TNyxText(IntToStr(GSequence));
end;

function Source: TNyxText;
var
  LReply: TNyxDataValue;
  LLine: Integer;
  LIndex: Integer;
begin
  Result := '';
  LLine := 1;
  repeat
    LReply := Call('nyx_source', [NyxField('line', NyxData(LLine)),
      NyxField('count', NyxData(80))]);
    Check(LReply.Field('revision').AsInteger = GRevision, 'Exact bounded source revision');
    Check(LReply.Field('lines').Count > 0, 'Source pagination advances');
    for LIndex := 0 to LReply.Field('lines').Count - 1 do
    begin
      Result := Result + LReply.Field('lines').Item(LIndex).AsText + TNyxText(#10);
    end;
    Inc(LLine, LReply.Field('lines').Count);
  until LLine > LReply.Field('totalLines').AsInteger;
end;

procedure Apply(const APatch: INyxCollectionPatch; ARevision: Integer = 0;
  ARefuse: Boolean = False);
begin

  if ARevision = 0 then
  begin
    ARevision := GRevision;
  end;
  Call('nyx_collections', [NyxField('mode', NyxData('apply')),
    NyxField('expectedRevision', NyxData(ARevision)),
    NyxField('operationId', NyxData(Operation)), NyxField('changes', APatch.ToData)],
    True, ARefuse);
  GRevision := Call('nyx_session', []).Field('revision').AsInteger;
end;

procedure History(const ADirection: TNyxText);
begin
  Call('nyx_history', [NyxField('direction', NyxData(ADirection)),
    NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData(Operation))]);
  GRevision := Call('nyx_session', []).Field('revision').AsInteger;
end;

{ Ordinary authenticated build receipts are polled to a terminal state. Compare
  the actual HTTP compiler input with accepted bounded source; admission alone
  is not compilation or successful application execution. }
procedure Build(const ATarget, AScope: TNyxText);
var
  LReply: TNyxDataValue;
  LOutput: TNyxDataValue;
  LFields: array of TNyxDataField;
  LJob: TNyxText;
  LStarted: QWord;
  LHTTP: TFPHTTPClient;
  LBytes: TMemoryStream;
  LCompiled: TNyxText;
begin
  LOutput := Call('nyx_build', [NyxField('mode', NyxData('outputs'))]);
  SetLength(LFields, 6 + Ord(AScope = 'view'));
  LFields[0] := NyxField('mode', NyxData('request'));
  LFields[1] := NyxField('expectedRevision', NyxData(GRevision));
  LFields[2] := NyxField('operationId', NyxData(Operation));
  LFields[3] := NyxField('outputID', LOutput.Field('outputID'));
  LFields[4] := NyxField('target', NyxData(ATarget));
  LFields[5] := NyxField('scope', NyxData(AScope));

  if AScope = 'view' then
  begin
    LFields[6] := NyxField('view', NyxData('grid-review'));
  end;
  LJob := Call('nyx_build', LFields).Field('job').AsText;
  LStarted := GetTickCount64;
  repeat
    LReply := Call('nyx_build', [NyxField('mode', NyxData('status')),
      NyxField('job', NyxData(LJob)), NyxField('severity', NyxData('error')),
      NyxField('limit', NyxData(8))]);

    if (LReply.Field('state').AsText <> 'queued') and
      (LReply.Field('state').AsText <> 'running') then
    begin
      Break;
    end;

    if GetTickCount64 - LStarted > 90000 then
    begin
      raise Exception.Create('Compiler job exceeded the maintained completion bound');
    end;
    Sleep(100);
  until False;
  Save(ATarget + '-' + AScope + '-build.json', LReply.ToJSON);
  Check((LReply.Field('state').AsText = 'succeeded') and
    LReply.Field('currentSource').AsBoolean, 'Actual query compiler job succeeds');
  LHTTP := TFPHTTPClient.Create(nil);
  LBytes := TMemoryStream.Create;
  try
    LHTTP.Get(TNyxText(ParamStr(4)) + TNyxText('/') +
      LReply.Field('compiledSource').AsText, LBytes);
    SetLength(LCompiled, LBytes.Size);

    if LBytes.Size > 0 then
    begin
      Move(LBytes.Memory^, LCompiled[1], LBytes.Size);
    end;
    Check(LCompiled = Source, 'Actual compiler source equals accepted semantic export');
  finally
    LBytes.Free;
    LHTTP.Free;
  end;
end;

var
  LPrimary: TNyxDataValue;
  LBefore: TNyxText;
  LChanged: TNyxText;
  LBindings: TNyxText;
  LReply: TNyxDataValue;
  LKey: TNyxCollectionRef;
  LPolicy: TNyxCollectionQuery;
  LPatch: INyxCollectionPatch;
  LOldRevision: Integer;
begin

  if ParamCount <> 4 then
  begin
    raise Exception.Create('Use query companion <explicit config.toml> <owned grid workspace> <new evidence directory> <editor URL without trailing slash>');
  end;

  if DirectoryExists(ParamStr(3)) then
  begin
    raise Exception.Create('Evidence directory must be new; prior results remain retained');
  end;
  GWorkspace := ParamStr(2);
  GDirectory := IncludeTrailingPathDelimiter(ParamStr(3));
  ForceDirectories(GDirectory);
  GClient := TNyxMCPTestClient.Create(ParamStr(1), 'Scooty typed query companion');
  try
    LReply := GClient.RPC('tools/list', NyxObject([])).Field('result').Field('tools');
    Check(LReply.Count = 21, 'Current authenticated twenty-one-tool catalog');
    LPrimary := Call('nyx_session', [], False);
    GRevision := Call('nyx_session', []).Field('revision').AsInteger;
    LBefore := Source;
    Check((Pos('INyxTable', LBefore) > 0) and (Pos('work-items', LBefore) > 0) and
      (Pos('NyxWhere', LBefore) = 0), 'Exact previously composed query-free English grid');
    LBindings := Call('nyx_collections', [NyxField('mode', NyxData('bindings')),
      NyxField('owner', NyxData('work-table'))]).Field('effective').ToJSON;
    LReply := Call('nyx_collections', [NyxField('mode', NyxData('query')),
      NyxField('owner', NyxData('work-table')), NyxField('source', NyxData('effective'))]);
    Check(not LReply.Field('query').Field('defined').AsBoolean,
      'Authenticated bounded query inspection starts empty');
    LKey := NyxCollection('work-items');
    LPolicy := NyxCollectionQuery.Where(NyxWhere(NyxIntegerField('priority')).AtLeast(2)
      .AndAlso(NyxWhere(NyxTextField('task')).EqualTo('').Negated))
      .OrderBy(NyxIntegerField('priority'), nsdDescending)
      .ThenBy(NyxTextField('task'), nsdAscending, nqtAsciiInsensitive);
    LPatch := NyxCollectionPatch([
      NyxUpdateCollectionRow(NyxCollectionItem(NyxItem(LKey, 'design'))
        .WithValue(NyxTextField('status'), 'In progress')),
      NyxSetCollectionQuery(NyxBindingOwner('work-table'), LKey, cpTable, LPolicy)]);
    LOldRevision := GRevision;
    Apply(LPatch);
    LChanged := Source;
    Check((Pos('.AtLeast(2)', LChanged) > 0) and (Pos('In progress', LChanged) > 0),
      'One semantic group generates crafted typed query and row source');
    LReply := Call('nyx_collections', [NyxField('mode', NyxData('query')),
      NyxField('owner', NyxData('work-table')), NyxField('source', NyxData('effective')),
      NyxField('limit', NyxData(1))]);
    Check((LReply.Field('query').Field('nodes').Count = 1) and
      (LReply.Field('query').Field('total').AsInteger = 4) and
      (LReply.Field('query').Field('order').Count = 2) and
      (Length(LReply.ToJSON) < 1000), 'Authenticated query context is small and paged');
    LReply := Call('nyx_collections', [NyxField('mode', NyxData('query-value')),
      NyxField('owner', NyxData('work-table')), NyxField('source', NyxData('effective')),
      NyxField('path', NyxArray([NyxData(0)]))]);
    Check(LReply.Field('value').Field('value').AsInteger = 2,
      'Authenticated exact numeric predicate context');
    Check(Call('nyx_collections', [NyxField('mode', NyxData('bindings')),
      NyxField('owner', NyxData('work-table'))]).Field('effective').ToJSON = LBindings,
      'Query-only edit retains paged columns, parent, scope and selection');
    Apply(LPatch, LOldRevision, True);
    Check(Source = LChanged, 'Stale query edit retains accepted source');
    History('undo');
    Check(Source = LBefore, 'One paired Undo restores exact pre-group source');
    Check(not Call('nyx_collections', [NyxField('mode', NyxData('query')),
      NyxField('owner', NyxData('work-table')), NyxField('source', NyxData('effective'))])
      .Field('query').Field('defined').AsBoolean, 'Undo restores original empty query');
    History('redo');
    Check(Source = LChanged, 'One paired Redo restores exact query/row source');
    Save('nyx.generated.view.pas', LChanged);
    Save('workspace.txt', GWorkspace);
    Build('browser', 'application');
    Build('lcl', 'application');
    Build('browser', 'view');
    Build('lcl', 'view');
    Check(Call('nyx_session', [], False).Field('revision').AsInteger =
      LPrimary.Field('revision').AsInteger, 'Primary project revision remains unchanged');
    Save('receipt.json', NyxObject([NyxField('checks', NyxData(GChecks)),
      NyxField('revision', NyxData(GRevision)), NyxField('workspace', NyxData(GWorkspace)),
      NyxField('browserUIQualified', NyxData(False))]).ToJSON);
    WriteLn('PASS ', GChecks, ' authenticated query/source/history/build checks');
  finally
    LPatch := nil;
    try
      GClient.Close;
    finally
      GClient.Free;
    end;
  end;
end.
