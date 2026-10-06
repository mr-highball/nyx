{ Copyright (c) mr-highball. SPDX-License-Identifier: MIT.
  Bounded semantic qualification/export of the isolated container companion.
  An optional exact workspace confines every call to an explicitly composed
  companion. The tool never claims a project, retargets a service or uses an
  editor token. Its authenticated transport is retired before destruction. }
program nyx_container_mcp_review;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.text, nyx.data, nyx.test.mcp.client;

var
  GClient: TNyxMCPTestClient;
  GRevision, GChecks: Integer;
  GWorkspace: TNyxText;

{ Workspace is a protocol context, independent of the observing editor's
  navigation. Copy arguments so intentional refusal calls use the same context. }
function Context(const AArguments: TNyxDataValue): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin

  if GWorkspace = '' then
  begin
    Exit(AArguments);
  end;
  SetLength(LFields, AArguments.Count + 1);
  for LIndex := 0 to AArguments.Count - 1 do
  begin
    LFields[LIndex] := NyxField(AArguments.Key(LIndex), AArguments.Field(AArguments.Key(LIndex)));
  end;
  LFields[AArguments.Count] := NyxField('workspace', NyxData(GWorkspace));
  Result := NyxObject(LFields);
end;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function Call(const ATool: TNyxText; const AArguments: TNyxDataValue): TNyxDataValue;
var
  LReply: TNyxDataValue;
begin
  LReply := GClient.Tool(ATool, Context(AArguments));
  Check(not LReply.Field('isError').AsBoolean,
    'Container semantic operation refused: ' + Copy(LReply.ToJSON, 1, 1024));
  Result := LReply.Field('structuredContent');
  GRevision := Result.Field('revision').AsInteger;
end;

function Source: TNyxText;
var
  LWindow: TNyxDataValue;
  LLine, LIndex, LRevision: Integer;
begin
  Result := '';
  LLine := 1;
  LRevision := GRevision;
  repeat
    LWindow := Call('nyx_source', NyxObject([
      NyxField('line', NyxData(LLine)), NyxField('count', NyxData(80))]));
    Check((GRevision = LRevision) and (LWindow.Field('lines').Count > 0),
      'Source windows must retain one exact accepted revision');
    for LIndex := 0 to LWindow.Field('lines').Count - 1 do
    begin
      Result := Result + LWindow.Field('lines').Item(LIndex).AsText + TNyxText(#10);
    end;
    Inc(LLine, LWindow.Field('lines').Count);
  until LLine > LWindow.Field('totalLines').AsInteger;
end;

procedure History(const ADirection, AIdentity: TNyxText);
begin
  Call('nyx_history', NyxObject([NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData(AIdentity + '-' + IntToStr(GRevision))),
    NyxField('direction', NyxData(ADirection))]));
end;

var
  LBefore, LAfter: TNyxText;
  LReply, LDefinition: TNyxDataValue;
  LRevision: Integer;
  LFile: TFileStream;
begin

  if (ParamCount < 2) or (ParamCount > 3) then
  begin
    raise Exception.Create('Use container MCP review <explicit config.toml> <source directory> [exact workspace]');
  end;

  if ParamCount = 3 then
  begin
    GWorkspace := ParamStr(3);
  end;
  GClient := TNyxMCPTestClient.Create(ParamStr(1), 'Scooty container qualification');
  try
    LReply := Call('nyx_session', NyxObject([]));
    Check((LReply.Field('title').AsText = 'Room for ideas') and
      LReply.Field('canUndo').AsBoolean, 'Only the composed isolated companion is qualified');
    LDefinition := Call('nyx_presentations', NyxObject([NyxField('name', NyxData('compact card'))]));
    Check(LDefinition.Field('definition').Field('container').AsText = 'card space',
      'Bounded MCP inspection publishes the exact query container');
    LBefore := Source;
    Check(Pos('TNyxPresentationCondition.Within(NyxContainer(''card space'')', LBefore) > 0,
      'Generated Pascal uses a typed container condition');
    Check(Pos('.QueryContainer(NyxContainer(''card space''))', LBefore) > 0,
      'Generated Pascal publishes a distinct typed container reference');
    Check(Pos('.Containment(nccWidth)', LBefore) > 0, 'Generated containment uses an enum');
    LRevision := GRevision;
    LReply := GClient.Tool('nyx_transaction', Context(NyxObject([
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData('refuse-container-family')),
      NyxField('operations', NyxArray([NyxObject([
        NyxField('op', NyxData('presentation-define')), NyxField('name', NyxData('compact card')),
        NyxField('container', NyxData(False)), NyxField('widthMinimum', NyxData(0)),
        NyxField('widthMaximum', NyxData(300)), NyxField('heightMinimum', NyxData(0)),
        NyxField('heightMaximum', NyxData(0)), NyxField('orientation', NyxData('any'))])]))])));
    Check(LReply.Field('isError').AsBoolean, 'Wrong container scalar family refuses');
    Call('nyx_session', NyxObject([]));
    Check((GRevision = LRevision) and (Source = LBefore), 'Refusal preserves source and revision');
    LReply := GClient.Tool('nyx_transaction', Context(NyxObject([
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData('refuse-container-bounds')),
      NyxField('operations', NyxArray([
        NyxObject([NyxField('op', NyxData('update')), NyxField('id', NyxData('card-notes')),
          NyxField('properties', NyxObject([NyxField('min-width', NyxData(240))]))]),
        NyxObject([NyxField('op', NyxData('presentation-set')), NyxField('name', NyxData('compact card')),
          NyxField('id', NyxData('card-notes')), NyxField('attribute', NyxData('max-width')),
          NyxField('platform', NyxData('any')), NyxField('value', NyxData(180))])]))])));
    Check(LReply.Field('isError').AsBoolean, 'Conflicting grouped container bounds refuse at the actual MCP boundary');
    Call('nyx_session', NyxObject([]));
    Check((GRevision = LRevision) and (Source = LBefore), 'Bound refusal preserves source and revision');
    History('undo', 'undo-container-composition');
    LAfter := Source;
    Check(Pos('QueryContainer(', LAfter) = 0, 'One Undo removes the complete grouped composition');
    History('redo', 'redo-container-composition');
    Check(Source = LBefore, 'One Redo restores the exact accepted companion bytes');
    ForceDirectories(ParamStr(2));
    LFile := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(2)) + 'nyx.generated.view.pas', fmCreate);
    try
      LFile.WriteBuffer(LBefore[1], Length(LBefore));
    finally
      LFile.Free;
    end;
    WriteLn('PASS ', GChecks, ' bounded container MCP/source/refusal/history checks');
  finally
    try
      GClient.Close;
    finally
      GClient.Free;
    end;
  end;
end.
