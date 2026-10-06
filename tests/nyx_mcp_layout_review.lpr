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
program nyx_mcp_layout_review;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.studio.builds, nyx.text, nyx.data, nyx.test.mcp.client;

var
  GClient: TNyxMCPTestClient;
  GRevision: Integer;
  GDirectory: TNyxText;
  GIdentity: TNyxText;

{ Fixture and accepted-source boundaries preserve UTF-8 bytes. The supplied
  service configuration remains private; this author never prints its token or
  claims/replaces a user's project through an operator endpoint. }
function ReadBytes(const APath: TNyxText): TNyxText;
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Result, LStream.Size);

    if Length(Result) > 0 then
    begin
      LStream.ReadBuffer(Result[1], Length(Result));
    end;
  finally
    LStream.Free;
  end;
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

function Call(const ATool: TNyxText; const AArguments: TNyxDataValue): TNyxDataValue;
var
  LReply: TNyxDataValue;
  LIndex: Integer;
begin
  LReply := GClient.Tool(ATool, AArguments);

  if LReply.Field('isError').AsBoolean then
  begin
    raise Exception.Create('Semantic layout request refused: ' + LReply.ToJSON);
  end;
  Result := LReply.Field('structuredContent');
  for LIndex := 0 to Result.Count - 1 do
  begin

    if Result.Key(LIndex) = 'revision' then
    begin
      GRevision := Result.Field('revision').AsInteger;
    end;
  end;
end;

{ Bounded accepted-source windows must all belong to the same immutable
  revision. Export is compilation evidence, never a replacement document. }
procedure ExportSource;
var
  LValue: TNyxDataValue;
  LSource: TNyxText;
  LLine: Integer;
  LIndex: Integer;
  LRevision: Integer;
begin
  LSource := '';
  LLine := 1;
  LRevision := GRevision;
  repeat
    LValue := Call('nyx_source', NyxObject([
      NyxField('line', NyxData(LLine)), NyxField('count', NyxData(80))]));

    if (GRevision <> LRevision) or (LValue.Field('lines').Count = 0) then
    begin
      raise Exception.Create('Accepted layout source changed during bounded export');
    end;
    for LIndex := 0 to LValue.Field('lines').Count - 1 do
    begin
      LSource := LSource + LValue.Field('lines').Item(LIndex).AsText + #10;
    end;
    Inc(LLine, LValue.Field('lines').Count);
  until LLine > LValue.Field('totalLines').AsInteger;
  Save('nyx.generated.view.pas', LSource);
end;

{ Each request has one recorded identity and receipt. Poll the same live job;
  a timeout leaves its receipt available and never resubmits a mutation. }
procedure Compile(const ATarget, AOutput: TNyxText);
var
  LReceipt: TNyxDataValue;
  LStatus: TNyxDataValue;
  LStarted: QWord;
begin
  LReceipt := Call('nyx_build', NyxObject([
    NyxField('mode', NyxData('request')),
    NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData(GIdentity + '-build-' + ATarget)),
    NyxField('target', NyxData(ATarget)), NyxField('scope', NyxData('application')),
    NyxField('outputID', NyxData(AOutput))]));
  Save(ATarget + '-receipt.json', LReceipt.ToJSON);
  LStarted := GetTickCount64;
  repeat
    LStatus := Call('nyx_build', NyxObject([
      NyxField('mode', NyxData('status')), NyxField('job', LReceipt.Field('job')),
      NyxField('severity', NyxData('error')), NyxField('limit', NyxData(2))]));
    Save(ATarget + '-build.json', LStatus.ToJSON);

    if NyxBuildJobTerminal(ParseNyxBuildJobState(LStatus.Field('state').AsText)) then
    begin
      Break;
    end;

    if GetTickCount64 - LStarted > 180000 then
    begin
      raise Exception.Create('Compiler is still running; inspect the retained receipt');
    end;
    Sleep(100);
  until False;

  if (LStatus.Field('state').AsText <> 'succeeded') or
    not LStatus.Field('currentSource').AsBoolean then
  begin
    raise Exception.Create('Layout compiler did not qualify the accepted pair');
  end;
  WriteLn('PASS actual MCP ', ATarget, ' application compiler');
end;

{ Query only the new policy controls when that maintained root is present.
  Rejected enum/scalar patches must leave the same paired revision/history;
  inspection never replaces a project or silently coerces behavior strings. }
procedure InspectPolicies;
var
  LOutline: TNyxDataValue;
  LIndex: Integer;
  LPresent: Boolean;
  LValue: TNyxDataValue;
  LReply: TNyxDataValue;
  LRevision: Integer;
  LUndo: Boolean;
  LRedo: Boolean;

  { Metadata must describe the same absent-property behavior as both adapters.
    A spacer's implicit weight is one; an ordinary label's weight is zero.
    Inspecting the default must not materialize it in the accepted document. }
  procedure InspectWeight(const AID, ADefault: TNyxText);
  var
    LMetadata: TNyxDataValue;
    LProperty: TNyxDataValue;
  begin
    LMetadata := Call('nyx_node', NyxObject([
      NyxField('id', NyxData(AID)), NyxField('keys', NyxArray([NyxData('flex')]))]));
    Save(AID + '-default.json', LMetadata.ToJSON);

    if LMetadata.Field('properties').Count <> 1 then
    begin
      raise Exception.Create('Weight inspection must return one bounded property');
    end;
    LProperty := LMetadata.Field('properties').Item(0);

    if (LProperty.Field('type').AsText <> 'integer') or
      (LProperty.Field('default').AsText <> ADefault) or
      (LProperty.Field('value').Kind <> ndNull) then
    begin
      raise Exception.Create('Weight metadata disagrees with the absent-property contract');
    end;
  end;

  procedure Refuse(const AKey: TNyxText; const AValue: TNyxDataValue);
  begin
    LReply := GClient.Tool('nyx_transaction', NyxObject([
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData(GIdentity + '-refuse-' + AKey)),
      NyxField('operations', NyxArray([NyxObject([
        NyxField('op', NyxData('update')), NyxField('id', NyxData('policy-alignment')),
        NyxField('properties', NyxObject([NyxField(AKey, AValue)]))])]))]));
    Save('refused-' + AKey + '.json', LReply.ToJSON);

    if not LReply.Field('isError').AsBoolean then
    begin
      raise Exception.Create('The closed policy property accepted an invalid argument');
    end;
    LValue := Call('nyx_session', NyxObject([]));

    if (GRevision <> LRevision) or (LValue.Field('canUndo').AsBoolean <> LUndo) or
      (LValue.Field('canRedo').AsBoolean <> LRedo) then
    begin
      raise Exception.Create('A refused policy patch changed the accepted revision/history');
    end;
  end;
begin
  LOutline := Call('nyx_outline', NyxObject([
    NyxField('scope', NyxData('pages')), NyxField('limit', NyxData(10))]));
  LPresent := False;
  for LIndex := 0 to LOutline.Field('items').Count - 1 do
  begin
    LPresent := LPresent or (LOutline.Field('items').Item(LIndex).Field('id').AsText = 'policy-review');
  end;

  if not LPresent then
  begin
    Exit;
  end;
  LValue := Call('nyx_node', NyxObject([
    NyxField('id', NyxData('policy-alignment')),
    NyxField('keys', NyxArray([NyxData('flow-wrap'), NyxData('cross-alignment'),
      NyxData('justification')]))]));
  Save('policy-properties.json', LValue.ToJSON);

  if LValue.Field('properties').Count <> 3 then
  begin
    raise Exception.Create('Policy metadata must expose all three bounded choices');
  end;
  for LIndex := 0 to LValue.Field('properties').Count - 1 do
  begin

    if LValue.Field('properties').Item(LIndex).Field('type').AsText <> 'enum' then
    begin
      raise Exception.Create('A closed flow choice was published as untyped text');
    end;
  end;
  InspectWeight('policy-spacer', '1');
  InspectWeight('policy-small', '0');
  LRevision := GRevision;
  LValue := Call('nyx_session', NyxObject([]));
  LUndo := LValue.Field('canUndo').AsBoolean;
  LRedo := LValue.Field('canRedo').AsBoolean;
  Refuse('flow-wrap', NyxData('sometimes'));
  Refuse('cross-alignment', NyxData(17));
  Refuse('height-sizing', NyxData(True));
  WriteLn('PASS bounded policy metadata and exact-revision typed refusals');
end;

var
  LValue: TNyxDataValue;
  LOperations: TNyxDataValue;
  LGUID: TGUID;
begin
  GClient := nil;
  try

    if (ParamCount <> 4) or not ((ParamStr(4) = 'compose') or (ParamStr(4) = 'inspect')) then
    begin
      raise Exception.Create('Supply local MCP config, operations fixture, owned source directory, compose/inspect');
    end;
    GDirectory := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(3)));
    ForceDirectories(GDirectory);
    CreateGUID(LGUID);
    GIdentity := 'layout-review-' + GUIDToString(LGUID);
    GClient := TNyxMCPTestClient.Create(ParamStr(1), 'Scooty layout review qualification');
    LValue := Call('nyx_session', NyxObject([]));

    if ParamStr(4) = 'compose' then
    begin
      { JSON is the explicit semantic wire boundary. Integer/Boolean fixture
        values retain their types; one related group admits one paired Undo. }
      LOperations := TNyxDataValue.ParseJSON(ReadBytes(ParamStr(2)));
      Save('transaction-identity.txt', GIdentity);
      Call('nyx_transaction', NyxObject([
        NyxField('expectedRevision', NyxData(GRevision)),
        NyxField('operationId', NyxData(GIdentity)), NyxField('operations', LOperations)]));
    end;
    LValue := Call('nyx_outline', NyxObject([
      NyxField('parent', NyxData('layout-review')), NyxField('limit', NyxData(10))]));

    if LValue.Field('total').AsInteger <> 6 then
    begin
      raise Exception.Create('The maintained layout review must retain its six exact children');
    end;
    Save('outline.json', LValue.ToJSON);
    LValue := Call('nyx_node', NyxObject([
      NyxField('id', NyxData('layout-first-column')),
      NyxField('keys', NyxArray([NyxData('flex'), NyxData('height')]))]));
    Save('properties.json', LValue.ToJSON);
    InspectPolicies;
    ExportSource;
    LValue := Call('nyx_build', NyxObject([NyxField('mode', NyxData('outputs'))]));
    Compile('browser', LValue.Field('outputID').AsText);
    Compile('lcl', LValue.Field('outputID').AsText);
    GClient.Close;
    WriteLn('PASS bounded layout inspection/export and both compilers / revision ', GRevision);
  except
    on LException: Exception do
    begin
      WriteLn(StdErr, 'FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
  GClient.Free;
end.
