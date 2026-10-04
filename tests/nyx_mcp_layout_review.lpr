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
  Classes, SysUtils, nyx.text, nyx.data, nyx.test.mcp.client;

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

    if LStatus.Field('state').AsText <> 'running' then
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
