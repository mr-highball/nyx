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

program nyx_mcp_designer_review;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, MD5, nyx.text, nyx.data, nyx.studio.agents, nyx.studio.reviews,
  nyx.test.mcp.client;

var
  GClient: TNyxMCPTestClient;
  GReview: TNyxReviewRef;
  GRevision: Integer;
  GDirectory: TNyxText;
  GIdentity: TNyxText;
  GSourceHash: TNyxText;

{ Presence/activity is expected to advance during an agent journey. All published
  primary content/history/navigation fields must remain byte-for-byte unchanged. }
function PrimaryFrame(const ASession: TNyxDataValue): TNyxText;
const
  CFields: array[0..9] of TNyxText = ('revision', 'permission', 'title',
    'selection', 'view', 'pages', 'components', 'pendingDraft', 'canUndo', 'canRedo');
var
  LIndex: Integer;
begin
  Result := '';
  for LIndex := Low(CFields) to High(CFields) do
  begin
    Result := Result + ASession.Field(CFields[LIndex]).ToJSON + #10;
  end;
end;

{ Explicit UTF-8 file boundary. The tool never prints credentials, imports an
  operator project or starts a listener. Its ephemeral review is owned by this
  real MCP transport and is retired before that transport closes. }
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
begin
  LReply := GClient.Tool(ATool, NyxWithReview(AArguments, GReview));

  if LReply.Field('isError').AsBoolean then
  begin
    raise Exception.Create('Designer semantic request refused: ' + LReply.ToJSON);
  end;
  Result := LReply.Field('structuredContent');

  if NyxAgentHas(Result, 'revision') then
  begin
    GRevision := Result.Field('revision').AsInteger;
  end;
end;

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
      raise Exception.Create('Designer source changed during bounded export');
    end;
    for LIndex := 0 to LValue.Field('lines').Count - 1 do
    begin
      LSource := LSource + LValue.Field('lines').Item(LIndex).AsText + #10;
    end;
    Inc(LLine, LValue.Field('lines').Count);
  until LLine > LValue.Field('totalLines').AsInteger;
  Save('nyx.generated.view.pas', LSource);
  GSourceHash := MD5Print(MD5Buffer(LSource[1], Length(LSource)));
end;

procedure Compile(const ATarget, AOutput: TNyxText);
var
  LReceipt: TNyxDataValue;
  LStatus: TNyxDataValue;
  LStarted: QWord;
  LState: TNyxText;
  LRevision: Integer;
begin
  LRevision := GRevision;
  LReceipt := Call('nyx_build', NyxObject([
    NyxField('mode', NyxData('request')),
    NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData(GIdentity + '-' + ATarget)),
    NyxField('target', NyxData(ATarget)), NyxField('scope', NyxData('application')),
    NyxField('outputID', NyxData(AOutput))]));
  Save(ATarget + '-receipt.json', LReceipt.ToJSON);
  LStarted := GetTickCount64;
  repeat
    LStatus := Call('nyx_build', NyxObject([
      NyxField('mode', NyxData('status')), NyxField('job', LReceipt.Field('job')),
      NyxField('severity', NyxData('warning')), NyxField('limit', NyxData(10))]));
    Save(ATarget + '-status.json', LStatus.ToJSON);
    LState := LStatus.Field('state').AsText;

    if (LState = 'succeeded') or (LState = 'failed') then
    begin
      Break;
    end;

    if GetTickCount64 - LStarted > 60000 then
    begin
      raise Exception.Create('Designer compiler observation timed out; receipt retained');
    end;
    Sleep(200);
  until False;

  if (LState <> 'succeeded') or not LStatus.Field('currentSource').AsBoolean or
    not LStatus.Field('currentOutput').AsBoolean or
    (LStatus.Field('revision').AsInteger <> LRevision) or
    (LStatus.Field('currentRevision').AsInteger <> LRevision) or
    (LStatus.Field('sourceFingerprint').AsText <> GSourceHash) then
  begin
    raise Exception.Create('Designer companion did not compile at its exact accepted pair');
  end;
end;

var
  LValue: TNyxDataValue;
  LBefore: TNyxText;
  LPrimaryFrame: TNyxText;
  LGUID: TGUID;
begin
  GClient := nil;
  GReview := NyxActiveWorkspace;
  try

    if ParamCount <> 3 then
    begin
      raise Exception.Create('Supply local MCP config, operations fixture and owned source directory');
    end;
    GDirectory := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(3)));
    ForceDirectories(GDirectory);
    CreateGUID(LGUID);
    GIdentity := 'designer-' + GUIDToString(LGUID);
    GClient := TNyxMCPTestClient.Create(ParamStr(1), 'Scooty native designer qualification');
    try
      LValue := Call('nyx_session', NyxObject([]));
      LBefore := LValue.ToJSON;
      LPrimaryFrame := PrimaryFrame(LValue);
      LValue := Call('nyx_reviews', NyxObject([
        NyxField('mode', NyxData('create')), NyxField('expectedRevision', NyxData(GRevision)),
        NyxField('operationId', NyxData(GIdentity + '-create')),
        NyxField('label', NyxData('Native designer controls')), NyxField('base', NyxData('empty'))]));
      GReview := NyxReview(LValue.Field('review').AsText);
      Call('nyx_session', NyxObject([]));
      LValue := Call('nyx_transaction', NyxObject([
        NyxField('expectedRevision', NyxData(GRevision)),
        NyxField('operationId', NyxData(GIdentity + '-compose')),
        NyxField('operations', TNyxDataValue.ParseJSON(ReadBytes(ParamStr(2))))]));
      Save('transaction.json', LValue.ToJSON);
      ExportSource;
      LValue := Call('nyx_build', NyxObject([NyxField('mode', NyxData('outputs'))]));
      Compile('browser', LValue.Field('outputID').AsText);
      Compile('lcl', LValue.Field('outputID').AsText);
      { A declined/stale cleanup is not replaced with a reset of any project. }
      LValue := GClient.Tool('nyx_reviews', NyxObject([
        NyxField('mode', NyxData('discard')), NyxField('review', NyxData(GReview.ID)),
        NyxField('expectedRevision', NyxData(GRevision)),
        NyxField('operationId', NyxData(GIdentity + '-discard'))]));

      if LValue.Field('isError').AsBoolean then
      begin
        raise Exception.Create('Exact designer review cleanup refused: ' + LValue.ToJSON);
      end;
      GReview := NyxActiveWorkspace;
      LValue := Call('nyx_session', NyxObject([]));
      Save('primary-before.json', LBefore);
      Save('primary-after.json', LValue.ToJSON);

      if PrimaryFrame(LValue) <> LPrimaryFrame then
      begin
        raise Exception.Create('Designer review changed the primary editor frame');
      end;
      WriteLn('PASS semantic grouped designer review, bounded export and both application compilers');
    finally
      GClient.Close;
    end;
  except
    on LException: Exception do
    begin
      WriteLn(StdErr, 'FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
  GClient.Free;
end.
