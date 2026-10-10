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

program nyx_mcp_source_workflow;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, Base64, nyx.text, nyx.bytes, nyx.data, nyx.studio.reviews,
  nyx.studio.builds, nyx.studio.buildjobs, nyx.studio.projects,
  nyx.studio.session, nyx.test.mcp.client;

const
  CInitialCaption: TNyxText = 'A place for your next idea';
  CAuthoredCaption: TNyxText = 'A thoughtfully crafted workspace';
  CImplementation: TNyxText = 'implementation' + #10;
  CThemeHelper: TNyxText = #10 +
    '{ Application theme helpers stay outside the managed view. }' + #10 +
    'type' + #10 +
    '  TWorkshopTheme = class' + #10 +
    '  public' + #10 +
    '    class function Caption: TNyxText;' + #10 +
    '  end;' + #10 + #10 +
    'class function TWorkshopTheme.Caption: TNyxText;' + #10 +
    'begin' + #10 +
    '  Result := ''A thoughtfully crafted workspace'';' + #10 +
    'end;' + #10 + #10 +
    'function WorkshopCaption: TNyxText;' + #10 +
    'begin' + #10 +
    '  Result := TWorkshopTheme.Caption;' + #10 +
    'end;' + #10;

var
  GClient: TNyxMCPTestClient;
  GReview: TNyxReviewRef;
  GRevision: Integer;
  GChecks: Integer;
  GEvidence: TNyxText;

procedure Save(const AName: TNyxText; const ABytes: RawByteString); forward;

{ Failed protocol operations report only their tool name. Credentials, user
  pairs and arbitrary server response bodies never enter console diagnostics. }
function Call(const ATool: TNyxText; const AArguments: TNyxDataValue): TNyxDataValue;
var
  LReply: TNyxDataValue;
begin
  LReply := GClient.Tool(ATool, NyxWithReview(AArguments, GReview));

  if LReply.Field('isError').AsBoolean then
  begin
    Save('refusal-' + ATool + '.json', LReply.ToJSON);
    raise Exception.Create('Source workflow refused: ' + ATool);
  end;
  Result := LReply.Field('structuredContent');
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Source workflow: ' + AReason);
  end;
  Inc(GChecks);
end;

{ Receipts are private qualification artifacts. The stream owns its handle;
  byte strings retain their exact UTF-8/PNG representation at this boundary. }
procedure Save(const AName: TNyxText; const ABytes: RawByteString);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(GEvidence + AName, fmCreate);
  try

    if Length(ABytes) > 0 then
    begin
      LStream.WriteBuffer(ABytes[1], Length(ABytes));
    end;
  finally
    LStream.Free;
  end;
end;

{ Small owned reviews fit one bounded window. Never page/export the user's
  accepted unit to qualify this workflow. The server owns scalar coordinates. }
function Source: TNyxText;
var
  LWindow: TNyxDataValue;
begin
  LWindow := Call('nyx_pascal', NyxObject([
    NyxField('mode', NyxData('unit')),
    NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('line', NyxData(1)), NyxField('count', NyxData(4096))]));
  Check(LWindow.Field('nextOffset').AsInteger = LWindow.Field('total').AsInteger,
    'Small review fits one bounded accepted-source window');
  Result := LWindow.Field('text').AsText;
end;

function OffsetOf(const ASource, AAnchor: TNyxText): Integer;
var
  LPosition: Integer;
  LCursor: Integer;
  LScalar: Integer;
begin
  LPosition := Pos(AAnchor, ASource);
  Check(LPosition > 0, 'Exact source anchor exists');
  LCursor := 1;
  Result := 0;
  while LCursor < LPosition do
  begin

    if not NyxNextScalar(ASource, LCursor, LScalar) then
    begin
      raise Exception.Create('Malformed source Unicode');
    end;
    Inc(Result);
  end;
end;

{ Semantic jobs capture the review's exact pair. Poll bounded error diagnostics;
  timeouts fail without retrying a mutation or claiming successful execution. }
procedure Build(ATarget: TNyxBuildTarget; const AOutput: TNyxText;
  const ASeries: TNyxText = 'source-workshop'; const ASource: TNyxText = '');
var
  LTarget: TNyxText;
  LJob: TNyxText;
  LStatus: TNyxDataValue;
  LStarted: QWord;
begin
  LTarget := NyxBuildTargetName(ATarget);
  LJob := Call('nyx_build', NyxObject([
    NyxField('mode', NyxData('request')),
    NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData(ASeries + '-' + LTarget)),
    NyxField('outputID', NyxData(AOutput)),
    NyxField('target', NyxData(LTarget)),
    NyxField('scope', NyxData('application'))])).Field('job').AsText;
  LStarted := GetTickCount64;
  repeat
    LStatus := Call('nyx_build', NyxObject([
      NyxField('mode', NyxData('status')), NyxField('job', NyxData(LJob)),
      NyxField('severity', NyxData('error')), NyxField('limit', NyxData(3))]));

    if GetTickCount64 - LStarted > 120000 then
    begin
      raise Exception.Create('Source workshop compiler deadline exceeded');
    end;
    Sleep(100);
  until NyxBuildJobTerminal(ParseNyxBuildJobState(LStatus.Field('state').AsText));

  if ASeries = 'source-workshop' then
  begin
    Save('build-' + LTarget + '.json', LStatus.ToJSON);
  end
  else
  begin
    Save('build-' + ASeries + '-' + LTarget + '.json', LStatus.ToJSON);
  end;
  Check((LStatus.Field('state').AsText = 'succeeded') and
    LStatus.Field('currentSource').AsBoolean, 'Exact authored ' + LTarget + ' job succeeds');

  if ASource <> '' then
  begin
    Check(LStatus.Field('sourceFingerprint').AsText = NyxBuildFingerprint(ASource),
      'Imported compiler job retains its exact accepted Pascal bytes');
  end;
  WriteLn('Compiled source workshop: ', LTarget);
  Flush(Output);
end;

{ Query only the affected published property. This proves paired design
  admission/restoration without exporting the complete document or source. }
procedure CheckCaption(const AExpected: TNyxText);
var
  LNode: TNyxDataValue;
begin
  LNode := Call('nyx_node', NyxObject([
    NyxField('id', NyxData('workshop-title')),
    NyxField('keys', NyxArray([NyxData('text')]))]));
  Check((LNode.Field('revision').AsInteger = GRevision) and
    (LNode.Field('properties').Count = 1) and
    (LNode.Field('properties').Item(0).Field('value').AsText = AExpected),
    'Published heading matches the source pair');
end;

{ Export only this owned review, never the primary project. Scalar windows
  remain pinned to one revision; joining them locally does not turn a semantic
  query into a whole-document response. The packet includes exact pending/base. }
function ReviewProjectPacket: TNyxText;
var
  LParts: TNyxStrings;
  LReply: TNyxDataValue;
  LOffset: Integer;
begin
  LParts := TNyxStrings.Create;
  try
    LOffset := 0;
    repeat
      LReply := Call('nyx_project', NyxObject([
        NyxField('mode', NyxData('export')), NyxField('expectedRevision', NyxData(GRevision)),
        NyxField('part', NyxData('project')), NyxField('offset', NyxData(LOffset)),
        NyxField('count', NyxData(4096))]));
      LParts.Add(LReply.Field('text').AsText);
      LOffset := LReply.Field('nextOffset').AsInteger;
    until LOffset = LReply.Field('total').AsInteger;
    Result := LParts.Join;
  finally
    LParts.Free;
  end;
end;

{ Installed tool qualification stays in this transport's independent review.
  The English input is ordinary public Studio authoring. No primary replacement,
  shadow fixture attachment, browser editor automation or target configuration
  is needed. New jobs use distinct receipts and retain their own evidence. }
procedure ImportReviewProject(const AOutput: TNyxText);
var
  LAuthor: TNyxStudioSession;
  LPair: TNyxProjectPair;
  LPacket: TNyxText;
  LBefore: TNyxText;
  LImport: TNyxText;
  LTicket: TNyxText;
  LArgs: TNyxDataValue;
  LReply: TNyxDataValue;
  LPreview: TNyxDataValue;
  LIndex: Integer;
  LStart: Integer;
  LScalar: Integer;
  LCount: Integer;
  LOffset: Integer;
  LSerial: Integer;
begin
  Check(GReview.ID <> '', 'Project import requires an explicit owned review');
  LBefore := ReviewProjectPacket;
  LAuthor := TNyxStudioSession.Create;
  try
    LAuthor.AddPage;
    LAuthor.Document.Title := 'Project import review';
    LPair := LAuthor.ProjectSnapshot;
  finally
    LAuthor.Free;
  end;
  LPacket := EncodeNyxProject(LPair);
  Save('project-import-input.nyx', LPacket);
  LArgs := NyxObject([NyxField('mode', NyxData('begin-import')),
    NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData('project-import-reserve')),
    NyxField('bytes', NyxData(NyxUTF8ByteCount(LPacket)))]);
  LReply := Call('nyx_project', LArgs);
  Check(Call('nyx_project', LArgs).ToJSON = LReply.ToJSON, 'Installed reserve retry is exact');
  LImport := LReply.Field('projectImport').Field('import').AsText;
  LIndex := 1;
  LOffset := 0;
  LSerial := 0;
  while LIndex <= Length(LPacket) do
  begin
    LStart := LIndex;
    LCount := 0;
    while (LIndex <= Length(LPacket)) and (LCount < 4096) do
    begin

      if not NyxNextScalar(LPacket, LIndex, LScalar) then
      begin
        raise Exception.Create('Malformed project review input');
      end;
      Inc(LCount);
    end;
    Inc(LSerial);
    LArgs := NyxObject([NyxField('mode', NyxData('append-import')),
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData('project-import-chunk-' + IntToStr(LSerial))),
      NyxField('import', NyxData(LImport)), NyxField('offset', NyxData(LOffset)),
      NyxField('text', NyxData(Copy(LPacket, LStart, LIndex - LStart)))]);
    LReply := Call('nyx_project', LArgs);
    Check(Call('nyx_project', LArgs).ToJSON = LReply.ToJSON, 'Installed chunk retry retains exact input');
    LOffset := LReply.Field('projectImport').Field('nextOffset').AsInteger;
  end;
  Check(ReviewProjectPacket = LBefore, 'Installed staging preserves the accepted review pair');
  LArgs := NyxObject([NyxField('mode', NyxData('review-import')),
    NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData('project-import-review')),
    NyxField('import', NyxData(LImport)), NyxField('resolution', NyxData('match'))]);
  LReply := Call('nyx_project', LArgs);
  Check(Call('nyx_project', LArgs).ToJSON = LReply.ToJSON, 'Installed review retry retains its ticket');
  LTicket := LReply.Field('projectImport').Field('reviewID').AsText;
  Check(LReply.Field('projectImport').Field('candidate').Field('pages').AsInteger = 2,
    'Installed bounded review describes the two-page input');
  LArgs := NyxObject([NyxField('mode', NyxData('apply')),
    NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData('project-import-apply')),
    NyxField('import', NyxData(LImport)), NyxField('reviewID', NyxData(LTicket))]);
  LReply := Call('nyx_project', LArgs);
  GRevision := LReply.Field('revision').AsInteger;
  Check(Call('nyx_project', LArgs).ToJSON = LReply.ToJSON, 'Installed apply retry adds no history');
  Check(ReviewProjectPacket = LPacket, 'Installed admission retains the exact authored project pair');
  Build(btBrowser, AOutput, 'project-import', LPair.Source);
  Build(btNativeLCL, AOutput, 'project-import', LPair.Source);
  LPreview := GClient.Tool('nyx_preview', NyxWithReview(NyxObject([
    NyxField('expectedRevision', NyxData(GRevision)), NyxField('view', NyxData('home')),
    NyxField('width', NyxData(900)), NyxField('height', NyxData(600)),
    NyxField('capture', NyxData(True))]), GReview));
  Check(not LPreview.Field('isError').AsBoolean and (LPreview.Field('content').Count = 3),
    'Selective imported review preview returns its rendered PNG');
  Save('project-import-preview.png',
    DecodeStringBase64(LPreview.Field('content').Item(2).Field('data').AsText));
  LReply := Call('nyx_history', NyxObject([
    NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData('undo-project-import')),
    NyxField('direction', NyxData('undo'))]));
  GRevision := LReply.Field('revision').AsInteger;
  Check(ReviewProjectPacket = LBefore, 'One installed Undo restores the exact previous pair');
  LReply := Call('nyx_history', NyxObject([
    NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData('redo-project-import')),
    NyxField('direction', NyxData('redo'))]));
  GRevision := LReply.Field('revision').AsInteger;
  Check(ReviewProjectPacket = LPacket, 'One installed Redo restores the exact imported pair');
  LReply := Call('nyx_history', NyxObject([
    NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData('restore-review-before-import')),
    NyxField('direction', NyxData('undo'))]));
  GRevision := LReply.Field('revision').AsInteger;
  Check(ReviewProjectPacket = LBefore, 'Owned review returns to its original workshop');
end;

procedure Run;
var
  LPrimary: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LBefore: TNyxText;
  LCaption: TNyxText;
  LPreview: TNyxDataValue;
  LOutput: TNyxText;
  LArguments: TNyxDataValue;
begin
  { Reviews belong to this authenticated transport. Teardown retires them even
    on failure; neither creation nor grouped source Undo touches the primary. }
  LPrimary := Call('nyx_session', NyxObject([]));
  LReceipt := Call('nyx_reviews', NyxObject([
    NyxField('mode', NyxData('create')),
    NyxField('expectedRevision', LPrimary.Field('revision')),
    NyxField('operationId', NyxData('source-workshop')),
    NyxField('base', NyxData('empty')), NyxField('label', NyxData('Source workshop'))]));
  GReview := NyxReview(LReceipt.Field('review').AsText);
  GRevision := LReceipt.Field('session').Field('revision').AsInteger;
  LReceipt := Call('nyx_transaction', NyxObject([
    NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData('compose-source-workshop')),
    NyxField('operations', NyxArray([
      NyxObject([NyxField('op', NyxData('create')), NyxField('kind', NyxData('page')),
        NyxField('id', NyxData('workshop')), NyxField('root', NyxData('page'))]),
      NyxObject([NyxField('op', NyxData('create')), NyxField('kind', NyxData('heading')),
        NyxField('id', NyxData('workshop-title')), NyxField('parent', NyxData('workshop')),
        NyxField('properties', NyxObject([NyxField('text', NyxData(CInitialCaption))]))])]))]));
  GRevision := LReceipt.Field('revision').AsInteger;
  LBefore := Source;
  LCaption := '''' + CInitialCaption + '''';
  LArguments := NyxObject([
    NyxField('mode', NyxData('edit-unit')),
    NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData('author-theme-and-view')),
    NyxField('changes', NyxArray([
      NyxObject([NyxField('offset', NyxData(OffsetOf(LBefore, CImplementation))),
        NyxField('expected', NyxData(CImplementation)),
        NyxField('replacement', NyxData(CImplementation + CThemeHelper))]),
      NyxObject([NyxField('offset', NyxData(OffsetOf(LBefore, LCaption))),
        NyxField('expected', NyxData(LCaption)),
        NyxField('replacement', NyxData('''' + CAuthoredCaption + ''''))])]))]);
  LReceipt := Call('nyx_pascal', LArguments);
  GRevision := LReceipt.Field('revision').AsInteger;
  Check(LReceipt.Field('sourceEdit').Field('changes').AsInteger = 2,
    'Class/helper/view change is one semantic group');
  Check(Call('nyx_pascal', LArguments).ToJSON = LReceipt.ToJSON,
    'Exact authenticated retry creates no duplicate history step');
  Check(Pos(CThemeHelper, Source) > 0, 'Handwritten class/helper survives admission');
  CheckCaption(CAuthoredCaption);
  LOutput := Call('nyx_build', NyxObject([
    NyxField('mode', NyxData('outputs'))])).Field('outputID').AsText;
  Build(btBrowser, LOutput);
  Build(btNativeLCL, LOutput);
  LPreview := GClient.Tool('nyx_preview', NyxWithReview(NyxObject([
    NyxField('expectedRevision', NyxData(GRevision)), NyxField('view', NyxData('workshop')),
    NyxField('width', NyxData(900)), NyxField('height', NyxData(600)),
    NyxField('capture', NyxData(True))]), GReview));
  Check(not LPreview.Field('isError').AsBoolean and
    (LPreview.Field('content').Count = 3), 'Selective semantic preview returns its PNG');
  Save('semantic-preview.png',
    DecodeStringBase64(LPreview.Field('content').Item(2).Field('data').AsText));
  LReceipt := Call('nyx_history', NyxObject([
    NyxField('direction', NyxData('undo')), NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData('undo-theme-and-view'))]));
  GRevision := LReceipt.Field('revision').AsInteger;
  Check(Source = LBefore, 'One paired Undo restores the exact source');
  CheckCaption(CInitialCaption);
  ImportReviewProject(LOutput);
  CheckCaption(CInitialCaption);
  GReview := NyxActiveWorkspace;
  Check(Call('nyx_session', NyxObject([])).Field('revision').AsInteger =
    LPrimary.Field('revision').AsInteger, 'Primary revision remains unchanged');
end;

begin
  GClient := nil;
  GReview := NyxActiveWorkspace;
  try
    try

      if ParamCount <> 2 then
      begin
        raise Exception.Create('Supply existing local MCP config and NEW private evidence directory');
      end;
      GEvidence := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(2)));
      Check(not DirectoryExists(GEvidence), 'Evidence directory is new');
      ForceDirectories(GEvidence);
      GClient := TNyxMCPTestClient.Create(ParamStr(1), 'Scooty source workshop');
      Run;
      WriteLn('PASS ', GChecks, ' authenticated source workflow checks');
    except
      on LException: Exception do
      begin
        WriteLn(StdErr, LException.ClassName, ': ', LException.Message);
        ExitCode := 1;
      end;
    end;
  finally
    try

      if GClient <> nil then
      begin
        { A plain TObject.Free does not retire a remote MCP transport/review.
          Explicit DELETE releases private authority; Close is idempotent. }
        GClient.Close;
      end;
    finally
      GClient.Free;
    end;
  end;
end.
