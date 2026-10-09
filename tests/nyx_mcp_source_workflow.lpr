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
  Classes, SysUtils, Base64, nyx.text, nyx.data, nyx.studio.reviews,
  nyx.studio.builds, nyx.test.mcp.client;

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
procedure Build(ATarget: TNyxBuildTarget; const AOutput: TNyxText);
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
    NyxField('operationId', NyxData('source-workshop-' + LTarget)),
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
  Save('build-' + LTarget + '.json', LStatus.ToJSON);
  Check((LStatus.Field('state').AsText = 'succeeded') and
    LStatus.Field('currentSource').AsBoolean, 'Exact authored ' + LTarget + ' job succeeds');
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
    GClient.Free;
  end;
end.
