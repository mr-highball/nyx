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

program nyx_project_file_seed;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.editing, nyx.studio.reviews,
  nyx.studio.buildjobs, nyx.studio.builds, nyx.test.mcp.client;

var
  GClient: TNyxMCPTestClient;
  GReview: TNyxReviewRef;
  GRevision: Integer;
  GDirectory: TNyxText;

{ This maintained seed uses only bounded semantic tools in an owned review.
  Its temporary authority retires on Close; no ordinary user pair is replaced. }
function Call(const ATool: TNyxText; const AArguments: TNyxDataValue): TNyxDataValue;
var
  LReply: TNyxDataValue;
begin
  LReply := GClient.Tool(ATool, NyxWithReview(AArguments, GReview));

  if LReply.Field('isError').AsBoolean then
  begin
    raise Exception.Create('Project seed refused: ' + ATool);
  end;
  Result := LReply.Field('structuredContent');
end;

procedure Save(const AName, AText: TNyxText);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(GDirectory + AName, fmCreate);
  try

    if Length(AText) > 0 then
    begin
      LStream.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LStream.Free;
  end;
end;

function ExportPart(const APart: TNyxText): TNyxText;
var
  LReply: TNyxDataValue;
  LParts: TNyxStrings;
  LOffset: Integer;
begin
  LParts := TNyxStrings.Create;
  try
    LOffset := 0;
    repeat
      LReply := Call('nyx_project', NyxObject([
        NyxField('mode', NyxData('export')), NyxField('expectedRevision', NyxData(GRevision)),
        NyxField('part', NyxData(APart)), NyxField('offset', NyxData(LOffset)),
        NyxField('count', NyxData(4096))]));
      LParts.Add(LReply.Field('text').AsText);
      LOffset := LReply.Field('nextOffset').AsInteger;
    until LOffset = LReply.Field('total').AsInteger;
    Result := LParts.Join;
  finally
    LParts.Free;
  end;
end;

procedure Build(const ATarget, AOutput, ASource: TNyxText);
var
  LJob: TNyxText;
  LReply: TNyxDataValue;
  LStarted: QWord;
begin
  LJob := Call('nyx_build', NyxObject([
    NyxField('mode', NyxData('request')), NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData('file-seed-' + ATarget)), NyxField('outputID', NyxData(AOutput)),
    NyxField('target', NyxData(ATarget)), NyxField('scope', NyxData('application'))])).Field('job').AsText;
  LStarted := GetTickCount64;
  repeat
    LReply := Call('nyx_build', NyxObject([
      NyxField('mode', NyxData('status')), NyxField('job', NyxData(LJob)),
      NyxField('severity', NyxData('error')), NyxField('limit', NyxData(3))]));

    if GetTickCount64 - LStarted > 120000 then
    begin
      raise Exception.Create('Project seed compiler did not reach a terminal state');
    end;
    Sleep(100);
  until NyxBuildJobTerminal(ParseNyxBuildJobState(LReply.Field('state').AsText));
  Save('build-' + ATarget + '.json', LReply.ToJSON);

  if (LReply.Field('state').AsText <> 'succeeded') or
    not LReply.Field('currentSource').AsBoolean or
    (LReply.Field('sourceFingerprint').AsText <> NyxBuildFingerprint(ASource)) then
  begin
    raise Exception.Create('Project seed exact-source compilation failed: ' + ATarget);
  end;
end;

procedure Run;
var
  LPrimary: Integer;
  LReply: TNyxDataValue;
  LSource: TNyxText;
  LOutput: TNyxText;
  LAnchor: TNyxText;
  LOffset: Integer;
begin
  LPrimary := Call('nyx_session', NyxObject([])).Field('revision').AsInteger;
  LReply := Call('nyx_reviews', NyxObject([
    NyxField('mode', NyxData('create')), NyxField('expectedRevision', NyxData(LPrimary)),
    NyxField('operationId', NyxData('project-file-seed')), NyxField('base', NyxData('empty')),
    NyxField('label', NyxData('Project file workshop'))]));
  GReview := NyxReview(LReply.Field('review').AsText);
  GRevision := LReply.Field('session').Field('revision').AsInteger;
  LReply := Call('nyx_transaction', NyxObject([
    NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData('compose-project-file-workshop')),
    NyxField('operations', NyxArray([
      NyxObject([NyxField('op', NyxData('title')), NyxField('value', NyxData('A thoughtful notebook'))]),
      NyxObject([NyxField('op', NyxData('create')), NyxField('kind', NyxData('page')),
        NyxField('id', NyxData('home')), NyxField('root', NyxData('page'))]),
      NyxObject([NyxField('op', NyxData('create')), NyxField('kind', NyxData('page')),
        NyxField('id', NyxData('settings')), NyxField('root', NyxData('page'))]),
      NyxObject([NyxField('op', NyxData('create')), NyxField('kind', NyxData('card')),
        NyxField('id', NyxData('note-card')), NyxField('root', NyxData('component'))]),
      NyxObject([NyxField('op', NyxData('create')), NyxField('kind', NyxData('heading')),
        NyxField('id', NyxData('note-heading')), NyxField('parent', NyxData('note-card')),
        NyxField('properties', NyxObject([NyxField('text', NyxData('Keep a good idea')),
          NyxField('part', NyxData('caption'))]))]),
      NyxObject([NyxField('op', NyxData('create')), NyxField('kind', NyxData('memo')),
        NyxField('id', NyxData('note-body')), NyxField('parent', NyxData('note-card')),
        NyxField('properties', NyxObject([NyxField('text', NyxData('Your notes')),
          NyxField('placeholder', NyxData('Write something worth keeping.')),
          NyxField('part', NyxData('body'))]))]),
      NyxObject([NyxField('op', NyxData('instance')), NyxField('id', NyxData('home-note')),
        NyxField('component', NyxData('note-card')), NyxField('parent', NyxData('home'))]),
      NyxObject([NyxField('op', NyxData('instance')), NyxField('id', NyxData('settings-note')),
        NyxField('component', NyxData('note-card')), NyxField('parent', NyxData('settings'))])
    ]))]));
  GRevision := LReply.Field('revision').AsInteger;
  LSource := ExportPart('source');
  LAnchor := 'implementation' + #10;
  LOffset := Pos(LAnchor, LSource);

  if LOffset < 1 then
  begin
    raise Exception.Create('Owned generated unit has no implementation anchor');
  end;
  LReply := Call('nyx_pascal', NyxObject([
    NyxField('mode', NyxData('edit-unit')), NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData('craft-project-file-companion')),
    NyxField('changes', NyxArray([NyxObject([
      NyxField('offset', NyxData(NyxTextScalarCount(Copy(LSource, 1, LOffset - 1)))),
      NyxField('expected', NyxData(LAnchor)),
      NyxField('replacement', NyxData(LAnchor + #10 +
        '{ A handwritten helper stays beside every saved design. }' + #10 +
        'function NotebookHint: TNyxText;' + #10 +
        'begin' + #10 + '  Result := ''Keep an idea for tomorrow.'';' + #10 +
        'end;' + #10))])]))]));
  GRevision := LReply.Field('revision').AsInteger;
  LSource := ExportPart('source');
  Save('project.nyxproject', ExportPart('project'));
  Save('design.nyx', ExportPart('design'));
  Save('nyx.generated.view.pas', LSource);
  LOutput := Call('nyx_build', NyxObject([NyxField('mode', NyxData('outputs'))])).Field('outputID').AsText;
  Build('browser', LOutput, LSource);
  Build('lcl', LOutput, LSource);
  GReview := NyxActiveWorkspace;

  if Call('nyx_session', NyxObject([])).Field('revision').AsInteger <> LPrimary then
  begin
    raise Exception.Create('Primary revision changed during owned project composition');
  end;
end;

begin
  GClient := nil;
  GReview := NyxActiveWorkspace;
  try
    try

      if (ParamCount <> 2) or DirectoryExists(ParamStr(2)) or FileExists(ParamStr(2)) then
      begin
        raise Exception.Create('Supply current MCP config and a new owned seed directory');
      end;
      GDirectory := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(2)));
      ForceDirectories(GDirectory);
      GClient := TNyxMCPTestClient.Create(ParamStr(1), 'Scooty project files');
      Run;
      WriteLn('PASS semantic two-page reusable project, crafted Pascal and both compiler jobs');
    except
      on LException: Exception do
      begin
        WriteLn(StdErr, LException.ClassName, ': ', LException.Message);
        ExitCode := 1;
      end;
    end;
  finally

    if GClient <> nil then
    begin
      GClient.Close;
    end;
    GClient.Free;
  end;
end.
