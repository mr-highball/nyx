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
program nyx_project_files_native;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.model, nyx.codec,
  nyx.source, nyx.pascal.project, nyx.studio.projects, nyx.studio.projectstore,
  nyx.studio.session, nyx.studio.directories, nyx.studio.outputs,
  nyx.studio.buildexecutor, nyx.studio.builds, nyx.studio.projectinputs,
  nyx.studio.sourceprojection, nyx.test.projectfiles, nyx.test.compiled.runtime,
  nyx.test.pascal.project;

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: String);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function ReadText(const APath: TNyxText): TNyxText;
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try
    SetLength(LBytes, LStream.Size);

    if Length(LBytes) > 0 then
    begin
      LStream.ReadBuffer(LBytes[0], Length(LBytes));
    end;
    Result := NyxDecodeUTF8(LBytes);
  finally
    LStream.Free;
  end;
end;

procedure WriteText(const APath, AText: TNyxText);
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LBytes := NyxEncodeUTF8(AText);
  LStream := TFileStream.Create(APath, fmCreate);
  try

    if Length(LBytes) > 0 then
    begin
      LStream.WriteBuffer(LBytes[0], Length(LBytes));
    end;
  finally
    LStream.Free;
  end;
end;

procedure Run;
var
  LDirectories: TNyxStudioDirectories;
  LTools: TNyxDataValue;
  LProfile: TNyxOutputConfiguration;
  LExecutor: TNyxBuildExecutor;
  LStore: TNyxProjectStore;
  LSession: TNyxStudioSession;
  LDocument: TNyxDocument;
  LSource: TNyxText;
  LInputs: INyxPascalProject;
  LChanged: INyxPascalProject;
  LCommented: INyxPascalProject;
  LFirst: INyxPascalProjectBuild;
  LSecond: INyxPascalProjectBuild;
  LComment: INyxPascalProjectBuild;
  LCold: INyxPascalProjectBuild;
  LBrowser: INyxPascalProjectBuild;
  LPair: TNyxProjectPair;
  LRead: TNyxProjectPair;
  LWorkspace: TNyxSourceWorkspace;
  LResolved: TNyxProjectPair;
  LCheckpoint: TNyxSourceCheckpoint;
  LRevision: TNyxText;
  LNextRevision: TNyxText;
  LRemote: TNyxText;
  LPacket: TNyxText;
  LBefore: TNyxText;
  LRefused: Boolean;
  LNewBody: TNyxText;
  LReceipt: TNyxDataValue;
  LJournalPath: TNyxText;
begin

  if (ParamCount <> 3) or DirectoryExists(ParamStr(3)) or FileExists(ParamStr(3)) then
  begin
    raise Exception.Create('Supply repository, toolchain JSON and a NEW owned runtime');
  end;
  LDirectories := TNyxStudioDirectories.ForRepository(ParamStr(1)).RunningIn(ParamStr(3));
  LTools := TNyxDataValue.ParseJSON(ReadText(ParamStr(2)));
  LProfile := TNyxOutputConfiguration.Create;
  LExecutor := nil;
  LStore := nil;
  LSession := nil;
  LDocument := nil;
  LWorkspace := nil;
  try
    LProfile.SetField('fpc', LTools.Field('FPC').AsText);
    LProfile.SetField('pas2js', LTools.Field('PAS2JS').AsText);
    LProfile.SetField('runtime', LTools.Field('PAS2JS_RUNTIME').AsText);
    LExecutor := TNyxBuildExecutor.Create(LDirectories, LProfile.Encode);
    LDocument := CompiledRuntimeDocument('First plan', LSource);
    FreeAndNil(LDocument);
    LInputs := CompiledRuntimeProject(LSource, 'First plan');
    LNewBody := 'function ProjectPlan: TNyxText;' + #10 + 'begin' + #10 +
      '  Result := ''Second plan 🚀 𐐷'';' + #10 + 'end;' + #10;
    LChanged := LInputs.WithInclude(NyxPascalInclude('plan-values.inc'), LNewBody);
    LCommented := LChanged.WithInclude(NyxPascalInclude('plan-values.inc'),
      LNewBody + #10 + '{ A thoughtful implementation note. }' + #10);
    WriteLn('Constructing first project');
    LFirst := LExecutor.ProjectSource(LInputs, btNativeLCL, spcChecked);
    WriteLn('Constructing changed helper project');
    LSecond := LExecutor.ProjectSource(LChanged, btNativeLCL, spcChecked);
    WriteLn('Constructing same-design helper project');
    LComment := LExecutor.ProjectSource(LCommented, btNativeLCL, spcChecked);
    Inc(GChecks, RunNyxProjectFilesChecks(LFirst, LSecond, LComment));

    LPair := NyxProjectPair(LSecond.Projection.Design, LSecond.Projection.Source);
    LPair.Files := TNyxPascalProjectFiles.Capture(LSecond.Inputs);
    LPair.Pending := True;
    LPair.Draft := '{ Unfinished independent buffer 🚀';
    LPair.DraftBase := 'An older baseline 𐐷';
    LSession := TNyxStudioSession.Create;
    LSession.AdoptProjectedProject(LPair, LSecond.Projection);
    LCheckpoint := LSession.AcceptedSourceCheckpoint;
    LStore := TNyxProjectStore.Create(LDirectories.Projects);
    LJournalPath := IncludeTrailingPathDelimiter(LDirectories.Projects) +
      'owned-files' + PathDelim + 'pending.nyxproject';
    Check(LStore.SaveCapturedProject('owned-files', '', LPair, LCheckpoint,
      LRevision, LRemote), 'captured ordinary project save succeeds');
    LPacket := EncodeNyxProject(LPair);
    FreeAndNil(LStore);
    LStore := TNyxProjectStore.Create(LDirectories.Projects);
    Check(LStore.ReadProject('owned-files', LNextRevision) = LPacket,
      'cold store reopening preserves every supporting file and main draft');
    Check(LNextRevision = LRevision, 'unchanged cold bytes retain optimistic revision');
    LRead := DecodeNyxProject(LStore.ReadProject('owned-files', LNextRevision));
    Check(LRead.Files.Open(LRead.Source).Input(2).Text = LNewBody,
      'the stored include carries exact supplementary Unicode source');
    LBefore := LStore.ReadProject('owned-files', LNextRevision);
    LRead.Files := TNyxPascalProjectFiles.Capture(LComment.Inputs);
    LRefused := False;
    try
      LStore.SaveCapturedProject('owned-files', LRevision, LRead, LCheckpoint,
        LNextRevision, LRemote);
    except
      on ENyxProjectConflict do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LStore.ReadProject('owned-files', LNextRevision) = LBefore),
      'same main/design cannot save a changed inventory beside an older checkpoint');

    LRead := DecodeNyxProject(LBefore);
    WriteLn('Constructing exact cold recovered project');
    LCold := LExecutor.ProjectSource(LRead.Files.Open(LRead.Source), btNativeLCL, spcChecked);
    Check((LCold.Projection.State = spsExecuted) and
      (LCold.Projection.Design = LRead.Design), 'cold recovered files reconstruct exact meaning');
    AdmitNyxProjectedProject(LRead, LCold.Projection, LDocument, LWorkspace, LResolved);
    Check(EncodeNyxProject(LResolved) = LPacket, 'cold compiler admission keeps the entire pending pair');
    FreeAndNil(LDocument);
    FreeAndNil(LWorkspace);

    { An interrupted write leaves a complete journal. Reading it returns exact
      data after literal admission refuses, with no fabricated execution flag. }
    WriteText(LJournalPath, LPacket);
    Check(LStore.ReadProject('owned-files', LNextRevision) = LPacket,
      'interrupted multi-file journal remains complete compiler-required input');
    Check(FileExists(LJournalPath),
      'reading does not clear an unadmitted multi-file journal');
    Check(LStore.CompleteCapturedRecovery('owned-files', LNextRevision,
      LRead, LCheckpoint, LRevision), 'exact current journal completion admits its checkpoint');
    Check(not FileExists(LJournalPath),
      'exact admitted completion clears only its owned journal');
    Check(LStore.ReadProject('owned-files', LNextRevision) = LPacket,
      'completed recovery retains exact supporting/main/draft bytes');

    WriteLn('Compiling matching browser project');
    LBrowser := LExecutor.ProjectSource(LRead.Files.Open(LRead.Source), btBrowser, spcChecked);
    Check(LBrowser.Projection.State = spsCompiled,
      'matching browser project compiles without claiming construction');
    LRefused := False;
    try
      LSession.AdoptProjectedProject(LRead, LBrowser.Projection);
    except
      on ENyxProjectConflict do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (EncodeNyxProject(LSession.ProjectSnapshot) = LPacket),
      'compiled-only browser result cannot publish accepted project meaning');
    LReceipt := NyxObject([
      NyxField('first', NyxData(LFirst.Reference.Name)),
      NyxField('second', NyxData(LSecond.Reference.Name)),
      NyxField('comment', NyxData(LComment.Reference.Name)),
      NyxField('cold', NyxData(LCold.Reference.Name)),
      NyxField('browser', NyxData(LBrowser.Reference.Name)),
      NyxField('checks', NyxData(GChecks))]);
    WriteText(IncludeTrailingPathDelimiter(ParamStr(3)) + 'project-files-receipt.json',
      LReceipt.ToJSON);
  finally
    LWorkspace.Free;
    LDocument.Free;
    LSession.Free;
    LStore.Free;
    LBrowser := nil;
    LCold := nil;
    LComment := nil;
    LSecond := nil;
    LFirst := nil;
    LExecutor.Free;
    LProfile.Free;
  end;
end;

begin
  try
    Run;
    WriteLn('PASS ', GChecks, ' actual compiled project-file ownership/history/store checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.ClassName, ': ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
