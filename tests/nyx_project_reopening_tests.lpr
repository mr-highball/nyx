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
program nyx_project_reopening_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses Interfaces, Classes, SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.source, nyx.model,
  nyx.studio.projects, nyx.studio.projectstore, nyx.studio.session,
  nyx.studio.sourcejobs, nyx.studio.sourcecompilation.native,
  nyx.studio.directories, nyx.studio.outputs, nyx.studio.buildexecutor,
  nyx.studio.builds, nyx.studio.sourceprojection, nyx.test.projection,
  nyx.test.projectopening;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxModel.Create('Compiler-backed project reopening: ' + AReason);
  end;
  Inc(GChecks);
end;

function ReadText(const APath: TNyxText): TNyxText;
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try

    if (LStream.Size < 1) or (LStream.Size > 4 * 1024 * 1024) then
    begin
      raise ENyxModel.Create('Qualification input exceeds its byte budget');
    end;
    SetLength(LBytes, LStream.Size);
    LStream.ReadBuffer(LBytes[0], Length(LBytes));
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
  LStream := TFileStream.Create(APath, fmCreate or fmShareExclusive);
  try

    if Length(LBytes) <> 0 then
    begin
      LStream.WriteBuffer(LBytes[0], Length(LBytes));
    end;
  finally
    LStream.Free;
  end;
end;

procedure Pump(ACommands: TNyxSourceCommands);
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    CheckSynchronize(1);

    if not ACommands.Busy then
    begin
      Exit;
    end;
  until GetTickCount64 - LStarted > 180000;
  raise ENyxModel.Create('Owned compiler/project command exceeded its wait budget');
end;

procedure Run;
const
  CUnfinished: TNyxText = 'Still writing this idea 🚀.';
  CNewer: TNyxText = 'New notes while checking 𐐷.';
  CTitleLine: TNyxText = 'LDocument.Title := ''Handwritten notebook'';';
  CThrowLine: TNyxText = 'raise Exception.Create(''Saved construction failed'');';
var
  LDirectories: TNyxStudioDirectories;
  LTools: TNyxDataValue;
  LProfile: TNyxOutputConfiguration;
  LExecutor: TNyxBuildExecutor;
  LBuild: INyxSourceProjectionBuild;
  LSession: TNyxStudioSession;
  LCommands: TNyxSourceCommands;
  LStore: TNyxProjectStore;
  LSource: TNyxText;
  LPair: TNyxProjectPair;
  LBefore: TNyxProjectPair;
  LCurrent: TNyxText;
  LRevision: TNyxText;
  LRemote: TNyxText;
  LPath: TNyxText;
  LRefused: Boolean;
  LFailure: TNyxProjectPair;
  LIndex: Integer;
begin

  if (ParamCount <> 3) or DirectoryExists(ParamStr(3)) or FileExists(ParamStr(3)) then
  begin
    raise ENyxModel.Create('Supply repository, toolchain JSON and a NEW owned runtime');
  end;
  LDirectories := TNyxStudioDirectories.ForRepository(ParamStr(1)).RunningIn(ParamStr(3));
  LTools := TNyxDataValue.ParseJSON(ReadText(ParamStr(2)));
  LSource := ReadText(LDirectories.SourceRoot + 'tests/fixtures/nyx.projection.fixture.pas');
  LProfile := TNyxOutputConfiguration.Create;
  LExecutor := nil;
  LCommands := nil;
  LSession := nil;
  LStore := nil;
  try
    LProfile.SetField('fpc', LTools.Field('FPC').AsText);
    LProfile.SetField('pas2js', LTools.Field('PAS2JS').AsText);
    LProfile.SetField('runtime', LTools.Field('PAS2JS_RUNTIME').AsText);
    LExecutor := TNyxBuildExecutor.Create(LDirectories, LProfile.Encode);
    LBuild := LExecutor.ProjectSource(LSource, NyxPascalUnit(NyxCompanionUnitName(LSource)),
      btNativeLCL, spcChecked);
    Check((LBuild.Projection.State = spsExecuted) and
      (LBuild.Projection.Design = ExpectedNyxProjectionDesign),
      'real FPC helpers, loop, resources and Unicode reproduce independent expected meaning');
    Inc(GChecks, RunNyxProjectOpeningChecks(LBuild.Projection));
    FreeAndNil(LExecutor);

    LSession := TNyxStudioSession.Create;
    LBefore := LSession.ProjectSnapshot;
    LCommands := TNyxSourceCommands.Create(LSession, nil);
    LCommands.UseCompiler(NewNyxNativeSourceCompiler(LDirectories, LProfile.Encode,
      TNyxCompilerLimits.Default));
    LPair := NyxProjectPair(ExpectedNyxProjectionDesign, LSource);
    LPair.Pending := True;
    LPair.Draft := CUnfinished;
    LPair.DraftBase := LSource;
    LCommands.OpenProject(LPair);
    Check(LCommands.Busy and (LSession.Source = LBefore.Source),
      'ordinary queue retains current project while actual FPC checking runs');
    Pump(LCommands);
    Check((LCommands.State = nssApplied) and
      (EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LPair)),
      'configured Open compiles accepted source while carrying invalid unfinished Pascal as data');
    Check(not LSession.CanUndo and not LSession.CanRedo, 'opening starts fresh project history');

    LStore := TNyxProjectStore.Create(LDirectories.RuntimeRoot + PathDelim + 'paired');
    LRefused := False;
    try
      LStore.SaveProject('notebook', '', LPair, LRevision, LRemote);
    except
      on ENyxSource do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'general save receives no authorization from serialized expression source');
    Check(LStore.SaveCapturedProject('notebook', '', LPair,
      LSession.AcceptedSourceCheckpoint, LRevision, LRemote),
      'live admitted checkpoint saves the complete executed companion');
    LCurrent := LStore.ReadProject('notebook', LRevision);
    Check(LCurrent = EncodeNyxProject(LPair), 'adjacent file round trip retains exact unfinished text');
    LSession.LoadProject(LBefore);
    LCommands.OpenProject(DecodeNyxProject(LCurrent));
    Pump(LCommands);
    Check((LCommands.State = nssApplied) and (LSession.Source = LSource),
      'ordinary saved Open recompiles after the original live owner was retired');

    { Simulate interruption after complete journal commit and before all member
      replacements. Read must return the committed input without pretending its
      disk bytes carry fresh execution authority. No listener is involved. }
    LPath := IncludeTrailingPathDelimiter(LStore.Root) + 'notebook' + PathDelim;
    WriteText(LPath + 'pending.nyxproject', EncodeNyxProject(LPair));
    WriteText(LPath + 'design.nyx', LBefore.Design);
    LCurrent := LStore.ReadProject('notebook', LRevision);
    Check((LCurrent = EncodeNyxProject(LPair)) and
      (ReadText(LPath + 'design.nyx') = LBefore.Design),
      'executed journal is queryable input; read leaves partial member bytes untouched');
    Check(not LStore.CompleteCapturedRecovery('notebook', 'stale', LPair,
      LSession.AcceptedSourceCheckpoint, LRemote), 'changed journal revision refuses publication');
    Check(FileExists(LPath + 'pending.nyxproject') and
      (ReadText(LPath + 'design.nyx') = LBefore.Design), 'refusal retains complete journal and member bytes');
    Check(LStore.CompleteCapturedRecovery('notebook', LRevision, LPair,
      LSession.AcceptedSourceCheckpoint, LRemote), 'exact admitted owner finishes journal recovery');
    Check(not FileExists(LPath + 'pending.nyxproject') and
      (LStore.ReadProject('notebook', LRemote) = EncodeNyxProject(LPair)),
      'completed recovery exposes one whole paired project');
    Check(not LStore.CompleteCapturedRecovery('notebook', 'stale', LPair,
      LSession.AcceptedSourceCheckpoint, LRemote), 'ordinary file revision changes also refuse acknowledgement');

    LSession.LoadProject(LBefore);
    LSession.SetTitle('History before a failed constructor');
    LSession.Undo;
    LCurrent := EncodeNyxProject(LSession.ProjectSnapshot);
    LFailure := LPair;
    LIndex := Pos(CTitleLine, LSource);
    Check(LIndex > 0, 'owned failure fixture has an exact typed UTF-8 replacement boundary');
    LFailure.Source := Copy(LSource, 1, LIndex - 1) + CThrowLine +
      Copy(LSource, LIndex + Length(CTitleLine), Length(LSource));
    LCommands.OpenProject(LFailure);
    Pump(LCommands);
    Check((LCommands.State = nssRejected) and
      (Pos('constructor failed during execution', LCommands.Message) > 0),
      'actual constructor failure is a visible project refusal');
    Check((EncodeNyxProject(LSession.ProjectSnapshot) = LCurrent) and LSession.CanRedo,
      'failed project execution retains every accepted file, draft and existing history');

    LSession.LoadProject(LBefore);
    LSession.SetTitle('Local history before checking');
    LSession.Undo;
    LCommands.OpenProject(LPair);
    LSession.SetSourceDraft(CNewer);
    LCurrent := EncodeNyxProject(LSession.ProjectSnapshot);
    Pump(LCommands);
    Check((LCommands.State = nssStale) and
      (EncodeNyxProject(LSession.ProjectSnapshot) = LCurrent) and LSession.CanRedo,
      'actual compiler completion cannot overwrite newer typing or existing paired history');

    LSession.LoadProject(LBefore);
    LCommands.OpenProject(LPair);
    LCommands.Cancel;
    Check((LCommands.State = nssCancelled) and
      (EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore)),
      'cancelling actual project compilation retains the old project');
  finally
    { Controller retirement revokes delivery, cancels and drains its real native
      operation lifetimes before the accepted session can be freed. }
    LCommands.Free;
    LSession.Free;
    LStore.Free;
    LBuild := nil;
    LExecutor.Free;
    LProfile.Free;
  end;
end;

begin
  try
    Run;
    WriteLn('PASS ', GChecks, ' compiler-backed saved project admission/queue/journal checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.ClassName, ': ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
