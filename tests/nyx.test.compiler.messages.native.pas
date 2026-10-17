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

unit nyx.test.compiler.messages.native;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text;

{ A fresh owned runtime and maintained fixture compiler exercise the production
  semantic engine while suspended. No socket, enrollment refresh of an existing
  project, browser, capture or protected document is involved. }
function RunNyxLiveCompilerMessages(const ARoot, ACompiler, ARuntime: TNyxText): Integer;

implementation

uses Classes, SysUtils, Windows, nyx.data, nyx.studio.mcp, nyx.studio.outputs,
  nyx.studio.compiler, nyx.studio.editorbuild, nyx.studio.buildjobs,
  nyx.studio.builds, nyx.studio.projects, nyx.studio.agents,
  nyx.test.compiler.fixture, nyx.studio.buildexecutor, nyx.model, nyx.codec;

type
  { A deliberately failing synchronous receiver captures a live fixture process
    handle before raising. It owns only that observation handle; the executor
    must retire its own compiler and input lease without a receiver cleanup call. }
  TRejectingCompilerProgress = class(TInterfacedObject, INyxCompilerProgress)
  private
    FJobs: TNyxText;
  public
    Handle: THandle;
    Calls: Integer;
    constructor Create(const AJobs: TNyxText);
    destructor Destroy; override;
    procedure Publish(const AReport: INyxCompilerReport);
  end;

constructor TRejectingCompilerProgress.Create(const AJobs: TNyxText);
begin
  inherited Create;
  FJobs := AJobs;
end;

destructor TRejectingCompilerProgress.Destroy;
begin

  if Handle <> 0 then
  begin
    CloseHandle(Handle);
  end;
  inherited Destroy;
end;

procedure TRejectingCompilerProgress.Publish(const AReport: INyxCompilerReport);
var
  LEntry: TSearchRec;
  LText: TStringList;
  LMarker: TNyxText;
begin
  Inc(Calls);
  LText := TStringList.Create;
  try

    if FindFirst(FJobs + 'job-*', faDirectory, LEntry) = 0 then
    begin
      try
        repeat
          LMarker := NyxCompilerFixtureMarkerPath(FJobs + LEntry.Name, cfrCompiler);

          if FileExists(LMarker) then
          begin
            LText.LoadFromFile(LMarker);
            Handle := OpenProcess(SYNCHRONIZE, False, StrToInt(Trim(LText.Text)));
            Break;
          end;
        until FindNext(LEntry) <> 0;
      finally
        SysUtils.FindClose(LEntry);
      end;
    end;

    if (Handle = 0) or (WaitForSingleObject(Handle, 0) <> WAIT_TIMEOUT) or
      (AReport = nil) or (AReport.Count <> 1) then
    begin
      raise ENyxModel.Create('The owned progress receiver did not capture a living compiler');
    end;
    raise ENyxModel.Create('Owned progress receiver refusal');
  finally
    LText.Free;
  end;
end;

procedure Save(const APath, AText: TNyxText);
var
  LFile: TFileStream;
begin
  LFile := TFileStream.Create(APath, fmCreate);
  try

    if Length(AText) > 0 then
    begin
      LFile.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LFile.Free;
  end;
end;

function RunNyxLiveCompilerMessages(const ARoot, ACompiler, ARuntime: TNyxText): Integer;
const
  COwner: TNyxText = 'compiler-messages-owner';
var
  LEngine: TNyxStudioMCP;
  LProfile: TNyxOutputConfiguration;
  LSeed: TNyxAgentSession;
  LPair: TNyxProjectPair;
  LReport: INyxCompilerReport;
  LToken: TNyxText;
  LJobs: TNyxText;
  LDirectory: TNyxText;
  LStatus: TNyxDataValue;
  LFirst: TNyxDataValue;
  LBefore: TNyxDataValue;
  LValue: TNyxDataValue;
  LJob: TNyxBuildJobRef;
  LHandle: THandle;
  LText: TStringList;
  LEntry: TSearchRec;
  LStarted: QWord;
  LChecks: Integer;
  LCase: Integer;
  LFirstText: TNyxText;
  LExecutor: TNyxBuildExecutor;
  LDocument: TNyxDocument;
  LReceiver: TRejectingCompilerProgress;
  LReceiverOwner: INyxCompilerProgress;
  LRefused: Boolean;

  procedure Check(AValue: Boolean; const AReason: TNyxText);
  begin

    if not AValue then
    begin
      raise Exception.Create(AReason);
    end;
    Inc(LChecks);
  end;

  function Observe: TNyxDataValue;
  begin
    Result := LEngine.EditorExchange(LToken, NyxObject([
      NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))]));
  end;

  function Status: TNyxDataValue;
  begin
    Result := LEngine.InvokeBuild(COwner, 'Scooty', NyxCompilerStatus(LJob));
  end;

  procedure WaitMessages(ACount: Integer);
  begin
    LStarted := GetTickCount64;
    repeat
      LStatus := Status;

      if LStatus.Field('diagnostics').Field('total').AsInteger >= ACount then
      begin
        Break;
      end;

      if NyxBuildJobTerminal(ParseNyxBuildJobState(LStatus.Field('state').AsText)) or
        (GetTickCount64 - LStarted > 3000) then
      begin
        Break;
      end;
      Sleep(5);
    until False;
    Check(LStatus.Field('diagnostics').Field('total').AsInteger >= ACount,
      'The semantic job exposes complete compiler messages before terminal completion');
  end;

  procedure CaptureCompiler;
  var
    LMarker: TNyxText;
  begin
    LStarted := GetTickCount64;
    LDirectory := '';
    repeat

      if FindFirst(LJobs + 'job-*', faDirectory, LEntry) = 0 then
      begin
        try
          repeat
            LMarker := NyxCompilerFixtureMarkerPath(LJobs + LEntry.Name, cfrCompiler);

            if FileExists(LMarker) then
            begin
              LText.LoadFromFile(LMarker);
              LHandle := OpenProcess(SYNCHRONIZE, False, StrToInt(Trim(LText.Text)));

              if (LHandle <> 0) and (WaitForSingleObject(LHandle, 0) = WAIT_TIMEOUT) then
              begin
                LDirectory := LJobs + LEntry.Name + '/fixture-status/';
                Break;
              end;

              if LHandle <> 0 then
              begin
                CloseHandle(LHandle);
                LHandle := 0;
              end;
            end;
          until FindNext(LEntry) <> 0;
        finally
          SysUtils.FindClose(LEntry);
        end;
      end;

      if LDirectory <> '' then
      begin
        Break;
      end;

      if GetTickCount64 - LStarted > 5000 then
      begin
        raise Exception.Create('The exact owned compiler marker did not become live');
      end;
      Sleep(5);
    until False;
    Check(LHandle <> 0, 'A retained handle identifies the actual live compiler');
  end;

begin
  LChecks := 0;
  LHandle := 0;
  LEngine := nil;
  LProfile := nil;
  LSeed := nil;
  LExecutor := nil;
  LDocument := nil;
  LText := TStringList.Create;
  try
    Check(not DirectoryExists(ARoot) and not FileExists(ARoot),
      'The live-message consumer owns a fresh runtime');
    ForceDirectories(ARoot + '/src');
    ForceDirectories(ARoot + '/studio');
    LJobs := IncludeTrailingPathDelimiter(ExpandFileName(ARoot)) + 'build/studio/jobs/';
    ForceDirectories(LJobs);
    Save(LJobs + 'fixture.mode', 'messages');
    LProfile := TNyxOutputConfiguration.Create;
    LProfile.SetField('pas2js', ExpandFileName(ACompiler));
    LProfile.SetField('runtime', ExpandFileName(ARuntime));
    LSeed := TNyxAgentSession.Create;
    LPair := LSeed.BuildPair(LSeed.Revision, bsView, 'home');
    LEngine := TNyxStudioMCP.Create(ARoot, 8628, 8629, LProfile.Encode);
    LValue := LEngine.ConnectEditor(NyxObject([NyxField('op', NyxData('claim')),
      NyxField('project', NyxData(EncodeNyxProject(LPair))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
    LToken := LValue.Field('token').AsText;
    LReport := ReadNyxCompilerReport(LPair.Source, LPair.Source, 'accepted.pas',
      'accepted.pas(1,1) Warning: Already accepted');
    LEngine.EditorExchange(LToken, NyxObject([NyxField('op', NyxData('report')),
      NyxField('after', NyxData(0)), NyxField('report', NyxData(LReport.Encode))]));
    for LCase := 1 to 2 do
    begin
      LBefore := Observe;
      LValue := LEngine.InvokeBuild(COwner, 'Scooty',
        NewNyxCompilerRequest.Target(btBrowser).Scope(bsApplication)
          .AtRevision(LBefore.Field('session').Field('revision').AsInteger)
          .Output(NyxBuildOutput(NyxBuildFingerprint(LProfile.Encode)))
          .Operation(NyxBuildOperation('messages-' + TNyxText(IntToStr(LCase)))).Arguments);
      LJob := NyxBuildJob(LValue.Field('job').AsText);
      CaptureCompiler;
      WaitMessages(1);
      Check((WaitForSingleObject(LHandle, 0) = WAIT_TIMEOUT) and
        (LStatus.Field('state').AsText = 'running'), 'A live message is not terminal compilation evidence');
      Check(LStatus.Field('manifest').Count = 0, 'Early diagnostics grant no artifact manifest');
      LFirst := LStatus;
      LFirstText := LFirst.ToJSON;
      Check((LFirst.Field('diagnostics').Field('items').Count = 1) and
        (LFirst.Field('diagnostics').Field('items').Item(0).Field('message').AsText =
          TNyxText('Early message 🌙')), 'Split UTF-8 reaches the exact semantic message');
      Check(LFirst.Field('currentSource').AsBoolean and
        LFirst.Field('diagnostics').Field('items').Item(0).Field('navigable').AsBoolean,
        'The production semantic owner grants the unchanged accepted-source location');
      Check(Observe.Field('compiler').ToJSON = LBefore.Field('compiler').ToJSON,
        'Live messages cannot replace the accepted compiler report');

      if LCase = 1 then
      begin
        Save(LDirectory + 'messages.next', 'continue');
        WaitMessages(2);
        Check(LStatus.Field('diagnostics').Field('items').Item(0).Field('severity').AsText = 'error',
          'New errors take priority through the existing severity query contract');
        Check(LFirst.ToJSON = LFirstText, 'An earlier semantic page remains independent');
        Check(Observe.Field('compiler').ToJSON = LBefore.Field('compiler').ToJSON,
          'Later in-flight messages still grant no accepted report publication');
        Save(LDirectory + 'messages.finish', 'finish');
      end
      else
      begin
        LEngine.InvokeBuild(COwner, 'Scooty', NyxCompilerCancel(LJob,
          LBefore.Field('session').Field('revision').AsInteger,
          NyxBuildOperation('cancel-live-messages')));
      end;
      LStarted := GetTickCount64;
      repeat
        LStatus := Status;

        if NyxBuildJobTerminal(ParseNyxBuildJobState(LStatus.Field('state').AsText)) then
        begin
          Break;
        end;

        if GetTickCount64 - LStarted > 10000 then
        begin
          raise Exception.Create('The same owned compiler did not reach a terminal result');
        end;
        Sleep(5);
      until False;
      Check(WaitForSingleObject(LHandle, 0) = WAIT_OBJECT_0,
        'Terminal semantic status follows the retained compiler handle join');
      Check(Observe.Field('project').AsText = LBefore.Field('project').AsText,
        'Progress and terminal diagnostics preserve the complete design/source/draft pair');

      if LCase = 1 then
      begin
        Check((LStatus.Field('state').AsText = 'failed') and
          (LStatus.Field('diagnostics').Field('total').AsInteger = 2),
          'Actual failing EOF retains both complete diagnostic records');
      end
      else
      begin
        Check((LStatus.Field('state').AsText = 'cancelled') and
          (LStatus.Field('diagnostics').Field('total').AsInteger = 0) and
          (LFirst.ToJSON = LFirstText),
          'Cancellation retires its worker report while earlier immutable pages remain exact');
        Check(Observe.Field('compiler').ToJSON = LBefore.Field('compiler').ToJSON,
          'A cancelled completion never publishes its earlier live report');
      end;
      CloseHandle(LHandle);
      LHandle := 0;
    end;
    LDirectory := IncludeTrailingPathDelimiter(ExpandFileName(ARoot)) + 'receiver-failure/';
    ForceDirectories(LDirectory + 'src');
    ForceDirectories(LDirectory + 'studio');
    ForceDirectories(LDirectory + 'build/studio/jobs');
    Save(LDirectory + 'src/fixture.inc', 'before');
    Save(LDirectory + 'build/studio/jobs/fixture.mode', 'messages');
    LReceiver := TRejectingCompilerProgress.Create(LDirectory + 'build/studio/jobs/');
    LReceiverOwner := LReceiver;
    LExecutor := TNyxBuildExecutor.Create(LDirectory, LProfile.Encode);
    LExecutor.ConfigureProgress(LReceiverOwner);
    LDocument := TNyxCodec.Decode(LPair.Design);
    LRefused := False;
    try
      LExecutor.Build(LDocument, 'browser', 'application', '', LPair.Source).Free;
    except
      on LException: ENyxModel do
      begin
        LRefused := LException.Message = 'Owned progress receiver refusal';
      end;
    end;
    Check(LRefused and (LReceiver.Calls = 1),
      'A receiver failure propagates once without publishing a compiler result');
    Check(WaitForSingleObject(LReceiver.Handle, 0) = WAIT_OBJECT_0,
      'Receiver failure returns only after the exact live compiler joins');
    Save(LDirectory + 'src/fixture.inc', 'after');
    LText.LoadFromFile(LDirectory + 'src/fixture.inc');
    Check(Trim(LText.Text) = 'after',
      'Receiver failure retires input ownership before later source writes');
    Result := LChecks;
  finally
    LDocument.Free;
    LExecutor.Free;
    LReceiverOwner := nil;
    LEngine.Free;
    LSeed.Free;
    LProfile.Free;
    LText.Free;

    if LHandle <> 0 then
    begin
      CloseHandle(LHandle);
    end;
  end;
end;

end.
