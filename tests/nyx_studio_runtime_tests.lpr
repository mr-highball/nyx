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

program nyx_studio_runtime_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.studio.builds, Process, {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  nyx.text, nyx.data, nyx.model, nyx.codec,
  nyx.studio.directories, nyx.studio.release, nyx.studio.server,
  nyx.studio.mcp, nyx.studio.projects, nyx.studio.outputs, nyx.generated.view;

type
  { Trusted persistence consumer, borrowed only by this owned suspended engine.
    Saves go to the same typed runtime profile location used by the real server. }
  TProfileWriter = class
  public
    Directories: TNyxStudioDirectories;
    Saves: Integer;
    procedure Save(const AProfile: TNyxText);
  end;

var
  LRelease: TNyxText;
  LRuntime: TNyxText;
  LDirectories: TNyxStudioDirectories;
  LOriginalDirectories: TNyxStudioDirectories;
  LLegacy: TNyxStudioDirectories;
  LUnused: TNyxStudioDirectories;
  LEngine: TNyxStudioMCP;
  LServer: TNyxStudioServer;
  LProfile: TNyxOutputConfiguration;
  LDocument: TNyxDocument;
  LWriter: TProfileWriter;
  LPair: TNyxProjectPair;
  LClaim: TNyxDataValue;
  LBefore: TNyxDataValue;
  LOutputs: TNyxDataValue;
  LBrowserJob: TNyxDataValue;
  LNativeJob: TNyxDataValue;
  LFrame: TNyxDataValue;
  LManifest: TNyxDataValue;
  LToken: TNyxText;
  LOutputID: TNyxText;
  LChecks: Integer;
  LRefused: Boolean;
  {$IFDEF MSWINDOWS}
  GArtifactPID: DWORD;
  GArtifactWindow: HWND;
  {$ENDIF}

procedure Check(AValue: Boolean; const AReason: TNyxText); forward;

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

{$IFDEF MSWINDOWS}
function ArtifactWindow(AWindow: HWND; AData: LPARAM): BOOL; stdcall;
var
  LProcessID: DWORD;
  LCaption: array[0..255] of WideChar;
begin
  Result := True;
  GetWindowThreadProcessId(AWindow, @LProcessID);

  if (LProcessID = GArtifactPID) and IsWindowVisible(AWindow) then
  begin
    GetWindowTextW(AWindow, LCaption, Length(LCaption));

    if UTF8Encode(UnicodeString(LCaption)) = LDocument.Title then
    begin
      GArtifactWindow := AWindow;
      Result := False;
    end;
  end;
end;

function FindArtifactEdit(AParent: HWND): HWND;
var
  LChild: HWND;
  LClass: array[0..127] of WideChar;
begin
  Result := 0;
  LChild := GetWindow(AParent, GW_CHILD);
  while LChild <> 0 do
  begin
    GetClassNameW(LChild, LClass, Length(LClass));

    if UnicodeUpperCase(UnicodeString(LClass)) = 'EDIT' then
    begin
      Exit(LChild);
    end;
    Result := FindArtifactEdit(LChild);

    if Result <> 0 then
    begin
      Exit;
    end;
    LChild := GetWindow(LChild, GW_HWNDNEXT);
  end;
end;

procedure ExecuteNativeArtifact(const AStatus: TNyxDataValue);
var
  LProcess: TProcess;
  LPath: TNyxText;
  LStarted: QWord;
begin
  LPath := AStatus.Field('artifact').AsText;
  LPath := LDirectories.Jobs + StringReplace(Copy(LPath, 8, MaxInt), '/',
    PathDelim, [rfReplaceAll]);
  LProcess := TProcess.Create(nil);
  try
    LProcess.Executable := LPath;
    LProcess.CurrentDirectory := ExtractFileDir(LPath);
    LProcess.Options := [poNoConsole];
    LProcess.Execute;
    GArtifactPID := LProcess.ProcessID;
    GArtifactWindow := 0;
    LStarted := GetTickCount64;
    repeat
      EnumWindows(@ArtifactWindow, 0);

      if GArtifactWindow <> 0 then
      begin
        Break;
      end;

      if not LProcess.Running or (GetTickCount64 - LStarted > 20000) then
      begin
        raise Exception.Create('Owned compiled native application did not mount its exact document');
      end;
      Sleep(20);
    until False;
    Check(FindArtifactEdit(GArtifactWindow) <> 0,
      'Actual compiled native artifact mounts its authored input control');
    PostMessage(GArtifactWindow, WM_CLOSE, 0, 0);
    LStarted := GetTickCount64;
    while LProcess.Running and (GetTickCount64 - LStarted < 5000) do
    begin
      Sleep(20);
    end;
    Check(not LProcess.Running and (LProcess.ExitStatus = 0),
      'Owned compiled native application closes normally');
  finally

    if LProcess.Running then
    begin
      { Exceptional cleanup affects only this exact owned executable/process. }
      LProcess.Terminate(1);
      LProcess.WaitOnExit;
    end;
    LProcess.Free;
  end;
end;
{$ENDIF}

procedure WriteBytes(const APath, AText: TNyxText);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath, fmCreate);
  try

    if Length(AText) > 0 then
    begin
      LStream.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LStream.Free;
  end;
end;

procedure TProfileWriter.Save(const AProfile: TNyxText);
begin

  if not ForceDirectories(ExtractFileDir(Directories.OutputProfile)) then
  begin
    raise Exception.Create('Cannot prepare owned runtime profile directory');
  end;
  WriteBytes(Directories.OutputProfile, AProfile);
  Inc(Saves);
end;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(LChecks);
  WriteLn('Check ', LChecks, ': ', AReason);
  Flush(Output);
end;

function BuildCall(const AArguments: TNyxDataValue): TNyxDataValue;
begin
  Result := LEngine.EditorExchange(LToken, NyxObject([
    NyxField('op', NyxData('build')), NyxField('after', NyxData(0)),
    NyxField('build', AArguments)])).Field('buildReply');
end;

function Request(const ATarget, AOperation: TNyxText): TNyxDataValue;
begin
  { Target text is a protocol boundary. The live editor/MCP and this consumer
    use the same strict revision/output/scope operation without shell arguments. }
  Result := BuildCall(NyxObject([
    NyxField('mode', NyxData('request')),
    NyxField('expectedRevision', LBefore.Field('session').Field('revision')),
    NyxField('outputID', NyxData(LOutputID)),
    NyxField('operationId', NyxData(AOperation)),
    NyxField('target', NyxData(ATarget)),
    NyxField('scope', NyxData('application'))]));
end;

function WaitForJob(const AJob: TNyxDataValue): TNyxDataValue;
var
  LStarted: QWord;
  LIndex: Integer;
  LMember: TNyxDataValue;
  LRelative: TNyxText;
  LFile: TNyxText;
begin
  LStarted := GetTickCount64;
  repeat
    Result := BuildCall(NyxObject([
      NyxField('mode', NyxData('status')), NyxField('job', AJob.Field('job'))]));

    if NyxBuildJobTerminal(ParseNyxBuildJobState(Result.Field('state').AsText)) then
    begin
      Break;
    end;

    if GetTickCount64 - LStarted > 90000 then
    begin
      raise Exception.Create('Owned compiler job exceeded its qualification budget');
    end;
    Sleep(20);
  until False;
  Check(Result.Field('state').AsText = 'succeeded', 'Actual compiler job succeeded');
  Check(Result.Field('currentSource').AsBoolean and Result.Field('currentOutput').AsBoolean,
    'Compiler result retains exact admitted source/output');
  Check(Result.Field('manifest').Count >= 3, 'Compiler result owns its byte manifest');
  for LIndex := 0 to Result.Field('manifest').Count - 1 do
  begin
    LMember := Result.Field('manifest').Item(LIndex);
    LRelative := LMember.Field('path').AsText;
    Check(Copy(LRelative, 1, 7) = 'builds/', 'Artifact references retain their admitted HTTP route');
    LFile := LDirectories.Jobs + StringReplace(Copy(LRelative, 8, MaxInt), '/',
      PathDelim, [rfReplaceAll]);
    Check(FileExists(LFile), 'Every artifact is physically beneath runtime Jobs');
  end;
  LRelative := Result.Field('compiledSource').AsText;
  LFile := LDirectories.Jobs + StringReplace(Copy(LRelative, 8, MaxInt), '/',
    PathDelim, [rfReplaceAll]);
  Check(ReadBytes(LFile) = LPair.Source, 'Immutable compiler companion is byte-exact');
end;

begin
  LEngine := nil;
  LServer := nil;
  LProfile := nil;
  LDocument := nil;
  LWriter := nil;
  try

    if (ParamCount <> 4) and (ParamCount <> 5) then
    begin
      raise Exception.Create('Usage: <pristine-release> <new-runtime> <private-profile> <semantic-source> [owned-junction]');
    end;
    LRelease := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(1)));
    LRuntime := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(2)));
    LManifest := VerifyNyxStudioRelease(LRelease);
    Check(not DirectoryExists(LRuntime), 'Runtime fixture is new');
    LDirectories := TNyxStudioDirectories.ForRelease(LRelease, LRuntime);
    Check(not DirectoryExists(LRuntime), 'Typed directory admission performs no writes');
    LOriginalDirectories := LDirectories;
    LDirectories := LDirectories.EnrollingProject(LRuntime + 'enrollment');
    Check((LOriginalDirectories.EnrollmentRoot = LRuntime) and
      (LDirectories.EnrollmentRoot <> LOriginalDirectories.EnrollmentRoot),
      'Fluent enrollment keeps prior directory values independent');
    LLegacy := TNyxStudioDirectories.ForRepository(LRuntime + 'legacy');
    Check((LLegacy.SourceRoot = LLegacy.RuntimeRoot) and
      (LLegacy.WebRoot = LLegacy.SourceRoot + 'build' + PathDelim + 'browser' + PathDelim),
      'Repository launches retain their established layout');
    LOriginalDirectories := LLegacy;
    LLegacy := LLegacy.ServingFrom(LRuntime + 'staged-web');
    Check((LLegacy.WebRoot = LRuntime + 'staged-web' + PathDelim) and
      (LOriginalDirectories.WebRoot <> LLegacy.WebRoot) and
      (LLegacy.RuntimeRoot = LOriginalDirectories.RuntimeRoot) and
      (LLegacy.CompilerUnits = LOriginalDirectories.CompilerUnits),
      'Staged development web is copied independently of source and runtime roles');
    LRefused := False;
    try
      LDirectories.ServingFrom(LRuntime + 'staged-web');
    except
      on ENyxStudioRelease do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LDirectories.WebRoot = LRelease + 'web' + PathDelim),
      'Sealed release web refuses development override without changing its value');
    LRefused := False;
    try
      LUnused.Validate;
    except
      on ENyxStudioRelease do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Uninitialized directory values refuse');
    LRefused := False;
    try
      TNyxStudioDirectories.ForRelease(LRelease, LRelease + '.local');
    except
      on ENyxStudioRelease do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Runtime inside payload refuses before writes');
    LRefused := False;
    try
      LDirectories.EnrollingProject(LRelease);
    except
      on ENyxStudioRelease do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Enrollment inside payload refuses before writes');

    if ParamCount = 5 then
    begin
      LRefused := False;
      try
        TNyxStudioDirectories.ForRelease(LRelease, ParamStr(5));
      except
        on ENyxStudioRelease do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused, 'Actual runtime junction refuses before following its target');
    end;
    LProfile := TNyxOutputConfiguration.Decode(ReadBytes(ParamStr(3)));
    LWriter := TProfileWriter.Create;
    LWriter.Directories := LDirectories;
    LEngine := TNyxStudioMCP.Create(LDirectories, 8408, 8409, LProfile.Encode);
    { Never start this engine. Destruction terminates its suspended thread before
      Execute; guarded protocol operations and real compiler workers run directly.
      Authenticated HTTP/observing Studio remain an explicit later release gate. }
    LEngine.OnOperatorProfileChange := LWriter.Save;
    Check(FileExists(LDirectories.EnrollmentRoot + '.codex' + PathDelim + 'config.toml'),
      'Private protocol enrollment uses the explicit project directory');
    LDocument := BuildNyxDocument;
    LPair := NyxProjectPair(TNyxCodec.Encode(LDocument), ReadBytes(
      IncludeTrailingPathDelimiter(ParamStr(4)) + 'nyx.generated.view.pas'));
    LClaim := LEngine.ConnectEditor(NyxObject([
      NyxField('op', NyxData('claim')), NyxField('project', NyxData(EncodeNyxProject(LPair))),
      NyxField('selection', NyxData('workspace')), NyxField('view', NyxData('home'))]));
    LToken := LClaim.Field('token').AsText;
    LBefore := LEngine.EditorExchange(LToken, NyxObject([
      NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))]));
    LOutputs := BuildCall(NyxObject([NyxField('mode', NyxData('outputs'))]));
    LOutputID := LOutputs.Field('outputID').AsText;
    LFrame := BuildCall(NyxObject([
      NyxField('mode', NyxData('profile')), NyxField('expectedOutputID', NyxData(LOutputID)),
      NyxField('profile', TNyxDataValue.ParseJSON(LProfile.Encode))]));
    Check((LWriter.Saves = 1) and (LFrame.Field('outputID').AsText = LOutputID) and
      (ReadBytes(LDirectories.OutputProfile) = LProfile.Encode),
      'Operator profile persists only beneath runtime before acknowledgment');
    LBrowserJob := Request('browser', 'runtime-browser-application');
    LNativeJob := Request('lcl', 'runtime-native-application');
    LBrowserJob := WaitForJob(LBrowserJob);
    LNativeJob := WaitForJob(LNativeJob);
    {$IFDEF MSWINDOWS}
    ExecuteNativeArtifact(LNativeJob);
    {$ENDIF}
    WriteBytes(LRuntime + 'browser-status.nyx', LBrowserJob.ToJSON);
    WriteBytes(LRuntime + 'native-status.nyx', LNativeJob.ToJSON);
    LFrame := LEngine.EditorExchange(LToken, NyxObject([
      NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))]));
    Check(LFrame.Field('project').AsText = LBefore.Field('project').AsText,
      'Compiler work preserves the exact paired project');
    Check((LFrame.Field('session').Field('selection').AsText = 'workspace') and
      (LFrame.Field('session').Field('view').AsText = 'home'),
      'Compiler work preserves editor navigation');
    FreeAndNil(LEngine);
    { Construct/destruct the actual host with the saved runtime profile. Run is
      never called: no HTTP/MCP socket is bound and no active service is touched. }
    LServer := TNyxStudioServer.Create(LDirectories, 8418, '127.0.0.1', 8419);
    FreeAndNil(LServer);
    Check(VerifyNyxStudioRelease(LRelease).ToJSON = LManifest.ToJSON,
      'Protocol workers, enrollment and actual host leave the pristine payload byte-exact');
    Check(not DirectoryExists(LRelease + '.local') and not DirectoryExists(LRelease + '.codex') and
      not DirectoryExists(LRelease + 'build'), 'No private runtime directories enter the payload');
    WriteLn('PASS ', LChecks, ' integrated release/runtime checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL ', LError.ClassName, ': ', LError.Message);
      ExitCode := 1;
    end;
  end;
  LServer.Free;
  LEngine.Free;
  LWriter.Free;
  LDocument.Free;
  LProfile.Free;
end.
