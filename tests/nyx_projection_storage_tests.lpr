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

program nyx_projection_storage_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, Windows, nyx.text, nyx.bytes, nyx.data, nyx.model,
  nyx.source, nyx.studio.builds, nyx.studio.directories, nyx.studio.outputs,
  nyx.studio.buildexecutor, nyx.studio.sourceprojection,
  nyx.studio.projectionstorage, nyx.studio.sourcecompilation,
  nyx.studio.sourcecompilation.native, nyx.test.projection;

type
  { The worker owns its completion port. State is observed only after the
    operation's interlocked terminal transition, after executor/process join. }
  TProjectionPort = class(TInterfacedObject, INyxSourceCompilationPort)
  public
    Projection: INyxSourceProjection;
    Failure: TNyxText;
    Calls: Integer;
    procedure Complete(const AProjection: INyxSourceProjection;
      const AFailure: TNyxText);
  end;

  { Observe an actual constructor marker, not a guessed compilation delay.
    Only this invocation's new qualification root is inspected. }
  TCancelOnMarker = class(TThread)
  private
    FJobs: TNyxText;
    FCancellation: INyxBuildCancellation;
  protected
    procedure Execute; override;
  public
    Observed: Boolean;
    constructor Create(const AJobs: TNyxText; const ACancel: INyxBuildCancellation);
  end;

const
  CUnit: TNyxText = 'nyx.projection.fixture';
  CExactSource: TNyxText = 'Exact source 🚀';

var
  GChecks: Integer;
  GRoot: TNyxText;
  GPolicy: TNyxProjectionStoragePolicy;

function CreateHardLinkW(ANew, AExisting: PWideChar; AAttributes: Pointer): BOOL;
  stdcall; external 'kernel32' name 'CreateHardLinkW';

procedure Check(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Projection storage: ' + AMessage);
  end;
  Inc(GChecks);
end;

procedure SaveText(const APath, AText: TNyxText);
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

function ReadText(const APath: TNyxText): TNyxText;
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try
    Check(LStream.Size <= 8 * 1024 * 1024, 'bounded retained evidence input');
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

function FreshReference: TNyxSourceProjectionRef;
var
  LID: TGUID;
begin
  CreateGUID(LID);
  Result := NyxSourceProjectionRef('job-' + Copy(GUIDToString(LID), 2, 36));
end;

function NewStorage(const APolicy: TNyxProjectionStoragePolicy): TNyxOwnedProjectionStorage;
var
  LReport: TNyxProjectionStorageReport;
begin
  Result := TNyxOwnedProjectionStorage.Create(GRoot + 'guards', FreshReference,
    CUnit, btNativeLCL, APolicy, LReport);
end;

{ Make an actual directory junction using Win32, so qualification does not need
  administrator symlink privileges or a shell wrapper. All paths are exclusively
  new descendants of this test's caller-admitted root. It is deliberately left
  as evidence; no recursive traversal/removal can reach its target. }
procedure MakeJunction(const ALink, ATarget: TNyxText);
var
  LLink: UnicodeString;
  LSubstitute: UnicodeString;
  LPrint: UnicodeString;
  LData: array of Byte;
  LSize: Word;
  LSubstituteSize: Word;
  LPrintOffset: Word;
  LPrintSize: Word;
  LReturned: Cardinal;
  LHandle: THandle;
begin
  LLink := UTF8Decode(ALink);
  LSubstitute := '\??\' + UTF8Decode(ATarget);
  LPrint := UTF8Decode(ATarget);
  LSubstituteSize := Length(LSubstitute) * 2;
  LPrintOffset := LSubstituteSize + 2;
  LPrintSize := Length(LPrint) * 2;
  LSize := 8 + LPrintOffset + LPrintSize + 2;
  SetLength(LData, LSize + 8);
  PCardinal(@LData[0])^ := $A0000003;
  Move(LSize, LData[4], 2);
  Move(LSubstituteSize, LData[10], 2);
  Move(LPrintOffset, LData[12], 2);
  Move(LPrintSize, LData[14], 2);
  Move(LSubstitute[1], LData[16], LSubstituteSize);
  Move(LPrint[1], LData[16 + LPrintOffset], LPrintSize);
  Check(CreateDirectoryW(PWideChar(LLink), nil), 'fresh junction directory');
  LHandle := CreateFileW(PWideChar(LLink), GENERIC_WRITE, 0, nil, OPEN_EXISTING,
    FILE_FLAG_OPEN_REPARSE_POINT or FILE_FLAG_BACKUP_SEMANTICS, 0);
  Check(LHandle <> INVALID_HANDLE_VALUE, 'owned junction handle');
  try
    Check(DeviceIoControl(LHandle, $000900A4, @LData[0], Length(LData), nil, 0,
      @LReturned, nil), 'actual junction installed');
  finally
    CloseHandle(LHandle);
  end;
end;

procedure GuardChecks;
var
  LStorage: TNyxOwnedProjectionStorage;
  LOther: TNyxOwnedProjectionStorage;
  LReference: TNyxSourceProjectionRef;
  LReport: TNyxProjectionStorageReport;
  LRejected: Boolean;
  LHandle: THandle;
  LInfo: BY_HANDLE_FILE_INFORMATION;
  LFirst: UnicodeString;
  LSecond: UnicodeString;
  LPolicy: TNyxProjectionStoragePolicy;
  LUnknown: Integer;
begin
  LPolicy := TNyxProjectionStoragePolicy.Default;
  Check((LPolicy.Retention = prEvidence) and
    (LPolicy.RequiredFreeBytes = 128 * Int64(1024) * 1024), 'documented typed defaults');
  LPolicy := LPolicy.Retaining(prAllFiles).MinimumFreeBytes(0);
  Check(GPolicy.Retention = prEvidence, 'policy copies retain independent choices');
  LRejected := False;
  LUnknown := 99;
  try
    {$push}{$R-}
    LPolicy := GPolicy.Retaining(TNyxProjectionRetention(LUnknown));
    {$pop}
  except
    on ENyxModel do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'unknown retention rejected before allocation');
  LRejected := False;
  try
    LPolicy := GPolicy.MinimumFreeBytes(-1);
  except
    on ENyxModel do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'negative capacity rejected before allocation');
  LReference := FreshReference;
  LStorage := nil;
  LRejected := False;
  try
    LStorage := TNyxOwnedProjectionStorage.Create(GRoot + 'guards', LReference,
      'CON', btNativeLCL, GPolicy, LReport);
  except
    on ENyxModel do
    begin
      LRejected := True;
    end;
  end;
  LStorage.Free;
  Check(LRejected and not DirectoryExists(GRoot + 'guards/' + LReference.Name),
    'valid Pascal identifier cannot write a DOS device as source');

  LReference := FreshReference;
  LStorage := TNyxOwnedProjectionStorage.Create(GRoot + 'guards', LReference,
    CUnit, btNativeLCL, GPolicy, LReport);
  try
    LStorage.WriteEvidence(CUnit + '.pas', CExactSource);
    LOther := nil;
    LRejected := False;
    try
      LOther := TNyxOwnedProjectionStorage.Create(GRoot + 'guards', LReference,
        CUnit, btNativeLCL, GPolicy, LReport);
    except
      on ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    LOther.Free;
    Check(LRejected and (LReport.State = psDeferred), 'existing invocation never adopted');
    Check(ReadText(LStorage.Directory + CUnit + '.pas') = CExactSource,
      'exclusive admission retains exact source');
    LFirst := UTF8Decode(ExcludeTrailingPathDelimiter(LStorage.Directory));
    LSecond := UTF8Decode(LStorage.Directory + '-moved');
    Check(not MoveFileW(PWideChar(LFirst), PWideChar(LSecond)), 'live invocation cannot move');
    LFirst := UTF8Decode(LStorage.Directory + 'units');
    LHandle := CreateFileW(PWideChar(LFirst), GENERIC_WRITE,
      FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil, OPEN_EXISTING,
      FILE_FLAG_OPEN_REPARSE_POINT or FILE_FLAG_BACKUP_SEMANTICS, 0);

    if LHandle <> INVALID_HANDLE_VALUE then
    begin
      CloseHandle(LHandle);
    end;
    Check(LHandle = INVALID_HANDLE_VALUE, 'live units cannot acquire a reparse writer');
    LStorage.FinishAfterJoin;
    Check(LStorage.Snapshot.State = psDeferred, 'missing compiler capture cannot claim retirement');
  finally
    LStorage.Free;
  end;

  LStorage := NewStorage(GPolicy);
  try
    SaveText(LStorage.Directory + 'units/unchanged.o', 'owned');
    SaveText(LStorage.Directory + 'units/changed.o', 'FIRST');
    SaveText(LStorage.Directory + 'units/replaced.o', 'same');
    SaveText(LStorage.Directory + 'units/locked.o', 'locked');
    LFirst := UTF8Decode(LStorage.Directory + 'units/changed.o');
    LHandle := CreateFileW(PWideChar(LFirst), GENERIC_READ, FILE_SHARE_READ, nil,
      OPEN_EXISTING, 0, 0);
    Check(GetFileInformationByHandle(LHandle, @LInfo), 'capture original write time');
    CloseHandle(LHandle);
    LStorage.CaptureCompilerDerivatives;
    SaveText(LStorage.Directory + 'units/changed.o', 'OTHER');
    LHandle := CreateFileW(PWideChar(LFirst), GENERIC_WRITE, FILE_SHARE_READ or
      FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil, OPEN_EXISTING, 0, 0);
    Check(SetFileTime(LHandle, nil, nil, @LInfo.ftLastWriteTime), 'restore equal timestamp');
    CloseHandle(LHandle);
    LFirst := UTF8Decode(LStorage.Directory + 'units/replaced.o');
    LSecond := UTF8Decode(GRoot + 'moved-original.o');
    LRejected := not MoveFileW(PWideChar(LFirst), PWideChar(LSecond));
    LUnknown := GetLastError;
    Check(LRejected and (LUnknown = ERROR_SHARING_VIOLATION) and
      not FileExists(GRoot + 'moved-original.o'),
      'pinned directory prevents moving a captured derivative out of its scope');
    LFirst := UTF8Decode(LStorage.Directory + 'units/locked.o');
    LHandle := CreateFileW(PWideChar(LFirst), GENERIC_READ, FILE_SHARE_READ, nil,
      OPEN_EXISTING, 0, 0);
    Check(LHandle <> INVALID_HANDLE_VALUE, 'actual retirement lock');
    try
      SaveText(LStorage.Directory + 'units/late.o', 'late');
      LStorage.FinishAfterJoin;
      LReport := LStorage.Snapshot;
      Check((LReport.State = psDeferred) and (LReport.CapturedFiles = 4) and
        (LReport.RetiredFiles = 2) and (LReport.RetiredBytes = 9),
        'only unchanged captured derivative retires; group accurately defers');
      Check(not FileExists(LStorage.Directory + 'units/unchanged.o'), 'retirement actually removes file');
      Check(ReadText(LStorage.Directory + 'units/changed.o') = 'OTHER',
        'same length/time changed contents remain');
      Check(not FileExists(LStorage.Directory + 'units/replaced.o'),
        'unchanged derivative retires after its move was prevented');
      Check(FileExists(LStorage.Directory + 'units/locked.o'), 'locked file remains');
      Check(FileExists(LStorage.Directory + 'units/late.o'), 'uncaptured new file remains');
      LStorage.FinishAfterJoin;
      Check(LStorage.Snapshot.RetiredFiles = 2, 'retirement is idempotent');
    finally
      CloseHandle(LHandle);
    end;
  finally
    LStorage.Free;
  end;

  LStorage := NewStorage(GPolicy);
  try
    SaveText(GRoot + 'outside.txt', 'outside');
    LFirst := UTF8Decode(LStorage.Directory + 'units/linked.ppu');
    LSecond := UTF8Decode(GRoot + 'outside.txt');
    LRejected := not CreateHardLinkW(PWideChar(LFirst), PWideChar(LSecond), nil);
    LUnknown := GetLastError;
    Check(LRejected and (LUnknown = ERROR_SHARING_VIOLATION) and
      not FileExists(LStorage.Directory + 'units/linked.ppu'),
      'pinned directory refuses creating an external hardlink inside the invocation');
    LStorage.CaptureCompilerDerivatives;
    LStorage.FinishAfterJoin;
    Check((LStorage.Snapshot.State = psRetired) and
      (LStorage.Snapshot.RetiredFiles = 0), 'refused link created no retireable derivative');
    Check(ReadText(GRoot + 'outside.txt') = 'outside', 'external hardlink target remains exact');
  finally
    LStorage.Free;
  end;
  ForceDirectories(GRoot + 'junction-target');
  MakeJunction(GRoot + 'junction', GRoot + 'junction-target');
  LStorage := nil;
  LRejected := False;
  try
    LStorage := TNyxOwnedProjectionStorage.Create(GRoot + 'junction/jobs',
      FreshReference, CUnit, btNativeLCL, GPolicy, LReport);
  except
    on ENyxModel do
    begin
      LRejected := True;
    end;
  end;
  LStorage.Free;
  Check(LRejected and not DirectoryExists(GRoot + 'junction-target/jobs'),
    'reparse ancestor refused before touching its target');

  LStorage := NewStorage(GPolicy.Retaining(prAllFiles));
  try
    SaveText(LStorage.Directory + 'units/retained.o', 'retained');
    LStorage.CaptureCompilerDerivatives;
    LStorage.FinishAfterJoin;
    Check((LStorage.Snapshot.State = psPreserved) and
      (ReadText(LStorage.Directory + 'units/retained.o') = 'retained'),
      'typed all-files override preserves files');
  finally
    LStorage.Free;
  end;
end;

procedure TProjectionPort.Complete(const AProjection: INyxSourceProjection;
  const AFailure: TNyxText);
begin
  Projection := AProjection;
  Failure := AFailure;
  Inc(Calls);
end;

constructor TCancelOnMarker.Create(const AJobs: TNyxText;
  const ACancel: INyxBuildCancellation);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FJobs := AJobs;
  FCancellation := ACancel;
end;

procedure TCancelOnMarker.Execute;
var
  LSearch: TSearchRec;
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  while not Terminated and (GetTickCount64 - LStarted < 60000) do
  begin

    if FindFirst(FJobs + 'job-*', faDirectory, LSearch) = 0 then
    begin
      try
        repeat

          if FileExists(FJobs + LSearch.Name + '/cancel.started') then
          begin
            Observed := True;
            FCancellation.Cancel;
            Exit;
          end;
        until FindNext(LSearch) <> 0;
      finally
        SysUtils.FindClose(LSearch);
      end;
    end;
    Sleep(10);
  end;
end;

function MinimalSource(const ABody: TNyxText): TNyxText;
begin
  Result := 'unit ' + CUnit + ';' + #10 +
    '{$mode delphi}{$H+}{$codepage utf8}' + #10 +
    'interface uses nyx.model; function BuildNyxDocument: TNyxDocument;' + #10 +
    'implementation uses Classes, SysUtils;' + #10 +
    'function BuildNyxDocument: TNyxDocument;' + #10 + 'begin' + #10 +
    ABody + #10 + 'end;' + #10 + 'end.' + #10;
end;

function StorageOf(const ABuild: INyxSourceProjectionBuild): TNyxProjectionStorageReport;
var
  LObserved: INyxProjectionStorageBuild;
begin
  Check(Supports(ABuild, INyxProjectionStorageBuild, LObserved), 'native detached storage facet');
  Result := LObserved.Storage;
end;

{ A bounded inventory of this qualification's isolated root identifies the
  actual owning worker's fresh job, without relying on a projection's data as
  filesystem authority or inspecting another session's jobs. }
procedure CaptureJobs(const AJobs: TNyxText; ANames: TNyxStrings);
var
  LSearch: TSearchRec;
begin
  ANames.Clear;

  if FindFirst(AJobs + 'job-*', faDirectory, LSearch) = 0 then
  begin
    try
      repeat
        Check(ANames.Count < 32, 'bounded isolated worker job inventory');
        ANames.Add(TNyxText(LSearch.Name));
      until FindNext(LSearch) <> 0;
    finally
      SysUtils.FindClose(LSearch);
    end;
  end;
end;

procedure CompileChecks;
var
  LDirectories: TNyxStudioDirectories;
  LTools: TNyxDataValue;
  LProfile: TNyxOutputConfiguration;
  LExecutor: TNyxBuildExecutor;
  LBuild: INyxSourceProjectionBuild;
  LReport: TNyxProjectionStorageReport;
  LSource: TNyxText;
  LPath: TNyxText;
  LArtifact: TNyxText;
  LRetained: INyxSourceProjection;
  LCompiler: INyxSourceCompiler;
  LOperation: INyxSourceCompilation;
  LPort: TProjectionPort;
  LPortOwner: INyxSourceCompilationPort;
  LStarted: QWord;
  LCancel: INyxBuildCancellation;
  LCancelThread: TCancelOnMarker;
  LJobsBefore: TNyxStrings;
  LJobsAfter: TNyxStrings;
  LIndex: Integer;
  LNewJobs: Integer;
begin
  LDirectories := TNyxStudioDirectories.ForRepository(ParamStr(1))
    .RunningIn(GRoot + 'compiler').EnrollingProject(GRoot + 'compiler');
  LTools := TNyxDataValue.ParseJSON(ReadText(ParamStr(2)));
  LSource := ReadText(LDirectories.SourceRoot + 'tests/fixtures/' + CUnit + '.pas');
  LProfile := TNyxOutputConfiguration.Create;
  LExecutor := nil;
  LJobsBefore := TNyxStrings.Create;
  LJobsAfter := TNyxStrings.Create;
  try
    LProfile.SetField('fpc', LTools.Field('FPC').AsText);
    LProfile.SetField('pas2js', LTools.Field('PAS2JS').AsText);
    LProfile.SetField('runtime', LTools.Field('PAS2JS_RUNTIME').AsText);
    LExecutor := TNyxBuildExecutor.Create(LDirectories, LProfile.Encode);
    LExecutor.ConfigureProjectionStorage(GPolicy.MinimumFreeBytes(High(Int64)));
    LBuild := LExecutor.ProjectSource(LSource, NyxPascalUnit(CUnit), btNativeLCL);
    LReport := StorageOf(LBuild);
    Check((LBuild.Projection.State = spsUnavailable) and
      (LReport.State = psCapacityRefused) and (LReport.Message <> '') and
      not DirectoryExists(LDirectories.Jobs + LBuild.Reference.Name),
      'capacity pressure reports typed unavailable before invocation allocation');
    LExecutor.ConfigureProjectionStorage(GPolicy);
    LBuild := LExecutor.ProjectSource(LSource, NyxPascalUnit(CUnit), btNativeLCL, spcChecked);
    LReport := StorageOf(LBuild);
    Check(LBuild.Projection.State = spsExecuted, 'actual checked FPC constructor executed');
    Check(LBuild.Projection.Design = ExpectedNyxProjectionDesign,
      'complete helper/class/loop/state/resource/Unicode meaning survives retirement');
    Check((LReport.State = psRetired) and (LReport.RetiredFiles > 0) and
      (LReport.RetiredFiles = LReport.CapturedFiles) and (LReport.RetiredBytes > 0),
      'actual compiler derivatives fully retired after native execution join');
    LPath := LDirectories.Jobs + LBuild.Reference.Name + PathDelim;
    Check(not FileExists(LPath + 'nyx_projection.exe'), 'executed constructor executable retired');
    Check((ReadText(LPath + CUnit + '.pas') = LSource) and
      (ReadText(LPath + 'nyx_projection.lpr') = GenerateNyxSourceProjectionProgram(
        NyxPascalUnit(CUnit), LBuild.Reference, btNativeLCL)), 'exact source and wrapper evidence retained');
    Check((ReadText(LPath + NyxProjectionResultFile) <> '') and
      (ReadText(LPath + 'compiler.log') <> '') and
      (ReadText(LPath + 'execution.log') = LBuild.RuntimeLog), 'exact result and both logs retained');
    Check(Pos('0 unfreed memory blocks', LBuild.RuntimeLog) > 0, 'checked constructor clean heap');
    LRetained := LBuild.Projection;
    LBuild := LExecutor.ProjectSource(MinimalSource('  Result := 123;'),
      NyxPascalUnit(CUnit), btNativeLCL, spcChecked);
    LReport := StorageOf(LBuild);
    Check((LBuild.Projection.State = spsCompilationFailed) and
      (LBuild.Projection.Report <> nil) and (LBuild.Projection.Report.Count > 0) and
      (LReport.State = psRetired), 'actual compiler failure retains diagnostics and retires intermediates');
    Check(ReadText(LDirectories.Jobs + LBuild.Reference.Name + '/compiler.log') <> '',
      'failure log remains readable');
    LBuild := LExecutor.ProjectSource(MinimalSource(
      '  raise Exception.Create(''Storage constructor qualification'');'),
      NyxPascalUnit(CUnit), btNativeLCL, spcChecked);
    Check((LBuild.Projection.State = spsExecutionFailed) and
      (StorageOf(LBuild).State = psRetired), 'actual throwing constructor retires derivatives');

    LCancel := NewNyxBuildCancellation;
    LCancelThread := TCancelOnMarker.Create(LDirectories.Jobs, LCancel);
    try
      LCancelThread.Start;
      LBuild := LExecutor.ProjectSource(MinimalSource(
        '  TFileStream.Create(''cancel.started'', fmCreate).Free;' + #10 +
        '  while True do' + #10 + '  begin' + #10 + '    Sleep(10);' + #10 +
        '  end;'), NyxPascalUnit(CUnit), btNativeLCL, spcChecked, LCancel);
    finally
      LCancelThread.Terminate;
      LCancelThread.WaitFor;
    end;
    try
      LReport := StorageOf(LBuild);
      Check(LCancelThread.Observed and (LBuild.Projection.State = spsCancelled) and
        (LReport.RetiredFiles > 0) and (LReport.State = psDeferred),
        'actual constructor cancellation joins; unknown marker remains with accurate deferral');
      Check(ReadText(LDirectories.Jobs + LBuild.Reference.Name + '/execution.log') =
        LBuild.RuntimeLog, 'cancellation retains exact collected execution log');
    finally
      LCancelThread.Free;
    end;

    LCompiler := NewNyxNativeSourceCompiler(LDirectories, LProfile.Encode,
      TNyxCompilerLimits.Default, GPolicy);
    CaptureJobs(LDirectories.Jobs, LJobsBefore);
    LPort := TProjectionPort.Create;
    LPortOwner := LPort;
    LOperation := LCompiler.Start(LSource, LPortOwner);
    LStarted := GetTickCount64;
    while LOperation.State in [scsPending, scsRunning] do
    begin

      if GetTickCount64 - LStarted > 90000 then
      begin
        LOperation.Cancel;
        { Releasing the host retires its scheduler; never loop forever around a
          pending operation or infer successful completion from elapsed time. }
        LCompiler := nil;
        raise Exception.Create('Owning constructor worker exceeded its qualification budget');
      end;
      CheckSynchronize(0);
      Sleep(10);
    end;
    LCompiler := nil;
    Check((LOperation.State = scsCompleted) and (LPort.Calls = 1) and
      (LPort.Failure = '') and (LPort.Projection.Design = ExpectedNyxProjectionDesign),
      'owning worker preserves full meaning after terminal host release and storage retirement');
    CaptureJobs(LDirectories.Jobs, LJobsAfter);
    LNewJobs := 0;
    for LIndex := 0 to LJobsAfter.Count - 1 do
    begin

      if LJobsBefore.IndexOf(LJobsAfter[LIndex]) < 0 then
      begin
        Inc(LNewJobs);
        LPath := LDirectories.Jobs + LJobsAfter[LIndex] + PathDelim;
        Check(not FileExists(LPath + 'nyx_projection.exe') and
          (ReadText(LPath + CUnit + '.pas') = LSource) and
          FileExists(LPath + NyxProjectionResultFile) and
          FileExists(LPath + 'compiler.log') and FileExists(LPath + 'execution.log'),
          'actual owning worker retires executable and retains its exact evidence');
      end;
    end;
    Check(LNewJobs = 1, 'one owning worker creates one independent invocation');
    LOperation := nil;
    LPortOwner := nil;

    LBuild := LExecutor.ProjectSource(LSource, NyxPascalUnit(CUnit), btBrowser);
    Check((LBuild.Projection.State = spsCompiled) and
      (StorageOf(LBuild).State = psPreserved), 'actual pas2js package remains compiled-only and preserved');
    LPath := LDirectories.Jobs + LBuild.Reference.Name + '/nyx_projection.js';
    LArtifact := ReadText(LPath);
    Check((LBuild.Artifact <> '') and (Pos('rtl.module', LArtifact) > 0) and
      (Pos('nyx.projection.fixture', LArtifact) > 0) and
      FileExists(LDirectories.Jobs + LBuild.Reference.Name + '/' + CUnit + '.pas'),
      'complete compiled worker and source package readable');
    LExecutor.Free;
    LExecutor := nil;
    Check(ReadText(LPath) = LArtifact, 'browser package survives executor release unchanged');
    Check(LRetained.Design = ExpectedNyxProjectionDesign,
      'native admitted design survives executor and receipt release');
  finally
    LExecutor.Free;
    LProfile.Free;
    LJobsAfter.Free;
    LJobsBefore.Free;
  end;
end;

begin

  if (ParamCount <> 3) or DirectoryExists(ParamStr(3)) or FileExists(ParamStr(3)) then
  begin
    raise Exception.Create('Supply repository, local toolchain JSON and a NEW owned runtime home');
  end;
  GRoot := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(3)));
  GPolicy := TNyxProjectionStoragePolicy.Default.MinimumFreeBytes(0);
  ForceDirectories(GRoot);
  GuardChecks;
  WriteLn('PASS storage guard checks ', GChecks);
  CompileChecks;
  WriteLn('PASS constructor storage ', GChecks);
end.
