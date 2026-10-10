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

program nyx_source_projection_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.model, nyx.codec,
  nyx.source, nyx.studio.builds, nyx.studio.directories, nyx.studio.outputs,
  nyx.studio.buildexecutor, nyx.studio.sourceprojection, nyx.studio.compiler,
  nyx.test.projection, nyx.test.projectionediting;

type
  { Cancellation waits for a marker written by the actual constructor, so it
    exercises child retirement rather than merely preventing compiler startup.
    The thread observes only this fresh owned Jobs tree and owns no executor. }
  TCancelAtExecution = class(TThread)
  private
    FRoot: TNyxText;
    FCancellation: INyxBuildCancellation;
    FObserved: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(const ARoot: TNyxText;
      const ACancellation: INyxBuildCancellation);
    property Observed: Boolean read FObserved;
  end;

const
  CUnit: TNyxText = 'nyx.projection.fixture';

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Projection executor: ' + AReason);
  end;
  Inc(GChecks);
end;

function ReadText(const APath: TNyxText): TNyxText;
var
  LFile: TFileStream;
  LBytes: TNyxBytes;
begin
  LFile := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try

    if (LFile.Size < 1) or (LFile.Size > NyxProjectionMaximumResultBytes) then
    begin
      raise Exception.Create('Qualification input requires bounded bytes');
    end;
    SetLength(LBytes, LFile.Size);
    LFile.ReadBuffer(LBytes[0], Length(LBytes));
    Result := NyxDecodeUTF8(LBytes);
  finally
    LFile.Free;
  end;
end;

procedure SaveText(const APath, AText: TNyxText);
var
  LFile: TFileStream;
  LBytes: TNyxBytes;
begin
  LBytes := NyxEncodeUTF8(AText);
  LFile := TFileStream.Create(APath, fmCreate);
  try

    if Length(LBytes) > 0 then
    begin
      LFile.WriteBuffer(LBytes[0], Length(LBytes));
    end;
  finally
    LFile.Free;
  end;
end;

constructor TCancelAtExecution.Create(const ARoot: TNyxText;
  const ACancellation: INyxBuildCancellation);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FRoot := ARoot;
  FCancellation := ACancellation;
end;

procedure TCancelAtExecution.Execute;
var
  LStarted: QWord;
  LSearch: TSearchRec;
begin
  LStarted := GetTickCount64;
  while not Terminated and (GetTickCount64 - LStarted < 30000) do
  begin

    if FindFirst(FRoot + 'job-*', faDirectory, LSearch) = 0 then
    begin
      try
        repeat

          if FileExists(FRoot + LSearch.Name + PathDelim + 'executing.marker') then
          begin
            FObserved := True;
            FCancellation.Cancel;
            Exit;
          end;
        until FindNext(LSearch) <> 0;
      finally
        FindClose(LSearch);
      end;
    end;
    Sleep(20);
  end;
end;

function MinimalSource(const ABody: TNyxText; ANativeMarker: Boolean = False): TNyxText;
var
  LUses: TNyxText;
begin
  LUses := 'SysUtils, nyx.model';

  if ANativeMarker then
  begin
    LUses := 'Classes, ' + LUses;
  end;
  Result := 'unit ' + CUnit + ';' + #10 +
    '{$mode delphi}{$H+}{$codepage utf8}' + #10 +
    'interface' + #10 + 'uses ' + LUses + ';' + #10 +
    'function BuildNyxDocument: TNyxDocument;' + #10 +
    'implementation' + #10 + 'function BuildNyxDocument: TNyxDocument;' + #10 +
    'begin' + #10 + ABody + #10 + 'end;' + #10 + 'end.' + #10;
end;

procedure NoDesign(const ABuild: INyxSourceProjectionBuild;
  AState: TNyxSourceProjectionState; const AReason: TNyxText);
var
  LCopy: TNyxDocument;
  LRefused: Boolean;
begin
  Check((ABuild <> nil) and (ABuild.Projection.State = AState) and
    (ABuild.Projection.Design = '') and (ABuild.Artifact = ''), AReason);
  LCopy := nil;
  LRefused := False;
  try
    try
      LCopy := ABuild.Projection.CopyDocument;
    except
      on LException: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LCopy = nil), AReason + ' has no usable tree');
  finally
    LCopy.Free;
  end;
end;

procedure Run;
var
  LDirectories: TNyxStudioDirectories;
  LProfile: TNyxOutputConfiguration;
  LTools: TNyxDataValue;
  LExecutor: TNyxBuildExecutor;
  LUnavailable: TNyxBuildExecutor;
  LBuild: INyxSourceProjectionBuild;
  LRetained: INyxSourceProjection;
  LCancellation: INyxBuildCancellation;
  LCancelThread: TCancelAtExecution;
  LSource: TNyxText;
  LExpected: TNyxText;
  LBad: TNyxText;
  LThrow: TNyxText;
  LNil: TNyxText;
  LManifest: TNyxDataValue;
  LEntries: array[0..2] of TNyxDataValue;
  LTarget: TNyxBuildTarget;
  LIndex: Integer;
  LDiagnostic: Integer;
  LHasError: Boolean;
  LCopy: TNyxDocument;
  LRejected: Boolean;
begin

  if (ParamCount <> 3) or DirectoryExists(ParamStr(3)) or FileExists(ParamStr(3)) then
  begin
    raise Exception.Create('Supply repository, local toolchain JSON and a NEW owned runtime home');
  end;
  LDirectories := TNyxStudioDirectories.ForRepository(ParamStr(1))
    .RunningIn(ParamStr(3)).EnrollingProject(ParamStr(3));
  ForceDirectories(LDirectories.RuntimeRoot + 'web');
  LSource := ReadText(LDirectories.SourceRoot + 'tests/fixtures/' + CUnit + '.pas');
  LExpected := ExpectedNyxProjectionDesign;
  SaveText(LDirectories.RuntimeRoot + 'web/source.pas', LSource);
  SaveText(LDirectories.RuntimeRoot + 'web/expected.nyx', LExpected);
  LTools := TNyxDataValue.ParseJSON(ReadText(ParamStr(2)));
  LProfile := TNyxOutputConfiguration.Create;
  LExecutor := nil;
  LUnavailable := nil;
  try
    LProfile.SetField('fpc', LTools.Field('FPC').AsText);
    LProfile.SetField('pas2js', LTools.Field('PAS2JS').AsText);
    LProfile.SetField('runtime', LTools.Field('PAS2JS_RUNTIME').AsText);
    { No Lazarus path/widgetset is configured: this is model execution only. }
    LExecutor := TNyxBuildExecutor.Create(LDirectories, LProfile.Encode);
    LBuild := LExecutor.ProjectSource(LSource, NyxPascalUnit(CUnit),
      btNativeLCL, spcChecked);
    SaveText(LDirectories.RuntimeRoot + 'native-execution.log', LBuild.RuntimeLog);
    Check(LBuild.Projection.State = spsExecuted, 'native handwritten constructor executed');
    Check((LBuild.Projection.Report <> nil) and
      (LBuild.Projection.Report.Source = LSource), 'native exact compiler report');
    Check(Pos('0 unfreed memory blocks', LBuild.RuntimeLog) > 0,
      'actual checked native child has zero heap leaks');
    GChecks := GChecks + RunNyxProjectionPacketChecks(LSource, LBuild.Reference,
      btNativeLCL, ReadText(LDirectories.Jobs + LBuild.Reference.Name +
        PathDelim + NyxProjectionResultFile), LExpected);
    LRetained := LBuild.Projection;
    WriteLn('PASS guarded compiler source publication ',
      RunNyxProjectionEditingChecks(LRetained));
    LBuild := nil;

    LBad := MinimalSource('  Result := 123;');
    LThrow := MinimalSource('  raise Exception.Create(''Deliberate constructor failure'');');
    LNil := MinimalSource('  Result := nil;');
    SaveText(LDirectories.RuntimeRoot + 'web/throw.pas', LThrow);
    SaveText(LDirectories.RuntimeRoot + 'web/nil.pas', LNil);
    for LTarget := Low(TNyxBuildTarget) to High(TNyxBuildTarget) do
    begin
      LBuild := LExecutor.ProjectSource(LBad, NyxPascalUnit(CUnit), LTarget, spcChecked);
      NoDesign(LBuild, spsCompilationFailed, 'actual type failure');
      Check((LBuild.Projection.Report <> nil) and
        (LBuild.Projection.Report.Source = LBad), 'failed exact compiler report');
      LHasError := False;
      for LDiagnostic := 0 to LBuild.Projection.Report.Count - 1 do
      begin

        if LBuild.Projection.Report.Item(LDiagnostic).Severity in [csError, csFatal] then
        begin
          LHasError := True;
        end;
      end;
      Check(LHasError, 'ordinary compiler reports the source type error');
    end;
    LBuild := LExecutor.ProjectSource(LThrow, NyxPascalUnit(CUnit), btNativeLCL, spcChecked);
    NoDesign(LBuild, spsExecutionFailed, 'actual throwing constructor');
    LBuild := LExecutor.ProjectSource(LNil, NyxPascalUnit(CUnit), btNativeLCL, spcChecked);
    NoDesign(LBuild, spsInvalidDesign, 'actual nil constructor');

    for LIndex := 0 to 2 do
    begin
      case LIndex of
        0:
          begin
            LBad := LSource;
          end;
        1:
          begin
            LBad := LThrow;
          end;
        2:
          begin
            LBad := LNil;
          end;
      end;
      LBuild := LExecutor.ProjectSource(LBad, NyxPascalUnit(CUnit), btBrowser);
      Check((LBuild.Projection.State = spsCompiled) and
        (LBuild.Projection.Design = '') and (LBuild.Artifact <> ''),
        'browser compilation advertises only its worker');
      LEntries[LIndex] := NyxObject([
        NyxField('ticket', NyxData(LBuild.Reference.Name)),
        NyxField('artifact', NyxData(LBuild.Artifact))]);
    end;
    LManifest := NyxObject([NyxField('version', NyxData(1)),
      NyxField('workers', NyxArray(LEntries))]);
    SaveText(LDirectories.RuntimeRoot + 'web/projection.json', LManifest.ToJSON);

    LProfile.Free;
    LProfile := nil;
    LProfile := TNyxOutputConfiguration.Create;
    LUnavailable := TNyxBuildExecutor.Create(LDirectories, LProfile.Encode);
    for LTarget := Low(TNyxBuildTarget) to High(TNyxBuildTarget) do
    begin
      NoDesign(LUnavailable.ProjectSource(LSource, NyxPascalUnit(CUnit), LTarget),
        spsUnavailable, 'missing toolchain remains optional');
    end;
    LCancellation := NewNyxBuildCancellation;
    LCancellation.Cancel;
    for LTarget := Low(TNyxBuildTarget) to High(TNyxBuildTarget) do
    begin
      NoDesign(LExecutor.ProjectSource(LSource, NyxPascalUnit(CUnit), LTarget,
        spcChecked, LCancellation), spsCancelled, 'pre-start cancellation');
    end;

    LRejected := False;
    try
      LExecutor.ProjectSource('', NyxPascalUnit(CUnit), btNativeLCL);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'empty source refuses before compilation');

    LExecutor.ConfigureLimits(TNyxCompilerLimits.Default.TimeMilliseconds(10000));
    LBuild := LExecutor.ProjectSource(MinimalSource('  while True do' + #10 +
      '  begin' + #10 + '  end;'), NyxPascalUnit(CUnit), btNativeLCL, spcChecked);
    NoDesign(LBuild, spsExecutionFailed, 'actual constructor deadline retires child');
    Check(Pos('time budget exceeded', LBuild.RuntimeLog) > 0,
      'deadline occurred during execution');
    LExecutor.ConfigureLimits(TNyxCompilerLimits.Default);

    LCancellation := NewNyxBuildCancellation;
    LCancelThread := TCancelAtExecution.Create(LDirectories.Jobs, LCancellation);
    try
      LCancelThread.Start;
      LBuild := LExecutor.ProjectSource(MinimalSource(
        '  TFileStream.Create(''executing.marker'', fmCreate).Free;' + #10 +
        '  while True do' + #10 + '  begin' + #10 + '  end;', True),
        NyxPascalUnit(CUnit), btNativeLCL, spcChecked, LCancellation);
      LCancelThread.Terminate;
      LCancelThread.WaitFor;
      Check(LCancelThread.Observed, 'cancellation observed actual running constructor');
      NoDesign(LBuild, spsCancelled, 'running child cancellation retires family');
    finally
      LCancelThread.Terminate;
      LCancelThread.WaitFor;
      LCancelThread.Free;
    end;
    LExecutor.Free;
    LExecutor := nil;
    LCopy := LRetained.CopyDocument;
    try
      Check(TNyxCodec.Encode(LCopy) = LExpected,
        'immutable executed result survives executor/receipt release');
    finally
      LCopy.Free;
    end;
  finally
    LExecutor.Free;
    LUnavailable.Free;
    LProfile.Free;
  end;
end;

begin
  try
    Run;
    WriteLn('PASS source projection ', GChecks);
  except
    on LException: Exception do
    begin
      WriteLn(StdErr, LException.ClassName, ': ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
