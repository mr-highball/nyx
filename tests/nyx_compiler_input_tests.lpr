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
program nyx_compiler_input_tests;

{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch nestedprocvars}

uses
  Classes, SysUtils, Windows, fpjson, nyx.text, nyx.data, nyx.model, nyx.codec, nyx.editing,
  nyx.studio.compilerinputs, nyx.studio.buildexecutor, nyx.studio.builds,
  nyx.studio.outputs, nyx.studio.agents, nyx.studio.projects, nyx.test.compiler.fixture;

type
  { Runs the real executor on a detached pair and explicitly owned fixture root.
    Cancellation/final join own its compiler; only the joined result is inspected. }
  TInputWorker = class(TThread)
  private
    FRoot: TNyxText;
    FProfile: TNyxText;
    FPair: TNyxProjectPair;
    FCancellation: INyxBuildCancellation;
  protected
    procedure Execute; override;
  public
    Output: TNyxDataValue;
    Error: TNyxText;
    constructor Create(const ARoot, AProfile: TNyxText; const APair: TNyxProjectPair);
    destructor Destroy; override;
  end;

var
  GRoot: TNyxText;
  GChecks: Integer;

{ This Win32 API creates only an explicitly named fixture hardlink to another
  fixture file. No ownership handle or linked content enters a product lease. }
function CreateHardLinkW(ANew, AExisting: PWideChar; AAttributes: Pointer): BOOL;
  stdcall; external 'kernel32' name 'CreateHardLinkW';

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure Save(const APath, AText: TNyxText);
var
  LFile: THandleStream;
  LWide: UnicodeString;
  LHandle: THandle;
begin
  LWide := UTF8Decode(APath);
  LHandle := CreateFileW(PWideChar(LWide), GENERIC_WRITE, 0, nil, CREATE_ALWAYS,
    FILE_ATTRIBUTE_NORMAL, 0);

  if LHandle = INVALID_HANDLE_VALUE then
  begin
    raise Exception.Create('Cannot write the exact owned fixture input');
  end;
  { The stream borrows the wide-API handle. Never route Unicode fixture paths
    through an ANSI filename overload or alter the process-global codepage. }
  LFile := nil;
  try
    LFile := THandleStream.Create(LHandle);

    if Length(AText) > 0 then
    begin
      LFile.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LFile.Free;
    CloseHandle(LHandle);
  end;
end;

procedure Prepare(const APath: TNyxText);
begin
  Check(not DirectoryExists(APath), 'Input consumer requires a fresh owned directory');
  ForceDirectories(APath + '/src/nested');
  ForceDirectories(APath + '/studio');
  Save(APath + '/src/first.inc', 'first input');
  Save(APath + '/src/nested/second.inc', 'second input');
  Save(APath + '/studio/third.pas', 'third input');
end;

function TryWriting(const APath: TNyxText): Boolean;
var
  LHandle: THandle;
  LPath: UnicodeString;
begin
  LPath := UTF8Decode(APath);
  LHandle := CreateFileW(PWideChar(LPath), GENERIC_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil,
    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  Result := LHandle <> INVALID_HANDLE_VALUE;

  if Result then
  begin
    CloseHandle(LHandle);
  end;
end;

procedure WaitForChange(AInputs: TNyxCompilerInputs);
var
  LStarted: QWord;
  LRefused: Boolean;
begin
  LStarted := GetTickCount64;
  repeat
    LRefused := False;
    try
      AInputs.Check;
    except
      on ENyxCompilerInputs do
      begin
        LRefused := True;
      end;
    end;

    if LRefused then
    begin
      Break;
    end;

    if GetTickCount64 - LStarted > 5000 then
    begin
      raise Exception.Create('The exact owned directory watch did not observe a change');
    end;
    Sleep(1);
  until False;
  Check(LRefused, 'Directory changes invalidate the live source lease');
end;

procedure SourceOwnership;
var
  LRoot: TNyxText;
  LFirst: TNyxCompilerInputs;
  LSecond: TNyxCompilerInputs;
  LWriter: TFileStream;
  LRefused: Boolean;
  LCalls: Integer;
  LCancel: Boolean;
  LTransient: TNyxText;
  LTransientWide: UnicodeString;

  procedure CheckScope;
  begin
    Inc(LCalls);

    if LCancel and (LCalls >= 8) then
    begin
      raise EAbort.Create('owned acquisition cancellation');
    end;
  end;

begin
  LRoot := GRoot + '/ordinary';
  Prepare(LRoot);
  LFirst := nil;
  LSecond := nil;
  LCalls := 0;
  LCancel := False;
  try
    LFirst := TNyxCompilerInputs.Create(LRoot, CheckScope);
    Check(LFirst.FileCount = 3, 'The complete nested input set is retained');
    Check(LFirst.ByteCount = 34, 'Retained byte counts include every owned input');
    Check(LCalls > 8, 'Nested cancellation checks run during actual acquisition');
    LSecond := TNyxCompilerInputs.Create(LRoot);
    Check(LSecond.FileCount = 3, 'Concurrent compiler readers share the same inputs');
    Check(not TryWriting(LRoot + '/src/first.inc'), 'Input writers refuse while readers own the source');
    Check(not RenameFile(LRoot + '/src/nested', LRoot + '/src/moved'),
      'Pinned directory identity refuses replacement');
    FreeAndNil(LFirst);
    Check(not TryWriting(LRoot + '/src/first.inc'), 'The second worker independently retains its lease');
    FreeAndNil(LSecond);
    Check(TryWriting(LRoot + '/src/first.inc'), 'Final input retirement permits later edits');

    LWriter := TFileStream.Create(LRoot + '/src/first.inc', fmOpenReadWrite or fmShareDenyNone);
    try
      LRefused := False;
      try
        LFirst := TNyxCompilerInputs.Create(LRoot);
      except
        on ENyxCompilerInputs do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused, 'An existing writer refuses the entire input lease');
    finally
      LWriter.Free;
    end;
    Check(TryWriting(LRoot + '/src/nested/second.inc'), 'Failed acquisition releases earlier retained inputs');
    LCancel := True;
    LCalls := 0;
    LRefused := False;
    try
      LFirst := TNyxCompilerInputs.Create(LRoot, CheckScope);
    except
      on EAbort do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Caller cancellation interrupts bounded input acquisition');
    Check(TryWriting(LRoot + '/src/first.inc'), 'Cancellation releases all acquired handles and watches');
    LCancel := False;

    { A transient name enters and leaves while its original complete inventory
      remains the same. A before/after filename comparison cannot detect this. }
    LFirst := TNyxCompilerInputs.Create(LRoot);
    LTransient := LRoot + '/src/transient.inc';
    Save(LTransient, 'temporary input');
    Check(FileExists(LTransient), 'A fresh owned transient input enters the search tree');
    LTransientWide := UTF8Decode(LTransient);
    Check(DeleteFileW(PWideChar(LTransientWide)),
      'Only that newly created fixture input leaves before final observation');
    WaitForChange(LFirst);
    LRefused := False;
    try
      LFirst.Check;
    except
      on ENyxCompilerInputs do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'An invalidated lease never silently rearms');
    FreeAndNil(LFirst);
    LFirst := TNyxCompilerInputs.Create(LRoot);
    LFirst.Check;
    Check(LFirst.FileCount = 3, 'An explicit later acquisition admits the stable current inputs');
    FreeAndNil(LFirst);

    { Source identity is actual file ownership, not only a source extension.
      A file's logical size exercises admission before any source-byte allocation. }
    LWriter := TFileStream.Create(LRoot + '/src/oversized.dat', fmCreate);
    try
      LWriter.Size := 65 * Int64(1024) * 1024;
    finally
      LWriter.Free;
    end;
    LRefused := False;
    try
      LFirst := TNyxCompilerInputs.Create(LRoot);
    except
      on ENyxCompilerInputs do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Oversized inputs refuse before retaining an unbounded source set');
    Check(TryWriting(LRoot + '/studio/third.pas'), 'Byte-budget refusal releases unrelated input ownership');

  finally
    LSecond.Free;
    LFirst.Free;
  end;
end;

procedure ExactInputs;
var
  LRoot: TNyxText;
  LName: TNyxText;
  LLease: TNyxCompilerInputs;
  LWideName: UnicodeString;
  LWideTarget: UnicodeString;
  LRefused: Boolean;
begin
  LRoot := GRoot + '/exact';
  Prepare(LRoot);
  LLease := nil;
  LName := LRoot + TNyxText('/src/English-🦊.inc');
  Save(LName, TNyxText('Exact bytes 🚀') + #0 + 'tail');
  try
    LLease := TNyxCompilerInputs.Create(LRoot);
    Check(LLease.FileCount = 4, 'Supplementary Unicode source names enter the complete lease');
    Check(not TryWriting(LName), 'The exact wide-API Unicode input is protected');
    LLease.Check;
    FreeAndNil(LLease);
    Check(TryWriting(LName), 'The same Unicode input is writable after release');
    Save(LRoot + '/external-fixture.inc', 'owned external fixture');
    LWideName := UTF8Decode(LRoot + '/src/linked.inc');
    LWideTarget := UTF8Decode(LRoot + '/external-fixture.inc');
    Check(CreateHardLinkW(PWideChar(LWideName), PWideChar(LWideTarget), nil),
      'The explicitly owned fixture really contains an additional hardlink');
    LRefused := False;
    try
      LLease := TNyxCompilerInputs.Create(LRoot);
    except
      on ENyxCompilerInputs do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'An existing hardlink refuses source input authority');
    Check(TryWriting(LRoot + '/external-fixture.inc'),
      'Link refusal releases handles without altering the linked bytes');
  finally
    LLease.Free;
  end;
end;

constructor TInputWorker.Create(const ARoot, AProfile: TNyxText;
  const APair: TNyxProjectPair);
begin
  inherited Create(True);
  FRoot := ARoot;
  FProfile := AProfile;
  FPair := APair;
  FCancellation := NewNyxBuildCancellation;
  Output := NyxNull;
  Start;
end;

destructor TInputWorker.Destroy;
begin
  FCancellation.Cancel;
  WaitFor;
  inherited Destroy;
end;

procedure TInputWorker.Execute;
var
  LExecutor: TNyxBuildExecutor;
  LDocument: TNyxDocument;
  LResult: TJSONObject;
begin
  LExecutor := nil;
  LDocument := nil;
  LResult := nil;
  try
    try
      LExecutor := TNyxBuildExecutor.Create(FRoot, FProfile);
      LDocument := TNyxCodec.Decode(FPair.Design);
      LResult := LExecutor.Build(LDocument, 'browser', 'view', 'home', FPair.Source,
        FCancellation);
      Output := TNyxDataValue.ParseJSON(TNyxText(LResult.AsJSON));
    except
      on LException: Exception do
      begin
        Error := LException.Message;
      end;
    end;
  finally
    LResult.Free;
    LDocument.Free;
    LExecutor.Free;
  end;
end;

procedure ExecutorObservation;
var
  LRoot: TNyxText;
  LJobs: TNyxText;
  LMarker: TNyxText;
  LEntry: TSearchRec;
  LSession: TNyxAgentSession;
  LProfile: TNyxOutputConfiguration;
  LPair: TNyxProjectPair;
  LWorker: TInputWorker;
  LHandle: THandle;
  LStarted: QWord;
  LText: TStringList;
begin
  LRoot := GRoot + '/executor';
  Prepare(LRoot);
  LJobs := LRoot + '/build/studio/jobs/';
  LSession := TNyxAgentSession.Create;
  LProfile := TNyxOutputConfiguration.Create;
  LWorker := nil;
  LHandle := 0;
  LText := TStringList.Create;
  try
    LProfile.SetField('pas2js', ExpandFileName(ParamStr(2)));
    LProfile.SetField('runtime', ExpandFileName(ParamStr(3)));
    LPair := LSession.BuildPair(LSession.Revision, bsView, 'home');
    LWorker := TInputWorker.Create(LRoot, LProfile.Encode, LPair);
    LStarted := GetTickCount64;
    LMarker := '';
    repeat

      if FindFirst(LJobs + 'job-*', faDirectory, LEntry) = 0 then
      begin
        try
          repeat

            if FileExists(NyxCompilerFixtureMarkerPath(LJobs + LEntry.Name, cfrCompiler)) then
            begin
              LMarker := NyxCompilerFixtureMarkerPath(LJobs + LEntry.Name, cfrCompiler);
              Break;
            end;
          until FindNext(LEntry) <> 0;
        finally
          SysUtils.FindClose(LEntry);
        end;
      end;

      if LMarker <> '' then
      begin
        Break;
      end;

      if LWorker.Finished or (GetTickCount64 - LStarted > 10000) then
      begin
        raise Exception.Create('The owned executor did not start its fixture compiler: ' + LWorker.Error);
      end;
      Sleep(1);
    until False;
    LText.LoadFromFile(LMarker);
    LHandle := OpenProcess(SYNCHRONIZE, False, StrToInt(Trim(LText.Text)));
    Check((LHandle <> 0) and (WaitForSingleObject(LHandle, 0) = WAIT_TIMEOUT),
      'The exact marked compiler is alive before source mutation');
    Save(LRoot + '/src/late.inc', 'introduced while compiling');
    LWorker.WaitFor;
    Check(LWorker.Error = '', 'Input refusal is an ordinary compiler result, not a host exception');
    Check(not LWorker.Output.Field('ok').AsBoolean,
      'Changed input membership refuses the actual executor result');
    Check(LWorker.Output.Field('failure').AsText = 'compiler',
      'Input failure retains the existing closed execution protocol');
    Check(Pos('library inputs changed', LWorker.Output.Field('log').AsText) > 0,
      'The owning compiler log explains the source consistency refusal');
    Check(WaitForSingleObject(LHandle, 0) = WAIT_OBJECT_0,
      'The result is not published before the exact marked compiler joins');
    Check(TryWriting(LRoot + '/src/first.inc'), 'Executor terminal completion releases source leases');
    Check(LSession.BuildPair(LSession.Revision, bsView, 'home').Source = LPair.Source,
      'Compiler input failure preserves accepted authoring source');
  finally
    LWorker.Free;
    LText.Free;

    if LHandle <> 0 then
    begin
      CloseHandle(LHandle);
    end;
    LProfile.Free;
    LSession.Free;
  end;
end;

begin
  try

    if ParamCount <> 3 then
    begin
      raise Exception.Create('Supply a fresh owned runtime, fixture compiler and matching RTL file');
    end;
    GRoot := ExpandFileName(ParamStr(1));
    Check(not DirectoryExists(GRoot), 'The source input journey owns a new root');
    SourceOwnership;
    ExactInputs;
    ExecutorObservation;
    WriteLn('PASS ', GChecks, ' actual compiler input ownership checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL after ', GChecks, ' checks: ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
