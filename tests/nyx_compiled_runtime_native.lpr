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
program nyx_compiled_runtime_native;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, Process, Windows, fpjson, nyx.text, nyx.bytes,
  nyx.data, nyx.types, nyx.model, nyx.codec, nyx.source, nyx.composition,
  nyx.studio.outputs, nyx.studio.builds, nyx.studio.buildexecutor,
  nyx.studio.directories, nyx.studio.compilerprocess, nyx.test.compiled.runtime,
  nyx.pascal.project, nyx.studio.projectinputs, nyx.studio.sourceprojection,
  nyx.test.pascal.project, nyx.test.projectinputs.native;

type
  { Own exactly one freshly compiled process family. All native window/control
    queries and messages are restricted to that held process's PID. Normal close
    joins the UI loop; failure retires only this private family. There is no
    global input, clipboard, socket, browser or unrelated service access. }
  TLaunchedApplication = class
  private
    FProcess: TNyxCompilerProcess;
    FWindow: HWND;
    FMatch: TNyxText;
    FFound: HWND;
    function FindFace(const AText: TNyxText): HWND;
    procedure RequireLive;
  public
    constructor Create(const AExecutable: TNyxText);
    destructor Destroy; override;
    function AwaitFace(const AText: TNyxText): HWND;
    procedure Click(const ACaption: TNyxText);
    procedure Edit(const ABefore, AAfter: TNyxText);
    procedure ExpectEditor(const ATitle: TNyxText; ANavigation: Boolean);
    procedure Close;
  end;

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

function ReadText(const APath: String): TNyxText;
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

procedure SaveText(const APath: String; const AText: TNyxText);
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

{ WM_GETTEXT uses the owning process's real Windows controls. UTF-16 at the OS
  boundary preserves supplementary text independently of the console codepage. }
function WindowText(AWindow: HWND): TNyxText;
var
  LBuffer: array[0..16383] of WideChar;
  LReply: DWORD_PTR;
begin
  Result := '';
  LBuffer[0] := #0;

  if SendMessageTimeoutW(AWindow, WM_GETTEXT, Length(LBuffer), LPARAM(@LBuffer[0]),
    SMTO_ABORTIFHUNG, 200, @LReply) <> 0 then
  begin
    Result := UTF8Encode(UnicodeString(PWideChar(@LBuffer[0])));
  end;
end;

function FindMainWindow(AWindow: HWND; AData: LPARAM): BOOL; stdcall;
var
  LApplication: TLaunchedApplication;
  LProcessID: DWORD;
begin
  Result := True;
  LApplication := TLaunchedApplication(PtrUInt(AData));
  GetWindowThreadProcessId(AWindow, @LProcessID);

  if (LProcessID = LApplication.FProcess.ProcessID) and IsWindowVisible(AWindow) and
    (WindowText(AWindow) = CompiledRuntimeTitle) then
  begin
    LApplication.FWindow := AWindow;
    Result := False;
  end;
end;

function FindControl(AWindow: HWND; AData: LPARAM): BOOL; stdcall;
var
  LApplication: TLaunchedApplication;
  LProcessID: DWORD;
begin
  Result := True;
  LApplication := TLaunchedApplication(PtrUInt(AData));
  GetWindowThreadProcessId(AWindow, @LProcessID);

  if (LProcessID = LApplication.FProcess.ProcessID) and IsWindowVisible(AWindow) and
    (WindowText(AWindow) = LApplication.FMatch) then
  begin
    LApplication.FFound := AWindow;
    Result := False;
  end;
end;

{ Assertion-failure evidence for this fixture's own windows only. Class names
  and exact text distinguish an input/oracle mistake from a callback failure;
  no screenshot or unrelated process/window content is collected. }
function DescribeControl(AWindow: HWND; AData: LPARAM): BOOL; stdcall;
var
  LApplication: TLaunchedApplication;
  LProcessID: DWORD;
  LText: TNyxText;
  LClass: array[0..63] of WideChar;
begin
  Result := True;
  LApplication := TLaunchedApplication(PtrUInt(AData));
  GetWindowThreadProcessId(AWindow, @LProcessID);

  if (LProcessID = LApplication.FProcess.ProcessID) and IsWindowVisible(AWindow) then
  begin
    LText := WindowText(AWindow);
    GetClassNameW(AWindow, LClass, Length(LClass));
    WriteLn('Owned face ', UTF8Encode(UnicodeString(LClass)), ' base64: ',
      NyxEncodeBase64(NyxEncodeUTF8(LText), 65536));
  end;
end;

constructor TLaunchedApplication.Create(const AExecutable: TNyxText);
var
  LStarted: QWord;
  LIndex: Integer;
  LEntry: String;
begin
  inherited Create;
  Check(FileExists(AExecutable), 'The admitted native artifact exists');
  FProcess := TNyxCompilerProcess.Create(nil);
  FProcess.Executable := AExecutable;
  FProcess.CurrentDirectory := ExtractFilePath(AExecutable);
  FProcess.Options := [poNoConsole];
  for LIndex := 1 to GetEnvironmentVariableCount do
  begin
    LEntry := GetEnvironmentString(LIndex);

    if not SameText(Copy(LEntry, 1, Pos('=', LEntry) - 1), 'NYX_STUDIO_RESOURCE_REPORT') then
    begin
      FProcess.Environment.Add(LEntry);
    end;
  end;
  FProcess.Execute;
  LStarted := GetTickCount64;
  repeat
    RequireLive;
    EnumWindows(@FindMainWindow, LPARAM(PtrUInt(Self)));

    if FWindow <> 0 then
    begin
      Break;
    end;

    if GetTickCount64 - LStarted > 15000 then
    begin
      raise Exception.Create('The exact compiled application did not show its main window');
    end;
    Sleep(10);
  until False;
  Check(True, 'The exact process mounts the compiled application entry point');
end;

destructor TLaunchedApplication.Destroy;
begin
  { The process adapter joins its private family even on an assertion failure.
    Tests never enumerate or stop other applications by executable name. }
  FProcess.Free;
  inherited Destroy;
end;

procedure TLaunchedApplication.RequireLive;
begin

  if not FProcess.Running then
  begin
    raise Exception.Create('The compiled application exited before the requested control input');
  end;
end;

function TLaunchedApplication.FindFace(const AText: TNyxText): HWND;
begin
  RequireLive;
  FMatch := AText;
  FFound := 0;
  EnumChildWindows(FWindow, @FindControl, LPARAM(PtrUInt(Self)));
  Result := FFound;
end;

function TLaunchedApplication.AwaitFace(const AText: TNyxText): HWND;
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    Result := FindFace(AText);

    if Result <> 0 then
    begin
      Exit;
    end;

    if GetTickCount64 - LStarted > 8000 then
    begin
      EnumChildWindows(FWindow, @DescribeControl, LPARAM(PtrUInt(Self)));
      { Failure receipts carry only the explicitly expected fixture text, never
        other applications or a screenshot of the user's desktop. }
      raise Exception.Create('Compiled control text was not observed; expected UTF-8 base64: ' +
        NyxEncodeBase64(NyxEncodeUTF8(AText), 65536));
    end;
    Sleep(10);
  until False;
end;

procedure TLaunchedApplication.Click(const ACaption: TNyxText);
var
  LControl: HWND;
  LReply: DWORD_PTR;
begin
  LControl := AwaitFace(ACaption);
  Check(IsWindowEnabled(LControl), 'The requested real control permits activation');
  Check(SendMessageTimeoutW(LControl, BM_CLICK, 0, 0, SMTO_ABORTIFHUNG, 2000,
    @LReply) <> 0, 'The real native button handles activation on its UI thread');
end;

procedure TLaunchedApplication.Edit(const ABefore, AAfter: TNyxText);
var
  LControl: HWND;
  LClass: array[0..63] of WideChar;
  LValue: UnicodeString;
  LReply: DWORD_PTR;
begin
  LControl := AwaitFace(ABefore);
  GetClassNameW(LControl, LClass, Length(LClass));
  Check(UnicodeString(LClass) = 'Edit', 'Editing targets an actual native text control');
  LValue := UTF8Decode(AAfter);
  { Replacing the selected text uses the native editing path. The retained first
    attempt used WM_SETTEXT: it changed the multiline HWND text without publishing
    that memo's binding. Do not mistake a caption assignment for user editing. }
  Check(SendMessageTimeoutW(LControl, EM_SETSEL, 0, -1,
    SMTO_ABORTIFHUNG, 2000, @LReply) <> 0, 'Native editing selects this control text');
  Check(SendMessageTimeoutW(LControl, EM_REPLACESEL, 1, LPARAM(PWideChar(LValue)),
    SMTO_ABORTIFHUNG, 2000, @LReply) <> 0, 'Native text editing reaches the owned process');
  Check(WindowText(LControl) = AAfter, 'The real editor retains exact supplementary Unicode');
end;

procedure TLaunchedApplication.ExpectEditor(const ATitle: TNyxText; ANavigation: Boolean);
begin
  Check(AwaitFace(ATitle) <> 0, 'The current runtime has its independent title default');
  Check(AwaitFace(CompiledRuntimeMemo) <> 0, 'The current runtime has its independent memo default');
  Check(AwaitFace(CompiledRuntimeStatus) <> 0, 'The callback receipt starts with its own default');
  Check((FindFace('Compose') <> 0) = ANavigation, 'Automatic navigation is present only in full applications');
  Check((FindFace('Review') <> 0) = ANavigation, 'Focused applications do not leak unrelated navigation');
end;

procedure TLaunchedApplication.Close;
var
  LStarted: QWord;
begin
  RequireLive;
  Check(PostMessageW(FWindow, WM_CLOSE, 0, 0), 'Normal close reaches only this compiled main window');
  LStarted := GetTickCount64;
  while FProcess.FamilyRunning do
  begin

    if GetTickCount64 - LStarted > 15000 then
    begin
      raise Exception.Create('The compiled application did not gracefully retire its UI loop');
    end;
    Sleep(10);
  end;
  Check(FProcess.ExitStatus = 0, 'The compiled application returns success after ordinary shutdown');
  FWindow := 0;
end;

procedure Run;
const
  CDefault: TNyxText = 'A clear plan';
  CRebuilt: TNyxText = 'A fresh plan';
  CEdited: TNyxText = 'A native plan 🌙';
  CDetail: TNyxText = 'An exact reply 𐐷';
  CReceipt: TNyxText = '1 | A native plan 🌙 | An exact reply 𐐷 | stamped';
  CSecond: TNyxText = '2 | A native plan 🌙 | An exact reply 𐐷 | stamped';
  CJoined: TNyxText = 'A native plan 🌙 ~ An exact reply 𐐷';
var
  LTools: TNyxDataValue;
  LOutputs: TNyxOutputConfiguration;
  LDirectories: TNyxStudioDirectories;
  LExecutor: TNyxBuildExecutor;
  LDocument: TNyxDocument;
  LRebuilt: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LFrame: TNyxSourceCheckpoint;
  LSource: TNyxText;
  LRebuiltSource: TNyxText;
  LBefore: TNyxText;
  LOriginal: TLaunchedApplication;
  LFocused: TLaunchedApplication;
  LNew: TLaunchedApplication;
  LResults: TJSONArray;
  LDirectory: TNyxText;
  LView: TNyxText;
  LIndex: Integer;
  LProjectMode: Boolean;
  LProjectInputs: INyxPascalProject;
  LRebuiltInputs: INyxPascalProject;
  LProject: INyxPascalProjectBuild;
  LRebuiltProject: INyxPascalProjectBuild;
  LProjectReceipts: TNyxText;

  { Exercise the real production executor and unchanged generated launchers.
    Complete source and design receipts stay in each invocation-owned job. }
  function Build(ADocument: TNyxDocument; const ASource, AScope, AView: TNyxText;
    ATarget: TNyxBuildTarget): TNyxText;
  var
    LReply: TJSONObject;
    LExpected: TNyxDocument;
    LPrefix: TNyxText;
    LBody: TNyxText;
    LSuffix: TNyxText;
    LCompiledPrefix: TNyxText;
    LCompiledBody: TNyxText;
    LCompiledSuffix: TNyxText;
    LProjectReceipt: INyxPascalProjectBuild;
    LScope: TNyxBuildScope;
    LInputIndex: Integer;
  begin
    WriteLn('Building ', NyxBuildTargetName(ATarget), ' ', AScope, ' ', AView);

    if LProjectMode then
    begin
      LProjectReceipt := LProject;

      if ADocument = LRebuilt then
      begin
        LProjectReceipt := LRebuiltProject;
      end;
      LScope := bsApplication;

      if AScope = 'view' then
      begin
        LScope := bsView;

        if AView = 'PlanEditor' then
        begin
          LScope := bsReusable;
        end;
      end;
      LReply := LExecutor.BuildProject(LProjectReceipt, ATarget, LScope, AView);
    end
    else
    begin
      LWorkspace.Accept(ADocument, ASource);
      LFrame := LWorkspace.Capture(ADocument);
      LReply := LExecutor.Build(ADocument, NyxBuildTargetName(ATarget), AScope,
        AView, ASource, LFrame);
    end;
    try

      if not LReply.Get('ok', False) then
      begin
        raise Exception.Create('Actual application compilation failed: ' + LReply.Get('log', ''));
      end;
      Check(True, 'The selected actual compiler accepts the companion and launcher');
      Result := LDirectories.Jobs + LReply.Get('build', '') + PathDelim;
      LExpected := nil;
      try

        if AScope = 'view' then
        begin
          LExpected := CloneNyxViewDocument(ADocument, ADocument.Find(AView));

          if AView = 'PlanEditor' then
          begin
            Check((LExpected.Count = 0) and (LExpected.ComponentCount = 1),
              'A diagram-retained reusable focus runs without inventing a page');
          end;
        end
        else
        begin
          LExpected := ADocument.Clone;
        end;
        Check(ReadText(Result + 'design.nyx') = TNyxCodec.Encode(LExpected),
          'The compiler retains the exact independently owned application/view context');
      finally
        LExpected.Free;
      end;
      SplitNyxSourceFrame(ASource, LPrefix, LBody, LSuffix);
      SplitNyxSourceFrame(ReadText(Result + 'nyx.generated.view.pas'),
        LCompiledPrefix, LCompiledBody, LCompiledSuffix);
      Check((LPrefix = LCompiledPrefix) and (LSuffix = LCompiledSuffix),
        'Ordinary source isolation retains every exact handwritten callback byte');
      if LProjectMode then
      begin
        Check(ReadText(Result + 'nyx.generated.view.pas') = ASource,
          'Project application/view compilation preserves the exact main unit including includes');
        for LInputIndex := 0 to LProjectReceipt.Inputs.Count - 1 do
        begin
          Check(ReadText(Result + 'project' + PathDelim +
            LProjectReceipt.Inputs.Input(LInputIndex).Name) =
            LProjectReceipt.Inputs.Input(LInputIndex).Text,
            'Application/focused jobs retain the exact captured unit/include inventory');
        end;
      end
      else
      begin
        Check(LWorkspace.Capture.Source = ASource, 'Compilation preserves the accepted source checkpoint');
      end;
      LResults.Add(LReply);
      LReply := nil;
    finally
      LReply.Free;
    end;
  end;

begin

  LProjectMode := (ParamCount = 5) and (ParamStr(5) = '--project-inputs');

  if ((ParamCount <> 4) and not LProjectMode) or
    DirectoryExists(ParamStr(3)) or FileExists(ParamStr(3)) then
  begin
    raise Exception.Create('Supply repository, toolchain JSON, NEW owned runtime and native platform');
  end;
  LTools := TNyxDataValue.ParseJSON(ReadText(ParamStr(2)));
  LDirectories := TNyxStudioDirectories.ForRepository(ParamStr(1))
    .RunningIn(ParamStr(3)).EnrollingProject(ParamStr(3));
  LOutputs := TNyxOutputConfiguration.Create;
  LExecutor := nil;
  LDocument := nil;
  LRebuilt := nil;
  LOriginal := nil;
  LFocused := nil;
  LNew := nil;
  LWorkspace := TNyxSourceWorkspace.Create;
  LResults := TJSONArray.Create;
  try
    LOutputs.SetField('fpc', LTools.Field('LCL_FPC').AsText);
    LOutputs.SetField('lazarus', LTools.Field('LAZARUS').AsText);
    LOutputs.SetField('platform', ParamStr(4));
    LOutputs.SetField('widgetset', 'win32');
    LOutputs.SetField('pas2js', LTools.Field('PAS2JS').AsText);
    LOutputs.SetField('runtime', LTools.Field('PAS2JS_RUNTIME').AsText);
    LExecutor := TNyxBuildExecutor.Create(LDirectories, LOutputs.Encode);
    LDocument := CompiledRuntimeDocument(CDefault, LSource);
    LBefore := TNyxCodec.Encode(LDocument);

    if LProjectMode then
    begin
      LProjectInputs := CompiledRuntimeProject(LSource, CDefault);
      LProject := LExecutor.ProjectSource(LProjectInputs, btNativeLCL, spcChecked);
      Check((LProject.Projection.State = spsExecuted) and
        (LProject.Projection.Design = LBefore),
        'Actual native main/helper includes construct the exact complete runtime design');
      Check(Pos('0 unfreed memory blocks', LProject.RuntimeLog) > 0,
        'Captured multi-file native construction retires its independent source tree');
      LSource := LProject.Inputs.Source;
    end;
    LDirectory := Build(LDocument, LSource, 'application', '', btNativeLCL);
    LOriginal := TLaunchedApplication.Create(LDirectory + 'nyx_native.exe');
    LOriginal.ExpectEditor(CDefault, True);
    LOriginal.Edit(CDefault, CEdited);
    LOriginal.Edit(CompiledRuntimeMemo, CDetail);
    LOriginal.Click('Record plan');
    Check(LOriginal.AwaitFace(CReceipt) <> 0, 'Both ordered authored callbacks see live input bindings');
    LOriginal.Click('Join plan');
    Check(LOriginal.AwaitFace(CJoined) <> 0, 'A saved Logic route consumes the actual runtime bindings');
    LOriginal.Click('Review');
    Check(LOriginal.AwaitFace(CEdited) <> 0, 'Another page observes the retained runtime title');
    Check(LOriginal.AwaitFace(CDetail) <> 0, 'Another page observes the retained runtime memo');
    Check(LOriginal.AwaitFace(CReceipt) <> 0, 'Navigation retains the composed callback result');
    LOriginal.Click('Compose');
    Check(LOriginal.AwaitFace(CEdited) <> 0, 'Returning remounts the existing runtime values');
    LOriginal.Click('Record plan');
    Check(LOriginal.AwaitFace(CSecond) <> 0, 'Remounting neither drops nor duplicates callback registrations');
    for LIndex := 0 to 1 do
    begin
      LView := 'compose';

      if LIndex = 1 then
      begin
        LView := 'PlanEditor';
      end;
      LDirectory := Build(LDocument, LSource, 'view', LView, btNativeLCL);
      LFocused := TLaunchedApplication.Create(LDirectory + 'nyx_native.exe');
      LFocused.ExpectEditor(CDefault, False);
      LFocused.Edit(CDefault, CEdited);
      LFocused.Edit(CompiledRuntimeMemo, CDetail);
      LFocused.Click('Record plan');
      Check(LFocused.AwaitFace(CReceipt) <> 0,
        'An independently focused page/reusable executes its exact authored callbacks');
      LFocused.Click('Join plan');
      Check(LFocused.AwaitFace(CJoined) <> 0,
        'An independently focused page/reusable executes its saved Logic route');
      Check(LOriginal.AwaitFace(CSecond) <> 0, 'Focused runtime changes cannot alter the already running application');
      LFocused.Close;
      FreeAndNil(LFocused);
    end;
    LRebuilt := CompiledRuntimeDocument(CRebuilt, LRebuiltSource);

    if LProjectMode then
    begin
      LRebuiltInputs := CompiledRuntimeProject(LRebuiltSource, CRebuilt);
      Check(LRebuiltInputs.Source = LProjectInputs.Source,
        'Changing only a helper include preserves every main-unit byte');
      LRebuiltProject := LExecutor.ProjectSource(LRebuiltInputs, btNativeLCL, spcChecked);
      Check((LRebuiltProject.Projection.State = spsExecuted) and
        (LRebuiltProject.Projection.Design = TNyxCodec.Encode(LRebuilt)),
        'A changed helper snapshot constructs its own exact accepted defaults');
      Check(LProject.Inputs.Input(2).Text <> LRebuiltProject.Inputs.Input(2).Text,
        'Earlier construction retains its original include meaning after a new project capture');
      LRebuiltSource := LRebuiltProject.Inputs.Source;
    end;
    LDirectory := Build(LRebuilt, LRebuiltSource, 'application', '', btNativeLCL);
    LNew := TLaunchedApplication.Create(LDirectory + 'nyx_native.exe');
    LNew.ExpectEditor(CRebuilt, True);
    Check(LOriginal.AwaitFace(CSecond) <> 0, 'A new build/launch preserves the existing live application');
    LNew.Close;
    FreeAndNil(LNew);
    Build(LDocument, LSource, 'application', '', btBrowser);
    Build(LDocument, LSource, 'view', 'compose', btBrowser);
    Build(LDocument, LSource, 'view', 'PlanEditor', btBrowser);
    Build(LRebuilt, LRebuiltSource, 'application', '', btBrowser);
    Check(TNyxCodec.Encode(LDocument) = LBefore,
      'All native interaction/builds preserve the source document and its default state');
    Check(LOriginal.AwaitFace(CSecond) <> 0, 'Both-target compilation leaves the old live application usable');
    LOriginal.Close;
    FreeAndNil(LOriginal);

    if LProjectMode then
    begin
      Inc(GChecks, RunNyxProjectInputGuards(LExecutor, LDirectories,
        LProject, LRebuiltProject, LProjectReceipts));
      SaveText(IncludeTrailingPathDelimiter(ParamStr(3)) + 'project-input-guards.json', LProjectReceipts);
      SaveText(IncludeTrailingPathDelimiter(ParamStr(3)) + 'project-constructors.json',
        NyxArray([NyxData(LProject.Reference.Name),
          NyxData(LRebuiltProject.Reference.Name)]).ToJSON);
    end;
    SaveText(IncludeTrailingPathDelimiter(ParamStr(3)) + 'compiled-runtime.json', LResults.AsJSON);
  finally
    LNew.Free;
    LFocused.Free;
    LOriginal.Free;
    LResults.Free;
    LWorkspace.Free;
    LRebuilt.Free;
    LDocument.Free;
    LExecutor.Free;
    LOutputs.Free;
  end;
end;

begin
  try
    Run;
    WriteLn('PASS ', GChecks, ' actual compiled native application/page/reusable checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
