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

program nyx_native_build_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Interfaces, Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Windows,
  Graphics, IntfGraphics, FPWritePNG,
  nyx.text, nyx.types, nyx.model, nyx.codec, nyx.data, nyx.editing, nyx.editing.lcl,
  nyx.studio.lcl, nyx.studio.mcp, nyx.studio.projects,
  nyx.studio.exchange, nyx.studio.outputs, nyx.studio.agents,
  nyx.studio.buildjobs, nyx.studio.workspaces, nyx.studio.preview,
  nyx.studio.preview.lcl, nyx.studio.editorbuild, nyx.studio.buildview, nyx.studio.builds,
  nyx.generated.view;

type
  { Exercise the real private protocol and compiler workers without starting a
    listener. The queued adapter implements the public UI-thread transport
    lifetime, not a replacement compiler or simulated job result. }
  TDirectEditorExchange = class(TNyxStudioEditorExchange)
  private
    FReply: TNyxEditorReply;
    FToken: TNyxText;
    FBody: TNyxText;
    FConnect: Boolean;
    FTick: TNyxEditorTick;
    FTimer: TTimer;
    procedure Deliver(AData: PtrInt);
    procedure TimerTick(ASender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Post(AConnect: Boolean; const AToken, ABody: TNyxText;
      AReply: TNyxEditorReply); override;
    procedure CancelRequest; override;
    procedure Schedule(ADelayMS: Integer; ATick: TNyxEditorTick); override;
    procedure CancelTick; override;
  end;

  TBuildStudio = class(TNyxNativeStudio)
  protected
    function CreateEditorExchange: TNyxStudioEditorExchange; override;
  end;

  TControlAccess = class(TControl);
  TFailureObserver = class
    Error: TNyxText;
    SaveError: Boolean;
    Saves: Integer;
    Prepared: Boolean;
    PreparationSucceeded: Boolean;
    PreparationError: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
    procedure SaveProfile(const AProfile: TNyxText);
    procedure PreviewPrepared(ASucceeded: Boolean; const AError: TNyxText);
  end;

var
  GEngine: TNyxStudioMCP;
  GStudio: TBuildStudio;
  GForm: TForm;
  GObserver: TFailureObserver;
  GCount: Integer;
  GPair: TNyxProjectPair;
  GToken: TNyxText;
  GDirectory: TNyxText;
  GLastJob: TNyxText;
  GArtifactDirectory: TNyxText;
  GWindow: HWND;
  GWindowProcess: DWORD;
  GExpectedTitle: TNyxText;
  GPublishArtifacts: Boolean;
  GPublishedJob: TNyxText;
  GMemos: array[0..1] of HWND;
  GMemoCount: Integer;
  GSend: HWND;

procedure PublishOwnedArtifact(const AResult: TNyxDataValue); forward;
procedure Capture(const AName: TNyxText); forward;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if GObserver.Error <> '' then
  begin
    raise Exception.Create('Native compiler callback: ' + GObserver.Error);
  end;

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GCount);
  WriteLn('Check ', GCount, ': ', AReason);
  Flush(Output);
end;

function ReadBytes(const APath: TNyxText): TNyxText;
var
  LFile: TFileStream;
begin
  LFile := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Result, LFile.Size);

    if Result <> '' then
    begin
      LFile.ReadBuffer(Result[1], Length(Result));
    end;
  finally
    LFile.Free;
  end;
end;

procedure Save(const AName, AText: TNyxText);
var
  LFile: TFileStream;
begin
  LFile := TFileStream.Create(GDirectory + AName, fmCreate);
  try

    if AText <> '' then
    begin
      LFile.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LFile.Free;
  end;
end;

constructor TDirectEditorExchange.Create;
begin
  inherited Create;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.OnTimer := TimerTick;
end;

destructor TDirectEditorExchange.Destroy;
begin
  CancelRequest;
  CancelTick;
  FTimer.Free;
  inherited Destroy;
end;

procedure TDirectEditorExchange.Post(AConnect: Boolean; const AToken, ABody: TNyxText;
  AReply: TNyxEditorReply);
begin
  CancelRequest;
  FConnect := AConnect;
  FToken := AToken;
  FBody := ABody;
  FReply := AReply;
  Application.QueueAsyncCall(Deliver, 0);
end;

procedure TDirectEditorExchange.Deliver(AData: PtrInt);
var
  LResult: TNyxDataValue;
  LReply: TNyxEditorReply;
  LText: TNyxText;
  LStatus: Integer;
begin
  LReply := FReply;
  FReply := nil;
  LStatus := 200;
  try

    if FConnect then
    begin
      LResult := GEngine.ConnectEditor(TNyxDataValue.ParseJSON(FBody));
    end
    else
    begin
      LResult := GEngine.EditorExchange(FToken, TNyxDataValue.ParseJSON(FBody));
    end;
    { Serve this test's exact newly completed artifact before the ordinary
      consumer observes it. The engine result and worker/download remain real;
      this is explicit staging into the unchanged listener's admitted build root. }

    if GPublishArtifacts and NyxAgentHas(LResult, 'buildReply') then
    begin

      if NyxAgentHas(LResult.Field('buildReply'), 'manifest') and
        (LResult.Field('buildReply').Field('state').AsText = 'succeeded') and
        (LResult.Field('buildReply').Field('job').AsText <> GPublishedJob) then
      begin
        PublishOwnedArtifact(LResult.Field('buildReply'));
      end;
    end;
    LText := LResult.ToJSON;
  except
    on LException: Exception do
    begin
      LStatus := 409;
      LText := NyxObject([NyxField('error', NyxData(TNyxText(LException.Message)))]).ToJSON;
    end;
  end;

  if Assigned(LReply) then
  begin
    LReply(LStatus, LText);
  end;
end;

procedure TDirectEditorExchange.CancelRequest;
begin
  FReply := nil;
  Application.RemoveAsyncCalls(Self);
end;

procedure TDirectEditorExchange.Schedule(ADelayMS: Integer; ATick: TNyxEditorTick);
begin
  FTick := ATick;
  FTimer.Interval := ADelayMS;
  FTimer.Enabled := True;
end;

procedure TDirectEditorExchange.CancelTick;
begin
  FTimer.Enabled := False;
  FTick := nil;
end;

procedure TDirectEditorExchange.TimerTick(ASender: TObject);
var
  LTick: TNyxEditorTick;
begin
  FTimer.Enabled := False;
  LTick := FTick;
  FTick := nil;

  if Assigned(LTick) then
  begin
    LTick;
  end;
end;

function TBuildStudio.CreateEditorExchange: TNyxStudioEditorExchange;
begin
  Result := TDirectEditorExchange.Create;
end;

procedure TFailureObserver.Failed(ASender: TObject; AException: Exception);
begin
  Error := AException.Message;
end;

procedure TFailureObserver.SaveProfile(const AProfile: TNyxText);
var
  LProfile: TNyxOutputConfiguration;
begin

  if SaveError then
  begin
    raise Exception.Create('Qualification profile persistence refused');
  end;
  LProfile := TNyxOutputConfiguration.Decode(AProfile);
  try
    Inc(Saves);
  finally
    LProfile.Free;
  end;
end;

procedure TFailureObserver.PreviewPrepared(ASucceeded: Boolean; const AError: TNyxText);
begin
  Prepared := True;
  PreparationSucceeded := ASucceeded;
  PreparationError := AError;
end;

procedure Pump;
begin
  Application.ProcessMessages;
  Sleep(5);
end;

procedure Ready;
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    Pump;

    if GetTickCount64 - LStarted > 10000 then
    begin
      raise Exception.Create('Native compiler editor did not synchronize');
    end;
  until GStudio.Agents.Connected and not GStudio.Agents.Busy and not GStudio.Agents.Conflict;
end;

procedure Click(const AID: TNyxText);
var
  LPaints: Integer;
begin
  LPaints := GStudio.PaintCount;
  if GStudio.ShellView.Root.Find(AID) <> nil then
  begin
    TControlAccess(GStudio.ShellView.ControlFor(AID)).Click;
  end
  else
  begin
    TControlAccess(GStudio.SourceView.ControlFor(AID)).Click;
  end;
  Check(GStudio.PaintCount = LPaints, 'native compiler action defers its paint: ' + AID);
  Pump;
end;

function OperatorBuild(const AArguments: TNyxDataValue): TNyxDataValue;
begin
  Result := GEngine.EditorExchange(GToken, NyxObject([
    NyxField('op', NyxData('build')), NyxField('after', NyxData(0)),
    NyxField('build', AArguments)])).Field('buildReply');
end;

procedure ExerciseBuildControls(const AFixture: TNyxText);
var
  LProfile, LHoldProfile: TNyxOutputConfiguration;
  LBefore, LAfter, LReply, LJobs, LItems: TNyxDataValue;
  LBlockers: array[0..1] of TNyxBuildJobRef;
  LJob: TNyxBuildJobRef;
  LRow: TNyxNode;
  LRevision, LIndex: Integer;
  LStarted: QWord;
  LOutput: TNyxBuildOutputRef;
  LPreview: Integer;

  procedure AwaitJob(const AState: TNyxText);
  var
    LItemIndex: Integer;
  begin
    LStarted := GetTickCount64;
    repeat
      Pump;
      LItems := GStudio.Agents.BuildJobs.Field('items');
      LJob := Default(TNyxBuildJobRef);
      for LItemIndex := 0 to LItems.Count - 1 do
      begin

        if (LItems.Item(LItemIndex).Field('target').AsText = 'lcl') and
          (LItems.Item(LItemIndex).Field('state').AsText = AState) then
        begin
          LJob := NyxBuildJob(LItems.Item(LItemIndex).Field('job').AsText);
        end;
      end;

      if LJob.ID <> '' then
      begin
        Break;
      end;

      if GetTickCount64 - LStarted > 5000 then
      begin
        Save('panel-timeout.nyx', GStudio.Agents.BuildJobs.ToJSON);
        raise Exception.Create('Ordinary build panel did not observe ' + AState + ': ' +
          GStudio.Status + ' / ' + GStudio.Agents.BuildReply.ToJSON);
      end;
    until False;
  end;

  procedure CancelRow;
  var
    LItemIndex: Integer;
  begin
    LRow := nil;
    for LItemIndex := 0 to LItems.Count - 1 do
    begin

      if LItems.Item(LItemIndex).Field('job').AsText = LJob.ID then
      begin
        LRow := GStudio.ShellView.Root.Find('studio-build-' + IntToStr(LItemIndex) + '-cancel');
      end;
    end;
    Check((LRow <> nil) and (LRow.Prop('enabled') <> 'false') and
      (LRow.Extensions.Value(NyxStudioCancelBuildKey).AsText = LJob.ID),
      'ordinary cancel row retains exact job identity independently of display order');
    Click(LRow.ID);
    LStarted := GetTickCount64;
    repeat
      Pump;
      LReply := OperatorBuild(NyxCompilerStatus(LJob));

      if LReply.Field('state').AsText = 'cancelled' then
      begin
        Break;
      end;

      if GetTickCount64 - LStarted > 5000 then
      begin
        raise Exception.Create('Ordinary cancel action did not retire its compiler');
      end;
    until False;
    Ready;
    Check((LReply.Field('artifact').AsText = '') and (LReply.Field('manifest').Count = 0),
      'visible cancellation reaches joined terminal state without an artifact');
    Check(GStudio.CompiledPreviewProcessID = LPreview,
      'visible cancellation retains the last actual running compiled preview');
    Check(GStudio.ShellView.Root.Find('action-compiled-run').Prop('enabled') <> 'false',
      'accepted artifact remains runnable after another job is cancelled');
  end;

begin
  LBefore := GEngine.EditorExchange(GToken, NyxObject([NyxField('op', NyxData('observe'))]));
  LRevision := LBefore.Field('session').Field('revision').AsInteger;
  LPreview := GStudio.CompiledPreviewProcessID;
  Check(LPreview <> 0, 'build cancellation qualification starts with an actual accepted preview');
  LReply := OperatorBuild(NyxObject([NyxField('mode', NyxData('profile'))]));
  LProfile := TNyxOutputConfiguration.Decode(LReply.Field('profile').ToJSON);
  LHoldProfile := TNyxOutputConfiguration.Decode(LProfile.Encode);
  try
    { Only the isolated runtime's two blocking compilers use the owned Pascal
      fixture. Restore the original immutable output before the ordinary LCL
      request so its accepted preview and current profile remain identical. }
    Save('build/studio/jobs/fixture.mode', 'editor-hold');
    LHoldProfile.SetField('pas2js', AFixture);
    LReply := OperatorBuild(NyxObject([NyxField('mode', NyxData('profile')),
      NyxField('expectedOutputID', LReply.Field('outputID')),
      NyxField('profile', TNyxDataValue.ParseJSON(LHoldProfile.Encode))]));
    LOutput := NyxBuildOutput(LReply.Field('outputID').AsText);
    for LIndex := 0 to High(LBlockers) do
    begin
      LReply := OperatorBuild(NewNyxCompilerRequest.Target(btBrowser).Scope(bsApplication)
        .AtRevision(LRevision).Output(LOutput)
        .Operation(NyxBuildOperation('panel-blocker-' + IntToStr(LIndex))).Arguments);
      LBlockers[LIndex] := NyxBuildJob(LReply.Field('job').AsText);
    end;
    LReply := OperatorBuild(NyxObject([NyxField('mode', NyxData('profile')),
      NyxField('expectedOutputID', NyxData(LOutput.ID)),
      NyxField('profile', TNyxDataValue.ParseJSON(LProfile.Encode))]));
    Check(LReply.Field('outputID').AsText = NyxBuildFingerprint(LProfile.Encode),
      'ordinary queued job uses the unchanged accepted output identity');
    Click('action-builds');
    Click('action-build-view');
    AwaitJob('queued');
    Check((GStudio.Agents.BuildJobs.Field('running').AsInteger = 2) and
      (GStudio.Agents.BuildJobs.Field('queued').AsInteger = 1),
      'ordinary Nyx panel observes two real workers and its queued request');
    LJobs := OperatorBuild(NyxCompilerJobs(cjfAll, 1, 1));
    Check((LJobs.Field('items').Count = 1) and (LJobs.Field('total').AsInteger >= 3),
      'operator discovery pages metadata without returning the document');
    LReply := OperatorBuild(NyxCompilerCancel(LJob, LRevision - 1,
      NyxBuildOperation('panel-stale-cancel')));
    Check(LReply.Field('state').AsText = 'rejected',
      'stale cancellation refuses while the visible immutable request remains queued');
    Capture('native-builds-desktop');
    GForm.ClientWidth := 390;
    Pump;
    Capture('native-builds-narrow');
    GForm.ClientWidth := 1280;
    Pump;
    CancelRow;
    { Exercise the same row contract against a real running fixture compiler.
      Its held lifetime makes input timing deterministic without pretending an
      uncontrolled fast real compiler must still be running after a UI paint. }
    LJob := LBlockers[0];
    LItems := GStudio.Agents.BuildJobs.Field('items');
    Check(OperatorBuild(NyxCompilerStatus(LJob)).Field('state').AsText = 'running',
      'running cancellation starts with an actual held compiler worker');
    CancelRow;
    OperatorBuild(NyxCompilerCancel(LBlockers[1], LRevision,
      NyxBuildOperation('retire-panel-blocker-1')));
    LStarted := GetTickCount64;
    repeat
      Pump;
      LJobs := OperatorBuild(NyxCompilerJobs);

      if GetTickCount64 - LStarted > 5000 then
      begin
        raise Exception.Create('Owned panel blockers did not join');
      end;
    until LJobs.Field('total').AsInteger = 0;
    LAfter := GEngine.EditorExchange(GToken, NyxObject([NyxField('op', NyxData('observe'))]));
    Save('build-controls-before.nyx', LBefore.ToJSON);
    Save('build-controls-after.nyx', LAfter.ToJSON);
    Check(LAfter.Field('project').AsText = LBefore.Field('project').AsText,
      'queued/running panel cancellation retains the exact accepted pair');
    Check(LAfter.Field('compiler').ToJSON = LBefore.Field('compiler').ToJSON,
      'queued/running panel cancellation retains the accepted compiler report');
    Check((LAfter.Field('session').Field('revision').AsInteger = LRevision) and
      (LAfter.Field('session').Field('canUndo').ToJSON = LBefore.Field('session').Field('canUndo').ToJSON),
      'build controls do not add document history');
    Click('action-builds');
  finally
    LHoldProfile.Free;
    LProfile.Free;
  end;
end;

procedure Terminal(const ATarget, AScope: TNyxText; ASuccess: Boolean = True);
var
  LStarted: QWord;
  LReply: TNyxDataValue;
  LState: TNyxText;
begin
  LStarted := GetTickCount64;
  repeat
    Pump;
    LReply := GStudio.Agents.BuildReply;
    LState := '';

    if NyxAgentHas(LReply, 'state') then
    begin
      LState := LReply.Field('state').AsText;
    end;

    if NyxAgentHas(LReply, 'job') and (LReply.Field('job').AsText = GLastJob) then
    begin
      LState := '';
    end;

    if (LState = 'rejected') or (Pos('changed', GStudio.Status) > 0) then
    begin
      raise Exception.Create('Native build refused: ' + GStudio.Status);
    end;

    if GetTickCount64 - LStarted > 60000 then
    begin
      raise Exception.Create('Native compiler observation timed out: ' + GStudio.Status);
    end;
  until ((LState = 'succeeded') or (LState = 'failed')) and NyxAgentHas(LReply, 'currentSource');
  Save(ATarget + '-' + AScope + '-status.json', LReply.ToJSON);
  GLastJob := LReply.Field('job').AsText;
  Check((LState = 'succeeded') = ASuccess, 'real compiler result agrees: ' + ATarget + '/' + AScope);
  Check((LReply.Field('target').AsText = ATarget) and (LReply.Field('scope').AsText = AScope),
    'native compiler retains its exact target and scope');
  Check(LReply.Field('currentSource').AsBoolean and LReply.Field('currentOutput').AsBoolean,
    'compiled native-editor result belongs to its unchanged accepted pair and profile');
  Check(LReply.Field('sourceFingerprint').AsText = NyxBuildFingerprint(GStudio.Session.Source),
    'compiler consumes the unchanged accepted companion');

  if ASuccess then
  begin
    Check((LReply.Field('artifact').AsText <> '') and (LReply.Field('manifest').Count >= 3),
      'successful job returns an admitted artifact and byte manifest');
  end
  else
  begin
    Check((LReply.Field('artifact').AsText = '') and (LReply.Field('diagnostics').Field('total').AsInteger > 0),
      'failed compiler returns diagnostics and no runnable artifact');
  end;
  Check(not GStudio.Agents.Conflict, 'compiler work never freezes the ordinary editor');
  Ready;
end;

procedure PublishOwnedArtifact(const AResult: TNyxDataValue);
var
  LPath: TNyxText;
  LSource: TNyxText;
  LDestination: TNyxText;
  LInput: TFileStream;
  LOutput: TFileStream;
  LIndex: Integer;
begin
  { The already-running qualification listener serves exact immutable artifact
    bytes. Copy only this test's compiler manifest into its admitted build root;
    do not launch a listener, replace its profile or touch a project document. }
  for LIndex := 0 to AResult.Field('manifest').Count - 1 do
  begin
    LPath := AResult.Field('manifest').Item(LIndex).Field('path').AsText;
    Check((Copy(LPath, 1, 11) = 'builds/job-') and (Pos('..', LPath) = 0) and
      (Pos('\', LPath) = 0), 'artifact publication uses only an admitted test job path');
    LPath := Copy(LPath, 8, MaxInt);
    LSource := GDirectory + 'build/studio/jobs/' + LPath;
    LDestination := GArtifactDirectory + LPath;
    Check(Copy(ExpandFileName(LDestination), 1, Length(GArtifactDirectory)) = GArtifactDirectory,
      'artifact destination remains inside the explicitly owned qualification build root');
    ForceDirectories(ExtractFileDir(LDestination));
    LInput := TFileStream.Create(LSource, fmOpenRead or fmShareDenyWrite);
    try
      LOutput := TFileStream.Create(LDestination, fmCreate);
      try
        LOutput.CopyFrom(LInput, LInput.Size);
      finally
        LOutput.Free;
      end;
    finally
      LInput.Free;
    end;
  end;
  GPublishedJob := AResult.Field('job').AsText;
end;

function FindPreviewWindow(AWindow: HWND; AData: LPARAM): BOOL; stdcall;
var
  LProcess: DWORD;
  LBuffer: array[0..255] of WideChar;
  LTitle: WideString;
  LCount: Integer;
begin
  GetWindowThreadProcessID(AWindow, @LProcess);

  if (LProcess = GWindowProcess) and IsWindowVisible(AWindow) then
  begin
    { Lazarus also owns an application/helper window. Admit the document's
      exact titled form, rather than whichever visible window enumerates first. }
    LCount := GetWindowTextW(AWindow, @LBuffer[0], Length(LBuffer));
    SetString(LTitle, PWideChar(@LBuffer[0]), LCount);

    if LTitle = UTF8Decode(GExpectedTitle) then
    begin
      GWindow := AWindow;
      Exit(False);
    end;
  end;
  Result := True;
end;

procedure PreviewRunning(APreviousProcess: Integer = 0);
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    Pump;
    GWindowProcess := GStudio.CompiledPreviewProcessID;
    GWindow := 0;

    if (GWindowProcess <> 0) and (GWindowProcess <> DWORD(APreviousProcess)) then
    begin
      EnumWindows(@FindPreviewWindow, 0);
    end;

    if GetTickCount64 - LStarted > 15000 then
    begin
      raise Exception.Create('Compiled native preview did not mount its own window: ' + GStudio.Status);
    end;
  until GWindow <> 0;
  { Keep qualification windows off the desktop while still allowing Win32 to
    paint their native children. Match only the process this Studio owns. }
  SetWindowPos(GWindow, 0, -30000, -30000, 0, 0, SWP_NOSIZE or SWP_NOZORDER);
  Check(GWindowProcess = DWORD(GStudio.CompiledPreviewProcessID),
    'real compiled executable mounts an owned native application window');
end;

function NativeCaption(AWindow: HWND): WideString;
var
  LBuffer: array[0..1023] of WideChar;
  LCount: DWORD_PTR;
begin
  Result := '';
  LCount := 0;
  { GetWindowText does not retrieve another process's edit-control text.
    Use the documented marshaled system message with a bounded wait, only for
    children of the exact preview process created by this editor. }

  if SendMessageTimeoutW(AWindow, WM_GETTEXT, Length(LBuffer), LPARAM(@LBuffer[0]),
    SMTO_ABORTIFHUNG, 1000, @LCount) <> 0 then
  begin
    SetString(Result, PWideChar(@LBuffer[0]), Integer(LCount));
  end;
end;

function FindPreviewControls(AWindow: HWND; AData: LPARAM): BOOL; stdcall;
var
  LProcess: DWORD;
  LText: WideString;
begin
  GetWindowThreadProcessID(AWindow, @LProcess);

  if LProcess = GWindowProcess then
  begin
    LText := NativeCaption(AWindow);

    if (LText = 'Your reply starts here.') and (GMemoCount < Length(GMemos)) then
    begin
      GMemos[GMemoCount] := AWindow;
      Inc(GMemoCount);
    end
    else if (LText = 'Send reply') and (GSend = 0) then
    begin
      GSend := AWindow;
    end;
  end;
  Result := True;
end;

procedure ExerciseCompiledReply;
var
  LText: WideString;
  LResult: DWORD_PTR;
  LStarted: QWord;
  LParent: HWND;
  LAssociated: Boolean;
  LSwap: HWND;
begin
  LStarted := GetTickCount64;
  repeat
    GMemoCount := 0;
    GSend := 0;
    EnumChildWindows(GWindow, @FindPreviewControls, 0);
    Pump;

    if GetTickCount64 - LStarted > 5000 then
    begin
      raise Exception.Create('Compiled reply controls did not finish mounting');
    end;
  until (GMemoCount = 2) and (GSend <> 0);
  Check((GMemoCount = 2) and (GSend <> 0),
    'compiled page mounts two independent reusable reply controls');
  { Native enumeration is not an ownership order. Pair the button with the memo
    in its own reusable container before asserting instance independence. }
  LParent := GMemos[0];
  LAssociated := False;
  while LParent <> 0 do
  begin

    if LParent = GetParent(GSend) then
    begin
      LAssociated := True;
    end;
    LParent := GetParent(LParent);
  end;

  if not LAssociated then
  begin
    LSwap := GMemos[0];
    GMemos[0] := GMemos[1];
    GMemos[1] := LSwap;
  end;
  LText := 'A reply from the compiled preview';
  Check(SendMessageTimeoutW(GMemos[0], WM_SETTEXT, 0, LPARAM(PWideChar(LText)),
    SMTO_ABORTIFHUNG, 1000, @LResult) <> 0, 'actual compiled memo receives native host input');
  { The default Nyx button reuses Lazarus TCDButton, rather than the Win32
    standard BUTTON procedure. Its qualified Space key path is the public native
    activation contract; BM_CLICK is not a contract of custom LCL controls. }
  Check(SendMessageTimeoutW(GSend, WM_KEYDOWN, VK_SPACE, 1,
    SMTO_ABORTIFHUNG, 1000, @LResult) <> 0, 'actual compiled button receives native key-down');
  Check(SendMessageTimeoutW(GSend, WM_KEYUP, VK_SPACE, LPARAM($C0000001),
    SMTO_ABORTIFHUNG, 1000, @LResult) <> 0, 'actual compiled button receives native key-up');
  LStarted := GetTickCount64;
  repeat
    Pump;

    if GetTickCount64 - LStarted > 3000 then
    begin
      raise Exception.Create('Compiled reply action did not clear its own memo');
    end;
  until NativeCaption(GMemos[0]) = '';
  Check(NativeCaption(GMemos[1]) = 'Your reply starts here.',
    'compiled action preserves the second reusable instance');
end;

procedure ProbeCompiledInput;
var
  LRunner: TNyxLCLCompiledPreview;
  LStarted: QWord;
  LResult: TNyxDataValue;
  LBadManifest: TNyxDataValue;
  LArtifact: TNyxCompiledArtifact;
  LRefused: Boolean;
  LProcess: Integer;
  LDocument: TNyxDocument;
begin
  { Focus a failed cross-process input investigation without repeating compiler
    or Studio journeys. The previous owned job is an explicit retained artifact;
    this probe does not establish service/project currentness or editor reload. }
  LRunner := TNyxLCLCompiledPreview.Create(ParamStr(5), GDirectory + 'input-probe');
  try
    LDocument := BuildNyxDocument;
    try
      GExpectedTitle := LDocument.Title;
    finally
      LDocument.Free;
    end;
    LResult := TNyxDataValue.ParseJSON(ReadBytes(GDirectory + 'lcl-application-status.json'));
    LArtifact := AdmitNyxCompiledArtifact(LResult);
    LRunner.Prepare(LArtifact, GObserver.PreviewPrepared);
    LStarted := GetTickCount64;
    repeat
      Pump;

      if GetTickCount64 - LStarted > 10000 then
      begin
        raise Exception.Create('Retained compiled input preparation timed out');
      end;
    until GObserver.Prepared;
    Check(GObserver.PreparationSucceeded, 'retained artifact downloads and verifies before input probe');
    LRunner.Launch;
    GWindowProcess := LRunner.ProcessID;
    LStarted := GetTickCount64;
    repeat
      GWindow := 0;
      EnumWindows(@FindPreviewWindow, 0);
      Pump;

      if GetTickCount64 - LStarted > 5000 then
      begin
        raise Exception.Create('Retained compiled input window did not mount');
      end;
    until GWindow <> 0;
    SetWindowPos(GWindow, 0, -30000, -30000, 0, 0, SWP_NOSIZE or SWP_NOZORDER);
    ExerciseCompiledReply;
    LProcess := LRunner.ProcessID;
    { Test delivery mismatch at the explicit wire boundary without modifying any
      served file. The admitted size/path remain exact; this fingerprint is
      deliberately wrong. No failed preparation may replace the running app. }
    LBadManifest := NyxObject([NyxField('state', NyxData('succeeded')),
      NyxField('currentSource', NyxData(True)), NyxField('currentOutput', NyxData(True)),
      NyxField('target', NyxData('lcl')), NyxField('job', LResult.Field('job')),
      NyxField('artifact', LResult.Field('artifact')),
      NyxField('manifest', NyxArray([NyxObject([
        NyxField('path', NyxData(LArtifact.RelativePath)),
        NyxField('bytes', NyxData(LArtifact.ByteCount)),
        NyxField('md5', NyxData('00000000000000000000000000000000'))])]))]);
    GObserver.Prepared := False;
    LRunner.Prepare(AdmitNyxCompiledArtifact(LBadManifest), GObserver.PreviewPrepared);
    LStarted := GetTickCount64;
    repeat
      Pump;

      if GetTickCount64 - LStarted > 10000 then
      begin
        raise Exception.Create('Mismatched preview download did not finish');
      end;
    until GObserver.Prepared;
    Check(not GObserver.PreparationSucceeded and not LRunner.Ready and
      (GObserver.PreparationError <> ''), 'HTTP byte mismatch refuses runnable preparation');
    LRefused := False;
    try
      LRunner.Launch;
    except
      on LException: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LRunner.ProcessID = LProcess),
      'failed verification retains the previous process and refuses launch');
    GObserver.Prepared := False;
    LRunner.Prepare(LArtifact, GObserver.PreviewPrepared);
    LRunner.Cancel;
    Pump;
    Check(not GObserver.Prepared and not LRunner.Ready and (LRunner.ProcessID = LProcess),
      'download cancellation detaches its receiver and retains the running app');
    LRunner.Stop;
    Check(LRunner.ProcessID = 0, 'input probe retires its own process');
  finally
    LRunner.Free;
  end;
end;

procedure Capture(const AName: TNyxText);
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
begin
  LBitmap := TBitmap.Create;
  LImage := nil;
  LWriter := nil;
  try
    LBitmap.SetSize(GForm.ClientWidth, GForm.ClientHeight);
    GForm.PaintTo(LBitmap.Canvas, 0, 0);
    LImage := LBitmap.CreateIntfImage;
    LWriter := TFPWriterPNG.Create;
    LImage.SaveToFile(GDirectory + AName + '.png', LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
end;

var
  LDocument: TNyxDocument;
  LProfile: TNyxOutputConfiguration;
  LClaim: TNyxDataValue;
  LBefore: TNyxDataValue;
  LReply: TNyxDataValue;
  LOutputID: TNyxText;
  LStarted: QWord;
  LSource: TNyxText;
  LBroken: TNyxText;
  LOldProcess: Integer;
  LErrorAction: TNyxNode;
  LRefused: Boolean;
  LDiagnostics: TNyxDataValue;
  LProcessHandle: THandle;
  LPresented: TNyxText;
  LPosition: Integer;
begin
  LDocument := nil;
  LProfile := nil;
  try

    if (ParamCount <> 5) and (ParamCount <> 6) and (ParamCount <> 7) then
    begin
      raise Exception.Create('Supply owned repository, profile, semantic source directory, existing artifact build root and HTTP origin');
    end;
    GDirectory := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(1)));
    GArtifactDirectory := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(4)));

    if not FileExists(GDirectory + 'src/nyx.model.pas') or
      not FileExists(GDirectory + 'studio/nyx.studio.outputs.pas') then
    begin
      raise Exception.Create('The owned compiler repository requires its existing Nyx src and studio directories');
    end;
    Application.Initialize;
    GObserver := TFailureObserver.Create;
    Application.OnException := GObserver.Failed;

    if (ParamCount = 6) and (ParamStr(6) = 'probe-input') then
    begin
      ProbeCompiledInput;
      WriteLn('PASS ', GCount, ' focused compiled-input checks');
    end
    else
    begin

      if (ParamCount = 6) and (ParamStr(6) <> 'diagnostics') then
      begin
        raise Exception.Create('The optional qualification mode is probe-input or diagnostics');
      end;

      if (ParamCount = 7) and ((ParamStr(6) <> 'build-controls') or
        not FileExists(ParamStr(7))) then
      begin
        raise Exception.Create('Build controls require their existing owned Pascal compiler fixture');
      end;
      LProfile := TNyxOutputConfiguration.Decode(ReadBytes(ParamStr(2)));
      GEngine := TNyxStudioMCP.Create(GDirectory, 8388, 8389, LProfile.Encode);
      WriteLn('Prepared suspended protocol engine');
      Flush(Output);
      { Never Start this engine. The protocol consumer directly exercises guarded
        operator admission, worker jobs and UI callbacks; live HTTP stays a gate. }
      GEngine.OnOperatorProfileChange := GObserver.SaveProfile;
      LDocument := BuildNyxDocument;
      GExpectedTitle := LDocument.Title;
      GPair.Design := TNyxCodec.Encode(LDocument);
      GPair.Source := ReadBytes(IncludeTrailingPathDelimiter(ParamStr(3)) + 'nyx.generated.view.pas');
      GPair.Draft := '';
      GPair.Pending := False;
      LClaim := GEngine.ConnectEditor(NyxObject([NyxField('op', NyxData('claim')),
        NyxField('project', NyxData(EncodeNyxProject(GPair))),
        NyxField('selection', NyxData('designer-review')), NyxField('view', NyxData('designer-review'))]));
      GToken := LClaim.Field('token').AsText;
      Check(LClaim.Field('state').Field('editorBuilds').AsBoolean,
        'private editor explicitly advertises asynchronous compiler support');
      GEngine.EditorExchange(GToken, NyxObject([NyxField('op', NyxData('configure')),
        NyxField('permission', NyxData('disabled'))]));
      LBefore := GEngine.EditorExchange(GToken, NyxObject([NyxField('op', NyxData('observe'))]));
      LReply := OperatorBuild(NyxObject([NyxField('mode', NyxData('profile'))]));
      LOutputID := LReply.Field('outputID').AsText;
      Check(LReply.Field('profile').Field('fields').Field('pas2js').AsText = LProfile.Field('pas2js'),
        'only the private operator response contains machine configuration');
      GObserver.SaveError := True;
      LReply := OperatorBuild(NyxObject([NyxField('mode', NyxData('profile')),
        NyxField('expectedOutputID', NyxData(LOutputID)),
        NyxField('profile', TNyxDataValue.ParseJSON(LProfile.Encode))]));
      Check(LReply.Field('state').AsText = 'rejected', 'failed profile persistence refuses without an editor conflict');
      Check(OperatorBuild(NyxObject([NyxField('mode', NyxData('outputs'))]))
        .Field('outputID').AsText = LOutputID, 'failed persistence preserves the accepted output identity');
      GObserver.SaveError := False;
      LRefused := False;
      try
        GEngine.EditorExchange(GToken, NyxObject([NyxField('op', NyxData('build')),
          NyxField('after', NyxData('invalid')),
          NyxField('build', NyxObject([NyxField('mode', NyxData('profile')),
            NyxField('expectedOutputID', NyxData(LOutputID)),
            NyxField('profile', TNyxDataValue.ParseJSON(LProfile.Encode))]))]));
      except
        on LException: Exception do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused and (GObserver.Saves = 0),
        'malformed editor envelope refuses before any profile write');
      LReply := OperatorBuild(NyxObject([NyxField('mode', NyxData('profile')),
        NyxField('expectedOutputID', NyxData('00000000000000000000000000000000')),
        NyxField('profile', TNyxDataValue.ParseJSON(LProfile.Encode))]));
      Check((LReply.Field('state').AsText = 'rejected') and (GObserver.Saves = 0),
        'stale profile saves refuse before persistence');
      LReply := OperatorBuild(NyxObject([NyxField('mode', NyxData('profile')),
        NyxField('expectedOutputID', NyxData(LOutputID))]));
      Check(LReply.Field('state').AsText = 'rejected', 'profile read refuses unused save identity');
      LReply := OperatorBuild(NyxObject([NyxField('mode', NyxData('request')),
        NyxField('target', NyxData('browser')), NyxField('scope', NyxData('view')),
        NyxField('view', NyxData('designer-review')), NyxField('operationId', NyxData('stale-request')),
        NyxField('outputID', NyxData(LOutputID)), NyxField('expectedRevision', NyxData(0))]));
      Check(LReply.Field('state').AsText = 'rejected', 'private operator cannot bypass exact revision admission');
      Check(GEngine.EditorExchange(GToken, NyxObject([NyxField('op', NyxData('observe'))]))
        .Field('project').AsText = LBefore.Field('project').AsText,
        'compiler/profile refusals retain the complete paired project');
      LReply := OperatorBuild(NyxObject([NyxField('mode', NyxData('profile')),
        NyxField('expectedOutputID', NyxData(LOutputID)),
        NyxField('profile', TNyxDataValue.ParseJSON(LProfile.Encode))]));
      Check((LReply.Field('outputID').AsText = LOutputID) and (GObserver.Saves = 1),
        'an exact operator profile save persists before acknowledgment');

      GForm := TForm.CreateNew(nil);
      GForm.ClientWidth := 1280;
      GForm.ClientHeight := 900;
      GForm.Position := poDesigned;
      GForm.Left := -30000;
      GForm.Top := -30000;
      GStudio := TBuildStudio.Create(GForm, GDirectory + 'local-projects');
      WriteLn('Mount native editor');
      Flush(Output);
      GStudio.Run;
      GForm.Show;
      GStudio.ConnectService(ParamStr(5), NyxPrimaryWorkspace);
      WriteLn('Await native connection');
      Flush(Output);
      Ready;
      Check(GStudio.Agents.CanBuild and (GStudio.Agents.Permission = apDisabled),
        'operator builds remain available with agents disabled');
      Click('action-build-app');
      Check(Pos('Choose an output', GStudio.Status) > 0, 'missing selected output is useful and never blocks launch');

      if (ParamCount = 5) or (ParamCount = 7) then
      begin
        if ParamCount = 5 then
        begin
          Click('output-browser');
          Click('action-build-view');
          Terminal('browser', 'view');
          Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = EncodeNyxProject(GPair),
            'readiness/profile/compiler work adds no project history or changes');
          Click('action-outputs');
          Click('view-component-0');
          Ready;
          Click('action-build-view');
          Terminal('browser', 'reusable');
          Click('action-outputs');
        end;
        Click('output-lcl');
        Click('action-build-app');
        Terminal('lcl', 'application');
        Check(GObserver.Saves = 1, 'read-only profile preflight never persists a configuration');
        Click('action-agents');
        Click('action-outputs');
        PublishOwnedArtifact(GStudio.Agents.BuildReply);
        Click('action-compiled-run');
        PreviewRunning;
        LOldProcess := GStudio.CompiledPreviewProcessID;
        Capture('native-compiler-desktop');
        GForm.ClientWidth := 390;
        Pump;
        Click('action-panel-design');
        Capture('native-compiler-narrow');
        GForm.ClientWidth := 1280;
        Pump;
        Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = EncodeNyxProject(GPair),
          'compiled execution retains the authored accepted pair');
        if ParamCount = 5 then
        begin
          Click('action-compiled-stop');
          Check(GStudio.CompiledPreviewProcessID = 0, 'explicit Stop retires only the owned preview');
          Click('action-compiled-run');
          PreviewRunning;
          Check(GStudio.CompiledPreviewProcessID <> LOldProcess, 'a new explicit Run owns a fresh application process');
        end;
        ExerciseCompiledReply;

        if ParamCount = 7 then
        begin
          ExerciseBuildControls(ParamStr(7));
          Click('action-compiled-stop');
          Click('action-compiled-run');
          PreviewRunning;
          Check(GStudio.CompiledPreviewProcessID <> LOldProcess,
            'a cancelled newer job leaves the earlier accepted job available for actual re-Run');
        end;
        GPublishArtifacts := True;
        LOldProcess := GStudio.CompiledPreviewProcessID;
        LProcessHandle := OpenProcess(SYNCHRONIZE, False, LOldProcess);
        try
          Click('view-page-0');
          Ready;
          Click('action-build-view');
          Terminal('lcl', 'view');
          PreviewRunning(LOldProcess);
          Check((LProcessHandle <> 0) and (WaitForSingleObject(LProcessHandle, 0) = WAIT_OBJECT_0),
            'successful current view reload retires the previous owned process');
          ExerciseCompiledReply;
        finally

          if LProcessHandle <> 0 then
          begin
            CloseHandle(LProcessHandle);
          end;
        end;
        { Stopping an old preview during another build must retain that build's
          admission/result. This follows the actual asynchronous preflight, not
          a forged job reply or private controller-state assignment. }
        Click('action-build-view');
        Click('action-compiled-stop');
        Check(GStudio.CompiledPreviewProcessID = 0,
          'Stop during compilation retires the old preview process');
        Terminal('lcl', 'view');
        Check(GStudio.CompiledPreviewProcessID = 0,
          'the continuing build does not silently restart a stopped preview');
        Click('action-compiled-run');
        PreviewRunning;
        Click('action-compiled-stop');
      end
      else
      begin
        { Follow a failed coordinate check through the actual editor/protocol
          and one real compiler error, without repeating completed build/reload
          journeys. Native text ranges include its physical line endings. }
        Click('output-lcl');
        Click('action-outputs');
      end;

      LSource := GStudio.Session.Source;
      LPosition := Pos(#10 + 'end.' + #10, LSource);
      Check(LPosition > 0, 'retained source has an explicit unit-end helper boundary');
      LBroken := Copy(LSource, 1, LPosition) +
        TNyxText('procedure NativeCompilerCheck;' + #10 + 'begin' + #10 +
          '  { Supplementary position check 🌙 } MissingNativeCompilerHelper;' + #10 +
          'end;' + #10 + #10) + Copy(LSource, LPosition + 1, MaxInt);
      Check(LBroken <> LSource, 'deliberate compiler error changes only a retained Pascal helper');
      Click('action-code');
      TMemo(GStudio.CodeView.InputFor('studio-code')).Text := LBroken;
      Pump;
      Click('action-apply-source');
      Ready;
      Check(GStudio.Session.Source = LBroken, 'actual native source admission retains the exact helper for compilation');
      Click('action-build-app');
      Terminal('lcl', 'application', False);
      Click('action-messages-tab');
      LErrorAction := GStudio.SourceView.Root.Find('action-compiler-diagnostic-0');
      Check((LErrorAction <> nil) and (LErrorAction.Prop('enabled') <> 'false'),
        'real helper error exposes current source navigation through ordinary Nyx controls');
      Click('action-compiler-diagnostic-0');
      Check(Screen.ActiveControl = GStudio.CodeView.InputFor('studio-code'),
        'native diagnostic action focuses the source editor');
      LDiagnostics := GStudio.Agents.BuildReply.Field('diagnostics').Field('items').Item(0);
      LPresented := NyxLCLInputText(TWinControl(GStudio.CodeView.InputFor('studio-code')));
      { TextPosition returns a one-based native byte index. The editing range
        uses zero-based Unicode scalars and the actual control's line endings.
        Convert the prefix, rather than comparing different coordinate units. }
      LPosition := NyxTextScalarCount(Copy(LPresented, 1, NyxTextPosition(LPresented,
        LDiagnostics.Field('line').AsInteger, LDiagnostics.Field('column').AsInteger) - 1));
      WriteLn('Diagnostic scalar position actual=', CaptureNyxLCLSelection(
        TWinControl(GStudio.CodeView.InputFor('studio-code'))).Start, ' expected=', LPosition);
      Flush(Output);
      Check(CaptureNyxLCLSelection(TWinControl(GStudio.CodeView.InputFor('studio-code'))).Start =
        LPosition,
        'native compiler diagnostic retains its exact Unicode source line and column');
      Capture('native-compiler-diagnostics');
      TMemo(GStudio.CodeView.InputFor('studio-code')).Text := LSource;
      Pump;
      Click('action-apply-source');
      Ready;
      LReply := GStudio.Agents.BuildReply;
      LSource := GStudio.Session.Source;
      TEdit(GStudio.ShellView.InputFor('project-title')).Text := 'Changed after compilation';
      Ready;
      LReply := OperatorBuild(NyxObject([NyxField('mode', NyxData('status')), NyxField('job', LReply.Field('job'))]));
      Check(not LReply.Field('currentSource').AsBoolean, 'actual native edit makes the previous job stale on the service');
      Check(GStudio.Session.Source <> LSource, 'local source has independently changed');
      { Retire queued UI calls and the observer before the never-started engine is
        released. Worker joins must not deliver into native widgets/session owners. }
      GStudio.RequestRefresh;
      FreeAndNil(GStudio);
      Pump;
      LStarted := GetTickCount64;
      FreeAndNil(GEngine);
      Check(GetTickCount64 - LStarted < 10000, 'suspended protocol engine retires without starting its listener');
      WriteLn('PASS ', GCount, ' native compiler protocol/control checks (no HTTP listener)');
    end;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      Flush(Output);
      ExitCode := 1;
    end;
  end;
  Application.OnException := nil;
  GStudio.Free;
  GForm.Free;
  GEngine.Free;
  LDocument.Free;
  LProfile.Free;
  GObserver.Free;
end.
