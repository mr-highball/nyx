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

program nyx_file_open_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.types, nyx.model,
  nyx.codec, nyx.state, nyx.controls, nyx.behavior, nyx.scheduler,
  nyx.flow, nyx.flow.types, nyx.flow.declarations, nyx.flow.execution,
  nyx.logic, nyx.logic.http, nyx.logic.files.open, nyx.files.open,
  nyx.application.context, nyx.linq, nyx.linq.execution, nyx.test.files.openfixture
  {$ifdef NYX_FLOW_CONDITIONS}, nyx.test.flow.conditionfixture{$endif}
  {$ifdef NYX_FILE_OPEN_COMPANION}, nyx.generated.view{$endif}
  {$ifdef PAS2JS}, JS, Web, nyx.application.browser, nyx.files.open.browser
  {$else}, Interfaces, Forms, Controls, ExtCtrls, Windows,
  nyx.widgets.lcl, nyx.application.lcl, nyx.files.open.lcl{$endif};

type
  { UI result receiver owns no application. It keeps a weak context/generation,
    immutable graph and current run. Shutdown explicitly cancels before release.
    Only a complete read formats/publishes bound data; other host outcomes retain
    the preceding data and publish a separate status caption. }
  TJourney = class(TInterfacedObject, INyxQueryResultPort<INyxFlowResult>)
  public
    Context: INyxApplicationContext;
    Generation: INyxCancellationScope;
    Graph: INyxFlowGraph;
    Run: INyxQueryRun;
    Last: INyxFlowResult;
    Calls: Integer;
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    function Publish(const AValues: TNyxSequenceArray<INyxFlowResult>): Boolean;
    {$ifndef PAS2JS}procedure DialogTurn(ASender: TObject);{$endif}
  end;
  {$ifndef PAS2JS}
  { Closed physical test choices never enter document data or host options. }
  TDialogAction = (odaChoose, odaCancel, odaRetire);
  {$endif}

var
  GJourney: TJourney;
  GPort: INyxQueryResultPort<INyxFlowResult>;
  GDoc: TNyxDocument;
  GBefore: TNyxText;
  GChecks: Integer;
  GStage: Integer;
  GHandledFailure: INyxQueryRun;
  GStarted: Double;
  {$ifdef NYX_FLOW_CONDITIONS}GStateRevision: Integer;{$endif}
  {$ifdef PAS2JS}
  GApp: TNyxBrowserApplication;
  {$else}
  GApp: TNyxLCLApplication;
  GTimer: TTimer;
  GDirectory: TNyxText;
  GOpenLeaf: TNyxText;
  GDialog: HWND;
  GFilename: HWND;
  GDialogAction: TDialogAction;
  {$endif}

function OpenPort(AValue: TNyxOpenFilePort): TNyxFlowEndpoint;
begin
  Result := NyxFlowEndpoint(NyxFlowStep('choose'), NyxOpenFilePort(AValue));
end;

function Decoded: TNyxFlowEndpoint;
begin
  Result := NyxFlowEndpoint(NyxFlowStep('decode'), NyxLogicPort(nlpText));
end;

function Clock: Double;
begin
  {$ifdef PAS2JS}Result := TJSDate.now;{$else}Result := GetTickCount64;{$endif}
end;

procedure Check(AValue: Boolean; const AMessage: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(String(AMessage));
  end;
  Inc(GChecks);
end;

procedure TJourney.Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin

  if (ANode.ID <> 'open-report') or (AEvent.Trigger <> ntClick) then
  begin
    Exit;
  end;

  if Run <> nil then
  begin
    Run.Cancel;
  end;
  Generation := Context.Generation;
  Run := TNyxFlowExecution.Submit(Graph,
    [NyxFlowInput(OpenPort(nofpTrigger), NyxFlowSignal)], Context.Scheduler,
    GPort, neUIQueue, Generation);
end;

function TJourney.Publish(const AValues: TNyxSequenceArray<INyxFlowResult>): Boolean;
var
  LFrame: INyxFlowResult;
  LText: TNyxText;
  LPretty: TNyxText;
  LMetadata: TNyxDataValue;
begin
  Result := False;

  if not Context.Active or Generation.Cancelled or (Context.Generation <> Generation) then
  begin
    Exit;
  end;
  Check(Length(AValues) = 1, 'UI publication receives one complete graph frame');
  LFrame := AValues[0];

  if LFrame.Value(OpenPort(nofpRead)).AsBoolean then
  begin
    LText := LFrame.Value(Decoded).AsText;
    LPretty := '';

    if LText <> '' then
    begin
      LPretty := NewNyxJSONFormat(njlIndented, 2).Format(TNyxDataValue.ParseJSON(LText));
    end;
    LMetadata := LFrame.Value(OpenPort(nofpMetadata)).AsJSON;
    Context.State.Apply([
      NyxStateValue(NyxTextState('pretty'), LPretty),
      NyxStateValue(NyxTextState('read-name'), LFrame.Value(OpenPort(nofpName)).AsText),
      NyxStateValue(NyxTextState('read-status'), 'Read complete'),
      NyxStateValue(NyxTextState('metadata'), NewNyxJSONFormat(njlIndented, 2).Format(LMetadata))]);
  end
  else
  begin
    Context.State.SetValue(NyxTextState('read-status'),
      NyxFileOpenOutcomeName(TNyxFileOpenOutcome(LFrame.Value(OpenPort(nofpOutcome)).AsInteger)));
  end;
  Last := LFrame;
  Inc(Calls);
  Result := True;
end;

procedure CheckRead(const AName: TNyxText; AByteCount: Integer);
var
  LMetadata: TNyxDataValue;
begin
  Check(GJourney.Last.Value(OpenPort(nofpRead)).AsBoolean, 'actual host reports a completed file snapshot');
  Check(GJourney.Last.Value(OpenPort(nofpName)).AsText = AName,
    'selected portable leaf retains exact Unicode');
  Check(GJourney.Last.Value(OpenPort(nofpByteCount)).AsInteger = AByteCount,
    'actual read accounts for the complete byte payload');
  LMetadata := GJourney.Last.Value(OpenPort(nofpMetadata)).AsJSON;
  Check(LMetadata.Field('lastModifiedMilliseconds').AsNumber = OpenFixtureModified,
    'actual host retains the independently fixed modification time');
  Check(LMetadata.Field('byteCount').AsInteger = AByteCount,
    'metadata and byte outputs refer to the same completed snapshot');
end;

procedure CheckStage;
var
  LExpected: TNyxText;
  {$ifdef NYX_FLOW_CONDITIONS}LReport: TNyxDataValue;{$endif}
begin
  LExpected := NewNyxJSONFormat(njlIndented, 2).Format(TNyxDataValue.ParseJSON(OpenFixtureJSON));
  {$ifdef NYX_FLOW_CONDITIONS}
  LReport := GApp.FlowRoutes.Report(0);

  if (LReport.Field('started').AsInteger < GStage + 1) or
    (LReport.Field('active').AsInteger > 0) then
  begin
    Exit;
  end;
  case GStage of
    0:
      begin
        Check(LReport.Field('succeeded').AsInteger = 1,
          'ordinary saved route completes the full file/decode/parse/format chain');
        Check(GApp.State.Revision = GStateRevision + 1,
          'ordinary route publishes all guarded outputs in one state revision');
        Check(GApp.State.GetValue(NyxTextState('pretty')) = LExpected,
          'saved conditional route preserves exact JSON numbers and supplementary text');
        Check(GApp.State.GetValue(NyxTextState('read-name')) = OpenFixtureLeaf,
          'conditional name branch retains the exact selected Unicode leaf');
        Check(TNyxDataValue.ParseJSON(GApp.State.GetValue(NyxTextState('metadata')))
          .Field('lastModifiedMilliseconds').AsNumber = OpenFixtureModified,
          'saved metadata formatting retains the independently fixed modification time');
        {$ifdef PAS2JS}
        Check(Pos('9007199254740993', GApp.View.ElementFor('read-output').textContent) > 0,
          'ordinary browser bindings display the actual saved-route result');
        {$endif}
      end;
    1:
      begin
        Check((LReport.Field('succeeded').AsInteger = 2) and
          (LReport.Field('failed').AsInteger = 0),
          'a failed selection completes its usable non-read branch without invoking a parser');
        Check(GApp.State.Revision = GStateRevision + 1,
          'non-read flags and diagnostic publish once without fabricated report values');
        Check(not GApp.State.GetValue(NyxBooleanState('read-complete')),
          'host failure does not pretend an empty file was read');
        Check(GApp.State.GetValue(NyxTextState('read-status')) <> '',
          'usable file failure retains its host diagnostic');
      end;
    2, 3:
      begin
        Check(LReport.Field('failed').AsInteger = GStage - 1,
          'selected invalid UTF-8 or empty JSON fails the selected branch');
        Check(GApp.State.Revision = GStateRevision,
          'a selected parser/decode failure publishes no successful prefix');
        Check(LReport.Field('failure').AsText <> '',
          'selected-branch failure remains inspectable through the ordinary route report');
      end;
    {$ifndef PAS2JS}
    4:
      begin
        Check(LReport.Field('succeeded').AsInteger = 3,
          'real native picker cancellation completes a usable conditional route');
        Check(GApp.State.GetValue(NyxBooleanState('selection-cancelled')) and
          not GApp.State.GetValue(NyxBooleanState('read-complete')),
          'native selection cancellation has explicit distinct routed flags');
        Check(GApp.State.Revision = GStateRevision + 1,
          'user cancellation publishes only its actual outcome once');
      end;
    5:
      begin
        Check(LReport.Field('cancelled').AsInteger = 1,
          'page retirement cancels the actual pending saved route');
        Check(GApp.State.Revision = GStateRevision,
          'retired modal reads cannot publish into the next view');
      end;
    {$endif}
  end;
  Check(GApp.State.GetValue(NyxTextState('pretty')) = LExpected,
    'a skipped or refused report branch preserves the previous displayed report');
  Check(GApp.State.GetValue(NyxTextState('read-name')) = OpenFixtureLeaf,
    'skipped name output preserves the previous successful name');
  GStateRevision := GApp.State.Revision;
  {$else}
  case GStage of
    0:
      begin

        if GJourney.Calls < 1 then
        begin
          Exit;
        end;
        CheckRead(OpenFixtureLeaf, Length(NyxEncodeUTF8(OpenFixtureJSON)));
        Check(GApp.State.GetValue(NyxTextState('pretty')) = LExpected,
          'file/read/decode produces exact JSON numbers, supplementary text and NUL escapes');
      end;
    1:
      begin

        if GJourney.Calls < 2 then
        begin
          Exit;
        end;
        Check(not GJourney.Last.Value(OpenPort(nofpRead)).AsBoolean and
          (GJourney.Last.Value(OpenPort(nofpOutcome)).AsInteger = Ord(nfooFailed)),
          'over-budget selection exposes usable failure without any partial bytes');
        Check(GJourney.Last.Value(OpenPort(nofpMetadata)).AsJSON.Kind = ndNull,
          'unread metadata remains explicitly unavailable');
        Check(GApp.State.GetValue(NyxTextState('pretty')) = LExpected,
          'failed selection preserves the previous displayed report');
      end;
    2:
      begin

        if (GJourney.Run = nil) or (GJourney.Run.Phase <> nqpFailed) then
        begin
          Exit;
        end;
        Check(GJourney.Calls = 2, 'invalid UTF-8 prevents the entire graph publication');
        Check(GApp.State.GetValue(NyxTextState('pretty')) = LExpected,
          'strict decoding failure preserves previous control values');
        GHandledFailure := GJourney.Run;
      end;
    3:
      begin

        if GJourney.Calls < 3 then
        begin
          Exit;
        end;
        CheckRead('empty.txt', 0);
        Check(GApp.State.GetValue(NyxTextState('pretty')) = '',
          'an intentionally selected empty file is a valid empty read');
      end;
    {$ifndef PAS2JS}
    4:
      begin

        if GJourney.Calls < 4 then
        begin
          Exit;
        end;
        Check(GJourney.Last.Value(OpenPort(nofpCancelled)).AsBoolean,
          'actual native dialog cancellation is distinct usable data');
      end;
    5:
      begin

        if (GJourney.Run = nil) or (GJourney.Run.Phase <> nqpCancelled) then
        begin
          Exit;
        end;
        Check(GJourney.Calls = 4, 'actual page retirement silently cancels the open dialog');
        Check(GApp.State.GetValue(NyxTextState('pretty')) = '',
          'retired native reads cannot publish into the next view');
      end;
    {$endif}
  end;
  {$endif}
  Inc(GStage);
end;

{$ifdef PAS2JS}
procedure Drive;
const
  CStages: array[0..4] of String = ('ready', 'read', 'limited', 'invalid', 'empty');
begin
  try
    CheckStage;
    document.body.setAttribute('data-file-open-stage', CStages[GStage]);

    if GStage = 4 then
    begin
      Check(TNyxCodec.Encode(GDoc) = GBefore, 'real file interactions never mutate the design');
      document.body.setAttribute('data-file-open-controls', 'passed');
      document.body.setAttribute('data-file-open-checks', IntToStr(GChecks));
      Exit;
    end;

    if (GJourney.Run <> nil) and (GJourney.Run.Phase = nqpFailed) and
      (GStage <> 2) and (GJourney.Run <> GHandledFailure) then
    begin
      raise Exception.Create(String(GJourney.Run.Failure));
    end;

    if Clock - GStarted > 45000 then
    begin
      raise Exception.Create('Actual browser file-read journey deadline');
    end;
    window.setTimeout(@Drive, 10);
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-file-open-controls', 'failed');
      document.body.setAttribute('data-file-open-error', E.Message);
    end;
  end;
end;
{$else}
function FindDialog(AWindow: HWND; AData: LPARAM): BOOL; stdcall;
var
  LProcess: DWORD;
  LTitle: array[0..255] of WideChar;
begin
  Result := True;
  GetWindowThreadProcessId(AWindow, @LProcess);

  if (LProcess <> GetCurrentProcessId) or not IsWindowVisible(AWindow) then
  begin
    Exit;
  end;
  GetWindowTextW(AWindow, LTitle, Length(LTitle));

  if UnicodeString(LTitle) = 'Choose a report' then
  begin
    GDialog := AWindow;
    Result := False;
  end;
end;

function FindFilename(AWindow: HWND; AData: LPARAM): BOOL; stdcall;
var
  LClass: array[0..63] of WideChar;
begin
  Result := True;
  GetClassNameW(AWindow, LClass, Length(LClass));

  if (UnicodeString(LClass) = 'Edit') and
    ((GetDlgCtrlID(AWindow) = 1001) or (GetDlgCtrlID(AWindow) = 1148)) then
  begin
    GFilename := AWindow;
    Result := False;
  end;
end;

procedure TJourney.DialogTurn(ASender: TObject);
var
  LPath: UnicodeString;
  LReply: DWORD_PTR;
begin
  GDialog := 0;
  EnumWindows(@FindDialog, 0);

  if GDialog = 0 then
  begin
    Exit;
  end;

  if GDialogAction = odaRetire then
  begin
    GApp.ShowPage('other');
    Exit;
  end;

  if GDialogAction = odaCancel then
  begin
    PostMessageW(GDialog, WM_COMMAND, IDCANCEL, 0);
    Exit;
  end;
  GFilename := 0;
  EnumChildWindows(GDialog, @FindFilename, 0);

  if (GFilename = 0) or not IsWindowEnabled(GFilename) then
  begin
    Exit;
  end;
  LPath := UTF8Decode(GDirectory + TNyxText(PathDelim) + GOpenLeaf);

  if SendMessageTimeoutW(GFilename, WM_SETTEXT, 0, LPARAM(PWideChar(LPath)),
    SMTO_ABORTIFHUNG, 1000, @LReply) <> 0 then
  begin
    PostMessageW(GDialog, WM_COMMAND, IDOK, 0);
  end;
end;

procedure WriteFixture(const AName: TNyxText; const ABytes: TNyxBytes);
var
  LHandle: THandle;
  LStream: THandleStream;
  LPath: UnicodeString;
  LRaw: QWord;
  LTime: TFileTime;
begin
  LPath := UTF8Decode(GDirectory + TNyxText(PathDelim) + AName);
  LHandle := CreateFileW(PWideChar(LPath), GENERIC_WRITE, 0, nil, CREATE_NEW,
    FILE_ATTRIBUTE_NORMAL, 0);

  if LHandle = INVALID_HANDLE_VALUE then
  begin
    RaiseLastOSError;
  end;
  LStream := THandleStream.Create(LHandle);
  try

    if Length(ABytes) > 0 then
    begin
      LStream.WriteBuffer(ABytes[0], Length(ABytes));
    end;
    LRaw := 116444736000000000 + QWord(Trunc(OpenFixtureModified)) * 10000;
    LTime.dwLowDateTime := DWORD(LRaw and $FFFFFFFF);
    LTime.dwHighDateTime := DWORD(LRaw shr 32);
    Check(SetFileTime(LHandle, nil, nil, @LTime), 'owned source file receives an independent exact timestamp');
  finally
    LStream.Free;
    CloseHandle(LHandle);
  end;
end;
{$endif}

var
  LHost: INyxFileOpenHost;
  {$ifndef PAS2JS}
  LBytes: TNyxBytes;
  LIndex: Integer;
  LBeforeStage: Integer;
  LStageStarted: Double;
  LReadScheduler: INyxScheduler;
  LMonitor: INyxSchedulerMonitor;
  LDrainUntil: Double;
  {$endif}
begin
  GJourney := TJourney.Create;
  GPort := GJourney;
  try
    {$ifdef NYX_FILE_OPEN_COMPANION}
    GDoc := BuildNyxDocument;
    {$else}
    {$ifdef NYX_FLOW_CONDITIONS}
    GDoc := ConditionalFileDocument;
    {$else}
    GDoc := FileOpenDocument;
    {$endif}
    {$endif}
    GBefore := TNyxCodec.Encode(GDoc);
    {$ifdef PAS2JS}
    GApp := TNyxBrowserApplication.Create;
    LHost := NewNyxBrowserFileOpenHost;
    {$ifdef NYX_FLOW_CONDITIONS}
    GApp.ConfigureFileOpening(LHost);
    {$endif}
    GApp.Run(GDoc, TJSHTMLElement(document.body));
    {$else}

    if (ParamCount <> 1) or DirectoryExists(ParamStr(1)) then
    begin
      raise Exception.Create('Supply a fresh owned file-read fixture directory');
    end;
    GDirectory := TNyxText(ExpandFileName(ParamStr(1)));
    ForceDirectories(String(GDirectory));
    WriteFixture(OpenFixtureLeaf, NyxEncodeUTF8(OpenFixtureJSON));
    LBytes := nil;
    SetLength(LBytes, 4097);
    for LIndex := 0 to High(LBytes) do
    begin
      LBytes[LIndex] := 32;
    end;
    WriteFixture('large.bin', LBytes);
    LBytes := nil;
    SetLength(LBytes, 1);
    LBytes[0] := 255;
    WriteFixture('invalid.bin', LBytes);
    WriteFixture('empty.txt', nil);
    Application.Initialize;
    GApp := TNyxLCLApplication.Create;
    LHost := NewNyxLCLFileOpenHost(GDirectory);
    {$ifdef NYX_FLOW_CONDITIONS}
    GApp.ConfigureFileOpening(LHost);
    {$endif}
    GApp.Mount(GDoc);
    GApp.Window.Show;
    GTimer := TTimer.Create(nil);
    GTimer.Interval := 50;
    GTimer.OnTimer := GJourney.DialogTurn;
    {$endif}
    GJourney.Context := GApp.Context;
    {$ifndef PAS2JS}
    LReadScheduler := GApp.Context.Scheduler;
    LMonitor := LReadScheduler as INyxSchedulerMonitor;
    {$endif}
    {$ifdef NYX_FLOW_CONDITIONS}
    GStateRevision := GApp.State.Revision;
    Check(GApp.FlowRoutes.Count = 1, 'ordinary application attaches the saved conditional route');
    {$else}
    GJourney.Graph := GDoc.Flows.Definition(NyxFlowRef('Read report')).Build(
      NewNyxOpenFileLogicResolver(LHost));
    GApp.View.OnEvent := GJourney.Event;
    {$endif}
    GStarted := Clock;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-file-open-controls', 'ready');
    Drive;
    {$else}
    while GStage < 6 do
    begin
      GDialogAction := odaChoose;
      case GStage of
        0: GOpenLeaf := OpenFixtureLeaf;
        1: GOpenLeaf := 'large.bin';
        2: GOpenLeaf := 'invalid.bin';
        3: GOpenLeaf := 'empty.txt';
        4: GDialogAction := odaCancel;
        5: GDialogAction := odaRetire;
      end;
      LBeforeStage := GStage;
      LStageStarted := Clock;
      TNyxLCLButton(GApp.View.ControlFor('open-report')).Click;
      repeat
        CheckSynchronize;
        Application.ProcessMessages;
        CheckStage;

        if Clock - LStageStarted > 15000 then
        begin
          raise Exception.Create('Actual native file-read stage deadline ' + IntToStr(GStage));
        end;
        Sleep(1);
      until GStage <> LBeforeStage;
    end;
    Check(TNyxCodec.Encode(GDoc) = GBefore, 'native file interactions preserve the exact portable design');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-file-open-controls', 'failed');
      document.body.setAttribute('data-file-open-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
  {$ifndef PAS2JS}
  GTimer.Free;

  if GJourney.Run <> nil then
  begin
    GJourney.Run.Cancel;
  end;

  if GApp <> nil then
  begin
    GApp.View.OnEvent := nil;
  end;
  GApp.Free;
  GPort := nil;
  GDoc.Free;
  LHost := nil;

  if LReadScheduler <> nil then
  begin
    { The inspection lease owns this scheduler beyond application teardown.
      Product shutdown is nonblocking. The checked fixture must explicitly
      close and drain its own workers before console/heap finalization. }
    LReadScheduler.Shutdown;
    LDrainUntil := Clock + 100;
    while (Clock < LDrainUntil) or (LMonitor.WorkerLoad.ActiveWorkers > 0) do
    begin
      Application.ProcessMessages;
      CheckSynchronize;
      Sleep(1);

      if Clock > LDrainUntil + 5000 then
      begin
        raise Exception.Create('Owned native file-read workers did not retire');
      end;
    end;
    Check(LMonitor.WorkerLoad.ActiveWorkers = 0,
      'owned file-read workers retire before console heap finalization');
    LMonitor := nil;
    LReadScheduler := nil;
  end;

  if ExitCode = 0 then
  begin
    WriteLn('PASS ', GChecks, ' actual native file/read/metadata/graph checks');
  end;
  {$endif}
end.
