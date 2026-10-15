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

program nyx_file_save_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, nyx.text, nyx.bytes, nyx.data, nyx.types, nyx.model,
  nyx.controls, nyx.behavior, nyx.logic, nyx.logic.files, nyx.files.save,
  nyx.files.save.editor,
  nyx.files.save.activation, nyx.confirmation,
  nyx.flow, nyx.flow.types, nyx.flow.execution, nyx.linq, nyx.linq.execution,
  nyx.scheduler
  {$ifdef PAS2JS}, JS, Web, nyx.render.browser, nyx.files.save.browser,
  nyx.confirmation.browser, nyx.files.save.activation.browser
  {$else}, Interfaces, Forms, Controls, StdCtrls, ExtCtrls, Windows,
  nyx.widgets.lcl, nyx.render.lcl, nyx.files.save.lcl, nyx.confirmation.lcl,
  nyx.files.save.activation.lcl{$endif};

type
  {$ifdef NYX_SAVE_ACTIVATION}
  { Observes actual public target confirmations without replacing their UI or
    input APIs. The managed observation outlives request cleanup; no request or
    payload is retained back from the presenter. }
  TPrompts = class(TInterfacedObject, INyxSaveActivationPrompts)
  public
    Count: Integer;
    Current: {$ifdef PAS2JS}INyxBrowserConfirmation{$else}INyxLCLConfirmation{$endif};
    function CreatePrompt(const AOptions: TNyxFileSaveOptions;
      AByteCount: Integer): INyxConfirmation;
    function PromptOptions(const AOptions: TNyxFileSaveOptions): TNyxConfirmationOptions;
  end;
  {$endif}
  { One complete graph publication, owned independently of rendered controls.
    The fixture reads it on later UI turns and never publishes a partial frame. }
  TPort = class(TInterfacedObject, INyxQueryResultPort<INyxFlowResult>)
  public
    Calls: Integer;
    Last: INyxFlowResult;
    function Publish(const AValues: TNyxSequenceArray<INyxFlowResult>): Boolean;
  end;

  { Fixture-owned event receiver, revoked before renderer disposal. It submits
    the public JSON/UTF-8/save graph and retains its run until cancellation or
    complete output. Native dialog automation addresses only this process. }
  TJourney = class
  public
    Scheduler: INyxScheduler;
    Host: INyxFileSaveHost;
    Run: INyxQueryRun;
    Scope: INyxCancellationScope;
    Port: TPort;
    PortLease: INyxQueryResultPort<INyxFlowResult>;
    Graph: INyxFlowGraph;
    Clicks: Integer;
    ActivationReplies: Integer;
    {$ifdef NYX_SAVE_ACTIVATION}
    PreparedText: TNyxText;
    PreparedOptions: TNyxFileSaveOptions;
    procedure StartPrepared;
    {$endif}
  public
    {$ifndef PAS2JS}
    DialogMode: Integer;
    DialogCount: Integer;
    DialogStarted: QWord;
    DialogError: TNyxText;
    procedure DialogTurn(ASender: TObject);
    {$ifdef NYX_SAVE_ACTIVATION}
    procedure PromptTurn(ASender: TObject);
    {$endif}
    {$endif}
    procedure ActivationReply(const AResult: TNyxFileSaveResult);
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
  end;

  {$ifdef NYX_SAVE_ACTIVATION}
  { Test-only asynchronous preparation delay. It captures no platform gesture;
    its borrowed owner remains alive until the fixture shuts down its scheduler. }
  TPreparedTurn = class(TInterfacedObject, INyxWork)
  public
    Owner: TJourney;
    procedure Execute(const AExecution: INyxExecution);
  end;
  {$endif}

const
  FileSaveJSON: TNyxText =
    '{"title":"Evening notes","count":9007199254740993,"detail":"Hello 🌙\u0000world"}';
  FileSaveLeaf: TNyxText = 'evening-notes.json';

var
  GOwner: TJourney;
  GDocument: TNyxDocument;
  GChecks: Integer;
  {$ifdef NYX_SAVE_ACTIVATION}
  GPrompts: TPrompts;
  GPromptsLease: INyxSaveActivationPrompts;
  {$ifdef PAS2JS}
  GActivationStage: Integer;
  GPromptFitChecked: Integer;
  {$endif}
  {$endif}
  {$ifdef PAS2JS}
  GRenderer: TNyxBrowserRenderer;
  GStarted: Double;
  {$else}
  GRenderer: TNyxLCLRenderer;
  GForm: TForm;
  GTimer: TTimer;
  GDirectory: String;
  GDialog: HWND;
  GFilenameEdit: HWND;
  {$ifdef NYX_SAVE_ACTIVATION}
  GPromptTimer: TTimer;
  {$endif}
  {$endif}

procedure Check(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

{$ifdef NYX_SAVE_ACTIVATION}
function TPrompts.PromptOptions(const AOptions: TNyxFileSaveOptions): TNyxConfirmationOptions;
begin
  Result := NyxConfirmation('Ready to save').Sizing(ncsContent);
end;

function TPrompts.CreatePrompt(const AOptions: TNyxFileSaveOptions;
  AByteCount: Integer): INyxConfirmation;
begin
  Inc(Count);
  Current := {$ifdef PAS2JS}NewNyxBrowserConfirmation({$else}NewNyxLCLConfirmation(GForm, {$endif}
    NewNyxSaveActivationContent(
      'pending-save-' + TNyxText(IntToStr(Count)), AOptions, AByteCount));
  Result := Current;
  {$ifdef PAS2JS}

  if Count = 1 then
  begin
    Check(not Boolean(TJSObject(TJSObject(window.navigator)['userActivation'])['isActive']),
      'actual browser activation expired before recovery presentation');
    document.body.setAttribute('data-save-activation-expired', 'true');
  end;
  document.body.setAttribute('data-save-confirm-selector',
    '[data-node="' + Current.Content.ActionsConfirmButton.ID + '"]');
  {$endif}
end;

procedure TPreparedTurn.Execute(const AExecution: INyxExecution);
begin

  if not AExecution.Cancelled then
  begin
    Owner.StartPrepared;
  end;
end;
{$endif}

function TPort.Publish(const AValues: TNyxSequenceArray<INyxFlowResult>): Boolean;
begin
  Check(Length(AValues) = 1, 'graph publishes one complete result');
  Inc(Calls);
  Last := AValues[0];
  Result := True;
end;

function Endpoint(const AStep: TNyxText; APort: TNyxLogicPort): TNyxFlowEndpoint;
begin
  Result := NyxFlowEndpoint(NyxFlowStep(AStep), NyxLogicPort(APort));
end;

function FileEndpoint(APort: TNyxFilePort): TNyxFlowEndpoint;
begin
  Result := NyxFlowEndpoint(NyxFlowStep('save'), NyxFilePort(APort));
end;

procedure TJourney.ActivationReply(const AResult: TNyxFileSaveResult);
begin
  Inc(ActivationReplies);
  Check((AResult.Outcome = nfsoActivationRequired) and (AResult.ByteCount = 0),
    'a browser download without user activation has no external effect');
end;

procedure TJourney.Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
var
  LOptions: TNyxFileSaveOptions;
  {$ifdef NYX_SAVE_ACTIVATION}
  LTurn: TPreparedTurn;
  LWork: INyxWork;
  {$endif}
begin

  if (ANode.ID <> 'save-report') or (AEvent.Trigger <> ntClick) then
  begin
    Exit;
  end;
  Inc(Clicks);
  {$ifdef NYX_SAVE_ACTIVATION}
  PreparedText := GRenderer.Root.Find('report-source').Prop('value');
  PreparedOptions := CaptureNyxFileSaveEditor('save-options', GRenderer.Root);
  {$ifdef PAS2JS}

  if GActivationStage = 0 then
  begin
    LTurn := TPreparedTurn.Create;
    LWork := LTurn;
    LTurn.Owner := Self;
    NyxSchedulerDelays(Scheduler).PostAfter(LWork, 6500);
  end
  else
  begin
    StartPrepared;
  end;
  {$else}
  StartPrepared;
  {$endif}
  {$else}
  LOptions := CaptureNyxFileSaveEditor('save-options', GRenderer.Root);
  Graph := NewNyxFlowGraph
    .WithStep(NyxFlowStep('parse'), NewNyxJSONParse)
    .WithStep(NyxFlowStep('pretty'), NewNyxJSONFormat(njlIndented, 2))
    .WithStep(NyxFlowStep('encode'), NewNyxUTF8Encode)
    .WithStep(NyxFlowStep('save'), NewNyxSaveFile(LOptions, Host))
    .Connect(Endpoint('parse', nlpJSON), Endpoint('pretty', nlpJSON))
    .Connect(Endpoint('pretty', nlpText), Endpoint('encode', nlpText))
    .Connect(NyxFlowEndpoint(NyxFlowStep('encode'), NyxFilePort(nfpBytes)), FileEndpoint(nfpBytes))
    .Build;
  Scope := NewNyxCancellationScope;
  Run := TNyxFlowExecution.Submit(Graph, [
    NyxFlowInput(Endpoint('parse', nlpText), NyxFlowText(
      GRenderer.Root.Find('report-source').Prop('value'))),
    NyxFlowInput(FileEndpoint(nfpTrigger), NyxFlowSignal)],
    Scheduler, PortLease, neUIQueue, Scope);
  {$endif}
end;

{$ifdef NYX_SAVE_ACTIVATION}
procedure TJourney.StartPrepared;
begin
  Graph := NewNyxFlowGraph
    .WithStep(NyxFlowStep('parse'), NewNyxJSONParse)
    .WithStep(NyxFlowStep('pretty'), NewNyxJSONFormat(njlIndented, 2))
    .WithStep(NyxFlowStep('encode'), NewNyxUTF8Encode)
    .WithStep(NyxFlowStep('save'), NewNyxSaveFile(PreparedOptions, Host))
    .Connect(Endpoint('parse', nlpJSON), Endpoint('pretty', nlpJSON))
    .Connect(Endpoint('pretty', nlpText), Endpoint('encode', nlpText))
    .Connect(NyxFlowEndpoint(NyxFlowStep('encode'), NyxFilePort(nfpBytes)), FileEndpoint(nfpBytes))
    .Build;
  Scope := NewNyxCancellationScope;
  Run := TNyxFlowExecution.Submit(Graph, [
    NyxFlowInput(Endpoint('parse', nlpText), NyxFlowText(PreparedText)),
    NyxFlowInput(FileEndpoint(nfpTrigger), NyxFlowSignal)],
    Scheduler, PortLease, neUIQueue, Scope);
end;
{$endif}

{$ifndef PAS2JS}
function FindOwnedDialog(AWindow: HWND; AData: LPARAM): BOOL; stdcall;
var
  LProcess: DWORD;
  LTitle: array[0..255] of WideChar;
begin
  Result := True;
  GetWindowThreadProcessId(AWindow, @LProcess);

  if LProcess <> GetCurrentProcessId then
  begin
    Exit;
  end;
  GetWindowTextW(AWindow, LTitle, Length(LTitle));

  if UnicodeString(LTitle) = 'Save a Nyx report' then
  begin
    GDialog := AWindow;
    Result := False;
  end;
end;

function FindFilenameEdit(AWindow: HWND; AData: LPARAM): BOOL; stdcall;
var
  LClass: array[0..127] of WideChar;
  LID: Integer;
begin
  Result := True;
  LID := GetDlgCtrlID(AWindow);
  GetClassNameW(AWindow, LClass, Length(LClass));

  if (UnicodeString(LClass) = 'Edit') and ((LID = 1001) or (LID = 1148)) then
  begin
    GFilenameEdit := AWindow;
    Result := False;
  end;
end;

procedure TJourney.DialogTurn(ASender: TObject);
var
  LEdit: HWND;
begin
  GDialog := 0;
  EnumWindows(@FindOwnedDialog, 0);

  if GDialog = 0 then
  begin
    Exit;
  end;

  if GetTickCount64 - DialogStarted > 10000 then
  begin
    DialogError := 'Owned save dialog automation exceeded its deadline';
    PostMessage(GDialog, WM_CLOSE, 0, 0);
    Exit;
  end;

  if DialogMode = 2 then
  begin
    Scope.Cancel;
    Exit;
  end;
  Inc(DialogCount);

  if DialogMode = 1 then
  begin
    PostMessage(GDialog, WM_COMMAND, IDCANCEL, 0);
    Exit;
  end;
  GFilenameEdit := 0;
  EnumChildWindows(GDialog, @FindFilenameEdit, 0);
  LEdit := GFilenameEdit;

  if LEdit = 0 then
  begin
    LEdit := GetDlgItem(GDialog, 1001);
  end;

  if LEdit = 0 then
  begin
    { Modern dialogs create their nested filename edit on a later modal turn. }
    Exit;
  end;
  SetWindowTextW(LEdit, PWideChar(UnicodeString(GDirectory + PathDelim + String(FileSaveLeaf))));
  PostMessage(GDialog, WM_COMMAND, IDOK, 0);
end;
{$ifdef NYX_SAVE_ACTIVATION}
procedure TJourney.PromptTurn(ASender: TObject);
begin

  if (GPrompts.Current = nil) or not GPrompts.Current.IsOpen then
  begin
    Exit;
  end;

  if DialogMode = 2 then
  begin
    Scope.Cancel;
  end
  else if DialogMode = 1 then
  begin
    TNyxLCLButton(GPrompts.Current.Renderer.ControlFor(
      GPrompts.Current.Content.ActionsCancelButton.ID)).Click;
  end
  else
  begin
    TNyxLCLButton(GPrompts.Current.Renderer.ControlFor(
      GPrompts.Current.Content.ActionsConfirmButton.ID)).Click;
  end;
end;
{$endif}
{$endif}

{$ifndef PAS2JS}
procedure Retire;
var
  LStarted: QWord;
begin
  FreeAndNil(GTimer);
  {$ifdef NYX_SAVE_ACTIVATION}
  FreeAndNil(GPromptTimer);
  {$endif}

  if GOwner <> nil then
  begin

    if GOwner.Run <> nil then
    begin
      GOwner.Run.Cancel;
    end;
    GOwner.Run := nil;
    GOwner.Graph := nil;
    GOwner.PortLease := nil;
    GOwner.Host := nil;

    if GOwner.Scheduler <> nil then
    begin
      GOwner.Scheduler.Shutdown;
    end;
  end;

  if GRenderer <> nil then
  begin
    GRenderer.OnEvent := nil;
  end;
  FreeAndNil(GRenderer);
  FreeAndNil(GForm);
  LStarted := GetTickCount64;
  repeat
    CheckSynchronize;
    Application.ProcessMessages;
    Sleep(1);
  until GetTickCount64 - LStarted > 100;
  FreeAndNil(GOwner);
  {$ifdef NYX_SAVE_ACTIVATION}
  GPromptsLease := nil;
  {$endif}
  FreeAndNil(GDocument);
end;
{$endif}

procedure CheckGraph;
var
  LExpected: TNyxFileSaveOutcome;
begin
  LExpected := {$ifdef PAS2JS}nfsoDownloadRequested{$else}nfsoWritten{$endif};
  Check(GOwner.Clicks = 1, 'ordinary Nyx button callback starts exactly one graph');
  Check(GOwner.Port.Calls = 1, 'graph continuation publishes once');
  Check(GOwner.Port.Last.Value(FileEndpoint(nfpOutcome)).AsInteger = Ord(LExpected),
    'actual adapter distinguishes write completion from download dispatch');
  Check(GOwner.Port.Last.Value(FileEndpoint(nfpDelivered)).AsBoolean,
    'complete graph exposes delivered data');
  Check(GOwner.Port.Last.Value(FileEndpoint(nfpWritten)).AsBoolean =
    (LExpected = nfsoWritten), 'download cannot masquerade as a written file');
  Check(GOwner.Port.Last.Value(Endpoint('pretty', nlpText)).AsText =
    NewNyxJSONFormat(njlIndented, 2).Format(TNyxDataValue.ParseJSON(FileSaveJSON)),
    'exact formatted text survives the host continuation');
end;

{$ifdef PAS2JS}
procedure Drive;
begin
  try

    if GOwner.Run <> nil then
    begin

      if GOwner.Run.Phase = nqpFailed then
      begin
        raise Exception.Create(GOwner.Run.Failure);
      end;

      {$ifdef NYX_SAVE_ACTIVATION}

      if (GActivationStage = 0) and (GOwner.Port.Calls = 1) then
      begin
        CheckGraph;
        Check((GPrompts.Count = 1) and not GPrompts.Current.IsOpen,
          'fresh confirmation resumes the same pending graph and then closes');
        GOwner.Host := NewNyxActivatedFileSaveHost(NewNyxBrowserFileSaveHost,
          GPromptsLease, nsapAlwaysPrompt);
        GActivationStage := 1;
        document.body.setAttribute('data-save-activation-stage', 'saved');
      end
      else if (GActivationStage = 1) and (GOwner.Port.Calls = 2) then
      begin
        Check(GOwner.Port.Last.Value(FileEndpoint(nfpCancelled)).AsBoolean and
          not GOwner.Port.Last.Value(FileEndpoint(nfpDelivered)).AsBoolean,
          'actual Escape cancels the pending save without file dispatch');
        Check(not GPrompts.Current.IsOpen, 'Escape closes the actual Nyx modal');
        GActivationStage := 2;
        document.body.setAttribute('data-save-activation-stage', 'cancelled');
      end
      else if (GActivationStage = 2) and (GPrompts.Count = 3) and
        (GOwner.Run.Phase = nqpCancelled) then
      begin
        Check(GOwner.Port.Calls = 2, 'retired browser scope publishes no extra graph frame');
        Check(not GPrompts.Current.IsOpen, 'retired browser scope closes the actual Nyx modal');
        document.body.setAttribute('data-file-save-controls', 'passed');
        document.body.setAttribute('data-file-save-checks', IntToStr(GChecks));
        Exit;
      end;
      {$else}

      if GOwner.Port.Calls > 0 then
      begin
        CheckGraph;
        document.body.setAttribute('data-file-save-controls', 'passed');
        document.body.setAttribute('data-file-save-checks', IntToStr(GChecks));
        Exit;
      end;
      {$endif}
    end;
    {$ifdef NYX_SAVE_ACTIVATION}

    if (GPrompts.Current <> nil) and GPrompts.Current.IsOpen then
    begin

      if GPromptFitChecked <> GPrompts.Count then
      begin
        Check(GPrompts.Current.Host.Element.scrollHeight <= GPrompts.Current.Host.Element.clientHeight,
          'short prepared-save content fits without an artificial vertical scrollbar');
        GPromptFitChecked := GPrompts.Count;
      end;
      document.body.setAttribute('data-save-activation-prompt', 'open');
      document.body.setAttribute('data-save-activation-prompt-count', IntToStr(GPrompts.Count));

      if GPrompts.Count = 3 then
      begin
        document.body.setAttribute('data-save-activation-stage', 'retiring');

        if document.body.getAttribute('data-capture-observed') = 'retiring-save-prompt' then
        begin
          GOwner.Scope.Cancel;
        end;
      end;
    end
    else
    begin
      document.body.setAttribute('data-save-activation-prompt', 'closed');
    end;
    {$endif}

    if TJSDate.now - GStarted > 30000 then
    begin
      raise Exception.Create('File-save input deadline');
    end;
    window.setTimeout(@Drive, 10);
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-file-save-controls', 'failed');
      document.body.setAttribute('data-file-save-error', E.Message);
    end;
  end;
end;
{$endif}

var
  LPage: INyxPage;
  LMemo: INyxMemo;
  LOptions: TNyxFileSaveOptions;
  {$ifndef PAS2JS}
  LStarted: QWord;
  LStream: TFileStream;
  LActual: RawByteString;
  LExpected: TNyxBytes;
  LIndex: Integer;
  LExact: Boolean;
  {$endif}
  LRequest: INyxFileSaveRequest;
begin
  GOwner := TJourney.Create;
  GDocument := TNyxDocument.Create;
  try
    GOwner.Scheduler := NewNyxScheduler;
    GOwner.Port := TPort.Create;
    GOwner.PortLease := GOwner.Port;
    LPage := NewNyxPage('home');
    LPage.Add(NewNyxHeading('title').WithText('A report worth keeping'));
    LPage.Add(NewNyxLabel('intro').WithText(
      'Format your JSON and save its exact UTF-8 bytes through connected Nyx components.'));
    LMemo := NewNyxMemo('report-source').WithText('JSON source');
    LMemo.Configure.Value(FileSaveJSON).Height(180).Done;
    LPage.Add(LMemo);
    LPage.Add(NewNyxButton('save-report').WithText('Save formatted report'));
    LOptions := NyxFileSaveOptions('draft.json').Title('Save a Nyx report')
      .MediaType(NyxMediaType(nfmtJSON)).Allow('json')
      .Mode({$ifdef PAS2JS}nfsmDownload{$else}nfsmPicker{$endif});
    LPage.Add(NewNyxFileSaveEditor('save-options', LOptions));
    GDocument.AddPage(LPage);
    {$ifdef PAS2JS}
    GOwner.Host := NewNyxBrowserFileSaveHost;
    LRequest := GOwner.Host.BeginSave(NyxFileSaveOptions('activation.txt')
      .Mode(nfsmDownload), NyxEncodeUTF8('prepared'), GOwner.Scheduler, nil,
      @GOwner.ActivationReply);
    Check(GOwner.ActivationReplies = 1, 'activation refusal completes exactly once');
    LRequest := nil;
    {$ifdef NYX_SAVE_ACTIVATION}
    GPrompts := TPrompts.Create;
    GPromptsLease := GPrompts;
    GOwner.Host := NewNyxActivatedFileSaveHost(GOwner.Host, GPromptsLease);
    {$else}
    GOwner.Host := NewNyxBrowserActivatedFileSaveHost(GOwner.Host);
    {$endif}
    GRenderer := TNyxBrowserRenderer.Create;
    GRenderer.OnEvent := @GOwner.Event;
    GRenderer.Render(GDocument, LPage.Node, TJSHTMLElement(document.body));
    document.body.setAttribute('data-file-save-controls', 'ready');
    GStarted := TJSDate.now;
    Drive;
    {$else}

    if ParamCount <> 1 then
    begin
      raise Exception.Create('Supply a fresh owned output directory');
    end;
    GDirectory := ExpandFileName(ParamStr(1));

    if DirectoryExists(GDirectory) then
    begin
      raise Exception.Create('Native save fixture requires a fresh output directory');
    end;
    ForceDirectories(GDirectory);
    Application.Initialize;
    GForm := TForm.CreateNew(nil);
    GForm.SetBounds(20, 20, 840, 650);
    GRenderer := TNyxLCLRenderer.Create;
    GRenderer.OnEvent := GOwner.Event;
    GRenderer.Render(GDocument, LPage.Node, GForm);
    GForm.Show;
    GOwner.Host := NewNyxLCLFileSaveHost(TNyxText(GDirectory));
    {$ifdef NYX_SAVE_ACTIVATION}
    GPrompts := TPrompts.Create;
    GPromptsLease := GPrompts;
    GOwner.Host := NewNyxActivatedFileSaveHost(GOwner.Host, GPromptsLease, nsapAlwaysPrompt);
    GPromptTimer := TTimer.Create(nil);
    GPromptTimer.Interval := 100;
    GPromptTimer.OnTimer := GOwner.PromptTurn;
    {$else}
    GOwner.Host := NewNyxLCLActivatedFileSaveHost(GForm, GOwner.Host);
    {$endif}
    GTimer := TTimer.Create(nil);
    GTimer.Interval := 50;
    GTimer.OnTimer := GOwner.DialogTurn;
    GOwner.DialogStarted := GetTickCount64;
    TEdit(GRenderer.InputFor('save-options-name')).Text := FileSaveLeaf;
    Check(CaptureNyxFileSaveEditor('save-options', GRenderer.Root).SuggestedName = FileSaveLeaf,
      'actual native filename edit configures the typed save operation');
    TNyxLCLButton(GRenderer.ControlFor('save-report')).Click;
    LStarted := GetTickCount64;
    repeat
      CheckSynchronize;
      Application.ProcessMessages;

      if GOwner.DialogError <> '' then
      begin
        raise Exception.Create(GOwner.DialogError);
      end;

      if (GOwner.Run <> nil) and (GOwner.Run.Phase = nqpFailed) then
      begin
        raise Exception.Create(GOwner.Run.Failure);
      end;

      if GOwner.Port.Calls > 0 then
      begin
        Break;
      end;

      if GetTickCount64 - LStarted > 20000 then
      begin
        raise Exception.Create('Native graph save deadline');
      end;
      Sleep(1);
    until False;
    CheckGraph;
    {$ifdef NYX_SAVE_ACTIVATION}
    Check((GPrompts.Count = 1) and not GPrompts.Current.IsOpen,
      'actual native Save action closes its Nyx prompt before opening the OS picker');
    {$endif}
    Check(GOwner.DialogCount > 0, 'actual owned native save dialog was driven');
    LExpected := NyxEncodeUTF8(NewNyxJSONFormat(njlIndented, 2)
      .Format(TNyxDataValue.ParseJSON(FileSaveJSON)));
    LStream := TFileStream.Create(GDirectory + PathDelim + String(FileSaveLeaf), fmOpenRead);
    try
      SetLength(LActual, LStream.Size);

      if LStream.Size > 0 then
      begin
        LStream.ReadBuffer(LActual[1], Length(LActual));
      end;
    finally
      LStream.Free;
    end;
    Check(Length(LActual) = Length(LExpected), 'written file has exact byte length');
    LExact := True;
    for LIndex := 0 to High(LExpected) do
    begin
      LExact := LExact and (Ord(LActual[LIndex + 1]) = LExpected[LIndex]);
    end;
    Check(LExact, 'every written UTF-8 byte is exact');

    { The same real button/graph now enters a second actual picker. Cancellation
      is usable graph data and must leave the already written report unchanged. }
    GOwner.DialogMode := 1;
    GOwner.DialogStarted := GetTickCount64;
    TNyxLCLButton(GRenderer.ControlFor('save-report')).Click;
    LStarted := GetTickCount64;
    repeat
      CheckSynchronize;
      Application.ProcessMessages;

      if (GOwner.Run.Phase = nqpFailed) or (GOwner.DialogError <> '') then
      begin
        raise Exception.Create('Native cancel journey failed: ' + GOwner.Run.Failure + GOwner.DialogError);
      end;

      if GetTickCount64 - LStarted > 20000 then
      begin
        raise Exception.Create('Native cancel journey deadline');
      end;
      Sleep(1);
    until GOwner.Port.Calls = 2;
    Check(GOwner.Port.Last.Value(FileEndpoint(nfpCancelled)).AsBoolean and
      not GOwner.Port.Last.Value(FileEndpoint(nfpDelivered)).AsBoolean,
      'real native picker cancellation produces an undelivered result');
    Check(FileExists(GDirectory + PathDelim + String(FileSaveLeaf)),
      'picker cancellation retains the previously written report');

    { Retirement while a modal dialog is actually open revokes the reply, closes
      that dialog and leaves the complete previous graph output untouched. }
    GOwner.DialogMode := 2;
    GOwner.DialogStarted := GetTickCount64;
    TNyxLCLButton(GRenderer.ControlFor('save-report')).Click;
    LStarted := GetTickCount64;
    repeat
      CheckSynchronize;
      Application.ProcessMessages;

      if (GOwner.Run.Phase = nqpFailed) or (GOwner.DialogError <> '') then
      begin
        raise Exception.Create('Native retirement failed: ' + GOwner.Run.Failure + GOwner.DialogError);
      end;

      if GetTickCount64 - LStarted > 20000 then
      begin
        raise Exception.Create('Native retirement deadline');
      end;
      Sleep(1);
    until GOwner.Run.Phase = nqpCancelled;
    Check(GOwner.Port.Calls = 2, 'actual modal retirement cannot publish a late output');
    {$ifdef NYX_SAVE_ACTIVATION}
    Check(not GPrompts.Current.IsOpen, 'native scope retirement closes the actual Nyx confirmation');
    {$endif}
    LStream := TFileStream.Create(GDirectory + PathDelim + String(FileSaveLeaf), fmOpenRead);
    try
      SetLength(LActual, LStream.Size);
      LStream.ReadBuffer(LActual[1], Length(LActual));
    finally
      LStream.Free;
    end;
    LExact := Length(LActual) = Length(LExpected);
    for LIndex := 0 to High(LExpected) do
    begin
      LExact := LExact and (Ord(LActual[LIndex + 1]) = LExpected[LIndex]);
    end;
    Check(LExact, 'both cancelled native saves preserve every prior disk byte');
    LPage := nil;
    LMemo := nil;
    Retire;
    WriteLn('PASS ', GChecks, ' actual native Nyx graph, save dialog and exact disk bytes');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-file-save-controls', 'failed');
      document.body.setAttribute('data-file-save-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      LPage := nil;
      LMemo := nil;
      Retire;
      {$endif}
    end;
  end;
end.
