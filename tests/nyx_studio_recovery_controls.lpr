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
program nyx_studio_recovery_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifdef PAS2JS}JS, Web, nyx.files, nyx.studio.browser,
    nyx.studio.sourcecompilation.shared.browser, nyx.studio.exchange,
  {$else}Interfaces, Classes, Forms, Controls, StdCtrls, Types, nyx.bytes, nyx.studio.lcl,
    nyx.studio.projectstore, nyx.studio.workspaces, nyx.studio.session,
    nyx.test.capture.lcl, nyx.menu, nyx.menu.lcl, nyx.types, nyx.studio.menu,
  {$endif}
  SysUtils, nyx.text, nyx.data, nyx.studio.projects, nyx.studio.agentview;

{$ifdef PAS2JS}
type
  { Controlled file-host outcomes qualify the ordinary editor's branching.
    Requested captures an independent immutable file in memory, not a disk save;
    cancelled/refused exports never replace an earlier captured backup. }
  TBackupOutcome = (boRequested, boCancelled, boRefused);

  { Hold is only the setup of a retained-local journey. No exchange packet is
    admitted or answered; ordinary source edits persist before the page closes.
    Reopening Automatic uses the real backend and its unchanged protocol. }
  TFixtureConnection = (fcAutomatic, fcHeld);

  { Owns the production clock/transport and a copied held request. Never fakes
    an acknowledgement or calls a receiver. Cancel revokes the borrowed receiver
    before releasing bytes; the owning Studio/page retires the complete wrapper. }
  THeldConnection = class(TNyxStudioEditorExchange)
  private
    FInner: TNyxStudioEditorExchange;
    FReply: TNyxEditorReply;
    FBody: TNyxText;
  public
    constructor Create(AInner: TNyxStudioEditorExchange);
    destructor Destroy; override;
    procedure Post(AConnect: Boolean; const AToken, ABody: TNyxText;
      AReply: TNyxEditorReply); override;
    procedure CancelRequest; override;
    procedure Schedule(ADelayMS: Integer; ATick: TNyxEditorTick); override;
    procedure CancelTick; override;
  end;

  { The real Studio consumes its public file-exchange seam. Pick asynchronously
    returns the captured backup through the ordinary import path; it never edits
    a session or fabricates source authority. Cancel revokes the borrowed reply
    and timer. A self lease lasts only until completion/cancellation. }
  TRecoveryFiles = class(TInterfacedObject, INyxTextFileExchange)
  private
    FLast: INyxTextFile;
    FReply: TNyxTextFileReply;
    FSelection: TNyxTextFileSelection;
    FTimer: NativeInt;
    FLease: INyxTextFileExchange;
    procedure Picked;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Pick(const ASelection: TNyxTextFileSelection; AReply: TNyxTextFileReply);
    function ExportFiles(const AFiles: TNyxTextFiles): Boolean;
    procedure Cancel;
  end;

  { Controlled file delivery and an optional unsent setup claim use public host
    seams. Automatic reconnect uses the real same-origin exchange. Rendering,
    input routing, recovery and source admission remain production code. }
  TRecoveryStudio = class(TNyxStudio)
  protected
    function CreateProjectFiles: INyxTextFileExchange; override;
    function CreateEditorExchange: TNyxStudioEditorExchange; override;
  end;

var
  GStudio: TRecoveryStudio;
  GOutcome: TBackupOutcome;
  GConnection: TFixtureConnection;

constructor THeldConnection.Create(AInner: TNyxStudioEditorExchange);
begin
  inherited Create;
  FInner := AInner;
end;

destructor THeldConnection.Destroy;
begin
  CancelRequest;
  CancelTick;
  FInner.Free;
  inherited Destroy;
end;

procedure THeldConnection.Post(AConnect: Boolean; const AToken, ABody: TNyxText;
  AReply: TNyxEditorReply);
begin

  if Assigned(FReply) then
  begin
    raise Exception.Create('The held connection already owns a pending request');
  end;
  FBody := ABody;
  FReply := AReply;
end;

procedure THeldConnection.CancelRequest;
begin
  FReply := nil;
  FBody := '';
  FInner.CancelRequest;
end;

procedure THeldConnection.Schedule(ADelayMS: Integer; ATick: TNyxEditorTick);
begin
  FInner.Schedule(ADelayMS, ATick);
end;

procedure THeldConnection.CancelTick;
begin
  FInner.CancelTick;
end;

constructor TRecoveryFiles.Create;
begin
  inherited Create;
  FTimer := -1;
end;

destructor TRecoveryFiles.Destroy;
begin
  Cancel;
  inherited Destroy;
end;

procedure TRecoveryFiles.Cancel;
begin

  if FTimer >= 0 then
  begin
    window.clearTimeout(FTimer);
    FTimer := -1;
  end;
  FReply := nil;
  FLease := nil;
end;

procedure TRecoveryFiles.Pick(const ASelection: TNyxTextFileSelection;
  AReply: TNyxTextFileReply);
begin
  Cancel;
  FSelection := ASelection;
  FReply := AReply;
  FLease := Self;
  FTimer := window.setTimeout(@Picked, 0);
end;

procedure TRecoveryFiles.Picked;
var
  LLease: INyxTextFileExchange;
  LReply: TNyxTextFileReply;
  LFiles: TNyxTextFiles;
begin
  LLease := FLease;
  LReply := FReply;
  FLease := nil;
  FReply := nil;
  FTimer := -1;

  if not Assigned(LReply) then
  begin
    Exit;
  end;

  if FLast = nil then
  begin
    LReply(fpsCancelled, nil, '');
    Exit;
  end;
  SetLength(LFiles, 1);
  LFiles[0] := FLast;
  try
    ValidateNyxTextFiles(LFiles, FSelection);
  except
    on LException: Exception do
    begin
      LReply(fpsFailed, nil, LException.Message);
      Exit;
    end;
  end;
  LReply(fpsSelected, LFiles, '');
end;

function TRecoveryFiles.ExportFiles(const AFiles: TNyxTextFiles): Boolean;
begin
  ValidateNyxTextFiles(AFiles, NyxTextFileSelection.UpToFiles(1));

  if Length(AFiles) <> 1 then
  begin
    raise ENyxFile.Create('The recovery fixture expects one exact project backup');
  end;
  document.body.setAttribute('data-recovery-backup-attempt', AFiles[0].Text);

  if GOutcome = boRefused then
  begin
    document.body.setAttribute('data-recovery-backup-outcome', 'refused');
    raise ENyxFile.Create('Controlled backup host refusal');
  end;

  if GOutcome = boCancelled then
  begin
    document.body.setAttribute('data-recovery-backup-outcome', 'cancelled');
    Exit(False);
  end;
  FLast := NyxTextFile(AFiles[0].Name, AFiles[0].Text);
  document.body.setAttribute('data-recovery-backup-outcome', 'requested');
  document.body.setAttribute('data-recovery-backup-packet', FLast.Text);
  Result := True;
end;

function TRecoveryStudio.CreateProjectFiles: INyxTextFileExchange;
begin
  Result := TRecoveryFiles.Create;
end;

function TRecoveryStudio.CreateEditorExchange: TNyxStudioEditorExchange;
begin
  Result := inherited CreateEditorExchange;

  if GConnection = fcHeld then
  begin
    Result := THeldConnection.Create(Result);
  end;
end;

{ Read only the public copied snapshot between presentation callbacks. This
  small-document fixture publishes exact comparison evidence, never a mutable
  application object. It does not qualify editor latency or physical disk I/O.
  The page owns the timer and Studio together for its complete lifetime. }
procedure Observe;
var
  LPacket: TNyxText;
begin

  if (GStudio = nil) or GStudio.PresentationPending then
  begin
    Exit;
  end;
  LPacket := EncodeNyxProject(GStudio.ProjectSnapshot);

  if Length(LPacket) > 131072 then
  begin
    document.body.setAttribute('data-recovery-fixture-error', 'Fixture document exceeds its evidence budget');
    Exit;
  end;
  document.body.setAttribute('data-recovery-local-packet', LPacket);
  document.body.setAttribute('data-recovery-observation', FloatToStr(TJSDate.now));
end;

procedure RunBrowser;
var
  LArguments: TJSURLSearchParams;
  LMode: JSValue;
begin
  GOutcome := boRequested;
  GConnection := fcAutomatic;
  LArguments := TJSURLSearchParams.new(window.location.search);
  LMode := LArguments.get('backup');

  if isString(LMode) then
  begin

    if String(LMode) = 'cancel' then
    begin
      GOutcome := boCancelled;
    end
    else if String(LMode) = 'refuse' then
    begin
      GOutcome := boRefused;
    end
    else if String(LMode) <> 'request' then
    begin
      raise Exception.Create('Unknown controlled backup outcome');
    end;
  end;
  LMode := LArguments.get('connect');

  if isString(LMode) then
  begin

    if String(LMode) = 'hold' then
    begin
      GConnection := fcHeld;
    end
    else if String(LMode) <> 'auto' then
    begin
      raise Exception.Create('Unknown qualification connection mode');
    end;
  end;
  document.body.setAttribute('data-recovery-file-host', 'controlled; no disk writes');
  GStudio := TRecoveryStudio.Create(nil, NewNyxSharedBrowserSourceCompilerFactory);
  GStudio.Run;
  window.setInterval(@Observe, 500);
end;
{$else}
type
  { Qualification invokes the actual LCL control's published click contract.
    This accessor does not change Studio, renderer or model implementation. }
  TControlAccess = class(TControl);

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure AwaitStudio(AStudio: TNyxNativeStudio);
var
  LDeadline: QWord;
begin
  LDeadline := GetTickCount64 + 30000;
  repeat
    Application.ProcessMessages;

    if GetTickCount64 > LDeadline then
    begin
      raise Exception.Create('Ordinary recovery control timed out / ' + AStudio.Status);
    end;
    Sleep(1);
  until not AStudio.PresentationPending and not AStudio.SourceBusy and not AStudio.Agents.Busy;
end;

procedure Click(AStudio: TNyxNativeStudio; const AID: TNyxText);
var
  LControl: TControl;
begin
  LControl := AStudio.ShellView.ControlFor(AID);
  Check((LControl <> nil) and (LControl is TWinControl) and LControl.Enabled and
    TWinControl(LControl).Showing,
    'Ordinary recovery action is mounted and showing: ' + AID);

  if LControl is TWinControl then
  begin
    TWinControl(LControl).SetFocus;
    Check(TWinControl(LControl).Focused, 'Recovery action can receive native focus: ' + AID);
  end;
  TControlAccess(LControl).Click;
  AwaitStudio(AStudio);
end;

{ Observe the destination before the harness requests any further focus. A
  mounted/Showing control may still be clipped by an enclosing independent
  section's scroll host; require its full face inside every client rectangle. }
procedure CheckDestination(AStudio: TNyxNativeStudio; const AID: TNyxText);
var
  LControl: TWinControl;
  LParent: TWinControl;
  LStart: TPoint;
  LEnd: TPoint;
  LClip: TPoint;
begin
  LControl := AStudio.ShellView.FocusFor(AID);
  Check((LControl <> nil) and LControl.Focused,
    'Studio directs keyboard focus to the next recovery action: ' + AID);
  LStart := LControl.ClientToScreen(Point(0, 0));
  LEnd := LControl.ClientToScreen(Point(LControl.ClientWidth, LControl.ClientHeight));
  LParent := LControl.Parent;
  while LParent <> nil do
  begin
    LClip := LParent.ClientToScreen(Point(0, 0));
    Check((LStart.X >= LClip.X) and (LStart.Y >= LClip.Y) and
      (LEnd.X <= LClip.X + LParent.ClientWidth) and
      (LEnd.Y <= LClip.Y + LParent.ClientHeight),
      'Recovery destination is not clipped by ' + LParent.ClassName + ': ' + AID);
    LParent := LParent.Parent;
  end;
end;

{ Short presentation intentionally hides the panel bar. Follow the real menu
  family rather than invoking a hidden desktop control or a controller method.
  Managed menu leases keep their borrowed renderer lookups local to each step. }
procedure ProjectMenuAction(AStudio: TNyxNativeStudio; AAction: TNyxStudioMenuAction);
var
  LParent: INyxLCLMenu;
  LChild: INyxMenu;
  LNative: INyxLCLMenu;
  LControl: TControl;
  LID: TNyxText;
begin
  case AAction of
    smaAgents: LID := 'studio-menu-agents';
    smaOpen: LID := 'studio-menu-open';
  else
    raise Exception.Create('This journey qualifies only recovery and Open project menu actions');
  end;
  Click(AStudio, NyxStudioActionMenuID);
  LParent := AStudio.ActionMenu;
  Check((LParent <> nil) and LParent.IsOpen, 'The ordinary Actions popup is open');
  LControl := LParent.Presentation.Renderer.ControlFor('studio-menu-project');
  Check((LControl is TWinControl) and TWinControl(LControl).Showing,
    'Project is a showing menu action');
  TControlAccess(LControl).Click;
  AwaitStudio(AStudio);
  LChild := LParent.Submenu(NyxPart('project'));
  Check((LChild <> nil) and LChild.IsOpen and Supports(LChild, INyxLCLMenu, LNative),
    'Project opens its independent native submenu');
  LControl := LNative.Presentation.Renderer.ControlFor(LID);
  Check((LControl is TWinControl) and TWinControl(LControl).Showing,
    'The selected project command is a showing submenu action');
  TWinControl(LControl).SetFocus;
  Check(TWinControl(LControl).Focused, 'The project menu action accepts native focus');
  TControlAccess(LControl).Click;
  AwaitStudio(AStudio);
end;

function ReadText(const APath: TNyxText): TNyxText;
var
  LFile: TFileStream;
  LBytes: TNyxBytes;
begin
  LFile := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try
    SetLength(LBytes, LFile.Size);

    if Length(LBytes) > 0 then
    begin
      LFile.ReadBuffer(LBytes[0], Length(LBytes));
    end;
    Result := NyxDecodeUTF8(LBytes);
  finally
    LFile.Free;
  end;
end;

procedure RunNative;
var
  LRoot: TNyxText;
  LExpected: TNyxText;
  LWidth: Integer;
  LHeight: Integer;
  LWindow: TForm;
  LStudio: TNyxNativeStudio;
  LStore: TNyxProjectStore;
  LBefore: TNyxText;
  LBackup: TNyxText;
  LBackupName: TNyxText;
  LName: TEdit;
  LRevision: TNyxText;
  LHistory: TNyxStudioRecoveryFrame;
  LAfter: TNyxStudioRecoveryFrame;
begin

  if (ParamCount <> 4) and (ParamCount <> 5) then
  begin
    raise Exception.Create('Usage: evidence-root service-url client-width expected-peer-packet [client-height]');
  end;
  LRoot := ExpandFileName(ParamStr(1));
  LWidth := StrToInt(ParamStr(3));
  LHeight := 844;

  if ParamCount = 5 then
  begin
    LHeight := StrToInt(ParamStr(5));
  end;

  if (LWidth < 240) or (LWidth > 4096) or (LHeight < 240) or (LHeight > 4096) then
  begin
    raise Exception.Create('Recovery qualification client dimensions must be 240..4096 pixels');
  end;
  LExpected := ReadText(ParamStr(4));
  ForceDirectories(LRoot);
  LWindow := TForm.CreateNew(nil);
  LStudio := nil;
  LStore := nil;
  try
    LWindow.Caption := 'Nyx ordinary recovery qualification';
    LWindow.ClientWidth := LWidth;
    LWindow.ClientHeight := LHeight;
    LWindow.Show;
    LStudio := TNyxNativeStudio.Create(LWindow, IncludeTrailingPathDelimiter(LRoot) + 'project');
    LStudio.Session.SetTitle('A first local plan');
    LStudio.Session.SetTitle('The retained local plan');
    LStudio.Session.SetSourceDraft(LStudio.Session.Source + #10 + TNyxText('{ Unfinished local idea 🌙'));
    LStudio.Run;
    AwaitStudio(LStudio);
    LBefore := EncodeNyxProject(LStudio.Session.ProjectSnapshot);
    LHistory := LStudio.Session.RecoveryFrame;
    LStudio.ConnectService(ParamStr(2), NyxPrimaryWorkspace);
    AwaitStudio(LStudio);
    Check(LStudio.Agents.Conflict and
      (EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore),
      'The real service retains the independently different local pair');

    if (LHeight < 501) and (LWidth < 1201) then
    begin
      ProjectMenuAction(LStudio, smaAgents);
      CheckDestination(LStudio, NyxStudioRecoveryBackupID);
    end
    else if LWidth < 640 then
    begin
      Click(LStudio, 'action-panel-design');

      if (LStudio.ShellView.ControlFor('action-details-toggle') <> nil) and
        ((LStudio.ShellView.ControlFor('action-agent-accept') = nil) or
          not TWinControl(LStudio.ShellView.ControlFor('action-agent-accept')).Showing) then
      begin
        Click(LStudio, 'action-details-toggle');
      end;
    end;
    Click(LStudio, 'action-agent-accept');
    Check(LStudio.Agents.Conflict and
      (LStudio.Agents.RecoveryReview.Delivery = sbdStored) and
      (EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore),
      'Saving the native backup does not itself select the shared project');
    CheckDestination(LStudio, NyxStudioRecoveryConfirmID);
    LStore := TNyxProjectStore.Create(IncludeTrailingPathDelimiter(LRoot) + 'project');
    LBackupName := LStudio.Agents.RecoveryReview.BackupName;
    LBackup := LStore.ReadProject(LBackupName, LRevision);
    Check((LRevision <> '') and (LBackup = LBefore),
      'An independent disk reader observes the exact complete local backup');
    SaveNyxNativeCapture(LWindow, IncludeTrailingPathDelimiter(LRoot) +
      'backup-review-printed.png', ncmPrint);
    Click(LStudio, NyxStudioRecoveryCancelID);
    LAfter := LStudio.Session.RecoveryFrame;
    Check((LStudio.Agents.RecoveryReview.Delivery = sbdNone) and
      (EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore) and
      (Length(LAfter.Undo) = Length(LHistory.Undo)) and
      (Length(LAfter.Redo) = Length(LHistory.Redo)),
      'Cancel retains the complete local pair and history');
    CheckDestination(LStudio, NyxStudioRecoveryBackupID);
    Click(LStudio, 'action-agent-pause');
    Check(LStudio.Agents.Paused and not LStudio.Agents.Connected and
      (EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore),
      'Keep local has a distinct paused outcome and preserves the exact local pair');
    CheckDestination(LStudio, NyxStudioRecoveryReconnectID);
    Click(LStudio, 'action-agent-connect');
    Check(LStudio.Agents.Conflict and not LStudio.Agents.Paused,
      'Reconnect reviews the genuinely different shared project again');
    Click(LStudio, 'action-agent-accept');
    Click(LStudio, NyxStudioRecoveryConfirmID);
    Check(not LStudio.Agents.Conflict and
      (EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LExpected),
      'Only the explicit confirmation admits the exact shared project');
    ProjectMenuAction(LStudio, smaOpen);
    Check(LStudio.ShellView.InputFor('project-file-name') is TEdit,
      'Ordinary Open project exposes the saved-project name');
    LName := TEdit(LStudio.ShellView.InputFor('project-file-name'));
    LName.Text := String(LBackupName);
    LName.OnChange(LName);
    AwaitStudio(LStudio);
    Click(LStudio, 'action-project-open');

    if LStudio.ShellView.RootFor('project-conflict-warning') <> nil then
    begin
      Click(LStudio, 'action-project-use-remote');
    end;
    Check((EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore) and
      LStudio.Session.SourceDraftPending,
      'Ordinary native Open restores the exact saved backup and its unfinished Unicode draft');
    SaveNyxNativeCapture(LWindow, IncludeTrailingPathDelimiter(LRoot) +
      'restored-backup-printed.png', ncmPrint);
  finally
    LStore.Free;
    LStudio.Free;
    LWindow.Free;
  end;
end;
{$endif}

begin
  try
    {$ifdef PAS2JS}
    RunBrowser;
    {$else}
    Application.Initialize;
    RunNative;
    WriteLn('PASS ', GChecks, ' ordinary recovery control checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-recovery-fixture-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
