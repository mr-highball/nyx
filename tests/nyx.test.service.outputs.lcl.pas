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
unit nyx.test.service.outputs.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.application.lcl, nyx.model;

{ Borrow the already mounted generated application/document until this call
  returns. Neither service factories nor renderer/host implementations change.
  Command-line arguments select width1280/390 and a fresh owned output directory.
  Synthetic LCL input and this process's actual Windows Save dialogs qualify real
  effects; they do not establish physical-device or accessibility quality. }
procedure RunNyxGeneratedServiceOutputs(AApplication: TNyxLCLApplication;
  ADocument: TNyxDocument);

implementation

uses Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Grids, Types, Windows,
  nyx.text, nyx.bytes, nyx.types, nyx.codec, nyx.state, nyx.editing.lcl,
  nyx.clipboard, nyx.files.save, nyx.menu.lcl, nyx.popover.lcl,
  nyx.collections, nyx.collections.view, nyx.collections.view.types,
  nyx.collections.mount, nyx.collections.columns, nyx.collections.arrangement,
  nyx.collections.arrangement.controls, nyx.collections.arrangement.presenter,
  nyx.collections.arrangement.lcl,
  nyx.collections.query, nyx.collections.paging, nyx.collections.cell.actions,
  nyx.collections.export, nyx.collections.export.actions, nyx.test.capture.lcl;

const
  CGatewayCommand: TNyxText = '/opt/example/bin/gateway --listen=127.0.0.1 --config=/opt/example/config/gateway.json';
  CReportsCommand: TNyxText = '/opt/example/bin/reports --schedule=hourly';
  CHeaders: TNyxText = 'name' + #9 + 'notes' + #9 + 'pid' + #9 + 'status';
  CGatewayRow: TNyxText = 'Request gateway' + #9 +
    'Accepts incoming requests and routes them to healthy workers.' + #9 + '2401' + #9 + 'Running';
  CReportsRow: TNyxText = 'Report scheduler' + #9 +
    'The next report is ready to run.' + #9 + '2427' + #9 + 'Running';

type
  { Capture asynchronous event failures in this process only. Detach the borrowed
    receiver before freeing it, restoring the caller's previous exception hook. }
  TFailureObserver = class
  public
    Text: TNyxText;
    procedure Failed(ASender: TObject; AError: Exception);
  end;
  { The timer selects/cancels only this process's named export Save dialog.
    Paths belong to the fresh evidence directory; existing files are refused.
    A nested dialog loop may run this timer while the ordinary menu is entered. }
  TSaveDialogDriver = class
  public
    Path: UnicodeString;
    Cancel: Boolean;
    Dialog: HWND;
    Filename: HWND;
    Actions: Integer;
    Started: QWord;
    Error: TNyxText;
    procedure Tick(ASender: TObject);
  end;

var
  GChecks: Integer;
  GFailure: TFailureObserver;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Generated Service outputs: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure TFailureObserver.Failed(ASender: TObject; AError: Exception);
begin
  Text := TNyxText(AError.Message);
end;

procedure Drain;
var
  LTurn: Integer;
begin
  for LTurn := 1 to 4 do
  begin
    CheckSynchronize;
    Application.ProcessMessages;
    Application.Idle(False);

    if GFailure.Text <> '' then
    begin
      raise Exception.Create(GFailure.Text);
    end;
  end;
end;

function FindSaveDialog(AWindow: HWND; AData: LPARAM): BOOL; stdcall;
var
  LProcess: DWORD;
  LTitle: array[0..127] of WideChar;
begin
  Result := True;
  GetWindowThreadProcessId(AWindow, @LProcess);

  if (LProcess <> GetCurrentProcessId) or not IsWindowVisible(AWindow) then
  begin
    Exit;
  end;
  GetWindowTextW(AWindow, LTitle, Length(LTitle));

  if UnicodeString(LTitle) = 'Export accepted table rows' then
  begin
    TSaveDialogDriver(AData).Dialog := AWindow;
    Result := False;
  end;
end;

function FindSaveFilename(AWindow: HWND; AData: LPARAM): BOOL; stdcall;
var
  LClass: array[0..63] of WideChar;
begin
  Result := True;
  GetClassNameW(AWindow, LClass, Length(LClass));

  if (UnicodeString(LClass) = 'Edit') and
    ((GetDlgCtrlID(AWindow) = 1001) or (GetDlgCtrlID(AWindow) = 1148)) then
  begin
    TSaveDialogDriver(AData).Filename := AWindow;
    Result := False;
  end;
end;

procedure TSaveDialogDriver.Tick(ASender: TObject);
var
  LReply: DWORD_PTR;
begin
  Dialog := 0;
  EnumWindows(@FindSaveDialog, LPARAM(Self));

  if Dialog = 0 then
  begin
    Exit;
  end;

  if GetTickCount64 - Started > 10000 then
  begin
    Error := 'Owned Save dialog exceeded its input deadline';
    Cancel := True;
  end;

  if Cancel then
  begin
    TTimer(ASender).Enabled := False;
    PostMessageW(Dialog, WM_COMMAND, IDCANCEL, 0);
    Inc(Actions);
    Exit;
  end;
  Filename := 0;
  EnumChildWindows(Dialog, @FindSaveFilename, LPARAM(Self));

  if (Filename = 0) or not IsWindowEnabled(Filename) then
  begin
    Exit;
  end;

  if SendMessageTimeoutW(Filename, WM_SETTEXT, 0, LPARAM(PWideChar(Path)),
    SMTO_ABORTIFHUNG, 1000, @LReply) = 0 then
  begin
    Exit;
  end;
  TTimer(ASender).Enabled := False;
  PostMessageW(Dialog, WM_COMMAND, IDOK, 0);
  Inc(Actions);
end;

{ Observe Windows' real plain-text clipboard. No expected-value injection or
  content logging occurs. The clipboard and borrowed global memory stay locked
  only while comparing the known value and one terminating UTF-16 code unit. }
function ClipboardMatches(const AExpected: TNyxText): Boolean;
var
  LHandle: THandle;
  LText: PWideChar;
  LIndex: Integer;
begin
  Result := False;

  if not OpenClipboard(0) then
  begin
    raise Exception.Create('Cannot read system clipboard');
  end;
  try
    LHandle := GetClipboardData(CF_UNICODETEXT);

    if (LHandle = 0) or
      (GlobalSize(LHandle) < (PtrUInt(Length(AExpected)) + 1) * SizeOf(WideChar)) then
    begin
      Exit;
    end;
    LText := PWideChar(GlobalLock(LHandle));

    if LText = nil then
    begin
      Exit;
    end;
    try
      for LIndex := 1 to Length(AExpected) do
      begin

        if LText[LIndex - 1] <> AExpected[LIndex] then
        begin
          Exit;
        end;
      end;
      Result := LText[Length(AExpected)] = #0;
    finally
      GlobalUnlock(LHandle);
    end;
  finally
    CloseClipboard;
  end;
end;

{ Compare actual disk bytes against hand-written accepted-record expectations,
  independently of the table export encoder and its own LastPayload. }
procedure VerifyDisk(const APath: String; const AExpected: TNyxText);
var
  LStream: TFileStream;
  LActual: TNyxBytes;
  LExpected: TNyxBytes;
  LIndex: Integer;
begin
  LExpected := NyxEncodeUTF8(AExpected);
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try
    Check(LStream.Size = Length(LExpected), 'disk export has the exact independent UTF-8 length');
    SetLength(LActual, LStream.Size);

    if Length(LActual) > 0 then
    begin
      LStream.ReadBuffer(LActual[0], Length(LActual));
    end;
  finally
    LStream.Free;
  end;
  for LIndex := 0 to High(LExpected) do
  begin

    if LActual[LIndex] <> LExpected[LIndex] then
    begin
      raise Exception.Create('Actual exported TSV differs from the independent expected bytes');
    end;
  end;
  Check(True, 'actual disk TSV has exact headers, visible field order and accepted raw records');
end;

procedure RunNyxGeneratedServiceOutputs(AApplication: TNyxLCLApplication;
  ADocument: TNyxDocument);
var
  LMount: INyxCollectionMount;
  LColumns: INyxCollectionColumnArrangementMount;
  LManager: INyxColumnArrangementManager;
  LActionsMount: INyxCollectionCellActionsMount;
  LActions: INyxCollectionCellActions;
  LExports: INyxCollectionExportMount;
  LPages: INyxCollectionPages;
  LInspector: INyxLCLPopover;
  LTimer: TTimer;
  LDialog: TSaveDialogDriver;
  LPreviousException: TExceptionEvent;
  LBefore: TNyxText;
  LWidth: Integer;
  LDirectory: String;
  LGrid: TStringGrid;
  LColumn: Integer;
  LRevision: Integer;

  function GridFor(const ACaption: TNyxText): TStringGrid;
  var
    LTable: TStringGrid;
    LCandidate: TStringGrid;
    LChild: Integer;
    LPhysical: Integer;
  begin
    LTable := TStringGrid(AApplication.View.ControlFor('service-table'));
    for LChild := -1 to LTable.ControlCount - 1 do
    begin
      LCandidate := LTable;

      if LChild >= 0 then
      begin

        if not (LTable.Controls[LChild] is TStringGrid) then
        begin
          Continue;
        end;
        LCandidate := TStringGrid(LTable.Controls[LChild]);
      end;
      for LPhysical := 0 to LCandidate.ColCount - 1 do
      begin

        if LCandidate.IsVisible and (LCandidate.ColWidths[LPhysical] > 0) and
          (Pos(ACaption, TNyxText(LCandidate.Cells[LPhysical, 0])) = 1) then
        begin
          LColumn := LPhysical;
          Exit(LCandidate);
        end;
      end;
    end;
    raise Exception.Create('Generated table has no visible field ' + ACaption);
  end;

  procedure Context(const ACaption: TNyxText; ARow: Integer = 1);
  var
    LBounds: Types.TRect;
    LPoint: Types.TPoint;
  begin
    LGrid := GridFor(ACaption);
    LGrid.LeftCol := LColumn;
    LGrid.Col := LColumn;
    LGrid.Row := ARow;
    LGrid.SetFocus;
    Drain;
    LBounds := LGrid.CellRect(LColumn, ARow);
    Check(PtInRect(LGrid.ClientRect, Types.Point(LBounds.Left + 4, LBounds.Top + 4)),
      'context input targets a real visible native data cell');
    LPoint := LGrid.ClientToScreen(Types.Point(LBounds.Left + 4, LBounds.Top + 4));
    SendMessage(LGrid.Handle, WM_CONTEXTMENU, LGrid.Handle, MakeLParam(LPoint.X, LPoint.Y));
    Drain;
    Check((LActions.Menu <> nil) and LActions.Menu.IsOpen, 'real native context message opens cell menu');
  end;

  procedure Menu(const AID: TNyxText);
  var
    LMenu: INyxLCLMenu;
    LButton: TButton;
  begin
    Check(Supports(LActions.Menu, INyxLCLMenu, LMenu), 'menu uses actual native Nyx renderer');
    LMenu.Presentation.Renderer.Reveal(AID, niRuntime);
    LButton := TButton(LMenu.Presentation.Renderer.FocusFor(AID));
    Check((LButton <> nil) and LButton.Enabled, 'ordinary cell command is mounted and enabled');
    LButton.Click;
    Drain;
  end;

  procedure Inspector(const AID: TNyxText);
  begin
    Check(Supports(LActions.Inspector, INyxLCLPopover, LInspector) and LInspector.IsOpen,
      'inspection uses actual native Nyx popover');
    LInspector.Renderer.Reveal(AID, niRuntime);
    TButton(LInspector.Renderer.FocusFor(AID)).Click;
    Drain;
  end;

  procedure CloseInspection;
  begin
    Inspector(NyxCellInspectorCloseID);
    LInspector := nil;
    LActions.Close;
    Drain;
  end;

  procedure ColumnVisibility(AAction: TNyxColumnArrangementAction);
  var
    LTarget: INyxLCLColumnArrangementPresenter;
    LID: TNyxText;
    LButton: TButton;
  begin
    LManager.Open;
    Drain;
    Check(Supports(LManager.Presenter, INyxLCLColumnArrangementPresenter, LTarget),
      'Columns uses actual native Nyx presenter');
    LID := NyxColumnArrangementActionID(NyxColumnArrangementEditorID,
      LManager.Observation, NyxColumnIdentity(NyxTextField('command')), AAction);
    LTarget.Presentation.Renderer.Reveal(LID, niRuntime);
    LButton := TButton(LTarget.Presentation.Renderer.ControlFor(LID));
    Check((LButton <> nil) and LButton.Enabled, 'Columns action is physically mounted and enabled');
    LButton.Click;
    Drain;
    { Successful arrangement replaces its presenter. The entered command's
      original native observation is retired; close the current managed form. }
    Check(LManager.Presenter <> nil, 'arrangement retains its current presenter after the command');
    LManager.Presenter.Close;
    Drain;
  end;

  procedure SaveExport(const ACommand: TNyxText; const ALeaf: String;
    AMembership: TNyxCollectionExportRows; const AExpected: TNyxText; ACancel: Boolean = False);
  var
    LStarted: QWord;
    LPreviousActions: Integer;
    LPath: String;
  begin
    LPath := LDirectory + PathDelim + ALeaf;
    Check(not FileExists(LPath), 'export destination is a fresh owned path');
    LDialog.Path := UTF8Decode(LPath);
    LDialog.Cancel := ACancel;
    LDialog.Started := GetTickCount64;
    LDialog.Error := '';
    LPreviousActions := LDialog.Actions;
    LTimer.Enabled := True;
    Context('Process ID');
    Menu(ACommand);
    LStarted := GetTickCount64;
    repeat
      Drain;

      if GetTickCount64 - LStarted > 20000 then
      begin
        raise Exception.Create('Real generated export did not finish before its deadline');
      end;
      Sleep(1);
    until LExports.ExportCommands.Phase = ncepCompleted;
    LTimer.Enabled := False;
    Check((LDialog.Error = '') and (LDialog.Actions = LPreviousActions + 1),
      'the actual process-owned Save dialog received exactly one terminal choice');

    if ACancel then
    begin
      Check((LExports.ExportCommands.LastResult.Outcome = nfsoCancelled) and not FileExists(LPath),
        'real Save cancellation creates no output file and claims no write');
    end
    else
    begin
      Check(LExports.ExportCommands.LastResult.Outcome = nfsoWritten,
        'configured native host confirms a completed disk write');
      Check(LExports.ExportCommands.LastPayload.Membership = AMembership,
        'terminal export retains its explicit captured scope');
      VerifyDisk(LPath, AExpected);
    end;
    CloseInspection;
  end;

begin
  GChecks := 0;

  if (ParamCount <> 2) or not TryStrToInt(ParamStr(1), LWidth) or
    ((LWidth <> 1280) and (LWidth <> 390)) then
  begin
    raise Exception.Create('Supply width1280/390 and a fresh evidence directory');
  end;
  LDirectory := ExpandFileName(ParamStr(2));
  Check(not DirectoryExists(LDirectory), 'runtime qualification uses a fresh evidence directory');
  Check(ForceDirectories(LDirectory), 'fresh runtime evidence directory is available');
  GFailure := TFailureObserver.Create;
  LPreviousException := Application.OnException;
  LDialog := TSaveDialogDriver.Create;
  LTimer := TTimer.Create(nil);
  LTimer.Enabled := False;
  LTimer.Interval := 50;
  LTimer.OnTimer := LDialog.Tick;
  Application.OnException := GFailure.Failed;
  try
    LBefore := TNyxCodec.Encode(ADocument);
    AApplication.Window.ClientWidth := LWidth;
    AApplication.Window.ClientHeight := 760;
    AApplication.Window.Show;
    AApplication.Window.BringToFront;
    Drain;
    Check(AApplication.Window.ClientWidth = LWidth, 'real generated window has the requested allocation');
    LMount := AApplication.View.CollectionMount('service-table');
    LRevision := LMount.View.Store.Snapshot.Revision;
    Check((LMount.View.Store.Snapshot.Count = 4) and
      Supports(LMount, INyxCollectionColumnArrangementMount, LColumns) and
      Supports(LMount, INyxCollectionCellActionsMount, LActionsMount) and
      Supports(LMount, INyxCollectionExportMount, LExports),
      'generated setup mounts all real table services over its four accepted records');
    LManager := LColumns.ColumnArrangement;
    LActions := LActionsMount.CellActions;
    ColumnVisibility(ncaaShow);
    Context('Command line');
    Menu(NyxCellActionsInspectID);
    Check(Supports(LActions.Inspector, INyxLCLPopover, LInspector), 'long value is mounted in the ordinary inspector');
    Check(NyxLCLInputText(TCustomEdit(LInspector.Renderer.FocusFor(NyxCellInspectorValueID))) = CGatewayCommand,
      'native inspector retains every character of the accepted command');
    Inspector(NyxCellInspectorCopyID);
    Check((LActions.LastResult.Outcome = ncwoWritten) and ClipboardMatches(CGatewayCommand),
      'generated clipboard host writes the exact gateway command to Windows');
    LInspector.Window.BringToFront;
    SaveNyxNativeCapture(LInspector.Window, TNyxText(LDirectory + '/copy.png'), ncmDisplay);
    CloseInspection;
    Context('Command line', 4);
    Menu(NyxCellActionsInspectID);
    Inspector(NyxCellInspectorCopyID);
    Check((LActions.LastResult.Outcome = ncwoWritten) and ClipboardMatches(CReportsCommand),
      'a different native Copy proves a fresh system clipboard write');
    CloseInspection;
    ColumnVisibility(ncaaHide);
    LMount.View.ConfigureQuery(NyxCollectionQuery.Where(NyxWhere(NyxTextField('status')).EqualTo('Running')));
    Check(Supports(LMount.View, INyxCollectionPages, LPages), 'generated runtime exposes typed paging');
    LPages.ConfigurePaging(NyxCollectionPaging.PageSize(1));
    Drain;
    Check((LMount.View.Snapshot.Count = 1) and (LMount.View.Snapshot.ItemAt(0).Ref.ID = 'gateway'),
      'one-row runtime page is smaller than its two-row filtered result');
    SaveExport(NyxCellActionsExportPageID, 'page.tsv', ncerCurrentPage,
      CHeaders + #10 + CGatewayRow);
    SaveExport(NyxCellActionsExportFilteredID, 'filtered.tsv', ncerAllFiltered,
      CHeaders + #10 + CGatewayRow + #10 + CReportsRow);
    SaveExport(NyxCellActionsExportFilteredID, 'cancelled.tsv', ncerAllFiltered, '', True);
    VerifyDisk(LDirectory + '/page.tsv', CHeaders + #10 + CGatewayRow);
    VerifyDisk(LDirectory + '/filtered.tsv', CHeaders + #10 + CGatewayRow + #10 + CReportsRow);
    Check((TNyxCodec.Encode(ADocument) = LBefore) and
      (LMount.View.Store.Snapshot.Revision = LRevision) and (LMount.View.Store.Snapshot.Count = 4),
      'real host effects preserve the exact design and accepted runtime records');
    AApplication.Window.BringToFront;
    SaveNyxNativeCapture(AApplication.Window, TNyxText(LDirectory + '/exports.png'), ncmDisplay);
    WriteLn('PASS ', GChecks, ' instrumented generated native table output checks at ', LWidth);
  finally
    LTimer.Enabled := False;
    LTimer.Free;
    LDialog.Free;
    LInspector := nil;
    LActions := nil;
    LActionsMount := nil;
    LExports := nil;
    LPages := nil;
    LManager := nil;
    LColumns := nil;
    LMount := nil;
    Application.OnException := LPreviousException;
    GFailure.Free;
    GFailure := nil;
    Application.Terminate;
  end;
end;

end.
