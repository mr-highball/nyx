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

program nyx_file_runtime_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses Interfaces, Forms, Controls, ExtCtrls, Windows, Classes, SysUtils,
  nyx.text, nyx.bytes, nyx.data, nyx.types, nyx.model, nyx.codec, nyx.state,
  nyx.flow, nyx.flow.types, nyx.logic, nyx.application.lcl, nyx.files.save,
  nyx.files.save.lcl, nyx.widgets.lcl, nyx.test.files.runtime,
  nyx.generated.view;

type
  { Closed owned-dialog actions. Retirement changes the application page, so
    its real view generation must cancel the pending save before publication. }
  TDialogAction = (ndaSelect, ndaCancel, ndaRetirePage);
  { Ordinary creator resolver composed with the explicit file layer. It owns no
    application; calls prove that non-file operations still reach creator code. }
  TCreatorResolver = class(TInterfacedObject, INyxLogicResolver)
  public
    Calls: Integer;
    function Resolve(const AKind: TNyxLogicKindRef;
      const AConfiguration: nyx.data.TNyxDataValue): INyxLogicOperation;
  end;
  { UI-thread test receiver. Its timer addresses only this process's visible
    save dialog; it is disabled before the application or receiver is freed. }
  TDialogDriver = class
  public
    App: TNyxLCLApplication;
    Directory: String;
    Mode: TDialogAction;
    Actions: Integer;
    procedure Tick(ASender: TObject);
  end;

var
  GDriver: TDialogDriver;
  GDialog: HWND;
  GFilename: HWND;
  GChecks: Integer;

function TCreatorResolver.Resolve(const AKind: TNyxLogicKindRef;
  const AConfiguration: nyx.data.TNyxDataValue): INyxLogicOperation;
begin
  Inc(Calls);
  Result := NyxLogicFromData(AKind, AConfiguration);
end;

procedure Check(ACondition: Boolean; const AMessage: String);
begin

  if not ACondition then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

function FindDialog(AWindow: HWND; AData: LPARAM): BOOL; stdcall;
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

  if UnicodeString(LTitle) = 'Save a Nyx report' then
  begin
    GDialog := AWindow;
    Result := False;
  end;
end;

function FindFilename(AWindow: HWND; AData: LPARAM): BOOL; stdcall;
var
  LClass: array[0..63] of WideChar;
  LID: Integer;
begin
  Result := True;
  LID := GetDlgCtrlID(AWindow);
  GetClassNameW(AWindow, LClass, Length(LClass));

  if (UnicodeString(LClass) = 'Edit') and ((LID = 1001) or (LID = 1148)) then
  begin
    GFilename := AWindow;
    Result := False;
  end;
end;

procedure TDialogDriver.Tick(ASender: TObject);
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

  if Mode = ndaRetirePage then
  begin
    App.ShowPage('other');
    Inc(Actions);
    Exit;
  end;

  if Mode = ndaCancel then
  begin
    PostMessageW(GDialog, WM_COMMAND, IDCANCEL, 0);
    Inc(Actions);
    Exit;
  end;
  GFilename := 0;
  EnumChildWindows(GDialog, @FindFilename, 0);

  if (GFilename = 0) or not IsWindowEnabled(GFilename) then
  begin
    Exit;
  end;
  LPath := UTF8Decode(Directory + PathDelim + String(RuntimeReportLeaf));

  if SendMessageTimeoutW(GFilename, WM_SETTEXT, 0, LPARAM(PWideChar(LPath)),
    SMTO_ABORTIFHUNG, 1000, @LReply) = 0 then
  begin
    Exit;
  end;
  PostMessageW(GDialog, WM_COMMAND, IDOK, 0);
  Inc(Actions);
end;

procedure Pump(AApp: TNyxLCLApplication; AWaitOutcome: Boolean);
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    CheckSynchronize;
    Application.ProcessMessages;

    if AApp.FlowRoutes.Report(0).Field('failure').AsText <> '' then
    begin
      raise Exception.Create(String(AApp.FlowRoutes.Report(0).Field('failure').AsText));
    end;

    if AWaitOutcome and (AApp.State.GetValue(NyxIntegerState('save-outcome')) >= 0) then
    begin
      Exit;
    end;

    if not AWaitOutcome and (GetTickCount64 - LStarted > 250) and
      (AApp.FlowRoutes.Report(0).Field('active').AsInteger = 0) then
    begin
      Exit;
    end;

    if GetTickCount64 - LStarted > 20000 then
    begin
      raise Exception.Create('Ordinary file application exceeded its deadline');
    end;
    Sleep(1);
  until False;
end;

procedure VerifyFile(const ADirectory: String);
var
  LStream: TFileStream;
  LExpected: TNyxBytes;
  LActual: TNyxBytes;
  LIndex: Integer;
begin
  LExpected := NyxEncodeUTF8(NewNyxJSONFormat(njlIndented, 2).Format(
    nyx.data.TNyxDataValue.ParseJSON(RuntimeReportJSON)));
  LStream := TFileStream.Create(ADirectory + PathDelim + String(RuntimeReportLeaf), fmOpenRead);
  try
    SetLength(LActual, LStream.Size);

    if Length(LActual) > 0 then
    begin
      LStream.ReadBuffer(LActual[0], Length(LActual));
    end;
  finally
    LStream.Free;
  end;
  Check(Length(LActual) = Length(LExpected), 'actual application writes the exact UTF-8 length');
  for LIndex := 0 to High(LExpected) do
  begin

    if LActual[LIndex] <> LExpected[LIndex] then
    begin
      raise Exception.Create('Actual application disk bytes differ');
    end;
  end;
  Check(True, 'actual application disk payload is independently exact');
end;

var
  LDoc: TNyxDocument;
  LApp: TNyxLCLApplication;
  LTimer: TTimer;
  LResolver: TCreatorResolver;
  LResolverLease: INyxLogicResolver;
  LBefore: TNyxText;
  LDirectory: String;
  LPass: Integer;
  LRefused: Boolean;
  LStarted: QWord;
begin
  LDoc := nil;
  LApp := nil;
  LTimer := nil;
  GDriver := TDialogDriver.Create;
  try

    if ParamCount <> 1 then
    begin
      raise Exception.Create('Supply a fresh owned output directory');
    end;
    LDirectory := ExpandFileName(ParamStr(1));

    if DirectoryExists(LDirectory) then
    begin
      raise Exception.Create('Application controls require a fresh output directory');
    end;
    ForceDirectories(LDirectory);
    Application.Initialize;
    LDoc := BuildNyxDocument;
    LBefore := TNyxCodec.Encode(LDoc);
    LApp := TNyxLCLApplication.Create;
    LApp.ConfigureFileSaving(NewNyxLCLFileSaveHost).ConfigureFileSaving(nil);
    LApp.Mount(LDoc);
    LApp.Window.Show;
    TNyxLCLButton(LApp.View.ControlFor('save-report')).Click;
    LStarted := GetTickCount64;
    repeat
      CheckSynchronize;
      Application.ProcessMessages;

      if GetTickCount64 - LStarted > 3000 then
      begin
        raise Exception.Create('Unconfigured file route did not report its missing host');
      end;
      Sleep(1);
    until LApp.FlowRoutes.Report(0).Field('failure').AsText <> '';
    Check(LApp.State.GetValue(NyxIntegerState('save-outcome')) = -1,
      'explicitly disabled authority refuses a saved route before publishing outputs');
    Check(LApp.State.GetValue(NyxTextState('pretty')) = '',
      'missing file authority never publishes earlier partial graph results');
    Check(TNyxCodec.Encode(LDoc) = LBefore, 'missing authority never edits the design');
    FreeAndNil(LApp);
    for LPass := 0 to 1 do
    begin
      LApp := TNyxLCLApplication.Create;
      LResolver := TCreatorResolver.Create;
      LResolverLease := LResolver;
      GDriver.Directory := LDirectory + PathDelim + IntToStr(LPass);
      ForceDirectories(GDriver.Directory);

      if LPass = 0 then
      begin
        LApp.ConfigureFileSaving(NewNyxLCLFileSaveHost(TNyxText(GDriver.Directory)))
          .ConfigureLogic(LResolverLease);
      end
      else
      begin
        LApp.ConfigureLogic(LResolverLease)
          .ConfigureFileSaving(NewNyxLCLFileSaveHost(TNyxText(GDriver.Directory)));
      end;
      LApp.Mount(LDoc);
      LApp.Window.Show;
      GDriver.App := LApp;
      GDriver.Mode := ndaSelect;
      LTimer := TTimer.Create(nil);
      LTimer.Interval := 50;
      LTimer.OnTimer := GDriver.Tick;
      LRefused := False;
      try
        LApp.ConfigureFileSaving(nil);
      except
        on ENyxModel do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused, 'mounted applications refuse authority changes without replacing their host');
      TNyxLCLButton(LApp.View.ControlFor('save-report')).Click;
      Pump(LApp, True);
      Check(LApp.State.GetValue(NyxIntegerState('save-outcome')) = Ord(nfsoWritten),
        'ordinary saved route receives the explicitly configured real file host');
      Check(LApp.State.GetValue(NyxTextState('saved-name')) = RuntimeReportLeaf,
        'the selected leaf publishes through ordinary application state');
      Check(LResolver.Calls = 3, 'creator resolver receives non-file operations in either configuration order');
      VerifyFile(GDriver.Directory);
      LApp.State.SetValue(NyxIntegerState('save-outcome'), -1);
      GDriver.Mode := ndaCancel;
      TNyxLCLButton(LApp.View.ControlFor('save-report')).Click;
      Pump(LApp, True);
      Check(LApp.State.GetValue(NyxIntegerState('save-outcome')) = Ord(nfsoCancelled),
        'real user cancellation publishes its saved typed outcome');
      VerifyFile(GDriver.Directory);
      LApp.State.SetValue(NyxIntegerState('save-outcome'), -1);
      GDriver.Mode := ndaRetirePage;
      TNyxLCLButton(LApp.View.ControlFor('save-report')).Click;
      Pump(LApp, False);
      Check(LApp.State.GetValue(NyxIntegerState('save-outcome')) = -1,
        'page retirement cancels the open picker without late state publication');
      VerifyFile(GDriver.Directory);
      Check(TNyxCodec.Encode(LDoc) = LBefore, 'runtime authority and outcomes preserve the exact design');
      FreeAndNil(LTimer);
      GDriver.App := nil;
      FreeAndNil(LApp);
      LResolverLease := nil;
    end;
    WriteLn('PASS ', GChecks, ' ordinary compiled native file application checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      ExitCode := 1;
    end;
  end;
  LTimer.Free;
  LApp.Free;
  GDriver.Free;
  LDoc.Free;
end.
