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

program nyx_source_compilation_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses Interfaces, SysUtils, Classes, Forms, Controls, StdCtrls, ExtCtrls, LCLType,
  Graphics, IntfGraphics, FPWritePNG, nyx.text, nyx.bytes, nyx.data, nyx.model,
  nyx.studio.lcl, nyx.split.lcl, nyx.studio.directories,
  nyx.studio.projects, nyx.studio.projectstore,
  nyx.studio.sourceconfiguration, nyx.studio.sourceconfiguration.native;

type
  TControlAccess = class(TControl);
  TSplitGripAccess = class(TNyxLCLSplitGrip);
  TObserver = class
  public
    Failure: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
  end;

var
  GForm: TForm;
  GStudio: TNyxNativeStudio;
  GObserver: TObserver;
  GChecks: Integer;

procedure TObserver.Failed(ASender: TObject; AException: Exception);
begin
  Failure := AException.Message;
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Compiled source controls: ' + AReason);
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

    if (LFile.Size < 1) or (LFile.Size > 4 * 1024 * 1024) then
    begin
      raise Exception.Create('Owned qualification input requires bounded bytes');
    end;
    SetLength(LBytes, LFile.Size);
    LFile.ReadBuffer(LBytes[0], Length(LBytes));
    Result := NyxDecodeUTF8(LBytes);
  finally
    LFile.Free;
  end;
end;

procedure Pump;
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    Application.ProcessMessages;

    if GObserver.Failure <> '' then
    begin
      raise Exception.Create(GObserver.Failure);
    end;

    if not GStudio.SourceBusy and not GStudio.PresentationPending then
    begin
      Exit;
    end;
    Sleep(1);
  until GetTickCount64 - LStarted > 180000;
  raise Exception.Create('The live native source/presentation command exceeded its budget');
end;

procedure Click(const AID: TNyxText);
var
  LControl: TControl;
begin

  if GStudio.ShellView.Root.Find(AID) <> nil then
  begin
    LControl := GStudio.ShellView.ControlFor(AID);
  end
  else
  begin
    LControl := GStudio.SourceView.ControlFor(AID);
  end;
  Check(LControl <> nil, 'real mounted action ' + AID);
  TControlAccess(LControl).Click;
  Pump;
end;

procedure Capture(const APath: TNyxText);
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
    GForm.PaintTo(LBitmap.Canvas.Handle, 0, 0);
    LImage := LBitmap.CreateIntfImage;
    LWriter := TFPWriterPNG.Create;
    LImage.SaveToFile(APath, LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
end;

procedure EditSetting(const AID, AValue: TNyxText);
var
  LInput: TEdit;
begin
  LInput := TEdit(GStudio.ShellView.InputFor(AID));
  Check(LInput <> nil, 'actual local source setting ' + AID);
  LInput.Text := AValue;
  Pump;
end;

procedure CaptureSettings(const AName: TNyxText);
var
  LSplit: TNyxLCLSplitView;
  LScroll: TScrollBox;
  LKey: Word;
begin
  { Exercise the existing physical keyboard resize grip, then reveal the public
    settings card in its actual scroll host. No descriptor or design is mutated
    to manufacture a screenshot. End uses the admitted split maximum. }
  LSplit := TNyxLCLSplitView(GStudio.ShellView.ControlFor('studio-details-split'));
  Check(LSplit <> nil, 'settings use the public resizable details workspace');
  LKey := VK_END;
  TSplitGripAccess(LSplit.Grip).KeyDown(LKey, []);
  Pump;
  LScroll := TScrollBox(GStudio.ShellView.ControlFor('studio-details'));
  Check(LScroll <> nil, 'settings use the real native scroll host');
  LScroll.ScrollInView(GStudio.ShellView.ControlFor('studio-local-source-settings'));
  Pump;
  Capture(IncludeTrailingPathDelimiter(ParamStr(4)) + AName);
end;

var
  LDirectories: TNyxStudioDirectories;
  LTools: TNyxDataValue;
  LSettings: TNyxLocalSourceSettings;
  LSettingsPath: TNyxText;
  LSettingsPacket: TNyxText;
  LUnicodeSettings: TNyxLocalSourceSettings;
  LStream: TFileStream;
  LBytes: TNyxBytes;
  LSource: TNyxText;
  LExpected: TNyxText;
  LBeforeSource: TNyxText;
  LBeforeDesign: TNyxText;
  LInput: TMemo;
  LPair: TNyxProjectPair;
  LStore: TNyxProjectStore;
  LRevision: TNyxText;
  LSaved: TNyxText;
  LRefused: Boolean;
begin
  try

    if (ParamCount <> 4) or DirectoryExists(ParamStr(4)) or FileExists(ParamStr(4)) then
    begin
      raise Exception.Create('Supply repository, toolchain, compiled fixture home and NEW evidence home');
    end;
    ForceDirectories(ParamStr(4));
    LDirectories := TNyxStudioDirectories.ForRepository(ParamStr(1))
      .RunningIn(IncludeTrailingPathDelimiter(ParamStr(4)) + 'runtime');
    LTools := TNyxDataValue.ParseJSON(ReadText(ParamStr(2)));
    LSource := ReadText(IncludeTrailingPathDelimiter(ParamStr(3)) + 'web/source.pas');
    LExpected := ReadText(IncludeTrailingPathDelimiter(ParamStr(3)) + 'web/expected.nyx');
    Application.Initialize;
    GObserver := TObserver.Create;
    Application.OnException := GObserver.Failed;
    GForm := TForm.CreateNew(nil);
    GForm.SetBounds(30, 30, 1280, 900);
    GForm.Show;
    GStudio := TNyxNativeStudio.Create(GForm, LDirectories.Projects);
    GStudio.Run;
    Pump;
    LBeforeSource := GStudio.Session.Source;
    LBeforeDesign := GStudio.Session.Save;
    Check(not GStudio.LocalSourceEnabled and not GStudio.SourceCommands.ProjectCompilerAvailable,
      'ordinary launch needs no compiler strategy');
    Click('action-outputs');
    Check(GStudio.ShellView.Root.Find(NyxLocalSourceLibraryID) <> nil,
      'source settings are available before selecting application output');
    Check(GStudio.ShellView.Root.Find('output-none').Prop('variant') = 'primary',
      'source configuration does not require an output target');
    EditSetting(NyxLocalSourceLibraryID, LDirectories.SourceRoot);
    EditSetting(NyxLocalSourceRuntimeID, LDirectories.RuntimeRoot);
    EditSetting(NyxLocalSourceCompilerID, LTools.Field('FPC').AsText);
    Click(NyxLocalSourceConfigureID);
    Check(GStudio.LocalSourceEnabled and GStudio.SourceCommands.ProjectCompilerAvailable,
      'actual configuration action enables Apply and Open');
    LSettings := GStudio.LocalSourceSettings;
    LSettingsPath := IncludeTrailingPathDelimiter(LDirectories.Projects) +
      '.local' + PathDelim + 'source-settings.json';
    LSettingsPacket := ReadNyxLocalSourceSettings(LSettingsPath);
    Check(TNyxLocalSourceSettings.Decode(LSettingsPacket).Encode = LSettings.Encode,
      'exact typed machine hints persist independently');
    LUnicodeSettings := LSettings.WithLibrary('A rocket 🚀, a letter 𐐷, and é.');
    Check(TNyxLocalSourceSettings.Decode(LUnicodeSettings.Encode).LibraryRoot =
      LUnicodeSettings.LibraryRoot, 'machine hints preserve exact supplementary Unicode');
    Check((GStudio.Session.Source = LBeforeSource) and (GStudio.Session.Save = LBeforeDesign) and
      not GStudio.Session.CanUndo and not GStudio.Session.CanRedo,
      'configuration preserves initial pair/history');
    EditSetting(NyxLocalSourceCompilerID, LSettings.CompilerPath + '.missing');
    Click(NyxLocalSourceConfigureID);

    if Pos('does not exist', GStudio.ShellView.Root.Find('source-settings-message').Prop('text')) = 0 then
    begin
      WriteLn('Visible configuration status: ', GStudio.Status);
      WriteLn('Visible configuration message: ',
        GStudio.ShellView.Root.Find('source-settings-message').Prop('text'));
      Capture(IncludeTrailingPathDelimiter(ParamStr(4)) + 'configuration-refusal-failed.png');
    end;
    Check(GStudio.LocalSourceEnabled and (Pos('does not exist', GStudio.Status) > 0) and
      (Pos('does not exist', GStudio.ShellView.Root.Find('source-settings-message').Prop('text')) > 0) and
      (ReadNyxLocalSourceSettings(LSettingsPath) = LSettingsPacket),
      'failed physical configuration retains old strategy and exact saved hints');
    EditSetting(NyxLocalSourceCompilerID, LSettings.CompilerPath);
    EditSetting(NyxLocalSourceRuntimeID, LSettings.RuntimeRoot + '-unpublished');
    LStream := TFileStream.Create(LSettingsPath + '.lock', fmOpenReadWrite or fmShareExclusive);
    try
      Click(NyxLocalSourceConfigureID);
      Check(GStudio.LocalSourceEnabled and
        (ReadNyxLocalSourceSettings(LSettingsPath) = LSettingsPacket) and
        (GStudio.Session.Source = LBeforeSource) and (GStudio.Session.Save = LBeforeDesign),
        'another settings writer refuses publication without altering compiler or pair');
    finally
      LStream.Free;
    end;
    EditSetting(NyxLocalSourceRuntimeID, LSettings.RuntimeRoot);
    Click(NyxLocalSourceConfigureID);
    Check((GStudio.LocalSourceSettings.Encode = LSettings.Encode) and
      (Pos('changed paths', GStudio.ShellView.Root.Find('source-settings-status').Prop('text')) = 0),
      'successful retry publishes canonical settings and clears pending-path presentation');
    Check(GStudio.ShellView.Root.Find('action-save-outputs') = nil,
      'offline source settings do not advertise unavailable service profile actions');
    CaptureSettings('source-configuration.png');
    Click('action-outputs');
    Click('action-code');
    LInput := TMemo(GStudio.CodeView.InputFor('studio-code'));
    Check(LInput <> nil, 'ordinary native Pascal editor is mounted');
    LInput.Text := LSource;
    Pump;
    Check(GStudio.Session.DraftSource = LSource, 'physical memo captures exact complete Pascal');
    TControlAccess(GStudio.SourceView.ControlFor('action-apply-source')).Click;
    Check(GStudio.SourceBusy, 'actual Apply has a live immutable source request');
    LRefused := False;
    try
      GStudio.ConfigureLocalSource(LSettings);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (ReadNyxLocalSourceSettings(LSettingsPath) = LSettingsPacket),
      'busy configuration refuses before changing machine settings');
    LRefused := False;
    try
      GStudio.DisableLocalSource;
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and GStudio.LocalSourceEnabled,
      'busy disable cannot retire an active source producer');
    Pump;
    Check((GStudio.Session.Source = LSource) and (GStudio.Session.Save = LExpected) and
      not GStudio.Session.SourceDraftPending, 'real Apply compiles/evaluates helpers and loops');
    Check((GStudio.Session.ActiveViewID = 'notebook-1') and
      (GStudio.CanvasView.Root <> nil), 'compiled root replacement mounts its actual native view');
    Check(GStudio.Session.CanUndo and not GStudio.Session.CanRedo,
      'physical Apply owns one paired Undo');
    Capture(IncludeTrailingPathDelimiter(ParamStr(4)) + 'compiled-apply.png');
    Click('action-undo');
    Check((GStudio.Session.Source = LBeforeSource) and (GStudio.Session.Save = LBeforeDesign),
      'real Undo restores the complete initial pair');
    Click('action-redo');
    Check((GStudio.Session.Source = LSource) and (GStudio.Session.Save = LExpected),
      'real Redo restores the exact compiled pair');
    Check(GStudio.CodeView.InputFor('studio-code') = LInput,
      'compiler publication/history retains the actual source input');
    { The same real Studio now opens a saved whole-Pascal pair. Its accepted
      source is executable; its unfinished draft is intentionally invalid Pascal
      and must remain editable data. This is physical widget/controller evidence,
      not an OS file-dialog or browser qualification. }
    LPair := GStudio.Session.ProjectSnapshot;
    LPair.Pending := True;
    LPair.Draft := 'An unfinished notebook idea.';
    LPair.DraftBase := LPair.Source;
    GStudio.LoadProject(LPair);
    Check(GStudio.SourceBusy, 'saved-project checking remains observable in real Studio');
    LRefused := False;
    try
      GStudio.LoadProject(NyxProjectPair(LBeforeDesign, LBeforeSource));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'a second Open cannot replace the retained pending import');
    Pump;
    Check((GStudio.Session.Source = LSource) and (GStudio.Session.Save = LExpected) and
      (GStudio.Session.DraftSource = LPair.Draft), 'configured native Open carries unfinished text through real compilation');
    Check(not GStudio.Session.CanUndo and not GStudio.Session.CanRedo,
      'actual Studio Open resets project history');
    Check(TMemo(GStudio.CodeView.InputFor('studio-code')).Text = LPair.Draft,
      'real retained source input shows the recovered unfinished buffer');
    Click('action-import');
    TEdit(GStudio.ShellView.InputFor('project-file-name')).Text := 'compiled-notebook';
    Pump;
    Click('action-project-save');
    LSaved := EncodeNyxProject(LPair);
    LStore := TNyxProjectStore.Create(LDirectories.Projects);
    try
      Check(LStore.ReadProject('compiled-notebook', LRevision) = LSaved,
        'actual Save writes the admitted complete companion and unfinished buffer');
    finally
      LStore.Free;
    end;
    TControlAccess(GStudio.ShellView.ControlFor('action-project-open')).Click;
    Check(GStudio.SourceBusy, 'named Open starts actual compiler checking');
    TControlAccess(GStudio.ShellView.ControlFor('action-project-open')).Click;
    Check(Pos('Wait for pending editor changes', GStudio.Status) > 0,
      'a second named Open refuses before changing retained file metadata');
    Pump;
    Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LSaved,
      'real named Open recompiles and restores exact saved files/draft');
    Check(GStudio.ShellView.Root.Find('project-import-warning') = nil,
      'successful compiler opening retires its input conflict');
    Capture(IncludeTrailingPathDelimiter(ParamStr(4)) + 'compiled-project-reopened.png');
    Click('action-outputs');
    LBeforeSource := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
    Click(NyxLocalSourceDisableID);
    Check(not GStudio.LocalSourceEnabled and not GStudio.SourceCommands.ProjectCompilerAvailable,
      'physical disable restores compiler-independent source admission');
    Check((EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBeforeSource) and
      (ReadNyxLocalSourceSettings(LSettingsPath) = LSettingsPacket),
      'disable preserves complete pair/draft and persisted paths');
    Click(NyxLocalSourceConfigureID);
    Check(GStudio.LocalSourceEnabled and
      (EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBeforeSource),
      'physical re-enable preserves complete pending project');
    { An independent editor shares only the machine hints file. Its startup
      remains compiler-independent. A changed settings file refuses installation
      in the first editor without replacing that editor's accepted compiler. }
    SaveNyxLocalSourceSettings(LSettingsPath, LSettings.WithRuntime(
      IncludeTrailingPathDelimiter(LDirectories.RuntimeRoot) + 'another-workspace'),
      True, LSettingsPacket);
    Click(NyxLocalSourceConfigureID);
    Check(GStudio.LocalSourceEnabled and (Pos('another editor', GStudio.Status) > 0) and
      (EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBeforeSource),
      'conflicting settings publication preserves current compiler and exact project');
    SaveNyxLocalSourceSettings(LSettingsPath, LSettings, True,
      ReadNyxLocalSourceSettings(LSettingsPath));
    FreeAndNil(GStudio);
    GStudio := TNyxNativeStudio.Create(GForm, LDirectories.Projects);
    GStudio.Run;
    Pump;
    Check(not GStudio.LocalSourceEnabled and not GStudio.SourceCommands.ProjectCompilerAvailable and
      (GStudio.LocalSourceSettings.Encode = LSettings.Encode),
      'relaunch loads exact hints without compiler authority or tool prerequisite');
    Click('action-outputs');
    CaptureSettings('source-hints-reopened.png');
    FreeAndNil(GStudio);
    { Deliberately damage only this freshly owned machine file. The same ordinary
      startup must preserve design access and visibly explain invalid hints. }
    LBytes := NyxEncodeUTF8('{"version":9}');
    LStream := TFileStream.Create(LSettingsPath, fmCreate or fmShareExclusive);
    try
      LStream.WriteBuffer(LBytes[0], Length(LBytes));
    finally
      LStream.Free;
    end;
    GStudio := TNyxNativeStudio.Create(GForm, LDirectories.Projects);
    GStudio.Run;
    Pump;
    Click('action-outputs');
    Check(not GStudio.LocalSourceEnabled and
      (Pos('could not be loaded', GStudio.ShellView.Root.Find('source-settings-message').Prop('text')) > 0),
      'malformed saved hints leave the ordinary uncompiled editor usable');
    CaptureSettings('source-hints-invalid.png');
    WriteLn('PASS ', GChecks, ' actual native compiler source controls');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
  GStudio.Free;
  GForm.Free;
  GObserver.Free;
  Application.OnException := nil;
  Application.ProcessMessages;
end.
