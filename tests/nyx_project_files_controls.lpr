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

program nyx_project_files_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses Interfaces, Classes, SysUtils, Forms, Controls, StdCtrls, Graphics,
  IntfGraphics, FPWritePNG, nyx.text, nyx.files, nyx.files.lcl,
  nyx.studio.files, nyx.studio.lcl, nyx.studio.projects, nyx.studio.projectstore;

type
  TControlAccess = class(TControl);
  { Controlled dialog delivery, actual native byte adapters and ordinary widget
    callbacks. This qualifies controller integration, not physical OS dialog input. }
  TFileHost = class(TInterfacedObject, INyxTextFileExchange)
  public
    Reply: TNyxTextFileReply;
    Selection: TNyxTextFileSelection;
    Exported: TNyxTextFiles;
    Directory: TNyxText;
    Cancelled: Boolean;
    Picks: Integer;
    procedure Pick(const ASelection: TNyxTextFileSelection; AReply: TNyxTextFileReply);
    function ExportFiles(const AFiles: TNyxTextFiles): Boolean;
    procedure Cancel;
    procedure Deliver(const AFiles: TNyxTextFiles);
  end;
  TStudio = class(TNyxNativeStudio)
  protected
    function CreateProjectFiles: INyxTextFileExchange; override;
  end;
  TObserver = class
  public
    Error: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
  end;

var
  GFiles: TFileHost;
  GLease: INyxTextFileExchange;
  GStudio: TStudio;
  GForm: TForm;
  GObserver: TObserver;
  GChecks: Integer;
  GDirectory: TNyxText;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Native project files: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure TObserver.Failed(ASender: TObject; AException: Exception);
begin
  Error := AException.Message;
end;

procedure Pump;
var
  LStart: QWord;
begin
  LStart := GetTickCount64;
  repeat
    Application.ProcessMessages;

    if GObserver.Error <> '' then
    begin
      raise Exception.Create(GObserver.Error);
    end;

    if GetTickCount64 - LStart > 30000 then
    begin
      raise Exception.Create('Native project file presentation did not retire');
    end;
    Sleep(1);
  until not GStudio.PresentationPending and not GStudio.SourceBusy;
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
  Check(LControl <> nil, 'Ordinary Nyx action exists / ' + AID);
  Check(LControl.IsVisible, 'Ordinary action is in the visible native presentation / ' + AID);
  TControlAccess(LControl).Click;
  Pump;
end;

procedure TFileHost.Cancel;
begin
  Reply := nil;
end;

procedure TFileHost.Pick(const ASelection: TNyxTextFileSelection; AReply: TNyxTextFileReply);
begin
  ASelection.Validate;
  Selection := ASelection;
  Reply := AReply;
  Inc(Picks);
end;

function TFileHost.ExportFiles(const AFiles: TNyxTextFiles): Boolean;
var
  LIndex: Integer;
begin
  ValidateNyxTextFiles(AFiles, NyxTextFileSelection.UpToFiles(2));
  Exported := AFiles;

  if Cancelled then
  begin
    Exit(False);
  end;
  for LIndex := 0 to High(AFiles) do
  begin
    WriteNyxTextFile(Directory + AFiles[LIndex].Name, AFiles[LIndex]);
  end;
  Result := True;
end;

procedure TFileHost.Deliver(const AFiles: TNyxTextFiles);
var
  LReply: TNyxTextFileReply;
begin
  LReply := Reply;
  Reply := nil;
  Check(Assigned(LReply), 'Public file exchange captured the ordinary import callback');
  ValidateNyxTextFiles(AFiles, Selection);
  LReply(fpsSelected, AFiles, '');
  Pump;
end;

function TStudio.CreateProjectFiles: INyxTextFileExchange;
begin
  Result := GLease;
end;

procedure Capture(const AName: TNyxText; AForm: TForm);
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
begin
  LBitmap := TBitmap.Create;
  LImage := nil;
  LWriter := nil;
  try
    LBitmap.SetSize(AForm.ClientWidth, AForm.ClientHeight);
    AForm.PaintTo(LBitmap.Canvas, 0, 0);
    LImage := LBitmap.CreateIntfImage;
    LWriter := TFPWriterPNG.Create;
    LImage.SaveToFile(GDirectory + AName + '.png', LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
end;

procedure Run(const ASeed: TNyxText);
const
  CTextFileName: TNyxText = 'notes-🌙.txt';
  CTextFileValue: TNyxText = 'Exact supplementary text / 😀' + #0 + #10;
var
  LFiles: TNyxTextFiles;
  LSeed: TNyxProjectPair;
  LSeedPacket: TNyxText;
  LBefore: TNyxText;
  LSaved: TNyxText;
  LBad: TNyxProjectPair;
  LCallback: TNyxTextFileReply;
  LStore: TNyxProjectStore;
  LRevision: TNyxText;
  LSource: TMemo;
  LPaints: Integer;
begin
  LFiles := nil;
  SetLength(LFiles, 1);
  WriteNyxTextFile(GFiles.Directory + CTextFileName,
    NyxTextFile(CTextFileName, CTextFileValue));
  Check(ReadNyxTextFile(GFiles.Directory + CTextFileName).Text = CTextFileValue,
    'Native UTF-8 filename/byte boundaries retain supplementary text and NUL');
  LFiles[0] := ReadNyxTextFile(IncludeTrailingPathDelimiter(ASeed) + 'project.nyxproject');
  LSeedPacket := ReadNyxStudioProjectFiles(LFiles);
  LSeed := DecodeNyxProject(LSeedPacket);
  GStudio.Run;
  Pump;
  LBefore := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
  Click('action-import');
  LPaints := GStudio.PaintCount;
  Click('action-project-export');
  Check((GStudio.PaintCount = LPaints) and
    (ReadNyxTextFile(GFiles.Directory + 'project.nyxproject').Text = LBefore),
    'Pure backup export writes the complete pair without replacing mounted views');
  GFiles.Cancelled := True;
  Click('action-project-export');
  GFiles.Cancelled := False;
  Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBefore,
    'Host cancellation never changes the current document or draft');
  Click('action-project-import');
  Check((GFiles.Picks = 1) and Assigned(GFiles.Reply),
    'The formerly inert native import action now invokes the public file exchange');
  GFiles.Deliver(LFiles);
  Check((GStudio.Session.Document.Count = 2) and
    (GStudio.Session.Document.ComponentCount = 1) and
    (EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LSeedPacket),
    'Ordinary file callback admits the exact MCP-authored multipage reusable project');
  Check(Pos('function NotebookHint', GStudio.Session.Source) > 0,
    'Crafted Pascal helper survives ordinary native import');
  Click('view-page-1');
  Click('view-component-0');
  Check((GStudio.Session.ActiveViewID = 'note-card') and
    (GStudio.CanvasView.Root.ID = 'note-card') and
    (GStudio.CanvasView.ControlFor('note-card') <> nil),
    'Reusable root has its own mounted native design canvas after import');
  Click('view-page-0');
  Click('action-project-export-files');
  Check((ReadNyxTextFile(GFiles.Directory + 'design.nyx').Text = LSeed.Design) and
    (ReadNyxTextFile(GFiles.Directory + GFiles.Exported[1].Name).Text = LSeed.Source),
    'Actual native adjacent files retain the complete crafted companion');
  Click('action-code');
  LSource := TMemo(GStudio.CodeView.InputFor('studio-code'));
  LSource.Text := 'An unfinished notebook idea.';
  Pump;
  LSaved := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
  Click('action-project-export');
  Check(ReadNyxTextFile(GFiles.Directory + 'project.nyxproject').Text = LSaved,
    'Backup includes actual memo input and its original paired draft baseline');
  TEdit(GStudio.ShellView.InputFor('project-file-name')).Text := 'saved-notebook';
  Pump;
  Click('action-project-save');
  LStore := TNyxProjectStore.Create(GDirectory + 'projects');
  try
    Check(LStore.ReadProject('saved-notebook', LRevision) = LSaved,
      'Ordinary named Save publishes complete accepted files and unfinished input');
  finally
    LStore.Free;
  end;
  Click('action-project-open');
  Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LSaved,
    'Ordinary saved Open restores the exact draft and companion');

  LBad := LSeed;
  LBad.Source := 'Unsupported Pascal kept for the author.';
  LFiles := NyxStudioProjectBackup(LBad);
  Click('action-project-import');
  GFiles.Deliver(LFiles);
  Check((GStudio.ShellView.Root.Find('project-import-warning') <> nil) and
    (EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LSaved),
    'Divergent files retain the current pair and expose explicit conflict choices');
  Click('action-project-input-backup');
  Check(ReadNyxTextFile(GFiles.Directory + 'imported-project.nyxproject').Text =
    EncodeNyxProject(LBad), 'Input export preserves the complete divergent packet');
  Capture('native-files-conflict', GForm);
  Click('action-project-use-design');
  Check(GStudio.Session.SourceDraftPending and (GStudio.Session.DraftSource = LBad.Source),
    'Explicit design choice retains unsupported Pascal as an unfinished draft');
  LBad := LSeed;
  LBad.Source := StringReplace(LSeed.Source, 'Keep a good idea', 'A revised notebook', [rfReplaceAll]);

  if LBad.Source = LSeed.Source then
  begin
    LBad.Source := StringReplace(LSeed.Source, 'Keep an idea', 'A revised notebook', [rfReplaceAll]);
  end;
  LFiles := NyxStudioProjectBackup(LBad);
  Click('action-project-import');
  GFiles.Deliver(LFiles);
  Click('action-project-use-pascal');
  Check((GStudio.Session.Source = LBad.Source) and not GStudio.Session.SourceDraftPending,
    'Explicit Pascal choice admits exact changed values and retains the handcrafted helper');
  Click('action-project-export-files');
  GForm.ClientWidth := 390;
  Pump;
  Click('action-panel-design');
  Click('action-expand-source');
  Check(GStudio.SourceModal.IsOpen, 'Compact native project source opens in the public modal');
  Capture('native-files-expanded', TForm(GStudio.SourceModal.Control));
  Click('action-expand-source');
  GForm.ClientWidth := 1280;
  Pump;
  if GStudio.ShellView.Root.Find('action-panel-project') <> nil then
  begin
    Click('action-panel-project');
  end;
  Click('action-project-import');
  LCallback := GFiles.Reply;
  GStudio.LoadProject(LSeed);
  Pump;
  LCallback(fpsSelected, LFiles, '');
  Pump;
  Check((EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LSeedPacket) and
    (GStudio.ShellView.Root.Find('project-import-warning') = nil),
    'Even a late old callback cannot publish into a new same-named project');
  Capture('native-files-desktop', GForm);
end;

begin
  GStudio := nil;
  GForm := nil;
  GObserver := nil;
  GLease := nil;
  try
    try

      if (ParamCount <> 2) or DirectoryExists(ParamStr(1)) or FileExists(ParamStr(1)) then
      begin
        raise Exception.Create('Supply a new owned evidence directory and semantic seed directory');
      end;
      GDirectory := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(1)));
      ForceDirectories(GDirectory);
      ForceDirectories(GDirectory + 'exported');
      GFiles := TFileHost.Create;
      GFiles.Directory := GDirectory + 'exported' + PathDelim;
      GLease := GFiles;
      GObserver := TObserver.Create;
      Application.Initialize;
      Application.OnException := GObserver.Failed;
      GForm := TForm.CreateNew(nil);
      GForm.SetBounds(30, 30, 1280, 900);
      GForm.Show;
      GStudio := TStudio.Create(GForm, GDirectory + 'projects');
      Run(ParamStr(2));
      WriteLn('PASS ', GChecks, ' actual native project file controls');
    except
      on LException: Exception do
      begin
        WriteLn('FAIL ', LException.Message);
        DumpExceptionBackTrace(Output);
        ExitCode := 1;
      end;
    end;
  finally
    GStudio.Free;
    GForm.Free;
    GObserver.Free;
    GLease := nil;
    Application.OnException := nil;
    Application.ProcessMessages;
  end;
end.
