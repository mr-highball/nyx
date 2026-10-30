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

program nyx_studio_named_project_browser;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Web, nyx.text, nyx.files, nyx.studio.projects,
  nyx.studio.browser, nyx.studio.sourcecompilation.shared.browser;

type
  { This regression captures only Studio's automatic pre-open text backup.
    Named project persistence, source admission, HTTP and compilation stay on
    production paths. The Studio is borrowed until Cancel; no picker callback
    or file/DOM handle is retained. This does not prove a physical disk backup. }
  TBackupCapture = class(TInterfacedObject, INyxTextFileExchange)
  private
    FStudio: TNyxStudio;
    FCount: Integer;
  public
    constructor Create(AStudio: TNyxStudio);
    procedure Pick(const ASelection: TNyxTextFileSelection; AReply: TNyxTextFileReply);
    function ExportFiles(const AFiles: TNyxTextFiles): Boolean;
    procedure Cancel;
  end;

  { Ordinary Studio with one explicit public file-host substitution. All project
    operations, source queues, authoring controls and agent synchronization use
    their normal implementations. Browser local recovery is disabled by Run so
    an owned test workspace cannot overwrite the user's browser recovery data. }
  TNamedProjectStudio = class(TNyxStudio)
  protected
    function CreateProjectFiles: INyxTextFileExchange; override;
  end;

var
  GStudio: TNamedProjectStudio;

constructor TBackupCapture.Create(AStudio: TNyxStudio);
begin
  inherited Create;
  FStudio := AStudio;
end;

procedure TBackupCapture.Pick(const ASelection: TNyxTextFileSelection;
  AReply: TNyxTextFileReply);
begin
  raise ENyxFile.Create('This regression opens named server projects through the ordinary Project panel');
end;

function TBackupCapture.ExportFiles(const AFiles: TNyxTextFiles): Boolean;
var
  LExpected: TNyxText;
begin

  if FStudio = nil then
  begin
    raise ENyxFile.Create('The backup capture has retired');
  end;
  ValidateNyxTextFiles(AFiles, NyxTextFileSelection.UpToFiles(1));

  if (Length(AFiles) <> 1) or (AFiles[0].Name <> 'project-before-open.nyxproject') then
  begin
    raise ENyxFile.Create('Only the automatic pre-open backup is captured by this regression');
  end;
  LExpected := EncodeNyxProject(FStudio.ProjectSnapshot);

  if AFiles[0].Text <> LExpected then
  begin
    raise ENyxFile.Create('The automatic backup differs from the exact current project pair');
  end;
  Inc(FCount);
  document.body.setAttribute('data-authoring-backup-count', IntToStr(FCount));
  document.body.setAttribute('data-authoring-backup-bytes', IntToStr(AFiles[0].ByteCount));
  document.body.setAttribute('data-authoring-backup-exact', 'true');
  Result := True;
end;

procedure TBackupCapture.Cancel;
begin
  FStudio := nil;
end;

function TNamedProjectStudio.CreateProjectFiles: INyxTextFileExchange;
begin
  Result := TBackupCapture.Create(Self);
end;

begin
  GStudio := TNamedProjectStudio.Create(nil, NewNyxSharedBrowserSourceCompilerFactory);
  GStudio.Run(False);
  GStudio.ConnectAgents;
end.