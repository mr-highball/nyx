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

unit nyx.studio.files;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.files, nyx.studio.projects;

{ Portable file meanings shared by both ordinary Studio controllers. Invalid
  membership refuses; complete imported bytes remain available for explicit
  project conflict review. Admission is still the project/session boundary. }
function NyxStudioProjectFiles: TNyxTextFileSelection;
function ReadNyxStudioProjectFiles(const AFiles: TNyxTextFiles): TNyxText;
function NyxStudioProjectBackup(const APair: TNyxProjectPair): TNyxTextFiles;
{ Adjacent files contain accepted values; the backup separately retains drafts. }
function NyxStudioProjectCompanion(const APair: TNyxProjectPair): TNyxTextFiles;

implementation

uses SysUtils, nyx.source;

const
  CProjectExtension: TNyxText = 'nyxproject';
  CDesignExtension: TNyxText = 'nyx';
  CPascalExtension: TNyxText = 'pas';

function NyxStudioProjectFiles: TNyxTextFileSelection;
begin
  Result := NyxTextFileSelection.Allow(CProjectExtension).Allow(CDesignExtension)
    .Allow(CPascalExtension).UpToFiles(2);
end;

function ReadNyxStudioProjectFiles(const AFiles: TNyxTextFiles): TNyxText;
var
  LIndex: Integer;
  LExtension: TNyxText;
  LDesign: INyxTextFile;
  LPascal: INyxTextFile;
begin
  ValidateNyxTextFiles(AFiles, NyxStudioProjectFiles);
  LDesign := nil;
  LPascal := nil;

  if Length(AFiles) = 1 then
  begin

    if LowerCase(ExtractFileExt(AFiles[0].Name)) <> '.' + CProjectExtension then
    begin
      raise ENyxFile.Create('Select a project backup, or both design and Pascal files');
    end;
    Exit(AFiles[0].Text);
  end;
  for LIndex := 0 to High(AFiles) do
  begin
    LExtension := LowerCase(ExtractFileExt(AFiles[LIndex].Name));

    if (LExtension = '.' + CDesignExtension) and (LDesign = nil) then
    begin
      LDesign := AFiles[LIndex];
    end
    else if (LExtension = '.' + CPascalExtension) and (LPascal = nil) then
    begin
      LPascal := AFiles[LIndex];
    end
    else
    begin
      raise ENyxFile.Create('Select one design and its Pascal companion together');
    end;
  end;

  if (LDesign = nil) or (LPascal = nil) then
  begin
    raise ENyxFile.Create('A paired project needs both design and Pascal files');
  end;
  Result := EncodeNyxProject(NyxProjectPair(LDesign.Text, LPascal.Text));
end;

function NyxStudioProjectBackup(const APair: TNyxProjectPair): TNyxTextFiles;
begin
  Result := nil;
  SetLength(Result, 1);
  Result[0] := NyxTextFile('project.nyxproject', EncodeNyxProject(APair));
end;

function NyxStudioProjectCompanion(const APair: TNyxProjectPair): TNyxTextFiles;
begin
  Result := nil;
  SetLength(Result, 2);
  Result[0] := NyxTextFile('design.nyx', APair.Design);
  Result[1] := NyxTextFile(NyxCompanionUnitName(APair.Source) + '.pas', APair.Source);
end;

end.
