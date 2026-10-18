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
unit nyx.pascal.files.project;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.pascal.files, nyx.pascal.sources, nyx.pascal.drafts;

{ Capture current project/editing membership through its existing immutable owner.
  Logical main/unit filenames append .pas; includes keep their admitted leaf.
  SDK files may subsequently be explicitly added without entering source admission.
  This bridge keeps project/code-generation dependencies outside the analysis core.
  Caller serializes alternate source providers during this one-time freeze. }
function CaptureNyxPascalProjectFiles(const ASources: INyxPascalSourceSnapshot;
  ACase: TNyxPascalFileCase): INyxPascalFileCatalog;
{ Map a project selector to a logical file, retaining main/unit/include ownership.
  This mapping grants no filesystem, compiler or editor mutation capability. }
function NyxPascalProjectPhysicalFile(const ASources: INyxPascalSourceSnapshot;
  const AFile: TNyxPascalSourceRef): TNyxPascalPhysicalFile;

implementation

uses nyx.pascal.context;

function NyxPascalProjectPhysicalFile(const ASources: INyxPascalSourceSnapshot;
  const AFile: TNyxPascalSourceRef): TNyxPascalPhysicalFile;
begin

  if ASources = nil then
  begin
    raise ENyxPascalContext.Create('A source selector needs its captured project');
  end;
  AFile.Validate;
  { Mapping names never manufactures membership for an unrelated selector. }
  ASources.Text(AFile);

  if AFile.Role = npsMain then
  begin
    Result := NyxPascalPhysicalFile(ASources.MainUnit.Name + '.pas');
  end
  else if AFile.Role = npsUnit then
  begin
    Result := NyxPascalPhysicalFile(AFile.Name + '.pas');
  end
  else
  begin
    Result := NyxPascalPhysicalFile(AFile.Name);
  end;
end;

function CaptureNyxPascalProjectFiles(const ASources: INyxPascalSourceSnapshot;
  ACase: TNyxPascalFileCase): INyxPascalFileCatalog;
var
  LSource: INyxPascalSourceSnapshot;
  LAt: Integer;
  LFile: TNyxPascalSourceRef;
begin
  LSource := CaptureNyxPascalSources(ASources);
  Result := NewNyxPascalFileCatalog(ACase).WithIncludeDirectory('');
  for LAt := 0 to LSource.Count - 1 do
  begin
    LFile := LSource.FileReference(LAt);
    Result := Result.WithFile(NyxPascalProjectPhysicalFile(LSource, LFile), LSource.Text(LFile));
  end;
end;

end.
