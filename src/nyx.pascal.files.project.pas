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

uses nyx.pascal.files, nyx.pascal.sources, nyx.pascal.drafts, nyx.pascal.names;

{ Capture current project/editing membership through its existing immutable owner.
  Main metadata gains .pas; auxiliary unit/include selectors already own leaves.
  SDK files may subsequently be explicitly added without entering source admission.
  This bridge keeps project/code-generation dependencies outside the analysis core.
  Caller serializes alternate source providers during this one-time freeze. }
function CaptureNyxPascalProjectFiles(const ASources: INyxPascalSourceSnapshot;
  ACase: TNyxPascalFileCase): INyxPascalFileCatalog;
{ Overlay the current buffer as canonical authoring UTF-8, preserving its exact
  immutable text owner for physical lexical reuse. Accepted membership and all
  byte/view validation remain with the existing source/catalog owners. }
function WithNyxPascalProjectFile(const AFiles: INyxPascalFileCatalog;
  const ASources: INyxPascalSourceSnapshot; const AFile: TNyxPascalSourceRef): INyxPascalFileCatalog;
{ Map a project selector to a logical file, retaining main/unit/include ownership.
  This mapping grants no filesystem, compiler or editor mutation capability. }
function NyxPascalProjectPhysicalFile(const ASources: INyxPascalSourceSnapshot;
  const AFile: TNyxPascalSourceRef): TNyxPascalPhysicalFile;
{ Resolve a main/unit selector to its typed namespace, removing only the unit
  selector's validated final .pas suffix. Includes have no independent namespace
  and refuse; callers must provide their explicit main/unit owner instead. }
function NyxPascalProjectUnit(const ASources: INyxPascalSourceSnapshot;
  const AFile: TNyxPascalSourceRef): TNyxPascalUnitRef;

implementation

uses SysUtils, nyx.pascal.context, nyx.pascal.lexing, nyx.pascal.encoding;

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
  else
  begin
    Result := NyxPascalPhysicalFile(AFile.Name);
  end;
end;

function NyxPascalProjectUnit(const ASources: INyxPascalSourceSnapshot;
  const AFile: TNyxPascalSourceRef): TNyxPascalUnitRef;
begin

  if ASources = nil then
  begin
    raise ENyxPascalContext.Create('A source owner needs its captured project');
  end;
  AFile.Validate;
  ASources.Text(AFile);
  case AFile.Role of
    npsMain:
      begin
        Result := ASources.MainUnit;
      end;
    npsUnit:
      begin
        Result := NyxPascalUnit(Copy(AFile.Name, 1, Length(AFile.Name) - 4));
      end;
    npsInclude:
      begin
        raise ENyxPascalContext.Create('An include needs an explicit main/unit namespace owner');
      end;
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
    Result := WithNyxPascalProjectFile(Result, LSource, LFile);
  end;
end;

function WithNyxPascalProjectFile(const AFiles: INyxPascalFileCatalog;
  const ASources: INyxPascalSourceSnapshot; const AFile: TNyxPascalSourceRef): INyxPascalFileCatalog;
var
  LSource: INyxPascalSourceSnapshot;
  LInput: INyxPascalLexicalInput;
  LFiles: INyxPascalFileCatalog;
  LBytes: INyxPascalByteFileCatalog;
  LView: INyxPascalSourceView;
begin
  LSource := ASources;
  LInput := NyxPascalSourcesLexicalInput(LSource, AFile);

  if LInput = nil then
  begin
    LSource := CaptureNyxPascalSources(LSource);
    LInput := NyxPascalSourcesLexicalInput(LSource, AFile);
  end;
  LFiles := CaptureNyxPascalFiles(AFiles);
  Supports(LFiles, INyxPascalByteFileCatalog, LBytes);
  LView := NyxPascalSourceViewFromLexicalInput(LInput);
  Result := LBytes.WithByteSource(NyxPascalProjectPhysicalFile(LSource, AFile), LView.Original, LView);
end;

end.
