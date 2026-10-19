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

uses nyx.pascal.files, nyx.pascal.sources, nyx.pascal.drafts, nyx.pascal.names,
  nyx.scheduler;

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
{ One atomic current-project overlay. It clones the captured seed once, keeps
  unchanged SDK bytes/directories and retains per-buffer immutable text owners.
  Existing quotas and optional current work/cancellation apply throughout. }
function WithNyxPascalProjectFiles(const AFiles: INyxPascalFileCatalog;
  const ASources: INyxPascalSourceSnapshot;
  const ACancellation: INyxCancellationScope = nil): INyxPascalFileCatalog;
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

type
  { Construction-only adapter prepares each requested buffer once. The result
    catalog keeps exact bytes/views, never this source provider or work scope. }
  TProjectPackets = class(TInterfacedObject, INyxPascalFilePackets)
  public
    Sources: INyxPascalSourceSnapshot;
    Cancellation: INyxCancellationScope;
    Single: Boolean;
    Selected: TNyxPascalSourceRef;
    function Count: Integer;
    function Packet(AIndex: Integer): INyxPascalFilePacket;
  end;

function TProjectPackets.Count: Integer;
begin

  if Single then Result := 1 else Result := Sources.Count;
end;

function TProjectPackets.Packet(AIndex: Integer): INyxPascalFilePacket;
var
  LFile: TNyxPascalSourceRef;
  LView: INyxPascalSourceView;
begin

  if (AIndex < 0) or (AIndex >= Count) then
    raise ENyxPascalContext.Create('A project packet index is outside its captured source');

  if Single then LFile := Selected else LFile := Sources.FileReference(AIndex);
  LView := NyxPascalSourceViewFromLexicalInput(NyxPascalSourcesLexicalInput(Sources, LFile), Cancellation);
  Result := NewNyxPascalFilePacket(NyxPascalProjectPhysicalFile(Sources, LFile),
    LView.Original, LView, Cancellation);
end;

function OverlayProject(const AFiles: INyxPascalFileCatalog;
  const ASources: INyxPascalSourceSnapshot; ASingle: Boolean;
  const ASelected: TNyxPascalSourceRef;
  const ACancellation: INyxCancellationScope): INyxPascalFileCatalog;
var
  LSource: INyxPascalSourceSnapshot;
  LBase: INyxPascalFileCatalog;
  LExecution: INyxCancellationScope;
  LReader: TProjectPackets;
  LPackets: INyxPascalFilePackets;
  LSelected: TNyxPascalSourceRef;
begin
  LSelected := ASelected;
  LBase := AFiles;
  LSource := ASources;
  LExecution := ACancellation;

  if (LExecution <> nil) and LExecution.Cancelled then
    raise ENyxPascalAnalysisCancelled.Create('Project source preparation cancelled');
  LSource := CaptureNyxPascalSources(LSource);
  LReader := TProjectPackets.Create;
  LPackets := LReader;
  LReader.Sources := LSource;
  LReader.Cancellation := LExecution;
  LReader.Single := ASingle;
  LReader.Selected := LSelected;
  Result := OverlayNyxPascalFilePackets(LBase, LPackets, LExecution);
end;

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
begin
  Result := WithNyxPascalProjectFiles(NewNyxPascalFileCatalog(ACase)
    .WithIncludeDirectory(''), ASources);
end;

function WithNyxPascalProjectFile(const AFiles: INyxPascalFileCatalog;
  const ASources: INyxPascalSourceSnapshot; const AFile: TNyxPascalSourceRef): INyxPascalFileCatalog;
begin
  Result := OverlayProject(AFiles, ASources, True, AFile, nil);
end;

function WithNyxPascalProjectFiles(const AFiles: INyxPascalFileCatalog;
  const ASources: INyxPascalSourceSnapshot;
  const ACancellation: INyxCancellationScope): INyxPascalFileCatalog;
begin
  Result := OverlayProject(AFiles, ASources, False, TNyxPascalSourceRef.Main, ACancellation);
end;

end.
