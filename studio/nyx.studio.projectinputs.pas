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
unit nyx.studio.projectinputs;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.pascal.project, nyx.studio.sourceprojection;

type
  { Detached projection attachment for exact supporting-file admission. This is
    local producer data, never a wire execution flag. The trusted compiler host
    binds it to its result; editor admission compares the entire inventory as
    well as the ordinary exact source/design/state checks. }
  INyxPascalProjectProjection = interface(INyxSourceProjection)
    ['{68FE918B-457C-45BE-B2E0-CA2300000005}']
    function GetFiles: TNyxPascalProjectFiles;
    property Files: TNyxPascalProjectFiles read GetFiles;
  end;

  { In-process compiler receipt retaining the exact frozen main/unit/include
    snapshot. The ordinary projection still distinguishes compilation/execution.
    This facet has no wire representation and grants no editor publication,
    import/recovery or application-launch authority. Retaining it borrows no
    executor, mutable provider, project, process or native filesystem handle. }
  INyxPascalProjectBuild = interface(INyxSourceProjectionBuild)
    ['{68FE918B-457C-45BE-B2E0-CA2300000003}']
    function GetInputs: INyxPascalProject;
    property Inputs: INyxPascalProject read GetInputs;
  end;

{ Trusted producer/receiver bridge. Bind only inputs captured for this exact
  compiler job and an already bound projection. A caller-supplied file packet or
  execution flag cannot establish that provenance. Capture freezes providers;
  mismatch refuses before allocating the detached wrapper. This supplies no
  executor-local application-build facet or editor publication authority. }
function BindNyxProjectSourceProjection(const AProjection: INyxSourceProjection;
  const AProject: INyxPascalProject): INyxSourceProjection;
{ Same trusted binding for a detached private service receipt. The returned
  value owns metadata only and deliberately lacks the executor-local storage/
  application-build facet. No process, transport or editor is borrowed. }
function BindNyxProjectSourceBuild(const ABuild: INyxSourceProjectionBuild;
  const AProject: INyxPascalProject): INyxPascalProjectBuild;
implementation

uses SysUtils, nyx.text, nyx.model, nyx.studio.builds, nyx.studio.compiler;

type
  { Immutable projection delegate binds the captured supporting-file inventory
    to the actual constructor's detached result. It owns both values and borrows
    no executor, document, provider, native handle or enclosing build wrapper. }
  TProjectProjection = class(TInterfacedObject, INyxSourceProjection,
    INyxPascalProjectProjection)
  private
    FProjection: INyxSourceProjection;
    FInputs: INyxPascalProject;
  public
    function GetSource: TNyxText;
    function GetTarget: TNyxBuildTarget;
    function GetState: TNyxSourceProjectionState;
    function GetMessage: TNyxText;
    function GetDesign: TNyxText;
    function GetReport: INyxCompilerReport;
    function CopyDocument: TNyxDocument;
    function GetFiles: TNyxPascalProjectFiles;
  end;

  { Portable receipt delegate retains an authenticated producer result and its
    exact locally captured inputs. This wrapper cannot mint executor-local build
    provenance; its optional project facet describes immutable source data. }
  TProjectSourceBuild = class(TInterfacedObject, INyxSourceProjectionBuild,
    INyxPascalProjectBuild)
  private
    FBuild: INyxSourceProjectionBuild;
    FProjection: INyxSourceProjection;
    FInputs: INyxPascalProject;
  public
    function GetReference: TNyxSourceProjectionRef;
    function GetProjection: INyxSourceProjection;
    function GetArtifact: TNyxText;
    function GetRuntimeLog: TNyxText;
    function GetInputs: INyxPascalProject;
  end;

function TProjectProjection.GetSource: TNyxText;
begin
  Result := FProjection.Source;
end;

function TProjectProjection.GetTarget: TNyxBuildTarget;
begin
  Result := FProjection.Target;
end;

function TProjectProjection.GetState: TNyxSourceProjectionState;
begin
  Result := FProjection.State;
end;

function TProjectProjection.GetMessage: TNyxText;
begin
  Result := FProjection.Message;
end;

function TProjectProjection.GetDesign: TNyxText;
begin
  Result := FProjection.Design;
end;

function TProjectProjection.GetReport: INyxCompilerReport;
begin
  Result := FProjection.Report;
end;

function TProjectProjection.CopyDocument: TNyxDocument;
begin
  Result := FProjection.CopyDocument;
end;

function TProjectProjection.GetFiles: TNyxPascalProjectFiles;
begin
  { Saving observes the portable packet budget; a larger compiler-only project
    remains valid without acquiring editor/import authority or losing files. }
  Result := TNyxPascalProjectFiles.Capture(FInputs);
end;

function BindNyxProjectSourceProjection(const AProjection: INyxSourceProjection;
  const AProject: INyxPascalProject): INyxSourceProjection;
var
  LInputs: INyxPascalProject;
  LProjection: TProjectProjection;
begin
  LInputs := CaptureNyxPascalProject(AProject);

  if (AProjection = nil) or (AProjection.Source <> LInputs.Source) then
  begin
    raise ENyxModel.Create('Project projection requires its exact captured main source');
  end;
  LProjection := TProjectProjection.Create;
  Result := LProjection;
  LProjection.FInputs := LInputs;
  LProjection.FProjection := AProjection;
end;

function TProjectSourceBuild.GetReference: TNyxSourceProjectionRef;
begin
  Result := FBuild.Reference;
end;

function TProjectSourceBuild.GetProjection: INyxSourceProjection;
begin
  Result := FProjection;
end;

function TProjectSourceBuild.GetArtifact: TNyxText;
begin
  Result := FBuild.Artifact;
end;

function TProjectSourceBuild.GetRuntimeLog: TNyxText;
begin
  Result := FBuild.RuntimeLog;
end;

function TProjectSourceBuild.GetInputs: INyxPascalProject;
begin
  Result := FInputs;
end;

function BindNyxProjectSourceBuild(const ABuild: INyxSourceProjectionBuild;
  const AProject: INyxPascalProject): INyxPascalProjectBuild;
var
  LInputs: INyxPascalProject;
  LProjection: INyxSourceProjection;
  LBuild: TProjectSourceBuild;
begin

  if ABuild = nil then
  begin
    raise ENyxModel.Create('Project source binding needs a detached compiler result');
  end;
  LInputs := CaptureNyxPascalProject(AProject);
  LProjection := BindNyxProjectSourceProjection(ABuild.Projection, LInputs);
  LBuild := TProjectSourceBuild.Create;
  Result := LBuild;
  LBuild.FBuild := ABuild;
  LBuild.FProjection := LProjection;
  LBuild.FInputs := LInputs;
end;

end.
