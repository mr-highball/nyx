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

  { Private local producer facet. Only executor-created values carry this
    attachment; a public/wire projection or an alternate metadata wrapper cannot
    silently attach different helper meaning to an earlier construction. }
  INyxCapturedProjectBuild = interface(IInterface)
    ['{68FE918B-457C-45BE-B2E0-CA2300000004}']
    function GetCapturedBuild: INyxSourceProjectionBuild;
    function GetInputs: INyxPascalProject;
  end;

  { One immutable attachment around the invocation's existing detached result.
    Both input and build interfaces own their data and contain no parent borrow.
    Storage statistics forward the actual native host's optional evidence. }
  TProjectBuild = class(TInterfacedObject, INyxSourceProjectionBuild,
    INyxPascalProjectBuild, INyxProjectionStorageBuild, INyxCapturedProjectBuild)
  private
    FInputs: INyxPascalProject;
    FBuild: INyxSourceProjectionBuild;
    FStorage: INyxProjectionStorageBuild;
    FProjection: INyxSourceProjection;
  public
    function GetInputs: INyxPascalProject;
    function GetCapturedBuild: INyxSourceProjectionBuild;
    function GetReference: TNyxSourceProjectionRef;
    function GetProjection: INyxSourceProjection;
    function GetArtifact: TNyxText;
    function GetRuntimeLog: TNyxText;
    function GetStorage: TNyxProjectionStorageReport;
  end;

function TProjectBuild.GetInputs: INyxPascalProject;
begin
  Result := FInputs;
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

function TProjectBuild.GetCapturedBuild: INyxSourceProjectionBuild;
begin
  Result := FBuild;
end;

function TProjectBuild.GetReference: TNyxSourceProjectionRef;
begin
  Result := FBuild.Reference;
end;

function TProjectBuild.GetProjection: INyxSourceProjection;
begin
  Result := FProjection;
end;

function TProjectBuild.GetArtifact: TNyxText;
begin
  Result := FBuild.Artifact;
end;

function TProjectBuild.GetRuntimeLog: TNyxText;
begin
  Result := FBuild.RuntimeLog;
end;

function TProjectBuild.GetStorage: TNyxProjectionStorageReport;
begin
  Result := FStorage.Storage;
end;

function NewNyxPascalProjectBuild(const AInputs: INyxPascalProject;
  const ABuild: INyxSourceProjectionBuild): INyxPascalProjectBuild;
var
  LResult: TProjectBuild;
  LStorage: INyxProjectionStorageBuild;
  LProjection: TProjectProjection;
  LOwnedProjection: INyxSourceProjection;
begin

  if (AInputs = nil) or (ABuild = nil) or (ABuild.Projection = nil) or
    (ABuild.Projection.Source <> AInputs.Source) then
  begin
    raise ENyxModel.Create('A project compiler receipt requires its exact captured main source');
  end;

  if not Supports(ABuild, INyxProjectionStorageBuild, LStorage) then
  begin
    raise ENyxModel.Create('A project compiler receipt requires actual invocation storage evidence');
  end;
  LProjection := TProjectProjection.Create;
  LOwnedProjection := LProjection;
  LProjection.FProjection := ABuild.Projection;
  LProjection.FInputs := AInputs;
  { Attach the interface before any remaining allocation so both owners retire
    together on failure. All borrowed/provider validation has already completed. }
  LResult := TProjectBuild.Create;
  Result := LResult;
  LResult.FProjection := LOwnedProjection;
  LResult.FInputs := AInputs;
  LResult.FBuild := ABuild;
  LResult.FStorage := LStorage;
end;
