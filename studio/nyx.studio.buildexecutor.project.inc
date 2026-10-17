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
  Result := FBuild.Projection;
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
  LResult := TProjectBuild.Create;
  LResult.FInputs := AInputs;
  LResult.FBuild := ABuild;
  LResult.FStorage := LStorage;
  Result := LResult;
end;
