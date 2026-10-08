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

unit nyx.studio.directories;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text;

type
  { Repository mode retains the existing development layout. Release mode uses
    verified immutable payload sources/web and disjoint host-owned writable roots.
    These are local host choices, never portable design properties or MCP inputs. }
  TNyxStudioDirectoryMode = (nsdmRepository, nsdmRelease);

  { Immutable value configuration. Factories normalize paths and fluent methods
    return new copies; workers retain a value, never a mutable host/config object.
    No factory creates directories, writes enrollment or starts a listener.
    A default/uninitialized record refuses Validate. }
  TNyxStudioDirectories = record
  private
    FMode: TNyxStudioDirectoryMode;
    FSourceRoot: TNyxText;
    FRuntimeRoot: TNyxText;
    FEnrollmentRoot: TNyxText;
    function GetCompilerUnits: TNyxText;
    function GetWebRoot: TNyxText;
    function GetJobs: TNyxText;
    function GetProjects: TNyxText;
    function GetOutputProfile: TNyxText;
    function GetPreviews: TNyxText;
    function GetSessionCheckpoint: TNyxText;
  public
    { Backward-compatible development layout, including repositories created by
      local test hosts. Filesystem creation belongs to the actual host consumer. }
    class function ForRepository(const ARepository: TNyxText): TNyxStudioDirectories; static;
    { Verify the pristine release manifest once, then admit a disjoint writable
      runtime root with ordinary existing ancestors. Missing output compilers do
      not participate in this admission and never prevent authoring. }
    class function ForRelease(const ARelease, ARuntime: TNyxText): TNyxStudioDirectories; static;
    { Explicit enrollment project may differ from runtime storage. Its .codex and
      .local write locations must not overlap payload sources. In particular, a
      containing development repository is allowed when those locations are outside
      its build/release subtree. Returns a copy; prior jobs retain their roots. }
    function EnrollingProject(const AProject: TNyxText): TNyxStudioDirectories;
    { Copied host configuration separates development sources from private job,
      project and recovery storage too. No paths are created. Release callers
      retain their stricter disjoint-root admission. }
    function RunningIn(const ARuntime: TNyxText): TNyxStudioDirectories;
    { Recheck role separation and ordinary release/runtime/enrollment ancestors
      before a host creates files. Does not rehash a release or change any path. }
    procedure Validate;
    property Mode: TNyxStudioDirectoryMode read FMode;
    property SourceRoot: TNyxText read FSourceRoot;
    property RuntimeRoot: TNyxText read FRuntimeRoot;
    property EnrollmentRoot: TNyxText read FEnrollmentRoot;
    property CompilerUnits: TNyxText read GetCompilerUnits;
    property WebRoot: TNyxText read GetWebRoot;
    property Jobs: TNyxText read GetJobs;
    property Projects: TNyxText read GetProjects;
    property OutputProfile: TNyxText read GetOutputProfile;
    property Previews: TNyxText read GetPreviews;
    { Native private session/history recovery; never an exported design member. }
    property SessionCheckpoint: TNyxText read GetSessionCheckpoint;
  end;

implementation

uses
  nyx.studio.release;

function NormalizeDirectory(const APath: TNyxText): TNyxText;
begin

  if Trim(APath) = '' then
  begin
    raise ENyxStudioRelease.Create('A Studio host directory must be explicit');
  end;
  Result := IncludeTrailingPathDelimiter(ExpandFileName(APath));
end;

function ContainsDirectory(const AParent, AChild: TNyxText): Boolean;
var
  LParent: TNyxText;
  LChild: TNyxText;
begin
  LParent := AParent;
  LChild := AChild;
  {$IFDEF MSWINDOWS}
  { Host path comparison follows Windows case-insensitive directory identity.
    The trailing delimiter prevents sibling prefixes such as release/release-old
    from being confused. This does not claim Unicode filesystem normalization. }
  LParent := UTF8Encode(UnicodeUpperCase(UTF8Decode(LParent)));
  LChild := UTF8Encode(UnicodeUpperCase(UTF8Decode(LChild)));
  {$ENDIF}
  Result := Copy(LChild, 1, Length(LParent)) = LParent;
end;

procedure RequireDisjoint(const ASource, AWritable: TNyxText);
begin

  if ContainsDirectory(ASource, AWritable) or ContainsDirectory(AWritable, ASource) then
  begin
    raise ENyxStudioRelease.Create('Release sources and writable host locations must be disjoint');
  end;
end;

class function TNyxStudioDirectories.ForRepository(
  const ARepository: TNyxText): TNyxStudioDirectories;
begin
  Result.FMode := nsdmRepository;
  Result.FSourceRoot := NormalizeDirectory(ARepository);
  Result.FRuntimeRoot := Result.FSourceRoot;
  Result.FEnrollmentRoot := Result.FSourceRoot;
end;

class function TNyxStudioDirectories.ForRelease(
  const ARelease, ARuntime: TNyxText): TNyxStudioDirectories;
begin
  Result.FMode := nsdmRelease;
  Result.FSourceRoot := NormalizeDirectory(ARelease);
  Result.FRuntimeRoot := NormalizeDirectory(ARuntime);
  Result.FEnrollmentRoot := Result.FRuntimeRoot;
  Result.Validate;
  VerifyNyxStudioRelease(Result.FSourceRoot);
end;

function TNyxStudioDirectories.EnrollingProject(const AProject: TNyxText): TNyxStudioDirectories;
begin
  Result := Self;
  Result.FEnrollmentRoot := NormalizeDirectory(AProject);
  Result.Validate;
end;

function TNyxStudioDirectories.RunningIn(const ARuntime: TNyxText): TNyxStudioDirectories;
begin
  Result := Self;
  Result.FRuntimeRoot := NormalizeDirectory(ARuntime);
  Result.Validate;
end;

procedure TNyxStudioDirectories.Validate;
begin

  if (FSourceRoot = '') or (FRuntimeRoot = '') or (FEnrollmentRoot = '') then
  begin
    raise ENyxStudioRelease.Create('Studio directories are not initialized');
  end;

  if FMode = nsdmRelease then
  begin
    RequireDisjoint(FSourceRoot, FRuntimeRoot);
    RequireDisjoint(FSourceRoot, FEnrollmentRoot + '.codex' + PathDelim);
    RequireDisjoint(FSourceRoot, FEnrollmentRoot + '.local' + PathDelim);
    ValidateNyxStudioDirectoryPath(FSourceRoot);
    ValidateNyxStudioDirectoryPath(FRuntimeRoot, True);
    ValidateNyxStudioDirectoryPath(FEnrollmentRoot, True);
    { Existing write subdirectories may themselves be junctions. Recheck their
      ancestors before enrollment/profile/job consumers follow a configured root. }
    ValidateNyxStudioDirectoryPath(FEnrollmentRoot + '.codex', True);
    ValidateNyxStudioDirectoryPath(FEnrollmentRoot + '.local', True);
    ValidateNyxStudioDirectoryPath(FRuntimeRoot + '.local', True);
    ValidateNyxStudioDirectoryPath(GetJobs, True);
    ValidateNyxStudioDirectoryPath(GetProjects, True);
    ValidateNyxStudioDirectoryPath(GetPreviews, True);
  end;
end;

function TNyxStudioDirectories.GetCompilerUnits: TNyxText;
begin
  Result := FSourceRoot + 'src';
end;

function TNyxStudioDirectories.GetWebRoot: TNyxText;
begin

  if FMode = nsdmRelease then
  begin
    Result := FSourceRoot + 'web' + PathDelim;
  end
  else
  begin
    Result := FSourceRoot + 'build' + PathDelim + 'browser' + PathDelim;
  end;
end;

function TNyxStudioDirectories.GetJobs: TNyxText;
begin
  Result := FRuntimeRoot + 'build' + PathDelim + 'studio' + PathDelim + 'jobs' + PathDelim;
end;

function TNyxStudioDirectories.GetProjects: TNyxText;
begin
  Result := FRuntimeRoot + '.local' + PathDelim + 'projects';
end;

function TNyxStudioDirectories.GetOutputProfile: TNyxText;
begin
  Result := FRuntimeRoot + '.local' + PathDelim + 'studio-outputs.nyx';
end;

function TNyxStudioDirectories.GetPreviews: TNyxText;
begin
  Result := FRuntimeRoot + 'build' + PathDelim + 'agent-previews' + PathDelim;
end;

function TNyxStudioDirectories.GetSessionCheckpoint: TNyxText;
begin
  Result := FRuntimeRoot + '.local' + PathDelim + 'studio-session.nyx';
end;

end.
