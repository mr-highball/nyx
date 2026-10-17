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
unit nyx.test.projectinputs.native;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.studio.directories, nyx.studio.buildexecutor,
  nyx.studio.projectinputs;

{ Additional compiler/storage guards for an already owned runtime. No listeners,
  editor/project adoption, existing-job cleanup or application launch occurs.
  Returned JSON records compiler receipts; the caller persists it in its packet. }
function RunNyxProjectInputGuards(AExecutor: TNyxBuildExecutor;
  const ADirectories: TNyxStudioDirectories;
  const AOriginal, AChanged: INyxPascalProjectBuild;
  out AReceipts: TNyxText): Integer;

implementation

uses Classes, SysUtils, fpjson, nyx.bytes, nyx.model, nyx.source, nyx.pascal.project,
  nyx.studio.builds, nyx.studio.sourceprojection, nyx.studio.projectionstorage;

type
  { Deliberately misleading public wrapper pairs a real old projection with new
    helper text. It owns those values but has no executor-local capture facet.
    Public interface shape/main-source equality must not grant a build shortcut. }
  TPublicProjectBuild = class(TInterfacedObject, INyxSourceProjectionBuild,
    INyxPascalProjectBuild)
  public
    Original: INyxPascalProjectBuild;
    Proposed: INyxPascalProject;
    function GetInputs: INyxPascalProject;
    function GetReference: TNyxSourceProjectionRef;
    function GetProjection: INyxSourceProjection;
    function GetArtifact: TNyxText;
    function GetRuntimeLog: TNyxText;
  end;

function TPublicProjectBuild.GetInputs: INyxPascalProject;
begin
  Result := Proposed;
end;

function TPublicProjectBuild.GetReference: TNyxSourceProjectionRef;
begin
  Result := Original.Reference;
end;

function TPublicProjectBuild.GetProjection: INyxSourceProjection;
begin
  Result := Original.Projection;
end;

function TPublicProjectBuild.GetArtifact: TNyxText;
begin
  Result := Original.Artifact;
end;

function TPublicProjectBuild.GetRuntimeLog: TNyxText;
begin
  Result := Original.RuntimeLog;
end;

function ReadText(const APath: TNyxText): TNyxText;
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try
    SetLength(LBytes, LStream.Size);

    if Length(LBytes) > 0 then
    begin
      LStream.ReadBuffer(LBytes[0], Length(LBytes));
    end;
    Result := NyxDecodeUTF8(LBytes);
  finally
    LStream.Free;
  end;
end;

function JobCount(const ADirectory: TNyxText): Integer;
var
  LSearch: TSearchRec;
begin
  Result := 0;

  if FindFirst(ADirectory + 'job-*', faDirectory, LSearch) = 0 then
  begin
    try
      repeat

        if (LSearch.Attr and faDirectory) <> 0 then
        begin
          Inc(Result);
        end;
      until FindNext(LSearch) <> 0;
    finally
      FindClose(LSearch);
    end;
  end;
end;

function WriteRefused(const APath: TNyxText): Boolean;
var
  LStream: TFileStream;
begin
  Result := False;
  try
    LStream := TFileStream.Create(APath, fmOpenWrite or fmShareDenyNone);
    LStream.Free;
  except
    on E: EFOpenError do
    begin
      Result := True;
    end;
  end;
end;

function RunNyxProjectInputGuards(AExecutor: TNyxBuildExecutor;
  const ADirectories: TNyxStudioDirectories;
  const AOriginal, AChanged: INyxPascalProjectBuild;
  out AReceipts: TNyxText): Integer;
var
  LChecks: Integer;
  LPublic: TPublicProjectBuild;
  LPublicOwner: INyxPascalProjectBuild;
  LBuild: INyxPascalProjectBuild;
  LInput: INyxPascalProject;
  LReply: TJSONObject;
  LReports: TJSONArray;
  LFailed: Boolean;
  LBefore: Integer;
  LStorage: TNyxOwnedProjectionStorage;
  LReport: TNyxProjectionStorageReport;
  LScope: INyxProjectionStorageBuild;
  LRole: TNyxOwnedCompilerEntry;
  LID: TGUID;
  LDirectory: TNyxText;
  LFile: TNyxText;
  LIndex: Integer;
  LStream: TFileStream;
  LWrapper: TNyxText;
  LTarget: TNyxBuildTarget;

  procedure Check(AValue: Boolean; const AReason: String);
  begin

    if not AValue then
    begin
      raise Exception.Create('Project input guard: ' + AReason);
    end;
    Inc(LChecks);
  end;

  procedure Retain(const AValue: INyxPascalProjectBuild);
  begin
    LReports.Add(TJSONObject.Create([
      'job', AValue.Reference.Name,
      'state', NyxSourceProjectionStateName(AValue.Projection.State),
      'artifact', AValue.Artifact]));
  end;

begin
  LChecks := 0;
  AReceipts := '';
  LReports := TJSONArray.Create;
  try
    LPublic := TPublicProjectBuild.Create;
    LPublicOwner := LPublic;
    LPublic.Original := AOriginal;
    LPublic.Proposed := AChanged.Inputs;
    Check(LPublic.GetProjection.Source = LPublic.GetInputs.Source,
      'the misleading wrapper has the exact same main source');
    LBefore := JobCount(ADirectories.Jobs);
    LFailed := False;
    try
      LReply := AExecutor.BuildProject(LPublicOwner, btNativeLCL, bsApplication);
      LReply.Free;
    except
      on E: ENyxModel do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (JobCount(ADirectories.Jobs) = LBefore),
      'public metadata cannot attach new helper meaning to an old construction');
    LPublicOwner := nil;
    LInput := AOriginal.Inputs.WithoutInclude(NyxPascalInclude('plan-values.inc'));
    LFailed := False;
    try
      LBuild := AExecutor.ProjectSource(LInput, btNativeLCL, spcChecked);
    except
      on E: ENyxSource do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (JobCount(ADirectories.Jobs) = LBefore),
      'incomplete input inventories refuse before invocation allocation');
    LInput := AOriginal.Inputs.WithInclude(NyxPascalInclude('plan-values.inc'),
      'This deliberately is not Pascal.');
    LBuild := AExecutor.ProjectSource(LInput, btNativeLCL, spcChecked);
    Check(LBuild.Projection.State = spsCompilationFailed,
      'actual helper-include syntax errors remain compiler failures');
    Check((LBuild.Inputs.Input(2).Text = 'This deliberately is not Pascal.') and
      (LBuild.Projection.Source = AOriginal.Inputs.Source),
      'a failed compiler receipt retains its exact independent multi-file input');
    Retain(LBuild);
    LInput := AOriginal.Inputs.WithInclude(NyxPascalInclude('plan-values.inc'),
      'function ProjectPlan: TNyxText;' + #10 + 'begin' + #10 +
      '  Result := '''';' + #10 +
      '  raise Exception.Create(''An owned project constructor failure'');' + #10 +
      'end;' + #10);
    { The helper imports SysUtils explicitly for its own ordinary Pascal body. }
    LInput := LInput.WithUnit(NyxPascalUnit('project.values'),
      'unit project.values;' + #10 + '{$mode delphi}{$H+}{$codepage utf8}' + #10 +
      'interface' + #10 + 'uses SysUtils, nyx.text;' + #10 +
      'function ProjectPlan: TNyxText;' + #10 + 'implementation' + #10 +
      '{$I plan-values.inc}' + #10 + 'end.' + #10);
    LBuild := AExecutor.ProjectSource(LInput, btNativeLCL, spcChecked);
    Check(LBuild.Projection.State = spsExecutionFailed,
      'a compiled helper exception is distinct from compilation and design admission');
    Retain(LBuild);
    LFailed := False;
    try
      LReply := AExecutor.BuildProject(LBuild, btNativeLCL, bsApplication);
      LReply.Free;
    except
      on E: ENyxModel do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'a failed constructor cannot become an application build');
    LBuild := AExecutor.ProjectSource(AOriginal.Inputs, btBrowser, spcChecked);
    Check((LBuild.Projection.State = spsCompiled) and
      (LBuild.Projection.Design = '') and (LBuild.Artifact <> ''),
      'browser project compilation still grants no executed application design');
    Check(Supports(LBuild, INyxProjectionStorageBuild, LScope) and
      (LScope.Storage.State = psPreserved), 'the new receipt forwards actual browser storage evidence');
    Retain(LBuild);
    LFailed := False;
    try
      LReply := AExecutor.BuildProject(LBuild, btBrowser, bsApplication);
      LReply.Free;
    except
      on E: ENyxModel do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'a compiled-only project cannot impersonate an executed constructor');
    for LRole := Low(TNyxOwnedCompilerEntry) to High(TNyxOwnedCompilerEntry) do
    begin
      CreateGUID(LID);
      LTarget := btNativeLCL;

      if LRole = oceBrowserApplication then
      begin
        LTarget := btBrowser;
      end;
      LStorage := TNyxOwnedProjectionStorage.Create(ADirectories.Jobs,
        NyxSourceProjectionRef('job-' + Copy(GUIDToString(LID), 2, 36)),
        AOriginal.Inputs.MainUnit.Name, LTarget,
        TNyxProjectionStoragePolicy.Default.Retaining(prAllFiles).MinimumFreeBytes(0),
        LReport, LRole);
      try
        LDirectory := LStorage.Directory;
        LStorage.WriteEvidence(AOriginal.Inputs.MainUnit.Name + '.pas', AOriginal.Inputs.Source);
        LWrapper := 'nyx_projection.lpr';

        if LRole = oceBrowserApplication then
        begin
          LWrapper := 'nyx_preview.lpr';
        end
        else if LRole = oceNativeApplication then
        begin
          LWrapper := 'nyx_native.lpr';
        end;
        LStorage.WriteEvidence(LWrapper, 'program OwnedEntry; begin end.');
        for LIndex := 0 to AOriginal.Inputs.Count - 1 do
        begin
          LStorage.WriteProjectInput(AOriginal.Inputs.Input(LIndex));
          LFile := LDirectory + 'project' + PathDelim + AOriginal.Inputs.Input(LIndex).Name;
          Check(ReadText(LFile) = AOriginal.Inputs.Input(LIndex).Text,
            'the owned project source directory retains exact file bytes');
          Check(WriteRefused(LFile), 'captured units/includes refuse a concurrent writer');
        end;
        LFailed := False;
        try
          LStorage.WriteProjectInput(AOriginal.Inputs.Input(0));
        except
          on E: ENyxModel do
          begin
            LFailed := True;
          end;
        end;
        Check(LFailed, 'an existing captured source is never overwritten');
        LStorage.FinishAfterJoin;
        Check(WriteRefused(LFile), 'retained storage owns source pins until explicit scope release');
      finally
        LStorage.Free;
      end;
      LStream := TFileStream.Create(LFile, fmOpenWrite or fmShareExclusive);
      LStream.Free;
      Check(ReadText(LFile) = AOriginal.Inputs.Input(AOriginal.Inputs.Count - 1).Text,
        'scope retirement releases the exact file without changing its bytes');
    end;
    AReceipts := LReports.AsJSON;
    Result := LChecks;
  finally
    LReports.Free;
  end;
end;

end.
