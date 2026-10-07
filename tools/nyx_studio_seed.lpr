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

program nyx_studio_seed;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.text, nyx.data, nyx.model, nyx.studio.legacy,
  nyx.studio.directories, nyx.studio.recovery, nyx.studio.agents,
  nyx.studio.workspaces;

var
  LStream: TFileStream;
  LText: TNyxText;
  LSnapshot: TNyxLegacyStudioSnapshot;
  LStore: TNyxStudioRuntimeStore;
  LDirectories: TNyxStudioDirectories;
  LPolicy: TNyxLegacyHistoryPolicy;
  LIdentity: TGUID;
  LRuntime: TNyxText;
  LRecoveredPrimary: TNyxAgentSession;
  LRecoveredWorkspaces: TNyxStudioWorkspaces;
begin
  LSnapshot := nil;
  LStore := nil;
  try

    if (ParamCount <> 4) or
      ((ParamStr(4) <> 'require-empty-history') and (ParamStr(4) <> 'reset-test-history')) then
    begin
      raise ENyxModel.Create('Usage: <snapshot.json> <release-root> <new-runtime> ' +
        'require-empty-history|reset-test-history');
    end;
    LPolicy := nlhRequireEmptyHistory;

    if ParamStr(4) = 'reset-test-history' then
    begin
      LPolicy := nlhResetTestHistory;
    end;
    LStream := TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyWrite);
    try

      if (LStream.Size < 1) or (LStream.Size > 4 * 1024 * 1024) then
      begin
        raise ENyxModel.Create('Legacy snapshot exceeds its 4 MiB input budget');
      end;
      SetLength(LText, LStream.Size);
      LStream.ReadBuffer(LText[1], Length(LText));
    finally
      LStream.Free;
    end;
    CreateGUID(LIdentity);
    LSnapshot := TNyxLegacyStudioSnapshot.Create(TNyxDataValue.ParseJSON(LText),
      LPolicy, Copy(TNyxText(GUIDToString(LIdentity)), 2, 36));
    LRuntime := ExpandFileName(ParamStr(3));

    if DirectoryExists(LRuntime) or FileExists(LRuntime) then
    begin
      raise ENyxModel.Create('Seed destination already exists; choose a fresh runtime');
    end;
    { Admission of every pair precedes filesystem creation. Native directory and
      store admission recheck ordinary ancestors, role separation and the unique
      process lock. This tool starts no listener and owns no enrollment. }
    LDirectories := TNyxStudioDirectories.ForRelease(ParamStr(2), LRuntime);

    if not ForceDirectories(LRuntime) then
    begin
      raise ENyxModel.Create('Cannot create the fresh admitted runtime');
    end;
    LStore := TNyxStudioRuntimeStore.Create(LDirectories);
    LStore.Save(LSnapshot.Primary, LSnapshot.Workspaces);
    LRecoveredPrimary := nil;
    LRecoveredWorkspaces := nil;
    try

      if not LStore.Load(LRecoveredPrimary, LRecoveredWorkspaces) or
        (LRecoveredPrimary.RecoveryStamp <> LSnapshot.Primary.RecoveryStamp) or
        (LRecoveredWorkspaces.RecoveryStamp <> LSnapshot.Workspaces.RecoveryStamp) then
      begin
        raise ENyxModel.Create('Saved legacy seed differs from its admitted runtime');
      end;
    finally
      LRecoveredWorkspaces.Free;
      LRecoveredPrimary.Free;
    end;
    WriteLn(LSnapshot.Report.ToJSON);
  except
    on LException: Exception do
    begin
      WriteLn(StdErr, LException.ClassName + ': ' + LException.Message);
      ExitCode := 1;
    end;
  end;
  LStore.Free;
  LSnapshot.Free;
end.
