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


program nyx_studio_deployment_check;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, nyx.text, nyx.data, nyx.studio.projects, nyx.test.mcp.client;

{ Read-only deployment admission/verification. The preflight checks every exact
  observing pair and refuses active compiler work. Verification obtains a rotated
  observing credential by claiming the already restored primary, compares all
  nine contexts and saves only the private operator receipt. Neither path edits
  a document, accepts a draft, changes history or creates a demo workspace.
  The explicit workshop mode additionally checks all eight protected contexts
  while a named owned test project differs. It requires exact expected accepted
  Pascal with no pending draft; its receipt never claims that project's history
  unchanged and grants no restoration or replacement operation. }

{ Read a private native UTF-8 byte boundary, refusing empty/oversized inputs
  before allocation. The caller owns no stream after this routine returns. }
function ReadValue(const APath: String; ALimit: Integer): TNyxDataValue;
var
  LStream: TFileStream;
  LText: TNyxText;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try

    if (LStream.Size < 1) or (LStream.Size > ALimit) then
    begin
      raise Exception.Create('Deployment input exceeds its private byte bound');
    end;
    SetLength(LText, LStream.Size);
    LStream.ReadBuffer(LText[1], Length(LText));
  finally
    LStream.Free;
  end;
  Result := TNyxDataValue.ParseJSON(LText);
end;

{ Exact accepted source is a native UTF-8 byte boundary, not JSON or an ANSI
  collection. The workshop audit compares every byte and releases its stream. }
function ReadSource(const APath: String): TNyxText;
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try

    if (LStream.Size < 1) or (LStream.Size > 4 * 1024 * 1024) then
    begin
      raise Exception.Create('Workshop source exceeds its private byte bound');
    end;
    SetLength(Result, LStream.Size);
    LStream.ReadBuffer(Result[1], Length(Result));
  finally
    LStream.Free;
  end;
end;

{ Write an exact UTF-8 JSON receipt to a caller-selected private file. These
  outputs can contain rotated authority or machine paths and are never printed. }
procedure SaveValue(const APath: String; const AValue: TNyxDataValue);
var
  LStream: TFileStream;
  LText: TNyxText;
begin
  LText := AValue.ToJSON;
  LStream := TFileStream.Create(APath, fmCreate);
  try
    LStream.WriteBuffer(LText[1], Length(LText));
  finally
    LStream.Free;
  end;
end;

const
  CFields: array[0..6] of TNyxText = ('selection', 'revision', 'canUndo',
    'view', 'canRedo', 'pendingDraft', 'permission');

{ Keep exceptional CLI refusals inside the program's ownership boundary. The
  caller receives a useful nonzero result after the transport is released. }
procedure Run;
var
  LClient: TNyxMCPTestClient;
  LBase: TNyxText;
  LBefore: TNyxDataValue;
  LPrivate: TNyxDataValue;
  LReply: TNyxDataValue;
  LExpected: TNyxDataValue;
  LArguments: TNyxDataValue;
  LJobs: TNyxDataValue;
  LItems: TNyxDataValue;
  LItem: TNyxDataValue;
  LWorkspace: TNyxText;
  LProject: Integer;
  LField: Integer;
  LIndex: Integer;
  LMatches: Integer;
  LTools: Integer;
  LProfile: TNyxDataValue;
  LSession: TNyxDataValue;
  LWorkshop: Boolean;
  LOwned: Boolean;
  LOwnedCount: Integer;
  LOwnedSource: TNyxText;
  LOwnedPair: TNyxProjectPair;
  LOwnedReply: TNyxDataValue;
  LPrior: Integer;
begin
  LClient := nil;
  try

    LWorkshop := (ParamCount = 8) and (ParamStr(1) = 'workshop');

    if not LWorkshop and ((ParamCount <> 6) or
      not ((ParamStr(1) = 'preflight') or (ParamStr(1) = 'verify'))) then
    begin
      raise Exception.Create('Use preflight/verify, origin, Codex config, protected pairs, ' +
        'private editor receipt, result path; workshop also requires owned workspace and exact source');
    end;

    if LWorkshop then
    begin

      if ParamStr(7) = '' then
      begin
        raise Exception.Create('Workshop audit requires a non-primary owned context');
      end;
      LOwnedSource := ReadSource(ParamStr(8));
    end;
    LOwnedCount := 0;
    LBase := ParamStr(2);
    LBefore := ReadValue(ParamStr(4), 4 * 1024 * 1024);

    if (LBefore.Kind <> ndArray) or (LBefore.Count <> 9) or
      (LBefore.Item(0).Field('workspace').AsText <> '') then
    begin
      raise Exception.Create('Deployment requires the complete nine-context baseline');
    end;
    { Count alone cannot prove complete coverage: a repeated handle could stand
      in for a missing protected project. Admit unique identities before making
      any connection or publishing a private audit receipt. }
    for LProject := 0 to LBefore.Count - 1 do
    begin
      for LPrior := 0 to LProject - 1 do
      begin

        if LBefore.Item(LProject).Field('workspace').AsText =
          LBefore.Item(LPrior).Field('workspace').AsText then
        begin
          raise Exception.Create('Deployment baseline contains duplicate project identities');
        end;
      end;
    end;
    LPrivate := ReadValue(ParamStr(5), 4 * 1024 * 1024);
    LClient := TNyxMCPTestClient.Create(ParamStr(3), 'Scooty preserved deployment verification');
    LTools := LClient.RPC('tools/list', NyxObject([])).Field('result').Field('tools').Count;

    if ParamStr(1) = 'verify' then
    begin
      LExpected := LBefore.Item(0);
      { Never seed an unclaimed/default host while verifying recovery. Bounded
        authenticated context must already match this nonzero protected revision
        and navigation before an operator claim can obtain rotated authority. }
      LSession := LClient.Tool('nyx_session', NyxObject([]));

      if LSession.Field('isError').AsBoolean then
      begin
        raise Exception.Create('Restored semantic context refused');
      end;
      LSession := LSession.Field('structuredContent');

      if (LExpected.Field('revision').AsInteger <= 0) or
        (LSession.Field('revision').AsInteger <> LExpected.Field('revision').AsInteger) or
        (LSession.Field('selection').AsText <> LExpected.Field('selection').AsText) or
        (LSession.Field('view').AsText <> LExpected.Field('view').AsText) then
      begin
        raise Exception.Create('Verification requires the already restored primary context');
      end;
      { This is an operator connection to the already recovered primary. Exact
        pair comparison below must succeed before publishing the new credential. }
      LPrivate := NyxTestEditorExchange(LBase, '/api/agents/connect', '', NyxObject([
        NyxField('op', NyxData('claim')), NyxField('project', LExpected.Field('project')),
        NyxField('selection', LExpected.Field('selection')), NyxField('view', LExpected.Field('view'))]));

      if LPrivate.Field('warning').AsText <> '' then
      begin
        raise Exception.Create('Installed enrollment reported a private diagnostic');
      end;
    end;
    for LProject := 0 to LBefore.Count - 1 do
    begin
      LExpected := LBefore.Item(LProject);
      LWorkspace := LExpected.Field('workspace').AsText;
      LOwned := LWorkshop and (LWorkspace = TNyxText(ParamStr(7)));

      if LOwned then
      begin
        Inc(LOwnedCount);

        if (LProject = 0) or (LWorkspace = '') or
          (LExpected.Field('label').AsText <> 'Manual presentations') then
        begin
          raise Exception.Create('Only the explicitly owned Manual presentations workshop may differ');
        end;
      end;
      LArguments := NyxObject([NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))]);

      if LWorkspace <> '' then
      begin
        LArguments := NyxObject([NyxField('op', NyxData('observe')),
          NyxField('after', NyxData(0)), NyxField('workspace', NyxData(LWorkspace))]);
      end;
      LReply := NyxTestEditorExchange(LBase, '/api/agents',
        LPrivate.Field('token').AsText, LArguments);

      if not LOwned and
        (LReply.Field('project').AsText <> LExpected.Field('project').AsText) then
      begin
        raise Exception.Create('Deployment changed an exact Pascal/design/draft pair');
      end;
      for LField := 0 to High(CFields) do
      begin

        if LOwned and (CFields[LField] <> 'permission') then
        begin
          Continue;
        end;

        if LReply.Field('session').Field(CFields[LField]).ToJSON <>
          LExpected.Field(CFields[LField]).ToJSON then
        begin
          raise Exception.Create('Deployment changed retained ' + CFields[LField]);
        end;
      end;

      if LOwned then
      begin
        LOwnedPair := DecodeNyxProject(LReply.Field('project').AsText);

        if LOwnedPair.Pending or (LOwnedPair.Source <> LOwnedSource) then
        begin
          raise Exception.Create('Owned workshop is not the exact expected accepted source without a draft');
        end;
        LOwnedReply := LReply;
      end;
      LItems := LReply.Field('workspaces');

      if LItems.Count <> LBefore.Count then
      begin
        raise Exception.Create('Deployment changed the complete project count');
      end;
      LMatches := 0;
      for LIndex := 0 to LItems.Count - 1 do
      begin
        LItem := LItems.Item(LIndex);

        if LItem.Field('workspace').AsText = LWorkspace then
        begin
          Inc(LMatches);

          if LItem.Field('label').AsText <> LExpected.Field('label').AsText then
          begin
            raise Exception.Create('Deployment changed a retained project label');
          end;
        end;
      end;

      if LMatches <> 1 then
      begin
        raise Exception.Create('Deployment changed retained workspace identity');
      end;
      LArguments := NyxObject([NyxField('mode', NyxData('jobs')),
        NyxField('filter', NyxData('active')), NyxField('limit', NyxData(1))]);

      if LWorkspace <> '' then
      begin
        LArguments := NyxObject([NyxField('mode', NyxData('jobs')),
          NyxField('filter', NyxData('active')), NyxField('limit', NyxData(1)),
          NyxField('workspace', NyxData(LWorkspace))]);
      end;
      LJobs := LClient.Tool('nyx_build', LArguments);

      if LJobs.Field('isError').AsBoolean then
      begin
        raise Exception.Create('Bounded semantic compiler preflight refused');
      end;

      if LJobs.Field('structuredContent').Field('total').AsInteger <> 0 then
      begin
        raise Exception.Create('An active compiler job postpones service replacement');
      end;
    end;

    if LWorkshop and (LOwnedCount <> 1) then
    begin
      raise Exception.Create('The baseline does not contain exactly one explicit owned workshop');
    end;
    { Output paths stay private, but a restart must retain the actual machine
      profile independently of all portable pairs. Save it for byte comparison
      by the platform installer, without printing compiler paths or tokens. }
    LProfile := NyxTestEditorExchange(LBase, '/api/agents',
      LPrivate.Field('token').AsText, NyxObject([
      NyxField('op', NyxData('build')), NyxField('after', NyxData(0)),
      NyxField('build', NyxObject([NyxField('mode', NyxData('profile'))]))]))
      .Field('buildReply').Field('profile');
    SaveValue(ParamStr(6) + '.outputs.json', LProfile);
    { Publish the owned observation only after every protected context, idle-job
      gate and output-profile read passes. A failed later project check must not
      leave a new workshop receipt that resembles a successful complete audit. }

    if LWorkshop then
    begin
      SaveValue(ParamStr(6) + '.workshop.json', LOwnedReply);
    end;
    SaveValue(ParamStr(6), LPrivate);

    if LWorkshop then
    begin
      WriteLn('Preserved workshop: eight exact protected contexts, exact owned accepted Pascal, ',
        'no active compiler jobs, ', LTools, ' authenticated tools');
    end
    else
    begin
      WriteLn('Preserved deployment: nine exact contexts, no active compiler jobs, ',
        LTools, ' authenticated tools');
    end;
    LClient.Close;
  finally
    LClient.Free;
  end;
end;

begin
  try
    Run;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
