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
program nyx_mcp_authority_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.authority.fixture, nyx.studio.agents,
  nyx.studio.mcp, nyx.studio.directories, nyx.studio.outputs,
  nyx.studio.workspaces;

type
  { Owns a suspended actual protocol engine with a fresh private runtime. Its
    public ordinary-tool seam is the same one used after HTTP authentication.
    Neither engine.Start nor a Studio HTTP listener is called. All qualification
    mutations use semantic tools and trusted operator observation. }
  THostAuthorityDriver = class(TNyxAuthorityDriver)
  private
    FEngine: TNyxStudioMCP;
    FToken: TNyxText;
    FWorkspace: TNyxWorkspaceRef;
  public
    constructor Create(const ARuntime: TNyxText);
    destructor Destroy; override;
    function Call(const ATool, AOwner: TNyxText;
      const AArguments: TNyxDataValue): TNyxDataValue; override;
    function Snapshot: TNyxDataValue; override;
    { Creates and selects only an ordinary independent semantic project. }
    procedure UseNewProject;
  end;

constructor THostAuthorityDriver.Create(const ARuntime: TNyxText);
var
  LSeed: TNyxAgentSession;
  LProfile: TNyxOutputConfiguration;
  LClaim: TNyxDataValue;
begin
  inherited Create;

  if DirectoryExists(ARuntime) then
  begin
    raise Exception.Create('Authority qualification requires a new owned runtime');
  end;
  LSeed := TNyxAgentSession.Create;
  LProfile := nil;
  try
    LProfile := TNyxOutputConfiguration.Create;
    FEngine := TNyxStudioMCP.Create(TNyxStudioDirectories.ForRepository(ARuntime),
      8628, 8629, LProfile.Encode);
    LClaim := FEngine.ConnectEditor(NyxObject([NyxField('op', NyxData('claim')),
      NyxField('project', LSeed.Exchange(NyxObject([
        NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))])).Field('project')),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
    FToken := LClaim.Field('token').AsText;
  finally
    LProfile.Free;
    LSeed.Free;
  end;
end;

destructor THostAuthorityDriver.Destroy;
begin
  FEngine.Free;
  inherited Destroy;
end;

function THostAuthorityDriver.Call(const ATool, AOwner: TNyxText;
  const AArguments: TNyxDataValue): TNyxDataValue;
begin
  Result := FEngine.InvokeTool(ATool, AOwner, 'Same visible actor 😀',
    NyxWithWorkspace(AArguments, FWorkspace));
end;

function THostAuthorityDriver.Snapshot: TNyxDataValue;
begin
  Result := FEngine.EditorExchange(FToken, NyxWithWorkspace(NyxObject([
    NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))]), FWorkspace));
end;

procedure THostAuthorityDriver.UseNewProject;
var
  LCreated: TNyxDataValue;
begin
  LCreated := FEngine.InvokeTool('nyx_workspaces', 'connection-one',
    'Same visible actor 😀', NyxObject([
      NyxField('mode', NyxData('create')), NyxField('operationId', NyxData('create-project')),
      NyxField('expectedRevision', Snapshot.Field('session').Field('revision')),
      NyxField('base', NyxData('accepted')), NyxField('label', NyxData('Authority project'))]));
  FWorkspace := NyxWorkspace(LCreated.Field('workspace').AsText);
end;

var
  LDriver: THostAuthorityDriver;
  LChecks: Integer;
  LPrimary: TNyxDataValue;
begin
  LDriver := nil;
  LChecks := 0;
  try

    if ParamCount <> 1 then
    begin
      raise Exception.Create('Supply a new owned qualification runtime');
    end;
    LDriver := THostAuthorityDriver.Create(ExpandFileName(ParamStr(1)));
    VerifyNyxAgentAuthority(LDriver, LChecks);
    LPrimary := LDriver.Snapshot;
    LDriver.UseNewProject;
    VerifyNyxAgentAuthority(LDriver, LChecks);
    LDriver.FWorkspace := NyxPrimaryWorkspace;

    if LDriver.Snapshot.Field('project').AsText <> LPrimary.Field('project').AsText then
    begin
      raise Exception.Create('Project authority qualification changed the primary pair');
    end;
    Inc(LChecks);
    WriteLn('PASS ', LChecks, ' actual protocol authority checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL: ', LException.Message);
      ExitCode := 1;
    end;
  end;
  LDriver.Free;
end.
