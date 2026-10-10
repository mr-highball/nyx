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

unit nyx.studio.sourcecompilation.service.browser;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.studio.workspaces, nyx.studio.transport,
  nyx.studio.sourcecompilation.browser;

{ Explicit private-editor compilation provider. Capture capability, project and
  acknowledged revision from that editor connection, never from a design file.
  Every operation owns its copied context, same-origin XHR, polling and deadline;
  it borrows no bridge/session. Construction starts no request and needs no
  compiler or output selection. Exact retries recover a lost admission receipt.
  Cancellation asks the server to retire the captured job, then observes its join;
  aborting a local XHR alone never claims server cancellation. }
function NewNyxBrowserSourceService(const AEditorCapability: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; AExpectedRevision: Integer;
  const APolicy: INyxTransportPolicy = nil): INyxBrowserSourceBuilder;

implementation

uses SysUtils, Web, nyx.bytes, nyx.data, nyx.model, nyx.editing, nyx.studio.builds,
  nyx.studio.editorbuild,
  nyx.studio.sourcebuilds, nyx.studio.sourceprojection,
  nyx.studio.sourcecompilation;

type
  TSourceWireMode = (swmRequest, swmStatus, swmCancel);
  THTTPCompilation = class(TInterfacedObject, INyxSourceCompilation)
  private
    FState: TNyxSourceCompilationState;
    FToken: TNyxText;
    FWorkspace: TNyxWorkspaceRef;
    FSource: TNyxText;
    FArguments: TNyxDataValue;
    FJob: TNyxBuildJobRef;
    FLimits: TNyxTransportLimits;
    FPort: INyxBrowserSourceBuildPort;
    { Keeps retirement alive after the caller releases a cancelled token. }
    FLease: INyxSourceCompilation;
    FRequest: TJSXMLHttpRequest;
    FTimer: NativeInt;
    FDeadline: NativeInt;
    FCancelled: Boolean;
    procedure Send;
    procedure Ready;
    procedure Tick;
    procedure Expired;
    procedure Retire;
    procedure Fail(const AReason: TNyxText);
    procedure Finish(const ABuild: INyxSourceProjectionBuild);
  public
    procedure Cancel;
    function GetState: TNyxSourceCompilationState;
  end;
  TSourceService = class(TInterfacedObject, INyxBrowserSourceBuilder)
  public
    Token: TNyxText;
    Workspace: TNyxWorkspaceRef;
    Revision: Integer;
    Limits: TNyxTransportLimits;
    function Compile(const ASource: TNyxText;
      const APort: INyxBrowserSourceBuildPort): INyxSourceCompilation;
  end;

procedure THTTPCompilation.Retire;
begin

  if FRequest <> nil then
  begin
    FRequest.onreadystatechange := nil;
    FRequest.abort;
    FRequest := nil;
  end;

  if FTimer <> 0 then
  begin
    window.clearTimeout(FTimer);
    FTimer := 0;
  end;

  if FDeadline <> 0 then
  begin
    window.clearTimeout(FDeadline);
    FDeadline := 0;
  end;
end;

function THTTPCompilation.GetState: TNyxSourceCompilationState;
begin
  Result := FState;
end;

procedure THTTPCompilation.Cancel;
begin

  if not (FState in [scsPending, scsRunning]) then
  begin
    Exit;
  end;
  FCancelled := True;
  FPort := nil;
  { Keep a pending admission request: its exact receipt is needed to cancel the
    server job. If that response is lost, Send retries the same operation ID. }

  if (FRequest = nil) and (FTimer = 0) then
  begin
    Send;
  end;
end;

procedure THTTPCompilation.Fail(const AReason: TNyxText);
var
  { Completion/retirement can release the last external token. }
  LLease: INyxSourceCompilation;
  LPort: INyxBrowserSourceBuildPort;
begin
  LLease := Self;
  Retire;
  FState := scsFailed;
  LPort := FPort;
  FPort := nil;
  FLease := nil;

  if LPort <> nil then
  begin
    LPort.Compiled(nil, AReason);
  end;
end;

procedure THTTPCompilation.Finish(const ABuild: INyxSourceProjectionBuild);
var
  { Independent callbacks must survive releasing their own producer lease. }
  LLease: INyxSourceCompilation;
  LPort: INyxBrowserSourceBuildPort;
begin
  LLease := Self;
  Retire;
  FState := scsCancelled;
  LPort := FPort;
  FPort := nil;
  FLease := nil;

  if not FCancelled then
  begin
    FState := scsCompleted;

    if ABuild.Projection.State <> spsCompiled then
    begin
      FState := scsFailed;
    end;
    LPort.Compiled(ABuild);
  end;
end;

procedure THTTPCompilation.Expired;
begin
  { This is a bounded local transport refusal, not a claim that a remote worker
    joined. The server independently expires queued/running source jobs at 120s;
    each spawned compiler also retains the existing process-family time budget. }
  Fail('Source compiler transport exceeded its observation deadline; accepted work is retained');
end;

procedure THTTPCompilation.Tick;
begin
  FTimer := 0;
  Send;
end;

procedure THTTPCompilation.Send;
const
  CMode: array[TSourceWireMode] of TNyxText = ('request', 'status', 'cancel');
var
  LMode: TSourceWireMode;
  LArguments: TNyxDataValue;
begin
  try
    LArguments := FArguments;

    if FJob.ID <> '' then
    begin
      LMode := swmStatus;

      if FCancelled then
      begin
        LMode := swmCancel;
      end;
      LArguments := NyxObject([NyxField('mode', NyxData(CMode[LMode])),
        NyxField('job', NyxData(FJob.ID))]);
    end;
    FRequest := TJSXMLHttpRequest.new;
    FRequest.onreadystatechange := @Ready;
    FRequest.open('POST', 'api/agents/source', True);
    FRequest.timeout := FLimits.DeadlineMS;
    FRequest.setRequestHeader('Content-Type', 'application/json; charset=utf-8');
    FRequest.setRequestHeader('X-Nyx-Editor', FToken);
    FRequest.send(NyxWithWorkspace(NyxObject([NyxField('compile', LArguments)]),
      FWorkspace).ToJSON);
  except
    on LException: Exception do
    begin
      Fail(LException.Message);
    end;
  end;
end;

procedure THTTPCompilation.Ready;
var
  { The server may finish while the parent compiler releases this token. }
  LLease: INyxSourceCompilation;
  LStatus: Integer;
  LText: TNyxText;
  LValue: TNyxDataValue;
  LState: TNyxBuildJobState;
  LJob: TNyxBuildJobRef;
  LBuild: INyxSourceProjectionBuild;
begin

  if (FRequest = nil) or (FRequest.readyState <> 4) then
  begin
    Exit;
  end;
  LLease := Self;
  LStatus := FRequest.status;
  LText := FRequest.responseText;
  FRequest.onreadystatechange := nil;
  FRequest := nil;
  try

    if LStatus = 0 then
    begin
      { A failed observation is not a terminal server job or permission to
        resubmit under another identity. Re-poll/recover this exact operation. }
      FTimer := window.setTimeout(@Tick, 250);
      Exit;
    end;

    if NyxUTF8ByteCount(LText) > NyxProjectionMaximumResultBytes + 8192 then
    begin
      raise ENyxModel.Create('Source compiler response exceeds its byte budget');
    end;
    LValue := TNyxDataValue.ParseJSON(LText);

    if LStatus <> 200 then
    begin
      raise ENyxModel.Create('Source compiler refused: ' + LValue.Field('error').AsText);
    end;

    if NyxWorkspaceArgument(LValue).ID <> FWorkspace.ID then
    begin
      raise ENyxModel.Create('Source compiler reply belongs to another project');
    end;
    LJob := NyxBuildJob(LValue.Field('job').AsText);

    if (FJob.ID <> '') and (FJob.ID <> LJob.ID) then
    begin
      raise ENyxModel.Create('Source compiler reply substituted its owned job');
    end;
    FJob := LJob;
    FState := scsRunning;
    LState := ParseNyxBuildJobState(LValue.Field('state').AsText);

    if not NyxBuildJobTerminal(LState) then
    begin
      FTimer := window.setTimeout(@Tick, 100);
      Exit;
    end;

    if LState = bjsCancelled then
    begin
      LBuild := NewNyxSourceProjectionBuild(
        NyxSourceProjectionRef('cancelled-' + FJob.ID),
        NyxSourceProjectionFailure(FSource, btBrowser, spsCancelled,
          'The owned source compiler job was cancelled'), '', '');
    end
    else if LValue.Field('receipt').Kind = ndNull then
    begin
      raise ENyxModel.Create(LValue.Field('error').AsText);
    end
    else
    begin
      LBuild := DecodeNyxBrowserSourceBuild(FSource, LValue.Field('receipt'));
    end;
    Finish(LBuild);
  except
    on LException: Exception do
    begin
      Fail(LException.Message);
    end;
  end;
end;

function TSourceService.Compile(const ASource: TNyxText;
  const APort: INyxBrowserSourceBuildPort): INyxSourceCompilation;
var
  LOwner: THTTPCompilation;
  LID: TGUID;
begin
  ValidateNyxProjectionSource(ASource);

  if APort = nil then
  begin
    raise ENyxModel.Create('Source compilation requires its owned browser delivery port');
  end;
  CreateGUID(LID);
  LOwner := THTTPCompilation.Create;
  Result := LOwner;
  LOwner.FSource := ASource;
  LOwner.FToken := Token;
  LOwner.FWorkspace := Workspace;
  LOwner.FLimits := Limits;
  LOwner.FPort := APort;
  LOwner.FLease := Result;
  LOwner.FArguments := NyxObject([NyxField('mode', NyxData('request')),
    NyxField('operationId', NyxData('source-' + GUIDToString(LID))),
    NyxField('expectedRevision', NyxData(Revision)), NyxField('source', NyxData(ASource))]);
  LOwner.FDeadline := window.setTimeout(@LOwner.Expired, 150000);
  LOwner.Send;
end;

function NewNyxBrowserSourceService(const AEditorCapability: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; AExpectedRevision: Integer;
  const APolicy: INyxTransportPolicy): INyxBrowserSourceBuilder;
var
  LOwner: TSourceService;
begin

  if (NyxTextScalarCount(AEditorCapability) < 1) or
    (NyxTextScalarCount(AEditorCapability) > 512) or (AExpectedRevision < 1) then
  begin
    raise ENyxModel.Create('Source compilation needs a private editor capability and revision');
  end;
  LOwner := TSourceService.Create;
  Result := LOwner;
  LOwner.Token := AEditorCapability;
  LOwner.Workspace := AWorkspace;
  LOwner.Revision := AExpectedRevision;
  LOwner.Limits := NewNyxTransportPolicy.Snapshot;

  if APolicy <> nil then
  begin
    LOwner.Limits := APolicy.Snapshot;
  end;
  ValidateNyxTransportLimits(LOwner.Limits);
end;

end.
