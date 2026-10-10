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

uses nyx.text, nyx.data, nyx.studio.workspaces, nyx.studio.transport,
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

{ Explicit shared provider: captures the claimed issuer as well as capability/
  project/revision, and asks the server to retain precompile publication context.
  Use with the specialized shared execution contract; a local Apply completion
  must not silently discard the resulting server revision. Construction is idle. }
function NewNyxSharedBrowserSourceService(const AEditorCapability, AIssuer: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; AExpectedRevision: Integer;
  const APolicy: INyxTransportPolicy = nil): INyxBrowserSharedSourceBuilder;

{ Visual publication sends only a copied semantic intent beside the exact
  proposed source. The server constructs its own sealed proposal; this value
  transports neither an accepted document nor execution/admission authority. }
function NewNyxVisualSharedBrowserSourceService(const AEditorCapability, AIssuer: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; AExpectedRevision: Integer;
  const AIntent: TNyxDataValue;
  const APolicy: INyxTransportPolicy = nil): INyxBrowserSharedSourceBuilder;

implementation

uses SysUtils, Web, nyx.bytes, nyx.model, nyx.editing, nyx.studio.builds,
  nyx.studio.editorbuild,
  nyx.studio.session,
  nyx.studio.sourcebuilds, nyx.studio.sourceprojection,
  nyx.studio.sourcecompilation, nyx.studio.sourcepublications;

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
    Issuer: TNyxText;
    { Immutable request copy; null keeps the original Apply wire shape. }
    Intent: TNyxDataValue;
    function SharedPublication: Boolean; virtual;
    function Compile(const ASource: TNyxText;
      const APort: INyxBrowserSourceBuildPort): INyxSourceCompilation;
  end;
  TSharedSourceService = class(TSourceService, INyxBrowserSharedSourceBuilder)
  public
    function SharedPublication: Boolean; override;
    function Publish(const ABuild: INyxSourceProjectionBuild;
      const AProjection: INyxSourceProjection;
      const APort: INyxBrowserSourcePublicationPort): INyxSourceCompilation;
  end;
  { Owns exact completion bytes and a copied context. Lost HTTP acknowledgements
    retry the same producer result; the server returns its original committed
    receipt. Cancel detaches delivery, never promises to undo remote admission. }
  THTTPPublication = class(TInterfacedObject, INyxSourceCompilation)
  private
    FState: TNyxSourceCompilationState;
    FToken: TNyxText;
    FIssuer: TNyxText;
    FWorkspace: TNyxWorkspaceRef;
    FReference: TNyxSourceProjectionRef;
    FBody: TNyxText;
    FLimits: TNyxTransportLimits;
    FPort: INyxBrowserSourcePublicationPort;
    FLease: INyxSourceCompilation;
    FRequest: TJSXMLHttpRequest;
    FTimer: NativeInt;
    FDeadline: NativeInt;
    { Once an acknowledgement is lost, a later refusal cannot establish that
      the earlier request never committed (its retained handle may have expired). }
    FUnconfirmed: Boolean;
    procedure Retire;
    procedure Finish(const AReceipt: TNyxSourcePublicationReceipt;
      AOutcome: TNyxSourcePublicationOutcome = npoCommitted; const AMessage: TNyxText = '');
    procedure Send;
    procedure Ready;
    procedure Expired;
  public
    procedure Cancel;
    function GetState: TNyxSourceCompilationState;
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
  LFields: array of TNyxDataField;
  LIndex: Integer;
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

  if SharedPublication then
  begin
    LOwner.FArguments := NyxObject([
      NyxField('mode', LOwner.FArguments.Field('mode')),
      NyxField('operationId', LOwner.FArguments.Field('operationId')),
      NyxField('expectedRevision', LOwner.FArguments.Field('expectedRevision')),
      NyxField('source', LOwner.FArguments.Field('source')),
      NyxField('publish', NyxData(True)), NyxField('issuer', NyxData(Issuer))]);

    if Intent.Kind <> ndNull then
    begin
      SetLength(LFields, LOwner.FArguments.Count + 1);
      for LIndex := 0 to LOwner.FArguments.Count - 1 do
      begin
        LFields[LIndex] := NyxField(LOwner.FArguments.Key(LIndex),
          LOwner.FArguments.Field(LOwner.FArguments.Key(LIndex)));
      end;
      LFields[High(LFields)] := NyxField('visual', Intent.Copy);
      LOwner.FArguments := NyxObject(LFields);
    end;
  end;
  LOwner.FDeadline := window.setTimeout(@LOwner.Expired, 150000);
  LOwner.Send;
end;

function TSourceService.SharedPublication: Boolean;
begin
  Result := False;
end;

function TSharedSourceService.SharedPublication: Boolean;
begin
  Result := True;
end;

procedure THTTPPublication.Retire;
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

function THTTPPublication.GetState: TNyxSourceCompilationState;
begin
  Result := FState;
end;

procedure THTTPPublication.Cancel;
var
  LLease: INyxSourceCompilation;
begin
  LLease := Self;

  if not (FState in [scsPending, scsRunning]) then
  begin
    Exit;
  end;
  FState := scsCancelled;
  Retire;
  FPort := nil;
  FLease := nil;
end;

procedure THTTPPublication.Finish(const AReceipt: TNyxSourcePublicationReceipt;
  AOutcome: TNyxSourcePublicationOutcome; const AMessage: TNyxText);
var
  LLease: INyxSourceCompilation;
  LPort: INyxBrowserSourcePublicationPort;
begin
  LLease := Self;

  if not (FState in [scsPending, scsRunning]) then
  begin
    Exit;
  end;
  FState := scsCompleted;

  if AOutcome <> npoCommitted then
  begin
    FState := scsFailed;
  end;
  Retire;
  LPort := FPort;
  FPort := nil;
  FLease := nil;
  LPort.Complete(AOutcome, AReceipt, AMessage);
end;

procedure THTTPPublication.Expired;
begin
  Finish(Default(TNyxSourcePublicationReceipt), npoUnconfirmed,
    'Source publication acknowledgement timed out; remote admission may have completed');
end;

procedure THTTPPublication.Send;
begin
  FTimer := 0;

  if not (FState in [scsPending, scsRunning]) then
  begin
    Exit;
  end;
  try
    FState := scsRunning;
    FRequest := TJSXMLHttpRequest.new;
    FRequest.onreadystatechange := @Ready;
    FRequest.open('POST', 'api/agents/source', True);
    FRequest.timeout := FLimits.DeadlineMS;
    FRequest.setRequestHeader('Content-Type', 'application/json; charset=utf-8');
    FRequest.setRequestHeader('X-Nyx-Editor', FToken);
    FRequest.send(FBody);
  except
    on LException: Exception do
    begin
      Finish(Default(TNyxSourcePublicationReceipt), npoUnconfirmed, LException.Message);
    end;
  end;
end;

procedure THTTPPublication.Ready;
var
  LValue: TNyxDataValue;
  LText: TNyxText;
  LStatus: Integer;
  LReceipt: TNyxSourcePublicationReceipt;
begin

  if (FRequest = nil) or (FRequest.readyState <> 4) or
    not (FState in [scsPending, scsRunning]) then
  begin
    Exit;
  end;
  LText := FRequest.responseText;
  LStatus := FRequest.status;
  FRequest.onreadystatechange := nil;
  FRequest := nil;
  try

    if LStatus = 0 then
    begin
      FUnconfirmed := True;
      FTimer := window.setTimeout(@Send, 250);
      Exit;
    end;

    if NyxUTF8ByteCount(LText) > 8192 then
    begin
      raise ENyxModel.Create('Source publication acknowledgement exceeds its small response budget');
    end;
    LValue := TNyxDataValue.ParseJSON(LText);

    if LStatus <> 200 then
    begin
      raise ENyxModel.Create('Source publication refused: ' + LValue.Field('error').AsText);
    end;
    LReceipt := DecodeNyxSourcePublicationReceipt(LValue, FIssuer, FWorkspace, FReference);
    Finish(LReceipt);
  except
    on LException: Exception do
    begin

      if (LStatus = 200) or FUnconfirmed then
      begin
        Finish(Default(TNyxSourcePublicationReceipt), npoUnconfirmed, LException.Message);
      end
      else
      begin
        Finish(Default(TNyxSourcePublicationReceipt), npoRefused, LException.Message);
      end;
    end;
  end;
end;

function TSharedSourceService.Publish(const ABuild: INyxSourceProjectionBuild;
  const AProjection: INyxSourceProjection;
  const APort: INyxBrowserSourcePublicationPort): INyxSourceCompilation;
var
  LOwner: THTTPPublication;
  LDocument: TNyxDocument;
  LPacket: TNyxDataValue;
  LBody: TNyxText;
begin

  if (APort = nil) or (ABuild = nil) or (AProjection = nil) or
    (ABuild.Projection.State <> spsCompiled) or (ABuild.Projection.Target <> btBrowser) or
    (AProjection.State <> spsExecuted) or (AProjection.Target <> btBrowser) or
    (AProjection.Source <> ABuild.Projection.Source) then
  begin
    raise ENyxModel.Create('Shared source publication needs its exact compiled worker and live executed result');
  end;
  LDocument := AProjection.CopyDocument;
  try
    LPacket := CaptureNyxSourceProjection(LDocument, ABuild.Reference, btBrowser);

    if LPacket.Field('design').AsText <> AProjection.Design then
    begin
      raise ENyxModel.Create('Shared producer serialization changed its admitted meaning');
    end;
  finally
    LDocument.Free;
  end;
  LBody := NyxWithWorkspace(NyxObject([NyxField('compile', NyxObject([
    NyxField('mode', NyxData('complete')),
    NyxField('reference', NyxData(ABuild.Reference.Name)),
    NyxField('projection', NyxData(LPacket.ToJSON))]))]), Workspace).ToJSON;

  if NyxUTF8ByteCount(LBody) > 4 * 1024 * 1024 then
  begin
    raise ENyxModel.Create('Shared producer request exceeds its formatted transport budget');
  end;
  LOwner := THTTPPublication.Create;
  Result := LOwner;
  LOwner.FToken := Token;
  LOwner.FIssuer := Issuer;
  LOwner.FWorkspace := Workspace;
  LOwner.FReference := ABuild.Reference;
  LOwner.FBody := LBody;
  LOwner.FLimits := Limits;
  LOwner.FPort := APort;
  LOwner.FLease := Result;
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

function CreateSharedSourceService(const AEditorCapability, AIssuer: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; AExpectedRevision: Integer;
  const AIntent: TNyxDataValue;
  const APolicy: INyxTransportPolicy): INyxBrowserSharedSourceBuilder;
var
  LOwner: TSharedSourceService;
begin

  if (NyxTextScalarCount(AEditorCapability) < 1) or
    (NyxTextScalarCount(AEditorCapability) > 512) or (AExpectedRevision < 1) or
    (AIssuer = '') or (Length(AIssuer) > 128) then
  begin
    raise ENyxModel.Create('Shared source compilation needs its owning editor capability, issuer and revision');
  end;
  LOwner := TSharedSourceService.Create;
  Result := LOwner;
  LOwner.Token := AEditorCapability;
  LOwner.Issuer := AIssuer;
  LOwner.Workspace := AWorkspace;
  LOwner.Revision := AExpectedRevision;
  LOwner.Intent := AIntent.Copy;
  LOwner.Limits := NewNyxTransportPolicy.Snapshot;

  if APolicy <> nil then
  begin
    LOwner.Limits := APolicy.Snapshot;
  end;
  ValidateNyxTransportLimits(LOwner.Limits);
end;

function NewNyxSharedBrowserSourceService(const AEditorCapability, AIssuer: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; AExpectedRevision: Integer;
  const APolicy: INyxTransportPolicy): INyxBrowserSharedSourceBuilder;
begin
  Result := CreateSharedSourceService(AEditorCapability, AIssuer, AWorkspace,
    AExpectedRevision, NyxNull, APolicy);
end;

function NewNyxVisualSharedBrowserSourceService(const AEditorCapability, AIssuer: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; AExpectedRevision: Integer;
  const AIntent: TNyxDataValue;
  const APolicy: INyxTransportPolicy): INyxBrowserSharedSourceBuilder;
begin
  ReadNyxStudioDesignIntent(AIntent);
  Result := CreateSharedSourceService(AEditorCapability, AIssuer, AWorkspace,
    AExpectedRevision, AIntent, APolicy);
end;

end.
