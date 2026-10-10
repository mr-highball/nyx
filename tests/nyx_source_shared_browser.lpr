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
program nyx_source_shared_browser;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, JS, Web, nyx.text, nyx.data, nyx.schema, nyx.source,
  nyx.studio.projects, nyx.studio.session, nyx.studio.workspaces,
  nyx.studio.sourceprojection, nyx.studio.projectionediting,
  nyx.studio.sourceobservations, nyx.studio.sourcepublications,
  nyx.studio.sourcecompilation, nyx.studio.sourcecompilation.shared,
  nyx.studio.sourcecompilation.shared.browser,
  nyx.studio.sourcecompilation.service.browser, nyx.test.projection;

type
  { The fixture owns its sessions for the entire journey. This completion port
    receives the specialized shared result; it never presents a server revision
    as an ordinary local compiler acknowledgement. Production bridge reservation
    and retirement coordination remain a separate Studio integration requirement. }
  TSharedPort = class(TInterfacedObject, INyxSharedSourceCompilationPort)
    procedure Complete(AOutcome: TNyxSourcePublicationOutcome;
      const AProjection: INyxSourceProjection;
      const AReceipt: TNyxSourcePublicationReceipt; const AMessage: TNyxText = '');
  end;

var
  GRequest: TJSXMLHttpRequest;
  GSource: TNyxText;
  GToken: TNyxText;
  GIssuer: TNyxText;
  GRevision: Integer;
  GCommittedRevision: Integer;
  GChecks: Integer;
  GTimeout: NativeInt;
  GLocal: TNyxStudioSession;
  GObserver: TNyxStudioSession;
  GBefore: TNyxProjectPair;
  GHistoryBefore: TNyxProjectPair;
  GExpectedDesign: TNyxText;
  GVisual: Boolean;
  GDesignRequest: TNyxStudioDesignRequest;
  GDesignProposal: INyxPreparedDesign;
  GCaptured: TNyxStudioSourceRequest;
  GSchemas: INyxSchemaSnapshot;
  GOperation: INyxSourceCompilation;
  GCompiler: INyxSharedSourceCompiler;
  GPort: INyxSharedSourceCompilationPort;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Shared browser source: ' + AReason);
  end;
  Inc(GChecks);
end;

{ Terminal cleanup revokes delivery before freeing the fixture's independent
  owners. The generated worker/XHR operations own and retire their own leases. }
procedure Retire;
begin

  if GTimeout <> 0 then
  begin
    window.clearTimeout(GTimeout);
    GTimeout := 0;
  end;

  if GOperation <> nil then
  begin
    GOperation.Cancel;
    GOperation := nil;
  end;
  GCompiler := nil;
  GPort := nil;
  GDesignProposal := nil;
  GSchemas := nil;

  if GRequest <> nil then
  begin
    GRequest.onload := nil;
    GRequest.onerror := nil;
    GRequest.ontimeout := nil;
    GRequest.abort;
    GRequest := nil;
  end;
  FreeAndNil(GObserver);
  FreeAndNil(GLocal);
end;

procedure Failed(const AMessage: TNyxText);
begin
  Retire;
  document.body.setAttribute('data-source-shared', 'fail');
  document.body.textContent := 'FAIL ' + AMessage;
end;

procedure Expired;
begin
  Failed('Owned worker/publication/observation did not reach terminal within the fixture budget');
end;

function NetworkFailed(AEvent: TJSProgressEvent): Boolean;
begin
  Result := False;
  Failed('Owned private HTTP exchange did not arrive');
end;

{ The first executed/observed pair supplies the local opaque baseline. Only the
  semantic edit travels to the owning server, which reconstructs its own exact
  proposal before its second actual browser compilation/worker invocation. }
procedure StartVisual;
var
  LEdit: TNyxStudioDesignEdit;
begin
  GHistoryBefore := GLocal.ProjectSnapshot;
  LEdit := Default(TNyxStudioDesignEdit);
  LEdit.Action := sdaProperty;
  LEdit.Selection := 'heading-1';
  LEdit.View := 'notebook-1';
  LEdit.Name := 'text';
  LEdit.Value := 'A crafted shared heading 🚀 𐐷';
  GDesignRequest := GLocal.PrepareDesignRequest(LEdit, GSchemas.Revision);
  GDesignProposal := PrepareNyxStudioDesign(GDesignRequest, GSchemas);
  Check(GDesignProposal.RequiresCompilation and not GDesignProposal.Diagnostic.Defined,
    'shared visual edit prepares an opaque local compiler proposal');
  GVisual := True;
  GRevision := GCommittedRevision;
  GSource := GDesignProposal.Source;
  GExpectedDesign := GDesignProposal.Design;
  GCompiler := NewNyxSharedBrowserSourceCompiler(NewNyxVisualSharedBrowserSourceService(
    GToken, GIssuer, NyxPrimaryWorkspace, GRevision, GDesignRequest.IntentData));
  GOperation := GCompiler.Start(GSource, GPort);
  Check(GOperation <> nil, 'shared visual continuation starts its own compiler/worker lifetime');
end;

function Observed(AEvent: TJSProgressEvent): Boolean;
var
  LState: TNyxDataValue;
  LPair: TNyxProjectPair;
  LFrame: TNyxSourceCheckpoint;
begin
  Result := False;
  try
    Check(GRequest.status = 200, 'actual private observation succeeds');
    LState := TNyxDataValue.ParseJSON(GRequest.responseText);
    LPair := DecodeNyxProject(LState.Field('project').AsText);
    LFrame := ReceiveNyxSourceObservation(LState.Field('sourceObservation'),
      GIssuer, NyxPrimaryWorkspace, GCommittedRevision, LPair);
    Check((LState.Field('session').Field('revision').AsInteger = GCommittedRevision) and
      (LFrame.Origin = nsoExecuted), 'observed executed pair matches exact committed revision');
    if GObserver = nil then
    begin
      GObserver := TNyxStudioSession.Create(GBefore);
    end;
    GObserver.AdoptCapturedProject(LPair, LFrame);
    Check((GObserver.Save = GExpectedDesign) and (GObserver.Source = GSource) and
      (GObserver.Document <> GLocal.Document), 'independent observer owns exact executed meaning');
    GObserver.Undo;
    Check((GObserver.Source = GHistoryBefore.Source) and
      (GObserver.Save = GHistoryBefore.Design),
      'observing Undo restores earlier whole pair');
    GObserver.Redo;
    Check((GObserver.Source = GSource) and
      (GObserver.AcceptedSourceCheckpoint.Origin = nsoExecuted), 'observing Redo retains execution provenance');

    if not GVisual then
    begin
      StartVisual;
      Exit;
    end;
    Retire;
    document.body.setAttribute('data-source-shared', 'pass');
    document.body.setAttribute('data-source-shared-checks', IntToStr(GChecks));
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' actual shared worker/HTTP/observation checks';
  except
    on LException: Exception do
    begin
      Failed(LException.Message);
    end;
  end;
end;

procedure Observe;
begin
  GRequest := TJSXMLHttpRequest.new;
  GRequest.open('POST', 'api/agents/editor', True);
  GRequest.timeout := 10000;
  GRequest.setRequestHeader('Content-Type', 'application/json; charset=utf-8');
  GRequest.setRequestHeader('X-Nyx-Editor', GToken);
  GRequest.onload := @Observed;
  GRequest.onerror := @NetworkFailed;
  GRequest.ontimeout := @NetworkFailed;
  GRequest.send(NyxObject([NyxField('op', NyxData('observe')),
    NyxField('after', NyxData(GCommittedRevision - 1))]).ToJSON);
end;

procedure TSharedPort.Complete(AOutcome: TNyxSourcePublicationOutcome;
  const AProjection: INyxSourceProjection;
  const AReceipt: TNyxSourcePublicationReceipt; const AMessage: TNyxText);
begin
  try
    Check(AOutcome = npoCommitted, 'owning backend commits actual worker result: ' + AMessage);
    Check((AProjection <> nil) and (AProjection.State = spsExecuted) and
      (AProjection.Source = GSource) and (AProjection.Design = GExpectedDesign),
      'complete handwritten constructor executes in its owned browser worker');
    Check((AReceipt.Issuer = GIssuer) and (AReceipt.Workspace.ID = NyxPrimaryWorkspace.ID) and
      (AReceipt.Revision = GRevision + 1), 'typed receipt names the exact originating context');
    if GVisual then
    begin
      Check(GLocal.CompleteDesignRequest(GDesignRequest,
        PrepareNyxCompiledDesign(GDesignRequest, GDesignProposal, AProjection, GSchemas)) = nscApplied,
        'local visual admission verifies exact compiled source and whole proposed meaning');
    end
    else
    begin
      Check(GLocal.CompleteSourceRequest(GCaptured, PrepareNyxProjectedSource(AProjection, GSchemas)) = nscApplied,
        'local paired admission uses the original source request and creator capture');
    end;
    Check((GLocal.Save = GExpectedDesign) and (GLocal.Source = GSource) and
      not GLocal.SourceDraftPending, 'local accepted source and model match the backend completion');
    GLocal.Undo;
    Check((GLocal.Save = GHistoryBefore.Design) and (GLocal.Source = GHistoryBefore.Source),
      'local result is one ordinary paired Undo');
    GLocal.Redo;
    Check(GLocal.Source = GSource, 'local paired Redo restores exact full source');
    GCommittedRevision := AReceipt.Revision;
    Observe;
  except
    on LException: Exception do
    begin
      Failed(LException.Message);
    end;
  end;
end;

function Connected(AEvent: TJSProgressEvent): Boolean;
var
  LClaim: TNyxDataValue;
begin
  Result := False;
  try
    Check(GRequest.status = 200, 'owned editor claims same-origin authority');
    LClaim := TNyxDataValue.ParseJSON(GRequest.responseText);
    Check(LClaim.Field('state').Field('sharedSourcePublication').AsBoolean,
      'current server advertises opt-in worker publication');
    Check(LClaim.Field('state').Field('sharedVisualSourcePublication').AsBoolean,
      'current server advertises independent semantic visual publication');
    GToken := LClaim.Field('token').AsText;
    GIssuer := LClaim.Field('state').Field('sourceObservationIssuer').AsText;
    GRevision := LClaim.Field('state').Field('session').Field('revision').AsInteger;
    GSchemas := CaptureNyxSchemas;
    GCaptured := GLocal.PrepareSourceRequest(GSchemas.Revision);
    GCompiler := NewNyxSharedBrowserSourceCompiler(NewNyxSharedBrowserSourceService(
      GToken, GIssuer, NyxPrimaryWorkspace, GRevision));
    GPort := TSharedPort.Create;
    GOperation := GCompiler.Start(GSource, GPort);
    Check(GOperation <> nil, 'shared compilation starts one owned operation');
  except
    on LException: Exception do
    begin
      Failed(LException.Message);
    end;
  end;
end;

function Loaded(AEvent: TJSProgressEvent): Boolean;
begin
  Result := False;
  try
    Check(GRequest.status = 200, 'owned source input arrives over HTTP');
    { Differ from any staged producer artifact: this exact unit must be compiled
      by the private service during this journey, then executed in a new worker. }
    GSource := GRequest.responseText + #10 + '{ Fresh shared browser publication. }' + #10;
    GLocal := TNyxStudioSession.Create;
    GLocal.SetSourceDraft(GSource);
    GBefore := GLocal.ProjectSnapshot;
    GHistoryBefore := GBefore;
    GExpectedDesign := ExpectedNyxProjectionDesign;
    GRequest := TJSXMLHttpRequest.new;
    GRequest.open('POST', 'api/agents/connect', True);
    GRequest.timeout := 10000;
    GRequest.setRequestHeader('Content-Type', 'application/json; charset=utf-8');
    GRequest.onload := @Connected;
    GRequest.onerror := @NetworkFailed;
    GRequest.ontimeout := @NetworkFailed;
    GRequest.send(NyxObject([NyxField('op', NyxData('claim')),
      NyxField('project', NyxData(EncodeNyxProject(GBefore))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]).ToJSON);
  except
    on LException: Exception do
    begin
      Failed(LException.Message);
    end;
  end;
end;

begin
  document.body.setAttribute('data-source-shared', 'pending');
  GTimeout := window.setTimeout(@Expired, 240000);
  GRequest := TJSXMLHttpRequest.new;
  GRequest.open('GET', 'source.pas', True);
  GRequest.timeout := 10000;
  GRequest.onload := @Loaded;
  GRequest.onerror := @NetworkFailed;
  GRequest.ontimeout := @NetworkFailed;
  GRequest.send;
end.
