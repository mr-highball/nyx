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

unit nyx.studio.completion;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.data, nyx.pascal.completion, nyx.studio.exchange,
  nyx.studio.workspaces;

type
  { Managed UI-thread receiver owns its dependencies. A delivery is detached
    before invocation, so it may request/cancel another query. Schedule owner/
    client destruction after callback return; the exchange may still be dispatching. }
  INyxStudioCompletionReply = interface(IInterface)
    ['{65FB0C4E-156E-4C85-B580-AB01288A9E01}']
    procedure Receive(const AQuery: INyxPascalCompletionRequest;
      const AResult: INyxPascalCompletionResult);
  end;

  { Local editor readiness is independent of agent permission and selected
    output. Synchronizing may be retried after ordinary acknowledgement; offline
    and unavailable are explicit feedback, never permission to send stale text. }
  TNyxStudioCompletionReadiness = (scrSubmitted, scrSynchronizing, scrOffline, scrUnavailable);
  { Revocable UI-thread service borrowed through a managed lease. The bridge owns
    transport/session authority; this port owns neither. Requests contain only
    immutable inspection context, and Current requires exact acknowledged buffers.
    Cancel revokes delivery, not admitted analysis. Detach permanently retires the
    borrow before its bridge/session disappears; retained ports then refuse. }
  INyxStudioCompletionContext = interface(IInterface)
    ['{4847558D-A467-44CA-B8C8-59A435EF9D01}']
    function Request(const AQuery: INyxPascalCompletionRequest;
      const AReply: INyxStudioCompletionReply): TNyxStudioCompletionReadiness;
    function Current(const AQuery: INyxPascalCompletionRequest): Boolean;
    procedure Cancel;
    procedure Detach;
  end;

  { Owns one private asynchronous editor exchange. No UI/session/document is
    borrowed. Request captures immutable query data and a managed receiver;
    Cancel revokes local delivery but cannot cancel admitted server analysis.
    The embedding Studio owner must require its exact acknowledged source frame
    and revision before requesting, and recheck its mount/current buffers/caret
    before presenting. This transport grants no source publication authority. }
  TNyxStudioCompletionClient = class
  private
    FExchange: TNyxStudioEditorExchange;
    FQuery: INyxPascalCompletionRequest;
    FReceiver: INyxStudioCompletionReply;
    FWorkspace: TNyxWorkspaceRef;
    FRevision: Integer;
    FID: TNyxText;
    FSerial: Integer;
    procedure Reply(AStatus: Integer; const AText: TNyxText);
  public
    { Takes ownership of the supplied exchange. Its private endpoint/configuration
      remains outside all designs and query packets. Nil refuses. }
    constructor Create(AExchange: TNyxStudioEditorExchange);
    destructor Destroy; override;
    { UI-thread submission supersedes pending local delivery after validating all
      inputs. The caller must already acknowledge the query's complete sources.
      Private transport authority is retained by the exchange, never the packet. }
    procedure Request(const AToken: TNyxText; const AWorkspace: TNyxWorkspaceRef;
      ARevision: Integer; const AQuery: INyxPascalCompletionRequest;
      const AReceiver: INyxStudioCompletionReply);
    { Revoke local delivery and release its receiver/query. An already admitted
      server read may finish independently; this grants no server cancellation. }
    procedure Cancel;
    { True until the owned pending delivery completes or is explicitly revoked. }
    function Busy: Boolean;
  end;

{ Private editor request contains selectors/caret/pages, never supplied source or
  executable paths. The server reconstructs all acknowledged files itself.
  The caller supplies the ordinary project's acknowledged exact revision. }
function NyxStudioCompletionRequest(const AQuery: INyxPascalCompletionRequest;
  ARevision: Integer; const AID: TNyxText;
  const AWorkspace: TNyxWorkspaceRef): TNyxDataValue;
{ Strict server-side boundary. Operator authority comes from the authenticated
  transport, never an input field. Returns ordinary source query arguments. }
function NyxStudioCompletionArguments(const ARequest: TNyxDataValue;
  out AID: TNyxText): TNyxDataValue;
{ Convert the already independently admitted bounded semantic page into the
  strict shared result codec. No complete source/input snapshot is returned. }
function NyxStudioCompletionEnvelope(const APage: TNyxDataValue;
  const AID: TNyxText; const AWorkspace: TNyxWorkspaceRef): TNyxDataValue; overload;
{ Private operator handoff carries the already admitted frozen result directly
  into the shared reply codec. Public semantic page fields remain unchanged. }
function NyxStudioCompletionEnvelope(const APage: TNyxDataValue;
  const AID: TNyxText; const AWorkspace: TNyxWorkspaceRef;
  const AResult: INyxPascalCompletionResult): TNyxDataValue; overload;
{ Require exact request/project/revision correlation before the shared bounded
  result decoder admits replacement ranges against the caller's frozen sources. }
function ReadNyxStudioCompletionEnvelope(const AEnvelope: TNyxDataValue;
  const AID: TNyxText; const AWorkspace: TNyxWorkspaceRef; ARevision: Integer;
  const AQuery: INyxPascalCompletionRequest): INyxPascalCompletionResult;

implementation

uses SysUtils, nyx.editing, nyx.pascal.project, nyx.pascal.sources,
  nyx.pascal.drafts, nyx.pascal.completion.wire, nyx.studio.agents;

procedure CheckID(const AID: TNyxText);
begin

  if (AID = '') or (NyxTextScalarCount(AID) > 120) then
  begin
    raise ENyxPascalProject.Create('Completion request identity requires 1..120 Unicode scalars');
  end;
end;

function FileData(const AFile: TNyxPascalSourceRef): TNyxDataValue;
const
  CKinds: array[TNyxPascalSourceRole] of TNyxText = ('main', 'unit', 'include');
begin
  AFile.Validate;

  if AFile.Role = npsMain then
  begin
    Exit(NyxObject([NyxField('kind', NyxData('main'))]));
  end;
  Result := NyxObject([NyxField('kind', NyxData(CKinds[AFile.Role])),
    NyxField('name', NyxData(AFile.Name))]);
end;

function NyxStudioCompletionRequest(const AQuery: INyxPascalCompletionRequest;
  ARevision: Integer; const AID: TNyxText;
  const AWorkspace: TNyxWorkspaceRef): TNyxDataValue;
const
  COrigins: array[TNyxPascalSourceSnapshotKind] of TNyxText = ('accepted', 'editing');
var
  LQuery: INyxPascalCompletionRequest;
begin
  CheckID(AID);

  if ARevision < 1 then
  begin
    raise ENyxPascalProject.Create('Completion requires its acknowledged revision');
  end;
  LQuery := CaptureNyxPascalCompletionRequest(AQuery);
  Result := NyxWithWorkspace(NyxObject([
    NyxField('op', NyxData('completion')), NyxField('requestId', NyxData(AID)),
    NyxField('expectedRevision', NyxData(ARevision)),
    NyxField('snapshot', NyxData(COrigins[LQuery.Sources.Kind])),
    NyxField('file', FileData(LQuery.FileReference)), NyxField('owner', FileData(LQuery.Owner)),
    NyxField('caret', NyxData(LQuery.Caret)), NyxField('offset', NyxData(LQuery.Offset)),
    NyxField('limit', NyxData(LQuery.Limit))]), AWorkspace);
end;

function NyxStudioCompletionArguments(const ARequest: TNyxDataValue;
  out AID: TNyxText): TNyxDataValue;
var
  LAt: Integer;
  LCount: Integer;
  LName: TNyxText;
  LID: TNyxText;
  LFields: array of TNyxDataField;
begin
  AID := '';
  NyxAgentFields(ARequest,
    '|op|requestId|expectedRevision|snapshot|file|owner|caret|offset|limit|workspace|');

  if ARequest.Field('op').AsText <> 'completion' then
  begin
    raise ENyxPascalProject.Create('Expected the private editor completion operation');
  end;
  LID := ARequest.Field('requestId').AsText;
  CheckID(LID);
  SetLength(LFields, ARequest.Count - 1);
  LCount := 1;
  LFields[0] := NyxField('mode', NyxData('complete'));
  for LAt := 0 to ARequest.Count - 1 do
  begin
    LName := ARequest.Key(LAt);

    if (LName <> 'op') and (LName <> 'requestId') then
    begin
      LFields[LCount] := NyxField(LName, ARequest.Field(LName));
      Inc(LCount);
    end;
  end;
  SetLength(LFields, LCount);
  Result := NyxObject(LFields);
  AID := LID;
end;

function NyxStudioCompletionEnvelope(const APage: TNyxDataValue;
  const AID: TNyxText; const AWorkspace: TNyxWorkspaceRef): TNyxDataValue;
var
  LItems: TNyxDataValue;
  LItem: TNyxDataValue;
  LRows: array of TNyxDataValue;
  LAt: Integer;
  LWire: TNyxDataValue;
begin
  CheckID(AID);
  LItems := APage.Field('items');

  if (LItems.Kind <> ndArray) or (LItems.Count > 50) then
  begin
    raise ENyxPascalProject.Create('Completion envelope exceeds its row budget');
  end;
  SetLength(LRows, LItems.Count);
  for LAt := 0 to High(LRows) do
  begin
    LItem := LItems.Item(LAt);
    LRows[LAt] := NyxObject([NyxField('name', LItem.Field('name')),
      NyxField('insert', LItem.Field('insert')), NyxField('detail', LItem.Field('detail')),
      NyxField('kind', LItem.Field('kind'))]);
  end;
  LWire := NyxObject([NyxField('version', NyxData(1)), NyxField('id', NyxData(AID)),
    NyxField('status', APage.Field('status')), NyxField('message', APage.Field('message')),
    NyxField('start', APage.Field('replacement').Field('offset')),
    NyxField('expected', APage.Field('replacement').Field('expected')),
    NyxField('total', APage.Field('total')), NyxField('items', NyxArray(LRows))]);
  Result := NyxObject([NyxField('op', NyxData('completion')),
    NyxField('requestId', NyxData(AID)), NyxField('revision', APage.Field('revision')),
    NyxField('workspace', NyxData(AWorkspace.ID)), NyxField('page', LWire)]);
end;

function NyxStudioCompletionEnvelope(const APage: TNyxDataValue;
  const AID: TNyxText; const AWorkspace: TNyxWorkspaceRef;
  const AResult: INyxPascalCompletionResult): TNyxDataValue;
var
  LWire: TNyxDataValue;
begin
  CheckID(AID);
  LWire := TNyxDataValue.ParseJSON(EncodeNyxCompletionReply(AResult, AID));
  Result := NyxObject([NyxField('op', NyxData('completion')),
    NyxField('requestId', NyxData(AID)), NyxField('revision', APage.Field('revision')),
    NyxField('workspace', NyxData(AWorkspace.ID)), NyxField('page', LWire)]);
end;

function ReadNyxStudioCompletionEnvelope(const AEnvelope: TNyxDataValue;
  const AID: TNyxText; const AWorkspace: TNyxWorkspaceRef; ARevision: Integer;
  const AQuery: INyxPascalCompletionRequest): INyxPascalCompletionResult;
begin
  CheckID(AID);
  NyxAgentFields(AEnvelope, '|op|requestId|revision|workspace|page|');

  if (AEnvelope.Field('op').AsText <> 'completion') or
    (AEnvelope.Field('requestId').AsText <> AID) or
    (AEnvelope.Field('revision').AsInteger <> ARevision) or
    (AEnvelope.Field('workspace').AsText <> AWorkspace.ID) then
  begin
    raise ENyxPascalProject.Create('Completion response belongs to a different operator request');
  end;
  Result := DecodeNyxCompletionReply(AEnvelope.Field('page').ToJSON, AID, AQuery);
end;

constructor TNyxStudioCompletionClient.Create(AExchange: TNyxStudioEditorExchange);
begin
  inherited Create;

  if AExchange = nil then
  begin
    raise ENyxPascalProject.Create('Completion client requires its owned editor exchange');
  end;
  FExchange := AExchange;
end;

destructor TNyxStudioCompletionClient.Destroy;
begin
  Cancel;
  FExchange.Free;
  inherited Destroy;
end;

function TNyxStudioCompletionClient.Busy: Boolean;
begin
  Result := FQuery <> nil;
end;

procedure TNyxStudioCompletionClient.Cancel;
begin
  FReceiver := nil;
  FQuery := nil;
  FID := '';

  if FExchange <> nil then
  begin
    FExchange.CancelRequest;
  end;
end;

procedure TNyxStudioCompletionClient.Request(const AToken: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; ARevision: Integer;
  const AQuery: INyxPascalCompletionRequest; const AReceiver: INyxStudioCompletionReply);
var
  LQuery: INyxPascalCompletionRequest;
  LID: TNyxText;
  LPacket: TNyxDataValue;
begin

  if (AToken = '') or (AReceiver = nil) or (FSerial = High(Integer)) then
  begin
    raise ENyxPascalProject.Create('Completion requires private authority, a receiver and available request identity');
  end;
  LQuery := CaptureNyxPascalCompletionRequest(AQuery);
  LID := 'editor-completion-' + IntToStr(FSerial + 1);
  LPacket := NyxStudioCompletionRequest(LQuery, ARevision, LID, AWorkspace);
  Cancel;
  Inc(FSerial);
  FQuery := LQuery;
  FReceiver := AReceiver;
  FWorkspace := AWorkspace;
  FRevision := ARevision;
  FID := LID;
  try
    FExchange.Post(False, AToken, LPacket.ToJSON, Reply);
  except
    Cancel;
    raise;
  end;
end;

procedure TNyxStudioCompletionClient.Reply(AStatus: Integer; const AText: TNyxText);
var
  LReceiver: INyxStudioCompletionReply;
  LQuery: INyxPascalCompletionRequest;
  LResult: INyxPascalCompletionResult;
begin

  if FQuery = nil then
  begin
    Exit;
  end;
  LQuery := FQuery;
  LReceiver := FReceiver;
  try

    if AStatus <> 200 then
    begin
      raise ENyxPascalProject.Create('Completion transport refused');
    end;
    LResult := ReadNyxStudioCompletionEnvelope(TNyxDataValue.ParseJSON(AText),
      FID, FWorkspace, FRevision, LQuery);
  except
    on E: Exception do
    begin
      LResult := NewNyxPascalCompletionResult(LQuery, pcsFailed,
        'The editor completion service could not return a valid page', 0, '', [], 0);
    end;
  end;
  FReceiver := nil;
  FQuery := nil;
  FID := '';
  { Do not touch client fields after delivery; a receiver may request/cancel
    another operation. Destruction must still be queued after transport return. }
  LReceiver.Receive(LQuery, LResult);
end;

end.
