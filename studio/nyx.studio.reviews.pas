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
unit nyx.studio.reviews;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.studio.agents, nyx.studio.projects;

type
  { Session identity, separate from a design root or application-defined name.
    An empty value explicitly denotes the active user workspace. Review IDs are
    never reused during this owner lifetime; knowing an ID grants no permission. }
  TNyxReviewRef = record
  private
    FID: TNyxText;
  public
    property ID: TNyxText read FID;
  end;
  TNyxReviewBase = (nrbEmpty, nrbAccepted);

  { Owns at most eight independent ordinary Studio sessions. The active session
    is borrowed and must outlive this manager. No child borrows its document,
    Pascal workspace, history or nodes. Transports serialize ALL methods under
    their existing model lock; returned session pointers are borrowed only until
    that lock is released. Workers receive immutable pairs, never these pointers.
    A transport identity, rather than its editable display name, owns each review. }
  TNyxReviewWorkspaces = class
  private
    FActive: TNyxAgentSession;
    FSerial: Integer;
    FEntries: array of record
      Reference: TNyxReviewRef;
      Owner: TNyxText;
      Actor: TNyxText;
      LabelText: TNyxText;
      Session: TNyxAgentSession;
    end;
    { At most sixty-four immutable lifecycle receipts per authenticated transport
      retain exact retry identity through disposal. Creation reserves cleanup
      capacity; receipts never evict while that connection is alive. This avoids
      re-creating a workspace from an old delivery retry at an unchanged active
      revision. The HTTP owner admits at most sixty-four live transports. }
    FReceipts: array of record
      Owner: TNyxText;
      Operation: TNyxText;
      Request: TNyxText;
      Result: TNyxDataValue;
    end;
    function Index(const ARef: TNyxReviewRef): Integer;
    function Describe(AIndex: Integer): TNyxDataValue;
    procedure RequireAccess(AEdit: Boolean);
    procedure Remember(const AOwner, AOperation, ARequest: TNyxText;
      const AResult: TNyxDataValue);
  public
    constructor Create(AActive: TNyxAgentSession);
    { Trusted host rollback may atomically replace the primary owner under the
      transport lock. Review sessions stay independent and retain their owners;
      rebind before disposing the former borrowed primary. No wire authority. }
    procedure RebindActive(AActive: TNyxAgentSession);
    destructor Destroy; override;
    { Typed creation copies either an empty document or the exact accepted pair
      at AExpected. A user's pending draft stays solely in the active workspace.
      Creation/disposal never changes its revision, selection or history. }
    function CreateReview(const AOwner, AActor, ALabel: TNyxText;
      ABase: TNyxReviewBase; AExpected: Integer): TNyxReviewRef;
    procedure Discard(const AOwner: TNyxText; const ARef: TNyxReviewRef;
      AExpected: Integer);
    { Authenticated transport teardown retires its ephemeral owners even when
      agent access is disabled. It cannot release any other client's review. }
    procedure ReleaseOwner(const AOwner: TNyxText);
    { Strict semantic lifecycle boundary: list/inspect/create/discard. Creation
      guards the active revision; disposal guards that review's revision. }
    function Manage(const AOwner, AActor: TNyxText;
      const AArguments: TNyxDataValue): TNyxDataValue;
    { Resolve before reading or editing. Foreign/retired/empty supplied handles
      refuse; there is no fallback to the user's document. Permission is inherited
      from the operator on every call. An absent routing field denotes active. }
    function Resolve(const AOwner: TNyxText; const ARef: TNyxReviewRef): TNyxAgentSession;
    { The trusted bounded connection owner authorizes session receipts and
      removal reviews on both primary and private review routes. AActor remains
      visible activity text; identical/renamed labels never retarget authority. }
    function Call(const ATool, AOwner, AActor: TNyxText;
      const AArguments: TNyxDataValue): TNyxDataValue;
    { Trusted operator presentation is bounded to eight summaries. It contains
      neither transport owner IDs nor document/source/draft dumps. }
    function Observe: TNyxDataValue;
    { Completion routing remains valid independently of creator disconnection.
      Nil means the review has retired; callers mark that result stale and never
      publish its compiler report into the active user's workspace. }
    function Find(const ARef: TNyxReviewRef): TNyxAgentSession;
  end;

function NyxReview(const AID: TNyxText): TNyxReviewRef;
function NyxActiveWorkspace: TNyxReviewRef;
{ Exact immutable object copies at the routing boundary. Only the outer routing
  key is removed; nested semantic operation fields retain strict admission. }
function NyxReviewArguments(const AArguments: TNyxDataValue): TNyxDataValue;
function NyxReviewArgument(const AArguments: TNyxDataValue): TNyxReviewRef;
function NyxWithReview(const AResult: TNyxDataValue;
  const ARef: TNyxReviewRef): TNyxDataValue;

implementation

uses
  nyx.model, nyx.codec, nyx.codegen, nyx.editing;

procedure TNyxReviewWorkspaces.RebindActive(AActive: TNyxAgentSession);
begin

  if AActive = nil then
  begin
    raise ENyxModel.Create('A review manager requires an active session');
  end;
  FActive := AActive;
end;

const
  CSeparator: TNyxText = ' · ';
  CCompleted: TNyxText = ' · completed';
  CRefused: TNyxText = ' · refused';

function NyxActiveWorkspace: TNyxReviewRef;
begin
  Result.FID := '';
end;

function NyxReview(const AID: TNyxText): TNyxReviewRef;
begin

  if (AID = '') or (Length(AID) > 120) then
  begin
    raise ENyxModel.Create('A supplied review reference must contain 1..120 characters');
  end;
  Result.FID := AID;
end;

function NyxReviewArgument(const AArguments: TNyxDataValue): TNyxReviewRef;
begin
  Result := NyxActiveWorkspace;

  if NyxAgentHas(AArguments, 'review') then
  begin
    Result := NyxReview(AArguments.Field('review').AsText);
  end;
end;

function NyxReviewArguments(const AArguments: TNyxDataValue): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin
  { Validate the supplied reference before removing it, including wrong JSON
    types and empty strings. Never silently reinterpret them as active context. }
  NyxReviewArgument(AArguments);
  LFields := nil;
  for LIndex := 0 to AArguments.Count - 1 do
  begin

    if AArguments.Key(LIndex) <> 'review' then
    begin
      SetLength(LFields, Length(LFields) + 1);
      LFields[High(LFields)] := NyxField(AArguments.Key(LIndex),
        AArguments.Field(AArguments.Key(LIndex)));
    end;
  end;
  Result := NyxObject(LFields);
end;

function NyxWithReview(const AResult: TNyxDataValue;
  const ARef: TNyxReviewRef): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin
  Result := AResult;

  if ARef.ID = '' then
  begin
    Exit;
  end;
  SetLength(LFields, AResult.Count + 1);
  for LIndex := 0 to AResult.Count - 1 do
  begin
    LFields[LIndex] := NyxField(AResult.Key(LIndex), AResult.Field(AResult.Key(LIndex)));
  end;
  LFields[High(LFields)] := NyxField('review', NyxData(ARef.ID));
  Result := NyxObject(LFields);
end;

constructor TNyxReviewWorkspaces.Create(AActive: TNyxAgentSession);
begin
  inherited Create;

  if AActive = nil then
  begin
    raise ENyxModel.Create('Review workspaces require their active session owner');
  end;
  FActive := AActive;
end;

destructor TNyxReviewWorkspaces.Destroy;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FEntries) do
  begin
    FEntries[LIndex].Session.Free;
  end;
  FActive := nil;
  inherited Destroy;
end;

procedure TNyxReviewWorkspaces.RequireAccess(AEdit: Boolean);
begin

  if FActive.Permission = apDisabled then
  begin
    raise ENyxModel.Create('Agent access is disabled in Studio');
  end;

  if AEdit and (FActive.Permission <> apEdit) then
  begin
    raise ENyxModel.Create('Review edits require Allow edits in Studio');
  end;
end;

function TNyxReviewWorkspaces.Index(const ARef: TNyxReviewRef): Integer;
var
  LIndex: Integer;
begin
  Result := -1;
  for LIndex := 0 to High(FEntries) do
  begin

    if FEntries[LIndex].Reference.ID = ARef.ID then
    begin
      Exit(LIndex);
    end;
  end;
end;

function TNyxReviewWorkspaces.Find(const ARef: TNyxReviewRef): TNyxAgentSession;
var
  LIndex: Integer;
begin
  Result := FActive;

  if ARef.ID = '' then
  begin
    Exit;
  end;
  LIndex := Index(ARef);
  Result := nil;

  if LIndex >= 0 then
  begin
    Result := FEntries[LIndex].Session;
    Result.InheritPermission(FActive.Permission);
  end;
end;

function TNyxReviewWorkspaces.Resolve(const AOwner: TNyxText;
  const ARef: TNyxReviewRef): TNyxAgentSession;
var
  LIndex: Integer;
begin
  RequireAccess(False);

  if ARef.ID = '' then
  begin
    Exit(FActive);
  end;
  LIndex := Index(ARef);

  if (LIndex < 0) or (FEntries[LIndex].Owner <> AOwner) then
  begin
    raise ENyxModel.Create('Review is missing, retired or owned by another transport session');
  end;
  Result := FEntries[LIndex].Session;
  Result.InheritPermission(FActive.Permission);
end;

function TNyxReviewWorkspaces.CreateReview(const AOwner, AActor, ALabel: TNyxText;
  ABase: TNyxReviewBase; AExpected: Integer): TNyxReviewRef;
var
  LPair: TNyxProjectPair;
  LDocument: TNyxDocument;
  LSession: TNyxAgentSession;
  LIndex: Integer;
begin
  RequireAccess(True);

  if (AOwner = '') or (Length(ALabel) > 256) or (ALabel = '') then
  begin
    raise ENyxModel.Create('Review requires a transport owner and a 1..256-character label');
  end;

  if (Length(FEntries) >= 8) or (FSerial = High(Integer)) then
  begin
    raise ENyxModel.Create('Review workspace budget reached; dispose an owned review');
  end;

  if AExpected <> FActive.Revision then
  begin
    raise ENyxModel.Create('Review creation revision conflict; inspect the active session');
  end;

  if ABase = nrbEmpty then
  begin
    LDocument := TNyxDocument.Create;
    try
      LPair := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
    finally
      LDocument.Free;
    end;
  end
  else
  begin
    LPair := FActive.ReviewSeed(AExpected);
  end;
  LSession := TNyxAgentSession.Create(LPair);
  try
    Result := NyxReview('review-' + IntToStr(FSerial + 1));
    LIndex := Length(FEntries);
    SetLength(FEntries, LIndex + 1);
    FEntries[LIndex].Reference := Result;
    FEntries[LIndex].Owner := AOwner;
    FEntries[LIndex].Actor := AActor;
    FEntries[LIndex].LabelText := ALabel;
    FEntries[LIndex].Session := LSession;
    LSession := nil;
    Inc(FSerial);
  finally
    LSession.Free;
  end;
end;

procedure TNyxReviewWorkspaces.Discard(const AOwner: TNyxText;
  const ARef: TNyxReviewRef; AExpected: Integer);
var
  LIndex: Integer;
  LMove: Integer;
  LSession: TNyxAgentSession;
begin
  RequireAccess(True);

  if ARef.ID = '' then
  begin
    raise ENyxModel.Create('The active user workspace cannot be disposed as a review');
  end;
  LSession := Resolve(AOwner, ARef);

  if AExpected <> LSession.Revision then
  begin
    raise ENyxModel.Create('Review disposal revision conflict; inspect it first');
  end;
  LIndex := Index(ARef);
  LSession.Free;
  for LMove := LIndex + 1 to High(FEntries) do
  begin
    FEntries[LMove - 1] := FEntries[LMove];
  end;
  SetLength(FEntries, Length(FEntries) - 1);
end;

function TNyxReviewWorkspaces.Describe(AIndex: Integer): TNyxDataValue;
var
  LState: TNyxDataValue;
begin
  FEntries[AIndex].Session.InheritPermission(FActive.Permission);
  LState := FEntries[AIndex].Session.Exchange(NyxObject([
    NyxField('op', NyxData('observe')),
    NyxField('after', NyxData(FEntries[AIndex].Session.Revision))]));
  Result := NyxObject([NyxField('review', NyxData(FEntries[AIndex].Reference.ID)),
    NyxField('label', NyxData(FEntries[AIndex].LabelText)),
    NyxField('actor', NyxData(FEntries[AIndex].Actor)),
    NyxField('session', LState.Field('session'))]);
end;

procedure TNyxReviewWorkspaces.ReleaseOwner(const AOwner: TNyxText);
var
  LIndex: Integer;
  LMove: Integer;
begin
  for LIndex := High(FEntries) downto 0 do
  begin

    if FEntries[LIndex].Owner = AOwner then
    begin
      FEntries[LIndex].Session.Free;
      for LMove := LIndex + 1 to High(FEntries) do
      begin
        FEntries[LMove - 1] := FEntries[LMove];
      end;
      SetLength(FEntries, Length(FEntries) - 1);
      FActive.RecordActivity('MCP transport', 'review retirement', 'disconnected owner');
    end;
  end;
  for LIndex := High(FReceipts) downto 0 do
  begin

    if FReceipts[LIndex].Owner = AOwner then
    begin
      for LMove := LIndex + 1 to High(FReceipts) do
      begin
        FReceipts[LMove - 1] := FReceipts[LMove];
      end;
      SetLength(FReceipts, Length(FReceipts) - 1);
    end;
  end;
end;

function TNyxReviewWorkspaces.Observe: TNyxDataValue;
var
  LItems: array of TNyxDataValue;
  LIndex: Integer;
begin
  SetLength(LItems, Length(FEntries));
  for LIndex := 0 to High(FEntries) do
  begin
    FEntries[LIndex].Session.InheritPermission(FActive.Permission);
    LItems[LIndex] := Describe(LIndex);
  end;
  Result := NyxArray(LItems);
end;

procedure TNyxReviewWorkspaces.Remember(const AOwner, AOperation, ARequest: TNyxText;
  const AResult: TNyxDataValue);
var
  LIndex: Integer;
begin
  SetLength(FReceipts, Length(FReceipts) + 1);
  LIndex := High(FReceipts);
  FReceipts[LIndex].Owner := AOwner;
  FReceipts[LIndex].Operation := AOperation;
  FReceipts[LIndex].Request := ARequest;
  FReceipts[LIndex].Result := AResult;
end;

function TNyxReviewWorkspaces.Manage(const AOwner, AActor: TNyxText;
  const AArguments: TNyxDataValue): TNyxDataValue;
var
  LMode: TNyxText;
  LOperation: TNyxText;
  LRequest: TNyxText;
  LIndex: Integer;
  LItems: array of TNyxDataValue;
  LRef: TNyxReviewRef;
  LBase: TNyxReviewBase;
  LUsed: Integer;
  LLive: Integer;
begin
  RequireAccess(False);
  LMode := AArguments.Field('mode').AsText;

  if LMode = 'list' then
  begin
    NyxAgentFields(AArguments, '|mode|');
    LItems := nil;
    for LIndex := 0 to High(FEntries) do
    begin

      if FEntries[LIndex].Owner = AOwner then
      begin
        SetLength(LItems, Length(LItems) + 1);
        LItems[High(LItems)] := Describe(LIndex);
      end;
    end;
    Exit(NyxObject([NyxField('items', NyxArray(LItems)),
      NyxField('total', NyxData(Length(LItems))), NyxField('maximum', NyxData(8))]));
  end;

  if LMode = 'inspect' then
  begin
    NyxAgentFields(AArguments, '|mode|review|');
    LRef := NyxReview(AArguments.Field('review').AsText);
    Resolve(AOwner, LRef);
    Exit(Describe(Index(LRef)));
  end;
  RequireAccess(True);

  if LMode = 'create' then
  begin
    NyxAgentFields(AArguments, '|mode|expectedRevision|operationId|label|base|');
  end
  else if LMode = 'discard' then
  begin
    NyxAgentFields(AArguments, '|mode|review|expectedRevision|operationId|');
  end
  else
  begin
    raise ENyxModel.Create('Review mode must be list, inspect, create or discard');
  end;
  LOperation := AArguments.Field('operationId').AsText;

  if (LOperation = '') or (Length(LOperation) > 120) then
  begin
    raise ENyxModel.Create('Review operationId must contain 1..120 characters');
  end;
  LRequest := AArguments.ToJSON;
  for LIndex := 0 to High(FReceipts) do
  begin

    if (FReceipts[LIndex].Owner = AOwner) and (FReceipts[LIndex].Operation = LOperation) then
    begin

      if FReceipts[LIndex].Request <> LRequest then
      begin
        raise ENyxModel.Create('Review operationId was used with different arguments');
      end;
      Exit(FReceipts[LIndex].Result);
    end;
  end;

  if LMode = 'create' then
  begin
    LUsed := 0;
    LLive := 0;
    for LIndex := 0 to High(FReceipts) do
    begin

      if FReceipts[LIndex].Owner = AOwner then
      begin
        Inc(LUsed);
      end;
    end;
    for LIndex := 0 to High(FEntries) do
    begin

      if FEntries[LIndex].Owner = AOwner then
      begin
        Inc(LLive);
      end;
    end;

    if LUsed + LLive + 2 > 64 then
    begin
      raise ENyxModel.Create('Review lifecycle receipt budget reached; dispose reviews and reconnect');
    end;
    LBase := nrbAccepted;

    if AArguments.Field('base').AsText = 'empty' then
    begin
      LBase := nrbEmpty;
    end
    else if AArguments.Field('base').AsText <> 'accepted' then
    begin
      raise ENyxModel.Create('Review base must be empty or accepted');
    end;
    LRef := CreateReview(AOwner, AActor, AArguments.Field('label').AsText,
      LBase, AArguments.Field('expectedRevision').AsInteger);
    Result := Describe(Index(LRef));
  end
  else
  begin
    LRef := NyxReview(AArguments.Field('review').AsText);
    Discard(AOwner, LRef, AArguments.Field('expectedRevision').AsInteger);
    Result := NyxObject([NyxField('review', NyxData(LRef.ID)),
      NyxField('disposed', NyxData(True))]);
  end;
  Remember(AOwner, LOperation, LRequest, Result);
  FActive.RecordActivity(AActor, 'nyx_reviews', LMode + CSeparator + LRef.ID);
end;

function TNyxReviewWorkspaces.Call(const ATool, AOwner, AActor: TNyxText;
  const AArguments: TNyxDataValue): TNyxDataValue;
var
  LRef: TNyxReviewRef;
  LSession: TNyxAgentSession;
begin
  LRef := NyxActiveWorkspace;
  try

    if (AOwner = '') or (NyxTextScalarCount(AOwner) > 120) then
    begin
      raise ENyxModel.Create('Semantic review dispatch requires a bounded authenticated connection owner');
    end;

    if ATool = 'nyx_reviews' then
    begin
      Exit(Manage(AOwner, AActor, AArguments));
    end;
    LRef := NyxReviewArgument(AArguments);
    LSession := Resolve(AOwner, LRef);
    Result := NyxWithReview(LSession.Call(ATool, AActor,
      NyxReviewArguments(AArguments), AOwner), LRef);

    if LRef.ID <> '' then
    begin
      FActive.RecordActivity(AActor, ATool, 'review ' + LRef.ID + CCompleted);
    end;
  except
    on LException: Exception do
    begin

      if (LRef.ID <> '') or (ATool = 'nyx_reviews') or
        NyxAgentHas(AArguments, 'review') then
      begin
        FActive.RecordActivity(AActor, ATool, 'review ' + LRef.ID + CRefused);
      end;
      raise;
    end;
  end;
end;

end.
