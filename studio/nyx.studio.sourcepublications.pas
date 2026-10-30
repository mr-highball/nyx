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

unit nyx.studio.sourcepublications;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.data, nyx.studio.workspaces, nyx.studio.sourceprojection,
  nyx.studio.editorbuild, nyx.studio.projects
  {$ifndef PAS2JS}, nyx.schema, nyx.studio.session, nyx.studio.projectimport,
  nyx.studio.transactions{$endif};

type
  { Unconfirmed means transport lost acknowledgement, not proof of failed remote
    admission. The owning editor must observe/reconcile before retrying new work. }
  TNyxSourcePublicationOutcome = (npoCommitted, npoRefused, npoUnconfirmed);
  {$ifndef PAS2JS}
  { Apply consumes its acknowledged draft. Visual publication preserves that
    buffer; project Open admits an independent captured pair as one command. }
  TNyxStudioSourcePublicationKind = (spkApply, spkVisual, spkProjectImportReview,
    spkSourceEdit, spkProjectOpening);
  { Opaque precompile capture. Values and immutable creators remain independently
    owned; no document, session, renderer or worker is borrowed. There is no wire
    decoder for this ticket. Authority comparison never exposes its credential.
    Failed durable publication may retry; success changes the captured revision. }
  TNyxStudioSourcePublication = record
  private
    FAuthority: TNyxText;
    FWorkspace: TNyxWorkspaceRef;
    FRevision: Integer;
    FBaseline: TNyxProjectPair;
    FRequest: TNyxStudioSourceRequest;
    FKind: TNyxStudioSourcePublicationKind;
    FDesignRequest: TNyxStudioDesignRequest;
    FDesignProposal: INyxPreparedDesign;
    FSchemas: INyxSchemaSnapshot;
    { Callback intent and removal review stay private, transport-owned captured
      values. They are revalidated at publication and never confer execution. }
    FCallbackOwner: TNyxText;
    FCallbackArguments: TNyxDataValue;
    FCallbackResults: TNyxDataValue;
    { Body-only intent has no removal ticket. Navigation stays private proposed
      metadata until exact source and whole-meaning construction is admitted. }
    FHandlers: Boolean;
    FHandlerArguments: TNyxDataValue;
    FHandlerResults: TNyxDataValue;
    { Exact general source intent is captured separately from the editor draft.
      Its sealed ordinary source request may intentionally change design meaning. }
    FSourceEditArguments: TNyxDataValue;
    { Import construction prepares a transport-owned review, never automatic
      adoption. Input is an immutable complete upload captured by its session. }
    FProjectImport: INyxProjectImportReviewRequest;
    { The server captures this ordinary sealed project request against its own
      baseline. It carries imported files/drafts, never serialized trust. }
    FOpeningRequest: TNyxStudioProjectRequest;
    { Focused data tools share visual construction but retain their typed origin
      for observing activity. Default Design means the mixed transaction tool. }
    FTransactionDomain: TNyxTransactionDomain;
    function GetSource: TNyxText;
    function GetIsVisual: Boolean;
    function GetIsCallbacks: Boolean;
    function GetIsSourceEdit: Boolean;
    function GetIsProjectOpening: Boolean;
  public
    function IsCaptured: Boolean;
    function OwnedBy(const AAuthority: TNyxText): Boolean;
    property Source: TNyxText read GetSource;
    property Workspace: TNyxWorkspaceRef read FWorkspace;
    property Revision: Integer read FRevision;
    property Baseline: TNyxProjectPair read FBaseline;
    property Request: TNyxStudioSourceRequest read FRequest;
    property IsVisual: Boolean read GetIsVisual;
    property DesignRequest: TNyxStudioDesignRequest read FDesignRequest;
    property DesignProposal: INyxPreparedDesign read FDesignProposal;
    property Schemas: INyxSchemaSnapshot read FSchemas;
    property IsCallbacks: Boolean read GetIsCallbacks;
    property CallbackOwner: TNyxText read FCallbackOwner;
    property CallbackArguments: TNyxDataValue read FCallbackArguments;
    property CallbackResults: TNyxDataValue read FCallbackResults;
    property IsHandlers: Boolean read FHandlers;
    property HandlerArguments: TNyxDataValue read FHandlerArguments;
    property HandlerResults: TNyxDataValue read FHandlerResults;
    property IsSourceEdit: Boolean read GetIsSourceEdit;
    property SourceEditArguments: TNyxDataValue read FSourceEditArguments;
    property ProjectImport: INyxProjectImportReviewRequest read FProjectImport;
    property IsProjectOpening: Boolean read GetIsProjectOpening;
    property OpeningRequest: TNyxStudioProjectRequest read FOpeningRequest;
    property TransactionDomain: TNyxTransactionDomain read FTransactionDomain;
  end;

  {$endif}

  { Small owning-editor acknowledgement, not source/execution authority. It names
    the exact retained job, producer, server, workspace and committed revision.
    A caller must coordinate that revision with its local/observing editor before
    releasing any synchronization reservation. No project or source is returned. }
  TNyxSourcePublicationReceipt = record
  private
    FIssuer: TNyxText;
    FWorkspace: TNyxWorkspaceRef;
    FJob: TNyxBuildJobRef;
    FReference: TNyxSourceProjectionRef;
    FRevision: Integer;
  public
    property Issuer: TNyxText read FIssuer;
    property Workspace: TNyxWorkspaceRef read FWorkspace;
    property Job: TNyxBuildJobRef read FJob;
    property Reference: TNyxSourceProjectionRef read FReference;
    property Revision: Integer read FRevision;
  end;

{ Trusted server capture from the ordinary session's sealed request, under its
  registry lock. No request/source/creator input is reconstructed from JSON. }
{$ifndef PAS2JS}
function CaptureNyxStudioSourcePublication(const AAuthority: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; ARevision: Integer;
  const ABaseline: TNyxProjectPair; const ARequest: TNyxStudioSourceRequest;
  const ASchemas: INyxSchemaSnapshot): TNyxStudioSourcePublication;
{ Native owning-host capture of a validated exact range group. Unlike a visual
  patch, general source may change the constructed design. No visible draft is
  staged, and no serialized argument can forge this sealed session request. }
function CaptureNyxStudioSourceEditPublication(const AAuthority: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; ARevision: Integer;
  const ABaseline: TNyxProjectPair; const ARequest: TNyxStudioSourceRequest;
  const ASchemas: INyxSchemaSnapshot;
  const AArguments: TNyxDataValue): TNyxStudioSourcePublication;
{ Trusted server capture only. The server has independently prepared this exact
  semantic edit from its own live checkpoint. The retained proposal has no
  transferable model/workspace; no client origin flag or draft replacement is
  accepted. Completion must reproduce its exact whole meaning. }
function CaptureNyxStudioVisualPublication(const AAuthority: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; ARevision: Integer;
  const ABaseline: TNyxProjectPair; const ARequest: TNyxStudioDesignRequest;
  const AProposal: INyxPreparedDesign;
  const ASchemas: INyxSchemaSnapshot): TNyxStudioSourcePublication;
{ Focused data Apply uses an ordinary independently prepared transaction; this
  trusted capture records only its closed origin, never a second execution path. }
function CaptureNyxStudioDataPublication(const AAuthority: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; ARevision: Integer;
  const ABaseline: TNyxProjectPair; const ARequest: TNyxStudioDesignRequest;
  const AProposal: INyxPreparedDesign; const ASchemas: INyxSchemaSnapshot;
  ADomain: TNyxTransactionDomain): TNyxStudioSourcePublication;
{ The owning backend prepares this group and its bounded proposed rows. These
  rows describe intent, not compiler success; publication retains the same
  independently executed whole-meaning check as other visual edits. }
function CaptureNyxStudioCallbackPublication(const AAuthority: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; ARevision: Integer;
  const ABaseline: TNyxProjectPair; const ARequest: TNyxStudioDesignRequest;
  const AProposal: INyxPreparedDesign; const ASchemas: INyxSchemaSnapshot;
  const AOwner: TNyxText; const AArguments, AResults: TNyxDataValue): TNyxStudioSourcePublication;
{ Independently server-prepared exact body group. No transferable source or
  client-supplied execution claim is admitted through this native-only factory. }
function CaptureNyxStudioHandlerPublication(const AAuthority: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; ARevision: Integer;
  const ABaseline: TNyxProjectPair; const ARequest: TNyxStudioDesignRequest;
  const AProposal: INyxPreparedDesign; const ASchemas: INyxSchemaSnapshot;
  const AArguments, AResults: TNyxDataValue): TNyxStudioSourcePublication;
{ Native owning-host capture for explicit compiler review. The eventual candidate
  stays private to the initiating transport until ordinary ticketed adoption. }
function CaptureNyxStudioProjectImportReview(const AAuthority: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; const ABaseline: TNyxProjectPair;
  const ARequest: INyxProjectImportReviewRequest;
  const ASchemas: INyxSchemaSnapshot): TNyxStudioSourcePublication;
{ Ordinary private editor Open differs from transport-owned import review:
  successful execution may publish the complete captured pair immediately as
  one undoable command. Baseline, creators and issuing authority stay private. }
function CaptureNyxStudioProjectOpening(const AAuthority: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; ARevision: Integer;
  const ABaseline: TNyxProjectPair; const ARequest: TNyxStudioProjectRequest;
  const ASchemas: INyxSchemaSnapshot): TNyxStudioSourcePublication;
{$endif}

{ Construct after staging successful publication, with its exact next revision.
  Encoding/decoding is a private acknowledgement boundary, never an admission API. }
function NyxSourcePublicationReceipt(const AIssuer: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; const AJob: TNyxBuildJobRef;
  const AReference: TNyxSourceProjectionRef; ARevision: Integer): TNyxSourcePublicationReceipt;
function EncodeNyxSourcePublicationReceipt(const AReceipt: TNyxSourcePublicationReceipt): TNyxDataValue;
function DecodeNyxSourcePublicationReceipt(const AData: TNyxDataValue;
  const AIssuer: TNyxText; const AWorkspace: TNyxWorkspaceRef;
  const AReference: TNyxSourceProjectionRef): TNyxSourcePublicationReceipt;

implementation

{$ifndef PAS2JS}
function TNyxStudioSourcePublication.GetSource: TNyxText;
begin

  if FKind = spkProjectOpening then
  begin
    Exit(FOpeningRequest.Source);
  end;

  if FKind = spkProjectImportReview then
  begin
    Exit(FProjectImport.Input.Source);
  end;

  if FKind = spkVisual then
  begin
    Exit(FDesignProposal.Source);
  end;
  Result := FRequest.Source;
end;

function TNyxStudioSourcePublication.GetIsVisual: Boolean;
begin
  Result := FKind = spkVisual;
end;

function TNyxStudioSourcePublication.GetIsCallbacks: Boolean;
begin
  Result := IsVisual and (FCallbackOwner <> '');
end;

function TNyxStudioSourcePublication.GetIsSourceEdit: Boolean;
begin
  Result := FKind = spkSourceEdit;
end;

function TNyxStudioSourcePublication.GetIsProjectOpening: Boolean;
begin
  Result := FKind = spkProjectOpening;
end;

function CaptureNyxStudioProjectOpening(const AAuthority: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; ARevision: Integer;
  const ABaseline: TNyxProjectPair; const ARequest: TNyxStudioProjectRequest;
  const ASchemas: INyxSchemaSnapshot): TNyxStudioSourcePublication;
begin

  if (AAuthority = '') or (ARevision < 0) or (ASchemas = nil) or
    (ARequest.Source = '') or (ARequest.History <> pohPairedCommand) then
  begin
    raise ENyxProjectConflict.Create('Project opening publication needs its complete captured context');
  end;
  Result := Default(TNyxStudioSourcePublication);
  Result.FAuthority := AAuthority;
  Result.FWorkspace := AWorkspace;
  Result.FRevision := ARevision;
  Result.FBaseline := ABaseline;
  Result.FSchemas := ASchemas;
  Result.FKind := spkProjectOpening;
  Result.FOpeningRequest := ARequest;
end;

function TNyxStudioSourcePublication.IsCaptured: Boolean;
begin
  Result := (FAuthority <> '') and (FSchemas <> nil);
end;

function TNyxStudioSourcePublication.OwnedBy(const AAuthority: TNyxText): Boolean;
begin
  Result := IsCaptured and (FAuthority = AAuthority);
end;

function CaptureNyxStudioSourcePublication(const AAuthority: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; ARevision: Integer;
  const ABaseline: TNyxProjectPair; const ARequest: TNyxStudioSourceRequest;
  const ASchemas: INyxSchemaSnapshot): TNyxStudioSourcePublication;
var
  LBuffer: TNyxText;
begin
  LBuffer := ABaseline.Source;

  if ABaseline.Pending and not ARequest.IsFileEdit then
  begin
    LBuffer := ABaseline.Draft;
  end;

  if (AAuthority = '') or (ARevision < 0) or (ASchemas = nil) or
    (ARequest.Source = '') or (ARequest.Source <> LBuffer) then
  begin
    raise ENyxProjectConflict.Create('Source publication requires its complete captured editor context');
  end;
  Result := Default(TNyxStudioSourcePublication);
  Result.FAuthority := AAuthority;
  Result.FKind := spkApply;
  Result.FWorkspace := AWorkspace;
  Result.FRevision := ARevision;
  Result.FBaseline := ABaseline;
  Result.FRequest := ARequest;
  Result.FSchemas := ASchemas;
end;

function CaptureNyxStudioSourceEditPublication(const AAuthority: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; ARevision: Integer;
  const ABaseline: TNyxProjectPair; const ARequest: TNyxStudioSourceRequest;
  const ASchemas: INyxSchemaSnapshot;
  const AArguments: TNyxDataValue): TNyxStudioSourcePublication;
begin

  if (AAuthority = '') or (ARevision < 1) or (ASchemas = nil) or
    ABaseline.Pending or (ABaseline.FileDrafts.Count > 0) or
    not ARequest.IsSourceEdit or not ARequest.Changed or (ARequest.Source = '') or
    ((ARequest.Source = ABaseline.Source) and
      (ARequest.Files.Encoded = ABaseline.Files.Encoded)) or
    (ARequest.SchemaRevision <> ASchemas.Revision) or
    ((AArguments.Field('mode').AsText <> 'edit-unit') and
      (AArguments.Field('mode').AsText <> 'edit-files') and
      (AArguments.Field('mode').AsText <> 'edit-routines')) or
    (AArguments.Field('expectedRevision').AsInteger <> ARevision) then
  begin
    raise ENyxProjectConflict.Create('Source edit publication requires its sealed complete context');
  end;
  Result := Default(TNyxStudioSourcePublication);
  Result.FAuthority := AAuthority;
  Result.FKind := spkSourceEdit;
  Result.FWorkspace := AWorkspace;
  Result.FRevision := ARevision;
  Result.FBaseline := ABaseline;
  Result.FRequest := ARequest;
  Result.FSchemas := ASchemas;
  Result.FSourceEditArguments := AArguments.Copy;
end;

function CaptureNyxStudioVisualPublication(const AAuthority: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; ARevision: Integer;
  const ABaseline: TNyxProjectPair; const ARequest: TNyxStudioDesignRequest;
  const AProposal: INyxPreparedDesign;
  const ASchemas: INyxSchemaSnapshot): TNyxStudioSourcePublication;
begin

  if (AAuthority = '') or (ARevision < 1) or not ARequest.RequiresExecution or
    (AProposal = nil) or not AProposal.RequiresCompilation or
    AProposal.Diagnostic.Defined or not AProposal.Matches(ARequest) or
    (ASchemas = nil) or (ASchemas.Revision <> ARequest.SchemaRevision) then
  begin
    raise ENyxProjectConflict.Create('Visual publication requires its exact server-prepared proposal');
  end;
  Result := Default(TNyxStudioSourcePublication);
  Result.FAuthority := AAuthority;
  Result.FKind := spkVisual;
  Result.FWorkspace := AWorkspace;
  Result.FRevision := ARevision;
  Result.FBaseline := ABaseline;
  Result.FDesignRequest := ARequest;
  Result.FDesignProposal := AProposal;
  Result.FSchemas := ASchemas;
end;

function CaptureNyxStudioDataPublication(const AAuthority: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; ARevision: Integer;
  const ABaseline: TNyxProjectPair; const ARequest: TNyxStudioDesignRequest;
  const AProposal: INyxPreparedDesign; const ASchemas: INyxSchemaSnapshot;
  ADomain: TNyxTransactionDomain): TNyxStudioSourcePublication;
begin

  if (ADomain = ntdDesign) or
    (ARequest.IntentData.Field('edit').Field('action').AsInteger <> Ord(sdaTransaction)) then
  begin
    raise ENyxProjectConflict.Create('Focused data publication requires its captured transaction');
  end;
  Result := CaptureNyxStudioVisualPublication(AAuthority, AWorkspace, ARevision,
    ABaseline, ARequest, AProposal, ASchemas);
  Result.FTransactionDomain := ADomain;
end;

function CaptureNyxStudioCallbackPublication(const AAuthority: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; ARevision: Integer;
  const ABaseline: TNyxProjectPair; const ARequest: TNyxStudioDesignRequest;
  const AProposal: INyxPreparedDesign; const ASchemas: INyxSchemaSnapshot;
  const AOwner: TNyxText; const AArguments, AResults: TNyxDataValue): TNyxStudioSourcePublication;
begin

  if (AOwner = '') or (ARequest.IntentData.Field('edit').Field('action').AsInteger <>
    Ord(sdaCallbacks)) or
    (AArguments.Field('expectedRevision').AsInteger <> ARevision) or
    (AArguments.Field('mode').AsText <> 'apply') or (AResults.Kind <> ndArray) then
  begin
    raise ENyxProjectConflict.Create('Callback publication requires its captured typed group and owning transport');
  end;
  Result := CaptureNyxStudioVisualPublication(AAuthority, AWorkspace, ARevision,
    ABaseline, ARequest, AProposal, ASchemas);
  Result.FCallbackOwner := AOwner;
  Result.FCallbackArguments := AArguments.Copy;
  Result.FCallbackResults := AResults.Copy;
end;

function CaptureNyxStudioHandlerPublication(const AAuthority: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; ARevision: Integer;
  const ABaseline: TNyxProjectPair; const ARequest: TNyxStudioDesignRequest;
  const AProposal: INyxPreparedDesign; const ASchemas: INyxSchemaSnapshot;
  const AArguments, AResults: TNyxDataValue): TNyxStudioSourcePublication;
begin

  if (ARequest.IntentData.Field('edit').Field('action').AsInteger <> Ord(sdaHandlers)) or
    (AArguments.Field('expectedRevision').AsInteger <> ARevision) or
    (AArguments.Field('mode').AsText <> 'apply') or (AResults.Kind <> ndArray) then
  begin
    raise ENyxProjectConflict.Create('Handler publication requires its captured typed body group');
  end;
  Result := CaptureNyxStudioVisualPublication(AAuthority, AWorkspace, ARevision,
    ABaseline, ARequest, AProposal, ASchemas);
  Result.FHandlers := True;
  Result.FHandlerArguments := AArguments.Copy;
  Result.FHandlerResults := AResults.Copy;
end;

function CaptureNyxStudioProjectImportReview(const AAuthority: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; const ABaseline: TNyxProjectPair;
  const ARequest: INyxProjectImportReviewRequest;
  const ASchemas: INyxSchemaSnapshot): TNyxStudioSourcePublication;
begin

  if (AAuthority = '') or (ARequest = nil) or (ASchemas = nil) or
    (ARequest.Revision < 1) or (ASchemas.Revision <> NyxSchemaRevision) then
  begin
    raise ENyxProjectConflict.Create('Compiler import review requires its owning captured context');
  end;
  Result := Default(TNyxStudioSourcePublication);
  Result.FAuthority := AAuthority;
  Result.FKind := spkProjectImportReview;
  Result.FWorkspace := AWorkspace;
  Result.FRevision := ARequest.Revision;
  Result.FBaseline := ABaseline;
  Result.FSchemas := ASchemas;
  Result.FProjectImport := ARequest;
end;
{$endif}

function NyxSourcePublicationReceipt(const AIssuer: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; const AJob: TNyxBuildJobRef;
  const AReference: TNyxSourceProjectionRef; ARevision: Integer): TNyxSourcePublicationReceipt;
begin

  if (AIssuer = '') or (Length(AIssuer) > 128) or (AJob.ID = '') or
    (AReference.Name = '') or (ARevision < 0) then
  begin
    raise ENyxProjectConflict.Create('Source publication acknowledgement requires its exact owning context');
  end;
  Result.FIssuer := AIssuer;
  Result.FWorkspace := AWorkspace;
  Result.FJob := AJob;
  Result.FReference := AReference;
  Result.FRevision := ARevision;
end;

function EncodeNyxSourcePublicationReceipt(const AReceipt: TNyxSourcePublicationReceipt): TNyxDataValue;
begin
  NyxSourcePublicationReceipt(AReceipt.Issuer, AReceipt.Workspace, AReceipt.Job,
    AReceipt.Reference, AReceipt.Revision);
  Result := NyxObject([
    NyxField('version', NyxData(1)), NyxField('state', NyxData('published')),
    NyxField('issuer', NyxData(AReceipt.Issuer)),
    NyxField('workspace', NyxData(AReceipt.Workspace.ID)),
    NyxField('job', NyxData(AReceipt.Job.ID)),
    NyxField('reference', NyxData(AReceipt.Reference.Name)),
    NyxField('revision', NyxData(AReceipt.Revision))]);
end;

function DecodeNyxSourcePublicationReceipt(const AData: TNyxDataValue;
  const AIssuer: TNyxText; const AWorkspace: TNyxWorkspaceRef;
  const AReference: TNyxSourceProjectionRef): TNyxSourcePublicationReceipt;
begin

  if (AData.Kind <> ndObject) or (AData.Count <> 7) or
    (AData.Field('version').AsInteger <> 1) or
    (AData.Field('state').AsText <> 'published') or
    (AData.Field('issuer').AsText <> AIssuer) or
    (AData.Field('workspace').AsText <> AWorkspace.ID) or
    (AData.Field('reference').AsText <> AReference.Name) then
  begin
    raise ENyxProjectConflict.Create('Source publication acknowledgement belongs to another owning context');
  end;
  Result := NyxSourcePublicationReceipt(AIssuer, AWorkspace,
    NyxBuildJob(AData.Field('job').AsText), AReference, AData.Field('revision').AsInteger);
end;

end.
