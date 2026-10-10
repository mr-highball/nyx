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
  {$ifndef PAS2JS}, nyx.schema, nyx.studio.session{$endif};

type
  { Unconfirmed means transport lost acknowledgement, not proof of failed remote
    admission. The owning editor must observe/reconcile before retrying new work. }
  TNyxSourcePublicationOutcome = (npoCommitted, npoRefused, npoUnconfirmed);
  {$ifndef PAS2JS}
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
    FSchemas: INyxSchemaSnapshot;
    function GetSource: TNyxText;
  public
    function IsCaptured: Boolean;
    function OwnedBy(const AAuthority: TNyxText): Boolean;
    property Source: TNyxText read GetSource;
    property Workspace: TNyxWorkspaceRef read FWorkspace;
    property Revision: Integer read FRevision;
    property Baseline: TNyxProjectPair read FBaseline;
    property Request: TNyxStudioSourceRequest read FRequest;
    property Schemas: INyxSchemaSnapshot read FSchemas;
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
  Result := FRequest.Source;
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

  if ABaseline.Pending then
  begin
    LBuffer := ABaseline.Draft;
  end;

  if (AAuthority = '') or (ARevision < 0) or (ASchemas = nil) or
    (ARequest.Source = '') or (ARequest.Source <> LBuffer) then
  begin
    raise ENyxProjectConflict.Create('Source publication requires its complete captured editor context');
  end;
  Result.FAuthority := AAuthority;
  Result.FWorkspace := AWorkspace;
  Result.FRevision := ARevision;
  Result.FBaseline := ABaseline;
  Result.FRequest := ARequest;
  Result.FSchemas := ASchemas;
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
