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

unit nyx.studio.sourceobservations;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.data, nyx.source, nyx.studio.projects, nyx.studio.workspaces;

{ Private owning-editor response, never a project/import/recovery format. The
  server must capture APair and ACheckpoint together under its registry lock.
  Exact accepted files are checked before emitting compact UTF-8 frame lengths;
  the source/design already travel in the paired project and are not duplicated.
  Issuer is an opaque server lifetime identity, not a credential or signature. }
function EncodeNyxSourceObservation(const AIssuer: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; ARevision: Integer;
  const APair: TNyxProjectPair; const ACheckpoint: TNyxSourceCheckpoint): TNyxDataValue;

{ Receive only from the authenticated private editor transport. AIssuer was
  captured from that connection's claim; workspace is its immutable request
  context, and revision is the same response's session revision. The owning
  channel authenticates the complete pair and frame, not this metadata alone.
  Reconstructs copied source storage, refusing wrong context/shape/UTF-8 splits.
  The caller must still AdmitNyxCapturedProject before publishing any owners.
  Never call this for arbitrary project strings, MCP mutations or recovery data. }
function ReceiveNyxSourceObservation(const AData: TNyxDataValue;
  const AIssuer: TNyxText; const AWorkspace: TNyxWorkspaceRef;
  ARevision: Integer; const APair: TNyxProjectPair): TNyxSourceCheckpoint;

implementation

uses
  nyx.bytes;

procedure ValidateContext(const AIssuer: TNyxText; ARevision: Integer);
begin

  if (AIssuer = '') or (Length(AIssuer) > 128) or (ARevision < 0) then
  begin
    raise ENyxProjectConflict.Create('Source observation requires an owning server and revision');
  end;
end;

function EncodeNyxSourceObservation(const AIssuer: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; ARevision: Integer;
  const APair: TNyxProjectPair; const ACheckpoint: TNyxSourceCheckpoint): TNyxDataValue;
var
  LWorkspace: TNyxSourceWorkspace;
  LFrame: TNyxDataValue;
  LOrigin: TNyxText;
begin
  ValidateContext(AIssuer, ARevision);

  if (ACheckpoint.Design = '') or (ACheckpoint.Design <> APair.Design) or
    (ACheckpoint.Source <> APair.Source) then
  begin
    raise ENyxProjectConflict.Create('Source observation must capture the exact admitted pair');
  end;
  LOrigin := 'declarative';

  if ACheckpoint.Origin = nsoExecuted then
  begin
    LOrigin := 'executed';
  end;
  LWorkspace := TNyxSourceWorkspace.Create;
  try
    LWorkspace.Restore(ACheckpoint);
    LFrame := TNyxDataValue.ParseJSON(LWorkspace.Snapshot);
    Result := NyxObject([
      NyxField('version', NyxData(1)),
      NyxField('issuer', NyxData(AIssuer)),
      NyxField('workspace', NyxData(AWorkspace.ID)),
      NyxField('revision', NyxData(ARevision)),
      NyxField('prefixBytes', NyxData(NyxUTF8ByteCount(LFrame.Field('prefix').AsText))),
      NyxField('bodyBytes', NyxData(NyxUTF8ByteCount(LFrame.Field('body').AsText))),
      NyxField('custom', LFrame.Field('custom')),
      NyxField('origin', NyxData(LOrigin))]);
  finally
    LWorkspace.Free;
  end;
end;

function ReceiveNyxSourceObservation(const AData: TNyxDataValue;
  const AIssuer: TNyxText; const AWorkspace: TNyxWorkspaceRef;
  ARevision: Integer; const APair: TNyxProjectPair): TNyxSourceCheckpoint;
var
  LPrefixBytes: Integer;
  LBodyBytes: Integer;
  LSourceBytes: TNyxBytes;
  LOrigin: TNyxText;
  LFields: array of TNyxDataField;
  LWorkspace: TNyxSourceWorkspace;
begin
  ValidateContext(AIssuer, ARevision);

  if (AData.Kind <> ndObject) or (AData.Count <> 8) or
    (AData.Field('version').AsInteger <> 1) or
    (AData.Field('issuer').AsText <> AIssuer) or
    (AData.Field('workspace').AsText <> AWorkspace.ID) or
    (AData.Field('revision').AsInteger <> ARevision) then
  begin
    raise ENyxProjectConflict.Create('Source observation belongs to another server, project or revision');
  end;
  LPrefixBytes := AData.Field('prefixBytes').AsInteger;
  LBodyBytes := AData.Field('bodyBytes').AsInteger;
  LSourceBytes := NyxEncodeUTF8(APair.Source);

  if (LPrefixBytes < 0) or (LPrefixBytes > Length(LSourceBytes)) or
    (LBodyBytes < 0) or (LBodyBytes > Length(LSourceBytes) - LPrefixBytes) then
  begin
    raise ENyxProjectConflict.Create('Source observation has invalid frame boundaries');
  end;
  LOrigin := AData.Field('origin').AsText;

  if (LOrigin <> 'declarative') and (LOrigin <> 'executed') then
  begin
    raise ENyxProjectConflict.Create('Source observation has an unknown construction origin');
  end;
  SetLength(LFields, 5);
  { Wire lengths count UTF-8 bytes on both targets. Decode each independent slice
    so a split supplementary scalar refuses instead of silently replacing text. }
  LFields[0] := NyxField('prefix', NyxData(NyxDecodeUTF8(Copy(LSourceBytes, 0, LPrefixBytes))));
  LFields[1] := NyxField('body', NyxData(NyxDecodeUTF8(Copy(LSourceBytes, LPrefixBytes, LBodyBytes))));
  LFields[2] := NyxField('suffix', NyxData(NyxDecodeUTF8(Copy(LSourceBytes,
    LPrefixBytes + LBodyBytes, Length(LSourceBytes) - LPrefixBytes - LBodyBytes))));
  LFields[3] := NyxField('design', NyxData(APair.Design));
  LFields[4] := NyxField('custom', NyxData(AData.Field('custom').AsBoolean));

  if LOrigin = 'executed' then
  begin
    SetLength(LFields, 6);
    LFields[5] := NyxField('executed', NyxData(True));
  end;
  LWorkspace := TNyxSourceWorkspace.Create;
  try
    LWorkspace.Restore(NyxObject(LFields).ToJSON);
    Result := LWorkspace.Capture;
  finally
    LWorkspace.Free;
  end;
end;

end.
