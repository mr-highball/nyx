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
program nyx_project_import_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, {$ifdef PAS2JS}Web,{$endif}
  nyx.text, nyx.bytes, nyx.data, nyx.model, nyx.codec, nyx.studio.projects,
  nyx.studio.projectimport, nyx.studio.session, nyx.studio.agents, nyx.test.source;

var
  GSession: TNyxAgentSession;
  GChecks: Integer;
  GSerial: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Project import: ' + AReason);
  end;
  Inc(GChecks);
end;

function Snapshot: TNyxText;
begin
  Result := GSession.Exchange(NyxObject([NyxField('op', NyxData('observe'))]))
    .Field('project').AsText;
end;

function Arguments(const AMode: TNyxText; const AFields: array of TNyxDataField;
  AMutation: Boolean = True): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LBase: Integer;
  LIndex: Integer;
begin
  LBase := 2;

  if AMutation then
  begin
    Inc(LBase);
  end;
  SetLength(LFields, LBase + Length(AFields));
  LFields[0] := NyxField('mode', NyxData(AMode));
  LFields[1] := NyxField('expectedRevision', NyxData(GSession.Revision));

  if AMutation then
  begin
    Inc(GSerial);
    LFields[2] := NyxField('operationId', NyxData('import-test-' + IntToStr(GSerial)));
  end;
  for LIndex := 0 to High(AFields) do
  begin
    LFields[LBase + LIndex] := AFields[LIndex];
  end;
  Result := NyxObject(LFields);
end;

function Call(const AArgs: TNyxDataValue; const AOwner: TNyxText = 'owner-one'): TNyxDataValue;
begin
  Result := GSession.Call('nyx_project', 'Scooty', AArgs, AOwner);
end;

procedure Refuses(const AArgs: TNyxDataValue; const AReason: TNyxText;
  const AOwner: TNyxText = 'owner-one');
var
  LBefore: TNyxText;
  LStamp: TNyxText;
  LRefused: Boolean;
begin
  LBefore := Snapshot;
  LStamp := GSession.RecoveryStamp;
  LRefused := False;
  try
    Call(AArgs, AOwner);
  except
    on Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (Snapshot = LBefore) and (GSession.RecoveryStamp = LStamp), AReason);
end;

{ Small scalar chunks exercise both supplementary encodings. Offsets come from
  server receipts. The completed input is compared against the original packet,
  independently of normalized design/source admission. }
function Upload(const APacket: TNyxText): TNyxText;
var
  LArgs: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LIndex: Integer;
  LStart: Integer;
  LCount: Integer;
  LScalar: Integer;
  LOffset: Integer;
begin
  LArgs := Arguments('begin-import', [NyxField('bytes', NyxData(NyxUTF8ByteCount(APacket)))]);
  LReceipt := Call(LArgs);
  Check(Call(LArgs).ToJSON = LReceipt.ToJSON, 'Begin retry retains one reservation');
  Result := LReceipt.Field('projectImport').Field('import').AsText;
  LIndex := 1;
  LOffset := 0;
  while LIndex <= Length(APacket) do
  begin
    LStart := LIndex;
    LCount := 0;
    while (LIndex <= Length(APacket)) and (LCount < 257) do
    begin

      if not NyxNextScalar(APacket, LIndex, LScalar) then
      begin
        raise Exception.Create('Malformed project fixture scalar');
      end;
      Inc(LCount);
    end;
    LArgs := Arguments('append-import', [NyxField('import', NyxData(Result)),
      NyxField('offset', NyxData(LOffset)),
      NyxField('text', NyxData(Copy(APacket, LStart, LIndex - LStart)))]);
    LReceipt := Call(LArgs);
    Check(Call(LArgs).ToJSON = LReceipt.ToJSON, 'Chunk retry never duplicates input');
    LOffset := LReceipt.Field('projectImport').Field('nextOffset').AsInteger;
  end;
  Check(LReceipt.Field('projectImport').Field('complete').AsBoolean,
    'Exact UTF-8 reservation completes');
end;

function ReadPart(const AMode, AImport, APart: TNyxText): TNyxText;
var
  LReply: TNyxDataValue;
  LArgs: TNyxDataValue;
  LOffset: Integer;
  LParts: TNyxStrings;
begin
  LParts := TNyxStrings.Create;
  try
    LOffset := 0;
    repeat

      if AMode = 'export' then
      begin
        LArgs := Arguments(AMode, [NyxField('part', NyxData(APart)),
          NyxField('offset', NyxData(LOffset)), NyxField('count', NyxData(113))], False);
      end
      else
      begin
        LArgs := Arguments(AMode, [NyxField('import', NyxData(AImport)),
          NyxField('part', NyxData(APart)), NyxField('offset', NyxData(LOffset)),
          NyxField('count', NyxData(113))], False);
      end;
      LReply := Call(LArgs);
      LParts.Add(LReply.Field('text').AsText);
      LOffset := LReply.Field('nextOffset').AsInteger;
    until LOffset = LReply.Field('total').AsInteger;
    Result := LParts.Join;
  finally
    LParts.Free;
  end;
end;

function Review(const AImport, AResolution: TNyxText): TNyxText;
var
  LArgs: TNyxDataValue;
  LReceipt: TNyxDataValue;
begin
  LArgs := Arguments('review-import', [NyxField('import', NyxData(AImport)),
    NyxField('resolution', NyxData(AResolution))]);
  LReceipt := Call(LArgs);
  Check(Call(LArgs).ToJSON = LReceipt.ToJSON, 'Review retry retains its exact ticket');
  Result := LReceipt.Field('projectImport').Field('reviewID').AsText;
end;

procedure History(const ADirection: TNyxText);
begin
  Inc(GSerial);
  GSession.Call('nyx_history', 'Scooty', NyxObject([
    NyxField('expectedRevision', NyxData(GSession.Revision)),
    NyxField('operationId', NyxData('history-' + IntToStr(GSerial))),
    NyxField('direction', NyxData(ADirection))]), 'owner-one');
end;

procedure Run;
var
  LDocument: TNyxDocument;
  LAuthor: TNyxStudioSession;
  LCopy: TNyxAgentSession;
  LUpload: INyxProjectImportUpload;
  LNext: INyxProjectImportUpload;
  LPair: TNyxProjectPair;
  LSource: TNyxText;
  LPacket: TNyxText;
  LBefore: TNyxText;
  LImport: TNyxText;
  LTicket: TNyxText;
  LArgs: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LRevision: Integer;
  LRejected: Boolean;
begin
  LDocument := CreateNyxEditedFixture(LSource);
  try
    LPair := NyxProjectPair(TNyxCodec.Encode(LDocument), LSource);
  finally
    LDocument.Free;
  end;
  LPacket := EncodeNyxProject(LPair);
  GSession := TNyxAgentSession.Create;
  LBefore := Snapshot;
  LRevision := GSession.Revision;
  LUpload := NewNyxProjectImport(4);
  LNext := LUpload.Append(0, NyxScalarText($1f319));
  Check((LUpload.ReceivedBytes = 0) and (LNext.ReceivedBytes = 4) and
    (LNext.ScalarCount = 1), 'Immutable upload separates byte/scalar accounting');
  LRejected := False;
  try
    LNext.Append(1, 'x');
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and LNext.Complete, 'Over-reservation append retains completed value');
  Refuses(Arguments('begin-import', [NyxField('bytes', NyxData(4194305))]), 'Over-budget file refuses');
  LImport := Upload(LPacket);
  Check((GSession.Revision = LRevision) and (Snapshot = LBefore),
    'Staging has no design/source/history mutation');
  Check(ReadPart('inspect-import', LImport, 'input') = LPacket, 'Complete original input stays exact');
  Refuses(Arguments('inspect-import', [NyxField('import', NyxData(LImport))], False),
    'A second transport with the same display actor cannot inspect input', 'owner-two');
  Refuses(Arguments('append-import', [NyxField('import', NyxData(LImport)),
    NyxField('offset', NyxData(0)), NyxField('text', NyxData('x'))]), 'Out-of-order offset refuses');
  LTicket := Review(LImport, 'match');
  Check(ReadPart('inspect-import', LImport, 'source') = LSource, 'Handwritten helper/source stays exact');
  LCopy := GSession.Clone;
  try
    LCopy.ReleaseProjectImports('owner-one');
    Check(Call(Arguments('inspect-import', [NyxField('import', NyxData(LImport))], False))
      .Field('complete').AsBoolean, 'Rollback-copy retirement does not alias live uploads');
  finally
    LCopy.Free;
  end;
  Refuses(Arguments('apply', [NyxField('import', NyxData(LImport)),
    NyxField('reviewID', NyxData('wrong-ticket'))]), 'Wrong ticket preserves exact pair/history');
  LArgs := Arguments('apply', [NyxField('import', NyxData(LImport)), NyxField('reviewID', NyxData(LTicket))]);
  LReceipt := Call(LArgs);
  Check(Call(LArgs).ToJSON = LReceipt.ToJSON, 'Apply retry never adds history');
  Check(ReadPart('export', '', 'project') = LPacket, 'Imported pair preserves exact Unicode/numeric/source values');
  Check(GSession.Revision = LRevision + 1, 'Whole-file apply advances one revision');
  History('undo');
  Check(Snapshot = LBefore, 'One Undo restores complete previous design/Pascal');
  History('redo');
  Check(Snapshot = LPacket, 'One Redo restores complete imported design/Pascal');
  Refuses(Arguments('inspect-import', [NyxField('import', NyxData(LImport))], False),
    'Applied upload is retired');
  LImport := Upload('{}');
  Refuses(Arguments('review-import', [NyxField('import', NyxData(LImport)),
    NyxField('resolution', NyxData('match'))]), 'Malformed complete input refuses atomically');
  Check(ReadPart('inspect-import', LImport, 'input') = '{}', 'Malformed original remains inspectable');
  Call(Arguments('cancel-import', [NyxField('import', NyxData(LImport))]));
  Refuses(Arguments('inspect-import', [NyxField('import', NyxData(LImport))], False), 'Cancelled upload is retired');

  { Conflict resolutions use the existing public project admission contract.
    A saved independent draft must never disappear to make an import succeed. }
  LAuthor := TNyxStudioSession.Create(LPair);
  try
    LAuthor.Document.Title := 'Another design title';
    LPair.Design := LAuthor.Save;
  finally
    LAuthor.Free;
  end;
  LImport := Upload(EncodeNyxProject(LPair));
  Refuses(Arguments('review-import', [NyxField('import', NyxData(LImport)),
    NyxField('resolution', NyxData('match'))]), 'Divergent pair requires deliberate resolution');
  LTicket := Review(LImport, 'pascal');
  Check(ReadPart('inspect-import', LImport, 'source') = LSource, 'Pascal resolution retains source');
  LTicket := Review(LImport, 'design');
  Check(ReadPart('inspect-import', LImport, 'draft') = LSource, 'Design resolution retains original Pascal as draft');
  Call(Arguments('apply', [NyxField('import', NyxData(LImport)), NyxField('reviewID', NyxData(LTicket))]));
  LPair := DecodeNyxProject(Snapshot);
  Check(LPair.Pending and (LPair.Draft = LSource) and (LPair.DraftBase = LPair.Source),
    'Imported pending draft and baseline remain exact');
  LImport := Upload(LPacket);
  LTicket := Review(LImport, 'match');
  Refuses(Arguments('apply', [NyxField('import', NyxData(LImport)),
    NyxField('reviewID', NyxData(LTicket))]), 'Current pending draft blocks replacement');
  GSession.ReleaseProjectImports('owner-one');
  Refuses(Arguments('inspect-import', [NyxField('import', NyxData(LImport))], False), 'Disconnect retires private input');

  LArgs := Arguments('begin-import', [NyxField('bytes', NyxData(4194304))]);
  LReceipt := Call(LArgs);
  LImport := LReceipt.Field('projectImport').Field('import').AsText;
  Call(Arguments('begin-import', [NyxField('bytes', NyxData(4194304))]));
  Refuses(Arguments('begin-import', [NyxField('bytes', NyxData(1))]), 'Aggregate UTF-8 reservations refuse above 8 MiB');
  LCopy := TNyxAgentSession.CreateRecovered(GSession.RecoveryFrame);
  try
    LRejected := False;
    try
      LCopy.Call('nyx_project', 'Scooty', Arguments('inspect-import',
        [NyxField('import', NyxData(LImport))], False), 'owner-one');
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'Durable recovery excludes all private upload authority');
  finally
    LCopy.Free;
  end;
  GSession.ReleaseProjectImports('owner-one');

  LPair.Draft := 'Unfinished independent source / ' + NyxScalarText($1f319);
  LImport := Upload(EncodeNyxProject(LPair));
  LTicket := Review(LImport, 'match');
  Check(ReadPart('inspect-import', LImport, 'draft') = LPair.Draft,
    'Independent imported draft survives admission');
  Refuses(Arguments('review-import', [NyxField('import', NyxData(LImport)),
    NyxField('resolution', NyxData('design'))]), 'Design choice refuses a second independent Pascal buffer');
  GSession.ReleaseProjectImports('owner-one');
  LBefore := ReadPart('export', '', 'draft');
  GSession.Exchange(NyxObject([NyxField('op', NyxData('configure')),
    NyxField('permission', NyxData('readOnly'))]));
  Refuses(Arguments('begin-import', [NyxField('bytes', NyxData(2))]), 'Read-only forbids private staging');
  Check(ReadPart('export', '', 'draft') = LBefore, 'Read-only export retains exact pending source');
  GSession.Exchange(NyxObject([NyxField('op', NyxData('configure')),
    NyxField('permission', NyxData('disabled'))]));
  Refuses(Arguments('export', [NyxField('part', NyxData('project'))], False), 'Disabled refuses export');
end;

begin
  GSession := nil;
  try
    try
      Run;
      {$ifdef PAS2JS}
      document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' project import checks';
      document.body.setAttribute('data-test-result', 'passed');
      {$else}
      WriteLn('PASS ', GChecks, ' project import checks');
      {$endif}
    except
      on LException: Exception do
      begin
        {$ifdef PAS2JS}
        document.body.textContent := 'FAIL ' + LException.Message;
        document.body.setAttribute('data-test-result', 'failed');
        {$else}
        WriteLn(StdErr, LException.Message);
        ExitCode := 1;
        {$endif}
      end;
    end;
  finally
    GSession.Free;
  end;
end.
