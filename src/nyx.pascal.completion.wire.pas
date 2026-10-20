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

unit nyx.pascal.completion.wire;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.pascal.completion;

{ Private helper transport, not a project import/export or compiler authority.
  Version/operation/ID/fields are strict. All paths remain logical source leaves.
  The byte limits cover encoded UTF-8, including escaped NUL/supplementary data;
  callers may select smaller budgets. The current JSON codec caps requests at
  4 MiB and each container at 1024 members; larger projects need another provider
  transport. Portable direct-provider queries retain their own source limits. }
function EncodeNyxCompletionRequest(const ARequest: INyxPascalCompletionRequest;
  const AID: TNyxText; AMaximumBytes: Integer = 4 * 1024 * 1024): TNyxText;
function DecodeNyxCompletionRequest(const AText: TNyxText; out AID: TNyxText;
  AMaximumBytes: Integer = 4 * 1024 * 1024): INyxPascalCompletionRequest;
{ A reply is rebound only to the supplied exact frozen query and matching ID.
  There is no wire field for execution provenance, session publication or editor
  mutation. Version 1 bytes remain unchanged when project definitions are absent.
  Version 2 adds strict optional project-local scalar locations, never lexical
  snapshots or SDK files. Public capture revalidates all range/row/text budgets. }
function EncodeNyxCompletionReply(const AResult: INyxPascalCompletionResult;
  const AID: TNyxText; AMaximumBytes: Integer = 1024 * 1024): TNyxText;
function DecodeNyxCompletionReply(const AText, AID: TNyxText;
  const ARequest: INyxPascalCompletionRequest;
  AMaximumBytes: Integer = 1024 * 1024): INyxPascalCompletionResult;

implementation

uses SysUtils, nyx.bytes, nyx.data, nyx.json, nyx.editing,
  nyx.pascal.project, nyx.pascal.sources, nyx.pascal.drafts, nyx.pascal.names;

type
  { Temporary decoder row contains only copied text/value references, never
    COM interfaces inside a record. Capture validates its complete membership. }
  TWireFile = record
    Ref: TNyxPascalSourceRef;
    Accepted: TNyxText;
    Buffer: TNyxText;
  end;

  { Read-only untrusted transport staging. Only the detached capture escapes;
    this object is never an INyxPascalProject and owns no compiler capability. }
  TWireSources = class(TInterfacedObject, INyxPascalSourceSnapshot)
  public
    SourceKind: TNyxPascalSourceSnapshotKind;
    UnitRef: TNyxPascalUnitRef;
    Bounds: TNyxPascalProjectLimits;
    Files: array of TWireFile;
    function Kind: TNyxPascalSourceSnapshotKind;
    function MainUnit: TNyxPascalUnitRef;
    function Limits: TNyxPascalProjectLimits;
    function Count: Integer;
    function FileReference(AIndex: Integer): TNyxPascalSourceRef;
    function Text(const AFile: TNyxPascalSourceRef): TNyxText;
    function AcceptedText(const AFile: TNyxPascalSourceRef): TNyxText;
    function WithBuffer(const AFile: TNyxPascalSourceRef;
      const AText: TNyxText): INyxPascalSourceSnapshot;
  end;

const
  CStatuses: array[TNyxPascalCompletionStatus] of TNyxText =
    ('ready', 'unavailable', 'failed', 'cancelled');
  CKinds: array[TNyxPascalCompletionKind] of TNyxText =
    ('unknown', 'type', 'method', 'property', 'field', 'variable', 'constant',
      'parameter', 'unit', 'keyword');

procedure Fields(const AValue: TNyxDataValue; const ANames: array of TNyxText);
var
  LAt: Integer;
  LName: Integer;
  LFound: Boolean;
begin

  if (AValue.Kind <> ndObject) or (AValue.Count <> Length(ANames)) then
  begin
    raise ENyxPascalProject.Create('Completion packet fields do not match this protocol');
  end;
  for LAt := 0 to AValue.Count - 1 do
  begin
    LFound := False;
    for LName := 0 to High(ANames) do
    begin

      if AValue.Key(LAt) = ANames[LName] then
      begin
        LFound := True;
        Break;
      end;
    end;

    if not LFound then
    begin
      raise ENyxPascalProject.Create('Unknown completion packet field');
    end;
  end;
end;

procedure Budget(const AText: TNyxText; AMaximum: Integer);
begin

  if (AMaximum < 1) or (AMaximum > NyxMaximumJSONBytes) or
    (NyxUTF8ByteCount(AText) > AMaximum) then
  begin
    raise ENyxPascalProject.Create('Completion packet exceeds its UTF-8 transport budget');
  end;
end;

procedure ID(const AID: TNyxText);
begin

  if (AID = '') or (NyxTextScalarCount(AID) > 120) then
  begin
    raise ENyxPascalProject.Create('Completion transport needs a bounded correlation identity');
  end;
end;

function RefData(const ARef: TNyxPascalSourceRef): TNyxDataValue;
const
  CNames: array[TNyxPascalSourceRole] of TNyxText = ('main', 'unit', 'include');
begin
  ARef.Validate;
  Result := NyxObject([NyxField('kind', NyxData(CNames[ARef.Role])),
    NyxField('name', NyxData(ARef.Name))]);
end;

function RefValue(const AValue: TNyxDataValue): TNyxPascalSourceRef;
var
  LKind: TNyxText;
  LName: TNyxText;
begin
  Fields(AValue, ['kind', 'name']);
  LKind := AValue.Field('kind').AsText;
  LName := AValue.Field('name').AsText;

  if (LKind = 'main') and (LName = '') then
  begin
    Exit(TNyxPascalSourceRef.Main);
  end;

  if (LKind = 'unit') and (Length(LName) > 4) and
    (Copy(LName, Length(LName) - 3, 4) = '.pas') then
  begin
    Exit(TNyxPascalSourceRef.UnitFile(NyxPascalUnit(Copy(LName, 1, Length(LName) - 4))));
  end;

  if LKind = 'include' then
  begin
    Exit(TNyxPascalSourceRef.IncludeFile(NyxPascalInclude(LName)));
  end;
  raise ENyxPascalProject.Create('Unknown logical completion source reference');
end;

function TWireSources.Kind: TNyxPascalSourceSnapshotKind;
begin
  Result := SourceKind;
end;

function TWireSources.MainUnit: TNyxPascalUnitRef;
begin
  Result := UnitRef;
end;

function TWireSources.Limits: TNyxPascalProjectLimits;
begin
  Result := Bounds;
end;

function TWireSources.Count: Integer;
begin
  Result := Length(Files);
end;

function TWireSources.FileReference(AIndex: Integer): TNyxPascalSourceRef;
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ENyxPascalProject.Create('Completion source ordinal is out of range');
  end;
  Result := Files[AIndex].Ref;
end;

function TWireSources.Text(const AFile: TNyxPascalSourceRef): TNyxText;
var
  LAt: Integer;
begin
  for LAt := 0 to High(Files) do
  begin

    if Files[LAt].Ref.SameFile(AFile) then
    begin
      Exit(Files[LAt].Buffer);
    end;
  end;
  raise ENyxPascalProject.Create('Completion source is not captured');
end;

function TWireSources.AcceptedText(const AFile: TNyxPascalSourceRef): TNyxText;
var
  LAt: Integer;
begin
  for LAt := 0 to High(Files) do
  begin

    if Files[LAt].Ref.SameFile(AFile) then
    begin
      Exit(Files[LAt].Accepted);
    end;
  end;
  raise ENyxPascalProject.Create('Accepted completion source is not captured');
end;

function TWireSources.WithBuffer(const AFile: TNyxPascalSourceRef;
  const AText: TNyxText): INyxPascalSourceSnapshot;
begin
  Result := CaptureNyxPascalSources(Self as INyxPascalSourceSnapshot).WithBuffer(AFile, AText);
end;

function EncodeNyxCompletionRequest(const ARequest: INyxPascalCompletionRequest;
  const AID: TNyxText; AMaximumBytes: Integer): TNyxText;
var
  LRequest: INyxPascalCompletionRequest;
  LSources: INyxPascalSourceSnapshot;
  LFiles: array of TNyxDataValue;
  LAt: Integer;
  LRef: TNyxPascalSourceRef;
begin
  ID(AID);
  LRequest := CaptureNyxPascalCompletionRequest(ARequest);
  LSources := LRequest.Sources;
  SetLength(LFiles, LSources.Count);
  for LAt := 0 to LSources.Count - 1 do
  begin
    LRef := LSources.FileReference(LAt);
    LFiles[LAt] := NyxObject([NyxField('file', RefData(LRef)),
      NyxField('accepted', NyxData(LSources.AcceptedText(LRef))),
      NyxField('buffer', NyxData(LSources.Text(LRef)))]);
  end;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('op', NyxData('complete')), NyxField('id', NyxData(AID)),
    NyxField('origin', NyxData(Ord(LSources.Kind))),
    NyxField('main', NyxData(LSources.MainUnit.Name)),
    NyxField('maximumFiles', NyxData(LSources.Limits.MaximumFiles)),
    NyxField('maximumBytes', NyxData(LSources.Limits.MaximumBytes)),
    NyxField('files', NyxArray(LFiles)), NyxField('owner', RefData(LRequest.Owner)),
    NyxField('file', RefData(LRequest.FileReference)),
    NyxField('caret', NyxData(LRequest.Caret)), NyxField('offset', NyxData(LRequest.Offset)),
    NyxField('limit', NyxData(LRequest.Limit))]).ToJSON;
  Budget(Result, AMaximumBytes);
end;

function DecodeNyxCompletionRequest(const AText: TNyxText; out AID: TNyxText;
  AMaximumBytes: Integer): INyxPascalCompletionRequest;
var
  LPacket: TNyxDataValue;
  LFiles: TNyxDataValue;
  LRow: TNyxDataValue;
  LSources: TWireSources;
  LHold: INyxPascalSourceSnapshot;
  LAt: Integer;
  LOrigin: Integer;
  LID: TNyxText;
begin
  AID := '';
  Budget(AText, AMaximumBytes);
  LPacket := TNyxDataValue.ParseJSON(AText);
  Fields(LPacket, ['version', 'op', 'id', 'origin', 'main', 'maximumFiles',
    'maximumBytes', 'files', 'owner', 'file', 'caret', 'offset', 'limit']);

  if (LPacket.Field('version').AsInteger <> 1) or
    (LPacket.Field('op').AsText <> 'complete') then
  begin
    raise ENyxPascalProject.Create('Unsupported completion request protocol');
  end;
  LID := LPacket.Field('id').AsText;
  ID(LID);
  LSources := TWireSources.Create;
  LHold := LSources;
  LOrigin := LPacket.Field('origin').AsInteger;

  if (LOrigin < Ord(Low(TNyxPascalSourceSnapshotKind))) or
    (LOrigin > Ord(High(TNyxPascalSourceSnapshotKind))) then
  begin
    raise ENyxPascalProject.Create('Unknown completion source origin');
  end;
  LSources.SourceKind := TNyxPascalSourceSnapshotKind(LOrigin);
  LSources.UnitRef := NyxPascalUnit(LPacket.Field('main').AsText);
  LSources.Bounds := TNyxPascalProjectLimits.Defaults
    .UpToFiles(LPacket.Field('maximumFiles').AsInteger)
    .UpToBytes(LPacket.Field('maximumBytes').AsInteger);
  LFiles := LPacket.Field('files');

  if (LFiles.Kind <> ndArray) or (LFiles.Count < 1) or
    (LFiles.Count > LSources.Bounds.MaximumFiles) then
  begin
    raise ENyxPascalProject.Create('Invalid completion source membership budget');
  end;
  SetLength(LSources.Files, LFiles.Count);
  for LAt := 0 to LFiles.Count - 1 do
  begin
    LRow := LFiles.Item(LAt);
    Fields(LRow, ['file', 'accepted', 'buffer']);
    LSources.Files[LAt].Ref := RefValue(LRow.Field('file'));
    LSources.Files[LAt].Accepted := LRow.Field('accepted').AsText;
    LSources.Files[LAt].Buffer := LRow.Field('buffer').AsText;
  end;
  Result := CaptureNyxPascalCompletionRequest(NewNyxPascalCompletionRequest(LHold)
    .At(RefValue(LPacket.Field('file')), LPacket.Field('caret').AsInteger)
    .From(RefValue(LPacket.Field('owner')))
    .Skip(LPacket.Field('offset').AsInteger).Take(LPacket.Field('limit').AsInteger));
  AID := LID;
end;

function EncodeNyxCompletionReply(const AResult: INyxPascalCompletionResult;
  const AID: TNyxText; AMaximumBytes: Integer): TNyxText;
var
  LItems: array of TNyxDataValue;
  LItem: TNyxPascalCompletionItem;
  LAt: Integer;
  LChecked: INyxPascalCompletionResult;
  LDefinitions: INyxPascalCompletionDefinitions;
  LLocations: array of TNyxDataValue;
  LLocation: TNyxPascalCompletionLocation;
  LHasDefinitions: Boolean;
begin
  ID(AID);
  LChecked := CaptureNyxPascalCompletionResult(AResult);
  SetLength(LItems, LChecked.Count);
  SetLength(LLocations, LChecked.Count);
  LHasDefinitions := False;
  Supports(LChecked, INyxPascalCompletionDefinitions, LDefinitions);
  for LAt := 0 to LChecked.Count - 1 do
  begin
    LItem := LChecked.Item(LAt);
    LItems[LAt] := NyxObject([NyxField('name', NyxData(LItem.Name)),
      NyxField('insert', NyxData(LItem.InsertText)), NyxField('detail', NyxData(LItem.Detail)),
      NyxField('kind', NyxData(CKinds[LItem.Kind]))]);
    LLocations[LAt] := NyxNull;

    if LDefinitions <> nil then
    begin
      LLocation := LDefinitions.DefinitionLocation(LAt);

      if LLocation.Available then
      begin
        LHasDefinitions := True;
        LLocations[LAt] := NyxObject([
          NyxField('file', RefData(LLocation.FileReference)),
          NyxField('start', NyxData(LLocation.Start)),
          NyxField('length', NyxData(LLocation.Length)),
          NyxField('name', NyxData(LLocation.Name))]);
      end;
    end;
  end;

  if LHasDefinitions then
  begin
    Result := NyxObject([NyxField('version', NyxData(2)), NyxField('id', NyxData(AID)),
      NyxField('status', NyxData(CStatuses[LChecked.Status])),
      NyxField('message', NyxData(LChecked.MessageText)),
      NyxField('start', NyxData(LChecked.ReplaceStart)),
      NyxField('expected', NyxData(LChecked.ExpectedText)),
      NyxField('total', NyxData(LChecked.Total)), NyxField('items', NyxArray(LItems)),
      NyxField('definitions', NyxArray(LLocations))]).ToJSON;
  end
  else
  begin
    Result := NyxObject([NyxField('version', NyxData(1)), NyxField('id', NyxData(AID)),
      NyxField('status', NyxData(CStatuses[LChecked.Status])),
      NyxField('message', NyxData(LChecked.MessageText)),
      NyxField('start', NyxData(LChecked.ReplaceStart)),
      NyxField('expected', NyxData(LChecked.ExpectedText)),
      NyxField('total', NyxData(LChecked.Total)), NyxField('items', NyxArray(LItems))]).ToJSON;
  end;
  Budget(Result, AMaximumBytes);
end;

function DecodeNyxCompletionReply(const AText, AID: TNyxText;
  const ARequest: INyxPascalCompletionRequest;
  AMaximumBytes: Integer): INyxPascalCompletionResult;
var
  LPacket: TNyxDataValue;
  LItems: TNyxDataValue;
  LRow: TNyxDataValue;
  LValues: array of TNyxPascalCompletionItem;
  LStatus: TNyxPascalCompletionStatus;
  LKind: TNyxPascalCompletionKind;
  LAt: Integer;
  LFound: Boolean;
  LName: TNyxText;
  LVersion: Integer;
  LDefinitions: TNyxDataValue;
  LLocations: array of TNyxPascalCompletionLocation;
  LBase: INyxPascalCompletionResult;
begin
  Result := nil;
  ID(AID);
  Budget(AText, AMaximumBytes);
  LPacket := TNyxDataValue.ParseJSON(AText);
  LVersion := LPacket.Field('version').AsInteger;

  if LVersion = 1 then
  begin
    Fields(LPacket, ['version', 'id', 'status', 'message', 'start', 'expected', 'total', 'items']);
  end
  else if LVersion = 2 then
  begin
    Fields(LPacket, ['version', 'id', 'status', 'message', 'start', 'expected', 'total', 'items',
      'definitions']);
  end
  else
  begin
    raise ENyxPascalProject.Create('Unsupported completion reply protocol');
  end;

  if LPacket.Field('id').AsText <> AID then
  begin
    raise ENyxPascalProject.Create('Completion reply belongs to another protocol/request');
  end;
  LName := LPacket.Field('status').AsText;
  LFound := False;
  for LStatus := Low(TNyxPascalCompletionStatus) to High(TNyxPascalCompletionStatus) do
  begin

    if CStatuses[LStatus] = LName then
    begin
      LFound := True;
      Break;
    end;
  end;

  if not LFound then
  begin
    raise ENyxPascalProject.Create('Unknown completion reply status');
  end;
  LItems := LPacket.Field('items');

  if (LItems.Kind <> ndArray) or (LItems.Count > 50) then
  begin
    raise ENyxPascalProject.Create('Completion reply exceeds its row budget');
  end;
  SetLength(LValues, LItems.Count);
  for LAt := 0 to LItems.Count - 1 do
  begin
    LRow := LItems.Item(LAt);
    Fields(LRow, ['name', 'insert', 'detail', 'kind']);
    LName := LRow.Field('kind').AsText;
    LFound := False;
    for LKind := Low(TNyxPascalCompletionKind) to High(TNyxPascalCompletionKind) do
    begin

      if CKinds[LKind] = LName then
      begin
        LFound := True;
        Break;
      end;
    end;

    if not LFound then
    begin
      raise ENyxPascalProject.Create('Unknown completion item kind');
    end;
    LValues[LAt] := NyxPascalCompletion(LRow.Field('name').AsText,
      LRow.Field('insert').AsText, LRow.Field('detail').AsText, LKind);
  end;
  LBase := NewNyxPascalCompletionResult(ARequest, LStatus, LPacket.Field('message').AsText,
    LPacket.Field('start').AsInteger, LPacket.Field('expected').AsText,
    LValues, LPacket.Field('total').AsInteger);

  if LVersion = 2 then
  begin
    LDefinitions := LPacket.Field('definitions');

    if (LDefinitions.Kind <> ndArray) or (LDefinitions.Count <> Length(LValues)) then
    begin
      raise ENyxPascalProject.Create('Completion definitions do not match their reply rows');
    end;
    SetLength(LLocations, Length(LValues));
    for LAt := 0 to High(LLocations) do
    begin
      LRow := LDefinitions.Item(LAt);

      if LRow.Kind <> ndNull then
      begin
        Fields(LRow, ['file', 'start', 'length', 'name']);
        LLocations[LAt] := NyxPascalCompletionLocation(RefValue(LRow.Field('file')),
          LRow.Field('start').AsInteger, LRow.Field('length').AsInteger,
          LRow.Field('name').AsText);
      end;
    end;
    LBase := NewNyxPascalCompletionDefinitions(LBase, LLocations);
  end;
  Result := LBase;
end;

end.
