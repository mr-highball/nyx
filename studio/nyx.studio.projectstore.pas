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

unit nyx.studio.projectstore;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text,
  nyx.model,
  nyx.studio.projects;

type
  { Native repository for adjacent design/Pascal files and a portable recovery
    packet. A complete flushed write-ahead packet commits the save; interrupted
    member writes are rolled forward before the next repository read. Readers
    through this store never observe a partially published pair. External editors
    can see individual replacements and should reload after the save completes.
    The Studio service serializes calls. This class is not a concurrent lock. }
  TNyxProjectStore = class
  private
    FRoot: TNyxText;
    function DirectoryFor(const AName: TNyxText): TNyxText;
    procedure Publish(const ADirectory: TNyxText; const APair: TNyxProjectPair);
    procedure Recover(const ADirectory: TNyxText);
  public
    { Owns only explicit project directories below this root. Never deletes a
      project. Root is supplied by the local host, never by an HTTP client. }
    constructor Create(const ARoot: TNyxText);
    { Missing names return an empty packet/revision. Existing files are read exactly;
      divergent external edits are returned for explicit client-side admission.
      Revision covers both files and recovery metadata, not timestamps. }
    function ReadProject(const AName: TNyxText; out ARevision: TNyxText): TNyxText;
    { Strictly admits the complete candidate before checking expected revision or
      touching disk. False denotes an optimistic conflict and returns the remote
      packet/revision. A successful save retains the prior packet as previous.
      Empty expected revision creates only a genuinely absent project. }
    function SaveProject(const AName, AExpected: TNyxText;
      const APair: TNyxProjectPair; out ARevision, ARemote: TNyxText): Boolean;
    property Root: TNyxText read FRoot;
  end;

implementation

uses
  Classes,
  SysUtils,
  md5,
  nyx.source;

const
  ProjectPacketFile = 'project.nyxproject';
  ProjectJournalFile = 'pending.nyxproject';
  ProjectDesignFile = 'design.nyx';

function ReadText(const APath: TNyxText): TNyxText;
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try

    if LStream.Size > 4 * 1024 * 1024 then
    begin
      raise ENyxModel.Create('Project member exceeds the file budget');
    end;
    SetLength(Result, LStream.Size);
    SetCodePage(RawByteString(Result), CP_UTF8, False);

    if LStream.Size > 0 then
    begin
      LStream.ReadBuffer(Result[1], LStream.Size);
    end;
  finally
    LStream.Free;
  end;
end;

procedure WriteText(const APath, AText: TNyxText);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath, fmCreate or fmShareExclusive);
  try

    if AText <> '' then
    begin
      LStream.WriteBuffer(AText[1], Length(AText));
    end;

    if not FileFlush(LStream.Handle) then
    begin
      raise ENyxModel.Create('Cannot flush the project file');
    end;
  finally
    LStream.Free;
  end;
end;

procedure ReplaceText(const APath, AText: TNyxText);
var
  LTemporary: TNyxText;
begin
  LTemporary := APath + '.next';
  WriteText(LTemporary, AText);
  { The complete journal is already durable before member replacement. A gap
    between delete and rename is recoverable; callers must not read past it. }

  if FileExists(APath) and not DeleteFile(APath) then
  begin
    raise ENyxModel.Create('Cannot replace a project member');
  end;

  if not RenameFile(LTemporary, APath) then
  begin
    raise ENyxModel.Create('Cannot publish a project member');
  end;
end;

procedure ValidatePair(const APair: TNyxProjectPair);
var
  LDocument: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LResolved: TNyxProjectPair;
begin
  AdmitNyxProject(APair, nprRequireMatch, LDocument, LWorkspace, LResolved);
  LWorkspace.Free;
  LDocument.Free;
end;

constructor TNyxProjectStore.Create(const ARoot: TNyxText);
begin
  inherited Create;
  FRoot := IncludeTrailingPathDelimiter(ExpandFileName(ARoot));

  if not ForceDirectories(FRoot) then
  begin
    raise ENyxModel.Create('Cannot create the local project directory');
  end;
end;

function TNyxProjectStore.DirectoryFor(const AName: TNyxText): TNyxText;
var
  LLink: TRawbyteSymLinkRec;
begin
  ValidateNyxProjectName(AName);
  Result := IncludeTrailingPathDelimiter(ExpandFileName(FRoot + AName));

  if Pos(FRoot, Result) <> 1 then
  begin
    raise ENyxModel.Create('Project escaped its storage root');
  end;
  { Host-owned roots must not contain links/junctions to another directory.
    Refuse them instead of following a syntactically confined redirected path.
    The RTL's link query handles target details on each native platform; its
    platform-marked attribute bit does not belong in the portable store. }

  if DirectoryExists(Result) and
    FileGetSymLinkTarget(ExcludeTrailingPathDelimiter(Result), LLink) then
  begin
    raise ENyxModel.Create('Project directories cannot be symbolic links');
  end;
end;

procedure TNyxProjectStore.Publish(const ADirectory: TNyxText;
  const APair: TNyxProjectPair);
begin
  ReplaceText(ADirectory + ProjectDesignFile, APair.Design);
  ReplaceText(ADirectory + NyxCompanionUnitName(APair.Source) + '.pas', APair.Source);
  ReplaceText(ADirectory + ProjectPacketFile, EncodeNyxProject(APair));

  if not DeleteFile(ADirectory + ProjectJournalFile) then
  begin
    raise ENyxModel.Create('Project committed but its recovery journal could not be cleared');
  end;
end;

procedure TNyxProjectStore.Recover(const ADirectory: TNyxText);
var
  LPair: TNyxProjectPair;
begin

  if FileExists(ADirectory + ProjectJournalFile) then
  begin
    LPair := DecodeNyxProject(ReadText(ADirectory + ProjectJournalFile));
    ValidatePair(LPair);
    Publish(ADirectory, LPair);
  end;
end;

function TNyxProjectStore.ReadProject(const AName: TNyxText;
  out ARevision: TNyxText): TNyxText;
var
  LDirectory: TNyxText;
  LPacket: TNyxText;
  LPair: TNyxProjectPair;
  LSourcePath: TNyxText;
  LFingerprint: TNyxStrings;
  LBytes: TNyxText;
begin
  ARevision := '';
  Result := '';
  LDirectory := DirectoryFor(AName);
  Recover(LDirectory);

  if not FileExists(LDirectory + ProjectPacketFile) then
  begin

    if DirectoryExists(LDirectory) then
    begin
      raise ENyxModel.Create('Existing project directory has no recovery packet; import its files');
    end;
    Exit;
  end;
  LPacket := ReadText(LDirectory + ProjectPacketFile);
  LPair := DecodeNyxProject(LPacket);
  LSourcePath := LDirectory + NyxCompanionUnitName(LPair.Source) + '.pas';
  { Do not repair direct edits from yesterday's packet. Exact current files
    determine the revision and travel to Studio for mismatch resolution. }
  LPair.Design := ReadText(LDirectory + ProjectDesignFile);
  LPair.Source := ReadText(LSourcePath);
  Result := EncodeNyxProject(LPair);
  LFingerprint := TNyxStrings.Create;
  try
    LFingerprint.Add(IntToStr(Length(LPacket)));
    LFingerprint.Add(':');
    LFingerprint.Add(LPacket);
    LFingerprint.Add(Result);
    LBytes := LFingerprint.Join;
    { Hash raw UTF-8 bytes: the MD5 String overload on older FPC can convert an
      argument through the system ANSI codepage before hashing it. This revision
      is an optimistic concurrency marker, not an authentication credential. }
    ARevision := MD5Print(MD5Buffer(LBytes[1], Length(LBytes)));
  finally
    LFingerprint.Free;
  end;
end;

function TNyxProjectStore.SaveProject(const AName, AExpected: TNyxText;
  const APair: TNyxProjectPair; out ARevision, ARemote: TNyxText): Boolean;
var
  LDirectory: TNyxText;
  LCurrent: TNyxText;
begin
  ValidatePair(APair);
  LDirectory := DirectoryFor(AName);
  LCurrent := ReadProject(AName, ARevision);
  ARemote := LCurrent;
  Result := False;

  if ARevision <> AExpected then
  begin
    Exit;
  end;

  if not ForceDirectories(LDirectory) then
  begin
    raise ENyxModel.Create('Cannot create the project directory');
  end;

  if LCurrent <> '' then
  begin
    ReplaceText(LDirectory + 'previous.nyxproject', LCurrent);
  end;
  { Write/flush first, then rename a complete packet to the commit journal.
    An incomplete .next is never recovered as an accepted transaction. }
  WriteText(LDirectory + ProjectJournalFile + '.next', EncodeNyxProject(APair));

  if not RenameFile(LDirectory + ProjectJournalFile + '.next',
    LDirectory + ProjectJournalFile) then
  begin
    raise ENyxModel.Create('Cannot commit the project recovery packet');
  end;
  Publish(LDirectory, APair);
  ARemote := ReadProject(AName, ARevision);
  Result := True;
end;

end.
