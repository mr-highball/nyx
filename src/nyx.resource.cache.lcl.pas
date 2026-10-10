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
unit nyx.resource.cache.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.resources, nyx.resource.sources, nyx.resource.cache;

{ A private bounded native cache, defaulting below the user's temporary folder.
  An explicit root supports host configuration and isolated qualification.
  Only owned hashed .nyx-resource files are read/written. Writes stage a complete
  UTF-8 envelope then atomically replace that one entry; failures retain its
  previous bytes. Storage quotas refuse new writes without purging user files.
  Construction validates configuration but performs no filesystem creation.
  Missing folders are cold reads; unavailable storage reports through the job
  callback so a resolver can retain valid HTTP content in its private memory.
  The first eligible write creates the folder, inside that same failure boundary.
  Local I/O may complete synchronously. HTTP fetching belongs to the resolver. }
{ Bounded file I/O completes inline on the calling thread. Use one provider on
  that thread; this adapter does not serialize writers in another process or
  promise cross-process quota isolation. A loader must keep network work off the
  UI thread and marshal control publication separately. }
function NewNyxFileResourceCache(const ARoot: TNyxText = '';
  AEntryLimit: Integer = 128; AByteLimit: Integer = 16777216): INyxResourceCacheStorage;

implementation

uses SysUtils, Classes, MD5, nyx.bytes, nyx.data
  {$ifdef MSWINDOWS}, Windows{$endif};

type
  TCacheIOJob = class(TNyxResourceCacheJob);
  TFileCache = class(TInterfacedObject, INyxResourceCacheStorage)
  private
    FRoot: TNyxText;
    FEntryLimit: Integer;
    FByteLimit: Integer;
    FCounter: Integer;
    function Filename(const AURL: TNyxResourceURL; AKind: TNyxResourceKind): TNyxText;
    procedure Budget(const ATarget: TNyxText; ABytes: Integer);
  public
    constructor Create(const ARoot: TNyxText; AEntryLimit, AByteLimit: Integer);
    function Read(const AURL: TNyxResourceURL; AKind: TNyxResourceKind;
      AReply: TNyxResourceCacheRead): INyxResourceCacheJob;
    function Write(const AEntry: TNyxResourceCacheEntry;
      AReply: TNyxResourceCacheWrite): INyxResourceCacheJob;
  end;

constructor TFileCache.Create(const ARoot: TNyxText; AEntryLimit, AByteLimit: Integer);
begin
  inherited Create;

  if (AEntryLimit < 1) or (AEntryLimit > 128) or
    (AByteLimit < 1) or (AByteLimit > 16777216) then
  begin
    raise ENyxResource.Create('Native cache requires 1..128 entries and 1..16 MiB');
  end;
  FEntryLimit := AEntryLimit;
  FByteLimit := AByteLimit;
  FRoot := ARoot;

  if FRoot = '' then
  begin
    FRoot := TNyxText(GetTempDir(False)) + TNyxText('nyx-resource-cache-v1');
  end;
  FRoot := TNyxText(IncludeTrailingPathDelimiter(ExpandFileName(FRoot)));
  { Environment failures belong to Read/Write callbacks, rather than preventing
    a host from constructing its resolver or mounting unrelated embedded data.
    Invalid budgets above still refuse immediately as caller configuration. }
end;

function TFileCache.Filename(const AURL: TNyxResourceURL;
  AKind: TNyxResourceKind): TNyxText;
var
  LKey: TNyxText;
begin
  NyxResourceURL(AURL.Address);
  LKey := AURL.Address + TNyxText(#10) + NyxResourceKindName(AKind);
  { The digest supplies a bounded filename, not an authentication claim. Read
    still verifies exact URL and file kind inside the admitted envelope. }
  Result := FRoot + TNyxText(MD5Print(MD5Buffer(LKey[1], Length(LKey)))) +
    TNyxText('.nyx-resource');
end;

procedure TFileCache.Budget(const ATarget: TNyxText; ABytes: Integer);
var
  LSearch: TSearchRec;
  LCount: Integer;
  LBytes: Int64;
  LPath: TNyxText;
begin
  LCount := 1;
  LBytes := ABytes;

  if FindFirst(FRoot + TNyxText('*.nyx-resource'), faAnyFile, LSearch) = 0 then
  begin
    try
      repeat
        LPath := FRoot + TNyxText(LSearch.Name);

        if ((LSearch.Attr and faDirectory) = 0) and (LPath <> ATarget) then
        begin
          Inc(LCount);
          Inc(LBytes, LSearch.Size);
        end;
      until FindNext(LSearch) <> 0;
    finally
      SysUtils.FindClose(LSearch);
    end;
  end;

  if (LCount > FEntryLimit) or (LBytes > FByteLimit) then
  begin
    raise ENyxResource.Create('Native resource cache storage budget is full');
  end;
end;

function TFileCache.Read(const AURL: TNyxResourceURL; AKind: TNyxResourceKind;
  AReply: TNyxResourceCacheRead): INyxResourceCacheJob;
var
  LJob: TCacheIOJob;
  LPath: TNyxText;
  LStream: TFileStream;
  LBytes: TNyxBytes;
  LEntry: TNyxResourceCacheEntry;
  LFound: Boolean;
  LError: TNyxText;
begin
  LJob := TCacheIOJob.Create(AReply);
  Result := LJob;
  LEntry := Default(TNyxResourceCacheEntry);
  LFound := False;
  LError := '';
  try
    LPath := Filename(AURL, AKind);

    if FileExists(ExcludeTrailingPathDelimiter(FRoot)) then
    begin
      raise ENyxResource.Create('Native resource cache folder is unavailable');
    end;

    if FileExists(LPath) then
    begin
      LStream := TFileStream.Create(LPath, fmOpenRead or fmShareDenyWrite);
      try

        if (LStream.Size < 1) or (LStream.Size > 2 * NyxMaximumPackedBytes + 16384) then
        begin
          raise ENyxResource.Create('Cached envelope exceeds the bounded file budget');
        end;
        SetLength(LBytes, LStream.Size);
        LStream.ReadBuffer(LBytes[0], Length(LBytes));
      finally
        LStream.Free;
      end;
      LEntry := TNyxResourceCacheEntry.FromData(TNyxDataValue.ParseJSON(NyxDecodeUTF8(LBytes)));

      if (LEntry.URL.Address <> AURL.Address) or (LEntry.Kind <> AKind) then
      begin
        raise ENyxResource.Create('Cached envelope belongs to another URL or file kind');
      end;
      LFound := True;
    end;
  except
    on LException: Exception do
    begin
      LError := LException.Message;
    end;
  end;
  LJob.CompleteRead(LFound, LEntry, LError);
end;

function TFileCache.Write(const AEntry: TNyxResourceCacheEntry;
  AReply: TNyxResourceCacheWrite): INyxResourceCacheJob;
var
  LJob: TCacheIOJob;
  LEntry: TNyxResourceCacheEntry;
  LBytes: TNyxBytes;
  LPath: TNyxText;
  LTemporary: TNyxText;
  LStream: TFileStream;
  LError: TNyxText;
begin
  LJob := TCacheIOJob.Create(AReply);
  Result := LJob;
  LError := '';
  LTemporary := '';
  try
    LEntry := TNyxResourceCacheEntry.FromData(AEntry.ToData);
    LPath := Filename(LEntry.URL, LEntry.Kind);
    LBytes := NyxEncodeUTF8(LEntry.ToData.ToJSON);

    if not ForceDirectories(FRoot) then
    begin
      raise ENyxResource.Create('Cannot create the native resource cache folder');
    end;
    Budget(LPath, Length(LBytes));
    Inc(FCounter);
    LTemporary := LPath + TNyxText('.') + TNyxText(IntToStr(GetProcessID)) +
      TNyxText('.') + TNyxText(IntToStr(FCounter)) + TNyxText('.pending');
    LStream := TFileStream.Create(LTemporary, fmCreate);
    try
      LStream.WriteBuffer(LBytes[0], Length(LBytes));
    finally
      LStream.Free;
    end;
    {$ifdef MSWINDOWS}

    if not MoveFileExW(PWideChar(UTF8Decode(LTemporary)), PWideChar(UTF8Decode(LPath)),
      MOVEFILE_REPLACE_EXISTING or MOVEFILE_WRITE_THROUGH) then
    begin
      raise ENyxResource.Create('Cannot atomically publish the native cache entry');
    end;
    {$else}

    if not RenameFile(LTemporary, LPath) then
    begin
      raise ENyxResource.Create('Cannot atomically publish the native cache entry');
    end;
    {$endif}
    LTemporary := '';
  except
    on LException: Exception do
    begin
      LError := LException.Message;
    end;
  end;

  if LTemporary <> '' then
  begin
    SysUtils.DeleteFile(LTemporary);
  end;
  LJob.CompleteWrite(LError = '', LError);
end;

function NewNyxFileResourceCache(const ARoot: TNyxText;
  AEntryLimit, AByteLimit: Integer): INyxResourceCacheStorage;
begin
  Result := TFileCache.Create(ARoot, AEntryLimit, AByteLimit);
end;

end.
