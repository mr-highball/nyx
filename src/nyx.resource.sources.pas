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
unit nyx.resource.sources;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.bytes, nyx.data;

type
  { Resource transport and caching are closed choices, independent of the file's
    image/JSON/text/binary interpretation. Persistent means the target's resource
    cache: user temporary storage natively, browser Cache Storage where available.
    Server restrictions are respected by default; Override is an explicit policy
    for Nyx-managed storage and cannot alter the browser's own HTTP cache. }
  TNyxResourceSourceKind = (rskEmbedded, rskHosted);
  TNyxResourceCacheMode = (rcmBypass, rcmMemory, rcmPersistent);
  TNyxResourceServerPolicy = (rcspRespect, rcspOverride);

  { Absolute HTTP(S) location. Preserve exact path/query spelling; neither a
    machine filename nor an executable URI is admitted as a hosted resource. }
  TNyxResourceURL = record
  private
    FAddress: TNyxText;
  public
    property Address: TNyxText read FAddress;
  end;

  { Immutable cache policy. FreshFor and StaleFor use seconds; zero freshness
    requires validation before reuse. Stale data is opt-in and usable only after
    a loading failure, within the extra stale window. MaximumBytes limits both
    downloaded and cached payload admission. Every fluent operation returns a
    copy, so deriving one resource's policy never changes another resource. }
  TNyxResourceCachePolicy = record
  private
    FMode: TNyxResourceCacheMode;
    FFreshSeconds: Integer;
    FStaleSeconds: Integer;
    FMaximumBytes: Integer;
    FServerPolicy: TNyxResourceServerPolicy;
  public
    function Bypass: TNyxResourceCachePolicy;
    function Memory: TNyxResourceCachePolicy;
    function Persistent: TNyxResourceCachePolicy;
    function FreshFor(ASeconds: Integer): TNyxResourceCachePolicy;
    function StaleFor(ASeconds: Integer): TNyxResourceCachePolicy;
    function MaximumBytes(ABytes: Integer): TNyxResourceCachePolicy;
    function ServerPolicy(APolicy: TNyxResourceServerPolicy): TNyxResourceCachePolicy;
    procedure Validate;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxResourceCachePolicy; static;
    property Mode: TNyxResourceCacheMode read FMode;
    property FreshSeconds: Integer read FFreshSeconds;
    property StaleSeconds: Integer read FStaleSeconds;
    property ByteLimit: Integer read FMaximumBytes;
    property Server: TNyxResourceServerPolicy read FServerPolicy;
  end;

  { One portable location abstraction for packed files and hosted data. Bytes
    returns independent embedded bytes and refuses an unresolved hosted source.
    Network/caching adapters consume hosted URL/policy; no target handle, request
    callback, document or cache implementation is retained in the design. }
  TNyxResourceSource = record
  private
    FKind: TNyxResourceSourceKind;
    FEncoded: TNyxText;
    FURL: TNyxResourceURL;
    FCache: TNyxResourceCachePolicy;
  public
    function Cache(const APolicy: TNyxResourceCachePolicy): TNyxResourceSource;
    function Bytes: TNyxBytes;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxResourceSource; static;
    property Kind: TNyxResourceSourceKind read FKind;
    property URL: TNyxResourceURL read FURL;
    property CachePolicy: TNyxResourceCachePolicy read FCache;
  end;

function NyxResourceURL(const AAddress: TNyxText): TNyxResourceURL;
function NyxResourceCache: TNyxResourceCachePolicy;
function NyxEmbeddedResourceSource(const ABytes: TNyxBytes): TNyxResourceSource;
function NyxHostedResourceSource(const AURL: TNyxResourceURL): TNyxResourceSource;

implementation

function NyxResourceURL(const AAddress: TNyxText): TNyxResourceURL;
var
  LIndex: Integer;
  LScalar: Integer;
  LStart: Integer;
  LEnd: Integer;
  LScheme: TNyxText;
begin
  NyxUTF8ByteCount(AAddress);

  if (Length(AAddress) > 8192) or (AAddress = '') then
  begin
    raise ENyxBytes.Create('Hosted resource URL requires 1..8192 characters');
  end;
  LStart := Pos('://', AAddress);
  LScheme := LowerCase(Copy(AAddress, 1, LStart - 1));

  if (LStart = 0) or ((LScheme <> 'http') and (LScheme <> 'https')) then
  begin
    raise ENyxBytes.Create('Hosted resources require an absolute HTTP(S) URL');
  end;
  LEnd := LStart + 3;
  while (LEnd <= Length(AAddress)) and not (AAddress[LEnd] in ['/', '?', '#']) do
  begin

    if AAddress[LEnd] = '@' then
    begin
      raise ENyxBytes.Create('Hosted resource credentials belong to the resolver');
    end;
    Inc(LEnd);
  end;

  if LEnd = LStart + 3 then
  begin
    raise ENyxBytes.Create('Hosted resource URL requires a host');
  end;
  LIndex := 1;
  while LIndex <= Length(AAddress) do
  begin
    NyxNextScalar(AAddress, LIndex, LScalar);

    if (LScalar <= 32) or (LScalar = 127) or (LScalar = 92) then
    begin
      raise ENyxBytes.Create('Hosted resource URL requires escaped whitespace and valid URI characters');
    end;
  end;
  Result.FAddress := AAddress;
end;

function NyxResourceCache: TNyxResourceCachePolicy;
begin
  Result := Default(TNyxResourceCachePolicy);
  Result.FMode := rcmPersistent;
  Result.FFreshSeconds := 300;
  Result.FMaximumBytes := NyxMaximumPackedBytes;
end;

function TNyxResourceCachePolicy.Bypass: TNyxResourceCachePolicy;
begin
  Result := Self;
  Result.FMode := rcmBypass;
end;

function TNyxResourceCachePolicy.Memory: TNyxResourceCachePolicy;
begin
  Result := Self;
  Result.FMode := rcmMemory;
end;

function TNyxResourceCachePolicy.Persistent: TNyxResourceCachePolicy;
begin
  Result := Self;
  Result.FMode := rcmPersistent;
end;

function TNyxResourceCachePolicy.FreshFor(ASeconds: Integer): TNyxResourceCachePolicy;
var
  LCandidate: TNyxResourceCachePolicy;
begin
  LCandidate := Self;
  LCandidate.FFreshSeconds := ASeconds;
  LCandidate.Validate;
  Result := LCandidate;
end;

function TNyxResourceCachePolicy.StaleFor(ASeconds: Integer): TNyxResourceCachePolicy;
var
  LCandidate: TNyxResourceCachePolicy;
begin
  LCandidate := Self;
  LCandidate.FStaleSeconds := ASeconds;
  LCandidate.Validate;
  Result := LCandidate;
end;

function TNyxResourceCachePolicy.MaximumBytes(ABytes: Integer): TNyxResourceCachePolicy;
var
  LCandidate: TNyxResourceCachePolicy;
begin
  LCandidate := Self;
  LCandidate.FMaximumBytes := ABytes;
  LCandidate.Validate;
  Result := LCandidate;
end;

function TNyxResourceCachePolicy.ServerPolicy(APolicy: TNyxResourceServerPolicy): TNyxResourceCachePolicy;
var
  LCandidate: TNyxResourceCachePolicy;
begin
  LCandidate := Self;
  LCandidate.FServerPolicy := APolicy;
  LCandidate.Validate;
  Result := LCandidate;
end;

procedure TNyxResourceCachePolicy.Validate;
begin

  if (Ord(FMode) < Ord(Low(TNyxResourceCacheMode))) or
    (Ord(FMode) > Ord(High(TNyxResourceCacheMode))) or
    (Ord(FServerPolicy) < Ord(Low(TNyxResourceServerPolicy))) or
    (Ord(FServerPolicy) > Ord(High(TNyxResourceServerPolicy))) or
    (FFreshSeconds < 0) or (FFreshSeconds > 31536000) or
    (FStaleSeconds < 0) or (FStaleSeconds > 31536000) or
    (FMaximumBytes < 1) or (FMaximumBytes > NyxMaximumPackedBytes) then
  begin
    raise ENyxBytes.Create('Resource cache policy requires valid choices, 0..1 year and 1..1 MiB');
  end;
end;

function TNyxResourceCachePolicy.ToData: TNyxDataValue;
begin
  Validate;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('mode', NyxData(Ord(FMode))), NyxField('fresh', NyxData(FFreshSeconds)),
    NyxField('stale', NyxData(FStaleSeconds)), NyxField('bytes', NyxData(FMaximumBytes)),
    NyxField('server', NyxData(Ord(FServerPolicy)))]);
end;

class function TNyxResourceCachePolicy.FromData(const AData: TNyxDataValue): TNyxResourceCachePolicy;
var
  LCandidate: TNyxResourceCachePolicy;
  LMode: Integer;
  LServer: Integer;
begin

  if (AData.Kind <> ndObject) or (AData.Count <> 6) or
    (AData.Field('version').AsInteger <> 1) then
  begin
    raise ENyxBytes.Create('Unsupported resource cache policy');
  end;
  LCandidate := NyxResourceCache;
  LMode := AData.Field('mode').AsInteger;
  LServer := AData.Field('server').AsInteger;

  if (LMode < Ord(Low(TNyxResourceCacheMode))) or (LMode > Ord(High(TNyxResourceCacheMode))) or
    (LServer < Ord(Low(TNyxResourceServerPolicy))) or (LServer > Ord(High(TNyxResourceServerPolicy))) then
  begin
    raise ENyxBytes.Create('Unknown resource cache policy choice');
  end;
  LCandidate.FMode := TNyxResourceCacheMode(LMode);
  LCandidate.FServerPolicy := TNyxResourceServerPolicy(LServer);
  LCandidate.FFreshSeconds := AData.Field('fresh').AsInteger;
  LCandidate.FStaleSeconds := AData.Field('stale').AsInteger;
  LCandidate.FMaximumBytes := AData.Field('bytes').AsInteger;
  LCandidate.Validate;
  Result := LCandidate;
end;

function NyxEmbeddedResourceSource(const ABytes: TNyxBytes): TNyxResourceSource;
var
  LCandidate: TNyxResourceSource;
begin
  LCandidate := Default(TNyxResourceSource);
  LCandidate.FCache := NyxResourceCache.Bypass;
  LCandidate.FEncoded := NyxEncodeBase64(ABytes);
  Result := LCandidate;
end;

function NyxHostedResourceSource(const AURL: TNyxResourceURL): TNyxResourceSource;
var
  LCandidate: TNyxResourceSource;
begin
  LCandidate := Default(TNyxResourceSource);
  LCandidate.FURL := NyxResourceURL(AURL.Address);
  LCandidate.FKind := rskHosted;
  LCandidate.FCache := NyxResourceCache;
  Result := LCandidate;
end;

function TNyxResourceSource.Cache(const APolicy: TNyxResourceCachePolicy): TNyxResourceSource;
begin
  APolicy.Validate;

  if FKind <> rskHosted then
  begin
    raise ENyxBytes.Create('Embedded resources do not require a hosted cache policy');
  end;
  Result := Self;
  Result.FCache := APolicy;
end;

function TNyxResourceSource.Bytes: TNyxBytes;
begin

  if FKind <> rskEmbedded then
  begin
    raise ENyxBytes.Create('Hosted bytes require the resource resolver');
  end;
  Result := NyxDecodeBase64(FEncoded);
end;

function TNyxResourceSource.ToData: TNyxDataValue;
begin

  if FKind = rskEmbedded then
  begin
    Exit(NyxObject([NyxField('version', NyxData(1)),
      NyxField('embedded', NyxData(FEncoded))]));
  end;
  NyxResourceURL(FURL.Address);
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('url', NyxData(FURL.Address)), NyxField('cache', FCache.ToData)]);
end;

class function TNyxResourceSource.FromData(const AData: TNyxDataValue): TNyxResourceSource;
var
  LCandidate: TNyxResourceSource;
begin

  if (AData.Kind <> ndObject) or (AData.Field('version').AsInteger <> 1) then
  begin
    raise ENyxBytes.Create('Unsupported resource source');
  end;

  if AData.Count = 2 then
  begin
    LCandidate := NyxEmbeddedResourceSource(NyxDecodeBase64(AData.Field('embedded').AsText));
  end
  else if AData.Count = 3 then
  begin
    LCandidate := NyxHostedResourceSource(NyxResourceURL(AData.Field('url').AsText))
      .Cache(TNyxResourceCachePolicy.FromData(AData.Field('cache')));
  end
  else
  begin
    raise ENyxBytes.Create('Resource source requires exact embedded or hosted fields');
  end;
  Result := LCandidate;
end;

end.
