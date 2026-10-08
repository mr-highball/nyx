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
unit nyx.resource.cache;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, Math, nyx.text, nyx.data, nyx.resources, nyx.resource.sources;

type
  { Cache freshness is independent of the stored file kind. Stale is eligible
    only for the caller's explicit failure fallback, never a normal fresh hit. }
  TNyxResourceCacheState = (rcsMiss, rcsFresh, rcsStale, rcsExpired);

  { Owned HTTP metadata at an adapter boundary. Public defaults allow a private
    application TTL where the server supplied no freshness restriction. Parsed
    no-store/no-cache/must-revalidate and max-age/Age qualify Respect; Override
    deliberately substitutes the caller's resource policy. Unknown directives
    are retained by neither core nor design. ETag/Last-Modified belong to the
    eventual transport revalidation request, not a caption/property string. }
  TNyxResourceCacheHints = record
  private
    FNoStore: Boolean;
    FValidate: Boolean;
    FMustRevalidate: Boolean;
    FMaximumAge: Integer;
    FAge: Integer;
  public
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxResourceCacheHints; static;
  end;

  { Immutable persisted cache envelope. ReceivedAt uses finite UTC seconds,
    supplied by the adapter's clock so restart/fixtures do not depend on a
    process tick counter. A backwards clock refuses reuse. Content is an
    admitted embedded definition; origin URL and cache policy are retained
    separately from saved resources. Reads copy values without store references. }
  TNyxResourceCacheEntry = record
  private
    FURL: TNyxResourceURL;
    FDefinition: TNyxDataValue;
    FReceivedAt: Double;
    FHints: TNyxResourceCacheHints;
    FKind: TNyxResourceKind;
    FByteCount: Integer;
  public
    function Definition: INyxResourceDefinition;
    function StateAt(const APolicy: TNyxResourceCachePolicy;
      ATime: Double): TNyxResourceCacheState;
    function CanStore(const APolicy: TNyxResourceCachePolicy): Boolean;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxResourceCacheEntry; static;
    property URL: TNyxResourceURL read FURL;
    property Kind: TNyxResourceKind read FKind;
    property ByteCount: Integer read FByteCount;
  end;

  TNyxResourceCacheRead = procedure(AFound: Boolean;
    const AEntry: TNyxResourceCacheEntry; const AError: TNyxText) of object;
  TNyxResourceCacheWrite = procedure(ASuccess: Boolean; const AError: TNyxText) of object;

  { Disconnect a borrowed callback before destroying its receiver. Completion
    retires it once. Providers may complete synchronously or asynchronously;
    retaining this token never retains a project/tree/control. }
  INyxResourceCacheJob = interface
    ['{E7D5F634-4BCD-4101-811F-B3103362EEA2}']
    procedure Cancel;
  end;

  { Storage and network policy remain separate. Reads return an owned envelope;
    the resolver evaluates StateAt using the requesting resource's policy.
    Corrupt entries report a miss/error and never become accepted file data.
    Persistent browser storage can be unavailable/evicted; callers use the same
    abstraction for explicit memory fallback. No provider silently fetches URLs. }
  INyxResourceCacheStorage = interface
    ['{40A5F59A-2D2A-4AE5-B1A9-65733A0BAC80}']
    function Read(const AURL: TNyxResourceURL; AKind: TNyxResourceKind;
      AReply: TNyxResourceCacheRead): INyxResourceCacheJob;
    function Write(const AEntry: TNyxResourceCacheEntry;
      AReply: TNyxResourceCacheWrite): INyxResourceCacheJob;
  end;

  { Adapter operation base. Pending browser subclasses keep their own lease
    until their promise finishes; Cancel removes borrowed callbacks immediately.
    Receiver exceptions propagate after callback retirement and are never
    transformed into a second storage completion. }
  TNyxResourceCacheJob = class(TInterfacedObject, INyxResourceCacheJob)
  private
    FRead: TNyxResourceCacheRead;
    FWrite: TNyxResourceCacheWrite;
  protected
    function Active: Boolean;
    procedure CompleteRead(AFound: Boolean; const AEntry: TNyxResourceCacheEntry;
      const AError: TNyxText);
    procedure CompleteWrite(ASuccess: Boolean; const AError: TNyxText);
  public
    constructor Create(AReply: TNyxResourceCacheRead); overload;
    constructor Create(AReply: TNyxResourceCacheWrite); overload;
    destructor Destroy; override;
    procedure Cancel;
  end;

function NyxResourceCacheHeaders(const ACacheControl, AAge: TNyxText): TNyxResourceCacheHints;
function NyxResourceCacheEntry(const AURL: TNyxResourceURL;
  const ADefinition: INyxResourceDefinition; AReceivedAt: Double;
  const AHints: TNyxResourceCacheHints): TNyxResourceCacheEntry;
{ One caller/UI thread owns the mutable provider. Operations complete inline;
  it has no worker or internal lock. A threaded host supplies a synchronized
  storage implementation through INyxResourceCacheStorage. }
function NewNyxMemoryResourceCache(AEntryLimit: Integer = 128;
  AByteLimit: Integer = 16777216): INyxResourceCacheStorage;

implementation

uses nyx.state;

type
  TMemoryCache = class(TInterfacedObject, INyxResourceCacheStorage)
  private
    FEntries: array of TNyxResourceCacheEntry;
    FEntryLimit: Integer;
    FByteLimit: Integer;
  public
    constructor Create(AEntryLimit, AByteLimit: Integer);
    function Read(const AURL: TNyxResourceURL; AKind: TNyxResourceKind;
      AReply: TNyxResourceCacheRead): INyxResourceCacheJob;
    function Write(const AEntry: TNyxResourceCacheEntry;
      AReply: TNyxResourceCacheWrite): INyxResourceCacheJob;
  end;

function NyxResourceCacheHeaders(const ACacheControl, AAge: TNyxText): TNyxResourceCacheHints;
var
  LIndex: Integer;
  LHints: TNyxResourceCacheHints;
  LStart: Integer;
  LPart: TNyxText;
  LValue: TNyxText;
  LInteger: Integer;
begin
  LHints := Default(TNyxResourceCacheHints);
  LHints.FMaximumAge := -1;

  if (Length(ACacheControl) > 8192) or (Length(AAge) > 32) then
  begin
    raise ENyxResource.Create('Resource cache headers exceed metadata budget');
  end;
  LStart := 1;
  while LStart <= Length(ACacheControl) do
  begin
    LIndex := LStart;
    while (LIndex <= Length(ACacheControl)) and (ACacheControl[LIndex] <> ',') do
    begin
      Inc(LIndex);
    end;
    LPart := LowerCase(Trim(Copy(ACacheControl, LStart, LIndex - LStart)));
    LStart := LIndex + 1;

    if LPart = 'no-store' then
    begin
      LHints.FNoStore := True;
    end
    else if (LPart = 'no-cache') or (Copy(LPart, 1, 9) = 'no-cache=') then
    begin
      LHints.FValidate := True;
    end
    else if LPart = 'must-revalidate' then
    begin
      LHints.FMustRevalidate := True;
    end
    else if Copy(LPart, 1, 8) = 'max-age=' then
    begin
      LValue := Trim(Copy(LPart, 9, MaxInt));

      if (Length(LValue) > 1) and (LValue[1] = '"') and (LValue[Length(LValue)] = '"') then
      begin
        LValue := Copy(LValue, 2, Length(LValue) - 2);
      end;

      if not TryNyxStateInteger(LValue, LInteger) or (LInteger < 0) then
      begin
        LHints.FValidate := True;
      end
      else if LHints.FMaximumAge < 0 then
      begin
        LHints.FMaximumAge := Min(LInteger, 31536000);
      end
      else
      begin
        { Conflicting repeated freshness is conservative validation, never an
          accidental extension of the server's admitted lifetime. }
        LHints.FValidate := True;
      end;
    end;
  end;

  if AAge <> '' then
  begin

    if not TryNyxStateInteger(Trim(AAge), LInteger) or (LInteger < 0) then
    begin
      LHints.FValidate := True;
    end
    else
    begin
      LHints.FAge := Min(LInteger, 31536000);
    end;
  end;
  Result := LHints;
end;

function TNyxResourceCacheHints.ToData: TNyxDataValue;
begin
  Result := NyxObject([NyxField('store', NyxData(not FNoStore)),
    NyxField('validate', NyxData(FValidate)), NyxField('revalidate', NyxData(FMustRevalidate)),
    NyxField('maxAge', NyxData(FMaximumAge)), NyxField('age', NyxData(FAge))]);
end;

class function TNyxResourceCacheHints.FromData(const AData: TNyxDataValue): TNyxResourceCacheHints;
var
  LHints: TNyxResourceCacheHints;
begin

  if (AData.Kind <> ndObject) or (AData.Count <> 5) then
  begin
    raise ENyxResource.Create('Cache hints require five exact fields');
  end;
  LHints := Default(TNyxResourceCacheHints);
  LHints.FNoStore := not AData.Field('store').AsBoolean;
  LHints.FValidate := AData.Field('validate').AsBoolean;
  LHints.FMustRevalidate := AData.Field('revalidate').AsBoolean;
  LHints.FMaximumAge := AData.Field('maxAge').AsInteger;
  LHints.FAge := AData.Field('age').AsInteger;

  if (LHints.FMaximumAge < -1) or (LHints.FMaximumAge > 31536000) or
    (LHints.FAge < 0) or (LHints.FAge > 31536000) then
  begin
    raise ENyxResource.Create('Cache hints require bounded freshness and age');
  end;
  Result := LHints;
end;

function NyxResourceCacheEntry(const AURL: TNyxResourceURL;
  const ADefinition: INyxResourceDefinition; AReceivedAt: Double;
  const AHints: TNyxResourceCacheHints): TNyxResourceCacheEntry;
var
  LEntry: TNyxResourceCacheEntry;
  LDefinition: INyxResourceDefinition;
begin

  if (ADefinition = nil) or IsNan(AReceivedAt) or IsInfinite(AReceivedAt) or
    (AReceivedAt < 0) or (AReceivedAt > 9007199254740991.0) then
  begin
    raise ENyxResource.Create('Cache entry requires a file and finite UTC seconds');
  end;
  LDefinition := NyxResourceFromData(ADefinition.ToData);

  if LDefinition.Source.Kind <> rskEmbedded then
  begin
    raise ENyxResource.Create('Cache entry requires resolved embedded bytes');
  end;
  LEntry.FURL := NyxResourceURL(AURL.Address);
  LEntry.FDefinition := LDefinition.ToData;
  LEntry.FKind := LDefinition.Kind;
  LEntry.FByteCount := LDefinition.ByteCount;
  LEntry.FReceivedAt := AReceivedAt;
  LEntry.FHints := TNyxResourceCacheHints.FromData(AHints.ToData);
  Result := LEntry;
end;

function TNyxResourceCacheEntry.Definition: INyxResourceDefinition;
begin
  Result := NyxResourceFromData(FDefinition);
end;

function TNyxResourceCacheEntry.CanStore(const APolicy: TNyxResourceCachePolicy): Boolean;
begin
  APolicy.Validate;
  Result := (APolicy.Mode <> rcmBypass) and (FByteCount <= APolicy.ByteLimit) and
    ((APolicy.Server = rcspOverride) or not FHints.FNoStore);
end;

function TNyxResourceCacheEntry.StateAt(const APolicy: TNyxResourceCachePolicy;
  ATime: Double): TNyxResourceCacheState;
var
  LFresh: Integer;
  LStale: Integer;
  LAge: Double;
begin

  if IsNan(ATime) or IsInfinite(ATime) or (ATime < FReceivedAt) then
  begin
    Exit(rcsMiss);
  end;

  if not CanStore(APolicy) then
  begin
    Exit(rcsMiss);
  end;
  LFresh := APolicy.FreshSeconds;
  LStale := APolicy.StaleSeconds;

  if APolicy.Server = rcspRespect then
  begin

    if FHints.FValidate then
    begin
      Exit(rcsExpired);
    end;

    if FHints.FMaximumAge >= 0 then
    begin
      LFresh := Min(LFresh, Max(0, FHints.FMaximumAge - FHints.FAge));
    end;

    if FHints.FMustRevalidate then
    begin
      LStale := 0;
    end;
  end;
  LAge := ATime - FReceivedAt;

  if LAge < LFresh then
  begin
    Exit(rcsFresh);
  end;

  if (LStale > 0) and (LAge < LFresh + LStale) then
  begin
    Exit(rcsStale);
  end;
  Result := rcsExpired;
end;

function TNyxResourceCacheEntry.ToData: TNyxDataValue;
begin
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('url', NyxData(FURL.Address)), NyxField('definition', FDefinition),
    NyxField('receivedAt', NyxData(FReceivedAt)), NyxField('hints', FHints.ToData)]);
end;

class function TNyxResourceCacheEntry.FromData(const AData: TNyxDataValue): TNyxResourceCacheEntry;
begin

  if (AData.Kind <> ndObject) or (AData.Count <> 5) or
    (AData.Field('version').AsInteger <> 1) then
  begin
    raise ENyxResource.Create('Unsupported resource cache envelope');
  end;
  Result := NyxResourceCacheEntry(NyxResourceURL(AData.Field('url').AsText),
    NyxResourceFromData(AData.Field('definition')), AData.Field('receivedAt').AsNumber,
    TNyxResourceCacheHints.FromData(AData.Field('hints')));
end;

constructor TNyxResourceCacheJob.Create(AReply: TNyxResourceCacheRead);
begin
  inherited Create;
  FRead := AReply;
end;

constructor TNyxResourceCacheJob.Create(AReply: TNyxResourceCacheWrite);
begin
  inherited Create;
  FWrite := AReply;
end;

destructor TNyxResourceCacheJob.Destroy;
begin
  Cancel;
  inherited Destroy;
end;

procedure TNyxResourceCacheJob.Cancel;
begin
  FRead := nil;
  FWrite := nil;
end;

function TNyxResourceCacheJob.Active: Boolean;
begin
  Result := Assigned(FRead) or Assigned(FWrite);
end;

procedure TNyxResourceCacheJob.CompleteRead(AFound: Boolean;
  const AEntry: TNyxResourceCacheEntry; const AError: TNyxText);
var
  LReply: TNyxResourceCacheRead;
  LLease: INyxResourceCacheJob;
begin
  LLease := Self;
  LReply := FRead;
  Cancel;

  if Assigned(LReply) then
  begin
    LReply(AFound, AEntry, AError);
  end;
  LLease := nil;
end;

procedure TNyxResourceCacheJob.CompleteWrite(ASuccess: Boolean; const AError: TNyxText);
var
  LReply: TNyxResourceCacheWrite;
  LLease: INyxResourceCacheJob;
begin
  LLease := Self;
  LReply := FWrite;
  Cancel;

  if Assigned(LReply) then
  begin
    LReply(ASuccess, AError);
  end;
  LLease := nil;
end;

constructor TMemoryCache.Create(AEntryLimit, AByteLimit: Integer);
begin
  inherited Create;

  if (AEntryLimit < 1) or (AEntryLimit > 128) or
    (AByteLimit < 1) or (AByteLimit > 16777216) then
  begin
    raise ENyxResource.Create('Cache storage requires 1..128 entries and 1..16 MiB');
  end;
  FEntryLimit := AEntryLimit;
  FByteLimit := AByteLimit;
end;

function TMemoryCache.Read(const AURL: TNyxResourceURL; AKind: TNyxResourceKind;
  AReply: TNyxResourceCacheRead): INyxResourceCacheJob;
var
  LJob: TNyxResourceCacheJob;
  LIndex: Integer;
  LEntry: TNyxResourceCacheEntry;
begin
  NyxResourceURL(AURL.Address);
  NyxResourceKindName(AKind);
  LJob := TNyxResourceCacheJob.Create(AReply);
  Result := LJob;
  for LIndex := 0 to High(FEntries) do
  begin

    if (FEntries[LIndex].URL.Address = AURL.Address) and
      (FEntries[LIndex].Kind = AKind) then
    begin
      LJob.CompleteRead(True, FEntries[LIndex], '');
      Exit;
    end;
  end;
  LEntry := Default(TNyxResourceCacheEntry);
  LJob.CompleteRead(False, LEntry, '');
end;

function TMemoryCache.Write(const AEntry: TNyxResourceCacheEntry;
  AReply: TNyxResourceCacheWrite): INyxResourceCacheJob;
var
  LJob: TNyxResourceCacheJob;
  LEntry: TNyxResourceCacheEntry;
  LIndex: Integer;
  LTarget: Integer;
  LBytes: Integer;
  LError: TNyxText;
begin
  LJob := TNyxResourceCacheJob.Create(AReply);
  Result := LJob;
  LTarget := -1;
  LError := '';
  try
    LEntry := TNyxResourceCacheEntry.FromData(AEntry.ToData);
    LBytes := LEntry.ByteCount;
    for LIndex := 0 to High(FEntries) do
    begin

      if (FEntries[LIndex].URL.Address = LEntry.URL.Address) and
        (FEntries[LIndex].Kind = LEntry.Kind) then
      begin
        LTarget := LIndex;
      end
      else
      begin
        Inc(LBytes, FEntries[LIndex].ByteCount);
      end;
    end;

    if (LBytes > FByteLimit) or ((LTarget < 0) and (Length(FEntries) >= FEntryLimit)) then
    begin
      raise ENyxResource.Create('Resource cache storage budget is full');
    end;

    if LTarget < 0 then
    begin
      LTarget := Length(FEntries);
      SetLength(FEntries, LTarget + 1);
    end;
    FEntries[LTarget] := LEntry;
  except
    on LException: Exception do
    begin
      LError := LException.Message;
    end;
  end;
  LJob.CompleteWrite(LError = '', LError);
end;

function NewNyxMemoryResourceCache(AEntryLimit: Integer;
  AByteLimit: Integer): INyxResourceCacheStorage;
begin
  Result := TMemoryCache.Create(AEntryLimit, AByteLimit);
end;

end.
