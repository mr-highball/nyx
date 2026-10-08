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

{ Included by nyx.application.resources. Diagnostic snapshots retain immutable
  declarations and copied status values, never the resource owner's lifetime. }

type
  TResourceRuntimeSnapshot = class(TInterfacedObject, INyxResourceRuntimeSnapshot)
  private
    FDeclarations: INyxResources;
    FEntries: array of TNyxResourceRuntimeEntry;
    FStopped: Boolean;
    FLocale: TNyxLocaleRef;
    FFallback: TNyxLocaleRef;
  public
    constructor Create(AOwner: TApplicationResources);
    function GetCount: Integer;
    function GetStopped: Boolean;
    function Entry(AIndex: Integer): TNyxResourceRuntimeEntry;
    function MatchesDeclarations(const AResources: INyxResources): Boolean;
    function Summary: TNyxDataValue;
    function Page(AOffset: Integer; ALimit: Integer = 8): TNyxDataValue;
  end;

function NyxResourcePhaseName(AValue: TNyxApplicationResourcePhase): TNyxText;
const
  CNames: array[TNyxApplicationResourcePhase] of TNyxText =
    ('idle', 'queued', 'loading', 'waiting', 'ready', 'failed', 'rejected', 'cancelled');
begin
  Result := CNames[AValue];
end;

function NyxResourceOriginName(AValue: TNyxResourceLoadOrigin): TNyxText;
const
  CNames: array[TNyxResourceLoadOrigin] of TNyxText =
    ('failed', 'embedded', 'network', 'fresh-cache', 'stale-cache', 'fallback');
begin
  Result := CNames[AValue];
end;

function NyxResourceCacheUseName(AValue: TNyxResourceCacheUse): TNyxText;
const
  CNames: array[TNyxResourceCacheUse] of TNyxText = ('none', 'memory', 'persistent');
begin
  Result := CNames[AValue];
end;

function RuntimeDiagnostic(const AText: TNyxText): TNyxText;
var
  LIndex: Integer;
  LScalar: Integer;
  LCount: Integer;
begin
  LIndex := 1;
  LCount := 0;
  while (LCount < 512) and NyxNextScalar(AText, LIndex, LScalar) do
  begin
    Inc(LCount);
  end;
  Result := Copy(AText, 1, LIndex - 1);

  if LIndex <= Length(AText) then
  begin
    Result := Result + TNyxText('…');
  end;
end;

function NyxApplicationResourceDiagnostics(
  const AResources: INyxApplicationResources): INyxApplicationResourceDiagnostics;
begin

  if (AResources = nil) or not Supports(AResources, INyxApplicationResourceDiagnostics, Result) then
  begin
    raise ENyxResource.Create('Application resource diagnostics are not supported');
  end;
end;

constructor TResourceRuntimeSnapshot.Create(AOwner: TApplicationResources);
var
  LIndex: Integer;
  LSlot: TResourceSlot;
  LDefinition: INyxResourceDefinition;
begin
  inherited Create;
  FDeclarations := AOwner.FDeclarations;
  FStopped := AOwner.FStopped;
  FLocale := AOwner.FContext.Locale;
  FFallback := AOwner.FContext.Fallback;
  SetLength(FEntries, Length(AOwner.FSlots));
  for LIndex := 0 to High(FEntries) do
  begin
    LSlot := AOwner.FSlots[LIndex];
    LDefinition := FDeclarations.Definition(LSlot.Reference, LSlot.Locale);
    FEntries[LIndex].Reference := LSlot.Reference;
    FEntries[LIndex].Locale := LSlot.Locale;
    FEntries[LIndex].Kind := LDefinition.Kind;
    FEntries[LIndex].Hosted := LDefinition.Source.Kind = rskHosted;
    FEntries[LIndex].Policy := LDefinition.Source.CachePolicy;
    FEntries[LIndex].Status := LSlot.Status;
    FEntries[LIndex].HasPublishedLoad := LSlot.HasPublishedLoad;
    FEntries[LIndex].PublishedOrigin := LSlot.PublishedOrigin;
    FEntries[LIndex].PublishedCacheRead := LSlot.PublishedCacheRead;
    FEntries[LIndex].PublishedCacheWrite := LSlot.PublishedCacheWrite;
  end;
end;

function TApplicationResources.CaptureRuntime: INyxResourceRuntimeSnapshot;
begin
  FScheduler.RequireUI;
  Result := TResourceRuntimeSnapshot.Create(Self);
end;

function TResourceRuntimeSnapshot.GetCount: Integer;
begin
  Result := Length(FEntries);
end;

function TResourceRuntimeSnapshot.GetStopped: Boolean;
begin
  Result := FStopped;
end;

function TResourceRuntimeSnapshot.Entry(AIndex: Integer): TNyxResourceRuntimeEntry;
begin

  if (AIndex < 0) or (AIndex >= Length(FEntries)) then
  begin
    raise ENyxResource.Create('Runtime resource entry is outside the snapshot');
  end;
  Result := FEntries[AIndex];
end;

function TResourceRuntimeSnapshot.MatchesDeclarations(const AResources: INyxResources): Boolean;
var
  LIndex: Integer;
  LReference: TNyxResourceRef;
  LLocale: TNyxLocaleRef;
begin
  Result := False;

  if (AResources = nil) or (AResources.Count <> FDeclarations.Count) then
  begin
    Exit;
  end;
  for LIndex := 0 to FDeclarations.Count - 1 do
  begin
    LReference := FDeclarations.Reference(LIndex);
    LLocale := FDeclarations.Locale(LIndex);

    if not AResources.Contains(LReference, LLocale) or
      (AResources.Definition(LReference, LLocale).ToData.ToJSON <>
        FDeclarations.Definition(LReference, LLocale).ToData.ToJSON) then
    begin
      Exit;
    end;
  end;
  Result := True;
end;

function TResourceRuntimeSnapshot.Summary: TNyxDataValue;
var
  LPhases: array[TNyxApplicationResourcePhase] of Integer;
  LReads: array[TNyxResourceCacheUse] of Integer;
  LWrites: array[TNyxResourceCacheUse] of Integer;
  LPhase: TNyxApplicationResourcePhase;
  LUse: TNyxResourceCacheUse;
  LIndex: Integer;
  LPhasesData: array of TNyxDataField;
  LReadsData: array of TNyxDataField;
  LWritesData: array of TNyxDataField;
  LPublished: Integer;
begin
  for LPhase := Low(TNyxApplicationResourcePhase) to High(TNyxApplicationResourcePhase) do
  begin
    LPhases[LPhase] := 0;
  end;
  for LUse := Low(TNyxResourceCacheUse) to High(TNyxResourceCacheUse) do
  begin
    LReads[LUse] := 0;
    LWrites[LUse] := 0;
  end;
  LPublished := 0;
  for LIndex := 0 to High(FEntries) do
  begin
    Inc(LPhases[FEntries[LIndex].Status.Phase]);
    Inc(LReads[FEntries[LIndex].Status.CacheRead]);
    Inc(LWrites[FEntries[LIndex].Status.CacheWrite]);

    if FEntries[LIndex].HasPublishedLoad then
    begin
      Inc(LPublished);
    end;
  end;
  SetLength(LPhasesData, Ord(High(TNyxApplicationResourcePhase)) + 1);
  for LPhase := Low(TNyxApplicationResourcePhase) to High(TNyxApplicationResourcePhase) do
  begin
    LPhasesData[Ord(LPhase)] := NyxField(NyxResourcePhaseName(LPhase), NyxData(LPhases[LPhase]));
  end;
  SetLength(LReadsData, Ord(High(TNyxResourceCacheUse)) + 1);
  SetLength(LWritesData, Length(LReadsData));
  for LUse := Low(TNyxResourceCacheUse) to High(TNyxResourceCacheUse) do
  begin
    LReadsData[Ord(LUse)] := NyxField(NyxResourceCacheUseName(LUse), NyxData(LReads[LUse]));
    LWritesData[Ord(LUse)] := NyxField(NyxResourceCacheUseName(LUse), NyxData(LWrites[LUse]));
  end;
  Result := NyxObject([NyxField('stopped', NyxData(FStopped)),
    NyxField('total', NyxData(GetCount)), NyxField('publishedLoads', NyxData(LPublished)),
    NyxField('phases', NyxObject(LPhasesData)), NyxField('cacheReads', NyxObject(LReadsData)),
    NyxField('cacheWrites', NyxObject(LWritesData))]);
end;

function TResourceRuntimeSnapshot.Page(AOffset: Integer; ALimit: Integer): TNyxDataValue;
var
  LIndex: Integer;
  LCount: Integer;
  LBytes: TNyxTextBytes;
  LEntry: TNyxResourceRuntimeEntry;
  LPolicy: TNyxDataValue;
  LPublished: TNyxDataValue;
  LOrigin: TNyxDataValue;
  LItems: array of TNyxDataValue;
const
  CKinds: array[TNyxResourceKind] of TNyxText = ('image', 'json', 'text', 'binary');
begin

  if (AOffset < 0) or (AOffset > GetCount) or (ALimit < 1) or (ALimit > 16) then
  begin
    raise ENyxResource.Create('Runtime resource page requires an existing offset and 1..16 entries');
  end;
  LCount := GetCount - AOffset;

  if LCount > ALimit then
  begin
    LCount := ALimit;
  end;
  SetLength(LItems, LCount);
  LBytes := 0;
  for LIndex := 0 to LCount - 1 do
  begin
    LEntry := FEntries[AOffset + LIndex];
    LPolicy := NyxNull;
    LPublished := NyxNull;
    LOrigin := NyxNull;

    if LEntry.Hosted then
    begin
      LPolicy := LEntry.Policy.ToData;
    end;

    if LEntry.HasPublishedLoad then
    begin
      LPublished := NyxData(NyxResourceOriginName(LEntry.PublishedOrigin));
    end;

    if LEntry.Status.Phase in [nrpWaiting, nrpReady, nrpFailed, nrpRejected] then
    begin
      LOrigin := NyxData(NyxResourceOriginName(LEntry.Status.Origin));
    end;
    LItems[LIndex] := NyxObject([NyxField('name', NyxData(LEntry.Reference.Name)),
      NyxField('locale', NyxData(LEntry.Locale.Name)), NyxField('kind', NyxData(CKinds[LEntry.Kind])),
      NyxField('hosted', NyxData(LEntry.Hosted)), NyxField('policy', LPolicy),
      NyxField('phase', NyxData(NyxResourcePhaseName(LEntry.Status.Phase))),
      NyxField('attemptOrigin', LOrigin), NyxField('publishedOrigin', LPublished),
      NyxField('cacheRead', NyxData(NyxResourceCacheUseName(LEntry.Status.CacheRead))),
      NyxField('cacheWrite', NyxData(NyxResourceCacheUseName(LEntry.Status.CacheWrite))),
      NyxField('publishedCacheRead', NyxData(NyxResourceCacheUseName(LEntry.PublishedCacheRead))),
      NyxField('publishedCacheWrite', NyxData(NyxResourceCacheUseName(LEntry.PublishedCacheWrite))),
      NyxField('error', NyxData(RuntimeDiagnostic(LEntry.Status.Error))),
      NyxField('cacheWarning', NyxData(RuntimeDiagnostic(LEntry.Status.CacheWarning))),
      NyxField('notificationError', NyxData(RuntimeDiagnostic(LEntry.Status.NotificationError)))]);
    Inc(LBytes, NyxUTF8ByteCount(LItems[LIndex].ToJSON));

    if LBytes > 40 * 1024 then
    begin
      { JSON escaping and supplementary scalars affect byte size. A page may
        return fewer than its requested upper limit; nextOffset remains exact. }
      LCount := LIndex;
      SetLength(LItems, LCount);
      Break;
    end;
  end;
  Result := NyxObject([NyxField('origin', NyxData('application-runtime')),
    NyxField('stopped', NyxData(FStopped)), NyxField('locale', NyxData(FLocale.Name)),
    NyxField('fallback', NyxData(FFallback.Name)), NyxField('total', NyxData(GetCount)),
    NyxField('offset', NyxData(AOffset)), NyxField('nextOffset', NyxData(AOffset + LCount)),
    NyxField('items', NyxArray(LItems))]);
end;
