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
    constructor CreateWire(const AData: TNyxDataValue; const ADeclarations: INyxResources);
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

function EncodeNyxResourceRuntime(const ASnapshot: INyxResourceRuntimeSnapshot): TNyxDataValue;
var
  LItems: array of TNyxDataValue;
  LPage: TNyxDataValue;
  LOffset: Integer;
  LIndex: Integer;
begin

  if (ASnapshot = nil) or (ASnapshot.Count < 0) or
    (ASnapshot.Count > NyxMaximumResources) then
  begin
    raise ENyxResource.Create('Reporting requires a bounded resource snapshot');
  end;
  SetLength(LItems, ASnapshot.Count);
  LPage := ASnapshot.Page(0, 1);
  LOffset := 0;
  while LOffset < ASnapshot.Count do
  begin
    LPage := ASnapshot.Page(LOffset, 8);

    if LPage.Field('nextOffset').AsInteger <= LOffset then
    begin
      raise ENyxResource.Create('Reporting snapshot pagination made no progress');
    end;
    for LIndex := 0 to LPage.Field('items').Count - 1 do
    begin
      LItems[LOffset + LIndex] := LPage.Field('items').Item(LIndex);
    end;
    LOffset := LPage.Field('nextOffset').AsInteger;
  end;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('stopped', NyxData(ASnapshot.Stopped)), NyxField('locale', LPage.Field('locale')),
    NyxField('fallback', LPage.Field('fallback')), NyxField('entries', NyxArray(LItems))]);

  if NyxUTF8ByteCount(Result.ToJSON) > NyxMaximumRuntimeReportBytes then
  begin
    raise ENyxResource.Create('Resource runtime report exceeds two MiB');
  end;
end;

function DecodeNyxResourceRuntime(const AData: TNyxDataValue;
  const ADeclarations: INyxResources): INyxResourceRuntimeSnapshot;
begin
  Result := TResourceRuntimeSnapshot.CreateWire(AData, ADeclarations);
end;

constructor TResourceRuntimeSnapshot.CreateWire(const AData: TNyxDataValue;
  const ADeclarations: INyxResources);
var
  LItems: TNyxDataValue;
  LItem: TNyxDataValue;
  LIndex: Integer;
  LPrevious: Integer;
  LDefinition: INyxResourceDefinition;
  LPolicy: TNyxDataValue;
  LValue: TNyxDataValue;
  LPhase: TNyxApplicationResourcePhase;
  LOrigin: TNyxResourceLoadOrigin;
  function LocaleValue(const AName: TNyxText): TNyxLocaleRef;
  begin
    Result := NyxDefaultLocale;

    if AName <> '' then
    begin
      Result := NyxLocale(AName);
    end;
  end;
  function CacheValue(const AValue: TNyxDataValue): TNyxResourceCacheUse;
  var
    LChoice: TNyxResourceCacheUse;
  begin
    for LChoice := Low(TNyxResourceCacheUse) to High(TNyxResourceCacheUse) do
    begin

      if AValue.AsText = NyxResourceCacheUseName(LChoice) then
      begin
        Exit(LChoice);
      end;
    end;
    raise ENyxResource.Create('Unknown runtime cache tier');
  end;
  function OriginValue(const AValue: TNyxDataValue): TNyxResourceLoadOrigin;
  var
    LChoice: TNyxResourceLoadOrigin;
  begin
    for LChoice := Low(TNyxResourceLoadOrigin) to High(TNyxResourceLoadOrigin) do
    begin

      if AValue.AsText = NyxResourceOriginName(LChoice) then
      begin
        Exit(LChoice);
      end;
    end;
    raise ENyxResource.Create('Unknown runtime load origin');
  end;
  function Diagnostic(const AValue: TNyxDataValue): TNyxText;
  begin
    Result := AValue.AsText;

    if NyxTextScalarCount(Result) > 513 then
    begin
      raise ENyxResource.Create('Runtime diagnostic exceeds its Unicode window');
    end;
  end;
begin
  inherited Create;

  if (ADeclarations = nil) or (AData.Kind <> ndObject) or (AData.Count <> 5) or
    (AData.Field('version').AsInteger <> 1) or
    (NyxUTF8ByteCount(AData.ToJSON) > NyxMaximumRuntimeReportBytes) then
  begin
    raise ENyxResource.Create('Runtime report requires its exact bounded version-one envelope');
  end;
  LItems := AData.Field('entries');

  if (LItems.Kind <> ndArray) or (LItems.Count <> ADeclarations.Count) or
    (LItems.Count > NyxMaximumResources) then
  begin
    raise ENyxResource.Create('Runtime report must cover every trusted resource variant exactly once');
  end;
  FDeclarations := ADeclarations.Clone;
  FStopped := AData.Field('stopped').AsBoolean;
  FLocale := LocaleValue(AData.Field('locale').AsText);
  FFallback := LocaleValue(AData.Field('fallback').AsText);
  SetLength(FEntries, LItems.Count);
  for LIndex := 0 to High(FEntries) do
  begin
    LItem := LItems.Item(LIndex);

    if (LItem.Kind <> ndObject) or (LItem.Count <> 15) then
    begin
      raise ENyxResource.Create('Runtime entry requires fifteen exact fields');
    end;
    FEntries[LIndex].Reference := NyxResourceRef(LItem.Field('name').AsText);
    FEntries[LIndex].Locale := LocaleValue(LItem.Field('locale').AsText);
    for LPrevious := 0 to LIndex - 1 do
    begin

      if (FEntries[LPrevious].Reference.Name = FEntries[LIndex].Reference.Name) and
        (FEntries[LPrevious].Locale.Name = FEntries[LIndex].Locale.Name) then
      begin
        raise ENyxResource.Create('Runtime report contains a duplicate variant');
      end;
    end;
    LDefinition := FDeclarations.Definition(FEntries[LIndex].Reference, FEntries[LIndex].Locale);
    FEntries[LIndex].Kind := LDefinition.Kind;
    FEntries[LIndex].Hosted := LDefinition.Source.Kind = rskHosted;
    FEntries[LIndex].Policy := LDefinition.Source.CachePolicy;
    LPolicy := NyxNull;

    if FEntries[LIndex].Hosted then
    begin
      LPolicy := FEntries[LIndex].Policy.ToData;
    end;

    if (LItem.Field('kind').AsText <> NyxResourceKindName(LDefinition.Kind)) or
      (LItem.Field('hosted').AsBoolean <> FEntries[LIndex].Hosted) or
      (LItem.Field('policy').ToJSON <> LPolicy.ToJSON) then
    begin
      raise ENyxResource.Create('Runtime report metadata differs from trusted declarations');
    end;
    LPhase := Low(TNyxApplicationResourcePhase);
    while (LPhase < High(TNyxApplicationResourcePhase)) and
      (NyxResourcePhaseName(LPhase) <> LItem.Field('phase').AsText) do
    begin
      LPhase := TNyxApplicationResourcePhase(Ord(LPhase) + 1);
    end;

    if NyxResourcePhaseName(LPhase) <> LItem.Field('phase').AsText then
    begin
      raise ENyxResource.Create('Unknown runtime resource phase');
    end;
    FEntries[LIndex].Status.Phase := LPhase;
    LValue := LItem.Field('attemptOrigin');
    LOrigin := rloFailed;

    if LValue.Kind <> ndNull then
    begin
      LOrigin := OriginValue(LValue);
    end;

    if (LValue.Kind = ndNull) <> not (LPhase in [nrpWaiting, nrpReady, nrpFailed, nrpRejected]) then
    begin
      raise ENyxResource.Create('Runtime attempt origin does not match its phase');
    end;
    FEntries[LIndex].Status.Origin := LOrigin;
    LValue := LItem.Field('publishedOrigin');
    FEntries[LIndex].HasPublishedLoad := LValue.Kind <> ndNull;

    if FEntries[LIndex].HasPublishedLoad then
    begin
      FEntries[LIndex].PublishedOrigin := OriginValue(LValue);

      if FEntries[LIndex].PublishedOrigin = rloFailed then
      begin
        raise ENyxResource.Create('A failed load cannot be an installed publication');
      end;
    end;
    FEntries[LIndex].Status.CacheRead := CacheValue(LItem.Field('cacheRead'));
    FEntries[LIndex].Status.CacheWrite := CacheValue(LItem.Field('cacheWrite'));
    FEntries[LIndex].PublishedCacheRead := CacheValue(LItem.Field('publishedCacheRead'));
    FEntries[LIndex].PublishedCacheWrite := CacheValue(LItem.Field('publishedCacheWrite'));
    FEntries[LIndex].Status.Error := Diagnostic(LItem.Field('error'));
    FEntries[LIndex].Status.CacheWarning := Diagnostic(LItem.Field('cacheWarning'));
    FEntries[LIndex].Status.NotificationError := Diagnostic(LItem.Field('notificationError'));
  end;
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
