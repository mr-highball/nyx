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

unit nyx.resources.runtime.view;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.bytes, nyx.editing, nyx.data, nyx.resources, nyx.application.resources,
  nyx.resources.loader, nyx.resource.sources, nyx.controls;

type
  { Presentation availability is distinct from the application's load phase.
    A newly selected variant awaits its exact reply; absent membership is an
    explicit result. Neither state invents a load/publication observation. }
  TNyxResourceDetailAvailability = (rdaAwaiting, rdaMissing, rdaAvailable);
  { Copied, bounded runtime summary for remote observers and ordinary Nyx views.
    FromData is the explicit wire boundary. All counters are nonnegative and
    partition the total; public callers read strongly typed phases/cache tiers. }
  TNyxResourceRuntimeSummary = record
  private
    FTotal: Integer;
    FPublished: Integer;
    FStopped: Boolean;
    FPhases: array[TNyxApplicationResourcePhase] of Integer;
    FReads: array[TNyxResourceCacheUse] of Integer;
    FWrites: array[TNyxResourceCacheUse] of Integer;
  public
    class function FromData(const AData: TNyxDataValue): TNyxResourceRuntimeSummary; static;
    function PhaseCount(AValue: TNyxApplicationResourcePhase): Integer;
    function CacheReads(AValue: TNyxResourceCacheUse): Integer;
    function CacheWrites(AValue: TNyxResourceCacheUse): Integer;
    property Total: Integer read FTotal;
    property PublishedLoads: Integer read FPublished;
    property Stopped: Boolean read FStopped;
  end;

  { A single status-only wire item, copied into the ordinary portable entry type.
    FromData validates closed choices, diagnostic budgets and nullable origins;
    trusted declaration/run admission remains owned by the reporting broker.
    No payload, URL, callback or application handle is owned by this value. }
  TNyxResourceRuntimeDetail = record
  private
    FEntry: TNyxResourceRuntimeEntry;
  public
    class function FromData(const AData: TNyxDataValue): TNyxResourceRuntimeDetail; static;
    property Entry: TNyxResourceRuntimeEntry read FEntry;
  end;

{ An ordinary public compound card, shared by both Studio targets and embeddable
  in applications. It owns its labels/badges and no live application or callback.
  Status is a copied observation, so painting cannot start network/cache work. }
function NewNyxResourceRuntimeView(const AID: TNyxText;
  const ASummary: TNyxResourceRuntimeSummary): INyxCard;
{ Independent ordinary Nyx card for one observed variant. Attempt and installed
  publication stay distinct: a failed reload may leave earlier content displayed.
  The card owns only its controls/copied text; it cannot reload or cancel a host. }
function NewNyxResourceRuntimeDetailView(const AID: TNyxText;
  const AEntry: TNyxResourceRuntimeEntry;
  AAvailability: TNyxResourceDetailAvailability = rdaAvailable): INyxCard;
{ Exact-variant read. Missing membership returns a null entry, never a fallback
  locale or another resource. The returned three-field selector/result owns its
  bounded diagnostic item and retains no snapshot, document or live owner. }
function NyxSelectedResourceRuntime(const ASnapshot: INyxResourceRuntimeSnapshot;
  const AReference: TNyxResourceRef; const ALocale: TNyxLocaleRef): TNyxDataValue;

implementation

function NyxSelectedResourceRuntime(const ASnapshot: INyxResourceRuntimeSnapshot;
  const AReference: TNyxResourceRef; const ALocale: TNyxLocaleRef): TNyxDataValue;
var
  LIndex: Integer;
  LEntry: TNyxResourceRuntimeEntry;
  LItem: TNyxDataValue;
begin

  if (ASnapshot = nil) or (ASnapshot.Count < 0) or
    (ASnapshot.Count > NyxMaximumResources) then
  begin
    raise ENyxResource.Create('Selected observation requires a bounded runtime snapshot');
  end;
  NyxResourceRef(AReference.Name);
  LItem := NyxNull;
  for LIndex := 0 to ASnapshot.Count - 1 do
  begin
    LEntry := ASnapshot.Entry(LIndex);

    if (LEntry.Reference.Name = AReference.Name) and (LEntry.Locale.Name = ALocale.Name) then
    begin
      LItem := ASnapshot.Page(LIndex, 1).Field('items').Item(0).Copy;
      Break;
    end;
  end;
  Result := NyxObject([NyxField('reference', NyxData(AReference.Name)),
    NyxField('locale', NyxData(ALocale.Name)), NyxField('entry', LItem)]);
end;

class function TNyxResourceRuntimeDetail.FromData(
  const AData: TNyxDataValue): TNyxResourceRuntimeDetail;
var
  LKind: TNyxResourceKind;
  LPhase: TNyxApplicationResourcePhase;
  LValue: TNyxDataValue;
  function Origin(const AValue: TNyxDataValue): TNyxResourceLoadOrigin;
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
    raise ENyxResource.Create('Unknown observed resource origin');
  end;
  function CacheUse(const AValue: TNyxDataValue): TNyxResourceCacheUse;
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
    raise ENyxResource.Create('Unknown observed resource cache tier');
  end;
  function Diagnostic(const AValue: TNyxDataValue): TNyxText;
  begin
    Result := AValue.AsText;

    if NyxTextScalarCount(Result) > 513 then
    begin
      raise ENyxResource.Create('Observed resource diagnostic exceeds its Unicode window');
    end;
  end;
begin
  Result := Default(TNyxResourceRuntimeDetail);

  if (AData.Kind <> ndObject) or (AData.Count <> 15) or
    (NyxUTF8ByteCount(AData.ToJSON) > 40960) then
  begin
    raise ENyxResource.Create('Observed resource detail requires one bounded fifteen-field item');
  end;
  Result.FEntry.Reference := NyxResourceRef(AData.Field('name').AsText);
  Result.FEntry.Locale := NyxDefaultLocale;

  if AData.Field('locale').AsText <> '' then
  begin
    Result.FEntry.Locale := NyxLocale(AData.Field('locale').AsText);
  end;
  LKind := Low(TNyxResourceKind);
  while (LKind < High(TNyxResourceKind)) and
    (NyxResourceKindName(LKind) <> AData.Field('kind').AsText) do
  begin
    LKind := Succ(LKind);
  end;

  if NyxResourceKindName(LKind) <> AData.Field('kind').AsText then
  begin
    raise ENyxResource.Create('Unknown observed resource kind');
  end;
  Result.FEntry.Kind := LKind;
  Result.FEntry.Hosted := AData.Field('hosted').AsBoolean;

  if Result.FEntry.Hosted then
  begin
    Result.FEntry.Policy := TNyxResourceCachePolicy.FromData(AData.Field('policy'));
  end
  else
  begin

    if AData.Field('policy').Kind <> ndNull then
    begin
      raise ENyxResource.Create('Embedded resource observation cannot carry hosted cache policy');
    end;
  end;
  LPhase := Low(TNyxApplicationResourcePhase);
  while (LPhase < High(TNyxApplicationResourcePhase)) and
    (NyxResourcePhaseName(LPhase) <> AData.Field('phase').AsText) do
  begin
    LPhase := Succ(LPhase);
  end;

  if NyxResourcePhaseName(LPhase) <> AData.Field('phase').AsText then
  begin
    raise ENyxResource.Create('Unknown observed resource phase');
  end;
  Result.FEntry.Status.Phase := LPhase;
  LValue := AData.Field('attemptOrigin');
  Result.FEntry.Status.Origin := rloFailed;

  if (LValue.Kind = ndNull) <> not (LPhase in [nrpWaiting, nrpReady, nrpFailed, nrpRejected]) then
  begin
    raise ENyxResource.Create('Observed attempt origin differs from its phase');
  end;

  if LValue.Kind <> ndNull then
  begin
    Result.FEntry.Status.Origin := Origin(LValue);
  end;
  LValue := AData.Field('publishedOrigin');
  Result.FEntry.HasPublishedLoad := LValue.Kind <> ndNull;

  if Result.FEntry.HasPublishedLoad then
  begin
    Result.FEntry.PublishedOrigin := Origin(LValue);

    if Result.FEntry.PublishedOrigin = rloFailed then
    begin
      raise ENyxResource.Create('Failed content cannot be an observed publication');
    end;
  end;
  Result.FEntry.Status.CacheRead := CacheUse(AData.Field('cacheRead'));
  Result.FEntry.Status.CacheWrite := CacheUse(AData.Field('cacheWrite'));
  Result.FEntry.PublishedCacheRead := CacheUse(AData.Field('publishedCacheRead'));
  Result.FEntry.PublishedCacheWrite := CacheUse(AData.Field('publishedCacheWrite'));
  Result.FEntry.Status.Error := Diagnostic(AData.Field('error'));
  Result.FEntry.Status.CacheWarning := Diagnostic(AData.Field('cacheWarning'));
  Result.FEntry.Status.NotificationError := Diagnostic(AData.Field('notificationError'));
end;

function NewNyxResourceRuntimeDetailView(const AID: TNyxText;
  const AEntry: TNyxResourceRuntimeEntry;
  AAvailability: TNyxResourceDetailAvailability): INyxCard;
const
  CPhases: array[TNyxApplicationResourcePhase] of TNyxText =
    ('Idle', 'Queued', 'Loading', 'Waiting for view', 'Ready', 'Failed', 'Rejected', 'Cancelled');
  COrigins: array[TNyxResourceLoadOrigin] of TNyxText =
    ('Failed', 'Project resource', 'Network', 'Fresh cache', 'Stale cache', 'Declared fallback');
  CTiers: array[TNyxResourceCacheUse] of TNyxText = ('None', 'Memory', 'Persistent');
var
  LDisplayed: TNyxText;
  LAttempt: TNyxText;
  LLocale: TNyxText;
  LAvailable: Boolean;
  procedure AddText(const ASuffix, AText: TNyxText; AVisible: Boolean);
  var
    LLabel: INyxLabel;
  begin
    LLabel := NewNyxLabel(AID + ASuffix).WithText(AText);
    LLabel.Configure.Visible(AVisible).Done;
    Result.Add(LLabel);
  end;
begin
  { Fixed named parts let retained target views update visibility/text without
    replacing a neighbouring resource proposal or its focused physical input. }

  if not (Ord(AAvailability) in [Ord(rdaAwaiting), Ord(rdaMissing), Ord(rdaAvailable)]) then
  begin
    raise ENyxResource.Create('Unknown selected-resource presentation availability');
  end;
  LAvailable := AAvailability = rdaAvailable;
  Result := NewNyxCard(AID);
  Result.Configure.Gap(8).Padding(12).Done;
  LLocale := AEntry.Locale.Name;

  if LLocale = '' then
  begin
    LLocale := 'Default locale';
  end;
  Result.Add(NewNyxHeading(AID + '-title').WithText(AEntry.Reference.Name));
  Result.Add(NewNyxLabel(AID + '-locale').WithText(LLocale));
  AddText('-awaiting', 'Waiting for this resource observation.', AAvailability = rdaAwaiting);
  AddText('-missing', 'This exact resource variant is not present in this run.',
    AAvailability = rdaMissing);
  LAttempt := CPhases[AEntry.Status.Phase];

  if AEntry.Status.Phase in [nrpWaiting, nrpReady, nrpFailed, nrpRejected] then
  begin
    LAttempt := LAttempt + TNyxText(' / ') + COrigins[AEntry.Status.Origin];
  end;
  Result.Add(NewNyxBadge(AID + '-attempt').WithText('Latest attempt: ' + LAttempt));
  Result.Node.Find(AID + '-attempt').Configure.Visible(LAvailable).Done;
  LDisplayed := 'Authored defaults';

  if AEntry.HasPublishedLoad then
  begin
    LDisplayed := COrigins[AEntry.PublishedOrigin];
  end;
  AddText('-displayed', 'Displayed content: ' + LDisplayed, LAvailable);
  AddText('-attempt-cache', 'Latest cache read: ' + CTiers[AEntry.Status.CacheRead] +
    TNyxText(' / write: ') + CTiers[AEntry.Status.CacheWrite], LAvailable);
  AddText('-displayed-cache', 'Displayed cache read: ' + CTiers[AEntry.PublishedCacheRead] +
    TNyxText(' / write: ') + CTiers[AEntry.PublishedCacheWrite], LAvailable and AEntry.HasPublishedLoad);
  AddText('-error', 'Load diagnostic: ' + AEntry.Status.Error,
    LAvailable and (AEntry.Status.Error <> ''));
  AddText('-cache-warning', 'Cache diagnostic: ' + AEntry.Status.CacheWarning,
    LAvailable and (AEntry.Status.CacheWarning <> ''));
  AddText('-notification-error', 'Publication callback: ' + AEntry.Status.NotificationError,
    LAvailable and (AEntry.Status.NotificationError <> ''));
end;

class function TNyxResourceRuntimeSummary.FromData(
  const AData: TNyxDataValue): TNyxResourceRuntimeSummary;
var
  LPhase: TNyxApplicationResourcePhase;
  LUse: TNyxResourceCacheUse;
  LPhases: TNyxDataValue;
  LReads: TNyxDataValue;
  LWrites: TNyxDataValue;
  LPhaseTotal: Integer;
  LReadTotal: Integer;
  LWriteTotal: Integer;
  function Counter(const AValue: TNyxDataValue): Integer;
  begin
    Result := AValue.AsInteger;

    if (Result < 0) or (Result > NyxMaximumResources) then
    begin
      raise ENyxResource.Create('Runtime summary counter is outside 0..128');
    end;
  end;
begin
  Result := Default(TNyxResourceRuntimeSummary);

  if (AData.Kind <> ndObject) or (AData.Count <> 6) then
  begin
    raise ENyxResource.Create('Runtime summary requires six exact fields');
  end;
  Result.FTotal := Counter(AData.Field('total'));
  Result.FPublished := Counter(AData.Field('publishedLoads'));
  Result.FStopped := AData.Field('stopped').AsBoolean;
  LPhases := AData.Field('phases');
  LReads := AData.Field('cacheReads');
  LWrites := AData.Field('cacheWrites');

  if (LPhases.Kind <> ndObject) or (LPhases.Count <> 8) or
    (LReads.Kind <> ndObject) or (LReads.Count <> 3) or
    (LWrites.Kind <> ndObject) or (LWrites.Count <> 3) then
  begin
    raise ENyxResource.Create('Runtime summary requires exact phase/cache counters');
  end;
  LPhaseTotal := 0;
  LReadTotal := 0;
  LWriteTotal := 0;
  for LPhase := Low(TNyxApplicationResourcePhase) to High(TNyxApplicationResourcePhase) do
  begin
    Result.FPhases[LPhase] := Counter(LPhases.Field(NyxResourcePhaseName(LPhase)));
    Inc(LPhaseTotal, Result.FPhases[LPhase]);
  end;
  for LUse := Low(TNyxResourceCacheUse) to High(TNyxResourceCacheUse) do
  begin
    Result.FReads[LUse] := Counter(LReads.Field(NyxResourceCacheUseName(LUse)));
    Result.FWrites[LUse] := Counter(LWrites.Field(NyxResourceCacheUseName(LUse)));
    Inc(LReadTotal, Result.FReads[LUse]);
    Inc(LWriteTotal, Result.FWrites[LUse]);
  end;

  if (LPhaseTotal <> Result.FTotal) or (LReadTotal <> Result.FTotal) or
    (LWriteTotal <> Result.FTotal) or (Result.FPublished > Result.FTotal) then
  begin
    raise ENyxResource.Create('Runtime summary counters must partition their exact total');
  end;
end;

function TNyxResourceRuntimeSummary.PhaseCount(AValue: TNyxApplicationResourcePhase): Integer;
begin
  Result := FPhases[AValue];
end;

function TNyxResourceRuntimeSummary.CacheReads(AValue: TNyxResourceCacheUse): Integer;
begin
  Result := FReads[AValue];
end;

function TNyxResourceRuntimeSummary.CacheWrites(AValue: TNyxResourceCacheUse): Integer;
begin
  Result := FWrites[AValue];
end;

function NewNyxResourceRuntimeView(const AID: TNyxText;
  const ASummary: TNyxResourceRuntimeSummary): INyxCard;
var
  LPhase: TNyxApplicationResourcePhase;
  LStatus: TNyxText;
  LBadge: INyxBadge;
begin
  Result := NewNyxCard(AID);
  Result.Configure.Gap(8).Padding(12).Done;
  LStatus := 'Runtime resources';

  if ASummary.Stopped then
  begin
    LStatus := 'Stopped runtime resources';
  end;
  Result.Add(NewNyxHeading(AID + '-title').WithText(LStatus));
  Result.Add(NewNyxLabel(AID + '-published').WithText(
    'Published loads: ' + TNyxText(IntToStr(ASummary.PublishedLoads)) +
    ' / Resource variants: ' + TNyxText(IntToStr(ASummary.Total))));
  for LPhase := Low(TNyxApplicationResourcePhase) to High(TNyxApplicationResourcePhase) do
  begin

    LBadge := NewNyxBadge(AID + '-' + NyxResourcePhaseName(LPhase)).WithText(
      NyxResourcePhaseName(LPhase) + ': ' + TNyxText(IntToStr(ASummary.PhaseCount(LPhase))));
    LBadge.Configure.Visible(ASummary.PhaseCount(LPhase) <> 0).Done;
    Result.Add(LBadge);
  end;
  Result.Add(NewNyxLabel(AID + '-cache-reads').WithText('Cache reads: memory ' +
    TNyxText(IntToStr(ASummary.CacheReads(rcuMemory))) + ', persistent ' +
    TNyxText(IntToStr(ASummary.CacheReads(rcuPersistent)))));
  Result.Add(NewNyxLabel(AID + '-cache-writes').WithText('Cache writes: memory ' +
    TNyxText(IntToStr(ASummary.CacheWrites(rcuMemory))) + ', persistent ' +
    TNyxText(IntToStr(ASummary.CacheWrites(rcuPersistent)))));
end;

end.
