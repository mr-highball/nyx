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

uses SysUtils, nyx.text, nyx.data, nyx.resources, nyx.application.resources,
  nyx.resources.loader, nyx.controls;

type
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

{ An ordinary public compound card, shared by both Studio targets and embeddable
  in applications. It owns its labels/badges and no live application or callback.
  Status is a copied observation, so painting cannot start network/cache work. }
function NewNyxResourceRuntimeView(const AID: TNyxText;
  const ASummary: TNyxResourceRuntimeSummary): INyxCard;

implementation

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

    if ASummary.PhaseCount(LPhase) <> 0 then
    begin
      Result.Add(NewNyxBadge(AID + '-' + NyxResourcePhaseName(LPhase)).WithText(
        NyxResourcePhaseName(LPhase) + ': ' + TNyxText(IntToStr(ASummary.PhaseCount(LPhase)))));
    end;
  end;
  Result.Add(NewNyxLabel(AID + '-cache-reads').WithText('Cache reads: memory ' +
    TNyxText(IntToStr(ASummary.CacheReads(rcuMemory))) + ', persistent ' +
    TNyxText(IntToStr(ASummary.CacheReads(rcuPersistent)))));
  Result.Add(NewNyxLabel(AID + '-cache-writes').WithText('Cache writes: memory ' +
    TNyxText(IntToStr(ASummary.CacheWrites(rcuMemory))) + ', persistent ' +
    TNyxText(IntToStr(ASummary.CacheWrites(rcuPersistent)))));
end;

end.
