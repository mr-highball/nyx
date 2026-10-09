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

{ Included by nyx.studio.agents. Only a trusted host can publish copied runtime
  evidence. Public semantic tools inspect it without borrowing live receivers. }

function TNyxAgentSession.GetPendingDraft: Boolean;
begin
  Result := FSession.DraftSource <> FSession.Source;
end;

function NyxStudioRuntime(const AName: TNyxText): TNyxStudioRuntimeRef;
var
  LIndex: Integer;
  LCount: Integer;
  LScalar: Integer;
begin
  Result.FName := NyxResourceRef(AName).Name;
  LIndex := 1;
  LCount := 0;
  while NyxNextScalar(AName, LIndex, LScalar) do
  begin
    Inc(LCount);
  end;

  if LCount > 80 then
  begin
    raise ENyxResource.Create('Runtime name requires at most 80 Unicode scalars');
  end;
end;

function TNyxAgentSession.ObserveResourceRuntime(AExpected: Integer;
  const AReference: TNyxStudioRuntimeRef; AScope: TNyxStudioRuntimeScope;
  ATarget: TNyxPlatform; const AView: TNyxText;
  const ASnapshot: INyxResourceRuntimeSnapshot): TNyxStudioResourceObservation;
var
  LRun: TNyxStudioResourceRun;
  LIndex: Integer;
  LIdentity: TGUID;
  LRetired: Integer;
  LCount: Integer;
  LTarget: Integer;
  LRuns: array of TNyxStudioResourceRun;
  LSnapshots: array of INyxResourceRuntimeSnapshot;
begin
  RequireRevision(NyxObject([NyxField('expectedRevision', NyxData(AExpected))]));
  NyxStudioRuntime(AReference.Name);

  if (FSession.DraftSource <> FSession.Source) or (ASnapshot = nil) or
    (ASnapshot.Count < 0) or (ASnapshot.Count > NyxMaximumResources) or
    not ASnapshot.MatchesDeclarations(FSession.Document.Resources) then
  begin
    raise ENyxResource.Create('Runtime enrollment requires the exact accepted resource declarations and no draft');
  end;

  if not (ATarget in [npfBrowser, npfNativeLCL]) then
  begin
    raise ENyxResource.Create('Runtime enrollment requires a concrete browser/native target');
  end;

  if ((AScope = srsView) and ((FSession.Document.Find(AView) = nil) or
    (FSession.Document.Find(AView).Parent <> nil))) or
    ((AScope <> srsView) and (AView <> '')) then
  begin
    raise ENyxResource.Create('Runtime view must name one exact root; other scopes omit it');
  end;
  LRetired := -1;
  for LIndex := 0 to High(FResourceRuns) do
  begin

    if FResourceRuns[LIndex].Reference.Name = AReference.Name then
    begin
      raise ENyxResource.Create('Runtime name is already enrolled; use a distinct run identity');
    end;

    if (LRetired < 0) and not FResourceRuns[LIndex].Active then
    begin
      LRetired := LIndex;
    end;
  end;

  if (Length(FResourceRuns) = 8) and (LRetired < 0) then
  begin
    raise ENyxResource.Create('Runtime observation budget is eight per design revision');
  end;

  if Length(FResourceRuns) < 8 then
  begin
    LRetired := -1;
  end;

  if CreateGUID(LIdentity) <> 0 then
  begin
    raise ENyxResource.Create('Runtime observation identity could not be created');
  end;
  LRun.Reference := AReference;
  LRun.Scope := AScope;
  LRun.Target := ATarget;
  LRun.View := AView;
  LRun.Authority := TNyxText(GUIDToString(LIdentity));
  LRun.Sequence := 1;
  LRun.Active := not ASnapshot.Stopped;
  LCount := Length(FResourceRuns) + 1;

  if LRetired >= 0 then
  begin
    Dec(LCount);
  end;
  SetLength(LRuns, LCount);
  SetLength(LSnapshots, LCount);
  LTarget := 0;
  for LIndex := 0 to High(FResourceRuns) do
  begin

    if LIndex = LRetired then
    begin
      Continue;
    end;
    LRuns[LTarget] := FResourceRuns[LIndex];
    LSnapshots[LTarget] := FResourceSnapshots[LIndex];
    Inc(LTarget);
  end;
  LRuns[LTarget] := LRun;
  LSnapshots[LTarget] := ASnapshot;
  { All allocation precedes membership exchange. Only inactive evidence may be
    evicted, so long-running hosts can retire/restart without editing a design. }
  FResourceRuns := LRuns;
  FResourceSnapshots := LSnapshots;
  Result.FIdentity := LRun.Authority;
  Log('Application host', 'resource observation', 'enrolled ' + AReference.Name);
end;

procedure TNyxAgentSession.PublishResourceRuntime(
  const AObservation: TNyxStudioResourceObservation;
  const ASnapshot: INyxResourceRuntimeSnapshot);
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FResourceRuns) do
  begin

    if (AObservation.FIdentity <> '') and
      (FResourceRuns[LIndex].Authority = AObservation.FIdentity) then
    begin

      if not FResourceRuns[LIndex].Active or
        (FSession.DraftSource <> FSession.Source) or (ASnapshot = nil) or
        not ASnapshot.MatchesDeclarations(FSession.Document.Resources) then
      begin
        raise ENyxResource.Create('Runtime publication is retired, mismatched or blocked by a draft');
      end;

      if FResourceRuns[LIndex].Sequence = High(Integer) then
      begin
        raise ENyxResource.Create('Runtime observation sequence budget exhausted');
      end;
      FResourceSnapshots[LIndex] := ASnapshot;
      Inc(FResourceRuns[LIndex].Sequence);
      FResourceRuns[LIndex].Active := not ASnapshot.Stopped;
      Log('Application host', 'resource observation', 'updated ' + FResourceRuns[LIndex].Reference.Name);
      Exit;
    end;
  end;
  raise ENyxResource.Create('Runtime observation authority is absent or revoked');
end;

procedure TNyxAgentSession.RetireResourceRuntime(
  const AObservation: TNyxStudioResourceObservation);
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FResourceRuns) do
  begin

    if (AObservation.FIdentity <> '') and
      (FResourceRuns[LIndex].Authority = AObservation.FIdentity) then
    begin

      if FResourceRuns[LIndex].Active then
      begin

        if FResourceRuns[LIndex].Sequence = High(Integer) then
        begin
          raise ENyxResource.Create('Runtime observation sequence budget exhausted');
        end;
        FResourceRuns[LIndex].Active := False;
        Inc(FResourceRuns[LIndex].Sequence);
        Log('Application host', 'resource observation', 'retired ' + FResourceRuns[LIndex].Reference.Name);
      end;
      Exit;
    end;
  end;
  { Retirement is idempotent after a paired edit has already revoked the run. }
end;

function TNyxAgentSession.ResourceRuntimeReports: TNyxDataValue;
begin
  Result := ResourceRuntimeReports(Default(TNyxResourceRef), NyxDefaultLocale);
end;

function TNyxAgentSession.ResourceRuntimeReports(const AReference: TNyxResourceRef;
  const ALocale: TNyxLocaleRef): TNyxDataValue;
const
  CScopes: array[TNyxStudioRuntimeScope] of TNyxText = ('application', 'view', 'resources');
var
  LIndex: Integer;
  LTarget: TNyxText;
  LItems: array of TNyxDataValue;
  LFields: array of TNyxDataField;
  LFieldIndex: Integer;
begin
  SetLength(LItems, Length(FResourceRuns));
  for LIndex := 0 to High(FResourceRuns) do
  begin
    LTarget := 'browser';

    if FResourceRuns[LIndex].Target = npfNativeLCL then
    begin
      LTarget := 'native-lcl';
    end;
    LItems[LIndex] := NyxObject([
      NyxField('run', NyxData(FResourceRuns[LIndex].Reference.Name)),
      NyxField('scope', NyxData(CScopes[FResourceRuns[LIndex].Scope])),
      NyxField('view', NyxData(FResourceRuns[LIndex].View)),
      NyxField('target', NyxData(LTarget)), NyxField('revision', NyxData(FRevision)),
      NyxField('sequence', NyxData(FResourceRuns[LIndex].Sequence)),
      NyxField('active', NyxData(FResourceRuns[LIndex].Active)),
      NyxField('summary', FResourceSnapshots[LIndex].Summary)]);

    if AReference.Name <> '' then
    begin
      SetLength(LFields, LItems[LIndex].Count + 1);
      for LFieldIndex := 0 to LItems[LIndex].Count - 1 do
      begin
        LFields[LFieldIndex] := NyxField(LItems[LIndex].Key(LFieldIndex),
          LItems[LIndex].Field(LItems[LIndex].Key(LFieldIndex)));
      end;
      LFields[High(LFields)] := NyxField('selection',
        NyxSelectedResourceRuntime(FResourceSnapshots[LIndex], AReference, ALocale));
      LItems[LIndex] := NyxObject(LFields);
    end;
  end;
  Result := NyxArray(LItems);
end;

function TNyxAgentSession.ResourceRuntimeQuery(const AArguments: TNyxDataValue): TNyxDataValue;
var
  LName: TNyxText;
  LIndex: Integer;
  LSequence: Integer;
  LOffset: Integer;
  LLimit: Integer;
  LLocale: TNyxLocaleRef;
begin
  RequireRevision(AArguments);

  if TextArgument(AArguments, 'mode') = 'runtimes' then
  begin
    NyxAgentFields(AArguments, '|mode|expectedRevision|');
    Exit(NyxObject([NyxField('origin', NyxData('trusted-runtime-observations')),
      NyxField('items', ResourceRuntimeReports)]));
  end;
  NyxAgentFields(AArguments, '|mode|expectedRevision|run|expectedSequence|offset|limit|reference|locale|');
  LName := NyxStudioRuntime(TextArgument(AArguments, 'run')).Name;
  LSequence := IntegerArgument(AArguments, 'expectedSequence', -1, 1, High(Integer));
  LOffset := IntegerArgument(AArguments, 'offset', 0, 0, NyxMaximumResources);
  LLimit := IntegerArgument(AArguments, 'limit', 8, 1, 16);
  for LIndex := 0 to High(FResourceRuns) do
  begin

    if FResourceRuns[LIndex].Reference.Name = LName then
    begin

      if FResourceRuns[LIndex].Sequence <> LSequence then
      begin
        raise ENyxResource.Create('Runtime observation changed; inspect its current sequence');
      end;

      if NyxAgentHas(AArguments, 'reference') then
      begin

        if NyxAgentHas(AArguments, 'offset') or NyxAgentHas(AArguments, 'limit') then
        begin
          raise ENyxResource.Create('Exact resource observation cannot also request a page');
        end;
        LLocale := NyxDefaultLocale;

        if TextArgument(AArguments, 'locale', '') <> '' then
        begin
          LLocale := NyxLocale(TextArgument(AArguments, 'locale'));
        end;
        Exit(NyxObject([NyxField('run', NyxData(LName)), NyxField('sequence', NyxData(LSequence)),
          NyxField('active', NyxData(FResourceRuns[LIndex].Active)),
          NyxField('selection', NyxSelectedResourceRuntime(FResourceSnapshots[LIndex],
            NyxResourceRef(TextArgument(AArguments, 'reference')), LLocale))]));
      end;

      if NyxAgentHas(AArguments, 'locale') then
      begin
        raise ENyxResource.Create('Resource locale observation requires an exact reference');
      end;
      Exit(NyxObject([NyxField('run', NyxData(LName)), NyxField('sequence', NyxData(LSequence)),
        NyxField('active', NyxData(FResourceRuns[LIndex].Active)),
        NyxField('page', FResourceSnapshots[LIndex].Page(LOffset, LLimit))]));
    end;
  end;
  raise ENyxResource.Create('Runtime observation is absent at this design revision');
end;
