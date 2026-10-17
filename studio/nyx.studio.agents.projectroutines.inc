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

{ Select an accepted leaf and its lexical translation-unit context. A shared
  fragment never acquires an inferred main/helper scope from its filename. }
procedure RoutineContext(const AArguments: TNyxDataValue;
  out AFile, AOwner: TNyxPascalSourceRef);
begin
  AFile := TNyxPascalSourceRef.Main;

  if NyxAgentHas(AArguments, 'file') then
  begin
    AFile := SourceFileSelection(AArguments.Field('file'));
  end;
  AOwner := AFile;

  if NyxAgentHas(AArguments, 'owner') then
  begin
    AOwner := SourceFileSelection(AArguments.Field('owner'));

    if not NyxAgentHas(AArguments, 'file') then
    begin
      AFile := AOwner;
    end;
  end;

  if AOwner.Role = npsInclude then
  begin
    raise ENyxModel.Create('Include routines require an explicit owning main/helper unit');
  end;

  if (AFile.Role <> npsInclude) and not AFile.SameFile(AOwner) then
  begin
    raise ENyxModel.Create('A unit routine query retains that exact owning unit');
  end;
end;

function RoutineMatchesFile(const ASite: TNyxPascalRoutineSite;
  const AFile: TNyxPascalSourceRef): Boolean;
begin
  Result := (AFile.Role <> npsInclude) or ASite.TouchesFile(AFile);
end;

{ Undefined/split spans return no fabricated file/caret coordinates. All source
  locations are accepted physical-file Unicode positions, never expanded offsets. }
function RoutineLocationData(const ALocation: TNyxPascalRoutineLocation): TNyxDataValue;
begin

  if not ALocation.IsDefined then
  begin
    Exit(NyxObject([NyxField('defined', NyxData(False))]));
  end;
  Result := NyxObject([NyxField('defined', NyxData(True)),
    NyxField('file', SourceFileSelectionData(ALocation.FileReference)),
    NyxField('offset', NyxData(ALocation.Offset)), NyxField('line', NyxData(ALocation.Line)),
    NyxField('column', NyxData(ALocation.Column))]);
end;

{ Preserve the simple line field only when it belongs to the queried file.
  In particular, default main queries must never advertise an include's line
  as a main-unit caret. Qualified location objects carry all other file sites. }
function RoutineQueryLine(const ALocation: TNyxPascalRoutineLocation;
  const AFile: TNyxPascalSourceRef): Integer;
begin
  Result := 0;

  if ALocation.IsDefined and ALocation.FileReference.SameFile(AFile) then
  begin
    Result := ALocation.Line;
  end;
end;

function TNyxAgentSession.RoutineIndex(const AOwner: TNyxPascalSourceRef): INyxPascalRoutineIndex;
var
  LInputs: INyxPascalProject;
  LIndex: INyxPascalRoutineIndex;
begin

  if (FSourceRoutineIndex = nil) or (FSourceRoutineRevision <> FRevision) or
    not FSourceRoutineOwner.SameFile(AOwner) then
  begin
    LInputs := FSession.ProjectSnapshot.Files.Open(FSession.Source);

    if LInputs = nil then
    begin
      LInputs := NewNyxPascalProject(NyxPascalUnit(NyxCompanionUnitName(FSession.Source)),
        FSession.Source);
    end;
    LIndex := CaptureNyxPascalRoutineIndex(LInputs, AOwner);
    FSourceRoutineIndex := LIndex;
    FSourceRoutineRevision := FRevision;
    FSourceRoutineOwner := AOwner;
  end;
  Result := FSourceRoutineIndex;
end;

function TNyxAgentSession.ProjectRoutines(const AArguments: TNyxDataValue): TNyxDataValue;
const
  CKindNames: array[TNyxRoutineKind] of TNyxText =
    ('procedure', 'function', 'constructor', 'destructor');
  CVisibility: array[TNyxPascalRoutineVisibility] of TNyxText =
    ('unknown', 'implementation', 'interface');
var
  LIndex: INyxPascalRoutineIndex;
  LFile: TNyxPascalSourceRef;
  LOwner: TNyxPascalSourceRef;
  LSite: TNyxPascalRoutineSite;
  LItems: array of TNyxDataValue;
  LMode: TNyxText;
  LName: TNyxText;
  LPart: TNyxText;
  LWhole: TNyxText;
  LText: TNyxText;
  LSignature: TNyxText;
  LTextSource: TNyxPascalRoutineLocation;
  LScan: Integer;
  LCount: Integer;
  LMatch: Integer;
  LOffset: Integer;
  LLimit: Integer;
  LTotal: Integer;
  LSignatureCount: Integer;
  LFound: Integer;
  LPending: Boolean;
begin
  LMode := TextArgument(AArguments, 'mode');

  if LMode = 'routines' then
  begin
    NyxAgentFields(AArguments, '|mode|expectedRevision|file|owner|offset|limit|');
  end
  else
  begin
    NyxAgentFields(AArguments, '|mode|expectedRevision|file|owner|routine|ordinal|part|offset|count|');

    if (LMode <> 'routine') and (LMode <> 'declaration') then
    begin
      raise ENyxModel.Create('Project routine context requires a supported read mode');
    end;

    if (LMode = 'routine') and NyxAgentHas(AArguments, 'part') then
    begin
      raise ENyxModel.Create('Routine bodies have no declaration part selector');
    end;
  end;

  if NyxAgentHas(AArguments, 'expectedRevision') then
  begin
    RequireRevision(AArguments);
  end;

  if NyxAgentHas(AArguments, 'ordinal') and not NyxAgentHas(AArguments, 'expectedRevision') then
  begin
    raise ENyxModel.Create('Routine inspection ordinals require their exact expectedRevision');
  end;
  RoutineContext(AArguments, LFile, LOwner);
  FSession.SourceFileText(LFile);
  LIndex := RoutineIndex(LOwner);
  LPending := FSession.SourceDraftPending;

  if LFile.Role <> npsMain then
  begin
    LPending := FSession.FileDrafts.Contains(LFile);
  end;

  if LMode = 'routines' then
  begin
    LOffset := IntegerArgument(AArguments, 'offset', 0, 0, 4096);
    LLimit := IntegerArgument(AArguments, 'limit', 20, 1, 50);
    SetLength(LItems, LLimit);
    LCount := 0;
    LMatch := 0;
    for LScan := 0 to LIndex.Count - 1 do
    begin
      LSite := LIndex.Item(LScan);

      if not RoutineMatchesFile(LSite, LFile) then
      begin
        Continue;
      end;

      if (LMatch >= LOffset) and (LCount < LLimit) then
      begin
        LItems[LCount] := NyxObject([
          NyxField('ordinal', NyxData(LScan)), NyxField('routine', NyxData(LSite.Routine.Name)),
          NyxField('kind', NyxData(CKindNames[LSite.Kind])),
          NyxField('line', NyxData(RoutineQueryLine(LSite.Definition, LFile))),
          NyxField('definition', RoutineLocationData(LSite.Definition)),
          NyxField('bodySource', RoutineLocationData(LSite.BodyLocation)),
          NyxField('signatureSource', RoutineLocationData(LSite.SignatureLocation)),
          NyxField('declarationSource', RoutineLocationData(LSite.DeclarationLocation)),
          NyxField('visibility', NyxData(CVisibility[LSite.Visibility])),
          NyxField('editable', NyxData(LSite.Editable)), NyxField('reason', NyxData(LSite.Reason))]);
        Inc(LCount);
      end;
      Inc(LMatch);
    end;
    SetLength(LItems, LCount);
    Exit(NyxObject([NyxField('revision', NyxData(FRevision)),
      NyxField('file', SourceFileSelectionData(LFile)), NyxField('owner', SourceFileSelectionData(LOwner)),
      NyxField('offset', NyxData(LOffset)), NyxField('total', NyxData(LMatch)),
      NyxField('nextOffset', NyxData(Min(LOffset + LCount, LMatch))),
      NyxField('routines', NyxArray(LItems)), NyxField('pendingDraft', NyxData(LPending)),
      NyxField('pendingDrafts', NyxData(FSession.SourceDraftsPending))]));
  end;
  LName := NyxRoutine(TextArgument(AArguments, 'routine')).Name;
  LFound := -1;

  if NyxAgentHas(AArguments, 'ordinal') then
  begin
    LFound := IntegerArgument(AArguments, 'ordinal', -1, 0, 4095);
    LSite := LIndex.Item(LFound);

    if not SameText(LSite.Routine.Name, LName) or not RoutineMatchesFile(LSite, LFile) then
    begin
      raise ENyxModel.Create('Routine inspection ordinal differs from its name/file context');
    end;
  end
  else
  begin
    for LScan := 0 to LIndex.Count - 1 do
    begin
      LSite := LIndex.Item(LScan);

      if SameText(LSite.Routine.Name, LName) and RoutineMatchesFile(LSite, LFile) then
      begin

        if LFound >= 0 then
        begin
          raise ENyxModel.Create('Overloaded routine inspection requires a revision-pinned ordinal');
        end;
        LFound := LScan;
      end;
    end;
  end;

  if LFound < 0 then
  begin
    raise ENyxModel.Create('Routine is absent from this file and owning-unit context');
  end;
  LSite := LIndex.Item(LFound);
  LPart := '';

  if LMode = 'declaration' then
  begin
    LPart := 'interface';

    if NyxAgentHas(AArguments, 'part') then
    begin
      LPart := TextArgument(AArguments, 'part');
    end;

    if LPart = 'implementation' then
    begin
      LWhole := LSite.Signature;
      LTextSource := LSite.SignatureLocation;
    end
    else if LPart = 'interface' then
    begin
      LWhole := LSite.Declaration;
      LTextSource := LSite.DeclarationLocation;
    end
    else
    begin
      raise ENyxModel.Create('Declaration part must be interface or implementation');
    end;
  end
  else
  begin
    LWhole := LSite.Body;
    LTextSource := LSite.BodyLocation;
  end;
  LOffset := IntegerArgument(AArguments, 'offset', 0, 0, NyxMaximumSourceCharacters);
  LLimit := IntegerArgument(AArguments, 'count', 2048, 1, 4096);
  LText := TextSpan(LWhole, LOffset, LLimit, LTotal);

  if LOffset > LTotal then
  begin
    raise ENyxModel.Create('Routine source window is outside its accepted text');
  end;
  LSignature := TextSpan(LSite.Signature, 0, 1024, LSignatureCount);

  if LMode = 'routine' then
  begin
    LMatch := RoutineQueryLine(LSite.Definition, LFile);
  end
  else
  begin
    LMatch := RoutineQueryLine(LTextSource, LFile);
  end;

  if LTextSource.IsDefined then
  begin
    LPending := FSession.SourceDraftPending;

    if LTextSource.FileReference.Role <> npsMain then
    begin
      LPending := FSession.FileDrafts.Contains(LTextSource.FileReference);
    end;
  end
  else
  begin
    LPending := FSession.SourceDraftsPending;
  end;
  Result := NyxObject([NyxField('revision', NyxData(FRevision)),
    NyxField('file', SourceFileSelectionData(LFile)), NyxField('owner', SourceFileSelectionData(LOwner)),
    NyxField('ordinal', NyxData(LFound)), NyxField('routine', NyxData(LSite.Routine.Name)),
    NyxField('kind', NyxData(CKindNames[LSite.Kind])), NyxField('part', NyxData(LPart)),
    NyxField('line', NyxData(LMatch)),
    NyxField('visibility', NyxData(CVisibility[LSite.Visibility])),
    NyxField('definition', RoutineLocationData(LSite.Definition)),
    NyxField('textSource', RoutineLocationData(LTextSource)),
    NyxField('signature', NyxData(LSignature)), NyxField('signatureCharacters', NyxData(LSignatureCount)),
    NyxField('editable', NyxData(LSite.Editable)), NyxField('reason', NyxData(LSite.Reason)),
    NyxField('offset', NyxData(LOffset)), NyxField('total', NyxData(LTotal)),
    NyxField('nextOffset', NyxData(Min(LOffset + LLimit, LTotal))), NyxField('text', NyxData(LText)),
    NyxField('pendingDraft', NyxData(LPending)),
    NyxField('pendingDrafts', NyxData(FSession.SourceDraftsPending))]);
end;

{ Native and semantic callers share the typed owner/body preparation. An include
  selector narrows discovery; it cannot manufacture another owning unit or body. }
function ReadProjectRoutinePatch(const AProject: INyxPascalProject;
  const AChanges: TNyxDataValue): INyxSourceProjectPatch;
var
  LEdits: array of TNyxProjectRoutineEdit;
  LChange: TNyxDataValue;
  LFile: TNyxPascalSourceRef;
  LOwner: TNyxPascalSourceRef;
  LIndex: INyxPascalRoutineIndex;
  LSite: TNyxPascalRoutineSite;
  LScan: Integer;
  LItem: Integer;
  LFound: Integer;
  LName: TNyxRoutineRef;
begin

  if (AChanges.Kind <> ndArray) or (AChanges.Count < 1) or
    (AChanges.Count > NyxMaximumRoutineChanges) then
  begin
    raise ENyxModel.Create('Project routine edits require 1..16 changes');
  end;
  SetLength(LEdits, AChanges.Count);
  for LItem := 0 to AChanges.Count - 1 do
  begin
    LChange := AChanges.Item(LItem);
    NyxAgentFields(LChange, '|file|owner|routine|expected|implementation|');
    RoutineContext(LChange, LFile, LOwner);
    LFile.Text(AProject);
    LName := NyxRoutine(TextArgument(LChange, 'routine'));

    if LFile.Role = npsInclude then
    begin
      LIndex := CaptureNyxPascalRoutineIndex(AProject, LOwner);
      LFound := 0;
      for LScan := 0 to LIndex.Count - 1 do
      begin
        LSite := LIndex.Item(LScan);

        if SameText(LSite.Routine.Name, LName.Name) and LSite.TouchesFile(LFile) then
        begin
          Inc(LFound);
        end;
      end;

      if LFound <> 1 then
      begin
        raise ENyxModel.Create('Selected include has no unambiguous routine in this owner context');
      end;
    end;
    LEdits[LItem] := NyxProjectRoutineEdit(LOwner,
      NyxRoutineEdit(LName, TextArgument(LChange, 'expected'), TextArgument(LChange, 'implementation')));
  end;
  Result := NewNyxProjectRoutinePatch(LEdits);
end;
