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

procedure TNyxAgentSession.RemoveProjectImport(AIndex: Integer);
var
  LIndex: Integer;
begin
  for LIndex := AIndex + 1 to High(FProjectImports) do
  begin
    FProjectImports[LIndex - 1] := FProjectImports[LIndex];
    FProjectUploads[LIndex - 1] := FProjectUploads[LIndex];
    FProjectCandidates[LIndex - 1] := FProjectCandidates[LIndex];
  end;
  SetLength(FProjectImports, Length(FProjectImports) - 1);
  SetLength(FProjectUploads, Length(FProjectUploads) - 1);
  SetLength(FProjectCandidates, Length(FProjectCandidates) - 1);
end;

procedure TNyxAgentSession.ReleaseProjectImports(const AOwner: TNyxText);
var
  LIndex: Integer;
begin
  for LIndex := High(FProjectImports) downto 0 do
  begin

    if FProjectImports[LIndex].Field('owner').AsText = AOwner then
    begin
      RemoveProjectImport(LIndex);
    end;
  end;
end;

function ProjectImportContext(const AUpload: INyxProjectImportUpload;
  const ACandidate: INyxProjectImportCandidate; const AID, AReviewID: TNyxText;
  ARevision: Integer): TNyxDataValue;
var
  LSummary: TNyxProjectImportSummary;
  LCandidate: TNyxDataValue;
  LTotal: Integer;
  LResolution: TNyxText;
begin
  LCandidate := NyxNull;

  if ACandidate <> nil then
  begin
    LSummary := ACandidate.Summary;
    case LSummary.Resolution of
      nprRequireMatch: LResolution := 'match';
      nprUsePascal: LResolution := 'pascal';
      nprUseDesign: LResolution := 'design';
    end;
    LCandidate := NyxObject([
      NyxField('title', NyxData(TextSpan(LSummary.Title, 0, 256, LTotal))),
      NyxField('titleScalars', NyxData(LTotal)),
      NyxField('pages', NyxData(LSummary.Pages)),
      NyxField('components', NyxData(LSummary.Components)),
      NyxField('resources', NyxData(LSummary.Resources)),
      NyxField('stateDefaults', NyxData(LSummary.StateDefaults)),
      NyxField('collections', NyxData(LSummary.Collections)),
      NyxField('sourceScalars', NyxData(LSummary.SourceScalars)),
      NyxField('draftScalars', NyxData(LSummary.DraftScalars)),
      NyxField('draftBaseScalars', NyxData(LSummary.DraftBaseScalars)),
      NyxField('pendingDraft', NyxData(LSummary.Pending)),
      NyxField('designChanged', NyxData(LSummary.DesignChanged)),
      NyxField('sourceChanged', NyxData(LSummary.SourceChanged)),
      NyxField('resolution', NyxData(LResolution))]);
  end;
  Result := NyxObject([
    NyxField('revision', NyxData(ARevision)), NyxField('import', NyxData(AID)),
    NyxField('expectedBytes', NyxData(AUpload.ExpectedBytes)),
    NyxField('receivedBytes', NyxData(AUpload.ReceivedBytes)),
    NyxField('nextOffset', NyxData(AUpload.ScalarCount)),
    NyxField('complete', NyxData(AUpload.Complete)),
    NyxField('reviewID', NyxData(AReviewID)), NyxField('candidate', LCandidate)]);
end;

function ProjectFilePart(const APair: TNyxProjectPair;
  const APart: TNyxText): TNyxText;
begin
  { Names are closed wire choices. Application callers keep the typed pair. }

  if APart = 'project' then
  begin
    Result := EncodeNyxProject(APair);
  end
  else if APart = 'design' then
  begin
    Result := APair.Design;
  end
  else if APart = 'source' then
  begin
    Result := APair.Source;
  end
  else if APart = 'draft' then
  begin
    Result := APair.Draft;
  end
  else if APart = 'draft-base' then
  begin
    Result := APair.DraftBase;
  end
  else
  begin
    raise ENyxModel.Create('Project part must be project, design, source, draft or draft-base');
  end;
end;

function TNyxAgentSession.ProjectFile(const AArguments: TNyxDataValue;
  const AAuthority: TNyxText): TNyxDataValue;
var
  LMode: TNyxText;
  LID: TNyxText;
  LReviewID: TNyxText;
  LPart: TNyxText;
  LText: TNyxText;
  LWindow: TNyxText;
  LIndex: Integer;
  LFound: Integer;
  LReserved: Integer;
  LBytes: Integer;
  LOffset: Integer;
  LCount: Integer;
  LTotal: Integer;
  LResolution: TNyxProjectResolution;
  LUpload: INyxProjectImportUpload;
  LCandidate: INyxProjectImportCandidate;
  LPair: TNyxProjectPair;
begin
  RequireRevision(AArguments);
  LMode := TextArgument(AArguments, 'mode');

  if LMode = 'export' then
  begin
    NyxAgentFields(AArguments, '|mode|expectedRevision|part|offset|count|');
    LPart := AArguments.Field('part').AsText;

    if FProjectExportRevision <> FRevision then
    begin
      FProjectExportPair := FSession.ProjectSnapshot;
      FProjectExportRevision := FRevision;
    end;

    if LPart = 'project' then
    begin

      if FProjectExportPacket = '' then
      begin
        FProjectExportPacket := EncodeNyxProject(FProjectExportPair);
      end;
      LText := FProjectExportPacket;
    end
    else
    begin
      LText := ProjectFilePart(FProjectExportPair, LPart);
    end;
  end
  else if LMode = 'begin-import' then
  begin
    NyxAgentFields(AArguments, '|mode|expectedRevision|operationId|bytes|');
    LBytes := IntegerArgument(AArguments, 'bytes', 0, 1, NyxMaximumProjectImportBytes);
    LReserved := LBytes;
    for LIndex := 0 to High(FProjectUploads) do
    begin
      Inc(LReserved, FProjectUploads[LIndex].ExpectedBytes);
    end;

    if (Length(FProjectImports) >= 8) or (LReserved > 8388608) or
      (FProjectImportSerial = High(Integer)) then
    begin
      raise ENyxModel.Create('Project import reservations exceed eight files or 8 MiB; cancel unused imports');
    end;
    LUpload := NewNyxProjectImport(LBytes);
    LID := 'project-import-' + IntToStr(FProjectImportSerial + 1);
    Result := ProjectImportContext(LUpload, nil, LID, '', FRevision);
    BoundContext(Result);
    Inc(FProjectImportSerial);
    LIndex := Length(FProjectImports);
    SetLength(FProjectImports, LIndex + 1);
    SetLength(FProjectUploads, LIndex + 1);
    SetLength(FProjectCandidates, LIndex + 1);
    FProjectImports[LIndex] := NyxObject([
      NyxField('id', NyxData(LID)), NyxField('owner', NyxData(AAuthority)),
      NyxField('reviewID', NyxData(''))]);
    FProjectUploads[LIndex] := LUpload;
    Exit;
  end
  else
  begin
    LID := AArguments.Field('import').AsText;
    LFound := -1;
    for LIndex := 0 to High(FProjectImports) do
    begin

      if (FProjectImports[LIndex].Field('id').AsText = LID) and
        (FProjectImports[LIndex].Field('owner').AsText = AAuthority) then
      begin
        LFound := LIndex;
        Break;
      end;
    end;

    if LFound < 0 then
    begin
      raise ENyxModel.Create('Project import is missing, expired or owned by another transport');
    end;
    LUpload := FProjectUploads[LFound];
    LCandidate := FProjectCandidates[LFound];
    LReviewID := FProjectImports[LFound].Field('reviewID').AsText;

    if LMode = 'append-import' then
    begin
      NyxAgentFields(AArguments, '|mode|expectedRevision|operationId|import|offset|text|');
      LUpload := LUpload.Append(IntegerArgument(AArguments, 'offset', -1, 0,
        NyxMaximumProjectImportBytes), AArguments.Field('text').AsText);
      Result := ProjectImportContext(LUpload, nil, LID, '', FRevision);
      BoundContext(Result);
      FProjectUploads[LFound] := LUpload;
      Exit;
    end
    else if LMode = 'review-import' then
    begin
      NyxAgentFields(AArguments, '|mode|expectedRevision|operationId|import|resolution|');
      LText := AArguments.Field('resolution').AsText;

      if LText = 'match' then
      begin
        LResolution := nprRequireMatch;
      end
      else if LText = 'pascal' then
      begin
        LResolution := nprUsePascal;
      end
      else if LText = 'design' then
      begin
        LResolution := nprUseDesign;
      end
      else
      begin
        raise ENyxModel.Create('Project resolution must be match, pascal or design');
      end;

      if FProjectReviewSerial = High(Integer) then
      begin
        raise ENyxModel.Create('Project review serial exhausted');
      end;
      LCandidate := LUpload.Review(LResolution);
      LReviewID := 'project-review-' + IntToStr(FProjectReviewSerial + 1);
      Result := ProjectImportContext(LUpload, LCandidate, LID, LReviewID, FRevision);
      BoundContext(Result);
      Inc(FProjectReviewSerial);
      FProjectCandidates[LFound] := LCandidate;
      FProjectImports[LFound] := NyxObject([
        NyxField('id', NyxData(LID)), NyxField('owner', NyxData(AAuthority)),
        NyxField('reviewID', NyxData(LReviewID))]);
      Exit;
    end
    else if LMode = 'apply' then
    begin
      NyxAgentFields(AArguments, '|mode|expectedRevision|operationId|import|reviewID|');

      if (LCandidate = nil) or (LReviewID = '') or
        (AArguments.Field('reviewID').AsText <> LReviewID) then
      begin
        raise ENyxModel.Create('Project apply requires its exact live owner/revision review ticket');
      end;

      if FSession.SourceDraftPending then
      begin
        raise ENyxModel.Create('Resolve the current project draft before applying an imported project');
      end;
      Result := ProjectImportContext(LUpload, LCandidate, LID, LReviewID, FRevision);
      BoundContext(WithResults(Summary, Result, 'projectImport', True));
      LPair := LCandidate.Pair;
      FSession.AdoptProject(LPair);
      RemoveProjectImport(LFound);
      Exit;
    end
    else if LMode = 'cancel-import' then
    begin
      NyxAgentFields(AArguments, '|mode|expectedRevision|operationId|import|');
      Result := NyxObject([NyxField('import', NyxData(LID)),
        NyxField('cancelled', NyxData(True))]);
      RemoveProjectImport(LFound);
      Exit;
    end
    else if LMode = 'inspect-import' then
    begin
      NyxAgentFields(AArguments, '|mode|expectedRevision|import|part|offset|count|');

      if not NyxAgentHas(AArguments, 'part') then
      begin

        if NyxAgentHas(AArguments, 'offset') or NyxAgentHas(AArguments, 'count') then
        begin
          raise ENyxModel.Create('Import text pagination requires an explicit part');
        end;
        Exit(ProjectImportContext(LUpload, LCandidate, LID, LReviewID, FRevision));
      end;
      LPart := AArguments.Field('part').AsText;

      if LPart = 'input' then
      begin
        LText := LUpload.Packet;
      end
      else
      begin

        if LCandidate = nil then
        begin
          raise ENyxModel.Create('Candidate parts require a successful project review');
        end;
        LPair := LCandidate.Pair;
        LText := ProjectFilePart(LPair, LPart);
      end;
    end
    else
    begin
      raise ENyxModel.Create('Unknown project-file mode');
    end;
  end;
  LOffset := IntegerArgument(AArguments, 'offset', 0, 0, NyxMaximumProjectImportBytes);
  LCount := IntegerArgument(AArguments, 'count', 2048, 1, NyxMaximumProjectImportChunk);
  LWindow := TextSpan(LText, LOffset, LCount, LTotal);

  if LOffset > LTotal then
  begin
    raise ENyxModel.Create('Project text offset exceeds the requested part');
  end;
  Result := NyxObject([
    NyxField('revision', NyxData(FRevision)), NyxField('part', NyxData(LPart)),
    NyxField('offset', NyxData(LOffset)), NyxField('total', NyxData(LTotal)),
    NyxField('nextOffset', NyxData(Min(LOffset + LCount, LTotal))),
    NyxField('text', NyxData(LWindow))]);
end;
