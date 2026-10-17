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
{ Shared local graph history. Prepare allocates independent stacks; only the
  existing callback-free coordinated Install adopts them with the graph. }

constructor TDiagramHistoryEntry.Create(const ADiagram: INyxDiagram;
  const ASelection: TNyxDiagramVertexRef; const AEdge: TNyxDiagramEdgeRef;
  const AWire: TNyxText; ABytes: Integer);
begin
  inherited Create;
  FDiagram := ADiagram;
  FSelection := ASelection;
  FEdge := AEdge;
  FWire := AWire;
  FBytes := ABytes;
end;

function TDiagramHistoryEntry.GetDiagram: INyxDiagram;
begin
  Result := FDiagram;
end;

function TDiagramHistoryEntry.GetSelection: TNyxDiagramVertexRef;
begin
  Result := FSelection;
end;

function TDiagramHistoryEntry.GetEdge: TNyxDiagramEdgeRef;
begin
  Result := FEdge;
end;

function TDiagramHistoryEntry.GetWire: TNyxText;
begin
  Result := FWire;
end;

function TDiagramHistoryEntry.GetBytes: Integer;
begin
  Result := FBytes;
end;

function DiagramHistoryPush(const AValues: TDiagramHistoryEntries;
  const AEntry: IDiagramHistoryEntry): TDiagramHistoryEntries;
var
  LIndex: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues) + 1);
  for LIndex := 0 to High(AValues) do
  begin
    Result[LIndex] := AValues[LIndex];
  end;
  Result[High(Result)] := AEntry;
end;

function DiagramHistoryBytes(const AValues: TDiagramHistoryEntries): Integer;
var
  LIndex: Integer;
begin
  Result := 0;
  for LIndex := 0 to High(AValues) do
  begin
    Inc(Result, AValues[LIndex].Bytes);
  end;
end;

function TEditorView.GetUndoCount: Integer;
begin
  RequireConnected;
  Result := Length(FUndo);
end;

function TEditorView.GetRedoCount: Integer;
begin
  RequireConnected;
  Result := Length(FRedo);
end;

function TEditorView.GetHistoryBytes: Integer;
begin
  RequireConnected;
  Result := FHistoryBytes + DiagramHistoryBytes(FUndo) + DiagramHistoryBytes(FRedo);
end;

procedure TEditorView.RefreshHistory;
var
  LEnabled: Boolean;
  LHint: TNyxText;
begin
  LEnabled := (FOptions.MaximumHistoryCommands > 0) and (FOptions.MaximumHistoryBytes > 0);
  LHint := 'Undo the last diagram change';

  if LEnabled and (FHistoryBytes = 0) then
  begin
    LHint := 'Diagram exceeds the configured history byte budget';
  end;
  RetainNyxControl(Part(ndepUndo)).Configure.Visible(LEnabled).Hint(LHint)
    .Enabled((Length(FUndo) > 0) and not GetSavePending).Done;
  RetainNyxControl(Part(ndepRedo)).Configure.Visible(LEnabled)
    .Enabled((Length(FRedo) > 0) and not GetSavePending).Done;
end;

procedure TEditorView.PrepareHistory(ACommit: TDiagramEditorCommit;
  AAction: TDiagramHistoryAction);
var
  LCurrent: IDiagramHistoryEntry;
  LTarget: IDiagramHistoryEntry;
  LDrop: Integer;
  LRemaining: Integer;
  LCount: Integer;
begin

  if (FOptions.MaximumHistoryCommands = 0) or (FOptions.MaximumHistoryBytes = 0) then
  begin
    Exit;
  end;
  LCurrent := TDiagramHistoryEntry.Create(FDiagram, FSelection, FSelectedEdge,
    FHistoryWire, FHistoryBytes);

  if AAction = dhaUndo then
  begin
    LTarget := FUndo[High(FUndo)];
    ACommit.NewUndo := Copy(FUndo, 0, Length(FUndo) - 1);
    ACommit.NewRedo := DiagramHistoryPush(FRedo, LCurrent);
    ACommit.NewHistoryWire := LTarget.Wire;
    ACommit.NewHistoryBytes := LTarget.Bytes;
    Exit;
  end;

  if AAction = dhaRedo then
  begin
    LTarget := FRedo[High(FRedo)];
    ACommit.NewRedo := Copy(FRedo, 0, Length(FRedo) - 1);
    ACommit.NewUndo := DiagramHistoryPush(FUndo, LCurrent);
    ACommit.NewHistoryWire := LTarget.Wire;
    ACommit.NewHistoryBytes := LTarget.Bytes;
    Exit;
  end;
  { The candidate is already a private admitted immutable graph. Encoding it
    cannot invoke an application creator. Cache once per accepted candidate;
    previews and replay use no extra encoding for history. Empty/disabled history
    also avoids retaining a second graph wire representation. }
  ACommit.NewHistoryWire := ACommit.Candidate.ToData.ToJSON;
  ACommit.NewHistoryBytes := NyxUTF8ByteCount(ACommit.NewHistoryWire);

  if ACommit.NewHistoryBytes > FOptions.MaximumHistoryBytes then
  begin
    ACommit.NewHistoryWire := '';
    ACommit.NewHistoryBytes := 0;
    Exit;
  end;

  if ACommit.SourceChange then
  begin
    { Old graphs cannot carry a previous source's resources/definitions back
      into a new source owner. Reset only when that source actually installs. }
    Exit;
  end;

  if ACommit.NewHistoryWire = FHistoryWire then
  begin
    { An exact graph no-op preserves Redo, even if its accepted replacement
      advances the existing revision or normalizes presentation selection. }
    ACommit.NewUndo := FUndo;
    ACommit.NewRedo := FRedo;
    Exit;
  end;

  if FHistoryWire = '' then
  begin
    { The previous graph itself exceeded the budget. The new smaller graph
      starts a fresh bounded history; no oversized checkpoint is retained. }
    Exit;
  end;
  ACommit.NewUndo := DiagramHistoryPush(FUndo, LCurrent);
  { Subtract from the admitted remaining budget instead of temporarily summing
    an oversized proposal. All public totals remain exact portable Integers,
    including when the caller chooses High(Integer) as its byte budget. }
  LRemaining := FOptions.MaximumHistoryBytes - ACommit.NewHistoryBytes;
  LDrop := Length(ACommit.NewUndo);
  LCount := 0;
  while (LDrop > 0) and (LCount < FOptions.MaximumHistoryCommands) do
  begin

    if ACommit.NewUndo[LDrop - 1].Bytes > LRemaining then
    begin
      Break;
    end;
    Dec(LDrop);
    Dec(LRemaining, ACommit.NewUndo[LDrop].Bytes);
    Inc(LCount);
  end;

  if LDrop > 0 then
  begin
    ACommit.NewUndo := Copy(ACommit.NewUndo, LDrop, Length(ACommit.NewUndo) - LDrop);
  end;
  { Redo is deliberately empty on a changed accepted branch. Keeping current
    plus all retained states inside one byte budget makes replay a permutation:
    Undo can always retain its matching Redo without evicting another state. }
end;

procedure TEditorView.ReplayHistory(AAction: TDiagramHistoryAction;
  AExpectedRevision: Integer; ADiscardProperties: Boolean);
var
  LTarget: IDiagramHistoryEntry;
begin
  RequireWritable;

  if (AExpectedRevision <> FRevision) or (FRevision = High(Integer)) then
  begin
    raise ENyxDiagram.Create('Diagram revision is stale or exhausted');
  end;

  if GetSavePending then
  begin
    raise ENyxDiagram.Create('Wait for the pending diagram save before changing local history');
  end;

  if GetHasPendingProperties and not ADiscardProperties then
  begin
    raise ENyxDiagram.Create('Apply or revert unfinished properties before changing diagram history');
  end;

  if AAction = dhaUndo then
  begin

    if Length(FUndo) = 0 then
    begin
      Exit;
    end;
    LTarget := FUndo[High(FUndo)];
  end
  else
  begin

    if Length(FRedo) = 0 then
    begin
      Exit;
    end;
    LTarget := FRedo[High(FRedo)];
  end;
  PublishSourceSelection(LTarget.Diagram, AExpectedRevision, LTarget.Selection,
    LTarget.Edge, False, nil, nil, Default(TNyxDiagramSourceSnapshot), AAction);
end;

procedure TEditorView.Undo(AExpectedRevision: Integer; ADiscardProperties: Boolean);
begin
  ReplayHistory(dhaUndo, AExpectedRevision, ADiscardProperties);
end;

procedure TEditorView.Redo(AExpectedRevision: Integer; ADiscardProperties: Boolean);
begin
  ReplayHistory(dhaRedo, AExpectedRevision, ADiscardProperties);
end;

procedure TEditorView.ClearHistory;
begin
  RequireWritable;
  FUndo := nil;
  FRedo := nil;
  RefreshHistory;
  FRenderer.Sync;
end;
