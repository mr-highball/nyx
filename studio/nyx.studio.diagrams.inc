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
{ Per-controller windows retain independent model/input across shell refresh and
  modal dismissal. Session/load identities prevent a cached view from silently
  acquiring another project's source authority. }

{$ifdef PAS2JS}
procedure TNyxStudio.ClearDiagramWindows;
{$else}
procedure TNyxNativeStudio.ClearDiagramWindows;
{$endif}
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FDiagramWindows) do
  begin

    if FDiagramWindows[LIndex] <> nil then
    begin
      FDiagramWindows[LIndex].Disconnect;
      FDiagramWindows[LIndex] := nil;
    end;
  end;
  FDiagramWindows := nil;
  FDiagramContexts := nil;
  FDiagramNames := nil;
end;

{$ifdef PAS2JS}
procedure TNyxStudio.ResumeDiagram;
{$else}
procedure TNyxNativeStudio.ResumeDiagram;
{$endif}
var
  LIndex: Integer;
begin
  {$ifdef PAS2JS}
  FShellRenderer.Events.Scheduler.RequireUI;
  {$else}
  FShellView.Events.Scheduler.RequireUI;
  {$endif}

  if (FLastDiagramWindow < 0) or (FLastDiagramWindow >= Length(FDiagramWindows)) or
    (FDiagramWindows[FLastDiagramWindow] = nil) or
    (FDiagramWindows[FLastDiagramWindow].View = nil) then
  begin
    raise ENyxDiagram.Create('Open a saved diagram or start a new diagram first');
  end;
  for LIndex := 0 to High(FDiagramWindows) do
  begin

    if FDiagramWindows[LIndex] <> nil then
    begin
      FDiagramWindows[LIndex].Hide;
    end;
  end;
  FDiagramWindows[FLastDiagramWindow].Show;
end;

{$ifdef PAS2JS}
function TNyxStudio.ShowDiagram(const AReference: TNyxDiagramRef): INyxDiagramEditorWindow;
{$else}
function TNyxNativeStudio.ShowDiagram(const AReference: TNyxDiagramRef): INyxDiagramEditorWindow;
{$endif}
var
  LReference: TNyxDiagramRef;
  LIndex: Integer;
  LSlot: Integer;
  LNumber: Integer;
  LName: TNyxText;
  LID: TNyxText;
  LUsed: Boolean;
  LHost: INyxDiagramSaveHost;
  LView: INyxDiagramEditorView;
  LCandidate: INyxDiagramEditorWindow;
begin
  {$ifdef PAS2JS}
  FShellRenderer.Events.Scheduler.RequireUI;
  {$else}
  FShellView.Events.Scheduler.RequireUI;
  {$endif}

  if ((FSourceModal <> nil) and FSourceModal.IsOpen) or
    ((FQueryPreviewModal <> nil) and FQueryPreviewModal.IsOpen) then
  begin
    raise ENyxDiagram.Create('Close the other floating workspace before opening a diagram');
  end;
  LHost := FSourceCommands.DiagramSaveHost(FSession.CommandContext);
  LReference := AReference;

  if not LReference.Defined then
  begin
    LNumber := 1;
    repeat
      LName := 'Diagram';

      if LNumber > 1 then
      begin
        LName := LName + ' ' + TNyxText(IntToStr(LNumber));
      end;
      LReference := NyxDiagramRef(LName);
      LUsed := FSession.Document.Diagrams.Contains(LReference);
      for LIndex := 0 to High(FDiagramWindows) do
      begin
        LUsed := LUsed or ((FDiagramWindows[LIndex] <> nil) and
          FSession.MatchesCommandContext(FDiagramContexts[LIndex]) and
          (FDiagramNames[LIndex].Name = LReference.Name));
      end;
      Inc(LNumber);
    until not LUsed;
  end;
  LSlot := -1;
  for LIndex := 0 to High(FDiagramWindows) do
  begin

    if (FDiagramWindows[LIndex] <> nil) and
      FSession.MatchesCommandContext(FDiagramContexts[LIndex]) and
      (FDiagramNames[LIndex].Name = LReference.Name) then
    begin
      LView := FDiagramWindows[LIndex].View;

      if (LView <> nil) and (LView.HasUnsavedChanges or LView.SavePending or
        (FSession.Document.Diagrams.Contains(LReference) and
          (LView.Diagram.ToData.ToJSON =
            FSession.Document.Diagrams.Definition(LReference).ToData.ToJSON))) then
      begin
        FLastDiagramWindow := LIndex;
        ResumeDiagram;
        Exit(FDiagramWindows[LIndex]);
      end;
      LSlot := LIndex;
      Break;
    end;
  end;

  if LSlot < 0 then
  begin
    LSlot := Length(FDiagramWindows);

    if LSlot >= 16 then
    begin
      LSlot := -1;
      for LIndex := 0 to High(FDiagramWindows) do
      begin

        if FDiagramWindows[LIndex] = nil then
        begin
          LSlot := LIndex;
          Break;
        end;
        LView := FDiagramWindows[LIndex].View;

        if (LView = nil) or (not FDiagramWindows[LIndex].IsOpen and
          not LView.HasUnsavedChanges and not LView.SavePending) then
        begin
          LSlot := LIndex;
          Break;
        end;
      end;

      if LSlot < 0 then
      begin
        raise ENyxDiagram.Create('Save a retained diagram before opening another workspace');
      end;
    end
    else
    begin
      SetLength(FDiagramWindows, LSlot + 1);
      SetLength(FDiagramContexts, LSlot + 1);
      SetLength(FDiagramNames, LSlot + 1);
    end;
  end;
  for LIndex := 0 to High(FDiagramWindows) do
  begin

    if FDiagramWindows[LIndex] <> nil then
    begin
      FDiagramWindows[LIndex].Hide;
    end;
  end;
  repeat
    Inc(FDiagramSerial);
    LID := 'studio-diagram-' + TNyxText(IntToStr(FDiagramSerial));
  until FSession.Document.Find(LID) = nil;
  {$ifdef PAS2JS}
  LCandidate := NewNyxBrowserDiagramEditorWindow(LID, FSession.Document,
    LReference, FShellHost, LHost);
  {$else}
  LCandidate := NewNyxLCLDiagramEditorWindow(LID, FSession.Document,
    LReference, FHost, LHost, FTheme);
  {$endif}

  if FDiagramWindows[LSlot] <> nil then
  begin
    FDiagramWindows[LSlot].Disconnect;
  end;
  FDiagramWindows[LSlot] := LCandidate;
  FDiagramContexts[LSlot] := FSession.CommandContext;
  FDiagramNames[LSlot] := LReference;
  FLastDiagramWindow := LSlot;
  Result := LCandidate;
end;
