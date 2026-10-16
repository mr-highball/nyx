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
{ Gesture capture and coalesced previews share the ordinary canvas publisher.
  A preview owns immutable graph data but never changes accepted Diagram/revision. }

procedure TDiagramFrameWork.Execute(const AExecution: INyxExecution);
var
  LOwner: TEditorView;
  LLease: INyxDiagramEditorView;
begin
  LOwner := Owner;

  if (LOwner = nil) or AExecution.Cancelled then
  begin
    Exit;
  end;
  LLease := LOwner;

  if LOwner.GetConnected then
  begin
    LOwner.FFrameQueued := False;
    LOwner.RefreshFrame;
  end;
end;

function TEditorView.Projection(const ADiagram: INyxDiagram;
  const AVertex: TNyxDiagramVertexRef; const AEdge: TNyxDiagramEdgeRef): INyxDrawingPlan;
var
  LZoom: Double;
  LMinimum: TNyxCanvasBounds;
begin
  LZoom := FOptions.Camera.Zoom;

  if FCanvas <> nil then
  begin
    LZoom := FCanvas.Camera.Zoom;
  end;
  LMinimum := Default(TNyxCanvasBounds);

  if FPointerActive then
  begin
    LMinimum := FPointerMinimum;
  end;
  Result := DrawNyxDiagramEditor(ADiagram, AVertex, AEdge, 8 / LZoom, LMinimum);
end;

function TEditorView.GetDisplayDiagram: INyxDiagram;
begin
  RequireConnected;
  Result := FPreviewDiagram;

  if Result = nil then
  begin
    Result := FDiagram;
  end;
end;

function TEditorView.GetManipulating: Boolean;
begin
  RequireConnected;
  Result := FPointerActive;
end;

procedure TEditorView.QueueFrame;
begin

  if not FConnected or FFrameQueued or (FFrameLease = nil) then
  begin
    Exit;
  end;
  FFrameQueued := True;
  try
    FScheduler.PostUI(FFrameLease);
  except
    FFrameQueued := False;
    raise;
  end;
end;

function SameDiagramCamera(const ALeft, ARight: TNyxCanvasCamera): Boolean;
begin
  Result := (ALeft.Zoom = ARight.Zoom) and
    (ALeft.WorldCenter.X = ARight.WorldCenter.X) and
    (ALeft.WorldCenter.Y = ARight.WorldCenter.Y) and
    (ALeft.ViewportWidth = ARight.ViewportWidth) and
    (ALeft.ViewportHeight = ARight.ViewportHeight);
end;

function TEditorView.PointerFrameCurrent: Boolean;
var
  LPolicy: TNyxInteractionPolicy;
begin
  Result := False;

  if not FConnected or not FPointerActive or FPointerAbortRequested then
  begin
    Exit;
  end;
  LPolicy := NyxInteractionPolicy(FRenderer.Root);
  Result := LPolicy.CanIssueCommand and not LPolicy.ReadOnly and
    (FRevision = FPointerRevision) and (PropertyBuffer = FPointerProperties) and
    (FSelection.Name = FPointerChange.Vertex.Reference.Name) and
    SameDiagramCamera(FPointerCamera, FCanvas.Camera) and
    (FPointerEvents = FPointerEventsLease) and
    (FPointerEvents.ViewRevision = FPointerEventRevision);
end;

function TEditorView.PointerWorld(const AEvent: TNyxEventInfo): TNyxCanvasPoint;
var
  LGeometry: INyxCanvasControlGeometry;
  LBounds: TNyxCanvasBounds;
begin

  if not AEvent.Pointer.HasPosition or
    not Supports(FCanvas, INyxCanvasControlGeometry, LGeometry) or
    not LGeometry.TryControlBounds(FItem, NyxControl(FPicture.ID), LBounds) then
  begin
    raise ENyxDiagram.Create('Diagram pointer has no current allocated canvas plane');
  end;
  Result := FCanvas.Camera.ViewportToWorld(NyxCanvasPoint(
    LBounds.Left + AEvent.Pointer.X, LBounds.Top + AEvent.Pointer.Y));
end;

procedure TEditorView.ReleaseCapturedPointer;
{$ifdef PAS2JS}
var
  LFace: TJSHTMLElement;
{$else}
var
  LFace: TControl;
{$endif}
begin

  if (FCanvas = nil) or (FCanvas.MountedCount = 0) or (FPicture = nil) then
  begin
    Exit;
  end;
  {$ifdef PAS2JS}
  LFace := FCanvas.RendererFor(FItem).ElementFor(FPicture.ID);

  if (LFace <> nil) and TNyxGestureElement(LFace).hasPointerCapture(FPointerID) then
  begin
    LFace.releasePointerCapture(FPointerID);
  end;
  {$else}
  LFace := FCanvas.RendererFor(FItem).ControlFor(FPicture.ID);
  ReleaseNyxLCLPointer(LFace);
  {$endif}
end;

procedure TEditorView.RetirePointer;
var
  LCaptured: Boolean;
begin
  LCaptured := FPointerActive;
  FPointerActive := False;
  FPointerAbortRequested := False;
  FPointerSamplePending := False;
  FPointerEventsLease := nil;
  FPointerMinimum := Default(TNyxCanvasBounds);

  if LCaptured then
  begin
    ReleaseCapturedPointer;
  end;
end;

procedure TEditorView.CancelManipulation;
var
  LLease: INyxDiagramEditorView;
begin
  RequireConnected;

  if FPublishing then
  begin
    FPointerAbortRequested := True;
    QueueFrame;
    Exit;
  end;
  LLease := Self;
  RetirePointer;

  if (FPreviewDiagram <> nil) or (FProjectionZoom <> FCanvas.Camera.Zoom) then
  begin
    PublishSelection(FDiagram, FRevision, FSelection, FSelectedEdge, True);
    FPreviewDiagram := nil;
  end;
end;

procedure TEditorView.RefreshFrame;
var
  LChange: TNyxDiagramManipulation;
begin
  RequireConnected;

  if FPointerActive then
  begin

    if not PointerFrameCurrent then
    begin
      CancelManipulation;
      Exit;
    end;

    if FPointerSamplePending then
    begin
      FPointerSamplePending := False;
      try
        LChange := FPointerChange.At(FPointerSample, FOptions.Grid, FPointerBypassGrid);
        PublishSelection(FDiagram.WithVertex(LChange.Vertex), FPointerRevision,
          FSelection, FSelectedEdge, True);
        FPointerChange := LChange;
      except
        on E: Exception do
        begin
          CancelManipulation;
          Feedback(E.Message);
        end;
      end;
    end;
  end
  else if (FPreviewDiagram <> nil) or (FProjectionZoom <> FCanvas.Camera.Zoom) then
  begin
    PublishSelection(FDiagram, FRevision, FSelection, FSelectedEdge, True);
    FPreviewDiagram := nil;
  end;
end;

function TEditorView.BeginPointer(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution): Boolean;
var
  LResponse: INyxGestureResponse;
  LEventResponse: INyxEventResponse;
  LPoint: TNyxCanvasPoint;
  LVertex: TNyxDiagramVertexRef;
  LGrip: TNyxDiagramGrip;
  LCamera: TNyxCanvasCamera;
  {$ifdef PAS2JS}
  LFocus: TJSHTMLElement;
  {$else}
  LFocus: TWinControl;
  {$endif}
begin
  Result := False;

  if not Supports(AExecution, INyxGestureResponse, LResponse) or
    not LResponse.CanRequest(ngcCapturePointer) then
  begin
    Exit;
  end;
  LPoint := PointerWorld(AEvent);
  { A zoom notification queues re-projection. Preserve the event's old target
    coordinates first, then settle the grip/allocation plane before capture. }

  if FProjectionZoom <> FCanvas.Camera.Zoom then
  begin
    RefreshFrame;
  end;
  LGrip := ndgNone;

  if FSelection.Defined then
  begin
    LGrip := HitNyxDiagramGrip(FDiagram.FindVertex(FSelection).Bounds,
      LPoint, 7 / FCanvas.Camera.Zoom);
  end;
  LVertex := FSelection;

  if LGrip = ndgNone then
  begin
    LVertex := FDiagram.PickVertex(LPoint);
  end;

  if not LVertex.Defined then
  begin
    Exit;
  end;

  if GetHasPendingProperties then
  begin
    raise ENyxDiagram.Create('Apply or revert unfinished geometry before dragging');
  end;
  LCamera := FCanvas.Camera;
  SelectVertex(LVertex, False);
  {$ifdef PAS2JS}
  LFocus := FRenderer.FocusFor(NyxDiagramEditorPart(FEditorID, ndepSelection).ID);

  if LFocus <> nil then
  begin
    NyxFocusWithoutScroll(LFocus);
  end;
  {$else}
  LFocus := FRenderer.FocusFor(NyxDiagramEditorPart(FEditorID, ndepSelection).ID);

  if (LFocus <> nil) and LFocus.CanFocus then
  begin
    LFocus.SetFocus;
  end;
  {$endif}
  RequireConnected;
  Result := True;

  if not SameDiagramCamera(LCamera, FCanvas.Camera) then
  begin
    { A selection label may reflow chrome. The first click selects; a fresh
      gesture captures the newly allocated plane rather than inventing a delta. }
    Exit;
  end;

  if LGrip = ndgNone then
  begin
    FPointerChange := NyxDiagramMove(FDiagram.FindVertex(LVertex), LPoint);
  end
  else
  begin
    FPointerChange := NyxDiagramResize(FDiagram.FindVertex(LVertex), LGrip, LPoint);
  end;
  FPointerCamera := FCanvas.Camera;
  FPointerRevision := FRevision;
  FPointerProperties := PropertyBuffer;
  FPointerMinimum := FProvider.Plan.ViewBox;
  FPointerEventsLease := FPointerEvents;
  FPointerEventRevision := FPointerEvents.ViewRevision;
  FPointerID := AEvent.Pointer.ID;
  FPointerActive := True;
  FPointerAbortRequested := False;
  FPointerSamplePending := False;
  LResponse.CapturePointer;
  LEventResponse := NyxEventResponse(AExecution);

  if LEventResponse.CanConsume then
  begin
    LEventResponse.Consume;
  end;
end;

procedure TEditorView.HandlePointer(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
var
  LChange: TNyxDiagramManipulation;
  LPoint: TNyxCanvasPoint;
  LRevision: Integer;
  LLease: INyxDiagramEditorView;
  LResponse: INyxEventResponse;
begin

  if not FPointerActive then
  begin
    Exit;
  end;

  if AEvent.HasPointer and (AEvent.Pointer.ID <> FPointerID) then
  begin
    Exit;
  end;

  if FPublishing then
  begin

    if AEvent.Trigger in [ntPointerCancel, ntPointerCaptureLost, ntAfterExit] then
    begin
      FPointerAbortRequested := True;
      QueueFrame;
    end;
    Exit;
  end;
  LLease := Self;

  if (AEvent.Trigger in [ntPointerCancel, ntPointerCaptureLost, ntAfterExit]) or
    not PointerFrameCurrent then
  begin
    CancelManipulation;
    Exit;
  end;

  if not (AEvent.Trigger in [ntPointerMove, ntPointerUp]) then
  begin
    Exit;
  end;
  LResponse := NyxEventResponse(AExecution);

  if LResponse.CanConsume then
  begin
    LResponse.Consume;
  end;
  try
    LPoint := PointerWorld(AEvent);

    if AEvent.Trigger = ntPointerMove then
    begin
      FPointerSample := LPoint;
      FPointerBypassGrid := nmAlt in AEvent.Pointer.Modifiers;
      FPointerSamplePending := True;
      QueueFrame;
      Exit;
    end;
    LRevision := FPointerRevision;
    LChange := FPointerChange.At(LPoint, FOptions.Grid, nmAlt in AEvent.Pointer.Modifiers);
    RetirePointer;

    if LChange.Changed then
    begin
      { Capture is retired first; only this synchronous internal commit may
        replace the still-visible preview. The publisher separately reserves
        all reentrant writes before calling any target preparation/observers. }
      FCommittingPointer := True;
      try
        try
          SetGeometry(LChange.Vertex.Reference, LChange.Bounds, LRevision);
        except
          CancelManipulation;
          raise;
        end;
      finally
        FCommittingPointer := False;
      end;
    end
    else
    begin
      CancelManipulation;
    end;
  except
    CancelManipulation;
    raise;
  end;
end;
