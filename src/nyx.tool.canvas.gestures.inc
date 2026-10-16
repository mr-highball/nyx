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

type
  { A physical gesture borrows the face through this revocable callback. The
    managed lease is acquired before entering owner code; no callback/view cycle
    is retained by the event router. Clearing subscriptions first revokes Owner. }
  TToolGestureCallback = class(TNyxEventCallback)
  public
    Owner: TToolView;
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

procedure TToolGestureCallback.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
var
  LOwner: TToolView;
  LLease: INyxToolCanvasView;
begin
  LOwner := Owner;

  if LOwner = nil then
  begin
    Exit;
  end;
  LLease := LOwner;
  LOwner.Gesture(AEvent, AExecution);
end;

procedure TToolView.ClearGestures;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FGestureCallbacks) do
  begin

    if FGestureCallbacks[LIndex] <> nil then
    begin
      TToolGestureCallback(FGestureCallbacks[LIndex]).Owner := nil;
    end;
  end;
  for LIndex := 0 to High(FGestureSubscriptions) do
  begin

    if FGestureSubscriptions[LIndex] <> nil then
    begin
      FGestureSubscriptions[LIndex].Cancel;
    end;
  end;
  FGestureSubscriptions := nil;
  FGestureCallbacks := nil;
end;

procedure TToolView.BindGestures;

  procedure Collect(ANode: TNyxNode);
  const
    CTriggers: array[0..4] of TNyxTrigger =
      (ntDragStart, ntDragEnter, ntDragOver, ntDrop, ntDragEnd);
  var
    LIndex: Integer;
    LCallback: TToolGestureCallback;
    LCount: Integer;
    LLease: INyxEventCallback;
    LSubscription: INyxEventSubscription;
  begin

    if (ANode.Prop(NyxAttributeName(atDragSource)) = 'true') or
      (ANode.Prop(NyxAttributeName(atDropTarget)) = 'true') then
    begin
      for LIndex := 0 to High(CTriggers) do
      begin
        LCallback := TToolGestureCallback.Create;
        LLease := LCallback;
        LCallback.Owner := Self;
        LSubscription := FRenderer.Events
          .On(NyxControlEvents(ANode.ID, niRuntime), CTriggers[LIndex]).Subscribe(LLease);
        try
          LCount := Length(FGestureSubscriptions);
          SetLength(FGestureSubscriptions, LCount + 1);
          SetLength(FGestureCallbacks, LCount + 1);
          FGestureCallbacks[LCount] := LCallback;
          FGestureSubscriptions[LCount] := LSubscription;
        except
          LCallback.Owner := nil;
          LSubscription.Cancel;
          raise;
        end;
      end;
    end;
    for LIndex := 0 to ANode.Count - 1 do
    begin
      Collect(ANode.Children[LIndex]);
    end;
  end;

begin

  if FConnected and (FRenderer <> nil) and (FRenderer.Root <> nil) then
  begin
    Collect(FRenderer.Root);
  end;
end;

procedure TToolView.Gesture(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
var
  LConnections: INyxToolCanvasConnections;
  LResponse: INyxGestureResponse;
  LNode: TNyxNode;
begin

  if not FConnected or FUpdating or
    not Supports(FInspector, INyxToolCanvasConnections, LConnections) or
    not Supports(AExecution, INyxGestureResponse, LResponse) then
  begin
    Exit;
  end;
  LNode := FRenderer.Root.Find(AEvent.OriginID);

  if LConnections.HandleGesture(LNode, AEvent, LResponse) then
  begin
    QueueRefresh;
  end;
end;

procedure TToolView.RefreshQueued;
var
  LLease: INyxToolCanvasView;
begin

  if not FConnected or FUpdating then
  begin
    Exit;
  end;
  LLease := Self;

  if (FRenderedBlueprint <> nil) and
    (FRenderedBlueprint.Node = FInspector.Root.Node) then
  begin

    if FQueuedUpdate = ntcuNone then
    begin
      Exit;
    end;
    { An explicit caller already rendered this exact replacement. Feedback
      may still need synchronization, but an older queued rebuild must not
      retire the new form's focused editor a second time. }
    FQueuedUpdate := ntcuSynchronize;
  end;
  Refresh;
end;

procedure TToolView.QueueRefresh;
begin

  if not FConnected then
  begin
    Exit;
  end;

  if FRefreshWork = nil then
  begin
    FRefreshWork := NewNyxContentWork({$ifdef PAS2JS}@{$endif}RefreshQueued);
  end;

  if (FRefreshExecution = nil) or
    not (FRefreshExecution.Status in [nesPending, nesRunning]) then
  begin
    FRefreshExecution := FRenderer.Events.Scheduler.Submit(FRefreshWork, neUIQueue);
  end;
end;
