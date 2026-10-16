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
{ Ordinary property, connection and host-pointer input shares the retained
  content fixture. All added ports/lines are retired before its original journey
  resumes. This checks ownership and input, not a foreground visual capture. }

procedure PortPointer(const AWorld: TNyxCanvasPoint);
var
  LMapping: TNyxDrawingMapping;
  LLocal: TNyxCanvasPoint;
  {$ifdef PAS2JS}
  LFace: TJSHTMLElement;
  LOptions: TJSObject;
  LRect: TJSDOMRect;
  {$else}
  LFace: TControl;
  {$endif}
begin
  {$ifdef PAS2JS}
  LFace := GCanvas.RendererFor(NyxCanvasItem('Diagram picture'))
    .ElementFor(GDescriptor.ID + '-diagram-picture');
  LMapping := NyxDrawingMapping(DrawNyxDiagramEditor(GView.DisplayDiagram, GView.Selection,
    GView.SelectedEdge, 8 / GView.Canvas.Camera.Zoom, 10 / GView.Canvas.Camera.Zoom,
    Default(TNyxCanvasBounds)), LFace.clientWidth, LFace.clientHeight);
  LLocal := LMapping.Point(AWorld);
  LRect := LFace.getBoundingClientRect;
  LOptions := TJSObject.new;
  LOptions['clientX'] := LRect.left + LLocal.X;
  LOptions['clientY'] := LRect.top + LLocal.Y;
  LOptions['button'] := 0;
  LOptions['buttons'] := 1;
  LOptions['pointerId'] := 1;
  LOptions['pointerType'] := 'mouse';
  LOptions['isPrimary'] := True;
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;
  LFace.dispatchEvent(TPortPointerEvent.new('pointerdown', LOptions));
  LOptions['buttons'] := 0;
  LFace.dispatchEvent(TPortPointerEvent.new('pointerup', LOptions));
  {$else}
  LFace := GCanvas.RendererFor(NyxCanvasItem('Diagram picture'))
    .ControlFor(GDescriptor.ID + '-diagram-picture');
  LMapping := NyxDrawingMapping(TNyxLCLDrawing(LFace).Plan, LFace.Width, LFace.Height);
  LLocal := LMapping.Point(AWorld);
  TControlAccess(LFace).MouseDown(mbLeft, [], Round(LLocal.X), Round(LLocal.Y));
  TControlAccess(LFace).MouseUp(mbLeft, [], Round(LLocal.X), Round(LLocal.Y));
  {$endif}
end;

procedure PortJourney; {$ifdef PAS2JS}async;{$endif}
var
  LPorts: INyxDiagramPortsView;
  LForm: TNyxDiagramVertexRef;
  LImage: TNyxDiagramVertexRef;
  LItem: TNyxCanvasItemRef;
  LEdge: TNyxDiagramEdge;
  LOriginal: TNyxText;
  LBefore: TNyxText;
  LCaption: TNyxText;
  LRevision: Integer;
  LFailed: Boolean;
  LFace: {$ifdef PAS2JS}TJSHTMLElement{$else}TControl{$endif};
begin
  Check(Supports(GView, INyxDiagramPortsView, LPorts), 'both adapters expose the same optional typed port editor');
  LForm := NyxDiagramVertexRef('Form');
  LImage := NyxDiagramVertexRef('Image');
  LOriginal := GView.Diagram.ToData.ToJSON;
  LItem := GContent.ContentItem(LForm);
  LFace := GCanvas.RendererFor(LItem).InputFor(MemoID);
  LCaption := ReadMemo;
  GView.SelectVertex(LForm);
  ClickPart(ndepTogglePorts);
  Check(LPorts.PortToolsExpanded and not GView.ToolsExpanded and not GContent.ContentToolsExpanded,
    'ordinary Ports action reveals one optional public workspace');
  WriteField(ndepPortName, 'Message');
  WriteField(ndepPortY, '0.25');
  WriteField(ndepPortCaption, 'Reply output');
  Choose(ndepPortDirection, NyxDiagramPortDirectionCaption(ndaRight));
  Check(GView.HasPendingProperties, 'port inputs stay local until deliberate Apply');
  ClickPart(ndepApplyPort);
  Check(not GView.HasPendingProperties and
    (GView.Diagram.FindVertex(LForm).FindPort(NyxDiagramPortRef('Message')).Position.Y = 0.25),
    'ordinary Apply admits a typed relative attachment');
  LPorts.SetPort(LImage, NyxDiagramPort(NyxDiagramPortRef('Input'), 0, 0.5).Directed(ndaLeft), GView.Revision);
  LPorts.SetPort(LForm, NyxDiagramPort(NyxDiagramPortRef('Loop'), 1, 0.75).Directed(ndaRight), GView.Revision);
  {$ifdef PAS2JS}await(Idle);{$else}Idle;{$endif}
  Check((GCanvas.RendererFor(LItem).InputFor(MemoID) = LFace) and (ReadMemo = LCaption) and
    (MemoCaret = 5) and GSubscription.Active, 'port admission retains embedded text, caret, face and callbacks');
  ClickPart(ndepStartPortConnection);
  ClickPart(ndepCancelPortConnection);
  Check(not GView.ConnectionStart.Defined and not LPorts.ConnectionStartPort.Defined,
    'ordinary Ports cancellation clears only transient connection intent');
  PortPointer(NyxDiagramPortPoint(GView.Diagram.FindVertex(LForm), NyxDiagramPortRef('Message')));
  Check((LPorts.ConnectionStartPort.Name = 'Message') and
    (GView.ConnectionStart.Name = LForm.Name), 'actual underlay pointer hook starts from the visible named port');
  GView.SelectVertex(LImage);
  PortPointer(NyxDiagramPortPoint(GView.Diagram.FindVertex(LImage), NyxDiagramPortRef('Input')));
  LEdge := GView.Diagram.FindEdge(GView.SelectedEdge);
  Check((LEdge.Start.Kind = ndekPort) and (LEdge.Start.Port.Name = 'Message') and
    (LEdge.Finish.Kind = ndekPort) and (LEdge.Finish.Port.Name = 'Input') and
    not GView.ConnectionStart.Defined, 'destination port pointer commits one exact typed connection');

  GView.BeginReconnect(LEdge.Reference, ndceFinish);
  GView.SelectVertex(LForm);
  Choose(ndepPortChoice, NyxDiagramPortChoiceCaption(NyxDiagramPortRef('Loop')));
  ClickPart(ndepFinishPortConnection);
  Check((GView.Diagram.FindEdge(LEdge.Reference).Finish.Vertex.Name = LForm.Name) and
    (GView.Diagram.FindEdge(LEdge.Reference).Finish.Port.Name = 'Loop') and
    (GView.Diagram.FindEdge(LEdge.Reference).Style.ToData.ToJSON = LEdge.Style.ToData.ToJSON),
    'ordinary destination action reconnects in place and retains styles');
  GView.BeginReconnect(LEdge.Reference, ndceFinish);
  GView.SelectVertex(LImage);
  ClickPart(ndepFinishPortConnection);
  Check(GView.Diagram.FindEdge(LEdge.Reference).ToData.ToJSON = LEdge.ToData.ToJSON,
    'reconnection can restore the exact named endpoint without replacing the edge');

  GView.SelectVertex(LForm);
  WriteField(ndepPortY, '-unfinished');
  LBefore := GView.Diagram.ToData.ToJSON;
  LRevision := GView.Revision;
  LFailed := False;
  try
    LPorts.SetPort(LForm, NyxDiagramPort(NyxDiagramPortRef('Other'), 1, 1), LRevision);
  except
    on ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and GView.HasPendingProperties and (ReadField(ndepPortY) = '-unfinished') and
    (GView.Diagram.ToData.ToJSON = LBefore), 'typed authoring refuses while preserving unfinished port fields');
  ClickPart(ndepRestorePort);
  LFailed := False;
  try
    LPorts.RemovePort(LForm, NyxDiagramPortRef('Message'), ndrmRequireDisconnected, LRevision);
  except
    on ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (GView.Revision = LRevision) and (GView.Diagram.ToData.ToJSON = LBefore),
    'connected port removal requires explicit incident-line consent');
  {$ifdef PAS2JS}
  TJSHTMLInputElement(GTarget.Renderer.InputFor(PartID(ndepPortIncidentEdges))).checked := True;
  GTarget.Renderer.InputFor(PartID(ndepPortIncidentEdges)).dispatchEvent(TJSEvent.new('change'));
  {$else}
  TCheckBox(GTarget.Renderer.InputFor(PartID(ndepPortIncidentEdges))).Checked := True;
  {$endif}
  ClickPart(ndepRemovePort);
  Check((GView.Diagram.FindVertex(LForm).PortCount = 1) and
    (GView.Diagram.EdgeCount = NyxDiagramFromData(TNyxDataValue.ParseJSON(LOriginal)).EdgeCount),
    'ordinary explicit removal retires only lines incident to that exact port');
  LPorts.RemovePort(LForm, NyxDiagramPortRef('Loop'), ndrmRequireDisconnected, GView.Revision);
  LPorts.RemovePort(LImage, NyxDiagramPortRef('Input'), ndrmRequireDisconnected, GView.Revision);
  Check(GView.Diagram.ToData.ToJSON = LOriginal, 'port journey leaves the original complete graph exact');
  LPorts.ShowPortTools(False);
  {$ifdef PAS2JS}await(Idle);{$else}Idle;{$endif}
  Check((GCanvas.RendererFor(LItem).InputFor(MemoID) = LFace) and (ReadMemo = LCaption) and GSubscription.Active,
    'connection/reconnection/removal retains the unrelated live content lifetime');
end;
