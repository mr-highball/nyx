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
program nyx_diagram_content_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses SysUtils, nyx.text, nyx.data, nyx.types, nyx.model, nyx.controls, nyx.state, nyx.events,
  nyx.behavior, nyx.scheduler, nyx.diagram, nyx.diagram.content,
  nyx.diagram.declarations, nyx.diagram.editor, nyx.diagram.adoption, nyx.canvas.types,
  nyx.root.types, nyx.canvas.workspace, nyx.canvas.scene, nyx.publication,
  nyx.canvas.geometry, nyx.resources, nyx.images, nyx.image.fixtures, nyx.drawing,
  nyx.diagram.drawing, nyx.diagram.manipulation, nyx.drawing.view,
  nyx.test.diagram.content
  {$ifdef PAS2JS}, JS, Web, nyx.diagram.editor.browser, nyx.canvas.browser
  {$else}, Classes, Interfaces, Forms, Controls, StdCtrls, ExtCtrls, Graphics,
    nyx.diagram.editor.lcl, nyx.canvas.lcl, nyx.images.lcl, nyx.drawing.lcl, nyx.test.capture.lcl{$endif};

type
  { The installed ordinary button callback borrows its test-owned state store.
    Its independent UI scheduler refuses worker access before touching that
    store. Subscription retirement precedes store disposal. }
  TReply = class(TNyxEventCallback)
  public
    Store: TNyxState;
    Scheduler: INyxScheduler;
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

  {$ifdef PAS2JS}
  { Construct standard browser PointerEvents for bounded host dispatch only.
    No event outlives dispatch; this requires PointerEvent support and does not
    establish hardware, touch or embedded-control occlusion behavior. }
  TPortPointerEvent = class external name 'PointerEvent'(TJSEvent)
    constructor new(const AType: String; const AOptions: TJSObject);
  end;
  {$endif}

  {$ifndef PAS2JS}
  { Borrow a mounted widget only to invoke its installed native Click method;
    this alias creates or owns no controls and claims no hardware input. }
  TControlAccess = class(TControl);
  { Test-owned exception sink outlives all deferred native UI retirement. }
  TFailure = class
  public
    Message: TNyxText;
    procedure Failed(ASender: TObject; AError: Exception);
  end;
  {$endif}

var
  GDocument: TNyxDocument;
  GState: TNyxState;
  GDescriptor: INyxDiagramEditor;
  GView: INyxDiagramEditorView;
  GContent: INyxDiagramContentView;
  GTarget: {$ifdef PAS2JS}INyxBrowserDiagramEditorView{$else}INyxLCLDiagramEditorView{$endif};
  GCanvas: {$ifdef PAS2JS}INyxBrowserCanvasWorkspaceView{$else}INyxLCLCanvasWorkspaceView{$endif};
  GSubscription: INyxEventSubscription;
  GChecks: Integer;
  {$ifdef PAS2JS}
  GHost: TJSHTMLElement;
  {$else}
  GHost: TForm;
  GFailure: TFailure;
  {$endif}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin
  {$ifndef PAS2JS}

  if (GFailure <> nil) and (GFailure.Message <> '') then
  begin
    raise Exception.Create(GFailure.Message);
  end;
  {$endif}

  if not AValue then
  begin
    raise Exception.Create('Diagram content controls: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure TReply.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin
  Scheduler.RequireUI;
  Store.SetValue(NyxIntegerState('replies'), Store.GetValue(NyxIntegerState('replies')) + 1);
end;

{$ifndef PAS2JS}
procedure TFailure.Failed(ASender: TObject; AError: Exception);
begin
  Message := AError.ClassName + ': ' + AError.Message;
end;
{$endif}

{$ifdef PAS2JS}
{ Let actual browser layout/image/event-loop work settle normally. This does not
  drain private schedulers or substitute capture, loading or decoder behavior. }
procedure Pause(AResolve, AReject: TJSPromiseResolver);
begin
  window.setTimeout(
    procedure
    begin
      AResolve(True);
    end, 25);
end;
{$endif}

procedure Idle; {$ifdef PAS2JS}async;{$endif}
{$ifndef PAS2JS}
var
  LTurn: Integer;
{$endif}
begin
  {$ifdef PAS2JS}
  await(TJSPromise.resolve(TJSPromise.new(@Pause)));
  await(TJSPromise.resolve(TJSPromise.new(@Pause)));
  {$else}
  for LTurn := 1 to 3 do
  begin
    Application.ProcessMessages;
    CheckSynchronize;
    Sleep(10);
  end;
  {$endif}
end;

function FindKind(ARoot: TNyxNode; AKind: TNyxKind): TNyxNode;
var
  LIndex: Integer;
begin
  Result := nil;

  if ARoot.Kind = NyxKindName(AKind) then
  begin
    Exit(ARoot);
  end;
  for LIndex := 0 to ARoot.Count - 1 do
  begin
    Result := FindKind(ARoot.Children[LIndex], AKind);

    if Result <> nil then
    begin
      Exit;
    end;
  end;
end;

function MemoID: TNyxText;
var
  LNode: TNyxNode;
begin
  LNode := FindKind(GCanvas.RendererFor(GContent.ContentItem(NyxDiagramVertexRef('Form'))).Root, nkMemo);
  Check(LNode <> nil, 'ordinary materialized reusable contains its Nyx memo');
  Result := LNode.ID;
end;

function ButtonID: TNyxText;
var
  LNode: TNyxNode;
begin
  LNode := FindKind(GCanvas.RendererFor(GContent.ContentItem(NyxDiagramVertexRef('Form'))).Root, nkButton);
  Check(LNode <> nil, 'ordinary materialized reusable contains its Nyx button');
  Result := LNode.ID;
end;

function ReadMemo: TNyxText;
begin
  Result := {$ifdef PAS2JS}TJSHTMLTextAreaElement{$else}TMemo{$endif}(
    GCanvas.RendererFor(GContent.ContentItem(NyxDiagramVertexRef('Form'))).InputFor(MemoID))
    .{$ifdef PAS2JS}value{$else}Text{$endif};
end;

function PartID(APart: TNyxDiagramEditorPart): TNyxText;
begin
  Result := NyxDiagramEditorPart(GDescriptor.ID, APart).ID;
end;

procedure ClickPart(APart: TNyxDiagramEditorPart);
begin
  {$ifdef PAS2JS}
  GTarget.Renderer.ElementFor(PartID(APart)).click;
  {$else}
  TButton(GTarget.Renderer.ControlFor(PartID(APart))).Click;
  {$endif}
end;

procedure WriteField(APart: TNyxDiagramEditorPart; const AValue: TNyxText);
begin
  {$ifdef PAS2JS}
  TJSHTMLInputElement(GTarget.Renderer.InputFor(PartID(APart))).value := AValue;
  GTarget.Renderer.InputFor(PartID(APart)).dispatchEvent(TJSEvent.new('input'));
  {$else}
  TEdit(GTarget.Renderer.InputFor(PartID(APart))).Text := AValue;
  {$endif}
end;

procedure Choose(APart: TNyxDiagramEditorPart; const AValue: TNyxText);
{$ifndef PAS2JS}
var
  LChoice: TComboBox;
{$endif}
begin
  {$ifdef PAS2JS}
  TJSHTMLSelectElement(GTarget.Renderer.InputFor(PartID(APart))).value := AValue;
  GTarget.Renderer.InputFor(PartID(APart)).dispatchEvent(TJSEvent.new('change'));
  {$else}
  LChoice := TComboBox(GTarget.Renderer.InputFor(PartID(APart)));
  Check(LChoice.Items.IndexOf(AValue) >= 0, 'content choice is present in the actual native picker');
  LChoice.ItemIndex := LChoice.Items.IndexOf(AValue);
  LChoice.OnChange(LChoice);
  {$endif}
end;

procedure PinLocale;
begin
  {$ifdef PAS2JS}
  TJSHTMLInputElement(GTarget.Renderer.InputFor(PartID(ndepContentLocalized))).checked := True;
  GTarget.Renderer.InputFor(PartID(ndepContentLocalized)).dispatchEvent(TJSEvent.new('change'));
  {$else}
  TCheckBox(GTarget.Renderer.InputFor(PartID(ndepContentLocalized))).Checked := True;
  {$endif}
end;

function ReadField(APart: TNyxDiagramEditorPart): TNyxText;
begin
  Result := {$ifdef PAS2JS}TJSHTMLInputElement{$else}TCustomEdit{$endif}(
    GTarget.Renderer.InputFor(PartID(APart))).{$ifdef PAS2JS}value{$else}Text{$endif};
end;

function MemoCaret: Integer;
begin
  Result := {$ifdef PAS2JS}TJSHTMLTextAreaElement{$else}TMemo{$endif}(
    GCanvas.RendererFor(GContent.ContentItem(NyxDiagramVertexRef('Form'))).InputFor(MemoID))
    .{$ifdef PAS2JS}selectionStart{$else}SelStart{$endif};
end;

procedure TypeMemo;
{$ifdef PAS2JS}
var
  LInput: TJSHTMLTextAreaElement;
{$else}
var
  LInput: TMemo;
{$endif}
begin
  LInput := {$ifdef PAS2JS}TJSHTMLTextAreaElement{$else}TMemo{$endif}(
    GCanvas.RendererFor(GContent.ContentItem(NyxDiagramVertexRef('Form'))).InputFor(MemoID));
  {$ifdef PAS2JS}
  LInput.value := 'A reply that stays live';
  LInput.focus;
  LInput.dispatchEvent(TJSEvent.new('input'));
  { The matched RTL exposes these standard writable selection fields, while
    its textarea bridge does not declare the setSelectionRange convenience API. }
  LInput.selectionStart := 5;
  LInput.selectionEnd := 5;
  {$else}
  LInput.Text := 'A reply that stays live';
  LInput.SetFocus;
  LInput.SelStart := 5;
  {$endif}
end;

procedure CheckImage; {$ifdef PAS2JS}async;{$endif}
var
  LItem: TNyxCanvasItemRef;
  LID: TNyxText;
  {$ifdef PAS2JS}
  LImage: TJSHTMLImageElement;
  LStarted: Double;
  {$else}
  LImage: TNyxLCLImage;
  LBitmap: TBitmap;
  {$endif}
begin
  LItem := GContent.ContentItem(NyxDiagramVertexRef('Image'));
  LID := GContent.ContentControl(NyxDiagramVertexRef('Image')).ID;
  {$ifdef PAS2JS}
  LImage := TJSHTMLImageElement(GCanvas.RendererFor(LItem).ElementFor(LID));
  Check(LImage <> nil, 'image content exposes its ordinary browser face');
  LStarted := window.performance.now;
  while not LImage.complete do
  begin
    Check(window.performance.now - LStarted < 5000, 'embedded image decode remains bounded');
    await(Idle);
  end;
  Check((LImage.naturalWidth = 100) and (LImage.naturalHeight = 50) and
    (LImage.src = NyxEmbeddedImage(nimPNG, ImagePNG).ToWire), 'browser face decodes the exact packed resource');
  {$else}
  Check(GCanvas.RendererFor(LItem).ControlFor(LID) is TNyxLCLImage,
    'image content exposes its specialized native face');
  LImage := TNyxLCLImage(GCanvas.RendererFor(LItem).ControlFor(LID));
  Check((LImage.Picture.Width = 100) and (LImage.Picture.Height = 50),
    'native content owns the decoded 100 by 50 packed raster');
  LBitmap := TBitmap.Create;
  try
    LBitmap.Assign(LImage.Picture.Graphic);
    Check((ColorToRGB(LBitmap.Canvas.Pixels[5, 5]) = ColorToRGB(clRed)) and
      (ColorToRGB(LBitmap.Canvas.Pixels[95, 5]) = ColorToRGB(clBlue)),
      'native resource image contains the actual independently generated red/blue pixels');
  finally
    LBitmap.Free;
  end;
  {$endif}
end;

procedure Capture(const ASuffix: String);
begin
  {$ifndef PAS2JS}

  if (ParamStr(1) <> '') and (ParamStr(1) <> '--input-only') then
  begin
    { Read the displayed owned window. A form redraw omits embedded child-window
      faces on Win32 and cannot qualify their visible content or clipping. }
    GHost.BringToFront;
    Idle;
    GHost.Update;
    SaveNyxNativeCapture(GHost, ParamStr(1) + ASuffix + '.png', ncmDisplay);
  end;
  {$endif}
end;

procedure Setup;
begin
  GDocument := DiagramContentDocument;
  GState := GDocument.State.Clone;
  GDescriptor := NewNyxDiagramEditor('content-workshop', GDocument, NyxDiagramRef('Content board'));
  {$ifdef PAS2JS}
  GHost := TJSHTMLElement(document.createElement('div'));
  GHost.style.setProperty('height', '100dvh');
  document.body.appendChild(GHost);
  GTarget := MountNyxBrowserDiagramEditor(GDescriptor, GDocument, GHost, GState);
  GCanvas := GTarget.Canvas as INyxBrowserCanvasWorkspaceView;
  {$else}
  GHost := TForm.CreateNew(nil);
  GHost.SetBounds(20, 20, 1080, 760);

  if ParamStr(2) = '--compact' then
  begin
    GHost.SetBounds(20, 20, 390, 800);
  end;
  GHost.Caption := 'Nyx diagram content';
  GHost.Show;
  GTarget := MountNyxLCLDiagramEditor(GDescriptor, GDocument, GHost, GState);
  GCanvas := GTarget.Canvas as INyxLCLCanvasWorkspaceView;
  {$endif}
  GView := GTarget;
  Check(Supports(GView, INyxDiagramContentView, GContent), 'public editor exposes typed content inspection/authoring');
  GView.Canvas.SetCamera(GView.Canvas.Camera.AtCenter(NyxCanvasPoint(80, 20)));
end;

procedure SourceRefresh; {$ifdef PAS2JS}async;{$endif} forward;

{$I nyx_diagram_port_input.inc}

procedure Run; {$ifdef PAS2JS}async;{$endif}
var
  LFormItem: TNyxCanvasItemRef;
  LImageItem: TNyxCanvasItemRef;
  LInitialID: TNyxText;
  LBefore: TNyxText;
  LRevision: Integer;
  LCallback: TReply;
  LFace: {$ifdef PAS2JS}TJSHTMLElement{$else}TControl{$endif};
  LVertex: TNyxDiagramVertex;
  LFailed: Boolean;
  LCaption: TNyxText;
  LHistory: INyxDiagramHistoryView;
begin
  {$ifdef PAS2JS}await(Idle);{$else}Idle;{$endif}
  Check(GView.Connected and (GView.Canvas.MountedCount = 3),
    'background and two ordinary Nyx content roots mount in the same public canvas');
  {$ifdef PAS2JS}await(CheckImage);{$else}CheckImage;{$endif}
  LFormItem := GContent.ContentItem(NyxDiagramVertexRef('Form'));
  LImageItem := GContent.ContentItem(NyxDiagramVertexRef('Image'));
  LInitialID := GContent.ContentControl(NyxDiagramVertexRef('Form')).ID;
  LFace := GCanvas.RendererFor(LFormItem).InputFor(MemoID);
  Check(ReadMemo = GState.GetValue(NyxTextState('reply')), 'live reusable memo reads the independent supplied runtime store');
  TypeMemo;
  {$ifdef PAS2JS}await(Idle);{$else}Idle;{$endif}
  LCaption := ReadMemo;
  Check(GState.GetValue(NyxTextState('reply')) = LCaption, 'actual target text input writes the ordinary typed state binding');
  LCallback := TReply.Create;
  LCallback.Store := GState;
  LCallback.Scheduler := NewNyxScheduler;
  GSubscription := GCanvas.EventsFor(LFormItem).On(NyxControlEvents(ButtonID, niRuntime), ntClick)
    .Subscribe(LCallback);
  {$ifdef PAS2JS}
  GCanvas.RendererFor(LFormItem).ElementFor(ButtonID).click;
  {$else}
  TControlAccess(GCanvas.RendererFor(LFormItem).ControlFor(ButtonID)).Click;
  {$endif}
  {$ifdef PAS2JS}await(Idle);{$else}Idle;{$endif}
  Check(GState.GetValue(NyxIntegerState('replies')) = 1,
    'ordinary injected button callbacks run against the same typed runtime store at ' + ButtonID);

  LVertex := GView.Diagram.FindVertex(NyxDiagramVertexRef('Form'));
  LRevision := GView.Revision;
  GView.SetGeometry(LVertex.Reference, NyxCanvasBounds(80, -70, 340, 280), LRevision);
  {$ifdef PAS2JS}await(Idle);{$else}Idle;{$endif}
  Check((GView.Revision = LRevision + 1) and
    (GCanvas.RendererFor(LFormItem).InputFor(MemoID) = LFace) and (ReadMemo = LCaption) and GSubscription.Active,
    'geometry publication retains the exact live editor, value and callback subscription');
  Check(MemoCaret = 5, 'geometry publication preserves the live memo caret');
  Check(Supports(GView, INyxDiagramHistoryView, LHistory),
    'embedded content uses the ordinary local diagram history');
  LHistory.Undo(GView.Revision);
  {$ifdef PAS2JS}await(Idle);{$else}Idle;{$endif}
  Check((GCanvas.RendererFor(LFormItem).InputFor(MemoID) = LFace) and
    (ReadMemo = LCaption) and (MemoCaret = 5) and GSubscription.Active and
    (GView.Diagram.FindVertex(LVertex.Reference).Bounds.Left = LVertex.Bounds.Left),
    'local geometry Undo retains the exact bound memo, caret, runtime value and callback lifetime');
  LHistory.Redo(GView.Revision);
  {$ifdef PAS2JS}await(Idle);{$else}Idle;{$endif}
  Check((GCanvas.RendererFor(LFormItem).InputFor(MemoID) = LFace) and
    (ReadMemo = LCaption) and (MemoCaret = 5) and GSubscription.Active and
    (GView.Diagram.FindVertex(LVertex.Reference).Bounds.Left = 80),
    'local Redo restores geometry without reconstructing retained embedded controls');
  {$ifdef PAS2JS}await(PortJourney);{$else}PortJourney;{$endif}
  GContent.SetContent(LVertex.Reference, LVertex.Content.Padded(18).CaptionSpace(36), GView.Revision);
  {$ifdef PAS2JS}await(Idle);{$else}Idle;{$endif}
  Check((GCanvas.RendererFor(LFormItem).InputFor(MemoID) = LFace) and (ReadMemo = LCaption) and
    (GContent.ContentControl(LVertex.Reference).ID = LInitialID), 'padding/header edits retain the same control lifetime');

  LBefore := GView.Diagram.ToData.ToJSON;
  LRevision := GView.Revision;
  LFailed := False;
  try
    GContent.SetContent(LVertex.Reference, NyxDiagramComponent(NyxComponent('Missing reusable')), LRevision);
  except
    on ENyxModel do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (GView.Revision = LRevision) and (GView.Diagram.ToData.ToJSON = LBefore) and
    (GCanvas.RendererFor(LFormItem).InputFor(MemoID) = LFace),
    'missing reusable refuses before model, membership or actual controls change');
  LFailed := False;
  try
    GContent.SetContent(NyxDiagramVertexRef('Image'),
      NyxDiagramImage(NyxResourceImage(NyxResourceRef('Missing image'))), LRevision);
  except
    on ENyxResource do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (GView.Diagram.ToData.ToJSON = LBefore) and
    (GContent.ContentItem(NyxDiagramVertexRef('Image')).Name = LImageItem.Name), 'missing resource refuses without retargeting the accepted image lifetime');

  LFailed := False;
  try
    GContent.SetContent(LVertex.Reference, NyxDiagramComponent(NyxComponent('Unavailable drawing')), LRevision);
  except
    on ENyxDrawing do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (GView.Revision = LRevision) and (GView.Diagram.ToData.ToJSON = LBefore) and
    (GCanvas.RendererFor(LFormItem).InputFor(MemoID) = LFace) and GSubscription.Active and
    (ReadMemo = LCaption) and (MemoCaret = 5) and (GView.Canvas.MountedCount = 3),
    'failed target-provider staging retains complete graph, membership, editor, caret and subscription');

  GView.SelectVertex(LVertex.Reference);
  ClickPart(ndepToggleContent);
  Check(GContent.ContentToolsExpanded and not GView.ToolsExpanded and not GView.DrawToolsExpanded,
    'ordinary Content button opens the public alternate panel');
  WriteField(ndepContentPadding, '22.5');
  WriteField(ndepContentHeader, '40');
  Check(GView.HasPendingProperties, 'actual content input remains an unfinished proposal');
  ClickPart(ndepToggleTools);
  Check(not GContent.ContentToolsExpanded and GView.ToolsExpanded and
    (ReadField(ndepContentPadding) = '22.5'), 'switching panels preserves exact hidden content input');
  WriteField(ndepWidth, '360');
  LRevision := GView.Revision;
  ClickPart(ndepApply);
  {$ifdef PAS2JS}await(Idle);{$else}Idle;{$endif}
  Check((GView.Revision = LRevision + 1) and not GView.HasPendingProperties and
    (GView.Diagram.FindVertex(LVertex.Reference).Bounds.Width = 360) and
    (GView.Diagram.FindVertex(LVertex.Reference).Content.Padding = 22.5) and
    (GView.Diagram.FindVertex(LVertex.Reference).Content.HeaderHeight = 40) and
    (GCanvas.RendererFor(LFormItem).InputFor(MemoID) = LFace),
    'one ordinary Apply adopts both geometry and content without remounting live reusable input');
  ClickPart(ndepToggleContent);
  Choose(ndepContentKind, NyxDiagramContentCaption(ndckImage));
  Choose(ndepContentReference, 'Tile image');
  Choose(ndepContentFit, NyxDiagramImageFitCaption(nifCover));
  PinLocale;
  WriteField(ndepContentLocale, 'en-US');
  WriteField(ndepContentFallback, 'en');
  LBefore := GView.Diagram.ToData.ToJSON;
  LRevision := GView.Revision;
  LFailed := False;
  try
    GContent.SetContent(LVertex.Reference, NyxNoDiagramContent, LRevision);
  except
    on ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (GView.Diagram.ToData.ToJSON = LBefore) and GView.HasPendingProperties,
    'typed content mutation refuses while ordinary property input is unfinished');
  Capture('-content');
  ClickPart(ndepApplyContent);
  {$ifdef PAS2JS}await(Idle);{$else}Idle;{$endif}
  LVertex := GView.Diagram.FindVertex(LVertex.Reference);
  Check((GView.Revision = LRevision + 1) and (LVertex.Content.Kind = ndckImage) and
    (LVertex.Content.Image.Reference.Name = 'Tile image') and (LVertex.Content.ImageFit = nifCover) and
    LVertex.Content.Image.Localized and (LVertex.Content.Image.Locale.Name = 'en-US') and
    (LVertex.Content.Image.Fallback.Name = 'en') and not GSubscription.Active,
    'public picker Apply authors a localized typed image and retires the former component callbacks');
  WriteField(ndepContentPadding, '-invalid');
  LBefore := GView.Diagram.ToData.ToJSON;
  LRevision := GView.Revision;
  LFailed := False;
  try
    GView.ApplyProperties;
  except
    on ENyxDiagram do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (GView.Revision = LRevision) and (GView.Diagram.ToData.ToJSON = LBefore) and
    (ReadField(ndepContentPadding) = '-invalid'), 'invalid field admission preserves graph and exact pending input');
  ClickPart(ndepRestoreContent);
  Check(not GView.HasPendingProperties and (ReadField(ndepContentPadding) = '22.5'),
    'ordinary Revert restores the accepted content fields');
  Choose(ndepContentKind, NyxDiagramContentCaption(ndckComponent));
  Choose(ndepContentReference, 'Details');
  ClickPart(ndepApplyContent);
  {$ifdef PAS2JS}await(Idle);{$else}Idle;{$endif}
  Check((GView.Diagram.FindVertex(LVertex.Reference).Content.Kind = ndckComponent) and
    (ReadMemo = LCaption), 'ordinary reusable picker restores a live control using unchanged runtime state');
  ClickPart(ndepToggleContent);

  {$ifndef PAS2JS}

  if ParamStr(2) = '--compact' then
  begin
    GView.SelectVertex(NyxDiagramVertexRef('Form'));
    GView.RevealSelection;
    Idle;
  end;
  {$endif}
  Capture('-live');
  GContent.SetContent(LVertex.Reference, NyxDiagramComponent(NyxComponent('Summary')), GView.Revision);
  {$ifdef PAS2JS}await(Idle);{$else}Idle;{$endif}
  Check(not GSubscription.Active and
    (FindKind(GCanvas.RendererFor(LFormItem).Root, nkMemo) = nil),
    'template replacement retires former callbacks and materializes the independent replacement');
  GContent.SetContent(LVertex.Reference, NyxNoDiagramContent, GView.Revision);
  {$ifdef PAS2JS}await(Idle);{$else}Idle;{$endif}
  Check(not GContent.ContentItem(LVertex.Reference).Defined and (GView.Canvas.MountedCount = 2),
    'explicit clear retires content without deleting shape or its connected edges');
  GContent.SetContent(LVertex.Reference, NyxDiagramComponent(NyxComponent('Details')), GView.Revision);
  {$ifdef PAS2JS}await(Idle);{$else}Idle;{$endif}
  Check((GContent.ContentControl(LVertex.Reference).ID <> LInitialID) and
    (GContent.ContentItem(LVertex.Reference).Name <> LFormItem.Name),
    'removed and recreated content receives fresh control/item authority');
  Check(GDocument.State.GetValue(NyxTextState('reply')) = 'Write a thoughtful reply',
    'runtime editing changes no document-owned defaults');
  {$ifdef PAS2JS}await(SourceRefresh);{$else}SourceRefresh;{$endif}
end;

{$I nyx_diagram_source_input.inc}

procedure Cleanup;
begin

  if GView <> nil then
  begin
    GView.Disconnect;
  end;
  GSubscription := nil;
  GContent := nil;
  GView := nil;
  GCanvas := nil;
  GTarget := nil;
  GDescriptor := nil;
  GDocument.Free;
  GDocument := nil;
  GState.Free;
  GState := nil;
  {$ifdef PAS2JS}

  if GHost <> nil then
  begin
    GHost.remove;
    GHost := nil;
  end;
  {$else}
  Idle;
  GHost.Free;
  GHost := nil;
  Application.OnException := nil;
  GFailure.Free;
  GFailure := nil;
  {$endif}
end;

{$ifdef PAS2JS}
procedure Start; async;
begin
  try
    await(Run);
    Cleanup;
    await(Idle);
    document.body.setAttribute('data-diagram-content-controls', 'passed');
    document.body.setAttribute('data-diagram-content-controls-checks', IntToStr(GChecks));
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-diagram-content-controls', 'failed');
      document.body.setAttribute('data-diagram-content-controls-error', E.Message);
    end;
  end;
end;
{$endif}

begin
  try
    {$ifndef PAS2JS}
    Application.Initialize;
    GFailure := TFailure.Create;
    Application.OnException := GFailure.Failed;
    {$endif}
    Setup;
    {$ifdef PAS2JS}
    window.setTimeout(@Start, 150);
    {$else}
    try
      Run;
    finally
      Cleanup;
    end;
    WriteLn('PASS ', GChecks, ' actual diagram content checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-diagram-content-controls', 'failed');
      document.body.setAttribute('data-diagram-content-controls-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      Cleanup;
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
