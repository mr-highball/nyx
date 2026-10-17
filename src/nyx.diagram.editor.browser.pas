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
unit nyx.diagram.editor.browser;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, Math, JS, Web, nyx.text, nyx.model, nyx.state, nyx.theme,
  nyx.diagram.editor, nyx.render.browser, nyx.canvas.browser,
  nyx.diagram, nyx.diagram.selection, nyx.diagram.declarations, nyx.diagram.adoption,
  nyx.application.context, nyx.flow, nyx.application.resources;

type
  { Borrow the ordinary Browser chrome renderer only until Disconnect. Hosts
    outlive this attachment; target handles grant no source or project authority. }
  INyxBrowserDiagramEditorView = interface(INyxDiagramEditorView)
    ['{49A94FCB-9238-42AE-834F-131A1E3D5104}']
    function GetRenderer: TNyxBrowserRenderer;
    property Renderer: TNyxBrowserRenderer read GetRenderer;
  end;

{ Mount public Nyx chrome and the existing canvas adapter. Document/theme are
  copied; host and runtime state are borrowed until Disconnect. Context supplies
  its exact store when AState is nil; a different supplied store refuses. Without
  either, the view owns independent defaults. Runtime callbacks/graphs use the
  existing revocable canvas behavior; DesignPreview never constructs them.
  Resolver requires context. Disconnect before disposing the host/context owner
  or its borrowed store. No callback/view retains the owning application object.
  Optional AResources delegates hosted image loading/publication to an existing
  application broker through the canvas; caller owns Start/Stop. Neither runtime
  bytes nor broker authority changes the copied authored diagram/document. }
function MountNyxBrowserDiagramEditor(const AEditor: INyxDiagramEditor;
  ADocument: TNyxDocument; AHost: TJSHTMLElement; AState: TNyxState = nil;
  ATheme: TNyxTheme = nil; AMode: TNyxDiagramContentMode = ndcmRuntime;
  const AContext: INyxApplicationContext = nil;
  const AResolver: INyxLogicResolver = nil;
  const AResources: INyxApplicationResources = nil): INyxBrowserDiagramEditorView;

{ Open an independent floating Nyx editor. Dismissal retains its same applied
  model and unfinished controls. A missing name starts an unsaved English workflow;
  only an explicitly supplied save host can publish it through its document owner.
  Content mode/context follow the embedded mount; application runtime authority
  grants no document/source saving authority. Disconnect before its owner retires. }
function NewNyxBrowserDiagramEditorWindow(const AID: TNyxText; ADocument: TNyxDocument;
  const AReference: TNyxDiagramRef; AOwner: TJSHTMLElement;
  const ASaveHost: INyxDiagramSaveHost; ATheme: TNyxTheme = nil;
  AMode: TNyxDiagramContentMode = ndcmRuntime;
  const AContext: INyxApplicationContext = nil;
  const AResolver: INyxLogicResolver = nil;
  const AResources: INyxApplicationResources = nil): INyxDiagramEditorWindow;

implementation

uses
  nyx.types, nyx.controls, nyx.data, nyx.codec, nyx.bytes, nyx.diagram.drawing,
  nyx.drawing, nyx.drawing.view, nyx.canvas.geometry, nyx.canvas.types,
  nyx.canvas.scene, nyx.canvas.workspace, nyx.canvas.routes, nyx.events,
  nyx.behavior, nyx.scheduler, nyx.publication.types, nyx.canvas.observers,
  nyx.interaction, nyx.modal, nyx.modal.browser, nyx.hostspace,
  nyx.diagram.manipulation, nyx.gestures, nyx.gestures.browser,
  nyx.diagram.content, nyx.diagram.materialization, nyx.images, nyx.resources;

type
  { Private alias shares the ordinary Browser renderer; it adds no platform
    toolkit or ownership. The view explicitly owns and disconnects each instance. }
  TDiagramRenderer = TNyxBrowserRenderer;
  { Borrowed target host type; the mounted attachment never frees its caller's
    DOM element. It retires children before the caller releases the host. }
  TDiagramHost = TJSHTMLElement;
  { Alias retains the public existing canvas adapter, including its camera,
    prepared publication, clipping and ordinary embedded Nyx renderer behavior. }
  IDiagramCanvasView = INyxBrowserCanvasWorkspaceView;
  { Target inspection interface preserves the portable editor boundary while
    exposing borrowed target chrome to the owning host/qualification consumer. }
  IDiagramTargetView = INyxBrowserDiagramEditorView;

{$I nyx.diagram.editor.view.inc}

type
  { Existing browser modal owns its HTML dialog element; this alias adds no
    toolkit or source authority. The floating editor retires its child view first. }
  IDiagramModal = INyxBrowserModalHost;

{$I nyx.diagram.editor.window.inc}

function NewNyxBrowserDiagramEditorWindow(const AID: TNyxText; ADocument: TNyxDocument;
  const AReference: TNyxDiagramRef; AOwner: TJSHTMLElement;
  const ASaveHost: INyxDiagramSaveHost; ATheme: TNyxTheme;
  AMode: TNyxDiagramContentMode; const AContext: INyxApplicationContext;
  const AResolver: INyxLogicResolver;
  const AResources: INyxApplicationResources): INyxDiagramEditorWindow;
begin
  Result := TDiagramWindow.Create(AID, ADocument, AReference, AOwner, ASaveHost, ATheme,
    AMode, AContext, AResolver, AResources);
end;

function MountNyxBrowserDiagramEditor(const AEditor: INyxDiagramEditor;
  ADocument: TNyxDocument; AHost: TJSHTMLElement; AState: TNyxState;
  ATheme: TNyxTheme; AMode: TNyxDiagramContentMode;
  const AContext: INyxApplicationContext; const AResolver: INyxLogicResolver;
  const AResources: INyxApplicationResources): INyxBrowserDiagramEditorView;
begin
  Result := TEditorView.Create(AEditor, ADocument, AHost, AState, ATheme,
    AMode, AContext, AResolver, AResources);
end;

end.
