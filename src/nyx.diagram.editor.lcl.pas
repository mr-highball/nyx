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
unit nyx.diagram.editor.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, Math, Classes, Controls, nyx.text, nyx.model, nyx.state, nyx.theme,
  nyx.diagram.editor, nyx.render.lcl, nyx.canvas.lcl;

type
  { Borrow the ordinary LCL chrome renderer only until Disconnect. Hosts
    outlive this attachment; target handles grant no source or project authority. }
  INyxLCLDiagramEditorView = interface(INyxDiagramEditorView)
    ['{49A94FCB-9238-42AE-834F-131A1E3D5103}']
    function GetRenderer: TNyxLCLRenderer;
    property Renderer: TNyxLCLRenderer read GetRenderer;
  end;

{ Mount public Nyx chrome and the existing canvas adapter. Document/theme are
  copied; the target host and optional state are borrowed until Disconnect.
  Without a supplied store this mount owns an independent default store. }
function MountNyxLCLDiagramEditor(const AEditor: INyxDiagramEditor;
  ADocument: TNyxDocument; AHost: TWinControl; AState: TNyxState = nil;
  ATheme: TNyxTheme = nil): INyxLCLDiagramEditorView;

implementation

uses
  nyx.types, nyx.controls, nyx.data, nyx.diagram, nyx.diagram.drawing,
  nyx.drawing, nyx.drawing.view, nyx.canvas.geometry, nyx.canvas.types,
  nyx.canvas.scene, nyx.canvas.workspace, nyx.canvas.routes, nyx.events,
  nyx.behavior, nyx.scheduler, nyx.publication.types, nyx.canvas.observers,
  nyx.interaction;

type
  { Private alias shares the ordinary LCL renderer; it adds no platform
    toolkit or ownership. The view explicitly owns and disconnects each instance. }
  TDiagramRenderer = TNyxLCLRenderer;
  { Borrowed target host type; the mounted attachment never frees its caller's
    LCL control. It retires children before the caller releases the host. }
  TDiagramHost = TWinControl;
  { Alias retains the public existing canvas adapter, including its camera,
    prepared publication, clipping and ordinary embedded Nyx renderer behavior. }
  IDiagramCanvasView = INyxLCLCanvasWorkspaceView;
  { Target inspection interface preserves the portable editor boundary while
    exposing borrowed target chrome to the owning host/qualification consumer. }
  IDiagramTargetView = INyxLCLDiagramEditorView;

{$I nyx.diagram.editor.view.inc}

function MountNyxLCLDiagramEditor(const AEditor: INyxDiagramEditor;
  ADocument: TNyxDocument; AHost: TWinControl; AState: TNyxState;
  ATheme: TNyxTheme): INyxLCLDiagramEditorView;
begin
  Result := TEditorView.Create(AEditor, ADocument, AHost, AState, ATheme);
end;

end.
