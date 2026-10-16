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
unit nyx.diagram.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, Math, nyx.text, nyx.bytes, nyx.types, nyx.model, nyx.controls, nyx.diagram,
  nyx.canvas.geometry, nyx.canvas.workspace;

type
  { Stable public composition parts; callers use typed references rather than
    depending on private descendant IDs. Mounted faces remain adapter-owned. }
  TNyxDiagramEditorPart = (ndepToolbar, ndepCanvasHost, ndepTools, ndepSelection,
    ndepPrevious, ndepNext, ndepZoomOut, ndepZoomIn, ndepFit, ndepActualSize,
    ndepToggleTools, ndepLeft, ndepUp, ndepRight, ndepDown, ndepX, ndepY,
    ndepWidth, ndepHeight, ndepCaption, ndepApply, ndepRestore, ndepStatus);

  { Copied runtime presentation policy. Camera/grid reuse the public canvas
    values; Nudge is a positive world-unit distance, not a device pixel count.
    Tools start collapsed unless explicitly requested. Options own no view. }
  TNyxDiagramEditorOptions = record
  private
    FCamera: TNyxCanvasCamera;
    FGrid: TNyxCanvasGrid;
    FNudge: Double;
    FToolsExpanded: Boolean;
  public
    function WithCamera(const AValue: TNyxCanvasCamera): TNyxDiagramEditorOptions;
    function WithGrid(const AValue: TNyxCanvasGrid): TNyxDiagramEditorOptions;
    function Nudge(AWorldDistance: Double): TNyxDiagramEditorOptions;
    function ToolsExpanded(AValue: Boolean): TNyxDiagramEditorOptions;
    property Camera: TNyxCanvasCamera read FCamera;
    property Grid: TNyxCanvasGrid read FGrid;
    property NudgeDistance: Double read FNudge;
    property InitiallyExpanded: Boolean read FToolsExpanded;
  end;

  { Public managed Panel/Row/Button/Input composition. Each mount independently
    captures InitialDiagram and options, with its own selection/camera/properties.
    No callback, application or renderer is retained by this tree. Plain Node
    cloning copies chrome only; editable graph intent requires InitialDiagram or
    an explicit mounted snapshot, never recovery from the Drawing projection. }
  INyxDiagramEditor = interface(INyxPanel)
    ['{49A94FCB-9238-42AE-834F-131A1E3D5101}']
    { Keep the specialized fluent contract when naming this composition. }
    function Captioned(const AText: TNyxText): INyxDiagramEditor;
    function GetInitialDiagram: INyxDiagram;
    function GetOptions: TNyxDiagramEditorOptions;
    function WithOptions(const AOptions: TNyxDiagramEditorOptions): INyxDiagramEditor;
    property InitialDiagram: INyxDiagram read GetInitialDiagram;
    property Options: TNyxDiagramEditorOptions read GetOptions;
  end;

  { Default reference-counted composition. The initial graph is normalized into
    an independent immutable value; subclasses retain ordinary Nyx named parts.
    Runtime editing is owned by a mounted view, not this descriptor. }
  TNyxDiagramEditor = class(TNyxPanel, INyxDiagramEditor)
  private
    FInitial: INyxDiagram;
    FOptions: TNyxDiagramEditorOptions;
  public
    constructor Create(const AID: TNyxText; const ADiagram: INyxDiagram); reintroduce;
    function Captioned(const AText: TNyxText): INyxDiagramEditor;
    function GetInitialDiagram: INyxDiagram;
    function GetOptions: TNyxDiagramEditorOptions;
    function WithOptions(const AOptions: TNyxDiagramEditorOptions): INyxDiagramEditor;
  end;

  { UI-owned mounted editor. Revisions count accepted graph replacements only;
    camera/selection and unfinished properties are separate presentation state.
    Exact snapshots export through Diagram.ToData. Revision checks and complete
    candidate/projection admission precede publication with scene geometry.
    No project/source/history, file or executable-graph authority is granted.
    Hosts integrate exported intent through their normal document owner.
    Selecting/replacing while properties are unfinished refuses unless deliberate
    discard is requested. Hiding tools and navigating never discard those fields.
    Disconnect revokes callbacks before releasing target faces; all calls and
    final connected release belong to the owning UI scheduler. }
  INyxDiagramEditorView = interface(IInterface)
    ['{49A94FCB-9238-42AE-834F-131A1E3D5102}']
    function GetConnected: Boolean;
    function GetDiagram: INyxDiagram;
    function GetRevision: Integer;
    function GetSelection: TNyxDiagramVertexRef;
    function GetCanvas: INyxCanvasWorkspaceView;
    function GetHasPendingProperties: Boolean;
    function GetToolsExpanded: Boolean;
    function GetLastError: TNyxText;
    procedure SelectVertex(const AReference: TNyxDiagramVertexRef;
      ADiscardProperties: Boolean = False);
    procedure SelectAt(const AWorldPoint: TNyxCanvasPoint;
      ADiscardProperties: Boolean = False);
    procedure ReplaceDiagram(const ADiagram: INyxDiagram; AExpectedRevision: Integer;
      ADiscardProperties: Boolean = False);
    procedure SetGeometry(const AReference: TNyxDiagramVertexRef;
      const ABounds: TNyxCanvasBounds; AExpectedRevision: Integer);
    procedure NudgeSelection(AX, AY: Double; AExpectedRevision: Integer);
    procedure ApplyProperties;
    procedure RestoreProperties;
    procedure ShowTools(AExpanded: Boolean);
    procedure RevealSelection;
    procedure Disconnect;
    property Connected: Boolean read GetConnected;
    property Diagram: INyxDiagram read GetDiagram;
    property Revision: Integer read GetRevision;
    property Selection: TNyxDiagramVertexRef read GetSelection;
    property Canvas: INyxCanvasWorkspaceView read GetCanvas;
    property HasPendingProperties: Boolean read GetHasPendingProperties;
    property ToolsExpanded: Boolean read GetToolsExpanded;
    property LastError: TNyxText read GetLastError;
  end;

function NyxDiagramEditorOptions: TNyxDiagramEditorOptions;
{ Exact IDs admit at most 64 UTF-8 bytes on both targets, reserving room for
  stable descendant/provider names. Validate before partial construction. }
function NyxDiagramEditorPart(const AEditorID: TNyxText;
  APart: TNyxDiagramEditorPart): TNyxControlRef;
function NewNyxDiagramEditor(const AID: TNyxText;
  const ADiagram: INyxDiagram): INyxDiagramEditor;

implementation

function NyxDiagramEditorOptions: TNyxDiagramEditorOptions;
begin
  Result := Default(TNyxDiagramEditorOptions);
  Result.FCamera := NyxCanvasCamera(800, 600, NyxCanvasPoint(0, 0));
  Result.FGrid := NyxCanvasGrid(20, ncgsNone, ncsmFree);
  Result.FNudge := 10;
end;

function TNyxDiagramEditorOptions.WithCamera(const AValue: TNyxCanvasCamera): TNyxDiagramEditorOptions;
begin

  if not AValue.Defined then
  begin
    raise ENyxDiagram.Create('Diagram editor camera is absent');
  end;
  Result := Self;
  Result.FCamera := AValue;
end;

function TNyxDiagramEditorOptions.WithGrid(const AValue: TNyxCanvasGrid): TNyxDiagramEditorOptions;
begin

  if not AValue.Defined then
  begin
    raise ENyxDiagram.Create('Diagram editor grid is absent');
  end;
  Result := Self;
  Result.FGrid := AValue;
end;

function TNyxDiagramEditorOptions.Nudge(AWorldDistance: Double): TNyxDiagramEditorOptions;
begin

  if IsNan(AWorldDistance) or IsInfinite(AWorldDistance) or
    (AWorldDistance <= 0) or (AWorldDistance > 1000000) then
  begin
    raise ENyxDiagram.Create('Diagram nudge requires finite 0..1000000 world units, excluding zero');
  end;
  Result := Self;
  Result.FNudge := AWorldDistance;
end;

function TNyxDiagramEditorOptions.ToolsExpanded(AValue: Boolean): TNyxDiagramEditorOptions;
begin
  Result := Self;
  Result.FToolsExpanded := AValue;
end;

function NyxDiagramEditorPart(const AEditorID: TNyxText;
  APart: TNyxDiagramEditorPart): TNyxControlRef;
const
  CNames: array[TNyxDiagramEditorPart] of TNyxText = (
    'toolbar', 'canvas-host', 'tools', 'selection', 'previous', 'next',
    'zoom-out', 'zoom-in', 'fit', 'actual-size', 'toggle-tools',
    'left', 'up', 'right', 'down', 'x', 'y', 'width', 'height',
    'caption', 'apply', 'restore', 'status');
begin

  if (AEditorID = '') or (NyxUTF8ByteCount(AEditorID) > 64) or
    (Ord(APart) < Ord(Low(TNyxDiagramEditorPart))) or
    (Ord(APart) > Ord(High(TNyxDiagramEditorPart))) then
  begin
    raise ENyxDiagram.Create('Diagram editor requires a short exact ID and known part');
  end;
  Result := NyxControl(AEditorID + '-diagram-' + CNames[APart]);
end;

function ButtonCaption(APart: TNyxDiagramEditorPart): TNyxText;
begin
  case APart of
    ndepPrevious: Result := 'Previous';
    ndepNext: Result := 'Next';
    ndepZoomOut: Result := 'Zoom -';
    ndepZoomIn: Result := 'Zoom +';
    ndepFit: Result := 'Fit';
    ndepActualSize: Result := '100%';
    ndepToggleTools: Result := 'Properties';
    ndepLeft: Result := 'Left';
    ndepUp: Result := 'Up';
    ndepRight: Result := 'Right';
    ndepDown: Result := 'Down';
    ndepApply: Result := 'Apply';
    ndepRestore: Result := 'Revert';
  else
    raise ENyxDiagram.Create('Unknown diagram editor button');
  end;
end;

constructor TNyxDiagramEditor.Create(const AID: TNyxText; const ADiagram: INyxDiagram);
var
  LPart: TNyxDiagramEditorPart;
  LToolbar: INyxRow;
  LTools: INyxCard;
  LFields: INyxRow;
  LActions: INyxRow;
  LHost: INyxPanel;
  LInput: INyxInput;
begin

  if ADiagram = nil then
  begin
    raise ENyxDiagram.Create('Diagram editor requires its initial graph');
  end;
  for LPart := Low(TNyxDiagramEditorPart) to High(TNyxDiagramEditorPart) do
  begin
    NyxDiagramEditorPart(AID, LPart);
  end;
  FInitial := NyxDiagramFromData(ADiagram.ToData);
  FOptions := NyxDiagramEditorOptions;
  inherited Create(AID);
  Node.Configure.Padding(0).Gap(0).HeightSizing(nsFill).MinimumHeight(240);
  LToolbar := NewNyxRow(NyxDiagramEditorPart(AID, ndepToolbar).ID);
  LToolbar.Configure.Padding(8).Gap(6).Wrap(nfwWrap).Done;
  for LPart := ndepPrevious to ndepToggleTools do
  begin
    LToolbar.Add(NewNyxButton(NyxDiagramEditorPart(AID, LPart).ID)
      .WithText(ButtonCaption(LPart)));
  end;
  Node.Add(LToolbar);
  Node.Add(NewNyxButton(NyxDiagramEditorPart(AID, ndepSelection).ID)
    .WithText('No shape selected'));
  LTools := NewNyxCard(NyxDiagramEditorPart(AID, ndepTools).ID);
  LTools.Configure.Padding(10).Gap(8).Visible(False).Done;
  LFields := NewNyxRow(AID + '-diagram-fields');
  LFields.Configure.Gap(8).Wrap(nfwWrap).Done;
  for LPart := ndepX to ndepHeight do
  begin
    LInput := NewNyxInput(NyxDiagramEditorPart(AID, LPart).ID);
    case LPart of
      ndepX: LInput.Text := 'X';
      ndepY: LInput.Text := 'Y';
      ndepWidth: LInput.Text := 'Width';
      ndepHeight: LInput.Text := 'Height';
    else
      raise ENyxDiagram.Create('Unknown geometry field');
    end;
    LInput.Configure.Width(100).Done;
    LFields.Add(LInput);
  end;
  LTools.Add(LFields);
  LInput := NewNyxInput(NyxDiagramEditorPart(AID, ndepCaption).ID);
  LInput.Text := 'Caption';
  LTools.Add(LInput);
  LActions := NewNyxRow(AID + '-diagram-geometry-actions');
  LActions.Configure.Gap(6).Wrap(nfwWrap).Done;
  for LPart := ndepLeft to ndepDown do
  begin
    LActions.Add(NewNyxButton(NyxDiagramEditorPart(AID, LPart).ID)
      .WithText(ButtonCaption(LPart)));
  end;
  LActions.Add(NewNyxButton(NyxDiagramEditorPart(AID, ndepApply).ID).WithText('Apply'));
  LActions.Add(NewNyxButton(NyxDiagramEditorPart(AID, ndepRestore).ID).WithText('Revert'));
  LTools.Add(LActions);
  Node.Add(LTools);
  LHost := NewNyxPanel(NyxDiagramEditorPart(AID, ndepCanvasHost).ID);
  LHost.Configure.Padding(0).Gap(0).Flex(1).HeightSizing(nsFill).MinimumHeight(120).Done;
  Node.Add(LHost);
  Node.Add(NewNyxLabel(NyxDiagramEditorPart(AID, ndepStatus).ID).WithText('Ready'));
end;

function TNyxDiagramEditor.Captioned(const AText: TNyxText): INyxDiagramEditor;
begin
  Node.Configure.Text(AText);
  Result := Self as INyxDiagramEditor;
end;

function TNyxDiagramEditor.GetInitialDiagram: INyxDiagram;
begin
  Result := FInitial;
end;

function TNyxDiagramEditor.GetOptions: TNyxDiagramEditorOptions;
begin
  Result := FOptions;
end;

function TNyxDiagramEditor.WithOptions(const AOptions: TNyxDiagramEditorOptions): INyxDiagramEditor;
var
  LOptions: TNyxDiagramEditorOptions;
begin
  LOptions := NyxDiagramEditorOptions.WithCamera(AOptions.Camera).WithGrid(AOptions.Grid)
    .Nudge(AOptions.NudgeDistance).ToolsExpanded(AOptions.InitiallyExpanded);
  FOptions := LOptions;
  Result := Self as INyxDiagramEditor;
end;

function NewNyxDiagramEditor(const AID: TNyxText;
  const ADiagram: INyxDiagram): INyxDiagramEditor;
begin
  Result := TNyxDiagramEditor.Create(AID, ADiagram);
end;

end.
