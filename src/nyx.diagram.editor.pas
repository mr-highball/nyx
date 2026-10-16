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
  nyx.canvas.geometry, nyx.canvas.workspace, nyx.diagram.declarations, nyx.diagram.adoption,
  nyx.diagram.manipulation, nyx.diagram.content, nyx.canvas.types, nyx.images;

type
  { Explicit content execution policy. Design preview renders authored controls
    without constructing project callbacks/graphs. Runtime requires application
    context for executable content. Editor chrome stays active in either mode;
    no application authority is serialized in a diagram or control descriptor. }
  TNyxDiagramContentMode = (ndcmRuntime, ndcmDesignPreview);

  { Stable public composition parts; callers use typed references rather than
    depending on private descendant IDs. Mounted faces remain adapter-owned. }
  TNyxDiagramEditorPart = (ndepToolbar, ndepCanvasHost, ndepTools, ndepSelection,
    ndepPrevious, ndepNext, ndepZoomOut, ndepZoomIn, ndepFit, ndepActualSize,
    ndepToggleTools, ndepLeft, ndepUp, ndepRight, ndepDown, ndepX, ndepY,
    ndepWidth, ndepHeight, ndepCaption, ndepApply, ndepRestore, ndepStatus,
    ndepSave, ndepReloadSaved, ndepConfirmReload, ndepClose,
    ndepToggleDraw, ndepDrawTools, ndepAddRectangle, ndepAddEllipse, ndepAddDiamond,
    ndepStartOrthogonal, ndepStartStraight, ndepFinishConnection, ndepCancelConnection,
    ndepConnectionPrompt, ndepRemoveVertex, ndepIncidentEdges, ndepPreviousEdge,
    ndepNextEdge, ndepStraightEdge, ndepOrthogonalEdge, ndepRemoveEdge,
    ndepReconnectStart, ndepReconnectFinish,
    ndepToggleContent, ndepContentTools, ndepContentScroll, ndepContentKind,
    ndepContentReference, ndepContentPadding, ndepContentHeader, ndepContentFit,
    ndepContentLocalized, ndepContentLocale, ndepContentFallback, ndepContentHelp,
    ndepApplyContent, ndepRestoreContent,
    ndepTogglePorts, ndepPortTools, ndepPortChoice, ndepPortName, ndepPortX,
    ndepPortY, ndepPortDirection, ndepPortCaption, ndepApplyPort, ndepRestorePort,
    ndepRemovePort, ndepPortIncidentEdges, ndepStartPortConnection, ndepFinishPortConnection,
    ndepCancelPortConnection);

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
    Unbound views grant no project/source/history, file or executable-graph
    authority. Explicit save hosts delegate publication to their document owner.
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
    function GetSelectedEdge: TNyxDiagramEdgeRef;
    function GetConnectionStart: TNyxDiagramVertexRef;
    function GetDrawToolsExpanded: Boolean;
    { Preview is immutable presentation data, separate from accepted Diagram.
      Save/model commands refuse an active capture; cancellation restores paint
      without altering model revision, history or unfinished property fields. }
    function GetDisplayDiagram: INyxDiagram;
    function GetManipulating: Boolean;
    function GetReconnectingEdge: TNyxDiagramEdgeRef;
    procedure CancelManipulation;
    function GetCanvas: INyxCanvasWorkspaceView;
    function GetHasPendingProperties: Boolean;
    function GetToolsExpanded: Boolean;
    function GetLastError: TNyxText;
    { Saving is explicitly bound to a copied definition baseline and revocable
      owning host. Applied snapshots publish asynchronously; acknowledgement
      advances only that baseline and never replaces newer local model/input. }
    procedure SavingTo(const ATarget: TNyxDiagramSaveTarget; const AHost: INyxDiagramSaveHost);
    procedure Save;
    { Explicit local replacement from current accepted intent. Dirty model or
      unfinished properties require deliberate discard; no source/history edits. }
    procedure ReloadSaved(ADiscardLocal: Boolean = False);
    function GetSavePending: Boolean;
    function GetHasUnsavedChanges: Boolean;
    procedure SelectVertex(const AReference: TNyxDiagramVertexRef;
      ADiscardProperties: Boolean = False);
    procedure SelectAt(const AWorldPoint: TNyxCanvasPoint;
      ADiscardProperties: Boolean = False);
    { Edge selection is presentation only, exclusive of vertex selection.
      Unfinished geometry is retained unless discard is explicitly requested. }
    procedure SelectEdge(const AReference: TNyxDiagramEdgeRef;
      ADiscardProperties: Boolean = False);
    { Typed local graph changes share complete prepared publication. Add refuses
      duplicate vertex names; SetEdge deliberately creates or replaces one edge.
      Connected removal requires its explicit policy. Pending fields refuse. }
    procedure AddVertex(const AVertex: TNyxDiagramVertex; AExpectedRevision: Integer);
    procedure SetEdge(const AEdge: TNyxDiagramEdge; AExpectedRevision: Integer);
    procedure RemoveVertex(const AReference: TNyxDiagramVertexRef;
      ARemoval: TNyxDiagramRemoval; AExpectedRevision: Integer);
    procedure RemoveEdge(const AReference: TNyxDiagramEdgeRef; AExpectedRevision: Integer);
    { Two-step connection intent owns only a source reference and typed routing.
      Selection/camera changes retain it; any applied graph replacement cancels
      it. Completion uses current geometry and a fresh unique edge identity. }
    procedure BeginConnection(const AStart: TNyxDiagramVertexRef; ARouting: TNyxDiagramRouting);
    procedure CompleteConnection(const AFinish: TNyxDiagramVertexRef; AExpectedRevision: Integer);
    procedure CancelConnection;
    { Existing-edge reconnection preserves identity/opposite end/style/arrows.
      Begin uses the same pointer/button destination contract as new connections.
      The direct typed operation also accepts a free endpoint. }
    procedure BeginReconnect(const AEdge: TNyxDiagramEdgeRef; AEnd: TNyxDiagramConnectionEnd);
    procedure ReconnectEdge(const AEdge: TNyxDiagramEdgeRef; AEnd: TNyxDiagramConnectionEnd;
      const AEndpoint: TNyxDiagramEndpoint; AExpectedRevision: Integer);
    { Draw and Properties are optional alternate panels; hiding retains input. }
    procedure ShowDrawTools(AExpanded: Boolean);
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
    property SelectedEdge: TNyxDiagramEdgeRef read GetSelectedEdge;
    property ConnectionStart: TNyxDiagramVertexRef read GetConnectionStart;
    property DrawToolsExpanded: Boolean read GetDrawToolsExpanded;
    property DisplayDiagram: INyxDiagram read GetDisplayDiagram;
    property Manipulating: Boolean read GetManipulating;
    property ReconnectingEdge: TNyxDiagramEdgeRef read GetReconnectingEdge;
    property Canvas: INyxCanvasWorkspaceView read GetCanvas;
    property HasPendingProperties: Boolean read GetHasPendingProperties;
    property ToolsExpanded: Boolean read GetToolsExpanded;
    property LastError: TNyxText read GetLastError;
    property SavePending: Boolean read GetSavePending;
    property HasUnsavedChanges: Boolean read GetHasUnsavedChanges;
  end;

  { Optional coherent source/graph replacement for an open editor. The supplied
    document is copied; unchanged content retains its actual controls, while
    affected reusable/image content stages before a complete publication. Dirty
    properties require explicit discard. Runtime stores and resource brokers
    retain their independent application ownership; incompatible bindings refuse.
    No project source/history or compiler authority is granted. }
  INyxDiagramSourceView = interface(IInterface)
    ['{D09F0508-7F25-43F6-A015-101600000303}']
    procedure ReplaceSource(ADocument: TNyxDocument; const ADiagram: INyxDiagram;
      AExpectedRevision: Integer; ADiscardProperties: Boolean = False);
  end;

type
  { Optional UI-owned physical-port authoring. Every change uses the same prepared
    graph/scene publisher and exact local revision. Ports grant no executable
    route authority. Unfinished fields refuse direct mutations; Apply properties
    deliberately submits the complete field group. Hiding retains input.
    Explicit incident removal retires only edges attached to that exact port. }
  INyxDiagramPortsView = interface(IInterface)
    ['{D09F0508-7F25-43F6-A015-101600000401}']
    function GetPortToolsExpanded: Boolean;
    function GetConnectionStartPort: TNyxDiagramPortRef;
    procedure ShowPortTools(AExpanded: Boolean);
    procedure SetPort(const AVertex: TNyxDiagramVertexRef;
      const APort: TNyxDiagramPort; AExpectedRevision: Integer);
    procedure RemovePort(const AVertex: TNyxDiagramVertexRef;
      const APort: TNyxDiagramPortRef; APolicy: TNyxDiagramRemoval; AExpectedRevision: Integer);
    procedure BeginPortConnection(const AVertex: TNyxDiagramVertexRef;
      const APort: TNyxDiagramPortRef; ARouting: TNyxDiagramRouting);
    procedure CompletePortConnection(const AVertex: TNyxDiagramVertexRef;
      const APort: TNyxDiagramPortRef; AExpectedRevision: Integer);
    property PortToolsExpanded: Boolean read GetPortToolsExpanded;
    property ConnectionStartPort: TNyxDiagramPortRef read GetConnectionStartPort;
  end;

  { Optional UI-owned content capability. Typed item/control references expose
    ordinary canvas inspection without borrowing target controls or retaining a
    document. Missing vertex names refuse; no-content vertices return absence.
    SetContent joins the same revision/property/preview guards as geometry edits. }
  INyxDiagramContentView = interface(IInterface)
    ['{59398DA1-4B38-4657-9E16-5C7850220004}']
    function ContentItem(const AVertex: TNyxDiagramVertexRef): TNyxCanvasItemRef;
    function ContentControl(const AVertex: TNyxDiagramVertexRef): TNyxControlRef;
    procedure SetContent(const AVertex: TNyxDiagramVertexRef;
      const AContent: TNyxDiagramContent; AExpectedRevision: Integer);
    { Alternate presentation only; hiding this panel retains unfinished fields.
      Apply/Revert share one geometry/content proposal with Properties. }
    procedure ShowContentTools(AExpanded: Boolean);
    function GetContentToolsExpanded: Boolean;
    property ContentToolsExpanded: Boolean read GetContentToolsExpanded;
  end;

  { UI-owned managed floating presentation. Hide preserves the exact mounted
    model, selection and unfinished inputs; Disconnect explicitly retires it.
    The adapter owns the modal and view, never a document or source controller. }
  INyxDiagramEditorWindow = interface(IInterface)
    ['{08CB4FF5-BDD8-439C-AD7F-54CCB1016004}']
    function GetView: INyxDiagramEditorView;
    function GetOpen: Boolean;
    procedure Show;
    procedure Hide;
    procedure Disconnect;
    property View: INyxDiagramEditorView read GetView;
    property IsOpen: Boolean read GetOpen;
  end;

{ English starter for an actual newly authored workflow. Vertices, style and
  connectivity use the same public model; callers can replace any of its parts. }
function NewNyxStarterDiagram: INyxDiagram;
function NyxDiagramEditorOptions: TNyxDiagramEditorOptions;
{ Stable English choice captions at the editor's UI boundary. Authoring uses
  enums/records; captions are never persisted as behavioral property values. }
function NyxDiagramContentCaption(AKind: TNyxDiagramContentKind): TNyxText;
function NyxDiagramImageFitCaption(AFit: TNyxImageFit): TNyxText;
{ Exact IDs admit at most 64 UTF-8 bytes on both targets, reserving room for
  stable descendant/provider names. Validate before partial construction. }
function NyxDiagramEditorPart(const AEditorID: TNyxText;
  APart: TNyxDiagramEditorPart): TNyxControlRef;
{ Presentation-only labels for closed direction choices and exact port references.
  New-port and existing-reference captions occupy separate presentation forms. }
function NyxDiagramPortDirectionCaption(AValue: TNyxDiagramAttachment): TNyxText;
function NyxDiagramPortChoiceCaption(const AReference: TNyxDiagramPortRef): TNyxText;
function NewNyxDiagramEditor(const AID: TNyxText;
  const ADiagram: INyxDiagram): INyxDiagramEditor; overload;
{ Reopen exact saved intent into an independent runtime descriptor. The document
  is borrowed only for lookup; missing names/nil owners refuse before construction.
  Editing and explicit export never mutate the original document declaration. }
function NewNyxDiagramEditor(const AID: TNyxText; ADocument: TNyxDocument;
  const AReference: TNyxDiagramRef): INyxDiagramEditor; overload;

implementation

function NyxDiagramContentCaption(AKind: TNyxDiagramContentKind): TNyxText;
const
  CNames: array[TNyxDiagramContentKind] of TNyxText =
    ('No content', 'Image resource', 'Reusable component');
begin
  Result := CNames[AKind];
end;

function NyxDiagramImageFitCaption(AFit: TNyxImageFit): TNyxText;
const
  CNames: array[TNyxImageFit] of TNyxText =
    ('Contain', 'Cover', 'Stretch', 'Natural size', 'Shrink to fit');
begin
  Result := CNames[AFit];
end;

function NewNyxStarterDiagram: INyxDiagram;
begin
  Result := NewNyxDiagram
    .WithVertex(NyxDiagramVertex(NyxDiagramVertexRef('start'), ndsRectangle,
      NyxCanvasBounds(0, 0, 140, 80)).Captioned('Start'))
    .WithVertex(NyxDiagramVertex(NyxDiagramVertexRef('review'), ndsDiamond,
      NyxCanvasBounds(240, -10, 160, 100)).Captioned('Ready?'))
    .WithVertex(NyxDiagramVertex(NyxDiagramVertexRef('finish'), ndsEllipse,
      NyxCanvasBounds(500, 0, 140, 80)).Captioned('Publish'))
    .WithEdge(NyxDiagramEdge(NyxDiagramEdgeRef('review'),
      NyxDiagramAttached(NyxDiagramVertexRef('start'), ndaRight),
      NyxDiagramAttached(NyxDiagramVertexRef('review'), ndaLeft)).Routed(ndrOrthogonal))
    .WithEdge(NyxDiagramEdge(NyxDiagramEdgeRef('publish'),
      NyxDiagramAttached(NyxDiagramVertexRef('review'), ndaRight),
      NyxDiagramAttached(NyxDiagramVertexRef('finish'), ndaLeft)).Routed(ndrOrthogonal));
end;

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
    'caption', 'apply', 'restore', 'status', 'save', 'reload-saved', 'confirm-reload', 'close',
    'toggle-draw', 'draw-tools', 'add-rectangle', 'add-ellipse', 'add-diamond',
    'start-orthogonal', 'start-straight', 'finish-connection', 'cancel-connection',
    'connection-prompt', 'remove-vertex', 'incident-edges', 'previous-edge',
    'next-edge', 'straight-edge', 'orthogonal-edge', 'remove-edge',
    'reconnect-start', 'reconnect-finish',
    'toggle-content', 'content-tools', 'content-scroll', 'content-kind',
    'content-reference', 'content-padding', 'content-header', 'content-fit',
    'content-localized', 'content-locale', 'content-fallback', 'content-help',
    'apply-content', 'restore-content',
    'toggle-ports', 'port-tools', 'port-choice', 'port-name', 'port-x', 'port-y',
    'port-direction', 'port-caption', 'apply-port', 'restore-port',
    'remove-port', 'port-incident-edges', 'start-port-connection', 'finish-port-connection',
    'cancel-port-connection');
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
    ndepSave: Result := 'Save diagram';
    ndepReloadSaved: Result := 'Reload saved';
  else
    raise ENyxDiagram.Create('Unknown diagram editor button');
  end;
end;

function NyxDiagramPortDirectionCaption(AValue: TNyxDiagramAttachment): TNyxText;
const
  CNames: array[TNyxDiagramAttachment] of TNyxText =
    ('Automatic', 'Left', 'Top', 'Right', 'Bottom');
begin

  if (Ord(AValue) < Ord(Low(TNyxDiagramAttachment))) or
    (Ord(AValue) > Ord(High(TNyxDiagramAttachment))) then
  begin
    raise ENyxDiagram.Create('Choose a known port exit direction');
  end;
  Result := CNames[AValue];
end;

function NyxDiagramPortChoiceCaption(const AReference: TNyxDiagramPortRef): TNyxText;
begin

  if AReference.Defined then
  begin
    Result := TNyxText('Edit: ') + AReference.Name;
  end
  else
  begin
    Result := 'Create a port';
  end;
end;

{ Public Nyx primitives compose the optional port workspace. Its controls keep
  their own descriptor references; no callback or mounted view is captured. }
function NewDiagramPortTools(const AID: TNyxText): INyxCard;
var
  LScroll: INyxScroll;
  LRow: INyxRow;
  LChoice: INyxSelect;
  LChoices: TNyxStrings;
  LDirection: TNyxDiagramAttachment;
begin
  Result := NewNyxCard(NyxDiagramEditorPart(AID, ndepPortTools).ID);
  Result.Configure.Padding(10).Gap(8).Visible(False).Done;
  LScroll := NewNyxScroll(AID + '-diagram-port-scroll');
  LScroll.Configure.Height(210).MinimumHeight(90).Gap(8).Done;
  LScroll.Add(NewNyxSelect(NyxDiagramEditorPart(AID, ndepPortChoice).ID)
    .WithText('Connection point'));
  LScroll.Add(NewNyxInput(NyxDiagramEditorPart(AID, ndepPortName).ID)
    .WithText('Stable port name'));
  LRow := NewNyxRow(AID + '-diagram-port-position');
  LRow.Configure.Gap(8).Wrap(nfwWrap).Done;
  LRow.Add(NewNyxInput(NyxDiagramEditorPart(AID, ndepPortX).ID)
    .Configure.Text('Width ratio').Width(125).Done);
  LRow.Add(NewNyxInput(NyxDiagramEditorPart(AID, ndepPortY).ID)
    .Configure.Text('Height ratio').Width(125).Done);
  LScroll.Add(LRow);
  LChoice := NewNyxSelect(NyxDiagramEditorPart(AID, ndepPortDirection).ID)
    .WithText('Connector exit');
  LChoices := TNyxStrings.Create;
  try
    for LDirection := Low(TNyxDiagramAttachment) to High(TNyxDiagramAttachment) do
    begin
      LChoices.Add(NyxDiagramPortDirectionCaption(LDirection));
    end;
    LChoice.Items := LChoices.Text;
  finally
    LChoices.Free;
  end;
  LScroll.Add(LChoice);
  LScroll.Add(NewNyxInput(NyxDiagramEditorPart(AID, ndepPortCaption).ID)
    .WithText('Description'));
  LScroll.Add(NewNyxLabel(AID + '-diagram-port-help').WithText(
    'Ratios follow the shape as it resizes: 0 is left/top, 1 is right/bottom. A different name creates another port.'));
  Result.Add(LScroll);
  LRow := NewNyxRow(AID + '-diagram-port-actions');
  LRow.Configure.Gap(6).Wrap(nfwWrap).Done;
  LRow.Add(NewNyxButton(NyxDiagramEditorPart(AID, ndepApplyPort).ID).WithText('Apply all properties'));
  LRow.Add(NewNyxButton(NyxDiagramEditorPart(AID, ndepRestorePort).ID).WithText('Revert all properties'));
  LRow.Add(NewNyxButton(NyxDiagramEditorPart(AID, ndepRemovePort).ID).WithText('Remove port'));
  Result.Add(LRow);
  Result.Add(NewNyxCheckbox(NyxDiagramEditorPart(AID, ndepPortIncidentEdges).ID)
    .WithText('Also remove lines attached to this port'));
  LRow := NewNyxRow(AID + '-diagram-port-connect');
  LRow.Configure.Gap(6).Wrap(nfwWrap).Done;
  LRow.Add(NewNyxButton(NyxDiagramEditorPart(AID, ndepStartPortConnection).ID)
    .WithText('Start from port'));
  LRow.Add(NewNyxButton(NyxDiagramEditorPart(AID, ndepFinishPortConnection).ID)
    .WithText('Connect to port'));
  LRow.Add(NewNyxButton(NyxDiagramEditorPart(AID, ndepCancelPortConnection).ID)
    .WithText('Cancel connection'));
  Result.Add(LRow);
end;

constructor TNyxDiagramEditor.Create(const AID: TNyxText; const ADiagram: INyxDiagram);
var
  LPart: TNyxDiagramEditorPart;
  LToolbar: INyxRow;
  LTools: INyxCard;
  LDraw: INyxCard;
  LFields: INyxRow;
  LActions: INyxRow;
  LHost: INyxPanel;
  LInput: INyxInput;
  LContent: INyxCard;
  LScroll: INyxScroll;
  LChoice: INyxSelect;
  LKind: TNyxDiagramContentKind;
  LFit: TNyxImageFit;
  LChoices: TNyxStrings;
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
  LToolbar.Add(NewNyxButton(NyxDiagramEditorPart(AID, ndepToggleDraw).ID).WithText('Draw'));
  LToolbar.Add(NewNyxButton(NyxDiagramEditorPart(AID, ndepToggleContent).ID).WithText('Content'));
  LToolbar.Add(NewNyxButton(NyxDiagramEditorPart(AID, ndepTogglePorts).ID).WithText('Ports'));
  LToolbar.Add(NewNyxButton(NyxDiagramEditorPart(AID, ndepSave).ID)
    .Configure.Text(ButtonCaption(ndepSave)).Visible(False).Done);
  LToolbar.Add(NewNyxButton(NyxDiagramEditorPart(AID, ndepReloadSaved).ID)
    .Configure.Text(ButtonCaption(ndepReloadSaved)).Visible(False).Done);
  LToolbar.Add(NewNyxCheckbox(NyxDiagramEditorPart(AID, ndepConfirmReload).ID)
    .Configure.Text('Discard local changes when reloading').Width(300)
    .Value(False).Visible(False).Done);
  { The floating composition reveals this ordinary Nyx button and supplies its
    revocable callback. Embedded editors have no implicit dismissal authority. }
  LToolbar.Add(NewNyxButton(NyxDiagramEditorPart(AID, ndepClose).ID)
    .Configure.Text('Close').Visible(False).Done);
  Node.Add(NewNyxButton(NyxDiagramEditorPart(AID, ndepSelection).ID)
    .WithText('No shape selected'));
  LDraw := NewNyxCard(NyxDiagramEditorPart(AID, ndepDrawTools).ID);
  LDraw.Configure.Padding(10).Gap(6).Visible(False).Done;
  LActions := NewNyxRow(AID + '-diagram-shapes');
  LActions.Configure.Gap(6).Wrap(nfwWrap).Done;
  LActions.Add(NewNyxButton(NyxDiagramEditorPart(AID, ndepAddRectangle).ID).WithText('+ Rectangle'));
  LActions.Add(NewNyxButton(NyxDiagramEditorPart(AID, ndepAddEllipse).ID).WithText('+ Ellipse'));
  LActions.Add(NewNyxButton(NyxDiagramEditorPart(AID, ndepAddDiamond).ID).WithText('+ Decision'));
  LDraw.Add(LActions);
  LDraw.Add(NewNyxLabel(NyxDiagramEditorPart(AID, ndepConnectionPrompt).ID)
    .WithText('Choose a shape, start a connection, then choose its destination.'));
  LActions := NewNyxRow(AID + '-diagram-connect');
  LActions.Configure.Gap(6).Wrap(nfwWrap).Done;
  LActions.Add(NewNyxButton(NyxDiagramEditorPart(AID, ndepStartOrthogonal).ID).WithText('Start orthogonal'));
  LActions.Add(NewNyxButton(NyxDiagramEditorPart(AID, ndepStartStraight).ID).WithText('Start straight'));
  LActions.Add(NewNyxButton(NyxDiagramEditorPart(AID, ndepFinishConnection).ID).WithText('Connect to selected'));
  LActions.Add(NewNyxButton(NyxDiagramEditorPart(AID, ndepCancelConnection).ID).WithText('Cancel connection'));
  LDraw.Add(LActions);
  LActions := NewNyxRow(AID + '-diagram-edge-actions');
  LActions.Configure.Gap(6).Wrap(nfwWrap).Done;
  LActions.Add(NewNyxButton(NyxDiagramEditorPart(AID, ndepPreviousEdge).ID).WithText('Previous line'));
  LActions.Add(NewNyxButton(NyxDiagramEditorPart(AID, ndepNextEdge).ID).WithText('Next line'));
  LActions.Add(NewNyxButton(NyxDiagramEditorPart(AID, ndepStraightEdge).ID).WithText('Straight'));
  LActions.Add(NewNyxButton(NyxDiagramEditorPart(AID, ndepOrthogonalEdge).ID).WithText('Orthogonal'));
  LActions.Add(NewNyxButton(NyxDiagramEditorPart(AID, ndepRemoveEdge).ID).WithText('Remove line'));
  LActions.Add(NewNyxButton(NyxDiagramEditorPart(AID, ndepReconnectStart).ID).WithText('Reconnect start'));
  LActions.Add(NewNyxButton(NyxDiagramEditorPart(AID, ndepReconnectFinish).ID).WithText('Reconnect end'));
  LDraw.Add(LActions);
  LActions := NewNyxRow(AID + '-diagram-remove-shape');
  LActions.Configure.Gap(6).Wrap(nfwWrap).Done;
  LActions.Add(NewNyxButton(NyxDiagramEditorPart(AID, ndepRemoveVertex).ID).WithText('Remove shape'));
  LActions.Add(NewNyxCheckbox(NyxDiagramEditorPart(AID, ndepIncidentEdges).ID)
    .Configure.Text('Also remove its connected lines').Width(260).Value(False).Done);
  LDraw.Add(LActions);
  Node.Add(LDraw);
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
  LContent := NewNyxCard(NyxDiagramEditorPart(AID, ndepContentTools).ID);
  LContent.Configure.Padding(8).Gap(6).Visible(False).Done;
  LScroll := NewNyxScroll(NyxDiagramEditorPart(AID, ndepContentScroll).ID);
  LScroll.Configure.Height(220).MinimumHeight(0).Gap(8).Done;
  LChoices := TNyxStrings.Create;
  try
    for LKind := Low(TNyxDiagramContentKind) to High(TNyxDiagramContentKind) do
    begin
      LChoices.Add(NyxDiagramContentCaption(LKind));
    end;
    LChoice := NewNyxSelect(NyxDiagramEditorPart(AID, ndepContentKind).ID).WithText('Content type');
    LChoice.Items := LChoices.Text;
    LScroll.Add(LChoice);
    LScroll.Add(NewNyxSelect(NyxDiagramEditorPart(AID, ndepContentReference).ID)
      .WithText('Resource or reusable'));
    LFields := NewNyxRow(AID + '-diagram-content-metrics');
    LFields.Configure.Gap(8).Wrap(nfwWrap).Done;
    LFields.Add(NewNyxInput(NyxDiagramEditorPart(AID, ndepContentPadding).ID)
      .Configure.Text('Inset').Width(100).Done);
    LFields.Add(NewNyxInput(NyxDiagramEditorPart(AID, ndepContentHeader).ID)
      .Configure.Text('Caption space').Width(140).Done);
    LChoices.Clear;
    for LFit := Low(TNyxImageFit) to High(TNyxImageFit) do
    begin
      LChoices.Add(NyxDiagramImageFitCaption(LFit));
    end;
    LChoice := NewNyxSelect(NyxDiagramEditorPart(AID, ndepContentFit).ID).WithText('Image fit');
    LChoice.Items := LChoices.Text;
    LChoice.Configure.Width(150).Done;
    LFields.Add(LChoice);
    LScroll.Add(LFields);
    LScroll.Add(NewNyxCheckbox(NyxDiagramEditorPart(AID, ndepContentLocalized).ID)
      .WithText('Pin image locale and fallback'));
    LFields := NewNyxRow(AID + '-diagram-content-locales');
    LFields.Configure.Gap(8).Wrap(nfwWrap).Done;
    LFields.Add(NewNyxInput(NyxDiagramEditorPart(AID, ndepContentLocale).ID)
      .Configure.Text('Locale (blank = default)').Width(200).Done);
    LFields.Add(NewNyxInput(NyxDiagramEditorPart(AID, ndepContentFallback).ID)
      .Configure.Text('Fallback locale').Width(170).Done);
    LScroll.Add(LFields);
    LScroll.Add(NewNyxLabel(NyxDiagramEditorPart(AID, ndepContentHelp).ID)
      .WithText('Choose a resource or reusable from this editor''s source context.'));
  finally
    LChoices.Free;
  end;
  LContent.Add(LScroll);
  LActions := NewNyxRow(AID + '-diagram-content-actions');
  LActions.Configure.Gap(6).Wrap(nfwWrap).Done;
  LActions.Add(NewNyxButton(NyxDiagramEditorPart(AID, ndepApplyContent).ID).WithText('Apply all properties'));
  LActions.Add(NewNyxButton(NyxDiagramEditorPart(AID, ndepRestoreContent).ID).WithText('Revert all properties'));
  LContent.Add(LActions);
  Node.Add(LContent);
  Node.Add(NewDiagramPortTools(AID));
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

function NewNyxDiagramEditor(const AID: TNyxText; ADocument: TNyxDocument;
  const AReference: TNyxDiagramRef): INyxDiagramEditor;
begin

  if ADocument = nil then
  begin
    raise ENyxDiagram.Create('Reopening a diagram requires its document');
  end;
  Result := NewNyxDiagramEditor(AID, ADocument.Diagrams.Definition(AReference));
end;

end.
