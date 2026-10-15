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
unit nyx.canvas.workspace;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, Math, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.events,
  nyx.canvas.geometry, nyx.canvas.scene, nyx.canvas.types, nyx.canvas.declarations,
  nyx.interaction.policy, nyx.view.behavior, nyx.scheduler, nyx.publication.types,
  nyx.flow.declarations, nyx.flow.editing;

const
  { Preserve the original workspace unit's enum spellings for existing callers.
    The defining type now lives in the document-independent declaration layer. }
  nccsReflow = nyx.canvas.types.nccsReflow;
  nccsLogicalSize = nyx.canvas.types.nccsLogicalSize;
  { Live native child windows have finite extents. Both adapters refuse visible
    allocations above this shared budget before changing camera or controls.
    Off-screen world coordinates remain unbounded within geometry limits. }
  MaximumNyxCanvasControlExtent = 32768;

type
  { Adapter-owned copied constraints. This tiny lease retains no tree/view;
    its owner updates it on the UI thread, and revokes it before retirement. }
  INyxCanvasInteractionGate = interface(IInterface)
    ['{A48BF41B-18F8-43A8-AD8B-51138DFFAB70}']
    procedure Update(AEnabled, AReadOnly: Boolean);
    function EnabledPolicy: INyxRuntimeEnabledPolicy;
    function ReadOnlyPolicy: INyxRuntimeReadOnlyPolicy;
  end;
  TNyxCanvasControlScaling = nyx.canvas.types.TNyxCanvasControlScaling;
  TNyxCanvasCommand = (nccZoomOut, nccZoomIn, nccFit, nccHome);
  TNyxCanvasWorkspacePart = (ncwpToolbar, ncwpZoomOut, ncwpZoomIn,
    ncwpFit, ncwpHome, ncwpSurface);

  { Admitted logical-pixel placement. Invisible frames carry zero allocation;
    callers retain mounted controls but do not convert huge off-screen positions
    into widget integers. Reflow scales live allocation, never an affine image or
    font; LogicalSize keeps allocation fixed while position follows the camera. }
  TNyxCanvasControlFrame = record
    Visible: Boolean;
    Left: Integer;
    Top: Integer;
    Width: Integer;
    Height: Integer;
  end;

  { Public Nyx composition, with ordinary Panel/Row/Button parts. Scene ownership
    is independent of this chrome's tree: it holds actual injected control roots.
    Configuration is copied by a mounted view; views own independent cameras.
    This explicit contract serves runtime-owned scenes. For saved application
    content, INyxCanvas.WithScene uses ordinary renderer attachment and Studio
    authoring. A plain clone of this runtime workspace's Node copies chrome only;
    its separately retained Scene must not be inferred from that clone. }
  INyxCanvasWorkspace = interface(INyxPanel)
    ['{737A7921-4621-4C6F-8C01-0CA000000010}']
    function GetScene: INyxCanvasScene;
    function GetCamera: TNyxCanvasCamera;
    function GetGrid: TNyxCanvasGrid;
    function GetScaling: TNyxCanvasControlScaling;
    function WithCamera(const ACamera: TNyxCanvasCamera): INyxCanvasWorkspace;
    function WithGrid(const AGrid: TNyxCanvasGrid): INyxCanvasWorkspace;
    function WithScaling(AScaling: TNyxCanvasControlScaling): INyxCanvasWorkspace;
    property Scene: INyxCanvasScene read GetScene;
    property Camera: TNyxCanvasCamera read GetCamera;
    property Grid: TNyxCanvasGrid read GetGrid;
    property Scaling: TNyxCanvasControlScaling read GetScaling;
  end;

  { Portable mounted behavior. A view retains its workspace/scene, never a
    document/editor through that tree. Target mount functions borrow their host
    and optional application store; they copy document context. Disconnect must
    precede disposing either borrowed dependency. Camera/grid updates are runtime
    presentation, not design/history changes. Controls are retained across
    camera moves; Refresh reconciles explicit scene revisions, not property edits.
    Each mounted item uses ordinary Nyx renderer events and live bindings.
    All live view methods and final connected-view release belong to the UI
    thread. Workers use immutable scene/camera copies and scheduler UI work;
    direct view mutation refuses before touching target controls. }
  INyxCanvasWorkspaceView = interface(IInterface)
    ['{737A7921-4621-4C6F-8C01-0CA000000011}']
    function GetConnected: Boolean;
    function GetCamera: TNyxCanvasCamera;
    function GetMountedCount: Integer;
    procedure SetCamera(const ACamera: TNyxCanvasCamera);
    procedure PanBy(AX, AY: Double);
    procedure ZoomAt(AZoom: Double; const AAnchor: TNyxCanvasPoint);
    procedure Execute(ACommand: TNyxCanvasCommand);
    procedure Refresh;
    { Restrict nested editing/commands without rewriting authored or bound
      properties. Ordinary hosts update these Boolean gates during Sync. }
    procedure Constrain(AEnabled, AReadOnly: Boolean);
    function EventsFor(const AItem: TNyxCanvasItemRef): INyxEvents;
    procedure Disconnect;
    property Connected: Boolean read GetConnected;
    property Camera: TNyxCanvasCamera read GetCamera;
    property MountedCount: Integer read GetMountedCount;
  end;

  { Optional atomic placement/camera publication, independent of renderer types.
    Revision counts successful presentations, including ordinary resize/refresh;
    a captured worker result cannot overwrite intervening view changes. Apply
    stages all target allocations/mounts, then commits the prepared scene and
    camera without external callouts between them. Failed staging changes neither.
    False means cancellation, retirement or an unallocated viewport. Physical
    control/observer errors after publication do not roll back an accepted model.
    The caller owns cancellation; the view retains no program or application. }
  INyxCanvasLayoutView = interface(IInterface)
    ['{E291C5CB-6B87-4E85-A8E6-000000000203}']
    function GetPresentationRevision: Integer;
    function ApplyLayout(const APlan: INyxCanvasLayoutPlan;
      const ACamera: TNyxCanvasCamera; AExpectedPresentation: Integer;
      const ACancellation: INyxCancellationScope = nil): Boolean;
    property PresentationRevision: Integer read GetPresentationRevision;
  end;

  { Cancellation during coordinated admission is a refusal before any model
    installs. Adapters translate this category to False, retaining all owners. }
  ENyxCanvasPublicationCancelled = class(ENyxCanvasScene);

  { Optional UI-only multi-owner layout admission, preserving the original view
    interfaces. All physical mounts stage before Companion.Validate, allowing
    lazy state preparations to reserve only after new bindings subscribe.
    The scene plan must expose INyxPreparedPublication/Readiness. Its scene,
    camera/view frame and Companion install as one PublishNyxGroup before any
    observer runs. Validation/cancellation/retirement refuses the whole group;
    notification errors describe already committed models. Every adopted
    participant retires once. The caller still retires an unadopted companion. }
  INyxCanvasCoordinatedLayoutView = interface(IInterface)
    ['{E291C5CB-6B87-4E85-A8E6-000000000207}']
    function ApplyCoordinatedLayout(const APlan: INyxCanvasLayoutPlan;
      const ACamera: TNyxCanvasCamera; AExpectedPresentation: Integer;
      const ACompanion: INyxPreparedPublication;
      const ACancellation: INyxCancellationScope = nil): Boolean;
  end;

  { Optional UI-owned graph editing for this runtime workspace. The detached
    declarations can be exported by the application; authored documents/history
    are never changed here. Whole candidate admission and exact scene/flow guards
    precede one coordinated replacement of mounted item routes and future-mount
    source. Static callbacks, live editors and nested workspaces retain independent
    ownership. All graph work in an affected fragment retires together; unrelated
    fragments retain their running work. A graph edit grants no host capabilities. }
  INyxCanvasFlowEditing = interface(IInterface)
    ['{B070D203-9195-41EB-8503-ED7993C4AE21}']
    function RuntimeFlows: INyxFlowDeclarations;
    function GetFlowRevision: Integer;
    procedure ApplyFlowChanges(const AChanges: array of TNyxFlowChange;
      AExpectedScene, AExpectedFlows: Integer);
    { This revision belongs to graph meaning, independently of camera/resize.
      Ordinary panel layout must not make a typed wire selection stale. }
    property FlowRevision: Integer read GetFlowRevision;
  end;

  { Optional behavior inspection preserves the original workspace-view ABI.
    Returned attachments are independent weak capabilities; retirement closes
    them before item renderers die. A plain/design-only host may return nil. }
  INyxCanvasRuntimeView = interface(IInterface)
    ['{D1411014-6120-4C84-A708-000000000017}']
    function BehaviorFor(const AItem: TNyxCanvasItemRef): INyxViewBehavior;
    { Runtime scene owns independently materialized content; geometry/content
      edits require Refresh and never mutate the saved design/history. }
    function GetScene: INyxCanvasScene;
    property Scene: INyxCanvasScene read GetScene;
  end;

  TNyxCanvasWorkspace = class(TNyxPanel, INyxCanvasWorkspace)
  private
    FScene: INyxCanvasScene;
    FCamera: TNyxCanvasCamera;
    FGrid: TNyxCanvasGrid;
    FScaling: TNyxCanvasControlScaling;
  public
    constructor Create(const AID: TNyxText; const AScene: INyxCanvasScene); reintroduce;
    function GetScene: INyxCanvasScene;
    function GetCamera: TNyxCanvasCamera;
    function GetGrid: TNyxCanvasGrid;
    function GetScaling: TNyxCanvasControlScaling;
    function WithCamera(const ACamera: TNyxCanvasCamera): INyxCanvasWorkspace;
    function WithGrid(const AGrid: TNyxCanvasGrid): INyxCanvasWorkspace;
    function WithScaling(AScaling: TNyxCanvasControlScaling): INyxCanvasWorkspace;
  end;

function NewNyxCanvasWorkspace(const AID: TNyxText;
  const AScene: INyxCanvasScene): INyxCanvasWorkspace; overload;
function NewNyxCanvasInteractionGate: INyxCanvasInteractionGate;
{ Materialize independently owned template copies from a saved declaration.
  Saved roots/settings remain untouched and both adapters share this path. }
function NewNyxCanvasWorkspace(const AID: TNyxText; ADocument: TNyxDocument;
  const AReference: TNyxCanvasRef): INyxCanvasWorkspace; overload;
{ Named chrome identities are exact references; closed parts/commands never
  use executable action strings. Long root names refuse before partial creation. }
function NyxCanvasWorkspacePart(const AWorkspace: TNyxText;
  APart: TNyxCanvasWorkspacePart): TNyxControlRef;
function TryNyxCanvasCommand(const AWorkspace, AControl: TNyxText;
  out ACommand: TNyxCanvasCommand): Boolean;
function NyxCanvasControlFrame(const ACamera: TNyxCanvasCamera;
  const ABounds: TNyxCanvasBounds; AScaling: TNyxCanvasControlScaling;
  AVisible: Boolean = True): TNyxCanvasControlFrame;
{ Fit visible content under the live allocation policy. LogicalSize centers
  actual fixed-size extents and reduces position scale when needed. Caller zoom
  limits or controls larger than the viewport may prevent complete fitting.
  Empty scenes retain the camera; oversized world unions refuse explicitly. }
function FitNyxCanvasScene(const ACamera: TNyxCanvasCamera;
  const ASnapshot: INyxCanvasSnapshot;
  AScaling: TNyxCanvasControlScaling = nccsReflow): TNyxCanvasCamera;
{ Own an admitted source context for a detached scene fragment. Existing roots
  already in the source document resolve to their copied counterpart; new roots
  clone into a private reusable partition. Whole-document validation preserves
  identity/state/resource constraints. Result owns AOwnedRoot; caller frees only
  Result, never that borrowed root. Failure changes no supplied ownership. }
function NewNyxCanvasRenderContext(ADocument: TNyxDocument; ARoot: TNyxNode;
  out AOwnedRoot: TNyxNode): TNyxDocument;

implementation

uses
  nyx.schema, nyx.root.types;

type
  TNyxCanvasInteractionGate = class(TInterfacedObject, INyxCanvasInteractionGate,
    INyxRuntimeEnabledPolicy, INyxRuntimeReadOnlyPolicy)
  private
    FEnabled: Boolean;
    FReadOnly: Boolean;
  public
    constructor Create;
    procedure Update(AEnabled, AReadOnly: Boolean);
    function GetAllowed: Boolean;
    function GetReadOnly: Boolean;
    function EnabledPolicy: INyxRuntimeEnabledPolicy;
    function ReadOnlyPolicy: INyxRuntimeReadOnlyPolicy;
  end;

constructor TNyxCanvasInteractionGate.Create;
begin
  inherited Create;
  FEnabled := True;
end;

procedure TNyxCanvasInteractionGate.Update(AEnabled, AReadOnly: Boolean);
begin
  FEnabled := AEnabled;
  FReadOnly := AReadOnly;
end;

function TNyxCanvasInteractionGate.GetAllowed: Boolean;
begin
  Result := FEnabled;
end;

function TNyxCanvasInteractionGate.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

function TNyxCanvasInteractionGate.EnabledPolicy: INyxRuntimeEnabledPolicy;
begin
  Result := Self;
end;

function TNyxCanvasInteractionGate.ReadOnlyPolicy: INyxRuntimeReadOnlyPolicy;
begin
  Result := Self;
end;

function NewNyxCanvasInteractionGate: INyxCanvasInteractionGate;
begin
  Result := TNyxCanvasInteractionGate.Create;
end;

const
  CPartNames: array[TNyxCanvasWorkspacePart] of TNyxText =
    ('toolbar', 'zoom-out', 'zoom-in', 'fit', 'home', 'surface');
  CCommandParts: array[TNyxCanvasCommand] of TNyxCanvasWorkspacePart =
    (ncwpZoomOut, ncwpZoomIn, ncwpFit, ncwpHome);
  CCommandCaptions: array[TNyxCanvasCommand] of TNyxText =
    ('Zoom out', 'Zoom in', 'Fit content', 'Home');

function NyxCanvasWorkspacePart(const AWorkspace: TNyxText;
  APart: TNyxCanvasWorkspacePart): TNyxControlRef;
begin

  if (Ord(APart) < Ord(Low(TNyxCanvasWorkspacePart))) or
    (Ord(APart) > Ord(High(TNyxCanvasWorkspacePart))) then
  begin
    raise ENyxCanvasScene.Create('Unknown workspace part');
  end;
  NyxCanvasItem(AWorkspace);
  Result := NyxControl(NyxCanvasItem(AWorkspace + '-canvas-' + CPartNames[APart]).Name);
end;

function TryNyxCanvasCommand(const AWorkspace, AControl: TNyxText;
  out ACommand: TNyxCanvasCommand): Boolean;
var
  LCommand: TNyxCanvasCommand;
begin
  Result := False;
  ACommand := nccHome;
  for LCommand := Low(TNyxCanvasCommand) to High(TNyxCanvasCommand) do
  begin

    if NyxCanvasWorkspacePart(AWorkspace, CCommandParts[LCommand]).ID = AControl then
    begin
      ACommand := LCommand;
      Exit(True);
    end;
  end;
end;

constructor TNyxCanvasWorkspace.Create(const AID: TNyxText; const AScene: INyxCanvasScene);
var
  LToolbar: INyxRow;
  LSurface: INyxPanel;
  LButton: INyxButton;
  LPart: TNyxCanvasWorkspacePart;
  LCommand: TNyxCanvasCommand;
begin

  if AScene = nil then
  begin
    raise ENyxCanvasScene.Create('A workspace requires an owned scene');
  end;
  for LPart := Low(TNyxCanvasWorkspacePart) to High(TNyxCanvasWorkspacePart) do
  begin
    NyxCanvasWorkspacePart(AID, LPart);
  end;
  inherited Create(AID);
  FScene := AScene;
  FCamera := NyxCanvasCamera(800, 600, NyxCanvasPoint(0, 0));
  FGrid := NyxCanvasGrid;
  FScaling := nccsReflow;
  Node.Configure.Gap(0).Padding(0).HeightSizing(nsFill).MinimumHeight(240);
  LToolbar := NewNyxRow(NyxCanvasWorkspacePart(AID, ncwpToolbar).ID);
  LToolbar.Configure.Padding(8).Gap(8).Wrap(nfwWrap);
  for LCommand := Low(TNyxCanvasCommand) to High(TNyxCanvasCommand) do
  begin
    LButton := NewNyxButton(NyxCanvasWorkspacePart(AID, CCommandParts[LCommand]).ID);
    LButton.Text := CCommandCaptions[LCommand];
    LToolbar.Add(LButton);
  end;
  { Do not publish a Self interface before a pas2js constructor finishes.
    Adopt through the descriptor; its child token retains the real parts. }
  Node.Add(LToolbar);
  LSurface := NewNyxPanel(NyxCanvasWorkspacePart(AID, ncwpSurface).ID);
  LSurface.Configure.Padding(0).Gap(0).HeightSizing(nsFill).MinimumHeight(200);
  Node.Add(LSurface);
end;

function NewNyxCanvasWorkspace(const AID: TNyxText;
  const AScene: INyxCanvasScene): INyxCanvasWorkspace;
begin
  Result := TNyxCanvasWorkspace.Create(AID, AScene);
end;

function NewNyxCanvasWorkspace(const AID: TNyxText; ADocument: TNyxDocument;
  const AReference: TNyxCanvasRef): INyxCanvasWorkspace;
var
  LDefinition: INyxCanvasDefinition;
  LScene: INyxCanvasScene;
  LLayer: TNyxCanvasLayerInfo;
  LItem: TNyxCanvasDeclarationItem;
  LNode: TNyxNode;
  LControl: INyxControl;
  LIndex: Integer;
begin

  if ADocument = nil then
  begin
    raise ENyxCanvasScene.Create('Saved workspace construction requires its source document');
  end;
  ADocument.Validate;
  LDefinition := ADocument.Canvases.Definition(AReference);
  LScene := NewNyxCanvasScene;
  for LIndex := 0 to LDefinition.LayerCount - 1 do
  begin
    LLayer := LDefinition.Layers[LIndex];
    LScene.AddLayer(LLayer.Layer).ConfigureLayer(LLayer.Layer,
      LLayer.IsVisible, LLayer.IsPickable);
  end;
  for LIndex := 0 to LDefinition.ItemCount - 1 do
  begin
    LItem := LDefinition.Items[LIndex];
    LNode := ADocument.FindRoot(LItem.Content).Clone;
    try
      LControl := RetainNyxControl(LNode);
    except
      LNode.Free;
      raise;
    end;
    LNode.ReleaseOwnership;
    LScene.AddControl(LItem.Item, LItem.Placement, LControl);
  end;
  Result := NewNyxCanvasWorkspace(AID, LScene).WithCamera(LDefinition.Camera)
    .WithGrid(LDefinition.Grid).WithScaling(LDefinition.Scaling);
end;

function TNyxCanvasWorkspace.GetScene: INyxCanvasScene;
begin
  Result := FScene;
end;

function TNyxCanvasWorkspace.GetCamera: TNyxCanvasCamera;
begin
  Result := FCamera;
end;

function TNyxCanvasWorkspace.GetGrid: TNyxCanvasGrid;
begin
  Result := FGrid;
end;

function TNyxCanvasWorkspace.GetScaling: TNyxCanvasControlScaling;
begin
  Result := FScaling;
end;

function TNyxCanvasWorkspace.WithCamera(const ACamera: TNyxCanvasCamera): INyxCanvasWorkspace;
begin

  if not ACamera.Defined then
  begin
    raise ENyxCanvasScene.Create('Workspace camera is absent');
  end;
  FCamera := ACamera;
  Result := Self;
end;

function TNyxCanvasWorkspace.WithGrid(const AGrid: TNyxCanvasGrid): INyxCanvasWorkspace;
begin

  if not AGrid.Defined then
  begin
    raise ENyxCanvasScene.Create('Workspace grid is absent');
  end;
  FGrid := AGrid;
  Result := Self;
end;

function TNyxCanvasWorkspace.WithScaling(
  AScaling: TNyxCanvasControlScaling): INyxCanvasWorkspace;
begin

  if not (AScaling in [nccsReflow, nccsLogicalSize]) then
  begin
    raise ENyxCanvasScene.Create('Unknown embedded control scaling');
  end;
  FScaling := AScaling;
  Result := Self;
end;

function NyxCanvasControlFrame(const ACamera: TNyxCanvasCamera;
  const ABounds: TNyxCanvasBounds; AScaling: TNyxCanvasControlScaling;
  AVisible: Boolean): TNyxCanvasControlFrame;
var
  LLeft: Double;
  LTop: Double;
  LWidth: Double;
  LHeight: Double;
begin

  if not ACamera.Defined or not ABounds.Defined or
    not (AScaling in [nccsReflow, nccsLogicalSize]) then
  begin
    raise ENyxCanvasScene.Create('Live allocation requires camera, bounds and typed scaling');
  end;
  Result := Default(TNyxCanvasControlFrame);

  if not AVisible then
  begin
    Exit;
  end;
  { Use finite edge arithmetic before narrowing. A far off-screen item may
    project past the point-record limit; culling must not materialize it first. }
  LLeft := (ABounds.Left - ACamera.WorldCenter.X) * ACamera.Zoom +
    ACamera.ViewportWidth / 2;
  LTop := (ABounds.Top - ACamera.WorldCenter.Y) * ACamera.Zoom +
    ACamera.ViewportHeight / 2;
  LWidth := ABounds.Width;
  LHeight := ABounds.Height;

  if AScaling = nccsReflow then
  begin
    LWidth := LWidth * ACamera.Zoom;
    LHeight := LHeight * ACamera.Zoom;
  end;

  if (LWidth <= 0) or (LHeight <= 0) or
    (LLeft + LWidth <= 0) or (LTop + LHeight <= 0) or
    (LLeft >= ACamera.ViewportWidth) or (LTop >= ACamera.ViewportHeight) then
  begin
    Exit;
  end;

  if (LWidth > MaximumNyxCanvasControlExtent) or
    (LHeight > MaximumNyxCanvasControlExtent) then
  begin
    raise ENyxCanvasScene.Create('Visible live control allocation exceeds the shared extent budget');
  end;
  Result.Visible := True;
  Result.Left := Floor(LLeft + 0.5);
  Result.Top := Floor(LTop + 0.5);
  Result.Width := Max(1, Floor(LWidth + 0.5));
  Result.Height := Max(1, Floor(LHeight + 0.5));
end;

function FitNyxCanvasScene(const ACamera: TNyxCanvasCamera;
  const ASnapshot: INyxCanvasSnapshot;
  AScaling: TNyxCanvasControlScaling): TNyxCanvasCamera;
var
  LIndex: Integer;
  LInfo: TNyxCanvasItemInfo;
  LLeft: Double;
  LTop: Double;
  LRight: Double;
  LBottom: Double;
  LFound: Boolean;
  LMaximumWidth: Double;
  LMaximumHeight: Double;
  LScreenWidth: Double;
  LScreenHeight: Double;
  LAvailableWidth: Double;
  LAvailableHeight: Double;
  LLowZoom: Double;
  LHighZoom: Double;
  LZoom: Double;
  LIteration: Integer;

  { Measure relative to admitted world minima, avoiding unnecessarily large
    absolute world * zoom intermediates. Every fixed-size extent is monotone
    in this positive position scale, so a bounded bisection can fit a group. }
  procedure Measure(AZoom: Double);
  var
    LItemIndex: Integer;
    LBounds: TNyxCanvasBounds;
  begin
    LScreenWidth := 0;
    LScreenHeight := 0;
    for LItemIndex := 0 to ASnapshot.Count - 1 do
    begin

      if not ASnapshot.Items[LItemIndex].Placement.IsVisible then
      begin
        Continue;
      end;
      LBounds := ASnapshot.Items[LItemIndex].Placement.Bounds;
      LScreenWidth := Max(LScreenWidth, (LBounds.Left - LLeft) * AZoom + LBounds.Width);
      LScreenHeight := Max(LScreenHeight, (LBounds.Top - LTop) * AZoom + LBounds.Height);
    end;
  end;
begin

  if not ACamera.Defined or (ASnapshot = nil) or
    not (AScaling in [nccsReflow, nccsLogicalSize]) then
  begin
    raise ENyxCanvasScene.Create('Fit requires a camera and scene snapshot');
  end;
  LFound := False;
  LLeft := 0;
  LTop := 0;
  LRight := 0;
  LBottom := 0;
  LMaximumWidth := 0;
  LMaximumHeight := 0;
  for LIndex := 0 to ASnapshot.Count - 1 do
  begin
    LInfo := ASnapshot.Items[LIndex];

    if not LInfo.Placement.IsVisible then
    begin
      Continue;
    end;

    if not LFound then
    begin
      LLeft := LInfo.Placement.Bounds.Left;
      LTop := LInfo.Placement.Bounds.Top;
      LRight := LLeft + LInfo.Placement.Bounds.Width;
      LBottom := LTop + LInfo.Placement.Bounds.Height;
      LFound := True;
    end
    else
    begin
      LLeft := Min(LLeft, LInfo.Placement.Bounds.Left);
      LTop := Min(LTop, LInfo.Placement.Bounds.Top);
      LRight := Max(LRight, LInfo.Placement.Bounds.Left + LInfo.Placement.Bounds.Width);
      LBottom := Max(LBottom, LInfo.Placement.Bounds.Top + LInfo.Placement.Bounds.Height);
    end;
    LMaximumWidth := Max(LMaximumWidth, LInfo.Placement.Bounds.Width);
    LMaximumHeight := Max(LMaximumHeight, LInfo.Placement.Bounds.Height);
  end;
  Result := ACamera;

  if LFound then
  begin
    Result := ACamera.Fit(NyxCanvasBounds(LLeft, LTop, LRight - LLeft, LBottom - LTop));

    if AScaling = nccsLogicalSize then
    begin
      { Reduce the customary inset when an unscaled control needs that space.
        This never changes its readable typography or fixed allocation policy. }
      LAvailableWidth := ACamera.ViewportWidth -
        2 * Min(24, Max(0, (ACamera.ViewportWidth - LMaximumWidth) / 2));
      LAvailableHeight := ACamera.ViewportHeight -
        2 * Min(24, Max(0, (ACamera.ViewportHeight - LMaximumHeight) / 2));
      LZoom := Result.Zoom;
      Measure(LZoom);

      if (LScreenWidth > LAvailableWidth) or (LScreenHeight > LAvailableHeight) then
      begin
        LLowZoom := ACamera.Limits.Minimum;
        LHighZoom := LZoom;
        Measure(LLowZoom);

        if (LScreenWidth <= LAvailableWidth) and (LScreenHeight <= LAvailableHeight) then
        begin
          for LIteration := 1 to 48 do
          begin
            LZoom := (LLowZoom + LHighZoom) / 2;
            Measure(LZoom);

            if (LScreenWidth <= LAvailableWidth) and (LScreenHeight <= LAvailableHeight) then
            begin
              LLowZoom := LZoom;
            end
            else
            begin
              LHighZoom := LZoom;
            end;
          end;
        end;
        LZoom := LLowZoom;
      end;
      Measure(LZoom);
      Result := NyxCanvasCamera(ACamera.ViewportWidth, ACamera.ViewportHeight,
        NyxCanvasPoint(LLeft + LScreenWidth / (2 * LZoom),
          LTop + LScreenHeight / (2 * LZoom)), LZoom, ACamera.Limits);
    end;
  end;
end;

function NewNyxCanvasRenderContext(ADocument: TNyxDocument; ARoot: TNyxNode;
  out AOwnedRoot: TNyxNode): TNyxDocument;
var
  LRoot: TNyxNode;
  LExisting: TNyxNode;
  LIndex: Integer;
  LPartition: TNyxRootKind;
begin
  AOwnedRoot := nil;

  if (ADocument = nil) or (ARoot = nil) or ARoot.IsRealized then
  begin
    raise ENyxCanvasScene.Create('Canvas rendering requires an authored root and source context');
  end;
  Result := ADocument.Clone;
  try

    if ADocument.Contains(ARoot) then
    begin
      AOwnedRoot := Result.Find(ARoot.ID);
    end
    else
    begin
      LPartition := nrReusable;
      LExisting := Result.Find(ARoot.ID);

      if LExisting <> nil then
      begin

        if LExisting.Parent <> nil then
        begin
          raise ENyxCanvasScene.Create('Canvas fragment root conflicts with a context descendant');
        end;
        LPartition := nrPage;
        for LIndex := 0 to Result.ComponentCount - 1 do
        begin

          if Result.Components[LIndex] = LExisting then
          begin
            LPartition := nrReusable;
            Break;
          end;
        end;
        Result.RemoveRoot(NyxRoot(LPartition, LExisting.ID));
      end;
      LRoot := ARoot.Clone;
      try
        if LPartition = nrPage then
        begin
          Result.AddPage(LRoot);
        end
        else
        begin
          Result.AddComponent(LRoot);
        end;
      except
        LRoot.Free;
        raise;
      end;
      AOwnedRoot := LRoot;
    end;
    ValidateNyxDocumentProperties(Result);
  except
    AOwnedRoot := nil;
    Result.Free;
    raise;
  end;
end;

end.
