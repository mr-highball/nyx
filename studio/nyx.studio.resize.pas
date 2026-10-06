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

unit nyx.studio.resize;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.types, nyx.model, nyx.events, nyx.designer.resize,
  nyx.studio.drag, nyx.studio.projects, nyx.studio.session, nyx.designer.guides;

const
  NyxStudioResizeWidthID = 'studio-resize-width';
  NyxStudioResizeHeightID = 'studio-resize-height';
  NyxStudioResizeBothID = 'studio-resize-both';
  NyxStudioResizeOwnerKey = 'studio.resize-owner';

type
  { Borrowed UI-thread geometry receiver. Read the allocated outer face through
    the public adapter contract, using exact design identity, never client size. }
  TNyxStudioResizeMeasure = function(const AControl: TNyxControlRef): TNyxResizeSize of object;
  TNyxStudioResizeStatus = procedure(const AMessage: TNyxText) of object;
  { Borrowed UI-thread sink for copied canvas presentation. Clear precedes
    commit/cancel/disconnection; it never grants a document mutation lease. }
  TNyxStudioResizePresentation = procedure(const APreview: TNyxResizePreview) of object;
  { Captures copied visible sibling geometry at start and checks it again at
    release. Nil preserves grid-only behavior for hosts without layout guides. }
  TNyxStudioResizeGuides = function(const AControl: TNyxControlRef): TNyxAlignmentContext of object;

  { Editor bridge for reusable Nyx grips. A start captures the exact accepted
    pair, mounted session/load, selection/view and creator generation. Preview
    publishes copied presentation only, never a per-pixel design edit. Release validates
    the unchanged lease and submits the existing isolated processor once.
    Roots/inherited part descriptors stay with their separate authoring tools. }
  TNyxStudioResize = class
  private
    FCapture: TNyxStudioDragCapture;
    FMeasure: TNyxStudioResizeMeasure;
    FStatus: TNyxStudioResizeStatus;
    FPresentation: TNyxStudioResizePresentation;
    FGuides: TNyxStudioResizeGuides;
    FGuideContext: TNyxAlignmentContext;
    FHandles: array[TNyxResizeAxis] of TNyxResizeHandle;
    FCanvasGrips: INyxCanvasResizeGrips;
    FEvents: INyxEvents;
    FRevision: Integer;
    FMount: TNyxStudioCommandContext;
    FOwner: TNyxControlRef;
    FContext: TNyxStudioCommandContext;
    FPair: TNyxProjectPair;
    FView: TNyxText;
    FSchemaRevision: Integer;
    FActive: Boolean;
    function BeginChange(AAxis: TNyxResizeAxis; out ASize: TNyxResizeSize;
      out APolicy: TNyxResizePolicy): Boolean;
    procedure Feedback(AAxis: TNyxResizeAxis; APhase: TNyxResizePhase;
      const ASize: TNyxResizeSize);
    function Live(const AContext: TNyxStudioDragContext): Boolean;
    procedure CanvasRetired;
  public
    constructor Create(ACapture: TNyxStudioDragCapture;
      AMeasure: TNyxStudioResizeMeasure; AStatus: TNyxStudioResizeStatus;
      APresentation: TNyxStudioResizePresentation = nil;
      AGuides: TNyxStudioResizeGuides = nil);
    destructor Destroy; override;
    { Shell descriptors are borrowed during connection. Same live mount/owner
      retains grips; replacement cancels the old preview before detaching. }
    procedure Connect(const AEvents: INyxEvents; AShell: TNyxNode;
      const AMount: TNyxStudioCommandContext);
    procedure Disconnect;
    { Managed public adornment for the same exact owner and paired operation.
      Canvas adapters retain it while mounting their independent input scopes. }
    property CanvasGrips: INyxCanvasResizeGrips read FCanvasGrips;
  end;

{ Caller owns the returned panel. It adopts three public specialized buttons;
  portable model metadata connects an exact owner without a renderer back edge. }
function BuildNyxStudioResizeTools(const AOwner: TNyxControlRef): TNyxNode;

implementation

uses
  SysUtils, nyx.controls, nyx.schema, nyx.studio.edits, nyx.composition, nyx.responsive;

const
  CGripIDs: array[TNyxResizeAxis] of TNyxText =
    (NyxStudioResizeWidthID, NyxStudioResizeHeightID, NyxStudioResizeBothID);

function BuildNyxStudioResizeTools(const AOwner: TNyxControlRef): TNyxNode;
var
  LRow: TNyxNode;
  LAxis: TNyxResizeAxis;
  LGrip: INyxButton;
begin
  Result := TNyxNode.Create(nkPanel, 'studio-resize-tools');
  try
    Result.Configure.Layout(nlColumn).Padding(0).Gap(6).Done;
    Result.Add(TNyxNode.Create(nkLabel, 'studio-resize-help')
      .Configure.Text('Resize grips · nearby size/alignment guides, then 8 px grid. Alt bypasses snapping; Escape cancels.').Done);
    LRow := TNyxNode.Create(nkRow, 'studio-resize-grips');
    Result.Add(LRow);
    LRow.Configure.Wrap(nfwWrap).Gap(6).Done;
    for LAxis := Low(TNyxResizeAxis) to High(TNyxResizeAxis) do
    begin
      LGrip := NewNyxResizeGrip(CGripIDs[LAxis], LAxis);
      LGrip.Node.SetProp(NyxStudioResizeOwnerKey, AOwner.ID);
      LRow.Add(LGrip.Node);
    end;
  except
    Result.Free;
    raise;
  end;
end;

constructor TNyxStudioResize.Create(ACapture: TNyxStudioDragCapture;
  AMeasure: TNyxStudioResizeMeasure; AStatus: TNyxStudioResizeStatus;
  APresentation: TNyxStudioResizePresentation; AGuides: TNyxStudioResizeGuides);
begin
  inherited Create;

  if not Assigned(ACapture) or not Assigned(AMeasure) or not Assigned(AStatus) then
  begin
    raise ENyxModel.Create('Studio resizing requires current mounts, geometry and status');
  end;
  FCapture := ACapture;
  FMeasure := AMeasure;
  FStatus := AStatus;
  FPresentation := APresentation;
  FGuides := AGuides;
end;

destructor TNyxStudioResize.Destroy;
begin
  Disconnect;
  FCapture := nil;
  FMeasure := nil;
  FStatus := nil;
  FPresentation := nil;
  FGuides := nil;
  inherited Destroy;
end;

procedure TNyxStudioResize.Disconnect;
var
  LAxis: TNyxResizeAxis;
begin
  { Prevent teardown feedback from publishing through retired controller views. }
  FActive := False;

  if FCanvasGrips <> nil then
  begin
    FCanvasGrips.Disconnect;
    FCanvasGrips := nil;
  end;

  if Assigned(FPresentation) then
  begin
    FPresentation(Default(TNyxResizePreview));
  end;
  for LAxis := Low(TNyxResizeAxis) to High(TNyxResizeAxis) do
  begin
    FreeAndNil(FHandles[LAxis]);
  end;
  FEvents := nil;
  FOwner := Default(TNyxControlRef);
  FPair := Default(TNyxProjectPair);
end;

procedure TNyxStudioResize.Connect(const AEvents: INyxEvents; AShell: TNyxNode;
  const AMount: TNyxStudioCommandContext);
var
  LContext: TNyxStudioDragContext;
  LGrip: TNyxNode;
  LOwner: TNyxText;
  LAxis: TNyxResizeAxis;
begin

  if (AShell = nil) or (AEvents = nil) then
  begin
    Disconnect;
    Exit;
  end;
  LContext := FCapture();
  LGrip := AShell.Find(NyxStudioResizeWidthID);
  LOwner := LContext.Session.SelectedID;

  if (LContext.Session.Selected = nil) or
    (LContext.Session.Selected.Parent = nil) or
    (LContext.Session.Selected.Kind = 'slot-override') then
  begin
    LOwner := '';
  end;

  if (LGrip <> nil) and (LGrip.Prop(NyxStudioResizeOwnerKey) <> LOwner) then
  begin
    raise ENyxModel.Create('Inspector resize tools belong to another selection');
  end;

  if (FEvents = AEvents) and (FRevision = AEvents.ViewRevision) and
    (FOwner.ID = LOwner) and LContext.Session.MatchesCommandContext(FMount) then
  begin
    Exit;
  end;
  Disconnect;

  if LOwner = '' then
  begin
    Exit;
  end;

  if not LContext.Session.MatchesCommandContext(AMount) then
  begin
    raise ENyxModel.Create('Resize grips belong to a retired editor shell');
  end;
  FMount := AMount;
  FOwner := NyxControl(LOwner);
  FEvents := AEvents;
  FRevision := AEvents.ViewRevision;
  try
    FCanvasGrips := NewNyxCanvasResizeGrips(FOwner, BeginChange, Feedback, CanvasRetired);
    for LAxis := Low(TNyxResizeAxis) to High(TNyxResizeAxis) do
    begin
      LGrip := AShell.Find(CGripIDs[LAxis]);

      if LGrip = nil then
      begin
        { Compact Design hides its Inspector. The independently owned canvas
          document still exposes all three grips for the same selected owner. }
        Continue;
      end;

      if LGrip.Prop(NyxStudioResizeOwnerKey) <> FOwner.ID then
      begin
        raise ENyxModel.Create('Resize grips must share one exact authored owner');
      end;
      FHandles[LAxis] := TNyxResizeHandle.Create(AEvents, NyxControl(LGrip.ID),
        LAxis, BeginChange, Feedback);
    end;
  except
    Disconnect;
    raise;
  end;
end;

function TNyxStudioResize.Live(const AContext: TNyxStudioDragContext): Boolean;
begin
  Result := FActive and AContext.Designing and not AContext.Commands.Busy and
    not AContext.Session.SourceDraftPending and
    AContext.Session.MatchesCommandContext(FContext) and
    AContext.Session.MatchesCommandContext(AContext.SourceMount) and
    AContext.Session.MatchesCommandContext(AContext.CanvasMount) and
    (AContext.Session.SelectedID = FOwner.ID) and
    (AContext.Session.ActiveViewID = FView) and (NyxSchemaRevision = FSchemaRevision);
end;

procedure TNyxStudioResize.CanvasRetired;
begin
  { Adaptor teardown already retires its paint. Revoke the shared lease without
    reentering shell/canvas painting from an input scope destructor. }
  FActive := False;
  FPair := Default(TNyxProjectPair);
end;

function TNyxStudioResize.BeginChange(AAxis: TNyxResizeAxis; out ASize: TNyxResizeSize;
  out APolicy: TNyxResizePolicy): Boolean;
var
  LContext: TNyxStudioDragContext;
  LProjection: TNyxNode;
  LRealized: TNyxNode;
  LAttribute: TNyxAttribute;
  LNode: TNyxNode;
  LIndex: Integer;
  LCondition: TNyxViewportCondition;
  LPlatform: TNyxPlatform;
  LScopedAttribute: TNyxAttribute;
begin
  Result := False;
  ASize := Default(TNyxResizeSize);
  APolicy := NyxResizePolicy;
  LContext := FCapture();

  if FActive or not LContext.Designing or LContext.Commands.Busy or
    LContext.Session.SourceDraftPending or
    not LContext.Session.MatchesCommandContext(FMount) or
    not LContext.Session.MatchesCommandContext(LContext.SourceMount) or
    not LContext.Session.MatchesCommandContext(LContext.CanvasMount) or
    (LContext.Session.SelectedID <> FOwner.ID) then
  begin
    Exit;
  end;
  LNode := LContext.Session.Selected;

  if (LNode = nil) or (LNode.Parent = nil) or (LNode.Kind = 'slot-override') then
  begin
    Exit;
  end;
  LRealized := RealizeNyxContext(LContext.Session.Document, LNode, LProjection);
  try
    { A baseline gesture cannot choose which conditional presentation the
      author intended to change. Refuse scoped dimensions and parent flow
      rather than publish a default that the current viewport silently masks. }
    for LIndex := 0 to LProjection.Props.Count - 1 do
    begin

      if LProjection.TryResponsiveKey(LProjection.Props.Names[LIndex], LCondition,
        LPlatform, LScopedAttribute) and (LScopedAttribute in
        [atWidth, atHeight, atWidthSizing, atHeightSizing, atFlex,
        atMinimumWidth, atMaximumWidth, atMinimumHeight, atMaximumHeight]) then
      begin
        FStatus('Use the responsive size fields for this control; its viewport sizing is explicitly overridden.');
        Exit;
      end;
    end;
    if LProjection.Parent = nil then
    begin
      Exit;
    end;
    for LIndex := 0 to LProjection.Parent.Props.Count - 1 do
    begin

      if LProjection.Parent.TryResponsiveKey(LProjection.Parent.Props.Names[LIndex], LCondition,
        LPlatform, LScopedAttribute) and (LScopedAttribute = atLayout) then
      begin
        FStatus('Use responsive size fields; the parent changes flow between viewport presentations.');
        Exit;
      end;
    end;
    { A portable resize must not silently lose to existing scoped dimensions or
      weights. Such controls keep their explicit platform Inspector fields.
      This bounded first gesture path does not guess which override to erase. }
    for LAttribute in [atWidth, atHeight, atWidthSizing, atHeightSizing, atFlex,
      atMinimumWidth, atMaximumWidth, atMinimumHeight, atMaximumHeight] do
    begin

      if (LProjection.Prop(NyxPlatformKey(npfBrowser, LAttribute)) <> '') or
        (LProjection.Prop(NyxPlatformKey(npfNativeLCL, LAttribute)) <> '') then
      begin
        FStatus('Use the platform size fields for this control; its target sizing is explicitly overridden.');
        Exit;
      end;
    end;

    if (NyxResizeReleasesWeight(LProjection.Parent, AAxis, npfAny) <>
      NyxResizeReleasesWeight(LProjection.Parent, AAxis, npfBrowser)) or
      (NyxResizeReleasesWeight(LProjection.Parent, AAxis, npfAny) <>
      NyxResizeReleasesWeight(LProjection.Parent, AAxis, npfNativeLCL)) then
    begin
      FStatus('Use target size fields or Resize Both; the parent has different target flow policies.');
      Exit;
    end;
    APolicy := APolicy.Bounds(NyxNodeSizeConstraints(LProjection));
    ASize := FMeasure(FOwner);
  finally
    LRealized.Free;
  end;
  FPair := LContext.Session.ProjectSnapshot;
  FGuideContext := Default(TNyxAlignmentContext);

  if Assigned(FGuides) then
  begin
    FGuideContext := FGuides(FOwner);
    APolicy := APolicy.Guides(FGuideContext);
  end;
  FContext := LContext.Session.CommandContext;
  FView := LContext.Session.ActiveViewID;
  FSchemaRevision := NyxSchemaRevision;
  FActive := True;
  Result := True;
end;

procedure TNyxStudioResize.Feedback(AAxis: TNyxResizeAxis; APhase: TNyxResizePhase;
  const ASize: TNyxResizeSize);
var
  LContext: TNyxStudioDragContext;
  LPair: TNyxProjectPair;
  LEdit: TNyxStudioDesignEdit;
  LMessage: TNyxText;
begin

  if not FActive then
  begin
    Exit;
  end;
  LContext := FCapture();

  if (APhase = nrpCancel) or not Live(LContext) then
  begin
    FActive := False;
    FPair := Default(TNyxProjectPair);

    if Assigned(FPresentation) then
    begin
      FPresentation(Default(TNyxResizePreview));
    end;
    FStatus('Resize canceled; accepted dimensions retained');
    Exit;
  end;

  if APhase = nrpPreview then
  begin
    LMessage := TNyxText('Resize preview · ') + FOwner.ID + TNyxText(' · ') +
      TNyxText(IntToStr(ASize.Width)) + TNyxText(' × ') +
      TNyxText(IntToStr(ASize.Height)) + TNyxText(' px');

    if ASize.WidthGuide.Kind <> ngkNone then
    begin
      LMessage := LMessage + TNyxText(' · ') + ASize.WidthGuide.Caption;
    end;

    if ASize.HeightGuide.Kind <> ngkNone then
    begin
      LMessage := LMessage + TNyxText(' · ') + ASize.HeightGuide.Caption;
    end;
    FStatus(LMessage);

    if Assigned(FPresentation) and Live(FCapture()) then
    begin
      { Status may refresh the containing shell/layout. Paint only after that
        update and a fresh lease check, so native child windows stay in front
        and a remounted browser body cannot discard the just-created strips. }
      FPresentation(NyxResizePreview(FOwner, ASize));
    end;
    Exit;
  end;
  { Full paired equality is checked once at commit, not on every pointer pixel.
    Existing preparation/publication independently checks its own captured pair. }
  LPair := LContext.Session.ProjectSnapshot;
  FActive := False;

  if Assigned(FPresentation) then
  begin
    FPresentation(Default(TNyxResizePreview));
  end;

  if Assigned(FGuides) and not FGuideContext.SameContext(FGuides(FOwner)) then
  begin
    FPair := Default(TNyxProjectPair);
    FStatus('Resize canceled; neighboring layout changed during the gesture');
    Exit;
  end;

  if (LPair.Design <> FPair.Design) or (LPair.Source <> FPair.Source) or
    LPair.Pending or (LPair.Draft <> FPair.Draft) or (LPair.DraftBase <> FPair.DraftBase) then
  begin
    FPair := Default(TNyxProjectPair);
    FStatus('Resize canceled; the accepted design or source changed during the gesture');
    Exit;
  end;
  FPair := Default(TNyxProjectPair);
  LEdit := LContext.Session.CaptureResize(NyxResizeControl(FOwner, AAxis, ASize), FContext);
  LContext.Commands.Edit(LEdit);
end;

end.
