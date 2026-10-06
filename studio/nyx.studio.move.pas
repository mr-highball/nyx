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
unit nyx.studio.move;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.types, nyx.model, nyx.events, nyx.designer.guides,
  nyx.designer.move, nyx.designer.resize, nyx.studio.drag, nyx.studio.resize,
  nyx.studio.projects, nyx.studio.session;

const
  NyxStudioMoveGripID = 'studio-position-grip';
  NyxStudioMoveOwnerKey = 'studio.position-owner';

type
  { Ordinary Studio bridge for the public move behavior. It borrows synchronous
    UI-thread receivers until Destroy, capturing an exact accepted pair, owner,
    view, creator epoch and mounted command contexts. Every pointer pixel paints
    a copied proposal only. Release admits one isolated paired operation after
    rechecking the entire captured lease and copied sibling geometry. }
  TNyxStudioMove = class
  private
    FCapture: TNyxStudioDragCapture;
    FGuides: TNyxStudioResizeGuides;
    FStatus: TNyxStudioResizeStatus;
    FPresentation: TNyxStudioResizePresentation;
    FPointerMap: TNyxMovePointerMap;
    FHandle: TNyxMoveHandle;
    FCanvasGrip: INyxCanvasMoveGrip;
    FEvents: INyxEvents;
    FRevision: Integer;
    FMount, FContext: TNyxStudioCommandContext;
    FOwner: TNyxControlRef;
    FPair: TNyxProjectPair;
    FView: TNyxText;
    FSchemaRevision: Integer;
    FGeometry: TNyxAlignmentContext;
    FActive: Boolean;
    function BeginChange(out APosition: TNyxMovePosition; out APolicy: TNyxMovePolicy): Boolean;
    procedure Feedback(APhase: TNyxMovePhase; const APosition: TNyxMovePosition);
    function Live(const AContext: TNyxStudioDragContext): Boolean;
    procedure CanvasRetired;
  public
    constructor Create(ACapture: TNyxStudioDragCapture; AGuides: TNyxStudioResizeGuides;
      AStatus: TNyxStudioResizeStatus; APresentation: TNyxStudioResizePresentation;
      APointerMap: TNyxMovePointerMap);
    destructor Destroy; override;
    { Borrow an ordinary shell for this call only. Exact retained scope/mount/
      owner keeps subscriptions; replacement cancels and retires old receivers. }
    procedure Connect(const AEvents: INyxEvents; AShell: TNyxNode;
      const AMount: TNyxStudioCommandContext);
    procedure Cancel;
    procedure Disconnect;
    property CanvasGrip: INyxCanvasMoveGrip read FCanvasGrip;
  end;

{ Caller owns the returned ordinary Nyx panel. Only a portable admitted absolute
  control offers this grip; refusal leaves the existing flow placement tools. }
function BuildNyxStudioMoveTools(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef): TNyxNode;

implementation

uses SysUtils, nyx.controls, nyx.schema, nyx.studio.edits;

function BuildNyxStudioMoveTools(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef): TNyxNode;
var
  LGrip: INyxButton;
begin
  Result := nil;
  try
    ValidateNyxPositionOwner(ADocument, AOwner);
  except
    on E: ENyxModel do
    begin
      Exit;
    end;
  end;
  Result := TNyxNode.Create(nkPanel, 'studio-position-tools');
  try
    Result.Configure.Layout(nlColumn).Gap(6).Padding(0).Done;
    Result.Add(NewNyxLabel('studio-position-help').WithText(
      'Move in this layout · edge/center guides, then 8 px grid. Alt bypasses snapping; Escape cancels.').Node);
    LGrip := NewNyxMoveGrip(NyxStudioMoveGripID);
    LGrip.Node.SetProp(NyxStudioMoveOwnerKey, AOwner.ID);
    Result.Add(LGrip.Node);
  except
    Result.Free;
    raise;
  end;
end;

constructor TNyxStudioMove.Create(ACapture: TNyxStudioDragCapture;
  AGuides: TNyxStudioResizeGuides; AStatus: TNyxStudioResizeStatus;
  APresentation: TNyxStudioResizePresentation; APointerMap: TNyxMovePointerMap);
begin
  inherited Create;

  if not Assigned(ACapture) or not Assigned(AGuides) or not Assigned(AStatus) or
    not Assigned(APresentation) or not Assigned(APointerMap) then
  begin
    raise ENyxModel.Create('Studio movement requires current mounts, geometry and borrowed receivers');
  end;
  FCapture := ACapture;
  FGuides := AGuides;
  FStatus := AStatus;
  FPresentation := APresentation;
  FPointerMap := APointerMap;
end;

destructor TNyxStudioMove.Destroy;
begin
  Disconnect;
  FCapture := nil;
  FGuides := nil;
  FStatus := nil;
  FPresentation := nil;
  FPointerMap := nil;
  inherited Destroy;
end;

procedure TNyxStudioMove.Cancel;
begin

  if FHandle <> nil then
  begin
    FHandle.Cancel;
  end;

  if FCanvasGrip <> nil then
  begin
    FCanvasGrip.Cancel;
  end;
end;

procedure TNyxStudioMove.Disconnect;
var
  LPainted: Boolean;
begin
  LPainted := FActive;
  FActive := False;
  FreeAndNil(FHandle);

  if FCanvasGrip <> nil then
  begin
    FCanvasGrip.Disconnect;
    FCanvasGrip := nil;
  end;

  if LPainted and Assigned(FPresentation) then
  begin
    FPresentation(Default(TNyxCanvasPreview));
  end;
  FEvents := nil;
  FOwner := Default(TNyxControlRef);
  FPair := Default(TNyxProjectPair);
end;

procedure TNyxStudioMove.Connect(const AEvents: INyxEvents; AShell: TNyxNode;
  const AMount: TNyxStudioCommandContext);
var
  LGrip: TNyxNode;
  LContext: TNyxStudioDragContext;
begin

  if (AEvents = nil) or (AShell = nil) then
  begin
    Disconnect;
    Exit;
  end;
  LGrip := AShell.Find(NyxStudioMoveGripID);
  LContext := FCapture();

  if not LContext.Designing or (LContext.Session.SelectedID = '') then
  begin
    Disconnect;
    Exit;
  end;

  if (LGrip <> nil) and (LGrip.Prop(NyxStudioMoveOwnerKey) <> LContext.Session.SelectedID) then
  begin
    raise ENyxModel.Create('Inspector move grip belongs to another selected owner');
  end;
  try
    ValidateNyxPositionOwner(LContext.Session.Document, NyxControl(LContext.Session.SelectedID));
  except
    on E: ENyxModel do
    begin
      Disconnect;
      Exit;
    end;
  end;

  if (FEvents = AEvents) and (FRevision = AEvents.ViewRevision) and
    (FOwner.ID = LContext.Session.SelectedID) and
    LContext.Session.MatchesCommandContext(FMount) then
  begin
    Exit;
  end;
  Disconnect;

  if not LContext.Session.MatchesCommandContext(AMount) then
  begin
    raise ENyxModel.Create('Move grip belongs to a retired shell');
  end;
  FMount := AMount;
  FOwner := NyxControl(LContext.Session.SelectedID);
  FEvents := AEvents;
  FRevision := AEvents.ViewRevision;
  FCanvasGrip := NewNyxCanvasMoveGrip(FOwner, BeginChange, Feedback, CanvasRetired);

  if LGrip <> nil then
  begin
    FHandle := TNyxMoveHandle.Create(AEvents, NyxControl(NyxStudioMoveGripID),
      BeginChange, Feedback, FPointerMap);
  end;
end;

procedure TNyxStudioMove.CanvasRetired;
begin
  FActive := False;
  FPair := Default(TNyxProjectPair);
end;

function TNyxStudioMove.Live(const AContext: TNyxStudioDragContext): Boolean;
begin
  Result := FActive and AContext.Designing and not AContext.Commands.Busy and
    not AContext.Session.SourceDraftPending and
    AContext.Session.MatchesCommandContext(FContext) and
    AContext.Session.MatchesCommandContext(AContext.SourceMount) and
    AContext.Session.MatchesCommandContext(AContext.CanvasMount) and
    (AContext.Session.SelectedID = FOwner.ID) and
    (AContext.Session.ActiveViewID = FView) and (NyxSchemaRevision = FSchemaRevision);
end;

function TNyxStudioMove.BeginChange(out APosition: TNyxMovePosition;
  out APolicy: TNyxMovePolicy): Boolean;
var
  LContext: TNyxStudioDragContext;
begin
  Result := False;
  APosition := Default(TNyxMovePosition);
  APolicy := NyxMovePolicy;
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
  try
    ValidateNyxPositionOwner(LContext.Session.Document, FOwner);
  except
    on E: ENyxModel do
    begin
      FStatus(TNyxText(E.Message));
      Exit;
    end;
  end;
  FGeometry := FGuides(FOwner);

  if not FGeometry.Defined or not FGeometry.PositionsEnabled then
  begin
    FStatus('Move refused; the mounted absolute geometry is unavailable');
    Exit;
  end;
  APosition := NyxMovePosition(Round(FGeometry.OwnerBox.Left), Round(FGeometry.OwnerBox.Top));
  APolicy := APolicy.Guides(FGeometry);
  FPair := LContext.Session.ProjectSnapshot;
  FContext := LContext.Session.CommandContext;
  FView := LContext.Session.ActiveViewID;
  FSchemaRevision := NyxSchemaRevision;
  FActive := True;
  Result := True;
end;

procedure TNyxStudioMove.Feedback(APhase: TNyxMovePhase; const APosition: TNyxMovePosition);
var
  LContext: TNyxStudioDragContext;
  LPair: TNyxProjectPair;
  LMessage: TNyxText;
  LPreview: TNyxCanvasPreview;
  LEdit: TNyxStudioDesignEdit;
begin

  if not FActive then
  begin
    Exit;
  end;
  LContext := FCapture();

  if (APhase = nmpCancel) or not Live(LContext) then
  begin
    FActive := False;
    FPair := Default(TNyxProjectPair);
    FPresentation(Default(TNyxCanvasPreview));
    FStatus('Move canceled; accepted position retained');
    Exit;
  end;

  if APhase = nmpPreview then
  begin
    LMessage := TNyxText('Move preview · ') + FOwner.ID + TNyxText(' · ') +
      TNyxText(IntToStr(APosition.Left)) + TNyxText(', ') + TNyxText(IntToStr(APosition.Top)) + TNyxText(' px');

    if APosition.HorizontalGuide.Kind <> ngkNone then
    begin
      LMessage := LMessage + TNyxText(' · ') + APosition.HorizontalGuide.Caption;
    end;

    if APosition.VerticalGuide.Kind <> ngkNone then
    begin
      LMessage := LMessage + TNyxText(' · ') + APosition.VerticalGuide.Caption;
    end;
    FStatus(LMessage);

    if Live(FCapture()) then
    begin
      LPreview := NyxResizePreview(FOwner, NyxResizeSize(Round(FGeometry.OwnerBox.Width),
        Round(FGeometry.OwnerBox.Height))).Translated(APosition.Left - FGeometry.OwnerBox.Left,
        APosition.Top - FGeometry.OwnerBox.Top).Aligned(APosition.HorizontalGuide, APosition.VerticalGuide);
      FPresentation(LPreview);
    end;
    Exit;
  end;
  LPair := LContext.Session.ProjectSnapshot;
  FActive := False;
  FPresentation(Default(TNyxCanvasPreview));

  if not FGeometry.SameContext(FGuides(FOwner)) or
    (LPair.Design <> FPair.Design) or (LPair.Source <> FPair.Source) or
    LPair.Pending or (LPair.Draft <> FPair.Draft) or (LPair.DraftBase <> FPair.DraftBase) then
  begin
    FPair := Default(TNyxProjectPair);
    FStatus('Move canceled; the accepted pair or neighboring geometry changed');
    Exit;
  end;
  FPair := Default(TNyxProjectPair);
  LEdit := LContext.Session.CapturePosition(NyxPositionControl(FOwner, APosition), FContext);
  LContext.Commands.Edit(LEdit);
end;

end.
