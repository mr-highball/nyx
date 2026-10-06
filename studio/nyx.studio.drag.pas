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

unit nyx.studio.drag;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.types, nyx.model, nyx.behavior, nyx.events, nyx.gestures,
  nyx.designer.input, nyx.studio.projects, nyx.studio.session,
  nyx.studio.edits, nyx.studio.sourcejobs;

const
  NyxStudioDragMoveID = 'studio-drag-move';
  NyxStudioDropPositionID = 'studio-drop-position';

type
  { UI-thread borrowed context, used only during the capture callback. The
    broker copies pair/identity/epoch values and never retains either owner.
    Both mount contexts identify the actual source/target views, not merely a
    current project whose IDs happen to match a retired view. }
  TNyxStudioDragContext = record
    Session: TNyxStudioSession;
    Commands: TNyxSourceCommands;
    SourceMount: TNyxStudioCommandContext;
    CanvasMount: TNyxStudioCommandContext;
    Designing: Boolean;
    Placement: TNyxPlacement;
  end;
  TNyxStudioDragCapture = function: TNyxStudioDragContext of object;
  { Presentation-only feedback. An empty target restores accepted selection.
    Receivers must not publish source/design, refresh sources or navigate here. }
  TNyxStudioDragFeedback = procedure(const ATarget: TNyxControlRef) of object;

  { Local per-editor drag broker. Public Nyx source streams and designer drop
    input feed this one command path. Transfers contain an opaque local lease,
    never executable text, a document, compiler paths or imported HTML. Hover
    uses copied format/target identity only; final pair admission stays with the
    existing source processor. Destroy/Disconnect detach borrowed callbacks
    before canceling registrations; no router/receiver ownership cycle exists. }
  TNyxStudioDrag = class
  private
    FCapture: TNyxStudioDragCapture;
    FFeedback: TNyxStudioDragFeedback;
    FCallbacks: array of INyxEventCallback;
    FSubscriptions: array of INyxEventSubscription;
    FConnectedEvents: INyxEvents;
    FConnectedRevision: Integer;
    FConnectedContext: TNyxStudioCommandContext;
    FConnectedMove: TNyxText;
    FNonce: TNyxText;
    FSerial: Integer;
    FLease: TNyxText;
    FKind: TNyxKindRef;
    FControl: TNyxControlRef;
    FContext: TNyxStudioCommandContext;
    FPair: TNyxProjectPair;
    FView: TNyxText;
    FSchemaRevision: Integer;
    FPlacement: TNyxPlacement;
    FMarked: TNyxControlRef;
    function Capture: TNyxStudioDragContext;
    function Live(out AContext: TNyxStudioDragContext): Boolean;
    function Target(const ATarget: TNyxDesignerTarget;
      ASession: TNyxStudioSession): TNyxControlRef;
    procedure Mark(const ATarget: TNyxControlRef);
    function Offer(const AKind: TNyxKindRef; const AControl: TNyxControlRef;
      const AEvent: TNyxEventInfo; const AResponse: INyxGestureResponse): TNyxText;
    procedure Finish(const ALease: TNyxText);
    procedure AddSource(const AEvents: INyxEvents; ANode: TNyxNode);
  public
    { Borrow both method receivers until Destroy. Capture must run on the UI
      thread and return owners alive throughout this synchronous invocation. }
    constructor Create(ACapture: TNyxStudioDragCapture;
      AFeedback: TNyxStudioDragFeedback = nil);
    destructor Destroy; override;
    { Scan only the ordinary editor shell, never authored/runtime descendants.
      Retained source views reuse registrations; replacement or changed move
      identity cancels the old lease. Borrow the shell for this call only. }
    procedure ConnectSources(const AEvents: INyxEvents; AShell: TNyxNode;
      const AMount: TNyxStudioCommandContext);
    procedure DisconnectSources;
    { Consume copied adapter input. A readable exact local lease at drop is
      required; formats during protected hover never authorize a mutation. }
    procedure Gesture(const ATarget: TNyxDesignerTarget;
      const AEvent: TNyxEventInfo; const ADecision: INyxGestureDecision);
    procedure Cancel;
  end;

{ One bounded private transfer format shared by browser/native adapters. }
function NyxStudioPlacementFormat: TNyxTransferFormatRef;

implementation

uses
  SysUtils, nyx.schema, nyx.scheduler;

type
  INyxStudioDragSource = interface(INyxEventCallback)
    ['{2414A2FD-273D-48A3-B33B-E6908FE81D75}']
    procedure Detach;
  end;

  TSourceCallback = class(TNyxEventCallback, INyxStudioDragSource)
  private
    FBroker: TNyxStudioDrag;
    FKind: TNyxKindRef;
    FControl: TNyxControlRef;
    FLease: TNyxText;
  public
    constructor Create(ABroker: TNyxStudioDrag; const AKind: TNyxKindRef;
      const AControl: TNyxControlRef);
    procedure Detach;
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

function NyxStudioPlacementFormat: TNyxTransferFormatRef;
begin
  Result := NyxTransferFormat('application/x-nyx-studio-placement');
end;

constructor TSourceCallback.Create(ABroker: TNyxStudioDrag; const AKind: TNyxKindRef;
  const AControl: TNyxControlRef);
begin
  inherited Create;
  FBroker := ABroker;
  FKind := AKind;
  FControl := AControl;
end;

procedure TSourceCallback.Detach;
begin
  FBroker := nil;
  FLease := '';
end;

procedure TSourceCallback.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin

  if (FBroker = nil) or AExecution.Cancelled then
  begin
    Exit;
  end;

  if AEvent.Trigger = ntDragStart then
  begin
    FLease := FBroker.Offer(FKind, FControl, AEvent, NyxGestureResponse(AExecution));
  end
  else if AEvent.Trigger = ntDragEnd then
  begin
    FBroker.Finish(FLease);
    FLease := '';
  end;
end;

constructor TNyxStudioDrag.Create(ACapture: TNyxStudioDragCapture;
  AFeedback: TNyxStudioDragFeedback);
var
  LID: TGUID;
begin
  inherited Create;

  if not Assigned(ACapture) then
  begin
    raise ENyxModel.Create('Designer dragging requires its current editor capture');
  end;

  if CreateGUID(LID) <> 0 then
  begin
    raise ENyxModel.Create('Cannot create a local designer drag identity');
  end;
  FNonce := TNyxText(GUIDToString(LID));
  FCapture := ACapture;
  FFeedback := AFeedback;
end;

destructor TNyxStudioDrag.Destroy;
begin
  FFeedback := nil;
  DisconnectSources;
  FCapture := nil;
  inherited Destroy;
end;

function TNyxStudioDrag.Capture: TNyxStudioDragContext;
begin
  Result := FCapture();

  if (Result.Session = nil) or (Result.Commands = nil) then
  begin
    raise ENyxModel.Create('Designer drag capture returned a retired editor');
  end;
end;

procedure TNyxStudioDrag.Mark(const ATarget: TNyxControlRef);
begin

  if FMarked.ID = ATarget.ID then
  begin
    Exit;
  end;
  FMarked := ATarget;

  if Assigned(FFeedback) then
  begin
    FFeedback(ATarget);
  end;
end;

procedure TNyxStudioDrag.Cancel;
begin
  FLease := '';
  FKind := Default(TNyxKindRef);
  FControl := Default(TNyxControlRef);
  FPair := Default(TNyxProjectPair);
  FContext := Default(TNyxStudioCommandContext);
  FView := '';
  Mark(Default(TNyxControlRef));
end;

procedure TNyxStudioDrag.Finish(const ALease: TNyxText);
begin

  if (ALease <> '') and (ALease = FLease) then
  begin
    Cancel;
  end;
end;

procedure TNyxStudioDrag.DisconnectSources;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FCallbacks) do
  begin
    (FCallbacks[LIndex] as INyxStudioDragSource).Detach;
  end;
  for LIndex := 0 to High(FSubscriptions) do
  begin
    FSubscriptions[LIndex].Cancel;
  end;
  FSubscriptions := nil;
  FCallbacks := nil;
  FConnectedEvents := nil;
  FConnectedMove := '';
  Cancel;
end;

procedure TNyxStudioDrag.AddSource(const AEvents: INyxEvents; ANode: TNyxNode);
var
  LCallback: INyxEventCallback;
  LKind: TNyxKindRef;
  LControl: TNyxControlRef;
  LIndex: Integer;
begin
  LKind := NyxCustomKind(ANode.Prop('add-kind'));
  LControl := NyxControl(ANode.Prop('designer-drag-control'));

  if (LKind.Name <> '') or (LControl.ID <> '') then
  begin
    LCallback := TSourceCallback.Create(Self, LKind, LControl);
    SetLength(FCallbacks, Length(FCallbacks) + 1);
    FCallbacks[High(FCallbacks)] := LCallback;
    SetLength(FSubscriptions, Length(FSubscriptions) + 2);
    FSubscriptions[High(FSubscriptions) - 1] := AEvents
      .OnDragStart(NyxControlEvents(ANode.ID, niRuntime)).Policy(neSequential).Subscribe(LCallback);
    FSubscriptions[High(FSubscriptions)] := AEvents
      .OnDragEnd(NyxControlEvents(ANode.ID, niRuntime)).Policy(neSequential).Subscribe(LCallback);
  end;
  for LIndex := 0 to ANode.Count - 1 do
  begin
    AddSource(AEvents, ANode.Children[LIndex]);
  end;
end;

procedure TNyxStudioDrag.ConnectSources(const AEvents: INyxEvents; AShell: TNyxNode;
  const AMount: TNyxStudioCommandContext);
var
  LContext: TNyxStudioDragContext;
  LMove: TNyxNode;
  LMoveID: TNyxText;
begin

  if (AEvents = nil) or (AShell = nil) then
  begin
    DisconnectSources;
    Exit;
  end;
  LContext := Capture;
  LMove := AShell.Find(NyxStudioDragMoveID);
  LMoveID := '';

  if LMove <> nil then
  begin
    LMoveID := LMove.Prop('designer-drag-control');
  end;

  if (FConnectedEvents = AEvents) and
    (FConnectedRevision = AEvents.ViewRevision) and (FConnectedMove = LMoveID) and
    LContext.Session.MatchesCommandContext(FConnectedContext) then
  begin
    Exit;
  end;
  DisconnectSources;

  if not LContext.Session.MatchesCommandContext(AMount) then
  begin
    raise ENyxModel.Create('Designer drag sources belong to a retired shell');
  end;
  FConnectedEvents := AEvents;
  FConnectedRevision := AEvents.ViewRevision;
  FConnectedContext := AMount;
  FConnectedMove := LMoveID;
  try
    AddSource(AEvents, AShell);
  except
    DisconnectSources;
    raise;
  end;
end;

function TNyxStudioDrag.Offer(const AKind: TNyxKindRef; const AControl: TNyxControlRef;
  const AEvent: TNyxEventInfo; const AResponse: INyxGestureResponse): TNyxText;
var
  LContext: TNyxStudioDragContext;
  LNode: TNyxNode;
  LAllowed: TNyxDropOperations;
begin
  Result := '';
  Cancel;
  LContext := Capture;

  if not LContext.Designing or LContext.Commands.Busy or
    LContext.Session.SourceDraftPending or
    not LContext.Session.MatchesCommandContext(FConnectedContext) or
    not LContext.Session.MatchesCommandContext(LContext.SourceMount) or
    not AEvent.HasDrag or (AEvent.Drag.Phase <> ndpStart) or
    not AEvent.Drag.CanRespond or
    not AResponse.CanRequest(ngcOfferDrag) then
  begin
    Exit;
  end;

  if AKind.Name <> '' then
  begin

    if (AControl.ID <> '') or (LContext.Session.Catalog.IndexOf(AKind.Name) < 0) then
    begin
      Exit;
    end;
    LAllowed := [ndoCopy];
  end
  else
  begin
    LNode := LContext.Session.Document.Find(AControl.ID);

    if (LNode = nil) or (LNode.Parent = nil) or (LNode.Kind = 'slot-override') then
    begin
      Exit;
    end;
    LAllowed := [ndoMove];
  end;

  if FSerial = High(Integer) then
  begin
    raise ENyxModel.Create('Designer drag sequence is exhausted');
  end;
  Inc(FSerial);
  FPair := LContext.Session.ProjectSnapshot;
  FContext := LContext.Session.CommandContext;
  FView := LContext.Session.ActiveViewID;
  FSchemaRevision := NyxSchemaRevision;
  FPlacement := LContext.Placement;
  FKind := AKind;
  FControl := AControl;
  FLease := FNonce + '/' + IntToStr(FSerial);
  AResponse.OfferDrag(NyxTransferCustom(NyxStudioPlacementFormat, FLease), LAllowed);
  Result := FLease;
end;

function TNyxStudioDrag.Live(out AContext: TNyxStudioDragContext): Boolean;
begin
  Result := False;

  if FLease = '' then
  begin
    Exit;
  end;
  AContext := Capture;
  Result := AContext.Designing and not AContext.Commands.Busy and
    not AContext.Session.SourceDraftPending and
    AContext.Session.MatchesCommandContext(FContext) and
    AContext.Session.MatchesCommandContext(AContext.CanvasMount) and
    (AContext.Session.ActiveViewID = FView) and
    (AContext.Placement = FPlacement) and (NyxSchemaRevision = FSchemaRevision);

  if not Result then
  begin
    Cancel;
  end;
end;

function TNyxStudioDrag.Target(const ATarget: TNyxDesignerTarget;
  ASession: TNyxStudioSession): TNyxControlRef;
var
  LOwner: TNyxNode;
  LSource: TNyxNode;
  LAncestor: TNyxNode;
  LParent: TNyxNode;
  LIndex: Integer;
begin
  Result := Default(TNyxControlRef);
  LOwner := ASession.Document.Find(ATarget.Owner.ID);

  if LOwner = nil then
  begin
    Exit;
  end;
  LSource := ASession.Document.Find(ATarget.Source.ID);
  LAncestor := LSource;
  while (LAncestor <> nil) and (LAncestor <> LOwner) do
  begin
    LAncestor := LAncestor.Parent;
  end;

  if LAncestor = LOwner then
  begin
    Result := NyxControl(LSource.ID);
  end
  else if (ATarget.Path.Name = '.') and (FPlacement <> nplInside) then
  begin
    Result := NyxControl(LOwner.ID);
  end
  else if (FPlacement = nplInside) and ATarget.Container and (ATarget.Path.Name <> '') then
  begin
    { Inherited definition content is never an editable descendant of this
      instance. Only an exact existing customized layout descriptor is eligible;
      creating overrides implicitly would silently change user ownership. }
    for LIndex := 0 to LOwner.Count - 1 do
    begin

      if (LOwner.Children[LIndex].Kind = 'slot-override') and
        (LOwner.Children[LIndex].Prop('path') = ATarget.Path.Name) then
      begin
        Result := NyxControl(LOwner.Children[LIndex].ID);
        Break;
      end;
    end;
  end;

  if Result.ID = '' then
  begin
    Exit;
  end;
  LParent := ASession.Document.Find(Result.ID);

  if FPlacement <> nplInside then
  begin
    LParent := LParent.Parent;
  end;

  if (LParent = nil) or ((FPlacement = nplInside) and not ATarget.Container) or
    ((LParent.ProjectionKind = NyxKindName(nkComponent)) and
      (LParent.Prop('component') <> '')) then
  begin
    Exit(Default(TNyxControlRef));
  end;

  if LParent.Kind = 'slot-override' then
  begin

    if (LParent.Prop('mode') <> 'properties') and
      (LParent.Prop('mode') <> 'append') and (LParent.Prop('mode') <> 'prepend') then
    begin
      Exit(Default(TNyxControlRef));
    end;
  end
  else
  begin
    LIndex := ASession.Catalog.IndexOf(LParent.Kind);

    if (LIndex < 0) or not ASession.Catalog[LIndex].Container then
    begin
      Exit(Default(TNyxControlRef));
    end;
  end;

  if FKind.Name = '' then
  begin
    LSource := ASession.Document.Find(FControl.ID);

    if (LSource = nil) or (LSource.ID = Result.ID) then
    begin
      Exit(Default(TNyxControlRef));
    end;
    LAncestor := LParent;
    while LAncestor <> nil do
    begin

      if LAncestor = LSource then
      begin
        Exit(Default(TNyxControlRef));
      end;
      LAncestor := LAncestor.Parent;
    end;
  end;
end;

procedure TNyxStudioDrag.Gesture(const ATarget: TNyxDesignerTarget;
  const AEvent: TNyxEventInfo; const ADecision: INyxGestureDecision);
var
  LContext: TNyxStudioDragContext;
  LTarget: TNyxControlRef;
  LPair: TNyxProjectPair;
  LOperation: TNyxDropOperation;
  LEdit: TNyxStudioDesignEdit;
begin

  if not AEvent.HasDrag or not Live(LContext) then
  begin
    Exit;
  end;

  if AEvent.Drag.Phase = ndpExit then
  begin
    Mark(Default(TNyxControlRef));
    Exit;
  end;

  if not (AEvent.Drag.Phase in [ndpEnter, ndpOver, ndpDrop]) or
    not AEvent.Drag.Transfer.HasFormat(NyxStudioPlacementFormat) or
    AEvent.Drag.Transfer.HasFiles or not ADecision.CanRequest(ngcAcceptDrop) then
  begin
    Mark(Default(TNyxControlRef));
    Exit;
  end;
  LTarget := Target(ATarget, LContext.Session);

  if LTarget.ID = '' then
  begin
    Mark(Default(TNyxControlRef));
    Exit;
  end;
  LOperation := ndoMove;

  if FKind.Name <> '' then
  begin
    LOperation := ndoCopy;
  end;

  if not (LOperation in AEvent.Drag.Allowed) then
  begin
    Exit;
  end;

  if AEvent.Drag.Phase <> ndpDrop then
  begin
    ADecision.AcceptDrop(LOperation);
    Mark(ATarget.Owner);
    Exit;
  end;

  if not AEvent.Drag.Transfer.Readable or
    (AEvent.Drag.Transfer.TextFor(NyxStudioPlacementFormat) <> FLease) then
  begin
    Cancel;
    Exit;
  end;
  { Pair encoding/admission occurs only at the final operation, not each hover.
    This also catches callers that mutated a borrowed document outside commands.
    The isolated publication rechecks the exact captured pair/creator epoch. }
  LPair := LContext.Session.ProjectSnapshot;

  if LPair.Pending or (LPair.Design <> FPair.Design) or (LPair.Source <> FPair.Source) then
  begin
    Cancel;
    Exit;
  end;

  if FKind.Name <> '' then
  begin
    LEdit := LContext.Session.CaptureNewPlacement(FKind, LTarget, FPlacement, LContext.CanvasMount);
  end
  else
  begin
    LEdit := LContext.Session.CapturePlacement(NyxPlaceControl(FControl, LTarget,
      FPlacement), LContext.CanvasMount);
  end;
  Cancel;
  LContext.Commands.Edit(LEdit);
  ADecision.AcceptDrop(LOperation);
end;

end.
