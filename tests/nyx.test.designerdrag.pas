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
unit nyx.test.designerdrag;

{$mode delphi}{$H+}{$codepage utf8}

interface

{ Shared contract checks use owned fixtures and actual event subscriptions.
  They do not assert physical input or execute a generated application. }
function RunNyxDesignerDragGuards: Integer;

implementation

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.data, nyx.behavior, nyx.gestures,
  nyx.events, nyx.schema, nyx.composition, nyx.designer.input, nyx.studio.drag,
  nyx.studio.edits, nyx.studio.projects, nyx.studio.session,
  nyx.studio.sourcejobs, nyx.test.placement;

type
  { Own the test owners; the broker borrows this capture receiver. Retained
    event registrations must be inert after that broker is destroyed. }
  TDragContext = class
    Session: TNyxStudioSession;
    Commands: TNyxSourceCommands;
    Context: TNyxStudioDragContext;
    LastMarked: TNyxText;
    function Capture: TNyxStudioDragContext;
    procedure Feedback(const ATarget: TNyxControlRef);
  end;

function TDragContext.Capture: TNyxStudioDragContext;
begin
  Result := Context;
end;

procedure TDragContext.Feedback(const ATarget: TNyxControlRef);
begin
  LastMarked := ATarget.ID;
end;

function RunNyxDesignerDragGuards: Integer;
var
  LOwner: TDragContext;
  LBroker: TNyxStudioDrag;
  LShell: TNyxNode;
  LCanvas: TNyxNode;
  LEvents: INyxEvents;
  LOffer: TNyxGestureResult;
  LBefore: TNyxProjectPair;
  LPolicy: TNyxDesignerInput;
  LChecks: Integer;

  procedure Check(AValue: Boolean; const AReason: TNyxText);
  begin

    if not AValue then
    begin
      raise ENyxModel.Create('Designer drag guard: ' + AReason);
    end;
    Inc(LChecks);
  end;

  procedure Connect;
  begin
    LOwner.Context.SourceMount := LOwner.Session.CommandContext;
    LOwner.Context.CanvasMount := LOwner.Session.CommandContext;
    LBroker.ConnectSources(LEvents, LShell, LOwner.Context.SourceMount);
  end;

  function Offer(const AID: TNyxText): TNyxGestureResult;
  var
    LInfo: TNyxEventInfo;
    LDecision: INyxGestureDecision;
    LTickets: TNyxExecutions;
    LIndex: Integer;
  begin
    LInfo := Default(TNyxEventInfo);
    LInfo.Value := NyxNull;
    LInfo.Trigger := ntDragStart;
    LInfo.Name := NyxEvent(NyxTriggerName(ntDragStart));
    LInfo.SourceID := AID;
    LInfo.OriginID := AID;
    LInfo.HasDrag := True;
    LInfo.Drag := NyxDragSnapshot(ndpStart, NyxTransferText(''),
      [ndoCopy, ndoMove], ndoNone, AID, True);
    LDecision := NewNyxGestureDecision([ngcOfferDrag]);
    try
      LTickets := LEvents.DispatchGesture(LInfo, AID, AID, LDecision);
      for LIndex := 0 to High(LTickets) do
      begin

        if LTickets[LIndex].Failure <> '' then
        begin
          raise ENyxModel.Create('Source callback failed: ' + LTickets[LIndex].Failure);
        end;
      end;
    finally
      Result := LDecision.Seal;
    end;
  end;

  function Gesture(const AID: TNyxText; APhase: TNyxDragPhase;
    const ATransfer: TNyxTransferSnapshot;
    AAllowed: TNyxDropOperations): TNyxGestureResult;
  var
    LInfo: TNyxEventInfo;
    LDecision: INyxGestureDecision;
  begin
    LInfo := Default(TNyxEventInfo);
    LInfo.HasDrag := True;
    LInfo.Drag := NyxDragSnapshot(APhase, ATransfer, AAllowed, ndoNone, '', True);
    LDecision := NewNyxGestureDecision([ngcAcceptDrop], AAllowed);
    try
      LBroker.Gesture(NyxDesignerTarget(LCanvas.Find(AID)), LInfo, LDecision);
    finally
      Result := LDecision.Seal;
    end;
    Check(not LDecision.CanRequest(ngcAcceptDrop), 'each test window is sealed');
  end;

begin
  LChecks := 0;
  LOwner := TDragContext.Create;
  LBroker := nil;
  LShell := nil;
  LCanvas := nil;
  try
    LOwner.Session := TNyxStudioSession.Create;
    LOwner.Session.LoadProject(NyxPlacementFixture);
    LOwner.Commands := TNyxSourceCommands.Create(LOwner.Session, nil);
    LOwner.Context := Default(TNyxStudioDragContext);
    LOwner.Context.Session := LOwner.Session;
    LOwner.Context.Commands := LOwner.Commands;
    LOwner.Context.Designing := True;
    LOwner.Context.Placement := nplInside;
    LEvents := NewNyxEvents;
    LShell := TNyxNode.Create(nkColumn, 'shell');
    LShell.Add(TNyxNode.Create(nkButton, 'palette-button')
      .Configure.DragSource(True).Done.SetProp('add-kind', 'button'));
    LShell.Add(TNyxNode.Create(nkButton, NyxStudioDragMoveID)
      .Configure.DragSource(True).Done.SetProp('designer-drag-control', 'notes-editor'));
    LCanvas := RealizeNyxView(LOwner.Session.Document, LOwner.Session.ActiveView);
    LBroker := TNyxStudioDrag.Create(LOwner.Capture, LOwner.Feedback);
    Connect;
    LPolicy := NyxDesignerInput;
    Check(not LPolicy.DropEnabled and LPolicy.Drops(True).DropEnabled and
      not LPolicy.DropEnabled, 'public policy configures a copied opt-in value');
    Check((NyxDesignerTarget(LCanvas.Find('left-layout')).Owner.ID = 'left-layout') and
      NyxDesignerTarget(LCanvas.Find('left-layout')).Container,
      'realized local target copies authored identity and container meaning');
    Check((NyxDesignerTarget(LCanvas.Find('reusable-instance/definition-actions')).Owner.ID =
      'reusable-instance') and
      (NyxDesignerTarget(LCanvas.Find('reusable-instance/definition-actions')).Path.Name = 'actions'),
      'inherited content carries exact instance and named part identity');
    LBefore := LOwner.Session.ProjectSnapshot;
    Check(not Gesture('right-layout', ndpDrop,
      NyxTransferCustom(NyxStudioPlacementFormat, 'external'), [ndoCopy]).Accepted,
      'external advertised format without an active local lease cannot mutate');
    LOffer := Offer('palette-button');
    Check(LOffer.Offered and (LOffer.Allowed = [ndoCopy]) and
      (LOffer.Transfer.TextFor(NyxStudioPlacementFormat) <> ''),
      'registered palette event produces one copied local lease');
    Check(Gesture('right-layout', ndpOver, LOffer.Transfer.ProtectedCopy,
      [ndoCopy]).Accepted, 'protected hover accepts exact local container');
    Check((LOwner.LastMarked = 'right-layout') and not LOwner.Commands.Busy and
      (EncodeNyxProject(LOwner.Session.ProjectSnapshot) = EncodeNyxProject(LBefore)),
      'hover changes presentation without paired files or queued work');
    Check(not Gesture('notes-editor', ndpOver, LOffer.Transfer.ProtectedCopy,
      [ndoCopy]).Accepted, 'inside refuses a leaf');
    Check(not Gesture('reusable-instance/definition-actions', ndpOver,
      LOffer.Transfer.ProtectedCopy, [ndoCopy]).Accepted,
      'inherited content without an exact local override cannot be edited');
    Check(not Gesture('right-layout', ndpOver, LOffer.Transfer.ProtectedCopy,
      [ndoMove]).Accepted, 'operation negotiation cannot turn a copy into a move');
    Check(not Gesture('right-layout', ndpDrop, LOffer.Transfer.ProtectedCopy,
      [ndoCopy]).Accepted, 'unreadable drop never publishes');
    Check(not Gesture('right-layout', ndpOver, LOffer.Transfer.ProtectedCopy,
      [ndoCopy]).Accepted, 'a refused final transfer permanently retires its lease');
    LOffer := Offer(NyxStudioDragMoveID);
    Check(LOffer.Offered and (LOffer.Allowed = [ndoMove]), 'move grip offers only a move');
    Check(not Gesture('notes-editor', ndpOver, LOffer.Transfer.ProtectedCopy,
      [ndoMove]).Accepted, 'self placement is refused');
    Check(Gesture('right-layout', ndpOver, LOffer.Transfer.ProtectedCopy,
      [ndoMove]).Accepted, 'exact authored move can hover another layout');
    Check(not Gesture('right-layout', ndpDrop,
      NyxTransferCustom(NyxStudioPlacementFormat, 'wrong lease'), [ndoMove]).Accepted,
      'wrong readable lease cannot publish or import text');
    LShell.Find(NyxStudioDragMoveID).SetProp('designer-drag-control', 'left-layout');
    Connect;
    LOwner.Context.Placement := nplBefore;
    LOffer := Offer(NyxStudioDragMoveID);
    Check(not Gesture('notes-editor', ndpOver, LOffer.Transfer.ProtectedCopy,
      [ndoMove]).Accepted, 'moving a layout beside its own descendant refuses a cycle');
    LShell.Find(NyxStudioDragMoveID).SetProp('designer-drag-control', 'notes-editor');
    Connect;
    LOwner.Context.Placement := nplBefore;
    LOffer := Offer(NyxStudioDragMoveID);
    Check(not Gesture('home', ndpOver, LOffer.Transfer.ProtectedCopy,
      [ndoMove]).Accepted, 'relative placement beside a document root is refused');
    Check(Gesture('send-button', ndpOver, LOffer.Transfer.ProtectedCopy,
      [ndoMove]).Accepted, 'relative placement beside a leaf resolves its parent');
    LOwner.Context.Placement := nplAfter;
    Check(not Gesture('send-button', ndpOver, LOffer.Transfer.ProtectedCopy,
      [ndoMove]).Accepted, 'changing drop position invalidates a captured drag');
    LOwner.Context.Placement := nplInside;
    LOffer := Offer('palette-button');
    LOwner.Session.SetSourceDraft(LOwner.Session.Source + #10 + '{ Draft }');
    Check(not Gesture('right-layout', ndpOver, LOffer.Transfer.ProtectedCopy,
      [ndoCopy]).Accepted, 'a pending source draft retires the drag');
    LOwner.Session.DiscardSourceDraft;
    Check(not Gesture('right-layout', ndpOver, LOffer.Transfer.ProtectedCopy,
      [ndoCopy]).Accepted, 'restoring accepted text cannot resurrect a retired lease');
    LOffer := Offer('palette-button');
    LOwner.Context.Designing := False;
    Check(not Gesture('right-layout', ndpOver, LOffer.Transfer.ProtectedCopy,
      [ndoCopy]).Accepted, 'runtime preview never accepts designer placement');
    Check(not Offer('palette-button').Offered, 'runtime preview cannot create designer offers');
    LOwner.Context.Designing := True;
    LOffer := Offer('palette-button');
    LOwner.Session.Activate('archive');
    Check(not Gesture('right-layout', ndpOver, LOffer.Transfer.ProtectedCopy,
      [ndoCopy]).Accepted, 'changing active view invalidates the target lease');
    LOwner.Session.Activate('home');
    LOffer := Offer('palette-button');
    LOwner.Session.Document.Find('first-caption').Configure.Text('Changed outside commands').Done;
    Check(not Gesture('right-layout', ndpDrop, LOffer.Transfer, [ndoCopy]).Accepted,
      'the final exact pair check refuses borrowed-tree changes');
    LOwner.Session.LoadProject(NyxPlacementFixture);
    Check(not Offer('palette-button').Offered, 'retired shell cannot offer in a new load');
    Connect;
    LOffer := Offer('palette-button');
    LEvents.CancelPending;
    Connect;
    Check(not Gesture('right-layout', ndpOver, LOffer.Transfer.ProtectedCopy,
      [ndoCopy]).Accepted, 'source view replacement revokes the old transfer');
    LOwner.Session.ApplyPatch(NyxReusablePatch([
      NyxOverrideComponentPart(NyxControl('reusable-instance'), NyxControl('local-actions'),
        NyxPart('actions'), noProperties)]));
    LCanvas.Free;
    LCanvas := RealizeNyxView(LOwner.Session.Document, LOwner.Session.ActiveView);
    Connect;
    LOffer := Offer('palette-button');
    Check(Gesture('reusable-instance/definition-actions', ndpOver,
      LOffer.Transfer.ProtectedCopy, [ndoCopy]).Accepted,
      'exact existing local layout override is an eligible target');
    Check(LOwner.LastMarked = 'reusable-instance',
      'override hover marks the visible instance, not an invisible descriptor');
    RegisterNyxSchema(NyxCustomKind('designer-drag-guard-epoch'), [], []);
    Check(not Gesture('right-layout', ndpOver, LOffer.Transfer.ProtectedCopy,
      [ndoCopy]).Accepted, 'a changed creator epoch invalidates the lease');
    LOwner.Session.LoadProject(NyxPlacementFixture);
    Connect;
    LOffer := Offer('palette-button');
    Check(LOffer.Offered, 'replacement source registrations remain usable');
    LBroker.Free;
    LBroker := nil;
    Check(not Offer('palette-button').Offered,
      'retained router becomes inert after borrowed receiver teardown');
    Check(not LOwner.Commands.Busy and not LOwner.Session.CanUndo,
      'all refused/hover operations leave the accepted pair and history intact');
    Result := LChecks;
  finally
    LBroker.Free;
    LEvents := nil;
    LCanvas.Free;
    LShell.Free;
    LOwner.Commands.Free;
    LOwner.Session.Free;
    LOwner.Free;
  end;
end;

end.
