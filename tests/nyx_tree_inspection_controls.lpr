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
program nyx_tree_inspection_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  {$ifdef PAS2JS}
  JS, Web, nyx.application.browser,
  {$else}
  Interfaces, Classes, Forms, Controls, StdCtrls, ComCtrls, Windows, nyx.application.lcl,
  nyx.collections.lcl.tree, nyx.editing.lcl,
  {$endif}
  SysUtils, nyx.text, nyx.model, nyx.controls, nyx.state, nyx.codec, nyx.codegen,
  nyx.collections, nyx.collections.view, nyx.collections.mount, nyx.collections.selection,
  nyx.collections.query, nyx.collections.query.session, nyx.collections.tree.inspection,
  nyx.publication, nyx.process.tree, nyx.editing, nyx.collections.recovery
  {$ifdef NYX_PROCESS_GENERATED}, nyx.generated.view{$endif}
  {$ifdef NYX_TREE_EDIT_GENERATED}, nyx.generated.tree.editing{$endif};

{$ifdef PAS2JS}
type
  { CSSOM scroll offsets are fractional pixels. This standard HTMLElement bridge
    borrows the mounted Tree only within the fixture; no external host is owned. }
  TTreeScrollElement = class external name 'HTMLElement'(TJSHTMLElement)
    scrollTopPixels: Double; external name 'scrollTop';
  end;
{$endif}

type
  { A borrowed observer records the one ordered retirement publication. The
    subscription is released before this probe; a veto never installs state. }
  TReplacementProbe = class
  public
    Reject: Boolean;
    Notifications: Integer;
    Changes: INyxCollectionChanges;
    procedure Validate(const ACandidate: INyxCollectionSnapshot;
      const AChanges: INyxCollectionChanges);
    procedure Changed(const AStore: INyxCollection; const AChanges: INyxCollectionChanges);
  end;

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create('Tree inspection: ' + AReason);
  end;
end;

procedure TReplacementProbe.Validate(const ACandidate: INyxCollectionSnapshot;
  const AChanges: INyxCollectionChanges);
begin

  if Reject then
  begin
    raise ENyxCollection.Create('Expected mounted replacement veto');
  end;
end;

procedure TReplacementProbe.Changed(const AStore: INyxCollection;
  const AChanges: INyxCollectionChanges);
begin
  Inc(Notifications);
  Changes := AChanges;
end;

{ Use a separate application so the original inspection journey's single
  deliberate deletion and its source-revision assertion remain unchanged. }
procedure ReplacementJourney(AWidth: Integer);
var
  LDocument: TNyxDocument;
  LView: INyxCollectionView;
  LPeer: INyxCollectionView;
  LTree: INyxTreeHierarchy;
  LBefore: INyxCollectionSnapshot;
  LFirstSnapshot: INyxCollectionSnapshot;
  LPeerSnapshot: INyxCollectionSnapshot;
  LToken: INyxCollectionSubscription;
  LProbe: TReplacementProbe;
  LGateway: TNyxItemRef;
  LComputer: TNyxItemRef;
  LArchive: TNyxItemRef;
  LRefused: Boolean;
  LSaved: TNyxText;
  LPhysicalSelected: Boolean;
  {$ifdef PAS2JS}
  LApplication: TNyxBrowserApplication;
  LHost: TJSHTMLElement;
  {$else}
  LApplication: TNyxLCLApplication;
  {$endif}

  function GatewayExpanded: Boolean;
  {$ifdef PAS2JS}
  var
    LRow: TJSHTMLElement;
  {$else}
  var
    LFace: TTreeView;
    LNode: TTreeNode;
  {$endif}
  begin
    {$ifdef PAS2JS}
    LRow := TJSHTMLElement(LApplication.View.ElementFor(ProcessTreeID)
      .querySelector('[data-nyx-item="gateway"]'));
    Check(LRow <> nil, 'replacement gateway has an actual browser Tree row');
    LPhysicalSelected := LRow.getAttribute('aria-selected') = 'true';
    Result := LRow.hasAttribute('open');
    {$else}
    Application.ProcessMessages;
    LFace := TTreeView(LApplication.View.ControlFor(ProcessTreeID));
    LNode := LFace.Items.GetFirstNode;

    while (LNode <> nil) and (TNyxText(LNode.Text) <> 'Request gateway') do
    begin
      LNode := LNode.GetNext;
    end;
    Check(LNode <> nil, 'replacement gateway has an actual native Tree row');
    LPhysicalSelected := LFace.Selected = LNode;
    Result := LNode.Expanded;
    {$endif}
  end;

begin
  {$ifdef NYX_PROCESS_GENERATED}
  LDocument := nyx.generated.view.BuildNyxDocument;
  {$else}
  LDocument := BuildProcessTreeDocument;
  {$endif}
  LApplication := nil;
  LProbe := TReplacementProbe.Create;
  try
    LSaved := TNyxCodec.Encode(LDocument);
    {$ifdef PAS2JS}
    LHost := TJSHTMLElement(document.createElement('div'));
    LHost.style.setProperty('width', IntToStr(AWidth) + 'px');
    document.body.appendChild(LHost);
    LApplication := TNyxBrowserApplication.Create;
    LApplication.Run(LDocument, LHost);
    {$else}
    LApplication := TNyxLCLApplication.Create;
    LApplication.Mount(LDocument);
    LApplication.Window.ClientWidth := AWidth;
    LApplication.Window.ClientHeight := 900;
    LApplication.Window.Show;
    Application.ProcessMessages;
    {$endif}
    LView := LApplication.View.CollectionMount(ProcessTreeID).View;
    LPeer := LApplication.View.CollectionMount(ProcessPeerID).View;
    LTree := NyxTreeHierarchy(LView);
    LGateway := NyxItem(NyxCollection(ProcessCollectionName), 'gateway');
    LComputer := NyxItem(NyxCollection(ProcessCollectionName), 'computer');
    LArchive := NyxItem(NyxCollection(ProcessCollectionName), 'archive');
    LTree.SetExpanded(LComputer, True);
    LTree.SetExpanded(NyxItem(NyxCollection(ProcessCollectionName), 'services'), True);
    LTree.SetExpanded(LGateway, True);
    LView.Select(LGateway);
    LPeer.Select(LArchive);
    NyxTreeHierarchy(LPeer).SetExpanded(LComputer, False);
    Check(GatewayExpanded, 'the mounted gateway is expanded before replacement');
    Check(LPhysicalSelected, 'the old gateway has actual widget selection before replacement');
    LBefore := LView.Store.Snapshot;
    LFirstSnapshot := LView.Snapshot;
    LPeerSnapshot := LPeer.Snapshot;
    LToken := LView.Store.Subscribe(LProbe.Changed, LProbe.Validate);
    LProbe.Reject := True;
    LRefused := False;
    try
      LView.Store.Apply([NyxRemove(LGateway),
        NyxInsert(LBefore.IndexOf(LGateway), LBefore.Item(LGateway))]);
    except
      on E: ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LView.Store.Snapshot = LBefore) and
      (LView.Snapshot = LFirstSnapshot) and (LPeer.Snapshot = LPeerSnapshot) and
      (LProbe.Notifications = 0), 'veto retains source, independent views and observer state');
    Check(LView.Selection.Contains(LGateway) and LPeer.Selection.Contains(LArchive) and
      LTree.IsExpanded(LGateway) and GatewayExpanded,
      'veto retains old selection and actual mounted disclosure');
    LProbe.Reject := False;
    LView.Store.Apply([NyxRemove(LGateway),
      NyxInsert(LBefore.IndexOf(LGateway), LBefore.Item(LGateway))]);
    Check((LView.Store.Snapshot.Revision = LBefore.Revision + 1) and
      (LProbe.Notifications = 1) and (LProbe.Changes.Count = 2) and
      (LProbe.Changes.Kind(0) = nceRemove) and (LProbe.Changes.Kind(1) = nceInsert) and
      (LView.Store.Snapshot.IndexOf(LGateway) = LBefore.IndexOf(LGateway)) and
      LView.Store.Snapshot.Item(LGateway).SameItem(LBefore.Item(LGateway)),
      'equal data/order publishes the exact two-step retirement once');
    Check(not LView.Selection.Contains(LGateway) and not LTree.IsExpanded(LGateway) and
      not GatewayExpanded and LTree.IsExpanded(LComputer) and
      LPeer.Selection.Contains(LArchive) and not NyxTreeHierarchy(LPeer).IsExpanded(LComputer),
      'replacement inherits no old selection/disclosure while unrelated peer intent survives' +
      ' [selected=' + BoolToStr(LView.Selection.Contains(LGateway), True) +
      '; expanded=' + BoolToStr(LTree.IsExpanded(LGateway), True) +
      '; physical=' + BoolToStr(GatewayExpanded, True) +
      '; parent=' + BoolToStr(LTree.IsExpanded(LComputer), True) +
      '; peer-selected=' + BoolToStr(LPeer.Selection.Contains(LArchive), True) +
      '; peer-parent=' + BoolToStr(NyxTreeHierarchy(LPeer).IsExpanded(LComputer), True) + ']');
    Check(not LPhysicalSelected and not LView.Selection.Focus.Defined and
      not LView.Selection.Anchor.Defined,
      'actual widget selection and logical focus/anchor are retired before returning to input');
    Check(TNyxCodec.Encode(LDocument) = LSaved, 'replacement never changes saved Tree defaults');
  finally
    LToken := nil;
    LProbe.Free;
    LApplication.Free;
    LDocument.Free;
    {$ifdef PAS2JS}LHost.remove;{$endif}
  end;
end;

{$ifdef PAS2JS}
function Journey(AWidth: Integer): JSValue; async;
{$else}
procedure Journey(AWidth: Integer);
{$endif}
var
  LDocument: TNyxDocument;
  LBinding: TNyxProcessTreeDemo;
  LView: INyxCollectionView;
  LPeer: INyxCollectionView;
  LTree: INyxTreeHierarchy;
  LCapture: INyxTreeInspectionCapture;
  LHeld: INyxTreeInspectionCapture;
  LFacts: TNyxTreeInspectionObservation;
  LQuery: INyxCollectionSnapshot;
  LSelection: INyxCollectionSelection;
  LSource: INyxCollectionSnapshot;
  LSession: INyxCollectionQuerySession;
  LPeerOwner: INyxCollectionQueryOwner;
  LPlan: INyxPreparedPublication;
  LSaved: TNyxText;
  LRefused: Boolean;
  {$ifdef PAS2JS}
  LApplication: TNyxBrowserApplication;
  LHost: TJSHTMLElement;
  {$else}
  LApplication: TNyxLCLApplication;
  LStream: TFileStream;
  LBytes: UTF8String;
  {$endif}

  function Item(const AID: TNyxText): TNyxItemRef;
  begin
    Result := NyxItem(NyxCollection(ProcessCollectionName), AID);
  end;

  procedure Click(const AID: TNyxText);
  begin
    {$ifdef PAS2JS}
    TJSHTMLElement(LApplication.View.ElementFor(AID)).click;
    {$else}
    SendMessage(TWinControl(LApplication.View.ControlFor(AID)).Handle, BM_CLICK, 0, 0);
    Application.ProcessMessages;
    {$endif}
  end;

  procedure EnterSearch(const AText: TNyxText);
  begin
    {$ifdef PAS2JS}
    TJSHTMLInputElement(LApplication.View.InputFor(ProcessSearchID)).value := AText;
    LApplication.View.InputFor(ProcessSearchID).dispatchEvent(TJSEvent.new('input'));
    {$else}
    TCustomEdit(LApplication.View.InputFor(ProcessSearchID)).Text := AText;
    {$endif}
    Check(LApplication.State.GetValue(NyxTextState(ProcessSearchID)) = AText,
      'editing the actual input updates its bound search text');
  end;

  procedure PhysicalFocus(const ACaption: TNyxText; const AID: TNyxText);
  begin
    {$ifdef PAS2JS}
    Check(LApplication.View.ElementFor(ProcessTreeID).querySelector(
      '[data-nyx-item="' + AID + '"]') <> nil, 'revealed browser row is realized');
    {$else}
    Check((TTreeView(LApplication.View.ControlFor(ProcessTreeID)).Selected <> nil) and
      (TNyxText(TTreeView(LApplication.View.ControlFor(ProcessTreeID)).Selected.Text) = ACaption),
      'actual native tree cursor follows explicit reveal');
    {$endif}
  end;

  {$ifdef PAS2JS}
  function ViewportCases: JSValue; async;
  {$else}
  procedure ViewportCases;
  {$endif}
  var
    LMount: INyxCollectionMount;
    LReveal: INyxTreeReveal;
    LBefore: INyxCollectionSnapshot;
    LChanges: array of TNyxCollectionEdit;
    LIndex: Integer;
    LFirst: Integer;
    LSerial: Integer;
    LTarget: TNyxItemRef;
    {$ifdef PAS2JS}
    LFace: TJSHTMLElement;
    LButton: TJSHTMLElement;
    {$else}
    LFace: TTreeView;
    LButton: TWinControl;
    LNode: TTreeNode;
    {$endif}

    {$ifdef PAS2JS}
    function Settle: JSValue; async;
    begin
      Result := Undefined;
      await(TJSPromise.resolve(TJSPromise.new(procedure(AResolve, AReject: TJSPromiseResolver)
        begin
          window.setTimeout(procedure begin AResolve(Undefined); end, 50);
        end)));
    end;
    {$endif}

    function InViewport: Boolean;
    {$ifdef PAS2JS}
    var
      LRow: TJSHTMLElement;
      LBounds: TJSDOMRect;
      LClip: TJSDOMRect;
    {$else}
    var
      LBounds: TRect;
      LPoint: TPoint;
      LRowTop: Integer;
      LRowBottom: Integer;
      LParent: TWinControl;
    {$endif}
    begin
      {$ifdef PAS2JS}
      LRow := TJSHTMLElement(LFace.querySelector('[data-nyx-item="' + LTarget.ID + '"]'));
      Result := LRow <> nil;

      if Result then
      begin
        LBounds := LRow.querySelector('summary').getBoundingClientRect;
        LClip := LFace.getBoundingClientRect;
        Result := (LBounds.bottom > LBounds.top) and (LBounds.top >= LClip.top - 1) and
          (LBounds.bottom <= LClip.bottom + 1) and (LBounds.top >= -1) and
          (LBounds.bottom <= window.innerHeight + 1);
      end;
      {$else}
      LBounds := LNode.DisplayRect(False);
      Result := (LBounds.Bottom > LBounds.Top) and (LBounds.Top >= 0) and
        (LBounds.Bottom <= LFace.ClientHeight);
      LPoint.X := 0;
      LPoint.Y := LBounds.Top;
      LRowTop := LFace.ClientToScreen(LPoint).Y;
      LPoint.Y := LBounds.Bottom;
      LRowBottom := LFace.ClientToScreen(LPoint).Y;
      LParent := LFace.Parent;
      while Result and (LParent <> nil) do
      begin
        LPoint.Y := 0;
        Result := LRowTop >= LParent.ClientToScreen(LPoint).Y;
        LPoint.Y := LParent.ClientHeight;
        Result := Result and (LRowBottom <= LParent.ClientToScreen(LPoint).Y);
        LParent := LParent.Parent;
      end;
      {$endif}
    end;

    function CallerFocused: Boolean;
    begin
      {$ifdef PAS2JS}Result := document.activeElement = LButton;
      {$else}Result := LApplication.Window.ActiveControl = LButton;{$endif}
    end;

    procedure ScrollAway;
    begin
      {$ifdef PAS2JS}
      LButton.focus;
      TTreeScrollElement(LFace).scrollTopPixels := 0;
      LFace.dispatchEvent(TJSEvent.new('scroll'));
      {$else}
      LButton.SetFocus;
      LFace.TopItem := LFace.Items.GetFirstNode;
      Application.ProcessMessages;
      {$endif}
    end;

  begin
    {$ifdef PAS2JS}Result := Undefined;{$endif}
    Click(ProcessClearSearchID);
    LMount := LApplication.View.CollectionMount(ProcessTreeID);
    Check(Supports(LView, INyxTreeReveal, LReveal), 'the ordinary Tree exposes typed explicit viewport intent');
    LBefore := LView.Store.Snapshot;
    LFirst := LBefore.Count;
    SetLength(LChanges, 200);
    for LIndex := 0 to High(LChanges) do
    begin
      LChanges[LIndex] := NyxInsert(LFirst + LIndex,
        NyxCollectionItem(Item('viewport-' + TNyxText(IntToStr(LIndex))))
          .WithValue(NyxTextField('name'), 'Process worker ' + TNyxText(IntToStr(LIndex)))
          .WithValue(NyxTextField('parent'), 'workers'));
    end;
    LView.Store.Apply(LChanges, LBefore.Revision);
    LTree.ExpandAll;
    LView.Select(Item('archive'));
    LBefore := LView.Store.Snapshot;
    LTarget := Item('viewport-199');
    {$ifdef PAS2JS}
    LFace := LApplication.View.ElementFor(ProcessTreeID);
    LButton := LApplication.View.ElementFor(ProcessFindID);
    await(Settle);
    {$else}
    LFace := TTreeView(LApplication.View.ControlFor(ProcessTreeID));
    LButton := TWinControl(LApplication.View.ControlFor(ProcessFindID));
    LNode := LFace.Items.FindNodeWithText('Process worker 199');
    Check(LNode <> nil, 'the long native Tree contains its exact target');
    {$endif}
    ScrollAway;
    {$ifdef PAS2JS}await(Settle);{$endif}
    Check(not InViewport, 'the long Tree target starts outside the actual viewport');
    Check(RevealNyxTreeItem(LView, LTarget) = ntrrRevealed, 'a typed reveal is admitted');
    {$ifdef PAS2JS}await(Settle);{$else}Application.ProcessMessages;{$endif}
    Check(InViewport and CallerFocused, 'default reveal moves the actual viewport while preserving caller focus');
    Check((LView.Selection.Count = 1) and LView.Selection.Contains(Item('archive')) and
      (LView.Selection.Anchor.ID = 'archive'), 'viewport reveal preserves selected membership and anchor');
    LSerial := LReveal.CaptureTreeReveal.Serial;
    ScrollAway;
    {$ifdef PAS2JS}await(Settle);{$endif}
    Check(not InViewport, 'the reader can deliberately scroll away after the first reveal');
    RevealNyxTreeItem(LView, LTarget);
    {$ifdef PAS2JS}await(Settle);{$else}Application.ProcessMessages;{$endif}
    Check(InViewport and (LReveal.CaptureTreeReveal.Serial > LSerial),
      'repeating reveal at unchanged logical focus performs a fresh physical request');
    ScrollAway;
    {$ifdef PAS2JS}await(Settle);{$endif}
    LMount.Refresh;
    {$ifdef PAS2JS}await(Settle);{$endif}
    Check(not InViewport, 'an ordinary refresh does not replay a consumed reveal and pull the reader back');
    RevealNyxTreeItem(LView, LTarget, ntrfTake);
    {$ifdef PAS2JS}await(Settle);{$else}Application.ProcessMessages;{$endif}
    {$ifdef PAS2JS}
    Check(InViewport and LFace.contains(document.activeElement), 'opt-in reveal transfers actual browser focus to the Tree');
    {$else}
    Check(InViewport and (LApplication.Window.ActiveControl = LFace), 'opt-in reveal transfers actual native focus to the Tree');
    {$endif}
    LTree.SetExpanded(Item('workers'), False);
    {$ifdef PAS2JS}await(Settle);{$endif}
    Check(not LReveal.CaptureTreeReveal.Item.Defined,
      'a later disclosure command invalidates old viewport intent before queued geometry can reuse it');
    LTree.ExpandAll;
    ScrollAway;
    LMount.SetInteraction(False, False);
    RevealNyxTreeItem(LView, LTarget, ntrfTake);
    LMount.SetInteraction(True, False);
    {$ifdef PAS2JS}await(Settle);{$else}Application.ProcessMessages;{$endif}
    Check(CallerFocused, 'enabling a target cannot revive a focus request issued while it was disabled');
    Check(LView.Store.Snapshot = LBefore, 'viewport requests do not alter accepted source data');
    LView.Store.Update(NyxCollectionItem(Item('archive')).WithValue(NyxTextField('name'), 'Archive worker updated'));
    Check(not LReveal.CaptureTreeReveal.Item.Defined,
      'a newer source revision invalidates a former request even when its target ID survives');
  end;

begin
  {$ifdef PAS2JS}Result := Undefined;{$endif}
  {$ifdef NYX_PROCESS_GENERATED}
  LDocument := nyx.generated.view.BuildNyxDocument;
  {$else}
  LDocument := BuildProcessTreeDocument;
  {$endif}
  LApplication := nil;
  LBinding := nil;
  try
    LSaved := TNyxCodec.Encode(LDocument);
    {$ifndef PAS2JS}

    if ParamCount > 1 then
    begin
      Check(not FileExists(ParamStr(2)), 'the generated companion is fresh');
      LBytes := UTF8Encode(TNyxCodegen.Generate(LDocument));
      LStream := TFileStream.Create(ParamStr(2), fmCreate);
      try
        LStream.WriteBuffer(LBytes[1], Length(LBytes));
      finally
        LStream.Free;
      end;
    end;
    LApplication := TNyxLCLApplication.Create;
    LApplication.Mount(LDocument);
    LApplication.Window.ClientWidth := AWidth;
    LApplication.Window.ClientHeight := 900;
    LApplication.Window.Show;
    Application.ProcessMessages;
    Check(LApplication.View.ControlFor(ProcessTreeID).IsVisible,
      'the ordinary native Tree is mounted and visible');
    {$else}
    LHost := TJSHTMLElement(document.createElement('div'));
    LHost.style.setProperty('width', IntToStr(AWidth) + 'px');
    document.body.appendChild(LHost);
    LApplication := TNyxBrowserApplication.Create;
    LApplication.Run(LDocument, LHost);
    {$endif}
    LView := LApplication.View.CollectionMount(ProcessTreeID).View;
    LPeer := LApplication.View.CollectionMount(ProcessPeerID).View;
    LTree := NyxTreeHierarchy(LView);
    LBinding := TNyxProcessTreeDemo.Create(LApplication.Context, LApplication.View.Events, LView);
    LSource := LView.Store.Snapshot;
    Check((LView.Store = LPeer.Store) and (LView <> LPeer), 'two views share one source and independent runtime intent');
    Check(LView.Selection.Count = 0, 'the fresh Tree has no selected membership');
    {$ifdef PAS2JS}
    Check(not LView.Selection.Focus.Defined,
      'the browser startup journey exercises an undefined optional focus');
    {$endif}
    EnterSearch('INDEX');
    Click(ProcessFindID);
    LCapture := CaptureNyxTreeInspection(LView);
    Check((LCapture.Observation.MatchedRows = 1) and
      (LCapture.Observation.ContextRows = 4),
      'ordinary startup Find works before any process is selected');
    Click(ProcessNextID);
    LCapture := CaptureNyxTreeInspection(LView);
    Check(LView.Selection.Focus.Defined and (LView.Selection.Focus.ID = 'indexer') and
      (LView.Selection.Count = 0) and (LCapture.Observation.DisclosedRows = 5),
      'next match reveals and focuses the first result without inventing selection');
    Check((LPeer.Snapshot.Count = 7) and not NyxTreeHierarchy(LPeer).IsExpanded(Item('computer')),
      'startup search and reveal leave the peer independent');
    Click(ProcessClearSearchID);
    Check((LView.Snapshot.Count = 7) and LTree.IsExpanded(Item('computer')) and
      (LView.Selection.Count = 0),
      'clearing startup search preserves deliberate disclosure and empty membership');
    LView.Select(Item('archive'));
    LTree.CollapseAll;
    LCapture := CaptureNyxTreeInspection(LView);
    Check((LCapture.Observation.CollapsedSelectedRows = 1) and
      (LCapture.Observation.FilteredSelectedRows = 0), 'a closed branch is distinct from a filtered selection');
    EnterSearch('INDEX');
    Click(ProcessFindID);
    LCapture := CaptureNyxTreeInspection(LView);
    LFacts := LCapture.Observation;
    Check(LFacts.Defined and (LFacts.SourceRows = 7) and (LFacts.ResultRows = 5) and
      (LFacts.MatchedRows = 1) and (LFacts.ContextRows = 4) and (LFacts.DisclosedRows = 1),
      'search reports one actual match, four context ancestors and one disclosed row' +
      ' [source/result/match/context/disclosed=' + IntToStr(LFacts.SourceRows) + '/' +
      IntToStr(LFacts.ResultRows) + '/' + IntToStr(LFacts.MatchedRows) + '/' +
      IntToStr(LFacts.ContextRows) + '/' + IntToStr(LFacts.DisclosedRows) +
      '; binding=' + LApplication.View.LastBindingError + ']');
    Check((LCapture.RoleOf(Item('computer')) = ntrContext) and
      (LCapture.RoleOf(Item('indexer')) = ntrMatch) and
      (LCapture.RoleOf(Item('archive')) = ntrAbsent), 'typed roles do not call an ancestor a match');
    Check((LCapture.VisibilityOf(Item('indexer')) = ntvCollapsed) and
      (LCapture.VisibilityOf(Item('archive')) = ntvFiltered) and
      (LFacts.FilteredSelectedRows = 1), 'hidden selected data remains independently inspectable');
    Check(Pos('1 selected hidden by filters', LApplication.State.GetValue(NyxTextState(ProcessCountsID))) > 0,
      'the public bound status explains the hidden selection');
    Check((LPeer.Snapshot.Count = 7) and not NyxTreeHierarchy(LPeer).IsExpanded(Item('computer')),
      'search and disclosure never change the peer');
    LHeld := LCapture;
    Click(ProcessNextID);
    LCapture := CaptureNyxTreeInspection(LView);
    Check((LCapture.VisibilityOf(Item('indexer')) = ntvDisclosed) and
      (LCapture.Observation.DisclosedRows = 5) and (LView.Selection.Focus.ID = 'indexer'),
      'the actual next-match button opens the complete deeply nested path');
    Check((LView.Selection.Count = 1) and LView.Selection.Contains(Item('archive')) and
      (LView.Selection.Anchor.ID = 'archive'), 'revealing changes neither selected membership nor the range anchor');
    PhysicalFocus('Search index worker', 'indexer');
    LQuery := LView.Snapshot;
    LSelection := LView.Selection;
    Click(ProcessRevealID);
    Check((LView.Snapshot = LQuery) and (LView.Selection = LSelection) and
      (Pos('Filters hide', LApplication.State.GetValue(NyxTextState(ProcessMessageID))) > 0),
      'revealing a filtered selected process explains the obstacle without changing policy or focus');
    LTree.SetExpanded(Item('gateway'), False);
    Click(ProcessClearSearchID);
    Check((LView.Snapshot.Count = 7) and not LTree.IsExpanded(Item('gateway')) and
      LView.Selection.Contains(Item('archive')), 'clearing search retains later disclosure and surviving selection');
    Check(LApplication.State.GetValue(NyxTextState(ProcessSearchID)) = '',
      'Clear my search clears its input and predicate in the same model publication');
    Check((LHeld.Observation.MatchedRows = 1) and (LHeld.VisibilityOf(Item('indexer')) = ntvCollapsed),
      'retained capture facts do not change with later disclosure or queries');
    LSession := NyxCollectionQuerySession(LView);
    LPeerOwner := LSession.AcquireOwner(NyxQueryOrigin('running-filter'), 'Only running processes');
    LPlan := LSession.PrepareSession(LSession.Capture, LPeerOwner,
      NyxQuerySessionEdits.Filter(NyxQueryChannel('running'), NyxWhere(NyxBooleanField('running')).EqualTo(True)));
    PublishNyxGroup([LPlan]);
    EnterSearch('index');
    Click(ProcessFindID);
    Click(ProcessRelaxID);
    Check((LSession.Capture.Count = 1) and not LView.Snapshot.Has(Item('archive')) and
      (LView.Selection.Focus.ID <> 'archive') and LView.Selection.Contains(Item('archive')),
      'deliberate relaxation clears only this search owner; a peer filter still hides the selection');
    LPlan := LSession.PrepareSession(LSession.Capture, LPeerOwner, NyxQuerySessionEdits.ClearOwner);
    PublishNyxGroup([LPlan]);
    LView.Select(Item('gateway'));
    EnterSearch('index');
    Click(ProcessFindID);
    Click(ProcessClearSearchID);
    Check((LView.Selection.Count = 1) and LView.Selection.Contains(Item('gateway')) and
      not LView.Selection.Contains(Item('archive')), 'clearing does not restore an obsolete selection snapshot');
    LView.Store.Remove(Item('indexer'));
    EnterSearch('index');
    Click(ProcessFindID);
    Check((CaptureNyxTreeInspection(LView).Observation.MatchedRows = 0) and
      (RevealNyxTreeItem(LView, Item('indexer')) = ntrrMissing), 'an intervening deletion produces no substitute match or focus');
    LRefused := False;
    try
      RevealNyxTreeItem(LView, NyxItem(NyxCollection('foreign'), 'indexer'));
    except
      on E: ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'a foreign identity cannot acquire navigation authority');
    Click(ProcessClearSelectionID);
    Check(LView.Selection.Count = 0, 'the actual Clear selection button is explicit and effective');
    Check(TNyxCodec.Encode(LDocument) = LSaved, 'runtime navigation never mutates saved defaults/history');
    Check(LView.Store.Snapshot.Revision = LSource.Revision + 1,
      'all inspection operations leave the source unchanged except the deliberate deletion');
    {$ifdef PAS2JS}await(ViewportCases);{$else}ViewportCases;{$endif}
    LBinding.Close;
    LQuery := LView.Snapshot;
    LApplication.State.SetValue(NyxTextState(ProcessSearchID), 'gateway');
    Click(ProcessFindID);
    Check(LView.Snapshot = LQuery, 'closed bindings withdraw actual button authority');
    LApplication.Free;
    LApplication := nil;
    Check((LHeld.Observation.SourceRows = 7) and (LHeld.MatchAt(0).ID = 'indexer'),
      'detached inspection remains readable after application/widget retirement');
  finally
    LBinding.Free;
    LApplication.Free;
    LDocument.Free;
    {$ifdef PAS2JS}LHost.remove;{$endif}
  end;
end;

{$ifndef PAS2JS}
{ This journey uses the actual LCL inline label editor, not a manual OnEdited
  invocation. Its app/source are independent of the original search assertions. }
procedure EditingJourney(AWidth: Integer);
const
  CDraft: TNyxText = 'Careful 🌙 unfinished label';
var
  LDocument: TNyxDocument;
  LApplication: TNyxLCLApplication;
  LMount: INyxCollectionMount;
  LPeer: INyxCollectionMount;
  LView: INyxCollectionView;
  LStore: INyxCollection;
  LTree: TTreeView;
  LEditor: TCustomEdit;
  LNode: TTreeNode;
  LJournal: INyxCollectionDraftRecovery;
  LBudget: INyxCollectionDraftRecoveryBudget;
  LPending: INyxCollectionPendingDraftMount;
  LAdmission: INyxCollectionInputAdmission;
  LReview: INyxPendingCollectionDraft;
  LBefore: INyxCollectionSnapshot;
  LGateway: TNyxItemRef;
  LWorkers: TNyxItemRef;
  LSession: INyxCollectionQuerySession;
  LOwner: INyxCollectionQueryOwner;
  LPlan: INyxPreparedPublication;
  LProbe: TReplacementProbe;
  LToken: INyxCollectionSubscription;
  LSelection: TNyxTextSelection;
  LCount: Integer;
  LSaved: TNyxText;
  LBytes: UTF8String;
  LStream: TFileStream;

  procedure Settle;
  var
    LTurn: Integer;
  begin
    for LTurn := 1 to 4 do
    begin
      Application.ProcessMessages;
      Application.Idle(False);
      CheckSynchronize(0);
    end;
  end;

  procedure BeginDraft(const AText: TNyxText);
  var
    LNode: TTreeNode;
  begin
    LView.Select(LGateway);
    Settle;
    LNode := LTree.Items.FindNodeWithText(UTF8Encode(LView.CellText(LGateway, 0)));
    Check(LNode <> nil, 'editable Tree resolves its actual gateway node');
    LNode.EditText;
    Settle;
    LEditor := NyxLCLTreeEditor(LTree);
    Check((LEditor <> nil) and LEditor.Focused, 'the actual native Tree label editor accepts focus');
    LBefore := LStore.Snapshot;
    RestoreNyxLCLInputText(LEditor, AText);
    SelectNyxLCLText(LEditor, NyxTextSelection(AText, 8, 9));
    Check((NyxLCLInputText(LEditor) = AText) and (LStore.Snapshot = LBefore),
      'typing supplementary text remains an unfinished draft without source writes');
  end;

  procedure FilterGateway(AHide: Boolean);
  begin

    if AHide then
    begin
      LPlan := LSession.PrepareSession(LSession.Capture, LOwner,
        NyxQuerySessionEdits.Filter(NyxQueryChannel('editing-filter'),
          NyxWhere(NyxTextField('name')).EqualTo('Archive worker')));
    end
    else
    begin
      LPlan := LSession.PrepareSession(LSession.Capture, LOwner, NyxQuerySessionEdits.ClearOwner);
    end;
    PublishNyxGroup([LPlan]);
    LPlan := nil;
    Settle;
  end;

begin
  {$ifdef NYX_TREE_EDIT_GENERATED}
  LDocument := nyx.generated.tree.editing.BuildNyxDocument;
  {$else}
  LDocument := BuildProcessTreeDocument(True);
  {$endif}
  LApplication := nil;
  LProbe := TReplacementProbe.Create;
  try
    LSaved := TNyxCodec.Encode(LDocument);

    if ParamCount > 2 then
    begin
      Check(not FileExists(ParamStr(3)), 'the editable Tree companion is freshly exported');
      LBytes := UTF8Encode(TNyxCodegen.Generate(LDocument, 'nyx.generated.tree.editing'));
      LStream := TFileStream.Create(ParamStr(3), fmCreate);
      try
        LStream.WriteBuffer(LBytes[1], Length(LBytes));
      finally
        LStream.Free;
      end;
    end;
    LApplication := TNyxLCLApplication.Create;
    LApplication.Mount(LDocument);
    LApplication.Window.ClientWidth := AWidth;
    LApplication.Window.ClientHeight := 900;
    LApplication.Window.Show;
    Settle;
    LMount := LApplication.View.CollectionMount(ProcessTreeID);
    LPeer := LApplication.View.CollectionMount(ProcessPeerID);
    LView := LMount.View;
    LStore := LView.Store;
    LTree := TTreeView(LApplication.View.ControlFor(ProcessTreeID));
    LGateway := NyxItem(NyxCollection(ProcessCollectionName), 'gateway');
    LWorkers := NyxItem(LGateway.Collection, 'workers');
    LJournal := NyxCollectionDraftRecovery(LMount);
    LBudget := NyxCollectionDraftRecoveryBudget(LJournal);
    Supports(LMount, INyxCollectionPendingDraftMount, LPending);
    Supports(LMount, INyxCollectionInputAdmission, LAdmission);
    LSession := NyxCollectionQuerySession(LView);
    LOwner := LSession.AcquireOwner(NyxQueryOrigin('editing'), 'Temporary label inspection');
    LToken := LStore.Subscribe(LProbe.Changed, LProbe.Validate);
    BeginDraft(CDraft);
    LStore.Update(NyxCollectionItem(NyxItem(LGateway.Collection, 'archive'))
      .WithValue(NyxBooleanField('running'), True));
    Settle;
    LEditor := NyxLCLTreeEditor(LTree);
    Check((LEditor <> nil) and (NyxLCLInputText(LEditor) = CDraft) and
      (LJournal.Capture.Count = 0), 'an unrelated source update preserves the actual unfinished Tree editor');
    LSelection := CaptureNyxLCLSelection(LEditor);
    Check(LSelection.Defined and (LSelection.Start = 8) and (LSelection.Finish = 9),
      'Tree refresh preserves the supplementary character selection in scalar positions');
    LBefore := LStore.Snapshot;
    LTree.Selected := LTree.Items.FindNodeWithText('Archive worker');
    Settle;
    Check(not LTree.IsEditing and LView.Selection.Focus.Defined and
      (LView.Selection.Focus.ID = 'archive') and (LStore.Snapshot = LBefore),
      'real native selection leaves the old label dormant without stealing new focus');
    LTree.Selected := LTree.Items.FindNodeWithText(UTF8Encode(LView.CellText(LGateway, 0)));
    Settle;
    LNode := LTree.Selected;
    LNode.EditText;
    Settle;
    LEditor := NyxLCLTreeEditor(LTree);
    Check((LEditor <> nil) and (NyxLCLInputText(LEditor) = CDraft) and
      (LJournal.Capture.Count = 0), 'returning through native F2 restores the dormant exact label');
    LSelection := CaptureNyxLCLSelection(LEditor);
    Check((LSelection.Start = 8) and (LSelection.Finish = 9), 'dormant native labels retain scalar caret provenance');
    LView.Select(NyxItem(LGateway.Collection, 'archive'));
    Settle;
    Check(not LTree.IsEditing and (LView.Selection.Focus.ID = 'archive'),
      'a programmatic selection likewise does not reopen an editor over new focus');
    LView.Select(LGateway);
    Settle;
    LTree.Selected.EditText;
    Settle;
    LEditor := NyxLCLTreeEditor(LTree);
    Check((LEditor <> nil) and (NyxLCLInputText(LEditor) = CDraft),
      'explicitly returning to a programmatically displaced draft restores its value');
    NyxTreeHierarchy(LView).SetExpanded(NyxItem(LGateway.Collection, 'services'), False);
    Settle;
    Check(not LTree.IsEditing and (LJournal.Capture.Count = 0) and (LStore.Snapshot = LBefore),
      'closing an ancestor keeps a compatible draft dormant without inventing recovery or a source write');
    NyxTreeHierarchy(LView).SetExpanded(NyxItem(LGateway.Collection, 'services'), True);
    LView.Select(LGateway);
    Settle;
    LTree.Selected.EditText;
    Settle;
    LEditor := NyxLCLTreeEditor(LTree);
    Check((LEditor <> nil) and (NyxLCLInputText(LEditor) = CDraft),
      'reopening the path restores the same dormant label through normal editing');
    FinishNyxLCLTreeEditor(LTree, False);
    Settle;
    Check((LStore.Snapshot.Item(LGateway).GetValue(NyxTextField('name')) = CDraft) and
      (LJournal.Capture.Count = 0) and not LTree.IsEditing,
      'ordinary native completion commits exact Unicode once without recovering its own accepted edit');
    BeginDraft(CDraft + ' veto');
    LBefore := LStore.Snapshot;
    LProbe.Reject := True;
    FinishNyxLCLTreeEditor(LTree, False);
    Settle;
    LEditor := NyxLCLTreeEditor(LTree);
    Check((LStore.Snapshot = LBefore) and (LEditor <> nil) and
      (NyxLCLInputText(LEditor) = CDraft + ' veto'), 'a validator veto reopens the exact unfinished editor');
    LProbe.Reject := False;
    FinishNyxLCLTreeEditor(LTree, True);
    Settle;
    BeginDraft(CDraft + ' changed');
    LStore.Update(NyxCollectionItem(LGateway).WithValue(NyxTextField('name'), 'Gateway renamed'));
    Settle;
    Check((LJournal.Capture.Count = 1) and
      (LJournal.EntryAt(0).Reason = ncdlValueChanged) and
      (LJournal.EntryAt(0).Cell.Text = CDraft + ' changed') and not LTree.IsEditing,
      'changed accepted labels recover old draft text instead of overwriting the producer');
    BeginDraft(CDraft + ' dormant');
    LTree.Selected := LTree.Items.FindNodeWithText('Archive worker');
    Settle;
    LStore.Update(NyxCollectionItem(LGateway).WithValue(NyxTextField('name'), 'Dormant gateway changed'));
    Settle;
    Check((LJournal.Capture.Count = 2) and
      (LJournal.EntryAt(1).Reason = ncdlValueChanged) and
      (LJournal.EntryAt(1).Cell.Text = CDraft + ' dormant') and not LTree.IsEditing,
      'a displaced dormant label still uses ordinary recovery when accepted source changes');
    BeginDraft(CDraft + ' replacement');
    LBefore := LStore.Snapshot;
    LStore.Apply([NyxRemove(LGateway), NyxInsert(LBefore.IndexOf(LGateway), LBefore.Item(LGateway))]);
    Settle;
    Check((LJournal.Capture.Count = 3) and
      (LJournal.EntryAt(2).Reason = ncdlIdentityReplaced) and
      (LJournal.EntryAt(2).Cell.Text = CDraft + ' replacement') and not LTree.IsEditing,
      'same-value replacement cannot inherit the old native Tree editor');
    BeginDraft(CDraft + ' hidden');
    LBefore := LStore.Snapshot;
    FilterGateway(True);
    Check((LStore.Snapshot = LBefore) and (LJournal.Capture.Count = 4) and
      (LJournal.EntryAt(3).Reason = ncdlHidden) and not LTree.IsEditing and
      LPeer.View.Snapshot.Has(LGateway), 'filtering recovers the hidden draft while source and peer stay independent');
    FilterGateway(False);
    BeginDraft(CDraft + ' composing');
    LCount := LJournal.Capture.Count;
    SendMessage(LEditor.Handle, WM_IME_STARTCOMPOSITION, 0, 0);
    LStore.Update(NyxCollectionItem(LGateway).WithValue(NyxTextField('name'), 'After composition'));
    Check(LTree.IsEditing and (NyxLCLInputText(LEditor) = CDraft + ' composing') and
      (LJournal.Capture.Count = LCount) and
      (LPeer.View.Snapshot.Revision = LStore.Snapshot.Revision),
      'native IME observation defers Tree destruction while committed source and peer advance');
    SendMessage(LEditor.Handle, WM_IME_ENDCOMPOSITION, 0, 0);
    Settle;
    Check((LJournal.Capture.Count = LCount + 1) and
      (LJournal.EntryAt(LCount).Cell.Text = CDraft + ' composing') and not LTree.IsEditing,
      'post-composition idle admits recovery before closing its original native editor');
    BeginDraft(CDraft + ' dormant pressure');
    LTree.Selected := LTree.Items.FindNodeWithText('Archive worker');
    Settle;
    LTree.Selected.EditText;
    Settle;
    LEditor := NyxLCLTreeEditor(LTree);
    Check(LEditor <> nil, 'a second row has its own active native label editor');
    RestoreNyxLCLInputText(LEditor, CDraft + ' companion');
    SelectNyxLCLText(LEditor, NyxTextSelection(CDraft + ' companion', 8, 9));
    LCount := LJournal.Capture.Count;
    LBudget.ConfigureLimits(NyxDraftRecoveryLimits.Entries(LCount).PayloadBytes(8192));
    LStore.Update(NyxCollectionItem(LGateway).WithValue(NyxTextField('name'), 'Dormant pressure changed'));
    Settle;
    LReview := LPending.CapturePendingDraft;
    Check(NyxCollectionDraftRecoveryHeld(LMount) and (LReview <> nil) and
      (LReview.Cell.Item.ID = 'gateway') and (LReview.Cell.Text = CDraft + ' dormant pressure') and
      LTree.IsEditing and (NyxLCLInputText(LEditor) = CDraft + ' companion'),
      'dormant journal pressure exposes the correct pending row without losing another active editor');
    Check(LPending.DiscardPendingDraft(LReview) and LTree.IsEditing and
      (NyxLCLInputText(LEditor) = CDraft + ' companion'),
      'discarding a dormant receipt never cancels a different active native editor');
    LMount.Refresh;
    Settle;
    LEditor := NyxLCLTreeEditor(LTree);
    LReview := LPending.CapturePendingDraft;
    LBytes := '';

    if LReview <> nil then
    begin
      LBytes := LReview.Cell.Item.ID + ':' + LReview.Cell.Text;
    end;
    Check(LAdmission.InputReady and (LEditor <> nil) and
      (NyxLCLInputText(LEditor) = CDraft + ' companion') and
      (LJournal.Capture.Count = LCount), 'refresh after dormant discard preserves unrelated exact work' +
      ' [ready=' + BoolToStr(LAdmission.InputReady, True) +
      '; editor=' + BoolToStr(LEditor <> nil, True) +
      '; text=' + BoolToStr((LEditor <> nil) and
        (NyxLCLInputText(LEditor) = CDraft + ' companion'), True) +
      '; journal=' + IntToStr(LJournal.Capture.Count) + '/' + IntToStr(LCount) +
      '; held=' + BoolToStr(NyxCollectionDraftRecoveryHeld(LMount), True) +
      '; pending=' + String(LBytes) +
      '; error=' + LMount.ErrorText + ']');
    FinishNyxLCLTreeEditor(LTree, True);
    Settle;
    BeginDraft(CDraft + ' pressure');
    LCount := LJournal.Capture.Count;
    LBudget.ConfigureLimits(NyxDraftRecoveryLimits.Entries(LCount).PayloadBytes(8192));
    LStore.Apply([NyxUpdate(NyxCollectionItem(LWorkers).WithValue(NyxTextField('parent'), 'services')),
      NyxRemove(LGateway)]);
    Settle;
    Check(NyxCollectionDraftRecoveryHeld(LMount) and not LAdmission.InputReady and
      LTree.IsEditing and (NyxLCLInputText(LEditor) = CDraft + ' pressure') and
      not LPeer.View.Snapshot.Has(LGateway), 'journal pressure holds the real Tree editor while the peer accepts removal');
    LReview := LPending.CapturePendingDraft;
    Check((LReview <> nil) and (LReview.Cell.Text = CDraft + ' pressure') and
      LReview.Cell.Selection.Defined, 'pending Tree review exposes exact detached text and scalar selection');
    RestoreNyxLCLInputText(LEditor, CDraft + ' newer');
    Check(not LPending.DiscardPendingDraft(LReview) and LTree.IsEditing,
      'a stale pending receipt cannot discard newer native Tree text');
    LReview := LPending.CapturePendingDraft;
    LBefore := LStore.Snapshot;
    Check(LPending.DiscardPendingDraft(LReview), 'explicit current pending discard accepts its exact Tree receipt');
    Check(NyxCollectionDraftRecoveryHeld(LMount) and not LAdmission.InputReady and
      (LStore.Snapshot = LBefore) and (LJournal.Capture.Count = LCount),
      'discard clears only reviewed text; explicit refresh owns subsequent map adoption');
    LMount.Refresh;
    Settle;
    Check(not NyxCollectionDraftRecoveryHeld(LMount) and LAdmission.InputReady and
      not LTree.IsEditing and (LStore.Snapshot = LBefore) and
      (LJournal.Capture.Count = LCount), 'discard changes no source or journal and releases the held native map');
    Check(TNyxCodec.Encode(LDocument) = LSaved, 'runtime Tree drafts never mutate saved defaults');
    LToken := nil;
    LApplication.Free;
    LApplication := nil;
    Check(not LJournal.Capture.Active and (LJournal.EntryAt(0).Cell.Text = CDraft + ' changed'),
      'recovered Tree data stays readable after its native widget and app retire');
  finally
    LToken := nil;
    LApplication.Free;
    LProbe.Free;
    LDocument.Free;
  end;
end;
{$endif}

{$ifdef PAS2JS}
function BrowserChecks: JSValue; async;
begin
  Result := Undefined;
  try
    ReplacementJourney(1280);
    ReplacementJourney(390);
    await(Journey(1280));
    await(Journey(390));
    document.body.setAttribute('data-tree-inspection-checks', IntToStr(GChecks));
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-tree-inspection-error', E.Message);
    end;
  end;
end;
{$endif}

begin
  try
    {$ifdef PAS2JS}
    BrowserChecks;
    {$else}
    Application.Initialize;
    Application.CaptureExceptions := False;
    EditingJourney(StrToInt(ParamStr(1)));
    ReplacementJourney(StrToInt(ParamStr(1)));
    Journey(StrToInt(ParamStr(1)));
    WriteLn('PASS ', GChecks, ' Tree inspection control checks at ', ParamStr(1));
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-tree-inspection-error', E.Message);
      {$else}
      WriteLn('FAIL after ', GChecks, ' checks: ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
