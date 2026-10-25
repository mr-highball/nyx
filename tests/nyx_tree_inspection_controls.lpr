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
  {$endif}
  SysUtils, nyx.text, nyx.model, nyx.controls, nyx.state, nyx.codec, nyx.codegen,
  nyx.collections, nyx.collections.view, nyx.collections.mount, nyx.collections.selection,
  nyx.collections.query, nyx.collections.query.session, nyx.collections.tree.inspection,
  nyx.publication, nyx.process.tree
  {$ifdef NYX_PROCESS_GENERATED}, nyx.generated.view{$endif};

{$ifdef PAS2JS}
type
  { CSSOM scroll offsets are fractional pixels. This standard HTMLElement bridge
    borrows the mounted Tree only within the fixture; no external host is owned. }
  TTreeScrollElement = class external name 'HTMLElement'(TJSHTMLElement)
    scrollTopPixels: Double; external name 'scrollTop';
  end;
{$endif}

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
  LDocument := BuildNyxDocument;
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
      'search reports one actual match, four context ancestors and one disclosed row');
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

{$ifdef PAS2JS}
function BrowserChecks: JSValue; async;
begin
  Result := Undefined;
  try
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
