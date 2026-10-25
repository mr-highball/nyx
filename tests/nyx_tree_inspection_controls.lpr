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

procedure Journey(AWidth: Integer);
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

begin
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

begin
  try
    {$ifdef PAS2JS}
    Journey(1280);
    Journey(390);
    document.body.setAttribute('data-tree-inspection-checks', IntToStr(GChecks));
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
