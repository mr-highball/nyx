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
program nyx_tree_reveal_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.collections, nyx.collections.view,
  nyx.collections.view.types, nyx.collections.selection, nyx.collections.query,
  nyx.collections.tree.inspection, nyx.collections.mount
  {$ifdef PAS2JS}, Web{$endif};

type
  { Model target exercises real mount acknowledgement/epoch rules, independently
    of target API effects. Raw test access is borrowed while its interface owns
    the target; actual native viewport/focus is tested in the public journey. }
  TRevealTarget = class(TNyxCollectionMountBase)
  public
    Applied: Integer;
    Fail: Boolean;
    Defer: Boolean;
    Last: TNyxTreeRevealRequest;
    procedure RenderDataset; override;
    procedure DetachTarget; override;
    function DeferTargetRefresh: Boolean; override;
    procedure Hold(AValue: Boolean);
  end;

  { Weak observer is disconnected before destruction. It reads committed request,
    focus and disclosure together; a notification failure must not roll them back. }
  TRevealProbe = class
  public
    Calls: Integer;
    Fail: Boolean;
    Atomic: Boolean;
    Request: TNyxTreeRevealRequest;
    procedure Changed(const AView: INyxCollectionView; const AChanges: INyxCollectionChanges);
  end;

var
  GChecks: Integer;

procedure TRevealTarget.RenderDataset;
var
  LRequest: TNyxTreeRevealRequest;
begin
  LRequest := PendingTreeReveal;

  if LRequest.Item.Defined then
  begin
    TreeRevealApplied(LRequest.Serial);

    if Fail then
    begin
      raise ENyxCollection.Create('Expected target failure after proposed acknowledgement');
    end;
    Last := LRequest;
    Inc(Applied);
  end;
end;

procedure TRevealTarget.DetachTarget;
begin
  Last := Default(TNyxTreeRevealRequest);
end;

function TRevealTarget.DeferTargetRefresh: Boolean;
begin
  Result := Defer;

  if Result then
  begin
    FTargetRefreshDeferred := True;
  end;
end;

procedure TRevealTarget.Hold(AValue: Boolean);
begin
  SetCellEditsSuspended(AValue);
end;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create('Tree reveal: ' + AReason);
  end;
end;

procedure TRevealProbe.Changed(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
var
  LReveal: INyxTreeReveal;
begin
  Inc(Calls);
  Supports(AView, INyxTreeReveal, LReveal);
  Request := LReveal.CaptureTreeReveal;
  Atomic := Request.Item.Defined and (AView.Selection.Focus.ID = Request.Item.ID) and
    (CaptureNyxTreeInspection(AView).VisibilityOf(Request.Item) = ntvDisclosed);

  if Fail then
  begin
    raise ENyxCollection.Create('Expected committed reveal receiver failure');
  end;
end;

procedure Run;
const
  CLeaf: TNyxText = 'worker-🌙';
var
  LStore: INyxCollection;
  LView: INyxCollectionView;
  LPeer: INyxCollectionView;
  LReveal: INyxTreeReveal;
  LPeerReveal: INyxTreeReveal;
  LToken: INyxCollectionViewSubscription;
  LProbe: TRevealProbe;
  LKey: TNyxCollectionRef;
  LRoot: TNyxItemRef;
  LLeaf: TNyxItemRef;
  LOther: TNyxItemRef;
  LSpec: TNyxCollectionViewSpec;
  LBefore: INyxCollectionSnapshot;
  LRequest: TNyxTreeRevealRequest;
  LCalls: Integer;
  LRefused: Boolean;
  LMount: INyxCollectionMount;
  LTarget: TRevealTarget;
begin
  LProbe := TRevealProbe.Create;
  try
    LKey := NyxCollection('reveal');
    LRoot := NyxItem(LKey, 'root');
    LLeaf := NyxItem(LKey, CLeaf);
    LOther := NyxItem(LKey, 'other');
    LStore := NewNyxCollection(LKey, NyxCollectionSchema
      .Text(NyxTextField('name'), '').Text(NyxTextField('parent'), ''), [
        NyxCollectionItem(LRoot).WithValue(NyxTextField('name'), 'Root'),
        NyxCollectionItem(LLeaf).WithValue(NyxTextField('name'), 'Worker')
          .WithValue(NyxTextField('parent'), 'root'),
        NyxCollectionItem(LOther).WithValue(NyxTextField('name'), 'Other')]);
    LSpec := NyxCollectionView(LKey).Column(NyxTextField('name'), 'Name')
      .Parent(NyxTextField('parent')).Selection(nsmMultiple);
    LView := NewNyxCollectionView(LStore, LSpec, cpTree);
    LPeer := NewNyxCollectionView(LStore, LSpec, cpTree);
    Supports(LView, INyxTreeReveal, LReveal);
    Supports(LPeer, INyxTreeReveal, LPeerReveal);
    Check(not LReveal.CaptureTreeReveal.Item.Defined, 'new views do not invent a viewport request');
    LView.Select(LOther);
    LToken := LView.Subscribe(LProbe.Changed);
    LBefore := LStore.Snapshot;
    LReveal.RevealTreeItem(LLeaf);
    LRequest := LReveal.CaptureTreeReveal;
    Check(LProbe.Atomic and (LProbe.Calls = 1) and (LRequest.Serial = 1) and
      (LRequest.Item.ID = CLeaf) and (LRequest.Focus = ntrfPreserve),
      'one notification observes exact identity, ancestor disclosure, focus and new intent together');
    Check((LView.Selection.Count = 1) and LView.Selection.Contains(LOther) and
      (LView.Selection.Anchor.ID = LOther.ID) and (LStore.Snapshot = LBefore),
      'viewport intent changes no source, selection membership or anchor');
    Check(not LPeerReveal.CaptureTreeReveal.Item.Defined and not NyxTreeHierarchy(LPeer).IsExpanded(LRoot),
      'a peer has independent request and disclosure state');
    LReveal.RevealTreeItem(LLeaf, ntrfTake);
    Check(LProbe.Atomic and (LProbe.Calls = 2) and
      (LReveal.CaptureTreeReveal.Serial = LRequest.Serial + 1) and
      (LReveal.CaptureTreeReveal.Focus = ntrfTake), 'same-focus reveal is a fresh typed command');
    LCalls := LProbe.Calls;
    LRequest := LReveal.CaptureTreeReveal;
    LRefused := False;
    try
      LReveal.RevealTreeItem(NyxItem(LKey, 'missing'));
    except
      on E: ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LProbe.Calls = LCalls) and
      (LReveal.CaptureTreeReveal.Serial = LRequest.Serial), 'missing-row refusal keeps the admitted request exact');
    { Even a silent ordinary focus command is newer intent. It must withdraw
      old queued correction although it does not need another model notification. }
    LView.SetSelection([LOther], LLeaf, LOther);
    Check(not LReveal.CaptureTreeReveal.Item.Defined and (LProbe.Calls = LCalls),
      'a later no-op focus command invalidates old intent without extra notification');
    LReveal.RevealTreeItem(LLeaf);
    LView.ConfigureQuery(NyxCollectionQuery.Where(NyxWhere(NyxTextField('name')).EqualTo('Other')));
    Check(not LReveal.CaptureTreeReveal.Item.Defined and
      (RevealNyxTreeItem(LView, LLeaf) = ntrrFiltered), 'query displacement withdraws old intent and never relaxes itself');
    LView.ConfigureQuery(Default(TNyxCollectionQuery));
    Check(not LReveal.CaptureTreeReveal.Item.Defined, 'returning to an identical query cannot revive historical intent');
    LReveal.RevealTreeItem(LLeaf);
    LStore.Apply([NyxRemove(LLeaf), NyxInsert(1, LBefore.Item(LLeaf))], LBefore.Revision);
    Check(not LReveal.CaptureTreeReveal.Item.Defined, 'same-ID equal-value replacement retires old navigation authority');
    LProbe.Fail := True;
    LRefused := False;
    try
      LReveal.RevealTreeItem(LLeaf);
    except
      on E: ENyxCollectionNotification do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and LProbe.Atomic and LReveal.CaptureTreeReveal.Item.Defined,
      'observer failure reports an already committed reveal rather than rollback');
    LRequest := LReveal.CaptureTreeReveal;
    LToken := nil;
    LView := nil;
    LReveal := nil;
    Check((LRequest.Item.ID = CLeaf) and (LRequest.Serial > 0), 'copied request data survives its former view');
    LPeerReveal.RevealTreeItem(LLeaf);
    LTarget := TRevealTarget.Create(LPeer);
    LMount := LTarget;
    LTarget.Activate;
    Check(LTarget.Applied = 0, 'a newly attached physical generation never replays historical intent');
    LTarget.Fail := True;
    LRefused := False;
    try
      LPeerReveal.RevealTreeItem(LLeaf);
    except
      on E: ENyxCollectionNotification do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LTarget.Applied = 0), 'failed target work does not consume its proposed acknowledgement');
    LTarget.Fail := False;
    LMount.Refresh;
    Check((LTarget.Applied = 1) and (LTarget.Last.Serial = LPeerReveal.CaptureTreeReveal.Serial),
      'an ordinary retry still receives the failed request once');
    LMount.Refresh;
    Check(LTarget.Applied = 1, 'a successful request is not replayed on later ordinary refresh');
    LTarget.Defer := True;
    LPeerReveal.RevealTreeItem(LLeaf);
    Check(LTarget.Applied = 1, 'target deferral does not acknowledge unperformed viewport work');
    LTarget.Defer := False;
    LMount.Refresh;
    Check(LTarget.Applied = 2, 'deferred intent is available when its target becomes ready');
    LTarget.Hold(True);
    LPeerReveal.RevealTreeItem(LLeaf);
    LTarget.Hold(False);
    LMount.Refresh;
    Check(LTarget.Applied = 2, 'resuming an input epoch cannot revive intent issued during suspension');
    LPeerReveal.RevealTreeItem(LLeaf);
    Check(LTarget.Applied = 3, 'a new explicit request after resumption has fresh authority');
    LMount.Disconnect;
    LPeerReveal.RevealTreeItem(LLeaf);
    Check((LTarget.Applied = 3) and not LTarget.Last.Item.Defined,
      'a disconnected target cannot receive later commands from a retained view');
    LMount := nil;
  finally

    if LMount <> nil then
    begin
      LMount.Disconnect;
    end;
    LMount := nil;
    LToken := nil;
    LProbe.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-tree-reveal-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' Tree reveal model checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-tree-reveal-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
