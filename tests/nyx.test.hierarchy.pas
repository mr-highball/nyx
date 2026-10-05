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
unit nyx.test.hierarchy;

{$mode delphi}{$H+}{$codepage utf8}

interface

{ Portable ownership/identity/selection qualification for the actual shell tree.
  Rendered input/geometry remain the separate target-control consumers. }
function RunNyxStudioHierarchyTests: Integer;

implementation

uses
  Classes, SysUtils, nyx.text, nyx.types, nyx.behavior, nyx.model, nyx.controls,
  nyx.data,
  nyx.collections, nyx.collections.view, nyx.collections.view.types,
  nyx.studio.hierarchy, nyx.studio.session, nyx.studio.projects,
  nyx.events, nyx.scheduler;

type
  { Borrowed method receiver deliberately released while queued work is retained;
    cancellation must prevent the event callback from consulting it afterward. }
  TNyxHierarchyReceiver = class
  public
    Calls: Integer;
    procedure Changed(const AEvent: TNyxEventInfo);
  end;

procedure TNyxHierarchyReceiver.Changed(const AEvent: TNyxEventInfo);
begin
  Inc(Calls);
end;

function RunNyxStudioHierarchyTests: Integer;
var
  LSession: TNyxStudioSession;
  LShell: TNyxDocument;
  LTree: INyxTree;
  LView: INyxCollectionView;
  LContext: INyxCollectionContext;
  LBefore: TNyxText;
  LName: TNyxText;
  LIndex: Integer;
  LEvent: TNyxEventInfo;
  LChanged: Boolean;
  LRejected: Boolean;
  LNode: TNyxNode;
  LForeign: INyxCollection;
  LForeignView: INyxCollectionView;
  LEvents: INyxEvents;
  LToken: INyxEventSubscription;
  LReceiver: TNyxHierarchyReceiver;
  LExecutions: TNyxExecutions;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise Exception.Create('Studio hierarchy: ' + AReason);
    end;
    Inc(Result);
  end;

begin
  Result := 0;
  LSession := TNyxStudioSession.Create;
  LShell := TNyxDocument.Create;
  LReceiver := TNyxHierarchyReceiver.Create;
  try
    LName := '';
    for LIndex := 1 to 128 do
    begin
      LName := LName + TNyxText('界');
    end;
    LSession.ActiveView.Add(NewNyxLabel(LName).WithText('Private identity qualification'));
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    LTree := BuildNyxStudioHierarchy(LShell, LSession);
    Check((LTree.Node.Kind = NyxKindName(nkTree)) and (LTree.Node.Prop('height') = '280') and
      LTree.Node.HasCollectionView, 'The shell consumes a bounded specialized public tree');
    LContext := NewNyxCollectionContext(LShell.Collections);
    LView := NewNyxCollectionView(LContext.Resolve(LTree.Node.CollectionView, ''),
      LTree.Node.CollectionView, cpTree);
    Check(LView.Snapshot.Count = 12, 'Every active-view descendant owns one row without expanded renderer parts');
    Check(LView.Snapshot.IndexOf(NyxItem(LView.Spec.Key, LName)) >= 0,
      'A maximum-length Unicode ID remains exact item identity');
    Check(LView.CellText(NyxItem(LView.Spec.Key, LName), 0) = TNyxText('label / ') + LName,
      'A maximum-length Unicode caption survives independent collection ownership');
    SelectNyxStudioHierarchy(LView, LSession);
    Check(LView.Selection.Focus.ID = LSession.SelectedID, 'Initialization selects the existing component');
    LEvent := Default(TNyxEventInfo);
    LEvent.Trigger := ntSelectionChange;
    LEvent.Name := NyxEvent(NyxTriggerName(ntSelectionChange));
    LEvent.Value := NyxNull;
    LEvent.HasCollectionSelection := True;
    LEvent.Selection := LView.Selection.Snapshot;
    Check(RouteNyxStudioHierarchy(LSession, LTree.Node, LEvent, LChanged) and not LChanged,
      'Initialization is a recognized no-op and cannot request recursive painting');
    LView.Select(NyxItem(LView.Spec.Key, LName));
    LEvent.Selection := LView.Selection.Snapshot;
    Check(RouteNyxStudioHierarchy(LSession, LTree.Node, LEvent, LChanged) and LChanged and
      (LSession.SelectedID = LName), 'A typed tree event selects its exact authored identity');
    Check(not LSession.CanUndo and (EncodeNyxProject(LSession.ProjectSnapshot) = LBefore),
      'Hierarchy navigation changes no paired files or accepted history');

    LEvents := NewNyxEvents;
    LToken := SubscribeNyxStudioHierarchy(LEvents, LReceiver.Changed);
    LExecutions := LEvents.Dispatch(LEvent, NyxStudioHierarchyID, NyxStudioHierarchyID);
    Check((Length(LExecutions) = 1) and (LReceiver.Calls = 0) and
      (LExecutions[0].Status = nesPending),
      'Typed UI-queue selection cannot replace a shell inside its originating notification');
    LEvents.CancelPending;
    Check(LExecutions[0].Cancelled and LToken.Active,
      'Shell replacement revokes queued selection while retaining the next shell registration');
    LExecutions := LEvents.Dispatch(LEvent, NyxStudioHierarchyID, NyxStudioHierarchyID);
    LToken.Cancel;
    Check(LExecutions[0].Cancelled and not LToken.Active,
      'Receiver retirement revokes pending typed selection before releasing its borrowed method');
    FreeAndNil(LReceiver);
    LEvents.Close;
    {$ifndef PAS2JS}
    { Retire the native scheduler's cancelled couriers before leak accounting.
      They own their snapshots but must never consult the released receiver. }
    CheckSynchronize;
    {$endif}

    LForeign := NewNyxCollection(NyxCollection('foreignHierarchy'),
      NyxCollectionSchema.Text(NyxTextField('caption'), ''),
      [NyxCollectionItem(NyxItem(NyxCollection('foreignHierarchy'), LName))
        .WithValue(NyxTextField('caption'), 'Foreign')]);
    LForeignView := NewNyxCollectionView(LForeign,
      NyxCollectionView(NyxCollection('foreignHierarchy'))
        .Column(NyxTextField('caption'), 'Component'), cpList);
    LForeignView.Select(NyxItem(NyxCollection('foreignHierarchy'), LName));
    LEvent.Selection := LForeignView.Selection.Snapshot;
    LRejected := False;
    try
      RouteNyxStudioHierarchy(LSession, LTree.Node, LEvent, LChanged);
    except
      on ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.SelectedID = LName),
      'A same-spelled foreign selection cannot retarget Studio');

    LEvent.Selection := LView.Selection.Snapshot;
    LNode := LSession.ActiveView.Extract(LSession.ActiveView.Count - 1);
    try
      LRejected := False;
      try
        RouteNyxStudioHierarchy(LSession, LTree.Node, LEvent, LChanged);
      except
        on ENyxModel do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected, 'A retained event for a removed component refuses before changing selection');
    finally
      LSession.ActiveView.Add(LNode);
    end;
    LEvent.HasCollectionSelection := False;
    LRejected := False;
    try
      RouteNyxStudioHierarchy(LSession, LTree.Node, LEvent, LChanged);
    except
      on ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'A generic selection event without owned item context refuses');

    LTree := nil;
    FreeAndNil(LShell);
    FreeAndNil(LSession);
    Check(LView.CellText(NyxItem(LView.Spec.Key, LName), 0) = TNyxText('label / ') + LName,
      'A retained runtime view owns its captions after shell/document retirement');
  finally

    if LToken <> nil then
    begin
      LToken.Cancel;
    end;
    LToken := nil;

    if LEvents <> nil then
    begin
      LEvents.Close;
    end;
    LExecutions := nil;
    LEvents := nil;
    LReceiver.Free;
    LForeignView := nil;
    LForeign := nil;
    LView := nil;
    LContext := nil;
    LTree := nil;
    LShell.Free;
    LSession.Free;
  end;
end;

end.
