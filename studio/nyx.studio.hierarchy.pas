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
unit nyx.studio.hierarchy;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.types, nyx.model, nyx.controls, nyx.behavior,
  nyx.collections, nyx.collections.view, nyx.events, nyx.scheduler,
  nyx.studio.session;

type
  { Borrowed UI receiver. Its subscription must be cancelled before destroying
    the receiver; no node, shell or Studio owner is retained by this callback. }
  TNyxStudioHierarchyEvent = procedure(const AEvent: TNyxEventInfo) of object;

const
  { Stable editor identity; authored component IDs remain item data. }
  NyxStudioHierarchyID = 'studio-hierarchy';

{ Shell-owned typed defaults and an ordinary public Nyx tree replace a separate
  native widget per component. The renderer owns one bounded tree viewport;
  collection rows retain exact IDs/parents/captions without retaining document
  nodes. The shell is independent of the designed project and its history. }
function BuildNyxStudioHierarchy(AShell: TNyxDocument;
  ASession: TNyxStudioSession): INyxTree;

{ Restore runtime selection after mounting the new shell. This may emit a normal
  collection selection event; the route below recognizes an unchanged selection
  without requesting another paint. Views/session are borrowed for this call. }
procedure SelectNyxStudioHierarchy(const AView: INyxCollectionView;
  ASession: TNyxStudioSession);

{ Consume only this tree's typed single-selection snapshot. Unknown/stale/foreign
  items reject before changing Studio selection. Changed distinguishes a real
  user command from initialization/no-op; neither creates project Undo history. }
function RouteNyxStudioHierarchy(ASession: TNyxStudioSession; ANode: TNyxNode;
  const AEvent: TNyxEventInfo; out AChanged: Boolean): Boolean;

{ Register through the public typed event stream. UI-queue delivery finishes the
  originating tree notification before Studio may replace its shell. Renderer
  generation cancellation suppresses events from a retired shell. The caller
  retains/cancels the returned subscription; dropping it alone does not cancel. }
function SubscribeNyxStudioHierarchy(const AEvents: INyxEvents;
  AReceiver: TNyxStudioHierarchyEvent): INyxEventSubscription;

implementation

uses
  nyx.collections.view.types;

type
  TNyxStudioHierarchyCallback = class(TNyxEventCallback)
  private
    FReceiver: TNyxStudioHierarchyEvent;
  public
    constructor Create(AReceiver: TNyxStudioHierarchyEvent);
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;

constructor TNyxStudioHierarchyCallback.Create(AReceiver: TNyxStudioHierarchyEvent);
begin
  inherited Create;
  FReceiver := AReceiver;
end;

procedure TNyxStudioHierarchyCallback.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin

  if not AExecution.Cancelled then
  begin
    FReceiver(AEvent);
  end;
end;

function SubscribeNyxStudioHierarchy(const AEvents: INyxEvents;
  AReceiver: TNyxStudioHierarchyEvent): INyxEventSubscription;
begin

  if (AEvents = nil) or not Assigned(AReceiver) then
  begin
    raise ENyxModel.Create('Studio hierarchy requires an event router and receiver');
  end;
  Result := AEvents.OnSelectionChange(NyxControlEvents(NyxStudioHierarchyID))
    .Policy(neUIQueue).Subscribe(TNyxStudioHierarchyCallback.Create(AReceiver));
end;

function HierarchyKey: TNyxCollectionRef;
begin
  Result := NyxCollection('studioHierarchy');
end;

function CountItems(ANode: TNyxNode): Integer;
var
  LIndex: Integer;
begin
  Result := 1;
  for LIndex := 0 to ANode.Count - 1 do
  begin
    Inc(Result, CountItems(ANode.Children[LIndex]));
  end;
end;

function BuildNyxStudioHierarchy(AShell: TNyxDocument;
  ASession: TNyxStudioSession): INyxTree;
var
  LItems: array of TNyxCollectionItem;
  LCount: Integer;
  LIndex: Integer;
  LSpec: TNyxCollectionViewSpec;

  procedure AppendItem(ANode: TNyxNode; const AParent: TNyxText);
  var
    LChild: Integer;
  begin
    LItems[LIndex] := NyxCollectionItem(NyxItem(HierarchyKey, ANode.ID))
      .WithValue(NyxTextField('caption'), TNyxText(ANode.Kind) +
        TNyxText(' / ') + ANode.ID)
      .WithValue(NyxTextField('parent'), AParent);
    Inc(LIndex);
    for LChild := 0 to ANode.Count - 1 do
    begin
      AppendItem(ANode.Children[LChild], ANode.ID);
    end;
  end;

begin

  if (AShell = nil) or (ASession = nil) then
  begin
    raise ENyxModel.Create('A hierarchy requires its independent shell and design session');
  end;
  LCount := 0;

  if ASession.ActiveView <> nil then
  begin
    LCount := CountItems(ASession.ActiveView);
  end;
  SetLength(LItems, LCount);
  LIndex := 0;

  if ASession.ActiveView <> nil then
  begin
    AppendItem(ASession.ActiveView, '');
  end;
  AShell.Collections.Define(HierarchyKey,
    NyxCollectionSchema.Text(NyxTextField('caption'), '')
      .Text(NyxTextField('parent'), ''), LItems);
  LSpec := NyxCollectionView(HierarchyKey)
    .Column(NyxTextField('caption'), 'Component')
    .Parent(NyxTextField('parent'));
  Result := NewNyxTree(NyxStudioHierarchyID);
  Result.Configure.Height(280).Hint('Select a component in the active view.').Done;
  Result.Binds.Collection(LSpec).Done;
end;

procedure SelectNyxStudioHierarchy(const AView: INyxCollectionView;
  ASession: TNyxStudioSession);
var
  LItem: TNyxItemRef;
begin

  if (AView = nil) or (ASession = nil) then
  begin
    raise ENyxModel.Create('Hierarchy selection requires its mounted view and session');
  end;

  if ASession.SelectedID = '' then
  begin
    Exit;
  end;
  LItem := NyxItem(HierarchyKey, ASession.SelectedID);

  if AView.Snapshot.IndexOf(LItem) >= 0 then
  begin
    AView.Select(LItem);
  end;
end;

function RouteNyxStudioHierarchy(ASession: TNyxStudioSession; ANode: TNyxNode;
  const AEvent: TNyxEventInfo; out AChanged: Boolean): Boolean;
var
  LItem: TNyxItemRef;
begin
  Result := (ANode <> nil) and (ANode.ID = NyxStudioHierarchyID) and
    (AEvent.Trigger = ntSelectionChange);
  AChanged := False;

  if not Result then
  begin
    Exit;
  end;

  if (ASession = nil) or not AEvent.HasCollectionSelection or
    not AEvent.Selection.Defined or (AEvent.Selection.Count <> 1) or
    not AEvent.Selection.Focus.Defined then
  begin
    raise ENyxModel.Create('Hierarchy selection requires one exact component identity');
  end;
  LItem := AEvent.Selection.Focus;

  if (LItem.Collection.Name <> HierarchyKey.Name) or
    not AEvent.Selection.Contains(LItem) or (ASession.ActiveView = nil) or
    (ASession.ActiveView.Find(LItem.ID) = nil) then
  begin
    raise ENyxModel.Create('Hierarchy selection belongs to a stale or different view');
  end;

  if ASession.SelectedID = LItem.ID then
  begin
    Exit;
  end;
  ASession.Select(LItem.ID);
  AChanged := True;
end;

end.
