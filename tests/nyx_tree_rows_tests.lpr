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
program nyx_tree_rows_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.state, nyx.collections, nyx.collections.view,
  nyx.collections.view.types, nyx.collections.selection, nyx.collections.query,
  nyx.collections.tree.rows
  {$ifdef PAS2JS}, Web{$endif};

type
  TFault = (rfNone, rfParent, rfCycle, rfChildren, rfLeafExpanded,
    rfDuplicate, rfMissing, rfStamp, rfSnapshot);
  { Alternative provider forwards the same public view contract, then injects
    one read inconsistency. No production class internals or mutation is used. }
  TAlternativeView = class(TInterfacedObject, INyxCollectionView, INyxTreeHierarchy)
  private
    FView: INyxCollectionView;
    FHierarchy: INyxTreeHierarchy;
    FFault: TFault;
    FStampReads: Integer;
  public
    constructor Create(const AView: INyxCollectionView; AFault: TFault);
    function ParentIndex(AIndex: Integer): Integer;
    function GetCommandSerial: Integer;
    function HasChildren(const AItem: TNyxItemRef): Boolean;
    function IsExpanded(const AItem: TNyxItemRef): Boolean;
    function VisibleItems: TNyxItemRefs;
    function GetSpec: TNyxCollectionViewSpec;
    function GetSnapshot: INyxCollectionSnapshot;
    function GetStore: INyxCollection;
    function GetProjection: TNyxCollectionProjection;
    function GetHasSelection: Boolean;
    function GetSelected: TNyxItemRef;
    function GetSelection: INyxCollectionSelection;
    function GetQuery: TNyxCollectionQuery;
    function ConfigureQuery(const APolicy: TNyxCollectionQuery): INyxCollectionView;
    function CellText(const AItem: TNyxItemRef; AColumn: Integer): TNyxText;
    procedure Select(const AItem: TNyxItemRef); overload;
    procedure Select(const AItem: TNyxItemRef; AAction: TNyxSelectionAction); overload;
    procedure SetSelection(const AItems: array of TNyxItemRef; const AFocus, AAnchor: TNyxItemRef);
    procedure SelectAll;
    procedure SelectRange(const AItem: TNyxItemRef; const AVisibleOrder: array of TNyxItemRef; AAdd: Boolean);
    procedure ClearSelection;
    procedure Edit(const AItem: TNyxItemRef; AColumn: Integer; const AValue: TNyxStateValue);
    procedure EditWire(const AItem: TNyxItemRef; AColumn: Integer; const AValue: TNyxText);
    procedure Apply(const AEdits: array of TNyxCollectionEdit; AExpectedRevision: Integer = -1);
    function Subscribe(AObserver: TNyxCollectionViewObserver): INyxCollectionViewSubscription;
    function SetExpanded(const AItem: TNyxItemRef; AExpanded: Boolean): INyxTreeHierarchy;
    function ExpandAll: INyxTreeHierarchy;
    function CollapseAll: INyxTreeHierarchy;
  end;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Tree rows: ' + AReason);
  end;
  Inc(GChecks);
end;

constructor TAlternativeView.Create(const AView: INyxCollectionView; AFault: TFault);
begin
  inherited Create;
  FView := AView;
  FHierarchy := NyxTreeHierarchy(AView);
  FFault := AFault;
end;

function TAlternativeView.ParentIndex(AIndex: Integer): Integer;
begin
  Result := FView.ParentIndex(AIndex);

  if (FFault = rfParent) and (AIndex = 1) then
  begin
    Result := FView.Snapshot.Count;
  end;

  if (FFault = rfCycle) and (AIndex = 1) then
  begin
    Result := 0;
  end;
end;

function TAlternativeView.GetCommandSerial: Integer;
begin
  Inc(FStampReads);
  Result := FHierarchy.CommandSerial;

  if (FFault = rfStamp) and (FStampReads > 1) then
  begin
    Inc(Result);
  end;
end;

function TAlternativeView.HasChildren(const AItem: TNyxItemRef): Boolean;
begin
  Result := FHierarchy.HasChildren(AItem);

  if FFault = rfChildren then
  begin
    Result := not Result;
  end;
end;

function TAlternativeView.IsExpanded(const AItem: TNyxItemRef): Boolean;
begin
  Result := FHierarchy.IsExpanded(AItem);

  if (FFault = rfLeafExpanded) and not FHierarchy.HasChildren(AItem) then
  begin
    Result := True;
  end;
end;

function TAlternativeView.VisibleItems: TNyxItemRefs;
begin
  Result := FHierarchy.VisibleItems;

  if (FFault = rfDuplicate) and (Length(Result) > 1) then
  begin
    Result[1] := Result[0];
  end;

  if (FFault = rfMissing) and (Length(Result) > 0) then
  begin
    SetLength(Result, Length(Result) - 1);
  end;
end;

function TAlternativeView.GetSpec: TNyxCollectionViewSpec;
begin
  Result := FView.Spec;
end;

function TAlternativeView.GetSnapshot: INyxCollectionSnapshot;
begin
  Result := FView.Snapshot;

  if (FFault = rfSnapshot) and (FStampReads > 0) then
  begin
    Result := nil;
  end;
end;

function TAlternativeView.GetStore: INyxCollection;
begin
  Result := FView.Store;
end;

function TAlternativeView.GetProjection: TNyxCollectionProjection;
begin
  Result := FView.Projection;
end;

function TAlternativeView.GetHasSelection: Boolean;
begin
  Result := FView.HasSelection;
end;

function TAlternativeView.GetSelected: TNyxItemRef;
begin
  Result := FView.Selected;
end;

function TAlternativeView.GetSelection: INyxCollectionSelection;
begin
  Result := FView.Selection;
end;

function TAlternativeView.GetQuery: TNyxCollectionQuery;
begin
  Result := FView.QueryPolicy;
end;

function TAlternativeView.ConfigureQuery(const APolicy: TNyxCollectionQuery): INyxCollectionView;
begin
  Result := FView.ConfigureQuery(APolicy);
end;

function TAlternativeView.CellText(const AItem: TNyxItemRef; AColumn: Integer): TNyxText;
begin
  Result := FView.CellText(AItem, AColumn);
end;

procedure TAlternativeView.Select(const AItem: TNyxItemRef);
begin
  FView.Select(AItem);
end;

procedure TAlternativeView.Select(const AItem: TNyxItemRef; AAction: TNyxSelectionAction);
begin
  FView.Select(AItem, AAction);
end;

procedure TAlternativeView.SetSelection(const AItems: array of TNyxItemRef; const AFocus, AAnchor: TNyxItemRef);
begin
  FView.SetSelection(AItems, AFocus, AAnchor);
end;

procedure TAlternativeView.SelectAll;
begin
  FView.SelectAll;
end;

procedure TAlternativeView.SelectRange(const AItem: TNyxItemRef; const AVisibleOrder: array of TNyxItemRef; AAdd: Boolean);
begin
  FView.SelectRange(AItem, AVisibleOrder, AAdd);
end;

procedure TAlternativeView.ClearSelection;
begin
  FView.ClearSelection;
end;

procedure TAlternativeView.Edit(const AItem: TNyxItemRef; AColumn: Integer; const AValue: TNyxStateValue);
begin
  FView.Edit(AItem, AColumn, AValue);
end;

procedure TAlternativeView.EditWire(const AItem: TNyxItemRef; AColumn: Integer; const AValue: TNyxText);
begin
  FView.EditWire(AItem, AColumn, AValue);
end;

procedure TAlternativeView.Apply(const AEdits: array of TNyxCollectionEdit; AExpectedRevision: Integer);
begin
  FView.Apply(AEdits, AExpectedRevision);
end;

function TAlternativeView.Subscribe(AObserver: TNyxCollectionViewObserver): INyxCollectionViewSubscription;
begin
  Result := FView.Subscribe(AObserver);
end;

function TAlternativeView.SetExpanded(const AItem: TNyxItemRef; AExpanded: Boolean): INyxTreeHierarchy;
begin
  Result := FHierarchy.SetExpanded(AItem, AExpanded);
end;

function TAlternativeView.ExpandAll: INyxTreeHierarchy;
begin
  Result := FHierarchy.ExpandAll;
end;

function TAlternativeView.CollapseAll: INyxTreeHierarchy;
begin
  Result := FHierarchy.CollapseAll;
end;


function Key: TNyxCollectionRef;
begin
  Result := NyxCollection('handbook');
end;

function Item(const AID: TNyxText): TNyxItemRef;
begin
  Result := NyxItem(Key, AID);
end;

procedure Run;
const
  CMoon: TNyxText = 'chapter-🌙';
var
  LStore: INyxCollection;
  LView: INyxCollectionView;
  LAlternative: INyxCollectionView;
  LHierarchy: INyxTreeHierarchy;
  LRows: INyxTreeRows;
  LRetained: INyxTreeRows;
  LRow: TNyxTreeRow;
  LFault: TFault;
  LRejected: Boolean;
  LSnapshot: INyxCollectionSnapshot;
  LStamp: Integer;
  LItems: array of TNyxCollectionItem;
  LIndex: Integer;
  LBeforeBytes: Integer;
begin
  LStore := NewNyxCollection(Key, NyxCollectionSchema
    .Text(NyxTextField('caption'), '').Text(NyxTextField('parent'), ''), [
    NyxCollectionItem(Item('alpha')).WithValue(NyxTextField('caption'), 'Alpha')
      .WithValue(NyxTextField('parent'), 'parent'),
    NyxCollectionItem(Item('parent')).WithValue(NyxTextField('caption'), 'Parent'),
    NyxCollectionItem(Item('second')).WithValue(NyxTextField('caption'), 'Second root'),
    NyxCollectionItem(Item('zebra')).WithValue(NyxTextField('caption'), 'Zebra')
      .WithValue(NyxTextField('parent'), 'parent'),
    NyxCollectionItem(Item(CMoon)).WithValue(NyxTextField('caption'), 'Moon chapter')
      .WithValue(NyxTextField('parent'), 'alpha')]);
  LView := NewNyxCollectionView(LStore, NyxCollectionView(Key)
    .Column(NyxTextField('caption'), 'Section').Parent(NyxTextField('parent'))
    .Selection(nsmMultiple), cpTree);
  LHierarchy := NyxTreeHierarchy(LView);
  LRows := NyxTreeRows(LView);
  Check((LRows.Count = 2) and (LRows.RowAt(0).Item.ID = 'parent'), 'closed roots use actual preorder');
  LRow := LRows.RowAt(0);
  Check(LRow.Defined and (LRow.Level = 1) and (LRow.Position = 1) and
    (LRow.Size = 2) and not LRow.Parent.Defined and LRow.HasChildren and
    not LRow.Expanded, 'root metadata is copied and one-based');
  Check((LRows.IndexOf(Item('alpha')) = -1) and
    (LRows.IndexOf(NyxItem(NyxCollection('foreign'), 'parent')) = -1) and
    (LRows.IndexOf(Default(TNyxItemRef)) = -1), 'hidden/foreign/undefined lookup is absent');
  LHierarchy.SetExpanded(Item('parent'), True).SetExpanded(Item('alpha'), True);
  LRows := NyxTreeRows(LView);
  Check(LRows.Count = 5, 'disclosed projection includes complete hierarchy');
  LRow := LRows.RowAt(2);
  Check((LRow.Item.ID = CMoon) and (LRow.Parent.ID = 'alpha') and
    (LRow.Level = 3) and (LRow.Position = 1) and (LRow.Size = 1) and
    not LRow.HasChildren and not LRow.Expanded, 'deep Unicode leaf retains exact parent metadata');
  Check((LRows.RowAt(3).Item.ID = 'zebra') and (LRows.RowAt(3).Position = 2) and
    (LRows.RowAt(3).Size = 2), 'siblings use logical group size rather than viewport count');
  LRow := Default(TNyxTreeRow);
  Check(LRows.RowAt(2).Defined, 'changing a copied row cannot mutate its projection');
  LRetained := LRows;
  LHierarchy.CollapseAll;
  LRows := NyxTreeRows(LView);
  Check((LRows.Count = 2) and (LRetained.Count = 5) and LRetained.RowAt(0).Expanded,
    'later collapse preserves retained disclosure and rows');
  LRejected := False;
  try
    LRows.RowAt(2);
  except
    on ENyxCollection do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'ordinal outside disclosed rows refuses');
  LSnapshot := LView.Snapshot;
  LStamp := LHierarchy.CommandSerial;
  for LFault := rfNone to High(TFault) do
  begin
    LAlternative := TAlternativeView.Create(LView, LFault);
    LRejected := False;
    try
      LRows := NyxTreeRows(LAlternative);
    except
      on ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected = (LFault <> rfNone), 'alternative provider admission/refusal follows exact read evidence');
    Check((LView.Snapshot = LSnapshot) and (LHierarchy.CommandSerial = LStamp),
      'read refusal never mutates the underlying view');
    LAlternative := nil;
  end;
  LHierarchy.ExpandAll;
  LView.ConfigureQuery(NyxCollectionQuery.OrderBy(NyxTextField('caption'), nsdDescending));
  LRows := NyxTreeRows(LView);
  Check((LRows.RowAt(0).Item.ID = 'second') and
    (LRows.RowAt(LRows.IndexOf(Item('zebra'))).Position = 1), 'sorted roots/siblings retain current metadata');
  LBeforeBytes := LStore.Snapshot.DataBytes;
  LView.ConfigureQuery(NyxCollectionQuery.Where(NyxWhere(NyxTextField('caption')).StartsWith('Moon')));
  LRows := NyxTreeRows(LView);
  Check((LRows.Count = 3) and (LRows.Snapshot.DataBytes = LBeforeBytes) and
    (LRows.RowAt(0).Size = 1), 'filtered projection retains ancestors and full source accounting');
  LView := nil;
  LHierarchy := nil;
  LStore := nil;
  Check(LRetained.RowAt(2).Item.ID = CMoon, 'projection survives view/store retirement');
  LRetained := nil;
  SetLength(LItems, 4096);
  for LIndex := 0 to High(LItems) do
  begin
    LItems[LIndex] := NyxCollectionItem(Item('node-' + IntToStr(LIndex)))
      .WithValue(NyxTextField('caption'), 'Section ' + IntToStr(LIndex));

    if LIndex > 0 then
    begin
      LItems[LIndex] := LItems[LIndex].WithValue(NyxTextField('parent'), 'node-' + IntToStr(LIndex - 1));
    end;
  end;
  LStore := NewNyxCollection(Key, NyxCollectionSchema
    .Text(NyxTextField('caption'), '').Text(NyxTextField('parent'), ''), LItems);
  LView := NewNyxCollectionView(LStore, NyxCollectionView(Key)
    .Column(NyxTextField('caption'), 'Section').Parent(NyxTextField('parent')), cpTree);
  NyxTreeHierarchy(LView).ExpandAll;
  LRows := NyxTreeRows(LView);
  Check(LRows.Count = 4096, 'deep source is fully disclosed without recursion');
  for LIndex := 0 to LRows.Count - 1 do
  begin
    LRow := LRows.RowAt(LIndex);
    Check((LRow.Level = LIndex + 1) and (LRow.Size = 1) and (LRow.Position = 1) and
      (LRows.IndexOf(LRow.Item) = LIndex), 'deep ordinal/level/lookup remain exact');
  end;
  LRows := nil;
  LView := nil;
  LStore := nil;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-tree-rows', 'passed');
    document.body.setAttribute('data-tree-row-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' portable tree-row projection checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-tree-rows', 'failed');
      document.body.setAttribute('data-event-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
