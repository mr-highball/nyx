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
unit nyx.collections.browser;

{$mode delphi}{$H+}
{$modeswitch externalclass}
{$codepage utf8}

interface

uses
  JS,
  Web,
  nyx.collections.view,
  nyx.collections.mount;

{ Adapter boundary. Host must be the list/table/tree element projected by Nyx.
  The returned mount borrows it; its renderer disconnects before removing DOM.
  Rows are reused by stable item identity, including during reorder. Lists expose
  selection; tables/tree captions offer column editing where cmEditable permits.
  Lists/tables realize a measured viewport window, retaining focused editors and exact
  offscreen drafts. Same-source refresh visits owned rows and the incoming window;
  source changes still remap logical slots. Trees still materialize rows, and
  full update/frame/memory budgets remain open. }
function MountNyxBrowserCollection(AHost: TJSHTMLElement;
  const AView: INyxCollectionView): INyxCollectionMount;

implementation

uses
  SysUtils,
  Math,
  nyx.text,
  nyx.state,
  nyx.collections,
  nyx.collections.view.types,
  nyx.typeahead,
  nyx.collections.grid,
  nyx.collections.columns,
  nyx.collections.query,
  nyx.collections.query.controls,
  nyx.layout.flow,
  nyx.collections.window,
  nyx.collections.selection,
  nyx.collections.tree.rows;

type
  { Typed bridge to standard DOM focus options, missing from older Web units. }
  TFocusOptions = class external name 'Object' (TJSObject)
    preventScroll: Boolean;
  end;
  TFocusable = class external name 'HTMLElement' (TJSHTMLElement)
    procedure focus(AOptions: TFocusOptions); reintroduce;
  end;
  { CSSOM scroll coordinates are doubles. Older Web declarations narrow the
    window method to integers; this bridge preserves fractional host offsets. }
  TScrollableWindow = class external name 'Window' (TJSWindow)
    scrollXPixels: Double; external name 'scrollX';
    scrollYPixels: Double; external name 'scrollY';
    procedure scrollTo(AX, AY: Double); reintroduce;
  end;
  TScrollableElement = class external name 'HTMLElement' (TJSHTMLElement)
    scrollTopPixels: Double; external name 'scrollTop';
    scrollLeftPixels: Double; external name 'scrollLeft';
  end;
  { Listener capture is part of DOM callback identity. Some Web versions omit
    the remove overload; teardown must use the same capture flag as admission. }
  TCapturedDocument = class external name 'Document' (TJSDocument)
    procedure removeEventListener(AType: String; AListener: JSValue;
      ACapture: Boolean); reintroduce;
  end;
  TNyxDetails = class external name 'HTMLDetailsElement' (TJSHTMLElement)
    open: Boolean;
  end;

  TBrowserMount = class;
  TBrowserRow = class
  private
    FOwner: TBrowserMount;
    FRef: TNyxItemRef;
    FElement: TJSHTMLElement;
    FChildren: TJSHTMLElement;
    { Exact owned data-cell surfaces. Editors remain children with a separate
      editing mode; row membership is independent of this physical cursor. }
    FCells: array of TJSHTMLElement;
    FLabels: array of TJSHTMLElement;
    FInputs: array of TJSHTMLInputElement;
    FInitialized: Boolean;
    function Click(AEvent: TJSMouseEvent): Boolean;
    function DoubleClick(AEvent: TJSMouseEvent): Boolean;
    function Change(AEvent: TEventListenerEvent): Boolean;
    function Toggle(AEvent: TEventListenerEvent): Boolean;
  public
    constructor Create(AOwner: TBrowserMount; const ARef: TNyxItemRef);
    destructor Destroy; override;
    procedure Sync(AValuesChanged: Boolean);
    { Pure comparison with accepted scalar display. Detached draft rows retain
      their owned controls until normalization, removal or teardown. }
    function HasDraft: Boolean;
  end;

  TBrowserMount = class(TNyxCollectionMountBase)
  private
    FHost: TJSHTMLElement;
    FBody: TJSHTMLElement;
    FRows: array of TBrowserRow;
    FRendered: INyxCollectionSnapshot;
    { Immutable hierarchy reads retain only snapshot/metadata. The mount owns
      the view association, so cache invalidation compares its snapshot/stamp
      without a projection-to-view retention cycle. }
    FTreeRows: INyxTreeRows;
    FTreeGeometryRows: INyxTreeRows;
    FFocusedColumn: Integer;
    { Stable header controls survive row refresh and own no per-row closures.
      The delegated handlers run after the renderer's canonical host events. }
    FHeaderCells: array of TJSHTMLElement;
    FHeaderLabels: array of TJSHTMLElement;
    FHeaderSorts: array of TJSHTMLElement;
    FHeaderFilters: array of TJSHTMLElement;
    procedure RefreshQueryHeaders;
    procedure QueryColumn(AColumn: Integer; AFilter, AAppend: Boolean);
    function QueryClick(AEvent: TEventListenerEvent): Boolean;
    function QueryKey(AEvent: TJSKeyboardEvent): Boolean;
    { Edit ownership is runtime-only and uses a stable item/column tuple.
      Hidden editors retain draft/caret identity without becoming extra Tab or
      accessibility entries. No document, source or row object is retained. }
    FEditingItem: TNyxItemRef;
    FEditingColumn: Integer;
    function EnterCellEditor(const AItem: TNyxItemRef; AColumn: Integer): Boolean;
    function CellEditing(const AItem: TNyxItemRef; AColumn: Integer): Boolean;
    function EditorFocusChanged(AEvent: TEventListenerEvent): Boolean;
    FColumnNatural: TNyxFlowSizes;
    FColumnWidths: TNyxFlowSizes;
    FColumnFont: String;
    FColumnAvailable: Integer;
    procedure SizeTableColumns;
    { Numerical geometry is owned; row slots are logical indices and may be nil.
      Only realized rows and explicit unfinished drafts own controls. }
    FGeometry: TNyxCollectionRowGeometry;
    { Sorted logical indices of owned flat rows, including detached table drafts.
      Scroll/selection refresh visits these rows and the new viewport only;
      the dataset-sized slot map remains private for direct keyboard lookup. }
    FResidentRows: array of Integer;
    FSpacers: array of TJSHTMLElement;
    FForcedItem: TNyxItemRef;
    { Keep a keyboard destination revealed while new neighboring rows acquire
      measured heights. Manual host scrolling cancels this short-lived intent;
      stable geometry consumes it. Values retain no DOM or document owner. }
    FRevealItem: TNyxItemRef;
    FRevealScrollTop: Double;
    FWindowFrame: Integer;
    FWindowQueued: Boolean;
    FWindowObserver: TJSHTMLResizeObserver;
    FWindowAncestors: array of TJSHTMLElement;
    function ScrollChanged(AEvent: TEventListenerEvent): Boolean;
    function WindowChanged(AEvent: TEventListenerEvent): Boolean;
    procedure RowBoundsChanged(AEntries: TJSHTMLResizeObserverEntryArray;
      AObserver: TJSHTMLResizeObserver);
    procedure QueueWindow;
    procedure WindowFrame(ATime: Double);
    procedure ObserveWindowAncestors;
    procedure RowViewport(out AOffset, AExtent: Double);
    procedure RenderFlatWindow;
    { Disclosed rows use viewport geometry; retained ancestors own their real
      child groups. Hidden drafts retain controls without becoming tree entries. }
    procedure RenderTreeWindow;
    function TreeRows: INyxTreeRows;
    { Realizes a logical destination before navigation publishes selection.
      It does not scroll or rewrite source/history. Normal focus reveals it. }
    procedure EnsureRow(AIndex: Integer);
    { Reveal within the list's own scroll plane before focus. An already-focused
      retained option may have moved during a query; DOM focus alone is a no-op.
      The last borrowed focus call may retire this mount through application UI. }
    procedure FocusListOption(AOption: TJSHTMLElement);
    procedure RevealListOption(AOption: TJSHTMLElement);
    function GridKey(AEvent: TJSKeyboardEvent; const AOrder: TNyxItemRefs;
      ARow: Integer): Boolean;
    function Key(AEvent: TJSKeyboardEvent): Boolean;
    function VisibleOrder: TNyxItemRefs;
    procedure Gesture(const AItem: TNyxItemRef; AShift, AControl: Boolean;
      AFocusOnly: Boolean = False);
  protected
    procedure RenderDataset; override;
    procedure DetachTarget; override;
  public
    constructor Create(AHost: TJSHTMLElement; const AView: INyxCollectionView);
  end;

function NewElement(const ATag: TNyxText): TJSHTMLElement;
begin
  Result := TJSHTMLElement(document.createElement(ATag));
end;

function TBrowserMount.TreeRows: INyxTreeRows;
var
  LHierarchy: INyxTreeHierarchy;
begin
  LHierarchy := NyxTreeHierarchy(FView);

  if (FTreeRows = nil) or (FTreeRows.Snapshot <> FView.Snapshot) or
    (FTreeRows.CommandSerial <> LHierarchy.CommandSerial) then
  begin
    FTreeRows := NyxTreeRows(FView);
  end;
  Result := FTreeRows;
end;

constructor TBrowserMount.Create(AHost: TJSHTMLElement;
  const AView: INyxCollectionView);
var
  LHeader: TJSHTMLElement;
  LRow: TJSHTMLElement;
  LCell: TJSHTMLElement;
  LIndex: Integer;
begin
  inherited Create(AView);

  if AHost = nil then
  begin
    raise ENyxCollection.Create('Browser collection requires a host');
  end;

  if ((AView.Projection = cpList) and (LowerCase(AHost.tagName) <> 'ul')) or
    ((AView.Projection = cpTable) and (LowerCase(AHost.tagName) <> 'table')) or
    ((AView.Projection = cpTree) and (LowerCase(AHost.tagName) <> 'div')) then
  begin
    raise ENyxCollection.Create('Browser collection host differs from its projection');
  end;
  FHost := AHost;
  FHost.textContent := '';

  if AView.Projection = cpTable then
  begin
    FHost.setAttribute('role', 'grid');
    LHeader := NewElement('thead');
    LRow := NewElement('tr');
    LRow.setAttribute('role', 'row');
    LRow.setAttribute('aria-rowindex', '1');
    LHeader.appendChild(LRow);
    SetLength(FHeaderCells, AView.Spec.Count);
    SetLength(FHeaderLabels, AView.Spec.Count);
    SetLength(FHeaderSorts, AView.Spec.Count);
    SetLength(FHeaderFilters, AView.Spec.Count);
    for LIndex := 0 to AView.Spec.Count - 1 do
    begin
      LCell := NewElement('th');
      LCell.setAttribute('role', 'columnheader');
      LCell.setAttribute('aria-colindex', IntToStr(LIndex + 1));
      LCell.setAttribute('scope', 'col');
      FHeaderCells[LIndex] := LCell;
      FHeaderLabels[LIndex] := NewElement('span');
      FHeaderLabels[LIndex].textContent := AView.Spec.ColumnAt(LIndex).Title;
      LCell.appendChild(FHeaderLabels[LIndex]);
      FHeaderSorts[LIndex] := NewElement('button');
      FHeaderSorts[LIndex].className := 'nyx-button';
      FHeaderSorts[LIndex].setAttribute('type', 'button');
      FHeaderSorts[LIndex].style.setProperty('padding', '4px 6px');
      FHeaderSorts[LIndex].style.setProperty('white-space', 'normal');
      FHeaderSorts[LIndex].setAttribute('data-nyx-query-sort', IntToStr(LIndex));
      FHeaderSorts[LIndex].style.setProperty('display', 'none');
      LCell.appendChild(FHeaderSorts[LIndex]);
      FHeaderFilters[LIndex] := NewElement('button');
      FHeaderFilters[LIndex].className := 'nyx-button';
      FHeaderFilters[LIndex].textContent := 'Filter';
      FHeaderFilters[LIndex].setAttribute('type', 'button');
      FHeaderFilters[LIndex].style.setProperty('padding', '4px 6px');
      FHeaderFilters[LIndex].setAttribute('data-nyx-query-column', IntToStr(LIndex));
      FHeaderFilters[LIndex].setAttribute('aria-label', 'Filter ' + AView.Spec.ColumnAt(LIndex).Title);
      FHeaderFilters[LIndex].setAttribute('aria-haspopup', 'dialog');
      FHeaderFilters[LIndex].style.setProperty('display', 'none');
      LCell.appendChild(FHeaderFilters[LIndex]);
      LRow.appendChild(LCell);
    end;
    FHost.appendChild(LHeader);
    FBody := NewElement('tbody');
    FHost.appendChild(FBody);
  end
  else
  begin
    FBody := FHost;
    FHost.setAttribute('role', 'listbox');

    if AView.Projection = cpTree then
    begin
      FHost.setAttribute('role', 'tree');
      FHost.style.setProperty('overflow', 'auto');
    end;
  end;
  FHost.setAttribute('aria-multiselectable', 'false');

  if AView.Spec.SelectionMode = nsmMultiple then
  begin
    FHost.setAttribute('aria-multiselectable', 'true');
  end;
  { Nyx's canonical keyboard listener was installed by the renderer first.
    Delegating row selection at this host lets its callbacks consume the key
    before the collection applies the default action. }
  FHost.addEventListener('keydown', @QueryKey);
  FHost.addEventListener('click', @QueryClick);
  FHost.addEventListener('keydown', @Key);

  if AView.Projection in [cpList, cpTable, cpTree] then
  begin
    FGeometry := TNyxCollectionRowGeometry.Create(0, 32);
    document.addEventListener('scroll', @ScrollChanged, True);
    window.addEventListener('resize', @WindowChanged);
    FHost.addEventListener('focusin', @WindowChanged);

    if AView.Projection in [cpTable, cpTree] then
    begin
      FHost.addEventListener('focusin', @EditorFocusChanged);
      FHost.addEventListener('focusout', @EditorFocusChanged);
    end;

    if Assigned(window['ResizeObserver']) then
    begin
      FWindowObserver := TJSHTMLResizeObserver.new(@RowBoundsChanged);
      FWindowObserver.observe(FHost);
    end;
    QueueWindow;
  end;
end;

constructor TBrowserRow.Create(AOwner: TBrowserMount; const ARef: TNyxItemRef);
var
  LCell: TJSHTMLElement;
  LCaption: TJSHTMLElement;
  LColumn: TNyxCollectionColumn;
  LColumns: Integer;
  LIndex: Integer;
begin
  inherited Create;
  FOwner := AOwner;
  FRef := ARef;
  LColumns := 1;

  if FOwner.FView.Projection = cpTable then
  begin
    FElement := NewElement('tr');
    FElement.setAttribute('role', 'row');
    LColumns := FOwner.FView.Spec.Count;
  end
  else if FOwner.FView.Projection = cpTree then
  begin
    FElement := NewElement('details');
    FElement.setAttribute('role', 'treeitem');
    LCaption := NewElement('summary');
    LCaption.setAttribute('tabindex', '-1');
    FElement.addEventListener('toggle', @Toggle);
    FElement.appendChild(LCaption);
    FChildren := NewElement('div');
    FChildren.setAttribute('role', 'group');
    FChildren.style.setProperty('padding-left', '16px');
    FElement.appendChild(FChildren);
  end
  else
  begin
    FElement := NewElement('li');
    FElement.setAttribute('role', 'option');
  end;
  FElement.setAttribute('data-nyx-item', FRef.ID);
  FElement.setAttribute('tabindex', '0');
  FElement.onclick := Click;

  if FOwner.FView.Projection in [cpTable, cpTree] then
  begin
    FElement.ondblclick := DoubleClick;
  end;
  SetLength(FLabels, LColumns);
  SetLength(FInputs, LColumns);
  SetLength(FCells, LColumns);
  for LIndex := 0 to LColumns - 1 do
  begin
    LCell := FElement;

    if FOwner.FView.Projection = cpTable then
    begin
      LCell := NewElement('td');
      LCell.setAttribute('role', 'gridcell');
      LCell.setAttribute('data-nyx-column', IntToStr(LIndex));
      LCell.setAttribute('aria-colindex', IntToStr(LIndex + 1));
      LCell.setAttribute('tabindex', '-1');
      FElement.appendChild(LCell);
    end
    else if FOwner.FView.Projection = cpTree then
    begin
      LCell := LCaption;
      LCell.setAttribute('data-nyx-column', '0');
    end;
    LColumn := FOwner.FView.Spec.ColumnAt(LIndex);
    FCells[LIndex] := LCell;

    if FOwner.FView.Projection in [cpTable, cpTree] then
    begin
      { A data cell exposes its actual value in navigation mode. An inactive
        textbox's value is not equivalent cell text for assistive technology. }
      FLabels[LIndex] := NewElement('span');
      FLabels[LIndex].classList.add('nyx-cell-value');
      LCell.appendChild(FLabels[LIndex]);
    end;

    if (LColumn.Mode = cmEditable) and (FOwner.FView.Projection <> cpList) then
    begin
      FInputs[LIndex] := TJSHTMLInputElement(document.createElement('input'));

      if FOwner.FView.Projection in [cpTable, cpTree] then
      begin
        FInputs[LIndex].classList.add('nyx-cell-editor');
        FInputs[LIndex].setAttribute('hidden', '');
      end;
      { The composite has one Tab entry in navigation mode. F2/Enter enters an
        owned editor; its Tab/Escape handling then preserves keyboard access to
        columns without putting every field in the surrounding page's Tab order. }
      FInputs[LIndex].setAttribute('tabindex', '-1');
      FInputs[LIndex].setAttribute('aria-label', LColumn.Title);
      FInputs[LIndex].setAttribute('data-nyx-column', IntToStr(LIndex));
      FInputs[LIndex].onchange := Change;
      FInputs[LIndex]._type := 'text';
      FInputs[LIndex].style.setProperty('min-width', '0');
      FInputs[LIndex].style.setProperty('width', '100%');
      FInputs[LIndex].style.setProperty('box-sizing', 'border-box');

      if LColumn.Kind = nskBoolean then
      begin
        FInputs[LIndex]._type := 'checkbox';
        FInputs[LIndex].style.removeProperty('width');
      end
      else if LColumn.Kind in [nskInteger, nskNumber] then
      begin
        FInputs[LIndex].setAttribute('inputmode', 'decimal');
      end;
      LCell.appendChild(FInputs[LIndex]);
    end
    else
    begin

      if FLabels[LIndex] = nil then
      begin
        FLabels[LIndex] := NewElement('span');
        LCell.appendChild(FLabels[LIndex]);
      end;
    end;
  end;

  if (FOwner.FView.Projection = cpTable) and
    (Length(FOwner.FColumnWidths) = LColumns) then
  begin
    for LIndex := 0 to LColumns - 1 do
    begin
      FCells[LIndex].style.setProperty('width', IntToStr(FOwner.FColumnWidths[LIndex]) + 'px');
      FCells[LIndex].style.setProperty('min-width', IntToStr(FOwner.FColumnWidths[LIndex]) + 'px');
      FCells[LIndex].style.setProperty('max-width', IntToStr(FOwner.FColumnWidths[LIndex]) + 'px');
      FCells[LIndex].style.setProperty('box-sizing', 'border-box');
    end;
  end;

  if FOwner.FWindowObserver <> nil then
  begin
    { Font loading, wrapped text and creator styling can change a visible row
      without changing its fixed-height scroll host. Observe owned row faces. }
    FOwner.FWindowObserver.observe(FElement);
  end;
end;

destructor TBrowserRow.Destroy;
var
  LIndex: Integer;
begin

  if (FOwner <> nil) and (FOwner.FWindowObserver <> nil) and (FElement <> nil) then
  begin
    FOwner.FWindowObserver.unobserve(FElement);
  end;

  if FElement <> nil then
  begin
    FElement.onclick := nil;
    FElement.ondblclick := nil;
    FElement.onkeydown := nil;
    FElement.removeEventListener('toggle', @Toggle);
  end;
  for LIndex := 0 to Length(FInputs) - 1 do
  begin

    if FInputs[LIndex] <> nil then
    begin
      FInputs[LIndex].onchange := nil;
    end;
  end;

  if (FElement <> nil) and (FElement.parentNode <> nil) then
  begin
    FElement.parentNode.removeChild(FElement);
  end;
  inherited Destroy;
end;

procedure TBrowserRow.Sync(AValuesChanged: Boolean);
var
  LIndex: Integer;
  LText: TNyxText;
  LApplyValues: Boolean;
  LData: INyxCollectionSnapshot;
  LPrevious: Integer;
  LColumn: TNyxCollectionColumn;
  LEditing: Boolean;
begin
  { Compare accepted scalar values per cell, rather than treating every dataset
    publication as permission to replace every draft. Stable row identities keep
    their DOM editors across selection, reorder and unrelated item/field edits. }
  LData := FOwner.FView.Snapshot;
  LPrevious := -1;

  if FOwner.FRendered <> nil then
  begin
    LPrevious := FOwner.FRendered.IndexOf(FRef);
  end;
  for LIndex := 0 to Length(FLabels) - 1 do
  begin

    if FOwner.FView.Projection = cpTable then
    begin
      FCells[LIndex].setAttribute('tabindex', '-1');
      FCells[LIndex].setAttribute('aria-readonly', LowerCase(BoolToStr(
        FOwner.FReadOnly or (FOwner.FView.Spec.ColumnAt(LIndex).Mode <> cmEditable), True)));
    end;
    LApplyValues := not FInitialized or (AValuesChanged and (LPrevious < 0));

    if AValuesChanged and not LApplyValues and (FOwner.FRendered.Revision <> LData.Revision) then
    begin
      LColumn := FOwner.FView.Spec.ColumnAt(LIndex);
      LApplyValues := not LColumn.Read(LData.Item(FRef)).SameValue(
        LColumn.Read(FOwner.FRendered.ItemAt(LPrevious)));
    end;

    if FOwner.FNormalizeValues and (FOwner.FNormalizeItem.ID = FRef.ID) and
      (FOwner.FNormalizeColumn = LIndex) then
    begin
      LApplyValues := True;
    end;

    if FInputs[LIndex] <> nil then
    begin
      { Read-only text remains inspectable, selectable and copyable. Checkboxes
        have no native readOnly behavior, so their editing authority is disabled
        explicitly while row selection stays available. }
      FInputs[LIndex].disabled := not FOwner.FEnabled or
        (FOwner.FReadOnly and (FInputs[LIndex]._type = 'checkbox'));
      FInputs[LIndex].readOnly := FOwner.FReadOnly;

      if FOwner.FView.Projection in [cpTable, cpTree] then
      begin
        LEditing := FOwner.CellEditing(FRef, LIndex);

        if LEditing then
        begin
          FInputs[LIndex].removeAttribute('hidden');
          FLabels[LIndex].setAttribute('hidden', '');
        end
        else
        begin
          FInputs[LIndex].setAttribute('hidden', '');
          FLabels[LIndex].removeAttribute('hidden');
        end;
      end;

      if LApplyValues then
      begin
        LText := FOwner.FView.CellText(FRef, LIndex);

        if FInputs[LIndex]._type = 'checkbox' then
        begin
          FInputs[LIndex].checked := LText = 'true';
        end
        else if FInputs[LIndex].value <> LText then
        begin
          FInputs[LIndex].value := LText;
        end;

        if FLabels[LIndex] <> nil then
        begin
          FLabels[LIndex].textContent := LText;
        end;
      end;
    end
    else if LApplyValues then
    begin
      { Unchanged label nodes are retained just like editors. Reassigning
        textContent also destroys their DOM text node and needlessly repaints. }
      LText := FOwner.FView.CellText(FRef, LIndex);
      FLabels[LIndex].textContent := LText;
    end;
  end;
  FElement.setAttribute('aria-selected', 'false');
  FElement.classList.remove('nyx-selected');

  if FOwner.FView.Selection.Contains(FRef) then
  begin
    FElement.setAttribute('aria-selected', 'true');
    FElement.classList.add('nyx-selected');
  end;
  FElement.setAttribute('tabindex', '-1');
  FInitialized := True;

  if FOwner.FView.Projection = cpTree then
  begin
    Toggle(nil);
  end;
end;

function TBrowserRow.HasDraft: Boolean;
var
  LIndex: Integer;
  LText: TNyxText;
begin
  Result := False;
  for LIndex := 0 to Length(FInputs) - 1 do
  begin

    if FInputs[LIndex] <> nil then
    begin
      LText := FOwner.FView.CellText(FRef, LIndex);

      if FInputs[LIndex]._type = 'checkbox' then
      begin
        Result := FInputs[LIndex].checked <> (LText = 'true');
      end
      else
      begin
        Result := FInputs[LIndex].value <> LText;
      end;

      if Result then
      begin
        Exit;
      end;
    end;
  end;
end;

function TBrowserMount.ScrollChanged(AEvent: TEventListenerEvent): Boolean;
begin
  Result := True;

  if (FHost = nil) or ((AEvent.target is TJSNode) and
    not TJSNode(AEvent.target).contains(FHost)) then
  begin
    Exit;
  end;
  { Descendant editor scrolling and unrelated panes do not move this viewport. }

  if FRevealItem.Defined and (AEvent.target = FHost) and
    (Abs(TScrollableElement(FHost).scrollTopPixels - FRevealScrollTop) > 0.5) then
  begin
    FRevealItem := Default(TNyxItemRef);
  end;
  QueueWindow;
end;

function TBrowserMount.WindowChanged(AEvent: TEventListenerEvent): Boolean;
begin
  Result := True;
  QueueWindow;
end;

procedure TBrowserMount.RowBoundsChanged(AEntries: TJSHTMLResizeObserverEntryArray;
  AObserver: TJSHTMLResizeObserver);
begin
  QueueWindow;
end;

procedure TBrowserMount.QueueWindow;
begin

  if (FHost = nil) or FWindowQueued then
  begin
    Exit;
  end;
  FWindowQueued := True;
  FWindowFrame := window.requestAnimationFrame(@WindowFrame);
end;

procedure TBrowserMount.WindowFrame(ATime: Double);
begin
  FWindowQueued := False;

  if GetConnected then
  begin
    { Refresh retains its managed call frame through any observer/focus callback.
      No borrowed mount/DOM state is consulted after that call returns. }
    Refresh;
  end;
end;

procedure TBrowserMount.ObserveWindowAncestors;
var
  LNext: array of TJSHTMLElement;
  LAncestor: TJSHTMLElement;
  LIndex: Integer;
  LNextIndex: Integer;
  LFound: Boolean;
begin

  if FWindowObserver = nil then
  begin
    Exit;
  end;
  LAncestor := FHost;
  while LAncestor <> nil do
  begin
    SetLength(LNext, Length(LNext) + 1);
    LNext[High(LNext)] := LAncestor;
    FWindowObserver.observe(LAncestor);
    LAncestor := TJSHTMLElement(LAncestor.parentElement);
  end;
  for LIndex := 0 to Length(FWindowAncestors) - 1 do
  begin
    LFound := False;
    for LNextIndex := 0 to Length(LNext) - 1 do
    begin
      LFound := LFound or (LNext[LNextIndex] = FWindowAncestors[LIndex]);
    end;

    if not LFound then
    begin
      { A retained view can move/park without unmounting. Its old scroll/layout
        owners must not remain observer targets or retain obsolete DOM roots. }
      FWindowObserver.unobserve(FWindowAncestors[LIndex]);
    end;
  end;
  FWindowAncestors := LNext;
end;

procedure TBrowserMount.RowViewport(out AOffset, AExtent: Double);
var
  LClipTop: Double;
  LClipBottom: Double;
  LBodyTop: Double;
  LAncestor: TJSHTMLElement;
  LRect: TJSDOMRect;
  LStyle: TJSCSSStyleDeclaration;
  LOverflow: String;
  LScale: Double;
  LAncestorScale: Double;
  LHeaderCell: TJSHTMLElement;
  LHeaderHeight: String;
  LPadding: Double;
begin
  ObserveWindowAncestors;
  AOffset := 0;
  AExtent := 320;

  if not document.documentElement.contains(FHost) or
    (Length(FHost.getClientRects) = 0) then
  begin
    { A detached candidate realizes a small prefetch window. Its first layout
      observation recomputes actual host/ancestor/window intersections. }
    Exit;
  end;
  LClipTop := 0;
  LClipBottom := window.innerHeight;
  LScale := 1;

  if FHost.offsetHeight > 0 then
  begin
    LScale := FHost.getBoundingClientRect.height / FHost.offsetHeight;
  end;

  if IsNan(LScale) or IsInfinite(LScale) or (LScale <= 0) then
  begin
    LScale := 1;
  end;
  LBodyTop := FBody.getBoundingClientRect.top;

  if (FView.Projection = cpTable) and (FBody.children.length = 0) then
  begin
    { An empty tbody can report the zero rectangle on an already attached host.
      The actual header end establishes its prospective first data-row origin. }
    LBodyTop := TJSHTMLElement(FHost.querySelector('thead')).getBoundingClientRect.bottom;
  end;

  if FView.Projection in [cpList, cpTree] then
  begin
    { Lists use their own scrolling content rather than a separate tbody.
      Keep the origin in that logical plane, including an empty attached host. }
    LPadding := parseFloat(window.getComputedStyle(FHost).getPropertyValue('padding-top'));

    if IsNan(LPadding) or IsInfinite(LPadding) then
    begin
      LPadding := 0;
    end;
    LBodyTop := FHost.getBoundingClientRect.top +
      (FHost.clientTop + LPadding - TScrollableElement(FHost).scrollTopPixels) * LScale;
  end;
  LAncestor := FHost;
  while LAncestor <> nil do
  begin
    LStyle := window.getComputedStyle(LAncestor);
    LOverflow := LStyle.getPropertyValue('overflow-y');

    if (LOverflow = 'auto') or (LOverflow = 'scroll') or
      (LOverflow = 'hidden') or (LOverflow = 'clip') then
    begin
      LRect := LAncestor.getBoundingClientRect;
      LAncestorScale := 1;

      if LAncestor.offsetHeight > 0 then
      begin
        LAncestorScale := LRect.height / LAncestor.offsetHeight;
      end;
      LClipTop := Max(LClipTop, LRect.top + LAncestor.clientTop * LAncestorScale);
      LClipBottom := Min(LClipBottom,
        LRect.top + (LAncestor.clientTop + LAncestor.clientHeight) * LAncestorScale);
    end;
    LAncestor := TJSHTMLElement(LAncestor.parentElement);
  end;
  LHeaderCell := TJSHTMLElement(FHost.querySelector('thead th'));

  if (LHeaderCell <> nil) and
    (window.getComputedStyle(LHeaderCell).getPropertyValue('position') = 'sticky') then
  begin
    { Sticky headings occlude part of the scroll plane. Exclude that measured
      area from realization/paging and use logical height for nearest scrolling,
      including scaled Studio previews. }
    LClipTop := Max(LClipTop, Min(LClipBottom, LHeaderCell.getBoundingClientRect.bottom));
    LHeaderHeight := IntToStr(Max(1, Ceil(LHeaderCell.offsetHeight))) + 'px';

    if FHost.style.getPropertyValue('--nyx-grid-header-height') <> LHeaderHeight then
    begin
      FHost.style.setProperty('--nyx-grid-header-height', LHeaderHeight);
    end;
  end;
  { Rectangles are viewport coordinates; row geometry is the logical CSS plane.
    Compensate axis-aligned scaling rather than shrinking authored text. }
  AOffset := Max(0, (LClipTop - LBodyTop) / LScale);
  AExtent := Max(0, (LClipBottom - Max(LClipTop, LBodyTop)) / LScale);

  if AExtent = 0 then
  begin
    { A wholly clipped table retains a bounded prefetch, never a full source. }
    AExtent := 320;
  end;
end;

procedure TBrowserMount.EnsureRow(AIndex: Integer);
begin

  if not GetConnected or (AIndex < 0) or (AIndex >= FView.Snapshot.Count) then
  begin
    Exit;
  end;

  if (AIndex >= Length(FRows)) or (FRows[AIndex] = nil) or
    not FHost.contains(FRows[AIndex].FElement) then
  begin
    FForcedItem := FView.Snapshot.ItemAt(AIndex).Ref;
    Refresh;
  end;
end;

procedure TBrowserMount.FocusListOption(AOption: TJSHTMLElement);
var
  LOptions: TFocusOptions;
  LFace: TJSHTMLElement;
begin

  if not GetConnected or (AOption = nil) or not FHost.contains(AOption) then
  begin
    Exit;
  end;
  FRevealItem := NyxItem(FView.Spec.Key, AOption.getAttribute('data-nyx-item'));
  LFace := AOption;

  if FView.Projection = cpTree then
  begin
    { A branch box includes its descendant group. Its own summary, rather than
      the complete subtree, is the keyboard destination to reveal. }
    LFace := TJSHTMLElement(AOption.querySelector('summary'));
  end;
  RevealListOption(LFace);
  LOptions := TFocusOptions.new;
  LOptions.preventScroll := True;
  TFocusable(AOption).focus(LOptions);
end;

procedure TBrowserMount.RevealListOption(AOption: TJSHTMLElement);
var
  LHostBounds: TJSDOMRect;
  LOptionBounds: TJSDOMRect;
  LScale: Double;
  LTop: Double;
  LBottom: Double;
  LDelta: Double;
begin

  if not GetConnected or (AOption = nil) or not FHost.contains(AOption) then
  begin
    Exit;
  end;
  LHostBounds := FHost.getBoundingClientRect;
  LOptionBounds := AOption.getBoundingClientRect;
  LScale := 1;

  if FHost.offsetHeight > 0 then
  begin
    LScale := LHostBounds.height / FHost.offsetHeight;
  end;

  if IsNan(LScale) or IsInfinite(LScale) or (LScale <= 0) then
  begin
    LScale := 1;
  end;
  LTop := LHostBounds.top + FHost.clientTop * LScale;
  LBottom := LTop + FHost.clientHeight * LScale;
  LDelta := 0;

  if LOptionBounds.top < LTop then
  begin
    LDelta := LOptionBounds.top - LTop;
  end
  else if LOptionBounds.bottom > LBottom then
  begin
    LDelta := LOptionBounds.bottom - LBottom;
  end;
  TScrollableElement(FHost).scrollTopPixels :=
    TScrollableElement(FHost).scrollTopPixels + LDelta / LScale;
  FRevealScrollTop := TScrollableElement(FHost).scrollTopPixels;
end;

procedure TBrowserMount.SizeTableColumns;
var
  LCanvas: TJSHTMLCanvasElement;
  LContext: TJSCanvasRenderingContext2D;
  LFont: String;
  LAvailable: Integer;
  LPolicies: TNyxColumnSizings;
  LWidths: TNyxFlowSizes;
  LColumn: Integer;
  LRow: Integer;
  LResidentIndex: Integer;
  LNatural: Double;
  LHeaders: TJSNodeList;
  LWidth: String;
  LData: INyxCollectionSnapshot;
begin
  LAvailable := Max(0, FHost.clientWidth);
  LFont := window.getComputedStyle(FHost).getPropertyValue('font');

  if LFont = '' then
  begin
    LFont := '13px sans-serif';
  end;
  LData := FView.Snapshot;

  if (Length(FColumnNatural) <> FView.Spec.Count) or (FColumnFont <> LFont) then
  begin
    { Bounded initial sample avoids materializing/measuring an entire virtual
      dataset. Width metrics then remain stable across scrolling and edits.
      Font changes invalidate measurement; resize only reallocates the widths. }
    LCanvas := TJSHTMLCanvasElement(document.createElement('canvas'));
    LContext := TJSCanvasRenderingContext2D(LCanvas.getContext('2d'));
    LContext.font := LFont;
    SetLength(FColumnNatural, FView.Spec.Count);
    for LColumn := 0 to FView.Spec.Count - 1 do
    begin
      LNatural := LContext.measureText(FView.Spec.ColumnAt(LColumn).Title).width;
      for LRow := 0 to Min(31, LData.Count - 1) do
      begin
        LNatural := Max(LNatural, LContext.measureText(
          FView.CellText(LData.ItemAt(LRow).Ref, LColumn)).width);
      end;
      FColumnNatural[LColumn] := Max(48, Ceil(LNatural + 24));
    end;
    FColumnFont := LFont;
    FColumnAvailable := -1;
  end;

  if (FColumnAvailable = LAvailable) and (Length(FColumnWidths) = FView.Spec.Count) then
  begin
    Exit;
  end;
  SetLength(LPolicies, FView.Spec.Count);
  for LColumn := 0 to High(LPolicies) do
  begin
    LPolicies[LColumn] := FView.Spec.ColumnAt(LColumn).Sizing;
  end;
  LWidths := NyxColumnWidths(LAvailable, LPolicies, FColumnNatural);
  FColumnWidths := LWidths;
  FColumnAvailable := LAvailable;
  LHeaders := FHost.querySelectorAll('thead th');
  for LColumn := 0 to High(LWidths) do
  begin
    LWidth := IntToStr(LWidths[LColumn]) + 'px';
    TJSHTMLElement(LHeaders[LColumn]).style.setProperty('width', LWidth);
    TJSHTMLElement(LHeaders[LColumn]).style.setProperty('min-width', LWidth);
    TJSHTMLElement(LHeaders[LColumn]).style.setProperty('max-width', LWidth);
    TJSHTMLElement(LHeaders[LColumn]).style.setProperty('box-sizing', 'border-box');
    for LResidentIndex := 0 to High(FResidentRows) do
    begin
      LRow := FResidentRows[LResidentIndex];

      if FRows[LRow] <> nil then
      begin
        FRows[LRow].FCells[LColumn].style.setProperty('width', LWidth);
        FRows[LRow].FCells[LColumn].style.setProperty('min-width', LWidth);
        FRows[LRow].FCells[LColumn].style.setProperty('max-width', LWidth);
        FRows[LRow].FCells[LColumn].style.setProperty('box-sizing', 'border-box');
      end;
    end;
  end;
end;

procedure TBrowserMount.RenderFlatWindow;
const
  CEstimate = 32;
  COverscan = 6;
type
  TScrollPosition = record
    Element: TJSHTMLElement;
    Top: Double;
    Left: Double;
  end;
var
  LData: INyxCollectionSnapshot;
  LNext: array of TBrowserRow;
  LUsed: array of Boolean;
  LWork: array of Integer;
  LResident: array of Integer;
  LScroll: array of TScrollPosition;
  LWindow: TNyxCollectionRowWindow;
  LFocused: TJSHTMLElement;
  LFocusRow: TJSHTMLElement;
  LFocusID: TNyxText;
  LTabStop: TJSHTMLElement;
  LNextFocus: TJSHTMLElement;
  LInput: TJSHTMLInputElement;
  LOptions: TFocusOptions;
  LIndex: Integer;
  LPrevious: Integer;
  LCursor: Integer;
  LPosition: Integer;
  LWorkIndex: Integer;
  LListEntry: Integer;
  LSameOrder: Boolean;
  LSameSnapshot: Boolean;
  LRealize: Boolean;
  LMeasured: Boolean;
  LOffset: Double;
  LExtent: Double;
  LHeight: Double;
  LRowGap: Double;
  LSpacing: String;
  LAnchor: TJSNode;
  LAncestor: TJSHTMLElement;
  LWindowTop: Double;
  LWindowLeft: Double;
  LCaretStart: NativeInt;
  LCaretEnd: NativeInt;
  LCaretDirection: String;

  { This private work vector admits only current logical indices. It combines
    existing owned faces/drafts, the viewport and pinned navigation destinations.
    Sorting once avoids dataset-sized marker allocation on every scroll. }
  procedure AddWork(AIndex: Integer);
  begin

    if (AIndex < 0) or (AIndex >= LData.Count) then
    begin
      Exit;
    end;
    SetLength(LWork, Length(LWork) + 1);
    LWork[High(LWork)] := AIndex;
  end;

  procedure SortWork(AFirst, ALast: Integer);
  var
    LLeft: Integer;
    LRight: Integer;
    LPivot: Integer;
    LSwap: Integer;
  begin
    LLeft := AFirst;
    LRight := ALast;
    LPivot := LWork[AFirst + (ALast - AFirst) div 2];
    repeat
      while LWork[LLeft] < LPivot do
      begin
        Inc(LLeft);
      end;
      while LWork[LRight] > LPivot do
      begin
        Dec(LRight);
      end;

      if LLeft <= LRight then
      begin
        LSwap := LWork[LLeft];
        LWork[LLeft] := LWork[LRight];
        LWork[LRight] := LSwap;
        Inc(LLeft);
        Dec(LRight);
      end;
    until LLeft > LRight;

    if AFirst < LRight then
    begin
      SortWork(AFirst, LRight);
    end;

    if LLeft < ALast then
    begin
      SortWork(LLeft, ALast);
    end;
  end;

  { Spacers represent exact missing logical intervals. They carry no item/cell
    identity and are excluded from accessibility and all keyboard/range orders.
    Owned Nyx theme uses collapsed borders; separate spacing is compensated. }
  procedure AddGap(AFirst, AAfter: Integer);
  var
    LRow: TJSHTMLElement;
    LCell: TJSHTMLElement;
    LPixels: Double;
    LFormat: TFormatSettings;
  begin

    if AAfter <= AFirst then
    begin
      Exit;
    end;
    LPixels := Max(0, FGeometry.OffsetAt(AAfter) - FGeometry.OffsetAt(AFirst) - LRowGap);

    if FView.Projection = cpTable then
    begin
      LRow := NewElement('tr');
    end
    else
    begin
      LRow := NewElement('li');
    end;
    LRow.setAttribute('aria-hidden', 'true');
    LRow.setAttribute('data-nyx-row-spacer', 'true');
    LCell := LRow;

    if FView.Projection = cpTable then
    begin
      LCell := NewElement('td');
      LCell.setAttribute('colspan', IntToStr(FView.Spec.Count));
      LRow.appendChild(LCell);
    end;
    { CSS uses a decimal point independently of application/user RTL locale. }
    LFormat := FormatSettings;
    LFormat.DecimalSeparator := '.';
    LCell.style.setProperty('height', FloatToStr(LPixels, LFormat) + 'px');
    LCell.style.setProperty('padding', '0');
    LCell.style.setProperty('border', '0');
    LCell.style.setProperty('line-height', '0');
    SetLength(FSpacers, Length(FSpacers) + 1);
    FSpacers[High(FSpacers)] := LRow;
    LAnchor := nil;

    if LPosition < FBody.children.length then
    begin
      LAnchor := FBody.children[LPosition];
    end;
    FBody.insertBefore(LRow, LAnchor);
    Inc(LPosition);
  end;

begin
  LData := FView.Snapshot;

  if not FEnabled then
  begin
    FRevealItem := Default(TNyxItemRef);
  end;

  if FEditingItem.Defined and
    (not FEnabled or FReadOnly or (LData.IndexOf(FEditingItem) < 0)) then
  begin
    { Revoked authority or filtered/removed membership ends runtime editing.
      A later reappearance of the same open item name must not resurrect an
      editor that no longer owns focus. Authored values remain untouched. }
    FEditingItem := Default(TNyxItemRef);
  end;
  LFocused := TJSHTMLElement(document.activeElement);

  if FView.Projection = cpTable then
  begin
    SizeTableColumns;
  end;
  LFocusID := '';
  LInput := nil;

  if FHost.contains(LFocused) then
  begin
    LFocusRow := TJSHTMLElement(LFocused.closest('[data-nyx-item]'));

    if LFocusRow <> nil then
    begin
      LFocusID := LFocusRow.getAttribute('data-nyx-item');
    end;

    if (LFocused is TJSHTMLInputElement) and
      (TJSHTMLInputElement(LFocused)._type <> 'checkbox') then
    begin
      LInput := TJSHTMLInputElement(LFocused);
      LCaretStart := LInput.selectionStart;
      LCaretEnd := LInput.selectionEnd;
      LCaretDirection := LInput.selectionDirection;
    end;
  end
  else
  begin
    LFocused := nil;
  end;
  LSameSnapshot := FRendered = LData;
  LSameOrder := (FRendered <> nil) and (FRendered.Count = LData.Count);

  if LSameSnapshot then
  begin
    { The mount exclusively owns these mutable row slots. Same-source refresh
      needs no copy/remapping; retained runtime views never borrow this array. }
    LNext := FRows;
    LWork := Copy(FResidentRows, 0, Length(FResidentRows));
  end
  else
  begin
    SetLength(LNext, LData.Count);
    SetLength(LUsed, Length(FRows));
    for LIndex := 0 to LData.Count - 1 do
    begin
      LPrevious := -1;

      if FRendered <> nil then
      begin
        LPrevious := FRendered.IndexOf(LData.ItemAt(LIndex).Ref);
      end;

      if LPrevious <> LIndex then
      begin
        LSameOrder := False;
      end;

      if (LPrevious >= 0) and (LPrevious < Length(FRows)) then
      begin
        LNext[LIndex] := FRows[LPrevious];
        LUsed[LPrevious] := True;

        if LNext[LIndex] <> nil then
        begin
          AddWork(LIndex);
        end;
      end;
    end;
  end;

  if not LSameOrder then
  begin
    FGeometry.Reset(LData.Count, CEstimate);
  end;
  RowViewport(LOffset, LExtent);
  LWindow := FGeometry.Window(LOffset, LExtent, COverscan);
  LListEntry := -1;

  if (FView.Projection = cpList) and (LData.Count > 0) then
  begin
    { A list with no selected/focused option retains the first logical Tab
      entry, even after manual scrolling. DOM membership does not own order. }
    LListEntry := 0;

    if FView.Selection.Focus.Defined then
    begin
      LListEntry := LData.IndexOf(FView.Selection.Focus);
    end;
    AddWork(LListEntry);
  end;
  for LIndex := LWindow.First to LWindow.AfterLast - 1 do
  begin
    AddWork(LIndex);
  end;

  if LFocusID <> '' then
  begin
    AddWork(LData.IndexOf(NyxItem(LData.Key, LFocusID)));
  end;

  if FView.Selection.Focus.Defined then
  begin
    AddWork(LData.IndexOf(FView.Selection.Focus));
  end;

  if FForcedItem.Defined then
  begin
    AddWork(LData.IndexOf(FForcedItem));
  end;

  if Length(LWork) > 1 then
  begin
    SortWork(0, High(LWork));
  end;
  LRowGap := 0;

  if (FView.Projection = cpTable) and
    (window.getComputedStyle(FHost).getPropertyValue('border-collapse') <> 'collapse') then
  begin
    LSpacing := Trim(window.getComputedStyle(FHost).getPropertyValue('border-spacing'));
    LPrevious := Pos(' ', LSpacing);

    if LPrevious > 0 then
    begin
      LSpacing := Copy(LSpacing, LPrevious + 1, Length(LSpacing));
    end;
    LRowGap := parseFloat(LSpacing);

    if IsNan(LRowGap) or IsInfinite(LRowGap) or (LRowGap < 0) then
    begin
      LRowGap := 0;
    end;
  end;
  { Capture before even an offscreen draft/row is detached: DOM removal may
    synchronously clamp a scroll owner. Keep the surrounding policy untouched. }
  LWindowTop := TScrollableWindow(window).scrollYPixels;
  LWindowLeft := TScrollableWindow(window).scrollXPixels;
  LAncestor := FHost;
  while LAncestor <> nil do
  begin
    SetLength(LScroll, Length(LScroll) + 1);
    LScroll[High(LScroll)].Element := LAncestor;
    LScroll[High(LScroll)].Top := TScrollableElement(LAncestor).scrollTopPixels;
    LScroll[High(LScroll)].Left := TScrollableElement(LAncestor).scrollLeftPixels;
    LAncestor := TJSHTMLElement(LAncestor.parentElement);
  end;
  FBody.style.setProperty('overflow-anchor', 'none');
  LTabStop := nil;
  for LWorkIndex := 0 to High(LWork) do
  begin
    LIndex := LWork[LWorkIndex];

    if (LWorkIndex > 0) and (LIndex = LWork[LWorkIndex - 1]) then
    begin
      Continue;
    end;
    LRealize := ((LIndex >= LWindow.First) and (LIndex < LWindow.AfterLast)) or
      (LIndex = LListEntry) or
      (LData.ItemAt(LIndex).Ref.ID = LFocusID) or
      (FView.Selection.Focus.Defined and
        (LData.ItemAt(LIndex).Ref.ID = FView.Selection.Focus.ID)) or
      (FForcedItem.Defined and (LData.ItemAt(LIndex).Ref.ID = FForcedItem.ID));

    if LRealize and (LNext[LIndex] = nil) then
    begin
      LNext[LIndex] := TBrowserRow.Create(Self, LData.ItemAt(LIndex).Ref);
    end;

    if LNext[LIndex] <> nil then
    begin
      LNext[LIndex].Sync(FRefreshPlan.ValuesChanged(LIndex));

      if not LRealize and not LNext[LIndex].HasDraft then
      begin
        LNext[LIndex].Free;
        LNext[LIndex] := nil;
      end
      else if not LRealize then
      begin

        if LNext[LIndex].FElement.parentNode <> nil then
        begin
          LNext[LIndex].FElement.remove;
        end;
      end
      else
      begin
        if FView.Projection = cpTable then
        begin
          LNext[LIndex].FElement.setAttribute('aria-rowindex', IntToStr(LIndex + 2));
        end
        else
        begin
          LNext[LIndex].FElement.setAttribute('aria-posinset', IntToStr(LIndex + 1));
          LNext[LIndex].FElement.setAttribute('aria-setsize', IntToStr(LData.Count));
        end;

        if (LTabStop = nil) or (FView.Selection.Focus.Defined and
          (LData.ItemAt(LIndex).Ref.ID = FView.Selection.Focus.ID)) then
        begin
          LTabStop := LNext[LIndex].FCells[FFocusedColumn];
        end;
      end;

      if LNext[LIndex] <> nil then
      begin
        SetLength(LResident, Length(LResident) + 1);
        LResident[High(LResident)] := LIndex;
      end;
    end;
  end;

  if not LSameSnapshot then
  begin
    for LIndex := 0 to Length(FRows) - 1 do
    begin

      if not LUsed[LIndex] then
      begin
        FRows[LIndex].Free;
      end;
    end;
  end;
  FRows := LNext;
  FResidentRows := LResident;
  for LIndex := 0 to Length(FSpacers) - 1 do
  begin
    FSpacers[LIndex].remove;
  end;
  FSpacers := nil;
  LCursor := 0;
  LPosition := 0;
  for LWorkIndex := 0 to High(FResidentRows) do
  begin
    LIndex := FResidentRows[LWorkIndex];

    if (FRows[LIndex] <> nil) and
      (((LIndex >= LWindow.First) and (LIndex < LWindow.AfterLast)) or
        (LIndex = LListEntry) or
        (FRows[LIndex].FRef.ID = LFocusID) or
        (FView.Selection.Focus.Defined and
          (FRows[LIndex].FRef.ID = FView.Selection.Focus.ID)) or
        (FForcedItem.Defined and (FRows[LIndex].FRef.ID = FForcedItem.ID))) then
    begin
      AddGap(LCursor, LIndex);
      LAnchor := nil;

      if LPosition < FBody.children.length then
      begin
        LAnchor := FBody.children[LPosition];
      end;

      if FRows[LIndex].FElement <> LAnchor then
      begin
        FBody.insertBefore(FRows[LIndex].FElement, LAnchor);
      end;
      Inc(LPosition);
      LCursor := LIndex + 1;
    end;
  end;
  AddGap(LCursor, LData.Count);
  for LIndex := 0 to Length(LScroll) - 1 do
  begin
    TScrollableElement(LScroll[LIndex].Element).scrollTopPixels := LScroll[LIndex].Top;
    TScrollableElement(LScroll[LIndex].Element).scrollLeftPixels := LScroll[LIndex].Left;
  end;

  if (TScrollableWindow(window).scrollXPixels <> LWindowLeft) or
    (TScrollableWindow(window).scrollYPixels <> LWindowTop) then
  begin
    TScrollableWindow(window).scrollTo(LWindowLeft, LWindowTop);
  end;
  LMeasured := False;
  for LWorkIndex := 0 to High(FResidentRows) do
  begin
    LIndex := FResidentRows[LWorkIndex];

    if (FRows[LIndex] <> nil) and FHost.contains(FRows[LIndex].FElement) then
    begin
      { offsetHeight is the logical outer allocation, excluding CSS transforms;
        screen rectangles would feed scaled heights back into logical spacers. }
      LHeight := FRows[LIndex].FElement.offsetHeight + LRowGap;

      if (LHeight > 0) and not IsNan(LHeight) and not IsInfinite(LHeight) and
        (Abs(LHeight - FGeometry.HeightAt(LIndex)) > 0.25) then
      begin
        LMeasured := FGeometry.Measure(LIndex, LHeight) or LMeasured;
      end;
    end;
  end;
  if FRevealItem.Defined then
  begin
    LIndex := LData.IndexOf(FRevealItem);

    if (LIndex >= 0) and (FRows[LIndex] <> nil) and FHost.contains(FRows[LIndex].FElement) then
    begin
      { Newly realized rows above the destination can increase its physical
        offset after the first key. Reapply nearest reveal through that measured
        pass, without refocusing or changing the containing page scroll. }
      RevealListOption(FRows[LIndex].FElement);
    end;

    if (LIndex < 0) or not LMeasured then
    begin
      FRevealItem := Default(TNyxItemRef);
    end;
  end;
  FRendered := LData;
  FForcedItem := Default(TNyxItemRef);
  FHost.setAttribute('tabindex', '-1');

  if FEnabled then
  begin

    if LTabStop <> nil then
    begin
      LTabStop.setAttribute('tabindex', '0');
    end
    else
    begin
      FHost.setAttribute('tabindex', '0');
    end;
  end;

  if LMeasured then
  begin
    QueueWindow;
  end;
  LNextFocus := nil;

  if (LFocused <> nil) and FEnabled then
  begin
    LNextFocus := LTabStop;

    if LNextFocus = nil then
    begin
      LNextFocus := FHost;
    end;

    if FHost.contains(LFocused) and
      not ((LFocused is TJSHTMLInputElement) and TJSHTMLInputElement(LFocused).disabled) then
    begin
      LNextFocus := LFocused;
    end;
  end;

  if (LNextFocus <> nil) and (document.activeElement <> LNextFocus) then
  begin
    LOptions := TFocusOptions.new;
    LOptions.preventScroll := True;
    TFocusable(LNextFocus).focus(LOptions);

    if GetConnected and (LInput <> nil) and (document.activeElement = LInput) then
    begin
      LInput.setSelectionRange(LCaretStart, LCaretEnd, LCaretDirection);
    end;
  end;
end;

function TBrowserRow.Toggle(AEvent: TEventListenerEvent): Boolean;
var
  LMount: INyxCollectionMount;
  LHierarchy: INyxTreeHierarchy;
  LExpanded: Boolean;
begin
  Result := True;
  LMount := FOwner as INyxCollectionMount;

  if not LMount.Connected then
  begin
    Exit;
  end;
  LHierarchy := NyxTreeHierarchy(FOwner.FView);
  { Real disclosure proposes runtime state. A nil event synchronizes and never
    publishes during Notify. DOM toggle events coalesce; current final state is
    authoritative instead of an obsolete event transition. }
  LExpanded := TNyxDetails(FElement).open;

  if (AEvent <> nil) and FOwner.FEnabled and LHierarchy.HasChildren(FRef) and
    (LExpanded <> LHierarchy.IsExpanded(FRef)) then
  begin
    LHierarchy.SetExpanded(FRef, LExpanded);
    { An observer can free this row. Only managed locals may be used afterward. }
    Exit;
  end;
  TNyxDetails(FElement).open := LHierarchy.IsExpanded(FRef);

  if not LHierarchy.HasChildren(FRef) then
  begin
    { A leaf is not a collapsed parent. Exposing aria-expanded on it would
      advertise disclosure that has no corresponding tree children. }
    FElement.removeAttribute('aria-expanded');
    Exit;
  end;
  FElement.setAttribute('aria-expanded', 'false');

  if TNyxDetails(FElement).open then
  begin
    FElement.setAttribute('aria-expanded', 'true');
  end;
end;

function TBrowserRow.Click(AEvent: TJSMouseEvent): Boolean;
const
  CEditorSelector = 'input,textarea,select,button,a[href],[contenteditable="true"]';
var
  LMount: INyxCollectionMount;
  LHost: TJSHTMLElement;
  LFocus: TJSHTMLElement;
  LEditor: TJSElement;
  LOptions: TFocusOptions;
  LCell: TJSHTMLElement;
  LColumn: Integer;
begin
  { A nested tree item owns its selection; bubbling must not select its parent. }
  AEvent.stopPropagation;

  if AEvent.defaultPrevented then
  begin
    Exit(True);
  end;

  if not FOwner.FEnabled then
  begin
    { aria-disabled alone does not remove a custom row's mouse default. Refuse
      disclosure, selection and focus before consulting the admitted view. }
    AEvent.preventDefault;
    Exit(True);
  end;
  { Selecting can notify an application observer that unmounts this whole view.
    Retain the attachment and capture the host before any row is detached. Never
    touch this row after publication; Disconnect may have destroyed it. }
  LMount := FOwner as INyxCollectionMount;
  LHost := FOwner.FHost;
  LFocus := FElement;

  if FOwner.FView.Projection = cpTable then
  begin
    LCell := TJSHTMLElement(TJSHTMLElement(AEvent.target).closest('[role=gridcell]'));

    if LCell <> nil then
    begin
      LColumn := StrToIntDef(LCell.getAttribute('data-nyx-column'), -1);

      if (LColumn >= 0) and (LColumn < Length(FCells)) and (FCells[LColumn] = LCell) then
      begin
        FOwner.FFocusedColumn := LColumn;
        LFocus := LCell;
      end;
    end;
  end;
  { Selecting an editable row must not steal the caret from the actual editor.
    Capture that managed DOM element before publication can detach this row. }

  LEditor := TJSHTMLElement(AEvent.target).closest(CEditorSelector);

  if LEditor <> nil then
  begin
    LFocus := TJSHTMLElement(LEditor);
  end;
  FOwner.Gesture(FRef, AEvent.shiftKey, AEvent.ctrlKey or AEvent.metaKey);

  if LMount.Connected then
  begin
    { A same-row click may change only the column, so no selection publication
      is required. Refresh the single Tab entry while preserving all drafts. }
    LMount.Refresh;
  end;

  if LMount.Connected then
  begin
    LOptions := TFocusOptions.new;
    LOptions.preventScroll := True;
    TFocusable(LFocus).focus(LOptions);
  end;
  { Preserve Nyx's existing control callback/scheduler boundary while stopping
    ancestor row selection. Forward the original event to the projected host's
    handler, with its modifiers and default-consumption state intact. }

  if LMount.Connected and Assigned(LHost.onclick) then
  begin
    LHost.onclick(AEvent);
  end;
  Result := True;
end;

{ Text belongs to the cell until its exact eligible editor is entered. Retained
  editor objects keep unfinished values and Unicode selection across unrelated
  publications; only visibility and runtime edit ownership change here. }
function TBrowserMount.CellEditing(const AItem: TNyxItemRef; AColumn: Integer): Boolean;
begin
  Result := FEnabled and not FReadOnly and FEditingItem.Defined and
    (FEditingItem.ID = AItem.ID) and (FEditingColumn = AColumn);
end;

function TBrowserMount.EnterCellEditor(const AItem: TNyxItemRef; AColumn: Integer): Boolean;
var
  LKeepAlive: INyxCollectionMount;
  LRow: Integer;
  LInput: TJSHTMLInputElement;
  LOptions: TFocusOptions;
begin
  Result := False;

  if not GetConnected or not FEnabled or FReadOnly or
    not (FView.Projection in [cpTable, cpTree]) then
  begin
    Exit;
  end;
  LKeepAlive := Self as INyxCollectionMount;
  LRow := FView.Snapshot.IndexOf(AItem);

  if (LRow < 0) or (LRow >= Length(FRows)) or (FRows[LRow] = nil) or
    (AColumn < 0) or (AColumn >= Length(FRows[LRow].FInputs)) then
  begin
    Exit;
  end;
  LInput := FRows[LRow].FInputs[AColumn];

  if (LInput = nil) or LInput.disabled then
  begin
    Exit;
  end;
  FEditingItem := AItem;
  FEditingColumn := AColumn;
  FFocusedColumn := AColumn;
  Select(AItem, nsaFocus);

  if GetConnected and FHost.contains(LInput) then
  begin
    Refresh;

    if GetConnected and FHost.contains(LInput) then
    begin
      LOptions := TFocusOptions.new;
      LOptions.preventScroll := True;
      TFocusable(LInput).focus(LOptions);
      Result := True;
    end;
  end;
end;

function TBrowserMount.EditorFocusChanged(AEvent: TEventListenerEvent): Boolean;
var
  LCell: TJSHTMLElement;
  LInput: TJSHTMLInputElement;
  LRow: Integer;
  LColumn: Integer;
  LItem: TNyxItemRef;
  LRelated: TJSElement;
begin
  Result := True;

  if not GetConnected or not (FView.Projection in [cpTable, cpTree]) or
    not (AEvent.target is TJSHTMLInputElement) then
  begin
    Exit;
  end;
  LInput := TJSHTMLInputElement(AEvent.target);
  LCell := TJSHTMLElement(LInput.closest('[role=gridcell]'));

  if FView.Projection = cpTree then
  begin
    LCell := TJSHTMLElement(LInput.closest('summary'));
  end;

  if (LCell = nil) or not FHost.contains(LCell) then
  begin
    Exit;
  end;
  LItem := NyxItem(FView.Spec.Key, LCell.parentElement.getAttribute('data-nyx-item'));
  LRow := FView.Snapshot.IndexOf(LItem);
  LColumn := StrToIntDef(LCell.getAttribute('data-nyx-column'), -1);

  if (LRow < 0) or (LRow >= Length(FRows)) or (FRows[LRow] = nil) or
    (LColumn < 0) or (LColumn >= Length(FRows[LRow].FInputs)) or
    (FRows[LRow].FInputs[LColumn] <> LInput) then
  begin
    Exit;
  end;

  if AEvent._type = 'focusin' then
  begin
    FEditingItem := LItem;
    FEditingColumn := LColumn;
  end
  else if CellEditing(LItem, LColumn) then
  begin
    LRelated := TJSElement(TJSFocusEvent(AEvent).relatedTarget);

    if (LRelated = nil) or not FHost.contains(LRelated) or
      (LowerCase(LRelated.tagName) <> 'input') then
    begin
      FEditingItem := Default(TNyxItemRef);
    end;
  end;
  { Focus events run inside the host's focusing algorithm. Defer presentation
    rather than hide or refocus the element while its blur/change is draining. }
  QueueWindow;
end;

function TBrowserRow.DoubleClick(AEvent: TJSMouseEvent): Boolean;
var
  LMount: INyxCollectionMount;
  LOwner: TBrowserMount;
  LHost: TJSHTMLElement;
  LCell: TJSHTMLElement;
  LItem: TNyxItemRef;
  LColumn: Integer;
begin
  Result := True;
  AEvent.stopPropagation;
  LMount := FOwner as INyxCollectionMount;

  if not LMount.Connected or AEvent.defaultPrevented then
  begin
    Exit;
  end;
  LCell := TJSHTMLElement(TJSHTMLElement(AEvent.target).closest('[role=gridcell]'));

  if FOwner.FView.Projection = cpTree then
  begin
    LCell := TJSHTMLElement(TJSHTMLElement(AEvent.target).closest('summary'));
  end;

  if LCell = nil then
  begin
    Exit;
  end;
  LColumn := StrToIntDef(LCell.getAttribute('data-nyx-column'), -1);

  if FOwner.FView.Projection = cpTree then
  begin
    LColumn := 0;
  end;

  if (LColumn < 0) or (LColumn >= Length(FCells)) or (FCells[LColumn] <> LCell) then
  begin
    Exit;
  end;
  LItem := FRef;
  LHost := FOwner.FHost;
  LOwner := FOwner;

  if Assigned(LHost.ondblclick) then
  begin
    LHost.ondblclick(AEvent);
  end;

  if LMount.Connected and not AEvent.defaultPrevented then
  begin
    LOwner.EnterCellEditor(LItem, LColumn);
  end;
end;

{$include nyx.collections.browser.selection.inc}

function TBrowserRow.Change(AEvent: TEventListenerEvent): Boolean;
var
  LIndex: Integer;
  LValue: TNyxText;
begin
  Result := True;
  AEvent.stopPropagation;

  if FOwner.FUpdating then
  begin
    { Relocating retained rows can blur an editor on older DOM engines. That
      publication noise must never admit an unfinished draft as a user edit. }
    Exit;
  end;
  for LIndex := 0 to Length(FInputs) - 1 do
  begin

    if AEvent.target = FInputs[LIndex] then
    begin
      LValue := FInputs[LIndex].value;

      if FInputs[LIndex]._type = 'checkbox' then
      begin
        LValue := 'false';

        if FInputs[LIndex].checked then
        begin
          LValue := 'true';
        end;
      end;
      FOwner.EditCell(FRef, LIndex, LValue);
      Exit;
    end;
  end;
end;

{$I nyx.collections.browser.tree.inc}

procedure TBrowserMount.RenderDataset;
begin
  FHost.setAttribute('aria-disabled', LowerCase(BoolToStr(not FEnabled, True)));
  RefreshQueryHeaders;
  case FView.Projection of
    cpTable:
      begin
        FHost.setAttribute('aria-readonly', LowerCase(BoolToStr(FReadOnly, True)));
        FHost.setAttribute('aria-rowcount', IntToStr(FView.Snapshot.Count + 1));
        FHost.setAttribute('aria-colcount', IntToStr(FView.Spec.Count));
        RenderFlatWindow;
      end;
    cpList:
      begin
        RenderFlatWindow;
      end;
    cpTree:
      begin
        RenderTreeWindow;
      end;
  end;
end;

procedure TBrowserMount.RefreshQueryHeaders;
var
  LColumn, LIndex: Integer;
  LSortEnabled, LFilterEnabled: Boolean;
  LSort: TNyxCollectionSort;
begin
  for LColumn := 0 to High(FHeaderCells) do
  begin
    FHeaderLabels[LColumn].textContent := QueryHeader(LColumn);
    FHeaderSorts[LColumn].textContent := QueryHeader(LColumn);
    FHeaderSorts[LColumn].setAttribute('aria-label', 'Sort ' + QueryHeader(LColumn) +
      '. Activate for ascending, descending, then default order. Shift adds a key.');
    LSortEnabled := (FQueries <> nil) and FView.Spec.TableQueryPolicy.SortEnabled and
      FView.Spec.ColumnAt(LColumn).QueryPolicy.SortEnabled;
    LFilterEnabled := (FQueries <> nil) and FView.Spec.TableQueryPolicy.FilterEnabled and
      FView.Spec.ColumnAt(LColumn).QueryPolicy.FilterEnabled;
    FHeaderLabels[LColumn].style.setProperty('display', '');
    FHeaderSorts[LColumn].style.setProperty('display', 'none');
    FHeaderFilters[LColumn].style.setProperty('display', 'none');

    if LSortEnabled then
    begin
      FHeaderLabels[LColumn].style.setProperty('display', 'none');
      FHeaderSorts[LColumn].style.setProperty('display', '');
    end;

    if LFilterEnabled then
    begin
      FHeaderFilters[LColumn].style.setProperty('display', '');
    end;

    if FEnabled then
    begin
      FHeaderSorts[LColumn].removeAttribute('disabled');
      FHeaderFilters[LColumn].removeAttribute('disabled');
    end
    else
    begin
      FHeaderSorts[LColumn].setAttribute('disabled', '');
      FHeaderFilters[LColumn].setAttribute('disabled', '');
    end;
    FHeaderCells[LColumn].setAttribute('aria-sort', 'none');
    for LIndex := 0 to FView.QueryPolicy.SortCount - 1 do
    begin
      LSort := FView.QueryPolicy.SortAt(LIndex);

      if LSort.FieldName = FView.Spec.ColumnAt(LColumn).FieldName then
      begin

        if LIndex > 0 then
        begin
          FHeaderCells[LColumn].setAttribute('aria-sort', 'other');
        end
        else if LSort.Direction = nsdAscending then
        begin
          FHeaderCells[LColumn].setAttribute('aria-sort', 'ascending');
        end
        else
        begin
          FHeaderCells[LColumn].setAttribute('aria-sort', 'descending');
        end;
      end;
    end;
  end;
end;

procedure TBrowserMount.QueryColumn(AColumn: Integer; AFilter, AAppend: Boolean);
var
  LQueries: INyxCollectionQueryWorkbench;
  LIndex: Integer;
  LSort: TNyxCollectionSort;
  LObservation: TNyxQueryWorkbenchObservation;
  LField: TNyxText;
  LAscending, LReset: Boolean;
begin

  if not GetConnected or not FEnabled or FUpdating or (FQueries = nil) or
    (AColumn < 0) or (AColumn >= FView.Spec.Count) then
  begin
    Exit;
  end;
  LQueries := FQueries;
  try

    if AFilter then
    begin

      if FView.Spec.TableQueryPolicy.FilterEnabled and
        FView.Spec.ColumnAt(AColumn).QueryPolicy.FilterEnabled then
      begin
        LQueries.OpenColumn(AColumn);
      end
      else
      begin
        LQueries.OpenSort;
      end;
      Exit;
    end;
    LAscending := True;
    LReset := False;
    LField := FView.Spec.ColumnAt(AColumn).FieldName;
    LObservation := LQueries.Observation;

    if not GetConnected or not FEnabled or (FQueries <> LQueries) then
    begin
      Exit;
    end;
    for LIndex := 0 to LObservation.Query.SortCount - 1 do
    begin
      LSort := LObservation.Query.SortAt(LIndex);

      if LObservation.HasUserSort and (LSort.FieldName = LField) then
      begin
        LAscending := False;
        LReset := LSort.Direction = nsdDescending;
        Break;
      end;
    end;

    if LReset then
    begin
      LQueries.ResetSort;
    end
    else if LAscending then
    begin
      LQueries.SortColumn(AColumn, nsdAscending, AAppend);
    end
    else
    begin
      LQueries.SortColumn(AColumn, nsdDescending, AAppend);
    end;
  except
    on LException: Exception do
    begin
      Failed(LException);
    end;
  end;
end;

function TBrowserMount.QueryClick(AEvent: TEventListenerEvent): Boolean;
var
  LKeep: INyxCollectionMount;
  LButton: TJSHTMLElement;
  LColumn: Integer;
  LFilter: Boolean;
begin
  Result := True;
  LKeep := Self as INyxCollectionMount;

  if not GetConnected or not FEnabled or AEvent.defaultPrevented or
    not (AEvent.target is TJSHTMLElement) then
  begin
    Exit;
  end;
  LButton := TJSHTMLElement(TJSHTMLElement(AEvent.target).closest(
    '[data-nyx-query-column],[data-nyx-query-sort]'));

  if (LButton = nil) or not FHost.contains(LButton) then
  begin
    Exit;
  end;
  LFilter := LButton.hasAttribute('data-nyx-query-column');
  LColumn := StrToIntDef(LButton.getAttribute('data-nyx-query-sort'), -1);

  if LFilter then
  begin
    LColumn := StrToIntDef(LButton.getAttribute('data-nyx-query-column'), -1);
  end;
  AEvent.preventDefault;
  QueryColumn(LColumn, LFilter, TJSMouseEvent(AEvent).shiftKey);
end;

function TBrowserMount.QueryKey(AEvent: TJSKeyboardEvent): Boolean;
var
  LKeep: INyxCollectionMount;
  LElement, LHeader: TJSHTMLElement;
  LColumn: Integer;
  LFilter: Boolean;
begin
  Result := True;
  LKeep := Self as INyxCollectionMount;

  if not GetConnected or not FEnabled or (FQueries = nil) or
    AEvent.defaultPrevented or AEvent.isComposing or
    not (AEvent.target is TJSHTMLElement) then
  begin
    Exit;
  end;
  LElement := TJSHTMLElement(AEvent.target);

  if (LowerCase(LElement.tagName) = 'input') or
    (LowerCase(LElement.tagName) = 'textarea') or
    (LowerCase(LElement.tagName) = 'select') then
  begin
    Exit;
  end;
  LColumn := FFocusedColumn;
  LHeader := TJSHTMLElement(LElement.closest('[data-nyx-query-column],[data-nyx-query-sort]'));
  LFilter := False;

  if LHeader <> nil then
  begin
    LFilter := LHeader.hasAttribute('data-nyx-query-column');
    LColumn := StrToIntDef(LHeader.getAttribute('data-nyx-query-sort'), -1);

    if LFilter then
    begin
      LColumn := StrToIntDef(LHeader.getAttribute('data-nyx-query-column'), -1);
    end;
  end;

  if (AEvent.altKey and (AEvent.key = 'ArrowDown')) or
    ((LHeader <> nil) and not AEvent.ctrlKey and not AEvent.metaKey and
      ((AEvent.key = 'Enter') or (AEvent.key = ' '))) then
  begin
    AEvent.preventDefault;
    QueryColumn(LColumn, LFilter or AEvent.altKey, AEvent.shiftKey);
  end;
end;

procedure TBrowserMount.DetachTarget;
var
  LIndex: Integer;
begin

  if FHost <> nil then
  begin
    FHost.removeEventListener('keydown', @Key);
    FHost.removeEventListener('keydown', @QueryKey);
    FHost.removeEventListener('click', @QueryClick);
    FHost.removeEventListener('focusin', @WindowChanged);
    FHost.removeEventListener('focusin', @EditorFocusChanged);
    FHost.removeEventListener('focusout', @EditorFocusChanged);
  end;
  TCapturedDocument(document).removeEventListener('scroll', @ScrollChanged, True);
  window.removeEventListener('resize', @WindowChanged);

  if FWindowQueued then
  begin
    window.cancelAnimationFrame(FWindowFrame);
    FWindowQueued := False;
  end;

  if FWindowObserver <> nil then
  begin
    FWindowObserver.disconnect;
    FWindowObserver := nil;
  end;
  FWindowAncestors := nil;
  FGeometry.Free;
  FGeometry := nil;
  for LIndex := 0 to Length(FRows) - 1 do
  begin
    FRows[LIndex].Free;
  end;
  FRows := nil;
  FTreeRows := nil;
  FTreeGeometryRows := nil;
  FResidentRows := nil;
  FRevealItem := Default(TNyxItemRef);
  for LIndex := 0 to Length(FSpacers) - 1 do
  begin
    FSpacers[LIndex].remove;
  end;
  FSpacers := nil;
  FRendered := nil;
  FHeaderCells := nil;
  FHeaderLabels := nil;
  FHeaderSorts := nil;
  FHeaderFilters := nil;
  FBody := nil;
  FHost := nil;
end;

function MountNyxBrowserCollection(AHost: TJSHTMLElement;
  const AView: INyxCollectionView): INyxCollectionMount;
var
  LMount: TBrowserMount;
begin
  LMount := TBrowserMount.Create(AHost, AView);
  Result := LMount;
  LMount.Activate;
end;

end.
