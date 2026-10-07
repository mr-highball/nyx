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
  Tables realize a measured viewport window, retaining focused editors and exact
  offscreen drafts. Lists/trees still materialize rows; full budgets remain open. }
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
  nyx.collections.window,
  nyx.collections.selection;

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
    FFocusedColumn: Integer;
    { Numerical geometry is owned; row slots are logical indices and may be nil.
      Only realized rows and explicit unfinished drafts own controls. }
    FGeometry: TNyxCollectionRowGeometry;
    FSpacers: array of TJSHTMLElement;
    FForcedItem: TNyxItemRef;
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
    procedure TableViewport(out AOffset, AExtent: Double);
    procedure RenderTableWindow;
    { Realizes a logical destination before navigation publishes selection.
      It does not scroll or rewrite source/history. Normal focus reveals it. }
    procedure EnsureRow(AIndex: Integer);
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
    for LIndex := 0 to AView.Spec.Count - 1 do
    begin
      LCell := NewElement('th');
      LCell.setAttribute('role', 'columnheader');
      LCell.setAttribute('aria-colindex', IntToStr(LIndex + 1));
      LCell.textContent := AView.Spec.ColumnAt(LIndex).Title;
      LCell.setAttribute('scope', 'col');
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
  FHost.addEventListener('keydown', @Key);

  if AView.Projection = cpTable then
  begin
    FGeometry := TNyxCollectionRowGeometry.Create(0, 32);
    document.addEventListener('scroll', @ScrollChanged, True);
    window.addEventListener('resize', @WindowChanged);
    FHost.addEventListener('focusin', @WindowChanged);

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
    end;
    LColumn := FOwner.FView.Spec.ColumnAt(LIndex);
    FCells[LIndex] := LCell;

    if (LColumn.Mode = cmEditable) and (FOwner.FView.Projection <> cpList) then
    begin
      FInputs[LIndex] := TJSHTMLInputElement(document.createElement('input'));
      { The composite has one Tab entry in navigation mode. F2/Enter enters an
        owned editor; its Tab/Escape handling then preserves keyboard access to
        columns without putting every field in the surrounding page's Tab order. }
      FInputs[LIndex].setAttribute('tabindex', '-1');
      FInputs[LIndex].setAttribute('aria-label', LColumn.Title);
      FInputs[LIndex].setAttribute('data-nyx-column', IntToStr(LIndex));
      FInputs[LIndex].onchange := Change;
      FInputs[LIndex]._type := 'text';

      if LColumn.Kind = nskBoolean then
      begin
        FInputs[LIndex]._type := 'checkbox';
      end
      else if LColumn.Kind in [nskInteger, nskNumber] then
      begin
        FInputs[LIndex].setAttribute('inputmode', 'decimal');
      end;
      LCell.appendChild(FInputs[LIndex]);
    end
    else
    begin
      FLabels[LIndex] := NewElement('span');
      LCell.appendChild(FLabels[LIndex]);
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

procedure TBrowserMount.TableViewport(out AOffset, AExtent: Double);
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

  if FBody.children.length = 0 then
  begin
    { An empty tbody can report the zero rectangle on an already attached host.
      The actual header end establishes its prospective first data-row origin. }
    LBodyTop := TJSHTMLElement(FHost.querySelector('thead')).getBoundingClientRect.bottom;
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

procedure TBrowserMount.RenderTableWindow;
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
    LRow := NewElement('tr');
    LRow.setAttribute('aria-hidden', 'true');
    LRow.setAttribute('data-nyx-row-spacer', 'true');
    LCell := NewElement('td');
    LCell.setAttribute('colspan', IntToStr(FView.Spec.Count));
    { CSS uses a decimal point independently of application/user RTL locale. }
    LFormat := FormatSettings;
    LFormat.DecimalSeparator := '.';
    LCell.style.setProperty('height', FloatToStr(LPixels, LFormat) + 'px');
    LCell.style.setProperty('padding', '0');
    LCell.style.setProperty('border', '0');
    LCell.style.setProperty('line-height', '0');
    LRow.appendChild(LCell);
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
  LFocused := TJSHTMLElement(document.activeElement);
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
  SetLength(LNext, LData.Count);
  SetLength(LUsed, Length(FRows));
  for LIndex := 0 to LData.Count - 1 do
  begin
    LPrevious := -1;

    if LSameSnapshot then
    begin
      { Scrolling/selection keeps this exact immutable snapshot. Copy only the
        private row slots; avoid redundant source identity lookup/remapping. }
      LPrevious := LIndex;
    end
    else if FRendered <> nil then
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
    end;
  end;

  if not LSameOrder then
  begin
    FGeometry.Reset(LData.Count, CEstimate);
  end;
  TableViewport(LOffset, LExtent);
  LWindow := FGeometry.Window(LOffset, LExtent, COverscan);
  LRowGap := 0;

  if window.getComputedStyle(FHost).getPropertyValue('border-collapse') <> 'collapse' then
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
  for LIndex := 0 to LData.Count - 1 do
  begin
    LRealize := ((LIndex >= LWindow.First) and (LIndex < LWindow.AfterLast)) or
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
        LNext[LIndex].FElement.setAttribute('aria-rowindex', IntToStr(LIndex + 2));

        if (LTabStop = nil) or (FView.Selection.Focus.Defined and
          (LData.ItemAt(LIndex).Ref.ID = FView.Selection.Focus.ID)) then
        begin
          LTabStop := LNext[LIndex].FCells[FFocusedColumn];
        end;
      end;
    end;
  end;
  for LIndex := 0 to Length(FRows) - 1 do
  begin

    if not LUsed[LIndex] then
    begin
      FRows[LIndex].Free;
    end;
  end;
  FRows := LNext;
  for LIndex := 0 to Length(FSpacers) - 1 do
  begin
    FSpacers[LIndex].remove;
  end;
  FSpacers := nil;
  LCursor := 0;
  LPosition := 0;
  for LIndex := 0 to Length(FRows) - 1 do
  begin

    if (FRows[LIndex] <> nil) and
      (((LIndex >= LWindow.First) and (LIndex < LWindow.AfterLast)) or
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
  for LIndex := 0 to Length(FRows) - 1 do
  begin

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

procedure TBrowserMount.RenderDataset;
var
  LNext: array of TBrowserRow;
  LData: INyxCollectionSnapshot;
  LIndex: Integer;
  LPrevious: Integer;
  LParent: Integer;
  LDestination: TJSHTMLElement;
  LFocused: TJSHTMLElement;
  LOptions: TFocusOptions;
  LRootPosition: Integer;
  LPositions: array of Integer;
  LAnchor: TJSNode;
  LHierarchyChanged: Boolean;
  LVisible: TNyxItemRefs;
  LTabStop: TJSHTMLElement;
  LFocusID: TNyxText;
  LFocusVisible: Boolean;
  LFocusRow: TJSHTMLElement;
  LNextFocus: TJSHTMLElement;
begin
  LData := FView.Snapshot;
  FHost.setAttribute('aria-disabled', LowerCase(BoolToStr(not FEnabled, True)));

  if FView.Projection = cpTable then
  begin
    FHost.setAttribute('aria-readonly', LowerCase(BoolToStr(FReadOnly, True)));
    FHost.setAttribute('aria-rowcount', IntToStr(LData.Count + 1));
    FHost.setAttribute('aria-colcount', IntToStr(FView.Spec.Count));
    RenderTableWindow;
    Exit;
  end;
  LFocused := TJSHTMLElement(document.activeElement);

  if not FHost.contains(LFocused) then
  begin
    LFocused := nil;
  end;
  LFocusID := '';

  if LFocused <> nil then
  begin
    LFocusRow := TJSHTMLElement(LFocused.closest('[data-nyx-item]'));

    if LFocusRow <> nil then
    begin
      LFocusID := LFocusRow.getAttribute('data-nyx-item');
    end;
  end;
  SetLength(LNext, LData.Count);
  LHierarchyChanged := False;
  for LIndex := 0 to LData.Count - 1 do
  begin
    LPrevious := -1;

    if FRendered <> nil then
    begin
      LPrevious := FRendered.IndexOf(LData.ItemAt(LIndex).Ref);
    end;

    if LPrevious >= 0 then
    begin
      LNext[LIndex] := FRows[LPrevious];

      if (FView.Spec.ParentField <> '') and
        (LData.ItemAt(LIndex).GetValue(NyxTextField(FView.Spec.ParentField)) <>
        FRendered.ItemAt(LPrevious).GetValue(NyxTextField(FView.Spec.ParentField))) then
      begin
        LHierarchyChanged := True;
      end;
    end
    else
    begin
      LNext[LIndex] := TBrowserRow.Create(Self, LData.ItemAt(LIndex).Ref);
    end;
    LNext[LIndex].Sync(FRefreshPlan.ValuesChanged(LIndex));

    if FView.Projection = cpTable then
    begin
      LNext[LIndex].FElement.setAttribute('aria-rowindex', IntToStr(LIndex + 2));
    end;
  end;
  { First relocate surviving tree rows, so removing an ancestor never destroys
    a retained child that was reparented in the same atomic batch. }

  if LHierarchyChanged then
  begin
    { Flatten old parent relationships before applying the new acyclic graph.
      Otherwise a valid parent swap can temporarily place an ancestor inside its
      old descendant. Preserve nodes/listeners and restore focus without scroll. }
    for LIndex := 0 to Length(LNext) - 1 do
    begin
      FBody.appendChild(LNext[LIndex].FElement);
    end;
  end;
  SetLength(LPositions, Length(LNext));
  LRootPosition := 0;
  for LIndex := 0 to Length(LNext) - 1 do
  begin
    LDestination := FBody;
    LParent := FView.ParentIndex(LIndex);

    if LParent >= 0 then
    begin
      LDestination := LNext[LParent].FChildren;
    end;
    LPrevious := LRootPosition;

    if LParent >= 0 then
    begin
      LPrevious := LPositions[LParent];
      Inc(LPositions[LParent]);
    end
    else
    begin
      Inc(LRootPosition);
    end;
    LAnchor := nil;

    if LPrevious < LDestination.children.length then
    begin
      LAnchor := LDestination.children[LPrevious];
    end;

    if LNext[LIndex].FElement <> LAnchor then
    begin
      LDestination.insertBefore(LNext[LIndex].FElement, LAnchor);
    end;
  end;
  for LIndex := 0 to Length(FRows) - 1 do
  begin

    if not LData.Has(FRows[LIndex].FRef) then
    begin
      FRows[LIndex].Free;
    end;
  end;
  FRows := LNext;
  FRendered := LData;
  { Parent relationships are now final. Disclosure metadata must use those
    relationships, rather than children borrowed from the previous dataset. }

  if FView.Projection = cpTree then
  begin
    for LIndex := 0 to Length(FRows) - 1 do
    begin
      FRows[LIndex].Toggle(nil);
    end;
  end;
  { Dataset order can put a child before its parent. A collapsed child must not
    become the only keyboard entry point. Retain a visible focused row when one
    exists, otherwise admit the first row in the actual rendered hierarchy. }
  LVisible := VisibleOrder;
  LTabStop := nil;
  LFocusVisible := False;
  for LIndex := 0 to Length(LVisible) - 1 do
  begin
    LPrevious := LData.IndexOf(LVisible[LIndex]);

    if LVisible[LIndex].ID = LFocusID then
    begin
      LFocusVisible := True;
    end;

    if (LTabStop = nil) or (FView.Selection.Focus.Defined and
      (LVisible[LIndex].ID = FView.Selection.Focus.ID)) then
    begin
      LTabStop := FRows[LPrevious].FElement;

      if FView.Projection = cpTable then
      begin
        LTabStop := FRows[LPrevious].FCells[FFocusedColumn];
      end;
    end;
  end;
  FHost.setAttribute('tabindex', '-1');

  if FEnabled then
  begin

    if LTabStop <> nil then
    begin
      LTabStop.setAttribute('tabindex', '0');
    end
    else
    begin
      { Empty composites retain one focusable host. They do not invent a
        selected item, and users can still reach their label and leave by Tab. }
      FHost.setAttribute('tabindex', '0');
    end;
  end;
  LNextFocus := nil;

  if (LFocused <> nil) and FEnabled then
  begin
    LNextFocus := LTabStop;

    if LNextFocus = nil then
    begin
      LNextFocus := FHost;
    end;

    if LFocusVisible and FHost.contains(LFocused) and
      not ((LFocused is TJSHTMLInputElement) and TJSHTMLInputElement(LFocused).disabled) then
    begin
      LNextFocus := LFocused;
    end;
  end;

  if (LNextFocus <> nil) and (document.activeElement <> LNextFocus) then
  begin
    { Recover only focus that belonged to this composite. Surviving editors keep
      their caret/draft; removed or hidden rows move to the admitted fallback.
      The focus call may navigate/disconnect the mount: no borrowed target is
      read afterward, and Refresh retains the interface call frame. }
    LOptions := TFocusOptions.new;
    LOptions.preventScroll := True;
    TFocusable(LNextFocus).focus(LOptions);
  end;
end;

procedure TBrowserMount.DetachTarget;
var
  LIndex: Integer;
begin

  if FHost <> nil then
  begin
    FHost.removeEventListener('keydown', @Key);
    FHost.removeEventListener('focusin', @WindowChanged);
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
  for LIndex := 0 to Length(FSpacers) - 1 do
  begin
    FSpacers[LIndex].remove;
  end;
  FSpacers := nil;
  FRendered := nil;
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
