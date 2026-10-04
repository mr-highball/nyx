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
  This adapter materializes all rows; production virtualization remains separate. }
function MountNyxBrowserCollection(AHost: TJSHTMLElement;
  const AView: INyxCollectionView): INyxCollectionMount;

implementation

uses
  SysUtils,
  nyx.text,
  nyx.state,
  nyx.collections,
  nyx.collections.view.types,
  nyx.collections.selection;

type
  { Typed bridge to standard DOM focus options, missing from older Web units. }
  TFocusOptions = class external name 'Object' (TJSObject)
    preventScroll: Boolean;
  end;
  TFocusable = class external name 'HTMLElement' (TJSHTMLElement)
    procedure focus(AOptions: TFocusOptions); reintroduce;
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
    FLabels: array of TJSHTMLElement;
    FInputs: array of TJSHTMLInputElement;
    function Click(AEvent: TJSMouseEvent): Boolean;
    function Change(AEvent: TEventListenerEvent): Boolean;
    function Toggle(AEvent: TEventListenerEvent): Boolean;
  public
    constructor Create(AOwner: TBrowserMount; const ARef: TNyxItemRef);
    destructor Destroy; override;
    procedure Sync;
  end;

  TBrowserMount = class(TNyxCollectionMountBase)
  private
    FHost: TJSHTMLElement;
    FBody: TJSHTMLElement;
    FRows: array of TBrowserRow;
    FRendered: INyxCollectionSnapshot;
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
    LHeader.appendChild(LRow);
    for LIndex := 0 to AView.Spec.Count - 1 do
    begin
      LCell := NewElement('th');
      LCell.setAttribute('role', 'columnheader');
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
  for LIndex := 0 to LColumns - 1 do
  begin
    LCell := FElement;

    if FOwner.FView.Projection = cpTable then
    begin
      LCell := NewElement('td');
      LCell.setAttribute('role', 'gridcell');
      FElement.appendChild(LCell);
    end
    else if FOwner.FView.Projection = cpTree then
    begin
      LCell := LCaption;
    end;
    LColumn := FOwner.FView.Spec.ColumnAt(LIndex);

    if (LColumn.Mode = cmEditable) and (FOwner.FView.Projection <> cpList) then
    begin
      FInputs[LIndex] := TJSHTMLInputElement(document.createElement('input'));
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
end;

destructor TBrowserRow.Destroy;
var
  LIndex: Integer;
begin

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

procedure TBrowserRow.Sync;
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
    LText := FOwner.FView.CellText(FRef, LIndex);
    LApplyValues := LPrevious < 0;

    if not LApplyValues and (FOwner.FRendered.Revision <> LData.Revision) then
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
      FInputs[LIndex].disabled := not FOwner.FEnabled or FOwner.FReadOnly;
      FInputs[LIndex].readOnly := FOwner.FReadOnly;

      if LApplyValues then
      begin

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
    else
    begin
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

  if (FOwner.FView.Selection.Focus.Defined and
    (FOwner.FView.Selection.Focus.ID = FRef.ID)) or
    (not FOwner.FView.Selection.Focus.Defined and
      (FOwner.FView.Snapshot.IndexOf(FRef) = 0)) then
  begin
    FElement.setAttribute('tabindex', '0');
  end;

  if FOwner.FView.Projection = cpTree then
  begin
    Toggle(nil);
  end;
end;

function TBrowserRow.Toggle(AEvent: TEventListenerEvent): Boolean;
begin
  Result := True;
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
begin
  { A nested tree item owns its selection; bubbling must not select its parent. }
  AEvent.stopPropagation;

  if AEvent.defaultPrevented then
  begin
    Exit(True);
  end;
  { Selecting can notify an application observer that unmounts this whole view.
    Retain the attachment and capture the host before any row is detached. Never
    touch this row after publication; Disconnect may have destroyed it. }
  LMount := FOwner as INyxCollectionMount;
  LHost := FOwner.FHost;
  LFocus := FElement;
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
  LHasTabStop: Boolean;
begin
  LData := FView.Snapshot;
  LFocused := TJSHTMLElement(document.activeElement);

  if not FHost.contains(LFocused) then
  begin
    LFocused := nil;
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
    LNext[LIndex].Sync;
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
  { Dataset order can put a child before its parent. A collapsed child must not
    become the only keyboard entry point. Retain a visible focused row when one
    exists, otherwise admit the first row in the actual rendered hierarchy. }
  LVisible := VisibleOrder;
  LHasTabStop := False;
  for LIndex := 0 to Length(LVisible) - 1 do
  begin
    LPrevious := LData.IndexOf(LVisible[LIndex]);
    LHasTabStop := LHasTabStop or
      (FRows[LPrevious].FElement.getAttribute('tabindex') = '0');
  end;

  if not LHasTabStop and (Length(LVisible) > 0) then
  begin
    for LIndex := 0 to Length(FRows) - 1 do
    begin
      FRows[LIndex].FElement.setAttribute('tabindex', '-1');
    end;
    LPrevious := LData.IndexOf(LVisible[0]);
    FRows[LPrevious].FElement.setAttribute('tabindex', '0');
  end;

  if (LFocused <> nil) and FHost.contains(LFocused) and
    (document.activeElement <> LFocused) then
  begin
    LOptions := TFocusOptions.new;
    LOptions.preventScroll := True;
    TFocusable(LFocused).focus(LOptions);
  end;
end;

procedure TBrowserMount.DetachTarget;
var
  LIndex: Integer;
begin

  if FHost <> nil then
  begin
    FHost.removeEventListener('keydown', @Key);
  end;
  for LIndex := 0 to Length(FRows) - 1 do
  begin
    FRows[LIndex].Free;
  end;
  FRows := nil;
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
