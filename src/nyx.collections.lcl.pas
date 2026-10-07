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
unit nyx.collections.lcl;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  Controls,
  nyx.collections.view,
  nyx.collections.mount;

{ Native adapter boundary for Nyx's real TListBox/TStringGrid/TTreeView controls.
  Renderer retains/disconnects the mount before destroying its borrowed control.
  No dataset is serialized into newline/tab-separated items. Lists select rows;
  grids and tree labels edit permitted columns through atomic typed admission. }
function MountNyxLCLCollection(AControl: TControl;
  const AView: INyxCollectionView): INyxCollectionMount;

implementation

uses
  Classes,
  SysUtils,
  Math,
  StdCtrls,
  ComCtrls,
  Grids,
  Graphics,
  LCLType,
  nyx.text,
  nyx.state,
  nyx.data,
  nyx.collections,
  nyx.collections.grid,
  nyx.collections.view.types,
  nyx.typeahead,
  nyx.collections.selection;

type
  TNativeMount = class(TNyxCollectionMountBase)
  private
    FList: TListBox;
    FGrid: TStringGrid;
    FTree: TTreeView;
    FNodes: array of TTreeNode;
    FRendered: INyxCollectionSnapshot;
    FPreviousSelection: TSelectionChangeEvent;
    FPreviousCell: TOnSelectCellEvent;
    FPreviousValidate: TValidateEntryEvent;
    FPreviousTreeChange: TTVChangedEvent;
    FPreviousTreeEdit: TTVEditedEvent;
    FPreviousOptions: TGridOptions;
    FPreviousReadOnly: Boolean;
    FPreviousKey: TKeyEvent;
    FPreviousUTF8: TUTF8KeyPressEvent;
    FRejectTextKey: Boolean;
    FConsumedTextKey: Boolean;
    FPreviousMouse: TMouseEvent;
    FPreviousPrepare: TOnPrepareCanvasEvent;
    FPreviousTreeSelection: TNotifyEvent;
    FPreviousMultiSelect: Boolean;
    FPreviousExtendedSelect: Boolean;
    FPreviousMultiStyle: TMultiSelectStyle;
    FGridShift: TShiftState;
    procedure ListSelected(Sender: TObject; User: Boolean);
    procedure GridSelected(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure GridEdited(Sender: TObject; ACol, ARow: Integer;
      const OldValue: String; var NewValue: String);
    procedure TreeSelected(Sender: TObject; ANode: TTreeNode);
    procedure TreeSelectionChanged(Sender: TObject);
    procedure KeyDown(Sender: TObject; var AKey: Word; AShift: TShiftState);
    procedure UTF8KeyPress(Sender: TObject; var AKey: TUTF8Char);
    procedure GridMouseDown(Sender: TObject; AButton: TMouseButton;
      AShift: TShiftState; AX, AY: Integer);
    procedure PrepareGrid(Sender: TObject; AColumn, ARow: Integer; AState: TGridDrawState);
    function VisibleOrder: TNyxItemRefs;
    procedure TreeEdited(Sender: TObject; ANode: TTreeNode; var AText: String);
    function NodeIndex(ANode: TTreeNode): Integer;
    function DisplayText(const AValue: TNyxText): TNyxText;
    function EditText(const AItem: TNyxItemRef; AColumn: Integer;
      const AValue: TNyxText): TNyxText;
  protected
    procedure RenderDataset; override;
    procedure DetachTarget; override;
  public
    constructor Create(AControl: TControl; const AView: INyxCollectionView);
  end;

{$include nyx.collections.lcl.selection.inc}

constructor TNativeMount.Create(AControl: TControl; const AView: INyxCollectionView);
var
  LIndex: Integer;
begin
  inherited Create(AView);
  { LCL may establish its initial cell/cursor while creating a widget handle.
    Admit that native initialization before installing selection observers; it
    must not turn an empty portable selection into a user selection. The mounted
    control already belongs to its host, including a hidden renderer candidate. }

  if AControl is TWinControl then
  begin
    TWinControl(AControl).HandleNeeded;
  end;
  case AView.Projection of
    cpList:
      begin

        if not (AControl is TListBox) then
        begin
          raise ENyxCollection.Create('List binding requires a native list box');
        end;
        FList := TListBox(AControl);
        FPreviousSelection := FList.OnSelectionChange;
        FList.OnSelectionChange := ListSelected;
        FPreviousKey := FList.OnKeyDown;
        FList.OnKeyDown := KeyDown;
        FPreviousUTF8 := FList.OnUTF8KeyPress;
        FList.OnUTF8KeyPress := UTF8KeyPress;
        FPreviousMultiSelect := FList.MultiSelect;
        FPreviousExtendedSelect := FList.ExtendedSelect;
        FList.MultiSelect := AView.Spec.SelectionMode = nsmMultiple;
        FList.ExtendedSelect := True;
      end;
    cpTable:
      begin

        if not (AControl is TStringGrid) then
        begin
          raise ENyxCollection.Create('Table binding requires a native string grid');
        end;
        FGrid := TStringGrid(AControl);
        FPreviousCell := FGrid.OnSelectCell;
        FPreviousValidate := FGrid.OnValidateEntry;
        FPreviousOptions := FGrid.Options;
        FGrid.OnSelectCell := GridSelected;
        FGrid.OnValidateEntry := GridEdited;
        FPreviousKey := FGrid.OnKeyDown;
        FPreviousMouse := FGrid.OnMouseDown;
        FPreviousPrepare := FGrid.OnPrepareCanvas;
        FGrid.OnKeyDown := KeyDown;
        FGrid.OnMouseDown := GridMouseDown;
        FGrid.OnPrepareCanvas := PrepareGrid;
        for LIndex := 0 to AView.Spec.Count - 1 do
        begin

          if AView.Spec.ColumnAt(LIndex).Mode = cmEditable then
          begin
            FGrid.Options := FGrid.Options + [goEditing];
          end;
        end;
      end;
    cpTree:
      begin

        if not (AControl is TTreeView) then
        begin
          raise ENyxCollection.Create('Tree binding requires a native tree view');
        end;
        FTree := TTreeView(AControl);
        FPreviousTreeChange := FTree.OnChange;
        FPreviousTreeEdit := FTree.OnEdited;
        FPreviousReadOnly := FTree.ReadOnly;
        FTree.OnChange := TreeSelected;
        FTree.OnEdited := TreeEdited;
        FTree.ReadOnly := AView.Spec.ColumnAt(0).Mode <> cmEditable;
        FPreviousKey := FTree.OnKeyDown;
        FTree.OnKeyDown := KeyDown;
        FPreviousUTF8 := FTree.OnUTF8KeyPress;
        FTree.OnUTF8KeyPress := UTF8KeyPress;
        FPreviousTreeSelection := FTree.OnSelectionChanged;
        FTree.OnSelectionChanged := TreeSelectionChanged;
        FPreviousMultiSelect := FTree.MultiSelect;
        FPreviousMultiStyle := FTree.MultiSelectStyle;
        FTree.MultiSelect := AView.Spec.SelectionMode = nsmMultiple;
        FTree.MultiSelectStyle := [msControlSelect, msShiftSelect, msVisibleOnly];
      end;
  end;
end;

function TNativeMount.DisplayText(const AValue: TNyxText): TNyxText;
begin
  Result := AValue;
  { Win32 widgets treat embedded NUL as an end marker. An explicit quoted JSON
    representation preserves the entire value and distinguishes literal escapes.
    Ordinary text remains ordinary text; no process-wide codepage is changed. }

  if Pos(#0, AValue) > 0 then
  begin
    Result := NyxData(AValue).ToJSON;
  end;
end;

function TNativeMount.EditText(const AItem: TNyxItemRef; AColumn: Integer;
  const AValue: TNyxText): TNyxText;
begin
  Result := AValue;

  if (FView.Spec.ColumnAt(AColumn).Kind = nskText) and
    (Pos(#0, FView.CellText(AItem, AColumn)) > 0) then
  begin
    Result := TNyxDataValue.ParseJSON(AValue).AsText;
  end;
end;

procedure TNativeMount.ListSelected(Sender: TObject; User: Boolean);
var
  LKeepAlive: INyxCollectionMount;
  LItems: TNyxItemRefs;
  LFocus: TNyxItemRef;
  LAnchor: TNyxItemRef;
  LIndex: Integer;
  LCount: Integer;
begin

  if FUpdating or not GetConnected then
  begin
    Exit;
  end;
  LKeepAlive := Self as INyxCollectionMount;

  if not FEnabled then
  begin
    Refresh;
    Exit;
  end;

  if FView.Spec.SelectionMode = nsmMultiple then
  begin
    SetLength(LItems, FView.Snapshot.Count);
    LCount := 0;
    LFocus := Default(TNyxItemRef);
    for LIndex := 0 to FView.Snapshot.Count - 1 do
    begin

      if FList.Selected[LIndex] then
      begin
        LItems[LCount] := FView.Snapshot.ItemAt(LIndex).Ref;
        Inc(LCount);
      end;
    end;
    SetLength(LItems, LCount);

    if (FList.ItemIndex >= 0) and (FList.ItemIndex < FView.Snapshot.Count) then
    begin
      LFocus := FView.Snapshot.ItemAt(FList.ItemIndex).Ref;
    end;
    LAnchor := FView.Selection.Anchor;

    if not (ssShift in GetKeyShiftState) then
    begin
      LAnchor := LFocus;
    end;
    SetSelection(LItems, LFocus, LAnchor);
  end
  else if (FList.ItemIndex >= 0) and (FList.ItemIndex < FView.Snapshot.Count) then
  begin
    Select(FView.Snapshot.ItemAt(FList.ItemIndex).Ref);
  end
  else
  begin
    FView.ClearSelection;
  end;

  if GetConnected and Assigned(FPreviousSelection) then
  begin
    FPreviousSelection(Sender, User);
  end;
end;

procedure TNativeMount.GridSelected(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
var
  LKeepAlive: INyxCollectionMount;
begin

  if FUpdating or not GetConnected then
  begin
    Exit;
  end;
  LKeepAlive := Self as INyxCollectionMount;

  if not FEnabled then
  begin
    CanSelect := False;
    Refresh;
    Exit;
  end;

  if Assigned(FPreviousCell) then
  begin
    FPreviousCell(Sender, ACol, ARow, CanSelect);
  end;

  if GetConnected and CanSelect and (ARow > 0) and (ARow <= FView.Snapshot.Count) then
  begin
    { Shared row membership is independent of the grid's cell/editor cursor. }

    if ssShift in FGridShift then
    begin
      SelectRange(FView.Snapshot.ItemAt(ARow - 1).Ref, VisibleOrder,
        ssCtrl in FGridShift);
    end
    else if ssCtrl in FGridShift then
    begin
      Select(FView.Snapshot.ItemAt(ARow - 1).Ref, nsaToggle);
    end
    else
    begin
      Select(FView.Snapshot.ItemAt(ARow - 1).Ref);
    end;
    FGridShift := [];
  end;
end;

procedure TNativeMount.GridEdited(Sender: TObject; ACol, ARow: Integer;
  const OldValue: String; var NewValue: String);
var
  LItem: TNyxItemRef;
  LWire: TNyxText;
  LKeepAlive: INyxCollectionMount;
  LView: INyxCollectionView;
begin

  if FUpdating or not GetConnected then
  begin
    Exit;
  end;
  LKeepAlive := Self as INyxCollectionMount;

  if (ARow < 1) or (ARow > FView.Snapshot.Count) or
    (ACol < 0) or (ACol >= FView.Spec.Count) then
  begin
    NewValue := OldValue;
    Exit;
  end;

  if Assigned(FPreviousValidate) then
  begin
    FPreviousValidate(Sender, ACol, ARow, OldValue, NewValue);
  end;

  if not GetConnected or (ARow > FView.Snapshot.Count) then
  begin
    NewValue := OldValue;
    Exit;
  end;
  { An admitted edit may notify an observer that unmounts the control. Keep its
    view independently for the returned accepted text; never borrow the widget
    again after publication. The callback's value belongs to its caller. }
  LView := FView;
  LItem := LView.Snapshot.ItemAt(ARow - 1).Ref;
  try
    LWire := EditText(LItem, ACol, TNyxText(NewValue));
    EditCell(LItem, ACol, LWire);
  except
    { Malformed escaped text is rejected before reaching the shared wire editor.
      Restore the full accepted display, including supplementary text and NUL. }
    on LException: Exception do
    begin
      Failed(LException);
      NewValue := DisplayText(LView.CellText(LItem, ACol));
      Exit;
    end;
  end;
  NewValue := DisplayText(LView.CellText(LItem, ACol));
end;

function TNativeMount.NodeIndex(ANode: TTreeNode): Integer;
var
  LIndex: Integer;
begin
  Result := -1;

  if (ANode = nil) or (PtrUInt(ANode.Data) < 1) or
    (PtrUInt(ANode.Data) > PtrUInt(Length(FNodes))) then
  begin
    Exit;
  end;
  { Mount-owned nodes carry a checked ordinal, never an object pointer or a
    dataset row reference. Validate the pointer too, so a foreign native node
    cannot impersonate a portable item. Visible traversal is therefore linear. }
  LIndex := Integer(PtrUInt(ANode.Data)) - 1;

  if FNodes[LIndex] = ANode then
  begin
    Result := LIndex;
  end;
end;

procedure TNativeMount.TreeSelected(Sender: TObject; ANode: TTreeNode);
var
  LIndex: Integer;
  LKeepAlive: INyxCollectionMount;
begin

  if FUpdating or not GetConnected then
  begin
    Exit;
  end;
  LKeepAlive := Self as INyxCollectionMount;

  if FView.Spec.SelectionMode = nsmMultiple then
  begin

    if Assigned(FPreviousTreeChange) then
    begin
      FPreviousTreeChange(Sender, ANode);
    end;
    Exit;
  end;

  if not FEnabled then
  begin
    Refresh;
    Exit;
  end;
  LIndex := NodeIndex(ANode);

  if LIndex >= 0 then
  begin
    Select(FView.Snapshot.ItemAt(LIndex).Ref);
  end
  else
  begin
    FView.ClearSelection;
  end;

  if GetConnected and Assigned(FPreviousTreeChange) then
  begin
    FPreviousTreeChange(Sender, ANode);
  end;
end;

procedure TNativeMount.TreeEdited(Sender: TObject; ANode: TTreeNode;
  var AText: String);
var
  LIndex: Integer;
  LItem: TNyxItemRef;
  LKeepAlive: INyxCollectionMount;
  LView: INyxCollectionView;
begin

  if FUpdating or not GetConnected then
  begin
    Exit;
  end;
  LKeepAlive := Self as INyxCollectionMount;
  LIndex := NodeIndex(ANode);

  if LIndex < 0 then
  begin
    Exit;
  end;

  if Assigned(FPreviousTreeEdit) then
  begin
    FPreviousTreeEdit(Sender, ANode, AText);
  end;

  if not GetConnected then
  begin
    Exit;
  end;
  LIndex := NodeIndex(ANode);

  if LIndex < 0 then
  begin
    Exit;
  end;
  LView := FView;
  LItem := LView.Snapshot.ItemAt(LIndex).Ref;
  try
    EditCell(LItem, 0, EditText(LItem, 0, TNyxText(AText)));
  except
    on LException: Exception do
    begin
      Failed(LException);
      AText := DisplayText(LView.CellText(LItem, 0));
      Exit;
    end;
  end;
  AText := DisplayText(LView.CellText(LItem, 0));
end;

procedure TNativeMount.RenderDataset;
var
  LData: INyxCollectionSnapshot;
  LIndex: Integer;
  LColumn: Integer;
  LSelected: Integer;
  LPrevious: Integer;
  LParent: Integer;
  LTop: Integer;
  LNext: array of TTreeNode;
  LStructureChanged: Boolean;
  LEditable: Boolean;
  LApplyValues: Boolean;
  LColumnSpec: TNyxCollectionColumn;
  LText: TNyxText;
  LEditingItem: TNyxItemRef;
  LEditingColumn: Integer;
  LEditingText: String;
  LEditingStart: Integer;
  LEditingLength: Integer;
  LRestoreEditor: Boolean;
  LEditor: TCustomEdit;
begin
  LData := FView.Snapshot;
  LRestoreEditor := False;
  LEditingItem := Default(TNyxItemRef);
  LEditingColumn := -1;

  if (FGrid <> nil) and (FRendered <> nil) and (FRendered <> LData) and
    FGrid.EditorMode and (FGrid.Editor is TCustomEdit) and
    (FGrid.Row > 0) and (FGrid.Row <= FRendered.Count) and
    (FGrid.Col >= 0) and (FGrid.Col < FView.Spec.Count) then
  begin
    { End the positional editor while Refresh has FUpdating set, so it cannot
      admit its draft against a different row after sorting/filtering. Retain
      the draft/caret only when the same item/column accepted scalar survives. }
    LEditingItem := FRendered.ItemAt(FGrid.Row - 1).Ref;
    LEditingColumn := FGrid.Col;
    LEditor := TCustomEdit(FGrid.Editor);
    LEditingText := LEditor.Text;
    LEditingStart := LEditor.SelStart;
    LEditingLength := LEditor.SelLength;
    LColumnSpec := FView.Spec.ColumnAt(LEditingColumn);
    LRestoreEditor := LData.Has(LEditingItem) and
      LColumnSpec.Read(FRendered.Item(LEditingItem)).SameValue(
        LColumnSpec.Read(LData.Item(LEditingItem))) and
      not (FNormalizeValues and (FNormalizeItem.ID = LEditingItem.ID) and
        (FNormalizeColumn = LEditingColumn));
    FGrid.EditorMode := False;
  end;

  if FGrid <> nil then
  begin
    LEditable := False;
    for LColumn := 0 to FView.Spec.Count - 1 do
    begin

      if FEnabled and not FReadOnly and (FView.Spec.ColumnAt(LColumn).Mode = cmEditable) then
      begin
        LEditable := True;
      end;
    end;

    if LEditable <> (goEditing in FGrid.Options) then
    begin

      if LEditable then
      begin
        FGrid.Options := FGrid.Options + [goEditing];
      end
      else
      begin
        FGrid.Options := FGrid.Options - [goEditing];
      end;
    end;
  end;

  if FTree <> nil then
  begin
    FTree.ReadOnly := not FEnabled or FReadOnly or (FView.Spec.ColumnAt(0).Mode <> cmEditable);
  end;
  LSelected := -1;

  if FView.Selection.Focus.Defined then
  begin
    LSelected := LData.IndexOf(FView.Selection.Focus);
  end;

  if FList <> nil then
  begin
    LTop := FList.TopIndex;

    if (FRendered <> nil) and (LTop >= 0) and (LTop < FRendered.Count) then
    begin
      LTop := LData.IndexOf(FRendered.ItemAt(LTop).Ref);
    end;
    FList.Items.BeginUpdate;
    try
      FList.Items.Clear;
      for LIndex := 0 to LData.Count - 1 do
      begin
        FList.Items.Add(DisplayText(FView.CellText(LData.ItemAt(LIndex).Ref, 0)));
      end;
      FList.ItemIndex := LSelected;

      if FList.MultiSelect then
      begin
        for LIndex := 0 to LData.Count - 1 do
        begin
          FList.Selected[LIndex] := FView.Selection.Contains(LData.ItemAt(LIndex).Ref);
        end;
      end;

      if LTop >= 0 then
      begin
        FList.TopIndex := LTop;
      end;
    finally
      FList.Items.EndUpdate;
    end;
  end
  else if FGrid <> nil then
  begin
    FGrid.BeginUpdate;
    try
      { Preserve drafts when their accepted scalar is unchanged. Native grid
        cells are positional, so moved/new rows must receive their new content;
        unchanged rows survive unrelated field publications and normalization. }

      if FNormalizeValues or (FRendered <> LData) then
      begin

        if FGrid.ColCount <> FView.Spec.Count then
        begin
          FGrid.ColCount := FView.Spec.Count;
        end;
        FGrid.FixedCols := 0;

        if FGrid.RowCount <> Max(2, LData.Count + 1) then
        begin
          FGrid.RowCount := Max(2, LData.Count + 1);
        end;
        FGrid.FixedRows := 1;
        for LColumn := 0 to FView.Spec.Count - 1 do
        begin
          FGrid.Cells[LColumn, 0] := FView.Spec.ColumnAt(LColumn).Title;

          if LData.Count = 0 then
          begin
            FGrid.Cells[LColumn, 1] := '';
          end;
        end;
        for LIndex := 0 to LData.Count - 1 do
        begin
          LPrevious := -1;

          if FRendered <> nil then
          begin
            LPrevious := FRendered.IndexOf(LData.ItemAt(LIndex).Ref);
          end;
          for LColumn := 0 to FView.Spec.Count - 1 do
          begin
            LApplyValues := LPrevious <> LIndex;

            if (LColumn = LEditingColumn) and
              (LData.ItemAt(LIndex).Ref.ID = LEditingItem.ID) then
            begin
              { Closing the old editor may write its text into the physical
                cell. Normalize it from accepted data before restoring a draft. }
              LApplyValues := True;
            end;

            if not LApplyValues and (FRendered.Revision <> LData.Revision) then
            begin
              LColumnSpec := FView.Spec.ColumnAt(LColumn);
              LApplyValues := not LColumnSpec.Read(LData.ItemAt(LIndex)).SameValue(
                LColumnSpec.Read(FRendered.ItemAt(LPrevious)));
            end;

            if FNormalizeValues and
              (FNormalizeItem.ID = LData.ItemAt(LIndex).Ref.ID) and
              (FNormalizeColumn = LColumn) then
            begin
              LApplyValues := True;
            end;

            if LApplyValues then
            begin
              FGrid.Cells[LColumn, LIndex + 1] := DisplayText(
                FView.CellText(LData.ItemAt(LIndex).Ref, LColumn));
            end;
          end;
        end;
        { The widget's fixed default width clips ordinary task captions. Fit
          admitted headers/content once through LCL's own font measurement;
          later publications retain user column widths and active editor drafts.
          This remains adapter presentation, never an authored schema mutation. }

        if FRendered = nil then
        begin
          FGrid.AutoSizeColumns;
        end;
      end;

      if (LSelected >= 0) and (FGrid.Row <> LSelected + 1) then
      begin
        FGrid.Row := LSelected + 1;
      end;

      if LRestoreEditor then
      begin
        FGrid.Col := LEditingColumn;
        FGrid.Row := LData.IndexOf(LEditingItem) + 1;
        FGrid.EditorMode := True;

        if FGrid.Editor is TCustomEdit then
        begin
          LEditor := TCustomEdit(FGrid.Editor);
          LEditor.Text := LEditingText;
          LEditor.SelStart := LEditingStart;
          LEditor.SelLength := LEditingLength;
        end;
      end;
    finally
      FGrid.EndUpdate;
    end;
    FGrid.Invalidate;
  end
  else
  begin
    FTree.Items.BeginUpdate;
    try

      if FRendered = nil then
      begin
        FTree.Items.Clear;
      end;
      SetLength(LNext, LData.Count);
      LStructureChanged := (FRendered = nil);

      if FRendered <> nil then
      begin
        LStructureChanged := FRendered.Count <> LData.Count;
      end;
      for LIndex := 0 to LData.Count - 1 do
      begin
        LPrevious := -1;

        if FRendered <> nil then
        begin
          LPrevious := FRendered.IndexOf(LData.ItemAt(LIndex).Ref);
        end;

        if LPrevious >= 0 then
        begin
          LNext[LIndex] := FNodes[LPrevious];

          if LPrevious <> LIndex then
          begin
            LStructureChanged := True;
          end;

          if (FView.Spec.ParentField <> '') and
            (LData.ItemAt(LIndex).GetValue(NyxTextField(FView.Spec.ParentField)) <>
            FRendered.ItemAt(LPrevious).GetValue(NyxTextField(FView.Spec.ParentField))) then
          begin
            LStructureChanged := True;
          end;
        end
        else
        begin
          LNext[LIndex] := FTree.Items.Add(nil, '');
          LStructureChanged := True;
        end;
        LNext[LIndex].Data := Pointer(PtrUInt(LIndex + 1));
        LText := DisplayText(FView.CellText(LData.ItemAt(LIndex).Ref, 0));

        if TNyxText(LNext[LIndex].Text) <> LText then
        begin
          LNext[LIndex].Text := LText;
        end;
      end;

      if LStructureChanged and (FRendered <> nil) then
      begin
        { Scalar/selection changes preserve structure and expansion completely.
          Flatten only for an admitted reorder/reparent/removal, before deleting
          old ancestors or applying new links that could temporarily form cycles. }
        for LIndex := 0 to Length(LNext) - 1 do
        begin
          LNext[LIndex].MoveTo(nil, naAdd);
        end;
      end;
      for LIndex := 0 to Length(FNodes) - 1 do
      begin

        if not LData.Has(FRendered.ItemAt(LIndex).Ref) then
        begin
          FNodes[LIndex].Delete;
        end;
      end;
      FNodes := LNext;

      if LStructureChanged then
      begin
        for LIndex := 0 to Length(FNodes) - 1 do
        begin
          LParent := FView.ParentIndex(LIndex);

          if LParent >= 0 then
          begin
            FNodes[LIndex].MoveTo(FNodes[LParent], naAddChild);
          end;
        end;
      end;

      if LSelected >= 0 then
      begin
        FTree.Selected := FNodes[LSelected];
      end
      else
      begin
        FTree.Selected := nil;
      end;

      if FTree.MultiSelect then
      begin
        for LIndex := 0 to Length(FNodes) - 1 do
        begin
          FNodes[LIndex].MultiSelected := FView.Selection.Contains(LData.ItemAt(LIndex).Ref);
        end;
      end;
    finally
      FTree.Items.EndUpdate;
    end;
  end;
  FRendered := LData;
end;

procedure TNativeMount.DetachTarget;
begin

  if FList <> nil then
  begin
    FList.OnSelectionChange := FPreviousSelection;
    FList.OnKeyDown := FPreviousKey;
    FList.OnUTF8KeyPress := FPreviousUTF8;
    FList.MultiSelect := FPreviousMultiSelect;
    FList.ExtendedSelect := FPreviousExtendedSelect;
    FList := nil;
  end;

  if FGrid <> nil then
  begin
    FGrid.OnSelectCell := FPreviousCell;
    FGrid.OnValidateEntry := FPreviousValidate;
    FGrid.OnKeyDown := FPreviousKey;
    FGrid.OnMouseDown := FPreviousMouse;
    FGrid.OnPrepareCanvas := FPreviousPrepare;
    FGrid.Options := FPreviousOptions;
    FGrid := nil;
  end;

  if FTree <> nil then
  begin
    FTree.OnChange := FPreviousTreeChange;
    FTree.OnEdited := FPreviousTreeEdit;
    FTree.ReadOnly := FPreviousReadOnly;
    FTree.OnKeyDown := FPreviousKey;
    FTree.OnUTF8KeyPress := FPreviousUTF8;
    FTree.OnSelectionChanged := FPreviousTreeSelection;
    FTree.MultiSelect := FPreviousMultiSelect;
    FTree.MultiSelectStyle := FPreviousMultiStyle;
    FTree := nil;
  end;
  FNodes := nil;
  FRendered := nil;
end;

function MountNyxLCLCollection(AControl: TControl;
  const AView: INyxCollectionView): INyxCollectionMount;
var
  LMount: TNativeMount;
begin
  LMount := TNativeMount.Create(AControl, AView);
  Result := LMount;
  LMount.Activate;
end;

end.
