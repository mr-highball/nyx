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
unit nyx.collections.edits;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.editing, nyx.state, nyx.collections, nyx.collections.arrangement;

type
  { An owned physical cell draft, independent of widgets and authored defaults.
    Item and typed field identify its exact accepted scalar. Text is the host's
    wire draft (including an escaped NUL representation), not a promised scalar
    conversion. Selection uses Nyx's Unicode scalar offsets on both hosts.
    A nonediting dirty cell can survive browser row-window eviction; only one
    cell per snapshot may own editing/focus. An untouched record is absent. }
  TNyxCollectionCellEdit = record
  private
    FDefined: Boolean;
    FEditing: Boolean;
    FFocused: Boolean;
    FItem: TNyxItemRef;
    FField: TNyxColumnIdentity;
    FAccepted: TNyxStateValue;
    FText: TNyxText;
    FSelection: TNyxTextSelection;
    function GetAccepted: TNyxStateValue;
  public
    function Copy: TNyxCollectionCellEdit;
    procedure Validate;
    property Defined: Boolean read FDefined;
    property Item: TNyxItemRef read FItem;
    property Field: TNyxColumnIdentity read FField;
    property Accepted: TNyxStateValue read GetAccepted;
    property Text: TNyxText read FText;
    property Selection: TNyxTextSelection read FSelection;
    property Editing: Boolean read FEditing;
    property Focused: Boolean read FFocused;
  end;
  { Owned value vector; Copy before changing a shared candidate's entries. }
  TNyxCollectionCellEdits = array of TNyxCollectionCellEdit;

{ Copies host wire text/caret without parsing an unfinished scalar. }
function NyxCollectionCellEdit(const AItem: TNyxItemRef;
  const AField: TNyxColumnIdentity; const AAccepted: TNyxStateValue;
  const AText: TNyxText; const ASelection: TNyxTextSelection;
  AEditing, AFocused: Boolean): TNyxCollectionCellEdit;

{ Canonical scalar text at the edit boundary. This preserves literal user text;
  a native widget's escaped NUL display is an adapter concern. }
function NyxCellEditWireText(const AValue: TNyxStateValue): TNyxText;

implementation

uses SysUtils;

function NyxCellEditWireText(const AValue: TNyxStateValue): TNyxText;
begin
  AValue.Validate;
  Result := '';
  case AValue.Kind of
    nskText:
      Result := AValue.TextValue;
    nskBoolean:
      begin

        if AValue.BooleanValue then
        begin
          Result := 'true';
        end
        else
        begin
          Result := 'false';
        end;
      end;
    nskInteger:
      Result := TNyxText(IntToStr(AValue.IntegerValue));
    nskNumber:
      Result := AValue.NumberText;
  end;
end;

function TNyxCollectionCellEdit.GetAccepted: TNyxStateValue;
begin
  Result := FAccepted.Copy;
end;

function TNyxCollectionCellEdit.Copy: TNyxCollectionCellEdit;
begin
  Result := Self;
  Result.FAccepted := FAccepted.Copy;
end;

procedure TNyxCollectionCellEdit.Validate;
var
  LSelection: TNyxTextSelection;
begin

  if not FDefined or not FItem.Defined then
  begin
    raise ENyxCollection.Create('Cell edit requires a defined item and draft');
  end;
  FField.Validate;
  FAccepted.Validate;

  if (FAccepted.Kind <> FField.Kind) or (FFocused and not FEditing) then
  begin
    raise ENyxCollection.Create('Cell edit field, value and focus ownership must agree');
  end;
  NyxTextScalarCount(FText);

  if FSelection.Defined then
  begin
    LSelection := NyxTextSelection(FText, FSelection.Start, FSelection.Finish,
      FSelection.Direction);

    if not LSelection.SameRange(FSelection) then
    begin
      raise ENyxCollection.Create('Cell edit selection does not describe its exact draft');
    end;
  end;
end;

function NyxCollectionCellEdit(const AItem: TNyxItemRef;
  const AField: TNyxColumnIdentity; const AAccepted: TNyxStateValue;
  const AText: TNyxText; const ASelection: TNyxTextSelection;
  AEditing, AFocused: Boolean): TNyxCollectionCellEdit;
begin
  Result := Default(TNyxCollectionCellEdit);
  Result.FDefined := True;
  Result.FItem := AItem;
  Result.FField := AField;
  Result.FAccepted := AAccepted.Copy;
  Result.FText := AText;
  Result.FSelection := ASelection;
  Result.FEditing := AEditing;
  Result.FFocused := AFocused;
  Result.Validate;
end;

end.
