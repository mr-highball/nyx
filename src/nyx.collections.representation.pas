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
unit nyx.collections.representation;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.state, nyx.contract, nyx.collections,
  nyx.collections.arrangement, nyx.collections.clipboard, nyx.formatting;

type
  { Invalid representation/cell provenance refuses before any output authority. }
  ENyxCollectionRepresentation = class(ENyxCollection);

  { RAW is always the default. Display text is an explicit different product;
    a draft is reserved for deliberate target capture, never guessed from a
    formatted string or silently substituted for accepted data. }
  TNyxCollectionCellRepresentation = (nccrAcceptedRaw, nccrFormattedDisplay,
    nccrExplicitDraft);

  { Detached exact single-cell output. All item/full-field/revision/scalar facts
    are copied; no original payload, view/store, formatter/locale provider,
    editor or callback is retained. Value remains the accepted scalar even for
    formatted display. Formatter returns the complete immutable policy and
    identity, and refuses when HasFormatter=False. Neither constructor reads an
    editor or claims that target controls already use its presentation policy. }
  INyxCollectionCellText = interface(IInterface)
    ['{94ECD898-681E-4684-8B1C-08E961F1610B}']
    function GetRepresentation: TNyxCollectionCellRepresentation;
    function GetText: TNyxText;
    function GetRawText: TNyxText;
    function GetByteCount: Integer;
    function GetItem: TNyxItemRef;
    function GetField: TNyxColumnIdentity;
    function GetRevision: Integer;
    function GetValue: TNyxStateValue;
    function GetHasFormatter: Boolean;
    function GetFormatter: TNyxValueFormat;
    function GetDomain: TNyxValueDomain;
    property Representation: TNyxCollectionCellRepresentation read GetRepresentation;
    property Text: TNyxText read GetText;
    property RawText: TNyxText read GetRawText;
    property ByteCount: Integer read GetByteCount;
    property Item: TNyxItemRef read GetItem;
    property Field: TNyxColumnIdentity read GetField;
    property Revision: Integer read GetRevision;
    property Value: TNyxStateValue read GetValue;
    property HasFormatter: Boolean read GetHasFormatter;
    property Formatter: TNyxValueFormat read GetFormatter;
    property Domain: TNyxValueDomain read GetDomain;
  end;

{ Actual accepted-RAW single-cell input is required. Selected-row TSV, even a
  one-cell quoted row, cannot be relabeled as an accepted raw cell. }
function NewNyxCollectionCellText(const ARaw: INyxCollectionClipboardText;
  AMaximumUTF8Bytes: Integer = 4194304): INyxCollectionCellText;
function NewNyxCollectionCellDisplay(const ARaw: INyxCollectionClipboardText;
  const AFormatter: TNyxValueFormat; const ADomain: TNyxValueDomain;
  AMaximumUTF8Bytes: Integer = 4194304): INyxCollectionCellText;

implementation

uses nyx.bytes;

type
  { Private builder is invisible until provenance/format/output admission all
    succeed. Returned records are copied on both native and pas2js targets. }
  TCellText = class(TInterfacedObject, INyxCollectionCellText)
  public
    Representation: TNyxCollectionCellRepresentation;
    Text: TNyxText;
    RawText: TNyxText;
    Bytes: Integer;
    Item: TNyxItemRef;
    Field: TNyxColumnIdentity;
    Revision: Integer;
    Value: TNyxStateValue;
    HasFormatter: Boolean;
    Formatter: TNyxValueFormat;
    Domain: TNyxValueDomain;
    procedure Capture(const ARaw: INyxCollectionClipboardText; AMaximum: Integer);
    function GetRepresentation: TNyxCollectionCellRepresentation;
    function GetText: TNyxText;
    function GetRawText: TNyxText;
    function GetByteCount: Integer;
    function GetItem: TNyxItemRef;
    function GetField: TNyxColumnIdentity;
    function GetRevision: Integer;
    function GetValue: TNyxStateValue;
    function GetHasFormatter: Boolean;
    function GetFormatter: TNyxValueFormat;
    function GetDomain: TNyxValueDomain;
  end;

procedure TCellText.Capture(const ARaw: INyxCollectionClipboardText; AMaximum: Integer);
var
  LRaw: INyxCollectionClipboardText;
  LKey: TNyxCollectionRef;
begin
  LRaw := ARaw;

  if (LRaw = nil) or (LRaw.RowCount <> 1) or (LRaw.FieldCount <> 1) or
    (AMaximum < 1) or (AMaximum > 4194304) then
  begin
    raise ENyxCollectionRepresentation.Create('Representation requires one bounded accepted raw cell');
  end;
  LKey := LRaw.Key;
  Item := LRaw.ItemAt(0);
  Field := LRaw.FieldAt(0);
  Field.Validate;
  Revision := LRaw.Revision;
  Value := LRaw.CellValue(0, 0).Copy;
  Value.Validate;
  RawText := LRaw.Text;

  if not Item.Defined or (Item.Collection.Name <> LKey.Name) or
    (Revision < 0) or (Value.Kind <> Field.Kind) or
    (RawText <> NyxRawScalarText(Value)) or
    (LRaw.ByteCount <> NyxUTF8ByteCount(RawText)) then
  begin
    raise ENyxCollectionRepresentation.Create('Accepted raw cell provenance/scalar does not match');
  end;
  Item := NyxItem(NyxCollection(LKey.Name), Item.ID);
  Field := TNyxColumnIdentity.FromData(Field.ToData);
  Text := RawText;
  Bytes := NyxUTF8ByteCount(Text);

  if Bytes > AMaximum then
  begin
    raise ENyxCollectionRepresentation.Create('Cell representation exceeds its UTF-8 byte budget');
  end;
  Domain := NyxNoDomain;
end;

function NewNyxCollectionCellText(const ARaw: INyxCollectionClipboardText;
  AMaximumUTF8Bytes: Integer): INyxCollectionCellText;
var
  LCell: TCellText;
begin
  LCell := TCellText.Create;
  Result := LCell;
  LCell.Capture(ARaw, AMaximumUTF8Bytes);
end;

function NewNyxCollectionCellDisplay(const ARaw: INyxCollectionClipboardText;
  const AFormatter: TNyxValueFormat; const ADomain: TNyxValueDomain;
  AMaximumUTF8Bytes: Integer): INyxCollectionCellText;
var
  LCell: TCellText;
  LFormatter: TNyxValueFormat;
  LDomain: TNyxValueDomain;
begin
  LFormatter := AFormatter.Copy;
  LDomain := ADomain.Copy;
  LCell := TCellText.Create;
  Result := LCell;
  LCell.Capture(ARaw, AMaximumUTF8Bytes);
  LCell.Text := LFormatter.FormatValue(LCell.Value, LDomain, AMaximumUTF8Bytes);
  LCell.Bytes := NyxUTF8ByteCount(LCell.Text);
  LCell.Representation := nccrFormattedDisplay;
  LCell.Formatter := LFormatter;
  LCell.HasFormatter := True;
  LCell.Domain := LDomain;
end;

function TCellText.GetRepresentation: TNyxCollectionCellRepresentation;
begin
  Result := Representation;
end;

function TCellText.GetText: TNyxText;
begin
  Result := Text;
end;

function TCellText.GetRawText: TNyxText;
begin
  Result := RawText;
end;

function TCellText.GetByteCount: Integer;
begin
  Result := Bytes;
end;

function TCellText.GetItem: TNyxItemRef;
begin
  Result := NyxItem(Item.Collection, Item.ID);
end;

function TCellText.GetField: TNyxColumnIdentity;
begin
  Result := TNyxColumnIdentity.FromData(Field.ToData);
end;

function TCellText.GetRevision: Integer;
begin
  Result := Revision;
end;

function TCellText.GetValue: TNyxStateValue;
begin
  Result := Value.Copy;
end;

function TCellText.GetHasFormatter: Boolean;
begin
  Result := HasFormatter;
end;

function TCellText.GetFormatter: TNyxValueFormat;
begin

  if not HasFormatter then
  begin
    raise ENyxCollectionRepresentation.Create('Accepted raw representation has no display formatter');
  end;
  Result := Formatter.Copy;
end;

function TCellText.GetDomain: TNyxValueDomain;
begin
  Result := Domain.Copy;
end;

end.
