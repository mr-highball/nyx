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
unit nyx.collections.cell;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.state, nyx.collections, nyx.collections.arrangement;

type
  { One accepted cell, independent of selection, display formatting and drafts.
    Item includes its logical collection; Field includes its scalar family.
    Revision describes the original source, never a visible row/column index.
    Owned immutable scalars remain readable after source/view retirement. This
    value grants no write, clipboard or future-command authority: callers must
    admit their own operation against its original identities and revision.
    The managed marker makes absent older event records safe on both compilers;
    no COM interface, store, host, callback or target object lives in the record. }
  TNyxCollectionCellSnapshot = record
  private
    FMarker: TNyxText;
    FItem: TNyxItemRef;
    FField: TNyxColumnIdentity;
    FValue: TNyxStateValue;
    FRevision: Integer;
    function GetDefined: Boolean;
    procedure RequireDefined;
    function GetItem: TNyxItemRef;
    function GetField: TNyxColumnIdentity;
    function GetValue: TNyxStateValue;
    function GetRevision: Integer;
  public
    function Copy: TNyxCollectionCellSnapshot;
    property Defined: Boolean read GetDefined;
    property Item: TNyxItemRef read GetItem;
    property Field: TNyxColumnIdentity read GetField;
    property Value: TNyxStateValue read GetValue;
    property Revision: Integer read GetRevision;
  end;

  { Optional prepared-input facet for opaque target envelopes. Reading returns
    only already captured accepted data; it never resolves another live cell.
    Unsupported/unprepared/retired envelopes return an absent snapshot. Copying
    the result owns no envelope or execution authority. Existing interfaces and
    alternative target implementations retain their established GUID/vtables. }
  INyxCollectionCellContextSnapshot = interface(IInterface)
    ['{7A6059E8-90ED-4790-8905-CE9111E8C7BC}']
    function GetCellSnapshot: TNyxCollectionCellSnapshot;
    property CellSnapshot: TNyxCollectionCellSnapshot read GetCellSnapshot;
  end;

{ Explicit accepted-data boundary. Undefined identities, negative revisions and
  different field/value families refuse. Input scalars are copied, not coerced. }
function NyxCollectionCellSnapshot(const AItem: TNyxItemRef;
  const AField: TNyxColumnIdentity; const AValue: TNyxStateValue;
  ARevision: Integer): TNyxCollectionCellSnapshot;

implementation

function TNyxCollectionCellSnapshot.GetDefined: Boolean;
begin
  Result := FMarker = 'nyx-accepted-cell';
end;

procedure TNyxCollectionCellSnapshot.RequireDefined;
begin

  if not Defined then
  begin
    raise ENyxCollection.Create('Collection cell snapshot is absent');
  end;
end;

function TNyxCollectionCellSnapshot.GetItem: TNyxItemRef;
begin
  RequireDefined;
  Result := NyxItem(NyxCollection(FItem.Collection.Name), FItem.ID);
end;

function TNyxCollectionCellSnapshot.GetField: TNyxColumnIdentity;
begin
  RequireDefined;
  case FField.Kind of
    nskText:
      begin
        Result := NyxColumnIdentity(NyxTextField(FField.Name));
      end;
    nskBoolean:
      begin
        Result := NyxColumnIdentity(NyxBooleanField(FField.Name));
      end;
    nskInteger:
      begin
        Result := NyxColumnIdentity(NyxIntegerField(FField.Name));
      end;
    nskNumber:
      begin
        Result := NyxColumnIdentity(NyxNumberField(FField.Name));
      end;
  end;
end;

function TNyxCollectionCellSnapshot.GetValue: TNyxStateValue;
begin
  RequireDefined;
  Result := FValue.Copy;
end;

function TNyxCollectionCellSnapshot.GetRevision: Integer;
begin
  RequireDefined;
  Result := FRevision;
end;

function TNyxCollectionCellSnapshot.Copy: TNyxCollectionCellSnapshot;
begin
  Result := Default(TNyxCollectionCellSnapshot);

  if not Defined then
  begin
    Exit;
  end;
  { Copy admitted scalars explicitly for pas2js, without rescanning the original
    text or consulting a source/schema/controller during retained event delivery. }
  Result.FMarker := FMarker;
  Result.FItem := GetItem;
  Result.FField := GetField;
  Result.FValue := FValue.Copy;
  Result.FRevision := FRevision;
end;

function NyxCollectionCellSnapshot(const AItem: TNyxItemRef;
  const AField: TNyxColumnIdentity; const AValue: TNyxStateValue;
  ARevision: Integer): TNyxCollectionCellSnapshot;
begin
  Result := Default(TNyxCollectionCellSnapshot);

  if not AItem.Defined then
  begin
    raise ENyxCollection.Create('Collection cell snapshot requires an item');
  end;

  if not AField.Defined then
  begin
    raise ENyxCollection.Create('Collection cell snapshot requires a field');
  end;
  AField.Validate;
  AValue.Validate;

  if (ARevision < 0) or (AField.Kind <> AValue.Kind) then
  begin
    raise ENyxCollection.Create('Collection cell snapshot requires matching type and source revision');
  end;
  Result.FMarker := 'nyx-accepted-cell';
  Result.FItem := NyxItem(NyxCollection(AItem.Collection.Name), AItem.ID);
  Result.FField := AField;
  Result.FValue := AValue.Copy;
  Result.FRevision := ARevision;
end;

end.
