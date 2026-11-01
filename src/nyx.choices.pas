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
unit nyx.choices;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data;

const
  { Node wire field is typed only in design version seventeen or later. Earlier
    opaque fields retain extension meaning until explicitly resolved by callers. }
  NyxChoiceItemsWireField = 'choiceItems';
  NyxMaximumChoiceItems = 1024;
  { Logical UTF-8 key/caption bytes, excluding bounded descriptor punctuation.
    No target-specific string-length arithmetic changes the shared allowance. }
  NyxMaximumChoiceTextBytes = 256 * 1024;

type
  { Choice admission fails before publication. It never chooses a fallback row,
    invokes a creator or changes any application state. }
  ENyxChoice = class(Exception);

  { Exact text identity, independent from a caption or physical index. An explicit
    empty key is useful for a None option; the uninitialized record is invalid.
    References own copied portable text and never retain controls or providers. }
  TNyxChoiceRef = record
  private
    FValue: TNyxText;
    FInitialized: Boolean;
    function GetValue: TNyxText;
  public
    property Value: TNyxText read GetValue;
  end;

  { Immutable admitted row. Captions are single-line Unicode display text and
    may repeat; only the key identifies a value. A copied row remains valid
    after its collection/provider retires. Default rows have no valid key. }
  TNyxChoiceItem = record
  private
    FKey: TNyxChoiceRef;
    FCaption: TNyxText;
  public
    property Key: TNyxChoiceRef read FKey;
    property Caption: TNyxText read FCaption;
  end;

  { Managed immutable ordered choices. Fluent additions return independent
    branches; getters expose value snapshots, never mutable arrays or receivers.
    Duplicate captions are valid, duplicate exact keys refuse. -1 means absent
    in IndexOf; an empty key still resolves normally. No model, event, widget,
    state store, locale service or executable creator is retained.
    ToData is the explicit version-one persistence/extension boundary. }
  INyxChoiceItems = interface(IInterface)
    ['{430EACD7-286D-4B4F-9CB5-006DE6A81101}']
    function GetCount: Integer;
    function GetTextBytes: Integer;
    function Item(AIndex: Integer): TNyxChoiceItem;
    function IndexOf(const AKey: TNyxChoiceRef): Integer;
    function WithChoice(const AKey: TNyxChoiceRef;
      const ACaption: TNyxText): INyxChoiceItems;
    function ToData: TNyxDataValue;
    property Count: Integer read GetCount;
    property TextBytes: Integer read GetTextBytes;
  end;

function NyxChoice(const AValue: TNyxText): TNyxChoiceRef;
function NyxChoiceItem(const AKey: TNyxChoiceRef;
  const ACaption: TNyxText): TNyxChoiceItem;
function NewNyxChoices: INyxChoiceItems; overload;
function NewNyxChoices(const AItems: array of TNyxChoiceItem): INyxChoiceItems; overload;
{ Strict descriptor admission owns and validates every value before publishing.
  Unknown fields/versions, wrong families, duplicate keys and excess budgets
  refuse. An alternative implementation can be normalized through its ToData;
  no reported byte count, backing array or provider lifetime is trusted. }
function NyxChoicesFromData(const AData: TNyxDataValue): INyxChoiceItems;
{ Exact logical comparison, including order and captions. Nil differs from an
  explicit empty set. Holds both immutable participants across value getters;
  this is observation, not admission of a foreign provider into a model. Built-in
  renderers pass only their normalized model snapshots and never rebuild equal
  option sets merely because composition produced another managed instance. }
function NyxSameChoiceItems(const ALeft, ARight: INyxChoiceItems): Boolean;

implementation

uses nyx.bytes;

type
  { Private owned vector. All construction copies rows explicitly, including on
    pas2js where dynamic-array assignment does not provide Pascal copy-on-write. }
  TChoiceVector = array of TNyxChoiceItem;

  { Data-only immutable implementation. New instances are leased before fallible
    validation; failure frees all candidate storage and leaves older branches exact. }
  TChoices = class(TInterfacedObject, INyxChoiceItems)
  private
    FItems: TChoiceVector;
    FTextBytes: Integer;
  public
    function GetCount: Integer;
    function GetTextBytes: Integer;
    function Item(AIndex: Integer): TNyxChoiceItem;
    function IndexOf(const AKey: TNyxChoiceRef): Integer;
    function WithChoice(const AKey: TNyxChoiceRef;
      const ACaption: TNyxText): INyxChoiceItems;
    function ToData: TNyxDataValue;
  end;

function NyxSameChoiceItems(const ALeft, ARight: INyxChoiceItems): Boolean;
var
  LLeft: INyxChoiceItems;
  LRight: INyxChoiceItems;
  LLeftItem: TNyxChoiceItem;
  LRightItem: TNyxChoiceItem;
  LCount: Integer;
  LIndex: Integer;
begin
  LLeft := ALeft;
  LRight := ARight;

  if LLeft = LRight then
  begin
    Exit(True);
  end;

  if (LLeft = nil) or (LRight = nil) then
  begin
    Exit(False);
  end;
  LCount := LLeft.Count;

  if LCount <> LRight.Count then
  begin
    Exit(False);
  end;
  for LIndex := 0 to LCount - 1 do
  begin
    LLeftItem := LLeft.Item(LIndex);
    LRightItem := LRight.Item(LIndex);

    if (LLeftItem.Key.Value <> LRightItem.Key.Value) or
      (LLeftItem.Caption <> LRightItem.Caption) then
    begin
      Exit(False);
    end;
  end;
  Result := True;
end;

function TextBytes(const AText: TNyxText): Integer;
begin
  try
    Result := NyxUTF8ByteCount(AText);
  except
    on ENyxBytes do
    begin
      raise ENyxChoice.Create('Choices require exact valid Unicode text');
    end;
  end;

  if Result > NyxMaximumChoiceTextBytes then
  begin
    raise ENyxChoice.Create('Choice text exceeds its portable byte budget');
  end;
end;

function TNyxChoiceRef.GetValue: TNyxText;
begin

  if not FInitialized then
  begin
    raise ENyxChoice.Create('Construct an explicit choice key first');
  end;
  Result := FValue;
end;

function NyxChoice(const AValue: TNyxText): TNyxChoiceRef;
begin
  TextBytes(AValue);
  Result.FValue := AValue;
  Result.FInitialized := True;
end;

function NyxChoiceItem(const AKey: TNyxChoiceRef;
  const ACaption: TNyxText): TNyxChoiceItem;
var
  LIndex: Integer;
  LScalar: Integer;
  LValue: TNyxText;
begin
  LValue := AKey.Value;

  if TextBytes(LValue) > NyxMaximumChoiceTextBytes - TextBytes(ACaption) then
  begin
    raise ENyxChoice.Create('Choice row exceeds its portable byte budget');
  end;
  LIndex := 1;
  while LIndex <= Length(ACaption) do
  begin

    if not NyxNextScalar(ACaption, LIndex, LScalar) or (LScalar < 32) or
      ((LScalar >= $7f) and (LScalar <= $9f)) or
      (LScalar = $2028) or (LScalar = $2029) then
    begin
      raise ENyxChoice.Create('Choice captions require single-line Unicode display text');
    end;
  end;
  Result.FKey := AKey;
  Result.FCaption := ACaption;
end;

function NewNyxChoices: INyxChoiceItems;
begin
  Result := NewNyxChoices([]);
end;

function NewNyxChoices(const AItems: array of TNyxChoiceItem): INyxChoiceItems;
var
  LChoices: TChoices;
  LIndex: Integer;
  LPrior: Integer;
  LBytes: Integer;
  LItem: TNyxChoiceItem;
begin

  if Length(AItems) > NyxMaximumChoiceItems then
  begin
    raise ENyxChoice.Create('Choice collection exceeds its item allowance');
  end;
  LChoices := TChoices.Create;
  Result := LChoices;
  SetLength(LChoices.FItems, Length(AItems));
  for LIndex := 0 to High(AItems) do
  begin
    LItem := NyxChoiceItem(AItems[LIndex].Key, AItems[LIndex].Caption);
    for LPrior := 0 to LIndex - 1 do
    begin

      if LChoices.FItems[LPrior].Key.Value = LItem.Key.Value then
      begin
        raise ENyxChoice.Create('Choice keys must be unique');
      end;
    end;
    LBytes := TextBytes(LItem.Key.Value) + TextBytes(LItem.Caption);

    if LBytes > NyxMaximumChoiceTextBytes - LChoices.FTextBytes then
    begin
      raise ENyxChoice.Create('Choice collection exceeds its portable text budget');
    end;
    LChoices.FTextBytes := LChoices.FTextBytes + LBytes;
    LChoices.FItems[LIndex] := LItem;
  end;
end;

function TChoices.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TChoices.GetTextBytes: Integer;
begin
  Result := FTextBytes;
end;

function TChoices.Item(AIndex: Integer): TNyxChoiceItem;
begin

  if (AIndex < 0) or (AIndex >= Length(FItems)) then
  begin
    raise ENyxChoice.Create('Choice item index is unavailable');
  end;
  Result := FItems[AIndex];
end;

function TChoices.IndexOf(const AKey: TNyxChoiceRef): Integer;
var
  LValue: TNyxText;
begin
  LValue := AKey.Value;
  for Result := 0 to High(FItems) do
  begin

    if FItems[Result].Key.Value = LValue then
    begin
      Exit;
    end;
  end;
  Result := -1;
end;

function TChoices.WithChoice(const AKey: TNyxChoiceRef;
  const ACaption: TNyxText): INyxChoiceItems;
var
  LNext: TChoices;
  LItem: TNyxChoiceItem;
  LBytes: Integer;
  LIndex: Integer;
begin

  if Length(FItems) >= NyxMaximumChoiceItems then
  begin
    raise ENyxChoice.Create('Choice collection exceeds its item allowance');
  end;
  LItem := NyxChoiceItem(AKey, ACaption);

  if IndexOf(AKey) >= 0 then
  begin
    raise ENyxChoice.Create('Choice keys must be unique');
  end;
  LBytes := TextBytes(AKey.Value) + TextBytes(ACaption);

  if LBytes > NyxMaximumChoiceTextBytes - FTextBytes then
  begin
    raise ENyxChoice.Create('Choice collection exceeds its portable text budget');
  end;
  { The sealed prefix is already admitted. Copy its value rows without another
    quadratic duplicate scan or repeated UTF-8 traversal of every older caption. }
  LNext := TChoices.Create;
  Result := LNext;
  SetLength(LNext.FItems, Length(FItems) + 1);
  for LIndex := 0 to High(FItems) do
  begin
    LNext.FItems[LIndex] := FItems[LIndex];
  end;
  LNext.FItems[High(LNext.FItems)] := LItem;
  LNext.FTextBytes := FTextBytes + LBytes;
end;

function TChoices.ToData: TNyxDataValue;
var
  LItems: array of TNyxDataValue;
  LIndex: Integer;
begin
  SetLength(LItems, Length(FItems));
  for LIndex := 0 to High(FItems) do
  begin
    LItems[LIndex] := NyxObject([
      NyxField('value', NyxData(FItems[LIndex].Key.Value)),
      NyxField('caption', NyxData(FItems[LIndex].Caption))]);
  end;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('items', NyxArray(LItems))]);
end;

{ These wire objects have exactly two closed fields. Checking names before
  reading values also rejects misspellings and preserves extension boundaries. }
procedure RequireFields(const AData: TNyxDataValue; const AFirst, ASecond: TNyxText);
begin

  if (AData.Kind <> ndObject) or (AData.Count <> 2) then
  begin
    raise ENyxChoice.Create('Choice descriptor requires its exact declared fields');
  end;

  if not (((AData.Key(0) = AFirst) and (AData.Key(1) = ASecond)) or
    ((AData.Key(0) = ASecond) and (AData.Key(1) = AFirst))) then
  begin
    raise ENyxChoice.Create('Choice descriptor contains unknown or missing fields');
  end;
end;

function NyxChoicesFromData(const AData: TNyxDataValue): INyxChoiceItems;
var
  LRows: TNyxDataValue;
  LRow: TNyxDataValue;
  LItems: TChoiceVector;
  LIndex: Integer;
begin

  RequireFields(AData, 'version', 'items');

  if (AData.Field('version').Kind <> ndNumber) or
    (AData.Field('version').AsDecimal.Text <> '1') then
  begin
    raise ENyxChoice.Create('Choice descriptor version is unavailable');
  end;
  LRows := AData.Field('items');

  if (LRows.Kind <> ndArray) or (LRows.Count > NyxMaximumChoiceItems) then
  begin
    raise ENyxChoice.Create('Choice descriptor requires a bounded item array');
  end;
  SetLength(LItems, LRows.Count);
  for LIndex := 0 to LRows.Count - 1 do
  begin
    LRow := LRows.Item(LIndex);

    RequireFields(LRow, 'value', 'caption');

    if (LRow.Field('value').Kind <> ndText) or (LRow.Field('caption').Kind <> ndText) then
    begin
      raise ENyxChoice.Create('Choice items require exact text value/caption fields');
    end;
    LItems[LIndex] := NyxChoiceItem(NyxChoice(LRow.Field('value').AsText),
      LRow.Field('caption').AsText);
  end;
  Result := NewNyxChoices(LItems);
end;

end.
