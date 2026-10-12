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

unit nyx.test.linq.materialization;

{$mode delphi}{$H+}{$codepage utf8}

interface

function RunNyxMaterializationChecks: Integer;

implementation

uses
  SysUtils, nyx.text, nyx.linq, nyx.linq.grouping, nyx.linq.collections,
  nyx.linq.aggregates, nyx.linq.materialization
  {$ifndef PAS2JS}, Generics.Collections, nyx.linq.native{$endif};

type
  TEntry = record
    Key: Integer;
    Value: TNyxText;
  end;
  TIntEntry = INyxKeyValue<Integer, TNyxText>;
  TCollision = class(TInterfacedObject, INyxEquality<Integer>)
  public
    Hashes: Integer;
    Comparisons: Integer;
    Collide: Boolean;
    function Equal(const ALeft, ARight: Integer): Boolean;
    function Hash(const AValue: Integer): Cardinal;
  end;
  EMaterializeCallback = class(Exception);
  TLifetime = class
    Destroyed: Integer;
  end;
  IItem = interface
    ['{0733B967-C692-4AA2-B0B3-614894DE85B7}']
    function GetValue: Integer;
  end;
  TItem = class(TInterfacedObject, IItem)
  private
    FLifetime: TLifetime;
  public
    constructor Create(ALifetime: TLifetime);
    destructor Destroy; override;
    function GetValue: Integer;
  end;
  TNilSource = class(TNyxSequence<Integer>)
    function GetEnumerator: INyxEnumerator<Integer>; override;
  end;
  TItemEntry = INyxKeyValue<Integer, IItem>;

var
  GChecks: Integer;
  GKeys: Integer;
  GValues: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxQuery.Create('Materialization qualification: ' + AReason);
  end;
  Inc(GChecks);
end;

function EntryKey(const AValue: TEntry): Integer;
begin
  Inc(GKeys);
  Result := AValue.Key;
end;

function EntryValue(const AValue: TEntry): TNyxText;
begin
  Inc(GValues);
  Result := AValue.Value;
end;

function IntegerKey(const AValue: Integer): Integer;
begin
  Inc(GKeys);
  Result := AValue;
end;

function IntegerValue(const AValue: Integer): Integer;
begin
  Inc(GValues);
  Result := AValue * 2;
end;

function FailValue(const AValue: Integer): Integer;
begin
  Result := AValue;

  if AValue = 2 then
  begin
    raise EMaterializeCallback.Create('value failed / 🌙');
  end;
end;

function TextIdentity(const AValue: TNyxText): TNyxText;
begin
  Result := AValue;
end;

function ManagedKey(const AValue: IItem): Integer;
begin
  Result := AValue.GetValue;
end;

function ManagedValue(const AValue: IItem): IItem;
begin
  Result := AValue;
end;

function TCollision.Equal(const ALeft, ARight: Integer): Boolean;
begin
  Inc(Comparisons);
  Result := ALeft = ARight;
end;

function TCollision.Hash(const AValue: Integer): Cardinal;
begin
  Inc(Hashes);

  if Collide then
  begin
    Result := High(Cardinal);
  end
  else
  begin
    Result := NewNyxIntegerEquality.Hash(AValue);
  end;
end;

constructor TItem.Create(ALifetime: TLifetime);
begin
  inherited Create;
  FLifetime := ALifetime;
end;

destructor TItem.Destroy;
begin
  Inc(FLifetime.Destroyed);
  inherited Destroy;
end;

function TItem.GetValue: Integer;
begin
  Result := 7;
end;

function TNilSource.GetEnumerator: INyxEnumerator<Integer>;
begin
  Result := nil;
end;

procedure PortableListAndTexts;
var
  LRows: TNyxSequenceArray<Integer>;
  LSource: INyxSequence<Integer>;
  LList: INyxList<Integer>;
  LSequence: INyxSequence<Integer>;
  LValues: TNyxSequenceArray<Integer>;
  LRefused: Boolean;
  LText: TNyxStrings;
  LTextQuery: INyxSequence<TNyxText>;
  LTextValues: TNyxSequenceArray<TNyxText>;
begin
  SetLength(LRows, 3);
  LRows[0] := 3;
  LRows[1] := 1;
  LRows[2] := 3;
  LSource := TNyxSequence<Integer>.From(LRows);
  LList := TNyxMaterialize<Integer>.ToList(LSource);
  LRows[0] := 9;
  LSource := nil;
  Check((LList.Count = 3) and (LList.ItemAt(0) = 3) and (LList.ItemAt(2) = 3),
    'owned list retains original order/multiplicity after caller mutation');
  LValues := LList.ToArray;
  LValues[0] := 8;
  Check(LList.ItemAt(0) = 3, 'mutable exported vector is detached from list storage');
  LSequence := LList.AsSequence;
  LList := nil;
  Check((LSequence.Count = 3) and (LSequence.First = 3), 'independent query survives materialized list disposal');
  LList := TNyxMaterialize<Integer>.ToList(TNyxSequence<Integer>.Empty);
  Check(LList.Count = 0, 'empty materialized list has no phantom member');
  LRefused := False;
  try
    LList.ItemAt(0);
  except
    on ENyxQueryEmpty do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'absent list position refuses');
  LSource := TNilSource.Create;
  LRefused := False;
  try
    TNyxMaterialize<Integer>.ToList(LSource);
  except
    on ENyxQuery do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'invalid materialization cursor refuses');
  LSource := nil;
  LText := TNyxStrings.Create;
  try
    LText.Add(TNyxText('English / 🌙') + TNyxText(#0));
    LText.Add('Second');
    LTextQuery := LText.AsNyx;
    LText[0] := 'Changed';
  finally
    LText.Free;
  end;
  LTextValues := LTextQuery.ToArray;
  Check((LTextValues[0] = TNyxText('English / 🌙') + TNyxText(#0)) and
    (LTextValues[1] = 'Second'), 'portable owned text helper captures exact values without ANSI conversion');
end;

procedure DictionariesAndSets;
var
  LRows: TNyxSequenceArray<TEntry>;
  LSource: INyxSequence<TEntry>;
  LDictionary: INyxDictionary<Integer, TNyxText>;
  LEntries: INyxSequence<TIntEntry>;
  LEntry: TIntEntry;
  LKey: INyxSelector<TEntry, Integer>;
  LValue: INyxSelector<TEntry, TNyxText>;
  LRefused: Boolean;
  LMissing: TNyxText;
  LSet: INyxHashSet<Integer>;
  LSetQuery: INyxSequence<Integer>;
  LProbe: TCollision;
  LEquality: INyxEquality<Integer>;
  LTextDictionary: INyxDictionary<TNyxText, TNyxText>;
begin
  SetLength(LRows, 3);
  LRows[0].Key := 3;
  LRows[0].Value := 'First';
  LRows[1].Key := 1;
  LRows[1].Value := 'Second';
  LRows[2].Key := 3;
  LRows[2].Value := 'Replacement';
  LSource := TNyxSequence<TEntry>.From(LRows);
  LKey := TNyxSelector<TEntry, Integer>.From(EntryKey);
  LValue := TNyxSelector<TEntry, TNyxText>.From(EntryValue);
  LProbe := TCollision.Create;
  LProbe.Collide := True;
  LEquality := LProbe;
  GValues := 0;
  LRefused := False;
  try
    TNyxDictionary<TEntry, Integer, TNyxText>.From(LSource, LKey, LValue, LEquality);
  except
    on ENyxQueryDuplicateKey do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (GValues = 2), 'default dictionary policy refuses duplicates before duplicate value callback');
  GKeys := 0;
  GValues := 0;
  LDictionary := TNyxDictionary<TEntry, Integer, TNyxText>.From(
    LSource, LKey, LValue, LEquality, ndpKeepFirst);
  Check((LDictionary.Count = 2) and (LDictionary.GetValue(3) = 'First') and
    (GKeys = 3) and (GValues = 2), 'KeepFirst preserves first value and skips duplicate projection');
  LDictionary := TNyxDictionary<TEntry, Integer, TNyxText>.From(
    LSource, LKey, LValue, LEquality, ndpReplaceValue);
  Check((LDictionary.GetValue(3) = 'Replacement') and (LDictionary.EntryAt(0).Key = 3),
    'replacement selects final value while retaining first-key order');
  Check(LDictionary.GetValue(1) = 'Second', 'constant hash collisions resolve by typed equality');
  Check(not LDictionary.ContainsKey(9) and not LDictionary.TryGetValue(9, LMissing)
    and (LMissing = ''), 'absent dictionary key has explicit false/cleared output');
  LEntries := LDictionary.Entries;
  LDictionary := nil;
  LSource := nil;
  LRows := nil;
  Check(LEntries.Count = 2, 'detached entry sequence survives dictionary/source disposal');
  LEntry := LEntries.First;
  Check((LEntry.Key = 3) and (LEntry.Value = 'Replacement'), 'retained typed entry exposes original key/value');
  LSet := TNyxMaterialize<Integer>.ToHashSet(TNyxSequence<Integer>.From([3, 1, 3, 2]), LEquality);
  Check((LSet.Count = 3) and LSet.Contains(3) and not LSet.Contains(9), 'owned hash set removes duplicates and resolves collisions');
  LSetQuery := LSet.AsSequence;
  LSet := nil;
  Check(LSetQuery.SequenceEqual(TNyxSequence<Integer>.From([3, 1, 2]), NewNyxIntegerEquality),
    'hash-set query retains first-occurrence order after set disposal');
  LTextDictionary := TNyxDictionary<TNyxText, TNyxText, TNyxText>.From(
    TNyxSequence<TNyxText>.From([TNyxText('A🌙') + TNyxText(#0), TNyxText('a🌙') + TNyxText(#0)]),
    TNyxSelector<TNyxText, TNyxText>.From(TextIdentity),
    TNyxSelector<TNyxText, TNyxText>.From(TextIdentity),
    NewNyxTextEquality(ntoAsciiInsensitive), ndpReplaceValue);
  Check((LTextDictionary.Count = 1) and
    (LTextDictionary.Keys.First = TNyxText('A🌙') + TNyxText(#0)) and
    (LTextDictionary.GetValue(TNyxText('A🌙') + TNyxText(#0)) = TNyxText('a🌙') + TNyxText(#0)),
    'equivalent text replacement keeps first exact Unicode/NUL key and last value');
end;

procedure LargeAndFailure;
var
  LProbe: TCollision;
  LEquality: INyxEquality<Integer>;
  LDictionary: INyxDictionary<Integer, Integer>;
  LSource: INyxSequence<Integer>;
  LIndex: Integer;
  LRefused: Boolean;
begin
  LProbe := TCollision.Create;
  LEquality := LProbe;
  LSource := NyxRange(0, 4096);
  LDictionary := TNyxDictionary<Integer, Integer, Integer>.From(LSource,
    TNyxSelector<Integer, Integer>.From(IntegerKey),
    TNyxSelector<Integer, Integer>.From(IntegerValue), LEquality);
  Check((LDictionary.Count = 4096) and (LProbe.Hashes = 4096) and (LProbe.Comparisons = 0),
    'large dictionary admission caches growth hashes without rescanning prior keys');
  for LIndex := 0 to 4095 do
  begin
    Check((LDictionary.EntryAt(LIndex).Key = LIndex) and
      (LDictionary.GetValue(LIndex) = LIndex * 2), 'complete indexed dictionary membership/order');
  end;
  LDictionary := nil;
  LRefused := False;
  try
    LDictionary := TNyxDictionary<Integer, Integer, Integer>.From(NyxRange(0, 4),
      TNyxSelector<Integer, Integer>.From(IntegerKey),
      TNyxSelector<Integer, Integer>.From(FailValue), LEquality);
  except
    on EMaterializeCallback do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LDictionary = nil), 'value failure returns no partial dictionary and preserves original type');
end;

procedure ManagedValues;
var
  LLifetime: TLifetime;
  LValue: IItem;
  LRead: IItem;
  LRows: TNyxSequenceArray<IItem>;
  LSource: INyxSequence<IItem>;
  LList: INyxList<IItem>;
  LDictionary: INyxDictionary<Integer, IItem>;
  LEntries: INyxSequence<TItemEntry>;
  LEntry: TItemEntry;
begin
  LLifetime := TLifetime.Create;
  try
    LValue := TItem.Create(LLifetime);
    SetLength(LRows, 1);
    LRows[0] := LValue;
    LSource := TNyxSequence<IItem>.From(LRows);
    LRows := nil;
    LList := TNyxMaterialize<IItem>.ToList(LSource);
    LDictionary := TNyxDictionary<IItem, Integer, IItem>.From(LSource,
      TNyxSelector<IItem, Integer>.From(ManagedKey),
      TNyxSelector<IItem, IItem>.From(ManagedValue), NewNyxIntegerEquality);
    LEntries := LDictionary.Entries;
    LValue := nil;
    LSource := nil;
    LDictionary := nil;
    LList := nil;
    Check(LLifetime.Destroyed = 0, 'detached materialized entries retain managed payload after container retirement');
    LEntry := LEntries.First;
    LRead := LEntry.Value;
    LEntries := nil;
    LEntry := nil;
    Check((LRead.GetValue = 7) and (LLifetime.Destroyed = 0), 'independent entry reader survives all container/query disposal');
    LRead := nil;
    Check(LLifetime.Destroyed = 1, 'last materialized managed reader retires payload once');
  finally
    LRows := nil;
    LRead := nil;
    LEntry := nil;
    LEntries := nil;
    LSource := nil;
    LDictionary := nil;
    LList := nil;
    LValue := nil;
    LLifetime.Free;
  end;
end;

{$ifndef PAS2JS}
procedure NativeHelpers;
var
  LList: TNyxIntegerList;
  LTextList: TNyxTextList;
  LQueue: TNyxIntegerQueue;
  LStack: TNyxIntegerStack;
  LGeneric: TNyxQueryList<TEntry>;
  LDictionary: TDictionary<Integer, TNyxText>;
  LQuery: INyxSequence<Integer>;
  LTextQuery: INyxSequence<TNyxText>;
  LGenericQuery: INyxSequence<TEntry>;
  LEntry: TEntry;
  LEntries: INyxSequence<TIntEntry>;
  LCopy: TList<Integer>;
  LRefused: Boolean;
begin
  LList := TNyxIntegerList.Create;
  try
    LList.Add(3);
    LList.Add(1);
    LQuery := LList.AsNyx;
    LList[0] := 9;
    LList.Clear;
  finally
    LList.Free;
  end;
  Check(LQuery.SequenceEqual(TNyxSequence<Integer>.From([3, 1]), NewNyxIntegerEquality),
    'native list extension captures independently of caller mutation/disposal');
  LCopy := TNyxNativeQuery<Integer>.ToRTLList(LQuery);
  try
    LCopy[0] := 8;
    Check(LQuery.First = 3, 'caller-owned editable RTL copy does not mutate query storage');
  finally
    LCopy.Free;
  end;
  LTextList := TNyxTextList.Create;
  try
    LTextList.Add(TNyxText('English / 🌙') + TNyxText(#0));
    LTextQuery := LTextList.AsNyx;
  finally
    LTextList.Free;
  end;
  Check(LTextQuery.Single = TNyxText('English / 🌙') + TNyxText(#0),
    'native text list helper retains exact UTF8/NUL without ANSI collections');
  LQueue := TNyxIntegerQueue.Create;
  try
    LQueue.Enqueue(1);
    LQueue.Enqueue(2);
    LQuery := LQueue.AsNyx;
    Check((LQueue.Count = 2) and (LQueue.Peek = 1), 'query capture does not dequeue caller contents');
  finally
    LQueue.Free;
  end;
  Check(LQuery.SequenceEqual(NyxRange(1, 2), NewNyxIntegerEquality), 'queue captures FIFO encounter order');
  LStack := TNyxIntegerStack.Create;
  try
    LStack.Push(1);
    LStack.Push(2);
    LQuery := LStack.AsNyx;
    Check((LStack.Count = 2) and (LStack.Peek = 2), 'query capture does not pop caller contents');
    Check(LStack.AsNyx(nsoNativeEncounter).SequenceEqual(NyxRange(1, 2), NewNyxIntegerEquality),
      'typed native encounter policy preserves qualified RTL stack enumeration');
  finally
    LStack.Free;
  end;
  Check(LQuery.SequenceEqual(TNyxSequence<Integer>.From([2, 1]), NewNyxIntegerEquality),
    'default stack capture preserves top-first pop order without mutation');
  LGeneric := TNyxQueryList<TEntry>.Create;
  try
    LEntry.Key := 5;
    LEntry.Value := 'Generic value';
    LGeneric.Add(LEntry);
    LGenericQuery := LGeneric.AsNyx;
  finally
    LGeneric.Free;
  end;
  Check(LGenericQuery.Single.Value = 'Generic value', 'generic RTL-derived list provides extension syntax for arbitrary records');
  LDictionary := TDictionary<Integer, TNyxText>.Create;
  try
    LDictionary.Add(3, 'Third');
    LDictionary.Add(1, 'First');
    LEntries := TNyxNativeDictionary<Integer, TNyxText>.FromDictionary(LDictionary);
    LDictionary.Clear;
  finally
    LDictionary.Free;
  end;
  Check((LEntries.Count = 2) and (LEntries.First.Value <> ''),
    'native dictionary query owns typed entries after container disposal');
  LRefused := False;
  try
    TNyxNativeQuery<Integer>.FromEnumerable(nil);
  except
    on ENyxQuery do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'nil native container capture refuses');
end;

procedure NativeManagedCapture;
var
  LLifetime: TLifetime;
  LValue: IItem;
  LRead: IItem;
  LItems: TList<IItem>;
  LCaptured: INyxSequence<IItem>;
begin
  LLifetime := TLifetime.Create;
  LItems := nil;
  try
    LValue := TItem.Create(LLifetime);
    LItems := TList<IItem>.Create;
    LItems.Add(LValue);
    LCaptured := TNyxNativeQuery<IItem>.FromEnumerable(LItems);
    LItems.Free;
    LItems := nil;
    LValue := nil;
    Check(LLifetime.Destroyed = 0, 'generic native capture retains managed input after caller container disposal');
    LRead := LCaptured.First;
    LCaptured := nil;
    Check((LRead.GetValue = 7) and (LLifetime.Destroyed = 0), 'independent native capture reader owns its managed result');
    LRead := nil;
    Check(LLifetime.Destroyed = 1, 'last native capture reader retires managed result once');
  finally
    LRead := nil;
    LCaptured := nil;
    LValue := nil;
    LItems.Free;
    LLifetime.Free;
  end;
end;
{$endif}

function RunNyxMaterializationChecks: Integer;
begin
  GChecks := 0;
  PortableListAndTexts;
  DictionariesAndSets;
  LargeAndFailure;
  ManagedValues;
  {$ifndef PAS2JS}
  NativeHelpers;
  NativeManagedCapture;
  {$endif}
  Result := GChecks;
end;

end.
