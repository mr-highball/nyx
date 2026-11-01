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
program nyx_choice_items_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.choices
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure RefuseData(const AJSON: TNyxText);
var
  LRefused: Boolean;
begin
  LRefused := False;
  try
    NyxChoicesFromData(TNyxDataValue.ParseJSON(AJSON));
  except
    on ENyxChoice do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'invalid choice descriptor must refuse: ' + AJSON);
end;

procedure Cases;
var
  LEmpty: INyxChoiceItems;
  LFirst: INyxChoiceItems;
  LSecond: INyxChoiceItems;
  LRoundTrip: INyxChoiceItems;
  LRows: array of TNyxChoiceItem;
  LRefused: Boolean;
  LIndex: Integer;
  LBefore: TNyxText;
  LKey: TNyxText;
  LCaption: TNyxText;
begin
  LEmpty := NewNyxChoices;
  Check((LEmpty.Count = 0) and (LEmpty.TextBytes = 0),
    'defined empty choices are independent from an absent model choice declaration');
  LRefused := False;
  try
    LEmpty.IndexOf(Default(TNyxChoiceRef));
  except
    on ENyxChoice do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'an uninitialized key must not silently mean the explicit empty choice');
  LFirst := LEmpty.WithChoice(NyxChoice(''), 'None')
    .WithChoice(NyxChoice('parse-json'), 'Parse JSON');
  LBefore := LFirst.ToData.ToJSON;
  Check(NyxSameChoiceItems(LFirst, NyxChoicesFromData(LFirst.ToData)),
    'independent admitted snapshots with equal keys/captions/order compare equal');
  Check(not NyxSameChoiceItems(nil, LEmpty),
    'absence remains distinct from an explicitly empty immutable set');
  LSecond := LFirst.WithChoice(NyxChoice('parse-json-copy'), 'Parse JSON')
    .WithChoice(NyxChoice('記録 🌙'), 'English caption 🌙');
  Check((LEmpty.Count = 0) and (LFirst.Count = 2) and (LSecond.Count = 4),
    'immutable fluent branches never append into an older collection');
  Check(LFirst.ToData.ToJSON = LBefore, 'a later branch preserves exact earlier descriptor bytes');
  Check(not NyxSameChoiceItems(LFirst, LSecond),
    'different immutable branches do not compare equal merely through shared prefixes');
  Check((LSecond.IndexOf(NyxChoice('parse-json')) = 1) and
    (LSecond.IndexOf(NyxChoice('parse-json-copy')) = 2) and
    (LSecond.Item(1).Caption = LSecond.Item(2).Caption),
    'duplicate captions retain distinct stable values');
  Check((LSecond.IndexOf(NyxChoice('')) = 0) and
    (LSecond.IndexOf(NyxChoice('missing')) = -1) and
    (LSecond.IndexOf(NyxChoice('PARSE-JSON')) = -1),
    'empty, absent and exact case-sensitive keys remain distinct');
  Check((LSecond.Item(3).Key.Value = TNyxText('記録 🌙')) and
    (LSecond.Item(3).Caption = TNyxText('English caption 🌙')),
    'keys and captions preserve supplementary and non-Latin Unicode as independent data');
  Check(LSecond.TextBytes = 80, 'logical UTF-8 accounting agrees across native and browser');
  LRoundTrip := NyxChoicesFromData(TNyxDataValue.ParseJSON(LSecond.ToData.ToJSON));
  Check(LRoundTrip.ToData.ToJSON = LSecond.ToData.ToJSON,
    'descriptor round trip retains ordered exact keys and captions');
  LRefused := False;
  try
    LSecond.WithChoice(NyxChoice('parse-json'), 'A different caption');
  except
    on ENyxChoice do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LFirst.ToData.ToJSON = LBefore) and (LSecond.Count = 4),
    'duplicate-key refusal never changes any accepted branch');
  LRefused := False;
  try
    NyxChoiceItem(NyxChoice('line'), 'Two' + #10 + 'lines');
  except
    on ENyxChoice do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'display captions reject unsupported multiline control text');
  SetLength(LRows, 1);
  LRows[0] := NyxChoiceItem(NyxChoice('owned'), 'Owned caption');
  LRoundTrip := NewNyxChoices(LRows);
  LRows[0] := NyxChoiceItem(NyxChoice('changed'), 'Changed by caller');
  SetLength(LRows, 0);
  Check(LRoundTrip.Item(0).Key.Value = TNyxText('owned'),
    'caller array replacement and retirement cannot mutate admitted choices');

  RefuseData('null');
  RefuseData('{"version":1,"items":[],"extra":false}');
  RefuseData('{"version":2,"items":[]}');
  RefuseData('{"version":1.5,"items":[]}');
  RefuseData('{"version":1,"items":{}}');
  RefuseData('{"version":1,"items":[{"value":"a","caption":false}]}');
  RefuseData('{"version":1,"items":[{"value":"a","Caption":"Wrong field"}]}');
  RefuseData('{"version":1,"items":[{"value":"a","caption":"A","extra":true}]}');
  RefuseData('{"version":1,"items":[{"value":"a","caption":"A"},{"value":"a","caption":"B"}]}');

  SetLength(LRows, NyxMaximumChoiceItems);
  for LIndex := 0 to High(LRows) do
  begin
    LRows[LIndex] := NyxChoiceItem(NyxChoice(TNyxText(IntToStr(LIndex))), 'Choice');
  end;
  LRoundTrip := NewNyxChoices(LRows);
  Check((LRoundTrip.Count = NyxMaximumChoiceItems) and
    (LRoundTrip.IndexOf(NyxChoice('1023')) = 1023), 'full bounded choice collection remains queryable');
  LRefused := False;
  try
    LRoundTrip.WithChoice(NyxChoice('overflow'), 'Overflow');
  except
    on ENyxChoice do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LRoundTrip.Count = NyxMaximumChoiceItems),
    'item overflow refuses without changing the accepted full collection');
  LCaption := TNyxText(StringOfChar('a', NyxMaximumChoiceTextBytes));
  LRoundTrip := LEmpty.WithChoice(NyxChoice(''), LCaption);
  Check(LRoundTrip.TextBytes = NyxMaximumChoiceTextBytes, 'exact text-byte boundary is admitted');
  LRefused := False;
  try
    LRoundTrip.WithChoice(NyxChoice('x'), '');
  except
    on ENyxChoice do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LRoundTrip.TextBytes = NyxMaximumChoiceTextBytes),
    'aggregate text overflow preserves the full admitted collection');
  LKey := TNyxText(StringOfChar(#0, NyxMaximumChoiceTextBytes));
  LRoundTrip := LEmpty.WithChoice(NyxChoice(LKey), '');
  LRoundTrip := NyxChoicesFromData(LRoundTrip.ToData);
  Check(LRoundTrip.Item(0).Key.Value = LKey,
    'worst escaped key at the logical byte limit still round trips exactly');
end;

begin
  try
    Cases;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-choice-items', 'passed');
    document.body.setAttribute('data-choice-checks', IntToStr(GChecks));
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' portable choice data checks';
    {$else}
    WriteLn('PASS ', GChecks, ' portable choice data checks');
    {$endif}
  except
    on LError: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-choice-items', 'failed');
      document.body.textContent := LError.Message;
      {$else}
      WriteLn('FAIL ', LError.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
