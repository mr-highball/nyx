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
program nyx_source_worker_transport_browser;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, JS, Web, Math, nyx.text, nyx.data, nyx.json,
  nyx.studio.worker.transport;

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

function Pair(const AFirst, ASecond: JSValue): TJSArray;
begin
  Result := TJSArray.new;
  Result.push(AFirst);
  Result.push(ASecond);
end;

procedure Refuse(const AValue: JSValue);
var
  LPacket: TNyxSourceWorkerPacket;
  LRefused: Boolean;
begin
  LRefused := False;
  try
    LPacket := TNyxSourceWorkerPacket.Read(AValue);
  except
    on ENyxSourceWorkerTransport do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'malformed host framing is refused before logical decoding');
end;

procedure Run;
var
  LText: TNyxText;
  LPacket: TNyxSourceWorkerPacket;
  LCopy: TNyxSourceWorkerPacket;
  LArray: TJSArray;
  LValue: TNyxDataValue;
  LRefused: Boolean;
  LIndex: Integer;
begin
  LText := TNyxText('{"number":9007199254740993,"text":"moon 🌙\u0000"}');
  LPacket := TNyxSourceWorkerPacket.Tagged(TNyxSourceWorkerSequence(23), LText);
  LCopy := TNyxSourceWorkerPacket.Read(LPacket.ToValue);
  Check(LCopy.HasSequence and (Integer(LCopy.Sequence) = 23), 'ordinal identity survives structured cloning shape');
  Check(LCopy.Text = LText, 'logical text preserves exact numbers, escapes and supplementary Unicode');
  LValue := TNyxDataValue.ParseJSON(LCopy.Text);
  Check(LValue.Field('number').ToJSON = '9007199254740993',
    'transport does not coerce a large exact JSON number through Double');
  Check(LValue.Field('text').AsText = TNyxText('moon 🌙') + #0,
    'logical decoding retains supplementary Unicode and an embedded NUL');
  LArray := TJSArray(LPacket.ToValue);
  LArray[0] := 24;
  LArray[1] := 'Different';
  Check((LPacket.Text = LText) and (Integer(LPacket.Sequence) = 23),
    'exported host array cannot mutate an owned packet');
  LCopy := LPacket.WithText('Reply');
  Check((Integer(LCopy.Sequence) = 23) and (LCopy.Text = 'Reply') and (LPacket.Text = LText),
    'reply text retains exact request identity without modifying the request');
  LCopy := TNyxSourceWorkerPacket.Read(LText);
  Check(not LCopy.HasSequence and (Integer(LCopy.Sequence) = 0) and
    (LCopy.Text = LText) and isString(LCopy.ToValue), 'older text transport remains explicit and byte-exact');
  LCopy := TNyxSourceWorkerPacket.Read(TNyxSourceWorkerPacket.Tagged(
    TNyxSourceWorkerSequence(High(Integer)), 'Last ordinal').ToValue);
  Check(Integer(LCopy.Sequence) = High(Integer), 'highest supported request ordinal remains exact');
  Refuse(TJSObject.new);
  Refuse(TJSArray.new);
  Refuse(Pair('1', 'Text'));
  Refuse(Pair(True, 'Text'));
  Refuse(Pair(1, TJSObject.new));
  Refuse(Pair(0, 'Text'));
  Refuse(Pair(-1, 'Text'));
  Refuse(Pair(0.5, 'Text'));
  Refuse(Pair(1.5, 'Text'));
  Refuse(Pair(2147483648.0, 'Text'));
  Refuse(Pair(NaN, 'Text'));
  Refuse(Pair(Infinity, 'Text'));
  Refuse(Pair(-Infinity, 'Text'));
  LArray := Pair(1, 'Text');
  LArray.push('Unexpected');
  Refuse(LArray);
  for LIndex := -1 to 0 do
  begin
    LRefused := False;
    try
      LPacket := TNyxSourceWorkerPacket.Tagged(TNyxSourceWorkerSequence(LIndex), 'Text');
    except
      on ENyxSourceWorkerTransport do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'typed construction rejects a nonpositive request ordinal');
  end;
  { The framing carries text outside the logical decoder. A formerly valid
    exact-budget packet must remain valid, and one extra byte must still fail. }
  LText := NyxObject([NyxField('version', NyxData(1)),
    NyxField('schemas', NyxObject([])), NyxField('source', NyxData(''))]).ToJSON;
  LText := StringReplace(LText, '""',
    '"' + StringOfChar('x', NyxMaximumJSONBytes - Length(LText)) + '"', []);
  LPacket := TNyxSourceWorkerPacket.Read(TNyxSourceWorkerPacket.Tagged(
    TNyxSourceWorkerSequence(25), LText).ToValue);
  LValue := TNyxDataValue.ParseJSON(LPacket.Text);
  Check((Length(LPacket.Text) = NyxMaximumJSONBytes) and (LValue.Count = 3),
    'tagged framing preserves the exact original logical JSON byte boundary');
  LRefused := False;
  try
    LValue := TNyxDataValue.ParseJSON(LPacket.Text + ' ');
  except
    on ENyxJSON do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'framing cannot raise or bypass the original JSON byte limit');
  document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' worker transport checks';
  document.body.setAttribute('data-test-result', 'passed');
  document.body.setAttribute('data-test-checks', IntToStr(GChecks));
end;

begin
  try
    Run;
  except
    on LException: Exception do
    begin
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-test-result', 'failed');
    end;
  end;
end.
