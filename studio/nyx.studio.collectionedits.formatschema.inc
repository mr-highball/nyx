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



{ The six closed display policy shapes mirror TNyxValueFormat.FromData. Text
  byte admission and actual schema-domain checks still run in the typed codec. }
function ColumnFormatAgentSchema: TNyxText;
var
  LBase: TNyxText;
  LLocale: TNyxText;
  LBranches: TNyxText;

  procedure Branch(const AStyle, AProperties, ARequired: TNyxText);
  begin

    if LBranches <> '' then
    begin
      LBranches := LBranches + ',';
    end;
    LBranches := LBranches + '{"type":"object","properties":{' + LBase +
      ',"style":{"const":"' + AStyle + '"}' + AProperties +
      '},"required":["version","name","revision","locale","style"' + ARequired +
      '],"additionalProperties":false}';
  end;

begin
  LLocale := '{"type":"object","properties":{' +
    '"name":{"type":"string"},"decimal":{"type":"string","minLength":1,"maxLength":1},' +
    '"group":{"type":"string","maxLength":1}},' +
    '"required":["name","decimal","group"],"additionalProperties":false}';
  LBase := '"version":{"const":1},"name":{"type":"string","minLength":1,"maxLength":128},' +
    '"revision":{"type":"integer","minimum":1,"maximum":2147483646},"locale":' + LLocale;
  LBranches := '';
  Branch('canonical', '', '');
  Branch('decimal', ',"scale":{"type":"integer","minimum":0,"maximum":28},' +
    '"rounding":{"enum":["exact","to-even","away-from-zero","toward-zero","floor","ceiling"]}',
    ',"scale","rounding"');
  Branch('calendar-date', ',"order":{"enum":["year-month-day","day-month-year","month-day-year"]},' +
    '"separator":{"type":"string","minLength":1,"maxLength":1}', ',"order","separator"');
  Branch('clock-time', ',"precision":{"enum":["minute","second","tenth","hundredth","millisecond"]}', ',"precision"');
  Branch('boolean-labels', ',"true":{"type":"string","maxLength":4096},' +
    '"false":{"type":"string","maxLength":4096}', ',"true","false"');
  Branch('text-affixes', ',"prefix":{"type":"string","maxLength":4096},' +
    '"suffix":{"type":"string","maxLength":4096}', ',"prefix","suffix"');
  Result := '{"oneOf":[' + LBranches + ']}';
end;