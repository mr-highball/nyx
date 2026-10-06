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

unit nyx.test.dates;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.model;

{ Exercise the same portable date contract on checked FPC and actual pas2js.
  The caller owns its document; enrichment uses public typed contracts, and is
  explicitly separate from the unchanged semantic companion's accepted source. }
procedure ConfigureNyxDateReview(ADocument: TNyxDocument);
function RunNyxDateTests: Integer;

implementation

uses
  SysUtils, nyx.text, nyx.dates, nyx.data, nyx.contract, nyx.controls,
  nyx.types, nyx.state, nyx.binding.types, nyx.schema, nyx.codec, nyx.codegen,
  nyx.source;

procedure ConfigureNyxDateReview(ADocument: TNyxDocument);
begin
  ADocument.Components[0].Part('start').Contract.Value(
    NyxDateDomain.Range(NyxDate(2026, 1, 1), NyxDate(2026, 12, 31)));
  ADocument.Components[0].Part('finish').Contract.Value(
    NyxDateDomain.Range(NyxDate(2026, 1, 1), NyxDate(2026, 12, 31)));
  ADocument.State.SetValue(NyxTextState('arrival'), NyxDate(2026, 10, 6).ToText);
  ADocument.Find('first-arrival').Binds.Value(NyxTextState('arrival')).Done;
end;

function RunNyxDateTests: Integer;
var
  LDate: TNyxCalendarDate;
  LDomain: TNyxValueDomain;
  LBase: TNyxValueDomain;
  LControl: INyxDate;
  LEmpty: INyxDate;
  LPage: INyxPage;
  LDocument: TNyxDocument;
  LCopy: TNyxDocument;
  LWire: TNyxText;
  LSource: TNyxText;
  LRefused: Boolean;
  LIndex: Integer;
  LWorkspace: TNyxSourceWorkspace;
  LDraft: TNyxText;
const
  CInvalid: array[0..8] of TNyxText = ('1900-02-29', '2026-02-30',
    '2026-13-01', '0000-01-01', '2026-1-01', ' 2026-01-01',
    '2026-01-01 ', '２０２６-01-01', '2026-01-0🌙');

  procedure Check(AValue: Boolean; const AReason: TNyxText);
  begin

    if not AValue then
    begin
      raise Exception.Create('Date contract: ' + AReason);
    end;
    Inc(Result);
  end;

  procedure RefuseSource(const ABefore, AAfter, AReason: TNyxText);
  var
    LPosition: Integer;
    LCandidate: TNyxDocument;
    LFailed: Boolean;
  begin
    LPosition := Pos(ABefore, LSource);
    Check(LPosition > 0, 'the source refusal targets actual emitted syntax');
    LDraft := Copy(LSource, 1, LPosition - 1) + AAfter +
      Copy(LSource, LPosition + Length(ABefore), Length(LSource));
    LCandidate := nil;
    LFailed := False;
    try
      try
        LCandidate := LWorkspace.Candidate(LDocument, LDraft);
      except
        on LException: Exception do
        begin
          LFailed := LException.Message <> '';
        end;
      end;
    finally
      LCandidate.Free;
    end;
    Check(LFailed and (TNyxCodec.Encode(LDocument) = LWire), AReason);
  end;

begin
  Result := 0;
  Check(TryNyxDate('', LDate) and not LDate.Defined, 'empty is independent of today');
  Check(NyxDate(1, 1, 1).ToText = '0001-01-01', 'minimum year has exact padding');
  Check(NyxDate(9999, 12, 31).ToText = '9999-12-31', 'maximum year is exact');
  Check(TryNyxDate('2000-02-29', LDate) and (LDate.Day = 29), '400-year leap');
  Check(NyxDaysInMonth(1900, 2) = 28, 'century exception');
  Check(NyxDaysInMonth(2024, 2) = 29, 'ordinary leap');
  Check(NyxDate(2026, 12, 31).Compare(NyxDate(2027, 1, 1)) = -1, 'year ordering');
  Check(NyxDate(2026, 1, 1).Compare(NyxDate(2026, 1, 1)) = 0, 'equal dates');
  for LIndex := Low(CInvalid) to High(CInvalid) do
  begin
    Check(not TryNyxDate(CInvalid[LIndex], LDate) and not LDate.Defined,
      'malformed/impossible/non-ASCII date refuses without a retained value');
  end;
  LRefused := False;
  try
    NyxDate(2026, 2, 29);
  except
    on E: ENyxDateValue do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'typed impossible day refuses');
  LRefused := False;
  try
    NyxNoDate.Compare(NyxDate(2026, 1, 1));
  except
    on E: ENyxDateValue do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'empty is not an ordering epoch');

  LDomain := NyxDateDomain.Range(NyxDate(2026, 1, 1), NyxDate(2026, 12, 31)).Definition;
  Check(LDomain.CalendarDate and (LDomain.Kind = nskText), 'calendar semantics retain text wire');
  Check(LDomain.ReadWire('2026-01-01').AsText = '2026-01-01', 'inclusive lower bound');
  Check(LDomain.ReadWire('2026-12-31').AsText = '2026-12-31', 'inclusive upper bound');
  Check(LDomain.ReadWire('').AsText = '', 'range permits no date');
  LRefused := False;
  try
    LDomain.ReadWire('2027-01-01');
  except
    on E: ENyxContract do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'out-of-range value refuses');
  LRefused := False;
  try
    NyxDateDomain.Range(NyxDate(2026, 12, 31), NyxDate(2026, 1, 1));
  except
    on E: ENyxContract do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'descending range refuses atomically');
  Check(LDomain.ReadWire('2026-01-01').AsText = '2026-01-01', 'failed builder does not mutate an existing domain');
  LBase := NyxTextDomain.Choices(['2026-10-06', '2026-10-12']).Definition;
  Check(not LBase.CalendarDate, 'legacy base remains independent');
  LDomain := NyxDateDomain(LBase).Definition;
  Check(LDomain.CalendarDate and (LDomain.ToData.Field('choices').Count = 2),
    'legacy choice domain enrichment preserves its whitelist');
  LRefused := False;
  try
    NyxDateDomain(NyxTextDomain.Choices(['tomorrow']).Definition);
  except
    on E: ENyxContract do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'legacy invalid date choices refuse');
  LRefused := False;
  try
    TNyxValueDomain.FromData(NyxObject([
      NyxField('type', NyxData('integer')), NyxField('format', NyxData('date'))]));
  except
    on E: ENyxContract do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'date format cannot decorate integer domains');

  LDocument := TNyxDocument.Create;
  LCopy := nil;
  LWorkspace := TNyxSourceWorkspace.Create;
  try
    LPage := NewNyxPage('home');
    LDocument.AddPage(LPage);
    LControl := NewNyxDate('arrival');
    LPage.Add(LControl);
    LControl.Contract.Value(NyxDateDomain
      .Range(NyxDate(2026, 1, 1), NyxDate(2026, 12, 31))
      .Choices([NyxDate(2026, 10, 6), NyxDate(2026, 10, 12)]));
    LControl.WithDate(NyxDate(2026, 10, 6));
    Check(LControl.DateValue.ToText = '2026-10-06', 'specialized managed typed date');
    LControl.Configure.Value(NyxDate(2026, 10, 12)).Done;
    Check(LControl.DateValue.Day = 12, 'typed fluent configuration');
    LEmpty := NewNyxDate('departure');
    LPage.Add(LEmpty);
    LEmpty.WithDate(NyxNoDate);
    Check(not LEmpty.DateValue.Defined, 'specialized empty date has no implicit default');
    LWire := TNyxCodec.Encode(LDocument);
    LCopy := TNyxCodec.Decode(LWire);
    Check(TNyxCodec.Encode(LCopy) = LWire, 'exact date/contract persistence');
    Check(NyxNodeValueDomain(LCopy.Find('arrival')).CalendarDate, 'wire preserves domain semantics');
    LSource := TNyxCodegen.Generate(LDocument);
    Check(Pos('.Value(NyxDate(2026, 10, 12))', LSource) > 0, 'generated value is typed');
    Check(Pos('NyxDateDomain.Range(NyxDate(2026, 1, 1), NyxDate(2026, 12, 31))', LSource) > 0,
      'generated range is typed');
    Check(Pos('.Choices([NyxDate(2026, 10, 6), NyxDate(2026, 10, 12)])', LSource) > 0,
      'generated choices are typed');
    Check(LSource = TNyxCodegen.Generate(LCopy), 'generation is stable across exact wire');
    Check(Pos('.Value(NyxNoDate)', LSource) > 0, 'empty date generates a typed empty value');
    LCopy.Free;
    LCopy := nil;
    LSource := LWorkspace.Render(LDocument);
    LCopy := LWorkspace.Candidate(LDocument, LSource);
    Check(TNyxCodec.Encode(LCopy) = LWire,
      'Studio source admission exactly reconstructs dates, bounds and typed choices');
    RefuseSource('.Value(NyxDate(2026, 10, 12))', '.Value(NyxDate(2026, 2, 29))',
      'impossible typed source date refuses atomically');
    RefuseSource('NyxDate(2026, 1, 1)', 'NyxDate(''2026'', 1, 1)',
      'calendar parts cannot be supplied as raw source strings');
    RefuseSource('.Range(NyxDate(2026, 1, 1),', '.Range(''2026-01-01'',',
      'calendar bounds cannot be supplied as raw source strings');
    RefuseSource('.Choices([NyxDate(2026, 10, 6),', '.Choices([True,',
      'wrong typed choice source refuses atomically');
  finally
    LCopy.Free;
    LWorkspace.Free;
    LEmpty := nil;
    LControl := nil;
    LPage := nil;
    LDocument.Free;
  end;
end;

end.
