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

program nyx_time_values_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.times, nyx.data, nyx.json, nyx.contract, nyx.state,
  nyx.test.times, nyx.test.dates
  {$ifndef PAS2JS}, Classes, nyx.model, nyx.codec, nyx.codegen{$endif}
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;
  GDateChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

{ The same Pascal program qualifies immutable values on both compilers. DOM
  attributes only publish its result; this fixture does not qualify a picker,
  binding, native popup or operating-system time convention. }
procedure Run;
const
  CValid: array[0..12] of TNyxText = ('', '00:00', '23:59', '08:30:00',
    '08:30:59', '00:00:00.0', '08:30:00.01', '08:30:00.010',
    '08:30:00.100', '08:30:00.12', '23:59:59.999', '12:03:04.5', '00:00:00.000');
  CInvalid: array[0..20] of TNyxText = ('0:00', '00:0', '24:00', '12:60',
    '23:59:60', '12:00:00.', '12:00:00.0000', '12:00.5', ' 12:00', '12:00 ',
    '12:00Z', '12:00+00:00', '12:00 PM', '1200', '12-00', '12:00:00,1',
    '١٢:٠٠', '１２:００', '12:🙂', '2026-10-07T12:00', '12:00' + #0);
var
  LTime: TNyxClockTime;
  LCopy: TNyxClockTime;
  LParsed: TNyxClockTime;
  LIndex: Integer;
  LHour: Integer;
  LMinute: Integer;
  LRefused: Boolean;
  LParts: array[0..3] of Integer;
begin
  LTime := NyxNoTime;
  Check(not LTime.Defined and (LTime.ToText = ''), 'Default clock is explicitly empty');
  LTime := NyxTime(0, 0);
  Check(LTime.Defined and (LTime.MillisecondsSinceMidnight = 0) and
    (LTime.ToText = '00:00'), 'Defined midnight is distinct from an empty clock');
  Check(NyxTime(23, 59, 59, 999).MillisecondsSinceMidnight = NyxMillisecondsPerDay - 1,
    'Last valid millisecond has exact bounded integer representation');
  for LIndex := Low(CValid) to High(CValid) do
  begin
    Check(TryNyxTime(CValid[LIndex], LTime), 'Valid clock spelling parses');
    Check(LTime.ToText = CValid[LIndex], 'Parsing retains the exact ASCII spelling and precision');

    if LTime.Defined then
    begin
      LCopy := NyxTime(LTime.Hour, LTime.Minute, LTime.Second, LTime.Millisecond)
        .WithPrecision(LTime.Precision);
      Check((LCopy.Compare(LTime) = 0) and (LCopy.ToText = CValid[LIndex]),
        'Typed parts and precision reconstruct the exact reading and spelling');
    end;
  end;
  for LIndex := Low(CInvalid) to High(CInvalid) do
  begin
    LTime := NyxTime(12, 0);
    Check(not TryNyxTime(CInvalid[LIndex], LTime) and not LTime.Defined,
      'Malformed wire refuses without retaining a partial/prior result');
    LRefused := False;
    try
      LTime := TNyxClockTime.FromText(CInvalid[LIndex]);
    except
      on ENyxTimeValue do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Checked wire constructor reports the specific argument failure');
  end;

  { Every valid minute is checked with a nonzero second/fraction. This spans day
    boundaries without enumerating millions of mechanically identical values. }
  for LHour := 0 to 23 do
  begin
    for LMinute := 0 to 59 do
    begin
      LTime := NyxTime(LHour, LMinute, 59, 999);
      Check(TryNyxTime(LTime.ToText, LParsed) and (LTime.Compare(LParsed) = 0) and
        (LParsed.MillisecondsSinceMidnight =
          ((LHour * 60 + LMinute) * 60 + 59) * 1000 + 999),
        'Every minute retains exact last-second/millisecond parts through wire');
    end;
  end;

  for LIndex := 0 to 7 do
  begin
    LParts[0] := 12;
    LParts[1] := 30;
    LParts[2] := 0;
    LParts[3] := 0;

    if LIndex mod 2 = 0 then
    begin
      LParts[LIndex div 2] := -1;
    end
    else
    begin
      case LIndex div 2 of
        0: LParts[0] := 24;
        1, 2: LParts[LIndex div 2] := 60;
        3: LParts[3] := 1000;
      end;
    end;
    LRefused := False;
    try
      LTime := NyxTime(LParts[0], LParts[1], LParts[2], LParts[3]);
    except
      on ENyxTimeValue do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Out-of-range typed parts refuse before construction');
  end;

  LTime := NyxTime(8, 30);
  LCopy := LTime.WithPrecision(ntpMillisecond);
  Check((LTime.ToText = '08:30') and (LCopy.ToText = '08:30:00.000') and
    (LTime.Compare(LCopy) = 0), 'Precision changes an independent spelling without changing time');
  Check((NyxTime(8, 30, 0, 100).WithPrecision(ntpTenth).ToText = '08:30:00.1') and
    (NyxTime(8, 30, 0, 120).WithPrecision(ntpHundredth).ToText = '08:30:00.12'),
    'Lossless shorter fractional spellings use typed precision choices');
  for LIndex := Ord(ntpMinute) to Ord(ntpHundredth) do
  begin
    LTime := NyxTime(8, 30, 1, 125);
    LRefused := False;
    try
      LCopy := LTime.WithPrecision(TNyxTimePrecision(LIndex));
    except
      on ENyxTimeValue do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LTime.ToText = '08:30:01.125'),
      'Lossy precision refuses without changing the independent baseline');
  end;
  Check((NyxTime(23, 59).Compare(NyxTime(0, 0)) = 1) and
    (NyxTime(0, 0).Compare(NyxTime(0, 0, 0, 1)) = -1),
    'Clock comparison follows reading order, independently of periodic ranges');
  LRefused := False;
  try
    LTime := NyxNoTime;
    LTime.Compare(NyxTime(0, 0));
  except
    on ENyxTimeValue do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'An empty clock cannot silently compare as midnight');
  LRefused := False;
  try
    LTime := NyxNoTime;
    LIndex := LTime.MillisecondsSinceMidnight;
  except
    on ENyxTimeValue do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'An empty clock has no numeric reading');
end;

{ Domain checks exercise independent specifications and descriptor admission,
  without pretending that a store-only fixture establishes live UI binding. }
procedure RunDomains;
var
  LDomain: TNyxTimeDomain;
  LCopy: TNyxTimeDomain;
  LDefinition: TNyxValueDomain;
  LBefore: TNyxText;
  LRefused: Boolean;
  LIndex: Integer;

  function Admitted(const ADomain: TNyxTimeDomain; const AWire: TNyxText): Boolean;
  begin
    Result := False;
    try
      ADomain.Definition.ReadWire(AWire);
      Result := True;
    except
      on ENyxContract do
      begin
        { Rejected user values are the expected contract failure. }
      end;
    end;
  end;

  procedure BadDescriptor(const AData: TNyxDataValue);
  var
    LRejected: Boolean;
  begin
    LRejected := False;
    try
      TNyxValueDomain.FromData(AData);
    except
      on ENyxContract do
      begin
        LRejected := True;
      end;
      on ENyxJSON do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'Malformed/wrong-family clock descriptor refuses');
  end;

begin
  LDomain := NyxTimeDomain;
  LDefinition := LDomain.Definition;
  Check(LDefinition.ClockTime and not LDefinition.CalendarDate and
    (LDefinition.Kind = nskText) and (LDefinition.TimeStepMilliseconds = 0),
    'Clock specification uses exact text storage with an explicit time format');
  Check((LDefinition.ReadWire('08:30:00.010').AsText = '08:30:00.010') and
    Admitted(LDomain, ''), 'Clock admission retains exact wire precision and optional empty');
  Check(not Admitted(LDomain, '24:00') and not Admitted(LDomain, '09:30 PM'),
    'Clock domain refuses invalid/locale wire without interpretation');
  LDomain := NyxTimeDomain.Range(NyxTime(9, 0), NyxTime(17, 0));
  Check(Admitted(LDomain, '09:00') and Admitted(LDomain, '17:00:00.000') and
    not Admitted(LDomain, '08:59:59.999') and not Admitted(LDomain, '17:00:00.001'),
    'Ordinary time bounds are exact and inclusive');
  LDomain := NyxTimeDomain.Range(NyxTime(21, 0), NyxTime(6, 0));
  Check(Admitted(LDomain, '21:00') and Admitted(LDomain, '06:00') and
    Admitted(LDomain, '00:00') and not Admitted(LDomain, '12:00'),
    'Reversed bounds admit the two portions spanning midnight');
  LCopy := NyxTimeDomain.Minimum(NyxTime(21, 0));
  Check(Admitted(LCopy, '23:59:59.999') and not Admitted(LCopy, '20:59:59.999'),
    'An independent minimum uses the last clock millisecond as the open upper limit');
  LCopy := NyxTimeDomain.Maximum(NyxTime(6, 0));
  Check(Admitted(LCopy, '00:00') and not Admitted(LCopy, '06:00:00.001'),
    'An independent maximum uses midnight as the open lower limit');
  LCopy := NyxTimeDomain.Range(NyxTime(12, 0), NyxTime(12, 0));
  Check(Admitted(LCopy, '12:00:00') and not Admitted(LCopy, '12:00:00.001') and
    not Admitted(LCopy, '00:00'), 'Equal bounds designate one reading, not the whole day');
  LDomain := NyxTimeDomain.Minimum(NyxTime(0, 0, 0, 5)).StepMilliseconds(10);
  Check(Admitted(LDomain, '00:00:00.005') and Admitted(LDomain, '00:00:00.015') and
    not Admitted(LDomain, '00:00:00.010'), 'A fixed exact step uses the minimum as its base');
  LDomain := NyxTimeDomain.Range(NyxTime(21, 0, 0, 500), NyxTime(6, 0, 0, 500))
    .StepSeconds(3600);
  Check(Admitted(LDomain, '00:00:00.500') and not Admitted(LDomain, '00:00:00.501') and
    not Admitted(LDomain, '00:30:00.500'), 'Signed step differences work across midnight without rounding');
  LBefore := LDomain.Definition.ToData.ToJSON;
  LCopy := LDomain.AnyStep;
  Check(Admitted(LCopy, '00:30:00.001') and (LCopy.Definition.TimeStepMilliseconds = 0) and
    (LDomain.Definition.ToData.ToJSON = LBefore), 'AnyStep returns an independent specification');
  for LIndex := 0 to 2 do
  begin
    LRefused := False;
    try
      case LIndex of
        0: LCopy := LDomain.StepMilliseconds(0);
        1: LCopy := LDomain.StepMilliseconds(-1);
        2: LCopy := LDomain.StepSeconds(High(Integer));
      end;
    except
      on ENyxContract do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LDomain.Definition.ToData.ToJSON = LBefore),
      'Invalid/overflowing steps refuse without changing the original domain');
  end;
  LDomain := NyxTimeDomain.Choices([NyxNoTime, NyxTime(8, 30), NyxTime(9, 0, 0, 120)]);
  Check(Admitted(LDomain, '') and Admitted(LDomain, '08:30:00.000') and
    Admitted(LDomain, '09:00:00.12') and not Admitted(LDomain, '08:31'),
    'Typed choice membership compares clock readings while preserving wire text');
  Check(LDomain.Definition.ReadWire('09:00:00.12').AsText = '09:00:00.12',
    'Choice membership never rewrites the accepted spelling');
  LBefore := LDomain.Definition.ToData.ToJSON;
  LRefused := False;
  try
    LCopy := LDomain.Choices([NyxTime(8, 30), NyxTime(8, 30).WithPrecision(ntpSecond)]);
  except
    on ENyxContract do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LDomain.Definition.ToData.ToJSON = LBefore),
    'Equivalent temporal choices refuse as duplicates without changing the original');
  LDefinition := TNyxValueDomain.FromData(LDomain.Definition.ToData);
  Check(LDefinition.ToData.ToJSON = LBefore, 'Clock descriptor reconstruction retains exact metadata');
  BadDescriptor(NyxObject([NyxField('type', NyxData(NyxStateKindName(nskText))), NyxField('format', NyxData('time')),
    NyxField('min', NyxData(''))]));
  BadDescriptor(NyxObject([NyxField('type', NyxData(NyxStateKindName(nskText))), NyxField('format', NyxData('time')),
    NyxField('max', NyxData('24:00'))]));
  BadDescriptor(NyxObject([NyxField('type', NyxData(NyxStateKindName(nskText))), NyxField('format', NyxData('time')),
    NyxField('step', NyxData(True))]));
  BadDescriptor(NyxObject([NyxField('type', NyxData(NyxStateKindName(nskText))), NyxField('format', NyxData('time')),
    NyxField('step', NyxData(0))]));
  BadDescriptor(NyxObject([NyxField('type', NyxData(NyxStateKindName(nskText))), NyxField('format', NyxData('time')),
    NyxField('step', NyxData(1.5))]));
  BadDescriptor(NyxObject([NyxField('type', NyxData(NyxStateKindName(nskText))), NyxField('step', NyxData(10))]));
  BadDescriptor(NyxObject([NyxField('type', NyxData(NyxStateKindName(nskInteger))), NyxField('format', NyxData('time'))]));
end;

{$ifndef PAS2JS}
{ Export the exact fixture for real compiler reconstruction. This optional file
  boundary creates no listener and edits no accepted Studio document or source. }
procedure ExportFixture(const ADirectory: TNyxText);
var
  LDocument: TNyxDocument;

  procedure Save(const AName, AText: TNyxText);
  var
    LStream: TFileStream;
  begin
    LStream := TFileStream.Create(IncludeTrailingPathDelimiter(ADirectory) + AName, fmCreate);
    try

      if AText <> '' then
      begin
        LStream.WriteBuffer(AText[1], Length(AText));
      end;
    finally
      LStream.Free;
    end;
  end;

begin
  LDocument := CreateNyxTimeFixture;
  try
    Save('nyx.generated.time.pas', TNyxCodegen.Generate(LDocument, 'nyx.generated.time'));
    Save('time-fixture.nyx', TNyxCodec.Encode(LDocument));
  finally
    LDocument.Free;
  end;
end;
{$endif}

{ Run the complete checked suite unchanged on both compilers. Browser startup
  defers this work until navigation can finish; success still requires every
  assertion, not script download or an intermediate phase marker. }
procedure Qualify;
begin
  try
    {$ifdef PAS2JS}document.body.setAttribute('data-time-phase', 'values');{$endif}
    Run;
    {$ifdef PAS2JS}document.body.setAttribute('data-time-phase', 'domains');{$endif}
    RunDomains;
    {$ifdef PAS2JS}document.body.setAttribute('data-time-phase', 'authoring');{$endif}
    Inc(GChecks, RunNyxTimeAuthoringTests);
    GDateChecks := RunNyxDateTests;
    {$ifndef PAS2JS}

    if ParamCount > 0 then
    begin
      ExportFixture(ParamStr(1));
    end;
    {$endif}
    {$ifdef PAS2JS}
    document.body.setAttribute('data-time-values', 'passed');
    document.body.setAttribute('data-time-checks', IntToStr(GChecks));
    document.body.setAttribute('data-date-regression-checks', IntToStr(GDateChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' portable clock-time value checks');
    WriteLn('PASS ', GDateChecks, ' unchanged typed calendar regression checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-time-values', 'failed');
      document.body.setAttribute('data-time-error', LException.Message);
      document.body.setAttribute('data-event-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end;

begin
  {$ifdef PAS2JS}
  { Give the normal HTTP document/navigation turn a chance to finish. This uses
    the browser's real clock and leaves all checked value/domain/source work in
    the maintained Pascal consumer. }
  window.setTimeout(@Qualify, 100);
  {$else}
  Qualify;
  {$endif}
end.
