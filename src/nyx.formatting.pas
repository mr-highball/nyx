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
unit nyx.formatting;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.state, nyx.data, nyx.contract, nyx.resources,
  nyx.numbers.decimal, nyx.times;

type
  { Rule/family/budget refusal never changes a scalar or draft. Existing exact
    decimal/calendar/domain primitives retain their specific value errors. }
  ENyxFormatting = class(ENyxState);

  { An open application formatter identity, distinct from resource names and
    paths. Revision and complete copied policy accompany this descriptive name;
    equal names alone never certify equal formatting rules. }
  TNyxFormatterRef = record
  private
    FName: TNyxText;
  public
    property Name: TNyxText read FName;
  end;

  { Explicit portable locale facts. The locale name records provenance; it does
    not invoke a machine locale or claim automatic CLDR lookup. Callers supply
    decimal/group separators deliberately, and copies retain exact Unicode.
    Grouping is a three-digit convention; empty grouping disables it. }
  TNyxFormatLocale = record
  private
    FDefined: Boolean;
    FLocale: TNyxLocaleRef;
    FDecimal: TNyxText;
    FGroup: TNyxText;
  public
    class function Invariant: TNyxFormatLocale; static;
    function Named(const ALocale: TNyxLocaleRef): TNyxFormatLocale;
    function NumberSeparators(const ADecimal, AGroup: TNyxText): TNyxFormatLocale;
    function Copy: TNyxFormatLocale;
    procedure Validate;
    { Closed persistence owns explicit facts, never a machine locale lookup. }
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxFormatLocale; static;
    property Locale: TNyxLocaleRef read FLocale;
    property DecimalSeparator: TNyxText read FDecimal;
    property GroupSeparator: TNyxText read FGroup;
  end;

  { Closed presentation choices never guess a date/number from a text caption.
    Canonical retains the established accepted-value spelling. }
  TNyxValueFormatStyle = (nvfsCanonical, nvfsDecimal, nvfsCalendarDate,
    nvfsClockTime, nvfsBooleanLabels, nvfsTextAffixes);

  { Calendar order is explicit, independent of OS short-date preferences. }
  TNyxFormatDateOrder = (nfdoYearMonthDay, nfdoDayMonthYear, nfdoMonthDayYear);

  { Paint status is independent of accepted data and strict output commands.
    Unavailable is an expected presentation refusal, never a model failure. }
  TNyxValuePresentationOutcome = (nvpoCanonical, nvpoFormatted, nvpoUnavailable);

  { Owned paint facts retain full canonical RAW separately from visible text.
    A visible refusal marker prevents an unavailable format from masquerading
    as successful formatted output; no view, callback or locale owner survives. }
  TNyxValuePresentation = record
  private
    FOutcome: TNyxValuePresentationOutcome;
    FText: TNyxText;
    FRaw: TNyxText;
    FDiagnostic: TNyxText;
  public
    property Outcome: TNyxValuePresentationOutcome read FOutcome;
    property Text: TNyxText read FText;
    property RawText: TNyxText read FRaw;
    property Diagnostic: TNyxText read FDiagnostic;
  end;

  { Immutable formatter snapshot with no host, callback, source or live locale
    owner. Decimal presentation reuses Nyx's exact 96-bit/scale28 decimal and
    explicit rounding; values outside that exact domain refuse instead of
    silently passing through binary/target-dependent FormatFloat. Calendar and
    clock rules require the corresponding existing text domain. }
  TNyxValueFormat = record
  private
    FDefined: Boolean;
    FIdentity: TNyxFormatterRef;
    FRevision: Integer;
    FLocale: TNyxFormatLocale;
    FStyle: TNyxValueFormatStyle;
    FScale: Integer;
    FRounding: TNyxDecimalRounding;
    FOrder: TNyxFormatDateOrder;
    FDateSeparator: TNyxText;
    FTimePrecision: TNyxTimePrecision;
    FFirst: TNyxText;
    FSecond: TNyxText;
    function BeginStyle(AStyle: TNyxValueFormatStyle): TNyxValueFormat;
  public
    class function Canonical: TNyxValueFormat; static;
    function Identified(const AIdentity: TNyxFormatterRef; ARevision: Integer): TNyxValueFormat;
    function Localized(const ALocale: TNyxFormatLocale): TNyxValueFormat;
    function Decimal(AScale: Integer; ARounding: TNyxDecimalRounding = ndrToEven): TNyxValueFormat;
    function CalendarDate(AOrder: TNyxFormatDateOrder;
      const ASeparator: TNyxText = '-'): TNyxValueFormat;
    function ClockTime(APrecision: TNyxTimePrecision): TNyxValueFormat;
    function BooleanLabels(const ATrue, AFalse: TNyxText): TNyxValueFormat;
    function TextAffixes(const APrefix, ASuffix: TNyxText): TNyxValueFormat;
    function Copy: TNyxValueFormat;
    procedure Validate;
    { Family/domain admission is separate from formatting a value. A display
      choice cannot reinterpret an ordinary text field as a date or number. }
    procedure ValidateKind(AKind: TNyxStateKind);
    procedure ValidateDomain(AKind: TNyxStateKind; const ADomain: TNyxValueDomain);
    { Null denotes absence; a defined canonical policy remains explicit. Each
      style has a closed shape so generated fluent code preserves exact facts. }
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxValueFormat; static;
    function FormatValue(const AValue: TNyxStateValue;
      const ADomain: TNyxValueDomain; AMaximumUTF8Bytes: Integer = 4194304): TNyxText;
    { Paint/Fit only. Structural/schema/value admission occurs before the narrow
      expected format-refusal catch. Strict FormatValue and copy/export remain
      unchanged. A full RAW value survives even if only the marker fits. }
    function PresentValue(const AValue: TNyxStateValue;
      const ADomain: TNyxValueDomain; AMaximumUTF8Bytes: Integer = 4194304): TNyxValuePresentation;
    property Identity: TNyxFormatterRef read FIdentity;
    property Revision: Integer read FRevision;
    property Locale: TNyxFormatLocale read FLocale;
    property Style: TNyxValueFormatStyle read FStyle;
    property Scale: Integer read FScale;
    property Rounding: TNyxDecimalRounding read FRounding;
    property DateOrder: TNyxFormatDateOrder read FOrder;
    property DateSeparator: TNyxText read FDateSeparator;
    property TimePrecision: TNyxTimePrecision read FTimePrecision;
    property FirstText: TNyxText read FFirst;
    property SecondText: TNyxText read FSecond;
    property Defined: Boolean read FDefined;
  end;

function NyxFormatter(const AName: TNyxText): TNyxFormatterRef;
{ Readable starting points for immutable authored display policies. }
function NyxValueFormat: TNyxValueFormat;
function NyxFormatLocale: TNyxFormatLocale;
{ Explicit canonical scalar spelling shared by representations. Text includes
  NUL/newlines unchanged, Boolean is lower-case and numbers use NumberText. }
function NyxRawScalarText(const AValue: TNyxStateValue): TNyxText;
{ Paint/Fit fast path for an already admitted collection scalar. It copies
  canonical spelling/status without re-admitting formatter/schema/domain JSON.
  The visible byte ceiling still applies, and a refusal retains exact RAW. }
function NyxCanonicalPresentation(const AValue: TNyxStateValue;
  AMaximumUTF8Bytes: Integer = 4194304): TNyxValuePresentation;

implementation

uses nyx.bytes, nyx.dates;

function NyxValueFormat: TNyxValueFormat;
begin
  Result := TNyxValueFormat.Canonical;
end;

function NyxFormatLocale: TNyxFormatLocale;
begin
  Result := TNyxFormatLocale.Invariant;
end;

function NyxFormatter(const AName: TNyxText): TNyxFormatterRef;
var
  LIndex: Integer;
  LScalar: Integer;
  LCount: Integer;
begin
  LIndex := 1;
  LCount := 0;
  while LIndex <= Length(AName) do
  begin

    if not NyxNextScalar(AName, LIndex, LScalar) or (LScalar < 32) or
      (LScalar = 127) then
    begin
      raise ENyxFormatting.Create('Formatter identity requires printable Unicode');
    end;
    Inc(LCount);
  end;

  if (LCount < 1) or (LCount > 128) then
  begin
    raise ENyxFormatting.Create('Formatter identity requires 1..128 Unicode scalars');
  end;
  Result.FName := AName;
end;

function ScalarData(const AValue: TNyxStateValue): TNyxDataValue;
begin
  case AValue.Kind of
    nskText: Result := NyxData(AValue.TextValue);
    nskBoolean: Result := NyxData(AValue.BooleanValue);
    nskInteger: Result := NyxData(AValue.IntegerValue);
    nskNumber: Result := NyxData(AValue.NumberValue);
  end;
end;

procedure Separator(const AText: TNyxText; AEmpty, ANumeric: Boolean);
var
  LIndex: Integer;
  LScalar: Integer;
begin

  if AEmpty and (AText = '') then
  begin
    Exit;
  end;
  LIndex := 1;

  if not NyxNextScalar(AText, LIndex, LScalar) or (LIndex <= Length(AText)) or
    (LScalar < 32) or (LScalar = 127) or (LScalar = 8232) or (LScalar = 8233) or
    ((LScalar >= Ord('0')) and (LScalar <= Ord('9'))) or
    (ANumeric and ((LScalar = Ord('+')) or (LScalar = Ord('-')) or
      (LScalar = Ord('e')) or (LScalar = Ord('E')))) then
  begin
    raise ENyxFormatting.Create('Format separator requires one nonnumeric printable scalar');
  end;
end;

class function TNyxFormatLocale.Invariant: TNyxFormatLocale;
begin
  Result := Default(TNyxFormatLocale);
  Result.FDefined := True;
  Result.FLocale := NyxDefaultLocale;
  Result.FDecimal := '.';
end;

procedure TNyxFormatLocale.Validate;
begin

  if not FDefined then
  begin
    raise ENyxFormatting.Create('Format locale must be explicitly initialized');
  end;

  if FLocale.Defined then
  begin
    NyxLocale(FLocale.Name);
  end;
  Separator(FDecimal, False, True);
  Separator(FGroup, True, True);

  if FDecimal = FGroup then
  begin
    raise ENyxFormatting.Create('Decimal and grouping separators must differ');
  end;
end;

function TNyxFormatLocale.Copy: TNyxFormatLocale;
begin
  Validate;
  Result := Self;
end;

function TNyxFormatLocale.Named(const ALocale: TNyxLocaleRef): TNyxFormatLocale;
begin
  Result := Copy;
  Result.FLocale := ALocale;
  Result.Validate;
end;

function TNyxFormatLocale.NumberSeparators(const ADecimal,
  AGroup: TNyxText): TNyxFormatLocale;
begin
  Result := Copy;
  Result.FDecimal := ADecimal;
  Result.FGroup := AGroup;
  Result.Validate;
end;

procedure RequireFormatShape(const AData: TNyxDataValue;
  ACount: Integer; const AKeys: TNyxText);
var
  LIndex: Integer;
begin
  AData.Validate;

  if (AData.Kind <> ndObject) or (AData.Count <> ACount) then
  begin
    raise ENyxFormatting.Create('Invalid display policy descriptor shape');
  end;
  for LIndex := 0 to AData.Count - 1 do
  begin

    if Pos('|' + AData.Key(LIndex) + '|', AKeys) = 0 then
    begin
      raise ENyxFormatting.Create('Unknown display policy descriptor field');
    end;
  end;
end;

function TNyxFormatLocale.ToData: TNyxDataValue;
begin
  Validate;
  Result := NyxObject([NyxField('name', NyxData(FLocale.Name)),
    NyxField('decimal', NyxData(FDecimal)), NyxField('group', NyxData(FGroup))]);
end;

class function TNyxFormatLocale.FromData(const AData: TNyxDataValue): TNyxFormatLocale;
var
  LName: TNyxText;
begin
  RequireFormatShape(AData, 3, '|name|decimal|group|');
  Result := Invariant;
  LName := AData.Field('name').AsText;

  if LName <> '' then
  begin
    Result := Result.Named(NyxLocale(LName));
  end;
  Result := Result.NumberSeparators(AData.Field('decimal').AsText,
    AData.Field('group').AsText);
end;

class function TNyxValueFormat.Canonical: TNyxValueFormat;
begin
  Result := Default(TNyxValueFormat);
  Result.FDefined := True;
  Result.FIdentity := NyxFormatter('nyx.canonical');
  Result.FRevision := 1;
  Result.FLocale := TNyxFormatLocale.Invariant;
  Result.FDateSeparator := '-';
end;

procedure TNyxValueFormat.Validate;
begin

  if not FDefined or (FRevision < 1) or (FRevision >= High(Integer)) or
    (Ord(FStyle) < Ord(Low(TNyxValueFormatStyle))) or
    (Ord(FStyle) > Ord(High(TNyxValueFormatStyle))) or
    (FScale < 0) or (FScale > 28) or
    (Ord(FRounding) < Ord(Low(TNyxDecimalRounding))) or
    (Ord(FRounding) > Ord(High(TNyxDecimalRounding))) or
    (Ord(FOrder) < Ord(Low(TNyxFormatDateOrder))) or
    (Ord(FOrder) > Ord(High(TNyxFormatDateOrder))) or
    (Ord(FTimePrecision) < Ord(Low(TNyxTimePrecision))) or
    (Ord(FTimePrecision) > Ord(High(TNyxTimePrecision))) then
  begin
    raise ENyxFormatting.Create('Formatter requires initialized known rules and a bounded revision');
  end;
  NyxFormatter(FIdentity.Name);
  FLocale.Validate;
  Separator(FDateSeparator, False, False);

  if (NyxUTF8ByteCount(FFirst) > 4096) or (NyxUTF8ByteCount(FSecond) > 4096) then
  begin
    raise ENyxFormatting.Create('Formatter text exceeds its explicit byte budget');
  end;
end;

function TNyxValueFormat.Copy: TNyxValueFormat;
begin
  Validate;
  Result := Self;
end;

function TNyxValueFormat.BeginStyle(AStyle: TNyxValueFormatStyle): TNyxValueFormat;
begin
  Validate;
  { Dormant rule parameters are not hidden policy state. Switching the style
    starts its parameters afresh while retaining explicit identity/locale. }
  Result := Canonical;
  Result.FIdentity := FIdentity;
  Result.FRevision := FRevision;
  Result.FLocale := FLocale.Copy;
  Result.FStyle := AStyle;
end;

procedure TNyxValueFormat.ValidateKind(AKind: TNyxStateKind);
begin
  Validate;

  if (Ord(AKind) < Ord(Low(TNyxStateKind))) or
    (Ord(AKind) > Ord(High(TNyxStateKind))) or
    ((FStyle = nvfsDecimal) and not (AKind in [nskInteger, nskNumber])) or
    ((FStyle in [nvfsCalendarDate, nvfsClockTime, nvfsTextAffixes]) and (AKind <> nskText)) or
    ((FStyle = nvfsBooleanLabels) and (AKind <> nskBoolean)) then
  begin
    raise ENyxFormatting.Create('Display policy differs from the declared scalar family');
  end;
end;

procedure TNyxValueFormat.ValidateDomain(AKind: TNyxStateKind;
  const ADomain: TNyxValueDomain);
begin
  ValidateKind(AKind);

  if (ADomain.Defined and (ADomain.Kind <> AKind)) or
    ((FStyle = nvfsCalendarDate) and not ADomain.CalendarDate) or
    ((FStyle = nvfsClockTime) and not ADomain.ClockTime) then
  begin
    raise ENyxFormatting.Create('Display policy requires the actual matching field domain');
  end;
end;

function TNyxValueFormat.ToData: TNyxDataValue;
const
  CStyles: array[TNyxValueFormatStyle] of TNyxText =
    ('canonical', 'decimal', 'calendar-date', 'clock-time', 'boolean-labels', 'text-affixes');
  CRounding: array[TNyxDecimalRounding] of TNyxText =
    ('exact', 'to-even', 'away-from-zero', 'toward-zero', 'floor', 'ceiling');
  COrders: array[TNyxFormatDateOrder] of TNyxText = ('year-month-day', 'day-month-year', 'month-day-year');
  CPrecision: array[TNyxTimePrecision] of TNyxText = ('minute', 'second', 'tenth', 'hundredth', 'millisecond');
var
  LFields: array of TNyxDataField;
begin
  Result := NyxNull;

  if not FDefined then
  begin
    Exit;
  end;
  Validate;
  SetLength(LFields, 5);
  LFields[0] := NyxField('version', NyxData(1));
  LFields[1] := NyxField('name', NyxData(FIdentity.Name));
  LFields[2] := NyxField('revision', NyxData(FRevision));
  LFields[3] := NyxField('locale', FLocale.ToData);
  LFields[4] := NyxField('style', NyxData(CStyles[FStyle]));
  case FStyle of
    nvfsCanonical: begin end;
    nvfsDecimal, nvfsCalendarDate, nvfsBooleanLabels, nvfsTextAffixes:
      begin
        SetLength(LFields, 7);
        case FStyle of
          nvfsDecimal:
            begin
              LFields[5] := NyxField('scale', NyxData(FScale));
              LFields[6] := NyxField('rounding', NyxData(CRounding[FRounding]));
            end;
          nvfsCalendarDate:
            begin
              LFields[5] := NyxField('order', NyxData(COrders[FOrder]));
              LFields[6] := NyxField('separator', NyxData(FDateSeparator));
            end;
          nvfsBooleanLabels:
            begin
              LFields[5] := NyxField('true', NyxData(FFirst));
              LFields[6] := NyxField('false', NyxData(FSecond));
            end;
          nvfsTextAffixes:
            begin
              LFields[5] := NyxField('prefix', NyxData(FFirst));
              LFields[6] := NyxField('suffix', NyxData(FSecond));
            end;
        else
          begin end;
        end;
      end;
    nvfsClockTime:
      begin
        SetLength(LFields, 6);
        LFields[5] := NyxField('precision', NyxData(CPrecision[FTimePrecision]));
      end;
  end;
  Result := NyxObject(LFields);
end;

class function TNyxValueFormat.FromData(const AData: TNyxDataValue): TNyxValueFormat;
const
  CBase = '|version|name|revision|locale|style|';
  CRounding: array[TNyxDecimalRounding] of TNyxText =
    ('exact', 'to-even', 'away-from-zero', 'toward-zero', 'floor', 'ceiling');
  COrders: array[TNyxFormatDateOrder] of TNyxText = ('year-month-day', 'day-month-year', 'month-day-year');
  CPrecision: array[TNyxTimePrecision] of TNyxText = ('minute', 'second', 'tenth', 'hundredth', 'millisecond');
var
  LStyle: TNyxText;
  LChoice: TNyxText;
  LRounding: TNyxDecimalRounding;
  LOrder: TNyxFormatDateOrder;
  LPrecision: TNyxTimePrecision;
  LFound: Boolean;
begin
  Result := Default(TNyxValueFormat);
  AData.Validate;

  if AData.Kind = ndNull then
  begin
    Exit;
  end;

  if (AData.Kind <> ndObject) or (AData.Field('version').AsInteger <> 1) then
  begin
    raise ENyxFormatting.Create('Unsupported display policy version');
  end;
  Result := Canonical;
  LStyle := AData.Field('style').AsText;

  if LStyle = 'canonical' then
  begin
    RequireFormatShape(AData, 5, CBase);
  end
  else if LStyle = 'decimal' then
  begin
    RequireFormatShape(AData, 7, CBase + 'scale|rounding|');
    LChoice := AData.Field('rounding').AsText;
    LFound := False;
    for LRounding := Low(TNyxDecimalRounding) to High(TNyxDecimalRounding) do
    begin

      if LChoice = CRounding[LRounding] then
      begin
        Result := Result.Decimal(AData.Field('scale').AsInteger, LRounding);
        LFound := True;
      end;
    end;

    if not LFound then
    begin
      raise ENyxFormatting.Create('Unknown display rounding choice');
    end;
  end
  else if LStyle = 'calendar-date' then
  begin
    RequireFormatShape(AData, 7, CBase + 'order|separator|');
    LChoice := AData.Field('order').AsText;
    LFound := False;
    for LOrder := Low(TNyxFormatDateOrder) to High(TNyxFormatDateOrder) do
    begin

      if LChoice = COrders[LOrder] then
      begin
        Result := Result.CalendarDate(LOrder, AData.Field('separator').AsText);
        LFound := True;
      end;
    end;

    if not LFound then
    begin
      raise ENyxFormatting.Create('Unknown display calendar order');
    end;
  end
  else if LStyle = 'clock-time' then
  begin
    RequireFormatShape(AData, 6, CBase + 'precision|');
    LChoice := AData.Field('precision').AsText;
    LFound := False;
    for LPrecision := Low(TNyxTimePrecision) to High(TNyxTimePrecision) do
    begin

      if LChoice = CPrecision[LPrecision] then
      begin
        Result := Result.ClockTime(LPrecision);
        LFound := True;
      end;
    end;

    if not LFound then
    begin
      raise ENyxFormatting.Create('Unknown display clock precision');
    end;
  end
  else if LStyle = 'boolean-labels' then
  begin
    RequireFormatShape(AData, 7, CBase + 'true|false|');
    Result := Result.BooleanLabels(AData.Field('true').AsText, AData.Field('false').AsText);
  end
  else if LStyle = 'text-affixes' then
  begin
    RequireFormatShape(AData, 7, CBase + 'prefix|suffix|');
    Result := Result.TextAffixes(AData.Field('prefix').AsText, AData.Field('suffix').AsText);
  end
  else
  begin
    raise ENyxFormatting.Create('Unknown display policy style');
  end;
  Result := Result.Identified(NyxFormatter(AData.Field('name').AsText),
    AData.Field('revision').AsInteger).Localized(TNyxFormatLocale.FromData(AData.Field('locale')));
end;

function TNyxValueFormat.Identified(const AIdentity: TNyxFormatterRef;
  ARevision: Integer): TNyxValueFormat;
begin
  Result := Copy;
  Result.FIdentity := AIdentity;
  Result.FRevision := ARevision;
  Result.Validate;
end;

function TNyxValueFormat.Localized(const ALocale: TNyxFormatLocale): TNyxValueFormat;
begin
  Result := Copy;
  Result.FLocale := ALocale.Copy;
end;

function TNyxValueFormat.Decimal(AScale: Integer;
  ARounding: TNyxDecimalRounding): TNyxValueFormat;
begin
  Result := BeginStyle(nvfsDecimal);
  Result.FScale := AScale;
  Result.FRounding := ARounding;
  Result.Validate;
end;

function TNyxValueFormat.CalendarDate(AOrder: TNyxFormatDateOrder;
  const ASeparator: TNyxText): TNyxValueFormat;
begin
  Result := BeginStyle(nvfsCalendarDate);
  Result.FOrder := AOrder;
  Result.FDateSeparator := ASeparator;
  Result.Validate;
end;

function TNyxValueFormat.ClockTime(APrecision: TNyxTimePrecision): TNyxValueFormat;
begin
  Result := BeginStyle(nvfsClockTime);
  Result.FTimePrecision := APrecision;
  Result.Validate;
end;

function TNyxValueFormat.BooleanLabels(const ATrue, AFalse: TNyxText): TNyxValueFormat;
begin
  Result := BeginStyle(nvfsBooleanLabels);
  Result.FFirst := ATrue;
  Result.FSecond := AFalse;
  Result.Validate;
end;

function TNyxValueFormat.TextAffixes(const APrefix, ASuffix: TNyxText): TNyxValueFormat;
begin
  Result := BeginStyle(nvfsTextAffixes);
  Result.FFirst := APrefix;
  Result.FSecond := ASuffix;
  Result.Validate;
end;

function AdmittedScalarText(const AValue: TNyxStateValue): TNyxText;
begin
  case AValue.Kind of
    nskText: Result := AValue.TextValue;
    nskInteger: Result := TNyxText(IntToStr(AValue.IntegerValue));
    nskNumber: Result := AValue.NumberText;
    nskBoolean:
      begin
        Result := 'false';

        if AValue.BooleanValue then
        begin
          Result := 'true';
        end;
      end;
  end;
end;

function NyxRawScalarText(const AValue: TNyxStateValue): TNyxText;
begin
  AValue.Validate;
  Result := AdmittedScalarText(AValue);
end;

function NyxCanonicalPresentation(const AValue: TNyxStateValue;
  AMaximumUTF8Bytes: Integer): TNyxValuePresentation;
const
  CUnavailable: TNyxText = '[Display unavailable]';
begin

  if (AMaximumUTF8Bytes < NyxUTF8ByteCount(CUnavailable)) or
    (AMaximumUTF8Bytes > 4194304) then
  begin
    raise ENyxFormatting.Create('Presentation needs space for its explicit refusal marker');
  end;
  Result.FOutcome := nvpoCanonical;
  Result.FRaw := AdmittedScalarText(AValue);
  Result.FText := Result.FRaw;
  Result.FDiagnostic := '';

  if NyxUTF8ByteCount(Result.FText) > AMaximumUTF8Bytes then
  begin
    Result.FOutcome := nvpoUnavailable;
    Result.FText := CUnavailable;
    Result.FDiagnostic := 'Canonical text exceeds its presentation UTF-8 byte budget';
  end;
end;

function Padded(AValue, AWidth: Integer): TNyxText;
begin
  Result := TNyxText(IntToStr(AValue));
  while Length(Result) < AWidth do
  begin
    Result := '0' + Result;
  end;
end;

function DecimalText(const AValue: TNyxStateValue; AScale: Integer;
  ARounding: TNyxDecimalRounding; const ALocale: TNyxFormatLocale): TNyxText;
var
  LNumber: TNyxDecimalNumber;
  LText: TNyxText;
  LWhole: TNyxText;
  LFraction: TNyxText;
  LSign: TNyxText;
  LIndex: Integer;
begin

  if not (AValue.Kind in [nskInteger, nskNumber]) then
  begin
    raise ENyxFormatting.Create('Decimal presentation requires an accepted numeric scalar');
  end;
  { Enter the existing decimal domain exactly; only the requested display
    quantization may round. This avoids accidental double-rounding. }
  LNumber := TNyxDecimalNumber.FromDecimal(NyxDecimal(NyxRawScalarText(AValue)), ndrExact);
  LText := LNumber.Quantize(AScale, ARounding).ToText;
  LSign := '';

  if LText[1] = '-' then
  begin
    LSign := '-';
    Delete(LText, 1, 1);
  end;
  LIndex := Pos('.', LText);
  LWhole := LText;
  LFraction := '';

  if LIndex > 0 then
  begin
    LWhole := Copy(LText, 1, LIndex - 1);
    LFraction := Copy(LText, LIndex + 1, Length(LText));
  end;
  Result := '';
  for LIndex := 1 to Length(LWhole) do
  begin

    if (LIndex > 1) and ((Length(LWhole) - LIndex + 1) mod 3 = 0) then
    begin
      Result := Result + ALocale.GroupSeparator;
    end;
    Result := Result + LWhole[LIndex];
  end;
  while Length(LFraction) < AScale do
  begin
    LFraction := LFraction + '0';
  end;

  if AScale > 0 then
  begin
    Result := Result + ALocale.DecimalSeparator + LFraction;
  end;
  Result := LSign + Result;
end;

function TNyxValueFormat.FormatValue(const AValue: TNyxStateValue;
  const ADomain: TNyxValueDomain; AMaximumUTF8Bytes: Integer): TNyxText;
var
  LDate: TNyxCalendarDate;
  LTime: TNyxClockTime;
  LIndex: Integer;
begin
  Validate;
  AValue.Validate;
  ValidateDomain(AValue.Kind, ADomain);

  if (AMaximumUTF8Bytes < 1) or (AMaximumUTF8Bytes > 4194304) then
  begin
    raise ENyxFormatting.Create('Formatted output needs a bounded positive UTF-8 budget');
  end;

  if ADomain.Defined then
  begin
    ADomain.Admit(ScalarData(AValue));
  end;
  case FStyle of
    nvfsCanonical: Result := NyxRawScalarText(AValue);
    nvfsDecimal: Result := DecimalText(AValue, FScale, FRounding, FLocale);
    nvfsCalendarDate:
      begin

        if (AValue.Kind <> nskText) or not ADomain.CalendarDate then
        begin
          raise ENyxFormatting.Create('Date presentation requires the actual calendar text domain');
        end;
        LDate := TNyxCalendarDate.FromText(AValue.TextValue);
        Result := '';

        if LDate.Defined then
        begin
          case FOrder of
            nfdoYearMonthDay: Result := Padded(LDate.Year, 4) + FDateSeparator +
              Padded(LDate.Month, 2) + FDateSeparator + Padded(LDate.Day, 2);
            nfdoDayMonthYear: Result := Padded(LDate.Day, 2) + FDateSeparator +
              Padded(LDate.Month, 2) + FDateSeparator + Padded(LDate.Year, 4);
            nfdoMonthDayYear: Result := Padded(LDate.Month, 2) + FDateSeparator +
              Padded(LDate.Day, 2) + FDateSeparator + Padded(LDate.Year, 4);
          end;
        end;
      end;
    nvfsClockTime:
      begin

        if (AValue.Kind <> nskText) or not ADomain.ClockTime then
        begin
          raise ENyxFormatting.Create('Clock presentation requires the actual clock text domain');
        end;
        LTime := TNyxClockTime.FromText(AValue.TextValue);
        Result := '';

        if LTime.Defined then
        begin
          Result := LTime.WithPrecision(FTimePrecision).ToText;
          LIndex := Pos('.', Result);

          if LIndex > 0 then
          begin
            Result := System.Copy(Result, 1, LIndex - 1) + FLocale.DecimalSeparator +
              System.Copy(Result, LIndex + 1, Length(Result));
          end;
        end;
      end;
    nvfsBooleanLabels:
      begin

        if AValue.Kind <> nskBoolean then
        begin
          raise ENyxFormatting.Create('Boolean presentation never coerces another scalar family');
        end;
        Result := FSecond;

        if AValue.BooleanValue then
        begin
          Result := FFirst;
        end;
      end;
    nvfsTextAffixes:
      begin

        if AValue.Kind <> nskText then
        begin
          raise ENyxFormatting.Create('Text presentation never coerces another scalar family');
        end;
        Result := FFirst + AValue.TextValue + FSecond;
      end;
  end;

  if NyxUTF8ByteCount(Result) > AMaximumUTF8Bytes then
  begin
    raise ENyxFormatting.Create('Formatted output exceeds its UTF-8 byte budget');
  end;
end;

function TNyxValueFormat.PresentValue(const AValue: TNyxStateValue;
  const ADomain: TNyxValueDomain; AMaximumUTF8Bytes: Integer): TNyxValuePresentation;
const
  CUnavailable: TNyxText = '[Display unavailable]';
var
  LIndex: Integer;
  LStart: Integer;
  LScalar: Integer;
  LBytes: Integer;
  LNextBytes: Integer;
begin
  ValidateDomain(AValue.Kind, ADomain);
  AValue.Validate;

  if (AMaximumUTF8Bytes < NyxUTF8ByteCount(CUnavailable)) or
    (AMaximumUTF8Bytes > 4194304) then
  begin
    raise ENyxFormatting.Create('Presentation needs space for its explicit refusal marker');
  end;

  if ADomain.Defined then
  begin
    { An invalid accepted value/schema must still fail ordinary admission; it
      is not a display limitation and cannot be hidden behind a paint marker. }
    ADomain.Admit(ScalarData(AValue));
  end;
  Result.FRaw := NyxRawScalarText(AValue);
  Result.FDiagnostic := '';
  Result.FOutcome := nvpoFormatted;

  if FStyle = nvfsCanonical then
  begin
    Result.FOutcome := nvpoCanonical;
  end;
  try
    Result.FText := FormatValue(AValue, ADomain, AMaximumUTF8Bytes);
  except
    on E: ENyxDecimalNumber do
    begin
      Result.FOutcome := nvpoUnavailable;
      Result.FDiagnostic := TNyxText(E.Message);
    end;
    on E: ENyxTimeValue do
    begin
      Result.FOutcome := nvpoUnavailable;
      Result.FDiagnostic := TNyxText(E.Message);
    end;
    on E: ENyxFormatting do
    begin
      Result.FOutcome := nvpoUnavailable;
      Result.FDiagnostic := TNyxText(E.Message);
    end;
  end;

  if Result.FOutcome = nvpoUnavailable then
  begin
    { Diagnostics are a separate bounded status, not unbounded user data added
      to paint. Keep only complete Unicode scalars within one KiB. }
    LIndex := 1;
    LStart := 1;
    LBytes := 0;
    while NyxNextScalar(Result.FDiagnostic, LIndex, LScalar) do
    begin
      LNextBytes := NyxUTF8ByteCount(NyxScalarText(LScalar));

      if LNextBytes > 1024 - LBytes then
      begin
        Break;
      end;
      Inc(LBytes, LNextBytes);
      LStart := LIndex;
    end;
    Result.FDiagnostic := System.Copy(Result.FDiagnostic, 1, LStart - 1);

    if Result.FDiagnostic = '' then
    begin
      Result.FDiagnostic := 'The selected display policy cannot represent this accepted value';
    end;
    Result.FText := CUnavailable;

    if NyxUTF8ByteCount(Result.FRaw) <= AMaximumUTF8Bytes -
      NyxUTF8ByteCount(CUnavailable) - 1 then
    begin
      Result.FText := CUnavailable + ' ' + Result.FRaw;
    end;
  end;
end;

end.
