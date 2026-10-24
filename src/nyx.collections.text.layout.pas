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
unit nyx.collections.text.layout;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.collections.presentation;

type
  { Synchronous borrowed metric receiver for the exact admitted display font.
    It returns a finite nonnegative extent in the same units as the clip width.
    It must not mutate source/controls or retire its adapter. Hosts retain their
    input/frame guards throughout layout. Exceptions propagate unchanged; this
    boundary never interprets an arbitrary callback error as budget refusal. }
  TNyxTableTextMeasure = function(const AText: TNyxText): Double of object;

  { Complete means every display scalar fits the chosen soft/hard lines. Clip
    and Ellipsis are honest visual overflow results; Unavailable separately
    reports operational admission refusal. None is a formatted-copy receipt. }
  TNyxTableTextLayoutOutcome = (ntloComplete, ntloClipped, ntloEllipsized,
    ntloUnavailable);

  { Owner-free measured display lines. A line may include a visual ellipsis;
    it is never substituted for accepted RAW, explicit formatted output or a
    draft. Copy detaches its private vector, and no metric receiver is retained.
    UTF-8/UTF-16 scalar boundaries, combining marks, emoji joiners/modifiers and
    regional-indicator pairs are kept together. Soft breaks prefer ASCII space
    or tab, then split an unspaced path; this is not a full Unicode grapheme or
    linguistic line-breaking implementation. }
  TNyxTableTextLayout = record
  private
    FLines: array of TNyxText;
    FOutcome: TNyxTableTextLayoutOutcome;
    FAdmission: TNyxTableTextAdmission;
    FDiagnostic: TNyxText;
    FMeasurements: Integer;
    function GetCount: Integer;
  public
    function Copy: TNyxTableTextLayout;
    function LineAt(AIndex: Integer): TNyxText;
    property Count: Integer read GetCount;
    property Outcome: TNyxTableTextLayoutOutcome read FOutcome;
    property Admission: TNyxTableTextAdmission read FAdmission;
    property Diagnostic: TNyxText read FDiagnostic;
    property Measurements: Integer read FMeasurements;
  end;

  { Detached full-text metric for Fit. It measures every admitted hard line,
    irrespective of soft wrapping or a clamped visual tail. A refusal has no
    usable width and never authorizes applying an approximate column width. }
  TNyxTableTextNaturalWidth = record
    Width: Double;
    Admission: TNyxTableTextAdmission;
    Diagnostic: TNyxText;
    Measurements: Integer;
  end;

{ Width is the resolved content clip, excluding cell insets. Hard CR/LF/CRLF
  delimiters become visible line boundaries without changing the original text.
  The final clamped Wrap line is ellipsized; Clip/Ellipsis use only one line.
  Callers keep complete accessible/inspection text independently of this result. }
function LayoutNyxTableText(const AText: TNyxText;
  const APolicy: TNyxColumnTextPolicy; AWidth: Double;
  const ALimits: TNyxTableTextWorkLimits;
  AMeasure: TNyxTableTextMeasure): TNyxTableTextLayout;

function MeasureNaturalNyxTableText(const AText: TNyxText;
  const APolicy: TNyxColumnTextPolicy; const ALimits: TNyxTableTextWorkLimits;
  AMeasure: TNyxTableTextMeasure): TNyxTableTextNaturalWidth;

{ Explicit host/pass refusal, separate from formatting and accepted values.
  Callers must supply a real non-ready reason and nonempty diagnostic. }
function UnavailableNyxTableTextLayout(AAdmission: TNyxTableTextAdmission;
  const ADiagnostic: TNyxText): TNyxTableTextLayout;

implementation

uses Math, nyx.unicode, nyx.unicode.types;

function UnavailableNyxTableTextLayout(AAdmission: TNyxTableTextAdmission;
  const ADiagnostic: TNyxText): TNyxTableTextLayout;
var
  LLayout: TNyxTableTextLayout;
begin

  if (Ord(AAdmission) <= Ord(nttaReady)) or
    (Ord(AAdmission) > Ord(High(TNyxTableTextAdmission))) or (ADiagnostic = '') then
  begin
    raise EArgumentException.Create('Layout refusal requires a real reason and diagnostic');
  end;
  LLayout := Default(TNyxTableTextLayout);
  SetLength(LLayout.FLines, 1);
  LLayout.FLines[0] := '[Layout unavailable]';
  LLayout.FOutcome := ntloUnavailable;
  LLayout.FAdmission := AAdmission;
  LLayout.FDiagnostic := ADiagnostic;
  Result := LLayout;
end;

function MeasureNaturalNyxTableText(const AText: TNyxText;
  const APolicy: TNyxColumnTextPolicy; const ALimits: TNyxTableTextWorkLimits;
  AMeasure: TNyxTableTextMeasure): TNyxTableTextNaturalWidth;
var
  LValue: TNyxTableTextNaturalWidth;
  LStart: Integer;
  LAt: Integer;
  LAfter: Integer;
  LScalar: Integer;
  LWidth: Double;
begin
  APolicy.Validate;
  ALimits.Validate;

  if not Assigned(AMeasure) then
  begin
    raise EArgumentException.Create('Natural text measurement requires an actual metric receiver');
  end;
  LValue := Default(TNyxTableTextNaturalWidth);
  LValue.Admission := ALimits.Admit(APolicy, AText);

  if LValue.Admission <> nttaReady then
  begin
    LValue.Diagnostic := 'Full display text exceeds this host text-layout budget';
    Exit(LValue);
  end;
  LStart := 1;
  LAt := 1;
  repeat
    while LAt <= Length(AText) do
    begin
      LAfter := LAt;
      NyxNextScalar(AText, LAfter, LScalar);

      if LScalar in [10, 13] then
      begin
        Break;
      end;
      LAt := LAfter;
    end;

    if LValue.Measurements >= ALimits.MaximumMeasurements then
    begin
      LValue.Admission := nttaMeasurementLimit;
      LValue.Diagnostic := 'Full hard-line measurement exceeds this host metric-call budget';
      LValue.Width := 0;
      Exit(LValue);
    end;
    Inc(LValue.Measurements);
    LWidth := AMeasure(System.Copy(AText, LStart, LAt - LStart));

    if IsNan(LWidth) or IsInfinite(LWidth) or (LWidth < 0) then
    begin
      raise EArgumentException.Create('Natural text metric must be finite and nonnegative');
    end;
    LValue.Width := Max(LValue.Width, LWidth);

    if LAt > Length(AText) then
    begin
      Break;
    end;
    LAt := LAfter;

    if (LScalar = 13) and (LAt <= Length(AText)) and (AText[LAt] = #10) then
    begin
      Inc(LAt);
    end;
    LStart := LAt;
  until False;
  Result := LValue;
end;

function TNyxTableTextLayout.GetCount: Integer;
begin
  Result := Length(FLines);
end;

function TNyxTableTextLayout.LineAt(AIndex: Integer): TNyxText;
begin

  if (AIndex < 0) or (AIndex >= Length(FLines)) then
  begin
    raise EArgumentException.Create('Text layout line is outside its captured vector');
  end;
  Result := FLines[AIndex];
end;

function TNyxTableTextLayout.Copy: TNyxTableTextLayout;
var
  LLayout: TNyxTableTextLayout;
  I: Integer;
begin
  LLayout := Self;
  LLayout.FLines := nil;
  SetLength(LLayout.FLines, Length(FLines));
  for I := 0 to High(FLines) do
  begin
    LLayout.FLines[I] := FLines[I];
  end;
  Result := LLayout;
end;

function LayoutNyxTableText(const AText: TNyxText;
  const APolicy: TNyxColumnTextPolicy; AWidth: Double;
  const ALimits: TNyxTableTextWorkLimits;
  AMeasure: TNyxTableTextMeasure): TNyxTableTextLayout;
const
  CEllipsis: TNyxText = '…';
  CUnavailable: TNyxText = '[Layout unavailable]';
var
  LLayout: TNyxTableTextLayout;
  LStarts: array of Integer;
  LScalars: array of Integer;
  LIndex: Integer;
  LStart: Integer;
  LCount: Integer;
  LPosition: Integer;
  LEnd: Integer;
  LFit: Integer;
  LNext: Integer;
  I: Integer;
  LScalar: Integer;
  LFullWidth: Double;
  LMarkerWidth: Double;
  LExhausted: Boolean;
  LHardTail: Boolean;
  LSoftTail: Boolean;
  LLast: Boolean;

  function BreakBlocked(AFirst, AAfter: Integer): Boolean;
  var
    LPrevious: Integer;
    LFollowing: Integer;
    LAt: Integer;
    LRegional: Integer;
  begin
    Result := False;

    if (AAfter <= AFirst) or (AAfter >= LCount) then
    begin
      Exit;
    end;
    LPrevious := LScalars[AAfter - 1];
    LFollowing := LScalars[AAfter];
    Result := (NyxScalarCategory(LFollowing) in
      [nucNonspacingMark, nucSpacingMark, nucEnclosingMark]) or
      (LFollowing = $200D) or (LPrevious = $200D) or
      ((LFollowing >= $1F3FB) and (LFollowing <= $1F3FF));

    if not Result and (LPrevious >= $1F1E6) and (LPrevious <= $1F1FF) and
      (LFollowing >= $1F1E6) and (LFollowing <= $1F1FF) then
    begin
      LAt := AAfter - 1;
      LRegional := 0;
      while (LAt >= AFirst) and (LScalars[LAt] >= $1F1E6) and (LScalars[LAt] <= $1F1FF) do
      begin
        Inc(LRegional);
        Dec(LAt);
      end;
      Result := Odd(LRegional);
    end;
  end;

  function SafeBreak(AFirst, AAfter: Integer): Integer;
  begin
    Result := AAfter;
    while BreakBlocked(AFirst, Result) do
    begin
      Dec(Result);
    end;
  end;

  function TextBetween(AFirst, AAfter: Integer): TNyxText;
  begin
    Result := System.Copy(AText, LStarts[AFirst], LStarts[AAfter] - LStarts[AFirst]);
  end;

  function Measure(const AValue: TNyxText): Double;
  begin

    if LExhausted then
    begin
      Exit(0);
    end;

    if LLayout.FMeasurements >= ALimits.MaximumMeasurements then
    begin
      LExhausted := True;
      Exit(0);
    end;
    Inc(LLayout.FMeasurements);
    Result := AMeasure(AValue);

    if IsNan(Result) or IsInfinite(Result) or (Result < 0) then
    begin
      raise EArgumentException.Create('Text metric must be finite and nonnegative');
    end;
  end;

  function Fit(AFirst, AAfter: Integer; AAvailable: Double): Integer;
  var
    LLow: Integer;
    LHigh: Integer;
    LMiddle: Integer;
  begin
    LLow := AFirst;
    LHigh := AAfter;
    while (LLow < LHigh) and not LExhausted do
    begin
      LMiddle := LLow + (LHigh - LLow + 1) div 2;

      if Measure(TextBetween(AFirst, LMiddle)) <= AAvailable then
      begin
        LLow := LMiddle;
      end
      else
      begin
        LHigh := LMiddle - 1;
      end;
    end;
    Result := SafeBreak(AFirst, LLow);
    { Removing part of a shaping/combining sequence can make a shorter prefix
      wider. The search is only a candidate; admit the exact safe string that
      will be drawn, within the same bounded metric-call allowance. }
    while (Result > AFirst) and not LExhausted and
      (Measure(TextBetween(AFirst, Result)) > AAvailable) do
    begin
      Result := SafeBreak(AFirst, Result - 1);
    end;
  end;

  procedure Append(const AValue: TNyxText);
  var
    LLine: Integer;
  begin
    LLine := Length(LLayout.FLines);
    SetLength(LLayout.FLines, LLine + 1);
    LLayout.FLines[LLine] := AValue;
  end;

  procedure Unavailable(AAdmission: TNyxTableTextAdmission);
  begin
    LLayout.FLines := nil;
    Append(CUnavailable);
    LLayout.FOutcome := ntloUnavailable;
    LLayout.FAdmission := AAdmission;
    case AAdmission of
      nttaReady:
        begin
          raise EArgumentException.Create('Ready text cannot report operational refusal');
        end;
      nttaLineLimit:
        begin
          LLayout.FDiagnostic := 'Authored line count exceeds this host measurement budget';
        end;
      nttaTextLimit:
        begin
          LLayout.FDiagnostic := 'Display text exceeds this host UTF-8 measurement budget';
        end;
      nttaMeasurementLimit:
        begin
          LLayout.FDiagnostic := 'Text metric work exceeds this host call budget';
        end;
      nttaCellLimit:
        begin
          LLayout.FDiagnostic := 'Visible cells exceed this host measurement pass budget';
        end;
      nttaHostMetrics:
        begin
          LLayout.FDiagnostic := 'This host requires a resolved metric for the selected font style';
        end;
    end;
  end;

  procedure Ellipsize(AFirst, AAfter: Integer);
  var
    LThrough: Integer;
  begin
    LMarkerWidth := Measure(CEllipsis);

    if LMarkerWidth <= AWidth then
    begin
      LThrough := Fit(AFirst, AAfter, Max(0, AWidth - LMarkerWidth));
      { Font shaping may kern the marker and prefix together. Verify the exact
        drawn string rather than treating two independent extents as additive. }
      while (LThrough > AFirst) and not LExhausted and
        (Measure(TextBetween(AFirst, LThrough) + CEllipsis) > AWidth) do
      begin
        LThrough := SafeBreak(AFirst, LThrough - 1);
      end;
      Append(TextBetween(AFirst, LThrough) + CEllipsis);
      LLayout.FOutcome := ntloEllipsized;
    end
    else
    begin
      Append('');
      LLayout.FOutcome := ntloClipped;
    end;
  end;

begin
  APolicy.Validate;
  ALimits.Validate;

  if IsNan(AWidth) or IsInfinite(AWidth) or (AWidth < 0) or not Assigned(AMeasure) then
  begin
    raise EArgumentException.Create('Text layout requires a finite clip and actual metric receiver');
  end;
  LLayout := Default(TNyxTableTextLayout);
  LLayout.FAdmission := ALimits.Admit(APolicy, AText);

  if LLayout.FAdmission <> nttaReady then
  begin
    Unavailable(LLayout.FAdmission);
    Exit(LLayout);
  end;
  { Length is a safe upper bound on scalar count on both hosts. The admitted
    byte bound limits this detached indexing work before any metric call. }
  SetLength(LStarts, Length(AText) + 1);
  SetLength(LScalars, Length(AText));
  LIndex := 1;
  LCount := 0;
  while LIndex <= Length(AText) do
  begin
    LStart := LIndex;
    NyxNextScalar(AText, LIndex, LScalar);
    LStarts[LCount] := LStart;
    LScalars[LCount] := LScalar;
    Inc(LCount);
  end;
  LStarts[LCount] := Length(AText) + 1;
  SetLength(LStarts, LCount + 1);
  SetLength(LScalars, LCount);
  LPosition := 0;
  LExhausted := False;
  repeat
    LEnd := LPosition;
    while (LEnd < LCount) and not (LScalars[LEnd] in [10, 13]) do
    begin
      Inc(LEnd);
    end;
    LHardTail := LEnd < LCount;
    LFullWidth := Measure(TextBetween(LPosition, LEnd));
    LSoftTail := LFullWidth > AWidth;
    LLast := Length(LLayout.FLines) = APolicy.MaximumLines - 1;

    if (APolicy.Overflow = nctoEllipsis) or
      ((APolicy.Overflow = nctoWrap) and LLast) then
    begin

      if LHardTail or LSoftTail then
      begin
        Ellipsize(LPosition, LEnd);
      end
      else
      begin
        Append(TextBetween(LPosition, LEnd));
      end;
      Break;
    end;

    if APolicy.Overflow = nctoClip then
    begin
      LFit := LEnd;

      if LSoftTail then
      begin
        LFit := Fit(LPosition, LEnd, AWidth);
      end;
      Append(TextBetween(LPosition, LFit));

      if LSoftTail or LHardTail then
      begin
        LLayout.FOutcome := ntloClipped;
      end;
      Break;
    end;
    LFit := LEnd;

    if LSoftTail then
    begin
      LFit := Fit(LPosition, LEnd, AWidth);

      if LFit = LPosition then
      begin
        { A cluster wider than the clip still consumes a complete sequence. The
          target clips its physical rectangle; no partial scalar/joiner appears. }
        Inc(LFit);
        while (LFit < LEnd) and BreakBlocked(LPosition, LFit) do
        begin
          Inc(LFit);
        end;
        LLayout.FOutcome := ntloClipped;
      end;
      LNext := LFit;
      for I := LFit - 1 downto LPosition do
      begin

        if LScalars[I] in [9, 32] then
        begin
          LNext := I + 1;
          Break;
        end;
      end;
      { The preferred word break is another shaped string. Retain the already
        admitted character break if that shorter prefix itself does not fit. }

      if (LNext = LFit) or (Measure(TextBetween(LPosition, LNext)) <= AWidth) then
      begin
        LFit := LNext;
      end;
    end;
    Append(TextBetween(LPosition, LFit));
    LPosition := LFit;

    if LExhausted then
    begin
      Break;
    end;

    if LPosition < LEnd then
    begin
      Continue;
    end;

    if not LHardTail then
    begin
      Break;
    end;
    LPosition := LEnd + 1;

    if (LScalars[LEnd] = 13) and (LPosition < LCount) and (LScalars[LPosition] = 10) then
    begin
      Inc(LPosition);
    end;
  until False;

  if LExhausted then
  begin
    Unavailable(nttaMeasurementLimit);
  end;
  Result := LLayout;
end;

end.
