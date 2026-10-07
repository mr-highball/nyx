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

unit nyx.sliders;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, Math, nyx.text, nyx.state, nyx.data, nyx.contract;

type
  { Immutable numeric scale, independently copied from its declared domain.
    It retains no node, store, adapter or caller array. Positions are private
    ordinal control coordinates; applications observe numeric values instead.
    Exact sorted choices have one tick per choice. Integer ranges up to one
    million ticks use unit increments; larger ranges and Numbers use the
    requested resolution. Endpoints remain exact, including a single value.
    Invalid families, bounds, values or positions raise ENyxContract. }
  TNyxSliderScale = record
  private
    FDomain: TNyxValueDomain;
    FMinimum: Double;
    FMaximum: Double;
    FMaximumPosition: Integer;
    FChoices: array of TNyxDataValue;
    function Fraction(AValue: Double): Double;
  public
    class function Create(const ADomain: TNyxValueDomain;
      AMinimum: Integer = 0; AMaximum: Integer = 100;
      AIntervals: Integer = 1000): TNyxSliderScale; static;
    { Wire reads preserve exact accepted spelling in the mounted value cache.
      Off-tick programmatic values need not be quantized into physical ticks. }
    function PositionOf(const AValue: TNyxText): Integer;
    function ValueAt(APosition: Integer): TNyxText;
    property Minimum: Double read FMinimum;
    property Maximum: Double read FMaximum;
    property MaximumPosition: Integer read FMaximumPosition;
  end;

  { Mount-owned accepted value cache. Accept validates a complete candidate
    before replacing its baseline; refusal leaves both old scale and value.
    An unchanged thumb reports the original wire spelling, even off-tick.
    Moving away emits the selected numeric value; only subsequent accepted
    publication changes the baseline. No renderer or UI lifetime is retained. }
  TNyxSliderValue = record
  private
    FScale: TNyxSliderScale;
    FAccepted: TNyxText;
    FPosition: Integer;
    FDefined: Boolean;
  public
    procedure Accept(const AScale: TNyxSliderScale; const AValue: TNyxText);
    function ReadPosition(APosition: Integer): TNyxText;
    property Position: Integer read FPosition;
  end;

implementation

function HasMember(const AData: TNyxDataValue; const AName: TNyxText): Boolean;
var
  LIndex: Integer;
begin
  Result := False;
  for LIndex := 0 to AData.Count - 1 do
  begin

    if AData.Key(LIndex) = AName then
    begin
      Exit(True);
    end;
  end;
end;

class function TNyxSliderScale.Create(const ADomain: TNyxValueDomain;
  AMinimum, AMaximum, AIntervals: Integer): TNyxSliderScale;
var
  LData: TNyxDataValue;
  LChoices: TNyxDataValue;
  LItem: TNyxDataValue;
  LIndex: Integer;
  LOther: Integer;
  LSpan: Double;
begin
  ADomain.Validate;

  if not (ADomain.Kind in [nskInteger, nskNumber]) then
  begin
    raise ENyxContract.Create('A slider requires an Integer or Number domain');
  end;

  if (AIntervals < 1) or (AIntervals > 1000000) then
  begin
    raise ENyxContract.Create('Slider intervals require 1..1000000');
  end;
  Result.FDomain := ADomain.Copy;
  Result.FMinimum := AMinimum;
  Result.FMaximum := AMaximum;
  Result.FMaximumPosition := 0;
  SetLength(Result.FChoices, 0);
  LData := ADomain.ToData;

  if HasMember(LData, 'min') then
  begin
    Result.FMinimum := LData.Field('min').AsNumber;
    Result.FMaximum := LData.Field('max').AsNumber;
  end;

  if HasMember(LData, 'choices') then
  begin
    LChoices := LData.Field('choices');
    SetLength(Result.FChoices, LChoices.Count);
    for LIndex := 0 to LChoices.Count - 1 do
    begin
      Result.FChoices[LIndex] := LChoices.Item(LIndex).Copy;
    end;
    { Sort detached records, never mutate authored order or its snapshots. }
    for LIndex := 1 to High(Result.FChoices) do
    begin
      LItem := Result.FChoices[LIndex].Copy;
      LOther := LIndex;
      while (LOther > 0) do
      begin

        if Result.FChoices[LOther - 1].AsNumber <= LItem.AsNumber then
        begin
          Break;
        end;
        Result.FChoices[LOther] := Result.FChoices[LOther - 1].Copy;
        Dec(LOther);
      end;
      Result.FChoices[LOther] := LItem.Copy;
    end;
    Result.FMaximumPosition := High(Result.FChoices);
    Result.FMinimum := Result.FChoices[0].AsNumber;
    Result.FMaximum := Result.FChoices[Result.FMaximumPosition].AsNumber;
    Exit;
  end;

  if Result.FMinimum > Result.FMaximum then
  begin
    raise ENyxContract.Create('Slider minimum exceeds maximum');
  end;

  if Result.FMinimum = Result.FMaximum then
  begin
    Exit;
  end;
  Result.FMaximumPosition := AIntervals;

  if ADomain.Kind = nskInteger then
  begin
    { Double exactly holds the complete signed 32-bit difference; subtracting
      in Integer would overflow. Large spans use bounded ordinal ticks. }
    LSpan := Result.FMaximum - Result.FMinimum;

    if LSpan <= 1000000 then
    begin
      Result.FMaximumPosition := Round(LSpan);
    end;
  end;
end;

function TNyxSliderScale.Fraction(AValue: Double): Double;
const
  { Conservative shared IEEE Double threshold. RTL MaxDouble constants differ
    between native and pas2js; only large opposite bounds need halving. }
  CHalfThreshold: Double = 8e307;
var
  LMinimum: Double;
  LMaximum: Double;
  LValue: Double;
  LNumerator: Double;
  LDenominator: Double;
begin
  { Halving before subtraction avoids overflow across opposite finite bounds.
    Same-sign subtraction retains tiny ranges without half underflow. }

  if (FMinimum < 0) and (FMaximum > 0) and
    ((FMinimum < -CHalfThreshold) or (FMaximum > CHalfThreshold)) then
  begin
    LMinimum := FMinimum / 2;
    LMaximum := FMaximum / 2;
    LValue := AValue / 2;
  end
  else
  begin
    { Keeping tiny opposite bounds unscaled avoids underflow into 0/0. }
    LMinimum := FMinimum;
    LMaximum := FMaximum;
    LValue := AValue;
  end;
  { Store each operation in Double. Native x87 extended expression evaluation
    must not select a different tick from pas2js IEEE Double arithmetic. }
  LNumerator := LValue - LMinimum;
  LDenominator := LMaximum - LMinimum;
  Result := LNumerator / LDenominator;
end;

function TNyxSliderScale.PositionOf(const AValue: TNyxText): Integer;
var
  LValue: TNyxDataValue;
  LNumber: Double;
  LIndex: Integer;
  LPosition: Double;
begin
  LValue := FDomain.ReadWire(AValue);
  LNumber := LValue.AsNumber;

  if (LNumber < FMinimum) or (LNumber > FMaximum) then
  begin
    raise ENyxContract.Create('Slider value is outside its physical range');
  end;

  if Length(FChoices) > 0 then
  begin
    for LIndex := 0 to High(FChoices) do
    begin

      if FChoices[LIndex].AsNumber = LNumber then
      begin
        Exit(LIndex);
      end;
    end;
    raise ENyxContract.Create('Slider value is outside its choices');
  end;

  if FMaximumPosition = 0 then
  begin
    Exit(0);
  end;
  LPosition := Fraction(LNumber) * FMaximumPosition;
  LPosition := LPosition + 0.5;
  Result := Floor(LPosition);
  Result := Max(0, Min(FMaximumPosition, Result));
end;

function TNyxSliderScale.ValueAt(APosition: Integer): TNyxText;
var
  LNumber: Double;
  LFraction: Double;
  LComplement: Double;
  LLeft: Double;
  LRight: Double;
begin

  if (APosition < 0) or (APosition > FMaximumPosition) then
  begin
    raise ENyxContract.Create('Slider position is outside its physical range');
  end;

  if Length(FChoices) > 0 then
  begin
    Exit(FChoices[APosition].ToJSON);
  end;

  if APosition = 0 then
  begin
    LNumber := FMinimum;
  end
  else if APosition = FMaximumPosition then
  begin
    LNumber := FMaximum;
  end
  else
  begin
    LFraction := APosition / FMaximumPosition;
    { Avoid overflowing max-min for a range crossing the complete Double span. }
    LComplement := 1 - LFraction;
    LLeft := LComplement * FMinimum;
    LRight := LFraction * FMaximum;
    LNumber := LLeft + LRight;
  end;

  if FDomain.Kind = nskInteger then
  begin
    Result := IntToStr(Floor(LNumber + 0.5));
  end
  else
  begin
    Result := TNyxStateValue.FromNumber(LNumber).NumberText;
  end;
  FDomain.ReadWire(Result);
end;

procedure TNyxSliderValue.Accept(const AScale: TNyxSliderScale;
  const AValue: TNyxText);
var
  LPosition: Integer;
begin
  LPosition := AScale.PositionOf(AValue);
  FScale := AScale;
  FAccepted := AValue;
  FPosition := LPosition;
  FDefined := True;
end;

function TNyxSliderValue.ReadPosition(APosition: Integer): TNyxText;
begin

  if not FDefined then
  begin
    raise ENyxContract.Create('Slider has no accepted value');
  end;

  if APosition = FPosition then
  begin
    Exit(FAccepted);
  end;
  Result := FScale.ValueAt(APosition);
end;

end.
