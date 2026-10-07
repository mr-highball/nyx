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

program nyx_data_read_bench;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, {$ifdef PAS2JS}Web,{$endif} nyx.text, nyx.data;

const
  CIterations = 16;
  CKeys: array[0..3] of TNyxText = ('title', 'description', 'notes', 'help');

{ This same deterministic workload separates snapshot construction from reads.
  Native wall-clock milliseconds and browser performance.now samples are only
  comparable within one target/configuration. Checksums retain every exact read;
  successful timing alone never qualifies value or ownership semantics. }
function NowMilliseconds: Double;
begin
  {$ifdef PAS2JS}
  Result := window.performance.now;
  {$else}
  Result := GetTickCount64;
  {$endif}
end;

function Sample(ALength: Integer): TNyxText;
var
  LText: TNyxText;
  LValue: TNyxDataValue;
  LFields: array[0..3] of TNyxDataField;
  LStart: Double;
  LBuild: Double;
  LRead: Double;
  LChecksum: Integer;
  LIteration: Integer;
  LIndex: Integer;
begin
  LText := TNyxText(StringOfChar('x', ALength)) + TNyxText('🌙') + NyxScalarText(0);
  LStart := NowMilliseconds;
  for LIndex := Low(CKeys) to High(CKeys) do
  begin
    LFields[LIndex] := NyxField(CKeys[LIndex], NyxData(LText));
  end;
  LValue := NyxObject(LFields);
  LBuild := NowMilliseconds - LStart;
  LChecksum := 0;
  LStart := NowMilliseconds;
  for LIteration := 1 to CIterations do
  begin
    for LIndex := Low(CKeys) to High(CKeys) do
    begin

      if (LValue.Count <> Length(CKeys)) or (LValue.Key(LIndex) <> CKeys[LIndex]) or
        (LValue.Field(CKeys[LIndex]).AsText <> LText) then
      begin
        raise Exception.Create('Structured read lost exact value/order');
      end;
      Inc(LChecksum, Length(LValue.Field(CKeys[LIndex]).AsText));
    end;
  end;
  LRead := NowMilliseconds - LStart;

  if LChecksum <> CIterations * Length(CKeys) * Length(LText) then
  begin
    raise Exception.Create('Structured read checksum changed');
  end;
  Result := TNyxText(IntToStr(ALength)) + ',' + TNyxText(IntToStr(CIterations)) + ',' +
    TNyxText(IntToStr(Round(LBuild))) + ',' + TNyxText(IntToStr(Round(LRead))) + ',' +
    TNyxText(IntToStr(LChecksum));
end;

var
  LReport: TNyxText;
begin
  LReport := TNyxText('asciiUnits,iterations,constructMS,readMS,checksum') + #10 +
    Sample(512) + #10 + Sample(5001) + #10 + Sample(50001) + #10;
  {$ifdef PAS2JS}
  document.body.textContent := LReport;
  document.body.setAttribute('data-nyx-data-read', 'passed');
  {$else}
  Write(LReport);
  {$endif}
end.
