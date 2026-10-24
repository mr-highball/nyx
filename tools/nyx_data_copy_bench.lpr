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

program nyx_data_copy_bench;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils,
  {$ifdef PAS2JS}
  Web,
  {$endif}
  nyx.text,
  nyx.data;

const
  CIterations = 500000;

{ Samples isolate Copy and an ordinary extension read from initial admission.
  Exact wire/order/checksums still gate success; elapsed time is diagnostic only
  and is compared only for the same workload, compiler and target settings. }
function NowMilliseconds: Double;
begin
  {$ifdef PAS2JS}
  Result := window.performance.now;
  {$else}
  Result := GetTickCount64;
  {$endif}
end;

function Sample(AMembers: Integer; AReadStore: Boolean): TNyxText;
var
  LParts: TNyxStrings;
  LSource: TNyxDataValue;
  LCopy: TNyxDataValue;
  LStore: TNyxExtensions;
  LKey: TNyxExtensionRef;
  LWire: TNyxText;
  LLastKey: TNyxText;
  LMode: TNyxText;
  LStart: Double;
  LElapsed: Double;
  LIndex: Integer;
  LChecksum: Integer;
begin
  LParts := TNyxStrings.Create;
  try
    for LIndex := 0 to AMembers - 1 do
    begin
      LParts.Add('"field-' + TNyxText(IntToStr(LIndex)) + '":' +
        TNyxText(IntToStr(LIndex)));
    end;
    LSource := TNyxDataValue.ParseJSON('{' + LParts.Join(',') + '}');
  finally
    LParts.Free;
  end;
  LWire := LSource.ToJSON;
  LLastKey := LSource.Key(AMembers - 1);
  LKey := NyxExtension('app.copy-sample');
  LStore := TNyxExtensions.Create(nesNode);
  try
    LStore.SetValue(LKey, LSource);
    LChecksum := 0;
    LStart := NowMilliseconds;
    for LIndex := 1 to CIterations do
    begin

      if AReadStore then
      begin
        LCopy := LStore.Value(LKey);
      end
      else
      begin
        LCopy := LSource.Copy;
      end;
      Inc(LChecksum, LCopy.Count + Length(LCopy.Key(AMembers - 1)));
    end;
    LElapsed := NowMilliseconds - LStart;
    LStore.Remove(LKey);
    LSource := NyxNull;

    if (LChecksum <> CIterations * (AMembers + Length(LLastKey))) or
      (LCopy.ToJSON <> LWire) or
      (LCopy.Field(LLastKey).AsInteger <> AMembers - 1) then
    begin
      raise Exception.Create('Snapshot copy changed exact wire/order/lifetime');
    end;
  finally
    LStore.Free;
  end;
  LMode := 'copy';

  if AReadStore then
  begin
    LMode := 'extension-read';
  end;
  Result := LMode + ',' + TNyxText(IntToStr(AMembers)) + ',' +
    TNyxText(IntToStr(CIterations)) + ',' + TNyxText(IntToStr(Round(LElapsed))) +
    ',' + TNyxText(IntToStr(LChecksum));
end;

var
  LReport: TNyxText;
begin
  LReport := TNyxText('operation,members,iterations,elapsedMS,checksum') + #10 +
    Sample(16, False) + #10 + Sample(256, False) + #10 +
    Sample(16, True) + #10 + Sample(256, True) + #10;
  {$ifdef PAS2JS}
  document.body.textContent := LReport;
  document.body.setAttribute('data-nyx-data-copy', 'passed');
  {$else}
  Write(LReport);
  {$endif}
end.
