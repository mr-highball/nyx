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

unit nyx.test.compiler.stream;

{$mode delphi}{$H+}{$codepage utf8}

interface

{ Portable incremental parsing/snapshot/EOF/budget checks. Compiler and actual
  job/transport lifetimes belong to their separate native consumers. }
function RunNyxCompilerStreamTests: Integer;

implementation

uses SysUtils, nyx.text, nyx.studio.compiler;

function RunNyxCompilerStreamTests: Integer;
const
  CSource: TNyxText = 'unit workshop; interface implementation end.';
  CFile: TNyxText = 'jobs/owned/workshop.pas';
var
  LReader: INyxCompilerReportReader;
  LBefore: INyxCompilerReport;
  LExpected: INyxCompilerReport;
  LLog: TNyxText;
  LPrefix: TNyxText;
  LBeforeText: TNyxText;
  LIndex: Integer;
  LSplit: Integer;
  LEnd: Integer;
  LRejected: Boolean;
  LChecks: Integer;

  procedure Check(AValue: Boolean; const AReason: String);
  begin

    if not AValue then
    begin
      raise Exception.Create(AReason);
    end;
    Inc(LChecks);
  end;

begin
  LChecks := 0;
  LLog := TNyxText('Compiler banner') + #13#10 + CFile + TNyxText('(1,1) Warning: Readable 🌙') +
    #13#10 + 'dependency.pas(2,3) Note: An independent dependency' + #10 +
    CFile + '(1,1) Error: Final message without newline';
  LExpected := ReadNyxCompilerReport(CSource, CSource, CFile, LLog);
  Check(LExpected.Count = 3, 'The complete oracle contains all three diagnostic families');
  for LSplit := 0 to Length(LLog) do
  begin
    LReader := NewNyxCompilerReportReader(CSource, CSource, CFile);
    LReader.Append(Copy(LLog, 1, LSplit));
    LEnd := 0;
    for LIndex := 1 to LSplit do
    begin

      if LLog[LIndex] in [#10, #13] then
      begin
        LEnd := LIndex;
      end;
    end;
    LPrefix := Copy(LLog, 1, LEnd);
    LBefore := LReader.Snapshot;
    LBeforeText := LBefore.Encode;
    Check(LBeforeText = ReadNyxCompilerReport(CSource, CSource, CFile, LPrefix).Encode,
      'Only complete records enter a split-position snapshot');
    LReader.Append(Copy(LLog, LSplit + 1, MaxInt));
    LReader.Finish;
    Check(LReader.Snapshot.Encode = LExpected.Encode,
      'Every native-byte/browser-code-unit split reconstructs the same complete report');
    Check(LBefore.Encode = LBeforeText, 'Earlier snapshots never grow with their reader');
    Check(not LReader.Finish, 'EOF sealing is idempotent');
  end;
  LRejected := False;
  try
    LReader.Append('Warning: Too late');
  except
    on ENyxCompilerReportSealed do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Sealed input cannot be silently reopened');
  LReader := NewNyxCompilerReportReader(CSource, CSource, CFile);
  for LIndex := 1 to Length(LLog) do
  begin
    LReader.Append(Copy(LLog, LIndex, 1));
  end;
  Check(LReader.Snapshot.Count = 2, 'The final incomplete line remains private before EOF');
  Check(LReader.Finish and (LReader.Snapshot.Encode = LExpected.Encode),
    'One-unit chunks preserve CR/LF and supplementary characters through EOF');
  LBefore := LReader.Snapshot;
  LReader := nil;
  Check(LBefore.Encode = LExpected.Encode, 'A snapshot outlives its complete reader');
  LReader := NewNyxCompilerReportReader(CSource, CSource, CFile);
  LRejected := False;
  try
    LReader.Append(TNyxText(StringOfChar('x', 1024 * 1024 + 1)));
  except
    on EArgumentException do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LReader.Snapshot.Count = 0), 'Oversized chunks refuse atomically');
  Check(LReader.Append('Warning: After refusal' + #10), 'A refused chunk consumes no parser budget');
  LReader := NewNyxCompilerReportReader(CSource, CSource, CFile);
  for LIndex := 1 to 520 do
  begin
    LReader.Append('Note: Bounded diagnostic' + #10);
  end;
  LReader.Finish;
  Check(LReader.Snapshot.Count = 512, 'The retained diagnostic bound matches complete reports');
  LReader := NewNyxCompilerReportReader(CSource, CSource, CFile);
  LPrefix := TNyxText(StringOfChar('x', 30000));
  LIndex := 1;
  while LIndex <= Length(LPrefix) do
  begin
    LReader.Append(Copy(LPrefix, LIndex, 17));
    Inc(LIndex, 17);
  end;
  Check(LReader.Snapshot.Count = 0, 'Long unterminated chatter cannot become a diagnostic');
  LReader.Append(#10 + 'Warning: After long chatter' + #10);
  Check(LReader.Snapshot.Count = 1, 'Scanning resumes after a fragmented non-diagnostic line');
  Result := LChecks;
end;

end.
