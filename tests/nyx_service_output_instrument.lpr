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
program nyx_service_output_instrument;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, nyx.text, nyx.bytes;

const
  CUses: TNyxText = ', nyx.generated.view;';
  CTestUses: TNyxText = ', nyx.generated.view, nyx.test.service.outputs.lcl;';
  CRun: TNyxText = '    Application.Run;';
  CTestRun: TNyxText = '    RunNyxGeneratedServiceOutputs(LApplication, LDocument);' + #10 +
    '    Application.Run;';

{ A fresh qualification copy adds exactly one driver unit and one call before
  the ordinary event loop. Generated mounting, services, reporter and teardown
  remain byte exact. No host factory or accepted document is substituted. }
function ReplaceOnce(const AText, AFrom, ATo: TNyxText): TNyxText;
var
  LAt: Integer;
begin
  LAt := Pos(AFrom, AText);

  if (LAt = 0) or (Pos(AFrom, Copy(AText, LAt + Length(AFrom), MaxInt)) > 0) then
  begin
    raise Exception.Create('Generated entry boundary must occur exactly once');
  end;
  Result := Copy(AText, 1, LAt - 1) + ATo + Copy(AText, LAt + Length(AFrom), MaxInt);
end;

var
  LStream: TFileStream;
  LBytes: TNyxBytes;
  LSource: TNyxText;
  LInstrumented: TNyxText;
begin
  try

    if (ParamCount <> 2) or FileExists(ParamStr(2)) then
    begin
      raise Exception.Create('Supply generated entry and a fresh instrumented entry path');
    end;
    LStream := TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyWrite);
    try

      if (LStream.Size < 1) or (LStream.Size > 1024 * 1024) then
      begin
        raise Exception.Create('Unexpected generated entry size');
      end;
      SetLength(LBytes, LStream.Size);
      LStream.ReadBuffer(LBytes[0], Length(LBytes));
    finally
      LStream.Free;
    end;
    LSource := NyxDecodeUTF8(LBytes);

    if Pos('program nyx_native;', LSource) <> 1 then
    begin
      raise Exception.Create('Expected Studio native entry point');
    end;
    LInstrumented := ReplaceOnce(ReplaceOnce(LSource, CUses, CTestUses), CRun, CTestRun);

    if ReplaceOnce(ReplaceOnce(LInstrumented, CTestRun, CRun), CTestUses, CUses) <> LSource then
    begin
      raise Exception.Create('Instrumentation changed generated application setup');
    end;
    LBytes := NyxEncodeUTF8(LInstrumented);
    LStream := TFileStream.Create(ParamStr(2), fmCreate);
    try
      LStream.WriteBuffer(LBytes[0], Length(LBytes));
    finally
      LStream.Free;
    end;
    WriteLn('PASS reversible driver insertion; generated services, mount and teardown unchanged');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL ', LError.Message);
      ExitCode := 1;
    end;
  end;
end.
