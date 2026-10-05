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

program nyx_design_source_consumer;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.text, nyx.model, nyx.codec, nyx.generated.view;

var
  LDocument: TNyxDocument;
  LStream: TFileStream;
  LExpected: TNyxText;

begin
  LDocument := nil;
  try
    try

      if ParamCount <> 1 then
      begin
        raise ENyxModel.Create('Supply the exact expected design artifact');
      end;
      { File bytes are portable UTF-8, never ANSI RTL line collections. Execute
        the exact admitted companion, including its crafted local/expression,
        rather than asking the generator for another replacement builder. }
      LStream := TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyWrite);
      try

        if LStream.Size > 16 * 1024 * 1024 then
        begin
          raise ENyxModel.Create('Expected design exceeds the fixture boundary');
        end;
        SetLength(LExpected, LStream.Size);

        if Length(LExpected) > 0 then
        begin
          LStream.ReadBuffer(LExpected[1], Length(LExpected));
        end;
      finally
        LStream.Free;
      end;
      LDocument := BuildNyxDocument;

      if TNyxCodec.Encode(LDocument) <> LExpected then
      begin
        raise ENyxModel.Create('Compiled design companion differs from its admitted pair');
      end;
      WriteLn('PASS exact compiled design/source pair');
    except
      on LException: Exception do
      begin
        WriteLn('FAIL ', LException.Message);
        DumpExceptionBackTrace(Output);
        ExitCode := 1;
      end;
    end;
  finally
    LDocument.Free;
  end;
end.
