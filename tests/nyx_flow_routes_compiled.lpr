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


program nyx_flow_routes_compiled;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.codec, nyx.model, nyx.schema,
  nyx.flow.route.model, nyx.generated.view
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

procedure Verify(const AExpected: TNyxText);
var
  LDocument: TNyxDocument;
begin
  LDocument := BuildNyxDocument;
  try
    ValidateNyxDocumentProperties(LDocument);

    if TNyxCodec.Encode(LDocument) <> AExpected then
    begin
      raise Exception.Create('Compiled route companion differs from its complete admitted pair');
    end;

    if NyxControlFlowRoutes(LDocument.Find('constants-button')).Route(0).InputCount <> 7 then
    begin
      raise Exception.Create('Compiled constants lost an admitted typed mapping');
    end;
  finally
    LDocument.Free;
  end;
end;

{$ifdef PAS2JS}
var
  GRequest: TJSXMLHttpRequest;

function Loaded(AEvent: TJSProgressEvent): Boolean;
begin
  Result := True;
  try

    if GRequest.status <> 200 then
    begin
      raise Exception.Create('Expected route design did not load over HTTP');
    end;
    Verify(GRequest.responseText);
    document.body.setAttribute('data-flow-routes-compiled', 'passed');
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-flow-routes-compiled', 'failed');
      document.body.setAttribute('data-flow-routes-compiled-error', E.Message);
    end;
  end;
end;
{$else}
var
  LStream: TFileStream;
  LText: TNyxText;
{$endif}

begin
  {$ifdef PAS2JS}
  GRequest := TJSXMLHttpRequest.new;
  GRequest.onload := @Loaded;
  GRequest.open('GET', 'design.nyx');
  GRequest.send;
  {$else}
  try

    if ParamCount <> 1 then
    begin
      raise Exception.Create('Supply exact expected UTF-8 design bytes');
    end;
    LStream := TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyWrite);
    try
      SetLength(LText, LStream.Size);

      if Length(LText) > 0 then
      begin
        LStream.ReadBuffer(LText[1], Length(LText));
      end;
    finally
      LStream.Free;
    end;
    Verify(LText);
    WriteLn('PASS exact compiled route companion, all seven constant families');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
  {$endif}
end.
