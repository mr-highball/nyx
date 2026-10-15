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

program nyx_program_document_compiled;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.bytes, nyx.model, nyx.codec, nyx.generated.view,
  nyx.programs.types, nyx.programs.contract, nyx.programs.declarations
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

{$ifdef PAS2JS}
var
  GRequest: TJSXMLHttpRequest;
{$endif}

{ Execute the real compiler-produced constructor before comparing its entire
  model with the adjacent admitted design. Only explicit Run below executes
  interpreted source; opening a definition carries no invocation authority. }
procedure Verify(const AExpected: TNyxText);
var
  LDocument: TNyxDocument;
  LDefinition: INyxProgramDefinition;
  LRefused: Boolean;
begin
  LDocument := BuildNyxDocument;
  try

    if TNyxCodec.Encode(LDocument) <> AExpected then
    begin
      raise Exception.Create('Complete compiled program document differs from its accepted design');
    end;
    LDefinition := LDocument.Programs.Definition(NyxProgramRef('Greeting 😀'));

    if LDefinition.Compile.Run(NyxProgramValues.WithValue(NyxProgramSymbol('Name'),
      NyxProgramValue('Ada'))).Outputs.Value(NyxProgramSymbol('Caption')).AsText <>
      TNyxText('Hello, Ada!😀') + #0 then
    begin
      raise Exception.Create('Compiler-produced saved program did not preserve exact executable behavior');
    end;
    LRefused := False;
    try
      LDocument.Programs.Definition(NyxProgramRef('Canvas layout')).Compile.Run(NyxProgramValues);
    except
      on E: ENyxProgram do
      begin
        LRefused := E.Diagnostic.Kind = npdCapability;
      end;
    end;

    if not LRefused then
    begin
      raise Exception.Create('Compiled declarations must not create runtime capability grants');
    end;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-program-compiled', 'passed');
    document.body.setAttribute('data-program-compiled-checks', '3');
    {$else}
    WriteLn('PASS 3 complete compiled program/source/runtime checks');
    {$endif}
  finally
    LDocument.Free;
  end;
end;

{$ifdef PAS2JS}
function Loaded(AEvent: TJSProgressEvent): Boolean;
begin
  Result := True;
  try

    if (GRequest.status <> 200) or (Length(GRequest.responseText) > 16 * 1024 * 1024) then
    begin
      raise Exception.Create('Exact admitted design was not served within its byte bound');
    end;
    Verify(GRequest.responseText);
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-program-compiled', 'failed');
      document.body.setAttribute('data-program-compiled-error', E.Message);
    end;
  end;
  GRequest := nil;
end;
{$else}
var
  LFile: TFileStream;
  LBytes: TNyxBytes;
{$endif}

begin
  {$ifdef PAS2JS}
  GRequest := TJSXMLHttpRequest.new;
  GRequest.open('GET', 'design.nyx');
  GRequest.onload := @Loaded;
  GRequest.send;
  {$else}
  try

    if ParamCount <> 1 then
    begin
      raise Exception.Create('Supply the adjacent exact admitted design');
    end;
    LFile := TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyWrite);
    try

      if LFile.Size > 16 * 1024 * 1024 then
      begin
        raise Exception.Create('Expected design exceeds the bounded fixture input');
      end;
      SetLength(LBytes, LFile.Size);

      if Length(LBytes) > 0 then
      begin
        LFile.ReadBuffer(LBytes[0], Length(LBytes));
      end;
    finally
      LFile.Free;
    end;
    Verify(NyxDecodeUTF8(LBytes));
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
