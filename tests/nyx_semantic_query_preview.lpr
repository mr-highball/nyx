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



program nyx_semantic_query_preview;
{$mode delphi}{$H+}{$codepage utf8}
uses
  SysUtils, {$ifdef PAS2JS}Web,{$else}Classes,{$endif}nyx.text,
  nyx.test.query.preview, nyx.studio.projects
  {$ifdef NYX_COMPILED_QUERY_PREVIEW}, nyx.generated.view, nyx.model, nyx.codec,
  nyx.collections{$endif};
var
  LChecks: Integer;
  LPair: TNyxProjectPair;
  {$ifndef PAS2JS}LStream: TFileStream;{$endif}
  {$ifdef NYX_COMPILED_QUERY_PREVIEW}LDocument: TNyxDocument;{$endif}
begin
  try
    LChecks := RunNyxSemanticQueryPreview;
    {$ifdef NYX_COMPILED_QUERY_PREVIEW}
    { The real compiler-built builder must preserve the complete grouped seed,
      including omitted defaults, reusable inheritance and Unicode/NUL rows. }
    LPair := CreateNyxQueryPreviewSeed(True);
    LDocument := BuildNyxDocument;
    try

      if TNyxCodec.Encode(LDocument) <> LPair.Design then
      begin
        raise Exception.Create('Compiled grouped builder differs from the admitted design');
      end;
      Inc(LChecks);

      if not LDocument.Collections.Snapshot(NyxCollection('tasks')).ItemAt(29)
        .GetValue(NyxBooleanField('done')) then
      begin
        raise Exception.Create('Compiled grouped builder lost its true Boolean cell');
      end;
      Inc(LChecks);
    finally
      LDocument.Free;
    end;
    {$endif}
    {$ifndef PAS2JS}

    if ParamCount = 1 then
    begin

      if FileExists(ParamStr(1)) then
      begin
        raise Exception.Create('Generated qualification source requires a new file');
      end;
      LPair := CreateNyxQueryPreviewSeed(True);
      LStream := TFileStream.Create(ParamStr(1), fmCreate);
      try
        { Portable native text is UTF-8 bytes; no ANSI string writer is used. }

        if LPair.Source <> '' then
        begin
          LStream.WriteBuffer(LPair.Source[1], Length(LPair.Source));
        end;
      finally
        LStream.Free;
      end;
    end;
    {$endif}
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(LChecks) + ' semantic query preview checks';
    document.body.setAttribute('data-semantic-query-preview', 'passed');
    {$else}
    WriteLn('PASS ', LChecks, ' semantic query preview checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := LException.Message;
      document.body.setAttribute('data-semantic-query-preview', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
