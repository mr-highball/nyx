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

program nyx_expression_local_compiled;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.model, nyx.codec, nyx.state, nyx.expressions,
  nyx.binding.types, nyx.source, nyx.generated.view
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

{$ifdef PAS2JS}
var
  GRequest: TJSXMLHttpRequest;
{$endif}

procedure Verify;
var
  LDocument: TNyxDocument;
  LParsed: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSpec: TNyxBindingSpec;
  LSource: TNyxText;
  {$ifndef PAS2JS}
  LFile: TFileStream;
  {$endif}
begin
  LDocument := BuildNyxDocument;
  LParsed := nil;
  LWorkspace := nil;
  try

    if not LDocument.Find('order-total').FindBinding(bpText, LSpec) or
      ((LSpec.Expression as INyxTextExpression).Evaluate(
        CaptureNyxExpressionInputs(LDocument.State)) <> TNyxText('Total 🌙: 40')) then
    begin
      raise Exception.Create('Compiled named expressions retain exact Unicode and calculation');
    end;

    if not LDocument.Find('order-submit').FindBinding(bpEnabled, LSpec) or
      not (LSpec.Expression as INyxBooleanExpression).Evaluate(
        CaptureNyxExpressionInputs(LDocument.State)) then
    begin
      raise Exception.Create('Compiled named condition retains specialized Boolean meaning');
    end;
    {$ifdef PAS2JS}
    LSource := GRequest.responseText;
    {$else}

    if ParamCount <> 1 then
    begin
      raise Exception.Create('Supply the exact handcrafted companion');
    end;
    LFile := TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyNone);
    try
      SetLength(LSource, LFile.Size);

      if LFile.Size > 0 then
      begin
        LFile.ReadBuffer(LSource[1], LFile.Size);
      end;
    finally
      LFile.Free;
    end;
    {$endif}
    LParsed := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);

    if TNyxCodec.Encode(LParsed) <> TNyxCodec.Encode(LDocument) then
    begin
      raise Exception.Create('Handcrafted compiler and managed source reader reconstruct identical documents');
    end;

    if LWorkspace.Render(LParsed) <> LSource then
    begin
      raise Exception.Create('Handcrafted compiled source is retained byte-exact on no-op rendering');
    end;
  finally
    LWorkspace.Free;
    LParsed.Free;
    LDocument.Free;
  end;
end;

procedure Complete;
begin
  try
    Verify;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-expression-local-compiled', 'passed');
    document.body.textContent := 'PASS 4 compiled handcrafted expression checks';
    {$else}
    WriteLn('PASS 4 compiled handcrafted expression checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-expression-local-compiled', 'failed');
      document.body.setAttribute('data-expression-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end;

{$ifdef PAS2JS}
function Loaded(AEvent: TJSProgressEvent): Boolean;
begin
  Result := True;

  if GRequest.status <> 200 then
  begin
    document.body.setAttribute('data-expression-local-compiled', 'failed');
    document.body.setAttribute('data-expression-error', 'Exact companion HTTP request failed');
    Exit;
  end;
  Complete;
  GRequest := nil;
end;
{$endif}

begin
  {$ifdef PAS2JS}
  GRequest := TJSXMLHttpRequest.new;
  GRequest.onload := @Loaded;
  GRequest.open('GET', 'nyx.generated.view.pas', True);
  GRequest.send;
  {$else}
  Complete;
  {$endif}
end.
