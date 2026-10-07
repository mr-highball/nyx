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

program nyx_time_policy_generated;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifdef PAS2JS}JS, Web,{$else}Classes,{$endif}
  SysUtils, nyx.text, nyx.data, nyx.model, nyx.codec, nyx.contract, nyx.schema, nyx.state,
  nyx.generated.time;

{$ifdef PAS2JS}
var
  GRequest: TJSXMLHttpRequest;
{$endif}

{ Compile the unchanged source exported by the actual clock Inspector/queue.
  Exact whole-document equality proves source publication as well as constructor
  behavior; domain and default checks give useful bounded failure diagnostics.
  This consumer owns the reconstructed document and never contacts live Studio. }
procedure Verify(const AExpected: TNyxText);
var
  LDocument: TNyxDocument;
  LDomain: TNyxValueDomain;
  LChecks: Integer;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise Exception.Create(AReason);
    end;
    Inc(LChecks);
  end;

begin
  LDocument := nil;
  LChecks := 0;
  try
    LDocument := BuildNyxDocument;
    LDocument.Validate;
    ValidateNyxDocumentProperties(LDocument);
    Check(TNyxCodec.Encode(LDocument) = AExpected,
      'Compiled source reconstructs the exact accepted clock document');
    LDomain := NyxNodeValueDomain(LDocument.Find('start-time'));
    Check(LDomain.ClockTime and (LDomain.TimeStepMilliseconds = 500),
      'Compiled constructors retain the authored millisecond step');
    Check((LDomain.ToData.Field('min').AsText = '23:00:00.000') and
      (LDomain.ToData.Field('max').AsText = '01:00:00.000'),
      'Compiled constructors retain exact midnight-crossing bounds');
    Check(LDocument.Find('start-time').Prop('value') = '23:00:00.000',
      'Domain authoring preserves the independent control default');
    Check(LDocument.State.GetValue(NyxTextState('reminder')) = '00:30:00.000',
      'Domain authoring preserves the independent exact state default');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-time-policy-generated', 'passed');
    document.body.setAttribute('data-time-policy-generated-checks', IntToStr(LChecks));
    {$else}
    WriteLn('PASS ', LChecks, ' exact compiled clock-policy reconstruction checks');
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

    if GRequest.status <> 200 then
    begin
      raise Exception.Create('Exact accepted clock design did not load');
    end;
    Verify(GRequest.responseText);
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-time-policy-generated', 'failed');
      document.body.setAttribute('data-time-policy-generated-error', LException.Message);
    end;
  end;
end;
{$else}
var
  LStream: TFileStream;
  LExpected: TNyxText;
{$endif}

begin
  {$ifdef PAS2JS}
  GRequest := TJSXMLHttpRequest.new;
  GRequest.open('GET', 'design.nyx.json', True);
  GRequest.onload := @Loaded;
  GRequest.send;
  {$else}
  try

    if ParamCount <> 1 then
    begin
      raise Exception.Create('Supply the exact accepted clock design artifact');
    end;
    LStream := TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyWrite);
    try
      SetLength(LExpected, LStream.Size);

      if LExpected <> '' then
      begin
        LStream.ReadBuffer(LExpected[1], Length(LExpected));
      end;
    finally
      LStream.Free;
    end;
    Verify(LExpected);
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
  {$endif}
end.
