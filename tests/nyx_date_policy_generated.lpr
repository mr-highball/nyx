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

program nyx_date_policy_generated;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifdef PAS2JS}JS, Web,{$else}Classes,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.contract, nyx.codec,
  nyx.schema, nyx.composition, nyx.generated.view;

{$ifdef PAS2JS}
var
  GRequest: TJSXMLHttpRequest;
{$endif}

{ Compile the exact source exported by the actual Inspector consumer. Compare
  its executed reconstruction with the entire accepted wire companion, not
  merely an expression or source admission result. All reconstructed trees are
  independently owned and released, including effective reusable projections. }
procedure Verify(const AExpected: TNyxText);
var
  LDocument: TNyxDocument;
  LContext: TNyxNode;
  LProjection: TNyxNode;
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
  LContext := nil;
  LChecks := 0;
  try
    LDocument := BuildNyxDocument;
    LDocument.Validate;
    ValidateNyxDocumentProperties(LDocument);
    Check(TNyxCodec.Encode(LDocument) = AExpected,
      'Executed generated source reconstructs the entire exact accepted document');
    LContext := RealizeNyxContext(LDocument, LDocument.Find('first-arrival'), LProjection);
    LDomain := NyxNodeValueDomain(LProjection);
    Check(LDomain.CalendarDate and
      (LDomain.ToData.Field('min').AsText = '2026-10-01') and
      (LDomain.ToData.Field('max').AsText = '2026-10-31'),
      'Executed source retains inclusive typed Gregorian bounds');
    Check((LDomain.ToData.Field('choices').Count = 3) and
      (LDomain.ToData.Field('choices').Item(2).AsText = ''),
      'Executed source retains exact choices including an optional empty date');
    FreeAndNil(LContext);
    LContext := RealizeNyxContext(LDocument, LDocument.Find('second-arrival'), LProjection);
    Check(NyxNodeValueDomain(LProjection).ToData.ToJSON =
      NyxDateDomain.Definition.ToData.ToJSON,
      'The second reusable instance retains its independent inherited policy');
    WriteLn('PASS ', LChecks, ' exact executed date-policy reconstruction checks');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-date-policy-generated', 'passed');
    document.body.setAttribute('data-date-policy-generated-checks', IntToStr(LChecks));
    {$endif}
  finally
    LContext.Free;
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
      raise Exception.Create('The exact accepted design companion did not load');
    end;
    Verify(GRequest.responseText);
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-date-policy-generated', 'failed');
      document.body.setAttribute('data-event-error', LException.Message);
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
