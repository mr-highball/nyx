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
program nyx_resource_labels_generated;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.data, nyx.resources, nyx.resource.sources,
  nyx.bytes, nyx.model, nyx.codec, nyx.generated.labels
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxResource.Create('Compiled labels: ' + AReason);
  end;
  Inc(GChecks);
end;

{ Execute the exact exported unit, rather than writing a second builder that
  resembles generated source. Both compilers consume the same UTF-8 artifact. }
procedure Run;
var
  LDocument: TNyxDocument;
  LRoundTrip: TNyxDocument;
  LDefinition: INyxResourceDefinition;
begin
  LDocument := BuildNyxDocument;
  LRoundTrip := nil;
  try
    Check(LDocument.Resources.Count = 5, 'all resource families survive the builder');
    LDefinition := LDocument.Resources.Definition(NyxResourceRef('notes'), NyxDefaultLocale);
    Check((LDefinition.Text = TNyxText('Exact notes 🌙')) and (LDefinition.ByteCount = 16),
      'supplementary text and exact bytes survive literal compilation');
    Check((NyxResourceLabelsOf(LDefinition).Count = 2) and
      NyxResourceLabelsOf(LDefinition).Contains(NyxResourceLabel('Docs, "quick" | 🌙')),
      'specialized factory calls retain exact creator tags');
    LDefinition := LDocument.Resources.Definition(NyxResourceRef('remote'), NyxDefaultLocale);
    Check((LDefinition.Source.Kind = rskHosted) and
      (LDefinition.Source.CachePolicy.Mode = rcmPersistent) and
      (LDefinition.Source.CachePolicy.FreshSeconds = 30), 'hosted policy survives the labelled builder');
    Check(NyxResourceLabelsOf(LDefinition).Contains(NyxResourceLabel('Hosted')) and
      (NyxResourceLabelsOf(LDefinition.FallbackDefinition).Count = 2),
      'hosted and fallback annotations remain independently defined');
    LDefinition := LDocument.Resources.Definition(NyxResourceRef('numbers'), NyxDefaultLocale);
    Check((NyxDecodeUTF8(LDefinition.Bytes) = '{"value":9007199254740993,"caption":"Dataset"}') and
      NyxResourceLabelsOf(LDefinition).Contains(NyxResourceLabel('Data')),
      'JSON retains exact large numeric tokens alongside labels');
    LDefinition := LDocument.Resources.Definition(NyxResourceRef('packed'), NyxDefaultLocale);
    Check((NyxEncodeBase64(LDefinition.Bytes) = 'AAH/') and
      NyxResourceLabelsOf(LDefinition).Contains(NyxResourceLabel('Data')),
      'packed binary resources retain exact bytes and discovery');
    LRoundTrip := TNyxCodec.Decode(TNyxCodec.Encode(LDocument));
    Check(TNyxCodec.Encode(LRoundTrip) = TNyxCodec.Encode(LDocument),
      'compiled document has exact versioned persistence');
  finally
    LDefinition := nil;
    LRoundTrip.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Run;
    WriteLn('PASS ', GChecks, ' exact compiled resource label checks');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-test-result', 'passed');
    document.body.setAttribute('data-label-checks', IntToStr(GChecks));
    {$endif}
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-test-result', 'failed');
      document.body.setAttribute('data-label-error', LException.Message);
      {$else}ExitCode := 1;{$endif}
    end;
  end;
end.
