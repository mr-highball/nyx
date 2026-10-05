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

program nyx_event_queue_generated;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.types, nyx.model, nyx.callbacks, nyx.scheduler, nyx.generated.view
  {$ifdef PAS2JS}, Web{$endif};

var
  LDocument: TNyxDocument;
  LEvents: TNyxAuthoredEventInfos;
  LChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: String);
begin

  if not ACondition then
  begin
    raise ENyxModel.Create('Compiled event companion: ' + AReason);
  end;
  Inc(LChecks);
end;

begin
  try
    LDocument := BuildNyxDocument;
    try
      LDocument.Validate;
      LEvents := NyxAuthoredEvents(LDocument.Find('reply-memo'));
      Check((Length(LEvents) = 1) and (LEvents[0].Trigger = ntBeforeKeyPress) and
        (Length(LEvents[0].Callbacks) = 1) and (LEvents[0].Policy = neSequential),
        'Built-in event reproduces the accepted policy and surviving registration');
      LEvents := NyxAuthoredEvents(LDocument.Find('first-search'));
      Check((Length(LEvents) = 1) and (LEvents[0].Trigger = ntNamed) and
        (LEvents[0].Name.Name = NyxSemantic(nseSearch).Name) and
        (Length(LEvents[0].Callbacks) = 1), 'Named reusable instance reproduces its exact registration');
      Check(Length(NyxAuthoredEvents(LDocument.Find('second-search'))) = 0,
        'Another reusable instance remains independent');
      Check((LDocument.Count = 2) and (LDocument.ComponentCount = 1),
        'Application retains both pages and reusable definition');
    finally
      LDocument.Free;
    end;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-event-generated', 'passed');
    document.body.setAttribute('data-event-generated-checks', IntToStr(LChecks));
    {$else}
    WriteLn('PASS ', LChecks, ' compiled callback companion reconstruction checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-event-generated', 'failed');
      document.body.setAttribute('data-event-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
