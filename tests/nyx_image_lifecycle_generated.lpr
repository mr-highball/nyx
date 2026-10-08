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
program nyx_image_lifecycle_generated;

{$mode delphi}{$H+}
{$codepage utf8}

uses
  SysUtils, nyx.model, nyx.types, nyx.scheduler, nyx.callbacks, nyx.events,
  nyx.generated.image.lifecycle
  {$ifdef PAS2JS}, Web{$endif};

var
  LDocument: TNyxDocument;
  LEvents: INyxEvents;
  LTarget: TNyxEventTarget;
  LTrigger: TNyxTrigger;
  LCount: Integer;

begin
  { Compile and execute the exact companion emitted by the ordinary session.
    Its TODO handlers and initialization registrations belong to that source;
    the harness supplies no replacement classes or handcrafted descriptors. }
  LDocument := BuildNyxDocument;
  LEvents := NewNyxEvents;
  try
    BindNyxCallbacks(LDocument, LEvents);
    LTarget := NyxControlEvents('hero-image', niRuntime);
    LCount := 0;
    for LTrigger := ntImageLoading to ntImageCleared do
    begin

      if LTrigger = ntImageReady then
      begin

        if (LEvents.On(LTarget, LTrigger).Count <> 2) or
          (LEvents.OnImageReady(LTarget).ExecutionPolicy <> neUIQueue) then
        begin
          raise Exception.Create('Exact compiled ready registrations differ');
        end;
      end
      else
      begin

        if LEvents.On(LTarget, LTrigger).Count <> 1 then
        begin
          raise Exception.Create('Exact compiled image registration differs');
        end;
      end;
      Inc(LCount);
    end;
    WriteLn('PASS / compiled image lifecycle / ', LCount, ' checks');
    {$ifdef PAS2JS}document.body.setAttribute('data-test-result', 'passed');{$endif}
  finally
    LEvents.Close;
    LEvents := nil;
    LDocument.Free;
  end;
end.
