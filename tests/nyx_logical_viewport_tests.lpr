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
program nyx_logical_viewport_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.layout.viewport
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: String);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

{ The same geometry cases compile for both targets. Native checked arithmetic
  exercises signed endpoints; browser execution remains a separate host gate. }
procedure Run;
var
  LBox: TNyxViewportBox;
  LClip: TNyxViewportBox;
  LPlace: TNyxViewportPlacement;
  LOriginal: TNyxViewportBox;
  LIndex: Integer;
  LRefused: Boolean;
begin
  LClip := NyxViewportBox(0, 0, 640, 300);
  LOriginal := NyxViewportBox(12, 80000, 600, 72);
  LBox := LOriginal.Translated(0, -79950);
  LPlace := NyxPlaceViewport(LBox, LClip, 0, 0, 32767, 65535);
  Check(LPlace.HasArea and (LPlace.Y = 50) and (LPlace.Height = 72),
    'Distant input reaches the physical viewport with its complete native face');
  Check((LOriginal.Y = 80000) and (LOriginal.Height = 72),
    'Projection retains independent original logical geometry');

  LBox := NyxViewportBox(-14, -24, 300, 80);
  LPlace := NyxPlaceViewport(LBox, LClip, 0, 0, 32767, 65535);
  Check((LPlace.X = -14) and (LPlace.Y = -24) and (LPlace.Height = 80),
    'Small partly visible inputs preserve negative origin and complete paint size');
  Check((LPlace.Clip.X = 0) and (LPlace.Clip.Height = 56) and
    (LPlace.ContentOffsetY = 0), 'Small native face uses parent clipping');

  LBox := NyxViewportBox(0, -80000, 640, 82000);
  LPlace := NyxPlaceViewport(LBox, LClip, 0, 0, 32767, 65535);
  Check((LPlace.Y = 0) and (LPlace.Height = 300) and
    (LPlace.ContentOffsetY = 80000), 'Oversized container projects exact visible intersection');
  Check((LBox.Height = 82000) and (LPlace.ScreenY = 0),
    'Physical size never replaces the complete scroll extent');

  LBox := NyxViewportBox(0, 70000, 640, 80);
  LPlace := NyxPlaceViewport(LBox, LClip, 0, 0, 32767, 65535);
  Check(not LPlace.HasArea and (LPlace.Width = 0) and (LPlace.Height = 0),
    'Wholly offscreen faces allocate no physical area');

  LClip := NyxViewportBox(120, 140, 90, 110);
  LBox := NyxViewportBox(110, 130, 140, 160);
  LPlace := NyxPlaceViewport(LBox, LClip, 100, 100, 32767, 65535);
  Check((LPlace.X = 10) and (LPlace.Y = 30) and
    (LPlace.Clip.X = 120) and (LPlace.Clip.Height = 110),
    'Nested parent window origin is distinct from its intersection origin');

  LBox := NyxViewportBox(High(Integer) - 10, Low(Integer), 10, 10);
  Check((LBox.Right = High(Integer)) and (LBox.Bottom = Low(Integer) + 10),
    'Exact signed 32-bit endpoints remain legal');

  { Explicit platform limits, hidden viewports and malformed values must not
    acquire a plausible but false rectangle through clipping or wraparound. }
  for LIndex := 0 to 6 do
  begin
    LRefused := False;
    try
      case LIndex of
        0:
        begin
          LBox := NyxViewportBox(High(Integer), 0, 1, 1);
        end;
        1:
        begin
          LBox := NyxViewportBox(0, 0, -1, 1);
        end;
        2:
        begin
          LBox := NyxViewportBox(Low(Integer), 0, 1, 1).Translated(-1, 0);
        end;
        3:
        begin
          LBox := Default(TNyxViewportBox).Translated(1, 0);
        end;
        4:
        begin
          LPlace := NyxPlaceViewport(Default(TNyxViewportBox), LClip, 0, 0, 32767, 65535);
        end;
        5:
        begin
          LPlace := NyxPlaceViewport(LBox, LClip, 0, 0, 0, 65535);
        end;
        6:
        begin
          LBox := NyxViewportBox(0, 0, 70000, 80000);
          LPlace := NyxPlaceViewport(LBox, LBox, 0, 0, 32767, 65535);
        end;
      end;
    except
      on LException: EArgumentException do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Invalid/unrepresentable viewport refuses case ' + IntToStr(LIndex));
  end;
  LClip := NyxViewportBox(0, 0, 0, 0);
  LBox := NyxViewportBox(0, 0, 640, 82000);
  LPlace := NyxPlaceViewport(LBox, LClip, 0, 0, 32767, 65535);
  Check(not LPlace.HasArea and (LBox.Height = 82000),
    'Temporarily hidden viewport preserves full logical content');
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-logical-tests', 'passed');
    document.body.setAttribute('data-logical-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' portable logical viewport checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-logical-tests', 'failed');
      document.body.setAttribute('data-logical-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
