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

unit nyx.test.theme;

{$mode delphi}{$H+}
{$codepage utf8}

interface

function RunNyxThemeTests: Integer;

implementation

uses
  nyx.text,
  nyx.model,
  nyx.theme;

procedure Check(ACondition: Boolean; const AMessage: TNyxText; var ACount: Integer);
begin

  if not ACondition then
  begin
    raise ENyxModel.Create('FAIL theme: ' + AMessage);
  end;
  Inc(ACount);
end;

procedure Reject(ATheme: TNyxTheme; var ACount: Integer);
var
  LRejected: Boolean;
begin
  LRejected := False;
  try
    ATheme.CSS;
  except
    on LException: ENyxModel do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'malformed theme cannot silently produce a CSS fallback', ACount);
end;

function RunNyxThemeTests: Integer;
var
  LTheme: TNyxTheme;
  LCSS: TNyxText;
begin
  Result := 0;
  Check((NyxThemeRGB('#12AbEF') = $12ABEF) and (NyxThemeRGB('#000000') = 0) and
    (NyxThemeRGB('#FFFFFF') = $FFFFFF), 'canonical RGB decoding is target independent', Result);
  LTheme := TNyxTheme.Create(True);
  try
    LTheme.Validate;
    LTheme.Accent := '#123456';
    LTheme.Radius := 23;
    LTheme.ControlRadius := 19;
    LTheme.FontSize := 17;
    LCSS := LTheme.CSS;
    Check((Pos('--nyx-accent:#123456', LCSS) > 0) and
      (Pos('--nyx-radius:23px', LCSS) > 0) and
      (Pos('--nyx-control-radius:19px', LCSS) > 0) and (Pos('font:17px/', LCSS) > 0),
      'a caller-owned palette reaches CSS without substituted defaults', Result);
    LTheme.Accent := 'red';
    Reject(LTheme, Result);
    LTheme.Accent := '#12GGFF';
    Reject(LTheme, Result);
    LTheme.Accent := '#123456';
    LTheme.Radius := -1;
    Reject(LTheme, Result);
    LTheme.Radius := 1001;
    Reject(LTheme, Result);
    LTheme.Radius := 12;
    LTheme.FontSize := 0;
    Reject(LTheme, Result);
    LTheme.FontSize := 257;
    Reject(LTheme, Result);
    LTheme.FontSize := 14;
    LTheme.ControlRadius := -1;
    Reject(LTheme, Result);
  finally
    LTheme.Free;
  end;
end;

end.
