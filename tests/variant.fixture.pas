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
unit variant.fixture;
{$mode delphi}{$H+}{$codepage utf8}

interface

{ Supplementary source text before the include stays exact: 🦊. }
{$i variant.fixture.inc}

{ Exercise both ordinary fields and selected nested alternatives in real code. }
function RunVariants: LongInt;

implementation

function RunVariants: LongInt;
var
  LCoordinates: TCoordinates;
  LPacket: TPacket;
begin
  LCoordinates.Values[0] := 5;
  LCoordinates.Values[1] := 6;
  LPacket.Fixed := 10;
  LPacket.Kind := True;
  LPacket.Prefix := 7;
  LPacket.Mode := 1;
  LPacket.Detail := 8;
  Result := LCoordinates.X + LCoordinates.Y + LPacket.Prefix +
    LPacket.Detail + LPacket.Other;
end;

end.
