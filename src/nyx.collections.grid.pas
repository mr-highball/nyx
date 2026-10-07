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

unit nyx.collections.grid;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils;

type
  { Closed navigation intentions, mapped from admitted host keys by adapters.
    Home/End address this row; Control+Home/End address the whole data grid.
    Navigation never wraps across an edge or changes selection membership. }
  TNyxGridMove = (ngmLeft, ngmRight, ngmUp, ngmDown, ngmRowStart,
    ngmRowEnd, ngmFirstCell, ngmLastCell);

  { Detached zero-based data-cell position. Header rows are excluded. Adapters
    resolve Row against stable collection identities at each publication; this
    scalar snapshot retains no document, widget, item or mutable array. }
  TNyxGridCell = record
    Row: Integer;
    Column: Integer;
  end;

{ Return the admitted destination using the same arithmetic on both targets.
  Dimensions must be positive and the supplied position must be inside them;
  invalid/empty grids raise before a host or selection can be changed. Empty
  composites instead retain their separate ordinary keyboard entry point. }
function MoveNyxGridCell(ARow, AColumn, ARows, AColumns: Integer;
  AMove: TNyxGridMove): TNyxGridCell;

implementation

function MoveNyxGridCell(ARow, AColumn, ARows, AColumns: Integer;
  AMove: TNyxGridMove): TNyxGridCell;
begin

  if (ARows < 1) or (AColumns < 1) or (ARow < 0) or (ARow >= ARows) or
    (AColumn < 0) or (AColumn >= AColumns) then
  begin
    raise EArgumentException.Create('Grid movement requires an admitted data cell');
  end;
  Result.Row := ARow;
  Result.Column := AColumn;
  case AMove of
    ngmLeft:
      begin

        if AColumn > 0 then
        begin
          Dec(Result.Column);
        end;
      end;
    ngmRight:
      begin

        if AColumn < AColumns - 1 then
        begin
          Inc(Result.Column);
        end;
      end;
    ngmUp:
      begin

        if ARow > 0 then
        begin
          Dec(Result.Row);
        end;
      end;
    ngmDown:
      begin

        if ARow < ARows - 1 then
        begin
          Inc(Result.Row);
        end;
      end;
    ngmRowStart:
      begin
        Result.Column := 0;
      end;
    ngmRowEnd:
      begin
        Result.Column := AColumns - 1;
      end;
    ngmFirstCell:
      begin
        Result.Row := 0;
        Result.Column := 0;
      end;
    ngmLastCell:
      begin
        Result.Row := ARows - 1;
        Result.Column := AColumns - 1;
      end;
  end;
end;

end.
