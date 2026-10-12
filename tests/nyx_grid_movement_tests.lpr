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

program nyx_grid_movement_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.collections.grid
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: String);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

{ An independent floating-point oracle adds the intended displacement then clips
  it. The product must instead clamp before Integer arithmetic. All these sums
  are exact in Double's integer plane, including High(Integer) steps, on both
  targets; no unimplemented browser Int64 contract or rounding is required. }
function Oracle(ARow, ARows, AStep: Integer): Integer;
var
  LWide: Double;
begin
  LWide := Double(ARow) + AStep;

  if LWide < 0 then
  begin
    LWide := 0;
  end;

  if LWide >= ARows then
  begin
    LWide := ARows - 1;
  end;
  Result := Integer(Trunc(LWide));
end;

procedure Run;
const
  CSteps: array[0..3] of Integer = (1, 2, 4, High(Integer));
var
  LRows: Integer;
  LColumns: Integer;
  LRow: Integer;
  LColumn: Integer;
  LStep: Integer;
  LCell: TNyxGridCell;
  LRefused: Boolean;
begin
  for LRows := 1 to 9 do
  begin
    for LColumns := 1 to 4 do
    begin
      for LRow := 0 to LRows - 1 do
      begin
        for LColumn := 0 to LColumns - 1 do
        begin
          for LStep := 0 to High(CSteps) do
          begin
            LCell := MoveNyxGridCell(LRow, LColumn, LRows, LColumns,
              ngmPageDown, CSteps[LStep]);
            Check((LCell.Row = Oracle(LRow, LRows, CSteps[LStep])) and
              (LCell.Column = LColumn), 'Page Down agrees with the independent clipped displacement');
            LCell := MoveNyxGridCell(LRow, LColumn, LRows, LColumns,
              ngmPageUp, CSteps[LStep]);
            Check((LCell.Row = Oracle(LRow, LRows, -CSteps[LStep])) and
              (LCell.Column = LColumn), 'Page Up agrees with the independent clipped displacement');
          end;
        end;
      end;
    end;
  end;
  LCell := MoveNyxGridCell(High(Integer) - 2, 1, High(Integer), 2,
    ngmPageDown, High(Integer));
  Check((LCell.Row = High(Integer) - 1) and (LCell.Column = 1),
    'large admitted dimensions do not overflow while paging down');
  LCell := MoveNyxGridCell(High(Integer) - 2, 1, High(Integer), 2,
    ngmPageUp, High(Integer));
  Check((LCell.Row = 0) and (LCell.Column = 1),
    'large admitted dimensions do not overflow while paging up');
  for LStep := -1 to 0 do
  begin
    LRefused := False;
    try
      MoveNyxGridCell(0, 0, 2, 2, ngmPageDown, LStep);
    except
      on EArgumentException do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'nonpositive paging steps refuse');
  end;
  LRefused := False;
  try
    MoveNyxGridCell(0, 0, 0, 2, ngmPageUp, 1);
  except
    on EArgumentException do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'empty grids refuse before navigation');
  LCell := MoveNyxGridCell(0, 0, 2, 2, ngmRight, 0);
  Check((LCell.Row = 0) and (LCell.Column = 1), 'ordinary moves retain their existing page-independent contract');
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-grid-movement-checks', IntToStr(GChecks));
    document.body.setAttribute('data-grid-movement-tests', 'passed');
    {$else}
    WriteLn('PASS ', GChecks, ' typed grid paging movement checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-grid-movement-tests', 'failed');
      document.body.setAttribute('data-event-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
