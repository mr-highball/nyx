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

unit nyx.literal.items;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text;

{ Literal Items are line-delimited rows; a table row is tab-delimited cells.
  Quotes are ordinary user text, and leading/trailing empty cells are retained.
  This is not CSV: the DOM and native adapters must not independently invent
  quoting or ANSI conversions. The caller owns and must free the returned list.
  Structured hierarchy, item identity and editing belong to collection views. }
function NyxLiteralCells(const ARow: TNyxText): TNyxStrings;

implementation

function NyxLiteralCells(const ARow: TNyxText): TNyxStrings;
var
  LIndex: Integer;
  LStart: Integer;
begin
  Result := TNyxStrings.Create;
  try
    LStart := 1;
    for LIndex := 1 to Length(ARow) do
    begin

      if ARow[LIndex] = #9 then
      begin
        Result.Add(Copy(ARow, LStart, LIndex - LStart));
        LStart := LIndex + 1;
      end;
    end;
    Result.Add(Copy(ARow, LStart, Length(ARow) - LStart + 1));
  except
    Result.Free;
    raise;
  end;
end;

end.