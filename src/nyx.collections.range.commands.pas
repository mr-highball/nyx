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
unit nyx.collections.range.commands;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.collections.range, nyx.collections.range.actions,
  nyx.collections.clipboard.actions;

type
  { Optional command resolution preserves the original clipboard interfaces.
    Begin reserves canonical input/intent before any range/provider getter;
    ResolveRange supplies the already captured selection exactly once. A
    retired selection remains claimed and can never borrow a newer range. }
  INyxCollectionClipboardRangeCommand = interface(INyxCollectionClipboardCommand)
    ['{ECEC8C8F-177E-4095-9857-91EE7FA224A5}']
    procedure ResolveRange(const AIntent: INyxCollectionCellRangeIntent);
  end;

  { Optional controller port uses the established clipboard controller and
    SAME asynchronous host slot. No second authority/receipt engine belongs
    here. RAW range TSV uses explicitly configured headers/line endings and
    visible presentation fields between the exact selected endpoints. }
  INyxCollectionClipboardRangeInput = interface(IInterface)
    ['{E5CB112B-408B-444E-9388-7E302C85EC23}']
    function BeginRangeInput: INyxCollectionClipboardRangeCommand;
  end;

  { Runtime cell-range selection is explicitly off until configured. It never
    changes ordinary View.Selection. SelectCells admits typed stable identities
    against this exact mounted page/visible columns and preserves the previous
    range on invalid input. Any view publication or input incarnation change
    clears authority; GetCellRange returns immutable original facts only. Methods are
    UI-only and clipboard authority is still separately injected. }
  INyxCollectionCellRangeMount = interface(IInterface)
    ['{20C7056A-1233-4349-B743-AC8FB9E10007}']
    procedure ConfigureCellRange(AEnabled: Boolean);
    function SelectCells(const AAnchor, AFocus: TNyxCollectionCellIdentity): Boolean;
    procedure ClearCellRange;
    function GetCellRange: INyxCollectionCellRangeIntent;
    property CellRange: INyxCollectionCellRangeIntent read GetCellRange;
  end;

implementation

end.
