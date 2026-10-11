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

unit nyx.studio.callbackintent;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.data;

type
  { Independent normalized callback intent for queued records. No COM interface,
    source, widget or session enters the value; execution/publication authority
    stays in the capturing Studio request. Unconstructed snapshots refuse. }
  TNyxCallbackBatchSnapshot = record
  private
    FDefined: Boolean;
    FChanges: TNyxDataValue;
  public
    function ToData: TNyxDataValue;
    property Defined: Boolean read FDefined;
  end;

{ Strict explicit transport boundary, normalized by the existing typed callback
  decoder. Built-in Pascal authoring captures a typed patch through callbackedits. }
function ReadNyxCallbackBatchSnapshot(const AChanges: TNyxDataValue): TNyxCallbackBatchSnapshot;

implementation

uses SysUtils, nyx.model, nyx.studio.callbackedits;

function ReadNyxCallbackBatchSnapshot(const AChanges: TNyxDataValue): TNyxCallbackBatchSnapshot;
var
  LPatch: INyxCallbackPatch;
  LPlan: INyxCallbackPlan;
begin
  LPatch := ReadNyxCallbackPatch(AChanges);

  if not Supports(LPatch, INyxCallbackPlan, LPlan) then
  begin
    raise ENyxModel.Create('Callback intent requires a snapshot-capable typed patch');
  end;
  Result := Default(TNyxCallbackBatchSnapshot);
  Result.FChanges := LPlan.ToData.Copy;
  Result.FDefined := True;
end;

function TNyxCallbackBatchSnapshot.ToData: TNyxDataValue;
begin

  if not FDefined then
  begin
    raise ENyxModel.Create('Construct the callback batch before capturing its source intent');
  end;
  Result := FChanges.Copy;
end;

end.
