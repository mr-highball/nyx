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

unit nyx.studio.handlerintent;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.data;

type
  { Independent exact callback-body changes for queued editor intent. No source
    owner, widget or patch interface enters this value. Undefined values refuse;
    trusted request capture and real compilation retain admission authority. }
  TNyxHandlerBatchSnapshot = record
  private
    FDefined: Boolean;
    FChanges: TNyxDataValue;
  public
    function ToData: TNyxDataValue;
    property Defined: Boolean read FDefined;
  end;

{ Strict transport admission retains exact Unicode expected/replacement bodies.
  Built-in Pascal authoring captures a typed patch through handleredits. }
function ReadNyxHandlerBatchSnapshot(const AChanges: TNyxDataValue): TNyxHandlerBatchSnapshot;

implementation

uses SysUtils, nyx.model, nyx.studio.handleredits;

function ReadNyxHandlerBatchSnapshot(const AChanges: TNyxDataValue): TNyxHandlerBatchSnapshot;
var
  LPatch: INyxHandlerPatch;
  LPlan: INyxHandlerPlan;
begin
  LPatch := ReadNyxHandlerPatch(AChanges);

  if not Supports(LPatch, INyxHandlerPlan, LPlan) then
  begin
    raise ENyxModel.Create('Handler intent requires a snapshot-capable typed patch');
  end;
  Result := Default(TNyxHandlerBatchSnapshot);
  Result.FChanges := LPlan.ToData.Copy;
  Result.FDefined := True;
end;

function TNyxHandlerBatchSnapshot.ToData: TNyxDataValue;
begin

  if not FDefined then
  begin
    raise ENyxModel.Create('Construct the handler batch before capturing its source intent');
  end;
  Result := FChanges.Copy;
end;

end.

