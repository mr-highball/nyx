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
unit nyx.order.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses Controls;

{ Read a back-to-front suffix of existing LCL/native siblings on the UI thread.
  Parent/children are borrowed and must outlive this call. No handle is created,
  control changed or callback invoked. True requires both lists to agree. False
  also covers absent handles and unsupported widgetsets: callers must retain
  their ordinary LCL reconciliation instead of inferring an optimization there.
  Win32 GetWindow reads actual HWND siblings; external native reorder or late
  handle creation therefore cannot hide behind an unchanged LCL child list. }
function NyxLCLChildSuffixCurrent(AParent: TWinControl;
  const AChildren: array of TControl): Boolean;

implementation

uses LCLPlatformDef
  {$ifdef MSWINDOWS}, Windows{$endif};

function NyxLCLChildSuffixCurrent(AParent: TWinControl;
  const AChildren: array of TControl): Boolean;
{$ifdef MSWINDOWS}
var
  LIndex: Integer;
  LPosition: Integer;
  LWindow: HWND;
{$endif}
begin
  Result := False;
  {$ifdef MSWINDOWS}

  if (GetBuildLCLWidgetType <> lpWin32) or (AParent = nil) then
  begin
    Exit;
  end;

  if not AParent.HandleAllocated or (Length(AChildren) > AParent.ControlCount) then
  begin
    Exit;
  end;
  LWindow := Windows.GetWindow(AParent.Handle, GW_CHILD);
  for LIndex := High(AChildren) downto 0 do
  begin
    LPosition := AParent.ControlCount - Length(AChildren) + LIndex;

    if (AParent.Controls[LPosition] <> AChildren[LIndex]) or
      not (AChildren[LIndex] is TWinControl) then
    begin
      Exit;
    end;

    if not TWinControl(AChildren[LIndex]).HandleAllocated or
      (LWindow <> TWinControl(AChildren[LIndex]).Handle) then
    begin
      Exit;
    end;
    LWindow := Windows.GetWindow(LWindow, GW_HWNDNEXT);
  end;
  Result := True;
  {$endif}
end;

end.
