{ nyx

  Copyright (c) 2020 mr-highball

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}
unit nyx.element.button;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  nyx.types;

type

  { INyxElementButton }
  (*
    base button element
  *)
  INyxElementButton = interface(INyxElement)
    ['{D93EBE55-E5A7-45A8-AEF0-5B04EB806A10}']
  end;

function NewNyxButton : INyxElementButton;

implementation
uses
{$IFDEF BROWSER}
  nyx.element.button.browser;
{$ELSE}
  nyx.element.button.std;
{$ENDIF}
var
  DefaultNyxButton : TNyxElementClass;

function NewNyxButton: INyxElementButton;
begin
  Result := DefaultNyxButton.Create as INyxElementButton;
end;

initialization
{$IFDEF BROWSER}
  DefaultNyxButton := TNyxElementButtonBrowserImpl;
{$ELSE}
  DefaultNyxButton := TNyxElementButtonStdImpl;
{$ENDIF}
end.

