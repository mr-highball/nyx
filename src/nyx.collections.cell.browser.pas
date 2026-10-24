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
unit nyx.collections.cell.browser;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses Web, nyx.model, nyx.root.types, nyx.menu, nyx.popover,
  nyx.collections.cell.actions;

{ Borrow the actual table anchor until Revoke. New ordinary managed presenters
  own their independent default theme and document; no renderer/theme is borrowed
  from the table. The factory has no model, mount or controller back-reference. }
function NewNyxBrowserCollectionCellPresenterFactory(AAnchor: TJSHTMLElement):
  INyxCollectionCellPresenterFactory;

implementation

uses SysUtils, nyx.collections.clipboard, nyx.menu.browser, nyx.popover.browser;

type
  { The browser anchor is a borrowed GC-owned DOM element. Revocation clears
    it before teardown; detached/hidden invokers are rejected by the popover. }
  TBrowserCellFactory = class(TInterfacedObject, INyxCollectionCellPresenterFactory)
  private
    FAnchor: TJSHTMLElement;
    FRevoked: Boolean;
    procedure RequireAnchor;
  public
    constructor Create(AAnchor: TJSHTMLElement);
    function CreateMenu(ADocument: TNyxDocument; const ARoot: TNyxRootRef;
      const AItems: TNyxMenuItems): INyxMenu;
    function CreateInspector(ADocument: TNyxDocument;
      const ARoot: TNyxRootRef): INyxPopover;
    procedure Revoke;
  end;

constructor TBrowserCellFactory.Create(AAnchor: TJSHTMLElement);
begin
  inherited Create;

  if AAnchor = nil then
  begin
    raise ENyxCollectionClipboard.Create('Cell presenters require their actual table anchor');
  end;
  FAnchor := AAnchor;
end;

procedure TBrowserCellFactory.RequireAnchor;
begin

  if FRevoked or (FAnchor = nil) or not document.body.contains(FAnchor) then
  begin
    raise ENyxCollectionClipboard.Create('Original cell presenter anchor was retired');
  end;
end;

function TBrowserCellFactory.CreateMenu(ADocument: TNyxDocument;
  const ARoot: TNyxRootRef; const AItems: TNyxMenuItems): INyxMenu;
var
  LAnchor: TJSHTMLElement;
begin
  RequireAnchor;
  LAnchor := FAnchor;
  Result := NewNyxBrowserMenu(LAnchor, ADocument, ARoot, AItems);
end;

function TBrowserCellFactory.CreateInspector(ADocument: TNyxDocument;
  const ARoot: TNyxRootRef): INyxPopover;
var
  LAnchor: TJSHTMLElement;
begin
  RequireAnchor;
  LAnchor := FAnchor;
  Result := NewNyxBrowserPopover(LAnchor, ADocument, ARoot);
end;

procedure TBrowserCellFactory.Revoke;
begin
  FRevoked := True;
  FAnchor := nil;
end;

function NewNyxBrowserCollectionCellPresenterFactory(AAnchor: TJSHTMLElement):
  INyxCollectionCellPresenterFactory;
begin
  Result := TBrowserCellFactory.Create(AAnchor);
end;

end.