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
unit nyx.collections.cell.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses Controls, nyx.model, nyx.root.types, nyx.menu, nyx.popover,
  nyx.collections.cell.actions;

{ Anchor is borrowed under FreeNotification until Revoke. Each ordinary managed
  presenter owns independent default theme/document/control lifetimes. No table
  theme, renderer, tree or mount is retained by this target factory. }
function NewNyxLCLCollectionCellPresenterFactory(AAnchor: TWinControl):
  INyxCollectionCellPresenterFactory;

implementation

uses Classes, SysUtils, nyx.collections.clipboard, nyx.menu.lcl, nyx.popover.lcl;

type
  { Weak native component observation clears a borrowed anchor as soon as its
    actual component retires, without acquiring a cycle back to its factory. }
  TCellAnchor = class(TComponent)
  public
    Anchor: TWinControl;
    constructor Create(AAnchor: TWinControl); reintroduce;
    destructor Destroy; override;
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  end;

  { Managed factory owns only a weak component observer. Revoke is flags-only
    and existing presenters have their own original anchor notifications. }
  TLCLCellFactory = class(TInterfacedObject, INyxCollectionCellPresenterFactory)
  private
    FTargets: TCellAnchor;
    FRevoked: Boolean;
    function RequireAnchor: TWinControl;
  public
    constructor Create(AAnchor: TWinControl);
    destructor Destroy; override;
    function CreateMenu(ADocument: TNyxDocument; const ARoot: TNyxRootRef;
      const AItems: TNyxMenuItems): INyxMenu;
    function CreateInspector(ADocument: TNyxDocument;
      const ARoot: TNyxRootRef): INyxPopover;
    procedure Revoke;
  end;

constructor TCellAnchor.Create(AAnchor: TWinControl);
begin
  inherited Create(nil);
  Anchor := AAnchor;
  Anchor.FreeNotification(Self);
end;

destructor TCellAnchor.Destroy;
begin

  if Anchor <> nil then
  begin
    Anchor.RemoveFreeNotification(Self);
  end;
  inherited Destroy;
end;

procedure TCellAnchor.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);

  if (AOperation = opRemove) and (AComponent = Anchor) then
  begin
    Anchor := nil;
  end;
end;

constructor TLCLCellFactory.Create(AAnchor: TWinControl);
begin
  inherited Create;

  if AAnchor = nil then
  begin
    raise ENyxCollectionClipboard.Create('Cell presenters require their actual table anchor');
  end;
  FTargets := TCellAnchor.Create(AAnchor);
end;

destructor TLCLCellFactory.Destroy;
begin
  FTargets.Free;
  inherited Destroy;
end;

function TLCLCellFactory.RequireAnchor: TWinControl;
begin

  if FRevoked or (FTargets = nil) or (FTargets.Anchor = nil) then
  begin
    raise ENyxCollectionClipboard.Create('Original cell presenter anchor was retired');
  end;
  Result := FTargets.Anchor;
end;

function TLCLCellFactory.CreateMenu(ADocument: TNyxDocument;
  const ARoot: TNyxRootRef; const AItems: TNyxMenuItems): INyxMenu;
var
  LAnchor: TWinControl;
begin
  LAnchor := RequireAnchor;
  Result := NewNyxLCLMenu(LAnchor, ADocument, ARoot, AItems);
end;

function TLCLCellFactory.CreateInspector(ADocument: TNyxDocument;
  const ARoot: TNyxRootRef): INyxPopover;
var
  LAnchor: TWinControl;
begin
  LAnchor := RequireAnchor;
  Result := NewNyxLCLPopover(LAnchor, ADocument, ARoot);
end;

procedure TLCLCellFactory.Revoke;
begin
  FRevoked := True;
end;

function NewNyxLCLCollectionCellPresenterFactory(AAnchor: TWinControl):
  INyxCollectionCellPresenterFactory;
begin
  Result := TLCLCellFactory.Create(AAnchor);
end;

end.