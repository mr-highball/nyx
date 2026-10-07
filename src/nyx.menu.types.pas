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

unit nyx.menu.types;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.types, nyx.popover.types, nyx.typeahead, nyx.errors;

type
  { Value-owned horizontal-bar policy. Labels are user text; traversal and
    hover are typed choices. A fresh bar remembers its last focused heading. }
  TNyxMenuBarOptions = record
  private
    FDefined: Boolean;
    FLabel: TNyxText;
    FWrap: Boolean;
    FHover: Boolean;
    FSearch: TNyxTypeAheadOptions;
  public
    { Arrow traversal wraps by default; False retains the current boundary. }
    function Wrap(AValue: Boolean): TNyxMenuBarOptions;
    { Only mouse entry switches an already open dropdown; touch never hovers. }
    function HoverSwitch(AValue: Boolean): TNyxMenuBarOptions;
    { Copy the shared Unicode matching/window policy without retaining a reader. }
    function TypeAhead(const AValue: TNyxTypeAheadOptions): TNyxMenuBarOptions;
    procedure Validate;
    property Caption: TNyxText read FLabel;
    property Wraps: Boolean read FWrap;
    property Hovers: Boolean read FHover;
    property Search: TNyxTypeAheadOptions read FSearch;
  end;

  { Open application identities own text, never execution keywords or controls. }
  TNyxMenuCommandRef = record
    Name: TNyxText;
  end;
  TNyxMenuGroupRef = record
    Name: TNyxText;
  end;
  TNyxMenuItemKind = (nmiAction, nmiCheck, nmiRadio, nmiSeparator, nmiSubmenu);
  TNyxMenuOpening = (nmoFirst, nmoLast);

  { Menu navigation is an explicit typed runtime policy. Every open starts at
    first/last visible item. Typeahead uses the shared Unicode search contract. }
  TNyxMenuOptions = record
  private
    FPresentation: TNyxPopoverOptions;
    FTypeAhead: TNyxTypeAheadOptions;
    FOpening: TNyxMenuOpening;
    FWrap: Boolean;
  public
    { Copy an existing validated popover policy; the menu owns its initial focus. }
    function Presentation(const AValue: TNyxPopoverOptions): TNyxMenuOptions;
    { Copy enabled/matching/inter-key timing policy into the next open. }
    function TypeAhead(const AValue: TNyxTypeAheadOptions): TNyxMenuOptions;
    { First/last visible command, including logically disabled commands. }
    function Opening(AValue: TNyxMenuOpening): TNyxMenuOptions;
    { Choose whether arrow traversal wraps at the first/last command. }
    function Wrap(AValue: Boolean): TNyxMenuOptions;
    { Undefined policies and unknown enum ordinals raise before presentation. }
    procedure Validate;
    property Placement: TNyxPopoverOptions read FPresentation;
    property Search: TNyxTypeAheadOptions read FTypeAhead;
    property OpenAt: TNyxMenuOpening read FOpening;
    property Wraps: Boolean read FWrap;
  end;

{ Open identities use bounded Unicode named-event admission and retain only text. }
function NyxMenuCommand(const AName: TNyxText): TNyxMenuCommandRef;
function NyxMenuGroup(const AName: TNyxText): TNyxMenuGroupRef;
function NyxMenu(const ATitle: TNyxText): TNyxMenuOptions;
{ Portable authored bar policy, shared by saved declarations and runtime owners. }
function NyxMenuBar(const ALabel: TNyxText): TNyxMenuBarOptions;

implementation

function NyxMenuBar(const ALabel: TNyxText): TNyxMenuBarOptions;
begin
  Result := Default(TNyxMenuBarOptions);
  Result.FDefined := True;
  Result.FLabel := ALabel;
  Result.FWrap := True;
  Result.FHover := True;
  Result.FSearch := NyxTypeAhead;
  Result.Validate;
end;

procedure TNyxMenuBarOptions.Validate;
begin

  if not FDefined or (FLabel = '') then
  begin
    raise ENyxModel.Create('Menu bar requires a defined policy and accessible label');
  end;
  FSearch.Validate;
end;

function TNyxMenuBarOptions.Wrap(AValue: Boolean): TNyxMenuBarOptions;
begin
  Result := Self;
  Result.FWrap := AValue;
  Result.Validate;
end;

function TNyxMenuBarOptions.HoverSwitch(AValue: Boolean): TNyxMenuBarOptions;
begin
  Result := Self;
  Result.FHover := AValue;
  Result.Validate;
end;

function TNyxMenuBarOptions.TypeAhead(const AValue: TNyxTypeAheadOptions): TNyxMenuBarOptions;
begin
  Result := Self;
  Result.FSearch := AValue;
  Result.Validate;
end;

function NyxMenuCommand(const AName: TNyxText): TNyxMenuCommandRef;
begin
  Result.Name := NyxNamedEvent(AName).Name;
end;

function NyxMenuGroup(const AName: TNyxText): TNyxMenuGroupRef;
begin
  Result.Name := NyxNamedEvent(AName).Name;
end;

function NyxMenu(const ATitle: TNyxText): TNyxMenuOptions;
begin
  Result := Default(TNyxMenuOptions);
  Result.FPresentation := NyxPopover(ATitle).Size(280, 600);
  Result.FTypeAhead := NyxTypeAhead;
  Result.FWrap := True;
end;

procedure TNyxMenuOptions.Validate;
begin
  FPresentation.Validate;
  FTypeAhead.Validate;

  if (Ord(FOpening) < Ord(Low(TNyxMenuOpening))) or
    (Ord(FOpening) > Ord(High(TNyxMenuOpening))) then
  begin
    raise ENyxModel.Create('Unknown menu opening policy');
  end;
end;

function TNyxMenuOptions.Presentation(const AValue: TNyxPopoverOptions): TNyxMenuOptions;
begin
  Result := Self;
  Result.FPresentation := AValue;
  Result.Validate;
end;

function TNyxMenuOptions.TypeAhead(const AValue: TNyxTypeAheadOptions): TNyxMenuOptions;
begin
  Result := Self;
  Result.FTypeAhead := AValue;
  Result.Validate;
end;

function TNyxMenuOptions.Opening(AValue: TNyxMenuOpening): TNyxMenuOptions;
begin
  Result := Self;
  Result.FOpening := AValue;
  Result.Validate;
end;

function TNyxMenuOptions.Wrap(AValue: Boolean): TNyxMenuOptions;
begin
  Result := Self;
  Result.FWrap := AValue;
  Result.Validate;
end;
end.
