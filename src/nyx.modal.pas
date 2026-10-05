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


unit nyx.modal;
{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text;

type
  { A dismiss observer is borrowed. Clear it before the receiver retires;
    it must return from the current notification before destroying its host. }
  TNyxModalDismiss = procedure of object;

  { Immutable fluent modal geometry in logical viewport percent/pixels.
    Adapter hosts contain ordinary Nyx views, never a second widget toolkit. }
  TNyxModalOptions = record
  private
    FTitle: TNyxText;
    FViewportPercent: Integer;
    FMaximumWidth: Integer;
  public
    { Return a copied option value. Percent accepts 20..100; pixels 240..16384.
      Invalid dimensions raise ENyxModel before a target changes its window. }
    function Viewport(APercent: Integer): TNyxModalOptions;
    function MaximumWidth(APixels: Integer): TNyxModalOptions;
    property Title: TNyxText read FTitle;
    property ViewportPercent: Integer read FViewportPercent;
    property WidthLimit: Integer read FMaximumWidth;
  end;

  { Managed presentation lifetime, independent of document/history ownership.
    A controller moves or unmounts its Nyx view before retiring this host.
    Hide is silent; user Escape/window dismissal notifies the borrowed receiver.
    Source/document pointers and platform control types never cross this API. }
  INyxModalHost = interface(IInterface)
    ['{271BE5DA-8A62-4F45-A8BC-001005005001}']
    function GetOpen: Boolean;
    function GetOnDismiss: TNyxModalDismiss;
    procedure SetOnDismiss(AValue: TNyxModalDismiss);
    { Show validates copied options before opening. Repeated Show updates options
      without replacing content; adapters retain their current owning viewport. }
    procedure Show(const AOptions: TNyxModalOptions);
    procedure Hide;
    property IsOpen: Boolean read GetOpen;
    property OnDismiss: TNyxModalDismiss read GetOnDismiss write SetOnDismiss;
  end;

{ Title is portable user text. Defaults use 94 percent and a 1400-pixel width cap. }
function NyxModal(const ATitle: TNyxText): TNyxModalOptions;

implementation

uses
  nyx.model;

function NyxModal(const ATitle: TNyxText): TNyxModalOptions;
begin
  Result := Default(TNyxModalOptions);
  Result.FTitle := ATitle;
  Result.FViewportPercent := 94;
  Result.FMaximumWidth := 1400;
end;

function TNyxModalOptions.Viewport(APercent: Integer): TNyxModalOptions;
begin

  if (APercent < 20) or (APercent > 100) then
  begin
    raise ENyxModel.Create('Modal viewport percent must be 20..100');
  end;
  Result := Self;
  Result.FViewportPercent := APercent;
end;

function TNyxModalOptions.MaximumWidth(APixels: Integer): TNyxModalOptions;
begin

  if (APixels < 240) or (APixels > 16384) then
  begin
    raise ENyxModel.Create('Modal maximum width must be 240..16384 logical pixels');
  end;
  Result := Self;
  Result.FMaximumWidth := APixels;
end;

end.
