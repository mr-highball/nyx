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
unit nyx.layout;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  nyx.types;

type

  (*
    an enum to control how an element should be aligned via a layout
    in the horizontal direction
  *)
  TElementHorzAlignment = (
    haLeft,
    haCenter,
    haRight
  );

  (*
    an enum to control how an element should be aligned via a layout
    in the vertical direction
  *)
  TElementVertAlignment = (
    vaTop,
    vaCenter,
    vaBottom
  );

  { INyxFixedBounds }
  (*
    bounds controlling how an element should be position inside of
    a parent container with fixed units
  *)
  INyxFixedBounds = interface(INyxElement)
    ['{89FD2A4F-1B32-43BB-BFA6-2E71BA9334C9}']

    //property methods
    function GetHAlign: TElementHorzAlignment;
    function GetLeft: Integer;
    function GetTop: Integer;
    function GetVAlign: TElementVertAlignment;
    procedure SetHAlign(const AValue: TElementHorzAlignment);
    procedure SetLeft(const AValue: Integer);
    procedure SetTop(const AValue: Integer);
    procedure SetVAlign(const AValue: TElementVertAlignment);

    //properties

    (*
      controls how the calculation should be performed to align an element horizontally
    *)
    property HorzAlignment : TElementHorzAlignment read GetHAlign write SetHAlign;

    (*
      controls how the calculation should be performed to align an element vertically
    *)
    property VertAlignment : TElementVertAlignment read GetVAlign write SetVAlign;

    (*
      controls horizontal position of the element, where 0 would be the
      "farthest left" inside of the parent
    *)
    property Left : Integer read GetLeft write SetLeft;

    (*
      controls vertical position of the element, where 0 would be the
      "farthest top" inside of the parent
    *)
    property Top : Integer read GetTop write SetTop;

    //methods

    (*
      fluent method for updating the horz alignment
    *)
    function UpdateHorzAlignment(const AValue : TElementHorzAlignment) : INyxFixedBounds;

    (*
      fluent method for updating the vert alignment
    *)
    function UpdateHorzAlignment(const AValue : TElementHorzAlignment) : INyxFixedBounds;

    (*
      fluent method for updating the left position
    *)
    function UpdateLeft(const AValue : Integer) : INyxFixedBounds;

    (*
      fluent method for updating the top position
    *)
    function UpdateTop(const AValue : Integer) : INyxFixedBounds;
  end;

  { INyxLayoutFixed }
  (*
    a layout to control the placement of elements with fixed units
  *)
  INyxLayoutFixed = interface(INyxLayout)
    ['{7DA0F2BA-A95E-48A9-B3AF-C7D2DD49724A}']

    //property methods
    function GetBounds(const AElement : INyxElement): INyxFixedBounds;

    //properties

    (*
      accessor for the bounds of a given element
    *)
    property Bounds[const AElement : INyxElement] : INyxFixedBounds read GetBounds; default;

    //methods

    (*
      adds an element to have it's placement inside of it's parent container
      using fixed set of bounds
    *)
    function Add(const AElement : INyxElement;
      const ABounds : INyxFixedBounds) : INyxLayoutFixed; overload;

    (*
      removes an element from this layout
      this method is the same as accessing the element collection and removing
      from their, however this scopes back to the specialized fixed layout
    *)
    function Remove(const AElement : INyxElement) : INyxLayoutFixed;
  end;

  { TNyxLayoutFixedImpl }
  (*
    base implementation for all fixed layouts
  *)
  TNyxLayoutFixedImpl = class(TNyxLayoutBaseImpl, INyxLayoutFixed)
  strict private
    FBounds : INyxElements;
  strict protected
    function DoUpdatePlacement(out Error: String): Boolean; override;

    (*
      children fixed layouts need to override this in order to place an
      element within their parent container provided the bounds
    *)
    function DoPlaceElement(const AElement : INyxElement;
      const ABounds : INyxFixedBounds; out Error : String) : Boolean; virtual; abstract;
  protected
    function GetBounds(const AElement : INyxElement): INyxFixedBounds;
  public
    property Bounds[const AElement : INyxElement] : INyxFixedBounds read GetBounds; default;

    function Add(const AElement : INyxElement;
      const ABounds : INyxFixedBounds) : INyxLayoutFixed; overload;

    function Remove(const AElement : INyxElement) : INyxLayoutFixed;

    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ TNyxLayoutFixedImpl }

function TNyxLayoutFixedImpl.DoUpdatePlacement(out Error: String): Boolean;
begin
  //todo - iterate stored elements and make a call to DoPlaceElement()
end;

function TNyxLayoutFixedImpl.GetBounds(const AElement: INyxElement): INyxFixedBounds;
begin
  Result := nil;

  //we use an element collection to store bounds with the same id as the element
  //so we can use the input as lookup
  if not FBounds.IndexOf(AElement) >= 0 then
    Exit;

  Result := FBounds[AElement] as INyxFixedBounds;
end;

function TNyxLayoutFixedImpl.Add(const AElement: INyxElement;
  const ABounds: INyxFixedBounds): INyxLayoutFixed;
begin

end;

function TNyxLayoutFixedImpl.Remove(const AElement: INyxElement): INyxLayoutFixed;
begin

end;

constructor TNyxLayoutFixedImpl.Create;
begin
  inherited Create;
  FBounds := NewNyxElements;

  //todo - add a remove observer on the elements to remove bounds too
end;

destructor TNyxLayoutFixedImpl.Destroy;
begin
  FBounds.Clear;
  FBounds := nil;
  inherited Destroy;
end;

end.

