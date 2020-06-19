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
  INyxFixedBounds = interface
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

    //properties

    //methods

    (*
      adds an element to have it's placement inside of it's parent container
      using fixed set of bounds
    *)
    function Add(const AElement : INyxElement;
      const ABounds : INyxFixedBounds) : INyxLayoutFixed; overload;

    (*
      removes an element from this layout
    *)
    function RemoveElement(const AElement : INyxElement) : INyxLayoutFixed;
  end;

implementation

end.

