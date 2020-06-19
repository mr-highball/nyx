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
  *)
  TElementAlignment = (
    eaLeft,
    eaCenter,
    eaRight
  );

  { INyxFixedBounds }
  (*
    bounds controlling how an element should be position inside of
    a parent container with fixed units
  *)
  INyxFixedBounds = interface
    ['{89FD2A4F-1B32-43BB-BFA6-2E71BA9334C9}']

    //property methods
    function GetAlign: TElementAlignment;
    procedure SetAlign(const AValue: TElementAlignment);

    //properties

    (*
      controls how the calculation should be performed to align an element
    *)
    property Alignment : TElementAlignment read GetAlign write SetAlign;

    //methods

    (*
      fluent method for updating the alignment
    *)
    function UpdateAlignment(const AValue : TElementAlignment) : INyxFixedBounds;
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
    function AddElement(const AElement : INyxElement) : INyxLayoutFixed;
  end;

implementation

end.

