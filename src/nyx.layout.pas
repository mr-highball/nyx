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
  nyx.types,
  nyx.element,
  nyx.container;

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

  { INyxBounds }
  (*
    base bounds which for controlling how an element should be positioned
  *)
  INyxBounds = interface(INyxElement)
    ['{B61FD617-820F-4845-A4B8-6D6963056661}']

    //property methods
    function GetVAlign: TElementVertAlignment;
    procedure SetVAlign(const AValue: TElementVertAlignment);
    procedure SetHAlign(const AValue: TElementHorzAlignment);
    function GetHAlign: TElementHorzAlignment;

    //properties

    (*
      controls how the calculation should be performed to align an element horizontally
    *)
    property HorzAlignment : TElementHorzAlignment read GetHAlign write SetHAlign;

    (*
      controls how the calculation should be performed to align an element vertically
    *)
    property VertAlignment : TElementVertAlignment read GetVAlign write SetVAlign;

    //methods

    (*
      fluent method for updating the horz alignment
    *)
    function UpdateHorzAlignment(const AValue : TElementHorzAlignment) : INyxBounds;

    (*
      fluent method for updating the vert alignment
    *)
    function UpdateVertAlignment(const AValue : TElementVertAlignment) : INyxBounds;
  end;

  { INyxFixedBounds }
  (*
    bounds controlling how an element should be position inside of
    a parent container with fixed units
  *)
  INyxFixedBounds = interface(INyxBounds)
    ['{89FD2A4F-1B32-43BB-BFA6-2E71BA9334C9}']

    //property methods
    function GetLeft: Integer;
    function GetTop: Integer;
    procedure SetLeft(const AValue: Integer);
    procedure SetTop(const AValue: Integer);

    //properties

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
      fluent method for updating the left position
    *)
    function UpdateLeft(const AValue : Integer) : INyxFixedBounds;

    (*
      fluent method for updating the top position
    *)
    function UpdateTop(const AValue : Integer) : INyxFixedBounds;
  end;

  { TNyxBoundsImpl }
  (*
    base bounds implementation
  *)
  TNyxBoundsImpl = class(TNyxElementBaseImpl, INyxBounds)
  strict private
    FHAlign: TElementHorzAlignment;
    FVAlign: TElementVertAlignment;
  strict protected
  protected
    function GetHAlign: TElementHorzAlignment;
    function GetVAlign: TElementVertAlignment;
    procedure SetHAlign(const AValue: TElementHorzAlignment);
    procedure SetVAlign(const AValue: TElementVertAlignment);
  public
    property HorzAlignment : TElementHorzAlignment read GetHAlign write SetHAlign;
    property VertAlignment : TElementVertAlignment read GetVAlign write SetVAlign;

    function UpdateHorzAlignment(const AValue : TElementHorzAlignment) : INyxBounds;
    function UpdateVertAlignment(const AValue : TElementVertAlignment) : INyxBounds;

    constructor Create; override;
  end;

  { TNyxFixedBoundsImpl }
  (*
    base fixed bounds implementation
  *)
  TNyxFixedBoundsImpl = class(TNyxBoundsImpl, INyxFixedBounds)
  strict private
    FLeft,
    FTop: Integer;
  strict protected
  protected
    function GetLeft: Integer;
    function GetTop: Integer;
    procedure SetLeft(const AValue: Integer);
    procedure SetTop(const AValue: Integer);
  public
    property Left : Integer read GetLeft write SetLeft;
    property Top : Integer read GetTop write SetTop;

    function UpdateLeft(const AValue : Integer) : INyxFixedBounds;
    function UpdateTop(const AValue : Integer) : INyxFixedBounds;

    constructor Create; override;
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

  { TNyxLayoutBaseImpl }
  (*
    base implementation for all INyxLayout
  *)
  TNyxLayoutBaseImpl = class(TNyxContainerBaseImpl, INyxLayout)
  strict private
  protected
  strict protected
    function DoUpdatePlacement(out Error : String) : Boolean; virtual; abstract;

    procedure DoUpdateElementParent(const AElement: INyxElement); override;
  public
    function UpdatePlacement(out Error : String) : Boolean; overload;
    function UpdatePlacement : Boolean; overload;
  end;

  //meta class for concrete nyx layouts
  TNyxLayoutClass = class of TNyxLayoutBaseImpl;

  { TNyxLayoutFixedImpl }
  (*
    base implementation for all fixed layouts
  *)
  TNyxLayoutFixedImpl = class(TNyxLayoutBaseImpl, INyxLayoutFixed)
  strict private
    FBounds : INyxElements;
    FRemoveBoundsID : String;

    procedure RemoveBounds(const AElement : INyxElement;
      const AEvent : TElementsObserveEvent);
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

  (*
    metaclass for fixed layouts
  *)
  TNyxLayoutFixedClass = class of TNyxLayoutFixedImpl;

  { INyxProportionalBounds }

  INyxProportionalBounds = interface(INyxBounds)
    ['{B0A4EDF1-6580-4CD5-AABC-F7CB808904DD}']

    //property methods
    function GetLeft: Double;
    function GetTop: Double;
    procedure SetLeft(const AValue: Double);
    procedure SetTop(const AValue: Double);

    //properties

    (*
      controls horizontal position of the element, where 0 (0%) would be the
      "farthest left" inside of the parent and 1.0 (100%) would be the "farthest right"
    *)
    property Left : Double read GetLeft write SetLeft;

    (*
      controls vertical position of the element, where 0 (0%) would be the
      "farthest top" inside of the parent and 1.0 (100%) would
      be the "farthest bottom"
    *)
    property Top : Double read GetTop write SetTop;

    //methods
    function UpdateLeft(const AValue : Double) : INyxProportionalBounds;
    function UpdateTop(const AValue : Double) : INyxProportionalBounds;
  end;

  { TNyxProportionalBoundsImpl }
  (*
    base proportional bounds implementation
  *)
  TNyxProportionalBoundsImpl = class(TNyxBoundsImpl, INyxProportionalBounds)
  strict private
    FLeft,
    FTop: Double;
  strict protected
  protected
    function GetLeft: Double;
    function GetTop: Double;
    procedure SetLeft(const AValue: Double);
    procedure SetTop(const AValue: Double);
  public
    property Left : Double read GetLeft write SetLeft;
    property Top : Double read GetTop write SetTop;

    function UpdateLeft(const AValue : Double) : INyxProportionalBounds;
    function UpdateTop(const AValue : Double) : INyxProportionalBounds;

    constructor Create; override;
  end;

  { INyxLayoutProportional }
  (*
    a layout to control the placement of elements via percentages
  *)
  INyxLayoutProportional = interface(INyxLayout)
    ['{C649AA5F-2D1F-40A0-93CC-D5925B20AC12}']
    //property methods
    function GetBounds(const AElement : INyxElement): INyxProportionalBounds;

    //properties

    (*
      accessor for the bounds of a given element
    *)
    property Bounds[const AElement : INyxElement] : INyxProportionalBounds read GetBounds; default;

    //methods

    (*
      adds an element to have it's placement inside of it's parent container
      using fixed set of bounds
    *)
    function Add(const AElement : INyxElement;
      const ABounds : INyxProportionalBounds) : INyxLayoutProportional; overload;

    (*
      removes an element from this layout
      this method is the same as accessing the element collection and removing
      from their, however this scopes back to the specialized fixed layout
    *)
    function Remove(const AElement : INyxElement) : INyxLayoutProportional;
  end;

  { TNyxLayoutProportionalImpl }
  (*
    base class for proportional layouts
  *)
  TNyxLayoutProportionalImpl = class(TNyxLayoutBaseImpl, INyxLayoutProportional)
  strict private
    FBounds : INyxElements;
    FRemoveBoundsID : String;

    procedure RemoveBounds(const AElement : INyxElement;
      const AEvent : TElementsObserveEvent);
  strict protected
    function DoUpdatePlacement(out Error: String): Boolean; override;

    (*
      children proportional layouts need to override this in order to place an
      element within their parent container provided the bounds
    *)
    function DoPlaceElement(const AElement : INyxElement;
      const ABounds : INyxProportionalBounds; out Error : String) : Boolean; virtual; abstract;
  protected
    function GetBounds(const AElement : INyxElement): INyxProportionalBounds;
  public
    property Bounds[const AElement : INyxElement] : INyxProportionalBounds read GetBounds; default;

    function Add(const AElement : INyxElement;
      const ABounds : INyxProportionalBounds) : INyxLayoutProportional; overload;
    function Remove(const AElement : INyxElement) : INyxLayoutProportional;

    constructor Create; override;
    destructor Destroy; override;
  end;

  (*
    metaclass for proportional layouts
  *)
  TNyxLayoutProportionalClass = class of TNyxLayoutProportionalImpl;

  { INyxRelationalBounds }

  INyxRelationalBounds = interface(INyxBounds)
    ['{45315432-0E9F-47C6-A03F-6FBCE619A994}']

    //property methods
    function GetLeft: Double;
    function GetTop: Double;
    procedure SetLeft(const AValue: Double);
    procedure SetTop(const AValue: Double);

    //properties

    (*
      controls horizontal position of the element, where 0 (0%) would be the
      "farthest left" inside of the parent and 1.0 (100%) would be the "farthest right"
    *)
    property Left : Double read GetLeft write SetLeft;

    (*
      controls vertical position of the element, where 0 (0%) would be the
      "farthest top" inside of the parent and 1.0 (100%) would
      be the "farthest bottom"
    *)
    property Top : Double read GetTop write SetTop;

    //methods
    function UpdateLeft(const AValue : Double) : INyxRelationalBounds;
    function UpdateTop(const AValue : Double) : INyxRelationalBounds;
  end;

  { TNyxRelationalBoundsImpl }
  (*
    base Relational bounds implementation
  *)
  TNyxRelationalBoundsImpl = class(TNyxBoundsImpl, INyxRelationalBounds)
  strict private
    FLeft,
    FTop: Double;
  strict protected
  protected
    function GetLeft: Double;
    function GetTop: Double;
    procedure SetLeft(const AValue: Double);
    procedure SetTop(const AValue: Double);
  public
    property Left : Double read GetLeft write SetLeft;
    property Top : Double read GetTop write SetTop;

    function UpdateLeft(const AValue : Double) : INyxRelationalBounds;
    function UpdateTop(const AValue : Double) : INyxRelationalBounds;

    constructor Create; override;
  end;

  { INyxLayoutRelational }
  (*
    a layout to control the placement of elements via percentages
  *)
  INyxLayoutRelational = interface(INyxLayout)
    ['{9909AA2C-989B-499D-907D-25AD2BD6F1BD}']

    //property methods
    function GetBounds(const AElement : INyxElement): INyxRelationalBounds;

    //properties

    (*
      accessor for the bounds of a given element
    *)
    property Bounds[const AElement : INyxElement] : INyxRelationalBounds read GetBounds; default;

    //methods

    (*
      adds an element to have it's placement inside of it's parent container
      using using an "anchor" element as the point of reference
    *)
    function Add(const AElement, AAnchor : INyxElement;
      const ABounds : INyxRelationalBounds) : INyxLayoutRelational; overload;

    (*
      removes an element from this layout
      this method is the same as accessing the element collection and removing
      from their, however this scopes back to the specialized fixed layout
    *)
    function Remove(const AElement : INyxElement) : INyxLayoutRelational;
  end;

  { TNyxLayoutRelationalImpl }
  (*
    base class for relational layouts
  *)
  TNyxLayoutRelationalImpl = class(TNyxLayoutBaseImpl, INyxLayoutRelational)
  strict private
    FBounds,
    FAnchors: INyxElements;
    FAnchorMap : TStringList;
    FRemoveBoundsID : String;

    procedure RemoveBounds(const AElement : INyxElement;
      const AEvent : TElementsObserveEvent);
  strict protected
    function DoUpdatePlacement(out Error: String): Boolean; override;

    (*
      children relational layouts need to override this in order to place an
      element within their parent container provided the bounds
    *)
    function DoPlaceElement(const AElement : INyxElement;
      const ABounds : INyxRelationalBounds; out Error : String) : Boolean; virtual; abstract;
  protected
    function GetBounds(const AElement : INyxElement): INyxRelationalBounds;

    (*
      children can call this method to retrieve the anchor element
      associated with element. will return false if not found
    *)
    function GetAnchor(const AElement : INyxElement;
      out AAnchor : INyxElement): Boolean;
  public
    property Bounds[const AElement : INyxElement] : INyxRelationalBounds read GetBounds; default;

    function Add(const AElement, AAnchor : INyxElement;
      const ABounds : INyxRelationalBounds) : INyxLayoutRelational; overload;
    function Remove(const AElement : INyxElement) : INyxLayoutRelational;

    constructor Create; override;
    destructor Destroy; override;
  end;

  (*
    metaclass for relational layouts
  *)
  TNyxLayoutRelationalClass = class of TNyxLayoutRelationalImpl;

(*
  helper function to return a fixed layout
*)
function NewNyxLayoutFixed : INyxLayoutFixed;

(*
  helper function to return a fixed bounds
*)
function NewNyxFixedBounds : INyxFixedBounds;

(*
  helper function to return a proportional bounds
*)
function NewNyxProportionalBounds : INyxProportionalBounds;

(*
  helper function to return a proportional layout
*)
function NewNyxLayoutProportional : INyxLayoutProportional;

(*
  helper function to return a relational bounds
*)
function NewNyxRelationalBounds : INyxRelationalBounds;

(*
  helper function to return a relational layout
*)
function NewNyxLayoutRelational : INyxLayoutRelational;

implementation
uses
{$IFDEF BROWSER}
  nyx.layout.fixed.browser,
  nyx.layout.proportional.browser,
nyx.layout.relational.browser;
{$ELSE}
  nyx.layout.fixed.std;
{$ENDIF}

var
  DefaultNyxLayoutFixed : TNyxLayoutFixedClass;
  DefaultNyxLayoutProportional : TNyxLayoutProportionalClass;
  DefaultNyxLayoutRelational : TNyxLayoutRelationalClass;

function NewNyxLayoutFixed: INyxLayoutFixed;
begin
  Result := DefaultNyxLayoutFixed.Create;
end;

function NewNyxFixedBounds: INyxFixedBounds;
begin
  Result := TNyxFixedBoundsImpl.Create;
end;

function NewNyxProportionalBounds: INyxProportionalBounds;
begin
  Result := TNyxProportionalBoundsImpl.Create;
end;

function NewNyxLayoutProportional: INyxLayoutProportional;
begin
  Result := DefaultNyxLayoutProportional.Create;
end;

function NewNyxRelationalBounds: INyxRelationalBounds;
begin
  Result := TNyxRelationalBoundsImpl.Create;
end;

function NewNyxLayoutRelational: INyxLayoutRelational;
begin
  Result := DefaultNyxLayoutRelational.Create;
end;

{ TNyxLayoutBaseImpl }

procedure TNyxLayoutBaseImpl.DoUpdateElementParent(const AElement: INyxElement);
begin
  //since we hijacked the container impl we need to not parent here
  //so this is an empty method on purpose
end;

function TNyxLayoutBaseImpl.UpdatePlacement(out Error: String): Boolean;
begin
  Result := DoUpdatePlacement(Error);
end;

function TNyxLayoutBaseImpl.UpdatePlacement: Boolean;
var
  LError : String;
begin
  Result := UpdatePlacement(LError);
end;

{ TNyxLayoutRelationalImpl }

procedure TNyxLayoutRelationalImpl.RemoveBounds(const AElement: INyxElement;
  const AEvent: TElementsObserveEvent);
var
  I: Integer;
  LAnchor: INyxElement;
begin
  //use the element to find the bounds and remove if exists
  FBounds.IndexOf(AElement, I);

  if (AEvent = eoExtract) and (I >= 0) then
  begin
    //on element remove, also remove the anchor associated (if any)
    if GetAnchor(AElement, LAnchor) then
      FAnchors.Delete(LAnchor);

    FBounds.Delete(AElement);
  end;
end;

function TNyxLayoutRelationalImpl.DoUpdatePlacement(out Error: String): Boolean;
var
  I, J: Integer;
  LElement: INyxElement;
  LBound: INyxRelationalBounds;
begin
  try
    Result := False;

    //iterate elements to call down and place each element
    for I := 0 to Pred(Elements.Count) do
    begin
      LElement := Elements[I];
      FBounds.IndexOf(LElement, J);

      //only call the place method when there is a bounds provided, otherwise
      //the caller wants the default placement
      if J >= 0 then
      begin
        LBound := FBounds.Items[J] as INyxRelationalBounds;

        //if we can't place an element bail with the error
        if not DoPlaceElement(LElement, LBound, Error) then
          Exit;
      end;
    end;

    //success
    Result := True;
  except on E : Exception do
    Error := E.Message;
  end;
end;

function TNyxLayoutRelationalImpl.GetBounds(const AElement: INyxElement): INyxRelationalBounds;
var
  I: Integer;
begin
  Result := nil;

  //we use an element collection to store bounds with the same id as the element
  //so we can use the input as lookup
  FBounds.IndexOf(AElement, I);

  if not I >= 0 then
    Exit;

  Result := FBounds[I] as  INyxRelationalBounds;
end;

function TNyxLayoutRelationalImpl.GetAnchor(const AElement: INyxElement; out
  AAnchor: INyxElement): Boolean;
var
  I: Integer;
  LID: String;

  function FindAnchor(const AElement : INyxElement) : Boolean;
  begin
    if AElement.ID = LID then
      Result := True
    else
      Exit(False);
  end;

begin
  Result := False;

  if not Assigned(AElement) then
    Exit;

  //find the index of the key
  I := FAnchorMap.IndexOfName(AElement.ID);

  if I < 0 then
    Exit;

  //now get the id of anchor for lookup in the anchor collection
  LID := FAnchorMap.ValueFromIndex[I];

  AAnchor := FAnchors.Find(@FindAnchor);
  Result := Assigned(AAnchor);
end;

function TNyxLayoutRelationalImpl.Add(const AElement, AAnchor: INyxElement;
  const ABounds: INyxRelationalBounds): INyxLayoutRelational;
begin
  Result := Self as INyxLayoutRelational;

  //add the element normally
  Add(AElement);

  //when the element is valid, update the id and add
  if Assigned(AElement) and Assigned(ABounds) then
  begin
    ABounds.ID := AElement.ID; //used for lookup
    FBounds.Add(ABounds);
  end;

  //now for the anchors, we need to use a map instead of updating the id
  //since we don't control the anchor element, just reference it
  if Assigned(AElement) and Assigned(AAnchor) then
  begin
    FAnchors.Add(AAnchor);

    //use the element as the "key" and the anchor as the "value"
    FAnchorMap.AddPair(AElement.ID, AAnchor.ID);
  end;
end;

function TNyxLayoutRelationalImpl.Remove(const AElement: INyxElement): INyxLayoutRelational;
begin
  Result := Self as INyxLayoutRelational;
  Elements.Delete(AElement);
end;

constructor TNyxLayoutRelationalImpl.Create;
begin
  inherited Create;
  FBounds := NewNyxElements;
  FAnchors := NewNyxElements;
  FAnchorMap := TStringList.Create;

  //add an observer to remove the bounds when an element is removed
  Elements.Observe(@RemoveBounds, FRemoveBoundsID);
end;

destructor TNyxLayoutRelationalImpl.Destroy;
begin
  //remove the observer since we're about to clear the bounds
  Elements.RemoveObserver(FRemoveBoundsID);
  FBounds.Clear;
  FAnchors.Clear;
  FAnchorMap.Free;
  FBounds := nil;
  FAnchors := nil;
  inherited Destroy;
end;

{ TNyxRelationalBoundsImpl }

function TNyxRelationalBoundsImpl.GetLeft: Double;
begin
  Result := FLeft;
end;

function TNyxRelationalBoundsImpl.GetTop: Double;
begin
  Result := FTop;
end;

procedure TNyxRelationalBoundsImpl.SetLeft(const AValue: Double);
begin
  FLeft := AValue;
end;

procedure TNyxRelationalBoundsImpl.SetTop(const AValue: Double);
begin
  FTop := AValue;
end;

function TNyxRelationalBoundsImpl.UpdateLeft(const AValue: Double): INyxRelationalBounds;
begin
  Result := Self as INyxRelationalBounds;
  Left := AValue;
end;

function TNyxRelationalBoundsImpl.UpdateTop(const AValue: Double): INyxRelationalBounds;
begin
  Result := Self as INyxRelationalBounds;
  Top := AValue;
end;

constructor TNyxRelationalBoundsImpl.Create;
begin
  inherited Create;
  FLeft := 0;
  FTop := 0;
end;

{ TNyxLayoutProportionalImpl }

procedure TNyxLayoutProportionalImpl.RemoveBounds(const AElement: INyxElement;
  const AEvent: TElementsObserveEvent);
var
  I: Integer;
begin
  //use the element to find the bounds and remove if exists
  FBounds.IndexOf(AElement, I);

  if (AEvent = eoExtract) and (I >= 0) then
    FBounds.Delete(AElement);
end;

function TNyxLayoutProportionalImpl.DoUpdatePlacement(out Error: String): Boolean;
var
  I, J: Integer;
  LElement: INyxElement;
  LBound: INyxProportionalBounds;
begin
  try
    Result := False;

    //iterate elements to call down and place each element
    for I := 0 to Pred(Elements.Count) do
    begin
      LElement := Elements[I];
      FBounds.IndexOf(LElement, J);

      //only call the place method when there is a bounds provided, otherwise
      //the caller wants the default placement
      if J >= 0 then
      begin
        LBound := FBounds.Items[J] as INyxProportionalBounds;

        //if we can't place an element bail with the error
        if not DoPlaceElement(LElement, LBound, Error) then
          Exit;
      end;
    end;

    //success
    Result := True;
  except on E : Exception do
    Error := E.Message;
  end;
end;

function TNyxLayoutProportionalImpl.GetBounds(const AElement: INyxElement): INyxProportionalBounds;
var
  I: Integer;
begin
  Result := nil;

  //we use an element collection to store bounds with the same id as the element
  //so we can use the input as lookup
  FBounds.IndexOf(AElement, I);

  if not I >= 0 then
    Exit;

  Result := FBounds[I] as INyxProportionalBounds;
end;

function TNyxLayoutProportionalImpl.Add(const AElement: INyxElement;
  const ABounds: INyxProportionalBounds): INyxLayoutProportional;
var
  LBounds : INyxProportionalBounds;
begin
  Result := Self as INyxLayoutProportional;
  LBounds := ABounds;

  //add the element normally
  Add(AElement);

  //when the element is valid, update the id and add
  if Assigned(AElement) and Assigned(LBounds) then
  begin
    ABounds.ID := AElement.ID; //used for lookup
    FBounds.Add(LBounds);
  end;
end;

function TNyxLayoutProportionalImpl.Remove(const AElement: INyxElement): INyxLayoutProportional;
begin
  Result := Self as INyxLayoutProportional;
  Elements.Delete(AElement);
end;

constructor TNyxLayoutProportionalImpl.Create;
begin
  inherited Create;
  FBounds := NewNyxElements;

  //add an observer to remove the bounds when an element is removed
  Elements.Observe(@RemoveBounds, FRemoveBoundsID);
end;

destructor TNyxLayoutProportionalImpl.Destroy;
begin
  //remove the observer since we're about to clear the bounds
  Elements.RemoveObserver(FRemoveBoundsID);
  FBounds.Clear;
  FBounds := nil;
  inherited Destroy;
end;

{ TNyxProportionalBoundsImpl }

function TNyxProportionalBoundsImpl.GetLeft: Double;
begin
  Result := FLeft;
end;

function TNyxProportionalBoundsImpl.GetTop: Double;
begin
  Result := FTop;
end;

procedure TNyxProportionalBoundsImpl.SetLeft(const AValue: Double);
begin
  FLeft := AValue;
end;

procedure TNyxProportionalBoundsImpl.SetTop(const AValue: Double);
begin
  FTop := AValue;
end;

function TNyxProportionalBoundsImpl.UpdateLeft(const AValue: Double): INyxProportionalBounds;
begin
  Result := Self as INyxProportionalBounds;
  Left := AValue;
end;

function TNyxProportionalBoundsImpl.UpdateTop(const AValue: Double): INyxProportionalBounds;
begin
  Result := Self as INyxProportionalBounds;
  Top := AValue;
end;

constructor TNyxProportionalBoundsImpl.Create;
begin
  inherited Create;
  FLeft := 0;
  FTop := 0;
end;

{ TNyxBoundsImpl }

function TNyxBoundsImpl.GetHAlign: TElementHorzAlignment;
begin
  Result := FHAlign;
end;

function TNyxFixedBoundsImpl.GetLeft: Integer;
begin
  Result := FLeft;
end;

function TNyxFixedBoundsImpl.GetTop: Integer;
begin
  Result := FTop;
end;

function TNyxBoundsImpl.GetVAlign: TElementVertAlignment;
begin
  Result := FVAlign;
end;

procedure TNyxBoundsImpl.SetHAlign(const AValue: TElementHorzAlignment);
begin
  FHAlign := AValue;
end;

procedure TNyxFixedBoundsImpl.SetLeft(const AValue: Integer);
begin
  FLeft := AValue;
end;

procedure TNyxFixedBoundsImpl.SetTop(const AValue: Integer);
begin
  FTop := AValue;
end;

procedure TNyxBoundsImpl.SetVAlign(const AValue: TElementVertAlignment);
begin
  FVAlign := AValue;
end;

function TNyxBoundsImpl.UpdateHorzAlignment(const AValue: TElementHorzAlignment): INyxBounds;
begin
  Result := Self as INyxBounds;
  HorzAlignment := AValue;
end;

function TNyxBoundsImpl.UpdateVertAlignment(const AValue: TElementVertAlignment): INyxBounds;
begin
  Result := Self as INyxBounds;
  VertAlignment := AValue;
end;

function TNyxFixedBoundsImpl.UpdateLeft(const AValue: Integer): INyxFixedBounds;
begin
  Result := Self as INyxFixedBounds;
  Left := AValue;
end;

function TNyxFixedBoundsImpl.UpdateTop(const AValue: Integer): INyxFixedBounds;
begin
  Result := Self as INyxFixedBounds;
  Top := AValue;
end;

constructor TNyxFixedBoundsImpl.Create;
begin
  inherited Create;
  FLeft := 0;
  FTop := 0;
end;

constructor TNyxBoundsImpl.Create;
begin
  inherited Create;
  FHAlign := haLeft;
  FVAlign := vaTop;
end;

{ TNyxLayoutFixedImpl }

procedure TNyxLayoutFixedImpl.RemoveBounds(const AElement: INyxElement;
  const AEvent: TElementsObserveEvent);
var
  I: Integer;
begin
  //use the element to find the bounds and remove if exists
  FBounds.IndexOf(AElement, I);

  if (AEvent = eoExtract) and (I >= 0) then
    FBounds.Delete(AElement);
end;

function TNyxLayoutFixedImpl.DoUpdatePlacement(out Error: String): Boolean;
var
  I, J: Integer;
  LElement: INyxElement;
  LBound: INyxFixedBounds;
begin
  try
    Result := False;

    //iterate elements to call down and place each element
    for I := 0 to Pred(Elements.Count) do
    begin
      LElement := Elements[I];
      FBounds.IndexOf(LElement, J);

      //only call the place method when there is a bounds provided, otherwise
      //the caller wants the default placement
      if J >= 0 then
      begin
        LBound := FBounds.Items[J] as INyxFixedBounds;

        //if we can't place an element bail with the error
        if not DoPlaceElement(LElement, LBound, Error) then
          Exit;
      end;
    end;

    //success
    Result := True;
  except on E : Exception do
    Error := E.Message;
  end;
end;

function TNyxLayoutFixedImpl.GetBounds(const AElement: INyxElement): INyxFixedBounds;
var
  I: Integer;
begin
  Result := nil;

  //we use an element collection to store bounds with the same id as the element
  //so we can use the input as lookup
  FBounds.IndexOf(AElement, I);

  if not I >= 0 then
    Exit;

  Result := FBounds[I] as INyxFixedBounds;
end;

function TNyxLayoutFixedImpl.Add(const AElement: INyxElement;
  const ABounds: INyxFixedBounds): INyxLayoutFixed;
var
  LBounds: INyxFixedBounds;
begin
  Result := Self as INyxLayoutFixed;
  LBounds := ABounds;

  //add the element normally
  Add(AElement);

  //when the element is valid, update the id and add
  if Assigned(AElement) and Assigned(ABounds) then
  begin
    ABounds.ID := AElement.ID; //used for lookup
    FBounds.Add(ABounds);
  end;
end;

function TNyxLayoutFixedImpl.Remove(const AElement: INyxElement): INyxLayoutFixed;
begin
  Result := Self as INyxLayoutFixed;
  Elements.Delete(AElement);
end;

constructor TNyxLayoutFixedImpl.Create;
begin
  inherited Create;
  FBounds := NewNyxElements;

  //add an observer to remove the bounds when an element is removed
  Elements.Observe(@RemoveBounds, FRemoveBoundsID);
end;

destructor TNyxLayoutFixedImpl.Destroy;
begin
  //remove the observer since we're about to clear the bounds
  Elements.RemoveObserver(FRemoveBoundsID);
  FBounds.Clear;
  FBounds := nil;
  inherited Destroy;
end;

initialization
{$IFDEF BROWSER}
  DefaultNyxLayoutFixed := TNyxLayoutFixedBrowserImpl;
  DefaultNyxLayoutProportional := TNyxLayoutProportionalBrowserImpl;
  DefaultNyxLayoutRelational := TNyxLayoutRelationalBrowserImpl;
{$ELSE}
  DefaultNyxLayoutFixed := TNyxLayoutFixedStdImpl;
  DefaultNyxLayoutProportional := TNyxLayoutProportionalStdImpl;
{$ENDIF}
end.

