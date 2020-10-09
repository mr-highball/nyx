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
unit nyx.size;

{$mode delphi}

interface

uses
  nyx.types,
  nyx.utils.observe;

type

  { TNyxSizeImpl }
  (*
    base implementation for INyxSize
  *)
  TNyxSizeImpl = class(TInterfacedObject, INyxSize)
  strict private
    FObserve : TNyxObservationHelper;
    FHeight,
    FWidth : Double;
    FMode : TSizeMode;
    FElement : INyxElement;
    procedure DoPropertyNotify(const AType : TPropertyUpdateType;
      const AProperty : TSizeProperty);
  protected
    function GetElement: INyxElement;
    function GetHeight: Double;
    function GetMode: TSizeMode;
    function GetWidth: Double;
    procedure SetHeight(const AValue: Double);
    procedure SetMode(const AValue: TSizeMode);
    procedure SetWidth(const AValue: Double);
    procedure SetElement(const AValue: INyxElement);
  strict protected
  public
    property Height : Double read GetHeight write SetHeight;
    property Width : Double read GetWidth write SetWidth;
    property Mode : TSizeMode read GetMode write SetMode;
    property Element : INyxElement read GetElement write SetElement;

    function Observe(const AEvent : TSizeProperty;
      const AObserver : TSizePropertyObserveMethod; out ID : String) : INyxSize;
    function RemoveObserver(const AID : String) : INyxSize;
    function UpdateHeight(const AHeight : Double) : INyxSize;
    function UpdateWidth(const AWidth : Double) : INyxSize;
    function UpdateMode(const AMode : TSizeMode) : INyxSize;

    constructor Create; virtual;
    destructor Destroy; override;
  end;

  //metaclass for nyx sizes
  TNyxSizeClass = class of TNyxSizeImpl;

implementation

{ TNyxSizeImpl }

function TNyxSizeImpl.GetElement: INyxElement;
begin
  Result := FElement;
end;

function TNyxSizeImpl.GetHeight: Double;
begin
  Result := FHeight;
end;

function TNyxSizeImpl.GetMode: TSizeMode;
begin
  Result := FMode;
end;

function TNyxSizeImpl.GetWidth: Double;
begin
  Result := FWidth;
end;

procedure TNyxSizeImpl.SetHeight(const AValue: Double);
begin
  //before notifty
  DoPropertyNotify(puBeforeUpdate, scHeight);

  FHeight := AValue;

  //after notify
  DoPropertyNotify(puAfterUpdate, scHeight);
end;

procedure TNyxSizeImpl.SetMode(const AValue: TSizeMode);
begin
  //before notifty
  DoPropertyNotify(puBeforeUpdate, scMode);

  FMode := AValue;

  //after notify
  DoPropertyNotify(puAfterUpdate, scMode);
end;

procedure TNyxSizeImpl.SetWidth(const AValue: Double);
begin
  //before notifty
  DoPropertyNotify(puBeforeUpdate, scWidth);

  FWidth := AValue;

  //after notify
  DoPropertyNotify(puAfterUpdate, scWidth);
end;

procedure TNyxSizeImpl.SetElement(const AValue: INyxElement);
begin
  //before notifty
  DoPropertyNotify(puBeforeUpdate, scElement);

  FElement := AValue;

  //after notify
  DoPropertyNotify(puAfterUpdate, scElement);
end;

procedure TNyxSizeImpl.DoPropertyNotify(const AType: TPropertyUpdateType;
  const AProperty: TSizeProperty);
var
  LMethod: TSizePropertyObserveMethod;
  I: Integer;
  LSize: INyxSize;
  LObservers: TObserverArray;
begin
  LSize := Self as INyxSize;
  LObservers := FObserve.ObserversByEvent(Ord(AProperty));

  for I := 0 to High(LObservers) do
    try
      LMethod := TSizePropertyObserveMethod(LObservers[I]);

      //call the method
      LMethod(AType, LSize, AProperty);
    finally
    end;
end;

function TNyxSizeImpl.Observe(const AEvent: TSizeProperty;
  const AObserver: TSizePropertyObserveMethod; out ID: String): INyxSize;
begin
  Result := Self as INyxSize;

  if not Assigned(AObserver) then
    Exit;

  ID := FObserve.Observe(Ord(AEvent), Pointer(AObserver));
end;

function TNyxSizeImpl.RemoveObserver(const AID: String): INyxSize;
begin
  Result := Self as INyxSize;
  FObserve.RemoveByID(AID);
end;

function TNyxSizeImpl.UpdateHeight(const AHeight: Double): INyxSize;
begin
  Result := Self as INyxSize;
  SetHeight(AHeight);
end;

function TNyxSizeImpl.UpdateWidth(const AWidth: Double): INyxSize;
begin
  Result := Self as INyxSize;
  SetWidth(AWidth);
end;

function TNyxSizeImpl.UpdateMode(const AMode: TSizeMode): INyxSize;
begin
  Result := Self as INyxSize;
  SetMode(AMode);
end;

constructor TNyxSizeImpl.Create;
begin
  inherited Create;
  FHeight := -1;
  FWidth := -1;
  FObserve := TNyxObservationHelper.Create;
end;

destructor TNyxSizeImpl.Destroy;
begin
  FObserve.Free;
  inherited Destroy;
end;

end.

