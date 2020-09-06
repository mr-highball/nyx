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
unit nyx.element;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  nyx.types,
  nyx.utils.observe;

type

  { TNyxElementBaseImpl }
  (*
    base implementation class for all INyxElement
  *)
  TNyxElementBaseImpl = class(TInterfacedObject, INyxElement)
  strict private
    FID,
    FName : String;
    FContainer : INyxContainer;
    FSizeIDs : TStringList;
    FSize : INyxSize;
    FObserve,
    FPropObserve: TNyxObservationHelper;
    function GetSize: INyxSize;
    procedure SetSize(const AValue: INyxSize);
    procedure RemoveSizeObservers;

    procedure NotifyHeight(const AType : TPropertyUpdateType;
      const ASize : INyxSize; const AProperty : TSizeProperty);
    procedure NotifyWidth(const AType : TPropertyUpdateType;
      const ASize : INyxSize; const AProperty : TSizeProperty);
    procedure NotifyMode(const AType : TPropertyUpdateType;
      const ASize : INyxSize; const AProperty : TSizeProperty);
    procedure NotifyElement(const AType : TPropertyUpdateType;
      const ASize : INyxSize; const AProperty : TSizeProperty);

    procedure PropertyNotify(const AType : TPropertyUpdateType;
      const AProperty : TElementProperty);
  protected
    procedure SetID(const AValue: String);
    function GetID: String;
    function GetName: String;
    procedure SetName(const AValue: String);
    function GetContainer: INyxContainer;
    procedure SetContainer(const AValue: INyxContainer);
  strict protected

    procedure DoSetID(const AValue: String); virtual;
    function DoGetID: String; virtual;
    function DoGetName: String; virtual;
    procedure DoSetName(const AValue: String); virtual;
    function DoGetContainer: INyxContainer; virtual;
    procedure DoSetContainer(const AValue: INyxContainer); virtual;

    (*
      children can override this method to generate a unique identifier
      if the default generation will not suffice
    *)
    function DoGenerateID : String; virtual;

    (*
      when a valid size is assigned to this element, this method
      is called afterwards for parenting and any other setup involved
    *)
    procedure DoParentSize(const ASize : INyxSize); virtual;

    (*
      size method called when height has been changed
    *)
    procedure DoUpdateHeight; virtual;

    (*
      size method called when width has been changed
    *)
    procedure DoUpdateWidth; virtual;

    (*
      size method called when mode has been changed
    *)
    procedure DoUpdateMode; virtual;

    (*
      remove an observer for a given id
    *)
    procedure DoRemoveObserver(const AID : String); virtual;

    (*
      called by children to notify on an event
    *)
    procedure Notify(const AEvent : TElementEvent);

    (*
      can be overridden to change the return for fluent method, property
      and event notifications
    *)
    function DoGetSelf : INyxElement; virtual;
  public
    property ID : String read GetID write SetID;
    property Name : String read GetName write SetName;
    property Container : INyxContainer read GetContainer write SetContainer;
    property Size : INyxSize read GetSize write SetSize;

    function Observe(const AEvent : TElementEvent; const AObserver : TElementObserveMethod;
      out ID : String) : INyxElement; overload;
    function Observe(const AProperty : TElementProperty;
      const AObserver : TElementPropertyObserveMethod; out ID : String) : INyxElement; overload;

    function RemoveObserver(const AID : String) : INyxElement;

    function UpdateName(const AName : String) : INyxElement;
    function UpdateSize(const ASize : INyxSize) : INyxElement;
    function UpdateContainer(const AContainer : INyxContainer) : INyxElement;
    function UpdateID(const AID : String) : INyxElement;

    function Condition(const ACondition : Boolean; const ATrue, AFalse : TNyxElementCallback) : INyxElement; overload;
    function Condition(const ACondition : Boolean; const ATrue, AFalse : TNyxElementNestedCallback) : INyxElement; overload;
    function Condition(const ACondition : Boolean; const ATrue, AFalse : TNyxElementMethod) : INyxElement; overload;

    function Condition(const ACondition : TNyxElementBoolCallback; const ATrue, AFalse : TNyxElementCallback) : INyxElement; overload;
    function Condition(const ACondition : TNyxElementBoolNestedCallback; const ATrue, AFalse : TNyxElementNestedCallback) : INyxElement; overload;
    function Condition(const ACondition : TNyxElementBoolMethod; const ATrue, AFalse : TNyxElementMethod) : INyxElement; overload;

    constructor Create; virtual;
    destructor Destroy; override;
  end;

  (*
    metaclass for a nyx element
  *)
  TNyxElementClass = class of TNyxElementBaseImpl;

implementation

{ TNyxElementBaseImpl }

function TNyxElementBaseImpl.GetSize: INyxSize;
begin
  Result := FSize;
end;

procedure TNyxElementBaseImpl.SetSize(const AValue: INyxSize);
begin
  //first remove observers if we have any
  RemoveSizeObservers;

  FSize := AValue;

  if Assigned(AValue) then
    DoParentSize(AValue);
end;

procedure TNyxElementBaseImpl.RemoveSizeObservers;
var
  I: Integer;
begin
  if not Assigned(FSize) then
    Exit;

  for I := 0 to Pred(FSizeIDs.Count) do
    FSize.RemoveObserver(FSizeIDs[I]);
end;

procedure TNyxElementBaseImpl.NotifyHeight(const AType: TPropertyUpdateType;
  const ASize: INyxSize; const AProperty: TSizeProperty);
begin
  if AType = puAfterUpdate then
    DoUpdateHeight;
end;

procedure TNyxElementBaseImpl.NotifyWidth(const AType: TPropertyUpdateType;
  const ASize: INyxSize; const AProperty: TSizeProperty);
begin
  if AType = puAfterUpdate then
    DoUpdateWidth;
end;

procedure TNyxElementBaseImpl.NotifyMode(const AType: TPropertyUpdateType;
  const ASize: INyxSize; const AProperty: TSizeProperty);
begin
  if AType = puAfterUpdate then
    DoUpdateMode;
end;

procedure TNyxElementBaseImpl.NotifyElement(const AType: TPropertyUpdateType;
  const ASize: INyxSize; const AProperty: TSizeProperty);
begin
  //just remove observers and clear ref
  if AType = puAfterUpdate then
  begin
    RemoveSizeObservers;
    FSize := nil;
  end;
end;

procedure TNyxElementBaseImpl.SetID(const AValue: String);
begin
  //before notifty
  PropertyNotify(puBeforeUpdate, epID);

  DoSetID(AValue);

  //after notifty
  PropertyNotify(puAfterUpdate, epID);
end;

function TNyxElementBaseImpl.GetID: String;
begin
  Result := DoGetID;
end;

function TNyxElementBaseImpl.GetName: String;
begin
  Result := DoGetName;
end;

procedure TNyxElementBaseImpl.SetName(const AValue: String);
begin
  //before notifty
  PropertyNotify(puBeforeUpdate, epName);

  DoSetName(AValue);

  //after notifty
  PropertyNotify(puAfterUpdate, epName);
end;

function TNyxElementBaseImpl.GetContainer: INyxContainer;
begin
  Result := DoGetContainer;
end;

procedure TNyxElementBaseImpl.SetContainer(const AValue: INyxContainer);
begin
  //before notifty
  PropertyNotify(puBeforeUpdate, epContainer);

  DoSetContainer(AValue);

  //after notifty
  PropertyNotify(puAfterUpdate, epContainer);
end;

procedure TNyxElementBaseImpl.DoSetID(const AValue: String);
begin
  FID := AValue;
end;

function TNyxElementBaseImpl.DoGetID: String;
begin
  Result := FID;
end;

function TNyxElementBaseImpl.DoGetName: String;
begin
  Result := FName;
end;

procedure TNyxElementBaseImpl.DoSetName(const AValue: String);
begin
  FName := AValue;
end;

function TNyxElementBaseImpl.DoGetContainer: INyxContainer;
begin
  Result := FContainer;
end;

procedure TNyxElementBaseImpl.DoSetContainer(const AValue: INyxContainer);
begin
  FContainer := nil;
  FContainer := AValue;
end;

function TNyxElementBaseImpl.DoGenerateID: String;
var
  LGUID: TGUID;
begin
  CreateGUID(LGUID);
  Result := GUIDToString(LGUID);
end;

procedure TNyxElementBaseImpl.DoParentSize(const ASize: INyxSize);
var
  LSelf: INyxElement;
  LID: String;
begin
  LSelf := DoGetSelf;
  ASize.Element := LSelf;

  //now attach handlers for property updates
  ASize.Observe(scHeight, @NotifyHeight, LID);
  FSizeIDs.Add(LID);

  ASize.Observe(scWidth, @NotifyWidth, LID);
  FSizeIDs.Add(LID);

  ASize.Observe(scMode, @NotifyMode, LID);
  FSizeIDs.Add(LID);

  ASize.Observe(scElement, @NotifyElement, LID);
  FSizeIDs.Add(LID);
end;

procedure TNyxElementBaseImpl.DoUpdateHeight;
begin
  //nothing in base
end;

procedure TNyxElementBaseImpl.DoUpdateWidth;
begin
  //nothing in base
end;

procedure TNyxElementBaseImpl.DoUpdateMode;
begin
  //nothing in base
end;

procedure TNyxElementBaseImpl.DoRemoveObserver(const AID: String);
begin
  FObserve.RemoveByID(AID);
  FPropObserve.RemoveByID(AID);
end;

function TNyxElementBaseImpl.Observe(const AEvent: TElementEvent;
  const AObserver: TElementObserveMethod; out ID: String): INyxElement;
begin
  Result := DoGetSelf;

  if not Assigned(AObserver) then
    Exit;

  ID := FObserve.Observe(Ord(AEvent), Pointer(AObserver));
end;

procedure TNyxElementBaseImpl.PropertyNotify(
  const AType: TPropertyUpdateType; const AProperty: TElementProperty);
var
  LMethod: TElementPropertyObserveMethod;
  I: Integer;
  LElement: INyxElement;
  LObservers: TObserverArray;
begin
  LElement := DoGetSelf;
  LObservers := FObserve.ObserversByEvent(Ord(AProperty));

  for I := 0 to High(LObservers) do
    try
      LMethod := TElementPropertyObserveMethod(LObservers[I]);

      //call the method
      LMethod(AType, LElement, AProperty);
    finally
    end;
end;

procedure TNyxElementBaseImpl.Notify(const AEvent: TElementEvent);
var
  LMethod: TElementObserveMethod;
  I: Integer;
  LElement: INyxElement;
  LObservers: TObserverArray;
begin
  LElement := DoGetSelf;
  LObservers := FObserve.ObserversByEvent(Ord(AEvent));

  for I := 0 to High(LObservers) do
    try
      LMethod := TElementObserveMethod(LObservers[I]);

      //call the method
      LMethod(LElement, AEvent);
    finally
    end;
end;

function TNyxElementBaseImpl.DoGetSelf: INyxElement;
begin
  Result := Self as INyxElement;
end;

function TNyxElementBaseImpl.Observe(const AProperty: TElementProperty;
  const AObserver: TElementPropertyObserveMethod; out ID: String): INyxElement;
begin
  Result := DoGetSelf;

  if not Assigned(AObserver) then
    Exit;

  ID := FPropObserve.Observe(Ord(AProperty), Pointer(AObserver));
end;

function TNyxElementBaseImpl.RemoveObserver(const AID: String): INyxElement;
begin
  Result := DoGetSelf;
  DoRemoveObserver(AID);
end;

function TNyxElementBaseImpl.UpdateName(const AName: String): INyxElement;
begin
  Result := DoGetSelf;
  SetName(AName);
end;

function TNyxElementBaseImpl.UpdateSize(const ASize: INyxSize): INyxElement;
begin
  Result := DoGetSelf;
  SetSize(ASize);
end;

function TNyxElementBaseImpl.UpdateContainer(const AContainer: INyxContainer): INyxElement;
begin
  Result := DoGetSelf;
  SetContainer(AContainer);
end;

function TNyxElementBaseImpl.UpdateID(const AID: String): INyxElement;
begin
  Result := DoGetSelf;
  SetID(AID);
end;

function TNyxElementBaseImpl.Condition(const ACondition: Boolean; const ATrue,
  AFalse: TNyxElementCallback): INyxElement;
begin
  Result := DoGetSelf;

  //eval the condition and call the appropriate method
  if ACondition then
  begin
    if Assigned(ATrue) then
      ATrue(Result)
  end
  else if Assigned(AFalse) then
    AFalse(Result);
end;

function TNyxElementBaseImpl.Condition(const ACondition: Boolean; const ATrue,
  AFalse: TNyxElementNestedCallback): INyxElement;
begin
  Result := DoGetSelf;

  //eval the condition and call the appropriate method
  if ACondition then
  begin
    if Assigned(ATrue) then
      ATrue(Result)
  end
  else if Assigned(AFalse) then
    AFalse(Result);
end;

function TNyxElementBaseImpl.Condition(const ACondition: Boolean; const ATrue,
  AFalse: TNyxElementMethod): INyxElement;
begin
  Result := DoGetSelf;

  //eval the condition and call the appropriate method
  if ACondition then
  begin
    if Assigned(ATrue) then
      ATrue(Result)
  end
  else if Assigned(AFalse) then
    AFalse(Result);
end;

function TNyxElementBaseImpl.Condition(
  const ACondition: TNyxElementBoolCallback; const ATrue,
  AFalse: TNyxElementCallback): INyxElement;
begin
  Result := DoGetSelf;

  //no input to evaluate, so call false if assigned
  if not Assigned(ACondition) then
  begin
    if Assigned(AFalse) then
      AFalse(Result);

    Exit;
  end;

  //eval the condition and call the appropriate method
  if ACondition(Result) then
  begin
    if Assigned(ATrue) then
      ATrue(Result)
  end
  else if Assigned(AFalse) then
    AFalse(Result);
end;

function TNyxElementBaseImpl.Condition(
  const ACondition: TNyxElementBoolNestedCallback; const ATrue,
  AFalse: TNyxElementNestedCallback): INyxElement;
begin
  Result := DoGetSelf;

  //no input to evaluate, so call false if assigned
  if not Assigned(ACondition) then
  begin
    if Assigned(AFalse) then
      AFalse(Result);

    Exit;
  end;

  //eval the condition and call the appropriate method
  if ACondition(Result) then
  begin
    if Assigned(ATrue) then
      ATrue(Result)
  end
  else if Assigned(AFalse) then
    AFalse(Result);
end;

function TNyxElementBaseImpl.Condition(const ACondition: TNyxElementBoolMethod;
  const ATrue, AFalse: TNyxElementMethod): INyxElement;
begin
  Result := DoGetSelf;

  //no input to evaluate, so call false if assigned
  if not Assigned(ACondition) then
  begin
    if Assigned(AFalse) then
      AFalse(Result);

    Exit;
  end;

  //eval the condition and call the appropriate method
  if ACondition(Result) then
  begin
    if Assigned(ATrue) then
      ATrue(Result)
  end
  else if Assigned(AFalse) then
    AFalse(Result);
end;

constructor TNyxElementBaseImpl.Create;
begin
  FObserve := TNyxObservationHelper.Create;
  FPropObserve := TNyxObservationHelper.Create;
  FContainer := nil;
  FSizeIDs := TStringList.Create;
  Size := NewNyxSize; //use property here to properly parent/setup
  FID := DoGenerateID;
end;

destructor TNyxElementBaseImpl.Destroy;
begin
  FObserve.Free;
  FPropObserve.Free;
  FContainer := nil;
  FSizeIDs.Free;
  RemoveSizeObservers;
  FSize := nil;
  inherited Destroy;
end;

end.

