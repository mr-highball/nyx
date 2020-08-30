unit nyx.element;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  nyx.types;

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
  public
    property ID : String read GetID write SetID;
    property Name : String read GetName write SetName;
    property Container : INyxContainer read GetContainer write SetContainer;
    property Size : INyxSize read GetSize write SetSize;

    function UpdateName(const AName : String) : INyxElement;

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
  DoSetID(AValue);
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
  DoSetName(AValue);
end;

function TNyxElementBaseImpl.GetContainer: INyxContainer;
begin
  Result := DoGetContainer;
end;

procedure TNyxElementBaseImpl.SetContainer(const AValue: INyxContainer);
begin
  DoSetContainer(AValue);
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
  LSelf := Self as INyxElement;
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

function TNyxElementBaseImpl.UpdateName(const AName: String): INyxElement;
begin
  SetName(AName);
  Result := Self as INyxElement;
end;

function TNyxElementBaseImpl.Condition(const ACondition: Boolean; const ATrue,
  AFalse: TNyxElementCallback): INyxElement;
begin
  Result := Self as INyxElement;

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
  Result := Self as INyxElement;

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
  Result := Self as INyxElement;

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
  Result := Self as INyxElement;

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
  Result := Self as INyxElement;

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
  Result := Self as INyxElement;

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
  FContainer := nil;
  FSizeIDs := TStringList.Create;
  Size := NewNyxSize; //use property here to properly parent/setup
  FID := DoGenerateID;
end;

destructor TNyxElementBaseImpl.Destroy;
begin
  FContainer := nil;
  FSizeIDs.Free;
  RemoveSizeObservers;
  FSize := nil;
  inherited Destroy;
end;

end.

