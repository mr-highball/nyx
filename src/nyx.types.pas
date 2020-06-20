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
unit nyx.types;

{$mode delphi}

{$IFNDEF BROWSER}
{$ModeSwitch nestedprocvars}
{$ENDIF}

interface

uses
  Classes,
  SysUtils;

type

  //forward
  INyxContainer = interface;
  INyxElement = interface;

  TNyxElementCallback = procedure(const AElement : INyxElement);
  TNyxElementNestedCallback = procedure(const AElement : INyxElement) is nested;
  TNyxElementMethod = procedure(const AElement : INyxElement) of object;

  TNyxElementBoolCallback = function(const AElement : INyxElement) : Boolean;
  TNyxElementBoolNestedCallback = function(const AElement : INyxElement) : Boolean is nested;
  TNyxElementBoolMethod = function(const AElement : INyxElement) : Boolean of object;

  TNyxElementConditonCallback = function(const ACondition : TNyxElementBoolCallback;const ATrue, AFalse : TNyxElementCallback) : Boolean;
  TNyxElementConditionNestedCallback = function(const ACondition : TNyxElementBoolNestedCallback;const ATrue, AFalse : TNyxElementNestedCallback) : Boolean is nested;
  TNyxElementConditionMethod = function(const ACondition : TNyxElementBoolMethod;const ATrue, AFalse : TNyxElementMethod) : Boolean of object;

  { INyxElement }
  (*
    smallest building block for a nyx UI, can be a control, graphic, etc...
  *)
  INyxElement = interface
    ['{E102F2DB-955B-4626-A540-CA50F356D05E}']

    //property methods
    function GetID: String;
    function GetName: String;
    function GetContainer: INyxContainer;
    procedure SetContainer(const AValue: INyxContainer);
    procedure SetID(const AValue: String);
    procedure SetName(const AValue: String);

    //properties

    (*
      auto-generated identifier for this element
    *)
    property ID : String read GetID write SetID;

    (*
      optional friendly name for the element
    *)
    property Name : String read GetName write SetName;

    (*
      parent container of this element, nil if none
    *)
    property Container : INyxContainer read GetContainer write SetContainer;

    //methods

    (*
      updates the name and returns this element
    *)
    function UpdateName(const AName : String) : INyxElement;

    (*
      allows for boolean conditions
    *)
    function Condition(const ACondition : TNyxElementBoolCallback; const ATrue, AFalse : TNyxElementCallback) : INyxElement; overload;
    function Condition(const ACondition : TNyxElementBoolNestedCallback; const ATrue, AFalse : TNyxElementNestedCallback) : INyxElement; overload;
    function Condition(const ACondition : TNyxElementBoolMethod; const ATrue, AFalse : TNyxElementMethod) : INyxElement; overload;
  end;

  { TNyxElementBaseImpl }
  (*
    base implementation class for all INyxElement
  *)
  TNyxElementBaseImpl = class(TInterfacedObject, INyxElement)
  strict private
    FID,
    FName : String;
    FContainer : INyxContainer;
  protected
    procedure SetID(const AValue: String);
    function GetID: String;
    function GetName: String;
    procedure SetName(const AValue: String);
    function GetContainer: INyxContainer;
    procedure SetContainer(const AValue: INyxContainer);
  strict protected

    (*
      children can override this method to generate a unique identifier
      if the default generation will not suffice
    *)
    function DoGetID : String; virtual;
  public
    property ID : String read GetID write SetID;
    property Name : String read GetName write SetName;
    property Container : INyxContainer read GetContainer write SetContainer;

    function UpdateName(const AName : String) : INyxElement;

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

  (*
    observer types for elements collection
  *)
  TElementsObserveEvent = (eoAdd, eoExtract);
  TElementsObserveMethod = procedure(const AElement : INyxElement; const AEvent : TElementsObserveEvent) of object;

  { INyxElements }
  (*
    a collection of elements
  *)
  INyxElements = interface
    ['{09F68005-93C3-4DCF-8900-36A96B07E19D}']
    //property methods
    function GetCount: Integer;
    function GetItem(const AIndex : Integer): INyxElement;

    //properties

    (*
      number of elements in the collection
    *)
    property Count : Integer read GetCount;

    (*
      returns the element at a given index, if the index doesn't exist
      an exception will be thrown
    *)
    property Items[const AIndex : Integer] : INyxElement read GetItem; default;

    //methods

    (*
      adds a new element to the collection and returns the index
    *)
    function Add(const AItem : INyxElement) : Integer;

    (*
      will assign the index argument to the found element if it exists. if not
      found the value be lower than zero
    *)
    function IndexOf(const AElement : INyxElement; out Index : Integer) : INyxElements;

    (*
      deletes an element at a given index, if the index doesn't exist
      will simply return
    *)
    function Delete(const AIndex : Integer) : INyxElements; overload;

    (*
      deletes an element, if the index doesn't exist will simply return
    *)
    function Delete(const AElement : INyxElement) : INyxElements; overload;

    (*
      removes the element at a given index, but returns it to the caller
    *)
    function Extract(const AIndex : Integer) : INyxElement;

    (*
      provided a handler, will iterate all elements in the collection
    *)
    function ForEach(const AProc : TNyxElementCallback) : INyxElements; overload;
    function ForEach(const AProc : TNyxElementNestedCallback) : INyxElements; overload;
    function ForEach(const AProc : TNyxElementMethod) : INyxElements; overload;

    (*
      provided a handler, will iterate all elements in the collection until
      the handler returns true (signalling a "found")
    *)
    function Find(const AProc : TNyxElementBoolCallback) : INyxElement; overload;
    function Find(const AProc : TNyxElementBoolNestedCallback) : INyxElement; overload;
    function Find(const AProc : TNyxElementBoolMethod) : INyxElement; overload;

    (*
      provided a handler, will iterate all elements in the collection
      and will add all "found" elements to the resulting collection
    *)
    function FindAll(const AProc : TNyxElementBoolCallback; const ARecurse : Boolean = True) : INyxElements; overload;
    function FindAll(const AProc : TNyxElementBoolNestedCallback; const ARecurse : Boolean = True) : INyxElements; overload;
    function FindAll(const AProc : TNyxElementBoolMethod; const ARecurse : Boolean = True) : INyxElements; overload;

    (*
      clears the collection
    *)
    function Clear : INyxElements;

    procedure Observe(const AObserver : TElementsObserveMethod; out ID : String);
    procedure RemoveObserver(const AID : String);
  end;

  { TNyxElementsBaseImpl }
  (*
    base implementation for a collection of elements INyxElements
  *)
  TNyxElementsBaseImpl = class(TInterfacedObject, INyxElements)
  strict private
    FID : TStringList;
    procedure RaiseError(const AMethod, AError : String);
    procedure ClearObservers;
    procedure NotifyAdd(const AElement : INyxElement);
    procedure NotifyExtract(const AElement : INyxElement);
  protected
    function GetCount: Integer;
    function GetItem(const AIndex : Integer): INyxElement;
  strict protected
    function DoGetCount : Integer; virtual; abstract;

    function DoGetItem(const AIndex : Integer; out Item : INyxElement;
      out Error : String) : Boolean; virtual; abstract;

    function DoAddAtem(const AItem : INyxElement; out Index : Integer;
      out Error : String) : Boolean; virtual; abstract;

    function DoRemoveItem(const AIndex : Integer; out Item : INyxElement;
      out Error : String) : Boolean; virtual; abstract;
  public
    property Count : Integer read GetCount;
    property Items[const AIndex : Integer] : INyxElement read GetItem; default;

    function Add(const AItem : INyxElement) : Integer;
    function IndexOf(const AElement : INyxElement; out Index : Integer) : INyxElements;
    function Delete(const AIndex : Integer) : INyxElements; overload;
    function Delete(const AElement : INyxElement) : INyxElements; overload;
    function Extract(const AIndex : Integer) : INyxElement;

    function ForEach(const AProc : TNyxElementCallback) : INyxElements; overload;
    function ForEach(const AProc : TNyxElementNestedCallback) : INyxElements; overload;
    function ForEach(const AProc : TNyxElementMethod) : INyxElements; overload;

    function Find(const AProc : TNyxElementBoolCallback) : INyxElement; overload;
    function Find(const AProc : TNyxElementBoolNestedCallback) : INyxElement; overload;
    function Find(const AProc : TNyxElementBoolMethod) : INyxElement; overload;

    function FindAll(const AProc : TNyxElementBoolCallback; const ARecurse : Boolean = True) : INyxElements; overload;
    function FindAll(const AProc : TNyxElementBoolNestedCallback; const ARecurse : Boolean = True) : INyxElements; overload;
    function FindAll(const AProc : TNyxElementBoolMethod; const ARecurse : Boolean = True) : INyxElements; overload;

    function Clear : INyxElements;

    procedure Observe(const AObserver : TElementsObserveMethod; out ID : String);
    procedure RemoveObserver(const AID : String);

    constructor Create; virtual;
    destructor Destroy; override;
  end;

  (*
    metaclass for concrete nyx elements
  *)
  TNyxElementsClass = class of TNyxElementsBaseImpl;



  //forward
  INyxUI = interface;

  { INyxContainer }
  (*
    the container holds elements but is also an element itself, allowing
    for building larger components
  *)
  INyxContainer = interface(INyxElement)
    ['{5D1CB48B-63FE-4E2A-8333-371D1F0266AB}']

    //property methods
    function GetElements: INyxElements;
    function GetUI: INyxUI;
    procedure SetUI(const AValue: INyxUI);

    //properties

    (*
      collection of all children elements
    *)
    property Elements : INyxElements read GetElements;

    (*
      parent nyx ui, will be nil if none set
    *)
    property UI : INyxUI read GetUI write SetUI;

    //methods

    (*
      adds an element to the elements collection and returns this container.
      also sets the container property on the input element to this instance
    *)
    function Add(const AItem : INyxElement; out Index : Integer) : INyxContainer; overload;
    function Add(const AItem : INyxElement) : INyxContainer; overload;
  end;

  { TNyxContainerBaseImpl }
  (*
    base implementation class for all INyxContainer
  *)
  TNyxContainerBaseImpl = class(TNyxElementBaseImpl, INyxContainer)
  strict private
    FElements : INyxElements;
    FUI : INyxUI;
  protected
    function GetElements: INyxElements;
    function GetUI: INyxUI;
    procedure SetUI(const AValue: INyxUI);
    function GetContainer: INyxContainer; reintroduce;
  strict protected
  public
    property Elements : INyxElements read GetElements;
    property UI : INyxUI read GetUI write SetUI;

    function Add(const AItem : INyxElement; out Index : Integer) : INyxContainer; overload;
    function Add(const AItem : INyxElement) : INyxContainer; overload;

    constructor Create; override;
    destructor Destroy; override;
  end;

  (*
    metaclass for nyx containers
  *)
  TNyxContainerClass = class of TNyxContainerBaseImpl;

  { INyxLayout }
  (*
    responsible for determining the visual placement/contstraints
    of a INyxContainer
  *)
  INyxLayout = interface(INyxContainer)
    ['{7A4611A4-6C0C-4D7A-845E-2232743CC8B3}']

    //property methods

    //properties

    //methods

    (*
      called to recalculate the parent container's placement/constraints
    *)
    function UpdatePlacement(out Error : String) : Boolean; overload;
    function UpdatePlacement : Boolean; overload;
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
  public
    function UpdatePlacement(out Error : String) : Boolean; overload;
    function UpdatePlacement : Boolean; overload;
  end;

  //meta class for concrete nyx layouts
  TNyxLayoutClass = class of TNyxLayoutBaseImpl;

  TNyxActionCallback = procedure(const AUI : INyxUI; const AArgs : array of const);
  TNyxActionNestedCallback = procedure(const AUI : INyxUI; const AArgs : array of const) is nested;
  TNyxActionMethod = procedure(const AUI : INyxUI; const AArgs : array of const) of object;

  { INyxRenderSettings }
  (*
    settings used for rendering the a nyx ui
  *)
  INyxRenderSettings = interface
    ['{91137F74-9500-4A55-98CB-C6924D1FB8F2}']

    //property methods
    function GetUI: INyxUI;
    procedure SetUI(const AValue: INyxUI);

    //properties
    property UI : INyxUI read GetUI write SetUI;
  end;

  { TNyxRenderSettingsBaseImpl }
  (*
    base implementation for all ui settings
  *)
  TNyxRenderSettingsBaseImpl = class(TInterfacedObject, INyxRenderSettings)
  strict private
    FUI : INyxUI;
  protected
    function GetUI: INyxUI;
    procedure SetUI(const AValue: INyxUI);
  strict protected
  public
    property UI : INyxUI read GetUI write SetUI;

    constructor Create; virtual;
    destructor Destroy; override;
  end;

  { INyxUI }
  (*
    the main builder for a user interface
  *)
  INyxUI = interface
    ['{94032034-CA91-4A29-8148-56255A8F89BC}']

    //property methods
    function GetContainers: INyxElements;
    function GetSettings: INyxRenderSettings;
    procedure SetSettings(const AValue: INyxRenderSettings);

    //properties
    property Containers : INyxElements read GetContainers;
    property Settings : INyxRenderSettings read GetSettings write SetSettings;

    //methods

    (*
      pulls a container from the Containers property and casts it
    *)
    function ContainerByIndex(const AIndex : Integer) : INyxContainer;

    (*
      updates the render settings for the nyx ui
    *)
    function UpdateSettings(const ASettings : INyxRenderSettings) : INyxUI;

    (*
      adds a container to the containers collection and outputs the
      index
    *)
    function AddContainer(const AContainer : INyxContainer; out Index : Integer) : INyxUI;

    (*
      in-between building a UI, an action can be taken
    *)
    function TakeAction(const AAction : TNyxActionCallback; const AArgs : array of const) : INyxUI; overload;
    function TakeAction(const AAction : TNyxActionNestedCallback; const AArgs : array of const) : INyxUI; overload;
    function TakeAction(const AAction : TNyxActionMethod; const AArgs : array of const) : INyxUI; overload;

    (*
      renders all containers to the screen
    *)
    function Render() : INyxUI; overload;
    function Render(const ASettings : INyxRenderSettings) : INyxUI; overload;

    (*
      hides any rendered containers from the screen
    *)
    function Hide() : INyxUI;

    (*
      clears this UI and cleans up any resources held
    *)
    function Clear : INyxUI;
  end;

  { TNyxUIBaseImpl }
  (*
    base implementation for all INyxUI
  *)
  TNyxUIBaseImpl = class(TInterfacedPersistent, INyxUI)
  strict private
    FSettings : INyxRenderSettings;
    FContainers : INyxElements;
  protected
    function GetContainers: INyxElements;
    function GetSettings: INyxRenderSettings;
    procedure SetSettings(const AValue: INyxRenderSettings);
  strict protected
    (*
      children need to override this method render all containers and
      their elements to the screen
    *)
    procedure DoRender; virtual; abstract;

    (*
      children need to override this method to "hide" any rendered
      containers/elements on the screen
    *)
    procedure DoHide; virtual; abstract;

    (*
      children can override this method to take action when the settings
      change
    *)
    procedure DoSettingsChange; virtual;

    (*
      children can override this to perform additional steps of a clear
      default, will call Hide() then clear the containers collection
    *)
    procedure DoClear; virtual;
  public
    property Containers : INyxElements read GetContainers;
    property Settings : INyxRenderSettings read GetSettings write SetSettings;

    function ContainerByIndex(const AIndex : Integer) : INyxContainer;
    function UpdateSettings(const ASettings : INyxRenderSettings) : INyxUI;
    function AddContainer(const AContainer : INyxContainer; out Index : Integer) : INyxUI;

    function TakeAction(const AAction : TNyxActionCallback; const AArgs : array of const) : INyxUI; overload;
    function TakeAction(const AAction : TNyxActionNestedCallback; const AArgs : array of const) : INyxUI; overload;
    function TakeAction(const AAction : TNyxActionMethod; const AArgs : array of const) : INyxUI; overload;

    function Render() : INyxUI; overload;
    function Render(const ASettings : INyxRenderSettings) : INyxUI; overload;

    function Hide() : INyxUI;

    function Clear : INyxUI;

    constructor Create; virtual;
    destructor Destroy; override;
  end;

  (*
    metaclass for a nyx ui
  *)
  TNyxUIClass = class of TNyxUIBaseImpl;

(*
  helper function to return a nyx container
*)
function NewNyxContainer : INyxContainer;

(*
  helper function to return a nyx ui
*)
function NewNyxUI : INyxUI;

(*
  helper function to return a nyx elements collection
*)
function NewNyxElements : INyxElements;

implementation
uses
{$IFDEF BROWSER}
  nyx.element.browser,
  nyx.elements.browser,
  nyx.container.browser,
  nyx.ui.browser;
{$ELSE}
  nyx.element.std,
  nyx.elements.std,
  nyx.container.std,
  nyx.ui.std;
{$ENDIF}

var
  DefaultNyxElements : TNyxElementsClass;
  DefaultNyxUI : TNyxUIClass;
  DefaultNyxLayout : TNyxLayoutClass;
  DefaultNyxContainer : TNyxContainerClass;

function NewNyxContainer: INyxContainer;
begin
  Result := DefaultNyxContainer.Create;
end;

function NewNyxUI: INyxUI;
begin
  Result := DefaultNyxUI.Create;
end;

function NewNyxElements: INyxElements;
begin
  Result := DefaultNyxElements.Create;
end;

{ TNyxRenderSettingsBaseImpl }

function TNyxRenderSettingsBaseImpl.GetUI: INyxUI;
begin
  Result := FUI;
end;

procedure TNyxRenderSettingsBaseImpl.SetUI(const AValue: INyxUI);
begin
  FUI := nil;
  FUI := AValue;
end;

constructor TNyxRenderSettingsBaseImpl.Create;
begin
  FUI := nil;
end;

destructor TNyxRenderSettingsBaseImpl.Destroy;
begin
  FUI := nil;
  inherited Destroy;
end;

{ TNyxLayoutBaseImpl }

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

{ TNyxUIBaseImpl }

function TNyxUIBaseImpl.GetContainers: INyxElements;
begin
  Result := FContainers;
end;

function TNyxUIBaseImpl.GetSettings: INyxRenderSettings;
begin
  Result := FSettings;
end;

procedure TNyxUIBaseImpl.SetSettings(const AValue: INyxRenderSettings);
begin
  FSettings := nil;
  FSettings := AValue;

  if Assigned(AValue) then
    AValue.UI := Self as INyxUI;

  DoSettingsChange;
end;

procedure TNyxUIBaseImpl.DoSettingsChange;
begin
  //nothing in base
end;

procedure TNyxUIBaseImpl.DoClear;
begin
  Hide;
  FContainers.Clear;
end;

function TNyxUIBaseImpl.ContainerByIndex(const AIndex: Integer): INyxContainer;
begin
  Result := FContainers[AIndex] as INyxContainer;
end;

function TNyxUIBaseImpl.UpdateSettings(const ASettings: INyxRenderSettings): INyxUI;
begin
  Result := Self as INyxUI;
  Settings := ASettings;
end;

function TNyxUIBaseImpl.AddContainer(const AContainer: INyxContainer; out
  Index: Integer): INyxUI;
begin
  Result := Self as INyxUI;
  Index := FContainers.Add(AContainer);

  if Assigned(AContainer) then
    AContainer.UI := Self as INyxUI;
end;

function TNyxUIBaseImpl.TakeAction(const AAction: TNyxActionCallback;
  const AArgs: array of const): INyxUI;
begin
  Result := Self as INyxUI;

  if not Assigned(AAction) then
    Exit;

  AAction(Result, AArgs);
end;

function TNyxUIBaseImpl.TakeAction(const AAction: TNyxActionNestedCallback;
  const AArgs: array of const): INyxUI;
begin
  Result := Self as INyxUI;

  if not Assigned(AACtion) then
    Exit;

  AAction(Result, AArgs);
end;

function TNyxUIBaseImpl.TakeAction(const AAction: TNyxActionMethod;
  const AArgs: array of const): INyxUI;
begin
  Result := Self as INyxUI;

  if not Assigned(AACtion) then
    Exit;

  AAction(Result, AArgs);
end;

function TNyxUIBaseImpl.Render(): INyxUI;
begin
  Result := Self as INyxUI;

  //hide the current ui before rendering
  Hide;

  //render new ui
  DoRender;
end;

function TNyxUIBaseImpl.Render(const ASettings: INyxRenderSettings): INyxUI;
begin
  FSettings := nil;
  FSettings := ASettings;
  Result := Render;
end;

function TNyxUIBaseImpl.Hide(): INyxUI;
begin
  Result := Self as INyxUI;
  DoHide;
end;

function TNyxUIBaseImpl.Clear: INyxUI;
begin
  Result := Self as INyxUI;
  DoClear;
end;

constructor TNyxUIBaseImpl.Create;
begin
  FSettings := nil;
  FContainers := DefaultNyxElements.Create;
end;

destructor TNyxUIBaseImpl.Destroy;
begin
  FSettings := nil;
  FContainers := nil;
  inherited Destroy;
end;

{ TNyxElementsBaseImpl }

procedure TNyxElementsBaseImpl.RaiseError(const AMethod, AError: String);
begin
  raise Exception.Create(Self.ClassName + '::' + AMethod + '::' + AError);
end;

procedure TNyxElementsBaseImpl.ClearObservers;
begin
  FID.Clear;
end;

procedure TNyxElementsBaseImpl.NotifyAdd(const AElement: INyxElement);
var
  I: Integer;
  LMethod : TElementsObserveMethod;
begin
  for I := 0 to Pred(FID.Count) do
    try
      LMethod := TElementsObserveMethod(Pointer(FID.Objects[I]));
      LMethod(AElement, eoAdd);
    finally
    end;
end;

procedure TNyxElementsBaseImpl.NotifyExtract(const AElement: INyxElement);
var
  I: Integer;
  LMethod : TElementsObserveMethod;
begin
  for I := 0 to Pred(FID.Count) do
    try
      LMethod := TElementsObserveMethod(Pointer(FID.Objects[I]));
      LMethod(AElement, eoExtract);
    finally
    end;
end;

function TNyxElementsBaseImpl.GetCount: Integer;
begin
  Result := DoGetCount;
end;

function TNyxElementsBaseImpl.GetItem(const AIndex: Integer): INyxElement;
var
  LError: String;
begin
  try
    if not DoGetItem(AIndex, Result, LError) then
      RaiseError('GetItem', LError);
  except on E : Exception do
    RaiseError('GetItem', E.Message);
  end;
end;

function TNyxElementsBaseImpl.Add(const AItem: INyxElement): Integer;
var
  LError: String;
begin
  try
    if not DoAddAtem(AItem, Result, LError) then
      RaiseError('Add', LError);

    NotifyAdd(AItem);
  except on E : Exception do
    RaiseError('Add', E.Message);
  end;
end;

function TNyxElementsBaseImpl.IndexOf(const AElement: INyxElement; out
  Index: Integer): INyxElements;
var
  I: Integer;
  LElement: INyxElement;
begin
  try
    Result := Self as INyxElements;
    Index := -1;

    //iterate the item list and check against the id as a form of comparison
    for I := 0 to Pred(Count) do
    begin
      LElement := Items[I];

      //special handling for nil elements (input or stored)
      if not Assigned(LElement) then
      begin
        if not Assigned(AElement) then
        begin
          Index := I;
          Exit;
        end;
      end
      else
      begin
        //on nil move next
        if not Assigned(AElement) then
          Continue
        //otherwise we found a match
        else if LElement.ID = AElement.ID then
        begin
          Index := I;
          Exit;
        end;
      end;
    end;
  except on E : Exception do
    RaiseError('IndexOf', E.Message);
  end;
end;

function TNyxElementsBaseImpl.Delete(const AIndex: Integer): INyxElements;
var
  LError: String;
  LItem: INyxElement;
begin
  try
    Result := Self as INyxElements;

    //remove but the throw away the item
    if not DoRemoveItem(AIndex, LItem, LError) then
      RaiseError('Delete', LError);
  except on E : Exception do
    RaiseError('Delete', E.Message);
  end;
end;

function TNyxElementsBaseImpl.Delete(const AElement: INyxElement): INyxElements;
var
  I: Integer;
begin
  Result := IndexOf(AElement, I);
  Delete(I);
end;

function TNyxElementsBaseImpl.Extract(const AIndex: Integer): INyxElement;
var
  LError: String;
begin
  try
    if not DoRemoveItem(AIndex, Result, LError) then
      RaiseError('Extract', LError);

    NotifyExtract(Result);
  except on E : Exception do
    RaiseError('Extract', E.Message);
  end;
end;

function TNyxElementsBaseImpl.ForEach(const AProc: TNyxElementCallback): INyxElements;
var
  LCount, I: Integer;
  LItem: INyxElement;
begin
  Result := Self as INyxElements;

  if not Assigned(AProc) then
    Exit;

  //get the count of items
  LCount := Count;

  //iterate using the the items property
  for I := 0 to Pred(LCount) do
  begin
    LItem := Items[I];

    try
      AProc(LItem);
    except on E : Exception do
      RaiseError('ForEach (callback)', E.Message);
    end;
  end;
end;

function TNyxElementsBaseImpl.ForEach(const AProc: TNyxElementNestedCallback): INyxElements;
var
  LCount, I: Integer;
  LItem: INyxElement;
begin
  Result := Self as INyxElements;

  if not Assigned(AProc) then
    Exit;

  //get the count of items
  LCount := Count;

  //iterate using the the items property
  for I := 0 to Pred(LCount) do
  begin
    LItem := Items[I];

    try
      AProc(LItem);
    except on E : Exception do
      RaiseError('ForEach (nested)', E.Message);
    end;
  end;
end;

function TNyxElementsBaseImpl.ForEach(const AProc: TNyxElementMethod): INyxElements;
var
  LCount, I: Integer;
  LItem: INyxElement;
begin
  Result := Self as INyxElements;

  if not Assigned(AProc) then
    Exit;

  //get the count of items
  LCount := Count;

  //iterate using the the items property
  for I := 0 to Pred(LCount) do
  begin
    LItem := Items[I];

    try
      AProc(LItem);
    except on E : Exception do
      RaiseError('ForEach (method)', E.Message);
    end;
  end;
end;

function TNyxElementsBaseImpl.Find(const AProc: TNyxElementBoolCallback): INyxElement;
var
  LCount, I: Integer;
  LItem: INyxElement;
begin
  Result := nil;

  if not Assigned(AProc) then
    Exit;

  //get the count of items
  LCount := Count;

  //iterate using the the items property
  for I := 0 to Pred(LCount) do
  begin
    LItem := Items[I];

    try
      //once we find the item, then exit
      if AProc(LItem) then
      begin
        Result := LItem;
        Exit;
      end;
    except on E : Exception do
      RaiseError('Find (callback)', E.Message);
    end;
  end;
end;

function TNyxElementsBaseImpl.Find(const AProc: TNyxElementBoolNestedCallback): INyxElement;
var
  LCount, I: Integer;
  LItem: INyxElement;
begin
  Result := nil;

  if not Assigned(AProc) then
    Exit;

  //get the count of items
  LCount := Count;

  //iterate using the the items property
  for I := 0 to Pred(LCount) do
  begin
    LItem := Items[I];

    try
      //once we find the item, then exit
      if AProc(LItem) then
      begin
        Result := LItem;
        Exit;
      end;
    except on E : Exception do
      RaiseError('Find (nested)', E.Message);
    end;
  end;
end;

function TNyxElementsBaseImpl.Find(const AProc: TNyxElementBoolMethod): INyxElement;
var
  LCount, I: Integer;
  LItem: INyxElement;
begin
  Result := nil;

  if not Assigned(AProc) then
    Exit;

  //get the count of items
  LCount := Count;

  //iterate using the the items property
  for I := 0 to Pred(LCount) do
  begin
    LItem := Items[I];

    try
      //once we find the item, then exit
      if AProc(LItem) then
      begin
        Result := LItem;
        Exit;
      end;
    except on E : Exception do
      RaiseError('Find (method)', E.Message);
    end;
  end;
end;

function TNyxElementsBaseImpl.FindAll(const AProc: TNyxElementBoolCallback;
  const ARecurse: Boolean): INyxElements;
var
  LCount,
  I, J, K,
  LRecurseCount, LContainterCount: Integer;
  LItem: INyxElement;
  LContainer: INyxContainer;
  LRecurseResult: INyxElements;
begin
  //create the result elements collection
  Result := DefaultNyxElements.Create;

  if not Assigned(AProc) then
    Exit;

  //get the count of items
  LCount := Count;

  //iterate using the the items property
  for I := 0 to Pred(LCount) do
  begin
    //clear ref if exists
    LItem := nil;

    //get the item at the index
    LItem := Items[I];

    //if we are recusing check if the item is a container
    if ARecurse then
    begin
      //iterate all items in container and call FindAll()
      if LItem is INyxContainer then
      begin
        LContainer := nil;
        LContainer := LItem as INyxContainer;
        LContainterCount := LContainer.Elements.Count;

        for J := 0 to Pred(LContainterCount) do
        begin
          LRecurseResult := LContainer.Elements.FindAll(AProc, ARecurse);
          LRecurseCount := LRecurseResult.Count;

          for K := 0 to Pred(LRecurseCount) do
            Result.Add(LRecurseResult[K]);
        end;
      end;
    end;

    try
      //once we find the item, then add it to the collection
      if AProc(LItem) then
        Result.Add(LItem);
    except on E : Exception do
      RaiseError('FindAll (callback)', E.Message);
    end;
  end;
end;

function TNyxElementsBaseImpl.FindAll(
  const AProc: TNyxElementBoolNestedCallback; const ARecurse: Boolean): INyxElements;
var
  LCount,
  I, J, K,
  LRecurseCount, LContainterCount: Integer;
  LItem: INyxElement;
  LContainer: INyxContainer;
  LRecurseResult: INyxElements;
begin
  //create the result elements collection
  Result := DefaultNyxElements.Create;

  if not Assigned(AProc) then
    Exit;

  //get the count of items
  LCount := Count;

  //iterate using the the items property
  for I := 0 to Pred(LCount) do
  begin
    //clear ref if exists
    LItem := nil;

    //get the item at the index
    LItem := Items[I];

    //if we are recusing check if the item is a container
    if ARecurse then
    begin
      //iterate all items in container and call FindAll()
      if LItem is INyxContainer then
      begin
        LContainer := nil;
        LContainer := LItem as INyxContainer;
        LContainterCount := LContainer.Elements.Count;

        for J := 0 to Pred(LContainterCount) do
        begin
          LRecurseResult := LContainer.Elements.FindAll(AProc, ARecurse);
          LRecurseCount := LRecurseResult.Count;

          for K := 0 to Pred(LRecurseCount) do
            Result.Add(LRecurseResult[K]);
        end;
      end;
    end;

    try
      //once we find the item, then add it to the collection
      if AProc(LItem) then
        Result.Add(LItem);
    except on E : Exception do
      RaiseError('FindAll (nested)', E.Message);
    end;
  end;
end;

function TNyxElementsBaseImpl.FindAll(const AProc: TNyxElementBoolMethod;
  const ARecurse: Boolean): INyxElements;
var
  LCount,
  I, J, K,
  LRecurseCount, LContainterCount: Integer;
  LItem: INyxElement;
  LContainer: INyxContainer;
  LRecurseResult: INyxElements;
begin
  //create the result elements collection
  Result := DefaultNyxElements.Create;

  if not Assigned(AProc) then
    Exit;

  //get the count of items
  LCount := Count;

  //iterate using the the items property
  for I := 0 to Pred(LCount) do
  begin
    //clear ref if exists
    LItem := nil;

    //get the item at the index
    LItem := Items[I];

    //if we are recusing check if the item is a container
    if ARecurse then
    begin
      //iterate all items in container and call FindAll()
      if LItem is INyxContainer then
      begin
        LContainer := nil;
        LContainer := LItem as INyxContainer;
        LContainterCount := LContainer.Elements.Count;

        for J := 0 to Pred(LContainterCount) do
        begin
          LRecurseResult := LContainer.Elements.FindAll(AProc, ARecurse);
          LRecurseCount := LRecurseResult.Count;

          for K := 0 to Pred(LRecurseCount) do
            Result.Add(LRecurseResult[K]);
        end;
      end;
    end;

    try
      //once we find the item, then add it to the collection
      if AProc(LItem) then
        Result.Add(LItem);
    except on E : Exception do
      RaiseError('FindAll (method)', E.Message);
    end;
  end;
end;

function TNyxElementsBaseImpl.Clear: INyxElements;
begin
  Result := Self as INyxElements;

  //delete until no more items
  while Count > 0 do
    Delete(0);
end;

procedure TNyxElementsBaseImpl.Observe(const AObserver: TElementsObserveMethod;
  out ID: String);
var
  LGUID : TGuid;
begin
  if not Assigned(AObserver) then
    Exit;

  //use a guid as the ID and add the observer as an "object"
  CreateGUID(LGUID);
  ID := GUIDToString(LGUID);
  FID.AddObject(ID, TObject(Pointer(AObserver)));
end;

procedure TNyxElementsBaseImpl.RemoveObserver(const AID: String);
var
  I: Integer;
begin
  I := FID.IndexOf(AID);

  //remove if we found the id
  if I >= 0 then
    FID.Delete(I);
end;

constructor TNyxElementsBaseImpl.Create;
begin
  FID := TStringList.Create;
end;

destructor TNyxElementsBaseImpl.Destroy;
begin
  //clear the elements
  Clear;
  ClearObservers;
  inherited Destroy;
end;

{ TNyxContainerBaseImpl }

function TNyxContainerBaseImpl.GetElements: INyxElements;
begin
  Result := FElements;
end;

function TNyxContainerBaseImpl.GetUI: INyxUI;
begin
  Result := FUI;
end;

procedure TNyxContainerBaseImpl.SetUI(const AValue: INyxUI);
begin
  FUI := nil;
  FUI := AValue;
end;

function TNyxContainerBaseImpl.GetContainer: INyxContainer;
begin
  Result := Self as INyxContainer;
end;

function TNyxContainerBaseImpl.Add(const AItem: INyxElement; out Index: Integer): INyxContainer;
begin
  Result := Self as INyxContainer;
  AItem.Container := Result;
  Index := FElements.Add(AItem);
end;

function TNyxContainerBaseImpl.Add(const AItem: INyxElement): INyxContainer;
var
  I : Integer;
begin
  Result := Add(AItem, I);
end;

constructor TNyxContainerBaseImpl.Create;
begin
  inherited Create;
  FElements := DefaultNyxElements.Create;
  FUI := nil
end;

destructor TNyxContainerBaseImpl.Destroy;
begin
  FUI := nil;
  inherited Destroy;
end;

{ TNyxElementBaseImpl }

procedure TNyxElementBaseImpl.SetID(const AValue: String);
begin
  FID := AValue;
end;

function TNyxElementBaseImpl.GetID: String;
begin
  Result := FID;
end;

function TNyxElementBaseImpl.GetName: String;
begin
  Result := FName;
end;

procedure TNyxElementBaseImpl.SetName(const AValue: String);
begin
  FName := AValue;
end;

function TNyxElementBaseImpl.GetContainer: INyxContainer;
begin
  Result := FContainer;
end;

procedure TNyxElementBaseImpl.SetContainer(const AValue: INyxContainer);
begin
  FContainer := nil;
  FContainer := AValue;
end;

function TNyxElementBaseImpl.DoGetID: String;
var
  LGUID: TGUID;
begin
  CreateGUID(LGUID);
  Result := GUIDToString(LGUID);
end;

function TNyxElementBaseImpl.UpdateName(const AName: String): INyxElement;
begin
  SetName(AName);
  Result := Self as INyxElement;
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

  //evalue the condition and call the appropriate method
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

  //evalue the condition and call the appropriate method
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

  //evalue the condition and call the appropriate method
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
  FID := DoGetID;
end;

destructor TNyxElementBaseImpl.Destroy;
begin
  FContainer := nil;
  inherited Destroy;
end;

initialization
{$IFDEF BROWSER}
DefaultNyxElements := TNyxElementsBrowserImpl;
DefaultNyxContainer := TNyxContainerBrowserImpl;
DefaultNyxUI := TNyxUIBrowserImpl;
//todo - set the default nyx layout class
{$ELSE}
DefaultNyxElements := TNyxElementsStdImpl;
DefaultNyxContainer := TNyxContainerStdImpl;
//todo - set the default nyx ui class
//todo - set the default nyx layout class
{$ENDIF}
end.

