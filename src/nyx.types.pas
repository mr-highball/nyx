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

  { INyxElement }
  (*
    smallest building block for a nyx UI, can be a control, graphic, etc...
  *)
  INyxElement = interface
    ['{E102F2DB-955B-4626-A540-CA50F356D05E}']

    //property methods
    function GetID: String;
    function GetName: String;
    procedure SetName(const AValue: String);

    //properties

    (*
      auto-generated identifier for this element
    *)
    property ID : String read GetID;

    (*
      optional friendly name for the element
    *)
    property Name : String read GetName write SetName;

    //methods

    (*
      updates the name and returns this element
    *)
    function UpdateName(const AName : String) : INyxElement;
  end;

  { TNyxElementBaseImpl }
  (*
    base implementation class for all INyxElement
  *)
  TNyxElementBaseImpl = class(TInterfacedObject, INyxElement)
  strict private
    FID,
    FName : String;
  protected
    function GetID: String;
    function GetName: String;
    procedure SetName(const AValue: String);
  strict protected
    (*
      children will override this method to generate a unique identifier
    *)
    function DoGetID : String; virtual;
  public
    property ID : String read GetID;
    property Name : String read GetName write SetName;

    function UpdateName(const AName : String) : INyxElement;
    constructor Create; virtual;
  end;

  TNyxElementCallback = procedure(const AElement : INyxElement);
  TNyxElementNestedCallback = procedure(const AElement : INyxElement) is nested;
  TNyxElementMethod = procedure(const AElement : INyxElement) of object;

  TNyxElementFindCallback = function(const AElement : INyxElement) : Boolean;
  TNyxElementFindNestedCallback = function(const AElement : INyxElement) : Boolean is nested;
  TNyxElementFindMethod = function(const AElement : INyxElement) : Boolean of object;

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
      deletes an element at a given index, if the index doesn't exist
      will simply return
    *)
    function Delete(const AIndex : Integer) : INyxElements;

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
    function Find(const AProc : TNyxElementFindCallback) : INyxElement; overload;
    function Find(const AProc : TNyxElementFindNestedCallback) : INyxElement; overload;
    function Find(const AProc : TNyxElementFindMethod) : INyxElement; overload;

    (*
      provided a handler, will iterate all elements in the collection
      and will add all "found" elements to the resulting collection
    *)
    function FindAll(const AProc : TNyxElementFindCallback; const ARecurse : Boolean = True) : INyxElements; overload;
    function FindAll(const AProc : TNyxElementFindNestedCallback; const ARecurse : Boolean = True) : INyxElements; overload;
    function FindAll(const AProc : TNyxElementFindMethod; const ARecurse : Boolean = True) : INyxElements; overload;

    (*
      clears the collection
    *)
    function Clear : INyxElements;
  end;

  { INyxContainer }
  (*
    the container holds elements but is also an element itself, allowing
    for building larger components
  *)
  INyxContainer = interface(INyxElement)
    ['{5D1CB48B-63FE-4E2A-8333-371D1F0266AB}']

    //property methods
    function GetElements: INyxElements;

    //properties

    (*
      collection of all children elements
    *)
    property Elements : INyxElements read GetElements;

    //methods

    (*
      adds an element to the elements collection and returns this container
    *)
    function Add(const AItem : INyxElement) : INyxContainer;
  end;

  { TNyxContainerBaseImpl }
  (*
    base implementation class for all INyxContainer
  *)
  TNyxContainerBaseImpl = class(TNyxElementBaseImpl, INyxContainer)
  strict private
    FElements : INyxElements;
  protected
    function GetElements: INyxElements;
  strict protected
  public
    property Elements : INyxElements read GetElements;

    function Add(const AItem : INyxElement) : INyxContainer;
  end;

  //forward
  INyxUI = interface;

  TNyxActionCallback = procedure(const AUI : INyxUI; const AArgs : array of const);
  TNyxActionNestedCallback = procedure(const AUI : INyxUI; const AArgs : array of const) is nested;
  TNyxActionMethod = procedure(const AUI : INyxUI; const AArgs : array of const) of object;

  { INyxRenderSettings }
  (*
    settings used for rendering the a nyx ui
  *)
  INyxRenderSettings = interface
    ['{91137F74-9500-4A55-98CB-C6924D1FB8F2}']
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
    procedure SetSettings(AValue: INyxRenderSettings);

    //properties
    property Containers : INyxElements read GetContainers;
    property Settings : INyxRenderSettings read GetSettings write SetSettings;

    //methods

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

    function Render() : INyxUI; overload;
    function Render(const ASettings : INyxRenderSettings) : INyxUI; overload;
  end;

implementation

{ TNyxContainerBaseImpl }

function TNyxContainerBaseImpl.GetElements: INyxElements;
begin
  Result := FElements;
end;

function TNyxContainerBaseImpl.Add(const AItem: INyxElement): INyxContainer;
begin
  Result := Self as INyxContainer;
  FElements.Add(AItem);
end;

{ TNyxElementBaseImpl }

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

constructor TNyxElementBaseImpl.Create;
begin
  inherited;
  FID := DoGetID;
end;

end.

