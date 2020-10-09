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
unit nyx.element.checkbox;

{$mode delphi}

interface

uses
  nyx.types,
  nyx.utils.observe,
  nyx.element;

type

  (*
    enum for all properties of a Checkbox
  *)
  TCheckboxProperty = (
    cpEnabled,
    cpText,
    cpVisible,
    cpChecked
  );

  //forward
  INyxElementCheckbox = interface;

  (*
    observer method for element properties
  *)
  TCheckboxPropertyObserveMethod = procedure(const AType : TPropertyUpdateType;
    const ACheckbox : INyxElementCheckbox; const AProperty : TCheckboxProperty) of object;

  { INyxElementCheckbox }
  (*
    base Checkbox element
  *)
  INyxElementCheckbox = interface(INyxElement)
    ['{D93EBE55-E5A7-45A8-AEF0-5B04EB806A10}']
    function GetChecked: Boolean;

    //property methods
    function GetText: String;
    procedure SetChecked(const AValue: Boolean);
    procedure SetText(const AValue: String);
    procedure SetEnabled(const AValue: Boolean);
    function GetEnabled: Boolean;
    function GetVisible: Boolean;
    procedure SetVisible(const AValue: Boolean);

    //property

    (*
      visual text of the Checkbox
    *)
    property Text : String read GetText write SetText;

    (*
      determines if the Checkbox is enabled or not
    *)
    property Enabled : Boolean read GetEnabled write SetEnabled;

    (*
      determines if the Checkbox is visible or not
    *)
    property Visible : Boolean read GetVisible write SetVisible;

    (*
      whether this checkbox is "checked" or not
    *)
    property Checked : Boolean read GetChecked write SetChecked;

    //methods

    (*
      fluent setter for the Checkbox text
    *)
    function UpdateText(const AText : String) : INyxElementCheckbox;

    (*
      fluent setter for the Checkbox's enabled property
    *)
    function UpdateEnabled(const AEnabled : Boolean) : INyxElementCheckbox;

    (*
      fluent setter for the Checkbox's checked property
    *)
    function UpdateChecked(const AChecked : Boolean) : INyxElementCheckbox;

    (*
      fluent setter for the Checkbox's visible property
    *)
    function UpdateVisible(const AVisible : Boolean) : INyxElementCheckbox;

    function Observe(const AProperty : TCheckboxProperty;
      const AObserver : TCheckboxPropertyObserveMethod; out ID : String) : INyxElementCheckbox; overload;
  end;

  { TNyxElementCheckboxBaseImpl }
  (*
    base Checkbox implementation
  *)
  TNyxElementCheckboxBaseImpl = class(TNyxElementBaseImpl, INyxElementCheckbox)
  strict private
    FPropertyObserve: TNyxObservationHelper;
  protected
    function GetText: String;
    procedure SetText(const AValue: String);
    procedure SetEnabled(const AValue: Boolean);
    function GetEnabled: Boolean;
    function GetVisible: Boolean;
    procedure SetVisible(const AValue: Boolean);
    function GetChecked: Boolean;
    procedure SetChecked(const AValue: Boolean);

    procedure DoPropertyNotify(const AType : TPropertyUpdateType;
      const AProperty : TCheckboxProperty);
  strict protected
    function DoGetText: String; virtual; abstract;
    procedure DoSetText(const AValue: String); virtual; abstract;

    function DoGetEnabled: Boolean; virtual; abstract;
    procedure DoSetEnabled(const AValue: Boolean); virtual; abstract;

    function DoGetVisible: Boolean; virtual; abstract;
    procedure DoSetVisible(const AValue: Boolean); virtual; abstract;

    function DoGetChecked: Boolean; virtual; abstract;
    procedure DoSetChecked(const AValue: Boolean); virtual; abstract;

    procedure DoRemoveObserver(const AID: String); override;
  public
    property Text : String read GetText write SetText;
    property Enabled : Boolean read GetEnabled write SetEnabled;
    property Visible : Boolean read GetVisible write SetVisible;

    function UpdateText(const AText : String) : INyxElementCheckbox;
    function UpdateEnabled(const AEnabled : Boolean) : INyxElementCheckbox;
    function UpdateVisible(const AVisible : Boolean) : INyxElementCheckbox;
    function UpdateChecked(const AChecked: Boolean): INyxElementCheckbox;

    function Observe(const AProperty : TCheckboxProperty;
      const AObserver : TCheckboxPropertyObserveMethod; out ID : String) : INyxElementCheckbox; overload;

    constructor Create; override;
    destructor Destroy; override;
  end;

(*
  helper to return a new nyx Checkbox
*)
function NewNyxCheckbox : INyxElementCheckbox;

(*
  helper to be used in a nyx condition method for determining if an element
  is a INyxCheckbox
*)
function IsNyxCheckbox(const AElement : INyxElement) : Boolean;
implementation
uses
{$IFDEF BROWSER}
  nyx.element.Checkbox.browser;
{$ELSE}
  nyx.element.Checkbox.std;
{$ENDIF}
var
  DefaultNyxCheckbox : TNyxElementClass;

function NewNyxCheckbox: INyxElementCheckbox;
begin
  Result := DefaultNyxCheckbox.Create as INyxElementCheckbox;
end;

function IsNyxCheckbox(const AElement: INyxElement): Boolean;
begin
  Result := Assigned(AElement) and (AElement is INyxElementCheckbox);
end;

{ TNyxElementCheckboxBaseImpl }

function TNyxElementCheckboxBaseImpl.GetText: String;
begin
  Result := DoGetText;
end;

procedure TNyxElementCheckboxBaseImpl.SetText(const AValue: String);
begin
  DoSetText(AValue);
end;

procedure TNyxElementCheckboxBaseImpl.SetEnabled(const AValue: Boolean);
begin
  DoSetEnabled(AValue);
end;

function TNyxElementCheckboxBaseImpl.GetEnabled: Boolean;
begin
  Result := DoGetEnabled;
end;

function TNyxElementCheckboxBaseImpl.GetVisible: Boolean;
begin
  Result := DoGetVisible;
end;

procedure TNyxElementCheckboxBaseImpl.SetVisible(const AValue: Boolean);
begin
  DoSetVisible(AValue);
end;

procedure TNyxElementCheckboxBaseImpl.DoPropertyNotify(
  const AType: TPropertyUpdateType; const AProperty: TCheckboxProperty);
var
  LMethod: TCheckboxPropertyObserveMethod;
  I: Integer;
  LCheckbox: INyxElementCheckbox;
  LObservers: TObserverArray;
begin
  LCheckbox := DoGetSelf as INyxElementCheckbox;
  LObservers := FPropertyObserve.ObserversByEvent(Ord(AProperty));

  for I := 0 to High(LObservers) do
    try
      LMethod := TCheckboxPropertyObserveMethod(LObservers[I]);

      //call the method
      LMethod(AType, LCheckbox, AProperty);
    finally
    end;
end;

procedure TNyxElementCheckboxBaseImpl.DoRemoveObserver(const AID: String);
begin
  inherited DoRemoveObserver(AID);
  FPropertyObserve.RemoveByID(AID);
end;

function TNyxElementCheckboxBaseImpl.GetChecked: Boolean;
begin
  Result := DoGetChecked;
end;

procedure TNyxElementCheckboxBaseImpl.SetChecked(const AValue: Boolean);
begin
  DoPropertyNotify(puBeforeUpdate, cpChecked);
  DoSetChecked(AValue);
  DoPropertyNotify(puAfterUpdate, cpChecked);
end;

function TNyxElementCheckboxBaseImpl.UpdateChecked(const AChecked: Boolean
  ): INyxElementCheckbox;
begin
  Result := DoGetSelf as INyxElementCheckbox;
  SetChecked(AChecked);
end;

function TNyxElementCheckboxBaseImpl.Observe(
  const AProperty: TCheckboxProperty;
  const AObserver: TCheckboxPropertyObserveMethod; out ID: String
  ): INyxElementCheckbox;
begin
  Result := DoGetSelf as INyxElementCheckbox;

  if not Assigned(AObserver) then
    Exit;

  ID := FPropertyObserve.Observe(Ord(AProperty), Pointer(AObserver));
end;

function TNyxElementCheckboxBaseImpl.UpdateText(const AText: String): INyxElementCheckbox;
begin
  Result := DoGetSelf as INyxElementCheckbox;
  SetText(AText);
end;

function TNyxElementCheckboxBaseImpl.UpdateEnabled(const AEnabled: Boolean): INyxElementCheckbox;
begin
  Result := DoGetSelf as INyxElementCheckbox;
  SetEnabled(AEnabled);
end;

function TNyxElementCheckboxBaseImpl.UpdateVisible(const AVisible: Boolean): INyxElementCheckbox;
begin
  Result := DoGetSelf as INyxElementCheckbox;
  SetVisible(AVisible);
end;

constructor TNyxElementCheckboxBaseImpl.Create;
begin
  inherited Create;
  FPropertyObserve := TNyxObservationHelper.Create;
end;

destructor TNyxElementCheckboxBaseImpl.Destroy;
begin
  FPropertyObserve.Free;
  inherited Destroy;
end;

initialization
{$IFDEF BROWSER}
  DefaultNyxCheckbox := TNyxElementCheckboxBrowserImpl;
{$ELSE}
  DefaultNyxCheckbox := TNyxElementCheckboxStdImpl;
{$ENDIF}
end.

