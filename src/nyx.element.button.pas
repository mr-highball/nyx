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
unit nyx.element.button;

{$mode delphi}

interface

uses
  nyx.types,
  nyx.utils.observe,
  nyx.element;

type

  (*
    enum for all properties of a button
  *)
  TButtonProperty = (
    bpEnabled,
    bpText,
    bpVisible
  );

  //forward
  INyxElementButton = interface;

  (*
    observer method for element properties
  *)
  TButtonPropertyObserveMethod = procedure(const AType : TPropertyUpdateType;
    const AButton : INyxElementButton; const AProperty : TButtonProperty) of object;

  { INyxElementButton }
  (*
    base button element
  *)
  INyxElementButton = interface(INyxElement)
    ['{D93EBE55-E5A7-45A8-AEF0-5B04EB806A10}']

    //property methods
    function GetText: String;
    procedure SetText(const AValue: String);
    procedure SetEnabled(const AValue: Boolean);
    function GetEnabled: Boolean;
    function GetVisible: Boolean;
    procedure SetVisible(const AValue: Boolean);

    //property

    (*
      visual text of the button
    *)
    property Text : String read GetText write SetText;

    (*
      determines if the button is enabled or not
    *)
    property Enabled : Boolean read GetEnabled write SetEnabled;

    (*
      determines if the button is visible or not
    *)
    property Visible : Boolean read GetVisible write SetVisible;

    //methods

    (*
      fluent setter for the button text
    *)
    function UpdateText(const AText : String) : INyxElementButton;

    (*
      fluent setter for the button's enabled property
    *)
    function UpdateEnabled(const AEnabled : Boolean) : INyxElementButton;

    (*
      fluent setter for the button's visible property
    *)
    function UpdateVisible(const AVisible : Boolean) : INyxElementButton;

    function Observe(const AProperty : TButtonProperty;
      const AObserver : TButtonPropertyObserveMethod; out ID : String) : INyxElementButton; overload;
  end;

  { TNyxElementButtonBaseImpl }
  (*
    base button implementation
  *)
  TNyxElementButtonBaseImpl = class(TNyxElementBaseImpl, INyxElementButton)
  strict private
    FPropertyObserve: TNyxObservationHelper;
  protected
    function GetText: String;
    procedure SetText(const AValue: String);
    procedure SetEnabled(const AValue: Boolean);
    function GetEnabled: Boolean;
    function GetVisible: Boolean;
    procedure SetVisible(const AValue: Boolean);

    procedure DoPropertyNotify(const AType : TPropertyUpdateType;
      const AProperty : TButtonProperty);
  strict protected
    function DoGetText: String; virtual; abstract;
    procedure DoSetText(const AValue: String); virtual; abstract;

    function DoGetEnabled: Boolean; virtual; abstract;
    procedure DoSetEnabled(const AValue: Boolean); virtual; abstract;

    function DoGetVisible: Boolean; virtual; abstract;
    procedure DoSetVisible(const AValue: Boolean); virtual; abstract;

    procedure DoRemoveObserver(const AID: String); override;
  public
    property Text : String read GetText write SetText;
    property Enabled : Boolean read GetEnabled write SetEnabled;
    property Visible : Boolean read GetVisible write SetVisible;

    function UpdateText(const AText : String) : INyxElementButton;
    function UpdateEnabled(const AEnabled : Boolean) : INyxElementButton;
    function UpdateVisible(const AVisible : Boolean) : INyxElementButton;

    function Observe(const AProperty : TButtonProperty;
      const AObserver : TButtonPropertyObserveMethod; out ID : String) : INyxElementButton; overload;

    constructor Create; override;
    destructor Destroy; override;
  end;

(*
  helper to return a new nyx button
*)
function NewNyxButton : INyxElementButton;

(*
  helper to be used in a nyx condition method for determining if an element
  is a INyxButton
*)
function IsNyxButton(const AElement : INyxElement) : Boolean;
implementation
uses
{$IFDEF BROWSER}
  nyx.element.button.browser;
{$ELSE}
  nyx.element.button.std;
{$ENDIF}
var
  DefaultNyxButton : TNyxElementClass;

function NewNyxButton: INyxElementButton;
begin
  Result := DefaultNyxButton.Create as INyxElementButton;
end;

function IsNyxButton(const AElement: INyxElement): Boolean;
begin
  Result := Assigned(AElement) and (AElement is INyxElementButton);
end;

{ TNyxElementButtonBaseImpl }

function TNyxElementButtonBaseImpl.GetText: String;
begin
  Result := DoGetText;
end;

procedure TNyxElementButtonBaseImpl.SetText(const AValue: String);
begin
  DoPropertyNotify(puBeforeUpdate, bpText);
  DoSetText(AValue);
  DoPropertyNotify(puAfterUpdate, bpText);
end;

procedure TNyxElementButtonBaseImpl.SetEnabled(const AValue: Boolean);
begin
  DoPropertyNotify(puBeforeUpdate, bpEnabled);
  DoSetEnabled(AValue);
  DoPropertyNotify(puAfterUpdate, bpEnabled);
end;

function TNyxElementButtonBaseImpl.GetEnabled: Boolean;
begin
  Result := DoGetEnabled;
end;

function TNyxElementButtonBaseImpl.GetVisible: Boolean;
begin
  Result := DoGetVisible;
end;

procedure TNyxElementButtonBaseImpl.SetVisible(const AValue: Boolean);
begin
  DoPropertyNotify(puBeforeUpdate, bpVisible);
  DoSetVisible(AValue);
  DoPropertyNotify(puAfterUpdate, bpVisible);
end;

procedure TNyxElementButtonBaseImpl.DoPropertyNotify(
  const AType: TPropertyUpdateType; const AProperty: TButtonProperty);
var
  LMethod: TButtonPropertyObserveMethod;
  I: Integer;
  LButton: INyxElementButton;
  LObservers: TObserverArray;
begin
  LButton := DoGetSelf as INyxElementButton;
  LObservers := FPropertyObserve.ObserversByEvent(Ord(AProperty));

  for I := 0 to High(LObservers) do
    try
      LMethod := TButtonPropertyObserveMethod(LObservers[I]);

      //call the method
      LMethod(AType, LButton, AProperty);
    finally
    end;
end;

procedure TNyxElementButtonBaseImpl.DoRemoveObserver(const AID: String);
begin
  inherited DoRemoveObserver(AID);
  FPropertyObserve.RemoveByID(AID);
end;

function TNyxElementButtonBaseImpl.UpdateText(const AText: String): INyxElementButton;
begin
  Result := DoGetSelf as INyxElementButton;
  SetText(AText);
end;

function TNyxElementButtonBaseImpl.UpdateEnabled(const AEnabled: Boolean): INyxElementButton;
begin
  Result := DoGetSelf as INyxElementButton;
  SetEnabled(AEnabled);
end;

function TNyxElementButtonBaseImpl.UpdateVisible(const AVisible: Boolean): INyxElementButton;
begin
  Result := DoGetSelf as INyxElementButton;
  SetVisible(AVisible);
end;

function TNyxElementButtonBaseImpl.Observe(const AProperty: TButtonProperty;
  const AObserver: TButtonPropertyObserveMethod; out ID: String
  ): INyxElementButton;
begin
  Result := DoGetSelf as INyxElementButton;

  if not Assigned(AObserver) then
    Exit;

  ID := FPropertyObserve.Observe(Ord(AProperty), Pointer(AObserver));
end;

constructor TNyxElementButtonBaseImpl.Create;
begin
  inherited Create;
  FPropertyObserve := TNyxObservationHelper.Create;
end;

destructor TNyxElementButtonBaseImpl.Destroy;
begin
  FPropertyObserve.Free;
  inherited Destroy;
end;

initialization
{$IFDEF BROWSER}
  DefaultNyxButton := TNyxElementButtonBrowserImpl;
{$ELSE}
  DefaultNyxButton := TNyxElementButtonStdImpl;
{$ENDIF}
end.

