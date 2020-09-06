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
unit nyx.element.input;

{$mode delphi}

interface

uses
  nyx.types,
  nyx.utils.observe,
  nyx.element;

type

  (*
    enum for all properties of a Input
  *)
  TInputProperty = (
    ipEnabled,
    ipText,
    ipVisible
  );

  //forward
  INyxElementInput = interface;

  (*
    observer method for element properties
  *)
  TInputPropertyObserveMethod = procedure(const AType : TPropertyUpdateType;
    const AElement : INyxElement; const AProperty : TInputProperty) of object;

  { INyxElementInput }
  (*
    base Input element
  *)
  INyxElementInput = interface(INyxElement)
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
      visual text of the Input
    *)
    property Text : String read GetText write SetText;

    (*
      determines if the Input is enabled or not
    *)
    property Enabled : Boolean read GetEnabled write SetEnabled;

    (*
      determines if the Input is visible or not
    *)
    property Visible : Boolean read GetVisible write SetVisible;

    //methods

    (*
      fluent setter for the Input text
    *)
    function UpdateText(const AText : String) : INyxElementInput;

    (*
      fluent setter for the Input's enabled property
    *)
    function UpdateEnabled(const AEnabled : Boolean) : INyxElementInput;

    (*
      fluent setter for the Input's visible property
    *)
    function UpdateVisible(const AVisible : Boolean) : INyxElementInput;

    (*
      callers can attach an observe method with a particular property
      and will get notified when that event occurs
    *)
    function Observe(const AProperty : TInputProperty; const AObserver : TInputPropertyObserveMethod;
      out ID : String) : INyxElementInput; overload;
  end;

  { TNyxElementInputBaseImpl }
  (*
    base Input implementation
  *)
  TNyxElementInputBaseImpl = class(TNyxElementBaseImpl, INyxElementInput)
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
      const AProperty : TInputProperty);
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

    function UpdateText(const AText : String) : INyxElementInput;
    function UpdateEnabled(const AEnabled : Boolean) : INyxElementInput;
    function UpdateVisible(const AVisible : Boolean) : INyxElementInput;

    function Observe(const AProperty : TInputProperty; const AObserver : TInputPropertyObserveMethod;
      out ID : String) : INyxElementInput; overload;

    constructor Create; override;
    destructor Destroy; override;
  end;

(*
  helper to return a new nyx Input
*)
function NewNyxInput : INyxElementInput;

(*
  helper to be used in a nyx condition method for determining if an element
  is a INyxInput
*)
function IsNyxInput(const AElement : INyxElement) : Boolean;
implementation
uses
{$IFDEF BROWSER}
  nyx.element.input.browser;
{$ELSE}
  nyx.element.input.std;
{$ENDIF}
var
  DefaultNyxInput : TNyxElementClass;

function NewNyxInput: INyxElementInput;
begin
  Result := DefaultNyxInput.Create as INyxElementInput;
end;

function IsNyxInput(const AElement: INyxElement): Boolean;
begin
  Result := Assigned(AElement) and (AElement is INyxElementInput);
end;

{ TNyxElementInputBaseImpl }

function TNyxElementInputBaseImpl.GetText: String;
begin
  Result := DoGetText;
end;

procedure TNyxElementInputBaseImpl.SetText(const AValue: String);
begin
  DoSetText(AValue);
end;

procedure TNyxElementInputBaseImpl.SetEnabled(const AValue: Boolean);
begin
  DoSetEnabled(AValue);
end;

function TNyxElementInputBaseImpl.GetEnabled: Boolean;
begin
  Result := DoGetEnabled;
end;

function TNyxElementInputBaseImpl.GetVisible: Boolean;
begin
  Result := DoGetVisible;
end;

procedure TNyxElementInputBaseImpl.SetVisible(const AValue: Boolean);
begin
  DoSetVisible(AValue);
end;

procedure TNyxElementInputBaseImpl.DoPropertyNotify(
  const AType: TPropertyUpdateType; const AProperty: TInputProperty);
var
  LMethod: TInputPropertyObserveMethod;
  I: Integer;
  LInput: INyxElementInput;
  LObservers: TObserverArray;
begin
  LInput := DoGetSelf as INyxElementInput;
  LObservers := FPropertyObserve.ObserversByEvent(Ord(AProperty));

  for I := 0 to High(LObservers) do
    try
      LMethod := TInputPropertyObserveMethod(LObservers[I]);

      //call the method
      LMethod(AType, LInput, AProperty);
    finally
    end;
end;

procedure TNyxElementInputBaseImpl.DoRemoveObserver(const AID: String);
begin
  inherited DoRemoveObserver(AID);
  FPropertyObserve.RemoveByID(AID);
end;

function TNyxElementInputBaseImpl.UpdateText(const AText: String): INyxElementInput;
begin
  Result := DoGetSelf as INyxElementInput;
  SetText(AText);
end;

function TNyxElementInputBaseImpl.UpdateEnabled(const AEnabled: Boolean): INyxElementInput;
begin
  Result := DoGetSelf as INyxElementInput;
  SetEnabled(AEnabled);
end;

function TNyxElementInputBaseImpl.UpdateVisible(const AVisible: Boolean): INyxElementInput;
begin
  Result := DoGetSelf as INyxElementInput;
  SetVisible(AVisible);
end;

function TNyxElementInputBaseImpl.Observe(const AProperty: TInputProperty;
  const AObserver: TInputPropertyObserveMethod; out ID: String): INyxElementInput;
begin
  Result := DoGetSelf as INyxElementInput;

  if not Assigned(AObserver) then
    Exit;

  ID := FPropertyObserve.Observe(Ord(AProperty), Pointer(AObserver));
end;

constructor TNyxElementInputBaseImpl.Create;
begin
  inherited Create;
  FPropertyObserve := TNyxObservationHelper.Create;
end;

destructor TNyxElementInputBaseImpl.Destroy;
begin
  FPropertyObserve.Free;
  inherited Destroy;
end;

initialization
{$IFDEF BROWSER}
  DefaultNyxInput := TNyxElementInputBrowserImpl;
{$ELSE}
  DefaultNyxInput := TNyxElementInputStdImpl;
{$ENDIF}
end.

