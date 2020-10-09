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
unit nyx.element.inputmulti;

{$mode delphi}

interface

uses
  nyx.types,
  nyx.utils.observe,
  nyx.element,
  Classes;

type

  (*
    enum for all properties of a InputMulti
  *)
  TInputMultiProperty = (
    imEnabled,
    imText,
    imVisible,
    imLines
  );

  //forward
  INyxElementInputMulti = interface;

  (*
    observer method for element properties
  *)
  TInputMultiPropertyObserveMethod = procedure(const AType : TPropertyUpdateType;
    const AInputMulti : INyxElementInputMulti; const AProperty : TInputMultiProperty) of object;

  { INyxElementInputMulti }
  (*
    base InputMulti element
  *)
  INyxElementInputMulti = interface(INyxElement)
    ['{D93EBE55-E5A7-45A8-AEF0-5B04EB806A10}']

    //property methods
    function GetLines: TStrings;
    function GetText: String;
    procedure SetText(const AValue: String);
    procedure SetEnabled(const AValue: Boolean);
    function GetEnabled: Boolean;
    function GetVisible: Boolean;
    procedure SetVisible(const AValue: Boolean);

    //property

    (*
      visual text of the InputMulti
    *)
    property Text : String read GetText write SetText;

    (*
      determines if the InputMulti is enabled or not
    *)
    property Enabled : Boolean read GetEnabled write SetEnabled;

    (*
      determines if the InputMulti is visible or not
    *)
    property Visible : Boolean read GetVisible write SetVisible;

    (*
      every line in the input
    *)
    property Lines : TStrings read GetLines;

    //methods

    (*
      fluent setter for the InputMulti text
    *)
    function UpdateText(const AText : String) : INyxElementInputMulti;

    (*
      fluent setter for the InputMulti's enabled property
    *)
    function UpdateEnabled(const AEnabled : Boolean) : INyxElementInputMulti;

    (*
      fluent setter for the InputMulti's visible property
    *)
    function UpdateVisible(const AVisible : Boolean) : INyxElementInputMulti;

    function Observe(const AProperty : TInputMultiProperty;
      const AObserver : TInputMultiPropertyObserveMethod; out ID : String) : INyxElementInputMulti; overload;
  end;

  { TNyxElementInputMultiBaseImpl }
  (*
    base InputMulti implementation
  *)
  TNyxElementInputMultiBaseImpl = class(TNyxElementBaseImpl, INyxElementInputMulti)
  strict private
    FPropertyObserve: TNyxObservationHelper;
  protected
    function GetLines: TStrings;
    function GetText: String;
    procedure SetText(const AValue: String);
    procedure SetEnabled(const AValue: Boolean);
    function GetEnabled: Boolean;
    function GetVisible: Boolean;
    procedure SetVisible(const AValue: Boolean);

    procedure DoPropertyNotify(const AType : TPropertyUpdateType;
      const AProperty : TInputMultiProperty);
  strict protected
    function DoGetText: String; virtual; abstract;
    procedure DoSetText(const AValue: String); virtual; abstract;

    function DoGetEnabled: Boolean; virtual; abstract;
    procedure DoSetEnabled(const AValue: Boolean); virtual; abstract;

    function DoGetVisible: Boolean; virtual; abstract;
    procedure DoSetVisible(const AValue: Boolean); virtual; abstract;

    function DoGetLines : TStrings; virtual; abstract;

    procedure DoRemoveObserver(const AID: String); override;
  public
    property Text : String read GetText write SetText;
    property Enabled : Boolean read GetEnabled write SetEnabled;
    property Visible : Boolean read GetVisible write SetVisible;
    property Lines : TStrings read GetLines;

    function UpdateText(const AText : String) : INyxElementInputMulti;
    function UpdateEnabled(const AEnabled : Boolean) : INyxElementInputMulti;
    function UpdateVisible(const AVisible : Boolean) : INyxElementInputMulti;

    function Observe(const AProperty : TInputMultiProperty;
      const AObserver : TInputMultiPropertyObserveMethod; out ID : String) : INyxElementInputMulti; overload;

    constructor Create; override;
    destructor Destroy; override;
  end;

(*
  helper to return a new nyx InputMulti
*)
function NewNyxInputMulti : INyxElementInputMulti;

(*
  helper to be used in a nyx condition method for determining if an element
  is a INyxInputMulti
*)
function IsNyxInputMulti(const AElement : INyxElement) : Boolean;
implementation
uses
{$IFDEF BROWSER}
  nyx.element.InputMulti.browser;
{$ELSE}
  nyx.element.InputMulti.std;
{$ENDIF}
var
  DefaultNyxInputMulti : TNyxElementClass;

function NewNyxInputMulti: INyxElementInputMulti;
begin
  Result := DefaultNyxInputMulti.Create as INyxElementInputMulti;
end;

function IsNyxInputMulti(const AElement: INyxElement): Boolean;
begin
  Result := Assigned(AElement) and (AElement is INyxElementInputMulti);
end;

{ TNyxElementInputMultiBaseImpl }

function TNyxElementInputMultiBaseImpl.GetLines: TStrings;
begin
  Result := DoGetLines;
end;

function TNyxElementInputMultiBaseImpl.GetText: String;
begin
  Result := DoGetText;
end;

procedure TNyxElementInputMultiBaseImpl.SetText(const AValue: String);
begin
  DoSetText(AValue);
end;

procedure TNyxElementInputMultiBaseImpl.SetEnabled(const AValue: Boolean);
begin
  DoSetEnabled(AValue);
end;

function TNyxElementInputMultiBaseImpl.GetEnabled: Boolean;
begin
  Result := DoGetEnabled;
end;

function TNyxElementInputMultiBaseImpl.GetVisible: Boolean;
begin
  Result := DoGetVisible;
end;

procedure TNyxElementInputMultiBaseImpl.SetVisible(const AValue: Boolean);
begin
  DoSetVisible(AValue);
end;

procedure TNyxElementInputMultiBaseImpl.DoPropertyNotify(
  const AType: TPropertyUpdateType; const AProperty: TInputMultiProperty);
var
  LMethod: TInputMultiPropertyObserveMethod;
  I: Integer;
  LInputMulti: INyxElementInputMulti;
  LObservers: TObserverArray;
begin
  LInputMulti := DoGetSelf as INyxElementInputMulti;
  LObservers := FPropertyObserve.ObserversByEvent(Ord(AProperty));

  for I := 0 to High(LObservers) do
    try
      LMethod := TInputMultiPropertyObserveMethod(LObservers[I]);

      //call the method
      LMethod(AType, LInputMulti, AProperty);
    finally
    end;
end;

procedure TNyxElementInputMultiBaseImpl.DoRemoveObserver(const AID: String);
begin
  inherited DoRemoveObserver(AID);
  FPropertyObserve.RemoveByID(AID);
end;

function TNyxElementInputMultiBaseImpl.UpdateText(const AText: String): INyxElementInputMulti;
begin
  Result := DoGetSelf as INyxElementInputMulti;
  SetText(AText);
end;

function TNyxElementInputMultiBaseImpl.UpdateEnabled(const AEnabled: Boolean): INyxElementInputMulti;
begin
  Result := DoGetSelf as INyxElementInputMulti;
  SetEnabled(AEnabled);
end;

function TNyxElementInputMultiBaseImpl.UpdateVisible(const AVisible: Boolean): INyxElementInputMulti;
begin
  Result := DoGetSelf as INyxElementInputMulti;
  SetVisible(AVisible);
end;

function TNyxElementInputMultiBaseImpl.Observe(const AProperty: TInputMultiProperty;
  const AObserver: TInputMultiPropertyObserveMethod; out ID: String
  ): INyxElementInputMulti;
begin
  Result := DoGetSelf as INyxElementInputMulti;

  if not Assigned(AObserver) then
    Exit;

  ID := FPropertyObserve.Observe(Ord(AProperty), Pointer(AObserver));
end;

constructor TNyxElementInputMultiBaseImpl.Create;
begin
  inherited Create;
  FPropertyObserve := TNyxObservationHelper.Create;
end;

destructor TNyxElementInputMultiBaseImpl.Destroy;
begin
  FPropertyObserve.Free;
  inherited Destroy;
end;

initialization
{$IFDEF BROWSER}
  DefaultNyxInputMulti := TNyxElementInputMultiBrowserImpl;
{$ELSE}
  DefaultNyxInputMulti := TNyxElementInputMultiStdImpl;
{$ENDIF}
end.

