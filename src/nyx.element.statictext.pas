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
unit nyx.element.statictext;

{$mode delphi}

interface

uses
  nyx.types,
  nyx.utils.observe,
  nyx.element;

type

  (*
    enum for all properties of a StaticText
  *)
  TStaticTextProperty = (
    stEnabled,
    stText,
    stVisible,
    stFormat
  );

  TStaticTextFormat = (
    sfBold,
    sfItalic,
    sfUnderline,
    sfStrikeThrough
  );

  TStaticTextFormats = set of TStaticTextFormat;

  //forward
  INyxElementStaticText = interface;

  (*
    observer method for element properties
  *)
  TStaticTextPropertyObserveMethod = procedure(const AType : TPropertyUpdateType;
    const AStaticText : INyxElementStaticText; const AProperty : TStaticTextProperty) of object;

  { INyxElementStaticText }
  (*
    base StaticText element
  *)
  INyxElementStaticText = interface(INyxElement)
    ['{4F43A951-3E56-474E-8141-32E81C253BF0}']

    //property methods
    function GetText: String;
    procedure SetFormat(const AValue: TStaticTextFormats);
    function GetFormat: TStaticTextFormats;
    procedure SetText(const AValue: String);
    procedure SetEnabled(const AValue: Boolean);
    function GetEnabled: Boolean;
    function GetVisible: Boolean;
    procedure SetVisible(const AValue: Boolean);

    //property

    (*
      visual text of the StaticText
    *)
    property Text : String read GetText write SetText;

    (*
      determines if the StaticText is enabled or not
    *)
    property Enabled : Boolean read GetEnabled write SetEnabled;

    (*
      determines if the StaticText is visible or not
    *)
    property Visible : Boolean read GetVisible write SetVisible;


    (*
      defines the text format for a static text element
    *)
    property Format : TStaticTextFormats read GetFormat write SetFormat;

    //methods

    (*
      fluent setter for the StaticText text
    *)
    function UpdateText(const AText : String) : INyxElementStaticText;

    (*
      fluent setter for the StaticText's enabled property
    *)
    function UpdateEnabled(const AEnabled : Boolean) : INyxElementStaticText;

    (*
      fluent setter for the StaticText's visible property
    *)
    function UpdateVisible(const AVisible : Boolean) : INyxElementStaticText;

    (*
      fluent setter for format property
    *)
    function UpdateFormat(const AFormat : TStaticTextFormats) : INyxElementStaticText;

    function Observe(const AProperty : TStaticTextProperty;
      const AObserver : TStaticTextPropertyObserveMethod; out ID : String) : INyxElementStaticText; overload;
  end;

  { TNyxElementStaticTextBaseImpl }
  (*
    base StaticText implementation
  *)
  TNyxElementStaticTextBaseImpl = class(TNyxElementBaseImpl, INyxElementStaticText)
  strict private
    FPropertyObserve: TNyxObservationHelper;
  protected
    function GetText: String;
    procedure SetText(const AValue: String);
    procedure SetEnabled(const AValue: Boolean);
    function GetEnabled: Boolean;
    function GetVisible: Boolean;
    procedure SetVisible(const AValue: Boolean);
    procedure SetFormat(const AValue: TStaticTextFormats);
    function GetFormat: TStaticTextFormats;

    procedure DoPropertyNotify(const AType : TPropertyUpdateType;
      const AProperty : TStaticTextProperty);
  strict protected
    function DoGetText: String; virtual; abstract;
    procedure DoSetText(const AValue: String); virtual; abstract;

    function DoGetEnabled: Boolean; virtual; abstract;
    procedure DoSetEnabled(const AValue: Boolean); virtual; abstract;

    function DoGetVisible: Boolean; virtual; abstract;
    procedure DoSetVisible(const AValue: Boolean); virtual; abstract;

    procedure DoSetFormat(const AValue: TStaticTextFormats); virtual; abstract;
    function DoGetFormat: TStaticTextFormats; virtual; abstract;

    procedure DoRemoveObserver(const AID: String); override;
  public
    property Text : String read GetText write SetText;
    property Enabled : Boolean read GetEnabled write SetEnabled;
    property Visible : Boolean read GetVisible write SetVisible;
    property Format : TStaticTextFormats read GetFormat write SetFormat;

    function UpdateText(const AText : String) : INyxElementStaticText;
    function UpdateEnabled(const AEnabled : Boolean) : INyxElementStaticText;
    function UpdateVisible(const AVisible : Boolean) : INyxElementStaticText;
    function UpdateFormat(const AFormat : TStaticTextFormats) : INyxElementStaticText;

    function Observe(const AProperty : TStaticTextProperty;
      const AObserver : TStaticTextPropertyObserveMethod; out ID : String) : INyxElementStaticText; overload;

    constructor Create; override;
    destructor Destroy; override;
  end;

(*
  helper to return a new nyx StaticText
*)
function NewNyxStaticText : INyxElementStaticText;

(*
  helper to be used in a nyx condition method for determining if an element
  is a INyxStaticText
*)
function IsNyxStaticText(const AElement : INyxElement) : Boolean;
implementation
uses
{$IFDEF BROWSER}
  nyx.element.statictext.browser;
{$ELSE}
  nyx.element.statictext.std;
{$ENDIF}
var
  DefaultNyxStaticText : TNyxElementClass;

function NewNyxStaticText: INyxElementStaticText;
begin
  Result := DefaultNyxStaticText.Create as INyxElementStaticText;
end;

function IsNyxStaticText(const AElement: INyxElement): Boolean;
begin
  Result := Assigned(AElement) and (AElement is INyxElementStaticText);
end;

{ TNyxElementStaticTextBaseImpl }

function TNyxElementStaticTextBaseImpl.GetText: String;
begin
  Result := DoGetText;
end;

procedure TNyxElementStaticTextBaseImpl.SetText(const AValue: String);
begin
  DoPropertyNotify(puBeforeUpdate, stText);
  DoSetText(AValue);
  DoPropertyNotify(puAfterUpdate, stText);
end;

procedure TNyxElementStaticTextBaseImpl.SetEnabled(const AValue: Boolean);
begin
  DoPropertyNotify(puBeforeUpdate, stEnabled);
  DoSetEnabled(AValue);
  DoPropertyNotify(puAfterUpdate, stEnabled);
end;

function TNyxElementStaticTextBaseImpl.GetEnabled: Boolean;
begin
  Result := DoGetEnabled;
end;

function TNyxElementStaticTextBaseImpl.GetVisible: Boolean;
begin
  Result := DoGetVisible;
end;

procedure TNyxElementStaticTextBaseImpl.SetVisible(const AValue: Boolean);
begin
  DoPropertyNotify(puBeforeUpdate, stVisible);
  DoSetVisible(AValue);
  DoPropertyNotify(puAfterUpdate, stVisible);
end;

procedure TNyxElementStaticTextBaseImpl.SetFormat(
  const AValue: TStaticTextFormats);
begin
  DoPropertyNotify(puBeforeUpdate, stFormat);
  DoSetFormat(AValue);
  DoPropertyNotify(puAfterUpdate, stFormat);
end;

function TNyxElementStaticTextBaseImpl.GetFormat: TStaticTextFormats;
begin
  Result := DoGetFormat;
end;

procedure TNyxElementStaticTextBaseImpl.DoPropertyNotify(
  const AType: TPropertyUpdateType; const AProperty: TStaticTextProperty);
var
  LMethod: TStaticTextPropertyObserveMethod;
  I: Integer;
  LStaticText: INyxElementStaticText;
  LObservers: TObserverArray;
begin
  LStaticText := DoGetSelf as INyxElementStaticText;
  LObservers := FPropertyObserve.ObserversByEvent(Ord(AProperty));

  for I := 0 to High(LObservers) do
    try
      LMethod := TStaticTextPropertyObserveMethod(LObservers[I]);

      //call the method
      LMethod(AType, LStaticText, AProperty);
    finally
    end;
end;

procedure TNyxElementStaticTextBaseImpl.DoRemoveObserver(const AID: String);
begin
  inherited DoRemoveObserver(AID);
  FPropertyObserve.RemoveByID(AID);
end;

function TNyxElementStaticTextBaseImpl.UpdateText(const AText: String): INyxElementStaticText;
begin
  Result := DoGetSelf as INyxElementStaticText;
  SetText(AText);
end;

function TNyxElementStaticTextBaseImpl.UpdateEnabled(const AEnabled: Boolean): INyxElementStaticText;
begin
  Result := DoGetSelf as INyxElementStaticText;
  SetEnabled(AEnabled);
end;

function TNyxElementStaticTextBaseImpl.UpdateVisible(const AVisible: Boolean): INyxElementStaticText;
begin
  Result := DoGetSelf as INyxElementStaticText;
  SetVisible(AVisible);
end;

function TNyxElementStaticTextBaseImpl.UpdateFormat(
  const AFormat: TStaticTextFormats): INyxElementStaticText;
begin
  Result := DoGetSelf as INyxElementStaticText;
  SetFormat(AFormat);
end;

function TNyxElementStaticTextBaseImpl.Observe(const AProperty: TStaticTextProperty;
  const AObserver: TStaticTextPropertyObserveMethod; out ID: String
  ): INyxElementStaticText;
begin
  Result := DoGetSelf as INyxElementStaticText;

  if not Assigned(AObserver) then
    Exit;

  ID := FPropertyObserve.Observe(Ord(AProperty), Pointer(AObserver));
end;

constructor TNyxElementStaticTextBaseImpl.Create;
begin
  inherited Create;
  FPropertyObserve := TNyxObservationHelper.Create;
end;

destructor TNyxElementStaticTextBaseImpl.Destroy;
begin
  FPropertyObserve.Free;
  inherited Destroy;
end;

initialization
{$IFDEF BROWSER}
  DefaultNyxStaticText := TNyxElementStaticTextBrowserImpl;
{$ELSE}
  DefaultNyxStaticText := TNyxElementStaticTextStdImpl;
{$ENDIF}
end.

