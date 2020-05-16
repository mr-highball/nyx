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
  Classes,
  SysUtils,
  nyx.types,
  nyx.utils.observe;

type

  (*
    enum for all observable events of a button
  *)
  TButtonObserveEvent = (
    boClick
    //todo - add all event types for a button
  );

  //forward
  INyxElementButton = interface;

  (*
    observer method for all button events
  *)
  TButtonObserveMethod = procedure(const AButton : INyxElementButton;
    const AEvent : TButtonObserveEvent) of object;

  { INyxElementButton }
  (*
    base button element
  *)
  INyxElementButton = interface(INyxElement)
    ['{D93EBE55-E5A7-45A8-AEF0-5B04EB806A10}']

    //property methods
    function GetText: String;
    procedure SetText(const AValue: String);

    //property

    (*
      visual text of the button
    *)
    property Text : String read GetText write SetText;

    //methods

    (*
      fluent setter for the button text
    *)
    function UpdateText(const AText : String) : INyxElementButton;

    (*
      callers can attach an observe method with a particular event
      and will get notified when that event occurs
    *)
    function Observe(const AEvent : TButtonObserveEvent; const AObserver : TButtonObserveMethod;
      out ID : String) : INyxElementButton;

    (*
      removes an observer
    *)
    function RemoveObserver(const AID : String) : INyxElementButton;
  end;

  { TNyxElementButtonBaseImpl }
  (*
    base buttom implementation
  *)
  TNyxElementButtonBaseImpl = class(TNyxElementBaseImpl, INyxElementButton)
  strict private
    FObserve : TNyxObservationHelper;
  protected
    function GetText: String;
    procedure SetText(const AValue: String);
  strict protected
    function DoGetText: String; virtual; abstract;
    procedure DoSetText(const AValue: String); virtual; abstract;

    procedure Notify(const AEvent : TButtonObserveEvent);
  public
    property Text : String read GetText write SetText;

    function UpdateText(const AText : String) : INyxElementButton;

    function Observe(const AEvent : TButtonObserveEvent; const AObserver : TButtonObserveMethod;
      out ID : String) : INyxElementButton;
    function RemoveObserver(const AID : String) : INyxElementButton;

    constructor Create; override;
    destructor Destroy; override;
  end;

function NewNyxButton : INyxElementButton;

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

{ TNyxElementButtonBaseImpl }

function TNyxElementButtonBaseImpl.GetText: String;
begin
  Result := DoGetText;
end;

procedure TNyxElementButtonBaseImpl.SetText(const AValue: String);
begin
  DoSetText(AValue);
end;

procedure TNyxElementButtonBaseImpl.Notify(const AEvent: TButtonObserveEvent);
var
  LMethod: TButtonObserveMethod;
  I: Integer;
  LButton: INyxElementButton;
  LObservers: TObserverArray;
begin
  LButton := Self as INyxElementButton;
  LObservers := FObserve.ObserversByEvent(Ord(AEvent));

  for I := 0 to High(LObservers) do
    try
      LMethod := TButtonObserveMethod(LObservers[I]);

      //call the method
      LMethod(LButton, AEvent);
    finally
    end;
end;

function TNyxElementButtonBaseImpl.UpdateText(const AText: String): INyxElementButton;
begin
  Result := Self as INyxElementButton;
  SetText(AText);
end;

function TNyxElementButtonBaseImpl.Observe(const AEvent: TButtonObserveEvent;
  const AObserver: TButtonObserveMethod; out ID: String): INyxElementButton;
begin
  Result := Self as INyxElementButton;

  if not Assigned(AObserver) then
    Exit;

  ID := FObserve.Observe(Ord(AEvent), Pointer(AObserver));
end;

function TNyxElementButtonBaseImpl.RemoveObserver(const AID: String): INyxElementButton;
begin
  Result := Self as INyxElementButton;
  FObserve.RemoveByID(AID);
end;

constructor TNyxElementButtonBaseImpl.Create;
begin
  inherited Create;
  FObserve := TNyxObservationHelper.Create;
end;

destructor TNyxElementButtonBaseImpl.Destroy;
begin
  FObserve.Free;
  inherited Destroy;
end;

initialization
{$IFDEF BROWSER}
  DefaultNyxButton := TNyxElementButtonBrowserImpl;
{$ELSE}
  DefaultNyxButton := TNyxElementButtonStdImpl;
{$ENDIF}
end.

