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
unit nyx.element.browser;

{$mode delphi}

interface

uses
  SysUtils,
  web,
  nyx.types,
  nyx.element;

type

  { INyxElementBrowser }
  (*
    base browser element
  *)
  INyxElementBrowser = interface(INyxElement)
    ['{CC8886D4-5AA1-412B-8099-86F93C6870AA}']

    //property methods
    function GetJSElement: TJSElement;
    function GetVisible: Boolean;
    procedure SetVisible(const AValue: Boolean);

    //properties
    property JSElement : TJSElement read GetJSElement;

    (*
      uses display: none to determine element visibility
    *)
    property Visible : Boolean read GetVisible write SetVisible;
  end;


  { TNyxElementBrowserImpl }
  (*
    base implementation for all browser elements
  *)
  TNyxElementBrowserImpl = class(TNyxElementBaseImpl, INyxElementBrowser)
  strict private
    FElement : TJSElement;

    (*
      notifies observers for click events
    *)
    procedure ClickHandler(Event: TEventListenerEvent);

    (*
      notifies observers for double click events
    *)
    procedure DoubleClickHandler(Event: TEventListenerEvent);

    (*
      notifies observers for mouse enter events
    *)
    procedure MouseEnterHandler(Event: TEventListenerEvent);

    (*
      notifies observers for mouse exit events
    *)
    procedure MouseExitHandler(Event: TEventListenerEvent);

    (*
      notifies observers for mouse down events
    *)
    procedure MouseDownHandler(Event: TEventListenerEvent);

    (*
      notifies observers for mouse up events
    *)
    procedure MouseUpHandler(Event: TEventListenerEvent);

    (*
      notifies observers for key up events
    *)
    procedure KeyUpHandler(Event: TEventListenerEvent);

    (*
      notifies observers for key down events
    *)
    procedure KeyDownHandler(Event: TEventListenerEvent);

    (*
      notifies observers for focus events
    *)
    procedure FocusHandler(Event: TEventListenerEvent);

    (*
      notifies observers for lose focus events
    *)
    procedure LoseFocusHandler(Event: TEventListenerEvent);

    procedure InitializeCSS;
    procedure InitializeEvents;
    procedure UpdateSize;
  protected
    function GetJSElement: TJSElement;
    function GetVisible: Boolean;
    procedure SetVisible(const AValue: Boolean);
  strict protected

    (*
      children need to override this method to have document create
      the root JS element associated with this browser element
    *)
    function DoCreateElement : TJSElement; virtual; abstract;

    procedure DoUpdateHeight; override;
    procedure DoUpdateWidth; override;
    procedure DoUpdateMode; override;
  public
    property JSElement : TJSElement read GetJSElement;
    property Visible : Boolean read GetVisible write SetVisible;

    constructor Create; override;
  end;

implementation
uses
  webwidget,
  nyx.utils.browser.css;

{ TNyxElementBrowserImpl }

procedure TNyxElementBrowserImpl.ClickHandler(Event: TEventListenerEvent);
begin
  Notify(evClick);
end;

procedure TNyxElementBrowserImpl.DoubleClickHandler(Event: TEventListenerEvent);
begin
  Notify(evDoubleClick);
end;

procedure TNyxElementBrowserImpl.MouseEnterHandler(Event: TEventListenerEvent);
begin
  Notify(evMouseEnter);
end;

procedure TNyxElementBrowserImpl.MouseExitHandler(Event: TEventListenerEvent);
begin
  Notify(evMouseExit);
end;

procedure TNyxElementBrowserImpl.MouseDownHandler(Event: TEventListenerEvent);
begin
  Notify(evMouseDown);
end;

procedure TNyxElementBrowserImpl.MouseUpHandler(Event: TEventListenerEvent);
begin
  Notify(evMouseUp);
end;

procedure TNyxElementBrowserImpl.KeyUpHandler(Event: TEventListenerEvent);
begin
  Notify(evKeyUp);
end;

procedure TNyxElementBrowserImpl.KeyDownHandler(Event: TEventListenerEvent);
begin
  Notify(evKeyDown);
end;

procedure TNyxElementBrowserImpl.FocusHandler(Event: TEventListenerEvent);
begin
  Notify(evFocus)
end;

procedure TNyxElementBrowserImpl.LoseFocusHandler(Event: TEventListenerEvent);
begin
  Notify(evLoseFocus);
end;

procedure TNyxElementBrowserImpl.InitializeCSS;
var
  LCSS : TNyxCSSHelper;
  LStyle: String;
begin
  LCSS := TNyxCSSHelper.Create;
  try
    LStyle := FElement.getAttribute('style');

    if not Assigned(LStyle) then
      LStyle := '';

    LCSS.CSS := LStyle;

    FElement.setAttribute('style', LCSS.CSS);
  finally
    LCSS.Free;
  end;
end;

procedure TNyxElementBrowserImpl.InitializeEvents;
begin
  JSElement.addEventListener(sEventClick, @ClickHandler);
  JSElement.addEventListener(sEventDblClick, @DoubleClickHandler);
  JSElement.addEventListener(sEventMouseEnter, @MouseEnterHandler);
  JSElement.addEventListener(sEventMouseLeave, @MouseExitHandler);
  JSElement.addEventListener(sEventMouseDown, @MouseDownHandler);
  JSElement.addEventListener(sEventMouseUp, @MouseUpHandler);
  JSElement.addEventListener(sEventKeyUp, @KeyUpHandler);
  JSElement.addEventListener(sEventKeyDown, @KeyDownHandler);
  JSElement.addEventListener(sEventFocus, @FocusHandler);
  JSElement.addEventListener(sEventBlur, @LoseFocusHandler);
end;

procedure TNyxElementBrowserImpl.UpdateSize;
var
  LCSS : TNyxCSSHelper;
  LSelf: INyxElementBrowser;
begin
  LSelf := DoGetSelf as INyxElementBrowser;
  LCSS := TNyxCSSHelper.Create;
  try
    //copy the current style
    LCSS.CopyFromElement(LSelf);

    //depending on mode, update css differently
    if Size.Mode = smFixed then
    begin
      if Size.Height >= 0 then
        LCSS['height'] := IntToStr(Round(Size.Height)) + 'px'
      else
        LCSS.Delete('height');

      if Size.Width >= 0 then
        LCSS['width'] := IntToStr(Round(Size.Width)) + 'px'
      else
        LCSS.Delete('width');
    end
    else
    begin
      if Size.Height >= 0 then
        LCSS['height'] := IntToStr(Round(Size.Height * 100)) + '%'
      else
        LCSS.Delete('height');

      if Size.Width >= 0 then
        LCSS['width'] := IntToStr(Round(Size.Width * 100)) + '%'
      else
        LCSS.Delete('width');
    end;

    //update the new style
    LCSS.CopyToElement(LSelf);
  finally
    LCSS.Free;
  end;
end;

function TNyxElementBrowserImpl.GetJSElement: TJSElement;
begin
  Result := FElement;
end;

function TNyxElementBrowserImpl.GetVisible: Boolean;
var
  LCSS: TNyxCSSHelper;
  LStyle: String;
begin
  if not Assigned(FElement) then
    Exit(False);

  LCSS := TNyxCSSHelper.Create;
  try
    LStyle := FElement.getAttribute('style');

    //when no style, then visibility hasn't been set (so we are visible)
    if not Assigned(LStyle) then
      Exit(True);

    LCSS.CSS := LStyle;

    //we use display none
    Result := not (LCSS.Exists('display') and (LowerCase(LCSS['display']) = 'none'));
  finally
    LCSS.Free;
  end;
end;

procedure TNyxElementBrowserImpl.SetVisible(const AValue: Boolean);
var
  LCSS: TNyxCSSHelper;
  LStyle: String;
begin
  if not Assigned(FElement) then
    Exit;

  LCSS := TNyxCSSHelper.Create;
  try
    LStyle := FElement.getAttribute('style');

    if not Assigned(LStyle) then
      LStyle := '';

    LCSS.CSS := LStyle;

    //we use display none
    if AValue then
      LCSS.Delete('display')
    else
      LCSS.Upsert('display', 'none');

    //set the css
    FElement.setAttribute('style', LCSS.CSS);
  finally
    LCSS.Free;
  end;
end;

procedure TNyxElementBrowserImpl.DoUpdateHeight;
begin
  UpdateSize;
end;

procedure TNyxElementBrowserImpl.DoUpdateWidth;
begin
  UpdateSize;
end;

procedure TNyxElementBrowserImpl.DoUpdateMode;
begin
  UpdateSize;
end;

constructor TNyxElementBrowserImpl.Create;
begin
  inherited Create;
  FElement :=  DoCreateElement;
  FElement.id := ID;
  InitializeCSS;
  InitializeEvents;
end;

end.

