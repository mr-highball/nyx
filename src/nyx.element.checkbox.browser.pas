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
unit nyx.element.checkbox.browser;

{$mode delphi}

interface

uses
  web,
  nyx.types,
  nyx.element.browser,
  nyx.element.Checkbox;

type

  { TNyxElementCheckboxBrowserImpl }
  (*
    base implementation for all browser nyx Checkboxs
  *)
  TNyxElementCheckboxBrowserImpl = class(TNyxElementCheckboxBaseImpl, INyxElementBrowser, INyxElementCheckbox)
  strict private
    FBrowser : TNyxElementBrowserImpl;

    (*
      will notify before change of the "checked" property once a user
      has moused down over the element only if the element is enabled
    *)
    procedure MouseDownHandler(Event : TEventListenerEvent);

    (*
      will notify for the change of the "checked" property once a user
      has clicked
    *)
    procedure ClickHandler(Event : TEventListenerEvent);
  protected
    function GetBrowser: TNyxElementBrowserImpl;

    type


      { TBrowserElementComponent }
      (*
        component which handles the INyxElement contract
      *)
      TBrowserElementComponent = class(TNyxElementBrowserImpl)
      strict protected
        function DoCreateElement: TJSElement; override;
        function DoGetSelf: INyxElement; override;
      public
        Parent : TNyxElementCheckboxBrowserImpl;
        Check : TJSHTMLInputElement;
        Lbl : TJSHTMLLabelElement;
      end;

  strict protected
    function DoGetChecked: Boolean; override;
    procedure DoSetChecked(const AValue: Boolean); override;

    procedure DoSetText(const AValue: String); override;
    function DoGetText: String; override;

    function DoGetEnabled: Boolean; override;
    procedure DoSetEnabled(const AValue: Boolean); override;

    function DoGetVisible: Boolean; override;
    procedure DoSetVisible(const AValue: Boolean); override;


    (*
      because we use composition, we need to redirect calls
    *)
    procedure DoUpdateHeight; override;
    procedure DoUpdateWidth; override;
    procedure DoUpdateMode; override;

    function DoGetContainer: INyxContainer; override;
    procedure DoSetContainer(const AValue: INyxContainer); override;

  public
    property BrowserElementImpl : TNyxElementBrowserImpl read GetBrowser implements INyxElementBrowser;

    constructor Create; override;
    destructor Destroy; override;
  end;

implementation
uses
  webwidget,
  nyx.utils.browser.css;

{ TNyxElementCheckboxBrowserImpl }

function TNyxElementCheckboxBrowserImpl.DoGetChecked: Boolean;
begin
  Result := TBrowserElementComponent(FBrowser).Check.checked;
end;

procedure TNyxElementCheckboxBrowserImpl.DoSetChecked(const AValue: Boolean);
begin
  TBrowserElementComponent(FBrowser).Check.checked := AValue;
end;

procedure TNyxElementCheckboxBrowserImpl.MouseDownHandler(
  Event: TEventListenerEvent);
begin
  if Enabled and Visible then
    DoPropertyNotify(puBeforeUpdate, cpChecked);
end;

procedure TNyxElementCheckboxBrowserImpl.ClickHandler(Event: TEventListenerEvent);
begin
  //check for exact id match to avoid the label triggering the event twice
  if Event.targetElement.id = TBrowserElementComponent(FBrowser).Check.id then
    DoPropertyNotify(puAfterUpdate, cpChecked);
end;

function TNyxElementCheckboxBrowserImpl.GetBrowser: TNyxElementBrowserImpl;
begin
  //we have to do this at the time we are adding to dom
  if FBrowser.JSElement.children.length < 1 then
  begin
    FBrowser.JSElement.appendChild(TBrowserElementComponent(FBrowser).Check);
    FBrowser.JSElement.appendChild(TBrowserElementComponent(FBrowser).Lbl);
  end;

  Result := FBrowser;
end;

procedure TNyxElementCheckboxBrowserImpl.DoSetText(const AValue: String);
begin
  TBrowserElementComponent(FBrowser).Lbl.textContent := AValue;
end;

function TNyxElementCheckboxBrowserImpl.DoGetText: String;
begin
  Result := TBrowserElementComponent(FBrowser).Lbl.textContent;
end;

function TNyxElementCheckboxBrowserImpl.DoGetEnabled: Boolean;
begin
  Result := not TBrowserElementComponent(FBrowser).Check.disabled;
end;

procedure TNyxElementCheckboxBrowserImpl.DoSetEnabled(const AValue: Boolean);
begin
  TBrowserElementComponent(FBrowser).Check.disabled := not AValue;
end;

function TNyxElementCheckboxBrowserImpl.DoGetVisible: Boolean;
begin
  Result := FBrowser.Visible;
end;

procedure TNyxElementCheckboxBrowserImpl.DoSetVisible(const AValue: Boolean);
begin
  FBrowser.Visible := AValue;
end;

procedure TNyxElementCheckboxBrowserImpl.DoUpdateHeight;
begin
  FBrowser.Size.UpdateHeight(Size.Height);
end;

procedure TNyxElementCheckboxBrowserImpl.DoUpdateWidth;
begin
  FBrowser.Size.UpdateWidth(Size.Width);
end;

procedure TNyxElementCheckboxBrowserImpl.DoUpdateMode;
begin
  FBrowser.Size.UpdateMode(Size.Mode);
end;

function TNyxElementCheckboxBrowserImpl.DoGetContainer: INyxContainer;
begin
  Result := FBrowser.Container;
end;

procedure TNyxElementCheckboxBrowserImpl.DoSetContainer(
  const AValue: INyxContainer);
begin
  FBrowser.Container := AValue;
end;

function TNyxElementCheckboxBrowserImpl.TBrowserElementComponent.DoCreateElement: TJSElement;
begin
  Result := document.createElement('span');
  Check := TJSHTMLInputElement(document.createElement('input'));
  Check.id := DoGenerateID;
  Check.setAttribute('type', 'checkbox');
  Lbl := TJSHTMLLabelElement(document.createElement('label'));
  Lbl.setAttribute('for', Check.id);

  //have the label take up the entire portion of the span
  Lbl.setAttribute('style', 'height:100%;width:100%;');
end;

function TNyxElementCheckboxBrowserImpl.TBrowserElementComponent.DoGetSelf: INyxElement;
begin
  Result := Parent as INyxElement;
end;

constructor TNyxElementCheckboxBrowserImpl.Create;
begin
  inherited Create;
  FBrowser := TBrowserElementComponent.Create;
  FBrowser.JSElement.addEventListener(sEventClick, @ClickHandler);
  FBrowser.JSElement.addEventListener(sEventMouseDown, @MouseDownHandler);
  TBrowserElementComponent(FBrowser).Parent := Self;
  BindEvents(FBrowser);
end;

destructor TNyxElementCheckboxBrowserImpl.Destroy;
begin
  FBrowser.Free;
  inherited Destroy;
end;

end.

