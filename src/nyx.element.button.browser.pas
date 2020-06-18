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
unit nyx.element.button.browser;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  web,
  nyx.types,
  nyx.element.browser,
  nyx.element.button;

type

  { TNyxElementButtonBrowserImpl }
  (*
    base implementation for all browser nyx buttons
  *)
  TNyxElementButtonBrowserImpl = class(TNyxElementButtonBaseImpl, INyxElementBrowser, INyxElementButton)
  strict private
    FBrowser : TNyxElementBrowserImpl;

    (*
      notifies observers for click events
    *)
    function ClickHandler(Event: TEventListenerEvent): Boolean;
  protected
    function GetBrowser: TNyxElementBrowserImpl;

    type

      (*
        component which handles the INyxElement contract
      *)
      TBrowserElementComponent = class(TNyxElementBrowserImpl)
      strict protected
        function DoCreateElement: TJSElement; override;
      end;

  strict protected
    procedure DoSetText(const AValue: String); override;
    function DoGetText: String; override;

    function DoGetEnabled: Boolean; override;
    procedure DoSetEnabled(const AValue: Boolean); override;
  public
    property BrowserElementImpl : TNyxElementBrowserImpl read GetBrowser implements INyxElementBrowser;

    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ TNyxElementButtonBrowserImpl }

function TNyxElementButtonBrowserImpl.ClickHandler(Event: TEventListenerEvent): Boolean;
begin
  Notify(boClick);
end;

function TNyxElementButtonBrowserImpl.GetBrowser: TNyxElementBrowserImpl;
begin
  Result := FBrowser;
end;

procedure TNyxElementButtonBrowserImpl.DoSetText(const AValue: String);
begin
  FBrowser.JSElement.textContent := AValue;
end;

function TNyxElementButtonBrowserImpl.DoGetText: String;
begin
  Result := FBrowser.JSElement.textContent;
end;

function TNyxElementButtonBrowserImpl.DoGetEnabled: Boolean;
begin
  Result := not TJSHTMLButtonElement(FBrowser.JSElement).disabled;
end;

procedure TNyxElementButtonBrowserImpl.DoSetEnabled(const AValue: Boolean);
begin
  TJSHTMLButtonElement(FBrowser.JSElement).disabled := not AValue;
end;

function TNyxElementButtonBrowserImpl.TBrowserElementComponent.DoCreateElement: TJSElement;
begin
  Result := document.createElement('button');
end;

constructor TNyxElementButtonBrowserImpl.Create;
begin
  inherited Create;
  FBrowser := TBrowserElementComponent.Create;
  FBrowser.JSElement.addEventListener('click', @ClickHandler);
end;

destructor TNyxElementButtonBrowserImpl.Destroy;
begin
  FBrowser.Free;
  inherited Destroy;
end;

end.

