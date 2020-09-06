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
unit nyx.element.input.browser;

{$mode delphi}

interface

uses
  web,
  nyx.types,
  nyx.element.browser,
  nyx.element.input;

type

  { TNyxElementInputBrowserImpl }
  (*
    base implementation for all browser nyx Inputs
  *)
  TNyxElementInputBrowserImpl = class(TNyxElementInputBaseImpl, INyxElementBrowser, INyxElementInput)
  strict private
    FBrowser : TNyxElementBrowserImpl;
  protected
    function DoGetTextPrompt: String; override;
    procedure DoSetTextPrompt(const AValue: String); override;
    function GetBrowser: TNyxElementBrowserImpl;

    type

      (*
        component which handles the INyxElement contract
      *)

      { TBrowserElementComponent }

      TBrowserElementComponent = class(TNyxElementBrowserImpl)
      strict protected
        function DoCreateElement: TJSElement; override;
        function DoGetSelf: INyxElement; override;
      public
        Parent : TNyxElementInputBrowserImpl;
      end;

  strict protected
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
  nyx.utils.browser.css;

{ TNyxElementInputBrowserImpl }

function TNyxElementInputBrowserImpl.DoGetTextPrompt: String;
begin
  Result := TJSHTMLInputElement(FBrowser.JSElement).placeholder;
end;

procedure TNyxElementInputBrowserImpl.DoSetTextPrompt(const AValue: String);
begin
  TJSHTMLInputElement(FBrowser.JSElement).placeholder := AValue;
end;

function TNyxElementInputBrowserImpl.GetBrowser: TNyxElementBrowserImpl;
begin
  Result := FBrowser;
end;

procedure TNyxElementInputBrowserImpl.DoSetText(const AValue: String);
begin
  TJSHTMLInputElement(FBrowser.JSElement).value := AValue;
end;

function TNyxElementInputBrowserImpl.DoGetText: String;
begin
  Result := TJSHTMLInputElement(FBrowser.JSElement).value;
end;

function TNyxElementInputBrowserImpl.DoGetEnabled: Boolean;
begin
  Result := not TJSHTMLInputElement(FBrowser.JSElement).disabled;
end;

procedure TNyxElementInputBrowserImpl.DoSetEnabled(const AValue: Boolean);
begin
  TJSHTMLInputElement(FBrowser.JSElement).disabled := not AValue;
end;

function TNyxElementInputBrowserImpl.DoGetVisible: Boolean;
begin
  Result := FBrowser.Visible;
end;

procedure TNyxElementInputBrowserImpl.DoSetVisible(const AValue: Boolean);
begin
  FBrowser.Visible := AValue;
end;

procedure TNyxElementInputBrowserImpl.DoUpdateHeight;
begin
  FBrowser.Size.UpdateHeight(Size.Height);
end;

procedure TNyxElementInputBrowserImpl.DoUpdateWidth;
begin
  FBrowser.Size.UpdateWidth(Size.Width);
end;

procedure TNyxElementInputBrowserImpl.DoUpdateMode;
begin
  FBrowser.Size.UpdateMode(Size.Mode);
end;

function TNyxElementInputBrowserImpl.DoGetContainer: INyxContainer;
begin
  Result := FBrowser.Container;
end;

procedure TNyxElementInputBrowserImpl.DoSetContainer(
  const AValue: INyxContainer);
begin
  FBrowser.Container := AValue;
end;

function TNyxElementInputBrowserImpl.TBrowserElementComponent.DoCreateElement: TJSElement;
begin
  Result := document.createElement('input');
end;

function TNyxElementInputBrowserImpl.TBrowserElementComponent.DoGetSelf: INyxElement;
begin
  Result := Parent as INyxElement;
end;

constructor TNyxElementInputBrowserImpl.Create;
begin
  inherited Create;
  FBrowser := TBrowserElementComponent.Create;
  TBrowserElementComponent(FBrowser).Parent := Self;
end;

destructor TNyxElementInputBrowserImpl.Destroy;
begin
  FBrowser.Free;
  inherited Destroy;
end;

end.

