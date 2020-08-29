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
unit nyx.container.browser;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  web,
  nyx.types,
  nyx.container,
  nyx.element.browser;

type

  { INyxContainerBrowser }
  (*
    base element container for the browser
  *)
  INyxContainerBrowser = interface(INyxContainer)
    ['{D21F1A4A-3686-4DAE-BB6F-EACBD2E1E307}']

    //property methods
    function GetBrowserElement: INyxElementBrowser;

    //properties
    (*
      access to the browser element for this container
    *)
    property BrowserElement : INyxElementBrowser read GetBrowserElement;
  end;

  { TNyxContainerBrowserImpl }
  (*
    base implementation for all browser containers
  *)
  TNyxContainerBrowserImpl = class(TNyxContainerBaseImpl, INyxContainerBrowser, INyxElementBrowser)
  strict private
    FBrowserElement : TNyxElementBrowserImpl;
    FID : String;

    procedure ObserveCollection(const AElement : INyxElement; const AEvent : TElementsObserveEvent);
  protected
    function GetBrowserElement: INyxElementBrowser;

    type

      { TContainerJSElement }
      (*
        JS element used to contain other elements
      *)
      TContainerJSElement = class(TNyxElementBrowserImpl)
      strict protected
        function DoCreateElement: TJSElement; override;
      end;

  strict protected

    (*
      children can override this if they wish to use a different
      JS element as the container
    *)
    function GetImpl : TContainerJSElement; virtual;
  public
    (*
      delegate component which handles the JS element implementation
    *)
    property BrowserElementImpl : TContainerJSElement read GetImpl implements INyxElementBrowser;

    property BrowserElement : INyxElementBrowser read GetBrowserElement;

    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ TNyxContainerBrowserImpl.TContainerJSElement }

function TNyxContainerBrowserImpl.TContainerJSElement.DoCreateElement: TJSElement;
begin
  //base will use a div as the container
  Result := document.createElement('div');
end;

{ TNyxContainerBrowserImpl }

procedure TNyxContainerBrowserImpl.ObserveCollection(
  const AElement: INyxElement; const AEvent: TElementsObserveEvent);
var
  LJS : INyxElementBrowser;
begin
  //cast to a browser element
  LJS := AElement as INyxElementBrowser;

  //depending on the event type, handle the change to root js element
  case AEvent of
    eoAdd:
      FBrowserElement.JSElement.appendChild(LJS.JSElement);
    eoExtract:
      FBrowserElement.JSElement.removeChild(LJS.JSElement);
  end;
end;

function TNyxContainerBrowserImpl.GetBrowserElement: INyxElementBrowser;
begin
  Result := Self as INyxElementBrowser;
end;

function TNyxContainerBrowserImpl.GetImpl: TContainerJSElement;
begin
  Result := TContainerJSElement(FBrowserElement);
end;

constructor TNyxContainerBrowserImpl.Create;
begin
  inherited Create;
  FBrowserElement := TContainerJSElement.Create;
  FBrowserElement.ID := ID; //set element id to "ours"
  Elements.Observe(@ObserveCollection, FID);
end;

destructor TNyxContainerBrowserImpl.Destroy;
begin
  Elements.RemoveObserver(FID);
  FBrowserElement.Free;
  inherited Destroy;
end;

end.

