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
  Classes,
  SysUtils,
  web,
  nyx.types;

type

  { INyxElementBrowser }
  (*
    base browser element
  *)
  INyxElementBrowser = interface(INyxElement)
    ['{CC8886D4-5AA1-412B-8099-86F93C6870AA}']

    //property methods
    function GetJSElement: TJSElement;

    //properties
    property JSElement : TJSElement read GetJSElement;
  end;


  { TNyxElementBrowserImpl }
  (*
    base implementation for all browser elements
  *)
  TNyxElementBrowserImpl = class(TNyxElementBaseImpl, INyxElementBrowser)
  strict private
    FElement : TJSElement;
  protected
    function GetJSElement: TJSElement;
  strict protected

    (*
      children need to override this method to have document create
      the root JS element associated with this browser element
    *)
    function DoCreateElement : TJSElement; virtual; abstract;
  public
    property JSElement : TJSElement read GetJSElement;

    constructor Create; override;
  end;

implementation

{ TNyxElementBrowserImpl }

function TNyxElementBrowserImpl.GetJSElement: TJSElement;
begin
  Result := FElement;
end;

constructor TNyxElementBrowserImpl.Create;
begin
  inherited Create;
  FElement :=  DoCreateElement;
  FElement.id := ID;
end;

end.

