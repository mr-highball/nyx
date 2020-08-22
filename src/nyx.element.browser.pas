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

    procedure InitializeCSS;
    procedure UpdateSize;
  protected
    function GetJSElement: TJSElement;
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

    constructor Create; override;
  end;

implementation
uses
  nyx.utils.browser.css;

{ TNyxElementBrowserImpl }

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

    //force setting a display if we don't have one to try and avoid size checks failing
    if not LCSS.Exists('display') then
      LCSS['display'] := 'initial';

    FElement.setAttribute('style', LCSS.CSS);
  finally
    LCSS.Free;
  end;
end;

procedure TNyxElementBrowserImpl.UpdateSize;
var
  LCSS : TNyxCSSHelper;
  LSelf: INyxElementBrowser;
begin
  LSelf := Self as INyxElementBrowser;
  LCSS := TNyxCSSHelper.Create;
  try
    //copy the current style
    LCSS.CopyFromElement(LSelf);

    //depending on mode, update css differently
    if Size.Mode = smFixed then
    begin
      LCSS['height'] := IntToStr(Round(Size.Height)) + 'px';
      LCSS['width'] := IntToStr(Round(Size.Width)) + 'px';
    end
    else
    begin
      LCSS['height'] := IntToStr(Round(Size.Height)) + '%';
      LCSS['width'] := IntToStr(Round(Size.Width)) + '%';
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

procedure TNyxElementBrowserImpl.DoUpdateHeight;
begin
  inherited DoUpdateHeight;
  UpdateSize;
end;

procedure TNyxElementBrowserImpl.DoUpdateWidth;
begin
  inherited DoUpdateWidth;
  UpdateSize;
end;

procedure TNyxElementBrowserImpl.DoUpdateMode;
begin
  inherited DoUpdateMode;
  UpdateSize;
end;

constructor TNyxElementBrowserImpl.Create;
begin
  inherited Create;
  FElement :=  DoCreateElement;
  FElement.id := ID;
  InitializeCSS;
end;

end.

