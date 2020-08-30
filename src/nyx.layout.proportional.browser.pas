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
unit nyx.layout.proportional.browser;


{$mode delphi}

interface

uses
  SysUtils,
  nyx.types,
  nyx.layout,
  nyx.utils.browser.css;

type

  { TNyxLayoutProportionalBrowserImpl }
  (*
    browser implementation for a proportional layout
  *)
  TNyxLayoutProportionalBrowserImpl = class(TNyxLayoutProportionalImpl)
  strict private
    FCSS : TNyxCSSHelper;
  strict protected
    function DoPlaceElement(const AElement: INyxElement;
      const ABounds: INyxProportionalBounds; out Error: String): Boolean; override;
  protected
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation
uses
  web,
  nyx.element.browser,
  nyx.container.browser;

{ TNyxLayoutProportionalBrowserImpl }

function TNyxLayoutProportionalBrowserImpl.DoPlaceElement(
  const AElement: INyxElement; const ABounds: INyxProportionalBounds; out
  Error: String): Boolean;
var
  LElement: INyxElementBrowser;
  LRect: TJSDOMRect;
  LLeftOff, LTopOff: Double;
  LStyle: String;
  LContainer: INyxContainer;
  LBrowserContainer: INyxContainerBrowser;
begin
  try
    Result := False;

    //cast the element
    LElement := AElement as INyxElementBrowser;

    //copy the inline css of the element
    LStyle := LElement.JSElement.getAttribute('style');

    if Assigned(LStyle) then
      FCSS.CSS := LStyle;

    //we will use the position / attribute pair to force "proportional" positioning
    FCSS.Upsert('position', 'absolute');

    //get the bounding box of the element to calculate alignment
    LRect := LElement.JSElement.getBoundingClientRect;

    //normal computation based on the left most point of the element
    if ABounds.HorzAlignment = haLeft then
      LLeftOff := 0
    //calculate the offset using the center of the element
    else if ABounds.HorzAlignment = haCenter then
      LLeftOff := Round(LRect.width / 2)
    //otherwise we'll be using the right most point of the element
    else
      LLeftOff := Round(LRect.width);

    //normal vertical alignment will use the top of element to position
    if ABounds.VertAlignment = vaTop then
      LTopOff := 0
    //uses the center of the element to position vertically
    else if ABounds.VertAlignment = vaCenter then
      LTopOff := LRect.height / 2
    //otherwise the bottom of the element will be used
    else
      LTopOff := LRect.height;

    //we need to find the parent container's rect in order to calculate a percentage
    LContainer := LElement.Container;
    if Assigned(LContainer)
      and (LContainer is INyxContainerBrowser)
    then
    begin
      //get the bounding rect
      LBrowserContainer := LContainer as INyxContainerBrowser;
      LRect := LBrowserContainer.BrowserElement.JSElement.getBoundingClientRect;

      //find percentage of parent for width
      if LRect.width > 0 then
        LLeftOff := LLeftOff / LRect.width
      else
        LLeftOff := 0;

      //find percentage of parent for height
      if LRect.height > 0 then
        LTopOff := LTopOff / LRect.height
      else
        LTopOff := 0;
    end
    else
    begin
      LLeftOff := 0;
      LTopOff := 0;
    end;

    //using the offsets calculated above we can set new left / top values in css
    FCSS.Upsert('left', IntToStr(Round((ABounds.Left - LLeftOff) * 100)) + '%');
    FCSS.Upsert('top', IntToStr(Round((ABounds.Top - LTopOff) * 100)) + '%');

    //finally set the inline style with the new computed values
    LElement.JSElement.setAttribute('style', FCSS.CSS);

    //success
    Result := True;
  except on E : Exception do
    Error := E.Message;
  end;
end;

constructor TNyxLayoutProportionalBrowserImpl.Create;
begin
  inherited Create;
  FCSS := TNyxCSSHelper.Create;
end;

destructor TNyxLayoutProportionalBrowserImpl.Destroy;
begin
  FCSS.Free;
  inherited Destroy;
end;

end.

