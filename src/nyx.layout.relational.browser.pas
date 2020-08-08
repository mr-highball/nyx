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
unit nyx.layout.relational.browser;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  nyx.types,
  nyx.layout,
  nyx.utils.browser.css;

type

  { TNyxLayoutRelationalBrowserImpl }
  (*
    browser implementation for a Relational layout
  *)
  TNyxLayoutRelationalBrowserImpl = class(TNyxLayoutRelationalImpl)
  strict private
    FCSS : TNyxCSSHelper;
  strict protected
    function DoPlaceElement(const AElement: INyxElement;
      const ABounds: INyxRelationalBounds; out Error: String): Boolean; override;
  protected
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation
uses
  web,
  nyx.element.browser;

{ TNyxLayoutRelationalBrowserImpl }

function TNyxLayoutRelationalBrowserImpl.DoPlaceElement(
  const AElement: INyxElement; const ABounds: INyxRelationalBounds; out
  Error: String): Boolean;
var
  LElement, LBrowserAnchor: INyxElementBrowser;
  LRect: TJSDOMRect;
  LLeftOff, LTopOff, LLeft, LRight, LTop, LWidth, LHeight, LBottom: Double;
  LAnchor: INyxElement;
  LHasAnchor: Boolean;
  LLeftUnits: String = 'px';
  LTopUnits: String = 'px';
  LStyle : String;
  LCSS : TNyxCSSHelper;
  LHTMlElement: TJSHTMLElement;

  (*
    helper that handles fetching a floating point positional / size value
    from an attribute
    todo - maybe move this to css helper unit? and name it better...
  *)
  procedure GetStyleValue(const AStyle : String; var Value : Double;
    var Units : String);
  begin
    if Trim(AStyle) = '' then
      Exit;

    //absolute measurement
    if Pos('px', LowerCase(AStyle)) > 0 then
    begin
      Units := 'px';
      Value := StrToFloat(Copy(AStyle, 1, Length(AStyle) - 2));
    end
    //percent measurement
    else if Pos('%', LowerCase(AStyle)) > 0 then
    begin
      Units := '%';
      Value := StrToFloat(Copy(AStyle, 1, Length(AStyle) - 1));
    end
    //relative 'weighted' measurement
    else if Pos('em', LowerCase(AStyle)) > 0 then
    begin
      Units := 'em';
      Value := StrToFloat(Copy(AStyle, 1, Length(AStyle) - 2));
    end;
  end;

begin
  try
    Result := False;
    LHasAnchor := False;

    //first try to fetch the anchor element
    if GetAnchor(AElement, LAnchor) then
    begin
      LBrowserAnchor := LAnchor as INyxElementBrowser;
      LHasAnchor := True;
    end;

    //cast the element
    LElement := AElement as INyxElementBrowser;

    //copy the inline css of the element
    FCSS.CopyFromElement(LElement);

    //we will use the position / attribute pair to force "Relational" positioning
    FCSS.Upsert('position', 'absolute');

    //as long as we have an anchor, we will use it as the point to offset this element
    if LHasAnchor then
    begin
      (*
        below we seemingly can't rely on just calling getBoundingClientRect()
        because while testing it's returning (0, 0, 0, 0). this could be
        due to the dom not being ready, 'display: none' or who knows what spice...
        but the inline should be reliable as long as nyx is used to build the ui,
        so this will be our fallback
      *)
      LRect := LBrowserAnchor.JSElement.getBoundingClientRect;
      LLeft := LRect.left;
      LRight := LRect.right;
      LTop := LRect.top;
      LWidth := LRect.width;
      LHeight := LRect.height;
      LBottom := LRect.bottom;

      //check for an 'empty' rect, if so then use css
      if (LLeft + LRight + LTop + LWidth + LHeight + LBottom) = 0 then
      begin
        LCSS := TNyxCSSHelper.Create;
        try
          LCSS.CopyFromElement(LBrowserAnchor);

          if Assigned(TJSHTMLElement(LBrowserAnchor.JSElement)) then
            LHTMlElement := TJSHTMLElement(LBrowserAnchor.JSElement)
          else
            LHTMlElement := nil;

          //first check for height set in style
          if LCSS.Exists('height') then
          begin
            LStyle := LCSS['height'];
            GetStyleValue(LStyle, LHeight, LTopUnits);
          end
          //otherwise we'll use the offset
          else if Assigned(LHTMLElement)
            and Assigned(LHTMLElement.offsetHeight)
            and (LHTMLElement.offsetHeight <> 0)
          then
            LWidth := LHTMlElement.offsetHeight
          //lastly, if no other checks pass, use the client width (doesn't account of borders)
          else
            LWidth := LBrowserAnchor.JSElement.clientWidth;

          //first check for width set in style
          if LCSS.Exists('width') then
          begin
            LStyle := LCSS['width'];
            GetStyleValue(LStyle, LWidth, LLeftUnits);
          end
          //otherwise we'll use the offset
          else if Assigned(LHTMLElement)
            and Assigned(LHTMLElement.offsetWidth)
            and (LHTMLElement.offsetWidth <> 0)
          then
            LWidth := LHTMlElement.offsetWidth
          //lastly, if no other checks pass, use the client width (doesn't account of borders)
          else
            LWidth := LBrowserAnchor.JSElement.clientWidth;

          //get the left attribute and check to see if we have a value
          if LCSS.Exists('left') then
          begin
            LStyle := LCSS['left'];
            GetStyleValue(LStyle, LLeft, LLeftUnits);
          end;

          if LCSS.Exists('top') then
          begin
            LStyle := LCSS['top'];
            GetStyleValue(LStyle, LTop, LTopUnits);
          end;

          //now for the computed values
          LRight := LLeft + LWidth;
          LBottom := LTop + LHeight;
        finally
          LCSS.Free;
        end;
      end;

      //normal computation based on the left most point of the anchor
      if ABounds.HorzAlignment = haLeft then
        LLeftOff := LLeft
      //calculate the offset using the center of the anchor
      else if ABounds.HorzAlignment = haCenter then
        LLeftOff := LLeft + (LWidth / 2)
      //otherwise we'll be using the right most point of the anchor
      else
        LLeftOff := LRight;

      //normal vertical alignment will use the top of anchor to position
      if ABounds.VertAlignment = vaTop then
        LTopOff := LTop
      //uses the center of the anchor to position vertically
      else if ABounds.VertAlignment = vaCenter then
        LTopOff := LTop + (LHeight / 2)
      //otherwise the bottom of the anchor will be used
      else
        LTopOff := LBottom;
    end;

    //using the offsets calculated above we can set new left / top values
    //in css. also round since no fractional pixels
    FCSS.Upsert('left', IntToStr(Round(ABounds.Left + LLeftOff)) + LLeftUnits);
    FCSS.Upsert('top', IntToStr(Round(ABounds.Top + LTopOff)) + LTopUnits);

    //finally set the inline style with the new computed values
    FCSS.CopyToElement(LElement);

    //success
    Result := True;
  except on E : Exception do
    Error := E.Message;
  end;
end;

constructor TNyxLayoutRelationalBrowserImpl.Create;
begin
  inherited Create;
  FCSS := TNyxCSSHelper.Create;
end;

destructor TNyxLayoutRelationalBrowserImpl.Destroy;
begin
  FCSS.Free;
  inherited Destroy;
end;

end.

