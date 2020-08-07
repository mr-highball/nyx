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
  nyx.element.browser,
  nyx.container.browser;

{ TNyxLayoutRelationalBrowserImpl }

function TNyxLayoutRelationalBrowserImpl.DoPlaceElement(
  const AElement: INyxElement; const ABounds: INyxRelationalBounds; out
  Error: String): Boolean;
var
  LElement, LBrowserAnchor: INyxElementBrowser;
  LRect: TJSDOMRect;
  LLeftOff, LTopOff: Double;
  LStyle: String;
  LAnchor: INyxElement;
  LHasAnchor: Boolean;
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
    LStyle := LElement.JSElement.getAttribute('style');

    if Assigned(LStyle) then
      FCSS.CSS := LStyle;

    //we will use the position / attribute pair to force "Relational" positioning
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
    //todo - had to use input element, saw strange behavior where container was null
    //       as well as ID was different... not sure if this is a pas2js bug, try this out later
    if Assigned(AElement.Container)
      and (AElement.Container is INyxContainerBrowser)
    then
    begin
      LRect := INyxContainerBrowser(AElement.Container).BrowserElement.JSElement.getBoundingClientRect;

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
    FCSS.Upsert('left', IntToStr(Round((ABounds.Left * 100) - LLeftOff)) + '%');
    FCSS.Upsert('top', IntToStr(Round((ABounds.Top * 100) - LTopOff)) + '%');

    //finally set the inline style with the new computed values
    LElement.JSElement.setAttribute('style', FCSS.CSS);

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

