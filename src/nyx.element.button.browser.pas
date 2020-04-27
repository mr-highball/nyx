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
  TNyxElementButtonBrowserImpl = class(TNyxElementBrowserImpl, INyxElementButton)
  strict private
  protected
  strict protected
    function DoCreateElement: TJSElement; override;
  public
  end;

implementation

{ TNyxElementButtonBrowserImpl }

function TNyxElementButtonBrowserImpl.DoCreateElement: TJSElement;
begin
  Result := document.createElement('button');
end;

end.

