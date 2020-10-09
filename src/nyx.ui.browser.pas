unit nyx.ui.browser;

{$mode delphi}

interface

uses
  Classes,
  nyx.types;

type

  { INyxUIBrowser }
  (*
    base UI for the browser
  *)
  INyxUIBrowser = interface(INyxUI)
    ['{EC551EFA-92CC-43FB-9F89-9ABA15C82D95}']
  end;

  { TNyxUIBrowserImpl }
  (*
    base implementation for all nyx ui's for the browser
  *)
  TNyxUIBrowserImpl = class(TNyxUIBaseImpl, INyxUIBrowser)
  strict private
  protected
  strict protected
    procedure DoRender; override;
    procedure DoHide; override;
  public
  end;

implementation
uses
  web,
  nyx.settings.browser,
  nyx.container.browser;

{ TNyxUIBrowserImpl }

procedure TNyxUIBrowserImpl.DoRender;
var
  LBrowserSettings: INyxRenderSettingsBrowser;
  I: Integer;
  LTarget: TJSElement;
  LContainer: INyxContainerBrowser;
  LSettings: INyxRenderSettings;
  LContainers: INyxElements;
begin
  LSettings := Settings;

  //if we have settings assigned, use that target
  if Assigned(LSettings) then
  begin
    LBrowserSettings := LSettings as INyxRenderSettingsBrowser;
    LTarget := LBrowserSettings.Target;
  end;

  //otherwise, we'll add to the dom's body directly
  if not Assigned(LTarget) then
    LTarget := document.body;

  LContainers := Containers;

  //iterate containers and add to the target js element
  for I := 0 to Pred(LContainers.Count) do
  begin
    LContainer := LContainers[I] as INyxContainerBrowser;
    LTarget.appendChild(LContainer.BrowserElement.JSElement);
  end;
end;

procedure TNyxUIBrowserImpl.DoHide;
begin
  //todo - hide based on the setting's target (remove child? or just set css https://stackoverflow.com/questions/6242976/javascript-hide-show-element)
end;

end.

