unit nyx.settings.browser;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  web,
  nyx.types;

type

  { INyxRenderSettingsBrowser }
  (*
    base settings for the browser
  *)
  INyxRenderSettingsBrowser = interface(INyxRenderSettings)
    ['{150DD59B-7BAC-41D7-98B5-8EDEB7D19110}']

    //property methods
    function GetTarget: TJSElement;
    procedure SetTarget(const AValue: TJSElement);

    //properties
    property Target : TJSElement read GetTarget write SetTarget;
  end;

  { TNyxRenderSettingsBrowserImpl }
  (*
    base implementation for ui settings in the browser
  *)
  TNyxRenderSettingsBrowserImpl = class(TNyxRenderSettingsBaseImpl, INyxRenderSettingsBrowser)
  strict private
    FTarget : TJSElement;
  protected
    function GetTarget: TJSElement;
    procedure SetTarget(const AValue: TJSElement);
  strict protected
  public
    property Target : TJSElement read GetTarget write SetTarget;
  end;

implementation

{ TNyxRenderSettingsBrowserImpl }

function TNyxRenderSettingsBrowserImpl.GetTarget: TJSElement;
begin
  Result := FTarget;
end;

procedure TNyxRenderSettingsBrowserImpl.SetTarget(const AValue: TJSElement);
begin
  FTarget := AValue;
end;

end.

