{ Nyx. Copyright (c) 2020 mr-highball. MIT license; see LICENSE. }
unit nyx.button.configuration;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.controls, nyx.button.presentation;

type
  { Additive managed authoring facet. The original Configure and Button interface
    layouts stay stable. This facade retains the caller's exact selected scope,
    including platform and viewport conditions, without retaining a raw widget. }
  INyxButtonConfiguration = interface(IInterface)
    ['{6E40BE07-36ED-4EF8-9C77-034885C90D01}']
    function Presentation(const AValue: TNyxButtonPresentation): INyxButtonConfiguration;
    function Done: INyxConfiguration;
  end;

{ Select scopes through the ordinary Configure contract, then extend that scope:
  NyxButtonConfigure(Button.Configure.ForPlatform(npfBrowser)).Presentation(...).
  The returned facade is fresh and owns its managed configuration until release. }
function NyxButtonConfigure(const AConfiguration: INyxConfiguration): INyxButtonConfiguration;

implementation

uses SysUtils, nyx.types;

type
  { Configuration already retains its control. One-way ownership avoids a cycle
    and also permits alternative implementations of the existing public facade. }
  TButtonConfiguration = class(TInterfacedObject, INyxButtonConfiguration)
  private
    FConfiguration: INyxConfiguration;
  public
    constructor Create(const AConfiguration: INyxConfiguration);
    function Presentation(const AValue: TNyxButtonPresentation): INyxButtonConfiguration;
    function Done: INyxConfiguration;
  end;

constructor TButtonConfiguration.Create(const AConfiguration: INyxConfiguration);
begin
  inherited Create;

  if AConfiguration = nil then
  begin
    raise EArgumentException.Create('Button presentation requires a configuration scope');
  end;
  FConfiguration := AConfiguration;
end;

function TButtonConfiguration.Presentation(const AValue: TNyxButtonPresentation): INyxButtonConfiguration;
var
  LLease: INyxButtonConfiguration;
  LConfiguration: INyxConfiguration;
begin
  { Metadata is an outward call on an alternate facade. Retain both receivers
    before it can release the caller's last facade or configuration reference. }
  LLease := Self;
  LConfiguration := FConfiguration;
  LConfiguration.Metadata(atButtonPresentation, AValue.ToData.ToJSON);
  Result := LLease;
end;

function TButtonConfiguration.Done: INyxConfiguration;
begin
  Result := FConfiguration;
end;

function NyxButtonConfigure(const AConfiguration: INyxConfiguration): INyxButtonConfiguration;
begin
  Result := TButtonConfiguration.Create(AConfiguration);
end;

end.
