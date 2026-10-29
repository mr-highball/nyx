{ nyx
  Copyright (c) 2020 mr-highball

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
}
unit nyx.scroll.configuration;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.controls, nyx.scroll.presentation;

type
  { Additive managed authoring facet retains the exact selected platform,
    viewport or named presentation scope. It owns no native/DOM object and does
    not change existing specialized-control or Configure interface layouts. }
  INyxScrollConfiguration = interface(IInterface)
    ['{BB30E1B5-7B91-47EF-9A4E-DC91D625386C}']
    function Presentation(const AValue: TNyxScrollPresentation): INyxScrollConfiguration;
    function UseDefault: INyxScrollConfiguration;
    function Done: INyxConfiguration;
  end;

function NyxScrollConfigure(const AConfiguration: INyxConfiguration): INyxScrollConfiguration;

implementation

uses SysUtils, nyx.types;

type
  { One-way ownership of the caller's managed configuration avoids cycles and
    permits alternate implementations through its existing Metadata boundary. }
  TScrollConfiguration = class(TInterfacedObject, INyxScrollConfiguration)
  private
    FConfiguration: INyxConfiguration;
  public
    constructor Create(const AConfiguration: INyxConfiguration);
    function Presentation(const AValue: TNyxScrollPresentation): INyxScrollConfiguration;
    function UseDefault: INyxScrollConfiguration;
    function Done: INyxConfiguration;
  end;

constructor TScrollConfiguration.Create(const AConfiguration: INyxConfiguration);
begin
  inherited Create;

  if AConfiguration = nil then
  begin
    raise EArgumentException.Create('Scroll presentation requires a configuration scope');
  end;
  FConfiguration := AConfiguration;
end;

function TScrollConfiguration.Presentation(
  const AValue: TNyxScrollPresentation): INyxScrollConfiguration;
var
  LLease: INyxScrollConfiguration;
  LConfiguration: INyxConfiguration;
begin
  LLease := Self;
  LConfiguration := FConfiguration;
  LConfiguration.Metadata(atScrollPresentation, AValue.ToData.ToJSON);
  Result := LLease;
end;

function TScrollConfiguration.UseDefault: INyxScrollConfiguration;
var
  LLease: INyxScrollConfiguration;
  LConfiguration: INyxConfiguration;
begin
  LLease := Self;
  LConfiguration := FConfiguration;
  LConfiguration.Metadata(atScrollPresentation, '');
  Result := LLease;
end;

function TScrollConfiguration.Done: INyxConfiguration;
begin
  Result := FConfiguration;
end;

function NyxScrollConfigure(const AConfiguration: INyxConfiguration): INyxScrollConfiguration;
begin
  Result := TScrollConfiguration.Create(AConfiguration);
end;

end.
