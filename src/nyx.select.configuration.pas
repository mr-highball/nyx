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
unit nyx.select.configuration;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.controls, nyx.model, nyx.choices;

type
  { Additive managed select authoring. Keeps the original specialized control
    and Configure interfaces byte-stable. The facade retains the exact control,
    owns no widget, and authors portable default choice data. Alternative select
    implementations participate through their existing managed Node bridge. }
  INyxSelectConfiguration = interface(IInterface)
    ['{430EACD7-286D-4B4F-9CB5-006DE6A81102}']
    { Admits an owned immutable snapshot. Empty choices mask an inherited set;
      nil refuses. Failure preserves the accepted descriptor and value. }
    function Choices(const AItems: INyxChoiceItems): INyxSelectConfiguration;
    { Removes this declaration, restoring inherited choices or legacy Items. }
    function InheritChoices: INyxSelectConfiguration;
    function Done: INyxSelect;
  end;

function NyxSelectConfigure(const ASelect: INyxSelect): INyxSelectConfiguration;
{ Typed attachment also covers inherited/reusable/part controls whose authored
  interface cannot yet be narrowed to INyxSelect. Full document composition
  admits the effective projection. Retains both participants across admission;
  nil control/items refuse without publishing any replacement. }
procedure SetNyxChoiceItems(const AControl: INyxNode; const AItems: INyxChoiceItems);

implementation

uses SysUtils;

type
  { One-way ownership: a fresh facade owns the control, which never caches this
    facade. Retained chains survive caller-variable replacement and release. }
  TSelectConfiguration = class(TInterfacedObject, INyxSelectConfiguration)
  private
    FSelect: INyxSelect;
  public
    constructor Create(const ASelect: INyxSelect);
    function Choices(const AItems: INyxChoiceItems): INyxSelectConfiguration;
    function InheritChoices: INyxSelectConfiguration;
    function Done: INyxSelect;
  end;

constructor TSelectConfiguration.Create(const ASelect: INyxSelect);
begin
  inherited Create;

  if ASelect = nil then
  begin
    raise EArgumentException.Create('Select configuration requires a managed select');
  end;
  FSelect := ASelect;
end;

function TSelectConfiguration.Choices(const AItems: INyxChoiceItems): INyxSelectConfiguration;
var
  LLease: INyxSelectConfiguration;
  LSelect: INyxSelect;
begin
  LLease := Self;
  LSelect := FSelect;
  LSelect.Node.SetChoiceItems(AItems);
  Result := LLease;
end;

function TSelectConfiguration.InheritChoices: INyxSelectConfiguration;
var
  LLease: INyxSelectConfiguration;
  LSelect: INyxSelect;
begin
  LLease := Self;
  LSelect := FSelect;
  LSelect.Node.RemoveChoiceItems;
  Result := LLease;
end;

function TSelectConfiguration.Done: INyxSelect;
begin
  Result := FSelect;
end;

function NyxSelectConfigure(const ASelect: INyxSelect): INyxSelectConfiguration;
begin
  Result := TSelectConfiguration.Create(ASelect);
end;

procedure SetNyxChoiceItems(const AControl: INyxNode; const AItems: INyxChoiceItems);
var
  LControl: INyxNode;
  LItems: INyxChoiceItems;
begin

  if AControl = nil then
  begin
    raise EArgumentException.Create('Choice attachment requires a managed control');
  end;
  LControl := AControl;
  LItems := AItems;
  LControl.Node.SetChoiceItems(LItems);
end;

end.
