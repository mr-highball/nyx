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

unit nyx.canvas.route.types;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.types, nyx.data, nyx.canvas.types, nyx.flow.routes;

type
  { Detached per-item control route intent, never a renderer or runtime handle.
    Route data is copied through the shared typed codec. Explicit application
    export/adoption may retain this intent; opaque content lifetime tokens never
    enter persistence or gain execution authority. A default value is undefined. }
  TNyxCanvasRouteChange = record
  private
    FData: TNyxDataValue;
    function GetItem: TNyxCanvasItemRef;
    function GetControl: TNyxControlRef;
    function GetRoutes: INyxFlowRoutes;
  public
    function Defined: Boolean;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxCanvasRouteChange; static;
    property Item: TNyxCanvasItemRef read GetItem;
    property Control: TNyxControlRef read GetControl;
    property Routes: INyxFlowRoutes read GetRoutes;
  end;

  { Independent copied route-intent window. Records contain no COM interfaces,
    so caller copies retain identical ownership on native and pas2js targets. }
  TNyxCanvasRouteChanges = array of TNyxCanvasRouteChange;

{ Construct a detached whole route set for one exact item/control. Empty routes
  are an explicit mask, independently from absence/inheritance. No document,
  scene token, scheduler, renderer or execution authority is retained. }
function NyxCanvasRoutes(const AItem: TNyxCanvasItemRef; const AControl: TNyxControlRef;
  const ARoutes: INyxFlowRoutes): TNyxCanvasRouteChange;

implementation

uses nyx.flow.types;

function NyxCanvasRoutes(const AItem: TNyxCanvasItemRef; const AControl: TNyxControlRef;
  const ARoutes: INyxFlowRoutes): TNyxCanvasRouteChange;
var
  LRoutes: INyxFlowRoutes;
  LItem: TNyxCanvasItemRef;
  LControl: TNyxControlRef;
begin
  LRoutes := ARoutes;

  if (AItem.Name = '') or (AControl.ID = '') or (LRoutes = nil) then
  begin
    raise ENyxCanvasScene.Create('Canvas routes require an item, authored control and complete typed set');
  end;
  { Creator serialization is an external callout. Capture the caller's exact
    references before it can retire or change any aliases passed by const. }
  LItem := NyxCanvasItem(AItem.Name);
  LControl := NyxControl(AControl.ID);
  Result := TNyxCanvasRouteChange.FromData(NyxObject([
    NyxField('item', NyxData(LItem.Name)), NyxField('control', NyxData(LControl.ID)),
    NyxField('routes', LRoutes.ToData)]));
end;

function TNyxCanvasRouteChange.Defined: Boolean;
begin
  Result := FData.Defined and (FData.Kind <> ndNull);
end;

function TNyxCanvasRouteChange.ToData: TNyxDataValue;
begin

  if not Defined then
  begin
    raise ENyxCanvasScene.Create('Construct a typed canvas route change before use');
  end;
  Result := FData.Copy;
end;

class function TNyxCanvasRouteChange.FromData(const AData: TNyxDataValue): TNyxCanvasRouteChange;
begin
  ValidateNyxFlowFields(AData, ['item', 'control', 'routes']);
  NyxCanvasItem(AData.Field('item').AsText);

  if AData.Field('control').AsText = '' then
  begin
    raise ENyxCanvasScene.Create('Canvas route changes require an exact authored control');
  end;
  NyxControl(AData.Field('control').AsText);
  NyxFlowRoutesFromData(AData.Field('routes'));
  Result.FData := AData.Copy;
end;

function TNyxCanvasRouteChange.GetItem: TNyxCanvasItemRef;
begin
  Result := NyxCanvasItem(ToData.Field('item').AsText);
end;

function TNyxCanvasRouteChange.GetControl: TNyxControlRef;
begin
  Result := NyxControl(ToData.Field('control').AsText);
end;

function TNyxCanvasRouteChange.GetRoutes: INyxFlowRoutes;
begin
  Result := NyxFlowRoutesFromData(ToData.Field('routes'));
end;

end.
