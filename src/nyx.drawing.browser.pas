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
unit nyx.drawing.browser;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, JS, Web, nyx.text, nyx.data, nyx.colors, nyx.theme,
  nyx.design.tokens, nyx.drawing, nyx.drawing.view, nyx.canvas.geometry;

type
  { Binding-owned SVG projection. Normal Nyx DOM ownership/events remain on
    the supplied host. Configure builds a detached complete subtree, then swaps
    it; no raw markup/code enters SVG. Disconnect retires the observer before
    the borrowed host ends. The theme is copied and providers are managed. }
  TNyxBrowserDrawing = class
  private
    FHost: TJSHTMLElement;
    FSVG: TJSElement;
    FObserver: TJSHTMLResizeObserver;
    FBase: INyxDrawingPlan;
    FPlan: INyxDrawingPlan;
    FProvider: INyxDrawingProvider;
    FTheme: TNyxTheme;
    FLastError: TNyxText;
    FCaption: TNyxText;
    function BuildSVG(const APlan: INyxDrawingPlan; ATheme: TNyxTheme): TJSElement;
    procedure BoundsChanged(AEntries: TJSHTMLResizeObserverEntryArray;
      AObserver: TJSHTMLResizeObserver);
  public
    constructor Create(AHost: TJSHTMLElement);
    destructor Destroy; override;
    procedure Configure(const APlan: INyxDrawingPlan;
      const AProvider: INyxDrawingProvider; ATheme: TNyxTheme;
      const ACaption: TNyxText);
    procedure Disconnect;
    property Plan: INyxDrawingPlan read FPlan;
    property LastError: TNyxText read FLastError;
  end;

implementation

const
  SVGNamespace = 'http://www.w3.org/2000/svg';

function Number(AValue: Double): TNyxText;
begin
  Result := NyxData(AValue).ToJSON;
end;

function PaintValue(const APaint: TNyxDrawingPaint; ATheme: TNyxTheme): TNyxText;
begin
  Result := 'none';

  if APaint.Kind <> ndpkNone then
  begin
    Result := ResolveNyxDrawingPaint(APaint, ATheme).ToText;
  end;
end;

constructor TNyxBrowserDrawing.Create(AHost: TJSHTMLElement);
begin
  inherited Create;

  if AHost = nil then
  begin
    raise ENyxDrawing.Create('Drawing host required');
  end;
  FHost := AHost;
  FObserver := TJSHTMLResizeObserver.new(@BoundsChanged);
  FObserver.observe(FHost);
end;

destructor TNyxBrowserDrawing.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

procedure TNyxBrowserDrawing.Disconnect;
begin

  if FObserver <> nil then
  begin
    FObserver.disconnect;
    FObserver := nil;
  end;
  FHost := nil;
  FSVG := nil;
  FProvider := nil;
  FBase := nil;
  FPlan := nil;
  FTheme.Free;
  FTheme := nil;
end;

function TNyxBrowserDrawing.BuildSVG(const APlan: INyxDrawingPlan;
  ATheme: TNyxTheme): TJSElement;
var
  LIndex: Integer;
  LPointIndex: Integer;
  LCommand: TNyxDataValue;
  LShape: TJSElement;
  LPoints: TNyxDrawingPoints;
  LStyle: TNyxDrawingStyle;
  LWire: TNyxText;
  LBox: TNyxCanvasBounds;
begin
  Result := document.createElementNS(SVGNamespace, 'svg');
  LBox := APlan.ViewBox;
  Result.setAttribute('viewBox', Number(LBox.Left) + ' ' + Number(LBox.Top) + ' ' +
    Number(LBox.Width) + ' ' + Number(LBox.Height));
  Result.setAttribute('preserveAspectRatio', 'xMidYMid meet');
  Result.setAttribute('width', '100%');
  Result.setAttribute('height', '100%');
  Result.setAttribute('aria-hidden', 'true');
  Result.setAttribute('focusable', 'false');
  Result.setAttribute('style', 'display:block;overflow:hidden;pointer-events:none');
  for LIndex := 0 to APlan.Count - 1 do
  begin
    LCommand := APlan.Command(LIndex);

    if LCommand.Field('kind').AsInteger = 0 then
    begin
      LPoints := NyxDrawingPathPoints(TNyxDrawingPath.FromData(LCommand.Field('path')));
      LStyle := TNyxDrawingStyle.FromData(LCommand.Field('style'));
      LWire := '';
      for LPointIndex := 0 to High(LPoints) do
      begin
        LWire := LWire + Number(LPoints[LPointIndex].X) + ',' +
          Number(LPoints[LPointIndex].Y) + ' ';
      end;
      { Separate fill and stroke match native polygon-fill/polyline-stroke:
        an open path's fill closes implicitly, while its stroke stays open. }
      LShape := document.createElementNS(SVGNamespace, 'polygon');
      LShape.setAttribute('points', LWire);
      LShape.setAttribute('fill', PaintValue(LStyle.FillPaint, ATheme));
      LShape.setAttribute('stroke', 'none');
      Result.appendChild(LShape);
      LShape := document.createElementNS(SVGNamespace, 'polyline');
      LShape.setAttribute('points', LWire);
      LShape.setAttribute('fill', 'none');
      LShape.setAttribute('stroke', PaintValue(LStyle.StrokePaint, ATheme));
      LShape.setAttribute('stroke-width', Number(LStyle.StrokeWidth));
      LShape.setAttribute('stroke-linejoin', 'miter');
      Result.appendChild(LShape);
    end
    else
    begin
      LShape := document.createElementNS(SVGNamespace, 'text');
      LShape.textContent := LCommand.Field('text').AsText;
      LShape.setAttribute('x', Number(LCommand.Field('position').Item(0).AsNumber));
      LShape.setAttribute('y', Number(LCommand.Field('position').Item(1).AsNumber));
      LShape.setAttribute('dominant-baseline', 'text-before-edge');
      LShape.setAttribute('font-family', 'Segoe UI, sans-serif');
      LShape.setAttribute('font-size', Number(LCommand.Field('size').AsNumber));
      LShape.setAttribute('fill', PaintValue(TNyxDrawingPaint.FromData(LCommand.Field('paint')), ATheme));
      case TNyxDrawingTextAnchor(LCommand.Field('anchor').AsInteger) of
        ndtaStart: LShape.setAttribute('text-anchor', 'start');
        ndtaCenter: LShape.setAttribute('text-anchor', 'middle');
        ndtaEnd: LShape.setAttribute('text-anchor', 'end');
      end;

      if LCommand.Field('bold').AsBoolean then
      begin
        LShape.setAttribute('font-weight', 'bold');
      end;
      Result.appendChild(LShape);
    end;
  end;
end;

procedure TNyxBrowserDrawing.Configure(const APlan: INyxDrawingPlan;
  const AProvider: INyxDrawingProvider; ATheme: TNyxTheme;
  const ACaption: TNyxText);
var
  LTheme: TNyxTheme;
  LBase: INyxDrawingPlan;
  LPlan: INyxDrawingPlan;
  LSVG: TJSElement;
begin

  if FHost = nil then
  begin
    raise ENyxDrawing.Create('Drawing view is disconnected');
  end;

  if APlan = nil then
  begin
    raise ENyxDrawing.Create('Drawing plan required');
  end;
  LBase := NyxDrawingPlanFromData(APlan.ToData);
  LTheme := NewNyxDocumentTheme(nil, ATheme);
  try
    LPlan := ResolveNyxDrawing(LBase, AProvider, FHost.clientWidth, FHost.clientHeight, LTheme);
    LSVG := BuildSVG(LPlan, LTheme);

    if FSVG = nil then
    begin
      FHost.appendChild(LSVG);
    end
    else
    begin
      FHost.replaceChild(LSVG, FSVG);
    end;
    FSVG := LSVG;
    FTheme.Free;
    FTheme := LTheme;
    LTheme := nil;
    FBase := LBase;
    FPlan := LPlan;
    FProvider := AProvider;
    FLastError := '';
    FCaption := ACaption;
    FHost.setAttribute('role', 'img');
    FHost.setAttribute('aria-label', ACaption);
  finally
    LTheme.Free;
  end;
end;

procedure TNyxBrowserDrawing.BoundsChanged(AEntries: TJSHTMLResizeObserverEntryArray;
  AObserver: TJSHTMLResizeObserver);
begin

  if (FHost = nil) or (FBase = nil) then
  begin
    Exit;
  end;
  try
    Configure(FBase, FProvider, FTheme, FCaption);
  except
    on E: Exception do
    begin
      FLastError := E.Message;
    end;
  end;
end;

end.
