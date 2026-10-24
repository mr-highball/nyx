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
  { A renderer supplies a weak entry receiver. Its returned local lease keeps
    the drawing and borrowed host data alive across a provider callback; the
    drawing stores only this method, avoiding a managed owner cycle. }
  TNyxBrowserDrawingEnter = function: IInterface of object;

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
    FEnter: TNyxBrowserDrawingEnter;
    function BuildSVG(const APlan: INyxDrawingPlan; ATheme: TNyxTheme): TJSElement;
    procedure BoundsChanged(AEntries: TJSHTMLResizeObserverEntryArray;
      AObserver: TJSHTMLResizeObserver);
  public
    constructor Create(AHost: TJSHTMLElement;
      AEnter: TNyxBrowserDrawingEnter = nil);
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

constructor TNyxBrowserDrawing.Create(AHost: TJSHTMLElement;
  AEnter: TNyxBrowserDrawingEnter);
begin
  inherited Create;

  if AHost = nil then
  begin
    raise ENyxDrawing.Create('Drawing host required');
  end;
  FHost := AHost;
  FEnter := AEnter;
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
  FEnter := nil;

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
  LCommand: TNyxDrawingCommand;
  LCommands: INyxDrawingCommands;
  LShape: TJSElement;
  LPoints: TNyxDrawingPoints;
  LStyle: TNyxDrawingStyle;
  LWire: TNyxText;
  LBox: TNyxCanvasBounds;
begin
  Result := document.createElementNS(SVGNamespace, 'svg');
  LCommands := NyxDrawingCommands(APlan);
  LBox := APlan.ViewBox;
  Result.setAttribute('viewBox', Number(LBox.Left) + ' ' + Number(LBox.Top) + ' ' +
    Number(LBox.Width) + ' ' + Number(LBox.Height));
  Result.setAttribute('preserveAspectRatio', 'xMidYMid meet');
  Result.setAttribute('width', '100%');
  Result.setAttribute('height', '100%');
  Result.setAttribute('aria-hidden', 'true');
  Result.setAttribute('focusable', 'false');
  Result.setAttribute('style', 'display:block;overflow:hidden;pointer-events:none');
  for LIndex := 0 to LCommands.Count - 1 do
  begin
    LCommand := LCommands.TypedCommand(LIndex);

    if LCommand.Kind = ndckPath then
    begin
      LPoints := NyxDrawingPathPoints(LCommand.Path);
      LStyle := LCommand.Style;
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
      LShape.textContent := LCommand.Text;
      LShape.setAttribute('x', Number(LCommand.Position.X));
      LShape.setAttribute('y', Number(LCommand.Position.Y));
      LShape.setAttribute('dominant-baseline', 'text-before-edge');
      LShape.setAttribute('font-family', 'Segoe UI, sans-serif');
      LShape.setAttribute('font-size', Number(LCommand.Size));
      LShape.setAttribute('fill', PaintValue(LCommand.Paint, ATheme));
      case LCommand.Anchor of
        ndtaStart: LShape.setAttribute('text-anchor', 'start');
        ndtaCenter: LShape.setAttribute('text-anchor', 'middle');
        ndtaEnd: LShape.setAttribute('text-anchor', 'end');
      end;

      if LCommand.Bold then
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
  LHost: TJSHTMLElement;
  LProvider: INyxDrawingProvider;
  LCaption: TNyxText;
begin

  if FHost = nil then
  begin
    raise ENyxDrawing.Create('Drawing view is disconnected');
  end;

  if APlan = nil then
  begin
    raise ENyxDrawing.Create('Drawing plan required');
  end;
  LHost := FHost;
  LProvider := AProvider;
  LCaption := ACaption;
  { Copy the actual borrowed palette before any foreign plan/provider getter.
    A callback may retire the drawing, including its previous owned palette. }
  LTheme := NewNyxDocumentTheme(nil, ATheme);
  try
    LBase := CaptureNyxDrawingPlan(APlan);

    if FHost <> LHost then
    begin
      Exit;
    end;
    LPlan := ResolveNyxDrawing(LBase, LProvider, LHost.clientWidth, LHost.clientHeight, LTheme);

    if FHost <> LHost then
    begin
      Exit;
    end;
    LSVG := BuildSVG(LPlan, LTheme);

    if FHost <> LHost then
    begin
      Exit;
    end;

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
    FProvider := LProvider;
    FLastError := '';
    FCaption := LCaption;
    FHost.setAttribute('role', 'img');
    FHost.setAttribute('aria-label', LCaption);
  finally
    LTheme.Free;
  end;
end;

procedure TNyxBrowserDrawing.BoundsChanged(AEntries: TJSHTMLResizeObserverEntryArray;
  AObserver: TJSHTMLResizeObserver);
var
  LLease: IInterface;
  LEnter: TNyxBrowserDrawingEnter;
begin
  LEnter := FEnter;

  if Assigned(LEnter) then
  begin
    LLease := LEnter();

    if LLease = nil then
    begin
      Exit;
    end;
  end;

  if (FHost = nil) or (FBase = nil) then
  begin
    Exit;
  end;
  try
    Configure(FBase, FProvider, FTheme, FCaption);
  except
    on E: Exception do
    begin
      if FHost <> nil then
      begin
        FLastError := E.Message;
      end;
    end;
  end;
end;

end.
