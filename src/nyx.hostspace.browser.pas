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
unit nyx.hostspace.browser;

{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch externalclass}

interface

uses nyx.hostspace;

{ Observe this browsing context's window, including an iframe's own viewport.
  The managed observer borrows Window/VisualViewport, never changes global DOM
  styles and detaches both resize listeners on Disconnect/destruction. Older
  engines without VisualViewport retain layout sizing. Receiver is borrowed. }
function NewNyxBrowserHostSpace(
  const AOptions: TNyxHostSizingOptions): INyxHostSpace;
{ Immediate copied metrics for consumers that do not need a subscription. }
function ReadNyxBrowserHostSpace: TNyxHostSpaceSnapshot;

implementation

uses JS, Web;

type
  { The matched Web binding predates VisualViewport. This bridge uses only the
    CSSOM View interface, in CSS pixels, without handwritten JavaScript.
    https://www.w3.org/TR/cssom-view-1/#the-visualviewport-interface }
  TNyxVisualViewport = class external name 'VisualViewport'(TJSEventTarget)
    height: Double;
    scale: Double;
  end;
  TNyxViewportWindow = class external name 'Window'(TJSWindow)
    visualViewport: TNyxVisualViewport;
  end;

  TNyxBrowserHostSpace = class(TNyxHostSpaceObserver)
  private
    FVisual: TNyxVisualViewport;
    FResize: TJSEventHandler;
    function Resized(AEvent: TJSEvent): Boolean;
  protected
    function Capture: TNyxHostSpaceSnapshot; override;
  public
    constructor Create(const AOptions: TNyxHostSizingOptions);
    procedure Disconnect; override;
  end;

function VisualViewport: TNyxVisualViewport;
begin
  Result := nil;

  if isObject(TJSObject(window)['visualViewport']) then
  begin
    Result := TNyxViewportWindow(window).visualViewport;
  end;
end;

function ReadNyxBrowserHostSpace: TNyxHostSpaceSnapshot;
var
  LVisual: TNyxVisualViewport;
  LHeight: Double;
  LScale: Double;
begin
  LHeight := window.innerHeight;
  LScale := 1;
  LVisual := VisualViewport;

  if (LVisual <> nil) and (LVisual.scale > 0) then
  begin
    LHeight := LVisual.height;
    LScale := LVisual.scale;
  end;
  { Inactive documents expose a zero visual scale. Keep the layout observation
    until activation supplies usable visual metrics; do not infer a keyboard. }
  Result := NyxHostSpace(window.innerWidth, window.innerHeight, LHeight, LScale);
end;

constructor TNyxBrowserHostSpace.Create(const AOptions: TNyxHostSizingOptions);
begin
  inherited Create;
  Initialize(AOptions, ReadNyxBrowserHostSpace);
  FVisual := VisualViewport;
  FResize := @Resized;
  window.addEventListener('resize', FResize);

  if FVisual <> nil then
  begin
    FVisual.addEventListener('resize', FResize);
  end;
end;

function TNyxBrowserHostSpace.Capture: TNyxHostSpaceSnapshot;
begin
  Result := ReadNyxBrowserHostSpace;
end;

function TNyxBrowserHostSpace.Resized(AEvent: TJSEvent): Boolean;
begin
  Refresh;
  Result := True;
end;

procedure TNyxBrowserHostSpace.Disconnect;
begin

  if GetConnected then
  begin
    window.removeEventListener('resize', FResize);

    if FVisual <> nil then
    begin
      FVisual.removeEventListener('resize', FResize);
    end;
    FVisual := nil;
    FResize := nil;
  end;
  inherited Disconnect;
end;

function NewNyxBrowserHostSpace(
  const AOptions: TNyxHostSizingOptions): INyxHostSpace;
begin
  Result := TNyxBrowserHostSpace.Create(AOptions);
end;

end.
