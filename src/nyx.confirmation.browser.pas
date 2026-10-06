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

unit nyx.confirmation.browser;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Web, nyx.text, nyx.model, nyx.controls, nyx.theme, nyx.behavior, nyx.events,
  nyx.confirmation, nyx.modal.browser, nyx.render.browser;

type
  { Renderer is borrowed; Host returns a retained managed target seam.
    Retained hosts stay closed and disconnected after the presenter retires.
    Never remove mounted elements or free this presenter's renderer. }
  INyxBrowserConfirmation = interface(INyxConfirmation)
    ['{271BE5DA-8A62-4F45-A8BC-001006000002}']
    function GetRenderer: TNyxBrowserRenderer;
    function GetHost: INyxBrowserModalHost;
    property Renderer: TNyxBrowserRenderer read GetRenderer;
    property Host: INyxBrowserModalHost read GetHost;
  end;

{ Template is copied, never borrowed after construction. Optional theme follows
  the renderer's borrowed lifetime contract and must outlive this presenter. }
function NewNyxBrowserConfirmation(const ATemplate: INyxConfirmationDialog;
  ATheme: TNyxTheme = nil): INyxBrowserConfirmation;

implementation

uses
  SysUtils, Math, nyx.types, nyx.interaction;

type
  TNyxBrowserConfirmation = class(TNyxConfirmationPresenter,
    INyxConfirmation, INyxBrowserConfirmation)
  private
    FRenderer: TNyxBrowserRenderer;
    FHost: INyxBrowserModalHost;
    FInvoker: TJSHTMLElement;
    procedure Semantic(ANode: TNyxNode; const AEvent: TNyxEventInfo);
  protected
    procedure Present(const AOptions: TNyxConfirmationOptions;
      const AFocusID: TNyxText); override;
    procedure Conceal; override;
  public
    constructor Create(const ATemplate: INyxConfirmationDialog; ATheme: TNyxTheme);
    destructor Destroy; override;
    function GetRenderer: TNyxBrowserRenderer;
    function GetHost: INyxBrowserModalHost;
    function GetEvents: INyxEvents; override;
  end;

constructor TNyxBrowserConfirmation.Create(
  const ATemplate: INyxConfirmationDialog; ATheme: TNyxTheme);
begin
  inherited Create(ATemplate);
  FHost := NewNyxBrowserModalHost;
  FHost.OnDismiss := Dismiss;
  FRenderer := TNyxBrowserRenderer.Create(ATheme);
  FRenderer.OnEvent := Semantic;
end;

destructor TNyxBrowserConfirmation.Destroy;
begin

  if FHost <> nil then
  begin
    FHost.OnDismiss := nil;
    Conceal;
  end;

  if FRenderer <> nil then
  begin
    FRenderer.OnEvent := nil;
    FRenderer.Free;
  end;
  FHost := nil;
  inherited Destroy;
end;

procedure TNyxBrowserConfirmation.Semantic(ANode: TNyxNode;
  const AEvent: TNyxEventInfo);
begin
  Accept(AEvent);
end;

procedure TNyxBrowserConfirmation.Present(const AOptions: TNyxConfirmationOptions;
  const AFocusID: TNyxText);
var
  LFocus: TJSHTMLElement;
  LHeight: Integer;
begin
  FInvoker := TJSHTMLElement(Web.document.activeElement);
  FRenderer.Render(Document, GetContent.Node, FHost.Element);
  LFocus := FRenderer.FocusFor(AFocusID, niDesign);

  if (LFocus = nil) or not NyxInteractionPolicy(
    FRenderer.Root.Find(AFocusID)).CanIssueCommand then
  begin
    raise ENyxModel.Create('Confirmation initial part has no available focus face');
  end;
  try
    FHost.Show(AOptions.Modal);
    { The same ordinary Nyx view owns content; allow long customized recipes to
      scroll inside this compact host rather than clipping their actions. }
    FHost.Element.style.setProperty('overflow', 'auto');

    if AOptions.SizeMode = ncsContent then
    begin
      LHeight := Max(120, Ceil(FRenderer.ElementFor(GetContent.ID).getBoundingClientRect.height));

      if AOptions.Modal.HeightLimit <> 0 then
      begin
        LHeight := Min(LHeight, AOptions.Modal.HeightLimit);
      end;
      FHost.Element.style.setProperty('height', 'min(' + IntToStr(LHeight) + 'px, ' +
        IntToStr(AOptions.Modal.ViewportPercent) + 'dvh)');
    end;
    NyxFocusWithoutScroll(LFocus);

    if not LFocus.contains(Web.document.activeElement) then
    begin
      raise ENyxModel.Create('Confirmation initial part cannot receive focus');
    end;
  except
    Conceal;
    raise;
  end;
end;

procedure TNyxBrowserConfirmation.Conceal;
var
  LInvoker: TJSHTMLElement;
begin
  LInvoker := FInvoker;
  FInvoker := nil;
  FHost.Hide;

  if (LInvoker <> nil) and Web.document.body.contains(LInvoker) then
  begin
    NyxFocusWithoutScroll(LInvoker);
  end;
end;

function TNyxBrowserConfirmation.GetRenderer: TNyxBrowserRenderer;
begin
  Result := FRenderer;
end;

function TNyxBrowserConfirmation.GetHost: INyxBrowserModalHost;
begin
  Result := FHost;
end;

function TNyxBrowserConfirmation.GetEvents: INyxEvents;
begin
  Result := FRenderer.Events;
end;

function NewNyxBrowserConfirmation(const ATemplate: INyxConfirmationDialog;
  ATheme: TNyxTheme): INyxBrowserConfirmation;
begin
  Result := TNyxBrowserConfirmation.Create(ATemplate, ATheme);
end;

end.
