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


unit nyx.modal.browser;
{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch externalclass}

interface

uses
  Web, nyx.modal;

type
  { Borrow Element only while this managed host is alive. Render/move a public
    Nyx view into it; the adapter creates only the standard HTML dialog host.
    Show reconnects this same owned element after surrounding host remounts;
    its mounted descendants and editor input are retained. }
  INyxBrowserModalHost = interface(INyxModalHost)
    ['{271BE5DA-8A62-4F45-A8BC-001005005002}']
    function GetElement: TJSHTMLElement;
    property Element: TJSHTMLElement read GetElement;
  end;

{ Creates one empty standard dialog host. Its interface owns that DOM element;
  ordinary Nyx documents/renderers retain ownership of mounted content. }
function NewNyxBrowserModalHost: INyxBrowserModalHost;

implementation

uses
  SysUtils, nyx.text;

type
  { The installed Web binding predates this interface. The bridge is limited
    to HTML Living Standard dialog operations, not application JavaScript.
    showModal supplies the top layer, background inertness and tab containment.
    https://html.spec.whatwg.org/multipage/interactive-elements.html#the-dialog-element }
  TNyxHTMLDialog = class external name 'HTMLDialogElement'(TJSHTMLElement)
    procedure showModal;
    procedure close;
  end;

  TNyxBrowserModalHost = class(TInterfacedObject, INyxModalHost, INyxBrowserModalHost)
  private
    FDialog: TNyxHTMLDialog;
    FOpen: Boolean;
    FOnDismiss: TNyxModalDismiss;
    function Cancel(AEvent: TJSEvent): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function GetOpen: Boolean;
    function GetOnDismiss: TNyxModalDismiss;
    procedure SetOnDismiss(AValue: TNyxModalDismiss);
    function GetElement: TJSHTMLElement;
    procedure Show(const AOptions: TNyxModalOptions);
    procedure Hide;
  end;

constructor TNyxBrowserModalHost.Create;
begin
  inherited Create;
  FDialog := TNyxHTMLDialog(document.createElement('dialog'));
  FDialog.setAttribute('data-nyx-modal', 'true');
  FDialog.setAttribute('aria-modal', 'true');
  FDialog.addEventListener('cancel', @Cancel);
  document.body.appendChild(FDialog);
end;

destructor TNyxBrowserModalHost.Destroy;
begin
  FOnDismiss := nil;
  Hide;
  FDialog.removeEventListener('cancel', @Cancel);
  FDialog.remove;
  inherited Destroy;
end;

function TNyxBrowserModalHost.GetOpen: Boolean;
begin
  Result := FOpen;
end;

function TNyxBrowserModalHost.GetOnDismiss: TNyxModalDismiss;
begin
  Result := FOnDismiss;
end;

procedure TNyxBrowserModalHost.SetOnDismiss(AValue: TNyxModalDismiss);
begin
  FOnDismiss := AValue;
end;

function TNyxBrowserModalHost.GetElement: TJSHTMLElement;
begin
  Result := FDialog;
end;

procedure TNyxBrowserModalHost.Show(const AOptions: TNyxModalOptions);
begin
  { Revalidate default/untrusted record values before changing presentation. }
  AOptions.Viewport(AOptions.ViewportPercent).MaximumWidth(AOptions.WidthLimit);
  { A Nyx view mounted into body may replace its shell while this independently
    owned modal is closed or open. The detached dialog still owns its mounted
    descendants. Reconnect that exact host, clear the previous top-layer state
    and let showModal establish modality again; never recreate editor input. }

  if not document.body.contains(FDialog) then
  begin
    FDialog.close;
    FOpen := False;
    document.body.appendChild(FDialog);
  end;
  FDialog.setAttribute('aria-label', AOptions.Title);
  FDialog.style.setProperty('width', IntToStr(AOptions.ViewportPercent) + 'vw');
  FDialog.style.setProperty('height', IntToStr(AOptions.ViewportPercent) + 'dvh');
  FDialog.style.setProperty('max-width', IntToStr(AOptions.WidthLimit) + 'px');
  FDialog.style.setProperty('max-height', '100dvh');
  FDialog.style.setProperty('padding', '0');
  FDialog.style.setProperty('border', '1px solid #dfe3ec');
  FDialog.style.setProperty('border-radius', '12px');
  FDialog.style.setProperty('overflow', 'hidden');

  if not FOpen then
  begin
    FDialog.showModal;
    FOpen := True;
  end;
end;

procedure TNyxBrowserModalHost.Hide;
begin

  if FOpen then
  begin
    FDialog.close;
    FOpen := False;
  end;
end;

function TNyxBrowserModalHost.Cancel(AEvent: TJSEvent): Boolean;
begin
  { The controller first moves the retained view and closes this host. Prevent
    an independent browser close from racing that lifetime/return operation. }
  AEvent.preventDefault;

  if Assigned(FOnDismiss) then
  begin
    FOnDismiss;
  end
  else
  begin
    Hide;
  end;
  Result := True;
end;

function NewNyxBrowserModalHost: INyxBrowserModalHost;
begin
  Result := TNyxBrowserModalHost.Create;
end;

end.
