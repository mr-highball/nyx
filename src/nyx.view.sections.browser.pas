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
unit nyx.view.sections.browser;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Web, nyx.model, nyx.state, nyx.behavior, nyx.view.sections, nyx.render.browser;

type
  { Configure factories only before each candidate Render. The borrowed receiver
    must outlive section/change handles; do not retain candidates, pump queues,
    schedule work or subscribe directly to unpublished candidate events. }
  TNyxBrowserSectionConfigure = procedure(ARenderer: TNyxBrowserRenderer) of object;

  INyxBrowserViewSection = interface(INyxViewSection)
    ['{E5C4D781-3777-40FA-A47B-87158D7648FD}']
    { Borrow current renderer for element lookup/event subscription. Its target
      lifetime belongs to publication; do not Render/Unmount/MoveHost directly. }
    function GetRenderer: TNyxBrowserRenderer;
    property Renderer: TNyxBrowserRenderer read GetRenderer;
  end;

{ Host is an empty dedicated attached element. Its parent must outlive all
  handles: staging/rollback use inert hidden sibling elements. Observer is managed,
  host/configurator receivers explicitly borrowed. Candidate Render copies source;
  no Node framework or separate UI implementation is introduced. }
function NewNyxBrowserViewSection(const AReference: TNyxViewSectionRef;
  AHost: TJSHTMLElement; AConfigure: TNyxBrowserSectionConfigure = nil;
  const AObserver: INyxViewSectionObserver = nil): INyxBrowserViewSection;

implementation

uses
  SysUtils, nyx.editing, nyx.editing.browser, nyx.schema;

type
  TSectionHost = TJSHTMLElement;
  TSectionFocus = TJSHTMLElement;
  TSectionParent = TJSNode;
  TSectionRenderer = TNyxBrowserRenderer;
  TSectionConfigure = TNyxBrowserSectionConfigure;

function SectionParent(AHost: TSectionHost): TSectionParent;
begin
  Result := AHost.parentNode;
end;

function SectionWidth(AHost: TSectionHost): Integer;
begin
  Result := AHost.clientWidth;
end;

function SectionHeight(AHost: TSectionHost): Integer;
begin
  Result := AHost.clientHeight;
end;

function ValidSectionHost(AHost: TSectionHost): Boolean;
begin
  Result := (AHost <> nil) and (AHost.parentNode <> nil) and
    (AHost.firstChild = nil) and document.body.contains(AHost);
end;

function NewSectionParking(AHost: TSectionHost): TSectionHost;
begin
  Result := TJSHTMLElement(document.createElement('div'));
  Result.className := AHost.className;
  Result.style.cssText := AHost.style.cssText;
  Result.setAttribute('inert', '');
  Result.setAttribute('aria-hidden', 'true');
  Result.style.setProperty('position', 'fixed');
  Result.style.setProperty('left', '-100000px');
  Result.style.setProperty('top', '0');
  Result.style.setProperty('visibility', 'hidden');
  Result.style.setProperty('pointer-events', 'none');
  Result.style.setProperty('box-sizing', 'border-box');
  Result.style.setProperty('width', IntToStr(AHost.clientWidth) + 'px');
  Result.style.setProperty('height', IntToStr(AHost.clientHeight) + 'px');
  AHost.parentNode.appendChild(Result);
end;

procedure FreeSectionParking(var AHost: TSectionHost);
begin

  if AHost <> nil then
  begin
    AHost.remove;
    AHost := nil;
  end;
end;

function SectionPlacementMatches(AHost: TSectionHost; ARenderer: TSectionRenderer): Boolean;
begin
  Result := False;

  if (AHost = nil) or (AHost.parentNode = nil) or not document.body.contains(AHost) then
  begin
    Exit;
  end;

  if ARenderer = nil then
  begin
    Exit(AHost.firstChild = nil);
  end;
  { Ordinary Render owns its scoped style plus one root element. Foreign
    children make replacement refuse rather than erase caller-owned content. }
  Result := (ARenderer.ElementFor(ARenderer.Root.ID).parentNode = AHost) and
    (AHost.childNodes.length = 2);
end;

procedure CaptureSectionFocus(AHost: TSectionHost; out AFocus: TSectionFocus;
  out ASelection: TNyxTextSelection);
begin
  AFocus := nil;
  ASelection := Default(TNyxTextSelection);

  if (document.activeElement <> nil) and AHost.contains(document.activeElement) then
  begin
    AFocus := TJSHTMLElement(document.activeElement);
    ASelection := CaptureNyxBrowserSelection(AFocus);
  end;
end;

procedure RestoreSectionFocus(AFocus: TSectionFocus; const ASelection: TNyxTextSelection);
begin

  if AFocus <> nil then
  begin
    NyxFocusWithoutScroll(AFocus);

    if ASelection.Defined then
    begin
      SelectNyxBrowserText(AFocus, ASelection);
    end;
  end;
end;

procedure FinishSectionPreview(ARenderer: TSectionRenderer);
begin
  { MoveHost already calls the ordinary browser Sync. No duplicate update. }

  if ARenderer = nil then
  begin
    raise ENyxModel.Create('A browser section preview requires its mounted candidate');
  end;
end;

{$include nyx.view.sections.adapter.inc}

function NewNyxBrowserViewSection(const AReference: TNyxViewSectionRef;
  AHost: TJSHTMLElement; AConfigure: TNyxBrowserSectionConfigure;
  const AObserver: INyxViewSectionObserver): INyxBrowserViewSection;
begin
  Result := TSection.Create(AReference, AHost, AConfigure, AObserver);
end;

end.
