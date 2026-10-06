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

program nyx_studio_preview;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, JS, Web, nyx.text, nyx.data, nyx.model, nyx.codec, nyx.render.browser, nyx.presentations;

var
  GRequest: TJSXMLHttpRequest;
  GDocument: TNyxDocument;
  GRenderer: TNyxBrowserRenderer;
  GFrame: TJSHTMLIFrameElement;
  GPacket: TNyxDataValue;

{ Old preview packets have no explicit choice. New packets carry null or one
  exact manual reference as a value, never a JSON member name. }
function PresentationName(const APacket: TNyxDataValue): TNyxText;
var
  LIndex: Integer;
  LValue: TNyxDataValue;
begin
  Result := '';
  for LIndex := 0 to APacket.Count - 1 do
  begin

    if APacket.Key(LIndex) = 'presentation' then
    begin
      LValue := APacket.Field('presentation');

      if LValue.Kind <> ndNull then
      begin
        Result := LValue.AsText;
      end;
      Exit;
    end;
  end;
end;

{ A headless browser's minimum outer-window width is not a CSS viewport promise.
  The independently admitted preview lives in an exact-size child viewport;
  its real dimensions and revision must agree before the outer page is ready. }
procedure CheckFrame;
var
  LBody: TJSHTMLElement;
  LReady: String;
begin

  if (GFrame.contentDocument = nil) or (GFrame.contentDocument.body = nil) then
  begin
    window.setTimeout(@CheckFrame, 25);
    Exit;
  end;
  LBody := TJSHTMLElement(GFrame.contentDocument.body);
  LReady := LBody.getAttribute('data-nyx-preview-ready');

  if LReady = 'true' then
  begin

    if (LBody.getAttribute('data-nyx-preview-presentation') <> PresentationName(GPacket)) or
      (LBody.getAttribute('data-nyx-preview-width') <>
      IntToStr(GPacket.Field('width').AsInteger)) or
      (LBody.getAttribute('data-nyx-preview-height') <>
      IntToStr(GPacket.Field('height').AsInteger)) or
      (LBody.getAttribute('data-nyx-preview-revision') <>
      IntToStr(GPacket.Field('revision').AsInteger)) then
    begin
      document.body.setAttribute('data-nyx-preview-ready', 'failed');
      Exit;
    end;
    document.body.setAttribute('data-nyx-preview-width', LBody.getAttribute('data-nyx-preview-width'));
    document.body.setAttribute('data-nyx-preview-presentation', LBody.getAttribute('data-nyx-preview-presentation'));
    document.body.setAttribute('data-nyx-preview-height', LBody.getAttribute('data-nyx-preview-height'));
    document.body.setAttribute('data-nyx-preview-revision', LBody.getAttribute('data-nyx-preview-revision'));
    document.body.setAttribute('data-nyx-preview-ready', 'true');
  end
  else if LReady = 'failed' then
  begin
    document.body.setAttribute('data-nyx-preview-ready', 'failed');
  end
  else
  begin
    window.setTimeout(@CheckFrame, 25);
  end;
end;

procedure Ready;
var
  LPacket: TNyxDataValue;
  LRoot: TNyxNode;
  LHost: TJSHTMLElement;
  LPresentation: TNyxText;
begin

  if GRequest.readyState <> 4 then
  begin
    Exit;
  end;
  try

    if GRequest.status <> 200 then
    begin
      raise Exception.Create('The preview snapshot expired; request a new revision');
    end;
    LPacket := TNyxDataValue.ParseJSON(GRequest.responseText);

    if Pos('&frame=1', window.location.search) = 0 then
    begin
      { Same-origin child navigation retains the opaque snapshot capability.
        CSS/media queries and viewport units see the admitted dimensions even
        if the host window is larger. Only Pascal controls this presentation. }
      GPacket := LPacket;
      GFrame := TJSHTMLIFrameElement(document.createElement('iframe'));
      GFrame.setAttribute('title', 'Nyx rendered preview');
      GFrame.style.setProperty('display', 'block');
      GFrame.style.setProperty('width', IntToStr(LPacket.Field('width').AsInteger) + 'px');
      GFrame.style.setProperty('height', IntToStr(LPacket.Field('height').AsInteger) + 'px');
      GFrame.style.setProperty('border', '0');
      GFrame.src := window.location.pathname + window.location.search + '&frame=1';
      document.body.appendChild(GFrame);
      window.setTimeout(@CheckFrame, 25);
      Exit;
    end;
    GDocument := TNyxCodec.Decode(LPacket.Field('design').AsText);
    LRoot := GDocument.Find(LPacket.Field('view').AsText);

    if LRoot = nil then
    begin
      raise Exception.Create('Preview root is missing');
    end;
    LHost := TJSHTMLElement(document.createElement('div'));
    LHost.style.setProperty('min-height', '100vh');
    document.body.appendChild(LHost);
    GRenderer := TNyxBrowserRenderer.Create;
    GRenderer.Render(GDocument, LRoot, LHost, False);
    LPresentation := PresentationName(LPacket);

    if LPresentation <> '' then
    begin
      GRenderer.Presentations.Select(NyxPresentation(LPresentation));
    end;
    document.body.setAttribute('data-nyx-preview-presentation', LPresentation);
    document.body.setAttribute('data-nyx-preview-width', IntToStr(window.innerWidth));
    document.body.setAttribute('data-nyx-preview-height', IntToStr(window.innerHeight));
    document.body.setAttribute('data-nyx-preview-ready', 'true');
    document.body.setAttribute('data-nyx-preview-revision', IntToStr(LPacket.Field('revision').AsInteger));
  except
    on LException: Exception do
    begin
      document.body.textContent := LException.Message;
      document.body.setAttribute('data-nyx-preview-ready', 'failed');
    end;
  end;
end;

begin
  GRequest := TJSXMLHttpRequest.new;
  GRequest.onreadystatechange := @Ready;
  GRequest.open('GET', 'api/agents/preview' + window.location.search, True);
  GRequest.send;
end.
