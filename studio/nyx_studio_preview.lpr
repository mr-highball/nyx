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
  SysUtils, JS, Web, nyx.text, nyx.data, nyx.model, nyx.codec, nyx.render.browser;

var
  GRequest: TJSXMLHttpRequest;
  GDocument: TNyxDocument;
  GRenderer: TNyxBrowserRenderer;

procedure Ready;
var
  LPacket: TNyxDataValue;
  LRoot: TNyxNode;
  LHost: TJSHTMLElement;
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
