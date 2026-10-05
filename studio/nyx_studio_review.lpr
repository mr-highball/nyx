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
program nyx_studio_review;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, JS, Web, nyx.text, nyx.data, nyx.types, nyx.model, nyx.codec,
  nyx.render.browser;

var
  GRequest: TJSXMLHttpRequest;
  GDocument: TNyxDocument;
  GRenderer: TNyxBrowserRenderer;
  GChrome: TNyxDocument;
  GChromeRenderer: TNyxBrowserRenderer;
  GStatus: TNyxNode;
  GHost: TJSHTMLElement;
  GRevision: Integer;
  GRetired: Boolean;

procedure Poll; forward;

{ Chrome uses ordinary Nyx controls and the public renderer. Its owned document
  stays independent from the admitted review view; neither contains editor
  credentials, source buffers or a borrowed node from another workspace. }
procedure Status(const AText: TNyxText);
begin
  GChromeRenderer.Root.Find('review-status').Configure.Text(AText).Done;
  GChromeRenderer.Sync;
end;

{ One request is outstanding at a time. A new accepted candidate is decoded and
  mounted off-screen before replacing the previous renderer/document. An invalid
  packet retains the last good visual view. Rendering is observation: compiling
  or executing a Pascal callback remains an explicit build/application action. }
procedure Ready;
var
  LPacket: TNyxDataValue;
  LDocument: TNyxDocument;
  LRenderer: TNyxBrowserRenderer;
  LRoot: TNyxNode;
  LHost: TJSHTMLElement;
begin

  if GRequest.readyState <> 4 then
  begin
    Exit;
  end;
  try

    if GRequest.status = 404 then
    begin
      GRetired := True;
      Status('This review has ended. Its last preview is retained.');
      document.body.setAttribute('data-nyx-review-ready', 'retired');
      Exit;
    end;

    if GRequest.status <> 200 then
    begin
      raise Exception.Create('Review observation is temporarily unavailable');
    end;
    LPacket := TNyxDataValue.ParseJSON(GRequest.responseText);

    if LPacket.Field('revision').AsInteger <> GRevision then
    begin
      LDocument := TNyxCodec.Decode(LPacket.Field('design').AsText);
      LRenderer := TNyxBrowserRenderer.Create;
      LHost := TJSHTMLElement(document.createElement('div'));
      try
        LRoot := LDocument.Find(LPacket.Field('view').AsText);

        if LRoot <> nil then
        begin
          LRenderer.Render(LDocument, LRoot, LHost, False);
        end;
        { Publish only after complete candidate admission/rendering. Detach old
          host last so a failed candidate cannot clear the visible good view. }
        GHost.parentNode.replaceChild(LHost, GHost);
        GRenderer.Free;
        GDocument.Free;
        GHost := LHost;
        GRenderer := LRenderer;
        GDocument := LDocument;
        LRenderer := nil;
        LDocument := nil;
        GRevision := LPacket.Field('revision').AsInteger;
        document.body.setAttribute('data-nyx-review-revision', IntToStr(GRevision));
        document.body.setAttribute('data-nyx-review-id', LPacket.Field('review').AsText);
        document.body.setAttribute('data-nyx-review-ready', 'true');

        if LRoot = nil then
        begin
          Status('Empty review / revision ' + IntToStr(GRevision));
        end
        else
        begin
          Status('Live review / revision ' + IntToStr(GRevision));
        end;
      finally
        LRenderer.Free;
        LDocument.Free;
      end;
    end;
  except
    on LException: Exception do
    begin
      Status(LException.Message);
      document.body.setAttribute('data-nyx-review-observation', 'unavailable');
    end;
  end;

  if not GRetired then
  begin
    window.setTimeout(@Poll, 500);
  end;
end;

procedure Poll;
begin
  GRequest := TJSXMLHttpRequest.new;
  GRequest.onreadystatechange := @Ready;
  GRequest.open('GET', 'api/agents/review' + window.location.search +
    '&after=' + IntToStr(GRevision), True);
  GRequest.send;
end;

var
  LRoot: TNyxNode;
  LChromeHost: TJSHTMLElement;
begin
  GChrome := TNyxDocument.Create;
  LRoot := TNyxNode.Create(nkColumn, 'review-chrome')
    .Configure.Padding(16).Gap(8).Done;
  GChrome.AddPage(LRoot);
  LRoot.Add(TNyxNode.Create(nkHeading, 'review-heading')
    .Configure.Text('Independent review').Done);
  GStatus := TNyxNode.Create(nkLabel, 'review-status')
    .Configure.Text('Connecting to the live review').Done;
  LRoot.Add(GStatus);
  LChromeHost := TJSHTMLElement(document.createElement('div'));
  document.body.appendChild(LChromeHost);
  GChromeRenderer := TNyxBrowserRenderer.Create;
  GChromeRenderer.Render(GChrome, LRoot, LChromeHost, False);
  GHost := TJSHTMLElement(document.createElement('div'));
  document.body.appendChild(GHost);
  Poll;
end.
