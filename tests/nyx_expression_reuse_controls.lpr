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

program nyx_expression_reuse_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.state, nyx.model, nyx.codec, nyx.source,
  nyx.studio.projects, nyx.generated.view
  {$ifdef PAS2JS}, JS, Web, nyx.application.browser
  {$else}, Interfaces, Classes, Forms, Controls, StdCtrls, nyx.application.lcl{$endif};

var
  {$ifdef PAS2JS}
  GApp: TNyxBrowserApplication;
  GOther: TNyxBrowserApplication;
  GHost: TJSHTMLElement;
  GOtherHost: TJSHTMLElement;
  GRequest: TJSXMLHttpRequest;
  GNotes: TJSHTMLTextAreaElement;
  GFirst: TJSHTMLElement;
  {$else}
  GApp: TNyxLCLApplication;
  GOther: TNyxLCLApplication;
  GNotes: TMemo;
  GFirst: TControl;
  {$endif}
  GDocument: TNyxDocument;
  GBefore: TNyxText;
  GChecks: Integer;
  GPacket: TNyxText;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Actual expression reuse: ' + AReason);
  end;
  Inc(GChecks);
end;

function Caption(const AInstance: TNyxText): TNyxText;
var
  LID: TNyxText;
begin
  LID := NyxQualifiedID(AInstance, 'featured-caption');
  {$ifdef PAS2JS}
  Result := GApp.View.ElementFor(LID).textContent;
  {$else}
  Result := TNyxText(TLabel(GApp.View.ControlFor(LID)).Caption);
  {$endif}
end;

procedure Retire;
begin
  FreeAndNil(GApp);
  FreeAndNil(GOther);
  FreeAndNil(GDocument);
end;

procedure Verify;
var
  LPair: TNyxProjectPair;
  LResolved: TNyxProjectPair;
  LDocument: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
begin
  GDocument := BuildNyxDocument;
  GBefore := TNyxCodec.Encode(GDocument);
  LPair := DecodeNyxProject(GPacket);
  Check(LPair.Design = GBefore, 'compiled derived companion equals transferred design exactly');
  LDocument := nil;
  LWorkspace := nil;
  try
    AdmitNyxProject(LPair, nprRequireMatch, LDocument, LWorkspace, LResolved);
    Check(TNyxCodec.Encode(LDocument) = GBefore,
      'transferred source and compiled reusable application admit identical meaning');
    Check(LWorkspace.Render(LDocument) = LPair.Source,
      'transferred source retains exact no-op Pascal');
  finally
    LWorkspace.Free;
    LDocument.Free;
  end;
  {$ifdef PAS2JS}
  GHost := TJSHTMLElement(document.createElement('div'));
  GOtherHost := TJSHTMLElement(document.createElement('div'));
  document.body.appendChild(GHost);
  document.body.appendChild(GOtherHost);
  GApp := TNyxBrowserApplication.Create;
  GOther := TNyxBrowserApplication.Create;
  GApp.Run(GDocument, GHost);
  GOther.Run(GDocument, GOtherHost);
  {$else}
  Application.Initialize;
  GApp := TNyxLCLApplication.Create;
  GOther := TNyxLCLApplication.Create;
  GApp.Mount(GDocument);
  GOther.Mount(GDocument);
  GOther.Window.Show;
  GApp.Window.Show;
  {$endif}
  Check(Caption('featured-first') = 'Own quantity: 3',
    'compiled instance override reaches its actual label');
  Check(Caption('featured-second') = TNyxText('Total 🌙: 40'),
    'compiled sibling inherits the exact Unicode calculated formula');
  {$ifdef PAS2JS}
  GFirst := GApp.View.ElementFor(NyxQualifiedID('featured-first', 'featured-caption'));
  GNotes := TJSHTMLTextAreaElement(GApp.View.InputFor('order-notes'));
  GNotes.focus;
  GNotes.value := 'Unfinished reusable notes';
  {$else}
  GFirst := GApp.View.ControlFor(NyxQualifiedID('featured-first', 'featured-caption'));
  GNotes := TMemo(GApp.View.InputFor('order-notes'));
  GNotes.SetFocus;
  GNotes.Text := 'Unfinished reusable notes';
  {$endif}
  GApp.State.SetValue(NyxIntegerState('quantity'), 4);
  Check((Caption('featured-first') = 'Own quantity: 4') and
    (Caption('featured-second') = TNyxText('Total 🌙: 52.5')),
    'one state update recomputes both independently inherited/overridden controls');
  {$ifdef PAS2JS}
  Check((GApp.View.ElementFor(NyxQualifiedID('featured-first', 'featured-caption')) = GFirst) and
    (GApp.View.InputFor('order-notes') = GNotes) and
    (GNotes.value = 'Unfinished reusable notes') and (document.activeElement = GNotes),
    'actual browser instance identity and memo draft/focus survive recomputation');
  {$else}
  Check((GApp.View.ControlFor(NyxQualifiedID('featured-first', 'featured-caption')) = GFirst) and
    (GApp.View.InputFor('order-notes') = GNotes) and
    (GNotes.Text = 'Unfinished reusable notes') and GNotes.Focused,
    'actual native instance identity and memo draft/focus survive recomputation');
  {$endif}
  Check(GOther.State.GetValue(NyxIntegerState('quantity')) = 3,
    'a second mounted application retains independent runtime defaults');
  Check(TNyxCodec.Encode(GDocument) = GBefore,
    'compiled reusable controls never change definitions or authored defaults');
  GApp.ShowPage('review');
  GApp.ShowPage('home');
  Check((Caption('featured-first') = 'Own quantity: 4') and
    (Caption('featured-second') = TNyxText('Total 🌙: 52.5')),
    'navigation remount retains instance expression semantics');
end;

procedure Complete;
begin
  try
    Verify;
    Retire;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-expression-reuse-controls', 'passed');
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' actual transferred expression checks';
    {$else}
    WriteLn('PASS ', GChecks, ' actual transferred expression checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-expression-reuse-controls', 'failed');
      document.body.setAttribute('data-expression-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
      Retire;
    end;
  end;
end;

{$ifdef PAS2JS}
function Loaded(AEvent: TJSProgressEvent): Boolean;
begin
  Result := True;

  if GRequest.status <> 200 then
  begin
    document.body.setAttribute('data-expression-reuse-controls', 'failed');
    document.body.setAttribute('data-expression-error', 'Exact project packet HTTP request failed');
    Exit;
  end;
  GPacket := GRequest.responseText;
  Complete;
  GRequest := nil;
end;
{$else}
var
  LFile: TFileStream;
{$endif}

begin
  {$ifdef PAS2JS}
  GRequest := TJSXMLHttpRequest.new;
  GRequest.onload := @Loaded;
  GRequest.open('GET', 'project.nyx', True);
  GRequest.send;
  {$else}

  if ParamCount <> 1 then
  begin
    WriteLn('Supply the exact transferred project packet');
    ExitCode := 1;
    Exit;
  end;
  LFile := TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyNone);
  try
    SetLength(GPacket, LFile.Size);

    if LFile.Size > 0 then
    begin
      LFile.ReadBuffer(GPacket[1], LFile.Size);
    end;
  finally
    LFile.Free;
  end;
  Complete;
  {$endif}
end.
