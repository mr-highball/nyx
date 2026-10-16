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

program nyx_canvas_graph_compiled;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.codec, nyx.controls, nyx.state,
  nyx.canvas.types, nyx.canvas.workspace, nyx.flow.declarations, nyx.flow.types, nyx.generated.view
  {$ifdef PAS2JS}, JS, Web, nyx.application.browser, nyx.canvas.browser
  {$else}, Classes, Interfaces, Forms, Controls, StdCtrls,
    nyx.application.lcl, nyx.canvas.lcl{$endif};

var
  GDocument: TNyxDocument;
  GExpected: TNyxText;
  GCaption: TNyxText;
  GGraph: TNyxFlowRef;
  GChecks: Integer;
  GStage: Integer;
  GDone: Boolean;
  {$ifdef PAS2JS}
  GApp: TNyxBrowserApplication;
  GView: INyxBrowserCanvasWorkspaceView;
  GRequest: TJSXMLHttpRequest;
  GHost: TJSHTMLElement;
  GStarted: Double;
  {$else}
  GApp: TNyxLCLApplication;
  GView: INyxLCLCanvasWorkspaceView;
  GStarted: QWord;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Compiled graph saving: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure Start;
begin
  GDocument := BuildNyxDocument;
  Check(TNyxCodec.Encode(GDocument) = GExpected,
    'independent compiled constructor reproduces the complete exact accepted document');
  Check(GDocument.Flows.Definition(GGraph).StepCount = 2,
    'compiled source contains applied steps only');
  {$ifdef PAS2JS}
  GHost := TJSHTMLElement(document.createElement('div'));
  document.body.appendChild(GHost);
  GApp := TNyxBrowserApplication.Create;
  GApp.Run(GDocument, GHost);
  GStarted := TJSDate.now;
  {$else}
  GApp := TNyxLCLApplication.Create;
  GApp.Mount(GDocument);
  GApp.Window.Show;
  GStarted := GetTickCount64;
  {$endif}
end;

procedure Step;
var
  LID: TNyxText;
begin
  case GStage of
    0:
      begin
        LID := GDocument.Find('program-tools').Part(NyxPart('workspace')).ID;
        GView := GApp.View.CanvasView(LID) as
          {$ifdef PAS2JS}INyxBrowserCanvasWorkspaceView{$else}INyxLCLCanvasWorkspaceView{$endif};
        GView.Refresh;
        GView.Execute(nccFit);
        Check(GView.MountedCount > 0, 'compiled saved canvas mounts actual target controls');
        {$ifdef PAS2JS}
        TJSHTMLInputElement(GView.RendererFor(NyxCanvasItem('Greeting')).InputFor('name-input')).value := 'Avery';
        GView.RendererFor(NyxCanvasItem('Greeting')).InputFor('name-input')
          .dispatchEvent(TJSEvent.new('input'));
        {$else}
        TEdit(GView.RendererFor(NyxCanvasItem('Greeting')).InputFor('name-input')).Text := 'Avery';
        {$endif}
      end;
    1:
      begin

        if GApp.State.GetValue(NyxTextState('caption')) <> GCaption then
        begin
          Exit;
        end;
        Check(GApp.State.GetValue(NyxTextState('name')) = 'Avery',
          'actual input writes the independent application store');
        {$ifdef PAS2JS}
        Check(GView.RendererFor(NyxCanvasItem('Greeting')).ElementFor('caption-label').textContent = GCaption,
          'compiled graph, program and saved route publish into the actual browser bound label');
        {$else}
        Check(TLabel(GView.RendererFor(NyxCanvasItem('Greeting')).ControlFor('caption-label')).Caption = GCaption,
          'compiled graph, program and saved route publish into the actual native bound label');
        {$endif}
        Check(TNyxCodec.Encode(GDocument) = GExpected,
          'application execution preserves complete accepted source defaults and component definitions');
        GDone := True;
      end;
  end;
  Inc(GStage);
end;

procedure Retire;
begin
  GView := nil;
  FreeAndNil(GApp);
  FreeAndNil(GDocument);
end;

{$ifdef PAS2JS}
procedure Drive;
begin
  try

    if TJSDate.now - GStarted > 30000 then
    begin
      raise Exception.Create('Compiled graph application deadline at stage ' + IntToStr(GStage));
    end;
    Step;

    if GDone then
    begin
      document.body.setAttribute('data-canvas-graph-compiled', 'passed');
      document.body.setAttribute('data-canvas-graph-checks', IntToStr(GChecks));

      if Pos('retire=1', window.location.search) > 0 then
      begin
        Retire;
      end;
      Exit;
    end;
    window.setTimeout(@Drive, 20);
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-canvas-graph-compiled', 'failed');
      document.body.setAttribute('data-canvas-graph-error', E.Message);
      Retire;
    end;
  end;
end;

function Loaded(AEvent: TJSProgressEvent): Boolean;
begin
  Result := True;
  try
    Check(GRequest.status = 200, 'complete expected design is fetched over HTTP');
    GExpected := GRequest.responseText;
    GCaption := document.body.getAttribute('data-expected-caption');
    Start;
    Drive;
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-canvas-graph-compiled', 'failed');
      document.body.setAttribute('data-canvas-graph-error', E.Message);
      Retire;
    end;
  end;
end;
{$else}
var
  LFile: TFileStream;
  LTurn: Integer;
{$endif}
begin
  GGraph := NyxFlowRef('Greeting flow');
  {$ifdef PAS2JS}

  if document.body.hasAttribute('data-expected-graph') then
  begin
    GGraph := NyxFlowRef(document.body.getAttribute('data-expected-graph'));
  end;
  GRequest := TJSXMLHttpRequest.new;
  GRequest.open('GET', 'expected.nyx', True);
  GRequest.onload := @Loaded;
  GRequest.send;
  {$else}
  try

    if not (ParamCount in [2, 3]) then
    begin
      raise Exception.Create('Supply exact accepted design and expected caption');
    end;
    LFile := TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyWrite);
    try
      SetLength(GExpected, LFile.Size);

      if Length(GExpected) > 0 then
      begin
        LFile.ReadBuffer(GExpected[1], Length(GExpected));
      end;
    finally
      LFile.Free;
    end;
    GCaption := ParamStr(2);

    if ParamCount = 3 then
    begin
      GGraph := NyxFlowRef(ParamStr(3));
    end;
    Application.Initialize;
    Start;
    while not GDone do
    begin
      CheckSynchronize;
      Application.ProcessMessages;

      if GetTickCount64 - GStarted > 30000 then
      begin
        raise Exception.Create('Compiled graph application deadline at stage ' + IntToStr(GStage));
      end;
      Step;
      Sleep(1);
    end;
    Retire;
    for LTurn := 1 to 12 do
    begin
      CheckSynchronize;
      Application.ProcessMessages;
      Sleep(1);
    end;
    WriteLn('PASS ', GChecks, ' exact compiled graph/application checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      Retire;
    end;
  end;
  {$endif}
end.
